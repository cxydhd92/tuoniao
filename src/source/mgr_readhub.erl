%% ------------------------------------------------------------------
%% readhub站点管理进程
%% ------------------------------------------------------------------
-module(mgr_readhub).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(spider_sec, 25*60).

-include("common.hrl").
-include("todayhot.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(mgr_readhub, {
	list = []
	,time = 0
}).
-define(URL, "https://api.readhub.cn/topic?lastCursor=&pageSize=").
-define(Host, "https://www.readhub.cn/topic").
-export([start_link/0, call/1, cast/1, send/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

call(Msg) ->
	gen_server:call(?MODULE, Msg).

cast(Msg) ->
		gen_server:cast(?MODULE, Msg).

send(Msg) ->
	?MODULE ! Msg.
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	?INFO("start mgr_readhub",[]),
	erlang:send_after(20*1000, self(), start_spider),
	?INFO("finish mgr_readhub",[]),
	Time = api_todayhot:get_node_up_time(?todayhot_class_tech, ?todayhot_node_readhub),
	?INFO("Time~w",[Time]),
    {ok, #mgr_readhub{time = Time}}.

handle_call(Request, From, State) ->
	case catch do_handle_call(Request, From, State) of
		{reply, Reply, State} ->
			{reply, Reply, State};
		Reason ->
			?ERR("mgr_readhub Request ~w Reason ~w",[Request, Reason]),
			{reply, error, State}
	end.

handle_cast(Msg, State) ->
	case catch do_handle_cast(Msg, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_readhub Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
	case catch do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_readhub Request ~w Reason ~w",[Info, Reason]),
			{noreply, State}
	end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_handle_call(_Request, _From, State) ->
    {reply, ok, State}.

do_handle_cast(_Msg, State)->
	{noreply, State}.
	
do_handle_info(start_spider, State=#mgr_readhub{time=OldTime}) ->
	Now = util:now(),
	TodayZero = util:today(),
	do_start_spider(State, OldTime, Now, TodayZero, 20);
do_handle_info(_Msg, State) ->
	{noreply, State}.

do_start_spider(State, OldTime, Now, TodayZero, Num) ->
    case ibrowse:send_req(?URL++?i2l(Num), [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			BodyJsonBin = list_to_binary(ResponseBody),
			BodyTerm = jsx:decode(BodyJsonBin),
			{NNews, Time} = parse_body(BodyTerm, OldTime, Now, TodayZero),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_tech, ?todayhot_node_readhub, NNews, Time}), ignored),
			?INFO("NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State#mgr_readhub{time = Time}};
		_Err ->
			?ERR("fail ~w", [_Err]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State}
	end.

    
parse_body(Body, OldTime, Now, TodayZero) ->
	case proplists:get_value(<<"data">>, Body, undefined) of
		Data when is_list(Data) ->
			do_parse_data(Data, OldTime, [], OldTime, Now, TodayZero);
		_ ->
			{[], OldTime}
	end.
	
do_parse_data([], _OldTime, News, Time,_, _) ->
	{News, Time};
do_parse_data([Data|T], OldTime, News, NTime, Now, TodayZero) ->
	UpdateTime = proplists:get_value(<<"createdAt">>, Data),
	% Order = proplists:get_value(<<"order">>, Data),
	StrTime = ?b2l(UpdateTime),
	[Date, Time] = string:tokens(StrTime, "T"),
	[Year, Month, Day] = string:tokens(Date, "-"),
	[HTime, _] = string:tokens(Time, "."),
	[Hour, Minu, Sec] = string:tokens(HTime, ":"),
	NewTime = util:datetime_to_timestamp({{?l2i(Year), ?l2i(Month),?l2i(Day)},{?l2i(Hour), ?l2i(Minu),?l2i(Sec)}}) + 8*3600,
	case NewTime > OldTime andalso NewTime>=TodayZero of
		true -> %% 最新的并且是今天的新闻才存储
			Param = ?b2l(proplists:get_value(<<"id">>, Data)),
			Url = ?l2b(?Host ++ "/" ++ Param),
			Title = proplists:get_value(<<"title">>, Data),
			?INFO("readhub Title ~ts", [Title]),
			Abstract = proplists:get_value(<<"summary">>, Data),
			TNews = #todayhot_news{
				class = ?todayhot_class_tech,
				node_id = ?todayhot_node_readhub, sub_news=[#todayhot_sub_news{title = Title, url=Url, time=Now}]
				, abstract = Abstract, time=Now
			},
			NNTime = case NTime < NewTime of
				true -> NewTime;
				_ -> NTime
			end,
			do_parse_data(T, OldTime, [TNews|News], NNTime, Now, TodayZero);
		_ ->
			do_parse_data(T, OldTime, News, NTime, Now, TodayZero)
	end.