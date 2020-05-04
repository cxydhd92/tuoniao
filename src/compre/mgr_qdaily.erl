%% ------------------------------------------------------------------
%% 好奇心日报站点管理进程
%% ------------------------------------------------------------------
-module(mgr_qdaily).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(spider_sec, 57*60).

-include("common.hrl").
-include("todayhot.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(mgr_qdaily, {
	list = []
	,time = 0
}).
-define(URL, "http://www.qdaily.com/tags/tagmore/29/").
-define(HOST, "http://www.qdaily.com/articles/").
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
	?INFO("start mgr_qdaily",[]),
	erlang:send_after(27*1000, self(), start_spider),
	?INFO("finish mgr_qdaily",[]),
    Time = api_todayhot:get_node_up_time(?todayhot_class_compre,?todayhot_node_qdaily),
	?INFO("Time ~w",[Time]),
    {ok, #mgr_qdaily{time = Time}}.

handle_call(Request, From, State) ->
	case catch do_handle_call(Request, From, State) of
		{reply, Reply, State} ->
			{reply, Reply, State};
		Reason ->
			?ERR("mgr_qdaily Request ~w Reason ~w",[Request, Reason]),
			{reply, error, State}
	end.

handle_cast(Msg, State) ->
	case catch do_handle_cast(Msg, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_qdaily Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
	case catch do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_qdaily Request ~w Reason ~w",[Info, Reason]),
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
	
do_handle_info(start_spider, State=#mgr_qdaily{time=OldTime}) ->
	Now = util:now(),
	TodayZero = util:today(),
	do_start_spider(State, OldTime, Now, TodayZero);
do_handle_info(_Msg, State) ->
	{noreply, State}.

do_start_spider(State, OldTime, Now, TodayZero) ->
    case ibrowse:send_req(?URL++?i2l(Now)++".json", [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			BodyJsonBin = list_to_binary(ResponseBody),
			BodyTerm = jsx:decode(BodyJsonBin),
			{NNews, Time} = parse_body(BodyTerm, OldTime, Now, TodayZero),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_compre, ?todayhot_node_qdaily, NNews, Time}), ignored),
			?INFO("qdaily NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State#mgr_qdaily{time = Time}};
		_Err ->
			?ERR("fail ~w", [_Err]),
			erlang:send_after(10*1000, self(), start_spider),
			{noreply, State}
	end.

    
parse_body(Body, OldTime, Now, TodayZero) ->
	case proplists:get_value(<<"data">>, Body, undefined) of
		Data when is_list(Data) ->
			Feeds  = proplists:get_value(<<"feeds">>, Data, []),
			do_parse_data(Feeds, OldTime, [], OldTime, Now, TodayZero);
		_ ->
			{[], OldTime}
	end.
	
do_parse_data([], _OldTime, News, Time,_, _) ->
	{News, Time};
do_parse_data([OData|T], OldTime, News, NTime, Now, TodayZero) ->
	Data = proplists:get_value(<<"post">>, OData),
	UpdateTime = proplists:get_value(<<"publish_time">>, Data),
	% Order = proplists:get_value(<<"order">>, Data),
	StrTime = ?b2l(UpdateTime),
	[Date, HTime|_] = string:tokens(StrTime, " "),
	[Year, Month, Day] = string:tokens(Date, "-"),
	[Hour, Minu, Sec] = string:tokens(HTime, ":"),
	NewTime = util:datetime_to_timestamp({{?l2i(Year), ?l2i(Month),?l2i(Day)},{?l2i(Hour), ?l2i(Minu),?l2i(Sec)}}),
	case NewTime > OldTime of
		true -> %% 最新的新闻才存储
			Url = ?l2b(?HOST ++ ?i2l(proplists:get_value(<<"id">>, Data)) ++ ".html"),
			% Url = ?l2b(?Host ++ "/" ++ Param),
			Title = proplists:get_value(<<"title">>, Data),
			Abstract = proplists:get_value(<<"intro">>, Data, <<"">>),
			Source = proplists:get_value(<<"source">>, Data, <<"">>),
			Img = proplists:get_value(<<"image">>, OData, <<"">>),
			TNews = #todayhot_news{
				class = ?todayhot_class_compre,
				node_id = ?todayhot_node_qdaily, sub_news=[#todayhot_sub_news{title = Title, url=Url, time=Now, source=Source}]
				, abstract = Abstract, time=Now, img = Img
			},
			NNTime = case NTime < NewTime of
				true -> NewTime;
				_ -> NTime
			end,
			do_parse_data(T, OldTime, [TNews|News], NNTime, Now, TodayZero);
		_ ->
			do_parse_data(T, OldTime, News, NTime, Now, TodayZero)
	end.