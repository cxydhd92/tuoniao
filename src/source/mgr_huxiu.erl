%% ------------------------------------------------------------------
%% huxiu站点(快讯和24小时热榜)
%% ------------------------------------------------------------------
-module(mgr_huxiu).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(spider_sec, (57*60+1)).
-define(spider_sec1, (55*60+1)).

-include("common.hrl").
-include("todayhot.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(mgr_huxiu, {
	list = []
    ,time = 0 
}).
-define(URL, "https://www.huxiu.com/").
-define(Host, "https://www.huxiu.com/").

-define(URL1, "https://daily.huxiu.com/").
-define(Host1, "https://daily.huxiu.com").

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
	?INFO("start mgr_huxiu",[]),
    erlang:send_after(33*1000, self(), start_spider),
    % erlang:send_after(34*1000, self(), start_spider1),
	?INFO("finish mgr_huxiu",[]),
    Time = api_todayhot:get_node_up_time(?todayhot_class_tech, ?todayhot_node_huxiu),
    % NewsL1 = api_todayhot:get_node_news(?todayhot_class_tech, ?todayhot_node_huxiu_daily),
    ?INFO("Time~w  ",[Time]),
    {ok, #mgr_huxiu{time = Time}}.

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
			?ERR("mgr_huxiu Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
	case catch do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_huxiu Request ~w Reason ~w",[Info, Reason]),
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
do_handle_info(start_spider, State=#mgr_huxiu{}) ->
	Now = util:now(),
	do_start_spider(State, Now);
% do_handle_info(start_spider1, State=#mgr_huxiu{}) ->
% 	Now = util:now(),
% 	do_start_spider1(State, Now);
do_handle_info(_Msg, State) ->
	{noreply, State}.

%% 热榜
do_start_spider(State=#mgr_huxiu{time = OldTime}, Now) ->
     case ibrowse:send_req(?URL, [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			{NNews, Time} = parse_body(ResponseBody, OldTime, Now),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_tech, ?todayhot_node_huxiu, NNews, Time}), ignored),
			?INFO("huxiu NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State#mgr_huxiu{time = Time}};
		_ ->
			?ERR("URL ~w fail", [?URL]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State}
    end.

parse_body(Body, OldTime, Now) ->
    %% 正则获取
    case catch re:run(Body, "{\"route\":((.|\n)*?)\"}}}", [{capture, first, list}, global, unicode]) of
        {match, Ret1} ->
        	BodyJsonBin = list_to_binary(Ret1),
			BodyTerm = jsx:decode(BodyJsonBin),
			case proplists:get_value(<<"news">>, BodyTerm, undefined) of
				OData when is_list(OData) ->
					Data = proplists:get_value(<<"articleHot">>, OData),
					% Feeds  = proplists:get_value(<<"content">>, Data, []),
					{News, NTime, HotList} = do_parse_data(Data, OldTime, [], OldTime, Now, []),
					api_todayhot:insert_new_hot(?todayhot_class_tech, ?todayhot_node_huxiu, HotList),
					{News, NTime};
				_ ->
					{[], 0}
			end;
        _ ->
            {[], 0}
    end.
	
do_parse_data([], _OldTime, News, NTime, _Now, HotList) ->
	{News, NTime, lists:reverse(HotList)};
do_parse_data([Data|T], OldTime, News, NTime, Now, HotList) ->
	NewTime = ?l2i(?b2l(proplists:get_value(<<"dateline">>, Data))),
	Title = proplists:get_value(<<"title">>, Data),
	
	Url = proplists:get_value(<<"share_url">>, Data, <<"">>),
	Abstract = proplists:get_value(<<"summary">>, Data, <<"">>),
	Img = proplists:get_value(<<"pic_path">>, Data, <<"">>),
	UserInfo = proplists:get_value(<<"user_info">>, Data, <<"">>),
	Source = proplists:get_value(<<"username">>, UserInfo, <<"">>),
	CountInfo = proplists:get_value(<<"count_info">>, Data, <<"">>),
	Count = proplists:get_value(<<"viewnum">>, CountInfo, <<"">>),
	TNews = #todayhot_news{
		class = ?todayhot_class_tech,
		node_id = ?todayhot_node_huxiu, sub_news=[#todayhot_sub_news{title = Title, url=Url, time=Now, source=Source, count=Count}]
		, abstract = Abstract, time=Now, img = Img
	},
	NNTime = case NTime < NewTime of
		true -> NewTime;
		_ -> NTime
	end,
	?INFO("Title~ts",[Title]),
	case NewTime > OldTime  of
		true -> %% 最新的新闻才存储
			
			do_parse_data(T, OldTime, [TNews|News], NNTime, Now, [TNews|HotList]);
		_ ->
			do_parse_data(T, OldTime, News, NTime, Now, [TNews|HotList])
	end.