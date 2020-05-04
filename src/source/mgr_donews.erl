%% ------------------------------------------------------------------
%% Donews站点管理进程
%% ------------------------------------------------------------------
-module(mgr_donews).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(spider_sec, 56*60).

-include("common.hrl").
-include("todayhot.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(mgr_donews, {
	list = []
	,time = 0
}).
-define(URL, "http://www.donews.com/Column/get_news_ranking").
-define(Host, "http://www.donews.com").
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
	?INFO("start mgr_donews",[]),
	erlang:send_after(26*1000, self(), start_spider),
	?INFO("finish mgr_donews",[]),
    List = api_todayhot:get_node_news(?todayhot_class_tech, ?todayhot_node_donews),
	?INFO("Len ~w",[length(List)]),
	Today = util:today(),
	Sec = Today+86400 - util:now(),
	erlang:send_after(Sec*1000, self(), zero_up),
    {ok, #mgr_donews{list = List}}.

handle_call(Request, From, State) ->
	case catch do_handle_call(Request, From, State) of
		{reply, Reply, State} ->
			{reply, Reply, State};
		Reason ->
			?ERR("mgr_donews Request ~w Reason ~w",[Request, Reason]),
			{reply, error, State}
	end.

handle_cast(Msg, State) ->
	case catch do_handle_cast(Msg, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_donews Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
	case catch do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_donews Request ~w Reason ~w",[Info, Reason]),
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
do_handle_info(zero_up, State=#mgr_donews{list=List}) ->
	EndTime = util:today() - ?LIST_END_TIME,
	NList = api_todayhot:get_end_time_news(List, EndTime, []),
	erlang:send_after(86400*1000, self(), zero_up),
	{noreply, State#mgr_donews{list=NList}};
do_handle_info(start_spider, State=#mgr_donews{list=List}) ->
	Now = util:now(),
	TodayZero = util:today(),
	do_start_spider(State, List, Now,TodayZero);
do_handle_info(_Msg, State) ->
	{noreply, State}.

do_start_spider(State, List, Now,TodayZero) ->
    case ibrowse:send_req(?URL, [{"User-Agent","Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.132 Safari/537.36"},{"Host","www.donews.com"
},{"Accept","text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9"},{"Connection", "keep-alive"}], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			BodyJsonBin = list_to_binary(ResponseBody),
			BodyTerm = jsx:decode(BodyJsonBin),
			{NNews, Time} = parse_body(BodyTerm, List, Now, TodayZero),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_tech, ?todayhot_node_donews, NNews, Time}), ignored),
			?INFO("donews NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State#mgr_donews{list = List++NNews}};
		_Err ->
			?ERR("fail ~w", [_Err]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State}
	end.

    
parse_body(Body, List, Now, TodayZero) ->
	case proplists:get_value(<<"list">>, Body, undefined) of
		Data when is_list(Data) ->
			{News, NTime, HotList} = do_parse_data(Data, List, [], Now, TodayZero, []),
			api_todayhot:insert_new_hot(?todayhot_class_tech, ?todayhot_node_donews, HotList),
			{News, NTime};
		_ ->
			{[], Now}
	end.
	
do_parse_data([], _List, News, Time, _, HotList) ->
	{News, Time, lists:reverse(HotList)};
do_parse_data([Data|T], List, News, Now, TodayZero, HotList) ->
    Title = proplists:get_value(<<"title">>, Data),
    Url = proplists:get_value(<<"url">>, Data),
    Count = proplists:get_value(<<"read_count">>, Data),
    TNews = #todayhot_news{
		class = ?todayhot_class_tech,
		node_id = ?todayhot_node_donews, sub_news=[#todayhot_sub_news{title = Title, url=Url, time=Now, count=Count}]
		, abstract = <<"">>, time=Now
    },
    case api_todayhot:is_exist(Title, List) of
        false ->
            ?INFO("Title~ts", [Title]),
           
            do_parse_data(T, List, [TNews|News], Now, TodayZero, [TNews|HotList]);
        _ ->
            do_parse_data(T, List, News, Now, TodayZero, [TNews|HotList])
    end.