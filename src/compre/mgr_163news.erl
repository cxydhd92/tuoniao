%% ------------------------------------------------------------------
%% 163news站点(24小时点击榜)
%% ------------------------------------------------------------------
-module(mgr_163news).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(spider_sec, 40*60).
-define(spider_sec1, 42*60).
-include("common.hrl").
-include("todayhot.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(mgr_163news, {
	list = []
    ,list1 = [] %% 163pic
}).
-define(URL, "http://news.163.com/special/0001386F/rank_news.html").
-define(Host, "http://news.163.com").
-define(URL1, "http://news.163.com/photorank/").
-define(Host1, "http://news.163.com").
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
	?INFO("start mgr_163news",[]),
    erlang:send_after(32*1000, self(), start_spider),
    erlang:send_after(34*1000, self(), start_spider1),
	?INFO("finish mgr_163news",[]),
    NewsL = api_todayhot:get_node_news(?todayhot_class_compre, ?todayhot_node_163news),
    NewsL1 = api_todayhot:get_node_news(?todayhot_class_compre, ?todayhot_node_163pic),
    Today = util:today(),
	Sec = Today+86400 - util:now(),
	erlang:send_after(Sec*1000, self(), zero_up),
    {ok, #mgr_163news{list=NewsL, list1=NewsL1}}.
    
handle_call(Request, From, State) ->
	case catch do_handle_call(Request, From, State) of
		{reply, Reply, State} ->
			{reply, Reply, State};
		Reason ->
			?ERR("mgr_163news Request ~w Reason ~w",[Request, Reason]),
			{reply, error, State}
	end.

handle_cast(Msg, State) ->
	case catch do_handle_cast(Msg, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_163news Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
	case catch do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_163news Request ~w Reason ~w",[Info, Reason]),
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

do_handle_info(zero_up, State=#mgr_163news{list=List, list1=List1}) ->
	EndTime = util:today() - ?LIST_END_TIME,
	NList = api_todayhot:get_end_time_news(List, EndTime, []),
	NList1 = api_todayhot:get_end_time_news(List1, EndTime, []),
	erlang:send_after(86400*1000, self(), zero_up),
	{noreply, State#mgr_163news{list=NList, list1=NList1}};
do_handle_info(start_spider, State=#mgr_163news{}) ->
	Now = util:now(),
	do_start_spider(State, Now);
do_handle_info(start_spider1, State=#mgr_163news{}) ->
	Now = util:now(),
	do_start_spider1(State, Now);
do_handle_info(_Msg, State) ->
	{noreply, State}.

%% 24图集热榜
do_start_spider1(State=#mgr_163news{list = OldNewsL}, Now) ->
     case ibrowse:send_req(?URL1, [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			NNews = parse_body(ResponseBody, OldNewsL, Now, ?todayhot_node_163pic),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_compre, ?todayhot_node_163pic, NNews, Now}), ignored),
			?INFO("163pic NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State#mgr_163news{list = OldNewsL++NNews}};
		_Err ->
			?ERR("fail ~w", [_Err]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State}
    end.

%% 24热榜
do_start_spider(State=#mgr_163news{list = OldNewsL}, Now) ->
     case ibrowse:send_req(?URL, [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			NNews = parse_body(ResponseBody, OldNewsL, Now, ?todayhot_node_163news),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_compre, ?todayhot_node_163news, NNews, Now}), ignored),
			?INFO("163news NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State#mgr_163news{list = OldNewsL++NNews}};
		_Err ->
			?ERR("fail ~w", [_Err]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State}
    end.

parse_body(Body, List, Now, NodeId) ->
	NTitleL = do_parse_body(Body, List, Now, 1, []),
	{AddNewsL, HotList} = new_add(NTitleL, List, Now, [], NodeId, []),
	api_todayhot:insert_new_hot(?todayhot_class_compre, NodeId, HotList),
	AddNewsL.

do_parse_body(Body, List, Now, Rank, AddL) when Rank=<10 ->
    %% 正则获取
    {_, [_, [Href]|_T]} = re:run(Body, "<span>"++?i2l(Rank)++"</span><a href=\"(.*?)\">(.*?)</a>", [{capture, first, list}, global, unicode]),
    [_, SHref] = string:tokens(Href, "="),
    [CHref, CTitle|_] = string:tokens(SHref, ">"),
    ?INFO("Href~ts",[Href]),
    Url = get_url(CHref, 2),
    Title = get_title(CTitle, length("</a")),
    do_parse_body(Body, List, Now, Rank+1, [{Title, Url}|AddL]);
do_parse_body(_Body, _List, _Now, _, AddL) -> AddL.


new_add([], _, _Now, AddNewsL, _, HotList) ->
    {lists:reverse(AddNewsL), HotList};
new_add([{Title, Url}|CurData], OldList, Now, AddNewsL, NodeId, HotList) ->
	TNews = #todayhot_news{
		class = ?todayhot_class_compre,
		node_id = NodeId, sub_news=[#todayhot_sub_news{title = Title, url=Url, time=Now}]
		, abstract = <<"">>, time=Now
    },
    case api_todayhot:is_exist(Title, OldList) of
        false ->
            ?INFO("Title~ts", [Title]),
            
            new_add(CurData, OldList, Now, [TNews|AddNewsL], NodeId, [TNews|HotList]);
        _ ->
            new_add(CurData, OldList, Now, AddNewsL, NodeId, [TNews|HotList])
    end.

get_title(Title, AfLen) ->
    Len = length(Title),
    ?l2b(iconv:convert("gbk", "utf-8", string:sub_string(Title, 1, Len-AfLen))).

get_url(Href, Num) ->
    StrLen = length(Href),
    ?l2b(string:substr(Href, Num, StrLen-Num)).
