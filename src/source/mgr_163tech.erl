%% ------------------------------------------------------------------
%% 163tech站点(24小时点击榜)
%% ------------------------------------------------------------------
-module(mgr_163tech).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(spider_sec, 40*60).

-include("common.hrl").
-include("todayhot.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(mgr_163tech, {
	list = []
    ,time = 0 %% 163tech
}).
-define(URL, "http://news.163.com/special/0001386F/rank_tech.html").
-define(Host, "http://news.163.com").
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
	?INFO("start mgr_163tech",[]),
    erlang:send_after(32*1000, self(), start_spider),
	?INFO("finish mgr_163tech",[]),
    NewsL = api_todayhot:get_node_news(?todayhot_class_tech, ?todayhot_node_163tech),
    Today = util:today(),
	Sec = Today+86400 - util:now(),
	erlang:send_after(Sec*1000, self(), zero_up),
    {ok, #mgr_163tech{list=NewsL}}.
    
handle_call(Request, From, State) ->
	case catch do_handle_call(Request, From, State) of
		{reply, Reply, State} ->
			{reply, Reply, State};
		Reason ->
			?ERR("mgr_163tech Request ~w Reason ~w",[Request, Reason]),
			{reply, error, State}
	end.

handle_cast(Msg, State) ->
	case catch do_handle_cast(Msg, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_163tech Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
	case catch do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_163tech Request ~w Reason ~w",[Info, Reason]),
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
do_handle_info(zero_up, State=#mgr_163tech{list=List}) ->
	EndTime = util:today() - ?LIST_END_TIME,
	NList = api_todayhot:get_end_time_news(List, EndTime, []),
	erlang:send_after(86400*1000, self(), zero_up),
	{noreply, State#mgr_163tech{list=NList}};		
do_handle_info(start_spider, State=#mgr_163tech{}) ->
	Now = util:now(),
	do_start_spider(State, Now);
do_handle_info(_Msg, State) ->
	{noreply, State}.
%% 24热榜
do_start_spider(State=#mgr_163tech{list = OldNewsL}, Now) ->
     case ibrowse:send_req(?URL, [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			NNews = parse_body(ResponseBody, OldNewsL, Now),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_tech, ?todayhot_node_163tech, NNews, Now}), ignored),
			?INFO("163tech NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State#mgr_163tech{list = OldNewsL++NNews}};
		_Err ->
			?ERR("fail ~w", [_Err]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State}
    end.

parse_body(Body, List, Now) ->
	NTitleL = do_parse_body(Body, List, Now, 1, []),
	{AddNewsL, NewHotList} = new_add(NTitleL, List, Now, [], []),
	api_todayhot:insert_new_hot(?todayhot_class_tech, ?todayhot_node_163tech, NewHotList),
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


new_add([], _, _Now, AddNewsL, NewHotList) ->
    {lists:reverse(AddNewsL), NewHotList};
new_add([{Title, Url}|CurData], OldList, Now, AddNewsL, NewHotList) ->
	TNews = #todayhot_news{
		class = ?todayhot_class_tech,
		node_id = ?todayhot_node_163tech, sub_news=[#todayhot_sub_news{title = Title, url=Url, time=Now}]
		, abstract = <<"">>, time=Now
    },
    case api_todayhot:is_exist(Title, OldList) of
        false ->
            ?INFO("Title~ts", [Title]),
            new_add(CurData, OldList, Now, [TNews|AddNewsL], [TNews|NewHotList]);
        _ ->
            new_add(CurData, OldList, Now, AddNewsL, [TNews|NewHotList])
    end.

get_title(Title, AfLen) ->
    Len = length(Title),
    ?l2b(iconv:convert("gbk", "utf-8", string:sub_string(Title, 1, Len-AfLen))).

get_url(Href, Num) ->
    StrLen = length(Href),
    ?l2b(string:substr(Href, Num, StrLen-Num)).
