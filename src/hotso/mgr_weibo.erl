%% ------------------------------------------------------------------
%% weibo站点(热搜)
%% ------------------------------------------------------------------
-module(mgr_weibo).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(spider_sec, (15*60+11)).

-include("common.hrl").
-include("todayhot.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(mgr_weibo, {
	list = []
    ,time = 0 %% weibo
}).
-define(URL, "https://s.weibo.com/top/summary?Refer=top_hot&topnav=1&wvr=6").
-define(HOST, "https://s.weibo.com").
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
	?INFO("start mgr_weibo",[]),
    erlang:send_after(32*1000, self(), start_spider),
	?INFO("finish mgr_weibo",[]),
	Today = util:today(),
    NewsL = api_todayhot:get_node_news(?todayhot_class_hotso, ?todayhot_node_weibo, Today),
	Sec = Today+86400 - util:now(),
	erlang:send_after(Sec*1000, self(), zero_up),
    {ok, #mgr_weibo{list=NewsL}}.
    
handle_call(Request, From, State) ->
	case catch do_handle_call(Request, From, State) of
		{reply, Reply, State} ->
			{reply, Reply, State};
		Reason ->
			?ERR("mgr_weibo Request ~w Reason ~w",[Request, Reason]),
			{reply, error, State}
	end.

handle_cast(Msg, State) ->
	case catch do_handle_cast(Msg, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_weibo Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
	case catch do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_weibo Request ~w Reason ~w",[Info, Reason]),
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
do_handle_info(zero_up, State=#mgr_weibo{}) ->
	erlang:send_after(86400*1000, self(), zero_up),
	{noreply, State#mgr_weibo{list=[]}};		
do_handle_info(start_spider, State=#mgr_weibo{}) ->
	Now = util:now(),
	do_start_spider(State, Now);
do_handle_info(_Msg, State) ->
	{noreply, State}.
%% 热搜
do_start_spider(State=#mgr_weibo{list = OldNewsL}, Now) ->
     case ibrowse:send_req(?URL, [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			NNews = parse_body(ResponseBody, OldNewsL, Now),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_hotso, ?todayhot_node_weibo, NNews, Now}), ignored),
			?INFO("weibo NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State#mgr_weibo{list = OldNewsL++NNews}};
		_Err ->
			?ERR("fail ~w", [_Err]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State}
    end.

parse_body(Body, List, Now) ->
	{_, [_|Data]} = re:run(Body, "href=\"/weibo(.*?)\" target=\"_blank\">(.*?)</a>", [{capture, first, list}, global, unicode]),
	{_, CountL} = re:run(Body, "<span>(.*?)</span>", [{capture, first, list}, global, unicode]),
	NTitleL = do_parse_body(Data, CountL, []),
	{AddNewsL, NewHotList} = new_add(NTitleL, List, Now, [], []),
	api_todayhot:insert_new_hot(?todayhot_class_hotso, ?todayhot_node_weibo, NewHotList),
	AddNewsL.

do_parse_body([[Body]|Data], [[Count]|CL], AddL) ->
    %% 正则获取
    % ?INFO("Body~ts",[Body]),
    {_, [[Href]]} = re:run(Body, "href=\"(.*?)\"", [{capture, first, list}, global, unicode]),
    {_, [[Title]]} = re:run(Body, "target=\"_blank\">(.*?)</a>", [{capture, first, list}, global, unicode]),
	NHref = get_param(Href, 7, 0),
	NTitle = get_param(Title, length("target=\"_blank\">")+1, length("/a>")),	
	NCount = get_param(Count, length("<span>")+1, length("</span>")-1),
    do_parse_body(Data, CL, [{NTitle, NHref, NCount}|AddL]);
do_parse_body([], _, AddL) -> AddL.

get_param(Href, Num, ANum) ->
    StrLen = length(Href),
    ?l2b(string:substr(Href, Num, StrLen-Num-ANum)).

new_add([], _, _Now, AddNewsL, NewHotList) ->
    {lists:reverse(AddNewsL), NewHotList};
new_add([{Title, Url, Count}|CurData], OldList, Now, AddNewsL, NewHotList) ->
	NUrl = ?l2b(?HOST++?b2l(Url)),
	TNews = #todayhot_news{
		class = ?todayhot_class_hotso,
		node_id = ?todayhot_node_weibo, sub_news=[#todayhot_sub_news{title = Title, url=NUrl, time=Now, count=Count}]
		, abstract = <<"">>, time=Now
    },
    case api_todayhot:is_exist(Title, OldList) of
        false ->
            ?INFO("Title~ts Url~ts, Count~ts", [Title, NUrl, Count]),
            new_add(CurData, OldList, Now, [TNews|AddNewsL], [TNews|NewHotList]);
        _ ->
            new_add(CurData, OldList, Now, AddNewsL, [TNews|NewHotList])
    end.