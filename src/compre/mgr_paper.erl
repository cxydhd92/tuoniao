%% ------------------------------------------------------------------
%% paper站点(快讯和24小时热榜)
%% ------------------------------------------------------------------
-module(mgr_paper).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(spider_sec, 58*60).

-include("common.hrl").
-include("todayhot.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(mgr_paper, {
	list = []
    ,time = 0 %% paper
}).
-define(URL, "https://www.thepaper.cn/").
-define(Host, "https://www.thepaper.cn/").
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
	?INFO("start mgr_paper",[]),
    erlang:send_after(31*1000, self(), start_spider),
	?INFO("finish mgr_paper",[]),
    NewsL = api_todayhot:get_node_news(?todayhot_class_compre, ?todayhot_node_paper),
    Today = util:today(),
	Sec = Today+86400 - util:now(),
	erlang:send_after(Sec*1000, self(), zero_up),
    {ok, #mgr_paper{list=NewsL}}.

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

do_handle_info(zero_up, State=#mgr_paper{list=List}) ->
	EndTime = util:today() - ?LIST_END_TIME,
	NList = api_todayhot:get_end_time_news(List, EndTime, []),
	erlang:send_after(86400*1000, self(), zero_up),
	{noreply, State#mgr_paper{list=NList}};
do_handle_info(start_spider, State=#mgr_paper{}) ->
	Now = util:now(),
	do_start_spider(State, Now);
do_handle_info(_Msg, State) ->
	{noreply, State}.
%% 24热榜
do_start_spider(State=#mgr_paper{list = OldNewsL}, Now) ->
     case ibrowse:send_req(?URL, [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			NNews = parse_body(ResponseBody, OldNewsL, Now),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_compre, ?todayhot_node_paper, NNews, Now}), ignored),
			?INFO("paper NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State#mgr_paper{list = OldNewsL++NNews}};
		_Err ->
			?ERR("fail ~w", [_Err]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State}
    end.

parse_body(Body, List, Now) ->
    %% 正则获取
    case catch re:run(Body, "<ul class=\"list_hot\" id=\"listhot0\">(.|\n)*</ul>", [{capture, first, list}, global, unicode]) of
        {match, Ret1} ->
            {_, HrefL} = re:run(Ret1, "href=\"(.*?)\"", [{capture, first, list}, global]),
		    {_, TitleL} = re:run(Ret1, "target=\"_blank\" href=\"(.*?)\">(.*?)</a>", [{capture, first, list}, global,unicode]),
		    UrlL = get_urls(HrefL, []),
		    NTitleL = get_titles(TitleL, UrlL, []),
            {AddNewsL, HotList} = new_add(NTitleL, List, Now, [], []),
            api_todayhot:insert_new_hot(?todayhot_class_compre, ?todayhot_node_paper, HotList),
            AddNewsL;
        _ ->
            []
    end.

new_add([], _, _Now, AddNewsL, HotList) ->
    {lists:reverse(AddNewsL), HotList};
new_add([{Title, Url}|CurData], OldList, Now, AddNewsL, HotList) ->
	TNews = #todayhot_news{
		class = ?todayhot_class_compre,
		node_id = ?todayhot_node_paper, sub_news=[#todayhot_sub_news{title = Title, url=Url, time=Now}]
		, abstract = <<"">>, time=Now
    },
    case api_todayhot:is_exist(Title, OldList) of
        false ->
            ?INFO("Title~ts", [Title]),
            new_add(CurData, OldList, Now, [TNews|AddNewsL], [TNews|HotList]);
        _ ->
            new_add(CurData, OldList, Now, AddNewsL, [TNews|HotList])
    end.

get_titles([], _, TitleL) -> TitleL;
get_titles([[Title]|T], [Url|UT],TitleL) ->
    NTitle = get_title(Title, ">", length("</a")),
    get_titles(T, UT, [{NTitle, Url}|TitleL]).

get_urls([], Urls) -> lists:reverse(Urls);
get_urls([[Href]|T], Urls) ->
    Url1 = list_to_binary(?Host++get_param(Href)),
    get_urls(T, [Url1|Urls]).

get_title(Title, Token, AfLen) ->
    [_, T|_] = string:tokens(Title, Token),
    Len = length(T),
    list_to_binary(string:substr(T, 1, Len-AfLen)).

get_param(Href) ->
    StrLen = length(Href),
    string:substr(Href, 7, StrLen-7).
