%% ------------------------------------------------------------------
%% zhihu站点(快讯和24小时热榜)
%% ------------------------------------------------------------------
-module(mgr_zhihu).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(spider_sec, 58*60).
-define(spider_sec1, 59*60).

-include("common.hrl").
-include("todayhot.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(mgr_zhihu, {
	list = []
    ,list1 = [] %% zhihu daily
}).
-define(URL, "https://www.zhihu.com/billboard").
-define(Host, "https://www.zhihu.com/").

-define(URL1, "https://daily.zhihu.com/").
-define(Host1, "https://daily.zhihu.com").

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
	?INFO("start mgr_zhihu",[]),
    erlang:send_after(32*1000, self(), start_spider),
    erlang:send_after(37*1000, self(), start_spider1),
	?INFO("finish mgr_zhihu",[]),
    NewsL = api_todayhot:get_node_news(?todayhot_class_answer, ?todayhot_node_zhihu),
    NewsL1 = api_todayhot:get_node_news(?todayhot_class_rb, ?todayhot_node_zhihu_daily),
    ?INFO("NewsL~w NewsL1~w",[length(NewsL), length(NewsL1)]),
    Today = util:today(),
	Sec = Today+86400 - util:now(),
	erlang:send_after(Sec*1000, self(), zero_up),
    {ok, #mgr_zhihu{list=NewsL, list1=NewsL1}}.

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
			?ERR("mgr_zhihu Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
	case catch do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_zhihu Request ~w Reason ~w",[Info, Reason]),
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
do_handle_info(zero_up, State=#mgr_zhihu{list=List, list1=List1}) ->
	EndTime = util:today() - ?LIST_END_TIME,
	NList = api_todayhot:get_end_time_news(List, EndTime, []),
	NList1 = api_todayhot:get_end_time_news(List1, EndTime, []),
	erlang:send_after(86400*1000, self(), zero_up),
	{noreply, State#mgr_zhihu{list=NList, list1=NList1}};		
do_handle_info(start_spider, State=#mgr_zhihu{}) ->
	Now = util:now(),
	do_start_spider(State, Now);
do_handle_info(start_spider1, State=#mgr_zhihu{}) ->
	Now = util:now(),
	do_start_spider1(State, Now);
do_handle_info(_Msg, State) ->
	{noreply, State}.

%% 日报
do_start_spider1(State=#mgr_zhihu{list1 = OldNewsL}, Now) ->
     case ibrowse:send_req(?URL1, [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			NNews = parse_body1(ResponseBody, OldNewsL, Now),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_rb, ?todayhot_node_zhihu_daily, NNews, Now}), ignored),
			?INFO("zhihu daily NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec1*1000, self(), start_spider1),
			{noreply, State#mgr_zhihu{list1 = OldNewsL++NNews}};
		_Err ->
			?ERR("fail ~w", [_Err]),
			erlang:send_after(?spider_sec1*1000, self(), start_spider1),
			{noreply, State}
    end.
parse_body1(Body, List, Now) ->
    %% 正则获取
    case catch re:run(Body, "<div class=\"box\">(.*?)</div>", [{capture, first, list}, global, unicode]) of
        {match, Ret1} ->
        	{_, HrefL} = re:run(Ret1, "href=\"(.*?)\"", [{capture, first, list}, global]),
        	{_, Imgs} = re:run(Ret1, "src=\"(.*?)\"", [{capture, first, list}, global]),
		    {_, TitleL} = re:run(Ret1, "<span class=\"title\">(.*?)</span>", [{capture, first, list}, global,unicode]),
		    UrlL = get_urls(HrefL, []),
		    ImgL = get_imgs(Imgs, UrlL, []),
		    NTitleL = get_titles(TitleL, ImgL, []),
		    SubTitleL = lists:sublist(NTitleL, 10),
            AddNewsL = new_add(SubTitleL, List, Now, []),
            AddNewsL;
        _ ->
            []
    end.


new_add([], _, _Now, AddNewsL) ->
    AddNewsL;
new_add([{Title, Url, Img}|CurData], OldList, Now, AddNewsL) ->
    case api_todayhot:is_exist(Title, OldList) of
        false ->
            ?INFO("Title~ts", [Title]),
            TNews = #todayhot_news{
				class = ?todayhot_class_rb,
				node_id = ?todayhot_node_zhihu_daily, sub_news=[#todayhot_sub_news{title = Title, url=Url, time=Now}]
				, abstract = <<"">>, time=Now, img =Img
            },
            new_add(CurData, OldList, Now, [TNews|AddNewsL]);
        _ ->
            new_add(CurData, OldList, Now, AddNewsL)
    end.

get_titles([], _, TitleL) -> lists:reverse(TitleL);
get_titles([[Title]|T], [{Url, Img}|UT],TitleL) ->
    NTitle = get_title(Title, ">", length("</span")),
    get_titles(T, UT, [{NTitle, Url, Img}|TitleL]).

get_imgs([], _, ImgL) -> lists:reverse(ImgL);
get_imgs([[ImgSrc]|T], [Url|Urls], ImgL) ->
    Img = get_param(ImgSrc, 6),
    get_imgs(T, Urls, [{Url, Img}|ImgL]).

get_urls([], Urls) -> lists:reverse(Urls);
get_urls([[Href]|T], Urls) ->
    Url1 = ?l2b(?Host1++get_param(Href, 7)),
    get_urls(T, [Url1|Urls]).

get_title(Title, Token, AfLen) ->
    [_, T|_] = string:tokens(Title, Token),
    Len = length(T),
    ?l2b(string:substr(T, 1, Len-AfLen)).

get_param(Href, Len) ->
    StrLen = length(Href),
    string:substr(Href, Len, StrLen-Len).


%% 热榜
do_start_spider(State=#mgr_zhihu{list = OldNewsL}, Now) ->
     case ibrowse:send_req(?URL, [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			{NNews, _Time} = parse_body(ResponseBody, OldNewsL, Now),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_answer, ?todayhot_node_zhihu, NNews, Now}), ignored),
			?INFO("zhihu NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State#mgr_zhihu{list = OldNewsL++NNews}};
		_ ->
			?ERR("URL ~w fail", [?URL]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State}
    end.

parse_body(Body, List, Now) ->
    %% 正则获取
    case catch re:run(Body, "{\"initialState\"((.|\n)*?)\"subAppName\":\"mobile\"}", [{capture, first, list}, global, unicode]) of
        {match, Ret1} ->
        	BodyJsonBin = list_to_binary(Ret1),
			BodyTerm = jsx:decode(BodyJsonBin),
			case proplists:get_value(<<"initialState">>, BodyTerm, undefined) of
				OData when is_list(OData) ->
					Data0 = proplists:get_value(<<"topstory">>, OData),
					Data = proplists:get_value(<<"hotList">>, Data0),
					% Feeds  = proplists:get_value(<<"content">>, Data, []),
					{News, NTime, HotList} = do_parse_data(Data, List, [], Now, []),
					api_todayhot:insert_new_hot(?todayhot_class_answer, ?todayhot_node_zhihu, HotList),
					{News, NTime};
				_ ->
					{[], Now}
			end;
        _ ->
            {[], Now}
    end.
	
do_parse_data([], _List, News, Time, NewHotList) ->
	{News, Time, lists:reverse(NewHotList)};
do_parse_data([Data0|T], OList, News, Now, NewHotList) ->
	Data = proplists:get_value(<<"target">>, Data0),
	TextT = proplists:get_value(<<"titleArea">>, Data),
	Title = proplists:get_value(<<"text">>, TextT),
	TextL = proplists:get_value(<<"link">>, Data, <<"">>),
	Url = proplists:get_value(<<"url">>, TextL, <<"">>),
	TextE = proplists:get_value(<<"excerptArea">>, Data, <<"">>),
	Abstract = proplists:get_value(<<"text">>, TextE, <<"">>),
	TextI = proplists:get_value(<<"imageArea">>, Data),
	Img = proplists:get_value(<<"url">>, TextI, <<"">>),
	Source = proplists:get_value(<<"source">>, Data, <<"">>),
	TNews = #todayhot_news{
		class = ?todayhot_class_answer,
		node_id = ?todayhot_node_zhihu, sub_news=[#todayhot_sub_news{title = Title, url=Url, time=Now, source=Source}]
		, abstract = Abstract, time=Now, img = Img
	},
	case api_todayhot:is_exist(Title, OList)  of
		false -> %% 最新的新闻才存储
			?INFO("Title~ts",[Title]),
			do_parse_data(T, OList, [TNews|News], Now, [TNews|NewHotList]);
		_ ->
			do_parse_data(T, OList, News, Now, [TNews|NewHotList])
	end.