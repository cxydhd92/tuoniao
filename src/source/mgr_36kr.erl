%% ------------------------------------------------------------------
%% 36kr站点(快讯和24小时热榜)
%% ------------------------------------------------------------------
-module(mgr_36kr).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(spider_sec1, 15*60).
-define(spider_sec2, 54*60).

-include("common.hrl").
-include("todayhot.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(mgr_36kr, {
	list = []
    ,time1 = 0 %% 36kr快讯
    ,time2 = 0 %% 36kr热榜
}).
-define(URL1, "https://36kr.com/pp/api/newsflash?b_id=0&per_page=").
-define(Host1, "https://36kr.com/newsflashes/").
-define(URL2, "https://36kr.com/").
-define(Host2, "https://36kr.com").
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
	?INFO("start mgr_36kr",[]),
    erlang:send_after(30*1000, self(), start_spider1),
    erlang:send_after(60*1000, self(), start_spider2),
	?INFO("finish mgr_36kr",[]),
    Time1 = api_todayhot:get_node_up_time(?todayhot_class_tech, ?todayhot_node_36kr_1),
    NewsL = api_todayhot:get_node_news(?todayhot_class_tech, ?todayhot_node_36kr_2),
    Today = util:today(),
	Sec = Today+86400 - util:now(),
	erlang:send_after(Sec*1000, self(), zero_up),
    {ok, #mgr_36kr{time1 = Time1, list=NewsL}}.
    
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
do_handle_info(zero_up, State=#mgr_36kr{list=List}) ->
	EndTime = util:today() - ?LIST_END_TIME,
	NList = api_todayhot:get_end_time_news(List, EndTime, []),
	erlang:send_after(86400*1000, self(), zero_up),
	{noreply, State#mgr_36kr{list=NList}};		
do_handle_info(start_spider1, State=#mgr_36kr{time1=OldTime}) ->
	Now = util:now(),
	TodayZero = util:today(),
    do_start_spider1(State, OldTime, Now, TodayZero, 20);
do_handle_info(start_spider2, State=#mgr_36kr{}) ->
	Now = util:now(),
	do_start_spider2(State, Now);
do_handle_info(_Msg, State) ->
	{noreply, State}.
%% 24热榜
do_start_spider2(State=#mgr_36kr{list = OldNewsL}, Now) ->
     case ibrowse:send_req(?URL2, [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			NNews = parse_body2(ResponseBody, OldNewsL, Now),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_tech, ?todayhot_node_36kr_2, NNews, Now}), ignored),
			?INFO("36kr 24hour NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec2*1000, self(), start_spider2),
			{noreply, State#mgr_36kr{list = OldNewsL++NNews}};
		_Err ->
			?ERR("fail ~w", [_Err]),
			erlang:send_after(?spider_sec2*1000, self(), start_spider2),
			{noreply, State}
    end.

parse_body2(Body, List, Now) ->
    %% 正则获取前2名信息
    case catch re:run(Body, "<a class=\"hotlist-item-toptwo-title\" .*?>(.*?)</a>", [{capture, first, list}, global, unicode]) of
        {match, Ret1} ->
            {_, [[Href1], [Href2]]} = re:run(Ret1, "href=\"(.*?)\"", [{capture, first, list}, global]),
            Url1 = ?l2b(?Host2++get_param(Href1)), 
            Url2 = ?l2b(?Host2++get_param(Href2)),
            {_, [[Title1],[Title2]]} = re:run(Ret1, "<p class=\"ellipsis-2 weight-bold\">(.*?)</p>", [{capture, first, list}, global,unicode]),
            NTitle1 = get_title(Title1, length("<p class=\"ellipsis-2 weight-bold\">"), length("</p>")),
            NTitle2 = get_title(Title2, length("<p class=\"ellipsis-2 weight-bold\">"), length("</p>")),
            Toptwo = [{NTitle2, Url2}, {NTitle1, Url1}],
            OtherL = get_other(Body),
            {AddNewsL, NewHotList} = new_add(OtherL++Toptwo, List, Now, [], []),
            api_todayhot:insert_new_hot(?todayhot_class_tech, ?todayhot_node_36kr_2, NewHotList),
            AddNewsL;
        _ ->
            []
    end.

new_add([], _, _Now, AddNewsL, NewHotList) ->
    {lists:reverse(AddNewsL),  NewHotList};
new_add([{Title, Url}|CurData], OldList, Now, AddNewsL, NewHotList) ->
	TNews = #todayhot_news{
		class = ?todayhot_class_tech,
		node_id = ?todayhot_node_36kr_2, sub_news=[#todayhot_sub_news{title = Title, url=Url, time=Now}]
		, abstract = <<"">>, time=Now
    },
    case api_todayhot:is_exist(Title, OldList) of
        false ->
            ?INFO("Title~ts", [Title]),
            new_add(CurData, OldList, Now, [TNews|AddNewsL], [TNews|NewHotList]);
        _ ->
            new_add(CurData, OldList, Now, AddNewsL, [TNews|NewHotList])
    end.

get_other(Body) ->
    {match, Ret} = re:run(Body, "<a class=\"hotlist-item-other-title ellipsis-2 weight-bold\" .*?>(.*?)</a>", [{capture, first, list}, global, unicode]), 
    {_, HrefL} = re:run(Ret, "href=\"(.*?)\"", [{capture, first, list}, global]),
    {_, TitleL} = re:run(Ret, "target=\"_blank\" rel=\"noopener noreferrer\">(.*?)</a>", [{capture, first, list}, global,unicode]),
    UrlL = get_urls(HrefL, []),
    NTitleL = get_titles(TitleL, UrlL, []),
    NTitleL.

get_titles([], _, TitleL) -> TitleL;
get_titles([[Title]|T], [Url|UT],TitleL) ->
    NTitle = get_title(Title, length("target=\"_blank\" rel=\"noopener noreferrer\">"), length("</a>")),
    get_titles(T, UT, [{NTitle, Url}|TitleL]).

get_urls([], Urls) -> lists:reverse(Urls);
get_urls([[Href]|T], Urls) ->
    Url1 = ?l2b(?Host2++get_param(Href)),
    get_urls(T, [Url1|Urls]).

get_title(Title, PreLen, AfLen) ->
    Len = length(Title),
    ?l2b(string:sub_string(Title, PreLen+1, Len-AfLen)).

get_param(Href) ->
    StrLen = length(Href),
    string:substr(Href, 7, StrLen-7).

%% 每日快讯       
do_start_spider1(State, OldTime, Now, TodayZero, Num) ->
    case ibrowse:send_req(?URL1++?i2l(Num), [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			BodyJsonBin = list_to_binary(ResponseBody),
			BodyTerm = jsx:decode(BodyJsonBin),
			{NNews, Time} = parse_body(BodyTerm, OldTime, Now, TodayZero),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_tech, ?todayhot_node_36kr_1, NNews, Time}), ignored),
			?INFO("NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec1*1000, self(), start_spider1),
			{noreply, State#mgr_36kr{time1 = Time}};
		_ ->
			?ERR("URL ~w fail", [?URL1]),
			erlang:send_after(?spider_sec1*1000, self(), start_spider1),
			{noreply, State}
	end.

    
parse_body(Body, OldTime, Now, TodayZero) ->
	case proplists:get_value(<<"data">>, Body, undefined) of
		Data when is_list(Data) ->
            Items = proplists:get_value(<<"items">>, Data, []),
			do_parse_data(Items, OldTime, [], OldTime, Now, TodayZero);
		_ ->
			{[], OldTime}
	end.
	
do_parse_data([], _OldTime, News, Time,_, _) ->
	{News, Time};
do_parse_data([Data|T], OldTime, News, NTime, Now, TodayZero) ->
	UpdateTime = proplists:get_value(<<"created_at">>, Data),
	% Order = proplists:get_value(<<"order">>, Data),
	StrTime = ?b2l(UpdateTime),
	[Date, HTime] = string:tokens(StrTime, " "),
	[Year, Month, Day] = string:tokens(Date, "-"),
	[Hour, Minu, Sec] = string:tokens(HTime, ":"),
	NewTime = util:datetime_to_timestamp({{?l2i(Year), ?l2i(Month),?l2i(Day)},{?l2i(Hour), ?l2i(Minu),?l2i(Sec)}}),
	?INFO("NewTime~w OldTime~w",[NewTime, OldTime]),
	case NewTime > OldTime andalso NewTime>=TodayZero of
		true -> %% 最新的并且是今天的新闻才存储
			Param = proplists:get_value(<<"id">>, Data),
			Url = ?l2b(?Host1 ++ ?i2l(Param)),
            Title = proplists:get_value(<<"title">>, Data),
            ?INFO("Title~ts",[Title]),
			Abstract = proplists:get_value(<<"description">>, Data),
			TNews = #todayhot_news{
				class = ?todayhot_class_tech,
				node_id = ?todayhot_node_36kr_1, sub_news=[#todayhot_sub_news{title = Title, url=Url, time=Now}]
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