%% ------------------------------------------------------------------
%% tmt站点管理进程
%% ------------------------------------------------------------------
-module(mgr_tmt).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(spider_sec, 31*60).
-define(spider_sec1, (55*60+8)).
-include("common.hrl").
-include("todayhot.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(mgr_tmt, {
	list = []
	,time = 0
}).
-define(URL, "https://www.tmtpost.com/httpsserver/common/get?url=/v1/lists/home&data=offset=2&limit=").
-define(Host, "https://www.tmtpost.com").
-define(URL1, "https://www.tmtpost.com/hot").
-define(Host1, "https://www.tmtpost.com").

-define(MAX_LIST, 50).

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
	?INFO("start mgr_tmt",[]),
	erlang:send_after(27*1000, self(), start_spider),
	erlang:send_after(28*1000, self(), start_spider1),
	?INFO("finish mgr_tmt",[]),
    Time = api_todayhot:get_node_up_time(?todayhot_class_tech, ?todayhot_node_tmt),
    List = api_todayhot:get_node_news_num(?todayhot_class_tech, ?todayhot_node_tmt_hot, ?MAX_LIST),
    Today = util:today(),
	Sec = Today+86400 - util:now(),
	erlang:send_after(Sec*1000, self(), zero_up),
	?INFO("Time~w List ~w",[Time, length(List)]),
    {ok, #mgr_tmt{time = Time, list =List}}.

handle_call(Request, From, State) ->
	case catch do_handle_call(Request, From, State) of
		{reply, Reply, State} ->
			{reply, Reply, State};
		Reason ->
			?ERR("mgr_tmt Request ~w Reason ~w",[Request, Reason]),
			{reply, error, State}
	end.

handle_cast(Msg, State) ->
	case catch do_handle_cast(Msg, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_tmt Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
	case catch do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_tmt Request ~w Reason ~w",[Info, Reason]),
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
do_handle_info(zero_up, State=#mgr_tmt{list=List}) ->
	NList = api_todayhot:get_end_num_news(List, ?MAX_LIST, []),
	erlang:send_after(86400*1000, self(), zero_up),
	{noreply, State#mgr_tmt{list=NList}};	
do_handle_info(start_spider1, State=#mgr_tmt{}) ->
	Now = util:now(),
	do_start_spider1(State, Now);
do_handle_info(start_spider, State=#mgr_tmt{time=OldTime}) ->
	Now = util:now(),
	TodayZero = util:today(),
	do_start_spider(State, OldTime, Now, TodayZero, 20);
do_handle_info(_Msg, State) ->
	{noreply, State}.

do_start_spider1(State=#mgr_tmt{list = OldNewsL}, Now) ->
     case ibrowse:send_req(?URL1, [{"User-Agent","Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.132 Safari/537.36"}], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			NNews = parse_body1(ResponseBody, OldNewsL, Now),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_tech, ?todayhot_node_tmt_hot, NNews, Now}), ignored),
			?INFO("tmt_hot NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec1*1000, self(), start_spider1),
			{noreply, State#mgr_tmt{list = OldNewsL++NNews}};
		_Err ->
			?ERR("fail ~w", [_Err]),
			erlang:send_after(?spider_sec1*1000, self(), start_spider1),
			{noreply, State}
    end.

parse_body1(Body, List, Now) ->
    %% 正则获取
    {_, TitleHrefData} = re:run(Body, "<div class=\"pic\"><a target=\"_blank\" href=\"(.*?)\" title=\"(.*?)\">", [{capture, first, list}, global, unicode]),
    {_, IntroData} = re:run(Body, "<p class=\"intro\">(.*?)</p>", [{capture, first, list}, global, unicode]),
    {_, ImgData} = re:run(Body, "<img alt=\"(.*?)\" src=\"(.*?)\"", [{capture, first, list}, global, unicode]),
    {_, SourceData} = re:run(Body, "<a title=\"(.*?)\"", [{capture, first, list}, global, unicode]),
    
    TitleUrlL = build_title_url(TitleHrefData, []),
    NIntroData = build_intro(IntroData, TitleUrlL, []),
    NData = build_img(ImgData, NIntroData, []),
    NData1 = build_source(SourceData, NData, []),
    AddNewsL = new_add(NData1, List, Now, []),
    AddNewsL.

build_source([], _, Data) ->
	lists:reverse(Data);
build_source([[SourceData]|T], [{Href, Title, Intro, Img}|OldData], Data) ->
	Source = get_param(SourceData, length("<a title=\"")+1, 0),	
	build_source(T, OldData, [{Href, Title, Intro, Img, Source}|Data]).

build_img([], _, Data) ->
	lists:reverse(Data);
build_img([[ImgData]|T], [{Href, Title, Intro}|OldData], Data) ->
	{_, [[Img]]} = re:run(ImgData, "src=\"(.*?)\"", [{capture, first, list}, global, unicode]),
	NImg = get_param(Img, length("src=\"")+1, 0),	
	build_img(T, OldData, [{Href, Title, Intro, NImg}|Data]).

build_intro([], _, Data) ->
	lists:reverse(Data);
build_intro([[IntroData]|T], [{Href, Title}|TitleUrlL], Data) ->
	Intro = get_param(IntroData, length("<p class=\"intro\">")+1, length("</p>")-1),	
	build_intro(T, TitleUrlL, [{Href, Title, Intro}|Data]).

build_title_url([], TitleUrlL) ->
	lists:reverse(TitleUrlL);
build_title_url([[TitleHred]|T], TitleUrlL) ->
	{_, [[Href]]} = re:run(TitleHred, "href=\"(.*?)\"", [{capture, first, list}, global, unicode]),
	{_, [[Title]]} = re:run(TitleHred, "title=\"(.*?)\">", [{capture, first, list}, global, unicode]),
	NHref = get_param(Href, 7, 0),
	NTitle = get_param(Title, 8, 1),	
	build_title_url(T, [{NHref, NTitle}|TitleUrlL]).

new_add([], _, _Now, AddNewsL) ->
    AddNewsL;
new_add([{Url, Title, Intro, Img, Source}|CurData], OldList, Now, AddNewsL) ->
    case api_todayhot:is_exist(Title, OldList) of
        false ->
        	NUrl = ?l2b(?Host1 ++ ?b2l(Url)),
            ?INFO("Title~ts NUrl~ts", [Title, NUrl]),
            TNews = #todayhot_news{
				class = ?todayhot_class_tech,
				node_id = ?todayhot_node_tmt_hot, sub_news=[#todayhot_sub_news{title = Title, source=Source, url=NUrl, time=Now}]
				, abstract = Intro, time=Now, img=Img
            },
            new_add(CurData, OldList, Now, [TNews|AddNewsL]);
        _ ->
            new_add(CurData, OldList, Now, AddNewsL)
    end.

get_param(Href, Num, ANum) ->
    StrLen = length(Href),
    ?l2b(string:substr(Href, Num, StrLen-Num-ANum)).

do_start_spider(State, OldTime, Now, TodayZero, Num) ->
    case ibrowse:send_req(?URL++?i2l(Num), [{"User-Agent","Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.132 Safari/537.36"}], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			BodyJsonBin = list_to_binary(ResponseBody),
			BodyTerm = jsx:decode(BodyJsonBin),
			{NNews, Time} = parse_body(BodyTerm, OldTime, Now, TodayZero),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_tech, ?todayhot_node_tmt, NNews, Time}), ignored),
			?INFO("NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State#mgr_tmt{time = Time}};
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
	case proplists:get_value(<<"time_updated">>, Data) of
		undefined ->
			do_parse_data(T, OldTime, News, NTime, Now, TodayZero);
		_ ->
			NewTime = ?l2i(?b2l(proplists:get_value(<<"time_updated">>, Data))),
			?INFO("NewTime~w OldTime~w",[NewTime, OldTime]),
			case NewTime > OldTime andalso NewTime>=TodayZero of
				true -> %% 最新的并且是今天的新闻才存储
					Url = proplists:get_value(<<"short_url">>, Data),
					Title = proplists:get_value(<<"title">>, Data),
					Count = proplists:get_value(<<"number_of_reads">>, Data),
					ImgL = proplists:get_value(<<"thumb_image">>, Data),
					[ImgOL|_] = proplists:get_value(<<"original">>, ImgL),
					Img = proplists:get_value(<<"url">>, ImgOL),
					[Authors|_] = proplists:get_value(<<"authors">>, Data),
					Source = proplists:get_value(<<"username">>, Authors),
					?INFO("tmt Title ~ts", [Title]),
					Abstract = proplists:get_value(<<"summary">>, Data),
					TNews = #todayhot_news{
						class = ?todayhot_class_tech,
						node_id = ?todayhot_node_tmt, sub_news=[#todayhot_sub_news{title = Title, url=Url, count=Count,time=Now, source=Source}]
						, abstract = Abstract, time=Now, img=Img
					},
					NNTime = case NTime < NewTime of
						true -> NewTime;
						_ -> NTime
					end,
					do_parse_data(T, OldTime, [TNews|News], NNTime, Now, TodayZero);
				_ ->
					do_parse_data(T, OldTime, News, NTime, Now, TodayZero)
			end
	end.