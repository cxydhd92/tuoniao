%%%-------------------------------------------------------------------
%%% @author  Dhd
%%% @doc  订阅源进程
%%% @end  2020/5/2
%%%-------------------------------------------------------------------
-module(source).
-behaviour(gen_server).

-define(spider_sec, 10*60).
-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_class.hrl").
-include("cfg_news_source.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-export([
    start/1, stop/1, call/2, send/2
]).

-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).

-record(source, {
	list = []  %% [{SoureId, IsUser}]
	,mgr_pid 
	,today = [] %% 今日爬取标题[{SourceId, [Title,...]}]
}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc 开启子进程
start(Arg) ->
    gen_server:start_link(?MODULE, [Arg], []).

%% @doc 关闭子进程
stop(Pid) when is_pid(Pid) ->
    send(Pid, stop),
    ok;
stop(Other) ->
    ?ERR("stop error: bad argument ~w", [Other]),
    ok.

%% @doc 同步消息
call(Pid, Msg) when is_pid(Pid) ->
    case ?CALL(Pid, Msg) of
        {error, Error} ->
            ?ERR("call error: ~w; msg: ~w; node: ~w", [Error, Msg, node()]),
            false;
        Result ->
            Result
    end;
call(Other, Msg) ->
    ?ERR("call error: bad argument ~w; msg: ~w", [Other, Msg]),
    false.

%% @doc 异步消息
send(Pid, Msg) when is_pid(Pid) ->
    Pid ! Msg,
    ok;
send(Other, Msg) ->
    ?ERR("send error: bad argument ~w; msg: ~w", [Other, Msg]),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    try
        handle_init(Args)
    catch
        Type : Reason ->
            ?ERR("init: ~w, error: ~w, reason: ~w", [Args, Type, Reason]),
            {stop, Reason}
    end.

handle_call(Request, From, State) ->
    try
        do_handle_call(Request, From, State)
    catch
        Type : Reason ->
            ?ERR("handle_call: ~w, error: ~w, reason: ~w stacktrace: ~w", [Request, Type, Reason, erlang:get_stacktrace()]),
            {reply, false, State}
    end.

handle_cast(Msg, State) ->
    try
        do_handle_cast(Msg, State)
    catch
        Type : Reason ->
            ?ERR("handle_cast: ~w, error: ~w, reason: ~w stacktrace: ~w", [Msg, Type, Reason, erlang:get_stacktrace()]),
            {noreply, State}
    end.

handle_info(Info, State) ->
    try
        do_handle_info(Info, State)
    catch
        Type : Reason ->
            ?ERR("handle_info: ~w, error: ~w, reason: ~w stacktrace: ~w", [Info, Type, Reason, erlang:get_stacktrace()]),
            {noreply, State}
    end.

terminate(Msg, #source{mgr_pid=MgrPid}) ->
    try
        MgrPid ! {stop, self()}
    catch
        Type : Reason ->
            ?ERR("terminate: ~w, error: ~w, reason: ~w stacktrace: ~w", [Msg, Type, Reason, erlang:get_stacktrace()]),
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
handle_init([{MgrPid, SourceId, IsUser}]) ->
	?INFO("SourceId~w start",[SourceId]),
	State = #source{list = [{SourceId, IsUser}], mgr_pid = MgrPid},
	erlang:send_after(10*1000, self(), start_spider),
	Today = util:today(),
	Sec = Today+86400 - util:now(),
	erlang:send_after(Sec*1000*3, self(), zero_up),
	?INFO("SourceId~w end",[SourceId]),
	{ok, State}.

do_handle_call(_Request, _From, State) ->
    {reply, ok, State}.

do_handle_cast(_Msg, State)->
	{noreply, State}.

do_handle_info({add_source, SourceId, IsUser}, State=#source{list = List}) ->
	NList = [{SourceId, IsUser}|lists:keydelete(SourceId, 1, List)],
	{noreply, State#source{list = NList}};
do_handle_info(start_spider, State=#source{list = List, today = TodayData}) ->
	Now = util:now(),
	NTodayData = start_spider(List, TodayData, Now),
	erlang:send_after(?spider_sec*1000, self(), start_spider),
	{noreply, State#source{today = NTodayData}};
do_handle_info(zero_up, State=#source{}) ->
	erlang:send_after(86400*3*1000, self(), zero_up),
	{noreply, State#source{today=[]}};	
do_handle_info(_Msg, State) ->
	{noreply, State}.

start_spider([], TodayData, _Now) -> TodayData;
start_spider([{SourceId, IsUser}|List], TodayData, Now) ->
	NSubTodayData = case lists:keyfind(SourceId, 1, TodayData) of
		{_, SubTodayData} ->
			start_spider_f1(SubTodayData, SourceId, IsUser, Now);
		_ ->
			start_spider_f1([], SourceId, IsUser, Now)
	end,
	NTodayData = [{SourceId, NSubTodayData}|lists:keydelete(SourceId, 1, TodayData)],
	start_spider(List, NTodayData, Now).

start_spider_f1(TodayData, SourceId, ?false, Now) ->
	case cfg_news_source:get(SourceId) of
		Cfg = #cfg_news_source{type = Type, url = Url} when Type > 0 andalso Url =/= <<"">> ->
			do_start_spider(Cfg, TodayData, Now);
		_ ->
			TodayData
	end;
%% TODO:用户自定义源
start_spider_f1(TodayData, _SourceId, _, _Now) ->
	TodayData.

do_start_spider(Cfg=#cfg_news_source{type = 1}, TodayData, Now) ->
	do_start_spider_json(Cfg, TodayData, Now);
do_start_spider(Cfg=#cfg_news_source{type = 2}, TodayData, Now) ->
	do_start_spider_html(Cfg, TodayData, Now);
do_start_spider(Cfg=#cfg_news_source{type = 3}, TodayData, Now) ->
	do_start_spider_rss(Cfg, TodayData, Now);
do_start_spider(_Cfg, TodayData, _Now) ->
	TodayData.

new_add_rss(#cfg_news_source{class=Class, source_id=SourceId}, Item, Now) ->
	[#xmlText{value=TUrl}] = xmerl_xpath:string("/item/link/text()",Item),  
    [#xmlText{value=Title}] = xmerl_xpath:string("/item/title/text()",Item),  
    Source = case catch xmerl_xpath:string("/item/author/text()",Item) of
    	[#xmlText{value=AuthorValue}] -> AuthorValue;
    	_ -> <<"">>
    end,
    Abstract = case catch xmerl_xpath:string("/item/description/text()",Item) of
    	[#xmlText{value=AbstractValue}] -> AbstractValue;
    	_ -> <<"">>
    end,
    Count = case catch xmerl_xpath:string("/item/viewCount/text()",Item) of
    	[#xmlText{value=ViewCount}] -> parse_count(ViewCount);
    	_ -> <<"0">>
    end,
    NewTime = case catch xmerl_xpath:string("/item/pubDate/text()",Item) of
    	[#xmlText{value=DateValue}] -> util:date_format(4, DateValue);
    	_ -> Now
    end,
    #todayhot_news{
		class = Class,
		node_id = SourceId, title = Title, url=TUrl, news_time=NewTime, source=Source, count=Count
		, abstract = Abstract, time=Now, img = <<"">>
	}.

do_start_spider_rss(Cfg=#cfg_news_source{class=Class, source_id=SourceId, url = Url, is_top = IsTop}, TodayData, Now) ->
	case ibrowse:send_req(?b2l(Url), [], get) of
		{ok, "200", _ResponseHeaders, Body} ->
			{XmlDoc, _B} = xmerl_scan:string(Body),
			Items = xmerl_xpath:string("/rss/channel/item",XmlDoc),  
			{NNews, NTodayData, NewHotList} =  lists:foldl(fun(Item, {AccNews, AccTop, AccToday})->  
                                [#xmlText{value=Title}] = xmerl_xpath:string("/item/title/text()",Item), 
                                TNews = new_add_rss(Cfg, Item, Now),
                                case IsTop =:= ?true of
									true ->
										case lists:member(Title, AccToday) of
											false -> %% 
												{[TNews|AccNews], [TNews|AccTop], [Title|AccToday]};
											_ ->
												{AccNews, [TNews|AccTop], AccToday}
										end;
									_ ->
										case lists:member(Title, AccToday) of
											false -> %% 
												{[TNews|AccNews], AccTop, [Title|AccToday]};
											_ ->
												{AccNews, AccTop, AccToday}
										end
								end
                         end,  {[], [], TodayData}, Items),  
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, Class, SourceId, NNews, Now}), ignored),
			?IF(NewHotList=/=[], api_todayhot:insert_new_hot(Class, SourceId, lists:reverse(NewHotList)), ignored),
			?INFO("SourceId~w NNews len ~w", [SourceId, length(NNews)]),
			NTodayData;
		_Err ->
			?ERR("Url ~ts fail ~w", [Url, _Err]),
			TodayData
    end.

do_start_spider_html(Cfg=#cfg_news_source{class=Class, source_id=SourceId, url = Url}, TodayData, Now) ->
	case ibrowse:send_req(?b2l(Url), [], get) of
		{ok, "200", _ResponseHeaders, Body} ->
			#cfg_news_source{data=Data, container=ContainerF, title=TitleF, link_a=LinkA,
			desc=DescF, author=AuthorF, img=ImgF, count=CountF, time=TimeF} = Cfg,
			{_, Container} = re:run(Body, Data, [{capture, all_but_first, binary}, global]),
			{_, Item} = re:run(Container, ContainerF, [{capture, all_but_first, binary}, global]),
			{_, ItemTitleL} = re:run(Item, TitleF, [{capture, all_but_first, binary}, global]),
			{_, ItemLinkAL} = re:run(Item, LinkA, [{capture, all_but_first, binary}, global]),
			ItemDescL = get_html_data(DescF, Item),
			ItemAuthorFL = get_html_data(AuthorF, Item),
			ItemImgFL = get_html_data(ImgF, Item),
			ItemCountFL = get_html_data(CountF, Item),
			ItemTimeFL = get_html_data(TimeF, Item),
			% ?INFO("ItemTitleL~w",[ItemTitleL]),
			{NNews, NTodayData, NewHotList} = do_parse_html(Cfg, TodayData, [], [], Now, ItemTitleL, ItemLinkAL, ItemDescL, ItemAuthorFL, ItemImgFL, ItemCountFL, ItemTimeFL),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, Class, SourceId, NNews, Now}), ignored),
			?IF(NewHotList=/=[], api_todayhot:insert_new_hot(Class, SourceId, lists:reverse(NewHotList)), ignored),
			?INFO("SourceId~w NNews len ~w", [SourceId, length(NNews)]),
			NTodayData;
		_Err ->
			?ERR("Url ~ts fail ~w", [Url, _Err]),
			TodayData
    end.

get_html_data(DataFormat, Data) ->
	case DataFormat=:=<<"">> of
		true -> [];
		_ ->
			{_, ItemDescL} = re:run(Data, DataFormat, [{capture, all_but_first, binary}, global]),
			ItemDescL
	end.

get_html_item([]) -> {<<"">>, []};
get_html_item([[Item]|T]) -> {Item, T}.

new_add_html(Cfg, Title, LinkA, Now, ItemDescL, ItemAuthorFL, ItemImgFL, ItemCountFL, ItemTimeFL) ->
	#cfg_news_source{class = Class, source_id=SourceId, time_type=TimeType, link_pre=LinkPre} = Cfg,
	{TimeData, RtItemTimeL} = get_html_item(ItemTimeFL),
	{Abstract, RtItemDescL} = get_html_item(ItemDescL),
	{Source, RtItemAuthorFL} = get_html_item(ItemAuthorFL),
	{Img, RtItemImgFL} = get_html_item(ItemImgFL),
	{CountI, RtItemCountFL} = get_html_item(ItemCountFL),
	NewTime = util:date_format(TimeType, TimeData),
	TUrl = util:fbin(<<"~s~s"/utf8>>, [LinkPre, LinkA]),
	% Url = ?l2b(?Host ++ "/" ++ Param),
	Count = parse_count(CountI),
	TNews = #todayhot_news{
		class = Class,
		node_id = SourceId, title = Title, url=TUrl, news_time=NewTime, source=Source, count=Count
		, abstract = Abstract, time=Now, img = Img
	},
	{TNews, RtItemDescL, RtItemAuthorFL, RtItemImgFL, RtItemCountFL, RtItemTimeL}.

do_parse_html(Cfg, TodayData, News, TopL, _Now, [], _, _, _, _, _, _) ->
	{News, TodayData, TopL};
do_parse_html(Cfg=#cfg_news_source{is_top = IsTop}, TodayData, News, TopL, Now, [[Title]|ItemTitleL], [[LinkA]|ItemLinkAL], ItemDescL, ItemAuthorFL, ItemImgFL, ItemCountFL, ItemTimeFL) ->
	{TNews, RtItemDescL, RtItemAuthorFL, RtItemImgFL, RtItemCountFL, RtItemTimeL} = 
	new_add_html(Cfg, Title, LinkA, Now, ItemDescL, ItemAuthorFL, ItemImgFL, ItemCountFL, ItemTimeFL),				
	case IsTop =:= ?true of
		true ->
			case lists:member(Title, TodayData) of
				false -> %% 
					do_parse_html(Cfg, [Title|TodayData], [TNews|News],[TNews|TopL], Now, ItemTitleL, ItemLinkAL, RtItemDescL, RtItemAuthorFL, RtItemImgFL, RtItemCountFL, RtItemTimeL);
				_ ->
					do_parse_html(Cfg, TodayData, News, [TNews|TopL], Now, ItemTitleL, ItemLinkAL, RtItemDescL, RtItemAuthorFL, RtItemImgFL, RtItemCountFL, RtItemTimeL)
			end;
		_ ->
			case lists:member(Title, TodayData) of
				false -> %% 
					do_parse_html(Cfg, [Title|TodayData], [TNews|News], TopL, Now, ItemTitleL, ItemLinkAL, RtItemDescL, RtItemAuthorFL, RtItemImgFL, RtItemCountFL, RtItemTimeL);
				_ ->
					do_parse_html(Cfg, TodayData, News, TopL, Now, ItemTitleL, ItemLinkAL, RtItemDescL, RtItemAuthorFL, RtItemImgFL, RtItemCountFL, RtItemTimeL)
			end
	end.

do_start_spider_json(Cfg=#cfg_news_source{class=Class, source_id=SourceId, url = Url}, TodayData, Now) ->
    case ibrowse:send_req(?b2l(Url), [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			BodyJsonBin = list_to_binary(ResponseBody),
			BodyTerm = jsx:decode(BodyJsonBin),
			{NNews, NTodayData, NewHotList} = parse_body(Cfg, BodyTerm, Now, TodayData),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, Class, SourceId, NNews, Now}), ignored),
			?IF(NewHotList=/=[], api_todayhot:insert_new_hot(Class, SourceId, lists:reverse(NewHotList)), ignored),
			?INFO("SourceId~w NNews len ~w", [SourceId, length(NNews)]),
			NTodayData;
		_Err ->
			?ERR("Url ~ts fail ~w", [Url, _Err]),
			TodayData
	end.

    
get_data(Data, Body) ->
	DataStr = ?b2l(Data),
	DataL = string:tokens(DataStr, "|"),
	get_data_f1(DataL, Body).

get_data_f1([], Datas) -> Datas;
get_data_f1([Data|L], Body) ->
	case proplists:get_value(?l2b(Data), Body) of
		DataS when DataS =/= undefined ->
			get_data_f1(L, DataS);
		_ ->
			[]
	end.

parse_body(Cfg, Body, Now, TodayData) ->
	#cfg_news_source{data=Data} = Cfg,
	case get_data(Data, Body) of
		NewData when is_list(NewData) ->
			do_parse_data(Cfg, NewData, [], Now, TodayData, []);
		_ ->
			{[], TodayData, []}
	end.
	
parse_count(Count) ->
	case is_integer(Count) of
		true ->
			?l2b(?i2l(Count));
		_ ->
			Count
	end.

new_add_json(Cfg, Data) ->
	#cfg_news_source{class = Class, source_id=SourceId, link_pre=LinkPre, title=TitleF, link_a=LinkA, desc=DescF, author=AuthorF, 
		img = ImgF, count = CountF, time = Time, time_type=TimeType} = Cfg,
	TimeData = proplists:get_value(Time, Data),
	Now = util:now(),
	NewTime = ?IF(TimeData=:=undefined, Now, util:date_format(TimeType, TimeData)),
	Title = proplists:get_value(TitleF, Data),
	TUrl = util:fbin(<<"~s~s"/utf8>>, [LinkPre, proplists:get_value(LinkA, Data)]),
	% Url = ?l2b(?Host ++ "/" ++ Param),
	Abstract = proplists:get_value(DescF, Data, <<"">>),
	Source = proplists:get_value(AuthorF, Data, <<"">>),
	Img = proplists:get_value(ImgF, Data, <<"">>),
	Count = parse_count(proplists:get_value(CountF, Data, <<"0">>)),
	TNews = #todayhot_news{
		class = Class,
		node_id = SourceId, title = Title, url=TUrl, news_time=NewTime, source=Source, count=Count
		, abstract = Abstract, time=Now, img = Img
	},
	TNews.

do_parse_data(_, [], News, _, NTodayData, TopL) ->
	{News, NTodayData, TopL};
do_parse_data(Cfg, [Data|T], News, Now, TodayData, TopL) ->
	#cfg_news_source{title=TitleF, is_top=IsTop} = Cfg,
	Title = proplists:get_value(TitleF, Data),
	case IsTop =:= ?true of
		true ->
			TNews = new_add_json(Cfg, Data),
			case lists:member(Title, TodayData) of
				false -> %% 
					do_parse_data(Cfg, T, [TNews|News], Now, [Title|TodayData], [TNews|TopL]);
				_ ->
					do_parse_data(Cfg, T, News, Now, TodayData, [TNews|TopL])
			end;
		_ ->
			case lists:member(Title, TodayData) of
				false -> %% 
					TNews = new_add_json(Cfg, Data),
					do_parse_data(Cfg, T, [TNews|News], Now, [Title|TodayData], TopL);
				_ ->
					do_parse_data(Cfg, T, News, Now, TodayData, TopL)
			end
	end.