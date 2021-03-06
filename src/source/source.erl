%%%-------------------------------------------------------------------
%%% @author  Dhd
%%% @doc  订阅源进程
%%% @end  2020/5/2
%%%-------------------------------------------------------------------
-module(source).
-behaviour(gen_server).

-define(spider_sec, 600).
-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_class.hrl").
-include("cfg_news_source.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-export([
    start/1, stop/1, call/2, send/2
	,check/1
]).

-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).

-record(source, {
	list = []  %% [{SoureId, IsUser}]
	,mgr_pid 
	,titles = [] %% 最新100条爬取标题[{SourceId, [Title,...]}]
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
        Type : Reason : _Stack ->
            ?ERR("init: ~w, error: ~w, reason: ~w", [Args, Type, Reason]),
            {stop, Reason}
    end.

handle_call(Request, From, State) ->
    try
        do_handle_call(Request, From, State)
    catch
        Type : Reason : Stack ->
            ?ERR("handle_call: ~w, error: ~w, reason: ~w stacktrace: ~w", [Request, Type, Reason, Stack]),
            {reply, false, State}
    end.

handle_cast(Msg, State) ->
    try
        do_handle_cast(Msg, State)
    catch
        Type : Reason : Stack ->
            ?ERR("handle_cast: ~w, error: ~w, reason: ~w stacktrace: ~w", [Msg, Type, Reason, Stack]),
            {noreply, State}
    end.

handle_info(Info, State) ->
    try
        do_handle_info(Info, State)
    catch
        Type : Reason : Stack ->
            ?ERR("handle_info: ~w, error: ~w, reason: ~w stacktrace: ~w", [Info, Type, Reason, Stack]),
            {noreply, State}
    end.

terminate(Msg, #source{mgr_pid=MgrPid}) ->
    try
        MgrPid ! {stop, self()}
    catch
        Type : Reason : Stack ->
            ?ERR("terminate: ~w, error: ~w, reason: ~w stacktrace: ~w", [Msg, Type, Reason, Stack]),
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
	% Sec = Today+86400 - util:now(),
	% erlang:send_after(Sec*1000*3, self(), zero_up),
	% #cfg_news_source{class = Class} = api_todayhot:get_node(SourceId),
	#cfg_news_source{check_num = CheckNum} = api_todayhot:get_node(SourceId),
	News = api_todayhot:get_node_news_num(SourceId, Today, CheckNum),
	TitleL = [Title||#todayhot_news{title=Title}<-News],
	% gc_timer(),
	?INFO("SourceId~w titlelen ~w end",[SourceId, length(TitleL)]),
	{ok, State#source{titles = [{SourceId, TitleL}]}}.

% gc_timer() ->
% 	erlang:send_after(900*1000, self(), gc_loop_timer),
% 	ok.

do_handle_call(_Request, _From, State) ->
    {reply, ok, State}.

do_handle_cast(_Msg, State)->
	{noreply, State}.

do_handle_info({add_source, SourceId, IsUser}, State=#source{list = List, titles = ATitleL}) ->
	NList = [{SourceId, IsUser}|lists:keydelete(SourceId, 1, List)],
	Today = util:today(),
	#cfg_news_source{check_num = CheckNum} = api_todayhot:get_node(SourceId),
	News = api_todayhot:get_node_news_num(SourceId, Today, CheckNum),
	TitleL = [Title||#todayhot_news{title=Title}<-News],
	?INFO("add_source SourceId~w titlelen ~w end",[SourceId, length(TitleL)]),
	{noreply, State#source{list = NList, titles = [{SourceId, TitleL}|lists:keydelete(SourceId, 1, ATitleL)]}};
do_handle_info(start_spider, State=#source{list = List, titles = TodayData}) ->
	Now = util:now(),
	NTodayData = start_spider(List, TodayData, Now),
	erlang:send_after(?spider_sec*1000, self(), start_spider),
	% self() ! gc_loop_timer,
	{noreply, State#source{titles = NTodayData}};
do_handle_info(gc_loop_timer, State=#source{}) ->
	erlang:garbage_collect(),
	?INFO("gc_loop_timer",[]),
	% gc_timer(),
	{noreply, State};	
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
	#cfg_news_source{check_num = CheckNum} = api_todayhot:get_node(SourceId),
	NTodayData = [{SourceId, lists:sublist(NSubTodayData,CheckNum)}|lists:keydelete(SourceId, 1, TodayData)],
	erlang:garbage_collect(),
	start_spider(List, NTodayData, Now).

start_spider_f1(TodayData, SourceId, ?false, Now) ->
	case api_todayhot:get_node(SourceId) of
		Cfg = #cfg_news_source{class = Class, type = Type, url = Url} when Type > 0 andalso Url =/= <<"">> ->
		   ?IF(lists:member(Class,[12,13,14]), receive after 2000 -> true end, ignored),
			case catch do_start_spider(Cfg, TodayData, Now) of
				NTodayData when is_list(NTodayData) ->
					NTodayData;
		        _Err ->
		            ?ERR("SourceId: ~w, reason: ~w ", [SourceId, _Err]),
		            TodayData
		    end;
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
do_start_spider(Cfg=#cfg_news_source{type = 4}, TodayData, Now) ->
	do_start_spider_html_json(Cfg, TodayData, Now);
do_start_spider(_Cfg, TodayData, _Now) ->
	TodayData.

new_add_rss(#cfg_news_source{class=Class, source_id=SourceId}, Item, Now) ->
	[#xmlText{value=TUrl}] = xmerl_xpath:string("/item/link/text()",Item),  
    [#xmlText{value=Title}] = xmerl_xpath:string("/item/title/text()",Item),  
    Source = case catch xmerl_xpath:string("/item/author/text()",Item) of
    	[#xmlText{value=AuthorValue}] -> ?c2b(AuthorValue);
    	_ -> <<"">>
    end,
    % Abstract = case catch xmerl_xpath:string("/item/description/text()",Item) of
    % 	[#xmlText{value=AbstractValue}] -> ?c2b(AbstractValue);
    % 	_ -> <<"">>
    % end,
    Count = case catch xmerl_xpath:string("/item/viewCount/text()",Item) of
    	[#xmlText{value=ViewCount}] -> parse_count(SourceId, ViewCount);
    	_ -> <<"0">>
    end,
    NewTime = case catch xmerl_xpath:string("/item/pubDate/text()",Item) of
    	[#xmlText{value=DateValue}] -> util:date_format(4, DateValue);
    	_ -> Now
    end,
    #todayhot_news{
		class = Class,
		node_id = SourceId, title = ?c2b(Title), url=?c2b(TUrl), news_time=NewTime, source=Source, count=Count
		, abstract = <<"">>, time=Now, img = <<"">>
	}.

do_start_spider_rss(Cfg=#cfg_news_source{class=Class, source_id=SourceId, url = Url, is_top = IsTop}, TodayData, Now) ->
	case ibrowse:send_req(?b2l(Url), [{"user-agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36"}], get) of
		{ok, "200", _ResponseHeaders, Body} ->
			{XmlDoc, _B} = xmerl_scan:string(Body),
			Items = xmerl_xpath:string("/rss/channel/item",XmlDoc),  
			{NNews, NewHotList, NTodayData} =  lists:foldl(fun(Item, {AccNews, AccTop, AccToday})->  
                                [#xmlText{value=CTitle}] = xmerl_xpath:string("/item/title/text()",Item), 
                                Title = ?c2b(CTitle), 
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

do_start_spider_html(Cfg=#cfg_news_source{class=Class, source_id=SourceId, url = Url, head = Head}, TodayData, Now) ->
	case ibrowse:send_req(?b2l(Url), Head++[{"user-agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36"}], get) of
		{ok, "200", _ResponseHeaders, Body} ->
			#cfg_news_source{data=Data, container=ContainerF, title=TitleF, link_a=LinkA,
			desc=DescF, author=AuthorF, img=ImgF, count=CountF, time=TimeF} = Cfg,
			{_, Container} = ?IF(Data=:=<<"">>, {ok, Body}, re:run(Body, Data, [{capture, first, binary}, global])),
			{_, Item} = ?IF(ContainerF=:=<<"">>, {ok, Container}, re:run(Container, ContainerF, [{capture, first, binary}, global])),
			{_, ItemTitleL} = re:run(Item, TitleF, [{capture, all_but_first, binary}, global]),
			{_, ItemLinkAL} = re:run(Item, LinkA, [{capture, all_but_first, binary}, global]),
			ItemDescL = get_html_data(DescF, Item),
			ItemAuthorFL = get_html_data(AuthorF, Item),
			ItemImgFL = get_html_data(ImgF, Item),
			ItemCountFL = get_html_data(CountF, Item),
			ItemTimeFL = get_html_data(TimeF, Item),
			% ?INFO("ItemLinkAL~w",[ItemLinkAL]),
			{NNews, NTodayData, NewHotList} = do_parse_html(Cfg, TodayData, [], [], Now, ItemTitleL, ItemLinkAL, ItemDescL, ItemAuthorFL, ItemImgFL, ItemCountFL, ItemTimeFL),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, Class, SourceId, NNews, Now}), ignored),
			?IF(NewHotList=/=[], api_todayhot:insert_new_hot(Class, SourceId, lists:reverse(NewHotList)), ignored),
			?INFO("SourceId~w NNews len ~w", [SourceId, length(NNews)]),
			NTodayData;
		_Err ->
			?ERR("Url ~ts fail ~w", [Url, _Err]),
			TodayData
    end.

do_start_spider_html_json(Cfg=#cfg_news_source{class=Class, source_id=SourceId, url = Url, json_data=DataF, head=Head}, TodayData, Now) ->
	NUrl = get_send_url(SourceId, Url, Now),
	case ibrowse:send_req(NUrl, Head++[{"user-agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36"}], get) of
		{ok, "200", _ResponseHeaders, Body} ->
			?IF(SourceId==110002, ?INFO("Body~w DataF~ts",[length(Body), DataF]), ignored),
			case catch re:run(Body, DataF, [{capture, first, list}, global, unicode]) of
		        {_, RetJsonStr} ->
		        	BodyJsonBin = list_to_binary(RetJsonStr),
					BodyTerm = jsx:decode(BodyJsonBin),
					{NNews, NTodayData, NewHotList} = parse_body(Cfg, BodyTerm, Now, TodayData),
					?IF(length(NNews)>0, mgr_todayhot:send({up_news, Class, SourceId, NNews, Now}), ignored),
					?IF(NewHotList=/=[], api_todayhot:insert_new_hot(Class, SourceId, lists:reverse(NewHotList)), ignored),
					?INFO("SourceId~w NNews len ~w", [SourceId, length(NNews)]),
					NTodayData;
		        _Err ->
		        	?ERR("fail ~w", [ _Err]),
		            TodayData
		    end;
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
	NLinkA = build_link_a(SourceId,LinkA),
	TUrl = case catch re:run(NLinkA, "http", [{capture, first, list}, global, unicode]) of
		        {match, _} ->
		        	NLinkA;
		        _Err ->
		        	util:fbin(<<"~s~s"/utf8>>, [LinkPre, build_link_a(SourceId,LinkA)])
		    end,
	% Url = ?l2b(?Host ++ "/" ++ Param),
	% ?IF(SourceId==10006, ?INFO("Title~ts",[Title]), ignored),
	Count = parse_count(SourceId, CountI),
	% ?INFO("Title~ts",[Title]),
	TNews = #todayhot_news{
		class = Class,
		node_id = SourceId, title = Title, url=TUrl, news_time=NewTime, source=Source, count=Count
		, abstract = unicode:characters_to_binary(lists:sublist(unicode:characters_to_list(Abstract),50)), time=Now, img = Img
	},
	{TNews, RtItemDescL, RtItemAuthorFL, RtItemImgFL, RtItemCountFL, RtItemTimeL}.

re_title(CTitle) ->
	case unicode:characters_to_list(CTitle) of
		UCTitle when is_list(UCTitle) -> 
			Fun = fun(C, Acc) ->
				case lists:member(C, [10,13,32]) of
					true -> Acc;
					_ -> Acc ++ [C]
				end
			end,
			TL = lists:foldl(Fun, [], UCTitle),
			unicode:characters_to_binary(TL);
		_ ->
			?l2b(iconv:convert("gbk", "utf-8", ?b2l(CTitle)))
	end.
	% ?INFO("CTitle~ts UCTitle~w",[CTitle, UCTitle]),
	

do_parse_html(_Cfg, TodayData, News, TopL, _Now, [], _, _, _, _, _, _) ->
	{News, TodayData, TopL};
do_parse_html(Cfg=#cfg_news_source{is_top = IsTop}, TodayData, News, TopL, Now, [[OTitle]|ItemTitleL], [[LinkA]|ItemLinkAL], ItemDescL, ItemAuthorFL, ItemImgFL, ItemCountFL, ItemTimeFL) ->
	Title = re_title(OTitle),
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

get_send_url(UrlType, Url, Now) ->
	case UrlType of
		1 ->
			?b2l(Url)++?i2l(Now)++".json";
		2  ->
			?b2l(Url) ++ util:timestamp_to_datetime1(Now);
		_ ->
			?b2l(Url)
	end.

do_start_spider_json(Cfg=#cfg_news_source{class=Class, source_id=SourceId, url = Url,head=Head, url_type = UrlType}, TodayData, Now) ->
	NUrl = get_send_url(UrlType, Url, Now),
    case ibrowse:send_req(NUrl, Head++[{"user-agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36"}], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			BodyJsonBin = list_to_binary(ResponseBody),
			BodyTerm = jsx:decode(BodyJsonBin),
			{NNews, NTodayData, NewHotList} = parse_body(Cfg, BodyTerm, Now, TodayData),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, Class, SourceId, NNews, Now}), ignored),
			?IF(NewHotList=/=[], api_todayhot:insert_new_hot(Class, SourceId, lists:reverse(NewHotList)), ignored),
			?INFO("SourceId~w NNews len ~w", [SourceId, length(NNews)]),
			NTodayData;
		_Err ->
			?ERR("Url ~ts fail ~w", [NUrl, _Err]),
			TodayData
	end.

get_data(<<"">>, _Body, Default) -> Default;
get_data(Data, Body, Default) ->
	DataStr = ?b2l(Data),
	DataL = string:tokens(DataStr, "|"),
	get_data_f1(DataL, Body, Default).

get_data_f1([], Datas, _) -> Datas;
get_data_f1([Data|L], Body, Default) ->
	case proplists:get_value(?l2b(Data), Body) of
		DataS when DataS =/= undefined ->
			NDataS = case DataS of
				[DataL] when is_list(DataL) -> DataL;
				[_|_] -> DataS;
				_ ->
					case catch jsx:decode(DataS) of
						JDataS when is_list(JDataS) ->
							JDataS;
						_ ->
							DataS
					end
			end,
			get_data_f1(L, NDataS, Default);
		_ ->
			Default
	end.

parse_body(Cfg, Body, Now, TodayData) ->
	#cfg_news_source{data=Data} = Cfg,
	case get_data(Data, Body, Body) of
		NewData when is_list(NewData) ->
			do_parse_data(Cfg, NewData, [], Now, TodayData, []);
		_ ->
			{[], TodayData, []}
	end.
	
% parse_count(60001, Count) ->
% 	LC = ?b2l(Count),
% 	[SCount|_] = string:tokens(LC, " "),
% 	?l2b(?i2l(?l2i(SCount)*10000));
parse_count(_, Count) ->
	case is_integer(Count) of
		true ->
			?l2b(?i2l(Count));
		_ when is_list(Count) ->
			?l2b(Count);
		_ ->
			Count
	end.

build_link_a(_SourceId, LinkA) when is_integer(LinkA) ->
	?l2b(?i2l(LinkA));
% build_link_a(SourceId, LinkA)  when SourceId=:=10003 orelse SourceId =:= 110001->
% 	util:fbin(<<"~s~s"/utf8>>, [LinkA, <<"%2523">>]);
build_link_a(_SourceId, LinkA) ->
	LinkA.

get_item_data(OldData, Container) when Container =/= <<"">> ->
	get_data(Container, OldData, []);
get_item_data(OldData, _) -> OldData.

new_add_json(Cfg, Data) ->
	#cfg_news_source{class = Class, source_id=SourceId, link_pre=LinkPre, title=TitleF, link_a=LinkA, desc=DescF, author=AuthorF, 
		img = ImgF, count = CountF, time = Time, time_type=TimeType} = Cfg,
	TimeData = proplists:get_value(Time, Data),
	Now = util:now(),
	NewTime = ?IF(TimeData=:=undefined, Now, util:date_format(TimeType, TimeData)),
	Title = get_data(TitleF, Data, <<"">>),
	% ?INFO("Title~ts",[Title]),
	TUrl = util:fbin(<<"~s~s"/utf8>>, [LinkPre, build_link_a(SourceId,get_data(LinkA, Data, <<"">>))]),
	% Url = ?l2b(?Host ++ "/" ++ Param),
	Abstract = get_data(DescF, Data, <<"">>),
	Source = get_data(AuthorF, Data, <<"">>),
	Img = get_data(ImgF, Data, <<"">>),
	case catch parse_count(SourceId, get_data(CountF, Data, <<"0">>)) of
		Count when is_binary(Count) ->
			#todayhot_news{
				class = Class,
				node_id = SourceId, title = Title, url=TUrl, news_time=NewTime, source=Source, count=Count
				, abstract = unicode:characters_to_binary(lists:sublist(unicode:characters_to_list(Abstract),50)), time=Now, img = Img
			};
		_ ->
			false
	end.

do_parse_data(_, [], News, _, NTodayData, TopL) ->
	{News, NTodayData, TopL};
do_parse_data(Cfg, [OldData|T], News, Now, TodayData, TopL) ->
	#cfg_news_source{title=TitleF, is_top=IsTop, container = Container} = Cfg,
	Data = get_item_data(OldData, Container), 
	Title = get_data(TitleF, Data, <<"">>),
	TNews = new_add_json(Cfg, Data),
	case TNews=:=false orelse Title =:= <<"">>  of
		true ->
			do_parse_data(Cfg, T, News, Now, TodayData, TopL);
		_ ->
			case IsTop =:= ?true of
				true ->
					case lists:member(Title, TodayData) of
						false -> %% 
							do_parse_data(Cfg, T, [TNews|News], Now, [Title|TodayData], [TNews|TopL]);
						_ ->
							do_parse_data(Cfg, T, News, Now, TodayData, [TNews|TopL])
					end;
				_ ->
					case lists:member(Title, TodayData) of
						false -> %% 
							do_parse_data(Cfg, T, [TNews|News], Now, [Title|TodayData], TopL);
						_ ->
							do_parse_data(Cfg, T, News, Now, TodayData, TopL)
					end
			end
	end.

check(Cfg=#cfg_news_source{type = 1}) ->
	test_spider_json(Cfg, util:now());
check(Cfg=#cfg_news_source{type = 2}) ->
	test_spider_html(Cfg, util:now());
check(Cfg=#cfg_news_source{type = 3}) ->
	test_spider_rss(Cfg, util:now());
check(Cfg=#cfg_news_source{type = 4}) ->
	test_spider_html_json(Cfg, util:now()).
	
test_spider_json(Cfg=#cfg_news_source{source_id=SourceId, url = Url,head=Head}, Now) ->
	NUrl = get_send_url(SourceId, Url, Now),
	% ?INFO("xxxxxxxxxxxxxxxNUrl ~w",[NUrl]),
    case ibrowse:send_req(NUrl, Head++[{"user-agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36"}], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			BodyJsonBin = list_to_binary(ResponseBody),
			BodyTerm = jsx:decode(BodyJsonBin),
			{NNews, _NTodayData, _NewHotList} = parse_body(Cfg, BodyTerm, Now, []),
			NData = [[{title, Title},{url, CUrl}] || #todayhot_news{title = Title, url = CUrl} <- NNews],
			% ?INFO("SourceId~w NNews len ~w", [SourceId, length(NNews)]),
			NData;
		_Err ->
			?ERR("Url ~ts fail ~w", [NUrl, _Err]),
			[]
	end.

test_spider_rss(Cfg=#cfg_news_source{url = Url, is_top = IsTop}, Now) ->
		case ibrowse:send_req(?b2l(Url), [{"user-agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36"}], get) of
			{ok, "200", _ResponseHeaders, Body} ->
				{XmlDoc, _B} = xmerl_scan:string(Body),
				Items = xmerl_xpath:string("/rss/channel/item",XmlDoc),  
				{NNews, _NewHotList, _NTodayData} =  lists:foldl(fun(Item, {AccNews, AccTop, AccToday})->  
									[#xmlText{value=CTitle}] = xmerl_xpath:string("/item/title/text()",Item), 
									Title = ?c2b(CTitle), 
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
							 end,  {[], [], []}, Items),  
				NData = [[{title, NTitle},{url, NUrl}] || #todayhot_news{title = NTitle, url = NUrl} <- NNews],
				% ?INFO("SourceId~w NNews len ~w", [SourceId, length(NNews)]),
				NData;
			_Err ->
				?ERR("Url ~ts fail ~w", [Url, _Err]),
				[]
		end.
	
test_spider_html(Cfg=#cfg_news_source{url = Url, head = Head}, Now) ->
		case ibrowse:send_req(?b2l(Url), Head++[{"user-agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36"}], get) of
			{ok, "200", _ResponseHeaders, Body} ->
				#cfg_news_source{data=Data, container=ContainerF, title=TitleF, link_a=LinkA,
				desc=DescF, author=AuthorF, img=ImgF, count=CountF, time=TimeF} = Cfg,
				{_, Container} = ?IF(Data=:=<<"">>, {ok, Body}, re:run(Body, Data, [{capture, first, binary}, global])),
				{_, Item} = ?IF(ContainerF=:=<<"">>, {ok, Container}, re:run(Container, ContainerF, [{capture, first, binary}, global])),
				{_, ItemTitleL} = re:run(Item, TitleF, [{capture, all_but_first, binary}, global]),
				{_, ItemLinkAL} = re:run(Item, LinkA, [{capture, all_but_first, binary}, global]),
				ItemDescL = get_html_data(DescF, Item),
				ItemAuthorFL = get_html_data(AuthorF, Item),
				ItemImgFL = get_html_data(ImgF, Item),
				ItemCountFL = get_html_data(CountF, Item),
				ItemTimeFL = get_html_data(TimeF, Item),
				% ?INFO("ItemLinkAL~w",[ItemLinkAL]),
				{NNews, _NTodayData, _NewHotList} = do_parse_html(Cfg, [], [], [], Now, ItemTitleL, ItemLinkAL, ItemDescL, ItemAuthorFL, ItemImgFL, ItemCountFL, ItemTimeFL),
				% ?INFO("SourceId~w NNews len ~w", [SourceId, length(NNews)]),
				NData = [[{title, NTitle},{url, NUrl}] || #todayhot_news{title = NTitle, url = NUrl} <- NNews],
				% ?INFO("SourceId~w NNews len ~w", [SourceId, length(NNews)]),
				NData;
			_Err ->
				?ERR("Url ~ts fail ~w", [Url, _Err]),
				[]
		end.
	
test_spider_html_json(Cfg=#cfg_news_source{source_id=SourceId, url = Url, json_data=DataF, head=Head}, Now) ->
		NUrl = get_send_url(SourceId, Url, Now),
		case ibrowse:send_req(NUrl, Head++[{"user-agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36"}], get) of
			{ok, "200", _ResponseHeaders, Body} ->
				?IF(SourceId==110002, ?INFO("Body~w DataF~ts",[length(Body), DataF]), ignored),
				case catch re:run(Body, DataF, [{capture, first, list}, global, unicode]) of
					{_, RetJsonStr} ->
						BodyJsonBin = list_to_binary(RetJsonStr),
						BodyTerm = jsx:decode(BodyJsonBin),
						{NNews, _NTodayData, _NewHotList} = parse_body(Cfg, BodyTerm, Now, []),
						% ?IF(length(NNews)>0, mgr_todayhot:send({up_news, Class, SourceId, NNews, Now}), ignored),
						% ?IF(NewHotList=/=[], api_todayhot:insert_new_hot(Class, SourceId, lists:reverse(NewHotList)), ignored),
						% ?INFO("SourceId~w NNews len ~w", [SourceId, length(NNews)]),
						NData = [[{title, NTitle},{url, CUrl}] || #todayhot_news{title = NTitle, url = CUrl} <- NNews],
						% ?INFO("SourceId~w NNews len ~w", [SourceId, length(NNews)]),
						NData;
					_Err ->
						?ERR("fail ~w", [ _Err]),
						[]
				end;
			_Err ->
				?ERR("Url ~ts fail ~w", [Url, _Err]),
				[]
		end.
	
	