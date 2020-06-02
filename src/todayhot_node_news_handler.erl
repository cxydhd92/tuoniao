-module(todayhot_node_news_handler).
 
-export([init/2]).

-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_source.hrl").

init(Req0, State) ->
	Method = cowboy_req:method(Req0),
	% ?INFO(" Method~w	",[Method]),
	Req1 = case Method of
		<<"GET">> ->
			Param = cowboy_req:parse_qs(Req0),
			handle(Param, Req0);
		_ ->
			% ?INFO("Method~w",[Method]),
			cowboy_req:reply(405, Req0)
	end,
	{ok, Req1, State}.

handle(Param, Req) ->
	% ?INFO("Param~w",[Param]),
	case catch do_handle(Param, Req) of
		{ok,  Reply} -> Reply;
		_Err ->
			?ERR("_Err ~w",[_Err]),
			cowboy_req:reply(405, Req)
	end.

get_page_news(MinId, _ClassId, NodeId, PageSize, TimeZero, Today) ->
	{EtsName, HotNewsL} = case Today=:=TimeZero of
		true ->
			Node = ets:lookup(?ETS_TODAYHOT_HOTLIST, {NodeId, Today}),
			NNewsL = case Node of
				[#todayhot_node_news{news= NewsL}] ->	NewsL;
				_ -> []
			end,
			{?ETS_TODAYHOT_TODAY, NNewsL};
		_ ->
			{?ETS_TODAYHOT_NEWS, []}
	end,
	{TodayNewsL, IsDone, NTimeZero} = do_get_page_news(MinId, NodeId, PageSize, TimeZero, EtsName, HotNewsL, [], ?false),
	{lists:keysort(#todayhot_news.id, TodayNewsL), IsDone, NTimeZero} .

do_get_page_news(_MinId, _NodeId, _PageSize, TimeZero, _EtsName, _HotNewsL, NewsL, Loop) when Loop > 7 ->
	{NewsL, true, TimeZero};
do_get_page_news(MinId, NodeId, PageSize, TimeZero, EtsName, HotNewsL, CNewsL, Loop) ->	
	NNewsL = case ets:lookup(EtsName, {NodeId, TimeZero}) of
		[#todayhot_node_news{news = NewsL}] -> NewsL;
		_ -> []
	end,
	SortNewsL = lists:reverse(lists:keysort(#todayhot_news.id, NNewsL)),
	{PageNewsL, IsDone} = get_class_news(SortNewsL, MinId, PageSize, [], HotNewsL),
	TNewsL = PageNewsL++CNewsL,
	Len  = length(TNewsL),
	case Len >= 20 of
		true ->
			{TNewsL, IsDone, TimeZero};
		_ ->
			do_get_page_news(MinId, NodeId, PageSize - Len, TimeZero-86400, EtsName, HotNewsL, TNewsL, Loop+1)
	end.

get_next_time(TimeZero, _NodeId, ?false) -> TimeZero - 86400;
get_next_time(TimeZero, NodeId, Num) when Num > 0->
	case ets:lookup(?ETS_TODAYHOT_NEWS, {NodeId, TimeZero}) of
		[_|_] -> TimeZero;
		_ -> get_next_time(TimeZero-86400, NodeId, Num-1)
	end.

get_class_news([], _, _, NewsL, _) -> {NewsL, true};
get_class_news(_, _, 0, NewsL, _) -> {NewsL, false};
get_class_news([TN=#todayhot_news{id = Id}|SortNewsL], MinId, PageSize, NewsL, HotNewsL) when MinId=:=0 orelse Id < MinId ->
	case lists:keymember(Id, #todayhot_news.id, HotNewsL) of
		true ->
			get_class_news(SortNewsL, MinId, PageSize, NewsL, HotNewsL);
		_ ->
			get_class_news(SortNewsL, MinId, PageSize-1, [TN|NewsL], HotNewsL)
	end;
get_class_news([_|SortNewsL], MinId, PageSize, NewsL, HotNewsL) ->
	get_class_news(SortNewsL, MinId, PageSize, NewsL, HotNewsL).

do_handle([], Req) ->
	{ok, cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req)};
do_handle(PostVals, Req) ->
	ClassId = ?l2i(?b2l(proplists:get_value(<<"class_id">>, PostVals, <<"1">>))),
	NodeId = ?l2i(?b2l(proplists:get_value(<<"node_id">>, PostVals, <<"0">>))),
	MinId = ?l2i(?b2l(proplists:get_value(<<"min_id">>, PostVals, <<"0">>))),
	PageSize = ?l2i(?b2l(proplists:get_value(<<"page_size">>, PostVals, <<"50">>))),
	TimeZero = ?l2i(?b2l(proplists:get_value(<<"time">>, PostVals, <<"0">>))),

	% ?INFO("ClassId ~w MinId ~w PageSize ~w",[ClassId, MinId, PageSize]),
	case is_integer(NodeId) andalso is_integer(ClassId) andalso is_integer(MinId) andalso is_integer(PageSize) andalso is_integer(TimeZero) of
		true when ClassId > 0 andalso PageSize > 0 andalso NodeId > 0 ->
			Today = util:today(),
			TimeZero1 = ?IF(TimeZero =:= 0, Today, TimeZero), 
			{PageNewsL, IsDone, NTimeZero} = get_page_news(MinId, ClassId, NodeId, PageSize, TimeZero1, Today),
			#cfg_news_source{name=NodeName} = cfg_news_source:get(NodeId),
			{NNewsL, NNId} = case PageNewsL of
				[#todayhot_news{id=NId}|_] ->
					Fun  = fun(#todayhot_news{id=Id, abstract=Abs, img=Img, time=Time, title=Title,url=Url, source=Source}, Acc) ->
						[[{id, Id},{abstract, Abs}, {title, Title}, {url,Url}, {source, Source}, {img, Img}, {time, Time}]|Acc]
					end,
					{lists:foldl(Fun, [], PageNewsL), NId};
				_ ->
					{[], 0}
			end,
			% Data = [{data, NNewsL}],
			% ?INFO("NNewsL~w", [length(NNewsL)]),
			NextTime = ?IF(IsDone, get_next_time(NTimeZero-86400, NodeId, 30), NTimeZero), 
			Reply = jsx:encode([{data, NNewsL}, {next_time, NextTime}, {next_id, NNId},{name, NodeName}]),
			% ?INFO("Reply ~w",[Reply]),
			Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
		    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
		    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
			{ok, cowboy_req:reply(200, #{
				<<"content-type">> => <<"application/json; charset=utf-8">>
			}, Reply, Req3)};
		_ ->
			{ok, cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req)}
	end.
	