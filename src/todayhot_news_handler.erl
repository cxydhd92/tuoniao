-module(todayhot_news_handler).
 
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
			?INFO("Method~w",[Method]),
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

get_page_news(MinId, ClassId, PageSize, TimeZero, Today) ->
	EtsName = ?IF(Today=:=TimeZero, ?ETS_TODAYHOT_TODAY, ?ETS_TODAYHOT_NEWS),
	{TodayNewsL, IsDone, NTimeZero} = do_get_page_news(MinId, ClassId, PageSize, TimeZero, EtsName, [], ?false),
	{lists:keysort(#todayhot_news.id, TodayNewsL), IsDone, NTimeZero} .

do_get_page_news(_MinId, _ClassId, _PageSize, TimeZero, _EtsName, NewsL, Loop) when Loop > 1 ->	
	{NewsL, true, TimeZero};
do_get_page_news(MinId, ClassId, PageSize, TimeZero, EtsName, CNewsL, Loop) ->	
	Nodes = cfg_news_source:news_source_class(ClassId),
	Fun = fun(NodeId, Acc) ->
		case ets:lookup(EtsName, {NodeId, TimeZero}) of
			[#todayhot_node_news{news= NewsL}] ->	Acc ++ NewsL;
			_ -> Acc
		end
	end,
	NNewsL = lists:foldl(Fun, [], Nodes),
	SortNewsL = lists:reverse(lists:keysort(#todayhot_news.id, NNewsL)),
	{PageNewsL, IsDone} = get_class_news(SortNewsL, MinId, PageSize, []),
	TNewsL = CNewsL ++ PageNewsL,
	Len  = length(TNewsL),
	case Len >= 20 of
		true ->
			{TNewsL, IsDone, TimeZero};
		_ ->
			do_get_page_news(MinId, ClassId, PageSize - Len, TimeZero-86400, ?ETS_TODAYHOT_NEWS, TNewsL, Loop+1)
	end.

get_class_news([], _, _, NewsL) -> {NewsL, true};
get_class_news(_, _, 0, NewsL) -> {NewsL, false};
get_class_news([TN=#todayhot_news{id = Id, same_id=0}|SortNewsL], MinId, PageSize, NewsL) when MinId=:=0 orelse Id < MinId ->
	get_class_news(SortNewsL, MinId, PageSize-1, [TN|NewsL]);
get_class_news([_|SortNewsL], MinId, PageSize, NewsL) ->
	get_class_news(SortNewsL, MinId, PageSize, NewsL).

do_handle([], Req) ->
	{ok, cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req)};
do_handle(PostVals, Req) ->
	ClassId = ?l2i(?b2l(proplists:get_value(<<"class_id">>, PostVals, <<"1">>))),
	MinId = ?l2i(?b2l(proplists:get_value(<<"min_id">>, PostVals, <<"0">>))),
	PageSize = ?l2i(?b2l(proplists:get_value(<<"page_size">>, PostVals, <<"50">>))),
	TimeZero = ?l2i(?b2l(proplists:get_value(<<"time">>, PostVals, <<"0">>))),

	% ?INFO("ClassId ~w MinId ~w PageSize ~w",[ClassId, MinId, PageSize]),
	case is_integer(ClassId) andalso is_integer(MinId) andalso is_integer(PageSize) andalso is_integer(TimeZero) of
		true when ClassId > 0 andalso PageSize > 0 ->
			Today = util:today(),
			TimeZero1 = ?IF(TimeZero =:= 0, Today, TimeZero),
			{PageNewsL, IsDone, NTimeZero} = get_page_news(MinId, ClassId, PageSize, TimeZero1, Today),
			{NNewsL, NNId} = case PageNewsL of
				[#todayhot_news{id=NId}|_] ->
					Fun  = fun(#todayhot_news{id=Id, node_id = NodeId, abstract=Abs, img=Img, time=Time, title=Title,url=Url, source=Source}, Acc) ->
						#cfg_news_source{name=NodeName} = cfg_news_source:get(NodeId),
						[[{id, Id},{node_id, NodeId}, {node_name, NodeName},{abstract, Abs}, {title, Title}, {url,Url}, {source, Source}, {img, Img}, {time, Time}]|Acc]
					end,
					{lists:foldl(Fun, [], PageNewsL), NId};
				_ ->
					{[], 0}
			end,
			% Data = [{data, NNewsL}],
			% ?INFO("NNewsL~w", [length(NNewsL)]),
			NextTime = ?IF(IsDone, NTimeZero-86400, NTimeZero), 
			Reply = jsx:encode([{data, NNewsL}, {next_time, NextTime}, {next_id, NNId}]),
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
	