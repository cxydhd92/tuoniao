-module(todayhot_news_handler).
 
-export([init/2]).

-include("common.hrl").
-include("todayhot.hrl").
init(Req0, State) ->
	Method = cowboy_req:method(Req0),
	?INFO(" Method~w	",[Method]),
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
	?INFO("Param~w",[Param]),
	case catch do_handle(Param, Req) of
		{ok,  Reply} -> Reply;
		_Err ->
			?ERR("_Err ~w",[_Err]),
			cowboy_req:reply(405, Req)
	end.

get_page_news(MinId, ClassId, PageSize) ->
	TodayNewsL = get_page_news(MinId, ClassId, PageSize, ?ETS_TODAYHOT_TODAY),
	?INFO("TodayNewsL~w",[TodayNewsL]),
	case length(TodayNewsL)>=PageSize of
		true -> 
			lists:keysort(#todayhot_news.id, TodayNewsL);
		_ ->
			SurSize = PageSize - length(TodayNewsL),
			SurNewsL = get_page_news(MinId, ClassId, SurSize, ?ETS_TODAYHOT),
			lists:keysort(#todayhot_news.id, TodayNewsL++SurNewsL)
	end.



get_page_news(MinId, ClassId, PageSize, EtsName) ->	
	Nodes = api_todayhot:get_class_node(ClassId),
	?INFO("Nodes~w",[Nodes]),
	Fun = fun(NodeId, Acc) ->
		Node = ets:lookup(EtsName, {ClassId, NodeId}),
		case Node of
			[#todayhot_nodes{news= NewsL}] ->	Acc ++ NewsL;
			_ -> Acc
		end
	end,
	NNewsL = lists:foldl(Fun, [], Nodes),
	SortNewsL = lists:reverse(lists:keysort(#todayhot_news.id, NNewsL)),
	PageNewsL = get_class_news(SortNewsL, MinId, PageSize, []),
	PageNewsL.

get_class_news([], _, _, NewsL) -> NewsL;
get_class_news(_, _, 0, NewsL) -> NewsL;
get_class_news([TN=#todayhot_news{id = Id}|SortNewsL], MinId, PageSize, NewsL) when MinId=:=0 orelse Id < MinId ->
	get_class_news(SortNewsL, MinId, PageSize-1, [TN|NewsL]);
get_class_news([_|SortNewsL], MinId, PageSize, NewsL) ->
	get_class_news(SortNewsL, MinId, PageSize, NewsL).

do_handle([], Req) ->
	{ok, cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req)};
do_handle(PostVals, Req) ->
	ClassId = ?l2i(?b2l(proplists:get_value(<<"classId">>, PostVals, <<"1">>))),
	MinId = ?l2i(?b2l(proplists:get_value(<<"minId">>, PostVals, <<"0">>))),
	PageSize = ?l2i(?b2l(proplists:get_value(<<"pageSize">>, PostVals, <<"10">>))),
	?INFO("ClassId ~w MinId ~w PageSize ~w",[ClassId, MinId, PageSize]),
	case is_integer(ClassId) andalso is_integer(MinId) andalso is_integer(PageSize) of
		true when ClassId > 0 andalso PageSize > 0 ->
			PageNewsL = get_page_news(MinId, ClassId, PageSize),
			Fun  = fun(#todayhot_news{id=Id, node_id = NodeId, abstract=Abs, img=Img, time=Time, sub_news=[#todayhot_sub_news{title=Title,url=Url, source=Source}|_]}, Acc) ->
				{NodeName, _} = api_todayhot:get_node_name(NodeId),
				[[{id, Id},{node_name, NodeName},{abstract, Abs}, {title, Title}, {url,Url}, {source, Source}, {img, Img}, {time, Time}]|Acc]
			end,
			NNewsL = lists:foldl(Fun, [], PageNewsL),
			% Data = [{data, NNewsL}],
			?INFO("NNewsL~w", [NNewsL]),
			Reply = jsx:encode(NNewsL),
			?INFO("Reply ~w",[Reply]),
			Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
		    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
		    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
			{ok, cowboy_req:reply(200, #{
				<<"content-type">> => <<"application/json; charset=utf-8">>
			}, Reply, Req3)};
		_ ->
			{ok, cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req)}
	end.
	