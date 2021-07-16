-module(todayhot_node_hotlist_handler).
 
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

get_page_news(NodeId, Today) ->
	Node = ets:lookup(?ETS_TODAYHOT_HOTLIST, {NodeId, Today}),
	NNewsL = case Node of
		[#todayhot_node_news{news= NewsL}] ->	NewsL;
		_ -> []
	end,
	lists:reverse(NNewsL).

do_handle([], Req) ->
	{ok, cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req)};
do_handle(PostVals, Req) ->
	ClassId = ?l2i(?b2l(proplists:get_value(<<"class_id">>, PostVals, <<"1">>))),
	NodeId = ?l2i(?b2l(proplists:get_value(<<"node_id">>, PostVals, <<"0">>))),
	TimeZero = ?l2i(?b2l(proplists:get_value(<<"time">>, PostVals, <<"0">>))),
	
	case is_integer(ClassId) andalso is_integer(NodeId) of
		true when ClassId > 0 andalso NodeId > 0 ->
			Today = util:today(),
			NTimeZero = ?IF(TimeZero =:= 0, Today, TimeZero),
			#cfg_news_source{name = NodeName} = api_todayhot:get_node(NodeId),
			PageNewsL = get_page_news(NodeId, NTimeZero),
			Fun  = fun(#todayhot_news{id=Id, abstract=Abs, img=Img, time=Time, title=Title, url=Url, source=Source}, {Acc, Rank}) ->
				NTitle = case Rank of
					1 -> util:fbin(<<"~s~s"/utf8>>, [<<"<font class=\"fwb\" color=\"red\">1. </font>">>, Title]);
					2 -> util:fbin(<<"~s~s"/utf8>>, [<<"<font class=\"fwb\" color=\"blue\">2. </font>">>, Title]);
					3-> util:fbin(<<"~s~s"/utf8>>, [<<"<font class=\"fwb\" color=\"green\">3. </font>">>, Title]);
					_ -> util:fbin(<<"~w~s~s"/utf8>>, [Rank, <<". ">>, Title])
				end,
				{[[{id, Id},{abstract, unicode:characters_to_binary(Abs)}, {title, NTitle}, {url,Url}, {source, Source}, {img, Img}, {time, Time}]|Acc], Rank-1}
			end,
			{NNewsL, _} = lists:foldl(Fun, {[], length(PageNewsL)}, PageNewsL),
			% Data = [{data, NNewsL}],
			% ?INFO("NNewsL~w", [NNewsL]),
			Reply = jsx:encode([{data, NNewsL},{name, NodeName}]),
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
	