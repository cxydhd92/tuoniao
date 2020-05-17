%% 指定类型下所有源节点信息
-module(todayhot_nodeid_handler).

-export([init/2]).

-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_source.hrl").

init(Req0, State) ->
	Method = cowboy_req:method(Req0),
	% ?INFO(" Req0~w	",[Req0]),
	Req1 = case Method of
		<<"GET">> ->
			Param = cowboy_req:match_qs([{class_id, [], undefined}], Req0),
			handle_get(Param, Req0);
		<<"POST">> ->
			cowboy_req:reply(405, Req0)
	end,
	{ok, Req1, State}.

handle_get(#{class_id := undefined}, Req) ->
	cowboy_req:reply(400, #{}, <<"Missing class_id parameter.">>, Req);
handle_get(#{class_id := ClassId}, Req) ->
	IClassId = ?l2i(?b2l(ClassId)),
	NReply = case cfg_news_source:news_source_class(IClassId) of
		NodeIds = [_|_] ->
			Fun = fun(NodeId, Acc) ->
				#cfg_news_source{source_id=SourceId, name = Name, icon_name=IconName, is_top = IsTop} = cfg_news_source:get(NodeId),
				[[{node_id, SourceId},{name, Name}, {icon_name, IconName}, {is_top, IsTop}]|Acc]
			end,
			Datas = lists:foldl(Fun, [], NodeIds),
			Reply = jsx:encode([{data,Datas}]),
			Reply;
		_ ->
			[]
	end,
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
	{ok, cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json; charset=utf-8">>
	}, NReply, Req3)};
handle_get(_, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).
