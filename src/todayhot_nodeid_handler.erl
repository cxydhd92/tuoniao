%% 指定类型下所有源节点信息
-module(todayhot_nodeid_handler).

-export([init/2]).

-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_source.hrl").
-include("cfg_news_class.hrl").
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

handle_get(_, Req) ->
	Fun = fun(IClassId, TAcc) ->
		#cfg_news_class{name = ClassName} = cfg_news_class:get(IClassId),
		case cfg_news_source:news_source_class(IClassId) of
			NodeIds = [_|_] ->
				Fun1 = fun(NodeId, Acc) ->
					#cfg_news_source{source_id=SourceId, name = Name, icon_name=IconName} = cfg_news_source:get(NodeId),
					[[{node_id, SourceId},{name, Name}, {icon_name, IconName}, {desc, <<"给你最好看的"/utf8>>}]|Acc]
				end,
				Datas = lists:foldl(Fun1, [], NodeIds),
				[[{class_id, IClassId}, {class_name, ClassName}, {nodes, Datas}]|TAcc];
			_ ->
				TAcc
		end
	end,
	ClassL = lists:foldl(Fun, [], cfg_news_class:list_key()),
	Reply = jsx:encode([{data, ClassL}]),
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
	{ok, cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json; charset=utf-8">>
	}, Reply, Req3)}.
