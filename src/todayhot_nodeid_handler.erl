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
	Ids = api_user:user_rss_ids(Req),
	Fun = fun(#cfg_news_class{id=IClassId, name = ClassName}, TAcc) ->
		case api_todayhot:news_source_class(IClassId) of
			NodeIds = [_|_] ->
				Fun1 = fun(NodeId, Acc) ->
					#cfg_news_source{source_id=SourceId, name = Name, summry=Summry} = api_todayhot:get_node(NodeId),
					IsRss = ?IF(lists:member(NodeId, Ids), ?true, ?false),
					[[{node_id, SourceId},{name, Name}, {is_rss, IsRss}, {desc, Summry}]|Acc]
				end,
				Datas = lists:foldl(Fun1, [], NodeIds),
				[[{class_id, IClassId}, {class_name, ClassName}, {nodes, Datas}]|TAcc];
			_ ->
				TAcc
		end
	end,
	ClassL = lists:foldl(Fun, [], ets:tab2list(?ETS_CFG_CLASS)),
	Reply = jsx:encode([{data, ClassL}]),
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
	{ok, cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json; charset=utf-8">>
	}, Reply, Req3)}.
