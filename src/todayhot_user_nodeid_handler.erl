% 用户源节点信息
-module(todayhot_user_nodeid_handler).

-export([init/2]).

-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_source.hrl").
-include("cfg_news_class.hrl").
init(Req0, State) ->
	Method = cowboy_req:method(Req0),
	% ?INFO(" Req0~w	",[Req0]),
	NReq = case Method of
		<<"GET">> ->
			Cookies = cowboy_req:parse_cookies(Req0),
			case lists:keyfind(<<"sessionid">>, 1, Cookies) of
				{_, SessionId} ->
					handle(SessionId, Req0);
				_ ->
					Reply = jsx:encode([{code, 5}]),
					Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req0),
				    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
				    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
					cowboy_req:reply(200, #{
						<<"content-type">> => <<"application/json; charset=utf-8">>
					}, Reply, Req3)
			end;
		<<"POST">> ->
			cowboy_req:reply(405, Req0)
	end,
	{ok, NReq, State}.

handle(SessionId, Req) ->
	Now = util:now(),
	case ets:lookup(?ETS_TODAYHOT_USER_SESSION, SessionId) of
		[#todayhot_user_session{account = Account, time = EndTime}] when EndTime > Now ->
			List = api_user:get_rss_list(Account),
			case List of
				false -> 
					Reply = jsx:encode([{code, 5}]),
					Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
				    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
				    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
					cowboy_req:reply(200, #{
						<<"content-type">> => <<"application/json; charset=utf-8">>
					}, Reply, Req3);
				_ ->
					case catch do_handle(List, Req) of
						{ok,  Reply} -> Reply;
						_Err ->
							?ERR("_Err ~w",[_Err]),
							cowboy_req:reply(405, Req)
					end
			end;
		_ ->
			Reply = jsx:encode([{code, 5}]),
			Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
		    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
		    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
			cowboy_req:reply(200, #{
				<<"content-type">> => <<"application/json; charset=utf-8">>
			}, Reply, Req3)
	end.
	% ?INFO("Param~w",[Param]),
	


do_handle(Ids, Req) ->
	ClassIds = api_user:get_rss_class(Ids),
	Fun = fun({IClassId, NodeIds}, TAcc) ->
		#cfg_news_class{name = ClassName} = cfg_news_class:get(IClassId),
		Fun1 = fun(NodeId, Acc) ->
			#cfg_news_source{source_id=SourceId, name = Name, icon_name=IconName} = cfg_news_source:get(NodeId),
			[[{node_id, SourceId},{name, Name}, {icon_name, IconName}, {desc, <<"给你最好看的"/utf8>>}]|Acc]
		end,
		Datas = lists:foldl(Fun1, [], NodeIds),
		[[{class_id, IClassId}, {class_name, ClassName}, {nodes, Datas}]|TAcc]
	end,
	ClassL = lists:foldl(Fun, [], ClassIds),
	Reply = jsx:encode([{data, ClassL}, {code, 0}]),
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
	{ok, cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json; charset=utf-8">>
	}, Reply, Req3)}.
