%% 分类配置接口
-module(todayhot_cfg_class_handler).
 
-export([init/2]).

 -include("common.hrl").
 -include("todayhot.hrl").
 -include("cfg_news_class.hrl").

init(Req0, State) ->
	Method = cowboy_req:method(Req0),
	% ?INFO(" XXXXXXXXXXXXXMethod ~ts 	",[Method]),
	Req1 = case Method of
		<<"GET">> ->
			Param = cowboy_req:parse_qs(Req0),
			handle(Param, Req0);
		<<"OPTIONS">> ->
			cowboy_req:reply(200, Req0);
		_ ->
			{ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
			handle_post(PostVals, Req)
	end,
	{ok, Req1, State}.

handle_post(PostVals, Req) ->
	ClassId = ?l2i(?b2l(proplists:get_value(<<"class_id">>, PostVals, <<"0">>))),
	Name = proplists:get_value(<<"class_name">>, PostVals),
	case ClassId > 0 andalso Name =/= undefined of
		true ->
			mgr_todayhot:send({update_cfg_class,  #cfg_news_class{id = ClassId, name = Name}}),
			% ets:insert(?ETS_CFG_CLASS, #cfg_news_class{id = ClassId, name = Name}),
			Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
		    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
		    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
			cowboy_req:reply(200, #{
				<<"content-type">> => <<"application/json; charset=utf-8">>
			}, [], Req3);
		_ -> cowboy_req:reply(405, Req)
	end.


handle(Param, Req) ->
	% Status = ?l2i(?b2l(proplists:get_value(<<"status">>, Param, <<"0">>))),
	DelId = ?l2i(?b2l(proplists:get_value(<<"del">>, Param, <<"0">>))),
	Reply = case DelId > 0 of
		true ->
			mgr_todayhot:send({del_cfg_class, DelId}),
			[];
		_ ->
			Fun = fun(#cfg_news_class{id = ClassId, name = ClassName}, Acc) ->
					[[{class_id, ClassId},{class_name, ClassName}]|Acc]
				end,
			Datas = lists:foldl(Fun, [], ets:tab2list(?ETS_CFG_CLASS)),
			jsx:encode([{data, Datas}])
	end,
	?INFO("xxxxxxxxxxxxReplt~w",[Reply]),
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json; charset=utf-8">>
	}, Reply, Req3).

