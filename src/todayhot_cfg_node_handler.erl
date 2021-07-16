%% 节点配置接口
-module(todayhot_cfg_node_handler).
 
-export([init/2]).

 -include("common.hrl").
 -include("todayhot.hrl").
 -include("cfg_news_source.hrl").

init(Req0, State) ->
	Method = cowboy_req:method(Req0),
	?INFO(" XXXXXXXXXXXXXMethod ~ts 	",[Method]),
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
	try 
		IsCheck = ?l2i(?b2l(proplists:get_value(<<"is_check">>, PostVals, <<"0">>))),
		SourceId = ?l2i(?b2l(proplists:get_value(<<"source_id">>, PostVals, <<"0">>))),
		Class = ?l2i(?b2l(proplists:get_value(<<"class_id">>, PostVals, <<"0">>))),
		SubClass = proplists:get_value(<<"sub_class">>, PostVals),
		Name = proplists:get_value(<<"source_name">>, PostVals),
		Summry = proplists:get_value(<<"summry">>, PostVals),
		Url = proplists:get_value(<<"url">>, PostVals),
		UrlType = ?l2i(?b2l(proplists:get_value(<<"url_type">>, PostVals, <<"0">>))),
		Type = ?l2i(?b2l(proplists:get_value(<<"type">>, PostVals, <<"0">>))),
		IsTop = ?l2i(?b2l(proplists:get_value(<<"is_top">>, PostVals, <<"0">>))),
		LinkPre = proplists:get_value(<<"link_pre">>, PostVals),
		Data = proplists:get_value(<<"data">>, PostVals),
		Container = proplists:get_value(<<"container">>, PostVals),
		Title = proplists:get_value(<<"title">>, PostVals),
		LinkA = proplists:get_value(<<"link_a">>, PostVals),
		Desc = proplists:get_value(<<"desc">>, PostVals),
		Author = proplists:get_value(<<"author">>, PostVals),
		Img = proplists:get_value(<<"img">>, PostVals),
		Count = proplists:get_value(<<"count">>, PostVals),
		Time = proplists:get_value(<<"time">>, PostVals),
		TimeType =  ?l2i(?b2l(proplists:get_value(<<"time_type">>, PostVals, <<"0">>))),
		JsonData = proplists:get_value(<<"json_data">>, PostVals),
		Head = util:bitstring_to_term(proplists:get_value(<<"head">>, PostVals, <<"[]">>)),
		?INFO("xxxxxxxxxxxxxxxxxxxx_Name~w Class~w is_check~w SourceId~w",[Name, Class, IsCheck, SourceId]),
		case SourceId > 0 of
			true ->
				Cfg = #cfg_news_source{source_id = SourceId, class = Class, sub_class = SubClass, type = Type, name = Name, summry = Summry, url = Url,
				is_top = IsTop, link_pre = LinkPre, data = Data, container = Container, title = Title, link_a = LinkA, url_type = UrlType,
				desc = Desc, author =Author, img = Img, count = Count, time = Time, time_type=TimeType, json_data = JsonData, head = Head},
				Reply = case IsCheck of
					?false ->
						case source:check(Cfg) of
							[] ->
								jsx:encode([{code, 1}]);
							NewsData -> 
								mgr_todayhot:send({update_cfg_node,  Cfg}),
								jsx:encode([{code, 0},{data, NewsData}])
						end;
					_ ->
						case source:check(Cfg) of
							[] ->
								jsx:encode([{code, 1}]);
							NewsData -> 
								jsx:encode([{code, 0},{data, NewsData}])
						 end
				end,
				% ets:insert(?ETS_CFG_CLASS, #cfg_news_class{id = ClassId, name = Name}),
				Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
				Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
				Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
				cowboy_req:reply(200, #{
					<<"content-type">> => <<"application/json; charset=utf-8">>
				}, Reply, Req3);
			_ -> cowboy_req:reply(405, Req)
		end
	catch
		_ErrType:_Reason:Strac ->
			?ERR("xxxxxxxxxxxxxxxxxxxx_ErrType ~w  _Reason~w  Strac~w ",[_ErrType, _Reason, Strac]),
			Req11 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
			Req22 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req11),
			Req33 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req22),
			cowboy_req:reply(200, #{
					<<"content-type">> => <<"application/json; charset=utf-8">>
				}, jsx:encode([{code, 1}]), Req33)
	end.


handle(Param, Req) ->
	% Status = ?l2i(?b2l(proplists:get_value(<<"status">>, Param, <<"0">>))),
	DelId = ?l2i(?b2l(proplists:get_value(<<"del">>, Param, <<"0">>))),
	Reply = case DelId > 0 of
		true ->
			mgr_todayhot:send({del_cfg_class, DelId}),
			[];
		_ ->
			Datas = build_data(ets:first(?ETS_CFG_NODE), []),
			jsx:encode([{data, Datas}])
	end,
	% ?INFO("xxxxxxxxxxxxReplt~w",[Reply]),
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json; charset=utf-8">>
	}, Reply, Req3).


build_data('$end_of_table', List) -> List;
build_data(SourceId , List) ->
	NList = case api_todayhot:get_node(SourceId) of
		#cfg_news_source{source_id = SourceId, class = Class, sub_class = SubClass, type = Type, name = Name, summry = Summry, url = Url,
			is_top = IsTop, link_pre = LinkPre, data = Data, container = Container, title = Title, link_a = LinkA, url_type = UrlType,
			desc = Desc, author =Author, img = Img, count = Count, time = Time, time_type=TimeType, json_data = JsonData, head = Head} ->
				[[
					{source_id, SourceId}
					,{class_id, Class}
					,{sub_class , SubClass}
					,{type , Type}
					,{source_name , Name}
					,{summry, Summry}
					,{url , Url}
					,{url_type , UrlType}
					,{is_top , IsTop}
					,{link_pre , LinkPre}
					,{data , Data}
					,{container, Container}
					,{title , Title}
					,{link_a, LinkA}
					,{desc, Desc}
					,{author, Author}
					,{img, Img}
					,{count, Count}
					,{time, Time}
					,{time_type, TimeType}
					,{json_data, JsonData}
					,{head, util:term_to_bitstring(Head)}
					]|List];
		_ -> List
	end,
	build_data(ets:next(?ETS_CFG_NODE, SourceId), NList).