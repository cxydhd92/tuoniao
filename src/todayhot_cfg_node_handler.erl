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
			Cmd = proplists:get_value(<<"cmd">>, PostVals, <<"common">>),
			case Cmd of
				<<"weibo">> ->
					handle_weibo(PostVals, Req);
				<<"bzhan">> ->
					handle_bzhan(PostVals, Req);
				<<"gzh">> ->
					handle_gzh(PostVals, Req);
				<<"gzh_mod">> ->
					handle_mod_gzh(PostVals, Req);
				_ ->
					handle_post(PostVals, Req)
			end
	end,
	{ok, Req1, State}.

handle_weibo(PostVals, Req) ->
	UID = proplists:get_value(<<"uid">>, PostVals, <<"0">>),
	Url = util:fbin(<<"https://m.weibo.cn/api/container/getIndex?type=uid&value=~s&containerid=107603~s"/utf8>>, [UID, UID]),
	Class = 12,
	SubClass = 1,
	Name = proplists:get_value(<<"source_name">>, PostVals),
	Summry = proplists:get_value(<<"summry">>, PostVals),
	UrlType = 0,
	Type = 1,
	IsTop = 0,
	LinkPre = <<"https://m.weibo.cn/detail/">>,
	Data = <<"data|cards">>,
	Title = <<"mblog|text">>,
	LinkA = <<"mblog|mid">>,
	Time = <<"mblog|created_at">>,
	TimeType = 6,
	CheckNum = 50,
	SourceId = lists:max(api_todayhot:news_source_class(Class)) +1,
	Cfg = #cfg_news_source{source_id = SourceId, class = Class, sub_class = SubClass, type = Type, name = Name, summry = Summry, url = Url,
			is_top = IsTop, link_pre = LinkPre, data = Data, title = Title, link_a = LinkA, url_type = UrlType,
			time = Time, time_type=TimeType, check_num = CheckNum, fake_id = UID},
	do_check_update(Cfg, ?false, Req).

handle_bzhan(PostVals, Req) ->
	UID = proplists:get_value(<<"uid">>, PostVals, <<"0">>),
	Url = util:fbin(<<"https://api.bilibili.com/x/space/arc/search?mid=~s&pn=1&ps=10&index=1&jsonp=jsonp"/utf8>>, [UID]),
	Class = 13,
	SubClass = 1,
	Name = proplists:get_value(<<"source_name">>, PostVals),
	Summry = proplists:get_value(<<"summry">>, PostVals),
	UrlType = 0,
	Type = 1,
	IsTop = 0,
	LinkPre = <<"https://www.bilibili.com/video/">>,
	Data = <<"data|list|vlist">>,
	Title = <<"title">>,
	Desc = <<"description">>,
	LinkA = <<"bvid">>,
	Time = <<"created">>,
	Count = <<"play">>,
	TimeType = 1,
	CheckNum = 50,
	SourceId = lists:max(api_todayhot:news_source_class(Class)) +1,
	Cfg = #cfg_news_source{source_id = SourceId, class = Class, sub_class = SubClass, type = Type, name = Name, summry = Summry, url = Url,
			is_top = IsTop, link_pre = LinkPre, data = Data, title = Title, link_a = LinkA, url_type = UrlType, count = Count, desc = Desc,
			time = Time, time_type=TimeType, check_num = CheckNum, fake_id = UID},
	do_check_update(Cfg, ?false, Req).

handle_gzh(PostVals, Req) ->
	UID = ?l2i(?b2l(proplists:get_value(<<"uid">>, PostVals, <<"0">>))),
	Token = ?l2i(?b2l(proplists:get_value(<<"token">>, PostVals, <<"0">>))),
	Url = util:fbin(<<"https://mp.weixin.qq.com/cgi-bin/appmsg?action=list_ex&begin=0&count=5&fakeid=~s&type=9&query=&token=~s&lang=zh_CN&f=json&ajax=1"/utf8>>, [UID, Token]),
	Class = 14,
	SubClass = 1,
	Name = proplists:get_value(<<"source_name">>, PostVals),
	Summry = proplists:get_value(<<"summry">>, PostVals),
	UrlType = 0,
	Type = 1,
	IsTop = 0,
	LinkPre = <<"">>,
	Data = <<"app_msg_list">>,
	Title = <<"title">>,
	Desc = <<"digest">>,
	LinkA = <<"link">>,
	Time = <<"update_time">>,
	Count = <<"">>,
	TimeType = 1,
	CheckNum = 50,
	Head = util:bitstring_to_term(proplists:get_value(<<"head">>, PostVals, <<"[]">>)),
	SourceId = lists:max(api_todayhot:news_source_class(Class)) +1,
	Cfg = #cfg_news_source{source_id = SourceId, class = Class, sub_class = SubClass, type = Type, name = Name, summry = Summry, url = Url,
			is_top = IsTop, link_pre = LinkPre, data = Data, title = Title, link_a = LinkA, url_type = UrlType, count = Count, desc = Desc,
			time = Time, time_type=TimeType, check_num = CheckNum, head = Head, fake_id = UID},
	do_check_update(Cfg, ?false, Req).

%% 批量修改公众号token和head
handle_mod_gzh(PostVals, Req) ->
	Token = ?l2i(?b2l(proplists:get_value(<<"token">>, PostVals, <<"0">>))),
	Head = util:bitstring_to_term(proplists:get_value(<<"head">>, PostVals, <<"[]">>)),
	Fun = fun(NodeId) ->
		case api_todayhot:get_node(NodeId) of
			Cfg = #cfg_news_source{fake_id = FakeId} ->
				Url = util:fbin(<<"https://mp.weixin.qq.com/cgi-bin/appmsg?action=list_ex&begin=0&count=5&fakeid=~s&type=9&query=&token=~s&lang=zh_CN&f=json&ajax=1"/utf8>>, [FakeId, Token]),
				NCfg = Cfg#cfg_news_source{url = Url, head = Head},
				mgr_todayhot:send({update_cfg_node,  NCfg});
			_ -> ignored
		end
	end,
	lists:foreach(Fun, api_todayhot:news_source_class(14)),
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json; charset=utf-8">>
	}, [], Req3).

handle_post(PostVals, Req) ->
	try 
		IsCheck = ?l2i(?b2l(proplists:get_value(<<"is_check">>, PostVals, <<"0">>))),
		% SourceId = ?l2i(?b2l(proplists:get_value(<<"source_id">>, PostVals, <<"0">>))),
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
		CheckNum =  ?l2i(?b2l(proplists:get_value(<<"check_num">>, PostVals, <<"40">>))),
		JsonData = proplists:get_value(<<"json_data">>, PostVals),
		Head = util:bitstring_to_term(proplists:get_value(<<"head">>, PostVals, <<"[]">>)),
		SourceId = lists:max(api_todayhot:news_source_class(Class)) +1,
		?INFO("xxxxxxxxxxxxxxxxxxxx_Name~w Class~w is_check~w SourceId~w",[Name, Class, IsCheck, SourceId]),
		Cfg = #cfg_news_source{source_id = SourceId, class = Class, sub_class = SubClass, type = Type, name = Name, summry = Summry, url = Url,
			is_top = IsTop, link_pre = LinkPre, data = Data, container = Container, title = Title, link_a = LinkA, url_type = UrlType,
			desc = Desc, author =Author, img = Img, count = Count, time = Time, time_type=TimeType, json_data = JsonData, head = Head, check_num = CheckNum},
		do_check_update(Cfg, IsCheck, Req)
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

do_check_update(Cfg, IsCheck, Req) ->
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
	}, Reply, Req3).


handle(Param, Req) ->
	% Status = ?l2i(?b2l(proplists:get_value(<<"status">>, Param, <<"0">>))),
	DelId = ?l2i(?b2l(proplists:get_value(<<"del">>, Param, <<"0">>))),
	Reply = case DelId > 0 of
		true ->
			mgr_todayhot:send({del_cfg_node, DelId}),
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
			desc = Desc, author =Author, img = Img, count = Count, time = Time, time_type=TimeType, json_data = JsonData, head = Head, check_num = CheckNum} ->
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
					,{check_num, CheckNum}
					]|List];
		_ -> List
	end,
	build_data(ets:next(?ETS_CFG_NODE, SourceId), NList).