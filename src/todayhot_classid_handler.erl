%% 获取分类接口
-module(todayhot_classid_handler).
 
-export([init/2]).

 -include("common.hrl").
 -include("todayhot.hrl").
 -include("cfg_news_class.hrl").
init(Req0, State) ->
	Method = cowboy_req:method(Req0),
	% ?INFO(" Req0~w	",[Req0]),
	Req1 = handle(Method, Req0),
	{ok, Req1, State}.

handle(<<"GET">>, Req) ->
	Fun = fun(ClassId, Acc) ->
				#cfg_news_class{id = ClassId, name = ClassName} = cfg_news_class:get(ClassId),
				[[{class_id, ClassId},{class_name, ClassName}]|Acc]
		end,
	Datas = lists:foldl(Fun, [], cfg_news_class:list_key()),
	Reply = jsx:encode([{data, Datas}]),
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json; charset=utf-8">>
	}, Reply, Req);
handle(_, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

