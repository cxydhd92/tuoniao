-module(todayhot_classid_handler).
 
-export([init/2]).

 -include("common.hrl").
 -include("todayhot.hrl").
init(Req0, State) ->
	Method = cowboy_req:method(Req0),
	% ?INFO(" Req0~w	",[Req0]),
	Req1 = handle(Method, Req0),
	{ok, Req1, State}.

handle(<<"GET">>, Req) ->
	Fun = fun(ClassId, Acc) ->
				ClassName = api_todayhot:get_class_name(ClassId),
				[[{class_id, ClassId},{class_name, ClassName}]|Acc]
		end,
	Datas = lists:foldl(Fun, [], ?todayhot_class_list),
	Reply = jsx:encode(Datas),
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, Reply, Req);
handle(_, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

