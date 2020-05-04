-module(todayhot_handler).
 
-export([init/2]).

 -include("common.hrl").
 -include("todayhot.hrl").
init(Req0, State) ->
	Method = cowboy_req:method(Req0),
	% ?INFO(" Req0~w	",[Req0]),
	Req1 = case Method of
		<<"GET">> ->
			#{echo := Echo} = cowboy_req:match_qs([{echo, [], undefined}], Req0),
			echo(Method, Echo, Req0);
		<<"POST">> ->
			HasBody = cowboy_req:has_body(Req0),
			maybe_echo(Method, HasBody, Req0)
	end,
	{ok, Req1, State}.

echo(<<"GET">>, undefined, Req) ->
	cowboy_req:reply(400, #{}, <<"Missing echo parameter.">>, Req);
echo(<<"GET">>, Echo, Req) ->
	Node = ets:lookup(todayhot_ets, {2,?l2i(?b2l(Echo))}),
	NNewsL = case Node of
		[#todayhot_nodes{news=NewL}] ->
			Fun  = fun(#todayhot_news{id=Id, abstract=Abs, sub_news=SubNews}, Acc) ->
				Fun1=fun(#todayhot_sub_news{title=Title,url=Url}, Acc1) ->
					[[{title, Title}, {url,Url}]|Acc1]
				end,
				NSubNews = lists:foldl(Fun1, [], SubNews),
				[[{id, Id},{abstract, Abs}, {sub_news, NSubNews}]|Acc]
			end,
			lists:foldl(Fun, [], NewL);
		_ -> []
	end,
	Reply = jsx:encode(NNewsL),
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, Reply, Req);
echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

maybe_echo(<<"POST">>, true, Req0) ->
	{ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
	Echo = proplists:get_value(<<"echo">>, PostVals),
	post_echo(Echo, Req);
maybe_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).


post_echo(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
post_echo(Echo, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, Echo, Req).