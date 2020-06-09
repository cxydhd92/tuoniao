
-module(todayhot_user_login_handler).

-export([init/2]).

-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_source.hrl").

init(Req0, State) ->
	Method = cowboy_req:method(Req0),
	% ?INFO(" Method~w	",[Method]),
	Req1 = case Method of
		<<"GET">> ->
			Param = cowboy_req:parse_qs(Req0),
			handle(Param, Req0);
		_ ->
			% ?INFO("Method~w",[Method]),
			cowboy_req:reply(405, Req0)
	end,
	{ok, Req1, State}.

handle(Param, Req) ->
	% ?INFO("Param~w",[Param]),
	case catch do_handle(Param, Req) of
		{ok,  Reply} -> Reply;
		_Err ->
			?ERR("_Err ~w",[_Err]),
			cowboy_req:reply(405, Req)
	end.

create_account(Account, Password) ->
	case api_user:check_password(Password) of
		true ->
			case api_user:check_account(Account) of
				true ->
					api_user:add_account(Account, Password);
				Err -> Err
			end;
		_ ->
			{false, password}
	end.

login_account(Account, Password) ->
	MPwd = util:md5(Password),
	case ets:lookup(?ETS_TODAYHOT_USER, Account) of
		[#todayhot_user{password = MPwd}] ->
			SessionID = base64:encode(crypto:strong_rand_bytes(32)),
			Time = util:now()+?d_s(30),
			mgr_user:send({up_session, Account, SessionID, Time}),
			{ok, SessionID, ?d_s(30)};
		_ ->
			{false, login}
	end.

do_handle([], Req) ->
	{ok, cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req)};
do_handle(PostVals, Req) ->
	IsReg = ?l2i(?b2l(proplists:get_value(<<"is_reg">>, PostVals, <<"1">>))),
	Account = proplists:get_value(<<"account">>, PostVals, undefined),
	Password = proplists:get_value(<<"password">>, PostVals, undefined),

	% ?INFO("ClassId ~w MinId ~w PageSize ~w",[ClassId, MinId, PageSize]),
	case is_bitstring(Account) andalso is_bitstring(Password) andalso is_integer(IsReg) of
		true->
			Ret = case IsReg of
				?true -> %% 注册
					create_account(Account, Password);
				_ ->
					login_account(Account, Password)
			end,
			Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
		    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
		    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
			{NReply, NReq} = case Ret of
				{ok, SessionID, Time} ->
					Reply = jsx:encode([{code, 0}]),
					Req4 = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID, Req3, #{max_age => Time}),
					{Reply, Req4};
				{false, length} ->
					Reply = jsx:encode([ {code, 1}]),
					{Reply, Req3};
				{false, char} ->
					Reply = jsx:encode([{code, 1}]),
					{Reply, Req3};
				{false, exist} ->
					Reply = jsx:encode([{code, 2}]),
					{Reply, Req3};
				{false, password} ->
					Reply = jsx:encode([{code, 3}]),
					{Reply, Req3};
				{false, login} ->
					Reply = jsx:encode([{code, 4}]),
					{Reply, Req3}
			end,
			
			?INFO("Reply ~w",[NReply]),
			
			{ok, cowboy_req:reply(200, #{
				<<"content-type">> => <<"application/json; charset=utf-8">>
			}, NReply, NReq)};
		_ ->
			{ok, cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req)}
	end.
	