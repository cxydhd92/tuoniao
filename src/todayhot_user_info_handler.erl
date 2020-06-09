-module(todayhot_user_info_handler).
 
-export([init/2]).

-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_source.hrl").

init(Req0, State) ->
	Method = cowboy_req:method(Req0),
	% ?INFO(" Req0~w	",[Req0]),
	NReq = case Method of
		<<"GET">> ->
			Cookies = cowboy_req:parse_cookies(Req0),
			case lists:keyfind(<<"sessionid">>, 1, Cookies) of
				{_, SessionId} ->
					% Param = cowboy_req:parse_qs(Req0),
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
			case catch do_handle(Account, Req) of
				{ok,  Reply} -> Reply;
				_Err ->
					?ERR("_Err ~w",[_Err]),
					cowboy_req:reply(405, Req)
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

do_handle(Account, Req) ->
	Reply = jsx:encode([{code, 0}, {account, Account}]),
	% ?INFO("Reply ~w",[Reply]),
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
	{ok, cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json; charset=utf-8">>
	}, Reply, Req3)}.

	