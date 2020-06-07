-module(dao_user).

-include("common.hrl").
-include("todayhot.hrl").

-export([load/0, up_user_db/1, up_session_db/1, insert_user/1]).

load() ->
    Sql = <<"select account, name, password, source_list, users from todayhot_user ">>,
    {ok, _, Data}  = mysql_poolboy:query(?POOL, Sql),
    Now = util:now(),
    Sql1 = <<"select account, session_id, time from todayhot_user_session where time > ?">>,
    {ok, _, Data1}  = mysql_poolboy:query(?POOL, Sql1, [Now]),

    Fun = fun([Account, UserName, Password, SourceList], Acc) ->
        NSourceList = util:bitstring_to_term(SourceList),
        [#todayhot_user{account = Account, name = UserName, password = Password, source_list = NSourceList}|Acc]
    end,
    UserL = lists:foldl(Fun, [], Data),
    Fun1 = fun([Account, SessionId, Time], Acc) ->
        [#todayhot_user_session{account = Account, session_id = SessionId, time = Time}|Acc]
    end,
    UserSessionL = lists:foldl(Fun1, [], Data1),
    {UserL, UserSessionL}.

insert_user(#todayhot_user{account = Account, name=Name, password = Password, source_list = SourceList}) ->
    Sql = "INSERT INTO todayhot_user(account, name, password, source_list) VALUES (?,?,?, ?) ",
    ok = mysql_poolboy:query(?POOL, Sql, [Account, Name, Password, util:term_to_bitstring(SourceList)]),
    ok.


up_user_db([]) ->
    ok;
up_user_db(Changes) ->
    Fun = fun(#todayhot_user{account=Account, name=Name, password = Password, source_list = SourceList}, NewsAcc) ->
        [Account, Name, Password, util:term_to_bitstring(SourceList) | NewsAcc]
    end,
    NUserL = lists:foldl(Fun, [], Changes),
    Len = length(Changes),
    SqlSub = "REPLACE INTO todayhot_user(account, name, password, source_list) VALUES ",
    Sql = list_to_binary(lists:concat([SqlSub, lists:duplicate(Len-1, "(?,?,?, ?),"), "(?,?,?, ?)"])),
    ok = mysql_poolboy:query(?POOL, Sql, NUserL),
    ok.

up_session_db([]) -> ok;
up_session_db(SessionL) ->
    Fun = fun(#todayhot_user_session{account=Account, session_id = SessionId, time = Time}, NewsAcc) ->
        [Account, SessionId, Time | NewsAcc]
    end,
    NewNodeL = lists:foldl(Fun, [], SessionL),
    Len = length(SessionL),
    SqlSub = "REPLACE INTO todayhot_user_session(account, session, time) VALUES ",
    Sql = list_to_binary(lists:concat([SqlSub, lists:duplicate(Len-1, "(?,?,?),"), "(?,?,?)"])),
    ok = mysql_poolboy:query(?POOL, Sql, NewNodeL),
    ok.
