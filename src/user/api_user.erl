-module(api_user).
-include("common.hrl").
-include("todayhot.hrl").
-export([
		add_user/2
		,add_user_session/2
		,check_account/1
		,check_password/1
		,char_list/0
]).

char_list() ->
    [
        $a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v,$w,$x,$y,$z,
        $A,$B,$C,$D,$E,$F,$G,$H,$I,$J,$K,$L,$M,$N,$O,$P,$Q,$R,$S,$T,$U,$V,$W,$X,$Y,$Z,
        $@,$#,$%,$&,$*,$-,$_,$+,$=,$:,$;,$,,$.,$?,
        $0,$1,$2,$3,$4,$5,$6,$7,$8,$9
    ].

check_password(Password) ->
	PasswordL = unicode:characters_to_list(Password, utf8),
	case length(PasswordL) < 20 of
		true -> 
			check_password(PasswordL, char_list());
		_ -> false
	end.
	
check_password([], _) -> true;
check_password([P|T], L) ->
	case lists:member(P, L) of
		true -> check_password(T, L);
		_ -> false
	end.

check_account(Account) ->
	check_account([length, char, exist], Account).

%% 用户账号检测
check_account(length, Account) ->
	case unicode:characters_to_list(Account, utf8) < 10 of
		true -> true;
		_ -> {false, length}
	end;
check_account(char, Account) when is_list(Account) ->
    %name_banned_valid(list_to_bitstring(Text));
    check_account(char, unicode:characters_to_binary(Text, utf8));
check_account(char, Account) when is_bitstring(Account) ->
    case re:run(Account, "[^a-zA-Z0-9]", [{capture, none}, caseless, unicode]) of
        match -> {false, char};  %% 含有非法字符 
        nomatch -> true
    end;
check_account(exist, Account) ->
    case ets:lookup(Account, #todayhot_user.account, ?ETS_TODAYHOT_USER) of
        [_] -> {false, exist}; 
        [] -> true
    end.

check_account([], _) -> true;
check_account([P|T], Account) ->
	case check_account(P, Account) of
		true -> check_account(T, Account);
		Err -> Err
	end.

%% 
add_account(Account, Password) ->
	{ok, SessionId, Time} = mgr_user:call(add_account, {Account, Password}) of
	{ok, SessionId, Time}.

