-define(POOL, poolboy).

-define(DEBUG(Farmat, Args), lager:log(debug, [], "**[~w:~w/~w:~w]"++Farmat, [?MODULE,?FUNCTION_NAME, ?FUNCTION_ARITY,?LINE|Args])).
-define(DEBUG(Farmat), ?DEBUG(Farmat, [])).
-define(INFO(Farmat, Args), lager:log(info, [], "**[~w:~w/~w:~w]"++Farmat, [?MODULE,?FUNCTION_NAME, ?FUNCTION_ARITY,?LINE|Args])).
-define(INFO(Farmat), ?INFO(Farmat, [])).
-define(ERR(Farmat, Args), lager:log(error, [], "**[~w:~w/~w:~w]"++Farmat, [?MODULE,?FUNCTION_NAME, ?FUNCTION_ARITY,?LINE|Args])).
-define(ERR(Farmat), ?ERR(Farmat, [])).



-define(b2l(B), binary_to_list(B)).
-define(l2b(L), list_to_binary(L)).
-define(l2i(L), list_to_integer(L)).
-define(i2l(L), integer_to_list(L)).

-define(IF(Arg, Val1, Val2), case Arg of true -> Val1; _ -> Val2 end).

-define(CALL(Pid, Request, Timeout),
    case catch gen_server:call(Pid, Request,Timeout) of
        {'EXIT', {timeout, _}} -> {error, timeout};
        {'EXIT', {noproc, _}} -> {error, noproc};
        {'EXIT', normal} -> {error, normal};
        {'EXIT', Reason} -> {error, Reason};
        Rtn -> Rtn
    end
).
-define(CALL(Pid, Request), ?CALL(Pid, Request, 5000)).

-define(true, 1).
-define(false, 0).