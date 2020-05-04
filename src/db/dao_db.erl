-module(dao_db).
-include("common.hrl").

-export([get_tables_num/0]).

get_tables_num() ->
    Sql = <<"SELECT COUNT(*) FROM information_schema.TABLES   WHERE table_schema = 'hot'">>,
    {ok, _, Data}  = mysql_poolboy:query(?POOL, Sql),
    case Data of
        [Size] ->
            Size;
        Ret ->
            ?ERR("get_tables_num Ret ~w",[Ret]),
            0
    end.