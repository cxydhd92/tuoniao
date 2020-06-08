-module(todayhot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(C_ACCEPTORS,  100).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Routes    = routes(),
    Dispatch  = cowboy_router:compile(Routes),
    Port      = port(),
    TransOpts = [{port, Port}],
    ProtoOpts = #{env => #{dispatch => Dispatch}},
    {ok, _}   = cowboy:start_clear(http, TransOpts, ProtoOpts),
    todayhot_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================
routes() ->
    [
     {'_', [
            {"/", todayhot_handler, []}
            ,{"/api/class", todayhot_classid_handler, []}
            ,{"/api/news", todayhot_news_handler, []}
            ,{"/api/nodes", todayhot_nodeid_handler, []}
            ,{"/api/node/news", todayhot_node_news_handler, []}
            ,{"/api/node/hotlist", todayhot_node_hotlist_handler, []}
            ,{"/api/user/login", todayhot_user_login_handler, []}
            ,{"/api/user/nodes", todayhot_user_nodeid_handler, []}
            ,{"/api/user/news", todayhot_user_news_handler, []}
           ]}
    ].
 
port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.