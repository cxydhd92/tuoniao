-module(todayhot_sup).

-behaviour(supervisor).

-include("common.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ?INFO("start todayhot_sup", []),
	{PoolOptions, MySqlOptions} = parse_db_options(),
    ChildSpecs = [
        %% MySQL pools
        mysql_poolboy:child_spec(?POOL, PoolOptions, MySqlOptions)
        %% other workers...
        ,?CHILD(mgr_db, worker)
        ,?CHILD(mgr_todayhot, worker)
        ,?CHILD(mgr_source, worker)
        ,?CHILD(mgr_user, worker)
    ],
    ?INFO("finish todayhot_sup",[]),
    {ok, { {one_for_one, 10, 10}, ChildSpecs} }.

%% 数据库配置
parse_db_options()->
	PoolOptions  = [{size, 20}, {max_overflow, 60}],
    MySqlOptions = [{user, "root"}, {host, "127.0.0.1"}, {password, "123456"}, {database, "hot"}, {keep_alive, true}, {query_timeout, 30000}],
    {PoolOptions, MySqlOptions}.