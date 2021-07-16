%% ------------------------------------------------------------------
%% db管理进程
%% ------------------------------------------------------------------
-module(mgr_db).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("common.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    ?INFO("start mgr_db",[]),
    Num = dao_db:get_tables_num(),
    case Num of 
        [0] -> %% 第一次执行创建所需表  
            create_sys_id()
            ,create_todayhot_node()
            ,create_todayhot_user_cfg_node()
            ,create_todayhot_news()
            ,create_todayhot_node_hotlist()
            ,create_todayhot_user()
            ,create_todayhot_user_session()
            ,create_cfg_todayhot_class()
            ,create_cfg_todayhot_node()
            ;
        _ ->
            ignored
    end,
    ?INFO("finish mgr_db",[]),
    {ok, Args}.

handle_call(Request, From, State) ->
	case do_handle_call(Request, From, State) of
		{reply, Reply, State} ->
			{reply, Reply, State};
		Reason ->
			?ERR("mgr_db Request ~w Reason ~w",[Request, Reason]),
			{reply, error, State}
	end.

handle_cast(Msg, State) ->
    case do_handle_cast(Msg, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_db Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
    case do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_db Request ~w Reason ~w",[Info, Reason]),
			{noreply, State}
	end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_handle_call(_Request, _From, State) ->
    {reply, ok, State}.

do_handle_cast(_Msg, State)->
    {noreply, State}.

do_handle_info(_Msg, State)->
    {noreply, State}.

create_sys_id() ->
    Sql = 
    <<"CREATE TABLE IF NOT EXISTS `sys_id`(
        `id` INT(11) NOT NULL COMMENT '新闻唯一id',
        PRIMARY KEY ( `id` )
        )ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"
    /utf8>>,
    ok  = mysql_poolboy:query(?POOL, Sql),
    Sql1 = <<"insert into sys_id(id) values(1);">>,
    ok  = mysql_poolboy:query(?POOL, Sql1),
    ok.

create_todayhot_node() ->
    Sql = 
    <<"CREATE TABLE IF NOT EXISTS `todayhot_node`(
        `node_id` INT(11) NOT NULL  COMMENT '订阅源节点id',
        `count` INT(11) NOT NULL  COMMENT '订阅数',
        `users` text NOT NULL  COMMENT '自定义源用户列表',
        `add_time` INT(11)  NOT NULL COMMENT '时间',
        PRIMARY KEY ( `node_id` )
        )ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"/utf8
    >>,
    ok  = mysql_poolboy:query(?POOL, Sql),
    ok.

create_todayhot_user_cfg_node() ->
    Sql = 
    <<"CREATE TABLE IF NOT EXISTS `todayhot_user_cfg_node`(
        `node_id` INT(11) NOT NULL  COMMENT '订阅源节点id' auto_increment, 
        `class` INT(11) NOT NULL  COMMENT '分类',
        `node_name` varchar(50) NOT NULL  COMMENT '节点名称',
        `url` text NOT NULL  COMMENT '源地址：支持rss、atom订阅源',
        `is_open` INT(11) NOT NULL  COMMENT '是否公开',
        `verify` INT(11) NOT NULL  COMMENT '是否验证通过',
        `add_time` INT(11)  NOT NULL COMMENT '时间',
        `verify_time` INT(11)  NOT NULL COMMENT '验证通过时间',
        PRIMARY KEY ( `node_id` ),
        KEY `class` (`class`)
        )ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 auto_increment=1000000; "/utf8
    >>,
    ok  = mysql_poolboy:query(?POOL, Sql),
    ok.

create_todayhot_news() ->
    Sql = 
    <<"CREATE TABLE IF NOT EXISTS `todayhot_news`(
        `id` INT(11) NOT NULL COMMENT '新闻唯一id',
        `class` INT(11) NOT NULL  COMMENT 'class',
        `node_id` INT(11) NOT NULL  COMMENT '节点id',
        `abstract` text NOT NULL COMMENT '新闻摘要',
        `title` varchar(1000) NOT NULL DEFAULT '' COMMENT '新闻标题',
        `url` varchar(500) NOT NULL COMMENT '新闻链接',
        `source` varchar(100) NOT NULL COMMENT '新闻来源',
        `count` varchar(100) NOT NULL COMMENT '新闻热度',
        `sub_news` text NOT NULL COMMENT  '相似新闻id列表',
        `same_id` INT(11) NOT NULL DEFAULT 0 COMMENT '相似标题对应新闻id',
        `img` VARCHAR(200) NOT NULL DEFAULT '' COMMENT '图片',
        `news_time` INT(11) NOT NULL  COMMENT '新闻时间',
        `time` INT(11) NOT NULL  COMMENT '抓取时间',
        PRIMARY KEY ( `id` ),
        KEY `class` (`node_id`)
        )ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"/utf8
    >>,
    ok  = mysql_poolboy:query(?POOL, Sql),
    ok.

create_todayhot_node_hotlist() ->
    Sql = 
    <<"CREATE TABLE IF NOT EXISTS `todayhot_node_hotlist`(
        `node_id` INT(11) NOT NULL  COMMENT '订阅源节点id',
        `zero` INT(11) NOT NULL  COMMENT '每天零点时间戳',
        `news` longtext NOT NULL  COMMENT '榜单数据',
        PRIMARY KEY ( `node_id`, `zero`)
        )ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"/utf8
    >>,
    ok  = mysql_poolboy:query(?POOL, Sql),
    ok.

create_todayhot_user() ->
    Sql = 
    <<"CREATE TABLE IF NOT EXISTS `todayhot_user`(
        `account` varchar(50) NOT NULL COMMENT '用户账号', 
        `name` varchar(50) NOT NULL  COMMENT '用户昵称',
        `password` varchar(100) NOT NULL  COMMENT '密码',
        `source_list` text NOT NULL  COMMENT '订阅源',
        PRIMARY KEY ( `account` )
        )ENGINE=InnoDB DEFAULT CHARSET=utf8mb4; "/utf8
    >>,
    ok  = mysql_poolboy:query(?POOL, Sql),
    ok.

create_todayhot_user_session() ->
    Sql = 
    <<"CREATE TABLE IF NOT EXISTS `todayhot_user_session`(
        `account` varchar(50) NOT NULL COMMENT '用户账号', 
        `session` varchar(100) NOT NULL  COMMENT '',
        `time` INT(11) NOT NULL  COMMENT '过期时间',
        PRIMARY KEY ( `account` )
        )ENGINE=InnoDB DEFAULT CHARSET=utf8mb4; "/utf8
    >>,
    ok  = mysql_poolboy:query(?POOL, Sql),
    ok.

create_cfg_todayhot_class() ->
    Sql = 
    <<"CREATE TABLE IF NOT EXISTS `cfg_todayhot_class`(
        `id` INT(11) NOT NULL  COMMENT '订阅源id',
        `sort`  INT(11) NOT NULL DEFAULT 0  COMMENT '',
        `name` varchar(50) NOT NULL DEFAULT '' COMMENT '',
        PRIMARY KEY ( `id` )
        )ENGINE=InnoDB DEFAULT CHARSET=utf8mb4; "/utf8
    >>,
    ok  = mysql_poolboy:query(?POOL, Sql),
    ok.

create_cfg_todayhot_node() ->
    Sql = 
    <<"CREATE TABLE IF NOT EXISTS `cfg_todayhot_node`(
        `id` INT(11) NOT NULL  COMMENT '订阅源id',
        `class` INT(11) NOT NULL  COMMENT '分类', 
        `sub_class` INT(11) NOT NULL DEFAULT 1 COMMENT '子分类', 
        `type` INT(11) NOT NULL DEFAULT 1 COMMENT '子分类', 
        `is_top` INT(11) NOT NULL  DEFAULT 0 COMMENT '子分类', 
        `time_type` INT(11) NOT NULL DEFAULT 0  COMMENT '子分类', 
        `name` varchar(250) NOT NULL DEFAULT "" COMMENT '用户账号', 
        `summry` varchar(250) NOT NULL DEFAULT "" COMMENT '用户账号', 
        `url` varchar(250) NOT NULL DEFAULT "" COMMENT '用户账号', 
        `url_type` INT(11) NOT NULL DEFAULT 0 COMMENT '用户账号', 
        `link_pre` varchar(250) NOT NULL DEFAULT "" COMMENT '用户账号', 
        `data` varchar(250) NOT NULL DEFAULT "" COMMENT '用户账号', 
        `container` varchar(250) NOT NULL DEFAULT "" COMMENT '用户账号', 
        `title` varchar(250) NOT NULL DEFAULT "" COMMENT '用户账号', 
        `link_a` varchar(250) NOT NULL DEFAULT "" COMMENT '用户账号', 
        `desc0` varchar(250) NOT NULL DEFAULT "" COMMENT '用户账号', 
        `author` varchar(250) NOT NULL DEFAULT "" COMMENT '用户账号', 
        `img` varchar(250) NOT NULL DEFAULT "" COMMENT '用户账号', 
        `count` varchar(250) NOT NULL DEFAULT "" COMMENT '用户账号', 
        `json_data` varchar(250) NOT NULL DEFAULT "" COMMENT '用户账号', 
        `head` varchar(1000) NOT NULL DEFAULT "" COMMENT '',
        `time` varchar(250) NOT NULL  COMMENT '',
        PRIMARY KEY ( `id` )
        )ENGINE=InnoDB DEFAULT CHARSET=utf8mb4; "/utf8
    >>,
    ok  = mysql_poolboy:query(?POOL, Sql),
    ok.
