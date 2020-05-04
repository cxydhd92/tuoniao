%% ------------------------------------------------------------------
%% 测试模块
%% ------------------------------------------------------------------
-module(test).
-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_source.hrl").
-include_lib("xmerl/include/xmerl.hrl").  
-compile(export_all).

test_html(Cfg=#cfg_news_source{class=Class, source_id=SourceId, url = Url}, TodayData, Now) ->
	case ibrowse:send_req(?b2l(Url), [], get) of
		{ok, "200", _ResponseHeaders, Body} ->
			#cfg_news_source{data=Data, container=ContainerF, title=TitleF, link_a=LinkA,
			desc=DescF, author=AuthorF, img=ImgF, count=CountF, time=TimeF} = Cfg,
			{_, Container} = re:run(Body, Data, [{capture, all_but_first, binary}, global]),
			{_, Item} = re:run(Container, ContainerF, [{capture, all_but_first, binary}, global]),
			{_, ItemTitleL} = re:run(Item, TitleF, [{capture, all_but_first, binary}, global]),
			{_, ItemLinkAL} = re:run(Item, LinkA, [{capture, all_but_first, binary}, global]),
			?INFO("ItemTitleL~w",[ItemTitleL]);
			% {NNews, NTodayData, NewHotList} = do_parse_html(Cfg, TodayData, [], [], Now, ItemTitleL, ItemLinkAL, ItemDescL, ItemAuthorFL, ItemImgFL, ItemCountFL, ItemTimeFL),
			% ?IF(length(NNews)>0, mgr_todayhot:send({up_news, Class, SourceId, NNews, Now}), ignored),
			% ?IF(NewHotList=/=[], api_todayhot:insert_new_hot(Class, SourceId, lists:reverse(NewHotList)), ignored),
			% ?INFO("SourceId~w NNews len ~w", [SourceId, length(NNews)]),
			% NTodayData;
		_Err ->
			?ERR("Url ~ts fail ~w", [Url, _Err]),
			ok
    end.

test() ->
	case ibrowse:send_req("https://s.weibo.com", [], get) of
		{ok, "200", _ResponseHeaders, Body} ->
			?INFO("xBoday~w",[Body]),
			{_, Container} = re:run(Body, <<"<tbody>([\\s\\S]+?)</tbody>"/utf8>>, [{capture, all_but_first, binary}, global]),
			{_, Item} = re:run(Container, <<"<li[^>]+?>\\s+?<span[^>]+?>([\\s\\S]+?)</li>">>, [{capture, all_but_first, binary}, global]),
			{_, [ItemTitle|_]} = re:run(Item, <<"<a[^>]+?>([\\s\\S]+?)</a>">>, [{capture, all_but_first, binary}, global]),
			{_, [[ItemUrl]|_]} = re:run(Item, <<"href=\"([^\"]+?)\"">>, [{capture, all_but_first, binary}, global]),
			
			% ?INFO("Container~ts",[?l2b(Container)]),
			?INFO("ItemTitle~ts",[ItemTitle]),
			?INFO("ItemTitle~ts",[fbin(<<"~s~s"/utf8>>, [<<"https://s.weibo.com">>, ItemUrl])]),
			ok;
		_Err ->
			?ERR("fail ~w", [_Err])
    end,
	ok.

test_xml() ->
	case ibrowse:send_req("https://rss.huxiu.com/", [], get) of
		{ok, "200", _ResponseHeaders, Body} ->
			{XmlDoc, _B} = xmerl_scan:string(Body),
			 Items = xmerl_xpath:string("/rss/channel/item",XmlDoc),  
			lists:foreach(fun(Item)->  
                                     [#xmlText{value=IdValue}] = xmerl_xpath:string("/item/id/text()",Item),  
                                     [#xmlText{value=TItleValue}] = xmerl_xpath:string("/item/title/text()",Item),  
                                    	
                                     ?INFO("IdValue~ts ~ts",[IdValue, TItleValue])
                             end,  
                             Items),  
			?INFO("Items~w",[Items]),
			% ?INFO("ItemTitle~w",[Doc]),
			ok;
		_Err ->
			?ERR("fail ~w", [_Err])
    end,
	ok.

xml_namespace() ->
    "<?xml version=\"1.0\"?>"
	"<!-- initially, the default namespace is \"books\" -->"
	"<book xmlns='urn:loc.gov:books' xmlns:isbn='urn:ISBN:0-395-36341-6'>"
	"<title>Cheaper by the Dozen</title>"
	"<isbn:number>1568491379</isbn:number>"
	"<notes>"
	"<!-- make HTML the default namespace for some comments -->"
	"<p xmlns='urn:w3-org-ns:HTML'>"
	"This is a <i>funny</i> book!"
	"</p>"
	"</notes>"
	"</book>".

fbin(Bin, Args) ->
    list_to_binary(io_lib:format(Bin, Args)).

fbin(Bin) ->
    fbin(Bin, []).