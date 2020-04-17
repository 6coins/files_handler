%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 2014 duanzhichao
%%% @doc
%%%     files_handler模块的测试函数           
%%% @end

-module(files_handler_tests).


-include_lib("eunit/include/eunit.hrl").

-define(DIR, "ebin/").  % 测试用的目录
-define(FILENAME1, ?DIR ++ "files_to_file_datas_test_tmp1.txt").  % 测试用的文件1
-define(FILENAME2, ?DIR ++ "files_to_file_datas_test_tmp2.txt").  % 测试用的文件2

%% 调试用
%-compile(export_all).


%%%===================================================================
%%% Test functions
%%%===================================================================
files_to_file_datas_test() ->
    ok = create_files_to_file_datas_test_tmp(),                                     % 创建测试文件
    Config = [                                                                          % 配置
        {dir, ?DIR},                                                                    % 目录,其中有要处理的文件们
        {reg_exp, "files_to_file_datas_test_tmp*"},                                     % 搜索文件用的正则表达式
        {recursive, false},                                                             % 不递归的去子目录搜索文件 
        {filename_fun, {example_script, filename_fun}},                                 % 把文件路径处理成"文件数据"函数
        {chapters_conf,
            [                                                                           % 此类文件对应的"章信息"列表配置
                {
                    1,                                                                  % 章节序号
                    [            
                    {is_start_fun, {example_script, is_start_fun1}},                    % 判断当前行是否是1章开头的函数
                    {is_end_fun, {example_script, is_end_fun1}},                        % 判断当前行是否是1章结尾的函数
                    {content_fun, {example_script, content_fun1}}                       % 处理章开头以外内容的函数
                    ]
                },
                {
                    2,                                                                  % 章节序号
                    [            
                    {is_start_fun, {example_script, is_start_fun2}},                    % 判断当前行是否是2章开头的函数
                    {is_end_fun, {example_script, is_end_fun2}},                        % 判断当前行是否是2章结尾的函数
                    {content_fun, {example_script, content_fun2}}                       % 处理章开头以外内容的函数
                    ]
                }
            ]

        }
    ], 
    {ok, File_datas} = files_handler:files_to_file_datas(Config),                   % 调用要测试的函数
    ok = assert_files_to_file_datas_test_return(File_datas),                        % 断言结果
    ok = delete_files_to_file_datas_test_tmp(),                                     % 删除测试文件
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 创建测试文件
create_files_to_file_datas_test_tmp() ->
    File_content1 = "\n"
                   "chapter 1\n"
                   "1 2 3\n"
                   "\n"
                   "chapter 2\n"
                   "a b c\n"
                   "\n",                                    % 文件内容
    ok = file:write_file(?FILENAME1, File_content1),        % 创建文件1
    File_content2 = "\n"
                   "chapter 1\n"
                   "4 5 6\n"
                   "\n"
                   "chapter 2\n"
                   "d e f\n"
                   "\n",                                    % 文件内容
    ok = file:write_file(?FILENAME2, File_content2).        % 创建文件2


%% 删除测试文件
delete_files_to_file_datas_test_tmp() ->
    ok = file:delete(?FILENAME1),                           % 删除文件1
    ok = file:delete(?FILENAME2).                           % 删除文件2


%% 断言结果
assert_files_to_file_datas_test_return(File_datas) ->
    [File_data1, File_data2] = File_datas,
    ?FILENAME1 = proplists:get_value(filename, File_data1),
    "1" = proplists:get_value(other, File_data1),
    [{1, File_data1_chapter1}, {2, File_data1_chapter2}] = proplists:get_value(chapters, File_data1),
    ["chapter 1\n", "1 2 3\n"]= proplists:get_value(content_datas, File_data1_chapter1),
    ["a b c"]= proplists:get_value(content_datas, File_data1_chapter2),
    ?FILENAME2 = proplists:get_value(filename, File_data2),
    "2" = proplists:get_value(other, File_data2),
    [{1, File_data2_chapter1}, {2, File_data2_chapter2}] = proplists:get_value(chapters, File_data2),
    ["chapter 1\n", "4 5 6\n"]= proplists:get_value(content_datas, File_data2_chapter1),
    ["d e f"]= proplists:get_value(content_datas, File_data2_chapter2),
    ok.


