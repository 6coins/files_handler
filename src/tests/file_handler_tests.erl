%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 2014 duanzhichao
%%% @doc
%%%     file_handler模块的测试函数           
%%% @end

-module(file_handler_tests).


-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "ebin/file_handler_tests_tmp.txt").  % 测试文件

%% 调试用
%-compile(export_all).


%%%===================================================================
%%% Test functions
%%%===================================================================
handle_file_data_test() ->
    ok = create_files(),                                     % 创建测试文件
    File_data_in = 
    [
     {filename, ?FILENAME}                                                  % 文件路径
    ],
    Config = 
    [                                                                       % 配置
     {chapters_conf,
      [                                                                     % 此类文件对应的"章信息"列表配置
       {1,                                                                  % 章配置序号
        [            
         {is_start_fun, {example_script, is_start_fun1}},                   % 判断当前行是否是1章开头的函数
         {is_end_fun, {example_script, is_end_fun1}},                       % 判断当前行是否是1章结尾的函数
         {content_fun, {example_script, content_fun1}}                      % 处理章开头以外内容的函数
        ]
       },
       {2,                                                                  % 章配置序号
        [            
         {is_start_fun, {example_script, is_start_fun2}},                   % 判断当前行是否是2章开头的函数
         {is_end_fun, {example_script, is_end_fun2}},                       % 判断当前行是否是2章结尾的函数
         {content_fun, {example_script, content_fun2}}                      % 处理章开头以外内容的函数
        ]
       }
      ]
     }
    ], 
    File_data_out = file_handler:handle_file_data(File_data_in, Config),    % 调用要测试的函数
    ok = assert_handle_file_data_test_return(File_data_out),                % 断言结果
    ok = delete_files(),                                                    % 删除测试文件
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 创建测试文件
create_files() ->
    File_content1 = "\n"
                   "chapter 1\n"
                   "1 2 3\n"
                   "\n"
                   "chapter 2\n"
                   "a b c\n"
                   "\n"
                   "chapter 1\n"                    % 注意这还有个重复的章节
                   "4 5 6\n"
                   "\n",                                    % 文件内容
    ok = file:write_file(?FILENAME, File_content1).         % 创建文件


%% 删除测试文件
delete_files() ->
    ok = file:delete(?FILENAME).                            % 删除文件


%% 断言结果
assert_handle_file_data_test_return(File_data_out) ->
    [{1, File_data1_chapter1}, {2, File_data1_chapter2}, {3, File_data1_chapter3}] = proplists:get_value(chapters, File_data_out),
    ["chapter 1\n", "1 2 3\n"]= proplists:get_value(content_datas, File_data1_chapter1),
    ["a b c"]= proplists:get_value(content_datas, File_data1_chapter2),
    ["chapter 1\n", "4 5 6\n"]= proplists:get_value(content_datas, File_data1_chapter3),
    ok.


