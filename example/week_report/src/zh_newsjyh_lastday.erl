%% coding: latin-1 %% Erlang/OTP 17.0之后erlang程序文件的代码中有中文必须加这个
%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 
%%% @doc
%%%     华夏巡检周报生成模块
%%% @end
-module(zh_newsjyh_lastday).

-export([get_datas/1]).

-include("week_report.hrl").




%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% 得到文档中的数据
%% @end
-spec get_datas(Conf :: list()) -> [#zh_newsjyh_lastday_data{}].
%%--------------------------------------------------------------------
get_datas(Conf) ->
    In_dir = proplists:get_value(in_dir, Conf),
    Config = 
    [                                                                       % 配置
     {dir, In_dir},                                                         % 目录,其中有要处理的文件们
     {reg_exp, "^zh_newsjyh_lastday_.*txt$"},                               % 搜索文件用的正则表达式
     {recursive, false},                                                    % 不递归的去子目录搜索文件 
     {filename_fun, {zh_newsjyh_lastday_script, filename_fun}},             % 把文件路径处理成"文件数据"函数
     {chapters_conf,
      [                                                                     % 此类文件对应的"章信息"列表配置
       {1,                                                                  % 章节序号
        [            
         {is_start_fun, {zh_newsjyh_lastday_script, is_start_fun1}},        % 判断当前行是否是1章开头的函数
         {is_end_fun, {zh_newsjyh_lastday_script, is_end_fun1}},            % 判断当前行是否是1章结尾的函数
         {content_fun, {zh_newsjyh_lastday_script, content_fun}}            % 处理章开头以外内容的函数
        ]
       },
       {2,                                                                  % 章节序号
        [            
         {is_start_fun, {zh_newsjyh_lastday_script, is_start_fun2}},        % 判断当前行是否是2章开头的函数
         {is_end_fun, {zh_newsjyh_lastday_script, is_end_fun2}},            % 判断当前行是否是2章结尾的函数
         {content_fun, {zh_newsjyh_lastday_script, content_fun}}            % 处理章开头以外内容的函数
        ]
       }
      ]
     }
    ],
    {ok, File_datas} = files_handler:files_to_file_datas(Config),                  % 把文件处理成erlang数据
    [handle_file_data(F) || F <- File_datas].% 把每个文件数据处理成{日期, cpu用率列表, 内存使用率列表}的形式




%%%===================================================================
%%% Internal functions
%%%===================================================================


%% 把每个文件数据处理成{日期, cpu使用率列表, 内存使用率列表}的形式
handle_file_data(File_data) ->
    Date = proplists:get_value(date, File_data),            % 日期
    Chapters = proplists:get_value(chapters, File_data),    % 章数据列表
    [{content_datas, CPU_datas}] = proplists:get_value(1, Chapters),    % cpu使用率列表源数据
    [{content_datas, Mem_datas}] = proplists:get_value(2, Chapters),    % 内存使用率列表源数据
    #zh_newsjyh_lastday_data{date = Date, 
                             cpu_datas = handle_datas(CPU_datas),  
                             mem_datas = handle_datas(Mem_datas)}.


%% 把章数据进一步处理
handle_datas(Datas) ->
    Fun = fun("sjyh" ++ _ = X, {[], _}) -> {[X], []};
             ("sjyh" ++ _ = X, {Current, Res_acc_in}) -> {[X], [{hd(Current), hd(lists:reverse(Current))}|Res_acc_in]}; 
             (X, {Current, Res_acc_in}) -> {Current ++ [X], Res_acc_in}
          end,
    {Current_end, Res_acc_out} = lists:foldl(Fun, {[], []}, Datas),
    [{hd(Current_end), hd(lists:reverse(Current_end))} | Res_acc_out].  % (最后一发也要算上)
    % 如:Datas的形式如["sjyhxx1","0.1", ..., "0.18", "sjyhxx2", "0.1", ..., "0.9", "sjyhxxN", ...]
    % 此函数会取元素"sjyhxx1","0.1", ..., "0.18"中的第一个"sjyhxx1"和最后一个"0.18",组成{"sjyhxx1", "0.18"}
    % 以此类推整个Datas处理成[{"sjyhxx1", "0.18"},{"sjyhxx2", "0.9"}, {"sjyhxxN", ...}, ...]



