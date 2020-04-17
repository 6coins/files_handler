%% coding: latin-1 %% Erlang/OTP 17.0之后erlang程序文件的代码中有中文必须加这个
%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @doc
%%%     把"问题和解决"的文档导出成csv文件
%%% @end

-module(faq_export).

-export([run/1]).

%% 调试用
%-compile(export_all).


%%%===================================================================
%%% api
%%%===================================================================
run([Int_file, Out_file]) ->
    File_data_in = 
    [
     {filename, atom_to_list(Int_file)}                  % 文件路径
    ],
    Config = 
    [                                                                       % 配置
     {chapters_conf,
      [                                                                     % 此类文件对应的"章信息"列表配置
       {1,                                                                  % 章配置序号
        [            
         {is_start_fun, {faq_script, is_start_fun}},                        % 判断当前行是否是开头的函数
         {is_end_fun, {faq_script, is_end_fun}},                            % 判断当前行是否是结尾的函数
         {content_fun, {faq_script, content_fun}}                           % 处理章开头以外内容的函数
        ]
       }
      ]
     }
    ],
    File_data_out = file_handler:handle_file_data(File_data_in, Config),    % 把文件数据转成erlang数据
    V_list = get_v_list(File_data_out),                                     % 得到值列表
    Csv_data = csv_util:format(V_list),                                     % 生成写入csv文件的内容
    ok = file:write_file(atom_to_list(Out_file), Csv_data).                 % 写文件


   


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 得到值列表
get_v_list(File_data_out) ->
    Chapters = proplists:get_value(chapters, File_data_out),
    Fun = fun(Data) -> files_handler_util:split(Data, ["原因", "解决"]) end,
    [Fun(Data) || {_,[{_, Data}]} <- Chapters].


