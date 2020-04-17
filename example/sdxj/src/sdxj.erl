%% coding: latin-1 %% Erlang/OTP 17.0之后erlang程序文件的代码中有中文必须加这个
%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @doc
%%%     把"问题和解决"的文档导出成csv文件
%%% @end

-module(sdxj).

-export([run/1]).

%% 调试用
%-compile(export_all).

-define(IDS4, % 4.0机器
        ["sjyhwebjr10", "sjyhwebjr11", "sjyhwebjr12",
         "sjyhwebjr1", "sjyhwebjr2", "sjyhwebjr3", "sjyhwebjr4", 
         "sjyhwebjr5", "sjyhwebjr6", "sjyhwebjr7", "sjyhwebjr8", 
         "sjyhwebjr9", 

         "sjyhapp10", "sjyhapp11", "sjyhapp12", "sjyhapp13", "sjyhapp14",
         "sjyhapp1", "sjyhapp2", "sjyhapp3", "sjyhapp4", 
         "sjyhapp5", "sjyhapp6", "sjyhapp7", "sjyhapp8", 
         "sjyhapp9", 

         "sjyhmem1", "sjyhmem2", "sjyhmem3", "sjyhmem4", "sjyhpic1", "sjyhpic2", "sjyhcj1", "sjyhcj2", "sjyhmg1", "sjyhmg2",

         "sjyhtsapp1", "sjyhtsapp2", "sjyhtsmng1", "sjyhtsmng2", "sjyhxxts1", "sjyhxxts2"]).

-define(IDS3, %3.0机器
        ["sjdb1", "sjdb2", "sjmg1", "sjmg2", "sjlog", "sjjrjr5", "sjjrjr1", "sjjrap5", "sjjrap1"]). 


-define(DIRS, ["/logs", "/ewp", "/gtp", "/redis", "/oracle", "/baklogs"]).


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
         {is_start_fun, {sdxj_script, is_start_fun}},                        % 判断当前行是否是开头的函数
         {is_end_fun, {sdxj_script, is_end_fun}},                            % 判断当前行是否是结尾的函数
         {content_fun, {sdxj_script, content_fun}}                           % 处理章开头以外内容的函数
        ]
       }
      ]
     }
    ],
    File_data_out = file_handler:handle_file_data(File_data_in, Config),    % 把文件数据转成erlang数据
    V_list = get_v_list(File_data_out),                                     % 得到值列表
                io:format("daoV_list -------~p~n",[V_list ]),
    V_list2 = lists:filter(fun([_,undefined,undefined]) -> false;(_) -> true end, V_list),
    Csv_data = csv_util:format(V_list2),                                     % 生成写入csv文件的内容
    ok = file:write_file(atom_to_list(Out_file), Csv_data).                 % 写文件


   


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 得到值列表
get_v_list(File_data_out) ->
    Chapters = proplists:get_value(chapters, File_data_out),
    Fun = 
          fun(E, Acc) ->
                %io:format("daoE-------~p~n",[E]),
                {_, [{content_datas, C}]} = E,
                %io:format("daoC-------~p~n",[C]),
                  Id = proplists:get_value(id, C),
                  MSUs = proplists:get_all_values(msu, C),
                  lists:foldl(fun(MSU, AccAcc) ->
                      %io:format("daoMSU-------~p~n",[MSU]),
                      [{mounted_on,Mou},{size,Size},{use,Use}] = MSU,
                      simple_matrix:put_elem_by_list([{size, Id ++":"++ Mou, Size},{use, Id ++":"++ Mou, Use}], AccAcc)
                  end
                  , Acc, MSUs)
        end,
    M2 = lists:foldl(Fun, [], Chapters),
    Clumns = [size, use], 
    Rows = lists:sort([X ++":"++ Y ||X <-?IDS4 ++ ?IDS3, Y <-?DIRS]),
    V_list1 = simple_matrix:matrix_to_values_list(Clumns, Rows, M2),% 得到数值列表
    _V_list2 = simple_matrix:values_list_add_clums_rows(Clumns, Rows, V_list1).                           % 把矩阵对应的值列表加上行列(以方便转成csv)

    


