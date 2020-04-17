%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 
%%% @doc
%%%     simple_matrix模块的测试函数           
%%% @end

-module(simple_matrix_tests).


-include_lib("eunit/include/eunit.hrl").

%% 调试用
%-compile(export_all).


%%%===================================================================
%%% Test functions
%%%===================================================================


%% 测试new/0, put_elem/2, get_elem/3 put_elem_by_list/2
simple_test() -> 
    Matrix1 = simple_matrix:new(),                              % 创建矩阵
    undefined = simple_matrix:get_elem(c1, r1, Matrix1),     % 断言

    Matrix2 = simple_matrix:put_elem({c1, r1, 9}, Matrix1),  % 放入元素
    {c1, r1, 9} = simple_matrix:get_elem(c1, r1, Matrix2),   % 得到元素,并断言
    9 = simple_matrix:get_elem_value(c1, r1, Matrix2),       % 得到元素值,并断言

    Matrix3 = simple_matrix:put_elem({c2, r1, 8}, Matrix2),  % 放入元素
    Matrix4 = simple_matrix:put_elem({c1, r1, 99}, Matrix3), % 放入已有行列的元素
    {c2, r1, 8} = simple_matrix:get_elem(c2, r1, Matrix4),   % 断言
    {c1, r1, 99} = simple_matrix:get_elem(c1, r1, Matrix4),  % 断言

    List = [{c2, r1, 88}, {c1, r1, 999}, {c2, r2, 999}],
    Matrix5 = simple_matrix:put_elem_by_list(List, Matrix4), % 放入表中的元素
    {c2, r1, 88} = simple_matrix:get_elem(c2, r1, Matrix5),   % 断言
    {c1, r1, 999} = simple_matrix:get_elem(c1, r1, Matrix5),  % 断言
    {c2, r2, 999} = simple_matrix:get_elem(c2, r2, Matrix5),  % 断言

    [c1, c2] = simple_matrix:get_clumns(Matrix5), % 得到列编号,并断言
    [r1, r2] = simple_matrix:get_rows(Matrix5), % 得到行编号,并断言

    [{c1, r1, 999}] = simple_matrix:get_sub([c1], [r1], Matrix5),  % 得到子矩阵,并断言
    ok.


%% 测试to_number/1
to_number_test() ->
    1 = simple_matrix:to_number(1),
    1.5 = simple_matrix:to_number(1.5),
    2 = simple_matrix:to_number("2"),
    1.5 = simple_matrix:to_number("1.5"),
    {error, {"cx", no_number}} = simple_matrix:to_number("cx"),
    ok.


%% 测试get_max/1, get_max/4, get_number_max/3
get_max_test() ->
    Matrix1 = [{c1, r1, 4}, {c2, r1, 2}, {c3, r2, 6.1}],
    [{c3, r2, 6.1}] = simple_matrix:get_max(Matrix1),                       % 得到最大值列表,断言
    Matrix2 = [{c1, r1, 4}, {c4, r1, 6.1}, {c2, r1, 2}, {c3, r2, 6.1}],
    [{c4, r1, 6.1}, {c3, r2, 6.1}] = simple_matrix:get_max(Matrix2),        % 得到最大值列表,断言
    Matrix3 = [{c1, r1, "4"}, {c4, r1, "6.1"}, {c2, r1, "2"}, {c3, r2, "6.1"}],
    [{c4, r1, "6.1"}, {c3, r2, "6.1"}] = simple_matrix:get_max(Matrix3),    % 得到最大值列表,断言
    Fun = fun(V1, V2) -> simple_matrix:to_number(V1) >= simple_matrix:to_number(V2) end,
    [{c3, r2, "6.1"}] = simple_matrix:get_max([c1, c2, c3], [r1,r2], Fun, Matrix3), % 得到指定范围内最大值列表,断言
    [{c3, r2, "6.1"}] = simple_matrix:get_number_max([c1, c2, c3], [r1,r2], Matrix3), % 得到指定范围内最大值列表,断言
    ok.


%% 测试generate_csv/4
generate_csv_test() ->
    Filename = "ebin/simple_matrix_tests_tmp.csv",
    Matrix = [{c2, r1, "2"},{c1, r1, "4"}, {c3, r2, "6.1"}],     % 测试矩阵
    ok = simple_matrix:generate_csv(Filename, [c1, c2, c3],[r1, r2], Matrix),       % 生成用于写入csv文件的数据
    {ok, Data} = file:read_file(Filename),
    <<",c1,c2,c3,\nr1,4,2,undefined,\nr2,undefined,undefined,6.1,\n">> = Data,      % 断言
    ok = file:delete(Filename),                                                     % 清理测试生成的文件
    ok.


%% 测试values_list_to_matrix/3,matrix_to_values_list/3
values_list_to_matrix_and_matrix_to_values_list_test() ->
    Clumns = [c1, c2, c3],
    Rows = [r1, r2, r3],
    V_list = 
        [[1,8,0], 
         [5,2,6], 
         [8,3,7]],    
    Matrix = 
        [{c1, r1, 1}, {c2, r1, 8}, {c3, r1, 0},
         {c1, r2, 5}, {c2, r2, 2}, {c3, r2, 6},
         {c1, r3, 8}, {c2, r3, 3}, {c3, r3, 7}],
    M = simple_matrix:values_list_to_matrix(Clumns, Rows, V_list),     % 把列表转化成矩阵
    V_list = [[ev(c1, r1, M), ev(c2, r1, M), ev(c3, r1, M)],
             [ev(c1, r2, M), ev(c2, r2, M), ev(c3, r2, M)],
             [ev(c1, r3, M), ev(c2, r3, M), ev(c3, r3, M)]],    % 断言转化结果
    V_list = simple_matrix:matrix_to_values_list(Clumns, Rows, Matrix),% 把矩阵转化成列表,并断言转化结果
    ok.


%% put_values_list/3测试
put_values_list_test() ->
    Matrix1 = get_test_matrix(),                                                % 得到测试矩阵
    Cs = [c4, c5],                                                              % 指定放入2列的编号
    Rs = [r1, r2, r3],                                                          % 指定放入列属于那些行的编号
    V_list = [[11,44],
               [22,55], 
               [33,66]],                                                        % 放入的值列表
    Matrix2 = simple_matrix:put_values_list(Cs, Rs, V_list, Matrix1),          % 放入列
    V_list2 = simple_matrix:matrix_to_values_list([c1, c2, c3, c4, c5], 
                                                  Rs, Matrix2),                 % 得到值列表
    V_list2 = [[1,8,0,11,44], 
                [5,2,6,22,55], 
                [8,3,7,33,66]],                                                 % 断言
    ok.


%% clumns_fun/5, clumns_fun_v_list/4测试
clumns_fun_test() ->
    From_clumns = [c1, c3],             % 按顺序指定要计算的列的编号
    To_clumn = c4,                      % 计算后的结果会放入的列的编号
    Rows = [r1, r2, r3],                % 按顺序指定行的编号
    Fun = fun(V1, V2) -> V1 + V2 end,   % 两个行间元素的计算规则函数
    Matrix1 = get_test_matrix(),        % 初始测试矩阵
    Matrix2 = simple_matrix:clumns_fun(From_clumns, To_clumn, Rows, Fun, Matrix1),  % 计算
    [[1,8,0,1], 
     [5,2,6,11], 
     [8,3,7,15]] 
        = simple_matrix:matrix_to_values_list([c1, c2, c3, c4], Rows, Matrix2),     % 断言
    [[1],
     [11],
     [15]] = simple_matrix:clumns_fun_v_list(From_clumns, Rows, Fun, Matrix1),      % 计算,断言
    ok.


%% rows_fun/5, rows_fun_v_list/4测试
rows_fun_test() ->
    From_rows = [r1, r3],               % 按顺序指定要计算的行的编号
    To_row = r4,                        % 计算后的结果会放入的行的编号
    Clumns = [c1, c2, c3],              % 按顺序指定列的编号
    Fun = fun(V1, V2) -> V1 + V2 end,   % 两个行间元素的计算规则函数
    Matrix1 = get_test_matrix(),        % 初始测试矩阵
    Matrix2 = simple_matrix:rows_fun(From_rows, To_row, Clumns, Fun, Matrix1),      % 计算
    [[1,8,0], 
     [5,2,6], 
     [8,3,7],
     [9,11,7]] 
        = simple_matrix:matrix_to_values_list(Clumns, [r1, r2, r3, r4], Matrix2),   % 断言
    [[9,11,7]] = simple_matrix:rows_fun_v_list(From_rows, Clumns, Fun, Matrix1),    % 计算,断言
    ok.



%%%===================================================================
%%% Internal functions
%%%===================================================================

    
%% 减小函数长度,方面测试演示
ev(Clumn, Row, Matrix) -> 
    simple_matrix:get_elem_value(Clumn, Row, Matrix).


%% 得到测试矩阵
get_test_matrix() ->
    Clumns = [c1, c2, c3],
    Rows = [r1, r2, r3],
    V_list = 
        [[1,8,0], 
         [5,2,6], 
         [8,3,7]],    
    Matrix = simple_matrix:values_list_to_matrix(Clumns, Rows, V_list),        % 把列表转化成矩阵
    Matrix.

