# 默认
all: compile

# 编译
compile:
	@rm -rf ebin/
	@mkdir ebin/
	@erl -make -smp disable 

# 清理
clean:
	@rm -rf ebin/*
	@rm -rf tags
	@rm -rf TAGS
	@rm -rf erl_crash.dump

# 运行
run: compile
	@erl -pa ebin +K true


# 测试
test: test_files_handler_util test_file_handler	test_files_handler test_simple_matrix


# 测试
test_files_handler_util: compile
	@erl -pa ebin +K true -s eunit test files_handler_util -s init stop

# 测试 
test_file_handler: compile
	@erl -pa ebin +K true -s eunit test file_handler -s init stop

test_simple_matrix: compile
	@erl -pa ebin +K true -s eunit test simple_matrix -s init stop

# 测试 TODO
test_cyclic_chapter_file_handler: compile
	@erl -pa ebin +K true -s eunit test cyclic_chapter_file_handler -s init stop

# 测试
test_files_handler: compile
	@erl -pa ebin +K true -s eunit test files_handler -s init stop



