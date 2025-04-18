all: main

test:
	swipl -s tests/tests.pl -g run_tests,halt -t 'halt(1)'

test-env:
	swipl -s tests/env_test.pl -g run_tests,halt -t 'halt(1)'
test-scanner:
	swipl -s tests/scanner_test.pl -g run_tests,halt -t 'halt(1)'
test-parser:
	swipl -s tests/parser_test.pl -g run_tests,halt -t 'halt(1)'
test-interpreter:
	swipl -s tests/interpreter_test.pl -g run_tests,halt -t 'halt(1)'
test-isolated_cases:
	swipl -s tests/isolated_cases_test.pl -g run_tests,halt -t 'halt(1)'
test-builtin_functions:
	swipl -s tests/builtin_functions_test.pl -g run_tests,halt -t 'halt(1)'
test-loop:
	swipl -s tests/loop_test.pl -g run_tests,halt -t 'halt(1)'
test-scoping:
	swipl -s tests/scoping_test.pl -g run_tests,halt -t 'halt(1)'
test-fibonacci:
	swipl -s tests/fibonacci_test.pl -g run_tests,halt -t 'halt(1)'
main:
	swipl -o run_prolox -g main -c prolog/prolox/main.pl

clean:
	rm -f run_prolox
