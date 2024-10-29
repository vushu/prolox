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

main:
	swipl -o run_prolox -g main -c prolog/prolox/main.pl

clean:
	rm -f run_prolox
