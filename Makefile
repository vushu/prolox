all: main

test:
	swipl -s tests/tests.pl -g run_tests,halt -t 'halt(1)'

main:
	swipl -o prolox -g main -c prolog/prolox/main.pl

clean:
	rm -f prolox
