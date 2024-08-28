all: main

main:
	swipl -o prolox -g main -c prolog/prolox/main.pl prolog/prolox/scanner.pl

clean:
	rm -f prolox
