prefix := /usr/local
INSTALL:= /usr/bin/install

all: lambda_calculus

lambda_calculus: prolog/lambda_calculus.pl
	swipl -q -t lambda_compile prolog/lambda_calculus.pl

clear:
	rm -f lambda_calculus

install:
	$(INSTALL) -m 755 lambda_calculus $(prefix)/bin

uninstall:
	rm $(prefix)/bin/lambda_calculus
