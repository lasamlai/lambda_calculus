prefix := /usr/local
INSTALL:= /usr/bin/install
PRG    := $(shell ls prolog/*.pl)

all: lambda_calculus

lambda_calculus: $(PRG)
	swipl -t lambda_compile prolog/lambda_calculus.pl

clear:
	rm -f lambda_calculus

install: lambda_calculus
	$(INSTALL) -m 755 lambda_calculus $(prefix)/bin

uninstall:
	rm $(prefix)/bin/lambda_calculus
