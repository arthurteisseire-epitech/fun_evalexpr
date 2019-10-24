##
## EPITECH PROJECT, 2018
## FUN_deBruijn_2018
## File description:
## Makefile
##

all:
	stack build
	stack install --local-bin-path ./

tests_run:
	stack test

clean:
	stack clean
	rm funEvalExpr
