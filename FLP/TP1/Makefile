all: Main

.PHONY: test clean

Lexer.hs : Lexer.x
	alex --info Lexer.x

Parser.hs : Ast.hs Parser.y
	happy --info Parser.y

Main : Lexer.hs Parser.hs Ast.hs Interpreter.hs Main.hs
	ghc --make Main.hs

test :
	./test.sh

clean :
	rm -rf *.o *.hi *.info Parser.hs Lexer.hs Main
