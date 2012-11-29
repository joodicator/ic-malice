RUBBISH=*.o *.hi Alice{Lexer,Parser}.hs 
BINARIES=Alice{Lexer,Parser,Checker}Test compile

all: compile

compile: AliceCompiler.hs AliceChecker.hs AliceParser.hs
	ghc AliceCompiler.hs -o compile

AliceLexerTest: AliceLexerTest.hs AliceLexer.hs AliceToken.hs
	ghc AliceLexerTest.hs -o AliceLexerTest

AliceParserTest: AliceParserTest.hs AliceLexer.hs AliceParser.hs AliceASTShow.hs
	ghc AliceParserTest.hs -o AliceParserTest

AliceCheckerTest: AliceCheckerTest.hs AliceChecker.hs AliceParser.hs
	ghc AliceCheckerTest.hs -o AliceCheckerTest

AliceParser.hs: AliceParser.y AliceToken.hs AliceAST.hs AliceLexer.hs
	happy AliceParser.y

AliceLexer.hs: AliceLexer.x AliceToken.hs
	alex AliceLexer.x

clean:
	rm -f ${RUBBISH}

cleanall:
	rm -f ${BINARIES} ${RUBBISH}

test: AliceLexerTest AliceParserTest AliceCheckerTest
	./lexer_test.sh > /dev/null
	./parser_test.sh > /dev/null
	./checker_test.sh > /dev/null

.phony: all clean cleanall test

