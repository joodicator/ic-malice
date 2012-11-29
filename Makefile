RUBBISH=*.o *.hi Alice{Lexer,Parser}.hs 
BINARIES=Alice{Lexer,Parser,Checker}Test

all: AliceLexerTest AliceParserTest AliceCheckerTest

AliceLexerTest: AliceLexerTest.hs AliceLexer.hs AliceToken.hs
	ghc AliceLexerTest.hs -o AliceLexerTest

AliceParserTest: AliceParserTest.hs AliceLexer.hs AliceParser.hs AliceASTShow.hs
	ghc AliceParserTest.hs -o AliceParserTest

AliceCheckerTest: AliceCheckerTest.hs AliceChecker.hs AliceParser.hs
	ghc AliceCheckerTest.hs -o AliceCheckerTest

AliceParser.hs: AliceParser.y AliceToken.hs AliceAST.hs
	happy AliceParser.y

AliceLexer.hs: AliceLexer.x AliceToken.hs
	alex AliceLexer.x

clean:
	rm -f ${RUBBISH}

cleanall:
	rm -f ${BINARIES} ${RUBBISH}

test: all
	./lexer_test.sh > /dev/null
	./parser_test.sh > /dev/null

.phony: all clean cleanall test

