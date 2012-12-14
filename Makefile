RUBBISH     = *.o *.hi Alice{Lexer,Parser}.hs 
BINARIES    = Alice{Lexer,Parser,Checker}Test compile
LIBRARIES   = alex happy

ALEX        = alex/dist/build/alex/alex
ALEX_ARG    = --template=alex

HAPPY       = happy/dist/build/happy/happy
HAPPY_ARG   = --template=happy

all: compile

compile: AliceCompiler.hs AliceChecker.hs AliceParser.hs
	ghc AliceCompiler.hs -o compile

AliceLexerTest: AliceLexerTest.hs AliceLexer.hs AliceToken.hs
	ghc AliceLexerTest.hs -o AliceLexerTest

AliceParserTest: AliceParserTest.hs AliceLexer.hs AliceParser.hs AliceASTShow.hs
	ghc AliceParserTest.hs -o AliceParserTest

AliceCheckerTest: AliceCheckerTest.hs AliceChecker.hs AliceParser.hs
	ghc AliceCheckerTest.hs -o AliceCheckerTest

AliceParser.hs: AliceParser.y AliceToken.hs AliceAST.hs AliceLexer.hs $(HAPPY)
	$(HAPPY) $(HAPPY_ARG) AliceParser.y

AliceLexer.hs: AliceLexer.x AliceToken.hs $(ALEX)
	$(ALEX) $(ALEX_ARG) AliceLexer.x

$(ALEX):
	cp -R lib/alex-3.0.2 alex
	cd alex && cabal configure && cabal build

$(HAPPY):
	cp -R lib/happy-1.18.10 happy
	cd happy && cabal configure && cabal build

clean:
	rm -rf ${RUBBISH}

clean2: clean
	rm -rf ${BINARIES}

clean3: clean2
	rm -rf ${LIBRARIES}

test: AliceLexerTest AliceParserTest AliceCheckerTest
	./lexer_test.sh > /dev/null
	./parser_test.sh > /dev/null
	./checker_test.sh > /dev/null

.phony: all clean cleanall test

