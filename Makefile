all: AliceLexer.hs AliceParser.hs AliceLexerTest

AliceLexerTest: AliceLexerTest.hs AliceLexer.hs AliceToken.hs
	ghc AliceLexerTest.hs -o AliceLexerTest

AliceParser.hs: AliceParser.y AliceToken.hs AliceAST.hs
	happy AliceParser.y

AliceLexer.hs: AliceLexer.x AliceToken.hs
	alex AliceLexer.x

clean:
	rm -f AliceLexerTest *.o *.hi

.phony: all clean

