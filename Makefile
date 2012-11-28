all: AliceLexer.hs AliceParser.hs AliceLexerTest AliceParserTest

AliceLexerTest: AliceLexerTest.hs AliceLexer.hs AliceToken.hs
	 ghc AliceLexerTest.hs -o AliceLexerTest

AliceParserTest: AliceParserTest.hs AliceLexer.hs AliceParser.hs
	ghc AliceParserTest.hs -o AliceParserTest

AliceParser.hs: AliceParser.y AliceToken.hs AliceAST.hs
	happy AliceParser.y

AliceLexer.hs: AliceLexer.x AliceToken.hs
	alex AliceLexer.x

clean:
	rm -f AliceLexerTest AliceParserTest AliceLexer.hs AliceParser.hs *.o *.hi

.phony: all clean

