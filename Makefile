all: lexer parser

lexer: AliceLexer.hs

parser: AliceParser.hs

AliceLexer.hs: AliceLexer.x
	alex AliceLexer.x

AliceParser.hs: AliceParser.y
	happy AliceParser.y

.phony: all lexer parser
