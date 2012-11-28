module AliceToken where

--------------------------------------------------------------------------------
-- A concrete occurence of a lexical token, with information about its
-- manifestion in the input stream.
data TokenContext
  = TC                          {
        tcTok :: !Token         , -- The corresponding abstract token.
        tcPos :: !(Int,Int,Int) , -- Position (chr, row, col) in the stream.
        tcStr :: String         } -- Concrete representation from the input.
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- An abstract lexical token.
data Token
  = TAnd
  | TBut
  | TThen
  | TWhat
  | TA
  | TToo
  | THad
  | TOf
  | TBecame
  | TAte
  | TDrank
  | TSpoke
  | TSaid
  | TFound
  | TEither
  | TOr
  | TPerhaps
  | TSo
  | TMaybe
  | TEventually
  | TEnough
  | TTimes
  | TBecause
  | TAlice
  | TWas
  | TUnsure
  | TWhich
  | TThe
  | TRoom
  | TLookingGlass
  | TContained
  | TOpened
  | TClosed
  | TSpider
  | TNumber
  | TLetter
  | TSentence
  | TPiece
  | TComma
  | TDot
  | TQuest
  | TParenO
  | TParenC
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TPercent
  | TEqEq
  | TBangEq
  | TLess
  | TGreater
  | TLessEq
  | TGreaterEq
  | TAmpAmp
  | TBarBar
  | TAmp
  | TBar
  | TCaret
  | TTilde
  | TBang
  | TQSingle
  | TQDouble
  | TQChars String
  | TNumberLiteral !Integer
  | TIdentifier String
  | T's
  | TEOF
  | TError
  deriving (Eq, Show)
