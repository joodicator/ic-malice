module AliceToken where

--------------------------------------------------------------------------------
data Token
  = TAnd | TBut | TThen | TWhat | TA | TToo | THad | TOf
  | TBecame | TAte | TDrank | TSpoke | TSaid | TFound
  | TEither | TOr | TPerhaps | TSo | TMaybe | TEventually | TEnough | TTimes
  | TBecause | TAlice | TWas | TUnsure | TWhich
  | TThe | TRoom | TLookingGlass | TContained | TOpened | TClosed
  | TSpider | TNumber | TLetter | TSentence
  
  | TComma | TDot | TQuest | TParenO | TParenC
  | TPlus | TMinus | TStar | TSlash | TPercent
  | TEqEq | TBangEq | TLess | TGreater | TLessEq | TGreaterEq
  | TAmpAmp | TBarBar | TAmp | TBar | TCaret | TTilde | TBang

  | TQSingle | TQDouble | TQChar !Char | TQEscape String
  | TNumberLiteral !Integer
  | TIdentifier String
  
  | TEOF | TError String
  deriving (Eq, Show)
