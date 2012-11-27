module AliceToken where

--------------------------------------------------------------------------------
data Token
  = Tand | Tbut | Tthen | Twhat | Ta | Ttoo | Thad | Tof
  | Tbecame | Tate | Tdrank | Tspoke | Tsaid | Tfound
  | Teither | Tor | Tperhaps | Tso | Tmaybe | Teventually | Tenough | Ttimes
  | Tbecause | TAlice | Twas | Tunsure | Twhich
  | TThe | Troom | Tlooking_glass | Tcontained | Topened | Tclosed
  | Tspider | Tnumber | Tletter | Tsentence
  
  | TComma | TDot | TQuest | TParenO | TParenC
  | TPlus | TMinus | TStar | TSlash | TPercent
  | TEqEq | TBangEq | TLess | TGreater | TLessEq | TGreaterEq
  | TAmpAmp | TBarBar | TAmp | TBar | TCaret | TTilde | TBang

  | TQSingle | TQDouble | TQChar !Char | TQEscape String
  | TNumberLiteral !Integer
  | TIdentifier String
  
  | TEOF
  deriving (Eq, Show)
