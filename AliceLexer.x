--------------------------------------------------------------------------------
-- Herein is specified a lexical analyser for the Alice programming language.
-- It may be translated to Haskell using Alex: http://www.haskell.org/alex/
--------------------------------------------------------------------------------

{
module AliceLexer where
import Control.Monad
import System.IO
}

%wrapper    "posn"
$decit      = [0-9]
$letter     = [a-zA-Z_]

:-

$white+                         ;
\" ~\"* \"                      { tok }
\' ~\'* \'                      { tok }
$decit+                         { tok }
$letter(\-? [$letter$decit])*   { tok }
[^$white$letter$decit]+         { tok }

{
tok = flip const
main = mapM_ putStrLn . alexScanTokens =<< getContents
}
