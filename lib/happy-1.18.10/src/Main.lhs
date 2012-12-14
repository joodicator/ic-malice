-----------------------------------------------------------------------------
The main driver.

(c) 1993-2003 Andy Gill, Simon Marlow
GLR amendments (c) University of Durham, Ben Medlock 2001
-----------------------------------------------------------------------------

> module Main (main) where

Path settings auto-generated by Cabal:

> import Paths_happy

> import ParseMonad
> import GenUtils
> import AbsSyn
> import Grammar
> import Parser
> import First
> import LALR
> import ProduceCode (produceParser)
> import ProduceGLRCode 
> import Info (genInfoFile)
> import Target (Target(..))
> import System.Console.GetOpt
> import Control.Monad ( liftM )
> import System.Environment
> import System.Exit
> import Data.Char
> import System.IO
> import Data.Array( assocs, elems, (!) )
> import Data.List( nub, isSuffixOf )
> import Data.Version ( showVersion )
#if defined(mingw32_HOST_OS)
> import Foreign.Marshal.Array
> import Foreign
> import Foreign.C
#endif
#if defined(__GLASGOW_HASKELL__)
> import GHC.Exts ( unsafeCoerce# )
#endif

#if defined(__GLASGOW_HASKELL__)
> -- XXX What's going on here?!
> coerceParser :: ParseResult AbsSyn -> ParseResult AbsSyn
> coerceParser = unsafeCoerce#
#else
> coerceParser :: ParseResult AbsSyn -> ParseResult AbsSyn
> coerceParser = id
#endif

> main :: IO ()
> main = 

Read and parse the CLI arguments.

>       getArgs				>>= \ args ->
>       main2 args

> main2 :: [String] -> IO ()
> main2 args =

Read and parse the CLI arguments.

>       case getOpt Permute argInfo (constArgs ++ args) of
>               (cli,_,[]) | DumpVersion `elem` cli ->
>                  bye copyright
>               (cli,_,[]) | DumpHelp `elem` cli -> do
>                  prog <- getProgramName
>                  bye (usageInfo (usageHeader prog) argInfo)
>               (cli,_,_) | OptDebugParser `elem` cli
>                        && OptArrayTarget `notElem` cli -> do
>                  die "Cannot use debugging without -a\n"
>               (cli,[fl_name],[]) ->
>                  runParserGen cli fl_name
>               (_,_,errors) -> do
>                  prog <- getProgramName
>                  die (concat errors ++ 
>                       usageInfo (usageHeader prog) argInfo)

>  where 	
>    runParserGen cli fl_name =

Open the file.

>       readFile fl_name		     		>>= \ fl ->
>	possDelit (reverse fl_name) fl			>>= \ (file,name) ->

Parse, using bootstrapping parser.

>	case coerceParser (runP ourParser file 1) of {
>		FailP err -> die (fl_name ++ ':' : err);
>		OkP abssyn@(AbsSyn hd _ _ tl) -> 

Mangle the syntax into something useful.

>	case {-# SCC "Mangler" #-} (mangler fl_name abssyn) of {
>		Failed s -> die (unlines s ++ "\n");
>		Succeeded g -> 

#ifdef DEBUG

>       optPrint cli DumpMangle (putStr (show g)) >>

#endif


>       let first  	= {-# SCC "First" #-} (mkFirst g)
>	    closures    = {-# SCC "Closures" #-} (precalcClosure0 g)
>	    sets  	= {-# SCC "LR0_Sets" #-} (genLR0items g closures)
>	    _lainfo@(spont,prop) = {-# SCC "Prop" #-} (propLookaheads g sets first)
>	    la 		= {-# SCC "Calc" #-} (calcLookaheads (length sets) spont prop)
>	    items2	= {-# SCC "Merge" #-} (mergeLookaheadInfo la sets)
>           goto   	= {-# SCC "Goto" #-} (genGotoTable g sets)
>           action 	= {-# SCC "Action" #-} (genActionTable g first items2)
>	    (conflictArray,(sr,rr))   = {-# SCC "Conflict" #-} (countConflicts action)
>       in

#ifdef DEBUG

>       optPrint cli DumpLR0    (putStr (show sets))		>>
>       optPrint cli DumpAction (putStr (show action))      	>>
>       optPrint cli DumpGoto   (putStr (show goto))          	>>
>       optPrint cli DumpLA     (putStr (show _lainfo))		>>
>       optPrint cli DumpLA     (putStr (show la))		>>

#endif

Report any unused rules and terminals

>	let reduction_filter | OptGLR `elem` cli = any_reduction
>	                     | otherwise         = first_reduction
>	    (unused_rules, unused_terminals) 
>	                          = find_redundancies reduction_filter g action
>	in
>	optIO (not (null unused_rules))
>	   (hPutStrLn stderr ("unused rules: " ++ show (length unused_rules))) >>
>	optIO (not (null unused_terminals))
>	   (hPutStrLn stderr ("unused terminals: " ++ show (length unused_terminals))) >>

Report any conflicts in the grammar.

>       (case expect g of
>          Just n | n == sr && rr == 0 -> return ()
>          Just _ | rr > 0 -> 
>                  die ("The grammar has reduce/reduce conflicts.\n" ++
>                       "This is not allowed when an expect directive is given\n")
>          Just _ ->
>                 die ("The grammar has " ++ show sr ++ 
>                      " shift/reduce conflicts.\n" ++
>                      "This is different from the number given in the " ++
>                      "expect directive\n")
>          _ -> do

>	    (if sr /= 0
>		then hPutStrLn stderr ("shift/reduce conflicts:  " ++ show sr)
>		else return ())

>	    (if rr /= 0
>		then hPutStrLn stderr ("reduce/reduce conflicts: " ++ show rr)
>		else return ())

>       ) >>

Print out the info file.

>	getInfoFileName name cli		>>= \info_filename ->
>	let info = genInfoFile
>			(map fst sets)
>			g
>			action
>			goto
>			(token_specs g)
>			conflictArray
>			fl_name
>			unused_rules
>			unused_terminals
>	in

>	(case info_filename of
>		Just s  -> writeFile s info
>		Nothing -> return ())			>>

Now, let's get on with generating the parser.  Firstly, find out what kind
of code we should generate, and where it should go:

>	getTarget cli					>>= \target ->
>	getOutputFileName fl_name cli			>>= \outfilename ->
>	getTemplate getDataDir cli			>>= \template' ->
>	getCoerce target cli				>>= \opt_coerce ->
>	getStrict cli					>>= \opt_strict ->
>	getGhc cli					>>= \opt_ghc ->

Add any special options or imports required by the parsing machinery.

>	let
>	    header = Just (
>			(case hd of Just s -> s; Nothing -> "")
>			++ importsToInject target cli
>		     )
>	in


%---------------------------------------
Branch off to GLR parser production

>	let glr_decode | OptGLR_Decode `elem` cli = TreeDecode
>	               | otherwise                = LabelDecode
>	    filtering  | OptGLR_Filter `elem` cli = UseFiltering
>	               | otherwise                = NoFiltering
>	    ghc_exts   | OptGhcTarget `elem` cli  = UseGhcExts 
>						    (importsToInject target cli)
>						    (optsToInject target cli)
>	               | otherwise                = NoGhcExts
>	    debug      = OptDebugParser `elem` cli
>	in
>	if OptGLR `elem` cli 
>	then produceGLRParser outfilename   -- specified output file name 
>			      template'     -- template files directory
>			      action	    -- action table (:: ActionTable)
>			      goto 	    -- goto table (:: GotoTable)
>			      header 	    -- header from grammar spec
>			      tl	    -- trailer from grammar spec
>			      (debug, (glr_decode,filtering,ghc_exts))
>			                    -- controls decoding code-gen
>			      g		    -- grammar object
>	else 


%---------------------------------------
Resume normal (ie, non-GLR) processing

>	let 
>	    template = template_file template' target cli opt_coerce in

Read in the template file for this target:

>       readFile template				>>= \ templ ->

and generate the code.

>	getMagicName cli				>>= \ magic_name ->
>	let
>           outfile = produceParser 
>                       g
>                       action
>                       goto
>			(optsToInject target cli)
>                       header
>                       tl
>			target
>			opt_coerce
>			opt_ghc
>			opt_strict
>	    magic_filter = 
>	      case magic_name of
>		Nothing -> id
>		Just name' ->
>		  let
>		      small_name = name'
>		      big_name = toUpper (head name') : tail name'
>		      filter_output ('h':'a':'p':'p':'y':rest) =
>			small_name ++ filter_output rest
>		      filter_output ('H':'a':'p':'p':'y':rest) =
>			big_name ++ filter_output rest
>		      filter_output (c:cs) = c : filter_output cs
>		      filter_output [] = []
>		  in 
>		     filter_output 
>       in

>       (if outfilename == "-" then putStr else writeFile outfilename)
>		(magic_filter (outfile ++ templ))

Successfully Finished.

>	}}

-----------------------------------------------------------------------------

> getProgramName :: IO String
> getProgramName = liftM (`withoutSuffix` ".bin") getProgName
>    where str' `withoutSuffix` suff
>             | suff `isSuffixOf` str' = take (length str' - length suff) str'
>             | otherwise              = str'

> bye :: String -> IO a
> bye s = putStr s >> exitWith ExitSuccess

> die :: String -> IO a
> die s = hPutStr stderr s >> exitWith (ExitFailure 1)

> dieHappy :: String -> IO a
> dieHappy s = getProgramName >>= \prog -> die (prog ++ ": " ++ s)

> optIO :: Bool -> IO a -> IO a
> optIO fg io = if fg then io  else return (error "optIO")

#ifdef DEBUG
> optPrint :: [CLIFlags] -> CLIFlags -> IO () -> IO ()
> optPrint cli pass io = 
>       optIO (elem pass cli) (putStr "\n---------------------\n" >> io)
#endif

> constArgs :: [String]
> constArgs = []

-----------------------------------------------------------------------------
Find unused rules and tokens

> find_redundancies 
>        :: (LRAction -> [Int]) -> Grammar -> ActionTable -> ([Int], [String])
> find_redundancies extract_reductions g action_table = 
>	(unused_rules, map (env !) unused_terminals)
>    where
>	Grammar { terminals = terms,
>		  token_names = env,
>		  eof_term = eof,
>		  starts = starts',
>		  productions = productions'
>	        } = g

>	actions		 = concat (map assocs (elems action_table))
>	start_rules	 = [ 0 .. (length starts' - 1) ]
>	used_rules       = start_rules ++
>			   nub [ r | (_,a) <- actions, r <- extract_reductions a ]
>	used_tokens      = errorTok : eof : 
>			       nub [ t | (t,a) <- actions, is_shift a ]
>	n_prods		 = length productions'
>	unused_terminals = filter (`notElem` used_tokens) terms
>	unused_rules     = filter (`notElem` used_rules ) [0..n_prods-1]

> is_shift :: LRAction -> Bool
> is_shift (LR'Shift _ _)             = True
> is_shift (LR'Multiple _ LR'Shift{}) = True
> is_shift _                          = False

---
selects what counts as a reduction when calculating used/unused

> any_reduction :: LRAction -> [Int]
> any_reduction (LR'Reduce r _)    = [r] 
> any_reduction (LR'Multiple as a) = concatMap any_reduction (a : as)
> any_reduction _                  = []

> first_reduction :: LRAction -> [Int]
> first_reduction (LR'Reduce r _)   = [r] 
> first_reduction (LR'Multiple _ a) = first_reduction a   -- eg R/R conflict
> first_reduction _                 = []

------------------------------------------------------------------------------

> possDelit :: String -> String -> IO (String,String)
> possDelit ('y':'l':'.':nm) fl = return (deLitify fl,reverse nm)
> possDelit ('y':'.':nm) fl     = return (fl,reverse nm)
> possDelit f            _      = 
>	dieHappy ("`" ++ reverse f ++ "' does not end in `.y' or `.ly'\n")

> deLitify :: String -> String
> deLitify = deLit 
>  where 
>       deLit ('>':' ':r)  = deLit1 r
>       deLit ('>':'\t':r)  = '\t' : deLit1 r
>       deLit ('>':'\n':r)  = deLit r
>       deLit ('>':_)  = error "Error when de-litify-ing"
>       deLit ('\n':r) = '\n' : deLit r
>       deLit r        = deLit2 r
>       deLit1 ('\n':r) = '\n' : deLit r
>       deLit1 (c:r)    = c : deLit1 r
>       deLit1 []       = []
>       deLit2 ('\n':r) = '\n' : deLit r
>       deLit2 (_:r)    = deLit2 r
>       deLit2 []       = []

------------------------------------------------------------------------------
The command line arguments.

> data CLIFlags =
#ifdef DEBUG
>                 DumpMangle
>               | DumpLR0
>               | DumpAction
>               | DumpGoto
>		| DumpLA
>		
>               |
#endif
>                 DumpVersion
>               | DumpHelp
>		| OptInfoFile (Maybe String)
>		| OptTemplate String
>		| OptMagicName String
>
>		| OptGhcTarget
>		| OptArrayTarget
>		| OptUseCoercions
>		| OptDebugParser
>		| OptStrict
>		| OptOutputFile String
>		| OptGLR
>		| OptGLR_Decode
>		| OptGLR_Filter
>  deriving Eq

> argInfo :: [OptDescr CLIFlags]
> argInfo  = [
>    Option ['o'] ["outfile"] (ReqArg OptOutputFile "FILE")
>	"write the output to FILE (default: file.hs)",
>    Option ['i'] ["info"] (OptArg OptInfoFile "FILE")
>	"put detailed grammar info in FILE",
>    Option ['t'] ["template"] (ReqArg OptTemplate "DIR")
>	"look in DIR for template files",
>    Option ['m'] ["magic-name"] (ReqArg OptMagicName "NAME")
>	"use NAME as the symbol prefix instead of \"happy\"",
>    Option ['s'] ["strict"] (NoArg OptStrict)
>	"evaluate semantic values strictly (experimental)",
>    Option ['g'] ["ghc"]    (NoArg OptGhcTarget)
>	"use GHC extensions",
>    Option ['c'] ["coerce"] (NoArg OptUseCoercions)
>	"use type coercions (only available with -g)",
>    Option ['a'] ["array"] (NoArg OptArrayTarget)
>	"generate an array-based parser",
>    Option ['d'] ["debug"] (NoArg OptDebugParser)
>	"produce a debugging parser (only with -a)",
>    Option ['l'] ["glr"] (NoArg OptGLR)
>	"Generate a GLR parser for ambiguous grammars",
>    Option ['k'] ["decode"] (NoArg OptGLR_Decode)
>	"Generate simple decoding code for GLR result",
>    Option ['f'] ["filter"] (NoArg OptGLR_Filter)
>	"Filter the GLR parse forest with respect to semantic usage",
>    Option ['?'] ["help"] (NoArg DumpHelp)
>	"display this help and exit",
>    Option ['V','v'] ["version"] (NoArg DumpVersion)   -- ToDo: -v is deprecated
>       "output version information and exit"

#ifdef DEBUG

Various debugging/dumping options...

>    ,
>    Option [] ["mangle"] (NoArg DumpMangle)
>	"Dump mangled input",
>    Option [] ["lr0"] (NoArg DumpLR0)
>	"Dump LR0 item sets",
>    Option [] ["action"] (NoArg DumpAction)
>	"Dump action table",
>    Option [] ["goto"] (NoArg DumpGoto)
>	"Dump goto table",
>    Option [] ["lookaheads"] (NoArg DumpLA)
>	"Dump lookahead info"

#endif

>    ]

-----------------------------------------------------------------------------
How would we like our code to be generated?

> optToTarget :: CLIFlags -> Maybe Target
> optToTarget OptArrayTarget 	= Just TargetArrayBased
> optToTarget _			= Nothing

> template_file :: String -> Target -> [CLIFlags] -> Bool -> String
> template_file temp_dir target cli _coerce
>   = temp_dir ++ "/HappyTemplate" ++ array_extn ++ ghc_extn ++ debug_extn
>  where  
>	 ghc_extn   | OptUseCoercions `elem` cli = "-coerce"
>		    | OptGhcTarget    `elem` cli = "-ghc"
>		    | otherwise                  = ""
>
>	 array_extn | target == TargetArrayBased = "-arrays"
>		    | otherwise 		 = ""
>
>	 debug_extn | OptDebugParser `elem` cli  = "-debug"
>		    | otherwise			 = ""

Note: we need -cpp at the moment because the template has some
GHC version-dependent stuff in it.

> optsToInject :: Target -> [CLIFlags] -> String
> optsToInject tgt cli 
>	| OptGhcTarget `elem` cli   = "-fglasgow-exts -cpp"
> 	| tgt == TargetArrayBased   = "-cpp"
>	| OptDebugParser `elem` cli = "-cpp"
>	| otherwise                 = ""

> importsToInject :: Target -> [CLIFlags] -> String
> importsToInject tgt cli = "\n" ++ array_import ++ glaexts_import ++ debug_imports
>   where
>	glaexts_import | is_ghc		= import_glaexts
>		       | otherwise      = ""
>
>	array_import   | is_array	= import_array
>		       | otherwise	= ""
>
>	debug_imports  | is_debug	= import_debug
>		       | otherwise	= ""
>
>	is_ghc   = OptGhcTarget `elem` cli
>	is_debug = OptDebugParser `elem` cli
>	is_array = tgt == TargetArrayBased

CPP is turned on for -fglasgow-exts, so we can use conditional compilation:

> import_glaexts :: String
> import_glaexts = "import qualified GHC.Exts as Happy_GHC_Exts\n"

> import_array :: String
> import_array = "import qualified Data.Array as Happy_Data_Array\n"

> import_debug :: String
> import_debug =
> 		 "import qualified System.IO as Happy_System_IO\n" ++
> 		 "import qualified System.IO.Unsafe as Happy_System_IO_Unsafe\n" ++
> 		 "import qualified Debug.Trace as Happy_Debug_Trace\n"

------------------------------------------------------------------------------
Extract various command-line options.

> getTarget :: [CLIFlags] -> IO Target
> getTarget cli = case [ t | (Just t) <- map optToTarget cli ] of
> 			(t:ts) | all (==t) ts -> return t
>			[]  -> return TargetHaskell
>			_   -> dieHappy "multiple target options\n"

> getOutputFileName :: String -> [CLIFlags] -> IO String
> getOutputFileName ip_file cli
> 	= case [ s | (OptOutputFile s) <- cli ] of
>		[]   -> return (base ++ ".hs")
>			 where (base, _ext) = break (== '.') ip_file
>		f:fs -> return (last (f:fs))

> getInfoFileName :: String -> [CLIFlags] -> IO (Maybe String)
> getInfoFileName base cli
> 	= case [ s | (OptInfoFile s) <- cli ] of
>		[]	-> return Nothing
>		[f]     -> case f of
>			        Nothing -> return (Just (base ++ ".info"))
>				Just j  -> return (Just j)
>	        _many	-> dieHappy "multiple --info/-i options\n"

> getTemplate :: IO String -> [CLIFlags] -> IO String
> getTemplate def cli
> 	= case [ s | (OptTemplate s) <- cli ] of
>		[]	   -> def
>		f:fs       -> return (last (f:fs))

> getMagicName :: [CLIFlags] -> IO (Maybe String)
> getMagicName cli
> 	= case [ s | (OptMagicName s) <- cli ] of
>		[]	   -> return Nothing
>		f:fs       -> return (Just (map toLower (last (f:fs))))

> getCoerce :: Target -> [CLIFlags] -> IO Bool
> getCoerce _target cli
>	= if OptUseCoercions `elem` cli 
>	     then if OptGhcTarget `elem` cli
>			then return True
>			else dieHappy ("-c/--coerce may only be used " ++
>				       "in conjunction with -g/--ghc\n")
>	     else return False

> getGhc :: [CLIFlags] -> IO Bool
> getGhc cli = return (OptGhcTarget `elem` cli)

> getStrict :: [CLIFlags] -> IO Bool
> getStrict cli = return (OptStrict `elem` cli)

------------------------------------------------------------------------------

> copyright :: String
> copyright = unlines [
>  "Happy Version " ++ showVersion version ++ " Copyright (c) 1993-1996 Andy Gill, Simon Marlow (c) 1997-2005 Simon Marlow","",
>  "Happy is a Yacc for Haskell, and comes with ABSOLUTELY NO WARRANTY.",
>  "This program is free software; you can redistribute it and/or modify",
>  "it under the terms given in the file 'LICENSE' distributed with",
>  "the Happy sources."]

> usageHeader :: String -> String
> usageHeader prog = "Usage: " ++ prog ++ " [OPTION...] file\n"

-----------------------------------------------------------------------------