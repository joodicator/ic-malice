{-# OPTIONS_GHC -w #-}
module AliceParser where

import Data.Maybe
import Data.Char
import Numeric

import AliceToken
import AliceAST

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([Stmt])
	| HappyAbsSyn8 (Stmt)
	| HappyAbsSyn14 ([(Expr, [Stmt])])
	| HappyAbsSyn15 ((Expr, [Stmt]))
	| HappyAbsSyn21 ([Param])
	| HappyAbsSyn23 (Param)
	| HappyAbsSyn24 ([Expr])
	| HappyAbsSyn26 (Type)
	| HappyAbsSyn27 (Expr)
	| HappyAbsSyn31 (String)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (46) = happyShift action_14
action_2 (47) = happyReduce_1
action_2 (48) = happyShift action_15
action_2 (51) = happyShift action_16
action_2 (52) = happyReduce_1
action_2 (54) = happyReduce_1
action_2 (59) = happyShift action_17
action_2 (63) = happyShift action_18
action_2 (64) = happyReduce_1
action_2 (98) = happyReduce_1
action_2 (6) = happyGoto action_4
action_2 (7) = happyGoto action_5
action_2 (11) = happyGoto action_6
action_2 (12) = happyGoto action_7
action_2 (13) = happyGoto action_8
action_2 (14) = happyGoto action_9
action_2 (17) = happyGoto action_10
action_2 (18) = happyGoto action_11
action_2 (19) = happyGoto action_12
action_2 (20) = happyGoto action_13
action_2 _ = happyReduce_6

action_3 (98) = happyAccept
action_3 _ = happyFail

action_4 _ = happyReduce_3

action_5 (35) = happyShift action_35
action_5 (55) = happyShift action_36
action_5 (75) = happyShift action_37
action_5 (90) = happyShift action_38
action_5 (91) = happyShift action_39
action_5 (92) = happyShift action_40
action_5 (93) = happyShift action_41
action_5 (96) = happyShift action_42
action_5 (97) = happyShift action_43
action_5 (8) = happyGoto action_28
action_5 (9) = happyGoto action_29
action_5 (10) = happyGoto action_30
action_5 (27) = happyGoto action_31
action_5 (28) = happyGoto action_32
action_5 (29) = happyGoto action_33
action_5 (30) = happyGoto action_34
action_5 _ = happyFail

action_6 _ = happyReduce_4

action_7 _ = happyReduce_25

action_8 _ = happyReduce_26

action_9 (47) = happyShift action_27
action_9 (16) = happyGoto action_26
action_9 _ = happyReduce_36

action_10 _ = happyReduce_27

action_11 _ = happyReduce_28

action_12 _ = happyReduce_29

action_13 _ = happyReduce_30

action_14 (72) = happyShift action_25
action_14 _ = happyFail

action_15 (72) = happyShift action_24
action_15 (15) = happyGoto action_23
action_15 _ = happyFail

action_16 (72) = happyShift action_22
action_16 _ = happyFail

action_17 (60) = happyShift action_20
action_17 (61) = happyShift action_21
action_17 _ = happyFail

action_18 (4) = happyGoto action_19
action_18 (5) = happyGoto action_2
action_18 _ = happyReduce_2

action_19 (64) = happyShift action_90
action_19 _ = happyFail

action_20 (97) = happyShift action_89
action_20 _ = happyFail

action_21 (97) = happyShift action_88
action_21 _ = happyFail

action_22 (75) = happyShift action_37
action_22 (90) = happyShift action_38
action_22 (91) = happyShift action_39
action_22 (92) = happyShift action_40
action_22 (93) = happyShift action_41
action_22 (96) = happyShift action_42
action_22 (97) = happyShift action_51
action_22 (27) = happyGoto action_87
action_22 (28) = happyGoto action_32
action_22 (29) = happyGoto action_33
action_22 (30) = happyGoto action_34
action_22 _ = happyFail

action_23 _ = happyReduce_33

action_24 (75) = happyShift action_37
action_24 (90) = happyShift action_38
action_24 (91) = happyShift action_39
action_24 (92) = happyShift action_40
action_24 (93) = happyShift action_41
action_24 (96) = happyShift action_42
action_24 (97) = happyShift action_51
action_24 (27) = happyGoto action_86
action_24 (28) = happyGoto action_32
action_24 (29) = happyGoto action_33
action_24 (30) = happyGoto action_34
action_24 _ = happyFail

action_25 (75) = happyShift action_37
action_25 (90) = happyShift action_38
action_25 (91) = happyShift action_39
action_25 (92) = happyShift action_40
action_25 (93) = happyShift action_41
action_25 (96) = happyShift action_42
action_25 (97) = happyShift action_51
action_25 (27) = happyGoto action_85
action_25 (28) = happyGoto action_32
action_25 (29) = happyGoto action_33
action_25 (30) = happyGoto action_34
action_25 _ = happyFail

action_26 (54) = happyShift action_84
action_26 _ = happyFail

action_27 (50) = happyShift action_83
action_27 (4) = happyGoto action_82
action_27 (5) = happyGoto action_2
action_27 _ = happyReduce_2

action_28 _ = happyReduce_7

action_29 _ = happyReduce_5

action_30 (32) = happyShift action_77
action_30 (33) = happyShift action_78
action_30 (34) = happyShift action_79
action_30 (69) = happyShift action_80
action_30 (70) = happyShift action_81
action_30 _ = happyFail

action_31 (40) = happyShift action_56
action_31 (41) = happyShift action_57
action_31 (42) = happyShift action_58
action_31 (43) = happyShift action_59
action_31 (44) = happyShift action_60
action_31 (74) = happyShift action_61
action_31 (75) = happyShift action_62
action_31 (76) = happyShift action_63
action_31 (77) = happyShift action_64
action_31 (78) = happyShift action_65
action_31 (79) = happyShift action_66
action_31 (80) = happyShift action_67
action_31 (81) = happyShift action_68
action_31 (82) = happyShift action_69
action_31 (83) = happyShift action_70
action_31 (84) = happyShift action_71
action_31 (85) = happyShift action_72
action_31 (86) = happyShift action_73
action_31 (87) = happyShift action_74
action_31 (88) = happyShift action_75
action_31 (89) = happyShift action_76
action_31 _ = happyFail

action_32 _ = happyReduce_55

action_33 _ = happyReduce_56

action_34 _ = happyReduce_57

action_35 (56) = happyShift action_55
action_35 _ = happyFail

action_36 (45) = happyShift action_54
action_36 _ = happyFail

action_37 (75) = happyShift action_37
action_37 (90) = happyShift action_38
action_37 (91) = happyShift action_39
action_37 (92) = happyShift action_40
action_37 (93) = happyShift action_41
action_37 (96) = happyShift action_42
action_37 (97) = happyShift action_51
action_37 (27) = happyGoto action_53
action_37 (28) = happyGoto action_32
action_37 (29) = happyGoto action_33
action_37 (30) = happyGoto action_34
action_37 _ = happyFail

action_38 (75) = happyShift action_37
action_38 (90) = happyShift action_38
action_38 (91) = happyShift action_39
action_38 (92) = happyShift action_40
action_38 (93) = happyShift action_41
action_38 (96) = happyShift action_42
action_38 (97) = happyShift action_51
action_38 (27) = happyGoto action_52
action_38 (28) = happyGoto action_32
action_38 (29) = happyGoto action_33
action_38 (30) = happyGoto action_34
action_38 _ = happyFail

action_39 (75) = happyShift action_37
action_39 (90) = happyShift action_38
action_39 (91) = happyShift action_39
action_39 (92) = happyShift action_40
action_39 (93) = happyShift action_41
action_39 (96) = happyShift action_42
action_39 (97) = happyShift action_51
action_39 (27) = happyGoto action_50
action_39 (28) = happyGoto action_32
action_39 (29) = happyGoto action_33
action_39 (30) = happyGoto action_34
action_39 _ = happyFail

action_40 (94) = happyShift action_48
action_40 (95) = happyShift action_49
action_40 _ = happyFail

action_41 (31) = happyGoto action_47
action_41 _ = happyReduce_83

action_42 _ = happyReduce_79

action_43 (38) = happyShift action_44
action_43 (56) = happyShift action_45
action_43 (72) = happyShift action_46
action_43 _ = happyReduce_58

action_44 (75) = happyShift action_37
action_44 (90) = happyShift action_38
action_44 (91) = happyShift action_39
action_44 (92) = happyShift action_40
action_44 (93) = happyShift action_41
action_44 (96) = happyShift action_42
action_44 (97) = happyShift action_51
action_44 (27) = happyGoto action_128
action_44 (28) = happyGoto action_32
action_44 (29) = happyGoto action_33
action_44 (30) = happyGoto action_34
action_44 _ = happyFail

action_45 (36) = happyShift action_127
action_45 _ = happyFail

action_46 (75) = happyShift action_37
action_46 (90) = happyShift action_38
action_46 (91) = happyShift action_39
action_46 (92) = happyShift action_40
action_46 (93) = happyShift action_41
action_46 (96) = happyShift action_42
action_46 (97) = happyShift action_51
action_46 (24) = happyGoto action_124
action_46 (25) = happyGoto action_125
action_46 (27) = happyGoto action_126
action_46 (28) = happyGoto action_32
action_46 (29) = happyGoto action_33
action_46 (30) = happyGoto action_34
action_46 _ = happyReduce_48

action_47 (93) = happyShift action_121
action_47 (94) = happyShift action_122
action_47 (95) = happyShift action_123
action_47 _ = happyFail

action_48 (92) = happyShift action_120
action_48 _ = happyFail

action_49 (92) = happyShift action_119
action_49 _ = happyFail

action_50 (74) = happyShift action_61
action_50 (75) = happyShift action_62
action_50 (76) = happyShift action_63
action_50 (77) = happyShift action_64
action_50 (78) = happyShift action_65
action_50 (79) = happyShift action_66
action_50 (80) = happyShift action_67
action_50 (81) = happyShift action_68
action_50 (82) = happyShift action_69
action_50 (83) = happyShift action_70
action_50 (84) = happyShift action_71
action_50 (87) = happyShift action_74
action_50 (88) = happyShift action_75
action_50 (89) = happyShift action_76
action_50 _ = happyReduce_61

action_51 (72) = happyShift action_118
action_51 _ = happyReduce_58

action_52 _ = happyReduce_62

action_53 _ = happyReduce_60

action_54 (75) = happyShift action_37
action_54 (90) = happyShift action_38
action_54 (91) = happyShift action_39
action_54 (92) = happyShift action_40
action_54 (93) = happyShift action_41
action_54 (96) = happyShift action_42
action_54 (97) = happyShift action_51
action_54 (27) = happyGoto action_117
action_54 (28) = happyGoto action_32
action_54 (29) = happyGoto action_33
action_54 (30) = happyGoto action_34
action_54 _ = happyFail

action_55 (75) = happyShift action_37
action_55 (90) = happyShift action_38
action_55 (91) = happyShift action_39
action_55 (92) = happyShift action_40
action_55 (93) = happyShift action_41
action_55 (96) = happyShift action_42
action_55 (97) = happyShift action_51
action_55 (27) = happyGoto action_116
action_55 (28) = happyGoto action_32
action_55 (29) = happyGoto action_33
action_55 (30) = happyGoto action_34
action_55 _ = happyFail

action_56 (75) = happyShift action_37
action_56 (90) = happyShift action_38
action_56 (91) = happyShift action_39
action_56 (92) = happyShift action_40
action_56 (93) = happyShift action_41
action_56 (96) = happyShift action_42
action_56 (97) = happyShift action_51
action_56 (27) = happyGoto action_115
action_56 (28) = happyGoto action_32
action_56 (29) = happyGoto action_33
action_56 (30) = happyGoto action_34
action_56 _ = happyFail

action_57 _ = happyReduce_19

action_58 _ = happyReduce_20

action_59 _ = happyReduce_21

action_60 (55) = happyShift action_114
action_60 _ = happyFail

action_61 (75) = happyShift action_37
action_61 (90) = happyShift action_38
action_61 (91) = happyShift action_39
action_61 (92) = happyShift action_40
action_61 (93) = happyShift action_41
action_61 (96) = happyShift action_42
action_61 (97) = happyShift action_51
action_61 (27) = happyGoto action_113
action_61 (28) = happyGoto action_32
action_61 (29) = happyGoto action_33
action_61 (30) = happyGoto action_34
action_61 _ = happyFail

action_62 (75) = happyShift action_37
action_62 (90) = happyShift action_38
action_62 (91) = happyShift action_39
action_62 (92) = happyShift action_40
action_62 (93) = happyShift action_41
action_62 (96) = happyShift action_42
action_62 (97) = happyShift action_51
action_62 (27) = happyGoto action_112
action_62 (28) = happyGoto action_32
action_62 (29) = happyGoto action_33
action_62 (30) = happyGoto action_34
action_62 _ = happyFail

action_63 (75) = happyShift action_37
action_63 (90) = happyShift action_38
action_63 (91) = happyShift action_39
action_63 (92) = happyShift action_40
action_63 (93) = happyShift action_41
action_63 (96) = happyShift action_42
action_63 (97) = happyShift action_51
action_63 (27) = happyGoto action_111
action_63 (28) = happyGoto action_32
action_63 (29) = happyGoto action_33
action_63 (30) = happyGoto action_34
action_63 _ = happyFail

action_64 (75) = happyShift action_37
action_64 (90) = happyShift action_38
action_64 (91) = happyShift action_39
action_64 (92) = happyShift action_40
action_64 (93) = happyShift action_41
action_64 (96) = happyShift action_42
action_64 (97) = happyShift action_51
action_64 (27) = happyGoto action_110
action_64 (28) = happyGoto action_32
action_64 (29) = happyGoto action_33
action_64 (30) = happyGoto action_34
action_64 _ = happyFail

action_65 (75) = happyShift action_37
action_65 (90) = happyShift action_38
action_65 (91) = happyShift action_39
action_65 (92) = happyShift action_40
action_65 (93) = happyShift action_41
action_65 (96) = happyShift action_42
action_65 (97) = happyShift action_51
action_65 (27) = happyGoto action_109
action_65 (28) = happyGoto action_32
action_65 (29) = happyGoto action_33
action_65 (30) = happyGoto action_34
action_65 _ = happyFail

action_66 (75) = happyShift action_37
action_66 (90) = happyShift action_38
action_66 (91) = happyShift action_39
action_66 (92) = happyShift action_40
action_66 (93) = happyShift action_41
action_66 (96) = happyShift action_42
action_66 (97) = happyShift action_51
action_66 (27) = happyGoto action_108
action_66 (28) = happyGoto action_32
action_66 (29) = happyGoto action_33
action_66 (30) = happyGoto action_34
action_66 _ = happyFail

action_67 (75) = happyShift action_37
action_67 (90) = happyShift action_38
action_67 (91) = happyShift action_39
action_67 (92) = happyShift action_40
action_67 (93) = happyShift action_41
action_67 (96) = happyShift action_42
action_67 (97) = happyShift action_51
action_67 (27) = happyGoto action_107
action_67 (28) = happyGoto action_32
action_67 (29) = happyGoto action_33
action_67 (30) = happyGoto action_34
action_67 _ = happyFail

action_68 (75) = happyShift action_37
action_68 (90) = happyShift action_38
action_68 (91) = happyShift action_39
action_68 (92) = happyShift action_40
action_68 (93) = happyShift action_41
action_68 (96) = happyShift action_42
action_68 (97) = happyShift action_51
action_68 (27) = happyGoto action_106
action_68 (28) = happyGoto action_32
action_68 (29) = happyGoto action_33
action_68 (30) = happyGoto action_34
action_68 _ = happyFail

action_69 (75) = happyShift action_37
action_69 (90) = happyShift action_38
action_69 (91) = happyShift action_39
action_69 (92) = happyShift action_40
action_69 (93) = happyShift action_41
action_69 (96) = happyShift action_42
action_69 (97) = happyShift action_51
action_69 (27) = happyGoto action_105
action_69 (28) = happyGoto action_32
action_69 (29) = happyGoto action_33
action_69 (30) = happyGoto action_34
action_69 _ = happyFail

action_70 (75) = happyShift action_37
action_70 (90) = happyShift action_38
action_70 (91) = happyShift action_39
action_70 (92) = happyShift action_40
action_70 (93) = happyShift action_41
action_70 (96) = happyShift action_42
action_70 (97) = happyShift action_51
action_70 (27) = happyGoto action_104
action_70 (28) = happyGoto action_32
action_70 (29) = happyGoto action_33
action_70 (30) = happyGoto action_34
action_70 _ = happyFail

action_71 (75) = happyShift action_37
action_71 (90) = happyShift action_38
action_71 (91) = happyShift action_39
action_71 (92) = happyShift action_40
action_71 (93) = happyShift action_41
action_71 (96) = happyShift action_42
action_71 (97) = happyShift action_51
action_71 (27) = happyGoto action_103
action_71 (28) = happyGoto action_32
action_71 (29) = happyGoto action_33
action_71 (30) = happyGoto action_34
action_71 _ = happyFail

action_72 (75) = happyShift action_37
action_72 (90) = happyShift action_38
action_72 (91) = happyShift action_39
action_72 (92) = happyShift action_40
action_72 (93) = happyShift action_41
action_72 (96) = happyShift action_42
action_72 (97) = happyShift action_51
action_72 (27) = happyGoto action_102
action_72 (28) = happyGoto action_32
action_72 (29) = happyGoto action_33
action_72 (30) = happyGoto action_34
action_72 _ = happyFail

action_73 (75) = happyShift action_37
action_73 (90) = happyShift action_38
action_73 (91) = happyShift action_39
action_73 (92) = happyShift action_40
action_73 (93) = happyShift action_41
action_73 (96) = happyShift action_42
action_73 (97) = happyShift action_51
action_73 (27) = happyGoto action_101
action_73 (28) = happyGoto action_32
action_73 (29) = happyGoto action_33
action_73 (30) = happyGoto action_34
action_73 _ = happyFail

action_74 (75) = happyShift action_37
action_74 (90) = happyShift action_38
action_74 (91) = happyShift action_39
action_74 (92) = happyShift action_40
action_74 (93) = happyShift action_41
action_74 (96) = happyShift action_42
action_74 (97) = happyShift action_51
action_74 (27) = happyGoto action_100
action_74 (28) = happyGoto action_32
action_74 (29) = happyGoto action_33
action_74 (30) = happyGoto action_34
action_74 _ = happyFail

action_75 (75) = happyShift action_37
action_75 (90) = happyShift action_38
action_75 (91) = happyShift action_39
action_75 (92) = happyShift action_40
action_75 (93) = happyShift action_41
action_75 (96) = happyShift action_42
action_75 (97) = happyShift action_51
action_75 (27) = happyGoto action_99
action_75 (28) = happyGoto action_32
action_75 (29) = happyGoto action_33
action_75 (30) = happyGoto action_34
action_75 _ = happyFail

action_76 (75) = happyShift action_37
action_76 (90) = happyShift action_38
action_76 (91) = happyShift action_39
action_76 (92) = happyShift action_40
action_76 (93) = happyShift action_41
action_76 (96) = happyShift action_42
action_76 (97) = happyShift action_51
action_76 (27) = happyGoto action_98
action_76 (28) = happyGoto action_32
action_76 (29) = happyGoto action_33
action_76 (30) = happyGoto action_34
action_76 _ = happyFail

action_77 _ = happyReduce_9

action_78 _ = happyReduce_10

action_79 _ = happyReduce_11

action_80 _ = happyReduce_8

action_81 _ = happyReduce_12

action_82 _ = happyReduce_37

action_83 (72) = happyShift action_24
action_83 (15) = happyGoto action_97
action_83 _ = happyFail

action_84 (55) = happyShift action_96
action_84 _ = happyFail

action_85 (73) = happyShift action_95
action_85 (74) = happyShift action_61
action_85 (75) = happyShift action_62
action_85 (76) = happyShift action_63
action_85 (77) = happyShift action_64
action_85 (78) = happyShift action_65
action_85 (79) = happyShift action_66
action_85 (80) = happyShift action_67
action_85 (81) = happyShift action_68
action_85 (82) = happyShift action_69
action_85 (83) = happyShift action_70
action_85 (84) = happyShift action_71
action_85 (85) = happyShift action_72
action_85 (86) = happyShift action_73
action_85 (87) = happyShift action_74
action_85 (88) = happyShift action_75
action_85 (89) = happyShift action_76
action_85 _ = happyFail

action_86 (73) = happyShift action_94
action_86 (74) = happyShift action_61
action_86 (75) = happyShift action_62
action_86 (76) = happyShift action_63
action_86 (77) = happyShift action_64
action_86 (78) = happyShift action_65
action_86 (79) = happyShift action_66
action_86 (80) = happyShift action_67
action_86 (81) = happyShift action_68
action_86 (82) = happyShift action_69
action_86 (83) = happyShift action_70
action_86 (84) = happyShift action_71
action_86 (85) = happyShift action_72
action_86 (86) = happyShift action_73
action_86 (87) = happyShift action_74
action_86 (88) = happyShift action_75
action_86 (89) = happyShift action_76
action_86 _ = happyFail

action_87 (73) = happyShift action_93
action_87 (74) = happyShift action_61
action_87 (75) = happyShift action_62
action_87 (76) = happyShift action_63
action_87 (77) = happyShift action_64
action_87 (78) = happyShift action_65
action_87 (79) = happyShift action_66
action_87 (80) = happyShift action_67
action_87 (81) = happyShift action_68
action_87 (82) = happyShift action_69
action_87 (83) = happyShift action_70
action_87 (84) = happyShift action_71
action_87 (85) = happyShift action_72
action_87 (86) = happyShift action_73
action_87 (87) = happyShift action_74
action_87 (88) = happyShift action_75
action_87 (89) = happyShift action_76
action_87 _ = happyFail

action_88 (72) = happyShift action_92
action_88 _ = happyFail

action_89 (72) = happyShift action_91
action_89 _ = happyFail

action_90 _ = happyReduce_41

action_91 (65) = happyShift action_146
action_91 (66) = happyShift action_130
action_91 (67) = happyShift action_131
action_91 (68) = happyShift action_132
action_91 (21) = happyGoto action_147
action_91 (22) = happyGoto action_143
action_91 (23) = happyGoto action_144
action_91 (26) = happyGoto action_145
action_91 _ = happyReduce_42

action_92 (65) = happyShift action_146
action_92 (66) = happyShift action_130
action_92 (67) = happyShift action_131
action_92 (68) = happyShift action_132
action_92 (21) = happyGoto action_142
action_92 (22) = happyGoto action_143
action_92 (23) = happyGoto action_144
action_92 (26) = happyGoto action_145
action_92 _ = happyReduce_42

action_93 (54) = happyShift action_141
action_93 _ = happyFail

action_94 (49) = happyShift action_140
action_94 _ = happyFail

action_95 (49) = happyShift action_139
action_95 _ = happyFail

action_96 (56) = happyShift action_138
action_96 _ = happyFail

action_97 _ = happyReduce_34

action_98 (87) = happyShift action_74
action_98 _ = happyReduce_78

action_99 (87) = happyShift action_74
action_99 _ = happyReduce_77

action_100 _ = happyReduce_76

action_101 (74) = happyShift action_61
action_101 (75) = happyShift action_62
action_101 (76) = happyShift action_63
action_101 (77) = happyShift action_64
action_101 (78) = happyShift action_65
action_101 (79) = happyShift action_66
action_101 (80) = happyShift action_67
action_101 (81) = happyShift action_68
action_101 (82) = happyShift action_69
action_101 (83) = happyShift action_70
action_101 (84) = happyShift action_71
action_101 (85) = happyShift action_72
action_101 (87) = happyShift action_74
action_101 (88) = happyShift action_75
action_101 (89) = happyShift action_76
action_101 _ = happyReduce_75

action_102 (74) = happyShift action_61
action_102 (75) = happyShift action_62
action_102 (76) = happyShift action_63
action_102 (77) = happyShift action_64
action_102 (78) = happyShift action_65
action_102 (79) = happyShift action_66
action_102 (80) = happyShift action_67
action_102 (81) = happyShift action_68
action_102 (82) = happyShift action_69
action_102 (83) = happyShift action_70
action_102 (84) = happyShift action_71
action_102 (87) = happyShift action_74
action_102 (88) = happyShift action_75
action_102 (89) = happyShift action_76
action_102 _ = happyReduce_74

action_103 (74) = happyShift action_61
action_103 (75) = happyShift action_62
action_103 (76) = happyShift action_63
action_103 (77) = happyShift action_64
action_103 (78) = happyShift action_65
action_103 (79) = happyFail
action_103 (80) = happyFail
action_103 (81) = happyFail
action_103 (82) = happyFail
action_103 (83) = happyFail
action_103 (84) = happyFail
action_103 (87) = happyShift action_74
action_103 (88) = happyShift action_75
action_103 (89) = happyShift action_76
action_103 _ = happyReduce_73

action_104 (74) = happyShift action_61
action_104 (75) = happyShift action_62
action_104 (76) = happyShift action_63
action_104 (77) = happyShift action_64
action_104 (78) = happyShift action_65
action_104 (79) = happyFail
action_104 (80) = happyFail
action_104 (81) = happyFail
action_104 (82) = happyFail
action_104 (83) = happyFail
action_104 (84) = happyFail
action_104 (87) = happyShift action_74
action_104 (88) = happyShift action_75
action_104 (89) = happyShift action_76
action_104 _ = happyReduce_72

action_105 (74) = happyShift action_61
action_105 (75) = happyShift action_62
action_105 (76) = happyShift action_63
action_105 (77) = happyShift action_64
action_105 (78) = happyShift action_65
action_105 (79) = happyFail
action_105 (80) = happyFail
action_105 (81) = happyFail
action_105 (82) = happyFail
action_105 (83) = happyFail
action_105 (84) = happyFail
action_105 (87) = happyShift action_74
action_105 (88) = happyShift action_75
action_105 (89) = happyShift action_76
action_105 _ = happyReduce_71

action_106 (74) = happyShift action_61
action_106 (75) = happyShift action_62
action_106 (76) = happyShift action_63
action_106 (77) = happyShift action_64
action_106 (78) = happyShift action_65
action_106 (79) = happyFail
action_106 (80) = happyFail
action_106 (81) = happyFail
action_106 (82) = happyFail
action_106 (83) = happyFail
action_106 (84) = happyFail
action_106 (87) = happyShift action_74
action_106 (88) = happyShift action_75
action_106 (89) = happyShift action_76
action_106 _ = happyReduce_70

action_107 (74) = happyShift action_61
action_107 (75) = happyShift action_62
action_107 (76) = happyShift action_63
action_107 (77) = happyShift action_64
action_107 (78) = happyShift action_65
action_107 (79) = happyFail
action_107 (80) = happyFail
action_107 (81) = happyFail
action_107 (82) = happyFail
action_107 (83) = happyFail
action_107 (84) = happyFail
action_107 (87) = happyShift action_74
action_107 (88) = happyShift action_75
action_107 (89) = happyShift action_76
action_107 _ = happyReduce_69

action_108 (74) = happyShift action_61
action_108 (75) = happyShift action_62
action_108 (76) = happyShift action_63
action_108 (77) = happyShift action_64
action_108 (78) = happyShift action_65
action_108 (79) = happyFail
action_108 (80) = happyFail
action_108 (81) = happyFail
action_108 (82) = happyFail
action_108 (83) = happyFail
action_108 (84) = happyFail
action_108 (87) = happyShift action_74
action_108 (88) = happyShift action_75
action_108 (89) = happyShift action_76
action_108 _ = happyReduce_68

action_109 (87) = happyShift action_74
action_109 (88) = happyShift action_75
action_109 (89) = happyShift action_76
action_109 _ = happyReduce_67

action_110 (87) = happyShift action_74
action_110 (88) = happyShift action_75
action_110 (89) = happyShift action_76
action_110 _ = happyReduce_66

action_111 (87) = happyShift action_74
action_111 (88) = happyShift action_75
action_111 (89) = happyShift action_76
action_111 _ = happyReduce_65

action_112 (76) = happyShift action_63
action_112 (77) = happyShift action_64
action_112 (78) = happyShift action_65
action_112 (87) = happyShift action_74
action_112 (88) = happyShift action_75
action_112 (89) = happyShift action_76
action_112 _ = happyReduce_64

action_113 (76) = happyShift action_63
action_113 (77) = happyShift action_64
action_113 (78) = happyShift action_65
action_113 (87) = happyShift action_74
action_113 (88) = happyShift action_75
action_113 (89) = happyShift action_76
action_113 _ = happyReduce_63

action_114 _ = happyReduce_22

action_115 (74) = happyShift action_61
action_115 (75) = happyShift action_62
action_115 (76) = happyShift action_63
action_115 (77) = happyShift action_64
action_115 (78) = happyShift action_65
action_115 (79) = happyShift action_66
action_115 (80) = happyShift action_67
action_115 (81) = happyShift action_68
action_115 (82) = happyShift action_69
action_115 (83) = happyShift action_70
action_115 (84) = happyShift action_71
action_115 (85) = happyShift action_72
action_115 (86) = happyShift action_73
action_115 (87) = happyShift action_74
action_115 (88) = happyShift action_75
action_115 (89) = happyShift action_76
action_115 _ = happyReduce_18

action_116 (71) = happyShift action_137
action_116 (74) = happyShift action_61
action_116 (75) = happyShift action_62
action_116 (76) = happyShift action_63
action_116 (77) = happyShift action_64
action_116 (78) = happyShift action_65
action_116 (79) = happyShift action_66
action_116 (80) = happyShift action_67
action_116 (81) = happyShift action_68
action_116 (82) = happyShift action_69
action_116 (83) = happyShift action_70
action_116 (84) = happyShift action_71
action_116 (85) = happyShift action_72
action_116 (86) = happyShift action_73
action_116 (87) = happyShift action_74
action_116 (88) = happyShift action_75
action_116 (89) = happyShift action_76
action_116 _ = happyFail

action_117 (74) = happyShift action_61
action_117 (75) = happyShift action_62
action_117 (76) = happyShift action_63
action_117 (77) = happyShift action_64
action_117 (78) = happyShift action_65
action_117 (79) = happyShift action_66
action_117 (80) = happyShift action_67
action_117 (81) = happyShift action_68
action_117 (82) = happyShift action_69
action_117 (83) = happyShift action_70
action_117 (84) = happyShift action_71
action_117 (85) = happyShift action_72
action_117 (86) = happyShift action_73
action_117 (87) = happyShift action_74
action_117 (88) = happyShift action_75
action_117 (89) = happyShift action_76
action_117 _ = happyReduce_23

action_118 (75) = happyShift action_37
action_118 (90) = happyShift action_38
action_118 (91) = happyShift action_39
action_118 (92) = happyShift action_40
action_118 (93) = happyShift action_41
action_118 (96) = happyShift action_42
action_118 (97) = happyShift action_51
action_118 (24) = happyGoto action_136
action_118 (25) = happyGoto action_125
action_118 (27) = happyGoto action_126
action_118 (28) = happyGoto action_32
action_118 (29) = happyGoto action_33
action_118 (30) = happyGoto action_34
action_118 _ = happyReduce_48

action_119 _ = happyReduce_81

action_120 _ = happyReduce_80

action_121 _ = happyReduce_82

action_122 _ = happyReduce_84

action_123 _ = happyReduce_85

action_124 (73) = happyShift action_135
action_124 _ = happyFail

action_125 (69) = happyShift action_134
action_125 _ = happyReduce_49

action_126 (74) = happyShift action_61
action_126 (75) = happyShift action_62
action_126 (76) = happyShift action_63
action_126 (77) = happyShift action_64
action_126 (78) = happyShift action_65
action_126 (79) = happyShift action_66
action_126 (80) = happyShift action_67
action_126 (81) = happyShift action_68
action_126 (82) = happyShift action_69
action_126 (83) = happyShift action_70
action_126 (84) = happyShift action_71
action_126 (85) = happyShift action_72
action_126 (86) = happyShift action_73
action_126 (87) = happyShift action_74
action_126 (88) = happyShift action_75
action_126 (89) = happyShift action_76
action_126 _ = happyReduce_50

action_127 (66) = happyShift action_130
action_127 (67) = happyShift action_131
action_127 (68) = happyShift action_132
action_127 (26) = happyGoto action_133
action_127 _ = happyFail

action_128 (66) = happyShift action_130
action_128 (67) = happyShift action_131
action_128 (68) = happyShift action_132
action_128 (74) = happyShift action_61
action_128 (75) = happyShift action_62
action_128 (76) = happyShift action_63
action_128 (77) = happyShift action_64
action_128 (78) = happyShift action_65
action_128 (79) = happyShift action_66
action_128 (80) = happyShift action_67
action_128 (81) = happyShift action_68
action_128 (82) = happyShift action_69
action_128 (83) = happyShift action_70
action_128 (84) = happyShift action_71
action_128 (85) = happyShift action_72
action_128 (86) = happyShift action_73
action_128 (87) = happyShift action_74
action_128 (88) = happyShift action_75
action_128 (89) = happyShift action_76
action_128 (26) = happyGoto action_129
action_128 _ = happyFail

action_129 _ = happyReduce_17

action_130 _ = happyReduce_52

action_131 _ = happyReduce_53

action_132 _ = happyReduce_54

action_133 (37) = happyShift action_159
action_133 (39) = happyShift action_160
action_133 _ = happyReduce_14

action_134 (75) = happyShift action_37
action_134 (90) = happyShift action_38
action_134 (91) = happyShift action_39
action_134 (92) = happyShift action_40
action_134 (93) = happyShift action_41
action_134 (96) = happyShift action_42
action_134 (97) = happyShift action_51
action_134 (27) = happyGoto action_158
action_134 (28) = happyGoto action_32
action_134 (29) = happyGoto action_33
action_134 (30) = happyGoto action_34
action_134 _ = happyFail

action_135 (40) = happyReduce_59
action_135 (41) = happyReduce_59
action_135 (42) = happyReduce_59
action_135 (43) = happyReduce_59
action_135 (44) = happyReduce_59
action_135 (74) = happyReduce_59
action_135 (75) = happyReduce_59
action_135 (76) = happyReduce_59
action_135 (77) = happyReduce_59
action_135 (78) = happyReduce_59
action_135 (79) = happyReduce_59
action_135 (80) = happyReduce_59
action_135 (81) = happyReduce_59
action_135 (82) = happyReduce_59
action_135 (83) = happyReduce_59
action_135 (84) = happyReduce_59
action_135 (85) = happyReduce_59
action_135 (86) = happyReduce_59
action_135 (87) = happyReduce_59
action_135 (88) = happyReduce_59
action_135 (89) = happyReduce_59
action_135 _ = happyReduce_24

action_136 (73) = happyShift action_157
action_136 _ = happyFail

action_137 _ = happyReduce_13

action_138 (57) = happyShift action_156
action_138 _ = happyFail

action_139 (4) = happyGoto action_155
action_139 (5) = happyGoto action_2
action_139 _ = happyReduce_2

action_140 (4) = happyGoto action_154
action_140 (5) = happyGoto action_2
action_140 _ = happyReduce_2

action_141 (4) = happyGoto action_153
action_141 (5) = happyGoto action_2
action_141 _ = happyReduce_2

action_142 (73) = happyShift action_152
action_142 _ = happyFail

action_143 (69) = happyShift action_151
action_143 _ = happyReduce_43

action_144 _ = happyReduce_44

action_145 (97) = happyShift action_150
action_145 _ = happyFail

action_146 (66) = happyShift action_130
action_146 (67) = happyShift action_131
action_146 (68) = happyShift action_132
action_146 (26) = happyGoto action_149
action_146 _ = happyFail

action_147 (73) = happyShift action_148
action_147 _ = happyFail

action_148 (62) = happyShift action_168
action_148 _ = happyFail

action_149 (97) = happyShift action_167
action_149 _ = happyFail

action_150 _ = happyReduce_46

action_151 (65) = happyShift action_146
action_151 (66) = happyShift action_130
action_151 (67) = happyShift action_131
action_151 (68) = happyShift action_132
action_151 (23) = happyGoto action_166
action_151 (26) = happyGoto action_145
action_151 _ = happyFail

action_152 (63) = happyShift action_165
action_152 _ = happyFail

action_153 (52) = happyShift action_164
action_153 _ = happyFail

action_154 _ = happyReduce_35

action_155 (47) = happyShift action_163
action_155 _ = happyFail

action_156 (58) = happyShift action_162
action_156 _ = happyFail

action_157 _ = happyReduce_59

action_158 (74) = happyShift action_61
action_158 (75) = happyShift action_62
action_158 (76) = happyShift action_63
action_158 (77) = happyShift action_64
action_158 (78) = happyShift action_65
action_158 (79) = happyShift action_66
action_158 (80) = happyShift action_67
action_158 (81) = happyShift action_68
action_158 (82) = happyShift action_69
action_158 (83) = happyShift action_70
action_158 (84) = happyShift action_71
action_158 (85) = happyShift action_72
action_158 (86) = happyShift action_73
action_158 (87) = happyShift action_74
action_158 (88) = happyShift action_75
action_158 (89) = happyShift action_76
action_158 _ = happyReduce_51

action_159 _ = happyReduce_15

action_160 (75) = happyShift action_37
action_160 (90) = happyShift action_38
action_160 (91) = happyShift action_39
action_160 (92) = happyShift action_40
action_160 (93) = happyShift action_41
action_160 (96) = happyShift action_42
action_160 (97) = happyShift action_51
action_160 (27) = happyGoto action_161
action_160 (28) = happyGoto action_32
action_160 (29) = happyGoto action_33
action_160 (30) = happyGoto action_34
action_160 _ = happyFail

action_161 (74) = happyShift action_61
action_161 (75) = happyShift action_62
action_161 (76) = happyShift action_63
action_161 (77) = happyShift action_64
action_161 (78) = happyShift action_65
action_161 (79) = happyShift action_66
action_161 (80) = happyShift action_67
action_161 (81) = happyShift action_68
action_161 (82) = happyShift action_69
action_161 (83) = happyShift action_70
action_161 (84) = happyShift action_71
action_161 (85) = happyShift action_72
action_161 (86) = happyShift action_73
action_161 (87) = happyShift action_74
action_161 (88) = happyShift action_75
action_161 (89) = happyShift action_76
action_161 _ = happyReduce_16

action_162 _ = happyReduce_32

action_163 (4) = happyGoto action_172
action_163 (5) = happyGoto action_2
action_163 _ = happyReduce_2

action_164 (53) = happyShift action_171
action_164 _ = happyFail

action_165 (4) = happyGoto action_170
action_165 (5) = happyGoto action_2
action_165 _ = happyReduce_2

action_166 _ = happyReduce_45

action_167 _ = happyReduce_47

action_168 (36) = happyShift action_169
action_168 _ = happyFail

action_169 (66) = happyShift action_130
action_169 (67) = happyShift action_131
action_169 (68) = happyShift action_132
action_169 (26) = happyGoto action_175
action_169 _ = happyFail

action_170 (64) = happyShift action_174
action_170 _ = happyFail

action_171 _ = happyReduce_38

action_172 (54) = happyShift action_173
action_172 _ = happyFail

action_173 (55) = happyShift action_177
action_173 _ = happyFail

action_174 _ = happyReduce_40

action_175 (63) = happyShift action_176
action_175 _ = happyFail

action_176 (4) = happyGoto action_179
action_176 (5) = happyGoto action_2
action_176 _ = happyReduce_2

action_177 (56) = happyShift action_178
action_177 _ = happyFail

action_178 (57) = happyShift action_181
action_178 _ = happyFail

action_179 (64) = happyShift action_180
action_179 _ = happyFail

action_180 _ = happyReduce_39

action_181 (58) = happyShift action_182
action_181 _ = happyFail

action_182 _ = happyReduce_31

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (reverse happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn4
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 ++ happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  7 happyReduction_6
happyReduction_6  =  HappyAbsSyn4
		 ([]
	)

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  8 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  8 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  9 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 4 9 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (SRead{ sTarget=happy_var_3 }
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 4 10 happyReduction_14
happyReduction_14 ((HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (SVarDec{ sID=happy_var_1, sType=happy_var_4 }
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 5 10 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (SVarDec{ sID=happy_var_1, sType=happy_var_4 }
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 10 happyReduction_16
happyReduction_16 ((HappyAbsSyn27  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (SVarIni{ sID=happy_var_1, sType=happy_var_4, sSource=happy_var_6 }
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 4 10 happyReduction_17
happyReduction_17 ((HappyAbsSyn26  happy_var_4) `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (SArrDec{ sID=happy_var_1, sType=happy_var_4, sSize=happy_var_3 }
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_3  10 happyReduction_18
happyReduction_18 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn8
		 (SAssign{ sTarget=happy_var_1, sSource=happy_var_3 }
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  10 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn8
		 (SInc{ sTarget=happy_var_1 }
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  10 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn8
		 (SDec{ sTarget=happy_var_1 }
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  10 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn8
		 (SWrite{ sSource=happy_var_1 }
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  10 happyReduction_22
happyReduction_22 _
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn8
		 (SWrite{ sSource=happy_var_1 }
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  10 happyReduction_23
happyReduction_23 (HappyAbsSyn27  happy_var_3)
	_
	_
	 =  HappyAbsSyn8
		 (SReturn{ sSource=happy_var_3 }
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 4 10 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (SCall{ sID=happy_var_1, sCParams=happy_var_3 }
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1  11 happyReduction_25
happyReduction_25 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  11 happyReduction_26
happyReduction_26 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  11 happyReduction_27
happyReduction_27 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  11 happyReduction_28
happyReduction_28 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  11 happyReduction_29
happyReduction_29 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  11 happyReduction_30
happyReduction_30 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happyReduce 13 12 happyReduction_31
happyReduction_31 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (SBranch{ sBranches=[(happy_var_3, happy_var_6)], sDefault=happy_var_8 }
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 7 13 happyReduction_32
happyReduction_32 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (SBranch{ sBranches=happy_var_1, sDefault=happy_var_2 }
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_2  14 happyReduction_33
happyReduction_33 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn14
		 ([happy_var_2]
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happyReduce 4 14 happyReduction_34
happyReduction_34 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (happy_var_4 : happy_var_1
	) `HappyStk` happyRest

happyReduce_35 = happyReduce 5 15 happyReduction_35
happyReduction_35 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 ((happy_var_2, happy_var_5)
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_0  16 happyReduction_36
happyReduction_36  =  HappyAbsSyn4
		 ([]
	)

happyReduce_37 = happySpecReduce_2  16 happyReduction_37
happyReduction_37 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 8 17 happyReduction_38
happyReduction_38 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (SWhile{ sGuard=happy_var_3, sBody=happy_var_6 }
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 12 18 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (SFunDec{ sID=happy_var_3, sDParams=happy_var_5, sType=happy_var_9, sBody=happy_var_11 }
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 9 19 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (SSubDec{ sID=happy_var_3, sDParams=happy_var_5, sBody=happy_var_8 }
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_3  20 happyReduction_41
happyReduction_41 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (SBlock happy_var_2
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_0  21 happyReduction_42
happyReduction_42  =  HappyAbsSyn21
		 ([]
	)

happyReduce_43 = happySpecReduce_1  21 happyReduction_43
happyReduction_43 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (reverse happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  22 happyReduction_44
happyReduction_44 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  22 happyReduction_45
happyReduction_45 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_3 : happy_var_1
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  23 happyReduction_46
happyReduction_46 (HappyTerminal (TIdentifier happy_var_2))
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn23
		 ((happy_var_2, happy_var_1)
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  23 happyReduction_47
happyReduction_47 (HappyTerminal (TIdentifier happy_var_3))
	(HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn23
		 ((happy_var_3, TyArray happy_var_2)
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_0  24 happyReduction_48
happyReduction_48  =  HappyAbsSyn24
		 ([]
	)

happyReduce_49 = happySpecReduce_1  24 happyReduction_49
happyReduction_49 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (reverse happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  25 happyReduction_50
happyReduction_50 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  25 happyReduction_51
happyReduction_51 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_3 : happy_var_1
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  26 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn26
		 (TyNumber
	)

happyReduce_53 = happySpecReduce_1  26 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn26
		 (TyLetter
	)

happyReduce_54 = happySpecReduce_1  26 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn26
		 (TySentence
	)

happyReduce_55 = happySpecReduce_1  27 happyReduction_55
happyReduction_55 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  27 happyReduction_56
happyReduction_56 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  27 happyReduction_57
happyReduction_57 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  27 happyReduction_58
happyReduction_58 (HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn27
		 (EVariable happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happyReduce 4 27 happyReduction_59
happyReduction_59 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (EFunCall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_60 = happySpecReduce_2  27 happyReduction_60
happyReduction_60 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (EUnary UNeg happy_var_2
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_2  27 happyReduction_61
happyReduction_61 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (EUnary UNot happy_var_2
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  27 happyReduction_62
happyReduction_62 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (EUnary UNotB happy_var_2
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  27 happyReduction_63
happyReduction_63 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BAdd happy_var_1 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  27 happyReduction_64
happyReduction_64 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BSub happy_var_1 happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  27 happyReduction_65
happyReduction_65 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BMul happy_var_1 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  27 happyReduction_66
happyReduction_66 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BDiv happy_var_1 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  27 happyReduction_67
happyReduction_67 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BRem happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  27 happyReduction_68
happyReduction_68 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BEqu happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  27 happyReduction_69
happyReduction_69 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BNEq happy_var_1 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  27 happyReduction_70
happyReduction_70 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BLT happy_var_1 happy_var_3
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  27 happyReduction_71
happyReduction_71 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BGT happy_var_1 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  27 happyReduction_72
happyReduction_72 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BLE happy_var_1 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  27 happyReduction_73
happyReduction_73 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BGE happy_var_1 happy_var_3
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  27 happyReduction_74
happyReduction_74 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BAnd happy_var_1 happy_var_3
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  27 happyReduction_75
happyReduction_75 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BOr happy_var_1 happy_var_3
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_3  27 happyReduction_76
happyReduction_76 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BAndB happy_var_1 happy_var_3
	)
happyReduction_76 _ _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  27 happyReduction_77
happyReduction_77 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BOrB happy_var_1 happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  27 happyReduction_78
happyReduction_78 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (EBinary BXOrB happy_var_1 happy_var_3
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  28 happyReduction_79
happyReduction_79 (HappyTerminal (TNumberLiteral happy_var_1))
	 =  HappyAbsSyn27
		 (ENumber happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  29 happyReduction_80
happyReduction_80 _
	(HappyTerminal (TQChar happy_var_2))
	_
	 =  HappyAbsSyn27
		 (ELetter happy_var_2
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  29 happyReduction_81
happyReduction_81 _
	(HappyTerminal (TQEscape happy_var_2))
	_
	 =  HappyAbsSyn27
		 (ELetter (unescape happy_var_2)
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  30 happyReduction_82
happyReduction_82 _
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (ESentence (reverse happy_var_2)
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_0  31 happyReduction_83
happyReduction_83  =  HappyAbsSyn31
		 (""
	)

happyReduce_84 = happySpecReduce_2  31 happyReduction_84
happyReduction_84 (HappyTerminal (TQChar happy_var_2))
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_2 : happy_var_1
	)
happyReduction_84 _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_2  31 happyReduction_85
happyReduction_85 (HappyTerminal (TQEscape happy_var_2))
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 ((unescape happy_var_2) : happy_var_1
	)
happyReduction_85 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 98 98 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TAnd -> cont 32;
	TBut -> cont 33;
	TThen -> cont 34;
	TWhat -> cont 35;
	TA -> cont 36;
	TToo -> cont 37;
	THad -> cont 38;
	TOf -> cont 39;
	TBecame -> cont 40;
	TAte -> cont 41;
	TDrank -> cont 42;
	TSpoke -> cont 43;
	TSaid -> cont 44;
	TFound -> cont 45;
	TEither -> cont 46;
	TOr -> cont 47;
	TPerhaps -> cont 48;
	TSo -> cont 49;
	TMaybe -> cont 50;
	TEventually -> cont 51;
	TEnough -> cont 52;
	TTimes -> cont 53;
	TBecause -> cont 54;
	TAlice -> cont 55;
	TWas -> cont 56;
	TUnsure -> cont 57;
	TWhich -> cont 58;
	TThe -> cont 59;
	TRoom -> cont 60;
	TLookingGlass -> cont 61;
	TContained -> cont 62;
	TOpened -> cont 63;
	TClosed -> cont 64;
	TSpider -> cont 65;
	TNumber -> cont 66;
	TLetter -> cont 67;
	TSentence -> cont 68;
	TComma -> cont 69;
	TDot -> cont 70;
	TQuest -> cont 71;
	TParenO -> cont 72;
	TParenC -> cont 73;
	TPlus -> cont 74;
	TMinus -> cont 75;
	TStar -> cont 76;
	TSlash -> cont 77;
	TPercent -> cont 78;
	TEqEq -> cont 79;
	TBangEq -> cont 80;
	TLess -> cont 81;
	TGreater -> cont 82;
	TLessEq -> cont 83;
	TGreaterEq -> cont 84;
	TAmpAmp -> cont 85;
	TBarBar -> cont 86;
	TAmp -> cont 87;
	TBar -> cont 88;
	TCaret -> cont 89;
	TTilde -> cont 90;
	TBang -> cont 91;
	TQDouble -> cont 92;
	TQSingle -> cont 93;
	TQChar happy_dollar_dollar -> cont 94;
	TQEscape happy_dollar_dollar -> cont 95;
	TNumberLiteral happy_dollar_dollar -> cont 96;
	TIdentifier happy_dollar_dollar -> cont 97;
	_ -> happyError' (tk:tks)
	}

happyError_ 98 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseAlice tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


unescape :: String -> Char
unescape ['\\', c]
  = fromJust $ lookup c $ zip "\"'\\abfnrtv" "\"\\'\a\b\f\n\r\t\v"
unescape ('\\' : 'x' : cs)
  = chr n where [(n, "")] = readHex cs

parseError :: [Token] -> a
parseError []
    = error "syntax error at end of input"
parseError (t : _)
    = error $ "syntax error at token " ++ show t
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
