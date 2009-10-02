C-----------------------------------------------------------------------
C  Include file for PARSER.  This file must be kept with PARSER.FOR.
C  It defines some symbolic constants that PARSER and its subsidiary
C  routines use.
C.......................................................................
C Define Token types and values
C
      INTEGER    NT_BEG   , NT_STOP  , NT_EOS   ,
     X           NT_ADDOP , NT_MULOP , NT_EXPOP ,
     X           NT_OPEN  , NT_CLOSE , NT_COMMA ,
     X           NT_NUMBER, NT_FUNC  , NT_ERROR , NT_SYMBOL
C
      REAL*8     VT_PLUS  , VT_MINUS , VT_SLASH ,
     X           VT_STAR  , VT_STARS
C
      PARAMETER( NT_BEG   = 1000 , NT_STOP  = 1999 , NT_EOS   = 1000 ,
     X           NT_ADDOP = 1001 , NT_MULOP = 1002 , NT_EXPOP = 1003 ,
     X           NT_OPEN  = 1004 , NT_CLOSE = 1005 , NT_COMMA = 1006 ,
     X           NT_NUMBER= 1007 , NT_FUNC  = 1008 , NT_ERROR = 1999 ,
     X           NT_SYMBOL= 1009 ,
     X
     X           VT_PLUS  = 1.D0 , VT_MINUS = 2.D0 , VT_SLASH = 2.D0 ,
     X           VT_STAR  = 1.D0 , VT_STARS = 1.D0                    )
C.......................................................................
C  Define the Nonterminals
C
      INTEGER     NN_BEG   , NN_STOP  , NN_END   ,
     X            NN_E4    , NN_E6    , NN_E8    ,
     X            NN_E9    , NN_ARGTL
C
      PARAMETER ( NN_BEG   = 2000 , NN_STOP  = 2999 , NN_END   = 2000 ,
     X            NN_E4    = 2001 , NN_E6    = 2002 , NN_E8    = 2003 ,
     X            NN_E9    = 2004 , NN_ARGTL = 2005                    )
C.......................................................................
C  Define the Opcodes
C
      INTEGER     NC_BEG   , NC_STOP  , NC_ADD   ,
     X            NC_SUB   , NC_MUL   , NC_DIV   ,
     X            NC_POWER , NC_MINUS , NC_FUNC
C
      PARAMETER ( NC_BEG   = 3000 , NC_STOP  = 4999 , NC_ADD   = 3001 ,
     X            NC_SUB   = 3002 , NC_MUL   = 3003 , NC_DIV   = 3004 ,
     X            NC_POWER = 3005 , NC_MINUS = 3006 , NC_FUNC  = 4000  )
C.......................................................................
C  Define Function names, etc.
C
      INTEGER     NUM_FUNC
      PARAMETER ( NUM_FUNC = 111 )
C
      CHARACTER*32 C_FUNCNAME(NUM_FUNC+1)
      INTEGER      N_FUNCARGS(NUM_FUNC)
C
      DATA C_FUNCNAME /
     X 'SIN'  , 'COS'  , 'TAN'  , 'ASIN'  , 'ACOS'  , 'ATAN'  , 'ATAN2',
     X 'SINH' , 'COSH' , 'TANH' , 'ASINH' , 'ACOSH' , 'ATANH' , 'EXP'  ,
     X 'LOG'  , 'LOG10', 'ABS'  , 'INT'   , 'SQRT'  , 'MAX'   , 'MIN'  ,
     X 'AI'   , 'DAI'  , 'I0'   , 'I1'    , 'J0'    , 'J1'    , 'K0'   ,
     X 'K1'   , 'Y0'   , 'Y1'   , 'BI'    , 'DBI'   , 'ERF'   , 'ERFC' ,
     X 'GAMMA', 'QG'   , 'QGINV', 'BELL2' , 'RECT'  , 'STEP'  , 'BOOL' ,
     X 'AND'  , 'OR'   , 'MOFN' , 'ASTEP' , 'SIND'  , 'COSD'  , 'TAND' ,
     X 'MEDIAN',
     X 'FICO_T2P' , 'FICO_P2T' , 'FICO_T2Z' ,
     X 'FITT_T2P' , 'FITT_P2T' , 'FITT_T2Z' ,
     X 'FIFT_T2P' , 'FIFT_P2T' , 'FIFT_T2Z' ,
     X 'FIZT_T2P' , 'FIZT_P2T' , 'FIZT_T2Z' ,
     X 'FICT_T2P' , 'FICT_P2T' , 'FICT_T2Z' ,
     X 'FIBT_T2P' , 'FIBT_P2T' , 'FIBT_T2Z' ,
     X 'FIBN_T2P' , 'FIBN_P2T' , 'FIBN_T2Z' ,
     X 'FIGT_T2P' , 'FIGT_P2T' , 'FIGT_T2Z' ,
     X 'FIPT_T2P' , 'FIPT_P2T' , 'FIPT_T2Z' ,
     X
     X 'ZTONE' , 'LMODE' , 'HMODE' ,
     X 'GRAN'  , 'URAN'  , 'IRAN'  , 'ERAN' , 'LRAN' ,
     X
     X 'ORSTAT', 'TENT'  , 'MAD'   ,
     X 'ARGMAX', 'ARGNUM',
     X
     X 'NOTZERO' , 'ISZERO' , 'EQUALS' , 'ISPOSITIVE' , 'ISNEGATIVE' ,
     X
     X 'MEAN' , 'STDEV' , 'SEM' , 'PLEG' ,
     X
     X 'CDF2STAT' , 'STAT2CDF' ,
     X
     X 'PAIRMAX' , 'PAIRMIN' , 'AMONGST' , 'CBRT' , 'RHDDC2' ,
     X
     X 'HRFBK4'  , 'HRFBK5' , 'POSVAL' , 'NOT' , 'MOD' ,
     X
     X 'DUMMY' /
C
      DATA N_FUNCARGS / 6*1, 2, 12*1, 2*2, 21*1, 3*-1, 2, 3*1, -1,
     X                 3*4, 3*2, 3*3, 3*1, 3*2, 3*3, 3*3, 3*3, 3*2 ,
     X   1 , -1,-1 , 2 , 1 , 1 , 1 , 1 , -1 ,1 , -1  ,
     X  -1,-1, 1,1,2,1,1, -1,-1,-1, 2, 5,5, -1,-1,-1,1,3, 2,2,1,1,2 /
C-----------------------------------------------------------------------
