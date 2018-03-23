#ifndef _TSTAT_NAMES_
#define _TSTAT_NAMES_
static char *meth_names[] = {
   "Mean"          , "Slope"        , "Std Dev"       , "Coef of Var" ,
   "Median"        , "Med Abs Dev"  , "Max"           , "Min"         ,
   "Durbin-Watson" , "Std Dev(NOD)" , "Coef Var(NOD)" , "AutoCorr"    ,
   "AutoReg"       , "Absolute Max" , "ArgMax"        , "ArgMin"      ,
   "ArgAbsMax"     , "Sum"          , "Duration"      , "Centroid"    ,
   "CentDuration"  , "Absolute Sum" , "Non-zero Mean" , "Onset"       ,
   "Offset"        , "Accumulate"   , "SS"            , "BiwtMidV"    ,
   "ArgMin+1"      , "ArgMax+1"     , "ArgAbsMax+1"   , "CentroMean"  ,
   "CVarInv"       , "CvarInv (NOD)", "ZeroCount"     , "NZ Median"   ,
   "Signed Absmax" , "L2 Norm"      , "NonZero Count" , "NZ Stdev"    ,
   "Percentile %d" , "FirstValue"   , "TSNR"          , "MSSD"        ,
   "MSSDsqrt"      , "MASDx"
};

static int meth_pluginned[] = {
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 0             ,
   0               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   0               , 1              , 1               , 1             ,
   1               , 1 
};
#endif
