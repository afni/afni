#ifndef _AFNI_SPLASH_HEADER
#define _AFNI_SPLASH_HEADER

static byte map26[26] =
  {  30,  50,  70,  90, 106, 118, 130, 140, 146, 152, 158, 164, 170,
    176, 182, 190, 198, 206, 212, 218, 224, 230, 236, 242, 248, 254 } ;

#include "splash_blank.h"
#include "splash_cox1.h"
#include "splash_cox2.h"
#include "splash_cox3.h"

#undef  NOVER
#define NOVER 3

static char ** bover[NOVER] = { BAR_cox1   , BAR_cox2   , BAR_cox3   } ;
static int     xover[NOVER] = { NX_cox1    , NX_cox2    , NX_cox3    } ;
static int     yover[NOVER] = { NY_cox1    , NY_cox2    , NY_cox3    } ;
static int     lover[NOVER] = { NLINE_cox1 , NLINE_cox2 , NLINE_cox3 } ;

#undef  IXOVER
#undef  JYOVER
#define IXOVER 332
#define JYOVER 171

#endif /* _AFNI_SPLASH_HEADER */
