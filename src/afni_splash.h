/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#ifndef _AFNI_SPLASH_HEADER
#define _AFNI_SPLASH_HEADER

static byte map26[26] =
  {  30,  50,  70,  90, 106, 118, 130, 140, 146, 152, 158, 164, 170,
    176, 182, 190, 198, 206, 212, 218, 224, 230, 236, 242, 248, 254 } ;

#include "splash_blank.h"
#include "splash_cox1.h"
#include "splash_cox2.h"
#include "splash_cox3.h"
#include "splash_cox4.h"

#undef  NOVER
#define NOVER 4

static char **bover[NOVER]={BAR_cox1  ,BAR_cox2  ,BAR_cox3  ,BAR_cox4  };
static int    xover[NOVER]={NX_cox1   ,NX_cox2   ,NX_cox3   ,NX_cox4   };
static int    yover[NOVER]={NY_cox1   ,NY_cox2   ,NY_cox3   ,NY_cox4   };
static int    lover[NOVER]={NLINE_cox1,NLINE_cox2,NLINE_cox3,NLINE_cox4};

/*------------------------------------------------------------------------------------*/
#undef  NMAIN
#define NMAIN 1
#ifdef  NMAIN
#  include "splash_gang.h"

static char **bmain[NMAIN]={BAR_gang  };
static int    xmain[NMAIN]={NX_gang   };
static int    ymain[NMAIN]={NY_gang   };
static int    lmain[NMAIN]={NLINE_gang};
#endif
/*------------------------------------------------------------------------------------*/

#undef  IXOVER
#undef  JYOVER
#define IXOVER 332
#define JYOVER 171

#endif /* _AFNI_SPLASH_HEADER */
