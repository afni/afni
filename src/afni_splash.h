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
#include "splash_mcw.h"

#undef  NOVER
#define NOVER 5

static char **bover[NOVER]={BAR_cox1  ,BAR_cox2  ,BAR_cox3  ,BAR_cox4  ,BAR_mcw  };
static int    xover[NOVER]={NX_cox1   ,NX_cox2   ,NX_cox3   ,NX_cox4   ,NX_mcw   };
static int    yover[NOVER]={NY_cox1   ,NY_cox2   ,NY_cox3   ,NY_cox4   ,NY_mcw   };
static int    lover[NOVER]={NLINE_cox1,NLINE_cox2,NLINE_cox3,NLINE_cox4,NLINE_mcw};

/*------------------------------------------------------------------------------------*/
#undef  NMAIN
#define NMAIN 1
#ifdef  NMAIN
#  include "splash_gang.h"

static char **bmain[NMAIN]={BAR_gang  };
static int    xmain[NOVER]={NX_gang   };
static int    ymain[NOVER]={NY_gang   };
static int    lmain[NOVER]={NLINE_gang};
#endif
/*------------------------------------------------------------------------------------*/

#undef  IXOVER
#undef  JYOVER
#define IXOVER 332
#define JYOVER 171

#endif /* _AFNI_SPLASH_HEADER */
