/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#ifndef _AFNI_SPLASH_HEADER
#define _AFNI_SPLASH_HEADER

#include "splash_blank.h"
#include "splash_cox1.h"
#include "splash_cox2.h"
#include "splash_cox3.h"
#include "splash_cox4.h"

/*------------------------------------------------------*/
/* Stuff for the little sub-image overlay at the right */

#undef  NOVER
#define NOVER 4

static char **bover[NOVER]={BAR_cox1  ,BAR_cox2  ,BAR_cox3  ,BAR_cox4  };
static int    xover[NOVER]={NX_cox1   ,NX_cox2   ,NX_cox3   ,NX_cox4   };
static int    yover[NOVER]={NY_cox1   ,NY_cox2   ,NY_cox3   ,NY_cox4   };
static int    lover[NOVER]={NLINE_cox1,NLINE_cox2,NLINE_cox3,NLINE_cox4};

#undef  IXOVER
#undef  JYOVER
#define IXOVER 332
#define JYOVER 171

/*----------------------------------------*/
/* Stuff for the image overlay at the top */

#undef  NX_TOPOVER
#undef  NY_TOPOVER
#define NX_TOPOVER 436
#define NY_TOPOVER 140

#undef  NMAIN
#define NMAIN 2
#ifdef  NMAIN

#  include "splash_gang.h"  /* MCW gang [gray]  */
#  include "splash_sscc.h"  /* NIH gang [color] */

  static char **bmain[NMAIN]={BAR_sscc  , BAR_gang  };
  static int    xmain[NMAIN]={NX_sscc   , NX_gang   };
  static int    ymain[NMAIN]={NY_sscc   , NY_gang   };
  static int    lmain[NMAIN]={NLINE_sscc, NLINE_gang};
  static byte  *rmapm[NMAIN]={RMAP_sscc , NULL      };  /* color maps */
  static byte  *gmapm[NMAIN]={GMAP_sscc , NULL      };
  static byte  *bmapm[NMAIN]={BMAP_sscc , NULL      };
  static int    nmapm[NMAIN]={NC_sscc   , 0         };

#endif /* NMAIN */
/*------------------------------------------*/

#endif /* _AFNI_SPLASH_HEADER */
