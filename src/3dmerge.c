

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This program performs editing and/or merging of 3D datasets.

  File:    3dmerge.c
  Author:  R. W. Cox

  Mod:     Changes to implement "-doall" command option.
  Author:  B. D. Ward
  Date:    04 February 1998

  Mod:     Changes to implement "-1dindex" and "-1tindex" options.
  Author:  RWCox
  Date:    12 Nov 1998
-----------------------------------------------------------------------------*/

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#define PROGRAM_NAME "3dmerge"                    /* name of this program */
#define LAST_MOD_DATE "15 September 2000"         /* date of last program mod */

#include "mrilib.h"
#include "parser.h"    /* 09 Aug 2000 */

#define MAIN
#define MEGA  1048576  /* 2^20 */

#ifndef myXtFree
#define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

/*-------------------------- global data --------------------------*/

/*** combination flags:  mean, mean of nonzeros, max, ... ***/

#define CFLAG_MEAN   1  /* the default */
#define CFLAG_NZMEAN 2
#define CFLAG_MMAX   3
#define CFLAG_COUNT  4
#define CFLAG_AMAX   5
#define CFLAG_SMAX   6
#define CFLAG_ORDER  7
#define CFLAG_FISHER 8  /* 08 Aug 1996 */

#define THFLAG_NONE  0    /* 29 Aug 1996: added the ability to merge */
#define THFLAG_FICO  71   /*  threshold data as well as intensities  */

#define TANH(z)   tanh(z)  /* for the Fisher transformations */
#define ATANH(z) atanh(z)

/*** variables to hold status from command line options ***/

static EDIT_options MRG_edopt ;
static int MRG_have_edopt = 0 ;

static int   MRG_hits_g       = 0 ;
static int   MRG_cflag_g      = CFLAG_MEAN ;
static int   MRG_keepthr      = 0 ;
static float MRG_clust_rmm_g  = 0.0 ;
static float MRG_clust_vmul_g = 0.0 ;
static int   MRG_datum        = ILLEGAL_TYPE ;
static int   MRG_thdatum      = ILLEGAL_TYPE ;
static int   MRG_be_quiet     = 0 ;
static int   MRG_cflag_gthr   = THFLAG_NONE ;  /* 29 Aug 1996 */
static int   MRG_doall        = 0;             /* 02 Feb 1998 */

static char  MRG_output_session[THD_MAX_NAME]   = "./" ;
static char  MRG_output_prefix [THD_MAX_PREFIX] = "mrg" ;
#if 0
static char  MRG_output_label  [THD_MAX_LABEL]  = "\0" ;
#endif

static int   MRG_ivfim = -1 ;
static int   MRG_ivthr = -1 ;

static int   MRG_nscale = 0 ; /* 15 Sep 2000 */

/*--------------------------- prototypes ---------------------------*/
int MRG_read_opts( int , char ** ) ;
void MRG_Syntax(void) ;

/*--------------------------------------------------------------------
   read the arguments, load the global MRG_ variables, and return
   the index of the argument that we stopped at (that is, the first
   input filename)
----------------------------------------------------------------------*/

#ifdef MERGE_DEBUG
#  define DUMP1 fprintf(stderr,"ARG: %s\n",argv[nopt])
#  define DUMP2 fprintf(stderr,"ARG: %s %s\n",argv[nopt],argv[nopt+1])
#  define DUMP3 fprintf(stderr,"ARG: %s %s %s\n",argv[nopt],argv[nopt+1],argv[nopt+2])
#else
#  define DUMP1
#  define DUMP2
#  define DUMP3
#endif

int MRG_read_opts( int argc , char * argv[] )
{
   int nopt = 1 ;
   float val ;
   int  ival ;

   INIT_EDOPT( &MRG_edopt ) ;

   while( nopt < argc && argv[nopt][0] == '-' ){

      /**** check editing options ****/

      ival = EDIT_check_argv( argc , argv , nopt , &MRG_edopt ) ;
      if( ival > 0 ){
         nopt += ival ; MRG_have_edopt = 1 ;
         continue ;
      }

      /**** Nov 1998: -1tindex and -1dindex ****/

      if( strncmp(argv[nopt],"-1dindex",5) == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"need an argument after -1dindex!\n") ; exit(1) ;
         }
         MRG_ivfim = MRG_edopt.iv_fim = (int) strtod( argv[nopt++] , NULL ) ;
         continue ;
      }

      if( strncmp(argv[nopt],"-1tindex",5) == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"need an argument after -1tindex!\n") ; exit(1) ;
         }
         MRG_ivthr = MRG_edopt.iv_thr = (int) strtod( argv[nopt++] , NULL ) ;
         continue ;
      }

      /**** 09 Aug 2000: -1fmask dset ****/

      if( strncmp(argv[nopt],"-1fmask",6) == 0 ){
         THD_3dim_dataset * mset ; int nn ;

         if( MRG_edopt.nfmask > 0 ){
            fprintf(stderr,"Can't have 2 -fmask options!\n") ;
            exit(1) ;
         }
         if( ++nopt >= argc ){
            fprintf(stderr,"No argument after %s?\n",argv[nopt-1]) ;
            exit(1);
         }

         mset = THD_open_dataset( argv[nopt++] ) ;
         if( mset == NULL ){
            fprintf(stderr,"*** Can't open -fmask dataset\n") ;
            exit(1) ;
         }
         if( DSET_BRICK_TYPE(mset,0) == MRI_complex ){
            fprintf(stderr,"*** Can't deal with complex-valued -fmask dataset\n");
            exit(1) ;
         }

         MRG_edopt.fmask  = THD_makemask( mset , 0 , 666.0,-666.0 ) ;
         MRG_edopt.nfmask = DSET_NVOX(mset) ;

         nn = THD_countmask(MRG_edopt.nfmask,MRG_edopt.fmask) ;
         if( nn < 2 ){
            fprintf(stderr,"*** Too few (%d) nonzero voxels in -fmask!\n",nn) ;
            exit(1) ;
         }

         DSET_delete(mset) ;

         if( MRG_edopt.filter_opt == FCFLAG_AVER )
            MRG_edopt.filter_opt = FCFLAG_MEAN ;

         if( MRG_edopt.thrfilter_opt == FCFLAG_AVER )
            MRG_edopt.thrfilter_opt = FCFLAG_MEAN ;

         continue ;
      }

      /**** 11 Sep 2000: -1filter_winsor rmm nw ****/

      if( strcmp(argv[nopt],"-1filter_winsor") == 0 ){
         int nwin ;

         nopt++ ;
         if( nopt+1 >= argc ){
            fprintf(stderr,"*** Need 2 arguments after -1filter_winsor\n") ;
            exit(1) ;
         }
         MRG_edopt.filter_rmm  = strtod( argv[nopt++] , NULL ) ;
         if( MRG_edopt.filter_rmm <= 0.0 ){
            fprintf(stderr,"*** Illegal rmm value after -1filter_winsor\n");
            exit(1) ;
         }
         nwin = (int) strtod( argv[nopt++] , NULL ) ;
         if( nwin <= 0 || nwin >= FCFLAG_ONE_STEP ){
            fprintf(stderr,"*** Illegal nw value after -1filter_winsor\n");
            exit(1) ;
         }
         MRG_edopt.filter_opt = FCFLAG_WINSOR + nwin ;
         MRG_have_edopt = 1 ; continue ;
      }

      /**** 09 Aug 2000: -1filter_expr rmm expr ****/

      if( strncmp(argv[nopt],"-1filter_expr",13) == 0 ){
         PARSER_code * pcode ; int hsym[26] , aa , naa , nee ;
         double atoz[26] , val ;

         nopt++ ;
         if( nopt+1 >= argc ){
            fprintf(stderr,"*** Need 2 arguments after -1filter_expr\n") ;
            exit(1) ;
         }
         MRG_edopt.filter_opt = FCFLAG_EXPR;
         MRG_edopt.filter_rmm  = strtod( argv[nopt++] , NULL ) ;
         if( MRG_edopt.filter_rmm <= 0.0 ){
            fprintf(stderr,"*** Illegal rmm value after -1filter_expr\n");
            exit(1) ;
         }

         MRG_edopt.fexpr = argv[nopt++] ;
         pcode = PARSER_generate_code( MRG_edopt.fexpr ) ;  /* compile */

         if( pcode == NULL ){
            fprintf(stderr,"*** Illegal expr after -1filter_expr\n");
            exit(1);
         }

#undef   MASK
#undef   PREDEFINED_MASK
#define  MASK(x)          (1 << (x-'a'))
#define  PREDEFINED_MASK  ( MASK('r') | MASK('x') | MASK('y') | MASK('z')   \
                                      | MASK('i') | MASK('j') | MASK('k') )

         /* check if only legal symbols are used */

         PARSER_mark_symbols( pcode , hsym ) ;  /* find symbols used */

         for( nee=naa=aa=0 ; aa < 26 ; aa++ ){
            if( hsym[aa] ){
               if( ((1<<aa) & PREDEFINED_MASK) == 0 ){
                  nee++ ;  /* count of illegal symbols */
                  fprintf(stderr,"*** Symbol %c in -1filter_expr is illegal\n",'a'+aa) ;
               } else {
                  naa++ ;  /* count of legal symbols */
               }
            }
         }
         if( nee > 0 ) exit(1) ;  /* can't use this expression! */
         if( naa == 0 ){          /* no symbols?  check if identically 0 */
            double atoz[26] , val ;
            val = PARSER_evaluate_one( pcode , atoz ) ;
            if( val != 0.0 ){
               fprintf(stderr,"+++ Warning: -1filter_expr is constant = %g\n",val) ;
            } else {
               fprintf(stderr,"*** -1filter_expr is identically zero\n") ;
               exit(1) ;
            }
         }

         free(pcode) ;  /* will recompile when it is used */

         MRG_have_edopt = 1 ; continue ;
      }

      /**** -quiet ****/

      if( strncmp(argv[nopt],"-quiet",6) == 0 ){
         MRG_be_quiet = 1 ;
         nopt++ ; continue ;
      }


      /**** -keepthr ****/

      if( strncmp(argv[nopt],"-keepthr",6) == 0 ){
         MRG_keepthr = 1 ;
         nopt++ ; continue ;
      }

      /**** -doall ****/   /* 02 Feb 1998 */

      if( strncmp(argv[nopt],"-doall",6) == 0 ){
         MRG_doall = 1 ;
         nopt++ ; continue ;
      }

      /**** -datum type ****/

      if( strncmp(argv[nopt],"-datum",6) == 0 ){
DUMP2 ;
         if( ++nopt >= argc ){
            fprintf(stderr,"need an argument after -datum!\n") ; exit(1) ;
         }

         if( strcmp(argv[nopt],"short") == 0 ){
            MRG_datum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            MRG_datum = MRI_float ;
         } else if( strcmp(argv[nopt],"byte") == 0 ){
            MRG_datum = MRI_byte ;
         } else if( strcmp(argv[nopt],"complex") == 0 ){  /* not listed help */
            MRG_datum = MRI_complex ;
         } else {
            fprintf(stderr,"-datum of type '%s' is not supported in 3dmerge!\n",
                    argv[nopt] ) ;
            exit(1) ;
         }
         nopt++ ; continue ;  /* go to next arg */
      }

      /**** -thdatum type ****/

      if( strncmp(argv[nopt],"-thdatum",6) == 0 ){
DUMP2 ;
         if( ++nopt >= argc ){
            fprintf(stderr,"need an argument after -thdatum!\n") ; exit(1) ;
         }

         if( strcmp(argv[nopt],"short") == 0 ){
            MRG_thdatum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            MRG_thdatum = MRI_float ;
         } else if( strcmp(argv[nopt],"byte") == 0 ){
            MRG_thdatum = MRI_byte ;
         } else {
            fprintf(stderr,"-thdatum of type '%s' is not supported in 3dmerge!\n",
                    argv[nopt] ) ;
            exit(1) ;
         }
         MRG_keepthr = 1 ;    /* -thdatum is meaningless without this */
         nopt++ ; continue ;  /* go to next arg */
      }

      /**** -ghits count ****/

      if( strncmp(argv[nopt],"-ghits",6) == 0 ){
DUMP2 ;
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -ghits!\n") ; exit(1) ;
         }
         MRG_hits_g = strtod( argv[nopt++] , NULL ) ;
         if( MRG_hits_g <= 0 ){
            fprintf(stderr,"illegal value after -ghits\n") ;
            exit(1) ;
         }
         continue ;
      }

      /**** -gclust rmm vmul ****/

      if( strncmp(argv[nopt],"-gclust",6) == 0 ){
DUMP3 ;
         nopt++ ;
         if( nopt+1 >= argc ){
            fprintf(stderr,"need 2 arguments after -gclust!\n") ;
            exit(1) ;
         }
         MRG_clust_rmm_g  = strtod( argv[nopt++] , NULL ) ;
         MRG_clust_vmul_g = strtod( argv[nopt++] , NULL ) ;
         if( MRG_clust_rmm_g <= 0 || MRG_clust_vmul_g <= 0 ){
            fprintf(stderr,"illegal value after -gclust\n") ;
            exit(1) ;
         }
         continue ;
      }

      /**** -session dirname ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
DUMP2 ;
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -session!\n") ;
            exit(1) ;
         }
         MCW_strncpy( MRG_output_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
DUMP2 ;
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -prefix!\n") ;
            exit(1) ;
         }
         MCW_strncpy( MRG_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

#if 0
      /**** -label string ****/

      if( strncmp(argv[nopt],"-label",6) == 0 ){
DUMP2 ;
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -label!\n") ;
            exit(1) ;
         }
         MCW_strncpy( MRG_output_label , argv[nopt++] , THD_MAX_LABEL ) ;
         continue ;
      }
#endif

      /**** -nscale [15 Sep 2000] ****/

      if( strcmp(argv[nopt],"-nscale") == 0 ){
DUMP1 ;
         MRG_nscale = 1 ;
         nopt++ ; continue ;
      }


      /**** -gmean ****/

      if( strncmp(argv[nopt],"-gmean",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_MEAN ;
         nopt++ ; continue ;
      }

      /**** -gfisher ****/

      if( strncmp(argv[nopt],"-gfisher",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_FISHER ;
         nopt++ ; continue ;
      }

      /**** -tgfisher (29 Aug 1996) ****/

      if( strncmp(argv[nopt],"-tgfisher",6) == 0 ){
DUMP1 ;
         MRG_cflag_gthr = THFLAG_FICO ;
         nopt++ ; continue ;
      }

      /**** -gnzmean ****/

      if( strncmp(argv[nopt],"-gnzmean",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_NZMEAN ;
         nopt++ ; continue ;
      }

      /**** -gmax ****/

      if( strncmp(argv[nopt],"-gmax",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_MMAX ;
         nopt++ ; continue ;
      }

      /**** -gamax ****/

      if( strncmp(argv[nopt],"-gamax",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_AMAX ;
         nopt++ ; continue ;
      }

      /**** -gsmax ****/

      if( strncmp(argv[nopt],"-gsmax",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_SMAX ;
         nopt++ ; continue ;
      }

      /*** -gcount ****/

      if( strncmp(argv[nopt],"-gcount",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_COUNT ;
         nopt++ ; continue ;
      }

      /*** -gorder ****/

      if( strncmp(argv[nopt],"-gorder",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_ORDER ;
         nopt++ ; continue ;
      }

      /**** unknown switch ****/

      fprintf(stderr,"unrecognized option %s\n",argv[nopt]) ;
      exit(1) ;

   }  /* end of loop over options */

   /*** cleanup ***/

#if 0
   if( strlen(MRG_output_label) == 0 ){
      MCW_strncpy( MRG_output_label , MRG_output_prefix , THD_MAX_LABEL ) ;
   }
#endif

   return( nopt );
}

/*------------------------------------------------------------------*/

void MRG_Syntax(void)
{
   printf(
    "Edit and/or merge 3D datasets\n"
    "Usage: 3dmerge [options] datasets ...\n"
    "where the options are:\n"
   ) ;

   printf( "%s\n" , EDIT_options_help() ) ;

   printf(
    "OTHER OPTIONS:\n"
    "  -datum type = Coerce the output data to be stored as the given type,\n"
    "                  which may be byte, short, or float.\n"
    "          N.B.: Byte data cannot be negative.  If this datum type is chosen,\n"
    "                  any negative values in the edited and/or merged dataset\n"
    "                  will be set to zero.\n"
    "  -keepthr    = When using 3dmerge to edit exactly one dataset of a\n"
    "                  functional type with a threshold statistic attached,\n"
    "                  normally the resulting dataset is of the 'fim'\n"
    "                  (intensity only) type.  This option tells 3dmerge to\n"
    "                  copy the threshold data (unedited in any way) into\n"
    "                  the output dataset.\n"
    "          N.B.: This option is ignored if 3dmerge is being used to\n"
    "                  combine 2 or more datasets.\n"
    "          N.B.: The -datum option has no effect on the storage of the\n"
    "                  threshold data.  Instead use '-thdatum type'.\n"
    "\n"
    "  -doall      = Apply editing and merging options to ALL sub-bricks \n"
    "                  uniformly in a dataset.\n"
    "          N.B.: All datasets must have the same number of sub-bricks \n"
    "                  when using the -doall option. \n"
    "          N.B.: The threshold specific options (such as -1thresh, \n"
    "                  -keepthr, -tgfisher, etc.) are not compatible with \n"
    "                  the -doall command.  Neither are the -1dindex or\n"
    "                  the -1tindex options.\n"
    "          N.B.: All labels and statistical parameters for individual \n"
    "                  sub-bricks are copied from the first dataset.  It is \n"
    "                  the responsibility of the user to verify that these \n"
    "                  are appropriate.  Note that sub-brick auxiliary data \n"
    "                  can be modified using program 3drefit. \n"
    "\n"
    "  -1dindex j  = Uses sub-brick #j as the data source , and uses sub-brick\n"
    "  -1tindex k  = #k as the threshold source.  With these, you can operate\n"
    "                  on any given sub-brick of the inputs dataset(s) to produce\n"
    "                  as output a 1 brick dataset.  If desired, a collection\n"
    "                  of 1 brick datasets can later be assembled into a\n"
    "                  multi-brick bucket dataset using program '3dbucket'\n"
    "                  or into a 3D+time dataset using program '3dTcat'.\n"
    "          N.B.: If these options aren't used, j=0 and k=1 are the defaults\n"
    "\n"
    "  The following option allows you to specify a mask dataset that\n"
    "  limits the action of the 'filter' options to voxels that are\n"
    "  nonzero in the mask:\n"
    "\n"
    "  -1fmask mset = Read dataset 'mset' (which can include a\n"
    "                  sub-brick specifier) and use the nonzero\n"
    "                  voxels as a mask for the filter options.\n"
    "                  Filtering calculations will not use voxels\n"
    "                  that are outside the mask.  If an output\n"
    "                  voxel does not have ANY masked voxels inside\n"
    "                  the rmm radius, then that output voxel will\n"
    "                  be set to 0.\n"
    "         N.B.: * Only the -1filter_* and -t1filter_* options are\n"
    "                 affected by -1fmask.\n"
    "               * In the linear averaging filters (_mean, _nzmean,\n"
    "                 and _expr), voxels not in the mask will not be used\n"
    "                 or counted in either the numerator or denominator.\n"
    "                 This can give unexpected results.  If the mask is\n"
    "                 designed to exclude the volume outside the brain,\n"
    "                 then voxels exterior to the brain, but within 'rmm',\n"
    "                 will have a few voxels inside the brain included\n"
    "                 in the filtering.  Since the sum of weights (the\n"
    "                 denominator) is only over those few intra-brain\n"
    "                 voxels, the effect will be to extend the significant\n"
    "                 part of the result outward by rmm from the surface\n"
    "                 of the brain.  In contrast, without the mask, the\n"
    "                 many small-valued voxels outside the brain would\n"
    "                 be included in the numerator and denominator sums,\n"
    "                 which would barely change the numerator (since the\n"
    "                 voxel values are small outside the brain), but would\n"
    "                 increase the denominator greatly (by including many\n"
    "                 more weights).  The effect in this case (no -1fmask)\n"
    "                 is to make the filtering taper off gradually in the\n"
    "                 rmm-thickness shell around the brain.\n"
    "               * Thus, if the -1fmask is intended to clip off non-brain\n"
    "                 data from the filtering, its use should be followed by\n"
    "                 masking operation using 3dcalc:\n"
    "      3dmerge -1filter_aver 12 -1fmask mask+orig -prefix x input+orig\n"
    "      3dcalc  -a x -b mask+orig -prefix y -expr 'a*step(b)'\n"
    "      rm -f x+orig.*\n"
    "                 The desired result is y+orig - filtered using only\n"
    "                 brain voxels (as defined by mask+orig), and with\n"
    "                 the output confined to the brain voxels as well.\n"
    "\n"
    "  The following option allows you to specify an almost arbitrary\n"
    "  weighting function for 3D linear filtering:\n"
    "\n"
    "  -1filter_expr rmm expr\n"
    "     Defines a linear filter about each voxel of radius 'rmm' mm.\n"
    "     The filter weights are proportional to the expression evaluated\n"
    "     at each voxel offset in the rmm neighborhood.  You can use only\n"
    "     these symbols in the expression:\n"
    "         r = radius from center\n"
    "         x = dataset x-axis offset from center\n"
    "         y = dataset y-axis offset from center\n"
    "         z = dataset z-axis offset from center\n"
    "         i = x-axis index offset from center\n"
    "         j = y-axis index offset from center\n"
    "         k = z-axis index offset from center\n"
    "     Example:\n"
    "       -1filter_expr 12.0 'exp(-r*r/36.067)'\n"
    "     This does a Gaussian filter over a radius of 12 mm.  In this\n"
    "     example, the FWHM of the filter is 10 mm. [in general, the\n"
    "     denominator in the exponent would be 0.36067 * FWHM * FWHM.\n"
    "     This is the only way to get a Gaussian blur combined with the\n"
    "     -1fmask option.  The radius rmm=12 is chosen where the weights\n"
    "     get smallish.]  Another example:\n"
    "       -1filter_expr 20.0 'exp(-(x*x+16*y*y+z*z)/36.067)'\n"
    "     which is a non-spherical Gaussian filter.\n"
    "\n"
    "  The following option lets you apply a 'Winsor' filter to the data:\n"
    "\n"
    "  -1filter_winsor rmm nw\n"
    "     The data values within the radius rmm of each voxel are sorted.\n"
    "     Suppose there are 'N' voxels in this group.  We index the\n"
    "     sorted voxels as s[0] <= s[1] <= ... <= s[N-1], and we call the\n"
    "     value of the central voxel 'v' (which is also in array s[]).\n"
    "                 If v < s[nw]    , then v is replaced by s[nw]\n"
    "       otherwise If v > s[N-1-nw], then v is replace by s[N-1-nw]\n"
    "       otherwise v is unchanged\n"
    "     The effect is to increase 'too small' values up to some\n"
    "     middling range, and to decrease 'too large' values.\n"
    "     If N is odd, and nw=(N-1)/2, this would be a median filter.\n"
    "     In practice, I recommend that nw be about N/4; for example,\n"
    "       -dxyz=1 -1filter_winsor 2.5 19\n"
    "     is a filter with N=81 that gives nice results.\n"
    "   N.B.: This option is NOT affected by -1fmask\n"
    "   N.B.: This option is slow!\n"
    "\n"
    "MERGING OPTIONS APPLIED TO FORM THE OUTPUT DATASET:\n"
    " [That is, different ways to combine results. The]\n"
    " [following '-g' options are mutually exclusive! ]\n"
    "  -gmean     = Combine datasets by averaging intensities\n"
    "                 (including zeros) -- this is the default\n"
    "  -gnzmean   = Combine datasets by averaging intensities\n"
    "                 (not counting zeros)\n"
    "  -gmax      = Combine datasets by taking max intensity\n"
    "                 (e.g., -7 and 2 combine to 2)\n"
    "  -gamax     = Combine datasets by taking max absolute intensity\n"
    "                 (e.g., -7 and 2 combine to 7)\n"
    "  -gsmax     = Combine datasets by taking max signed intensity\n"
    "                 (e.g., -7 and 2 combine to -7)\n"
    "  -gcount    = Combine datasets by counting number of 'hits' in\n"
    "                  each voxel (see below for defintion of 'hit')\n"
    "  -gorder    = Combine datasets in order of input:\n"
    "                * If a voxel is nonzero in dataset #1, then\n"
    "                    that value goes into the voxel.\n"
    "                * If a voxel is zero in dataset #1 but nonzero\n"
    "                    in dataset #2, then the value from #2 is used.\n"
    "                * And so forth: the first dataset with a nonzero\n"
    "                    entry in a given voxel 'wins'\n"
    "  -gfisher   = Takes the arctanh of each input, averages these,\n"
    "                  and outputs the tanh of the average.  If the input\n"
    "                  datum is 'short', then input values are scaled by\n"
    "                  0.0001 and output values by 10000.  This option\n"
    "                  is for merging bricks of correlation coefficients.\n"
    "\n"
    "  -nscale    = If the output datum is shorts, don't do the scaling\n"
    "                  to the max range [similar to 3dcalc's -nscale option]\n"
    "\n"
    "MERGING OPERATIONS APPLIED TO THE THRESHOLD DATA:\n"
    " [That is, different ways to combine the thresholds.  If none of these ]\n"
    " [are given, the thresholds will not be merged and the output dataset  ]\n"
    " [will not have threshold data attached.  Note that the following '-tg']\n"
    " [command line options are mutually exclusive, but are independent of  ]\n"
    " [the '-g' options given above for merging the intensity data values.  ]\n"
    "  -tgfisher  = This option is only applicable if each input dataset\n"
    "                  is of the 'fico' or 'fith' types -- functional\n"
    "                  intensity plus correlation or plus threshold.\n"
    "                  (In the latter case, the threshold values are\n"
    "                  interpreted as correlation coefficients.)\n"
    "                  The correlation coefficients are averaged as\n"
    "                  described by -gfisher above, and the output\n"
    "                  dataset will be of the fico type if all inputs\n"
    "                  are fico type; otherwise, the output datasets\n"
    "                  will be of the fith type.\n"
    "         N.B.: The difference between the -tgfisher and -gfisher\n"
    "                  methods is that -tgfisher applies to the threshold\n"
    "                  data stored with a dataset, while -gfisher\n"
    "                  applies to the intensity data.  Thus, -gfisher\n"
    "                  would normally be applied to a dataset created\n"
    "                  from correlation coefficients directly, or from\n"
    "                  the application of the -1thtoin option to a fico\n"
    "                  or fith dataset.\n"
    "\n"
    "OPTIONAL WAYS TO POSTPROCESS THE COMBINED RESULTS:\n"
    " [May be combined with the above methods.]\n"
    " [Any combination of these options may be used.]\n"
    "  -ghits count     = Delete voxels that aren't !=0 in at least\n"
    "                       count datasets (!=0 is a 'hit')\n"
    "  -gclust rmm vmul = Form clusters with connection distance rmm\n"
    "                       and clip off data not in clusters of\n"
    "                       volume at least vmul microliters\n"
    "\n"
    "The '-g' and '-tg' options apply to the entire group of input datasets.\n"
    "\n"

    "OPTIONS THAT CONTROL THE NAMES OF THE OUTPUT DATASET:\n"
    "  -session dirname  = write output into given directory (default=./)\n"
    "  -prefix  pname    = use 'pname' for the output directory prefix\n"
    "                       (default=mrg)\n"
#if 0
    "  -label   string   = use 'string' for the label in the output\n"
    "                       dataset (the label is used for switching\n"
    "                       between datasets in AFNI)\n"
#endif
    "\n"

    "NOTES:\n"
    " **  If only one dataset is read into this program, then the '-g'\n"
    "       options do not apply, and the output dataset is simply the\n"
    "       '-1' options applied to the input dataset (i.e., edited).\n"
    " **  A merged output dataset is ALWAYS of the intensity-only variety.\n"
    " **  You can combine the outputs of 3dmerge with other sub-bricks\n"
    "       using the program 3dbucket.\n"
    " **  Complex-valued datasets cannot be merged.\n"
    " **  This program cannot handle time-dependent datasets.\n"
    " **  Note that the input datasets are specified by their .HEAD files,\n"
    "       but that their .BRIK files must exist also!\n"
   ) ;
   exit(0) ;
}

/*------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int file_num , first_file , nx,ny,nz , nxyz , ii , num_dset ,
       file_count , ptmin , iclu,nclu , edit_type , ival,ivout , tval ;
   float dx,dy,dz , fac , dxyz , rmm,vmul ;
   THD_3dim_dataset * dset=NULL , * new_dset=NULL ;
   THD_3dim_dataset ** dsetar=NULL ;                 /* Nov 1998 */
   short * gnum=NULL ;
   float * gfim=NULL , * tfim=NULL , * ggfim=NULL , * ttfim=NULL ;
   int     datum ;
   MCW_cluster_array * clar ;
   float fimfac , fimfacinv , first_fimfac , thrfac ;
   int   output_datum , output_thdatum ;
   int   input_datum  , input_thdatum , first_datum ;

   float thr_stataux[MAX_STAT_AUX] ;
   int   num_fico ;
   int   is_int=1 ;   /* 08 Jan 1998 */

   int iv, iv_bot, iv_top;      /* dataset sub-brick indices    02 Feb 1998 */

   /*----- identify program -----*/
   printf ("\n\nProgram %s \n", PROGRAM_NAME);
   printf ("Last revision: %s \n\n", LAST_MOD_DATE);

   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) MRG_Syntax() ;

   first_file = MRG_read_opts( argc , argv ) ;
   file_count = argc - first_file ;            /* number of datasets input */

   if( ! MRG_be_quiet )
      printf("3dmerge: edit and combine 3D datasets, by R.W. Cox (rwcox@mcw.edu)\n") ;

   if( first_file < 1 || first_file >= argc ){
      fprintf(stderr,"*** ILLEGAL COMMAND LINE ***\n") ; exit(1) ;
   }

   /*----- check for compatibility of user options -----*/ /* 02 Feb 1998 */
   if (MRG_doall)
     { int nerr = 0 ;

       if( MRG_ivfim >= 0 ){   /* Nov 1998 */
         fprintf(stderr,"-1dindex is not compatible with -doall option \n");
         nerr++ ;  }
       if( MRG_ivthr >= 0 ){   /* Nov 1998 */
         fprintf(stderr,"-1dindex is not compatible with -doall option \n");
         nerr++ ;  }

       if (MRG_edopt.thtoin > 0)  {
	 fprintf (stderr, "-1thtoin is not compatible with -doall option \n");
	 nerr++ ;  }
       if (MRG_edopt.thresh > 0.0)  {
	 fprintf (stderr, "-1thresh is not compatible with -doall option \n");
	 nerr++ ;  }
       if (MRG_edopt.thrfilter_opt > 0)  {
	 fprintf (stderr, "-t1filter is not compatible with -doall option \n");
	 nerr++ ;  }
       if (MRG_edopt.thrblur > 0.0)  {
	 fprintf (stderr, "-t1blur is not compatible with -doall option \n");
	 nerr++ ;  }
       if (MRG_thdatum >= 0)  {
	 fprintf (stderr, "-thdatum is not compatible with -doall option \n");
	 nerr++ ;  }
       if (MRG_keepthr)  {
	 fprintf (stderr, "-keepthr is not compatible with -doall option \n");
	 nerr++ ;  }
       if (MRG_cflag_gthr > 0)  {
	 fprintf (stderr, "-tgfisher is not compatible with -doall option \n");
	 nerr++ ;  }

       if( nerr > 0 ) exit(1) ;
     }

   /* Nov 1998: other checks */

   if( (MRG_ivfim >= 0 || MRG_ivthr >=0) && MRG_keepthr ){
      fprintf(stderr,"-keepthr is not compatible with -1dindex or -1tindex\n") ;
      exit(1) ;
   }

   if( (MRG_ivfim >= 0 || MRG_ivthr >=0) && MRG_cflag_gthr ){
      fprintf(stderr,"-tgfisher is not compatible with -1dindex or -1tindex\n") ;
      exit(1) ;
   }

   /* check for existence of each input data set. */   /* 09 December 1996 */
   for (file_num = first_file;  file_num < argc;  file_num++)
     {
       dset = THD_open_one_dataset( argv[file_num] ) ;
       if( ! ISVALID_3DIM_DATASET(dset) )
	 {
	   fprintf(stderr,"*** cannot open dataset %s\n",argv[file_num]) ;
	   exit(1) ;
         }

       /* Nov 1998: check user-controlled brick indexes */

       if( MRG_ivfim >= DSET_NVALS(dset) ){
          fprintf(stderr,
                  "*** Dataset %s does not have enough bricks for -1dindex %d\n" ,
                  argv[file_num],MRG_ivfim) ;
          exit(1) ;
       }
       if( MRG_ivthr >= DSET_NVALS(dset) ){
          fprintf(stderr,
                  "*** Dataset %s does not have enough bricks for -1tindex %d\n" ,
                  argv[file_num],MRG_ivthr) ;
          exit(1) ;
       }

       THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
     }

   /* read first dataset */

   dset = THD_open_one_dataset( argv[first_file] ) ;
   if( ! ISVALID_3DIM_DATASET(dset) ){
      fprintf(stderr,"*** Unable to open first dataset %s\n",argv[first_file]) ;
      exit(1) ;
   }

   if( DSET_NUM_TIMES(dset) > 1 && (!MRG_doall && MRG_ivfim < 0) ){
      fprintf(stderr, "*** Unable to merge time-dependent datasets"
                      " without -doall or -1dindex\n") ;
      exit(1) ;
   }

   /* get the dimensions */

   nx = dset->daxes->nxx ;
   ny = dset->daxes->nyy ;
   nz = dset->daxes->nzz ; nxyz = nx*ny*nz ;

   dx = fabs(dset->daxes->xxdel) ;
   dy = fabs(dset->daxes->yydel) ;
   dz = fabs(dset->daxes->zzdel) ;

   if( MRG_edopt.fake_dxyz ) dx = dy = dz = 1.0 ;  /* 11 Sep 2000 */

   nice(1) ;  /* slow us down, a little */

   if( MRG_edopt.nfmask > 0 && MRG_edopt.nfmask != nxyz ){
      fprintf(stderr,
              "*** -1fmask and 1st dataset don't have same number of voxels\n\a");
      exit(1) ;
   }

   /*******************************************************************/
   /****      if only one file, edit it, modify its names ...      ****/
   /****      then write the modified dataset to disk              ****/

   if( file_count == 1 ){

      ival        = DSET_PRINCIPAL_VALUE(dset) ;
      input_datum = DSET_BRICK_TYPE(dset,ival) ;
      if( MRG_datum >= 0 ) output_datum = MRG_datum ;
      else                 output_datum = input_datum ;

      /** 17 Sep 1998: Move the creation of the new dataset
                       to AFTER the editing operations, so that any
                       dataset parameter changes in EDIT_one_dataset
                       will properly be propagated to the new dataset.
                       The creation shown here is just to check if the
                       output dataset exists already.                   **/

      new_dset = EDIT_empty_copy( dset ) ;

      EDIT_dset_items( new_dset ,
                          ADN_prefix         , MRG_output_prefix ,
                          ADN_directory_name , MRG_output_session ,
                       ADN_none ) ;

      if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
         fprintf(stderr,
                 "*** Output file %s already exists -- cannot continue!\n",
                 new_dset->dblk->diskptr->header_name ) ;
         exit(1) ;
      }

      THD_delete_3dim_dataset( new_dset , False ) ;  /* toss this junk */

      /** get ready to go **/

      if( ! MRG_be_quiet ){
         printf("-- editing input dataset in memory (%.1f MB)\n",
                ((double)dset->dblk->total_bytes) / MEGA ) ;
         fflush(stdout) ;
      }

  /* 02 Feb 1998 */
  if (MRG_doall)
    {  iv_bot = 0;  iv_top = DSET_NVALS(dset);  }
  else if( MRG_ivfim >= 0 )                       /* Nov 1998 */
    {  iv_bot = MRG_ivfim ; iv_top = iv_bot+1 ; }
  else
    {  iv_bot = DSET_PRINCIPAL_VALUE(dset);  iv_top = iv_bot + 1;  }

  /*----- Iterate over sub-bricks -----*/
  for (iv = iv_bot;  iv < iv_top;  iv++)
    {
      if ((!MRG_be_quiet) && MRG_doall) printf ("Editing sub-brick %d\n", iv);

      MRG_edopt.iv_fim = iv;

      EDIT_one_dataset( dset , &MRG_edopt ) ;  /* all the real work */

      if( !MRG_be_quiet && !MRG_doall ){ printf(".") ; fflush(stdout) ; }
    }

   if( MRG_edopt.nfmask > 0 ){
     free(MRG_edopt.fmask) ; MRG_edopt.fmask = NULL ; MRG_edopt.nfmask = 0 ;
   }

    if( !MRG_be_quiet && !MRG_doall ) printf("\n") ;

      /** 17 Sep 1998: NOW create the new dataset **/

      new_dset = EDIT_empty_copy( dset ) ;

      tross_Copy_History( dset , new_dset ) ;
      tross_Make_History( "3dmerge" , argc , argv , new_dset ) ;

      EDIT_dset_items( new_dset ,
                          ADN_prefix , MRG_output_prefix ,
                          ADN_label1 , MRG_output_prefix ,
                          ADN_directory_name , MRG_output_session ,
                       ADN_none ) ;
      strcat( new_dset->self_name , "(ED)" ) ;
                                                           /* 02 Feb 1998 */
      if( (! MRG_keepthr) && (new_dset->dblk->nvals > 1) && (! MRG_doall) )
         EDIT_dset_items( new_dset ,
                             ADN_nvals , 1 ,
                             ADN_ntt   , 0 ,                 /* Nov 1998 */
                             ADN_func_type , FUNC_FIM_TYPE ,
                          ADN_none ) ;

      if( MRG_keepthr && ISFUNC(new_dset) && FUNC_HAVE_THR(new_dset->func_type) ){
         ii            = FUNC_ival_thr[dset->func_type] ;
         input_thdatum = DSET_BRICK_TYPE(dset,ii) ;
         if( MRG_thdatum >= 0 ) output_thdatum = MRG_thdatum ;
         else                   output_thdatum = input_thdatum ;
      } else {
         output_thdatum = input_thdatum = ILLEGAL_TYPE ;
      }

      /** Coerce the output data type into a new brick, if needed **/

  /*----- Iterate over sub-bricks [again] -----*/
  for (iv = iv_bot;  iv < iv_top;  iv++)
    {
  /* 02 Feb 1998 */
  if (MRG_doall)
    {
      ival  = iv;
      ivout = iv;
    }
  else if( MRG_ivfim >= 0 )
    {
      ival  = MRG_ivfim ;
      ivout = DSET_PRINCIPAL_VALUE(new_dset) ;
    }
  else
    {
      ival  = DSET_PRINCIPAL_VALUE(dset) ;
      ivout = DSET_PRINCIPAL_VALUE(new_dset) ;
    }

      if( input_datum == output_datum ){

         /** Attach the brick of the input dataset to the brick of the output.  **/
         /** (This isn't exactly kosher, but we are exiting almost immediately) **/

         mri_fix_data_pointer( DSET_ARRAY(dset,ival) , DSET_BRICK(new_dset,ivout) ) ;

#if 1
#   if 0
         if( ivout != ival )
#   endif
            DSET_BRICK_FACTOR(new_dset,ivout) = DSET_BRICK_FACTOR(dset,ival) ;
#endif

      } else {

         /** Must create a new brick and do the conversion **/

         void * dfim , * efim ;
         float efac = DSET_BRICK_FACTOR(dset,ival) ;

         if( ! MRG_be_quiet ){
            printf("-- coercing output datum to be %s\n",
                   MRI_TYPE_name[output_datum]);
         }

         efim = DSET_ARRAY(dset,ival) ;
         dfim = (void *) malloc( mri_datum_size(output_datum) * nxyz ) ;
         if( dfim == NULL ){
            fprintf(stderr,"*** Can't malloc output brick #%d\n",ivout); exit(1);
         }

         /** 03 Dec 1998: scale to integer and float types separately **/

         if( MRI_IS_INT_TYPE(output_datum) ){
            fimfac = EDIT_coerce_autoscale( nxyz , input_datum  , efim ,
                                                   output_datum , dfim  ) ;
            if( fimfac == 0.0 ) fimfac  = 1.0 ;
            if( efac   != 0.0 ) fimfac /= efac ;

            DSET_BRICK_FACTOR(new_dset,ivout) = (fimfac != 0.0 && fimfac != 1.0)
                                                ? 1.0/fimfac : 0.0 ;
         } else {

            EDIT_coerce_scale_type( nxyz , efac , input_datum  , efim ,
                                                  output_datum , dfim  ) ;
            DSET_BRICK_FACTOR(new_dset,ivout) = 0.0 ;
         }

         mri_free( DSET_BRICK(dset,ival) ) ;
         EDIT_substitute_brick( new_dset , ivout , output_datum , dfim ) ;
      }

      /** Now do the threshold data [won't happen if doall is also happening] **/

      if( output_thdatum >= 0 ){

         ival  = FUNC_ival_thr[    dset->func_type] ;
         ivout = FUNC_ival_thr[new_dset->func_type] ;

         if( input_thdatum == output_thdatum ){

            mri_fix_data_pointer( DSET_ARRAY(dset,ival),DSET_BRICK(new_dset,ivout) ) ;

#if 0
            DSET_BRICK_FACTOR(new_dset,ivout) = DSET_BRICK_FACTOR(dset,ival) ;
#endif

         } else {
            void * dfim , * efim ;

            if( ! MRG_be_quiet ){
               printf("-- coercing threshold datum to be %s\n",
                      MRI_TYPE_name[output_thdatum]);
            }

            efim = DSET_ARRAY(dset,ival) ;
            dfim = (void *) XtMalloc( mri_datum_size(output_thdatum) * nxyz ) ;

            switch( output_thdatum ){
               default: fprintf(stderr,"** illegal output_thdatum = %d\n",
                                output_thdatum);
               exit(1) ;

               case MRI_float:
                  fimfacinv = 0.0 ;
                  fimfac    = DSET_BRICK_FACTOR(dset,ival) ;
                  if( fimfac == 0.0 ){
                     fimfac = (input_thdatum == MRI_short)
                               ? 1.0/FUNC_scale_short[dset->func_type]
                               : (input_thdatum == MRI_byte)
                               ? 1.0/FUNC_scale_byte[dset->func_type] : 0.0 ;
                  }
               break ;

               case MRI_short:
                  if( input_datum == MRI_float ){
                     fimfac    = FUNC_scale_short[new_dset->func_type] ;
                     fimfacinv = 1.0 / fimfac ;
                  } else if( input_datum == MRI_byte ){
                     fimfac    = ((float)FUNC_scale_short[new_dset->func_type])
                                / FUNC_scale_byte[new_dset->func_type] ;
                     fimfacinv = 1.0 / FUNC_scale_short[new_dset->func_type] ;
                  } else {
                     fprintf(stderr,"** illegal input_thdatum = %d\n",input_thdatum);
                     exit(1) ;
                  }
               break ;

               case MRI_byte:
                  if( input_datum == MRI_float ){
                     fimfac    = FUNC_scale_byte[new_dset->func_type] ;
                     fimfacinv = 1.0 / fimfac ;
                  } else if( input_datum == MRI_short ){
                     fimfac    = ((float)FUNC_scale_byte[new_dset->func_type])
                                / FUNC_scale_short[new_dset->func_type] ;
                     fimfacinv = 1.0 / FUNC_scale_byte[new_dset->func_type] ;
                  } else {
                     fprintf(stderr,"** illegal input_thdatum = %d\n",input_thdatum);
                     exit(1) ;
                  }
               break ;
            }

            EDIT_coerce_scale_type( nxyz , fimfac ,
                                    DSET_BRICK_TYPE(dset,ival),efim ,
                                    output_thdatum,dfim ) ;

            DSET_BRICK_FACTOR(new_dset,ivout) = fimfacinv ;
            EDIT_substitute_brick( new_dset , ivout , output_thdatum , dfim ) ;
            mri_free( DSET_BRICK(dset,ival) ) ;
         }
      }

    }  /* iv    End of iteration over sub-bricks */

      if( ! MRG_be_quiet )
         printf("-- Writing edited dataset in files\n"
                "   %s and %s\n",
                new_dset->dblk->diskptr->header_name ,
                new_dset->dblk->diskptr->brick_name    ) ;

      THD_load_statistics( new_dset ) ;
      THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
      exit(0) ;
   }

   /************************************************************************/
   /********         more than one input dataset --> merger         ********/

   /* Nov 1998: make an array of input datasets, load 1st element */

   dsetar = (THD_3dim_dataset **) malloc(sizeof(THD_3dim_dataset *)*file_count);
   for( ii=0 ; ii < file_count ; ii++ ) dsetar[ii] = NULL ;
   dsetar[0] = dset ;

   /* make an empty copy of the first dataset, then modify it */

   new_dset = EDIT_empty_copy( dset ) ;
   ival     = DSET_PRINCIPAL_VALUE(dset) ;

   if( MRG_datum >= 0 ) output_datum = MRG_datum ;
   else                 output_datum = DSET_BRICK_TYPE(dset,ival) ;

   EDIT_dset_items( new_dset ,
                       ADN_prefix , MRG_output_prefix ,
                       ADN_label1 , MRG_output_prefix ,
                       ADN_directory_name , MRG_output_session ,
                    ADN_none ) ;
   strcat( new_dset->self_name , "(MG)" ) ;

   /* 29 Aug 1996: change the dataset type, depending on the merger type */

  if (! MRG_doall)   /* 02 Feb 1998 */
   switch( MRG_cflag_gthr ){
       default:
          EDIT_dset_items( new_dset , ADN_nvals,1 , ADN_ntt,0 , ADN_none ) ;
          if( ISFUNC(dset) )
             EDIT_dset_items( new_dset , ADN_func_type,FUNC_FIM_TYPE , ADN_none ) ;
       break ;

       case THFLAG_FICO:  /* do nothing to the dataset now */
          num_fico = 0 ;
       break ;
   }

   if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
      fprintf(stderr,
              "*** Output file %s already exists -- cannot continue!\n",
              new_dset->dblk->diskptr->header_name ) ;
      exit(1) ;
   }

   if( ! MRG_be_quiet && MRG_keepthr )
      printf("-- ignoring -keepthr option\n") ;


  /* 02 Feb 1998 */
   if (MRG_doall)
     {  iv_bot = 0;  iv_top = DSET_NVALS(dset);  }
   else if( MRG_ivfim >= 0 )                       /* Nov 1998 */
     {  iv_bot = MRG_ivfim ; iv_top = iv_bot+1 ; }
   else
     {  iv_bot = DSET_PRINCIPAL_VALUE(dset);  iv_top = iv_bot + 1;  }

  /*----- Iterate over sub-bricks -----*/

  ivout = 0 ;  /* Nov 1998 */

  for (iv = iv_bot;  iv < iv_top;  iv++)
    {
      if ((!MRG_be_quiet) ) printf ("-- Editing sub-brick %d \n", iv);

      MRG_edopt.iv_fim = iv;

   /* make space for the merger computations */

   tfim = (float *) XtMalloc( sizeof(float) * nxyz ) ;  /* dataset copy */
   gfim = (float *) XtMalloc( sizeof(float) * nxyz ) ;  /* results */
   gnum = (short *) XtMalloc( sizeof(short) * nxyz ) ;  /* counts */
   for( ii=0 ; ii < nxyz ; ii++ ) gfim[ii] = 0.0 ;      /* initialize */
   for( ii=0 ; ii < nxyz ; ii++ ) gnum[ii] = 0 ;

   /* 29 Aug 1996: make space for merger of thresholds, if desired */

   if( MRG_cflag_gthr != THFLAG_NONE ){
      ttfim = (float *) XtMalloc( sizeof(float) * nxyz ) ;  /* thresh copy */
      ggfim = (float *) XtMalloc( sizeof(float) * nxyz ) ;  /* thresh results */
      for( ii=0 ; ii < nxyz ; ii++ ) ggfim[ii] = 0.0 ;      /* initialize */

      for( ii=0 ; ii < MAX_STAT_AUX ; ii++ ) thr_stataux[ii] = 0 ;
   }

   if( ! MRG_be_quiet ){
      float nbytes = (2.0*sizeof(float)+sizeof(short))*nxyz ;
      if( MRG_cflag_gthr != THFLAG_NONE ) nbytes += 2.0*sizeof(float)*nxyz ;
      printf("-- allocated %.1f MB scratch memory\n", nbytes/MEGA ) ;
   }

   /***--- read datasets, edit them, add them into gfim and gnum ---***/

   num_dset = 0 ;
   for( file_num=first_file; file_num < argc ; file_num++ ){

      /** read dataset header if not already input **/

      dset = dsetar[ file_num - first_file ] ;  /* Nov 1998 */
      if( dset == NULL ){
         dset = dsetar[ file_num - first_file ] = THD_open_one_dataset( argv[file_num] ) ;
         if( ! ISVALID_3DIM_DATASET(dset) ){
            fprintf(stderr,"*** cannot open dataset %s\n",argv[file_num]) ; exit(1) ;
         }
      }

      /* check for dimensional mismatch */

      if( dset->daxes->nxx != nx ||
          dset->daxes->nyy != ny || dset->daxes->nzz != nz ){

         fprintf(stderr,"*** dataset brick size mismatch at file %s\n",
                 argv[file_num] ) ;
         exit(1) ;
      }

      /* 02 Feb 1998 */
      if ( (MRG_doall) && (DSET_NVALS(dset) != iv_top) )
         fprintf (stderr, "*** -doall dataset nvals mismatch at file %s\n",
                  argv[file_num]);


      if( DSET_NUM_TIMES(dset) > 1 && (!MRG_doall && MRG_ivfim < 0) ){ /* no time     */
         fprintf(stderr,                                               /* dependence! */
                 "*** cannot use time-dependent dataset %s"
                 " without -doall or -1dindex\n",argv[file_num]) ;
         exit(1) ;
      }

      /* check for dataset type, if needed for the merging operations ordered */

      if( MRG_cflag_gthr == THFLAG_FICO ){

         if( !ISFUNC(dset) ){
            fprintf(stderr,
                  "*** dataset from file %s is anatomical using '-tgfisher'!\n",
                argv[file_num] ) ;
            exit(1) ;
         }

         switch( dset->func_type ){
            default:
               fprintf(stderr,
                "*** dataset from file %s is illegal type using '-tgfisher'!\n",
                   argv[file_num] ) ;
            exit(1) ;

            case FUNC_COR_TYPE:   /* add up degrees-of-freedom */
               num_fico ++ ;
               for( ii=0 ; ii < FUNC_need_stat_aux[FUNC_COR_TYPE] ; ii++ )
                 thr_stataux[ii] += dset->stat_aux[ii] ;
            break ;

            case FUNC_THR_TYPE:  /* do nothing */
            break ;
         }
      }

      /* get the control information about this dataset */

#if 1
      ival = iv ;                  /* Nov 1998 */
#else
      if (MRG_doall)  ival = iv;   /* 02 Feb 1998 */
      else            ival = DSET_PRINCIPAL_VALUE(dset) ;
#endif
      datum = DSET_BRICK_TYPE(dset,ival) ;

      if( ! AFNI_GOOD_FUNC_DTYPE(datum) ){
         fprintf(stderr,"*** Illegal datum for 3dmerge: %s in file %s ***\n" ,
                 MRI_TYPE_name[datum] , argv[file_num] ) ;
         exit(1) ;
      }

      if( ! MRG_be_quiet ){
         printf("-- processing file %s" , argv[file_num] ) ;
         fflush(stdout) ;
      }

      /* mess with the input data, maybe */

      if( MRG_have_edopt )
         EDIT_one_dataset( dset , &MRG_edopt ) ;  /* some real work */
      else
         THD_load_datablock( dset->dblk , NULL ) ;

      if( ! MRG_be_quiet ){ printf(".") ; fflush(stdout) ; }

      /* copy it into tfim , scaling if needed */

      fimfac = DSET_BRICK_FACTOR(dset,ival) ;          /* normal case */

      if( MRG_cflag_g == CFLAG_FISHER             &&   /* special case */
          DSET_BRICK_TYPE(dset,ival) == MRI_short &&
          (fimfac==0.0 || fimfac==1.0)              ){

         fimfac = 1.0 / FUNC_COR_SCALE_SHORT ;
      }

      if( num_dset == 0 ){
         first_fimfac = fimfac ;  /* save for later */
         first_datum  = datum  ;
      }

      /** 08 Jan 1998: check if all inputs are integer types **/

      is_int = is_int && (MRI_IS_INT_TYPE(datum) && fimfac == 0.0) ;

      /* the actual copy+scaling operation */

      EDIT_coerce_scale_type( nxyz , fimfac ,
                              DSET_BRICK_TYPE(dset,ival) , DSET_ARRAY(dset,ival) ,
                              MRI_float , tfim ) ;

      /* 29 Aug 1996: get the threshold data into ttfim , if needed */

      if( MRG_cflag_gthr != THFLAG_NONE && (tval=DSET_THRESH_VALUE(dset)) >= 0 ){

         int thdatum = DSET_BRICK_TYPE(dset,tval) ;

         if( ! AFNI_GOOD_FUNC_DTYPE(thdatum) ){
            fprintf(stderr,"*** Illegal threshold for 3dmerge: %s in file %s ***\n" ,
                    MRI_TYPE_name[thdatum] , argv[file_num] ) ;
            exit(1) ;
         }

         thrfac = DSET_BRICK_FACTOR(dset,tval) ;          /* normal case */

         if( MRG_cflag_gthr == THFLAG_FICO           &&   /* special case */
             DSET_BRICK_TYPE(dset,tval) == MRI_short &&
             (thrfac==0.0 || thrfac==1.0)              ){

            thrfac = 1.0 / FUNC_COR_SCALE_SHORT ;
         }

         EDIT_coerce_scale_type( nxyz , thrfac ,
                                 thdatum , DSET_ARRAY(dset,tval) ,
                                 MRI_float , ttfim ) ;
      }

      DSET_unload(dset) ;  /* Nov 1998: don't need data bricks in memory any more */

      /*** merge tfim into gfim and gnum ***/

      if( MRG_cflag_g == CFLAG_MMAX ){
         for( ii=0 ; ii < nxyz ; ii++ ){
            if( tfim[ii] != 0 ){
               gnum[ii]++ ;
               if( tfim[ii] > gfim[ii] ) gfim[ii] = tfim[ii] ;
            }
         }
      } else if( MRG_cflag_g == CFLAG_AMAX ){
         float dab ;
         for( ii=0 ; ii < nxyz ; ii++ ){
            if( tfim[ii] != 0 ){
               gnum[ii]++ ;
               dab = fabs(tfim[ii]) ;
               if( dab > gfim[ii] ) gfim[ii] = dab ;
            }
         }
      } else if( MRG_cflag_g == CFLAG_SMAX ){
         float dab ;
         for( ii=0 ; ii < nxyz ; ii++ ){
            if( tfim[ii] != 0 ){
               gnum[ii]++ ;
               dab = fabs(tfim[ii]) ;
               if( dab > fabs(gfim[ii]) ) gfim[ii] = tfim[ii] ;
            }
         }
      } else if( MRG_cflag_g == CFLAG_ORDER ){
         for( ii=0 ; ii < nxyz ; ii++ ){
            if( tfim[ii] != 0 ){
               gnum[ii]++ ;
               if( gfim[ii] == 0 ) gfim[ii] = tfim[ii] ;
            }
         }
      } else if( MRG_cflag_g == CFLAG_FISHER ){
         for( ii=0 ; ii < nxyz ; ii++ ){
            if( tfim[ii] != 0 ){
               gnum[ii]++ ; gfim[ii] += ATANH(tfim[ii]) ;
            }
         }
      } else {                                /* default = sum up */
         for( ii=0 ; ii < nxyz ; ii++ ){
            if( tfim[ii] != 0 ){
               gnum[ii]++ ; gfim[ii] += tfim[ii] ;
            }
         }
      }

      /* 29 Aug 1996: merge the threshold data, if any */

      if( MRG_cflag_gthr == THFLAG_FICO ){
         for( ii=0 ; ii < nxyz ; ii++ ) ggfim[ii] += ATANH(ttfim[ii]) ;
      }

      if( ! MRG_be_quiet ){ printf(".\n") ; fflush(stdout) ; }

      num_dset++ ;
   }  /* end of combiner loop over datasets */

   myXtFree(tfim) ;                      /* not needed any more */
   if( ttfim != NULL ) myXtFree(ttfim) ;

   if( MRG_edopt.nfmask > 0 ){
     free(MRG_edopt.fmask) ; MRG_edopt.fmask = NULL ; MRG_edopt.nfmask = 0 ;
   }

   /*** if only one dset encountered, some error! ***/

   if( num_dset <= 1 ){
      fprintf(stderr,"*** Only found 1 dataset -- computations aborted!\n") ;
      exit(1) ;
   }

   if( ! MRG_be_quiet ) printf("-- merging results into sub-brick %d\n",ivout) ;

   /*** now, edit the merged dataset:
        cast out voxels that weren't hit enough ***/

   if( MRG_hits_g > 0 ){
      for( ii=0 ; ii < nxyz ; ii++ )
         if( gnum[ii] < MRG_hits_g ) { gfim[ii] = 0 ; gnum[ii] = 0 ; }
   }

   /*** decide if the output is to be stored as integers ***/
   /*   [at this point, is_int is true if all]
         inputs were integers and unscaled.  ] 08 Jan 1998 */

   switch( MRG_cflag_g ){
      default:          is_int = 0 ; break ;   /* not allowed */

      case CFLAG_COUNT: is_int = 1 ; break ;   /* it IS an integer */

      case CFLAG_MMAX:                         /* if all inputs */
      case CFLAG_SMAX:                         /* are integers, */
      case CFLAG_AMAX:                         /* then one of these */
      case CFLAG_ORDER: break ;                /* will be integer also */
   }

   /*** do the averaging as ordered ***/

   switch( MRG_cflag_g ){
      default: break ;

      case CFLAG_COUNT:
         first_fimfac = 0.0 ;
         for( ii=0 ; ii < nxyz ; ii++ ) gfim[ii] = gnum[ii] ;
      break ;

      case CFLAG_MEAN:
         fac = 1.0 / num_dset ;
         for( ii=0 ; ii < nxyz ; ii++ ) gfim[ii] *= fac ;
      break ;

      case CFLAG_NZMEAN:
         for( ii=0 ; ii < nxyz ; ii++ )
            if( gnum[ii] > 0 ) gfim[ii] /= gnum[ii] ;
      break ;

      case CFLAG_FISHER:
         fac = 1.0 / num_dset ;
         for( ii=0 ; ii < nxyz ; ii++ ) gfim[ii] = TANH( fac * gfim[ii] ) ;
      break ;
   }

   /* 29 Aug 1996: clean up the merged threshold, too */

   switch( MRG_cflag_gthr ){
      default: break ;

      case THFLAG_FICO:
         fac = 1.0 / num_dset ;
         for( ii=0 ; ii < nxyz ; ii++ ) ggfim[ii] = TANH( fac * ggfim[ii] ) ;
      break ;
   }

   /**** at this point, don't need the count brick "gnum" anymore;
         "gfim" contains the results we'll eventually write to disk ****/

   myXtFree( gnum ) ;

   /*** if desired, edit the result for cluster size ***/

   rmm   = MRG_clust_rmm_g ;
   vmul  = MRG_clust_vmul_g ;
   dxyz  = dx*dy*dz ;
   ptmin = vmul / dxyz + 0.99 ;

   if( (rmm >= dx || rmm >= dy || rmm >= dz) && ptmin > 1 ){
      if( ! MRG_be_quiet ) printf("-- editing merger for cluster size\n") ;

      clar  = MCW_find_clusters( nx,ny,nz , dx,dy,dz , MRI_float,gfim , rmm ) ;
      nclu  = 0 ;
      if( clar != NULL ){
         for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
            if( clar->clar[iclu] != NULL && clar->clar[iclu]->num_pt < ptmin ){
               KILL_CLUSTER(clar->clar[iclu]) ;
            } else if( clar->clar[iclu] != NULL ){
               nclu++ ;
            }
         }
      }

      if( nclu > 0 ){
         for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
            if( clar->clar[iclu] != NULL && clar->clar[iclu]->num_pt > 0 )
               MCW_cluster_to_vol( nx,ny,nz , MRI_float,gfim , clar->clar[iclu] ) ;
         }
      }
   }

   /*** scan results for non-zero-ositifulness ***/

   if( !MRG_doall ){  /* Nov 1998 */
      for( ii=0 ; ii < nxyz ; ii++ ) if( gfim[ii] != 0 ) break ;
      if( ii == nxyz ){
         fprintf(stderr,
           "*** Merged dataset has no nonzero entries -- will not write\n" ) ;
         exit(0) ;
      }
   }

   /*** attach new data to output brick      ***/
   /*   [Nov 1998: changed from ival to ivout] */

   switch( output_datum ){

      default:
         fprintf(stderr,
                 "*** Fatal Error ***\n"
                 "*** Somehow ended up with output_datum = %d\n",output_datum) ;
      exit(1) ;

      case MRI_complex:{
         void * dfim ;
         dfim = (void *) XtMalloc( sizeof(complex) * nxyz ) ;
         EDIT_coerce_type( nxyz , MRI_float,gfim , MRI_complex,dfim ) ;
         myXtFree( gfim ) ;
         EDIT_substitute_brick( new_dset , ivout , MRI_complex , dfim ) ;
         DSET_BRICK_FACTOR(new_dset,ivout) = 0.0 ;
      }
      break ;

      case MRI_float:
         EDIT_substitute_brick( new_dset , ivout , MRI_float , gfim ) ;
         DSET_BRICK_FACTOR(new_dset,ivout) = 0.0 ;
      break ;

      case MRI_byte:
      case MRI_short:{
         void * dfim ;
         float gtop ;

         gtop = MCW_vol_amax( nx,ny,nz , MRI_float,gfim ) ;

         if( MRG_cflag_g == CFLAG_FISHER ){
            fimfac = FUNC_COR_SCALE_SHORT ;
         } else if( gtop == 0.0 ||           /* 08 Jan 1998 */
                    MRG_nscale  ||           /* 15 Sep 2000 */
                    (is_int && gtop <= MRI_TYPE_maxval[output_datum]) ){
            fimfac = 0.0 ;
         } else {
            fimfac = MRI_TYPE_maxval[output_datum] / gtop ;
         }

         dfim = (void *) XtMalloc( mri_datum_size(output_datum) * nxyz ) ;
         EDIT_coerce_scale_type( nxyz,fimfac , MRI_float,gfim , output_datum,dfim ) ;
         myXtFree( gfim ) ;
         EDIT_substitute_brick( new_dset , ivout , output_datum , dfim ) ;
         DSET_BRICK_FACTOR(new_dset,ivout) = (fimfac != 0.0) ? 1.0/fimfac : 0.0 ;
      }
      break ;
   }

   /** 29 Aug 1996: attach output threshold, if any **/

   if( MRG_cflag_gthr != THFLAG_NONE ){
      short * dfim ;

      dfim   = (short *) XtMalloc( sizeof(short) * nxyz ) ;
      thrfac = FUNC_COR_SCALE_SHORT ;
      EDIT_coerce_scale_type( nxyz,thrfac , MRI_float,ggfim , MRI_short,dfim ) ;
      myXtFree( ggfim ) ;
      EDIT_substitute_brick( new_dset , DSET_THRESH_VALUE(new_dset) ,
                             MRI_short , dfim ) ;
      DSET_BRICK_FACTOR(new_dset,1) = 1.0/thrfac ;

      /* if all datasets were fico, then output is fico,
         and needs to get the degrees-of-freedom parameters stataux */

      if( num_fico == num_dset ){
         (void) EDIT_dset_items( new_dset , ADN_stat_aux,thr_stataux , ADN_none ) ;

      /* some datasets were fith, so output is fith */

      } else {
          EDIT_dset_items( new_dset , ADN_func_type,FUNC_THR_TYPE , ADN_none ) ;
      }
   }

   ivout++ ;

 }  /* iv    End of iteration over sub-bricks */


   /*** write to disk!!! ***/

   if( ! MRG_be_quiet )
      printf("-- Writing merged dataset in files\n"
             "   %s and %s\n",
             new_dset->dblk->diskptr->header_name ,
             new_dset->dblk->diskptr->brick_name    ) ;

   tross_Make_History( "3dmerge" , argc , argv , new_dset ) ;
   THD_load_statistics( new_dset ) ;
   THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
   exit(0) ;
}
