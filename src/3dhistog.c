/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
  This program generates a histogram for the input AFNI dataset.

  Mod:   2 June 1997
         Added option to generate histogram for only those voxels which are
         above the operator specified threshold value.

  Mod:   5 Dec 2000, by Vinai Roopchansingh, to include -mask option

  Mod:   16 Feb 2005, RWCox: remove threshold stuff, and add -doall.

  Mod:   19 May 2005, dglen: added -min/-max options

  Mod:   15 Dec 2005, rickr: fixed use of sub-brick factors

  Mod:   28 Apr 2006, rickr: fixed min/max range setting kk outside array
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "mrilib.h"
#include "thd_ttatlas_query.h"

static EDIT_options HI_edopt ;

#define NBIN_SPECIAL 65536
#define BIG_NUMBER 9.999e+37

static int   HI_nopt ;
static int   HI_nbin = 100 ;
static int   HI_log  = 0 ;

static int     HI_dind  = -1 ;   /* 23 Sep 1998 */
static int     HI_nomit = 0 ;
static float * HI_omit  = NULL ;
static int     HI_notit = 0 ;
static int     HI_doall = 0 ;    /* 16 Feb 2005 */

static byte  * HI_mask      = NULL ;
static int     HI_mask_nvox = 0 ;
static int     HI_mask_hits = 0 ;

static double  HI_min = BIG_NUMBER;
static double  HI_max = -BIG_NUMBER;

static char *  HI_unq = NULL;

#define KEEP(x) ( (HI_nomit==0) ? 1 :  \
                  (HI_nomit==1) ? ((x) != HI_omit[0]) : HI_keep(x) )

void HI_read_opts( int , char ** ) ;
#define HI_syntax(str) \
  do{ fprintf(stderr,"\n** ERROR: %s\a\n",str) ; exit(1) ; } while(0)

/*---------------------------------------------------------------------------------*/

int HI_keep(float x)  /* check if value x is in the omitted list */
{
   register int ii ;
   for( ii=0 ; ii < HI_nomit ; ii++ ) if( x == HI_omit[ii] ) return 0 ;
   return 1 ;
}

/*---------------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int iarg ;
   THD_3dim_dataset *dset ;

   int nx,ny,nz , nxyz , ii , kk , nopt , nbin ;
   float fbot , ftop ;
   int   ibot , itop , has_fac; /* to deal with multiple short sub-bricks */
   int *fbin=NULL , *tbin=NULL ;
   float df , dfi ;
   float fval ;

   float fimfac;
   int iv_fim, fim_type;
   byte    *bfim = NULL ;
   short   *sfim = NULL ;
   float   *ffim = NULL ;
   void    *vfim = NULL ;
   long cumfbin, cumtbin;
   int iv_bot , iv_top ;
   float vbot , vtop ;
   FILE *fout = NULL;
   int n_unq=0;

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ){
      printf("Compute histogram of 3D Dataset\n"
             "Usage: 3dhistog [editing options] [histogram options] dataset\n"
             "\n"
             "The editing options are the same as in 3dmerge\n"
             " (i.e., the options starting with '-1').\n"
             "\n"
             "The histogram options are:\n"
             "  -nbin #   Means to use '#' bins [default=100]\n"
             "            Special Case: for short or byte dataset bricks,\n"
             "                          set '#' to zero to have the number\n"
             "                          of bins set by the brick range.\n"
             "  -dind i   Means to take data from sub-brick #i, rather than #0\n"
             "  -omit x   Means to omit the value 'x' from the count;\n"
             "              -omit can be used more than once to skip multiple values.\n"
             "  -mask m   Means to use dataset 'm' to determine which voxels to use\n"
             "  -doall    Means to include all sub-bricks in the calculation;\n"
             "              otherwise, only sub-brick #0 (or that from -dind) is used.\n"
             "  -notit    Means to leave the title line off the output.\n"
             "  -log10    Output log10() of the counts, instead of the count values.\n"
             "  -min x    Means specify minimum of histogram.\n"
             "  -max x    Means specify maximum of histogram.\n"
             "  -unq U.1D Writes out the sorted unique values to file U.1D.\n"
             "            This option is not allowed for float data\n"
             "            If you have a problem with this, write\n"
             "            Ziad S. Saad (saadz@mail.nih.gov)\n"
             "\n"
             "The histogram is written to stdout.  Use redirection '>' if you\n"
             "want to save it to a file.  The format is a title line, then\n"
             "three numbers printed per line:\n"
             "  bottom-of-interval  count-in-interval  cumulative-count\n"
             "\n"
             "-- by RW Cox (V Roopchansingh added the -mask option)\n"
         ) ;

      printf("\n" MASTER_SHORTHELP_STRING ) ;

      PRINT_COMPILE_DATE ; exit(0) ;
   }

   HI_read_opts( argc , argv ) ;
   nopt = HI_nopt ;

   if( nopt >=  argc ) HI_syntax("no dset argument?") ;

   fbin = (int *) calloc( sizeof(int) , HI_nbin ) ;
   if( fbin == NULL ) HI_syntax("can't allocate histogram array!") ;

   iarg = nopt ;
   dset = THD_open_dataset( argv[iarg] ) ;
   if( dset == NULL ){
     fprintf(stderr,"** ERROR: Can't open dataset %s\n",argv[iarg]) ;
     exit(1) ;
   }

   if( (HI_mask_nvox > 0) && (HI_mask_nvox != DSET_NVOX(dset)) )
     HI_syntax("mask and input dataset bricks don't match in size!") ;

   DSET_mallocize( dset ) ;
   DSET_load( dset ) ;  CHECK_LOAD_ERROR(dset) ;
   EDIT_one_dataset( dset , &HI_edopt ) ;  /* edit value in memory */

   nx = dset->daxes->nxx ;
   ny = dset->daxes->nyy ;
   nz = dset->daxes->nzz ; nxyz = nx * ny * nz ;

   if( HI_doall ){
     iv_bot = 0 ; iv_top = DSET_NVALS(dset)-1 ;
     if( !THD_datum_constant(dset->dblk) )
       ERROR_exit("Dataset %s doesn't have same datum type in all sub-bricks!",
                  argv[iarg]) ;
   } else {
     iv_bot = (HI_dind >= 0) ? HI_dind
                             : DSET_IS_MASTERED(dset) ? 0
                                                      : DSET_PRINCIPAL_VALUE(dset) ;
     iv_top = iv_bot ;
     if( iv_bot < 0 || iv_bot >= DSET_NVALS(dset) )
       ERROR_exit("Sub-brick index %d out of range for dataset %s",
                  iv_bot , argv[iarg] ) ;
   }
   fim_type = DSET_BRICK_TYPE(dset,iv_bot) ;

   /* find global min and max of data in all used bricks */

   fbot = BIG_NUMBER ; ftop = -fbot ;
   itop = -32768 ; ibot = 32767 ;
   has_fac = 0 ;
   for( iv_fim=iv_bot ; iv_fim <= iv_top ; iv_fim++ ){
     vbot = mri_min( DSET_BRICK(dset,iv_fim) ) ;
     vtop = mri_max( DSET_BRICK(dset,iv_fim) ) ;
     fimfac = DSET_BRICK_FACTOR(dset,iv_fim) ; if (fimfac == 0.0)  fimfac = 1.0;

     /* if short, get range before applying factor */
     if( fim_type == MRI_short || fim_type == MRI_byte ){
        if( fimfac != 1.0 ) has_fac = 1 ;
        if( vbot < ibot ) ibot = vbot ;
        if( vtop > itop ) itop = vtop ;
     }

     vbot *= fimfac ; vtop *= fimfac ;
     if( vbot < fbot ) fbot = vbot;
     if( vtop > ftop ) ftop = vtop;
   }

   if(HI_min != BIG_NUMBER)
     fbot = HI_min;

   if(HI_max != -BIG_NUMBER)
     ftop = HI_max;

   if( fbot >= ftop ){
     fprintf(stderr,"** ERROR: all values in dataset are = %f!\n",fbot) ;
     exit(1) ;
   }
   switch( fim_type ){
      default:
        fprintf(stderr,"** ERROR: can't process data of this type!\n") ;
      exit(1) ;

      case MRI_byte:
      case MRI_short:
        nbin = (int)(itop-ibot+1.0) ;  /* ftop -> stop (for unscaled range) */
        if( nbin > HI_nbin ) nbin = HI_nbin ;
        if( nbin < 2       ) nbin = 2 ;
      break ;

      case MRI_float:
        nbin = (HI_nbin==NBIN_SPECIAL) ? 100 : HI_nbin ;
        if (HI_unq) {
         fprintf(stderr,"** ERROR: Unique operation not allowed for float data.\n") ;
      exit(1) ;
        }
      break ;
   }
   df  = (ftop-fbot) / (nbin-1) ;
   dfi = 1.0 / df ;

   /* loop over all bricks and accumulate histogram */
   if (HI_unq) {
      fout = fopen(HI_unq,"r");
      if (fout) {
         fclose(fout);
         fprintf(stderr,"** ERROR: Output file %s exists, will not overwrite.\n", HI_unq) ;
      exit(1) ;
      }
      fout = fopen(HI_unq,"w");
      if (!fout) {
         fprintf(stderr,"** ERROR: Could not open %s for write operation.\nCheck your directory permissions\n", HI_unq) ;
      exit(1) ;
      }
   }
   if (!fout) HI_unq = NULL; /* safety valve */

   for( iv_fim=iv_bot ; iv_fim <= iv_top ; iv_fim++ ){
     fimfac = DSET_BRICK_FACTOR(dset,iv_fim) ;
     if (fimfac == 0.0)  fimfac = 1.0;
     vfim = DSET_ARRAY(dset,iv_fim) ;

     switch( fim_type ){

       case MRI_short:{
         short *fim = (short *)vfim ;
         short *funq=NULL;
         for( ii=0 ; ii < nxyz ; ii++ ){
           fval = fim[ii]*fimfac ;
           /* make sure we stay in range       28 Apr 2006 [rickr] */
           if( (fval >= fbot && fval <= ftop) &&
                KEEP(fval) && (HI_mask == NULL || HI_mask[ii]) ){
             kk = (int)( (fval-fbot)*dfi ) ; /* use real value */
             fbin[kk]++ ;
           }
         }
         if (HI_unq) {
            funq = UniqueShort(fim, nxyz, &n_unq, 0);
            if (!funq) {
               fprintf(stderr,"** ERROR: Failed to uniquate.\n") ;
               exit(1) ;
            }
            fprintf(fout,"# %d unique values in %s\n", n_unq, argv[iarg] );
            for (ii=0; ii<n_unq; ++ii) fprintf(fout,"%d\n", funq[ii]);
            fclose(fout); fout = NULL;
            free(funq); funq = NULL;
         }
      }
      break ;

       case MRI_byte:{
         byte *fim = (byte *)vfim ;
         byte *funq=NULL;
         for( ii=0 ; ii < nxyz ; ii++ ){
           fval = fim[ii]*fimfac ;
           if( (fval >= fbot && fval <= ftop) &&
                KEEP(fval) && (HI_mask == NULL || HI_mask[ii]) ){
             kk = (int)( (fval-fbot)*dfi ) ;
             fbin[kk]++ ;
           }
         }
         if (HI_unq) {
            funq = UniqueByte(fim, nxyz, &n_unq, 0);
            if (!funq) {
               fprintf(stderr,"** ERROR: Failed to uniquate.\n") ;
               exit(1) ;
            }
            fprintf(fout,"# %d unique values in %s\n", n_unq, argv[iarg] );
            for (ii=0; ii<n_unq; ++ii) fprintf(fout,"%d\n", funq[ii]);
            fclose(fout); fout = NULL;
            free(funq); funq = NULL;
         }
       }
       break ;

       case MRI_float:{
         float *fim = (float *)vfim ;
         for( ii=0 ; ii < nxyz ; ii++ ){
	   fval = fim[ii]*fimfac ;  /* thanks to Tom Holroyd for noticing this*/
           if( (fval >= fbot && fval <= ftop) &&
                KEEP(fval) && (HI_mask == NULL || HI_mask[ii]) ){
             kk = (int)( (fval-fbot)*dfi ) ;
             fbin[kk]++ ;
           }
         }
       }
       break ;
     }  /* end of switch on data brick type */

     DSET_unload_one(dset,iv_fim) ;
   }

   /*** print something ***/

   cumfbin = 0;

   if( HI_log ){
     if( ! HI_notit )
       printf ("%12s %13s %13s\n", "#Magnitude", "Log_Freq", "Log_Cum_Freq");

     for( kk=0 ; kk < nbin ; kk++ ){
       cumfbin += fbin[kk];
       printf ("%12.6f %13.6f %13.6f\n",
               fbot+kk*df,
               log10((double)fbin[kk]+1.0), log10((double)cumfbin+1.0));
     }
   } else {
     if( ! HI_notit )
       printf ("%12s %13s %13s\n",  "#Magnitude", "Freq", "Cum_Freq");

     for( kk=0 ; kk < nbin ; kk++ ){
       cumfbin += fbin[kk];
       printf ("%12.6f %13d %13ld\n",
               fbot+kk*df, fbin[kk], cumfbin);
     }
   }
   exit(0) ;
}


/*--------------------------------------------------------------------
   read the arguments, and load the global variables
----------------------------------------------------------------------*/

#ifdef HIDEBUG
#  define DUMP1 fprintf(stderr,"ARG: %s\n",argv[nopt])
#  define DUMP2 fprintf(stderr,"ARG: %s %s\n",argv[nopt],argv[nopt+1])
#  define DUMP3 fprintf(stderr,"ARG: %s %s %s\n",argv[nopt],argv[nopt+1],argv[nopt+2])
#else
#  define DUMP1
#  define DUMP2
#  define DUMP3
#endif

void HI_read_opts( int argc , char * argv[] )
{
   int nopt = 1 ;
   float val ;
   int  ival , kk ;

   INIT_EDOPT( &HI_edopt ) ;
   HI_unq = NULL;
   while( nopt < argc && argv[nopt][0] == '-' ){

      /**** check for editing options ****/

      ival = EDIT_check_argv( argc , argv , nopt , &HI_edopt ) ;
      if( ival > 0 ){ nopt += ival ; continue ; }

      if( strncmp(argv[nopt],"-nbin",5) == 0 ){
        HI_nbin = strtol( argv[++nopt] , NULL , 10 ) ;
        if( HI_nbin < 10 && HI_nbin != 0 ) HI_syntax("illegal value of -nbin!") ;
        if( HI_nbin == 0 ) HI_nbin = NBIN_SPECIAL ;
        nopt++ ; continue ;
      }
      if( strncmp(argv[nopt],"-unq",4) == 0 ){
        nopt++;
        if (nopt == argc) {
         HI_syntax("Need 1D filename after -unq");
        }
        HI_unq = argv[nopt];
        nopt++ ; continue ;
      }
      if( strncmp(argv[nopt],"-dind",5) == 0 ){
        HI_dind = strtol( argv[++nopt] , NULL , 10 ) ;
        if( HI_dind < 0 ) HI_syntax("illegal value of -dind!") ;
        nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-doall",6) == 0 ){
        HI_doall = 1 ;
        nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-omit",5) == 0 ){
         char *cpt ; float val ;
         val = strtod( argv[++nopt] , &cpt ) ;
         if( cpt != NULL && *cpt != '\0' ) HI_syntax("illegal value of -omit!") ;
         HI_nomit++ ;
         if( HI_nomit == 1 )
            HI_omit = (float *) malloc( sizeof(float) ) ;
         else
            HI_omit = (float *) realloc( HI_omit , sizeof(float)*HI_nomit ) ;
         HI_omit[HI_nomit-1] = val ;
        nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-notit",5) == 0 ){
         HI_notit = 1 ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-log10",5) == 0 ){
         HI_log = 1 ;
         nopt++ ; continue ;
      }

      /* ----- -mask ----- */

      if( strncmp(argv[nopt],"-mask",5) == 0 )
      {
          THD_3dim_dataset * mset ; int ii,mc ;
          nopt++ ;

          if( nopt >= argc ) HI_syntax("need argument after -mask!") ;

          mset = THD_open_dataset( argv[nopt] ) ;
          if( mset == NULL ) HI_syntax("can't open -mask dataset!") ;
          HI_mask = THD_makemask( mset , 0 , 1.0,0.0 ) ;
          if( HI_mask == NULL ) HI_syntax("can't use -mask dataset!") ;

          HI_mask_nvox = DSET_NVOX(mset) ;
          DSET_delete(mset) ;

          for( ii=mc=0 ; ii < HI_mask_nvox ; ii++ ) if( HI_mask[ii] ) mc++ ;

          if( mc == 0 ) HI_syntax("mask is all zeros!") ;

          fprintf(stderr,"++ %d voxels in mask\n",mc) ;
          HI_mask_hits = mc ;
          nopt++ ; continue ;

      }

      if( strncmp(argv[nopt],"-min",4) == 0 ){
         HI_min = strtod( argv[++nopt] , NULL ) ;
         nopt++ ; continue ;
      }


      if( strncmp(argv[nopt],"-max",4) == 0 ){
         HI_max = strtod( argv[++nopt] , NULL ) ;
         nopt++ ; continue ;
      }


      /**** unknown switch ****/

      fprintf(stderr,"*** unrecognized option %s\a\n",argv[nopt]) ;
      exit(-1) ;

   }  /* end of loop over options */

#ifdef HIDEBUG
printf("*** finished with options\n") ;
#endif

   if( HI_doall ){
     if( HI_dind >= 0 ){
       fprintf(stderr,"** WARNING: -dind ignored with -doall!\n") ;
       HI_dind = -1 ;
     }
   }

   HI_nopt = nopt ;
   return ;
}
