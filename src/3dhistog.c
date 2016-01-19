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

static EDIT_options HI_edopt ;

#define NBIN_SPECIAL 65536
#define NBIN_DEFAULT 100
#define BIG_NUMBER 9.999e+37

#define HI_UNKNOWN 0
#define HI_INTOUT 1
#define HI_FLOATOUT 2

static int   HI_nopt ;
static int   HI_nbin = -1 ;
static int   HI_log  = 0 ;

static int     HI_dind  = -1 ;   /* 23 Sep 1998 */
static int     HI_nomit = 0 ;
static float * HI_omit  = NULL ;
static int     HI_notit = 0 ;
static int     HI_doall = 0 ;    /* 16 Feb 2005 */
static int     HI_noempty = 0 ;  /*  7 Aug 2015 [rickr] */

static byte  * HI_mask      = NULL ;
static int     HI_mask_nvox = 0 ;
static int     HI_mask_hits = 0 ;

static double  HI_min = BIG_NUMBER;
static double  HI_max = -BIG_NUMBER;

static char *  HI_unq = NULL;
static char *  HI_ni = NULL;

static int     HI_datatype = HI_UNKNOWN; /* DRG Aug 2011 */

static short * HI_roi_unq = NULL; /* ZSS March 2011 */
static int     HI_N_roi_unq = 0;
static THD_3dim_dataset *HI_roi=NULL;

static int     HI_pdf = 0;        /* ZSS Sept 2012 */

static int integral_dset(THD_3dim_dataset *dset, int iv_bot, int iv_top);

#define KEEP(x) ( (HI_nomit==0) ? 1 :  \
                  (HI_nomit==1) ? ((x) != HI_omit[0]) : HI_keep(x) )

#define CEIL_CHECK(x) ( use_ceil ? ceil(x) : (x) )

static int HI_igfac = 0 ;  /* 06 Jun 2013 [RWCox] */

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

void usage_3dhistog(int detail)
{
   printf(
    "Compute histogram of 3D Dataset\n"
    "Usage: 3dhistog [editing options] [histogram options] dataset\n"
    "\n"
    "The editing options are the same as in 3dmerge\n"
    " (i.e., the options starting with '-1').\n"
    "\n"
    "The histogram options are:\n"
    "  -nbin #   Means to use '#' bins [default=100]\n"
#if 0
    "            Special Case: for short or byte dataset bricks,\n"
    "                          set '#' to zero to have the number\n"
    "                          of bins set by the brick range.\n"
#endif
    "  -dind i   Means to take data from sub-brick #i, rather than #0\n"
    "  -omit x   Means to omit the value 'x' from the count;\n"
    "              -omit can be used more than once to skip multiple values.\n"
    "  -mask m   Means to use dataset 'm' to determine which voxels to use\n"
    "  -roi_mask r Means to create a histogram for each non-zero value in \n"
    "              dataset 'r'. If -mask option is also used, dataset 'r' is \n"
    "              masked by 'm' before creating the histograms.\n"
    "  -doall    Means to include all sub-bricks in the calculation;\n"
    "              otherwise, only sub-brick #0 (or that from -dind) is used.\n"
    "  -noempty  Only output bins that are not empty.\n"
    "            This does not apply to NIML output via -prefix.\n"
    "  -notitle  Means to leave the title line off the output.\n"
    "  -log10    Output log10() of the counts, instead of the count values.\n"
    "            This option cannot be used with -pdf or with -prefix\n"
    "  -pdf      Output the counts divided by the number of samples.\n"
    "            This option is only valid with -prefix\n"
    "  -min x    Means specify minimum (inclusive) of histogram.\n"
    "  -max x    Means specify maximum (inclusive) of histogram.\n"
    "  -igfac    Means to ignore sub-brick scale factors and histogram-ize\n"
    "              the 'raw' data in each volume.\n"
    "\n"
    "  Output options for integer and floating point data\n"
    "  By default, the program will determine if the data is integer or float\n"
    "   even if the data is stored as shorts with a scale factor.\n"
    "  Integer data will be binned by default to be 100 or the maximum number of\n"
    "   integers in the range, whichever is less. For example, data with the range\n"
    "   (0..20) gives 21 bins for each integer, and non-integral bin boundaries\n"
    "   will be raised to the next integer (2.3 will be changed to 3, for instance).\n"
    "  If the number of bins is higher than the number of integers in the range,\n"
    "   the bins will be labeled with floating point values, and multiple bins\n"
    "   may be zero between the integer values\n"
    "  Float data will be binned by default to 100 bins with absolute limits for\n"
    "   the min and max if these are specified as inclusive. For example,\n"
    "   float data ranging from (0.0 to 20.0) will be binned into bins that\n"
    "   are 0.2 large  (0..0.199999, 0.2..0.399999,...,19.8..20.0)\n"
    "  To have bins divided at 1.0 instead, specify the number of bins as 20\n"
    "   Bin 0 is 0..0.9999, Bin 1 is 1.0 to 1.9999, ..., Bin 20 is 19 to 20.0000\n"
    "   giving a slight bias to the last bin\n"
    "\n"
    "  -int      Treat data and output as integers\n"
    "  -float    Treat data and output as floats\n"
    "  -unq U.1D Writes out the sorted unique values to file U.1D.\n"
    "            This option is not allowed for float data\n"
    "            If you have a problem with this, write\n"
    "            Ziad S. Saad (saadz@mail.nih.gov)\n"
    "  -prefix HOUT: Write a copy of the histogram into file HOUT.1D\n"
    "                you can plot the file with:\n"
    "             1dplot -hist -sepscl -x HOUT.1D'[0]' HOUT.1D'[1,2]' \n"
    "        or   \n"
    "             1dRplot -input HOUT.1D\n"
    "\n"
    "Without -prefix, the histogram is written to stdout.  \n"
    "Use redirection '>' if you want to save it to a file.\n"
    "The format is a title line, then three numbers printed per line:\n"
    "  bottom-of-interval  count-in-interval  cumulative-count\n"
    "\n"
    "There is no 1dhistog program, for the simple reason that you can use\n"
    "this program for the same purpose, as in this example:\n"
    "  3dhistog -nbin 50 -notitle -min 0 -max .01 err.1D > ehist.1D\n"
    "  1dplot -hist -x ehist.1D'[0]' -xlabel 'err.1D' -ylabel 'histo' ehist.1D'[1]'\n"
    "\n"
    "-- by RW Cox, V Roopchansingh, and ZS Saad\n"
   ) ;

   if (detail) printf("\n" MASTER_SHORTHELP_STRING ) ;

   PRINT_COMPILE_DATE ;
   return;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int iarg, i_roi_unq;
   THD_3dim_dataset *dset ;

   int nx,ny,nz , nxyz , ii , kk , nopt , nbin ;
   float fbot , ftop, temp_fbot, temp_ftop ;
   /* removed ibot, itop              10 Jun 2011 [rcr,drg] */
   int   use_ceil;
   void *vfim = NULL;
   int64_t *fbin=NULL ;
   float df , dfi ;
   float fval ;

   float fimfac;
   int iv_fim, fim_type;
   int64_t cumfbin;
   int iv_bot , iv_top ;
   FILE *fout = NULL;
   int n_unq=0;


   mainENTRY("3dhistog") ; machdep() ; AFNI_logger("3dhistog",argc,argv) ;
   PRINT_VERSION("3dhistog") ;

   if(argc == 1){ usage_3dhistog(1); exit(0); } /* Bob's help shortcut */

   HI_read_opts( argc , argv ) ;
   nopt = HI_nopt ;

   if( nopt >=  argc ) HI_syntax("no dset argument?") ;

   /* check if user has requested number of bins */
   nbin = (HI_nbin < 0) ? NBIN_DEFAULT : HI_nbin ;

   iarg = nopt ;
   dset = THD_open_dataset( argv[iarg] ) ;
   if( dset == NULL ){
     fprintf(stderr,"** ERROR: Can't open dataset %s\n",argv[iarg]) ;
     exit(1) ;
   }
   if( HI_igfac )
     EDIT_dset_items( dset , ADN_brick_fac,NULL , ADN_none ) ;   /* 06 Jun 2013 */

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

   /* assume all sub-bricks have the same type */
   fim_type = DSET_BRICK_TYPE(dset,iv_bot) ;

   /* find global min and max of data in all used bricks */

   fbot = BIG_NUMBER ;
   ftop = -fbot ;

   /* assume rounding of bin bottom limit for integer data only*/
   if(HI_datatype == HI_UNKNOWN){
      if(integral_dset(dset, iv_bot, iv_top)) {
         HI_datatype = HI_INTOUT;
      }
      else
      HI_datatype = HI_FLOATOUT;
   }

   if(HI_datatype == HI_INTOUT) {
     use_ceil = 1 ;
   }
   else {
      use_ceil = 0 ;
   }

   /* renamed from minmax_dset to THD_slow_minmax_dset, and moved to */
   /* thd_info.c                                 18 Dec 2012 [rickr] */
   THD_slow_minmax_dset(dset, &temp_fbot, &temp_ftop, iv_bot, iv_top);

   if(HI_min != BIG_NUMBER) {
     fbot = HI_min;
   }
   else {
     fbot = temp_fbot;
   }

   if(HI_max != -BIG_NUMBER) {
     ftop = HI_max;
   }
   else {
     ftop = temp_ftop;
   }

   if( fbot >= ftop ){
     fprintf(stderr,"** ERROR: all values in dataset are = %f!\n",fbot) ;
     exit(1) ;
   }

   if( fbot > temp_ftop){
     WARNING_message(
        "minimum value to display is higher than maximum value in dataset!");
   }

   if( ftop < temp_fbot){
     WARNING_message(
        "maximum value to display is less than minimum value in dataset!");
   }

   if((HI_nbin<0) && (HI_datatype == HI_INTOUT))
      nbin = ftop-fbot+1;

   switch( fim_type ){
      default:
        fprintf(stderr,"** ERROR: can't process data of this type!\n") ;
      exit(1) ;

      case MRI_byte:
      case MRI_short:
        /* 10 Jun 2011 */
#if 0
        if((HI_nbin==NBIN_SPECIAL) && use_ceil && ((ftop-fbot)<100))
        /* integral data, calculate the default number of bins differently */
        if((HI_nbin<0) && (use_ceil))
               nbin = ftop-fbot+1;
#endif
      break ;

      case MRI_float:
        if (HI_unq) {
         fprintf(stderr,"** ERROR: Unique operation not allowed for float data.\n") ;
         exit(1) ;
        }
      break ;
   }
   df  = (ftop-fbot) / nbin ;
   if(df==0.0) dfi = 1;
   else dfi = 1.0 / df ;

   fbin = (int64_t *) calloc( sizeof(int64_t) , nbin ) ;
   if( fbin == NULL ) HI_syntax("can't allocate histogram array!") ;

   if (HI_roi) {
      if (DSET_NVOX(HI_roi) != nxyz) {
         fprintf(stderr,
            "** ERROR: Grid mismatch\n"
            "-roi_mask dset has %d voxels while dataset has %d\n",
            DSET_NVOX(HI_roi) ,  nxyz);
         exit(1);
      }
      if (HI_mask) { /* apply mask to HI_roi */
         short *c=DSET_ARRAY(HI_roi,0);
         for( ii=0 ; ii < nxyz ; ii++ ){
            if (!HI_mask[ii]) c[ii] = 0;
         }
      } else { /* Need a mask to play with */
         HI_mask = (byte *)calloc(sizeof(byte), nxyz);
      }
   }

   i_roi_unq = 0;
   do {
      if (HI_roi) {
         short *c=DSET_ARRAY(HI_roi,0);
         if (!HI_roi_unq[i_roi_unq]) {
            ++i_roi_unq;
            continue; /* skipping zero ROI */
         }
         /* prepare mask */
         HI_mask_hits = 0;
         for( ii=0 ; ii < nxyz ; ii++ ){
            if (c[ii] == HI_roi_unq[i_roi_unq]) {
               HI_mask[ii] = 1; ++HI_mask_hits;
            } else {
               HI_mask[ii] = 0;
            }
         }
         /* flush fbin */
         memset(fbin, 0, sizeof(int64_t)*nbin);
         /* fprintf(stderr,"Processing ROI %d\n", HI_roi_unq[i_roi_unq]); */
      }
      /* loop over all bricks and accumulate histogram */
      if (HI_unq) {
         fout = fopen(HI_unq,"r");
         if (fout) {
            fclose(fout);
            if (!THD_ok_overwrite()) {
               fprintf(stderr,
                        "** ERROR: Output file %s exists, will not overwrite.\n",
                        HI_unq) ;
               exit(1) ;
            }
         }
         fout = fopen(HI_unq,"w");
         if (!fout) {
            fprintf(stderr,
                     "** ERROR: Could not open %s for write operation.\n"
                     "Check your directory permissions\n", HI_unq) ;
         exit(1) ;
         }
      }
      if (!fout) HI_unq = NULL; /* safety valve */

   for( iv_fim=iv_bot ; iv_fim <= iv_top ; iv_fim++ ){
     fimfac = DSET_BRICK_FACTOR(dset,iv_fim) ;
     if (fimfac == 0.0)  fimfac = 1.0;
     vfim = DSET_ARRAY(dset,iv_fim) ;
     fim_type = DSET_BRICK_TYPE(dset,iv_fim) ;

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
                if(kk>=nbin) kk = nbin-1;
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
               for (ii=0; ii<n_unq; ++ii)
                  if (KEEP(funq[ii])) fprintf(fout,"%d\n", funq[ii]);
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
                if(kk>=nbin) kk = nbin-1;
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
               for (ii=0; ii<n_unq; ++ii)
                  if (KEEP(funq[ii])) fprintf(fout,"%d\n", funq[ii]);
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
                if(kk>=nbin) kk = nbin-1;
                fbin[kk]++ ;
              }
            }
          }
          break ;
        }  /* end of switch on data brick type */

        if (!HI_roi) DSET_unload_one(dset,iv_fim) ;
      }

      /*** print something ***/

      if (!HI_ni) {
         cumfbin = 0;
         if( HI_log ){
           if( ! HI_notit )
             printf ("%12s %13s %13s\n",
                     "#Magnitude", "Log_Freq", "Log_Cum_Freq");

           for( kk=0 ; kk < nbin ; kk++ ){
             if( HI_noempty && !fbin[kk] ) continue;  /* 7 Aug 2015 */
             cumfbin += fbin[kk];
             if((use_ceil) && (nbin <= (ftop-fbot+1)))
                printf ("%12d %13.6f %13.6f\n",
                     (int) ceil(fbot+kk*df),
                     log10((double)fbin[kk]+1.0), log10((double)cumfbin+1.0));
             else
                printf ("%12.6f %13.6f %13.6f\n",
                     (fbot+kk*df),
                     log10((double)fbin[kk]+1.0), log10((double)cumfbin+1.0));
           }
         } else {
           if( ! HI_notit )
             printf ("%12s %13s %13s\n",  "#Magnitude", "Freq", "Cum_Freq");

           for( kk=0 ; kk < nbin ; kk++ ){
             if( HI_noempty && !fbin[kk] ) continue;  /* 7 Aug 2015 */
             cumfbin += fbin[kk];
             if((use_ceil) && (nbin <= (ftop-fbot+1)))
                printf ("%12d %13lld %13lld\n",
                     (int) ceil(fbot+kk*df), (long long)fbin[kk], (long long)cumfbin);
             else
                printf ("%12.6f %13lld %13lld\n",
                     (fbot+kk*df), (long long)fbin[kk], (long long)cumfbin);
           }
         }
      } else { /*                ZSS Dec. 2010 */
         NI_stream ns=NULL;
         NI_element *hni=NULL;
         char sstr[strlen(HI_ni)+64];
         float *bb =(float* )calloc(nbin,sizeof(float) );
         double *cf=(double*)calloc(nbin,sizeof(double));
         double *bd=(double*)calloc(nbin,sizeof(double));

         if (HI_log) {
            ERROR_message("Option -log10 not available with -prefix");
            exit(1);
         }

         cumfbin = 0 ;
         for( kk=0 ; kk < nbin ; kk++ ){
          cumfbin += fbin[kk];
          cf[kk] = (double)cumfbin;
          bd[kk] = (double)fbin[kk];
          bb[kk] = CEIL_CHECK(fbot+kk*df);
         }
         if (HI_pdf) {
            for( kk=0 ; kk < nbin ; kk++ ){
               bd[kk] /= cf[nbin-1];
               cf[kk] /= cf[nbin-1];
            }
         }
         hni = NI_new_data_element("3dhistog", nbin);
         NI_add_column(hni, NI_FLOAT , bb);
         NI_add_column(hni, NI_DOUBLE, bd);
         NI_add_column(hni, NI_DOUBLE, cf);

              if (!HI_log && !HI_pdf)
            NI_set_attribute(hni, "ColumnLabels",
                        "Magnitude ; Freq ; Cum_Freq");
         else if (!HI_log && HI_pdf)
            NI_set_attribute(hni, "ColumnLabels",
                        "Magnitude ; PDF ; CDF");
         sprintf(sstr,"%f",df);
         NI_set_attribute(hni, "BinWidth", sstr);

         if (HI_roi) {
            sprintf(sstr,"file:%s.%03d.1D", HI_ni, HI_roi_unq[i_roi_unq]);
         } else {
            sprintf(sstr,"file:%s.1D", HI_ni);
         }
         if (!(ns = NI_stream_open(sstr,"w"))) {
            ERROR_message("Failed to open stream %s\n", sstr);
            exit(1);
         }
         NI_write_element(ns,hni,NI_TEXT_MODE | NI_HEADERSHARP_FLAG );
         NI_stream_close(ns);
         NI_free_element(hni);
         free(bb); free(cf); free(bd);
      }
      ++i_roi_unq;
   } while (i_roi_unq < HI_N_roi_unq);
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
   int  ival;

   INIT_EDOPT( &HI_edopt ) ;
   HI_unq = NULL;
   HI_datatype = HI_UNKNOWN;

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcmp(argv[nopt],"-help") == 0 || strcmp(argv[nopt],"-h") == 0){
        usage_3dhistog(strlen(argv[nopt])>3 ? 2:1);
        exit(0);
      }

      /**** check for editing options ****/

      ival = EDIT_check_argv( argc , argv , nopt , &HI_edopt ) ;
      if( ival > 0 ){ nopt += ival ; continue ; }

      if( strncmp(argv[nopt],"-nbin",5) == 0 ){
        HI_nbin = strtol( argv[++nopt] , NULL , 10 ) ;
        if( HI_nbin == 0 ) HI_nbin = NBIN_SPECIAL ;
        else if( HI_nbin < 1 ) HI_syntax("illegal value of -nbin!") ;
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

      if( strncmp(argv[nopt],"-notit",6) == 0 ){
         HI_notit = 1 ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-log10",6) == 0 ){
         HI_log = 1 ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) HI_syntax("need argument after -prefix!") ;
         HI_ni = argv[nopt] ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-pdf",6) == 0 ){
         HI_pdf = 1 ;
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

      if( strncmp(argv[nopt],"-roi_mask",5) == 0 )
      {
          THD_3dim_dataset * rset ;
          nopt++ ;

          if( nopt >= argc ) HI_syntax("need argument after -roi_mask!") ;

          rset = THD_open_dataset( argv[nopt] ) ;
          if( rset == NULL ) HI_syntax("can't open -roi_mask dataset!") ;
          DSET_load( rset ) ;  CHECK_LOAD_ERROR(rset) ;
          if (DSET_NVALS(rset)!=1) {
            HI_syntax("-roi_mask must have one sub-brick");
          }
          HI_roi = EDIT_empty_copy(rset);
          EDIT_dset_items( HI_roi ,
                          ADN_nvals, 1,
                          ADN_ntt, 1,
                          ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                          ADN_none ) ;
          EDIT_substscale_brick( HI_roi, 0, DSET_BRICK_TYPE(rset,0),
                                 DSET_ARRAY(rset,0), MRI_short, 1.0);
          /* There can be a leak here if rset is not short
             But that's OK */
          /* Now get unique values */
          HI_roi_unq = UniqueShort(DSET_ARRAY(HI_roi,0), DSET_NVOX(HI_roi),
                                    &HI_N_roi_unq, 0);
          if (!HI_roi_unq) {
             fprintf(stderr,"** ERROR: Failed to uniquate.\n") ;
             exit(1) ;
          }
          fprintf(stdout,"# %d unique values in roi_mask %s\n",
                              HI_roi_unq[0] ? HI_N_roi_unq:HI_N_roi_unq-1,
                              argv[nopt] );
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

      if( strncmp(argv[nopt],"-igfac",5) == 0 ){
         HI_igfac = 1 ; nopt++ ; continue ;
      }


      if( strcmp(argv[nopt],"-int") == 0 ){
         HI_datatype = HI_INTOUT ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-float") == 0 ){
         HI_datatype = HI_FLOATOUT ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-noempty") == 0 ){
         HI_noempty = 1 ;
         nopt++ ; continue ;
      }


      /**** unknown switch ****/

      fprintf(stderr,"*** unrecognized option %s\a\n",argv[nopt]) ;
      suggest_best_prog_option(argv[0], argv[nopt]);
      exit(-1) ;

   }  /* end of loop over options */

#ifdef HIDEBUG
printf("*** finished with options\n") ;
#endif

   if( argc < 2 ){
      ERROR_message("Too few options, try -help for details");
      exit(1);
   }

   if (HI_log && HI_pdf) {
      ERROR_message("Option -log10 not available with -pdf");
      exit(1);
   }

   if (HI_log && HI_ni) {
      ERROR_message("Option -log10 not available with -prefix");
      exit(1);
   }

   if (HI_pdf && !HI_ni) {
      ERROR_message("Option -pdf needs -prefix");
      exit(1);
   }

   if( HI_doall ){
     if( HI_dind >= 0 ){
       fprintf(stderr,"** WARNING: -dind ignored with -doall!\n") ;
       HI_dind = -1 ;
     }
   }

   HI_nopt = nopt ;
   return ;
}

/*----------------------------------------------------------------------------*/
/* do simple check to see if data is all integer */
static int
integral_dset(THD_3dim_dataset *dset, int iv_bot, int iv_top)
{
   int iv_fim, fim_type;
   float fimfac;

   for( iv_fim=iv_bot ; iv_fim <= iv_top ; iv_fim++ ){
     fim_type = DSET_BRICK_TYPE(dset,iv_fim) ;
     fimfac = DSET_BRICK_FACTOR(dset,iv_fim) ;
     if (fimfac == 0.0)  fimfac = 1.0;

     /* test if all the data is integral  - used for calculating bins below */
     if( (fim_type == MRI_short) || (fim_type == MRI_byte )){
        if( fimfac != 1.0 ) return(0) ;
     }
     if( fim_type == MRI_float) return(0);
   }
   /* made it through all sub-bricks without float factor or datatype */
   /*  then it must be integer */
   return(1);
}

/*----------------------------------------------------------------------------*/
/* get min and max of data in range of sub-bricks of dataset */
static int
minmax_dset(THD_3dim_dataset *dset, float *dmin, float *dmax, int iv_bot, int iv_top)
{
   int iv_fim;
   float fimfac;
   float vbot , vtop, temp_fbot, temp_ftop ;

   temp_fbot = BIG_NUMBER ;
   temp_ftop = -temp_fbot ;

   for( iv_fim=iv_bot ; iv_fim <= iv_top ; iv_fim++ ){
     /* minimum and maximum for sub-brick */
     vbot = mri_min( DSET_BRICK(dset,iv_fim) ) ;
     vtop = mri_max( DSET_BRICK(dset,iv_fim) ) ;
     fimfac = DSET_BRICK_FACTOR(dset,iv_fim) ;
     if (fimfac == 0.0)  fimfac = 1.0;
     vbot *= fimfac ; vtop *= fimfac ;

     /* update global min and max */
     if( vbot < temp_fbot ) temp_fbot = vbot;
     if( vtop > temp_ftop ) temp_ftop = vtop;
   }
   *dmin = temp_fbot;
   *dmax = temp_ftop;
   return(0);
}
