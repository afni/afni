/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include <string.h>

double DSET_cor( THD_3dim_dataset *, THD_3dim_dataset *, byte *, int ,
                 double *,double *,double *,double *,double *,int * ) ;
double DSET_eta2(THD_3dim_dataset *, THD_3dim_dataset *, byte *, int * ) ;
double FPTR_cor( float *, float *, int , byte *, int ,
                 double *,double *,double *,double *,double *,int * ) ;
double FPTR_eta2(float *, float *, int , byte *, int * ) ;
double FPTR_dice(float *, float *, int , byte *, int * ) ;
float * get_float_dset_data_pointer( THD_3dim_dataset * , int , int * , int) ;

void usage_3ddot(int detail) {
      printf(
"Usage: 3ddot [options] dset1 [dset2 dset3 ...]\n"
"Output = correlation coefficient between sub-brick pairs\n"
"         All datasets on the command line will get catenated\n"
"         at loading time and should all be on the same grid.\n"
"         - you can use sub-brick selectors on the dsets\n"
"         - the result is a number printed to stdout\n"
"Options:\n"
"  -mask mset   Means to use the dataset 'mset' as a mask:\n"
"                 Only voxels with nonzero values in 'mset'\n"
"                 will be averaged from 'dataset'.  Note\n"
"                 that the mask dataset and the input dataset\n"
"                 must have the same number of voxels.\n"
"  -mrange a b  Means to further restrict the voxels from\n"
"                 'mset' so that only those mask values\n"
"                 between 'a' and 'b' (inclusive) will\n"
"                 be used.  If this option is not given,\n"
"                 all nonzero values from 'mset' are used.\n"
"                 Note that if a voxel is zero in 'mset', then\n"
"                 it won't be included, even if a < 0 < b.\n"
"  -demean      Means to remove the mean from each volume\n"
"                 prior to computing the correlation.\n"
"  -docor       Return the correlation coefficient (default).\n"
"  -dodot       Return the dot product (unscaled).\n"
"  -docoef      Return the least square fit coefficients\n"
"                 {a,b} so that dset2 is approximately a + b*dset1\n"
"  -dosums      Return the 6 numbers xbar=<x> ybar=<y>\n"
"                 <(x-xbar)^2> <(y-ybar)^2> <(x-xbar)(y-ybar)> \n"
"                 and the correlation coefficient.\n"
"  -doeta2      Return eta-squared (Cohen, NeuroImage 2008).\n"
"  -dodice      Return the Dice coefficient (the Sorensen-Dice index).\n"
"  -show_labels Print sub-brick labels to help identify what \n"
"               is being correlated. This option is useful when\n"
"               you have more than 2 sub-bricks at input.\n"
"  -upper       Compute upper triangular matrix\n"
"  -full        Compute the whole matrix. A waste of time, but handy\n"
"               for parsing.\n"
"  -1D          Comment headings in order to read in 1D format.\n"
"               This is only useful with -full.\n"
"  -NIML        Write output in NIML 1D format. Nicer for plotting.\n"
"               -full and -show_labels are automatically turned on with -NIML.\n"
"               For example: \n"
"                    3ddot -NIML anat.001.sc7z.sigset+orig\"[0,1,2,3,4]\" \\\n"
"                                                                > corrmat.1D\n"
"                    1dRplot corrmat.1D \n"
"           or\n"
"                    1dRplot -save somecorr.jpg -i corrmat.1D\n"
"\n"
"  Note: This program is not efficient when more than two subbricks are input.\n"
"\n"   ) ;

      printf("\n" MASTER_SHORTHELP_STRING ) ;

      PRINT_COMPILE_DATE ;   
      return;
}

int main( int argc , char * argv[] )
{
   double dxy , xbar,ybar,xxbar,yybar,xybar ;
   int narg , ndset , nvox , demean=0 , mode=0 , nnn ,
       xar_new, yar_new, nsub, iii, jjj, mxlen=0, ShowLabels = 0,
       modelen = 0, OneD=0, iii1, jjj0, half = 1, ils = 1;
   THD_3dim_dataset * mask_dset=NULL , *cset=NULL;
   float mask_bot=666.0 , mask_top=-666.0, *xar=NULL, *yar=NULL;
   byte * mmm=NULL ;
   char *catname=NULL, form[256], val[256], *modelabel=NULL;
   
   mainENTRY("3dLocalstat main"); machdep();
   
   /*-- read command line arguments --*/
   
   if( argc == 1){ usage_3ddot(1); exit(0); } /* Bob's help shortcut */

   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){
     if( strcmp(argv[narg],"-help") == 0 || strcmp(argv[narg],"-h") == 0){
        usage_3ddot(strlen(argv[narg])>3 ? 2:1);
        exit(0);
     }

      if( strcmp(argv[narg],"-show_labels") == 0 ){
         ShowLabels = 1 ; narg++ ; continue ;
      }
      if( strcmp(argv[narg],"-1D") == 0 ){
         OneD = 1 ; narg++ ; continue ;
      }
      if( strcmp(argv[narg],"-NIML") == 0 ){
         half = 0 ; ShowLabels = 1 ;
         OneD = 2 ; narg++ ; continue ;
      }
      if( strcmp(argv[narg],"-full") == 0 ){
         half = 0 ; narg++ ; continue ;
      }
      if( strcmp(argv[narg],"-upper") == 0 ){
         half = 1 ; narg++ ; continue ;
      }
      if( strncmp(argv[narg],"-demean",5) == 0 ){
         demean++ ; narg++ ; continue ;
      }
      if( strcmp(argv[narg],"-dodot") == 0 ){
         mode = 1 ; narg++ ; continue ;
      }
      if( strcmp(argv[narg],"-docor") == 0 ){
         mode = 0 ; narg++ ; continue ;
      }
      if( strcmp(argv[narg],"-docoef") == 0 ){
         mode = 2 ; narg++ ; continue ;
      }
      if( strcmp(argv[narg],"-dosums") == 0 ){
         mode = 3 ; narg++ ; continue ;
      }
      if( strcmp(argv[narg],"-doeta2") == 0 ){
         mode = 4 ; narg++ ; continue ;
      }
      if( strcmp(argv[narg],"-dodice") == 0 ){
         mode = 5 ; narg++ ; continue ;
      }
      if( strncmp(argv[narg],"-mask",5) == 0 ){
         if( mask_dset != NULL ){
            fprintf(stderr,"*** Cannot have two -mask options!\n") ; exit(1) ;
         }
         if( narg+1 >= argc ){
            fprintf(stderr,"*** -mask option requires a following argument!\n");
             exit(1) ;
         }
         mask_dset = THD_open_dataset( argv[++narg] ) ;
         if( mask_dset == NULL ){
            fprintf(stderr,"*** Cannot open mask dataset!\n") ; exit(1) ;
         }
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex ){
            fprintf(stderr,
                    "*** Cannot deal with complex-valued mask dataset!\n");
            exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mrange",5) == 0 ){
         if( narg+2 >= argc ){
           fprintf(stderr,
                   "*** -mrange option requires 2 following arguments!\n") ;
           exit(1) ;
         }
         mask_bot = strtod( argv[++narg] , NULL ) ;
         mask_top = strtod( argv[++narg] , NULL ) ;
         if( mask_top < mask_top ){
           fprintf(stderr,"*** -mrange inputs are illegal!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      fprintf(stderr,"*** Unknown option: %s\n",argv[narg]) ; 
      suggest_best_prog_option(argv[0], argv[narg]);
      exit(1) ;
   }

   if( argc < 3 ){
      ERROR_message("Too few options, try -help for details");
      exit(1);
   }
   
   if( mode >= 2 ) demean = 1 ;

   for (jjj=0, iii=narg; iii<argc; ++iii) {
      jjj += (2+strlen(argv[iii]));
   }
   nsub = 0;
   catname = (char *)calloc(jjj, sizeof(char));
   for (iii=narg; iii<argc; ++iii) {
      strcat(catname, argv[iii]);
      strcat(catname, " ");
   }
   if (!(cset = THD_open_dataset( catname ))) {
      fprintf(stderr,"*** Failed to read catenation of %s\n", catname); exit(1) ;
   }
   nsub = DSET_NVALS(cset);
   if (nsub < 2) {
      fprintf(stderr,"*** Need at least two sub-bricks in input!\n") ; exit(1) ;
   }
   nvox = DSET_NVOX(cset) ;
   
   /* make a byte mask from mask dataset */

   if( mask_dset != NULL ){
      int mcount ;
      if( DSET_NVOX(mask_dset) != nvox ){
         fprintf(stderr,
                 "*** Input and mask datasets are not same dimensions!\n");
         exit(1) ;
      }
      mmm = THD_makemask( mask_dset , 0 , mask_bot,mask_top ) ;
      mcount = THD_countmask( nvox , mmm ) ;
      fprintf(stderr,"+++ %d voxels in the mask\n",mcount) ;
      if( mcount <= 5 ){
         fprintf(stderr,"*** Mask is too small!\n");exit(1);
      }
      DSET_delete(mask_dset) ;
   }
   
   /* compute output string lengths */
   switch( mode ){
      default: modelen = 12; modelabel = "correlation"; break;
      case 1: modelen = 12; modelabel = "dot product"; break;
      case 2: modelen = 12*2; modelabel = "a+by"; break;
      case 3: modelen = 12*6; 
           modelabel = "<x> <y> <(x-<x>)^2> <(y-<y>)^2> <(x-<x>)(y-<y>)> cor";
              break;
      case 4: modelen = 12; modelabel = "Eta2"; break;
   }
   
   if (ShowLabels) {
      DSET_load(cset); CHECK_LOAD_ERROR(cset);
      if (half) ils = 1;
      else ils = 0;
      mxlen = strlen(DSET_BRICK_LABEL(cset, 0));
      if (mxlen < modelen) mxlen = modelen;
      for (iii=ils; iii<nsub; ++iii) {
         if (iii==ils && OneD) jjj = strlen(DSET_BRICK_LABEL(cset, iii))+1;
         else jjj = strlen(DSET_BRICK_LABEL(cset, iii));
         if (jjj > mxlen) 
            mxlen = jjj;
      }
      sprintf(form,"%%%ds%c", mxlen, '\t');
      if (OneD == 1 || OneD == 0) {
         if (OneD) printf("#");
         for (iii=ils; iii<nsub; ++iii) {
            printf(form, DSET_BRICK_LABEL(cset, iii));
         }
         sprintf(val,":%s:", modelabel);
         printf(form, val);
         printf("\n");
      } else if (OneD == 2) {
         printf("#<3ddot\n# ni_type = \"%d*double\"\n# ni_dime = \"%d\"\n", 
                  nsub, nsub);
         printf("# Measure = \"%s\"\n",modelabel); 
         printf("# ColumnLabels = \"");
         for (iii=ils; iii<nsub; ++iii) {
            if (iii > ils) printf( " ; ");
            printf("%s", DSET_BRICK_LABEL(cset, iii));
         }
         printf("%s", "\"");
         printf("\n");
         printf("# CommandLine = \"");
         for (iii=0; iii<argc; ++iii) {
            printf("%s ", argv[iii]);
         }
         printf("\"\n# >\n");
      }
   } else {
      if (nsub == 2) { 
         sprintf(form,"%%s%c",'\t');
      } else {
         sprintf(form,"%%%ds%c",modelen, '\t'); 
      }
   }
   if (half) {
      iii1 = nsub-1;
   } else {
      iii1 = nsub;
   }
   for (iii=0; iii<iii1; ++iii) {
      xar = get_float_dset_data_pointer(cset, iii, &xar_new, 0);
   if (half) {
      jjj0 = iii+1;
      for (jjj=0; jjj<iii; ++jjj) printf(form," ");
   } else {
      jjj0 = 0;
   }
   for (jjj=jjj0; jjj<nsub; ++jjj) {
      yar = get_float_dset_data_pointer(cset, jjj, &yar_new, 0);   
      /* mode 4 is special: eta^2                     16 Jun 2011 [rickr] */
      if ( mode == 4 )      dxy = FPTR_eta2( xar , yar , nvox,  mmm , &nnn ) ;
      else if ( mode == 5 ) dxy = FPTR_dice( xar , yar , nvox,  mmm , &nnn ) ;
      else                  dxy = FPTR_cor ( xar , yar , nvox,  mmm , demean, 
                                       &xbar,&ybar,&xxbar,&yybar,&xybar, &nnn );

      if( nnn == 0 ) ERROR_exit("Can't compute for some reason!") ;
      if( nnn == 1 ) fprintf(stderr,"** only 1 masked voxel?\n") ;
      switch( mode ){
        default: snprintf(val, 255, "%g",dxy) ;  break ;

        case 1:  snprintf(val, 255, "%g",xybar*nnn) ; break ;

        case 2:{
          double a,b ;
          b = xybar / xxbar ;
          a = ybar - b*xbar ;
          snprintf(val, 255, "%g %g",a,b) ;
        }
        break ;

        case 3:
         /* the old version had no break for case 3 so it printed also dxy
         from case 4. I will mimic this behavior here too to keep things
         consistent. Help will be updated to match */
          snprintf(val, 255,"%g %g %g %g %g %g",xbar,ybar,xxbar,yybar,xybar,dxy);
          break ; 
      }
      if (OneD == 0 || OneD == 1) {
         printf(form,val);
      } else {
         printf("%s ", val);
      }
      if (yar_new) free(yar); yar=NULL;
   }/* jjj */
      
      if (ShowLabels) {
         if (OneD == 1) {
            printf("#");
         } else if (OneD == 0) {
            printf(form, DSET_BRICK_LABEL(cset, iii));
         }
      }
      printf("\n");
      if (xar_new) free(xar); xar=NULL;
   }/* iii */
   if (OneD == 2) {
      printf("# </matrix>\n");
   }  
   exit(0) ;
}

/*------------------------------------------------------------------*/

#undef  ASSIF
#define ASSIF(p,v) do{ if( (p)!=NULL ) *(p)=(v) ; } while(0)

double DSET_eta2( THD_3dim_dataset *xset, THD_3dim_dataset *yset,
                  byte *mmm , int *npt )
{
   double e2 ;
   float *fxar , *fyar;
   int fxar_new, fyar_new, nxyz;

   nxyz = DSET_NVOX(xset) ;


   /* load bricks */

   fxar = get_float_dset_data_pointer(xset, 0, &fxar_new, 1);
   fyar = get_float_dset_data_pointer(yset, 0, &fyar_new, 1);
   if ( ! fxar || ! fyar ) ERROR_exit("Cannot get float pointers!") ;
   

   e2 = FPTR_eta2(fxar, fyar, nxyz, mmm, npt);
   
   if( fxar_new ) free(fxar) ;
   if( fyar_new ) free(fyar) ;
   
   return(e2) ;
}

double FPTR_eta2( float *fxar, float *fyar, int nxyz, byte *mmm , int *npt )
{
   double e2 ;
   int ii , nnn ;

   ASSIF(npt,0) ;
   
   if ( ! fxar || ! fyar ) ERROR_exit("Cannot get float pointers!") ;

   if( npt ) { /* then count applied voxels (masked or all) */
      if( mmm ) {
         for( nnn=ii=0 ; ii < nxyz ; ii++ ) if( mmm[ii] ) nnn++;
      } else nnn = nxyz ;
      ASSIF(npt, nnn) ;
   }

   /* actual work: get eta^2 */
   e2 = THD_eta_squared_masked(nxyz, fxar, fyar, mmm);

   return e2 ;
}

/* dice: one word diff from FPTR_eta2    28 Oct, 2015 [rickr] */
double FPTR_dice( float *fxar, float *fyar, int nxyz, byte *mmm , int *npt )
{
   double e2 ;
   int ii , nnn ;

   ASSIF(npt,0) ;
   
   if ( ! fxar || ! fyar ) ERROR_exit("Cannot get float pointers!") ;

   if( npt ) { /* then count applied voxels (masked or all) */
      if( mmm ) {
         for( nnn=ii=0 ; ii < nxyz ; ii++ ) if( mmm[ii] ) nnn++;
      } else nnn = nxyz ;
      ASSIF(npt, nnn) ;
   }

   /* actual work: get eta^2 */
   e2 = THD_dice_coef_f_masked(nxyz, fxar, fyar, mmm);

   return e2 ;
}

double DSET_cor( THD_3dim_dataset *xset,
                 THD_3dim_dataset *yset, byte *mmm , int dm,
                 double *xbar, double *ybar,
                 double *xxbar, double *yybar, double *xybar , int *npt )
{
   double dxy ;
   float *fxar , *fyar ;
   int nxyz , fxar_new, fyar_new  ;

   nxyz = DSET_NVOX(xset) ;

   /* load bricks */

   fxar = get_float_dset_data_pointer(xset, 0, &fxar_new, 1);
   fyar = get_float_dset_data_pointer(yset, 0, &fyar_new, 1);
   if ( ! fxar || ! fyar ) ERROR_exit("Cannot get float pointers!") ;

   dxy = FPTR_cor(fxar, fyar, nxyz, mmm, dm, 
                   xbar, ybar, xxbar, yybar, xybar, npt);
   
   /* toss trash */

   if( fxar_new ) free(fxar) ;
   if( fyar_new ) free(fyar) ;

   return(dxy);
}

double FPTR_cor( float *fxar, 
                 float *fyar, int nxyz, byte *mmm , int dm,
                 double *xbar, double *ybar,
                 double *xxbar, double *yybar, double *xybar , int *npt )
{
   double sumxx , sumyy , sumxy , tx,ty , dxy ;
   double meanx=0.0 , meany=0.0 ;
   void  *xar , *yar ;
   int ii , ivx,ivy , itypx,itypy  , nnn ;


   ASSIF(npt,0) ; ASSIF(xbar,0.0) ; ASSIF(ybar,0.0) ;
   ASSIF(xxbar,0.0) ; ASSIF(yybar,0.0) ; ASSIF(xybar,0.0) ;

   if ( ! fxar || ! fyar ) ERROR_exit("Cannot get float pointers!") ;

   /* 29 Feb 2000: remove mean? */

   sumxx = sumyy = 0.0 ;
   for( nnn=ii=0 ; ii < nxyz ; ii++ ){
     if( mmm == NULL || mmm[ii] ){sumxx += fxar[ii]; sumyy += fyar[ii]; nnn++;}
   }
   if( nnn < 5 ) return 0.0 ;             /* ERROR */
   sumxx /= nnn ; sumyy /= nnn ;
   ASSIF(xbar,sumxx) ; ASSIF(ybar,sumyy) ; ASSIF(npt,nnn) ;
   meanx = sumxx ; meany = sumyy ;        /* save for later */

   /* modifying dset data causes a segmentation fault based on permission
      (seen on Fedora Core 5 and FreeBSD 7.2):
         Process terminating with default action of signal 11 (SIGSEGV)
          Bad permissions for mapped region at address 0x5055B74
            at 0x407642: DSET_cor (3ddot.c:238)
            by 0x4068D9: main (3ddot.c:146)
      
      --> leave the data as is; apply means upon read   16 Sep 2009 [rickr] */
#if 0
   if( dm ){
     for( ii=0 ; ii < nxyz ; ii++ ){
       if( mmm == NULL || mmm[ii] ){ fxar[ii] -= sumxx; fyar[ii] -= sumyy; }
     }
   }
#endif

   /* compute sums */

   sumxx = sumyy = sumxy = 0.0 ;
   for( ii=0 ; ii < nxyz ; ii++ ){
     if( mmm == NULL || mmm[ii] ){
       if( dm ) { tx = fxar[ii]-meanx ; ty = fyar[ii]-meany ; }
       else     { tx = fxar[ii] ;       ty = fyar[ii] ; }
       sumxx += tx * tx ; sumyy += ty * ty ; sumxy += tx * ty ;
     }
   }
   sumxx /= nnn ; ASSIF(xxbar,sumxx) ;
   sumyy /= nnn ; ASSIF(yybar,sumyy) ;
   sumxy /= nnn ; ASSIF(xybar,sumxy) ;

   /* compute result */

   dxy = sumxx * sumyy ; if( dxy <= 0.0 ) return 0.0 ;

   dxy = sumxy / sqrt(dxy) ; return dxy ;
}

/* return the data pointer (if float) or a pointer to converted data,
 * with a flag to specify whether conversion to float happened
 *   - index is the sub-brick to read             16 Jun 2011 [rickr]
 */
float * get_float_dset_data_pointer( THD_3dim_dataset * dset,
                                     int index, int * dnew, int purge )
{
   int     nxyz, dtype ;
   void  * dar;
   float * fdar; 

   nxyz = DSET_NVOX(dset) ;

   DSET_load(dset); CHECK_LOAD_ERROR(dset);
   dtype = DSET_BRICK_TYPE(dset,index) ;
   dar   = DSET_ARRAY(dset,index) ; if( dar == NULL ) return NULL ;
   if( dtype == MRI_float ){
     fdar = (float *) dar ; ASSIF(dnew, 0) ;
   } else {
     fdar = (float *) malloc( sizeof(float) * nxyz ) ; ASSIF(dnew, 1) ;
     EDIT_coerce_type( nxyz , dtype,dar , MRI_float,fdar ) ;
     if (purge) PURGE_DSET( dset ) ; 
   }

   return fdar;
}

