/*** program COVMAT:
     inputs:  (optional) covariance matrix file,
              reference time series file,
              a sequence of image files.
     output:  (updated) covariance matrix file, with all vectors
                correlated above threshold "added" into covariance matrix,
              (optional) file with all such vectors saved for display

     Uses libmri.a and mrilib.h (MRI file library routines).

     RW Cox, December 1993
***/

/*** modified January 1994 to include FIM-like "ignore" locations in
     reference time series.  The mechanism is to use a weighted inner
     product everywhere -- now, the weights are 0 or 1, but can easily
     be modified later.
***/

/****************************************************************************/

#include <mrilib.h>
#include <string.h>

#define NMAX 1050   /* maximum length of time series of images */

/***************************************************************************/

/*** detrend:
     routine to remove unwanted components from time series
***/

void detrend( int n , float vec[] , float wt[] )
{
   register int ii ;
   float sum0 , sum1 , sum2 , det , cf,lf ;

   static int first = 1 ;             /* initialization flag */
   static float cf0,cf1 , lf0,lf1 ;   /* to be initialized */

 /*** initialize coefficients for detrending ***/

   if( first ){
      first = 0 ;
      sum0 = sum1 = sum2 = 0.0 ;
      for( ii=0 ; ii < n ; ii++ ){
         sum0 += wt[ii] ;
         sum1 += wt[ii] * ii ;
         sum2 += wt[ii] * ii*ii ;
      }
      det = sum0 * sum2 - sum1 * sum1 ;
      cf0 =  sum2 / det ;     /* constant factor for sum0 */
      cf1 = -sum1 / det ;     /* constant factor for sum1 */
      lf0 = cf1 ;             /* linear factor for sum0 */
      lf1 =  sum0 / det ;     /* linear factor for sum1 */
   }

 /*** remove mean and linear trend ***/

   sum0 = sum1 = 0.0 ;
   for( ii=0 ; ii < n ; ii++ ){
      sum0 += wt[ii] * vec[ii] ;
      sum1 += wt[ii] * vec[ii] * ii ;
   }

   cf = cf0 * sum0 + cf1 * sum1 ;
   lf = lf0 * sum0 + lf1 * sum1 ;
   for( ii=0 ; ii < n ; ii++ ) vec[ii] -= cf + ii*lf ;

   return ;
}

/**************************************************************************/

/*** normalize:
     routine to scale a time series to unit vector
***/

void normalize( int n , float vec[] , float wt[] )
{
   register int ii ;
   float sqsum ;

   detrend( n , vec , wt ) ;

   sqsum = 0.0 ;
   for( ii=0 ; ii < n ; ii++ ) sqsum += wt[ii] * vec[ii] * vec[ii] ;

   if( sqsum < 0.000001 ){
      for( ii=0 ; ii < n ; ii++ ) vec[ii] = 0.0 ;
   } else {
      sqsum = 1.0 / sqrt(sqsum) ;
      for( ii=0 ; ii < n ; ii++ ) vec[ii] *= sqsum ;
   }

   return ;
}

/*************************************************************************/

/*** the main enchilada ***/

void main( int argc , char *argv[] )
{
   MRI_IMAGE *inim[NMAX] , *tempim ;
   short *inar[NMAX] ;
   int ii,jj,joff,ijoff , kk , narg , nimage , nx , ny , nimax=NMAX ;
   float cthresh = 0.5 ;
   float scale , fmax,fmin , sum ;

   FILE *reffile , *covfile , *savefile=NULL ;
   float ref[NMAX] , vec[NMAX] , org[NMAX] ;
   float wt[NMAX] ;                           /* Jan 1994 addition */
   int   lref = 0 , lcov = 0 ;
   char  buf[128] , cfname[128] , sfname[128] ;
   char *cchh ;

   double *vsum , *vsqsum ;
   int    numv , mvlen , lx,my , moff ;
   int    cvtype = 2 , cvinp ;               /* Jan 1994 addition */

   int ldetrend = FALSE , lmedfilt = FALSE , lsave = FALSE ;

 /*** inputs ***/

   narg = 1 ;   /* argument counter */
   kk   = 0 ;   /* image counter */
   do {

    /*** deal with switches ***/

      if( argc < 4 || strncmp(argv[narg],"-help",2) == 0 ){
         fprintf( stderr ,
           "Usage: %s [-nim number] [-pcnt threshold] [-detrend] [-noquad] \n" ,
           argv[0] ) ;
         fprintf( stderr ,
           "       [-medfilt] [-save savefile] cov_file ref_file im_files\n" );
         exit(0) ;
      }

      if( strncmp(argv[narg],"-save",4) == 0 ){
         strcpy( sfname , argv[++narg] ) ;
         lsave = TRUE ;
         continue ;
      }

      if( strncmp(argv[narg],"-detrend",4) == 0 ){
         ldetrend = TRUE ;
         continue ;
      }

      if( strncmp(argv[narg],"-noquad",4) == 0 ){
         cvtype = 1 ;
         continue ;
      }

      if( strncmp(argv[narg],"-medfilt",4) == 0 ){
         lmedfilt = TRUE ;
         continue ;
      }

      if( strncmp(argv[narg],"-pcnt",4) == 0 ){
         cthresh = strtod( argv[++narg] , NULL ) ;
         if( cthresh > 1 ) cthresh /= 100.0 ;
         continue ;
      }

      if( strncmp(argv[narg],"-nim",4) == 0 ){
         nimax = strtol( argv[++narg] , NULL , 0 ) ;
         if( nimax > NMAX || nimax < 9 ) nimax = NMAX ;
         continue ;
      }

      if( strncmp(argv[narg],"-",1) == 0 ){
         fprintf( stderr , "unknown switch %s\n" , argv[narg] ) ;
         exit(1) ;
      }

    /*** deal with files ***/

      if( !lcov ){                               /* 1st file = cov matrix */
         covfile = fopen( argv[narg] , "r" ) ;
         strcpy( cfname , argv[narg] ) ;
         if( covfile == NULL ){
            lcov = -1 ;
         } else {
            lcov = 1 ;
         }
         continue ;
      }

      if( !lref ){                            /* 2nd file = ref time series */
         if( cthresh <= 0.0 ){
            fprintf( stderr , "skipping ref file %s\n" , argv[narg] ) ;
            reffile == NULL ;
         } else {
            reffile = fopen( argv[narg] , "r" ) ;
            if( reffile == NULL ){
               fprintf( stderr , "cannot open ref file %s\n" , argv[narg] ) ;
               exit(1) ;
            }
         }
         lref = 1 ;
         continue ;
      }

      tempim = mri_read( argv[narg] ) ;    /* rest of files = images */

      if( tempim == NULL ) exit(1) ;       /* check for errors on read */
      if( kk >= nimax ){
         fprintf( stderr , "max #files exceeded at file # %d\n" , kk+1 ) ;
         exit(1) ;
      }

      if( kk == 0 ){           /* 1st image file ==> do some initializations */

         nx = tempim->nx ;     /* save dimensions */
         ny = tempim->ny ;

         fmin = mri_min( tempim ) ;   /* compute a scale factor */
         fmax = mri_max( tempim ) ;
         if( fmin >= fmax ){
            fprintf( stderr , "1st image is constant!\n" ) ;
            exit(1) ;
         }
         scale = 10000.0 / (fmax-fmin) ;

      } else if( nx != tempim->nx || ny != tempim->ny ){  /* check dimensions */
         fprintf( stderr ,
                  "file shape doesn't match first image: %s\n" , argv[narg] ) ;
         exit(1) ;
      }

      if( tempim->kind == MRI_short ){   /* convert all inputs to shorts */
         inim[kk] = tempim ;
      } else {
         inim[kk] = mri_to_short( scale , tempim ) ;
         mri_free( tempim ) ;
      }
      inar[kk] = mri_data_pointer( inim[kk] ) ;   /* pointer to data */
      kk++ ;
      continue ;

   } while( ++narg < argc && kk < nimax ) ;  /* end of loop over arguments */

 /*** check for reasonable inputs at this point */

   if( !lcov || !lref || kk < 9 ){
      fprintf( stderr , "enough files not given!\n" ) ;
      exit(1) ;
   }

   nimage = kk ;

 /*** read ref file now ***/

   if( cthresh > 0.0 ){
      for( kk=0 ; kk < nimage ; kk++ ){
         cchh = fgets( buf , 100 , reffile ) ;
         if( cchh == NULL ){
            fprintf( stderr , "premature EOF in ref file at line # %d\n" , kk+1 ) ;
            exit(1) ;
         }
         ref[kk] = strtod( buf , NULL ) ;
      }
      fclose( reffile ) ;
   } else {
      for( kk=0 ; kk < nimage ; kk++ ) ref[kk] = kk ;
   }

   for( kk=0 ; kk < nimage ; kk++ ){            /* set weights */
      wt[kk] = (ref[kk] < 33333) ? 1.0 : 0.0 ;
   }

   normalize( nimage , ref , wt ) ;   /* make into a unit vector */

 /*** allocate space for cov file stuff ***/

   vsum = (double *) malloc( sizeof(double) * nimage ) ;
   if( vsum == NULL ){
      fprintf( stderr , "cannot malloc space for vector sum\n" ) ;
      exit(1) ;
   }

   if( cvtype >= 2 ){
      vsqsum = (double *) malloc( sizeof(double) * nimage*nimage ) ;
      if( vsqsum == NULL ){
         fprintf( stderr ,"cannot malloc space for cov matrix\n" ) ;
         exit(1) ;
      }
   }

 /*** read cov file now, if present ***/

#define COVERR(nn) if(ii<(nn)){fprintf(stderr,"error reading cov file\n");exit(1);}

   if( lcov == 1 ){
      ii = fread( &numv  , sizeof(int) , 1 , covfile ) ; COVERR(1) ;
      ii = fread( &mvlen , sizeof(int) , 1 , covfile ) ; COVERR(1) ;
      ii = fread( &cvinp , sizeof(int) , 1 , covfile ) ; COVERR(1) ;

      if( mvlen != nimage || numv < 0 ){
         fprintf( stderr , "cov file has wrong sizes: nv mv = %d %d\n" ,
                  numv , mvlen ) ;
         exit(1) ;
      }
      if( cvinp != cvtype ){
         fprintf( stderr , "cov file has wrong type: %d\n" , cvinp ) ;
         exit(1) ;
      }

      ii = fread( vsum  , sizeof(double), nimage       , covfile ) ;
      COVERR(nimage);
      if( cvtype >= 2 ){
         ii = fread( vsqsum, sizeof(double), nimage*nimage, covfile ) ;
         COVERR(nimage*nimage) ;
      }

      fclose(covfile) ;
   } else if( lcov == -1 ){
      mvlen = nimage ;
      numv  = 0 ;
      for( ii=0 ; ii < nimage ; ii++ ) vsum[ii] = 0 ;
      if( cvtype >= 2 ){
         for( ii=0 ; ii < nimage*nimage ; ii++ ) vsqsum[ii] = 0 ;
      }
   } else {
      fprintf( stderr , "illegal value of lcov occured!\n" ) ;
      exit(1) ;
   }

 /*** do all pixels ***/

   for( jj=0 ; jj < ny ; jj++ ){
      joff = jj * nx ;
      for( ii=0 ; ii < nx ; ii++ ){
         ijoff = ii + joff ;

       /* load data from (ii,jj) pixel into vec[] */

         for( kk=0 ; kk < nimage ; kk++ ) vec[kk] = inar[kk][ijoff] ;

       /* if desired, median filter into org[] */

         if( lmedfilt ){
            org[0]        = vec[0] ;
            org[nimage-1] = vec[nimage-1] ;
            for( kk=0 ; kk < nimage-1 ; kk++ )
               org[kk] = MEDIAN( vec[kk-1] , vec[kk] , vec[kk+1] ) ;
         } else {
            for( kk=0 ; kk < nimage ; kk++ ) org[kk] = vec[kk] ;
         }

       /* dot product with reference */

         if( cthresh > 0.0 ){
            for( kk=0 ; kk < nimage ; kk++ ) vec[kk] = org[kk] ;
            normalize( nimage , vec , wt ) ;
            sum = 0.0 ;
            for( kk=0 ; kk < nimage ; kk++ ) sum += wt[kk] * ref[kk] * vec[kk] ;
         } else {
            sum = 0.0 ;
         }

       /* if we still like this pixel, do stuff with it */

         if( sum >= cthresh ){

            numv++ ;  /* another vector has passed */

            if( ldetrend ) detrend( nimage , org , wt ) ;

            for( my=0 ; my < nimage ; my++ ){  /* form sum & sum-of-products */
               vsum[my] += org[my] ;
               if( cvtype >= 2 ){
                  moff = nimage * my ;
                  for( lx=0 ; lx < nimage ; lx++ )
                     vsqsum[lx+moff] += org[my]*org[lx] ;
               }
            }

            if( lsave && cthresh > 0.0 ){      /* save timeseries? */
               if( savefile == NULL ){
                  savefile = fopen( sfname , "a" ) ;
                  if( savefile == NULL ){
                     fprintf( stderr , "cannot open save file\n" ) ;
                     exit(1) ;
                  }
               }
               fprintf( savefile , "# x=%3d  y=%3d\n" , ii,jj) ;
               for( lx=0 ; lx < nimage ; lx++ )
                  fprintf( savefile , "%d %12.4e\n" , lx,org[lx] ) ;
            }
                  
         } /* end sum >= cthresh */

      }  /* end ii */
   }  /* end jj */

 /*** save covariance ***/

   covfile = fopen( cfname , "w" ) ;
   fwrite( &numv  , sizeof(int) , 1 , covfile ) ;
   fwrite( &mvlen , sizeof(int) , 1 , covfile ) ;
   fwrite( &cvtype, sizeof(int) , 1 , covfile ) ;
   fwrite( vsum   , sizeof(double) , nimage , covfile ) ;
   if( cvtype >= 2 ){
      fwrite( vsqsum , sizeof(double) , nimage*nimage , covfile ) ;
   }
   fclose( covfile ) ;

   printf( "# vectors in covariance file now %d\n" , numv ) ;

   exit(0) ;
}
