#include "mrilib.h"

void print_results( char * , int , float *, float * ) ;  /* later, dude */

/*---------------------------------------------------------------------------*/
static double apar=0.0 ;   /** skewness parameter [-1..1] **/

#define USE_SKEWNESS

#ifndef USE_SKEWNESS    /********************************** symmetric model **/
static double pmodel_pdf( double x )
{
   double q = 1.0-x*x ;
   return (q <= 0.0) ? 0.0
                     : 0.9375*q*q ;
}

static double pmodel_cdf( double x )   /* integral of pmodel_pdf(x) */
{
   double q ;
   if( x <= -1.0 ) return 0.0 ;
   if( x >=  1.0 ) return 1.0 ;
   q = x*x ;
   return 0.5 + x*(15.0-10.0*q+3.0*q*q)/16.0 ;
}

#else /*********************************************** Model with skewness **/

static double pmodel_pdf( double x )
{
   double q = 1.0-x*x ;
   return (q <= 0.0) ? 0.0
                     : 0.9375*q*q
                      * (1.0 + apar*x*fabs(x)) ;
}

static double pmodel_cdf( double x )
{
   double q , val , sss ;
   if( x <= -1.0 ) return 0.0 ;
   if( x >=  1.0 ) return 1.0 ;
   q = x*x ;
   val = 0.5 + x*(15.0-10.0*q+3.0*q*q)/16.0 - apar/14.0 ;
   sss = (15.0/16.0) * (apar*x*q)*(1.0/3.0-0.4*q+q*q/7.0) ;
   if( x < 0.0 ) val -= sss ;
   else          val += sss ;
   return val ;
}
#endif

#define pmodel_bin(x1,x2,pk,wd) (pmodel_cdf((x2-pk)/wd)-pmodel_cdf((x1-pk)/wd))

/*---------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset , *mset ;
   byte *mask ;
   int iarg=1 , nmask,nfill , dilate=1 , dd,nx,ny,nz,nxy,nxyz , ii,jj,kk ;
   float SIhh=130.0 ;
   int   SIax=0 , SIbot,SItop ;
   short *sar , *mar ;
   float pval[128] , wval[128] ;
   int npk , verb=1 , win=0 , his=0 ;
   char *dname , cmd[2222] ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAnhist [options] dataset\n"
             "Input dataset is a T1-weighted high-res of the brain (shorts only).\n"
             "Output is a list of peaks in the histogram, to stdout, in the form\n"
             "  ( datasetname #peaks peak1 peak2 ... )\n"
             "In the C-shell, for example, you could do\n"
             "  set anhist = `3dAnhist -q -w1 dset+orig`\n"
             "Then the number of peaks found is in the shell variable $anhist[2].\n"
             "\n"
             "Options:\n"
             "  -q  = be quiet (don't print progress reports)\n"
             "  -h  = dump histogram data to Anhist.1D and plot to Anhist.ps\n"
             "  -w  = apply a Winsorizing filter prior to histogram scan\n"
             "         (or -w7 to Winsorize 7 times, etc.)\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dAnhist main"); machdep(); AFNI_logger("3dAnhist",argc,argv);

   /*-- options --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( argv[iarg][1] == 'q' ){ verb=0 ; iarg++ ; continue ; }
      if( argv[iarg][1] == 'h' ){ his =1 ; iarg++ ; continue ; }
      if( argv[iarg][1] == 'w' ){
        win = -1 ;
        if( isdigit(argv[iarg][2]) )
          sscanf(argv[iarg],"-w%d",&win) ;
        if( win <= 0 ) win = 1 ;
        iarg++ ; continue ;
      }

      fprintf(stderr,"** ILLEGAL option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /*------------------*/
   /*-- read dataset --*/

   dset = THD_open_dataset(argv[iarg]) ; dname = argv[iarg] ;
   if( !ISVALID_DSET(dset) ){ fprintf(stderr,"** CAN'T open dataset\n");exit(1); }
   if( DSET_BRICK_TYPE(dset,0) != MRI_short ){
      fprintf(stderr,"** ILLEGAL non-short dataset type\n"); exit(1);
   }
   nx = DSET_NX(dset); ny = DSET_NY(dset); nz = DSET_NZ(dset); nxy = nx*ny; nxyz = nxy*nz;
   if( nx < 32 || ny < 32 || nz < 32 ){
      fprintf(stderr,"** Dataset dimensions are less than 32x32x32?!\n"); exit(1);
   }
   if( verb ) fprintf(stderr,"++ Loading dataset\n") ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){ fprintf(stderr,"** CAN'T load dataset\n");exit(1); }

   /*-----------------------------*/
   /*** FIND THE BRAIN-ish MASK ***/

   if( verb ) fprintf(stderr,"++ Forming automask of connected high-intensity voxels\n") ;
   mask = THD_automask( dset ) ;
   if( mask == NULL ){
     fprintf(stderr,"** Mask creation fails for unknown reasons!\n"); exit(1);
   }

   /* do fillin, etc, after dilation */

   if( dilate > 0 ){
     int nmm=1 ;
     if( verb ) fprintf(stderr,"++ Dilating automask %d time%c\n",dilate,(dilate==1)?'.':'s') ;
     ii = rint(0.032*nx) ; nmm = MAX(nmm,ii) ;
     ii = rint(0.032*ny) ; nmm = MAX(nmm,ii) ;
     ii = rint(0.032*nz) ; nmm = MAX(nmm,ii) ;
     for( dd=0 ; dd < dilate ; dd++ ){
       THD_mask_dilate           ( nx,ny,nz , mask, 3   ) ;
       THD_mask_fillin_completely( nx,ny,nz , mask, nmm ) ;
     }
     for( ii=0 ; ii < nxyz ; ii++ ) mask[ii] = !mask[ii] ;
     THD_mask_clust( nx,ny,nz, mask ) ;
     for( ii=0 ; ii < nxyz ; ii++ ) mask[ii] = !mask[ii] ;
   }

   nmask = THD_countmask( DSET_NVOX(dset) , mask ) ;
   if( nmask == 0 ){
     fprintf(stderr,"** No voxels in the automask?!\n");
     print_results( dname,0,NULL,NULL ) ; exit(1);
   }

   /* find most superior point, clip off all more than SIhh (130) mm below it */

   if( SIhh > 0.0 ){

     if( ORIENT_tinystr[dset->daxes->xxorient][0] == 'S' ){
       for( ii=0 ; ii < nx ; ii++ )
         for( kk=0 ; kk < nz ; kk++ )
           for( jj=0 ; jj < ny ; jj++ )
             if( mask[ii+jj*nx+kk*nxy] ) goto CP5 ;
     CP5:
       SIax = 1 ; SIbot = ii + (int)(SIhh/fabs(DSET_DX(dset))+0.5) ; SItop = nx-1 ;
     }

     else if( ORIENT_tinystr[dset->daxes->xxorient][1] == 'S' ){
       for( ii=nx-1 ; ii >= 0 ; ii-- )
         for( kk=0 ; kk < nz ; kk++ )
           for( jj=0 ; jj < ny ; jj++ )
             if( mask[ii+jj*nx+kk*nxy] ) goto CP6 ;
     CP6:
       SIax = 1 ; SIbot = 0 ; SItop = ii - (int)(SIhh/fabs(DSET_DX(dset))+0.5) ;
     }

     else if( ORIENT_tinystr[dset->daxes->yyorient][0] == 'S' ){
       for( jj=0 ; jj < ny ; jj++ )
         for( kk=0 ; kk < nz ; kk++ )
           for( ii=0 ; ii < nx ; ii++ )
             if( mask[ii+jj*nx+kk*nxy] ) goto CP3 ;
     CP3:
       SIax = 2 ; SIbot = jj + (int)(SIhh/fabs(DSET_DY(dset))+0.5) ; SItop = ny-1 ;
     }

     else if( ORIENT_tinystr[dset->daxes->yyorient][1] == 'S' ){
       for( jj=ny-1 ; jj >= 0 ; jj-- )
         for( kk=0 ; kk < nz ; kk++ )
           for( ii=0 ; ii < nx ; ii++ )
             if( mask[ii+jj*nx+kk*nxy] ) goto CP4 ;
     CP4:
       SIax = 2 ; SIbot = 0 ; SItop = jj - (int)(SIhh/fabs(DSET_DY(dset))+0.5) ;
     }

     else if( ORIENT_tinystr[dset->daxes->zzorient][0] == 'S' ){
       for( kk=0 ; kk < nz ; kk++ )
         for( jj=0 ; jj < ny ; jj++ )
           for( ii=0 ; ii < nx ; ii++ )
             if( mask[ii+jj*nx+kk*nxy] ) goto CP1 ;
     CP1:
       SIax = 3 ; SIbot = kk + (int)(SIhh/fabs(DSET_DZ(dset))+0.5) ; SItop = nz-1 ;
     }

     else if( ORIENT_tinystr[dset->daxes->zzorient][1] == 'S' ){
       for( kk=nz-1 ; kk >= 0 ; kk-- )
         for( jj=0 ; jj < ny ; jj++ )
           for( ii=0 ; ii < nx ; ii++ )
             if( mask[ii+jj*nx+kk*nxy] ) goto CP2 ;
     CP2:
       SIax = 3 ; SIbot = 0 ; SItop = kk - (int)(SIhh/fabs(DSET_DZ(dset))+0.5) ;
     }

     /* cut off stuff below SIhh mm from most Superior point */

     if( SIax > 0 && SIbot <= SItop ){
       char *cax="xyz" ;
       if( verb )
         fprintf(stderr,"++ SI clipping mask along axis %c: slices %d..%d\n" ,
                cax[SIax-1] , SIbot,SItop ) ;
       switch( SIax ){
         case 1:
           for( ii=SIbot ; ii <= SItop ; ii++ )
             for( kk=0 ; kk < nz ; kk++ )
               for( jj=0 ; jj < ny ; jj++ ) mask[ii+jj*nx+kk*nxy] = 0 ;
         break ;
         case 2:
           for( jj=SIbot ; jj <= SItop ; jj++ )
             for( kk=0 ; kk < nz ; kk++ )
               for( ii=0 ; ii < nx ; ii++ ) mask[ii+jj*nx+kk*nxy] = 0 ;
         break ;
         case 3:
           for( kk=SIbot ; kk <= SItop ; kk++ )
             for( jj=0 ; jj < ny ; jj++ )
               for( ii=0 ; ii < nx ; ii++ ) mask[ii+jj*nx+kk*nxy] = 0 ;
         break ;
       }
     }
   }
   nmask = THD_countmask( DSET_NVOX(dset) , mask ) ;
   if( nmask <= 999 ){
     fprintf(stderr,"** Only %d voxels in the automask?!\n",nmask);
     print_results( dname,0,NULL,NULL); exit(1);
   }

   /* Winsorize? */

   if( win ){
     THD_3dim_dataset *wset ; float irad ;
     irad = (verb) ? 2.5 : -2.5 ;
     wset = WINsorize( dset , win , -1,-1 , irad , "winsor" , 0,0 , mask ) ;
     DSET_delete(dset) ;
     dset = wset ;
   }

   /* copy data in mask region to private array, then expunge dataset */

   sar = DSET_ARRAY(dset,0) ;
   mar = (short *) malloc( sizeof(short)*nmask ) ;
   for( jj=ii=0 ; ii < nxyz ; ii++ )
     if( mask[ii] && sar[ii] > 0 ) mar[jj++] = sar[ii] ;
   nmask = jj ;
   if( verb )
    fprintf(stderr,"++ %d voxels in mask [out of %d in dataset]\n",nmask,DSET_NVOX(dset)) ;

   DSET_delete(dset) ; free(mask) ;

   /*--------------------------------------------------------------------*/
   /*** Mask is finished.  Now build histogram of data in mask region. ***/

   { int hist[32768] , gist[32768] ;
     int qq,ncut,ib,nold , sbot,stop , kk,npos,nhalf , cbot,ctop,nwid ;
     double dsum ;

     sbot = stop = mar[0] ;
     for( ii=1 ; ii < nmask ; ii++ )
            if( mar[ii] < sbot ) sbot = mar[ii] ;
       else if( mar[ii] > stop ) stop = mar[ii] ;
     if( sbot == stop ){
       if( verb ) fprintf(stderr,"+ All voxels in mask have value = %d ?!\n",sbot);
       pval[0] = sbot ; wval[0] = 0 ;
       print_results( dname,1 , pval,wval ) ; exit(0) ;
     }

     /* build histogram */

     memset( hist , 0 , sizeof(int)*32768 ) ;
     for( ii=0 ; ii < nmask ; ii++ ) hist[mar[ii]]++ ;
     dsum = 0.0 ;
     for( ii=sbot ; ii <= stop ; ii++ )
       dsum += (double)(hist[ii])*(double)(hist[ii])*ii ;

     /* find cliplevel */

     qq = 0.65 * nmask ; ib = rint(0.5*sqrt(dsum/nmask)) ;
     for( kk=0,ii=stop ; ii >= ib && kk < qq ; ii-- ) kk += hist[ii] ;
     ncut = ii ; qq = 0 ;
     do{
      for( npos=0,ii=ncut ; ii <= stop ; ii++ ) npos += hist[ii] ; /* number >= cut */
       nhalf = npos/2 ;
       for( kk=0,ii=ncut ; ii <= stop && kk < nhalf ; ii++ )       /* find median */
         kk += hist[ii] ;
       nold = ncut ;
       ncut = 0.5 * ii ;                                           /* new cut */
       qq++ ;
     } while( qq < 20 && ncut != nold ) ;

     /* make smoothed histogram */

     cbot = ncut ; ctop = 3*ncut ;
     if( verb ) fprintf(stderr,"++ Histogram range = %d .. %d\n",cbot,ctop) ;
     nwid = rint(0.1*ncut) ;
     if( nwid == 0 ){
       if( verb ) fprintf(stderr,"++ Using unsmoothed histogram\n") ;
       memcpy( gist , hist , sizeof(short)*32768 ) ;
     } else {
       float *wt = (float *) malloc(sizeof(float)*(2*nwid+1)) ;
       float ws=0.0 ;
       int ibot,itop ;

       if( verb ) fprintf(stderr,"++ Smoothing histogram = %d .. %d around each value\n",-nwid,nwid) ;
       for( ii=0 ; ii <= 2*nwid ; ii++ ){
         wt[ii] = nwid+0.5-abs(nwid-ii) ; ws += wt[ii] ;
       }
       for( ii=0 ; ii <= 2*nwid ; ii++ ) wt[ii] /= ws ;

       for( jj=cbot ; jj <= ctop ; jj++ ){
         ibot = jj-nwid ; if( ibot < sbot ) ibot = 0 ;
         itop = jj+nwid ; if( itop > stop ) itop = stop ;
         ws = 0.0 ;
         for( ii=ibot ; ii <= itop ; ii++ )
           ws += wt[nwid-jj+ii] * hist[ii] ;
         gist[jj] = rint(ws) ;
       }
       free(wt) ;
     }

     if( verb ) fprintf(stderr,"++ Scanning histogram for peaks:" ) ;
     npk = 0 ;
     for( ii=cbot+2 ; ii <= ctop-2 ; ii++ ){
       if( gist[ii] > gist[ii-1] &&
           gist[ii] > gist[ii-2] &&
           gist[ii] > gist[ii+1] &&
           gist[ii] > gist[ii+2]   ){
             pval[npk]=ii; wval[npk++] = gist[ii];
             if( verb ) fprintf(stderr," %.1f",pval[npk-1]) ;
           }

       else if( gist[ii] == gist[ii+1] &&   /* very special case */
                gist[ii] >  gist[ii-1] &&
                gist[ii] >  gist[ii-2] &&
                gist[ii] >  gist[ii+2]   ){
                  pval[npk]=ii+0.5; wval[npk++] = gist[ii];
                  if( verb ) fprintf(stderr," %.1f",pval[npk-1]) ;
                }

       else if( gist[ii] == gist[ii+1] &&   /* super special case */
                gist[ii] == gist[ii-1] &&
                gist[ii] >  gist[ii-2] &&
                gist[ii] >  gist[ii+2]   ){
                  pval[npk]=ii; wval[npk++] = gist[ii];
                  if( verb ) fprintf(stderr," %.1f",pval[npk-1]) ;
                }
     }
     if( verb ) fprintf(stderr,"\n") ;

     if( his ){
       FILE *hf ;
       float **Gmat, *Hvec, *lam, *rez, sum,wtm, *wt,wbot,wtop, ebest=-1.0;
       float *ap,*pk,*ww ; int nregtry ;
       float *pkbest,*wwbest,*apbest,*lambest , pplm,aplm,wplm ;
       float *pklast,*wwlast,*aplast ;
       npos = 0 ;
       if( npk > 0 ){
         int ndim=ctop-cbot+1 , nvec=npk , iw,nw ;
         srand48((long)time(NULL)) ;
         Gmat = (float **)malloc(sizeof(float *)*nvec) ;
         for( jj=0 ; jj < nvec ; jj++ )
           Gmat[jj] = (float *)malloc(sizeof(float)*ndim) ;
         Hvec   = (float *)malloc(sizeof(float)*ndim) ;
         rez    = (float *)malloc(sizeof(float)*ndim) ;
         wt     = (float *)malloc(sizeof(float)*ndim) ;
         lam    = (float *)malloc(sizeof(float)*nvec) ;
         ww     = (float *)malloc(sizeof(float)*nvec) ;
         pk     = (float *)malloc(sizeof(float)*nvec) ;
         ap     = (float *)malloc(sizeof(float)*nvec) ;
         apbest = (float *)malloc(sizeof(float)*nvec) ;
         pkbest = (float *)malloc(sizeof(float)*nvec) ;
         wwbest = (float *)malloc(sizeof(float)*nvec) ;
         lambest= (float *)malloc(sizeof(float)*nvec) ;
         aplast = (float *)malloc(sizeof(float)*nvec) ;
         pklast = (float *)malloc(sizeof(float)*nvec) ;
         wwlast = (float *)malloc(sizeof(float)*nvec) ;

         if( verb ) fprintf(stderr,"++ Regressing histogram") ;
         wbot = 0.1*cbot; wtop=0.9*cbot; pplm=0.05*cbot; aplm=0.95; wplm=0.4*cbot;
         nregtry = 0 ;
   RegTry:
         switch(nvec){
           case 1: nw =   40000 ; break ;
           case 2: nw =  600000 ; break ;
          default: nw = 1200000 ; break ;
         }
         if( nregtry > 0 ){
           pplm *= 0.7 ; aplm *= 0.7 ; wplm *= 0.7 ; nw /= 2 ;
           memcpy(aplast,apbest,sizeof(float)*nvec) ;
           memcpy(pklast,pkbest,sizeof(float)*nvec) ;
           memcpy(wwlast,wwbest,sizeof(float)*nvec) ;
         } else {
           for( jj=0 ; jj < nvec ; jj++ ){
             wwlast[jj] = 0.5*cbot ;
             aplast[jj] = 0.0 ;
             pklast[jj] = pval[jj] ;
           }
         }
         for( iw=0 ; iw < nw ; iw++ ){           /* random search nw times */
           for( jj=0 ; jj < nvec ; jj++ ){
             ww[jj] = wwlast[jj] + (2.*drand48()-1.)*wplm ;
             pk[jj] = pklast[jj] + (2.*drand48()-1.)*pplm ;
             ap[jj] = aplast[jj] + (2.*drand48()-1.)*aplm ;
                  if( ap[jj] >  1.0 ) ap[jj] =  1.0 ;
             else if( ap[jj] < -1.0 ) ap[jj] = -1.0 ;
                  if( ww[jj] < 0.1*cbot ) ww[jj] = 0.1*cbot ;
             else if( ww[jj] > 0.9*ctop ) ww[jj] = 0.9*ctop ;

                  if( pk[jj]+ww[jj] > ctop ) ww[jj] = ctop-pk[jj] ;
             else if( pk[jj]-ww[jj] < cbot ) ww[jj] = pk[jj]-cbot ;
           }
           sum = wtm = 0.0 ;
           for( ii=0 ; ii < ndim ; ii++ ){
            wt[ii] = 0.01/ndim ;
            for( jj=0 ; jj < nvec ; jj++ ){
             apar = ap[jj] ;
             Gmat[jj][ii] = pmodel_bin(cbot+ii-0.5,cbot+ii+0.5,pk[jj],ww[jj]) ;
             if( Gmat[jj][ii] < 0.0 ) Gmat[jj][ii] = 0.0 ;
             wt[ii] += Gmat[jj][ii] ;
            }
            wtm = MAX(wtm,wt[ii]) ;
           }
           for( ii=0 ; ii < ndim ; ii++ ){
             wt[ii] = pow(wt[ii]/wtm,1.25) ; sum += wt[ii] ;
           }
           for( ii=0 ; ii < ndim ; ii++ ) wt[ii] /= sum ;
           for( ii=0 ; ii < ndim ; ii++ ){
             Hvec[ii] = gist[cbot+ii] * wt[ii] ;
             for( jj=0 ; jj < nvec ; jj++ ) Gmat[jj][ii] *= wt[ii] ;
           }
           for( jj=0 ; jj < nvec ; jj++ ) lam[jj] = 1.0 ;  /* constraints */
           for( ii=0 ; ii < ndim ; ii++ ) rez[ii] = 1.0 ;
           sum = cl1_solve_res( ndim,nvec , Hvec,Gmat , lam,1 , rez,1 ) ;
           if( sum >= 0.0 ){
             if( ebest < 0.0 || sum < ebest ){
               ebest = sum ;
               for( ii=0 ; ii < ndim ; ii++ ){
                 sum = 0.0 ;
                 for( jj=0 ; jj < nvec ; jj++ ) sum += Gmat[jj][ii] * lam[jj] ;
                 hist[cbot+ii] = (int)rint(sum/wt[ii]) ;
               }
               npos = 1 ;
               memcpy( apbest , ap , sizeof(float)*nvec ) ;
               memcpy( pkbest , pk , sizeof(float)*nvec ) ;
               memcpy( wwbest , ww , sizeof(float)*nvec ) ;
               memcpy( lambest, lam, sizeof(float)*nvec ) ;
               if( verb ) fprintf(stderr,"+") ;
             }
           }
         }
         if( verb ) fprintf(stderr,".") ;
         if( nregtry < 8 ){ nregtry++ ; goto RegTry ; }
         if( verb ) fprintf(stderr,"\n") ;
       }

       hf = fopen( "Anhist.1D" , "w" ) ;
       if( hf == NULL ){
         fprintf(stderr,"** Can't open Anhist.1D!\n") ;
       } else {
         char pbuf[1024]="\0" ;
         fprintf(hf,"# 3dAnhist") ;
         for( ii=1 ; ii < argc ; ii++ ) fprintf(hf," %s",argv[ii]) ;
         fprintf(hf,"\n") ;
         for( jj=0 ; jj < npk ; jj++ ){
           fprintf(hf,"# Peak %d: location=%.1f\n",jj+1,pval[jj]) ;
         }
         if( npos > 0 ){
           for( jj=0 ; jj < npk ; jj++ ){
             fprintf(hf,"# Peak %d fit: location=%.1f width=%.2f skew=%.3f height=%.1f\n",
                     jj+1,pkbest[jj],wwbest[jj],apbest[jj],lambest[jj] ) ;
             if( jj < 4 ){
               ii = strlen(pbuf) ;
               sprintf(pbuf+ii," #%d=%.1f\\pm%.1f",jj+1,pkbest[jj],wwbest[jj]) ;
             }
           }
           fprintf(hf,"#\n") ;
           fprintf(hf,"# Val Histog Fitted Hi-Fit\n") ;
           fprintf(hf,"# --- ------ ------ ------\n") ;
           for( ii=cbot ; ii <= ctop ; ii++ )
             fprintf(hf,"%5d %6d %6d %6d\n",ii,gist[ii],hist[ii],gist[ii]-hist[ii]) ;

           for( jj=0; jj < npk ; jj++ ) free(Gmat[jj]);
           free(Gmat);free(pk);free(ww);free(ap);free(Hvec);free(lam);free(rez);free(wt);
           free(apbest);free(wwbest);free(pkbest);free(lambest);
           free(aplast);free(wwlast);free(pklast);
           sprintf(cmd,"1dplot -ps -nopush -one -xzero %d -xlabel '%s:%s' 'Anhist.1D[1..3]' > Anhist.ps" ,
                   cbot , dname , pbuf ) ;
         } else {
           fprintf(hf,"# Val Histog\n") ;
           fprintf(hf,"# --- ------\n") ;
           for( ii=cbot ; ii <= ctop ; ii++ )
             fprintf(hf,"%5d %6d\n",ii,gist[ii]) ;
           for( jj=0 ; jj < npk && jj < 4 ; jj++ ){
             ii = strlen(pbuf) ;
             sprintf(pbuf+ii," #1%d=%.1f",jj+1,pval[jj]) ;
           }
           sprintf(cmd,"1dplot -ps -nopush -xzero %d -xlabel '%s:%s' 'Anhist.1D[1]' > Anhist.ps",
                   cbot , dname , pbuf ) ;
         }
         fclose(hf) ;
         if( verb ){
           fprintf(stderr,"++ %s\n",cmd) ;
           fprintf(stderr,"++ To view plot: gv -landscape Anhist.ps\n") ;
         }
         system( cmd ) ;
       }
     }

     print_results( dname,npk , pval , wval ) ;
   }

   exit(0) ;
}

/*---------------------------------------------------------------------------------*/

void print_results( char *dname , int np , float *pk , float *wd )
{
   int ii ;
   printf(" ( %s %d " , dname,np ) ;
   for( ii=0 ; ii < np ; ii++ )
     printf(" %f ",pk[ii]) ;
   printf(" )\n") ;
}
