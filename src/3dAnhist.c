#include "mrilib.h"

void print_results( char * , int , float *, float * ) ;  /* later, dude */

static float threshold=0.0 , cer=0.0 , cjv=0.0 , gmcount=0.0 , wmcount=0.0 ;

/*---------------------------------------------------------------------------*/
static double apar=0.0 ;   /** skewness parameter [-1..1] **/

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
   sss = 0.9375 * (apar*x*q)*(1.0/3.0-0.4*q+q*q/7.0) ;
   if( x < 0.0 ) val -= sss ;
   else          val += sss ;
   return val ;
}

#define pmodel_bin(x1,x2,pk,wd) (pmodel_cdf((x2-pk)/wd)-pmodel_cdf((x1-pk)/wd))

#define pmodel_mean(pk,wd) ((pk)+0.078125*apar*(wd))

#define pmodel_sigma(wd)   (0.002232142857*(wd)*sqrt(28672.-1225.*apar*apar))

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
   int npk , verb=1 , win=0 , his=0 , fit=1 , do2=0 ;
   char *dname , cmd[2222] , *label=NULL , *fname="Anhist" ;

   static int hist[32768] , gist[32768] ;
   int qq,ncut,ib,nold , sbot,stop , npos,nhalf , cbot,ctop,nwid ;
   double dsum ;
   float *wt , ws ;
   int ibot,itop ;
   FILE *hf ;
   float **Gmat, *Hvec, *lam, *rez, sum,wtm, wbot,wtop, ebest=-1.0;
   float *ap,*pk,*ww ; int nregtry , nbetter ;
   float *pkbest,*wwbest,*apbest,*lambest , pplm,aplm,wplm ;
   float *pklast,*wwlast,*aplast ;

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
             "  -F  = DON'T fit histogram with stupid curves.\n"
             "  -w  = apply a Winsorizing filter prior to histogram scan\n"
             "         (or -w7 to Winsorize 7 times, etc.)\n"
             "  -2  = Analyze top 2 peaks only, for overlap etc.\n"
             "\n"
             "  -label xxx = Use 'xxx' for a label on the Anhist.ps plot file\n"
             "                instead of the input dataset filename.\n"
             "  -fname fff = Use 'fff' for the filename instead of 'Anhist'.\n"
             "\n"
             "If the '-2' option is used, AND if 2 peaks are detected, AND if\n"
             "the -h option is also given, then stdout will be of the form\n"
             "  ( datasetname 2 peak1 peak2 thresh CER CJV count1 count2 count1/count2)\n"
             "where 2      = number of peaks\n"
             "      thresh = threshold between peak1 and peak2 for decision-making\n"
             "      CER    = classification error rate of thresh\n"
             "      CJV    = coefficient of joint variation\n"
             "      count1 = area under fitted PDF for peak1\n"
             "      count2 = area under fitted PDF for peak2\n"
             "      count1/count2 = ratio of the above quantities\n"
             "NOTA BENE\n"
             "---------\n"
             "* If the input is a T1-weighted MRI dataset (the usual case), then\n"
             "   peak 1 should be the gray matter (GM) peak and peak 2 the white\n"
             "   matter (WM) peak.\n"
             "* For the definitions of CER and CJV, see the paper\n"
             "   Method for Bias Field Correction of Brain T1-Weighted Magnetic\n"
             "   Resonance Images Minimizing Segmentation Error\n"
             "   JD Gispert, S Reig, J Pascau, JJ Vaquero, P Garcia-Barreno,\n"
             "   and M Desco, Human Brain Mapping 22:133-144 (2004).\n"
             "* Roughly speaking, CER is the ratio of the overlapping area of the\n"
             "   2 peak fitted PDFs to the total area of the fitted PDFS.  CJV is\n"
             "   (sigma_GM+sigma_WM)/(mean_WM-mean_GM), and is a different, ad hoc,\n"
             "   measurement of how much the two PDF overlap.\n"
             "* The fitted PDFs are NOT Gaussians.  They are of the form\n"
             "   f(x) = b((x-p)/w,a), where p=location of peak, w=width, 'a' is\n"
             "   a skewness parameter between -1 and 1; the basic distribution\n"
             "   is defined by b(x)=(1-x^2)^2*(1+a*x*abs(x)) for -1 < x < 1.\n"
             "\n"
             "-- RWCox - November 2004\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dAnhist main"); machdep(); AFNI_logger("3dAnhist",argc,argv);

   /*-- options --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-label") == 0 ){  /* 14 Mar 2003 */
        label = argv[++iarg] ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-fname") == 0 ){  /* 14 Mar 2003 */
        fname = argv[++iarg] ;
        if( !THD_filename_ok(fname) ){
          fprintf(stderr,"** Bad name after -fname!\n"); exit(1);
        }
        iarg++ ; continue ;
      }

      if( argv[iarg][1] == 'q' ){ verb=0 ; iarg++ ; continue ; }
      if( argv[iarg][1] == 'v' ){ verb++ ; iarg++ ; continue ; }
      if( argv[iarg][1] == 'h' ){ his =1 ; iarg++ ; continue ; }
      if( argv[iarg][1] == 'F' ){ fit =0 ; iarg++ ; continue ; }
      if( argv[iarg][1] == '2' ){ do2 =1 ; iarg++ ; continue ; }
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
   if( verb ) fprintf(stderr,"++ Loading dataset %s\n", DSET_BRIKNAME(dset) ) ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){ fprintf(stderr,"** CAN'T load dataset\n");exit(1); }

   if( label == NULL ) label = dname ;  /* 14 Mar 2003 */

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
     print_results( label,0,NULL,NULL ) ; exit(1);
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
     print_results( label,0,NULL,NULL); exit(1);
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

   sbot = stop = mar[0] ;
   for( ii=1 ; ii < nmask ; ii++ )
          if( mar[ii] < sbot ) sbot = mar[ii] ;
     else if( mar[ii] > stop ) stop = mar[ii] ;
   if( sbot == stop ){
     if( verb ) fprintf(stderr,"+ All voxels inside mask have value=%d ?!\n",sbot);
     pval[0] = sbot ; wval[0] = 0 ;
     print_results( label,1 , pval,wval ) ; exit(0) ;
   }
   if( verb > 1 ) fprintf(stderr,"++ Data range: %d .. %d\n",sbot,stop) ;

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
   if( nwid <= 0 ){
     if( verb ) fprintf(stderr,"++ Using unsmoothed histogram\n") ;
     memcpy( gist , hist , sizeof(int)*32768 ) ;
   } else {
     if( verb > 1 ) fprintf(stderr,"++ Computing smoothing weights:") ;
     ws = 0.0 ;
     wt = (float *)malloc(sizeof(float)*(2*nwid+1)) ;
     for( ii=0 ; ii <= 2*nwid ; ii++ ){
       wt[ii] = nwid-abs(nwid-ii) + 0.5f ;
       if( verb > 1 ) fprintf(stderr," (%d,%g)" , ii,wt[ii]) ;
       ws += wt[ii] ;
     }
     if( verb > 1 ) fprintf(stderr,"\n") ;
     for( ii=0 ; ii <= 2*nwid ; ii++ ) wt[ii] /= ws ;

     if( verb )
       fprintf(stderr,"++ Smoothing histogram = %d .. %d around each value\n",
               -nwid,nwid) ;

     for( jj=cbot ; jj <= ctop ; jj++ ){
       ibot = jj-nwid ; if( ibot < sbot ) ibot = sbot ;
       itop = jj+nwid ; if( itop > stop ) itop = stop ;
       ws = 0.0 ;
       for( ii=ibot ; ii <= itop ; ii++ )
         ws += wt[nwid-jj+ii] * hist[ii] ;
       gist[jj] = rint(ws) ;
       if( verb > 1 )
         fprintf(stderr," + %3d: unsmoothed=%d  smoothed=%d\n",
                 jj,hist[jj],gist[jj]) ;
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

   if( do2 && npk > 2 ){  /* find largest two peaks and keep only them */
     float pval_top, pval_2nd, wval_top, wval_2nd , www; int iii,itop ;
     www = wval[0] ; iii = 0 ;
     for( ii=1 ; ii < npk ; ii++ ){
       if( wval[ii] > www ){ www = wval[ii] ; iii = ii ; }
     }
     pval_top = pval[iii] ; wval_top = www ; itop = iii ;
     www = -1 ; iii = -1 ;
     for( ii=0 ; ii < npk ; ii++ ){
       if( ii != itop && wval[ii] > www ){ www = wval[ii] ; iii = ii ; }
     }
     pval_2nd = pval[iii] ; wval_2nd = www ;

     /* make sure peaks are increasing in pval */

     if( pval_top < pval_2nd ){
       pval[0] = pval_top ; wval[0] = wval_top ;
       pval[1] = pval_2nd ; wval[1] = wval_2nd ;
     } else {
       pval[0] = pval_2nd ; wval[0] = wval_2nd ;
       pval[1] = pval_top ; wval[1] = wval_top ;
     }
     npk = 2 ;
     if( verb )
       fprintf(stderr,"++ Keeping top 2 peaks: %.1f %.1f\n",pval[0],pval[1]) ;
   }

   if( his ){   /* fit histogram? */
     npos = 0 ;
     if( npk > 0 && fit ){
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
       wbot=0.1*cbot; wtop=0.9*cbot; pplm=0.05*cbot; aplm=0.75; wplm=0.4*cbot;
       nregtry = 0 ;
 RegTry:
       switch(nvec){
         case 1: nw =  30000 ; break ;
         case 2: nw = 400000 ; break ;
        default: nw = 666000 ; break ;
       }
       if( nregtry > 0 ){                 /* keep previous best estimates */
         pplm *= 0.7 ; aplm *= 0.7 ; wplm *= 0.7 ; nw /= 2 ;
         memcpy(aplast,apbest,sizeof(float)*nvec) ;
         memcpy(pklast,pkbest,sizeof(float)*nvec) ;
         memcpy(wwlast,wwbest,sizeof(float)*nvec) ;
       } else {
         for( jj=0 ; jj < nvec ; jj++ ){  /* initial estimates */
           wwlast[jj] = 0.5*cbot ;
           aplast[jj] = 0.0 ;
           pklast[jj] = pval[jj] ;
         }
       }

       for( ii=0 ; ii < ndim ; ii++ ){
         wt[ii] = pow( (double)(1+gist[cbot+ii]) , 1.25 ) ;
       }
       for( ii=0,sum=0.0 ; ii < ndim ; ii++ ) sum += wt[ii] ;
       for( ii=0 ; ii < ndim ; ii++ ) wt[ii] /= sum ;
       wtm = 1.0 ;
       for( ii=0 ; ii < ndim ; ii++ )
         if( wt[ii] < wtm && wt[ii] > 0.0 ) wtm = wt[ii] ;
       for( ii=0 ; ii < ndim ; ii++ )
         if( wt[ii] == 0.0 ) wt[ii] = wtm ;

       nbetter = 0 ;
       for( iw=0 ; iw < nw ; iw++ ){           /* random search nw times */
         for( jj=0 ; jj < nvec ; jj++ ){
           ww[jj] = wwlast[jj] + (2.*drand48()-1.)*wplm ;
           pk[jj] = pklast[jj] + (2.*drand48()-1.)*pplm ;
           ap[jj] = aplast[jj] + (2.*drand48()-1.)*aplm ;
                if( pk[jj] >  0.9*ctop ) pk[jj] = 0.9*ctop ;
           else if( pk[jj] <  1.1*cbot ) pk[jj] = 1.1*cbot ;
                if( ap[jj] >  1.0 ) ap[jj] =  1.0 ;
           else if( ap[jj] < -1.0 ) ap[jj] = -1.0 ;
                if( ww[jj] < 0.1*cbot ) ww[jj] = 0.1*cbot ;
           else if( ww[jj] > 0.9*cbot ) ww[jj] = 0.9*cbot ;

                if( pk[jj]+ww[jj] > ctop ) ww[jj] = ctop-pk[jj] ;
           else if( pk[jj]-ww[jj] < cbot ) ww[jj] = pk[jj]-cbot ;
         }
         sum = wtm = 0.0 ;
         for( ii=0 ; ii < ndim ; ii++ ){  /* create basis vectors */
          for( jj=0 ; jj < nvec ; jj++ ){  /* basis for jj-th peak pdf */
           apar = ap[jj] ;
           Gmat[jj][ii] = pmodel_bin(cbot+ii-0.5,cbot+ii+0.5,pk[jj],ww[jj]) ;
           if( Gmat[jj][ii] < 0.0 ) Gmat[jj][ii] = 0.0 ;
          }
         }
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
             npos = 1 ; nbetter++ ;
             memcpy( apbest , ap , sizeof(float)*nvec ) ;
             memcpy( pkbest , pk , sizeof(float)*nvec ) ;
             memcpy( wwbest , ww , sizeof(float)*nvec ) ;
             memcpy( lambest, lam, sizeof(float)*nvec ) ;
             if( verb ) fprintf(stderr,"+") ;
             if( verb > 1 ){
               fprintf(stderr,"[") ;
               for( jj=0 ; jj < nvec ; jj++ ){
                 fprintf(stderr,"p=%.1f w=%.2f a=%.3f",
                         pkbest[jj],wwbest[jj],apbest[jj]) ;
                 if( jj < nvec-1 ) fprintf(stderr,";") ;
               }
               fprintf(stderr,"]") ;
             }
           }
         }
       } /* end of nw searches */
       if( verb ) fprintf(stderr,".") ;
       if( nregtry < 8                 ){ nregtry++ ; goto RegTry ; }
       if( nregtry < 12 && nbetter > 0 ){ nregtry++ ; goto RegTry ; }
       if( verb ) fprintf(stderr,"\n") ;
     } /* end of fitting */

       /* save some results */

     sprintf(cmd,"%s.1D",fname) ;
     hf = fopen( cmd , "w" ) ;
     if( hf == NULL ){
       fprintf(stderr,"** Can't open file %s for output!\n",cmd) ;
     } else {
       static char pbuf[1024]="\0" ;
       fprintf(hf,"# 3dAnhist") ;
       for( ii=1 ; ii < argc ; ii++ ) fprintf(hf," %s",argv[ii]) ;
       fprintf(hf,"\n") ;
       for( jj=0 ; jj < npk ; jj++ ){
         fprintf(hf,"# Peak %d: location=%.1f histpeak=%.1f\n",jj+1,pval[jj],wval[jj]) ;
       }
       if( fit && npos > 0 ){
         float gval ; int gtot ;

         /* compute individual fits into Gmat[jj] curves,
            and total number in each fit into wt[jj] values */

         for( jj=0 ; jj < npk ; jj++ ) wt[jj] = 0.0 ;
         gtot = 0 ;
         for( ii=cbot ; ii <= ctop ; ii++ ){
           gtot += gist[ii] ;
           for( jj=0 ; jj < npk ; jj++ ){
             apar = apbest[jj] ;
             gval = lambest[jj] *
                    pmodel_bin(ii-0.5,ii+0.5,pkbest[jj],wwbest[jj]) ;
             Gmat[jj][ii] = rint(gval) ;
             wt[jj] += Gmat[jj][ii] ;
           }
         }

         for( jj=0 ; jj < npk ; jj++ ){
           fprintf(hf,"# Peak %d fit: location=%.1f width=%.2f skew=%.3f height=%.1f\n",
                   jj+1,pkbest[jj],wwbest[jj],apbest[jj],lambest[jj] ) ;
           if( jj < 4 ){
             ii = strlen(pbuf) ;
             sprintf(pbuf+ii," #%d=%.1f\\pm%.1f",jj+1,pkbest[jj],wwbest[jj]) ;
           }
         }
         fprintf(hf,"#\n") ;

         /* 03 Nov 2004: calculate some measures of the overlap */

         if( do2 && npk == 2 ){
           float mu_bot,sd_bot , mu_top,sd_top ;
           int ibot,itop , ix , gx0,gx1 ;

           /* statistics of variation and location */

           apar   = apbest[0] ;
           mu_bot = pmodel_mean(pkbest[0],wwbest[0]) ;
           sd_bot = pmodel_sigma(wwbest[0]) ;
           fprintf(hf,"# mu_bot=%.1f  sd_bot=%.2f\n",mu_bot,sd_bot) ;
           apar   = apbest[1] ;
           mu_top = pmodel_mean(pkbest[1],wwbest[1]) ;
           sd_top = pmodel_sigma(wwbest[1]) ;
           fprintf(hf,"# mu_top=%.1f  sd_top=%.2f\n",mu_top,sd_top) ;
           cjv    = (sd_bot+sd_top)/fabs(mu_top-mu_bot) ;
           fprintf(hf,"# CJV   = %.3f%%\n",100.0*cjv) ;

           fprintf(hf,"# Histogram = %d voxels\n",gtot) ;
           fprintf(hf,"# Fit 1     = %d voxels\n",(int)wt[0]) ;
           fprintf(hf,"# Fit 2     = %d voxels\n",(int)wt[1]) ;
           fprintf(hf,"# Ratio 1/2 = %.3f\n"     ,wt[0]/wt[1]) ;

           gmcount = wt[0] ; wmcount = wt[1] ;

           /* scan for crossover between fitted densities */

           ibot = (int)mu_bot ; itop = 1+(int)mu_top ;
           for( ix=ibot ; ix < itop ; ix++ )
             if( Gmat[0][ix] < Gmat[1][ix] ) break ;
           if( ix < itop && ix > ibot ){
             threshold = ix-0.5 ;
             gx0 = gx1 = 0 ;
             for( ii=cbot ; ii < ix   ; ii++ ) gx0 += Gmat[1][ii] ;
             for( ii=ix   ; ii < ctop ; ii++ ) gx1 += Gmat[0][ii] ;
             fprintf(hf,"# Overlaps: Thresh=%.1f  Fit 1=%d  Fit 2=%d\n",
                     threshold,gx0,gx1) ;
             cer = ((float)(gx0+gx1))/(wt[0]+wt[1]) ;
             fprintf(hf,"# CER = %.3f%%\n",100.0*cer) ;
           }
         }

         fprintf(hf,"# Histogram Region: min=%d max=%d\n",cbot,ctop) ;

         fprintf(hf,"# Val Histog FitSum Hi-Fit") ;
         for( jj=0 ; jj < npk ; jj++ ) fprintf(hf," Fit#%1d ",jj+1) ;
         fprintf(hf,"\n") ;

         fprintf(hf,"# --- ------ ------ ------") ;
         for( jj=0 ; jj < npk ; jj++ ) fprintf(hf," ------") ;
         fprintf(hf,"\n") ;
         for( ii=cbot ; ii <= ctop ; ii++ ){
           fprintf(hf,"%5d %6d %6d %6d",ii,gist[ii],hist[ii],gist[ii]-hist[ii]) ;
           for( jj=0 ; jj < npk ; jj++ )
             fprintf(hf," %6d",(int)Gmat[jj][ii]) ;
           fprintf(hf,"\n") ;
         }

#if 0
         for( jj=0; jj < npk ; jj++ ) free(Gmat[jj]);
         free(Gmat);free(pk);free(ww);free(ap);free(Hvec);free(lam);free(rez);free(wt);
         free(apbest);free(wwbest);free(pkbest);free(lambest);
         free(aplast);free(wwlast);free(pklast);
#endif
         ii = (do2 && npk == 2) ? 5 : 3 ;
         sprintf(cmd,
          "1dplot -ps -nopush -one -xzero %d -xlabel '%s:%s' '%s.1D[1..%d]' > %s.ps" ,
                 cbot , label , pbuf , fname,ii,fname ) ;
       } else {
         fprintf(hf,"# Histogram Region: min=%d max=%d\n",cbot,ctop) ;
         fprintf(hf,"# Val Histog\n") ;
         fprintf(hf,"# --- ------\n") ;
         for( ii=cbot ; ii <= ctop ; ii++ )
           fprintf(hf,"%5d %6d\n",ii,gist[ii]) ;
         for( jj=0 ; jj < npk && jj < 4 ; jj++ ){
           ii = strlen(pbuf) ;
           sprintf(pbuf+ii," #1%d=%.1f",jj+1,pval[jj]) ;
         }
         sprintf(cmd,"1dplot -ps -nopush -xzero %d -xlabel '%s:%s' '%s.1D[1]' > %s.ps",
                 cbot , label , pbuf , fname,fname ) ;
       }
       fclose(hf) ;
       if( verb ){
         fprintf(stderr,"++ %s\n",cmd) ;
         fprintf(stderr,"++ To view plot: gv -landscape %s.ps\n",fname) ;
       }
       system( cmd ) ;
     }
   }

   print_results( label,npk , pval , wval ) ;
   exit(0) ;
}

/*---------------------------------------------------------------------------------*/

void print_results( char *dname , int np , float *pk , float *wd )
{
   int ii ;
   printf(" ( %s %d " , dname,np ) ;
   for( ii=0 ; ii < np ; ii++ ) printf(" %.2f ",pk[ii]) ;

   if( threshold > 0.0 ){
     printf(" %.2f %.4f %.4f %.0f %.0f %.3f",
            threshold , cer , cjv , gmcount , wmcount , gmcount/wmcount ) ;
   }

   printf(" )\n") ;
}
