#include "mrilib.h"

void print_results( char * , int , float *, float * ) ;

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset , *mset ;
   byte *mask ;
   int iarg=1 , nmask,nfill , dilate=1 , dd,nx,ny,nz,nxy,nxyz , ii,jj,kk ;
   float SIhh=130.0 ;
   int   SIax=0 , SIbot,SItop ;
   short *sar , *mar ;
   float pval[128] , wval[128] ;
   int npk , verb=1 ;
   char *dname ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAnhist [-q] dataset\n"
             "Input dataset is a T1-weighted high-res of the brain (shorts only).\n"
             "Options:\n"
             "  -q  = be quiet (don't print progress reports)\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dAnhist main"); machdep(); AFNI_logger("3dAnhist",argc,argv);

   /*-- options --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( argv[iarg][1] == 'q' ){ verb=0 ; iarg++ ; continue ; }
        
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

       if( verb ) fprintf(stderr,"++ Smoothing range = %d..%d\n",-nwid,nwid) ;
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
     }

     npk = 0 ;
     for( ii=cbot+2 ; ii <= ctop-2 ; ii++ ){
       if( gist[ii] > gist[ii-1] &&
           gist[ii] > gist[ii-2] &&
           gist[ii] > gist[ii+1] &&
           gist[ii] > gist[ii+2]   ){ pval[npk]=ii; wval[npk++] = 0; }
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
