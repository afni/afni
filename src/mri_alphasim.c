#include "mrilib.h"
#include <sys/types.h>
#include <unistd.h>
#include <time.h>

#include "zgaussian.c"   /** fast function for generating Gaussian deviates **/

/*============================================================================*/
/* The function below is no longer used,
   but could be recycled if one wants to get a more AlphaSim-like result. ====*/

#if 0
intvec * ALP_count_clusters( MRI_IMAGE *bim , float rmm , int minsize )
{
   intvec *iv ;
   int nx,ny,nz , nclu ;
   MCW_cluster *clust , *mask ;
   int nxy,nxyz , ijk , ijk_last , mnum ;
   int icl , jma , ijkma ;
   float dx,dy,dz ;
   byte  *bfar ;
   short ic, jc, kc , im, jm, km, *mi,*mj,*mk ;

ENTRY("ALP_count_clusters") ;

   if( bim == NULL || bim->kind != MRI_byte ) RETURN(NULL) ;
   bfar = MRI_BYTE_PTR(bim) ;
   nx = bim->nx ; ny = bim->ny ; nz = bim->nz ;
   dx = bim->dx ; dy = bim->dy ; dz = bim->dz ;
   if( rmm <= 0.0f ){ dx = dy = dz = 1.0f ; rmm = 1.01f ; }

   /*--- make a cluster that is a mask of points closer than max_dist ---*/

   mask = MCW_build_mask( dx, dy, dz, rmm ) ;
   if( mask == NULL ){
     mask = MCW_build_mask( 1.0f,1.0f,1.0f, 1.01f ) ;
     if( mask == NULL ) RETURN(NULL) ;
   }

   nxy = nx*ny ; nxyz = nxy*nz ;

   mnum = mask->num_pt ; mi = mask->i ; mj = mask->j ; mk = mask->k ;

   /*--- scan through array, find nonzero point, build a cluster, ... ---*/

   nclu = ijk_last = 0 ; INIT_CLUSTER(clust) ; MAKE_intvec(iv,16) ;
   do {
     for( ijk=ijk_last ; ijk < nxyz && bfar[ijk] == 0 ; ijk++ ) ; /*nada*/
     if( ijk < nxyz ) bfar[ijk] = 0 ;  /* found a nonzero point */
     else             break ;          /* didn't find any ==> done */

     ijk_last = ijk+1 ;         /* start here next time */

     clust->num_pt = 0 ;        /* clear out old cluster */
     IJK_TO_THREE(ijk,ic,jc,kc,nx,nxy) ;
     ADDTO_CLUSTER_NOMAG( clust , ic,jc,kc ) ;  /* start it off */

     for( icl=0 ; icl < clust->num_pt ; icl++ ){
       ic = clust->i[icl] ;
       jc = clust->j[icl] ;
       kc = clust->k[icl] ;

       for( jma=0 ; jma < mnum ; jma++ ){
         im = ic + mi[jma] ; if( im < 0 || im >= nx ) continue ;
         jm = jc + mj[jma] ; if( jm < 0 || jm >= ny ) continue ;
         km = kc + mk[jma] ; if( km < 0 || km >= nz ) continue ;

         ijkma = THREE_TO_IJK(im,jm,km,nx,nxy) ;
         if( bfar[ijkma] == 0 ) continue ;

         ADDTO_CLUSTER_NOMAG( clust , im,jm,km ) ;
         bfar[ijkma] = 0 ;
       }
     }

     if( clust->num_pt >= minsize ){
       if( iv->nar == nclu ){ icl = 2*nclu+32; RESIZE_intvec(iv,icl); }
       iv->ar[nclu++] = clust->num_pt ;
     }

   } while( 1 ) ;

   KILL_CLUSTER(clust) ; KILL_CLUSTER(mask) ;

   if( nclu == 0 ) KILL_intvec(iv) ;
   else            RESIZE_intvec(iv,nclu) ;

   RETURN(iv) ;
}
#endif
/*============================================================================*/

/*----------------------------------------------------------------------------*/
/* Clusterize and return the size of the largest cluster in the image.
   The image is destroyed in the process; it must be comprised of bytes. -----*/

int ALP_largest_clustersize( MRI_IMAGE *bim , float rmm )
{
   int nx,ny,nz , nclu , biggest=0 ;
   MCW_cluster *clust , *mask ;
   int nxy,nxyz , ijk , ijk_last , mnum ;
   int icl , jma , ijkma , do_nn=0 ;
   float dx,dy,dz ;
   byte  *bfar ;
   short ic, jc, kc , im, jm, km, *mi,*mj,*mk ;

ENTRY("ALP_largest_clustersize") ;

   if( bim == NULL || bim->kind != MRI_byte ) RETURN(0) ;
   bfar = MRI_BYTE_PTR(bim) ;
   nx = bim->nx ; ny = bim->ny ; nz = bim->nz ;
   dx = bim->dx ; dy = bim->dy ; dz = bim->dz ;
   if( rmm <= 0.0f ){ dx = dy = dz = 1.0f ; rmm = 1.01f ; do_nn = 1 ; }

   /*--- make a cluster that is a mask of points closer than max_dist ---*/

   mask = MCW_build_mask( dx, dy, dz, rmm ) ;
   if( mask == NULL ){
     mask = MCW_build_mask( 1.0f,1.0f,1.0f, 1.01f ) ; do_nn = 1 ;
     if( mask == NULL ) RETURN(0) ;
   }

   nxy = nx*ny ; nxyz = nxy*nz ;

   mnum = mask->num_pt ; mi = mask->i ; mj = mask->j ; mk = mask->k ;

   /*--- scan through array, find nonzero point, build a cluster, ... ---*/

   nclu = ijk_last = 0 ; INIT_CLUSTER(clust) ;
   do {
     for( ijk=ijk_last ; ijk < nxyz && bfar[ijk] == 0 ; ijk++ ) ; /*nada*/
     if( ijk < nxyz ) bfar[ijk] = 0 ;  /* found a nonzero point */
     else             break ;          /* didn't find any ==> done */

     ijk_last = ijk+1 ;         /* start here next time */

     clust->num_pt = 0 ;        /* clear out old cluster */
     IJK_TO_THREE(ijk,ic,jc,kc,nx,nxy) ;
     ADDTO_CLUSTER_NOMAG( clust , ic,jc,kc ) ;  /* start it off */

     for( icl=0 ; icl < clust->num_pt ; icl++ ){
       ic = clust->i[icl] ;
       jc = clust->j[icl] ;
       kc = clust->k[icl] ;

#undef  PROC
#define PROC(p,q,r)                                    \
 ijkma = THREE_TO_IJK(p,q,r,nx,nxy) ;                  \
 if( bfar[ijkma] != 0 ){                               \
   ADDTO_CLUSTER_NOMAG(clust,p,q,r); bfar[ijkma] = 0;  \
 }

       if( do_nn ){  /*--------------------------- NN special code for speed */
         im = ic+1 ; if( im < nx ){ PROC(im,jc,kc) ; }
         im = ic-1 ; if( im >= 0 ){ PROC(im,jc,kc) ; }
         jm = jc+1 ; if( jm < ny ){ PROC(ic,jm,kc) ; }
         jm = jc-1 ; if( jm >= 0 ){ PROC(ic,jm,kc) ; }
         km = kc+1 ; if( km < nz ){ PROC(ic,jc,km) ; }
         km = kc-1 ; if( km >= 0 ){ PROC(ic,jc,km) ; }
       } else {      /*--------------------------- general rmm > 0 clustering */
         for( jma=0 ; jma < mnum ; jma++ ){
           im = ic + mi[jma] ; if( im < 0 || im >= nx ) continue ;
           jm = jc + mj[jma] ; if( jm < 0 || jm >= ny ) continue ;
           km = kc + mk[jma] ; if( km < 0 || km >= nz ) continue ;
           PROC(im,jm,km) ;
         }
       }
     }

     if( clust->num_pt > biggest ) biggest = clust->num_pt ;
   } while( 1 ) ;

   KILL_CLUSTER(clust) ; KILL_CLUSTER(mask) ;
   RETURN(biggest) ;
}

/*----------------------------------------------------------------------------*/
#define MAX_CLUSTSIZE 32768
static int verb = 1 ;

MRI_IMAGE * mri_alphasim( int   nx , int nz ,
                          float dx , float dz  ,
                          int niter , float rmm ,
                          int num_pval , float *pval ,
                          int num_fwhm , float *fwhmx , float *fwhmz ,
                          byte *mask , long seed )
{
   MRI_IMAGE *bim , *aim , *cim , *qim ,      *dim ;
   float     *bar , *aar , *car , *qar ; byte *dar ;
   int ite , jsm , kth , nxyz , ii,jj , ny ;
   float tt , u1,u2 , *ath , nitinv , *thr , dy , sx,sz ;
   float fdx,fdz , fx,fz , fnx,fnz ;
   double sd ;
   static long sseed=0 ;

ENTRY("mri_alphasim") ;

   if( nx < 8 || nz < 1 ) RETURN(NULL) ;

   if( dx <= 0.0f ) dx = 1.0f ;
   if( dz <= 0.0f ) dz = dx ;

   ny = nx ; dy = dx ;

   if( niter < 1 ) niter = 1000 ;

   if( num_pval < 1 || pval == NULL ){
     static float pp = 0.001f ;
     num_pval = 1 ; pval = &pp ;
   }

   if( num_fwhm < 1 || fwhmx == NULL ){
     static float ff = 0.0f ;
     num_fwhm = 1 ; fwhmx = &ff ;
   }
   if( fwhmz == NULL ) fwhmz = fwhmx ;

   aim = mri_new_vol( MAX_CLUSTSIZE , num_fwhm , num_pval , MRI_float ) ;
   aar = MRI_FLOAT_PTR(aim) ;
#undef  ATH
#define ATH(s,t) ( aar + ((s)*MAX_CLUSTSIZE + (t)*(MAX_CLUSTSIZE*num_fwhm) -1) )

   nxyz = nx*ny*nz ;

   if( seed != 0 ){
     srand48(seed) ;
   } else if( sseed == 0 ){
     sseed = (long)time(NULL) + (long)getpid() ;
     srand48(sseed) ;
   }

   thr = (float *)malloc(sizeof(float)*num_pval) ;
   for( kth=0 ; kth < num_pval ; kth++ )
     thr[kth] = nifti_rcdf2stat( (double)pval[kth] ,
                                 NIFTI_INTENT_ZSCORE , 0.0,0.0,0.0 ) ;

   qim = mri_new_vol( nx,ny,nz , MRI_float ) ; qar = MRI_FLOAT_PTR(qim) ;
   bim = mri_new_vol( nx,ny,nz , MRI_float ) ; bar = MRI_FLOAT_PTR(bim) ;
   cim = mri_new_vol( nx,ny,nz , MRI_float ) ; car = MRI_FLOAT_PTR(cim) ;
   dim = mri_new_vol( nx,ny,nz , MRI_byte  ) ; dar = MRI_BYTE_PTR (dim) ;
   dim->dx = dx ; dim->dy = dy ; dim->dz = dz ;

   /*-- iteration loop --*/

   nitinv = 1.0f / niter ;

   if( verb ) fprintf(stderr,"mri_alphasim:") ;
   for( ite=0 ; ite < niter ; ite++ ){

     if( verb && ite%500 == 499 ) fprintf(stderr,".") ;

     /*-- create uncorrelated random field --*/

     for( ii=0 ; ii < nxyz ; ii++ ) bar[ii] = zgaussian() ;
     memcpy( qar , bar , sizeof(float)*nxyz ) ;  /* qar = unsmoothed */

     fx = fz = 0.0f ;  /* set current smoothness level of bar */

     /*-- loop over smoothings --*/

     for( jsm=0 ; jsm < num_fwhm ; jsm++ ){

       /* incrementally blur dataset? */

       fnx = fwhmx[jsm] ; fdx = sqrtf(fnx*fnx-fx*fx) ;
       fnz = fwhmz[jsm] ; fdz = sqrtf(fnz*fnz-fz*fz) ;

       if( fnx > 2.0f*dx && fnz > 2.0*dz ){     /*---- incremental blur ----*/

         fdx = sqrtf(fnx*fnx-fx*fx) ; fdz = sqrtf(fnz*fnz-fz*fz) ;
         sx  = FWHM_TO_SIGMA(fdx)   ; sz  = FWHM_TO_SIGMA(fdz)   ;
         EDIT_blur_volume_3d( nx,ny,nz      , dx,dy,dz ,
                              MRI_float,bar , sx,sx,sz  ) ;
         memcpy( car , bar , sizeof(float)*nxyz ) ;
         fx = fnx ; fz = fnz ; /* set blur that bar has now */

       } else if( fnx > 0.0f || fnz > 0.0f ){      /*---- directly blur ----*/

         memcpy( car , qar , sizeof(float)*nxyz ) ;
         sx = FWHM_TO_SIGMA(fnx) ; sz = FWHM_TO_SIGMA(fnz) ;
         EDIT_blur_volume_3d( nx,ny,nz      , dx,dy,dz ,
                              MRI_float,car , sx,sx,sz  ) ;

       } else {
         memcpy( car , qar , sizeof(float)*nxyz ) ;  /*---- no blurring ----*/
       }

       /* find stdev of blurred dataset (we know the mean is zero) */

       sd = 0.0 ;
       for( ii=0 ; ii < nxyz ; ii++ ) sd += car[ii]*car[ii] ;
       sd = sqrt(sd/nxyz) ;

       /* mask blurred dataset */

       if( mask != NULL )
         for( ii=0 ; ii < nxyz ; ii++ ) if( mask[ii] == 0 ) car[ii] = 0.0f ;

       /*-- loop over per-voxel thresholds --*/

       for( kth=0 ; kth < num_pval ; kth++ ){

          /* threshold */

          tt = sd * thr[kth] ;
          for( ii=0 ; ii < nxyz ; ii++ ) dar[ii] = (car[ii] >= tt) ;

          /* clusterize and count into aar */

          jj = ALP_largest_clustersize( dim , rmm ) ;
          if( jj > 0 ){
            if( jj > MAX_CLUSTSIZE ) jj = MAX_CLUSTSIZE ;
            ath = ATH(jsm,kth); ath[jj] += 1.0f ;
          }

       } /* end of loop over thresholds */
     } /* end of loop over smoothings */
   } /* end of iterations */
   if( verb ) fprintf(stderr,"\n") ;

   mri_free(dim); mri_free(cim); mri_free(bim); mri_free(qim); free((void *)thr);

   /* convert x-th entry to prob(largest cluster size >= x) */

   for( jsm=0 ; jsm < num_fwhm ; jsm++ ){
     for( kth=0 ; kth < num_pval ; kth++ ){
       ath = ATH(jsm,kth) ;
       for( jj=MAX_CLUSTSIZE-1 ; jj >= 1 ; jj-- ) ath[jj] += ath[jj+1] ;
       for( jj=1 ; jj <= MAX_CLUSTSIZE ; jj++ ) ath[jj] *= nitinv ;
     }
   }

   RETURN(aim) ;
}

/**********************************************************/

int main( int argc , char *argv[] )
{
   int nx,ny,nz , niter , jsm,kth,ii,jj,kk,qq , nrev ;
   float dx,dy,dz , rmm , drev ;
   MRI_IMAGE *aim ; float *aar,*ath ;

#define NQVAL 12
   float qval[NQVAL] = { 1.0f, 1.3f, 1.6f, 2.0f, 2.5f, 3.0f,
                         4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f } ;
#define NPVAL (2*NQVAL+8)
#define PBASE 0.0001f
   float pval[NPVAL] ; int num_pval=NPVAL ;
   float fwhm[21] ; int num_fwhm=21 ; float dfwhm=0.25f ;
   MRI_IMAGE *maskim ; byte *mask, *mmm ;

   NI_element *nel ;
   NI_stream ns ;
   int clast , cfirst , cnum ;
   char atr[6666] ;

   float alph[ 3] = { 0.01f , 0.05f , 0.10f } ;
   int num_alph= 3 ;
   int qaa ; float aa , ff ;
   MRI_IMAGE *alpim ; float *alpar ;

   if( argc < 4 ){
     printf("args: nx nz niter\n") ; exit(0) ;
   }

   ny = nx = (int)strtod(argv[1],NULL); if( nx    < 8 ) ERROR_exit("nx bad")   ;
   nz      = (int)strtod(argv[2],NULL); if( nz    < 1 ) ERROR_exit("nz bad")   ;
   niter   = (int)strtod(argv[3],NULL); if( niter < 1 ) ERROR_exit("niter bad");

   for( ii=0 ; ii < num_pval ; ii++ ){
     jj = ii%NQVAL; kk = ii/NQVAL; pval[ii] = PBASE * qval[jj] * pow(10.0,kk);
   }
   for( ii=0 ; ii < num_fwhm ; ii++ ) fwhm[ii] = ii*dfwhm ;

   dx = dy = dz = 1.0f ; rmm = 0.0f ;

#if 0
   maskim = mri_new_vol(nx,ny,nz,MRI_byte) ; mask = MRI_BYTE_PTR(maskim) ;
   jsm = 1+nx*nx/4 ; kth = nx/2 ;
   for( kk=0 ; kk < nz ; kk++ ){
     for( jj=0 ; jj < ny ; jj++ ){
       mmm = mask + (kk*nx*ny + jj*nx) ; qq = jsm - (jj-kth)*(jj-kth) ;
       for( ii=0 ; ii < nx ; ii++ )
         mmm[ii] = ( (ii-kth)*(ii-kth) <= qq ) ;
     }
   }
   INFO_message("%d voxels in mask",THD_countmask(nx*ny*nz,mask)) ;
#else
   maskim = NULL ; mask = NULL ;
   INFO_message("%d voxels in dataset",nx*ny*nz) ;
#endif

   aim = mri_alphasim( nx,nz , dx,dz , niter,rmm ,
                       num_pval,pval , num_fwhm,fwhm,NULL , mask , 0 ) ;

   INFO_message("simulation done: CPU=%g",COX_cpu_time()) ;

   mri_free(maskim) ;

   if( aim == NULL ) ERROR_exit("aim bad") ;
   aar = MRI_FLOAT_PTR(aim) ;

#if 0
   ns = NI_stream_open( "fd:1" , "w" ) ; if( ns == NULL ) ERROR_exit("bad ns") ;
#endif
   for( qaa=0 ; qaa < num_alph ; qaa++ ){

     aa    = alph[qaa] ;
     alpim = mri_new( num_pval , num_fwhm , MRI_float ) ;
     alpar = MRI_FLOAT_PTR(alpim) ;

     for( jsm=0 ; jsm < num_fwhm ; jsm++ ){
       for( kth=0 ; kth < num_pval ; kth++ ){
         ath = ATH(jsm,kth) ;
         for( ii=MAX_CLUSTSIZE ; ii > 0 && ath[ii] < aa ; ii-- ) ; /*nada*/
         if( ii == 0 ){
           alpar[kth+jsm*num_pval] = 0.0f ;     /* failed to find cutoff */
         } else if( ii == MAX_CLUSTSIZE ){
           alpar[kth+jsm*num_pval] = MAX_CLUSTSIZE ; /* shouldn't happen */
         } else {
           ff = (ath[ii]-aa)/(ath[ii]-ath[ii+1]) ;  /* ath[ii] >= aa > ath[ii+1] */
           alpar[kth+jsm*num_pval] = ii+ff ;
         }
     }}

     nrev = 0 ; drev = 0.0f ;
     for( kth=0 ; kth < num_pval ; kth++ ){ /* edit each row to ensure it */
       for( jsm=1 ; jsm < num_fwhm ; jsm++ ){    /* is non-decreasing in fwhm */
         if( alpar[kth+jsm*num_pval] < alpar[kth+(jsm-1)*num_pval] ){
           nrev++ ; drev += alpar[kth+(jsm-1)*num_pval] - alpar[kth+jsm*num_pval] ;
           alpar[kth+jsm*num_pval] = alpar[kth+(jsm-1)*num_pval] ;
         }
       }
     }
     if( nrev > 0 ) INFO_message("%d reversals at Alpha=%g; sum=%g",nrev,aa,drev) ;

#if 0
     nel = NI_new_data_element( "AlphaSim" , num_pval ) ;
     for( jsm=0 ; jsm < num_fwhm ; jsm++ )
       NI_add_column( nel , NI_FLOAT , alpar+jsm*num_pval ) ;

     sprintf(atr,"%d %d %g",nx,nz,aa) ;
     NI_set_attribute( nel , "NxNzAlpha" , atr ) ;

     atr[0] = '\0' ;
     for( jsm=0 ; jsm < num_fwhm ; jsm++ ) sprintf(atr+strlen(atr),"%g ",fwhm[jsm]) ;
     atr[strlen(atr)-1] = '\0' ; NI_set_attribute( nel , "Fwhm" , atr ) ;

     atr[0] = '\0' ;
     for( kth=0 ; kth < num_pval ; kth++ ) sprintf(atr+strlen(atr),"%g ",pval[kth]) ;
     atr[strlen(atr)-1] = '\0' ; NI_set_attribute( nel , "Pval" , atr ) ;

     NI_write_element( ns , nel , NI_TEXT_MODE ) ;
     NI_free_element( nel ) ; mri_free(alpim) ;
#else

     for( jsm=0 ; jsm < num_fwhm ; jsm++ ){
       printf("static float alpha%02d_nx%03d_nz%02d_fwhm%03d[%d] = {\n ",
               (int)rint(aa*100.0) , nx , nz ,
               (int)rint(fwhm[jsm]*100.0) , num_pval ) ;
       for( kth=0 ; kth < num_pval ; kth++ ){
         printf("%.2f%c",alpar[kth+jsm*num_pval],(kth==num_pval-1)?'}':',' ) ;
         if( kth%10==9 && kth < num_pval-2 ) printf("\n ") ;
       }
       printf(";\n") ;
     }
     printf("static float *alpha%02d_nx%03d_nz%02d[%d] = {\n ",
             (int)rint(aa*100.0) , nx , nz , num_fwhm ) ;
     for( jsm=0 ; jsm < num_fwhm ; jsm++ ){
       printf("alpha%02d_nx%03d_nz%02d_fwhm%03d%c ",
              (int)rint(aa*100.0) , nx , nz ,
              (int)rint(fwhm[jsm]*100.0) , (jsm==num_fwhm-1)?'}':',' ) ;
       if( jsm%3==2 && jsm < num_fwhm-1 ) printf("\n ") ;
     }
     printf(";\n\n") ;
#endif

   }

   printf("static float **nx%03d_nz%02d[%d] = {\n ", nx , nz , num_alph ) ;
   for( qaa=0 ; qaa < num_alph ; qaa++ ){
     printf(" alpha%02d_nx%03d_nz%02d %c",
            (int)rint(alph[qaa]*100.0) , nx , nz ,
            (qaa==num_alph-1)?'}':',' ) ;
   }
   printf(";\n\n") ;

#if 0
   NI_stream_close( ns ) ;
#endif
   exit(0) ;
}
