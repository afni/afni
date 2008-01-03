#include "mrilib.h"
#include <sys/types.h>
#include <unistd.h>
#include <time.h>

static intvec * count_clusters( MRI_IMAGE *bim , float rmm , int minsize ) ;

MRI_IMAGE * mri_alphasim( int   nx, int   ny, int   nz ,
                          float dx, float dy, float dz ,
                          int niter , int max_clustsize , float rmm ,
                          int num_pval , float *pval ,
                          int num_fwhm , float *fwhm , byte *mask , long seed )
{
   MRI_IMAGE *bim , *tim , *aim , *cim ,      *dim ;
   float     *bar , *tar , *aar , *car ; byte *dar ;
   int ite , jsm , kth , nxyz , ii ;
   float *thr, tt , u1,u2 , *ath ;
   double sd ;
   intvec *iv ; int niv , *iva ;
   static long sseed=0 ;

ENTRY("mri_alphasim") ;

   if( nx < 8 || ny < 8 || nz < 1 ) RETURN(NULL) ;

   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;

   if( niter < 1 ) niter = 1000 ;
   if( max_clustsize < 16 ) max_clustsize = 10000 ;

   if( num_pval < 1 || pval == NULL ){
     static float pp = 0.001f ;
     num_pval = 1 ; pval = &pp ;
   }
   thr = (float *)malloc(sizeof(float)*num_pval) ;

   if( num_fwhm < 1 || fwhm == NULL ){
     static float ff = 0.0f ;
     num_fwhm = 1 ; fwhm = &ff ;
   }

   bim = mri_new_vol( nx,ny,nz , MRI_float ) ; bar = MRI_FLOAT_PTR(bim) ;
   tim = mri_new_vol( nx,ny,nz , MRI_float ) ; tar = MRI_FLOAT_PTR(tim) ;
   dim = mri_new_vol( nx,ny,nz , MRI_byte  ) ; dar = MRI_BYTE_PTR (dim) ;
   dim->dx = dx ; dim->dy = dy ; dim->dz = dz ;

   aim = mri_new_vol( max_clustsize , num_fwhm , num_pval , MRI_float ) ;
   aar = MRI_FLOAT_PTR(aim) ;
#undef  ATH
#define ATH(s,t) (aar+((s)*num_fwhm+(t)*(num_fwhm*num_pval)))

   nxyz = nx*ny*nz ;

   if( seed != 0 ){
     srand48(seed) ;
   } else if( sseed == 0 ){
     sseed = (long)time(NULL) + (long)getpid() ;
     srand48(sseed) ;
   }

   /*-- iteration loop --*/

   for( ite=0 ; ite < niter ; ite++ ){

     /*-- create uncorrelated random field --*/

#undef  TPI
#define TPI 6.283185f

     for( ii=0 ; ii < nxyz ; ii+=2 ){
       do{ u1 = (float)drand48(); } while( u1==0.0f ) ;
       u1 = sqrtf(-2.0f*logf(u1)) ; u2 = TPI * (float)drand48() ;
       bar[ii] = u1 * cosf(u2) ; bar[ii+1] = u1 * sinf(u2) ;
     }
     if( ii == nxyz-1 ){
       do{ u1 = (float)drand48(); } while( u1==0.0f ) ;
       u1 = sqrtf(-2.0f*logf(u1)) ; u2 = TPI * (float)drand48() ;
       bar[ii] = u1 * cosf(u2) ;
     }

     /*-- loop over smoothings --*/

     for( jsm=0 ; jsm < num_fwhm ; jsm++ ){

       /* blur dataset */

       cim = mri_copy(bim) ; car = MRI_FLOAT_PTR(cim) ;
       if( fwhm[jsm] > 0.0f )
         EDIT_blur_volume( nx,ny,nz , dx,dy,dz , MRI_float,car , fwhm[jsm] ) ;

       /* find sigma of blurred dataset (we know the mean is zero) */

       sd = 0.0 ;
       for( ii=0 ; ii < nxyz ; ii++ ) sd += car[ii]*car[ii] ;
       sd = sqrt(sd/nxyz) ;

       /* find thresholds for p-values in blurred dataset */

       for( kth=0 ; kth < num_pval ; kth++ ){
         thr[kth] = sd * nifti_rcdf2stat( (double)pval[kth] ,
                                          NIFTI_INTENT_ZSCORE , 0.0,0.0,0.0 ) ;
       }

       /* mask blurred dataset */

       if( mask != NULL )
         for( ii=0 ; ii < nxyz ; ii++ ) if( mask[ii] == 0 ) car[ii] = 0.0f ;

       /*-- loop over per-voxel thresholds --*/

       for( kth=0 ; kth < num_pval ; kth++ ){

          /* threshold at thr[kth] */

          tt = thr[kth] ;
          for( ii=0 ; ii < nxyz ; ii++ ) dar[ii] = (car[ii] >= tt) ;

          /* clusterize and count into aar[ csize + jsm*num_fwhm + kth*num_fwhm*num_pval ] */

          iv = count_clusters( dim , rmm , 1 ) ;
          if( iv != NULL ){
            niv = iv->nar ; iva = iv->ar ;
            ath = ATH(jsm,kth) ;
            for( ii=0 ; ii < niv ; ii++ ) ath[iva[ii]]++ ;
            KILL_intvec(iv) ;
          }

       } /* end of loop over thresholds */

       mri_free(cim) ;

     } /* end of loop over smoothings */

   } /* end of iterations */

   /* normalize aar[] to be alpha instead of counts */

   mri_free(dim) ; mri_free(tim) ; mri_free(bim) ;
   RETURN(aim) ;
}

/*--------------------------------------------------------------------------*/

static intvec * count_clusters( MRI_IMAGE *bim , float rmm , int minsize )
{
   intvec *iv ;
   int nx,ny,nz , nclu ;
   MCW_cluster *clust , *mask ;
   int nxy,nxyz , ijk , ijk_last , mnum ;
   int icl , jma , ijkma ;
   float dx,dy,dz ;
   byte  *bfar ;
   short ic, jc, kc , im, jm, km, *mi,*mj,*mk ;

ENTRY("count clusters") ;

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

   nclu = ijk_last = 0 ; INIT_CLUSTER(clust) ; MAKE_INTVEC(iv,16) ;
   do {
     for( ijk=ijk_last ; ijk < nxyz && bfar[ijk] != 0 ; ijk++ ) ; /*nada*/
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
