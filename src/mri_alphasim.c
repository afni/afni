#include "mrilib.h"
#include <sys/types.h>
#include <unistd.h>
#include <time.h>

#include "zgaussian.c"   /** fast function for generating Gaussian deviates **/

/*----------------------------------------------------------------------------*/

#if 0
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

ENTRY("count_clusters") ;

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

/*----------------------------------------------------------------------------*/

static int largest_clustersize( MRI_IMAGE *bim , float rmm )
{
   int nx,ny,nz , nclu , biggest=0 ;
   MCW_cluster *clust , *mask ;
   int nxy,nxyz , ijk , ijk_last , mnum ;
   int icl , jma , ijkma ;
   float dx,dy,dz ;
   byte  *bfar ;
   short ic, jc, kc , im, jm, km, *mi,*mj,*mk ;

ENTRY("largest_clustersize") ;

   if( bim == NULL || bim->kind != MRI_byte ) RETURN(0) ;
   bfar = MRI_BYTE_PTR(bim) ;
   nx = bim->nx ; ny = bim->ny ; nz = bim->nz ;
   dx = bim->dx ; dy = bim->dy ; dz = bim->dz ;
   if( rmm <= 0.0f ){ dx = dy = dz = 1.0f ; rmm = 1.01f ; }

   /*--- make a cluster that is a mask of points closer than max_dist ---*/

   mask = MCW_build_mask( dx, dy, dz, rmm ) ;
   if( mask == NULL ){
     mask = MCW_build_mask( 1.0f,1.0f,1.0f, 1.01f ) ;
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

     if( clust->num_pt > biggest ) biggest = clust->num_pt ;
   } while( 1 ) ;

   KILL_CLUSTER(clust) ; KILL_CLUSTER(mask) ;
   RETURN(biggest) ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_alphasim( int   nx, int   ny, int   nz ,
                          float dx, float dy, float dz ,
                          int niter , int max_clustsize , float rmm ,
                          int num_pval , float *pval ,
                          int num_fwhm , float *fwhm , byte *mask , long seed )
{
   MRI_IMAGE *bim , *aim , *cim ,      *dim ;
   float     *bar , *aar , *car ; byte *dar ;
   int ite , jsm , kth , nxyz , ii,jj ;
   float tt , u1,u2 , *ath , nitinv , *thr ;
   double sd ;
   static long sseed=0 ;
#if 0
   intvec *iv ; int niv , *iva ;
#endif

ENTRY("mri_alphasim") ;

   if( nx < 8 || ny < 8 || nz < 1 ) RETURN(NULL) ;

   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;

   if( niter         < 1  ) niter         =  1000 ;
   if( max_clustsize < 16 ) max_clustsize = 10000 ;

   if( num_pval < 1 || pval == NULL ){
     static float pp = 0.001f ;
     num_pval = 1 ; pval = &pp ;
   }

   if( num_fwhm < 1 || fwhm == NULL ){
     static float ff = 0.0f ;
     num_fwhm = 1 ; fwhm = &ff ;
   }

   aim = mri_new_vol( max_clustsize , num_fwhm , num_pval , MRI_float ) ;
   aar = MRI_FLOAT_PTR(aim) ;
#undef  ATH
#define ATH(s,t) ( aar + ((s)*max_clustsize + (t)*(max_clustsize*num_fwhm) -1) )

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

   bim = mri_new_vol( nx,ny,nz , MRI_float ) ; bar = MRI_FLOAT_PTR(bim) ;
   cim = mri_new_vol( nx,ny,nz , MRI_float ) ; car = MRI_FLOAT_PTR(bim) ;
   dim = mri_new_vol( nx,ny,nz , MRI_byte  ) ; dar = MRI_BYTE_PTR (dim) ;
   dim->dx = dx ; dim->dy = dy ; dim->dz = dz ;

   /*-- iteration loop --*/

   nitinv = 1.0f / niter ;

   for( ite=0 ; ite < niter ; ite++ ){

     /*-- create uncorrelated random field --*/

     for( ii=0 ; ii < nxyz ; ii++ ) bar[ii] = zgaussian() ;

     /*-- loop over smoothings --*/

     for( jsm=0 ; jsm < num_fwhm ; jsm++ ){

       /* blur dataset */

       memcpy( car , bar , sizeof(float)*nxyz ) ;
       if( fwhm[jsm] > 0.0f )
         EDIT_blur_volume( nx,ny,nz , dx,dy,dz ,
                           MRI_float,car , FWHM_TO_SIGMA(fwhm[jsm]) ) ;

       /* find sigma of blurred dataset (we know the mean is zero) */

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

#if 0
          iv = count_clusters( dim , rmm , 1 ) ;
          if( iv != NULL ){
            niv = iv->nar ; iva = iv->ar ; ath = ATH(jsm,kth) ;
            for( ii=0 ; ii < niv ; ii++ ){
              jj = iva[ii] ; if( jj > max_clustsize ) jj = max_clustsize ;
              ath[jj] += nitinv ;
            }
            KILL_intvec(iv) ;
          }
#else
          jj = largest_clustersize( dim , rmm ) ;
          if( jj > 0 ){
            if( jj > max_clustsize ) jj = max_clustsize ;
            ath = ATH(jsm,kth); ath[jj] += 1.0f ;
          }
#endif

       } /* end of loop over thresholds */
     } /* end of loop over smoothings */
   } /* end of iterations */

   mri_free(dim) ; mri_free(cim) ; mri_free(bim) ; free((void *)thr) ;

   /* convert x-th entry to prob(largest cluster size >= x) */

#if 1
   for( jsm=0 ; jsm < num_fwhm ; jsm++ ){
     for( kth=0 ; kth < num_pval ; kth++ ){
       ath = ATH(jsm,kth) ;
       for( jj=max_clustsize-1 ; jj >= 1 ; jj-- ) ath[jj] += ath[jj+1] ;
       for( jj=1 ; jj <= max_clustsize ; jj++ ) ath[jj] *= nitinv ;
     }
   }
#endif

   RETURN(aim) ;
}

/**********************************************************/

int main( int argc , char *argv[] )
{
   int nx,ny,nz , niter , jsm,kth,ii,jj,kk,qq , max_clustsize=32768 ;
   float dx,dy,dz , rmm ;
   MRI_IMAGE *aim ; float *aar,*ath ;

   float pval[28] ; int num_pval=28 ;
   float fwhm[21] ; int num_fwhm=21 ;
   MRI_IMAGE *maskim ; byte *mask, *mmm ;

   NI_element *nel ;
   NI_stream ns ;
   int clast , cfirst , cnum ;
   char atr[256] ;

   if( argc < 4 ){
     printf("args: nx nz niter\n") ; exit(0) ;
   }

   ny = nx = (int)strtod(argv[1],NULL); if( nx    < 8 ) ERROR_exit("nx bad")   ;
   nz      = (int)strtod(argv[2],NULL); if( nz    < 1 ) ERROR_exit("nz bad")   ;
   niter   = (int)strtod(argv[3],NULL); if( niter < 1 ) ERROR_exit("niter bad");

   for( ii=0 ; ii < 28 ; ii++ ) pval[ii] = (float)pow( 10.0 , 0.1*ii-4.0 ) ;
   for( ii=0 ; ii < 21 ; ii++ ) fwhm[ii] = ii*0.25 ;

   dx = dy = dz = 1.0f ; rmm = 0.0f ;

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

   aim = mri_alphasim( nx,ny,nz , dx,dy,dz , niter , max_clustsize , rmm ,
                       num_pval,pval , num_fwhm,fwhm , NULL , 0 ) ;

   INFO_message("simulation done: CPU=%g",COX_cpu_time()) ;

   mri_free(maskim) ;

   if( aim == NULL ) ERROR_exit("aim bad") ;
   aar = MRI_FLOAT_PTR(aim) ;

   ns = NI_stream_open( "fd:1" , "w" ) ; if( ns == NULL ) ERROR_exit("bad ns") ;
   for( jsm=0 ; jsm < num_fwhm ; jsm++ ){
     for( kth=0 ; kth < num_pval ; kth++ ){
       ath = ATH(jsm,kth) ;
       for( ii=max_clustsize ; ii > 0 && ath[ii] < 0.01f ; ii-- ) ; /*nada*/
       if( ii == 0 ) continue ;  /* should not happen */
       clast = MIN(max_clustsize,ii+1) ;
       for( ii=2 ; ii <= clast && ath[ii] > 0.5f ; ii++ ) ; /*nada*/
       cfirst = ii-1 ; cnum = clast-cfirst+1 ;
       nel = NI_new_data_element( "AlphaSim" , cnum ) ;
       NI_add_column( nel , NI_FLOAT , ath+cfirst ) ;
       sprintf(atr,"%d %d %g %g %d",nx,nz,pval[kth],fwhm[jsm],cfirst) ;
       NI_set_attribute( nel , "NxNzPvalBlurCfirst" , atr ) ;
       NI_write_element( ns , nel , NI_TEXT_MODE ) ;
       NI_free_element( nel ) ;
   }}

   NI_stream_close( ns ) ;
   exit(0) ;
}
