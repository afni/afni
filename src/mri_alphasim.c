#include "mrilib.h"
#include <sys/types.h>
#include <unistd.h>
#include <time.h>

/*****************************************************************************/
/* gauss.c - gaussian random numbers, using the Ziggurat method
 *
 * Copyright (C) 2005  Jochen Voss.
 *
 * For details see the following article.
 *
 *     George Marsaglia, Wai Wan Tsang
 *     The Ziggurat Method for Generating Random Variables
 *     Journal of Statistical Software, vol. 5 (2000), no. 8
 *     http://www.jstatsoft.org/v05/i08/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

/* position of right-most step */

#define PARAM_R 3.44428647676f

/* tabulated values for the heigt of the Ziggurat levels */

static const float ytab[128] = {
  1, 0.963598623011, 0.936280813353, 0.913041104253,
  0.892278506696, 0.873239356919, 0.855496407634, 0.838778928349,
  0.822902083699, 0.807732738234, 0.793171045519, 0.779139726505,
  0.765577436082, 0.752434456248, 0.739669787677, 0.727249120285,
  0.715143377413, 0.703327646455, 0.691780377035, 0.68048276891,
  0.669418297233, 0.65857233912, 0.647931876189, 0.637485254896,
  0.62722199145, 0.617132611532, 0.607208517467, 0.597441877296,
  0.587825531465, 0.578352913803, 0.569017984198, 0.559815170911,
  0.550739320877, 0.541785656682, 0.532949739145, 0.524227434628,
  0.515614886373, 0.507108489253, 0.498704867478, 0.490400854812,
  0.482193476986, 0.47407993601, 0.466057596125, 0.458123971214,
  0.450276713467, 0.442513603171, 0.434832539473, 0.427231532022,
  0.419708693379, 0.41226223212, 0.404890446548, 0.397591718955,
  0.390364510382, 0.383207355816, 0.376118859788, 0.369097692334,
  0.362142585282, 0.355252328834, 0.348425768415, 0.341661801776,
  0.334959376311, 0.328317486588, 0.321735172063, 0.31521151497,
  0.308745638367, 0.302336704338, 0.29598391232, 0.289686497571,
  0.283443729739, 0.27725491156, 0.271119377649, 0.265036493387,
  0.259005653912, 0.253026283183, 0.247097833139, 0.241219782932,
  0.235391638239, 0.229612930649, 0.223883217122, 0.218202079518,
  0.212569124201, 0.206983981709, 0.201446306496, 0.195955776745,
  0.190512094256, 0.185114984406, 0.179764196185, 0.174459502324,
  0.169200699492, 0.1639876086, 0.158820075195, 0.153697969964,
  0.148621189348, 0.143589656295, 0.138603321143, 0.133662162669,
  0.128766189309, 0.123915440582, 0.119109988745, 0.114349940703,
  0.10963544023, 0.104966670533, 0.100343857232, 0.0957672718266,
  0.0912372357329, 0.0867541250127, 0.082318375932, 0.0779304915295,
  0.0735910494266, 0.0693007111742, 0.065060233529, 0.0608704821745,
  0.056732448584, 0.05264727098, 0.0486162607163, 0.0446409359769,
  0.0407230655415, 0.0368647267386, 0.0330683839378, 0.0293369977411,
  0.0256741818288, 0.0220844372634, 0.0185735200577, 0.0151490552854,
  0.0118216532614, 0.00860719483079, 0.00553245272614, 0.00265435214565
};

/* tabulated values for 2^24 times x[i]/x[i+1],
 * used to accept for U*x[i+1]<=x[i] without any floating point operations */

static const unsigned long ktab[128] = {
  0, 12590644, 14272653, 14988939,
  15384584, 15635009, 15807561, 15933577, 16029594, 16105155, 16166147, 16216399,
  16258508, 16294295, 16325078, 16351831, 16375291, 16396026, 16414479, 16431002,
  16445880, 16459343, 16471578, 16482744, 16492970, 16502368, 16511031, 16519039,
  16526459, 16533352, 16539769, 16545755, 16551348, 16556584, 16561493, 16566101,
  16570433, 16574511, 16578353, 16581977, 16585398, 16588629, 16591685, 16594575,
  16597311, 16599901, 16602354, 16604679, 16606881, 16608968, 16610945, 16612818,
  16614592, 16616272, 16617861, 16619363, 16620782, 16622121, 16623383, 16624570,
  16625685, 16626730, 16627708, 16628619, 16629465, 16630248, 16630969, 16631628,
  16632228, 16632768, 16633248, 16633671, 16634034, 16634340, 16634586, 16634774,
  16634903, 16634972, 16634980, 16634926, 16634810, 16634628, 16634381, 16634066,
  16633680, 16633222, 16632688, 16632075, 16631380, 16630598, 16629726, 16628757,
  16627686, 16626507, 16625212, 16623794, 16622243, 16620548, 16618698, 16616679,
  16614476, 16612071, 16609444, 16606571, 16603425, 16599973, 16596178, 16591995,
  16587369, 16582237, 16576520, 16570120, 16562917, 16554758, 16545450, 16534739,
  16522287, 16507638, 16490152, 16468907, 16442518, 16408804, 16364095, 16301683,
  16207738, 16047994, 15704248, 15472926
};

/* tabulated values of 2^{-24}*x[i] */

static const float wtab[128] = {
  1.62318314817e-08, 2.16291505214e-08, 2.54246305087e-08, 2.84579525938e-08,
  3.10340022482e-08, 3.33011726243e-08, 3.53439060345e-08, 3.72152672658e-08,
  3.8950989572e-08, 4.05763964764e-08, 4.21101548915e-08, 4.35664624904e-08,
  4.49563968336e-08, 4.62887864029e-08, 4.75707945735e-08, 4.88083237257e-08,
  5.00063025384e-08, 5.11688950428e-08, 5.22996558616e-08, 5.34016475624e-08,
  5.44775307871e-08, 5.55296344581e-08, 5.65600111659e-08, 5.75704813695e-08,
  5.85626690412e-08, 5.95380306862e-08, 6.04978791776e-08, 6.14434034901e-08,
  6.23756851626e-08, 6.32957121259e-08, 6.42043903937e-08, 6.51025540077e-08,
  6.59909735447e-08, 6.68703634341e-08, 6.77413882848e-08, 6.8604668381e-08,
  6.94607844804e-08, 7.03102820203e-08, 7.11536748229e-08, 7.1991448372e-08,
  7.2824062723e-08, 7.36519550992e-08, 7.44755422158e-08, 7.52952223703e-08,
  7.61113773308e-08, 7.69243740467e-08, 7.77345662086e-08, 7.85422956743e-08,
  7.93478937793e-08, 8.01516825471e-08, 8.09539758128e-08, 8.17550802699e-08,
  8.25552964535e-08, 8.33549196661e-08, 8.41542408569e-08, 8.49535474601e-08,
  8.57531242006e-08, 8.65532538723e-08, 8.73542180955e-08, 8.8156298059e-08,
  8.89597752521e-08, 8.97649321908e-08, 9.05720531451e-08, 9.138142487e-08,
  9.21933373471e-08, 9.30080845407e-08, 9.38259651738e-08, 9.46472835298e-08,
  9.54723502847e-08, 9.63014833769e-08, 9.71350089201e-08, 9.79732621669e-08,
  9.88165885297e-08, 9.96653446693e-08, 1.00519899658e-07, 1.0138063623e-07,
  1.02247952126e-07, 1.03122261554e-07, 1.04003996769e-07, 1.04893609795e-07,
  1.05791574313e-07, 1.06698387725e-07, 1.07614573423e-07, 1.08540683296e-07,
  1.09477300508e-07, 1.1042504257e-07, 1.11384564771e-07, 1.12356564007e-07,
  1.13341783071e-07, 1.14341015475e-07, 1.15355110887e-07, 1.16384981291e-07,
  1.17431607977e-07, 1.18496049514e-07, 1.19579450872e-07, 1.20683053909e-07,
  1.21808209468e-07, 1.2295639141e-07, 1.24129212952e-07, 1.25328445797e-07,
  1.26556042658e-07, 1.27814163916e-07, 1.29105209375e-07, 1.30431856341e-07,
  1.31797105598e-07, 1.3320433736e-07, 1.34657379914e-07, 1.36160594606e-07,
  1.37718982103e-07, 1.39338316679e-07, 1.41025317971e-07, 1.42787873535e-07,
  1.44635331499e-07, 1.4657889173e-07, 1.48632138436e-07, 1.50811780719e-07,
  1.53138707402e-07, 1.55639532047e-07, 1.58348931426e-07, 1.61313325908e-07,
  1.64596952856e-07, 1.68292495203e-07, 1.72541128694e-07, 1.77574279496e-07,
  1.83813550477e-07, 1.92166040885e-07, 2.05295471952e-07, 2.22600839893e-07
};

static INLINE float zgaussian(void)
{
  unsigned long  U, sgn, i, j;
  float  x, y;

  while (1) {
    U   = (unsigned long)mrand48() ;
    i   = U & 0x0000007F;  /* 7 bit to choose the step */
    sgn = U & 0x00000080;  /* 1 bit for the sign */
    j   = U>>8;            /* 24 bit for the x-value */

    x = j*wtab[i]; if( j < ktab[i] ) break;

    if( i < 127 ){
      float  y0, y1;
      y0 = ytab[i];
      y1 = ytab[i+1];
      y = y1+(y0-y1)*drand48();
    } else {
      x = PARAM_R - log(1.0-drand48())/PARAM_R;
      y = exp(-PARAM_R*(x-0.5*PARAM_R))*drand48();
    }
    if( y < expf(-0.5f*x*x) ) break;
  }
  return (sgn ? x : -x) ;
}
/*****************************************************************************/

static intvec * count_clusters( MRI_IMAGE *bim , float rmm , int minsize ) ;

static int largest_clustersize( MRI_IMAGE *bim , float rmm ) ;

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
   intvec *iv ; int niv , *iva ;
   static long sseed=0 ;

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

#undef  TPI
#define TPI 6.283185f

     for( ii=0 ; ii < nxyz ; ii+=2 ){
       do{ u1 = (float)drand48(); } while( u1==0.0f ) ;
       u1 = sqrtf(-logf(u1)) ; u2 = TPI * (float)drand48() ;
       bar[ii] = u1 * cosf(u2) ; bar[ii+1] = u1 * sinf(u2) ;
     }
     if( ii == nxyz-1 ){
       do{ u1 = (float)drand48(); } while( u1==0.0f ) ;
       u1 = sqrtf(-logf(u1)) ; u2 = TPI * (float)drand48() ;
       bar[ii] = u1 * cosf(u2) ;
     }

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
            ath = ATH(jsm,kth) ; ath[jj] += 1.0f ;
          }
#endif

       } /* end of loop over thresholds */

     } /* end of loop over smoothings */

   } /* end of iterations */

   mri_free(dim) ; mri_free(cim) ; mri_free(bim) ; free((void *)thr) ;

   /* convert to rates of clusters >= given size */

#if 0
   for( jsm=0 ; jsm < num_fwhm ; jsm++ ){
     for( kth=0 ; kth < num_pval ; kth++ ){
       ath = ATH(jsm,kth) ;
       for( jj=max_clustsize-1 ; jj >= 1 ; jj-- ) ath[jj] += ath[jj+1] ;
     }
   }
#endif

   RETURN(aim) ;
}

/*----------------------------------------------------------------------------*/

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

/**********************************************************/

int main( int argc , char *argv[] )
{
   int nx,ny,nz , niter , num_pval=5,num_fwhm=5 , jsm,kth,ii , max_clustsize=10000 ;
   float dx,dy,dz , rmm ;
   float pval[5] = { 0.01f , 0.005f , 0.002f , 0.001f , 0.0005f } ;
   float fwhm[5] = { 0.0f  , 2.0f   , 4.0f   , 6.0f   , 8.0f    } ;
   MRI_IMAGE *aim ; float *aar,*ath ;

   if( argc < 9 ){
     printf("args: nx ny nz dx dy dz rmm niter\n") ; exit(0) ;
   }

   nx = (int)strtod(argv[1],NULL) ; if( nx < 8 ) ERROR_exit("nx") ;
   ny = (int)strtod(argv[2],NULL) ; if( ny < 8 ) ERROR_exit("ny") ;
   nz = (int)strtod(argv[3],NULL) ; if( nz < 1 ) ERROR_exit("nz") ;
   dx = (float)strtod(argv[4],NULL) ;
   dy = (float)strtod(argv[5],NULL) ;
   dz = (float)strtod(argv[6],NULL) ;
   rmm = (float)strtod(argv[7],NULL) ;
   niter = (int)strtod(argv[8],NULL) ;

   aim = mri_alphasim( nx,ny,nz , dx,dy,dz , niter , max_clustsize , rmm ,
                       num_pval,pval , num_fwhm,fwhm , NULL , 0 ) ;

   if( aim == NULL ) ERROR_exit("aim") ;
   aar = MRI_FLOAT_PTR(aim) ;

   for( jsm=0 ; jsm < num_fwhm ; jsm++ ){
     for( kth=0 ; kth < num_pval ; kth++ ){
       printf("\n********************************************************\n\n"
              "***** Blur = %g  pval = %g *****\n"
              " Size   Rate\n"
              "------ ----------\n" , fwhm[jsm] , pval[kth] ) ;
       ath = ATH(jsm,kth) ;
       for( ii=1 ; ii <= max_clustsize ; ii++ )
         if( ath[ii] > 0.0f ) printf( "%6d  %.6g\n" , ii , ath[ii] ) ;
   }}

   exit(0) ;
}
