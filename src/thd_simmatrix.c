#include "mrilib.h"
#include "thd_simmatrix.h"

/*----------------------------------------------------------------------------
  Item-by-item similarity matrices.  See thd_simmatrix.h.
                                                     -- P Molfese, Jul 2026
------------------------------------------------------------------------------*/

/*============================================================================*/
/*  Life cycle                                                                */
/*============================================================================*/

THD_simmat * THD_simmat_new( int n )
{
   THD_simmat *sm ;
   if( n < 2 ) return NULL ;
   sm = (THD_simmat *)calloc(1,sizeof(THD_simmat)) ;
   if( sm == NULL ) return NULL ;
   sm->n   = n ;
   sm->mat = (float *)calloc((size_t)n*n,sizeof(float)) ;
   if( sm->mat == NULL ){ free(sm) ; return NULL ; }
   sm->name[0] = '\0' ;
   return sm ;
}

void THD_simmat_free( THD_simmat *sm )
{
   if( sm == NULL ) return ;
   if( sm->mat != NULL ) free(sm->mat) ;
   free(sm) ;
}

/*============================================================================*/
/*  Ranking                                                                   */
/*============================================================================*/

typedef struct { float v ; int i ; } fi_pair ;

static int fi_cmp( const void *a , const void *b )
{
   float d = ((const fi_pair *)a)->v - ((const fi_pair *)b)->v ;
   return (d < 0.0f) ? -1 : (d > 0.0f) ? 1 : 0 ;
}

void THD_rank_avg( int n , float *x , float *rk )
{
   fi_pair *fp ; int ii , jj ; float rsum ;

   if( n<1 || x==NULL || rk==NULL ) return ;
   for( ii=0 ; ii<n ; ii++ ) if( !isfinite(x[ii]) ){
     for( jj=0 ; jj<n ; jj++ ) rk[jj]=NAN ;
     return ;
   }
   fp = (fi_pair *)malloc(sizeof(fi_pair)*n) ;
   if( fp==NULL ){ for( ii=0 ; ii<n ; ii++ ) rk[ii]=NAN ; return ; }
   for( ii=0 ; ii < n ; ii++ ){ fp[ii].v = x[ii] ; fp[ii].i = ii ; }
   qsort( fp , n , sizeof(fi_pair) , fi_cmp ) ;

   for( ii=0 ; ii < n ; ){
     for( jj=ii+1 ; jj < n && fp[jj].v == fp[ii].v ; jj++ ) ; /* tie run */
     rsum = 0.5f*((float)(ii+1) + (float)jj) ;   /* mean of ranks ii+1..jj */
     for( ; ii < jj ; ii++ ) rk[ fp[ii].i ] = rsum ;
   }
   free(fp) ;
}

/*============================================================================*/
/*  Construction                                                              */
/*============================================================================*/

int THD_simmat_fill_from_features( THD_simmat *sm, int nfeat, float *F,
                                    int metric, float *sc1, float *sc2 )
{
   int nit,ii,jj,kk,ownsc=0 ; size_t qq,ntot ; float val,sa,sb,sab,dd ;

   if( sm == NULL || sm->mat == NULL || sm->n < 2 || nfeat < 2 || F == NULL ||
       metric < SIM_PEARSON || metric > SIM_EUCLID ) return 1 ;
   nit=sm->n ;
   ntot=(size_t)nit*(size_t)nfeat ;
   for( qq=0 ; qq<ntot ; qq++ ) if( !isfinite(F[qq]) ) return 1 ;
   sm->is_dist = (metric == SIM_EUCLID) ;

   if( metric == SIM_SPEARMAN && (sc1 == NULL || sc2 == NULL) ){
     /* THD_spearman_corr is destructive. */
     sc1 = (float *)malloc(sizeof(float)*nfeat) ;
     sc2 = (float *)malloc(sizeof(float)*nfeat) ;
     ownsc=1 ;
     if( sc1 == NULL || sc2 == NULL ){
       free(sc1) ; free(sc2) ; return 1 ;
     }
   }

   for( ii=0 ; ii < nit ; ii++ ){
     sm->mat[ii*nit+ii] = sm->is_dist ? 0.0f : 1.0f ;

     for( jj=ii+1 ; jj < nit ; jj++ ){
       float *a = F + (size_t)ii*nfeat , *b = F + (size_t)jj*nfeat ;

       switch( metric ){
         case SIM_PEARSON:
           val = THD_pearson_corr( nfeat , a , b ) ;
         break ;

         case SIM_SPEARMAN:
           memcpy( sc1 , a , sizeof(float)*nfeat ) ;
           memcpy( sc2 , b , sizeof(float)*nfeat ) ;
           val = THD_spearman_corr( nfeat , sc1 , sc2 ) ;
         break ;

         case SIM_COSINE:
           sa = sb = sab = 0.0f ;
           for( kk=0 ; kk < nfeat ; kk++ ){
             sab += a[kk]*b[kk] ; sa += a[kk]*a[kk] ; sb += b[kk]*b[kk] ;
           }
           val = (sa > 0.0f && sb > 0.0f) ? sab/sqrtf(sa*sb) : 0.0f ;
         break ;

         case SIM_EUCLID:
           sab = 0.0f ;
           for( kk=0 ; kk < nfeat ; kk++ ){ dd = a[kk]-b[kk] ; sab += dd*dd ; }
           val = sqrtf(sab) ;
         break ;
       }
       if( !isfinite(val) ){
         if( ownsc ){ free(sc1) ; free(sc2) ; }
         return 1 ;
       }
       sm->mat[ii*nit+jj] = sm->mat[jj*nit+ii] = val ;
     }
   }

   if( ownsc ){ free(sc1) ; free(sc2) ; }
   return 0 ;
}

THD_simmat * THD_simmat_from_features( int nit , int nfeat , float *F , int metric )
{
   THD_simmat *sm ;
   if( nit < 2 || nfeat < 2 || F == NULL ) return NULL ;
   sm=THD_simmat_new(nit) ;
   if( sm == NULL || THD_simmat_fill_from_features(sm,nfeat,F,metric,NULL,NULL) ){
     THD_simmat_free(sm) ; return NULL ;
   }
   return sm ;
}

/*----------------------------------------------------------------------------*/
/*! Circular relative-lag table used by the F19 time-shift null.  Computing the
    lags present in need[] directly is O(npair*nused*T), thread-safe for
    arbitrary T, and bounded independently of duplicate draws.  The old path
    was O(nperm*npair*T), rebuilt shifted copies, and allocated a new similarity
    matrix for every draw. */

int THD_simmat_lag_table( int nit, int nfeat, float *F, int metric,
                          unsigned char *need, float *table,
                          float *prep, float *norm )
{
   int ii,jj,kk,ll,ky,pair=0 ; size_t qq,ntot ; float mean,ss,dot,dd,*x,*y ;

   if( nit < 2 || nfeat < 2 || F == NULL || table == NULL ||
       prep == NULL || norm == NULL || metric < SIM_PEARSON ||
       metric > SIM_EUCLID ) return 1 ;
   ntot=(size_t)nit*(size_t)nfeat ;
   for( qq=0 ; qq<ntot ; qq++ ) if( !isfinite(F[qq]) ) return 1 ;

   /* Prepare each subject once.  Circular rotation preserves ranks, means,
      sums of squares, and norms, so only the pairwise cross-product varies. */
   for( ii=0 ; ii < nit ; ii++ ){
     x=prep+(size_t)ii*nfeat ;
     memcpy(x,F+(size_t)ii*nfeat,sizeof(float)*nfeat) ;
     if( metric == SIM_SPEARMAN ) rank_order_float(nfeat,x) ;
     if( metric == SIM_PEARSON || metric == SIM_SPEARMAN ){
       if( metric == SIM_SPEARMAN ) mean=0.5f*(nfeat-1) ;
       else {
         for( mean=0.0f,kk=0 ; kk < nfeat ; kk++ ) mean += x[kk] ;
         mean /= (float)nfeat ;
       }
       for( kk=0 ; kk < nfeat ; kk++ ) x[kk] -= mean ;
     }
     for( ss=0.0f,kk=0 ; kk < nfeat ; kk++ ) ss += x[kk]*x[kk] ;
     norm[ii]=ss ;
   }

   for( ii=0 ; ii < nit ; ii++ ) for( jj=ii+1 ; jj < nit ; jj++,pair++ ){
     x=prep+(size_t)ii*nfeat ; y=prep+(size_t)jj*nfeat ;
     for( ll=0 ; ll < nfeat ; ll++ ){
       if( need != NULL && !need[(size_t)pair*nfeat+ll] ) continue ;
       if( metric == SIM_EUCLID ){
         for( ss=0.0f,kk=0,ky=ll ; kk < nfeat ; kk++ ){
           dd=x[kk]-y[ky] ; ss += dd*dd ; if( ++ky == nfeat ) ky=0 ;
         }
         table[(size_t)pair*nfeat+ll]=sqrtf(ss) ;
       } else {
         for( dot=0.0f,kk=0,ky=ll ; kk < nfeat ; kk++ ){
           dot += x[kk]*y[ky] ; if( ++ky == nfeat ) ky=0 ;
         }
         table[(size_t)pair*nfeat+ll]
           = (norm[ii] > 0.0f && norm[jj] > 0.0f)
             ? dot/sqrtf(norm[ii]*norm[jj]) : 0.0f ;
       }
       if( !isfinite(table[(size_t)pair*nfeat+ll]) ) return 1 ;
     }
   }
   return 0 ;
}

int THD_simmat_from_lag_table( THD_simmat *sm, int nfeat,
                               float *table, int *offset )
{
   int ii,jj,pair=0,lag,n ;
   if( sm == NULL || sm->mat == NULL || sm->n < 2 || nfeat < 2 ||
       table == NULL || offset == NULL ) return 1 ;
   n=sm->n ;
   for( ii=0 ; ii < n ; ii++ ){
     sm->mat[ii*n+ii]=sm->is_dist ? 0.0f : 1.0f ;
     for( jj=ii+1 ; jj < n ; jj++,pair++ ){
       lag=(offset[jj]-offset[ii])%nfeat ; if( lag < 0 ) lag += nfeat ;
       sm->mat[ii*n+jj]=sm->mat[jj*n+ii]=table[(size_t)pair*nfeat+lag] ;
     }
   }
   return 0 ;
}

/*----------------------------------------------------------------------------*/

THD_simmat * THD_simmat_from_column( int n , float *x , int rule )
{
   THD_simmat *sm ; float *v ; int ii , jj ; float dmax=0.0f , d , s ;

   if( n < 2 || x == NULL || rule < RUL_ANNAK || rule > RUL_ABSDIFF ) return NULL ;
   for( ii=0 ; ii<n ; ii++ ) if( !isfinite(x[ii]) ) return NULL ;

   sm = THD_simmat_new(n) ;
   if( sm==NULL ) return NULL ;
   sm->is_dist = (rule == RUL_ABSDIFF) ;
   v = (float *)malloc(sizeof(float)*n) ;
   if( v==NULL ){ THD_simmat_free(sm) ; return NULL ; }

   if( rule == RUL_ANNAK || rule == RUL_NN ) THD_rank_avg( n , x , v ) ;
   else                                      memcpy( v , x , sizeof(float)*n ) ;

   if( rule == RUL_ANNAK ){
     for( ii=0 ; ii < n ; ii++ ){
       sm->mat[ii*n+ii] = 1.0f ;
       for( jj=ii+1 ; jj < n ; jj++ ){
         s = 0.5f*(v[ii]+v[jj]) / (float)n ;
         sm->mat[ii*n+jj] = sm->mat[jj*n+ii] = s ;
       }
     }
   } else {
     for( ii=0 ; ii < n ; ii++ )
       for( jj=ii+1 ; jj < n ; jj++ ){
         d = fabsf( v[ii] - v[jj] ) ;
         sm->mat[ii*n+jj] = sm->mat[jj*n+ii] = d ;
         if( d > dmax ) dmax = d ;
       }

     if( rule != RUL_ABSDIFF ){          /* rescale distance -> similarity */
       if( dmax <= 0.0f ) dmax = 1.0f ;
       for( ii=0 ; ii < n ; ii++ ){
         sm->mat[ii*n+ii] = 1.0f ;
         for( jj=ii+1 ; jj < n ; jj++ ){
           s = 1.0f - sm->mat[ii*n+jj]/dmax ;
           sm->mat[ii*n+jj] = sm->mat[jj*n+ii] = s ;
         }
       }
     }
   }
   free(v) ; return sm ;
}

THD_simmat * THD_simmat_from_profile( int n , int p , float **cols )
{
   THD_simmat *sm ; float *Z , *mean , *sd ; int ii , jj , kk ;
   float dmax=0.0f , d , dd , s ;

   if( n < 2 || p < 1 || cols == NULL ) return NULL ;
   for( kk=0 ; kk<p ; kk++ ){
     if( cols[kk]==NULL ) return NULL ;
     for( ii=0 ; ii<n ; ii++ ) if( !isfinite(cols[kk][ii]) ) return NULL ;
   }

   /* standardize each column across subjects */
   Z    = (float *)malloc(sizeof(float)*(size_t)n*p) ;
   mean = (float *)calloc(p,sizeof(float)) ;
   sd   = (float *)calloc(p,sizeof(float)) ;
   if( Z==NULL || mean==NULL || sd==NULL ){
     free(Z) ; free(mean) ; free(sd) ; return NULL ;
   }

   for( kk=0 ; kk < p ; kk++ ){
     for( ii=0 ; ii < n ; ii++ ) mean[kk] += cols[kk][ii] ;
     mean[kk] /= (float)n ;
     for( ii=0 ; ii < n ; ii++ ){ d = cols[kk][ii]-mean[kk] ; sd[kk] += d*d ; }
     sd[kk] = sqrtf( sd[kk]/(float)n ) ;
     if( sd[kk] <= 0.0f ) sd[kk] = 1.0f ;
     for( ii=0 ; ii < n ; ii++ )
       Z[ ii*p + kk ] = (cols[kk][ii]-mean[kk]) / sd[kk] ;
   }

   sm = THD_simmat_new(n) ;
   if( sm==NULL ){ free(Z) ; free(mean) ; free(sd) ; return NULL ; }
   sm->is_dist = 0 ;

   for( ii=0 ; ii < n ; ii++ )
     for( jj=ii+1 ; jj < n ; jj++ ){
       for( d=0.0f,kk=0 ; kk < p ; kk++ ){ dd = Z[ii*p+kk]-Z[jj*p+kk] ; d += dd*dd ; }
       d = sqrtf(d) ;
       if( !isfinite(d) ){ free(Z) ; free(mean) ; free(sd) ; THD_simmat_free(sm) ; return NULL ; }
       sm->mat[ii*n+jj] = sm->mat[jj*n+ii] = d ;
       if( d > dmax ) dmax = d ;
     }

   if( dmax <= 0.0f ) dmax = 1.0f ;
   for( ii=0 ; ii < n ; ii++ ){
     sm->mat[ii*n+ii] = 1.0f ;
     for( jj=ii+1 ; jj < n ; jj++ ){
       s = 1.0f - sm->mat[ii*n+jj]/dmax ;
       sm->mat[ii*n+jj] = sm->mat[jj*n+ii] = s ;
     }
   }

   free(Z) ; free(mean) ; free(sd) ;
   return sm ;
}

/*----------------------------------------------------------------------------*/
/*! Mahalanobis multivariate profile.  See the header.                         */

THD_simmat * THD_simmat_from_profile_mahal( int n , int p , float **cols ,
                                            float *shrink , int *erank )
{
   THD_simmat *sm ; float *Z ; double *R , *V , *ev , *Rinv ;
   int ii , jj , kk , ll ; double d , dd , delta , dmax=0.0 ;

   if( n < 3 || p < 2 || cols == NULL ) return NULL ;

   /* z-score each column; a constant or non-finite column is fatal here (unlike
      the Euclidean profile, a zero-variance measure has no whitening) */
   Z = (float *)malloc(sizeof(float)*(size_t)n*p) ;
   if( Z==NULL ) return NULL ;
   for( kk=0 ; kk < p ; kk++ ){
     double m=0.0 , s=0.0 ;
     if( cols[kk]==NULL ){ free(Z) ; return NULL ; }
     for( ii=0 ; ii < n ; ii++ ){
       if( !isfinite(cols[kk][ii]) ){ free(Z) ; return NULL ; }
       m += cols[kk][ii] ;
     }
     m /= n ;
     for( ii=0 ; ii < n ; ii++ ){ d = cols[kk][ii]-m ; s += d*d ; }
     s = sqrt( s/n ) ;
     if( s <= 0.0 ){ free(Z) ; return NULL ; }   /* constant column */
     for( ii=0 ; ii < n ; ii++ ) Z[ii*p+kk] = (float)((cols[kk][ii]-m)/s) ;
   }

   /* correlation matrix R = (1/n) Z'Z (diagonal is 1 by construction) */
   R    = (double *)calloc((size_t)p*p,sizeof(double)) ;
   V    = (double *)malloc(sizeof(double)*(size_t)p*p) ;
   ev   = (double *)malloc(sizeof(double)*p) ;
   Rinv = (double *)calloc((size_t)p*p,sizeof(double)) ;
   if( R==NULL || V==NULL || ev==NULL || Rinv==NULL ){
     free(Z) ; free(R) ; free(V) ; free(ev) ; free(Rinv) ; return NULL ;
   }
   for( kk=0 ; kk < p ; kk++ )
     for( ll=0 ; ll < p ; ll++ ){
       double s=0.0 ;
       for( ii=0 ; ii < n ; ii++ ) s += (double)Z[ii*p+kk]*Z[ii*p+ll] ;
       R[kk+p*ll] = s/n ;
     }

   /* Ledoit-Wolf shrinkage toward the identity (target mean-eigenvalue = 1,
      since R is a correlation matrix).  delta = min(bbar2,d2)/d2. */
   { double d2=0.0 , bbar2=0.0 , b2 ;
     for( kk=0 ; kk < p ; kk++ )
       for( ll=0 ; ll < p ; ll++ ){
         double off = R[kk+p*ll] - ((kk==ll)?1.0:0.0) ; d2 += off*off ;
       }
     for( ii=0 ; ii < n ; ii++ )
       for( kk=0 ; kk < p ; kk++ )
         for( ll=0 ; ll < p ; ll++ ){
           double e = (double)Z[ii*p+kk]*Z[ii*p+ll] - R[kk+p*ll] ; bbar2 += e*e ;
         }
     bbar2 /= (double)n*n ;
     b2 = (bbar2 < d2) ? bbar2 : d2 ;
     delta = (d2 > 0.0) ? b2/d2 : 1.0 ;
     if( delta < 0.0 ) delta = 0.0 ; else if( delta > 1.0 ) delta = 1.0 ;
     for( kk=0 ; kk < p ; kk++ )
       for( ll=0 ; ll < p ; ll++ )
         R[kk+p*ll] = (1.0-delta)*R[kk+p*ll] + ((kk==ll)?delta:0.0) ;
   }

   /* eigendecompose the shrunken R (symeig_double overwrites its input with the
      eigenvectors; eigenvalues ascending), then form a floored inverse */
   memcpy( V , R , sizeof(double)*(size_t)p*p ) ;
   symeig_double( p , V , ev ) ;             /* V[k+p*j] = comp k of eigvec j */
   { double emax = ev[p-1] , floorv = (emax>0.0) ? 1.0e-8*emax : 1.0e-12 ;
     int er=0 ;
     for( jj=0 ; jj < p ; jj++ ){
       /* effective rank of the ORIGINAL R: undo the shrink lift lam_R =
          (lam - delta)/(1-delta), count directions carrying >1% of the top
          eigenvalue (a near-collinear measure falls below this) */
       double lamRmax = (delta<1.0) ? (emax-delta)/(1.0-delta) : emax ;
       double lamR    = (delta<1.0) ? (ev[jj]-delta)/(1.0-delta) : ev[jj] ;
       if( lamR > 1.0e-2*((lamRmax>0.0)?lamRmax:1.0) ) er++ ;
     }
     if( erank != NULL ) *erank = er ;
     for( jj=0 ; jj < p ; jj++ ){
       double lam = (ev[jj] > floorv) ? ev[jj] : floorv , inv = 1.0/lam ;
       for( kk=0 ; kk < p ; kk++ )
         for( ll=0 ; ll < p ; ll++ )
           Rinv[kk+p*ll] += V[kk+p*jj]*V[ll+p*jj]*inv ;
     }
   }
   if( shrink != NULL ) *shrink = (float)delta ;

   /* Mahalanobis distances over the whitened profile, then rescale to a
      similarity exactly as the Euclidean profile does */
   sm = THD_simmat_new(n) ;
   if( sm==NULL ){ free(Z) ; free(R) ; free(V) ; free(ev) ; free(Rinv) ; return NULL ; }
   sm->is_dist = 0 ;
   for( ii=0 ; ii < n ; ii++ )
     for( jj=ii+1 ; jj < n ; jj++ ){
       double q=0.0 ;
       for( kk=0 ; kk < p ; kk++ ){
         double dk = (double)Z[ii*p+kk]-Z[jj*p+kk] , acc=0.0 ;
         for( ll=0 ; ll < p ; ll++ )
           acc += Rinv[kk+p*ll]*((double)Z[ii*p+ll]-Z[jj*p+ll]) ;
         q += dk*acc ;
       }
       dd = (q > 0.0) ? sqrt(q) : 0.0 ;
       if( !isfinite(dd) ){
         THD_simmat_free(sm) ; free(Z) ; free(R) ; free(V) ; free(ev) ; free(Rinv) ;
         return NULL ;
       }
       sm->mat[ii*n+jj] = sm->mat[jj*n+ii] = (float)dd ;
       if( dd > dmax ) dmax = dd ;
     }
   if( dmax <= 0.0 ) dmax = 1.0 ;
   for( ii=0 ; ii < n ; ii++ ){
     sm->mat[ii*n+ii] = 1.0f ;
     for( jj=ii+1 ; jj < n ; jj++ ){
       float s = 1.0f - sm->mat[ii*n+jj]/(float)dmax ;
       sm->mat[ii*n+jj] = sm->mat[jj*n+ii] = s ;
     }
   }

   free(Z) ; free(R) ; free(V) ; free(ev) ; free(Rinv) ;
   return sm ;
}

/*----------------------------------------------------------------------------*/
/*! Cross-validated squared Euclidean (crossnobis) RDM.  See the header.        */

THD_simmat * THD_simmat_crossnobis( int ncond , int nrun , int nvox , float **pat )
{
   THD_simmat *sm ; int i , j , r , v ; size_t qq,ntot ; double denom ;

   if( ncond < 2 || nrun < 2 || nvox < 1 || pat == NULL ) return NULL ;
   ntot=(size_t)ncond*(size_t)nvox ;
   for( r=0 ; r<nrun ; r++ ){
     if( pat[r]==NULL ) return NULL ;
     for( qq=0 ; qq<ntot ; qq++ ) if( !isfinite(pat[r][qq]) ) return NULL ;
   }

   sm = THD_simmat_new(ncond) ; if( sm == NULL ) return NULL ;
   sm->is_dist = 1 ;
   denom = (double)nrun * (nrun-1) * nvox ;   /* ordered run pairs, per voxel */

   for( i=0 ; i < ncond ; i++ ){
     sm->mat[i*ncond+i] = 0.0f ;
     for( j=i+1 ; j < ncond ; j++ ){
       /* sum_{r!=s} delta_r . delta_s = |sum_r delta_r|^2 - sum_r |delta_r|^2,
          accumulated voxel by voxel so no per-run delta vector is materialized */
       double self=0.0 , tot=0.0 , d ;
       for( v=0 ; v < nvox ; v++ ){
         double sd=0.0 ;
         for( r=0 ; r < nrun ; r++ ){
           double dr = (double)pat[r][(size_t)i*nvox+v]
                     - (double)pat[r][(size_t)j*nvox+v] ;
           sd += dr ; self += dr*dr ;
         }
         tot += sd*sd ;
       }
       d = (tot - self) / denom ;             /* UNBIASED; may be negative */
       sm->mat[i*ncond+j] = sm->mat[j*ncond+i] = (float)d ;
     }
   }
   return sm ;
}

/*----------------------------------------------------------------------------*/
/*! F21 pairwise-valid crossnobis for missing/duplicated run conditions. */

THD_simmat * THD_simmat_crossnobis_valid( int ncond, int nrun, int nvox,
                                           float **pat, int **nrep )
{
   THD_simmat *sm ; int i,j,r,v,nv ; size_t qq,ntot ;
   if( ncond<2 || nrun<2 || nvox<1 || pat==NULL || nrep==NULL ) return NULL ;
   ntot=(size_t)ncond*(size_t)nvox ;
   for( r=0 ; r<nrun ; r++ ){
     if( pat[r]==NULL || nrep[r]==NULL ) return NULL ;
     for( i=0 ; i<ncond ; i++ ) if( nrep[r][i]<0 ) return NULL ;
     for( qq=0 ; qq<ntot ; qq++ ) if( !isfinite(pat[r][qq]) ) return NULL ;
   }
   sm=THD_simmat_new(ncond) ; if( sm==NULL ) return NULL ;
   sm->is_dist=1 ;
   for( i=0 ; i<ncond ; i++ ){
     sm->mat[i*ncond+i]=0.0f ;
     for( j=i+1 ; j<ncond ; j++ ){
       double self=0.0,tot=0.0,denom ;
       for( nv=0,r=0 ; r<nrun ; r++ ) if( nrep[r][i]>0 && nrep[r][j]>0 ) nv++ ;
       if( nv<2 ){ THD_simmat_free(sm) ; return NULL ; }
       denom=(double)nv*(nv-1)*nvox ;
       for( v=0 ; v<nvox ; v++ ){
         double sd=0.0 ;
         for( r=0 ; r<nrun ; r++ ) if( nrep[r][i]>0 && nrep[r][j]>0 ){
           double dr=(double)pat[r][(size_t)i*nvox+v]
                    -(double)pat[r][(size_t)j*nvox+v] ;
           sd+=dr ; self+=dr*dr ;
         }
         tot+=sd*sd ;
       }
       sm->mat[i*ncond+j]=sm->mat[j*ncond+i]=(float)((tot-self)/denom) ;
     }
   }
   return sm ;
}

/*----------------------------------------------------------------------------*/

static int dbl_cmp_asc( const void *a , const void *b )
{
   double aa = *(const double *)a , bb = *(const double *)b ;
   return (aa > bb) - (aa < bb) ;
}

/*! Univariate (diagonal) noise normalization.  See the header. */

void THD_noise_wdiag( int nvox , int nresid , float *R , float *w )
{
   int v , t , m=0 ; double *var , *pos , med ;

   if( nvox < 1 || nresid < 1 || R == NULL || w == NULL ) return ;

   var = (double *)malloc(sizeof(double)*nvox) ;
   pos = (double *)malloc(sizeof(double)*nvox) ;
   if( var==NULL || pos==NULL ){
     free(var) ; free(pos) ; for( v=0 ; v<nvox ; v++ ) w[v]=1.0f ; return ;
   }
   for( v=0 ; v < nvox ; v++ ){
     double s=0.0 ;
     for( t=0 ; t < nresid ; t++ ){
       double x = R[(size_t)t*nvox+v] ;
       if( !isfinite(x) ){ free(var) ; free(pos) ;
         for( v=0 ; v<nvox ; v++ ) w[v]=1.0f ; return ; }
       s += x*x ;
     }
     var[v] = s / nresid ;
     if( var[v] > 0.0 ) pos[m++] = var[v] ;
   }
   if( m == 0 ){ for( v=0 ; v < nvox ; v++ ) w[v] = 1.0f ; free(var) ; free(pos) ; return ; }
   qsort( pos , m , sizeof(double) , dbl_cmp_asc ) ;
   med = pos[m/2] ;
   /* floor variances to the median positive variance: a voxel quieter than
      typical is not boosted (that would just amplify its noise) */
   for( v=0 ; v < nvox ; v++ ){
     double vv = (var[v] > med) ? var[v] : med ;
     w[v] = (float)( 1.0 / sqrt(vv) ) ;
   }
   free(var) ; free(pos) ;
}

/*! Multivariate noise normalization matrix Sigma^{-1/2}.  See the header. */

int THD_noise_whalf( int nvox , int nresid , float *R , float *Whalf ,
                     float *shrink , int *erank )
{
   int p = nvox , k , l , t , j ; double *S , *V , *ev , mu , delta ;
   double sum_norm4=0.0 ;

   if( p < 1 || nresid < 1 || R == NULL || Whalf == NULL ) return 1 ;

   S  = (double *)calloc((size_t)p*p,sizeof(double)) ;
   V  = (double *)malloc(sizeof(double)*(size_t)p*p) ;
   ev = (double *)malloc(sizeof(double)*p) ;
   if( S==NULL || V==NULL || ev==NULL ){
     free(S) ; free(V) ; free(ev) ; return 1 ;
   }

   /* Sigma = R'R / nresid (R already demeaned per run).  Accumulate by
      residual row so the row norm needed by the Ledoit-Wolf numerator comes
      almost for free and S is updated contiguously along its upper triangle. */
   for( t=0 ; t < nresid ; t++ ){
     float *rt=R+(size_t)t*p ; double norm2=0.0 ;
     for( k=0 ; k < p ; k++ ){
       double x=rt[k] ;
       if( !isfinite(x) ){ free(S) ; free(V) ; free(ev) ; return 1 ; }
       norm2 += x*x ;
     }
     sum_norm4 += norm2*norm2 ;
     for( k=0 ; k < p ; k++ ){
       double x=rt[k] ;
       for( l=k ; l < p ; l++ ) S[(size_t)k*p+l] += x*rt[l] ;
     }
   }
   for( k=0 ; k < p ; k++ )
     for( l=k ; l < p ; l++ ){
       double s=S[(size_t)k*p+l]/nresid ;
       S[(size_t)k*p+l] = S[(size_t)l*p+k] = s ;
     }

   mu = 0.0 ; for( k=0 ; k < p ; k++ ) mu += S[k+(size_t)p*k] ; mu /= p ;

   /* Ledoit-Wolf shrinkage of Sigma toward mu*I */
   { double d2=0.0 , ss2=0.0 , bbar , b2 , bnum ;
     for( k=0 ; k < p ; k++ )
       for( l=0 ; l < p ; l++ ){
         double sv=S[k+(size_t)p*l] ;
         double off=sv-((k==l)?mu:0.0) ;
         d2 += off*off ; ss2 += sv*sv ;
       }
     /* sum_t ||r_t r_t' - S||_F^2
          = sum_t ||r_t||^4 - nresid ||S||_F^2.
        This is algebraically the former T*p*p loop, reduced to the row norms
        accumulated above plus one p*p norm.  Clamp roundoff at zero. */
     bnum = sum_norm4-(double)nresid*ss2 ;
     if( bnum < 0.0 ) bnum=0.0 ;
     bbar = bnum/((double)nresid*nresid) ;
     b2 = (bbar < d2) ? bbar : d2 ;
     delta = (d2 > 0.0) ? b2/d2 : 1.0 ;
     if( delta < 0.0 ) delta = 0.0 ; else if( delta > 1.0 ) delta = 1.0 ;
     for( k=0 ; k < p ; k++ )
       for( l=0 ; l < p ; l++ )
         S[k+(size_t)p*l] = (1.0-delta)*S[k+(size_t)p*l] + ((k==l)?delta*mu:0.0) ;
   }
   if( shrink != NULL ) *shrink = (float)delta ;

   memcpy( V , S , sizeof(double)*(size_t)p*p ) ;
   symeig_double( p , V , ev ) ;              /* V[k+p*j] = comp k of eigvec j */
   { double emax = ev[p-1] , floorv = (emax>0.0) ? 1.0e-8*emax : 1.0e-12 ;
     int er=0 ;
     for( j=0 ; j < p ; j++ ) if( ev[j] > 1.0e-2*((emax>0.0)?emax:1.0) ) er++ ;
     if( erank != NULL ) *erank = er ;
     for( k=0 ; k < p ; k++ )
       for( l=0 ; l < p ; l++ ) Whalf[(size_t)k*p+l] = 0.0f ;
     for( j=0 ; j < p ; j++ ){
       double lam = (ev[j] > floorv) ? ev[j] : floorv , is = 1.0/sqrt(lam) ;
       for( k=0 ; k < p ; k++ )
         for( l=0 ; l < p ; l++ )
           Whalf[(size_t)k*p+l] += (float)( V[k+(size_t)p*j]*V[l+(size_t)p*j]*is ) ;
     }
   }

   free(S) ; free(V) ; free(ev) ;
   return 0 ;
}

/*============================================================================*/
/*  1D input and output                                                       */
/*============================================================================*/

THD_simmat * THD_simmat_read_1D( char *fname , int nexpect )
{
   MRI_IMAGE *im ; THD_simmat *sm ; float *iar ; int ii , jj , n ;

   im = mri_read_1D( fname ) ;
   if( im == NULL ) ERROR_exit("can't read matrix file '%s'",fname) ;
   if( im->nx != im->ny )
     ERROR_exit("matrix file '%s' is %d x %d, but must be square",
                fname , im->nx , im->ny ) ;
   if( nexpect > 0 && im->nx != nexpect )
     ERROR_exit("matrix file '%s' is %d x %d, but %d x %d is needed here",
                fname , im->nx , im->ny , nexpect , nexpect ) ;

   n = im->nx ; sm = THD_simmat_new(n) ; iar = MRI_FLOAT_PTR(im) ;
   if( sm==NULL || iar==NULL ){
     THD_simmat_free(sm) ; mri_free(im) ;
     ERROR_exit("cannot allocate matrix read from '%s'",fname) ;
   }

   /* mri_read_1D gives element (i,j) at iar[j*nx+i]; store row major */
   for( ii=0 ; ii < n ; ii++ )
     for( jj=0 ; jj < n ; jj++ ) sm->mat[ii*n+jj] = iar[ jj*n + ii ] ;

   mri_free(im) ;

   /* A Mantel/RSA matrix must be finite and symmetric.  Only the strict upper
      triangle is analyzed, but the permutation relabels rows AND columns
      together (THD_simmat_to_tri_perm reads mat[p[i]][p[j]]), so an asymmetric
      input silently produces a WRONG null.  Validate at read time rather than
      let it through.  The diagonal is never used, so it is not checked. */
   {
     float maxabs=0.0f , maxasym=0.0f , tol ; int bi=0 , bj=0 ;
     for( ii=0 ; ii < n ; ii++ )
       for( jj=ii+1 ; jj < n ; jj++ ){
         float a = sm->mat[ii*n+jj] , b = sm->mat[jj*n+ii] , d ;
         if( !isfinite(a) || !isfinite(b) ){
           THD_simmat_free(sm) ;
           ERROR_exit("matrix file '%s' has a non-finite entry at (%d,%d); RSA\n"
                      "       needs finite similarities/distances",
                      fname , ii+1 , jj+1 ) ;
         }
         d = fabsf(a-b) ;
         if( d > maxasym ){ maxasym = d ; bi = ii ; bj = jj ; }
         if( fabsf(a) > maxabs ) maxabs = fabsf(a) ;
         if( fabsf(b) > maxabs ) maxabs = fabsf(b) ;
       }
     tol = 1.0e-5f * (1.0f + maxabs) ;
     if( maxasym > tol ){
       float a = sm->mat[bi*n+bj] , b = sm->mat[bj*n+bi] ;
       THD_simmat_free(sm) ;
       ERROR_exit("matrix file '%s' is not symmetric: (%d,%d)=%.8g but "
                  "(%d,%d)=%.8g\n       (differ by %.3g).  RSA relabels rows AND "
                  "columns together, so an\n       asymmetric matrix gives a wrong "
                  "permutation null.  Symmetrize it\n       (e.g. 0.5*(M+M')) "
                  "before use.",
                  fname , bi+1,bj+1,a , bj+1,bi+1,b , maxasym ) ;
     }
   }

   return sm ;
}

int THD_simmat_write_1D( char *fname , THD_simmat *sm )
{
   MRI_IMAGE *im ; float *iar ; int ii , jj , n , rv ;

   if( sm == NULL ) return 0 ;
   n = sm->n ;

   /* build in the layout mri_read_1D produces, then let mri_write_1D (which
      transposes on the way out) make it an exact round trip */
   im  = mri_new( n , n , MRI_float ) ;
   iar = MRI_FLOAT_PTR(im) ;
   for( ii=0 ; ii < n ; ii++ )
     for( jj=0 ; jj < n ; jj++ ) iar[ jj*n + ii ] = sm->mat[ ii*n + jj ] ;

   rv = mri_write_1D( fname , im ) ;
   mri_free(im) ;
   if( !rv ) WARNING_message("can't write matrix file %s",fname) ;
   return rv ;
}

/*============================================================================*/
/*  Triangles                                                                 */
/*============================================================================*/

void THD_simmat_to_tri( THD_simmat *sm , float *tri )
{
   int ii , jj , kk=0 , n=sm->n ;
   for( ii=0 ; ii < n ; ii++ )
     for( jj=ii+1 ; jj < n ; jj++ ) tri[kk++] = sm->mat[ii*n+jj] ;
}

void THD_simmat_to_tri_perm( THD_simmat *sm , int *p , float *tri )
{
   int ii , jj , kk=0 , n=sm->n ;
   for( ii=0 ; ii < n ; ii++ )
     for( jj=ii+1 ; jj < n ; jj++ ) tri[kk++] = sm->mat[ p[ii]*n + p[jj] ] ;
}

void THD_tri_to_simmat( int n , float *tri , THD_simmat *sm )
{
   int ii , jj , kk=0 ;
   for( ii=0 ; ii < n ; ii++ ) sm->mat[ii*n+ii] = 0.0f ;
   for( ii=0 ; ii < n ; ii++ )
     for( jj=ii+1 ; jj < n ; jj++ )
       sm->mat[ii*n+jj] = sm->mat[jj*n+ii] = tri[kk++] ;
}

/*----------------------------------------------------------------------------*/
/*! Covariance-weighted RDM transform.  With exchangeable condition noise the
    zero-signal distance covariance is V=(C C') o (C C').  Rather than form and
    invert that O(n^4) matrix, use its exact centered-kernel equivalent: turn
    the dissimilarities into G=-H D H/2 and take Frobenius inner products.
    This is the same identity used by rsatoolbox's default corr_cov/cosine_cov
    implementation. */

void THD_rdm_cov_transform( int n, float *tri, int remove_mean, float *out )
{
   int i,j,k=0,m=THD_NTRI(n) ; double mean=0.0,grand=0.0 ;

   if( n<2 || tri==NULL || out==NULL ) return ;
   { double rmean[n] ;
   memset(rmean,0,sizeof(rmean)) ;
   if( remove_mean ){
     for( k=0 ; k<m ; k++ ) mean += tri[k] ;
     mean /= (double)m ;
   }
   k=0 ;
   for( i=0 ; i<n ; i++ ) for( j=i+1 ; j<n ; j++ ){
     double d=(double)tri[k++]-mean ;
     rmean[i]+=d ; rmean[j]+=d ; grand+=2.0*d ;
   }
   for( i=0 ; i<n ; i++ ) rmean[i]/=(double)n ;
   grand/=(double)n*n ;
   k=0 ;
   for( i=0 ; i<n ; i++ ) for( j=0 ; j<n ; j++ ){
     double d=0.0 ;
     if( i<j ){
       int ix ;
       /* The direct index avoids materializing D.  n is ordinarily small;
          use the closed upper-triangle offset rather than an O(n) search. */
       ix=i*(2*n-i-1)/2 + (j-i-1) ; d=(double)tri[ix]-mean ;
     } else if( j<i ){
       int a=j,b=i,ix=a*(2*n-a-1)/2 + (b-a-1) ;
       d=(double)tri[ix]-mean ;
     }
     out[k++]=(float)(-0.5*(d-rmean[i]-rmean[j]+grand)) ;
   }
   }
}

float THD_rdm_cov_cosine( int n, float *a, float *b )
{
   size_t i,nn ; double ab=0.0,aa=0.0,bb=0.0,r ;
   if( n<2 || a==NULL || b==NULL ) return 0.0f ;
   nn=(size_t)n*n ;
   for( i=0 ; i<nn ; i++ ){
     double x=a[i],y=b[i] ;
     if( !isfinite(x) || !isfinite(y) ) return NAN ;
     ab+=x*y ; aa+=x*x ; bb+=y*y ;
   }
   if( aa<=0.0 || bb<=0.0 ) return 0.0f ;
   r=ab/sqrt(aa*bb) ;
   if( r>1.0 ) r=1.0 ; else if( r< -1.0 ) r=-1.0 ;
   return (float)r ;
}

float THD_tri_corr( int m , float *a , float *b , int cmp ,
                    float *sc1 , float *sc2 )
{
   int kk ;
   if( m<2 || a==NULL || b==NULL || cmp<CMP_SPEARMAN || cmp>CMP_RHOA ) return NAN ;
   for( kk=0 ; kk<m ; kk++ ) if( !isfinite(a[kk]) || !isfinite(b[kk]) ) return NAN ;
   if( (cmp==CMP_SPEARMAN || cmp==CMP_KTAUB || cmp==CMP_RHOA) &&
       (sc1==NULL || sc2==NULL) ) return NAN ;
   switch( cmp ){
     case CMP_SPEARMAN:
       memcpy(sc1,a,sizeof(float)*m) ; memcpy(sc2,b,sizeof(float)*m) ;
       return THD_spearman_corr( m , sc1 , sc2 ) ;

     case CMP_PEARSON:
       return THD_pearson_corr( m , a , b ) ;

     case CMP_KTAUB:
       memcpy(sc1,a,sizeof(float)*m) ; memcpy(sc2,b,sizeof(float)*m) ;
       return THD_ktaub_corr( m , sc1 , sc2 ) ;

     case CMP_KTAUA: {
       /* tau-a = (concordant - discordant) / (m choose 2); ties count in the
          denominator but not the numerator.  O(m^2) -- fine for the small
          triangles here, but the slowest option. */
       double nc=0.0 , nd=0.0 ; int ii , jj ; float s ;
       for( ii=0 ; ii < m ; ii++ )
         for( jj=ii+1 ; jj < m ; jj++ ){
           s = (a[ii]-a[jj]) * (b[ii]-b[jj]) ;
           if( s > 0.0f ) nc += 1.0 ; else if( s < 0.0f ) nd += 1.0 ;
         }
       return (float)( (nc-nd) / (0.5*(double)m*((double)m-1.0)) ) ;
     }

     case CMP_RHOA: {
       /* Expected Spearman correlation under independent random ordering
          within every tie group (Schuett et al. 2023, eq. 23).  Average ranks
          are the expected random ranks, while every fully tie-broken rank
          vector has the fixed centered sum of squares m(m^2-1)/12.  Unlike
          ordinary Spearman, do NOT normalize by the smaller tied-rank sums of
          squares: that normalization is precisely the tie advantage rho-a
          removes. */
       double dot=0.0,mid=0.5*((double)m+1.0),den ; int ii ; float r ;
       if( m < 2 ) return 0.0f ;
       THD_rank_avg(m,a,sc1) ; THD_rank_avg(m,b,sc2) ;
       for( ii=0 ; ii<m ; ii++ )
         dot += ((double)sc1[ii]-mid)*((double)sc2[ii]-mid) ;
       den=(double)m*((double)m*(double)m-1.0) ;
       r=(float)(12.0*dot/den) ;
       if( r>1.0f ) r=1.0f ; else if( r< -1.0f ) r=-1.0f ;
       return r ;
     }

     case CMP_CORR_COV:
     case CMP_COS_COV: {
       int n=(int)((1.0+sqrt(1.0+8.0*(double)m))*0.5+0.5) ;
       float *ga,*gb,r ;
       if( THD_NTRI(n)!=m ) return 0.0f ;
       ga=(float *)malloc(sizeof(float)*(size_t)n*n) ;
       gb=(float *)malloc(sizeof(float)*(size_t)n*n) ;
       if( ga==NULL || gb==NULL ){ free(ga) ; free(gb) ; return NAN ; }
       THD_rdm_cov_transform(n,a,cmp==CMP_CORR_COV,ga) ;
       THD_rdm_cov_transform(n,b,cmp==CMP_CORR_COV,gb) ;
       r=THD_rdm_cov_cosine(n,ga,gb) ; free(ga) ; free(gb) ; return r ;
     }
   }
   return NAN ;  /* guarded enum above; keeps compiler control-flow explicit */
}

/*============================================================================*/
/*  Labels                                                                    */
/*============================================================================*/

char * THD_simmat_metric_label( int metric )
{
   switch( metric ){
     case SIM_PEARSON:  return "corr"   ;
     case SIM_SPEARMAN: return "scorr"  ;
     case SIM_COSINE:   return "cosine" ;
     case SIM_EUCLID:   return "euclid" ;
   }
   return "?" ;
}

char * THD_simmat_rule_label( int rule )
{
   switch( rule ){
     case RUL_ANNAK:   return "annak"   ;
     case RUL_NN:      return "nn"      ;
     case RUL_EUCLID:  return "euclid"  ;
     case RUL_ABSDIFF: return "absdiff" ;
   }
   return "?" ;
}

char * THD_simmat_cmp_label( int cmp )
{
   switch( cmp ){
     case CMP_SPEARMAN: return "spearman"   ;
     case CMP_PEARSON:  return "pearson"    ;
     case CMP_KTAUB:    return "ktaub"      ;
     case CMP_KTAUA:    return "ktaua"      ;
     case CMP_CORR_COV: return "corr_cov"   ;
     case CMP_COS_COV:  return "cosine_cov" ;
     case CMP_RHOA:      return "rhoa"       ;
   }
   return "?" ;
}

/*============================================================================*/
/*  RDM permutation inference  (moved from thd_mantel.c; relabelings now come  */
/*  from a shared PERM_set so IS-RSA gets exchangeability blocks, and the same */
/*  set can later drive a common max-statistic FWE null across elements)       */
/*                                                    -- P Molfese, Jul 2026   */
/*============================================================================*/

#define BIGT 1.0e30f

#undef  MYatanh
#define MYatanh(x) ( ((x)<-0.999329f) ? -4.0f                \
                    :((x)>+0.999329f) ? +4.0f : atanhf(x) )

/*--- workspace: per-thread scratch, no RNG (relabelings come from a PERM_set) ---*/

THD_rdm_ws * THD_rdm_ws_new( int n , int ncol )
{
   THD_rdm_ws *ws ; int m ;

   if( n < 3 ) return NULL ;
   m = THD_NTRI(n) ;
   if( ncol < 1 ) ncol = 1 ;

   ws = (THD_rdm_ws *)calloc(1,sizeof(THD_rdm_ws)) ;
   ws->n = n ; ws->m = m ; ws->ncol = ncol ;

   ws->tri   = (float *)malloc(sizeof(float)*m) ;
   ws->sc1   = (float *)malloc(sizeof(float)*m) ;
   ws->sc2   = (float *)malloc(sizeof(float)*m) ;
   ws->rprep = (float *)malloc(sizeof(float)*m) ;
   ws->yperm = (float *)malloc(sizeof(float)*m) ;
   ws->yfit  = (float *)malloc(sizeof(float)*m) ;
   ws->resid = (float *)malloc(sizeof(float)*m) ;
   ws->Zfit  = (float *)malloc(sizeof(float)*m) ;
   ws->Zres  = (float *)malloc(sizeof(float)*m) ;
   ws->beta  = (float *)malloc(sizeof(float)*(ncol+1)) ;
   ws->Xmat  = (float *)malloc(sizeof(float)*(size_t)m*ncol) ;
   ws->Pmat  = (float *)malloc(sizeof(float)*(size_t)m*ncol) ;
   ws->ework = THD_simmat_new(n) ;
   return ws ;
}

void THD_rdm_ws_free( THD_rdm_ws *ws )
{
   if( ws == NULL ) return ;
   free(ws->tri) ; free(ws->sc1) ; free(ws->sc2) ; free(ws->rprep) ;
   free(ws->yperm) ; free(ws->yfit) ; free(ws->resid) ;
   free(ws->Zfit) ; free(ws->Zres) ; free(ws->beta) ;
   free(ws->Xmat) ; free(ws->Pmat) ;
   THD_simmat_free(ws->ework) ;
   free(ws) ;
}

/*--- small statistics ---*/

float THD_onesamp_t( int n , float *v )
{
   int ii ; float bar=0.0f , sd=0.0f , d ;

   for( ii=0 ; ii < n ; ii++ ) bar += v[ii] ;
   bar /= (float)n ;
   for( ii=0 ; ii < n ; ii++ ){ d = v[ii] - bar ; sd += d*d ; }
   sd = sqrtf( sd / (float)(n-1) ) ;

   if( sd <= 0.0f ) return (bar > 0.0f) ? BIGT : (bar < 0.0f) ? -BIGT : 0.0f ;
   return bar / ( sd / sqrtf((float)n) ) ;
}

float THD_p_to_z( float p , float sign_of )
{
   float z ;
   if( p <= 0.0f ) p = 1.0e-8f ;
   if( p >= 1.0f ) return 0.0f ;
   z = (float)qginv( 0.5*(double)p ) ;
   return (sign_of < 0.0f) ? -z : z ;
}

/*! z-score a vector in place; returns 0 if it was constant. */

static int rdm_zscore( int m , float *v )
{
   int ii ; float bar=0.0f , sd=0.0f , d ;

   for( ii=0 ; ii < m ; ii++ ) bar += v[ii] ;
   bar /= (float)m ;
   for( ii=0 ; ii < m ; ii++ ){ d = v[ii]-bar ; sd += d*d ; }
   sd = sqrtf( sd/(float)m ) ;
   if( sd <= 0.0f ){
     for( ii=0 ; ii < m ; ii++ ) v[ii] = 0.0f ;
     return 0 ;
   }
   for( ii=0 ; ii < m ; ii++ ) v[ii] = (v[ii]-bar)/sd ;
   return 1 ;
}

/*! Replace v[] by its average ranks, using scratch of length m. */

static void rdm_rank_inplace( int m , float *v , float *scratch )
{
   THD_rank_avg( m , v , scratch ) ;
   memcpy( v , scratch , sizeof(float)*m ) ;
}

/*============================================================================*/
/*  Fixed-model Mantel cache                                                  */
/*============================================================================*/

struct THD_mantel_cache {
   int n , m , nmodel , nperm , cmp ;
   PERM_set *pset ;               /* borrowed; cache does not own it          */
   float *prep ;                 /* [model][permutation][triangle entry] */
   float *ss ;                   /* centered/rank sum of squares          */
} ;

/*! Center a vector exactly as THD_pearson_corr does and return its SS. */
static float mantel_pearson_prepare( int m , float *v )
{
   float bar=0.0f , ss=0.0f ; int ii ;
   for( ii=0 ; ii < m ; ii++ ) bar += v[ii] ;
   bar /= m ;
   for( ii=0 ; ii < m ; ii++ ){ v[ii] -= bar ; ss += v[ii]*v[ii] ; }
   return ss ;
}

static float mantel_prepared_corr( int m , float *a , float ass,
                                   float *b , float bss )
{
   float dot=0.0f ; int ii ;
   if( ass <= 0.0f || bss <= 0.0f ) return 0.0f ;
   for( ii=0 ; ii < m ; ii++ ) dot += a[ii]*b[ii] ;
   return dot/sqrtf(ass*bss) ;
}

/*! rho-a from two average-ranked, centered vectors.  Unlike Spearman, its
    denominator is the untied rank variance and therefore depends only on m. */
static float mantel_prepared_rhoa( int m, float *a, float *b )
{
   double dot=0.0 ; int ii ;
   if( m < 2 ) return 0.0f ;
   for( ii=0 ; ii<m ; ii++ ) dot+=(double)a[ii]*b[ii] ;
   return (float)(12.0*dot/((double)m*((double)m*(double)m-1.0))) ;
}

THD_mantel_cache * THD_mantel_cache_build( int nmodel, THD_simmat **models,
                                            int cmp, PERM_set *pset )
{
   THD_mantel_cache *mc ; int mm,pk,n,m,np ; size_t ntot ;

   if( nmodel < 1 || models == NULL || models[0] == NULL || pset == NULL )
     return NULL ;
   if( cmp != CMP_PEARSON && cmp != CMP_SPEARMAN && cmp != CMP_RHOA )
     return NULL ;
   n=models[0]->n ; m=THD_NTRI(n) ; np=pset->nperm ;
   if( n < 3 || np < 1 || pset->nobs != n ) return NULL ;
   for( mm=0 ; mm < nmodel ; mm++ )
     if( models[mm] == NULL || models[mm]->n != n ) return NULL ;

   mc=(THD_mantel_cache *)calloc(1,sizeof(THD_mantel_cache)) ;
   mc->n=n ; mc->m=m ; mc->nmodel=nmodel ; mc->nperm=np ; mc->cmp=cmp ;
   mc->pset=pset ;
   ntot=(size_t)nmodel*np*m ;
   mc->prep=(float *)malloc(sizeof(float)*ntot) ;
   mc->ss=(float *)malloc(sizeof(float)*(size_t)nmodel*np) ;
   if( mc->prep == NULL || mc->ss == NULL ){
     THD_mantel_cache_free(mc) ; return NULL ;
   }

   for( mm=0 ; mm < nmodel ; mm++ ){
     for( pk=0 ; pk < np ; pk++ ){
       float *v=mc->prep+((size_t)mm*np+pk)*m ;
       int *perm=pset->perm+(size_t)pk*n ;
       THD_simmat_to_tri_perm(models[mm],perm,v) ;
       mc->ss[(size_t)mm*np+pk] = (cmp==CMP_SPEARMAN || cmp==CMP_RHOA)
                                    ? spearman_rank_prepare(m,v)
                                    : mantel_pearson_prepare(m,v) ;
     }
   }
   return mc ;
}

void THD_mantel_cache_free( THD_mantel_cache *mc )
{
   if( mc == NULL ) return ;
   free(mc->prep) ; free(mc->ss) ; free(mc) ;
}

size_t THD_mantel_cache_bytes( THD_mantel_cache *mc )
{
   if( mc == NULL ) return 0 ;
   return sizeof(THD_mantel_cache)
        + sizeof(float)*(size_t)mc->nmodel*mc->nperm*(mc->m+1) ;
}

/*============================================================================*/
/*  Single-model Mantel test                                                  */
/*============================================================================*/

THD_permstat THD_mantel_corr( THD_simmat *neural , THD_simmat *model ,
                              int cmp , PERM_set *pset , THD_rdm_ws *ws ,
                              float *permnull )
{
   THD_permstat ps ; int m = ws->m , n = neural->n , pk , nge ; float robs ;
   int np = (pset != NULL) ? pset->nperm : 0 ;

   ps.stat = 0.0f ; ps.pval = -1.0f ; ps.zscr = 0.0f ; ps.nperm = np ;

   THD_simmat_to_tri( neural , ws->tri ) ;
   THD_simmat_to_tri( model  , ws->sc1 ) ;      /* sc1 reused below, so copy */
   memcpy( ws->yfit , ws->sc1 , sizeof(float)*m ) ;

   robs = THD_tri_corr( m , ws->tri , ws->yfit , cmp , ws->sc1 , ws->sc2 ) ;
   ps.stat = robs ;

   if( pset == NULL ){ ps.zscr = MYatanh(robs) ; return ps ; }

   if( cmp == CMP_SPEARMAN || cmp == CMP_RHOA ){
     /* Only the model side is permuted, so the neural triangle can be ranked
        once and reused across every permutation. */
     float rv ;
     memcpy( ws->rprep , ws->tri , sizeof(float)*m ) ;
     rv = spearman_rank_prepare( m , ws->rprep ) ;

     for( nge=0,pk=0 ; pk < np ; pk++ ){
       int *perm = pset->perm + (size_t)pk*n ; float rp ;
       THD_simmat_to_tri_perm( model , perm , ws->sc1 ) ;
       if( cmp == CMP_RHOA ){
         (void)spearman_rank_prepare(m,ws->sc1) ;
         rp=mantel_prepared_rhoa(m,ws->sc1,ws->rprep) ;
       } else
         rp = (rv > 0.0f) ? spearman_rank_corr( m , ws->sc1 , rv , ws->rprep ) : 0.0f ;
       if( fabsf(rp) >= fabsf(robs) ) nge++ ;
       if( permnull != NULL ) permnull[pk] = fabsf(rp) ;
     }
   } else {
     for( nge=0,pk=0 ; pk < np ; pk++ ){
       int *perm = pset->perm + (size_t)pk*n ; float rp ;
       THD_simmat_to_tri_perm( model , perm , ws->yperm ) ;
       rp = THD_tri_corr( m , ws->tri , ws->yperm , cmp , ws->sc1 , ws->sc2 ) ;
       if( fabsf(rp) >= fabsf(robs) ) nge++ ;
       if( permnull != NULL ) permnull[pk] = fabsf(rp) ;
     }
   }

   /* the identity is slot 0 of the set, so it is counted in nge: this is the
      Phipson-Smyth (b+1)/(M+1) with no extra +1 */
   ps.pval = (float)nge / (float)np ;
   ps.zscr = THD_p_to_z( ps.pval , robs ) ;
   return ps ;
}

THD_permstat THD_mantel_corr_cached( THD_simmat *neural, THD_simmat *model,
                                     THD_mantel_cache *mc, int imod,
                                     THD_rdm_ws *ws, float *permnull )
{
   THD_permstat ps ; int m,pk,nge ; float robs,nss ;

   if( mc == NULL || neural == NULL || model == NULL || ws == NULL ){
     ps.stat=0.0f ; ps.pval=-1.0f ; ps.zscr=0.0f ; ps.nperm=0 ; return ps ;
   }
   if( imod < 0 || imod >= mc->nmodel || neural->n != mc->n ||
       model->n != mc->n || ws->m < mc->m )
     return THD_mantel_corr(neural,model,mc->cmp,mc->pset,ws,permnull) ;

   m=mc->m ; ps.stat=0.0f ; ps.pval=-1.0f ; ps.zscr=0.0f ; ps.nperm=mc->nperm ;
   THD_simmat_to_tri(neural,ws->tri) ;
   THD_simmat_to_tri(model,ws->yfit) ;
   robs=THD_tri_corr(m,ws->tri,ws->yfit,mc->cmp,ws->sc1,ws->sc2) ;
   ps.stat=robs ;

   memcpy(ws->rprep,ws->tri,sizeof(float)*m) ;
   nss=(mc->cmp==CMP_SPEARMAN || mc->cmp==CMP_RHOA)
          ? spearman_rank_prepare(m,ws->rprep)
          : mantel_pearson_prepare(m,ws->rprep) ;
   for( nge=0,pk=0 ; pk < mc->nperm ; pk++ ){
     size_t ix=(size_t)imod*mc->nperm+pk ;
     float rp=(mc->cmp==CMP_RHOA)
       ? mantel_prepared_rhoa(m,mc->prep+ix*m,ws->rprep)
       : mantel_prepared_corr(m,mc->prep+ix*m,mc->ss[ix],ws->rprep,nss) ;
     if( fabsf(rp) >= fabsf(robs) ) nge++ ;
     if( permnull != NULL ) permnull[pk]=fabsf(rp) ;
   }
   ps.pval=(float)nge/(float)mc->nperm ;
   ps.zscr=THD_p_to_z(ps.pval,robs) ;
   return ps ;
}

/*============================================================================*/
/*  Paired Mantel contrast: r(neural,A) - r(neural,B)                          */
/*============================================================================*/

float THD_mantel_contrast_effect( THD_simmat *neural, THD_simmat *A,
                                  THD_simmat *B, int cmp, THD_rdm_ws *ws,
                                  float *rA_out, float *rB_out )
{
   float ra,rb ; int m=ws->m ;
   THD_simmat_to_tri(neural,ws->tri) ;
   THD_simmat_to_tri(A,ws->yperm) ;
   ra=THD_tri_corr(m,ws->tri,ws->yperm,cmp,ws->sc1,ws->sc2) ;
   THD_simmat_to_tri(B,ws->yfit) ;
   rb=THD_tri_corr(m,ws->tri,ws->yfit,cmp,ws->sc1,ws->sc2) ;
   if( rA_out != NULL ) *rA_out=ra ;
   if( rB_out != NULL ) *rB_out=rb ;
   return ra-rb ;
}

THD_permstat THD_mantel_contrast( THD_simmat *neural , THD_simmat *A ,
                                  THD_simmat *B , int cmp , PERM_set *pset ,
                                  THD_rdm_ws *ws , float *permnull )
{
   THD_permstat ps ; int m = ws->m , n = neural->n , pk , nge ;
   float rA , rB , dobs ;
   int np = (pset != NULL) ? pset->nperm : 0 ;

   ps.stat = 0.0f ; ps.pval = -1.0f ; ps.zscr = 0.0f ; ps.nperm = np ;

   /* observed effect is computed independently of the chosen null generator */
   dobs = THD_mantel_contrast_effect(neural,A,B,cmp,ws,&rA,&rB) ;
   ps.stat = dobs ;

   if( pset == NULL ){ ps.zscr = MYatanh(dobs) ; return ps ; }

   if( cmp == CMP_SPEARMAN || cmp == CMP_RHOA ){
     /* only the model sides are relabeled, so rank the neural triangle once */
     float rv ;
     memcpy( ws->rprep , ws->tri , sizeof(float)*m ) ;
     rv = spearman_rank_prepare( m , ws->rprep ) ;

     for( nge=0,pk=0 ; pk < np ; pk++ ){
       int *perm = pset->perm + (size_t)pk*n ; float rap , rbp , dp ;
       THD_simmat_to_tri_perm( A , perm , ws->yperm ) ;
       THD_simmat_to_tri_perm( B , perm , ws->yfit  ) ;
       if( cmp == CMP_RHOA ){
         (void)spearman_rank_prepare(m,ws->yperm) ;
         (void)spearman_rank_prepare(m,ws->yfit) ;
         rap=mantel_prepared_rhoa(m,ws->yperm,ws->rprep) ;
         rbp=mantel_prepared_rhoa(m,ws->yfit ,ws->rprep) ;
       } else {
         rap = (rv > 0.0f) ? spearman_rank_corr( m , ws->yperm , rv , ws->rprep ) : 0.0f ;
         rbp = (rv > 0.0f) ? spearman_rank_corr( m , ws->yfit  , rv , ws->rprep ) : 0.0f ;
       }
       dp = rap - rbp ;
       if( fabsf(dp) >= fabsf(dobs) ) nge++ ;
       if( permnull != NULL ) permnull[pk] = fabsf(dp) ;
     }
   } else {
     for( nge=0,pk=0 ; pk < np ; pk++ ){
       int *perm = pset->perm + (size_t)pk*n ; float rap , rbp , dp ;
       THD_simmat_to_tri_perm( A , perm , ws->yperm ) ;
       rap = THD_tri_corr( m , ws->tri , ws->yperm , cmp , ws->sc1 , ws->sc2 ) ;
       THD_simmat_to_tri_perm( B , perm , ws->yfit ) ;
       rbp = THD_tri_corr( m , ws->tri , ws->yfit , cmp , ws->sc1 , ws->sc2 ) ;
       dp = rap - rbp ;
       if( fabsf(dp) >= fabsf(dobs) ) nge++ ;
       if( permnull != NULL ) permnull[pk] = fabsf(dp) ;
     }
   }

   ps.pval = (float)nge / (float)np ;
   ps.zscr = THD_p_to_z( ps.pval , dobs ) ;
   return ps ;
}

THD_permstat THD_mantel_contrast_cached( THD_simmat *neural,
                                         THD_simmat *A, THD_simmat *B,
                                         THD_mantel_cache *mc, int ia, int ib,
                                         THD_rdm_ws *ws, float *permnull )
{
   THD_permstat ps ; int m,pk,nge ; float rA,rB,dobs,nss ;

   if( mc == NULL || neural == NULL || A == NULL || B == NULL || ws == NULL ){
     ps.stat=0.0f ; ps.pval=-1.0f ; ps.zscr=0.0f ; ps.nperm=0 ; return ps ;
   }
   if( ia < 0 || ia >= mc->nmodel || ib < 0 || ib >= mc->nmodel ||
       neural->n != mc->n || A->n != mc->n || B->n != mc->n || ws->m < mc->m )
     return THD_mantel_contrast(neural,A,B,mc->cmp,mc->pset,ws,permnull) ;

   m=mc->m ; ps.stat=0.0f ; ps.pval=-1.0f ; ps.zscr=0.0f ; ps.nperm=mc->nperm ;
   dobs=THD_mantel_contrast_effect(neural,A,B,mc->cmp,ws,&rA,&rB) ;
   ps.stat=dobs ;

   memcpy(ws->rprep,ws->tri,sizeof(float)*m) ;
   nss=(mc->cmp==CMP_SPEARMAN || mc->cmp==CMP_RHOA)
          ? spearman_rank_prepare(m,ws->rprep)
          : mantel_pearson_prepare(m,ws->rprep) ;
   for( nge=0,pk=0 ; pk < mc->nperm ; pk++ ){
     size_t aix=(size_t)ia*mc->nperm+pk , bix=(size_t)ib*mc->nperm+pk ;
     float rap=(mc->cmp==CMP_RHOA)
       ? mantel_prepared_rhoa(m,mc->prep+aix*m,ws->rprep)
       : mantel_prepared_corr(m,mc->prep+aix*m,mc->ss[aix],ws->rprep,nss) ;
     float rbp=(mc->cmp==CMP_RHOA)
       ? mantel_prepared_rhoa(m,mc->prep+bix*m,ws->rprep)
       : mantel_prepared_corr(m,mc->prep+bix*m,mc->ss[bix],ws->rprep,nss) ;
     float dp=rap-rbp ;
     if( fabsf(dp) >= fabsf(dobs) ) nge++ ;
     if( permnull != NULL ) permnull[pk]=fabsf(dp) ;
   }
   ps.pval=(float)nge/(float)mc->nperm ;
   ps.zscr=THD_p_to_z(ps.pval,dobs) ;
   return ps ;
}

/*============================================================================*/
/*  Least squares over triangles                                              */
/*============================================================================*/

static int rdm_design_psinv( int m , int nc , float *Xin , float *Pout )
{
   MRI_IMAGE *imX , *imP ; float *xar , *par ; int ii , jj ;

   imX = mri_new( m , nc , MRI_float ) ;
   xar = MRI_FLOAT_PTR(imX) ;
   for( ii=0 ; ii < m ; ii++ )
     for( jj=0 ; jj < nc ; jj++ ) xar[ ii + jj*m ] = Xin[ ii*nc + jj ] ;

   imP = mri_matrix_psinv( imX , NULL , 0.0f ) ;
   mri_free(imX) ;
   if( imP == NULL ) return 0 ;

   par = MRI_FLOAT_PTR(imP) ;
   for( ii=0 ; ii < nc ; ii++ )
     for( jj=0 ; jj < m ; jj++ ) Pout[ ii*m + jj ] = par[ ii + jj*nc ] ;

   mri_free(imP) ;
   return 1 ;
}

static void rdm_apply_psinv( int m , int nc , float *P , float *y , float *beta )
{
   int ii , jj ; float sum ;
   for( ii=0 ; ii < nc ; ii++ ){
     float *prow = P + (size_t)ii*m ;
     for( sum=0.0f,jj=0 ; jj < m ; jj++ ) sum += prow[jj]*y[jj] ;
     beta[ii] = sum ;
   }
}

static void rdm_apply_design( int m , int nc , float *X , float *beta , float *fit )
{
   int ii , jj ; float sum ;
   for( ii=0 ; ii < m ; ii++ ){
     float *xrow = X + (size_t)ii*nc ;
     for( sum=0.0f,jj=0 ; jj < nc ; jj++ ) sum += xrow[jj]*beta[jj] ;
     fit[ii] = sum ;
   }
}

static void rdm_set_design_col( THD_rdm_ws *ws , float *X , int ncol , int c ,
                                THD_simmat *sm , int do_rank )
{
   int m = ws->m , ii ;
   THD_simmat_to_tri( sm , ws->tri ) ;
   if( do_rank ) rdm_rank_inplace( m , ws->tri , ws->rprep ) ;
   rdm_zscore( m , ws->tri ) ;
   for( ii=0 ; ii < m ; ii++ ) X[ ii*ncol + c ] = ws->tri[ii] ;
}

int THD_tri_regress( int m, float *yin, int nmod, float **xin, int cmp,
                     THD_rdm_ws *ws, float *beta )
{
   int ii, cc, do_rank ; float *y, *X, *P ;
   if( ws == NULL || yin == NULL || xin == NULL || beta == NULL ||
       m < 3 || m > ws->m || nmod < 1 || nmod > ws->ncol ) return 0 ;
   if( cmp != CMP_PEARSON && cmp != CMP_SPEARMAN ) return 0 ;

   do_rank = (cmp == CMP_SPEARMAN) ; y = ws->yperm ; X = ws->Xmat ; P = ws->Pmat ;
   memcpy(y,yin,sizeof(float)*m) ;
   if( do_rank ) rdm_rank_inplace(m,y,ws->rprep) ;
   if( !rdm_zscore(m,y) ){
     for( cc=0 ; cc < nmod ; cc++ ) beta[cc]=0.0f ;
     return 0 ;
   }
   for( cc=0 ; cc < nmod ; cc++ ){
     memcpy(ws->tri,xin[cc],sizeof(float)*m) ;
     if( do_rank ) rdm_rank_inplace(m,ws->tri,ws->rprep) ;
     if( !rdm_zscore(m,ws->tri) ) memset(ws->tri,0,sizeof(float)*m) ;
     for( ii=0 ; ii < m ; ii++ ) X[ii*nmod+cc] = ws->tri[ii] ;
   }
   if( !rdm_design_psinv(m,nmod,X,P) ){
     for( cc=0 ; cc < nmod ; cc++ ) beta[cc]=0.0f ;
     return 0 ;
   }
   rdm_apply_psinv(m,nmod,P,y,beta) ;
   return 1 ;
}

/* A circular-shift regression changes only y.  Keeping the standardized
   design pseudoinverse avoids repeating an SVD for every null draw. */
struct THD_tri_design {
   int m , ncol , cmp ;
   float *pinv ;                         /* [ncol*m] */
} ;

THD_tri_design * THD_tri_design_new( int m, int ncol, float **xin, int cmp,
                                     THD_rdm_ws *ws )
{
   THD_tri_design *dd ; int ii,cc,do_rank ; float *X ;
   if( ws == NULL || xin == NULL || m < 3 || m > ws->m ||
       ncol < 1 || ncol > ws->ncol ||
       (cmp != CMP_PEARSON && cmp != CMP_SPEARMAN) ) return NULL ;

   dd=(THD_tri_design *)calloc(1,sizeof(THD_tri_design)) ;
   if( dd == NULL ) return NULL ;
   dd->m=m ; dd->ncol=ncol ; dd->cmp=cmp ;
   dd->pinv=(float *)malloc(sizeof(float)*(size_t)ncol*m) ;
   if( dd->pinv == NULL ){ free(dd) ; return NULL ; }

   do_rank=(cmp == CMP_SPEARMAN) ; X=ws->Xmat ;
   for( cc=0 ; cc<ncol ; cc++ ){
     memcpy(ws->tri,xin[cc],sizeof(float)*m) ;
     if( do_rank ) rdm_rank_inplace(m,ws->tri,ws->rprep) ;
     if( !rdm_zscore(m,ws->tri) ) memset(ws->tri,0,sizeof(float)*m) ;
     for( ii=0 ; ii<m ; ii++ ) X[ii*ncol+cc]=ws->tri[ii] ;
   }
   if( !rdm_design_psinv(m,ncol,X,dd->pinv) ){
     THD_tri_design_free(dd) ; return NULL ;
   }
   return dd ;
}

int THD_tri_design_apply( THD_tri_design *dd, float *yin,
                          THD_rdm_ws *ws, float *beta )
{
   float *y ;
   if( dd == NULL || yin == NULL || ws == NULL || beta == NULL ||
       dd->m > ws->m || dd->ncol > ws->ncol ) return 0 ;
   y=ws->yperm ; memcpy(y,yin,sizeof(float)*dd->m) ;
   if( dd->cmp == CMP_SPEARMAN ) rdm_rank_inplace(dd->m,y,ws->rprep) ;
   if( !rdm_zscore(dd->m,y) ){
     memset(beta,0,sizeof(float)*dd->ncol) ; return 0 ;
   }
   rdm_apply_psinv(dd->m,dd->ncol,dd->pinv,y,beta) ;
   return 1 ;
}

void THD_tri_design_free( THD_tri_design *dd )
{
   if( dd == NULL ) return ;
   if( dd->pinv != NULL ) free(dd->pinv) ;
   free(dd) ;
}

static void rdm_residual_excluding( int m , int ncol , float *X , int skip ,
                                    float *y , float *out ,
                                    float *Zx , float *Zp , float *bz , float *fit )
{
   int nc = ncol-1 , ii , jj , kk ;

   if( nc == 0 ){ memcpy( out , y , sizeof(float)*m ) ; return ; }

   for( ii=0 ; ii < m ; ii++ )
     for( kk=0,jj=0 ; jj < ncol ; jj++ )
       if( jj != skip ) Zx[ ii*nc + (kk++) ] = X[ ii*ncol + jj ] ;

   if( !rdm_design_psinv( m , nc , Zx , Zp ) ){ memcpy(out,y,sizeof(float)*m) ; return ; }
   rdm_apply_psinv ( m , nc , Zp , y , bz ) ;
   rdm_apply_design( m , nc , Zx , bz , fit ) ;
   for( ii=0 ; ii < m ; ii++ ) out[ii] = y[ii] - fit[ii] ;
}

static int rdm_regress_core( THD_simmat *neural , int nmod , THD_simmat **models ,
                            int nort , THD_simmat **orts , int cmp ,
                            PERM_set *pset , THD_rdm_ws *ws ,
                            float *beta , float *partial_r , float *pval ,
                            float *permnull , int signed_null )
{
   int m = ws->m , n = ws->n , ii , mm , cc ;
   int ncol = nmod + nort ;
   int do_rank = (cmp == CMP_SPEARMAN) ;
   int np = (pset != NULL) ? pset->nperm : 0 ;
   float *y = ws->yperm ;
   float *X = ws->Xmat , *P = ws->Pmat , *betaF ;
   float *Zx=NULL , *Zp=NULL , *bz=NULL , *xj=NULL , *ry=NULL , *fit=NULL ;

   /* A caller pooling a max-statistic FWE null hands us a permnull scratch
      buffer that it will fold in unconditionally.  Any early return that skips
      the permutation loop below MUST neutralize it first, or stale values left
      from a previous element silently corrupt the shared max-null.  Zero means
      "this element contributed nothing", which is exactly right for an element
      where no test could run. */
   if( permnull != NULL && np > 0 )
     memset( permnull , 0 , sizeof(float)*(size_t)nmod*np ) ;

   if( nmod < 1 || ncol > ws->ncol ) return 1 ;

   THD_simmat_to_tri( neural , y ) ;
   if( do_rank ) rdm_rank_inplace( m , y , ws->rprep ) ;
   if( !rdm_zscore( m , y ) ){                    /* constant neural matrix */
     for( mm=0 ; mm < nmod ; mm++ ){
       beta[mm] = partial_r[mm] = 0.0f ;
       if( pval != NULL ) pval[mm] = 1.0f ;
     }
     return 0 ;
   }

   for( cc=0 ; cc < nmod ; cc++ ) rdm_set_design_col( ws,X,ncol,cc,models[cc],do_rank ) ;
   for( cc=0 ; cc < nort ; cc++ ) rdm_set_design_col( ws,X,ncol,nmod+cc,orts[cc],do_rank ) ;

   betaF = ws->beta ;
   if( !rdm_design_psinv( m , ncol , X , P ) ) return 1 ;
   rdm_apply_psinv( m , ncol , P , y , betaF ) ;
   for( mm=0 ; mm < nmod ; mm++ ) beta[mm] = betaF[mm] ;

   if( ncol > 1 ){
     Zx = (float *)malloc(sizeof(float)*(size_t)m*(ncol-1)) ;
     Zp = (float *)malloc(sizeof(float)*(size_t)m*(ncol-1)) ;
   }
   bz  = (float *)malloc(sizeof(float)*ncol) ;
   xj  = (float *)malloc(sizeof(float)*m) ;
   ry  = (float *)malloc(sizeof(float)*m) ;
   fit = (float *)malloc(sizeof(float)*m) ;

   for( mm=0 ; mm < nmod ; mm++ ){
     if( ncol == 1 ){ partial_r[0] = beta[0] ; break ; }
     rdm_residual_excluding( m , ncol , X , mm , y , ry , Zx , Zp , bz , fit ) ;
     for( ii=0 ; ii < m ; ii++ ) xj[ii] = X[ ii*ncol + mm ] ;
     rdm_residual_excluding( m , ncol , X , mm , xj , ws->sc2 , Zx , Zp , bz , fit ) ;
     partial_r[mm] = THD_pearson_corr( m , ry , ws->sc2 ) ;
   }

   if( pval == NULL || pset == NULL ){
     if( Zx != NULL ){ free(Zx) ; free(Zp) ; }
     free(bz) ; free(xj) ; free(ry) ; free(fit) ; return 0 ;
   }

   /*-- Freedman-Lane: fit y on all OTHER columns, relabel that reduced residual
        (via pset) and refit the full design. --*/

   for( mm=0 ; mm < nmod ; mm++ ){
     int nge=0 , pk ; float bobs = beta[mm] ;
     float aobs=fabsf(bobs),tie_tol=64.0f*FLT_EPSILON*(1.0f+aobs) ;

     rdm_residual_excluding( m , ncol , X , mm , y , ws->Zres , Zx , Zp , bz , fit ) ;
     for( ii=0 ; ii < m ; ii++ ) ws->Zfit[ii] = y[ii] - ws->Zres[ii] ;

     THD_tri_to_simmat( n , ws->Zres , ws->ework ) ;

     for( pk=0 ; pk < np ; pk++ ){
       int *perm = pset->perm + (size_t)pk*n ;
       THD_simmat_to_tri_perm( ws->ework , perm , ws->sc1 ) ;
       for( ii=0 ; ii < m ; ii++ ) ws->yfit[ii] = ws->Zfit[ii] + ws->sc1[ii] ;
       rdm_apply_psinv( m , ncol , P , ws->yfit , bz ) ;
       { float av=fabsf(bz[mm]) ;
         if( pk==0 || fabsf(av-aobs)<=tie_tol ) av=aobs ;
         if( av>=aobs ) nge++ ; }
       if( permnull != NULL ) permnull[(size_t)mm*np + pk]
                                = signed_null ? bz[mm] : fabsf(bz[mm]) ;
     }
     /* PERM_set slot zero is the identity.  Make that algebraic fact exact in
        both signed trajectory-combination nulls and absolute max-null buffers. */
     if( permnull != NULL && np>0 ) permnull[(size_t)mm*np]
                                     = signed_null ? bobs : aobs ;
     pval[mm] = (float)nge / (float)np ;
   }

   if( Zx != NULL ){ free(Zx) ; free(Zp) ; }
   free(bz) ; free(xj) ; free(ry) ; free(fit) ;
   return 0 ;
}

int THD_rdm_regress( THD_simmat *neural , int nmod , THD_simmat **models ,
                     int nort , THD_simmat **orts , int cmp ,
                     PERM_set *pset , THD_rdm_ws *ws ,
                     float *beta , float *partial_r , float *pval ,
                     float *permnull )
{
   return rdm_regress_core(neural,nmod,models,nort,orts,cmp,pset,ws,
                           beta,partial_r,pval,permnull,0) ;
}

int THD_rdm_regress_signed( THD_simmat *neural , int nmod , THD_simmat **models ,
                            int nort , THD_simmat **orts , int cmp ,
                            PERM_set *pset , THD_rdm_ws *ws ,
                            float *beta , float *partial_r , float *pval ,
                            float *permnull )
{
   return rdm_regress_core(neural,nmod,models,nort,orts,cmp,pset,ws,
                           beta,partial_r,pval,permnull,1) ;
}

/*----------------------------------------------------------------------------*/

void THD_rdm_model_corr( int nmod , THD_simmat **models , int cmp ,
                         THD_rdm_ws *ws , float *cmat )
{
   int ii , jj , m = ws->m ;
   float *a = (float *)malloc(sizeof(float)*(size_t)m*nmod) ;

   for( ii=0 ; ii < nmod ; ii++ )
     THD_simmat_to_tri( models[ii] , a + (size_t)ii*m ) ;

   for( ii=0 ; ii < nmod ; ii++ ){
     cmat[ii*nmod+ii] = 1.0f ;
     for( jj=ii+1 ; jj < nmod ; jj++ ){
       float c = THD_tri_corr( m , a+(size_t)ii*m , a+(size_t)jj*m , cmp ,
                               ws->sc1 , ws->sc2 ) ;
       cmat[ii*nmod+jj] = cmat[jj*nmod+ii] = c ;
     }
   }
   free(a) ;
}

/*============================================================================*/
/*  Leave-one-subject-out IS-RSA prediction                                   */
/*============================================================================*/

static float rdm_loo_predict( THD_simmat *S , float *behav , int cmp ,
                              float *bhat , float *wtmp , float *stmp ,
                              float *sc1 , float *sc2 )
{
   int n = S->n , i , j , k ;
   double num , den ;

   /* Weight neighbors by the RANK of their closeness to subject i, so the
      neurally nearest subjects dominate the prediction.  For a SIMILARITY
      matrix bigger is nearer, so we rank the raw values; for a DISTANCE matrix
      bigger is farther, so we rank the negated values -- otherwise the farthest
      subjects would (wrongly) get the largest weights.  Ranking -d handles ties
      exactly as ranking d does, so this is a clean reversal. */
   for( i=0 ; i < n ; i++ ){
     for( k=0,j=0 ; j < n ; j++ ){
       if( j == i ) continue ;
       stmp[k++] = S->is_dist ? -S->mat[i*n+j] : S->mat[i*n+j] ;
     }
     THD_rank_avg( n-1 , stmp , wtmp ) ;
     for( num=den=0.0,k=0,j=0 ; j < n ; j++ ){
       if( j == i ) continue ;
       num += (double)wtmp[k]*behav[j] ; den += (double)wtmp[k] ; k++ ;
     }
     bhat[i] = (den > 0.0) ? (float)(num/den) : 0.0f ;
   }
   return THD_tri_corr( n , bhat , behav , cmp , sc1 , sc2 ) ;
}

/*! Precompute the fold-specific neural typicalities used by the AnnaK
    predictor.  held[i] is subject i's mean closeness to its n-1 training
    subjects.  train[i*n+j] is training subject j's mean closeness to the other
    n-2 training subjects in fold i; j==i is unused.  Crucially, neither value
    for fold i uses the held target's behavior, nor does the fitted training
    predictor use neural edge (i,j) when estimating subject j's typicality. */
static void rdm_loo_annak_design( THD_simmat *S , float *held , float *train )
{
   int n=S->n , i,j,k ; double sum ;
   for( i=0 ; i<n ; i++ ){
     for( sum=0.0,j=0 ; j<n ; j++ )
       if( j != i ) sum += S->is_dist ? -S->mat[i*n+j] : S->mat[i*n+j] ;
     held[i]=(float)(sum/(n-1)) ;
     for( j=0 ; j<n ; j++ ){
       if( j == i ){ train[i*n+j]=0.0f ; continue ; }
       for( sum=0.0,k=0 ; k<n ; k++ )
         if( k != i && k != j )
           sum += S->is_dist ? -S->mat[j*n+k] : S->mat[j*n+k] ;
       train[i*n+j]=(float)(sum/(n-2)) ;
     }
   }
}

static float rdm_loo_predict_annak( int n , float *behav , int cmp ,
                                    float *held , float *train , float *bhat ,
                                    float *sc1 , float *sc2 )
{
   int i,j ;
   for( i=0 ; i<n ; i++ ){
     double mx=0.0,my=0.0,sxx=0.0,sxy=0.0 ;
     for( j=0 ; j<n ; j++ ) if( j != i ){
       mx += train[i*n+j] ; my += behav[j] ;
     }
     mx /= n-1 ; my /= n-1 ;
     for( j=0 ; j<n ; j++ ) if( j != i ){
       double dx=train[i*n+j]-mx , dy=behav[j]-my ;
       sxx += dx*dx ; sxy += dx*dy ;
     }
     bhat[i]=(sxx > 0.0) ? (float)(my+sxy/sxx*(held[i]-mx)) : (float)my ;
   }
   return THD_tri_corr( n , bhat , behav , cmp , sc1 , sc2 ) ;
}

static float rdm_loo_predict_profile( THD_simmat *S , int p , float **behav ,
                                      int cmp , float *bhat , float *wtmp ,
                                      float *stmp , float *sc1 , float *sc2 )
{
   int n=S->n , i,j,k,v ; double acc=0.0 ;

   /* bhat is variable-major [p][n].  Derive the neural weights once per held
      subject, then apply that identical, outcome-blind fold to every measure. */
   for( i=0 ; i<n ; i++ ){
     for( k=0,j=0 ; j<n ; j++ ) if( j != i )
       stmp[k++]=S->is_dist ? -S->mat[i*n+j] : S->mat[i*n+j] ;
     THD_rank_avg(n-1,stmp,wtmp) ;
     for( v=0 ; v<p ; v++ ){
       double num=0.0,den=0.0 ;
       for( k=0,j=0 ; j<n ; j++ ) if( j != i ){
         num += (double)wtmp[k]*behav[v][j] ; den += wtmp[k] ; k++ ;
       }
       bhat[(size_t)v*n+i]=(den>0.0) ? (float)(num/den) : 0.0f ;
     }
   }
   for( v=0 ; v<p ; v++ )
     acc += THD_tri_corr(n,bhat+(size_t)v*n,behav[v],cmp,sc1,sc2) ;
   return (float)(acc/p) ;
}

THD_permstat THD_isrsa_loo_pred( THD_simmat *neural , float *behav ,
                                 int cmp , PERM_set *pset , THD_rdm_ws *ws ,
                                 float *permnull , float *pred )
{
   THD_permstat ps ; int n = neural->n , pk , jj , nge ;
   int np = (pset != NULL) ? pset->nperm : 0 ;
   float robs , *bhat , *wtmp , *stmp , *bperm ;

   bhat  = (float *)malloc(sizeof(float)*n) ;
   wtmp  = (float *)malloc(sizeof(float)*n) ;
   stmp  = (float *)malloc(sizeof(float)*n) ;
   bperm = (float *)malloc(sizeof(float)*n) ;

   robs = rdm_loo_predict( neural , behav , cmp , bhat , wtmp , stmp , ws->sc1 , ws->sc2 ) ;
   if( pred != NULL ) memcpy(pred,bhat,sizeof(float)*n) ;
   ps.stat = robs ; ps.pval = -1.0f ; ps.zscr = MYatanh(robs) ; ps.nperm = np ;

   if( pset != NULL ){
     for( nge=0,pk=0 ; pk < np ; pk++ ){
       int *perm = pset->perm + (size_t)pk*n ; float rp ;
       for( jj=0 ; jj < n ; jj++ ) bperm[jj] = behav[ perm[jj] ] ;
       rp = rdm_loo_predict( neural , bperm , cmp , bhat , wtmp , stmp , ws->sc1 , ws->sc2 ) ;
       if( fabsf(rp) >= fabsf(robs) ) nge++ ;
       if( permnull != NULL ) permnull[pk] = fabsf(rp) ;
     }
     ps.pval = (float)nge / (float)np ;
     ps.zscr = THD_p_to_z( ps.pval , robs ) ;
   }

   free(bhat) ; free(wtmp) ; free(stmp) ; free(bperm) ;
   return ps ;
}

THD_permstat THD_isrsa_loo( THD_simmat *neural , float *behav ,
                            int cmp , PERM_set *pset , THD_rdm_ws *ws ,
                            float *permnull )
{
   return THD_isrsa_loo_pred(neural,behav,cmp,pset,ws,permnull,NULL) ;
}

THD_permstat THD_isrsa_loo_annak_pred( THD_simmat *neural , float *behav ,
                                       int cmp , PERM_set *pset ,
                                       THD_rdm_ws *ws , float *permnull ,
                                       float *pred )
{
   THD_permstat ps ; int n=neural->n,pk,jj,nge ;
   int np=(pset != NULL) ? pset->nperm : 0 ;
   float robs,*held,*train,*bhat,*bperm ;

   held=(float *)malloc(sizeof(float)*n) ;
   train=(float *)malloc(sizeof(float)*(size_t)n*n) ;
   bhat=(float *)malloc(sizeof(float)*n) ;
   bperm=(float *)malloc(sizeof(float)*n) ;
   rdm_loo_annak_design(neural,held,train) ;
   robs=rdm_loo_predict_annak(n,behav,cmp,held,train,bhat,ws->sc1,ws->sc2) ;
   if( pred != NULL ) memcpy(pred,bhat,sizeof(float)*n) ;
   ps.stat=robs ; ps.pval=-1.0f ; ps.zscr=MYatanh(robs) ; ps.nperm=np ;

   if( pset != NULL ){
     for( nge=0,pk=0 ; pk<np ; pk++ ){
       int *perm=pset->perm+(size_t)pk*n ; float rp ;
       for( jj=0 ; jj<n ; jj++ ) bperm[jj]=behav[perm[jj]] ;
       rp=rdm_loo_predict_annak(n,bperm,cmp,held,train,bhat,ws->sc1,ws->sc2) ;
       if( fabsf(rp) >= fabsf(robs) ) nge++ ;
       if( permnull != NULL ) permnull[pk]=fabsf(rp) ;
     }
     ps.pval=(float)nge/(float)np ; ps.zscr=THD_p_to_z(ps.pval,robs) ;
   }
   free(held) ; free(train) ; free(bhat) ; free(bperm) ; return ps ;
}

THD_permstat THD_isrsa_loo_annak( THD_simmat *neural , float *behav ,
                                  int cmp , PERM_set *pset , THD_rdm_ws *ws ,
                                  float *permnull )
{
   return THD_isrsa_loo_annak_pred(neural,behav,cmp,pset,ws,permnull,NULL) ;
}

THD_permstat THD_isrsa_loo_profile_pred( THD_simmat *neural , int p ,
                                         float **behav , int cmp ,
                                         PERM_set *pset , THD_rdm_ws *ws ,
                                         float *permnull , float *pred )
{
   THD_permstat ps ; int n=neural->n,pk,jj,v,nge ;
   int np=(pset != NULL) ? pset->nperm : 0 ;
   float robs,*bhat,*wtmp,*stmp,*bpflat,**bperm ;

   bhat=(float *)malloc(sizeof(float)*(size_t)p*n) ;
   wtmp=(float *)malloc(sizeof(float)*n) ;
   stmp=(float *)malloc(sizeof(float)*n) ;
   bpflat=(float *)malloc(sizeof(float)*(size_t)p*n) ;
   bperm=(float **)malloc(sizeof(float *)*p) ;
   for( v=0 ; v<p ; v++ ) bperm[v]=bpflat+(size_t)v*n ;
   robs=rdm_loo_predict_profile(neural,p,behav,cmp,bhat,wtmp,stmp,
                                ws->sc1,ws->sc2) ;
   if( pred != NULL ) memcpy(pred,bhat,sizeof(float)*(size_t)p*n) ;
   ps.stat=robs ; ps.pval=-1.0f ; ps.zscr=MYatanh(robs) ; ps.nperm=np ;

   if( pset != NULL ){
     for( nge=0,pk=0 ; pk<np ; pk++ ){
       int *perm=pset->perm+(size_t)pk*n ; float rp ;
       for( v=0 ; v<p ; v++ ) for( jj=0 ; jj<n ; jj++ )
         bperm[v][jj]=behav[v][perm[jj]] ;
       rp=rdm_loo_predict_profile(neural,p,bperm,cmp,bhat,wtmp,stmp,
                                  ws->sc1,ws->sc2) ;
       if( fabsf(rp) >= fabsf(robs) ) nge++ ;
       if( permnull != NULL ) permnull[pk]=fabsf(rp) ;
     }
     ps.pval=(float)nge/(float)np ; ps.zscr=THD_p_to_z(ps.pval,robs) ;
   }
   free(bhat) ; free(wtmp) ; free(stmp) ; free(bpflat) ; free(bperm) ; return ps ;
}

THD_permstat THD_isrsa_loo_profile( THD_simmat *neural , int p ,
                                    float **behav , int cmp , PERM_set *pset ,
                                    THD_rdm_ws *ws , float *permnull )
{
   return THD_isrsa_loo_profile_pred(neural,p,behav,cmp,pset,ws,permnull,NULL) ;
}

/*============================================================================*/
/*  Sign-flip one-sample test (classic within-subject RSA)                    */
/*============================================================================*/

THD_permstat THD_signflip_t( int n , float *v , PERM_set *pset , float *permnull )
{
   THD_permstat ps ; int pk , jj , nge ; float tobs , *flip ;
   int np = (pset != NULL) ? pset->nperm : 0 ;

   tobs = THD_onesamp_t( n , v ) ;
   ps.stat = tobs ; ps.pval = -1.0f ; ps.zscr = 0.0f ; ps.nperm = np ;

   if( pset == NULL ){
     ps.pval = (float)student_t2p( (double)fabsf(tobs) , (double)(n-1) ) ;
     ps.zscr = THD_p_to_z( ps.pval , tobs ) ;
     return ps ;
   }

   flip = (float *)malloc(sizeof(float)*n) ;
   for( nge=0,pk=0 ; pk < np ; pk++ ){
     signed char *sg = pset->sign + (size_t)pk*n ; float tp ;
     for( jj=0 ; jj < n ; jj++ ) flip[jj] = sg[jj] * v[jj] ;
     tp = fabsf(THD_onesamp_t(n,flip)) ;
     if( tp >= fabsf(tobs) ) nge++ ;
     if( permnull != NULL ) permnull[pk] = tp ;
   }
   free(flip) ;
   ps.pval = (float)nge / (float)np ;
   ps.zscr = THD_p_to_z( ps.pval , tobs ) ;
   return ps ;
}

/*============================================================================*/
/*  Wilcoxon signed-rank test by sign flipping                                 */
/*============================================================================*/

THD_permstat THD_signrank_signflip( int n , float *v , PERM_set *pset ,
                                    float *permnull )
{
   THD_permstat ps ; int pk , jj , nge ; float Wobs , *sr , *av , *rk ;
   int np = (pset != NULL) ? pset->nperm : 0 ;

   ps.stat = 0.0f ; ps.pval = -1.0f ; ps.zscr = 0.0f ; ps.nperm = np ;

   /* signed ranks: sr_i = sign(v_i) * rank(|v_i|), ties sharing their mean rank.
      Under a sign flip s the magnitudes |v_i| (hence the ranks) are unchanged and
      only the signs move, so the flipped statistic is simply sum_i s_i*sr_i --
      a plain sign-flip test on the signed-rank vector. */
   sr = (float *)malloc(sizeof(float)*n) ;
   av = (float *)malloc(sizeof(float)*n) ;
   rk = (float *)malloc(sizeof(float)*n) ;
   for( jj=0 ; jj < n ; jj++ ) av[jj] = fabsf(v[jj]) ;
   THD_rank_avg( n , av , rk ) ;
   for( Wobs=0.0f,jj=0 ; jj < n ; jj++ ){
     sr[jj] = ( (v[jj] > 0.0f) - (v[jj] < 0.0f) ) * rk[jj] ;
     Wobs += sr[jj] ;
   }
   ps.stat = Wobs ;

   if( pset == NULL ){
     /* sign-flip null: E[W]=0, Var[W]=sum sr_i^2 -> a normal approximation */
     double var=0.0 , z ;
     for( jj=0 ; jj < n ; jj++ ) var += (double)sr[jj]*sr[jj] ;
     z = (var > 0.0) ? (double)Wobs / sqrt(var) : 0.0 ;
     ps.pval = (float)erfc( fabs(z)/sqrt(2.0) ) ;
     ps.zscr = THD_p_to_z( ps.pval , Wobs ) ;
     free(sr) ; free(av) ; free(rk) ; return ps ;
   }

   for( nge=0,pk=0 ; pk < np ; pk++ ){
     signed char *sg = pset->sign + (size_t)pk*n ; float Wp=0.0f ;
     for( jj=0 ; jj < n ; jj++ ) Wp += sg[jj] * sr[jj] ;
     Wp = fabsf(Wp) ;
     if( Wp >= fabsf(Wobs) ) nge++ ;
     if( permnull != NULL ) permnull[pk] = Wp ;
   }
   free(sr) ; free(av) ; free(rk) ;
   ps.pval = (float)nge / (float)np ;
   ps.zscr = THD_p_to_z( ps.pval , Wobs ) ;
   return ps ;
}

/*============================================================================*/
/*  Two-model commonality (variance partitioning)                             */
/*============================================================================*/

/* Decompose the variance the (already z-scored) triangles At, Bt jointly explain
   in a z-scored neural triangle y into {uniq_A, uniq_B, common}, then append the
   two partial-R2 unique effects.  Everything is accumulated in DOUBLE and R2_AB
   uses the closed-form 2-predictor OLS -- 'common' is R2_A + R2_B - R2_AB, a
   difference of near-equal quantities that would lose all its significant digits
   in single precision when the true value is ~0. */

static void rdm_commonality_one( int m , float *y , float *At , float *Bt ,
                                 double *comp )
{
   double Saa=0.0 , Sbb=0.0 , Sab=0.0 , gA=0.0 , gB=0.0 , tss=0.0 ;
   double r2A , r2B , r2AB , det , denA , denB ; int ii ;

   for( ii=0 ; ii < m ; ii++ ){
     double a = At[ii] , b = Bt[ii] , yy = y[ii] ;
     Saa += a*a ; Sbb += b*b ; Sab += a*b ;
     gA  += yy*a ; gB += yy*b ; tss += yy*yy ;
   }
   r2A = (tss > 0.0 && Saa > 0.0) ? (gA*gA)/(tss*Saa) : 0.0 ;
   r2B = (tss > 0.0 && Sbb > 0.0) ? (gB*gB)/(tss*Sbb) : 0.0 ;

   det = Saa*Sbb - Sab*Sab ;
   if( tss <= 0.0 ){
     r2AB = 0.0 ;
   } else if( det > 1.0e-12*Saa*Sbb ){          /* well-conditioned 2-var OLS */
     double b0 = ( Sbb*gA - Sab*gB ) / det ;    /* beta = (X'X)^-1 X'y        */
     double b1 = ( Saa*gB - Sab*gA ) / det ;
     r2AB = ( b0*gA + b1*gB ) / tss ;           /* ESS/TSS = beta'X'y / TSS   */
   } else {
     r2AB = (r2A > r2B) ? r2A : r2B ;           /* collinear: one spans both  */
   }

   comp[0] = r2AB - r2B ;          /* unique to A */
   comp[1] = r2AB - r2A ;          /* unique to B */
   comp[2] = r2A + r2B - r2AB ;    /* common (may be negative) */
   denA = 1.0-r2B ;
   denB = 1.0-r2A ;
   comp[3] = (denA > 1.0e-12) ? comp[0]/denA : 0.0 ; /* partial R2 A | B */
   comp[4] = (denB > 1.0e-12) ? comp[1]/denB : 0.0 ; /* partial R2 B | A */
}

/* Freedman-Lane null for one unique effect.  'which' is 0 for A|B and 1 for
   B|A.  y, At, and Bt have already undergone the requested rank transform and
   z-scoring.  The reduced fit is therefore on the same scale as the observed
   commonality decomposition.  Relabeling the residual as an RDM (rather than
   shuffling triangle entries) preserves the item-label exchangeability
   contract used everywhere else in this file. */

static void rdm_commonality_fl( int m , int n , int which , float *y ,
                                float *At , float *Bt , double *obs ,
                                PERM_set *pset , THD_rdm_ws *ws , int *nge ,
                                float *permnull )
{
   float *red = (which == 0) ? Bt : At ;
   double ss=0.0 , gy=0.0 , beta ;
   int ii , pk , np=pset->nperm , raw=which , partial=which+3 ;

   for( ii=0 ; ii < m ; ii++ ){
     ss += (double)red[ii]*red[ii] ;
     gy += (double)red[ii]*y[ii] ;
   }
   beta = (ss > 0.0) ? gy/ss : 0.0 ;
   for( ii=0 ; ii < m ; ii++ ){
     ws->Zfit[ii] = (float)(beta*red[ii]) ;
     ws->Zres[ii] = y[ii]-ws->Zfit[ii] ;
   }
   THD_tri_to_simmat( n , ws->Zres , ws->ework ) ;

   for( pk=0 ; pk < np ; pk++ ){
     int *perm=pset->perm+(size_t)pk*n ;
     double comp[THD_NCOMMON] ;
     THD_simmat_to_tri_perm( ws->ework , perm , ws->sc1 ) ;
     for( ii=0 ; ii < m ; ii++ ) ws->yfit[ii]=ws->Zfit[ii]+ws->sc1[ii] ;
     rdm_commonality_one( m , ws->yfit , At , Bt , comp ) ;
     if( fabs(comp[raw])     >= fabs(obs[raw])     ) nge[raw]++ ;
     if( fabs(comp[partial]) >= fabs(obs[partial]) ) nge[partial]++ ;
     if( permnull != NULL ){
       permnull[(size_t)raw*np+pk]     = (float)fabs(comp[raw]) ;
       permnull[(size_t)partial*np+pk] = (float)fabs(comp[partial]) ;
     }
   }
}

int THD_tri_commonality( int m, float *yin, float *ain, float *bin, int cmp,
                         THD_rdm_ws *ws, float *out )
{
   float *y, *At, *Bt ; double comp[THD_NCOMMON] ; int cc, do_rank ;

   if( out != NULL ) for( cc=0 ; cc < THD_NCOMMON ; cc++ ) out[cc]=0.0f ;
   if( m < 3 || ws == NULL || m > ws->m || yin == NULL || ain == NULL ||
       bin == NULL || out == NULL ||
       (cmp != CMP_PEARSON && cmp != CMP_SPEARMAN) ) return 1 ;

   y=ws->yperm ; At=ws->tri ; Bt=ws->yfit ; do_rank=(cmp==CMP_SPEARMAN) ;
   memcpy(y ,yin,sizeof(float)*m) ;
   memcpy(At,ain,sizeof(float)*m) ;
   memcpy(Bt,bin,sizeof(float)*m) ;
   if( do_rank ){
     rdm_rank_inplace(m,y ,ws->rprep) ;
     rdm_rank_inplace(m,At,ws->rprep) ;
     rdm_rank_inplace(m,Bt,ws->rprep) ;
   }
   rdm_zscore(m,At) ; rdm_zscore(m,Bt) ;
   if( !rdm_zscore(m,y) )
     for( cc=0 ; cc < THD_NCOMMON ; cc++ ) comp[cc]=0.0 ;
   else rdm_commonality_one(m,y,At,Bt,comp) ;
   for( cc=0 ; cc < THD_NCOMMON ; cc++ ) out[cc]=(float)comp[cc] ;
   return 0 ;
}

int THD_commonality( THD_simmat *neural , THD_simmat *A , THD_simmat *B ,
                     int cmp , PERM_set *pset , THD_rdm_ws *ws ,
                     float *out , float *pval , float *zscr , float *permnull )
{
   int m , n , pk , cc ;
   int np = (pset != NULL) ? pset->nperm : 0 ;
   int do_rank = (cmp == CMP_SPEARMAN) ;
   float *At , *Bt , *y , *rk ;
   double obs[THD_NCOMMON] ; int nge[THD_NCOMMON] ;

   if( neural == NULL || A == NULL || B == NULL || ws == NULL || out == NULL )
     return 1 ;
   m=ws->m ; n=neural->n ;

   /* As in THD_rdm_regress, neutralize caller-owned FWE scratch before any
      early return so a bad/constant location cannot leak a previous null. */
   if( permnull != NULL && np > 0 )
     memset(permnull,0,sizeof(float)*(size_t)THD_NCOMMON*np) ;

   if( A->n != n || B->n != n || ws->n != n ||
       (pset != NULL && (pset->nobs != n || np < 1)) ||
       (cmp != CMP_PEARSON && cmp != CMP_SPEARMAN) ) return 1 ;

   At = (float *)malloc(sizeof(float)*m) ;
   Bt = (float *)malloc(sizeof(float)*m) ;
   y  = (float *)malloc(sizeof(float)*m) ;
   rk = (float *)malloc(sizeof(float)*m) ;

   /* model triangles: rank (Spearman) then z-score, fixed across the null */
   THD_simmat_to_tri( A , At ) ; if( do_rank ) rdm_rank_inplace(m,At,rk) ; rdm_zscore(m,At) ;
   THD_simmat_to_tri( B , Bt ) ; if( do_rank ) rdm_rank_inplace(m,Bt,rk) ; rdm_zscore(m,Bt) ;

   /* observed (unpermuted neural) */
   THD_simmat_to_tri( neural , y ) ;
   if( do_rank ) rdm_rank_inplace(m,y,rk) ;
   if( !rdm_zscore(m,y) )
     for( cc=0 ; cc < THD_NCOMMON ; cc++ ) obs[cc]=0.0 ;
   else rdm_commonality_one( m , y , At , Bt , obs ) ;
   for( cc=0 ; cc < THD_NCOMMON ; cc++ ) out[cc] = (float)obs[cc] ;

   if( pset == NULL ){                              /* point estimate only */
     if( pval != NULL ) for(cc=0;cc<THD_NCOMMON;cc++){
       pval[cc]=-1.0f ; if(zscr) zscr[cc]=0.0f ;
     }
     free(At);free(Bt);free(y);free(rk) ; return 0 ;
   }

   /* Unique-A/partial-A use a reduced y~B Freedman-Lane null; unique-B and
      partial-B analogously use y~A.  The common component is not a conditional
      added-variable effect, so it retains the complete neural-relabeling null. */
   for( cc=0 ; cc < THD_NCOMMON ; cc++ ) nge[cc]=0 ;
   rdm_commonality_fl(m,n,0,y,At,Bt,obs,pset,ws,nge,permnull) ;
   rdm_commonality_fl(m,n,1,y,At,Bt,obs,pset,ws,nge,permnull) ;

   for( pk=0 ; pk < np ; pk++ ){
     int *perm = pset->perm + (size_t)pk*n ; double comp[THD_NCOMMON] ;
     THD_simmat_to_tri_perm( neural , perm , y ) ;
     if( do_rank ) rdm_rank_inplace(m,y,rk) ;
     if( !rdm_zscore(m,y) )
       for( cc=0 ; cc < THD_NCOMMON ; cc++ ) comp[cc]=0.0 ;
     else rdm_commonality_one( m , y , At , Bt , comp ) ;
     if( fabs(comp[2]) >= fabs(obs[2]) ) nge[2]++ ;
     if( permnull != NULL ) permnull[(size_t)2*np+pk]=(float)fabs(comp[2]) ;
   }

   if( pval != NULL )
     for( cc=0 ; cc < THD_NCOMMON ; cc++ ){
       pval[cc] = (float)nge[cc] / (float)np ;      /* identity is slot 0 */
       if( zscr != NULL ) zscr[cc] = THD_p_to_z( pval[cc] , out[cc] ) ;
     }

   free(At);free(Bt);free(y);free(rk) ;
   return 0 ;
}

/*----------------------------------------------------------------------------*/
/*! Classic-RSA commonality with a synchronized condition-label null.  This is
    the group counterpart of THD_commonality: the conditional reduced fits are
    formed separately for every subject, but one condition permutation is used
    for every subject in a draw.  Averaging only after refitting preserves the
    subject as the independent sampling unit while avoiding the invalid sign
    flip of non-negative squared semipartial effects. */

int THD_classic_commonality( int nsub , int ncond , float *srdm ,
                             THD_simmat *A , THD_simmat *B , int cmp ,
                             PERM_set *cset , THD_rdm_ws *ws ,
                             float *out , float *pval , float *zscr ,
                             float *permnull , float *subout )
{
   int m=THD_NTRI(ncond), np=(cset!=NULL)?cset->nperm:0 ;
   int ss,cc,ii,pk,which,nge[THD_NCOMMON] ;
   float *At=NULL,*Bt=NULL,*y=NULL,*rk=NULL,*fit=NULL,*res=NULL,*nul=NULL ;
   double osum[THD_NCOMMON] ; int own_null=0,do_rank=(cmp==CMP_SPEARMAN) ;

   if( out != NULL ) for( cc=0 ; cc < THD_NCOMMON ; cc++ ) out[cc]=0.0f ;
   if( pval != NULL ) for( cc=0 ; cc < THD_NCOMMON ; cc++ ) pval[cc]=-1.0f ;
   if( zscr != NULL ) for( cc=0 ; cc < THD_NCOMMON ; cc++ ) zscr[cc]=0.0f ;
   if( nsub < 1 || ncond < 3 || srdm == NULL || A == NULL || B == NULL ||
       ws == NULL || out == NULL || A->n != ncond || B->n != ncond ||
       ws->n != ncond || ws->m < m ||
       (cmp != CMP_PEARSON && cmp != CMP_SPEARMAN) ||
       (cset != NULL && (cset->nobs != ncond || np < 1)) ) return 1 ;

   At=(float *)malloc(sizeof(float)*m) ; Bt=(float *)malloc(sizeof(float)*m) ;
   y =(float *)malloc(sizeof(float)*m) ; rk=(float *)malloc(sizeof(float)*m) ;
   fit=(float *)malloc(sizeof(float)*m) ; res=(float *)malloc(sizeof(float)*m) ;
   if( At==NULL || Bt==NULL || y==NULL || rk==NULL || fit==NULL || res==NULL ){
     free(At);free(Bt);free(y);free(rk);free(fit);free(res); return 1 ;
   }
   THD_simmat_to_tri(A,At) ; THD_simmat_to_tri(B,Bt) ;
   if( do_rank ){ rdm_rank_inplace(m,At,rk) ; rdm_rank_inplace(m,Bt,rk) ; }
   rdm_zscore(m,At) ; rdm_zscore(m,Bt) ;
   for( cc=0 ; cc < THD_NCOMMON ; cc++ ) osum[cc]=0.0 ;

   if( cset != NULL ){
     if( permnull != NULL ) nul=permnull ;
     else { nul=(float *)calloc((size_t)THD_NCOMMON*np,sizeof(float)); own_null=1; }
     if( nul == NULL ){ free(At);free(Bt);free(y);free(rk);free(fit);free(res); return 1; }
     memset(nul,0,sizeof(float)*(size_t)THD_NCOMMON*np) ;
   }

   for( ss=0 ; ss < nsub ; ss++ ){
     double obs[THD_NCOMMON] ; float *src=srdm+(size_t)ss*m ;
     memcpy(y,src,sizeof(float)*m) ;
     if( do_rank ) rdm_rank_inplace(m,y,rk) ;
     if( !rdm_zscore(m,y) ) for( cc=0 ; cc < THD_NCOMMON ; cc++ ) obs[cc]=0.0 ;
     else rdm_commonality_one(m,y,At,Bt,obs) ;
     for( cc=0 ; cc < THD_NCOMMON ; cc++ ){
       osum[cc] += obs[cc] ;
       if( subout != NULL ) subout[(size_t)cc*nsub+ss]=(float)obs[cc] ;
     }
     if( cset == NULL ) continue ;

     /* Conditional unique effects: relabel only the reduced-fit residual RDM.
        Models remain fixed, preserving their correlation in every draw. */
     for( which=0 ; which < 2 ; which++ ){
       float *red=(which==0)?Bt:At ; double den=0.0,gy=0.0,beta ;
       int raw=which,partial=which+3 ;
       for( ii=0 ; ii < m ; ii++ ){
         den += (double)red[ii]*red[ii] ; gy += (double)red[ii]*y[ii] ;
       }
       beta=(den>0.0)?gy/den:0.0 ;
       for( ii=0 ; ii < m ; ii++ ){
         fit[ii]=(float)(beta*red[ii]) ; res[ii]=y[ii]-fit[ii] ;
       }
       THD_tri_to_simmat(ncond,res,ws->ework) ;
       for( pk=0 ; pk < np ; pk++ ){
         double comp[THD_NCOMMON] ; int *perm=cset->perm+(size_t)pk*ncond ;
         THD_simmat_to_tri_perm(ws->ework,perm,ws->sc1) ;
         for( ii=0 ; ii < m ; ii++ ) ws->yperm[ii]=fit[ii]+ws->sc1[ii] ;
         rdm_commonality_one(m,ws->yperm,At,Bt,comp) ;
         nul[(size_t)raw*np+pk]     += (float)comp[raw] ;
         nul[(size_t)partial*np+pk] += (float)comp[partial] ;
       }
     }

     /* Shared/common variance is not an added-variable effect.  Its complete
        null relabels the prepared neural RDM by condition. */
     THD_tri_to_simmat(ncond,y,ws->ework) ;
     for( pk=0 ; pk < np ; pk++ ){
       double comp[THD_NCOMMON] ; int *perm=cset->perm+(size_t)pk*ncond ;
       THD_simmat_to_tri_perm(ws->ework,perm,ws->yperm) ;
       rdm_commonality_one(m,ws->yperm,At,Bt,comp) ;
       nul[(size_t)2*np+pk] += (float)comp[2] ;
     }
   }

   for( cc=0 ; cc < THD_NCOMMON ; cc++ ) out[cc]=(float)(osum[cc]/nsub) ;
   if( cset != NULL ){
     for( cc=0 ; cc < THD_NCOMMON ; cc++ ){
       float aobs=fabsf(out[cc]) ;
       float tie_tol=64.0f*FLT_EPSILON*(1.0f+aobs) ;
       nge[cc]=0 ;
       for( pk=0 ; pk < np ; pk++ ){
         float av=fabsf(nul[(size_t)cc*np+pk]/(float)nsub) ;
         /* Exact relabelings related by a model automorphism are genuine
            permutation ties.  Subject-wise float accumulation can otherwise
            place algebraically identical values on opposite sides of the
            observed threshold, making p depend on harmless roundoff. */
         if( fabsf(av-aobs) <= tie_tol ) av=aobs ;
         nul[(size_t)cc*np+pk]=av ;
       }
       /* Slot zero is the identity by PERM_set contract.  Assigning it from the
          observed group statistic prevents roundoff in two accumulation paths
          from ever dropping the mandatory identity exceedance. */
       nul[(size_t)cc*np]=aobs ;
       for( pk=0 ; pk < np ; pk++ )
         if( nul[(size_t)cc*np+pk] >= aobs ) nge[cc]++ ;
       if( pval != NULL ) pval[cc]=(float)nge[cc]/(float)np ;
       if( zscr != NULL && pval != NULL ) zscr[cc]=THD_p_to_z(pval[cc],out[cc]) ;
     }
   }

   if( own_null ) free(nul) ;
   free(At);free(Bt);free(y);free(rk);free(fit);free(res) ;
   return 0 ;
}

/*============================================================================*/
/*  Three-model commonality (F8)                                              */
/*============================================================================*/

static double rdm_r2_one_sums( double ss, double gy, double tss )
{
   return (tss>0.0 && ss>0.0) ? gy*gy/(tss*ss) : 0.0 ;
}

static double rdm_r2_two_sums( double aa, double bb, double ab,
                               double ga, double gb, double tss )
{
   double det=aa*bb-ab*ab ;
   if( tss<=0.0 ) return 0.0 ;
   if( det>1.0e-12*aa*bb )
     return ((bb*ga-ab*gb)*ga+(aa*gb-ab*ga)*gb)/(det*tss) ;
   { double ra=rdm_r2_one_sums(aa,ga,tss),rb=rdm_r2_one_sums(bb,gb,tss) ;
     return (ra>rb)?ra:rb ; }
}

/*! Seven exhaustive commonality coefficients plus three conditional partial
    R2 effects.  Inputs have already been ranked when requested and z-scored.
    All subset R2 and inclusion/exclusion arithmetic stays in double precision. */
static void rdm_commonality3_one( int m, float *y, float *A, float *B, float *C,
                                  double *comp )
{
   double aa=0.0,bb=0.0,cc=0.0,ab=0.0,ac=0.0,bc=0.0;
   double ga=0.0,gb=0.0,gc=0.0,tss=0.0,det,q;
   double ra,rb,rc,rab,rac,rbc,rabc,da,db,dc ; int ii ;
   for( ii=0 ; ii<m ; ii++ ){
     double a=A[ii],b=B[ii],c=C[ii],yy=y[ii] ;
     aa+=a*a ; bb+=b*b ; cc+=c*c ; ab+=a*b ; ac+=a*c ; bc+=b*c ;
     ga+=yy*a ; gb+=yy*b ; gc+=yy*c ; tss+=yy*yy ;
   }
   ra=rdm_r2_one_sums(aa,ga,tss) ; rb=rdm_r2_one_sums(bb,gb,tss) ;
   rc=rdm_r2_one_sums(cc,gc,tss) ;
   rab=rdm_r2_two_sums(aa,bb,ab,ga,gb,tss) ;
   rac=rdm_r2_two_sums(aa,cc,ac,ga,gc,tss) ;
   rbc=rdm_r2_two_sums(bb,cc,bc,gb,gc,tss) ;
   det=aa*(bb*cc-bc*bc)-ab*(ab*cc-ac*bc)+ac*(ab*bc-ac*bb) ;
   if( tss<=0.0 ) rabc=0.0 ;
   else if( det>1.0e-12*aa*bb*cc ){
     q=ga*ga*(bb*cc-bc*bc)+gb*gb*(aa*cc-ac*ac)+gc*gc*(aa*bb-ab*ab)
       +2.0*ga*gb*(ac*bc-ab*cc)+2.0*ga*gc*(ab*bc-ac*bb)
       +2.0*gb*gc*(ab*ac-aa*bc) ;
     rabc=q/(det*tss) ;
   } else {
     rabc=rab ; if( rac>rabc ) rabc=rac ; if( rbc>rabc ) rabc=rbc ;
   }
   comp[0]=rabc-rbc ;                              /* unique A | B,C */
   comp[1]=rabc-rac ;                              /* unique B | A,C */
   comp[2]=rabc-rab ;                              /* unique C | A,B */
   comp[3]=rac+rbc-rc-rabc ;                       /* AB, excluding C */
   comp[4]=rab+rbc-rb-rabc ;                       /* AC, excluding B */
   comp[5]=rab+rac-ra-rabc ;                       /* BC, excluding A */
   comp[6]=ra+rb+rc-rab-rac-rbc+rabc ;             /* shared A,B,C */
   da=1.0-rbc ; db=1.0-rac ; dc=1.0-rab ;
   comp[7]=(da>1.0e-12)?comp[0]/da:0.0 ;
   comp[8]=(db>1.0e-12)?comp[1]/db:0.0 ;
   comp[9]=(dc>1.0e-12)?comp[2]/dc:0.0 ;
}

static void rdm_reduced_fit2( int m, float *y, float *X1, float *X2,
                              float *fit, float *res )
{
   double aa=0.0,bb=0.0,ab=0.0,ga=0.0,gb=0.0,det,b1=0.0,b2=0.0 ; int ii ;
   for( ii=0 ; ii<m ; ii++ ){
     double a=X1[ii],b=X2[ii],yy=y[ii] ;
     aa+=a*a ; bb+=b*b ; ab+=a*b ; ga+=yy*a ; gb+=yy*b ;
   }
   det=aa*bb-ab*ab ;
   if( det>1.0e-12*aa*bb ){
     b1=(bb*ga-ab*gb)/det ; b2=(aa*gb-ab*ga)/det ;
   } else if( aa>0.0 || bb>0.0 ){
     if( bb<=0.0 || (aa>0.0 && ga*ga/aa>=gb*gb/bb) ) b1=ga/aa ;
     else b2=gb/bb ;
   }
   for( ii=0 ; ii<m ; ii++ ){
     fit[ii]=(float)(b1*X1[ii]+b2*X2[ii]) ; res[ii]=y[ii]-fit[ii] ;
   }
}

int THD_tri_commonality3( int m, float *yin, float *ain, float *bin, float *cin,
                          int cmp, THD_rdm_ws *ws, float *out )
{
   float *y=NULL,*A=NULL,*B=NULL,*C=NULL,*rk=NULL ; double co[THD_NCOMMON3] ;
   int k,do_rank=(cmp==CMP_SPEARMAN),bad=0 ;
   if( out ) for(k=0;k<THD_NCOMMON3;k++) out[k]=0.0f ;
   if( m<3 || !yin || !ain || !bin || !cin || !ws || m>ws->m || !out ||
       (cmp!=CMP_PEARSON && cmp!=CMP_SPEARMAN) ) return 1 ;
   y=(float *)malloc(sizeof(float)*m); A=(float *)malloc(sizeof(float)*m);
   B=(float *)malloc(sizeof(float)*m); C=(float *)malloc(sizeof(float)*m);
   rk=(float *)malloc(sizeof(float)*m) ;
   if(!y||!A||!B||!C||!rk){ bad=1; goto done_tri3; }
   memcpy(y,yin,sizeof(float)*m); memcpy(A,ain,sizeof(float)*m);
   memcpy(B,bin,sizeof(float)*m); memcpy(C,cin,sizeof(float)*m);
   if(do_rank){ rdm_rank_inplace(m,y,rk); rdm_rank_inplace(m,A,rk);
                rdm_rank_inplace(m,B,rk); rdm_rank_inplace(m,C,rk); }
   rdm_zscore(m,A); rdm_zscore(m,B); rdm_zscore(m,C) ;
   if(!rdm_zscore(m,y)) for(k=0;k<THD_NCOMMON3;k++) co[k]=0.0;
   else rdm_commonality3_one(m,y,A,B,C,co) ;
   for(k=0;k<THD_NCOMMON3;k++) out[k]=(float)co[k] ;
done_tri3:
   free(y);free(A);free(B);free(C);free(rk); return bad ;
}

int THD_commonality3( THD_simmat *neural, THD_simmat *Am, THD_simmat *Bm,
                      THD_simmat *Cm, int cmp, PERM_set *pset, THD_rdm_ws *ws,
                      float *out, float *pval, float *zscr, float *permnull )
{
   int m,n,np=(pset?pset->nperm:0),do_rank=(cmp==CMP_SPEARMAN),ii,k,pk,which;
   int nge[THD_NCOMMON3] ; float *y=NULL,*A=NULL,*B=NULL,*C=NULL,*rk=NULL;
   float *fit=NULL,*res=NULL,*nul=NULL ; double obs[THD_NCOMMON3] ; int bad=0,own=0 ;
   if(out)for(k=0;k<THD_NCOMMON3;k++)out[k]=0.0f;
   if(permnull&&np>0)memset(permnull,0,sizeof(float)*(size_t)THD_NCOMMON3*np);
   if(!neural||!Am||!Bm||!Cm||!ws||!out)return 1;
   n=neural->n;m=THD_NTRI(n);
   if(Am->n!=n||Bm->n!=n||Cm->n!=n||ws->n!=n||ws->m<m||
      (cmp!=CMP_PEARSON&&cmp!=CMP_SPEARMAN)||
      (pset&&(pset->nobs!=n||np<1)))return 1;
   y=(float *)malloc(sizeof(float)*m);A=(float *)malloc(sizeof(float)*m);
   B=(float *)malloc(sizeof(float)*m);C=(float *)malloc(sizeof(float)*m);
   rk=(float *)malloc(sizeof(float)*m);fit=(float *)malloc(sizeof(float)*m);
   res=(float *)malloc(sizeof(float)*m);
   if(!y||!A||!B||!C||!rk||!fit||!res){bad=1;goto done_is3;}
   THD_simmat_to_tri(Am,A);THD_simmat_to_tri(Bm,B);THD_simmat_to_tri(Cm,C);
   if(do_rank){rdm_rank_inplace(m,A,rk);rdm_rank_inplace(m,B,rk);rdm_rank_inplace(m,C,rk);}
   rdm_zscore(m,A);rdm_zscore(m,B);rdm_zscore(m,C);
   THD_simmat_to_tri(neural,y);if(do_rank)rdm_rank_inplace(m,y,rk);
   if(!rdm_zscore(m,y))for(k=0;k<THD_NCOMMON3;k++)obs[k]=0.0;
   else rdm_commonality3_one(m,y,A,B,C,obs);
   for(k=0;k<THD_NCOMMON3;k++){out[k]=(float)obs[k];nge[k]=0;}
   if(!pset){if(pval)for(k=0;k<THD_NCOMMON3;k++){pval[k]=-1.0f;if(zscr)zscr[k]=0.0f;}goto done_is3;}
   nul=permnull ; if(!nul){nul=(float *)calloc((size_t)THD_NCOMMON3*np,sizeof(float));own=1;}
   if(!nul){bad=1;goto done_is3;}
   for(which=0;which<3;which++){
     float *x1=(which==0)?B:A,*x2=(which==2)?B:C; int partial=7+which;
     rdm_reduced_fit2(m,y,x1,x2,fit,res);THD_tri_to_simmat(n,res,ws->ework);
     for(pk=0;pk<np;pk++){
       double co[THD_NCOMMON3];int *perm=pset->perm+(size_t)pk*n;
       THD_simmat_to_tri_perm(ws->ework,perm,ws->sc1);
       for(ii=0;ii<m;ii++)ws->yfit[ii]=fit[ii]+ws->sc1[ii];
       rdm_commonality3_one(m,ws->yfit,A,B,C,co);
       nul[(size_t)which*np+pk]=(float)fabs(co[which]);
       nul[(size_t)partial*np+pk]=(float)fabs(co[partial]);
     }
   }
   for(pk=0;pk<np;pk++){
     double co[THD_NCOMMON3];int *perm=pset->perm+(size_t)pk*n;
     THD_simmat_to_tri_perm(neural,perm,ws->yperm);
     if(do_rank)rdm_rank_inplace(m,ws->yperm,rk);
     if(!rdm_zscore(m,ws->yperm))for(k=0;k<THD_NCOMMON3;k++)co[k]=0.0;
     else rdm_commonality3_one(m,ws->yperm,A,B,C,co);
     for(k=3;k<=6;k++)nul[(size_t)k*np+pk]=(float)fabs(co[k]);
   }
   for(k=0;k<THD_NCOMMON3;k++){
     float ao=fabsf(out[k]),tol=64.0f*FLT_EPSILON*(1.0f+ao);nge[k]=0;
     for(pk=0;pk<np;pk++){
       float av=nul[(size_t)k*np+pk];if(fabsf(av-ao)<=tol)av=ao;
       nul[(size_t)k*np+pk]=av;
     }
     nul[(size_t)k*np]=ao;
     for(pk=0;pk<np;pk++)if(nul[(size_t)k*np+pk]>=ao)nge[k]++;
     if(pval){pval[k]=(float)nge[k]/np;if(zscr)zscr[k]=THD_p_to_z(pval[k],out[k]);}
   }
done_is3:
   if( own ) free(nul) ;
   free(y);free(A);free(B);free(C);free(rk);free(fit);free(res) ;
   return bad ;
}

int THD_classic_commonality3( int nsub,int ncond,float *srdm,THD_simmat *Am,
                              THD_simmat *Bm,THD_simmat *Cm,int cmp,
                              PERM_set *cset,THD_rdm_ws *ws,float *out,
                              float *pval,float *zscr,float *permnull,float *subout )
{
   int m=THD_NTRI(ncond),np=cset?cset->nperm:0,do_rank=(cmp==CMP_SPEARMAN);
   int ss,k,ii,pk,which,nge[THD_NCOMMON3],own=0,bad=0;double sum[THD_NCOMMON3];
   float *A=NULL,*B=NULL,*C=NULL,*y=NULL,*rk=NULL,*fit=NULL,*res=NULL,*nul=NULL;
   if(out)for(k=0;k<THD_NCOMMON3;k++)out[k]=0.0f;
   if(pval)for(k=0;k<THD_NCOMMON3;k++)pval[k]=-1.0f;
   if(zscr)for(k=0;k<THD_NCOMMON3;k++)zscr[k]=0.0f;
   if(nsub<1||ncond<3||!srdm||!Am||!Bm||!Cm||!ws||!out||Am->n!=ncond||
      Bm->n!=ncond||Cm->n!=ncond||ws->n!=ncond||ws->m<m||
      (cmp!=CMP_PEARSON&&cmp!=CMP_SPEARMAN)||
      (cset&&(cset->nobs!=ncond||np<1)))return 1;
   A=(float *)malloc(sizeof(float)*m);B=(float *)malloc(sizeof(float)*m);
   C=(float *)malloc(sizeof(float)*m);y=(float *)malloc(sizeof(float)*m);
   rk=(float *)malloc(sizeof(float)*m);fit=(float *)malloc(sizeof(float)*m);
   res=(float *)malloc(sizeof(float)*m);
   if(!A||!B||!C||!y||!rk||!fit||!res){bad=1;goto done_cl3;}
   THD_simmat_to_tri(Am,A);THD_simmat_to_tri(Bm,B);THD_simmat_to_tri(Cm,C);
   if(do_rank){rdm_rank_inplace(m,A,rk);rdm_rank_inplace(m,B,rk);rdm_rank_inplace(m,C,rk);}
   rdm_zscore(m,A);rdm_zscore(m,B);rdm_zscore(m,C);
   for(k=0;k<THD_NCOMMON3;k++)sum[k]=0.0;
   if(cset){nul=permnull;if(!nul){nul=(float *)calloc((size_t)THD_NCOMMON3*np,sizeof(float));own=1;}
     if(!nul){bad=1;goto done_cl3;}memset(nul,0,sizeof(float)*(size_t)THD_NCOMMON3*np);}
   for(ss=0;ss<nsub;ss++){
     double obs[THD_NCOMMON3];float *src=srdm+(size_t)ss*m;
     memcpy(y,src,sizeof(float)*m);if(do_rank)rdm_rank_inplace(m,y,rk);
     if(!rdm_zscore(m,y))for(k=0;k<THD_NCOMMON3;k++)obs[k]=0.0;
     else rdm_commonality3_one(m,y,A,B,C,obs);
     for(k=0;k<THD_NCOMMON3;k++){sum[k]+=obs[k];if(subout)subout[(size_t)k*nsub+ss]=(float)obs[k];}
     if(!cset)continue;
     for(which=0;which<3;which++){
       float *x1=(which==0)?B:A,*x2=(which==2)?B:C;int partial=7+which;
       rdm_reduced_fit2(m,y,x1,x2,fit,res);THD_tri_to_simmat(ncond,res,ws->ework);
       for(pk=0;pk<np;pk++){
         double co[THD_NCOMMON3];int *perm=cset->perm+(size_t)pk*ncond;
         THD_simmat_to_tri_perm(ws->ework,perm,ws->sc1);
         for(ii=0;ii<m;ii++)ws->yperm[ii]=fit[ii]+ws->sc1[ii];
         rdm_commonality3_one(m,ws->yperm,A,B,C,co);
         nul[(size_t)which*np+pk]+=(float)co[which];
         nul[(size_t)partial*np+pk]+=(float)co[partial];
       }
     }
     THD_tri_to_simmat(ncond,y,ws->ework);
     for(pk=0;pk<np;pk++){
       double co[THD_NCOMMON3];int *perm=cset->perm+(size_t)pk*ncond;
       THD_simmat_to_tri_perm(ws->ework,perm,ws->yperm);
       rdm_commonality3_one(m,ws->yperm,A,B,C,co);
       for(k=3;k<=6;k++)nul[(size_t)k*np+pk]+=(float)co[k];
     }
   }
   for(k=0;k<THD_NCOMMON3;k++)out[k]=(float)(sum[k]/nsub);
   if(cset)for(k=0;k<THD_NCOMMON3;k++){
     float ao=fabsf(out[k]),tol=64.0f*FLT_EPSILON*(1.0f+ao);nge[k]=0;
     for(pk=0;pk<np;pk++){float av=fabsf(nul[(size_t)k*np+pk]/nsub);
       if( fabsf(av-ao)<=tol ) av=ao ;
       nul[(size_t)k*np+pk]=av ;
     }
     nul[(size_t)k*np]=ao;for(pk=0;pk<np;pk++)if(nul[(size_t)k*np+pk]>=ao)nge[k]++;
     if( pval ) pval[k]=(float)nge[k]/np ;
     if( zscr && pval ) zscr[k]=THD_p_to_z(pval[k],out[k]) ;
   }
done_cl3:
   if( own ) free(nul) ;
   free(A);free(B);free(C);free(y);free(rk);free(fit);free(res) ;
   return bad ;
}
