#include "mrilib.h"

/*------------------------------------------------------------
  Set the one-sided tail probability at which we will cutoff
  the unusuality test.
--------------------------------------------------------------*/

static float zstar = 0.0 ;            /* the actual cutoff */
static float pstar = 0.0 ;            /* tail probability  */

void set_unusuality_tail( float p )
{
   if( p > 0.0 && p < 1.0 ){
      zstar = qginv(p) ;
      pstar = p ;
   }
   return ;
}

/*------------------------------------------------------------
  Inputs: rr[0..nr-1] = array of correlation coefficients
                        (will not be altered)
  Outputs: *up = unusuality index from positive tail of rr[]
           *um = unusuality index from negative tail of rr[]
--------------------------------------------------------------*/

#undef  NEAR1
#undef  NEARM1
#undef  NRBOT
#define NEAR1   0.999
#define NEARM1 -0.999
#define NRBOT   999

void unusuality( int nr , float * rr , float * up , float * um )
{
   int ii,jj , nzero , mzero ;
   float zmid,zsig,zmed, uval, fac, zrat, ff,fp, ss,ds,pp,ee , sigma ;
   float rmid , rcut , zsigt ;

   static int    nzz=0 ;       /* workspace array and its size */
   static float * zz=NULL ;
   float * aa ;

   if( up == NULL || um == NULL ) return ;                /* bad inputs */
   if( nr < NRBOT || rr == NULL ){ *up=*um=0.0; return; } /* bad inputs */

   /*-- make workspace, if needed --*/

   if( nzz < 2*nr ){
      if( zz != NULL ) free(zz) ;
      nzz = 2*nr ;
      zz = (float *) malloc(sizeof(float)*nzz) ;
   }
   aa = zz + nr ;  /* second half */

   /*-- set cutoff tail, if needed --*/

   if( zstar <= 0.0 ){
      char * cp = getenv("UTAIL") ;
      float pp = 0.01 ;                      /* default */
      if( cp != NULL ){
         float xx = strtod( cp , NULL ) ;
         if( xx > 0.0 && xx < 1.0 ) pp = xx ;
      }
      set_unusuality_tail( pp ) ;
   }

   /*-- copy data into workspace, eliding values near 1 --*/

   for( ii=jj=0 ; ii < nr ; ii++ )
      if( rr[ii] <= NEAR1 && rr[ii] >= NEARM1 ) zz[jj++] = rr[ii] ;

   nr = jj ;
   if( nr < NRBOT ){ *up=*um=0.0; return; }  /* shouldn't happen */

   /*-- find median of atanh(zz) --*/

   rmid = qmed_float( nr , zz ) ;    /* median of correlations */
   zmid = atanh(rmid) ;              /* median of atanh(zz)   */

   /*-- find MAD of atanh(zz) = median{fabs(atanh(zz)-zmid)} --*/
   /*   [be tricky -> use the subtraction formula for tanh]    */
   /*   [tanh(atanh(zz)-zmid) = (zz-rmid)/(1-zz*rmid), and]    */
   /*   [since tanh/atanh are monotonic increasing,  atanh]    */
   /*   [of median{fabs(tanh(atanh(zz)-zmid))} is the same]    */
   /*   [as median{fabs(atanh(zz)-zmid)}.                 ]    */

   for( ii=0 ; ii < nr ; ii++ )
      aa[ii] = fabs( (zz[ii]-rmid)/(1.0-zz[ii]*rmid) ) ;

   zmed = qmed_float( nr , aa ) ;  /* median of aa    */
   zmed = atanh(zmed) ;            /* MAD of atanh(zz) */

   zsig = 1.4826 * zmed ;          /* estimate standard deviation of zz */
                                   /* 1/1.4826 = sqrt(2)*erfinv(0.5)    */

   if( zsig <= 0.0 ){ *up=*um=0.0; return; }  /* shouldn't happen */

#undef  SQRT_2PI
#define SQRT_2PI 2.5066283                        /* sqrt(2*pi) */

#undef  PHI
#define PHI(s) (1.0-0.5*normal_t2p(ss))           /* N(0,1) cdf */

   /****************************************/
   /*** Compute positive tail unusuality ***/

   fac = 1.0 / zsig ;

   /* Find values >= zstar, compute sum of squares */

   rcut  = tanh( zsig * zstar + zmid ) ;  /* (atanh(zz)-zmid)/zsig >= zstar */
   zsigt = 0.0 ;
   for( mzero=ii=0 ; ii < nr ; ii++ ){
      if( zz[ii] >= rcut ){
         ff     = fac * ( atanh(zz[ii]) - zmid ) ; /* normalized Fisher */
         zsigt += ff*ff ;                          /* sum of squares   */
         mzero++ ;                                 /* how many we get */
      }
   }
   nzero = nr - mzero ;

   /* if we don't have decent data, output is 0 */

   if( nzero < 2 || mzero < MAX(1.0,pstar*nr) ){  /* too weird for words */
      *up = 0.0 ;
   } else {                                       /* have decent data here */
      zsigt = zsigt / mzero ;                     /* sigma-tilde squared */

      /* set up to compute f(s) */

      zrat = zstar*zstar / zsigt ;
      fac  = ( zrat * nzero ) / ( SQRT_2PI * mzero ) ;
      ss   = zstar ;          /* initial guess for s = zstar/sigma */

      /* Newton's method [almost] */

      for( ii=0 ; ii < 5 ; ii++ ){
         pp = PHI(ss) ;                              /* Phi(ss) \approx 1 */
         ee = exp(-0.5*ss*ss) ;
         ff = ss*ss - (fac/pp) * ss * ee - zrat ;    /* f(s) */
         fp = 2.0*ss + (fac/pp) * ee * (ss*ss-1.0) ; /* f'(s) */
         ds = ff / fp ;                              /* Newton step */
         ss -= ds ;                                  /* update */
      }

      sigma = zstar / ss ;                           /* estimate of sigma */
                                                     /* from upper tail data */

      if( sigma <= 1.0 ){                            /* the boring case */
         *up = 0.0 ;
      } else {

      /* compute the log-likelihood difference */

         uval =  nzero * log( PHI(ss)/(1.0-pstar) )
               - mzero * ( log(sigma) + 0.5 * zsigt * (1.0/(sigma*sigma)-1.0) ) ;

         *up = uval ;
      }
   }

   /****************************************/
   /*** Compute negative tail unusuality ***/

   fac = 1.0 / zsig ;

   /* Find values <= -zstar, compute sum of squares */

   rcut  = tanh( zmid - zsig * zstar ) ;  /* (atanh(zz)-zmid)/zsig <= -zstar */
   zsigt = 0.0 ;
   for( mzero=ii=0 ; ii < nr ; ii++ ){
      if( zz[ii] <= rcut ){
         ff     = fac * ( atanh(zz[ii]) - zmid ) ; /* normalized Fisher */
         zsigt += ff*ff ;                          /* sum of squares   */
         mzero++ ;                                 /* how many we get */
      }
   }
   nzero = nr - mzero ;

   /* if we don't have decent data, output is 0 */

   if( nzero < 2 || mzero < MAX(1.0,pstar*nr) ){  /* too weird for words */
      *um = 0.0 ;
   } else {                                       /* have decent data here */
      zsigt = zsigt / mzero ;                     /* sigma-tilde squared */

      /* set up to compute f(s) */

      zrat = zstar*zstar / zsigt ;
      fac  = ( zrat * nzero ) / ( SQRT_2PI * mzero ) ;
      ss   = zstar ;          /* initial guess for s = zstar/sigma */

      /* Newton's method [almost] */

      for( ii=0 ; ii < 5 ; ii++ ){
         pp = PHI(ss) ;                              /* Phi(ss) \approx 1 */
         ee = exp(-0.5*ss*ss) ;
         ff = ss*ss - (fac/pp) * ss * ee - zrat ;    /* f(s) */
         fp = 2.0*ss + (fac/pp) * ee * (ss*ss-1.0) ; /* f'(s) */
         ds = ff / fp ;                              /* Newton step */
         ss -= ds ;                                  /* update */
      }

      sigma = zstar / ss ;                           /* estimate of sigma */
                                                     /* from upper tail data */

      if( sigma <= 1.0 ){                            /* the boring case */
         *um = 0.0 ;
      } else {

      /* compute the log-likelihood difference */

         uval =  nzero * log( PHI(ss)/(1.0-pstar) )
               - mzero * ( log(sigma) + 0.5 * zsigt * (1.0/(sigma*sigma)-1.0) ) ;

         *um = uval ;
      }
   }

   /*-- done! --*/

   return ;
}

/***************************************************************************/

static int nt   = 0 ; /* length of vectors [bitvec and float] */
static int nfv  = 0 ; /* number of float vectors */
static int nlev = 2 ; /* default number of levels in bitvecs */

typedef struct {
  byte  * bv ;     /* bitvector    [nt]  */
  float * dp ;     /* dot products [nfv] */

  float up , um , ubest ;  /* unusualities */
  int nlev ;               /* # levels */
} bitvec ;

#define bv_free(b) \
   do{ if((b)!=NULL){free((b)->bv);free((b)->dp);free((b));} }while(0)

typedef struct {
      int num , nall ;
      bitvec ** bvarr ;
} bvarr ;

static bvarr *  bvar = NULL ;
static float ** fvar = NULL ;  /* nfv of these */

#define BITVEC_IN_BVARR(name,nn) ((name)->bvarr[(nn)])
#define BVARR_SUB                BITVEC_IN_BVARR
#define BVARR_COUNT(name)        ((name)->num)

#define INC_BVARR 32

#define INIT_BVARR(name)                                                           \
   do{ int iq ; (name) = (bvarr *) malloc(sizeof(bvarr)) ;                         \
       (name)->num = 0 ; (name)->nall = INC_BVARR ;                                \
       (name)->bvarr = (bitvec **)malloc(sizeof(bitvec *)*INC_BVARR) ;             \
       for( iq=(name)->num ; iq < (name)->nall ; iq++ ) (name)->bvarr[iq] = NULL ; \
       break ; } while(0)

#define ADDTO_BVARR(name,imm)                                                           \
   do{ int nn , iq ;                                                                    \
       if( (name)->num == (name)->nall ){                                               \
          nn = (name)->nall = 1.1*(name)->nall + INC_BVARR ;                            \
          (name)->bvarr = realloc( (name)->bvarr,sizeof(bitvec *)*nn );                 \
          for( iq=(name)->num ; iq < (name)->nall ; iq++ ) (name)->bvarr[iq] = NULL ; } \
       nn = (name)->num ; ((name)->num)++ ;                                             \
       (name)->bvarr[nn] = (imm) ; break ; } while(0)

#define FREE_BVARR(name)                                                        \
   do{ if( (name) != NULL ){                                                    \
          free((name)->bvarr); free((name)); (name) = NULL; } break; } while(0)

#define DESTROY_BVARR(name)                                                     \
   do{ int nn ;                                                                 \
       if( (name) != NULL ){                                                    \
          for( nn=0 ; nn < (name)->num ; nn++ ) bv_free((name)->bvarr[nn]) ;    \
          free((name)->bvarr); free((name)); (name) = NULL; } break; } while(0)

#define TRUNCATE_BVARR(name,qq)                                                 \
   do{ int nn ;                                                                 \
       if( (name) != NULL && qq < (name)->num ){                                \
          for( nn=qq ; nn < (name)->num ; nn++ ) bv_free((name)->bvarr[nn]);    \
          (name)->num = qq ;                                                    \
       } } while(0)

/***************************************************************************/

int equal_bitvector_piece( bitvec * b , bitvec * c , int aa , int bb )
{
   int ii ; byte * bv=b->bv , * cv=c->bv ;

   if( aa <  0  ) aa = 0 ;
   if( bb >= nt ) bb = nt-1 ;
   for( ii=aa ; ii <= bb ; ii++ ) if( bv[ii] != cv[ii] ) return 0 ;
   return 1;
}

int equal_bitvector( bitvec * b , bitvec * c )
{
   return equal_bitvector_piece( b , c , 0 , nt-1 ) ;
}

/*--------------------------------------------------------------------------*/

void randomize_bitvector_piece( bitvec * b , int aa , int bb )
{
   int ii ; byte * bv=b->bv ;

   if( aa <  0  ) aa = 0 ;
   if( bb >= nt ) bb = nt-1 ;
   for( ii=aa ; ii <= bb ; ii++ ) bv[ii] = (byte)( b->nlev*drand48() ) ;
   return ;
}

void randomize_bitvector( bitvec * b )
{
   randomize_bitvector_piece( b , 0 , nt-1 ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void zero_bitvector_piece( bitvec * b , int aa , int bb )
{
   int ii ; byte * bv=b->bv ;

   if( aa <  0  ) aa = 0 ;
   if( bb >= nt ) bb = nt-1 ;
   for( ii=aa ; ii <= bb ; ii++ ) bv[ii] = 0 ;
   return ;
}

void zero_bitvector( bitvec * b )
{
   zero_bitvector_piece( b , 0 , nt-1 ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void fix_bitvector_piece( bitvec * b , int aa , int bb , int val )
{
   int ii ; byte * bv=b->bv , vv=(byte)val ;

   if( aa <  0  ) aa = 0 ;
   if( bb >= nt ) bb = nt-1 ;
   for( ii=aa ; ii <= bb ; ii++ ) bv[ii] = vv ;
   return ;
}

void fix_bitvector( bitvec * b , int val )
{
   fix_bitvector_piece( b , 0 , nt-1 , val ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void invert_bitvector_piece( bitvec * b , int aa , int bb )
{
   int ii,nl=b->nlev-1 ; byte * bv=b->bv ;

   if( aa <  0  ) aa = 0 ;
   if( bb >= nt ) bb = nt-1 ;
   for( ii=aa ; ii <= bb ; ii++ ) bv[ii] = nl - bv[ii] ;
   return ;
}

void invert_bitvector( bitvec * b )
{
   invert_bitvector_piece( b , 0 , nt-1 ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

bitvec * new_bitvector(void)
{
   bitvec * b ;
   b     = (bitvec *) malloc(sizeof(bitvec)   ) ;
   b->bv = (byte *)   malloc(sizeof(byte) *nt ) ;
   b->dp = (float *)  malloc(sizeof(float)*nfv) ;

   b->nlev = nlev ;
   return b ;
}

bitvec * copy_bitvector( bitvec * b )
{
   int ii ;
   bitvec * c = new_bitvector() ;
   memcpy( c->bv , b->bv , sizeof(byte)*nt   ) ;
#if 0
   memcpy( c->dp , b->dp , sizeof(float)*nfv ) ;
   c->up = b->up ; c->um = b->um ; c->ubest = b->ubest ;
#endif
   c->nlev = b->nlev ;
   return c ;
}

/*--------------------------------------------------------------------------*/

int count_bitvector( bitvec * b )
{
   int ii,ss ;
   byte * bv = b->bv ;
   for( ii=ss=0 ; ii < nt ; ii++ ) if( bv[ii] ) ss++ ;
   return ss ;
}

/*--------------------------------------------------------------------------*/

#define LINEAR_DETREND

void normalize_floatvector( float * far )
{
   int ii ;
   float ff,gg ;

#ifdef LINEAR_DETREND
   THD_linear_detrend( nt , far , NULL , NULL ) ;               /* remove trend */
   for( ff=0.0,ii=0 ; ii < nt ; ii++ ) ff += far[ii]*far[ii] ;  /* and normalize */
   if( ff <= 0.0 ) return ;
   ff = 1.0 / sqrt(ff) ;
   for( ii=0 ; ii < nt ; ii++ ) far[ii] *= ff ;
#else
   for( ff=0.0,ii=0 ; ii < nt ; ii++ ) ff += far[ii] ;
   ff /= nt ;
   for( gg=0.0,ii=0 ; ii < nt ; ii++ ) gg += SQR( (far[ii]-ff) ) ;
   if( gg <= 0.0 ) return ;
   gg = 1.0 / sqrt(gg) ;
   for( ii=0 ; ii < nt ; ii++ ) far[ii] = (far[ii]-ff)*gg ;
#endif
   return ;
}

/*--------------------------------------------------------------------------*/

float corr_floatbit( float * far , bitvec * b )
{
   int    ii , ns ;
   float  ss , bb,bq ;
   byte * bar=b->bv ;

   if( b->nlev == 2 ){                               /* binary case */
      for( ss=0.0,ns=ii=0 ; ii < nt ; ii++ )
         if( bar[ii] ){ ns++ ; ss += far[ii] ; }
      if( ns == 0 || ns == nt ) return 0.0 ;
      ss *= sqrt( ((float) nt) / (float)(ns*(nt-ns)) ) ;

   } else {                                         /* multilevel case */
      for( ss=bb=bq=0.0,ii=0 ; ii < nt ; ii++ ){
         ss += bar[ii]*far[ii] ;
         bb += bar[ii] ; bq += bar[ii]*bar[ii] ;
      }
      bq -= bb*bb/nt ; if( bq <= 0.0 ) return 0.0 ;
      ss /= sqrt(bq) ;
   }

   return ss ;
}

/*--------------------------------------------------------------------------*/

void evaluate_bitvec( bitvec * bim )
{
   int jj ;

   for( jj=0 ; jj < nfv ; jj++ )
      bim->dp[jj] = corr_floatbit( fvar[jj] , bim ) ;

   unusuality( nfv , bim->dp , &(bim->up) , &(bim->um) ) ;

   if( bim->up < bim->um ){
      float tt ;
      invert_bitvector( bim ) ;
      tt = bim->um ; bim->um = bim->up ; bim->up = tt ;
   }

   bim->ubest = bim->up ;
   return ;
}

/*--------------------------------------------------------------------------*/

#define DERR(s) fprintf(stderr,"** %s\n",(s))

void init_floatvector_array( char * dname , char * mname )
{
   THD_3dim_dataset * dset ;
   byte * mask = NULL ;
   int ii,jj , nvox ;
   MRI_IMAGE * im ;

   dset = THD_open_dataset( dname ) ;
   if( dset == NULL ){ DERR("can't open dataset"); return; }
   nt = DSET_NVALS(dset) ;
   if( nt < 20 ){ DSET_delete(dset); DERR("dataset too short"); return; }
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){ DSET_delete(dset); DERR("can't load dataset"); return; }
   nvox = DSET_NVOX(dset) ;

   if( mname != NULL ){
      THD_3dim_dataset * mset ;
      mset = THD_open_dataset( mname ) ;
      if( mset == NULL ){ DSET_delete(dset); DERR("can't open mask"); return; }
      if( DSET_NVOX(mset) != nvox ){
         DSET_delete(mset); DSET_delete(dset); DERR("mask size mismatch"); return;
      }
      mask = THD_makemask( mset , 0 , 1.0,0.0 ) ;
      DSET_delete(mset) ;
      if( mask == NULL ){ DSET_delete(dset); DERR("mask is empty"); return; }
      nfv = THD_countmask( nvox , mask ) ;
      if( nfv < nt ){ DSET_delete(dset); DERR("mask is too small"); return; }
   } else {
      nfv = nvox ;
   }

   fvar = (float **) malloc(sizeof(float *)*nfv) ;
   for( jj=ii=0 ; ii < nvox ; ii++ ){
      if( mask != NULL && mask[ii] == 0 ) continue ; /* skip */

      im = THD_extract_series( ii , dset , 0 ) ;
      fvar[jj] = MRI_FLOAT_PTR(im) ;
      normalize_floatvector( fvar[jj] ) ;
      mri_fix_data_pointer(NULL,im) ; mri_free(im) ; jj++ ;
   }

   if( mask != NULL ) free(mask) ;
   DSET_delete(dset) ; return ;
}

/*--------------------------------------------------------------------------*/

void init_bitvector_array( int nbv )
{
   bitvec * bim ;
   int ii , jj ;
   byte * bar ;

   INIT_BVARR(bvar) ;

   for( ii=0 ; ii < nbv ; ii++ ){
      bim = new_bitvector() ;
      randomize_bitvector( bim ) ;
      evaluate_bitvec( bim ) ;
      ADDTO_BVARR(bvar,bim) ;
   }

   return ;
}

/*--------------------------------------------------------------------------*/

#define IRAN(j) (lrand48() % (j))

#define PROMO_MAX 4

static int promo_ok = 0 ;

void evolve_bitvector_array(void)
{
   static int    nrvec=-1 ;
   static float * rvec=NULL ;

   int ii , nbv,nbv1 , aa,bb,vv , qbv , kup ;
   bitvec * bim , * qim ;
   float aup,aum ;

   /* promote a few to a higher plane of being */

   nbv1=nbv = BVARR_COUNT(bvar) ;

   if( promo_ok ){
   for( aa=ii=0 ; ii < nbv ; ii++ )
      if( BVARR_SUB(bvar,ii)->nlev > nlev ) aa++ ;

   if( aa < nbv/4 ){
      for( kup=ii=0 ; ii < nbv && kup < PROMO_MAX ; ii++ ){

         bim = BVARR_SUB(bvar,ii) ;
         if( bim->nlev > nlev ) continue ; /* skip */

         qim = copy_bitvector(bim) ;
         qim->nlev *= 2 ;
         for( aa=0 ; aa < nt ; aa++ )
            if( qim->bv[aa] < nlev-1 ) qim->bv[aa] *= 2 ;
            else                       qim->bv[aa]  = 2*nlev-1 ;
         evaluate_bitvec( qim ) ;
         ADDTO_BVARR(bvar,qim) ;
         kup++ ;
      }
      fprintf(stderr,"%d PROMO up\n",kup) ;
   } else if( aa > 3*nbv/4 ){
      for( kup=ii=0 ; ii < nbv && kup < PROMO_MAX ; ii++ ){

         bim = BVARR_SUB(bvar,ii) ;
         if( bim->nlev == nlev ) continue ; /* skip */

         qim = copy_bitvector(bim) ;
         qim->nlev /= 2 ;
         for( aa=0 ; aa < nt ; aa++ ) qim->bv[aa] /= 2 ;
         evaluate_bitvec( qim ) ;
         ADDTO_BVARR(bvar,qim) ;
         kup++ ;
      }
      fprintf(stderr,"%d PROMO down\n",kup) ;
    }
   }
   /* create mutants */

   qim = copy_bitvector(BVARR_SUB(bvar,0)) ;  /* add copy of first one */
   evaluate_bitvec( qim ) ;
   ADDTO_BVARR(bvar,qim) ;

   nbv = BVARR_COUNT(bvar) ;

   for( ii=0 ; ii < nbv ; ii++ ){

      bim = BVARR_SUB(bvar,ii) ;

      aa = IRAN(nt) ; bb = aa + IRAN(5) ; if( bb >= nt ) bb = nt-1 ;

      qim = copy_bitvector(bim) ;
      zero_bitvector_piece( qim , aa , bb ) ;
      if( equal_bitvector_piece(bim,qim,aa,bb) ){
         bv_free(qim) ;
      } else {
         evaluate_bitvec( qim ) ;
         ADDTO_BVARR(bvar,qim) ;
      }

      vv  = (bim->nlev == 2) ? 1 : 1+IRAN(bim->nlev-1) ;
      qim = copy_bitvector(bim) ;
      fix_bitvector_piece( qim , aa , bb , vv ) ;
      if( equal_bitvector_piece(bim,qim,aa,bb) ){
         bv_free(qim) ;
      } else {
         evaluate_bitvec( qim ) ;
         ADDTO_BVARR(bvar,qim) ;
      }

      qim = copy_bitvector(bim) ;
      randomize_bitvector_piece( qim , aa , bb ) ;
      if( equal_bitvector_piece(bim,qim,aa,bb) ){
         bv_free(qim) ;
      } else {
         evaluate_bitvec( qim ) ;
         ADDTO_BVARR(bvar,qim) ;
      }

      qim = copy_bitvector(bim) ;
      invert_bitvector_piece( qim , aa , bb ) ;
      if( equal_bitvector_piece(bim,qim,aa,bb) ){
         bv_free(qim) ;
      } else {
         evaluate_bitvec( qim ) ;
         ADDTO_BVARR(bvar,qim) ;
      }
   }

   /* sort everybody */

   qbv = BVARR_COUNT(bvar) ;
   if( nrvec < qbv ){
      if( rvec != NULL ) free(rvec) ;
      rvec = (float *) malloc(sizeof(float)*qbv) ;
      nrvec = qbv ;
   }
   for( ii=0 ; ii < qbv ; ii++ )
      rvec[ii] = - BVARR_SUB(bvar,ii)->ubest ;

   qsort_floatstuff( qbv , rvec , (void **) bvar->bvarr ) ;

   TRUNCATE_BVARR( bvar , nbv1 ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int ii , nv , nite=0 , neq=0 , nopt , nbv=64 ;
   float fold , fnew ;
   char * mname=NULL , * dname ;
   bitvec * bim ;

   if( argc < 2 ){printf("Usage: unu [-mask mset] [-lev n] [-nbv n] dset > bname.1D\n");exit(0);}

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcmp(argv[nopt],"-mask") == 0 ){
         mname = argv[++nopt] ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-lev") == 0 ){
         nlev = (int)strtod(argv[++nopt],NULL) ;
         if( nlev < 2 || nlev > 8 ){ DERR("bad -nlev"); exit(1); }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-nbv") == 0 ){
         nbv = (int)strtod(argv[++nopt],NULL) ;
         if( nbv < 8 || nbv > 999 ){ DERR("bad -nbv"); exit(1); }
         nopt++ ; continue ;
      }

      fprintf(stderr,"** Illegal option %s\n",argv[nopt]); exit(1);
   }
   if( nopt >= argc ){fprintf(stderr,"** No dataset!?\n");exit(1);}

   dname = argv[nopt] ;

   init_floatvector_array( dname , mname ) ;
   if( fvar == NULL ){
      fprintf(stderr,"** Couldn't init floatvector!?\n") ; exit(1) ;
   } else {
      fprintf(stderr,"nt=%d  nfv=%d\n",nt,nfv) ;
   }

   srand48((long)time(NULL)) ;

   init_bitvector_array( nbv ) ;
   fold = BVARR_SUB(bvar,0)->ubest ;
   fprintf(stderr,"fold = %7.4f\n",fold) ;

   while(1){
      evolve_bitvector_array() ;
      nv = BVARR_COUNT(bvar) ;
      nite++ ;
#if 1
      fprintf(stderr,"---nite=%d\n",nite) ;
      for( nopt=ii=0 ; ii < nv ; ii++ ){
         fprintf(stderr," %7.4f[%d]",BVARR_SUB(bvar,ii)->ubest,BVARR_SUB(bvar,ii)->nlev) ;
         if( BVARR_SUB(bvar,ii)->nlev > nlev ) nopt++ ;
      }
      fprintf(stderr," *%d\n",nopt) ;
#endif

      fnew = fabs(BVARR_SUB(bvar,0)->ubest) ;
      if( fnew <= fold ){
         neq++ ;
         if( neq == 8 &&  promo_ok ) break ;
         if( neq == 8 && !promo_ok ){ promo_ok = 1 ; neq = 0 ; }
      } else {
         neq  = 0 ;
         fold = fnew ;
         fprintf(stderr,"%d: %7.4f\n",nite,fold) ;
      }
   }

   bim = BVARR_SUB(bvar,0) ;
   for( ii=0 ; ii < nt ; ii++ )
      printf(" %d\n",(int)bim->bv[ii]) ;

   exit(0) ;
}
