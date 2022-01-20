#include "mrilib.h"

/***---------------------------------------------------------***/
/*** This file is intended to be #include-d into mri_nwarp.c ***/
/***---------------------------------------------------------***/

/***
     #ifdef USE_OMP
     #include <omp.h>
     #endif
***/

#ifndef USE_OMP
static int omp_get_thread_num(void) { return 0 ; }
static int omp_get_max_threads(void){ return 1 ; }
#endif

typedef struct {
  int meth ;
} INCOR_generic ;

typedef struct {
  int meth ;
  int npt ;
  double sx , sxx , sy , syy , sxy , sw ;
} INCOR_pearson ;

typedef struct {
  int meth ;
  int npt ;
  double sx , sxx , sy , syy , sxy , sw ;
  double xcbot , xctop , ycbot , yctop ;
  double xdbot , xdtop , ydbot , ydtop ;
} INCOR_pearclp ;

typedef struct {
  int meth ;
  int nbin ;
  float *xc , *yc , *xyc , nww ;
  float xxbot , xxtop , yybot , yytop ;
  float xcbot , xctop , ycbot , yctop ;
} INCOR_2Dhist ;

typedef struct {
  int numblok , *nelm ;
  int nx,ny,nz, *blkn ;
  int *nsum ; float *sx,*sxx, *sy,*syy, *sxy,*sw , *pval ;
  float psum , wsum ; int ninserted ;
} INCORR_BLOK_set ;     /* 25 Jun 2014 */

typedef struct {
  int meth ;
  int iibot,iitop , jjbot,jjtop , kkbot,kktop ;
  int nii         , njj         , nkk         ;
  INCORR_BLOK_set *ibs ;
} INCOR_localpearson ;  /* 25 Jun 2014 */

#define ALLOW_DEBUG_LPC
#ifdef  ALLOW_DEBUG_LPC
static int debug_lpc = 0 ;
#else
#define debug_lpc 0
#endif

#undef  INCOR_methcode
#define INCOR_methcode(vp) ( ((vp) != NULL) ? ((INCOR_generic *)vp)->meth : 0 )

#undef  MYatanh
#define MYatanh(x) ( ((x)<-0.9993293) ? -4.0                \
                    :((x)>+0.9993293) ? +4.0 : atanh(x) )

/****************************************************************************/
/*** Local Pearson correlation [25 Jun 2014 -- at long last!]             ***/
/****************************************************************************/

#undef  MINCOR
#define MINCOR 9 /* min number of points to correlate */

#undef  CMAX
#define CMAX 0.99f

/* is abs(a) <= s ?? */

#undef  FAS
#define FAS(a,s) ( (a) <= (s) && (a) >= -(s) )

/** define inside of a ball; is point (a,b,c) inside?  **/
/** volume of ball = 4*PI/3 * siz**3 = 4.1888 * siz**3 **/

#define IB_BLOK_inside_ball(a,b,c,siz) \
  ( ((a)*(a)+(b)*(b)+(c)*(c)) <= (siz) )

/** define inside of a cube **/
/** volume of cube = 8 * siz**3 **/
/** lattice vectors = [2*siz,0,0]  [0,2*siz,0]  [0,0,2*siz] **/

#define IB_BLOK_inside_cube(a,b,c,siz) \
  ( FAS((a),(siz)) && FAS((b),(siz)) && FAS((c),(siz)) )

/** define inside of a rhombic dodecahedron (RHDD) **/
/** volume of RHDD = 2 * siz**3 **/
/** lattice vectors = [siz,siz,0]  [0,siz,siz]  [siz,0,siz] **/

#define IB_BLOK_inside_rhdd(a,b,c,siz)              \
  ( FAS((a)+(b),(siz)) && FAS((a)-(b),(siz)) &&     \
    FAS((a)+(c),(siz)) && FAS((a)-(c),(siz)) &&     \
    FAS((b)+(c),(siz)) && FAS((b)-(c),(siz))   )

/** define inside of a truncated octahedron (TOHD) **/
/** volume of TOHD = 4 * siz**3 **/
/** lattice vectors = [-siz,siz,siz]  [siz,-siz,siz]  [siz,siz,-siz] **/

#define IB_BLOK_inside_tohd(a,b,c,siz)                              \
  ( FAS((a),(siz)) && FAS((b),(siz)) && FAS((c),(siz))         &&   \
    FAS((a)+(b)+(c),1.5f*(siz)) && FAS((a)-(b)+(c),1.5f*(siz)) &&   \
    FAS((a)+(b)-(c),1.5f*(siz)) && FAS((a)-(b)-(c),1.5f*(siz))   )

/** define inside of an arbitrary blok type **/

#define IB_BLOK_inside(bt,a,b,c,s)                              \
 (  ((bt)==GA_BLOK_BALL) ? IB_BLOK_inside_ball((a),(b),(c),(s)) \
  : ((bt)==GA_BLOK_CUBE) ? IB_BLOK_inside_cube((a),(b),(c),(s)) \
  : ((bt)==GA_BLOK_RHDD) ? IB_BLOK_inside_rhdd((a),(b),(c),(s)) \
  : ((bt)==GA_BLOK_TOHD) ? IB_BLOK_inside_tohd((a),(b),(c),(s)) \
  : 0 )

/** add 1 value to a dynamically allocated integer array **/

#define IB_BLOK_ADDTO_intar(nar,nal,ar,val)                                 \
 do{ if( (nar) == (nal) ){                                                  \
       (nal) = 1.5*(nal)+16; (ar) = (int *)realloc((ar),sizeof(int)*(nal)); \
     }                                                                      \
     (ar)[(nar)++] = (val);                                                 \
 } while(0)

/** truncate dynamically allocated integer array down to size **/

#define IB_BLOK_CLIP_intar(nar,nal,ar)                               \
 do{ if( (nar) < (nal) && (nar) > 0 ){                               \
       (nal) = (nar); (ar) = (int *)realloc((ar),sizeof(int)*(nal)); \
 }} while(0)

/*----------------------------------------------------------------------------*/
/*! Fill a struct with list of points contained in sub-bloks of the base.

    - nx,ny,nz = 3D grid dimensions
    - dx,dy,dz = 3D grid spacings
    - mask     = byte mask of points to use (can be NULL)
    - bloktype = one of GA_BLOK_BALL, GA_BLOK_CUBE, GA_BLOK_RHDD, GA_BLOK_TOHD
    - blokrad  = radius parameter for the bloks to be built
    - minel    = minimum number of points to put in a blok
                 (if 0, function will pick a value)
*//*--------------------------------------------------------------------------*/

INCORR_BLOK_set * create_INCORR_BLOK_set(
                     int   nx , int   ny , int   nz ,
                     float dx , float dy , float dz ,
                     byte *mask ,
                     int bloktype , float blokrad , int minel )
{
   INCORR_BLOK_set *ibs ;
   float dxp,dyp,dzp , dxq,dyq,dzq , dxr,dyr,dzr , xt,yt,zt ;
   float xx,yy,zz , uu,vv,ww , siz ;
   THD_mat33 latmat , invlatmat ; THD_fvec3 pqr , xyz ;
   int pb,pt , qb,qt , rb,rt , pp,qq,rr , tnblok,nball , ii , nxy,nxyz ;
   int aa,bb,cc , dd,ss , np,nq,nr,npq , *nelm,*nalm,**elm ;
   int ntot,nsav,nblok , sgood , maxel ;
   int *ilist ;
   const float shfac = 1.0f ;

ENTRY("create_INCORR_BLOK_set") ;

   if( nx < 3 || ny < 3 || nz < 1 ) RETURN(NULL) ;
   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;

   /* Create lattice vectors to generate translated bloks:
      The (p,q,r)-th blok -- for integral p,q,r -- is at (x,y,z) offset
        (dxp,dyp,dzp)*p + (dxq,dyq,dzq)*q + (dxr,dyr,dzr)*r
      Also set the 'siz' parameter for the blok, to test for inclusion. */

   switch( bloktype ){

     /* balls go on a hexagonal close packed lattice,
        but with lattice spacing reduced to avoid gaps
        (of course, then the balls overlap -- c'est la geometrie) */

     case GA_BLOK_BALL:{
       float s3=1.73205f ,           /* sqrt(3) */
             s6=2.44949f ,           /* sqrt(6) */
             a =blokrad*0.866025f ;  /* shrink spacing to avoid gaps */
       siz = blokrad*blokrad ;
       /* hexagonal close packing basis vectors for sphere of radius a */
       a *= shfac ;
       dxp = 2.0f * a ; dyp = 0.0f  ; dzp = 0.0f             ;
       dxq = a        ; dyq = a * s3; dzq = 0.0f             ;
       dxr = a        ; dyr = a / s3; dzr = a * 0.666667f*s6 ;
     }
     break ;

     /* cubes go on a simple cubical lattice, spaced so faces touch */

     case GA_BLOK_CUBE:{
       float a =  blokrad ;
       siz = a ; a *= shfac ;
       dxp = 2*a ; dyp = 0.0f; dzp = 0.0f ;
       dxq = 0.0f; dyq = 2*a ; dzq = 0.0f ;
       dxr = 0.0f; dyr = 0.0f; dzr = 2*a  ;
     }
     break ;

     /* rhombic dodecahedra go on a FCC lattice,
        spaced so that faces touch (i.e., no volumetric overlap) */

     case GA_BLOK_RHDD:{
       float a = blokrad ;
       siz = a ; a *= shfac ;
       dxp = a   ; dyp = a   ; dzp = 0.0f ;
       dxq = 0.0f; dyq = a   ; dzq = a    ;
       dxr = a   ; dyr = 0.0f; dzr = a    ;
     }
     break ;

     /* truncated octahedra go on a BCC lattice,
        spaced so that faces touch (i.e., no volumetric overlap) */

     case GA_BLOK_TOHD:{
       float a = blokrad ;
       siz = a ; a *= shfac ;
       dxp = -a ; dyp =  a ; dzp =  a ;
       dxq =  a ; dyq = -a ; dzq =  a ;
       dxr =  a ; dyr =  a ; dzr = -a ;
     }
     break ;

     default:  RETURN(NULL) ;  /** should not happen! **/
   }

   /* find range of (p,q,r) indexes needed to cover volume,
      by checking out all 7 corners besides (0,0,0) (where p=q=r=0) */

   LOAD_MAT( latmat, dxp , dxq , dxr ,
                     dyp , dyq , dyr ,
                     dzp , dzq , dzr  ) ; invlatmat = MAT_INV(latmat) ;

   xt = (nx-1)*dx ; yt = (ny-1)*dy ; zt = (nz-1)*dz ;
   pb = pt = qb = qt = rb = rt = 0 ;  /* initialize (p,q,r) bot, top values */

   LOAD_FVEC3(xyz , xt,0.0f,0.0f ); pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   LOAD_FVEC3(xyz , xt,yt,0.0f )  ; pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   LOAD_FVEC3(xyz , xt,0.0f,zt )  ; pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   LOAD_FVEC3(xyz , xt,yt,zt )    ; pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   LOAD_FVEC3(xyz , 0.0f,yt,0.0f ); pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   LOAD_FVEC3(xyz , 0.0f,0.0f,zt ); pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   LOAD_FVEC3(xyz , 0.0f,yt,zt )  ; pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   /* Lattice index range is (p,q,r) = (pb..pt,qb..qt,rb..rt) inclusive */

   np = pt-pb+1 ;                /* number of p values to consider */
   nq = qt-qb+1 ; npq = np*nq ;
   nr = rt-rb+1 ;
   tnblok = npq*nr ;             /* total number of bloks to consider */

   /* Now have list of bloks, so put points into each blok list */

   nelm = (int *) calloc(sizeof(int)  ,tnblok) ;  /* # pts in each blok */
   nalm = (int *) calloc(sizeof(int)  ,tnblok) ;  /* # malloc-ed in each blok */
   elm  = (int **)calloc(sizeof(int *),tnblok) ;  /* list of pts in each blok */

   nxy = nx*ny ; nxyz = nxy*nz ;

   /* macro to test if (xx,yy,zz) goes into blok (ta,tb,tc), and put it there */

#undef  TEST_BLOK_xyz
#define TEST_BLOK_xyz(ta,tb,tc)                                      \
 do{ LOAD_FVEC3( pqr , ta,tb,tc ) ;                                  \
     xyz = MATVEC( latmat , pqr ) ;                                  \
     uu = xx-xyz.xyz[0] ; vv = yy-xyz.xyz[1] ; ww = zz-xyz.xyz[2] ;  \
     if( IB_BLOK_inside( bloktype , uu,vv,ww , siz ) ){              \
       dd = (ta-pb) + (tb-qb)*np + (tc-rb)*npq ; /* blok index */    \
       IB_BLOK_ADDTO_intar( nelm[dd], nalm[dd], elm[dd], ss ) ;      \
       ntot++ ; sgood = 1 ;                                          \
     }                                                               \
 } while(0) ;

   /* loop over points to insert; each point goes in (at most) ONE blok */

   for( ntot=ii=0 ; ii < nxyz ; ii++ ){
     if( mask != NULL && !mask[ii] ) continue ;       /* bad point */
     pp = ii%nx ; rr = ii/nxy ; qq = (ii-rr*nxy)/nx ; /* 3D indexes (pp,qq,rr) */
     sgood = 0 ; ss = ii ;                            /* index in 1D array */
     xx = pp*dx ; yy = qq*dy ; zz = rr*dz ;           /* xyz spatial coordinates */
     LOAD_FVEC3( xyz , xx,yy,zz ) ;
     pqr = MATVEC( invlatmat , xyz ) ;                /* float lattice coords */
     pp = (int)floorf(pqr.xyz[0]+.499f) ;             /* integer lattice coords */
     qq = (int)floorf(pqr.xyz[1]+.499f) ;
     rr = (int)floorf(pqr.xyz[2]+.499f) ;
     /* test if this blok holds the current point */
     if( pp >= pb && pp <= pt && qq >= qb && qq <= qt && rr >= rb && rr <= rt ){
       TEST_BLOK_xyz(pp,qq,rr) ; if( sgood ) goto TEST_BLOK_DONE ;
     }
     /* otherwise, search nearby bloks for inclusion of (xx,yy,zz) */
     for( cc=rr-1 ; cc <= rr+1 ; cc++ ){
       if( cc < rb || cc > rt ) continue ;
       for( bb=qq-1 ; bb <= qq+1 ; bb++ ){
         if( bb < qb || bb > qt ) continue ;
         for( aa=pp-1 ; aa <= pp+1 ; aa++ ){
           if( aa < pb || aa > pt ) continue ;
           if( aa==pp && bb==qq && cc==rr ) continue ;  /* already tested */
           TEST_BLOK_xyz(aa,bb,cc) ; if( sgood ) goto TEST_BLOK_DONE ;
         }
       }
     }
     TEST_BLOK_DONE: /*nada*/ ;
   }
#undef TEST_BLOK_xyz

   /* compute the min number of points allowed per blok? */

   if( minel < MINCOR ){
     for( minel=dd=0 ; dd < tnblok ; dd++ ) minel = MAX(minel,nelm[dd]) ;
     minel = (int)(0.321f*minel)+1 ; if( minel < MINCOR ) minel = MINCOR ;
   }
   for( maxel=dd=0 ; dd < tnblok ; dd++ ) maxel = MAX(maxel,nelm[dd]) ;

   /* now cast out bloks that have too few points */

   for( ntot=nblok=dd=0 ; dd < tnblok ; dd++ ){
     if( nelm[dd] < minel ){
       if( elm[dd] != NULL ){ free(elm[dd]); elm[dd] = NULL; }
       nelm[dd] = 0 ;      /* mark as an unsaved blok */
     } else {
       nblok++ ;           /* count of saved bloks */
       ntot += nelm[dd] ;  /* count of saved points */
     }
   }
   free(nalm) ;

   if( nblok == 0 ){  /* didn't find any arrays to keep!? */
     ERROR_message("create_INCORR_BLOK_set can't find bloks with at least %d nodes",minel);
     free(nelm) ; free(elm) ; RETURN(NULL) ;
   }

   /* make a map from point index to (saved) blok index */

   ilist = (int *)malloc(sizeof(int)*nxyz) ;
   for( pp=0 ; pp < nxyz ; pp++ ) ilist[pp] = -666 ;  /* mark as not in any blok */
   for( rr=qq=dd=0 ; dd < tnblok ; dd++ ){
     if( nelm[dd] == 0 ) continue ;  /* not a saved blok */
     for( pp=0 ; pp < nelm[dd] ; pp++ ) ilist[ elm[dd][pp] ] = rr ;
     free(elm[dd]) ;  /* no longer needed */
     rr++ ;           /* increment saved blok index */
   }

   /* create output struct */

   ibs = (INCORR_BLOK_set *)malloc(sizeof(INCORR_BLOK_set)) ;
   ibs->numblok  = nblok ;
   ibs->nelm     = (int *)  calloc(sizeof(int)  ,nblok) ;
   ibs->nsum     = (int *)  calloc(sizeof(int)  ,nblok) ;
   ibs->sx       = (float *)calloc(sizeof(float),nblok) ;
   ibs->sxx      = (float *)calloc(sizeof(float),nblok) ;
   ibs->sy       = (float *)calloc(sizeof(float),nblok) ;
   ibs->syy      = (float *)calloc(sizeof(float),nblok) ;
   ibs->sxy      = (float *)calloc(sizeof(float),nblok) ;
   ibs->sw       = (float *)calloc(sizeof(float),nblok) ;
   ibs->pval     = (float *)calloc(sizeof(float),nblok) ;

   ibs->nx = nx ; ibs->ny = ny ; ibs->nz = nz ; ibs->blkn = ilist ;
   ibs->psum = ibs->wsum = 0.0f ; ibs->ninserted = 0 ;

   for( rr=dd=0 ; dd < tnblok ; dd++ ){
     if( nelm[dd] > 0 ) ibs->nelm[rr++] = nelm[dd] ;
   }

   free(nelm) ; free(elm) ;

   RETURN(ibs) ;
}

/*---------------------------------------------------------------------------*/

void destroy_INCORR_BLOK_set( INCORR_BLOK_set *ibs )
{
   if( ibs == NULL ) return ;

   if( ibs->nelm != NULL ) free(ibs->nelm) ;
   if( ibs->blkn != NULL ) free(ibs->blkn) ;
   if( ibs->nsum != NULL ) free(ibs->nsum) ;
   if( ibs->sx   != NULL ) free(ibs->sx  ) ;
   if( ibs->sxx  != NULL ) free(ibs->sxx ) ;
   if( ibs->sy   != NULL ) free(ibs->sy  ) ;
   if( ibs->syy  != NULL ) free(ibs->syy ) ;
   if( ibs->sxy  != NULL ) free(ibs->sxy ) ;
   if( ibs->sw   != NULL ) free(ibs->sw  ) ;
   if( ibs->pval != NULL ) free(ibs->pval) ;
   free(ibs) ;                                            \
   return ;
}

/*---------------------------------------------------------------------------*/

void clear_INCORR_BLOK_set( INCORR_BLOK_set *ibs )
{
   if( ibs == NULL ) return ;

   AAmemset(ibs->nsum,0,sizeof(int)  *ibs->numblok) ;
   AAmemset(ibs->sx  ,0,sizeof(float)*ibs->numblok) ;
   AAmemset(ibs->sxx ,0,sizeof(float)*ibs->numblok) ;
   AAmemset(ibs->sy  ,0,sizeof(float)*ibs->numblok) ;
   AAmemset(ibs->syy ,0,sizeof(float)*ibs->numblok) ;
   AAmemset(ibs->sxy ,0,sizeof(float)*ibs->numblok) ;
   AAmemset(ibs->sw  ,0,sizeof(float)*ibs->numblok) ;
   AAmemset(ibs->pval,0,sizeof(float)*ibs->numblok) ;
   ibs->psum = ibs->wsum = 0.0f ; ibs->ninserted = 0 ;
   return ;
}

/*---------------------------------------------------------------------------*/
/* Insert data into the struct. */

void addto_INCORR_BLOK_set( INCORR_BLOK_set *ibs ,
                            int ibot , int itop ,  /* indexes are inclusive */
                            int jbot , int jtop ,
                            int kbot , int ktop ,
                            float *xval , float *yval , float *wt )
{
   int ii,jj,kk , nx,ny,nz,nxy , pp,dd , nblok=ibs->numblok ;
   int *nelm=ibs->nelm , *nsum=ibs->nsum , *blkn=ibs->blkn ;
   float *sx   = ibs->sx  , *sxx  = ibs->sxx , *sy   = ibs->sy  ,
         *syy  = ibs->syy , *sxy  = ibs->sxy , *sw   = ibs->sw  ,
         *pval = ibs->pval ;
   double xx,yy,xy,ww ; float rval ;

   nx = ibs->nx ; ny = ibs->ny ; nz = ibs->nz ; nxy = nx*ny ;

   if( wt == NULL ){
     for( pp=0,kk=kbot ; kk <= ktop ; kk++ ){
      for( jj=jbot ; jj <= jtop ; jj++ ){
       for( ii=ibot ; ii <= itop ; ii++,pp++ ){
         dd = blkn[ii + jj*nx + kk*nxy] ;
         if( dd >= 0 && nsum[dd] < nelm[dd] ){  /* in incomplete blok? */
           xx = (double)xval[pp] ; yy = (double)yval[pp] ;
           sx[dd] += xx ; sxx[dd] += xx*xx ;
           sy[dd] += yy ; syy[dd] += yy*yy ; sxy[dd] += xx*yy ; sw[dd] += 1.0 ;
           nsum[dd]++ ; ibs->ninserted++ ;
         }
     }}}
   } else {
     for( pp=0,kk=kbot ; kk <= ktop ; kk++ ){
      for( jj=jbot ; jj <= jtop ; jj++ ){
       for( ii=ibot ; ii <= itop ; ii++,pp++ ){
         dd = blkn[ii + jj*nx + kk*nxy] ;
         if( dd >= 0 && nsum[dd] < nelm[dd] ){  /* in incomplete blok? */
           ww = wt[pp] ; if( ww <= 0.0 ) continue ;      /* no weight? */
           xx = (double)xval[pp] ; yy = (double)yval[pp] ;
           sx[dd] += xx*ww; sxx[dd] += xx*xx*ww;
           sy[dd] += yy*ww; syy[dd] += yy*yy*ww; sxy[dd] += xx*yy*ww; sw[dd] += ww;
           nsum[dd]++ ; ibs->ninserted++ ;
         }
     }}}
   }

   /* find newly completed bloks and sum them up */

   for( dd=0 ; dd < nblok ; dd++ ){
     if( nsum[dd] == nelm[dd] && sw[dd] > 0.0f ){ /* now completed? */
       ww = 1.0 / sw[dd] ;
       xx = sxx[dd] - sx[dd] * sx[dd] * ww ;
       yy = syy[dd] - sy[dd] * sy[dd] * ww ;
       xy = sxy[dd] - sx[dd] * sy[dd] * ww ;
       if( xx <= 0.0 || yy <= 0.0 ){
         pval[dd] = 0.0f ;
       } else {
         rval = (float)(xy/sqrt(xx*yy)) ;
              if( rval >  CMAX ) rval =  CMAX ;
         else if( rval < -CMAX ) rval = -CMAX ;
         pval[dd] = logf( (1.0f+rval)/(1.0f-rval) ) ;
         ibs->psum += (float)sw[dd] * pval[dd] * fabsf(pval[dd]) ;
       }
       ibs->wsum += (float)sw[dd] ;
       sw[dd] = 0.0f ;  /* marked as completed and summed up */
     }
   }

   return ;
}

/*---------------------------------------------------------------------------*/
/* Compute correlation, including the extra data, if any
   (but this extra data is not stored into the struct).
   The intended usage is
     (1) setup struct with unchanging data with calls to addto_INCORR_BLOK_set
     (2) finalize the correlation with calls to correlate_INCORR_BLOK_set
         with data that changes -- from the warp patch currently being
         optimized, that is
*//*-------------------------------------------------------------------------*/

float correlate_INCORR_BLOK_set( INCORR_BLOK_set *ibs ,
                                 int ibot , int itop ,
                                 int jbot , int jtop ,
                                 int kbot , int ktop ,
                                 float *xval , float *yval , float *wt )
{
   int dd , nblok=ibs->numblok ;
   int *nelm=ibs->nelm , *nsum=NULL ;
   float *sx , *sxx , *sy , *syy , *sxy , *sw ;
   double xx,yy,xy,ww ; float rval , wsum,psum ;

   if( xval == NULL || yval == NULL ){  /* no extra data to add in */

     sx   = ibs->sx   ;  /* pointers to external struct data */
     sxx  = ibs->sxx  ;  /* this data will not be altered herein */
     sy   = ibs->sy   ;
     syy  = ibs->syy  ;
     sxy  = ibs->sxy  ;
     sw   = ibs->sw   ;

   } else {    /* add the extra data into a local copy of the struct's data */

     int *blkn = ibs->blkn ;  /* spatial index to blok index array */
     int nx,ny,nz,nxy , ii,jj,kk , pp ;

     nx = ibs->nx ; ny = ibs->ny ; nz = ibs->nz ; nxy = nx*ny ;

     sx   = (float *)calloc(sizeof(float),nblok) ;  /* local copies */
     sxx  = (float *)calloc(sizeof(float),nblok) ;  /* to be thrown */
     sy   = (float *)calloc(sizeof(float),nblok) ;  /* away at end */
     syy  = (float *)calloc(sizeof(float),nblok) ;
     sxy  = (float *)calloc(sizeof(float),nblok) ;
     sw   = (float *)calloc(sizeof(float),nblok) ;
     nsum = (int *  )calloc(sizeof(int)  ,nblok) ;
     if( ibs->ninserted > 0 ){
       AAmemcpy(sxx ,ibs->sxx ,sizeof(float)*nblok) ;
       AAmemcpy(sy  ,ibs->sy  ,sizeof(float)*nblok) ;
       AAmemcpy(syy ,ibs->syy ,sizeof(float)*nblok) ;
       AAmemcpy(sxy ,ibs->sxy ,sizeof(float)*nblok) ;
       AAmemcpy(sw  ,ibs->sw  ,sizeof(float)*nblok) ;
       AAmemcpy(nsum,ibs->nsum,sizeof(int)  *nblok) ;
     }

     if( debug_lpc )
       fprintf(stderr,"++++++++++ Debug LPC output: %d bloks  inserting x=%d..%d y=%d..%d z=%d..%d  wt=%p\n",
                      nblok,ibot,itop,jbot,jtop,kbot,ktop,(void *)wt) ;

     if( wt == NULL ){
       for( pp=0,kk=kbot ; kk <= ktop ; kk++ ){
        for( jj=jbot ; jj <= jtop ; jj++ ){
         for( ii=ibot ; ii <= itop ; ii++,pp++ ){
           dd = blkn[ii + jj*nx + kk*nxy] ;
           if( dd >= 0 && nsum[dd] < nelm[dd] ){  /* in incomplete blok? */
             xx = (double)xval[pp] ; yy = (double)yval[pp] ;
             sx[dd] += xx ; sxx[dd] += xx*xx ;
             sy[dd] += yy ; syy[dd] += yy*yy ; sxy[dd] += xx*yy ; sw[dd] += 1.0 ;
             nsum[dd]++ ;
           }
       }}}
     } else {
       for( pp=0,kk=kbot ; kk <= ktop ; kk++ ){
        for( jj=jbot ; jj <= jtop ; jj++ ){
         for( ii=ibot ; ii <= itop ; ii++,pp++ ){
           dd = blkn[ii + jj*nx + kk*nxy] ;
           if( dd >= 0 && nsum[dd] < nelm[dd] ){  /* in incomplete blok? */
             ww = wt[pp] ; if( ww <= 0.0 ) continue ;      /* no weight? */
             xx = (double)xval[pp] ; yy = (double)yval[pp] ;
             sx[dd] += xx*ww; sxx[dd] += xx*xx*ww;
             sy[dd] += yy*ww; syy[dd] += yy*yy*ww; sxy[dd] += xx*yy*ww; sw[dd] += ww;
             nsum[dd]++ ;
           }
       }}}
     }

     if( debug_lpc )
       fprintf(stderr,"  inserted %d points in temp copies\n",pp) ;

   }  /* end of putting data into local copy */

   /*-- now finalize the summation --*/

   psum = ibs->psum ; wsum = ibs->wsum ;

   if( debug_lpc )
     fprintf(stderr,"++++++++++ Debug LPC output: %d bloks  init psum=%g wsum=%g\n  pvals:",nblok,psum,wsum) ;

   for( dd=0 ; dd < nblok ; dd++ ){  /* scan and finish incomplete bloks */
     if( sw[dd] > 0.0f ){            /* incomplete but with data? */
       ww = 1.0 / sw[dd] ;
       xx = sxx[dd] - sx[dd] * sx[dd] * ww ;
       yy = syy[dd] - sy[dd] * sy[dd] * ww ;
       xy = sxy[dd] - sx[dd] * sy[dd] * ww ;
       if( xx > 0.0 && yy > 0.0 ){
         rval = (float)(xy/sqrt(xx*yy)) ;
              if( rval >  CMAX ) rval =  CMAX ;
         else if( rval < -CMAX ) rval = -CMAX ;
         rval  = logf( (1.0f+rval)/(1.0f-rval) ) ;
         psum += (float)sw[dd] * rval * fabsf(rval) ;
         if( debug_lpc ) fprintf(stderr," %g",rval) ;
       } else if( debug_lpc ){
         fprintf(stderr," {sx=%g sxx=%g sy=%g syy=%g sxy=%g sw=%g xx=%g yy=%g xy=%g}",
                 sx[dd],sxx[dd],sy[dd],syy[dd],sxy[dd],sw[dd],xx,yy,xy) ;
       }
       wsum += (float)sw[dd] ;
     } else if( debug_lpc ){
       fprintf(stderr," %g",ibs->pval[dd]) ;
     }
   }

   if( sx != ibs->sx ){  /* free local copies */
     free(sx); free(sxx); free(sy); free(syy); free(sxy); free(sw); free(nsum);
   }

   rval = (wsum <= 0.0f) ? 0.0f : psum/wsum ;

   if( debug_lpc )
     fprintf(stderr,"\n--------- final psum=%g wsum=%g rval=%g ----------------------------\n",psum,wsum,rval) ;

   return rval ;
}

/****************************************************************************/
/*** Histogram-based measurements of dependence between two float arrays. ***/
/****************************************************************************/

#undef  WW
#define WW(i) ((w==NULL) ? 1.0f : w[i])   /* weight function for i'th datum */

#undef  XYC
#define XYC(p,q) xyc[(p)+(q)*nbp]

#ifndef WAY_BIG
#  define WAY_BIG 1.e+10
#endif

#undef  GOODVAL
#define GOODVAL(x) ((x) < WAY_BIG)                 /* x is not preposterous */

#undef  RANGVAL
#define RANGVAL(x,b,t) ((x) >= (b) && (x) <= (t))  /* x between b and t */

/*--------------------------------------------------------------------------*/

float_pair INCOR_clipate( int nval , float *xar )
{
   MRI_IMAGE *qim; float cbot,ctop, mmm , *qar; float_pair rr; int ii,nq;

ENTRY("INCOR_clipate") ;

   qim = mri_new_vol( nval,1,1 , MRI_float ) ; qar = MRI_FLOAT_PTR(qim) ;
   for( ii=nq=0 ; ii < nval ; ii++ ) if( GOODVAL(xar[ii]) ) qar[nq++] = xar[ii];
   qim->nx = qim->nvox = nq ;
   if( nq < 666 ){ rr.a = 1.0f; rr.b = 0.0f; mri_free(qim); RETURN(rr); }
   mmm  = mri_min( qim ) ;
   if( mmm >= 0.0f ){   /* for positive images */
     cbot = THD_cliplevel( qim , 0.321f ) ;
     ctop = mri_quantile ( qim , 0.987f ) ;
     if( ctop > 6.543f*cbot ) ctop = 6.543f*cbot ;
   } else {  /* for images including negative values: no go */
     cbot = 1.0f; ctop = 0.0f;
   }
   mri_free(qim) ;
   rr.a = cbot ; rr.b = ctop ; RETURN(rr) ;
}

/*--------------------------------------------------------------------------*/

float_quad INCOR_2Dhist_xyclip( int nval , float *xval , float *yval )
{
   float_pair xcc , ycc ; float_quad xxyycc={0.0f,0.0f,0.0f,0.0f} ;

ENTRY("INCOR_2Dhist_xyclip") ;

   if( nval < 666 || xval == NULL || yval == NULL ) RETURN(xxyycc) ;

   xcc = INCOR_clipate( nval , xval ) ;
   ycc = INCOR_clipate( nval , yval ) ;

   if( xcc.a >= xcc.b || ycc.a >= ycc.b ) RETURN(xxyycc) ;

   xxyycc.a = xcc.a ; xxyycc.b = xcc.b ;
   xxyycc.c = ycc.a ; xxyycc.d = ycc.b ; RETURN(xxyycc) ;
}

/*--------------------------------------------------------------------------*/

float_quad INCOR_2Dhist_minmax( int nval , float *xval , float *yval )
{
   float_quad xxyy={0.0f,0.0f,0.0f,0.0f} ;
   int ii ; float xb,xt,yb,yt ;

ENTRY("INCOR_2Dhist_minmax") ;

   if( nval < 1 || xval == NULL || yval == NULL ) RETURN(xxyy) ;

   xb = xt = xval[0] ; yb = yt = yval[0] ;
   for( ii=1 ; ii < nval ; ii++ ){
          if( xval[ii] < xb ) xb = xval[ii] ;
     else if( xval[ii] > xt ) xt = xval[ii] ;
          if( yval[ii] < yb ) yb = yval[ii] ;
     else if( yval[ii] > yt ) yt = yval[ii] ;
   }
   xxyy.a = xb ; xxyy.b = xt ; xxyy.c = yb ; xxyy.d = yt ; RETURN(xxyy) ;
}

/*--------------------------------------------------------------------------*/

static byte *good=NULL ;
static int  agood=0 ;

void INCOR_setup_good( int ng )
{
   if( ng <= 0 ){
     if( good != NULL ){ free(good); good = NULL; }
     agood = 0 ;
   } else if( ng > agood ){
     good = realloc( good , sizeof(byte)*ng ) ; agood = ng ;
   }
   if( agood > 0 && good != NULL ) AAmemset(good,0,sizeof(byte)*agood) ;
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Load 2D histogram of x[0..n-1] and y[0..n-1], each point optionally
    weighted by w[0..n-1] (weights are all 1 if w==NULL).
    Used in the histogram-based measures of dependence between x[] and y[i].
    If something is bad on input, nbin is set to 0.  Otherwise, these global
    variables are set:
      - nbin = # of bins, nbp = nbin+1
      - nww  = sum of the weights used
      - xc   = marginal histogram of x[], for xc[0..nbin]   (nbp points in)
      - yc   = marginal histogram of y[], for yc[0..nbin]   (each direction)
      - xyc  = joint histogram of (x[],y[]), for XYC(0..nbin,0..nbin)
      - The histograms can be later normalized (by 1/nww) to have sum==1
      - Histogram can be retrieved by retrieve_2Dhist() and can be
        erased by clear_2Dhist()
      - Default number of equal-spaced bins in each direction is n^(1/3)
        - the exponent can be changed with INCOR_set_2Dhist_hpower()
        - you can set the number of bins with INCOR_set_2Dhist_hbin()
      - x[] values outside the range xbot..xtop (inclusive) will not be
        used in the histogram; mutatis mutandum for y[]
*//*------------------------------------------------------------------------*/

void INCOR_addto_2Dhist( INCOR_2Dhist *tdh , int n , float *x , float *y , float *w )
{
   int ii , ngood , xyclip ;
   float xxbot,xxtop , yybot,yytop ;
   float xcbot,xctop , ycbot,yctop ;
   int nbin,nbp,nbm ;
   float *xc , *yc , *xyc ; float nww ;
#if 0
   int use_omp , nthr ;
#else
#  define use_omp 0
#  define nthr    1
#endif

ENTRY("INCOR_addto_2Dhist") ;

   if( tdh == NULL || tdh->nbin < 3 || n <= 0 || x == NULL || y == NULL ) EXRETURN ;

   nbin = tdh->nbin ; nbp = nbin+1 ; nbm = nbin-1 ;
   xc = tdh->xc ; yc = tdh->yc ; xyc = tdh->xyc ; nww = tdh->nww ;

   /* get the min..max range for x and y data? */

   INCOR_setup_good(n+4) ;
   for( ngood=ii=0 ; ii < n ; ii++ ){
     if( GOODVAL(x[ii]) && GOODVAL(y[ii]) && (WW(ii) > 0.0f) ){
       good[ii] = 1 ; ngood++ ;
     }
   }
   if( ngood == 0 ) EXRETURN ;

   xxbot = tdh->xxbot ; xxtop = tdh->xxtop ;
   yybot = tdh->yybot ; yytop = tdh->yytop ;
   xcbot = tdh->xcbot ; xctop = tdh->xctop ;
   ycbot = tdh->ycbot ; yctop = tdh->yctop ;

   if( (xxbot >= xxtop) || (yybot >= yytop) ){  /* data ranges undefined */

     xxbot = WAY_BIG ; xxtop = -WAY_BIG ;
     for( ii=0 ; ii < n ; ii++ ){
       if( good[ii] ){
              if( x[ii] > xxtop ) xxtop = x[ii] ;
         else if( x[ii] < xxbot ) xxbot = x[ii] ;
       }
     }
     if( xxbot >= xxtop ) EXRETURN ;

     yybot = WAY_BIG ; yytop = -WAY_BIG ;
     for( ii=0 ; ii < n ; ii++ ){
       if( good[ii] ){
              if( y[ii] > yytop ) yytop = y[ii] ;
         else if( y[ii] < yybot ) yybot = y[ii] ;
       }
     }
     if( yybot >= yytop ) EXRETURN ;

     tdh->xxbot = xxbot ; tdh->xxtop = xxtop ;
     tdh->yybot = yybot ; tdh->yytop = yytop ;

     xcbot = ycbot = tdh->xcbot = tdh->ycbot =  WAY_BIG ;  /* disable */
     xctop = yctop = tdh->xctop = tdh->yctop = -WAY_BIG ;  /* clipping */
   }

   /*-- count number of good values left in range (in both x and y) --*/

   AAmemset(good,0,n) ;

   for( ngood=ii=0 ; ii < n ; ii++ ){
     if( RANGVAL(x[ii],xxbot,xxtop) && RANGVAL(y[ii],yybot,yytop) && (WW(ii) > 0.0f) ){
       good[ii] = 1 ; ngood++ ;
     }
   }
   if( ngood == 0 ) EXRETURN ;

   /*--------------- add to the 2D and 1D histograms ---------------*/

   xyclip = (xxbot < xcbot) && (xcbot < xctop) && (xctop < xxtop) &&
            (yybot < ycbot) && (ycbot < yctop) && (yctop < yytop) ;

#ifndef nthr
   nthr    = omp_get_max_threads() ;
   use_omp = (nthr > 1) ;
#endif

   if( !use_omp ){  /*** serial code ***/

     /* AFNI_do_nothing() ; fprintf(stderr,"h") ; */

     if( !xyclip ){  /*------------ equal size bins ------------*/

       float xb,xi , yb,yi , xx,yy , x1,y1 , ww ;
       int jj,kk ;

       xb = xxbot ; xi = nbm/(xxtop-xxbot) ;
       yb = yybot ; yi = nbm/(yytop-yybot) ;
       for( ii=0 ; ii < n ; ii++ ){
         if( !good[ii] ) continue ;
         xx = (x[ii]-xb)*xi ;
         jj = (int)xx ; xx = xx - jj ; x1 = 1.0f-xx ;
         yy = (y[ii]-yb)*yi ;
         kk = (int)yy ; yy = yy - kk ; y1 = 1.0f-yy ;
         ww = WW(ii) ; nww += ww ;

         if( jj < 0 || kk < 0 || jj >= nbin || kk >= nbin ) continue ;

         xc[jj] += (x1*ww); xc[jj+1] += (xx*ww);
         yc[kk] += (y1*ww); yc[kk+1] += (yy*ww);

         XYC(jj  ,kk  ) += x1*(y1*ww) ;
         XYC(jj+1,kk  ) += xx*(y1*ww) ;
         XYC(jj  ,kk+1) += x1*(yy*ww) ;
         XYC(jj+1,kk+1) += xx*(yy*ww) ;
       }

     } else if( xyclip ){  /*------------ mostly equal bins ----------------*/

       int jj,kk ;
       float xbc=xcbot , xtc=xctop , ybc=ycbot , ytc=yctop ;
       float xi,yi , xx,yy , x1,y1 , ww ;

       AFNI_do_nothing() ; /* fprintf(stderr,"c") ; */

       xi = (nbin-2.000001f)/(xtc-xbc) ;
       yi = (nbin-2.000001f)/(ytc-ybc) ;
       for( ii=0 ; ii < n ; ii++ ){
         if( !good[ii] ) continue ;
         xx = x[ii] ;
              if( xx < xbc ){ jj = 0   ; xx = 0.0f ; }
         else if( xx > xtc ){ jj = nbm ; xx = 1.0f ; }
         else               { xx = 1.0f+(xx-xbc)*xi; jj = (int)xx; xx = xx - jj; }
         yy = y[ii] ;
              if( yy < ybc ){ kk = 0   ; yy = 0.0f ; }
         else if( yy > ytc ){ kk = nbm ; yy = 1.0f ; }
         else               { yy = 1.0f+(yy-ybc)*yi; kk = (int)yy; yy = yy - kk; }

         if( jj < 0 || kk < 0 || jj >= nbin || kk >= nbin ) continue ;

         x1 = 1.0f-xx ; y1 = 1.0f-yy ; ww = WW(ii) ; nww += ww ;

         xc[jj] += (x1*ww); xc[jj+1] += (xx*ww);
         yc[kk] += (y1*ww); yc[kk+1] += (yy*ww);

         XYC(jj  ,kk  ) += x1*(y1*ww) ;
         XYC(jj+1,kk  ) += xx*(y1*ww) ;
         XYC(jj  ,kk+1) += x1*(yy*ww) ;
         XYC(jj+1,kk+1) += xx*(yy*ww) ;
       }

       AFNI_do_nothing() ; /* fprintf(stderr,".") ; */

     } /* end of clipped code */

   } else {  /*** parallelized using OpenMP ***/

     float **xccar , **yccar , **xyccar , *nwwar ; int nbpq=nbp*nbp,itt ;

     xccar  = (float **)calloc(sizeof(float *),nthr) ;  /* arrays for   */
     yccar  = (float **)calloc(sizeof(float *),nthr) ;  /* accumulation */
     xyccar = (float **)calloc(sizeof(float *),nthr) ;  /* in separate  */
     nwwar  = (float * )calloc(sizeof(float)  ,nthr) ;  /* threads      */

     for( itt=0 ; itt < nthr ; itt++ ){
       xccar [itt] = (float *)calloc(sizeof(float),nbp ) ;
       yccar [itt] = (float *)calloc(sizeof(float),nbp ) ;
       xyccar[itt] = (float *)calloc(sizeof(float),nbpq) ;
     }

     AFNI_do_nothing() ; fprintf(stderr,"H") ;

#undef  XYCC
#define XYCC(p,q) xycc[(p)+(q)*nbp]

     if( !xyclip ){  /*------------ equal size bins ------------*/

     AFNI_do_nothing() ; fprintf(stderr,"y") ;

 AFNI_OMP_START ;
#pragma omp parallel  /*** start of parallel code ***/
 {
       float *xcc, *ycc , *xycc ;
       float xb,xi , yb,yi , xx,yy , x1,y1 , ww ;
       int ii,jj,kk , ithr ;

       ithr = omp_get_thread_num() ;
#pragma omp barrier
       fprintf(stderr,"%d",ithr) ;
       xcc = xccar[ithr] ; ycc = yccar[ithr] ; xycc = xyccar[ithr] ;
       xb = xxbot ; xi = nbm/(xxtop-xxbot) ;
       yb = yybot ; yi = nbm/(yytop-yybot) ;
#pragma omp for
       for( ii=0 ; ii < n ; ii++ ){
         if( !good[ii] ) continue ;
         xx = (x[ii]-xb)*xi ;
         jj = (int)xx ; xx = xx-jj ; x1 = 1.0f-xx ;
         yy = (y[ii]-yb)*yi ;
         kk = (int)yy ; yy = yy-kk ; y1 = 1.0f-yy ;
         ww = WW(ii) ; nwwar[ithr] += ww ;

         if( jj < 0 || kk < 0 || jj >= nbin || kk >= nbin ) continue ;

         xcc[jj] += (x1*ww); xcc[jj+1] += (xx*ww);
         ycc[kk] += (y1*ww); ycc[kk+1] += (yy*ww);

         XYCC(jj  ,kk  ) += x1*(y1*ww) ;
         XYCC(jj+1,kk  ) += xx*(y1*ww) ;
         XYCC(jj  ,kk+1) += x1*(yy*ww) ;
         XYCC(jj+1,kk+1) += xx*(yy*ww) ;
       }
 }  /*** end of parallel code ***/
 AFNI_OMP_END ;

     } else if( xyclip ){  /*------------ mostly equal bins ----------------*/

     AFNI_do_nothing() ; fprintf(stderr,"x") ;

 AFNI_OMP_START ;
#pragma omp parallel  /*** start of parallel code ***/
 {
       float *xcc, *ycc , *xycc ;
       int ii,jj,kk , ithr ;
       float xbc=xcbot , xtc=xctop , ybc=ycbot , ytc=yctop ;
       float xi,yi , xx,yy , x1,y1 , ww ;

       ithr = omp_get_thread_num() ;
       fprintf(stderr,"%d",ithr) ;
#pragma omp barrier
       fprintf(stderr,":") ;
       xcc = xccar[ithr] ; ycc = yccar[ithr] ; xycc = xyccar[ithr] ;
       xi = (nbin-2.000001f)/(xtc-xbc) ;
       yi = (nbin-2.000001f)/(ytc-ybc) ;
#pragma omp for
       for( ii=0 ; ii < n ; ii++ ){
         if( !good[ii] ) continue ;
         xx = x[ii] ;
              if( xx < xbc ){ jj = 0   ; xx = 0.0f ; }
         else if( xx > xtc ){ jj = nbm ; xx = 1.0f ; }
         else               { xx = 1.0f+(xx-xbc)*xi; jj = (int)xx; xx = xx-jj; }
         yy = y[ii] ;
              if( yy < ybc ){ kk = 0   ; yy = 0.0f ; }
         else if( yy > ytc ){ kk = nbm ; yy = 1.0f ; }
         else               { yy = 1.0f+(yy-ybc)*yi; kk = (int)yy; yy = yy-kk; }

         if( jj < 0 || kk < 0 || jj >= nbin || kk >= nbin ) continue ;

         x1 = 1.0f-xx ; y1 = 1.0f-yy ; ww = WW(ii) ; nwwar[ithr] += ww ;

         xcc[jj] += (x1*ww); xcc[jj+1] += (xx*ww);
         ycc[kk] += (y1*ww); ycc[kk+1] += (yy*ww);

         XYCC(jj  ,kk  ) += x1*(y1*ww) ;
         XYCC(jj+1,kk  ) += xx*(y1*ww) ;
         XYCC(jj  ,kk+1) += x1*(yy*ww) ;
         XYCC(jj+1,kk+1) += xx*(yy*ww) ;
       }
#pragma omp barrier
       fprintf(stderr,"%d",ithr) ;
 }  /*** end of parallel code ***/
 AFNI_OMP_END ;

     }  /*-- end of mostly equal bins --*/

     /* now merge the parallel thread results */

     for( itt=0 ; itt < nthr ; itt++ ){
       if( nwwar[itt] > 0.0f ){
         nww += nwwar[itt] ;
         for( ii=0 ; ii < nbp ; ii++ ){ xc[ii] += xccar[itt][ii]; yc[ii] += yccar[itt][ii]; }
         for( ii=0 ; ii < nbpq ; ii++ ){ xyc[ii] += xyccar[itt][ii] ; }
       }
       free(xccar[itt]) ; free(yccar[itt]) ; free(xyccar[itt]) ;
     }
     free(xccar) ; free(yccar) ; free(xyccar) ; free(nwwar) ;

   } /* end of using OpenMP */

   /* AFNI_do_nothing() ; fprintf(stderr,".") ; */

   tdh->nww = nww ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* scale histogram to have sum==1 */

void INCOR_normalize_2Dhist( INCOR_2Dhist *tdh )
{
   float nww , *xc, *yc, *xyc ; int nbp ;
   if( tdh == NULL ) return ;
   nww = tdh->nww; xc = tdh->xc; yc = tdh->yc; xyc = tdh->xyc; nbp = tdh->nbin+1;
   if( nww > 0.0f && nww != 1.0f && xyc != NULL && xc != NULL && yc != NULL ){
     float ni ; int nbq , ii ;
     ni = 1.0f / nww ;
     for( ii=0 ; ii < nbp ; ii++ ){ xc[ii]  *= ni; yc[ii] *= ni; }
     nbq = nbp*nbp ;
     for( ii=0 ; ii < nbq ; ii++ ){ xyc[ii] *= ni; }
   }
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the mutual info from a histogram (which also normalizes it).
----------------------------------------------------------------------------*/

double INCOR_mutual_info( INCOR_2Dhist *tdh )
{
   int ii,jj ;
   double val , nww ;
   float *xc, *yc, *xyc ; int nbp ;

   if( tdh == NULL ) return 0.0 ;

   nww = (double)tdh->nww; if( nww <= 0.0 ) return 0.0 ;
   xc = tdh->xc; yc = tdh->yc; xyc = tdh->xyc; nbp = tdh->nbin+1;

   INCOR_normalize_2Dhist(tdh) ;

   /*-- compute MI from histogram --*/

   val = 0.0 ;
   for( ii=0 ; ii < nbp ; ii++ ){
    for( jj=0 ; jj < nbp ; jj++ ){
     if( XYC(ii,jj) > 0.0f )
      val += XYC(ii,jj) * log( XYC(ii,jj)/(xc[ii]*yc[jj]) ) ;
   }}
   return (1.4427*val) ;  /* units are bits, just for fun */
}

/*--------------------------------------------------------------------------*/
/*! Compute the normalized mutual info from a 2D histogram.
    Actually, returns H(x,y) / [ H(x)+H(y) ], which should be small if
    x and y are redundant and should be large if they are independent.
----------------------------------------------------------------------------*/

double INCOR_norm_mutinf( INCOR_2Dhist *tdh )
{
   int ii,jj ;
   double numer , denom , nww ;
   float *xc, *yc, *xyc ; int nbp ;

   if( tdh == NULL ) return 0.0 ;

   nww = (double)tdh->nww; if( nww <= 0.0 ) return 0.0 ;
   xc = tdh->xc; yc = tdh->yc; xyc = tdh->xyc; nbp = tdh->nbin+1;

   INCOR_normalize_2Dhist(tdh) ;

   /*-- compute NMI from histogram --*/

   denom = numer = 0.0 ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0f ) denom += xc[ii] * log( xc[ii] ) ;
     if( yc[ii] > 0.0f ) denom += yc[ii] * log( yc[ii] ) ;
     for( jj=0 ; jj < nbp ; jj++ ){
       if( XYC(ii,jj) > 0.0f ) numer += XYC(ii,jj) * log( XYC(ii,jj) );
     }
   }
   if( denom != 0.0 ) denom = numer / denom ;
   return denom ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the correlation ratio from a 2D histogram.
----------------------------------------------------------------------------*/

double INCOR_corr_ratio( INCOR_2Dhist *tdh , int crmode )
{
   int ii,jj ;
   double vv,mm , val , cyvar , uyvar , yrat,xrat , nww ;
   float *xc, *yc, *xyc ; int nbp ;

   if( tdh == NULL ) return 0.0 ;

   nww = (double)tdh->nww; if( nww <= 0.0 ) return 0.0 ;
   xc = tdh->xc; yc = tdh->yc; xyc = tdh->xyc; nbp = tdh->nbin+1;

   INCOR_normalize_2Dhist(tdh) ;

   /*-- compute CR(y|x) from histogram --*/

   cyvar = 0.0 ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0f ){
       vv = mm = 0.0 ;                /* mm=E(y|x)  vv=E(y^2|x) */
       for( jj=1 ; jj < nbp ; jj++ ){
         mm += (jj * XYC(ii,jj)) ; vv += jj * (jj * XYC(ii,jj)) ;
       }
       cyvar += (vv - mm*mm/xc[ii] ) ; /* Var(y|x) */
     }
   }
   vv = mm = uyvar = 0.0 ;
   for( jj=1 ; jj < nbp ; jj++ ){        /* mm=E(y)  vv=E(y^2) */
     mm += (jj * yc[jj]) ; vv += jj * (jj * yc[jj]) ;
   }
   uyvar = vv - mm*mm ;                  /* Var(y) */
   yrat  = (uyvar > 0.0 ) ? cyvar/uyvar  /* Var(y|x) / Var(y) */
                          : 1.0  ;

   if( crmode == 0 ) return (1.0-yrat) ;   /** unsymmetric **/

   /** compute CR(x|y) also, for symmetrization **/

   cyvar = 0.0 ;
   for( jj=0 ; jj < nbp ; jj++ ){
     if( yc[jj] > 0.0 ){
       vv = mm = 0.0 ;                /* mm=E(x|y)  vv=E(x^2|y) */
       for( ii=1 ; ii < nbp ; ii++ ){
         mm += (ii * XYC(ii,jj)) ; vv += ii * (ii * XYC(ii,jj)) ;
       }
       cyvar += (vv - mm*mm/yc[jj] ) ; /* Var(x|y) */
     }
   }
   vv = mm = uyvar = 0.0 ;
   for( ii=1 ; ii < nbp ; ii++ ){     /* mm=E(x)  vv=E(x^2) */
     mm += (ii * xc[ii]) ; vv += ii * (ii * xc[ii]) ;
   }
   uyvar = vv - mm*mm ;                 /* Var(x) */
   xrat  = (uyvar > 0.0) ? cyvar/uyvar  /* Var(x|y) / Var(x) */
                         : 1.0 ;

   if( crmode == 2 ) return (1.0 - 0.5*(xrat+yrat)) ; /** additive **/

   return (1.0 - xrat*yrat) ;                          /** multiplicative **/
}

/*--------------------------------------------------------------------------*/
/*! Compute the Hellinger metric from a 2D histogram.
----------------------------------------------------------------------------*/

double INCOR_hellinger( INCOR_2Dhist *tdh )
{
   int ii,jj ;
   double val , pq , nww ;
   float *xc, *yc, *xyc ; int nbp ;

   if( tdh == NULL ) return 0.0 ;

   nww = (double)tdh->nww; if( nww <= 0.0 ) return 0.0 ;
   xc = tdh->xc; yc = tdh->yc; xyc = tdh->xyc; nbp = tdh->nbin+1;

   INCOR_normalize_2Dhist(tdh) ;

   /*-- compute Hell metric from histogram --*/

   val = 0.0 ;
   for( ii=0 ; ii < nbp ; ii++ ){
    for( jj=0 ; jj < nbp ; jj++ ){
     pq = XYC(ii,jj) ;
     if( pq > 0.0 ) val += sqrt( pq * xc[ii] * yc[jj] ) ;
   }}
   return (1.0-val) ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the Hellinger metric, mutual info, normalized MI, and
    (additively) symmetrized correlation ratio, and return all 4
    (in that order), from a 2D histogram.

    The first 3 values all measure the closeness of the joint histogram to
    the product of the marginals:
      - Hellinger is smaller when the joint is closer to the marginals' product
      - MI is also smaller when the joint is closer to the marginal's product
      - NMI is larger when the joint is closer to the marginal's product
    Correlation ratio (symmetrized by addition == CRA) is larger when
    the two variables are nonlinearly correlated.

    As measures of association (generalized correlation): more closely
    associated variables correspond to larger Hellinger and MI and CRA,
    and to smaller NMI.
*//*------------------------------------------------------------------------*/

double_quad INCOR_helmicra( INCOR_2Dhist *tdh )
{
   int ii,jj ;
   double hel , pq , vv,uu , nww ;
   double val , cyvar , uyvar , yrat,xrat ;
   double_quad hmc = {0.0,0.0,0.0,0.0} ;
   float *xc, *yc, *xyc ; int nbp ;

   if( tdh == NULL ) return hmc ;

   nww = (double)tdh->nww; if( nww <= 0.0 ) return hmc ;
   xc = tdh->xc; yc = tdh->yc; xyc = tdh->xyc; nbp = tdh->nbin+1;

   INCOR_normalize_2Dhist(tdh) ;

   /*-- compute Hel, MI, NMI from histogram --*/

   hel = vv = uu = 0.0 ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0 ) vv += xc[ii] * log( xc[ii] ) ;
     if( yc[ii] > 0.0 ) vv += yc[ii] * log( yc[ii] ) ;
     for( jj=0 ; jj < nbp ; jj++ ){
       pq = XYC(ii,jj) ;
       if( pq > 0.0 ){
         hel += sqrt( pq * xc[ii] * yc[jj] ) ;
         uu  += pq * log( pq );
       }
     }
   }
   hmc.a = 1.0 - hel ;                  /* Hellinger */
   hmc.b = uu - vv ;                    /* MI */
   hmc.c = (vv != 0.0) ? uu/vv : 0.0 ;  /* NMI */

   /*-- compute CR(y|x) from histogram --*/

   cyvar = 0.0 ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0 ){
       vv = uu = 0.0 ;               /* uu=E(y|x)  vv=E(y^2|x) */
       for( jj=1 ; jj < nbp ; jj++ ){
         uu += (jj * XYC(ii,jj)) ; vv += jj * (jj * XYC(ii,jj)) ;
       }
       cyvar += (vv - uu*uu/xc[ii] ) ; /* Var(y|x) */
     }
   }
   vv = uu = uyvar = 0.0 ;
   for( jj=1 ; jj < nbp ; jj++ ){     /* uu=E(y)  vv=E(y^2) */
     uu += (jj * yc[jj]) ; vv += jj * (jj * yc[jj]) ;
   }
   uyvar = vv - uu*uu ;                  /* Var(y) */
   yrat  = (uyvar > 0.0) ? cyvar/uyvar   /* Var(y|x) / Var(y) */
                         : 1.0 ;

   /** compute CR(x|y) also, for symmetrization **/

   cyvar = 0.0 ;
   for( jj=0 ; jj < nbp ; jj++ ){
     if( yc[jj] > 0.0 ){
       vv = uu = 0.0 ;               /* uu=E(x|y)  vv=E(x^2|y) */
       for( ii=1 ; ii < nbp ; ii++ ){
         uu += (ii * XYC(ii,jj)) ; vv += ii * (ii * XYC(ii,jj)) ;
       }
       cyvar += (vv - uu*uu/yc[jj] ) ; /* Var(x|y) */
     }
   }
   vv = uu = uyvar = 0.0 ;
   for( ii=1 ; ii < nbp ; ii++ ){     /* uu=E(x)  vv=E(x^2) */
     uu += (ii * xc[ii]) ; vv += ii * (ii * xc[ii]) ;
   }
   uyvar = vv - uu*uu ;                 /* Var(x) */
   xrat  = (uyvar > 0.0) ? cyvar/uyvar  /* Var(x|y) / Var(x) */
                         : 1.0 ;

   hmc.d = 1.0 - 0.5*(xrat+yrat) ; /** additive symmetrization **/
   return hmc ;
}

/*----------------------------------------------------------------------------*/

static double hpow = 0.3333333333321 ;
void INCOR_set_2Dhist_hpower( double hh )
{
  hpow = (hh > 0.0 && hh < 1.0) ? hh : 0.3333333333321 ;
}

static int nhbin = 0 ;
void INCOR_set_2Dhist_hbin( int nn ){ nhbin = nn; }

int INCOR_2Dhist_compute_nbin( int ndata )
{
   int nbin ;

   nbin = (nhbin > 4) ? nhbin : (int)rint(pow((double)ndata,hpow)) ;
   if( nbin > 255 ) nbin = 255 ; else if( nbin < 5 ) nbin = 5 ;
   return nbin ;
}

/*----------------------------------------------------------------------------*/

INCOR_2Dhist * INCOR_create_2Dhist( int nbin ,
                                    float xbot , float xtop ,
                                    float ybot , float ytop ,
                                    float xcbot, float xctop,
                                    float ycbot, float yctop  )
{
   INCOR_2Dhist *tdh ; int nbp ;

ENTRY("INCOR_create_2Dhist") ;

   if( nbin < 3 ) nbin = 3 ;

   tdh = (INCOR_2Dhist *)calloc(1,sizeof(INCOR_2Dhist)) ;

   tdh->meth  = 0 ;  /* undefined as yet */
   tdh->nbin  = nbin ;
   tdh->xxbot = xbot ;  tdh->yybot = ybot ;
   tdh->xxtop = xtop ;  tdh->yytop = ytop ;
   tdh->xcbot = xcbot ; tdh->ycbot = ycbot ;
   tdh->xctop = xctop ; tdh->yctop = yctop ;

   nbp = nbin+1 ;
   tdh->xc  = (float *)calloc(sizeof(float),nbp) ;
   tdh->yc  = (float *)calloc(sizeof(float),nbp) ;
   tdh->xyc = (float *)calloc(sizeof(float),nbp*nbp) ;
   tdh->nww = 0.0f ;

   RETURN(tdh) ;
}

/*----------------------------------------------------------------------------*/

void INCOR_destroy_2Dhist( INCOR_2Dhist *tin )
{
   if( tin == NULL ) return ;
   if( tin->xc  != NULL ) free(tin->xc) ;
   if( tin->yc  != NULL ) free(tin->yc) ;
   if( tin->xyc != NULL ) free(tin->xyc) ;
   free(tin) ;
   return ;
}

/*----------------------------------------------------------------------------*/

void INCOR_copyover_2Dhist( INCOR_2Dhist *tin , INCOR_2Dhist *tout )
{
   int nbp ;

ENTRY("INCOR_copyover_2Dhist") ;

   if( tin == NULL || tout == NULL || tin == tout ) EXRETURN ;

   if( tout->xc  != NULL ) free(tout->xc) ;
   if( tout->yc  != NULL ) free(tout->yc) ;
   if( tout->xyc != NULL ) free(tout->xyc) ;

   tout->meth  = tin->meth ;
   tout->nbin  = tin->nbin ;
   tout->xxbot = tin->xxbot ; tout->yybot = tin->yybot ;
   tout->xxtop = tin->xxtop ; tout->yytop = tin->yytop ;
   tout->xcbot = tin->xcbot ; tout->ycbot = tin->ycbot ;
   tout->xctop = tin->xctop ; tout->yctop = tin->yctop ;
   tout->nww   = tin->nww ;

   nbp = tin->nbin + 1 ;
   tout->xc  = (float *)malloc(sizeof(float)*nbp) ;
   tout->yc  = (float *)malloc(sizeof(float)*nbp) ;
   tout->xyc = (float *)malloc(sizeof(float)*nbp*nbp) ;

   AAmemcpy( tout->xc , tin->xc , sizeof(float)*nbp ) ;
   AAmemcpy( tout->yc , tin->yc , sizeof(float)*nbp ) ;
   AAmemcpy( tout->xyc, tin->xyc, sizeof(float)*nbp*nbp ) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

void INCOR_addto_incomplete_pearson( int n, float *x, float *y,
                                            float *w, INCOR_pearson *inpear )
{
   int ii ; double sx,sxx , sy,syy,sxy , sw ;

   if( n <= 0 || x == NULL || y == NULL || inpear == NULL ) return ;

   sx = inpear->sx ; sxx = inpear->sxx ;
   sy = inpear->sy ; syy = inpear->syy ; sxy = inpear->sxy ; sw = inpear->sw ;

   if( w == NULL ){
     double xx , yy ;
     for( ii=0 ; ii < n ; ii++ ){
       xx = (double)x[ii] ; yy = (double)y[ii] ;
       sx += xx ; sxx += xx*xx ; sy += yy ; syy += yy*yy ; sxy += xx*yy ;
     }
     sw += (double)n ;
   } else {
     double xx , yy , ww ;
     for( ii=0 ; ii < n ; ii++ ){
       ww = (double)w[ii] ;
       if( ww > 0.0 ){
         xx = (double)x[ii] ; yy = (double)y[ii] ;
         sx += xx*ww ; sxx += xx*xx*ww ;
         sy += yy*ww ; syy += yy*yy*ww ; sxy += xx*yy*ww ; sw += ww ;
       }
     }
   }

   inpear->npt += n ;
   inpear->sx   = sx ; inpear->sxx = sxx ;
   inpear->sy   = sy ; inpear->syy = syy ; inpear->sxy = sxy ; inpear->sw = sw ;

   return ;
}

/*----------------------------------------------------------------------------*/

void INCOR_destroy_incomplete_pearson( INCOR_pearson *inpear )
{
   if( inpear != NULL ) free((void *)inpear) ;
}

/*----------------------------------------------------------------------------*/

INCOR_pearson * INCOR_create_incomplete_pearson(void)
{
   INCOR_pearson *inpear ;

   inpear = (INCOR_pearson *)calloc(1,sizeof(INCOR_pearson)) ;
   inpear->sx  = 0.0 ; inpear->sxx = 0.0 ;
   inpear->sy  = 0.0 ; inpear->syy = 0.0 ;
   inpear->sxy = 0.0 ; inpear->sw  = 0.0 ; inpear->npt = 0 ;

   inpear->meth = GA_MATCH_PEARSON_SCALAR ;
   return inpear ;
}

/*----------------------------------------------------------------------------*/

double INCOR_incomplete_pearson( INCOR_pearson *inpear )
{
   double xv , yv , xy , swi , val ;

   if( inpear->sw <= 0.0 ) return 0.0 ;

   swi = 1.0 / inpear->sw ;

   xv = inpear->sxx - inpear->sx * inpear->sx * swi ;
   yv = inpear->syy - inpear->sy * inpear->sy * swi ;
   xy = inpear->sxy - inpear->sx * inpear->sy * swi ;

   if( xv <= 0.0 || yv <= 0.0 ) return 0.0 ;
   val = (xy/sqrt(xv*yv)) ; val = MYatanh(val) ; return val ;
}

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

#ifndef USE_OMP /*--- Serial code version ---*/

void INCOR_addto_incomplete_pearclp_SS( int n, float *x, float *y,
                                        float *w, INCOR_pearclp *inpear )
{
   int ii ; double sx,sxx , sy,syy,sxy , sw ;
   double xcb,xct , ycb,yct , xmid,ymid ;
   double xdb,xdt , ydb,ydt ;

   sx = inpear->sx ; sxx = inpear->sxx ;
   sy = inpear->sy ; syy = inpear->syy ; sxy = inpear->sxy ; sw = inpear->sw ;

   xcb = inpear->xcbot ; xct = inpear->xctop ; xmid = 0.5f*(xcb+xct) ;
   ycb = inpear->ycbot ; yct = inpear->yctop ; ymid = 0.5f*(ycb+yct) ;
   xdb = inpear->xdbot ; xdt = inpear->xdtop ;
   ydb = inpear->ydbot ; ydt = inpear->ydtop ;

   if( w == NULL ){
     double xx , yy , ww ; int cl ;
     for( ii=0 ; ii < n ; ii++ ){
       cl = 1 ;
       xx = (double)x[ii] ;
       if( xx <= xcb ){ xx = xdb; cl++; } else if( xx >= xct ){ xx = xdt; cl++; }
       yy = (double)y[ii] ;
       if( yy <= ycb ){ yy = ydb; cl++; } else if( yy >= yct ){ yy = ydt; cl++; }
       ww = 1.0 / cl ; xx -= xmid ; yy -= ymid ;
       sx += xx*ww ; sxx += xx*xx*ww ; sy += yy*ww ; syy += yy*yy*ww ; sxy += xx*yy*ww ; sw += ww ;
     }
   } else {
     double xx , yy , ww ; int cl ;
     for( ii=0 ; ii < n ; ii++ ){
       ww = (double)w[ii] ;
       if( ww > 0.0 ){
         cl = 1 ;
         xx = (double)x[ii] ;
         if( xx <= xcb ){ xx = xdb; cl++; } else if( xx >= xct ){ xx = xdt; cl++; }
         yy = (double)y[ii] ;
         if( yy <= ycb ){ yy = ydb; cl++; } else if( yy >= yct ){ yy = ydt; cl++; }
         ww /= cl ; xx -= xmid ; yy -= ymid ;
         sx += xx*ww ; sxx += xx*xx*ww ;
         sy += yy*ww ; syy += yy*yy*ww ; sxy += xx*yy*ww ; sw += ww ;
       }
     }
   }

   inpear->npt += n ;
   inpear->sx   = sx ; inpear->sxx = sxx ;
   inpear->sy   = sy ; inpear->syy = syy ; inpear->sxy = sxy ; inpear->sw = sw ;

   return ;
}

/*----------------------------------------------------------------------------*/
#else /*--- Parallel-ized verison of the above ----------------------*/

void INCOR_addto_incomplete_pearclp_PP( int n, float *x, float *y,
                                        float *w, INCOR_pearclp *inpear )
{
   int jj ; double sx,sxx , sy,syy,sxy , sw ;
   double xcb,xct , ycb,yct , xmid,ymid ;
   double xdb,xdt , ydb,ydt ;

   xcb = inpear->xcbot ; xct = inpear->xctop ; xmid = 0.5f*(xcb+xct) ;
   ycb = inpear->ycbot ; yct = inpear->yctop ; ymid = 0.5f*(ycb+yct) ;
   xdb = inpear->xdbot ; xdt = inpear->xdtop ;
   ydb = inpear->ydbot ; ydt = inpear->ydtop ;

   for( jj=0 ; jj < nthmax ; jj++ ){
     dhaar[jj] = dhbbr[jj] = dhccr[jj] = dhddr[jj] = dheer[jj] = dhffr[jj] = 0.0 ;
   }

   if( w == NULL ){

#pragma omp parallel
     { double xx , yy , ww ; int cl,ii ; int ith=omp_get_thread_num() ;
       double tx=0.0,txx=0.0,ty=0.0,tyy=0.0,txy=0.0,tw=0.0 ;
#pragma omp for
       for( ii=0 ; ii < n ; ii++ ){
         cl = 1 ;
         xx = (double)x[ii] ;
         if( xx <= xcb ){ xx = xdb; cl++; } else if( xx >= xct ){ xx = xdt; cl++; }
         yy = (double)y[ii] ;
         if( yy <= ycb ){ yy = ydb; cl++; } else if( yy >= yct ){ yy = ydt; cl++; }
         ww = 1.0 / cl ; xx -= xmid ; yy -= ymid ;
         tx  += ww*xx    ; txx += ww*xx*xx ; ty += ww*yy ;
         tyy += ww*yy*yy ; txy += ww*xx*yy ; tw += ww ;
       }
#pragma omp critical
       { dhaar[ith] = tx  ; dhbbr[ith] = txx ; dhccr[ith] = ty ;
         dhddr[ith] = tyy ; dheer[ith] = txy ; dhffr[ith] = tw ; }
     } /* end parallel */

   } else {

#pragma omp parallel
     { double xx , yy , ww ; int cl,ii ; int ith=omp_get_thread_num() ;
       double tx=0.0,txx=0.0,ty=0.0,tyy=0.0,txy=0.0,tw=0.0 ;
#pragma omp for
       for( ii=0 ; ii < n ; ii++ ){
         ww = (double)w[ii] ;
         if( ww > 0.0 ){
           cl = 1 ;
           xx = (double)x[ii] ;
           if( xx <= xcb ){ xx = xdb; cl++; } else if( xx >= xct ){ xx = xdt; cl++; }
           yy = (double)y[ii] ;
           if( yy <= ycb ){ yy = ydb; cl++; } else if( yy >= yct ){ yy = ydt; cl++; }
           ww /= cl ; xx -= xmid ; yy -= ymid ;
           tx  += ww*xx    ; txx += ww*xx*xx ; ty += ww*yy ;
           tyy += ww*yy*yy ; txy += ww*xx*yy ; tw += ww ;
         }
       }
#pragma omp critical
       { dhaar[ith] = tx  ; dhbbr[ith] = txx ; dhccr[ith] = ty ;
         dhddr[ith] = tyy ; dheer[ith] = txy ; dhffr[ith] = tw ; }
     } /* end parallel */
   }

   /*-- add partial sums from each thread to the results --*/

   sx  = inpear->sx  ; sxx = inpear->sxx ;
   sy  = inpear->sy  ; syy = inpear->syy ;
   sxy = inpear->sxy ; sw  = inpear->sw  ;
   for( jj=0 ; jj < nthmax ; jj++ ){
     sx  += dhaar[jj] ; sxx += dhbbr[jj] ;
     sy  += dhccr[jj] ; syy += dhddr[jj] ;
     sxy += dheer[jj] ; sw  += dhffr[jj] ;
   }

   inpear->npt += n ;
   inpear->sx   = sx ; inpear->sxx = sxx ;
   inpear->sy   = sy ; inpear->syy = syy ; inpear->sxy = sxy ; inpear->sw = sw ;

   return ;
}
#endif  /* USE_OMP */

/*----------------------------------------------------------------------------*/

void INCOR_addto_incomplete_pearclp( int n, float *x, float *y,
                                     float *w, INCOR_pearclp *inpear )
{
   if( n <= 0 || x == NULL || y == NULL || inpear == NULL ) return ;
#ifdef USE_OMP
   INCOR_addto_incomplete_pearclp_PP( n , x,y,w , inpear ) ;
#else
   INCOR_addto_incomplete_pearclp_SS( n , x,y,w , inpear ) ;
#endif
   return ;
}

/*----------------------------------------------------------------------------*/

void INCOR_destroy_incomplete_pearclp( INCOR_pearclp *inpear )
{
   if( inpear != NULL ) free((void *)inpear) ;
}

/*----------------------------------------------------------------------------*/

INCOR_pearclp * INCOR_create_incomplete_pearclp(void)
{
   INCOR_pearclp *inpear ;

   inpear = (INCOR_pearclp *)calloc(1,sizeof(INCOR_pearclp)) ;
   inpear->sx  = 0.0 ; inpear->sxx = 0.0 ;
   inpear->sy  = 0.0 ; inpear->syy = 0.0 ;
   inpear->sxy = 0.0 ; inpear->sw  = 0.0 ; inpear->npt = 0 ;

   inpear->xcbot = inpear->xctop = inpear->ycbot = inpear->yctop = 0.0 ;
   inpear->xdbot = inpear->xdtop = inpear->ydbot = inpear->ydtop = 0.0 ;

   inpear->meth = GA_MATCH_PEARCLP_SCALAR ;
   return inpear ;
}

/*----------------------------------------------------------------------------*/

double INCOR_incomplete_pearclp( INCOR_pearclp *inpear )
{
   double xv , yv , xy , swi , val ;

   if( inpear->sw <= 0.0 ) return 0.0 ;

   swi = 1.0 / inpear->sw ;

   xv = inpear->sxx - inpear->sx * inpear->sx * swi ;
   yv = inpear->syy - inpear->sy * inpear->sy * swi ;
   xy = inpear->sxy - inpear->sx * inpear->sy * swi ;

   if( xv <= 0.0 || yv <= 0.0 ) return 0.0 ;
   val = (xy/sqrt(xv*yv)) ; val = MYatanh(val) ; return val ;
}

/*============================================================================*/
/* Generic INCOR functions */
/*============================================================================*/

/*----------------------------------------------------------------------------*/
/* Check if a method code is implemented. */

int INCOR_check_meth_code( int meth )
{
  switch( meth ){
     case GA_MATCH_PEARSON_SCALAR:    return 1 ;

     case GA_MATCH_PEARCLP_SCALAR:    return 3 ;

     case GA_MATCH_MUTINFO_SCALAR:
     case GA_MATCH_CORRATIO_SCALAR:
     case GA_MATCH_NORMUTIN_SCALAR:
     case GA_MATCH_HELLINGER_SCALAR:
     case GA_MATCH_CRAT_SADD_SCALAR:
     case GA_MATCH_CRAT_USYM_SCALAR:  return 2 ;  /* uses 2Dhist */

     case GA_MATCH_PEARSON_LOCALS:
     case GA_MATCH_PEARSON_LOCALA:    return 4 ;  /* 25 Jun 2014 */
  }

  return 0 ;
}

/*----------------------------------------------------------------------------*/

static byte *lpc_mask = NULL ;

void INCOR_set_lpc_mask( byte *mmm ){ lpc_mask = mmm ; }

/*----------------------------------------------------------------------------*/

static INCORR_BLOK_set *lpc_ibs = NULL ;

void INCOR_reset_lpc_ibs( int nx , int ny , int nz )
{
   int need_new ;

   need_new = (lpc_ibs     == NULL) ||
              (lpc_ibs->nx != nx  ) ||
              (lpc_ibs->ny != ny  ) ||
              (lpc_ibs->nz != nz  )   ;

   if( need_new ){
     if( lpc_ibs != NULL ) destroy_INCORR_BLOK_set(lpc_ibs) ;
     if( nx > 3 && ny > 3 && nz > 0 )
       lpc_ibs = create_INCORR_BLOK_set( nx,ny,nz, 1.0f,1.0f,1.0f,
                                         lpc_mask, GA_BLOK_TOHD, 6.54321f, 0 ) ;
     else
       lpc_ibs = NULL ;
   } else {
     clear_INCORR_BLOK_set( lpc_ibs ) ;
   }

   return ;
}

/*----------------------------------------------------------------------------*/
/* Create an INCOR object, return a pointer to it.
     meth = method to use
     mpar = floatvec with parameters:

       For meth == GA_MATCH_PEARSON_SCALAR, mpar is not used.

       For meth == 2D histogram method, mpar is used as follows:
         mpar->ar[0] = nbin
         mpar->ar[1] = xbot  mpar->ar[2] = xtop
         mpar->ar[3] = ybot  mpar->ar[4] = ytop
         mpar->ar[5] = xcbot mpar->ar[6] = xctop
         mpar->ar[7] = ycbot mpar->ar[8] = yctop
         * If you have a good idea of how many data points are coming in,
           you can use function INCOR_2Dhist_compute_nbin() to compute
           a useful nbin from the number of data points.
         * If xbot >= xtop or if ybot >= ytop, then these values will be
           computed from the first set of data input (and then fixed).
         * If we have the relationships
                 xbot < xcbot < xctop < xtop
             AND ybot < ycbot < yctop < ytop
           then the first and last bins are 'oddly' sized, and all the interior
           bins are evenly spaced.  Otherwise, all the bins are evenly spaced.
         * If you have some sample data to supply, you can use function
           INCOR_2Dhist_minmax() to compute xbot, xtop, ybot, and ytop.
           In addition, you can use function INCOR_2Dhist_xyclip() to compute
           xcbot, xctop, ycbot, and yctop.

       For meth == Local Pearson method, mpar is used as follows:
         mpar->ar[0] = nx   ar[1] = ny  ar[2] = nz  (of total volume)
               ar[3] = ibot ar[4] = itop            (of small volume)
               ar[5] = jbot ar[6] = jtop
               ar[7] = kbot ar[8] = ktop
*//*--------------------------------------------------------------------------*/

void * INCOR_create( int meth , floatvec *mpar )
{
   void *vinc = NULL;

ENTRY("INCOR_create") ;

   switch( meth ){

     case GA_MATCH_PEARSON_SCALAR:
       vinc = (void *)INCOR_create_incomplete_pearson() ;
     break ;

     case GA_MATCH_PEARCLP_SCALAR:{
       INCOR_pearclp *pc ;
       pc = INCOR_create_incomplete_pearclp() ; vinc = (void *)pc ;
       if( mpar != NULL && mpar->nar > 8 ){
         pc->xdbot = mpar->ar[1] ; pc->xdtop = mpar->ar[2] ;
         pc->ydbot = mpar->ar[3] ; pc->ydtop = mpar->ar[4] ;
         pc->xcbot = mpar->ar[5] ; pc->xctop = mpar->ar[6] ;
         pc->ycbot = mpar->ar[7] ; pc->yctop = mpar->ar[8] ;
       }
     }
     break ;

     case GA_MATCH_MUTINFO_SCALAR:     /* methods that use 2D histograms */
     case GA_MATCH_CORRATIO_SCALAR:
     case GA_MATCH_NORMUTIN_SCALAR:
     case GA_MATCH_HELLINGER_SCALAR:
     case GA_MATCH_CRAT_SADD_SCALAR:
     case GA_MATCH_CRAT_USYM_SCALAR:{
       INCOR_2Dhist *tdh ;
       int nbin ;
       float xbot,xtop, ybot,ytop, xcbot,xctop, ycbot,yctop;
       nbin = (mpar == NULL) ? 0 : (int)mpar->ar[0] ;
       if( nbin < 0 ) nbin = INCOR_2Dhist_compute_nbin(-nbin) ;
       if( nbin > 0 && mpar != NULL && mpar->nar > 8 ){
         xbot  = mpar->ar[1] ; xtop  = mpar->ar[2] ;
         ybot  = mpar->ar[3] ; ytop  = mpar->ar[4] ;
         xcbot = mpar->ar[5] ; xctop = mpar->ar[6] ;
         ycbot = mpar->ar[7] ; yctop = mpar->ar[8] ;
       } else {
         xbot = xtop = ybot = ytop = xcbot = xctop = ycbot = yctop = 0.0f ;
       }
       tdh = INCOR_create_2Dhist( nbin , xbot ,xtop , ybot ,ytop ,
                                         xcbot,xctop, ycbot,yctop ) ;
       if( tdh != NULL ){
         tdh->meth = meth ; vinc = (void *)tdh ;
       }
     }
     break ;

     case GA_MATCH_PEARSON_LOCALS:   /* local Pearson stuff [25 Jun 2014] */
     case GA_MATCH_PEARSON_LOCALA:{
       INCOR_localpearson *ilp ;
       int nx,ny,nz ;
       if( mpar == NULL || mpar->nar < 9 ) RETURN(vinc) ;  /* bad */
       ilp = (INCOR_localpearson *)calloc(1,sizeof(INCOR_localpearson)) ;
       ilp->meth  = meth ;
       ilp->iibot = (int)mpar->ar[3] ; ilp->iitop = (int)mpar->ar[4] ;
       ilp->jjbot = (int)mpar->ar[5] ; ilp->jjtop = (int)mpar->ar[6] ;
       ilp->kkbot = (int)mpar->ar[7] ; ilp->kktop = (int)mpar->ar[8] ;
       ilp->nii   = ilp->iitop - ilp->iibot + 1 ;
       ilp->njj   = ilp->jjtop - ilp->jjbot + 1 ;
       ilp->nkk   = ilp->kktop - ilp->kkbot + 1 ;
       nx = (int)mpar->ar[0] ; ny = (int)mpar->ar[1] ; nz = (int)mpar->ar[2] ;
       INCOR_reset_lpc_ibs(nx,ny,nz) ; ilp->ibs = lpc_ibs ;
       vinc = (void *)ilp ;

#ifdef ALLOW_DEBUG_LPC
       debug_lpc = AFNI_yesenv("AFNI_DEBUG_LPC") ;
#endif
     }
     break ;

   }

   RETURN(vinc) ;
}

/*----------------------------------------------------------------------------*/
/* Erase an INCOR struct from the Macrocosmic All. */

void INCOR_destroy( void *vp )
{
ENTRY("INCOR_destroy") ;

   if( vp == NULL ) EXRETURN ;

   switch( INCOR_methcode(vp) ){

     case GA_MATCH_PEARSON_SCALAR:
       INCOR_destroy_incomplete_pearson(vp) ;
     break ;

     case GA_MATCH_PEARCLP_SCALAR:
       INCOR_destroy_incomplete_pearclp(vp) ;
     break ;

     case GA_MATCH_MUTINFO_SCALAR:
     case GA_MATCH_CORRATIO_SCALAR:
     case GA_MATCH_NORMUTIN_SCALAR:
     case GA_MATCH_HELLINGER_SCALAR:
     case GA_MATCH_CRAT_SADD_SCALAR:
     case GA_MATCH_CRAT_USYM_SCALAR:
       INCOR_destroy_2Dhist(vp) ;
     break ;

     case GA_MATCH_PEARSON_LOCALS:   /* 25 Jun 2014 */
     case GA_MATCH_PEARSON_LOCALA:{
       if( lpc_mask == NULL ){
         INCOR_localpearson *ilp = (INCOR_localpearson *)vp ;
         destroy_INCORR_BLOK_set(ilp->ibs) ;
         free(ilp) ; lpc_ibs = NULL ;
       }
     }
     break ;

   }
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Copy the internal data of an INCOR struct 'vin' over that of 'vout'.
   This is used for adding data to vin while keeping vout pristine for re-use.
   If vin is NULL, then vout is cleared to the empty INCOR struct.
*//*--------------------------------------------------------------------------*/

void INCOR_copyover( void *vin , void *vout )
{
   int meth ;

ENTRY("INCOR_copyover") ;

   if( vout == NULL || vin == vout ) EXRETURN ;

   if( vin != NULL ) meth = INCOR_methcode(vin) ;
   else              meth = INCOR_methcode(vout) ;

   switch( meth ){

     case GA_MATCH_PEARSON_SCALAR:
       if( vin != NULL ){
         AAmemcpy( vout , vin , sizeof(INCOR_pearson) ) ;
       } else {
         INCOR_pearson *vp = (INCOR_pearson *)vout ;
         vp->sx  = 0.0 ; vp->sxx = 0.0 ;
         vp->sy  = 0.0 ; vp->syy = 0.0 ;
         vp->sxy = 0.0 ; vp->sw  = 0.0 ; vp->npt = 0 ;
       }
     break ;

     case GA_MATCH_PEARCLP_SCALAR:
       if( vin != NULL ){
         AAmemcpy( vout , vin , sizeof(INCOR_pearclp) ) ;
       } else {
         INCOR_pearclp *vp = (INCOR_pearclp *)vout ;
         vp->sx  = 0.0 ; vp->sxx = 0.0 ;
         vp->sy  = 0.0 ; vp->syy = 0.0 ;
         vp->sxy = 0.0 ; vp->sw  = 0.0 ; vp->npt = 0 ;
         vp->xcbot = vp->xctop = vp->ycbot = vp->yctop = 0.0 ;
         vp->xdbot = vp->xdtop = vp->ydbot = vp->ydtop = 0.0 ;
       }
     break ;

     case GA_MATCH_MUTINFO_SCALAR:
     case GA_MATCH_CORRATIO_SCALAR:
     case GA_MATCH_NORMUTIN_SCALAR:
     case GA_MATCH_HELLINGER_SCALAR:
     case GA_MATCH_CRAT_SADD_SCALAR:
     case GA_MATCH_CRAT_USYM_SCALAR:
       if( vin != NULL ){
         INCOR_copyover_2Dhist( vin , vout ) ;
       } else {
         INCOR_2Dhist *tdh=(INCOR_2Dhist *)vout ; int nbp=1+tdh->nbin ;
         AAmemset(tdh->xc ,0,sizeof(float)*nbp) ;
         AAmemset(tdh->yc ,0,sizeof(float)*nbp) ;
         AAmemset(tdh->xyc,0,sizeof(float)*nbp*nbp) ;
         tdh->nww = 0.0f ;
       }
     break ;

   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Add data to an INCOR struct, for use in computing the 'correlation' later. */

void INCOR_addto( void *vin , int n , float *x , float *y , float *w )
{
ENTRY("INCOR_addto") ;

   if( vin == NULL || n <= 0 || x == NULL || y == NULL ) EXRETURN ;

   switch( INCOR_methcode(vin) ){

     case GA_MATCH_PEARSON_SCALAR:
       INCOR_addto_incomplete_pearson( n , x , y , w , vin ) ;
     break ;

     case GA_MATCH_PEARCLP_SCALAR:
       INCOR_addto_incomplete_pearclp( n , x , y , w , vin ) ;
     break ;

     case GA_MATCH_MUTINFO_SCALAR:
     case GA_MATCH_CORRATIO_SCALAR:
     case GA_MATCH_NORMUTIN_SCALAR:
     case GA_MATCH_HELLINGER_SCALAR:
     case GA_MATCH_CRAT_SADD_SCALAR:
     case GA_MATCH_CRAT_USYM_SCALAR:
       INCOR_addto_2Dhist( vin , n , x , y, w ) ;
     break ;

     case GA_MATCH_PEARSON_LOCALS:  /* 25 Jun 2014 */
     case GA_MATCH_PEARSON_LOCALA:{
       INCOR_localpearson *ilp = (INCOR_localpearson *)vin ;
       if( n != ilp->ibs->nx * ilp->ibs->ny * ilp->ibs->nz )
         ERROR_exit("INCOR_addto mismatch for localpearson: n=%d nx=%d ny=%d nz=%d",
                    n,ilp->ibs->nx,ilp->ibs->ny,ilp->ibs->nz) ;
       addto_INCORR_BLOK_set( ilp->ibs ,
                              0 , ilp->ibs->nx-1 ,
                              0 , ilp->ibs->ny-1 ,
                              0 , ilp->ibs->nz-1 , x , y , w ) ;
     }
     break ;

   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Evaluate the value of an INCOR struct, as currently constituted.
   * The data specified in arguments {n,x,y,w} is used (if n > 0), but is not
     added into the vin INCOR struct.
   * This feature lets you do correlations with a base set of unchanging
     data, plus a set of data that changes, and thence let you optimize the
     correlation as that varying data is munged around.
*//*--------------------------------------------------------------------------*/

double INCOR_evaluate( void *vin , int n , float *x , float *y , float *w )
{
   void *vtmp=NULL ; double val=0.0 ; int meth ;

ENTRY("INCOR_evaluate") ;

   if( vin == NULL ) RETURN(val) ;  /* should never transpire */

   meth = INCOR_methcode(vin) ;

   if( !(meth == GA_MATCH_PEARSON_LOCALA || meth == GA_MATCH_PEARSON_LOCALS) ){
     vtmp = INCOR_create( meth , NULL ) ;
     INCOR_copyover( vin , vtmp ) ;
     INCOR_addto( vtmp , n , x , y , w ) ;
   }

   switch( meth ){
     case GA_MATCH_PEARSON_SCALAR:   val = INCOR_incomplete_pearson(vtmp); break;
     case GA_MATCH_PEARCLP_SCALAR:   val = INCOR_incomplete_pearclp(vtmp); break;
     case GA_MATCH_MUTINFO_SCALAR:   val = INCOR_mutual_info(vtmp) ;       break;
     case GA_MATCH_NORMUTIN_SCALAR:  val = INCOR_norm_mutinf(vtmp) ;       break;
     case GA_MATCH_HELLINGER_SCALAR: val = INCOR_hellinger(vtmp) ;         break;
     case GA_MATCH_CORRATIO_SCALAR:  val = INCOR_corr_ratio(vtmp,0) ;      break;
     case GA_MATCH_CRAT_SADD_SCALAR: val = INCOR_corr_ratio(vtmp,2) ;      break;
     case GA_MATCH_CRAT_USYM_SCALAR: val = INCOR_corr_ratio(vtmp,1) ;      break;

     case GA_MATCH_PEARSON_LOCALS:  /* 25 Jun 2014 */
     case GA_MATCH_PEARSON_LOCALA:{
       INCOR_localpearson *ilp = (INCOR_localpearson *)vin ;
       if( n != ilp->nii * ilp->njj * ilp->nkk )
         ERROR_exit("INCOR_evaluate mismatch for localpearson: n=%d nii=%d njj=%d nkk=%d",
                    n,ilp->nii,ilp->njj,ilp->nkk ) ;
       val = correlate_INCORR_BLOK_set( ilp->ibs ,
                                        ilp->iibot , ilp->iitop ,
                                        ilp->jjbot , ilp->jjtop ,
                                        ilp->kkbot , ilp->kktop , x,y,w ) ;
     }
     break ;
   }

   if( vtmp != NULL ) INCOR_destroy(vtmp) ;
   RETURN(val) ;
}
