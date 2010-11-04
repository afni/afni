#include "rhdd.c"

/*----------------------------------------------------------------------------*/

typedef struct {
  float xc , yc , zc ;   /* centroid of RHDD */
  int num_ijk , num_in_mask ;
  int    *ijk ;
  float *vijk ;
} RHDD_plist ;

#undef  DESTROY_RHDD_plist
#define DESTROY_RHDD_plist(pl) do{ if( (pl)->ijk  != NULL ) free((pl)->ijk ) ; \
                                   if( (pl)->vijk != NULL ) free((pl)->vijk) ; \
                                   free(pl) ;                                  \
                               } while(0)

/*----------------------------------------------------------------------------*/

typedef struct {
  float asiz ;
  int      num_rhdd ;
  RHDD_plist **rhdd ;
} RHDD_array ;

#undef  DESTROY_RHDD_array
#define DESTROY_RHDD_array(pa) do{ if( (pa)->rhdd != NULL ) free((pa)->rhdd) ; \
                                   free(pa) ;                                  \
                               } while(0)

/*----------------------------------------------------------------------------*/

typedef struct {
  mat44 amat ;                /*    12 params */
  mat33 dmat ;                /*     9 params */
  int    nrh ;
  float *xrh , *yrh , *zrh ;  /* 3*nrh params */
  RHDD_array *rar ;
} RHDD_warp ;

/*----------------------------------------------------------------------------*/

RHDD_plist * RHDD_plist_create( int   nx , int   ny , int   nz ,
                                float dx , float dy , float dz ,
                                float asiz, float xcen, float ycen, float zcen,
                                byte *mask )
{
   int ii,jj,kk,ijk,num=0,nall=0,nin=0,ibot,itop,jbot,jtop,kbot,ktop,nxy=nx*ny ;
   RHDD_plist *rpl ;
   float xbot,xtop , ybot,ytop , zbot,ztop , xx,yy,zz , val ;

ENTRY("RHDD_plist_create") ;

#if 0
   if( nx < 3 || ny < 3 || nz < 3 ) RETURN(NULL) ;
   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;
   if( asiz <= dx || asiz <= dy || asiz <= dz ) RETURN(NULL) ;
#endif

   rpl = (RHDD_plist *)calloc(1,sizeof(RHDD_plist)) ;
   rpl->xc = xcen ; rpl->yc = ycen ; rpl->zc = zcen ;

   xbot = xcen - 2.0f*asiz; ibot = (int)(xbot/dx)  ; if( ibot < 0  ) ibot = 0 ;
   xtop = xcen + 2.0f*asiz; itop = (int)(xtop/dx)+2; if( itop > nx ) itop = nx;

   ybot = ycen - 2.0f*asiz; jbot = (int)(ybot/dy)  ; if( jbot < 0  ) jbot = 0 ;
   ytop = ycen + 2.0f*asiz; jtop = (int)(ytop/dy)+2; if( jtop > ny ) jtop = ny;

   zbot = zcen - 2.0f*asiz; kbot = (int)(zbot/dz)  ; if( kbot < 0  ) kbot = 0 ;
   ztop = zcen + 2.0f*asiz; ktop = (int)(ztop/dz)+2; if( ktop > nz ) ktop = nz;

   for( kk=kbot ; kk <= ktop ; kk++ ){
     zz = (kk*dz - zcen) / asiz ;
     for( jj=jbot ; jj <= jtop ; jj++ ){
       yy = (jj*dy - ycen) / asiz ;
       for( ii=ibot ; ii <= itop ; ii++ ){
         xx = (ii*dx - xcen) / asiz ;
         val = rhddc2(xx,yy,zz) ;
         if( fabsf(val) > 0.01f ){
           if( num == nall ){
             nall = 2*num + 16 ;
             rpl->ijk  = (int *  )realloc(rpl->ijk ,sizeof(int  )*nall) ;
             rpl->vijk = (float *)realloc(rpl->vijk,sizeof(float)*nall) ;
           }
           rpl->ijk [num] = ijk = ii + jj*nx + kk*nxy ;
           rpl->vijk[num] = val ; num++ ;
           if( mask == NULL || mask[ijk] != 0 ) nin++ ;
         }
       }
     }
   }

   rpl->num_ijk = num ; rpl->num_in_mask = nin ;

        if( num == 0    ){ free(rpl) ; rpl = NULL ; }
   else if( num <  nall ){
     rpl->ijk  = (int *  )realloc(rpl->ijk ,sizeof(int  )*num) ;
     rpl->vijk = (float *)realloc(rpl->vijk,sizeof(float)*num) ;
   }

   RETURN(rpl) ;
}

/*----------------------------------------------------------------------------*/

RHDD_array * RHDD_array_create( int   nx , int   ny , int   nz ,
                                float dx , float dy , float dz ,
                                float asiz , byte *mask         )
{
   int pb,pt , qb,qt , rb,rt , pp,qq,rr , np,nq,nr,npq,npqr , nmax,ngood ;
   float ah=0.5f*asiz , xt,yt,zt ;
   THD_mat33 latmat , invlatmat ; THD_fvec3 pqr , xyz ;
   RHDD_plist *rpl ; RHDD_array *rar ;

ENTRY("RHDD_array_create") ;

   if( nx < 9 || ny < 9 || nz < 9 ) RETURN(NULL) ;
   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;
   xt = MAX(dx,dy) ; xt = MAX(xt,dz) ;
   if( asiz < 3.0f*xt ) RETURN(NULL) ;

   /* find range of (p,q,r) indexes needed to cover volume,
      by checking out all 7 corners besides (0,0,0) (where p=q=r=0) */

   LOAD_MAT( latmat , -ah,ah,ah , ah,-ah,ah , ah,ah,-ah ) ;
   invlatmat = MAT_INV(latmat) ;

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

   pb-- ; qb-- ; rb-- ;  /* push outwards, for luck */
   pt++ ; qt++ ; rt++ ;

   /* Lattice index range is (p,q,r) = (pb..pt,qb..qt,rb..rt) inclusive */

   np = pt-pb+1 ;                /* number of p values to consider */
   nq = qt-qb+1 ; npq  = np*nq  ;
   nr = rt-rb+1 ; npqr = npq*nr ;

   rar = (RHDD_array *)calloc(1,sizeof(RHDD_array)) ;
   rar->asiz     = asiz ;
   rar->num_rhdd = npqr ;
   rar->rhdd     = (RHDD_plist **)calloc(npqr,sizeof(RHDD_plist *)) ;

   for( nmax=0,rr=rb ; rr <= rt ; rr++ ){
    for( qq=qb ; qq <= qt ; qq++ ){
      for( pp=pb ; pp <= pt ; pp++ ){
        LOAD_FVEC3(pqr,pp,qq,rr) ; xyz = MATVEC(latmat,pqr) ;
        rar->rhdd[(pp-pb)+(qq-qb)*np+(rr-rb)*npq] = rpl =
          RHDD_plist_create( nx,ny,nz , dx,dy,dz , asiz ,
                             xyz.xyz[0] , xyz.xyz[1] , xyz.xyz[2] , mask ) ;
        if( rpl != NULL && rpl->num_in_mask > nmax ) nmax = rpl->num_in_mask ;
   }}}

   if( nmax <= 9 ){
     for( pp=0 ; pp < npqr ; pp++ ){
       if( rar->rhdd[pp] != NULL ) DESTROY_RHDD_plist(rar->rhdd[pp]) ;
     }
     DESTROY_RHDD_array(rar) ; RETURN(NULL) ;
   }

   nmax = (int)(0.3456789f*nmax) ;
   for( ngood=pp=0 ; pp < npqr ; pp++ ){
     if( rar->rhdd[pp] != NULL ){
       if( rar->rhdd[pp]->num_in_mask < nmax ){
         DESTROY_RHDD_plist(rar->rhdd[pp]) ; rar->rhdd[pp] = NULL ;
       } else {
         ngood++ ;
       }
     }
   }

   if( ngood < npqr ){
     RHDD_array *qar ;
     qar = (RHDD_array *)calloc(1,sizeof(RHDD_array)) ;
     qar->asiz     = asiz ;
     qar->num_rhdd = ngood ;
     qar->rhdd     = (RHDD_plist **)calloc(ngood,sizeof(RHDD_plist *)) ;
     for( pp=qq=0 ; pp < npqr ; pp++ ){
       if( rar->rhdd[pp] != NULL ) qar->rhdd[qq++] = rar->rhdd[pp] ;
     }
     DESTROY_RHDD_array(rar) ; rar = qar ;
   }

   RETURN(rar) ;
}
