#include "mrilib.h"

/* prototypes */

int THD_conformant_dataxes( THD_dataxes *ax , THD_dataxes *bx ) ;
THD_dataxes * THD_superset_dataxes( THD_dataxes *ax , THD_dataxes *bx ) ;
static int_pair zpadax_pm( int nx_super , float xorg_super ,
                           int nx_input , float xorg_input , float dx ) ;
int THD_conformist( int ndset , THD_3dim_dataset **dset ) ;

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int ndset=0 , ii ;
   THD_3dim_dataset **dset ;

mainENTRY("3dConformist") ;

   if( argc < 3 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
       "Program 3dConformist reads in a collection of datasets and\n"
       "zero pads them to the same size.  The input dataset ARE ALTERED!\n"
     ) ;
     exit(0) ;
   }

   ndset = argc-1 ;
   dset  = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)*ndset) ;
   for( ii=0 ; ii < ndset ; ii++ ){
     dset[ii] = THD_open_dataset(argv[ii+1]) ; CHECK_OPEN_ERROR(dset[ii],argv[ii+1]) ;
   }

   ii = THD_conformist(ndset,dset) ;

   switch(ii){
     default: INFO_message ("3dConformist: Re-wrote %d datasets",ii) ; break ;
     case -1: ERROR_message("3dConformist: bad input")               ; break ;
     case -2: ERROR_message("3dConformist: bad inputs")              ; break ;
     case -3: ERROR_message("3dConformist: can't match grids")       ; break ;
   }

   exit(0) ;
}

/*----------------------------------------------------------------------------*/

int THD_conformist( int ndset , THD_3dim_dataset **dset )
{
   int iset , xpad_m,xpad_p , ypad_m,ypad_p , zpad_m,zpad_p , nwrit ;
   THD_dataxes *cx , *dx ;
   THD_3dim_dataset *qset ;
   int_pair pm ;

ENTRY("THD_conformist") ;

   /* check for good inputs */

   if( ndset <= 0 || dset == NULL ) RETURN(-1) ;
   for( iset=0 ; iset < ndset ; iset++ )
     if( !ISVALID_DSET(dset[iset]) ) RETURN(-2) ;

   /* check if all inputs are on the same grid (i.e., no work to do) */

   for( iset=1 ; iset < ndset ; iset++ ){
     if( ! EQUIV_DATAXES(dset[0]->daxes,dset[iset]->daxes) ) break ;
   }
   if( iset == ndset ) RETURN(0) ;

   /* construct the dataxes that encloses all the input datasets */

   cx = THD_superset_dataxes( dset[0]->daxes , dset[1]->daxes ) ;
   if( cx == NULL ) RETURN(-3) ;

   for( iset=2 ; iset < ndset ; iset++ ){
     dx = THD_superset_dataxes( cx , dset[iset]->daxes ) ;
     if( dx == NULL ){ myXtFree(cx) ; RETURN(-3) ; }
     myXtFree(cx) ; cx = dx ; dx = NULL ;
   }

   /* now, re-create and re-write all datasets */

   fprintf(stderr,"3dConformist loop: ") ;
   for( nwrit=iset=0 ; iset < ndset ; iset++ ){
     fprintf(stderr,".") ;
     if( EQUIV_DATAXES(cx,dset[iset]->daxes) ) continue ; /* already OK */
     pm = zpadax_pm( cx->nxx , cx->xxorg ,
                     dset[iset]->daxes->nxx , dset[iset]->daxes->xxorg ,
                     cx->xxdel ) ;
     xpad_m = pm.i ; xpad_p = pm.j ;
     pm = zpadax_pm( cx->nyy , cx->yyorg ,
                     dset[iset]->daxes->nyy , dset[iset]->daxes->yyorg ,
                     cx->yydel ) ;
     ypad_m = pm.i ; ypad_p = pm.j ;
     pm = zpadax_pm( cx->nzz , cx->zzorg ,
                     dset[iset]->daxes->nzz , dset[iset]->daxes->zzorg ,
                     cx->zzdel ) ;
     zpad_m = pm.i ; zpad_p = pm.j ;
     qset = THD_zeropad( dset[iset] ,
                         xpad_m,xpad_p , ypad_m,ypad_p , zpad_m,zpad_p ,
                         "BertieWooster" , ZPAD_PURGE | ZPAD_IJK ) ;
     if( qset == NULL ){
       ERROR_message("3dConformist: skipping dataset %s",DSET_HEADNAME(dset[iset])) ;
       continue ;
     }
     qset->idcode = dset[iset]->idcode ;
     EDIT_dset_items( qset , ADN_prefix , DSET_PREFIX(dset[iset]) , ADN_none ) ;
     THD_delete_3dim_dataset( dset[iset] , True ) ;
     DSET_overwrite(qset) ; DSET_unload(qset) ; dset[iset] = qset ; nwrit++ ;
   }
   fprintf(stderr,"\n") ;

   RETURN(nwrit) ;
}

/*----------------------------------------------------------------------------*/

int THD_conformant_dataxes( THD_dataxes *ax , THD_dataxes *bx )
{
   float xo,yo,zo ;

   if( ax->xxorient != bx->xxorient ||
       ax->yyorient != bx->yyorient ||
       ax->zzorient != bx->zzorient   ) return 0 ;

   if( fabsf(ax->xxdel-bx->xxdel) > 0.001f ) return 0 ;
   if( fabsf(ax->yydel-bx->yydel) > 0.001f ) return 0 ;
   if( fabsf(ax->zzdel-bx->zzdel) > 0.001f ) return 0 ;

   xo = (ax->xxorg - bx->xxorg) / ax->xxdel ;
   yo = (ax->yyorg - bx->yyorg) / ax->yydel ;
   zo = (ax->zzorg - bx->zzorg) / ax->zzdel ;

   if( fabsf(xo-rintf(xo)) > 0.01f ||
       fabsf(yo-rintf(yo)) > 0.01f ||
       fabsf(zo-rintf(zo)) > 0.01f   ) return 0 ;

   return 1 ;
}

/*----------------------------------------------------------------------------*/

static int_pair zpadax_pm( int nx_super , float xorg_super ,
                           int nx_input , float xorg_input , float dx )
{
   int_pair pm ; float ts , ti ;

   ts   = xorg_super + nx_super * dx ;
   ti   = xorg_input + nx_input * dx ;
   pm.i = (int)rintf((xorg_input-xorg_super)/dx) ;
   pm.j = (int)rintf((ts-ti)/dx) ;

   return pm ;
}

/*----------------------------------------------------------------------------*/

THD_dataxes * THD_superset_dataxes( THD_dataxes *ax , THD_dataxes *bx )
{
   THD_dataxes *cx ;
   float dx,dy,dz , axo,ayo,azo , bxo,byo,bzo , ae,be ;
   int nxa,nya,nza , nxb,nyb,nzb , ndif ;
   float cxo,cyo,czo ;
   int   nxc,nyc,nzc ;

   if( !THD_conformant_dataxes(ax,bx) ) return NULL ;

   /* create new dataxes as copy of first one */

   cx = myXtNew(THD_dataxes) ; *cx = *ax ;
   cx->parent = NULL ;
   /* if( EQUIV_DATAXES(ax,bx) ) return cx ; */

   /* load some variables from the input structs */

   dx  = ax->xxdel ; dy  = ax->yydel ; dz  = ax->zzdel ;  /* same for ax & bx */
   axo = ax->xxorg ; ayo = ax->yyorg ; azo = ax->zzorg ;  /* ax origins */
   bxo = bx->xxorg ; byo = bx->yyorg ; bzo = bx->zzorg ;  /* bx origins */
   nxa = ax->nxx   ; nya = ax->nyy   ; nza = ax->nzz   ;  /* ax grid lengths */
   nxb = bx->nxx   ; nyb = bx->nyy   ; nzb = bx->nzz   ;  /* bx grid lengths */

   /* extend origins to the outermost */

   ndif = (int)rintf((axo-bxo)/dx) ; cxo = (ndif <= 0) ? axo : bxo ;
   ndif = (int)rintf((ayo-byo)/dy) ; cyo = (ndif <= 0) ? ayo : byo ;
   ndif = (int)rintf((azo-bzo)/dz) ; czo = (ndif <= 0) ? azo : bzo ;

   cx->xxorg = cxo ; cx->yyorg = cyo ; cx->zzorg = czo ;

   /* extend grid lengths to the outermost */

   ae = axo + nxa*dx ; be = bxo + nxb*dx ;
   ndif = (int)rintf((ae-be)/dx) ; if( ndif < 0 ) ae = be ;
   nxc  = (int)rintf((ae-cxo)/dx) ;

   ae = ayo + nya*dy ; be = byo + nyb*dy ;
   ndif = (int)rintf((ae-be)/dy) ; if( ndif < 0 ) ae = be ;
   nyc  = (int)rintf((ae-cyo)/dy) ;

   ae = azo + nza*dz ; be = bzo + nzb*dz ;
   ndif = (int)rintf((ae-be)/dz) ; if( ndif < 0 ) ae = be ;
   nzc  = (int)rintf((ae-czo)/dz) ;

   cx->nxx   = nxc ; cx->nyy   = nyc ; cx->nzz   = nzc ;

#if 0
fprintf(stderr,"\n") ;
INFO_message("ax: nxyz=%d %d %d  org=%g %g %g  del=%g %g %g",ax->nxx,ax->nyy,ax->nzz,ax->xxorg,ax->yyorg,ax->zzorg,ax->xxdel,ax->yydel,ax->zzdel) ;
INFO_message("bx: nxyz=%d %d %d  org=%g %g %g  del=%g %g %g",bx->nxx,bx->nyy,bx->nzz,bx->xxorg,bx->yyorg,bx->zzorg,bx->xxdel,bx->yydel,bx->zzdel) ;
INFO_message("cx: nxyz=%d %d %d  org=%g %g %g  del=%g %g %g",cx->nxx,cx->nyy,cx->nzz,cx->xxorg,cx->yyorg,cx->zzorg,cx->xxdel,cx->yydel,cx->zzdel) ;
#endif

   /* fix the matrices etc */

   LOAD_ZERO_MAT(cx->to_dicomm) ;
   THD_daxes_to_mat44(cx) ;
   THD_set_daxes_bbox(cx) ;
   cx->ijk_to_dicom_real = cx->ijk_to_dicom ;

   return cx ;
}
