#include "mrilib.h"

/*----------------------------------------------------------------------------*/
#undef  TRIPROD
#define TRIPROD(ax,ay,az,bx,by,bz,cx,cy,cz) ( (ax)*((by)*(cz)-(bz)*(cy)) \
                                             +(bx)*((cy)*(az)-(cz)*(ay)) \
                                             +(cx)*((ay)*(bz)-(az)*(by))  )

#undef  DA
#define DA(p,q) (p.a-q.a)
#undef  DB
#define DB(p,q) (p.b-q.b)
#undef  DC
#define DC(p,q) (p.c-q.c)

static float hexahedron_volume( float_triple x0 , float_triple x1 , float_triple x2 ,
                                float_triple x3 , float_triple x4 , float_triple x5 ,
                                float_triple x6 , float_triple x7                    )
{
   float xa,ya,za , xb,yb,zb , xc,yc,zc , vol ;

   xa = DA(x7,x1)+DA(x6,x0); ya = DB(x7,x1)+DB(x6,x0); za = DC(x7,x1)+DC(x6,x0);
   xb = DA(x7,x2)          ; yb = DB(x7,x2)          ; zb = DC(x7,x2) ;
   xc = DA(x3,x0)          ; yc = DB(x3,x0)          ; zc = DC(x3,x0) ;
   vol = TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   xa = DA(x6,x0)          ; ya = DB(x6,x0)          ; za = DC(x6,x0) ;
   xb = DA(x7,x2)+DA(x5,x0); yb = DB(x7,x2)+DB(x5,x0); zb = DC(x7,x2)+DC(x5,x0);
   xc = DA(x7,x4)          ; yc = DB(x7,x4)          ; zc = DC(x7,x4) ;
   vol += TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   xa = DA(x7,x1)          ; ya = DB(x7,x1)          ; za = DC(x7,x1) ;
   xb = DA(x5,x0)          ; yb = DB(x5,x0)          ; zb = DC(x5,x0) ;
   xc = DA(x7,x4)+DA(x3,x0); yc = DB(x7,x4)+DB(x3,x0); zc = DC(x7,x4)+DC(x3,x0);
   vol += TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   return vol ;
}

#undef  MINBLUR
#define MINBLUR 1.234567f

/*----------------------------------------------------------------------------*/
/* fim   = image to warp
   wimar = contains 3 images with index warps
   mode  = interpolation method
*/

MRI_IMAGE * GW_warpim( MRI_IMAGE *fim, MRI_IMARR *wimar , int mode )
{
   float *iar,*jar,*kar,*gar ; int npt ; MRI_IMAGE *gim ;

/* ININFO_message("Enter GW_warpim") ; */

   iar = MRI_FLOAT_PTR( IMARR_SUBIM(wimar,0) ) ;
   jar = MRI_FLOAT_PTR( IMARR_SUBIM(wimar,1) ) ;
   kar = MRI_FLOAT_PTR( IMARR_SUBIM(wimar,2) ) ;
   npt = fim->nvox ;
   gim = mri_new_vol( fim->nx , fim->ny , fim->nz , MRI_float ) ;
   gar = MRI_FLOAT_PTR(gim) ;

   switch( mode ){
     case MRI_NN:      GA_interp_NN     (fim,npt,iar,jar,kar,gar) ; break ;

     default:
     case MRI_LINEAR:  GA_interp_linear (fim,npt,iar,jar,kar,gar) ; break ;

     case MRI_CUBIC:   GA_interp_cubic  (fim,npt,iar,jar,kar,gar) ; break ;

     case MRI_QUINTIC: GA_interp_quintic(fim,npt,iar,jar,kar,gar) ; break ;
   }

/* ININFO_message(" exit GW_warpim") ; */
   return gim ;
}

/*----------------------------------------------------------------------------*/

void GW_blurim( MRI_IMAGE *qim , float blur )
{
   blur = MAX(blur,MINBLUR) ; blur = FWHM_TO_SIGMA(blur) ;
   FIR_blur_volume( qim->nx,qim->ny,qim->nz , 1.0f,1.0f,1.0f ,
                                              MRI_FLOAT_PTR(qim) , blur ) ;
   return ;
}

/*----------------------------------------------------------------------------*/

MRI_IMARR * GW_differentiate( MRI_IMAGE *iim , float blur )
{
   MRI_IMARR *outar ;
   MRI_IMAGE *qim , *qxim , *qyim , *qzim ;
   float     *qar , *qxar , *qyar , *qzar ;
   int nx,ny,nz,nxy,nxyz , ii,jj,kk , nx1,ny1,nz1 , km,kp,jm,jp,im,ip ;

/* INFO_message("Enter GW_differentiate") ; */

   nx = iim->nx ; nx1 = nx-1 ;
   ny = iim->ny ; ny1 = ny-1 ;
   nz = iim->nz ; nz1 = nz-1 ; nxy = nx*ny ; nxyz = nx*ny*nz ;

   qim = mri_copy(iim) ; GW_blurim(qim,blur) ; qar = MRI_FLOAT_PTR(qim) ;

   qxim = mri_new_vol(nx,ny,nz,MRI_float) ; qxar = MRI_FLOAT_PTR(qxim) ;
   qyim = mri_new_vol(nx,ny,nz,MRI_float) ; qyar = MRI_FLOAT_PTR(qyim) ;
   qzim = mri_new_vol(nx,ny,nz,MRI_float) ; qzar = MRI_FLOAT_PTR(qzim) ;

#undef  IJK
#define IJK(i,j,k) ((i)+(j)*nx+(k)*nxy)
   for( kk=0 ; kk < nz ; kk++ ){
     km = kk-1 ; if( km < 0   ) km = 0   ;
     kp = kk+1 ; if( kp > nz1 ) kp = nz1 ;
     for( jj=0 ; jj < ny ; jj++ ){
       jm = jj-1 ; if( jm < 0   ) jm = 0   ;
       jp = jj+1 ; if( jp > ny1 ) jp = ny1 ;
       for( ii=0 ; ii < nx ; ii++ ){
         im = ii-1 ; if( im < 0   ) im = 0   ;
         ip = ii+1 ; if( ip > nx1 ) ip = nx1 ;
         qxar[IJK(ii,jj,kk)] = 0.5f * ( qar[IJK(ip,jj,kk)] - qar[IJK(im,jj,kk)] ) ;
         qyar[IJK(ii,jj,kk)] = 0.5f * ( qar[IJK(ii,jp,kk)] - qar[IJK(ii,jm,kk)] ) ;
         qzar[IJK(ii,jj,kk)] = 0.5f * ( qar[IJK(ii,jj,kp)] - qar[IJK(ii,jj,km)] ) ;
   }}}
#undef IJK

   INIT_IMARR(outar) ;
   ADDTO_IMARR(outar,qim)  ; ADDTO_IMARR(outar,qxim) ;
   ADDTO_IMARR(outar,qyim) ; ADDTO_IMARR(outar,qzim) ; return outar ;
}

/*----------------------------------------------------------------------------*/
/* jim    = base image
   iimar  = 4 images: [0] = source image, [1..3] = derivatives of source
   wimar  = 3 images with current index warp
   dblur  = blurring to apply to deltim
   output = 3 images with additional index warp
*/

MRI_IMARR * GW_deltim( MRI_IMAGE *jim  , MRI_IMARR *iimar ,
                       MRI_IMARR *wimar, float blur        )
{
   MRI_IMARR *outar;
   MRI_IMAGE *iim , *ixim , *iyim , *izim , *tim ;
   float     *iwar, *ixar , *iyar , *izar ;
   float *iar , *jar ;
   int nx,ny,nz,nxy,nxyz , ii,jj,kk , nx1,ny1,nz1 , qoff ;
   float val,vmax,gg , sij,sjj ;

/* INFO_message("Enter GW_deltim") ; */

   nx = jim->nx ; ny = jim->ny ; nz = jim->nz ; nxy = nx*ny ; nxyz = nxy*nz ;
   nx1 = nx-1 ; ny1 = ny-1 ; nz1 = nz-1 ;

   iim  = GW_warpim( IMARR_SUBIM(iimar,0) , wimar , MRI_LINEAR ) ;
   ixim = GW_warpim( IMARR_SUBIM(iimar,1) , wimar , MRI_LINEAR ) ;
   iyim = GW_warpim( IMARR_SUBIM(iimar,2) , wimar , MRI_LINEAR ) ;
   izim = GW_warpim( IMARR_SUBIM(iimar,3) , wimar , MRI_LINEAR ) ;

   iar  = MRI_FLOAT_PTR(iim) ;
   ixar = MRI_FLOAT_PTR(ixim) ;
   iyar = MRI_FLOAT_PTR(iyim) ;
   izar = MRI_FLOAT_PTR(izim) ;

   jar  = MRI_FLOAT_PTR(jim) ;

   /* compute scale factor to convert jar values to iar values */

   sij = sjj = 0.0f ;
   for( ii=0 ; ii < nxyz ; ii++ ){
     sij += jar[ii]*iar[ii] ; sjj += jar[ii]*jar[ii] ;
   }
   if( sjj == 0.0f ) sjj = 1.0f ;
   sij = sij / sjj ;

   for( ii=0 ; ii < nxyz ; ii++ ){
     gg = ixar[ii]*ixar[ii] + iyar[ii]*iyar[ii] + izar[ii]*izar[ii] ;
     if( gg > 0.0f ){
       val  = sij*jar[ii] - iar[ii] ;
       val /= ( gg + val*val ) ;
       ixar[ii] *= val ; iyar[ii] *= val ; izar[ii] *= val ;
     }
   }
   mri_free(iim) ;

   tim=GW_warpim(ixim,wimar,MRI_LINEAR); mri_free(ixim); ixim=tim; ixar=MRI_FLOAT_PTR(ixim);
   tim=GW_warpim(iyim,wimar,MRI_LINEAR); mri_free(iyim); iyim=tim; iyar=MRI_FLOAT_PTR(iyim);
   tim=GW_warpim(izim,wimar,MRI_LINEAR); mri_free(izim); izim=tim; izar=MRI_FLOAT_PTR(izim);

   GW_blurim(ixim,blur) ; GW_blurim(iyim,blur) ; GW_blurim(izim,blur) ;

   vmax = 0.0f ;
   for( ii=0 ; ii < nxyz ; ii++ ){
     val = ixar[ii]*ixar[ii] + iyar[ii]*iyar[ii] + izar[ii]*izar[ii] ;
     if( val > vmax ) vmax = val ;
   }
   if( vmax == 0.0f ) vmax = 1.0f ; /* should not happen */
   val = 1.0f / sqrtf(vmax) ;
   for( ii=0 ; ii < nxyz ; ii++ ){
     ixar[ii] *= val ; iyar[ii] *= val ; izar[ii] *= val ;
   }

   INIT_IMARR(outar) ;
   ADDTO_IMARR(outar,ixim) ; ADDTO_IMARR(outar,iyim) ; ADDTO_IMARR(outar,izim) ;
   return outar ;
}

/*----------------------------------------------------------------------------*/
/* iim    = source image
   jim    = base image
   wimar  = base index warp [3]
   deltar = additional index warp [3]
   ds     = amount of deltar to apply
*/

float GW_diffval( MRI_IMAGE *iim   , MRI_IMAGE *jim    ,
                  MRI_IMARR *wimar , MRI_IMARR *deltar , float ds )
{
   int ii , nxyz ; float vv,val , sij,sjj ;
   MRI_IMAGE *wim ; float *war , *jar ;
   MRI_IMARR *qimar ;

   nxyz = iim->nx * iim->ny * iim->nz ;
   jar  = MRI_FLOAT_PTR(jim) ;

/* INFO_message("Enter GW_diffval") ; */

   if( ds == 0.0f ){
     qimar = wimar ;
   } else {
     MRI_IMAGE *qxim,*qyim,*qzim ;
     float *qxar,*qyar,*qzar , *wxar,*wyar,*wzar , *dxar,*dyar,*dzar ;
     INIT_IMARR(qimar) ;
     qxim = mri_new_vol(iim->nx,iim->ny,iim->nz,MRI_float); qxar = MRI_FLOAT_PTR(qxim);
     qyim = mri_new_vol(iim->nx,iim->ny,iim->nz,MRI_float); qyar = MRI_FLOAT_PTR(qyim);
     qzim = mri_new_vol(iim->nx,iim->ny,iim->nz,MRI_float); qzar = MRI_FLOAT_PTR(qzim);
     wxar = MRI_FLOAT_PTR(IMARR_SUBIM(wimar ,0)) ;
     wyar = MRI_FLOAT_PTR(IMARR_SUBIM(wimar ,1)) ;
     wzar = MRI_FLOAT_PTR(IMARR_SUBIM(wimar ,2)) ;
     dxar = MRI_FLOAT_PTR(IMARR_SUBIM(deltar,0)) ;
     dyar = MRI_FLOAT_PTR(IMARR_SUBIM(deltar,1)) ;
     dzar = MRI_FLOAT_PTR(IMARR_SUBIM(deltar,2)) ;
     for( ii=0 ; ii < nxyz ; ii++ ){
       qxar[ii] = wxar[ii] + ds*dxar[ii] ;
       qyar[ii] = wyar[ii] + ds*dyar[ii] ;
       qzar[ii] = wzar[ii] + ds*dzar[ii] ;
     }
     ADDTO_IMARR(qimar,qxim) ; ADDTO_IMARR(qimar,qyim) ; ADDTO_IMARR(qimar,qzim) ;
   }

   wim = GW_warpim( iim , qimar , MRI_LINEAR ) ; war = MRI_FLOAT_PTR(wim) ;
   if( qimar != wimar ) DESTROY_IMARR(qimar) ;

   sij = sjj = 0.0f ;
   for( ii=0 ; ii < nxyz ; ii++ ){
     sij += jar[ii]*war[ii] ; sjj += jar[ii]*jar[ii] ;
   }
/* ININFO_message("  compute sij=%g,sjj=%g",sij,sjj) ; */
   if( sjj == 0.0f ) sjj = 1.0f ;
   sij = sij / sjj ;

   vv = 0.0f ; jar = MRI_FLOAT_PTR(jim) ; war = MRI_FLOAT_PTR(wim) ;
   for( ii=0 ; ii < nxyz ; ii++ ){
    val = sij*jar[ii] - war[ii] ; vv += val*val ;
   }
   mri_free(wim) ;

/* ININFO_message("GW_diffval(%f) = %g",ds,vv) ; */
   return vv ;
}

/*----------------------------------------------------------------------------*/

float GW_best_step( MRI_IMAGE *iim   , MRI_IMAGE *jim   ,
                    MRI_IMARR *wimar , MRI_IMARR *deltar )
{
   float vbot , vtop , vmid , db=0.0f, dt=0.16f , dm , vbm,vmt ;
   int ii ;

/* INFO_message("Enter GW_best_step") ; */

   vbot = GW_diffval( iim , jim , wimar , deltar , db ) ;
   vtop = GW_diffval( iim , jim , wimar , deltar , dt ) ;

#if 0
   for( ii=0 ; ii < 2 ; ii++ ){
     dm   = 0.5f*(db+dt) ;
     vmid = GW_diffval( iim , jim , wimar , deltar , dm ) ;
     vbm  = MIN(vbot,vmid) ;
     vmt  = MIN(vmid,vtop) ;
     if( vbm <= vmt ){ dt = dm ; vtop = vmid ; }
     else            { db = dm ; vbot = vmid ; }
   }
#endif

   return ((vtop < vbot) ? dt : db) ;
}

/*----------------------------------------------------------------------------*/
/* Scale jim (in place) to match iim */

float GW_scaleim( MRI_IMAGE *iim , MRI_IMAGE *jim )
{
   float iclip , jclip , aa ; int ii , nvox ;
   float *jar ;

   iclip = THD_cliplevel( iim , 0.4f ) ;
   jclip = THD_cliplevel( iim , 0.4f ) ;
   if( jclip <= 0.0f || iclip <= 0.0f ) return 0.0f ;

   aa   = iclip / jclip ;
   nvox = jim->nvox ;
   jar  = MRI_FLOAT_PTR(jim) ;
   for( ii=0 ; ii < nvox ; ii++ ) jar[ii] *= aa ;
   return aa ;
}

/*----------------------------------------------------------------------------*/

static float       blur = 0.0f ;
static MRI_IMARR *iimar = NULL ;
static MRI_IMARR *wimar = NULL ;
static MRI_IMAGE *jim   = NULL ;

static THD_3dim_dataset *qset = NULL ;

void GW_setup( MRI_IMAGE *inim , MRI_IMAGE *bsim , float iblur , float jblur )
{
   MRI_IMAGE *wim ; float *war ;
   int nx,ny,nz,nxy,nxyz , ii,jj,kk,qq ;

   nx = inim->nx ;
   ny = inim->ny ;
   nz = inim->nz ; nxy = nx*ny ; nxyz = nx*ny*nz ;

   if( iimar != NULL ) DESTROY_IMARR(iimar) ;
   if( wimar != NULL ) DESTROY_IMARR(wimar) ;
   if( jim   != NULL ) mri_free(jim) ;

   blur  = MAX(iblur,MINBLUR) ;
   iimar = GW_differentiate( inim , blur ) ;
   jim   = mri_copy(bsim) ;
   blur  = MAX(jblur,MINBLUR) ;
   GW_blurim(jim,blur) ;
   (void)GW_scaleim( IMARR_SUBIM(iimar,0) , jim ) ;

{ MRI_IMAGE *tim = mri_copy(jim) ;
  EDIT_dset_items( qset , ADN_prefix,"zzbase" , ADN_none ) ;
  EDIT_substitute_brick( qset , 0 , MRI_float , MRI_FLOAT_PTR(tim) ) ;
  DSET_write(qset) ; WROTE_DSET(qset) ;
}

   INIT_IMARR(wimar) ;
   wim = mri_new_vol(jim->nx,jim->ny,jim->nz,MRI_float); ADDTO_IMARR(wimar,wim);
   war = MRI_FLOAT_PTR(wim) ;
   for( qq=kk=0 ; kk < nz ; kk++ )
     for( jj=0 ; jj < ny ; jj++ )
       for( ii=0 ; ii < nx ; ii++,qq++ ) war[qq] = ii ;

   wim = mri_new_vol(jim->nx,jim->ny,jim->nz,MRI_float); ADDTO_IMARR(wimar,wim);
   war = MRI_FLOAT_PTR(wim) ;
   for( qq=kk=0 ; kk < nz ; kk++ )
     for( jj=0 ; jj < ny ; jj++ )
       for( ii=0 ; ii < nx ; ii++,qq++ ) war[qq] = jj ;

   wim = mri_new_vol(jim->nx,jim->ny,jim->nz,MRI_float); ADDTO_IMARR(wimar,wim);
   war = MRI_FLOAT_PTR(wim) ;
   for( qq=kk=0 ; kk < nz ; kk++ )
     for( jj=0 ; jj < ny ; jj++ )
       for( ii=0 ; ii < nx ; ii++,qq++ ) war[qq] = kk ;

   return ;
}

/*----------------------------------------------------------------------------*/

float GW_iterate(void)
{
   MRI_IMARR *deltar ; float ds , qblur ;

/* INFO_message("Enter GW_iterate") ; */

   qblur = 1.444f*blur ; qblur = MAX(qblur,14.444f) ;
   deltar = GW_deltim( jim , iimar , wimar , qblur ) ;
   ds     = GW_best_step( IMARR_SUBIM(iimar,0) , jim , wimar , deltar ) ;

ININFO_message("best_step = %f",ds) ;

   if( ds != 0.0f ){
     float *wxar = MRI_FLOAT_PTR(IMARR_SUBIM(wimar ,0)) ;
     float *wyar = MRI_FLOAT_PTR(IMARR_SUBIM(wimar ,1)) ;
     float *wzar = MRI_FLOAT_PTR(IMARR_SUBIM(wimar ,2)) ;
     float *dxar = MRI_FLOAT_PTR(IMARR_SUBIM(deltar,0)) ;
     float *dyar = MRI_FLOAT_PTR(IMARR_SUBIM(deltar,1)) ;
     float *dzar = MRI_FLOAT_PTR(IMARR_SUBIM(deltar,2)) ;
     int ii , nxyz = jim->nvox ;
     for( ii=0 ; ii < nxyz ; ii++ ){
       wxar[ii] += ds*dxar[ii] ;
       wyar[ii] += ds*dyar[ii] ;
       wzar[ii] += ds*dzar[ii] ;
     }

/* debug output */

{ MRI_IMAGE *tim = GW_warpim(IMARR_SUBIM(iimar,0),wimar,MRI_LINEAR) ;
  char prefix[64] ;
  static int nite=0 ;
  nite++ ; sprintf(prefix,"zziter%04d",nite) ;
  EDIT_dset_items( qset , ADN_prefix,prefix , ADN_none ) ;
  EDIT_substitute_brick( qset , 0 , MRI_float , MRI_FLOAT_PTR(tim) ) ;
  DSET_write(qset) ; WROTE_DSET(qset) ; mri_clear_data_pointer(tim) ; mri_free(tim) ;
}
   }

   DESTROY_IMARR(deltar) ; return ds ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *iset , *bset , *oset ;
   MRI_IMAGE *iim , *bim ; int ii,iarg=1 ; float ds , bblur=0.0f,sblur=4.9f ;

   if( argc < 3 ){ printf("gwarp [-blur b s] base source\n") ; exit(0) ; }

INFO_message("gwarp is NOT a production program: it is merely for testing!") ;

   if( strcmp(argv[iarg],"-blur") == 0 ){
     bblur = strtod(argv[++iarg],NULL) ;
     sblur = strtod(argv[++iarg],NULL) ; iarg++ ;
   }

   bset = THD_open_dataset(argv[iarg++]) ; if( !ISVALID_DSET(bset) ) ERROR_exit("bset") ;
   DSET_load(bset) ; bim = mri_to_float(DSET_BRICK(bset,0)) ; DSET_unload(bset) ;

   iset = THD_open_dataset(argv[iarg++]) ; if( !ISVALID_DSET(iset) ) ERROR_exit("iset") ;
   DSET_load(iset) ; iim = mri_to_float(DSET_BRICK(iset,0)) ; DSET_unload(iset) ;

   qset = EDIT_empty_copy(iset) ;
   EDIT_dset_items( qset , ADN_nvals,1 , ADN_none ) ;

   GW_setup(iim,bim,sblur,bblur) ; mri_free(bim) ;

   for( ii=0 ; ii < 499 ; ii++ ){
     ds = GW_iterate() ; if( ds == 0.0f ) break ;
   }

   bim = GW_warpim(iim,wimar,MRI_LINEAR) ;

   oset = EDIT_empty_copy(iset) ;
   EDIT_dset_items( oset , ADN_prefix,"gwarp" , ADN_nvals,1 , ADN_none ) ;
   EDIT_substitute_brick( oset , 0 , MRI_float , MRI_FLOAT_PTR(bim) ) ;
   DSET_write(oset) ; WROTE_DSET(oset) ; exit(0) ;
}
