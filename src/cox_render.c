/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "cox_render.h"

#undef CREN_DEBUG

/*============================================================================
  CREN = Cox Renderer, a set of routines for volume rendering 3D bricks.

  (1) Use new_CREN_renderer() to create a renderer.
  (2) Load grayscale+color bricks into it with function CREN_set_databytes()
  (3) Set the viewing angles with CREN_set_viewpoint().
      (Optional: set axes ordering with CREN_set_axes().)
  (4) Create an image with CREN_render().
      Loop back through (2-4) as needed to create new images.
  (5) When finished, use destroy_CREN_renderer() to clean up.
==============================================================================*/

static int num_renderers = 0 ;  /* global count of how many are open */

static THD_mat33 rotmatrix( int ax1,float th1 ,
                            int ax2,float th2 , int ax3,float th3  ) ;

static void extract_byte_nn( int nx , int ny , int nz , byte * vol ,
                             Tmask * tm ,
                             int fixdir , int fixijk , float da , float db ,
                             int ma , int mb , byte * im ) ;

static void extract_byte_tsx( int nx , int ny , int nz , byte * vol ,
                              Tmask * tm ,
                              int fixdir , int fixijk , float da , float db ,
                              int ma , int mb , byte * im ) ;

static void extract_byte_lix( int nx , int ny , int nz , byte * vol ,
                              Tmask * tm ,
                              int fixdir , int fixijk , float da , float db ,
                              int ma , int mb , byte * im ) ;

typedef void exfunc(int,int,int,byte*,Tmask*,int,int,float,float,int,int,byte*);

#if 0
static void extract_byte_ts( int nx , int ny , int nz , byte * vol ,
                             Tmask * tm ,
                             int fixdir , int fixijk , float da , float db ,
                             int ma , int mb , byte * im ) ;
#endif

/*--------------------------------------------------------------------------
  Create a new renderer.
  The return pointer is passed to all other CREN routines.
----------------------------------------------------------------------------*/

void * new_CREN_renderer( void )
{
   CREN_stuff * ar ;
   int ii ;

   /*-- make storage for rendering struct --*/

   ar = (CREN_stuff *) malloc( sizeof(CREN_stuff) ) ;
   ar->type = CREN_TYPE ;

   /*-- initialize rendering struct to somewhat random values --*/

   ar->nx = ar->ny = ar->nz = ar->newvox = 0 ;
   ar->dx = ar->dy = ar->dz = 1.0 ;

   /* default axis rotation order is as in plug_render.c */

   ar->ax1 = 1   ; ar->ax2 = 0   ; ar->ax3 = 2   ;
   ar->th1 = 0.0 ; ar->th2 = 0.0 ; ar->th3 = 0.0 ; ar->newangles = 1 ;

   ar->vox = NULL ;  /* no data yet */
   ar->vtm = NULL ;  /* no Tmask yet */

   ar->newopa = 0 ;
   ar->opargb = 1.0 ;             /* colored voxels are opaque */
   for( ii=0 ; ii < 128 ; ii++ )  /* linear map for gray opacity */
      ar->opamap[ii] = ii/127.0 ;

   ar->nrgb   = 0 ;               /* no color map set */
   memset( ar->rmap , 0 , 128 ) ; memset( ar->gmap , 0 , 128 ) ;
   memset( ar->bmap , 0 , 128 ) ; memset( ar->imap , 0 , 128 ) ;

   ar->min_opacity = 0.05 ;
   ar->renmode     = CREN_SUM_VOX ;
   ar->intmode     = CREN_TWOSTEP ;

   LOAD_DIAG_MAT( ar->skewmat , 1.0,1.0,1.0 ) ;

   num_renderers ++ ; return (void *) ar ;
}

/*-----------------------------------------------------------------------------
   Get rid of a renderer and all its contents
-------------------------------------------------------------------------------*/

void destroy_CREN_renderer( void * ah )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return ;

   if( ar->vox != NULL ) free(ar->vox) ;
   if( ar->vtm != NULL ) free_Tmask(ar->vtm) ;
   free(ar) ;

   num_renderers -- ; return ;
}


/*-----------------------------------------------------------------------------
   Set the matrix to skew ijk indices to xyz coordinates (not normally needed)
-------------------------------------------------------------------------------*/

void CREN_set_skewmat( void * ah , THD_mat33 sm )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return ;

   ar->skewmat = sm ; return ;
}

/*-----------------------------------------------------------------------------
   Set the minimum voxel opacity to consider
-------------------------------------------------------------------------------*/

void CREN_set_min_opacity( void * ah , float opm )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return ;
   if( opm <= 0.0 || opm >= 1.0 ) opm = 0.05 ;
   ar->min_opacity = opm ;
   return ;
}

/*-----------------------------------------------------------------------------
   Set the rendering mode
     CREN_SUM_VOX = integral of voxel data times opacity
     CREN_MIP_VOX = maximum voxel intensity
     CREN_MIP_OPA = maximum opacity
-------------------------------------------------------------------------------*/

void CREN_set_render_mode( void * ah , int mmm )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;
   if( !ISVALID_CREN(ar) ) return ;

   if( mmm < 0 || mmm > CREN_LAST_MODE ) mmm = CREN_SUM_VOX ;
   ar->renmode = mmm ;
   return ;
}

/*-----------------------------------------------------------------------------
   Set the interpolation mode
     CREN_NN      = nearest neighbor
     CREN_TWOSTEP = two-step
     CREN_LINEAR  = linear
-------------------------------------------------------------------------------*/

void CREN_set_interp( void * ah , int mmm )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;
   if( !ISVALID_CREN(ar) ) return ;

   if( mmm < 0 || mmm > CREN_LINEAR ) mmm = CREN_TWOSTEP ;
   ar->intmode = mmm ;
   return ;
}

/*-----------------------------------------------------------------------------
  Load the user defined colormap.
    ncol    = number of colors (must be from 1 to 128, inclusive)
    rmap[i] = red   map for index i=0..ncol-1 (for voxel values i+128)
    gmap[i] = green map for index i=0..ncol-1 (for voxel values i+128)
    bmap[i] = blue  map for index i=0..ncol-1 (for voxel values i+128)
-------------------------------------------------------------------------------*/

void CREN_set_rgbmap( void *ah, int ncol, byte *rmap, byte *gmap, byte *bmap )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;
   int ii ;

   if( !ISVALID_CREN(ar) ) return ;
   if( ncol<1 || ncol>128 || rmap==NULL || gmap==NULL || bmap==NULL ) return ;

   ar->nrgb = ncol ;

   /* copy into rendering struct, and compute intensity of each color */

   for( ii=0 ; ii < ncol ; ii++ ){
      ar->rmap[ii] = rmap[ii] ;
      ar->gmap[ii] = gmap[ii] ;
      ar->bmap[ii] = bmap[ii] ;
      ar->imap[ii] = (byte)(0.299*rmap[ii]+0.587*gmap[ii]+0.114*bmap[ii]) ;
   }

   /* set leftovers to 0 */

   for( ii=ncol ; ii < 128 ; ii++ )
      ar->rmap[ii] = ar->gmap[ii] = ar->bmap[ii] = ar->imap[ii] = 0 ;

   return ;
}

/*----------------------------------------------------------------------
  Set the mapping from voxel value to opacity:
    opm[vv] = opacity of voxel value vv, for vv=0..127
    opgrb   = (fixed) opacity of colored voxels, with vv=128..255
  Opacities range from 0.0 to 1.0
------------------------------------------------------------------------*/

void CREN_set_opamap( void *ah, float *opm , float oprgb )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return ;

   if( opm != NULL )
      memcpy( ar->opamap , opm , sizeof(float)*128 ) ;

   if( oprgb >= 0.0 && oprgb <= 1.0 )
      ar->opargb = oprgb ;

#ifdef CREN_DEBUG
fprintf(stderr,"CREN_set_opamap: opm[0]=%g opm[127]=%g oprgb=%g\n",
opm[0],opm[127],oprgb) ;
#endif

   ar->newopa = 1 ; return ;
}

/*-----------------------------------------------------------------------
  See if the thing needs data bytes
-------------------------------------------------------------------------*/

int CREN_needs_data( void * ah )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return -1 ;

   return (ar->vox == NULL) ;
}

/*-----------------------------------------------------------------------------
  Set the data axes stuff.  The volume has 3 axes, indexed by (i,j,k).
  aii is set to denote the spatial direction of the increasing i direction:
      1 == +x direction
     -1 == -x direction
      2 == +y direction
     -2 == -y direction
      3 == +z direction
     -3 == -z direction
  and similarly for ajj, akk for the j and k directions.
  di, dj, dk are the magnitude of the stepsizes in each direction.
  (The number of points along each axis is set via CREN_set_databytes.)
-------------------------------------------------------------------------------*/

void CREN_set_axes( void * ah , int  aii , int  ajj , int  akk ,
                                float di , float dj , float dk  )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;
   int abii=abs(aii) , abjj=abs(ajj) , abkk=abs(akk) ;

   /*-- sanity checks --*/

   if( !ISVALID_CREN(ar) ) return ;

   if( abii < 1 || abii > 3 ||
       abjj < 1 || abjj > 3 ||
       abkk < 1 || abkk > 3 || abii+abjj+abkk != 6 ) return ;

   /*-- load stepsizes --*/

   ar->dx = fabs(di) ; if( ar->dx == 0.0 ) ar->dx = 1.0 ;
   ar->dy = fabs(dj) ; if( ar->dy == 0.0 ) ar->dy = 1.0 ;
   ar->dz = fabs(dk) ; if( ar->dz == 0.0 ) ar->dz = 1.0 ;

   /*-- construct skewmat --*/

   LOAD_ZERO_MAT(ar->skewmat) ;

#if 0
   ar->skewmat.mat[abii-1][0] = (aii > 0) ? 1.0 : -1.0 ;
   ar->skewmat.mat[abjj-1][1] = (ajj > 0) ? 1.0 : -1.0 ;
   ar->skewmat.mat[abkk-1][2] = (akk > 0) ? 1.0 : -1.0 ;
#else
   ar->skewmat.mat[0][abii-1] = (aii > 0) ? 1.0 : -1.0 ;
   ar->skewmat.mat[1][abjj-1] = (ajj > 0) ? 1.0 : -1.0 ;
   ar->skewmat.mat[2][abkk-1] = (akk > 0) ? 1.0 : -1.0 ;
#endif

#ifdef CREN_DEBUG
DUMP_MAT33("skewmat",ar->skewmat) ;
#endif

   ar->newangles = 1 ; return ;
}

/*---------------------------------------------------------------*/

void CREN_dset_axes( void * ah , THD_3dim_dataset * dset )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;
   int aii , ajj , akk ;
   float dx , dy , dz ;

   if( !ISVALID_CREN(ar) || !ISVALID_DSET(dset) ) return ;

   switch( dset->daxes->xxorient ){
      case ORI_R2L_TYPE: aii =  1 ; break ;
      case ORI_L2R_TYPE: aii = -1 ; break ;
      case ORI_A2P_TYPE: aii =  2 ; break ;
      case ORI_P2A_TYPE: aii = -2 ; break ;
      case ORI_I2S_TYPE: aii =  3 ; break ;
      case ORI_S2I_TYPE: aii = -3 ; break ;
   }
   switch( dset->daxes->yyorient ){
      case ORI_R2L_TYPE: ajj =  1 ; break ;
      case ORI_L2R_TYPE: ajj = -1 ; break ;
      case ORI_A2P_TYPE: ajj =  2 ; break ;
      case ORI_P2A_TYPE: ajj = -2 ; break ;
      case ORI_I2S_TYPE: ajj =  3 ; break ;
      case ORI_S2I_TYPE: ajj = -3 ; break ;
   }
   switch( dset->daxes->zzorient ){
      case ORI_R2L_TYPE: akk =  1 ; break ;
      case ORI_L2R_TYPE: akk = -1 ; break ;
      case ORI_A2P_TYPE: akk =  2 ; break ;
      case ORI_P2A_TYPE: akk = -2 ; break ;
      case ORI_I2S_TYPE: akk =  3 ; break ;
      case ORI_S2I_TYPE: akk = -3 ; break ;
   }

   dx = fabs(dset->daxes->xxdel) ;
   dy = fabs(dset->daxes->yydel) ;
   dz = fabs(dset->daxes->zzdel) ;

   CREN_set_axes( ah , aii,ajj,akk , dx,dy,dz ) ;
   return ;
}

/*-----------------------------------------------------------------------------
   Set the data brick in a renderer;
   Returns -1 if an error occurs, otherwise returns 0.

   Data bytes:
     values vv=0..127   correspond to grayscale 0..254 (vv << 1)
     values vv=128..255 correspond to colormap index vv-128.
-------------------------------------------------------------------------------*/

void CREN_set_databytes( void * ah , int ni,int nj,int nk, byte * grim )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;
   int nvox ;

   /*-- sanity checks --*/

   if( !ISVALID_CREN(ar) || grim == NULL ) return ;
   if( ni < 3 || nj < 3 || nk < 3 ) return ;

   /*-- free old data, if any --*/

   if( ar->vox != NULL ){ free(ar->vox)      ; ar->vox = NULL; }
   if( ar->vtm != NULL ){ free_Tmask(ar->vtm); ar->vtm = NULL; }

   /*-- set size of each axis--*/

   ar->nx = ni ; ar->ny = nj ; ar->nz = nk ;

   /*-- signal we have new voxel data --*/

   ar->newvox = 1 ;

   /*-- copy data from grim into internal storage --*/

   nvox = ni * nj * nk ;
   ar->vox = (byte *) malloc(nvox) ;
   memcpy( ar->vox , grim , nvox ) ;

   return ;
}

/*----------------------------------------------------------------------------
   Set the viewpoint of the user
     axI = axis about which rotation #I is to take place
            0 == x , 1 == y , 2 == z
     thI = angle of rotation (radians)
------------------------------------------------------------------------------*/

void CREN_set_viewpoint( void * ah , int ax1,float th1 ,
                                     int ax2,float th2 , int ax3,float th3  )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return ;

   ar->ax1 = ax1 ; ar->ax2 = ax2 ; ar->ax3 = ax3 ;
   ar->th1 = th1 ; ar->th2 = th2 ; ar->th3 = th3 ;

#ifdef CREN_DEBUG
fprintf(stderr,"CREN_set_viewpoint: ax1=%d th1=%g ax2=%d th2=%g ax3=%d th3=%g\n",
ax1,th1,ax2,th2,ax3,th3) ;
#endif

   ar->newangles = 1 ; return ;
}

/*-----------------------------------------------------------------------------*/

void CREN_set_rotaxes( void * ah , int ax1 , int ax2 , int ax3 )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return ;
   ar->ax1 = ax1 ; ar->ax2 = ax2 ; ar->ax3 = ax3 ;
   ar->newangles = 1 ; return ;
}

/*-----------------------------------------------------------------------------*/

void CREN_set_angles( void * ah , float th1 , float th2 , float th3 )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return ;
   ar->th1 = th1 ; ar->th2 = th2 ; ar->th3 = th3 ;
   ar->newangles = 1 ; return ;
}

/*-----------------------------------------------------------------------------
   Actually render an image.  Returns NULL if an error occurs.
   The image is always in MRI_rgb format.
-------------------------------------------------------------------------------*/

MRI_IMAGE * CREN_render( void * ah )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;
   MRI_IMAGE * bim , * qim ;
   byte * rgb , * var , * sl ;
   int nvox , nxy , vtop=128+ar->nrgb ;
   float *used=NULL , obot=ar->min_opacity ;
   THD_mat33 uu ;
   int ii,jj,kk , ni,nj,nk,pk , ma,mb,mab , nnn[3] ;
   float utop,uabs , a,b , aii,aij,aji,ajj , hnk , ba,bb ;
   int pk_reverse , ppk ;
   float dab ;
   register int vv , pij , p3 ;
   register float *omap , orgb ;
   exfunc *ifunc ;

   /*-- sanity checks --*/

   if( !ISVALID_CREN(ar) ) return NULL ;

   var = ar->vox ;
   if( var == NULL ) return NULL ;
   if( ar->nx < 3 || ar->ny < 3 || ar->nz < 3 ) return NULL ;

#ifdef CREN_DEBUG
fprintf(stderr,"CREN_render: nx=%d ny=%d nz=%d\n",ar->nx,ar->ny,ar->nz);
#endif

   nxy  = ar->nx * ar->ny ;  /* number of voxels in one plane */
   nvox = nxy * ar->nz ;     /* number of voxels in whole volume */

   /*-- make a Tmask to show which 1D rows contain opacity --*/

   omap = ar->opamap ; orgb = ar->opargb ;

#if 1
   if( ar->newvox || ar->newopa || ar->vtm == NULL ){
      register byte *tar, qrgb ;

      free_Tmask(ar->vtm) ;
      tar  = (byte *) malloc(nvox) ;
      qrgb = (orgb >= obot) ;  /* is color opaque? */
      for( pij=0 ; pij < nvox ; pij++ ){
         vv = var[pij] ; if( vv == 0 ) continue ;
         if( vv < 128 ) tar[pij] = (omap[vv] >= obot) ;
         else           tar[pij] = qrgb ;
      }

      ar->vtm = create_Tmask_byte( ar->nx,ar->ny,ar->nz , tar ) ;
      free(tar) ;
   }
#endif

   /*-- compute rotation matrix uu --*/

   uu = rotmatrix( ar->ax1,ar->th1 , ar->ax2,ar->th2 , ar->ax3,ar->th3 ) ;

   dab = MIN(ar->dx,ar->dy) ; dab = MIN(dab,ar->dz) ;  /* output grid spacing */

   /*-- we want
           diag{1/dx,1/dy,1/dz} [skewmat] [uu] diag{dab,dab,dab}
        which is the matrix which transforms from
        image index coords to volume index coords  --*/


   uu = MAT_MUL( ar->skewmat , uu ) ; /* a signed permutation? */

   uu.mat[0][0] *= (dab / ar->dx ) ;  /* multiply columns by dab  */
   uu.mat[0][1] *= (dab / ar->dx ) ;  /* rows by {1/dx,1/dy,1/dz} */
   uu.mat[0][2] *= (dab / ar->dx ) ;

   uu.mat[1][0] *= (dab / ar->dy ) ;
   uu.mat[1][1] *= (dab / ar->dy ) ;
   uu.mat[1][2] *= (dab / ar->dy ) ;

   uu.mat[2][0] *= (dab / ar->dz ) ;
   uu.mat[2][1] *= (dab / ar->dz ) ;
   uu.mat[2][2] *= (dab / ar->dz ) ;

#ifdef CREN_DEBUG
DUMP_MAT33("uu matrix",uu) ;
#endif

   /*-- find element uu(kk,2) that is largest: kk = projection axis --*/

   nnn[0] = ar->nx ; nnn[1] = ar->ny ; nnn[2] = ar->nz ;

#undef U
#define U(i,j) uu.mat[i][j]

   kk = 0 ; utop = fabs(U(0,2)) ;
   uabs = fabs(U(1,2)) ; if( uabs > utop ){ utop = uabs; kk = 1; }
   uabs = fabs(U(2,2)) ; if( uabs > utop ){ utop = uabs; kk = 2; }

   if( utop == 0.0 ) return NULL ;   /* bad matrix */

   ii = (kk+1) % 3 ;  /* image axes */
   jj = (kk+2) % 3 ;

   a = U(ii,2) / U(kk,2) ;  /* shearing parameters */
   b = U(jj,2) / U(kk,2) ;

#ifdef CREN_DEBUG
fprintf(stderr,"kk=%d a=%g b=%g\n",kk,a,b) ;
#endif

   aii = U(ii,0) - a * U(kk,0) ;  /* warping parameters   */
   aij = U(ii,1) - a * U(kk,1) ;  /* [to make projection] */
   aji = U(jj,0) - b * U(kk,0) ;  /* [image look correct] */
   ajj = U(jj,1) - b * U(kk,1) ;

#ifdef CREN_DEBUG
fprintf(stderr,"warp: aii=%g  aij=%g\n"
               "      aji=%g  ajj=%g\n" , aii,aij,aji,ajj ) ;
#endif

   /* n{ijk} = dimension of volume along axis {ijk} */

   ni = nnn[ii] ; nj = nnn[jj] ; nk = nnn[kk] ; hnk = 0.5*nk ;

   pk_reverse = ( U(kk,2) < 0.0 ) ;

   /* output image will be ma x mb pixels */

   ma = MAX(ni,nj) ; ma = MAX(ma,nk) ; ma *= 1.2 ;
   mb = ma ; mab = ma * mb ; ba = 0.5*(ma-ni) ; bb = 0.5*(mb-nj) ;

   sl = (byte *) malloc(mab) ;  /* will hold extracted sheared slice */

   /* create all zero output image */

   bim = mri_new(ma,mb,MRI_rgb); rgb = MRI_RGB_PTR(bim); memset(rgb,0,3*mab);

   /* prepare for projections of different types */

   switch( ar->renmode ){
     default:
     case CREN_SUM_VOX:
        used = (float *) malloc(sizeof(float)*mab) ;       /* how much opacity */
        for( pij=0 ; pij < mab ; pij++ ) used[pij] = 0.0 ; /* has been used up */
     break ;

     case CREN_MIP_VOX:
     break ;

     case CREN_MIP_OPA:
     break ;
   }

   /* extract sheared slices, then project them */

   switch( ar->intmode ){
      default:
      case CREN_TWOSTEP: ifunc = extract_byte_tsx ; break ;
      case CREN_NN:      ifunc = extract_byte_nn  ; break ;
      case CREN_LINEAR:  ifunc = extract_byte_lix ; break ;
   }

   for( pk=0 ; pk < nk ; pk++ ){  /* loop over slices */

      ppk = (pk_reverse) ? (nk-1-pk) : pk ;  /* maybe going backwards? */

      ifunc( ar->nx,ar->ny,ar->nz , var , ar->vtm ,
             kk+1 , ppk , ba-a*(ppk-hnk) , bb-b*(ppk-hnk) , ma,mb , sl ) ;

      switch( ar->renmode ){

#undef  MAX_OPACITY
#define MAX_OPACITY 0.95

        default:
        case CREN_SUM_VOX:{  /* integrate along the axis */
          register float opa ;

          for( p3=pij=0 ; pij < mab ; pij++,p3+=3 ){ /* loop over pixels */

             vv = sl[pij] ; if( vv == 0 ) continue ; /* skip voxel */

             if( used[pij] > MAX_OPACITY ) continue; /* skip voxel */

                  if( vv < 128  ) opa = omap[vv] ;   /* gray  */
             else if( vv < vtop ) opa = orgb ;       /* color */
             else                 continue ;         /* skip voxel */
             if( opa < obot ) continue ;             /* skip voxel */

             opa *= (1.0-used[pij]) ; used[pij] += opa ;

             if( vv < 128 ){                         /* gray */
                vv = (byte)( opa * (vv << 1) ) ;
                rgb[p3  ] += vv ;
                rgb[p3+1] += vv ;
                rgb[p3+2] += vv ;
             } else if( vv < vtop ){                 /* color */
                rgb[p3  ] += (byte)( opa * ar->rmap[vv-128] ) ;
                rgb[p3+1] += (byte)( opa * ar->gmap[vv-128] ) ;
                rgb[p3+2] += (byte)( opa * ar->bmap[vv-128] ) ;
             }

          }
        }
        break ;

        /* the MIP are grayscale projections:
           only fill in the R value now, and fix the GB values at the end */

        case CREN_MIP_VOX:   /* MIP on the signal intensity */
          for( p3=pij=0 ; pij < mab ; pij++,p3+=3 ){
             vv = sl[pij] ; if( vv == 0 ) continue ;      /* skip */
                  if( vv < 128  ) vv = vv << 1 ;          /* gray  */
             else if( vv < vtop ) vv = ar->imap[vv-128] ; /* color */
             else                 continue ;              /* skip  */

             if( vv > rgb[p3] ) rgb[p3] = vv ;      /* MIP   */
          }
        break ;

        case CREN_MIP_OPA:{  /* MIP on the opacity */
          float opa ;
          for( p3=pij=0 ; pij < mab ; pij++,p3+=3 ){
             vv = sl[pij] ; if( vv == 0 ) continue ;      /* skip */
                  if( vv < 128  ) opa = omap[vv] ;        /* gray  */
             else if( vv < vtop ) opa = orgb ;            /* color */
             else                 continue ;              /* skip  */

             vv = (byte)(255.9*opa) ;                     /* scale */
             if( vv > rgb[p3] ) rgb[p3] = vv ;      /* MIP   */
          }
        }
        break ;

     } /* end of switch over rendering mode */

   } /* end of loop over slices */

   free(sl) ;

   /*-- finalization --*/

   switch( ar->renmode ){
     default:
     case CREN_SUM_VOX:
        free(used) ;
     break ;

     case CREN_MIP_VOX:  /* fill in missing GB values */
     case CREN_MIP_OPA:
        for( p3=pij=0 ; pij < mab ; pij++,p3+=3 )
           rgb[p3+1] = rgb[p3+2] = rgb[p3] ;
     break ;
   }

   /* warp projection to final display coordinates */

   qim = mri_aff2d_rgb( bim , 1 , aii,aij,aji,ajj ) ;
   mri_free(bim) ; return qim ;
}

/*===========================================================================
   Functions to extract a plane of shifted bytes from a 3D volume.
     nx, ny, nz = dimensions of vol
     vol        = input 3D volume of bytes

     fixdir = fixed direction (1=x, 2=y, 3=z)
     fixijk = fixed index
     da, db = shift in planar coordinaes (non-fixed directions)
     ma, mb = dimensions of im
     im     = output 2D image

   Goal is im[a,b] = vol[ P(a-da,b-db,c=fixijk) ] for a=0..ma-1, b=0..mb-1,
   where P(a,b,c) is the permutation of (a,b,c) that goes with fixdir:
     P(x,y,z) = (y,z,x) for fixdir == 1
     P(x,y,z) = (z,x,y) for fixdir == 2
     P(x,y,z) = (x,y,z) for fixdir == 3
   For values outside the range of vol[], im[] is set to 0.
=============================================================================*/

  /* macros for offsets in vol[] to corners of the interpolation square */

#undef LL
#undef LR
#undef UL
#undef UR

#define LL 0                /* voxel offset to lower left  */
#define LR astep            /* voxel offset to lower right */
#define UL bstep            /* voxel offset to upper left  */
#define UR (astep+bstep)    /* voxel offset to upper right */

  /* macro to compute the stepsizes and dimensions in the
     3D array for each direction, given the fixed direction */

#undef  ASSIGN_DIRECTIONS
#define ASSIGN_DIRECTIONS                                       \
 do{ switch( fixdir ){                                          \
      default:                                                  \
      case 1:            /* x-direction: (a,b,c) = (y,z,x) */   \
         astep = nx ; bstep = nxy ; cstep = 1  ;                \
         na    = ny ; nb    = nz  ; nc    = nx ;                \
      break ;                                                   \
                                                                \
      case 2:            /* y-direction: (a,b,c) = (z,x,y) */   \
         astep = nxy ; bstep = 1  ; cstep = nx ;                \
         na    = nz  ; nb    = nx ; nc    = ny ;                \
      break ;                                                   \
                                                                \
      case 3:            /* z-direction: (a,b,c) = (x,y,z) */   \
         astep = 1  ; bstep = nx ; cstep = nxy ;                \
         na    = nx ; nb    = ny ; nc    = nz  ;                \
      break ;                                                   \
    } } while(0)

/*-----------------------------------------------------------------------*/

static void extract_assign_directions( int nx, int ny, int nz, int fixdir ,
                                       int *Astep, int *Bstep, int *Cstep ,
                                       int *Na   , int *Nb   , int *Nc     )
{
   int astep,bstep,cstep , na,nb,nc , nxy=nx*ny ;

   ASSIGN_DIRECTIONS ;

   *Astep = astep ; *Bstep = bstep ; *Cstep = cstep ;
   *Na    = na    ; *Nb    = nb    ; *Nc    = nc    ; return ;
}

/*-----------------------------------------------------------------------
   NN "interpolation"
-------------------------------------------------------------------------*/

static void extract_byte_nn( int nx , int ny , int nz , byte * vol ,
                             Tmask * tm ,
                             int fixdir , int fixijk , float da , float db ,
                             int ma , int mb , byte * im )
{
   int adel,bdel , abot,atop , bb,bbot,btop , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc ;
   byte * mask ;

   memset( im , 0 , ma*mb ) ;  /* initialize output to zero */

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   da += 0.5 ; adel = (int) da ; if( da < 0.0 ) adel-- ;  /* floor(da+0.5) */
   db += 0.5 ; bdel = (int) db ; if( db < 0.0 ) bdel-- ;  /* floor(db+0.5) */

   abot = 0       ; if( abot < adel ) abot = adel ;       /* range in im[] */
   atop = na+adel ; if( atop > ma   ) atop = ma ;

   bbot = 0       ; if( bbot < bdel ) bbot = bdel ;
   btop = nb+bdel ; if( btop > mb   ) btop = mb ;

   ijkoff = fixijk*cstep + (abot-adel)*astep + (bbot-bdel)*bstep ;
   boff   = bbot * ma ;

   mask = (tm == NULL) ? NULL
                       : tm->mask[fixdir%3] + (fixijk*nb - bdel) ;

   for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
      if( mask == NULL || mask[bb] )
         for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
            im[aa+boff] = vol[aoff+ijkoff] ;  /* im(aa,bb) = vol(aa-adel,bb-bdel,fixijk) */
                                              /*           = vol[ (aa-adel)*astep +
                                                                  (bb-bdel)*bstep +
                                                                  fixijk   *cstep   ]    */

   return ;
}

/*---------------------------------------------------------------------------
    Two-step interpolation
-----------------------------------------------------------------------------*/

#undef TSBOT
#undef TSTOP

#if 1
# define TSBOT 0.3
# define TSTOP 0.7
#else
# define TSBOT 0.25
# define TSTOP 0.75
#endif

/*---------------------------------------------------------------------------*/
#if 0
static void extract_byte_ts( int nx , int ny , int nz , byte * vol ,
                             Tmask * tm ,
                             int fixdir , int fixijk , float da , float db ,
                             int ma , int mb , byte * im )
{
   int adel,bdel , abot,atop , bb,bbot,btop , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc , nts,dts1,dts2 ;
   float fa , fb ;
   byte * mask ;

   memset( im , 0 , ma*mb ) ; /* initialize output to zero */

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   adel = (int) da ; if( da < 0.0 ) adel-- ;  /* floor(da) */
   bdel = (int) db ; if( db < 0.0 ) bdel-- ;  /* floor(db) */

   fa = da - adel ;               /* fractional part of da */
   fb = db - bdel ;               /* fractional part of db */

   fa = 1.0-fa ; fb = 1.0-fb ;    /* since im[a,b] = vol[a-da,b-db] */

   if( fa < TSBOT ){                      /*- Left 30% -*/
      if( fb < TSBOT ){                   /*- Lower 30% -*/
        nts = 1 ; dts1 = LL ;               /* [0,0] */
      } else if( fb > TSTOP ){            /*- Upper 30% -*/
        nts = 1 ; dts1 = UL ;               /* [0,1] */
      } else {                            /*- Middle 40% -*/
        nts = 2 ; dts1 = LL ; dts2 = UL ;   /* mid of [0,0] and [0,1] */
      }
   } else if( fa > TSTOP ){               /*- Right 30% -*/
      if( fb < TSBOT ){                   /*- Lower 30% -*/
        nts = 1 ; dts1 = LR ;               /* [1,0] */
      } else if( fb > TSTOP ){            /*- Upper 30% -*/
        nts = 1 ; dts1 = UR ;               /* [1,1] */
      } else {                            /*- Middle 40% -*/
        nts = 2 ; dts1 = LR ; dts2 = UR ;   /* mid of [1,0] and [1,1] */
      }
   } else {                               /*- Middle 40% -*/
      if( fb < TSBOT ){                   /*- Lower 30% -*/
        nts = 2 ; dts1 = LL ; dts2 = LR ;   /* mid of [0,0] and [1,0] */
      } else if( fb > TSTOP ){            /*- Upper 30% -*/
        nts = 2 ; dts1 = UL ; dts2 = UR ;   /* mid of [0,1] and [1,1] */
      } else {                            /*- Middle 40% -*/
        nts = 4 ;                           /* mid of all 4 points */
      }
   }

   adel++ ; bdel++ ;

   abot = 0         ; if( abot < adel ) abot = adel ;       /* range in im[] */
   atop = na+adel-1 ; if( atop > ma   ) atop = ma ;

   bbot = 0         ; if( bbot < bdel ) bbot = bdel ;
   btop = nb+bdel-1 ; if( btop > mb   ) btop = mb ;

   ijkoff = fixijk*cstep + (abot-adel)*astep + (bbot-bdel)*bstep ;
   boff   = bbot * ma ;

   mask = (tm == NULL) ? NULL
                       : tm->mask[fixdir%3] + (fixijk*nb - bdel) ;

   switch( nts ){

      case 1:
         ijkoff += dts1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
             for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = vol[aoff+ijkoff] ;
      break ;

      case 2:
         ijkoff += dts1 ; dts2 -= dts1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
             for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = (vol[aoff+ijkoff] + vol[aoff+(ijkoff+dts2)]) >> 1;
      break ;

      case 4:
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
             for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = ( vol[aoff+ijkoff]     +vol[aoff+(ijkoff+LR)]
                              +vol[aoff+(ijkoff+UL)]+vol[aoff+(ijkoff+UR)]) >> 2;
      break ;
   }

   return ;
}
#endif

/*---------------------------------------------------------------------------*/

static void extract_byte_tsx( int nx , int ny , int nz , byte * vol ,
                              Tmask * tm ,
                              int fixdir , int fixijk , float da , float db ,
                              int ma , int mb , byte * im )
{
   int adel,bdel , abot,atop , bb,bbot,btop , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc , nts,dts1,dts2 , nn ;
   float fa , fb ;
   byte *mask , v1,v2,v3,v4 ;

   memset( im , 0 , ma*mb ) ; /* initialize output to zero */

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   adel = (int) da ; if( da < 0.0 ) adel-- ;  /* floor(da) */
   bdel = (int) db ; if( db < 0.0 ) bdel-- ;  /* floor(db) */

   fa = da - adel ;               /* fractional part of da */
   fb = db - bdel ;               /* fractional part of db */

   fa = 1.0-fa ; fb = 1.0-fb ;    /* since im[a,b] = vol[a-da,b-db] */

   if( fa < TSBOT ){                      /*- Left 30% -*/
      if( fb < TSBOT ){                   /*- Lower 30% -*/
        nts = 1 ; dts1 = LL ;               /* [0,0] */
      } else if( fb > TSTOP ){            /*- Upper 30% -*/
        nts = 1 ; dts1 = UL ;               /* [0,1] */
      } else {                            /*- Middle 40% -*/
        nts = 2 ; dts1 = LL ; dts2 = UL ;   /* mid of [0,0] and [0,1] */
      }
   } else if( fa > TSTOP ){               /*- Right 30% -*/
      if( fb < TSBOT ){                   /*- Lower 30% -*/
        nts = 1 ; dts1 = LR ;               /* [1,0] */
      } else if( fb > TSTOP ){            /*- Upper 30% -*/
        nts = 1 ; dts1 = UR ;               /* [1,1] */
      } else {                            /*- Middle 40% -*/
        nts = 2 ; dts1 = LR ; dts2 = UR ;   /* mid of [1,0] and [1,1] */
      }
   } else {                               /*- Middle 40% -*/
      if( fb < TSBOT ){                   /*- Lower 30% -*/
        nts = 2 ; dts1 = LL ; dts2 = LR ;   /* mid of [0,0] and [1,0] */
      } else if( fb > TSTOP ){            /*- Upper 30% -*/
        nts = 2 ; dts1 = UL ; dts2 = UR ;   /* mid of [0,1] and [1,1] */
      } else {                            /*- Middle 40% -*/
        nts = 4 ;                           /* mid of all 4 points */
      }
   }

   nn = (fa < 0.5) ? ( (fb < 0.5) ? LL : UL )   /* NN index point */
                   : ( (fb < 0.5) ? LR : UR ) ;

   adel++ ; bdel++ ;

   abot = 0         ; if( abot < adel ) abot = adel ;       /* range in im[] */
   atop = na+adel-1 ; if( atop > ma   ) atop = ma ;

   bbot = 0         ; if( bbot < bdel ) bbot = bdel ;
   btop = nb+bdel-1 ; if( btop > mb   ) btop = mb ;

   ijkoff = fixijk*cstep + (abot-adel)*astep + (bbot-bdel)*bstep ;
   boff   = bbot * ma ;

   mask = (tm == NULL) ? NULL
                       : tm->mask[fixdir%3] + (fixijk*nb - bdel) ;

   /*-- the following is intended to implement

         im(aa,bb) = vol(aa-adel,bb-bdel,fixijk)
                   = vol[ (aa-adel)*astep +
                          (bb-bdel)*bstep +
                          fixijk   *cstep   ]    --*/

#define BECLEVER

   switch( nts ){

      case 1:
         ijkoff += dts1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
             for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = vol[aoff+ijkoff] ;
      break ;

      case 2:
         ijkoff += dts1 ; dts2 -= dts1 ; nn -= dts1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
             for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep ){
               v1 = vol[aoff+ijkoff] ;
               v2 = vol[aoff+(ijkoff+dts2)] ;
#ifdef BECLEVER
               if( (v1|v2) & 128 != 0 )
#else
               if( v1 < 128 && v2 < 128 )
#endif
                  im[aa+boff] = (v1+v2) >> 1 ;           /* grayscale */
               else
                  im[aa+boff] = vol[aoff+(ijkoff+nn)] ;  /* color code */
             }
      break ;

      case 4:
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
             for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep ){
               v1 = vol[aoff+ijkoff] ;
               v2 = vol[aoff+(ijkoff+LR)] ;
               v3 = vol[aoff+(ijkoff+UL)] ;
               v4 = vol[aoff+(ijkoff+UR)] ;
#ifdef BECLEVER
               if( (v1|v2|v3|v4) & 128 != 0 )
#else
               if( v1 < 128 && v2 < 128 && v3 < 128 && v4 < 128 )
#endif
                  im[aa+boff] = (v1+v2+v3+v4) >> 2 ;     /* grayscale */
               else
                  im[aa+boff] = vol[aoff+(ijkoff+nn)] ;  /* color code */
            }
      break ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/

static void extract_byte_lix( int nx , int ny , int nz , byte * vol ,
                              Tmask * tm ,
                              int fixdir , int fixijk , float da , float db ,
                              int ma , int mb , byte * im )
{
   int adel,bdel , abot,atop , bb,bbot,btop , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc , nn ;
   float fa , fb ;
   float f_a_b , f_ap_b , f_a_bp , f_ap_bp ;
   byte  b_a_b , b_ap_b , b_a_bp , b_ap_bp ;
   byte *mask , v1,v2,v3,v4 ;

   memset( im , 0 , ma*mb ) ; /* initialize output to zero */

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   adel = (int) da ; if( da < 0.0 ) adel-- ;  /* floor(da) */
   bdel = (int) db ; if( db < 0.0 ) bdel-- ;  /* floor(db) */

   fa = da - adel ;               /* fractional part of da */
   fb = db - bdel ;               /* fractional part of db */

   f_a_b   = fa      * fb      ;  /* bilinear interploation */
   f_ap_b  = (1.0-fa)* fb      ;  /* coefficients */
   f_a_bp  = fa      *(1.0-fb) ;
   f_ap_bp = (1.0-fa)*(1.0-fb) ;

   bb = (int)(256*f_a_b  + 0.499); if( bb == 256 ) bb--; b_a_b  = (byte) bb;
   bb = (int)(256*f_ap_b + 0.499); if( bb == 256 ) bb--; b_ap_b = (byte) bb;
   bb = (int)(256*f_a_bp + 0.499); if( bb == 256 ) bb--; b_a_bp = (byte) bb;
   bb = (int)(256*f_ap_bp+ 0.499); if( bb == 256 ) bb--; b_ap_bp= (byte) bb;

   nn = (fa > 0.5) ? ( (fb > 0.5) ? LL : UL )   /* NN index point */
                   : ( (fb > 0.5) ? LR : UR ) ;

   adel++ ; bdel++ ;

   abot = 0         ; if( abot < adel ) abot = adel ;       /* range in im[] */
   atop = na+adel-1 ; if( atop > ma   ) atop = ma ;

   bbot = 0         ; if( bbot < bdel ) bbot = bdel ;
   btop = nb+bdel-1 ; if( btop > mb   ) btop = mb ;

   ijkoff = fixijk*cstep + (abot-adel)*astep + (bbot-bdel)*bstep ;
   boff   = bbot * ma ;

   mask = (tm == NULL) ? NULL
                       : tm->mask[fixdir%3] + (fixijk*nb - bdel) ;

   /*-- the following is intended to implement

         im(aa,bb) = vol(aa-adel,bb-bdel,fixijk)
                   = vol[ (aa-adel)*astep +
                          (bb-bdel)*bstep +
                          fixijk   *cstep   ]    --*/

   for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
     if( mask == NULL || mask[bb] || mask[bb+1] )
       for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep ){
         v1 = vol[aoff+ijkoff]      ;
         v2 = vol[aoff+(ijkoff+LR)] ;
         v3 = vol[aoff+(ijkoff+UL)] ;
         v4 = vol[aoff+(ijkoff+UR)] ;
         if( v1 < 128 && v2 < 128 && v3 < 128 && v4 < 128 ) /* gray */
#if 0
           im[aa+boff] = (byte)(  f_a_b  * v1 + f_ap_b  * v2
                                + f_a_bp * v3 + f_ap_bp * v4 ) ;
#else
           im[aa+boff] = (byte)((  b_a_b  * v1 + b_ap_b  * v2
                                 + b_a_bp * v3 + b_ap_bp * v4 ) >> 8) ;
#endif
         else
           im[aa+boff] = vol[aoff+(ijkoff+nn)] ;  /* color code */
       }

   return ;
}

/*------------------------------------------------------------------------------*/

static THD_mat33 rotmatrix( int ax1,float th1 ,
                            int ax2,float th2 , int ax3,float th3  )
{
   THD_mat33 q , p ;

   LOAD_ROT_MAT( q , th1 , ax1 ) ;
   LOAD_ROT_MAT( p , th2 , ax2 ) ; q = MAT_MUL( p , q ) ;
   LOAD_ROT_MAT( p , th3 , ax3 ) ; q = MAT_MUL( p , q ) ;

   return q ;
}
