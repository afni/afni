#include "cox_render.h"

/*============================================================================
  CREN = Cox Renderer, a set of routines for volume rendering 3D bricks.

  (1) Use new_CREN_renderer() to create a renderer.
  (2) Load grayscale+color bricks into it with function CREN_set_databytes()
  (3) Set the viewing angles with CREN_set_viewpoint().
  (4) Create an image with CREN_render().
      Loop back through (2-4) as needed to create new images.
  (5) When finished, use destroy_CREN_renderer() to clean up.
==============================================================================*/

static int num_renderers = 0 ;  /* global count of how many are open */

/* a prototype (see end of code) */

static void CREN_rotate(int,int,int, float,float,float , byte * ,
                        int,float, int,float, int,float,
                        int,float,float,float , Tmask * ) ;

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

   /*-- initialize rendering struct --*/

   ar->nx = ar->ny = ar->nz = ar->newvox = 0 ;
   ar->dx = ar->dy = ar->dz = 1.0 ;

   ar->ax1 = 0   ; ar->ax2 = 1   ; ar->ax3 = 2   ;
   ar->th1 = 0.0 ; ar->th2 = 0.0 ; ar->th3 = 0.0 ; ar->newangles = 1 ;

   ar->vox    = NULL ;  /* no data yet */
   ar->vtm    = NULL ;
   ar->voxrot = NULL ;

   ar->newopa = 0 ;
   ar->opargb = 1.0 ;             /* colored voxels are opaque */
   for( ii=0 ; ii < 128 ; ii++ )  /* linear map for gray opacity */
      ar->opamap[ii] = ii/127.0 ;

   ar->nrgb   = 0 ;               /* no color map set */
   memset( ar->rmap , 0 , 128 ) ; memset( ar->gmap , 0 , 128 ) ;
   memset( ar->bmap , 0 , 128 ) ; memset( ar->imap , 0 , 128 ) ;

   ar->min_opacity = 0.05 ;
   ar->renmode     = CREN_SUM_VOX ;

   num_renderers ++ ; return (void *) ar ;
}

/*-----------------------------------------------------------------------------
   Get rid of a renderer and all its contents
-------------------------------------------------------------------------------*/

void destroy_CREN_renderer( void * ah )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;

   if( !ISVALID_CREN(ar) ) return ;

   if( ar->vox    != NULL ) free(ar->vox) ;
   if( ar->voxrot != NULL ) free(ar->voxrot) ;
   if( ar->vtm    != NULL ) free_Tmask(ar->vtm) ;
   free(ar) ;

   num_renderers-- ; return ;
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
   if( ncol < 1 || ncol > 128 || rmap==NULL || gmap==NULL || bmap==NULL ) return;

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

#if 0
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
   Set the data brick in a renderer;
   Returns -1 if an error occurs, otherwise returns 0.

   Data bytes:
     values vv=0..127   correspond to grayscale 0..254 (vv << 1)
     values vv=128..255 correspond to colormap index vv-128.
-------------------------------------------------------------------------------*/

int CREN_set_databytes( void * ah , MRI_IMAGE * grim )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;
   int nvox,ii ;
   byte * gar ;

   /*-- sanity checks --*/

   if( !ISVALID_CREN(ar) || grim == NULL || grim->kind != MRI_byte ) return -1 ;

   if( grim->nx < 3 || grim->ny < 3 || grim->nz < 3 ){
      fprintf(stderr,"**CREN: illegal dimensions for a data brick\n") ;
      return -1 ;
   }

   /*-- free old data --*/

   if( ar->vox    != NULL ){ free(ar->vox)      ; ar->vox    = NULL; }
   if( ar->voxrot != NULL ){ free(ar->voxrot)   ; ar->voxrot = NULL; }
   if( ar->vtm    != NULL ){ free_Tmask(ar->vtm); ar->vtm    = NULL; }

   /*-- set dimensions --*/


   ar->nx = grim->nx ;
   ar->ny = grim->ny ;
   ar->nz = grim->nz ;
   ar->dx = fabs(grim->dx) ;
   ar->dy = fabs(grim->dy) ;
   ar->dz = fabs(grim->dz) ;

   if( ar->dx == 0.0 ) ar->dx = 1.0 ;
   if( ar->dy == 0.0 ) ar->dy = 1.0 ;
   if( ar->dz == 0.0 ) ar->dz = 1.0 ;

   /*-- signal we have new voxel data --*/

   ar->newvox = 1 ;

   /*-- copy data from grim into internal storage --*/

   nvox = ar->nx * ar->ny * ar->nz ;

   ar->vox = (byte *) malloc(nvox) ;
   memcpy( ar->vox , MRI_BYTE_PTR(grim) , nvox ) ;

#if 0
fprintf(stderr,"CREN_set_databytes: nx=%d ny=%d nz=%d\n",ar->nx,ar->ny,ar->nz) ;
fprintf(stderr,"CREN_set_databytes: dx=%g dy=%g dz=%g\n",ar->dx,ar->dy,ar->dz) ;
#endif

   return 0 ;
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

#if 0
fprintf(stderr,"CREN_set_viewpoint: ax1=%d th1=%g ax2=%d th2=%g ax3=%d th3=%g\n",
ax1,th1,ax2,th2,ax3,th3) ;
#endif

   ar->newangles = 1 ; return ;
}

/*-----------------------------------------------------------------------------
   Actually render an image.  Returns NULL if an error occurs.
   The image is always in MRI_rgb format.
-------------------------------------------------------------------------------*/

MRI_IMAGE * CREN_render( void * ah )
{
   CREN_stuff * ar = (CREN_stuff *) ah ;
   MRI_IMAGE * im ;
   byte * rgb , * var ;
   int nvox , nxy , ii , kk , vv , vtop=128+ar->nrgb ;
   float * used=NULL , obot=ar->min_opacity ;

   /*-- sanity checks --*/

   if( !ISVALID_CREN(ar) ) return NULL ;

   var = ar->vox ;
   if( var == NULL ) return NULL ;

   if( ar->nx < 3 || ar->ny < 3 || ar->nz < 3 ) return NULL ;

   nxy  = ar->nx * ar->ny ;  /* number of voxels in one plane */
   nvox = nxy * ar->nz ;     /* number of voxels in whole volume */

   /*-- make a Tmask to show which 1D rows contain opacity --*/

   if( ar->newvox || ar->newopa || ar->vtm == NULL ){
      byte *tar, orgb ;

      free_Tmask(ar->vtm) ;
      tar  = (byte *) malloc(nvox) ;
      orgb = (ar->opargb >= ar->min_opacity) ;  /* is color opaque? */
      for( ii=0 ; ii < nvox ; ii++ ){
         vv = var[ii] ;
         if( vv < 128 ) tar[ii] = (ar->opamap[vv] >= obot) ;
         else           tar[ii] = orgb ;
      }

      ar->vtm = create_Tmask_byte( ar->nx,ar->ny,ar->nz , tar ) ;
      free(tar) ;
   }

   /*-- rotate the voxels to the correct alignment --*/

   if( ar->voxrot == NULL || ar->newvox || ar->newangles ){

      if( ar->voxrot == NULL )
         ar->voxrot = (byte *) malloc(nvox) ;

      memcpy( ar->voxrot , var , nvox ) ;

      CREN_rotate( ar->nx , ar->ny , ar->nz ,
                   ar->dx , ar->dy , ar->dz , ar->voxrot ,
                   ar->ax1, ar->th1, ar->ax2,ar->th2, ar->ax3,ar->th3 ,
                   DELTA_BEFORE , 0.0,0.0,0.0 , ar->vtm ) ;
   }

   /*-- prepare to create image --*/

   im  = mri_new( ar->nx , ar->ny , MRI_rgb ) ;
   rgb = MRI_RGB_PTR(im) ; memset(rgb,0,3*nxy) ;
   var = ar->voxrot ;

   switch( ar->renmode ){
     default:
     case CREN_SUM_VOX:
        used = (float *) malloc(sizeof(float)*nxy) ;   /* how much opacity */
        for( ii=0 ; ii < nxy ; ii++ ) used[ii] = 0.0 ; /* has been used up */
     break ;

     case CREN_MIP_VOX:
     break ;

     case CREN_MIP_OPA:
     break ;
   }

   /*-- create the output image by running down each column --*/

   for( kk=0 ; kk < ar->nz ; kk++,var+=nxy ){   /* at this level in z */

     switch( ar->renmode ){

#define MAX_OPACITY 0.95

        default:
        case CREN_SUM_VOX:{
          float opa ;

          for( ii=0 ; ii < nxy ; ii++ ){   /* loop over pixels in this level */

             if( used[ii] > MAX_OPACITY ) continue ; /* skip voxel */

             vv = var[ii] ;
                  if( vv < 128  ) opa = ar->opamap[vv] ;  /* gray  */
             else if( vv < vtop ) opa = ar->opargb ;      /* color */
             else                 continue ;         /* skip voxel */
             if( opa < obot ) continue ;             /* skip voxel */

             opa *= (1.0-used[ii]) ; used[ii] += opa ;
             if( opa < obot ) continue ;             /* skip voxel */

             if( vv < 128 ){                         /* gray */
                vv = (byte)( opa * (vv << 1) ) ;
                rgb[3*ii  ] += vv ;
                rgb[3*ii+1] += vv ;
                rgb[3*ii+2] += vv ;
             } else if( vv < vtop ){                 /* color */
                rgb[3*ii  ] += (byte)( opa * ar->rmap[vv-128] ) ;
                rgb[3*ii+1] += (byte)( opa * ar->gmap[vv-128] ) ;
                rgb[3*ii+2] += (byte)( opa * ar->bmap[vv-128] ) ;
             }

          }
        }
        break ;

        case CREN_MIP_VOX:
          for( ii=0 ; ii < nxy ; ii++ ){
             vv = var[ii] ;
                  if( vv < 128  ) vv = vv << 1 ;          /* gray  */
             else if( vv < vtop ) vv = ar->imap[vv-128] ; /* color */
             else                 continue ;              /* skip  */

             if( vv > rgb[3*ii] ) rgb[3*ii] = vv ;        /* MIP   */
          }
        break ;

        case CREN_MIP_OPA:{
          float opa ;
          for( ii=0 ; ii < nxy ; ii++ ){
             vv = var[ii] ;
                  if( vv < 128  ) opa = ar->opamap[vv] ;  /* gray  */
             else if( vv < vtop ) opa = ar->opargb ;      /* color */
             else                 continue ;              /* skip  */

             vv = (byte)(255.9*opa) ;                     /* scale */
             if( vv > rgb[3*ii] ) rgb[3*ii] = vv ;        /* MIP   */
          }
        }
        break ;

     } /* end of switch over rendering mode */

   } /* end of loop over levels */

   /*-- finalization --*/

   switch( ar->renmode ){
     default:
     case CREN_SUM_VOX:
        free(used) ;
     break ;

     case CREN_MIP_VOX:  /* fill in missing GB values */
     case CREN_MIP_OPA:
        for( ii=0 ; ii < nxy ; ii++ )
           rgb[3*ii+1] = rgb[3*ii+2] = rgb[3*ii] ;
     break ;
   }

   return im ;
}

/*===========================================================================
  Routines to rotate/shift a 3D volume of bytes using a 4 way shear
  decomposition and "two-step" interpolation, specifically for the
  volume renderer.  In this case, byte values are interepreted as
      0..127 = intensity (continuous)
    128..255 = index into color table (discrete)
  Interpolation for 0..127 is via the two-step method; for 128..255, via NN.

  RWCox - Nov 2000
=============================================================================*/

#include "thd_shear3d.h"

#define CACHE 7168 /* good for Pentium processors */

#define TSBOT 0.3  /* the "optimal" breakpoints for ts_shift */
#define TSTOP 0.7
#define NNBOT 0.5

static int    nlcbuf = 0 ;     /* workspace */
static byte * lcbuf = NULL ;

/*---------------------------------------------------------------------------
   Two-step interpolation and shifting
-----------------------------------------------------------------------------*/

static int ts_shift_byte( int n , float af , byte * f )
{
   register int ii , ia , ix ;
   float aa ;
   int ibot,itop ;

   if( fabs(af) < TSBOT ) return 0 ; /* do nothing if shift is too small */

   for( ii=0 ; ii < n && f[ii] == 0 ; ii++ ) ; /* nada */
   if( ii == n ) return 0 ;          /* do nothing if data all zero */

   af = -af ; ia = (int) af ; if( af < 0 ) ia-- ;  /* ia = floor */
   aa = af - ia ;

   if( n > nlcbuf ){
      if( lcbuf != NULL ) free(lcbuf) ;
      lcbuf  = (byte *) malloc(n) ;
      nlcbuf = n ;
   }

   ibot = -ia  ;   if( ibot < 0   ) ibot = 0 ;
   itop = n-2-ia ; if( itop > n-1 ) itop = n-1 ;

#if 1
   memset(lcbuf,0,n) ;   /* seems to be faster */
#else
   memset(lcbuf,0,ibot) ;
   memset(lcbuf+(itop+1),0,(n-(itop+1))) ;
#endif

   if( aa < TSBOT ){           /* NN to bottom */

      memcpy( lcbuf+ibot, f+(ibot+ia)  , (itop+1-ibot) ) ;

   } else if( aa > TSTOP ){  /* NN to top */

      memcpy( lcbuf+ibot, f+(ibot+1+ia), (itop+1-ibot) ) ;

   } else {                    /* average bottom and top, */
                               /* if grayscales (0..127); */
      byte *fp = f ;           /* otherwise, use NN       */
      if( aa > NNBOT ) fp++ ;

      for( ii=ibot ; ii <= itop ; ii++ ){
         ix = ii + ia ;
         if( f[ix] < 128 && f[ix+1] < 128 )        /* average */
            lcbuf[ii] = ( f[ix] + f[ix+1] ) >> 1 ;
         else
            lcbuf[ii] = fp[ix] ;                   /* NN */
      }

   }
   memcpy( f , lcbuf , n ) ;
   return 1 ;
}

/*---------------------------------------------------------------------------
   Flip a 3D array about the (x,y) axes:
    i <--> nx-1-i    j <--> ny-1-j
-----------------------------------------------------------------------------*/

#define VV(i,j,k) v[(i)+(j)*nx+(k)*nxy]
#define SX(i)     (nx1-(i))
#define SY(j)     (ny1-(j))
#define SZ(k)     (nz1-(k))

static void flip_xy( int nx , int ny , int nz , byte * v , Tmask * tm )
{
   int ii,jj,kk ;
   int nx1=nx-1,nx2=nx/2, ny1=ny-1,ny2=ny/2, nz1=nz-1,nz2=nz/2, nxy=nx*ny ;
   byte * r1 ;

   r1 = (byte *) malloc(nx) ;  /* save 1 row */

   for( kk=0 ; kk < nz ; kk++ ){              /* for each slice */
      for( jj=0 ; jj < ny2 ; jj++ ){          /* first 1/2 of rows */

         /* swap rows jj and ny1-jj, flipping them in ii as well */

         if( TM_XLINE(tm,jj+kk*ny) || TM_XLINE(tm,SY(jj)+kk*ny) ){
            for( ii=0; ii < nx; ii++ ) r1[ii]           = VV(SX(ii),SY(jj),kk) ;
            for( ii=0; ii < nx; ii++ ) VV(ii,SY(jj),kk) = VV(SX(ii),jj    ,kk) ;
            for( ii=0; ii < nx; ii++ ) VV(ii,jj    ,kk) = r1[ii] ;
         }
      }
      if( ny%2 == 1 && TM_XLINE(tm,jj+kk*ny) ){  /* central row? */
         for( ii=0; ii < nx; ii++ ) r1[ii]       = VV(SX(ii),jj,kk); /* flip it */
         for( ii=0; ii < nx; ii++ ) VV(ii,jj,kk) = r1[ii] ;          /* restore */
      }
   }

   free(r1) ; return ;
}

/*---------------------------------------------------------------------------
   Flip a 3D array about the (y,z) axes:
     j <--> ny-1-j   k <--> nz-1-k
-----------------------------------------------------------------------------*/

static void flip_yz( int nx , int ny , int nz , byte * v , Tmask * tm )
{
   int ii,jj,kk ;
   int nx1=nx-1,nx2=nx/2, ny1=ny-1,ny2=ny/2, nz1=nz-1,nz2=nz/2, nxy=nx*ny ;
   byte * r1 ;

   r1 = (byte *) malloc(ny) ;

   for( ii=0 ; ii < nx ; ii++ ){
      for( kk=0 ; kk < nz2 ; kk++ ){
         if( TM_YLINE(tm,kk+ii*nz) || TM_YLINE(tm,SZ(kk)+ii*nz) ){
            for( jj=0; jj < ny; jj++ ) r1[jj]           = VV(ii,SY(jj),SZ(kk)) ;
            for( jj=0; jj < ny; jj++ ) VV(ii,jj,SZ(kk)) = VV(ii,SY(jj),kk    ) ;
            for( jj=0; jj < ny; jj++ ) VV(ii,jj,kk    ) = r1[jj] ;
         }
      }
      if( nz%2 == 1 && TM_YLINE(tm,kk+ii*nz) ){
         for( jj=0; jj < ny; jj++ ) r1[jj]       = VV(ii,SY(jj),kk) ;
         for( jj=0; jj < ny; jj++ ) VV(ii,jj,kk) = r1[jj] ;
      }
   }

   free(r1) ; return ;
}

/*---------------------------------------------------------------------------
   Flip a 3D array about the (x,z) axes:
     i <--> nx-1-i   k <--> nz-1-k
-----------------------------------------------------------------------------*/

static void flip_xz( int nx , int ny , int nz , byte * v , Tmask * tm )
{
   int ii,jj,kk ;
   int nx1=nx-1,nx2=nx/2, ny1=ny-1,ny2=ny/2, nz1=nz-1,nz2=nz/2, nxy=nx*ny ;
   byte * r1 ;

   r1 = (byte *) malloc(nx) ;

   for( jj=0 ; jj < ny ; jj++ ){
      for( kk=0 ; kk < nz2 ; kk++ ){
         if( TM_XLINE(tm,jj+kk*ny) || TM_XLINE(tm,jj+SZ(kk)*ny) ){
            for( ii=0; ii < nx; ii++ ) r1[ii]           = VV(SX(ii),jj,SZ(kk)) ;
            for( ii=0; ii < nx; ii++ ) VV(ii,jj,SZ(kk)) = VV(SX(ii),jj,kk    ) ;
            for( ii=0; ii < nx; ii++ ) VV(ii,jj,kk    ) = r1[ii] ;
         }
      }
      if( nz%2 == 1 && TM_XLINE(tm,jj+kk*ny) ){
         for( ii=0; ii < nx; ii++ ) r1[ii]       = VV(SX(ii),jj,kk) ;
         for( ii=0; ii < nx; ii++ ) VV(ii,jj,kk) = r1[ii] ;
      }
   }

   free(r1) ; return ;
}

/*---------------------------------------------------------------------------
   Apply an x-axis shear to a 3D array: x -> x + a*y + b*z + s
   (dilation factor "f" assumed to be 1.0)
-----------------------------------------------------------------------------*/

static void apply_xshear( float a , float b , float s ,
                          int nx , int ny , int nz , byte * v , Tmask * tm )
{
   byte * fj0 ;
   int   nx1=nx-1    , ny1=ny-1    , nz1=nz-1    , nxy=nx*ny ;
   float nx2=0.5*nx1 , ny2=0.5*ny1 , nz2=0.5*nz1 ;
   int ii,jj,kk ;
   float st ;

   /* don't do anything if shift is too small */

   st = fabs(a)*ny2 + fabs(b)*nz2 + fabs(s); if( st < TSBOT ) return ;

   for( kk=0 ; kk < nz ; kk++ ){
     for( jj=0 ; jj < ny ; jj++ )
       if( TM_XLINE(tm,jj+kk*ny) )
         ts_shift_byte( nx, a*(jj-ny2)+b*(kk-nz2)+s, v+(jj*nx+kk*nxy) );
   }

   return ;
}

/*---------------------------------------------------------------------------
   Apply a y-axis shear to a 3D array: y -> y + a*x + b*z + s
-----------------------------------------------------------------------------*/

static void apply_yshear( float a , float b , float s ,
                          int nx , int ny , int nz , byte * v , Tmask * tm )
{
   byte * fj0 ;
   int   nx1=nx-1    , ny1=ny-1    , nz1=nz-1    , nxy=nx*ny ;
   float nx2=0.5*nx1 , ny2=0.5*ny1 , nz2=0.5*nz1 ;
   int ii,jj,kk ;
   float st ;
   int xnum , xx , xtop , *wk ;

   /* don't do anything if shift is too small */

   st = fabs(a)*nx2 + fabs(b)*nz2 + fabs(s) ; if( st < TSBOT ) return ;

   xnum = CACHE / (ny) ; if( xnum < 1 ) xnum = 1 ;
   fj0 = (byte *) malloc( xnum*ny ) ;
   wk  = (int *)   malloc( sizeof(int)*xnum ) ;

   for( kk=0 ; kk < nz ; kk++ ){
     for( ii=0 ; ii < nx ; ii+=xnum ){
       xtop = MIN(nx-ii,xnum) ;
       for( xx=0 ; xx < xtop ; xx++ )
         wk[xx] = fabs(a*(ii+xx-nx2)+b*(kk-nz2)+s) > TSBOT
                  && TM_YLINE(tm,kk+(ii+xx)*nz) ;
       for( jj=0; jj < ny; jj++ )
         for( xx=0 ; xx < xtop ; xx++ )
           if( wk[xx] ) fj0[jj+xx*ny] = VV(ii+xx,jj,kk) ;
       for( xx=0 ; xx < xtop ; xx++ )
         if( wk[xx] )
          wk[xx] = ts_shift_byte(ny, a*(ii+xx-nx2)+b*(kk-nz2)+s, fj0+xx*ny);
       for( jj=0; jj < ny; jj++ )
         for( xx=0 ; xx < xtop ; xx++ )
           if( wk[xx] ) VV(ii+xx,jj,kk) = fj0[jj+xx*ny] ;
     }
   }

   free(wk) ; free(fj0) ; return ;
}

/*---------------------------------------------------------------------------
   Apply a z-axis shear to a 3D array: z -> z + a*x + b*y + s
-----------------------------------------------------------------------------*/

static void apply_zshear( float a , float b , float s ,
                          int nx , int ny , int nz , byte * v , Tmask * tm )
{
   byte * fj0 ;
   int   nx1=nx-1    , ny1=ny-1    , nz1=nz-1    , nxy=nx*ny ;
   float nx2=0.5*nx1 , ny2=0.5*ny1 , nz2=0.5*nz1 ;
   int ii,jj,kk ;
   float st ;
   int xnum , xx , xtop , *wk ;

   /* don't do anything if shift is too small */

   st = fabs(a)*nx2 + fabs(b)*ny2 + fabs(s) ; if( st < TSBOT ) return ;

   xnum = CACHE / (nz) ; if( xnum < 1 ) xnum = 1 ;
   fj0 = (byte *) malloc( xnum*nz ) ;
   wk  = (int *)   malloc( sizeof(int)*xnum ) ;

   for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii+=xnum ){
       xtop = MIN(nx-ii,xnum) ;
       for( xx=0 ; xx < xtop ; xx++ )
          wk[xx] = fabs(a*(ii+xx-nx2)+b*(jj-ny2)+s) > TSBOT
                   && TM_ZLINE(tm,ii+jj*nx+xx) ;
       for( kk=0; kk < nz; kk++ )
         for( xx=0 ; xx < xtop ; xx++ )
           if( wk[xx] ) fj0[kk+xx*nz] = VV(ii+xx,jj,kk) ;
       for( xx=0 ; xx < xtop ; xx++ )
         if( wk[xx] )
          wk[xx] = ts_shift_byte(nz, a*(ii+xx-nx2)+b*(jj-ny2)+s, fj0+xx*nz);
       for( kk=0; kk < nz; kk++ )
         for( xx=0 ; xx < xtop ; xx++ )
           if( wk[xx] ) VV(ii+xx,jj,kk) = fj0[kk+xx*nz] ;
     }
   }

   free(wk) ; free(fj0) ; return ;
}

/*---------------------------------------------------------------------------
   Apply a set of shears to a 3D array of bytes.
   Note that we assume that the dilation factors ("f") are all 1.
-----------------------------------------------------------------------------*/

static void apply_3shear( MCW_3shear shr ,
                          int nx, int ny, int nz, byte * vol , Tmask * tm )
{
   int qq ;
   float a , b , s ;

   if( ! ISVALID_3SHEAR(shr) ) return ;

   /* carry out a preliminary 180 flippo ? */

   if( shr.flip0 >= 0 ){
      switch( shr.flip0 + shr.flip1 ){
         case 1: flip_xy( nx,ny,nz,vol,tm ) ; break ;
         case 2: flip_xz( nx,ny,nz,vol,tm ) ; break ;
         case 3: flip_yz( nx,ny,nz,vol,tm ) ; break ;
      }
      tm = NULL ;  /* can't use twice */
   }

   /* apply each shear */

   for( qq=0 ; qq < 4 ; qq++ ){
      switch( shr.ax[qq] ){
         case 0:
            a = shr.scl[qq][1] ;
            b = shr.scl[qq][2] ;
            s = shr.sft[qq]    ;
            apply_xshear( a,b,s , nx,ny,nz , vol , tm ) ;
         break ;

         case 1:
            a = shr.scl[qq][0] ;
            b = shr.scl[qq][2] ;
            s = shr.sft[qq]    ;
            apply_yshear( a,b,s , nx,ny,nz , vol , tm ) ;
         break ;

         case 2:
            a = shr.scl[qq][0] ;
            b = shr.scl[qq][1] ;
            s = shr.sft[qq]    ;
            apply_zshear( a,b,s , nx,ny,nz , vol , tm ) ;
         break ;
      }

      tm = NULL ;  /* can't use twice */
   }

   return ;
}

/*---------------------------------------------------------------------------
  Rotate and translate a 3D volume.
-----------------------------------------------------------------------------*/

static void CREN_rotate(int   nx   , int   ny   , int   nz   ,
                        float xdel , float ydel , float zdel , byte * vol ,
                        int ax1,float th1, int ax2,float th2, int ax3,float th3,
                        int dcode , float dx , float dy , float dz , Tmask * tm )
{
   MCW_3shear shr ;

   if( nx < 2 || ny < 2 || nz < 2 || vol == NULL ) return ;

   if( xdel == 0.0 ) xdel = 1.0 ;
   if( ydel == 0.0 ) ydel = 1.0 ;
   if( zdel == 0.0 ) zdel = 1.0 ;

   if( th1 == 0.0 && th2 == 0.0 && th3 == 0.0 ){  /* nudge rotation */
      th1 = 1.e-6 ; th2 = 1.1e-6 ; th3 = 0.9e-6 ;
   }

   shr = rot_to_shear( ax1,-th1 , ax2,-th2 , ax3,-th3 ,
                       dcode,dx,dy,dz , xdel,ydel,zdel ) ;

   if( ! ISVALID_3SHEAR(shr) ){
      fprintf(stderr,"*** CREN_rotate: can't compute shear transformation!\n") ;
      return ;
   }

   /*****************************************/

   apply_3shear( shr , nx,ny,nz , vol , tm ) ;

   /*****************************************/

   return ;
}
