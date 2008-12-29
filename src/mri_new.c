/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** 7D SAFE ***/

/***-------------------------- get a new 2D image --------------------------***/

MRI_IMAGE *mri_new( int nx , int ny , MRI_TYPE kind )
{
   MRI_IMAGE *newim ;

   newim = mri_new_7D_generic( nx,ny , 1,1,1,1,1 , kind , TRUE ) ;
   return newim ;
}

/***-------------- get a new 3D image, but with no data inside -------------***/

MRI_IMAGE *mri_new_vol_empty( int nx , int ny , int nz , MRI_TYPE kind )
{
   MRI_IMAGE *newim ;
   newim = mri_new_7D_generic( nx,ny,nz , 1,1,1,1 , kind , FALSE ) ;
   return newim ;
}

/***------------------------ get a new 3D image ------------------------***/

MRI_IMAGE *mri_new_vol( int nx , int ny , int nz , MRI_TYPE kind )
{
   MRI_IMAGE *newim ;
   newim = mri_new_7D_generic( nx,ny,nz , 1,1,1,1 , kind , TRUE ) ;
   return newim ;
}

/***---------------------- make a new 7D image -------------------------***/

MRI_IMAGE *mri_new_7D_generic(
            int nx, int ny, int nz, int nt, int nu, int nv, int nw,
            MRI_TYPE kind , int make_space )
{
   MRI_IMAGE *newim ;
   int npix ;

ENTRY("mri_new_7D_generic") ;

   newim = (MRI_IMAGE *)calloc( 1, sizeof(MRI_IMAGE) ) ;

   if( newim == NULL ){
      fprintf( stderr , "malloc failure for new image pointer\n" ) ;
      MRI_FATAL_ERROR ;
   }

   if( nx < 1 ) nx = 1 ;  /* 18 Mar 2005: fix stupid user problems */
   if( ny < 1 ) ny = 1 ;
   if( nz < 1 ) nz = 1 ;
   if( nt < 1 ) nt = 1 ;
   if( nu < 1 ) nu = 1 ;
   if( nv < 1 ) nv = 1 ;
   if( nw < 1 ) nw = 1 ;

   newim->nx   = nx ;
   newim->ny   = ny ; newim->nxy   = nx*ny ;
   newim->nz   = nz ; newim->nxyz  = nx*ny*nz ;
   newim->nt   = nt ; newim->nxyzt = nx*ny*nz*nt ;
   newim->nu   = nu ;
   newim->nv   = nv ;
   newim->nw   = nw ; newim->nvox  = newim->nxyzt * nu*nv*nw ;

   newim->kind = kind ;
   newim->name = NULL ;

   newim->dx = newim->dy = newim->dz =
   newim->dt = newim->du = newim->dv = 1.0f;  /* default dimensions */

   newim->dw = -666.0f;  /* 05 Feb 2001 - flag that dimensions aren't set */

   newim->xo = newim->yo = newim->zo =
   newim->to = newim->uo = newim->vo = newim->wo = 0.0f;  /* default offsets */

   newim->was_swapped = 0 ;  /* 07 Mar 2002 - flag that bytes were swapped */
   newim->vdim        = 0 ;

#ifdef USE_MRI_LABELS
   newim->xlab[0] = '\0' ;          /* default labels */
   newim->ylab[0] = '\0' ;
   newim->zlab[0] = '\0' ;
   newim->tlab[0] = '\0' ;
   newim->ulab[0] = '\0' ;
   newim->vlab[0] = '\0' ;
   newim->wlab[0] = '\0' ;
#endif

   newim->fname   = NULL ;
   newim->foffset = newim->fondisk = 0 ;

   npix = newim->nvox ;

   switch( kind ){
      case MRI_byte:    newim->pixel_size = sizeof(byte)     ; break ;
      case MRI_short:   newim->pixel_size = sizeof(short)    ; break ;
      case MRI_int:     newim->pixel_size = sizeof(int)      ; break ;
      case MRI_float:   newim->pixel_size = sizeof(float)    ; break ;
      case MRI_double:  newim->pixel_size = sizeof(double)   ; break ;
      case MRI_complex: newim->pixel_size = sizeof(complex)  ; break ;
      case MRI_rgb:     newim->pixel_size = 3 * sizeof(byte) ; break ;
      case MRI_rgba:    newim->pixel_size = sizeof(rgba)     ; break ;
      case MRI_fvect:   newim->pixel_size = sizeof(float)    ;
                        newim->vdim       = 1                ; break ;

      default:
        fprintf( stderr , "mri_new: unrecognized image kind %d\n",(int)kind ) ;
        MRI_FATAL_ERROR ;
   }

   if( PRINT_TRACING ){
     char str[245] ;
     sprintf(str,"nx=%d ny=%d nz=%d kind=%d bytes=%u %s",
             nx,ny,nz,(int)kind,(unsigned int)newim->pixel_size*npix,
             make_space ? "(calloc)" : "(null)" ) ;
     STATUS(str) ;
   }

   if( make_space ) newim->im = calloc( newim->pixel_size , npix ) ;
   else             newim->im = NULL ;

   if( make_space && newim->im == NULL ){
     ERROR_message("malloc failure for image space: %u bytes\n",
                   (unsigned int)npix*newim->pixel_size);
     MRI_FATAL_ERROR ;
   }

   RETURN(newim) ;
}

/*---------------------------------------------------------------------*/
/* Should properly move old data around, if there is any. */

void mri_adjust_fvectim( MRI_IMAGE *im , int vdim )
{
   void *vpt ; int vdold ;

   if( im == NULL || im->kind != MRI_fvect || vdim < 1 ) return ;

   vdold          = im->vdim ;
   im->vdim       = vdim ;
   im->pixel_size = sizeof(float)*vdim ;

   mri_unpurge(im) ;

   if( im->im == NULL ){       /** no data, so create some **/

     vpt = calloc( im->pixel_size , im->nvox ) ;

   } else if( vdold < vdim ){  /** making each vector longer **/

     vpt = realloc( im->im , im->pixel_size*im->nvox ) ;
     if( vpt != NULL ){
       int nvox , ii,jj ; float *far ;
       far = (float *)vpt ; nvox = im->nvox ;
       for( ii=nvox-1 ; ii >= 0 ; ii-- ){  /* move each old vect to new spot */
         for( jj=0 ; jj < vdold ; jj++ ) far[vdim*ii+jj] = far[vdold*ii+jj] ;
         for(      ; jj < vdim  ; jj++ ) far[vdim*ii+jj] = 0.0f ;
       }
     }

   } else if( vdold > vdim ){  /** making each vector shorter **/

     vpt = calloc( im->pixel_size , im->nvox ) ;  /* new space */
     if( vpt != NULL ){
       int nvox , ii,jj ; float *far , *gar ;
       far = (float *)vpt ; gar = (float *)im->im ; nvox = im->nvox ;
       for( ii=0 ; ii < nvox ; ii++ ){  /* move each old vect to new spot */
         for( jj=0 ; jj < vdim ; jj++ ) far[vdim*ii+jj] = gar[vdold*ii+jj] ;
       }
     }
     free(im->im) ; /* will be replaced below */

   } else {                    /** nothing to be done?! **/

     vpt = im->im ;

   }

   /** make sure modified image has data space ready **/

   if( vpt == NULL ){
     fprintf(stderr,"malloc failure for fvectim space: %d bytes\n",
             im->pixel_size*im->nvox );
     MRI_FATAL_ERROR ;
   }

   im->im = vpt ; return ;
}

/*---------------------------------------------------------------------*/

MRI_IMAGE * mri_new_fvectim( int nx , int ny , int nz , int vdim )
{
   MRI_IMAGE *im ;

   im = mri_new_vol_empty( nx , ny , nz , MRI_fvect ) ;
   mri_adjust_fvectim( im , vdim ) ;
   return(im) ;
}
