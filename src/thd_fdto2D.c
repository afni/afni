/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------
  Routine to extract a slice (fixed value of 3rd dimension) from
    a previously set up FD_brick structure.
  November 1995: may return an image of any legal type, not
                 just shorts.
-----------------------------------------------------------------*/

MRI_IMAGE * FD_brick_to_mri( int kslice , int ival , FD_brick * br )
{
   MRI_IMAGE * im ;  /* output */
   register int ii,di,ei , jj,dj,ej , base , pp ;
   char * iar ;      /* brick in the input */
   MRI_TYPE typ ;

   /** desire a fake image **/

   if( ival < 0 ){
      im     = mri_new( br->n1 , br->n2 , MRI_short ) ;
      im->dx = br->del1 ;
      im->dy = br->del2 ;
      im->dz = br->del3 ;
      return im ;
   }

   /** otherwise, get ready for a real image **/

   if( ival >= br->dset->dblk->nvals ) return NULL ;

   iar = DSET_ARRAY(br->dset,ival) ;

   if( iar == NULL ){  /* if data needs to be loaded from disk */
      (void) THD_load_datablock( br->dset->dblk ) ;
      iar = DSET_ARRAY(br->dset,ival) ;
      if( iar == NULL ) return NULL ;
   }

   typ    = DSET_BRICK_TYPE(br->dset,ival) ;
   im     = mri_new( br->n1 , br->n2 , typ ) ;
   im->dx = br->del1 ;
   im->dy = br->del2 ;
   im->dz = br->del3 ;

   switch( typ ){

      default:             /* don't know what to do --> return nada */
         mri_free( im ) ;
         return NULL ;

      case MRI_byte:{
         register byte * ar  = MRI_BYTE_PTR(im) ;
         register byte * bar = (byte *) iar ;

         di = br->d1 ; dj = br->d2 ;  /* strides */
         ei = br->e1 ; ej = br->e2 ;  /* final indices */
         base = br->start + kslice * br->d3 ;

         pp = 0 ;
         for( jj=0 ; jj != ej ; jj += dj )
            for( ii=0 ; ii != ei ; ii += di ) ar[pp++] = bar[ii+(jj+base)] ;
      }
      break ;

      case MRI_short:{
         register short * ar  = MRI_SHORT_PTR(im) ;
         register short * bar = (short *) iar ;

         di = br->d1 ; dj = br->d2 ;  /* strides */
         ei = br->e1 ; ej = br->e2 ;  /* final indices */
         base = br->start + kslice * br->d3 ;

         pp = 0 ;
         for( jj=0 ; jj != ej ; jj += dj )
            for( ii=0 ; ii != ei ; ii += di ) ar[pp++] = bar[ii+(jj+base)] ;
      }
      break ;

      case MRI_float:{
         register float * ar  = MRI_FLOAT_PTR(im) ;
         register float * bar = (float *) iar ;

         di = br->d1 ; dj = br->d2 ;  /* strides */
         ei = br->e1 ; ej = br->e2 ;  /* final indices */
         base = br->start + kslice * br->d3 ;

         pp = 0 ;
         for( jj=0 ; jj != ej ; jj += dj )
            for( ii=0 ; ii != ei ; ii += di ) ar[pp++] = bar[ii+(jj+base)] ;
      }
      break ;

      case MRI_int:{
         register int * ar  = MRI_INT_PTR(im) ;
         register int * bar = (int *) iar ;

         di = br->d1 ; dj = br->d2 ;  /* strides */
         ei = br->e1 ; ej = br->e2 ;  /* final indices */
         base = br->start + kslice * br->d3 ;

         pp = 0 ;
         for( jj=0 ; jj != ej ; jj += dj )
            for( ii=0 ; ii != ei ; ii += di ) ar[pp++] = bar[ii+(jj+base)] ;
      }
      break ;

      case MRI_double:{
         register double * ar  = MRI_DOUBLE_PTR(im) ;
         register double * bar = (double *) iar ;

         di = br->d1 ; dj = br->d2 ;  /* strides */
         ei = br->e1 ; ej = br->e2 ;  /* final indices */
         base = br->start + kslice * br->d3 ;

         pp = 0 ;
         for( jj=0 ; jj != ej ; jj += dj )
            for( ii=0 ; ii != ei ; ii += di ) ar[pp++] = bar[ii+(jj+base)] ;
      }
      break ;

      case MRI_complex:{
         register complex * ar  = MRI_COMPLEX_PTR(im) ;
         register complex * bar = (complex *) iar ;

         di = br->d1 ; dj = br->d2 ;  /* strides */
         ei = br->e1 ; ej = br->e2 ;  /* final indices */
         base = br->start + kslice * br->d3 ;

         pp = 0 ;
         for( jj=0 ; jj != ej ; jj += dj )
            for( ii=0 ; ii != ei ; ii += di ) ar[pp++] = bar[ii+(jj+base)] ;
      }
      break ;

   }

   if( DSET_BRICK_FACTOR(br->dset,ival) != 0.0 ){
      MRI_IMAGE * qim ;
STATUS(" scaling to float");
      qim = mri_scale_to_float( DSET_BRICK_FACTOR(br->dset,ival) , im ) ;
      mri_free(im) ; im = qim ;
   }

   return im ;
}
