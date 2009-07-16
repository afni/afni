#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------
  Routine to extract a float version of a brick from a dataset
  -- RWCox - 10 Aug 2001
-----------------------------------------------------------------*/

MRI_IMAGE * THD_extract_float_brick( int iv , THD_3dim_dataset *dset )
{
   MRI_IMAGE *im ;
   register float *var , fac ;
   register int ii , nvox ;

ENTRY("THD_extract_float_brick") ;

   if( iv < 0 || !ISVALID_DSET(dset) || iv >= DSET_NVALS(dset) ) RETURN(NULL);

   im   = mri_new_conforming( DSET_BRICK(dset,iv) , MRI_float ) ;
   var  = MRI_FLOAT_PTR(im) ;
   nvox = DSET_NVOX(dset) ;

   /*-- extract/scale brick into var --*/

   switch( DSET_BRICK_TYPE(dset,iv) ){

      default:
        mri_free(im) ;
        ERROR_message("Can't handle sub-bricks of type %s\n",
                      MRI_TYPE_name[DSET_BRICK_TYPE(dset,iv)] ) ;
      RETURN(NULL) ;

      case MRI_short:{
        register short *dar = DSET_ARRAY(dset,iv) ;
        for( ii=0 ; ii < nvox ; ii++ ) var[ii] = dar[ii] ;
      }
      break ;

      case MRI_byte:{
        register byte  *dar = DSET_ARRAY(dset,iv) ;
        for( ii=0 ; ii < nvox ; ii++ ) var[ii] = dar[ii] ;
      }
      break ;

      case MRI_float:{
        register float *dar = DSET_ARRAY(dset,iv) ;
        for( ii=0 ; ii < nvox ; ii++ ) var[ii] = dar[ii] ;
      }
      break ;

      case MRI_complex:{
        register complex *dar = DSET_ARRAY(dset,iv) ;
        for( ii=0 ; ii < nvox ; ii++ ) var[ii] = CABS(dar[ii]) ;
      }
      break ;

#if 0
      case MRI_int:{
        register int   *dar = DSET_ARRAY(dset,iv) ;
        for( ii=0 ; ii < nvox ; ii++ ) var[ii] = dar[ii] ;
      }
      break ;

      case MRI_double:{
        register double *dar = DSET_ARRAY(dset,iv) ;
        for( ii=0 ; ii < nvox ; ii++ ) var[ii] = dar[ii] ;
      }
      break ;
#endif

      case MRI_rgb:{
        register byte *dar = DSET_ARRAY(dset,iv) ;
        for( ii=0 ; ii < nvox ; ii++ )
          var[ii] = 0.299*dar[3*ii] + 0.587*dar[3*ii+1] + 0.114*dar[3*ii+2] ;
      }
   }

   fac = DSET_BRICK_FACTOR(dset,iv) ; if( fac == 0.0f ) fac = 1.0f ;
   if( fac != 1.0f ){ for( ii=0 ; ii < nvox ; ii++ ) var[ii] *= fac ; }

   RETURN(im) ;
}
