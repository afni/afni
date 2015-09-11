#include "mrilib.h"

/*-----------------------------------------------------------------*/
/*! Convert this dataset to float format, in place.                */
/*  Scale factors will always be applied, even for floats.         */

void EDIT_floatize_dataset( THD_3dim_dataset *dset )
{
   int nvals , iv ;
   MRI_IMAGE *bim , *qim ;
   float      bfac, *qar ;

ENTRY("EDIT_floatize_dataset") ;

   if( !ISVALID_DSET(dset) ) EXRETURN ;

   DSET_mallocize(dset) ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){
     ERROR_message("Can't load dataset '%s' for floatize",DSET_BRIKNAME(dset));
     EXRETURN ;
   }

   nvals = DSET_NVALS(dset) ;
   for( iv=0 ; iv < nvals ; iv++ ){
     bim  = DSET_BRICK(dset,iv) ;
     bfac = DSET_BRICK_FACTOR(dset,iv) ;
     /* still "scale" a float dataset if there are factors to apply
        (as they are cleared after the loop)   11 Sep, 2015 [rickr] */
     if( bim->kind == MRI_float && (bfac == 1.0 || bfac == 0.0) )
        continue ; /* already floatized */
     qim  = mri_scale_to_float( bfac , bim ) ;
     qar  = MRI_FLOAT_PTR(qim) ;
     EDIT_substitute_brick( dset , iv , MRI_float , qar ) ;
     mri_clear_data_pointer(qim); mri_free(qim) ;
   }
   EDIT_dset_items( dset , ADN_brick_fac,NULL , ADN_none ) ;

   EXRETURN ;
}

/*-----------------------------------------------------------------*/
/*! Return type of all bricks, if they are the same, or -1 if not. */

int DSET_pure_type( THD_3dim_dataset *dset )
{
   int nvals , iv , typ ;

ENTRY("DSET_pure_type") ;

   if( !ISVALID_DSET(dset) ) RETURN(-1) ;

   nvals = DSET_NVALS(dset) ;
   typ   = (int)DSET_BRICK_TYPE(dset,0) ;
   for( iv=1 ; iv < nvals ; iv++ )
     if( (int)DSET_BRICK_TYPE(dset,iv) != typ ) RETURN(-1) ;

   RETURN(typ) ;
}
