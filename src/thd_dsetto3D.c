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

STATUS("make new image") ;
   im   = mri_new_conforming( DSET_BRICK(dset,iv) , MRI_float ) ;
   var  = MRI_FLOAT_PTR(im) ;
   nvox = DSET_NVOX(dset) ;

   if( DSET_ARRAY(dset,iv) == NULL ) DSET_load(dset) ;  /* 28 Jul 2015 */

   /*-- extract/scale brick into var --*/

   switch( DSET_BRICK_TYPE(dset,iv) ){

      default:
        mri_free(im) ;
        ERROR_message("Can't handle sub-bricks of type %s\n",
                      MRI_TYPE_name[DSET_BRICK_TYPE(dset,iv)] ) ;
      RETURN(NULL) ;

      case MRI_short:{
        register short *dar = DSET_ARRAY(dset,iv) ;
        if( dar == NULL ){ ERROR_message("NULL ptr in THD_extract_float_brick(%d)",iv); RETURN(im); }
        for( ii=0 ; ii < nvox ; ii++ ) var[ii] = dar[ii] ;
      }
      break ;

      case MRI_byte:{
        register byte  *dar = DSET_ARRAY(dset,iv) ;
        if( dar == NULL ){ ERROR_message("NULL ptr in THD_extract_float_brick(%d)",iv); RETURN(im); }
        for( ii=0 ; ii < nvox ; ii++ ) var[ii] = dar[ii] ;
      }
      break ;

      case MRI_float:{
        register float *dar = DSET_ARRAY(dset,iv) ;
        if( dar == NULL ){ ERROR_message("NULL ptr in THD_extract_float_brick(%d)",iv); RETURN(im); }
        for( ii=0 ; ii < nvox ; ii++ ) var[ii] = dar[ii] ;
      }
      break ;

      case MRI_complex:{
        register complex *dar = DSET_ARRAY(dset,iv) ;
        if( dar == NULL ){ ERROR_message("NULL ptr in THD_extract_float_brick(%d)",iv); RETURN(im); }
        for( ii=0 ; ii < nvox ; ii++ ) var[ii] = CABS(dar[ii]) ;
      }
      break ;

#if 0
      case MRI_int:{
        register int   *dar = DSET_ARRAY(dset,iv) ;
        if( dar == NULL ){ ERROR_message("NULL ptr in THD_extract_float_brick(%d)",iv); RETURN(im); }
        for( ii=0 ; ii < nvox ; ii++ ) var[ii] = dar[ii] ;
      }
      break ;

      case MRI_double:{
        register double *dar = DSET_ARRAY(dset,iv) ;
        if( dar == NULL ){ ERROR_message("NULL ptr in THD_extract_float_brick(%d)",iv); RETURN(im); }
        for( ii=0 ; ii < nvox ; ii++ ) var[ii] = dar[ii] ;
      }
      break ;
#endif

      case MRI_rgb:{
        register byte *dar = DSET_ARRAY(dset,iv) ;
        if( dar == NULL ){ ERROR_message("NULL ptr in THD_extract_float_brick(%d)",iv); RETURN(im); }
        for( ii=0 ; ii < nvox ; ii++ )
          var[ii] = 0.299*dar[3*ii] + 0.587*dar[3*ii+1] + 0.114*dar[3*ii+2] ;
      }
      break;
   }

   fac = DSET_BRICK_FACTOR(dset,iv) ; if( fac == 0.0f ) fac = 1.0f ;
   if( fac != 1.0f ){ for( ii=0 ; ii < nvox ; ii++ ) var[ii] *= fac ; }

   RETURN(im) ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * THD_extract_double_brick( int iv , THD_3dim_dataset *dset )
{
   MRI_IMAGE *im ;
   register float  fac ;
   register double *var;
   register int ii , nvox ;

ENTRY("THD_extract_double_brick") ;

   if( iv < 0 || !ISVALID_DSET(dset) || iv >= DSET_NVALS(dset) ) RETURN(NULL);

   im   = mri_new_conforming( DSET_BRICK(dset,iv) , MRI_double ) ;
   var  = MRI_DOUBLE_PTR(im) ;
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
#endif

      case MRI_double:{
        register double *dar = DSET_ARRAY(dset,iv) ;
        for( ii=0 ; ii < nvox ; ii++ ) var[ii] = dar[ii] ;
      }
      break ;

      case MRI_rgb:{
        register byte *dar = DSET_ARRAY(dset,iv) ;
        for( ii=0 ; ii < nvox ; ii++ )
          var[ii] = 0.299*dar[3*ii] + 0.587*dar[3*ii+1] + 0.114*dar[3*ii+2] ;
      }
      break;
   }

   fac = DSET_BRICK_FACTOR(dset,iv) ; if( fac == 0.0f ) fac = 1.0f ;
   if( fac != 1.0f ){ for( ii=0 ; ii < nvox ; ii++ ) var[ii] *= fac ; }

   RETURN(im) ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * THD_extract_int_brick( int iv , THD_3dim_dataset *dset )
{
   MRI_IMAGE *im ;
   register float  fac ;
   register int *var;
   register int ii , nvox ;

ENTRY("THD_extract_int_brick") ;

   if( iv < 0 || !ISVALID_DSET(dset) || iv >= DSET_NVALS(dset) ) RETURN(NULL);

   im   = mri_new_conforming( DSET_BRICK(dset,iv) , MRI_int ) ;
   var  = MRI_INT_PTR(im) ;
   nvox = DSET_NVOX(dset) ;

   /*-- extract/scale brick into var --*/
   fac = DSET_BRICK_FACTOR(dset,iv) ; if( fac == 0.0f ) fac = 1.0f ;

   switch( DSET_BRICK_TYPE(dset,iv) ){

      default:
        mri_free(im) ;
        ERROR_message("Can't handle sub-bricks of type %s\n",
                      MRI_TYPE_name[DSET_BRICK_TYPE(dset,iv)] ) ;
      RETURN(NULL) ;

      case MRI_short:{
        register short *dar = DSET_ARRAY(dset,iv) ;
        if (fac == 1.0f) {
         for( ii=0 ; ii < nvox ; ii++ ) var[ii] = (int)dar[ii] ;
        } else {
         for( ii=0 ; ii < nvox ; ii++ ) var[ii] = (int)(fac*dar[ii]) ;
        }
      }
      break ;

      case MRI_byte:{
        register byte  *dar = DSET_ARRAY(dset,iv) ;
        if (fac == 1.0f) {
         for( ii=0 ; ii < nvox ; ii++ ) var[ii] = (int)dar[ii] ;
        } else {
         for( ii=0 ; ii < nvox ; ii++ ) var[ii] = (int)(fac*dar[ii]) ;
        }
      }
      break ;

      case MRI_float:{
        register float *dar = DSET_ARRAY(dset,iv) ;
        if (fac == 1.0f) {
         for( ii=0 ; ii < nvox ; ii++ ) var[ii] = (int)dar[ii] ;
        } else {
         for( ii=0 ; ii < nvox ; ii++ ) var[ii] = (int)(fac*dar[ii]) ;
        }
      }
      break ;

      case MRI_complex:{
        register complex *dar = DSET_ARRAY(dset,iv) ;
        if (fac == 1.0f) {
         for( ii=0 ; ii < nvox ; ii++ ) var[ii] = (int)CABS(dar[ii]) ;
        } else {
         for( ii=0 ; ii < nvox ; ii++ ) var[ii] = (int)(fac*CABS(dar[ii])) ;
        }
      }
      break ;

#if 0
      case MRI_int:{
        register int   *dar = DSET_ARRAY(dset,iv) ;
        if (fac == 1.0f) {
         for( ii=0 ; ii < nvox ; ii++ ) var[ii] = (int)dar[ii] ;
        } else {
         for( ii=0 ; ii < nvox ; ii++ ) var[ii] = (int)(fac*dar[ii]) ;
        }
      }
      break ;
#endif

      case MRI_double:{
        register double *dar = DSET_ARRAY(dset,iv) ;
        if (fac == 1.0f) {
         for( ii=0 ; ii < nvox ; ii++ ) var[ii] = (int)dar[ii] ;
        } else {
         for( ii=0 ; ii < nvox ; ii++ ) var[ii] = (int)(fac*dar[ii]) ;
        }
      }
      break ;

      case MRI_rgb:{
        register byte *dar = DSET_ARRAY(dset,iv) ;
        if (fac == 1.0f) {
         for( ii=0 ; ii < nvox ; ii++ )
            var[ii] = (int)(0.299*dar[3*ii] +
                            0.587*dar[3*ii+1] + 0.114*dar[3*ii+2] );
        } else { /* this should not happen... */
         for( ii=0 ; ii < nvox ; ii++ )
            var[ii] = (int)((0.299*dar[3*ii] +
                             0.587*dar[3*ii+1] + 0.114*dar[3*ii+2] )*fac);  }
      }
      break ;
   }


   RETURN(im) ;
}

/*----------------------------------------------------------------------------*/

/*
   Get copy contents of sub-brick iv into an integer array.
   if iv == -1, get the entire dset
   NO ROUNDING is done. Only type casting.
*/
int *THD_extract_to_int( int iv , THD_3dim_dataset *dset )
{
   MRI_IMAGE *im ;
   int *var=NULL, *vv=NULL;
   register int ii , nvox ;


   ENTRY("THD_extract_to_int") ;

   if (!dset) RETURN(var);
   if (iv >= 0) {
      if (!(im = THD_extract_int_brick(iv, dset))) RETURN(var);
      var = MRI_INT_PTR(im);mri_fix_data_pointer(NULL, im);
                            mri_free(im);im=NULL;
   } else if (iv == -1) {
      if (!(var = (int *)calloc(DSET_NVOX(dset)*DSET_NVALS(dset),sizeof(int)))){
         ERROR_message("Failed to allocate");
         RETURN(NULL);
      }
      for (ii=0; ii<DSET_NVALS(dset); ++ii) {
         if (!(im = THD_extract_int_brick(ii, dset))) {
            ERROR_message("Failed toextract sb %d from dset", ii);
            if (var) free(var);
            RETURN(NULL);
         }
         vv = MRI_INT_PTR(im);
         memcpy(var+ii*DSET_NVOX(dset),vv, sizeof(int)*DSET_NVOX(dset));
         mri_free(im);im=NULL;
      }
   } else {
      ERROR_message("Bad value of %d\n", iv);
   }
   RETURN(var);
}

/*----------------------------------------------------------------------------*/

/*
   Get copy contents of sub-brick iv into a float array.
   if iv == -1, get the entire dset
*/
float *THD_extract_to_float( int iv , THD_3dim_dataset *dset )
{
   MRI_IMAGE *im ;
   float *var=NULL, *vv=NULL;
   register int ii , nvox ;

   ENTRY("THD_extract_to_float") ;

   if (!dset) RETURN(var);
   if (iv >= 0) {
      if (!(im = THD_extract_float_brick(iv, dset))) RETURN(var);
      var = MRI_FLOAT_PTR(im);mri_fix_data_pointer(NULL, im);
                              mri_free(im);im=NULL;
   } else if (iv == -1) {
      if (!(var = (float *)calloc(DSET_NVOX(dset)*DSET_NVALS(dset),
                                  sizeof(float)))){
         ERROR_message("Failed to allocate");
         RETURN(NULL);
      }
      for (ii=0; ii<DSET_NVALS(dset); ++ii) {
         if (!(im = THD_extract_float_brick(ii, dset))) {
            ERROR_message("Failed toextract sb %d from dset", ii);
            if (var) free(var);
            RETURN(NULL);
         }
         vv = MRI_FLOAT_PTR(im);
         memcpy(var+ii*DSET_NVOX(dset),vv, sizeof(float)*DSET_NVOX(dset));
         mri_free(im);im=NULL;
      }
   } else {
      ERROR_message("Bad value of %d\n", iv);
   }

   RETURN(var);
}

/*----------------------------------------------------------------------------*/

/*
   Get copy contents of sub-brick iv into an double array.
   if iv == -1, get the entire dset
*/
double *THD_extract_to_double( int iv , THD_3dim_dataset *dset )
{
   MRI_IMAGE *im ;
   double *var=NULL, *vv=NULL;
   register int ii , nvox ;

   ENTRY("THD_extract_to_double") ;

   if (!dset) RETURN(var);
   if (iv >= 0) {
      if (!(im = THD_extract_double_brick(iv, dset))) RETURN(var);
      var = MRI_DOUBLE_PTR(im);mri_fix_data_pointer(NULL, im);
                              mri_free(im);im=NULL;
   } else if (iv == -1) {
      if (!(var = (double *)calloc(DSET_NVOX(dset)*DSET_NVALS(dset),
                                  sizeof(double)))){
         ERROR_message("Failed to allocate");
         RETURN(NULL);
      }
      for (ii=0; ii<DSET_NVALS(dset); ++ii) {
         if (!(im = THD_extract_double_brick(ii, dset))) {
            ERROR_message("Failed toextract sb %d from dset", ii);
            if (var) free(var);
            RETURN(NULL);
         }
         vv = MRI_DOUBLE_PTR(im);
         memcpy(var+ii*DSET_NVOX(dset),vv, sizeof(double)*DSET_NVOX(dset));
         mri_free(im);im=NULL;
      }
   } else {
      ERROR_message("Bad value of %d\n", iv);
   }

   RETURN(var);
}
