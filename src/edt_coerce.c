#include "mrilib.h"

#undef  AFNI_DEBUG
#undef  CLUST_DEBUG
#define STATUS(x) /* nada */
#define ENTRY(x)  /* nada */
#define EXRETURN  return
#define RETURN(x) return(x)

#undef ROUND
#ifndef NO_RINT
#  define ROUND(qq)   rint((qq))
#else
#  define ROUND(qq)   ((int)(qq))
#endif

/*------------------------------------------------------------------------
  convert one volume to another type
     nxy   = # voxels
     itype = input datum type
     ivol  = pointer to input volume
     otype = output datum type
     ovol  = pointer to output volume (must have space allocated already!)
--------------------------------------------------------------------------*/

void EDIT_coerce_type( int nxyz , int itype,void *ivol , int otype,void *ovol )
{
   register int ii ;

   complex * cin , * cout ;
   short   * sin , * sout ;
   float   * fin , * fout ;
   byte    * bin , * bout ;

ENTRY("EDIT_coerce_type") ;
#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"voxels=%d input type=%s output type=%s",
          nxyz, MRI_TYPE_name[itype],MRI_TYPE_name[otype]) ;
  STATUS(str) ; }
#endif


   if( nxyz <= 0 || ivol == NULL || ovol == NULL ) EXRETURN ;

   switch( itype ){
      default: EXRETURN ;
      case MRI_complex:  cin = (complex *) ivol ; break ;
      case MRI_short  :  sin = (short   *) ivol ; break ;
      case MRI_float  :  fin = (float   *) ivol ; break ;
      case MRI_byte   :  bin = (byte    *) ivol ; break ;
   }
   switch( otype ){
      default: EXRETURN ;
      case MRI_complex:  cout = (complex *) ovol ; break ;
      case MRI_short  :  sout = (short   *) ovol ; break ;
      case MRI_float  :  fout = (float   *) ovol ; break ;
      case MRI_byte   :  bout = (byte    *) ovol ; break ;
   }

   switch( otype ){

      /*** outputs are shorts ***/

      case MRI_short:
         switch( itype ){
            case MRI_short:   /* inputs are shorts */
               for( ii=0 ; ii < nxyz ; ii++ ) sout[ii] = sin[ii] ;
               EXRETURN ;
            case MRI_float:   /* inputs are floats */
               for( ii=0 ; ii < nxyz ; ii++ ) sout[ii] = ROUND(fin[ii]) ;
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               for( ii=0 ; ii < nxyz ; ii++ ) sout[ii] = bin[ii] ;
               EXRETURN ;
            case MRI_complex:    /* inputs are complex */
               for( ii=0 ; ii < nxyz ; ii++ ) sout[ii] = ROUND(CABS(cin[ii])) ;
               EXRETURN ;
         }
         EXRETURN ;

      /*** outputs are floats ***/

      case MRI_float:
         switch( itype ){
            case MRI_short:   /* inputs are shorts */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = sin[ii] ;
               EXRETURN ;
            case MRI_float:   /* inputs are floats */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = fin[ii] ;
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = bin[ii] ;
               EXRETURN ;
            case MRI_complex:    /* inputs are complex */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = CABS(cin[ii]) ;
               EXRETURN ;
         }
         EXRETURN ;

      /*** outputs are bytes ***/

      case MRI_byte:
         switch( itype ){
            case MRI_short:   /* inputs are shorts */
               for( ii=0 ; ii < nxyz ; ii++ ) bout[ii] = SHORT_TO_BYTE(sin[ii]) ;
               EXRETURN ;
            case MRI_float:   /* inputs are floats */
               for( ii=0 ; ii < nxyz ; ii++ ) bout[ii] = FLOAT_TO_BYTE(fin[ii]) ;
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               for( ii=0 ; ii < nxyz ; ii++ ) bout[ii] = bin[ii] ;
               EXRETURN ;
            case MRI_complex:{    /* inputs are complex */
               float val ;
               for( ii=0 ; ii < nxyz ; ii++ ){ val = CABS(cin[ii]) ;
                                               bout[ii] = FLOAT_TO_BYTE(val) ; }
            }
            EXRETURN ;
         }
         EXRETURN ;

      /*** outputs are complex ***/

      case MRI_complex:
         switch( itype ){
            case MRI_short:   /* inputs are shorts */
               for( ii=0 ; ii < nxyz ; ii++ )
                  cout[ii].r = sin[ii] , cout[ii].i = 0.0 ;
               EXRETURN ;
            case MRI_float:   /* inputs are floats */
               for( ii=0 ; ii < nxyz ; ii++ )
                  cout[ii].r = fin[ii] , cout[ii].i = 0.0 ;
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               for( ii=0 ; ii < nxyz ; ii++ )
                  cout[ii].r = bin[ii] , cout[ii].i = 0.0 ;
               EXRETURN ;
            case MRI_complex:    /* inputs are complex */
               for( ii=0 ; ii < nxyz ; ii++ ) cout[ii] = cin[ii] ;
               EXRETURN ;
         }
         EXRETURN ;
   }
   EXRETURN ;
}

/*------------------------------------------------------------------------
  convert one volume to another type, scaling as we go
     nxyz  = # voxels
     scl   = scale factor (if zero, same as calling EDIT_coerce_type)
     itype = input datum type
     ivol  = pointer to input volume
     otype = output datum type
     ovol  = pointer to output volume (again, must be pre-malloc-ed)
--------------------------------------------------------------------------*/

void EDIT_coerce_scale_type( int nxyz , float scl ,
                             int itype,void *ivol , int otype,void *ovol )
{
   register int ii ;
   register float fac = scl , val ;

   complex * cin , * cout ;
   short   * sin , * sout ;
   float   * fin , * fout ;
   byte    * bin , * bout ;

ENTRY("EDIT_coerce_scale_type") ;
#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"voxels=%d scale=%g input type=%s output type=%s",
          nxyz,scl , MRI_TYPE_name[itype],MRI_TYPE_name[otype]) ;
  STATUS(str) ; }
#endif

   if( nxyz <= 0 || ivol == NULL || ovol == NULL ) EXRETURN ;

   if( fac == 0.0 || fac == 1.0 ){
      EDIT_coerce_type( nxyz , itype,ivol , otype,ovol ) ;
      EXRETURN ;
   }

   switch( itype ){
      default: EXRETURN ;
      case MRI_complex:  cin = (complex *) ivol ; break ;
      case MRI_short  :  sin = (short   *) ivol ; break ;
      case MRI_float  :  fin = (float   *) ivol ; break ;
      case MRI_byte   :  bin = (byte    *) ivol ; break ;
   }
   switch( otype ){
      default: EXRETURN ;
      case MRI_complex:  cout = (complex *) ovol ; break ;
      case MRI_short  :  sout = (short   *) ovol ; break ;
      case MRI_float  :  fout = (float   *) ovol ; break ;
      case MRI_byte   :  bout = (byte    *) ovol ; break ;
   }

   switch( otype ){

      /*** outputs are shorts ***/

      case MRI_short:
         switch( itype ){
            case MRI_short:   /* inputs are shorts */
               for( ii=0 ; ii < nxyz ; ii++ ) sout[ii] = ROUND(fac*sin[ii]) ;
               EXRETURN ;
            case MRI_float:   /* inputs are floats */
               for( ii=0 ; ii < nxyz ; ii++ ) sout[ii] = ROUND(fac*fin[ii]) ;
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               for( ii=0 ; ii < nxyz ; ii++ ) sout[ii] = ROUND(fac*bin[ii]) ;
               EXRETURN ;
            case MRI_complex:    /* inputs are complex */
               for( ii=0 ; ii < nxyz ; ii++ ) sout[ii] = ROUND(fac*CABS(cin[ii])) ;
               EXRETURN ;
         }
         EXRETURN ;

      /*** outputs are floats ***/

      case MRI_float:
         switch( itype ){
            case MRI_short:   /* inputs are shorts */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = fac*sin[ii] ;
               EXRETURN ;
            case MRI_float:   /* inputs are floats */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = fac*fin[ii] ;
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = fac*bin[ii] ;
               EXRETURN ;
            case MRI_complex:    /* inputs are complex */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = fac*CABS(cin[ii]) ;
               EXRETURN ;
         }
         EXRETURN ;

      /*** outputs are bytes ***/

      case MRI_byte:
         switch( itype ){
            case MRI_short:   /* inputs are shorts */
               for( ii=0 ; ii < nxyz ; ii++ ){
                  val = fac*sin[ii] ; bout[ii] = FLOAT_TO_BYTE(val) ;
               }
               EXRETURN ;
            case MRI_float:   /* inputs are floats */
               for( ii=0 ; ii < nxyz ; ii++ ){
                  val = fac*fin[ii] ; bout[ii] = FLOAT_TO_BYTE(val) ;
               }
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               for( ii=0 ; ii < nxyz ; ii++ ){
                  val = fac*bin[ii] ; bout[ii] = FLOAT_TO_BYTE(val) ;
               }
               EXRETURN ;
            case MRI_complex:{    /* inputs are complex */
               for( ii=0 ; ii < nxyz ; ii++ ){
                  val = fac*CABS(cin[ii]) ; bout[ii] = FLOAT_TO_BYTE(val) ;
               }
            }
            EXRETURN ;
         }
         EXRETURN ;

      /*** outputs are complex ***/

      case MRI_complex:
         switch( itype ){
            case MRI_short:   /* inputs are shorts */
               for( ii=0 ; ii < nxyz ; ii++ )
                  cout[ii].r = fac*sin[ii] , cout[ii].i = 0.0 ;
               EXRETURN ;
            case MRI_float:   /* inputs are floats */
               for( ii=0 ; ii < nxyz ; ii++ )
                  cout[ii].r = fac*fin[ii] , cout[ii].i = 0.0 ;
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               for( ii=0 ; ii < nxyz ; ii++ )
                  cout[ii].r = fac*bin[ii] , cout[ii].i = 0.0 ;
               EXRETURN ;
            case MRI_complex:    /* inputs are complex */
               for( ii=0 ; ii < nxyz ; ii++ )
                  cout[ii].r = fac*cin[ii].r , cout[ii].i = fac*cin[ii].i ;
               EXRETURN ;
         }
         EXRETURN ;
   }
   EXRETURN ;
}

/*------------------------------------------------------------------------
  Convert one volume to another type, autoscaling:
     nxy   = # voxels
     itype = input datum type
     ivol  = pointer to input volume
     otype = output datum type
     ovol  = pointer to output volume (again, must be pre-malloc-ed)
  Return value is the scaling factor used (0.0 --> no scaling).
--------------------------------------------------------------------------*/

float EDIT_coerce_autoscale( int nxyz ,
                             int itype,void *ivol , int otype,void *ovol )
{
   float fac=0.0 , top ;

ENTRY("EDIT_coerce_autoscale") ;

   if( MRI_IS_INT_TYPE(otype) ){
      top = MCW_vol_amax( nxyz,1,1 , itype,ivol ) ;
      fac = (top > MRI_TYPE_maxval[otype]) ? MRI_TYPE_maxval[otype]/top : 0.0 ;
   }

   EDIT_coerce_scale_type( nxyz , fac , itype,ivol , otype,ovol ) ;
   RETURN( fac );
}
