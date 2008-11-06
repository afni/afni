/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

#undef  ROUND
#define ROUND(qq)   rint((qq)+0.00001)

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

   complex *cin , *cout ;
   short   *sin , *sout ;
   float   *fin , *fout ;
   byte    *bin , *bout ;
   double  *din , *dout ;  /* 10 Jan 1999 */

ENTRY("EDIT_coerce_type") ;
#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"voxels=%d input type=%s output type=%s",
          nxyz, MRI_TYPE_name[itype],MRI_TYPE_name[otype]) ;
  STATUS(str) ; }
#endif

   if( nxyz <= 0 || ivol == NULL || ovol == NULL ) EXRETURN ;

   switch( itype ){
      default:
        fprintf(stderr,"** Unknown itype=%d in EDIT_coerce_type\n",itype);
      EXRETURN ;
      case MRI_complex:  cin = (complex *) ivol ; break ;
      case MRI_short  :  sin = (short   *) ivol ; break ;
      case MRI_float  :  fin = (float   *) ivol ; break ;
      case MRI_byte   :  bin = (byte    *) ivol ; break ;
      case MRI_double :  din = (double  *) ivol ; break ;
   }
   switch( otype ){
      default:
        fprintf(stderr,"** Unknown otype=%d in EDIT_coerce_type\n",otype);
      EXRETURN ;
      case MRI_complex:  cout = (complex *) ovol ; break ;
      case MRI_short  :  sout = (short   *) ovol ; break ;
      case MRI_float  :  fout = (float   *) ovol ; break ;
      case MRI_byte   :  bout = (byte    *) ovol ; break ;
      case MRI_double :  dout = (double  *) ovol ; break ;
   }

   switch( otype ){

      /*** outputs are shorts ***/

      case MRI_short:
         switch( itype ){
            case MRI_short:   /* inputs are shorts */
               memcpy( sout , sin , sizeof(short)*nxyz ) ;
               EXRETURN ;
            case MRI_float:   /* inputs are floats */
               for( ii=0 ; ii < nxyz ; ii++ ) sout[ii] = ROUND(fin[ii]) ;
               EXRETURN ;
            case MRI_double:   /* inputs are doubles */
               for( ii=0 ; ii < nxyz ; ii++ ) sout[ii] = ROUND(din[ii]) ;
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
               memcpy( fout , fin , sizeof(float)*nxyz ) ;
               EXRETURN ;
            case MRI_double:    /* inputs are doubles */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = din[ii] ;
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = bin[ii] ;
               EXRETURN ;
            case MRI_complex:    /* inputs are complex */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = CABS(cin[ii]) ;
               EXRETURN ;
         }
         EXRETURN ;

      /*** outputs are doubles ***/

      case MRI_double:
         switch( itype ){
            case MRI_short:   /* inputs are shorts */
               for( ii=0 ; ii < nxyz ; ii++ ) dout[ii] = sin[ii] ;
               EXRETURN ;
            case MRI_float:   /* inputs are floats */
               for( ii=0 ; ii < nxyz ; ii++ ) dout[ii] = fin[ii] ;
               EXRETURN ;
            case MRI_double:   /* inputs are doubles */
               memcpy( dout , din , sizeof(double)*nxyz ) ;
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               for( ii=0 ; ii < nxyz ; ii++ ) dout[ii] = bin[ii] ;
               EXRETURN ;
            case MRI_complex:    /* inputs are complex */
               for( ii=0 ; ii < nxyz ; ii++ ) dout[ii] = CABS(cin[ii]) ;
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
            case MRI_double:   /* inputs are doubles */
               for( ii=0 ; ii < nxyz ; ii++ ) bout[ii] = FLOAT_TO_BYTE(din[ii]) ;
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               memcpy( bout , bin , sizeof(byte)*nxyz ) ;
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
            case MRI_double:   /* inputs are doubles */
               for( ii=0 ; ii < nxyz ; ii++ )
                  cout[ii].r = din[ii] , cout[ii].i = 0.0 ;
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               for( ii=0 ; ii < nxyz ; ii++ )
                  cout[ii].r = bin[ii] , cout[ii].i = 0.0 ;
               EXRETURN ;
            case MRI_complex:    /* inputs are complex */
               memcpy( cout , cin , sizeof(complex)*nxyz ) ;
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

   complex *cin , *cout ;
   short   *sin , *sout ;
   float   *fin , *fout ;
   byte    *bin , *bout ;
   double  *din , *dout ;   /* 11 Jan 1999 */

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
      default:
        fprintf(stderr,"** Unknown itype=%d in EDIT_coerce_scale_type\n",itype);
      EXRETURN ;
      case MRI_complex:  cin = (complex *) ivol ; break ;
      case MRI_short  :  sin = (short   *) ivol ; break ;
      case MRI_float  :  fin = (float   *) ivol ; break ;
      case MRI_byte   :  bin = (byte    *) ivol ; break ;
      case MRI_double :  din = (double  *) ivol ; break ;
   }
   switch( otype ){
      default:
        fprintf(stderr,"** Unknown otype=%d in EDIT_coerce_scale_type\n",otype);
      EXRETURN ;
      case MRI_complex:  cout = (complex *) ovol ; break ;
      case MRI_short  :  sout = (short   *) ovol ; break ;
      case MRI_float  :  fout = (float   *) ovol ; break ;
      case MRI_byte   :  bout = (byte    *) ovol ; break ;
      case MRI_double :  dout = (double  *) ovol ; break ;
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
            case MRI_double:   /* inputs are double */
               for( ii=0 ; ii < nxyz ; ii++ ) sout[ii] = ROUND(fac*din[ii]) ;
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
            case MRI_double:   /* inputs are doubles */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = fac*din[ii] ;
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = fac*bin[ii] ;
               EXRETURN ;
            case MRI_complex:    /* inputs are complex */
               for( ii=0 ; ii < nxyz ; ii++ ) fout[ii] = fac*CABS(cin[ii]) ;
               EXRETURN ;
         }
         EXRETURN ;

      /*** outputs are doubles ***/

      case MRI_double:
         switch( itype ){
            case MRI_short:   /* inputs are shorts */
               for( ii=0 ; ii < nxyz ; ii++ ) dout[ii] = fac*sin[ii] ;
               EXRETURN ;
            case MRI_float:   /* inputs are floats */
               for( ii=0 ; ii < nxyz ; ii++ ) dout[ii] = fac*fin[ii] ;
               EXRETURN ;
            case MRI_double:   /* inputs are doubles */
               for( ii=0 ; ii < nxyz ; ii++ ) dout[ii] = fac*din[ii] ;
               EXRETURN ;
            case MRI_byte:    /* inputs are bytes */
               for( ii=0 ; ii < nxyz ; ii++ ) dout[ii] = fac*bin[ii] ;
               EXRETURN ;
            case MRI_complex:    /* inputs are complex */
               for( ii=0 ; ii < nxyz ; ii++ ) dout[ii] = fac*CABS(cin[ii]) ;
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
            case MRI_double:   /* inputs are doubles */
               for( ii=0 ; ii < nxyz ; ii++ ){
                  val = fac*din[ii] ; bout[ii] = FLOAT_TO_BYTE(val) ;
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
            case MRI_double:   /* inputs are doubles */
               for( ii=0 ; ii < nxyz ; ii++ )
                  cout[ii].r = fac*din[ii] , cout[ii].i = 0.0 ;
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

/*------------------------------------------------------------------------
  Convert data to another type, scaling, if needed.    12 Dec 2005 [rickr]

  Scaling occurs when the output type is integral and either has a
  maximum value above limit (or the natural limit, if limit is zero),
  or has resulting values that are not integers.

  So for output integers not to be scaled, they must be bounded by
  limit and come from integral values.

     nxy   = # values
     itype = input data type
     ivol  = pointer to input data (memory must exist)
     otype = output data type
     ovol  = pointer to output data (memory must exist)
     limit = limit on magnitude of output data (0 to use natural limit)

  Return value is the scaling factor used (0.0 --> no scaling).
--------------------------------------------------------------------------*/

float EDIT_convert_dtype( int nxyz , int itype,void *ivol ,
                                     int otype,void *ovol , int limit )
{
   float fac=0.0 , top, olimit ;

ENTRY("EDIT_convert_dtype") ;

   if( MRI_IS_INT_TYPE(otype) ){
      olimit = (limit > 0) ? limit : MRI_TYPE_maxval[otype];
      top = MCW_vol_amax( nxyz,1,1 , itype,ivol ) ;
      if( top > olimit || !is_integral_data(nxyz, itype, ivol) )
          fac = olimit/top ;
   }

   EDIT_coerce_scale_type( nxyz , fac , itype,ivol , otype,ovol ) ;
   RETURN( fac );
}

/*------------------------------------------------------------------------
  If the data values are real-based, check to see if the values
  can be represented as int.
 *------------------------------------------------------------------------*/
int is_integral_data( int nxyz , int dtype, void *vol )
{
    int c ;

ENTRY("is_integral_data") ;

    if( dtype == MRI_complex )  /* do not process for now */
        RETURN(0);
    else if( dtype == MRI_float )
    {
      float * dptr = (float *)vol;
      float * dend = (float *)vol + nxyz;
      for( ; dptr < dend ; dptr ++ )
          if( (float)(int)*dptr != *dptr )
              RETURN(0);  /* then not integral */
    }
    else if( dtype == MRI_double )
    {
      double * dptr = (double *)vol;
      double * dend = (double *)vol + nxyz;
      for( ; dptr < dend ; dptr ++ )
          if( (double)(int)*dptr != *dptr )
              RETURN(0); /* then not integral */
    }

    RETURN(1);  /* so yes */
}

/*-----------------------------------------------------------------------*/

void EDIT_clip_float( float top , int nxyz , float * vol )
{
   int ii ;
   float bot ;

ENTRY("EDIT_clip_float") ;

   if( top <= 0.0 || nxyz <= 0 || vol == NULL ) EXRETURN ;

   bot = -top ;

   for( ii=0 ; ii < nxyz ; ii++ )
           if( vol[ii] > top ) vol[ii] = top ;
      else if( vol[ii] < bot ) vol[ii] = bot ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*!
  Convert one volume to another type, autoscaling:
     nxy   = # voxels
     itype = input datum type
     ivol  = pointer to input volume
     otype = output datum type
     ovol  = pointer to output volume (again, must be pre-malloc-ed)
  Return value is the scaling factor used (0.0 --> no scaling).
*/

float EDIT_coerce_autoscale_new( int nxyz , int itype ,
                                 void *ivol , int otype , void *ovol )
{
  float fac=0.0 , top ;

ENTRY("EDIT_coerce_autoscale_new") ;

  if( MRI_IS_INT_TYPE(otype) ){
    top = MCW_vol_amax( nxyz,1,1 , itype,ivol ) ;
    if (top == 0.0)  fac = 0.0;
    else  fac = MRI_TYPE_maxval[otype]/top;
  }

  EDIT_coerce_scale_type( nxyz , fac , itype,ivol , otype,ovol ) ;
  RETURN ( fac );
}

/*---------------------------------------------------------------------------*/
/*! Compute a measure of how good a scaled short array fits a float array.
    Smaller is better (0 is perfection, 1 is catastrophe).
*//*-------------------------------------------------------------------------*/

float EDIT_scale_misfit( int nxyz , float fac , short *sar , float *far )
{
   float sf , ff , sum=0.0f ;
   int ii , nf=0 ;

ENTRY("EDIT_scale_misfit") ;

   if( nxyz <= 0 || sar == NULL || far == NULL ) RETURN(0.0f) ;

   if( fac == 0.0f ) fac = 1.0f ;

   for( ii=0 ; ii < nxyz ; ii++ ){
     ff = far[ii] ; if( ff == 0.0f ) continue ;
     sf = fac*sar[ii] ;
     if( sf == 0.0f ){
       sum += 1.0f ;
     } else {
       sf = fabsf((sf-ff)/ff) ; if( sf > 1.0f ) sf = 1.0f ; sum += sf ;
     }
     nf++ ;
   }

   if( nf > 0 ) sum /= nf ;
   RETURN(sum) ;
}

/*---------------------------------------------------------------------------*/

void EDIT_misfit_report( char *name, int ib,
                         int nxyz, float fac, short *sar, float *far )
{
   float mf ; int im ;
   static char *msg[4] = { "* Caution"  , "** Warning"     ,
                           "*** Beware" , "**** Red Alert ****"  } ;

   mf = 100.0f * EDIT_scale_misfit( nxyz , fac , sar , far ) ;
        if( mf < 2.5f ) return ;
        if( mf < 3.5f ) im = 0 ;
   else if( mf < 5.0f ) im = 1 ;
   else if( mf < 9.9f ) im = 2 ;
   else                 im = 3 ;
   WARNING_message("%s[%d] scale to shorts misfit = %.2f%% -- %s",
                   name , ib , mf , msg[im] ) ;
   return ;
}
