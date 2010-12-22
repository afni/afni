#include "mrilib.h"

/*----------------------------------------------------------------------------*/

#undef LP1
#undef LP2
#undef LP3
#undef LP4
#undef LP5
#undef LP6
#undef LP7
#undef LP8
#undef LP9

#define LP1(x) (x)
#define LP2(x) ((x)*(x)-0.3333333f)
#define LP3(x) (((x)*(x)-0.6f)*(x))*1.5f
#define LP4(x) ((x)*(x)*((x)*(x)-0.857143f)+0.0857143f)*2.0f
#define LP5(x) (((x)*(x)*((x)*(x)-1.11111f)+0.238095f)*(x))*3.0f
#define LP6(x) ((x)*(x)*((x)*(x)*((x)*(x)-1.36364f)+0.454545f)-0.021645f)*6.0f
#define LP7(x) (((x)*(x)*((x)*(x)*((x)*(x)-1.61538f)+0.734266f)-0.081585f)*(x))*10.0f

#define LP8(x) ( (x)*(x) * \
               ( (x)*(x) * \
               ( (x)*(x) * \
               ( (x)*(x) - 1.86667f ) + 1.07692f ) - 0.195804f ) + 0.0054390f )*18.0f

#define LP9(x) ( ( (x)*(x) * \
                 ( (x)*(x) * \
                 ( (x)*(x) * \
                 ( (x)*(x) - 2.11765f ) + 1.48235f ) - 0.380090f ) + 0.0259153f ) * (x) )*32.0f

/*----------------------------------------------------------------------------*/

static void poly3D( int px, int py, int pz,
                    int nxyz, float *x, float *y, float *z, float *val )
{
   register int ii ;

   switch( px ){
     default: for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = 1.0f       ; break ;
     case 1:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP1(x[ii]) ; break ;
     case 2:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP2(x[ii]) ; break ;
     case 3:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP3(x[ii]) ; break ;
     case 4:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP4(x[ii]) ; break ;
     case 5:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP5(x[ii]) ; break ;
     case 6:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP6(x[ii]) ; break ;
     case 7:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP7(x[ii]) ; break ;
     case 8:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP8(x[ii]) ; break ;
     case 9:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP9(x[ii]) ; break ;
   }

   switch( py ){
     case 1:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP1(y[ii]) ; break ;
     case 2:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP2(y[ii]) ; break ;
     case 3:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP3(y[ii]) ; break ;
     case 4:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP4(y[ii]) ; break ;
     case 5:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP5(y[ii]) ; break ;
     case 6:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP6(y[ii]) ; break ;
     case 7:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP7(y[ii]) ; break ;
     case 8:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP8(y[ii]) ; break ;
     case 9:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP9(y[ii]) ; break ;
   }

   switch( pz ){
     case 1:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP1(z[ii]) ; break ;
     case 2:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP2(z[ii]) ; break ;
     case 3:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP3(z[ii]) ; break ;
     case 4:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP4(z[ii]) ; break ;
     case 5:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP5(z[ii]) ; break ;
     case 6:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP6(z[ii]) ; break ;
     case 7:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP7(z[ii]) ; break ;
     case 8:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP8(z[ii]) ; break ;
     case 9:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP9(z[ii]) ; break ;
   }

   return ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_polyfit( MRI_IMAGE *imin , int nord , float mrad , byte *mask )
{
   MRI_IMAGE *imout , *fim ;
   float *fin , *fout ;

ENTRY("mri_polyfit") ;

   if( imin == NULL || nord < 0 || nord > 9 ) RETURN(NULL) ;

   /* deal with vector-valued images -- see mrilib.h */

#undef  CALLME
#define CALLME(inn,out) (out) = mri_polyfit( (inn) , nord,mrad,mask )
   if( ISVECTIM(imin) ){ VECTORME(imin,imout) ; RETURN(imout) ; }

   fim = (imin->kind == MRI_float) ? imin : mri_to_float(imin) ;
   fin = MRI_FLOAT_PTR(fim) ;

}
