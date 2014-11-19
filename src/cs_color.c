

/*----------------------------------------------------------------------------*/
/*** The functions herein provide for nonlinear interpolation   ***
 *** between RGB colors.  All colors are expressed as float     ***
 *** triples, with RGB values required to be in the range 0..1. ***
 *** This file is meant to be #include-d into another file!     ***/
/*----------------------------------------------------------------------------*/

#ifndef TYPEDEF_float_triple
#define TYPEDEF_float_triple
typedef struct { float a,b,c ; } float_triple ;
#define float_trip float_triple
#endif

#ifndef INLINE
#ifdef __GNUC__
# define INLINE __inline__
#endif
#endif

/*----------------------------------------------------------------------------*/

#define rgb_up(x) ( ((x) > 0.040450f) ? powf(((x)+0.055f)/1.055f,2.4f)    : (x)/12.92f )
#define rgb_dn(x) ( ((x)> 0.0031308f) ? 1.055f*powf((x),0.416667f)-0.055f : 12.92f*(x) )

static INLINE float_triple COLOR_rgb_to_xyz( float_triple rgb )
{
   float_triple xyz ; float xx,yy,zz ;

   xx    = 100.0f * rgb_up(rgb.a) ;
   yy    = 100.0f * rgb_up(rgb.b) ;
   zz    = 100.0f * rgb_up(rgb.c) ;
   xyz.a = 0.4124f*xx + 0.3576f*yy + 0.1805f*zz ;
   xyz.b = 0.2126f*xx + 0.7152f*yy + 0.0722f*zz ;  /* luminance */
   xyz.c = 0.0193f*xx + 0.1192f*yy + 0.9505f*zz ;
   return xyz ;
}

static INLINE float_triple COLOR_xyz_to_rgb( float_triple xyz )
{
   float_triple rgb ; float xx,yy,zz ;

   xx = (xyz.a *  3.2406f + xyz.b * -1.5372f + xyz.c * -0.4986f) * 0.01f ;
   yy = (xyz.a * -0.9689f + xyz.b *  1.8758f + xyz.c *  0.0415f) * 0.01f ;
   zz = (xyz.a *  0.0557f + xyz.b * -0.2040f + xyz.c *  1.0570f) * 0.01f ;

   rgb.a = rgb_dn(xx) ; rgb.b = rgb_dn(yy) ; rgb.c = rgb_dn(zz) ;
   if( rgb.a > 1.0f ) rgb.a = 1.0f ; else if( rgb.a < 0.0f ) rgb.a = 0.0f ;
   if( rgb.b > 1.0f ) rgb.b = 1.0f ; else if( rgb.b < 0.0f ) rgb.b = 0.0f ;
   if( rgb.c > 1.0f ) rgb.c = 1.0f ; else if( rgb.c < 0.0f ) rgb.c = 0.0f ;
   return rgb ;
}

#undef rgb_up
#undef rgb_dn

/*----------------------------------------------------------------------------*/

#define XFUN(a) cbrtf(a)     /* nonlinear transform of XYZ before interp */
#define XINV(a) (a)*(a)*(a)  /* inverse transform after interp */

float_triple COLOR_rgb_interp( float_triple rgb1, float_triple rgb2, float fac )
{
   float_triple rgb , xyz1,xyz2 ;

        if( fac <= 0.0f ) return rgb2 ;
   else if( fac >= 1.0f ) return rgb1 ;

   xyz1 = COLOR_rgb_to_xyz(rgb1) ;
   xyz2 = COLOR_rgb_to_xyz(rgb2) ;
   xyz1.a = fac*XFUN(xyz1.a) + (1.0f-fac)*XFUN(xyz2.a) ; xyz1.a = XINV(xyz1.a) ;
   xyz1.b = fac*XFUN(xyz1.b) + (1.0f-fac)*XFUN(xyz2.b) ; xyz1.b = XINV(xyz1.b) ;
   xyz1.c = fac*XFUN(xyz1.c) + (1.0f-fac)*XFUN(xyz2.c) ; xyz1.c = XINV(xyz1.c) ;
   rgb = COLOR_xyz_to_rgb(xyz1) ; return rgb ;
}

#undef XFUN
#undef XINV

/*----------------------------------------------------------------------------*/
