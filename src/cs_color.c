

/*----------------------------------------------------------------------------*/
/*** The functions herein provide for nonlinear interpolation   ***
 *** between RGB colors.  All colors are expressed as float     ***
 *** triples, with values required to be in the range 0..1.     ***
 *** This file is meant to be #include-d into another file!     ***/
/*----------------------------------------------------------------------------*/

#ifndef TYPEDEF_float_triple
#define TYPEDEF_float_triple
typedef struct { float a,b,c ; } float_triple ;
#define float_trip float_triple
#endif

#ifndef TYPEDEF_byte
#define TYPEDEF_byte
typedef unsigned char byte ;
#endif

#ifndef INLINE
#ifdef __GNUC__
# define INLINE __inline__
#endif
#endif

/*----------------------------------------------------------------------------*/

#define rgb_up(x) ( ((x) > 0.040450f) ? powf(((x)+0.055f)/1.055f,2.4f)    : (x)/12.92f )
#define rgb_dn(x) ( ((x)> 0.0031308f) ? 1.055f*powf((x),0.416667f)-0.055f : 12.92f*(x) )

/*----------------------------------------------------------------------------*/

static byte rinit=0  ;
static byte rup[256] ;
static byte rdn[256] ;

static void COLOR_init_rupdn(void)
{
   unsigned short ii ; float rr;
   if( rinit ) return ;
   for( ii=1 ; ii < 255 ; ii++ ){
     rr = ii / 255.0f ;
     rup[ii] = (byte)(255.49f*rgb_up[rr]) ;
     rdn[ii] = (byte)(255.49f*rgb_dn[rr]) ;
   }
   rup[0] = rdn[0] = 0 ; rup[255] = rdn[255] = 255 ; rinit = 1 ; return ;
}

/*----------------------------------------------------------------------------*/

static INLINE float_triple COLOR_rgb_to_xyz( float_triple rgb )
{
   float_triple xyz ; float xx,yy,zz ;

   xx = rgb_up(rgb.a) ; yy = rgb_up(rgb.b) ; zz = rgb_up(rgb.c) ;
   xyz.a = 0.433877f*xx + 0.376223f*yy + 0.189900f*zz ;
   xyz.b = 0.212600f*xx + 0.715200f*yy + 0.072200f*zz ;  /* luminance */
   xyz.c = 0.017723f*xx + 0.109458f*yy + 0.872819f*zz ;
   if( xyz.a > 1.0f ) xyz.a = 1.0f ; else if( xyz.a < 0.0f ) xyz.a = 0.0f ;
   if( xyz.b > 1.0f ) xyz.b = 1.0f ; else if( xyz.b < 0.0f ) xyz.b = 0.0f ;
   if( xyz.c > 1.0f ) xyz.c = 1.0f ; else if( xyz.c < 0.0f ) xyz.c = 0.0f ;
   return xyz ;
}

static INLINE float_triple COLOR_xyz_to_rgb( float_triple xyz )
{
   float_triple rgb ; float xx,yy,zz ;

   xx =  3.0802100f*xyz.a - 1.537210f*xyz.b - 0.5430060f*xyz.c ;
   yy = -0.9209680f*xyz.a + 1.875760f*xyz.b + 0.0452125f*xyz.c ;
   zz =  0.0529522f*xyz.a - 0.204021f*xyz.b + 1.1510700f*xyz.c ;

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
