#include <stdio.h>
#include <stdlib.h>
#include <math.h>

static const double PI=3.14159265358979323;

/*
 * 1-d implemented directly from the formulas.
 * Very accurate, very slow.
 *
 * Modified to compute DCT scaled by sqrt(2)*4
 */

static void dct8_ref(int *data)
{
  double output[8] = {0};
  short x,n;
  for(x=0;x<8;x++) {
    for(n=0;n<8;n++)
      output[x] += data[n] * cos(PI * x * (2*n+1)/16.0);
  }
  for(x=0;x<8;x++) {
    output[x] /= 4.0;  /* Apply typical weighting to output */
    if(x==0) output[x] /= sqrt(2.0);

    output[x] *= 4.0 * sqrt(2);    /* Scale by sqrt(2)*4 */

    data[x] = floor(output[x] + 0.5); /* Round accurately */
  }
}

/*
 * 1-d implemented directly from the formulas.
 * Very accurate, very slow.
 *
 * Modified to compute IDCT scaled by sqrt(2)*4
 */

static void idct8_ref(int *data)
{
  double output[8] = {0};
  short x,n;
  for(x=0;x<8;x++) {
    output[x]= data[0] / sqrt(2.0);
    for(n=1;n<8;n++)
      output[x] += data[n] * cos(PI * n * (2*x+1)/16.0);
  }
  for(x=0;x<8;x++) {
    output[x] /= 2.0;
    output[x] *= sqrt(2.0);
    data[x] = floor(output[x] + 0.5); /* Round accurately */
  }
}

/*--------------------------------------------------------------------------
 * From Figure 1 of Loeffler, Ligtenberg, and Moschytz.
 * ("Practical Fast 1-D DCT Algorithms with 11 Multiplications,"
 * Acoustics, Speech, and Signal Processing, 1989. ICASSP-89, 1989.
 * pp 988-991.)
 *
 * Note: Output is regular DCT scaled by sqrt(2)*4.
 *
 * The choice of 10-bit fixed-point constants here is arbitrary.
 * Using more bits gives better accuracy, but with increased risk
 * of overflow.  Note that with 10 bit accuracy, sqrt(2) is 1448/1024,
 * which is an exact multiple of 8.  Hence, 181/128 is just as accurate,
 * and reduces overflow.
 *
 * output[x] = sqrt(2)*((x==0)?sqrt(2):1) SUM(n=0..n-1) input[n] * cos(pi*x*(2n+1)/16)
 *----------------------------------------------------------------------------*/

static void dct8(int *dctBlock)
{
  static const int c1=1004 ; /* cos(pi/16)<<10 */
  static const int s1=200  ; /* sin(pi/16)<<10 */
  static const int c3=851  ; /* cos(3pi/16)<<10 */
  static const int s3=569  ; /* sin(3pi/16)<<10 */
  static const int r2c6=554; /* sqrt(2)*cos(6pi/16)<<10 */
  static const int r2s6=1337;
  static const int r2=181;   /* sqrt(2)<<7 */
  int x0=dctBlock[0], x1=dctBlock[1], x2=dctBlock[2], x3=dctBlock[3],
    x4=dctBlock[4], x5=dctBlock[5], x6=dctBlock[6], x7=dctBlock[7];
  int x8;

  /* Stage 1 */
  x8=x7+x0; x0-=x7;  x7=x1+x6; x1-=x6;
  x6=x2+x5; x2-=x5;  x5=x3+x4; x3-=x4;

  /* Stage 2 */
  x4=x8+x5; x8-=x5;  x5=x7+x6; x7-=x6;
  x6=c1*(x1+x2); x2=(-s1-c1)*x2+x6; x1=(s1-c1)*x1+x6;
  x6=c3*(x0+x3); x3=(-s3-c3)*x3+x6; x0=(s3-c3)*x0+x6;

  /* Stage 3 */
  x6=x4+x5; x4-=x5;
  x5=r2c6*(x7+x8); x7=(-r2s6-r2c6)*x7+x5; x8=(r2s6-r2c6)*x8+x5;
  x5=x0+x2;x0-=x2; x2=x3+x1; x3-=x1;

  /* Stage 4, round, and output */
  dctBlock[0]=x6;  dctBlock[4]=x4;
  dctBlock[2]=(x8+512)>>10; dctBlock[6] = (x7+512)>>10;
  dctBlock[7]=(x2-x5+512)>>10; dctBlock[1]=(x2+x5+512)>>10;
  dctBlock[3]=(x3*r2+65536)>>17; dctBlock[5]=(x0*r2+65536)>>17;
}

/***************************************************************************/

/*
 * From Figure 1 of Loeffler, Ligtenberg, and Moschytz.
 * ("Practical Fast 1-D DCT Algorithms with 11 Multiplications,"
 * Acoustics, Speech, and Signal Processing, 1989. ICASSP-89, 1989.
 * pp 988-991.)
 *
 * Note: Output is regular IDCT scaled by sqrt(2)*4.
 *
 * output[x] = sqrt(2)*((x==0)?sqrt(2):1) SUM(n=0..n-1) input[n] * cos(pi*x*(2n+1)/16)
 */
static void idct8(int *dctBlock)
{
  static const int c1=251 ; /* cos(pi/16)<<8 */
  static const int s1=50  ; /* sin(pi/16)<<8 */
  static const int c3=213 ; /* cos(3pi/16)<<8 */
  static const int s3=142 ; /* sin(3pi/16)<<8 */
  static const int r2c6=277; /* cos(6pi/16)*sqrt(2)<<9 */
  static const int r2s6=669;
  static const int r2=181; /* sqrt(2)<<7 */

  /* Stage 4 */
  int x0=dctBlock[0]<<9, x1=dctBlock[1]<<7, x2=dctBlock[2],
    x3=dctBlock[3]*r2, x4=dctBlock[4]<<9, x5=dctBlock[5]*r2,
    x6=dctBlock[6], x7=dctBlock[7]<<7;
  int x8=x7+x1; x1 -= x7;

  /* Stage 3 */
  x7=x0+x4; x0-=x4; x4=x1+x5; x1-=x5; x5=x3+x8; x8-=x3;
  x3=r2c6*(x2+x6);x6=x3+(-r2c6-r2s6)*x6;x2=x3+(-r2c6+r2s6)*x2;

  /* Stage 2 */
  x3=x7+x2; x7-=x2; x2=x0+x6; x0-= x6;
  x6=c3*(x4+x5);x5=(x6+(-c3-s3)*x5)>>6;x4=(x6+(-c3+s3)*x4)>>6;
  x6=c1*(x1+x8);x1=(x6+(-c1-s1)*x1)>>6;x8=(x6+(-c1+s1)*x8)>>6;

  /* Stage 1, rounding and output */
  x7+=512; x2+=512;x0+=512;x3+=512;
  dctBlock[0]=(x3+x4)>>10;  dctBlock[1]=(x2+x8)>>10;
  dctBlock[2]=(x0+x1)>>10;  dctBlock[3]=(x7+x5)>>10;
  dctBlock[4]=(x7-x5)>>10;  dctBlock[5]=(x0-x1)>>10;
  dctBlock[6]=(x2-x8)>>10;  dctBlock[7]=(x3-x4)>>10;
}

/*---------------------------------------------------------*/

#define C1    0.980785  /* cos(PI/16)           */
#define S1    0.19509   /* sin(PI/16)           */
#define C3    0.83147   /* cos(3*PI/16)         */
#define S3    0.55557   /* sin(3*PI/16)         */
#define R2C6  0.541196  /* sqrt(2)*cos(6*PI/16) */
#define R2S6  1.30656   /* sqrt(2)*sin(6*PI/16) */
#define R2    1.41421   /* sqrt(2) (duh)        */

static void fdct8(float *vv)
{
  float x0=vv[0], x1=vv[1], x2=vv[2], x3=vv[3],
        x4=vv[4], x5=vv[5], x6=vv[6], x7=vv[7], x8 ;

  x8=x7+x0; x0-=x7;  x7=x1+x6; x1-=x6;
  x6=x2+x5; x2-=x5;  x5=x3+x4; x3-=x4;

  x4=x8+x5; x8-=x5;  x5=x7+x6; x7-=x6;
  x6=C1*(x1+x2); x2=(-S1-C1)*x2+x6; x1=(S1-C1)*x1+x6;
  x6=C3*(x0+x3); x3=(-S3-C3)*x3+x6; x0=(S3-C3)*x0+x6;

  x6=x4+x5; x4-=x5;
  x5=R2C6*(x7+x8); x7=(-R2S6-R2C6)*x7+x5; x8=(R2S6-R2C6)*x8+x5;
  x5=x0+x2;x0-=x2; x2=x3+x1; x3-=x1;

  vv[0]=x6     ; vv[4]=x4     ;
  vv[2]=x8     ; vv[6]=x7     ;
  vv[7]=(x2-x5); vv[1]=(x2+x5);
  vv[3]=x3*R2  ; vv[5]=x0*R2  ;
}
static void ifdct8(float *vv)
{
  float x0=vv[0]   , x1=vv[1], x2=vv[2]   ,
        x3=vv[3]*R2, x4=vv[4], x5=vv[5]*R2,
        x6=vv[6]   , x7=vv[7], x8=x7+x1    ;

  x1 -= x7;

  x7=x0+x4; x0-=x4; x4=x1+x5; x1-=x5; x5=x3+x8; x8-=x3;
  x3=R2C6*(x2+x6);x6=x3+(-R2C6-R2S6)*x6;x2=x3+(-R2C6+R2S6)*x2;

  x3=x7+x2; x7-=x2; x2=x0+x6; x0-= x6;
  x6=C3*(x4+x5);x5=(x6+(-C3-S3)*x5);x4=(x6+(-C3+S3)*x4);
  x6=C1*(x1+x8);x1=(x6+(-C1-S1)*x1);x8=(x6+(-C1+S1)*x8);

  vv[0]=(x3+x4)/8.0;  vv[1]=(x2+x8)/8.0;
  vv[2]=(x0+x1)/8.0;  vv[3]=(x7+x5)/8.0;
  vv[4]=(x7-x5)/8.0;  vv[5]=(x0-x1)/8.0;
  vv[6]=(x2-x8)/8.0;  vv[7]=(x3-x4)/8.0;
}

int main( int argc , char * argv[] )
{
   int xx[8] = { 1,2,3,4,4,4,3,2 } ;
   int yy[8] ; float zz[8] ; int ii ;

   for( ii=0 ;ii < 8 ; ii++ ) zz[ii] = xx[ii] ;

   memcpy(yy,xx,sizeof(int)*8) ;
   printf("y = %d %d %d %d %d %d %d %d\n",
          yy[0],yy[1],yy[2],yy[3],yy[4],yy[5],yy[6],yy[7]) ;
   dct8_ref(yy) ;
   printf("dct8_ref = %d %d %d %d %d %d %d %d\n",
          yy[0],yy[1],yy[2],yy[3],yy[4],yy[5],yy[6],yy[7]) ;
   idct8_ref(yy) ;
   printf("idct8_ref = %d %d %d %d %d %d %d %d\n",
          yy[0],yy[1],yy[2],yy[3],yy[4],yy[5],yy[6],yy[7]) ;

   memcpy(yy,xx,sizeof(int)*8) ;
   dct8(yy) ;
   printf("dct8 = %d %d %d %d %d %d %d %d\n",
          yy[0],yy[1],yy[2],yy[3],yy[4],yy[5],yy[6],yy[7]) ;
   idct8(yy) ;
   printf("idct8 = %d %d %d %d %d %d %d %d\n",
          yy[0],yy[1],yy[2],yy[3],yy[4],yy[5],yy[6],yy[7]) ;

   fdct8(zz) ;
   printf("fdct8 = %g %g %g %g %g %g %g %g\n",
          zz[0],zz[1],zz[2],zz[3],zz[4],zz[5],zz[6],zz[7]) ;
   ifdct8(zz) ;
   printf("ifdct8 = %g %g %g %g %g %g %g %g\n",
          zz[0],zz[1],zz[2],zz[3],zz[4],zz[5],zz[6],zz[7]) ;

   exit(0) ;
}
