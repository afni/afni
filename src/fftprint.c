#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#undef PI
#define PI (3.141592653589793238462643)

typedef struct complex { float r , i ; } complex ;

static void csfft_print( int mode , int idim , complex * xc ) ;
static void csfft_trigconsts( int idim ) ;

int main( int argc , char * argv[] )
{
   int len , ii ;
   complex * cx ;

   if( argc < 2 ){printf("Usage: fftprint len\n");exit(0);}

   len = strtol( argv[1] , NULL , 10 ) ;
   if( len < 4 ){ fprintf(stderr,"Illegal length\n"); exit(1); }

   ii = 2 ; do{ ii *= 2 ; } while( ii < len ) ;
   if( ii != len ){ fprintf(stderr,"Illegal length\n"); exit(1); }

   cx = (complex *) malloc( sizeof(complex) * len ) ;
   csfft_print(-1,len,cx) ;
   exit(0) ;
}

static complex * csplus = NULL , * csminus = NULL ;  /* trig consts */
static int nold = -666 ;

static void csfft_trigconsts( int idim )  /* internal function */
{
   register unsigned int  m, n, i0, i1, i2, i3, k;
   register complex       *r0, *csp;
   register float         co, si, f0, f1, f2, f3, f4;
   double                 al;

   if( idim == nold ) return ;

   if( idim > nold ){
      if( csplus != NULL ){ free(csplus) ; free(csminus) ; }
      csplus  = (complex *) malloc( sizeof(complex) * idim ) ;
      csminus = (complex *) malloc( sizeof(complex) * idim ) ;
      if( csplus == NULL || csminus == NULL ){
         fprintf(stderr,"\n*** csfft cannot malloc space! ***\n"); exit(1) ;
      }
   }
   nold = n = idim ;

   f1 = 1.0 ;  /* csplus init */
   m  = 1; k  = 0;
   while (n > m) {
      i3 = m << 1;
      f2 = m; al = f1*PI/f2;
      co = cos(al); si = sin(al);
      (csplus + k)->r = 1.;
      (csplus + k)->i = 0.;
      for (i0=0; i0 < m; i0++) {
         k++;
         csp = csplus + k; r0 = csp - 1;
         csp->r = r0->r * co - r0->i * si;
         csp->i = r0->i * co + r0->r * si;
      }
      m = i3;
   }

   f1 = -1.0 ;  /* csminus init */
   m  = 1; k  = 0;
   while (n > m) {
      i3 = m << 1;
      f2 = m; al = f1*PI/f2;
      co = cos(al); si = sin(al);
      (csminus + k)->r = 1.;
      (csminus + k)->i = 0.;
      for (i0=0; i0 < m; i0++) {
         k++;
         csp = csminus + k; r0  = csp - 1;
         csp->r = r0->r * co - r0->i * si;
         csp->i = r0->i * co + r0->r * si;
      }
      m = i3;
   }
   return ;
}

/*****************************************************************************
  The routine that generates a C function for a completely unrolled FFT.
  Basically the same as csfft_cox() with print statements replacing
  the actual computations.
  The resulting code also needs the csfft_trigconsts() function above.
******************************************************************************/

void csfft_print( int mode , int idim , complex * xc )
{
   register unsigned int  m, n, i0, i1, i2, i3, k;
   register complex       *r0, *r1, *csp;
   register float         co, si, f0, f1, f2, f3, f4;
   double                 al;

   csfft_trigconsts( idim ) ;
   csp = csplus ;

   n   = idim;
   i2  = idim >> 1;
   i1  = 0;

   /*-- function header --*/

   printf( "/**************************************/\n"
           "/* FFT routine unrolled of length %3d */\n"
           "\n"
           "void fft%d( int mode , complex * xc )\n"
           "{\n"
           "   register complex * csp , * xcx=xc;\n"
           "   register float f1,f2,f3,f4 ;\n"
           "\n"
           "   /** perhaps initialize **/\n"
           "\n"
           "   if( nold != %d ) csfft_trigconsts( %d ) ;\n"
           "\n"
           "   csp = (mode > 0) ? csplus : csminus ;  /* choose const array */\n"
           "\n"
           "   /** data swapping part **/\n" ,
      idim,idim,idim,idim ) ;

   /*-- swap some data elements --*/

   for (i0=0; i0 < n; i0 ++) {
      if ( i1 > i0 ) {

printf("\n") ;
printf("   f1 = xcx[%d].r ; f2 = xcx[%d].i ;\n",i0,i0) ;
printf("   xcx[%d].r = xcx[%d].r ; xcx[%d].i = xcx[%d].i ;\n" , i0,i1,i0,i1) ;
printf("   xcx[%d].r = f1 ; xcx[%d].i = f2 ;\n" , i1,i1) ;

      }
      m = i2;
      while ( m && !(i1 < m) ) {
         i1 -= m;
         m >>= 1;
     }
     i1 += m;
   }

   /*-- the actual computations --*/

   printf("\n   /** butterflying part **/\n") ;

   m = 1;
   k = 0;
   while (n > m) {
      i3 = m << 1;
      for (i0=0; i0 < m; i0 ++) {
         for (i1=i0; i1 < n; i1 += i3) {

printf("\n") ;
if( csp[k].r == 1.0 ){
  printf("   f1 = xcx[%d].r ; f3 = xcx[%d].i ;  /* cos=1 sin=0 */\n", i1+m,i1+m ) ;

} else {
  printf("   f1 = xcx[%d].r * csp[%d].r - xcx[%d].i * csp[%d].i ; /* twiddles */\n",
         i1+m,k , i1+m,k ) ;
  printf("   f3 = xcx[%d].r * csp[%d].i + xcx[%d].i * csp[%d].r ;\n",
         i1+m,k , i1+m,k ) ;
}

printf("   f2 = xcx[%d].r ; f4 = xcx[%d].i ;\n" , i1,i1 ) ;
printf("   xcx[%d].r = f2-f1 ; xcx[%d].i = f4-f3 ;\n",i1+m,i1+m) ;
printf("   xcx[%d].r = f2+f1 ; xcx[%d].i = f4+f3 ;\n",i1,i1) ;

         }
         k++;
      }
      m = i3;
   }

printf("\n   return ;\n}\n") ;

   return ;
}
