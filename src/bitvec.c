#include "mrilib.h"

/*--------------------------------------------------------------------------*/

int equal_bitvector_piece( MRI_IMAGE * b , MRI_IMAGE * c , int aa , int bb )
{
   int ii ; byte * bv=MRI_BYTE_PTR(b) , * cv=MRI_BYTE_PTR(c) ;

   if( aa <  0     ) aa = 0 ;
   if( bb >= b->nx ) bb = b->nx - 1 ;
   for( ii=aa ; ii <= bb ; ii++ ) if( bv[ii] != cv[ii] ) return 0 ;
   return 1;
}

int equal_bitvector( MRI_IMAGE * b , MRI_IMAGE * c )
{
   return equal_bitvector_piece( b , c , 0 , b->nx - 1 ) ;
}

/*--------------------------------------------------------------------------*/

void randomize_bitvector_piece( MRI_IMAGE * b , int aa , int bb )
{
   int ii ; byte * bv=MRI_BYTE_PTR(b) ;

   if( aa <  0     ) aa = 0 ;
   if( bb >= b->nx ) bb = b->nx - 1 ;
   for( ii=aa ; ii <= bb ; ii++ ) bv[ii] = ( drand48() > 0.5 ) ;
   return ;
}

void randomize_bitvector( MRI_IMAGE * b )
{
   randomize_bitvector_piece( b , 0 , b->nx - 1 ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void zero_bitvector_piece( MRI_IMAGE * b , int aa , int bb )
{
   int ii ; byte * bv=MRI_BYTE_PTR(b) ;

   if( aa <  0     ) aa = 0 ;
   if( bb >= b->nx ) bb = b->nx - 1 ;
   for( ii=aa ; ii <= bb ; ii++ ) bv[ii] = 0 ;
   return ;
}

void zero_bitvector( MRI_IMAGE * b )
{
   zero_bitvector_piece( b , 0 , b->nx - 1 ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void one_bitvector_piece( MRI_IMAGE * b , int aa , int bb )
{
   int ii ; byte * bv=MRI_BYTE_PTR(b) ;

   if( aa <  0     ) aa = 0 ;
   if( bb >= b->nx ) bb = b->nx - 1 ;
   for( ii=aa ; ii <= bb ; ii++ ) bv[ii] = 1 ;
   return ;
}

void one_bitvector( MRI_IMAGE * b )
{
   one_bitvector_piece( b , 0 , b->nx - 1 ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void invert_bitvector_piece( MRI_IMAGE * b , int aa , int bb )
{
   int ii ; byte * bv=MRI_BYTE_PTR(b) ;

   if( aa <  0     ) aa = 0 ;
   if( bb >= b->nx ) bb = b->nx - 1 ;
   for( ii=aa ; ii <= bb ; ii++ ) bv[ii] = 1 - bv[ii] ;
   return ;
}

void invert_bitvector( MRI_IMAGE * b )
{
   invert_bitvector_piece( b , 0 , b->nx - 1 ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

MRI_IMAGE * new_bitvector( int n )
{
   MRI_IMAGE * b ;
   b = mri_new( n , 1 , MRI_byte ) ;
   return b ;
}

MRI_IMAGE * copy_bitvector( MRI_IMAGE * b )
{
   int ii ;
   MRI_IMAGE * c = mri_new( b->nx , 1 , MRI_byte ) ;
   memcpy( MRI_BYTE_PTR(c) , MRI_BYTE_PTR(b) , b->nx ) ;
   return c ;
}

/*--------------------------------------------------------------------------*/

int count_bitvector( MRI_IMAGE * b )
{
   int ii,ss , n=b->nx ;
   byte * bv = MRI_BYTE_PTR(b) ;
   for( ii=ss=0 ; ii < n ; ii++ ) if( bv[ii] ) ss++ ;
   return ss ;
}

/*--------------------------------------------------------------------------*/

void normalize_floatvector( MRI_IMAGE * fim )
{
   int ii , n=fim->nx ;
   float ff,gg , * far=MRI_FLOAT_PTR(fim) ;

   for( ff=0.0,ii=0 ; ii < n ; ii++ ) ff += far[ii] ;
   ff /= n ;
   for( gg=0.0,ii=0 ; ii < n ; ii++ ) gg += SQR( (far[ii]-ff) ) ;
   if( gg <= 0.0 ) return ;
   gg = 1.0 / sqrt(gg) ;
   for( ii=0 ; ii < n ; ii++ ) far[ii] = (far[ii]-ff)*gg ;
   return ;
}

/*--------------------------------------------------------------------------*/

float corr_floatbit( MRI_IMAGE * fim , MRI_IMAGE * bim )
{
   int ii , n=fim->nx , ns ;
   float * far=MRI_FLOAT_PTR(fim) , ss ;
   byte  * bar=MRI_BYTE_PTR(bim) ;

   for( ss=0.0,ns=ii=0 ; ii < n ; ii++ )
      if( bar[ii] ){ ns++ ; ss += far[ii] ; }

   if( ns == 0 || ns == n ) return 0.0 ;

   ss *= sqrt( ((float) n) / (float)(ns*(n-ns)) ) ;
   return ss ;
}

/*--------------------------------------------------------------------------*/

MRI_IMARR * init_bitvector_array( int nbv , MRI_IMAGE * fim )
{
   MRI_IMARR * imar ;
   MRI_IMAGE * bim ;
   int ii , n=fim->nx ;
   byte * bar ; float * far=MRI_FLOAT_PTR(fim) ;

   INIT_IMARR(imar) ;
   normalize_floatvector( fim ) ;

   for( ii=0 ; ii < nbv ; ii++ ){
      bim = new_bitvector( n ) ;
      randomize_bitvector( bim ) ;
      bim->dx = corr_floatbit( fim , bim ) ;
      if( bim->dx < 0.0 ){ bim->dx = -bim->dx; invert_bitvector( bim ); }
      ADDTO_IMARR(imar,bim) ;
   }

   return imar ;
}

/*--------------------------------------------------------------------------*/

#define IRAN(j) (lrand48() % (j))

void evolve_bitvector_array( MRI_IMARR * bvar , MRI_IMAGE * fim )
{
   static int    nrvec=-1 ;
   static float * rvec=NULL ;

   int ii,nn=fim->nx , nbv=IMARR_COUNT(bvar) , aa,bb , qbv ;
   MRI_IMAGE * bim , * qim ;

   /* create mutants */

   for( ii=0 ; ii < nbv ; ii++ ){

      bim = IMARR_SUBIMAGE(bvar,ii) ;

      aa = IRAN(nn) ; bb = aa + IRAN(5) ; if( bb >= nn ) bb = nn-1 ;

      qim = copy_bitvector(bim) ;
      zero_bitvector_piece( qim , aa , bb ) ;
      if( equal_bitvector_piece(bim,qim,aa,bb) ){
         mri_free(qim) ;
      } else {
         qim->dx = corr_floatbit( fim , qim ) ;
         if( qim->dx < 0.0 ){ qim->dx = -qim->dx; invert_bitvector( qim ); }
         ADDTO_IMARR(bvar,qim) ;
      }

      qim = copy_bitvector(bim) ;
      one_bitvector_piece( qim , aa , bb ) ;
      if( equal_bitvector_piece(bim,qim,aa,bb) ){
         mri_free(qim) ;
      } else {
         qim->dx = corr_floatbit( fim , qim ) ;
         if( qim->dx < 0.0 ){ qim->dx = -qim->dx; invert_bitvector( qim ); }
         ADDTO_IMARR(bvar,qim) ;
      }

      qim = copy_bitvector(bim) ;
      randomize_bitvector_piece( qim , aa , bb ) ;
      if( equal_bitvector_piece(bim,qim,aa,bb) ){
         mri_free(qim) ;
      } else {
         qim->dx = corr_floatbit( fim , qim ) ;
         if( qim->dx < 0.0 ){ qim->dx = -qim->dx; invert_bitvector( qim ); }
         ADDTO_IMARR(bvar,qim) ;
      }

      qim = copy_bitvector(bim) ;
      invert_bitvector_piece( qim , aa , bb ) ;
      if( equal_bitvector_piece(bim,qim,aa,bb) ){
         mri_free(qim) ;
      } else {
         qim->dx = corr_floatbit( fim , qim ) ;
         if( qim->dx < 0.0 ){ qim->dx = -qim->dx; invert_bitvector( qim ); }
         ADDTO_IMARR(bvar,qim) ;
      }
   }

   /* sort everybody */

   qbv = IMARR_COUNT(bvar) ;
   if( nrvec < qbv ){
      if( rvec != NULL ) free(rvec) ;
      rvec = (float *) malloc(sizeof(float)*qbv) ;
      nrvec = qbv ;
   }
   for( ii=0 ; ii < qbv ; ii++ ) rvec[ii] = -fabs(IMARR_SUBIM(bvar,ii)->dx) ;

   qsort_floatstuff( qbv , rvec , (void **) bvar->imarr ) ;

   TRUNCATE_IMARR( bvar , nbv ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   MRI_IMAGE * fim , * bim ;
   MRI_IMARR * bvar ;
   int ii , nv , nite=0 , neq=0 ;
   float fold , fnew ;

   if( argc < 2 ){printf("Usage: bitvec fname.1D > bname.1D\n");exit(0);}

   fim = mri_read_1D( argv[1] ) ;
   if( fim == NULL ){fprintf(stderr,"Can't read %s\n",argv[1]);exit(1);}
   fprintf(stderr,"vector length = %d\n",fim->nx) ;

   srand48((long)time(NULL)) ;

   bvar = init_bitvector_array( 256 , fim ) ;
   fold = fabs(IMARR_SUBIM(bvar,0)->dx) ;
   fprintf(stderr,"fold = %7.4f\n",fold) ;

   while(1){
      evolve_bitvector_array( bvar , fim ) ;
      nv = IMARR_COUNT(bvar) ;
      nite++ ;
#if 0
      fprintf(stderr,"nite=%d\n",nite) ;
      for( ii=0 ; ii < nv ; ii++ )
         fprintf(stderr," %7.4f",IMARR_SUBIM(bvar,ii)->dx) ;
      fprintf(stderr,"\n\n") ;
#endif

      fnew = fabs(IMARR_SUBIM(bvar,0)->dx) ;
      if( fnew <= fold ){
         neq++ ; if( neq == 10 ) break ;
      } else {
         neq  = 0 ;
         fold = fnew ;
         fprintf(stderr,"%d: %7.4f\n",nite,fold) ;
      }
   }

   bim = IMARR_SUBIM(bvar,0) ;
   for( ii=0 ; ii < fim->nx ; ii++ )
      printf(" %d\n",(int)MRI_BYTE_PTR(bim)[ii]) ;

   exit(0) ;
}
