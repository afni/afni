#include "mrilib.h"

/*------------------------------------------------------------------*/

#define CORONAL_NUM 38
static float coronal_yy[] = {
  -100, -95, -90, -85, -80, -75, -70, -65,
   -60, -55, -50, -45, -40, -35, -32, -28,
   -24, -20, -16, -12,  -8,  -4,   0,   4,
     8,  12,  16,  20,  24,  28,  32,  35,
    40,  45,  50,  55,  60,  65
} ;
static char * coronal_ff[] = {
 "tt_corm99.ppm.gz" , "tt_corm95.ppm.gz" , "tt_corm90.ppm.gz" , "tt_corm85.ppm.gz" ,
 "tt_corm80.ppm.gz" , "tt_corm75.ppm.gz" , "tt_corm70.ppm.gz" , "tt_corm65.ppm.gz" ,
 "tt_corm60.ppm.gz" , "tt_corm55.ppm.gz" , "tt_corm50.ppm.gz" , "tt_corm45.ppm.gz" ,
 "tt_corm40.ppm.gz" , "tt_corm35.ppm.gz" , "tt_corm32.ppm.gz" , "tt_corm28.ppm.gz" ,
 "tt_corm24.ppm.gz" , "tt_corm20.ppm.gz" , "tt_corm16.ppm.gz" , "tt_corm12.ppm.gz" ,
 "tt_corm08.ppm.gz" , "tt_corm04.ppm.gz" , "tt_corp00.ppm.gz" , "tt_corp04.ppm.gz" ,
 "tt_corp08.ppm.gz" , "tt_corp12.ppm.gz" , "tt_corp16.ppm.gz" , "tt_corp20.ppm.gz" ,
 "tt_corp24.ppm.gz" , "tt_corp28.ppm.gz" , "tt_corp32.ppm.gz" , "tt_corp35.ppm.gz" ,
 "tt_corp40.ppm.gz" , "tt_corp45.ppm.gz" , "tt_corp50.ppm.gz" , "tt_corp55.ppm.gz" ,
 "tt_corp60.ppm.gz" , "tt_corp65.ppm.gz"
} ;

/*------------------------------------------------------------------------------*/

#define SAGITTAL_NUM 18
static float sagittal_xx[] = {
    0,  3,  5,  9, 13, 17, 21, 25, 29, 33 ,
   37, 41, 43, 47, 51, 55, 59, 61
} ;
static char * sagittal_ff[] = {
 "tt_sag00g.ppm.gz", "tt_sag03.ppm.gz" , "tt_sag05.ppm.gz" , "tt_sag09.ppm.gz" ,
 "tt_sag13.ppm.gz" , "tt_sag17.ppm.gz" , "tt_sag21.ppm.gz" , "tt_sag25.ppm.gz" ,
 "tt_sag29.ppm.gz" , "tt_sag33.ppm.gz" , "tt_sag37.ppm.gz" , "tt_sag41.ppm.gz" ,
 "tt_sag43.ppm.gz" , "tt_sag47.ppm.gz" , "tt_sag51.ppm.gz" , "tt_sag55.ppm.gz" ,
 "tt_sag59.ppm.gz" , "tt_sag61.ppm.gz"
} ;

/*------------------------------------------------------------------------------*/

#define AXIAL_NUM 27
static float axial_zz[] = {
   -40 , -36 , -32 , -28 , -24 , -20 , -16 , -12 , -8 , -4 ,
    -1 ,   1 ,   4 ,   8 ,  12 ,  16 ,  20 ,  24 , 28 , 32 , 35 ,
    40 ,  45 ,  50 ,  55 ,  60 ,  65
} ;
static char * axial_ff[] = {
 "tt_horm40.ppm.gz" , "tt_horm36.ppm.gz" , "tt_horm32.ppm.gz" , "tt_horm28.ppm.gz" ,
 "tt_horm24.ppm.gz" , "tt_horm20.ppm.gz" , "tt_horm16.ppm.gz" , "tt_horm12.ppm.gz" ,
 "tt_horm08.ppm.gz" , "tt_horm04.ppm.gz" , "tt_horm01.ppm.gz" , "tt_horp01.ppm.gz" ,
 "tt_horp04.ppm.gz" , "tt_horp08.ppm.gz" , "tt_horp12.ppm.gz" , "tt_horp16.ppm.gz" ,
 "tt_horp20.ppm.gz" , "tt_horp24.ppm.gz" , "tt_horp28.ppm.gz" , "tt_horp32.ppm.gz" ,
 "tt_horp35.ppm.gz" , "tt_horp40.ppm.gz" , "tt_horp45.ppm.gz" , "tt_horp50.ppm.gz" ,
 "tt_horp55.ppm.gz" , "tt_horp60.ppm.gz" , "tt_horp65.ppm.gz"
} ;

#define TTAHOME "http://128.231.212.175/TTA/"

/*------------------------------------------------------------------------------*/
/*! (xx,yy,zz) in RAI (Dicom) coords; code is (0=axial,1=sag,2=cor).            */
/*------------------------------------------------------------------------------*/

MRI_IMAGE * THD_ttget( int code , float xx , float yy , float zz )
{
   int ii,jj,kk , nn , ch,nch, nx,ny,maxval , id ;
   char nbuf[256] , *ttahome , buf[32] ;
   byte *ndata=NULL , *bar ;
   MRI_IMAGE *im ;

   ttahome = getenv("AFNI_TTAHOME") ;
   if( ttahome == NULL ) ttahome = TTAHOME ;

   switch( code ){
     case 1:                       /* sagittal */
       xx = fabs(xx) ;
       if( xx <= sagittal_xx[0] ){
         ii = 0 ;
       } else if( xx >= sagittal_xx[SAGITTAL_NUM-1] ){
         ii = SAGITTAL_NUM - 1 ;
       } else {
         for( ii=1 ; ii < SAGITTAL_NUM && xx > sagittal_xx[ii] ; ii++ ) ; /* nada */
         if( fabs(xx-sagittal_xx[ii-1]) < fabs(xx-sagittal_xx[ii]) ) ii-- ;
       }
       sprintf(nbuf,"%s%s", ttahome , sagittal_ff[ii] ) ;
     break ;

     case 2:                       /* coronal */
       if( yy <= coronal_yy[0] ){
          jj = 0 ;
       } else if( yy >= coronal_yy[CORONAL_NUM-1] ){
          jj = CORONAL_NUM - 1 ;
       } else {
          for( jj=1 ; jj < CORONAL_NUM && yy > coronal_yy[jj] ; jj++ ) ; /* nada */
          if( fabs(yy-coronal_yy[jj-1]) < fabs(yy-coronal_yy[jj]) ) jj-- ;
       }
       sprintf(nbuf,"%s%s", ttahome , coronal_ff[jj] ) ;
     break ;

     default:
     case 0:                       /* axial */
       if( zz <= axial_zz[0] ){
          kk = 0 ;
       } else if( zz >= axial_zz[AXIAL_NUM-1] ){
          kk = AXIAL_NUM - 1 ;
       } else {
          for( kk=1 ; kk < AXIAL_NUM && zz > axial_zz[kk] ; kk++ ) ; /* nada */
          if( fabs(zz-axial_zz[kk-1]) < fabs(zz-axial_zz[kk]) ) kk-- ;
       }
       sprintf(nbuf,"%s%s", ttahome , axial_ff[kk] ) ;
     break ;
   }

   /* get data */

   nn = NI_read_URL( nbuf , (char **)&ndata ) ;

   if( nn < 40960 || ndata == NULL || ndata[0] != 'P' || ndata[1] != '6' ){
      if( ndata != NULL ) free(ndata) ;
      return NULL ;
   }

   /* convert to image */

   id = 2 ; ch = ndata[id] ;  /* start scan after "P6" */

#define SKIPCOM                                                                      \
    {if(ch == '#') do{ch=ndata[++id];}while(id<nn-1 && ch != '\n');}

#define NUMSCAN(var)                                                                 \
   { SKIPCOM ;                                                                       \
     while( id<nn-1 && !isdigit(ch) ){ch = ndata[++id]; SKIPCOM; }                   \
     for( nch=0 ; id<nn-1 && isdigit(ch) ; nch++,ch=ndata[++id] ) {buf[nch] = ch ;}  \
     buf[nch]='\0';                                                                  \
     var = strtol( buf , NULL , 10 ) ; }

    NUMSCAN(nx) ; if( nx <= 2 || id >= nn-1 ){ free(ndata); return NULL; }
    NUMSCAN(ny) ; if( ny <= 2 || id >= nn-1 ){ free(ndata); return NULL; }

    NUMSCAN(maxval);
    if( maxval <= 7 || maxval > 255 || id >= nn-1 ){ free(ndata); return NULL; }

    id++ ;
    if( nn-id < 3*nx*ny ){ free(ndata); return NULL; }

    im = mri_new( nx , ny , MRI_rgb ) ;
    bar = MRI_RGB_PTR(im) ;
    memcpy( bar , ndata+id , 3*nx*ny ) ;
    free(ndata) ;

    if( maxval < 255 ){
      float fac = 255.4/maxval ;
      for( ii=0 ; ii < 3*nx*ny ; ii++ ) bar[ii] = (byte)( bar[ii]*fac ) ;
    }

    return im ;
}

int main( int argc , char *argv[] )
{
   MRI_IMAGE *im ;

   im = THD_ttget( 2 , 12.0,12.0,12.0 ) ;
   if( im == NULL ){ fprintf(stderr,"** failure!\n"); exit(0); }
   mri_write_pnm( "q2.ppm" , im ) ;
   fprintf(stderr,"wrote image\n") ;

   im = THD_ttget( 0 , 12.0,12.0,12.0 ) ;
   if( im == NULL ){ fprintf(stderr,"** failure!\n"); exit(0); }
   mri_write_pnm( "q0.ppm" , im ) ;
   fprintf(stderr,"wrote image\n") ;

   im = THD_ttget( 1 , 12.0,12.0,12.0 ) ;
   if( im == NULL ){ fprintf(stderr,"** failure!\n"); exit(0); }
   mri_write_pnm( "q1.ppm" , im ) ;
   fprintf(stderr,"wrote image\n") ;
   exit(0) ;
}
