#include "mrilib.h"

/*----------------------------------------------------------------------------*/

MRI_IMAGE *mri_rgb_scladd( float fa, MRI_IMAGE *ima, float fb, MRI_IMAGE *imb )
{
   MRI_IMAGE *imc ;
   byte *apt , *bpt , *cpt ;
   int npix , ii ; float val ;

   apt = MRI_RGB_PTR(ima) ;
   bpt = MRI_RGB_PTR(imb) ;
   imc = mri_new_conforming( ima , MRI_rgb ) ;
   cpt = MRI_RGB_PTR(imc) ;
   npix = ima->nvox ;
   for( ii=0 ; ii < npix ; ii++ ){
     val = fa * apt[3*ii+0] + fb * bpt[3*ii+0] ; cpt[3*ii+0] = BYTEIZE(val) ;
     val = fa * apt[3*ii+1] + fb * bpt[3*ii+1] ; cpt[3*ii+1] = BYTEIZE(val) ;
     val = fa * apt[3*ii+2] + fb * bpt[3*ii+2] ; cpt[3*ii+2] = BYTEIZE(val) ;
   }
   return imc ;
}

/*----------------------------------------------------------------------------*/

typedef unsigned short ushort ;

#define IN_R(i,j)  (ushort)inar [3*((i)+(j)*nxin) ]
#define OUT_R(i,j)         outar[3*((i)+(j)*nxout)]

#define IN_G(i,j)  (ushort)inar [3*((i)+(j)*nxin) +1]
#define OUT_G(i,j)         outar[3*((i)+(j)*nxout)+1]

#define IN_B(i,j)  (ushort)inar [3*((i)+(j)*nxin) +2]
#define OUT_B(i,j)         outar[3*((i)+(j)*nxout)+2]

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_rgb_scaledown_by2( MRI_IMAGE *inim )
{
   MRI_IMAGE *outim ;
   byte *inar , *outar ;
   int nxin,nyin , nxout,nyout , ii,jj , pp,qq , p1,q1 ;

   if( inim == NULL || inim->kind != MRI_rgb ) return NULL ;
   nxin  = inim->nx ; nyin  = inim->ny ;
   nxout = nxin/2   ; nyout = nyin/2 ;
   outim = mri_new(nxout,nyout,MRI_rgb) ;
   inar  = MRI_RGB_PTR(inim) ;
   outar = MRI_RGB_PTR(outim) ;

   for( jj=0 ; jj < nyout ; jj++ ){
     qq = 2*jj ; q1 = qq+1 ;
     for( ii=0 ; ii < nxout ; ii++ ){
       pp = 2*ii ; p1 = pp+1 ;
       OUT_R(ii,jj) = ( IN_R(pp,qq) + IN_R(p1,qq) + IN_R(pp,q1) + IN_R(p1,q1) ) / 4 ;
       OUT_G(ii,jj) = ( IN_G(pp,qq) + IN_G(p1,qq) + IN_G(pp,q1) + IN_G(p1,q1) ) / 4 ;
       OUT_B(ii,jj) = ( IN_B(pp,qq) + IN_B(p1,qq) + IN_B(pp,q1) + IN_B(p1,q1) ) / 4 ;
   }}

   return outim ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_rgb_scaledown_by3( MRI_IMAGE *inim )
{
   MRI_IMAGE *outim ;
   byte *inar , *outar ;
   int nxin,nyin , nxout,nyout , ii,jj , pp,qq , p1,q1 , p2,q2 ;

   if( inim == NULL || inim->kind != MRI_rgb ) return NULL ;
   nxin  = inim->nx ; nyin  = inim->ny ;
   nxout = nxin/3   ; nyout = nyin/3 ;
   outim = mri_new(nxout,nyout,MRI_rgb) ;
   inar  = MRI_RGB_PTR(inim) ;
   outar = MRI_RGB_PTR(outim) ;

   for( jj=0 ; jj < nyout ; jj++ ){
     qq = 3*jj ; q1 = qq+1 ; q2 = q1+1 ;
     for( ii=0 ; ii < nxout ; ii++ ){
       pp = 3*ii ; p1 = pp+1 ; p2 = p1+1 ;
       OUT_R(ii,jj) = (   IN_R(pp,qq) + 2*IN_R(p1,qq) +   IN_R(p2,qq)
                       +2*IN_R(pp,q1) + 4*IN_R(p1,q1) + 2*IN_R(p2,q1)
                       +  IN_R(pp,q2) + 2*IN_R(p1,q2) +   IN_R(p2,q2) ) / 16 ;
       OUT_G(ii,jj) = (   IN_G(pp,qq) + 2*IN_G(p1,qq) +   IN_G(p2,qq)
                       +2*IN_G(pp,q1) + 4*IN_G(p1,q1) + 2*IN_G(p2,q1)
                       +  IN_G(pp,q2) + 2*IN_G(p1,q2) +   IN_G(p2,q2) ) / 16 ;
       OUT_B(ii,jj) = (   IN_B(pp,qq) + 2*IN_B(p1,qq) +   IN_B(p2,qq)
                       +2*IN_B(pp,q1) + 4*IN_B(p1,q1) + 2*IN_B(p2,q1)
                       +  IN_B(pp,q2) + 2*IN_B(p1,q2) +   IN_B(p2,q2) ) / 16 ;
   }}

   return outim ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_rgb_scaledown_by4( MRI_IMAGE *inim )
{
   MRI_IMAGE *outim ;
   byte *inar , *outar ;
   int nxin,nyin , nxout,nyout , ii,jj , pp,qq , p1,q1 , p2,q2 , p3,q3;

   if( inim == NULL || inim->kind != MRI_rgb ) return NULL ;
   nxin  = inim->nx ; nyin  = inim->ny ;
   nxout = nxin/4   ; nyout = nyin/4 ;
   outim = mri_new(nxout,nyout,MRI_rgb) ;
   inar  = MRI_RGB_PTR(inim) ;
   outar = MRI_RGB_PTR(outim) ;

   for( jj=0 ; jj < nyout ; jj++ ){
     qq = 4*jj ; q1 = qq+1 ; q2 = q1+1 ; q3 = q2+1 ;
     for( ii=0 ; ii < nxout ; ii++ ){
       pp = 4*ii ; p1 = pp+1 ; p2 = p1+1 ; p3 = p2+1 ;
       OUT_R(ii,jj) = (   IN_R(pp,qq) + 2*IN_R(p1,qq) + 2*IN_R(p2,qq) +   IN_R(p3,qq)
                       +2*IN_R(pp,q1) + 4*IN_R(p1,q1) + 4*IN_R(p2,q1) + 2*IN_R(p3,q1)
                       +2*IN_R(pp,q2) + 4*IN_R(p1,q2) + 4*IN_R(p2,q2) + 2*IN_R(p3,q2)
                       +  IN_R(pp,q3) + 2*IN_R(p1,q3) + 2*IN_R(p2,q3) +   IN_R(p3,q3) ) / 36 ;
       OUT_G(ii,jj) = (   IN_G(pp,qq) + 2*IN_G(p1,qq) + 2*IN_G(p2,qq) +   IN_G(p3,qq)
                       +2*IN_G(pp,q1) + 4*IN_G(p1,q1) + 4*IN_G(p2,q1) + 2*IN_G(p3,q1)
                       +2*IN_G(pp,q2) + 4*IN_G(p1,q2) + 4*IN_G(p2,q2) + 2*IN_G(p3,q2)
                       +  IN_G(pp,q3) + 2*IN_G(p1,q3) + 2*IN_G(p2,q3) +   IN_G(p3,q3) ) / 36 ;
       OUT_B(ii,jj) = (   IN_B(pp,qq) + 2*IN_B(p1,qq) + 2*IN_B(p2,qq) +   IN_B(p3,qq)
                       +2*IN_B(pp,q1) + 4*IN_B(p1,q1) + 4*IN_B(p2,q1) + 2*IN_B(p3,q1)
                       +2*IN_B(pp,q2) + 4*IN_B(p1,q2) + 4*IN_B(p2,q2) + 2*IN_B(p3,q2)
                       +  IN_B(pp,q3) + 2*IN_B(p1,q3) + 2*IN_B(p2,q3) +   IN_B(p3,q3) ) / 36 ;
   }}

   return outim ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int npure=10 , nfade=5 , iarg=1 , ii,jj,kk , nbad , nx,ny,nin ;
   char *prefix="i2m" , *outname ;
   MRI_IMARR *in_imar=NULL ;
   MRI_IMAGE *inim , *qim , *jnim ;
   float fac ;
   int down=1 ;

   if( argc < 3 ){
     printf("Usage: im_to_mov [options] imagefile1 imagefile2 ...\n") ;
     printf("\n") ;
     printf("Options:\n"
            "--------\n"
            " -npure P  = number of pure copies of each image [10]\n"
            " -nfade F  = number of transition fades between pairs [5]\n"
            " -down  X  = downsample by factor X = 2 or 3 or 4 [none]\n"
            " -prefix Q = prefix for output files [i2m]\n"
           ) ;
     exit(0) ;
   }

   /*---- scan options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-npure") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need arg after -npure") ;
       npure = (int)strtod(argv[iarg],NULL) ;
       if( npure < 1 ) ERROR_exit("Illegal value after -npure") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-nfade") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need arg after -nfade") ;
       nfade = (int)strtod(argv[iarg],NULL) ;
       if( nfade < 1 ) ERROR_exit("Illegal value after -nfade") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-down") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need arg after -down") ;
       down = (int)strtod(argv[iarg],NULL) ;
       if( down < 1 || down > 4 ) ERROR_exit("Illegal value after -down") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need arg after -prefix") ;
       prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("Bad name after -prefix") ;
       iarg++ ; continue ;
     }

     ERROR_exit("Illegal option '%s'",argv[iarg]) ;
   }

   /*--- read input images ---*/

   if( argc-iarg < 2 ) ERROR_exit("Need at least 2 input images!") ;

   INIT_IMARR(in_imar) ;

   for( nbad=0,ii=iarg ; ii < argc ; ii++ ){
     inim = mri_read(argv[ii]) ;
     if( inim == NULL ){
       ERROR_message("Can't read image '%s'",argv[ii]) ; nbad++ ; continue ;
     }
     if( inim->kind != MRI_rgb ){
       INFO_message("Converting image %s to RGB",argv[ii]) ;
       qim = mri_to_rgb(inim) ; mri_free(inim) ; inim = qim ;
     }
     switch( down ){
       case 2: qim = mri_rgb_scaledown_by2(inim); mri_free(inim); inim = qim; break;
       case 3: qim = mri_rgb_scaledown_by3(inim); mri_free(inim); inim = qim; break;
       case 4: qim = mri_rgb_scaledown_by4(inim); mri_free(inim); inim = qim; break;
     }
     if( IMARR_COUNT(in_imar) == 0 ){
       nx = inim->nx ; ny = inim->ny ;
     } else if( inim->nx != nx || inim->ny != ny ){
       ERROR_message("Image '%s' has (nx,ny)=(%d,%d) mismatch from (%d,%d)",
                     argv[ii] , inim->nx,inim->ny , nx,ny ) ; nbad++ ; continue ;
     }
     ADDTO_IMARR(in_imar,inim) ;
   }

   if( nbad > 0 ) ERROR_exit("Can't continue after such blatant stupidities") ;

   /*--- loop over input images, emit various outputs ---*/

   outname = (char *)malloc(sizeof(char)*(strlen(prefix)+32)) ;
   nin     = IMARR_COUNT(in_imar) ;
   INFO_message("Read in %d images %dx%d",nin,nx,ny) ;

   fac = 1.0f / ( nfade + 1.0f ) ;

   fprintf(stderr,"++ Writing") ;
   for( kk=ii=0 ; ii < nin-1 ; ii++ ){
     inim = IMARR_SUBIM(in_imar,ii) ;
     jnim = IMARR_SUBIM(in_imar,ii+1) ;

     for( jj=0 ; jj < npure ; jj++ ){  /* copies of ii-th input */
       sprintf(outname,"%s_%06d.jpg",prefix,kk) ; kk++ ;
       mri_write_jpg(outname,inim) ;
     }

     for( jj=0 ; jj < nfade ; jj++ ){  /* interpolate for fade */
       qim = mri_rgb_scladd( 1.0f-(jj+1)*fac , inim , (jj+1)*fac , jnim ) ;
       sprintf(outname,"%s_%06d.jpg",prefix,kk) ; kk++ ;
       mri_write_jpg(outname,qim) ;
     }
     mri_free(inim) ;
     fprintf(stderr,".") ;
   }

   /* final copies of last image */

   for( jj=0 ; jj < npure ; jj++ ){
     sprintf(outname,"%s_%06d.jpg",prefix,kk) ; kk++ ;
     mri_write_jpg(outname,jnim) ;
   }
   mri_free(jnim) ;
   fprintf(stderr,".\n") ;

   INFO_message("Output %d images with filename format %s_xxxxxx.jpg",kk,prefix) ;
   exit(0) ;
}
