#include "mrilib.h"

static char *ppmto_mpeg_filter = NULL ;
#define MPEG_ENCODE_SUFFIX "-realquiet %s"

/*----------------------------------------------------------------------------*/

int suck_file( char *fname , char **fbuf )
{
   int len , fd , ii ;
   char *buf ;

   if( fname == NULL || fname[0] == '\0' || fbuf == NULL ) return 0;

   len = THD_filesize( fname ); if( len <= 0 ) return 0;

   buf = (char *) malloc( sizeof(char) * (len+4) ); if( buf == NULL ) return 0;

   fd = open( fname , O_RDONLY ); if( fd < 0 ) return 0;

   ii = read( fd , buf , len ); close( fd ); if( ii <= 0 ){ free(buf); return 0; }
   *fbuf = buf; return ii;
}

/*----------------------------------------------------------------------------*/

void file_copy( char *fname , int ncopy , char **fcopy )
{
   char *fbuf=NULL ; int nbuf=0 ; int ii ; FILE *fp ;

   if( fname == NULL || ncopy < 1 || fcopy == NULL ) return ;

   nbuf = suck_file( fname , &fbuf ) ;
   if( nbuf <= 0 || fbuf == NULL ) return ;

   for( ii=0 ; ii < ncopy ; ii++ ){
     if( fcopy[ii] == NULL || fcopy[ii][0] == '\0' ) continue ;
     fp = fopen( fcopy[ii] , "w" ) ; if( fp == NULL ) continue ;
     fwrite( fbuf , nbuf , 1 , fp ) ; fclose(fp) ;
   }

   free(fbuf) ; return ;
}

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
   int npure=18 , nfade=6 , iarg=1 , ii,jj,kk , nbad , nx,ny,nin ;
   char *prefix="i2m" , *outname ; char **fcopy ;
   MRI_IMARR *in_imar=NULL ;
   MRI_IMAGE *inim , *qim , *jnim ; MRI_IMAGE *im0=NULL ;
   float fac ;
   int down=1 ; int do_jpg=0 ; int do_loop=0 ;

   if( argc < 3 || strcasecmp(argv[1],"-help") == 0 ){
     printf("Usage: im_to_mov [options] imagefile1 imagefile2 ...\n") ;
     printf("\n") ;
     printf(
       "This program takes as input a sequence of images, and produces as\n"
       "output either another sequence of images, suitable for making into\n"
       "a video, or directly produces the video (MPEG-1 format) from the\n"
       "new sequence of images.\n"
       "\n"
       "For each input image, P copies (cf '-npure') are output, followed\n"
       "by F images interpolated between the current image and the next one\n"
       "(cf '-fade').  Thus, animating the output sequence of images will\n"
       "be suitable for making a video with a fixed dwell on each input\n"
       "image, followed by a smooth segue between images; for example:\n"
       "\n"
       "  im_to_mov -prefix Fred_movie Fred*.jpg\n"
       "\n"
       "The '-down' option lets you shrink the input images before processing,\n"
       "since high-resolution digital photos are usually too big to make\n"
       "useful movies.\n"
     ) ;
     printf("\n") ;
     printf("Options:\n"
            "--------\n"
            " -npure P  = number of pure copies of each image [18] (1 or more)\n"
            " -nfade F  = number of transition fades between pairs [6] (0 or more)\n"
            " -loop     = output a final set of fade images between the\n"
            "             last input image and the first one, so that the\n"
            "             video can be played in an endless loop\n"
            " -down  X  = downsample images by factor X = 2 or 3 or 4 [none]\n"
            " -prefix Q = prefix for output files [i2m]\n"
            " -jpg      = write JPEG output files and stop\n"
            " -mpg      = write PPM files, convert to MPEG-1, delete PPMs\n"
            "             [this is the default mode of operation]\n"
            "Notes:\n"
            "------\n"
            "* The input images must all be the same size!\n"
            "* Valid input image formats are JPEG, PPM, GIF, and PNG.\n"
            "* The output MPEG-1 frame rate is 24 per second,\n"
            "  so the default P and F parameters add up to 1 second\n"
            "  of movie per input image.\n"
           ) ;
     exit(0) ;
   }

   /*---- scan options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-jpg") == 0 ){
       do_jpg = 1 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-mpg") == 0 ){
       do_jpg = 0 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-loop") == 0 ){
       do_loop = 1 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-npure") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need arg after -npure") ;
       npure = (int)strtod(argv[iarg],NULL) ;
       if( npure < 1 ) ERROR_exit("Illegal value after -npure") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-nfade") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need arg after -nfade") ;
       nfade = (int)strtod(argv[iarg],NULL) ;
       if( nfade < 0 ) ERROR_exit("Illegal value after -nfade") ;
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

   /*--- if doing MPEG-1, find the program for that now ---*/

   if( !do_jpg ){
     char *pg = THD_find_executable( "mpeg_encode" ) ;
     if( pg != NULL ){
       ppmto_mpeg_filter = malloc( sizeof(char) * (strlen(pg)+64) ) ;
       sprintf(ppmto_mpeg_filter,"%s %s",pg,MPEG_ENCODE_SUFFIX) ;
     } else {
       ERROR_exit("Can't find mpeg_encode program!") ;
     }
   }

   /*--- read input images ---*/

   if( argc-iarg < 2 ) ERROR_exit("Need at least 2 input images!") ;

   INIT_IMARR(in_imar) ;

   fprintf(stderr,"Reading ") ;

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
     ADDTO_IMARR(in_imar,inim) ; fprintf(stderr,".") ;
   }
   fprintf(stderr,"!\n") ;

   if( nbad > 0 ) ERROR_exit("Can't continue after such blatant stupidities") ;

   /*--- loop over input images, emit various outputs ---*/

   outname = (char *)malloc(sizeof(char)*(strlen(prefix)+32)) ;
   nin     = IMARR_COUNT(in_imar) ;
   fac     = 1.0f / ( nfade + 1.0f ) ;

   if( down < 2 )
     INFO_message("Read in %d images %dx%d",nin,nx,ny) ;
   else
     INFO_message("Read in %d images, downsampled to %dx%d",nin,nx,ny) ;

   if( do_loop ) im0 = mri_copy( IMARR_SUBIM(in_imar,0) ) ;

   if( do_jpg ){  /*------------------ Just write JPEGs ------------------*/

     fprintf(stderr,"++ Writing JPEGs ") ;

     fcopy = (char **)malloc(sizeof(char *)*npure) ;
     for( jj=0 ; jj < npure ; jj++ )
       fcopy[jj] = (char *)malloc(sizeof(char)*strlen(outname)) ;

     for( kk=ii=0 ; ii < nin-1 ; ii++ ){
       inim = IMARR_SUBIM(in_imar,ii) ;
       jnim = IMARR_SUBIM(in_imar,ii+1) ;

#ifdef USE_COPY /* write image once, then simply copy file several times */
       sprintf(outname,"%s_%06d.jpg",prefix,kk) ; kk++ ;
       mri_write_jpg(outname,inim) ;
       if( npure > 1 ){
         for( jj=0 ; jj < npure-1 ; jj++ ){
           sprintf(fcopy[jj],"%s_%06d.jpg",prefix,kk) ; kk++ ;
         }
         file_copy( outname , npure-1 , fcopy ) ;
       }
#else  /* just write image repeatedly */
       for( jj=0 ; jj < npure ; jj++ ){  /* copies of ii-th input */
         sprintf(outname,"%s_%06d.jpg",prefix,kk) ; kk++ ;
         mri_write_jpg(outname,inim) ;
       }
#endif

       for( jj=0 ; jj < nfade ; jj++ ){  /* interpolate for fade */
         qim = mri_rgb_scladd( 1.0f-(jj+1)*fac , inim , (jj+1)*fac , jnim ) ;
         sprintf(outname,"%s_%06d.jpg",prefix,kk) ; kk++ ;
         mri_write_jpg(outname,qim) ; mri_free(qim) ;
       }
       mri_free(inim) ;
       fprintf(stderr,".") ;
     }

     /* final copies of last image */

#ifdef USE_COPY
       sprintf(outname,"%s_%06d.jpg",prefix,kk) ; kk++ ;
       mri_write_jpg(outname,jnim) ;
       if( npure > 1 ){
         for( jj=0 ; jj < npure-1 ; jj++ ){
           sprintf(fcopy[jj],"%s_%06d.jpg",prefix,kk) ; kk++ ;
         }
         file_copy( outname , npure-1 , fcopy ) ;
       }
#else
     for( jj=0 ; jj < npure ; jj++ ){
       sprintf(outname,"%s_%06d.jpg",prefix,kk) ; kk++ ;
       mri_write_jpg(outname,jnim) ;
     }
#endif

     if( do_loop && im0 != NULL ){
       for( jj=0 ; jj < nfade ; jj++ ){  /* interpolate for fade */
         qim = mri_rgb_scladd( 1.0f-(jj+1)*fac , jnim , (jj+1)*fac , im0 ) ;
         sprintf(outname,"%s_%06d.jpg",prefix,kk) ; kk++ ;
         mri_write_jpg(outname,qim) ; mri_free(qim) ;
       }
       mri_free(im0) ;
     }
     mri_free(jnim) ;

     fprintf(stderr,"!\n") ;

     INFO_message("Output %d images with filename format %s_xxxxxx.jpg",kk,prefix) ;

   } else { /*------------- PPM then MPEG ------------------------------------------*/

     fprintf(stderr,"++ Writing PPMs ") ;

     fcopy = (char **)malloc(sizeof(char *)*npure) ;
     for( jj=0 ; jj < npure ; jj++ )
       fcopy[jj] = (char *)malloc(sizeof(char)*strlen(outname)) ;

     for( kk=ii=0 ; ii < nin-1 ; ii++ ){
       inim = IMARR_SUBIM(in_imar,ii) ;
       jnim = IMARR_SUBIM(in_imar,ii+1) ;

#ifdef USE_COPY /* write image once, then simply copy file several times */
       sprintf(outname,"%s_%06d.ppm",prefix,kk) ; kk++ ;
       mri_write_pnm(outname,inim) ;
       if( npure > 1 ){
         for( jj=0 ; jj < npure-1 ; jj++ ){
           sprintf(fcopy[jj],"%s_%06d.ppm",prefix,kk) ; kk++ ;
         }
         file_copy( outname , npure-1 , fcopy ) ;
       }
#else  /* just write image repeatedly */
       for( jj=0 ; jj < npure ; jj++ ){  /* copies of ii-th input */
         sprintf(outname,"%s_%06d.ppm",prefix,kk) ; kk++ ;
         mri_write_pnm(outname,inim) ;
       }
#endif

       for( jj=0 ; jj < nfade ; jj++ ){  /* interpolate for fade */
         qim = mri_rgb_scladd( 1.0f-(jj+1)*fac , inim , (jj+1)*fac , jnim ) ;
         sprintf(outname,"%s_%06d.ppm",prefix,kk) ; kk++ ;
         mri_write_pnm(outname,qim) ; mri_free(qim) ;
       }
       mri_free(inim) ;
       fprintf(stderr,".") ;
     }

     /* final copies of last image */

#ifdef USE_COPY
       sprintf(outname,"%s_%06d.ppm",prefix,kk) ; kk++ ;
       mri_write_pnm(outname,jnim) ;
       if( npure > 1 ){
         for( jj=0 ; jj < npure-1 ; jj++ ){
           sprintf(fcopy[jj],"%s_%06d.ppm",prefix,kk) ; kk++ ;
         }
         file_copy( outname , npure-1 , fcopy ) ;
       }
#else
     for( jj=0 ; jj < npure ; jj++ ){
       sprintf(outname,"%s_%06d.ppm",prefix,kk) ; kk++ ;
       mri_write_pnm(outname,jnim) ;
     }
#endif

     if( do_loop && im0 != NULL ){
       for( jj=0 ; jj < nfade ; jj++ ){  /* interpolate for fade */
         qim = mri_rgb_scladd( 1.0f-(jj+1)*fac , jnim , (jj+1)*fac , im0 ) ;
         sprintf(outname,"%s_%06d.ppm",prefix,kk) ; kk++ ;
         mri_write_pnm(outname,qim) ; mri_free(qim) ;
       }
       mri_free(im0) ;
     }
     mri_free(jnim) ;

     fprintf(stderr,"!\n") ;

     INFO_message("Output %d images with filename format %s_xxxxxx.ppm",kk,prefix) ;

     /* now create MPEG from PPMs [stolen from imseq.c] */

     { int alen ; char *alf , *oof , *par , *frate ;
       char *qscale , *pattrn ; int mpar=0 ;
       FILE *fpar ;

       /* write mpeg_encode parameter file */

       par = malloc( sizeof(char)*(strlen(prefix)+32) ) ;
       sprintf(par,"%s.PARAM",prefix) ;
       fpar = fopen( par , "w" ) ;
       if( fpar == NULL ) ERROR_exit("Can't create MPEG parameter file %s",par) ;
       oof = malloc( sizeof(char)*(strlen(prefix)+32) ) ; /* output fname */
       sprintf(oof,"%s.mpg",prefix) ;
       qscale="5" ;
       frate ="24" ;
       pattrn = calloc(sizeof(char),(npure+nfade+24)) ;
       if( npure+nfade == 1 ){
         strcpy(pattrn,"IPPPPP") ;
       } else {
         pattrn[0] = 'I' ;
         if( npure > 1 ) memset( pattrn+1     , 'P' , npure-1 ) ;
         if( nfade > 0 ) memset( pattrn+npure , 'I' , nfade   ) ;
       }
       fprintf(fpar,
                  "OUTPUT %s\n"             /* oof */
                  "GOP_SIZE          %d\n"  /* npure+nfade */
                  "SLICES_PER_FRAME  1\n"
                  "FRAME_RATE        %s\n"  /* frate */
                  "BASE_FILE_FORMAT  PPM\n"
                  "INPUT_CONVERT     *\n"
                  "INPUT_DIR         .\n"
                  "PATTERN           %s\n"  /* pattrn */
                  "IQSCALE           %s\n"  /* qscale */
                  "PQSCALE           31\n"
                  "BQSCALE           31\n"
                  "PIXEL             HALF\n"
                  "RANGE             10 4\n"
                  "PSEARCH_ALG       LOGARITHMIC\n"
                  "BSEARCH_ALG       SIMPLE\n"
                  "REFERENCE_FRAME   ORIGINAL\n"
                  "INPUT\n"
                  "%s_*.ppm [%06d-%06d]\n"  /* prefix, from, to */
                  "END_INPUT\n"
               , oof , npure+nfade , frate , pattrn , qscale ,
                 prefix,0,kk-1 ) ;
       fclose(fpar) ; free(pattrn) ;

       /* make command to run */

       alen = strlen(par)+strlen(ppmto_mpeg_filter)+128 ;
       alf  = malloc(sizeof(char)*alen) ;
       sprintf(alf , ppmto_mpeg_filter, par ) ; /* command to run */
       INFO_message("Running '%s' to produce %s\n",alf,oof) ;
       system(alf) ;                            /* so run it!    */
       ININFO_message("** DONE -- deleting ppm files") ;
       sprintf(alf,"\\rm -f %s_??????.ppm",prefix) ; system(alf) ;
       remove(par); free(alf); free(oof); free(par); /* free trash */
     }

   } /*---------------------------------------------------------------------------*/

   exit(0) ;
}
