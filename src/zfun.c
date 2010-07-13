#include "mrilib.h"

/*===========================================================================*/
#ifndef HAVE_ZLIB

void zz_compress_dosave( int ii ){return;}
void zz_compress_dlev( int ii ){return;}
int zz_compress_some( int nsrc, void *ptr ){return -1;}
int zz_compress_all( int nsrc , char *src , char **dest ){return -1;}
int zz_uncompress_some( int nsrc, char *src, int ndest, char *dest ){return -1;}
int zz_uncompress_all( int nsrc , byte *src , char **dest ){return -1;}
MRI_IMAGE * zz_ncd_many( int nar , int *nsrc , char **src ){return NULL;}
float zz_ncd_pair( int n1 , char *s1 , int n2 , char *s2 ){return -1.0f;}
float THD_ncdfloat( int n , float *x , float *y ){return -1.0f;}
float THD_ncdfloat_scl( int n , float xbot,float xtop,float *x ,
                                float ybot,float ytop,float *y  ){return -1.0f;}

char * array_to_zzb64( int nsrc , char *src ){ return NULL; }
int    zzb64_to_array( char *zb , char **dest ){ reutrn -1; }

/*===========================================================================*/
#else  /* HAVE_ZLIB */

#include "zlib.h"

#define CHUNK 262144  /* 256 K */

/*------------------------------------------------------------------------*/

static int dosave = 1 ;
void zz_compress_dosave( int ii ){ dosave = ii ; }

static int dlev = 6 ;
void zz_compress_dlev( int ii ){ dlev = (ii < 1 || ii > 9) ? 6 : ii ; }

/*------------------------------------------------------------------------*/
/*! Start, continue, or end compressing a byte stream.
     - Startup:  nsrc = 0, ptr == NULL
     - Continue: nsrc > 0, ptr == (char *)source (mustn't be NULL)
     - Finish:   nsrc < 0, ptr == (char **)dest  (might be NULL)
     - Return value is total number of bytes in output thus far.
       Will be 0 for Startup call, will be >= 0 for other calls.
     - If a negative number is returned, that's bad news.
     - On return from the Finish call, the compressed byte stream
       will be available in *dest, if dest!=NULL, and dosave!=0;
       the array should be free()-ed by the caller when done with it.
*//*----------------------------------------------------------------------*/

int zz_compress_some( int nsrc, void *ptr )
{
   static int busy=0 ; static z_stream strm;
   static char *saved=NULL ; static unsigned int nsaved=0 ;
   int ret ; unsigned int have; char buf[CHUNK] ;

   /**----- Startup case -----**/

   if( nsrc == 0 || (nsrc > 0 && ptr == NULL) ){
     int lev = (nsrc < 1 || nsrc > 9) ? dlev : nsrc ;

     if( busy )
       ERROR_message("zz_compress_some: busy during Startup!") ;
     if( saved != NULL ){ free(saved); saved=NULL; }
     nsaved = 0 ;

     strm.zalloc = Z_NULL ;
     strm.zfree  = Z_NULL ;
     strm.opaque = Z_NULL ;
     ret = deflateInit( &strm , lev ) ;
     if( ret != Z_OK ){
       ERROR_message("zz_compress_none: Startup fails!") ;
       busy = 0 ; return -1 ;
     }
     busy = 1 ; return 0 ;
   }

   /**----- Continue case -----**/

   if( nsrc > 0 && ptr != NULL ){
     if( !busy ){
       ERROR_message("zz_compress_some: Continue from non-busy state!") ;
       if( saved != NULL ){ free(saved); saved=NULL; nsaved=0; }
       busy = 0 ; return -1 ;
     }

     strm.avail_in = nsrc ;
     strm.next_in  = ptr ;

     /** compress input data until it is all consumed **/

/* INFO_message("zz_compress_some: nsrc=%d bytes",nsrc) ; */
     do{
       strm.avail_out = CHUNK ;
       strm.next_out  = buf ;
       ret = deflate( &strm, Z_NO_FLUSH ) ;
       if( ret == Z_STREAM_ERROR ){
         ERROR_message("zz_compress_some: deflate failure!") ;
         if( saved != NULL ){ free(saved); saved=NULL; nsaved=0; }
         busy = 0 ; deflateEnd(&strm) ; return -1 ;
       }
       have = CHUNK - strm.avail_out;
       if( dosave && have > 0 ){
         saved = (char *)realloc( saved , sizeof(char)*(nsaved+have+1) ) ;
         memcpy( saved+nsaved , buf , have ) ;
         nsaved += have ;
       }
/* INFO_message("                  have=%u bytes",have) ; */
     } while( strm.avail_out == 0 ) ;

     if( strm.avail_in > 0 ){
       strm.avail_out = CHUNK ;
       strm.next_out  = buf ;
       ret = deflate( &strm, Z_SYNC_FLUSH ) ;
       if( ret == Z_STREAM_ERROR ){
         ERROR_message("zz_compress_some: deflate failure!") ;
         if( saved != NULL ){ free(saved); saved=NULL; nsaved=0; }
         busy = 0 ; deflateEnd(&strm) ; return -1 ;
       }
       have = CHUNK - strm.avail_out;
       if( dosave && have > 0 ){
         saved = (char *)realloc( saved , sizeof(char)*(nsaved+have+1) ) ;
         memcpy( saved+nsaved , buf , have ) ;
         nsaved += have ;
       }
/* INFO_message("             last have=%u bytes",have) ; */
     }

     return (int)strm.total_out ;
   }

    /**----- Finish case -----**/

   if( nsrc < 0 ){
     if( !busy ){
       ERROR_message("zz_compress_some: not busy during Finish!") ;
       if( saved != NULL ){ free(saved); saved=NULL; nsaved=0; }
       busy = 0 ; return -1 ;
     }

     do{
       strm.avail_in  = 0 ;
       strm.avail_out = CHUNK ;
       strm.next_out  = buf ;
       ret = deflate( &strm , Z_FINISH ) ;
       if( ret == Z_STREAM_ERROR ){
         ERROR_message("zz_compress_some: error during Finish!") ;
         if( saved != NULL ){ free(saved); saved=NULL; nsaved=0; }
         deflateEnd(&strm) ; busy = 0 ; return -1 ;
       }
       have = CHUNK - strm.avail_out ;
       if( dosave && have > 0 ){
         saved = (char *)realloc( saved , sizeof(char)*(nsaved+have+1) ) ;
         memcpy( saved+nsaved , buf , have ) ;
         nsaved += have ;
       }
/* INFO_message("           finish have=%u bytes",have) ; */
     } while( ret != Z_STREAM_END ) ;

     (void)deflateEnd(&strm) ;

     if( ptr != NULL && dosave ){
       char **cpt = (char **)ptr ; *cpt = saved ; ret = (int)nsaved ;
     } else {
       free(saved) ; ret = (int)strm.total_out ;
     }

     saved = NULL ; nsaved = 0 ; busy = 0 ; return ret ;
   }

   /* should never be reached */

   ERROR_message("zz_compress_some: runoff error!") ; return -1 ;
}

/*------------------------------------------------------------------------*/
/*! Compress all of src array into the *dest array (if not NULL).
    Return value is size of dest array, as malloc()-ed.
*//*----------------------------------------------------------------------*/

int zz_compress_all( int nsrc , char *src , char **dest )
{
   int ret ;
   zz_compress_dosave( (dest != NULL) ) ;
   ret = zz_compress_some( 0 , NULL )   ; if( ret < 0 ) return 0 ;
   ret = zz_compress_some( nsrc , src ) ; if( ret < 0 ) return 0 ;
   ret = zz_compress_some( -1 , dest )  ;
   return ret ;
}

/*------------------------------------------------------------------------*/
/*! - Startup:  nsrc >  0, src != NULL
    - Continue: nsrc == 0, src == don't care
    - Finish:   nsrc <  0, src == don't care
    - Return value = actual number of bytes stored into dest
    - Finish calls should repeat until this value is 0, to make sure
      that all the data has been extracted.  If you wish to stop the
      extraction before getting all the data, call with nsrc=-666.
    - A negative return value indicates an error.
*//*----------------------------------------------------------------------*/

int zz_uncompress_some( int nsrc, char *src, int ndest, char *dest )
{
   static int busy=0 ; static z_stream strm;
   int ret ;

   if( ndest <= 0 || dest == NULL ){
     ERROR_message("zz_uncompress_some: bad dest inputs!") ;
     if( busy ) inflateEnd(&strm) ;
     busy = 0 ; return -1 ;
   }
   if( nsrc > 0 && src == NULL ){
     ERROR_message("zz_uncompress_some: bad src inputs!") ;
     if( busy ) inflateEnd(&strm) ;
     busy = 0 ; return -1 ;
   }

   /**----- Start case -----**/

   if( nsrc > 0 ){
     if( busy )
       ERROR_message("zz_uncompress_some: Start call in busy state!") ;
     busy = 0 ;

     strm.zalloc   = Z_NULL ;
     strm.zfree    = Z_NULL ;
     strm.opaque   = Z_NULL ;
     strm.avail_in = nsrc ;
     strm.next_in  = src ;
     ret = inflateInit( &strm ) ;
     if( ret != Z_OK ){
       ERROR_message("zz_uncompress_some: can't initalize inflation!") ;
       return -1 ;
     }
     busy = 1 ;
   }

   if( !busy ){
     ERROR_message("zz_uncompress_some: non-Start call in non-busy state!") ;
     return -1 ;
   }

   /**----- Quit right now -----**/

   if( nsrc == -666 ){
     inflateEnd(&strm) ; busy = 0 ; return 0 ;
   }

   /**----- Continuation -----**/

   strm.avail_out = ndest ;
   strm.next_out  = dest ;
   ret = inflate( &strm , (nsrc >= 0) ? Z_SYNC_FLUSH : Z_FINISH ) ;

   if( ret != Z_OK ){
     ERROR_message("zz_uncompress_some: inflation fails!") ;
     inflateEnd(&strm) ; busy = 0 ; return -1 ;
   }

   ret = ndest - strm.avail_out ;  /* number of bytes output */

   if( ret == 0 ){ inflateEnd(&strm); busy = 0; } /* finished */

   return ret ;
}

/*------------------------------------------------------------------------*/
/*! Uncompress all nsrc bytes in src at once, malloc()-ing *dest;
    the number of bytes in *dest is the return value of this function.
*//*----------------------------------------------------------------------*/

int zz_uncompress_all( int nsrc , byte *src , char **dest )
{
   char buf[CHUNK] , *ddd=NULL ; int nbuf , nddd ;

   if( nsrc <= 0 || src == NULL || dest == NULL ) return -1 ;

   nbuf = zz_uncompress_some( nsrc , src , CHUNK , buf ) ;
   if( nbuf <= 0 ) return -1 ;

   nddd = nbuf ;
   ddd  = (char *)malloc(sizeof(char)*nddd) ;
   memcpy( ddd , buf , nbuf ) ;

   while(1){
     nbuf = zz_uncompress_some( 0 , NULL , CHUNK , buf ) ;
     if( nbuf <= 0 ) break ;
     ddd = (char *)realloc(ddd,sizeof(char)*(nddd+nbuf)) ;
     memcpy(ddd+nddd,buf,nbuf) ;
     nddd += nbuf ;
   }

   if( nbuf == 0 ){
     while(1){
       nbuf = zz_uncompress_some( -1 , NULL , CHUNK , buf ) ;
       if( nbuf <= 0 ) break ;
       ddd = (char *)realloc(ddd,sizeof(char)*(nddd+nbuf)) ;
       memcpy(ddd+nddd,buf,nbuf) ;
       nddd += nbuf ;
     }
   }

   *dest = ddd ; return nddd ;
}

/*------------------------------------------------------------------------*/

MRI_IMAGE * zz_ncd_many( int nar , int *nsrc , char **src )
{
   int ii , jj , nstop ;
   MRI_IMAGE *fim ;
   float     *far , *ncom , qcom,rcom , ncd ;
   char *qbuf ;

   if( nar < 2 || nsrc == NULL || src == NULL ) return NULL ;

   for( ii=0 ; ii < nar ; ii++ )
     if( nsrc[ii] <= 0 || src[ii] == NULL ) return NULL ;

   qbuf = getenv("ZLIB_NCD_FACTOR") ; ii = 6 ;
   if( qbuf != NULL ){
     ii = (int)strtol(qbuf,NULL,10) ;
     if( ii < 1 || ii > 9 ) ii = 6 ;
   }
   zz_compress_dlev(ii) ;

   ncom = (float *)malloc(sizeof(float)*nar) ;
   for( nstop=ii=0 ; ii < nar ; ii++ ){
     nstop    = MAX( nstop , nsrc[ii] ) ;
     ncom[ii] = zz_compress_all( nsrc[ii] , src[ii] , NULL ) ;
     if( ncom[ii] <= 0 ){ free(ncom); return NULL; }
   }

   qbuf = (char *)malloc(sizeof(char)*(2*nstop+16)) ;
   fim  = mri_new( nar , nar , MRI_float ) ;
   far  = MRI_FLOAT_PTR(fim) ;
   for( ii=0 ; ii < nar ; ii++ ){
     for( jj=ii+1 ; jj < nar ; jj++ ){
       memcpy( qbuf          , src[ii] , nsrc[ii] ) ;
       memcpy( qbuf+nsrc[ii] , src[jj] , nsrc[jj] ) ;
       qcom = zz_compress_all( nsrc[ii]+nsrc[jj] , qbuf , NULL ) ;

       memcpy( qbuf          , src[jj] , nsrc[jj] ) ;
       memcpy( qbuf+nsrc[jj] , src[ii] , nsrc[ii] ) ;
       rcom = zz_compress_all( nsrc[ii]+nsrc[jj] , qbuf , NULL ) ;

       ncd = (MIN(rcom,qcom)-MIN(ncom[ii],ncom[jj])) / MAX(ncom[ii],ncom[jj]) ;
       if( ncd > 1.0f ) ncd = 1.0f ;
       far[ii+jj*nar] = far[jj+ii*nar] = ncd ;
     }
     far[ii+ii*nar] = 1.0f ;
   }

   free(qbuf) ; free(ncom) ; return fim ;
}

/*------------------------------------------------------------------------*/

float zz_ncd_pair( int n1 , char *s1 , int n2 , char *s2 )
{
   int nsrc[2] ; char *src[2] ;
   MRI_IMAGE *fim ; float ncd , *far ;

   nsrc[0] = n1 ; src[0] = s1 ;
   nsrc[1] = n2 ; src[1] = s2 ;
   fim = zz_ncd_many( 2 , nsrc , src ) ;
   if( fim == NULL ) return -1.0f ;
   far = MRI_FLOAT_PTR(fim) ;
   ncd = far[1] ;
   mri_free(fim) ; return ncd ;
}

/*------------------------------------------------------------------------*/
/*! Compute NCD between two float vectors,
    which will be scaled to bytes in build_byteized_vectors().
*//*----------------------------------------------------------------------*/

float THD_ncdfloat_scl( int n , float xbot,float xtop,float *x ,
                                float ybot,float ytop,float *y  )
{
   MRI_IMAGE *bim ; byte *xbar,*ybar,*qbuf ; int nbar,ii ;
   float ncd , nx,ny,nxy,nxy2 ;

ENTRY("THD_ncdfloat_scl") ;

   /* scale data vectors to bytes (cf. thd_correlate.c) */

   bim = build_byteized_vectors( n , xbot,xtop,x , ybot,ytop,y ) ;
   if( bim == NULL ) RETURN(1.0f) ;

   nbar = bim->nx ;
   xbar = MRI_BYTE_PTR(bim) ;
   ybar = xbar + nbar ;

   qbuf = (byte *)getenv("ZLIB_NCD_FACTOR") ; ii = 6 ;
   if( qbuf != NULL ){
     ii = (int)strtol((char *)qbuf,NULL,10) ;
     if( ii < 1 || ii > 9 ) ii = 6 ;
   }
   zz_compress_dlev(ii) ;

   nx = (float)zz_compress_all( nbar , xbar , NULL ) ;
   ny = (float)zz_compress_all( nbar , ybar , NULL ) ;

   qbuf = (byte *)malloc(2*nbar) ;
   for( ii=0 ; ii < nbar ; ii++ ){  /* interleave */
     qbuf[2*ii] = xbar[ii] ; qbuf[2*ii+1] = ybar[ii] ;
   }
   nxy  = (float)zz_compress_all( 2*nbar , qbuf , NULL ) ;

   memcpy(qbuf     ,xbar,nbar) ;    /* x then y */
   memcpy(qbuf+nbar,ybar,nbar) ;
   nxy2 = (float)zz_compress_all( 2*nbar , qbuf , NULL ) ;
   nxy  = MIN(nxy,nxy2) ;

   memcpy(qbuf     ,ybar,nbar) ;    /* y then x */
   memcpy(qbuf+nbar,xbar,nbar) ;
   nxy2 = (float)zz_compress_all( 2*nbar , qbuf , NULL ) ;
   nxy  = MIN(nxy,nxy2) ;

   ncd = (nxy-MIN(nx,ny)) / MAX(nx,ny) ;
   if( ncd < 0.0f || ncd > 1.0f ) ncd = 1.0f ;

   free(qbuf) ; mri_free(bim) ; RETURN(ncd) ;
}

/*------------------------------------------------------------------------*/
/*! Compute NCD between two float vectors, autoscaled to bytes. */

float THD_ncdfloat( int n , float *x , float *y )
{
   return THD_ncdfloat_scl( n, 1.0f,-1.0f, x, 1.0f,-1.0f, y ) ;
}

/*------------------------------------------------------------------------*/
/*! Compress and Base64 string-ify an input array. */

char * array_to_zzb64( int nsrc , char *src )
{
   int nzb    ; char *zb    ;
   int nzdest ; char *zdest ;

   if( nsrc <= 0 || src == NULL ) return NULL ;

   zz_compress_dosave(1) ; zz_compress_dlev(9) ;

   nzdest = zz_compress_all( nsrc , src , &zdest ) ;
   if( nzdest <= 0 ) return NULL ;

   B64_set_crlf(0) ;
   B64_to_base64( nzdest , (byte *)zdest , &nzb , (byte **)(&zb) ) ;
   free(zdest) ;
   if( nzb <= 0 ) return NULL ;
   return zb ;
}

/*------------------------------------------------------------------------*/
/*! Inverse to array_to_zzb64(). */

int zzb64_to_array( char *zb , char **dest )
{
   int nzdest=0 ; byte *zdest=NULL ;
   int nout     ; char *cout       ;

   if( zb == NULL ) return 0 ;

   B64_to_binary( strlen(zb) , (byte *)zb , &nzdest , &zdest ) ;

   if( nzdest <= 0 ) return 0 ;

   nout = zz_uncompress_all( nzdest , zdest , dest ) ;
   free(zdest) ;
   return nout ;
}

#endif /* HAVE_ZLIB */
/*===========================================================================*/
