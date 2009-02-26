#include "mrilib.h"
#include "zlib.h"

#define CHUNK 262144

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

unsigned int zz_compress_all( unsigned int nsrc , char *src , char **dest )
{
   int ret ;
   zz_compress_dosave( (dest != NULL) ) ;
   ret = zz_compress_some( 0 , NULL )   ; if( ret < 0 ) return 0 ;
   ret = zz_compress_some( nsrc , src ) ; if( ret < 0 ) return 0 ;
   ret = zz_compress_some( -1 , dest )  ;
   return ret ;
}

/*------------------------------------------------------------------------*/

int_pair zz_uncompress_some( int nsrc, char *src,
                             int nused, int ndest, char *dest )
{
   static int busy=0 ; static z_stream strm;
   static char *saved=NULL ; static unsigned int nsaved=0 ;
   int ret ;
   int_pair rp = {-1,-1} ;
   char buf[CHUNK] ;

   if( nsrc <= 0 || src == NULL || nused < 0 || ndest <= 0 || dest == NULL ){
     ERROR_message("zz_uncompress_some: bad inputs!") ;
     busy = 0 ; return rp ;
   }

   /**----- Start case -----**/

   if( nused == 0 ){
     if( busy ){
       ERROR_message("zz_uncompress_some: Start call in busy state!") ;
       if( saved != NULL ){ free(saved); saved=NULL; }
     }

     strm.zalloc   = Z_NULL ;
     strm.zfree    = Z_NULL ;
     strm.opaque   = Z_NULL ;
     strm.avail_in = 0 ;
     strm.next_in  = Z_NULL ;
     ret = inflateInit( &strm ) ;
     if( ret != Z_OK ){
       ERROR_message("zz_uncompress_some: can't initalize inflation!") ;
       return rp ;
     }
     busy = 1 ; strm.avail_out = 0 ; strm.next_out = Z_NULL ; nsaved = 0 ;
   } else if( !busy ){
     ERROR_message("zz_uncompress_some: non-Start call in non-busy state!") ;
     return rp ;
   }

   /**----- See if any data


/*===========================================================================*/

#define NMAX 555

int main( int argc , char *argv[] )
{
   int nfile , ii , jj ;
   char         *buf[NMAX] , *qbuf ;
   unsigned int nbuf[NMAX] , nbtop ;
   double       ncom[NMAX] ,  qcom , ncd ;

   nfile = argc-1 ; if( nfile > NMAX ) nfile = NMAX ;
   zz_compress_dosave(0) ;
   zz_compress_dlev  (9) ;

   for( nbtop=0,ii=1 ; ii <= nfile ; ii++ ){
     buf[ii] = AFNI_suck_file(argv[ii]) ;
     if( buf[ii] == NULL ) ERROR_exit("Can't read file %s",argv[ii]) ;
     nbuf[ii] = strlen(buf[ii]) ;
     ncom[ii] = zz_compress_all( nbuf[ii] , buf[ii] , NULL ) ;
     nbtop    = MAX( nbtop , nbuf[ii] ) ;
   }

   qbuf = (char *)malloc(sizeof(char)*2*nbtop+16) ;

   for( ii=1 ; ii < nfile ; ii++ ){
     for( jj=ii+1 ; jj <= nfile ; jj++ ){
       strcpy( qbuf , buf[ii] ) ;
       strcat( qbuf , buf[jj] ) ;
       qcom = zz_compress_all( nbuf[ii]+nbuf[jj] , qbuf , NULL ) ;
       ncd  = (qcom-MIN(ncom[ii],ncom[jj])) / MAX(ncom[ii],ncom[jj]) ;
       printf("%.5f %s %s\n",ncd,argv[ii],argv[jj]) ;
     }
   }
   exit(0) ;
}
