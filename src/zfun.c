#include "mrilib.h"
#include "zlib.h"

/*===========================================================================*/

unsigned int zz_compressor( unsigned int nsrc, char *src, char *dest )
{
    z_stream stream;
    int err;
    Bytef *ddest ; uLongf ndest ; unsigned int nout=0 ;

    if( nsrc == 0 || src == NULL ) return nout;
    ndest = (uLongf)(16.0f+1.001f*nsrc) ;
    if( dest == NULL )
      ddest = (Bytef *)malloc(sizeof(Bytef)*ndest) ;
    else
      ddest = dest ;

    stream.next_in   = (Bytef*)src;
    stream.avail_in  = (uInt)nsrc;
    stream.next_out  = ddest;
    stream.avail_out = (uInt)ndest;
    stream.zalloc    = (alloc_func)0;
    stream.zfree     = (free_func)0;
    stream.opaque    = (voidpf)0;

    err = deflateInit(&stream, 9);
    if (err != Z_OK){ if(dest==NULL)free(ddest); return nout; }

    err = deflate(&stream, Z_FINISH);
    if (err != Z_STREAM_END) {
        deflateEnd(&stream);
        if(dest==NULL)free(ddest);
        return nout;
    }
    nout = stream.total_out ;
    err = deflateEnd(&stream);
    if(dest==NULL)free(ddest);
    return nout ;
}

/*===========================================================================*/

#define NMAX 555

int main( int argc , char *argv[] )
{
   int nfile , ii , jj ;
   char         *buf[NMAX] , *qbuf ;
   unsigned int nbuf[NMAX] , nbtop ;
   double       ncom[NMAX] ,  qcom , ncd ;

   nfile = argc-1 ; if( nfile > NMAX ) nfile = NMAX ;

   for( nbtop=0,ii=1 ; ii <= nfile ; ii++ ){
     buf[ii] = AFNI_suck_file(argv[ii]) ;
     if( buf[ii] == NULL ) ERROR_exit("Can't read file %s",argv[ii]) ;
     nbuf[ii] = strlen(buf[ii]) ;
     ncom[ii] = zz_compressor( nbuf[ii] , buf[ii] , NULL ) ;
     nbtop    = MAX( nbtop , nbuf[ii] ) ;
   }

   qbuf = (char *)malloc(sizeof(char)*2*nbtop+16) ;

   for( ii=1 ; ii < nfile ; ii++ ){
     for( jj=ii+1 ; jj <= nfile ; jj++ ){
       strcpy( qbuf , buf[ii] ) ;
       strcat( qbuf , buf[jj] ) ;
       qcom = zz_compressor( nbuf[ii]+nbuf[jj] , qbuf , NULL ) ;
       ncd  = (qcom-MIN(ncom[ii],ncom[jj])) / MAX(ncom[ii],ncom[jj]) ;
       printf("%.5f %s %s\n",ncd,argv[ii],argv[jj]) ;
     }
   }
   exit(0) ;
}
