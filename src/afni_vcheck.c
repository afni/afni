#define VERSION_URL "ftp://141.106.106.221/pub/cox/AFNI.version"

int AFNI_version_check( char * vnum , char * vdate )
{
   char * buf ;
   int    nbuf ;
   char vv[32] , dd[32] , mm[32]="\0" , yy[32]="\0" ;

   if( vnum == NULL || vdate == NULL ) return -1 ;

   nbuf = read_URL( VERSION_URL , &buf ) ;
   if( nbuf <= 0 ) return -1 ;

   nbuf = sscanf( "%31s%31s%31s%31s" , vv,dd,mm,yy )

   if( nbuf <= 0 ) return -1 ;

   vv[6] = '\0' ; strcpy(vnum,vv) ;

   if( nbuf == 1 ) return 1 ;

   sprintf(vdate,"%2s %9s %4s" , dd,mm,yy ) ;
   return 2 ;
}
