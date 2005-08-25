#include "afni.h"

#define VERSION_URL  "http://afni.nimh.nih.gov/afni/AFNI.version"
#define VERSION_FILE "/Volumes/afni/var/www/html/pub/dist/AFNI.version"

/*------------------------------------------------------------------------*/
/*! Function to check AFNI version.
--------------------------------------------------------------------------*/

void THD_check_AFNI_version(void)
{
   int nbuf ;
   char *vbuf=NULL , vv[128]="none" ;
   pid_t ppp ;

   ppp = fork() ;
   if( ppp < 0 ) return ; /* fork failed */

   /* parent: wait for child to exit (happens almost instantly) */

   if( ppp > 0 ){ waitpid(ppp,NULL,0); return; }

   /* child: fork again immediately, then this child exits;
      this is to prevent zombie processes from hanging around */

   ppp = fork() ; if( ppp != 0 ) _exit(0) ;

   /* grandchild process continues to do the actual work */

#undef  VSIZE
#define VSIZE  1066
#undef  VDELAY
#define VDELAY 429999  /* 429999 s = 5 days */

   { char *home=getenv("HOME") , mname[VSIZE]="file:" ;
     NI_stream ns ;
     if( home != NULL ) strcat(mname,home) ;
     strcat(mname,"/.afni.vctime") ;
     ns = NI_stream_open( mname , "r" ) ;
     if( ns != NULL ){
       NI_element *nel = NI_read_element(ns,22) ;
       NI_stream_close(ns) ;
       if( nel != NULL ){
         char *rhs = NI_get_attribute(nel,"version_check_time") ;
         if( rhs != NULL ){
           int last_time = strtol(rhs,NULL,10) ;
           int dtime     = ((int)time(NULL)) - last_time ;
           if( dtime >= 0 && dtime < VDELAY ) _exit(0) ;
         }
         NI_free_element(nel) ;
       }
     }
   }

   /*-- get information from the master computer --*/

   nbuf = read_URL( VERSION_URL , &vbuf ) ;  /* see thd_http.c */

   if( nbuf <= 0 || vbuf == NULL || vbuf[0] == '\0' ) _exit(0) ;

   sscanf( vbuf , "%127s" , vv ) ;

   if( strcmp(vv,VERSION) != 0 )
    fprintf(stderr,"\n"
                   "++ VERSION CHECK!  This program = %s\n"
                   "++         Current AFNI website = %s\n" ,
            VERSION , vv ) ;

   _exit(0) ;
}
