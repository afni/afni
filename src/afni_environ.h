#ifndef _AFNI_ENVIRON_HEADER_
#define _AFNI_ENVIRON_HEADER_

extern void AFNI_process_environ( char * ) ;
extern char * AFNI_suck_file( char * ) ;
extern char * my_getenv( char * ) ;
extern void AFNI_mark_environ_done(void) ;  /* 16 Apr 2000 */

#endif /* _AFNI_ENVIRON_HEADER_ */
