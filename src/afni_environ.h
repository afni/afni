#ifndef _AFNI_ENVIRON_HEADER_
#define _AFNI_ENVIRON_HEADER_

extern void AFNI_process_environ( char * ) ;
extern char * AFNI_suck_file( char * ) ;
extern char * my_getenv( char * ) ;
extern void AFNI_mark_environ_done(void) ;  /* 16 Apr 2000 */

/*-- 22 May 2000: macros for checking for Yes or No --*/

#define YSTART(cp) \
  ( (cp) != NULL && ( (cp)[0] == 'y' || (cp)[0] == 'Y' ) )

#define NSTART(cp) \
  ( (cp) != NULL && ( (cp)[0] == 'n' || (cp)[0] == 'N' ) )

#endif /* _AFNI_ENVIRON_HEADER_ */
