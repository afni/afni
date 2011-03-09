/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _AFNI_ENVIRON_HEADER_
#define _AFNI_ENVIRON_HEADER_

#ifdef  __cplusplus
extern "C" {
#endif

/*-- macro to set environment variable if not already defined --*/
/*-- moved here from afni.c on 09 Mar 2011 for general usage  --*/
/*-- originally created there on 31 Jan 2008                  --*/

#undef  PUTENV
#define PUTENV(nm,val) do{ if( getenv((nm)) == NULL ){           \
                             char *str = (char *)malloc(256) ;   \
                             strcpy(str,(nm)); strcat(str,"=");  \
                             strcat(str,val);  putenv(str);      \
                             if( PRINT_TRACING ) STATUS(str) ;   \
                           }} while(0)

extern int AFNI_process_environ( char * ) ;
extern char * AFNI_suck_file( char * ) ;
extern int AFNI_suck_file_len(void) ;       /* 27 Feb 2009 */
extern char * my_getenv( char * ) ;
extern void AFNI_mark_environ_done(void) ;  /* 16 Apr 2000 */
extern void AFNI_mark_environ_undone(void); /* 26 Nov 2008 */
extern int  AFNI_check_environ_done(void);  /* 17 Feb 2010 */
extern int AFNI_yesenv( char * ) ;          /* 21 Jun 2000 */
extern int AFNI_noenv ( char * ) ;          /* 21 Jun 2000 */
extern double AFNI_numenv( char * ) ;       /* 23 Aug 2003 */

extern double AFNI_numenv_def( char *, double ); /* 18 Sep 2007 */

extern int AFNI_setenv( char *cmd ) ;       /* 22 Jan 2003 */

extern int AFNI_prefilter_args( int *argc , char **argv ) ; /* 11 Dec 2007 */

/*-- 22 May 2000: macros for checking for Yes or No --*/

#define YSTART(cp) \
  ( (cp) != NULL && ( (cp)[0] == 'y' || (cp)[0] == 'Y' ) )

#define NSTART(cp) \
  ( (cp) != NULL && ( (cp)[0] == 'n' || (cp)[0] == 'N' ) )

/** 21 Jun 2000: look for Yes or No **/

#define YESSISH(ss) ( (ss)!=NULL && ( (ss)[0]=='Y' || (ss)[0]=='y' ) )
#define NOISH(ss)   ( (ss)!=NULL && ( (ss)[0]=='N' || (ss)[0]=='n' ) )

#ifdef  __cplusplus
}
#endif

#endif /* _AFNI_ENVIRON_HEADER_ */
