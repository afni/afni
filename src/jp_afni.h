/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#ifndef _AFNI_EPI_HEADER_
#define _AFNI_EPI_HEADER_

/*-- Symbols to define what code to include for specific systems;  --*/
/*-- exactly one of these must be #define-d, or things won't work. --*/

#undef  AFNI_BRUKER_BRI
#define AFNI_SIGNA_KECK

/*************************  AFNI variables, etc.  ******************************/

#include "iochan.h"                     /* I/O to other processes              */

#define AFNI_CONTROL_PORT  7954         /* always send control data to AFNI    */
#define AFNI_TCP_PORT      7953         /* maybe send image data to AFNI       */

#define AFNI_OPEN_CONTROL_MODE   1      /* 1st time thru: open control channel */
#define AFNI_WAIT_CONTROL_MODE   2      /* waiting for AFNI to open control    */
#define AFNI_OPEN_DATA_MODE      3      /* now can open data channel to AFNI   */
#define AFNI_CATCHUP_MODE        4      /* waiting for AFNI to open data       */
#define AFNI_CONTINUE_MODE       5      /* at last! data channel is ready!     */

/*-- global control variables --*/

#ifdef MAIN
  int      AFNI_mode        = 0 ;    /* if > 0, then means AFNI is active  */
  int      AFNI_use_tcp     = 0 ;    /* if > 0, use TCP/IP to send images */
  char     AFNI_host[128]   = "\0" ; /* hostname of CPU AFNI is on       */
  char     AFNI_iochan[128] = "\0" ; /* I/O channel name to AFNI        */
  IOCHAN * AFNI_ioc         = NULL ; /* ptr to I/O channel itself      */
  int      AFNI_atexit_setup= 0    ; /* is AFNI_exit setup yet?       */
  char     AFNI_buf[1024]          ; /* temporary space              */
  int      AFNI_verbose     = 0    ; /* debugging mode              */

#if defined(AFNI_BRUKER_BRI)
  char     AFNI_infocom[256]= "3T_toafni" ; /* command for AFNI info */
#elif defined(AFNI_SIGNA_KECK)
  char     AFNI_infocom[256]= "\0" ;        /* no command for AFNI */
#endif

#else
  extern int      AFNI_mode ;
  extern int      AFNI_use_tcp ;
  extern char     AFNI_host[] ;
  extern char     AFNI_iochan[] ;
  extern IOCHAN * AFNI_ioc ;
  extern char     AFNI_buf[] ;
  extern int      AFNI_atexit_setup ;
  extern char     AFNI_infocom[] ;
  extern int      AFNI_verbose ;
  extern int      read_Signa_cvs() ;
#endif
/*******************************************************************************/

/*-- prototypes --*/

extern void AFNI_start_io(int) ;
extern void AFNI_send_image(int) ;
extern void AFNI_exit(void) ;

/*-- help string --*/

#ifdef MAIN
#define AFNI_HELP_STRING                                                \
  "\n"                                                                  \
  "\n-afni host       - Send reconstructed images to AFNI running on"   \
  "\n                   computer 'host':"                               \
  "\n                    * If 'host' = '%%' or '.', then the local"     \
  "\n                      system will be used, and data will be"       \
  "\n                      transferred to AFNI with shared memory."     \
  "\n                    * For other values of 'host', a TCP/IP socket" \
  "\n                      will be used to transfer data to AFNI.  If"  \
  "\n                      you want to use TCP/IP on the local system," \
  "\n                      use 'localhost'."                            \
  "\n                    * You must start AFNI with the command"        \
  "\n                      'afni -rt' for it to be able to accept"      \
  "\n                      image data in realtime."                     \
  "\n-verbose          - turn on debug messages for AFNI I/O"
#endif

/*-- how to execute a command on another system --*/

#ifdef HP
# define RSH "remsh"
#else
# define RSH "rsh"
#endif

/*=============================================================================*/
#endif /* _AFNI_EPI_HEADER_ */
