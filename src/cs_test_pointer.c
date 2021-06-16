#include <signal.h>
#include <setjmp.h>

/*----------------------------------------------------------------------------*/
typedef void (*sig_t) (int);
sig_t   old_segv_handler ;    /* to restore the old signal handler */
sig_t   old_sbus_handler ;    /* to restore the old signal handler */
jmp_buf return_to_tester ;    /* how to jump out of signal handler below */
static volatile int siggg = 0 ;

void tester_sighandler( int sss )    /* called for SIGSEGV = seg fault */
{
   siggg = sss ;
   longjmp(return_to_edgeize,666) ;  /* jump back to mri_interior_edgeize */
}

/*----------------------------------------------------------------------------*/
/* Test the pointer to see if accessing causes a SEGV or BUS fault [Apr 2021] */
/* Return value == 1 for no errors, == 0 for an error. Use this info wisely.  */
/*----------------------------------------------------------------------------*/

int test_pointer_for_access( void *vvv )
{
   int rrr = 1 ;
   static int nerr=0 ;

   if( vvv == NULL ) return 0 ;

   old_segv_handler = signal( SIGSEGV , tester_sighandler ) ;
   old_sbus_handler = signal( SIGBUS  , tester_sighandler ) ;

   if( setjmp(return_to_tester) == 0 ){
     char *ppp , ccc ;
     ppp = (char *)vvv ;
     ccc = *ppp ;  /* this line the test of whether can access the pointer */
   } else {
     rrr = 0 ;
     if( ++nerr < 10 )
       fprintf(stderr,"** ERROR: invalid memory pointer %p\n",
               vvv ,
               (  (siggg==SIGSEGV) ? " (SIGSEGV)"
                 :(siggg==SIGBUS)  ? " (SIGBUS)" : "\0" ) ) ;
   }

   signal( SIGSEGV , old_segv_handler ) ;
   signal( SIGBUS  , old_sbus_handler ) ;
   return rrr ;
}
