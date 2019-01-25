#define _MCW_MALLOC_HEADER_
#include "mrilib.h"

#ifndef DARWIN
# include <malloc.h>
#endif

#include <unistd.h>
#include <time.h>

int MRILIB_verb = 0 ;

/*--------------------------------------------------------------------*/

static int be_quiet = 0 ;
int machdep_be_quiet(void){ return be_quiet ; }

/*--------------------------------------------------------------------
   Code to provide runtime fixups for various machines
   (things that can't be fixed by declarations in machdep.h).
   This should be called at the start of every program.
----------------------------------------------------------------------*/

void machdep()
{
   long seed ;
   static int first=1 ;

   if( !first ) return ; else first = 0 ;

   /*-- force use of mcw_malloc.c functions - 05 Nov 2001 --*/

#ifdef USING_MCW_MALLOC
   if( AFNI_yesenv("AFNI_FORCE_MCW_MALLOC") ) enable_mcw_malloc();
#endif

   /*-- disable mmap() in malloc() [21 Aug 2002: mostly] --*/

#if defined(LINUX) && defined(M_MMAP_MAX)
   mallopt( M_MMAP_MAX , 1 ) ;
#endif

   seed = (long)AFNI_numenv("AFNI_RANDOM_SEEDVAL") ;
   init_rand_seed(seed) ;

   be_quiet = AFNI_yesenv("AFNI_QUIET_STARTUP") ;  /* 08 Dec 2010 */

   if( AFNI_yesenv("AFNI_USE_FGETS") ) afni_fgets_setskip(1) ; /* 21 Dec 2011 */

   AFNI_do_nothing() ; /* 02 Oct 2012 */
   return ;
}

/*-------------------------------------------------------------------*/

#include <unistd.h>
#ifdef USE_SYSCTL
# include <sys/types.h>
# include <sys/sysctl.h>
#endif

int AFNI_get_ncpu(void)  /* 11 Feb 2016 */
{
   int32_t nnn=0 ;

#ifdef _SC_NPROCESSORS_CONF
   nnn = sysconf(_SC_NPROCESSORS_CONF) ;
#endif

#ifdef USE_SYSCTL
   { size_t isiz=sizeof(int32_t) ;
     if( nnn < 1 )
       sysctlbyname( "hw.logicalcpu" , &nnn , &isiz , NULL,0 ) ;

     if( nnn < 1 )
       sysctlbyname( "hw.ncpu" , &nnn , &isiz , NULL,0 ) ;
   }
#endif

   if( nnn < 1 ) nnn = 1 ;

   return (int)nnn ;
}

/*-------------------------------------------------------------------*/

int64_t AFNI_get_memsize(void)  /* in bytes -- 02 Aug 2016 */
{
   int64_t mmm=0 , psiz=0 , pnum=0 ;

#ifdef _SC_PAGESIZE
   psiz = (int64_t)sysconf(_SC_PAGESIZE) ;
#endif
#ifdef _SC_PHYS_PAGES
   pnum = (int64_t)sysconf(_SC_PHYS_PAGES) ;
#endif
#ifdef _SC_AVPHYS_PAGES
   if( pnum == 0 )
     pnum = (int64_t)sysconf(_SC_AVPHYS_PAGES) ;
#endif
   if( psiz > 0 && pnum > 0 ) return (psiz*pnum) ;

#ifdef USE_SYSCTL
   { size_t isiz=sizeof(int64_t) ;
     sysctlbyname( "hw.memsize" , &mmm , &isiz , NULL,0 ) ;
   }
#endif
   return mmm ;
}

/*-------------------------------------------------------------------*/

char * GetAfniWebBrowser(void)
{
   static char *awb=NULL;
   awb = getenv("AFNI_WEB_BROWSER") ;
#ifdef DARWIN
   if( awb == NULL ) awb = "open" ;  /* for Mac OS X */
#endif
   if( awb == NULL ) awb = THD_find_executable( "chrome" )   ;
   if( awb == NULL ) awb = THD_find_executable( "google-chrome" ) ;
   if( awb == NULL ) awb = THD_find_executable( "firefox" )  ;
   if( awb == NULL ) awb = THD_find_executable( "mozilla" )  ;
   if( awb == NULL ) awb = THD_find_executable( "netscape" ) ;
   if( awb == NULL ) awb = THD_find_executable( "opera" )    ;
   if( awb == NULL ) awb = THD_find_executable( "epiphany" ) ;
   if( awb == NULL ) awb = THD_find_executable( "midori" )   ;
   if( awb == NULL ) awb = THD_find_executable( "google-chrome-unstable" ) ;
   return(awb);
}

/*-------------------------------------------------------------------*/

char * GetAfniTextEditor(void)
{
   static char *ate=NULL;
   ate = getenv("AFNI_GUI_EDITOR");

   if( ate ) return ate;

   /* else, hunt */
   if( ate == NULL ) ate = THD_find_executable( "nedit" )   ;
   if( ate == NULL ) ate = THD_find_executable( "kedit" )   ;
   if( ate == NULL ) ate = THD_find_executable( "gedit" )   ;
   if( ate == NULL ) ate = THD_find_executable( "kwrite" )   ;
   if( ate == NULL ) ate = THD_find_executable( "kate" )   ;
#ifdef DARWIN
   if( ate == NULL ) ate = "open -t" ;  /* for Mac OS X */
#endif

   return(ate);
}

/*-------------------------------------------------------------------*/

char * GetAfniWebDownloader(void)
{
   static char *ate=NULL;
   ate = getenv("AFNI_WEB_DOWNLOADER");

   if( ate ) return ate;

   /* else, hunt */
   if( ate == NULL ) if (THD_find_executable( "curl" )) ate = "curl -O -f" ;
   if( ate == NULL ) ate = THD_find_executable( "wget" )   ;

   return(ate);
}

/*-------------------------------------------------------------------*/

char * GetAfniPDFViewer(void)
{
   static char *ate=NULL;
   ate = getenv("AFNI_PDF_VIEWER");

   if( ate ) return ate;

   /* else, hunt */
   if( ate == NULL ) ate = THD_find_executable( "Preview" )   ;
   if( ate == NULL ) ate = THD_find_executable( "evince" )   ;
   if( ate == NULL ) ate = THD_find_executable( "acroread" )   ;
   if( ate == NULL ) ate = GetAfniWebBrowser()   ; /* last resort? */

   return(ate);
}

/*-------------------------------------------------------------------*/

char * GetAfniImageViewer(void)
{
   static char *ate=NULL;
   ate = getenv("AFNI_IMAGE_VIEWER");

   if( ate ) return ate;

   /* else, hunt */
   if( ate == NULL ) ate = THD_find_executable( "Preview" )   ;
   if( ate == NULL ) ate = THD_find_executable( "aiv" )   ;

   return(ate);
}

/*-------------------------------------------------------------------*/

void init_rand_seed( long int seed )
{
   if( seed == 0 ){
     FILE *ufp=fopen("/dev/urandom","rb") ;
     seed = (long)time(NULL)+37*(long)getpid() ;
     if( ufp != NULL ){  /* get some extra randomness [20 Nov 2016] */
       byte urr=0 ;
       (void)fread( &urr , sizeof(byte),1, ufp ); fclose(ufp);
       seed += (long)urr ;
     }
   }
   srand48(seed) ;
}

/*-------------------------------------------------------------------
  To use the random()/srandom() library instead of srand48, do
    #define USE_RANDOM
  in machdep.h for your machine -- RWCox - 04 Sep 2001
---------------------------------------------------------------------*/
#ifdef USE_RANDOM
void srand48( long int s ){ srandom((unsigned int)s); }

double drand48(void){ return (((double)random())/LONG_MAX); }

long int lrand48(void){ return random(); }

long int mrand48(void)       /* need 32 bits but random only gives 31 */
{ register long i , j , k ;
  i = random() ; j = random() ;
  k = ((i & 0xFFFF)<<16) | (j & 0xFFFF) ; return k ;
}
#endif /* USE_RANDOM */

/*-------------------------------------------------------------------
  If the system doesn't provide this function for some reason ...
---------------------------------------------------------------------*/

#ifdef NEED_XSETLOCALE
#include <locale.h>
char * _Xsetlocale( int category, const char * locale)
{ return setlocale(category,locale) ; }
#endif

/*----- 09 Apr 2002 -----*/

#ifdef NEED_NL_LANGINFO
char * nl_langinfo(){ return "ISO-8859-1"; }
#endif

#ifdef NO_GAMMA
/*-------------------------------------------------------------------*/
/* If the system doesn't provide lgamma() for some reason.
---------------------------------------------------------------------*/

/** log of gamma, for argument between 1 and 2 **/

static double gamma_12( double y )
{
   double x , g ;
   x = y - 1.0 ;
   g = ((((((( 0.035868343 * x - 0.193527818 ) * x
                               + 0.482199394 ) * x
                               - 0.756704078 ) * x
                               + 0.918206857 ) * x
                               - 0.897056937 ) * x
                               + 0.988205891 ) * x
                               - 0.577191652 ) * x + 1.0 ;
   return log(g) ;
}

/** asymptotic expansion of ln(gamma(x)) for large positive x **/

#define LNSQRT2PI 0.918938533204672  /* ln(sqrt(2*PI)) */

static double gamma_asympt(double x)
{
   double sum ;

   sum = (x-0.5)*log(x) - x + LNSQRT2PI + 1.0/(12.0*x) - 1./(360.0*x*x*x) ;
   return sum ;
}
/** log of gamma, argument positive (not very efficient!) **/

double lgamma( double x )
{
   double w , g ;

   if( x <= 0.0 ){
      fprintf(stderr,"Internal gamma: argument %g <= 0\a\n",x) ;
      return 0.0 ;
   }

   if( x <  1.0 ) return gamma_12( x+1.0 ) - log(x) ;
   if( x <= 2.0 ) return gamma_12( x ) ;
   if( x >= 6.0 ) return gamma_asympt(x) ;

   g = 0 ; w = x ;
   while( w > 2.0 ){
      w -= 1.0 ; g += log(w) ;
   }
   return ( gamma_12(w) + g ) ;
}
#endif  /* NO_GAMMA */

/*---------------------------------------------------------------------------*/

char * Random_Insult(void)
{
#define NINSULT 18
   static char *ins[NINSULT]={ "Stupid"    , "Moronic"   , "Cretinous"   ,
                               "Idiotic"   , "Bozonic"   , "Criminal"    ,
                               "Repulsive" , "Dumb"      , "Pinheaded"   ,
                               "Fatuous"   , "Asinine"   , "Imbecilic"   ,
                               "Oafish"    , "Doltish"   , "Duncical"    ,
                               "Witless"   , "Brainless" , "Flatbrained"
   } ;
   int ii = (lrand48()>>5) % NINSULT ;
   return ins[ii] ;
}

/*---------------------------------------------------------------------------*/
#define SOL_TO_LOWER(c) ( ((c) >= 'A' && (c) <= 'Z') ? (c + 'a' - 'A') : (c) )
char *AFNI_strcasestr(const char *s1, const char *s2)
{
   char *s1u=NULL;
   char *s2u=NULL;
   char *so=NULL;
   int off=0;
   int i = 0;

   if (!s1 || !s2 || s2[0] == '\0')
      return(strstr(s1,s2)); /* let it fail as in strstr
                                You will get death if s2 is NULL */

   if (!(s1u = strdup(s1))) {
      fprintf(stderr,"AFNI_strcasestr: Failed to dup string 1\n");
      return(NULL);
   }
   if (!(s2u = strdup(s2))) {
      fprintf(stderr,"AFNI_strcasestr: Failed to dup string 2\n");
      free(s1u);
      return(NULL);
   }
   i=0; while (s1u[i]!='\0') { s1u[i] = SOL_TO_LOWER(s1u[i]); ++i; }
   i=0; while (s2u[i]!='\0') { s2u[i] = SOL_TO_LOWER(s2u[i]); ++i; }

   so = strstr(s1u,s2u);
   off=0;
   if (so)  off = so-s1u;
   free(s1u); free(s2u);

   if (so) return((char*)s1+off);
   return(NULL);
}

/*---------------------------------------------------------------------------*/

void AFNI_do_nothing(void){
  static int fdn=-666 ;
  if( fdn == -666 ) fdn = open("/dev/null",O_WRONLY) ;
  if( fdn >= 0 ) write(fdn," ",1) ;
  return ;
}

/*---------------------------------------------------------------------------*/
