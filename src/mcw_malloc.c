/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifdef USE_OMP
#include <omp.h>
#endif

#include "mcw_malloc.h"
#include "Amalloc.h"
/*--------------------------------------------------------------------------
  24 Jan 2001: Modified heavily to use a hash table instead of a linear
               array to store the data about each allocated block.
  16 Feb 2001: Modified to include some debug traceback info (if
               USE_TRACING is defined - searce for TRAC to see code).
  -- RWCox
----------------------------------------------------------------------------*/

#define MAGIC  ((char) 0xd7)
#define NEXTRA (2*sizeof(int))

#undef malloc
#undef realloc
#undef calloc
#undef free
#undef XtMalloc
#undef XtRealloc
#undef XtFree
#undef XtCalloc
#undef mcw_malloc
#undef mcw_realloc
#undef mcw_calloc
#undef mcw_free

#undef  UINT
#define UINT unsigned int

#ifdef USE_TRACING        /* 16 Feb 2001 */
# include "debugtrace.h"
# undef  NTB    /* number of traceback levels to keep for each malloc() */
# define NTB 5  /* (chosen to make sizeof(mallitem) a multiple of 8)    */
#endif

/*-- struct to hold info about each malloc()-ed block --*/

typedef struct {
   void * pmt ;   /* pointer to actually malloc-ed block */
   size_t psz ;   /* size of allocated block */
   char * pfn ;   /* function name that called */
   int    pln ;   /* function line number */
   UINT   pss ;   /* serial number of this malloc */
#ifdef USE_TRACING
   char * ptb[NTB] ; /* traceback from debugtrace.h */
#endif
} mallitem ;

/** set hash table size (to a prime number, please) **/

/** #define SLOTS 503   **/ /* a little small */

#define SLOTS 8191

/** #define SLOTS 16381 **/
/** #define SLOTS 32003 **/

static mallitem ** htab  = NULL ; /* table of lists */
static int *       nhtab = NULL ; /* size of each list */
static UINT       serial = 0    ; /* serial number of allocation */

#undef INLINE
#ifdef __GNUC__
# define INLINE inline
#else
# define INLINE /*nada*/
#endif

#ifdef USE_TRACING
# define ADD_TRACEBACK(ip)                                                   \
   do{ int tt ;                                                              \
       for( tt=1 ; tt <= NTB ; tt++ )                                        \
         ip->ptb[tt-1] = (tt < DBG_num) ? DBG_rout[DBG_num-tt] : NULL; } while(0)
#else
# define ADD_TRACEBACK(ip) /* nada */
#endif

/*---------------------------------------------------------------
  Compute a unique non-negative integer key from an address
-----------------------------------------------------------------*/

static INLINE UINT mallkey( char * fred )
{
   UINT q = ((UINT) fred) ;

   q =   ((q & 0xf0f0f0f0) >> 4)   /* swap nibbles */
       | ((q & 0x0f0f0f0f) << 4) ;

   return q ;
}

/*----------------------------------------------------------------
   Find an address in the hash table;
   returns a pointer to the mallitem that owns it (or NULL)
------------------------------------------------------------------*/

static mallitem * ptr_tracker( void * fred )
{
   int jj,kk ;

   if( fred == NULL ) return NULL ;

   jj = mallkey((char *)fred) % SLOTS ;      /* hash table location */

   if( htab[jj] == NULL ) return NULL ;  /* nothing there */

   for( kk=0 ; kk < nhtab[jj] ; kk++ )   /* scan for match */
      if( htab[jj][kk].pmt == fred ) return (htab[jj]+kk) ;

   return NULL ; /* no match found */
}

/*-----------------------------------------------------------------*/
/*-- this macro finds an address from the user in the hash table --*/

#define shift_tracker(fff)  ptr_tracker( ((char *)(fff)) - NEXTRA )

/*-----------------------------------------------------------------
   Find an empty entry in the hash table list [jj] and return
   a pointer to it.  Will create the entry, if need be.
-------------------------------------------------------------------*/

static mallitem * find_empty_slot( int jj )
{
   int kk ;

#pragma omp critical (MCW_MALLOC_fes)
 {
   if( htab[jj] == NULL ){                               /* must make new list  */
      htab[jj] = (mallitem *) malloc(sizeof(mallitem)) ; /* of length 1 at [jj] */
     nhtab[jj] = 1 ;
      kk       = 0 ;
      htab[jj][0].pmt = NULL ;  /* mark as empty */
   } else {
      for( kk=nhtab[jj]-1 ; kk >= 0 ; kk-- )    /* scan (backwards) for NULL entry */
         if( htab[jj][kk].pmt == NULL ) break ; /* found it? */

      if( kk < 0 ){                             /* must make list longer */
         kk = nhtab[jj] ; nhtab[jj]++ ;
         htab[jj] = (mallitem *) realloc( htab[jj], sizeof(mallitem)*nhtab[jj] ) ;
         htab[jj][kk].pmt = NULL ;  /* mark as empty */
      }
   }
 } /* end OpenMP critical */

   return (htab[jj]+kk) ;
}

/*----------------------------------------------------------------
   Add an entry to the hash table, given the
   address, the user's size, and the filename and line number.
------------------------------------------------------------------*/

static void add_tracker( void * fred , size_t n , char * fn , int ln )
{
   int jj ;
   mallitem *ip ;

   if( fred == NULL ) return ;   /* bad news */

#pragma omp critical (MCW_MALLOC_at)
{
   jj = mallkey((char *)fred) % SLOTS ;  /* which hash list to use */
   ip = find_empty_slot(jj) ;    /* get an empty slot in this list */

   /* now put the data into the hash table */

   ip->pmt = fred ;
   ip->psz = n ;
   ip->pfn = fn ;
   ip->pln = ln ;
   ip->pss = ++serial ;

   ADD_TRACEBACK(ip) ;  /* 16 Feb 2001 */
 } /* end OpenMP critical */

   return ;
}

/*-----------------------------------------------------------------
  The tracking replacement for malloc
-------------------------------------------------------------------*/

static void * malloc_track( size_t n , char *fn , int ln )
{
   char *fred=NULL ;

#pragma omp critical (MCW_MALLOC_mt)
 { size_t nn = n + 2*NEXTRA ; int ii ;

   fred = (char *) malloc(nn) ;
   if( fred == NULL ){                                     /* real bad news */
      long long val ;
      fprintf(stderr,
              "\n*** MCW_malloc(%u) from %s#%d FAILS!\a\n",  /* 02 Jan 2002 */
              (unsigned int)n , fn , ln ) ;
      val = MCW_MALLOC_total ;
      if( val > 0 ) fprintf(stderr,"*** current total usage=%lld bytes\n",val);
      goto IAMDONE ;
   }

   /* mark overrun buffers */

   for( ii=0 ; ii < NEXTRA ; ii++ )
      fred[ii] = fred[n+NEXTRA+ii] = MAGIC ;

   add_tracker(fred,n,fn,ln) ;      /* put in hash table */
   IAMDONE: ;
 } /* end OpenMP critical */
   if( fred == NULL ) return NULL ;
   return (void *)(fred+NEXTRA) ;
}

/*-----------------------------------------------------------------
  Check an entry in the hash table for local overrun integrity
-------------------------------------------------------------------*/

static const char *pr_nam = NULL ;
static       int   pr_lin = 0 ;

static void probe_track( mallitem *ip )
{
   int ii ;
   size_t n ;
   char *fred ;

   if( ip == NULL ){ pr_nam=NULL; return; } /* error */
   fred = (char *) ip->pmt ; if( fred == NULL ){ pr_nam=NULL; return; }
   n = ip->psz ;

   for( ii=0 ; ii < NEXTRA ; ii++ )
      if( fred[ii] != MAGIC ){
         fprintf(stderr,"*** MCW_malloc pre-corruption!  "
                        "serial=%u size=%u source=%s line#%d",
                        ip->pss,(unsigned int)ip->psz,ip->pfn,ip->pln ) ;
#ifdef USE_TRACING
         { int tt ;
           for( tt=0 ; tt < NTB && ip->ptb[tt] != NULL ; tt++ )
             fprintf(stderr," <- %s",ip->ptb[tt]) ;
         }
#endif
         fprintf(stderr,"\n") ;

         if( pr_nam != NULL )
          fprintf(stderr," [[ Called from source=%.50s line#%d ]]\n",pr_nam,pr_lin);
         break ;
      }

   for( ii=0 ; ii < NEXTRA ; ii++ )
      if( fred[n+NEXTRA+ii] != MAGIC ){
         fprintf(stderr,"*** MCW_malloc post-corruption!  "
                        "serial=%u size=%u source=%s line#%d\n",
                        ip->pss,(unsigned int)ip->psz,ip->pfn,ip->pln ) ;
#ifdef USE_TRACING
         { int tt ;
           for( tt=0 ; tt < NTB && ip->ptb[tt] != NULL ; tt++ )
             fprintf(stderr," <- %s",ip->ptb[tt]) ;
         }
#endif
         fprintf(stderr,"\n") ;

         if( pr_nam != NULL )
          fprintf(stderr," [[ Called from source=%.50s line#%d ]]\n",pr_nam,pr_lin);
         break ;
      }

   pr_nam = NULL ;  /* reset */
   return ;
}

/*-------------------------------------------------------------------
  The tracking replacement for realloc
---------------------------------------------------------------------*/

static void * realloc_track( mallitem *ip, size_t n, char *fn, int ln )
{
   char *nfred=NULL ;

   if( ip == NULL ) return NULL ;  /* should not happen */

#pragma omp critical (MCW_MALLOC_rt)
 { char *cfred ;
   size_t nn = n + 2*NEXTRA ;
   int ii , cjj,njj , kk ;

   pr_nam = (const char *)fn ; pr_lin = ln ;
   probe_track(ip) ;  /* check for integrity before reallocation */
   cfred = ip->pmt ;  /* old address */

   nfred = (char*)realloc( cfred , nn ) ;
   if( nfred == NULL ){                                       /* real bad */
      long long val ;
      fprintf(stderr,
              "\n*** MCW_realloc(%u) from %s#%d FAILS!\a\n",  /* 02 Jan 2002 */
              (unsigned int)n , fn , ln ) ;
      val = MCW_MALLOC_total ;
      if( val > 0 ) fprintf(stderr,"*** current total usage=%lld bytes\n",val);
      goto IAMDONE ;
   }

   for( ii=0 ; ii < NEXTRA ; ii++ )
      nfred[ii] = nfred[n+NEXTRA+ii] = MAGIC ;

   cjj = mallkey(cfred) % SLOTS ;  /* hash table list for old */
   njj = mallkey(nfred) % SLOTS ;  /* and for new address */

   if( cjj == njj ){  /* can just update old hashtable entry */

      ip->pmt = nfred ;
      ip->psz = n ;
      ip->pfn = fn ;
      ip->pln = ln ;
      ip->pss = ++serial ;

      ADD_TRACEBACK(ip) ;  /* 16 Feb 2001 */

   } else {           /* must move into a different list */

      add_tracker( nfred , n , fn , ln ) ;

      ip->pmt = NULL ; /* mark old entry as free */
   }
   IAMDONE: ;
 } /* end OpenMP critical */
   if( nfred == NULL ) return NULL ;
   return (void *)(nfred+NEXTRA) ;
}

/*-----------------------------------------------------------------
  Tracking replacement for calloc
-------------------------------------------------------------------*/

static void * calloc_track( size_t n , size_t m , char *fn , int ln )
{
   void *fred ;
   size_t nn = n*m ;

   fred = malloc_track(nn,fn,ln) ; if( fred == NULL ) return NULL ;
   memset( fred , 0 , nn ) ;
   return fred ;
}

/*-----------------------------------------------------------------
  Tracking replacement for free
-------------------------------------------------------------------*/

static void free_track( mallitem *ip )
{
   char *cfred ;

   if( ip == NULL ) return ;
   cfred = (char *) ip->pmt ;
   if( cfred == NULL ) return ;

#pragma omp critical (MCW_MALLOC_ft)
 {
   pr_nam = NULL ;
   probe_track(ip) ;  /* check for integrity before freeing */
   free(cfred) ; ip->pmt = NULL ;
 } /* end OpenMP critical */
   return ;
}

/*-----------------------------------------------------------------
  Return a status line about the situation
-------------------------------------------------------------------*/

static int use_tracking = 0 ;  /* is the tracking enabled? */

char * mcw_malloc_status(const char *fn , int ln)
{
   static char buf[128] = "\0" ;
   if( ! use_tracking ) return NULL ;

#pragma omp critical (MCW_MALLOC_statusfunc)
 { int jj,kk , nptr=0 ; long long nbyt=0 ;

   for( jj=0 ; jj < SLOTS ; jj++ ){
      for( kk=0 ; kk < nhtab[jj] ; kk++ ){
         if( htab[jj][kk].pmt != NULL ){
            pr_nam = (const char *)fn ; pr_lin = ln ;
            probe_track( htab[jj]+kk ) ; /* check for integrity */
            nptr++ ; nbyt += htab[jj][kk].psz ;
         }
      }
   }

   sprintf(buf,"chunks=%d bytes=%lld",nptr,nbyt) ;
 } /* end OpenMP critical */
  return buf ;
}

/*-----------------------------------------------------------------*/

long long mcw_malloc_total(void)  /* 01 Feb 2007 */
{
   long long nbyt=0 ; int jj,kk ;

   if( ! use_tracking ) return 0 ;

   for( jj=0 ; jj < SLOTS ; jj++ )
     for( kk=0 ; kk < nhtab[jj] ; kk++ )
       if( htab[jj][kk].pmt != NULL ) nbyt += htab[jj][kk].psz ;

   return nbyt ;
}

/*-----------------------------------------------------------------
  Write a file with lots of info about the current status
-------------------------------------------------------------------*/

extern int THD_is_file( char * ) ;
extern void qsort_intint( int , int * , int * ) ;

void mcw_malloc_dump(void)
{
   if( ! use_tracking ) return ;

#pragma omp critical (MCW_MALLOC_dump)
 { int ii,jj,kk ;
   char fname[32] , *str ;
   FILE *fp = NULL ;
   int nptr=0 ;
   int *ss , *jk ;

   /* find and open an output file */

   for( ii=1 ; ii < 1000 ; ii++ ){
      sprintf(fname,"malldump.%03d",ii) ;
      if( THD_is_file(fname) ) continue ;
      fp = fopen( fname , "w" ) ;
      if( fp == NULL ){
         fprintf(stderr,"** Unable to open file %s for malloc table dump!\n",
                 fname ) ;
         goto IAMDONE ;
      }
      break ;
   }

   if( fp == NULL ){
      fprintf(stderr,"** Attempt to exceed 999 malloc table dump files!\n") ;
      goto IAMDONE ;
   }

   /* count number of entries in the hash table */

   for( jj=0 ; jj < SLOTS ; jj++ ){
      for( kk=0 ; kk < nhtab[jj] ; kk++ ){
         if( htab[jj][kk].pmt != NULL ) nptr++ ;
      }
   }

   if( nptr < 1 ){
      fprintf(fp    ,"--- Nothing is malloc()-ed !? ---\n") ;
      fprintf(stderr,"--- Nothing is malloc()-ed !? ---\n") ;
      fclose(fp) ;
   }

   /* setup to sort by serial number */

   ss = (int *) malloc(sizeof(int)*nptr) ;  /* serial number */
   jk = (int *) malloc(sizeof(int)*nptr) ;  /* holds combination of jj and kk */

#define JBASE 32768  /* JBASE * SLOTS must be less than max int */

   /* scan table for non-NULL entries */

   for( ii=jj=0 ; jj < SLOTS ; jj++ ){
      for( kk=0 ; kk < nhtab[jj] ; kk++ ){
         if( htab[jj][kk].pmt != NULL ){
            ss[ii] = htab[jj][kk].pss ;   /* save serial number */
            jk[ii] = JBASE*jj + kk ;      /* save jj and kk */
            ii++ ;
         }
      }
   }

   qsort_intint( nptr , ss , jk ) ;  /* sort by ss, carrying jk along */

   /* now print table in serial number order */

#ifdef USE_TRACING
   fprintf(fp, "MCW Malloc Table Dump:\n"
               "serial# size       source file          line# address    hash(j,k) <- Called by\n"
               "------- ---------- -------------------- ----- ---------- ----- ---    ---------\n");
#else
   fprintf(fp, "MCW Malloc Table Dump:\n"
               "serial# size       source file          line# address    hash(j,k)\n"
               "------- ---------- -------------------- ----- ---------- ----- ---\n") ;
#endif

   for( ii=0 ; ii < nptr ; ii++ ){
      jj = jk[ii] / JBASE ;           /* retrieve jj and kk */
      kk = jk[ii] % JBASE ;
      if( htab[jj][kk].pmt != NULL ){
         fprintf(fp,"%7u %10d %-20.30s %5d %10p %5d %3d",
                 htab[jj][kk].pss , (int)htab[jj][kk].psz ,
                 htab[jj][kk].pfn , htab[jj][kk].pln , htab[jj][kk].pmt ,
                 jj,kk ) ;
#ifdef USE_TRACING
         { int tt ;
           for( tt=0 ; tt < NTB && htab[jj][kk].ptb[tt] != NULL ; tt++ )
              fprintf(fp," <- %s",htab[jj][kk].ptb[tt]) ;
         }
#endif
         fprintf(fp,"\n") ;
      }
      else
         fprintf(fp,"*** Error at ii=%d jj=%d kk=%d\n",ii,jj,kk) ;
   }

   free(ss) ; free(jk) ;

   /* and print out the summary line (to the file and screen) */

   str = mcw_malloc_status(NULL,0) ;
   fprintf(fp,"----- Summary: %s\n",str) ;
   fclose(fp) ;

   fprintf(stderr,"** Malloc table dumped to file %s\n",fname) ;
   fprintf(stderr,"** Summary: %s\n",str) ;

   IAMDONE: ;
 } /* end OpenMP critical */

 return ;
}

/*----------------------------------------------------------------
  Turn on use of the tracking routines
------------------------------------------------------------------*/

void enable_mcw_malloc()       /* cannot be disabled */
{
   char *str = getenv("AFNI_NO_MCW_MALLOC") ;  /* NOT my_getenv */

#pragma omp critical (MCW_MALLOC_enable)
 {
   if( use_tracking ) goto IAMDONE ;
   use_tracking = 1 ;
   if( str!=NULL && ( str[0]=='y' || str[0]=='Y') ) use_tracking = 0 ;

   if( use_tracking && htab == NULL ){  /* initialize hash table */
      int jj ;
      htab  = (mallitem **) malloc( SLOTS * sizeof(mallitem *) ) ;
      nhtab = (int *)       malloc( SLOTS * sizeof(int) ) ;
      for( jj=0 ; jj < SLOTS ; jj++ ){
         htab[jj] = NULL ; nhtab[jj] = 0 ;
      }
   }
   IAMDONE: ;
 } /* end OpenMP critical */
  return ;
}

/* ---------------------------------------
   Force a halt to memory tracking.
   the halt is meant to be temporary.
   Had to add this for SUMA to get around
   the problem of using allocate2D with
   large numbers of pointers. ZSS 03/04
   --------------------------------------- */
static int pz = 0;   /* flag to indicate pause */
void pause_mcw_malloc()
{
#pragma omp critical (MCW_MALLOC_pause)
 {
   if (!pz && use_tracking) {
      pz = 1; use_tracking = 0;
   }
 } /* end OpenMP critical */
 return;
}
void resume_mcw_malloc()
{
#pragma omp critical (MCW_MALLOC_pause)
 {
   if( pz ){
     pz = 0; use_tracking = 1;
   }
 } /* end OpenMP critical */
 return;
}
int mcw_malloc_paused()
{
   return(pz);
}

/*---------------------------------------------------------------*/
/*--- lets the user check if the tracking routines are in use ---*/

int mcw_malloc_enabled(void){ return (use_tracking != 0) ; }

/*-----------------------------------------------------------------
   The actual routine that replaces malloc() -- see mcw_malloc.h
-------------------------------------------------------------------*/

void * mcw_malloc( size_t n , char *fnam , int lnum )
{
   if( use_tracking ) return malloc_track(n,fnam,lnum) ;
   else               return malloc(n) ;
}

/*----------------------------------------------------------------
   The actual replacement for realloc()
------------------------------------------------------------------*/

void * mcw_realloc( void * fred , size_t n , char *fnam , int lnum )
{
   mallitem *ip ;

   if( fred == NULL )
      return mcw_malloc( n , fnam , lnum ) ;

   if( use_tracking && (ip=shift_tracker(fred)) != NULL )
      return realloc_track( ip , n , fnam,lnum ) ;
   else
      return realloc( fred , n ) ;
}

/*----------------------------------------------------------------
   The actual replacement for calloc()
------------------------------------------------------------------*/

void * mcw_calloc( size_t n , size_t m , char *fnam , int lnum )
{
   if( use_tracking ) return calloc_track( n , m , fnam,lnum ) ;
   else               return calloc( n , m ) ;
}

/*-----------------------------------------------------------------
    The actual replacment for free()
-------------------------------------------------------------------*/

void mcw_free( void *fred )
{
   mallitem *ip ;

   if( fred == NULL ) return ;
   if( use_tracking && (ip=shift_tracker(fred)) != NULL ) free_track( ip ) ;
   else                                                   free( fred ) ;
}

/*-----------------------------------------------------------------
   The actual replacement for XtMalloc()
-------------------------------------------------------------------*/

char * mcw_XtMalloc( Cardinal n , char *fnam , int lnum )
{
   if( use_tracking ) return (char *)malloc_track(n,fnam,lnum) ;
   else               return (char *)XtMalloc(n) ;
}

/*-----------------------------------------------------------------
   The actual replacement for XtRealloc()
-------------------------------------------------------------------*/

char * mcw_XtRealloc( char *p, Cardinal n , char *fnam , int lnum )
{
   mallitem *ip ;

   if( p == NULL )
      return mcw_XtMalloc( n , fnam , lnum ) ;

   if( use_tracking && (ip=shift_tracker(p)) != NULL )
      return (char*)realloc_track( ip , n , fnam,lnum ) ;
   else
      return (char*)XtRealloc( p , n ) ;
}

/*----------------------------------------------------------------
   The actual replacement for XtFree()
------------------------------------------------------------------*/

void mcw_XtFree( char *p )
{
   mallitem *ip ;

   if( p == NULL ) return ;
   if( use_tracking && (ip=shift_tracker(p)) != NULL ) free_track(ip) ;
   else                                                XtFree(p) ;
}

/*-----------------------------------------------------------------
  The actual replacement for XtCalloc()
-------------------------------------------------------------------*/

char * mcw_XtCalloc( Cardinal n , Cardinal m , char *fnam , int lnum )
{
   if( use_tracking ) return (char *) calloc_track( n , m , fnam,lnum ) ;
   else               return XtCalloc( n , m ) ;
}
