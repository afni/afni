#include "niml_private.h"

/***************************************************************************/
/************************  NIML malloc stuff *******************************/
/***************************************************************************/

/*-------------------------------------------------------------------------*/
/** 25 Mar 2003: allow user to replace malloc, realloc, free functions    **/

static void * (*user_malloc)(size_t)         = NULL ;
static void * (*user_realloc)(void *,size_t) = NULL ;
static void   (*user_free)(void *)           = NULL ;
static int  use_userfunc                     = 0    ;
static int  ni_mall_used                     = 0    ;

/*-------------------------------------------------------------------------*/
/*! Allow user to replace malloc(), realloc(), and free() functions used
    in NI_malloc(), NI_realloc(), and NI_free().
      - um = replacement for malloc()
      - ur = replacement for realloc()
      - uf = replacement for free()
      - all 3 must be non-NULL
      - this function must be called BEFORE any call to NI_malloc() etc.
        takes place, or this function will fail
      - return value is 1 if the replacement is accepted, 0 if not
      - note that NI_malloc() always 0 fills the result, even the one
         returned by um()
      - RWCox - 25 Mar 2003 (VR Day)
---------------------------------------------------------------------------*/

int NI_malloc_replace( void *(*um)(size_t)        ,
                       void *(*ur)(void *,size_t) ,
                       void  (*uf)(void *)         ){

  if( ni_mall_used ||
      use_userfunc ||
      um == NULL   ||
      ur == NULL   ||
      uf == NULL     ) return 0 ;

  user_malloc  = um ;
  user_realloc = ur ;
  user_free    = uf ;
  use_userfunc = 1  ;
  return 1 ;
}

#if defined(NIML_OLD_MALLOC) || defined(DONT_USE_MCW_MALLOC)

/*--------------------------------------------------------------------------*/
/*! Allocate memory (actually uses calloc); calls exit() if it fails.
----------------------------------------------------------------------------*/

void * old_NI_malloc( size_t len )
{
   void *p ;
   if( use_userfunc ){
     p = user_malloc(len) ; if( p != NULL ) memset(p,0,len) ;
   } else {
     p = calloc(1,len) ;
   }
   if( p == NULL ){
     fprintf(stderr,"** ERROR: old_NI_malloc() fails. Aauugghh!\n") ;
     NI_sleep(333); exit(1);
   }
   ni_mall_used = 1 ; return p ;
}

/*--------------------------------------------------------------------------*/
/*! Free memory; NULL pointer is just ignored.
----------------------------------------------------------------------------*/

void NI_free( void *p )
{
   if( p != NULL ){
     if( use_userfunc ) user_free(p) ;
     else               free(p) ;
   }
   ni_mall_used = 1 ;
}

/*--------------------------------------------------------------------------*/
/*! Reallocate memory; calls exit() if it fails.
----------------------------------------------------------------------------*/

void * old_NI_realloc( void *p , size_t len )
{
   void *q ;

   if( use_userfunc ) q = user_realloc( p , len ) ;
   else               q = realloc( p , len ) ;
   if( q == NULL && len > 0 ){
     fprintf(stderr,"** ERROR: old_NI_realloc() fails. Ooooogg!\n");
     NI_sleep(333); exit(1);
   }
   ni_mall_used = 1 ; return q ;
}

/**** Fake routines with no meaning in this NI_malloc version ****/

char * NI_malloc_status          (void){ return "disabled"; }
void   NI_malloc_dump            (void){ return;      }
void   NI_malloc_enable_tracking (void){ return;      }
int    NI_malloc_tracking_enabled(void){ return 0;    }

/*****************************************************************************/
#else  /**  not NIML_OLD_MALLOC or DONT_USE_MCW_MALLOC                 *******/
       /**  18 Nov 2002: keep track of mallocs, as in mcw_malloc.[ch]  *******/
/*****************************************************************************/

#define MAGIC  ((char) 0xd7)     /* goes in extra bytes at ends of blocks */
#define NEXTRA (2*sizeof(int))   /* number of extra bytes */

#undef  UINT
#define UINT unsigned int

/*-- struct to hold info about each malloc()-ed block --*/

typedef struct {
  void  *pmt ;   /* pointer to actually malloc-ed block
                    (user ptr is +NEXTRA bytes later)   */
  size_t psz ;   /* size of allocated block */
  char  *pfn ;   /* function name that called */
  int    pln ;   /* function line number */
  UINT   pss ;   /* serial number of this malloc */
} NI_mallitem ;

/** set hash table size (to a prime number, please) **/

#undef  SLOTS
#define SLOTS 1031

/** #define SLOTS 2053  **/
/** #define SLOTS 4099  **/
/** #define SLOTS 8191  **/
/** #define SLOTS 16381 **/
/** #define SLOTS 32003 **/

static NI_mallitem ** htab  = NULL ; /* table of lists */
static int *          nhtab = NULL ; /* size of each list */
static UINT          serial = 0    ; /* serial number of allocation */

#undef INLINE
#ifdef __GNUC__
# define INLINE inline
#else
# define INLINE /*nada*/
#endif

/*** prototypes for internal routines defined below ***/

static NI_mallitem * ptr_tracker( void * ) ;
static NI_mallitem * find_empty_slot( int ) ;
static void add_tracker( void * , size_t , char * , int ) ;
static void * malloc_track( size_t , char * , int ) ;
static void probe_track( NI_mallitem * , char *,int ) ;
static void * realloc_track( NI_mallitem *, size_t , char *, int  ) ;
static void * calloc_track( size_t , size_t , char * , int  ) ;
static void free_track( NI_mallitem * ) ;
static void qsort_intint( int, int *, int * ) ;

#undef malloc
#undef realloc
#undef calloc
#undef free

/*---------------------------------------------------------------*/
/*!  Compute a unique non-negative integer key from an address
-----------------------------------------------------------------*/

static INLINE UINT mallkey( char *fred )
{
   UINT q = (UINT)(intptr_t)fred ;

   q =   ((q & 0xf0f0f0f0) >> 4)   /* swap nibbles */
       | ((q & 0x0f0f0f0f) << 4) ;

   return q ;
}

/*----------------------------------------------------------------*/
/*! Find an address in the hash table;
    returns a pointer to the NI_mallitem that owns it (or NULL)
------------------------------------------------------------------*/

static NI_mallitem * ptr_tracker( void *fred )
{
   int jj,kk ;

   if( fred == NULL ) return NULL ;

   jj = mallkey((char *)fred) % SLOTS ;  /* hash table location */

   if( htab[jj] == NULL ) return NULL ;  /* nothing there */

   for( kk=0 ; kk < nhtab[jj] ; kk++ )   /* scan for match */
     if( htab[jj][kk].pmt == fred ) return (htab[jj]+kk) ;

   return NULL ; /* no match found */
}

/*-----------------------------------------------------------------*/
/*-- this macro finds an address from the user in the hash table --*/

#define shift_tracker(fff)  ptr_tracker( ((char *)(fff)) - NEXTRA )

/*-----------------------------------------------------------------*/
/*! Find an empty entry in the hash table list [jj] and return
   a pointer to it.  Will create the entry, if need be.
-------------------------------------------------------------------*/

static NI_mallitem * find_empty_slot( int jj )
{
   int kk ;

   if( htab[jj] == NULL ){                                    /* must make new list  */
     htab[jj] = (NI_mallitem *) malloc(sizeof(NI_mallitem)) ; /* of length 1 at [jj] */
    nhtab[jj] = 1 ;
     kk       = 0 ;
     htab[jj][0].pmt = NULL ;  /* mark as empty */
   } else {
     for( kk=nhtab[jj]-1 ; kk >= 0 ; kk-- )    /* scan (backwards) for NULL entry */
       if( htab[jj][kk].pmt == NULL ) break ;  /* found it? */

     if( kk < 0 ){                             /* must make list longer */
       kk = nhtab[jj] ; nhtab[jj]++ ;
       htab[jj] = (NI_mallitem *) realloc( htab[jj], sizeof(NI_mallitem)*nhtab[jj] ) ;
       htab[jj][kk].pmt = NULL ;  /* mark as empty */
     }
   }

   return (htab[jj]+kk) ;
}

/*----------------------------------------------------------------*/
/*! Add an entry to the hash table, given the
   address, the user's size, and the filename and line number.
------------------------------------------------------------------*/

static void add_tracker( void *fred , size_t n , char *fn , int ln )
{
   int jj ;
   NI_mallitem *ip ;

   if( fred == NULL ) return ;   /* bad news */

   jj = mallkey((char *)fred) % SLOTS ;  /* which hash list to use */
   ip = find_empty_slot(jj) ;            /* get an empty slot in this list */

   /* now put the data into the hash table */

   ip->pmt = fred ;
   ip->psz = n ;
   ip->pfn = fn ;
   ip->pln = ln ;
   ip->pss = ++serial ;

   return ;
}

/*-----------------------------------------------------------------*/
/*!  The tracking replacement for malloc().
-------------------------------------------------------------------*/

static void * malloc_track( size_t n , char *fn , int ln )
{
   char *fred ;
   size_t nn = n + 2*NEXTRA ;
   int ii ;

   fred = (char *)malloc(nn) ;
   if( fred == NULL ) return NULL ;  /* real bad news */

   /* mark overrun buffers */

   memset( fred           , MAGIC , NEXTRA ) ;
   memset( fred+(n+NEXTRA), MAGIC , NEXTRA ) ;

   ni_mall_used = 1 ;
   add_tracker(fred,n,fn,ln) ;      /* put in hash table */
   return (void *)(fred+NEXTRA) ;
}

/*-----------------------------------------------------------------*/
/*! Check an entry in the hash table for local overrun integrity.
-------------------------------------------------------------------*/

static void probe_track( NI_mallitem *ip , char *fn, int ln )
{
   int ii ;
   size_t n ;
   char *fred ;

   if( ip == NULL ) return ; /* error */
   fred = (char *) ip->pmt ; if( fred == NULL ) return ;
   n = ip->psz ;

   for( ii=0 ; ii < NEXTRA ; ii++ )
     if( fred[ii] != MAGIC ){
       fprintf(stderr,"*** NI_malloc pre-corruption!  "
                      "serial=%u size=%u source=%s line#=%d\n",
                      ip->pss,(unsigned int)ip->psz,ip->pfn,ip->pln ) ;
       if( fn != NULL ) fprintf(stderr,"   Caller=%s line#=%d\n",fn,ln) ;
       break ;
     }

   for( ii=0 ; ii < NEXTRA ; ii++ )
     if( fred[n+NEXTRA+ii] != MAGIC ){
       fprintf(stderr,"*** NI_malloc post-corruption!  "
                      "serial=%u size=%u source=%s line#=%d\n",
                      ip->pss,(unsigned int)ip->psz,ip->pfn,ip->pln ) ;
       if( fn != NULL ) fprintf(stderr,"   Caller=%s line#=%d\n",fn,ln) ;
       break ;
     }

   return ;
}

/*-------------------------------------------------------------------*/
/*! The tracking replacement for realloc().
---------------------------------------------------------------------*/

static void * realloc_track( NI_mallitem *ip, size_t n, char *fn, int ln )
{
   char *nfred , *cfred ;
   size_t nn = n + 2*NEXTRA ;
   int ii , cjj,njj , kk ;

   if( ip == NULL ) return NULL ;  /* should not happen */

   probe_track(ip,fn,ln) ;    /* check for integrity before reallocation */
   cfred = (char *)ip->pmt ;  /* old address */

   ni_mall_used = 1 ;
   nfred = (char *)realloc( (void *)cfred , nn ) ;
   if( nfred == NULL ) return NULL ;  /* this is bad - real bad */

   memset( nfred           , MAGIC , NEXTRA ) ;
   memset( nfred+(n+NEXTRA), MAGIC , NEXTRA ) ;

   cjj = mallkey(cfred) % SLOTS ;  /* hash table list for old */
   njj = mallkey(nfred) % SLOTS ;  /* and for new address */

   if( cjj == njj ){  /* can just update old hashtable entry */

     ip->pmt = nfred ;
     ip->psz = n ;
     ip->pfn = fn ;
     ip->pln = ln ;
     ip->pss = ++serial ;

   } else {           /* must move into a different list */

     add_tracker( nfred , n , fn , ln ) ;

     ip->pmt = NULL ; /* mark old entry as free */
   }

   return (void *)(nfred+NEXTRA) ;
}

/*-----------------------------------------------------------------*/
/*!  Tracking replacement for calloc().
-------------------------------------------------------------------*/

static void * calloc_track( size_t n , size_t m , char *fn , int ln )
{
   void *fred ;
   size_t nn = n*m ;

   fred = malloc_track(nn,fn,ln) ; if( fred == NULL ) return NULL ;
   memset( fred , 0 , nn ) ;
   return fred ;
}

/*-----------------------------------------------------------------*/
/*!  Tracking replacement for free().
-------------------------------------------------------------------*/

static void free_track( NI_mallitem *ip )
{
   char *cfred ;

   if( ip == NULL ) return ;
   cfred = (char *) ip->pmt ;
   if( cfred == NULL ) return ;

   probe_track(ip,NULL,0) ;  /* check for integrity before freeing */

   ni_mall_used = 1 ;
   free(cfred) ; ip->pmt = NULL ; return ;
}

/*-----------------------------------------------------------------*/
/*!  Return a status string about the situation.
     This is stored in a static buffer, so don't free it.
-------------------------------------------------------------------*/

static int use_tracking = 0 ;  /* is the tracking enabled? */

char * NI_malloc_status(void)
{
   static char buf[128] = "\0" ;
   int jj,kk , nptr=0 ; size_t nbyt=0 ;

   if( ! use_tracking ) return "not enabled" ;

   for( jj=0 ; jj < SLOTS ; jj++ ){
     for( kk=0 ; kk < nhtab[jj] ; kk++ ){
       if( htab[jj][kk].pmt != NULL ){
         probe_track( htab[jj]+kk , NULL,0 ) ; /* check for integrity */
         nptr++ ; nbyt += htab[jj][kk].psz ;
       }
     }
   }

   sprintf(buf,"chunks=%d bytes=%u",nptr,(UINT)nbyt) ;
   return buf ;
}

/*-----------------------------------------------------------------*/
/*!  Write a file with lots of info about the current status.
-------------------------------------------------------------------*/

void NI_malloc_dump(void)
{
   int ii,jj,kk ;
   char fname[32] , *str ;
   FILE *fp = NULL ;
   int nptr=0 ;
   int *ss , *jk ;

   if( ! use_tracking ) return ;

   /* find and open an output file */

   for( ii=1 ; ii < 1000 ; ii++ ){
     sprintf(fname,"NI_malldump.%03d",ii) ;
     if( NI_is_file(fname) ) continue ;
     fp = fopen( fname , "w" ) ;
     if( fp == NULL ){
       fprintf(stderr,"** Unable to open file %s for malloc table dump!\n",
               fname ) ;
       return ;
     }
     break ;
   }

   if( fp == NULL ){
     fprintf(stderr,"** Attempt to exceed 999 malloc table dump files!\n") ;
     return ;
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

   fprintf(fp, "MCW Malloc Table Dump:\n"
               "serial# size       source file          line# address    hash(j,k)\n"
               "------- ---------- -------------------- ----- ---------- ---------\n") ;

   for( ii=0 ; ii < nptr ; ii++ ){
     jj = jk[ii] / JBASE ;           /* retrieve jj and kk */
     kk = jk[ii] % JBASE ;
     if( htab[jj][kk].pmt != NULL ){
       fprintf(fp,"%7u %10u %-20.30s %5d %10p %5d %3d",
               htab[jj][kk].pss , (unsigned int)htab[jj][kk].psz ,
               htab[jj][kk].pfn , htab[jj][kk].pln , htab[jj][kk].pmt ,
               jj,kk ) ;
       fprintf(fp,"\n") ;
     }
     else
       fprintf(fp,"*** Error at ii=%d jj=%d kk=%d\n",ii,jj,kk) ;
   }

   free(ss) ; free(jk) ;

   /* and print out the summary line (to the file and screen) */

   str = NI_malloc_status() ;
   fprintf(fp,"----- Summary: %s\n",str) ;
   fclose(fp) ;

   fprintf(stderr,"** Malloc table dumped to file %s\n",fname) ;
   fprintf(stderr,"** Summary: %s\n",str) ;

   return ;
}

/*----------------------------------------------------------------*/
/*!  Turn on use of the tracking routines.
------------------------------------------------------------------*/

void NI_malloc_enable_tracking(void)       /* cannot be disabled */
{
   char *str ;

   if( use_userfunc ) return ;   /* 25 Mar 2003 */
   ni_mall_used = 1 ;

   if( use_tracking ) return ;   /* 05 Nov 2001 */

   str = getenv("AFNI_NO_MCW_MALLOC") ;
   if( str == NULL )
     str = getenv("NIML_MALLOC_DISABLE") ;

   use_tracking = 1 ;
   if( str!=NULL && ( *str=='y' || *str=='Y') ) use_tracking = 0 ;

   if( use_tracking && htab == NULL ){  /* initialize hash table */
     int jj ;
     htab  = (NI_mallitem **) malloc( SLOTS * sizeof(NI_mallitem *) ) ;
     nhtab = (int *)          malloc( SLOTS * sizeof(int) ) ;
     for( jj=0 ; jj < SLOTS ; jj++ ){
       htab[jj] = NULL ; nhtab[jj] = 0 ;
     }
   }

   return ;
}

/*---------------------------------------------------------------*/
/*! Lets the user check if the tracking routines are in use.
-----------------------------------------------------------------*/

int NI_malloc_tracking_enabled(void)
{
  return (use_tracking != 0) ;
}

/*--------------------------------------------------------------------------*/
/*! Allocate memory (actually uses calloc); calls exit() if it fails.
----------------------------------------------------------------------------*/

void * hidden_NI_malloc( size_t n , char *fnam , int lnum )
{
   void *p ;

        if( use_userfunc ){ p = user_malloc(n); if(p)memset(p,0,n); }
   else if( use_tracking )  p = calloc_track(1,n,fnam,lnum) ;
   else                     p = calloc(1,n) ;

   if( p == NULL ){
     fprintf(stderr,"** ERROR: NI_malloc() fails. Aauugghh!\n") ;
     NI_sleep(333); exit(1);
   }

#ifdef NIML_DEBUG
NI_dpr("hidden_NI_malloc: called from %s#%d\n",fnam,lnum) ;
#endif

   return p ;
}

/*--------------------------------------------------------------------------*/
/*! Reallocate memory; calls exit() if it fails.
----------------------------------------------------------------------------*/

void * hidden_NI_realloc( void *fred , size_t n , char *fnam , int lnum )
{
   NI_mallitem *ip ;
   void *q ;

   if( fred == NULL )
      return hidden_NI_malloc( n , fnam , lnum ) ;

   if( use_userfunc )
     q = user_realloc( fred , n ) ;
   else if( use_tracking && (ip=shift_tracker(fred)) != NULL )
     q = realloc_track( ip , n , fnam,lnum ) ;
   else
     q = realloc( fred , n ) ;

   if( q == NULL && n > 0 ){
      fprintf(stderr,"** ERROR: NI_realloc() fails. Ooooogg!\n");
      NI_sleep(333); exit(1);
   }

#ifdef NIML_DEBUG
NI_dpr("hidden_NI_realloc: called from %s#%d\n",fnam,lnum) ;
#endif

   return q ;
}

/*-----------------------------------------------------------------
    The actual replacment for free()
-------------------------------------------------------------------*/

void hidden_NI_free( void *fred , char *fnam , int lnum )
{
   NI_mallitem *ip ;

   if( fred == NULL ) return ;

   if( use_userfunc )                                          user_free(fred) ;
   else if( use_tracking && (ip=shift_tracker(fred)) != NULL ) free_track( ip ) ;
   else                                                        free( fred ) ;

#ifdef NIML_DEBUG
NI_dpr("hidden_NI_free: called from %s#%d\n",fnam,lnum) ;
#endif

}

/*------------------------------------------------*/
/*! Insertion_sort : sort an array of int + int.  */

static void isort_intint( int n , int * ar , int * iar )
{
   register int  j , p ;  /* array indices */
   register int   temp ;  /* a[j] holding place */
   register int  itemp ;
   register int * a = ar ;
   register int  * ia = iar ;

   if( n < 2 ) return ;

   for( j=1 ; j < n ; j++ ){

     if( a[j] < a[j-1] ){   /* out of order */
        p    = j ;
        temp = a[j] ; itemp = ia[j] ;

       do{
           a[p] =  a[p-1] ; /* at this point, a[p-1] > temp, so move it up */
          ia[p] = ia[p-1] ;
          p-- ;
        } while( p > 0 && temp < a[p-1] ) ;

        a[p] = temp ;       /* finally, put temp in its place */
       ia[p] = itemp ;
     }
   }
}

/*------------------------------------------------------*/
/*! Recursive part of quicksort (stack implementation). */

#define QS_STACK  1024  /* stack size */
#define QS_SWAPF(x,y) ( temp=(x),(x)=(y),(y)= temp)
#define QS_SWAPI(i,j) (itemp=(i),(i)=(j),(j)=itemp)

static void qsrec_intint( int n , int * ar , int * iar , int cutoff )
{
   register int i , j ;        /* scanning indices */
   register int temp , pivot ; /* holding places */
   register int  itemp , ipivot ;
   register int * a = ar ;
   register int  * ia = iar ;

   int left , right , mst , stack[QS_STACK] , nnew ;

   /* return if too short (insertion sort will clean up) */

   if( cutoff < 3 ) cutoff = 3 ;
   if( n < cutoff ) return ;

   /* initialize stack to start with whole array */

   stack[0] = 0   ;
   stack[1] = n-1 ;
   mst      = 2   ;

   /* loop while the stack is nonempty */

   while( mst > 0 ){
      right = stack[--mst] ;  /* work on subarray from left -> right */
      left  = stack[--mst] ;

      i = ( left + right ) / 2 ;           /* middle of subarray */

      /* sort the left, middle, and right a[]'s */

      if( a[left] > a[i]     ){ QS_SWAPF(a[left] ,a[i]    ); QS_SWAPI(ia[left] ,ia[i]    ); }
      if( a[left] > a[right] ){ QS_SWAPF(a[left] ,a[right]); QS_SWAPI(ia[left] ,ia[right]); }
      if( a[i] > a[right]    ){ QS_SWAPF(a[right],a[i]    ); QS_SWAPI(ia[right],ia[i]    ); }

      pivot  = a[i] ;                        /* a[i] is the median-of-3 pivot! */
      a[i]   = a[right] ;
      ipivot = ia[i] ;
      ia[i]  = ia[right] ;

      i = left ;                            /* initialize scanning */
      j = right ;

      /*----- partition:  move elements bigger than pivot up and elements
                          smaller than pivot down, scanning in from ends -----*/

      do{
        for( ; a[++i] < pivot ; ) ;  /* scan i up,   until a[i] >= pivot */
        for( ; a[--j] > pivot ; ) ;  /* scan j down, until a[j] <= pivot */

        if( j <= i ) break ;         /* if j meets i, quit */

        QS_SWAPF( a[i] , a[j] ) ; QS_SWAPI( ia[i] , ia[j] ) ;
      } while( 1 ) ;

      /*----- at this point, the array is partitioned -----*/

      a[right]  = a[i] ;           /*restore the pivot*/
      a[i]      = pivot ;
      ia[right] = ia[i] ;
      ia[i]     = ipivot ;

      /*----- push subarrays [left..i-1] and [i+1..right] onto stack, if big -----*/

      nnew = 0 ;
      if( (i-left)  > cutoff ){ stack[mst++] = left ; stack[mst++] = i-1   ; nnew++ ; }
      if( (right-i) > cutoff ){ stack[mst++] = i+1  ; stack[mst++] = right ; nnew++ ; }

      /* if just added two subarrays to stack, make sure shorter one comes first */

      if( nnew == 2 && stack[mst-3] - stack[mst-4] > stack[mst-1] - stack[mst-2] ){
         QS_SWAPI( stack[mst-4] , stack[mst-2] ) ;
         QS_SWAPI( stack[mst-3] , stack[mst-1] ) ;
      }

   }  /* end of while stack is non-empty */

}

/*-----------------------------------------------------------------*/
/*! Sort an array partially recursively, and partially insertion.  */

#ifndef QS_CUTOFF
#define QS_CUTOFF 10
#endif

void qsort_intint( int n , int * a , int * ia )
{
   qsrec_intint( n , a , ia , QS_CUTOFF ) ;
   isort_intint( n , a , ia ) ;
   return ;
}

/*-----------------------------------------------------------------*/
/*! 17 Dec 2003: In case a true NI_free() call gets thru somehow.  */
/*-----------------------------------------------------------------*/

#ifdef NI_free
#undef NI_free
#endif
void NI_free( void *p )
{
  hidden_NI_free( p , (char *)"Nada" , 0 ) ;
}

#endif  /* NIML_OLD_MALLOC or DONT_USE_MCW_MALLOC */
