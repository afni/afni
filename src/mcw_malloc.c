#include "mcw_malloc.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#define INC_MT 16384
static int amt = 0 , nmt = 0 ;
static void  ** pmt = NULL ;
static size_t * psz = NULL ;
static char  ** pfn = NULL ;
static int    * pln = NULL ;

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

/*-----------------------------------------------------------------*/

static int ptr_tracker( void * fred )
{
   int ii ;
   if( fred == NULL ) return -1 ;
   for( ii=0 ; ii < nmt ; ii++ ) if( fred == pmt[ii] ) return ii ;
   return -1 ;
}

#if 0
  static int shift_tracker( char * fred )
  {
     if( fred == NULL ) return -1 ;
     return ptr_tracker(fred-NEXTRA) ;
  }
#else
#  define shift_tracker(fff)  ptr_tracker( ((char *)(fff)) - NEXTRA )
#endif

/*-----------------------------------------------------------------*/

static int nul_tracker(void)
{
   int ii ;
#if 0
   for( ii=0 ; ii < nmt ; ii++ ) if( pmt[ii] == NULL ) return ii ;
#else
   for( ii=nmt-1 ; ii >= 0 ; ii-- ) if( pmt[ii] == NULL ) return ii ;
#endif
   return -1 ;
}

/*-----------------------------------------------------------------*/

static void add_tracker( void * fred , size_t n , char * fn , int ln )
{
   int ii ;
   if( nmt == amt ){
      amt += INC_MT ;
      pmt  = (void **)  realloc( pmt , sizeof(void *)*amt ) ;
      psz  = (size_t *) realloc( psz , sizeof(size_t)*amt ) ;
      pfn  = (char ** ) realloc( pfn , sizeof(char *)*amt ) ;
      pln  = (int *   ) realloc( pln , sizeof(int   )*amt ) ;
      for( ii=nmt ; ii < amt ; ii++ ) pmt[ii] = NULL ;
   }
   ii = nul_tracker() ;
   if( ii < 0 ){ psz[nmt]=n; pfn[nmt]=fn; pln[nmt]=ln; pmt[nmt++]=fred; }
   else        { psz[ii] =n; pfn[ii] =fn; pln[ii] =ln; pmt[ii]   =fred; }
   return ;
}

/*-----------------------------------------------------------------*/

static void * malloc_track( size_t n , char * fn , int ln )
{
   char * fred ;
   size_t nn = n + 2*NEXTRA ;
   int ii ;

   fred = malloc(nn) ; if( fred == NULL ) return NULL ;
   for( ii=0 ; ii < NEXTRA ; ii++ )
      fred[ii] = fred[n+NEXTRA+ii] = MAGIC ;

   add_tracker(fred,n,fn,ln) ;
   return (void *)(fred+NEXTRA) ;
}

/*-----------------------------------------------------------------*/

static void probe_track( int ip )
{
   int ii ;
   size_t n ;
   char * fred ;

   if( ip < 0 || ip >= nmt ) return ;  /* error */
   fred = (char *) pmt[ip] ; if( fred == NULL ) return ;
   n = psz[ip] ;

   for( ii=0 ; ii < NEXTRA ; ii++ )
      if( fred[ii] != MAGIC ){
         fprintf(stderr,"*** malloc pre-corruption!  "
                        "chunk=%d size=%d source=%s line#=%d\n",
                        ii,psz[ii],pfn[ii],pln[ii] ) ;
         break ;
      }

   for( ii=0 ; ii < NEXTRA ; ii++ )
      if( fred[n+NEXTRA+ii] != MAGIC ){
         fprintf(stderr,"*** malloc post-corruption!  "
                        "chunk=%d size=%d source=%s line#=%d\n",
                        ii,psz[ii],pfn[ii],pln[ii] ) ;
         break ;
      }

   return ;
}

/*-----------------------------------------------------------------*/

static void * realloc_track( int ip , size_t n , char * fn , int ln )
{
   char * nfred ;
   char * cfred = (char *) pmt[ip] ;
   size_t nn = n + 2*NEXTRA ;
   int ii ;

   probe_track(ip) ;  /* check for integrity before reallocation */

   nfred = realloc( cfred , nn ) ; if( nfred == NULL ) return NULL ;

   for( ii=0 ; ii < NEXTRA ; ii++ )
      nfred[ii] = nfred[n+NEXTRA+ii] = MAGIC ;

   psz[ip] = n ; pmt[ip] = nfred ;
   pfn[ip] = fn; pln[ip] = ln    ;

   return (void *)(nfred+NEXTRA) ;
}

/*-----------------------------------------------------------------*/

static void * calloc_track( size_t n , size_t m , char * fn , int ln )
{
   void * fred ;
   size_t nn = n*m ;

   fred = malloc_track(nn,fn,ln) ; if( fred == NULL ) return NULL ;
   memset( fred , 0 , nn ) ;
   return fred ;
}

/*-----------------------------------------------------------------*/

static void free_track( int ip )
{
   char * cfred = (char *) pmt[ip] ;

   if( cfred == NULL ) return ;

   probe_track(ip) ;  /* check for integrity before freeing */

   pmt[ip] = NULL ;
   if( ip == nmt-1 ){
      for( ; ip > 0 && pmt[ip] == NULL ; ip-- ) ; /* nada */
      nmt = ip+1 ;
   }

   free(cfred) ; return ;
}

/*-----------------------------------------------------------------*/

static int use_tracking = 0 ;

char * mcw_malloc_status(void)
{
   static char buf[128] = "\0" ;
   int ii , nptr=0 ; size_t nbyt=0 ;

   if( ! use_tracking ) return NULL ;

   for( ii=0 ; ii < nmt ; ii++ )
      if( pmt[ii] != NULL ){
         probe_track(ii) ;           /* check for integrity */
         nptr++ ; nbyt += psz[ii] ;
      }

   sprintf(buf,"chunks=%d bytes=%u",nptr,(unsigned int)nbyt) ;
   return buf ;
}

void mcw_malloc_dump(void)
{
   int ii ;
   char fname[32] ;
   FILE * fp = NULL ;

   if( ! use_tracking ) return ;

   for( ii=1 ; ii < 1000 ; ii++ ){
      sprintf(fname,"malldump.%03d",ii) ;
      if( THD_is_file(fname) ) continue ;
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

   fprintf(fp , "MCW Malloc Table Dump:\n"
                "chunk size       source file          line#\n"
                "----- ---------- -------------------- -----\n") ;

   for( ii=0 ; ii < nmt ; ii++ ){
      if( pmt[ii] != NULL )
         fprintf(fp,"%5d %10d %-20.30s %5d\n",ii,psz[ii],pfn[ii],pln[ii]) ;
      else
         fprintf(fp,"%5d FREE\n",ii) ;
   }

   fprintf(fp,"----- Summary: %s\n",mcw_malloc_status()) ;
   fclose(fp) ;

   fprintf(stderr,"** Malloc table dumped to file %s\n",fname) ;
   return ;
}

/*-----------------------------------------------------------------*/

void enable_mcw_malloc()       /* cannot be disabled */
{
   char * str = getenv("AFNI_NO_MCW_MALLOC") ; 
   if( str == NULL ) use_tracking = 1 ;
   return ;
}

int mcw_malloc_enabled(){ return (use_tracking != 0) ; }

/*-----------------------------------------------------------------*/

void * mcw_malloc( size_t n , char * fnam , int lnum )
{
   if( use_tracking ) return malloc_track(n,fnam,lnum) ;
   else               return malloc(n) ;
}

/*-----------------------------------------------------------------*/

void * mcw_realloc( void * fred , size_t n , char * fnam , int lnum )
{
   int ip ;

   if( fred == NULL )
      return mcw_malloc( n , fnam , lnum ) ;

   if( use_tracking && (ip=shift_tracker(fred)) >= 0 )
      return realloc_track( ip , n , fnam,lnum ) ;
   else
      return realloc( fred , n ) ;
}

/*-----------------------------------------------------------------*/

void * mcw_calloc( size_t n , size_t m , char * fnam , int lnum )
{
   if( use_tracking ) return calloc_track( n , m , fnam,lnum ) ;
   else               return calloc( n , m ) ;
}

/*-----------------------------------------------------------------*/

void mcw_free( void * fred )
{
   int ip ;

   if( fred == NULL ) return ;

   if( use_tracking && (ip=shift_tracker(fred)) >= 0 ) free_track( ip ) ;
   else                                                free( fred ) ;
}

/*-----------------------------------------------------------------*/

char * mcw_XtMalloc( Cardinal n , char * fnam , int lnum )
{
   if( use_tracking ) return (char *) malloc_track(n,fnam,lnum) ;
   else               return XtMalloc(n) ;
}

/*-----------------------------------------------------------------*/

char * mcw_XtRealloc( char *p, Cardinal n , char * fnam , int lnum )
{
   int ip ;

   if( p == NULL )
      return mcw_XtMalloc( n , fnam , lnum ) ;

   if( use_tracking && (ip=shift_tracker(p)) >= 0 )
      return realloc_track( ip , n , fnam,lnum ) ;
   else
      return XtRealloc( p , n ) ;
}

/*-----------------------------------------------------------------*/

void mcw_XtFree( char *p )
{
   int ip ;

   if( p == NULL ) return ;

   if( use_tracking && (ip=shift_tracker(p)) >= 0 ) free_track(ip) ;
   else                                             XtFree(p) ;
}

/*-----------------------------------------------------------------*/

char * mcw_XtCalloc( Cardinal n , Cardinal m , char * fnam , int lnum )
{
   if( use_tracking ) return (char *) calloc_track( n , m , fnam,lnum ) ;
   else               return XtCalloc( n , m ) ;
}
