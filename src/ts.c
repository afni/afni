/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "ts.h"

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** free a time_series structure ***/

void RWC_free_time_series(ts)
     time_series *ts;
{
   if( ts != NULL ){
      if( ts->fname != NULL ) free( ts->fname ) ;
      if( ts->ts    != NULL ) free( ts->ts ) ;
      free( ts ) ;
   }
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** make a blank time_series ***/

time_series * RWC_blank_time_series( npt )
  int npt ;
{
   int ii ;
   time_series *vec ;

   if( npt <= 0 ) return NULL ;

   vec = (time_series *) malloc( sizeof(time_series) ) ;
   if( vec == NULL ) MALLOC_ERR("new time_series") ;

   vec->ts = (float *) malloc( sizeof(float) * npt ) ;
   if( vec->ts == NULL ) MALLOC_ERR("new time_series vector") ;

   vec->fname = NULL ;   /* user must supply name */
   vec->len   = npt ;

   for( ii=0 ; ii < npt ; ii++ ) vec->ts[ii] = 0.0 ;
   return vec ;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** median filter a time series ***/

void RWC_medfilt_time_series( vec )
   time_series * vec ;
{
   int ii , npt ;
   float aa , bb , cc , mtemp ;
   float * id ;

#define MEDIAN(a,b,c) ( mtemp = 4*((a)<(b)) + 2*((a)<(c)) + ((b)<(c)) , \
                        (mtemp==3||mtemp==4) ? (a) :                    \
                        (mtemp==7||mtemp==0) ? (b) : (c) )

   if( vec == NULL || (npt = vec->len) < 3 ) return ;

   id = vec->ts ;

   bb = id[0] ; cc = id[1] ;
   for( ii=1 ; ii < npt-1 ; ii++ ){
      aa = bb ; bb = cc ; cc = id[ii+1] ;
      id[ii] = MEDIAN(aa,bb,cc) ;
   }

   return ;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** read a time_series structure from a file ***/

#define INC_TSARSIZE 256

time_series * RWC_read_time_series(fname)
     char *fname;
{
   int ii , used_tsar , alloc_tsar ;
   float ftemp ;
   time_series *vec ;
   FILE *fts ;

   vec = (time_series *) malloc( sizeof(time_series) ) ;
   if( vec == NULL ) MALLOC_ERR("new time_series") ;

   fts = fopen( fname , "r" ) ;
   if( fts == NULL ){
      fprintf( stderr , "cannot open time series file %s\n" , fname ) ;
      free( vec ) ;
      return NULL ;
   }

   ii = strlen( fname ) + 1 ;
   vec->fname = (char *) malloc( sizeof(char) * ii ) ;
   if( vec->fname == NULL ) MALLOC_ERR("new time_series fname") ;
   strcpy( vec->fname , fname ) ;

   used_tsar  = 0 ;
   alloc_tsar = INC_TSARSIZE ;
   vec->ts    = (float *) malloc( sizeof(float) * alloc_tsar ) ;
   if( vec->ts == NULL ) MALLOC_ERR("new time_series initial ts") ;

   while( 1 ){

      ii = fscanf( fts , "%f" , &ftemp ) ;
      if( ii != 1 ) break ;

      if( used_tsar == alloc_tsar ){
         alloc_tsar += INC_TSARSIZE ;
         vec->ts     = (float *)realloc( vec->ts,sizeof(float)*alloc_tsar );
         if( vec->ts == NULL ) MALLOC_ERR("expanding new time_series ts") ;
      }

      vec->ts[used_tsar++] = ftemp ;

   }

   if( used_tsar <= 0 ){
      fprintf( stderr , "cannot read data from time_series %s\n" , fname ) ;
      free( vec->fname ) ;
      free( vec->ts ) ;
      free( vec ) ;
      return NULL ;
   }

   vec->len = used_tsar ;
   vec->ts  = (float *) realloc( vec->ts , sizeof(float) * used_tsar ) ;
   if( vec->ts == NULL ) MALLOC_ERR("shrinking new time_series ts") ;

   return vec ;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** compute the L1 norm of a time_series ***/

float RWC_norm_ts(nn, vec)
     int nn;
     time_series *vec;
{
   int ii , itop = MIN(nn,vec->len) ;
   float *var = vec->ts ;
   float vsum = 0.0 ;

   for( ii=0 ; ii < itop ; ii++ ) vsum += fabs(var[ii]) ;

   return vsum ;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** compute the max of a time_series ***/

float RWC_max_ts(nn, vec)
     int nn;
     time_series *vec;
{
   int ii , itop = MIN(nn,vec->len) ;
   float *var = vec->ts ;
   float vsum ;

   vsum = var[0] ;
   for( ii=1 ; ii < itop ; ii++ ) vsum = MAX(vsum,var[ii]) ;
   return vsum ;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** compute the min of a time_series ***/

float RWC_min_ts(nn, vec)
     int nn;
     time_series *vec;
{
   int ii , itop = MIN(nn,vec->len) ;
   float *var = vec->ts ;
   float vsum ;

   vsum = var[0] ;
   for( ii=1 ; ii < itop ; ii++ ) vsum = MIN(vsum,var[ii]) ;
   return vsum ;
}
