/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#undef MAIN

#include "afni.h"

/*-----------------------------------------------------------------------------
   Register a function to be called from the "FIM+" menu.
     menu_name = label for menu
     nbrik     = number of values returned by user_func
     user_func = function to call for each data time series
     user_data = extra data to go to user_func

   void user_func( int n, float * ts, void * user_data, int nbrik, void * v )

     n         = length of time series
     ts[]      = time series data
     user_data = whatever you want (same as above)
     nbrik     = same as above
     val       = (float *) v = output array of values
     val[]     = user_func should fill val[0..nbrik-1] with values to save
                 (presumably computed from ts[])

   user_func should not destroy the data in ts[], since it will be re-used
   if more than one fimfunc is used at a time.

   Before the first call with time series data, user_func will be called like so:
     user_func( n,NULL,user_data,nbrik, FIMdata * fd ) ;
   where FIMdata is the type
     typedef struct {
        MRI_IMAGE * ref_ts , * ort_ts ;
        int nvox , ignore , polort ;
     } FIMdata ;
   and fd is used to pass in the parameters set by the user:
      fd->ref_ts = float image of reference ("ideal") time series
                   fd->ref_ts->nx = length of data
                   fd->ref_ts->ny = number of ref time series
                   MRI_FLOAT_PTR(fd->ref_ts) = pointer to ref timeseries data
      fd->ort_ts = similarly for the orts (but this may be NULL)
      fd->nvox   = number of voxels that will be passed in
                 = number of "normal" calls to user_data
      fd->ignore = ignore setting (but ignored data is included in ts[])
      fd->polort = polort setting (but no detrending is done to ts[])
   N.B.: FIMdata is typedef-ed in afni.h.

   After the last (nvox-th) time series is processed, user_func will be called
   one last time, with the form
     user_func( -k,NULL,user_data,nbrik, dset ) ;
   where dset is a pointer to the dataset (THD_3dim_dataset *) that has
   just been constructed.  The value k (negative of the first argument) will
   be the index of the first sub-brick in the dataset - it contains the data
   from val[0].  If desired, you can use this final call to set sub-brick
   parameters for sub-bricks k through k+nbrik-1 (for val[0] .. val[nbrik-1]).

   Both special calls have the second argument NULL, which is how user_func
   can distinguish them from a normal call with timeseries data in ts[].

   The initialization call will have the first argument be the number of
   points in the timeseries to come ('n') - this will be positive.  The
   final call will have the first argument be the negative of the sub-brick
   index of the first sub-brick created by this routine.  This value will
   be non-positive (will be 0 or less), which is how the initialization
   and final calls can be distinguished.  In each of these cases, the
   last argument must be properly cast to the correct pointer type before
   being used.

   A sample fimfunc for the Spearman rank correlation, spearman_fimfunc(),
   is given at the end of this file.
--------------------------------------------------------------------------------*/

void AFNI_register_fimfunc( char * menu_name , int nbrik ,
                            generic_func * user_func , void * user_data )
{
   MCW_function_list * rlist = &(GLOBAL_library.registered_fim) ;
   int num = rlist->num ;

ENTRY("AFNI_register_fimfunc") ;

   if( menu_name == NULL || menu_name[0] == '\0' ||
       nbrik <= 0        || user_func == NULL      ) EXRETURN ; /* bad inputs! */

   if( num == sizeof(int) ) EXRETURN ;  /* too many already! */

   if( num == 0 ){
     rlist->flags=NULL; rlist->labels=NULL; rlist->funcs=NULL;
     rlist->func_data=NULL; rlist->func_code=NULL; rlist->func_init=NULL;
   }

   rlist->flags = (int *) XtRealloc( (char *)rlist->flags, sizeof(int)*(num+1) ) ;

   rlist->labels = (char **) XtRealloc( (char *)rlist->labels ,
                                        sizeof(char *)*(num+1) ) ;

   rlist->funcs = (generic_func **) XtRealloc( (char *)rlist->funcs ,
                                               sizeof(generic_func *)*(num+1) ) ;

   rlist->func_data = (void **) XtRealloc( (char *)rlist->func_data ,
                                           sizeof(void *)*(num+1) ) ;

   rlist->func_code = (int *) XtRealloc( (char *)rlist->func_code, sizeof(int)*(num+1) ) ;

   rlist->func_init = (generic_func **) XtRealloc( (char *)rlist->func_init ,
                                                   sizeof(generic_func *)*(num+1) ) ;

   rlist->flags[num]     = nbrik ;
   rlist->labels[num]    = XtNewString(menu_name) ;
   rlist->funcs[num]     = user_func ;
   rlist->func_data[num] = user_data ;
   rlist->func_code[num] = FUNC_FIM  ;
   rlist->func_init[num] = NULL ;

   rlist->num = num+1 ;
   EXRETURN ;
}

/*******************************************************************************
  Code for the Spearman rank and quandrant correlation fimfuncs
********************************************************************************/

/*------------------------------------------------------------------------------
   Rank-order a float array, with ties getting the average rank.
   The output overwrites the input.
--------------------------------------------------------------------------------*/

static void rank_order_float( int n , float * a )
{
   register int ii , ns , n1 , ib ;
   static int    nb = 0 ;
   static int *   b = NULL ;  /* workspaces */
   static float * c = NULL ;
   float cs ;

   /*- handle special cases -*/

   if( a == NULL ){
      if( b != NULL ){ free(b); free(c); b=NULL ; c=NULL; nb=0; }  /* free workspaces */
      return ;
   }

   if( n < 1 ) return ;                     /* meaningless input */
   if( n == 1 ){ a[0] = 0.0 ; return ; }    /* only one point!? */

   /*- make workspaces, if needed -*/

   if( nb < n ){
      if( b != NULL ){ free(b); free(c); }
      b  = (int   *) malloc(sizeof(int  )*n) ;
      c  = (float *) malloc(sizeof(float)*n) ;
      nb = n ;
   }

   for( ii=0 ; ii < n ; ii++ ) c[ii] = b[ii] = ii ;

   /*- sort input, carrying b along -*/

   qsort_floatint( n , a , b ) ;  /* see cs_sort_fi.c */

   /* compute ranks into c[] */

   n1 = n-1 ;
   for( ii=0 ; ii < n1 ; ii++ ){
      if( a[ii] == a[ii+1] ){                  /* handle ties */
         cs = 2*ii+1 ; ns = 2 ; ib=ii ; ii++ ;
         while( ii < n1 && a[ii] == a[ii+1] ){ ii++ ; ns++ ; cs += ii ; }
         for( cs/=ns ; ib <= ii ; ib++ ) c[ib] = cs ;
      }
   }

   for( ii=0 ; ii < n ; ii++ ) a[b[ii]] = c[ii] ;

   return ;
}

/*---------------------------------------------------------------------------
   Rank orders a[], subtracts the mean rank, and returns the sum-of-squares
-----------------------------------------------------------------------------*/

static float spearman_rank_prepare( int n , float * a )
{
   register int ii ;
   register float rb , rs ;

   rank_order_float( n , a ) ;

   rb = 0.5*(n-1) ; rs=0.0 ;
   for( ii=0 ; ii < n ; ii++ ){
      a[ii] -= rb ;
      rs    += a[ii]*a[ii] ;
   }

   return rs ;
}

static float quadrant_corr_prepare( int n , float * a )
{
   register int ii ;
   register float rb , rs ;

   rank_order_float( n , a ) ;

   rb = 0.5*(n-1) ; rs=0.0 ;
   for( ii=0 ; ii < n ; ii++ ){
      a[ii] = (a[ii] > rb) ? 1.0
                           : (a[ii] < rb) ? -1.0 : 0.0 ;
      rs    += a[ii]*a[ii] ;
   }

   return rs ;
}

/*-----------------------------------------------------------------------------
    To correlate x[] with r[], do
      rv = spearman_rank_prepare(n,r) ;
    then
      corr = spearman_rank_corr(n,x,rv,r) ;
    Note that these 2 routines are destructive (r and x are replaced by ranks)
-------------------------------------------------------------------------------*/

static float spearman_rank_corr( int n , float * x , float rv , float * r )
{
   register int ii ;
   register float ss ; float xv ;

   xv = spearman_rank_prepare( n , x ) ; if( xv <= 0.0 ) return 0.0 ;

   for( ii=0,ss=0.0 ; ii < n ; ii++ ) ss += x[ii] * r[ii] ;

   return ( ss/sqrt(rv*xv) ) ;
}

static float spearman_rank_manycorr( int n , float * x ,
                                     int nr, float * rv, float ** r )
{
   register int ii ;
   register float ss ;
   int jj ;
   float bb , xv ;

   if( nr == 1 ) return spearman_rank_corr( n,x,rv[0],r[0] ) ;

   xv = spearman_rank_prepare( n , x ) ; if( xv <= 0.0 ) return 0.0 ;

   for( jj=0,bb=0.0 ; jj < nr ; jj++ ){
      for( ii=0,ss=0.0 ; ii < n ; ii++ ) ss += x[ii] * r[jj][ii] ;
      ss /= sqrt(rv[jj]*xv) ;
      if( fabs(ss) > fabs(bb) ) bb = ss ;
   }

   return bb ;
}

static float quadrant_corr( int n , float * x , float rv , float * r )
{
   register int ii ;
   register float ss ; float xv ;

   xv = quadrant_corr_prepare( n , x ) ; if( xv <= 0.0 ) return 0.0 ;

   for( ii=0,ss=0.0 ; ii < n ; ii++ ) ss += x[ii] * r[ii] ;

   return ( ss/sqrt(rv*xv) ) ;
}

static float quadrant_manycorr( int n , float * x ,
                                int nr, float * rv, float ** r )
{
   register int ii ;
   register float ss ;
   int jj ;
   float bb , xv ;

   if( nr == 1 ) return quadrant_corr( n,x,rv[0],r[0] ) ;

   xv = quadrant_corr_prepare( n , x ) ; if( xv <= 0.0 ) return 0.0 ;

   for( jj=0,bb=0.0 ; jj < nr ; jj++ ){
      for( ii=0,ss=0.0 ; ii < n ; ii++ ) ss += x[ii] * r[jj][ii] ;
      ss /= sqrt(rv[jj]*xv) ;
      if( fabs(ss) > fabs(bb) ) bb = ss ;
   }

   return bb ;
}

/*--------------------------------------------------------------------------
  A sample fimfunc to compute the Spearman rank correlation
----------------------------------------------------------------------------*/

void spearman_fimfunc( int n, float * ts, void * ud, int nb, void * val )
{
   static float * tsc=NULL , * refc=NULL , * refv=NULL , ** rr ;
   static int   * keep=NULL ;
   static int    ntsc , nref , nkeep , polort,ignore , ncall;

   int ii , kk ;
   float * v ;

   /*--- handle special cases ---*/

   if( ts == NULL ){

      /*--- the initialization call ---*/

      if( n > 0 ){

         FIMdata * fd = (FIMdata *) val ;

         polort = fd->polort ;  /* not used here */
         ignore = fd->ignore ;
         ncall  = 0 ;           /* how many times we have been called */

         /* make workspace for copy of ts data when it arrives */

         ntsc = n ;
         if( tsc != NULL ) free(tsc) ;
         tsc = (float *) malloc(sizeof(float)*ntsc) ;

         /* make copy of ref data */

         nref = fd->ref_ts->ny ;
         if( refc != NULL ) free(refc) ;
         if( refv != NULL ) free(refv) ;
         if( keep != NULL ) free(keep) ;
         if( rr   != NULL ) free(rr) ;
         refc = (float *) malloc(sizeof(float)*ntsc*nref) ;  /* copy of ref   */
         refv = (float *) malloc(sizeof(float)*nref) ;      /* rank variances */
         keep = (int *)   malloc(sizeof(int)*ntsc) ;       /* keeper indices  */
         rr   = (float **)malloc(sizeof(float *)*nref) ;  /* convenience ptrs */

         for( kk=0 ; kk < nref ; kk++ ){
            rr[kk] = refc+kk*ntsc ;                       /* compute ptr */
            memcpy( rr[kk] ,                              /* copy data  */
                    MRI_FLOAT_PTR(fd->ref_ts) + kk*fd->ref_ts->nx ,
                    sizeof(float)*ntsc  ) ;
         }

         /* mark those places we will keep (where ref is OK) */

         for( nkeep=0,ii=ignore ; ii < ntsc ; ii++ ){ /* for each time point */
            for( kk=0 ; kk < nref ; kk++ )            /* check each ref     */
               if( rr[kk][ii] >= WAY_BIG ) break ;

            if( kk == nref ) keep[nkeep++] = ii ;     /* mark if all are OK */
         }

         /* compress ref, eliminating non-keepers */

         if( nkeep < ntsc ){
            for( ii=0 ; ii < nkeep ; ii++ ){
               for( kk=0 ; kk < nref ; kk++ )
                  rr[kk][ii] = rr[kk][keep[ii]] ;  /* copy backwards */
            }
         }

         /* prepare each ref vector for rank processing */

         for( kk=0 ; kk < nref ; kk++ )
            refv[kk] = spearman_rank_prepare( nkeep , rr[kk] ) ;

#if 0
fprintf(stderr,"spearman_fimfunc: initialize ntsc=%d nkeep=%d nref=%d\n",
        ntsc,nkeep,nref);
#endif

         return ;

      /*--- the ending call ---*/

      } else {
         THD_3dim_dataset * dset = (THD_3dim_dataset *) val ;
         int kb = -n ;

         free(tsc)  ; tsc  = NULL ;  /* free workspaces */
         free(refc) ; refc = NULL ;
         free(refv) ; refv = NULL ;
         free(keep) ; keep = NULL ;
         free(rr)   ; rr   = NULL ;
         rank_order_float(0,NULL) ;

         /* edit sub-brick statistical parameters and name */

         EDIT_BRICK_TO_FICO( dset , kb , nkeep , (nref==1)?1:2 , 1 ) ;
         EDIT_BRICK_LABEL( dset , kb , "Spearman CC" ) ;

#if 0
fprintf(stderr,"spearman_fimfunc: finalize with kb=%d\n",kb);
#endif

         return ;
      }
   }

   /*--- the normal case [with data] ---*/

   ncall++ ;
#if 0
if(ncall%1000==0) fprintf(stderr,"spearman_fimfunc: ncall=%d\n",ncall);
#endif

   /* copy input timeseries data (since we will mangle it) */

   if( nkeep < ntsc )
      for( ii=0 ; ii < nkeep ; ii++ ) tsc[ii] = ts[keep[ii]]; /* the hard way */
   else
      memcpy(tsc,ts,sizeof(float)*ntsc) ;                     /* the easy way */

   /* compute our single result */

   v    = (float *) val ;
   v[0] = spearman_rank_manycorr( nkeep , tsc , nref , refv , rr ) ;

   return ;
}

/*--------------------------------------------------------------------------
  A sample fimfunc to compute the quadrant correlation
----------------------------------------------------------------------------*/

void quadrant_fimfunc( int n, float * ts, void * ud, int nb, void * val )
{
   static float * tsc=NULL , * refc=NULL , * refv=NULL , ** rr ;
   static int   * keep=NULL ;
   static int    ntsc , nref , nkeep , polort,ignore , ncall;

   int ii , kk ;
   float * v ;

   /*--- handle special cases ---*/

   if( ts == NULL ){

      /*--- the initialization call ---*/

      if( n > 0 ){

         FIMdata * fd = (FIMdata *) val ;

         polort = fd->polort ;  /* not used here */
         ignore = fd->ignore ;
         ncall  = 0 ;           /* how many times we have been called */

         /* make workspace for copy of ts data when it arrives */

         ntsc = n ;
         if( tsc != NULL ) free(tsc) ;
         tsc = (float *) malloc(sizeof(float)*ntsc) ;

         /* make copy of ref data */

         nref = fd->ref_ts->ny ;
         if( refc != NULL ) free(refc) ;
         if( refv != NULL ) free(refv) ;
         if( keep != NULL ) free(keep) ;
         if( rr   != NULL ) free(rr) ;
         refc = (float *) malloc(sizeof(float)*ntsc*nref) ;  /* copy of ref   */
         refv = (float *) malloc(sizeof(float)*nref) ;      /* rank variances */
         keep = (int *)   malloc(sizeof(int)*ntsc) ;       /* keeper indices  */
         rr   = (float **)malloc(sizeof(float *)*nref) ;  /* convenience ptrs */

         for( kk=0 ; kk < nref ; kk++ ){
            rr[kk] = refc+kk*ntsc ;                       /* compute ptr */
            memcpy( rr[kk] ,                              /* copy data  */
                    MRI_FLOAT_PTR(fd->ref_ts) + kk*fd->ref_ts->nx ,
                    sizeof(float)*ntsc  ) ;
         }

         /* mark those places we will keep (where ref is OK) */

         for( nkeep=0,ii=ignore ; ii < ntsc ; ii++ ){ /* for each time point */
            for( kk=0 ; kk < nref ; kk++ )            /* check each ref     */
               if( rr[kk][ii] >= WAY_BIG ) break ;

            if( kk == nref ) keep[nkeep++] = ii ;     /* mark if all are OK */
         }

         /* compress ref, eliminating non-keepers */

         if( nkeep < ntsc ){
            for( ii=0 ; ii < nkeep ; ii++ ){
               for( kk=0 ; kk < nref ; kk++ )
                  rr[kk][ii] = rr[kk][keep[ii]] ;  /* copy backwards */
            }
         }

         /* prepare each ref vector for rank processing */

         for( kk=0 ; kk < nref ; kk++ )
            refv[kk] = quadrant_corr_prepare( nkeep , rr[kk] ) ;

#if 0
fprintf(stderr,"quadrant_fimfunc: initialize ntsc=%d nkeep=%d nref=%d\n",
        ntsc,nkeep,nref);
#endif

         return ;

      /*--- the ending call ---*/

      } else {
         THD_3dim_dataset * dset = (THD_3dim_dataset *) val ;
         int kb = -n ;

         free(tsc)  ; tsc  = NULL ;  /* free workspaces */
         free(refc) ; refc = NULL ;
         free(refv) ; refv = NULL ;
         free(keep) ; keep = NULL ;
         free(rr)   ; rr   = NULL ;
         rank_order_float(0,NULL) ;

         /* edit sub-brick statistical parameters and name */

         EDIT_BRICK_TO_FICO( dset , kb , nkeep , (nref==1)?1:2 , 1 ) ;
         EDIT_BRICK_LABEL( dset , kb , "Quadrant CC" ) ;

#if 0
fprintf(stderr,"quadrant_fimfunc: finalize with kb=%d\n",kb);
#endif

         return ;
      }
   }

   /*--- the normal case [with data] ---*/

   ncall++ ;
#if 0
if(ncall%1000==0) fprintf(stderr,"quadrant_fimfunc: ncall=%d\n",ncall);
#endif

   /* copy input timeseries data (since we will mangle it) */

   if( nkeep < ntsc )
      for( ii=0 ; ii < nkeep ; ii++ ) tsc[ii] = ts[keep[ii]]; /* the hard way */
   else
      memcpy(tsc,ts,sizeof(float)*ntsc) ;                     /* the easy way */

   /* compute our single result */

   v    = (float *) val ;
   v[0] = quadrant_manycorr( nkeep , tsc , nref , refv , rr ) ;

   return ;
}
