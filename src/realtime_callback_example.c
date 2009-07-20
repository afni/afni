/**---------------------------------------------------------------------------
  Setup for realtime callback, which will be invoked every time
  AFNI is 'told' about new datasets (i.e., per 'update' # of TRs):

  + set global afni variable
      GLOBAL_library.realtime_callback = your_function_name
    will be invoked as
      your_function_name(void *junk) ;  [[junk will be NULL]]

  + to find out what's up, query the global afni variable
      RT_status *GLOBAL_library.realtime_status
    described below; each dataset should have the same number of volumes
    (cf. the trivial test callback at the bottom to grok what's happening)

  + status will be RT_STARTUP at first call, and there will be data
    in the datasets at that time
  + status will be RT_CONTINUE at subsequent calls (with new data)
  + status will be RT_FINISHED when this acquisition is over; the next
    time the realtime callback is invoked, status should be RT_STARTUP
    and there should be an entirely new set of dataset pointers to play with
---------------------------------------------------------------------------**/

typedef struct {
   int numchan ;              /* number of channels from scanner  */
   int status ;               /* one of the RT_ codes below      */
   int numdset ;              /* total number of datasets below */
   THD_3dim_dataset **dset ;  /* array of pointers to datasets */
} RT_status ;

#define RT_STARTUP    1  /* status codes [01 Jun 2009] */
#define RT_CONTINUE   2
#define RT_FINISHED   3

/**---------------------------------------------------------------------------
  Dataset order in RT_status struct:
    +  0..numchan-1 = channels from scanner
    +  if channel merger is on, then merged channel is next
    +  if registration is on, then registered dataset is last
    +  so normally, dset[numdset-1] is the one you want for SVM-ization
---------------------------------------------------------------------------**/

/*-------------------------------------------------------------------------*/
/* Test realtime_callback function [01 Jun 2009] */

void RT_test_callback(void *junk)
{
   RT_status *rts = GLOBAL_library.realtime_status ;
   int cc , nval,nbr ;

   if( rts == NULL ){ ERROR_message("bad call to RT_test_callback"); return; }

   INFO_message("RT_test_callback: numchan=%d status=%d numdset=%d",
                rts->numchan , rts->status , rts->numdset ) ;

   for( cc=0 ; cc < rts->numdset ; cc++ ){     /* print out some dataset info */
     if( !ISVALID_DSET(rts->dset[cc]) ){
       ININFO_message(" dset[%d] invalid!",cc) ;       /* should never happen */
     } else {
       nval = DSET_NVALS(rts->dset[cc]) ;                 /* number of bricks */
       nbr  = THD_count_databricks(rts->dset[cc]->dblk) ; /* number with data */
       ININFO_message(" dset[%d] '%s': nvals=%d  nbr=%d",
                      cc , DSET_HEADNAME(rts->dset[cc]) , nval,nbr ) ;
     }
   }
   return ;
}
