#include "afni.h"

/**************************************/
/** global data for NIML connections **/
/**************************************/

/*---------------------------------------*/
/*! Number of streams on which to listen */
#define NUM_NIML   1

/*--------------------------------------*/
/*! Array of streams on which to listen */

static NI_stream ns_listen[NUM_NIML] ;

/*------------------------*/
/*! Array of stream names */

static char ns_name[NUM_NIML][64] ;

/*-------------------------------------*/
/*! The SUMA stream index in ns_listen */

#define NS_SUMA 0

/*-------------------------*/

#ifndef SUMA_TCP_PORT
#define SUMA_TCP_PORT 53211
#endif

/*-----------------------------------------------*/
/*! Flag to tell if NIML things are initialized. */

static int started = 0 ;

/*---------------------*/
/* Internal prototypes */

static void    AFNI_niml_exit( void ) ;
static Boolean AFNI_niml_workproc( XtPointer ) ;
static void    AFNI_process_NIML_data( int , void * ) ;

/*-----------------------------------------------------------------------*/
/*! Routine executed at AFNI exit: shutdown all open NI_stream.
-------------------------------------------------------------------------*/

static void AFNI_niml_exit( void )
{
   int cc ;
   for( cc=0 ; cc < NUM_NIML ; cc++ )        /* close any open sockets */
      NI_stream_close( ns_listen[cc] ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Initialize NIML listening.
-------------------------------------------------------------------------*/

void AFNI_init_niml( void )
{
   int cc ;

ENTRY("AFNI_init_niml") ;

   if( started ) EXRETURN ;

   PLUTO_register_workproc( AFNI_niml_workproc , NULL ) ;
   atexit( AFNI_niml_exit ) ;

   for( cc=0 ; cc < NUM_NIML ; cc++ ) ns_listen[cc] = NULL ;

   sprintf(ns_name[0] , "tcp:host:%d" , SUMA_TCP_PORT ) ;

   started = 1 ; EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! Debug printout of a NIML element.
-------------------------------------------------------------------------*/

void NIML_to_stderr( void *nini )
{
   NI_stream ns_err ;
   ns_err = NI_stream_open( "fd:2" , "w" ) ;
   if( ns_err != NULL ){
     NI_write_element( ns_err , nini , NI_TEXT_MODE ) ;
     NI_stream_close( ns_err ) ;
   }
}

/*-----------------------------------------------------------------------*/
/*! NIML workprocess.
    - Listen for new incoming connections on any non-open connections.
    - Read and process any new data from open connections.

  (If the return is True, that means don't call this workproc again.
   If the return is False, that means call this workproc again.......)
-------------------------------------------------------------------------*/

static Boolean AFNI_niml_workproc( XtPointer elvis )
{
   int cc , nn ;
   void *nini ;

   /** loop over input NIML streams **/

   for( cc=0 ; cc < NUM_NIML ; cc++ ){

     /* open streams that aren't open */

     if( ns_listen[cc] == NULL ){
       ns_listen[cc] = NI_stream_open( ns_name[cc] , "r" ) ;
     }

     /* check if stream has gone bad */

     nn = NI_stream_goodcheck( ns_listen[cc] , 1 ) ;

     if( nn < 0 ){                          /* is bad */
       NI_stream_close( ns_listen[cc] ) ;
       ns_listen[cc] = NULL ;
       continue ;              /* skip to next stream */
     }

     if( nn == 0 ) continue ;  /* waiting: skip to next stream */

     /* if here, stream is good;
        see if there is any data to be read */

     nn = NI_stream_readcheck( ns_listen[cc] , 1 ) ;

     if( nn > 0 ){                                   /* has data */
       int ct = NI_clock_time() ;
fprintf(stderr,"NIML: reading data stream") ;

       nini = NI_read_element( ns_listen[cc] , 1 ) ;  /* read it */

fprintf(stderr," time=%d ms\n",NI_clock_time()-ct) ; ct = NI_clock_time() ;

       if( nini != NULL )
          AFNI_process_NIML_data( cc , nini ) ;

       NI_free_element( nini ) ;

fprintf(stderr,"processing time=%d ms\n",NI_clock_time()-ct) ;

     }
   }

   return False ;  /* always call me back */
}

/*----------------------------------------------------------------------*/
/*! Process NIML data.  "chan" is the type of stream it came from;
    this is currently not used.
------------------------------------------------------------------------*/

static void AFNI_process_NIML_data( int chan , void *nini )
{
   int tt = NI_element_type(nini) ;
   NI_element *nel ;

ENTRY("AFNI_process_NIML_data") ;

   if( tt < 0 ) EXRETURN ;  /* should never happen */

   /* process elements within a group separately */

   if( tt == NI_GROUP_TYPE ){
     NI_group *ngr = (NI_group *) nini ;
     int ii ;
     for( ii=0 ; ii < ngr->part_num ; ii++ )
        AFNI_process_NIML_data( chan , ngr->part[ii] ) ; /* recursion */
     EXRETURN ;
   }

   if( tt != NI_ELEMENT_TYPE ) EXRETURN ;  /* should never happen */

   /* if here, have a single data element;
      process the data based on the element name */

   nel = (NI_element *) nini ;

fprintf(stderr,"      name=%s vec_len=%d vec_filled=%d\n",nel->name,nel->vec_len,nel->vec_filled) ;

   /*--- Surface nodes for a dataset ---*/

   if( strcmp(nel->name,"SUMA_ixyz") == 0 ){
     THD_3dim_dataset *dset ;
     SUMA_surface *ag ;
     int *ic ; float *xc,*yc,*zc ; char *idc ;
     int ct ;

     /*-- check element for suitability --*/

     if( nel->vec_len    <  1        ||  /* empty element?             */
         nel->vec_filled <  1        ||  /* no data was filled in?      */
         nel->vec_num    <  4        ||  /* less than 4 columns?         */
         nel->vec_typ[0] != NI_INT   ||  /* must be int,float,float,float */
         nel->vec_typ[1] != NI_FLOAT ||
         nel->vec_typ[2] != NI_FLOAT ||
         nel->vec_typ[3] != NI_FLOAT   ) EXRETURN ;

     /*-- we need a "volume_idcode" or "dataset_idcode" attribute,
          so that we can attach this surface to a dataset for display;
          if we don't find the attribute or the dataset, then we quit --*/

     idc = NI_get_attribute( nel , "volume_idcode" ) ;
     if( idc == NULL )
       idc = NI_get_attribute( nel , "dataset_idcode" ) ;
     if( idc == NULL ) EXRETURN ;
     dset = PLUTO_find_dset_idc( idc ) ;
     if( dset == NULL ) EXRETURN ;

     /*-- if the dataset already has a surface, trash it */

     if( dset->su_surf != NULL ){
        SUMA_destroy_surface( dset->su_surf ) ; dset->su_surf = NULL ;
     }
     if( dset->su_vmap != NULL ){
        free( dset->su_vmap ) ; dset->su_vmap = NULL ;
     }
     if( dset->su_vnlist != NULL ){
        SUMA_destroy_vnlist( dset->su_vnlist ) ; dset->su_vnlist = NULL ;
     }
     if( dset->su_sname != NULL ){
        free( dset->su_sname ) ;
     }

     /*-- set the surface filename to the special "don't purge" string --*/

     dset->su_sname = strdup( "++LOCK++" ) ;

     /*-- initialize surface that we will fill up here */

ct = NI_clock_time() ;
fprintf(stderr,"      creating surface: %d nodes",nel->vec_filled) ;

     ag = SUMA_create_empty_surface() ;

     /*-- set IDCODEs of surface and of its dataset --*/

     MCW_strncpy( ag->idcode_dset , dset->idcode.str , 32 ) ;

     idc = NI_get_attribute( nel , "surface_idcode" ) ;
     if( idc == NULL )
       idc = NI_get_attribute( nel , "SUMA_idcode" ) ;
     if( idc == NULL ){
       idc = UNIQ_idcode(); MCW_strncpy(ag->idcode,idc,32); free(idc);
     } else {
       MCW_strncpy(ag->idcode,idc,32);
     }

     /*-- pointers to the data columns in the NI_element --*/

     ic = (int *)   nel->vec[0] ;
     xc = (float *) nel->vec[1] ;
     yc = (float *) nel->vec[2] ;
     zc = (float *) nel->vec[3] ;

     /*-- add nodes to the surface --*/

     SUMA_add_nodes_ixyz( ag , nel->vec_filled , ic,xc,yc,zc ) ;

     /*-- prepare the surface for AFNI --*/

fprintf(stderr," time=%d ms\n",NI_clock_time()-ct); ct = NI_clock_time() ;
fprintf(stderr,"      sorting surface") ;

     SUMA_ixyzsort_surface( ag ) ;
     dset->su_surf = ag ;

fprintf(stderr," time=%d ms\n",NI_clock_time()-ct); ct = NI_clock_time() ;
fprintf(stderr,"      vmapping surface") ;

     dset->su_vmap = SUMA_map_dset_to_surf( ag , dset ) ;

fprintf(stderr," time=%d ms\n",NI_clock_time()-ct); ct = NI_clock_time() ;
fprintf(stderr,"      vnlisting surface") ;

     dset->su_vnlist = SUMA_make_vnlist( ag , dset ) ;

     /*-- we're done! --*/

     PLUTO_dset_redisplay( dset ) ;  /* redisplay windows with this dataset */

fprintf(stderr," time=%d ms\n",NI_clock_time()-ct); ct = NI_clock_time() ;

     EXRETURN ;
   }

   /*** If here, then name of element didn't match anything ***/

   fprintf(stderr,"++ Unknown NIML input: %s\n",nel->name) ;
   EXRETURN ;
}
