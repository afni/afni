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

/*-------------------------*/
/*! Array of stream flags */

static int ns_flags[NUM_NIML] ;

/*! Waiting for connection flag */

#define FLAG_WAITING    1

/*! Connected flag */

#define FLAG_CONNECTED  2

/*! Skip flag */

#define FLAG_SKIP       4

/*-------------------------------------*/
/*! The SUMA stream index in ns_listen */

#define NS_SUMA 0

/*--------------------------------*/
/*! If 1, won't send info to SUMA */

static int dont_tell_suma = 1 ;

/*---------------------------------------*/
/*! If 1, won't listen to info from SUMA */

static int dont_hear_suma = 0 ;

/*-------------------------*/

#ifndef SUMA_TCP_PORT
#define SUMA_TCP_PORT 53211
#endif

#define EPS 0.01

/*------------------------------------------------*/
/*! If 1, send data; if 0, debug print it instead */

static int sendit=1 ;

/*-----------------------------------------------*/
/*! Flag to tell if NIML things are initialized. */

static int started = 0 ;

static int redisplay_key[MAX_CONTROLLERS] ;
static int viewpoint_key[MAX_CONTROLLERS] ;

/*---------------------*/
/* Internal prototypes */

static void    AFNI_niml_exit( void ) ;
static Boolean AFNI_niml_workproc( XtPointer ) ;
static void    AFNI_process_NIML_data( int , void * , int ) ;
static void    AFNI_niml_redisplay_CB( int,int,void *,void * ) ;
static void    AFNI_niml_viewpoint_CB( int,int,void *,void * ) ;

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

   /* initialize status and names of all listening NI_streams */

   for( cc=0 ; cc < NUM_NIML ; cc++ ){
     ns_listen[cc] = NULL ;
     ns_flags[cc]  = 0 ;
   }

   sprintf(ns_name[0] , "tcp:host:%d" , SUMA_TCP_PORT ) ;

   /* initialize all receive keys (cf. afni_receive.c) */

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){
     redisplay_key[cc] = -1 ;
     viewpoint_key[cc] = -1 ;
   }

   /* set up to receive notifications (callbacks)
      when the functional overlay is redisplayed (controller A only) */

   redisplay_key[0] = AFNI_receive_init( GLOBAL_library.controllers[0] ,
                                         RECEIVE_FUNCDISPLAY_MASK ,
                                         AFNI_niml_redisplay_CB ,
                                         GLOBAL_library.controllers[0] ) ;

   /* set up to receive notifications (callbacks)
      when the viewpoint is altered by the user  (controller A only) */

   viewpoint_key[0] = AFNI_receive_init( GLOBAL_library.controllers[0] ,
                                         RECEIVE_VIEWPOINT_MASK ,
                                         AFNI_niml_viewpoint_CB ,
                                         GLOBAL_library.controllers[0] ) ;

   /* determine if we actually want to send data */

   sendit = !AFNI_yesenv("AFNI_NIML_DONTSEND") ;

   /* and we're off to see the wizard */

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
   int cc , nn , ct , ngood=0 ;
   void *nini ;

   /** loop over input NIML streams **/

   for( cc=0 ; cc < NUM_NIML ; cc++ ){

     /* open streams that aren't open */

     if( ns_listen[cc] == NULL && (ns_flags[cc]&FLAG_SKIP)==0 ){
       ns_listen[cc] = NI_stream_open( ns_name[cc] , "r" ) ;
       if( ns_listen[cc] == NULL ){
          ns_flags[cc] = FLAG_SKIP ; continue ;
       }
       ns_flags[cc]  = FLAG_WAITING ;
     }

     ngood++ ;

     /* now check if stream has gone bad */

     nn = NI_stream_goodcheck( ns_listen[cc] , 1 ) ;

     if( nn < 0 ){                          /* is bad */
       fprintf(stderr,"++ NIML connection closed from %s\n",
                NI_stream_name(ns_listen[cc])               ) ;

       NI_stream_close( ns_listen[cc] ) ;
       ns_listen[cc] = NULL ;  /* will reopen next time */
       continue ;              /* skip to next stream  */
     }

     if( nn == 0 ) continue ;  /* waiting: skip to next stream */

     /* if here, stream is good */

     /* if just became good, print a message */

     if( ns_flags[cc] & FLAG_WAITING ){
       ns_flags[cc] = FLAG_CONNECTED ;
       fprintf(stderr,"++ NIML connection opened from %s\n",
               NI_stream_name(ns_listen[cc])                ) ;
     }

     /* see if there is any data to be read */

     nn = NI_stream_readcheck( ns_listen[cc] , 0 ) ;

     if( nn > 0 ){                                   /* has data!*/
       ct   = NI_clock_time() ;                      /* start timer */
       nini = NI_read_element( ns_listen[cc] , 2 ) ; /* read data */

       if( nini != NULL )                            /* handle it */
         AFNI_process_NIML_data( cc , nini , ct ) ;

       NI_free_element( nini ) ;                     /* trash it */
     }
   }

   dont_tell_suma = 0 ;                              /* talk to SUMA */

   if( ngood == 0 ){
      fprintf(stderr,"++ NIML shutting down: no listening sockets\n") ;
      return True ;
   }

   return False ;
}

/*----------------------------------------------------------------------*/
/*! Process NIML data.  "chan" is the type of stream it came from;
    this is currently not used.
------------------------------------------------------------------------*/

static void AFNI_process_NIML_data( int chan, void *nini, int ct_start )
{
   int tt=NI_element_type(nini) , ct_read=0 , ct_tot=0 ;
   NI_element *nel ;
   char msg[1024] ;

ENTRY("AFNI_process_NIML_data") ;

   if( tt < 0 ) EXRETURN ;  /* should never happen */

   /* process elements within a group separately */

   if( tt == NI_GROUP_TYPE ){
     NI_group *ngr = (NI_group *) nini ;
     int ii ;
     for( ii=0 ; ii < ngr->part_num ; ii++ )
        AFNI_process_NIML_data( chan , ngr->part[ii] , -1 ) ; /* recursion */
     EXRETURN ;
   }

   if( tt != NI_ELEMENT_TYPE ) EXRETURN ;  /* should never happen */

   /* if here, have a single data element;
      process the data based on the element name */

   nel = (NI_element *) nini ;

   if( ct_start >= 0 ) ct_read = NI_clock_time() - ct_start ;

#if 0
fprintf(stderr,"AFNI received NIML element name=%s\n",nel->name) ;
#endif

   /******** Surface nodes for a dataset *********/

   if( strcmp(nel->name,"SUMA_ixyz") == 0 ){
     THD_3dim_dataset *dset ;
     SUMA_surface *ag ;
     int *ic ; float *xc,*yc,*zc ; char *idc ;

     if( dont_hear_suma ) EXRETURN ;

     /*-- check element for suitability --*/

     if( nel->vec_len    <  1        ||  /* empty element?             */
         nel->vec_filled <  1        ||  /* no data was filled in?      */
         nel->vec_num    <  4        ||  /* less than 4 columns?         */
         nel->vec_typ[0] != NI_INT   ||  /* must be int,float,float,float */
         nel->vec_typ[1] != NI_FLOAT ||
         nel->vec_typ[2] != NI_FLOAT ||
         nel->vec_typ[3] != NI_FLOAT   ){

       AFNI_popup_message( "*** ERROR:\n\n"
                           " SUMA_ixyz surface data\n"
                           " is badly formatted! \n" ) ;
       EXRETURN ;
     }

     /*-- we need a "volume_idcode" or "dataset_idcode" attribute,
          so that we can attach this surface to a dataset for display;
          if we don't find the attribute or the dataset, then we quit --*/

     idc = NI_get_attribute( nel , "volume_idcode" ) ;
     if( idc == NULL )
       idc = NI_get_attribute( nel , "dataset_idcode" ) ;
     if( idc == NULL ){
        AFNI_popup_message( "***ERROR:\n "
                            " SUMA_ixyz surface input\n"
                            " does not identify dataset!\n " ) ;
        EXRETURN ;
     }
     dset = PLUTO_find_dset_idc( idc ) ;
     if( dset == NULL ){
        sprintf(msg, "***ERROR:\n\n"
                     " SUMA_ixyz surface dataset idcode is \n"
                     "   %s\n"
                     " Can't find this in AFNI\n", idc ) ;
        AFNI_popup_message( msg ) ;
        EXRETURN ;
     }

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

     SUMA_ixyzsort_surface( ag ) ;
     dset->su_surf = ag ;

#if 0
     dset->su_vmap = SUMA_map_dset_to_surf( ag , dset ) ;
#endif

     /*-- we're done! --*/

     if( ct_start >= 0 ) ct_tot = NI_clock_time() - ct_start ;

     sprintf(msg,"+++NOTICE:\n\n"
                 " SUMA_ixyz surface received:\n"
                 "  %d nodes attached to dataset \n"
                 "  %.222s\n" ,
                 nel->vec_filled , DSET_FILECODE(dset) ) ;

     if( ct_tot > 0 ) sprintf(msg+strlen(msg),"\n"
                                              "I/O time  =%4d ms\n"
                                              "Processing=%4d ms\n" ,
                              ct_read , ct_tot-ct_read ) ;

     AFNI_popup_message( msg ) ;

#if 1
     dont_tell_suma = 1 ;
     PLUTO_dset_redisplay( dset ) ;  /* redisplay windows with this dataset */
     dont_tell_suma = 0 ;
#endif

     XtSetSensitive( GLOBAL_library.controllers[0]->vwid->imag->pop_sumato_pb,
                     True  ) ;
     EXRETURN ;
   }

   /********* surface triangles from SUMA **********/

   if( strcmp(nel->name,"SUMA_ijk") == 0 ){
     THD_3dim_dataset *dset ;
     SUMA_surface *ag ;
     int *it, *jt , *kt ; char *idc ;

     if( dont_hear_suma ) EXRETURN ;

     /*-- check element for suitability --*/

     if( nel->vec_len    <  1      ||  /* empty element?        */
         nel->vec_filled <  1      ||  /* no data was filled in? */
         nel->vec_num    <  3      ||  /* less than 4 columns?  */
         nel->vec_typ[0] != NI_INT ||  /* must be int,int,int  */
         nel->vec_typ[1] != NI_INT ||
         nel->vec_typ[2] != NI_INT   ){

       AFNI_popup_message( "*** ERROR:\n\n"
                           " SUMA_ijk surface data\n"
                           " is badly formatted! \n" ) ;
       EXRETURN ;
     }

     /*-- we need a "volume_idcode" or "dataset_idcode" attribute,
          so that we can attach this surface to a dataset for display;
          if we don't find the attribute or the dataset, then we quit --*/

     idc = NI_get_attribute( nel , "volume_idcode" ) ;
     if( idc == NULL )
       idc = NI_get_attribute( nel , "dataset_idcode" ) ;
     if( idc == NULL ){
        AFNI_popup_message( "***ERROR:\n "
                            " SUMA_ijk surface input\n"
                            " does not identify dataset! \n" ) ;
        EXRETURN ;
     }
     dset = PLUTO_find_dset_idc( idc ) ;
     if( dset == NULL ){
        sprintf(msg, "***ERROR:\n\n"
                     " SUMA_ijk surface dataset idcode is \n"
                     "   %s\n"
                     " Can't find this in AFNI\n", idc ) ;
        AFNI_popup_message( msg ) ;
        EXRETURN ;
     }

     /*-- dataset musst already have a surface */

     ag = dset->su_surf ;
     if( ag == NULL ){
        sprintf(msg,"***ERROR:\n\n"
                    " SUMA_ijk surface data\n"
                    " received for dataset\n"
                    "  %.222s\n"
                    " before SUMA_ixyz data! \n" ,
                DSET_FILECODE(dset) ) ;
        AFNI_popup_message( msg ) ;
        EXRETURN ;
     }

     idc = NI_get_attribute( nel , "surface_idcode" ) ;
     if( idc == NULL )
       idc = NI_get_attribute( nel , "SUMA_idcode" ) ;
     if( idc == NULL ){
        AFNI_popup_message( "***ERROR:\n\n"
                            " SUMA_ijk surface input\n"
                            " does not have surface idcode! \n" ) ;
        EXRETURN ;
     }
     if( strcmp(dset->su_surf->idcode,idc) != 0 ){
        sprintf(msg,"***ERROR:\n\n"
                    "SUMA_ijk surface idcode = %s\n"
                    "but SUMA_ixyz had idcode= %s\n" ,
                idc , dset->su_surf->idcode ) ;
        AFNI_popup_message( msg ) ;
        EXRETURN ;
     }

     /*-- pointers to the data columns in the NI_element --*/

     it = (int *) nel->vec[0] ;
     jt = (int *) nel->vec[1] ;
     kt = (int *) nel->vec[2] ;

     /*-- add nodes to the surface --*/

     SUMA_add_triangles( ag , nel->vec_filled , it,jt,kt ) ;

     /*-- we're done! --*/

     if( ct_start >= 0 ) ct_tot = NI_clock_time() - ct_start ;

     sprintf(msg,"+++NOTICE:\n\n"
                 " SUMA_ijk triangles received:\n"
                 "  %d triangles attached to dataset \n"
                 "  %.222s\n" ,
                 nel->vec_filled , DSET_FILECODE(dset) ) ;

     if( ct_tot > 0 ) sprintf(msg+strlen(msg),"\n"
                                              "I/O time  =%4d ms\n"
                                              "Processing=%4d ms\n" ,
                              ct_read , ct_tot-ct_read ) ;

     AFNI_popup_message( msg ) ;

#if 1
     dont_tell_suma = 1 ;
     PLUTO_dset_redisplay( dset ) ;  /* redisplay windows with this dataset */
     dont_tell_suma = 0 ;
#endif

     EXRETURN ;
   }

   /********* new focus position **********/

   if( strcmp(nel->name,"SUMA_crosshair_xyz") == 0 ){
     float *xyz ;

     if( dont_hear_suma ) EXRETURN ;

     if( nel->vec_len    <  3        ||
         nel->vec_filled <  3        ||
         nel->vec_num    <  1        ||
         nel->vec_typ[0] != NI_FLOAT   ){

       AFNI_popup_message( "***ERROR:\n\n"
                           " SUMA_crosshair_xyz input \n"
                           " is badly formatted!\n" );
       EXRETURN ;
     }

     xyz = (float *) nel->vec[0] ;
     dont_tell_suma = 1 ;
     AFNI_jumpto_dicom( GLOBAL_library.controllers[0], xyz[0],xyz[1],xyz[2] );
     dont_tell_suma = 0 ;
     EXRETURN ;
   }

   /*** If here, then name of element didn't match anything ***/

   sprintf(msg,"***ERROR:\n\n"
               " Unknown NIML input: \n"
               " %.900s \n",nel->name) ;
   AFNI_popup_message(msg) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------*/
/*! Receives notice when user redisplays the functional overlay.
----------------------------------------------------------------------*/

static void AFNI_niml_redisplay_CB( int why, int q, void *qq, void *qqq )
{
   Three_D_View *im3d = (Three_D_View *) qqq ;
   THD_3dim_dataset *adset , *fdset ;
   SUMA_irgba *map ;
   int        nmap , nvused , nvtot , ct ;
   NI_element *nel ;
   char msg[16] ;

ENTRY("AFNI_niml_redisplay_CB") ;

   /* check inputs for reasonability */

   if( dont_tell_suma            ||
       !IM3D_OPEN(im3d)          ||
       !im3d->vinfo->func_visible  ) EXRETURN ;

   adset = im3d->anat_now ; if( adset->su_surf == NULL ) EXRETURN ;
   fdset = im3d->fim_now  ; if( fdset          == NULL ) EXRETURN ;

   if( sendit ){
     if( NI_stream_goodcheck(ns_listen[NS_SUMA],1) < 1 ) EXRETURN ;
   }

   /* build a node+color map */

   ct = NI_clock_time() ;

   nmap = AFNI_vnlist_func_overlay( im3d , &map , &nvused ) ;
   if( nmap < 0 ) EXRETURN ;

   if( nmap > 0 ){  /* make a data element with data */
      nel = NI_new_data_element( "SUMA_irgba" , -1 ) ;
      NI_define_rowmap_VA( nel ,
                           NI_INT  , offsetof(SUMA_irgba,id) ,
                           NI_BYTE , offsetof(SUMA_irgba,r ) ,
                           NI_BYTE , offsetof(SUMA_irgba,g ) ,
                           NI_BYTE , offsetof(SUMA_irgba,b ) ,
                           NI_BYTE , offsetof(SUMA_irgba,a ) , -1 ) ;
      NI_add_many_rows( nel , nmap , sizeof(SUMA_irgba) , map ) ;
      free(map) ;
   } else {         /* make an empty data element */
      nel = NI_new_data_element( "SUMA_irgba" , 0 ) ;
      nvused = 0 ;
   }
   nvtot = adset->su_vnlist->nvox ;  /* 13 Mar 2002 */

   /* 13 Mar 2002: send idcodes of surface and datasets involved */

   NI_set_attribute( nel , "surface_idcode" , adset->su_surf->idcode ) ;
   NI_set_attribute( nel , "volume_idcode"  , adset->idcode.str ) ;
   NI_set_attribute( nel , "function_idcode", fdset->idcode.str ) ;

   /* 13 Mar 2002: also send the number of voxels in the surface
                   and the number of voxels that were colored in */

   sprintf(msg,"%d",nvtot) ;
   NI_set_attribute( nel , "numvox_total" , msg ) ;
   sprintf(msg,"%d",nvused) ;
   NI_set_attribute( nel , "numvox_used" , msg ) ;

   if( sendit )
     NI_write_element( ns_listen[NS_SUMA] , nel , NI_BINARY_MODE ) ;
   else
     NIML_to_stderr(nel) ;

   if( GLOBAL_argopt.yes_niml > 1 )
      fprintf(stderr,
              "++ NIML write colored surface: voxels=%d nodes=%d time=%d ms\n",
              nvused , nmap , ct = NI_clock_time() - ct ) ;

   NI_free_element(nel) ;
}

/*--------------------------------------------------------------------*/
/*! Receives notice when user changes viewpoint position.
----------------------------------------------------------------------*/

static void AFNI_niml_viewpoint_CB( int why, int q, void *qq, void *qqq )
{
   Three_D_View *im3d = (Three_D_View *) qqq ;
   NI_element *nel ;
   float xyz[3] ;
   static float xold=-666,yold=-777,zold=-888 ;

ENTRY("AFNI_niml_viewpoint_CB") ;

   if( dont_tell_suma                 ||
       !IM3D_OPEN(im3d)               ||
       im3d->anat_now->su_surf == NULL  ) EXRETURN ;

   if( sendit ){
     if( NI_stream_goodcheck(ns_listen[NS_SUMA],1) < 1 ) EXRETURN ;
   }

   xyz[0] = im3d->vinfo->xi ;  /* current RAI coordinates */
   xyz[1] = im3d->vinfo->yj ;
   xyz[2] = im3d->vinfo->zk ;

   if( fabs(xyz[0]-xold) < EPS &&
       fabs(xyz[1]-yold) < EPS &&
       fabs(xyz[2]-zold) < EPS    ) EXRETURN ;  /* too close to old point */

   nel = NI_new_data_element( "SUMA_crosshair_xyz" , 3 ) ;
   NI_add_column( nel , NI_FLOAT , xyz ) ;

   /* 13 Mar 2002: add idcodes of what we are looking at right now */

   NI_set_attribute( nel, "surface_idcode", im3d->anat_now->su_surf->idcode ) ;
   NI_set_attribute( nel, "volume_idcode" , im3d->anat_now->idcode.str ) ;

   xold = xyz[0] ; yold = xyz[1] ; zold = xyz[2] ;  /* save old point */

   if( sendit )
     NI_write_element( ns_listen[NS_SUMA] , nel , NI_TEXT_MODE ) ;
   else
     NIML_to_stderr(nel) ;

   NI_free_element(nel) ;
}
