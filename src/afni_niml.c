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

/*! If 1, won't send func overlay to SUMA */

static int dont_overlay_suma = 1 ;

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
static int serrit=0 ;  /* print element headers to stderr? */

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
static void    AFNI_niml_driver( char * , NI_stream_type *, NI_element * ) ;

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

   /* 10 Dec 2002: allow user to specify NIML port number */

   cc = GLOBAL_argopt.port_niml ;
   if( cc < 1024 || cc > 65535 ) cc = SUMA_TCP_PORT ;
   sprintf( ns_name[0] , "tcp:host:%d" , cc ) ;

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
                                         GLOBAL_library.controllers[0] ,
                                         "AFNI_niml_redisplay_CB" ) ;

   /* set up to receive notifications (callbacks)
      when the viewpoint is altered by the user  (controller A only) */

   viewpoint_key[0] = AFNI_receive_init( GLOBAL_library.controllers[0] ,
                                         RECEIVE_VIEWPOINT_MASK ,
                                         AFNI_niml_viewpoint_CB ,
                                         GLOBAL_library.controllers[0] ,
                                         "AFNI_niml_viewpoint_CB" ) ;

   /* determine if we actually want to send data */

   sendit = !AFNI_yesenv("AFNI_NIML_DONTSEND") ;
   serrit = !sendit || AFNI_yesenv("AFNI_NIML_STDERR") ;   /* debugging */

   /* 12 Feb 2003: setup ni_do "DRIVE_AFNI" verb */

   NI_register_doer( "DRIVE_AFNI" , AFNI_niml_driver ) ;

   /* and we're off to see the wizard */

   started = 1 ; EXRETURN ;
}

/*---------------------------------------------------------------*/
/*! Drive AFNI from a NIML element. [12 Feb 2003] */

void AFNI_niml_driver( char *object , NI_stream_type *ns , NI_element *nel )
{
   (void) AFNI_driver( object ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Debug printout of a NIML element.
-------------------------------------------------------------------------*/

void NIML_to_stderr( void *nini , int send )
{
   NI_stream ns_err ;
   if( NI_element_type(nini) != NI_ELEMENT_TYPE ) return ;
   ns_err = NI_stream_open( "stderr:" , "w" ) ;
   if( ns_err != NULL ){
     if( send )
       fprintf(stderr,"-------------- AFNI sends NIML element: --------------\n");
     else
       fprintf(stderr,"-------------- AFNI gets NIML element:  --------------\n");
     NI_write_element( ns_err , nini , NI_TEXT_MODE | NI_HEADERONLY_FLAG ) ;
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

     nn = NI_stream_hasinput( ns_listen[cc] , 0 ) ;

     if( nn > 0 ){                                   /* has data!*/
       ct   = NI_clock_time() ;                      /* start timer */
       nini = NI_read_element( ns_listen[cc] , 2 ) ; /* read data */

       if( nini != NULL ){                           /* handle it */
         if( serrit ) NIML_to_stderr(nini,0) ;
         AFNI_process_NIML_data( cc , nini , ct ) ;
       }

       NI_free_element( nini ) ;                     /* trash it */
     }
   }

   dont_tell_suma = 0 ;                              /* talk to SUMA */
   dont_overlay_suma = 0 ;

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
     THD_slist_find find ;
     THD_3dim_dataset *dset ;
     SUMA_surface *ag ;
     int *ic ; float *xc,*yc,*zc ; char *idc , idstr[32] ;
     int num , ii ;
     Three_D_View *im3d = AFNI_find_open_controller() ;
     MCW_choose_cbs cbs ;

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

       if( nel->vec_len    < 1 ) fprintf(stderr,"** SUMA_ixyz vec_len    = %d\n",nel->vec_len) ;
       if( nel->vec_filled < 1 ) fprintf(stderr,"** SUMA_ixyz vec_filled = %d\n",nel->vec_filled) ;
       EXRETURN ;
     }

     /*-- we need a "volume_idcode" or "dataset_idcode" attribute,
          so that we can attach this surface to a dataset for display;
          if we don't find the attribute or the dataset, then we quit --*/

     idc = NI_get_attribute( nel , "volume_idcode" ) ;
     if( idc == NULL )
       idc = NI_get_attribute( nel , "dataset_idcode" ) ;
     if( idc == NULL ){
        AFNI_popup_message( "*** ERROR:\n "
                            " SUMA_ixyz surface input\n"
                            " does not identify dataset! \n " ) ;
        EXRETURN ;
     }
     find = PLUTO_dset_finder( idc ) ; dset = find.dset ;
     if( dset == NULL ){
        sprintf(msg, "*** ERROR:\n\n"
                     " SUMA_ixyz volume dataset idcode is \n"
                     "   %s\n"
                     " Can't find this in AFNI\n", idc ) ;
        AFNI_popup_message( msg ) ;
        EXRETURN ;
     }

     /*-- get surface ID code (or make it up) --*/

     idc = NI_get_attribute( nel , "surface_idcode" ) ;
     if( idc == NULL )
       idc = NI_get_attribute( nel , "SUMA_idcode" ) ;
     if( idc == NULL ){
       UNIQ_idcode_fill(idstr); idc = idstr ;
     }

     /*-- 14 Aug 2002: we used to trash old surfaces,
                       but now we just accumulate them --*/

     num = dset->su_num ;  /* number of surfaces currently attached */

     /* 19 Aug 2002: check for surface idcode in existing set of surfaces */

     for( ii=0 ; ii < num ; ii++ )
       if( strstr(dset->su_sname[ii],idc) != NULL ) break ;

     if( ii < num ){       /* found it, which is bad */
        sprintf(msg, "+++ WARNING:\n\n"
                     " SUMA_ixyz volume surface idcode is\n"
                     "  %s\n"
                     " which is already stored inside dataset \n"
                     "  %.222s\n" ,
                idc , DSET_FILECODE(dset) ) ;
        AFNI_popup_message( msg ) ;
        EXRETURN ;
     }

     /*-- make space for 1 more set of surface pointers --*/

     /* the surface itself [created below] */

     dset->su_surf = (SUMA_surface **) realloc(dset->su_surf,
                                               (num+1)*sizeof(SUMA_surface *)) ;

     /* the voxel-to-node map [created empty] */

     dset->su_vmap = (int **) realloc(dset->su_vmap,
                                      (num+1)*sizeof(int *)) ;
     dset->su_vmap[num] = NULL ;

     /* the voxel-to-node list [created empty] */

     dset->su_vnlist = (SUMA_vnlist **) realloc(dset->su_vnlist,
                                                (num+1)*sizeof(SUMA_vnlist *)) ;
     dset->su_vnlist[num] = NULL ;

     /* the surface filename [filled in below] */

     dset->su_sname = (char **) realloc(dset->su_sname,
                                        (num+1)*sizeof(char *)) ;

     /*-- initialize surface that we will fill up here --*/

     dset->su_surf[num] = ag = SUMA_create_empty_surface() ;

     MCW_strncpy(ag->idcode,idc,32);  /* idc is surface idcode from above */

     /*-- 19 Aug 2002: get surface label (or make it up) --*/

     idc = NI_get_attribute( nel , "surface_label" ) ;
     if( idc == NULL )
       idc = NI_get_attribute( nel , "SUMA_label" ) ;

     if( idc != NULL )
       MCW_strncpy(ag->label,idc,32) ;
     else
       sprintf(ag->label,"Surf#%d",num+1) ;

     /*-- set surface filename now ["++LOCK++" ==> keep in memory] --*/

     dset->su_sname[num] = AFMALL(char, 16+strlen(ag->idcode)) ;
     strcpy(dset->su_sname[num], "++LOCK++ ") ;
     strcat(dset->su_sname[num], ag->idcode ) ;

     /*-- set IDCODEs of surface and of its dataset --*/

     MCW_strncpy( ag->idcode_dset , dset->idcode.str , 32 ) ;

     /*-- pointers to the data columns in the NI_element --*/

     ic = (int *)   nel->vec[0] ;  /* index */
     xc = (float *) nel->vec[1] ;  /* x coordinate */
     yc = (float *) nel->vec[2] ;  /* y coordinate */
     zc = (float *) nel->vec[3] ;  /* z coordinate */

     /*-- add nodes to the surface --*/

     SUMA_add_nodes_ixyz( ag , nel->vec_filled , ic,xc,yc,zc ) ;

     /*-- prepare the surface for AFNI --*/

     SUMA_ixyzsort_surface( ag ) ;

     dset->su_num = num+1 ;     /* 14 Aug 2002 */

#if 0
     dset->su_vmap[num] = SUMA_map_dset_to_surf( ag , dset ) ;
#endif

     /*-- we're done! --*/

     if( ct_start >= 0 )                     /* keep track of how */
       ct_tot = NI_clock_time() - ct_start ; /* long this took   */

     sprintf(msg,"+++ NOTICE:\n\n"                    /* and tell  */
                 " SUMA_ixyz surface received:\n"     /* the user  */
                 "  %-14.14s\n"                       /* some info */
                 " %d nodes attached to dataset\n"
                 "  %.222s\n"
                 " This is surface #%d for this dataset \n" ,
                 ag->label, nel->vec_filled , DSET_FILECODE(dset), num+1 ) ;

     if( ct_tot > 0 ) sprintf(msg+strlen(msg),"\n"
                                              "I/O time  =%4d ms\n"
                                              "Processing=%4d ms\n" ,
                              ct_read , ct_tot-ct_read ) ;

     /* 16 Jun 2003: if need be, switch sessions and anatomy */

     if( find.sess_index != im3d->vinfo->sess_num ){
       cbs.ival = find.sess_index ;
       AFNI_finalize_dataset_CB( im3d->vwid->view->choose_sess_pb ,
                                 (XtPointer) im3d ,  &cbs          ) ;
     }
     if( ISANAT(dset) && find.dset_index != im3d->vinfo->anat_num ){
       cbs.ival = find.dset_index ;
       AFNI_finalize_dataset_CB( im3d->vwid->view->choose_anat_pb ,
                                 (XtPointer) im3d ,  &cbs          ) ;
     }

     AFNI_popup_message( msg ) ;

     /* need to make the "Control Surface"
        widgets know about this extra surface */

     AFNI_update_all_surface_widgets( dset ) ;  /* 19 Aug 2002 */

#if 1
     dont_tell_suma = 1 ;
     PLUTO_dset_redisplay( dset ) ;  /* redisplay windows with this dataset */
     dont_tell_suma = 0 ;
#endif

     XtSetSensitive( im3d->vwid->imag->pop_sumato_pb, True  ) ;
     EXRETURN ;
   }

   /********* surface triangles from SUMA **********/

   if( strcmp(nel->name,"SUMA_ijk") == 0 ){
     THD_3dim_dataset *dset ;
     SUMA_surface *ag ;
     int *it, *jt , *kt ; char *idc ;
     int num , ii , nold ;

     if( dont_hear_suma ) EXRETURN ;

     /*-- check element for suitability --*/

     if( nel->vec_len    <  1      ||  /* empty element?        */
         nel->vec_filled <  1      ||  /* no data was filled in? */
         nel->vec_num    <  3      ||  /* less than 4 columns?  */
         nel->vec_typ[0] != NI_INT ||  /* must be int,int,int  */
         nel->vec_typ[1] != NI_INT ||
         nel->vec_typ[2] != NI_INT   ){

       AFNI_popup_message( "*** ERROR:\n\n"
                           " SUMA_ijk surface data \n"
                           " is badly formatted!\n" ) ;
       EXRETURN ;
     }

     /*-- we need a "volume_idcode" or "dataset_idcode" attribute,
          so that we can attach this surface to a dataset for display;
          if we don't find the attribute or the dataset, then we quit --*/

     idc = NI_get_attribute( nel , "volume_idcode" ) ;
     if( idc == NULL )
       idc = NI_get_attribute( nel , "dataset_idcode" ) ;
     if( idc == NULL ){
        AFNI_popup_message( "*** ERROR:\n "
                            " SUMA_ijk surface input\n"
                            " does not identify dataset! \n" ) ;
        EXRETURN ;
     }
     dset = PLUTO_find_dset_idc( idc ) ;
     if( dset == NULL ){
        sprintf(msg, "*** ERROR:\n\n"
                     " SUMA_ijk surface dataset idcode is \n"
                     "   %s\n"
                     " Can't find this in AFNI\n", idc ) ;
        AFNI_popup_message( msg ) ;
        EXRETURN ;
     }

     /*-- dataset must already have a surface --*/

     num = dset->su_num ;
     if( num == 0 ){
        sprintf(msg,"*** ERROR:\n\n"
                    " SUMA_ijk surface data\n"
                    " received for dataset\n"
                    "  %.222s\n"
                    " before any SUMA_ixyz data! \n" ,
                DSET_FILECODE(dset) ) ;
        AFNI_popup_message( msg ) ;
        EXRETURN ;
     }

     idc = NI_get_attribute( nel , "surface_idcode" ) ;
     if( idc == NULL )
       idc = NI_get_attribute( nel , "SUMA_idcode" ) ;
     if( idc == NULL ){
        AFNI_popup_message( "*** ERROR:\n\n"
                            " SUMA_ijk surface input\n"
                            " does not have surface idcode! \n" ) ;
        EXRETURN ;
     }

     /* 14 Aug 2002: find surface idcode in dataset's list of surfaces */

     for( ii=0 ; ii < num ; ii++ )
       if( strstr(dset->su_sname[ii],idc) != NULL ) break ;

     if( ii == num ){
        sprintf(msg, "*** ERROR:\n\n"
                     " SUMA_ijk surface input surface idcode\n"
                     "  %s\n"
                     " does not match any surface on dataset \n"
                     "  %.222s\n" ,
                idc, DSET_FILECODE(dset) ) ;
        AFNI_popup_message( msg ) ;
        EXRETURN ;
     }

     ag = dset->su_surf[ii] ; /* set surface to run with */

     if( ag->num_ijk > 0 ){
        sprintf(msg, "*** WARNING:\n\n"
                     " SUMA_ijk surface input surface idcode\n"
                     "  %s\n"
                     " already has %d triangles in it, and\n"
                     " the SUMA user is trying to add %d more!\n" ,
                idc, ag->num_ijk , nel->vec_filled ) ;
        AFNI_popup_message( msg ) ;
        EXRETURN ;
     }

     /*-- pointers to the data columns in the NI_element --*/

     it = (int *) nel->vec[0] ;  /* node index #1 */
     jt = (int *) nel->vec[1] ;  /* node index #2 */
     kt = (int *) nel->vec[2] ;  /* node index #3 */

     /*-- add nodes to the surface --*/

     nold = ag->num_ijk ;  /* 19 Aug 2002: # triangles before */

     SUMA_add_triangles( ag , nel->vec_filled , it,jt,kt ) ;

     /*-- we're done! --*/

     if( ct_start >= 0 )                      /* keep track    */
       ct_tot = NI_clock_time() - ct_start ;  /* of time spent */

     if( nold == 0 )
       sprintf(msg,"+++ NOTICE:\n\n"                       /* let the   */
                   " SUMA_ijk triangles received:\n"       /* pitiful   */
                   " %d triangles attached to surface \n"  /* user see  */
                   "  %-14.14s\n"                          /* what just */
                   " in dataset\n"                         /* happened  */
                   "  %.222s\n" ,
                   nel->vec_filled , ag->label , DSET_FILECODE(dset) ) ;
     else
       sprintf(msg,"+++ NOTICE:\n\n"
                   " SUMA_ijk triangles received:\n"
                   " %d NEW triangles attached to surface\n"
                   "  %-14.14s\n"
                   " (previously had %d triangles) in dataset \n"
                   "  %.222s\n" ,
                   nel->vec_filled , ag->label , nold , DSET_FILECODE(dset) ) ;

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

       AFNI_popup_message( "+++ WARNING:\n\n"
                           " SUMA_crosshair_xyz input \n"
                           " is badly formatted!\n" );
       EXRETURN ;
     }

     xyz = (float *) nel->vec[0] ;
     dont_tell_suma = 1 ;
     AFNI_jumpto_dicom( AFNI_find_open_controller(), xyz[0],xyz[1],xyz[2] );
     dont_tell_suma = 0 ;
     EXRETURN ;
   }

   /*********** ROI drawing from SUMA **********/

   if( strcmp(nel->name,"Node_ROI") == 0 ){
     int *nlist , *nval , num_list , num,ii,jj,pp,ks ;
     char *surf_idc , *roi_prefix , *dset_idc ;
     THD_slist_find find ;
     THD_3dim_dataset *dset_anat , *dset_func ;
     SUMA_surface *ag ;
     Three_D_View *im3d = AFNI_find_open_controller() ;
     MCW_choose_cbs cbs ;
     THD_session *sess ;
     THD_fvec3 fv ; THD_ivec3 iv ;
     short *funcar ;
     float xbot,ybot,zbot , xtop,ytop,ztop ;
     int wodsave , nx,ny,nxy ;

     if( dont_hear_suma ) EXRETURN ;

STATUS("received Node_ROI element") ;

     if( nel->vec_num    <  2        ||
         nel->vec_typ[0] != NI_INT   ||
         nel->vec_typ[1] != NI_INT     ){

       AFNI_popup_message( "+++ WARNING:\n\n"
                           " Node_ROI input \n"
                           " is badly formatted!\n" );
       EXRETURN ;
     }

     nlist    = (int *) nel->vec[0] ;  /* node list */
     nval     = (int *) nel->vec[1] ;  /* value list */
     num_list = nel->vec_filled ;      /* number of nodes */

     /** get ID codes of surface and anat parents **/

STATUS("checking Node_ROI ID codes") ;

     surf_idc = NI_get_attribute( nel , "DomParent_idcode" ) ;
     if( surf_idc == NULL )
       surf_idc = NI_get_attribute( nel , "surface_idcode" ) ;
     if( surf_idc == NULL )
       surf_idc = NI_get_attribute( nel , "SUMA_idcode" ) ;

     dset_idc = NI_get_attribute( nel , "volume_idcode" ) ;
     if( dset_idc == NULL )
       dset_idc = NI_get_attribute( nel , "dataset_idcode" ) ;

     /** get name of dataset this goes into **/

     roi_prefix = NI_get_attribute( nel , "target_volume" ) ;
     if( roi_prefix == NULL )
       roi_prefix = NI_get_attribute( nel , "ROI_prefix" ) ;

     /** check for errors [there are lots of possibilities] **/

     if( surf_idc == NULL ){
       AFNI_popup_message( "*** ERROR:\n\n"
                           " Node_ROI input doesn't\n"
                           " set 'DomParent_idcode'!\n" ) ;
       EXRETURN ;
     }
     if( dset_idc == NULL ){
       AFNI_popup_message( "*** ERROR:\n\n"
                           " Node_ROI input doesn't\n"
                           " set 'volume_idcode'!\n" ) ;
       EXRETURN ;
     }
     if( roi_prefix == NULL ){
       AFNI_popup_message( "*** ERROR:\n\n"
                           " Node_ROI input doesn't\n"
                           " set 'target_volume'!\n" ) ;
       EXRETURN ;
     }
     if( !THD_filename_pure(roi_prefix) ){
       sprintf(msg, "*** ERROR:\n\n"
                    " Node_ROI 'target_volume' prefix \n"
                    "   %s\n"
                    " contains illegal characters!\n" , roi_prefix ) ;
       AFNI_popup_message( msg ) ;
       EXRETURN ;
     }

     /** find parent volume for this ROI (from its ID code) **/

STATUS("searching for Node_ROI parent volume") ;

     find = PLUTO_dset_finder( dset_idc ) ; dset_anat = find.dset ;
     if( dset_anat == NULL ){
       sprintf(msg, "*** ERROR:\n\n"
                    " Node_ROI volume dataset idcode is \n"
                    "   %s\n"
                    " Can't find this in AFNI\n", dset_idc ) ;
       AFNI_popup_message( msg ) ;
       EXRETURN ;
     }

     /** find the surface within this dataset (from its ID code) **/

STATUS("searching for Node_ROI surface") ;

     num = dset_anat->su_num ;
     if( num == 0 ){
       sprintf(msg,"*** ERROR:\n\n"
                   " Node_ROI data received for dataset\n"
                   "  %.222s\n"
                   " before any surfaces are attached! \n" ,
               DSET_FILECODE(dset_anat) ) ;
       AFNI_popup_message( msg ) ;
       EXRETURN ;
     }

     for( ks=0 ; ks < num ; ks++ )
       if( strstr(dset_anat->su_sname[ks],surf_idc) != NULL ) break ;

     if( ks == num ){
       sprintf(msg, "*** ERROR:\n\n"
                    " Node_ROI surface idcode\n"
                    "  %s\n"
                    " does not match any surface on dataset \n"
                    "  %.222s\n" ,
               surf_idc, DSET_FILECODE(dset_anat) ) ;
       AFNI_popup_message( msg ) ;
       EXRETURN ;
     }

     ag = dset_anat->su_surf[ks] ; /* set surface to run with */

#if 0
     if( dset_anat->su_vnlist[ks] == NULL ){
       dset_anat->su_vnlist[ks] = SUMA_make_vnlist( ag , dset_anat ) ;
       if( dset_anat->su_vnlist[ks] == NULL ){
         sprintf(msg, "*** INTERNAL AFNI ERROR:\n\n"
                      " Node_ROI dataset is\n"
                      "  %.222s\n"
                      " but can't make vnlist for surface?!\n" ,
                 DSET_FILECODE(dset_anat) ) ;
         AFNI_popup_message( msg ) ;
         EXRETURN ;
       }
     }
#endif

     /** switch session and anat dataset, if need be **/

     if( find.sess_index != im3d->vinfo->sess_num ){
       cbs.ival = find.sess_index ;
       AFNI_finalize_dataset_CB( im3d->vwid->view->choose_sess_pb ,
                                 (XtPointer) im3d ,  &cbs          ) ;
     }
     if( find.dset_index >= 0 && find.dset_index != im3d->vinfo->anat_num ){
       cbs.ival = find.dset_index ;
       AFNI_finalize_dataset_CB( im3d->vwid->view->choose_anat_pb ,
                                 (XtPointer) im3d ,  &cbs          ) ;
     }
     sess = GLOBAL_library.sslist->ssar[im3d->vinfo->sess_num] ;

     AFNI_update_all_surface_widgets( dset_anat ) ;
     XtSetSensitive( im3d->vwid->imag->pop_sumato_pb, True ) ;

     /* see if ROI dataset already exists */

STATUS("searching for Node_ROI functional dataset") ;

     find = THD_dset_in_session( FIND_PREFIX , roi_prefix , sess ) ;
     dset_func = find.dset ;

     /* func dataset already exists?  Check if for goodness. */

     if( dset_func != NULL ){
       if( !EQUIV_DATAXES(dset_anat->daxes,dset_func->daxes) ){
         sprintf(msg, "*** ERROR:\n\n"
                      " Node_ROI functional dataset\n"
                      "  %.222s\n"
                      " exists, but doesn't match geometry of anat dataset\n"
                      "  %.222s\n" ,
                 roi_prefix , DSET_FILECODE(dset_anat) ) ;
         AFNI_popup_message( msg ) ;
         EXRETURN ;
       }
       DSET_mallocize(dset_func) ;     /* oops */
       if( !DSET_LOADED(dset_func) ){
         DSET_load(dset_func) ;
         if( !DSET_LOADED(dset_func) ){
           sprintf(msg, "*** ERROR:\n\n"
                        " Node_ROI functional dataset\n"
                        "  %.222s\n"
                        " exists, but doesn't have data!\n" ,
                   roi_prefix ) ;
           AFNI_popup_message( msg ) ;
           EXRETURN ;
         }
         DESTROY_VVLIST(ag->vv) ; ag->vv = NULL ;
         sprintf(msg,"+++ NOTICE:\n\n"
                     " Node_ROI command is using existing dataset\n"
                     "  %.222s\n" , DSET_FILECODE(dset_func) ) ;
         AFNI_popup_message( msg ) ;
       }
       if( find.dset_index >= 0 && find.dset_index != im3d->vinfo->func_num ){
         cbs.ival = find.dset_index ;
         AFNI_finalize_dataset_CB( im3d->vwid->view->choose_func_pb ,
                                   (XtPointer) im3d ,  &cbs          ) ;
       }

     } else { /*** no pre-existing func?  create a dataset now ***/

       ii = sess->num_dsset ;
       if( ii >= THD_MAX_SESSION_SIZE ){
         sprintf(msg, "*** ERROR:\n\n"
                      " Can't create Node_ROI dataset\n"
                      "  %.222s\n"
                      " because of AFNI session overflow!\n" ,
                 roi_prefix ) ;
         AFNI_popup_message( msg ) ;
         EXRETURN ;
       }

       dset_func = EDIT_empty_copy( dset_anat ) ;
       EDIT_dset_items( dset_func ,
                          ADN_prefix    , roi_prefix     ,
                          ADN_type      , HEAD_FUNC_TYPE ,
                          ADN_func_type , FUNC_BUCK_TYPE ,
                          ADN_nvals     , 1              ,
                          ADN_ntt       , 0              ,
                          ADN_brick_fac , NULL           ,
                        ADN_none ) ;
       EDIT_BRICK_TO_NOSTAT( dset_func , 0 ) ;
       EDIT_substitute_brick( dset_func , 0 , MRI_short , NULL ) ;

       sess->dsset[ii][dset_func->view_type] = dset_func ;
       sess->num_dsset ++ ;

STATUS("switching func to Node_ROI dataset") ;

       cbs.ival = ii ;
       AFNI_finalize_dataset_CB( im3d->vwid->view->choose_func_pb ,
                                 (XtPointer) im3d ,  &cbs          ) ;

STATUS("popping up Node_ROI dataset creation notice") ;
       sprintf(msg,"+++ NOTICE:\n\n"
                   " Node_ROI command is creating dataset\n"
                   "  %.222s\n" ,
              DSET_FILECODE(dset_func) ) ;
       AFNI_popup_message( msg ) ;

STATUS("destroying any pre-existing Node_ROI vvlist") ;
       DESTROY_VVLIST(ag->vv) ; ag->vv = NULL ;
     }

STATUS("locking Node_ROI dataset into memory") ;
     DSET_lock(dset_func) ;  /* lock into memory (no purge allowed) */

     funcar = (short *) DSET_BRICK_ARRAY(dset_func,0) ;  /* array to draw */

     /** now, see if there is an old voxel value list
              attached to the surface ; if so zero out those voxels **/

     if( ag->vv != NULL ){
fprintf(stderr,"++ erasing %d voxels from previous SUMA ROI\n",ag->vv->nvox) ;
       for( ii=0 ; ii < ag->vv->nvox ; ii++ ) funcar[ ag->vv->voxijk[ii] ] = 0;
       DESTROY_VVLIST(ag->vv) ; ag->vv = NULL ;
     } else {
STATUS("no old Node_ROI vvlist") ;
     }

     /** now put values from SUMA into dataset array **/

     if( num_list > 0 ){
fprintf(stderr,"++ writing %d voxels from SUMA ROI\n",num_list) ;
       ag->vv = (SUMA_vvlist *) malloc( sizeof(SUMA_vvlist) ) ;
       ag->vv->nvox   = num_list ;
       ag->vv->voxijk = (int *)   malloc( sizeof(int)  *num_list ) ;
       ag->vv->voxval = (float *) malloc( sizeof(float)*num_list ) ;

       wodsave = dset_func->wod_flag ; dset_func->wod_flag = 0 ;

       xbot = DSET_XXMIN(dset_func) ; xtop = DSET_XXMAX(dset_func) ;
       ybot = DSET_YYMIN(dset_func) ; ytop = DSET_YYMAX(dset_func) ;
       zbot = DSET_ZZMIN(dset_func) ; ztop = DSET_ZZMAX(dset_func) ;
       nx = DSET_NX(dset_func); ny = DSET_NY(dset_func); nxy = nx*ny ;

       for( ii=0 ; ii < num_list ; ii++ ){
         pp = SUMA_find_node_id( ag , nlist[ii] ) ;
         if( pp >= 0 ){
           LOAD_FVEC3( fv , ag->ixyz[pp].x, ag->ixyz[pp].y, ag->ixyz[pp].z ) ;
           fv = THD_dicomm_to_3dmm( dset_func , fv ) ;
           if( fv.xyz[0] < xbot || fv.xyz[0] > xtop ) continue ;
           if( fv.xyz[1] < ybot || fv.xyz[1] > ytop ) continue ;
           if( fv.xyz[2] < zbot || fv.xyz[2] > ztop ) continue ;
           iv = THD_3dmm_to_3dind( dset_func , fv ) ;
           jj = iv.ijk[0] + iv.ijk[1]*nx + iv.ijk[2]*nxy ;
           funcar[jj] = nval[ii] ;
           ag->vv->voxijk[ii] = jj ; ag->vv->voxval[ii] = nval[ii] ;
         }
       }
     } else {
STATUS("no nodes in Node_ROI input") ;
     }

     DSET_write( dset_func ) ;  /* save to disk */

     dont_overlay_suma = 1 ;

#if 1
STATUS("redisplay Node_ROI function") ;
     MCW_set_bbox( im3d->vwid->view->see_func_bbox , 1 ) ;
     im3d->vinfo->func_visible = 1 ;
     PLUTO_dset_redisplay( dset_func ) ;  /* redisplay windows with this dataset */
     AFNI_process_drawnotice( im3d ) ;
#endif

     EXRETURN ;
   }

   /*** If here, then name of element didn't match anything ***/

   sprintf(msg,"*** ERROR:\n\n"
               " Unknown NIML input: \n"
               "  %.222s \n"
               " Ignoring it, and hoping it goes away.\n" ,
               nel->name) ;
   AFNI_popup_message(msg) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------*/

void AFNI_disable_suma_overlay( int aa )
{
   dont_overlay_suma = aa ;
}

/*--------------------------------------------------------------------*/
/*! Receives notice when user redisplays the functional overlay.
----------------------------------------------------------------------*/

static void AFNI_niml_redisplay_CB( int why, int q, void *qq, void *qqq )
{
   Three_D_View *im3d = (Three_D_View *) qqq ;
   THD_3dim_dataset *adset , *fdset ;
   SUMA_irgba *map ;
   int        nmap , nvused , nvtot , ct , ks ;
   NI_element *nel ;
   char msg[16] ;

ENTRY("AFNI_niml_redisplay_CB") ;

   /* check inputs for reasonability */

   if( dont_tell_suma            ||
       dont_overlay_suma         ||
       !IM3D_OPEN(im3d)          ||
       !im3d->vinfo->func_visible  ) EXRETURN ;

   adset = im3d->anat_now ; if( adset->su_num == 0    ) EXRETURN ;
   fdset = im3d->fim_now  ; if( fdset         == NULL ) EXRETURN ;

   if( sendit ){
     if( NI_stream_goodcheck(ns_listen[NS_SUMA],1) < 1 ) EXRETURN ;
   }

   /* build a node+color map */

   ct = NI_clock_time() ;

   /* 12 Dec 2002:
      Now we loop over all surfaces in the current anat
      and send the node+color map for each and every one! */

   for( ks=0 ; ks < adset->su_num ; ks++ ){

     nmap = AFNI_vnlist_func_overlay( im3d,ks , &map,&nvused ) ;

#if 0
     if( serrit ) fprintf(stderr,"AFNI_niml_redisplay_CB: nmap=%d\n",nmap) ;
#endif

     if( nmap < 0 || adset->su_vnlist[ks] == NULL ) continue ; /* this is bad */

     if( nmap > 0 ){  /*--- make a data element with data ---*/

       int *icol ; byte *rcol, *gcol, *bcol, *acol ; int ii ;

       nel = NI_new_data_element( "SUMA_irgba" , nmap ) ;

       /* adding a NULL column creates it, full of zeros */

       NI_add_column( nel , NI_INT  , NULL ) ; icol = nel->vec[0] ;
       NI_add_column( nel , NI_BYTE , NULL ) ; rcol = nel->vec[1] ;
       NI_add_column( nel , NI_BYTE , NULL ) ; gcol = nel->vec[2] ;
       NI_add_column( nel , NI_BYTE , NULL ) ; bcol = nel->vec[3] ;
       NI_add_column( nel , NI_BYTE , NULL ) ; acol = nel->vec[4] ;

       for( ii=0 ; ii < nmap ; ii++ ){   /* copy data into element */
         icol[ii] = map[ii].id ;
         rcol[ii] = map[ii].r  ; gcol[ii] = map[ii].g ;
         bcol[ii] = map[ii].b  ; acol[ii] = map[ii].a ;
       }

       free(map) ;       /* data in nel, so don't need map no more */

     } else {         /*--- make an empty data element ---*/

       nel = NI_new_data_element( "SUMA_irgba" , 0 ) ;
       nvused = 0 ;

     }

     nvtot = adset->su_vnlist[ks]->nvox ;  /* 13 Mar 2002 */

     /* 13 Mar 2002: send idcodes of surface and datasets involved */

     NI_set_attribute( nel, "surface_idcode" , adset->su_surf[ks]->idcode ) ;
     NI_set_attribute( nel, "volume_idcode"  , adset->idcode.str ) ;
     NI_set_attribute( nel, "function_idcode", fdset->idcode.str ) ;

     /* 13 Mar 2002: also send the number of voxels in the surface
                     and the number of voxels that were colored in */

     sprintf(msg,"%d",nvtot) ;
     NI_set_attribute( nel , "numvox_total" , msg ) ;
     sprintf(msg,"%d",nvused) ;
     NI_set_attribute( nel , "numvox_used" , msg ) ;

     if( sendit )
       NI_write_element( ns_listen[NS_SUMA] , nel , NI_BINARY_MODE ) ;
     if( serrit )
       NIML_to_stderr(nel,1) ;

#if 0
     if( serrit || GLOBAL_argopt.yes_niml > 1 )
       fprintf(stderr,
               "++ NIML write colored surface: voxels=%d nodes=%d time=%d ms\n",
               nvused , nmap , ct = NI_clock_time() - ct ) ;
#endif

     NI_free_element(nel) ;  /* it's gone, so forget it */

   } /* end of loop over surface in anat dataset */

   EXRETURN ;
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
   int ks , kbest=-1,ibest=-1     ,ii , nnod ;
   float             dbest=WAY_BIG,dd , xbot,xtop,ybot,ytop,zbot,ztop ;
   SUMA_surface *ag ;
   SUMA_ixyz *nod ;

ENTRY("AFNI_niml_viewpoint_CB") ;

   if( dont_tell_suma                    ||
       !IM3D_OPEN(im3d)                  ||
       im3d->anat_now->su_num     == 0   ||
       im3d->anat_now->su_surf[0] == NULL  ) EXRETURN ;

   if( sendit ){
     if( NI_stream_goodcheck(ns_listen[NS_SUMA],1) < 1 ) EXRETURN ;
   }

   xyz[0] = im3d->vinfo->xi ;  /* current RAI coordinates */
   xyz[1] = im3d->vinfo->yj ;
   xyz[2] = im3d->vinfo->zk ;

   if( fabs(xyz[0]-xold) < EPS &&
       fabs(xyz[1]-yold) < EPS &&
       fabs(xyz[2]-zold) < EPS    ) EXRETURN ;  /* too close to old point */

   /* 20 Feb 2003: find closest node */

   AFNI_get_xhair_node( im3d , &kbest , &ibest ) ;

   if( kbest < 0 ) kbest = 0 ;  /* default surface */

   /* now send info to SUMA */

   nel = NI_new_data_element( "SUMA_crosshair_xyz" , 3 ) ;
   NI_add_column( nel , NI_FLOAT , xyz ) ;

   /* 13 Mar 2002: add idcodes of what we are looking at right now */

   NI_set_attribute( nel, "surface_idcode", im3d->anat_now->su_surf[kbest]->idcode ) ;
   NI_set_attribute( nel, "volume_idcode" , im3d->anat_now->idcode.str ) ;

   /* 20 Feb 2003: set attribute showing closest node ID */

   if( ibest >= 0 ){
     char str[32] ;
     sprintf(str,"%d",ibest) ;
     NI_set_attribute( nel, "surface_nodeid" , str ) ;
   }

   xold = xyz[0] ; yold = xyz[1] ; zold = xyz[2] ;  /* save old point */

   if( sendit )
     NI_write_element( ns_listen[NS_SUMA] , nel , NI_TEXT_MODE ) ;
   if( serrit )
     NIML_to_stderr(nel,1) ;

   NI_free_element(nel) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void AFNI_get_xhair_node( void *qq3d , int *kkbest , int *iibest )
{
   Three_D_View *im3d = (Three_D_View *)qq3d ;
   int ks , kbest=-1,ibest=-1     ,ii , nnod ;
   float  xyz[3] ,   dbest=WAY_BIG,dd , xbot,xtop,ybot,ytop,zbot,ztop ;
   SUMA_surface *ag ;
   SUMA_ixyz *nod ;

   if( !IM3D_OPEN(im3d) || (kkbest==NULL && iibest==NULL) ) return ;
   if( im3d->anat_now->su_num     == 0   ||
       im3d->anat_now->su_surf[0] == NULL  ) return ;

   xyz[0] = im3d->vinfo->xi ;  /* current RAI coordinates */
   xyz[1] = im3d->vinfo->yj ;
   xyz[2] = im3d->vinfo->zk ;

   /* 20 Feb 2003: find closest node */

   xbot = ybot = zbot = xtop = ytop = ztop = 0.0 ;   /* unrestricted */

   if( im3d->vinfo->view_setter > 0 ){   /* restrict to a thick plane */
     THD_fvec3 fv ;
     LOAD_FVEC3(fv,DSET_DX(im3d->anat_now),
                   DSET_DY(im3d->anat_now),DSET_DZ(im3d->anat_now)) ;
     fv = THD_3dmm_to_dicomm(im3d->anat_now,fv) ;
     switch( im3d->vinfo->view_setter ){
       case AXIAL:
         dd = 0.499*fabs(fv.xyz[2]) ; zbot = xyz[2]-dd ; ztop = xyz[2]+dd ;
       break ;
       case SAGITTAL:
         dd = 0.499*fabs(fv.xyz[0]) ; xbot = xyz[0]-dd ; xtop = xyz[0]+dd ;
       break ;
       case CORONAL:
         dd = 0.499*fabs(fv.xyz[1]) ; ybot = xyz[1]-dd ; ytop = xyz[1]+dd ;
       break ;
     }
#if 0
     fprintf(stderr,"view_setter=%d box=%f,%f  %f,%f  %f,%f\n",
             im3d->vinfo->view_setter , xbot,xtop,ybot,ytop,zbot,ztop ) ;
#endif
   }

   /* search all surfaces */

   for( ks=0 ; ks < im3d->anat_now->su_num ; ks++ ){
     ag  = im3d->anat_now->su_surf[ks]; if( ag == NULL ) continue;
     nod = ag->ixyz ; nnod = ag->num_ixyz ;
     ii = AFNI_find_closest_node( nnod,nod , xyz[0],xyz[1],xyz[2] ,
                                  xbot,xtop,ybot,ytop,zbot,ztop    ) ;
     if( ii >= 0 ){
       dd = sqrt( (xyz[0]-nod[ii].x)*(xyz[0]-nod[ii].x)
                 +(xyz[1]-nod[ii].y)*(xyz[1]-nod[ii].y)
                 +(xyz[2]-nod[ii].z)*(xyz[2]-nod[ii].z) ) ;
       if( kbest < 0 || dd < dbest ){
         kbest = ks ; ibest = ii ; dbest = dd ;
       }
     }
   }

   /* if didn't find anything, try again unrestricted */
   if( kbest < 0 && im3d->vinfo->view_setter > 0 ){
     xbot = ybot = zbot = xtop = ytop = ztop = 0.0 ;
     for( ks=0 ; ks < im3d->anat_now->su_num ; ks++ ){
       ag  = im3d->anat_now->su_surf[ks]; if( ag == NULL ) continue;
       nod = ag->ixyz ; nnod = ag->num_ixyz ;
       ii = AFNI_find_closest_node( nnod,nod , xyz[0],xyz[1],xyz[2] ,
                                    xbot,xtop,ybot,ytop,zbot,ztop    ) ;
       if( ii >= 0 ){
         dd = sqrt( (xyz[0]-nod[ii].x)*(xyz[0]-nod[ii].x)
                   +(xyz[1]-nod[ii].y)*(xyz[1]-nod[ii].y)
                   +(xyz[2]-nod[ii].z)*(xyz[2]-nod[ii].z) ) ;
         if( kbest < 0 || dd < dbest ){
           kbest = ks ; ibest = ii ; dbest = dd ;
         }
       }
     }
   }

   if( kbest >= 0 ){
     ag = im3d->anat_now->su_surf[kbest] ; nod = ag->ixyz ;
     ibest = nod[ibest].id ;
   }

   if( kkbest != NULL ) *kkbest = kbest ;
   if( iibest != NULL ) *iibest = ibest ;
   return ;
}
