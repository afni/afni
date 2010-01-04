#include "afni.h"
#include "vol2surf.h"
#include "thd_ttatlas_query.h"

/*----------------------------------------------------------------------
 * history:
 *
 * ... [rwcox] ...
 *
 * 08 Oct 2004 [rickr]
 *   - AFNI_process_NIML_data() has been broken into many process_NIML_TYPE()
 *     functions.  Functionality has been added for local_domain_parents, such
 *     that surface data is sent per LDP, not per surface.
 * 25 Oct 2004 [rickr]
 *   - use vol2surf for all surfaces now (so nvused is no longer computed)
 *   - in ldp_surf_list, added _ldp suffix and full_label_ldp for clarity
 *   - added functions int_list_posn, slist_choose_surfs,
 *     slist_check_user_surfs and slist_surfs_for_ldp to handle an arbitrary
 *     number of surfaces per LDP
 *   - moved any fprintf off the margin
 *   - pass data/threshold pointers to AFNI_vol2surf_func_overlay()
 *   - prepare for sending data to suma (but must still define new NIML type)
 *     can get data and global threshold from vol2surf
 *   - for users, try to track actual LDP label in full_label_ldp
 * 04 Jan 2005 [rickr]
 *   - process_NIML_SUMA_ixyz: a new surface will replace the existing one
 *   - added g_show_as_popup, receive messages default to terminal
 *   - re-wrote receive messages, only to be shorter
 * 11 Jan 2005 [rickr]
 *   - slist_choose_surfs(): do slist_check_user_surfs() for nsurf == 1
 * 08 Aug 2006 [rickr]
 *   - get spec_file name from suma via surface_specfile_name atr
 * 31 Jan 2008 [RWCox]
 *   - modify process_NIML_AFNI_volumedata() to be able to create a
 *     dataset directly from a <VOLUME_DATA> element, and also so
 *     allow new <VOLUME_DATA_SPARSE> elements.
 * 01 Feb 2008 [RWCox]
 *   - make AFNI_process_NIML_data() visible to the world!
 *----------------------------------------------------------------------*/

/**************************************/
/** global data for NIML connections **/
/**************************************/

/*---------------------------------------*/
/*! Number of streams on which to listen */
#define NUM_NIML   2                        /* 09 Mar 2005: increased to 2 */

/*--------------------------------------*/
/*! Array of streams on which to listen */

static NI_stream_type *ns_listen[NUM_NIML] ;

/*--------------------------------*/
/*! Array of stream names to open */

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

/*----------------------------------------------------------*/
/*! if 1, display some messages as popups, else to terminal */

static int g_show_as_popup = 0 ;     /* 04 Jan 2005 [rickr] */

#undef  SHOW_MESSAGE

/*! Show a string as popup or just to stderr. [10 May 2005] */

#define SHOW_MESSAGE(mmm)                                      \
 do{      if( g_show_as_popup == 1 ) AFNI_popup_message(mmm);  \
     else if( g_show_as_popup == 0 ) fputs(mmm,stderr) ;      } while(0)

/*-------------------------*/

#ifndef SUMA_TCP_PORT
#define SUMA_TCP_PORT 53211
#endif

/*-----------------------------------------------------*/
/* Stuff for an extra NIML port for non-SUMA programs. */

#ifndef NIML_TCP_FIRST_PORT
#define NIML_TCP_FIRST_PORT 53212
#endif

/*-----------------------------------------------------*/

#define EPS 0.01  /* threshold for coordinate changes */

/*--------------------------------------------------------*/
/*! local structure types for organizing surfaces and LDPs */
typedef struct {
   char * idcode_ldp;
   char * label_ldp;
   char   full_label_ldp[64];
   int    nsurf;
   int    sA, sB;
   int    use_v2s;
} ldp_surf_list;

typedef struct {
   ldp_surf_list * list;
   int             nused, nalloc;
} LDP_list;

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

static void    AFNI_niml_atexit( void ) ;
static Boolean AFNI_niml_workproc( XtPointer ) ;
static void    AFNI_niml_redisplay_CB( int,int,void *,void * ) ;
static void    AFNI_niml_viewpoint_CB( int,int,void *,void * ) ;
static void    AFNI_niml_driver( char * , NI_stream_type *, NI_element * ) ;

/*----------------------------------------------------------------------
 * AFNI_process_NIML_data() has been broken in to many process_NIML_TYPE()
 * functions.  Functionality has been added for local_domain_parents, such
 * that surface data is sent per LDP, not per surface.  08 Oct 2004 [rickr]
 *----------------------------------------------------------------------*/

static int     process_NIML_SUMA_ixyz( NI_element * nel, int ct_start ) ;
static int     process_NIML_SUMA_ijk( NI_element * nel, int ct_start ) ;
static int     process_NIML_SUMA_node_normals( NI_element * nel, int ct_start );
static int     process_NIML_SUMA_crosshair_xyz(NI_element * nel) ;
static int     process_NIML_Node_ROI( NI_element * nel, int ct_start ) ;

static int     disp_ldp_surf_list(LDP_list * ldp_list, THD_session * sess);
static int     fill_ldp_surf_list(LDP_list * ldp_list, THD_session * sess,
                                  v2s_plugin_opts * po);
static int     get_ldp_surfs(THD_session *, int, int *, int *, int *);
static int     int_list_posn(int * vals, int nvals, int test_val);
static int     slist_choose_surfs(LDP_list * ldp_list, THD_session * sess,
                                  v2s_plugin_opts * po);
static int     slist_check_user_surfs(ldp_surf_list * lsurf, int * surfs,
                                      v2s_plugin_opts * po);
static int     slist_surfs_for_ldp(ldp_surf_list * lsurf, int * surfs, int max,
                                   THD_session * sess, int debug);

static ldp_surf_list *find_lpd_list(LDP_list *LDP, THD_session *sess, int surf);

/*----------------------------------------------------------------------*/
/* Functions for receiving an AFNI dataset from NIML elements.
 * 10 Mar 2005 - RWCox
------------------------------------------------------------------------*/

static void    process_NIML_AFNI_dataset   ( NI_group *   , int ) ;
static void    process_NIML_AFNI_volumedata( void *       , int ) ;
static void    process_NIML_MRI_IMAGE      ( NI_element * , int ) ;

/************************************************************************/

/*-----------------------------------------------------------------------*/

int AFNI_have_niml( void ){ return started ; }  /* 02 Feb 2007 */

/*-----------------------------------------------------------------------*/
/*! Routine executed at AFNI exit: shutdown all open NI_stream.
-------------------------------------------------------------------------*/

static void AFNI_niml_atexit( void )
{
   int cc ; NI_element *nel=NULL ;
   
STATUS("called AFNI_niml_atexit") ;

   for( cc=0 ; cc < NUM_NIML ; cc++ ){
     if( ns_listen[cc] ) {/* be polite and tell our customers */
       if( nel == NULL ) nel = NI_new_data_element("AuRevoir", 0) ;
       NI_write_element( ns_listen[cc] , nel , NI_BINARY_MODE ) ;
     }
   }

#if 0              /*** this stuff now handled in niml/niml_stream.c ***/
   for( cc=0 ; cc < NUM_NIML ; cc++ )        /* close any open sockets */
     NI_stream_closenow( ns_listen[cc] ) ;
#endif
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Initialize NIML listening.
-------------------------------------------------------------------------*/

void AFNI_init_niml( void )
{
   int cc , ii ;

ENTRY("AFNI_init_niml") ;

   if( started ) EXRETURN ;

   PLUTO_register_workproc( AFNI_niml_workproc , NULL ) ;
#if 1 /* Turned back on to notify SUMA for a graceful exit ZSS Nov 09*/
   atexit( AFNI_niml_atexit ) ;
#endif

   /* initialize status and names of all listening NI_streams */

   for( cc=0 ; cc < NUM_NIML ; cc++ ){
     ns_listen[cc] = NULL ;
     ns_flags[cc]  = 0 ;
   }

   /* 10 Dec 2002: allow user to specify NIML port number */

   cc = GLOBAL_argopt.port_niml ;
   if( cc < 1024 || cc > 65535 ) cc = SUMA_TCP_PORT ;
   sprintf( ns_name[0] , "tcp:host:%d" , cc ) ;

   /* 09 Mar 2005: add extra ports */

   cc = AFNI_numenv( "AFNI_NIML_FIRST_PORT" ) ;
   if( cc < 1024 || cc > 65535 ) cc = NIML_TCP_FIRST_PORT ;
   for( ii=1 ; ii < NUM_NIML ; ii++ )
     sprintf( ns_name[ii] , "tcp:host:%d" , (cc+ii-1) ) ;

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

   /* 04 Jan 2005 [rickr]: check for AFNI_SHOW_SURF_POPUPS */

        if( AFNI_yesenv("AFNI_SHOW_SURF_POPUPS") ) g_show_as_popup =  1 ;
   else if( AFNI_yesenv("AFNI_KILL_SURF_POPUPS") ) g_show_as_popup = -1 ;

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
     NI_stream_closenow( ns_err ) ;
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
   char str[512] ;
   int keep_reading , read_msec ; /* 17 Mar 2005 */

ENTRY("AFNI_niml_workproc") ;

   /** loop over input NIML streams **/

   AFNI_block_rescan(1) ;  /* 10 Nov 2005 */

   for( cc=0 ; cc < NUM_NIML ; cc++ ){

     keep_reading = 0 ;  /* 17 Mar 2005 */

     /* open streams that aren't open */

     if( ns_listen[cc] == NULL && (ns_flags[cc]&FLAG_SKIP)==0 ){
       if(PRINT_TRACING){
         sprintf(str,"call NI_stream_open('%s')",ns_name[cc]) ;
         STATUS(str) ;
       }

       ns_listen[cc] = NI_stream_open( ns_name[cc] , "r" ) ;

       if( ns_listen[cc] == NULL ){
         STATUS("NI_stream_open failed") ;
         ns_flags[cc] = FLAG_SKIP ; continue ;  /* skip to next NIML stream */
       }
       ns_flags[cc]  = FLAG_WAITING ;
     }
     if( ns_listen[cc] == NULL ) continue ; /* this is Ziad's fault! */

     ngood++ ;

     /* now check if stream has gone bad */

     if(PRINT_TRACING){
       sprintf(str,"call NI_stream_goodcheck('%s')",ns_listen[cc]->orig_name);
       STATUS(str) ;
     }

     /* 17 Mar 2005: loopback point if instructed to keep reading */

  Keep_Reading:
     read_msec = (keep_reading) ? 222 : 1 ;  /* 1/3 of the Beast! */

     nn = NI_stream_goodcheck( ns_listen[cc] , 1 ) ;

     if( nn < 0 ){                          /* is bad */
       STATUS("NI_stream_goodcheck was unhappy") ;
       fprintf(stderr,"++ NIML connection closed from %s\n",
                NI_stream_name(ns_listen[cc])               ) ;

       NI_stream_closenow( ns_listen[cc] ) ;
       ns_listen[cc] = NULL ;  /* will be reopened next time */
       ns_flags[cc]  = 0 ;
       keep_reading  = 0 ;
       continue ;              /* skip to next stream  */
     }

     if( nn == 0 ){
       STATUS("NI_stream_goodcheck was neutral") ;
       keep_reading = 0 ;
       continue ;  /* waiting: skip to next stream */
     }

     /* if here, stream is good */

     STATUS("NI_stream_goodcheck was good!") ;

     /* if just became good, print a message */

     if( ns_flags[cc] & FLAG_WAITING ){
       ns_flags[cc] = FLAG_CONNECTED ;
       NI_stream_setbufsize( ns_listen[cc] , 3*NI_BUFSIZE ) ; /* 02 Jun 2005 */
       fprintf(stderr,"++ NIML connection opened from %s\n",
               NI_stream_name(ns_listen[cc])                ) ;
     }

     /* see if there is any data to be read */

     nn = NI_stream_hasinput( ns_listen[cc] , read_msec ) ;

     if( nn > 0 ){                                           /* has data!*/
       STATUS("Reading data!") ;
       ct   = NI_clock_time() ;                           /* start timer */
       nini = NI_read_element( ns_listen[cc] , read_msec ) ;  /* read it */

       if( nini != NULL ){                                  /* handle it */
         if( serrit ) NIML_to_stderr(nini,0) ;

         /*--- a processing instruction? ---*/

         if( NI_element_type(nini) == NI_PROCINS_TYPE ){  /* 17 Mar 2005 */
           NI_procins *npi = (NI_procins *)nini ;

           /* deal with PI's we understand, skip the rest:
               "keep_reading"            ==> loop back to read again immediately
               "pause_reading"           ==> turn "keep_reading" off
               "drive_afni cmd='stuff'"  ==> execute a DRIVE_AFNI command right now */

           if(PRINT_TRACING){
             char sss[256]; sprintf(sss,"Processing instruction: '%s'",npi->name);
             STATUS(sss) ;
           }
           if( strcasecmp(npi->name,"keep_reading") == 0 )
             keep_reading = 1 ;
           else if( strcasecmp(npi->name,"pause_reading") == 0 )
             keep_reading = 0 ;
           else if( strcasecmp(npi->name,"drive_afni") == 0 ){
             char *cmd = NI_get_attribute(npi,"cmd") ;
             if( cmd != NULL ) (void) AFNI_driver(cmd) ;
           }

         /*--- actual data (single element or group)? ---*/

         } else {

           STATUS("Actual NIML data!") ;
           AFNI_process_NIML_data( cc , nini , ct ) ;    /* do something */

         }

         STATUS("Freeing NIML element") ;
         NI_free_element( nini ) ;                           /* trash it */
       }

     } else keep_reading = 0 ;  /* was no data in the read_msec interval */

     if( keep_reading ){
       STATUS("Loopback to Keep_Reading") ;
       goto Keep_Reading ;              /* try to get another input now! */
     }

   } /* end of loop over input NIML streams */

   dont_tell_suma = 0 ;                              /* talk to SUMA */
   dont_overlay_suma = 0 ;
   AFNI_block_rescan(0) ;  /* 10 Nov 2005 */

   /* hopefully the following will never happen */

   if( ngood == 0 ){
     fprintf(stderr,"++ NIML shutting down: no listening sockets\n") ;
     RETURN( True ) ;
   }

   RETURN( False ) ;   /* normal return: this function will be called again */
}

/*----------------------------------------------------------------------*/
/*! Process NIML data.
    - chan is the ns_listen[] index of the stream it came from;
      this is currently not used [22 Dec 2009: it is now].
    - ct_start is the NI_clock_time() [msec] at the start of reading
      this element; a function can use this info to print out some
      timing information on how long it took to get the data and
      do something useful with it
------------------------------------------------------------------------*/

void AFNI_process_NIML_data( int chan, void *nini, int ct_start )
{
   int tt=NI_element_type(nini) ;
   NI_element *nel ;
   char msg[256] ;

ENTRY("AFNI_process_NIML_data") ;

   if( tt < 0 ) EXRETURN ;  /* should never happen */

   if( tt == NI_PROCINS_TYPE ) EXRETURN ;   /* 16 Mar 2005 */

   /*----- we got a group element, so process it -----*/

   if( tt == NI_GROUP_TYPE ){
     NI_group *ngr = (NI_group *) nini ;

     /* 10 Mar 2005: add support for 2 types of groups [RWC] */

     if( strcmp(ngr->name,"AFNI_dataset") == 0 ){

       process_NIML_AFNI_dataset( ngr , ct_start ) ;   /* AFNI dataset header */

     } else if( strncmp(ngr->name,"VOLUME_DATA",11) == 0 ){ /* VOLUME_DATA or */
                                                        /* VOLUME_DATA_SPARSE */
       process_NIML_AFNI_volumedata( ngr , ct_start ) ; /* == AFNI sub-bricks */

     } else {             /* the old way: we don't know about this group name,
                                 so process the elements within it separately */
       int ii ;
       for( ii=0 ; ii < ngr->part_num ; ii++ )
         AFNI_process_NIML_data( chan , ngr->part[ii] , -1 ) ; /* recursion */
     }

     EXRETURN ;
   }

   if( tt != NI_ELEMENT_TYPE ) EXRETURN ;  /* should never happen */

   /*----- if here, have a single data element;
           process the data based on the element name -----*/

   nel = (NI_element *)nini ;

#if 0
 fprintf(stderr,"AFNI received NIML element name=%s\n",nel->name) ;
#endif

   /* broke out as functions, added node_normals         06 Oct 2004 [rickr] */

   if( strcmp(nel->name,"SUMA_ixyz") == 0 ){

     process_NIML_SUMA_ixyz(nel, ct_start) ;  /* surface nodes for a dataset */

   } else if( strcmp(nel->name,"SUMA_ijk") == 0 ){

     process_NIML_SUMA_ijk(nel, ct_start) ;   /* surface triangles from SUMA */

   } else if( strcmp(nel->name,"SUMA_node_normals") == 0 ){

     process_NIML_SUMA_node_normals(nel, ct_start) ;/* node normals for surf */

   } else if( strcmp(nel->name,"SUMA_crosshair_xyz") == 0 ){

     process_NIML_SUMA_crosshair_xyz(nel) ;            /* new focus position */

   } else if( strcmp(nel->name,"Node_ROI") == 0 ){

     process_NIML_Node_ROI(nel, ct_start) ;         /* ROI drawing from SUMA */

   } else if( strncmp(nel->name,"VOLUME_DATA",11) == 0 ){     /* 10 Mar 2005 */

     process_NIML_AFNI_volumedata( nel , ct_start ) ;     /* AFNI sub-bricks */

   } else if( strcmp(nel->name,"MRI_IMAGE") == 0 ){           /* 22 Mar 2005 */

     process_NIML_MRI_IMAGE( nel , ct_start ) ;       /* store as a .1D file */

   } else if( strcmp(nel->name,"3dGroupInCorr_setup") == 0 ){ /* 22 Dec 2009 */

     GICOR_setup_func( ns_listen[chan] , nel ) ;

   } else if( strcmp(nel->name,"3dGroupInCorr_dataset") == 0 ){  /* 23 Dec 2009 */

     GICOR_process_dataset( nel , ct_start ) ;

   } else {
     /*** If here, then name of element didn't match anything ***/
     sprintf(msg,"*** ERROR:\n\n"
                 " Unknown NIML input: \n"
                 "  <%.222s ...> \n"
                 " Ignoring it, and hoping it goes away.\n" ,
                 nel->name) ;
     AFNI_popup_message(msg) ;
   }
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
   static LDP_list ldp_list = { NULL, 0, 0 };   /* 07 Oct 2004 [rickr] */
   Three_D_View *im3d = (Three_D_View *) qqq ;
   THD_3dim_dataset *adset , *fdset ;
   SUMA_irgba *map ;
   float      *rdata, rthresh ;
   int        nmap, nvtot , ct , kldp ;
   int        sA, sB;
   NI_element *nel ;
   char msg[16] ;
   THD_session *sess ;   /* 20 Jan 2004 */

ENTRY("AFNI_niml_redisplay_CB") ;

   /* check inputs for reasonability */

   if( dont_tell_suma            ||
       dont_overlay_suma         ||
       !IM3D_OPEN(im3d)          ||
       !im3d->vinfo->func_visible  ) EXRETURN ;

   sess  = im3d->ss_now   ; if( sess->su_num  == 0    ) EXRETURN ;
   adset = im3d->anat_now ;
   fdset = im3d->fim_now  ; if( fdset         == NULL ) EXRETURN ;

   if( sendit ){
     if( NI_stream_goodcheck(ns_listen[NS_SUMA],1) < 1 ) EXRETURN ;
   }

   /* build a node+color map */

   ct = NI_clock_time() ;

   if ( gv2s_plug_opts.sopt.debug > 0 || gv2s_plug_opts.sopt.dnode >= 0 )
      fprintf(stderr,
           "============================================================\n");

   if( fill_ldp_surf_list(&ldp_list, sess, &gv2s_plug_opts) != 0 )
     EXRETURN ;

   if( gv2s_plug_opts.sopt.debug > 1 )    /* spit out some info */
      disp_ldp_surf_list(&ldp_list, sess);

   /* 07 Oct 2004 [rickr]
      Now we loop over all local domain parents in the current session
      and send the node+color map for each and every one! */
   for( kldp=0 ; kldp < ldp_list.nused ; kldp++ ){

     /* For each LDP, if use_v2s, use it
      *               else if one surface, use afni
      *               else, call vol2surf w/midpoint on the 2 surfaces
      *                                             07 Oct 2004 [rickr] */

     sA = ldp_list.list[kldp].sA;   /* for the sake of laziness */
     sB = ldp_list.list[kldp].sB;

     rdata = NULL;   /* if we want these values, send them to A_vol2surf */
     rthresh = 0.0;

     if( ldp_list.list[kldp].use_v2s ){            /* vol2surf was requested */
       nmap = AFNI_vol2surf_func_overlay(im3d, &map, sA,sB, 0, NULL, &rthresh);
     } else if ( ldp_list.list[kldp].nsurf > 1 ){  /* use v2s with defaults */
       nmap = AFNI_vol2surf_func_overlay(im3d, &map, sA,sB, 1, NULL, &rthresh);
     } else {  /* one surface, no request: use vnlist */
       /* okay, no more vnlist...  :(                   25 Oct 2004 [rickr] */
       /* nmap = AFNI_vnlist_func_overlay( im3d, sA, &map,&nvused ) ;       */

       nmap = AFNI_vol2surf_func_overlay(im3d, &map, sA, -1, 1, NULL, &rthresh);
     }

#if 0
     if( serrit ) fprintf(stderr,"AFNI_niml_redisplay_CB: nmap=%d\n",nmap) ;

     /* we always use v2s now */
     if( ! v2s && ( nmap < 0 || sess->su_surf[sA]->vn == NULL ) )
     {
       if( gv2s_plug_opts.sopt.debug > 0 )
         fprintf(stderr,"** afni: bad surface %d, ret: %d,%p\n", sA, nmap, map);
       continue ; /* this is bad */
     }
#endif

     /* base the error checking on which mapping method was used */
     if( nmap < 0 || (nmap > 0 && !map) ) /* 29 Sep 2004 [rickr] */
     {
       if( gv2s_plug_opts.sopt.debug > 0 )
         fprintf(stderr,"** bad v2s map %d, ret: %d,%p\n", sA, nmap, map);
       continue ; /* this is bad */
     }

     if( nmap > 0 ){  /*--- make a data element with data ---*/

       int *icol ; byte *rcol, *gcol, *bcol, *acol ; int ii ;

       nel = NI_new_data_element( "SUMA_irgba" , nmap ) ;

       /* adding a NULL column creates it, full of zeros */

       NI_add_column( nel , NI_INT  , NULL ) ; icol = nel->vec[0] ;
       NI_add_column( nel , NI_BYTE , NULL ) ; rcol = nel->vec[1] ;
       NI_add_column( nel , NI_BYTE , NULL ) ; gcol = nel->vec[2] ;
       NI_add_column( nel , NI_BYTE , NULL ) ; bcol = nel->vec[3] ;
       NI_add_column( nel , NI_BYTE , NULL ) ; acol = nel->vec[4] ;

#if 0       /* just as a reminder, will we send the data to suma? */
       if( rdata ){
          if( gv2s_plug_opts.sopt.debug > 1 )
            fprintf(stderr,"-d sending data and thresh (%f) to suma\n",rthresh);
          NI_add_column( nel , NI_FLOAT, rdata ) ;
          free(rdata) ;
          rdata = NULL ;
       }
#endif

       for( ii=0 ; ii < nmap ; ii++ ){   /* copy data into element */
         icol[ii] = map[ii].id ;
         rcol[ii] = map[ii].r  ; gcol[ii] = map[ii].g ;
         bcol[ii] = map[ii].b  ; acol[ii] = map[ii].a ;
       }

       free(map) ;       /* data in nel, so don't need map no more */

     } else {         /*--- make an empty data element ---*/

       nel = NI_new_data_element( "SUMA_irgba" , 0 ) ;
     }

     if ( sess->su_surf[sA]->vn )            /* 29 Sep 2004 [rickr] */
       nvtot = sess->su_surf[sA]->vn->nvox ; /* 13 Mar 2002 and 20 Jan 2004 */
     else
       nvtot = -1;      /* make it clear, vol2surf has no interface for this */

     /* 13 Mar 2002: send idcodes of surface and datasets involved */

     NI_set_attribute( nel, "surface_idcode" , sess->su_surf[sA]->idcode ) ;
     NI_set_attribute( nel, "local_domain_parent_ID" ,
                            sess->su_surf[sA]->idcode_ldp ) ;
     NI_set_attribute( nel, "volume_idcode"  , adset->idcode.str ) ;
     NI_set_attribute( nel, "function_idcode", fdset->idcode.str ) ;

     /* 13 Mar 2002: also send the number of voxels in the surface
                     and the number of voxels that were colored in */

     if( nvtot >= 0 ) {
       sprintf(msg,"%d",nvtot) ;
       NI_set_attribute( nel , "numvox_total" , msg ) ;
     }

#if 0  /* we no longer have this number */
     if ( nvused >= 0 ) {
       sprintf(msg,"%d",nvused) ;
       NI_set_attribute( nel , "numvox_used" , msg ) ;
     }
#endif

     /* 22 Oct 2004: pass the threshold (only works for vol2surf now!) */
     NI_set_attribute( nel , "threshold" , MV_format_fval(rthresh)) ;

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

   } /* end of loop over surface in session */

   EXRETURN ;
}

/*--------------------------------------------------------------------*/
/*! Display the contents of the LDP_list
----------------------------------------------------------------------*/
static int disp_ldp_surf_list(LDP_list * ldp_list, THD_session * sess)
{
   ldp_surf_list * slist;
   int             ldp;

ENTRY("disp_ldp_surf_list");

   if(!ldp_list || !sess ) {
      fprintf(stderr,"** disp_ldp_surf_list: bad params (%p,%p)\n",
              ldp_list, sess);
      RETURN(1);
   }

   if( ldp_list->nused <= 0 ){
      fprintf(stderr,"+d LDP_list: empty\n");
      RETURN(0);
   }

   fprintf(stderr,"+d LDP_list:\n"
                  "     (nused, nalloc)       = (%d, %d)\n",
                  ldp_list->nused, ldp_list->nalloc);

   for (ldp = 0, slist = ldp_list->list; ldp < ldp_list->nused; ldp++, slist++ )
      fprintf(stderr,"     (nsurf,sA,sB,use_v2s) = (%d, %d, %d, %d) : '%s'\n",
           slist->nsurf, slist->sA, slist->sB, slist->use_v2s,
           slist->full_label_ldp[0] ? slist->full_label_ldp : slist->label_ldp);
   RETURN(0);
}

/*--------------------------------------------------------------------*/
/*! For this session, return applical v2s LDP surfaces for the given surf.
 *  return 0 on success
----------------------------------------------------------------------*/
static int get_ldp_surfs(THD_session * sess, int surf, int * sA, int * sB,
                                                       int * use_v2s)
{
   /* don't keep allocating and freeing this list */
   static LDP_list   ldp_list = { NULL, 0, 0 };
   ldp_surf_list   * slist = NULL;
   int               verb = gv2s_plug_opts.sopt.debug;

   ENTRY("get_ldp_surfs");

   if( !sess || surf < 0 || !sA ) {
      fprintf(stderr,"** get_LDPS - bad inputs\n");
      RETURN(1);
   }

   /* construct current surface LDP list */
   if( fill_ldp_surf_list(&ldp_list, sess, &gv2s_plug_opts) ) {
      if(verb > 0) fprintf(stderr,"** get_LDPS: failed to fill ldp list\n");
      RETURN(1);
   }

    /* find the ldp struct for surf, and note surface A and maybe B */
   slist = find_lpd_list(&ldp_list, sess, surf);

   if( !slist ) {
      if(verb > 0) fprintf(stderr,"** get_LDPS: no LDP for surf %d\n", surf);
      RETURN(1);
   }

   if(verb > 1) fprintf(stderr,"-- LDP details for surface %d, ldp label %s\n"
                        "   nsurf=%d, sA=%d, sB=%d, use_v2s=%d\n",
                        surf, slist->label_ldp ? slist->label_ldp : "NONE",
                        slist->nsurf, slist->sA, slist->sB, slist->use_v2s);

   /* pass back anything requested */
   if( sA ) *sA = slist->sA;
   if( sB ) *sB = slist->sB;
   if( use_v2s ) *use_v2s = slist->use_v2s;

   RETURN(0);
}

/*--------------------------------------------------------------------*/
/*! For this session, make a list of surfaces per local domain parent.
----------------------------------------------------------------------*/
static int fill_ldp_surf_list(LDP_list * ldp_list, THD_session * sess,
                              v2s_plugin_opts * po)
{
   ldp_surf_list * slist;
   int             surf, ldp;

ENTRY("fill_ldp_surf_list");

   if(!ldp_list || !sess || !po ) {
      fprintf(stderr,"** fill_ldp_surf_list: bad params (%p,%p,%p)\n",
              ldp_list, sess, po);
      RETURN(1);
   }

   if ( sess->su_num <= 0 ) RETURN(0);

   /* first of all, be slightly wasteful and grow the list to su_num length */
   if( ldp_list->nalloc < sess->su_num ){
      ldp_list->nalloc = sess->su_num;
      ldp_list->list = (ldp_surf_list *)realloc(ldp_list->list,
                                 ldp_list->nalloc * sizeof(ldp_surf_list));
      if( !ldp_list->list ){
         fprintf(stderr,"** cannot allocate ldp_list (%d)\n", ldp_list->nalloc);
         exit(1);
      }
   }

   /* now, go through the very difficult task of emptying the ldp_list */
   ldp_list->nused = 0;

   /* next, fill the list with ldp and their surfaces (beware of O(n^2)...) */
   for ( surf = 0; surf < sess->su_num; surf++ ) {
      for ( ldp = 0; ldp < ldp_list->nused; ldp++ )  /* does this ldp exist? */
         if ( strncmp(ldp_list->list[ldp].idcode_ldp,
                      sess->su_surf[surf]->idcode_ldp,32) == 0 )
            break;
      slist = &ldp_list->list[ldp];     /* note where we are */

      if( ldp == ldp_list->nused ){     /* then we have a new ldp */
         slist->idcode_ldp        = sess->su_surf[surf]->idcode_ldp;
         slist->label_ldp         = sess->su_surf[surf]->label_ldp;
         slist->full_label_ldp[0] = '\0';  /* init to empty               */
         slist->nsurf             = 1;     /* adding first surface        */
         slist->sA                = surf;  /* the session's surface index */
         slist->sB                = -1;    /* init to unused              */
         slist->use_v2s           = 0;     /* assume no user request      */

         ldp_list->nused++;     /* we have added a new ldp entry */

         if ( po->sopt.debug > 2 )
            fprintf(stderr,"+d ldp_list add: ldp '%s', surf #%d '%s'\n",
                    slist->label_ldp, surf, sess->su_surf[surf]->label);
      } else {
         slist->nsurf++;
         if( slist->nsurf == 2 ) slist->sB = surf;

         if ( po->sopt.debug > 2 )
            fprintf(stderr,"+d ldp_list add: ldp '%s', surf #%d '%s'\n",
                    slist->label_ldp, surf, sess->su_surf[surf]->label);
      }
   }

   (void)slist_choose_surfs(ldp_list, sess, po);

   RETURN(0);
}

/*---------------------------------------------------------------------*/
/*! Now there are 2 things to clear up for each LDP (when there is more
 *  than 1 surface).  If the user has selected any surfaces for v2s, then
 *  set sA and sB appropriately (and do some error checking).  Also, any
 *  time there are more surfaces than needed, inform the user which will
 *  be used, and which will be ignored.
 */
static int slist_choose_surfs(LDP_list * ldp_list, THD_session * sess,
                                  v2s_plugin_opts * po)
{
   ldp_surf_list * lsurf;
   int           * surfs, max_surf, ldp;
   int             first, surf;
   static int      nwarn=0;
   
ENTRY("slist_choose_surfs");

   /* first, decide on how much memory we need for surfs */
   max_surf = 0;
   for ( ldp = 0; ldp < ldp_list->nused; ldp++ )  /* does this ldp exist? */
      if ( ldp_list->list[ldp].nsurf > max_surf )
         max_surf = ldp_list->list[ldp].nsurf;

   /* and allocate */
   surfs = (int *)malloc(max_surf * sizeof(int));
   if ( !surfs ) {
      fprintf(stderr,"** scs: failed to allocate %d ints\n", max_surf);
      exit(1);
   }

   /* now process each LDP */
   for ( ldp = 0; ldp < ldp_list->nused; ldp++ ) {
      lsurf = &ldp_list->list[ldp];                      /* set pointer   */
      /* if( nsurf < 2 ) continue;
         - must still slist_check_user_surfs()        11 Jan 2004 [rickr] */

      if ( slist_surfs_for_ldp(lsurf, surfs, max_surf, sess, po->sopt.debug) )
         continue;                              /* try with current sa sb */

      slist_check_user_surfs(lsurf, surfs, po);/* proceed even on failure */

      if ( lsurf->sB < 0 ) first = 1;               /* we know nsurf >= 2 */
      else                 first = 2;

      /* if something is discarded and using defaults or debug */
      if ( (first < lsurf->nsurf) && (! lsurf->use_v2s || po->sopt.debug > 1) ){
         if (po->sopt.debug > 1 || nwarn < 2 || !(nwarn % 25)) {
            fprintf(stderr,
              "--------------------------------------------------\n"
              "received too many surfaces for LDP '%s'\n",
              lsurf->full_label_ldp[0] ? 
                           lsurf->full_label_ldp : lsurf->label_ldp);
            for ( surf = 0; surf < first; surf++ )
               fprintf(stderr,"    using    surf #%d : %s\n",
                       surfs[surf], sess->su_surf[surfs[surf]]->label);
            for ( surf = first; surf < lsurf->nsurf; surf++ )
               fprintf(stderr,"    ignoring surf #%d : %s\n",
                       surfs[surf], sess->su_surf[surfs[surf]]->label);
            fprintf(stderr,
              "%s"
              "--------------------------------------------------\n",
              po->sopt.debug > 1 ? "" : 
                  "             Warning shown intermittenlty.\n");
         }
         ++nwarn;
      }
   }

   free(surfs);

   RETURN(0);
}

/*---------------------------------------------------------------------*/
/*! sort slist (and sa,sb) by user selections         21 Oct 2004 [rickr] */
/*  Note that sa and sb are already initialized to the first 2 surfaces.  */
static int slist_check_user_surfs( ldp_surf_list * lsurf, int * surfs,
                                   v2s_plugin_opts * po )
{
   int done = 0, posn;
   ENTRY("slist_check_user_surfs");

   /* the easiest check */
   if ( ! po->ready ) RETURN(0);
   if ( ! po->use0 && ! po->use1 ) RETURN(0);

   if ( po->use0 ) {
      posn = int_list_posn(surfs, lsurf->nsurf, po->s0A);
      if ( posn >= 0 ) {
         done = 1;
         lsurf->use_v2s = 1;                            /* ready for v2s   */
         if ( posn != 0 ) {                             /* swap and set sA */
             lsurf->sA   = surfs[posn];
             surfs[posn] = surfs[0];
             surfs[0]    = lsurf->sA;
         }

         /* check for surfB, but skip the first position (avoid duplicate) */
         if ( po->s0B < 0 )
            lsurf->sB = -1;
         else {
            posn = int_list_posn(surfs+1, lsurf->nsurf-1, po->s0B) + 1;
            if ( posn >= 1 ) {                          /* we've added 1   */
               if ( posn != 1 ) {                       /* swap and set sB */
                   lsurf->sB   = surfs[posn];
                   surfs[posn] = surfs[1];
                   surfs[1]    = lsurf->sB;
               }
            } else                      /* complain, and just use sB as is */
               fprintf(stderr,"** user requested surf pair (%d,%d), but\n"
                           "   cannot find surf %d for LDP '%s'\n"
                           "   --> giving up and using pair (%d,%d)\n",
                           po->s0A, po->s0B, po->s0B, lsurf->label_ldp,
                           lsurf->sA, lsurf->sB);
         }
      }
   }

   /* if we did not apply pair 0, check pair 1 */
   if ( ! done && po->use1 ) {
      posn = int_list_posn(surfs, lsurf->nsurf, po->s1A);
      if ( posn >= 0 ) {
         done = 1;
         lsurf->use_v2s = 1;                            /* ready for v2s   */
         if ( posn != 0 ) {                             /* swap and set sA */
             lsurf->sA   = surfs[posn];
             surfs[posn] = surfs[0];
             surfs[0]    = lsurf->sA;
         }

         /* check for surfB, but skip the first position (avoid duplicate) */
         if ( po->s1B < 0 )
            lsurf->sB = -1;
         else {
            posn = int_list_posn(surfs+1, lsurf->nsurf-1, po->s1B) + 1;
            if ( posn >= 1 ) {
               if ( posn != 1 ) {                       /* swap and set sB */
                   lsurf->sB   = surfs[posn];
                   surfs[posn] = surfs[1];
                   surfs[1]    = lsurf->sB;
               }
            } else                      /* complain, and just use sB as is */
               fprintf(stderr,"** user requested surf pair (%d,%d), but\n"
                              "   cannot find surf %d for LDP '%s'\n"
                              "   --> giving up and using pair (%d,%d)\n",
                              po->s1A, po->s1B, po->s1B, lsurf->label_ldp,
                              lsurf->sA, lsurf->sB);
         }
      }
   }

   if ( po->sopt.debug > 1 ) {
      if ( done )
         fprintf(stderr,"+d user surfs (sA,sB) = (%d,%d) {of %d}, LDP '%s'\n",
                          lsurf->sA, lsurf->sB, lsurf->nsurf, lsurf->label_ldp);
      else
         fprintf(stderr,"+d default    (sA,sB) = (%d,%d) {of %d}, LDP '%s'\n",
                          lsurf->sA, lsurf->sB, lsurf->nsurf, lsurf->label_ldp);
   }

   RETURN(0);
}

/*------------------------------------------------------------------------*/
/*! search list for test_val and return position     21 Oct 2004 [rickr] */
static int int_list_posn(int * vals, int nvals, int test_val)
{
   int c;
ENTRY("int_list_posn");

   for (c = 0; c < nvals; c++)
      if ( vals[c] == test_val )
         RETURN(c);

   RETURN(-1);
}

/*------------------------------------------------------------------------*/
/*! construct list of surfaces for this LDP          21 Oct 2004 [rickr] */
static int slist_surfs_for_ldp( ldp_surf_list * lsurf, int * surfs, int max,
                                THD_session * sess, int debug )
{
   SUMA_surface * ss;
   int            count, surf, len;

ENTRY("slist_surfs_for_ldp");

   if ( debug > 2 )
      fprintf(stderr,"-d ss_for_ldp: LDP '%s', ldp.nsurf = %d, su_num = %d\n",
              lsurf->label_ldp, lsurf->nsurf, sess->su_num);

   count = 0;
   for ( surf = 0; surf < sess->su_num; surf++ )
   {
      ss = sess->su_surf[surf];

      if ( strncmp(lsurf->idcode_ldp, ss->idcode_ldp, 32) == 0 ) {
         if (count >= max) {
            fprintf(stderr,"** failure: ss_for_ldp #1 (%s: %d,%d,%d)\n",
                    lsurf->label_ldp, surf, count, max);
            RETURN(1);
         }

         if ( debug > 2 )
            fprintf(stderr,"-d surfs_for_ldp: surf %d '%s' matches LDP '%s'\n",
                    surf, ss->label, ss->label_ldp);

         /* found a surface for this LDP: note it and check if it is LDP */
         surfs[count++] = surf;
         len = strlen(ss->label_ldp);
         if ( ((len >= 4) && (strncmp(ss->label+len-4, "SAME", 4) == 0)) ||
              (strncmp(lsurf->idcode_ldp, ss->idcode, 64) == 0) ) {
            /* then this surface is also a Local Domain Parent */
            strncpy(lsurf->full_label_ldp, ss->label, 63);
            lsurf->full_label_ldp[63] = '\0';
            if ( strlen(lsurf->full_label_ldp) < (63 - 11) )
               strcat(lsurf->full_label_ldp, " (via SAME)");

            if ( debug > 2 )
               fprintf(stderr,"-d surfs_for_ldp: surf %d '%s' is LDP '%s'\n",
                    surf, ss->label, lsurf->full_label_ldp);
         }
      }
   }

   /* do a little quick verification that the first 1 or 2 match sa and sb */
   if ( lsurf->sA != surfs[0] ) {
      fprintf(stderr,"** failure: ss_for_ldp #2 (%d,%d)\n",lsurf->sA,surfs[0]);
      RETURN(1);
   }
   if ( lsurf->nsurf > 1 && lsurf->sB != surfs[1] ) {
      fprintf(stderr,"** failure: ss_for_ldp #3 (%d,%d)\n",lsurf->sB,surfs[1]);
      RETURN(1);
   }
   /* and that we didn't miss anything */
   if ( count != lsurf->nsurf ) {
      fprintf(stderr,"** failure: ss_for_ldp #4 (%d,%d)\n",count,lsurf->nsurf);
      RETURN(1);
   }

   RETURN(0);
}

/*------------------------------------------------------------------------*/
/*! return the ldp_surf_list pointer corresponding to the given surface
 *------------------------------------------------------------------------*/
static ldp_surf_list *find_lpd_list(LDP_list *LDP, THD_session *sess, int surf)
{
   int lind;

   if( !LDP || !sess || surf < 0 ) return NULL;

   for( lind = 0; lind < LDP->nused; lind++ )
      if( !strncmp(LDP->list[lind].idcode_ldp,
                   sess->su_surf[surf]->idcode_ldp,32) )
         return &LDP->list[lind];       /* found it! */

   return NULL;
}

/*--------------------------------------------------------------------*/
/*! Receives notice when user changes viewpoint position.
----------------------------------------------------------------------*/

static void AFNI_niml_viewpoint_CB( int why, int q, void *qq, void *qqq )
{
   Three_D_View *im3d = (Three_D_View *) qqq ;
   NI_element *nel ;
   NI_group *ngr = NULL;
   float xyz[3], *vv=NULL ;
   static float xold=-666,yold=-777,zold=-888 ;
   int kbest=-1,ibest=-1, i1d=-1;
   char stmp[128]={""};

ENTRY("AFNI_niml_viewpoint_CB") ;

   if( dont_tell_suma                  ||
       !IM3D_OPEN(im3d)                ||
       im3d->ss_now->su_num     == 0   ||
       im3d->ss_now->su_surf[0] == NULL  ) EXRETURN ;

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
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "SUMA_crosshair");

   nel = NI_new_data_element( "SUMA_crosshair_xyz" , 3 ) ;
   NI_add_to_group( ngr, nel); 
   NI_add_column( nel , NI_FLOAT , xyz ) ;

   /* 13 Mar 2002: add idcodes of what we are looking at right now */

   NI_set_attribute( nel,  "surface_idcode", 
                           im3d->ss_now->su_surf[kbest]->idcode ) ;
   /*
      SUMA does not expect to receive volume_idcode attribute
      If it needs it, it will use underlay_idcode instead      Apr. 09 
      
      NI_set_attribute( nel,  "volume_idcode" , im3d->anat_now->idcode.str ) ;

   */
   
   /* 20 Feb 2003: set attribute showing closest node ID */

   if( ibest >= 0 ){
     char str[32] ;
     sprintf(str,"%d",ibest) ;
     NI_set_attribute( nel, "surface_nodeid" , str ) ;
   }

   xold = xyz[0] ; yold = xyz[1] ; zold = xyz[2] ;  /* save old point */

   /* April 2009: Get the values at that voxel from the underlay and overlay */
   nel = NI_new_data_element( "underlay_array", DSET_NVALS(im3d->anat_now));
   NI_set_attribute( nel,  "underlay_idcode" , im3d->anat_now->idcode.str ) ;
   vv = (float*)calloc(DSET_NVALS(im3d->anat_now),sizeof(float));

   i1d = im3d->vinfo->i1 + 
         im3d->vinfo->j2*DSET_NX(im3d->anat_now) +   
         im3d->vinfo->k3*DSET_NX(im3d->anat_now)*DSET_NY(im3d->anat_now);
   if (THD_extract_array( i1d, im3d->anat_now, 0, vv ) < 0) {
      fprintf(stderr,"Failed to get underlay array\n");
   } else {
      NI_add_column(nel, NI_FLOAT, vv); free(vv); vv=NULL;
      sprintf(stmp,"%d %d %d", 
                   im3d->vinfo->i1, im3d->vinfo->j2, im3d->vinfo->k3);
      NI_set_attribute(nel, "vox_ijk", stmp);
      if (HAS_TIMEAXIS(im3d->anat_now)) {
         NI_set_attribute(nel, "has_taxis", "y");
      } else {
         NI_set_attribute(nel, "has_taxis", "n");
      }
   }
   NI_add_to_group( ngr, nel); 
   
   /* get vol2surf underlay time series at this node     29 Apr 2009 [rickr] */
   do { /* cheat: break on error (until data has been allocated) */
      int sA, sB, usev2s;

      if( ibest < 0 || kbest < 0 ) break;       /* nothing to do */

      /* note surfaces to get time series from */
      if( get_ldp_surfs(im3d->ss_now, kbest, &sA, &sB, &usev2s) ) break;

      /* get time series */
      vv = AFNI_v2s_node_timeseries(im3d->ss_now, im3d->anat_now, sA,sB,
                                    ibest, usev2s);
      if( ! vv ) break;

      /* put the time series data into a new element */
      nel = NI_new_data_element("v2s_node_array",DSET_NVALS(im3d->anat_now));
      sprintf(stmp,"%d",ibest);
      NI_set_attribute(nel, "surface_nodeid", stmp);
      NI_add_column(nel, NI_FLOAT, vv); free(vv); vv=NULL;

      /* and add it to the group */
      NI_add_to_group(ngr, nel); 
   } while (0);

   if( sendit )
     NI_write_element( ns_listen[NS_SUMA] , ngr , NI_TEXT_MODE ) ;
   if( serrit )
     NIML_to_stderr(ngr,1) ;

   NI_free_element(ngr) ;
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
   if( im3d->ss_now->su_num     == 0   ||
       im3d->ss_now->su_surf[0] == NULL  ) return ;

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

   for( ks=0 ; ks < im3d->ss_now->su_num ; ks++ ){
     ag  = im3d->ss_now->su_surf[ks]; if( ag == NULL ) continue;
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
     for( ks=0 ; ks < im3d->ss_now->su_num ; ks++ ){
       ag  = im3d->ss_now->su_surf[ks]; if( ag == NULL ) continue;
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
     ag = im3d->ss_now->su_surf[kbest] ; nod = ag->ixyz ;
     ibest = nod[ibest].id ;
   }

   if( kkbest != NULL ) *kkbest = kbest ;
   if( iibest != NULL ) *iibest = ibest ;
   return ;
}

/***********************************************************************/
/**  process NIML elements, one process_NIML_TYPE function per TYPE   **/
/**  - broken out of AFNI_process_NIML_data()    06 Oct 2004 [rickr]  **/
/***********************************************************************/

/*------------------------------------------------------------------------*/
/******** Surface nodes for a dataset *********/
static int process_NIML_SUMA_ixyz( NI_element * nel, int ct_start )
{
   THD_slist_find find ;
   THD_3dim_dataset *dset ;
   THD_session *sess ;      /* 20 Jan 2004 */
   SUMA_surface *ag, *sold;
   int *ic ; float *xc,*yc,*zc ; char *idc , idstr[32] ;
   int num , surf_num , replace ;
   Three_D_View *im3d = AFNI_find_open_controller() ;
   MCW_choose_cbs cbs ;
   int nss = GLOBAL_library.sslist->num_sess ;
   int ct_read = 0, ct_tot = 0 ;
   char msg[1024] ;
   char *scon_tog , *scon_box , *scon_lin , *scon_plm ;

ENTRY("process_NIML_SUMA_ixyz");

   if( dont_hear_suma ) RETURN(0) ;

   if( ct_start >= 0 ) ct_read = NI_clock_time() - ct_start ;

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

     if( nel->vec_len    < 1 )
        fprintf(stderr,"** SUMA_ixyz vec_len    = %d\n",nel->vec_len) ;
     if( nel->vec_filled < 1 )
        fprintf(stderr,"** SUMA_ixyz vec_filled = %d\n",nel->vec_filled) ;
     RETURN(1) ;
   }

   /*-- we need a "volume_idcode" or "dataset_idcode" attribute,
        so that we can attach this surface to a dataset for display;
        if we don't find the attribute or the dataset, then we quit --*/

   idc = NI_get_attribute( nel , "volume_idcode" ) ;
   if( idc == NULL )
     idc = NI_get_attribute( nel , "dataset_idcode" ) ;
   if( idc == NULL && nss > 1 ){
      AFNI_popup_message( "*** ERROR:\n "
                          " SUMA_ixyz surface input\n"
                          " does not identify dataset! \n " ) ;
      RETURN(1) ;
   }
   find = PLUTO_dset_finder(idc) ; dset = find.dset ;
   if( dset == NULL && nss > 1 ){
      sprintf(msg, "*** ERROR:\n\n"
                   " SUMA_ixyz volume dataset idcode is \n"
                   "   %s\n"
                   " Can't find this in AFNI\n", idc ) ;
      AFNI_popup_message( msg ) ;
      RETURN(1) ;
   }

   if( dset != NULL )
     sess = GLOBAL_library.sslist->ssar[find.sess_index] ;  /* 20 Jan 2004 */
   else
     sess = GLOBAL_library.sslist->ssar[0] ;

   /*-- get surface ID code (or make it up) --*/

   idc = NI_get_attribute( nel , "surface_idcode" ) ;
   if( idc == NULL )
     idc = NI_get_attribute( nel , "SUMA_idcode" ) ;
   if( idc == NULL ){
     UNIQ_idcode_fill(idstr) ; idc = idstr ;
   }

   /*-- 14 Aug 2002: we used to trash old surfaces,
                     but now we just accumulate them
        20 Jan 2004: now we put them on the session instead of dataset --*/

   num = sess->su_num ;  /* number of surfaces currently attached */

   /* 19 Aug 2002: check for surface idcode in existing set of surfaces */

   for( surf_num=0 ; surf_num < num ; surf_num++ )
     if( strstr(sess->su_surf[surf_num]->idcode,idc) != NULL ) break ;

   /*-- 04 Jan 2005 [rickr]: allow surface replacement, check num_ixyz
                             for decision on whether to keep triangles --*/
   if( surf_num < num ){
      replace = 1 ;       /* this surface exists, replace it       */
   } else {
      replace = 0 ;
      num++ ;             /* note that there is one more surface now */

      /*-- make space for 1 more set of surface pointers --*/
      sess->su_surf = (SUMA_surface **) realloc(sess->su_surf,
                                             num*sizeof(SUMA_surface *)) ;
   }
   /* note: surf_num is the appropriate index for the received surface */

   /*-- initialize surface that we will fill up here --*/

   ag = SUMA_create_empty_surface() ;

   MCW_strncpy(ag->idcode,idc,32);  /* idc is surface idcode from above */

   /*-- 06 Oct 2004 [rickr]: get idcode of local domain parent (to surface) --*/

   idc = NI_get_attribute( nel , "local_domain_parent_ID" ) ;
   if( idc == NULL ){
     UNIQ_idcode_fill(idstr) ; idc = idstr ;
   }
   MCW_strncpy(ag->idcode_ldp,idc,32) ;

   /*-- 19 Aug 2002: get surface label (or make it up) --*/

   idc = NI_get_attribute( nel , "surface_label" ) ;
   if( idc == NULL )
     idc = NI_get_attribute( nel , "SUMA_label" ) ;

   if( idc != NULL )
     MCW_strncpy(ag->label,idc,64) ;
   else
     sprintf(ag->label,"Surf#%d",num) ;

   /*-- 06 Oct 2004: get label of local domain parent (or make it up) --*/

   idc = NI_get_attribute( nel , "local_domain_parent" ) ;
   if( idc == NULL )
     sprintf(ag->label_ldp,"Surf#%d_local_domain_parent",num) ;
   else
     MCW_strncpy(ag->label_ldp,idc,64) ;

   /*-- 06 Aug 2006 [rickr]: get the spec file for this surface  --*/
   /*   Note that ziad is sending both surface_specfile_name and   */
   /*   surface_specfile_path.  We may prepend the path later.     */

   idc = NI_get_attribute( nel , "surface_specfile_name" ) ;
   if( idc == NULL )
     strcpy(ag->spec_file,"NO_SPEC_FILE");
   else
     MCW_strncpy(ag->spec_file,idc,64) ;  /* THD_MAX_NAME for path, actually */

   /*-- set IDCODEs of surface and of its dataset --*/

   if( dset != NULL )
     MCW_strncpy( ag->idcode_dset , dset->idcode.str , 32 ) ;

   /*-- 06 Sep 2006: set Ziad's stupid colors --*/

   scon_box = NI_get_attribute( nel , "afni_surface_controls_nodes"     ) ;
   scon_lin = NI_get_attribute( nel , "afni_surface_controls_lines"     ) ;
#if 0
   scon_tog = NI_get_attribute( nel , "afni_surface_controls_toggle"    ) ;
   scon_plm = NI_get_attribute( nel , "afni_surface_controls_plusminus" ) ;
#endif

   if( scon_box != NULL || scon_lin != NULL )
     AFNI_init_suma_color( surf_num , scon_box , scon_lin ) ;

   /*-- pointers to the data columns in the NI_element --*/

   ic = (int *)   nel->vec[0] ;  /* index */
   xc = (float *) nel->vec[1] ;  /* x coordinate */
   yc = (float *) nel->vec[2] ;  /* y coordinate */
   zc = (float *) nel->vec[3] ;  /* z coordinate */

   /*-- add nodes to the surface --*/

   SUMA_add_nodes_ixyz( ag , nel->vec_filled , ic,xc,yc,zc ) ;

   /*-- prepare the surface for AFNI --*/

   SUMA_ixyzsort_surface( ag ) ;

   sess->su_num = num ;     /* 14 Aug 2002 (may be same value) */

   /* 04 Jan 2005 [rickr]: if we are replacing the old surface, do it now */

   sold = sess->su_surf[surf_num] ;  /* store the old pointer, in case  */
   sess->su_surf[surf_num] = ag ;    /* set the new pointer, either way */

   if( replace ){
      if( sold->num_ixyz == ag->num_ixyz ){
         /* same number of nodes, move the triangle information */
         ag->num_ijk  = sold->num_ijk  ;     sold->num_ijk  = 0    ;
         ag->nall_ijk = sold->nall_ijk ;     sold->nall_ijk = 0    ;
         ag->ijk      = sold->ijk      ;     sold->ijk      = NULL ;
      } else { /* the number of nodes has changed */
         sprintf(msg,"+++ NOTICE:\n"
               "  Surface '%-14.14s' (#%d) for\n"
               "  session '%.222s'\n"
               "  went from %d nodes to %d nodes\n" ,
               ag->label, surf_num, sess->sessname,
               sold->num_ixyz , ag->num_ixyz) ;
         AFNI_popup_message( msg ) ;  /* include this in clock time */
      }
      /* and finally, delete the old surface */
      SUMA_destroy_surface( sold ) ;
   }

   /*-- we're done! --*/

   if( ct_start >= 0 )                     /* keep track of how */
     ct_tot = NI_clock_time() - ct_start ; /* long this took   */

   /* notify the user */
   sprintf(msg,"\n+++ NOTICE: SUMA_ixyz: %s %d nodes\n"
               "  for surface %-14.14s (#%d),\n"
               "  session %.222s\n" ,
               replace ? "replaced" : "received",
               nel->vec_filled, ag->label, surf_num, sess->sessname ) ;

   if( ct_tot > 0 )
         sprintf(msg+strlen(msg),
                 "  I/O time = %4d ms, Processing = %4d ms\n" ,
                 ct_read , ct_tot-ct_read ) ;

   /* 16 Jun 2003: if need be, switch sessions and anatomy */

/* --- do not jump now, suma may send more data   28 Sep 2006 [rickr] --- */
/*     (suma will send a 'switch underlay' command when ready)            */
#if 0
   if( dset != NULL && find.sess_index != im3d->vinfo->sess_num ){
     cbs.ival = find.sess_index ;
     AFNI_finalize_dataset_CB( im3d->vwid->view->choose_sess_pb ,
                               (XtPointer) im3d ,  &cbs          ) ;
   }
#if 1
   if( dset != NULL && find.dset_index != im3d->vinfo->anat_num ){
     cbs.ival = find.dset_index ;
     AFNI_finalize_dataset_CB( im3d->vwid->view->choose_anat_pb ,
                               (XtPointer) im3d ,  &cbs          ) ;
   }
#endif

#endif  /*---  28 Sep 2006 --- */

   SHOW_MESSAGE(msg) ;

   /* need to make the "Control Surface"
      widgets know about this extra surface */

   AFNI_update_all_surface_widgets( sess ) ;  /* 19 Aug 2002 */

   dont_tell_suma = 1 ;
   PLUTO_dset_redisplay( dset ) ;  /* redisplay windows with this dataset */
   dont_tell_suma = 0 ;

#if 0
   XtSetSensitive( im3d->vwid->imag->pop_sumato_pb, True  ) ;
#endif
   RETURN(0) ;
}

/*------------------------------------------------------------------------*/
/********* surface triangles from SUMA **********/
static int process_NIML_SUMA_ijk( NI_element * nel, int ct_start )
{
   THD_3dim_dataset *dset ;
   SUMA_surface *ag ;
   int *it, *jt , *kt ; char *idc ;
   int num , surf_num , nold ;
   THD_session *sess ;             /* 20 Jan 2004 */
   THD_slist_find find ;
   int ct_read = 0, ct_tot = 0 ;
   char msg[1024] ;

ENTRY("process_NIML_SUMA_ijk");

   if( dont_hear_suma ) RETURN(0) ;

   if( ct_start >= 0 ) ct_read = NI_clock_time() - ct_start ;

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
     RETURN(1) ;
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
      RETURN(1) ;
   }
   find = PLUTO_dset_finder( idc ) ; dset = find.dset ;
   if( dset == NULL ){
      sprintf(msg, "*** ERROR:\n\n"
                   " SUMA_ijk surface dataset idcode is \n"
                   "   %s\n"
                   " Can't find this in AFNI\n", idc ) ;
      AFNI_popup_message( msg ) ;
      RETURN(1) ;
   }
   sess = GLOBAL_library.sslist->ssar[find.sess_index] ;  /* 20 Jan 2004 */

   /*-- session must already have a surface --*/

   num = sess->su_num ;
   if( num == 0 ){
      sprintf(msg,"*** ERROR:\n\n"
                  " SUMA_ijk surface data\n"
                  " received for dataset\n"
                  "  %.222s\n"
                  " before any SUMA_ixyz data! \n" ,
              DSET_FILECODE(dset) ) ;
      AFNI_popup_message( msg ) ;
      RETURN(1) ;
   }

   idc = NI_get_attribute( nel , "surface_idcode" ) ;
   if( idc == NULL )
     idc = NI_get_attribute( nel , "SUMA_idcode" ) ;
   if( idc == NULL ){
      AFNI_popup_message( "*** ERROR:\n\n"
                          " SUMA_ijk surface input\n"
                          " does not have surface idcode! \n" ) ;
      RETURN(1) ;
   }

   /* 14 Aug 2002: find surface idcode in dataset's list of surfaces */

   for( surf_num=0 ; surf_num < num ; surf_num++ )
     if( strstr(sess->su_surf[surf_num]->idcode,idc) != NULL ) break ;

   if( surf_num == num ){
      sprintf(msg, "*** ERROR:\n\n"
                   " SUMA_ijk surface input surface idcode\n"
                   "  %s\n"
                   " does not match any surface in session \n"
                   "  %.222s\n" ,
              idc, sess->sessname ) ;
      AFNI_popup_message( msg ) ;
      RETURN(1) ;
   }

   ag = sess->su_surf[surf_num] ; /* set surface to run with */

   if( ag->num_ijk > 0 ){
      sprintf(msg, "*** WARNING:\n\n"
                   " SUMA_ijk surface input surface idcode\n"
                   "  %s\n"
                   " already has %d triangles in it, and\n"
                   " the SUMA user is trying to add %d more!\n" ,
              idc, ag->num_ijk , nel->vec_filled ) ;
      AFNI_popup_message( msg ) ;
      RETURN(1) ;   /* perhaps we can remove this */
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

   /* let the pitiful user see what just happened */
   if( nold == 0 )
     sprintf(msg,"\n+++ NOTICE: SUMA_ijk: %d triangles attached\n"
                 "  to surface %-14.14s (#%d),\n"
                 "  session %.222s\n" ,
                 nel->vec_filled, ag->label, surf_num, sess->sessname ) ;
   else
     sprintf(msg,"\n+++ NOTICE: SUMA_ijk: %d triangles ADDED\n"
                 "  (was %d) to surface %-14.14s (#%d),\n"
                 "  session %.222s\n" ,
                 nel->vec_filled, nold, ag->label, surf_num, sess->sessname ) ;

   if( ct_tot > 0 ) sprintf(msg+strlen(msg),
                            "  I/O time = %4d ms, Processing = %4d ms\n" ,
                            ct_read , ct_tot-ct_read ) ;

   SHOW_MESSAGE(msg) ;

   dont_tell_suma = 1 ;
   PLUTO_dset_redisplay( dset ) ;  /* redisplay windows with this dataset */
   dont_tell_suma = 0 ;

   RETURN(0) ;
}

/*------------------------------------------------------------------------*/
/********* node normals from SUMA       05 Oct 2004 [rickr] **********/
static int process_NIML_SUMA_node_normals( NI_element * nel, int ct_start )
{
   THD_3dim_dataset *dset ;
   SUMA_surface *ag ;
   float *xc, *yc, *zc ;
   char  *idc ;
   int num , surf_num ;
   THD_session *sess ;
   THD_slist_find find ;
   int ct_read = 0, ct_tot = 0 ;
   char msg[1024] ;

ENTRY("process_NIML_SUMA_node_normals");

   if( dont_hear_suma ) RETURN(0) ;

   if( ct_start >= 0 ) ct_read = NI_clock_time() - ct_start ;

   /*-- check element for suitability --*/

   if( nel->vec_len    <  1        ||  /* empty element?        */
       nel->vec_filled <  1        ||  /* no data was filled in? */
       nel->vec_num    <  3        ||  /* less than 3 columns?  */
       nel->vec_typ[0] != NI_FLOAT ||  /* must be float,float,float */
       nel->vec_typ[1] != NI_FLOAT ||
       nel->vec_typ[2] != NI_FLOAT ){

     AFNI_popup_message( "*** ERROR:\n\n"
                         " SUMA_node_normals data \n"
                         " is badly formatted!\n" ) ;
     RETURN(1) ;
   }

   /*-- we need a "volume_idcode" or "dataset_idcode" attribute,
        so that we can attach this surface to a dataset for display;
        if we don't find the attribute or the dataset, then we quit --*/

   idc = NI_get_attribute( nel , "volume_idcode" ) ;
   if( idc == NULL )
     idc = NI_get_attribute( nel , "dataset_idcode" ) ;
   if( idc == NULL ){
      AFNI_popup_message( "*** ERROR:\n "
                          " SUMA_node_normals input\n"
                          " does not identify dataset! \n" ) ;
      RETURN(1) ;
   }
   find = PLUTO_dset_finder( idc ) ; dset = find.dset ;
   if( dset == NULL ){
      sprintf(msg, "*** ERROR:\n\n"
                   " SUMA_node_normals surface dataset idcode is \n"
                   "   %s\n"
                   " Can't find this in AFNI\n", idc ) ;
      AFNI_popup_message( msg ) ;
      RETURN(1) ;
   }
   sess = GLOBAL_library.sslist->ssar[find.sess_index] ;

   /*-- session must already have a surface --*/

   num = sess->su_num ;
   if( num == 0 ){
      sprintf(msg,"*** ERROR:\n\n"
                  " SUMA_node_normals surface data\n"
                  " received for dataset\n"
                  "  %.222s\n"
                  " before any SUMA_ixyz data! \n" ,
              DSET_FILECODE(dset) ) ;
      AFNI_popup_message( msg ) ;
      RETURN(1) ;
   }

   idc = NI_get_attribute( nel , "surface_idcode" ) ;
   if( idc == NULL )
     idc = NI_get_attribute( nel , "SUMA_idcode" ) ;
   if( idc == NULL ){
      AFNI_popup_message( "*** ERROR:\n\n"
                          " SUMA_node_normals surface input\n"
                          " does not have surface idcode! \n" ) ;
      RETURN(1) ;
   }

   /* find surface idcode in dataset's list of surfaces */

   for( surf_num=0 ; surf_num < num ; surf_num++ )
     if( strstr(sess->su_surf[surf_num]->idcode,idc) != NULL ) break ;

   if( surf_num == num ){
      sprintf(msg, "*** ERROR:\n\n"
                   " SUMA_node_normals surface input surface idcode\n"
                   "  %s\n"
                   " does not match any surface in session \n"
                   "  %.222s\n" ,
              idc, sess->sessname ) ;
      AFNI_popup_message( msg ) ;
      RETURN(1) ;
   }

   ag = sess->su_surf[surf_num] ; /* set surface to run with */

   if( nel->vec_filled != ag->num_ixyz ){
      sprintf(msg, "*** ERROR:\n\n"
                   " SUMA_node_normals surface input surface idcode\n"
                   "  %s\n"
                   " has %d nodes, but has been sent %d normals\n" ,
                   idc, ag->num_ixyz, nel->vec_filled ) ;
      AFNI_popup_message( msg ) ;
      RETURN(1) ;
   }

   if( ag->norm != NULL ){
      sprintf(msg, "*** WARNING:\n\n"
                   " SUMA_node_normals surface input surface idcode\n"
                   "  %s\n"
                   " already has normals associated with it,\n"
                   " replacing old normals with new ones\n" , idc ) ;
      AFNI_popup_message( msg ) ;
   }

   /*-- pointers to the data columns in the NI_element --*/

   xc = (float *) nel->vec[0] ;  /* norm.x     */
   yc = (float *) nel->vec[1] ;  /* norm.y     */
   zc = (float *) nel->vec[2] ;  /* norm.z     */

   /*-- add normals to the surface --*/

   if( SUMA_add_norms_xyz( ag , nel->vec_filled , xc,yc,zc ) ){
      sprintf(msg, "*** ERROR:SUMA_add_norms_ixyz failure!\n");
      AFNI_popup_message( msg ) ;
      RETURN(1) ;
   }

   /*-- we're done! --*/

   if( ct_start >= 0 )                      /* keep track    */
     ct_tot = NI_clock_time() - ct_start ;  /* of time spent */

   sprintf(msg,"\n+++ NOTICE: %d normals attached\n"
               "  to surface %-14.14s (#%d),\n"
               "  session %.222s\n" ,
               nel->vec_filled , ag->label , surf_num , sess->sessname ) ;

   if( ct_tot > 0 ) sprintf(msg+strlen(msg),
                            "  I/O time = %4d ms, Processing = %4d ms\n" ,
                            ct_read , ct_tot-ct_read ) ;

   SHOW_MESSAGE(msg) ;

   dont_tell_suma = 1 ;
   PLUTO_dset_redisplay( dset ) ;  /* redisplay windows with this dataset */
   dont_tell_suma = 0 ;

   RETURN(0) ;
}

/*------------------------------------------------------------------------*/
/********* new focus position **********/
static int process_NIML_SUMA_crosshair_xyz(NI_element * nel)
{
  float *xyz ;

ENTRY("process_NIML_SUMA_crosshair_xyz");

   if( dont_hear_suma ) RETURN(0) ;

   if( nel->vec_len    <  3        ||
       nel->vec_filled <  3        ||
       nel->vec_num    <  1        ||
       nel->vec_typ[0] != NI_FLOAT   ){

     SHOW_MESSAGE( "+++ WARNING:\n\n"
                   " SUMA_crosshair_xyz input \n"
                   " is badly formatted!\n" );
     RETURN(1) ;
   }

   xyz = (float *) nel->vec[0] ;
   dont_tell_suma = 1 ;
   AFNI_jumpto_dicom( AFNI_find_open_controller(), xyz[0],xyz[1],xyz[2] );
   dont_tell_suma = 0 ;
   RETURN(0) ;
}

/*------------------------------------------------------------------------*/
/*********** ROI drawing from SUMA **********/
static int process_NIML_Node_ROI( NI_element * nel, int ct_start )
{
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
   int ct_read = 0 ;
   char msg[1024] ;

ENTRY("process_NIML_Node_ROI");

   if( dont_hear_suma ) RETURN(0) ;

   if( ct_start >= 0 ) ct_read = NI_clock_time() - ct_start ;

STATUS("received Node_ROI element") ;

   if( nel->vec_num    <  2        ||
       nel->vec_typ[0] != NI_INT   ||
       nel->vec_typ[1] != NI_INT     ){

     SHOW_MESSAGE( "+++ WARNING:\n\n"
                   " Node_ROI input \n"
                   " is badly formatted!\n" );
     RETURN(1) ;
   }

   nlist    = (int *) nel->vec[0] ;  /* node list */
   nval     = (int *) nel->vec[1] ;  /* value list */
   num_list = nel->vec_filled ;      /* number of nodes */

   /** get ID codes of surface and anat parents **/

STATUS("checking Node_ROI ID codes") ;

   surf_idc = NI_get_attribute( nel , "domain_parent_idcode" ) ;
   if( surf_idc == NULL )
     surf_idc = NI_get_attribute( nel , "MeshParent_idcode" ) ;
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
                         " set 'MeshParent_idcode'!\n" ) ;
     RETURN(1) ;
   }
   if( dset_idc == NULL ){
     AFNI_popup_message( "*** ERROR:\n\n"
                         " Node_ROI input doesn't\n"
                         " set 'volume_idcode'!\n" ) ;
     RETURN(1) ;
   }
   if( roi_prefix == NULL ){
     AFNI_popup_message( "*** ERROR:\n\n"
                         " Node_ROI input doesn't\n"
                         " set 'target_volume'!\n" ) ;
     RETURN(1) ;
   }
   if( !THD_filename_pure(roi_prefix) ){
     sprintf(msg, "*** ERROR:\n\n"
                  " Node_ROI 'target_volume' prefix \n"
                  "   %s\n"
                  " contains illegal characters!\n" , roi_prefix ) ;
     AFNI_popup_message( msg ) ;
     RETURN(1) ;
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
     RETURN(1) ;
   }
   sess = GLOBAL_library.sslist->ssar[find.sess_index] ;  /* 20 Jan 2004 */

   /** find the surface within this session (from its ID code) **/

STATUS("searching for Node_ROI surface") ;

   num = sess->su_num ;
   if( num == 0 ){
     sprintf(msg,"*** ERROR:\n\n"
                 " Node_ROI data received for dataset\n"
                 "  %.222s\n"
                 " but no surfaces available in session! \n" ,
             DSET_FILECODE(dset_anat) ) ;
     AFNI_popup_message( msg ) ;
     RETURN(1) ;
   }

   for( ks=0 ; ks < num ; ks++ )
     if( strstr(sess->su_surf[ks]->idcode,surf_idc) != NULL ) break ;

   if( ks == num ){
     sprintf(msg, "*** ERROR:\n\n"
                  " Node_ROI surface idcode\n"
                  "  %s\n"
                  " does not match any surface in session \n"
                  "  %.222s\n" ,
             surf_idc, sess->sessname ) ;
     AFNI_popup_message( msg ) ;
     RETURN(1) ;
   }

   ag = sess->su_surf[ks] ; /* set surface to run with */

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

   AFNI_update_all_surface_widgets( sess ) ;
#if 0
   XtSetSensitive( im3d->vwid->imag->pop_sumato_pb, True ) ;
#endif

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
       RETURN(1) ;
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
         RETURN(1) ;
       }
       DESTROY_VVLIST(ag->vv) ; ag->vv = NULL ;
       sprintf(msg,"+++ NOTICE:\n\n"
                   " Node_ROI command is using existing dataset\n"
                   "  %.222s\n" , DSET_FILECODE(dset_func) ) ;
       SHOW_MESSAGE( msg ) ;
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
       RETURN(1) ;
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
     SHOW_MESSAGE( msg ) ;

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

STATUS("redisplay Node_ROI function") ;
   MCW_set_bbox( im3d->vwid->view->see_func_bbox , 1 ) ;
   im3d->vinfo->func_visible = 1 ;
   PLUTO_dset_redisplay( dset_func ) ;  /* redisplay windows with this dataset */
   AFNI_process_drawnotice( im3d ) ;

   RETURN(0) ;
}

/*--------------------------------------------------------------------*/
/*! Construct an AFNI dataset from the group element, and insert it
    into the current session in lowest open controller if it is a
    new dataset.  If it has the same idcode as an old dataset, replace
    that dataset with this one.
----------------------------------------------------------------------*/

static void process_NIML_AFNI_dataset( NI_group *ngr , int ct_start )
{
   Three_D_View *im3d = AFNI_find_open_controller() ;
   THD_3dim_dataset *dset , *old_dset ;
   THD_slist_find find ;
   THD_session *ss ;
   int ii , vv ;

   int ct_read = 0, ct_tot = 0 ;
   char msg[1024] ;

ENTRY("process_NIML_AFNI_dataset") ;

   if( ct_start >= 0 ) ct_read = NI_clock_time() - ct_start ;

   /* convert the group element contents into a dataset */

   dset = THD_niml_to_dataset( ngr , 1 ) ;  /* 1 ==> don't load sub-bricks */
   if( dset == NULL ){
     AFNI_popup_message("\n*** ERROR:\n"
                        " Received bad '<AFNI_dataset ...>'\n"
                        " Discarding data and continuing.\n"  ) ;
     EXRETURN ;
   }

   /* now see if this dataset idcode is already stored in AFNI somewhere */

   find = PLUTO_dset_finder( dset->idcode.str ) ; old_dset = find.dset ;

   if( old_dset == NULL ){     /********* this is a new dataset *************/

     ss = GLOBAL_library.sslist->ssar[im3d->vinfo->sess_num] ;  /* session  */
     ii = ss->num_dsset ;                                       /* row      */
     vv = dset->view_type ;                                     /* and view */

     if( ii >= THD_MAX_SESSION_SIZE ){                 /* session overflow! */
       DSET_delete(dset) ;
       AFNI_popup_message("\n*** ERROR:\n"
                          " Received new dataset but am out of space!\n\n" ) ;
       EXRETURN ;
     }

     ss->dsset[ii][vv] = dset ;     /*** insert dataset into session here ***/
     ss->num_dsset++ ;
     POPDOWN_strlist_chooser ;

   } else {                  /************* have an old dataset *************/

     DSET_delete(dset) ;                             /* delete the new copy */
     dset = old_dset ;     /* instead, will replace contents of old dataset */
   }

   DSET_superlock(dset) ;  /*-- make sure will not be purged from memory! --*/

   /* load any data bricks present in the group element */

   (void)THD_add_bricks( dset , ngr ) ;
   THD_update_statistics( dset ) ;

   /** wrapup **/

   if( ct_start >= 0 )                      /* keep track    */
     ct_tot = NI_clock_time() - ct_start ;  /* of time spent */

   if( old_dset == NULL )
     sprintf(msg,"\n+++ NOTICE: New AFNI dataset received.\n\n") ;
   else
     sprintf(msg,"\n+++ NOTICE: Replacement AFNI dataset received.\n\n") ;

   if( ct_tot > 0 ) sprintf(msg+strlen(msg),
                            "  I/O time = %4d ms, Processing = %4d ms\n" ,
                            ct_read , ct_tot-ct_read ) ;
   SHOW_MESSAGE( msg ) ;
   UNDUMMYIZE ;
   EXRETURN ;
}

/*--------------------------------------------------------------------*/
/*! Process a '<VOLUME_DATA ...>' element to add/replace sub-bricks
    in an AFNI dataset already stored somewhere (identified by
    the idcode).
----------------------------------------------------------------------*/

static void process_NIML_AFNI_volumedata( void *nini , int ct_start )
{
   char *idc ;
   THD_slist_find find ;
   THD_3dim_dataset *dset=NULL ;

   int ct_read = 0, ct_tot = 0 ;
   char msg[1024] ;

ENTRY("process_NIML_AFNI_volumedata") ;

   if( ct_start >= 0 ) ct_read = NI_clock_time() - ct_start ;

   /** find out who owns this otherwise anonymous data **/

                     idc = NI_get_attribute( nini , "domain_parent_idcode" ) ;
   if( idc == NULL ) idc = NI_get_attribute( nini , "AFNI_idcode" ) ;
   if( idc == NULL ) idc = NI_get_attribute( nini , "idcode"      ) ;
   if( idc == NULL ) idc = NI_get_attribute( nini , "target_idcode" ) ;
   if( idc == NULL ) idc = NI_get_attribute( nini , "target_name" ) ;
   if( idc == NULL ) idc = "Anonymous" ;   /* default name */

   /* see if this dataset already exists */

   find = PLUTO_dset_finder(idc) ; dset = find.dset ;

   /* if not, see if we can create a new one from a geometry parent */

   if( dset == NULL ){
     char *gidc ; THD_3dim_dataset *gset ;

                        gidc = NI_get_attribute( nini , "geometry_idcode" ) ;
     if( gidc == NULL ) gidc = NI_get_attribute( nini , "geometry_name" ) ;
     if( gidc == NULL ) gidc = NI_get_attribute( nini , "master_idcode" ) ;
     if( gidc == NULL ) gidc = NI_get_attribute( nini , "master_name" ) ;
     if( gidc == NULL ) gidc = NI_get_attribute( nini , "target_name" ) ;
     if( gidc == NULL ) gidc = "Anonymous" ;

     find = PLUTO_dset_finder(gidc) ; gset = find.dset ;

     if( gset == NULL ){
       gset = THD_open_dataset(gidc) ;
       if( gset == NULL && strchr(gidc,'/') == NULL ){
         char *dnam = get_atlas_dirname() ;
         if( dnam != NULL ){
           char enam[THD_MAX_NAME] ;
           strcpy(enam,dnam); strcat(enam,gidc); gset=THD_open_dataset(enam);
         }
       }
     }

     if( gset == NULL ){
       char *gstr = NI_get_attribute( nini , "geometry_string" ) ;
       if( gstr != NULL ) dset = EDIT_geometry_constructor(gstr,NULL) ;
     }

     if( dset != NULL || gset != NULL ){   /*--- create new dataset ---*/
       Three_D_View *im3d = AFNI_find_open_controller() ;
       THD_session    *ss = im3d->ss_now ; int qs = ss->num_dsset ;

       if( dset == NULL ) dset = EDIT_empty_copy(gset) ;
       DSET_superlock(dset) ;
       if( DSET_NVALS(dset) > 1 ) EDIT_dset_items(dset, ADN_nvals,1, ADN_none) ;

       gidc = NI_get_attribute( nini , "view") ;
       if( gidc != NULL && strcasecmp(gidc,"tlrc") == 0 &&
                           dset->view_type != VIEW_TALAIRACH_TYPE )
         EDIT_dset_items( dset , ADN_view_type,VIEW_TALAIRACH_TYPE , ADN_none );

       gidc = NI_get_attribute( nini , "target_name") ;
       if( gidc == NULL ) gidc = "Anonymous" ;
       EDIT_dset_items( dset , ADN_prefix,gidc , ADN_none ) ;

       POPDOWN_strlist_chooser ;
       if( qs < THD_MAX_SESSION_SIZE ){
         int vv=dset->view_type , qq ;
         if( vv < FIRST_VIEW_TYPE || vv > LAST_VIEW_TYPE ) vv = FIRST_VIEW_TYPE;
         for( qq=FIRST_VIEW_TYPE ; qq <= LAST_VIEW_TYPE ; qq++ )
           ss->dsset[qs][qq] = NULL ;
         ss->dsset[qs][vv] = dset ; ss->num_dsset++ ;
         INFO_message("Added dataset '%s' to controller %s",
                      DSET_FILECODE(dset), AFNI_controller_label(im3d) ) ;
         UNDUMMYIZE ;
       } else {
         DSET_delete(dset) ;
         ERROR_message("Can't create dataset '%s': session array overflow",gidc);
         EXRETURN ;
       }
       DSET_delete(gset) ;  /* don't need this no more */
     } else {
       ERROR_message("Can't find parent '%s' of VOLUME_DATA",gidc) ;
       EXRETURN ;
     }
   }

   /** actually put this data into the dataset **/

   (void)THD_add_bricks( dset , nini ) ;
   THD_update_statistics( dset ) ;

   /** if anyone is looking at this dataset, need to change stuff **/

   AFNI_update_dataset_viewing( dset ) ;  /* 21 Jul 2009 */

   /** wrapup **/

   if( ct_start >= 0 )                      /* keep track    */
     ct_tot = NI_clock_time() - ct_start ;  /* of time spent */

   sprintf(msg,"\n++ NOTICE: Replacement AFNI sub-bricks received.\n") ;

   if( ct_tot > 0 ) sprintf(msg+strlen(msg),
                            "  I/O time = %4d ms, Processing = %4d ms\n" ,
                            ct_read , ct_tot-ct_read ) ;
   SHOW_MESSAGE( msg ) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/
/* If any controller is viewing this dataset, update it's widgets. */

void AFNI_update_dataset_viewing( THD_3dim_dataset *dset ) /* 21 Jul 2009 */
{
   Three_D_View *qq3d ;
   int review , ii , kk ;
   MCW_arrowval *tav ;

   if( !ISVALID_DSET(dset) ) return ;

   /* check all controllers to see if they are looking at this dataset **/

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
     qq3d = GLOBAL_library.controllers[ii] ;
     if( !IM3D_OPEN(qq3d) ) break ;  /* skip this one */

     review = EQUIV_DSETS(dset,qq3d->anat_now) ||   /* same anat? */
              ( qq3d->vinfo->func_visible &&        /* or same func? */
                EQUIV_DSETS(dset,qq3d->fim_now) ) ;

     if( review ){
       DISABLE_LOCK ;
       tav = qq3d->vwid->imag->time_index_av ;
       kk  = DSET_NVALS(dset)-1 ;
       if( kk > tav->imax ){
         tav->fmax = tav->imax = qq3d->vinfo->time_index  = kk ;
         qq3d->vinfo->top_index = kk+1 ;
       }
       AV_assign_ival( tav , kk ) ;
       AFNI_time_index_CB( tav , (XtPointer)qq3d ) ;
       ENABLE_LOCK ;
     }
   }

   return ;
}

/*--------------------------------------------------------------------*/
/*! Process a '<MRI_IMAGE ...>' element to add a .1D file to AFNI's
    library of such things.
----------------------------------------------------------------------*/

static void process_NIML_MRI_IMAGE( NI_element *nel , int ct_start )
{
   MRI_IMAGE *im ;

ENTRY("process_NIML_MRI_IMAGE") ;

   im = niml_to_mri( nel ) ;   /* convert element to an image */

   /* reject bad or overlarge images */

   if( im == NULL ) EXRETURN ;
   if( im->nx < 2 || im->nz > 1 || im->ny > 99 ){ mri_free(im); EXRETURN; }

   /* convert to float, if needed */

   if( im->kind != MRI_float ){
     MRI_IMAGE *qim = mri_to_float(im) ;
     if( qim != NULL ){ mri_free(im); im = qim; }
   }

   /* make up a name, if none provided */

   if( im->name == NULL || im->name[0] == '\0' ){
     static int nnn=1 ; char mmm[32] ;
     sprintf(mmm,"niml_%03d",nnn) ;
     mri_add_name(mmm,im) ;
   }

   /* store in AFNI's list, and vamoose */

   AFNI_add_timeseries( im ) ;
   EXRETURN ;
}
