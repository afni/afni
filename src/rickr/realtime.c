
#include <stdio.h>
#include <math.h>

#include "mrilib.h"     /* from thd_iochan.h    25 Apr 2011 */
#include "Dimon.h"
#include "realtime.h"

extern ART_comm  gAC;

/* maybe we will want this elsewhere at some point */
static char orient_side_rai( float coord, char dir );

/* Keep a copy of a single image/volume as the size to send for end of
 * run (maybe lost at commit 030341058c7c759639ddb26ed910661e73e887d5,
 * 4 Jan 2011, when no longer sending via alloc'd im_store.x_im).  The
 * afni server will attempt to read the same image size that it has been,
 * so sending less would cause it to hang.
 *
 * Allocate xim of size xim_size, required to be >= xim_last_vsize.
 * But always send using xim_last_vsize.       6 Sep 2017 [rickr]
 */
static char * xim = NULL;         /* space for end_of_run            */
static int    xim_size = -1;      /* alloc'd, >= xim_last_vsize      */
static int    xim_last_vsize = 0; /* size of last image sent to afni */


/*----------------------------------------------------------------------
 * history:  see 'Dimon -hist'
 *----------------------------------------------------------------------
*/

/************************************************************************/
/*******   This file was based on rtfeedme.c and rtread.c (with   *******/
/*******   many thanks to R.W. Cox and R. Birn).  It is for use   *******/
/*******   as an optional part of Imon.                           *******/
/************************************************************************/

/*****************************************************************************
  Do I/O startup stuff.

  At any given moment, this routine is in one of a number of modes
  (the AFNI_mode variable).
  The first time in, AFNI_mode == AFNI_OPEN_CONTROL_MODE.  In each mode,
  certain tasks must be accomplished and this program must be synchronized
  with AFNI.  When the necessary deeds are done, the routine advances to
  the next mode.  If the deeds cannot be done when this routine is called,
  then it will stay in the same mode, and the next time it is called it
  will try to do them again.  This routine should be called repeatedly
  until it progresses to the last mode (AFNI_CONTINUE_MODE), which is for
  normal transmission of images (one at a time) to AFNI.

  If an error occurs, so that this program can no longer talk to AFNI, then
  AFNI_mode is set to 0, which means "do nothing further".  The rest of
  the data acquisition software will continue, but these routines will
  be stopped dead.
******************************************************************************/

/*****************************************************************************/
int ART_start_io( ART_comm * ac, int debug )
/*****************************************************************************/
{
   int ii ;

   /***** Check for illegal conditions *****/

   if( ac->mode <= 0 || ac->mode == AFNI_CONTINUE_MODE ) return 0 ;

   /***** If we are at the first time in,
          try to open a control socket to talk to AFNI *****/

   if( ac->mode == AFNI_OPEN_CONTROL_MODE ){

      sprintf( ac->ioc_name , "tcp:%s:%d" , 
               ac->host , get_port_named("AFNI_CONTROL_PORT") ) ;

      if( debug > 1 )
         fprintf(stderr,"ART: Opening control channel %s to AFNI.\n",
                 ac->ioc_name) ;

      ac->ioc = iochan_init( ac->ioc_name , "w" ) ;

      if( ac->ioc == NULL ){
         fprintf(stderr,"ART: ** Can't open control channel %s to AFNI!\a\n",
                 ac->ioc_name) ;
         return -1;
      } else {
         if( debug > 1 )
             fprintf(stderr,"ART: Entering AFNI_WAIT_CONTROL_MODE.\n") ;

         /* begin waiting for AFNI connection */
         ac->mode = AFNI_WAIT_CONTROL_MODE ;
      }
   }

   /***** Check if the control socket is connected to AFNI *****/

   if( ac->mode == AFNI_WAIT_CONTROL_MODE ){

      ii = iochan_writecheck( ac->ioc , 1 ) ;  /* Check; wait at most 1 msec */

      /** if ii == 0, then the channel is still pending,
          so do nothing; otherwise, take some action.    **/

      if( ii < 0 ){
         fprintf(stderr,"ART: ** Control channel to AFNI failed!\n") ;
         IOCHAN_CLOSENOW(ac->ioc) ;
         ac->mode = 0 ;                    /* disable AFNI */
         return -1;
      } else if( ii > 0 ){
         if( debug > 1 )
         {
            fprintf(stderr,"ART: Control channel connected to AFNI.");
            fprintf(stderr,"     Entering AFNI_OPEN_DATA_MODE.\n") ;
         }

         ac->mode = AFNI_OPEN_DATA_MODE ;  /* prepare to send data to AFNI */
      }
   }

   /***** Send the control information, which says
          how we will talk to AFNI in the future (shmem or TCP/IP),
          then close the control channel and open this new data channel *****/

   if( ac->mode == AFNI_OPEN_DATA_MODE ){

      /* decide name of data channel: it can be TCP/IP or shared memory */

      if ( ac->use_tcp )
         sprintf(ac->ioc_name,"tcp:%s:%d",
                 ac->host,get_port_named("AFNI_TCP_PORT")) ;

      strcpy(ac->buf, ac->ioc_name) ;     /* tell AFNI where to read data */

      if( debug > 1 )
         fprintf(stderr,"ART: Sending control information to AFNI:\n%s\n",
                 ac->buf) ;

      ii = iochan_sendall( ac->ioc , ac->buf , strlen(ac->buf)+1 ) ;

      /** A negative return is bad news **/

      if( ii < 0 ){
        char * ebuf = iochan_error_string();
        fprintf(stderr,"ART: Transmission of control data to AFNI failed!\a\n") ;
        fprintf(stderr,"   - iochan error: %s\n", ebuf ? ebuf : "EMPTY" );
        IOCHAN_CLOSENOW(ac->ioc) ;
        ac->mode = 0 ;
        return -1;
      } else {
         /* wait for control data to clear */
         while( ! iochan_clearcheck(ac->ioc,2) )
            iochan_sleep(2) ;
         IOCHAN_CLOSENOW(ac->ioc) ;                 /* close control channel */

         if( debug > 1 )
            fprintf(stderr,"ART: Opening data channel %s to AFNI.\n",
                    ac->ioc_name) ;

         ac->ioc = iochan_init( ac->ioc_name , "w" ) ; /* open data channel */
         if( ac->ioc == NULL ){
            fprintf(stderr,"ART: ** Can't open data channel %s to AFNI!\a\n",
                    ac->ioc_name) ;
            ac->mode = 0 ;
            return -1;
         } else {
            if(debug > 1) fprintf(stderr,"ART: Entering AFNI_CATCHUP_MODE.\n");
            ac->mode = AFNI_CATCHUP_MODE ;
         }
      }
   }

   /***** Wait for the data channel to be connected to AFNI,
          and then send any images that are reconstructed and ready to go *****/

   if( ac->mode == AFNI_CATCHUP_MODE ){

      ii = iochan_writecheck( ac->ioc , 1 ) ;  /* wait at most 1 msec */
      if( ii < 0 ){
         fprintf(stderr,
               "ART: AFNI data channel aborted before any data was sent!\a\n");
         IOCHAN_CLOSENOW(ac->ioc) ;
         ac->mode = 0 ;
         return -1;
      } else if( ii > 0 ){                      /* can now send data to AFNI! */
         ac->mode = AFNI_CONTINUE_MODE ;

        if ( debug > 1 ) fprintf(stderr,"ART: Entering AFNI_CONTINUE_MODE.\n");
      }
   }

   return 0;
}

/*----------------------------------------------------------------------
 * note the end of a run
 *
 * If the (run,seq) pair is new, send a message to afni.
 * There is image space in x_im.
 *----------------------------------------------------------------------
*/
int ART_send_end_of_run( ART_comm * ac, int run, int seq, int debug )
{
    static int   prev_run = -1;
    static int   prev_seq = -1;
    int          bytes = ART_COMMAND_MARKER_LEN+1;

    if ( ac->state != ART_STATE_IN_USE ) {
        if( debug > 3 ) fprintf(stderr,"ART: end of run, but not in use\n");
        return 0;
    }

    /* do nothing if run and seq number have not changed... */
    if ( (run == prev_run) && (seq == prev_seq) ) {
        if( debug > 3 )
           fprintf(stderr,"ART: end of run, but no change in run %d, seq %d\n",
                   run, seq);
        return 0;
    }
    
    /* otherwise, note update for next time */
    prev_run = run;
    prev_seq = seq;

    /* if xim has less space than last image size, allocate more */
    if( xim_size < xim_last_vsize ) {
       if ( debug > 1 ) fprintf(stderr, "ART: xim alloc: %d -> %d bytes\n",
                                xim_size, xim_last_vsize);
       xim_size = xim_last_vsize;
       if( ! xim ) xim = malloc(xim_size * sizeof(char));
       else        xim = realloc(xim, xim_size * sizeof(char));
    }

    /* xim still too small?  afni will expect an image's worth of data */
    if( xim_size < ART_COMMAND_MARKER_LEN+1 ) {
       fprintf(stderr,"** ART: end of run im size only %d bytes\n", xim_size);
       ac->state = ART_STATE_NO_USE;
       ART_exit();
       return -1;
    }

    /* this really needs to be done only once ever, but hey... */
    strcpy(xim, ART_COMMAND_MARKER);
    xim[ART_COMMAND_MARKER_LEN] = '\0';

    /* send only the marker, but of the size of the previous image */
    if ( iochan_sendall( ac->ioc, xim, xim_last_vsize ) < 0 )
    {
        char * ebuf = iochan_error_string();
        fprintf( stderr, "ART: ** failed to transmit EOR to afni @ %s\n"
                         "      - closing afni connection\n", ac->host );
        fprintf(stderr,  "      - iochan error: %s\n",ebuf?ebuf:"EMPTY" );
        ac->state = ART_STATE_NO_USE;
        ART_exit();
        return -1;
    }

    if ( debug > 1 )
        fprintf( stderr, "ART: EOR: end of run signal (%d,%d)\n", run,seq);

    /* we will need to send new control info to afni */
    ac->state = ART_STATE_TO_SEND_CTRL;

    iochan_sleep(50);                         /* give afni some time    */

    return 1;
}


/*----------------------------------------------------------------------
 * send a volume to afni
 *----------------------------------------------------------------------
*/
int ART_send_volume( ART_comm * ac, vol_t * v, int debug )
{
    char * image;
    int    slice, bytes;

    if ( ac == NULL || v == NULL )
    {
        fprintf(stderr, "ART: failure: ASV called with invalid arguments!\n" );
        return -1;
    }

    if ( ac->state != ART_STATE_IN_USE )
        return 0;

    /* send one complete volume */

    bytes = ac->param->fim_o[v->fs_1].nbytes;
    xim_last_vsize = bytes*v->nim;  /* track last vol bytes for xim space */

    /* note that v->nim will be 1 for siemens mosaic, no matter... */
    for ( slice = 0; slice < v->nim; slice++ )
    {
        image = ac->param->fim_o[v->fs_1 + slice].imdata;

        if ( ac->swap )               /* maybe we must swap the bytes first */
            swap_2( image, bytes/2 ); /* assuming DATUM short here ...      */

        if ( iochan_sendall( ac->ioc, image, bytes ) < 0 )
        {
            char * ebuf = iochan_error_string();
            fprintf( stderr, "ART: ** failed to transmit data to afni @ %s\n"
                             "      - closing afni connection\n", ac->host );
            fprintf(stderr,  "      - iochan error: %s\n", ebuf?ebuf:"EMPTY" );

            ac->state = ART_STATE_NO_USE;
            ART_exit();
            return -1;
        }
    }

    if ( debug > 2 )
        fprintf( stderr, "ART: sent images from volume (%d:%d) to host %s\n",
                 v->run, v->seq_num, ac->host );
    if ( debug > 3 )
        fprintf( stderr, "ART: %d slice volume has %d bytes\n", v->nim, bytes);

    return 0;
}


/*----------------------------------------------------------------------
 *
 *----------------------------------------------------------------------
*/
int ART_open_afni_link( ART_comm * ac, int num_tries, int again, int debug )
{
    int rv = 0, count;

    if ( ac == NULL )
        return -1;

    if ( debug > 1 ) fprintf( stderr, "ART: starting I/O to afni\n" );

    if ( ac->state != ART_STATE_TO_OPEN )
        return 0;

    for ( count = 0;
          (count < num_tries) && (rv == 0) && (ac->mode != AFNI_CONTINUE_MODE);
          count++ )
    {
        rv = ART_start_io( ac, debug );
        iochan_sleep(100);              /* even on success, give afni time */
    }

    if ( ac->mode == AFNI_CONTINUE_MODE )       /* afni comm is ready! */
    {
        if ( debug > 0 )
            fprintf( stderr, "ART: comm link to afni established at <%s>\n",
                     ac->host );
        ac->state = ART_STATE_TO_SEND_CTRL;
    }
    else if ( (rv == 0) && again )
    {
        if ( debug > 0 )
        {
            fprintf( stderr, "ART: ** failed to connect to afni at '%s' - "
                     "will try again later\n", ac->host );
        }
    }
    else                        /* bad news - give up on afni communication */
    {
        fprintf( stderr, "\nART: ** failed to connect to afni at '%s' - "
                 "GIVING UP!\n\n", ac->host );
        fprintf( stderr,
            "   Note that it may be necessary to perform either or both of\n"
            "   these operations on the remote computer (running afni):\n"
            "      1. allow access via iptabes (or stop iptables)\n"
            "      2. set AFNI_TRUSTHOST to this hostname or address\n\n");
        ac->state = ART_STATE_NO_USE;
    }

    return rv;
}


/*----------------------------------------------------------------------
 * initialize the AFNI communication struct
 *----------------------------------------------------------------------
*/
int ART_init_AC_struct( ART_comm * ac )
{
    if ( ac == NULL )
        return -1;

    memset(ac, 0, sizeof(ART_comm));

    ac->state       = ART_STATE_NO_USE;
    ac->mode        = 0;
    ac->use_tcp     = 1;
    ac->swap        = 0;
    ac->zorder      = NULL;
    strcpy( ac->host, "localhost" );
    ac->ioc_name[0] = '\0';
    ac->ioc         = NULL;
    ac->param       = NULL;

    return 0;
}


/*----------------------------------------------------------------------
 * send image control information to afni
 *----------------------------------------------------------------------
*/
int ART_send_control_info( ART_comm * ac, vol_t * v, int debug )
{
    char tbuf[ART_TBUF_LEN];          /* temporary buffer for adding to buf */
    int  rv;
    int  nim;                         /* siemens comes from mosaic info */

    if ( (ac == NULL) || (v == NULL) )
    {
        fprintf( stderr, "ART: ** ASCI called with invalid parameters\n" );
        ac->state = ART_STATE_NO_USE;
        return -1;
    }

    if ( (ac->state != ART_STATE_TO_SEND_CTRL) ||
         (ac->mode  != AFNI_CONTINUE_MODE) )
        return 0;

    /* note whether we have a mosaic (nim comes from v->minfo.nslices) */
    if( v->minfo.im_is_volume ) {
        if( debug>1 ) fprintf(stderr, "ART COMM: mosaic/volume of %d slices\n",
                              v->minfo.nslices);
        nim = v->minfo.nslices;
    } else
        nim = v->nim;

    ac->buf[0] = '\0';                      /* init message buffer to empty */

    /* maybe set number of channels */
    if ( ac->param->opts.num_chan > 0 ) {
       sprintf( tbuf, "NUM_CHAN %d", ac->param->opts.num_chan );
       ART_ADD_TO_BUF( ac->buf, tbuf );
    }

    /* data organization style rcr - here */
    /* was 2D+zt, but for num_chan > 1, should be 3D+t            8 Sep 2014 */
    /* NEW: 3D+timing is sorted by volume (see num_chan>1) but has 
            slice timing info (3D+t does not)                     3 Aug 2015 */
    strcpy( tbuf, "ACQUISITION_TYPE 3D+timing" );
    ART_ADD_TO_BUF( ac->buf, tbuf );

    /* slice order */
    if ( ac->zorder )
        sprintf( tbuf, "ZORDER %s", ac->zorder);
    else
        strcpy( tbuf, "ZORDER seq" );   /* back to seq for now  [v3.3 rickr] */
    ART_ADD_TO_BUF( ac->buf, tbuf );

    /* if mosaic with valid timing, apply it                     25 Apr 2011 */
    /* -> set tpattern = explicit and pass times                             */
    if ( v->minfo.im_is_volume &&
         valid_g_siemens_times(v->minfo.nslices, v->geh.tr, 0, 0) )
    {
        int off, nchar, ind;
        off = sprintf( tbuf, "TPATTERN explicit" );
        for( ind = 0; ind < g_siemens_timing_nused; ind++ ) {
            nchar = sprintf(tbuf+off, " %.3f", g_siemens_timing_times[ind]);
            if( nchar < 0 ) break;
            off += nchar;
        }
        ART_ADD_TO_BUF( ac->buf, tbuf );
    } else if ( ac->param->opts.sp ) {
        sprintf( tbuf, "TPATTERN %s", ac->param->opts.sp );
        ART_ADD_TO_BUF( ac->buf, tbuf );
    }

    /* volume time step */
    sprintf( tbuf, "TR %f", v->geh.tr );
    ART_ADD_TO_BUF( ac->buf, tbuf );

    /* possibly pass echo times                     13 Mar 2015 [rickr] */
    if( ac->param->opts.te_list ) {
       sprintf( tbuf, "ECHO_TIMES %s", ac->param->opts.te_list );
       ART_ADD_TO_BUF( ac->buf, tbuf );
    }

    /* volume dimensions */
    /* if the data is oblique, get dz directly from the image structure */
    /*                                              25 Jun 2009 [rickr] */
    /* similarly if volume (AFNI or mosaic)          3 Sep 2013 [rickr] */
    {
       float dz = v->z_delta;
       if( (ac->is_oblique || v->minfo.im_is_volume) &&
           v->image_dz > 0.0 ) dz = v->image_dz;
       sprintf( tbuf, "XYFOV %f %f %f", fabs(v->geh.nx * v->geh.dx),
                                        fabs(v->geh.ny * v->geh.dy),
                                        fabs(nim       * dz       ) );
    }
    ART_ADD_TO_BUF( ac->buf, tbuf );

    /* matrix sizes */
    sprintf( tbuf, "XYMATRIX %d %d %d", v->geh.nx, v->geh.ny, nim );
    ART_ADD_TO_BUF( ac->buf, tbuf );

    /* data type - no mrilib.h, and don't duplicate MRI_TYPE_name list */
    strcpy( tbuf, "DATUM short" );
    ART_ADD_TO_BUF( ac->buf, tbuf );

    /* axes orientations */
    sprintf( tbuf, "XYZAXES %c-%c %c-%c %c-%c",
                           v->geh.orients[0], v->geh.orients[1],
                           v->geh.orients[2], v->geh.orients[3],
                           v->geh.orients[4], v->geh.orients[5] );
    ART_ADD_TO_BUF( ac->buf, tbuf );

    /* volume offsets                          2003 June 25 [rickr] */
    /* now, base on param->ftype               2005 May  16 [rickr] */
    if( ac->param->ftype == IFM_IM_FTYPE_GEMS5 )
    {
        char o0 = v->geh.orients[0];    /* for ease of typing later */
        char o2 = v->geh.orients[2];
        char o4 = v->geh.orients[4];
        int  sx, sy, sz;                /* directional sign values  */

        /* Note - the LPI directions are negatives in GEMS 5.x files, */
        /*        so when one of those is the origin, negate it.      */

        /* rcr - this must rely on 0,0,0 being within the volume - fix it */

        /* just note o_i directions in s_i */
        if ( o0 == 'L' || o0 == 'P' || o0 == 'I' ) sx = -1; else sx = 1;
        if ( o2 == 'L' || o2 == 'P' || o2 == 'I' ) sy = -1; else sy = 1;
        if ( o4 == 'L' || o4 == 'P' || o4 == 'I' ) sz = -1; else sz = 1;

        /* note - we do not use a dz/2 offset, as we have slice locations */
        sprintf(tbuf,"XYZFIRST %f %f %f",
            sx * v->gex.xorg - v->geh.dx/2.0,
            sy * v->gex.yorg - v->geh.dy/2.0,
            sz * v->z_first );

        ART_ADD_TO_BUF( ac->buf, tbuf );
    }
    else /* also do this for AFNI   24 Jan 2013         16 May 2005 */
    {
        char o0 = v->geh.orients[0];    /* for ease of typing later */
        char o2 = v->geh.orients[2];
        char o4 = v->geh.orients[4];

        /* if the files were DICOM, then the origin should be accurate */
        /* (so pick code based on sign, and remove sign)               */

        sprintf(tbuf,"XYZFIRST %f%c %f%c %f%c",
                fabs(v->gex.xorg), orient_side_rai(v->gex.xorg, o0),
                fabs(v->gex.yorg), orient_side_rai(v->gex.yorg, o2),
                fabs(v->z_first),  orient_side_rai(v->z_first,  o4));
        ART_ADD_TO_BUF( ac->buf, tbuf );
    }

    /* BYTEORDER interface - send if swap flag is not set */
    if ( ! ac->swap )
    {
        sprintf( tbuf, "BYTEORDER %s", (ac->byte_order == LSB_FIRST) ?
                 "LSB_FIRST" : "MSB_FIRST" );
        ART_ADD_TO_BUF( ac->buf, tbuf );
    }

    /* OBLIQUE_XFORM interface - send if data is marked as oblique */
    if ( ac->is_oblique )
    {
        sprintf( tbuf,
            "OBLIQUE_XFORM %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f",
                 ac->oblique_xform[0],  ac->oblique_xform[1], 
                 ac->oblique_xform[2],  ac->oblique_xform[3], 
                 ac->oblique_xform[4],  ac->oblique_xform[5], 
                 ac->oblique_xform[6],  ac->oblique_xform[7], 
                 ac->oblique_xform[8],  ac->oblique_xform[9], 
                 ac->oblique_xform[10], ac->oblique_xform[11],
                 ac->oblique_xform[12], ac->oblique_xform[13],
                 ac->oblique_xform[14], ac->oblique_xform[15] );
        ART_ADD_TO_BUF( ac->buf, tbuf );
    }

    /* DRIVE_AFNI interface - open afni windows */
    {
        char * graph_win;                       /* graph window to open */
        char * image_win;                       /* image window to open */
        char   o4 = v->geh.orients[4];          /* note last axis       */
        int    nt = ac->param->opts.nt;         /* note user defined nt */

        if ( (o4 == 'R') || (o4 == 'r') || (o4 == 'L') || (o4 == 'l') )
        {
            graph_win = "sagittalgraph";
            image_win = "sagittalimage";
        }
        else if ( (o4 == 'I') || (o4 == 'i') || (o4 == 'S') || (o4 == 's') )
        {
            graph_win = "axialgraph";
            image_win = "axialimage";
        }
        else
        {
            graph_win = "coronalgraph";
            image_win = "coronalimage";
        }
        
        /* open image and graph window - possibly adding pinnum */
        sprintf(tbuf, "DRIVE_AFNI OPEN_WINDOW %s\n"
                      "DRIVE_AFNI OPEN_WINDOW %s", image_win, graph_win );

        if ( nt > 0 )
            sprintf( tbuf+strlen(tbuf), " pinnum=%d", nt );

        ART_ADD_TO_BUF( ac->buf, tbuf );
    }

    /* pass along any user specified realtime command(s)    v3.2 [rickr] */
    if ( ac->param->opts.rt_list.num )
    {
        string_list * list = &ac->param->opts.rt_list;
        char        * cp;
        int           ns;

        for ( ns = 0; ns < list->num; ns++ )
        {
            strncpy( tbuf, list->list[ns], 256 );

            /* sneaky... change any "\n" pairs to '\n' */
            for ( cp = tbuf; cp < (tbuf + strlen(tbuf) - 1); cp++ )
                if ( cp[0] == '\\' && cp[1] == 'n' )
                {
                    cp[0] = ' ';
                    cp[1] = '\n';
                    cp++;
                }

            ART_ADD_TO_BUF( ac->buf, tbuf );
        }
    }

    /* pass along any user specified drive command(s) */
    if ( ac->param->opts.drive_list.num )
    {
        string_list * list = &ac->param->opts.drive_list;
        char        * cp;
        int           ns;

        for ( ns = 0; ns < list->num; ns++ )
        {
            sprintf( tbuf, "DRIVE_AFNI %s", list->list[ns] );

            /* sneaky... change any "\n" pairs to '\n' */
            for ( cp = tbuf; cp < (tbuf + strlen(tbuf) - 1); cp++ )
                if ( cp[0] == '\\' && cp[1] == 'n' )
                {
                    cp[0] = ' ';
                    cp[1] = '\n';
                    cp++;
                }

            ART_ADD_TO_BUF( ac->buf, tbuf );
        }
    }

    /* pass along any user specified drive_wait command(s) */
    if ( ac->param->opts.wait_list.num )
    {
        string_list * list = &ac->param->opts.wait_list;
        char        * cp;
        int           ns;

        for ( ns = 0; ns < list->num; ns++ )
        {
            sprintf( tbuf, "DRIVE_WAIT %s", list->list[ns] );

            /* sneaky... change any "\n" pairs to '\n' */
            for ( cp = tbuf; cp < (tbuf + strlen(tbuf) - 1); cp++ )
                if ( cp[0] == '\\' && cp[1] == 'n' )
                {
                    cp[0] = ' ';
                    cp[1] = '\n';
                    cp++;
                }

            ART_ADD_TO_BUF( ac->buf, tbuf );
        }
    }

    /* NOTE interface - add a note to the dataset: the actual Imon command */
    {
        int count, len, tot_len;

        /* form-feeds will be replaced with newlines in plug_realtime */
        sprintf( tbuf, "NOTE created remotely via real-time afni\f"
                 "    starting with file : '%s'\f"
                 "    creation command   :",
                 v->first_file );
        tot_len = strlen( tbuf );

        for ( count = 0; count < ac->param->opts.argc; count++ )
        {
            len = strlen( ac->param->opts.argv[count] );

            /* are we out of space? */
            if ( tot_len + len + 5 >= ART_TBUF_LEN )
            {
                strcat( tbuf, " ..." );
                break;
            }

            strcat( tbuf, " " );
            strcat( tbuf, ac->param->opts.argv[count] );
        }
        
        ART_ADD_TO_BUF( ac->buf, tbuf );
    }

    if ( debug > 1 )
        fprintf( stderr,"ART: dataset control info (%d bytes) to afni:\n%s",
                 (int)strlen(ac->buf), ac->buf);
    if ( (debug > 0) && (strlen(ac->buf) > (ART_TBUF_LEN * 0.8)) )
        fprintf(stderr,"ART: ** warning: ac->buf len uses %d of %d bytes\n",
                (int)strlen(ac->buf), ART_TBUF_LEN);

    rv = iochan_sendall( ac->ioc, ac->buf, strlen(ac->buf)+1 );

    if ( rv < 0 )
    {
        char * ebuf = iochan_error_string();
        fprintf( stderr, "ART: ** failure to send control info to afni\n" );
        fprintf( stderr, "      - iochan error: %s\n", ebuf ? ebuf : "EMPTY" );
        ac->state = ART_STATE_NO_USE;

        return -1;
    } else if ( debug > 2 )
        fprintf(stderr,"ART: control info sent OK, rv = %d\n", rv);

    ac->state = ART_STATE_IN_USE;                 /* declaration of success */

    iochan_sleep(50);                             /* give afni some time    */

    return 0;
}


/*----------------------------------------------------------------------
 * Function to be called to make sure the AFNI data channels get closed.
 *----------------------------------------------------------------------
*/
void ART_exit( void )
{
    static int been_here = 0;           /* on an error, we may come back */

    if ( been_here == 0 )
    {
        iochan_close(gAC.ioc);
        fprintf( stderr, "ART: exit: closing afni control channel\n" );
        been_here = 1;
    }

    return;
}


/*----------------------------------------------------------------------
 * display ART_comm structure contents
 *----------------------------------------------------------------------
*/
int ART_idisp_ART_comm( char * info, ART_comm * ac )
{
    FILE * fp = stderr;
    int i, j;

    if ( info )
        fputs( info, fp );

    if ( ac == NULL )
    {
        fprintf( fp, "ART: idisp_ART_comm: ac == NULL\n" );
        return -1;
    }

    fprintf( fp,
            "ART_comm struct :\n"
            "   (state, mode)   = (%d, %d)\n"
            "   (use_tcp, swap) = (%d, %d)\n"
            "   byte_order      = %d\n"
            "   is_oblique      = %d\n"
            "   zorder          = %s\n"
            "   host            = %s\n"
            "   ioc_name        = %s\n",
            ac->state, ac->mode, ac->use_tcp, ac->swap, ac->byte_order,
            ac->is_oblique,
            CHECK_NULL_STR(ac->zorder), CHECK_NULL_STR(ac->host),
            CHECK_NULL_STR(ac->ioc_name) );

    if( ac->is_oblique ) {
        fprintf(fp, "   oblique_xform:\n");
        for(i = 0; i < 4; i++) {
            fprintf(fp, "       ");
            for(j=0; j<4; j++)
                fprintf(fp,"%10.4f  ", ac->oblique_xform[4*i+j]);
            fputc('\n', fp);
        }
    }

    return 0;
}


/*----------------------------------------------------------------------
 * swap pairs of bytes          - destructive
 *----------------------------------------------------------------------
*/
int swap_2( void * ptr, int npairs )
{
    unsigned char * addr = ptr;
    int             count;

    for ( count = 0; count < npairs; count++ )
    {
        addr[0] ^= addr[1]; addr[1] ^= addr[0]; addr[0] ^= addr[1];
        addr += 2;
    }

    return 0;
}


/* given a coord and direction character, return the side the coord is on */
/* (assume RAI) */
static char orient_side_rai( float coord, char dir )
{
    int d = toupper(dir);

    if ( d == 'R' || d == 'L' ) return( coord < 0 ? 'R' : 'L' );
    if ( d == 'A' || d == 'P' ) return( coord < 0 ? 'A' : 'P' );
    if ( d == 'I' || d == 'S' ) return( coord < 0 ? 'I' : 'S' );

    return 'R';  /* default */
}

