
#include <stdio.h>
#include <math.h>

#include "thd_iochan.h"
#include "Imon.h"
#include "realtime.h"

extern ART_comm  gAC;

/*----------------------------------------------------------------------
 * history:  see 'Imon -hist'
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

      sprintf( ac->ioc_name , "tcp:%s:%d" , ac->host , AFNI_CONTROL_PORT ) ;

      if( debug > 1 )
         fprintf(stderr,"Opening control channel %s to AFNI.\n",ac->ioc_name) ;

      ac->ioc = iochan_init( ac->ioc_name , "w" ) ;

      if( ac->ioc == NULL ){
         fprintf(stderr,"Can't open control channel %s to AFNI!\a\n",
		 ac->ioc_name) ;
         return -1;
      } else {
         if( debug > 1 )
	     fprintf(stderr,"Entering AFNI_WAIT_CONTROL_MODE.\n") ;

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
         fprintf(stderr,"Control channel to AFNI failed!\n") ;
         IOCHAN_CLOSE(ac->ioc) ;
         ac->mode = 0 ;                    /* disable AFNI */
         return -1;
      } else if( ii > 0 ){
         if( debug > 1 )
         {
            fprintf(stderr,"Control channel connected to AFNI.");
            fprintf(stderr,"  Entering AFNI_OPEN_DATA_MODE.\n") ;
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
         sprintf(ac->ioc_name,"tcp:%s:%d",ac->host,AFNI_TCP_PORT) ;

      strcpy(ac->buf, ac->ioc_name) ;     /* tell AFNI where to read data */

      if( debug > 1 )
         fprintf(stderr,"Sending control information to AFNI:\n%s\n",ac->buf) ;

      ii = iochan_sendall( ac->ioc , ac->buf , strlen(ac->buf)+1 ) ;

      /** A negative return is bad news **/

      if( ii < 0 ){
         fprintf(stderr,"Transmission of control data to AFNI failed!\a\n") ;
         IOCHAN_CLOSE(ac->ioc) ;
         ac->mode = 0 ;
         return -1;
      } else {
	 /* wait for control data to clear */
         while( ! iochan_clearcheck(ac->ioc,2) )
            iochan_sleep(2) ;
         IOCHAN_CLOSE(ac->ioc) ;                 /* close control channel */

         if( debug > 1 )
            fprintf(stderr,"Opening data channel %s to AFNI.\n",ac->ioc_name) ;

         ac->ioc = iochan_init( ac->ioc_name , "w" ) ; /* open data channel */
         if( ac->ioc == NULL ){
            fprintf(stderr,"Can't open data channel %s to AFNI!\a\n",
		    ac->ioc_name) ;
            ac->mode = 0 ;
            return -1;
         } else {
            if( debug > 1 ) fprintf(stderr,"Entering AFNI_CATCHUP_MODE.\n") ;
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
		 "AFNI data channel aborted before any data was sent!\a\n") ;
         IOCHAN_CLOSE( ac->ioc ) ;
         ac->mode = 0 ;
         return -1;
      } else if( ii > 0 ){                      /* can now send data to AFNI! */
         ac->mode = AFNI_CONTINUE_MODE ;

	if ( debug > 1 )
	    fprintf(stderr,"Entering AFNI_CONTINUE_MODE.\n");
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
    char       * image;

    if ( ac->state != ART_STATE_IN_USE )
	return 0;

    if ( (run != prev_run) || (seq != prev_seq) )
    {
	prev_run = run;
	prev_seq = seq;

	image = (char *)ac->param->im_store.x_im;

	if ( image == NULL )
	{
	    fprintf( stderr, "** failure: x_im is NULL\n"
		             "   - closing afni connection\n" );

	    ac->state = ART_STATE_NO_USE;
	    ART_exit();
	    return -1;
	}

	strcpy( image, ART_COMMAND_MARKER );
	image[ART_COMMAND_MARKER_LEN] = '\0';

	if ( iochan_sendall( ac->ioc, image, ac->param->im_store.im_size ) < 0 )
	{
	    fprintf( stderr, "** failed to transmit EOR to afni @ %s\n"
		             "   - closing afni connection\n", ac->host );

	    ac->state = ART_STATE_NO_USE;
	    ART_exit();
	    return -1;
	}

	if ( debug > 1 )
	    fprintf( stderr, "-- EOR: end of run signal (%d,%d)\n", run, seq );

	/* we will need to send new control info to afni */
	ac->state = ART_STATE_TO_SEND_CTRL;

	iochan_sleep(50);			  /* give afni some time    */

	return 1;
    }

    return 0;
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
	fprintf( stderr, "failure: ASV called with invalid arguments!\n" );
	return -1;
    }

    if ( ac->state != ART_STATE_IN_USE )
	return 0;

    /* send one complete volume */

    bytes = ac->param->im_store.im_size;

    for ( slice = 0; slice < v->nim; slice++ )
    {
	image = (char *)ac->param->im_store.im_ary[v->fl_1 + slice];

	if ( ac->swap )		     /* maybe we must swap the bytes first */
	    swap_2( image, bytes/2 );

	if ( iochan_sendall( ac->ioc, image, bytes ) < 0 )
	{
	    fprintf( stderr, "** failed to transmit data to afni @ %s\n"
		             "   - closing afni connection\n", ac->host );

	    ac->state = ART_STATE_NO_USE;
	    ART_exit();
	    return -1;
	}
    }

    if ( debug > 2 )
	fprintf( stderr, "++ sent images from volume (%d:%d) to host %s\n",
		 v->run, v->seq_num, ac->host );

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

    if ( debug > 1 )
	fprintf( stderr, "-- starting I/O to afni\n" );

    if ( ac->state != ART_STATE_TO_OPEN )
	return 0;

    for ( count = 0;
	  (count < num_tries) && (rv == 0) && (ac->mode != AFNI_CONTINUE_MODE);
	  count++ )
    {
	rv = ART_start_io( ac, debug );
	iochan_sleep(100);		/* even on success, give afni time */
    }

    if ( ac->mode == AFNI_CONTINUE_MODE )	/* afni comm is ready! */
    {
	if ( debug > 0 )
	    fprintf( stderr, "++ comm link to afni established at <%s>\n",
		     ac->host );
	ac->state = ART_STATE_TO_SEND_CTRL;
    }
    else if ( (rv == 0) && again )
    {
	if ( debug > 0 )
	{
	    fprintf( stderr, "** failed to connect to afni at '%s' - "
		     "will try again later\n", ac->host );
	}
    }
    else			/* bad news - give up on afni communication */
    {
	fprintf( stderr, "** failed to connect to afni at '%s' - "
		 "GIVING UP!\n", ac->host );
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
    char tbuf[ART_TBUF_LEN];	      /* temporary buffer for adding to buf */
    int  rv;

    if ( (ac == NULL) || (v == NULL) )
    {
	fprintf( stderr, "failure: ASCI called with invalid parameters\n" );
	ac->state = ART_STATE_NO_USE;
	return -1;
    }

    if ( (ac->state != ART_STATE_TO_SEND_CTRL) ||
	 (ac->mode  != AFNI_CONTINUE_MODE) )
	return 0;

    ac->buf[0] = '\0';			    /* init message buffer to empty */

    /* data organization style */
    strcpy( tbuf, "ACQUISITION_TYPE 2D+zt" );
    ART_ADD_TO_BUF( ac->buf, tbuf );

    /* slice order */
    if ( ac->zorder )
	sprintf( tbuf, "ZORDER %s", ac->zorder);
    else
	strcpy( tbuf, "ZORDER alt" );
    ART_ADD_TO_BUF( ac->buf, tbuf );

    /* volume time step */
    sprintf( tbuf, "TR %f", v->geh.tr );
    ART_ADD_TO_BUF( ac->buf, tbuf );

    /* volume dimensions */
    sprintf( tbuf, "XYFOV %f %f %f", fabs(v->geh.nx * v->geh.dx),
	                             fabs(v->geh.ny * v->geh.dy),
	                             fabs(v->nim    * v->z_delta) );
    ART_ADD_TO_BUF( ac->buf, tbuf );

    /* matrix sizes */
    sprintf( tbuf, "XYMATRIX %d %d %d", v->geh.nx, v->geh.ny, v->nim );
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
    {
	char o0 = v->geh.orients[0];	/* for ease of typing later */
	char o2 = v->geh.orients[2];
	char o4 = v->geh.orients[4];
	int  sx, sy, sz;		/* directional sign values  */

	/* Note - the LPI directions are negatives in GEMS 5.x files, */
	/*        so when one of those is the origin, negate it.      */

	/* just note o_i directions in s_i */
	if ( o0 == 'L' || o0 == 'P' || o0 == 'I' ) sx = -1; else sx = 1;
	if ( o2 == 'L' || o2 == 'P' || o2 == 'I' ) sy = -1; else sy = 1;
	if ( o4 == 'L' || o4 == 'P' || o4 == 'I' ) sz = -1; else sz = 1;

	/* notes - we do not use a dz/2 offset, as we have slice locations */
	sprintf(tbuf,"XYZFIRST %f %f %f",
	    sx * v->gex.xorg - v->geh.dx/2.0,
	    sy * v->gex.yorg - v->geh.dy/2.0,
            sz * v->z_first );

	ART_ADD_TO_BUF( ac->buf, tbuf );
    }

    /* BYTEORDER interface - send if swap flag is not set */
    if ( ! ac->swap )
    {
	sprintf( tbuf, "BYTEORDER %s", (ac->byte_order == LSB_FIRST) ?
		 "LSB_FIRST" : "MSB_FIRST" );
	ART_ADD_TO_BUF( ac->buf, tbuf );
    }

    /* DRIVE_AFNI interface - open afni windows */
    {
	char * graph_win;			/* graph window to open */
	char * image_win;			/* image window to open */
	char   o4 = v->geh.orients[4];		/* note last axis       */
	int    nt = ac->param->opts.nt;		/* note user defined nt */

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

    /* pass along any user specified drive command(s) */
    if ( ac->param->opts.drive_cmd != NULL )
    {
	char * cp;

	sprintf( tbuf, "DRIVE_AFNI %s", ac->param->opts.drive_cmd );

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
	fprintf( stderr, "++ dataset control info for afni:\n   %s", ac->buf );

    rv = iochan_sendall( ac->ioc, ac->buf, strlen(ac->buf)+1 );

    if ( rv < 0 )
    {
	fprintf( stderr, "** failure to send control info to afni\n" );
	ac->state = ART_STATE_NO_USE;

	return -1;
    }

    ac->state = ART_STATE_IN_USE;	          /* declaration of success */

    iochan_sleep(50);				  /* give afni some time    */

    return 0;
}


/*----------------------------------------------------------------------
 * Function to be called to make sure the AFNI data channels get closed.
 *----------------------------------------------------------------------
*/
void ART_exit( void )
{
    static int been_here = 0;		/* on an error, we may come back */

    if ( been_here == 0 )
    {
	iochan_close(gAC.ioc);
	fprintf( stderr, "ART_exit: closing afni control channel\n" );
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
    if ( info )
	fputs( info, stdout );

    if ( ac == NULL )
    {
	printf( "ART_idisp_ART_comm: ac == NULL\n" );
	return -1;
    }

    printf( "ART_comm struct at %p :\n"
	    "   (state, mode)   = (%d, %d)\n"
	    "   (use_tcp, swap) = (%d, %d)\n"
	    "   byte_order      = %d\n"
	    "   zorder          = %s\n"
	    "   host            = %s\n"
	    "   ioc_name        = %s\n"
	    "   (ioc, param)    = (0x%p, 0x%p)\n",
	    ac, ac->state, ac->mode, ac->use_tcp, ac->swap, ac->byte_order,
	    CHECK_NULL_STR(ac->zorder), CHECK_NULL_STR(ac->host),
	    CHECK_NULL_STR(ac->ioc_name), ac->ioc, ac->param );

    return 0;
}


/*----------------------------------------------------------------------
 * swap pairs of bytes		- destructive
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

