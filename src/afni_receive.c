/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#undef MAIN

#include "afni.h"

/*-------------------------------------------------------------------*/
/*! Set up to have AFNI send data to a receiver:

     rmask = bitwise OR (|) mask of RECEIVE_*_MASK (cf. afni.h)

     cb    = callback function to receive data; will be called like
               cb( int why , int np , void *vp , void *cb_data )
             where why = a RECEIVE_* code (cf. below)
                   np  = count of data in vp (may be 0)
                   vp  = pointer to data being sent (may be NULL)
               cb_data = pointer passed into this routine

     cbname = string to identify callback func (for debugging)

     why = RECEIVE_VIEWPOINT --> np = 3, vp = int *, pointing to
                                 array of dataset voxel indices just
                                 jumped to; vp[0] = x index, etc.

     why = RECEIVE_REDISPLAY --> the user did something to cause
                                 an image redisplay besides changing
                                 the viewpoint. For this call, np and
                                 vp are unused.

     why = RECEIVE_FUNCDISPLAY --> the user did something to cause
                                   the function display to change.
                                   For this call, np and vp are unused.

     why = RECEIVE_POINTS --> np = number of points drawn
                              vp = int **, pointer to array of arrays
                                   of dataset voxel indices:
                                    vp[0][i] = x index of point i
                                    vp[1][i] = y index of point i
                                    vp[2][i] = z index of point i,
                                    vp[3][0] = sending mode
                                   for i=0..np-1

     why = RECEIVE_DRAWNOTICE --> the user drew something and that
                                  information was sent to a receiver.
                                  This call will occur AFTER all the
                                  RECEIVE_POINTS calls have been done.
                                  If no RECEIVE_POINTS calls are made,
                                  then no RECEIVE_DRAWNOTICE calls will
                                  be made either.  For this call,
                                  np and vp are unused.

     why = RECEIVE_DSETCHANGE --> the user did something (like rescan
                                  a session) that may have changed
                                  dataset pointers.  The receiving
                                  routine should use dataset idcodes
                                  to re-find the correct dataset
                                  pointers.  For this call, np and vp
                                  are unused.

     why = RECEIVE_CLOSURE --> the user closed the controller window,
                               which means that no more data will
                               be coming from it -- even if it is
                               reopened, AFNI_receive_init must be
                               called again.  For this call, np and
                               vp are unused.

     why = RECEIVE_ALTERATION --> the user changed something in the
                                  controller -- the dataset, the
                                  time index, the resampling, ....
                                  This is basically a warning
                                  message.  For this call, np and
                                  vp are unused.

     why = RECEIVE_TIMEINDEX --> the time index changed in im3d;
                                 for this call, np,vp are unused.
                                 The new time index is in
                                   im3d->vinfo->time_index

   This function returns a non-negative int if all is OK -- this
   value is the "key" that is used in calls to AFNI_receive_control
   when you want to manipulate the status of this connection.

   This function returns -1 if an error occurs.

   Modified 29 Mar 1999 to allow for multiple receivers
   (but drawing can only be done in one mode).
---------------------------------------------------------------------*/

int AFNI_receive_init( Three_D_View *im3d , int rmask ,
                       gen_func *cb , void *cb_data  , char *cbname )
{
   int ir ;

ENTRY("AFNI_receive_init") ;

   /* check for invalid entries */

   if( ! IM3D_VALID(im3d)            ||               /* no good? */
       cb == NULL                    ||               /* no receiver? */
       (rmask & RECEIVE_ALL_MASK) == 0 ) RETURN(-1) ; /* no action? */

   for( ir=0 ; ir < im3d->vinfo->num_receiver ; ir++ )
     if( im3d->vinfo->receiver[ir] == NULL ) break ;

   if( ir == im3d->vinfo->num_receiver ){
#if 0
fprintf(stderr,"AFNI_receive_init AFREALL() with ir=%d num_receiver=%d\n",
        ir,im3d->vinfo->num_receiver ) ;
#endif
      im3d->vinfo->receiver = AFREALL( im3d->vinfo->receiver, AFNI_receiver *, ir+1 );
      im3d->vinfo->num_receiver ++ ;
   }
   im3d->vinfo->receiver[ir] = AFMALL( AFNI_receiver, sizeof(AFNI_receiver)) ;

   im3d->vinfo->receiver[ir]->receiver_func = cb ;
   im3d->vinfo->receiver[ir]->receiver_mask = rmask ;
   im3d->vinfo->receiver[ir]->receiver_data = cb_data ;

   im3d->vinfo->receiver[ir]->receiver_funcname =
     strdup( (cbname != NULL) ? cbname : "[unknown func]" ) ;

   AFNI_toggle_drawing( im3d ) ;

   RETURN(ir) ;  /* 15 Jun 1999: used to return 0, which is not correct! */
}

/*-------------------------------------------------------------------------*/
/*! Turn off all reception for this viewer.
---------------------------------------------------------------------------*/

void AFNI_receive_destroy( Three_D_View * im3d )
{
   int ir ;

ENTRY("AFNI_receive_destroy") ;

   if( !IM3D_VALID(im3d) ) EXRETURN ;

   if( im3d->vinfo->receiver != NULL && im3d->vinfo->num_receiver > 0 ){

      for( ir=0 ; ir < im3d->vinfo->num_receiver ; ir++ ){

         if( im3d->vinfo->receiver[ir] != NULL ){
#if 0
            im3d->vinfo->receiver[ir]->receiver_func(
                   RECEIVE_CLOSURE , 0 , NULL ,
                   im3d->vinfo->receiver[ir]->receiver_data ) ;
#else
            AFNI_CALL_VOID_4ARG( im3d->vinfo->receiver[ir]->receiver_func ,
                                 int,RECEIVE_CLOSURE , int,0 ,
                                 void *,NULL ,
                                 void *,im3d->vinfo->receiver[ir]->receiver_data ) ;
#endif

            if( im3d->vinfo->receiver[ir]->receiver_funcname != NULL )
              free( im3d->vinfo->receiver[ir]->receiver_funcname ) ;
            free( im3d->vinfo->receiver[ir] ) ;
            im3d->vinfo->receiver[ir] = NULL ;
         }
      }

      im3d->vinfo->num_receiver    = 0 ;
      im3d->vinfo->drawing_enabled = 0 ;
      im3d->vinfo->drawing_mode    = DRAWING_LINES ;  /* default */
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------*/
/*! Control how the receiver works:
    - code = indicates what action is
    - arg  = extra information, if code needs it

  This function returns 0 if all is OK, and returns -1 if an error
  occurs.
---------------------------------------------------------------------*/

int AFNI_receive_control( Three_D_View *im3d, int key, int code, void *arg )
{
ENTRY("AFNI_receive_control") ;

   /* check input for OK-osity */

   if( ! IM3D_VALID(im3d) || im3d->vinfo->receiver == NULL ) RETURN(-1) ;
   if( key < 0 || key >= im3d->vinfo->num_receiver )         RETURN(-1) ;
   if( im3d->vinfo->receiver[key] == NULL )                  RETURN(-1) ;

   /* take appropriate actions */

   switch( code ){

      default: RETURN(-1) ;

      case DRAWNOTICE_STARTUP:{    /* 30 Mar 1999 */
         im3d->vinfo->receiver[key]->receiver_mask |= RECEIVE_DRAWNOTICE_MASK ;
      }
      break ;

      case DRAWNOTICE_SHUTDOWN:{   /* 30 Mar 1999 */
         im3d->vinfo->receiver[key]->receiver_mask &= (RECEIVE_ALL_MASK - RECEIVE_DRAWNOTICE_MASK) ;
      }
      break ;

      case DSETCHANGE_STARTUP:{    /* 31 Mar 1999 */
         im3d->vinfo->receiver[key]->receiver_mask |= RECEIVE_DSETCHANGE_MASK ;
      }
      break ;

      case DSETCHANGE_SHUTDOWN:{   /* 30 Mar 1999 */
         im3d->vinfo->receiver[key]->receiver_mask &= (RECEIVE_ALL_MASK - RECEIVE_DSETCHANGE_MASK) ;
      }
      break ;

      case DRAWING_STARTUP:{
         im3d->vinfo->receiver[key]->receiver_mask |= RECEIVE_DRAWING_MASK ;
         AFNI_toggle_drawing( im3d ) ;
      }
      break ;

      case DRAWING_SHUTDOWN:{
         im3d->vinfo->receiver[key]->receiver_mask &= (RECEIVE_ALL_MASK - RECEIVE_DRAWING_MASK) ;
         AFNI_toggle_drawing( im3d ) ;
      }
      break ;

      case DRAWING_OVCINDEX:{
         int ind = (int) arg ;

         if( ind <= 0 || ind >= im3d->dc->ovc->ncol_ov ){
            RETURN(-1) ;
         } else {
            Pixel ppp = im3d->dc->ovc->pix_ov[ind] ;
            drive_MCW_imseq( im3d->s123, isqDR_button2_pixel, (XtPointer)ppp ) ;
            drive_MCW_imseq( im3d->s231, isqDR_button2_pixel, (XtPointer)ppp ) ;
            drive_MCW_imseq( im3d->s312, isqDR_button2_pixel, (XtPointer)ppp ) ;
            im3d->vinfo->drawing_pixel = ppp ;
         }
      }
      break ;

      case DRAWING_X11PIXEL:{
         Pixel ppp = (Pixel) arg ;
         drive_MCW_imseq( im3d->s123, isqDR_button2_pixel, (XtPointer)ppp ) ;
         drive_MCW_imseq( im3d->s231, isqDR_button2_pixel, (XtPointer)ppp ) ;
         drive_MCW_imseq( im3d->s312, isqDR_button2_pixel, (XtPointer)ppp ) ;
         im3d->vinfo->drawing_pixel = ppp ;
      }
      break ;

      case DRAWING_LINES:
      case DRAWING_FILL:
      case DRAWING_POINTS:
      case DRAWING_NODRAW:{
         im3d->vinfo->drawing_mode = code ;
         drive_MCW_imseq( im3d->s123, isqDR_button2_mode, (XtPointer)code ) ;
         drive_MCW_imseq( im3d->s231, isqDR_button2_mode, (XtPointer)code ) ;
         drive_MCW_imseq( im3d->s312, isqDR_button2_mode, (XtPointer)code ) ;
      }
      break ;

      case DRAWING_LINEWIDTH:{    /* 08 Oct 2002 */
        int ww = (int) arg ;
        if( ww >= 0 ){
          drive_MCW_imseq( im3d->s123, isqDR_button2_width, (XtPointer)ww ) ;
          drive_MCW_imseq( im3d->s231, isqDR_button2_width, (XtPointer)ww ) ;
          drive_MCW_imseq( im3d->s312, isqDR_button2_width, (XtPointer)ww ) ;
        }
      }
      break ;

      case VIEWPOINT_STARTUP:{
         im3d->vinfo->receiver[key]->receiver_mask |= RECEIVE_VIEWPOINT_MASK ;
      }
      break ;

      case VIEWPOINT_SHUTDOWN:{
         im3d->vinfo->receiver[key]->receiver_mask &= (RECEIVE_ALL_MASK - RECEIVE_VIEWPOINT_MASK) ;
      }
      break ;

      case REDISPLAY_STARTUP:{
         im3d->vinfo->receiver[key]->receiver_mask |= RECEIVE_REDISPLAY_MASK ;
      }
      break ;

      case REDISPLAY_SHUTDOWN:{
         im3d->vinfo->receiver[key]->receiver_mask &= (RECEIVE_ALL_MASK - RECEIVE_REDISPLAY_MASK) ;
      }
      break ;

      case FUNCDISPLAY_STARTUP:{
         im3d->vinfo->receiver[key]->receiver_mask |= RECEIVE_FUNCDISPLAY_MASK ;
      }
      break ;

      case FUNCDISPLAY_SHUTDOWN:{
         im3d->vinfo->receiver[key]->receiver_mask &= (RECEIVE_ALL_MASK - RECEIVE_FUNCDISPLAY_MASK) ;
      }
      break ;

      case OVERLAY_STARTUP:{
         im3d->vinfo->receiver[key]->receiver_mask |= RECEIVE_OVERLAY_MASK ;
      }
      break ;

      case OVERLAY_SHUTDOWN:{
         im3d->vinfo->receiver[key]->receiver_mask &= (RECEIVE_ALL_MASK - RECEIVE_OVERLAY_MASK) ;
      }
      break ;

      case TIMEINDEX_STARTUP:{
         im3d->vinfo->receiver[key]->receiver_mask |= RECEIVE_TIMEINDEX_MASK ;
      }
      break ;

      case TIMEINDEX_SHUTDOWN:{
         im3d->vinfo->receiver[key]->receiver_mask &= (RECEIVE_ALL_MASK - RECEIVE_TIMEINDEX_MASK) ;
      }
      break ;

      case EVERYTHING_SHUTDOWN:{
         im3d->vinfo->receiver[key]->receiver_mask = 0 ;
         AFNI_toggle_drawing( im3d ) ;
      }
      break ;

   } /* end of switch on codes */

   /* possibly remove this receiver, and maybe even all receivers */

   if( im3d->vinfo->receiver[key]->receiver_mask == 0 ){   /* receiving nothing at all? */
      int ir , nn=0 ;

      if( im3d->vinfo->receiver[key]->receiver_funcname != NULL )
        free( im3d->vinfo->receiver[key]->receiver_funcname ) ;

      free( im3d->vinfo->receiver[key] ) ;                 /* toss this one fer shur */
      im3d->vinfo->receiver[key] = NULL ;

      for( ir=0 ; ir < im3d->vinfo->num_receiver ; ir++ )  /* see if any are left */
         if( im3d->vinfo->receiver[ir] != NULL ) nn++ ;

      if( nn == 0 ) im3d->vinfo->num_receiver = 0 ;        /* if not, toss them all */
   }

   RETURN(0) ;
}

/*-------------------------------------------------------------------
   Turn the drawing on or off for the given controller
---------------------------------------------------------------------*/

void AFNI_toggle_drawing( Three_D_View * im3d )
{
   int turn_on = 0 ;

ENTRY("AFNI_toggle_drawing") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   /* count up how many receivers want the drawing */

   if( im3d->vinfo->receiver != NULL ){
      int ir ;
      for( ir=0 ; ir < im3d->vinfo->num_receiver ; ir++ )
         if( im3d->vinfo->receiver[ir] != NULL &&
             (im3d->vinfo->receiver[ir]->receiver_mask & RECEIVE_DRAWING_MASK) )
            turn_on++ ;
   }
   im3d->vinfo->drawing_enabled = (turn_on != 0) ;

   if( ! im3d->vinfo->drawing_enabled ){

   /*-- quench the flames --*/

      drive_MCW_imseq( im3d->s123 , isqDR_button2_disable , NULL ) ;
      drive_MCW_imseq( im3d->s231 , isqDR_button2_disable , NULL ) ;
      drive_MCW_imseq( im3d->s312 , isqDR_button2_disable , NULL ) ;

      drive_MCW_grapher( im3d->g123 , graDR_button2_disable , NULL ) ;
      drive_MCW_grapher( im3d->g231 , graDR_button2_disable , NULL ) ;
      drive_MCW_grapher( im3d->g312 , graDR_button2_disable , NULL ) ;

   } else {

   /*-- come on baby, light my fire --*/

      drive_MCW_imseq( im3d->s123 , isqDR_button2_enable , NULL ) ;
      drive_MCW_imseq( im3d->s231 , isqDR_button2_enable , NULL ) ;
      drive_MCW_imseq( im3d->s312 , isqDR_button2_enable , NULL ) ;

      if( im3d->vinfo->drawing_pixel > 0 ){
         drive_MCW_imseq( im3d->s123, isqDR_button2_pixel,
                          (XtPointer)im3d->vinfo->drawing_pixel ) ;
         drive_MCW_imseq( im3d->s231, isqDR_button2_pixel,
                          (XtPointer)im3d->vinfo->drawing_pixel ) ;
         drive_MCW_imseq( im3d->s312, isqDR_button2_pixel,
                          (XtPointer)im3d->vinfo->drawing_pixel ) ;
      }

      drive_MCW_imseq( im3d->s123, isqDR_button2_mode,
                       (XtPointer)im3d->vinfo->drawing_mode ) ;
      drive_MCW_imseq( im3d->s231, isqDR_button2_mode,
                       (XtPointer)im3d->vinfo->drawing_mode ) ;
      drive_MCW_imseq( im3d->s312, isqDR_button2_mode,
                       (XtPointer)im3d->vinfo->drawing_mode ) ;

      drive_MCW_grapher( im3d->g123 , graDR_button2_enable , NULL ) ;
      drive_MCW_grapher( im3d->g231 , graDR_button2_enable , NULL ) ;
      drive_MCW_grapher( im3d->g312 , graDR_button2_enable , NULL ) ;

   }

   EXRETURN ;
}

/*-------------------------------------------------------------------
   Send alteration message to all receivers
   (they don't have to register for this information)
---------------------------------------------------------------------*/

void AFNI_process_alteration( Three_D_View * im3d )
{
   int ir ;

ENTRY("AFNI_process_alteration") ;

   if( !IM3D_VALID(im3d) || im3d->vinfo->receiver == NULL  ||
                            im3d->vinfo->num_receiver == 0   ) EXRETURN ;

   for( ir=0 ; ir < im3d->vinfo->num_receiver ; ir++ ){
      if( im3d->vinfo->receiver[ir] != NULL ){
STATUS(im3d->vinfo->receiver[ir]->receiver_funcname) ;
#if 0
         im3d->vinfo->receiver[ir]->receiver_func(
                   RECEIVE_ALTERATION , 0 , NULL ,
                   im3d->vinfo->receiver[ir]->receiver_data ) ;
#else
         AFNI_CALL_VOID_4ARG( im3d->vinfo->receiver[ir]->receiver_func ,
                              int,RECEIVE_ALTERATION , int,0 ,
                              void *,NULL ,
                              void *,im3d->vinfo->receiver[ir]->receiver_data ) ;
#endif
      }
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------
   Send drawing notices to interested receivers.
   Really used to indicate that the values in a dataset have changed.
---------------------------------------------------------------------*/

void AFNI_process_drawnotice( Three_D_View * im3d )
{
   int ir ;

ENTRY("AFNI_process_drawnotice") ;

   if( !IM3D_VALID(im3d) || im3d->vinfo->receiver == NULL  ||
                            im3d->vinfo->num_receiver == 0   ) EXRETURN ;

   for( ir=0 ; ir < im3d->vinfo->num_receiver ; ir++ ){

      if( im3d->vinfo->receiver[ir] != NULL &&
          (im3d->vinfo->receiver[ir]->receiver_mask & RECEIVE_DRAWNOTICE_MASK) ){

STATUS(im3d->vinfo->receiver[ir]->receiver_funcname) ;
#if 0
         im3d->vinfo->receiver[ir]->receiver_func(
                   RECEIVE_DRAWNOTICE , 0 , NULL ,
                   im3d->vinfo->receiver[ir]->receiver_data ) ;
#else
         AFNI_CALL_VOID_4ARG( im3d->vinfo->receiver[ir]->receiver_func ,
                              int,RECEIVE_DRAWNOTICE , int,0 ,
                              void *,NULL ,
                              void *,im3d->vinfo->receiver[ir]->receiver_data ) ;
#endif
      }
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------
   Send dset change notices to interested receivers
---------------------------------------------------------------------*/

void AFNI_process_dsetchange( Three_D_View * im3d )
{
   int ir ;

ENTRY("AFNI_process_dsetchange") ;

   if( !IM3D_VALID(im3d) || im3d->vinfo->receiver == NULL  ||
                            im3d->vinfo->num_receiver == 0   ) EXRETURN ;

   for( ir=0 ; ir < im3d->vinfo->num_receiver ; ir++ ){

      if( im3d->vinfo->receiver[ir] != NULL &&
          (im3d->vinfo->receiver[ir]->receiver_mask & RECEIVE_DSETCHANGE_MASK) ){

STATUS(im3d->vinfo->receiver[ir]->receiver_funcname) ;
#if 0
         im3d->vinfo->receiver[ir]->receiver_func(
                   RECEIVE_DSETCHANGE , 0 , NULL ,
                   im3d->vinfo->receiver[ir]->receiver_data ) ;
#else
         AFNI_CALL_VOID_4ARG( im3d->vinfo->receiver[ir]->receiver_func ,
                              int,RECEIVE_DSETCHANGE , int,0 ,
                              void *,NULL ,
                              void *,im3d->vinfo->receiver[ir]->receiver_data ) ;
#endif
      }
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------
   Send new (i,j,k) coordinates to all receivers that care
---------------------------------------------------------------------*/

void AFNI_process_viewpoint( Three_D_View * im3d )
{
   int ir , ijk[3] ;

ENTRY("AFNI_process_viewpoint") ;

   if( !IM3D_VALID(im3d) || im3d->vinfo->receiver == NULL  ||
                            im3d->vinfo->num_receiver == 0   ) EXRETURN ;

   ijk[0] = im3d->vinfo->i1 ;
   ijk[1] = im3d->vinfo->j2 ;
   ijk[2] = im3d->vinfo->k3 ;

   for( ir=0 ; ir < im3d->vinfo->num_receiver ; ir++ ){
      if( im3d->vinfo->receiver[ir] != NULL &&
          (im3d->vinfo->receiver[ir]->receiver_mask & RECEIVE_VIEWPOINT_MASK) ){

STATUS(im3d->vinfo->receiver[ir]->receiver_funcname) ;
#if 0
         im3d->vinfo->receiver[ir]->receiver_func(
                   RECEIVE_VIEWPOINT , 3 , ijk ,
                   im3d->vinfo->receiver[ir]->receiver_data ) ;
#else
         AFNI_CALL_VOID_4ARG( im3d->vinfo->receiver[ir]->receiver_func ,
                              int,RECEIVE_VIEWPOINT , int,3 ,
                              void *,ijk ,
                              void *,im3d->vinfo->receiver[ir]->receiver_data ) ;
#endif
      }
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------*/
/*! Send time index change notice to receivers that care [29 Jan 2003]
---------------------------------------------------------------------*/

void AFNI_process_timeindex( Three_D_View * im3d )
{
   int ir ;

ENTRY("AFNI_process_timeindex") ;

   if( !IM3D_VALID(im3d) || im3d->vinfo->receiver == NULL  ||
                            im3d->vinfo->num_receiver == 0   ) EXRETURN ;

   for( ir=0 ; ir < im3d->vinfo->num_receiver ; ir++ ){
      if( im3d->vinfo->receiver[ir] != NULL &&
          (im3d->vinfo->receiver[ir]->receiver_mask & RECEIVE_TIMEINDEX_MASK) ){

STATUS(im3d->vinfo->receiver[ir]->receiver_funcname) ;
#if 0
         im3d->vinfo->receiver[ir]->receiver_func(
                   RECEIVE_TIMEINDEX , 0 , NULL ,
                   im3d->vinfo->receiver[ir]->receiver_data ) ;
#else
         AFNI_CALL_VOID_4ARG( im3d->vinfo->receiver[ir]->receiver_func ,
                              int,RECEIVE_TIMEINDEX , int,0 ,
                              void *,NULL ,
                              void *,im3d->vinfo->receiver[ir]->receiver_data ) ;
#endif
      }
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------*/
/*! Send redisplay notification to receivers that care [04 Mar 2002].
---------------------------------------------------------------------*/

void AFNI_process_redisplay( Three_D_View * im3d )
{
   int ir ;

ENTRY("AFNI_process_redisplay") ;

   if( !IM3D_VALID(im3d) || im3d->vinfo->receiver == NULL  ||
                            im3d->vinfo->num_receiver == 0   ) EXRETURN ;

   for( ir=0 ; ir < im3d->vinfo->num_receiver ; ir++ ){
      if( im3d->vinfo->receiver[ir] != NULL &&
          (im3d->vinfo->receiver[ir]->receiver_mask & RECEIVE_REDISPLAY_MASK) ){

STATUS(im3d->vinfo->receiver[ir]->receiver_funcname) ;
#if 0
         im3d->vinfo->receiver[ir]->receiver_func(
                   RECEIVE_REDISPLAY , 0 , NULL ,
                   im3d->vinfo->receiver[ir]->receiver_data ) ;
#else
         AFNI_CALL_VOID_4ARG( im3d->vinfo->receiver[ir]->receiver_func ,
                              int,RECEIVE_REDISPLAY , int,0 ,
                              void *,NULL ,
                              void *,im3d->vinfo->receiver[ir]->receiver_data ) ;
#endif
      }
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------*/
/*! Send funcdisplay notification to receivers that care [05 Mar 2002].
---------------------------------------------------------------------*/

void AFNI_process_funcdisplay( Three_D_View * im3d )
{
   int ir ;

ENTRY("AFNI_process_funcdisplay") ;

   if( !IM3D_VALID(im3d) || im3d->vinfo->receiver == NULL  ||
                            im3d->vinfo->num_receiver == 0   ) EXRETURN ;

   for( ir=0 ; ir < im3d->vinfo->num_receiver ; ir++ ){
      if( im3d->vinfo->receiver[ir] != NULL &&
          (im3d->vinfo->receiver[ir]->receiver_mask & RECEIVE_FUNCDISPLAY_MASK) ){

STATUS(im3d->vinfo->receiver[ir]->receiver_funcname) ;
#if 0
         im3d->vinfo->receiver[ir]->receiver_func(
                   RECEIVE_FUNCDISPLAY , 0 , NULL ,
                   im3d->vinfo->receiver[ir]->receiver_data ) ;
#else
         AFNI_CALL_VOID_4ARG( im3d->vinfo->receiver[ir]->receiver_func ,
                              int,RECEIVE_FUNCDISPLAY , int,0 ,
                              void *,NULL ,
                              void *,im3d->vinfo->receiver[ir]->receiver_data ) ;
#endif
      }
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------*/
/*! Process drawn points:
    - npts  = number of points sent in
    - mode  = type of points to follow:
              - PLANAR_MODE + zz ==> axis zz is fixed, for zz=1,2,3
              - SINGLE_MODE + zz ==> one point only, clicked in a
                                      window with zz fixed
              - THREED_MODE      ==> points are in 3D (not planar)
                                      (not currently possible)
              - UNDO_MODE        ==> undo last drawing action
              - INCVAL_MODE      ==> add 1 to drawing value
              - DECVAL_MODE      ==> subtract 1 from drawing value

    - xd[i] = x index of point i, for i=0..npts-1
    - yd[i] = y index of point i, for i=0..npts-1
    - zd[i] = z index of point i, for i=0..npts-1

   These indices are with respect to the dataset brick axes.
---------------------------------------------------------------------*/

void AFNI_process_drawing( Three_D_View *im3d , int mode ,
                           int npts , int *xd , int *yd , int *zd )
{
   int ii , nn , ir , nsent ;
   int *vp[4] , *xn=NULL , *yn=NULL , *zn=NULL ;

ENTRY("AFNI_process_drawing") ;

   if( !IM3D_VALID(im3d) || im3d->vinfo->receiver == NULL  ||
                            im3d->vinfo->num_receiver == 0 ||
       npts < 0          || im3d->vinfo->drawing_enabled == 0 ) EXRETURN ;

if(PRINT_TRACING){
  char str[256] ; sprintf(str,"received %d points",npts) ; STATUS(str) ;
}

   /*-- if no further treatment is needed,
        just flag input data to be set to receiver --*/

   if( im3d->vinfo->drawing_mode == DRAWING_POINTS ||
       im3d->vinfo->drawing_mode == DRAWING_NODRAW || npts <= 1 ){

      xn = xd ; yn = yd ; zn = zd ; nn = npts ;
   }

   /*-- must fill in the lines:
        create arrays xn, yn, zn of length nn --*/

   else if( im3d->vinfo->drawing_mode == DRAWING_LINES ){

      AFNI_3d_linefill( npts , xd,yd,zd , &nn , &xn , &yn , &zn ) ;
   }

   /*-- must fill in the polygon --*/

   else if( im3d->vinfo->drawing_mode == DRAWING_FILL ){

      /* this just fills the lines like above --
         need to write a real polygon filling routine? */

      AFNI_3d_linefill( npts , xd,yd,zd , &nn , &xn , &yn , &zn ) ;
   }

   /*-- send data to receivers that want it --*/

#if 0
fprintf(stderr,"Sending %d points to receiver\n",nn) ;
#endif

   vp[3] = &mode ;                        /* how the points are arranged */

   vp[0] = xn ; vp[1] = yn ; vp[2] = zn ;

   for( nsent=ir=0 ; ir < im3d->vinfo->num_receiver ; ir++ ){

      if( im3d->vinfo->receiver[ir] != NULL &&
          (im3d->vinfo->receiver[ir]->receiver_mask & RECEIVE_DRAWING_MASK) ){

STATUS(im3d->vinfo->receiver[ir]->receiver_funcname) ;
#if 0
         im3d->vinfo->receiver[ir]->receiver_func(
                   RECEIVE_POINTS , nn ,
                   (void *) vp , im3d->vinfo->receiver[ir]->receiver_data ) ;
#else
         AFNI_CALL_VOID_4ARG( im3d->vinfo->receiver[ir]->receiver_func ,
                              int,RECEIVE_POINTS , int,nn ,
                              void *,vp ,
                              void *,im3d->vinfo->receiver[ir]->receiver_data ) ;
#endif
         nsent++ ;
      }
   }

   /* 30 Mar 1999: also send DRAWNOTICE to receivers that like this stuff */

   if( nsent > 0 ) AFNI_process_drawnotice( im3d ) ;

   /*-- free any created arrays --*/

   if( xn != xd ){ free(xn); free(yn); free(zn); }
   EXRETURN ;
}

/*--------------------------------------------------------------------
   3D line filler-inner -- quick and dirty.
   The output arrays (*xout, *yout, *zout) should be freed when
   their usefulness is over.
----------------------------------------------------------------------*/

void AFNI_3d_linefill( int   nin  , int *  xin  , int *  yin  , int *  zin ,
                       int * nout , int ** xout , int ** yout , int ** zout )
{
   int * xut , * yut , * zut ;
   int   nut , iin , jout , nall ;
   int   x1,y1,z1 , x2,y2,z2 , dx,dy,dz , adx,ady,adz , xlast,ylast,zlast ;
   float fdxyz , fdx,fdy,fdz , fx,fy,fz ;

ENTRY("AFNI_3d_linefill") ;

   /* sanity check */

   if( nin <= 0 || xin == NULL || yin == NULL || zin == NULL ){
      *nout = 0 ; *xout = *yout = *zout = NULL ; EXRETURN ;
   }

   /* trivial case */

   if( nin == 1 ){
      nut = 1 ;                                             *nout = nut ;
      xut = (int *) malloc(sizeof(int)) ; xut[0] = xin[0] ; *xout = xut ;
      yut = (int *) malloc(sizeof(int)) ; yut[0] = yin[0] ; *yout = yut ;
      zut = (int *) malloc(sizeof(int)) ; zut[0] = zin[0] ; *zout = zut ;
      EXRETURN ;
   }

   /* setup to scan through lines */

   nall = nin ;
   xut  = (int *) malloc( sizeof(int) * nall ) ;
   yut  = (int *) malloc( sizeof(int) * nall ) ;
   zut  = (int *) malloc( sizeof(int) * nall ) ;
   nut  = 0 ;

#undef  ADDPT
#define ADDPT(i,j,k)                                          \
  do{ if( nut == nall ){                                      \
         nall += 128 ;                                        \
         xut = (int *) realloc( xut , sizeof(int) * nall ) ;  \
         yut = (int *) realloc( yut , sizeof(int) * nall ) ;  \
         zut = (int *) realloc( zut , sizeof(int) * nall ) ;  \
      }                                                       \
      xut[nut] = xlast = (i) ;                                \
      yut[nut] = ylast = (j) ;                                \
      zut[nut] = zlast = (k) ; nut++ ; } while(0)

   /* draw line from point #iin to #iin+1 */

   x2 = xin[0] ; y2 = yin[0] ; z2 = zin[0] ;
   for( iin=0 ; iin < nin-1 ; iin++ ){
      x1  = x2         ; y1  = y2         ; z1  = z2 ;
      x2  = xin[iin+1] ; y2  = yin[iin+1] ; z2  = zin[iin+1] ;
      dx  = x2 - x1    ; dy  = y2 - y1    ; dz  = z2 - z1 ;
      adx = abs(dx)    ; ady = abs(dy)    ; adz = abs(dz) ;

      /* add start point to list */

      ADDPT(x1,y1,z1) ;

      /* special case: neighbors ==> skip to next line */

      if( adx <= 1 && ady <= 1 && adz <= 1 ) continue ;

      /* OK, we have to do work (moan) */

#define SFRAC 0.495

      fdxyz = adx + ady + adz ;                /* Manhattan distance */
      fdx   = SFRAC * dx / fdxyz ;             /* steps */
      fdy   = SFRAC * dy / fdxyz ;
      fdz   = SFRAC * dz / fdxyz ;

      /* step thru in small increments,
         adding new integer points, and stopping at line's end */

#if 0
fprintf(stderr,"linefill: from %d %d %d to %d %d %d\n",
        x1,y1,z1 , x2,y2,z2 ) ;
#endif

      fx = x1+fdx+0.499 ; fy = y1+fdy+0.499 ; fz = z1+fdz+0.499 ;
      do {
         fx = fx + fdx ; fy = fy + fdy ; fz = fz + fdz ;
         x1 = (int) fx ; y1 = (int) fy ; z1 = (int) fz ;

#if 0
fprintf(stderr,"   at %d %d %d\n",x1,y1,z1) ;
#endif

         if( x1 == x2    && y1 == y2    && z1 == z2    ) break ;

         if( x1 != xlast || y1 != ylast || z1 != zlast ) ADDPT(x1,y1,z1) ;
      } while(1) ;

   } /* end of loop over lines */

   ADDPT(x2,y2,z2) ;  /* add last point */

   *nout = nut ; *xout = xut ; *yout = yut ; *zout = zut ;
   EXRETURN ;
}

/*********************************************************************
   Coordinate inter-conversion routines.
**********************************************************************/

void AFNI_ijk_to_xyz( THD_3dim_dataset * dset ,
                      int ii , int jj , int kk ,
                      float * xx , float * yy , float * zz )
{
   THD_fvec3 fv ;

   if( ! ISVALID_DSET(dset) ) return ;

   fv  = THD_3dind_to_3dmm( dset , TEMP_IVEC3(ii,jj,kk) ) ;
   *xx = fv.xyz[0] ;
   *yy = fv.xyz[1] ;
   *zz = fv.xyz[2] ;
   return ;
}

void AFNI_xyz_to_ijk( THD_3dim_dataset * dset ,
                      float xx , float yy , float zz ,
                      int * ii , int * jj , int * kk  )
{
   THD_ivec3 iv ;

   if( ! ISVALID_DSET(dset) ) return ;

   iv  = THD_3dmm_to_3dind ( dset , TEMP_FVEC3(xx,yy,zz) ) ;
   *ii = iv.ijk[0] ;
   *jj = iv.ijk[1] ;
   *kk = iv.ijk[2] ;
   return ;
}

void AFNI_xyz_to_dicomm( THD_3dim_dataset * dset ,
                         float xx , float yy , float zz ,
                         float * xd , float * yd , float * zd )
{
   THD_fvec3 fv ;

   if( ! ISVALID_DSET(dset) ) return ;

   fv  = THD_3dmm_to_dicomm( dset , TEMP_FVEC3(xx,yy,zz) ) ;
   *xd = fv.xyz[0] ;
   *yd = fv.xyz[1] ;
   *zd = fv.xyz[2] ;
   return ;
}

void AFNI_dicomm_to_xyz( THD_3dim_dataset * dset ,
                         float xd , float yd , float zd ,
                         float * xx , float * yy , float * zz )
{
   THD_fvec3 fv ;

   if( ! ISVALID_DSET(dset) ) return ;

   fv  = THD_3dmm_to_dicomm( dset , TEMP_FVEC3(xd,yd,zd) ) ;
   *xx = fv.xyz[0] ;
   *yy = fv.xyz[1] ;
   *zz = fv.xyz[2] ;
   return ;
}
