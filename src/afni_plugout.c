/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#undef MAIN
#include "afni.h"
#include "afni_plugout.h"

#ifdef ALLOW_PLUGINS

/** global data for plugouts **/

static IOCHAN *ioc_control[NUM_TCP_CONTROL] ;  /* 21 Nov 2001: now an array */
static char   *ioc_conname[NUM_TCP_CONTROL] ;

static int           npout = 0 ;       /* number of plugouts allocated */
static PLUGOUT_spec **pout = NULL ;    /* malloc-ed array of plugouts */

#undef RETRY

static int verbose = 0 ;  /* 28 April 1998 */
static int po_verbose = 1; /*15 Oct 2008 */
static int started = 0 ;  /* 07 Nov 2001 */

/*-----------------------------------------------------------------------*/

int AFNI_have_plugouts( void ){ return started ; }  /* 07 Nov 2001 */

void AFNI_plugout_verb(int verblevel){ po_verbose = verblevel; }  /* 14 Oct 2008 */

/*-----------------------------------------------------------------------
  Initialize plugouts: setup the work process
-------------------------------------------------------------------------*/

void AFNI_init_plugouts( void )
{
   int   cc ;
   char *env ;
   int   base_port ;

ENTRY("AFNI_init_plugouts") ;

   if( started ) EXRETURN ;  /* 07 Nov 2001 */

   PLUTO_register_workproc( AFNI_plugout_workproc , NULL ) ;
   atexit( AFNI_plugout_exit ) ;

   verbose = (GLOBAL_argopt.plugout_code & 1) != 0 ;

   /* 14 Dec 2005 by JMS: allow plugout tcp ports to be overrided */
   /*            (put into AFNI distribution 31 Jan 2006 [rickr]) */
   base_port = BASE_TCP_CONTROL;
   env = getenv("AFNI_PLUGOUT_TCP_BASE");
   if( env != NULL ){
      base_port = atoi(env);
      if( base_port < 1024 || base_port > 65535 ){     /* check for validity */
         fprintf(stderr,"\nPO: bad AFNI_PLUGOUT_TCP_BASE %d,"
                        " should be in [%d,%d]\n", base_port, 1024, 65535);
         base_port = BASE_TCP_CONTROL;            /* invalid, so use default */
      } else /* warn user (and use it) */
         fprintf(stderr,"\nPO: applying AFNI_PLUGOUT_TCP_BASE %d (%d ports)\n",
                 base_port, NUM_TCP_CONTROL);
   }

   for( cc=0 ; cc < NUM_TCP_CONTROL ; cc++ ){       /* 21 Nov 2001: */
      ioc_control[cc] = NULL ;                      /* initialize control */
      ioc_conname[cc] = AFMALL(char, 32) ;          /* sockets and names  */
      sprintf(ioc_conname[cc],"tcp:*:%d",base_port+cc) ;
   }

   started = 1 ; EXRETURN ;
}

/*-----------------------------------------------------------------------
  Routine executed at AFNI exit: shutdown all plugout IOCHANs.
-------------------------------------------------------------------------*/

void AFNI_plugout_exit( void )
{
   int jj , cc ;

   for( cc=0 ; cc < NUM_TCP_CONTROL ; cc++ ){  /* close any open  */
      iochan_set_cutoff( ioc_control[cc] ) ;   /* control sockets */
      iochan_close     ( ioc_control[cc] ) ;
   }

   for( jj=0 ; jj < npout ; jj++ ){            /* close any plugouts */
      if( pout[jj] != NULL && pout[jj]->ioc != NULL ){
         iochan_set_cutoff( pout[jj]->ioc ) ;
         iochan_close     ( pout[jj]->ioc ) ;
         if( verbose )
            fprintf(stderr,"PO: atexit closed channel from plugout %s\n",
                    pout[jj]->po_name ) ;
      }
   }
   return ;
}

/*-----------------------------------------------------------------------
  Plugout workprocess: listen for incoming control connections.
  (If the return is True, that means don't call this workproc again.
   If the return is False, that means call this workproc again.......)
-------------------------------------------------------------------------*/

#define CONTROL_BUFSIZE (16*1024)

Boolean AFNI_plugout_workproc( XtPointer elvis )
{
   int jj , ngood , pcode , ii , opcount=0 , cc ;
   PLUGOUT_spec *pp ;

   /********************************************/
   /** start up non-listening control sockets **/

   AFNI_block_rescan(1) ;    /* 10 Nov 2005 */

   for( cc=0 ; cc < NUM_TCP_CONTROL ; cc++ ){

     if( ioc_control[cc] == NULL ){  /* not open now, so open it */

        ioc_control[cc] = iochan_init( ioc_conname[cc] , "accept" ) ;

#ifdef RETRY
        if( ioc_control[cc] == NULL ){
           if( verbose )
              fprintf(stderr,"PO: waiting to listen on control channel") ;
           for( ii=0 ; ii < 10 ; ii++ ){
              if( verbose ) fprintf(stderr,".") ;
              AFNI_sleep(VLONG_DELAY) ;  /* wait a bit, try again */
              ioc_control[cc] = iochan_init( ioc_conname[cc] , "accept" ) ;
              if( ioc_control[cc] != NULL ) break ;
           }
           if( verbose ) fprintf(stderr,"\n") ;
        }
#endif /* RETRY */

        if( ioc_control[cc] != NULL ) opcount++ ;
     }
   }

   /****************************************/
   /** see if any control socket is ready **/

   for( cc=0 ; cc < NUM_TCP_CONTROL ; cc++ ){

     if( ioc_control[cc] == NULL ) continue ; /* not listening */

     jj = iochan_goodcheck(ioc_control[cc],SHORT_DELAY) ;

     if( jj == 0 ) continue ; /* not ready */

     if( jj == 1 ){           /* someone connected to the control socket! */
        int   npobuf ;
        char * pobuf ;

        /** check if the connection is trustworthy **/
        if(po_verbose)
           fprintf(stderr, \
             "PO: plugout connection from host %s\n",ioc_control[cc]->name) ;

        if( !TRUST_host(ioc_control[cc]->name) ){
          fprintf(stderr,"PO: untrusted host: %s -- connection denied\n" ,
                  ioc_control[cc]->name) ;
          iochan_set_cutoff(ioc_control[cc]) ; IOCHAN_CLOSE(ioc_control[cc]) ;
          continue ; /* skip to next control socket */
        }

        /** read all data possible from the control channel **/

        npobuf = 0 ;  /* number of bytes received so far */
        pobuf  = (char *) malloc( sizeof(char) * CONTROL_BUFSIZE ) ;

        while(1){
           jj = iochan_recv( ioc_control[cc] , pobuf+npobuf , CONTROL_BUFSIZE-npobuf ) ;
           if( jj < 1 ) break ;  /* stop if nothing more comes in */
           npobuf += jj ;
           if( npobuf >= CONTROL_BUFSIZE ){  /* stop if overflow */
              fprintf(stderr,"PO: control channel buffer overflow!\n") ;
              break ;
           }
           for( ii=0 ; ii < npobuf ; ii++ ) if( pobuf[ii] == '\0' ) break ;
           if( ii < npobuf ) break ;      /* stop if found a NUL character */

           AFNI_sleep( SHORT_DELAY ) ;  /* wait for some more? */
        }

        if( npobuf < 1 ){
          fprintf(stderr,"PO: control channel sent no data!\n") ;
          iochan_set_cutoff(ioc_control[cc]) ; IOCHAN_CLOSE(ioc_control[cc]) ;
          free(pobuf) ;
          continue ;    /* skip to next control socket */
        }

        /** process the input and make a new connection to a plugout program **/

        pp = new_PLUGOUT_spec( npobuf , pobuf ) ;

        if( pp == NULL ){
          fprintf(stderr,"PO: can't create PLUGOUT_spec.  Input was:\n%s\n",pobuf) ;
          PO_ACK_BAD(ioc_control[cc]) ;
          AFNI_sleep(LONG_DELAY) ;
          iochan_set_cutoff(ioc_control[cc]) ; IOCHAN_CLOSE(ioc_control[cc]) ;
          free(pobuf) ;
          continue ;    /* skip to next control socket */
        } else {
          if( pp->do_ack ) PO_ACK_OK(ioc_control[cc]) ;
          AFNI_sleep(LONG_DELAY) ;
          iochan_set_cutoff(ioc_control[cc]) ; IOCHAN_CLOSE(ioc_control[cc]) ;
          if(po_verbose)
             fprintf(stderr,"PO: plugout connection name is %s\n",pp->po_name) ;
        }

        if( npout == 0 ){
          pout = (PLUGOUT_spec **) malloc( sizeof(PLUGOUT_spec *) ) ;
        } else {
          pout = (PLUGOUT_spec **) realloc( pout , sizeof(PLUGOUT_spec *)*(npout+1) ) ;
        }
        pout[npout++] = pp ;
        free(pobuf) ;
        opcount++ ;

     } else if( jj == -1 ){  /* something bad happend to control socket */

#if 0
       if( verbose )
          fprintf(stderr,"PO: failure while listening to control channel!\n") ;
#endif

       iochan_set_cutoff(ioc_control[cc]) ; IOCHAN_CLOSE(ioc_control[cc]) ;
       continue ;    /* skip to next control socket */

     }
   } /* end of loop over control sockets */

   /*****************************************************/
   /** see if any of the existing plugouts sent data,  **/
   /** or needs to have data sent to it.               **/

   ngood = 0 ;
   for( jj=0 ; jj < npout ; jj++ ){
      if( pout[jj] != NULL ){
         pcode = AFNI_process_plugout( pout[jj] ) ;
         if( pcode < 0 ) DESTROY_PLUGOUT( pout[jj] ) ;
         else            ngood++ ;

         if( pcode ) opcount++ ;
      }
   }

   /* if all plugouts are deceased, free their array */

   if( ngood == 0 && pout != NULL ){ npout = 0 ; free(pout) ; pout = NULL ; }

   /* if nothing happened, take a short nap */

   AFNI_block_rescan(0) ;    /* 10 Nov 2005 */
   if( opcount == 0 ) AFNI_sleep(LONG_DELAY) ;
   return(False) ;
}

/*------------------------------------------------------------------------
   Process a plugout.  Return codes are:
       0 = nothing happened
      -1 = a fatal error for this plugout --> close it down!
       1 = data was sent to plugout
       2 = data was received from plugout
   Receiving data from a plugout has higher priority than sending
   data to a plugout.  Both will not occur in the same cycle.
--------------------------------------------------------------------------*/

#define PO_BUFSIZE (64*1024)  /* 14 Nov 2001: expand from 16K to 64K */

#define MAX_STR 1024   /* 21 Nov 2001 */

int AFNI_process_plugout( PLUGOUT_spec *pp )
{
   int ii , jj , npobuf , nend , retval=0 ;
   Three_D_View *im3d ;
   static char pobuf[PO_BUFSIZE+1] ; /* I/O buffer */
   static char oobuf[1024] ;

   int         nstr ;                /* 21 Nov 2001: number of substrings */
   static char *str[MAX_STR] ;       /* pointers to substrings from plugout */
   int ss , bb,aa  ;
   char *spt ;

   /** find the lowest numbered open controller **/

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
     im3d = GLOBAL_library.controllers[ii] ;
     if( IM3D_OPEN(im3d) ) break ;
   }
   if( ii == MAX_CONTROLLERS ) return(0) ;  /* nothing available?  weird */

   /** if the IOCHAN isn't ready yet, see if it has become ready **/

   if( ! pp->ioc_ready ){
      ii = iochan_goodcheck( pp->ioc , SHORT_DELAY ) ;
      if( ii == 0 ) return( 0) ;   /* still waiting */

      if( ii <  0 ){               /* something bad */
         fprintf(stderr,"PO: plugout %s IOCHAN failed to establish.\n",
                 pp->po_name ) ;
         spt = iochan_error_string() ;
         if( spt != NULL ) fprintf(stderr,"  : %s\n",spt) ;
         return(-1) ;
      }

      pp->ioc_ready = 1 ;          /* mark that it is ready */

      if( verbose )
         fprintf(stderr,"PO: plugout %s IOCHAN connection established.\n",
                 pp->po_name ) ;
   }

   /***************************************************/
   /** See if the plugout wants to send us some data **/

   jj = iochan_readcheck( pp->ioc , SHORT_DELAY ) ;

   if( jj < 0 ){    /* something bad happened */
      if( po_verbose )
         fprintf(stderr,"PO: plugout %s has broken connection!\n",pp->po_name) ;
      spt = iochan_error_string() ;
      if( spt != NULL && po_verbose ) fprintf(stderr,"  : %s\n",spt) ;
      return(-1) ;
   }

   if( jj > 0 ){  /*** data is incoming: get it, and process the commands ***/

     /** read the incoming data **/

     npobuf = iochan_recv( pp->ioc , pobuf , PO_BUFSIZE ) ;
     if( npobuf <= 0 ){                                /* this error is unlikely */
        if(po_verbose){
           fprintf(stderr, \
             "PO: failure to read data from plugout %s!\n",pp->po_name) ;
           spt = iochan_error_string() ;
           if( spt != NULL ) fprintf(stderr,"  : %s\n",spt) ;
        }
        return(-1) ;
     }

     if( verbose )
        fprintf(stderr,  \
          "PO: plugout %s sent %d bytes of data\n",pp->po_name,npobuf ) ;

     /** ensure data is NUL terminated **/

     if( pobuf[npobuf-1] != '\0' ) pobuf[npobuf++] = '\0' ;

     /* 21 Nov 2001: break input into substrings (at NULs) */

     ss = aa = 0 ;  /* ss=substring index , aa=pobuf index at start of substring */

     while( ss < MAX_STR && aa < npobuf ){

       /* skip aa ahead to next non-blank, non-NUL */

       for( ; aa < npobuf &&
              (pobuf[aa]=='\0' || isspace(pobuf[aa])) ; aa++ ) ; /* nada */

       if( aa == npobuf ) break ;                  /* didn't find anything */

       for( bb=aa+1 ; pobuf[bb] != '\0' ; bb++ ) ; /* skip bb to next NUL */

       str[ss++] = pobuf+aa ;                      /* point to this substring */

       aa = bb+1 ;                                 /* start at next char */
     }
     nstr = ss ; if( nstr > 0 ) retval = 2 ;

     if( verbose && nstr > 1 )
       fprintf(stderr,"PO: received %d command strings at once\n",nstr) ;

     /** determine what to do with each substring **/

     for( ss=0 ; ss < nstr ; ss++ ){

      /* set Talairach coordinates */

      if( strncmp(str[ss],"TT_XYZ_SET",10) == 0 ){
        float xx , yy , zz ;

        ii = sscanf( str[ss] , "TT_XYZ_SET %f %f %f" , &xx , &yy , &zz ) ;
        if( ii < 3 ){

           fprintf(stderr,"PO: malformed TT_XYZ_SET string from plugout %s: %s\n",
                   pp->po_name , str[ss] ) ;
           if( pp->do_ack ) PO_ACK_BAD( pp->ioc ) ;

        } else if( im3d->vinfo->view_type != VIEW_TALAIRACH_TYPE ){

           fprintf(stderr,"PO: can't accept TT_XYZ_SET from plugout %s in %s\n",
                          pp->po_name , VIEW_typestr[im3d->vinfo->view_type]) ;
           if( pp->do_ack ) PO_ACK_BAD( pp->ioc ) ;

        } else {  /* actually change coordinates */

           if( verbose )
              fprintf(stderr,"PO: command TT_XYZ_SET %g %g %g\n",xx,yy,zz) ;

           jj = AFNI_jumpto_dicom( im3d , -xx,-yy,zz ) ;
           if( pp->do_ack ){
              if( jj < 0 ) PO_ACK_BAD( pp->ioc ) ;
              else         PO_ACK_OK ( pp->ioc ) ;
           }

        }

      /* set coordinates in DICOM order */

      } else if( strncmp(str[ss],"DICOM_XYZ_SET",13) == 0 ){
        float xx , yy , zz ;

        ii = sscanf( str[ss] , "DICOM_XYZ_SET %f %f %f" , &xx , &yy , &zz ) ;
        if( ii < 3 ){

           fprintf(stderr,"PO: malformed DICOM_XYZ_SET string from plugout %s: %s\n",
                   pp->po_name , str[ss] ) ;
           if( pp->do_ack ) PO_ACK_BAD( pp->ioc ) ;

        } else {         /* actually set coordinates */

           if( verbose )
              fprintf(stderr,"PO: command DICOM_XYZ_SET %g %g %g\n",xx,yy,zz) ;

           jj = AFNI_jumpto_dicom( im3d , xx,yy,zz ) ;
           if( pp->do_ack ){
              if( jj < 0 ) PO_ACK_BAD( pp->ioc ) ;
              else         PO_ACK_OK ( pp->ioc ) ;
           }
        }

      /* set voxel indexes */

      } else if( strncmp(str[ss],"DSET_IJK_SET",12) == 0 ){
        int ix , jy , kz ;

        ii = sscanf( str[ss] , "DSET_IJK_SET %d %d %d" , &ix , &jy , &kz ) ;
        if( ii < 3 ){
           fprintf(stderr,"PO: malformed DSET_IJK_SET string from plugout %s: %s\n",
                   pp->po_name , str[ss] ) ;
           if( pp->do_ack ) PO_ACK_BAD( pp->ioc ) ;

        } else {   /* actually set indexes */

           if( verbose )
              fprintf(stderr,"PO: command DSET_IJK_SET %d %d %d\n",ix,jy,kz) ;

           AFNI_set_viewpoint( im3d , ix,jy,kz , REDISPLAY_ALL ) ;
           if( pp->do_ack ) PO_ACK_OK ( pp->ioc ) ;
        }

      /* send voxel indexes back to plugout */

      } else if( strncmp(str[ss],"DSET_IJK_GET",12) == 0 ){
        int ix=im3d->vinfo->i1 , jy=im3d->vinfo->j2 , kz=im3d->vinfo->k3 ;

        if( verbose )
           fprintf(stderr,"PO: command DSET_IJK_GET -> %d %d %d\n",ix,jy,kz) ;

        sprintf(oobuf,"DSET_IJK %d %d %d\n" , ix,jy,kz ) ;  /* send result */
        PO_SEND( pp->ioc , oobuf ) ;

        if( pp->do_ack ){
          jj = iochan_readcheck( pp->ioc, -1 ) ;  /* wait for return message */
          if( jj == -1 ) return(-1) ;             /* something bad! */
          jj = iochan_recv( pp->ioc , oobuf , POACKSIZE ) ; /* read message */
          if( verbose )
             fprintf(stderr, (jj > 0) ? "PO: received acknowledgment\n"
                                      : "PO: did not receive acknowledgment\n" ) ;
        }

      /* set surface node ID */

      } else if( strncmp(str[ss],"SURFID",6) == 0 ){
        if( SUMA_ENABLED && SESSION_HAS_SUMA(im3d->ss_now) ){
           int id ;

           ii = sscanf( str[ss] , "SURFID %d" , &id ) ;
           if( ii < 1 ){
              fprintf(stderr,"PO: malformed SURFID string from plugout %s: %s\n",
                      pp->po_name , str[ss] ) ;
              if( pp->do_ack ) PO_ACK_BAD( pp->ioc ) ;

           } else {  /* find the node and jump to that location */

              if( verbose )
                 fprintf(stderr,"PO: command SURFID %d\n",id) ;

              ii = SUMA_find_node_id( im3d->ss_now->su_surf[0] , id ) ;
              if( ii < 0 ){
                 fprintf(stderr,"PO: unknown SURFID node number %d\n",id) ;
                 if( pp->do_ack ) PO_ACK_BAD( pp->ioc ) ;
              } else {
                 AFNI_jumpto_dicom( im3d ,
                                    im3d->ss_now->su_surf[0]->ixyz[ii].x ,
                                    im3d->ss_now->su_surf[0]->ixyz[ii].y ,
                                    im3d->ss_now->su_surf[0]->ixyz[ii].z  ) ;
                 if( pp->do_ack ) PO_ACK_OK ( pp->ioc ) ;
              }
           }
        }

      /*-- 07 Nov 2001: drive various AFNI user interface widgets --*/

      } else if( strncmp(str[ss],"DRIVE_AFNI",10) == 0 ){
        char cmd[PLUGOUT_COM_LENGTH]="\0" ;

        if( strlen(str[ss]) < 11 ){
          fprintf(stderr,"PO: DRIVE_AFNI from plugout %s lacks command\n",
                         pp->po_name ) ;
          if( pp->do_ack ) PO_ACK_BAD( pp->ioc ) ;

        } else {  /* the command is everything after "DRIVE_AFNI " */

          MCW_strncpy(cmd,str[ss]+11,PLUGOUT_COM_LENGTH) ;
          if( verbose )
            fprintf(stderr,"PO: command DRIVE_AFNI %s\n",cmd) ;
          ii = AFNI_driver( cmd ) ;  /* just do it */
          if( pp->do_ack ){
            if( ii < 0 ) PO_ACK_BAD( pp->ioc ) ;
            else         PO_ACK_OK ( pp->ioc ) ;
          }
        }

#if 0
      /*-- 01 Feb 2008: a command to read NIML data from a file --*/

      } else if( strncmp(str[ss],"READ_NIML_FILE",14) == 0 ){

        if( strlen(str[ss]) < 16 ){
          fprintf(stderr,"PO: READ_NIML_FILE from plugout %s lacks filename\n",
                         pp->po_name ) ;
          if( pp->do_ack ) PO_ACK_BAD( pp->ioc ) ;
        } else {
          char fname[THD_MAX_NAME]="\0" ; void *nini ;
          sscanf(str[ss]+15,"%s",fname) ;
          nini = NI_read_element_fromfile(fname) ;
          if( nini != NULL ){
            AFNI_process_NIML_data(0,nini,-1) ;
            NI_free_element(nini) ;
            PO_ACK_OK(pp->ioc) ;
          } else {
            PO_ACK_BAD(pp->ioc);
          }
        }
#endif

      /*-- unknown plugout command: drop a daisy cutter on them --*/

      } else {
        fprintf(stderr,"PO: plugout %s sent unknown command string: %s\n",
                pp->po_name , str[ss] ) ;
        if( pp->do_ack ) PO_ACK_BAD( pp->ioc ) ;
      }

     } /* 21 Nov 2001: end of loop over substrings */

   } else {  /* no data incoming; maybe send something? */

   /****************************************************************/
   /** see if anything that has changed that we should tell about **/

      npobuf   = 0 ;
      pobuf[0] = '\0' ;

#define FEQ(a,b) (fabs((a)-(b)) < 0.01)

      /** check each output mode, and format output string if needed **/

      for( ii=0 ; ii < pp->npomode ; ii++ ){

         switch( pp->pomode[ii] ){

            case POMODE_DICOM_XYZ_DELTA:{
               float xx , yy , zz ;
               xx = im3d->vinfo->xi ;
               yy = im3d->vinfo->yj ;
               zz = im3d->vinfo->zk ;
               if( !FEQ(xx,pp->xi) || !FEQ(yy,pp->yj) || !FEQ(zz,pp->zk) )
                  sprintf( pobuf + npobuf , "DICOM_XYZ %9.3f %9.3f %9.3f\n" , xx,yy,zz ) ;
            }
            break ;

            case POMODE_TT_XYZ_DELTA:{
               float xx , yy , zz ;
               xx = im3d->vinfo->xi ;
               yy = im3d->vinfo->yj ;
               zz = im3d->vinfo->zk ;
               if( im3d->vinfo->view_type == VIEW_TALAIRACH_TYPE &&
                  (!FEQ(xx,pp->xi) || !FEQ(yy,pp->yj) || !FEQ(zz,pp->zk)) )
                  sprintf( pobuf + npobuf , "TT_XYZ %9.3f %9.3f %9.3f\n" , -xx,-yy,zz ) ;
            }
            break ;

            case POMODE_DSET_IJK_DELTA:{
               int ix , jy , kz ;
               ix = im3d->vinfo->i1 ;
               jy = im3d->vinfo->j2 ;
               kz = im3d->vinfo->k3 ;
               if( ix != pp->ix || jy != pp->jy || kz != pp->kz )
                  sprintf( pobuf + npobuf , "DSET_IJK %d %d %d\n" , ix,jy,kz ) ;
            }
            break ;

            /* 11 Sep 2002 */

            case POMODE_UNDERLAY_DELTA:{
               if( !EQUIV_DSETS(pp->dset_underlay,im3d->anat_now) &&
                   ISVALID_DSET(im3d->anat_now)                     ){

                   sprintf( pobuf + npobuf , "UNDERLAY %s\n" ,
                            DSET_HEADNAME(im3d->anat_now) ) ;
               }
            }
            break ;

            case POMODE_OVERLAY_DELTA:{
               if( !EQUIV_DSETS(pp->dset_overlay,im3d->fim_now) &&
                   ISVALID_DSET(im3d->fim_now)                     ){

                   sprintf( pobuf + npobuf , "OVERLAY %s\n" ,
                            DSET_HEADNAME(im3d->fim_now) ) ;
               }
            }
            break ;

         } /* end of switch on modes */

         npobuf = strlen( pobuf ) ;
      }

      /** send string, if any, then wait for acknowledgement **/

      if( npobuf > 0 ){
         if( verbose )
            fprintf(stderr,"PO: sending plugout %s this string:\n    %s",
                    pp->po_name , pobuf ) ;

         PO_SEND( pp->ioc , pobuf ) ;            /* send */
         if( pp->do_ack ){
           jj = iochan_readcheck( pp->ioc, -1 ) ;  /* wait for return message */
           if( jj == -1 ) return(-1) ;             /* something bad! */
           jj = iochan_recv( pp->ioc , pobuf , POACKSIZE ) ; /* read message */
           if( verbose )
             fprintf(stderr, (jj > 0) ? "PO: received acknowledgment\n"
                                      : "PO: did not receive acknowledgment\n" ) ;
         }
         retval = 1 ;
      }

   } /* end of potentially sending stuff to the plugout */

   /******************************************/
   /** Load memory of the current situation **/

Proust:
   pp->xi             = im3d->vinfo->xi ;
   pp->yj             = im3d->vinfo->yj ;
   pp->zk             = im3d->vinfo->zk ;
   pp->ix             = im3d->vinfo->i1 ;
   pp->jy             = im3d->vinfo->j2 ;
   pp->kz             = im3d->vinfo->k3 ;
   pp->time_index     = im3d->vinfo->time_index ;
   pp->view_type      = im3d->vinfo->view_type ;
   pp->sess_num       = im3d->vinfo->sess_num ;
   pp->anat_num       = im3d->vinfo->anat_num ;
   pp->func_num       = im3d->vinfo->func_num ;
   pp->func_threshold = im3d->vinfo->func_threshold ;

   pp->dset_underlay  = im3d->anat_now ;
   pp->dset_overlay   = im3d->fim_now  ;

   return(retval) ;
}

/*------------------------------------------------------------------------
   Create a new plugout,
   based on the control information in the buf string.
--------------------------------------------------------------------------*/

#define STARTER(st) (strncmp(buf,st,strlen(st)) == 0)
#define NBUF        256

PLUGOUT_spec * new_PLUGOUT_spec( int ninfo , char * info )
{
   PLUGOUT_spec * pp ;
   char buf[NBUF] ;
   char opt[32] ;
   int ii , jj , nstart,nend , nuse , nbuf ;

ENTRY("new_PLUGOUT_spec") ;

   /** check input for OK-osity **/

   if( ninfo <= 0 ) RETURN( NULL ) ;
   for( nend=0 ; nend < ninfo && info[nend] != '\0' ; nend++ ) ; /* nada */
   if( nend == ninfo ){
      fprintf(stderr,"PO: control string not NUL-terminated!\n") ;
      RETURN( NULL ) ;
   }

   /** initialize a new plugout specification **/

   pp = (PLUGOUT_spec *) malloc( sizeof(PLUGOUT_spec) ) ;

   pp->npomode     = 0 ;
   pp->ioc_name[0] = '\0' ;
   pp->ioc         = NULL ;
   strcpy(pp->po_name,"Old One-Eyed Dogface") ;

   pp->do_ack      = 1 ;   /* 06 Sep 2001 */

   if( verbose )
      fprintf(stderr,"PO: initializing new plugout -- got %d input bytes\n",nend) ;

   /** the input buffer should be of the form:
         IO_KEYWORD options
         IO_KEYWORD options
           ...
         IOCHAN iochan_specification

       where IO_KEYWORD specifies the type of
       input/output will be going from/to this plugout program. **/

   /******************************************/
   /** Scan ahead until next command string **/

   nstart = 0 ;
   while( nstart < nend ){

      /** skip whitespace **/

      for( ; nstart < nend && isspace(info[nstart]) ; nstart++ ) ; /* nada */
      if( nstart >= nend ) break ;

      /** copy characters into buf until the next '\n' or '\0' **/

      for( nbuf=0,jj=nstart ; nbuf < NBUF && info[jj] != '\n' && info[jj] != '\0' ; ){
         buf[nbuf++] = info[jj++] ;
      }
      if( nbuf == NBUF ){
         fprintf(stderr,"PO: line buffer overflow in control information!\n") ;
         nbuf-- ;
      }
      buf[nbuf] = '\0' ; nstart = jj ;

      /************************************/
      /*** Scan for legal input strings ***/

      if( verbose )
         fprintf(stderr,"PO: initializer command = %s\n",buf) ;

      if( STARTER("TT_XYZ_DELTA") ){

         pp->pomode[ pp->npomode ] = POMODE_TT_XYZ_DELTA ;
         pp->npomode ++ ;

      } else if( STARTER("DICOM_XYZ_DELTA") ){

         pp->pomode[ pp->npomode ] = POMODE_DICOM_XYZ_DELTA ;
         pp->npomode ++ ;

      } else if( STARTER("DSET_IJK_DELTA") ){

         pp->pomode[ pp->npomode ] = POMODE_DSET_IJK_DELTA ;
         pp->npomode ++ ;

      } else if( STARTER("SURFID_DELTA") ){  /* 05 Sep 2001 */

         pp->pomode[ pp->npomode ] = POMODE_SURFID_DELTA ;
         pp->npomode ++ ;

      } else if( STARTER("UNDERLAY_DELTA") ){  /* 11 Jan 2002 */

         pp->pomode[ pp->npomode ] = POMODE_UNDERLAY_DELTA ;
         pp->npomode ++ ;

      } else if( STARTER("OVERLAY_DELTA") ){

         pp->pomode[ pp->npomode ] = POMODE_OVERLAY_DELTA ;
         pp->npomode ++ ;

      } else if( STARTER("NO_ACK") ){        /* 06 Sep 2001 */

         pp->do_ack = 0 ;

      } else if( STARTER("IOCHAN") ){
         int kk ;

         sscanf( buf , "IOCHAN %127s" , pp->ioc_name ) ;

         for( kk=0 ; kk < 10 ; kk++ ){
            pp->ioc = iochan_init( pp->ioc_name , "accept" ) ;
            if( pp->ioc != NULL ) break ;
            AFNI_sleep(VLONG_DELAY) ; /* wait a bit, try again */
         }
         if( pp->ioc == NULL )
            pp->ioc = iochan_init( pp->ioc_name , "accept" ) ;  /* last try */

         if( pp->ioc == NULL )
            fprintf(stderr,"PO: can't listen on IOCHAN %s\n",pp->ioc_name) ;
         pp->ioc_ready = 0 ;

      } else if( STARTER("PONAME") ){

         sscanf( buf , "PONAME %127s" , pp->po_name ) ;

      } else {

         fprintf(stderr,"PO: illegal control info=%s\n",buf) ;
      }

   }  /* end of loop over command buffers */

   /****************************************/
   /** check that something good happened **/

   if( pp->ioc == NULL ){
      fprintf(stderr,"PO: no IOCHAN connection established.\n") ;
      free(pp) ; RETURN( NULL ) ;
   }

   /** initialize memory of things past **/

   pp->xi = pp->yj = pp->zk = -987.654 ;
   pp->time_index = -1 ;
   pp->view_type  = -1 ;
   pp->sess_num   = pp->anat_num = pp->func_num = -1 ;
   pp->func_threshold = -0.987654 ;

   pp->dset_underlay = pp->dset_overlay = NULL ; /* 11 Jan 2002 */

   if( verbose )
      fprintf(stderr,"PO: initialization completed successfully.\n") ;

   RETURN( pp ) ;
}

/*************************************************************************/
#else  /* ALLOW_PLUGINS not defined */

void AFNI_init_plugouts( void ){ return ; }
int AFNI_have_plugouts( void ){ return 0 ; }  /* 07 Nov 2001 */

#endif /* ALLOW_PLUGINS */
