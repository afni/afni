#include "SUMA_suma.h"

/**************************************/
/** global data for NIML connections **/
/**************************************/
extern int SUMAg_N_DOv; 
extern SUMA_DO *SUMAg_DOv;
extern SUMA_CommonFields *SUMAg_CF; 
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv;


/*-----------------------------------------------*/
/*! Flag to tell if NIML things are initialized. */

static int started = 0 ;


/*-----------------------------------------------------------------------*/
/*! NIML workprocess.
    - Read and process any new data from open connection.

  (If the return is True, that means don't call this workproc again.
   If the return is False, that means call this workproc again.......)
-------------------------------------------------------------------------*/

Boolean SUMA_niml_workproc( XtPointer thereiselvis )
{
   static char FuncName[]={"SUMA_niml_workproc"};
   int cc , nn, ngood = 0, id;
   void *nini ;
   static int nwarn=0;
   char tmpcom[100], *nel_track;
   SUMA_SurfaceViewer *sv;
   NI_element *nel ;
   DList *list = NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMA_NIML_WORKPROC_IO_NOTIFY) {SUMA_ENTRY;}

   if (!SUMAg_CF->niml_work_on) SUMAg_CF->niml_work_on = YUP;
   
   sv = (SUMA_SurfaceViewer *)thereiselvis;
   SUMA_LH("In");
   
   for (cc=0; cc<SUMA_MAX_STREAMS; ++cc) {
     if (cc == SUMA_AFNI_STREAM_INDEX2) continue; 
         /*      this stream is listened to by AFNI and is used by non-suma 
                 SUMA programs to communicate with AFNI directly. 
                 SUMA programs are not to receive elements back on this stream 
                 (unlike suma with SUMA_AFNI_STREAM_INDEX)
                  because communications for now are one way only. */
      
      /* *** post Dec. 18 03, making SUMA listen to people's needs */
      /* open streams that aren't open */
      
      if (  cc != SUMA_AFNI_STREAM_INDEX  && 
            cc != SUMA_AFNI_STREAM_INDEX2 &&
            cc != SUMA_TO_MATLAB_STREAM_INDEX ) { 
         /* Leave AFNI's and MATLAB streams alone, 
            SUMA initiates the connection on those.
            This block is for streams on which SUMA gets contacted 
            first. */                
         if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: Checking on stream %d, %s\n", 
                     FuncName, cc,  SUMAg_CF->NimlStream_v[cc]);
         if( SUMAg_CF->ns_v[cc] == NULL && 
            (SUMAg_CF->ns_flags_v[cc] & SUMA_FLAG_SKIP)==0 ){
            if (LocalHead) 
               fprintf (SUMA_STDERR, "%s: \tNot Skipped.\n", FuncName);
            SUMAg_CF->ns_v[cc] = 
               NI_stream_open( SUMAg_CF->NimlStream_v[cc] , "r" ) ;
            if( SUMAg_CF->ns_v[cc] == NULL ){
               fprintf (SUMA_STDERR, 
                        "%s: Stream %d, %s open returned NULL\n", 
                        FuncName, cc,  SUMAg_CF->NimlStream_v[cc]); 
               SUMAg_CF->ns_flags_v[cc] = SUMA_FLAG_SKIP ; continue;
            }
            if (LocalHead) 
               fprintf (SUMA_STDERR, 
                        "%s: Stream %d, %s open returned NOT null\n", 
                        FuncName, cc,  SUMAg_CF->NimlStream_v[cc]);
            SUMAg_CF->ns_flags_v[cc]  = SUMA_FLAG_WAITING ;
         }else {
            if (SUMAg_CF->ns_v[cc] == NULL) { 
               SUMA_LH("\tSkipped");
               continue;
            }
         }

         ngood ++;
      } else {
         if(   cc == SUMA_AFNI_STREAM_INDEX &&
               SUMAg_CF->ns_v[cc]) {
            ngood ++;
         } else if (  cc == SUMA_TO_MATLAB_STREAM_INDEX &&
               SUMAg_CF->ns_v[cc]) {
            ngood ++;
         } else {
            /* do nothing otherwise */
            continue;
         }
         
      }
      
     
     /* check if stream has gone bad */
     nn = NI_stream_goodcheck( SUMAg_CF->ns_v[cc] , 1 ) ;
     
     if( nn < 0 && cc == SUMA_AFNI_STREAM_INDEX){
         /* first check in case we are dealing with the 
         intermittent AFNI disruption */
         if (SUMA_isEnv("SUMA_AttemptTalkRecover","y")) {
            NI_stream_close( SUMAg_CF->ns_v[cc] ) ;
            SUMAg_CF->ns_v[cc] = NULL ;
            /* try again, a way to get around the weird disruption in 
               SHM connection*/
            SUMA_S_Note("Attempting recovery...");
            SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] = NOPE; 
            if (!list) list = SUMA_CreateList();
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleConnected, 
                                            SES_Suma, sv);
            if (!SUMA_Engine (&list)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_Engine.\n\a", FuncName);
            }
            nn = NI_stream_goodcheck( SUMAg_CF->ns_v[cc] , 1 ) ;
         } else {
            if (!nwarn) {
               SUMA_SLP_Note(
                  "Afni connection stream gone bad.\n"
                  "If Afni did not shutdown, and you \n"
                  "did not close the connection, you \n"
                  "can recover by pressing 't' twice in SUMA.\n"
                  "The disconnection is a known bug with\n"
                  "an as of yet unknown source. \n"
                  "\n"
                  "You can also turn on the automatic recovery mode,\n"
                  "with the environment variable \n"
                  "SUMA_AttemptTalkRecover set to yes (see \n"
                  "suma -environment or the environment section in\n"
                  "SUMA's ctrl+h help output for details.)\n"
                  "\n"
                  "Lastly, you can use -ah 127.0.0.1 to use sockets\n"
                  "instead of shared memory. But that kind of connection\n"
                  "is slow.\n"
                  "\n"
                  "This message is shown once per session.\n");
            } 
            ++ nwarn;
         }  
     }
     
     if( nn < 0 ){                          /* is bad */
       if (SUMAg_CF->ns_v[cc]) NI_stream_close( SUMAg_CF->ns_v[cc] ) ;
       SUMAg_CF->ns_v[cc] = NULL ; /* this will get checked next time */
       SUMA_S_Errv("Stream %d gone bad. Stream closed. \n", cc);
       
       /* close everything */
       if (!list) list = SUMA_CreateList();
       ED = SUMA_InitializeEngineListData(SE_CloseStream4All);
       if (!SUMA_RegisterEngineListCommand ( list, ED, 
                                          SEF_i, (void*)&cc,  
                                          SES_Suma, (void *)sv, NOPE,   
                                          SEI_Head, NULL)) {  
         fprintf (SUMA_STDERR, 
                  "Error %s: Failed to register command.\n", FuncName);   
       }

       if (!SUMA_Engine (&list)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Engine.\n\a", FuncName);
       }      
      
       continue;  /* skip to next stream */
     }

     if (nn == 0) { /* waiting, come back later */
         continue;
     }
     
     /* if here, stream is good;
        see if there is any data to be read */
      
      if (SUMAg_CF->ns_flags_v[cc] & SUMA_FLAG_WAITING) {
         SUMAg_CF->ns_flags_v[cc] = SUMA_FLAG_CONNECTED;
         SUMA_S_Notev( 
                  "++ NIML connection opened from %s on port %d (%dth steam)\n",
                  NI_stream_name(SUMAg_CF->ns_v[cc]), 
                  SUMAg_CF->TCP_port[cc], cc) ;
      }
   #if 0
      /* not good enough, checks socket only, not buffer */
      nn = NI_stream_readcheck( SUMAg_CF->ns , 1 ) ;
   #else
      nn = NI_stream_hasinput( SUMAg_CF->ns_v[cc] , 1 ) ;
   #endif
   
     if( nn > 0 ){                                   /* has data */
       int ct = NI_clock_time() ;
       if (LocalHead)   
         fprintf(SUMA_STDERR,"%s: reading data stream", FuncName) ;

       nini = NI_read_element( SUMAg_CF->ns_v[cc] , 1 ) ;  /* read it */
         #if SUMA_SUMA_NIML_DEBUG /* debugging corruption of niml ...*/
            nel = (NI_element *)nini ;
            if(   strcmp(nel->name,"SUMA_irgba") == 0 || 
                  strcmp(nel->name,"Node_RGBAb") == 0) 
            {
               int *ibad=NULL;
               
               ibad = (int *)nel->vec[0]; 
               if (ibad[0] > 1000) {
                  fprintf (SUMA_STDERR,
                           "**********\n\tibad[0] = %d\n****************\n", 
                           ibad[0]);
                  fprintf (SUMA_STDOUT,
                           "********** ibad[0] = %d ****************", 
                           ibad[0]);
               }
               if( nel->vec_len  < 1 || nel->vec_filled <  1) {  
                        /* empty element?             */
                  fprintf(SUMA_STDERR,
                           "--------\n"
                           "\tEmpty SUMA_irgba (len = %d, len = %d)\n"
                           "--------\n", 
                     nel->vec_len, nel->vec_filled);
                  fprintf(SUMA_STDOUT,
                           "-------- Empty SUMA_irgba "
                           "(len = %d, filled = %d) --------", 
                     nel->vec_len, nel->vec_filled);
               }
               fprintf (SUMA_STDOUT,"\n");
            }
         #endif
         
       if (LocalHead)   
         fprintf( SUMA_STDERR,
                  " time=%d ms\n",
                  NI_clock_time()-ct) ; ct = NI_clock_time() ;

       if( nini != NULL ) {
         nel = (NI_element *)nini ;
         if (SUMAg_CF->TrackingId_v[cc]) {
            nel_track = NI_get_attribute(nel,"Tracking_ID");
            if (nel_track) {
               id = atoi(nel_track);
               if (id != SUMAg_CF->TrackingId_v[cc] + 1) {
                  /* remember, "StartTracking" nel is the #1 element, 
                     first data element starts at 2 */
                  fprintf (SUMA_STDERR,
                           "Warning %s:\n"
                           " Expected element %d, received element %d.\n",
                           FuncName,  SUMAg_CF->TrackingId_v[cc] + 1 , id );
                  SUMA_BEEP;
               }
               SUMAg_CF->TrackingId_v[cc] = id;
            }
         }
         if (LocalHead)   {
            fprintf( SUMA_STDERR,
                     "%s:     name=%s vec_len=%d vec_filled=%d, vec_num=%d\n", 
                     FuncName, nel->name, nel->vec_len, 
                     nel->vec_filled, nel->vec_num );
         }      
          if (!SUMA_process_NIML_data( nini , sv)) {
             fprintf(SUMA_STDERR,
                     "Error %s: Failed in SUMA_process_NIML_data.\n", FuncName);
          }
      }

      NI_free_element( nini ) ;

      if (LocalHead)   
         fprintf(SUMA_STDERR,"processing time=%d ms\n",NI_clock_time()-ct) ;

     } 
   
   }/* for cc*/
   
   if (ngood == 0) {
      SUMAg_CF->niml_work_on = NOPE;
      SUMAg_CF->Listening = NOPE;
      if (SUMA_NIML_WORKPROC_IO_NOTIFY) {
         SUMA_RETURN(True) ;  /* don't call me back */
      }
         else return (True);
   }
   
   if (SUMA_NIML_WORKPROC_IO_NOTIFY) {
      SUMA_RETURN(False) ;  /* call me back baby*/
   }
      else return (False);
}

int SUMA_which_stream_index (SUMA_CommonFields *cf, char *nel_stream_name)
{
   static char FuncName[]={"SUMA_which_stream_index"};
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   for (i=0; i < SUMA_MAX_STREAMS; ++i) {
      if (strcmp(nel_stream_name, cf->NimlStream_v[i]) == 0) SUMA_RETURN(i);
   }   
   
   SUMA_RETURN(-1);
}

SUMA_Boolean SUMA_niml_hangup (SUMA_CommonFields *cf, char *nel_stream_name, 
                               SUMA_Boolean fromSUMA, SUMA_Boolean killit)
{
   static char FuncName[]={"SUMA_niml_hangup"};
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!nel_stream_name) {
      if (!fromSUMA) { SUMA_SL_Err("NULL stream name"); }
      else { SUMA_SLP_Err("NULL stream name"); }
      SUMA_RETURN(NOPE);
   }
   
   
   i = SUMA_which_stream_index (cf, nel_stream_name);
   
   
   if (i < 0) {
      if (!fromSUMA) { SUMA_SL_Err("Stream not found"); }
      else {  SUMA_SLP_Err("Stream not found"); }
      SUMA_RETURN(NOPE); 
   } else {
      SUMA_LH("Stream found.");
      fprintf (SUMA_STDERR,"%s: stream index %d\n", FuncName, i);
      if (killit) {
         SUMA_LH("Killing stream");
         NI_stream_kill(cf->ns_v[i]);
      }else {
        SUMA_LH("Just closing stream"); 
         NI_stream_close(cf->ns_v[i]);
      }
      cf->ns_v[i] = NULL;
      cf->Connected_v[i] = NOPE; 
      cf->ns_flags_v[i] = 0;
      cf->TrackingId_v[i] = 0;
   }
   
   SUMA_RETURN(YUP);
}

static int SUMA_WriteCheckWaitMax;

int SUMA_GetWriteCheckWaitMax(void) {
   return(SUMA_WriteCheckWaitMax);
}
void SUMA_SetWriteCheckWaitMax(int val) {
   if (val == 0) val = SUMA_WRITECHECKWAITMAX;
   SUMA_WriteCheckWaitMax = val;
}

/*!
   \brief Initiates a call on stream cf->ns_v[si]
   
   \param cf (SUMA_CommonFields *) Overkill common field structure.
                                    Only fields used are the niml 
                                    communication ones.... 
   \param si (int) index of stream to use
   \param fromSUMA (SUMA_Boolean) YUP means call is initiated from SUMA 
   \return NOPE: Sucked
           YUP : Did not suck
   
   \sa SUMA_niml_hangup
*/
SUMA_Boolean SUMA_niml_call ( SUMA_CommonFields *cf, int si, 
                              SUMA_Boolean fromSUMA)
{
   static char FuncName[]={"SUMA_niml_call"};
   int nn=-1, Wait_tot;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (si < 0 || si >= SUMA_MAX_STREAMS) {
      SUMA_SL_Err("Bad value for stream index.");
      SUMA_RETURN(NOPE);
   }
   
     /* find out if the stream has been established already */
      if (cf->ns_v[si]) { /* stream is open, nothing to do */
         cf->ns_flags_v[si] = SUMA_FLAG_CONNECTED;
         if (LocalHead) 
            fprintf(SUMA_STDOUT,"%s: Stream existed, reusing.\n", FuncName);
         fprintf(SUMA_STDOUT,"%s: Connected.\n", FuncName); fflush(SUMA_STDOUT);
      }else {   /* must open stream */              
         /* contact afni */
            SUMA_SetWriteCheckWaitMax(cf->ns_to[si]);
            fprintf( SUMA_STDOUT,
                     "%s: Contacting on %d, maximum wait %.3f sec\n", 
                     FuncName, si, (float)cf->ns_to[si]/1000.0);
            fflush(SUMA_STDOUT);
            cf->ns_v[si] =  NI_stream_open( cf->NimlStream_v[si] , "w" ) ;
            if (!cf->ns_v[si]) {
               cf->ns_flags_v[si] = 0;
               cf->TrackingId_v[si] = 0;

               if (fromSUMA) { SUMA_SLP_Err("NI_stream_open failed (1p)."); }
               else { SUMA_SL_Err("NI_stream_open failed (1)."); }
               SUMA_BEEP;
               cf->Connected_v[si] = !cf->Connected_v[si];
               SUMA_RETURN(NOPE) ;
            }
            if (!strcmp(cf->HostName_v[si],"localhost")) { 
               /* only try shared memory when 
                  AfniHostName is localhost */
               fprintf (SUMA_STDERR, 
                        "%s: Trying local connection...\n", FuncName);
               if( strstr( cf->NimlStream_v[si] , "tcp:localhost:" ) != NULL ) {
                  if (!NI_stream_reopen( cf->ns_v[si] , "shm:WeLikeElvis:1M" )){
                     fprintf (SUMA_STDERR, 
                              "Warning %s: "
                              "Shared memory communcation failed.\n",
                              FuncName);
                  }
               }
            }

            if( cf->ns_v[si] == NULL ){
               if (fromSUMA) { SUMA_SLP_Err("NI_stream_open failed (2p)");} 
               else { SUMA_SL_Err("NI_stream_open failed (2)");}
               SUMA_BEEP; 
               cf->Connected_v[si] = !cf->Connected_v[si];
               cf->ns_flags_v[si] = 0;
               cf->TrackingId_v[si] = 0;

               SUMA_RETURN(NOPE) ;
            }

         Wait_tot = 0;
         while(Wait_tot < SUMA_WriteCheckWaitMax){
            nn = NI_stream_writecheck( cf->ns_v[si] , SUMA_WriteCheckWait) ;
            if( nn == 1 ){ 
               fprintf(stderr,"\n") ; 
               cf->ns_flags_v[si] = SUMA_FLAG_CONNECTED;
               SUMA_RETURN(YUP) ; 
            }
            if( nn <  0 ){ 
               fprintf(stderr,"BAD\n"); 
               cf->Connected_v[si] = !cf->Connected_v[si];
               cf->ns_v[si] = NULL;
               cf->ns_flags_v[si] = 0;
               cf->TrackingId_v[si] = 0;
               SUMA_RETURN(NOPE);
            }
            Wait_tot += SUMA_WriteCheckWait;
            fprintf(SUMA_STDERR,".") ;
         }

         /* make sure you did not exit because of time out */
         if (nn!=1) {
            cf->Connected_v[si] = !cf->Connected_v[si];
            cf->ns_v[si] = NULL;
            cf->ns_flags_v[si] = 0; 
            cf->TrackingId_v[si] = 0;
            SUMA_S_Errv("WriteCheck timed out (> %d ms).\n", 
                        SUMA_WriteCheckWaitMax);
            SUMA_RETURN(NOPE);
         }
      } 

      /* Stream is open */
   SUMA_RETURN(YUP);
}

/*----------------------------------------------------------------------*/
/*! Process NIML data.  
------------------------------------------------------------------------*/

SUMA_Boolean SUMA_process_NIML_data( void *nini , SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_process_NIML_data"};
   int tt = NI_element_type(nini) ;
   int OverInd, loc_ID, iview, *IJK=NULL, N_Node, *FaceSetList=NULL, N_FaceSet;
   int i, I_C = -1, nodeid = -1, iv3[3], dest_SO_ID = -1, 
         N_SOlist, SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS], ip = 0;
   NI_element *nel = NULL ;
   NI_group *ngr = NULL;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *Elm = NULL, *el=NULL;
   char CommString[SUMA_MAX_COMMAND_LENGTH], *nel_surfidcode = NULL, 
         *nel_nodeid = NULL;
   char s[SUMA_MAX_STRING_LENGTH], sfield[100], sdestination[100], ssource[100];
   float **fm, dimfact,  *XYZ=NULL, *NodeList=NULL;
   byte *r, *g, *b;
   byte BrandNew = YUP;
   SUMA_NEW_SO_OPT *nsoopt=NULL;
   SUMA_Boolean Empty_irgba = NOPE,  Found = NOPE;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_SurfaceViewer *svi = NULL;
   SUMA_OVERLAYS * tmpptr; 
   GLfloat *glar_ColorList = NULL;
   SUMA_OVERLAY_PLANE_DATA sopd;
   SUMA_SurfSpecFile *Spec=NULL;
   SUMA_Boolean iselement = YUP;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if( tt < 0 ) {/* should never happen */
      fprintf(SUMA_STDERR,"Error %s: Should never have happened.\n", FuncName);
      SUMA_RETURN(NOPE);
   } 

   SUMA_LH("Checking on nini type");
   /* check if group or element */
   if(tt == NI_GROUP_TYPE) {
      iselement = NOPE; 
      SUMA_LH("Dealing with group");
   } else if (tt == NI_ELEMENT_TYPE) { 
      iselement = YUP; 
      SUMA_LH("Dealing with element");
   } else {
      fprintf(SUMA_STDERR,"Error %s: Not an element, nor a group. \n"
                          "What the hell are you doing?\n", FuncName);
      SUMA_RETURN(NOPE);
   }
   
   
   if (iselement) {
      /* if here, have a single data element;
         process the data based on the element name */

      nel = (NI_element *) nini ;

      if (LocalHead)  {
         fprintf(SUMA_STDERR,
                 "%s:     name=%s vec_len=%d vec_filled=%d, vec_num=%d\n", 
                 FuncName,
                 nel->name, nel->vec_len, nel->vec_filled, nel->vec_num );
      }

      /*--- stream closer ---*/
      if( strcmp(nel->name,"CloseKillStream") == 0) { /* CloseKillStream */
         if (LocalHead) 
            fprintf (SUMA_STDERR,"%s:\nClosing then killing stream %s ...\n",  
                     FuncName, NI_get_attribute(nel, "ni_stream_name"));
         if (!SUMA_niml_hangup (SUMAg_CF, 
                                NI_get_attribute(nel, "ni_stream_name"), 
                                NOPE, YUP)) {
            SUMA_SL_Err("Failed in SUMA_niml_hangup.\n");
            SUMA_RETURN(NOPE);
         }
         SUMA_RETURN(YUP);
      } /* CloseStreamKill */  

      /*--- stream tracking ON ---*/
      if( strcmp(nel->name,"StartTracking") == 0) { /* Start tracking */
         if (LocalHead) 
            fprintf (SUMA_STDERR,"%s:\n"
                                 " Starting NI element tracking for %s ...\n", 
                                 FuncName, 
                                 NI_get_attribute(nel, "ni_stream_name"));
         i = SUMA_which_stream_index(SUMAg_CF, 
                                     NI_get_attribute(nel, "ni_stream_name"));
         if ( i < 0) {
            SUMA_SL_Err("Failed to find stream!\n");
            SUMA_RETURN(NOPE);
         }
         if (NI_get_attribute(nel, "Tracking_ID")) {
            if (atoi(NI_get_attribute(nel, "Tracking_ID")) != 1) {
               SUMA_SL_Err("First tracking element is not 1.\n\n"
                           "Tracking ignored.\n");
               SUMA_RETURN(YUP);
            }
         }
         SUMA_LH("Tracking on ...");
         SUMAg_CF->TrackingId_v[i] = 1; /* this is to be the first element ! */
         SUMA_RETURN(YUP);
      } /* Start tracking */  

      /*--- stream tracking OFF ---*/
      if( strcmp(nel->name,"StopTracking") == 0) { /* Stop tracking */
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s:\n Stopping NI element tracking for %s ...\n", 
                     FuncName, NI_get_attribute(nel, "ni_stream_name"));
         i = SUMA_which_stream_index(SUMAg_CF, 
                                     NI_get_attribute(nel, "ni_stream_name"));
         if ( i < 0) {
            SUMA_SL_Err("Failed to find stream!\n");
            SUMA_RETURN(NOPE);
         }
         SUMA_LH("Tracking Off ...");
         SUMAg_CF->TrackingId_v[i] = 0; /* this is to be the first element ! */
         SUMA_RETURN(YUP);
      } /* Stop tracking */  

      
      if (strcmp(nel->name,"underlay_array") == 0) { /* underlay array */
         SUMA_S_Note("Have underlay array!");
         SUMA_RETURN(YUP) ;
      }
      
      /* New surface mesh_IJK, This one is now obsolete, 
         along with NewNode_XYZ, they were used to send a surface in chunks, now 
         I can send an entire surface. Look at commented out section in    
         SUMA_Mesh_IJK2Mesh_IJK_nel if you want to reuse this chunk*/
      if (strcmp(nel->name,"NewMesh_IJK") == 0) { /* NewMesh_IJK */
         SUMA_SL_Err("Element obsolete. Please use SUMA_SurfaceObject");
         SUMA_RETURN(NOPE) ;
         if( nel->vec_len  < 1 || nel->vec_filled <  1) {/* empty element? */
            fprintf(SUMA_STDERR,"%s: Empty NewMesh_IJK\n", FuncName);
            SUMA_RETURN(NOPE);
         }else {
            if( nel->vec_num != 1 || nel->vec_typ[0] != NI_INT) {
                 fprintf(SUMA_STDERR,"%s: NewMesh_IJK Bad format\n", FuncName);
               SUMA_RETURN(NOPE);
            }
         }
         /* show me nel */
         /* if (LocalHead) SUMA_nel_stdout (nel); */
         /* look for the surface idcode */
         nel_surfidcode = NI_get_attribute(nel, "surface_idcode");
         if (SUMA_IS_EMPTY_STR_ATTR(nel_surfidcode)) 
            nel_surfidcode = NI_get_attribute(nel, "domain_parent_idcode");
         if (SUMA_IS_EMPTY_STR_ATTR(nel_surfidcode)) {
            fprintf( SUMA_STDERR,
                     "Error %s: surface_idcode missing in nel (%s).\n", 
                     FuncName, nel->name);
            SUMA_RETURN(NOPE);
         } 

         SUMA_LH("Checking for new surface...");
         SO = SUMA_findSOp_inDOv (nel_surfidcode, SUMAg_DOv, SUMAg_N_DOv);
         if (SO) {
            fprintf( SUMA_STDERR,
                     "Warning %s: nel idcode was found in DOv.\n"
                     "Checking for mesh compatibility\n", FuncName);
            if (SO->N_FaceSet * SO->FaceSetDim == nel->vec_len) {
               fprintf(SUMA_STDERR,
                       "Note %s: Mesh dimensions match. \n"
                       "New mesh will be adopted.\n", FuncName);
            } else {
               fprintf(SUMA_STDERR,
                       "Error %s: Mesh dimensions mismatch.\n", FuncName);
               SUMA_RETURN(NOPE);
            }
         }

         /* get the number of nodes */
         if (!NI_get_attribute(nel, "N_Node")) {
            fprintf( SUMA_STDERR, 
                     "Error %s: NULL or non existent N_Node field.\n", FuncName);
            SUMA_RETURN(NOPE);   
         }

         if (LocalHead) 
            fprintf( SUMA_STDERR,"Number of nodes:%s...\n", 
                     NI_get_attribute(nel, "N_Node"));
         N_Node = atoi(NI_get_attribute(nel, "N_Node"));
         if (N_Node <= 0 || N_Node > 1000000) {
            fprintf(SUMA_STDERR,
                    "Error %s: Bad number of nodes %d \n"
                    "(limit of 1000000 nodes.)\n", FuncName, N_Node);
            SUMA_RETURN(NOPE);
         }

         if (!SO) { 
            SUMA_LH("A brand new surface.");
            BrandNew = YUP;
            NodeList = (float *)SUMA_malloc(3 * N_Node * sizeof(float)); 
                     /* do not use calloc so that you can see something ... */
            FaceSetList = (int *)SUMA_malloc(nel->vec_len * sizeof(int)); 
            if (!NodeList || !FaceSetList) {
               SUMA_SL_Crit("Failed to allocate for NodeList || FaceSetList");
               SUMA_RETURN(NOPE);
            }
            IJK = (int *)nel->vec[0];
            N_FaceSet = nel->vec_len / 3; 
            if (nel->vec_len % 3) {
               fprintf(SUMA_STDERR,
                       "Error %s: Bad number of elements in IJK vector\n"
                       " not divisible by 3! %d\n", FuncName, nel->vec_len);
               SUMA_RETURN(NOPE);
            }
            SUMA_LH("Copying new mesh");
            for (i=0; i < nel->vec_len; ++i) FaceSetList[i] = IJK[i];
            /* Now form the new surface */
            SUMA_LH("Now forming new surface");
            nsoopt = SUMA_NewNewSOOpt();
            nsoopt->DoNormals = NOPE; nsoopt->DoMetrics = NOPE; 
            nsoopt->DoCenter = NOPE; 
            nsoopt->idcode_str = SUMA_copy_string(nel_surfidcode);      
            SO = SUMA_NewSO(&NodeList, N_Node, &FaceSetList, N_FaceSet, nsoopt);
            nsoopt=SUMA_FreeNewSOOpt(nsoopt); 
         } else {
            SUMA_LH("A refit of an existing surface.");
            BrandNew = NOPE;
            if (N_Node != SO->N_Node) {
               fprintf( SUMA_STDERR,
                        "Error %s: Mismatch in number of nodes between new \n"
                        "mesh and pre-existing one (%d vs %d)\n", 
                        FuncName, N_Node, SO->N_Node);
               SUMA_RETURN(NOPE);
            }
            IJK = (int *)nel->vec[0];
            for (i=0; i < nel->vec_len; ++i) SO->FaceSetList[i] = IJK[i];
         }

         /* work the mesh a little and add it to DOv NO LONGER DONE HERE...*/
         SO->Group = SUMA_copy_string(NI_get_attribute(nel, "Group"));
         if (!SO->Group) 
            SO->Group = SUMA_copy_string(NI_get_attribute(nel, "Subject_Label"));
         SO->State = SUMA_copy_string(NI_get_attribute(nel, "State"));
         if (!SO->State) 
            SO->State = SUMA_copy_string(NI_get_attribute(nel, "Layer_Name"));
         SO->Label = SUMA_copy_string(NI_get_attribute(nel, "Label"));
         if (!SO->Label) 
            SO->Label = SUMA_copy_string(NI_get_attribute(nel, "Object_Label"));
         SO->EmbedDim = atoi(NI_get_attribute(nel, "EmbedDim"));
         if (!SO->EmbedDim) 
            SO->EmbedDim = atoi(NI_get_attribute(nel, "Embedding_Dimension"));
         SO->AnatCorrect = atoi(NI_get_attribute(nel, "AnatCorrect"));
         if (!SO->AnatCorrect) {
            char *tmp = NI_get_attribute(nel, "Anatomically_Correct");
            if (tmp) {
               if (strstr(tmp,"yes")) SO->AnatCorrect = 1;
               else if (strstr(tmp,"no")) SO->AnatCorrect = 0;
            }
         }
         /* add this surface to DOv */
         if (BrandNew) {
            if (!SUMA_AddDO(SUMAg_DOv, &(SUMAg_N_DOv), (void *)SO,  
                            SO_type, SUMA_WORLD)) {
               fprintf(SUMA_STDERR,"Error %s: Error Adding DO\n", FuncName);
               SUMA_RETURN(NOPE);
            }
         }

         /* don't free nel, it's freed later on */
         SUMA_RETURN(YUP) ;
      }/* NewMesh_IJK */   

      if (strcmp(nel->name,"PrepNewSurface") == 0) { /* PrepNewSurface */
         int viewopt = 0;
         /* show me nel */
         /* if (LocalHead) SUMA_nel_stdout (nel); */
         /* look for the surface idcode */
         nel_surfidcode = NI_get_attribute(nel, "surface_idcode");
         if (SUMA_IS_EMPTY_STR_ATTR(nel_surfidcode)) 
            nel_surfidcode = NI_get_attribute(nel, "domain_parent_idcode");
         if (SUMA_IS_EMPTY_STR_ATTR(nel_surfidcode)) {
            fprintf(SUMA_STDERR,
                    "Error %s: surface_idcode missing in nel (%s).\n", 
                    FuncName, nel->name);
            SUMA_RETURN(NOPE);
         } 
         SUMA_LH("Looking for  surface...");
         SO = SUMA_findSOp_inDOv (nel_surfidcode, SUMAg_DOv, SUMAg_N_DOv);
         if (!SO) {
            fprintf(SUMA_STDERR,
                    "Error %s: nel idcode was not found in DOv.\n", FuncName);
            SUMA_RETURN(NOPE);
         }

         if (LocalHead) 
            fprintf(SUMA_STDERR, "%s: Surface SO about to be prepped: "
                                 "Label %s, State %s, Group %s\n", 
                                 FuncName, SO->Label, SO->State, SO->Group);
         
         #if 0
         if (NI_get_attribute(nel, "VolParFilecode")) {
            SO->VolPar = SUMA_VolPar_Attr (NI_get_attribute(nel, 
                                                            "VolParFilecode"));
            if (!SO->VolPar) {
               SUMA_S_Err("Failed in SUMA_VolPar_Attr");
               SUMA_RETURN(NOPE);
            }
            SO->SUMA_VolPar_Aligned = YUP; /* Surface should already be in 
                                              alignment with volume, should not 
                                              call SUMA_Align_to_VolPar ... */
         }
         #else
            /* VolPar should have been dealt with by now */
            if (!SO->VolPar) SO->SUMA_VolPar_Aligned = NOPE;
            else SO->SUMA_VolPar_Aligned = YUP; /* Surface should already be in 
                                                   alignment with volume, should 
                                                   not call SUMA_Align_to_VolPar 
                                                   ... */
         #endif

         /* make this surface friendly for suma */
         if (!SUMA_PrepSO_GeomProp_GL(SO)) {
            SUMA_S_Err("Failed in SUMA_PrepSO_GeomProp_GL");
            SUMA_RETURN(NOPE);
         }
         /* Add this surface to SUMA's displayable objects */
         if (!SUMA_PrepAddmappableSO(SO, SUMAg_DOv, &(SUMAg_N_DOv), 
                                     0, SUMAg_CF->DsetList)) {
            SUMA_S_Err("Failed to add mappable SOs ");
            SUMA_RETURN(NOPE);
         }
         /* create a fake spec, be damned gates of spec! */
         Spec = SUMA_SOGroup_2_Spec (&SO, 1);

         /* register the new group with SUMA */
         if (!SUMA_RegisterGroup(SUMAg_CF, Spec)) {
            SUMA_SL_Err("Failed to register group");
            SUMA_RETURN(NOPE);
         }
 
	      /* Register the surfaces in Spec file with the surface 
            viewer and perform setups */
         viewopt = 0;
	      fprintf (SUMA_STDERR, 
                  "%s: Registering surfaces with surface viewers, "
                  "viewopt = %d...\n", FuncName, viewopt);

         for (i = 0; i< SUMA_MAX_SURF_VIEWERS; ++i) {
            if (!SUMA_SetupSVforDOs (*Spec, SUMAg_DOv, SUMAg_N_DOv, 
                     &(SUMAg_SVv[i]), viewopt)) {
			      fprintf (SUMA_STDERR, 
                        "Error %s: Failed in SUMA_SetupSVforDOs function.\n", 
                        FuncName);
			      SUMA_RETURN(NOPE);
		      }
	      }

         if (!SUMA_FreeSpecFields(Spec)) {
            SUMA_S_Err("Failed to free spec fields");
         }
         SUMA_free(Spec); Spec = NULL;

         /* switch viewer 0 to the group in question */
         if (!sv) sv = &(SUMAg_SVv[0]);
         if (!SUMA_SwitchGroups (sv, SO->Group)) {
            SUMA_SL_Err("Failed to switch groups!");
            SUMA_RETURN(NOPE);
         }
         if ((i = SUMA_WhichState(SO->State, sv, sv->CurGroupName)) < 0) {
            SUMA_SL_Err("Failed to find state!");
            SUMA_RETURN(NOPE);
         } else {
            if (!SUMA_SwitchState(  SUMAg_DOv, SUMAg_N_DOv, 
                                    sv, i, sv->CurGroupName)) {
               SUMA_SL_Err("Failed to switch states!");
               SUMA_RETURN(NOPE);
            }
         }

         /* file a redisplay request 
         In the past, when surface was sent in chunks, redisplay was
         held until geometry was received, now that a whole surface can be sent 
         at once, redisplay is appropriate here ZSS Sept. 06*/
         if (LocalHead) 
            fprintf(SUMA_STDERR, "%s: Redisplaying all visible...\n", FuncName);
         if (!list) list = SUMA_CreateList();
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                             SES_SumaFromAny, sv);
         if (!SUMA_Engine (&list)) {
            fprintf(SUMA_STDERR, 
               "Error %s: SUMA_Engine call failed.\n", FuncName);
            SUMA_RETURN(NOPE);
         }

         /* do we need to notify AFNI ? */
         /* YOU'll need to do the same using Send2Matlab
            The only difference is the changing stream index
            So you should set this at the top and use the
            same instructions.
            Also, you'll need to pass the stream along with the
            SE_ToggleConnected and SE_SetAfniThisSurf and others
            Or perhaps consider checking all applicable Connected_v
            at the targets in SUMA_Engine.c */ 
         if (NI_get_attribute(nel, "Send2Afni")) {
            SUMA_LH("Attempting to talk to AFNI");
            if (!SO->VolPar) {
               SUMA_SL_Err("Have no VolPar, cannot send to AFNI!\n"
                           "Command ignored.");
            } else {
               if (!SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX]) { 
                  /* need to send a toggle request */
                  if (LocalHead) 
                     fprintf(SUMA_STDERR, 
                        "%s: Sending talk request...\n", FuncName);
                  if (!list) list = SUMA_CreateList();
                  SUMA_REGISTER_HEAD_COMMAND_NO_DATA( list, SE_ToggleConnected, 
                                                      SES_SumaFromAny, sv);
                  if (!SUMA_Engine (&list)) {
                     fprintf( SUMA_STDERR, 
                              "Warning %s: "
                              "SUMA_Engine call failed.\nContinuing...", 
                              FuncName);
                  } 
               } else {
                  SUMA_LH("Looks like they're talking already");
               }
               /* now send the surface */
               SUMA_LH("Now trying to send surface");
               if (!SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX]) { 
                  fprintf( SUMA_STDERR, 
                           "Warning %s: "
                           "Failed to open connection.\nContinuing...", 
                           FuncName);
               } else {
                  SUMA_LH("Making Call");
                  if (!list) list = SUMA_CreateList();
                  ED = SUMA_InitializeEngineListData (SE_SetAfniThisSurf);
                  if (!( Elm = SUMA_RegisterEngineListCommand (  
                                 list, ED, 
                                 SEF_cp, (void *)SO->idcode_str, 
                                 SES_Suma, NULL, NOPE, 
                                 SEI_Tail, NULL ))) {
                     fprintf( SUMA_STDERR,
                              "Error %s: Failed to register command\n"
                              "Ignoring ...", FuncName);
                  }else {
                     int ti= 0;
                     SUMA_RegisterEngineListCommand (  
                        list, ED, 
                        SEF_s, (void *)("NodeList, FaceSetList, NodeNormList"), 
                        SES_Suma, NULL, NOPE, 
                        SEI_In, Elm );
                     SUMA_RegisterEngineListCommand (  
                        list, ED, 
                        SEF_i, (void *)&ti, /* 0, be quiet about it */
                        SES_Suma, NULL, NOPE, 
                        SEI_In, Elm );
                     if (!SUMA_Engine (&list)) {
                        fprintf(SUMA_STDERR, 
                           "Warning %s: SUMA_Engine call failed.\nContinuing...",
                           FuncName);
                     }
                  }
               }   
            }
         } else {
            SUMA_LH("No talking to AFNI requested.");
         }
         /* don't free nel, it's freed later on */
         SUMA_RETURN(YUP) ;
      } /* PrepNewSurface */

      /* NewNode_XYZ NOW OBSOLETE, see comment for NewMesh_IJK*/
      if( strcmp(nel->name,"NewNode_XYZ") == 0) {/* NewNode_XYZ */
         SUMA_SL_Err("Obsolete element, please use SUMA_SurfaceObject "
                     "element instead");
         SUMA_RETURN(NOPE);
         if( nel->vec_len  < 1 || nel->vec_filled <  1) {/* empty element? */
            fprintf(SUMA_STDERR,"%s: Empty NewNode_XYZ\n", FuncName);
            SUMA_RETURN(NOPE);
         }else {
            if( nel->vec_num != 1 || nel->vec_typ[0] != NI_FLOAT) {
                 fprintf(SUMA_STDERR,"%s: NewNode_XYZ Bad format\n", FuncName);
               SUMA_RETURN(NOPE);
            }
         }
         /* show me nel */
         /* if (LocalHead) SUMA_nel_stdout (nel); */

         /* look for the surface idcode */
         nel_surfidcode = NI_get_attribute(nel, "surface_idcode");
         if (SUMA_IS_EMPTY_STR_ATTR(nel_surfidcode)) 
               nel_surfidcode = NI_get_attribute(nel, "domain_parent_idcode");
         if (SUMA_IS_EMPTY_STR_ATTR(nel_surfidcode)) {
            fprintf(SUMA_STDERR,
                    "Error %s: surface_idcode missing in nel (%s).\n", 
                    FuncName, nel->name);
            SUMA_RETURN(NOPE);
         } 

         SO = SUMA_findSOp_inDOv (nel_surfidcode, SUMAg_DOv, SUMAg_N_DOv);
         if (!SO) {
            fprintf(SUMA_STDERR,"Error %s:%s: nel idcode is not found in DOv.\n",
                                FuncName, nel->name);
            SUMA_RETURN(NOPE);
         }

         /* now copy the new node coordinates over the old ones */
         if (nel->vec_len != SO->N_Node * 3) {
            fprintf(SUMA_STDERR,"Error %s:\n"
                                "Expected %d * 3 = %d values, found %d\n", 
                                FuncName, SO->N_Node, 
                                SO->N_Node * 3, nel->vec_len);
            SUMA_RETURN(NOPE);
         }

         XYZ = (float *)nel->vec[0];
         for (i=0; i < nel->vec_len; ++i) SO->NodeList[i] = XYZ[i];

         /* don't free nel, it's freed later on */
         SUMA_RETURN(YUP) ;
      } /* NewNode_XYZ */

      /* Node_XYZ */
      if( strcmp(nel->name,"Node_XYZ") == 0) {/* Node_XYZ */
         if( nel->vec_len  < 1 || nel->vec_filled <  1) {/* empty element?*/
            fprintf(SUMA_STDERR,"%s: Empty Node_XYZ\n", FuncName);
            SUMA_RETURN(NOPE);
         }else {
            if( nel->vec_num != 1 || nel->vec_typ[0] != NI_FLOAT) {
                 fprintf(SUMA_STDERR,"%s: Node_XYZ Bad format\n", FuncName);
               SUMA_RETURN(NOPE);
            }
         }
         /* show me nel */
         /* if (LocalHead) SUMA_nel_stdout (nel); */

         /* look for the surface idcode */
         nel_surfidcode = NI_get_attribute(nel, "surface_idcode");
         if (SUMA_IS_EMPTY_STR_ATTR(nel_surfidcode)) 
               nel_surfidcode = NI_get_attribute(nel, "domain_parent_idcode");
         if (SUMA_IS_EMPTY_STR_ATTR(nel_surfidcode)) {
            fprintf( SUMA_STDERR,
                     "Error %s: surface_idcode missing in nel (%s).\n", 
                     FuncName, nel->name);
            SUMA_RETURN(NOPE);
         } 

         SO = SUMA_findSOp_inDOv (nel_surfidcode, SUMAg_DOv, SUMAg_N_DOv);
         if (!SO) {
            fprintf(SUMA_STDERR,"Error %s:%s: nel idcode is not found in DOv.\n",
                                FuncName, nel->name);
            SUMA_RETURN(NOPE);
         }

         /* now copy the new node coordinates over the old ones */
         if (nel->vec_len != SO->N_Node * 3) {
            fprintf(SUMA_STDERR,"Error %s:\n"
                                "Expected %d * 3 = %d values, found %d\n", 
                                FuncName, SO->N_Node, 
                                SO->N_Node * 3, nel->vec_len);
            SUMA_RETURN(NOPE);
         }

         XYZ = (float *)nel->vec[0];
         for (i=0; i < nel->vec_len; ++i) SO->NodeList[i] = XYZ[i];

         /* must recompute normals */
         SUMA_RECOMPUTE_NORMALS(SO);

         /* file a redisplay request */
         if (LocalHead) fprintf(SUMA_STDERR, "%s: Redisplaying all visible...\n",                                 FuncName);
         if (!list) list = SUMA_CreateList();
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA( list, SE_Redisplay_AllVisible, 
                                             SES_SumaFromAny, sv);
         
         /* Need to deal with "Send2Matlab" as per comment above */
         if (NI_get_attribute(nel, "Send2Afni")) {
            if (SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX]) {
               SUMA_LH("Putting request for sending to afni ...");
               ED = SUMA_InitializeEngineListData (SE_SetAfniThisSurf);
               if (!( Elm = SUMA_RegisterEngineListCommand (  
                                 list, ED, 
                                 SEF_cp, (void *)SO->idcode_str, 
                                 SES_Suma, NULL, NOPE, 
                                 SEI_Tail, NULL ))) {
                  fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", 
                          FuncName);
                  SUMA_RETURN(NOPE);
               }

               /* You could save time and not send the NodeNormList 
                  but that means AFNI will end up with a bad set of 
                  normals for the final version of the surface
                  not a good idea... */
                  SUMA_RegisterEngineListCommand ( 
                        list, ED, 
                        SEF_s, (void *)("NodeList, NodeNormList"), 
                        SES_Suma, NULL, NOPE, 
                        SEI_In, Elm );
                  { int ti = 0; /* keep it quiet */
                     SUMA_RegisterEngineListCommand ( list, ED, 
                                                   SEF_i, (void *)&ti, 
                                                   SES_Suma, NULL, NOPE, 
                                                   SEI_In, Elm );
                  }
            } else {
               if (LocalHead) {
                  SUMA_SL_Note("Cannot send surface to afni, "
                               "no connection established");
               }
            }
         } 

         if (!SUMA_Engine (&list)) {
            fprintf( SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", 
                     FuncName);
            SUMA_RETURN(NOPE);
         }

         /* don't free nel, it's freed later on */
         SUMA_RETURN(YUP) ;


      }/* Node_XYZ */

      /* SUMA_irgba Node colors */
      if(   strcmp(nel->name,"SUMA_irgba") == 0 || 
            strcmp(nel->name,"Node_RGBAb") == 0) {/* SUMA_irgba */
         SUMA_OVERLAYS *ColPlane=NULL;
         int itmp=-1,popit = 0;
         
         if( nel->vec_len  < 1 || nel->vec_filled <  1) { /* empty element?  */
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: Empty SUMA_irgba.\n", FuncName);
            Empty_irgba = YUP;
           }else {
            if(   nel->vec_num != 5 || 
                  nel->vec_typ[0] != NI_INT || 
                  nel->vec_typ[1] != NI_BYTE ||
                  nel->vec_typ[2] != NI_BYTE || 
                  nel->vec_typ[3] != NI_BYTE) {
                 fprintf(SUMA_STDERR,"%s: SUMA_irgba Bad format\n", FuncName);
               SUMA_RETURN(NOPE);
           }
         }
         #if SUMA_SUMA_NIML_DEBUG
            fprintf(SUMA_STDERR,"Warning %s:\nSleeping ONLY ...\n", FuncName);
            NI_sleep(200);
            SUMA_RETURN(YUP);

            if (0) {  /* At times, I found the value in nel->vec[0] 
                        to be corrupted, use this to check on it */
               int *ibad;
               ibad = (int *)nel->vec[0]; 
               fprintf (SUMA_STDERR,"ibad[0] = %d\n", ibad[0]);
            }
         #endif

         /* show me nel */
         /* if (LocalHead) SUMA_nel_stdout (nel); */

         /* look for the surface idcode */
         nel_surfidcode = NI_get_attribute(nel, "surface_idcode");
         if (SUMA_IS_EMPTY_STR_ATTR(nel_surfidcode)) 
            nel_surfidcode = NI_get_attribute(nel, "domain_parent_idcode");
         if (SUMA_IS_EMPTY_STR_ATTR(nel_surfidcode)) {
            fprintf( SUMA_STDERR,
                     "Error %s: surface_idcode missing in nel (%s).\n", 
                     FuncName, nel->name);
            SUMA_RETURN(NOPE);
         } 

         SO = SUMA_findSOp_inDOv (nel_surfidcode, SUMAg_DOv, SUMAg_N_DOv);
         if (!SO) {
            fprintf( SUMA_STDERR,
                     "Error %s:%s: nel idcode is not found in DOv.\n", 
                     FuncName, nel->name);
            SUMA_RETURN(NOPE);
         }

         /* store the node colors */
         /* create a color overlay plane */
         /* you could create an overlay plane with partial node coverage 
            but you'd have to clean up and SUMA_reallocate
            with each new data sent since the number of colored nodes will 
            change. So I'll allocate for the entire node list 
            for the FuncAfni_0 color plane although only some values will 
            be used*/

         sopd.Type = SOPT_ibbb;
         sopd.Source = SES_Afni;
         sopd.GlobalOpacity = SUMA_AFNI_COLORPLANE_OPACITY;
         sopd.isBackGrnd = NOPE;
         sopd.Show = YUP;
         /* dim colors from maximum intensity to 
            preserve surface shape highlights, 
            division by is no longer necessary.
         */
         sopd.DimFact = SUMA_DIM_AFNI_COLOR_FACTOR;
         if (!Empty_irgba) {
            sopd.i = (void *)nel->vec[0];
            sopd.r = (void *)nel->vec[1];
            sopd.g = (void *)nel->vec[2];
            sopd.b = (void *)nel->vec[3];
            sopd.a = NULL;
            sopd.N = nel->vec_len;
         } else {
            sopd.i = sopd.r = sopd.g = sopd.b = sopd.a = NULL;
            sopd.N = 0;
         }

         if (!SUMA_Fetch_OverlayPointer(  SO->Overlays, SO->N_Overlays, 
                                          "FuncAfni_0", 
                                           &itmp)) {
            /* first timer, pop it up */
            popit = 1;
         } else popit = 0;
         
         if (!SUMA_iRGB_to_OverlayPointer (  SO, "FuncAfni_0", &sopd, &OverInd, 
                                             SUMAg_DOv, SUMAg_N_DOv, 
                                             SUMAg_CF->DsetList)) {
            SUMA_SLP_Err("Failed to fetch or create overlay pointer.");
            SUMA_RETURN(NOPE);
         }
         if (popit) {
            ColPlane = SUMA_Fetch_OverlayPointer(SO->Overlays, SO->N_Overlays, 
                                                 "FuncAfni_0", 
                                                 &itmp);
            if (!ColPlane) {
               SUMA_S_Errv("Failed to find dset %s\n", 
                           "FuncAfni_0"); 
            } else {
               if (LocalHead) 
                  fprintf (SUMA_STDERR,
                           "%s: Retrieved ColPlane named %s\n", 
                           FuncName, ColPlane->Name);
               SUMA_InitializeColPlaneShell(SO, ColPlane);
               SUMA_UpdateColPlaneShellAsNeeded(SO); 
                              /* update other open ColPlaneShells */
               /* If you're viewing one plane at a time, do a remix */
               if (SO->SurfCont->ShowCurForeOnly) SUMA_RemixRedisplay(SO);
            }
         }
         /* register a color remix request */
         if (LocalHead) 
            fprintf( SUMA_STDERR, 
                     "%s: Setting Remix Flag for all related surfaces. ...\n", 
                     FuncName);
         if(!SUMA_SetRemixFlag (SO->idcode_str, SUMAg_SVv, SUMAg_N_SVv)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_SetRemixFlag.\n", FuncName);
            SUMA_RETURN(NOPE);
         }

         /* file a redisplay request */
         if (LocalHead) 
            fprintf(SUMA_STDERR, "%s: Redisplaying all visible...\n", FuncName);
         if (!list) list = SUMA_CreateList();
         if (strcmp(nel->name,"SUMA_irgba") == 0) {
            /* call from AFNI */
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA( list, SE_Redisplay_AllVisible, 
                                                SES_SumaFromAfni, sv);
         } else {
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                                SES_SumaFromAny, sv);
         }

         if (!SUMA_Engine (&list)) {
            fprintf( SUMA_STDERR, 
                     "Error %s: SUMA_Engine call failed.\n", FuncName);
            SUMA_RETURN(NOPE);
         }

         /* don't free nel, it's freed later on */
         SUMA_RETURN(YUP) ;


      }/* SUMA_irgba */

      if (!strcmp(nel->name,"AuRevoir")) {
         int cc = SUMA_AFNI_STREAM_INDEX;
         /* Afni's gone to sleep */
         SUMAg_CF->Connected_v[cc] = NOPE;
         if (SUMAg_CF->ns_v[cc]) 
            NI_stream_close( SUMAg_CF->ns_v[cc] ) ;
         SUMAg_CF->ns_v[cc] = NULL ; 
         SUMA_S_Note("AFNI has bid us farewell");
         if (!list) list = SUMA_CreateList();
         ED = SUMA_InitializeEngineListData(SE_CloseStream4All);
         if (!SUMA_RegisterEngineListCommand ( list, ED, 
                                          SEF_i, (void*)&cc,  
                                          SES_Suma, (void *)sv, NOPE,   
                                          SEI_Head, NULL)) {  
            fprintf (SUMA_STDERR, 
                  "Error %s: Failed to register command.\n", FuncName);   
         }

         if (!SUMA_Engine (&list)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_Engine.\n\a", FuncName);
         }
         
         SUMA_RETURN(YUP) ;
      }
      
      if ( strcmp(nel->name,"3dGroupInCorr_setup") == 0 ){ 
         SUMA_LH("Got me a 3dGroupInCorr_setup");
         if (!SUMA_GICOR_setup_func( SUMAg_CF->ns_v[SUMA_GICORR_LINE], nel )) {
            SUMA_S_Err("Catastropha!");
            SUMA_RETURN(NOPE);
         }
         SUMAg_CF->Connected_v[SUMA_GICORR_LINE] = YUP;
         SUMA_RETURN(YUP) ;
      } 
      
      if( strcmp(nel->name,"3dGroupInCorr_dataset") == 0 ){  
         SUMA_LH("Got me a 3dGroupInCorr_dataset");
         if (!SUMA_GICOR_process_dataset( nel  ) ) {
            SUMA_S_Err("Maledizione!");
            SUMA_RETURN(NOPE);
         }
         SUMA_RETURN(YUP) ;
      }
      
      /*** If here, then name of element didn't match anything ***/

      SUMA_S_Errv("Unknown NIML input: %s\n", nel->name) ;
      SUMA_RETURN(NOPE) ;
   } /* end parse nels */ else { /* is group */
      ngr = (NI_group *) nini ;
      if( strcmp(ngr->name,"SUMA_crosshair") == 0) {/* SUMA_crosshair */
         nel = SUMA_FindNgrNamedElement(ngr, "SUMA_crosshair_xyz"); /* XYZ */
         if (!nel) {
            SUMA_S_Err("Missing bare minimum of crosshair group");
            SUMA_RETURN(NOPE);
         }
         {/* SUMA_crosshair_xyz */
            int found_type = 0;
            SUMA_SurfaceObject *SOaf=NULL;
            /* Do it for all viewers */
            for (iview = 0; iview < SUMAg_N_SVv; ++iview) {
               found_type = 0;
               svi = &(SUMAg_SVv[iview]);
               SUMA_LHv("Processing viewer %c\n", 65+iview); 
               if (svi->LinkAfniCrossHair) {/* link cross hair */
                  /* look for the surface idcode */
                  nel_surfidcode = NI_get_attribute(nel, "surface_idcode");
                  if (SUMA_IS_EMPTY_STR_ATTR(nel_surfidcode)) 
                     nel_surfidcode = 
                        NI_get_attribute(nel, "domain_parent_idcode");
                  if (SUMA_IS_EMPTY_STR_ATTR(nel_surfidcode)) {
                     if (LocalHead) 
                        fprintf(SUMA_STDERR,
                               "%s: surface_idcode missing in nel (%s), "
                               "using svi->Focus_SO_ID.\n", FuncName, nel->name);
                     dest_SO_ID = svi->Focus_SO_ID; /* default */
                     SOaf = (SUMA_SurfaceObject *)
                                 (SUMAg_DOv[svi->Focus_SO_ID].OP);
                  } else {
                     SOaf = SUMA_findSOp_inDOv (nel_surfidcode, 
                                                SUMAg_DOv, SUMAg_N_DOv);
                     if (!SOaf) {
                        SUMA_S_Warn("AFNI sending unkown id, "
                                    "taking default for viewer");
                        SOaf = (SUMA_SurfaceObject *)
                                       (SUMAg_DOv[svi->Focus_SO_ID].OP);
                     }
                     /* first try to find out if one of the displayed surfaces 
                        is or has a parent equal to nel_surfidcode */
                     if (LocalHead) 
                        fprintf (SUMA_STDERR,
                                 "%s: Searching displayed surfaces.\n", 
                                 FuncName);
                     N_SOlist = SUMA_RegisteredSOs(svi, SUMAg_DOv, SOlist);
                     Found = NOPE;
                     i = 0;
                     while (i < N_SOlist && !Found) { 
                        SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SOlist[i]].OP);
                        SUMA_LHv("Checking %s\n   %s versus\n   %s\n", 
                                 SO->Label, nel_surfidcode, SO->idcode_str);
                        if (strcmp(nel_surfidcode, SO->idcode_str) == 0) {
                           Found = YUP;
                           dest_SO_ID = SOlist[i];
                           found_type = 1; /* found surface currently 
                                              in viewer */
                        }
                        ++i;
                     }
                     if (!Found) { /* try for the parent */
                        i = 0;
                        while (i < N_SOlist && !Found) { 
                           SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SOlist[i]].OP);
                           SUMA_LHv("Checking %s\n   %s versus\n   %s\n", 
                                    SO->Label, nel_surfidcode, 
                                    SO->LocalDomainParentID);
                           if (SUMA_isRelated(SOaf, SO, 1)) { 
                              /* ZSS Aug. 06 (used to be: 
                                 (strcmp( nel_surfidcode, 
                                          SO->LocalDomainParentID) == 0) */
                              Found = YUP;
                              dest_SO_ID = SOlist[i];
                              found_type = 2;   /* found related surface 
                                                   currently in viewer */
                          }
                           ++i;
                        }   
                     }
                     /* if not found, look for any DO */
                     if (!Found) {
                        if (LocalHead) 
                           fprintf (SUMA_STDERR,
                                    "%s: None of the displayed surfaces "
                                    "(or their parents) match nel_surfidcode. "
                                    "Trying all of DOv...\n", FuncName);
                        dest_SO_ID = SUMA_findSO_inDOv ( nel_surfidcode, 
                                                         SUMAg_DOv, SUMAg_N_DOv);
                        if (dest_SO_ID < 0) {
                           if (LocalHead) 
                              fprintf( SUMA_STDERR,
                                       "%s:%s: nel idcode is not "
                                       "found in DOv.\n", 
                                       FuncName, nel->name);            
                           dest_SO_ID = svi->Focus_SO_ID; 
                        } else { /* good, set SO accordingly */
                           SO = (SUMA_SurfaceObject *)(SUMAg_DOv[dest_SO_ID].OP);
                           if (LocalHead) 
                              fprintf( SUMA_STDOUT,
                                       "%s: DOv[%d] Matched idcode for "
                                       "surface (%s)\n", 
                                       FuncName, dest_SO_ID, SO->Label);
                        }
                        found_type = 3;   /* found surface NOT in viewer */
                     }
                  }

                  SO = (SUMA_SurfaceObject *)(SUMAg_DOv[dest_SO_ID].OP);

                  if (LocalHead) SUMA_nel_stdout (nel);

                  /* check for node id */
                  nel_nodeid = NI_get_attribute (nel, "surface_nodeid");
                  if (!nel_nodeid) nodeid = -1;
                  else {
                     if (strlen(nel_nodeid)) 
                           nodeid = (int)strtod(nel_nodeid, NULL);
                     else nodeid = -1;
                  }

                  /*-- check element for suitability --*/
                  if( nel->vec_len    < 1 || nel->vec_filled <  1) {  
                                    /* empty element?             */
                     SUMA_SLP_Warn ("Empty crosshair xyz.\n");
                     SUMA_RETURN(YUP);
                  }

                  if(   nel->vec_len != 3 || nel->vec_num != 1 || 
                        nel->vec_typ[0] != NI_FLOAT) {
                     SUMA_SLP_Err(  "SUMA_crosshair_xyz requires\n"
                                    "3 floats in one vector.\n");
                     SUMA_RETURN(NOPE);
                  }


                  /* nodeid is supplied, even if the distance from the cross hair 
                     to the node is large,  set a limit */
                  if (nodeid >= 0) {
                     SUMA_LH("Node index courtesy of AFNI");
                     if (SO->AnatCorrect == YUP) {
                        I_C = nodeid; /* node index and XYZ are set by AFNI */
                        XYZ = (float *)SUMA_malloc(3*sizeof(float));
                        {  float *tf = nel->vec[0];
                           XYZ[0] = tf[0]; XYZ[1] = tf[1]; XYZ[2] = tf[2]; }
                     } else { 
                        I_C = nodeid; /* node index is set by AFNI */
                        XYZ = SUMA_XYZmap_XYZ ( nel->vec[0], SO, 
                                                SUMAg_DOv, SUMAg_N_DOv, &I_C);
                        if (!XYZ) {
                           XBell (svi->X->DPY, 50);             
                           SUMA_SL_Warn("XYZ could not be determined\n"
                                        "No action taken.");
                           break;
                        }
                        I_C = nodeid; /* node index is set by AFNI */
                     }
                  } else {
                     SUMA_LH("Searching for node index.");
                     /* set the cross hair XYZ for now and let 
                        SUMA_XYZmap_XYZ set the node index*/
                     I_C = -1;
                     XYZ = SUMA_XYZmap_XYZ ( nel->vec[0], SO, 
                                             SUMAg_DOv, SUMAg_N_DOv, &I_C);

                     if (XYZ == NULL || I_C < 0) {
                        SUMA_SL_Warn("AFNI cross hair too\n"
                                    "far from surface.\n"
                                    "No node id from AFNI.\n"
                                    "No action taken.");
                        XBell (svi->X->DPY, 50);             
                        if (XYZ) SUMA_free(XYZ); XYZ = NULL;
                        break;
                     }
                  }

                  /* SUMA_nel_stdout (nel); */
                  if (iview == 0) {
                     fprintf(SUMA_STDOUT, "***********************\n"
                                          "AFNI cross hair notice:\n"
                                          "From Afni: \n"
                                          "  Surface: %s\n"
                                          "  Node: %s, XYZ: %3.2f %3.2f %3.2f\n",
                         SUMA_find_SOLabel_from_idcode(nel_surfidcode, 
                                                       SUMAg_DOv, SUMAg_N_DOv), 
                         SUMA_CHECK_NULL_STR(nel_nodeid), 
                         *((float *)nel->vec[0]), 
                         *((float *)nel->vec[0]+1), 
                         *((float *)nel->vec[0]+2) );
                  }
                  fprintf(SUMA_STDOUT, "In Controller [%c]:\n"
                                       "  Surface: %s, adopted: %s\n"
                                       "  Node: %d, XYZ: %3.2f %3.2f %3.2f\n"
                                       ,
                                       65+iview, 
                                       (found_type == 1 || found_type == 2) ? 
                                             SO->Label:"NULL", 
                                       SO->Label, I_C,
                                       XYZ[0], XYZ[1], XYZ[2]);
                  if (iview == SUMAg_N_SVv-1) {
                     fprintf(SUMA_STDOUT, "\n");
                  }  

                  /* attach the cross hair to the selected surface */
                     iv3[0] = dest_SO_ID; /* nel_surfidcode == NULL is 
                                             handled above, May 15 03*/

                  iv3[1] = I_C; /* use the closest node for a link 
                                   otherwise when you switch states, 
                                   you'll get a wandering cross hair */
                  if (!list) list = SUMA_CreateList();


                  /* set the SO in Focus, if surface was visible */                                                                    /*ZSS Added this Aug. 06 */
                  if (found_type == 1 || found_type == 2) { 
                     /* To set a surface in focus, it must be in the viewer.
                        If not, then SO in focus would be set to a surface that 
                        is not in view, and that can lead to severe crashes. 
                        One way to deal with that situation would be to make SUMA 
                        switch state to that visible surface
                        but that's too visually complicated and jerky looking, 
                        I would imagine. */
                     ED = SUMA_InitializeEngineListData (SE_SetSOinFocus);
                     if (!SUMA_RegisterEngineListCommand (  
                                          list, ED, 
                                          SEF_i, (void*)&dest_SO_ID,
                                          SES_SumaFromAfni, (void *)svi, NOPE,
                                          SEI_Head, NULL)) {
                        fprintf( SUMA_STDERR,
                                 "Error %s: Failed to register element\n", 
                                 FuncName);
                        SUMA_RETURN (NOPE);
                     }
                  }
                  /* set selected node */          /*ZSS Added this Aug. 06 */
                  ED = SUMA_InitializeEngineListData (SE_SetSelectedNode); 
                  if (!(el=SUMA_RegisterEngineListCommand (  
                                       list, ED, 
                                       SEF_i, (void*)&I_C, 
                                       SES_SumaFromAfni, (void *)svi, NOPE,
                                       SEI_Tail, NULL))) {
                     fprintf( SUMA_STDERR,
                              "Error %s: Failed to register element\n", 
                              FuncName);
                     SUMA_RETURN (NOPE);
                  } else {
                     /* add the whole damned group, EngineData would want 
                        to work with it various components */
                     SUMA_RegisterEngineListCommand (  
                                       list, ED, 
                                       SEF_ngr, (void*)ngr, 
                                       SES_SumaFromAfni, (void *)svi, NOPE,
                                       SEI_In, el);
                  }

                  ED = SUMA_InitializeEngineListData (SE_BindCrossHair);
                  if (!SUMA_RegisterEngineListCommand (  
                                       list, ED, 
                                       SEF_iv3, (void*)iv3,
                                       SES_SumaFromAfni, (void *)svi, NOPE,
                                       SEI_Tail, NULL)) {
                     fprintf( SUMA_STDERR,
                              "Error %s: Failed to register element\n",
                              FuncName);
                     SUMA_RETURN (NOPE);
                  }

                  /* send cross hair coordinates */
                  ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
                  if (!SUMA_RegisterEngineListCommand (  
                                       list, ED, 
                                       SEF_fv3, (void*)XYZ,
                                       SES_SumaFromAfni, svi, NOPE,
                                       SEI_Tail, NULL)) {
                     fprintf(SUMA_STDERR,
                             "Error %s: Failed to register element\n", FuncName);
                     SUMA_RETURN (NOPE);
                  }

                  svi->ResetGLStateVariables = YUP; 


                  SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay, 
                                                     SES_SumaFromAfni, svi);
                  if (!SUMA_Engine (&list)) {
                     fprintf( SUMA_STDERR, 
                              "Error %s: SUMA_Engine call failed.\n", FuncName);
                  }


                  if (XYZ) SUMA_free(XYZ); XYZ = NULL;
               } /* link cross hair */    
            } /* iview ... for all viewers */
            /* don't free nel, it's freed later on
               dont't free attributes obtained in NI_get_attribute, 
               they are copies of pointers in nel  */
         }/* SUMA_crosshair_xyz */
         SUMA_RETURN(YUP) ;
      }/* SUMA_crosshair */

      if (strcmp(ngr->name,"SurfaceObject") == 0) { /* New Surface Object */
         SUMA_SurfaceObject *SOn=NULL;   

         SOn = SUMA_nimlSO2SO(ngr); 
         if (!SOn) {
            SUMA_SL_Err("Failed to interpret SO");
            SUMA_RETURN(NOPE) ;
         }

         SUMA_LH("Checking for new surface...");
         SO = SUMA_findSOp_inDOv (SOn->idcode_str, SUMAg_DOv, SUMAg_N_DOv);
         if (SO) {
            fprintf(SUMA_STDERR,"Warning %s: nel idcode was found in DOv.\n"
                                "Checking for mesh compatibility\n", FuncName);
            if (  SO->N_FaceSet * SO->FaceSetDim == 
                  SOn->N_FaceSet * SOn->FaceSetDim) {
               fprintf(SUMA_STDERR,"Note %s: Mesh dimensions match. \n"
                                   "New mesh will be adopted.\n", FuncName);
            } else {
               fprintf( SUMA_STDERR,"Error %s: Mesh dimensions mismatch.\n", 
                        FuncName);
               SUMA_RETURN(NOPE);
            }
         }

         if (!SO) { 
            BrandNew = YUP;
         } else {
            SUMA_LHv("A refit of an existing surface. SO->SurfCont = %p\n", 
                     SO->SurfCont);
            BrandNew = NOPE;
            if (SOn->N_Node != SO->N_Node) {
               fprintf(SUMA_STDERR,"Error %s: Mismatch in number of nodes\n"
                                   "between new mesh and pre-existing one\n"
                                   "(%d vs %d)\n", 
                                   FuncName, SO->N_Node, SO->N_Node);
               SUMA_RETURN(NOPE);
            }
            memcpy((void*)SO->FaceSetList, (void *)SOn->FaceSetList, 
                   SOn->N_FaceSet * SOn->FaceSetDim * sizeof(int));  
                           /* this one's likely to be completely useless! */  
            memcpy((void*)SO->NodeList, (void *)SOn->NodeList, 
                   SOn->N_Node * SOn->NodeDim * sizeof(float));
            /* swap VolPar */
            if (SOn->VolPar) {
               if (SO->VolPar) SUMA_Free_VolPar(SO->VolPar); 
               SO->VolPar = SOn->VolPar;
               SOn->VolPar = NULL;
            }
            SUMA_Free_Surface_Object(SOn); SOn = NULL; 
                  /* alas, not needed no more. 
                     Perhaps you should consider eliminating SO's EdgeLists, 
                     area vectors and the like,
                     You should also perhaps update VolPar with SOn's... */
            SUMA_LHv("Refit done, SO->SurfCont = %p\n", SO->SurfCont);
         }

         /* add this surface to DOv */
         if (BrandNew) {
            if (!SUMA_AddDO(SUMAg_DOv, &(SUMAg_N_DOv), 
                            (void *)SOn,  SO_type, SUMA_WORLD)) {
               fprintf(SUMA_STDERR,"Error %s: Error Adding DO\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            SUMA_LHv("A brand new surface. SO->SurfCont = %p\n", SOn->SurfCont);
         }

         /* don't free nel, it's freed later on */
         SUMA_RETURN(YUP) ;
      } else if (strcmp(ngr->name,"EngineCommand") == 0) {
         SUMA_nimlEngine2Engine(ngr);
         /* don't free nel, it's freed later on */
         SUMA_RETURN(YUP) ;
      } else if (strcmp(ngr->name,"Segment_DO") == 0) {
         SUMA_SegmentDO *SDO = SUMA_niSDO2SDO(ngr);
         /* addDO (mixing is taken care of internally)*/
         if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, (void *)SDO, 
                         SDO->do_type, SUMA_WORLD)) {
            fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
            SUMA_RETURN(NOPE);
         }
         
         if (!sv) sv = &(SUMAg_SVv[0]);

         /* register DO with viewer */
         if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
            fprintf( SUMA_STDERR,
                     "Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
            SUMA_RETURN(NOPE);
         }

         /* redisplay curent only*/
         sv->ResetGLStateVariables = YUP;
         SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);

         /* don't free nel, it's freed later on */
         SUMA_RETURN(YUP);
      }
   
      /*** If here, then name of group didn't match anything 
           Try processing its parts ***/
      if (LocalHead)  {
               fprintf( SUMA_STDERR,
                        "%s:  Working group %s \n",
                        FuncName, ngr->name);
      }
      for( ip=0 ; ip < ngr->part_num ; ip++ ){ 
         switch( ngr->part_typ[ip] ){
            /*-- a sub-group ==> recursion! --*/
            case NI_GROUP_TYPE:
               if (!SUMA_process_NIML_data( (VOID_CAST)ngr->part_typ[ip] , sv)) {
                  NI_group *ngr2=(NIGRP_CAST)ngr->part_typ[ip];
                  SUMA_S_Errv("Failed in SUMA_process_NIML_data for\n"
                              " group %s's subgroup %s\n", 
                              ngr->name, ngr2->name);
               }
               break ;
            case NI_ELEMENT_TYPE:
               nel = (NI_element *)ngr->part[ip] ;
               if (!SUMA_process_NIML_data( (void *)nel , sv)) { 
                     SUMA_S_Errv("Failed in SUMA_process_NIML_data for \n"
                                 " group %s's element %s\n", 
                                 ngr->name, nel->name);
               }   
               break;
            default:
               SUMA_SL_Err("Don't know what to make of this group element\n"
                           "ignoring.");
               break;
         }
      }
      
      SUMA_RETURN(YUP) ;
   }
}

/*------------------------------------------------------------------*/
/*! Make a NIML data element for a NI surface element IXYZ
   \param SO (SUMA_SurfaceObject *) surface object to turn to NI
   \ret  NULL if you input stupid values, NI if you input smart values
--------------------------------------------------------------------*/

NI_element * SUMA_makeNI_SurfIXYZ (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_makeNI_SurfIXYZ"};
   NI_element *nel;
   int *ic, ii, ND, id;
   float *xc, *yc, *zc;
    
   SUMA_ENTRY;

   
   if (SO == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Null SO.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   if (SO->N_Node <= 0) {
      fprintf(SUMA_STDERR,"Error %s: No nodes in SO.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   /* make a new data element, to be filled by columns */
   nel = NI_new_data_element( "SUMA_ixyz" , SO->N_Node) ;
   
   /* make the columns to be put in the element */
   ic = (int *)   SUMA_malloc( sizeof(int)   * SO->N_Node ) ;
   xc = (float *) SUMA_malloc( sizeof(float) * SO->N_Node ) ;
   yc = (float *) SUMA_malloc( sizeof(float) * SO->N_Node ) ;
   zc = (float *) SUMA_malloc( sizeof(float) * SO->N_Node ) ;

   if (!nel || !ic || !xc || !yc || !zc) {
      fprintf(SUMA_STDERR,"Error %s: Failed to allocate for nel, ic, xc, yc or zc.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   

   /* load the columns from the struct array */
   ND = SO->NodeDim;
   for( ii=0 ; ii < SO->N_Node ; ii++ ){
      ic[ii] = ii;
      id = ND * ii;
      xc[ii] = SO->NodeList[id];
      yc[ii] = SO->NodeList[id+1];
      zc[ii] = SO->NodeList[id+2];
   }

   /* put columns into element */

   NI_add_column( nel , NI_INT   , ic ) ; SUMA_free(ic) ;
   NI_add_column( nel , NI_FLOAT , xc ) ; SUMA_free(xc) ;
   NI_add_column( nel , NI_FLOAT , yc ) ; SUMA_free(yc) ;
   NI_add_column( nel , NI_FLOAT , zc ) ; SUMA_free(zc) ;

   if (SO->VolPar) NI_set_attribute (nel, "volume_idcode", SO->VolPar->vol_idcode_str);
   NI_set_attribute (nel, "surface_idcode", SO->idcode_str);
   NI_set_attribute (nel, "surface_label", SO->Label);
   NI_set_attribute (nel, "local_domain_parent_ID", SO->LocalDomainParentID);
   NI_set_attribute (nel, "local_domain_parent", SO->LocalDomainParent);
   if (SO->SpecFile.FileName) NI_set_attribute (nel, "surface_specfile_name", SO->SpecFile.FileName);
   else NI_set_attribute (nel, "surface_specfile_name", "Unknown");
   if (SO->SpecFile.Path) NI_set_attribute (nel, "surface_specfile_path", SO->SpecFile.Path);
   else NI_set_attribute (nel, "surface_specfile_path", "Unknown");
   
   SUMA_RETURN (nel);
}

/*------------------------------------------------------------------*/
/*! Make a NIML data element for a NI surface element i nx ny nz 
    onde index followed by node normal
   \param SO (SUMA_SurfaceObject *) surface object to turn to NI
   \ret  NULL if you input stupid values, NI if you input smart values
--------------------------------------------------------------------*/
/* #define DOINDEX */ /* uncomment if you want to pass node index along with normals */
NI_element * SUMA_makeNI_SurfINORM (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_makeNI_SurfINORM"};
   NI_element *nel=NULL;
   int *ic=NULL, ii, ND, id;
   float *xc=NULL, *yc=NULL, *zc=NULL;
   
   SUMA_ENTRY;

   
   if (SO == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Null SO.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   if (SO->N_Node <= 0) {
      fprintf(SUMA_STDERR,"Error %s: No nodes in SO.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   if (!SO->NodeNormList) {
      fprintf(SUMA_STDERR,"Error %s: No normals in SO.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   /* make a new data element, to be filled by columns */
   nel = NI_new_data_element( "SUMA_node_normals" , SO->N_Node) ;
   
   /* make the columns to be put in the element */
   #ifdef DOINDEX   
   ic = (int *)   SUMA_malloc( sizeof(int)   * SO->N_Node ) ;
   #endif
   xc = (float *) SUMA_malloc( sizeof(float) * SO->N_Node ) ;
   yc = (float *) SUMA_malloc( sizeof(float) * SO->N_Node ) ;
   zc = (float *) SUMA_malloc( sizeof(float) * SO->N_Node ) ;

   if (!nel || !xc || !yc || !zc) {
      fprintf(SUMA_STDERR,"Error %s: Failed to allocate for nel, ic, xc, yc or zc.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   

   /* load the columns from the struct array */
   ND = SO->NodeDim;
   for( ii=0 ; ii < SO->N_Node ; ii++ ){
   #ifdef DOINDEX
      ic[ii] = ii;
   #endif
      id = ND * ii;
      xc[ii] = SO->NodeNormList[id];
      yc[ii] = SO->NodeNormList[id+1];
      zc[ii] = SO->NodeNormList[id+2];
   }

   /* put columns into element */

   #ifdef DOINDEX
      NI_add_column( nel , NI_INT   , ic ) ; SUMA_free(ic) ; 
   #endif
   NI_add_column( nel , NI_FLOAT , xc ) ; SUMA_free(xc) ;
   NI_add_column( nel , NI_FLOAT , yc ) ; SUMA_free(yc) ;
   NI_add_column( nel , NI_FLOAT , zc ) ; SUMA_free(zc) ;

   NI_set_attribute (nel, "volume_idcode", SO->VolPar->vol_idcode_str);
   NI_set_attribute (nel, "surface_idcode", SO->idcode_str);
   NI_set_attribute (nel, "surface_label", SO->Label);
   NI_set_attribute (nel, "local_domain_parent_ID", SO->LocalDomainParentID);
   NI_set_attribute (nel, "local_domain_parent", SO->LocalDomainParent);
   SUMA_RETURN (nel);
}

/*------------------------------------------------------------------*/
/*! Make a NIML data element for a NI surface element IJK
   \param SO (SUMA_SurfaceObject *) surface object to turn to NI
   \ret  NULL if you input stupid values, NI if you input smart values
--------------------------------------------------------------------*/

NI_element * SUMA_makeNI_SurfIJK (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_makeNI_SurfIJK"};
   NI_element *nel;
   int  ii,  ip, NP;
   int *I, *J, *K;
   
   SUMA_ENTRY;

   
   if (SO == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Null SO.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   if (SO->N_FaceSet <= 0) {
      fprintf(SUMA_STDERR,"Error %s: No FaceSets in SO.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   NP = SO->FaceSetDim;
   /* make a new data element, to be filled by columns */
   nel = NI_new_data_element( "SUMA_ijk" , SO->N_FaceSet) ;
   
   /* make the columns to be put in the element */
   I = (int *) SUMA_malloc( sizeof(int) * SO->N_FaceSet ) ;
   J = (int *) SUMA_malloc( sizeof(int) * SO->N_FaceSet ) ;
   K = (int *) SUMA_malloc( sizeof(int) * SO->N_FaceSet ) ;

   if (!nel || !I || !J || !K ) {
      fprintf(SUMA_STDERR,"Error %s: Failed to allocate for nel, I, J or K.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   

   /* load the columns from the struct array */

   for( ii=0 ; ii < SO->N_FaceSet ; ii++ ){
      ip = NP * ii;
      I[ii] = SO->FaceSetList[ip];
      J[ii] = SO->FaceSetList[ip+1];
      K[ii] = SO->FaceSetList[ip+2];
   }

   /* put columns into element */

   NI_add_column( nel , NI_INT   , I ) ; SUMA_free(I) ;
   NI_add_column( nel , NI_INT   , J ) ; SUMA_free(J) ;
   NI_add_column( nel , NI_INT   , K ) ; SUMA_free(K) ;

   NI_set_attribute (nel, "volume_idcode", SO->VolPar->vol_idcode_str);
   NI_set_attribute (nel, "surface_idcode", SO->idcode_str);
   NI_set_attribute (nel, "surface_label", SO->Label);
   NI_set_attribute (nel, "local_domain_parent_ID", SO->LocalDomainParentID);
   NI_set_attribute (nel, "local_domain_parent", SO->LocalDomainParent);
   if (SO->SpecFile.FileName) NI_set_attribute (nel, "surface_specfile_name", SO->SpecFile.FileName);
   else NI_set_attribute (nel, "surface_specfile_name", "Unknown");
   if (SO->SpecFile.Path) NI_set_attribute (nel, "surface_specfile_path", SO->SpecFile.Path);
   else NI_set_attribute (nel, "surface_specfile_path", "Unknown");

   SUMA_RETURN (nel);
}

SUMA_Boolean SUMA_nel_stdout (NI_element *nel) 
{
   static char FuncName[]={"SUMA_nel_stdout"};
   NI_stream nstdout;

   SUMA_ENTRY;

   nstdout = NI_stream_open( "fd:1","w");
   if( nstdout == NULL ){ 
      fprintf(SUMA_STDERR,"%s: Can't open fd:1\n", FuncName); 
      SUMA_RETURN(NOPE); 
   }
   fprintf (stdout, "\n----------------------------nel stdout begin-------------------\n");
   NI_write_element( nstdout , nel , NI_TEXT_MODE ) ;
   fprintf (stdout, "----------------------------nel stdout end  -------------------\n");
   NI_stream_close(nstdout);

   SUMA_RETURN(YUP);
}

NI_element * SUMA_makeNI_CrossHair (SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_makeNI_CrossHair"};
   NI_element *nel;
   float *XYZmap;
   int I_C = -1;
   SUMA_SurfaceObject *SO;
   
   SUMA_ENTRY;

   if (sv == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Null sv.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   if (sv->Ch == NULL) {
      fprintf(SUMA_STDERR,"Error %s: NULL Ch.\n", FuncName);
      SUMA_RETURN (NULL);
   }

   SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Focus_SO_ID].OP);
   I_C = SO->SelectedNode;
   XYZmap = SUMA_XYZ_XYZmap (sv->Ch->c, SO, SUMAg_DOv, SUMAg_N_DOv, &I_C);
   
   if (XYZmap == NULL){
      fprintf( SUMA_STDERR,
               "%s: Linkage is not posible, using current XYZ\n", FuncName);
      XYZmap = (float *)SUMA_calloc (3, sizeof(float));
      if (XYZmap == NULL) {
         fprintf (SUMA_STDERR, "Error %s: Give me a break !\n", FuncName);
         SUMA_RETURN (NULL); 
      }
      XYZmap[0] = sv->Ch->c[0];
      XYZmap[1] = sv->Ch->c[1];
      XYZmap[2] = sv->Ch->c[2];
   }
   
   /* make a new data element */
   nel = NI_new_data_element( "SUMA_crosshair_xyz" , 3) ;
   
   if (!nel) {
      fprintf(SUMA_STDERR,"Error %s: Failed to allocate for nel\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   /* add some info about surface in question */
   NI_SETA_INT(nel, "surface_nodeid", SO->SelectedNode);
   NI_set_attribute( nel, "surface_idcode", SO->idcode_str);
   NI_set_attribute( nel, "surface_label", SO->Label);
   
   NI_add_column( nel , NI_FLOAT , XYZmap );
   
   if (XYZmap) SUMA_free(XYZmap);

   SUMA_RETURN (nel);
}

/*!
   ans = SUMA_CanTalkToAfni (dov, N_dov);
   determines if any of the Surface Viewers is allowed to talk to afni
   \param dov (SUMA_DO *) the Displayable Objects vector (ususally SUMAg_DOv)
   \param N_dov (int) the number of elements in dov (usually SUMAg_N_DOv)
   \ret ans (SUMA_Boolean) NOPE if none of the SOs shown in the viewer has both 
      LocalDomainParentID != NULL && VolPar != NULL
      
   This function is much different from the one prior to Tue Nov 19 11:44:24 EST 2002
*/

SUMA_Boolean SUMA_CanTalkToAfni (SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_CanTalkToAfni"};
   int i;
   SUMA_SurfaceObject *SO;
   
   SUMA_ENTRY;
   
   for (i=0; i< N_dov; ++i) {
      if (SUMA_isSO(dov[i])) {
         SO = (SUMA_SurfaceObject *)(dov[i].OP);
         if (SO->LocalDomainParentID != NULL && SO->VolPar != NULL) {
            SUMA_RETURN (YUP);
         }
      } 
   }
   
   SUMA_RETURN (NOPE);
}


/*------------------------------------------------------------------------*/
static int num_workp      = 0 ;
static XtWorkProc * workp = NULL ;
static XtPointer *  datap = NULL ;
static XtWorkProcId wpid ;

/*#define WPDEBUG*/

void SUMA_register_workproc( XtWorkProc func , XtPointer data )
{
   static char FuncName[]={"SUMA_register_workproc"};
   
   SUMA_ENTRY;

   if( func == NULL ){
      fprintf(SUMA_STDERR,"Error %s: func=NULL on entry!\n", FuncName) ;
      SUMA_RETURNe;
   }

   if( num_workp == 0 ){
      workp = (XtWorkProc *) SUMA_malloc( sizeof(XtWorkProc) ) ;
      datap = (XtPointer *)  SUMA_malloc( sizeof(XtPointer) ) ;
      wpid  = XtAppAddWorkProc(SUMAg_CF->X->App, SUMA_workprocess, NULL ) ;
#ifdef WPDEBUG
      fprintf(stderr,"SUMA_register_workproc: wpid = %x\n",(int)wpid) ;
#endif
   } else {
      workp = (XtWorkProc *) SUMA_realloc( workp, sizeof(XtWorkProc)*(num_workp+1) ) ;
      datap = (XtPointer*)   SUMA_realloc( datap, sizeof(XtPointer) *(num_workp+1) ) ;
   }

   workp[num_workp] = func ;
   datap[num_workp] = data ;
   num_workp++ ;

#ifdef WPDEBUG
fprintf(stderr,"SUMA_register_workproc: have %d workprocs\n",num_workp) ;
#endif

   SUMA_RETURNe ;
}

/*! 
The difference between SUMA_remove_workproc2 and SUMA_remove_workproc is that 
the workprocess removed is identified not just by the function name but also the data pointer 
*/
void SUMA_remove_workproc2( XtWorkProc func , XtPointer data )
{
   int ii , ngood ;
   static char FuncName[]={"SUMA_remove_workproc2"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (LocalHead)   fprintf (SUMA_STDERR, "%s: func = %p, num_workp = %d\n", FuncName, func, num_workp);
   
   if( func == NULL) {
      fprintf(SUMA_STDERR,"%s: *** illegal parameters!\n", FuncName) ;
      SUMA_RETURNe ;
   }
   if (num_workp == 0) {
      if (LocalHead)   fprintf(SUMA_STDERR,"%s: Nothing to do.\n", FuncName) ;
      SUMA_RETURNe ;
   }
   
   if( num_workp < 1 ){
      #ifdef WPDEBUG
            fprintf(SUMA_STDERR,"%s: No workprocs left\n", FuncName) ;
      #endif
      XtRemoveWorkProc( wpid ) ;
      SUMA_free(workp) ; workp = NULL ; SUMA_free(datap) ; datap = NULL ;
      num_workp = 0 ;
   } else {
      for( ii=0 ; ii < num_workp ; ii++ ){
         if( func == workp[ii] && data == datap[ii]) {   /* move last Workprocess to location of workprocess to be deleted */
            workp[ii] = workp[num_workp-1] ;
            datap[ii] = datap[num_workp-1] ;
            workp[num_workp-1] = NULL;
            num_workp--;
         }

         #ifdef WPDEBUG
            fprintf(SUMA_STDERR,"%s: %d workprocs left\n", FuncName, ngood) ;
         #endif
      }
   }

   SUMA_RETURNe ;

}

void SUMA_remove_workproc( XtWorkProc func )
{
   int ii , ngood ;
   static char FuncName[]={"SUMA_remove_workproc"};
   
   SUMA_ENTRY;

   if( func == NULL || num_workp == 0 ){
      fprintf(SUMA_STDERR,"Error %s: *** illegal parameters!\n", FuncName) ;
      SUMA_RETURNe ;
   }

   if( num_workp < 1 ){
      #ifdef WPDEBUG
            fprintf(stderr,"SUMA_remove_workproc: No workprocs left\n") ;
      #endif
      XtRemoveWorkProc( wpid ) ;
      SUMA_free(workp) ; workp = NULL ; SUMA_free(datap) ; datap = NULL ;
      num_workp = 0 ;
   } else {
      for( ii=0 ; ii < num_workp ; ii++ ){
         if( func == workp[ii] ) {   /* move last Workprocess to location of workprocess to be deleted */
            workp[ii] = workp[num_workp-1] ;
            datap[ii] = datap[num_workp-1] ;
            workp[num_workp-1] = NULL;
            num_workp--;
         }

         #ifdef WPDEBUG
            fprintf(stderr,"SUMA_remove_workproc: %d workprocs left\n",ngood) ;
         #endif
      }
   }

   SUMA_RETURNe ;
}

Boolean SUMA_workprocess( XtPointer fred )
{
   static char FuncName[]={"SUMA_workprocess"};
   int ii , ngood ;
   Boolean done ;

   if (SUMA_WORKPROC_IO_NOTIFY) {SUMA_ENTRY;}
   
#ifdef WPDEBUG
   { static int ncall=0 ;
     if( (ncall++) % 1000 == 0 )
       fprintf(stderr,"SUMA_workprocess: entry %d\n",ncall) ; }
#endif

   if( num_workp == 0 ) {
      if (SUMA_WORKPROC_IO_NOTIFY) {
         SUMA_RETURN(True) ;
      }
         else return(True);
   }

   for( ii=0,ngood=0 ; ii < num_workp ; ii++ ){
      if( workp[ii] != NULL ){
         done = workp[ii]( datap[ii] ) ;
         if( done == True ) workp[ii] = NULL ;
         else               ngood++ ;
      }
   }

   if( ngood == 0 ){
#ifdef WPDEBUG
      fprintf(stderr,"Found no workprocs left\n") ;
#endif
      SUMA_free(workp) ; workp = NULL ; SUMA_free(datap) ; datap = NULL ;
      num_workp = 0 ;
      if (SUMA_WORKPROC_IO_NOTIFY) {
         SUMA_RETURN(True) ;
      }
         else return (True);
   }
   
   if (SUMA_WORKPROC_IO_NOTIFY) {
      SUMA_RETURN(False) ;
   }
      else return(False);
}

/*---------------------------------------------------------------*/

/*!
   \brief A function to take a SUMA_DRAWN_ROI struct and return an equivalent
   SUMA_NIML_DRAWN_ROI struct. 
   
   - Do not free SUMA_NIML_DRAWN_ROI manually, many of its fields are 
   pointer copies of values in SUMA_DRAWN_ROI.
   
   \sa SUMA_Free_NIMLDrawROI
*/
SUMA_NIML_DRAWN_ROI * SUMA_DrawnROI_to_NIMLDrawnROI (SUMA_DRAWN_ROI *ROI)
{
   static char FuncName[]={"SUMA_DrawnROI_to_NIMLDrawnROI"};
   SUMA_NIML_DRAWN_ROI *nimlROI=NULL;
   SUMA_ROI_DATUM *ROI_Datum=NULL;
   DListElmt *Elm = NULL;
   int i = -1;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!ROI) {
      SUMA_SL_Err("Null ROI");
      SUMA_RETURN(NULL);
   }
   
   /* allocate for nimlROI */
   nimlROI = (SUMA_NIML_DRAWN_ROI *)SUMA_malloc(sizeof(SUMA_NIML_DRAWN_ROI));
   
   nimlROI->Type = (int)ROI->Type;
   nimlROI->idcode_str = ROI->idcode_str;
   nimlROI->Parent_idcode_str = ROI->Parent_idcode_str;
   nimlROI->Label = ROI->Label;
   nimlROI->iLabel = ROI->iLabel;
   nimlROI->N_ROI_datum = dlist_size(ROI->ROIstrokelist);
   nimlROI->ColPlaneName = ROI->ColPlaneName;
   nimlROI->FillColor[0] = ROI->FillColor[0];
   nimlROI->FillColor[1] = ROI->FillColor[1];
   nimlROI->FillColor[2] = ROI->FillColor[2];
   nimlROI->FillColor[3] = ROI->FillColor[3];
   nimlROI->EdgeColor[0] = ROI->EdgeColor[0];
   nimlROI->EdgeColor[1] = ROI->EdgeColor[1];
   nimlROI->EdgeColor[2] = ROI->EdgeColor[2];
   nimlROI->EdgeColor[3] = ROI->EdgeColor[3];
   nimlROI->EdgeThickness = ROI->EdgeThickness;
   if (!nimlROI->N_ROI_datum) {
      nimlROI->ROI_datum = NULL;
      SUMA_RETURN(nimlROI);
   }
   nimlROI->ROI_datum = 
      (SUMA_NIML_ROI_DATUM *)SUMA_malloc( nimlROI->N_ROI_datum * 
                                          sizeof(SUMA_NIML_ROI_DATUM));

   /* now fill the ROI_datum structures */
   Elm = NULL;
   i = 0;
   do {
      if (!Elm) Elm = dlist_head(ROI->ROIstrokelist);
      else Elm = Elm->next;
      ROI_Datum = (SUMA_ROI_DATUM *)Elm->data;
      nimlROI->ROI_datum[i].action = ROI_Datum->action;
      nimlROI->ROI_datum[i].Type = ROI_Datum->Type;
      nimlROI->ROI_datum[i].N_n = ROI_Datum->N_n;
      nimlROI->ROI_datum[i].nPath = ROI_Datum->nPath;
      
      /*    
      nimlROI->ROI_datum[i].N_t = ROI_Datum->N_t;
      nimlROI->ROI_datum[i].tPath = ROI_Datum->tPath; 
      */
      ++i;
   } while (Elm != dlist_tail(ROI->ROIstrokelist));
   
   SUMA_RETURN(nimlROI);
}


/*!
   \brief transfroms a SUMA_NIML_DRAWN_ROI * to a SUMA_DRAWN_ROI *
   
   \param nimlROI (SUMA_NIML_DRAWN_ROI *) the niml ROI structure
   \param ForDisplay (SUMA_Boolean) YUP: Action stack is created
                                          (use when ROIs will be displayed)
                                    NOPE: Action stack is not created
   \return ROI (SUMA_DRAWN_ROI *) the equivalent of niml ROI structure
                                          
   - Do not free SUMA_NIML_DRAWN_ROI manually, many of its fields are 
   pointer copies of values in SUMA_DRAWN_ROI.
   
   \sa SUMA_Free_NIMLDrawROI
*/
SUMA_DRAWN_ROI *SUMA_NIMLDrawnROI_to_DrawnROI (SUMA_NIML_DRAWN_ROI * nimlROI, SUMA_Boolean ForDisplay)
{
   static char FuncName[]={"SUMA_NIMLDrawnROI_to_DrawnROI"};
   SUMA_ROI_ACTION_STRUCT *ROIA=NULL;
   SUMA_DRAWN_ROI *ROI = NULL;
   SUMA_ROI_DATUM *ROI_Datum = NULL;
   DListElmt *tmpStackPos=NULL;
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!nimlROI) SUMA_RETURN(NULL);
   
   /* allocate and initialize the whimpy fields 
      Based on SUMA_AllocateDrawnROI*/
   ROI = (SUMA_DRAWN_ROI *) SUMA_malloc(sizeof(SUMA_DRAWN_ROI));
   if (  nimlROI->Type == SUMA_ROI_OpenPath || 
         nimlROI->Type == SUMA_ROI_ClosedPath ||
         nimlROI->Type == SUMA_ROI_FilledArea ) { 
            /* this ROI will gradually be reconstructed,
               start with the basics */
         ROI->Type = SUMA_ROI_OpenPath; 
            /* at the end of the construction you should reach nimlROI->Type */
   }else {
      /* nothing to reconstruct */
      ROI->Type = nimlROI->Type;
   }
   
   ROI->idcode_str = SUMA_copy_string(nimlROI->idcode_str);
   ROI->Parent_idcode_str = SUMA_copy_string(nimlROI->Parent_idcode_str);
   ROI->Label = SUMA_copy_string(nimlROI->Label);
   ROI->iLabel = nimlROI->iLabel;
   if (LocalHead) 
      fprintf (SUMA_STDERR, 
               "%s: ROI->Parent_idcode_str %s\n", 
               FuncName, ROI->Parent_idcode_str);
   
   ROI->ROIstrokelist = (DList *)SUMA_malloc (sizeof(DList));
   dlist_init(ROI->ROIstrokelist, SUMA_FreeROIDatum);
   
   ROI->DrawStatus = SUMA_ROI_Finished;
   ROI->StackPos = NULL;
   ROI->ActionStack = SUMA_CreateActionStack ();
   ROI->ColPlaneName = SUMA_copy_string(nimlROI->ColPlaneName);
   ROI->FillColor[0] = nimlROI->FillColor[0];
   ROI->FillColor[1] = nimlROI->FillColor[1];
   ROI->FillColor[2] = nimlROI->FillColor[2];
   ROI->FillColor[3] = nimlROI->FillColor[3];
   ROI->EdgeColor[0] = nimlROI->EdgeColor[0];
   ROI->EdgeColor[1] = nimlROI->EdgeColor[1];
   ROI->EdgeColor[2] = nimlROI->EdgeColor[2];
   ROI->EdgeColor[3] = nimlROI->EdgeColor[3];
   ROI->EdgeThickness = nimlROI->EdgeThickness;
   ROI->CE = NULL;
   ROI->N_CE = -1;
   /* fill in the ROI datum stuff */
   for (i=0; i<nimlROI->N_ROI_datum; ++i) {
         ROI_Datum = SUMA_AllocROIDatum ();
         ROI_Datum->action = nimlROI->ROI_datum[i].action;
         ROI_Datum->nPath = nimlROI->ROI_datum[i].nPath;
         ROI_Datum->Type = nimlROI->ROI_datum[i].Type;
         ROI_Datum->N_n = nimlROI->ROI_datum[i].N_n;
      if (ForDisplay) { /* create DO/UNDO stack */
         ROIA = (SUMA_ROI_ACTION_STRUCT *) SUMA_malloc (sizeof(SUMA_ROI_ACTION_STRUCT));
         ROIA->DrawnROI = ROI;
         ROIA->ROId = ROI_Datum;
         switch (ROI_Datum->action) {
            case SUMA_BSA_AppendStroke:
               SUMA_LH("Appending Stroke Action");
               tmpStackPos = SUMA_PushActionStack (ROI->ActionStack, ROI->StackPos, 
                  SUMA_AddToTailROIDatum, (void *)ROIA, SUMA_DestroyROIActionData);
               break;
            case SUMA_BSA_JoinEnds:
               SUMA_LH("Join Ends Action");
               tmpStackPos = SUMA_PushActionStack (ROI->ActionStack, ROI->StackPos, 
                  SUMA_AddToTailJunctionROIDatum, (void *)ROIA, SUMA_DestroyROIActionData);
               break;
            case SUMA_BSA_FillArea:
               SUMA_LH("Fill Area Action");
               tmpStackPos = SUMA_PushActionStack (ROI->ActionStack, ROI->StackPos, 
                  SUMA_AddFillROIDatum, (void *)ROIA, SUMA_DestroyROIActionData);
               break;
            default:
               fprintf (SUMA_STDERR, "Error %s: Not ready to deal with this action (%d).\n", 
                  FuncName, ROI_Datum->action);
               break; 

         }

         if (tmpStackPos) ROI->StackPos = tmpStackPos;
         else {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_PushActionStack.\n", FuncName);
         }
      } else {
         /* ROI will not be used for display purposes, just add datum */
         dlist_ins_next(ROI->ROIstrokelist, dlist_tail(ROI->ROIstrokelist), (void *)ROI_Datum);
      }
   }
   
   if (ForDisplay) {
      /* Saved ROIs are considered finished, put a finish action on the top of the action stack */
      ROIA = (SUMA_ROI_ACTION_STRUCT *) SUMA_malloc (sizeof(SUMA_ROI_ACTION_STRUCT)); 
      ROIA->DrawnROI = ROI;
      ROIA->ROId = NULL;
      tmpStackPos = SUMA_PushActionStack (ROI->ActionStack, ROI->StackPos, 
                  SUMA_FinishedROI, (void *)ROIA, SUMA_DestroyROIActionData);
      if (tmpStackPos) ROI->StackPos = tmpStackPos;
      else {
         SUMA_SL_Err("Failed in SUMA_PushActionStack.\n");
         SUMA_RETURN(NULL);
      } 
   }   
   SUMA_RETURN(ROI);
}

/*!
   \brief frees a nimlROI structure. These structures are created by
    the likes of SUMA_DrawnROI_to_NIMLDrawnROI
    
    \sa SUMA_DrawnROI_to_NIMLDrawnROI
*/
SUMA_NIML_DRAWN_ROI * SUMA_Free_NIMLDrawROI (SUMA_NIML_DRAWN_ROI *nimlROI)
{
   static char FuncName[]={"SUMA_Free_NIMLDrawROI"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!nimlROI) SUMA_RETURN(NULL);
   
   if (nimlROI->ROI_datum) SUMA_free(nimlROI->ROI_datum); /* DO NOT FREE MEMORY POINTED to by fields inside nimlROI->ROI_datum */
   SUMA_free(nimlROI);
   
   SUMA_RETURN(NULL);
}



/*!  
A temporary function to play with ni elements
Solo does writing only 
*/

typedef struct {
  int num_nod ;
  int *nod ;
} ROI_seg ;

typedef struct {
  int num_seg ;
  float val ;
  char  name[128] ;
  ROI_seg *seg ;
} ROI ;

void SUMA_FakeIt (int Solo)
{
      if (!Solo) {
      ROI *myroi ;
      ROI_seg *myseg , *inseg ;
      int roi_type ;
      NI_element *nel ;
      NI_stream ns ;
      char *atr ;
      int nseg,ii , nnod,jj ;

      /* define struct to read from element */

      roi_type = NI_rowtype_define( "ROI_seg" , "int,int[#1]" ) ;
      printf("roi_type code = %d\n",roi_type) ;

      /* open file and read 1 data element */

      ns = NI_stream_open( "file:qroi.dat" , "r" ) ;
      if( ns == NULL ){
        fprintf(stderr,"Can't open qroi.dat!\n"); exit(1);
      }
      nel = NI_read_element(ns,1) ;  NI_stream_close(ns) ;
      if( nel == NULL ){
        fprintf(stderr,"Can't read element from qroi.dat!\n"); exit(1);
      }

      /* check input element name and type */

      printf("element name = %s\n",nel->name) ;
      printf("  nel->vec_num     = %d\n",nel->vec_num) ;         /* # of vectors */
      printf("  nel->vec_type[0] = %d\n",nel->vec_typ[0]) ;    /* type of vec #0 */
      if( strcmp(nel->name,"ROI") != 0 ) exit(1) ;

      myroi = malloc(sizeof(ROI)) ;                  /* create output ROI struct */
      atr = NI_get_attribute( nel , "ROI_val") ;   /* set ROI val from attribute */
      myroi->val = (atr == NULL) ? 0.0 : strtod(atr,NULL) ;
      atr = NI_get_attribute( nel , "ROI_name") ; /* set ROI name from attribute */
      NI_strncpy(myroi->name,atr,128) ;
      myroi->num_seg = nseg = nel->vec_len ;      /* element is array of ROI_seg */
      inseg = nel->vec[0] ;                            /* input array of ROI_seg */
      myroi->seg = malloc(sizeof(ROI_seg)*nseg); /* make output array of ROI_seg */

      for( ii=0 ; ii < nseg ; ii++ ){        /* copy input array to output array */
        myroi->seg[ii].num_nod = nnod = inseg[ii].num_nod ;
        if( nnod > 0 ){
          myroi->seg[ii].nod = malloc(sizeof(int)*nnod) ;
          memcpy( myroi->seg[ii].nod , inseg[ii].nod , sizeof(int)*nnod ) ;
        } else {
          myroi->seg[ii].nod = NULL ;
        }
      }

      printf("  val    = %g\n"
             "  name   = %s\n"
             "  num_seg= %d\n" , myroi->val , myroi->name , myroi->num_seg ) ;
      for( ii=0 ; ii < nseg ; ii++ ){
        printf("  Segment #%d has %d nodes:",ii,myroi->seg[ii].num_nod) ;
        for( jj=0 ; jj < myroi->seg[ii].num_nod ; jj++ )
          printf(" %d",myroi->seg[ii].nod[jj]) ;
        printf("\n") ;
      }

      printf("\nWriting element to stdout\n") ; fflush(stdout) ;
      ns = NI_stream_open( "stdout:" , "w" ) ;
      NI_write_element( ns , nel , NI_TEXT_MODE | NI_HEADERSHARP_FLAG ) ;
      NI_stream_close( ns ) ; NI_free_element(nel) ;
   }
   /*********Me ROI*********/
   {
      char *idcode_str, *Parent_idcode_str, *Label, stmp[200]; 
      int *nPath0, *nPath1, N_n0, N_n1, i, niml_ROI_Datum_type;
      NI_element *nel ;
      NI_stream ns ;
      SUMA_NIML_DRAWN_ROI *niml_ROI = NULL;
      
      idcode_str = (char*) malloc(sizeof(char) * 200); sprintf(idcode_str,"Moma- idcode_str");
      Parent_idcode_str = (char*) malloc(sizeof(char) * 200); sprintf(Parent_idcode_str,"El Parent");
      Label = (char*) malloc(sizeof(char) * 200); sprintf(Label,"Da laba");
      N_n0 = 3;
      N_n1 = 4;
      nPath0 = (int*) calloc(N_n0, sizeof(int));
      nPath1 = (int*) calloc(N_n1, sizeof(int));
      nPath0[0] = 2; nPath0[1] = 1; nPath0[2] = 10;
      nPath1[0] = 9; nPath1[1] = 7; nPath1[2] = 23; nPath1[3] = -3;
       
      fprintf(stderr,"*********** Defining row type\n");
      niml_ROI_Datum_type = 
         NI_rowtype_define("SUMA_NIML_ROI_DATUM", "int,int,int,int[#3]");
      
      niml_ROI = (SUMA_NIML_DRAWN_ROI *)malloc(sizeof(SUMA_NIML_DRAWN_ROI));
      memset(niml_ROI, 0, sizeof(SUMA_NIML_DRAWN_ROI)); /* LPatrol */
      niml_ROI->Type = 4;
      niml_ROI->idcode_str = idcode_str;
      niml_ROI->Parent_idcode_str = Parent_idcode_str;
      niml_ROI->Label = Label;
      niml_ROI->iLabel = 20;
      niml_ROI->N_ROI_datum = 2;
      niml_ROI->ROI_datum = 
         (SUMA_NIML_ROI_DATUM *) /* 13 Feb 2009 [lesstif patrol] */
            calloc(niml_ROI->N_ROI_datum, sizeof(SUMA_NIML_ROI_DATUM));

      /* now fill the ROI_datum structures */
      
      niml_ROI->ROI_datum[0].N_n = N_n0;
      niml_ROI->ROI_datum[1].N_n = N_n1;
      if (1) {
         fprintf(stderr,"*********** Filling ROI_datum structures\n");
         niml_ROI->ROI_datum[0].nPath = nPath0;
         niml_ROI->ROI_datum[1].nPath = nPath1;
      }else {
         fprintf(stderr,"*********** Skipping ROI_datum structure fill.\n");
      }

      fprintf( stderr,
               "*********** Creating new data element, "
               "a column of %d elements \n", niml_ROI->N_ROI_datum);
      nel = NI_new_data_element("A_drawn_ROI",  niml_ROI->N_ROI_datum);
      
      fprintf(stderr,"*********** Adding column\n");
      NI_add_column( nel , niml_ROI_Datum_type, niml_ROI->ROI_datum );
      
      fprintf(stderr,"*********** Setting attributes element\n");
      NI_set_attribute (nel, "self_idcode", niml_ROI->idcode_str);
      NI_set_attribute (nel, "domain_parent_idcode", 
                             niml_ROI->Parent_idcode_str);
      NI_set_attribute (nel, "Label", niml_ROI->Label);
      sprintf(stmp,"%d", niml_ROI->iLabel);
      NI_set_attribute (nel, "iLabel", stmp);
      sprintf(stmp,"%d", niml_ROI->Type);
      NI_set_attribute (nel, "Type", stmp);
   
      /* Now write the element */
      ns = NI_stream_open( "fd:1" , "w" ) ;
      if (NI_write_element( ns , nel , NI_TEXT_MODE | NI_HEADERSHARP_FLAG ) < 0) {
         fprintf(stderr,"*********** Badness, failed to write nel\n");
      } 
      NI_stream_close( ns ) ; 

      /* free nel */
      NI_free_element(nel) ; nel = NULL;
      
      /* free the rest */
      free(nPath0);
      free(nPath1);
      free(idcode_str);
      free(Parent_idcode_str);
      free(Label);
   }

}

/*!
   \brief, creates a srtucture for holding communication variables 
   
   - free returned structure with SUMA_free
*/
SUMA_COMM_STRUCT *SUMA_Create_CommSrtuct(void)
{
   static char FuncName[]={"SUMA_Create_CommSrtuct"};
   SUMA_COMM_STRUCT *cs=NULL;
   int i;
   
   SUMA_ENTRY;
   
   cs = (SUMA_COMM_STRUCT *)SUMA_malloc(sizeof(SUMA_COMM_STRUCT));
   if (!cs) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   
   cs->talk_suma = 0;
   cs->comm_NI_mode = NI_BINARY_MODE;
   cs->rps = -1.0;
   cs->Send = NOPE;
   cs->afni_Send = NOPE;
   cs->GoneBad = NOPE;
   cs->afni_GoneBad = NOPE;
   cs->nelps = -1.0;
   cs->TrackID = 0;
   cs->istream = -1; /* the index of the stream in SUMAg_CF->ns_v */
   cs->afni_istream = -1;
   cs->suma_host_name = NULL;
   cs->afni_host_name = NULL;
   cs->kth = 1;
   cs->Feed2Afni = 0;
   for (i=0; i<SUMA_N_DSET_TYPES; ++i) cs->ElInd[i] = 0;
   SUMA_RETURN(cs);
}

SUMA_COMM_STRUCT *SUMA_Free_CommSrtuct(SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_Free_CommSrtuct"};
   
   SUMA_ENTRY;
   
   if (cs) {
      if (cs->suma_host_name) SUMA_free(cs->suma_host_name); cs->suma_host_name = NULL;
      if (cs->afni_host_name) SUMA_free(cs->afni_host_name); cs->afni_host_name = NULL;
      SUMA_free(cs);   
   }
   
   SUMA_RETURN(NULL);
}

/*! assign new afni host name 
    SUMA_Assign_HostName (cf, HostName, istream)
   
   Assigns a new HostName for niml communication on a particular stream
   
   \param cf (SUMA_CommonFields *) pointer to Common Fields structure, field AfniHostName will be modified here
   \param HostName (char *) hostname in IP number form, or name form afni.nimh.nih.gov or afni (if in /etc/hosts file)
                                 NULL to set cf->HostName_v[istream] to localhost if i = SUMA_AFNI_STREAM_INDEX
                                                                                 127.0.0.1 otherwise. That's done to keep
                                                                                 Shared Memory communication betwen AFNI 
                                                                                 and SUMA only.
   \param istream (int) if -1 then all streams are set to HostName
                        otherwise, only HostName_v[istream] is set
   \ret ans (SUMA_Boolean) YUP/NOPE
   
   
*/
SUMA_Boolean SUMA_Assign_HostName (SUMA_CommonFields *cf, char *HostName, int istream)
{
   static char FuncName[]={"SUMA_Assign_HostName"};
   int istart = 0, istop = 0, i = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (istream == -1) {
      istart = 0; istop = SUMA_MAX_STREAMS; 
   } else {
      istart = istream; istop = istream + 1;
   }
   
   for (i = istart; i < istop; ++i) {
      if (HostName == NULL)
         if (i == SUMA_AFNI_STREAM_INDEX) {
            sprintf(cf->HostName_v[i], "localhost"); /*  using localhost will allow the use of Shared Memory.
                                                         That is only allowed for SUMA<-->AFNI */
         } else {
            sprintf(cf->HostName_v[i], "127.0.0.1");  /* force TCP for the commoners */
         }  
      else {   
         if (strlen(HostName) > SUMA_MAX_NAME_LENGTH - 20) {
            fprintf(SUMA_STDERR,"Error %s: too long a host name (> %d chars).\n", FuncName, SUMA_MAX_NAME_LENGTH - 20);
            SUMA_RETURN (NOPE);
         }
         sprintf(cf->HostName_v[i],"%s", HostName);
      }

      sprintf(cf->NimlStream_v[i],"tcp:%s:%d", 
            cf->HostName_v[i], cf->TCP_port[i]);

      if (LocalHead) fprintf(SUMA_STDOUT, "%s: Set HostName %d to %s (stream name: %s)\n", 
                     FuncName, i, cf->HostName_v[i], cf->NimlStream_v[i]);
   }
   
   SUMA_RETURN (YUP);
}

/*! 
   \brief sends a full surface to SUMA
*/
SUMA_Boolean SUMA_SendSumaNewSurface(SUMA_SurfaceObject *SO, SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_SendSumaNewSurface"};
   NI_group *ngr=NULL;
   
   SUMA_ENTRY;
   
   if (!SO || !cs) { SUMA_SL_Err("NULL surface or NULL cs"); SUMA_RETURN(NOPE); }
   if (!cs->Send || !cs->talk_suma) { SUMA_SL_Err("Nothing to do"); SUMA_RETURN(NOPE); }
   
   
    
   if (0) {
      /* send the mesh since this is a new surface */
      if (!SUMA_SendToSuma (SO, cs, (void *)SO->FaceSetList, SUMA_NEW_MESH_IJK, 1)) {
         SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
         cs->Send = NOPE;
         cs->talk_suma = NOPE;
         SUMA_RETURN(NOPE);
      }
      /* now send the coordinates of the new surface */
      if (!SUMA_SendToSuma (SO, cs, (void *)SO->NodeList, SUMA_NEW_NODE_XYZ, 1)) {
         SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
         cs->Send = NOPE;
         cs->talk_suma = NOPE;
         SUMA_RETURN(NOPE);
      }
      /* now send the command to register the new surface with viewers*/
      if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_PREP_NEW_SURFACE, 1)) {
         SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
         cs->Send = NOPE;
         cs->talk_suma = NOPE;
         SUMA_RETURN(NOPE);
     }
      /* now manually clean up the function that created the new surface.
      last SUMA_SendToSuma call will only clean up the dtype that was being sent last.
      SUMA_SendToSuma can only clean when the same dtype is being sent. THAT NEEDS TO BE FIXED, perhaps send
      a flag to indicate how many objects you intend to send of any type. If it is one object
      then SendToSuma will do cleanup automatically without hangup ...*/
      SUMA_Mesh_IJK2Mesh_IJK_nel (SO, NULL, YUP, SUMA_NEW_MESH_IJK);
      SUMA_NodeXYZ2NodeXYZ_nel (SO, NULL, YUP, SUMA_NEW_NODE_XYZ);
   } else {
      /* the new way */
      ngr = SUMA_SO2nimlSO(SO, "NodeList, FaceSetList, VolPar", 1);
      if (!ngr) {
         SUMA_SL_Err("Failed to create surface");
         cs->Send = NOPE;
         cs->talk_suma = NOPE;
         SUMA_RETURN(NOPE);
      }
      /* now send the command to feed the new surface to suma*/
      if (!SUMA_SendToSuma (SO, cs, (void*)ngr, SUMA_SURFACE_OBJECT, 1)) {
         SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
         cs->Send = NOPE;
         cs->talk_suma = NOPE;
         SUMA_RETURN(NOPE);
      }
      NI_free_element(ngr); ngr = NULL;

      /* now send the command to register the new surface with viewers
         This now also causes a redisplay*/
      if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_PREP_NEW_SURFACE, 1)) {
         SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
         cs->Send = NOPE;
         cs->talk_suma = NOPE;
         SUMA_RETURN(NOPE);
      }
   }
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_Mesh_IJK_nel2Mesh_IJK(SUMA_SurfaceObject *SO, NI_element *nel)
{
   static char FuncName[]={"SUMA_Mesh_IJK_nel2Mesh_IJK"};
   SUMA_DSET_TYPE dtype;
   char *tmp=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   dtype = SUMA_Dset_Type(nel->name);
   if (dtype != SUMA_NEW_MESH_IJK && dtype != SUMA_MESH_IJK) {
      SUMA_SL_Err("Bad dtype for this function!");
      SUMA_RETURN(NOPE);
   }
   
   tmp = NI_get_attribute(nel, "domain_parent_idcode");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) {
      if (strcmp(SO->idcode_str, tmp)) {
         SUMA_SL_Err("idcode mismatch."); SUMA_RETURN(NOPE);
      }
   }
   tmp = NI_get_attribute(nel, "self_idcode");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->facesetlist_idcode_str = SUMA_copy_string(tmp);
   
   tmp = NI_get_attribute(nel, "Mesh_Dim");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->FaceSetDim = atoi(tmp);
   else SO->FaceSetDim = 0;
   
   if (SO->FaceSetDim != 3) {
      SUMA_SL_Err("FaceSetDim must be 3!"); SUMA_RETURN(NOPE);
   }
   
   if (SO->N_FaceSet) {
      if (SO->N_FaceSet == nel->vec_len/SO->FaceSetDim ) {
         if (!SO->FaceSetList) {
            SUMA_SL_Err("Bad init variables. SO->N_FaceSet == nel->vec_len/SO->FaceSetDim && !SO->FaceSetList"); SUMA_RETURN(NOPE);
         }
      } else {
         if (SO->FaceSetList) SUMA_free(SO->FaceSetList); SO->FaceSetList = NULL; 
      }
   } else {
      if (SO->FaceSetList) { SUMA_SL_Err("SO->FaceSetList should be null here!"); SUMA_RETURN(NOPE); }
   }
   SO->N_FaceSet = nel->vec_len/SO->FaceSetDim;
   if (!SO->FaceSetList) SO->FaceSetList = (int *)SUMA_malloc(nel->vec_len * sizeof(int));
   if (!SO->FaceSetList) {
      SUMA_SL_Crit("Failed to allocate for FaceSetList"); SUMA_RETURN(NOPE); 
   }
   memcpy((void*)SO->FaceSetList, nel->vec[0], nel->vec_len*sizeof(int));
   
   SUMA_RETURN(YUP);
}

/*! 
   A function to turn triangulation to nel to be sent to SUMA
   There's nothing to cleanup so worry not about making a cleanup call
   \sa SUMA_Mesh_IJK_nel2Mesh_IJK
*/
NI_element * SUMA_Mesh_IJK2Mesh_IJK_nel (SUMA_SurfaceObject *SO, int *val, SUMA_Boolean cleanup, SUMA_DSET_TYPE dtype)
{
   static char FuncName[]={"SUMA_Mesh_IJK2Mesh_IJK_nel"};
   static int i_in=0;
   char buf[500];
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   if (dtype != SUMA_NEW_MESH_IJK && dtype != SUMA_MESH_IJK) {
      SUMA_SL_Err("Bad dtype for this function!");
      SUMA_RETURN(NULL);
   }
   
   if (cleanup) {
      SUMA_LH("Cleanup..."); 
      SUMA_RETURN(NULL);
   }
   
   if (SO->FaceSetDim != 3) { /* only deals with XYZ for the moment */
      SUMA_SL_Err("FaceSetDim must be 3!");
      SUMA_RETURN(nel);
   }
   
   if (!i_in) {
      /* Initialization block. Nothing to do , really */
   }
   
   
   /* Now create that data element and write it out */
   SUMA_allow_nel_use(1);
   nel = SUMA_NewNel (  dtype, /* one of SUMA_DSET_TYPE */
                        SO->idcode_str, /* idcode of Domain Parent */
                        NULL, /* idcode of geometry parent, not useful here*/
                        3*SO->N_FaceSet,
                        NULL,
                        SO->facesetlist_idcode_str);
   if (!nel) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_NewNel", FuncName);
      SUMA_RETURN(NULL);
   }
   
   sprintf(buf, "%d", SO->FaceSetDim);
   NI_set_attribute (nel, "Mesh_Dim", buf);
   
   /* set the label */
   if (SO->Label) {
      sprintf(buf, "FaceSetList for surface %s", SO->Label);
      NI_set_attribute (nel, "Object_Label", buf);
   } else {
      NI_set_attribute (nel, "Object_Label", SUMA_EMPTY_ATTR);
   } 
     
   #if 0 /* no longer needed */
      if (!SO->idcode_str) { SUMA_SL_Err("Surface has a NULL idcode_str, BAD.\n"); SUMA_RETURN(NULL);} 
      NI_set_attribute (nel, "surface_idcode", SO->idcode_str);   

      if (!SO->Group) { SUMA_SL_Err("Surface has a NULL Group, BAD.\n"); SUMA_RETURN(NULL);} 
      NI_set_attribute(nel, "Group", SO->Group);
      if (!SO->Label) { NI_set_attribute(nel, "Label", "Def_MeshIJK2MeshIJK_nel"); }
      else NI_set_attribute(nel, "Label", SO->Label);
      if (!SO->State) { SUMA_SL_Err("Surface has a NULL state, BAD.\n"); SUMA_RETURN(NULL);} 
      NI_set_attribute(nel, "State", SO->State);
      sprintf(buf, "%d", SO->N_Node);
      NI_set_attribute(nel, "N_Node", buf);
      NI_set_attribute(nel, "EmbedDim", "3");
      NI_set_attribute(nel, "AnatCorrect", "1");
   #endif
   
   #if 0 /* the old way, no need for embellishments */
   /* Add the coordinate column */
   if (!SUMA_AddNelCol (nel, /* the famed nel */ 
                        "IJK indices", 
                        SUMA_NODE_INT, /* the column's type (description),
                                            one of SUMA_COL_TYPE */
                        (void *)val, /* the coordinates */
                        NULL  /* that's an optional structure containing 
                                 attributes of the added column. 
                                 Not used at the moment */
                        ,1
                        )) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
      SUMA_RETURN(NULL);                    
   }
   #else
      NI_add_column_stride ( nel, NI_INT, val, 1 );
   #endif

   ++i_in; 
         
   /* return the element */
   SUMA_RETURN(nel); 

}    

/*!
   The inverse of SUMA_NodeXYZ2NodeXYZ_nel
*/
SUMA_Boolean SUMA_NodeXYZ_nel2NodeXYZ (SUMA_SurfaceObject *SO, NI_element *nel)
{
   static char FuncName[]={"SUMA_NodeXYZ_nel2NodeXYZ"};
   char *tmp = NULL;
   SUMA_DSET_TYPE dtype;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   dtype = SUMA_Dset_Type(nel->name);
   
   if (dtype != SUMA_NODE_XYZ && dtype != SUMA_NEW_NODE_XYZ) {
      SUMA_SL_Err("Bad nel for this function");
      SUMA_RETURN(NOPE);
   }
   
   tmp = NI_get_attribute(nel, "Node_Dim");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) {
      SO->NodeDim = atoi(tmp);
      if (SO->NodeDim != 3) {
         SUMA_SL_Err("Not willing to deal with SO->NodeDim != 3");
         SUMA_RETURN(NOPE);
      }
   }

   tmp = NI_get_attribute(nel, "self_idcode");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) 
      SO->nodelist_idcode_str = SUMA_copy_string(tmp);

   tmp = NI_get_attribute(nel, "domain_parent_idcode");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) {
      if (strcmp(tmp, SO->idcode_str)) {
         SUMA_SL_Err("idcode of parent mismatch"); SUMA_RETURN(NOPE);
      }
   }

   /* how many elements? */
   if (SO->N_Node) {
      if (SO->N_Node == nel->vec_len/SO->NodeDim) {
         if (!SO->NodeList) {
            SUMA_SL_Err("Bad initial values in SO.\n"
                        "SO->N_Node == nel->vec_len/3 \n"
                        "but NULL SO->NodeList");
            SUMA_RETURN(NOPE); 
         } 
      } else {
         /* gotta cleanup */
         if (SO->NodeList) 
            SUMA_free(SO->NodeList); 
         SO->NodeList = NULL; SO->N_Node = 0;
      }
   } else {
      if (SO->NodeList) { 
         SUMA_SL_Err("Should not have a NodeList here"); 
         SUMA_RETURN(NOPE); 
      }
   }
   
   SO->N_Node = nel->vec_len/SO->NodeDim;
   if (!SO->NodeList) SO->NodeList = (float *)SUMA_malloc(nel->vec_len * sizeof(float));
   if (!SO->NodeList) {
      SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   memcpy((void *)SO->NodeList, nel->vec[0], nel->vec_len * sizeof(float));
   
   SUMA_RETURN(YUP);
}

/*! 
   A function to turn node XYZ to nel to be sent to SUMA
   There's nothing to cleanup so worry not about making a cleanup call
   \sa SUMA_NodeXYZ_nel2NodeXYZ
   
*/
NI_element * SUMA_NodeXYZ2NodeXYZ_nel (
   SUMA_SurfaceObject *SO, float *val, 
   SUMA_Boolean cleanup, SUMA_DSET_TYPE dtype)
{
   static char FuncName[]={"SUMA_NodeXYZ2NodeXYZ_nel"};
   static int i_in=0;
   char stmp[500];
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (dtype != SUMA_NEW_NODE_XYZ && dtype != SUMA_NODE_XYZ) {
      SUMA_SL_Err("Bad dtype for this function!");
      SUMA_RETURN(NULL);
   }
   
   if (cleanup) {
      SUMA_LH("Cleanup...");
      SUMA_RETURN(NULL);
   }
   
   if (SO->NodeDim != 3) { /* only deals with XYZ for the moment */
      SUMA_SL_Err("NodeDim must be 3!");
      SUMA_RETURN(nel);
   }
   
   if (!i_in) {
      /* Initialization block. Nothing to do , really */
      
   }
   
   
   /* Now create that data element and write it out */
   SUMA_allow_nel_use(1);
   nel = SUMA_NewNel (  dtype, /* one of SUMA_DSET_TYPE */
                        SO->idcode_str, /* idcode of Domain Parent Surface*/
                        NULL, /* idcode of geometry parent, not useful here*/
                        3*SO->N_Node,
                        NULL,
                        SO->nodelist_idcode_str); 
   if (!nel) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_NewNel", FuncName);
      SUMA_RETURN(NULL);
   }
   
   SUMA_LH("Setting attributes");
   sprintf(stmp, "%d", SO->NodeDim);
   NI_set_attribute (nel, "Node_Dim", stmp);

   #if 0 /* no longer needed */
   /* set the surface idcode attribute */
   NI_set_attribute (nel, "surface_idcode", SO->idcode_str);   
   #endif
   
   /* set the label */
   if (SO->Label) {
      SUMA_LH(" label");
      sprintf(stmp, "NodeList for surface %s", SO->Label);
      NI_set_attribute (nel, "Object_Label", stmp);
   } else {
      SUMA_LH(" no label");
      NI_set_attribute (nel, "Object_Label", SUMA_EMPTY_ATTR);
   }
         
   SUMA_LH("Adding data");
   #if 0 /* old way */
   /* Add the coordinate column */
   if (!SUMA_AddNelCol (nel, /* the famed nel */ 
                        "XYZ coords", 
                        SUMA_NODE_3C, /* the column's type (description),
                                            one of SUMA_COL_TYPE */
                        (void *)val, /* the coordinates */
                        NULL  /* that's an optional structure containing 
                                 attributes of the added column. 
                                 Not used at the moment */
                        ,1
                        )) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
      SUMA_RETURN(NULL);                    
   }
   #else /* new way, no need for embellishments */
      NI_add_column_stride ( nel, NI_FLOAT, val, 1 );
   #endif
   ++i_in; 
         
   /* return the element */
   SUMA_RETURN(nel); 

   
}

/*!
   \brief the inverse of SUMA_SOVolPar2VolPar_nel
*/
SUMA_Boolean SUMA_VolPar_nel2SOVolPar(SUMA_SurfaceObject *SO, NI_element *nel) 
{
   static char FuncName[]={"SUMA_VolPar_nel2SOVolPar"};
   char *tmp;
   float fv15[15];
   double dv15[15];
   SUMA_DSET_TYPE dtype;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
  
   dtype = SUMA_Dset_Type(nel->name);
   if (dtype != SUMA_SURFACE_VOLUME_PARENT) {
      SUMA_SL_Err("Bad dtype for this function!");
      SUMA_RETURN(NOPE);
   }
   
   if (SO->VolPar) { SUMA_SL_Err("SO->VolPar must be NULL here"); SUMA_RETURN(NOPE); }
   SO->VolPar = SUMA_Alloc_VolPar();
   
   tmp = NI_get_attribute(nel, "self_idcode");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->VolPar->idcode_str = SUMA_copy_string(tmp);

   tmp = NI_get_attribute(nel, "domain_parent_idcode");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) {
      if (strcmp(tmp, SO->idcode_str)) {
         SUMA_SL_Err("idcode of parent mismatch"); SUMA_RETURN(NOPE);
      }
   }
   
   tmp = NI_get_attribute(nel, "isanat"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->VolPar->isanat = atoi(tmp);
   
   tmp = NI_get_attribute(nel, "axis_hand"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->VolPar->Hand = atoi(tmp);
   
   tmp = NI_get_attribute(nel, "prefix"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->VolPar->prefix = SUMA_copy_string(tmp);
   
   tmp = NI_get_attribute(nel, "filecode"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->VolPar->filecode = SUMA_copy_string(tmp);
   
   tmp = NI_get_attribute(nel, "dirname"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->VolPar->dirname = SUMA_copy_string(tmp);
   
   tmp = NI_get_attribute(nel, "vol_idcode_str"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->VolPar->vol_idcode_str = SUMA_copy_string(tmp);
   
   tmp = NI_get_attribute(nel, "vol_idcode_date"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->VolPar->vol_idcode_date = SUMA_copy_string(tmp);
   
   tmp = NI_get_attribute(nel, "nxyz"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) { SUMA_StringToNum(tmp, (void *)fv15, 3,1); SO->VolPar->nx = (int)fv15[0]; SO->VolPar->ny = (int)fv15[1];   SO->VolPar->nz = (int)fv15[2]; }
   
   tmp = NI_get_attribute(nel, "xyzorient"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) { SUMA_StringToNum(tmp, (void *)fv15, 3,1); SO->VolPar->xxorient = (int)fv15[0]; SO->VolPar->yyorient = (int)fv15[1];   SO->VolPar->zzorient = (int)fv15[2]; }

   tmp = NI_get_attribute(nel, "dxyz"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) { SUMA_StringToNum(tmp, (void *)fv15, 3,1); SO->VolPar->dx = fv15[0]; SO->VolPar->dy = fv15[1];   SO->VolPar->dz = fv15[2]; }
   
   tmp = NI_get_attribute(nel, "xyzorg"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) { SUMA_StringToNum(tmp, (void *)fv15, 3,1); SO->VolPar->xorg = fv15[0]; SO->VolPar->yorg = fv15[1];   SO->VolPar->zorg = fv15[2]; }
      
   tmp = NI_get_attribute(nel, "CENTER_OLD"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) { 
      SUMA_StringToNum(tmp, (void*)dv15, 3,2); 
      SO->VolPar->CENTER_OLD = (double*)SUMA_malloc(sizeof(double)*3);
      SUMA_COPY_VEC(fv15, SO->VolPar->CENTER_OLD, 2, double, double);
   }
   
   tmp = NI_get_attribute(nel, "CENTER_BASE"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) { 
      SUMA_StringToNum(tmp, (void*)dv15, 3,2); 
      SO->VolPar->CENTER_BASE = (double*)SUMA_malloc(sizeof(double)*3);
      SUMA_COPY_VEC(fv15, SO->VolPar->CENTER_BASE, 2, double, double);
   }
   
   tmp = NI_get_attribute(nel, "MATVEC"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) { 
      SUMA_StringToNum(tmp, dv15, 12,2); 
      SO->VolPar->MATVEC = (double*)SUMA_malloc(sizeof(double)*12);
      SUMA_COPY_VEC(fv15, SO->VolPar->MATVEC, 2, double, double);
   }
   
   tmp = NI_get_attribute(nel, "MATVEC_source"); 
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) { 
      SO->VolPar->MATVEC_source = (SUMA_WARP_TYPES)atoi(tmp);
   }
   SUMA_RETURN(YUP);
}

/*!
   A function to turn the VolPar structure to a nel, this one's a group
   \sa SUMA_VolPar_nel2SOVolPar
*/
NI_element *SUMA_SOVolPar2VolPar_nel (SUMA_SurfaceObject *SO, 
                                       SUMA_VOLPAR *VolPar, SUMA_DSET_TYPE dtype)
{
   static char FuncName[]={"SUMA_SOVolPar2VolPar_nel"};
   NI_element *nel=NULL;
   int ibuf3[3], i;
   float fbuf3[3];   
   char stmp[500];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
  
   if (dtype != SUMA_SURFACE_VOLUME_PARENT) {
      SUMA_SL_Err("Bad dtype for this function!");
      SUMA_RETURN(NULL);
   }
   
   if (!VolPar) {
      SUMA_SL_Err("NULL VolPar");
      SUMA_RETURN(NULL);
   }
   
   
   if (!VolPar->idcode_str) { SUMA_NEW_ID(VolPar->idcode_str, NULL); }
   
   /* Now create that data element and write it out */
   SUMA_allow_nel_use(1);
   nel = SUMA_NewNel (  dtype, /* one of SUMA_DSET_TYPE */
                        SO->idcode_str, /* idcode of Domain Parent Surface*/
                        NULL, /* idcode of geometry parent, not useful here*/
                        0,
                        NULL,
                        VolPar->idcode_str); 
   if (!nel) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_NewNel", FuncName);
      SUMA_RETURN(NULL);
   }
   
   if (SO->Label) {
      sprintf(stmp,"Volume parent of %s", SO->Label);
      NI_set_attribute(nel, "Object_Label", stmp);
   } else {
      NI_set_attribute(nel, "Object_Label", SUMA_EMPTY_ATTR);
   }
   
   sprintf(stmp,"%d", VolPar->isanat); 
   NI_set_attribute(nel, "isanat", stmp);

   sprintf(stmp,"%d", VolPar->Hand); 
   NI_set_attribute(nel, "axis_hand", stmp);   
   
   if (VolPar->prefix) NI_set_attribute(nel, "prefix", VolPar->prefix);
   else NI_set_attribute(nel, "prefix", SUMA_EMPTY_ATTR);
   
   if (VolPar->filecode) NI_set_attribute(nel, "filecode", VolPar->filecode);
   else NI_set_attribute(nel, "filecode", SUMA_EMPTY_ATTR);
   
   if (VolPar->dirname) NI_set_attribute(nel, "dirname", VolPar->dirname);
   else NI_set_attribute(nel, "dirname", SUMA_EMPTY_ATTR);
   
   if (VolPar->vol_idcode_str) NI_set_attribute(nel, "vol_idcode_str", VolPar->vol_idcode_str);
   else NI_set_attribute(nel, "vol_idcode_str", SUMA_EMPTY_ATTR);
      
   if (VolPar->vol_idcode_date) NI_set_attribute(nel, "vol_idcode_date", VolPar->vol_idcode_date);
   else NI_set_attribute(nel, "vol_idcode_date", SUMA_EMPTY_ATTR);
   
   sprintf(stmp, "%d %d %d", VolPar->nx, VolPar->ny, VolPar->nz);
   NI_set_attribute(nel, "nxyz", stmp);
   
   sprintf(stmp, "%d %d %d", VolPar->xxorient, VolPar->yyorient, VolPar->zzorient);
   NI_set_attribute(nel, "xyzorient", stmp);
   
   sprintf(stmp, "%f %f %f", VolPar->dx, VolPar->dy, VolPar->dz);
   NI_set_attribute(nel, "dxyz", stmp);
   
   sprintf(stmp, "%f %f %f", VolPar->xorg, VolPar->yorg, VolPar->zorg);
   NI_set_attribute(nel, "xyzorg", stmp);
   
   if (VolPar->CENTER_OLD) {
      stmp[0] = '\0';
      for (i=0; i<3; ++i) sprintf(stmp,"%s %f", stmp, VolPar->CENTER_OLD[i]);
      NI_set_attribute(nel, "CENTER_OLD", stmp);
   }
   if (VolPar->CENTER_BASE) {
      stmp[0] = '\0';
      for (i=0; i<3; ++i) sprintf(stmp,"%s %f", stmp, VolPar->CENTER_BASE[i]);
      NI_set_attribute(nel, "CENTER_BASE", stmp);
   }
   
   if (VolPar->MATVEC) {
      stmp[0] = '\0';
      for (i=0; i<12; ++i) sprintf(stmp,"%s %f", stmp, VolPar->MATVEC[i]);
      NI_set_attribute(nel, "MATVEC", stmp);
   }
   
   sprintf(stmp, "%d", VolPar->MATVEC_source);
   NI_set_attribute(nel, "MATVEC_source", stmp);

   SUMA_RETURN(nel);  
}

/*! Macro specific for SUMA_NodeVal2irgba_nel */
#define SUMA_NODEVAL2IRGBA_CLEANUP { \
   if (node) SUMA_free(node); node = NULL;   \
   if (OptScl) SUMA_free(OptScl); OptScl = NULL;   \
   if (SV) SUMA_Free_ColorScaledVect (SV); SV = NULL; \
   if (rgba) SUMA_free(rgba); rgba = NULL;   \
}
/*! 
   A function to turn node values into a colored nel to be sent to SUMA
   \param SO (SUMA_SurfaceObject *) Surface object, domain of data
   \param val (float *)  vector of node values to be colored and stored as nel
   \param instanceID (char *) a unique identifier used to tag a set of val vectors that are sent
                              through repeated calls. With a new instanceID, static arrays are newly
                              allocated and will continue to be used as long as instanceID does not change
                              When instanceID changes, the function cleans up and reallocates automatically.
   \param option (int) Set this flag to:
                         1 to signal that you are done using this function for good and 
                                 want to make sure any local allocations are freed. 
                                 Returns NULL, does not need any previous params
                         -1 to signal that this function is to be called just once under a particular
                                 instance and that the function should cleanup before it returns.  
                                 Returns valid nel, requires all params. 
                                 You can also use -1 if that is the last call in a series
                        0 to signal that this function will still be called under that instance                           
                              
*/
NI_element * SUMA_NodeVal2irgba_nel (SUMA_SurfaceObject *SO, float *val, char *instanceID, int cleanup)
{
   static char FuncName[]={"SUMA_NodeVal2irgba_nel"};
   static int i_in=0, *node=NULL;
   static SUMA_COLOR_MAP *CM=NULL;
   static SUMA_SCALE_TO_MAP_OPT * OptScl=NULL;
   static int MapType;
   static SUMA_COLOR_SCALED_VECT * SV=NULL;
   static byte *rgba=NULL;
   static char past_instance[50]={""};
   char idcode_str[50];
   NI_element *nel=NULL;
   int i, i4, i3; 
   float IntRange[2], *Vsort= NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   if (cleanup == 1) {
      SUMA_LH("Cleanup...");
      SUMA_NODEVAL2IRGBA_CLEANUP;
      past_instance[0]='\0';
      i_in = 0;
      SUMA_RETURN(NULL);
   }

   if (!instanceID) {
      SUMA_SL_Err("This function requires instanceID to be non-null.");
      SUMA_RETURN(NULL);
   }
   
   if (strcmp(instanceID, past_instance)) {
      SUMA_LH("A new instance"); 
      /* clean up if necessary */
      if (i_in) SUMA_NODEVAL2IRGBA_CLEANUP;
      i_in = 0;
      sprintf(past_instance,"%s", instanceID);
   }
   
   
       
   if (!i_in) {
      /* first time around */
      /* create the color mapping of Cx (SUMA_CMAP_MATLAB_DEF_BYR64)*/
      CM = SUMA_FindNamedColMap ("byr64");
      if (CM == NULL) {
         fprintf (SUMA_STDERR,
                  "Error %s: Could not get standard colormap.\n", FuncName); 
         SUMA_RETURN (NULL);
      }

      /* get the options for creating the scaled color mapping */
      OptScl = SUMA_ScaleToMapOptInit();
      if (!OptScl) {
         fprintf (SUMA_STDERR,
                  "Error %s: Could not get scaling option structure.\n", 
                  FuncName);
         SUMA_RETURN (NULL); 
      }

      /* work the options a bit */
      OptScl->ApplyClip = NOPE;
      OptScl->MaskZero = NOPE;
      IntRange[0] = 0; IntRange[1] = 100; /* percentile clipping range*/ 
      Vsort = SUMA_PercRange (val, NULL, SO->N_Node, IntRange, IntRange, NULL); 
      if (Vsort[0] < 0 && Vsort[SO->N_Node -1] > 0 ) {
         /* the new method */
         if (fabs(IntRange[0]) > IntRange[1]) {
            IntRange[1] = -IntRange[0];
         } else {
            IntRange[0] = -IntRange[1];
         }
      } 
      OptScl->IntRange[0] = IntRange[0]; OptScl->IntRange[1] = IntRange[1];
      OptScl->BrightFact = 1.0;

      /* create structure to hold the colored values */
      SV = SUMA_Create_ColorScaledVect(SO->N_Node, 0);
      if (!SV) {
         fprintf (SUMA_STDERR,
                  "Error %s: Could not allocate for SV.\n", FuncName);
         SUMA_RETURN (NULL);
      }
      
      /* node vector */
      node = (int *) SUMA_malloc(sizeof(int) * SO->N_Node);
      /* color vectors to hold RGBA colors*/
      rgba = (byte *) SUMA_malloc(sizeof(byte) * SO->N_Node * 4);
      if (!node || !rgba) {
         SUMA_SL_Err("Failed to allocate for node or rgba.");
         SUMA_RETURN(NULL);
      }
      for (i=0; i < SO->N_Node; ++i) node[i] = i;
      
      if (Vsort) SUMA_free(Vsort); Vsort = NULL;
   }
    
   /* map the values in val to the colormap */

   /* finally ! */
   if (!SUMA_ScaleToMap (val, SO->N_Node, OptScl->IntRange[0], OptScl->IntRange[1], CM, OptScl, SV)) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_ScaleToMap.\n", FuncName);
      SUMA_RETURN (NOPE);
   }             
               
   /* copy the colors to rgba */
   for (i=0; i < SO->N_Node; ++i) {
      i4 = 4 * i;
      i3 = 3 *i;
      rgba[i4] = (byte)(SV->cV[i3  ] * 255); ++i4;
      rgba[i4] = (byte)(SV->cV[i3+1] * 255); ++i4;
      rgba[i4] = (byte)(SV->cV[i3+2] * 255); ++i4;
      rgba[i4] = 255;
   }
   
   /* now create the niml element */
   UNIQ_idcode_fill (idcode_str);
   /* Now create that data element and write it out */
   SUMA_allow_nel_use(1);
   nel = SUMA_NewNel (  SUMA_NODE_RGBAb, /* one of SUMA_DSET_TYPE */
                        SO->idcode_str, /* idcode of Domain Parent */
                        NULL, /* idcode of geometry parent, not useful here*/
                        SO->N_Node,/* Number of elements */
                        NULL, NULL); 
   if (!nel) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_NewNel", FuncName);
      SUMA_RETURN(NULL);
   }
   /* set the surface idcode attribute */
   NI_set_attribute (nel, "surface_idcode", SO->idcode_str);   
   
   /* Add the columns */
   SUMA_allow_nel_use(1);
   if (!SUMA_AddNelCol (nel, /* the famed nel */ 
                        "node index", 
                        SUMA_NODE_INDEX, /* the column's type (description),
                                            one of SUMA_COL_TYPE */
                        (void *)node, /* the list of node indices */
                        NULL  /* that's an optional structure containing 
                                 attributes of the added column. 
                                 Not used at the moment */
                        ,1 /* stride, useful when you need to copy a column
                              from a multiplexed vector. Say you have in p 
                              [rgb rgb rgb rgb], to get the g column you 
                              send in p+1 for the column pointer and a stride
                              of 3 */
                        )) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
      SUMA_RETURN(NULL);                    
   }

   /* insert from multiplexed rgb vector */
   SUMA_allow_nel_use(1);
   if (!SUMA_AddNelCol (nel, "red", SUMA_NODE_Rb, (void *)rgba, NULL ,4 )) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
      SUMA_RETURN(NULL);
   }

   SUMA_allow_nel_use(1);
   if (!SUMA_AddNelCol (nel, "green", SUMA_NODE_Gb, (void *)(rgba+1), NULL ,4)) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
      SUMA_RETURN(NULL);
   }

   SUMA_allow_nel_use(1);
   if (!SUMA_AddNelCol (nel, "blue", SUMA_NODE_Bb, (void *)(rgba+2), NULL ,4)) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
      SUMA_RETURN(NULL);
   }
   
   SUMA_allow_nel_use(1);
   if (!SUMA_AddNelCol (nel, "alpha", SUMA_NODE_Ab, (void *)(rgba+3), NULL ,4)) {
      fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
      SUMA_RETURN(NULL);
   }
   
   
   ++i_in; 
   
   if (cleanup == -1) {
      SUMA_LH("Last or one call only cleanup");
      SUMA_NODEVAL2IRGBA_CLEANUP;
      past_instance[0]='\0';
      i_in = 0;
   }
         
   /* return the element */
   SUMA_RETURN(nel); 
               
} 
#define SUMA_SEND_TO_SUMA_FUNC_CLEANUP {   \
      /* call all nel forming functions with cleanup. */ \
      SUMA_LH("Cleanup for SUMA_NodeVal2irgba_nel...");  \
      SUMA_NodeVal2irgba_nel (NULL, NULL, NULL, 1);   \
      SUMA_LH("Cleanup for SUMA_NodeXYZ2NodeXYZ_nel...");   \
      SUMA_NodeXYZ2NodeXYZ_nel (NULL, NULL, 1, SUMA_NODE_XYZ); \
      SUMA_LH("Cleanup for SUMA_Mesh_IJK2Mesh_IJK_nel..."); \
      SUMA_Mesh_IJK2Mesh_IJK_nel (NULL, NULL, 1, SUMA_NEW_MESH_IJK); \
}
void SUMA_Wait_Till_Stream_Goes_Bad(SUMA_COMM_STRUCT *cs, int slp, int WaitMax, int verb) 
{  
   static char FuncName[]={"SUMA_Wait_Till_Stream_Goes_Bad"};
   SUMA_Boolean good = YUP;
   int WaitClose = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (verb) fprintf (SUMA_STDERR,"\nWaiting for SUMA to close stream .");
   while (good && WaitClose < WaitMax) {
      if (NI_stream_goodcheck(SUMAg_CF->ns_v[cs->istream], 1) <= 0) {
         good = NOPE;
      } else {
         SUMA_LHv("Good Check OK. Sleeping for %d ms...", slp);
         NI_sleep(slp);
         if (verb) fprintf (SUMA_STDERR,".");
         WaitClose += slp;
      }
   }

   if (WaitClose >= WaitMax) { 
      if (verb) SUMA_S_Warnv("\nFailed to detect closed stream after %d ms.\nClosing shop anyway...", WaitMax);  
   }else{
      if (verb) fprintf (SUMA_STDERR,"Done.\n");
   }

   SUMA_RETURNe;
}
         
/*!
   \brief Function to handle send data elements to AFNI
   \param SO (SUMA_SurfaceObject *) pointer to surface object structure
   \param cs (SUMA_COMM_STRUCT *) Communication structure. (initialized when action is 0)
   \param data (void *) pointer to data that gets typecast as follows:
                        (float *) if dtype == Node_RGBAb or Node_XYZ
   \param dtype (SUMA_DSET_TYPE) Type of nel to be produced (this determines the typecasting of data)
   \param instanceID (char *) a unique identifier for the instance of data sent.
                              For data of a particular dtype, use same instanceID for data that is being sent repeatedly
   \param action (int)  2: Make cleanup call to functions producing nel out of data
                           Close stream
                        1: Create a nel out of data and send to AFNI
                        0: start connection with AFNI 
                           initialize cs
                           prepare functions producing
                           nels out of data
   \return errflag (SUMA_Boolean) YUP: All is OK (although connection might get closed)
                                  NOPE: Some'in bad a happening.
                                  Connections getting closed in the midst of things are
                                  not considered as errors because they should not halt 
                                  the execution of the main program
   NOTE: The cleanup automatically closes the connection. That is stupid whenever you need to 
   send multiple types of data for multiple surfaces. Cleanup should be done without closing connections!
   See comment in function SUMA_SendSumaNewSurface's code.
   Also, send kth should be more clever, keeping separate counts per datatype and per surface
   
   NOTE: For some data (lile                                  
*/
SUMA_Boolean SUMA_SendToSuma (SUMA_SurfaceObject *SO, SUMA_COMM_STRUCT *cs, void *data, SUMA_DSET_TYPE dtype, int action)
{
   static char FuncName[]={"SUMA_SendToSuma"};
   static float etm = 0.0;
   static int i_in = 0;
   char stmp[500];
   static struct  timeval tt;
   NI_element *nel=NULL;
   NI_group *ngr = NULL;
   float *f=NULL;
   int n=-1, WaitClose, WaitMax, *ip = NULL;
   float wtm;
   SUMA_Boolean good = YUP;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* fprintf (SUMA_STDERR, "%s: LocalHead = %d\n", FuncName, LocalHead); */
   
   if (action == 0) { /* initialization of connection */
      
      SUMA_LH("Setting up for communication with SUMA ...");
      cs->Send = YUP;
      if(!SUMA_Assign_HostName (SUMAg_CF, cs->suma_host_name, cs->istream)) {
		   fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Assign_HostName", FuncName);
		   exit (1);
	   }
      if (!SUMA_niml_call (SUMAg_CF, cs->istream, NOPE)) {
         SUMA_SL_Err("Failed in SUMA_niml_call");
         /* connection flag is reset in SUMA_niml_call */
         cs->Send = NOPE;
         SUMA_RETURN(NOPE);
      }

      nel = NI_new_data_element("StartTracking", 0); 
      cs->TrackID = 1; /* that's the index for StartTracking command */
      NI_set_attribute(nel,"ni_stream_name",  SUMAg_CF->NimlStream_v[cs->istream]);
      sprintf(stmp, "%d", cs->TrackID);
      NI_set_attribute(nel,"Tracking_ID", stmp);
      if (NI_write_element( SUMAg_CF->ns_v[cs->istream] , nel, cs->comm_NI_mode ) < 0) {
         SUMA_SL_Err("Failed to start tracking.\nContinuing...");
      } 
      if (nel) NI_free_element(nel); nel = NULL;
      
      /* here is where you would start the workprocess for this program
      But since communication is one way, then forget about it */
      ++i_in;
      SUMA_RETURN(YUP);
   }
   
   if (action == 1) { /* action == 1,  send data mode */
      if (!i_in) {
         SUMA_SL_Err("You must call SUMA_SendToSuma with action 0 before action 1.\nNo Communcation cleanup done.");
         cs->Send = NOPE;
         SUMA_RETURN(NOPE);
      }
      if ((cs->ElInd[dtype] % cs->kth)) {
         if (LocalHead) fprintf(SUMA_STDERR,"%s: Skipping element %d of type %d\n", FuncName, cs->ElInd[dtype], dtype);
         ++cs->ElInd[dtype];
         SUMA_RETURN(YUP);
      }
      ++cs->ElInd[dtype];
      SUMA_LH("Creating nel and sending it");
      switch (dtype) {
         case SUMA_NODE_RGBAb:
         case SUMA_NODE_XYZ:
         case SUMA_NEW_NODE_XYZ:
            n = 3 * SO->N_Node;
            f = (float *)data;
            break;
         case SUMA_NEW_MESH_IJK:
         case SUMA_MESH_IJK:
            n = 3 * SO->N_FaceSet;
            ip = (int *)data;
            break;
         case SUMA_PREP_NEW_SURFACE:
            break;
         case SUMA_SURFACE_OBJECT:
         case SUMA_SEGMENT_OBJECT:
         case SUMA_ENGINE_INSTRUCTION:   
            break;
         default:
            SUMA_SL_Err("Data type not supported.");
            cs->GoneBad = YUP;
            cs->Send = NOPE;
            SUMA_RETURN(NOPE);
            break;
      }

      /* make sure stream is till OK */
      if (NI_stream_goodcheck ( SUMAg_CF->ns_v[cs->istream] , 1 ) < 0) {
         cs->GoneBad = YUP;
         SUMA_SL_Warn("Communication stream gone bad.\nShutting down communication.");
         cs->Send = NOPE;
         SUMA_SEND_TO_SUMA_FUNC_CLEANUP;
         SUMA_RETURN(YUP); /* returning without error since program should continue */
      }

      
      nel = NULL; ngr = NULL;
      switch (dtype) {
         case SUMA_NODE_RGBAb:
            /* colorize data */
            nel = SUMA_NodeVal2irgba_nel (SO, f, SO->idcode_str, 0);
            if (!nel) {
               SUMA_SL_Err("Failed in SUMA_NodeVal2irgba_nel.\nCommunication off.")
               cs->Send = NOPE;
               SUMA_RETURN(NOPE);
            }
            break;
         case SUMA_NODE_XYZ:
         case SUMA_NEW_NODE_XYZ:
            /* turn XYZ to nel  */
            nel =  SUMA_NodeXYZ2NodeXYZ_nel(SO, f, NOPE, dtype);
            if (!nel) {
               SUMA_SL_Err("Failed in SUMA_NodeXYZ2NodeXYZ_nel.\nCommunication off.")
               cs->Send = NOPE;
               SUMA_RETURN(NOPE);
            }
            if (cs->Feed2Afni) NI_set_attribute(nel, "Send2Afni", "DoItBaby");
            break;
         case SUMA_MESH_IJK:
         case SUMA_NEW_MESH_IJK:
            /* turn IJK to nel  */
            nel =  SUMA_Mesh_IJK2Mesh_IJK_nel(SO, ip, NOPE, dtype);
            if (!nel) {
               SUMA_SL_Err("Failed in SUMA_Mesh_IJK2Mesh_IJK_nel.\nCommunication off.")
               cs->Send = NOPE;
               SUMA_RETURN(NOPE);
            }
            break;
         case SUMA_PREP_NEW_SURFACE:
            nel = NI_new_data_element(SUMA_Dset_Type_Name(dtype), 0);
            NI_set_attribute (nel, "surface_idcode", SO->idcode_str);
            if (SO->VolPar) {
               char *vppref=NULL;
               vppref = SUMA_append_replace_string(SO->VolPar->dirname, SO->VolPar->filecode, "/", 0);
               NI_set_attribute(nel, "VolParFilecode", vppref); SUMA_free(vppref); vppref = NULL;
               if (cs->Feed2Afni) NI_set_attribute(nel, "Send2Afni", "DoItBaby");
            }
            break;
         case SUMA_SURFACE_OBJECT:
         case SUMA_SEGMENT_OBJECT:
         case SUMA_ENGINE_INSTRUCTION:
            ngr = (NI_group *)data;
            break;
         default:
            SUMA_SL_Err("Unexpected element. Ignoring.");
            SUMA_RETURN(YUP);
            break;   
      }
      
        
      if (!nel && !ngr) {/* !nel */
         SUMA_SL_Err("Flow error.");
         SUMA_RETURN(NOPE);
      }else {/* !nel */
         if (nel && ngr) {
            SUMA_SL_Err("Flow error.");
            SUMA_RETURN(NOPE);
         }
         /* add tracking */
         ++cs->TrackID;
         sprintf(stmp,"%d", cs->TrackID);
         if (nel) {
            NI_set_attribute (nel, "Tracking_ID", stmp);
         } else if (ngr) {
            NI_set_attribute (ngr, "Tracking_ID", stmp);
         }
      }
      
      #if SUMA_SUMA_NIML_DEBUG /* writes every element to a text file for debugging ... */
      {
         NI_stream ns;  
         /* Test writing results in asc, 1D format */ 
         if (LocalHead) fprintf(stderr," %s:-\nWriting ascii 1D ...\n"
                        , FuncName);
         /* open the stream */
         sprintf(stmp, "file:niml_dbg_asc_TID_%d_.1D",cs->TrackID);
         ns = NI_stream_open( stmp , "w" ) ;
         if( ns == NULL ){
           fprintf (stderr,"Error  %s:\nCan't open Test_write_asc_1D!"
                        , FuncName); 
            SUMA_RETURN(NOPE);
         }

         if (nel) {
            /* write out the element */
            if (NI_write_element( ns , nel ,
                                  NI_TEXT_MODE | NI_HEADERSHARP_FLAG ) < 0) {
               fprintf (stderr,"Error  %s:\nFailed in NI_write_element"
                              , FuncName);
               SUMA_RETURN(NOPE);
            }
         } else if (ngr) {
            /* write out the element */
            if (NI_write_element( ns , ngr ,
                                  NI_TEXT_MODE | NI_HEADERSHARP_FLAG ) < 0) {
               fprintf (stderr,"Error  %s:\nFailed in NI_write_element"
                              , FuncName);
               SUMA_RETURN(NOPE);
            }
         }

         /* close the stream */
         NI_stream_close( ns ) ;
      }
      #endif

      if (cs->nelps > 0) { /* make sure that you are not sending 
                              elements too fast */
         if (!etm) {
            etm = 100000.0; /* first pass, an eternity */
            if (LocalHead) 
               fprintf (SUMA_STDOUT,"%s: Initializing timer\n", FuncName);
            SUMA_etime(&tt, 0);
         }
         else {
            if (LocalHead) 
               fprintf (SUMA_STDOUT,"%s: Calculating etm\n", FuncName);
            etm = SUMA_etime(&tt, 1);
         }
         wtm = 1./cs->nelps - etm;
         if (wtm > 0) { /* wait */
            if (LocalHead) 
               fprintf (SUMA_STDOUT, 
                        "%s: Sleeping by %f to meet refresh rate...", 
                        FuncName, wtm);
            NI_sleep((int)(wtm*1000));
         }
      }

      /* send it to SUMA */
      if (LocalHead) 
         fprintf (SUMA_STDOUT,"Sending element %d comm_NI_mode = %d...\n", 
                              cs->TrackID, cs->comm_NI_mode);
      if (nel) {
         if (NI_write_element(   SUMAg_CF->ns_v[cs->istream] , nel, 
                                 cs->comm_NI_mode ) < 0) {
            SUMA_LH("Failed updating SUMA...");
         }
      } else if (ngr) {
         if (NI_write_element(   SUMAg_CF->ns_v[cs->istream] , ngr, 
                                 cs->comm_NI_mode ) < 0) {
            SUMA_LH("Failed updating SUMA...");
         }
      }
      if (LocalHead) {
         if (cs->nelps > 0) 
            fprintf (SUMA_STDOUT,
                     "        element %d sent (%f sec)\n", 
                     cs->TrackID, SUMA_etime(&tt, 1));
         else fprintf (SUMA_STDOUT,"        element %d sent \n", cs->TrackID);
      }
      if (nel && nel != data) NI_free_element(nel) ; nel = NULL;
      if (ngr && ngr != data) NI_free_element(ngr) ; ngr = NULL;
      
      if (cs->nelps > 0) {
         if (LocalHead) 
            fprintf (SUMA_STDOUT,"%s: Resetting time...\n", FuncName);
         SUMA_etime(&tt, 0); /* start the timer */
      }
      ++i_in;
      SUMA_RETURN(YUP);
   }/* action == 1 */
   
   if (action == 2) {
      if (i_in < 2) {
         SUMA_SL_Err("You must call SUMA_SendToSuma with action 0 and 1 before action 2.\nNo Communcation cleanup done.");
         cs->Send = NOPE;
         SUMA_RETURN(NOPE);
      }
      /* reset static variables */
         i_in = 0;
         etm = 0.0;
         
      SUMA_SEND_TO_SUMA_FUNC_CLEANUP;      
      
      /* now close the stream*/
      if (cs->Send && !cs->GoneBad) { 
         SUMA_LH("Cleanup of nel producing functions...");
         /* stop tracking */
         nel = NI_new_data_element("StopTracking", 0);
         NI_set_attribute(nel,"ni_stream_name",  SUMAg_CF->NimlStream_v[cs->istream]);

         if (NI_write_element( SUMAg_CF->ns_v[cs->istream] , nel, cs->comm_NI_mode ) < 0) {
            SUMA_SL_Err("Failed to stop tracking.\nContinuing...");
         } 
         if (nel) NI_free_element(nel); nel = NULL;

         /* tell suma you're done with that stream */
         nel = NI_new_data_element("CloseKillStream",0);
         if (!nel) {
            SUMA_SL_Err("Failed to create nel");
            exit(1);
         }

         NI_set_attribute (nel, "ni_stream_name",  SUMAg_CF->NimlStream_v[cs->istream]);
         if (NI_write_element( SUMAg_CF->ns_v[cs->istream] , nel, cs->comm_NI_mode ) < 0) {
                        SUMA_LH("Failed updating SUMA...");
         }
         if (nel) NI_free_element(nel) ; nel = NULL;


         /* now wait till stream goes bad */
         SUMA_Wait_Till_Stream_Goes_Bad(cs, 1000, 5000, 1);
          
         NI_stream_close(SUMAg_CF->ns_v[cs->istream]);
         SUMAg_CF->ns_v[cs->istream] = NULL;
         SUMAg_CF->ns_flags_v[cs->istream] = 0;
         SUMAg_CF->TrackingId_v[cs->istream] = 0;
         cs->Send = NOPE;
         cs->GoneBad = NOPE;
         cs->nelps = -1.0;
         cs->TrackID = 0;
         cs->istream = -1;
         
         
         
      }
   
      SUMA_RETURN(YUP);
   }

   /* should not get here */
   SUMA_SL_Err("Flow error.\nThis should not be");
   SUMA_RETURN(NOPE);
}

/*!
   \brief Function to handle send data elements to AFNI
   \param cs (SUMA_COMM_STRUCT *) Communication structure. (initialized when action is 0)
   \param data (void *) pointer to data that gets typecast as an afni dset
   \param action (int)  2: Make cleanup call to functions producing nel out of data
                           Close stream
                        1: Create a nel out of data and send to AFNI
                        0: start connection with AFNI 
                           initialize cs
                           prepare functions producing
                           nels out of data
   \return errflag (SUMA_Boolean) YUP: All is OK (although connection might get closed)
                                  NOPE: Some'in bad a happening.
                                  Connections getting closed in the midst of things are
                                  not considered as errors because they should not halt 
                                  the execution of the main program
                                    
*/
SUMA_Boolean SUMA_SendToAfni (SUMA_COMM_STRUCT *cs, void *data, int action)
{
   static char FuncName[]={"SUMA_SendToAfni"};
   static float etm = 0.0;
   static int i_in = 0;
   char stmp[500];
   static struct  timeval tt;
   NI_element *nel=NULL;
   float *f=NULL;
   int n=-1, WaitClose, WaitMax, *ip = NULL;
   float wtm;
   SUMA_Boolean good = YUP;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   

   if (action == 0) { /* initialization of connection */
      
      SUMA_LH("Setting up for communication with AFNI ...");
      cs->afni_Send = YUP;
      if(!SUMA_Assign_HostName (SUMAg_CF, cs->afni_host_name, cs->afni_istream)) {
		   fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Assign_HostName", FuncName);
		   exit (1);
	   }
      if (!SUMA_niml_call (SUMAg_CF, cs->afni_istream, NOPE)) {
         SUMA_SL_Err("Failed in SUMA_niml_call");
         /* connection flag is reset in SUMA_niml_call */
         cs->afni_Send = NOPE;
         SUMA_RETURN(NOPE);
      }
      
      /* no tracking for talking to AFNI */
      
      /* here is where you would start the workprocess for this program
      But since communication is one way, then forget about it */
      ++i_in;
      SUMA_RETURN(YUP);
   }
   
   if (action == 1) { /* action == 1,  send data mode */
      if (!i_in) {
         SUMA_SL_Err("You must call SUMA_SendToAfni with action 0 before action 1.\nNo Communcation cleanup done.");
         cs->afni_Send = NOPE;
         SUMA_RETURN(NOPE);
      }
      
      SUMA_LH("Creating nel and sending it");
      
      /* make sure stream is till OK */
      if (NI_stream_goodcheck ( SUMAg_CF->ns_v[cs->afni_istream] , 1 ) < 0) {
         cs->afni_GoneBad = YUP;
         SUMA_SL_Warn("Communication stream with afni gone bad.\nShutting down communication.");
         cs->afni_Send = NOPE;
         SUMA_RETURN(YUP); /* returning without error since program should continue */
      }

      if (!SUMA_SendDset_Afni( SUMAg_CF->ns_v[cs->afni_istream], (THD_3dim_dataset *)data, 1)) {
         SUMA_SL_Err("Failed to send dset");
         cs->afni_Send = NOPE;
         SUMA_RETURN(NOPE);
      }
      
      ++i_in;
      SUMA_RETURN(YUP);
   }/* action == 1 */
   
   if (action == 2) {
      if (i_in < 2) {
         SUMA_SL_Err("You must call SUMA_SendToAfni with action 0 and 1 before action 2.\nNo Communcation cleanup done.");
         cs->afni_Send = NOPE;
         SUMA_RETURN(NOPE);
      }
      /* reset static variables */
         i_in = 0;
         etm = 0.0;
         
      /* now close the stream*/
      if (cs->afni_Send && !cs->afni_GoneBad) { 
         SUMA_LH("Cleanup of nel producing functions...");
      
         NI_stream_close(SUMAg_CF->ns_v[cs->afni_istream]);
         SUMAg_CF->ns_v[cs->afni_istream] = NULL;
         SUMAg_CF->ns_flags_v[cs->afni_istream] = 0;
         SUMAg_CF->TrackingId_v[cs->afni_istream] = 0;
         cs->afni_Send = NOPE;
         cs->afni_GoneBad = NOPE;
         cs->afni_istream = -1;
      }
   
      SUMA_RETURN(YUP);
   }

   /* should not get here */
   SUMA_SL_Err("Flow error.\nThis should not be");
   SUMA_RETURN(NOPE);
}

SUMA_Boolean SUMA_SendDset_Afni( NI_stream ns, THD_3dim_dataset *dset, int all)
{
   static char FuncName[]={"SUMA_SendDset_Afni"};
   NI_group *ngr = NULL;
   NI_element *nel = NULL;
   int iv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset) {
      SUMA_SL_Warn("NULL dset, nothing to do");
      SUMA_RETURN(YUP);
   }
   
   if (all == 1) {
      SUMA_LH("Sending all dset at once");
      ngr = THD_dataset_to_niml( dset ) ;
      NI_set_attribute( ngr , "AFNI_prefix" , DSET_PREFIX(dset) ) ;
      NI_write_element(ns, ngr, NI_BINARY_MODE);
      NI_free_element(ngr); ngr = NULL;
      SUMA_LH("Done.");
   } else {
      SUMA_SL_Warn("Sending one sub-brick at a time NOT TESTED IN SUMA YET");
      ngr = THD_nimlize_dsetatr( dset ) ;   /* header only */
      NI_set_attribute( ngr , "AFNI_prefix" , DSET_PREFIX(dset) ) ;
      NI_write_procins( ns , "keep_reading" ) ;
      NI_write_element( ns, ngr, NI_BINARY_MODE ) ;
      NI_free_element( ngr ) ; ngr = NULL;
      for( iv=0 ; iv < DSET_NVALS(dset) ; iv++ ){
         nel = THD_subbrick_to_niml( dset , iv , SBFLAG_INDEX ) ;
         NI_write_element( ns , nel , NI_BINARY_MODE ) ;
         NI_free_element(nel) ; nel = NULL;
      }
      NI_write_procins( ns , "pause_reading" ) ; /* not necessary but tidy */
   }
   
   
   SUMA_RETURN(YUP);
}
