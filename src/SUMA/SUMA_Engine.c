#define DEBUG_1
#ifdef DEBUG_1
   #define DEBUG_2
   #define DEBUG_3
#endif
   
/* Header FILES */
   
#include "SUMA_suma.h"

/* extern SUMA_SurfaceViewer *SUMAg_cSV; */   /* no longer used Tue Aug 13 16:07:41 EDT 2002 */
extern SUMA_DO *SUMAg_DOv;   
extern int SUMAg_N_DOv; 
extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv;

/*!
This is the function that runs the viewers. 
It acts on the viewer that sv points to
To add a new command:
include it SUMA_define.h in SUMA_ENGINE_CODE's typedef
include it in SUMA_ParseCommands.c, SUMA_CommandCode function 
*/

SUMA_Boolean SUMA_Engine (char *Command, SUMA_EngineData *EngineData, SUMA_SurfaceViewer *sv)
{
   char NextCom[SUMA_MAX_COMMAND_LENGTH], tmpcom[SUMA_MAX_COMMAND_LENGTH], ssource[100], sfield[100], sdestination[100];
   static char FuncName[]={"SUMA_Engine"};
   int NextComCode, ii, i, id, ND, ip, NP;
   SUMA_SurfaceObject *SO;
   float delta_t;
   struct  timeval tt;
   int it, Wait_tot, nn=0, N_SOlist, SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS];
   float ft, **fm, fv15[15];
   XtPointer elvis=NULL;
   NI_element *nel;
   SUMA_Boolean Found, LocalHead = NOPE;
   SUMA_SurfaceViewer *svi;
   
   /*int iv3[3], iv15[15], **im;
   float fv3[3];
   char s[SUMA_MAX_STRING_LENGTH];*/ /* keep standard unused variables undeclared, else compiler complains*/
   
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   NextComCode = SUMA_GetNextCommand (Command, SUMA_COMMAND_DELIMITER, SUMA_COMMAND_TERMINATOR, NextCom);
   if (!NextComCode) {
      fprintf (stderr, "%s Error: executing SUMA_GetNextCommand\n", FuncName);
      SUMA_RETURN (NOPE);
   } 
   
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: ", FuncName);
   while (NextComCode) {/* cycle through NextComs */
      if (LocalHead) fprintf (SUMA_STDOUT,"->%s<-\t", NextCom);
      switch (NextComCode) {/* switch NextComCode */
         case SE_SetLookAt:
            /* expects a center XYZ in EngineData->fv3[0 .. 2] */
            /* Double check on Data destination */
            if (EngineData->fv3_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            /* save the calling source */
            SUMA_EngineSourceString (ssource, EngineData->fv15_Dest);

            /* calculate the transform required to bring the new look at location to the current one */
            {
               float ulook_old[3], ulook_new[3];
               int Step = 10, iStep;
               float fracUp, fracDown;
              
               ulook_old[0] = sv->GVS[sv->StdView].ViewFrom[0] - sv->GVS[sv->StdView].ViewCenter[0];
               ulook_old[1] = sv->GVS[sv->StdView].ViewFrom[1] - sv->GVS[sv->StdView].ViewCenter[1];
               ulook_old[2] = sv->GVS[sv->StdView].ViewFrom[2] - sv->GVS[sv->StdView].ViewCenter[2];
               ulook_new[0] = ulook_new[1] = ulook_new[2] = 0.0;
               fm = (float **)SUMA_allocate2D(4,4,sizeof(float));
               
               for (iStep = 1; iStep <= Step; ++iStep) {
                  fprintf (SUMA_STDERR,"%d\n", iStep);
                  fracUp = (float)(iStep)/(float)Step;
                  fracDown = (float)(Step - iStep)/(float)Step;
                  ulook_new[0] = (EngineData->fv3[0] * fracUp + sv->GVS[sv->StdView].ViewFrom[0] * fracDown) \
                                 - sv->GVS[sv->StdView].ViewCenter[0];
                  ulook_new[1] = (EngineData->fv3[1] * fracUp + sv->GVS[sv->StdView].ViewFrom[1] * fracDown) \
                                 - sv->GVS[sv->StdView].ViewCenter[1];
                  ulook_new[2] = (EngineData->fv3[2] * fracUp + sv->GVS[sv->StdView].ViewFrom[2] * fracDown) \
                                 - sv->GVS[sv->StdView].ViewCenter[2];
                  if (fm == NULL) {
                     fprintf (SUMA_STDERR,"Error %s: Failed to allocate fm.\n",FuncName);
                     break;
                  }
                  if (!SUMA_FromToRotation (ulook_new, ulook_old, fm)) {
                     fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_FromToRotation.\n",FuncName);
                     break;
                  }
                  /* Register m with EngineData and send it to SetRotMatrix */
                  sprintf(sfield,"fm");
                  sprintf(sdestination,"SetRotMatrix");
                  EngineData->N_cols = 4;
                  EngineData->N_rows = 4;
                  if (!SUMA_RegisterEngineData (EngineData, sfield, (void *)fm, sdestination, ssource, NOPE)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
                     break;
                  }
                  /* register a call to SetRotMatrix */
                  #if 0
                     /* you can simply add the command to the queue here, you must force SUMA to execute */
                     sprintf(tmpcom,"Redisplay|SetRotMatrix");
                     SUMA_RegisterCommand (Command, SUMA_COMMAND_DELIMITER, SUMA_COMMAND_TERMINATOR, tmpcom, NOPE);
                  #else
                     /* you want to set the Rotation Matrix but not you redisplay call should be to ReidsplayNow
                     If you use Redisplay instead, you will not see the motion because all the calls will be rendered
                     once */
                     sprintf(tmpcom,"RedisplayNow|SetRotMatrix~");
                     if (!SUMA_Engine (tmpcom, EngineData, sv)) {
                        fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
                     }  
                     /* free fm for next call */         
                     if (!SUMA_ReleaseEngineData (EngineData, sdestination)) {
                        fprintf(SUMA_STDERR,"Error %s: Failed to Release EngineData \n", FuncName);
                     }
                  #endif
               }
               SUMA_free2D((char **)fm, 4);
            }
            break;
         
         case SE_ToggleConnected:
               if (!SUMA_CanTalkToAfni (SUMAg_DOv, SUMAg_N_DOv)) {
                  fprintf(SUMA_STDOUT,"%s: Cannot connect to AFNI.\n\tNot one of the surfaces is mappable and has a Surface Volume.\n\tDid you use the -sv option when launching SUMA ?\n", FuncName);
                  break;
               }
               SUMAg_CF->Connected = !SUMAg_CF->Connected;
               if (SUMAg_CF->Connected) {
                 /* find out if the stream has been established already */
                  if (SUMAg_CF->ns) { /* stream is open, nothing to do */
                     if (LocalHead) fprintf(SUMA_STDOUT,"%s: Stream existed, reusing.\n", FuncName);
                     fprintf(SUMA_STDOUT,"%s: Connected to AFNI.\n", FuncName);
                  }else {   /* must open stream */              
                     /* contact afni */
                        fprintf(SUMA_STDOUT,"%s: Contacting afni ...\n", FuncName);
                        SUMAg_CF->ns = NI_stream_open( SUMAg_CF->NimlAfniStream , "w" ) ;
                        fprintf (SUMA_STDERR, "%s: Trying shared memory...\n", FuncName);
                        if( strstr( SUMAg_CF->NimlAfniStream , "tcp:localhost:" ) != NULL ) {
                           if (!NI_stream_reopen( SUMAg_CF->ns , "shm:WeLikeElvis:1M" )) {
                              fprintf (SUMA_STDERR, "Warning %s: Shared memory communcation failed.\n", FuncName);
                           }
                        }
                        /*   SUMAg_CF->ns = NI_stream_open( "tcp:128.231.212.194:53211" , "w" ) ;*/

                     if( SUMAg_CF->ns == NULL ){
                        fprintf(SUMA_STDERR,"Error %s: NI_stream_open failed\n", FuncName) ; 
                        SUMAg_CF->Connected = !SUMAg_CF->Connected;
                        break ;
                     }

                     Wait_tot = 0;
                     while(Wait_tot < SUMA_WriteCheckWaitMax){
                       nn = NI_stream_writecheck( SUMAg_CF->ns , SUMA_WriteCheckWait) ;
                       if( nn == 1 ){ fprintf(stderr,"\n") ; break ; }
                       if( nn <  0 ){ fprintf(stderr,"BAD\n"); SUMAg_CF->Connected = !SUMAg_CF->Connected; SUMAg_CF->ns = NULL; break;}
                       Wait_tot += SUMA_WriteCheckWait;
                       fprintf(SUMA_STDERR,".") ;
                     }

                     /* make sure you did not exit because of time out */
                     if (nn!=1) {
                        SUMAg_CF->Connected = !SUMAg_CF->Connected;
                        SUMAg_CF->ns = NULL;
                        fprintf(SUMA_STDERR,"Error %s: WriteCheck timed out (> %d ms).\n", FuncName, SUMA_WriteCheckWaitMax);
                        break ;
                     }
                  } 
                  
                  /* Stream is open */
                  
                  /* start the listening WorkProcess */
                  SUMA_register_workproc(SUMA_niml_workproc, (XtPointer)sv);

                  /* register a call for sending the surface to afni (SetAfniSurf)*/
                  if (LocalHead) fprintf(SUMA_STDERR,"Notifying Afni of New surface...\n");
                  sprintf(tmpcom,"SetAfniSurf");
                  SUMA_RegisterCommand (Command, SUMA_COMMAND_DELIMITER, SUMA_COMMAND_TERMINATOR, tmpcom, NOPE);/* form surface nel */
                  break;
               } else {
                  fprintf(SUMA_STDOUT,"%s: Disconnecting from afni.\n", FuncName);
                  /* remove the listening workprocess) */
                  SUMA_remove_workproc( SUMA_niml_workproc );
                   
                  if (!SUMAg_CF->ns) {
                     /* It looks like the stream was closed, do the clean up */
                     fprintf(SUMA_STDERR,"Warning %s: sv->ns is null, stream must have gotten closed. Cleaning up ...\n", FuncName);
                     sprintf(tmpcom,"CloseStream4All");
                     SUMA_RegisterCommand (Command, SUMA_COMMAND_DELIMITER, SUMA_COMMAND_TERMINATOR, tmpcom, NOPE);
                     break;
                  }
                  
                 
                  /* Close the stream if nobody else wants it. 
                  This is not a great condition, one should be able to leave the stream open 
                  even if no viewer, for the moment, does not want to talk to AFNI.
                  Perhaps in the future. */
                  if (SUMAg_N_SVv == 1) {
                     fprintf(SUMA_STDERR,"%s: Nobody wants to talk to AFNI anymore, closing stream ...\n", FuncName);
                     NI_stream_close(SUMAg_CF->ns);
                     SUMAg_CF->ns = NULL;
                  }
                  break;
               }
   
         case SE_CloseStream4All:
            /* odds are AFNI died or closed stream, mark all surfaces as unsent */
            for (ii=0; ii<SUMAg_N_DOv; ++ii) {
               if (SUMA_isSO(SUMAg_DOv[ii])) {
                  SO = (SUMA_SurfaceObject *)(SUMAg_DOv[ii].OP);
                  if (SO->SentToAfni) SO->SentToAfni = NOPE;
               }
            }
            
            /* same for parent fields */
            /* check first if stream in SUMAg_CF still good by any chance */
            nn = NI_stream_goodcheck(SUMAg_CF->ns , 1 ) ;
            
            if( nn >= 0 ){ 
               fprintf(stderr,"Error %s: Stream still alive, this should not be. Closing anyway.\n", FuncName); 
               NI_stream_close(SUMAg_CF->ns); 
            }
            
            /* clean up and get out of here*/         
            SUMAg_CF->ns = NULL;
            break;
            
         case SE_SetForceAfniSurf:
            /* send to afni surfaces that can be sent even if they have been sent already */
            for (ii=0; ii<sv->N_DO; ++ii) {
               if (SUMA_isSO(SUMAg_DOv[sv->ShowDO[ii]])) {
                  SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->ShowDO[ii]].OP);
                  if (SO->SentToAfni) SO->SentToAfni = NOPE;
               }
            }
            
            /* proceed to SE_SetAfniSurf: */
            sprintf(tmpcom,"SetAfniSurf");
            SUMA_RegisterCommand (Command, SUMA_COMMAND_DELIMITER, SUMA_COMMAND_TERMINATOR, tmpcom, NOPE);/* form surface nel */
            break;
            
         case SE_SetAfniSurf:
            { int loc_ID;
            
            /* send to afni the list of inherently mappable surfaces and with a surface volume*/
            /* No surfaces are sent twice because there should not be duplicate 
            inherently mappable surfaces in SUMAg_DOv */
            /* prior to Wed Nov  6 17:47:20 EST 2002, only mappable surfaces that are related to the ones shown in the viewer
            were being sent to AFNI. Now all mappable surfaces loaded are sent regardless of what is shown */
            
            for (ii=0; ii<SUMAg_N_DOv; ++ii) {
               if (SUMA_isSO(SUMAg_DOv[ii])) {
                  SO = (SUMA_SurfaceObject *)(SUMAg_DOv[ii].OP);
                  if (!SUMA_isINHmappable(SO)) {
                     continue;
                  }
                  /* if this surface has been sent to AFNI before, bypass it */
                  if (SO->SentToAfni) {
                     if (LocalHead) fprintf(SUMA_STDERR, "Warning %s: Surface %s has been sent to AFNI before.\n", \
                        FuncName, SO->idcode_str);
                     continue;
                  }else {
                     if (LocalHead) fprintf(SUMA_STDERR, "Warning %s: Surface %s Will be sent to AFNI.\n", \
                        FuncName, SO->idcode_str);
                  }
                  nel = SUMA_makeNI_SurfIXYZ (SO);
                  if (!nel) {
                     fprintf(SUMA_STDERR,"Error %s: SUMA_makeNI_SurfIXYZ failed\n", FuncName);
                     break;
                  }
                  /* send surface nel */
                  fprintf(SUMA_STDERR,"%s: Sending SURF_iXYZ nel...\n ", FuncName) ;
                  nn = NI_write_element( SUMAg_CF->ns , nel , NI_BINARY_MODE ) ;

                  if( nn < 0 ){
                       fprintf(SUMA_STDERR,"Error %s: NI_write_element failed\n", FuncName);
                  }

                  #if 0
                     {
                        NI_stream nstdout;
                         nstdout = NI_stream_open( "fd:1","w");
                          if( nstdout == NULL ){ fprintf(SUMA_STDERR,"Can't open fd:1\n"); break; }
                           NI_write_element( nstdout , nel , NI_TEXT_MODE ) ;
                         NI_stream_close(nstdout);
                     }
                  #endif

                  NI_free_element(nel);

                  /* send triangles */
                  nel = SUMA_makeNI_SurfIJK (SO);
                  if (!nel) {
                     fprintf(SUMA_STDERR,"Error %s: SUMA_makeNI_SurfIJK failed\n", FuncName);
                     break;
                  }
                  /* send surface nel */
                  fprintf(SUMA_STDERR,"%s: Sending SURF_IJK nel ...\n", FuncName) ;
                  nn = NI_write_element( SUMAg_CF->ns , nel , NI_BINARY_MODE ) ;

                  if( nn < 0 ){
                       fprintf(SUMA_STDERR,"Error %s: NI_write_element failed\n", FuncName);
                  }
                  NI_free_element(nel);
                  nel = NULL;

                  /* mark surface as sent to afni */
                  SO->SentToAfni = YUP;
               }
            }
            break;
            }
         case SE_ToggleShowSelectedNode:
            for (ii=0; ii<sv->N_DO; ++ii) {
               if (SUMA_isSO(SUMAg_DOv[sv->ShowDO[ii]])) {
                  SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->ShowDO[ii]].OP);
                  SO->ShowSelectedNode = !SO->ShowSelectedNode;
                  fprintf(SUMA_STDOUT,"SO->ShowSelectedNode = %d\n", SO->ShowSelectedNode);
               }
            }
            break;
         
         case SE_SetSelectedNode:
            /* expects a node index in i */
            if (EngineData->i_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Focus_SO_ID].OP);
            SO->SelectedNode = EngineData->i;
            break;
            
         case SE_ToggleShowSelectedFaceSet:
            for (ii=0; ii<sv->N_DO; ++ii) {
               if (SUMA_isSO(SUMAg_DOv[sv->ShowDO[ii]])) {
                  SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->ShowDO[ii]].OP);
                  SO->ShowSelectedFaceSet = !SO->ShowSelectedFaceSet;
                  fprintf(SUMA_STDOUT,"SO->ShowSelectedFaceSet = %d\n", \
                     SO->ShowSelectedFaceSet);
               }
            }
            break;
         
         case SE_SetSelectedFaceSet:
            /* expects the index for the selected FaceSet */
            if (EngineData->i_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Focus_SO_ID].OP);
            ND = SO->NodeDim;
            NP = SO->FaceSetDim;
            ip = NP * EngineData->i;
            id = ND * SO->FaceSetList[ip];
            SO->FaceSetMarker->n0[0] = SO->NodeList[id];
            SO->FaceSetMarker->n0[1] = SO->NodeList[id+1];
            SO->FaceSetMarker->n0[2] = SO->NodeList[id+2];
            id = ND * SO->FaceSetList[ip+1];
            SO->FaceSetMarker->n1[0] = SO->NodeList[id];
            SO->FaceSetMarker->n1[1] = SO->NodeList[id+1];
            SO->FaceSetMarker->n1[2] = SO->NodeList[id+2];
            id = ND * SO->FaceSetList[ip+2];
            SO->FaceSetMarker->n2[0] = SO->NodeList[id];
            SO->FaceSetMarker->n2[1] = SO->NodeList[id+1];
            SO->FaceSetMarker->n2[2] = SO->NodeList[id+2];
            SO->FaceSetMarker->NormVect[0] = SO->FaceNormList[ip];
            SO->FaceSetMarker->NormVect[1] = SO->FaceNormList[ip+1];
            SO->FaceSetMarker->NormVect[2] = SO->FaceNormList[ip+2];
            
            SO->SelectedFaceSet = EngineData->i;
            break;
            
         case SE_ToggleCrossHair:
            sv->ShowCrossHair = !sv->ShowCrossHair;
            break;
            
         case SE_SetCrossHair:
            /* Expects Cross Hair coordinates in fv3 */
            if (EngineData->fv3_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            sv->Ch->c[0] = EngineData->fv3[0]; sv->Ch->c[1]= EngineData->fv3[1]; sv->Ch->c[2]= EngineData->fv3[2];
            break;
         
         case SE_BindCrossHair:
            /* expects SurfaceID to bind cross hair to*/
            if (EngineData->iv3_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            sv->Ch->SurfaceID = EngineData->iv3[0];
            sv->Ch->NodeID = EngineData->iv3[1];
            
            break;
         
         case SE_LockCrossHair:
            /* calls other viewers and determine if the cross hair needs to be locked to the calling sv */

            /* check to see if other viewers need to share the fate */
            ii = SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv);
            if (ii < 0) {
               fprintf (SUMA_STDERR,"Error %s: Failed to find index of sv.\n", FuncName);
               break;
            }
            if (SUMAg_CF->Locked[ii]) { /* This one's locked, find out which other viewers are locked to this one */
               for (i=0; i < SUMAg_N_SVv; ++i) {
                  svi = &SUMAg_SVv[i];
                  if (i != ii) {
                     switch (SUMAg_CF->Locked[ii]) { 
                        case SUMA_No_Lock:
                           break;
                        case SUMA_XYZ_Lock:
                           /* just set the XYZ, and free the binding to the surfaces */
                           svi->Ch->c[0] = sv->Ch->c[0];
                           svi->Ch->c[1] = sv->Ch->c[1];
                           svi->Ch->c[2] = sv->Ch->c[2];
                           svi->Ch->NodeID = -1;
                           svi->Ch->SurfaceID = -1;
                           /* register a redisplay */
                           svi->ResetGLStateVariables = YUP;
                           SUMA_postRedisplay(svi->X->GLXAREA, NULL, NULL);                           
                           break;
                        case SUMA_I_Lock: 
                           {
                              SUMA_SurfaceObject *SO1 = NULL, *SO2 = NULL;
                              
                              /* determine the list of shown surfaces */
                              N_SOlist = SUMA_ShownSOs(svi, SUMAg_DOv, SOlist);

                              /* first find the surface that the cross hair is bound to */
                              if (sv->Ch->SurfaceID < 0) {
                                 fprintf (SUMA_STDERR, "%s: Cannot link from this viewer's cross hair. No bound surface.\n", FuncName);
                                 break;
                              }
                              if (sv->Ch->NodeID < 0) {
                                 fprintf (SUMA_STDERR, "%s: Cannot link from this viewer's cross hair. No NodeID.\n", FuncName);
                                 break;
                              }
                              SO1 = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Ch->SurfaceID].OP;
                              Found = NOPE;
                              it = 0;
                              while (it < N_SOlist && !Found) {
                                 SO2 = (SUMA_SurfaceObject *)SUMAg_DOv[SOlist[it]].OP;
                                 if (SUMA_isRelated (SO1, SO2)) {
                                    svi->Ch->SurfaceID = SOlist[it];
                                    if (sv->Ch->NodeID > SO2->N_Node) {
                                       fprintf (SUMA_STDERR,"Error %s: NodeID is larger than N_Node. Setting NodeID to 0.\n", FuncName);
                                       svi->Ch->NodeID = 0;
                                    }else{
                                       svi->Ch->NodeID = sv->Ch->NodeID;
                                    }
                                    /* set the XYZ */
                                    svi->Ch->c[0] = SO2->NodeList[SO2->NodeDim*svi->Ch->NodeID];
                                    svi->Ch->c[1] = SO2->NodeList[SO2->NodeDim*svi->Ch->NodeID+1];
                                    svi->Ch->c[2] = SO2->NodeList[SO2->NodeDim*svi->Ch->NodeID+2];
                                    Found = YUP;
                                 }
                                 ++it;
                              }
                              if (!Found) {
                                 fprintf (SUMA_STDERR,"%s: No related surfaces found in viewer, cross hair will not be touched .\n", FuncName);
                                 break;
                              } else {
                                 /* FORCE a redisplay */
                                 svi->ResetGLStateVariables = YUP;
                                 SUMA_handleRedisplay((XtPointer)svi->X->GLXAREA);
                              }
                              
                           }
                           break;
                        default:
                           fprintf(SUMA_STDERR,"Error %s: Lock type (%d) undefined.\n", FuncName, SUMAg_CF->Locked[ii]);
                           break;
                     }
                  }
               }
            }else{
               /* not locked to anything */
            }
            break;
                        
         case SE_SetAfniCrossHair:
            /* sends the current cross hair to afni */
            /* form nel */
            nel = SUMA_makeNI_CrossHair (sv);
            if (!nel) {
               fprintf(SUMA_STDERR,"Error %s: SUMA_makeNI_SurfIXYZ failed\n", FuncName);
               break;
               }
            /*send it to afni */
            /*fprintf(SUMA_STDERR,"Sending cross hair nel ") ;*/
            nn = NI_write_element( SUMAg_CF->ns , nel , NI_TEXT_MODE ) ;
            /*SUMA_nel_stdout (nel);*/
      
            if( nn < 0 ){
                   fprintf(SUMA_STDERR,"Error %s: NI_write_element failed\n", FuncName);
            }
            
            NI_free_element(nel);

            break;
                  
         case SE_SetLookAtNode:
            /* expects a center XYZ in EngineData->fv15[0 .. 2]
            expects a normal vector in EngineData->fv15[3 .. 5] */
            if (EngineData->fv15_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            
            { float CurrentDistance;
              float **fm2_3;
              
            /* modify the ViewFrom Value such that the viewing distance remains the same */
            CurrentDistance = sqrt((sv->GVS[sv->StdView].ViewFrom[0]-sv->GVS[sv->StdView].ViewCenter[0])*(sv->GVS[sv->StdView].ViewFrom[0]-sv->GVS[sv->StdView].ViewCenter[0]) +\
                                    (sv->GVS[sv->StdView].ViewFrom[1]-sv->GVS[sv->StdView].ViewCenter[1])*(sv->GVS[sv->StdView].ViewFrom[1]-sv->GVS[sv->StdView].ViewCenter[1]) +\
                                    (sv->GVS[sv->StdView].ViewFrom[2]-sv->GVS[sv->StdView].ViewCenter[2])*(sv->GVS[sv->StdView].ViewFrom[2]-sv->GVS[sv->StdView].ViewCenter[2]));
            
            /* set the ViewCenter Value to that of the node's XYZ*/
            sv->GVS[sv->StdView].ViewCenter[0] = EngineData->fv15[0];
            sv->GVS[sv->StdView].ViewCenter[1] = EngineData->fv15[1]; 
            sv->GVS[sv->StdView].ViewCenter[2] = EngineData->fv15[2];
            
            /* obtain the LookFrom point based on CurrentDistance and the normal vector */
            fm2_3 = SUMA_Point_At_Distance(&(EngineData->fv15[3]), sv->GVS[sv->StdView].ViewCenter, CurrentDistance);
            if (fm2_3 == NULL) {
               fprintf(SUMA_STDOUT,"Error %s: SUMA_Point_At_Distance failed.\n", FuncName);
               break;
            }
            fprintf(SUMA_STDOUT,"\nPoints: %f %f %f\n%f %f %f\n", \
               fm2_3[0][0], fm2_3[0][1], fm2_3[0][2], \
               fm2_3[1][0], fm2_3[1][1], fm2_3[1][2]);
            
            sv->GVS[sv->StdView].ViewFrom[0] = fm2_3[0][0]; 
            sv->GVS[sv->StdView].ViewFrom[1] = fm2_3[0][1]; 
            sv->GVS[sv->StdView].ViewFrom[2] = fm2_3[0][2]; 
            
            /* fm2_3 not needed anymore */
            SUMA_free2D((char **)fm2_3, 2);
            
            gluLookAt (sv->GVS[sv->StdView].ViewFrom[0], sv->GVS[sv->StdView].ViewFrom[1], sv->GVS[sv->StdView].ViewFrom[2], sv->GVS[sv->StdView].ViewCenter[0], sv->GVS[sv->StdView].ViewCenter[1], sv->GVS[sv->StdView].ViewCenter[2], sv->GVS[sv->StdView].ViewCamUp[0], sv->GVS[sv->StdView].ViewCamUp[1], sv->GVS[sv->StdView].ViewCamUp[2]);
            }
            
            break;
         case SE_SetLookFrom:
            /* expects a center XYZ in EngineData->fv3[0 .. 2] */
            if (EngineData->fv3_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            /* set the LookFrom option */
            sv->GVS[sv->StdView].ViewFrom[0] = EngineData->fv3[0];
            sv->GVS[sv->StdView].ViewFrom[1] = EngineData->fv3[1]; 
            sv->GVS[sv->StdView].ViewFrom[2] = EngineData->fv3[2];
            gluLookAt (sv->GVS[sv->StdView].ViewFrom[0], sv->GVS[sv->StdView].ViewFrom[1], sv->GVS[sv->StdView].ViewFrom[2], sv->GVS[sv->StdView].ViewCenter[0], sv->GVS[sv->StdView].ViewCenter[1], sv->GVS[sv->StdView].ViewCenter[2], sv->GVS[sv->StdView].ViewCamUp[0], sv->GVS[sv->StdView].ViewCamUp[1], sv->GVS[sv->StdView].ViewCamUp[2]);
            break;

         case SE_Redisplay_AllVisible:
            /* post a redisplay to all visible viewers */
            for (ii=0; ii<SUMAg_N_SVv; ++ii) {
               if (LocalHead) fprintf (SUMA_STDERR,"%s: Checking viewer %d.\n", FuncName, ii);
               if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
                  /* you must check for both conditions because by default 
                  all viewers are initialized to isShaded = NOPE, even before they are ever opened */
                  if (LocalHead) fprintf (SUMA_STDERR,"%s: Redisplaying viewer %d.\n", FuncName, ii);
                  SUMAg_SVv[ii].ResetGLStateVariables = YUP;
                  SUMA_postRedisplay(SUMAg_SVv[ii].X->GLXAREA, NULL, NULL);
               }
            }
            break;
            
         case SE_Redisplay:
            /*post a redisplay to one specific viewer*/
            if (LocalHead) fprintf (SUMA_STDOUT,"%s: Redisplay ...", FuncName);
            SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
            if (LocalHead) fprintf (SUMA_STDOUT," Done\n");
            break;
         
         case SE_RedisplayNow:
            /*call handle redisplay immediately to one specific viewer*/
            if (LocalHead) fprintf (SUMA_STDOUT,"%s: Redisplaying NOW ...", FuncName);
            SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
            if (LocalHead) fprintf (SUMA_STDOUT," Done\n");
            break;
            
         case SE_ResetOpenGLState:
            /* reset OPEN GL's state variables */
            /* expects the surface viewer pointer in vp */
            if (EngineData->vp_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            if (LocalHead) fprintf (SUMA_STDOUT,"%s: Resetting OpenGL state variables.\n", FuncName);
            
            /* No need to call SUMA_OpenGLStateReset, that is now done in SUMA_display */
            svi = (SUMA_SurfaceViewer *)EngineData->vp;
            svi->ResetGLStateVariables = YUP;
            break;
            
         case SE_ToggleForeground:
            /* Show/hide the foreground */
            sv->ShowForeground = !sv->ShowForeground;
            if (!sv->ShowForeground) {
               fprintf(SUMA_STDOUT,"%s: Foreground Colors Off.\n", FuncName);
            } else {
               fprintf(SUMA_STDOUT,"%s: Foreground Colors ON.\n", FuncName);
            }
            /* set the color remix flag */
            if (!SUMA_SetShownLocalRemixFlag (sv)) {
               fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SetShownLocalRemixFlag.\n", FuncName);
               break;
            }
            break;
         
         case SE_ToggleBackground:
            /* Show/hide the background */
            sv->ShowBackground = !sv->ShowBackground;
            if (!sv->ShowBackground) {
               fprintf(SUMA_STDOUT,"%s: Background Colors OFF.\n", FuncName);
            } else {
               fprintf(SUMA_STDOUT,"%s: Background Colors ON.\n", FuncName);
            }
            /* set the color remix flag */
            if (!SUMA_SetShownLocalRemixFlag (sv)) {
               fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SetShownLocalRemixFlag.\n", FuncName);
               break;
            }
            break;
                     
         case SE_Home:
            sv->GVS[sv->StdView].translateVec[0]=0; sv->GVS[sv->StdView].translateVec[1]=0;
            glMatrixMode(GL_PROJECTION);
            /* sv->FOV[sv->iState] = FOV_INITIAL;   *//* Now done in SE_FOVreset *//* reset the zooming */
            sv->GVS[sv->StdView].ViewFrom[0] = sv->GVS[sv->StdView].ViewFromOrig[0];
            sv->GVS[sv->StdView].ViewFrom[1] = sv->GVS[sv->StdView].ViewFromOrig[1];
            sv->GVS[sv->StdView].ViewFrom[2] = sv->GVS[sv->StdView].ViewFromOrig[2];
            sv->GVS[sv->StdView].ViewCenter[0] = sv->GVS[sv->StdView].ViewCenterOrig[0];
            sv->GVS[sv->StdView].ViewCenter[1] = sv->GVS[sv->StdView].ViewCenterOrig[1];
            sv->GVS[sv->StdView].ViewCenter[2] = sv->GVS[sv->StdView].ViewCenterOrig[2];
            
            glMatrixMode(GL_MODELVIEW);
            glLoadIdentity();
            gluLookAt (sv->GVS[sv->StdView].ViewFrom[0], sv->GVS[sv->StdView].ViewFrom[1], sv->GVS[sv->StdView].ViewFrom[2], sv->GVS[sv->StdView].ViewCenter[0], sv->GVS[sv->StdView].ViewCenter[1], sv->GVS[sv->StdView].ViewCenter[2], sv->GVS[sv->StdView].ViewCamUp[0], sv->GVS[sv->StdView].ViewCamUp[1], sv->GVS[sv->StdView].ViewCamUp[2]);
            break;
         
         case SE_FOVreset:
            sv->FOV[sv->iState] = FOV_INITIAL;   /* reset the zooming */
            break;
            
         case SE_SetNodeColor:
            /* expects a four-columned fm in EngineData->fm[0 .. N][0..3] 
            [Node Index] [R] [G] [B] RGB between 0 and 1*/
            if (EngineData->fm_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Focus_SO_ID].OP);
            {
               GLfloat *glar_ColorList;
               glar_ColorList = SUMA_GetColorList(sv, SO->idcode_str);
               if (!glar_ColorList) {
                  fprintf (SUMA_STDERR,"Error %s: NULL color list array. Trouble.\n", FuncName);
                  break;
               }
               for (i=0; i < EngineData->N_rows; ++i){
                  ii = (int)(EngineData->fm[i][0]);
                  glar_ColorList[4*ii] = EngineData->fm[i][1];
                  glar_ColorList[4*ii+1] = EngineData->fm[i][2];
                  glar_ColorList[4*ii+2] = EngineData->fm[i][3];
                  glar_ColorList[4*ii+3] = 0.5;
               }
            }
            break;
            
         case SE_FlipLight0Pos:
            sv->light0_position[2] *= -1;
            glLightfv(GL_LIGHT0, GL_POSITION, sv->light0_position);
            break;
         
         case SE_HighlightNodes:
            /* highlight nodes inside the search box */
            /* expects Node XYZ in EngineData->fv15[0..2]
            Box dimensions in EngineData->fv15[3..5] */
            if (EngineData->fv15_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            /* save the calling source */
            SUMA_EngineSourceString (ssource, EngineData->fv15_Dest);
            {
               SUMA_ISINBOX IB;
               
               SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Focus_SO_ID].OP);
               ND = SO->NodeDim;
            
               SUMA_etime (&tt, 0);
               IB = SUMA_isinbox (SO->NodeList, SO->N_Node, &(EngineData->fv15[0]), &(EngineData->fv15[3]),  YUP);
               delta_t = SUMA_etime (&tt, 1);
               fprintf (SUMA_STDOUT,"Elapsed time for isinbox operation: %f\n", delta_t);
               fprintf (SUMA_STDOUT,"\t%d nodes (out of %d) found in box\n",IB.nIsIn, SO->N_Node);
               
               if (IB.nIsIn) { /* found some, find the closest node */
                  /* locate the closest node and store it's id in EngineData*/
                  SUMA_MIN_LOC_VEC (IB.d, IB.nIsIn, ft, it);
                  
                  /* XYZ and normal of the closets to the center */
                  #ifdef STUFF
                     /* This is not being used and if it is to be used, EngineData should 
                     not be set manually */
                     id = ND * IB.IsIn[it];
                     EngineData->fv15[0] = SO->NodeList[id];
                     EngineData->fv15[1] = SO->NodeList[id+1];
                     EngineData->fv15[2] = SO->NodeList[id+2];
                     EngineData->fv15[3] = SO->NodeNormList[id];
                     EngineData->fv15[4] = SO->NodeNormList[id+1];
                     EngineData->fv15[5] = SO->NodeNormList[id+2];
                  #endif
                  /* Color the nodes*/
                     fm = (float **)SUMA_allocate2D(IB.nIsIn, 4, sizeof(float));
                     if (fm == NULL) {
                        fprintf(SUMA_STDERR,"Error %s: Could not allocate for fm.\n", FuncName);
                        break;
                     }
                     for (i=0; i < IB.nIsIn; ++i) {
                         /* id = ND * IB.IsIn[i]; */
                         /*fprintf (SUMA_STDOUT,"\t[%d] %f %f %f\n", IB.IsIn[i] ,\
                                      SO->NodeList[id], SO->NodeList[id+1], SO->NodeList[id+2]);*/
                        /* color those nodes in yellow, just for kicks */
                        fm[i][0] = (float)IB.IsIn[i];
                        fm[i][1] = 0; 
                        fm[i][2] = 0.4;
                        fm[i][3] = 0.4; 
                     }
                     /* register fm with EngineData */
                     sprintf(sfield,"fm");
                     sprintf(sdestination,"SetNodeColor");
                     EngineData->N_cols = 4;
                     EngineData->N_rows = IB.nIsIn;
                     if (!SUMA_RegisterEngineData (EngineData, sfield, (void *)fm, sdestination, ssource, YUP)) {
                        fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
                        break;
                     }

                     /* add and place a call to SE_SetNodeColor */
                     sprintf(tmpcom,"Redisplay|SetNodeColor");
                     SUMA_RegisterCommand (Command, SUMA_COMMAND_DELIMITER, SUMA_COMMAND_TERMINATOR, tmpcom, NOPE);
                     if (!SUMA_Engine (Command, EngineData, sv)) {
                        fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
                        break;
                     }
                  
                  /* free fm since it was registered by pointer and is not automatically freed after the call to SUMA_Engine */
                  if (fm) SUMA_free2D ((char **)fm, IB.nIsIn);
                  
                  /* get ridd of IB's vectors */
                  if (!SUMA_Free_IsInBox (&IB)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to free IB\n", FuncName);
                  }
               } else { /* no node is close enough */
                  /* Do nothing yet */
                  fprintf (SUMA_STDOUT,"\nNo nodes found inside the specified box.\n");
               }
            }
            break;

         case SE_GetNearestNode:
            /* lookfor nodes inside the search box */
            /* expects Node XYZ in EngineData->fv15[0..2]
            Box dimensions in EngineData->fv15[3..5] */
            if (EngineData->fv15_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            {
               SUMA_ISINBOX IB;
               
               SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Focus_SO_ID].OP);
               ND = SO->NodeDim;
               SUMA_etime (&tt, 0);
               IB = SUMA_isinbox (SO->NodeList, SO->N_Node, &(EngineData->fv15[0]), &(EngineData->fv15[3]),  YUP);
               delta_t = SUMA_etime (&tt, 1);
               fprintf (SUMA_STDOUT,"Elapsed time for isinbox operation: %f\n", delta_t);
               fprintf (SUMA_STDOUT,"\t%d nodes (out of %d) found in box\n",IB.nIsIn, SO->N_Node);
               /* save the calling source */
               SUMA_EngineSourceString (ssource, EngineData->fv15_Dest);
               /* release fv15 since it will be reused below*/
               if (!SUMA_ReleaseEngineData (EngineData, NextCom)) {
                  fprintf(SUMA_STDERR,"Error %s: Failed to release fv15.\n", FuncName);
                  break;
               }

               if (IB.nIsIn) { /* found some, find the closest node */
                  /* locate the closest node and store it's id in EngineData*/
                  SUMA_MIN_LOC_VEC (IB.d, IB.nIsIn, ft, it);
                  
                  /* get the XYZ and normal of that node */
                  id = ND * IB.IsIn[it];
                  fv15[0] = SO->NodeList[id];
                  fv15[1] = SO->NodeList[id+1];
                  fv15[2] = SO->NodeList[id+2];
                  fv15[3] = SO->NodeNormList[id];
                  fv15[4] = SO->NodeNormList[id+1];
                  fv15[5] = SO->NodeNormList[id+2];
                  /* register fv in EngineData */
                     sprintf(sfield,"fv15");
                     sprintf(sdestination,"SetLookAtNode");
                     if (!SUMA_RegisterEngineData (EngineData, sfield, (void *)fv15, sdestination, ssource, NOPE)) {
                        fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
                        break;
                     }

                  /* register a call to SetLookAtNode */
                     sprintf(tmpcom,"SetLookAtNode");
                     SUMA_RegisterCommand (Command, SUMA_COMMAND_DELIMITER, SUMA_COMMAND_TERMINATOR, tmpcom, NOPE);

                  /* get ridd of IB's vectors */
                  if (!SUMA_Free_IsInBox (&IB)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to free IB\n", FuncName);
                  }
               } else { /* no node is close enough */
                  /* Do nothing yet */
               }
            }
            break;
            
         case SE_SetRotMatrix:
            /* expects a rotation matrix in fm, 4x4 */
            /* takes the rotation matrix 3x3 with 0 in 4th row and column and 1.0 at 4,4 
            makes a quaternion from it and sets csv->currentQuat and posts redisplay */
            /* Double check on Data destination */
            if (EngineData->fm_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            if (EngineData->N_rows != 4 || EngineData->N_cols != 4) {
               fprintf(SUMA_STDERR,"Error %s: fm must have 4 cols and 4 rows in SetRotMatrix\n", FuncName);
               break;
            }
            if (!SUMA_mattoquat (EngineData->fm, sv->GVS[sv->StdView].currentQuat))
               {
                  fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_mattoquat\n", FuncName);
                  break;
               }
            break;
            
         /*case SE_Something:
            break;*/

         case SE_BadCode:
            fprintf(SUMA_STDERR,"Error SUMA_Engine: Command ->%s<- Not understood. Perhaps Code is not defined in SUMA_CommandCode\n", NextCom);
            break;
         
      } /* switch NextComCode */
      /* release used EngineData */
      if (EngineData) {
         /*fprintf(SUMA_STDERR,"%s: Releasing Engine Data ...", FuncName);*/
         if (!SUMA_ReleaseEngineData (EngineData, NextCom)) {
            fprintf(SUMA_STDERR,"Error SUMA_Engine: Failed to Release EngineData \n");
         }
         /*fprintf(SUMA_STDERR,"%s: OK.\n", FuncName);*/
      }
      NextComCode = SUMA_GetNextCommand (Command, SUMA_COMMAND_DELIMITER, SUMA_COMMAND_TERMINATOR, NextCom);
   } /* cycle through NextCom */
   if (LocalHead) fprintf (SUMA_STDERR, "\n");
   /* If you get here, all is well */
   SUMA_RETURN (YUP);
}

/*!
   ans = SUMA_ShownSOs (sv, dov, SO_IDs);
   gets the IDs (indices into dov) and number of the Surface Objects shown in sv
   \param sv (SUMA_SurfaceViewer *) the surface viewer structure
   \param dov (SUMA_DO *) the Displayable Objects vector (accessible to sv)
   \param SO_IDs (int *) pre-allocated integer vector that will contain the IDs of the SO shown in sv
         send NULL if you do not care for it and all you'll get is ans
   \ret ans (int) the number of SOs shown in SV
   Still confused ? read the code for the function, it is shorter than the documentation.
*/
int SUMA_ShownSOs (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int *SO_IDs)
{
   static char FuncName[]={"SUMA_ShownSOs"};
   int i, k = 0;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   for (i=0; i< sv->N_DO; ++i) {
      if (SUMA_isSO(dov[sv->ShowDO[i]])) {
         if (SO_IDs != NULL) SO_IDs[k] = sv->ShowDO[i];
         ++k;
      }
   }

   SUMA_RETURN (k);
}

/*! 
   nxtState = SUMA_NextState(sv);

   get the next Viewing State available in sv
   \param sv (SUMA_SurfaceViewer *) pointer to surface viewer structure 
   \ret nxtState (int) the index into sv->VSv of the next state
      -1 if there is trouble
   \sa SUMA_PrevState 
*/
int SUMA_NextState(SUMA_SurfaceViewer *sv)
{
   static char FuncName[] = {"SUMA_NextState"};
   int inxt, icur;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   icur = SUMA_WhichState (sv->State, sv);
   if (icur < 0) {
      fprintf(SUMA_STDERR,"Error %s: SUMA_WhichState failed.\n", FuncName);
      SUMA_RETURN (-1);
   } else {
      SUMA_RETURN((icur + 1) % sv->N_VSv);
   }
   
   SUMA_RETURN (-1);
}

/*!
   precState = SUMA_PreviState (sv);
   get the previous Viewing State available in sv
   \sa SUMA_NextState
*/
int SUMA_PrevState(SUMA_SurfaceViewer *sv)
{
   static char FuncName[] = {"SUMA_PrevState"};
   int inxt, icur;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   icur = SUMA_WhichState (sv->State, sv);   if (icur < 0) {
      fprintf(SUMA_STDERR,"Error %s: SUMA_WhichState failed.\n", FuncName);
      SUMA_RETURN (-1);
   } else {
      icur = icur -1;
      if (icur < 0) icur = sv->N_VSv + icur;
      SUMA_RETURN(icur);
   }
   
   SUMA_RETURN (-1);
}



/*!
   SOnxtID = SUMA_NextSO (dov, n_dov, CurrentIDcode, SOnxt);
   Get the next Surface Object in DOv
   \param dov (SUMA_DO *) vector containing all displayable objects
   \param n_dov (int) number of elements in dov
   \param CurrentIDcode (char *) idcode of current surface
   \param SOnxt (SUMA_SurfaceObject *) pointer to next surface object 
   \ret SOnxtID (int) index into dov of SOnxt (-1) if there's an error
*/

int SUMA_NextSO (SUMA_DO *dov, int n_dov, char *idcode, SUMA_SurfaceObject *SOnxt)
{
   static char FuncName[] = {"SUMA_NextSO"};
   int icur, icheck, ncheck;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (SOnxt != NULL) {
      fprintf(SUMA_STDERR,"Error %s: SOnxt should be null when you call this function.\n", FuncName);
      SUMA_RETURN (-1);
   }
   if (n_dov < 1) {
      fprintf(SUMA_STDERR,"Error %s: dov contains no elements.\n", FuncName);
      SUMA_RETURN (-1);
   }
   icur = SUMA_findSO_inDOv (idcode, dov, n_dov);
   if (icur < 0) {
      fprintf (SUMA_STDERR,"Error %s: idcode not found in dov.\n", FuncName);
      SUMA_RETURN (-1);
   }
   
   ncheck = 0;
   icheck = icur;
   while (ncheck < n_dov) {
      icheck = (icheck + 1) % n_dov;
      /*fprintf(SUMA_STDERR,"%s: Checking %d\n", FuncName, icheck);*/
      if (SUMA_isSO(dov[icheck])) {
         /*fprintf(SUMA_STDERR,"%s: Settling on %d\n", FuncName, icheck);*/
         SOnxt = (SUMA_SurfaceObject *)dov[icheck].OP;
         SUMA_RETURN (icheck);
      }
      ++ncheck;
   }
   /* should not get here */
   SUMA_RETURN (-1);
}

/*! 
   Replaces one surface in ShowDO with another 

*/
SUMA_Boolean SUMA_SwitchSO (SUMA_DO *dov, int N_dov, int SOcurID, int SOnxtID, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_SwitchSO"};
   SUMA_Axis *EyeAxis;
   int EyeAxis_ID;
   char CommString[100];
   SUMA_EngineData ED;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* unregister the current surface from ShowDO */
   /*fprintf(SUMA_STDERR,"%s: Unregistering DOv[%d]...\n", FuncName, SOcurID);*/
   if (!SUMA_UnRegisterDO(SOcurID, sv)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to UnRegisterDO.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   /* set the focus ID to the current surface */
   sv->Focus_SO_ID = SOnxtID;

   /* register the new surface in ShowDO */
   /*fprintf(SUMA_STDERR,"%s: Registering DOv[%d]...\n", FuncName, sv->Focus_SO_ID); */
   if (!SUMA_RegisterDO(sv->Focus_SO_ID, sv)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to RegisterDO.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   /* modify the rotation center */
   if (!SUMA_UpdateRotaCenter(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update center of rotation", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* set the viewing points */
   if (!SUMA_UpdateViewPoint(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update view point", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* Change the defaults of the eye axis to fit standard EyeAxis */
   EyeAxis_ID = SUMA_GetEyeAxis (sv, dov);

   if (EyeAxis_ID < 0) {
      fprintf(SUMA_STDERR,"Error %s: No Eye Axis. %d\n", FuncName, EyeAxis_ID);
   } else {
      EyeAxis = (SUMA_Axis *)(dov[EyeAxis_ID].OP);
      SUMA_EyeAxisStandard (EyeAxis, sv);
   }
   
   /* Home call baby */
   sprintf(CommString,"Home~");
   if (!SUMA_Engine (CommString, &ED, sv)) {
      fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
   }

   
   /* take care of the cross hair's XYZ */

   /* to do elsewhere */
   /* when a cross hair needs to be communicated, you must use the MapRef_idcode_str surface and not the Focus_Surface */
   SUMA_RETURN (YUP);
}

/*! 
   ans = SUMA_SwitchState (dov, N_dov, sv, nxtstateID);
   
   Replaces one viewing state with another

*/
SUMA_Boolean SUMA_SwitchState (SUMA_DO *dov, int N_dov, SUMA_SurfaceViewer *sv, int nxtstateID)
{
   static char FuncName[]={"SUMA_SwitchState"};
   SUMA_Axis *EyeAxis;
   int EyeAxis_ID, I_C, OverInd, ND, id;
   char CommString[100];
   SUMA_EngineData ED;
   int curstateID, i, j, jmax, prec_ID;
   SUMA_SurfaceObject *SO_nxt, *SO_prec;
   float *XYZ, *XYZmap;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   XYZ = NULL;
   XYZmap = NULL;
   
   curstateID = SUMA_WhichState(sv->State, sv);
   
   /* unregister all the surfaces for the current view */
   if (LocalHead) fprintf(SUMA_STDERR,"Local Debug %s: Unregistering \n", FuncName);
   for (i=0; i<sv->VSv[curstateID].N_MembSOs; ++i) {
      if (!SUMA_UnRegisterDO(sv->VSv[curstateID].MembSOs[i], sv)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to UnRegisterDO.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
   }
   
   /* register all the surfaces from the next view */
   if (LocalHead) fprintf(SUMA_STDERR,"Local Debug %s: Registering DOv...\n", FuncName);
   for (i=0; i<sv->VSv[nxtstateID].N_MembSOs; ++i) {
      if (!SUMA_RegisterDO(sv->VSv[nxtstateID].MembSOs[i], sv)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to RegisterDO.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
   }
   
   /*set the Color Remix flag */
   if (!SUMA_SetShownLocalRemixFlag (sv)) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SetShownLocalRemixFlag.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   
   /* if no coloroverlay exists, link to MapReference surface, if possible */
   for (i=0; i<sv->VSv[nxtstateID].N_MembSOs; ++i) {
      /* next surface being checked */
      SO_nxt = (SUMA_SurfaceObject *)(dov[sv->VSv[nxtstateID].MembSOs[i]].OP);

      /* Get the Mapping Reference surface, that's the precursor*/
      if (!SO_nxt->MapRef_idcode_str) {
         prec_ID = -1;
      }else {
         prec_ID = SUMA_findSO_inDOv(SO_nxt->MapRef_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
      }
      if (prec_ID < 0) {
         /* no precursors found, notify user */
         fprintf(SUMA_STDERR, "\n\aWarning %s: No precursors found for surface %d.\nColors, selected nodes and facesets will not be reflect those in previous state.\n.",\
          FuncName, sv->VSv[nxtstateID].MembSOs[i]);
         continue;
      }

      SO_prec = (SUMA_SurfaceObject *)(dov[prec_ID].OP);

      /* check for risk of node inconsistencies */

      if (SO_prec->N_Node >= SO_nxt->N_Node ) {/* > or equal number of nodes*/
         /* matching number of nodes */
         /* Create a link to each overlay plane in the precursor unless such a plane exists already  */
         for (j=0; j < SO_prec->N_Overlays; ++j) {
            if (!SUMA_Fetch_OverlayPointer (SO_nxt->Overlays, SO_nxt->N_Overlays, SO_prec->Overlays[j]->Name, &OverInd)) {
               /* plane not found, create a link to it */
               if (LocalHead) fprintf (SUMA_STDERR,"Local Debug %s: Overlay plane %s not found, creating the link.\n", FuncName, SO_prec->Overlays[j]->Name);
               SO_nxt->Overlays_Inode[SO_nxt->N_Overlays] = SUMA_CreateInodeLink (SO_nxt->Overlays_Inode[SO_nxt->N_Overlays], SO_prec->Overlays_Inode[j]);
               if (!SO_nxt->Overlays_Inode[SO_nxt->N_Overlays]) {
                  fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInodeLink\n", FuncName);
                  SUMA_RETURN (NOPE);
               }
               /* now copy the actual overlay plane pointer */
               SO_nxt->Overlays[SO_nxt->N_Overlays] = SO_prec->Overlays[j];
               /*increment the number of overlay planes */
               ++SO_nxt->N_Overlays;
            } else {
               /* plane found, do nothing */
               if (LocalHead) fprintf (SUMA_STDERR,"Local Debug %s: Overlay plane %s found. Index#%d\n.", FuncName, SO_prec->Overlays[j]->Name, OverInd);
            }
         }


         if (SO_prec->N_Node > SO_nxt->N_Node) {/* More in prec */
            /* just warn */
            fprintf(SUMA_STDERR, "Warning %s: More nodes (%d) in precursor surface. \n Assuming upcoming surface is a subset of precursor.\n", FuncName, SO_prec->N_Node - SO_nxt->N_Node);
         }/* More in prec */ 

         /* link the selected nodes and facesets, if possible */
         /*fprintf(SUMA_STDERR, "%s: Linking selected nodes  ...\n", FuncName);*/
         /* check for risk of node inconsistencies */
         if (SO_prec->N_Node == SO_nxt->N_Node) {
            SO_nxt->SelectedNode = SO_prec->SelectedNode;
            } else { /* more nodes in precursor, make sure selected node is OK */
            if (SO_prec->SelectedNode < SO_nxt->N_Node) {
               SO_nxt->SelectedNode = SO_prec->SelectedNode;
               } else { /* this node does not exist in the upcoming thing */
               fprintf(SUMA_STDERR, "\n\aWarning %s: Slected node in precursor state does not exist in current state.\n Selected Node is left at previous setting in this view state.\n", FuncName);
               }
            }

         } /* > or equal number of nodes */ else { /* less in prec */
            fprintf(SUMA_STDERR, "\n\aWarning %s: More nodes (%d) in upcoming surface. Colors, selected nodes and facesets are not carried through from precursor.\n", FuncName, SO_nxt->N_Node - SO_prec->N_Node);
         }

         #if 0
         /* You do not want to mix colors yet, the flag for doing that has already been set*/
         /* Here you need to remix the colors */
         if (!SUMA_Overlays_2_GLCOLAR4(SO_nxt->Overlays, SO_nxt->N_Overlays, SUMA_GetColorList (sv, SO_nxt->idcode_str), SO_nxt->N_Node,\
             sv->Back_Modfact, sv->ShowBackground, sv->ShowForeground)) {
            fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Overlays_2_GLCOLAR4.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         #endif
         
      }
   
   /* Bind the cross hair to a reasonable surface, if possible */
   if (sv->Ch->SurfaceID >= 0) {      
      if (LocalHead) fprintf(SUMA_STDERR, "Local Debug %s: Linking Cross Hair via SurfaceID...\n", FuncName);
      j = SUMA_MapRefRelative (sv->Ch->SurfaceID, sv->VSv[nxtstateID].MembSOs, sv->VSv[nxtstateID].N_MembSOs, dov);
      if (LocalHead) fprintf(SUMA_STDERR, "Local Debug %s: Cross Hair's  New SurfaceID = %d\n", FuncName, j );
      
      /* set the XYZ of the cross hair based on the coordinates of the upcoming surface, if possible */
      if (j >= 0) {
         SO_nxt = (SUMA_SurfaceObject *)(dov[j].OP);
         ND = SO_nxt->NodeDim;
         id = ND * sv->Ch->NodeID;
         if (sv->Ch->NodeID >= 0) {
            if (LocalHead) fprintf(SUMA_STDERR, "Local Debug %s: Using NodeID for link.\n", FuncName);
            sv->Ch->c[0] = SO_nxt->NodeList[id];
            sv->Ch->c[1] = SO_nxt->NodeList[id+1];
            sv->Ch->c[2] = SO_nxt->NodeList[id+2];
         } else {
            /* no node associated with cross hair, use XYZ */
            if (LocalHead) fprintf(SUMA_STDERR, "Local Debug %s: Using XYZ for link.\n", FuncName);
            SO_prec = (SUMA_SurfaceObject *)(dov[sv->Ch->SurfaceID].OP);
            /* go from XYZ to XYZmap on current surface then from XYZmap to XYZ on new surface */
            I_C = -1;
            XYZmap = SUMA_XYZ_XYZmap (sv->Ch->c, SO_prec, dov, N_dov, &I_C);
            if (XYZmap == NULL) {
               fprintf(SUMA_STDERR, "Error %s: Failed in SUMA_XYZ_XYZmap\n", FuncName); 
            }else {
               XYZ = SUMA_XYZmap_XYZ (XYZmap, SO_nxt, dov, N_dov, &I_C);
               if (XYZ == NULL) {
                  fprintf(SUMA_STDERR, "Error %s: Failed in SUMA_XYZmap_XYZ\n", FuncName); 
               } else {
                  sv->Ch->c[0] = XYZ[0];
                  sv->Ch->c[1] = XYZ[1];
                  sv->Ch->c[2] = XYZ[2];
               }
               
            }
            if (XYZ) SUMA_free(XYZ);
            if (XYZmap) SUMA_free(XYZmap);
         }
      } else {
         fprintf(SUMA_STDERR, "%s: No relatives between states. CrossHair location will not correspond between states\n", FuncName); 
      }
       sv->Ch->SurfaceID = j;
      if (LocalHead) fprintf(SUMA_STDERR, "Local Debug %s: Linking Cross Hair Via NodeID Done.\n", FuncName);
   }
   


   /* switch the state accordingly */
   sv->State =  sv->VSv[nxtstateID].Name;
   sv->iState = nxtstateID;
   
   /* set the focus ID to the first surface in the next view   */
   sv->Focus_SO_ID = sv->VSv[nxtstateID].MembSOs[0];

   /* decide what the best state is */
   sv->StdView = SUMA_BestStandardView (sv,dov, N_dov);
   if (LocalHead) fprintf(SUMA_STDOUT,"%s: Standard View Now %d\n", FuncName, sv->StdView);
   if (sv->StdView == SUMA_Dunno) {
      fprintf(SUMA_STDERR,"Error %s: Could not determine the best standard view. Choosing default SUMA_3D\n", FuncName);
      sv->StdView = SUMA_3D;
   }

   /* modify the rotation center */
   if (!SUMA_UpdateRotaCenter(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update center of rotation", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* set the viewing points */
   if (!SUMA_UpdateViewPoint(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update view point", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* Change the defaults of the eye axis to fit standard EyeAxis */
   EyeAxis_ID = SUMA_GetEyeAxis (sv, dov);

   if (EyeAxis_ID < 0) {
      fprintf(SUMA_STDERR,"Error %s: No Eye Axis. %d\n", FuncName, EyeAxis_ID);
   } else {
      EyeAxis = (SUMA_Axis *)(dov[EyeAxis_ID].OP);
      SUMA_EyeAxisStandard (EyeAxis, sv);
   }
   
    
   /* Home call baby */
   sprintf(CommString,"Home~");
   if (!SUMA_Engine (CommString, &ED, sv)) {
      fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
   }

   SUMA_RETURN (YUP);
}

/*!
\brief ans = SUMA_OpenGLStateReset (dov, N_dov, sv);
Used when going from one surface viewer to another. The OpenGL state variables 
need to be reset when moving from one viewer to the next. Otherwise you risk having 
unpredictable results the first time you do something in one viewer after you'd been
in another. 
This function is a stripped down version of SUMA_SwitchState and should 
be followed by a call to SUMA_postRedisplay for all the changes to take effect.
Do not try executing all the commands in SUMA_display that affect the modelview 
matrix and the projection matrix without calling for a display the changes will not take effect.

\param dov (SUMA_DO *) Pointer to vector of displayable objects, typically SUMAg_DOv
\param N_dov (int) number of elements in dov, typically SUMAg_N_DOv
\param sv (SUMA_SurfaceViewer *) viewer making the request.
\return YUP/NOPE Good/Bad

*/
SUMA_Boolean SUMA_OpenGLStateReset (SUMA_DO *dov, int N_dov, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_OpenGLStateReset"};
   SUMA_Axis *EyeAxis;
   int EyeAxis_ID, I_C, OverInd, ND, id;
   char CommString[100];
   SUMA_EngineData ED;
   int  i, j, jmax, prec_ID;
   SUMA_SurfaceObject *SO_nxt, *SO_prec;
   SUMA_Boolean LocalHead = YUP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);   
   
   #if 0
   /* modify the rotation center */
   if (!SUMA_UpdateRotaCenter(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update center of rotation", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* set the viewing points */
   if (!SUMA_UpdateViewPoint(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update view point", FuncName);
      SUMA_RETURN (NOPE);
   }
   #endif
   
   /* This is all that is needed, the others above do not need to be updated at this stage*/
   
   /* Change the defaults of the eye axis to fit standard EyeAxis */
   EyeAxis_ID = SUMA_GetEyeAxis (sv, dov);

   if (EyeAxis_ID < 0) {
      fprintf(SUMA_STDERR,"Error %s: No Eye Axis. %d\n", FuncName, EyeAxis_ID);
   } else {
      EyeAxis = (SUMA_Axis *)(dov[EyeAxis_ID].OP);
      SUMA_EyeAxisStandard (EyeAxis, sv);
   }
   

   #if 0
   /* force an axis drawing to set the projection matrix correctly */
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Setting up matrix mode and perspective ...\n", FuncName);
   glMatrixMode (GL_PROJECTION);
   glLoadIdentity ();
   gluPerspective((GLdouble)sv->FOV[sv->iState], sv->Aspect, SUMA_PERSPECTIVE_NEAR, SUMA_PERSPECTIVE_FAR); /*lower angle is larger zoom,*/
   #endif
   
   /* You still need to call SUMA_display via SUMA_postRedisplay but that is done after this function returns */ 

   SUMA_RETURN (YUP);
}

/*!
   EyeAxisID = SUMA_GetEyeAxis (sv, dov);
   gets the ID (indices into dov) of the Eye Axis in sv
   \param sv (SUMA_SurfaceViewer *) the surface viewer structure
   \param dov (SUMA_DO *) the Displayable Objects vector (accessible to sv)
   \param Ax (SUMA_Axis *) a pointer to the Eye Axis structure (NULL if error )
   \ret EyeAxisID (int) the index into dov of the Eye Axis 
      if an error is encountered, including more than one Eye Axis, a -1 is returned
*/
int SUMA_GetEyeAxis (SUMA_SurfaceViewer *sv, SUMA_DO *dov)
{
   static char FuncName[]={"SUMA_GetEyeAxis"};
   int i, k = -1, cnt = 0;
   SUMA_Axis *AO;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   for (i=0; i< sv->N_DO; ++i) {
      if (dov[sv->ShowDO[i]].ObjectType == AO_type) {
         AO = (SUMA_Axis *)(dov[sv->ShowDO[i]].OP);
         if (strcmp(AO->Name, "Eye Axis") == 0) {
            k = sv->ShowDO[i];
            ++cnt;
         }
      }
   }
   if (cnt > 1) {
      fprintf (SUMA_STDERR,"Error %s: Found more than one Eye Axis. \n", FuncName);
      SUMA_RETURN (-1);
   }
   
   SUMA_RETURN (k);
}

/*! 
   transform current XYZ to XYZmap 
   The XYZ on an auxilliary surface are of no relevance to the volume. They must be transformed
   to mappable XYZ (in mm, RAI, in alignment with the Parent Volume)   
   XYZmap = SUMA_XYZ_XYZmap (XYZ, SO, dov, N_dov, I_C);

   \param XYZ (float *) XYZ triplet in SO's native coordinate space
   \param SO (SUMA_SurfaceObject *SO) obvious, ain't it
   \param dov (SUMA_DO*) vector containing all displayable objects
   \param N_dov (int) number of elements in dov
   \param I_C (int *) (pre allocated) pointer to the index of the closest (or representative) node 
                       in SO to the XYZ location. If you do not have it, make sure *I_C = -1. If you
                       do so, the function will search for nodes contained in a box mm wide
                       and centered on XYZ. If nodes are found in the box the I_C is set to the
                       index of the closest node and XYZmap contains the coordinates of I_C in the 
                       SO->MapRef_idcode_str surface.
   \ret XYZmap (float *) Mappable XYZ coordinates. NULL in case of trouble.

*/

float * SUMA_XYZ_XYZmap (float *XYZ, SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_dov, int *I_C)
{/* SUMA_XYZ_XYZmap */
   static char FuncName[]={"SUMA_XYZ_XYZmap"};
   float *XYZmap;
   int iclosest, id, ND;
   SUMA_SurfaceObject *SOmap;
   int SOmapID;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* allocate for return */
   XYZmap = (float *)SUMA_calloc (3, sizeof(float));
   if (XYZmap == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Could not allocate for XYZmap.\n", FuncName);
      SUMA_RETURN (NULL);
   } 
   
   /* if surface is Inherently mappable, do the obivious */
   if (SUMA_isINHmappable(SO)){
      /*fprintf(SUMA_STDERR,"%s: Surface is inherently mappable. XYZmap = XYZ.\n", FuncName); */
      SUMA_COPY_VEC (XYZ, XYZmap, 3, float, float);
      SUMA_RETURN (XYZmap);   
   }
   /* if surface is not Inherrently mappable, do the deed */
   if (!SUMA_ismappable(SO)){
      fprintf(SUMA_STDERR,"%s: Surface is NOT mappable, returning NULL.\n", FuncName);
      SUMA_free(XYZmap);
      SUMA_RETURN (NULL);
   }

   /* surface is mappable, things will get more complicated */

   /* find the closest node in SO */
   if (*I_C < 0) { /* user has not specified closest node ID*/
      /* must find closest node on my own */
         {
            SUMA_ISINBOX IB;
            float Bd[3], distance;
            int ii;
            
            /* set the search box dimensions */
            Bd[0] = Bd[1] = Bd[2] = SUMA_XYZ_XFORM_BOXDIM_MM;
            IB = SUMA_isinbox (SO->NodeList, SO->N_Node, XYZ, Bd,  YUP);
            fprintf (SUMA_STDOUT,"%s: %d nodes (out of %d) found in box\n",FuncName, IB.nIsIn, SO->N_Node);

            if (IB.nIsIn) { /* found some, find the closest node */
               /* locate the closest node and store it's id in EngineData*/
               /*for (ii=0; ii<IB.nIsIn; ++ii) {
                  fprintf (SUMA_STDERR,"%d\t%.3f\t\t", IB.IsIn[ii], IB.d[ii]);
               }*/
               SUMA_MIN_LOC_VEC (IB.d, IB.nIsIn, distance, iclosest);
               iclosest = IB.IsIn[iclosest];
               /* get ridd of IB's vectors */
               if (!SUMA_Free_IsInBox (&IB)) {
                  fprintf(SUMA_STDERR,"Error %s: Failed to free IB\n", FuncName);
               }

            } else { /* no node is close enough */
               fprintf (SUMA_STDERR,"%s: No node was close enough to XYZ, no linkage possible\n", FuncName);
               SUMA_free(XYZmap);
               SUMA_RETURN (NULL);
            }
            /* store iclosest for lazy user */
            *I_C = iclosest;
         }
   } else { 
      iclosest = *I_C;
   }
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Node identified for linking purposes is %d\n", FuncName, *I_C);
   /* find the SO that is the Mappable cahuna */
   SOmapID = SUMA_findSO_inDOv(SO->MapRef_idcode_str, dov, N_dov);
   if (SOmapID < 0) {
      fprintf (SUMA_STDERR,"%s: Failed in SUMA_findSO_inDOv This should not happen.\n", FuncName);
      SUMA_free(XYZmap);
      SUMA_RETURN (NULL);
   }

   SOmap = (SUMA_SurfaceObject *)(dov[SOmapID].OP);
   ND = SOmap->NodeDim;
   id = ND * iclosest;
   XYZmap[0]=SOmap->NodeList[id];
   XYZmap[1]=SOmap->NodeList[id+1];
   XYZmap[2]=SOmap->NodeList[id+2];

   /* all is done */

   SUMA_RETURN (XYZmap);
}/* SUMA_XYZ_XYZmap */

/*! 
   transform  XYZmap to XYZ on current surface
   
   XYZ = SUMA_XYZmap_XYZ (XYZmap, SO, dov, N_dov, I_C);

   \param XYZmap (float *) XYZmap triplet in SO's MapRef coordinate space
   \param SO (SUMA_SurfaceObject *SO) obvious, ain't it
   \param dov (SUMA_DO*) vector containing all displayable objects
   \param N_dov (int) number of elements in dov
   \param I_C (int *) (pre allocated) pointer to the index of the closest (or representative) node 
                       in SO's MapRef to the XYZmap location. If you do not have it, make sure *I_C = -1. If you
                       do so, the function will search for nodes contained in a box mm wide
                       and centered on XYZmap. If nodes are found in the box the I_C is set to the
                       index of the closest node and XYZ contains the coordinates of I_C in the 
                       SO surface.
   \ret XYZ (float *) Equivalent of XYZmap on the auxilliary surface SO. NULL in case of trouble.

   \sa SUMA_XYZ_XYZmap
*/

float * SUMA_XYZmap_XYZ (float *XYZmap, SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_dov, int *I_C)
{/* SUMA_XYZmap_XYZ */
   static char FuncName[]={"SUMA_XYZmap_XYZ"};
   float *XYZ;
   int iclosest, id, ND;
   SUMA_SurfaceObject *SOmap;
   int SOmapID;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* allocate for return */
   XYZ = (float *)SUMA_calloc (3, sizeof(float));
   if (XYZ == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Could not allocate for XYZ.\n", FuncName);
      SUMA_RETURN (NULL);
   } 
   
   /* if surface is not mappable, do the deed */
   if (!SUMA_ismappable(SO)){
      fprintf(SUMA_STDERR,"%s: Surface is NOT mappable, returning NULL.\n", FuncName);
      SUMA_free(XYZ);
      SUMA_RETURN (NULL);
   }

   /* if surface is Inherently mappable, do the obivious */
   if (SUMA_isINHmappable(SO)){
      fprintf(SUMA_STDERR,"%s: Surface is inherently mappable. XYZ = XYZmap.\n", FuncName);
      SUMA_COPY_VEC (XYZmap, XYZ, 3, float, float);
      SOmap = SO;
      /* do not return yet, must fix the node id too */
   } else {
      /* surface is mappable, things will get more complicated */
      /* find the SO that is the Mappable cahuna */
      SOmapID = SUMA_findSO_inDOv(SO->MapRef_idcode_str, dov, N_dov);
      if (SOmapID < 0) {
         fprintf (SUMA_STDERR,"%s: Failed in SUMA_findSO_inDOv This should not happen.\n", FuncName);
         SUMA_free(XYZ);
         SUMA_RETURN (NULL);
      }
      SOmap = (SUMA_SurfaceObject *)(dov[SOmapID].OP);
   }
   /* find the closest node in SO */
   if (*I_C < 0) { /* user has not specified closest node ID*/
      /* must find closest node on my own */
         {
            SUMA_ISINBOX IB;
            float Bd[3], distance;
            int ii;
            
            /* set the search box dimensions */
            Bd[0] = Bd[1] = Bd[2] = SUMA_XYZ_XFORM_BOXDIM_MM;
            IB = SUMA_isinbox (SOmap->NodeList, SOmap->N_Node, XYZmap, Bd,  YUP);
            fprintf (SUMA_STDERR,"%s: %d nodes (out of %d) found in box\n",FuncName, IB.nIsIn, SOmap->N_Node);

            if (IB.nIsIn) { /* found some, find the closest node */
               /* locate the closest node and store it's id in EngineData*/
               /*for (ii=0; ii<IB.nIsIn; ++ii) {
                  fprintf (SUMA_STDERR,"%d\t%.3f\t\t", IB.IsIn[ii], IB.d[ii]);
               }*/
               SUMA_MIN_LOC_VEC (IB.d, IB.nIsIn, distance, iclosest);
               iclosest = IB.IsIn[iclosest];
               /* get ridd of IB's vectors */
               if (!SUMA_Free_IsInBox (&IB)) {
                  fprintf(SUMA_STDERR,"Error %s: Failed to free IB\n", FuncName);
               }

            } else { /* no node is close enough */
               if (SO != SOmap) {
                  fprintf (SUMA_STDERR,"%s: No node was close enough to XYZmap, no linkage possible\n", FuncName);
                  SUMA_free(XYZ);
                  SUMA_RETURN (NULL);
               } else {
                  /* comes from inherrently mappable stuff, makes sense to leave XYZ */
                  fprintf (SUMA_STDERR,"%s: No node was close enough to XYZmap, no node linkage possible.\n",FuncName); 
                  SUMA_RETURN (XYZ);
               }
            }
            /* store iclosest for lazy user */
            *I_C = iclosest;
         }
   } else { 
      iclosest = *I_C;
   }
   fprintf (SUMA_STDERR,"%s: Node identified for linking purposes is %d\n", FuncName, *I_C);
   ND = SO->NodeDim;
   id = ND * iclosest;
   XYZ[0]=SO->NodeList[id];
   XYZ[1]=SO->NodeList[id+1];
   XYZ[2]=SO->NodeList[id+2];

   /* all is done */
   SUMA_RETURN (XYZ);
}/* SUMA_XYZmap_XYZ */

/*! 
   Prec_ID = SUMA_MapRefRelative (Cur_ID, Prec_List, N_Prec_List, dov);
   Returns the ID (index into dov) of the surface object in Prec_List that is related 
   (via MapRef) to the surface object Cur_ID.
   This means that SOcur.MapRef_idcode_str = SOprec.MapRef_icode_str or SOprec.idcode_str

   \param Cur_ID (int) index into dov of the current surface object
   \param Prec_List (int *) indices into dov of the precursor surface objects 
   \param N_Prec_List (int) number of indices in Prec_List
   \param dov (SUMA_DO *) the vector of Displayable Object Structures
   \ret Prec_ID (int) index into dov of the surface object that is related to Cur_ID

*/
int SUMA_MapRefRelative (int cur_id, int *prec_list, int N_prec_list, SUMA_DO *dov) 
{
   int i, rel_id = -1;
   static char FuncName[]={"SUMA_MapRefRelative"};
   SUMA_SurfaceObject *SOcur, *SO_prec;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SOcur = (SUMA_SurfaceObject *)(dov[cur_id].OP);
   /* if surface has no MapRef then it cannot receive colors from precursors */
   if (!SUMA_ismappable(SOcur)) {
      SUMA_RETURN (-1);
   }

   for (i=0; i<N_prec_list; ++i) {
      SO_prec = (SUMA_SurfaceObject *)(dov[prec_list[i]].OP);
      if (strcmp(SOcur->MapRef_idcode_str, SO_prec->MapRef_idcode_str) == 0 || strcmp(SOcur->MapRef_idcode_str, SO_prec->idcode_str) == 0) {
         /* there's some relationship here, save it for return */
         if (rel_id < 0) {
            rel_id = prec_list[i];
         } else {
            fprintf (SUMA_STDERR,"Error %s: I did not think that would occur! Ignoring other relatives for now.\n", FuncName); 
         }

      }
   }

   SUMA_RETURN (rel_id);

}

