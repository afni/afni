#define DEBUG_1
#ifdef DEBUG_1
	#define DEBUG_2
	#define DEBUG_3
#endif
   
/* Header FILES */
   
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <sys/time.h>
#include <Xm/Form.h>    /* Motif Form widget. */
#include <Xm/Frame.h>   /* Motif Frame widget. */
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* For XA_RGB_DEFAULT_MAP. */
#include <X11/Xmu/StdCmap.h>  /* For XmuLookupStandardColormap. */
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>
#include <GL/GLwMDrawA.h>  /* Motif OpenGL drawing area. */

#include "niml.h"
#include "SUMA_suma.h"

extern SUMA_SurfaceViewer *SUMAg_cSV; 
extern SUMA_DO *SUMAg_DOv;	
extern int SUMAg_N_DOv; 
extern SUMA_CommonFields *SUMAg_CF;

/*!
This is the function that runs the viewers. 
It acts on the viewer that SUMAg_cSV points to
To add a new command:
include it SUMA_define.h in SUMA_ENGINE_CODE's typedef
include it in SUMA_ParseCommands.c, SUMA_CommandCode function 
*/

SUMA_Boolean SUMA_Engine (char *Command, SUMA_EngineData *EngineData)
{
	char NextCom[SUMA_MAX_COMMAND_LENGTH], tmpcom[SUMA_MAX_COMMAND_LENGTH], ssource[100], sfield[100], sdestination[100];
	static char FuncName[]={"SUMA_Engine"};
	int NextComCode, ii, i;
	SUMA_SurfaceObject *SO;
	float delta_t;
	struct  timeval tt;
	int it, Wait_tot, nn;
	float ft, **fm, fv15[15];
	SUMA_Boolean BreakOut;
	XtPointer elvis;
	NI_element *nel;
	/*int iv3[3], iv15[15], **im;
	float fv3[3];
	char s[SUMA_MAX_STRING_LENGTH];*/ /* keep standard unused variables undeclared, else compiler complains*/
	
	
	NextComCode = SUMA_GetNextCommand (Command, SUMA_COMMAND_DELIMITER, SUMA_COMMAND_TERMINATOR, NextCom);
	if (!NextComCode) {
		fprintf (stderr, "%s Error: executing SUMA_GetNextCommand\n", FuncName);
		return (NOPE);
	} 
	
	/*fprintf (SUMA_STDOUT,"%s: ", FuncName);*/
	while (NextComCode) {/* cycle through NextComs */
		/*fprintf (SUMA_STDOUT,"->%s<-\t", NextCom);*/
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
					ulook_old[0] = SUMAg_cSV->ViewFrom[0] - SUMAg_cSV->ViewCenter[0];
					ulook_old[1] = SUMAg_cSV->ViewFrom[1] - SUMAg_cSV->ViewCenter[1];
					ulook_old[2] = SUMAg_cSV->ViewFrom[2] - SUMAg_cSV->ViewCenter[2];
					ulook_new[0] = EngineData->fv3[0]- SUMAg_cSV->ViewCenter[0];
					ulook_new[1] = EngineData->fv3[1]- SUMAg_cSV->ViewCenter[1];
					ulook_new[2] = EngineData->fv3[2]- SUMAg_cSV->ViewCenter[2];
					fm = (float **)SUMA_allocate2D(4,4,sizeof(float));
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
					SUMA_free2D((char **)fm, 4);
					/* register a call to SetRotMatrix */
					sprintf(tmpcom,"Redisplay|SetRotMatrix");
					SUMA_RegisterCommand (Command, SUMA_COMMAND_DELIMITER, SUMA_COMMAND_TERMINATOR, tmpcom, NOPE);
				}
				break;
			
			case SE_ToggleTalkToAfni:
					BreakOut = NOPE;
					if (!SUMA_CanTalkToAfni(SUMAg_cSV, SUMAg_DOv)) {
						SUMAg_cSV->TalkToAfni = NOPE;
						fprintf(SUMA_STDERR,"Error %s: Cannot talk to afni.\nMapping Reference or Volume Parent missing for at least one of the displayed surfaces.\n", FuncName);
						break;
					} else {
						/*fprintf(SUMA_STDERR,"Surface Viewer passed SUMA_CanTalkToAfni test\n");*/
					}
					SUMAg_cSV->TalkToAfni = !SUMAg_cSV->TalkToAfni;
					if (SUMAg_cSV->TalkToAfni) {
						fprintf(SUMA_STDOUT,"%s: Contacting afni ...\n", FuncName);
						/* contact afni */
							SUMAg_cSV->ns = NI_stream_open( SUMAg_CF->NimlAfniStream , "w" ) ;
							/*	SUMAg_cSV->ns = NI_stream_open( "tcp:128.231.212.194:53211" , "w" ) ;*/
						if( SUMAg_cSV->ns == NULL ){
         				fprintf(SUMA_STDERR,"Error %s: NI_stream_open failed\n", FuncName) ; break ;
      				}
      				Wait_tot = 0;
						while(Wait_tot < SUMA_WriteCheckWaitMax){
      				  nn = NI_stream_writecheck( SUMAg_cSV->ns , SUMA_WriteCheckWait) ;
      				  if( nn == 1 ){ fprintf(stderr,"\n") ; break ; }
      				  if( nn <  0 ){ fprintf(stderr,"BAD\n"); BreakOut = YUP; break;}
						  Wait_tot += SUMA_WriteCheckWait;
      				  fprintf(SUMA_STDERR,".") ;
      				}
						
						/* make sure you did not exit because of time out */
						if (nn!=1) {
							BreakOut = YUP;
							fprintf(SUMA_STDERR,"Error %s: WriteCheck timed out (> %d ms).\n", FuncName, SUMA_WriteCheckWaitMax);
						}
						
						if (BreakOut) {
							/* reverse TalkToAfni */
							SUMAg_cSV->TalkToAfni = !SUMAg_cSV->TalkToAfni;
							
							/* get out of case */
							break;
						}
      				
						/* Looks good, start the listening WorkProcess */
						SUMA_register_workproc(SUMA_niml_workproc, elvis);

						/* register a call for sending the surface to afni (SetAfniSurf)*/
						/*fprintf(SUMA_STDERR,"Notifying Afni of New surface...\n");*/
						sprintf(tmpcom,"SetAfniSurf");
						SUMA_RegisterCommand (Command, SUMA_COMMAND_DELIMITER, SUMA_COMMAND_TERMINATOR, tmpcom, NOPE);/* form surface nel */
						break;
					}	else {
						fprintf(SUMA_STDOUT,"Disconnecting from afni.\n");
						/* remove the listening workprocess) */
						SUMA_remove_workproc( SUMA_niml_workproc );
						/* close the stream */
						NI_stream_close(SUMAg_cSV->ns);
						SUMAg_cSV->ns = NULL;
						break;
					}
	
			case SE_SetAfniSurf:
				{ int _ID;
				
				/* send to afni the list of inherently mappable surfaces */
				/* No surfaces are sent twice because there should not be duplicate 
				inherently mappable surfaces in SUMAg_DOv */
				for (ii=0; ii<SUMAg_cSV->N_DO; ++ii) {
					if (SUMA_isSO(SUMAg_DOv[SUMAg_cSV->ShowDO[ii]])) {
						SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SUMAg_cSV->ShowDO[ii]].OP);
						if (!SUMA_isINHmappable(SO)) {
							if (!SO->MapRef_idcode_str) {
								/* not mappable */
								fprintf(SUMA_STDERR, "Warning %s: Surface %s is not mappable. Will not send to afni\n",\
				 					FuncName, SO->idcode_str);
								continue;
							}
							_ID = SUMA_findDO(SO->MapRef_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
							if (_ID < -1) {
								/* not found */
								fprintf(SUMA_STDERR, "Warning %s: Map reference %s not found.\n",\
								 FuncName, SO->MapRef_idcode_str);
								continue;
							}
							SO = (SUMA_SurfaceObject *)(SUMAg_DOv[_ID].OP);
							
						}
							nel = SUMA_makeNI_SurfIXYZ (SO);
							if (!nel) {
								fprintf(SUMA_STDERR,"Error %s: SUMA_makeNI_SurfIXYZ failed\n", FuncName);
								break;
							}
							/* send surface nel */
							fprintf(SUMA_STDERR,"%s: Sending SURF_iXYZ nel...\n ", FuncName) ;
      					nn = NI_write_element( SUMAg_cSV->ns , nel , NI_BINARY_MODE ) ;

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
      					nn = NI_write_element( SUMAg_cSV->ns , nel , NI_BINARY_MODE ) ;

      					if( nn < 0 ){
         					  fprintf(SUMA_STDERR,"Error %s: NI_write_element failed\n", FuncName);
      					}
							NI_free_element(nel);
							
							nel = NULL;
					}
				}
				break;
				}
			case SE_ToggleShowSelectedNode:
				for (ii=0; ii<SUMAg_cSV->N_DO; ++ii) {
					if (SUMA_isSO(SUMAg_DOv[SUMAg_cSV->ShowDO[ii]])) {
						SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SUMAg_cSV->ShowDO[ii]].OP);
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
				SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SUMAg_cSV->Focus_SO_ID].OP);
				SO->SelectedNode = EngineData->i;
				break;
				
			case SE_ToggleShowSelectedFaceSet:
				for (ii=0; ii<SUMAg_cSV->N_DO; ++ii) {
					if (SUMA_isSO(SUMAg_DOv[SUMAg_cSV->ShowDO[ii]])) {
						SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SUMAg_cSV->ShowDO[ii]].OP);
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
				SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SUMAg_cSV->Focus_SO_ID].OP);
				SO->FaceSetMarker->n0[0] = SO->NodeList[SO->FaceSetList[EngineData->i][0]][0];
				SO->FaceSetMarker->n0[1] = SO->NodeList[SO->FaceSetList[EngineData->i][0]][1];
				SO->FaceSetMarker->n0[2] = SO->NodeList[SO->FaceSetList[EngineData->i][0]][2];
				SO->FaceSetMarker->n1[0] = SO->NodeList[SO->FaceSetList[EngineData->i][1]][0];
				SO->FaceSetMarker->n1[1] = SO->NodeList[SO->FaceSetList[EngineData->i][1]][1];
				SO->FaceSetMarker->n1[2] = SO->NodeList[SO->FaceSetList[EngineData->i][1]][2];
				SO->FaceSetMarker->n2[0] = SO->NodeList[SO->FaceSetList[EngineData->i][2]][0];
				SO->FaceSetMarker->n2[1] = SO->NodeList[SO->FaceSetList[EngineData->i][2]][1];
				SO->FaceSetMarker->n2[2] = SO->NodeList[SO->FaceSetList[EngineData->i][2]][2];
				SO->FaceSetMarker->NormVect[0] = SO->FaceNormList[EngineData->i][0];
				SO->FaceSetMarker->NormVect[1] = SO->FaceNormList[EngineData->i][1];
				SO->FaceSetMarker->NormVect[2] = SO->FaceNormList[EngineData->i][2];
				
				SO->SelectedFaceSet = EngineData->i;
				break;
				
			case SE_ToggleCrossHair:
				SUMAg_cSV->ShowCrossHair = !SUMAg_cSV->ShowCrossHair;
				break;
				
			case SE_SetCrossHair:
				/* Expects Cross Hair coordinates in fv3 */
				if (EngineData->fv3_Dest != NextComCode) {
					fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
					break;
				} 
				SUMAg_cSV->Ch->c[0] = EngineData->fv3[0]; SUMAg_cSV->Ch->c[1]= EngineData->fv3[1]; SUMAg_cSV->Ch->c[2]= EngineData->fv3[2];
				
				if (SUMAg_cSV->TalkToAfni && EngineData->fv3_Source != SES_Afni) {
					/*fprintf(SUMA_STDERR,"Notifying Afni of CrossHair XYZ\n");*/
					/* register a call to SetAfniCrossHair */
					sprintf(tmpcom,"SetAfniCrossHair");
					SUMA_RegisterCommand (Command, SUMA_COMMAND_DELIMITER, SUMA_COMMAND_TERMINATOR, tmpcom, NOPE);
				} 
				break;
			
			case SE_BindCrossHair:
				/* expects SurfaceID to bind cross hair to*/
				if (EngineData->iv3_Dest != NextComCode) {
					fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
					break;
				} 
				SUMAg_cSV->Ch->SurfaceID = EngineData->iv3[0];
				SUMAg_cSV->Ch->NodeID = EngineData->iv3[1];
				break;
							
			case SE_SetAfniCrossHair:
				/* sends the current cross hair to afni */
				/* form nel */
				nel = SUMA_makeNI_CrossHair (SUMAg_cSV);
				if (!nel) {
					fprintf(SUMA_STDERR,"Error %s: SUMA_makeNI_SurfIXYZ failed\n", FuncName);
					break;
					}
				/*send it to afni */
				/*fprintf(SUMA_STDERR,"Sending cross hair nel ") ;*/
      		nn = NI_write_element( SUMAg_cSV->ns , nel , NI_TEXT_MODE ) ;
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
				CurrentDistance = sqrt((SUMAg_cSV->ViewFrom[0]-SUMAg_cSV->ViewCenter[0])*(SUMAg_cSV->ViewFrom[0]-SUMAg_cSV->ViewCenter[0]) +\
												(SUMAg_cSV->ViewFrom[1]-SUMAg_cSV->ViewCenter[1])*(SUMAg_cSV->ViewFrom[1]-SUMAg_cSV->ViewCenter[1]) +\
												(SUMAg_cSV->ViewFrom[2]-SUMAg_cSV->ViewCenter[2])*(SUMAg_cSV->ViewFrom[2]-SUMAg_cSV->ViewCenter[2]));
				
				/* set the ViewCenter Value to that of the node's XYZ*/
				SUMAg_cSV->ViewCenter[0] = EngineData->fv15[0];
				SUMAg_cSV->ViewCenter[1] = EngineData->fv15[1]; 
				SUMAg_cSV->ViewCenter[2] = EngineData->fv15[2];
				
				/* obtain the LookFrom point based on CurrentDistance and the normal vector */
				fm2_3 = SUMA_Point_At_Distance(&(EngineData->fv15[3]), SUMAg_cSV->ViewCenter, CurrentDistance);
				if (fm2_3 == NULL) {
					fprintf(SUMA_STDOUT,"Error %s: SUMA_Point_At_Distance failed.\n", FuncName);
					break;
				}
				fprintf(SUMA_STDOUT,"\nPoints: %f %f %f\n%f %f %f\n", \
					fm2_3[0][0], fm2_3[0][1], fm2_3[0][2], \
					fm2_3[1][0], fm2_3[1][1], fm2_3[1][2]);
				
				SUMAg_cSV->ViewFrom[0] = fm2_3[0][0]; 
				SUMAg_cSV->ViewFrom[1] = fm2_3[0][1]; 
				SUMAg_cSV->ViewFrom[2] = fm2_3[0][2]; 
				
				/* fm2_3 not needed anymore */
				SUMA_free2D((char **)fm2_3, 2);
				
				gluLookAt (SUMAg_cSV->ViewFrom[0], SUMAg_cSV->ViewFrom[1], SUMAg_cSV->ViewFrom[2], SUMAg_cSV->ViewCenter[0], SUMAg_cSV->ViewCenter[1], SUMAg_cSV->ViewCenter[2], SUMAg_cSV->ViewCamUp[0], SUMAg_cSV->ViewCamUp[1], SUMAg_cSV->ViewCamUp[2]);
				}
				
				break;
			case SE_SetLookFrom:
				/* expects a center XYZ in EngineData->fv3[0 .. 2] */
				if (EngineData->fv3_Dest != NextComCode) {
					fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
					break;
				} 
				/* set the LookFrom option */
				SUMAg_cSV->ViewFrom[0] = EngineData->fv3[0];
				SUMAg_cSV->ViewFrom[1] = EngineData->fv3[1]; 
				SUMAg_cSV->ViewFrom[2] = EngineData->fv3[2];
				gluLookAt (SUMAg_cSV->ViewFrom[0], SUMAg_cSV->ViewFrom[1], SUMAg_cSV->ViewFrom[2], SUMAg_cSV->ViewCenter[0], SUMAg_cSV->ViewCenter[1], SUMAg_cSV->ViewCenter[2], SUMAg_cSV->ViewCamUp[0], SUMAg_cSV->ViewCamUp[1], SUMAg_cSV->ViewCamUp[2]);
				break;

			case SE_Redisplay:
				/*post a redisplay */
				/*fprintf (SUMA_STDOUT,"%s: Redisplay ...", FuncName);*/
				postRedisplay();
				/*fprintf (SUMA_STDOUT,"%s: OK\n", FuncName);*/
				break;
			
			case SE_Home:
				SUMAg_cSV->translateVec[0]=0; SUMAg_cSV->translateVec[1]=0;
				glMatrixMode(GL_PROJECTION);
				SUMAg_cSV->FOV = FOV_INITIAL;	/* reset the zooming */
				SUMAg_cSV->ViewFrom[0] = SUMAg_cSV->ViewFromOrig[0];
				SUMAg_cSV->ViewFrom[1] = SUMAg_cSV->ViewFromOrig[1];
				SUMAg_cSV->ViewFrom[2] = SUMAg_cSV->ViewFromOrig[2];
				SUMAg_cSV->ViewCenter[0] = SUMAg_cSV->ViewCenterOrig[0];
				SUMAg_cSV->ViewCenter[1] = SUMAg_cSV->ViewCenterOrig[1];
				SUMAg_cSV->ViewCenter[2] = SUMAg_cSV->ViewCenterOrig[2];
				
				glMatrixMode(GL_MODELVIEW);
   			glLoadIdentity();
   			gluLookAt (SUMAg_cSV->ViewFrom[0], SUMAg_cSV->ViewFrom[1], SUMAg_cSV->ViewFrom[2], SUMAg_cSV->ViewCenter[0], SUMAg_cSV->ViewCenter[1], SUMAg_cSV->ViewCenter[2], SUMAg_cSV->ViewCamUp[0], SUMAg_cSV->ViewCamUp[1], SUMAg_cSV->ViewCamUp[2]);
				break;

			case SE_SetNodeColor:
				/* expects a four-columned fm in EngineData->fm[0 .. N][0..3] 
				[Node Index] [R] [G] [B] RGB between 0 and 1*/
				if (EngineData->fm_Dest != NextComCode) {
					fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
					break;
				} 
				SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SUMAg_cSV->Focus_SO_ID].OP);
				for (i=0; i < EngineData->N_rows; ++i){
					ii = (int)(EngineData->fm[i][0]);
					SO->glar_ColorList[4*ii] = EngineData->fm[i][1];
					SO->glar_ColorList[4*ii+1] = EngineData->fm[i][2];
					SO->glar_ColorList[4*ii+2] = EngineData->fm[i][3];
					SO->glar_ColorList[4*ii+3] = 0.5;
				}
				break;
				
			case SE_FlipLight0Pos:
				SUMAg_cSV->light0_position[2] *= -1;
				glLightfv(GL_LIGHT0, GL_POSITION, SUMAg_cSV->light0_position);
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
					
					SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SUMAg_cSV->Focus_SO_ID].OP);
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
							EngineData->fv15[0] = SO->NodeList[IB.IsIn[it]][0];
							EngineData->fv15[1] = SO->NodeList[IB.IsIn[it]][1];
							EngineData->fv15[2] = SO->NodeList[IB.IsIn[it]][2];
							EngineData->fv15[3] = SO->NodeNormList[IB.IsIn[it]][0];
							EngineData->fv15[4] = SO->NodeNormList[IB.IsIn[it]][1];
							EngineData->fv15[5] = SO->NodeNormList[IB.IsIn[it]][2];
						#endif
						/* Color the nodes*/
							fm = (float **)SUMA_allocate2D(IB.nIsIn, 4, sizeof(float));
							if (fm == NULL) {
								fprintf(SUMA_STDERR,"Error %s: Could not allocate for fm.\n", FuncName);
								break;
							}
							for (i=0; i < IB.nIsIn; ++i) {
								 /*fprintf (SUMA_STDOUT,"\t[%d] %f %f %f\n", IB.IsIn[i] ,\
						 						 SO->NodeList[IB.IsIn[i]][0], SO->NodeList[IB.IsIn[i]][1], SO->NodeList[IB.IsIn[i]][2]);*/
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
							if (!SUMA_Engine (Command, EngineData)) {
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
					
					SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SUMAg_cSV->Focus_SO_ID].OP);
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
						fv15[0] = SO->NodeList[IB.IsIn[it]][0];
						fv15[1] = SO->NodeList[IB.IsIn[it]][1];
						fv15[2] = SO->NodeList[IB.IsIn[it]][2];
						fv15[3] = SO->NodeNormList[IB.IsIn[it]][0];
						fv15[4] = SO->NodeNormList[IB.IsIn[it]][1];
						fv15[5] = SO->NodeNormList[IB.IsIn[it]][2];
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
				if (!SUMA_mattoquat (EngineData->fm, SUMAg_cSV->currentQuat))
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
	
	/* If you get here, all is well */
	return (YUP);
}

/*------------------------------------------------------------------*/
/*! Make a NIML data element for a NI surface element IXYZ
	\param SO (SUMA_SurfaceObject *) surface object to turn to NI
   \ret  NULL if you input stupid values, NI if you input smart values
--------------------------------------------------------------------*/

NI_element * SUMA_makeNI_SurfIXYZ (SUMA_SurfaceObject *SO)
{
	char FuncName[100];
	NI_element *nel;
	int *ic, ii;
	float *xc, *yc, *zc;
	
	sprintf(FuncName,"SUMA_makeNI_SurfIXYZ");
	
	if (SO == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Null SO.\n", FuncName);
		return (NULL);
	}
	if (SO->N_Node <= 0) {
		fprintf(SUMA_STDERR,"Error %s: No nodes in SO.\n", FuncName);
		return (NULL);
	}
	
	/* make a new data element, to be filled by columns */
   nel = NI_new_data_element( "SUMA_ixyz" , SO->N_Node) ;
	
   /* make the columns to be put in the element */
   ic = (int *)   malloc( sizeof(int)   * SO->N_Node ) ;
   xc = (float *) malloc( sizeof(float) * SO->N_Node ) ;
   yc = (float *) malloc( sizeof(float) * SO->N_Node ) ;
   zc = (float *) malloc( sizeof(float) * SO->N_Node ) ;

	if (!nel || !ic || !xc || !yc || !zc) {
		fprintf(SUMA_STDERR,"Error %s: Failed to allocate for nel, ic, xc, yc or zc.\n", FuncName);
		return (NULL);
	}
	

   /* load the columns from the struct array */

   for( ii=0 ; ii < SO->N_Node ; ii++ ){
      ic[ii] = ii;
      xc[ii] = SO->NodeList[ii][0];
      yc[ii] = SO->NodeList[ii][1];
      zc[ii] = SO->NodeList[ii][2];
   }

   /* put columns into element */

   NI_add_column( nel , NI_INT   , ic ) ; free(ic) ;
   NI_add_column( nel , NI_FLOAT , xc ) ; free(xc) ;
   NI_add_column( nel , NI_FLOAT , yc ) ; free(yc) ;
   NI_add_column( nel , NI_FLOAT , zc ) ; free(zc) ;

	NI_set_attribute (nel, "volume_idcode", SO->VolPar->idcode_str);
	NI_set_attribute (nel, "surface_idcode", SO->idcode_str);
   return (nel);
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
	int  ii;
	int *I, *J, *K;
	
	
	if (SO == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Null SO.\n", FuncName);
		return (NULL);
	}
	if (SO->N_FaceSet <= 0) {
		fprintf(SUMA_STDERR,"Error %s: No FaceSets in SO.\n", FuncName);
		return (NULL);
	}
	
	/* make a new data element, to be filled by columns */
   nel = NI_new_data_element( "SUMA_ijk" , SO->N_FaceSet) ;
	
   /* make the columns to be put in the element */
   I = (int *)   malloc( sizeof(int)   * SO->N_FaceSet ) ;
   J = (int *) malloc( sizeof(int) * SO->N_FaceSet ) ;
   K = (int *) malloc( sizeof(int) * SO->N_FaceSet ) ;

	if (!nel || !I || !J || !K ) {
		fprintf(SUMA_STDERR,"Error %s: Failed to allocate for nel, I, J or K.\n", FuncName);
		return (NULL);
	}
	

   /* load the columns from the struct array */

   for( ii=0 ; ii < SO->N_FaceSet ; ii++ ){
      I[ii] = SO->FaceSetList[ii][0];
      J[ii] = SO->FaceSetList[ii][1];
      K[ii] = SO->FaceSetList[ii][2];
   }

   /* put columns into element */

   NI_add_column( nel , NI_INT   , I ) ; free(I) ;
   NI_add_column( nel , NI_INT   , J ) ; free(J) ;
   NI_add_column( nel , NI_INT   , K ) ; free(K) ;

	NI_set_attribute (nel, "volume_idcode", SO->VolPar->idcode_str);
	NI_set_attribute (nel, "surface_idcode", SO->idcode_str);
   return (nel);
}

SUMA_Boolean SUMA_nel_stdout (NI_element *nel) 
{
	NI_stream nstdout;
	nstdout = NI_stream_open( "fd:1","w");
	if( nstdout == NULL ){ fprintf(SUMA_STDERR,"Can't open fd:1\n"); return(NOPE); }
	fprintf (stdout, "\n----------------------------nel stdout begin-------------------\n");
	NI_write_element( nstdout , nel , NI_TEXT_MODE ) ;
	fprintf (stdout, "----------------------------nel stdout end  -------------------\n");
	NI_stream_close(nstdout);
	return(YUP);
}

NI_element * SUMA_makeNI_CrossHair (SUMA_SurfaceViewer *sv)
{
	static char FuncName[]={"SUMA_makeNI_CrossHair"};
	NI_element *nel;
	float *XYZmap;
	int I_C = -1;
	SUMA_SurfaceObject *SO;
	
	if (sv == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Null sv.\n", FuncName);
		return (NULL);
	}
	if (sv->Ch == NULL) {
		fprintf(SUMA_STDERR,"Error %s: NULL Ch.\n", FuncName);
		return (NULL);
	}

	SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Focus_SO_ID].OP);
	I_C = SO->SelectedNode;
	XYZmap = SUMA_XYZ_XYZmap (sv->Ch->c, SO, SUMAg_DOv, SUMAg_N_DOv, &I_C);
	
	if (XYZmap == NULL){
		fprintf(SUMA_STDERR,"%s: Linkage is not posible, using current XYZ\n", FuncName);
		XYZmap = (float *)calloc (3, sizeof(float));
		if (XYZmap == NULL) {
			fprintf (SUMA_STDERR, "Error %s: Give me a break !\n", FuncName);
			return (NULL); 
		}
		XYZmap[0] = sv->Ch->c[0];
		XYZmap[1] = sv->Ch->c[1];
		XYZmap[2] = sv->Ch->c[2];
	}
	
	/* make a new data element */
   nel = NI_new_data_element( "SUMA_crosshair_xyz" , 3) ;
	
	if (!nel) {
		fprintf(SUMA_STDERR,"Error %s: Failed to allocate for nel\n", FuncName);
		return (NULL);
	}
	
	NI_add_column( nel , NI_FLOAT , XYZmap );
	
	if (XYZmap) free(XYZmap);
	return (nel);
}

/*!
	ans = SUMA_CanTalkToAfni (sv, dov);
	determines if Surface Viewer is allowed to talk to afni
	\param sv (SUMA_SurfaceViewer *) the surface viewer structure
	\param dov (SUMA_DO *) the Displayable Objects vector (accessible to sv)
	\ret ans (SUMA_Boolean) NOPE if any SO shown in the viewer has either 
		MapRef_idcode_str == NULL || VolPar == NULL
*/

SUMA_Boolean SUMA_CanTalkToAfni (SUMA_SurfaceViewer *sv, SUMA_DO *dov)
{
	static char FuncName[]={"SUMA_CanTalkToAfni"};
	int i;
	SUMA_SurfaceObject *SO;
	
	for (i=0; i< sv->N_DO; ++i) {
		if (SUMA_isSO(dov[sv->ShowDO[i]])) {
			SO = (SUMA_SurfaceObject *)(dov[sv->ShowDO[i]].OP);
			if (SO->MapRef_idcode_str == NULL || SO->VolPar == NULL) {
				return (NOPE);
			}
		} 
	}
	return (YUP);
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
	int i, k = 0;
	for (i=0; i< sv->N_DO; ++i) {
		if (SUMA_isSO(dov[sv->ShowDO[i]])) {
			if (SO_IDs != NULL) SO_IDs[k] = sv->ShowDO[i];
			++k;
		}
	}
	return (k);
}

/*! 
	nxtState = SUMA_NextState(sv);

	get the next Viewing State available in sv
	\param sv (SUMA_SurfaceViewer *) pointer to surface viewer structure 
	\ret nxtState (int) the index into sv->VSv of the next state
		-1 if there is trouble
*/
int SUMA_NextState(SUMA_SurfaceViewer *sv)
{
	int inxt, icur;
	static char FuncName[] = {"SUMA_NextState"};
	
	icur = SUMA_WhichState (sv->State, sv);
	if (icur < 0) {
		fprintf(SUMA_STDERR,"Error %s: SUMA_WhichState failed.\n", FuncName);
		return (-1);
	} else {
		return((icur + 1) % sv->N_VSv);
	}
	
	return (-1);
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
	int icur, icheck, ncheck;
	static char FuncName[] = {"SUMA_NextSO"};
	if (SOnxt != NULL) {
		fprintf(SUMA_STDERR,"Error %s: SOnxt should be null when you call this function.\n", FuncName);
		return (-1);
	}
	if (n_dov < 1) {
		fprintf(SUMA_STDERR,"Error %s: dov contains no elements.\n", FuncName);
		return (-1);
	}
	icur = SUMA_findDO (idcode, dov, n_dov);
	if (icur < 0) {
		fprintf (SUMA_STDERR,"Error %s: idcode not found in dov.\n", FuncName);
		return (-1);
	}
	
	ncheck = 0;
	icheck = icur;
	while (ncheck < n_dov) {
		icheck = (icheck + 1) % n_dov;
		/*fprintf(SUMA_STDERR,"%s: Checking %d\n", FuncName, icheck);*/
		if (SUMA_isSO(dov[icheck])) {
			/*fprintf(SUMA_STDERR,"%s: Settling on %d\n", FuncName, icheck);*/
			SOnxt = (SUMA_SurfaceObject *)dov[icheck].OP;
			return (icheck);
		}
		++ncheck;
	}
	/* should not get here */
	return (-1);
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
	
	/* unregister the current surface from ShowDO */
	/*fprintf(SUMA_STDERR,"%s: Unregistering DOv[%d]...\n", FuncName, SOcurID);*/
	if (!SUMA_UnRegisterDO(SOcurID, sv)) {
		fprintf(SUMA_STDERR,"Error %s: Failed to UnRegisterDO.\n", FuncName);
		return (NOPE);
	}

	/* set the focus ID to the current surface */
	sv->Focus_SO_ID = SOnxtID;

	/* register the new surface in ShowDO */
	/*fprintf(SUMA_STDERR,"%s: Registering DOv[%d]...\n", FuncName, sv->Focus_SO_ID); */
	if (!SUMA_RegisterDO(sv->Focus_SO_ID, sv)) {
		fprintf(SUMA_STDERR,"Error %s: Failed to RegisterDO.\n", FuncName);
		return (NOPE);
	}

	/* modify the rotation center */
	if (!SUMA_UpdateRotaCenter(sv, dov, N_dov)) {
		fprintf (SUMA_STDERR,"Error %s: Failed to update center of rotation", FuncName);
		return (NOPE);
	}
	
	/* set the viewing points */
	if (!SUMA_UpdateViewPoint(sv, dov, N_dov)) {
		fprintf (SUMA_STDERR,"Error %s: Failed to update view point", FuncName);
		return (NOPE);
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
	if (!SUMA_Engine (CommString, &ED)) {
		fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
	}

	
	/* take care of the cross hair's XYZ */

	/* to do elsewhere */
	/* when a cross hair needs to be communicated, you must use the MapRef_idcode_str surface and not the Focus_Surface */
	return (YUP);
}

/*! 
	ans = SUMA_SwitchState (dov, N_dov, sv, nxtstateID);
	
	Replaces one viewing state with another

*/
SUMA_Boolean SUMA_SwitchState (SUMA_DO *dov, int N_dov, SUMA_SurfaceViewer *sv, int nxtstateID)
{
	static char FuncName[]={"SUMA_SwitchState"};
	SUMA_Axis *EyeAxis;
	int EyeAxis_ID, I_C, OverInd;
	char CommString[100];
	SUMA_EngineData ED;
	int curstateID, i, j, jmax, prec_ID;
	SUMA_SurfaceObject *SO_nxt, *SO_prec;
	float *XYZ, *XYZmap;
	
	XYZ = NULL;
	XYZmap = NULL;
	
	curstateID = SUMA_WhichState(sv->State, sv);
	
	/* unregister all the surfaces for the current view */
	/*fprintf(SUMA_STDERR,"%s: Unregistering \n", FuncName);*/
	for (i=0; i<sv->VSv[curstateID].N_MembSOs; ++i) {
		if (!SUMA_UnRegisterDO(sv->VSv[curstateID].MembSOs[i], sv)) {
			fprintf(SUMA_STDERR,"Error %s: Failed to UnRegisterDO.\n", FuncName);
			return (NOPE);
		}
	}
	
	/* register all the surfaces from the next view */
	/*fprintf(SUMA_STDERR,"%s: Registering DOv...\n", FuncName); */
	for (i=0; i<sv->VSv[nxtstateID].N_MembSOs; ++i) {
		if (!SUMA_RegisterDO(sv->VSv[nxtstateID].MembSOs[i], sv)) {
			fprintf(SUMA_STDERR,"Error %s: Failed to RegisterDO.\n", FuncName);
			return (NOPE);
		}
	}

	#ifdef OLD_METH
		/* update colors of the nodes on the new surfaces using the latest map*/
		/*fprintf(SUMA_STDERR,"%s: Carrying node settings ...\n", FuncName);*/
		for (i=0; i<sv->VSv[nxtstateID].N_MembSOs; ++i) {
			/* next surface being checked */
			SO_nxt = (SUMA_SurfaceObject *)(dov[sv->VSv[nxtstateID].MembSOs[i]].OP);

			/* find out surface to borrow from in the previous state */
			prec_ID = SUMA_MapRefRelative (sv->VSv[nxtstateID].MembSOs[i], sv->VSv[curstateID].MembSOs, sv->VSv[curstateID].N_MembSOs, dov);

			if (prec_ID < 0) {
				/* no precursors found, notify used */
				fprintf(SUMA_STDERR, "Warning %s: No precursors found for surface %d. Colors will not be current.\n",\
				 FuncName, sv->VSv[nxtstateID].MembSOs[i]);
			} else {
				SO_prec = (SUMA_SurfaceObject *)(dov[prec_ID].OP);

				/* check for risk of node inconsistencies */
				if (SO_prec->N_Node == SO_nxt->N_Node) {
					/* copy the colors to the new nodes */
					/*fprintf(SUMA_STDERR, "%s: Copying colors ...", FuncName);*/
					jmax = SO_nxt->N_Node * 4;
					for (j=0; j<jmax; ++j) {
						SO_nxt->glar_ColorList[j] = SO_prec->glar_ColorList[j];
					}			
				} else {
					if (SO_prec->N_Node > SO_nxt->N_Node) {
						fprintf(SUMA_STDERR, "\aWarning %s: Mismatch (%d) in node numers. More nodes in precursor surface.\n", \
							FuncName, SO_prec->N_Node-SO_nxt->N_Node);
						jmax = SO_nxt->N_Node * 4;
						for (j=0; j<jmax; ++j) {
							SO_nxt->glar_ColorList[j] = SO_prec->glar_ColorList[j];
						}		
					} else {
						fprintf(SUMA_STDERR, "\aWarning %s: Mismatch (%d) in node numers. Less nodes in precursor surface. Some nodes may get no assignment\n", \
							FuncName, SO_nxt->N_Node-SO_prec->N_Node);
						jmax = SO_prec->N_Node * 4;
						for (j=0; j<jmax; ++j) {
							SO_nxt->glar_ColorList[j] = SO_prec->glar_ColorList[j];
						}	
					}	
				}
			}
		}
	#else 
		/* if no coloroverlay exists, link to MapReference surface, if possible */
		for (i=0; i<sv->VSv[nxtstateID].N_MembSOs; ++i) {
			/* next surface being checked */
			SO_nxt = (SUMA_SurfaceObject *)(dov[sv->VSv[nxtstateID].MembSOs[i]].OP);

			/* Get the Mapping Reference surface, that's the precursor*/
			{
				if (!SO_nxt->MapRef_idcode_str) {
				 prec_ID = -1;
				}else {
					prec_ID = SUMA_findDO(SO_nxt->MapRef_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
				}
			}	
			if (prec_ID < -1) {
				/* no precursors found, notify used */
				fprintf(SUMA_STDERR, "Warning %s: No precursors found for surface %d. Colors will not be current.\n",\
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
						/*fprintf (SUMA_STDOUT,"%s: Overlay plane %s not found, creating the link.\n", FuncName, SO_prec->Overlays[j]->Name);*/
						SO_nxt->Overlays_Inode[SO_nxt->N_Overlays] = SUMA_CreateInodeLink (SO_nxt->Overlays_Inode[SO_nxt->N_Overlays], SO_prec->Overlays_Inode[j]);
						if (!SO_nxt->Overlays_Inode[SO_nxt->N_Overlays]) {
							fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInodeLink\n", FuncName);
							return (NOPE);
						}
						/* now copy the actual overlay plane pointer */
						SO_nxt->Overlays[SO_nxt->N_Overlays] = SO_prec->Overlays[j];
						/*increment the number of overlay planes */
						++SO_nxt->N_Overlays;
					} else {
						/* plane found, do nothing */
						/*fprintf (SUMA_STDOUT,"%s: Overlay plane %s found. Index#%d\n.", FuncName, SO_prec->Overlays[j]->Name, OverInd);*/
					}
				}
				
				#if 0
				/* just copy colors, no need to remix */
					jmax = SO_nxt->N_Node * 4;
					for (j=0; j< jmax; ++j) {
						SO_nxt->glar_ColorList[j] = SO_prec->glar_ColorList[j];
					}
				#endif
				
				if (SO_prec->N_Node > SO_nxt->N_Node) {/* More in prec */
					/* just warn */
					fprintf(SUMA_STDERR, "Warning %s: More nodes (%d) in precursor surface. \nProceeding ...\n", FuncName, SO_prec->N_Node - SO_nxt->N_Node);
				}/* More in prec */ 
								
			} /* > or equal number of nodes */ else { /* less in prec */
				fprintf(SUMA_STDERR, "Warning %s: More nodes (%d) in next surface. Colors ware not carried through.\n", FuncName, SO_nxt->N_Node - SO_prec->N_Node);
			}
			
			/* Here you need to remix the colors */
			if (!SUMA_Overlays_2_GLCOLAR4(SO_nxt->Overlays, SO_nxt->N_Overlays, SO_nxt->glar_ColorList, SO_nxt->N_Node, SO_nxt->Back_Modfact)) {
				fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Overlays_2_GLCOLAR4.\n", FuncName);
				return (NOPE);
			}
				
		}
	#endif	
	
	/* link the selected nodes and facesets, if possible */
	/*fprintf(SUMA_STDERR, "%s: Linking selected nodes  ...\n", FuncName);*/
	for (i=0; i<sv->VSv[nxtstateID].N_MembSOs; ++i) {
		/* next surface being checked */
		SO_nxt = (SUMA_SurfaceObject *)(dov[sv->VSv[nxtstateID].MembSOs[i]].OP);
		
		/* find out surface to borrow from in the previous state */
		prec_ID = SUMA_MapRefRelative (sv->VSv[nxtstateID].MembSOs[i], sv->VSv[curstateID].MembSOs, sv->VSv[curstateID].N_MembSOs, dov);
		
		/* DO NOT LINK SELECTED FACESETS SINCE THOSE MAY CHANGE FROM ONE SURF TO THE NEXT */
		if (prec_ID < -1) {
			/* no precursors found, notify used */
			fprintf(SUMA_STDERR, "Warning %s: No precursors found for surface %d. Selected node will not be current.\n",\
			 FuncName, sv->VSv[nxtstateID].MembSOs[i]);
		} else {
			/* check for risk of node inconsistencies */
			if (SO_prec->N_Node == SO_nxt->N_Node) {
				SO_prec = (SUMA_SurfaceObject *)(dov[prec_ID].OP);
				SO_nxt->SelectedNode = SO_prec->SelectedNode;
			} else {
				/* Not equiped to deal with this option yet */
				fprintf(SUMA_STDERR, "Warning %s: Mismatch in node numers, not ready to deal with this yet.\n", FuncName);
			}
		}
	}
	
	/* Bind the cross hair to a reasonable surface, if possible */
	if (sv->Ch->SurfaceID >= 0) {
		
		/*fprintf(SUMA_STDERR, "%s: Linking Cross Hair via SurfaceID...\n", FuncName);*/
		j = SUMA_MapRefRelative (sv->Ch->SurfaceID, sv->VSv[nxtstateID].MembSOs, sv->VSv[nxtstateID].N_MembSOs, dov);
		/*fprintf(SUMA_STDERR, "%s: Cross Hair's New SurfaceID = %d\n", FuncName, j );*/
		
		/* set the XYZ of the cross hair based on the coordinates of the upcoming surface, if possible */
		if (j >= 0) {
			SO_nxt = (SUMA_SurfaceObject *)(dov[j].OP);
			if (sv->Ch->NodeID >= 0) {
				fprintf(SUMA_STDERR, "%s: Using NodeID for link.\n", FuncName);
				sv->Ch->c[0] = SO_nxt->NodeList[sv->Ch->NodeID][0];
				sv->Ch->c[1] = SO_nxt->NodeList[sv->Ch->NodeID][1];
				sv->Ch->c[2] = SO_nxt->NodeList[sv->Ch->NodeID][2];
			} else {
				/* no node associated with cross hair, use XYZ */
				fprintf(SUMA_STDERR, "%s: Using XYZ for link.\n", FuncName);
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
				if (XYZ) free (XYZ);
				if (XYZmap) free(XYZmap);
			}
		} else {
			fprintf(SUMA_STDERR, "%s: No relatives between states. CrossHair location will not correspond between states\n", FuncName); 
		}
	 	sv->Ch->SurfaceID = j;
		/*fprintf(SUMA_STDERR, "%s: Linking Cross Hair Via NodeID Done.\n", FuncName);*/
	}
	


	/* switch the state accordingly */
	sv->State =  sv->VSv[nxtstateID].Name;
	
	/* set the focus ID to the first surface in the next view   */
	sv->Focus_SO_ID = sv->VSv[nxtstateID].MembSOs[0];


	/* modify the rotation center */
	if (!SUMA_UpdateRotaCenter(sv, dov, N_dov)) {
		fprintf (SUMA_STDERR,"Error %s: Failed to update center of rotation", FuncName);
		return (NOPE);
	}
	
	/* set the viewing points */
	if (!SUMA_UpdateViewPoint(sv, dov, N_dov)) {
		fprintf (SUMA_STDERR,"Error %s: Failed to update view point", FuncName);
		return (NOPE);
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
	if (!SUMA_Engine (CommString, &ED)) {
		fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
	}

	
	/* take care of the cross hair's XYZ */

	/* to do elsewhere */
	/* when a cross hair needs to be communicated, you must use the MapRef_idcode_str surface and not the Focus_Surface */
	return (YUP);
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
	int i, k = -1, cnt = 0;
	static char FuncName[]={"SUMA_GetEyeAxis"};
	SUMA_Axis *AO;
	
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
		return (-1);
	}
	
	return (k);
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
	int iclosest;
	SUMA_SurfaceObject *SOmap;
	int SOmapID;

	/* allocate for return */
	XYZmap = (float *)calloc (3, sizeof(float));
	if (XYZmap == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Could not allocate for XYZmap.\n", FuncName);
		return (NULL);
	} 
	/* if surface is Inherently mappable, do the obivious */
	if (SUMA_isINHmappable(SO)){
		fprintf(SUMA_STDERR,"%s: Surface is inherrently mappable. XYZmap = XYZ.\n", FuncName);
		SUMA_COPY_VEC (XYZ, XYZmap, 3, float, float);
		return (XYZmap);	
	}
	/* if surface is not Inherrently mappable, do the deed */
	if (!SUMA_ismappable(SO)){
		fprintf(SUMA_STDERR,"%s: Surface is NOT mappable, returning NULL.\n", FuncName);
		free (XYZmap);
		return (NULL);
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
				fprintf (SUMA_STDERR,"%s: %d nodes (out of %d) found in box\n",FuncName, IB.nIsIn, SO->N_Node);

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
					fprintf (SUMA_STDERR,"%s: Non node was close enough to XYZ, no linkage possible\n", FuncName);
					free (XYZmap);
					return (NULL);
				}
				/* store iclosest for lazy user */
				*I_C = iclosest;
			}
	} else { 
		iclosest = *I_C;
	}
	fprintf (SUMA_STDERR,"%s: Node identified for linking purposes is %d\n", FuncName, *I_C);
	/* find the SO that is the Mappable cahuna */
	SOmapID = SUMA_findDO(SO->MapRef_idcode_str, dov, N_dov);
	if (SOmapID < 0) {
		fprintf (SUMA_STDERR,"%s: Failed in SUMA_findDO This should not happen.\n", FuncName);
		free (XYZmap);
		return (NULL);
	}

	SOmap = (SUMA_SurfaceObject *)(dov[SOmapID].OP);
	XYZmap[0]=SOmap->NodeList[iclosest][0];
	XYZmap[1]=SOmap->NodeList[iclosest][1];
	XYZmap[2]=SOmap->NodeList[iclosest][2];

	/* all is done */
	return (XYZmap);
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
	int iclosest;
	SUMA_SurfaceObject *SOmap;
	int SOmapID;

	/* allocate for return */
	XYZ = (float *)calloc (3, sizeof(float));
	if (XYZ == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Could not allocate for XYZ.\n", FuncName);
		return (NULL);
	} 
	/* if surface is Inherently mappable, do the obivious */
	if (SUMA_isINHmappable(SO)){
		fprintf(SUMA_STDERR,"%s: Surface is inherrently mappable. XYZ = XYZmap.\n", FuncName);
		SUMA_COPY_VEC (XYZmap, XYZ, 3, float, float);
		return (XYZ);	
	}
	/* if surface is not Inherrently mappable, do the deed */
	if (!SUMA_ismappable(SO)){
		fprintf(SUMA_STDERR,"%s: Surface is NOT mappable, returning NULL.\n", FuncName);
		free (XYZ);
		return (NULL);
	}

	/* surface is mappable, things will get more complicated */
	/* find the SO that is the Mappable cahuna */
	SOmapID = SUMA_findDO(SO->MapRef_idcode_str, dov, N_dov);
	if (SOmapID < 0) {
		fprintf (SUMA_STDERR,"%s: Failed in SUMA_findDO This should not happen.\n", FuncName);
		free (XYZ);
		return (NULL);
	}
	SOmap = (SUMA_SurfaceObject *)(dov[SOmapID].OP);

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
					fprintf (SUMA_STDERR,"%s: Non node was close enough to XYZmap, no linkage possible\n", FuncName);
					free (XYZ);
					return (NULL);
				}
				/* store iclosest for lazy user */
				*I_C = iclosest;
			}
	} else { 
		iclosest = *I_C;
	}
	fprintf (SUMA_STDERR,"%s: Node identified for linking purposes is %d\n", FuncName, *I_C);
	XYZ[0]=SO->NodeList[iclosest][0];
	XYZ[1]=SO->NodeList[iclosest][1];
	XYZ[2]=SO->NodeList[iclosest][2];
fprintf (SUMA_STDERR,"%s: 22222hhhhgd\n", FuncName);

	/* all is done */
	return (XYZ);
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

	SOcur = (SUMA_SurfaceObject *)(dov[cur_id].OP);
	/* if surface has no MapRef then it cannot receive colors from precursors */
	if (!SUMA_ismappable(SOcur)) {
		return (-1);
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

	return (rel_id);

}

