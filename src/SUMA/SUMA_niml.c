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
   int cc , nn ;
   void *nini ;
   static char FuncName[]={"SUMA_niml_workproc"};
   char tmpcom[100];
   SUMA_Boolean LocalHead = NOPE;
   SUMA_SurfaceViewer *sv;
   DList *list = NULL;
   SUMA_EngineData *ED = NULL;
   
   if (SUMA_NIML_WORKPROC_IO_NOTIFY && SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   sv = (SUMA_SurfaceViewer *)thereiselvis;
   
     /* check if stream is open */

     if( SUMAg_CF->ns == NULL ){
       fprintf(SUMA_STDERR,"Error SUMA_niml_workproc: Stream is not open. \n");
       if (SUMA_NIML_WORKPROC_IO_NOTIFY) {
         SUMA_RETURN(True); /* Don't call me with that lousy stream again */
       }
         else return (True);
     }

     /* check if stream has gone bad */

     nn = NI_stream_goodcheck( SUMAg_CF->ns , 1 ) ;

     if( nn < 0 ){                          /* is bad */
       NI_stream_close( SUMAg_CF->ns ) ;
       SUMAg_CF->ns = NULL ;
       fprintf(SUMA_STDERR,"Error SUMA_niml_workproc: Stream gone bad. Stream closed. \n");
       
       /* close everything */
       if (!list) list = SUMA_CreateList();
       SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_CloseStream4All, SES_Suma, sv);

       if (!SUMA_Engine (&list)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Engine.\n\a", FuncName);
       }      
      
       if (SUMA_NIML_WORKPROC_IO_NOTIFY) {
         SUMA_RETURN(True);               /* Don't call me with that lousy stream again */
       }
         else return (True);
     }

     /* if here, stream is good;
        see if there is any data to be read */

   #if 0
      /* not good enough, checks socket only, not buffer */
      nn = NI_stream_readcheck( SUMAg_CF->ns , 1 ) ;
   #else
      nn = NI_stream_hasinput( SUMAg_CF->ns , 1 ) ;
   #endif
   
     if( nn > 0 ){                                   /* has data */
       int ct = NI_clock_time() ;
       if (LocalHead)   fprintf(SUMA_STDERR,"%s: reading data stream", FuncName) ;

       nini = NI_read_element( SUMAg_CF->ns , 1 ) ;  /* read it */

      if (LocalHead)   fprintf(SUMA_STDERR," time=%d ms\n",NI_clock_time()-ct) ; ct = NI_clock_time() ;

       if( nini != NULL ) {
          if (LocalHead)   {
            NI_element *nel ;
            nel = (NI_element *)nini ;
            fprintf(SUMA_STDERR,"%s:     name=%s vec_len=%d vec_filled=%d, vec_num=%d\n", FuncName,\
                  nel->name, nel->vec_len, nel->vec_filled, nel->vec_num );
         }      
          if (!SUMA_process_NIML_data( nini , sv)) {
             fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_process_NIML_data.\n", FuncName);
          }
      }

      NI_free_element( nini ) ;

      if (LocalHead)   fprintf(SUMA_STDERR,"processing time=%d ms\n",NI_clock_time()-ct) ;

     }
   

   if (SUMA_NIML_WORKPROC_IO_NOTIFY) {
      SUMA_RETURN(False) ;  /* always call me back */
   }
      else return (False);
}

/*----------------------------------------------------------------------*/
/*! Process NIML data.  
------------------------------------------------------------------------*/

SUMA_Boolean SUMA_process_NIML_data( void *nini , SUMA_SurfaceViewer *sv)
{
   int tt = NI_element_type(nini) ;
   int OverInd, loc_ID, iview;
   int i, I_C = -1, nodeid = -1, iv3[3], dest_SO_ID = -1, 
         N_SOlist, SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS];
   NI_element *nel ;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   char CommString[SUMA_MAX_COMMAND_LENGTH], *nel_surfidcode = NULL, *nel_nodeid = NULL;
   char s[SUMA_MAX_STRING_LENGTH], sfield[100], sdestination[100], ssource[100];
   static char FuncName[]={"SUMA_process_NIML_data"};
   float **fm, dimfact,  *XYZ=NULL;
   byte *r, *g, *b;
   SUMA_Boolean Empty_irgba = NOPE, LocalHead = NOPE, Found = NOPE;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_SurfaceViewer *svi = NULL;
   SUMA_OVERLAYS * tmpptr; 
   GLfloat *glar_ColorList = NULL;
   SUMA_OVERLAY_PLANE_DATA sopd;

   /*int it;
   float fv3[3], fv15[15];*/
   /*float ft;
   int **im,  iv15[15];*/ /* keep unused variables undeclared to quite compiler */

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);


   if( tt < 0 ) {/* should never happen */
      fprintf(SUMA_STDERR,"Error %s: Should never have happened.\n", FuncName);
      SUMA_RETURN(NOPE);
   } 

   if( tt != NI_ELEMENT_TYPE ){  /* should never happen */
      fprintf(SUMA_STDERR,"Error %s: Should never have happened.\n", FuncName);
      SUMA_RETURN(NOPE);
   }
   
   /* if here, have a single data element;
      process the data based on the element name */

   nel = (NI_element *) nini ;
   
   if (LocalHead)  {
      fprintf(SUMA_STDERR,"%s:     name=%s vec_len=%d vec_filled=%d, vec_num=%d\n", FuncName,\
               nel->name, nel->vec_len, nel->vec_filled, nel->vec_num );
   }
   

   /*--- CrossHair XYZ --- */
   if( strcmp(nel->name,"SUMA_crosshair_xyz") == 0) {/* SUMA_crosshair_xyz */
      /* Do it for all viewers */
      for (iview = 0; iview < SUMAg_N_SVv; ++iview) {
         svi = &(SUMAg_SVv[iview]);
         if (svi->LinkAfniCrossHair) {/* link cross hair */
            /* look for the surface idcode */
            nel_surfidcode = NI_get_attribute(nel, "surface_idcode");
            
            if (nel_surfidcode == NULL) {
               if (LocalHead) fprintf(SUMA_STDERR,"%s: surface_idcode missing in nel, using svi->Focus_SO_ID.\n", FuncName);
               dest_SO_ID = svi->Focus_SO_ID; /* default */
            } else {
               /* first try to find out if one of the displayed surfaces has a parent equal to nel_surfidcode */
               if (LocalHead) fprintf (SUMA_STDERR,"%s: Searching displayed surfaces.\n", FuncName);
               Found = NOPE;
               i = 0;
               N_SOlist = SUMA_RegisteredSOs(svi, SUMAg_DOv, SOlist);
               while (i < N_SOlist && !Found) { 
                  SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SOlist[i]].OP);
                  if (strcmp(nel_surfidcode, SO->MapRef_idcode_str) == 0) {
                     Found = YUP;
                     dest_SO_ID = SOlist[i];
                  }
                  ++i;
               }
               /* if not found, look for any DO */
               if (!Found) {
                  if (LocalHead) fprintf (SUMA_STDERR,"%s: None of the displayed surfaces (or their parents) match nel_surfidcode. Trying all of DOv...\n", FuncName);
                  dest_SO_ID = SUMA_findSO_inDOv (nel_surfidcode, SUMAg_DOv, SUMAg_N_DOv);
                  if (dest_SO_ID < 0) {
                     if (LocalHead) fprintf(SUMA_STDERR,"%s: nel idcode is not found in DOv.\n", FuncName);            
                     dest_SO_ID = svi->Focus_SO_ID; 
                  } else { /* good, set SO accordingly */
                      if (LocalHead) fprintf(SUMA_STDOUT,"%s: DOv[%d] Matched idcode\n", FuncName, dest_SO_ID);
                  }
               }
            }
            
            SO = (SUMA_SurfaceObject *)(SUMAg_DOv[dest_SO_ID].OP);

            if (LocalHead) SUMA_nel_stdout (nel);
            
            /* check for node id */
            nel_nodeid = NI_get_attribute (nel, "surface_nodeid");
            if (!nel_nodeid) nodeid = -1;
            else {
               if (strlen(nel_nodeid)) nodeid = (int)strtod(nel_nodeid, NULL);
               else nodeid = -1;
            }
            
            /*-- check element for suitability --*/
            if( nel->vec_len    < 1 || nel->vec_filled <  1) {  /* empty element?             */
               SUMA_SLP_Warn ("Empty crosshair xyz.\n");
               SUMA_RETURN(YUP);
            }
            
            if( nel->vec_len != 3 || nel->vec_num != 1 || nel->vec_typ[0] != NI_FLOAT) {
               SUMA_SLP_Err(  "SUMA_crosshair_xyz requires\n"
                              "3 floats in one vector.\n");
               SUMA_RETURN(NOPE);
            }

               
            /* nodeid is supplied, even if the distance from the cross hair to the node is large, 
            set a limit */
            if (nodeid >= 0) {
               SUMA_LH("Node index courtesy of AFNI");
               /* get the XYZ on the mapping reference */
               I_C = -1;
               XYZ = SUMA_XYZmap_XYZ (nel->vec[0], SO, SUMAg_DOv, SUMAg_N_DOv, &I_C);
               if (!XYZ) {
                  SUMA_SL_Warn("AFNI cross hair too\n"
                              "far from surface.\n"
                              "No action taken.");
                  XBell (XtDisplay (sv->X->TOPLEVEL), 50);             
                  SUMA_RETURN(YUP);
               }
               I_C = nodeid; /* node index is set by AFNI */
            } else {
               SUMA_LH("Searching for node index.");
               /* set the cross hair XYZ for now and let SUMA_XYZmap_XYZ set the node index*/
               I_C = -1;
               XYZ = SUMA_XYZmap_XYZ (nel->vec[0], SO, SUMAg_DOv, SUMAg_N_DOv, &I_C);

               if (XYZ == NULL || I_C < 0) {
                  SUMA_SL_Warn("AFNI cross hair too\n"
                              "far from surface.\n"
                              "No action taken.");
                  XBell (XtDisplay (sv->X->TOPLEVEL), 50);             
                  SUMA_RETURN(YUP);
               }
            }
            
            /* attach the cross hair to the selected surface */
            #if 0
               if (nel_surfidcode == NULL) {
                  fprintf(SUMA_STDERR,"Error %s: surface_idcode missing in nel.\nLoose Crosshair\n", FuncName);
                  iv3[0] = -1;
               } else {
                  iv3[0] = dest_SO_ID;
               }
            #else
               iv3[0] = dest_SO_ID; /* nel_surfidcode == NULL is handled above, May 15 03*/
            #endif
            
            iv3[1] = I_C; /* use the closest node for a link otherwise when you switch states, you'll get a wandering cross hair */
            if (!list) list = SUMA_CreateList();
            ED = SUMA_InitializeEngineListData (SE_BindCrossHair);
            if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                   SEF_iv3, (void*)iv3,
                                                   SES_SumaFromAfni, (void *)svi, NOPE,
                                                   SEI_Head, NULL)) {
               fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
               SUMA_RETURN (NOPE);
            }

            /* send cross hair coordinates */
            ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
            if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                   SEF_fv3, (void*)XYZ,
                                                   SES_SumaFromAfni, svi, NOPE,
                                                   SEI_Tail, NULL)) {
               fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            
            svi->ResetGLStateVariables = YUP; 
            
            #if 0
            /* logic for that not too clear yet */
            /* set the SO in Focus */
            ED = SUMA_InitializeEngineListData (SE_SetSOinFocus);
            if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                   SEF_i, (void*)&dest_SO_ID,
                                                   SES_SumaFromAfni, (void *)svi, NOPE,
                                                   SEI_Tail, NULL)) {
               fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            #endif
            
            SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay, SES_SumaFromAfni, svi);
            if (!SUMA_Engine (&list)) {
               fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
            }
              
         } /* link cross hair */    
      } /* iview ... for all viewers */
      /* don't free nel, it's freed later on
         dont't free attributes obtained in NI_get_attribute, they are copies of pointers in nel  */
      SUMA_RETURN(YUP) ;
   }/* SUMA_crosshair_xyz */
   
   /* SUMA_irgba Node colors */
   if( strcmp(nel->name,"SUMA_irgba") == 0) {/* SUMA_irgba */
      if( nel->vec_len  < 1 || nel->vec_filled <  1) {  /* empty element?             */
         fprintf(SUMA_STDERR,"%s: Empty SUMA_irgba.\n", FuncName);
         Empty_irgba = YUP;
        }else {
         if( nel->vec_num != 5 || nel->vec_typ[0] != NI_INT || nel->vec_typ[1] != NI_BYTE || nel->vec_typ[2] != NI_BYTE || nel->vec_typ[3] != NI_BYTE) {
              fprintf(SUMA_STDERR,"%s: SUMA_irgba Bad format\n", FuncName);
            SUMA_RETURN(NOPE);
        }
      }
     
     /* show me nel */
     /* if (LocalHead) SUMA_nel_stdout (nel); */
     
      /* look for the surface idcode */
      nel_surfidcode = NI_get_attribute(nel, "surface_idcode");
      if (nel_surfidcode == NULL) {
         fprintf(SUMA_STDERR,"Error %s: surface_idcode missing in nel.\n", FuncName);
         SUMA_RETURN(NOPE);
      } 
      
      SO = SUMA_findSOp_inDOv (nel_surfidcode, SUMAg_DOv, SUMAg_N_DOv);
      if (!SO) {
         fprintf(SUMA_STDERR,"Error %s: nel idcode is not found in DOv.\n", FuncName);
         SUMA_RETURN(NOPE);
      }
      
      /* store the node colors */
      /* create a color overlay plane */
      /* you could create an overlay plane with partial node coverage but you'd have to clean up and SUMA_reallocate
      with each new data sent since the number of colored nodes will change. So I'll allocate for the entire node list 
      for the FuncAfni_0 color plane although only some values will be used*/

      sopd.Type = SOPT_ibbb;
      sopd.Source = SES_Afni;
      sopd.GlobalOpacity = SUMA_AFNI_COLORPLANE_OPACITY;
      sopd.BrightMod = NOPE;
      sopd.Show = YUP;
      /* dim colors from maximum intensity to preserve surface shape highlights, division by 255 is to scale color values between 1 and 0 */
      sopd.DimFact = SUMA_DIM_AFNI_COLOR_FACTOR / 255.0;
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

      if (!SUMA_iRGB_to_OverlayPointer (SO, "FuncAfni_0", &sopd, &OverInd, SUMAg_DOv, SUMAg_N_DOv)) {
         SUMA_SLP_Err("Failed to fetch or create overlay pointer.");
         SUMA_RETURN(NOPE);
      }
      
      
      /* register a color remix request */
      if (LocalHead) fprintf(SUMA_STDERR, "%s: Setting Remix Flag for all related surfaces. ...\n", FuncName);
      if(!SUMA_SetRemixFlag (SO->idcode_str, SUMAg_SVv, SUMAg_N_SVv)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SetRemixFlag.\n", FuncName);
         SUMA_RETURN(NOPE);
      }

      /* file a redisplay request */
      if (LocalHead) fprintf(SUMA_STDERR, "%s: Redisplaying all visible...\n", FuncName);
      if (!list) list = SUMA_CreateList();
      SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, SES_SumaFromAfni, sv);

      if (!SUMA_Engine (&list)) {
         fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
         SUMA_RETURN(NOPE);
      }

      /* don't free nel, it's freed later on */
      SUMA_RETURN(YUP) ;

       
   }/* SUMA_irgba */

   /*** If here, then name of element didn't match anything ***/

   fprintf(SUMA_STDERR,"Error %s: Unknown NIML input: %s\n", FuncName ,nel->name) ;
   SUMA_RETURN(NOPE) ;
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   
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

   NI_set_attribute (nel, "volume_idcode", SO->VolPar->idcode_str);
   NI_set_attribute (nel, "surface_idcode", SO->idcode_str);
   NI_set_attribute (nel, "surface_label", SO->Label);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   
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
   I = (int *)   SUMA_malloc( sizeof(int)   * SO->N_FaceSet ) ;
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

   NI_set_attribute (nel, "volume_idcode", SO->VolPar->idcode_str);
   NI_set_attribute (nel, "surface_idcode", SO->idcode_str);

   SUMA_RETURN (nel);
}

SUMA_Boolean SUMA_nel_stdout (NI_element *nel) 
{
   static char FuncName[]={"SUMA_nel_stdout"};
   NI_stream nstdout;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
      fprintf(SUMA_STDERR,"%s: Linkage is not posible, using current XYZ\n", FuncName);
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
      MapRef_idcode_str != NULL && VolPar != NULL
      
   This function is much different from the one prior to Tue Nov 19 11:44:24 EST 2002
*/

SUMA_Boolean SUMA_CanTalkToAfni (SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_CanTalkToAfni"};
   int i;
   SUMA_SurfaceObject *SO;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   for (i=0; i< N_dov; ++i) {
      if (SUMA_isSO(dov[i])) {
         SO = (SUMA_SurfaceObject *)(dov[i].OP);
         if (SO->MapRef_idcode_str != NULL && SO->VolPar != NULL) {
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

   if (SUMA_WORKPROC_IO_NOTIFY && SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   nimlROI->EdgeColor[0] = ROI->EdgeColor[0];
   nimlROI->EdgeColor[1] = ROI->EdgeColor[1];
   nimlROI->EdgeColor[2] = ROI->EdgeColor[2];
   nimlROI->EdgeThickness = ROI->EdgeThickness;
   if (!nimlROI->N_ROI_datum) {
      nimlROI->ROI_datum = NULL;
      SUMA_RETURN(nimlROI);
   }
   nimlROI->ROI_datum = (SUMA_NIML_ROI_DATUM *)SUMA_malloc(nimlROI->N_ROI_datum*sizeof(SUMA_NIML_ROI_DATUM));

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
      nimlROI->ROI_datum[i].tPath = ROI_Datum->tPath; */
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!nimlROI) SUMA_RETURN(NULL);
   
   /* allocate and initialize the whimpy fields 
      Based on SUMA_AllocateDrawnROI*/
   ROI = (SUMA_DRAWN_ROI *) SUMA_malloc(sizeof(SUMA_DRAWN_ROI));
   if (  nimlROI->Type == SUMA_ROI_OpenPath || 
         nimlROI->Type == SUMA_ROI_ClosedPath ||
         nimlROI->Type == SUMA_ROI_FilledArea ) { /* this ROI will gradually be reconstructed,
                                                       start with the basics */
         ROI->Type = SUMA_ROI_OpenPath; /* at the end of the construction you should reach nimlROI->Type */
   }else {
      /* nothing to reconstruct */
      ROI->Type = nimlROI->Type;
   }
   
   ROI->idcode_str = SUMA_copy_string(nimlROI->idcode_str);
   ROI->Parent_idcode_str = SUMA_copy_string(nimlROI->Parent_idcode_str);
   ROI->Label = SUMA_copy_string(nimlROI->Label);
   ROI->iLabel = nimlROI->iLabel;
   if (LocalHead) fprintf (SUMA_STDERR, "%s: ROI->Parent_idcode_str %s\n", FuncName, ROI->Parent_idcode_str);
   
   ROI->ROIstrokelist = (DList *)SUMA_malloc (sizeof(DList));
   dlist_init(ROI->ROIstrokelist, SUMA_FreeROIDatum);
   
   ROI->DrawStatus = SUMA_ROI_Finished;
   ROI->StackPos = NULL;
   ROI->ActionStack = SUMA_CreateActionStack ();
   ROI->ColPlaneName = SUMA_copy_string(nimlROI->ColPlaneName);
   ROI->FillColor[0] = nimlROI->FillColor[0];
   ROI->FillColor[1] = nimlROI->FillColor[1];
   ROI->FillColor[2] = nimlROI->FillColor[2];
   ROI->EdgeColor[0] = nimlROI->EdgeColor[0];
   ROI->EdgeColor[1] = nimlROI->EdgeColor[1];
   ROI->EdgeColor[2] = nimlROI->EdgeColor[2];
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
         ROIA = (SUMA_ROI_ACTION_STRUCT *) SUMA_malloc (sizeof(SUMA_ROI_ACTION_STRUCT *));
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
      ROIA = (SUMA_ROI_ACTION_STRUCT *) SUMA_malloc (sizeof(SUMA_ROI_ACTION_STRUCT *)); 
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
      niml_ROI_Datum_type = NI_rowtype_define("SUMA_NIML_ROI_DATUM", "int,int,int,int[#3]");
      
      niml_ROI = (SUMA_NIML_DRAWN_ROI *)malloc(sizeof(SUMA_NIML_DRAWN_ROI));
      niml_ROI->Type = 4;
      niml_ROI->idcode_str = idcode_str;
      niml_ROI->Parent_idcode_str = Parent_idcode_str;
      niml_ROI->Label = Label;
      niml_ROI->iLabel = 20;
      niml_ROI->N_ROI_datum = 2;
      niml_ROI->ROI_datum = (SUMA_NIML_ROI_DATUM *)malloc(niml_ROI->N_ROI_datum*sizeof(SUMA_NIML_ROI_DATUM));

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

      fprintf(stderr,"*********** Creating new data element, a column of %d elements \n", niml_ROI->N_ROI_datum);
      nel = NI_new_data_element("A_drawn_ROI",  niml_ROI->N_ROI_datum);
      
      fprintf(stderr,"*********** Adding column\n");
      NI_add_column( nel , niml_ROI_Datum_type, niml_ROI->ROI_datum );
      
      fprintf(stderr,"*********** Setting attributes element\n");
      NI_set_attribute (nel, "idcode_str", niml_ROI->idcode_str);
      NI_set_attribute (nel, "Parent_idcode_str", niml_ROI->Parent_idcode_str);
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
