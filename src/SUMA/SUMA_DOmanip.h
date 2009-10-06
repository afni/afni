#ifndef SUMA_DOMANIP_INCLUDED
#define SUMA_DOMANIP_INCLUDED

SUMA_SurfaceObject * SUMA_findanySOp_inDOv(SUMA_DO *dov, int N_dov);
int SUMA_ClosestNodeToVoxels(SUMA_SurfaceObject *SO, SUMA_VOLPAR *vp, int *closest_node, float *closest_dist, byte *vox_mask, int verb);
float * SUMA_IV_XYZextract (char *IV_filename, int *N_NodeList, int IncludeIndex);
int *SUMA_IV_FaceSetsextract (char *IV_filename, int *N_FaceSetList);
SUMA_SURF_NORM SUMA_SurfNorm (float *NodeList, int N_NodeList, int *FaceSetList, int N_FaceSetList );
int SUMA_SurfNormDir (SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_Free_Displayable_Object (SUMA_DO *dov);
SUMA_DO *SUMA_Alloc_DisplayObject_Struct (int N);
SUMA_Boolean SUMA_AddDO(SUMA_DO *dov, int *N_dov, void *op, SUMA_DO_Types DO_Type, SUMA_DO_CoordType DO_CoordType);
SUMA_Boolean SUMA_RemoveDO(SUMA_DO *dov, int *N_dov, void *op, SUMA_Boolean Free_op);
SUMA_Boolean SUMA_UnRegisterDO(int dov_id, SUMA_SurfaceViewer *cSV);
SUMA_Boolean SUMA_RegisterDO(int dov_id, SUMA_SurfaceViewer *cSV);
void SUMA_Show_DOv (SUMA_DO *dov, int N_dov, FILE *Out);
int * SUMA_GetDO_Type(SUMA_DO *dov, int N_dov, SUMA_DO_Types DO_Type, int *N);
SUMA_Boolean SUMA_Free_Displayable_Object_Vect (SUMA_DO *dov, int N);
SUMA_MEMBER_FACE_SETS *SUMA_MemberFaceSets (int Nind, int * FaceSetList, int nFr , int FaceDim, char *ownerid);
SUMA_Boolean SUMA_Free_MemberFaceSets (SUMA_MEMBER_FACE_SETS *MF);
SUMA_VOLPAR *SUMA_VolPar_Attr (char *volparent_name);
SUMA_VOLPAR *SUMA_VolParFromDset (THD_3dim_dataset *dset);
SUMA_Boolean SUMA_Free_VolPar (SUMA_VOLPAR *VP);
SUMA_VOLPAR *SUMA_Alloc_VolPar (void);
int SUMA_THD_handedness( THD_3dim_dataset * dset );
SUMA_Boolean SUMA_AfniExists(char *prefix, char *c2view);
SUMA_Boolean SUMA_AfniView (char *nameorig, char *cview);
SUMA_Boolean SUMA_AfniExistsView(int exists, char *view);
char *SUMA_AfniPrefix(char *name, char *view, char *path, int *exists);
byte * SUMA_isSkin(THD_3dim_dataset *dset, float *fvec, double thresh, int *N_skin);
void SUMA_Show_VolPar(SUMA_VOLPAR *VP, FILE *Out);
char *SUMA_VolPar_Info (SUMA_VOLPAR *VP);
SUMA_Boolean SUMA_Apply_Coord_xform(float *NodeList,
                                    int N_Node,
                                    int NodeDim, 
                                    double Xform[4][4],
                                    int doinv,
                                    double *pps);
SUMA_Boolean SUMA_Align_to_VolPar (SUMA_SurfaceObject *SO, void* S_struct);
SUMA_Boolean SUMA_Delign_to_VolPar (SUMA_SurfaceObject *SO, void * S_Struct);
SUMA_Boolean SUMA_Apply_VolReg_Trans (SUMA_SurfaceObject *SO);
const char *SUMA_WarpTypeName(SUMA_WARP_TYPES wt);
SUMA_Boolean SUMA_Read_SureFit_Param (char *f_name, SUMA_SureFit_struct *SF);
int SUMA_ReleaseLink (SUMA_INODE * IN);
int SUMA_AddLink (SUMA_INODE *IN);
SUMA_INODE *SUMA_CreateInode (void *data, char *ID);
SUMA_INODE *SUMA_CreateInodeLink (SUMA_INODE * FromIN, SUMA_INODE *ToIN);
SUMA_Boolean SUMA_isInodeLink (SUMA_INODE *IN, const char *HolderIDcode);
SUMA_INODE * SUMA_BreakInodeLink (SUMA_INODE *IN, const char *HolderIDcode);
SUMA_Boolean SUMA_existSO(char *idcode, SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_existDO(char *idcode, SUMA_DO *dov, int N_dov);
int SUMA_whichDO(char *idcode, SUMA_DO *dov, int N_dov);
int SUMA_findSO_inDOv(char *idcode, SUMA_DO *dov, int N_dov);
SUMA_SurfaceObject * SUMA_findSOp_inDOv(char *idcode, SUMA_DO *dov, int N_dov);
SUMA_SurfaceObject * SUMA_find_named_SOp_inDOv(char *coordname, SUMA_DO *dov, int N_dov);
SUMA_Boolean  SUMA_is_ID_4_SO(char *idcode, SUMA_SurfaceObject **SOp);
char *SUMA_find_SOLabel_from_idcode (char *idcode, SUMA_DO *dov, int N_dov);
char *SUMA_find_SOidcode_from_label (char *label, SUMA_DO *dov, int N_dov);
SUMA_SurfaceObject *SUMA_Contralateral_SO(SUMA_SurfaceObject *SO,
                                          SUMA_DO *dov, int N_dov); 
SUMA_DSET * SUMA_Contralateral_dset(SUMA_DSET *dset, SUMA_SurfaceObject *SO, 
                                    SUMA_SurfaceObject**SOCp);
SUMA_Boolean SUMA_ismappable (SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_isINHmappable (SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_isLocalDomainParent (SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_isSO (SUMA_DO DO); 
SUMA_Boolean SUMA_isSO_G (SUMA_DO DO, char *Group);
SUMA_DOMAIN_KINSHIPS SUMA_WhatAreYouToMe (SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2);
SUMA_Boolean SUMA_isRelated (SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2, int level);
SUMA_Boolean SUMA_isNBDOrelated (SUMA_NB_DO *SDO, SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_isdROIrelated (SUMA_DRAWN_ROI *ROI, SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_isROIrelated (SUMA_ROI *ROI, SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_isNIDOrelated (SUMA_NIDO *SDO, SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_isNIDO_SurfBased(SUMA_NIDO *nido);
SUMA_DRAWN_ROI * SUMA_FetchROI_InCreation (SUMA_SurfaceObject *SO, SUMA_DO * dov, int N_dov); 
SUMA_Boolean SUMA_AFNI_forward_warp_xyz( THD_warp * warp , float *XYZv, int N);
SUMA_Boolean SUMA_AFNItlrc_toMNI(float *NodeList, int N_Node, char *Coord);
int SUMA_Build_Mask_DrawnROI (SUMA_DRAWN_ROI *D_ROI, int *Mask);
int * SUMA_Build_Mask_AllROI (SUMA_DO *dov, int N_dov, SUMA_SurfaceObject *SO, int *Mask, int *N_added);
SUMA_ASSEMBLE_LIST_STRUCT *SUMA_AssembleAllROIList (SUMA_DO * dov, int N_dov, SUMA_Boolean SortByLabel); 
SUMA_ASSEMBLE_LIST_STRUCT *SUMA_FreeAssembleListStruct(SUMA_ASSEMBLE_LIST_STRUCT *str);
SUMA_ASSEMBLE_LIST_STRUCT *SUMA_CreateAssembleListStruct(void);
SUMA_Boolean SUMA_DeleteROI (SUMA_DRAWN_ROI *ROI); 
int SUMA_isTypicalSOforVolSurf (SUMA_SurfaceObject *SO);
char *SUMA_DOv_Info (SUMA_DO *dov, int N_dov, int detail);
int SUMA_BiggestLocalDomainParent(SUMA_DO *dov, int N_dov);
SUMA_Boolean SUMA_SetXformShowPreProc(SUMA_XFORM *xf, int ShowPreProc, 
                                      int fromgui);


/*!
   \brief SUMA_IS_DRAW_ROI_SWITCH_ROI_SHADED(Shaded)
   Shaded is YUP unless the Switch ROI window is currently open.
*/
#define SUMA_IS_DRAW_ROI_SWITCH_ROI_SHADED(Shaded)   \
{  \
   Shaded = YUP;  \
   if (SUMAg_CF->X) {   \
      if (SUMAg_CF->X->DrawROI) {\
         if (SUMAg_CF->X->DrawROI->SwitchROIlst) {\
            if (!SUMAg_CF->X->DrawROI->SwitchROIlst->isShaded) {\
               Shaded = NOPE;  \
            }  \
         }  \
      }  \
   }  \
}  

/*!
   \brief SUMA_IS_SWITCH_COL_PLANE_SHADED(SO, Shaded)
   Shaded is YUP unless the Switch Col plane window is currently open.
*/
#define SUMA_IS_SWITCH_COL_PLANE_SHADED(SO, Shaded)   \
{  \
   Shaded = YUP;  \
   if (SO) {   \
      if (SO->SurfCont) {\
         if (SO->SurfCont->SwitchDsetlst) {\
            if (!SO->SurfCont->SwitchDsetlst->isShaded) {\
               Shaded = NOPE;  \
            }  \
         }  \
      }  \
   }  \
}  





/* Xform stuff is in SUMA_display.h 
   Callback stuff is here */
typedef enum {
   SUMA_ERROR_ACTIVATE_EVENT = -1, 
   SUMA_NO_ACTIVATE_EVENT = 0, 
   SUMA_NEW_NODE_ACTIVATE_EVENT,
   SUMA_N_ACTIVATE_EVENTS, 
}  SUMA_CALLBACK_ACTIVATE_EVENTS;

    
typedef struct {
   SUMA_CALLBACK_ACTIVATE_EVENTS event;
   char creator_xform[SUMA_IDCODE_LENGTH]; /*!< In some cases, the callbacks are 
               created when a transform is applied to some dataset, or surface.
               The callback thus created may need to be modified when that
               transforms is altered. creator_xform stores the ID  
               (xform->idcode_str) of the xform that setup the callback. 
               So when xform is changed, it can alter callbacks that it created. 
               This field could be empty*/
   char parents[SUMA_MAX_XFCB_OBJS][SUMA_IDCODE_LENGTH]; /*!< IDs of parents upon                      which this callback acts.
                     These could be SOs or DSETS*/  
   char parents_domain[SUMA_MAX_XFCB_OBJS][SUMA_IDCODE_LENGTH]; /*!< IDs of SO
                   defining the domain of the parent. This is meaningful when
                   the parent is a dset */
   int N_parents;
   int active;
   int pending;
   SUMA_ENGINE_SOURCE trigger_source; /*!< A flag indicating who turned the
                                           pending flag on */
   char FunctionName[128];
   void  (*FunctionPtr)();
   NI_group *FunctionInput;
}  SUMA_CALLBACK;  

char *SUMA_Xforms_Info(DList *dl, int detail) ;
char *SUMA_Callbacks_Info(DList *dl, int detail);
void SUMA_Show_Xforms (DList *dl, FILE *Out, int detail);
void SUMA_Show_Callbacks (DList *dl, FILE *Out, int detail);
SUMA_Boolean SUMA_SetXformActive(SUMA_XFORM *xf, int active, int fromgui);
SUMA_Boolean SUMA_AddXformParent (SUMA_XFORM *xf, 
                                  char *parent_idcode, char *parent_domain);
SUMA_Boolean SUMA_AddXformChild (SUMA_XFORM *xf, 
                                 char *child_idcode);
SUMA_Boolean SUMA_AddCallbackParent (SUMA_CALLBACK *cb, 
                                     char *parent_idcode, char *parent_domain);
SUMA_XFORM *SUMA_NewXform(char *name, char *parent_idcode, char *parent_domain);
SUMA_CALLBACK *SUMA_NewCallback  (char *FunctionName, 
                               SUMA_CALLBACK_ACTIVATE_EVENTS event, 
                               void *FunctionPtr,
                               char *parent_idcode, 
                               char *parent_domain,
                               char *creator_xform);
void SUMA_FreeXform(void *data);
void SUMA_FreeCallback(void *data);
SUMA_Boolean SUMA_is_XformParent (SUMA_XFORM *xf, char *id, int *loc);
SUMA_Boolean SUMA_is_XformChild (SUMA_XFORM *xf, char *id, int *iloc);
SUMA_Boolean SUMA_is_CallbackParent (SUMA_CALLBACK *cb, char *id, int *loc);
SUMA_XFORM *SUMA_Find_XformByID(char *idcode_str);
SUMA_XFORM *SUMA_Find_XformByParent(char *name, char *parent_idcode, int *iloc);
SUMA_CALLBACK *SUMA_Find_CallbackByParent(char *FunctionName, 
                                       char *parent_idcode, int *iloc);
SUMA_CALLBACK *SUMA_Find_CallbackByCreatorXformID(char *creator_xform_idcode);
SUMA_Boolean SUMA_SetCallbackPending (SUMA_CALLBACK *cb, SUMA_Boolean pen,
                                      SUMA_ENGINE_SOURCE src);
SUMA_Boolean SUMA_FlushCallbackEventParameters (SUMA_CALLBACK *cb);
SUMA_Boolean SUMA_ExecuteCallback(SUMA_CALLBACK *cb, 
                                  int refresh, SUMA_SurfaceObject *SO,
                                  int doall) ;
void SUMA_FreeXformInterface(SUMA_GENERIC_XFORM_INTERFACE *gui);
SUMA_GENERIC_XFORM_INTERFACE * SUMA_NewXformInterface(
   SUMA_XFORM *xf);

#define SUMA_XFORM_STORE_AS_LAST_EVENT(nelpars) {\
         NI_set_attribute(nelpars, "last_event.new_node",   \
                          NI_get_attribute(nelpars,"event.new_node"));  \
         NI_set_attribute(nelpars, "last_event.SO_idcode",  \
                          NI_get_attribute(nelpars,"event.SO_idcode")); \
         NI_set_attribute(nelpars, "last_event.overlay_name",  \
                          NI_get_attribute(nelpars,"event.overlay_name")); \
}  

#define SUMA_XFORM_RETRIEVE_LAST_EVENT(nelpars) {\
         NI_set_attribute(nelpars, "event.new_node",   \
                          NI_get_attribute(nelpars,"last_event.new_node"));  \
         NI_set_attribute(nelpars, "event.SO_idcode",  \
                          NI_get_attribute(nelpars,"last_event.SO_idcode")); \
         NI_set_attribute(nelpars, "event.overlay_name",  \
                          NI_get_attribute(nelpars,"last_event.overlay_name")); \
}  

#define SUMA_XFORM_FLUSH_EVENT(nelpars) {\
         NI_SET_INT(nelpars, "event.new_node", -1);   \
         NI_set_attribute(nelpars, "event.SO_idcode", "");  \
         NI_set_attribute(nelpars,"event.overlay_name", "");   \
}  

#define SUMA_XFORM_SAVE_FLUSH_EVENT(nelpars) {  \
   SUMA_XFORM_STORE_AS_LAST_EVENT(nelpars);  \
   SUMA_XFORM_FLUSH_EVENT(nelpars); \
}


#endif
