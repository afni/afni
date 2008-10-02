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
char *SUMA_find_SOLabel_from_idcode (char *idcode, SUMA_DO *dov, int N_dov);
char *SUMA_find_SOidcode_from_label (char *label, SUMA_DO *dov, int N_dov);
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







#endif
