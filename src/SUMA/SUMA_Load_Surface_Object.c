
/*----------------------------------------------------------------------
 * history:
 *
 * 11 Dec 2003  [rickr]
 *   - added functions:
 *       o  SUMA_spec_select_surfs	- restrict spec struct from name list
 *       o  SUMA_swap_spec_entries	- swap 2 entries in spec struct
 *       o  SUMA_unique_name_ind	- verify unique surf name in spec
 *       o  SUMA_coord_file		- get file name, based on surf type
 *       o  swap_strings		- swap 2 strings via 3rd
 *----------------------------------------------------------------------
*/
    
/* Header FILES */
   
#include "SUMA_suma.h"

 
/*#define  DO_SCALE_RANGE   *//*!< scale node coordinates to 0 <--> 100. DO NOT USE IT, OBSOLETE*/
#ifndef DO_SCALE_RANGE
   #define DO_SCALE 319.7   /*!< scale node coordinates by specified factor. Useful for tesscon coordinate system in iv files*/
#endif

/* CODE */

/*!
  SUMA_AllocSpecFields (SUMA_SurfSpecFile *Spec)
 \brief Function to allocate space for the spec file fields. 
 Clumsy but does the work, needed to get around stack size limits of 8 Mb
 \sa  SUMA_FreeSpecFields
*/
SUMA_Boolean SUMA_AllocSpecFields (SUMA_SurfSpecFile *Spec)
{
   static char FuncName[]={"SUMA_AllocSpecFields"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   if (!Spec) SUMA_RETURN(NOPE);

   Spec->N_Surfs = -1;    /* flag for initialization */                                                 
   Spec->N_States = 0;                                                     
   Spec->N_Groups = 0;
   
   Spec->SurfaceType = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC , 
                              SUMA_MAX_LABEL_LENGTH, sizeof(char));     
   if (!Spec->SurfaceType) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->SurfaceFormat = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                              SUMA_MAX_LABEL_LENGTH , sizeof(char));  
   if (!Spec->SurfaceFormat) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->TopoFile = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC,
                                    SUMA_MAX_FP_NAME_LENGTH , sizeof(char));  
   if (!Spec->TopoFile) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->CoordFile = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                        SUMA_MAX_FP_NAME_LENGTH , sizeof(char)); 
   if (!Spec->CoordFile) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->MappingRef = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                        SUMA_MAX_FP_NAME_LENGTH , sizeof(char));  
   if (!Spec->MappingRef) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->SureFitVolParam = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                        SUMA_MAX_FP_NAME_LENGTH , sizeof(char));  
   if (!Spec->SureFitVolParam) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->SurfaceFile = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                        SUMA_MAX_FP_NAME_LENGTH , sizeof(char));   
   if (!Spec->SurfaceFile) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->VolParName = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                        SUMA_MAX_FP_NAME_LENGTH , sizeof(char));   
   if (!Spec->VolParName) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->IDcode = (char **)SUMA_calloc(SUMA_MAX_N_SURFACE_SPEC, sizeof(char*));
   if (!Spec->IDcode) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->State = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                              SUMA_MAX_LABEL_LENGTH, sizeof(char));       
   if (!Spec->State) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->Group = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                              SUMA_MAX_LABEL_LENGTH, sizeof(char));        
   if (!Spec->Group) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->SurfaceLabel = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                              SUMA_MAX_LABEL_LENGTH, sizeof(char)); 
   if (!Spec->SurfaceLabel) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->EmbedDim = (int *)SUMA_calloc(SUMA_MAX_N_SURFACE_SPEC, sizeof(int));                            
   if (!Spec->EmbedDim) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   
   /* modifications to the lame MappingRef field */
   Spec->AnatCorrect = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                              SUMA_MAX_LABEL_LENGTH, sizeof(char));    
   if (!Spec->AnatCorrect) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->Hemisphere = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                              SUMA_MAX_LABEL_LENGTH, sizeof(char));      
   if (!Spec->Hemisphere) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->DomainGrandParentID = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                              SUMA_MAX_LABEL_LENGTH, sizeof(char));    
   if (!Spec->DomainGrandParentID) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->OriginatorID = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                              SUMA_MAX_LABEL_LENGTH, sizeof(char));   
   if (!Spec->OriginatorID) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->LocalCurvatureParent = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC,
                        SUMA_MAX_FP_NAME_LENGTH , sizeof(char));    
   if (!Spec->LocalCurvatureParent) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->LocalDomainParent = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                        SUMA_MAX_FP_NAME_LENGTH , sizeof(char));       
   if (!Spec->LocalDomainParent) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->LabelDset = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                        SUMA_MAX_FP_NAME_LENGTH , sizeof(char));       
   if (!Spec->LabelDset) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->NodeMarker = (char **)SUMA_allocate2D(SUMA_MAX_N_SURFACE_SPEC, 
                        SUMA_MAX_FP_NAME_LENGTH , sizeof(char));       
   if (!Spec->NodeMarker) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   
   Spec->DO_name = (char **)SUMA_allocate2D(SUMA_MAX_N_DO_SPEC,
                                 SUMA_MAX_FP_NAME_LENGTH , sizeof(char));       
   Spec->DO_type = (int *)SUMA_calloc(SUMA_MAX_N_DO_SPEC, sizeof(int));
   Spec->N_DO = 0;
   
   Spec->StateList = (char *)SUMA_calloc( SUMA_MAX_N_SURFACE_SPEC*100, 
                                          sizeof(char));
   if (!Spec->StateList) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->SpecFilePath = (char *)SUMA_calloc(SUMA_MAX_DIR_LENGTH, sizeof(char));
   if (!Spec->SpecFilePath) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   Spec->SpecFileName = (char *)SUMA_calloc(SUMA_MAX_NAME_LENGTH, sizeof(char));
   if (!Spec->SpecFileName) { 
      SUMA_S_Err("Failed to allocate"); SUMA_RETURN(NOPE); 
   }
   
   SUMA_RETURN(YUP);
}


SUMA_Boolean SUMA_FreeSpecFields (SUMA_SurfSpecFile *Spec)
{
   static char FuncName[]={"SUMA_FreeSpecFields"};

   SUMA_ENTRY;
   
   if (!Spec) SUMA_RETURN(YUP);
   if ( (Spec->N_Surfs < 0 && Spec->N_Surfs != -1) || 
         Spec->N_Surfs > SUMA_MAX_N_SURFACE_SPEC ) {
      SUMA_S_Err("Suspicious values in Spec->N_Surfs, will not free fields");
      SUMA_RETURN(NOPE);
   }
   if (Spec->SurfaceType) { 
      SUMA_free2D((char **)Spec->SurfaceType, SUMA_MAX_N_SURFACE_SPEC);
      Spec->SurfaceType = NULL; }
   if (Spec->SurfaceFormat) { 
      SUMA_free2D((char **)Spec->SurfaceFormat, SUMA_MAX_N_SURFACE_SPEC);
      Spec->SurfaceFormat = NULL; }
   if (Spec->TopoFile) { 
      SUMA_free2D((char **)Spec->TopoFile, SUMA_MAX_N_SURFACE_SPEC);
      Spec->TopoFile = NULL; }
   if (Spec->CoordFile) { 
      SUMA_free2D((char **)Spec->CoordFile, SUMA_MAX_N_SURFACE_SPEC);
      Spec->CoordFile = NULL; }
   if (Spec->MappingRef) { 
      SUMA_free2D((char **)Spec->MappingRef, SUMA_MAX_N_SURFACE_SPEC);
      Spec->MappingRef = NULL; }
   if (Spec->SureFitVolParam) { 
      SUMA_free2D((char **)Spec->SureFitVolParam, SUMA_MAX_N_SURFACE_SPEC);
      Spec->SureFitVolParam = NULL; }

   if (Spec->SurfaceFile) { 
      SUMA_free2D((char **)Spec->SurfaceFile, SUMA_MAX_N_SURFACE_SPEC);
      Spec->SurfaceFile = NULL; }
   
   if (Spec->DO_name) { 
      SUMA_free2D((char **)Spec->DO_name, SUMA_MAX_N_DO_SPEC);
      Spec->DO_name = NULL; }
   if (Spec->DO_type) SUMA_free(Spec->DO_type); Spec->DO_type=NULL;
   Spec->N_DO = 0;
   
   if (Spec->VolParName) { 
      SUMA_free2D((char **)Spec->VolParName, SUMA_MAX_N_SURFACE_SPEC);
      Spec->VolParName = NULL; }
   if (Spec->IDcode) { SUMA_free(Spec->IDcode);
      Spec->IDcode = NULL; /* IDcode[i] = is a pointer copy */}
   if (Spec->State) { 
      SUMA_free2D((char **)Spec->State, SUMA_MAX_N_SURFACE_SPEC);
      Spec->State = NULL; }

   if (Spec->Group) { 
      SUMA_free2D((char **)Spec->Group, SUMA_MAX_N_SURFACE_SPEC);
      Spec->Group= NULL; }

   if (Spec->SurfaceLabel) { 
      SUMA_free2D((char **)Spec->SurfaceLabel, SUMA_MAX_N_SURFACE_SPEC);
      Spec->SurfaceLabel = NULL; }

   if (Spec->EmbedDim) { SUMA_free(Spec->EmbedDim);
      Spec->EmbedDim = NULL; }
   
   /* modifications to the lame MappingRef field */
   if (Spec->AnatCorrect) { 
      SUMA_free2D((char **)Spec->AnatCorrect, SUMA_MAX_N_SURFACE_SPEC);
      Spec->AnatCorrect = NULL; }

   if (Spec->Hemisphere) { 
      SUMA_free2D((char **)Spec->Hemisphere, SUMA_MAX_N_SURFACE_SPEC);
      Spec->Hemisphere = NULL; }

   if (Spec->DomainGrandParentID) { 
      SUMA_free2D((char **)Spec->DomainGrandParentID, SUMA_MAX_N_SURFACE_SPEC);
      Spec->DomainGrandParentID = NULL; }

   if (Spec->OriginatorID) { 
      SUMA_free2D((char **)Spec->OriginatorID, SUMA_MAX_N_SURFACE_SPEC);
      Spec->OriginatorID = NULL; }

   if (Spec->LocalCurvatureParent) { 
      SUMA_free2D((char **)Spec->LocalCurvatureParent, SUMA_MAX_N_SURFACE_SPEC);
      Spec->LocalCurvatureParent = NULL; }

   if (Spec->LocalDomainParent) { 
      SUMA_free2D((char **)Spec->LocalDomainParent, SUMA_MAX_N_SURFACE_SPEC);
      Spec->LocalDomainParent = NULL; }
   
   if (Spec->LabelDset) { 
      SUMA_free2D((char **)Spec->LabelDset, SUMA_MAX_N_SURFACE_SPEC);
      Spec->LabelDset = NULL; }
   
   if (Spec->NodeMarker) { 
      SUMA_free2D((char **)Spec->NodeMarker, SUMA_MAX_N_SURFACE_SPEC);
      Spec->NodeMarker = NULL; }

   Spec->N_Surfs = -2; /* flag for freeing */                                                         
   Spec->N_States = 0;                                                     
   Spec->N_Groups = 0;
   
   
   if (Spec->StateList) { SUMA_free(Spec->StateList);
      Spec->StateList = NULL; }
   if (Spec->SpecFilePath) { SUMA_free(Spec->SpecFilePath);
      Spec->SpecFilePath = NULL; }
   if (Spec->SpecFileName) { SUMA_free(Spec->SpecFileName);
      Spec->SpecFileName = NULL; }
      
   SUMA_RETURN(YUP);
}
   
/*!
   \brief Function to write surface objects to disk in various formats
   ans = SUMA_Save_Surface_Object (void * F_name, SUMA_SurfaceObject *SO, SUMA_SO_File_Type SO_FT, 
                              SUMA_SO_File_Format SO_FF, void *someparam);
   \param F_name (void *)
         For SUMA_INVENTOR_GENERIC F_name is (char *) containing path (if any) and filename of surface
         For SUMA_SUREFIT F_name is (SUMA_SFname *) containing full topo and coord names, with path (if any)
         For SUMA_FREE_SURFER F_name is  (char *) name of .asc file (with path)
         For SUMA_VEC (a dumb ascii format), F_name is (SUMA_SFname *) containing the nodelist file in name_coord 
          and facesetlist file in name_topo (path included).
         For SUMA_PLY (char *) name of .ply file (with path)
         For SUMA_OPENDX_MESH (char *) name of .dx file (with path)
   \param   SO_FT (SUMA_SO_File_Type) file type to be read (inventor, free surfer , Surefit )
   \param   SO_FF (SUMA_SO_File_Format) Ascii or Binary (only ascii at the moment, except for .ply files)
   \param someparam (void *) a pointer used to pass extra parameters. At the moment, used for passing a parent
                           surface when writing FreeSurferPatches
   \sa SUMA_Load_Surface_Object()
   
   NOTE:
   Vertex coordinates are written as in SO->NodeList
   The Volume Parent transformation is not undone. 
   For SureFit surfaces, the volume param shift is not undone.
*/
void * SUMA_Save_Surface_Object_Wrap ( 
                        char *surf_name, char *topo_name,
                        SUMA_SurfaceObject *SO, 
                        SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF, 
                        void *someparam)
{
   static char FuncName[]={"SUMA_Save_Surface_Object_Wrap"};
   void *SO_name=NULL;
   SUMA_Boolean exists;
   
   SUMA_ENTRY;

   if (!surf_name || !SO) {
      SUMA_S_Err("NULL input"); SUMA_RETURN(NULL);
   }
   
   if (SO_FT == SUMA_FT_NOT_SPECIFIED) {
      SO_FT = SUMA_GuessSurfFormatFromExtension(surf_name, "usegifti.gii");
   }

   if (!(SO_name = SUMA_2Prefix2SurfaceName (surf_name, topo_name, 
                                             NULL, NULL, SO_FT, &exists))) {
      SUMA_S_Err("Failed to form SO_name");
      SUMA_RETURN(NULL);
   }
   if (!SUMA_Save_Surface_Object (SO_name, SO, SO_FT, SO_FF, someparam)) {
      SUMA_S_Err("Failed to save surface");
      SUMA_RETURN(NULL);
   }
   
   SUMA_RETURN(SO_name);
}
                                             
SUMA_Boolean SUMA_Save_Surface_Object (void * F_name, SUMA_SurfaceObject *SO, 
                                       SUMA_SO_File_Type SO_FT,
                                       SUMA_SO_File_Format SO_FF, 
                                       void *someparam)
{/*SUMA_Save_Surface_Object*/
   static char FuncName[]={"SUMA_Save_Surface_Object"};
   
   SUMA_ENTRY;
   
   if (!F_name) {
      SUMA_S_Err("Null filename!");
      SUMA_RETURN(NOPE);
   }
   switch (SO_FT) {
      case SUMA_OBJ_MESH:
         SUMA_S_Err("Nothing for OBJ_MESH yet");
         SUMA_RETURN(NOPE);
         break;
      case SUMA_OPENDX_MESH:
         if (!SUMA_OpenDX_Write ((char *)F_name, SO)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed to write SUMA_OPENDX_MESH surface.\n"
                     , FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_PLY:
         if (!SUMA_Ply_Write ((char *)F_name, SO)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed to write PLY surface.\n"
                     , FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_STL:
         if (!SUMA_STL_Write ((char *)F_name, SO)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed to write STL surface.\n"
                     , FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_MNI_OBJ:
         if (!SUMA_MNI_OBJ_Write ((char *)F_name, SO)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed to write MNI_OBJ surface.\n"
                     , FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_FREE_SURFER:
         if (SO_FF == SUMA_FF_NOT_SPECIFIED) SO_FF = SUMA_ASCII;
         if (SO_FF != SUMA_ASCII) {
            fprintf (SUMA_STDERR, 
                     "Warning %s: "
                     "Only ASCII supported for Free Surfer surfaces.\n"
                     , FuncName);
         }
         if (!SUMA_FS_Write ((char *)F_name, SO, 
                              "!ascii version in FreeSurfer format (SUMA)")) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed to write FreeSurfer surface.\n"
                     , FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_FREE_SURFER_PATCH:
         if (SO_FF == SUMA_FF_NOT_SPECIFIED) SO_FF = SUMA_ASCII;
         if (SO_FF != SUMA_ASCII) {
            fprintf (SUMA_STDERR, 
                     "Warning %s: "
                     "Only ASCII supported for Free Surfer surface patches.\n"
                     , FuncName);
         }
         if (!SUMA_FreeSurfer_WritePatch ((char *)F_name, SO, NULL,
                                          (SUMA_SurfaceObject *)someparam)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to write FreeSurfer surface.\n"
                     , FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_SUREFIT:
         if (SO_FF == SUMA_FF_NOT_SPECIFIED) SO_FF = SUMA_ASCII;
         if (SO_FF != SUMA_ASCII) {
            fprintf (SUMA_STDERR, 
                     "Warning %s: Only ASCII supported for SureFit surfaces.\n"
                     , FuncName);
         }
         if (!SUMA_SureFit_Write ((SUMA_SFname *)F_name, SO)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed to write SureFit surface.\n"
                     , FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_VEC:
         if (SO_FF == SUMA_FF_NOT_SPECIFIED) SO_FF = SUMA_ASCII;
         if (SO_FF != SUMA_ASCII) {
            fprintf (SUMA_STDERR, 
                     "Warning %s: Only ASCII supported for vec surfaces.\n"
                     , FuncName);
         }
         if (!SUMA_VEC_Write ((SUMA_SFname *)F_name, SO)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed to write vec surface.\n"
                     , FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_BYU:
         if (SO_FF == SUMA_FF_NOT_SPECIFIED) SO_FF = SUMA_ASCII;
         if (SO_FF != SUMA_ASCII) {
            fprintf (SUMA_STDERR, 
                     "Warning %s: Only ASCII supported for BYU surfaces.\n"
                     , FuncName);
         }
         if (!SUMA_BYU_Write ((char *)F_name, SO, 1)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed to write BYU surface.\n"
                     , FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_INVENTOR_GENERIC:
         fprintf (SUMA_STDERR, 
                  "Error %s: "
                  "Not ready to deal with inventor surface writing.\n"
                  , FuncName);
         SUMA_RETURN (NOPE);
         break;
      case SUMA_BRAIN_VOYAGER:
         fprintf (SUMA_STDERR, 
                  "Error %s: "
                  "Not ready to deal with brain voyager surface writing.\n"
                  , FuncName);
         SUMA_RETURN (NOPE);
         break;
      case SUMA_GIFTI:
         if (!SUMA_GIFTI_Write ((char *)F_name, SO, SO_FF)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed to write GIFTI surface.\n"
                     , FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_PREDEFINED:
         if (!SUMA_GIFTI_Write ((char *)F_name, SO, SUMA_ASCII)) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed to write predefined GIFTI surface.\n"
                     , FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_FT_NOT_SPECIFIED:
      default:
         fprintf (SUMA_STDERR, "Error %s: Bad surface type.\n", FuncName);
         SUMA_RETURN (NOPE);
   
   }
   
   SUMA_RETURN (YUP);
}

/*!
   \brief for a new SO, calculate the following:
   Normals, dimensions, SUMA's NodeMarker FaceSetMarker, etc.
*/
SUMA_Boolean SUMA_PrepSO_GeomProp_GL(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_PrepSO_GeomProp_GL"};
   static int iwarn=0;
   int k, ND, id;
   SUMA_SURF_NORM SN;
   byte *PatchNodeMask=NULL;
   SUMA_SurfaceViewer *sv=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LHv("Unleashed on surface %s\n", CHECK_NULL_STR(SO->Label));
   
   /* Calculate Min, Max, Mean */
   
   if (!SUMA_isSODimInitialized(SO)) { 
      if (!SUMA_SetSODims(SO)) {
         SUMA_S_Err("Failed to set dims!");
         SUMA_RETURN(NOPE);
      }
   } else {
      SUMA_LH("SODim initialized already");
   }
   
   /* calculate the center and dimensions for the nodes in the patch only */
   PatchNodeMask = SUMA_MaskOfNodesInPatch(SO, &(SO->N_patchNode));
   if (!SO->N_patchNode || SO->N_patchNode == SO->N_Node) { 
      SUMA_LHv("Up here, isSphere = %d\n", SO->isSphere);
      if (!PatchNodeMask ) { 
         SUMA_SL_Err("Failed in SUMA_MaskOfNodesInPatch.\n"
                     "Using values from all nodes."); 
      } else {
         SUMA_LH("Using values from all nodes."); 
      }
      if (!SUMA_IS_GEOM_SYMM(SO->isSphere)) {
         SUMA_COPY_VEC(SO->Center, SO->patchCenter, 3, float, float);
      } else {
         SUMA_COPY_VEC(SO->SphereCenter, SO->patchCenter, 3, float, float);
      }
      SUMA_COPY_VEC(SO->MinDims, SO->patchMinDims, 3, float, float);
      SUMA_COPY_VEC(SO->MaxDims, SO->patchMaxDims, 3, float, float);
      SO->patchaMaxDims = SO->aMaxDims;
      SO->patchaMinDims = SO->aMinDims;
   }else {
      SUMA_LH("Down there");
      SUMA_MIN_MAX_SUM_VECMAT_MASK_COL (
                  SO->NodeList, SO->N_Node, 
                  SO->NodeDim, PatchNodeMask, 
                  SO->patchMinDims, SO->patchMaxDims, 
                  SO->patchCenter);
      SO->patchCenter[0] /= SO->N_patchNode;
      SO->patchCenter[1] /= SO->N_patchNode;
      SO->patchCenter[2] /= SO->N_patchNode;
      SUMA_MIN_VEC (SO->patchMinDims, 3, SO->patchaMinDims );
      SUMA_MAX_VEC (SO->patchMaxDims, 3, SO->patchaMaxDims);
   }
   
   if (SO->patchNodeMask) {
      SUMA_S_Err( "Hmm, unexpected."
          "This function is usually called once per surface, "
          "it is OK to call it more than once, but be sure it is necessary"
          "before you turn this error message off");
      SUMA_free(SO->patchNodeMask); 
   }
   SO->patchNodeMask = NULL;
   if (PatchNodeMask) {
      if (SO->N_patchNode != SO->N_Node) {
         SO->patchNodeMask =   PatchNodeMask; PatchNodeMask = NULL;
      } else {
         SUMA_free(PatchNodeMask) ; PatchNodeMask = NULL;
      }
   }
   #ifdef DO_SCALE_RANGE
   SUMA_LH("In scale range");
   { float tmpfact;
   /* Now do some scaling */
   tmpfact = (SO->aMaxDims - SO->aMinDims)/100;
   ND = SO->NodeDim;
   for (k=0; k < SO->N_Node; k++)
   {
      id = NodeDim * k;
      SO->NodeList[k] = (SO->NodeList[k] - SO->aMinDims)/tmpfact;
      SO->NodeList[k+1] = (SO->NodeList[k+1] - SO->aMinDims)/tmpfact;
      SO->NodeList[k+2] = (SO->NodeList[k+2] - SO->aMinDims)/tmpfact;
   }
   
   SO->Center[0] = (SO->Center[0] - SO->aMinDims)/tmpfact;
   SO->Center[1] = (SO->Center[1] - SO->aMinDims)/tmpfact;
   SO->Center[2] = (SO->Center[2] - SO->aMinDims)/tmpfact;

   SO->MinDims[0] = (SO->MinDims[0] - SO->aMinDims)/tmpfact;
   SO->MinDims[1] = (SO->MinDims[1] - SO->aMinDims)/tmpfact;
   SO->MinDims[2] = (SO->MinDims[2] - SO->aMinDims)/tmpfact;

   SO->MaxDims[0] = (SO->MaxDims[0] - SO->aMinDims)/tmpfact;
   SO->MaxDims[1] = (SO->MaxDims[1] - SO->aMinDims)/tmpfact;
   SO->MaxDims[2] = (SO->MaxDims[2] - SO->aMinDims)/tmpfact;

   SO->aMinDims = 0.0;
   SO->aMaxDims = 100.0;
   }
   #endif
   #ifdef DO_SCALE
   /* Now do some scaling */
   SUMA_LH("In scale section");
   if ( (SO->aMaxDims - SO->aMinDims) > SUMA_TESSCON_DIFF_FLAG) {
      if (AFNI_yesenv("SUMA_TESSCON_AutoScale")) {
         fprintf (stdout,  "\n"
                           "\n"
                           "WARNING %s:\n"
                           " Assuming surface to be in tesscon units,\n"
                           " scaling down by %f.\n"
                           "\aYou might have abnormally large or small \n"
                           "freakish vertex coordinates.\n"
                           "Max/Min Dims = %f/%f\n"
                           "\n",
            FuncName, SUMA_TESSCON_TO_MM,
            SO->aMaxDims, SO->aMinDims);
         ND = SO->NodeDim;
         for (k=0; k < SO->N_Node; k++)
         {
            id = ND * k;
            SO->NodeList[id] /= SUMA_TESSCON_TO_MM;
            SO->NodeList[id+1] /= SUMA_TESSCON_TO_MM;
            SO->NodeList[id+2] /= SUMA_TESSCON_TO_MM;
         }

         SO->Center[0] /= SUMA_TESSCON_TO_MM;
         SO->Center[1] /= SUMA_TESSCON_TO_MM;
         SO->Center[2] /= SUMA_TESSCON_TO_MM;

         SO->MinDims[0] /= SUMA_TESSCON_TO_MM;
         SO->MinDims[1] /= SUMA_TESSCON_TO_MM;
         SO->MinDims[2] /= SUMA_TESSCON_TO_MM;

         SO->MaxDims[0] /= SUMA_TESSCON_TO_MM;
         SO->MaxDims[1] /= SUMA_TESSCON_TO_MM;
         SO->MaxDims[2] /= SUMA_TESSCON_TO_MM;

         SO->aMinDims /= SUMA_TESSCON_TO_MM;
         SO->aMaxDims /= SUMA_TESSCON_TO_MM;
      } else {
         SUMA_S_Note("Surface dim range a little strange.\n"
                     "Make sure dimensions are OK.\n"
                     "Max/Min Dims = %f/%f\n",
                     SO->aMaxDims, SO->aMinDims);
      }
   } 
   #endif
   
   SUMA_LHv("Checking too small a surface: %f\n", SO->MaxCentDist);
   /* check for too small a surface */
   if (SO->MaxCentDist < 10.0 && !iwarn) {
      if (!(sv = SUMA_BestViewerForADO((SUMA_ALL_DO *)SO))) sv = SUMAg_SVv;
      if (sv) { /* This can be null when surfaces are created on the fly,
                   No need to warn in that case though.*/
           ;  /* took out actual warning - mostly nuisance - 10 Oct 2017 DRG */
           if (!iwarn) ++iwarn;
/*         if (sv->GVS[sv->StdView].DimSclFac < 5 && !iwarn) {
            ++iwarn;
            SUMA_S_Note(
               "Surface size is quite small, rendering errors might occur.\n"
               "If your coordinate units are in cm, set SUMA_NodeCoordsUnits \n"
               "In your ~/.sumarc to 'cm' instead of the default 'mm'\n"
            "If you do not have a '~/.sumarc', just run 'suma -update_env'\n");
         }*/
      }
   }
   SUMA_LH("Computing normals");
   /* Calculate SurfaceNormals */
   if (SO->NodeNormList && SO->FaceNormList) {
      SUMA_LH("Node normals already computed, skipping...");
   } else {
      SN = SUMA_SurfNorm(  SO->NodeList,  SO->N_Node, 
                           SO->FaceSetList, SO->N_FaceSet );
      SO->NodeNormList = SN.NodeNormList;
      SO->FaceNormList = SN.FaceNormList;
   }
   
   /*create the structures for GL rendering */
   /*The data is being duplicated at the moment and perhaps 
      I should just stick with the 1D stuf */
   if (sizeof(GLfloat) != sizeof(float)) { 
      SUMA_SL_Crit("GLfloat and float have differing sizes!\n"); 
      SUMA_RETURN(NOPE); }
   if (sizeof(GLint) != sizeof(int)) { 
      SUMA_SL_Crit("GLint and int have differing sizes!\n"); 
      SUMA_RETURN(NOPE); }
   
   SO->glar_NodeList = (GLfloat *) SO->NodeList; /* copy pointer, not data */
   SO->glar_FaceSetList = (GLint *) SO->FaceSetList; /* copy pointer, not data */
   SO->glar_FaceNormList = (GLfloat *) SO->FaceNormList; /* copy pointer */
   SO->glar_NodeNormList = (GLfloat *) SO->NodeNormList; /* copy pointer */

   /* a surface object does contribute to the rotation 
      center of the viewer displaying it */
   SO->RotationWeight = SO->N_Node;
   SO->ViewCenterWeight = SO->N_Node;
   
   /* No selections yet, but make the preps */
      SUMA_SV_SetShowSelectedDatum(sv, YUP, NOPE);
      SUMA_SV_SetShowSelectedFaceSet(sv, YUP, NOPE);
      SO->SelectedFaceSet = -1;
      SO->SelectedNode = -1;
      /* create the ball object*/
      if (SO->NodeMarker) {
         SUMA_LH("NodeMarker already present. Skipping");
      } else {
         SO->NodeMarker = SUMA_Alloc_SphereMarker ();
      }
      if (SO->NodeMarker == NULL) {
         fprintf( SUMA_STDERR,
                  "Error%s: Could not allocate for SO->NodeMarker\n", FuncName);
         SUMA_Free_Surface_Object (SO);
         SUMA_RETURN (NOPE);
      }
      /* create the FaceSetMarker object */
      if (SO->FaceSetMarker) {
         SUMA_LH("FaceSetMarker already present. Skipping");
      } else {
         SO->FaceSetMarker = SUMA_Alloc_FaceSetMarker();
      }
      if (SO->FaceSetMarker == NULL) {
         fprintf( SUMA_STDERR,
                  "Error%s: Could not allocate for SO->FaceSetMarker\n", 
                  FuncName);
         SUMA_Free_Surface_Object (SO);
         SUMA_RETURN (NOPE);
      }
   
   /* find normal directions, if possible.
      Do not do this here, normally, I can guess well and 
      if certain about orientation
      for a particular format, set at surface reading level. 
      No need to do more computations */
      /* if (SO->normdir == 0) SO->normdir = SUMA_SurfNormDir(SO); */
         
   SUMA_RETURN(YUP);
}
   
   
/*! 
   Call the function engine, with debug turned on.      20 Oct 2003 [rickr]
*/
SUMA_SurfaceObject * SUMA_Load_Surface_Object (void *SO_FileName_vp, SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF, char *VolParName)
{/*SUMA_Load_Surface_Object*/
   static char FuncName[]={"SUMA_Load_Surface_Object"};

   SUMA_ENTRY;

   SUMA_RETURN( SUMA_Load_Surface_Object_eng( SO_FileName_vp, SO_FT, SO_FF,
                                              VolParName, 1) );
}/*SUMA_Load_Surface_Object*/


/* - appended _eng to engine function name             20 Oct 2003 [rickr]
 * - added debug parmeter
 * - only print non-error info when debug flag is set
*/
/*!
\brief
      SO = SUMA_Load_Surface_Object_eng ( SO_FileName, SO_FT, SO_FF, char *VolParName, int debug)
   
   
Input paramters : 
\param   (void *) SO_FileName 
         For SUMA_INVENTOR_GENERIC SO_FileName is (char *) containing path (if any) and filename of surface
         For SUMA_SUREFIT SO_FileName is (SUMA_SFname *) containing full topo and coord names, with path (if any)
         For SUMA_FREE_SURFER SO_FileName is  (char *) name of .asc file (with path)
         For SUMA_VEC (a dumb ascii format), SO_FileName is (SUMA_SFname *) containing the nodelist file in name_coord 
          and facesetlist file in name_topo (path included).
         For SUMA_PLY (char *) name of .ply file (with path)
         For SUMA_OPENDX_MESH (char *) name of .dx file (with path)
\param   SO_FT (SUMA_SO_File_Type) file type to be read (inventor, free surfer , Surefit )
\param   SO_FF (SUMA_SO_File_Format) Ascii or Binary (only ascii at the moment, except for .ply files)
\param   VolParName (char *) filename (+path) of parent volume, pass NULL for none
         If you pass NULL, no transformation is applied to the coordinates read.
\param   debug (int) flag specifying whether to output surface object info
   
\return   SO (SUMA_SurfaceObject *) Surface Object pointer
   The following fields are set (or initialized):
   SO->NodeDim
   SO->FaceSetDim
   SO->NodeList
   SO->FaceSetList
   SO->N_Node;
   SO->N_FaceSet;
   SO->Name;
   SO->FileType;
   SO->FileFormat
   SO->idcode_str
   SO->Center
   SO->aMaxDims
   SO->aMinDims
   SO->NodeNormList
   SO->FaceNormList
   SO->glar_NodeList
   SO->glar_FaceSetList
   SO->glar_FaceNormList
   SO->glar_NodeNormList
   SO->RotationWeight
   SO->ViewCenterWeight
   SO->SelectedFaceSet
   SO->SelectedNode
   SO->NodeMarker
   SO->FaceSetMarker
   SO->VolPar
   SO->SUMA_VolPar_Aligned   
   
\sa SUMA_IV*
\sa SUMA_Save_Surface_Object()
\sa SUMA_Align_to_VolPar()   
   
***/
SUMA_SurfaceObject * SUMA_Load_Surface_Object_eng (
      void *SO_FileName_vp, SUMA_SO_File_Type SO_FT, 
      SUMA_SO_File_Format SO_FF, char *VolParName, 
      int debug)
{/*SUMA_Load_Surface_Object_eng*/
   static char FuncName[]={"SUMA_Load_Surface_Object_eng"};
   char stmp[1000], *SO_FileName=NULL;
   SUMA_SFname *SF_FileName; 
   SUMA_SureFit_struct *SF;
   SUMA_FreeSurfer_struct *FS;
   SUMA_SO_File_Type gSO_FT;
   char *tname=NULL, *psv=NULL, *pname=NULL;
   int i1=0, par=0;
   SUMA_SurfaceObject *SO;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   /* Allocate and initialize SUMA_SurfaceObject Pointer */
   SO = SUMA_Alloc_SurfObject_Struct(1);
   
   if (SO_FT == SUMA_SUREFIT || SO_FT == SUMA_VEC) {
      tname = ((SUMA_SFname*)SO_FileName_vp)->name_coord;   
   } else {
      tname = (char *)SO_FileName_vp;
      /* A little ugly, but check if this is a template name */
      i1 = SUMA_is_predefined_SO_name(tname, &par, NULL, &psv, &pname);
      if (i1 ==  4 ) {
         tname = pname;
         VolParName = psv;
      } else {
         SUMA_ifree(pname); SUMA_ifree(psv);
      }
   }
   if (tname) {
      gSO_FT = SUMA_GuessSurfFormatFromExtension(tname, NULL);
      if (SO_FT <= SUMA_FT_NOT_SPECIFIED && gSO_FT > SO_FT) {
         SUMA_S_Notev( "Surface type not specified.\n"
                       "Format appears to be %s\n"
                       "based on filename extension.\n",
                       SUMA_SurfaceTypeString(gSO_FT));
         SO_FT = gSO_FT;
      }
      if (i1 == 4) { /* Have template surface, adopt format */
         SO_FT = gSO_FT; 
      }
      if (  gSO_FT > SUMA_FT_NOT_SPECIFIED && 
            gSO_FT != SO_FT && !pname) {
         SUMA_S_Warnv("Warning Warning MSB!!!\n"
                      "Surface file name's (%s) extension indcates a\n"
                      "surface of type %s and conflicts with specified\n"
                      "type of %s.\n"
                      "Function will attempt to proceed as if type is\n"
                      "%s.\n",
                      tname, SUMA_SurfaceTypeString(gSO_FT),
                      SUMA_SurfaceTypeString(SO_FT),
                      SUMA_SurfaceTypeString(SO_FT));     
      }
   }  
   /* check if recognizable type */
   switch (SO_FT) {
      case SUMA_INVENTOR_GENERIC:
         break;
      case SUMA_SUREFIT:
         break;
      case SUMA_FREE_SURFER:
      case SUMA_FREE_SURFER_PATCH:
         break;
      case SUMA_STL:
         break;
      case SUMA_PLY:
         break;
      case SUMA_OBJ_MESH:
         break;
      case SUMA_MNI_OBJ:
         break;
      case SUMA_OPENDX_MESH:
         break;
      case SUMA_VEC:
         break;
      case SUMA_BRAIN_VOYAGER:
         break;
      case SUMA_BYU:
         break;
      case SUMA_GIFTI:
         break;
      case SUMA_PREDEFINED:
         break;
      default:
         SUMA_error_message(FuncName, "SO_FileType not supported", 0);
         SUMA_ifree(pname); SUMA_ifree(psv);
         SUMA_RETURN (NULL);
         break;
   } /* SO_FT*/

   
   /* proceed for reading */
   switch (SO_FT) {
      case SUMA_CMAP_SO:
         /* nothing to do here */
         SUMA_SL_Err("Don't know how to read those from disk:");
         SUMA_ifree(pname); SUMA_ifree(psv);
         SUMA_RETURN(NULL);
      
      case SUMA_FT_NOT_SPECIFIED:
         fprintf (SUMA_STDERR,"Error %s: No File Type specified.\n", FuncName);
         SUMA_ifree(pname); SUMA_ifree(psv);
         SUMA_RETURN(NULL);
      
      case SUMA_N_SO_FILE_TYPE:
         fprintf (SUMA_STDERR,
                  "Error %s: This should not happen (SUMA_N_SO_FILE_TYPE)\n", 
                  FuncName);
         SUMA_ifree(pname); SUMA_ifree(psv);
         SUMA_RETURN(NULL);
      
      case SUMA_STL:
         if (!SUMA_STL_Read (tname, SO)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_STL_Read.\n", FuncName);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN(NULL);
         }
         SUMA_NEW_ID(SO->idcode_str,tname); 
         
         /* change coordinates to align them with volparent data set, 
            if possible */
         if (VolParName != NULL) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed to load parent volume attributes.\n", 
                        FuncName);
            } else {

            if (!SUMA_Align_to_VolPar (SO, NULL)) SO->SUMA_VolPar_Aligned = NOPE;
               else {
                  SO->SUMA_VolPar_Aligned = YUP;
                  /*SUMA_Show_VolPar(SO->VolPar, NULL);*/
               }
            }
         } else { 
            SO->SUMA_VolPar_Aligned = NOPE;
         }
         SO->normdir = 0;  /* not set */
         break;
      case SUMA_PLY:
         if (!SUMA_Ply_Read (tname, SO)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_Ply_Read.\n", FuncName);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN(NULL);
         }
         SUMA_NEW_ID(SO->idcode_str,tname); 
         
         /* change coordinates to align them with volparent data set, 
            if possible */
         if (VolParName != NULL) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed to load parent volume attributes.\n", 
                        FuncName);
            } else {

            if (!SUMA_Align_to_VolPar (SO, NULL)) SO->SUMA_VolPar_Aligned = NOPE;
               else {
                  SO->SUMA_VolPar_Aligned = YUP;
                  /*SUMA_Show_VolPar(SO->VolPar, NULL);*/
               }
            }
         } else { 
            SO->SUMA_VolPar_Aligned = NOPE;
         }
         SO->normdir = 0;  /* not set */
         break;
      case SUMA_MNI_OBJ:
         if (!SUMA_MNI_OBJ_Read (tname, SO)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_MNI_OBJ_Read.\n", FuncName);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN(NULL);
         }
         SUMA_NEW_ID(SO->idcode_str,tname); 
         
         /* change coordinates to align them with 
            volparent data set, if possible */
         if (VolParName != NULL) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed to load parent volume attributes.\n",
                        FuncName);
            } else {

            if (!SUMA_Align_to_VolPar (SO, NULL)) 
               SO->SUMA_VolPar_Aligned = NOPE;
               else {
                  SO->SUMA_VolPar_Aligned = YUP;
                  /*SUMA_Show_VolPar(SO->VolPar, NULL);*/
               }
            }
         } else { 
            SO->SUMA_VolPar_Aligned = NOPE;
         }
         SO->normdir = 0;  /* not set */
         break;
      case SUMA_OPENDX_MESH:
         if (!SUMA_OpenDX_Read_SO (tname, SO)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_OpenDX_Read_SO.\n", FuncName);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN(NULL);
         }
         SUMA_NEW_ID(SO->idcode_str,tname); 
         
         /* change coordinates to align them with volparent data set, 
            if possible */
         if (VolParName != NULL) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf(SUMA_STDERR,
                        "Error %s: Failed to load parent volume attributes.\n", 
                        FuncName);
            } else {

            if (!SUMA_Align_to_VolPar (SO, NULL)) SO->SUMA_VolPar_Aligned = NOPE;
               else {
                  SO->SUMA_VolPar_Aligned = YUP;
                  /*SUMA_Show_VolPar(SO->VolPar, NULL);*/
               }
            }
         } else { 
            SO->SUMA_VolPar_Aligned = NOPE;
         }

         SO->normdir = -1;  /* negative */
         break;
     case SUMA_OBJ_MESH:
         { 
            if (!(SUMA_OBJ_Read_SO(tname, SO, NULL))) {
               SUMA_S_Err("Failed to read %s", tname);
               SUMA_ifree(pname); SUMA_ifree(psv);
               SUMA_RETURN(NULL);
            }
            /* Don't change ID here, that is done inside SUMA_OBJ_Read_SO */     
            /* change coordinates to align them with volparent data set, 
               if possible */
            if (VolParName != NULL) {
               SO->VolPar = SUMA_VolPar_Attr (VolParName);
               if (SO->VolPar == NULL) {
                  SUMA_S_Err("Failed to load parent volume attributes.\n");
               } else {
                  if (!SUMA_Align_to_VolPar (SO, NULL)) 
                     SO->SUMA_VolPar_Aligned = NOPE;
                  else {
                     SO->SUMA_VolPar_Aligned = YUP;
                     /*SUMA_Show_VolPar(SO->VolPar, NULL);*/
                  }
               }
            } else { 
               SO->SUMA_VolPar_Aligned = NOPE;
            }

            SO->normdir = -1;  /* negative */
            break;
            
         }
         break;
     case SUMA_PREDEFINED:
         i1 = SUMA_is_predefined_SO_name(tname, &par, 
                                         NULL, NULL, NULL);
         switch(i1) {
            case 1:
               SO = SUMA_CreateIcosahedron(30, par, NULL, "n", 1);
               break;
            case 2:
               SO = SUMA_CreateIcosahedron(30, par, NULL, "y", 1);
               break;
            case 0:
               break;
            default:
               SUMA_S_Errv("Not ready for typ %d on %s\n",
                           i1, tname);
               SUMA_ifree(pname); SUMA_ifree(psv);
               SUMA_RETURN(NULL);         
               break;
         }
         SO->Name = SUMA_StripPath(tname);
         SO->FileType = SUMA_PREDEFINED;
         /* change coordinates to align them with volparent data set, 
            if possible */
         if (VolParName != NULL) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf(SUMA_STDERR,
                        "Error %s: Failed to load parent volume attributes.\n", 
                        FuncName);
            } else {

            if (!SUMA_Align_to_VolPar (SO, NULL)) SO->SUMA_VolPar_Aligned = NOPE;
               else {
                  SO->SUMA_VolPar_Aligned = YUP;
                  /*SUMA_Show_VolPar(SO->VolPar, NULL);*/
               }
         }
         } else { 
            SO->SUMA_VolPar_Aligned = NOPE;
         }

         SO->normdir = 1;  
         break;
         
     case SUMA_BRAIN_VOYAGER:
         if (0 && SUMA_GuessSurfFormatFromExtension(tname,     
                                                         NULL)==SUMA_GIFTI) {
            /* Allowing for cases where BrainVoyager.gii surfaces are in the same
            coordinate system as their native format and so will require
            the VolPar transform below. I did not think this should happen
            with GIFTI. The surface coords should correspond directly to those
            of the NIFTI volume 
            Exception added for Adam Greenberg, surfaces are not well 
            centered in the viewer. This will need fixing is this condition
            is to be allowed. Problem is that it appears BV's gifti surfaces
            might still be in their native coord system, not as set by
            the transform matrix in the gii file */
            SUMA_S_Warn("This should not be used regularly.\n"
                        "Surfaces will not display in the proper place\n"
                        "in SUMA.\n");
            if (!SUMA_GIFTI_Read (tname, SO, 1)) {
               fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_GIFTI_Read.\n", FuncName);
               SUMA_ifree(pname); SUMA_ifree(psv);
               SUMA_RETURN(NULL);
            }
            SO->FileType = SUMA_BRAIN_VOYAGER;
         } else {
            if (!SUMA_BrainVoyager_Read (tname, SO, 1, 1)) {
               fprintf (SUMA_STDERR,
                        "Error %s: Failed in SUMA_BrainVoyager_Read.\n", 
                        FuncName);
               SUMA_ifree(pname); SUMA_ifree(psv);
               SUMA_RETURN(NULL);
            }
         }
         SUMA_NEW_ID(SO->idcode_str,tname); 
         
         /* change coordinates to align them with volparent data set, 
            if possible */
         if (VolParName != NULL) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed to load parent volume attributes.\n",
                        FuncName);
            } else {
            if (!SUMA_Align_to_VolPar (SO, NULL)) 
               SO->SUMA_VolPar_Aligned = NOPE;
               else {
                  SO->SUMA_VolPar_Aligned = YUP;
                  /*SUMA_Show_VolPar(SO->VolPar, NULL);*/
               }
            }
         } else { 
            SO->SUMA_VolPar_Aligned = NOPE;
         }
         
         if (SO->SUMA_VolPar_Aligned) SO->normdir = 1;  /* positive */
         else SO->normdir = -1; /* negative */ 
         break;
      
      case SUMA_BYU:
         if (!SUMA_BYU_Read (tname, SO, 1, 1)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_BYU_Read.\n", FuncName);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN(NULL);
         }
         SUMA_NEW_ID(SO->idcode_str,tname); 
         
         /* change coordinates to align them with volparent data set, 
            if possible */
         if (VolParName != NULL) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed to load parent volume attributes.\n", 
                        FuncName);
            } else {

            if (!SUMA_Align_to_VolPar (SO, NULL)) 
               SO->SUMA_VolPar_Aligned = NOPE;
               else {
                  SO->SUMA_VolPar_Aligned = YUP;
                  /*SUMA_Show_VolPar(SO->VolPar, NULL);*/
               }
            }
         } else { 
            SO->SUMA_VolPar_Aligned = NOPE;
         }
         
         SO->normdir = SUMA_SurfNormDir(SO);  /* guess */
         break;
      
      case SUMA_GIFTI:
         if (!SUMA_GIFTI_Read (tname, SO, 1)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_GIFTI_Read.\n", FuncName);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN(NULL);
         }
         SUMA_NEW_ID(SO->idcode_str,tname); 
         
         /* change coordinates to align them with volparent data set, 
            if possible */
         if (VolParName != NULL) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed to load parent volume attributes.\n", 
                        FuncName);
            } else {

            if (!SUMA_Align_to_VolPar (SO, NULL)) 
               SO->SUMA_VolPar_Aligned = NOPE;
               else {
                  SO->SUMA_VolPar_Aligned = YUP;
                  /*SUMA_Show_VolPar(SO->VolPar, NULL);*/
               }
            }
         } else { 
            SO->SUMA_VolPar_Aligned = NOPE;
         }
         
         SO->normdir = SUMA_SurfNormDir(SO);  /* guess */
         break;
               
      case SUMA_INVENTOR_GENERIC:
         SO_FileName = tname;
         /* You need to split name into path and name ... */
	      if ( debug )
            fprintf(stdout,"%s\n", SO_FileName);
         SO->Name = SUMA_StripPath(SO_FileName);
         /* check for file existence  */
         if (!SUMA_filexists(SO_FileName)) {
            snprintf(stmp,998,"File %s not found!", SO_FileName);
            SUMA_error_message(FuncName, stmp, 0);
            SUMA_RETURN (NULL);
         }
         SO->FileType = SO_FT;
         SO->FileFormat = SO_FF;
         SO->NodeDim = 3; /* This must be automated */
         SO->NodeList = SUMA_IV_XYZextract (SO_FileName, &(SO->N_Node), 0);
         if (SO->NodeList == NULL) {
            SUMA_error_message(FuncName,"SUMA_IV_XYZextract failed!",0);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN(NULL);
         }
         SO->FaceSetList = SUMA_IV_FaceSetsextract (SO_FileName, 
                                                   &(SO->N_FaceSet));
         if (SO->FaceSetList == NULL) {
            SUMA_error_message(FuncName,"SUMA_IV_FaceSetsextract failed!",0);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN(NULL);
         }
         SO->FaceSetDim = 3; /*This must also be automated */
         SUMA_NEW_ID(SO->idcode_str,SO_FileName); 

         SO->normdir = 0;  /* not set */
         break;
         
      case SUMA_FREE_SURFER:
      case SUMA_FREE_SURFER_PATCH:
         /* Allocate for FS */
         FS = (SUMA_FreeSurfer_struct *)
                  SUMA_malloc(sizeof(SUMA_FreeSurfer_struct));   
         if (FS == NULL) {
            fprintf( SUMA_STDERR,
                     "Error %s: Failed to allocate for FS\n", FuncName);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN (NULL);
         }
         /* add a couple of lines to appease the optimation gods...         */
         /* (suma, v2s, SSmooth were crashing on FC7)   23 Jan 2008 [rickr] */
         memset(FS, 0, sizeof(SUMA_FreeSurfer_struct));
         if(debug > 1) fprintf(stderr,"-- optimization appeasement message\n");
         SO->Name = SUMA_StripPath((char*)SO_FileName_vp);
         SO->FileType = SO_FT;
         SO->FileFormat = SO_FF;
         SO->NodeDim = 3; /* This must be automated */
         /*read the surface file */
         if (SO->FileFormat == SUMA_ASCII) {
            if (!SUMA_FreeSurfer_Read_eng((char*)SO_FileName_vp, FS, debug)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed in SUMA_FreeSurfer_Read.\n",
                        FuncName);
               SUMA_ifree(pname); SUMA_ifree(psv);
               SUMA_RETURN (NULL);
            }
         } else if (SO->FileFormat == SUMA_BINARY_BE) {
            if (!SUMA_FreeSurfer_ReadBin_eng((char*)SO_FileName_vp, FS, debug)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed in SUMA_FreeSurfer_Read.\n", 
                        FuncName);
               SUMA_ifree(pname); SUMA_ifree(psv);
               SUMA_RETURN (NULL);
            }
         } else {
            SUMA_SL_Err("Format not supported.");
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN (NULL);
         }
	 if ( debug > 1)
            SUMA_Show_FreeSurfer (FS, NULL);
         /* save the juice and clean up the rest */
         SO->N_Node = FS->N_Node;
         /* Save the pointers to NodeList and FaceSetList and 
            clear what is left of FS structure at the end */
         SO->NodeList = FS->NodeList;
         FS->NodeList = NULL;
         SO->FaceSetList = FS->FaceSetList;
         SO->N_FaceSet = FS->N_FaceSet;
         FS->FaceSetList = NULL;
         SO->FaceSetDim = 3; /*This must also be automated */
         
         
         /* change coordinates to align them with volparent data set, 
            if possible */
         if (VolParName != NULL) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed to load parent volume attributes.\n", 
                        FuncName);
            } else {
               if (!SUMA_Align_to_VolPar (SO, (void*)FS)) 
                  SO->SUMA_VolPar_Aligned = NOPE;
                  else {
                     SO->SUMA_VolPar_Aligned = YUP;
                     /*SUMA_Show_VolPar(SO->VolPar, NULL);*/
                  }
            }
         } else { 
            SO->SUMA_VolPar_Aligned = NOPE;
         }
         SO->normdir = 1; /* normals point out */
         /* free FS */
         if (!SUMA_Free_FreeSurfer (FS)) {
            fprintf( SUMA_STDERR,
                     "Error %s: Failed in SUMA_Free_FreeSurfer.\n", FuncName);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN (NULL);
         }
         
         /* create the IDcode */
         SUMA_NEW_ID(SO->idcode_str, tname);
         if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: Assigned idcode_str:%s:.\n", FuncName, SO->idcode_str);

         SO->normdir = 1;  /* positive */
         break;
         
      case SUMA_VEC:
         /* naming is with two files, similar to SureFit */
         SF_FileName = (SUMA_SFname *)SO_FileName_vp;      
         /* form the topo and the coord names */
         SO->Name_coord = SUMA_StripPath(SF_FileName->name_coord);
         SO->Name_topo = SUMA_StripPath(SF_FileName->name_topo);
         SO->FileType = SO_FT;
         SO->FileFormat = SO_FF;
         SO->NodeDim = 3; /* This must be automated */
         /* check for files */
         if (!SUMA_filexists(SF_FileName->name_coord)) {
            fprintf(SUMA_STDERR,"Error %s: Could not find %s\n", 
                     FuncName, SF_FileName->name_coord);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN (NULL);
         }
         if (!SUMA_filexists(SF_FileName->name_topo)) {
            fprintf(SUMA_STDERR,"Error %s: Could not find %s\n", 
                     FuncName, SF_FileName->name_topo);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN (NULL);
         }
         
         if (!SUMA_VEC_Read(SF_FileName, SO)) {
            SUMA_SLP_Err("Failed to read 1D file");
            if (SO->NodeList) SUMA_free(SO->NodeList);
            if (SO->FaceSetList) SUMA_free(SO->FaceSetList);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN (NULL);
         }
                  
         sprintf (stmp, "%s%s", SF_FileName->name_coord, SF_FileName->name_topo);
         SUMA_NEW_ID(SO->idcode_str, stmp);
         
         /* change coordinates to align them with volparent data set, 
            if possible */
         if (VolParName != NULL) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed to load parent volume attributes.\n", 
                        FuncName);
            } else {

            if (!SUMA_Align_to_VolPar (SO, NULL)) SO->SUMA_VolPar_Aligned = NOPE;
               else {
                  SO->SUMA_VolPar_Aligned = YUP;
                  /*SUMA_Show_VolPar(SO->VolPar, NULL);*/
               }
         }
         } else { 
            SO->SUMA_VolPar_Aligned = NOPE;
         }

         SO->normdir = 0;  /* not set */
         break;
         
      case SUMA_FT_ERROR:
         SUMA_SL_Err("Error specifying file type.");
         break;
         
      case SUMA_SUREFIT:
         /* Allocate for SF */
         SF = (SUMA_SureFit_struct *) SUMA_calloc(1, 
                                        sizeof(SUMA_SureFit_struct));   
         if (SF == NULL) {
            fprintf( SUMA_STDERR,
                     "Error %s: Failed to allocate for SF\n", FuncName);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN (NULL);
         }
         SF->NodeList= NULL;
         SF->NodeId = NULL;
         SF->allzerocoord = NULL;
         SF->Specs_mat = NULL;
         SF->FaceSetList = NULL;
         SF->FN.N_Neighb = NULL;
         SF->FN.FirstNeighb = NULL;
         SF->FN.NodeId = NULL;
         SF->caret_version = -1.0;
         SF_FileName = (SUMA_SFname *)SO_FileName_vp;
         /* form the topo and the coord names */
         SO->Name_coord = SUMA_StripPath(SF_FileName->name_coord);
         SO->Name_topo = SUMA_StripPath(SF_FileName->name_topo);
         SO->FileType = SO_FT;
         SO->FileFormat = SO_FF;
         SO->NodeDim = 3; /* This must be automated */
         
         /*read the coordinate file */
         if (!SUMA_SureFit_Read_Coord (SF_FileName->name_coord, 
                                       SF)) {
            fprintf( SUMA_STDERR,
                     "Error %s: Failed in SUMA_SureFit_Read_Coord.\n", FuncName);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN (NULL);
         }
         /* copy the pertinent data to SO */
         SO->N_Node = SF->N_Node;
         /* Save the pointers to NodeList and FaceSetList and 
            clear what is left of SF structure at the end */
         SO->NodeList = SF->NodeList;
         SF->NodeList = NULL;

         
         /*read the topology file */
         if (!SUMA_SureFit_Read_Topo (SF_FileName->name_topo, SF)) {
            fprintf( SUMA_STDERR,
                     "Error %s: Failed in SUMA_SureFit_Read_Topo.\n", FuncName);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN (NULL);
         }
         
         
         /* read the param file */
         if (strlen(SF_FileName->name_param)){
            if (!SUMA_Read_SureFit_Param(SF_FileName->name_param, SF)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed in SUMA_Read_SureFit_Param.\n", 
                        FuncName);
            }
         } else {
            if (VolParName != NULL) {
               SUMA_S_Note("Volume Parent specified without .param file.\n"
                     "Be sure surface and volume line up.");
            }
         }
         
         /* copy the pertinent data to SO */
         SO->FaceSetList = SF->FaceSetList;
         SO->N_FaceSet = SF->N_FaceSet;
         SF->FaceSetList = NULL;
         SO->FaceSetDim = 3; /*This must also be automated */
         
         /* change coordinates to align them with volparent data set, 
            if possible */
         if (VolParName != NULL ) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed to load parent volume attributes.\n", 
                        FuncName);
            } else {
               /*SUMA_Show_VolPar(SO->VolPar, NULL);*/

               if (!SUMA_Align_to_VolPar (SO, (void *)SF)) 
                  SO->SUMA_VolPar_Aligned = NOPE;
               else 
                  SO->SUMA_VolPar_Aligned = YUP;
            }
         } else { 
            /* OK to go there with new caret files */
            if (!SUMA_Align_to_VolPar (SO, (void *)SF)) 
               SO->SUMA_VolPar_Aligned = NOPE;
            else 
               SO->SUMA_VolPar_Aligned = YUP;
         }
         
         sprintf (stmp, "%s%s", SF_FileName->name_coord, SF_FileName->name_topo);
         SUMA_NEW_ID(SO->idcode_str, stmp);

         if ((int) SF->tag_version == 1) { SO->normdir = 1; }
         else SO->normdir = -1;
         
         /* SUMA_Show_SureFit(SF, SUMA_STDERR); */
         
         /* free SF */
         if (!SUMA_Free_SureFit (SF)) {
            fprintf( SUMA_STDERR,
                     "Error %s: Failed in SUMA_Free_SureFit.\n", FuncName);
            SUMA_ifree(pname); SUMA_ifree(psv);
            SUMA_RETURN (NULL);
         }
         
         break;
   } /* SO_FileType*/
   
   /* sanity check (this one's here for a reason) */
   if (SO->N_Node <=0 || SO->N_FaceSet<=0) {
      SUMA_SL_Crit("0 nodes or 0 facesets.\nProceed I will not.\n");
      SUMA_Free_Surface_Object (SO);
      SUMA_ifree(pname); SUMA_ifree(psv);
      SUMA_RETURN (NULL);
   }

   /* A trick for Stephen Tyree, Pat Bellgowan, and other mad men */
   if (AFNI_yesenv("SUMA_FLIP_X_COORD")) {
      int i, id;
      for (i=0; i < SO->N_Node; ++i) {
            id = i * SO->NodeDim;
            SO->NodeList[id] = -SO->NodeList[id];
      }
      SO->normdir = -SO->normdir;
   }
   if (SO->isSphere == SUMA_GEOM_NOT_SET) { 
      SUMA_SetSphereParams(SO, -0.1); 
   }  /* sets the spheriosity parameters */

   

   if (!SUMA_PrepSO_GeomProp_GL (SO)) {
      SUMA_SL_Err("Failed to set surface's properties");
      SUMA_ifree(pname); SUMA_ifree(psv);
      SUMA_RETURN (NULL);
   }
   
      
   SUMA_ifree(pname); SUMA_ifree(psv);
   SUMA_RETURN (SO);
   
}/*SUMA_Load_Surface_Object_eng*/

/*!< Look for datasets names exactly like the surface but with
a dataset extension and load them if found*/
SUMA_Boolean SUMA_AutoLoad_SO_Dsets(SUMA_SurfaceObject *SO)   
{
   static char FuncName[]={"SUMA_AutoLoad_SO_Dsets"};
   char *ddd=NULL, *ddde=NULL, *soname=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   soname = SUMA_SurfaceFileName(SO, 1);
   if (!soname) soname = SUMA_copy_string("No_SO_name.gii");
   ddd = SUMA_RemoveSurfNameExtension (soname, SO->FileType);
   SUMA_LH("Checking for %s dsets on root %s", soname, ddd);
   if (SUMA_filexists((ddde = SUMA_append_string(ddd,".niml.dset")))) {
      SUMA_S_Note("Auto Loading %s onto %s", ddde, soname);
      SUMA_LoadDsetOntoSO(ddde, (void *)SO);
      SUMA_ifree(ddde);
   }
   if (ddde) {/* try again */
      SUMA_ifree(ddde);
      if (SUMA_filexists((ddde = SUMA_append_string(ddd,".1D.dset")))) {
         SUMA_S_Note("Auto Loading %s onto %s", ddde, soname);
         SUMA_LoadDsetOntoSO(ddde, (void *)SO);
         SUMA_ifree(ddde);
      }
   }
   if (ddde) {/* try again */
      SUMA_ifree(ddde);
      if (SUMA_filexists((ddde = SUMA_append_string(ddd,".gii.dset")))) {
         SUMA_S_Note("Auto Loading %s onto %s", ddde, soname);
         SUMA_LoadDsetOntoSO(ddde, (void *)SO);
         SUMA_ifree(ddde);
      }
   }
   if (ddde) {/* try again */
      SUMA_ifree(ddde);
      if ( SO->FileType != SUMA_GIFTI &&
           SUMA_filexists((ddde = SUMA_append_string(ddd,".gii")))) {
         SUMA_S_Note("Auto Loading %s onto %s", ddde, soname);
         SUMA_LoadDsetOntoSO(ddde, (void *)SO);
         SUMA_ifree(ddde);
      }
   }
   SUMA_ifree(ddd);
   SUMA_ifree(soname);
   if (!ddde) SUMA_RETURN(YUP); /* got one */

   SUMA_ifree(ddde);
   SUMA_RETURN(NOPE);
}
   

 
/*!
   SUMA_Boolean SUMA_ParseLHS_RHS (char *s, char *lhs, char *rhs)
   
   Parses S of the form "lhs = rhs" 
   s, lhs and rhs must be allocated for
   
   \param s (char *) "joe = fred"
   \param lhs (char *) "joe" 
                     if you send in lhs[0] = '\0'; it gets 
                     filled with joe for you.
   \param rhs (char *) returned "fred"
   \ret YUP/NOPE for goodness, badness
   
*/
SUMA_Boolean SUMA_ParseLHS_RHS (char *s, char *lhs, char *rhs)
{
   static char FuncName[]={"SUMA_ParseLHS_RHS"};
   char *op=NULL, *op2=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
      
   if (s == NULL) {
      fprintf(SUMA_STDERR,"Error %s: NULL s\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   rhs[0] = '\0';
   
   /* skip first blank */
   SUMA_SKIP_BLANK(s, NULL);
   
   /* seek first = */
   SUMA_LHv("Have >%s<\n", s);
   op=s;
   SUMA_SKIP_TO_NEXT_CHAR(s,NULL,'=');
   if (lhs[0] == '\0') { /* load left hand side */
      SUMA_FILL_STRING(op, s, lhs);
      SUMA_DEBLANK_RHS(lhs);
      SUMA_LHv("LHS now: >%s<\n", lhs);
   }
   if (*s != '=') {
      SUMA_LH("No equal, no right hand side\n");
      SUMA_RETURN (NOPE);    
   }   
   /* get all the rhs */
   ++s; 
   SUMA_SKIP_BLANK(s, NULL);
   SUMA_FILL_STRING(s, NULL, rhs);
   /* deblank end of rhs */
   SUMA_DEBLANK_RHS(rhs);
   
   SUMA_LHv("Have %s, lhs>%s<, rhs >%s<\n", s, lhs, rhs);
   SUMA_RETURN (YUP); 
}

/* Undo the separation of left/right deformed states
                  the _lh and _rh are what @SUMA_Make_Spec_FS used
                  to flag these special states */
#define SUMA_LH_UNIFY_STATE_FIX(ss) { \
   SUMA_CropExtension((ss), "_lh_lh"); /* don't ask */\
   SUMA_CropExtension((ss), "_rh_rh"); /* don't ask */\
   SUMA_CropExtension((ss), "_lh"); \
   SUMA_CropExtension((ss), "_rh"); \
}

/*! 
   Function to read the surface specs file.
   \param fname (char *) name of the specs file
   \param Spec (SUMA_SurfSpecFile *) pre-allocated pointer to 
          SUMA_SurfSpecFile structure. )
   \ret YUP, good, NOPE, not good
*/
SUMA_Boolean SUMA_Read_SpecFile (
                  char *f_name, SUMA_SurfSpecFile * Spec)
{/* SUMA_Read_SpecFile */
   static char FuncName[]={"SUMA_Read_SpecFile"};
   char  s[SUMA_MAX_DIR_LENGTH], stmp[SUMA_MAX_DIR_LENGTH],
         stmp2[SUMA_MAX_DIR_LENGTH], c;
   int ex, skp, evl, i, kkk, LHunify=0;
   FILE *sf_file;
   SUMA_FileName SpecName;
   SUMA_Boolean onGIFTIalert = NOPE;
   SUMA_Boolean   OKread_SurfaceFormat, OKread_SurfaceType, 
                  OKread_TopoFile, OKread_CoordFile;
   SUMA_Boolean   OKread_MappingRef, OKread_SureFitVolParam,
                  OKread_FreeSurferSurface, OKread_InventorSurface;
   SUMA_Boolean   OKread_Group, OKread_State, OKread_EmbedDim,
                  OKread_SurfaceVolume, OKread_SurfaceLabel;
   SUMA_Boolean   OKread_AnatCorrect, OKread_Hemisphere,
                  OKread_DomainGrandParentID, OKread_OriginatorID;
   SUMA_Boolean   OKread_LocalCurvatureParent, OKread_LocalDomainParent,
                  OKread_LabelDset, OKread_NodeMarker;
   char DupWarn[]={  "Bad format in specfile "
                     "(you may need a NewSurface line). "
                     "Duplicate specification of"};
   char NewSurfWarn[]={ "Bad format in specfile. "
                        "You must start with NewSurface line "
                        "before any other field."};
   SUMA_Boolean LocalHead = NOPE;   

   SUMA_ENTRY;

   if (SUMA_isEnv("SUMA_LHunify","YES")) LHunify = 1;

   /*make sure file is there */
   if (!SUMA_filexists(f_name)) {
      fprintf(SUMA_STDERR,
               "Error %s: File %s does not exist or cannot be read.\n",
               FuncName, f_name);
      SUMA_RETURN (NOPE);
   }
   
   if (Spec->N_Surfs != -1) {
      fprintf(SUMA_STDERR,
               "Error %s: Spec is not fresh after SUMA_AllocSpecFields.\n"
               "Perhaps you have redundant definitions of the input surface\n",
               FuncName);
      SUMA_RETURN (NOPE);
   }
   
   Spec->N_Surfs = 0;
   
   /* set the path for the spec file */
   SpecName = SUMA_StripPath (f_name);
   if (strlen(SpecName.Path) > SUMA_MAX_DIR_LENGTH-1) {
      fprintf( SUMA_STDERR,
               "Error %s: Path of specfile > %d charcters.\n", 
               FuncName, SUMA_MAX_DIR_LENGTH-1);
      SUMA_RETURN (NOPE);
   }
   if (strlen(SpecName.FileName) > SUMA_MAX_NAME_LENGTH-1) {
      fprintf( SUMA_STDERR,
               "Error %s: Name of specfile > %d charcters.\n", 
               FuncName, SUMA_MAX_NAME_LENGTH-1);
      SUMA_RETURN (NOPE);
   }
   snprintf(Spec->SpecFilePath,SUMA_MAX_DIR_LENGTH*sizeof(char), 
            "%s", SpecName.Path);
   snprintf(Spec->SpecFileName,SUMA_MAX_NAME_LENGTH*sizeof(char), 
            "%s", SpecName.FileName);
   
   if (SUMA_iswordin(Spec->SpecFileName,"N27")) onGIFTIalert = 1;
   
   /* free SpecName since it's not used elsewhere */
   if (SpecName.Path) SUMA_free(SpecName.Path);
   if (SpecName.FileName) SUMA_free(SpecName.FileName);

   /*read the thing*/
   sf_file = fopen (f_name,"r");
   if (sf_file == NULL)
      {
         fprintf( SUMA_STDERR,
                  "Error %s: Could not open file for read\n", FuncName);
         SUMA_RETURN (NOPE);
      }
   
   /*read until you find not a comment */
 
   /* read the first line, skipping leading space */
   do {
      ex = fscanf (sf_file,"%c",&c);
   } while (ex != EOF && isspace(c));
   
   i=0;
   while (ex != EOF && c != '\n') {   
         s[i] = c; ++i;
      ex = fscanf (sf_file,"%c",&c);
   }
   s[i] = '\0';
   if (LocalHead) fprintf(SUMA_STDERR,"Read %s\n", s);
   OKread_Group = YUP; /* it is OK to read a group before 
                           a new surface is declared */
   OKread_SurfaceFormat = OKread_SurfaceType = 
      OKread_TopoFile = OKread_CoordFile = NOPE;
   OKread_MappingRef = OKread_SureFitVolParam = 
      OKread_FreeSurferSurface = OKread_InventorSurface = NOPE;
   OKread_State = OKread_EmbedDim = OKread_SurfaceVolume = 
      OKread_SurfaceLabel = NOPE ;
   OKread_AnatCorrect = OKread_Hemisphere = OKread_DomainGrandParentID =
      OKread_OriginatorID = NOPE;
   OKread_LocalCurvatureParent = OKread_LocalDomainParent = 
      OKread_LabelDset = OKread_NodeMarker = NOPE;
   
   Spec->StateList[0] = '\0';
   Spec->Group[0][0] = '\0';
   Spec->N_Surfs = Spec->N_States = Spec->N_Groups = 0;
   while (ex !=EOF) {
      evl = SUMA_iswordin (s,"#");
      if (evl != 1) { /* not a comment */
         if (LocalHead) fprintf(SUMA_STDERR,"Not a comment: %s\n", s);
         skp = 0;
         sprintf(stmp,"NewSurface");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            if(Spec->N_Surfs >= SUMA_MAX_N_SURFACE_SPEC) {
               fprintf( SUMA_STDERR,
                        "Error %s: Cannot read in more than %d new surfaces.\n",
                        FuncName, SUMA_MAX_N_SURFACE_SPEC);
               SUMA_RETURN (NOPE);
            }
            Spec->N_Surfs += 1;
            if (LocalHead) 
               fprintf( SUMA_STDERR,
                        "Found New Surface, N = %d\n", Spec->N_Surfs);
            /* initialize some of the fields */
            if (Spec->N_Surfs == 1) { /* first surface, initialize to empty */
               sprintf(Spec->SurfaceFormat[Spec->N_Surfs-1],"ASCII");
               Spec->SurfaceType[Spec->N_Surfs-1][0] = '\0';
               Spec->TopoFile[Spec->N_Surfs-1][0] =
                  Spec->CoordFile[Spec->N_Surfs-1][0] = '\0';
               Spec->MappingRef[Spec->N_Surfs-1][0] = '\0';  /* Should become
                                                                obsolete, 
                                                                ZSS Jan 02 03 */
               Spec->SureFitVolParam[Spec->N_Surfs-1][0] = '\0';
               Spec->SurfaceFile[Spec->N_Surfs-1][0] = '\0';
               Spec->State[Spec->N_Surfs-1][0] = '\0';
               Spec->IDcode[Spec->N_Surfs-1] = NULL; /* this field is set in
                                                         LoadSpec function */
               Spec->EmbedDim[Spec->N_Surfs-1] = 3;
               Spec->VolParName[Spec->N_Surfs-1][0] = '\0';
               Spec->SurfaceLabel[Spec->N_Surfs-1][0] = '\0';
               Spec->AnatCorrect[Spec->N_Surfs-1][0] = '\0';
               Spec->Hemisphere[Spec->N_Surfs-1][0] = '\0';
               Spec->DomainGrandParentID[Spec->N_Surfs-1][0] = '\0';
               Spec->OriginatorID[Spec->N_Surfs-1][0] = '\0';
               Spec->LocalCurvatureParent[Spec->N_Surfs-1][0] = '\0'; 
               Spec->LocalDomainParent[Spec->N_Surfs-1][0] = '\0';
               Spec->LabelDset[Spec->N_Surfs-1][0] = '\0';
               Spec->NodeMarker[Spec->N_Surfs-1][0] = '\0';
            } else { 
               /* make sure important fields have been filled */
               if (Spec->SurfaceType[Spec->N_Surfs-2][0] == '\0') {
                  fprintf(SUMA_STDERR,
                           "Error %s: Failed to specify surface type "
                           "for surface %d\n", 
                           FuncName, Spec->N_Surfs-2);
                  SUMA_RETURN (NOPE);
               }
               /* initilize SOME of the fields to previous one */
               Spec->CoordFile[Spec->N_Surfs-1][0] = '\0';  /* *** BA, Dec 03 */
               Spec->SurfaceFile[Spec->N_Surfs-1][0] = '\0';/* *** BA, Dec 03 */
               
               strcpy(  Spec->SurfaceFormat[Spec->N_Surfs-1],
                        Spec->SurfaceFormat[Spec->N_Surfs-2]);
               strcpy(  Spec->SurfaceType[Spec->N_Surfs-1],
                        Spec->SurfaceType[Spec->N_Surfs-2]);
               strcpy(  Spec->TopoFile[Spec->N_Surfs-1],
                        Spec->TopoFile[Spec->N_Surfs-2]);
               strcpy(  Spec->MappingRef[Spec->N_Surfs-1],
                        Spec->MappingRef[Spec->N_Surfs-2]);/*  Should become
                                                               obsolete, 
                                                               ZSS Jan 02 03 */
               strcpy(  Spec->SureFitVolParam[Spec->N_Surfs-1],
                        Spec->SureFitVolParam[Spec->N_Surfs-2]);
               Spec->VolParName[Spec->N_Surfs-1][0] = '\0'; 
                     /* it is confusing to users to inherit 
                        this one from the pervious, keep it separate.*/
               Spec->IDcode[Spec->N_Surfs-1] = NULL; /*  this field is set in
                                                         LoadSpec function */
               Spec->SurfaceLabel[Spec->N_Surfs-1][0] = '\0';
               strcpy(  Spec->Group[Spec->N_Surfs-1],
                        Spec->Group[Spec->N_Surfs-2]);
               strcpy(  Spec->State[Spec->N_Surfs-1], 
                        Spec->State[Spec->N_Surfs-2]);
               Spec->EmbedDim[Spec->N_Surfs-1] =
                  Spec->EmbedDim[Spec->N_Surfs-2];
               /* perhaps make these inheritable from previous */
               Spec->AnatCorrect[Spec->N_Surfs-1][0] = '\0';
               Spec->Hemisphere[Spec->N_Surfs-1][0] = '\0';
               Spec->DomainGrandParentID[Spec->N_Surfs-1][0] = '\0';
               Spec->OriginatorID[Spec->N_Surfs-1][0] = '\0';
               Spec->LocalCurvatureParent[Spec->N_Surfs-1][0] = '\0'; 
               Spec->LocalDomainParent[Spec->N_Surfs-1][0] = '\0';
               Spec->LabelDset[Spec->N_Surfs-1][0] = '\0';
               Spec->NodeMarker[Spec->N_Surfs-1][0] = '\0';
               /* only Spec->CoordFile, Spec->SurfaceFile MUST be 
                  specified with a new surface */
            } 
            OKread_SurfaceFormat = OKread_SurfaceType = OKread_TopoFile =
               OKread_CoordFile = YUP;
            OKread_MappingRef = OKread_SureFitVolParam =
               OKread_FreeSurferSurface = OKread_InventorSurface = YUP;
            OKread_Group = OKread_State = OKread_EmbedDim = 
               OKread_SurfaceLabel = OKread_SurfaceVolume = YUP;
            OKread_AnatCorrect = OKread_Hemisphere = 
               OKread_DomainGrandParentID = OKread_OriginatorID = YUP;
            OKread_LocalCurvatureParent = OKread_LocalDomainParent = 
               OKread_LabelDset = OKread_NodeMarker = YUP;
            skp = 1;
         }
         
         sprintf(stmp,"StateDef");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found a state definition, parse it 
            Use Spec->State[0] as buffer, since SurfaceState comes later*/
            if (LocalHead) 
               fprintf( SUMA_STDERR,
                        "%s: Sending %p )\n", 
                        FuncName, Spec->State[0]);
            if (!SUMA_ParseLHS_RHS (
                     s, stmp, 
                     Spec->State[0])) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", 
                        FuncName);
               SUMA_RETURN (NOPE);
            }
            
            if (LHunify) { /* remove the separation of left/right deformed states
                              the _lh and _rh are what @SUMA_Make_Spec_FS used
                              to flag these special states */
               SUMA_LH_UNIFY_STATE_FIX(Spec->State[0]);
            }
            if (Spec->N_States == 0) {
               /* first state, add it to the list of states */
               sprintf(Spec->StateList, "%s|", Spec->State[0]);
               Spec->N_States += 1;
            } else  {
               if (strcmp(Spec->StateList, Spec->State[0]) == 0) {
                  /* it's a duplicate, complain and get outa here */
                  fprintf( SUMA_STDERR,
                           "Error %s: Duplicate StateDef (%s).\n", 
                           FuncName, Spec->State[0]);
                  SUMA_RETURN (NOPE);
               } else {
                  /* a new one, add it to the list and 
                     increment States counter */
                  Spec->StateList = 
                     SUMA_append_replace_string(
                           Spec->StateList, "|", 
                           Spec->State[0], 1);
                  Spec->N_States += 1;
               }
            }
            Spec->State[0][0] = '\0';
            skp = 1;
         }
         
         sprintf(stmp,"Group");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found a group definition, parse it */
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: Found %s.\n", FuncName, stmp);
            if (Spec->N_Surfs < 1) { /* no surfaces have been defined yet, 
                                        group goes for all */
               if (!SUMA_ParseLHS_RHS (
                     s, stmp, 
                     Spec->Group[0])) {
                  fprintf( SUMA_STDERR,
                           "Error %s: Error in SUMA_ParseLHS_RHS.\n", 
                           FuncName);
                  SUMA_RETURN (NOPE);
               }
            } else {
               if (!SUMA_ParseLHS_RHS (
                     s, stmp, 
                     Spec->Group[Spec->N_Surfs-1])) {
                  fprintf( SUMA_STDERR,
                           "Error %s: Error in SUMA_ParseLHS_RHS.\n", 
                           FuncName);
                  SUMA_RETURN (NOPE);
               }
            }

            if (Spec->N_Surfs < 1) Spec->N_Groups += 1;
            else {   /* recount groups */
               Spec->N_Groups = 1;
               for (kkk=0; kkk< Spec->N_Surfs -1; ++kkk) {
                  if (strcmp(Spec->Group[kkk], Spec->Group[0])) {
                     Spec->N_Groups += 1; /* Have new group */
                     break;
                  }
               }
            }
            
            if (!OKread_Group) {
               fprintf( SUMA_STDERR,"Error %s: %s %s\n", 
                        FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_Group = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"Anatomical");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found Anatomically Correct field, parse it */
            if (!SUMA_ParseLHS_RHS (s, stmp,
                                    Spec->AnatCorrect[Spec->N_Surfs-1])) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            if (  strcmp(Spec->AnatCorrect[Spec->N_Surfs-1],"Y") &&
                  strcmp(Spec->AnatCorrect[Spec->N_Surfs-1],"N")) {
               SUMA_SL_Err("Anatomical can only be Y ot N");
               SUMA_RETURN (NOPE);
            }
            if (!OKread_AnatCorrect) {
               fprintf( SUMA_STDERR,"Error %s: %s %s\n", 
                        FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else  {
               OKread_AnatCorrect = NOPE;
            }
            skp = 1;
         } 
         
         sprintf(stmp,"Hemisphere");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found Hemisphere field, parse it */
            if (!SUMA_ParseLHS_RHS (
                     s, stmp, 
                     Spec->Hemisphere[Spec->N_Surfs-1])) {
               fprintf(SUMA_STDERR,
                  "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            if (  strcmp(Spec->Hemisphere[Spec->N_Surfs-1],"L") && 
                  strcmp(Spec->Hemisphere[Spec->N_Surfs-1],"R") && 
                  strcmp(Spec->Hemisphere[Spec->N_Surfs-1],"B")) {
               SUMA_SL_Err("Hemisphere can only be L or R or B");
               SUMA_RETURN (NOPE);
            }
            if (!OKread_Hemisphere) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else  {
               OKread_Hemisphere = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"DomainGrandParentID");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found DomainGrandParentID field, parse it */
            if (!SUMA_ParseLHS_RHS (
                     s, stmp, 
                     Spec->DomainGrandParentID[Spec->N_Surfs-1])) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            
            if (!OKread_DomainGrandParentID) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else  {
               OKread_DomainGrandParentID = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"OriginatorID");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found OriginatorID  field, parse it */
            if (!SUMA_ParseLHS_RHS (
                     s, stmp, 
                     Spec->OriginatorID[Spec->N_Surfs-1])) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            
            if (!OKread_OriginatorID) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else  {
               OKread_OriginatorID = NOPE;
            }
            skp = 1;
         }

         sprintf(stmp,"LocalCurvatureParent");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found LocalCurvatureParent  field, parse it */
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            snprintf (Spec->LocalCurvatureParent[Spec->N_Surfs-1], 
                        SUMA_MAX_FP_NAME_LENGTH * sizeof(char), 
                     "%s%s", Spec->SpecFilePath, stmp2);
                     
            if (!OKread_LocalCurvatureParent) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else  {
               OKread_LocalCurvatureParent = NOPE;
            }
            skp = 1;
         }

         sprintf(stmp,"LocalDomainParent");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found LocalDomainParent  field, parse it */
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            
            snprintf (Spec->LocalDomainParent[Spec->N_Surfs-1], 
                        SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
               "%s%s", Spec->SpecFilePath, stmp2);
            
            if (!OKread_LocalDomainParent) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else  {
               OKread_LocalDomainParent = NOPE;
            }
            skp = 1;
         }

         sprintf(stmp,"LabelDset");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found LabelDset  field, parse it */
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            
            snprintf (Spec->LabelDset[Spec->N_Surfs-1], 
                        SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
               "%s%s", Spec->SpecFilePath, stmp2);
            
            if (!OKread_LabelDset) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else  {
               OKread_LabelDset = NOPE;
            }
            skp = 1;
         }

         sprintf(stmp,"NodeMarker");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found NodeMarker  field, parse it */
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            
            snprintf (Spec->NodeMarker[Spec->N_Surfs-1], 
                        SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
               "%s%s", Spec->SpecFilePath, stmp2);
            
            if (!OKread_NodeMarker) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else  {
               OKread_NodeMarker = NOPE;
            }
            skp = 1;
         }

         
         sprintf(stmp,"EmbedDimension");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found surface embedding dimension, parse it */
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            Spec->EmbedDim[Spec->N_Surfs-1] = atoi(stmp2);
            if (  Spec->EmbedDim[Spec->N_Surfs-1] < 2 || 
                  Spec->EmbedDim[Spec->N_Surfs-1] > 3) {
               fprintf( SUMA_STDERR,
                        "Error %s: Bad Embedding dimension %d.\n"
                        "Only 2 and 3 allowed.\n", 
                  FuncName, Spec->EmbedDim[Spec->N_Surfs-1]);
               SUMA_RETURN (NOPE); 
            }
            if (!OKread_EmbedDim) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else  {
               OKread_EmbedDim = NOPE;
            }
            skp = 1;
         }
            
         sprintf(stmp,"SurfaceState");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found surface state, parse it */
            if (!SUMA_ParseLHS_RHS (
                     s, stmp, 
                     Spec->State[Spec->N_Surfs-1])) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            if (LHunify) {
               SUMA_LH_UNIFY_STATE_FIX(Spec->State[Spec->N_Surfs-1]);
            }

            /* make sure it is in the StateList */
            if (SUMA_iswordin (Spec->StateList, Spec->State[Spec->N_Surfs-1]) 
                != 1) {
               fprintf( SUMA_STDERR,
                        "Error %s: State %s was not predefined in StateDef.\n"
                        "StateDef List (| delimited) = %s \n",\
                FuncName, Spec->State[Spec->N_Surfs-1], Spec->StateList);
               SUMA_RETURN (NOPE);
            }
            if (!OKread_State) {
               fprintf( SUMA_STDERR,
                        "Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else  {
               OKread_State = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"SurfaceFormat");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            /*fprintf(SUMA_STDERR,"Found %s: ", stmp);*/
            
            if (!SUMA_ParseLHS_RHS (
                     s, stmp, 
                     Spec->SurfaceFormat[Spec->N_Surfs-1])) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            
            if (!OKread_SurfaceFormat) {
               fprintf( SUMA_STDERR,
                        "Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_SurfaceFormat = NOPE;
            }
            skp = 1;
            /*fprintf(  SUMA_STDERR,"%s\n",
                        Spec->SurfaceFormat[Spec->N_Surfs-1]);*/
         }
         
         sprintf(stmp,"SurfaceType");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            /*fprintf(SUMA_STDERR,"Found %s\n", stmp);*/
            if (!SUMA_ParseLHS_RHS (
                     s, stmp, 
                     Spec->SurfaceType[Spec->N_Surfs-1])) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            if (!OKread_SurfaceType) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_SurfaceType = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"TopoFile");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /*fprintf(SUMA_STDERR,"Found %s\n", stmp);*/
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            snprintf(Spec->TopoFile[Spec->N_Surfs-1], 
                        SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
               "%s%s", Spec->SpecFilePath, stmp2);
            
            if (!OKread_TopoFile) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_TopoFile = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"SureFitTopo");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /*fprintf(SUMA_STDERR,"Found %s\n", stmp);*/
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            snprintf(Spec->TopoFile[Spec->N_Surfs-1], 
                        SUMA_MAX_FP_NAME_LENGTH * sizeof(char),"%s%s", 
               Spec->SpecFilePath, stmp2);
            
            if (!OKread_TopoFile) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_TopoFile = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"CoordFile");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /*fprintf(SUMA_STDERR,"Found %s\n", stmp);*/
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            snprintf (Spec->CoordFile[Spec->N_Surfs-1], 
                        SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
               "%s%s", Spec->SpecFilePath, stmp2);
            
            if (!OKread_CoordFile) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_CoordFile = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"SureFitCoord");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /*fprintf(SUMA_STDERR,"Found %s\n", stmp);*/
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            snprintf (Spec->CoordFile[Spec->N_Surfs-1], 
                        SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
               "%s%s", Spec->SpecFilePath, stmp2);
            
            if (!OKread_CoordFile) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_CoordFile = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"MappingRef");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            if (LocalHead)  fprintf(SUMA_STDERR,"Found %s\n", stmp);
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            snprintf (Spec->MappingRef[Spec->N_Surfs-1], 
                        SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
               "%s%s", Spec->SpecFilePath, stmp2);
            if (!OKread_MappingRef) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_MappingRef = NOPE;
            }
            skp = 1;
         }
         /* Should become obsolete, ZSS Jan 02 03 */
         
         sprintf(stmp,"SureFitVolParam");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            if (LocalHead) fprintf(SUMA_STDERR,"Found %s\n", stmp);
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            snprintf (Spec->SureFitVolParam[Spec->N_Surfs-1], 
                        SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
               "%s%s", Spec->SpecFilePath, stmp2);
            
            if (!OKread_SureFitVolParam) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_SureFitVolParam = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"FreeSurferSurface");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            if (LocalHead) fprintf(SUMA_STDERR,"Found %s\n", stmp);
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf(SUMA_STDERR,
                  "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            snprintf (Spec->SurfaceFile[Spec->N_Surfs-1], 
                      SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
               "%s%s", Spec->SpecFilePath, stmp2);
            if (!OKread_FreeSurferSurface) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_FreeSurferSurface = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"SurfaceName");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            if (LocalHead) fprintf(SUMA_STDERR,"Found %s\n", stmp);
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            snprintf (Spec->SurfaceFile[Spec->N_Surfs-1], 
                        SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
               "%s%s", Spec->SpecFilePath, stmp2);
            if (!OKread_FreeSurferSurface) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_FreeSurferSurface = NOPE;
            }
            if (onGIFTIalert) {
               if (SUMA_isExtension(Spec->SurfaceFile[Spec->N_Surfs-1],".gii")) {
                  /* have gifti file with N27 spec, check gifti date */
                  if (THD_filetime_diff(Spec->SurfaceFile[Spec->N_Surfs-1],
                                        2013, 8, 1) < 0) {
                     SUMA_S_Warnv(
"*******************************************************\n"
"File %s is probably from SUMA's \n"
"N27 GIFTI surfaces that are in RAI. With this new version\n"
"of SUMA all GIFTI surfaces must be in LPI.\n"
"\n"
"You need to either correct these old surfaces or get\n"
"a new copy of the template surface archives: \n"
"   https://afni.nimh.nih.gov/pub/dist/tgz/suma_MNI_N27.tgz\n"
"or\n"
"   https://afni.nimh.nih.gov/pub/dist/tgz/suma_TT_N27.tgz\n"
"or\n"
"   https://afni.nimh.nih.gov/pub/dist/tgz/suma_MNI152_2009.tgz\n"
"\n"
"If you choose to correct what you have, instead of a new\n"
"download, see the help for option -nocor in @SUMA_Make_Spec_FS \n"
"for suggestions.\n"
"\n"
"Results obtained with older versions of SUMA on old GIFTI\n"
"surfaces are valid. You just can't mix old files with \n"
"versions of SUMA postdating Aug. 1st 2013\n"
"As usual, if you have concerns, open surfaces in SUMA \n"
"and talk to AFNI. If contours line up, you're OK.\n"
"*******************************************************\n"
"*******************************************************\n"
"\n",
                      Spec->SurfaceFile[Spec->N_Surfs-1]);
                                 
                  }
               }
            }
            skp = 1;
         }
         
         sprintf(stmp,"InventorSurface");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            if (LocalHead) fprintf(SUMA_STDERR,"Found %s\n", stmp);
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf(SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            snprintf(Spec->SurfaceFile[Spec->N_Surfs-1], 
                     SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
               "%s%s", Spec->SpecFilePath, stmp2);
            
            if (!OKread_InventorSurface) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_InventorSurface = NOPE;
            }
            skp = 1;
         }

         sprintf(stmp,"SurfaceVolume");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            if (LocalHead) fprintf(SUMA_STDERR,"Found %s\n", stmp);
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf(SUMA_STDERR,
                  "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            
            fprintf(SUMA_STDOUT,
               "Note %s: Found SurfaceVolume in Spec File, "
               "Name must include path to volume.\n", FuncName);
            
            sprintf(Spec->VolParName[Spec->N_Surfs-1], "%s",  stmp2);
            
            if (!OKread_SurfaceVolume) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_SurfaceVolume = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"SurfaceLabel");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            if (LocalHead) fprintf(SUMA_STDERR,"Found %s\n", stmp);
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
                        
            sprintf(Spec->SurfaceLabel[Spec->N_Surfs-1], "%s",  stmp2);
            
            if (!OKread_SurfaceLabel) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_SurfaceLabel = NOPE;
            }
            skp = 1;
         }
         
         if (!skp) {
            fprintf(SUMA_STDERR,
               "Error %s: Your spec file contains uncommented gibberish:\n"
               "  %s\n"
               "Please deal with it.\n", 
            FuncName, s);
            SUMA_RETURN (NOPE);
         }
      } else {/* not not a comment */
         /*fprintf(SUMA_STDERR,"A comment: %s\n", s);*/
      }

      /* read the next line */
      do {
         ex = fscanf (sf_file,"%c",&c);
      } while (ex!=EOF && isspace(c));
      i=0;
      while (ex != EOF && c != '\n') {   
         s[i] = c; ++i;
         ex = fscanf (sf_file,"%c",&c);
      }
      s[i] = '\0';
      if (LocalHead) fprintf(SUMA_STDERR,"Read %s\n", s); 
   }
   fclose (sf_file);
   /* make sure last entry was good */
   if (Spec->SurfaceType[Spec->N_Surfs-1][0] == '\0') {
      fprintf( SUMA_STDERR,
               "Error %s: Failed to specify surface type for surface %d\n", 
               FuncName, Spec->N_Surfs-1);
      SUMA_RETURN (NOPE);
   }

   if (!SUMA_CheckOnSpecFile (Spec)) {
      SUMA_SL_Err("Badness in the spec file.\n");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_RETURN (YUP); 
}/* SUMA_Read_SpecFile */


/*!
   \brief merge left and right side specfiles in a manner that suits AFNI
*/
#define SUMA_COPY_SPEC_FIELD(trgspec, cnt, srcspec, isurf, fld) {  \
            if(srcspec->fld[isurf]) {\
               snprintf(trgspec->fld[cnt],   \
                     SUMA_MAX_FP_NAME_LENGTH * sizeof(char),\
                     "%s", srcspec->fld[isurf]); \
            } else { /* Should not happen */\
               fprintf(SUMA_STDERR, "** Error %s\n"  \
                                    "This should not be\n", FuncName);   \
            }  \
         }
SUMA_Boolean SUMA_Merge_SpecFiles( SUMA_SurfSpecFile *lhs,
                                   SUMA_SurfSpecFile *rhs,
                                   SUMA_SurfSpecFile *bhs,
                                   char *FileName)
{
   static char FuncName[]={"SUMA_Merge_SpecFiles"};
   SUMA_SurfSpecFile *vs[20];
   char *ss[20], *ns=NULL;
   int i, c, k;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (!lhs || !rhs || !bhs ||
       (!lhs->N_Surfs && !rhs->N_Surfs) ) SUMA_RETURN(NOPE);
   
   SUMA_LH("Init");
   if (!SUMA_AllocSpecFields(bhs)) { 
      SUMA_S_Err("Failed to init spec fields."); 
      SUMA_RETURN(NOPE); 
   }
   
   vs[0] = lhs;
   ss[0] = "_lh";
   vs[1] = rhs;
   ss[1] = "_rh";
   
   SUMA_LH("Check");
   /* checks */
   for (k=1; k<2; ++k) {
      if (strcmp(lhs->Group[0], rhs->Group[0])) {
         SUMA_S_Err("Not ready to merge different groups");
         SUMA_RETURN(NOPE);
      }
   }
   
   
   /* loop accross states and relabel non-anatomically correct states */
   c = 0;
   for (k=0; k<2; ++k) {
      for (i=0; i<vs[k]->N_Surfs; ++i) {
         SUMA_LHv("Copy vs[%d,%d]-->%d\n",k,i,c);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, SurfaceType);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, SurfaceFormat);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, TopoFile);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, CoordFile);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, MappingRef);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, SureFitVolParam);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, SurfaceFile);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, VolParName);
         bhs->IDcode[c] = vs[k]->IDcode[i];/* IDcode is a pointer copy */
         if (vs[k]->AnatCorrect[i][0] == 'N') {
            ns = SUMA_append_string(vs[k]->State[i],ss[k]);
            snprintf(bhs->State[c], 
                     SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
                     "%s", ns); SUMA_free(ns); ns = NULL; 
         } else {
            SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, State);
         } 
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, LabelDset);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, NodeMarker);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, Group);
         if (strcmp(bhs->Group[0], vs[k]->Group[i])) {
            SUMA_S_Warn("Unexpected Group mismatch!\n"
                        "Assumption was that all surfs\n"
                        "in spec file are of the same group\n"
                        "Proceeding, beware.\n");
         }
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, SurfaceLabel);
         bhs->EmbedDim[c] = vs[k]->EmbedDim[i];
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, AnatCorrect);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, Hemisphere);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, DomainGrandParentID);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, OriginatorID);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, LocalCurvatureParent);
         SUMA_COPY_SPEC_FIELD(bhs, c, vs[k], i, LocalDomainParent);
         ++c;
      }
   }
   
   bhs->N_Surfs = c;   
   bhs->N_Groups = 1;
   
   SUMA_LH("States");
   /* do the states */
   sprintf(bhs->StateList, "%s|", bhs->State[0]);
   bhs->N_States = 1;
   for (c=1; c<bhs->N_Surfs; ++c) {
      SUMA_LHv("Is %s in %s\n", bhs->State[c], bhs->StateList);
      if (!strstr(bhs->StateList, bhs->State[c])) {/* new one */
         SUMA_LH("            No");
         bhs->StateList =  SUMA_append_replace_string(bhs->StateList, "|", 
                                                      bhs->State[c], 1);
         bhs->N_States += 1;
      }
   }

   snprintf(bhs->SpecFilePath,
            SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
            "%s", SUMA_FnameGet(FileName,"Pa", SUMAg_CF->cwd));
   snprintf(bhs->SpecFileName,
            SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
            "%s", SUMA_FnameGet(FileName,"f", SUMAg_CF->cwd));
   
   
   SUMA_RETURN(YUP);
}       

/*!
   Remove surfaces of a certain state from the spec struct
   If ldp is not NULL, remove if both domain parent and the
   state match. ldp match is always partial.
   
   Returns the number of surfaces remaining in the spec struct
*/
int SUMA_RemoveSpecState(SUMA_SurfSpecFile  *Spec, 
                         char *state_rm, int exact_match,
                         char *ldp)
{
   static char FuncName[]={"SUMA_RemoveSpecState"};
   int i, k, copyit;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!Spec || !state_rm) SUMA_RETURN(0);

   k = 0;
   for (i=0; i<Spec->N_Surfs; ++i) {
      copyit = 1;
      if ( (exact_match == 0 &&  strstr(Spec->State[i],state_rm)) ||
           (exact_match == 1 && !strcmp(Spec->State[i],state_rm))) {
         /* found state to remove */
         copyit = 0;
         SUMA_LHv("Will remove %s\n", Spec->State[i]);
      }
      /* don't use exact match for Spec->LocalDomainParent[i], 
         unless you account for the paths. "SAME" ends up being
         "./SAME", for example. */
      if (!copyit && ldp && !strstr(Spec->LocalDomainParent[i], ldp)) { 
         /* no match for ldp, cancel */
         copyit = 1;
         SUMA_LHv("Canceling removal ldp is %s\n", Spec->LocalDomainParent[i]);
      }
      if ( copyit ) {
         SUMA_LHv("Working to copy state %s for surface i=%d k=%d\n",
                        Spec->State[i], i, k);
         if (k < i) {
         sprintf(Spec->State[k], "%s",Spec->State[i]);
         sprintf(Spec->SurfaceType[k], "%s",Spec->SurfaceType[i]);
         sprintf(Spec->SurfaceFormat[k], "%s",Spec->SurfaceFormat[i]);
         sprintf(Spec->TopoFile[k], "%s",Spec->TopoFile[i]);
         sprintf(Spec->CoordFile[k], "%s",Spec->CoordFile[i]);
         sprintf(Spec->MappingRef[k], "%s",Spec->MappingRef[i]);
         sprintf(Spec->SureFitVolParam[k], "%s",Spec->SureFitVolParam[i]);
         sprintf(Spec->SurfaceFile[k], "%s",Spec->SurfaceFile[i]);
         sprintf(Spec->VolParName[k], "%s",Spec->VolParName[i]);
         if (Spec->IDcode[i]) sprintf(Spec->IDcode[k], "%s",Spec->IDcode[i]);
         else Spec->IDcode[k]=NULL;
         sprintf(Spec->State[k], "%s",Spec->State[i]);
         sprintf(Spec->LabelDset[k], "%s",Spec->LabelDset[i]);
         sprintf(Spec->Group[k], "%s",Spec->Group[i]);
         sprintf(Spec->SurfaceLabel[k], "%s",Spec->SurfaceLabel[i]);
         Spec->EmbedDim[k] = Spec->EmbedDim[i];
         sprintf(Spec->AnatCorrect[k], "%s",Spec->AnatCorrect[i]);
         sprintf(Spec->Hemisphere[k], "%s",Spec->Hemisphere[i]);
         sprintf(Spec->DomainGrandParentID[k], 
                                 "%s",Spec->DomainGrandParentID[i]);
         sprintf(Spec->OriginatorID[k], "%s",Spec->OriginatorID[i]);
         sprintf(Spec->LocalCurvatureParent[k], 
                                 "%s",Spec->LocalCurvatureParent[i]);
         sprintf(Spec->LocalDomainParent[k], "%s",Spec->LocalDomainParent[i]);
         sprintf(Spec->NodeMarker[k], "%s",Spec->NodeMarker[i]);
         }
         ++k;
      }
   }
   if (k != Spec->N_Surfs) Spec->N_States = Spec->N_States-1;
   Spec->N_Surfs = k;
   
   SUMA_RETURN(k);
}
           
/*!
   \brief Write SUMA_SurfSpecFile  structure to disk
   \param Spec: Structure containing specfile
   \param specFileNm: Name of specfile. 
                     If NULL, use
                      name in Spec->SpecFileName
   \param program: name of program to put in comments if not NULL
   \param histnote: a history note to add as comment if not NULL
*/ 
SUMA_Boolean SUMA_Write_SpecFile ( SUMA_SurfSpecFile * Spec, 
                           char *specFileNmU, 
                           char *program, 
                           char *histnote) {

   static char FuncName[]={"SUMA_Write_SpecFile"};
   FILE *outFile=NULL;
   int i=0, k=0, tag=0, ifSmwm=0, p=0;
   char writename[SUMA_MAX_FILENAME_LENGTH+3]={""};
   char *specFileNm=NULL;
   
   SUMA_ENTRY;
   
   if (!Spec) SUMA_RETURN(NOPE);
   
   
   if (specFileNmU && specFileNmU[0]) {
      specFileNm = SUMA_Extension(specFileNmU, ".spec", NOPE);
      sprintf(writename,"%s",specFileNm);
   } else {
      if (!Spec->SpecFileName) SUMA_RETURN(NOPE);
      specFileNm = SUMA_Extension(Spec->SpecFileName, ".spec", NOPE);
      if (Spec->SpecFilePath) {
         sprintf( writename, 
                  "%s/%s", 
                  Spec->SpecFilePath, 
                  specFileNm);
      } else {
         sprintf( writename, 
                  "%s", 
                  specFileNm);
      }
   }
   if (SUMA_filexists(writename) && !THD_ok_overwrite()) {
      SUMA_S_Errv("Spec file %s exists, will not overwrite\n", writename);
      SUMA_RETURN(NOPE);
   }
   if (specFileNm) SUMA_free(specFileNm); specFileNm = NULL;
   
   outFile = fopen(writename, "w");
      
   if (!outFile) {
      fprintf (SUMA_STDERR, "Failed in opening %s for writing.\n", writename); 
      exit (1);
   }
   else {
      if (program && program[0]) 
         fprintf (outFile, "# %s generated spec file\n", program);
      if (histnote) fprintf (outFile, "#History: %s\n\n", histnote);
      else fprintf (outFile, "\n");
      fprintf (outFile, "#define the group\n\tGroup = %s\n\n", Spec->Group[0]);
      fprintf (outFile, "#define various States\n");
      for (i=0; i<Spec->N_Surfs; ++i) {
         tag = 0;
         for (k=0; k<i; ++k) {
            if ( strcmp( Spec->State[k], Spec->State[i] ) == 0 ) tag = -1;
         }
         if (tag==0) {
            fprintf( outFile, "\tStateDef = %s\n", Spec->State[i]);
         }
      }

      for (i=0; i<Spec->N_Surfs; ++i) {
         fprintf (outFile, 
                  "\nNewSurface\n"
                  "\tSurfaceFormat = %s\n"
                  "\tSurfaceType = %s\n", 
                  Spec->SurfaceFormat[i], 
                  Spec->SurfaceType[i]);
         if (Spec->SurfaceFile[i][0]) {
            fprintf (outFile, 
                     "\tSurfaceName = %s\n",
                     Spec->SurfaceFile[i] );
         } else {
            fprintf (outFile, 
                     "\tCoordFile = %s\n",
                     Spec->CoordFile[i] );
            fprintf (outFile, 
                     "\tTopoFile = %s\n",
                     Spec->TopoFile[i] );
         }
         if (Spec->SureFitVolParam[i][0]) {
            fprintf (outFile, 
                     "\tSureFitVolParam = %s\n",
                     Spec->SureFitVolParam[i] );
         }
         if (Spec->LocalDomainParent[i][0]) {
            fprintf (outFile, 
                     "\tLocalDomainParent = %s\n", 
                   Spec->LocalDomainParent[i] );
         } else {
            fprintf (outFile, 
                     "\tLocalDomainParent = SAME\n" );
         }
         if (Spec->LabelDset[i][0]) {
            fprintf (outFile, 
                     "\tLabelDset = %s\n", 
                   Spec->LabelDset[i] );
         } else {
         }
         if (Spec->NodeMarker[i][0]) {
            fprintf (outFile, 
                     "\tNodeMarker = %s\n", 
                   Spec->NodeMarker[i] );
         } else {
         }
         fprintf (outFile, "\tSurfaceState = %s\n"
                           "\tEmbedDimension = %d\n", 
                           Spec->State[i], Spec->EmbedDim[i]);
         
         if (Spec->VolParName[i][0]) {
            fprintf (outFile, 
                     "\tSurfaceVolume = %s\n", 
                     Spec->VolParName[i] );
         }
         /* For now, only one group is allowed */
         if (  Spec->Group[i][0] && Spec->Group[0][0] && 
               strcmp(Spec->Group[i], Spec->Group[0])) {
            SUMA_S_Errv("SUMA does not read specfile with\n"
                        "surfaces from multiple groups yet\n"
                        "No point in trying to write that one.\n"
                        "Group[%d]=%s and Group[0]=%s\n"
                        , i, Spec->Group[i], Spec->Group[0]);
            SUMA_RETURN(NOPE);
            /* otherwise you just do: 
               You can add this if all the groups are the same.
               SUMA will recognize that it is the same group and
               won't complain, but it might confuse the users*/
            if (Spec->Group[i][0]) {
               fprintf (outFile, 
                        "\tGroup = %s\n", 
                        Spec->Group[i] );
            }            
         }
         
         if (Spec->SurfaceLabel[i][0]) {
            fprintf (outFile, 
                     "\tSurfaceLabel = %s\n", 
                     Spec->SurfaceLabel[i] );
         }
         if (Spec->AnatCorrect[i][0]) {
            fprintf (outFile, 
                     "\tAnatomical = %s\n", 
                     Spec->AnatCorrect[i] );
         }
         if (Spec->Hemisphere[i][0]) {
            fprintf (outFile, 
                     "\tHemisphere = %s\n", 
                     Spec->Hemisphere[i] );
         }
         if (Spec->DomainGrandParentID[i][0]) {
            fprintf (outFile, 
                     "\tDomainGrandParentID = %s\n", 
                     Spec->DomainGrandParentID[i] );
         }
         if (Spec->OriginatorID[i][0]) {
            fprintf (outFile, 
                     "\tOriginatorID = %s\n", 
                     Spec->OriginatorID[i] );
         }
         if (Spec->LocalCurvatureParent[i][0]) {
            fprintf (outFile, 
                     "\tLocalCurvatureParent = %s\n", 
                     Spec->LocalCurvatureParent[i] );
         }
/*
         if (Spec->xxx[i][0]) {
            fprintf (outFile, 
                     "\tyyy = %s\n", 
                     Spec->xxx[i] );
         }
*/
      }

      fclose(outFile);
   }
   SUMA_RETURN(YUP);
} /* SUMA_Write_SpecFile */

/*!
   \brief more checksums on the contents of the specfile
*/
SUMA_Boolean SUMA_CheckOnSpecFile (SUMA_SurfSpecFile *Spec)
{
   static char FuncName[]={"SUMA_CheckOnSpecFile"};
   static int ob_warn = 0;
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (Spec->N_Surfs == -1) {
      SUMA_S_Err("Struct fresh out of SUMA_AllocSpecFields");
      SUMA_RETURN(NOPE); 
   }
   
   for (i=0; i<Spec->N_Surfs; ++i) {
      if (  Spec->MappingRef[i][0] && 
           (Spec->LocalDomainParent[i][0] || 
            Spec->LocalCurvatureParent[i][0] || 
            Spec->OriginatorID[i][0] || 
            Spec->DomainGrandParentID[i][0]) ) {
         SUMA_SL_Err("You cannont mix MappingRef with\n"
                     "newer fields such as:\n"
                     "LocalDomainParent, LocalCurvatureParent\n"
                     "OriginatorID or DomainGrandParentID  ");
         SUMA_RETURN(NOPE);            
      }
      if (  Spec->MappingRef[i][0] ) {
         
         if (LocalHead && !ob_warn) { 
            fprintf(SUMA_STDERR, "Warning:\n"
                                 "The field MappingRef in the spec file \n"
                                 "is obsolete. Consider replacing: \n"
                                 "  MappingRef = %s\n"
                                 "  with\n"
                                 "  LocalDomainParent = %s\n"
                                 "Similar warnings will be muted.\n",
                                 Spec->MappingRef[i], Spec->MappingRef[i]);
         }
         strcpy(Spec->LocalDomainParent[i], Spec->MappingRef[i]);
         strcpy(Spec->LocalCurvatureParent[i], Spec->MappingRef[i]);
         Spec->MappingRef[i][0] = '\0';
         ++ob_warn;
      }
      if ( strlen(Spec->LocalCurvatureParent[i]) ) {
         if ( ! strstr( Spec->LocalCurvatureParent[i], 
                         Spec->LocalDomainParent[i]) ) {
            SUMA_SL_Err("Fields LocalCurvatureParent and LocalDomainParent \n"
                        "must be identical.\n");
            SUMA_RETURN(NOPE);
         }
      } else {
         sprintf( Spec->LocalCurvatureParent[i], "%s", 
                  Spec->LocalDomainParent[i]);
      }
      
      if (strlen(Spec->LocalDomainParent[i]) &&
         SUMA_iswordsame(Spec->SurfaceFile[i],Spec->LocalDomainParent[i]) == 1){
         SUMA_LH("Make LDP be SAME");
         snprintf (Spec->LocalDomainParent[i], 
                   SUMA_MAX_FP_NAME_LENGTH * sizeof(char),
                   "%s%s", Spec->SpecFilePath, "SAME");
      }
   }
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_ShowSpecStruct (SUMA_SurfSpecFile *Spec, FILE *Out, int detail)
{
   static char FuncName[]={"SUMA_ShowSpecStruct"};
   FILE *Outp;
   char *s;
   
   SUMA_ENTRY;
   
   if (!Spec) {
      SUMA_SL_Err("NULL Spec");
      SUMA_RETURN(NOPE);
   }
   
   if (Spec->N_Surfs == -1) {
      SUMA_S_Err("Struct fresh out of SUMA_AllocSpecFields");
      SUMA_RETURN(NOPE); 
   }
   
   if (!Out) Outp = stdout;
   else Outp = Out;
   
   s = SUMA_SpecStructInfo (Spec, detail);
   
   if (!s) {
      SUMA_SL_Err("Failed in   SUMA_SpecStructInfo");
      SUMA_RETURN(NOPE);
   }
   
   fprintf(Outp, "%s", s);
   
   SUMA_free(s); s = NULL;
   
   SUMA_RETURN(YUP);
}
/*!
   \brief show the contents of Spec structure 
   ans = SUMA_ShowSpecStruct (Spec, Out, detail);
   
   \param Spec (SUMA_SurfSpecFile *)
   \param Out (FILE *)  Pointer to output file
                        If NULL then output is to stdout
   \param detail (int) 1:  only surface name or coord file 
                           name if surface file is split to coord. 
                           and topo. files
                       2:  surface name and BOTH coord and topo files 
                           whenever applicable
                       3:  The whole nine yards.
   \return ans (YUP = good, NOPE = bad)
   \sa SUMA_Read_SpecFile
*/
char* SUMA_SpecStructInfo (SUMA_SurfSpecFile *Spec, int detail)
{
   static char FuncName[]={"SUMA_ShowSpecStructInfo"};
   char name_coord[SUMA_MAX_LABEL_LENGTH];
   char name_topo[SUMA_MAX_LABEL_LENGTH], *s = NULL;
   SUMA_STRING *SS = NULL;
   char stmp[1000];
   int i;
   SUMA_Boolean ShowCoord, ShowTopo, ShowRest;
   
   SUMA_ENTRY;
   
   ShowCoord = ShowTopo = ShowRest = NOPE;
   
   if (detail == 1) ShowCoord = YUP;
   else if (detail == 2) { ShowCoord = YUP; ShowTopo = YUP; }
   else if (detail == 3) { ShowCoord = YUP; ShowTopo = YUP; ShowRest = YUP; }
   else {
      SUMA_SL_Err("Bad value for detail, 0 < detail < 4");
      SUMA_RETURN(NULL);
   }
   
   SS = SUMA_StringAppend (NULL, NULL);
   if (Spec->N_Surfs == -1) 
      SS = SUMA_StringAppend_va (SS,"Spec fresh out of SUMA_AllocSpecFields");
   
   if (Spec->SpecFilePath) 
      SS = SUMA_StringAppend_va (SS,"SpecFilePath: %s\n", Spec->SpecFilePath);
   else SS = SUMA_StringAppend_va (SS,"SpecFilePath: NULL\n");
   if (Spec->SpecFileName) 
      SS = SUMA_StringAppend_va (SS,"SpecFileName: %s\n", Spec->SpecFileName);
   else SS = SUMA_StringAppend_va (SS,"SpecFileName: NULL\n");

   if (!Spec->N_Surfs) {
      SS = SUMA_StringAppend (SS,"No surfaces in Spec.\n");
   } else {
      
      sprintf (stmp, "%d surfaces in Spec, %d defined states, %d groups\n", 
                        Spec->N_Surfs, Spec->N_States, Spec->N_Groups);
      SS = SUMA_StringAppend (SS, stmp);
      
      for (i=0; i < Spec->N_Surfs; ++i) {
         name_coord[0] ='\0';
         name_topo[0] = '\0';
         if (  (SUMA_iswordin(Spec->SurfaceType[i], "SureFit") == 1) || 
               (SUMA_iswordin(Spec->SurfaceType[i], "1D") == 1)         ) {
            sprintf(name_coord, "%s ", Spec->CoordFile[i]);
            sprintf(name_topo,"%s ", Spec->TopoFile[i]);
         } else if ( (SUMA_iswordin(Spec->SurfaceType[i], "FreeSurfer") == 1) ||
                     (SUMA_iswordin(Spec->SurfaceType[i], "Ply") == 1)        ||
                     (SUMA_iswordin(Spec->SurfaceType[i], "GenericInventor") == 
                                                                           1) ||
                     (SUMA_iswordin(Spec->SurfaceType[i], "OpenDX") == 1) ) {
            sprintf(name_coord, "%s ", Spec->SurfaceFile[i]);
         }
         SS = SUMA_StringAppend_va (SS, "%d) ", i);/* print the index */
         
         if (ShowCoord)  SS = SUMA_StringAppend (SS, name_coord);
         if (ShowTopo &&name_topo[0]) SS = SUMA_StringAppend (SS,  name_topo);
         SS = SUMA_StringAppend (SS, "\n");
         
         if (ShowRest) {
            SS = SUMA_StringAppend_va (SS, "\tMappingRef: %s\n", 
                                       Spec->MappingRef[i]);   
                                          /* Should become obsolete, 
                                             ZSS Jan 02 03 */
            SS = SUMA_StringAppend_va (SS, 
                                       "\tType: %s\n", 
                                       Spec->SurfaceType[i]);
            SS = SUMA_StringAppend_va (SS, 
                                       "\tFormat: %s\n", 
                                       Spec->SurfaceFormat[i]);
            SS = SUMA_StringAppend_va (SS, 
                                       "\tEmbedDim: %d\n", 
                                       Spec->EmbedDim[i]);
            SS = SUMA_StringAppend_va (SS, 
                                       "\tState: %s, Group %s\n", 
                                       Spec->State[i], Spec->Group[i]);
            
            if (strlen(Spec->SureFitVolParam[i])) {
               SS = SUMA_StringAppend_va (SS, 
                                       "\tSureFitVolParam: %s\n", 
                                       Spec->SureFitVolParam[i]);
            } else  SS = SUMA_StringAppend_va (SS, 
                                       "\tSureFitVolParam: (empty)\n");
            
            if (strlen(Spec->VolParName[i]))  {
               SS = SUMA_StringAppend_va (SS, 
                                       "\tVolParName: %s\n", 
                                       Spec->VolParName[i]);
            } else SS = SUMA_StringAppend_va (SS, 
                                       "\tVolParName: (empty)\n");
            
            if (Spec->IDcode[i])  {
               SS = SUMA_StringAppend_va (SS, 
                                       "\tIDcode: %s\n", 
                                       Spec->IDcode[i]);
            } else SS = SUMA_StringAppend_va (SS, 
                                       "\tIDcode: (empty)\n");
            
            if (strlen(Spec->AnatCorrect[i])) {
               SS = SUMA_StringAppend_va (SS, 
                                       "\tAnatCorrect: %s\n", 
                                       Spec->AnatCorrect[i]);
            } else SS = SUMA_StringAppend_va (SS, 
                                       "\tAnatCorrect: (empty)\n");
            
            if (strlen(Spec->Hemisphere[i])) {
               SS = SUMA_StringAppend_va (SS, 
                                       "\tHemisphere: %s\n", 
                                       Spec->Hemisphere[i]);
            } else SS = SUMA_StringAppend_va (SS, 
                                       "\tHemisphere: (empty)\n");
            
            if (strlen(Spec->DomainGrandParentID[i])) {
               SS = SUMA_StringAppend_va (SS, 
                                       "\tDomainGrandParentID: %s\n", 
                                       Spec->DomainGrandParentID[i]);
            } else SS = SUMA_StringAppend_va (SS, 
                                       "\tDomainGrandParentID: (empty)\n");
            
            if (strlen(Spec->OriginatorID[i])) {
               SS = SUMA_StringAppend_va (SS, 
                                       "\tOriginatorID: %s\n", 
                                       Spec->OriginatorID[i]);
            } else SS = SUMA_StringAppend_va (SS, 
                                       "\tOriginatorID: (empty)\n");
            
            if (strlen(Spec->LocalCurvatureParent[i])) {
               SS = SUMA_StringAppend_va (SS, 
                                       "\tLocalCurvatureParent: %s\n", 
                                       Spec->LocalCurvatureParent[i]);
            } else SS = SUMA_StringAppend_va (SS, 
                                       "\tLocalCurvatureParent: (empty)\n");
            
            if (strlen(Spec->LocalDomainParent[i])) {
               SS = SUMA_StringAppend_va (SS, 
                                       "\tLocalDomainParent: %s\n", 
                                       Spec->LocalDomainParent[i]);
            } else SS = SUMA_StringAppend_va (SS, 
                                       "\tLocalDomainParent: (empty)\n");
            
            if (strlen(Spec->LabelDset[i])) {
               SS = SUMA_StringAppend_va (SS, 
                                       "\tLabelDset: %s\n", 
                                       Spec->LabelDset[i]);
            } else SS = SUMA_StringAppend_va (SS, 
                                       "\tLabelDset: (empty)\n");
            
            if (strlen(Spec->NodeMarker[i])) {
               SS = SUMA_StringAppend_va (SS, 
                                       "\tNodeMarker: %s\n", 
                                       Spec->NodeMarker[i]);
            } else SS = SUMA_StringAppend_va (SS, 
                                       "\tNodeMarker: (empty)\n");
            /*
            if (strlen(Spec->[i])) {
               SS = SUMA_StringAppend_va (SS, 
                                       "\t: %s\n", 
                                       Spec->[i]);
            } else SS = SUMA_StringAppend_va (SS, "\t: (empty)\n");
            */
         }   
      }
   }
      
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN (s);
}

/*!
   \brief loads a surface object specified in Spec[i]
   
   - NOTABLE SO fields filled in this function are:
   SO->NodeList, SO->N_NodeList, SO->FaceSetList, SO->N_FaceSet
   SO->Group, SO->idcode_str,
   SO->State
   SO->EmbedDim
   SO->Side
   SO->OriginatorID
   SO->DomainGrandParentID
   SO->LocalCurvatureParent
   SO->LocalDomainParent
   SO->AnatCorrect
   SO->SpecFile
   \returns SO (SUMA_SurfaceObject *)
*/
SUMA_SurfaceObject * SUMA_Load_Spec_Surf(
                           SUMA_SurfSpecFile *Spec, 
                           int i, 
                           char *tmpVolParName, 
                           int debug)
{  /* start SUMA_Load_Spec_Surf */
   static char FuncName[]={"SUMA_Load_Spec_Surf"};
   SUMA_SFname *SF_name;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean brk, SurfIn=NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   brk = NOPE;

   if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "SureFit") == 1) {
      /* load surefit surface */
      SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
      sprintf(SF_name->name_coord,"%s", Spec->CoordFile[i]); 
      sprintf(SF_name->name_topo,"%s", Spec->TopoFile[i]); 
      if (!strlen(Spec->SureFitVolParam[i])) { /* initialize to empty string */
         SF_name->name_param[0] = '\0'; 
      } else {
         sprintf(SF_name->name_param,"%s", Spec->SureFitVolParam[i]);
      }

      /* Load The Surface */
      if (SUMA_iswordin(Spec->SurfaceFormat[i], "ASCII") == 1) {
         SO = SUMA_Load_Surface_Object_eng ( (void *)SF_name, SUMA_SUREFIT, 
                                             SUMA_ASCII, tmpVolParName, debug);
      } else {
         fprintf( SUMA_STDERR,
                  "Error %s: Only ASCII surfaces can be read for now.\n", 
                  FuncName);
         SUMA_RETURN (NULL);
      }
      if (SO == NULL)   {
         fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
         SUMA_RETURN(NULL);
      }

      SUMA_free(SF_name); 

      SurfIn = YUP;         
      brk = YUP;
   }/* load surefit surface */ 
                  
   if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "1D") == 1) {
      /* load 1D surface */
      SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
      sprintf(SF_name->name_coord,"%s", Spec->CoordFile[i]); ;
      sprintf(SF_name->name_topo,"%s", Spec->TopoFile[i]); 
      SF_name->name_param[0] = '\0';


      /* Load The Surface */
      if (SUMA_iswordin(Spec->SurfaceFormat[i], "ASCII") == 1) {
         SO = SUMA_Load_Surface_Object_eng ((void *)SF_name, SUMA_VEC, 
                                             SUMA_ASCII, tmpVolParName, debug);
      } else {
         fprintf( SUMA_STDERR,
                  "Error %s: Only ASCII allowed for 1D files.\n", FuncName);
         SUMA_RETURN (NULL);
      }
      if (SO == NULL)   {
         fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
         SUMA_RETURN(NULL);
      }

      SUMA_free(SF_name); 

      SurfIn = YUP;         
      brk = YUP;
   }/* load 1D surface */

   if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "FreeSurfer") == 1) {
      /* load FreeSurfer surface */
      if (SUMA_iswordin(Spec->SurfaceFormat[i], "ASCII") == 1)
         SO = SUMA_Load_Surface_Object_eng ( (void *)Spec->SurfaceFile[i],      
                                             SUMA_FREE_SURFER, SUMA_ASCII, 
                                             tmpVolParName, debug);
      else if (SUMA_iswordin(Spec->SurfaceFormat[i], "BINARY") == 1)
         SO = SUMA_Load_Surface_Object_eng ( (void *)Spec->SurfaceFile[i], 
                                             SUMA_FREE_SURFER, SUMA_BINARY_BE, 
                                             tmpVolParName, debug);
      if (SO == NULL)   {
         fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
         SUMA_RETURN(NULL);
      }
      SurfIn = YUP;
      brk = YUP;
   } /* load FreeSurfer surface */

   if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "Ply") == 1) {
      /* load Ply format surface */
      SO = SUMA_Load_Surface_Object_eng ( (void *)Spec->SurfaceFile[i], SUMA_PLY,                                           SUMA_FF_NOT_SPECIFIED, tmpVolParName, 
                                          debug);

      if (SO == NULL)   {
         fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
         SUMA_RETURN(NULL);
      }
      SurfIn = YUP;
      brk = YUP;
   } /* load Ply format surface */

   if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "STL") == 1) {
      /* load STL format surface */
      SO = SUMA_Load_Surface_Object_eng ( (void *)Spec->SurfaceFile[i], SUMA_STL,                                           SUMA_FF_NOT_SPECIFIED, tmpVolParName, 
                                          debug);

      if (SO == NULL)   {
         fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
         SUMA_RETURN(NULL);
      }
      SurfIn = YUP;
      brk = YUP;
   } /* load STL format surface */

   if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "MNI") == 1) {
      /* load MNI_OBJ format surface */

      SO = SUMA_Load_Surface_Object_eng ((void *)Spec->SurfaceFile[i], 
                                          SUMA_MNI_OBJ, SUMA_ASCII, 
                                          tmpVolParName, debug);

      if (SO == NULL)   {
         fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
         SUMA_RETURN(NULL);
      }
      SurfIn = YUP;
      brk = YUP;
   } /* load MNI_OBJ format surface */

   if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "OpenDX") == 1) {
      SO = SUMA_Load_Surface_Object_eng ((void *)Spec->SurfaceFile[i], 
                        SUMA_OPENDX_MESH, SUMA_ASCII, tmpVolParName, debug);

      if (SO == NULL)   {
         fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
         SUMA_RETURN(NULL);
      }
      SurfIn = YUP;
      brk = YUP;
   } /* load OpenDX format surface */
   
   if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "OBJ") == 1) {
      SO = SUMA_Load_Surface_Object_eng ((void *)Spec->SurfaceFile[i], 
                        SUMA_OBJ_MESH, SUMA_ASCII, tmpVolParName, debug);

      if (SO == NULL)   {
         fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
         SUMA_RETURN(NULL);
      }
      SurfIn = YUP;
      brk = YUP;
   } /* load OBJ_MESH format surface */
   
   
   if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "Predefined") == 1) {

      SO = SUMA_Load_Surface_Object_eng ((void *)Spec->SurfaceFile[i], 
                     SUMA_PREDEFINED, SUMA_ASCII, tmpVolParName, debug);
      if (SO == NULL)   {
         fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
         SUMA_RETURN(NULL);
      }
      SurfIn = YUP;
      brk = YUP;
   } 
   
   if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "BrainVoyager") == 1) {
      /* load BrainVoyager format surface */
      SO = SUMA_Load_Surface_Object_eng ( (void *)Spec->SurfaceFile[i], 
                                          SUMA_BRAIN_VOYAGER, 
                                          SUMA_FF_NOT_SPECIFIED, tmpVolParName, 
                                          debug);

      if (SO == NULL)   {
         fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
         SUMA_RETURN(NULL);
      }
      SurfIn = YUP;
      brk = YUP;
   } /* load bv format surface */
   
   if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "BYU") == 1) {
         /* load BYU format surface */
      SO = SUMA_Load_Surface_Object_eng ( (void *)Spec->SurfaceFile[i], SUMA_BYU,
                                          SUMA_FF_NOT_SPECIFIED, tmpVolParName, 
                                          debug);

      if (SO == NULL)   {
         fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
         SUMA_RETURN(NULL);
      }
      SurfIn = YUP;
      brk = YUP;
   } /* load byu format surface */
   
   if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "GIFTI") == 1) {
      /* load GIFTI format surface */
      SO = SUMA_Load_Surface_Object_eng (
               (void *)Spec->SurfaceFile[i], SUMA_GIFTI, 
               SUMA_FF_NOT_SPECIFIED, tmpVolParName, debug);

      if (SO == NULL)   {
         fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
         SUMA_RETURN(NULL);
      }
      SurfIn = YUP;
      brk = YUP;
   } /* load gifti format surface */
   
   if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "GenericInventor") == 1) {
                     /* load generic inventor format surface */
      if (tmpVolParName != NULL) {
         fprintf( SUMA_STDERR,
                  "Error %s: Sorry, but Parent volumes "
                  "are not supported for generic inventor surfaces.\n", 
                  FuncName);
         SUMA_RETURN (NULL);
      }
      if (SUMA_iswordin(Spec->SurfaceFormat[i], "ASCII") == 1)
         SO = SUMA_Load_Surface_Object_eng ( (void *)Spec->SurfaceFile[i], 
                                             SUMA_INVENTOR_GENERIC, SUMA_ASCII, 
                                             NULL, debug);
      else {
         fprintf( SUMA_STDERR,
                  "Error %s: Only ASCII surfaces can be read for now.\n", 
                  FuncName);
         SUMA_RETURN(NULL);
      }
      if (SO == NULL)   {
         fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
         SUMA_RETURN(NULL);
      }
      SurfIn = YUP;

      brk = YUP;
   }
   
   if (!brk) {
      fprintf( SUMA_STDERR,
               "Error %s: Unknown SurfaceFormat %s.\n"
               "(Format syntax is case sensitive)\n", 
               FuncName, Spec->SurfaceType[i]);
      SUMA_RETURN(NULL);
   }

   if (!SurfIn) {
      fprintf(SUMA_STDERR,"Error %s: Failed to read input surface.\n", FuncName);
      SUMA_RETURN(NULL);
   }

   /* assign its Group and State and Side*/
   SUMA_LHv("Assigning group %s , state %s, and EmbedDim %d\n",
            Spec->Group[i], Spec->State[i], Spec->EmbedDim[i]);
   SO->Group = (char *)SUMA_calloc(strlen(Spec->Group[i])+1, sizeof(char));
   SO->State = (char *)SUMA_calloc(strlen(Spec->State[i])+1, sizeof(char));
   if (Spec->SurfaceLabel[i][0] == '\0') {
      if (!(SO->Label = SUMA_SurfaceFileName (SO, NOPE))) {
         SO->Label = SUMA_copy_string("Who_Am_I");
      }
   } else {
      SO->Label = SUMA_copy_string(Spec->SurfaceLabel[i]);
   }
   
   if (SO->isSphere == SUMA_GEOM_NOT_SET) { 
      SUMA_SetSphereParams(SO, -0.1);   /* sets the spheriosity parameters */
   }
   
   if (!SO->Group || !SO->State || !SO->Label) {
      fprintf(SUMA_STDERR,"Error %s: Error allocating lameness.\n", FuncName);
      SUMA_RETURN (NULL);
   }
         
   SO->Group = strcpy(SO->Group, Spec->Group[i]);
   SO->State = strcpy(SO->State, Spec->State[i]);
   SO->EmbedDim = Spec->EmbedDim[i];
   if (Spec->Hemisphere[i][0] == 'L') {
      SO->Side = SUMA_LEFT;
   } else if (Spec->Hemisphere[i][0] == 'R') {
      SO->Side = SUMA_RIGHT;
   } else if (Spec->Hemisphere[i][0] == 'B') {
      SO->Side = SUMA_LR;
   } else SO->Side = SUMA_GuessSide (SO);

   
   if (Spec->OriginatorID[i][0]) 
      SO->OriginatorID = SUMA_copy_string(Spec->OriginatorID[i]);
   if (Spec->DomainGrandParentID[i][0]) 
      SO->DomainGrandParentID = SUMA_copy_string(Spec->DomainGrandParentID[i]);
   if (Spec->LocalCurvatureParent[i][0]) 
      SO->LocalCurvatureParent = SUMA_copy_string(Spec->LocalCurvatureParent[i]);
   if (Spec->LocalDomainParent[i][0]) 
      SO->LocalDomainParent = SUMA_copy_string(Spec->LocalDomainParent[i]);
   if (Spec->AnatCorrect[i][0] == '\0') 
      Spec->AnatCorrect[i][0] = SUMA_GuessAnatCorrect(SO);
   SO->AnatCorrect = NOPE;
   if (Spec->AnatCorrect[i][0] == 'Y')  SO->AnatCorrect = YUP;
   else SO->AnatCorrect = NOPE;
   
   if (Spec->SpecFilePath) 
      SO->SpecFile.Path = SUMA_copy_string(Spec->SpecFilePath);
   if (Spec->SpecFileName) 
      SO->SpecFile.FileName = SUMA_copy_string(Spec->SpecFileName);
   
   SUMA_RETURN(SO);

} /* end SUMA_Load_Spec_Surf */

SUMA_SurfaceObject * SUMA_Load_Spec_Surf_with_Metrics(
                           SUMA_SurfSpecFile *Spec, 
                           int i, 
                           char *tmpVolParName, 
                           int debug)
{  
   static char FuncName[]={"SUMA_Load_Spec_Surf_with_Metrics"};
   SUMA_SurfaceObject *SO=NULL;
   
   SUMA_ENTRY;
   
   if (!Spec) SUMA_RETURN(SO);
   
   if (!(SO = SUMA_Load_Spec_Surf(Spec, i, tmpVolParName, debug))) {
      SUMA_S_Errv("Failed to find surface %s %s.\n",
           SPEC_NAME_DBG(Spec,i), SPEC_TOPO_DBG(Spec,i));
      SUMA_RETURN(SO);
   }
   
   if (!SO->EL) SUMA_SurfaceMetrics_eng(SO, "EdgeList", NULL, debug, 
                                          SUMAg_CF->DsetList);
   if (!SO->MF) SUMA_SurfaceMetrics_eng(SO, "MemberFace", NULL, debug,  
                                          SUMAg_CF->DsetList);
   if (!SO->Label && !(SO->Label = SUMA_SurfaceFileName(SO, NOPE))) {
      SO->Label = SUMA_copy_string("A_Horse_With_No_Name");
   }
   
   SUMA_RETURN(SO);
}

/*!
   Take a mappable SO , loaded as it would be out of, say, SUMA_Load_Spec_Surf, 
   find its metrics, initialize suma structures and add it to DOv
*/
SUMA_Boolean SUMA_PrepAddmappableSO(SUMA_SurfaceObject *SO, SUMA_DO *dov, 
                                    int *N_dov, int debug, DList *DsetList)
{ /* begin SUMA_PrepAddmappableSO */
   static char FuncName[]={"SUMA_PrepAddmappableSO"};
   SUMA_OVERLAYS *NewColPlane=NULL;
   SUMA_Boolean SurfIn = NOPE;
   char DoThis[100];
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SurfIn = YUP;
   
   /* set its MappingRef id to its own */
      SO->LocalDomainParentID = (char *)SUMA_calloc(  strlen(SO->idcode_str)+1, 
                                                      sizeof(char));
      if (SO->LocalDomainParentID == NULL) {
         fprintf( SUMA_STDERR,
                  "Error %s: Failed to allocate for SO->LocalDomainParentID. \n"
                  "That is pretty bad.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
      SO->LocalDomainParentID = strcpy(SO->LocalDomainParentID, SO->idcode_str);

   

   /* if the surface is loaded OK, and it has not been loaded 
      previously, register it */
   if (SurfIn) {
      sprintf(DoThis,"Convexity");
      if (!SO->EL || !SO->FN) strcat(DoThis,", EdgeList");
      if (!SO->MF) strcat(DoThis,", MemberFace");
      if (!SUMA_SurfaceMetrics_eng (SO, DoThis, NULL, debug, DsetList)) {
         fprintf (SUMA_STDERR,
                  "Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
         SUMA_RETURN (NOPE);
      }

      #if SUMA_CHECK_WINDING
      /* if you have surfaces that are not consistent, 
         you should fix them ahead of time
         because orientation affects calculations of normals, 
         areas (signed), convexity 
         etc.... */
      if (!SUMA_SurfaceMetrics_eng (SO, "CheckWind", NULL, debug, DsetList)) {
         fprintf (SUMA_STDERR,
                  "Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
      #endif

      /* Store it into dov, if not there already */
      if (SUMA_whichDO(SO->idcode_str, dov, *N_dov) < 0) {
         if (!SUMA_AddDO(dov, N_dov, (void *)SO,  SO_type, SUMA_WORLD)) {
            fprintf(SUMA_STDERR,"Error %s: Error Adding DO\n", FuncName);
            SUMA_RETURN(NOPE);
         }
      }
      
      /* create the surface controller */
      if (!SO->SurfCont) {
         SO->SurfCont = SUMA_CreateSurfContStruct(SO->idcode_str, SO_type);
      } else {
         SUMA_S_Note("Surface Controller Exists Already.");
      }
      
      {
         SUMA_DSET *dset=NULL;/* create the color plane for Convexity*/

       /* create an overlay plane */
         if (!(dset = (SUMA_DSET *)SUMA_GetCx(SO->idcode_str, DsetList, 1))) {
            SUMA_SL_Err("Failed to find dset!");
            SUMA_RETURN (NOPE);
         }
         NewColPlane = SUMA_CreateOverlayPointer ("Convexity", dset, 
                                                  SO->idcode_str, NULL);
         if (!NewColPlane) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed in SUMA_CreateOverlayPointer.\n", 
                     FuncName);
            SUMA_RETURN (NOPE);
         } 

         /* Add this plane to SO->Overlays */
         if (!SUMA_AddNewPlane ((SUMA_ALL_DO *)SO, NewColPlane, NULL, -1, 1)) { 
            /* duplicate planes will be ignored! */
            SUMA_SL_Err("Failed in SUMA_AddNewPlane");
            SUMA_FreeOverlayPointer(NewColPlane);
            SUMA_RETURN (NOPE);
         }

         if (!SUMAg_CF->scm) {   
            SUMAg_CF->scm = SUMA_Build_Color_maps();
            if (!SUMAg_CF->scm) {
               SUMA_SL_Err("Failed to build color maps.\n");
               SUMA_RETURN(NOPE);            
            }
         }
         if (!SUMA_SetConvexityPlaneDefaults(SO, DsetList)) {
            SUMA_SL_Err("Failed to set plane defaults."); SUMA_RETURN(NOPE);
         }

         /* colorize the plane */
         SUMA_ColorizePlane(NewColPlane);
         
         /* set current plane to it, if none are set */
         if (SO->SurfCont && !SO->SurfCont->curColPlane) {
            SO->SurfCont->curColPlane = NewColPlane;
         }
      }

      /* Create a Mesh Axis for the surface */
      SO->MeshAxis = SUMA_Alloc_Axis ("Surface Mesh Axis", AO_type);
      if (SO->MeshAxis == NULL) {
         fprintf(SUMA_STDERR,"Error %s: Error Allocating axis\n", FuncName);
         SUMA_RETURN(NOPE);
      }
      /* Change the defaults of Mesh axis to fit standard  */
      /* For the moment, use Box Axis */
      SO->MeshAxis->atype = SUMA_SCALE_BOX;
      SUMA_MeshAxisStandard (SO->MeshAxis, (SUMA_ALL_DO *)SO);
      /*turn on the viewing for the axis */
      SO->ShowMeshAxis = NOPE;


   }
   SUMA_RETURN(YUP);

} /* end SUMA_PrepAddmappableSO */


SUMA_Boolean SUMA_Load_SO_NodeMarker(SUMA_SurfaceObject *SO, 
                                     char *NodeMarker)
{
   static char FuncName[]={"SUMA_Load_SO_NodeMarker"};
   SUMA_NIDO *nido=NULL;
   int i=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !NodeMarker) SUMA_RETURN(NOPE);
   
   SUMA_LHv("Loading %s\n", NodeMarker);
   if (!(nido = SUMA_ReadNIDO (NodeMarker, SO->idcode_str))) {
      SUMA_S_Errv("Failed to load %s\n", NodeMarker);
      SUMA_RETURN(NOPE);
   }
   nido->do_type = NIDO_type;
   
   if (SO->CommonNodeObject) {
      SUMA_Free_Displayable_Object_Vect(SO->CommonNodeObject,1);
      SO->CommonNodeObject = NULL;
   }
   SO->CommonNodeObject = (SUMA_DO *)SUMA_calloc(1,sizeof(SUMA_DO));
   
   SO->CommonNodeObject->OP = (void *)nido; 
   SO->CommonNodeObject->ObjectType = NIDO_type;
   SO->CommonNodeObject->CoordType = SUMA_WORLD;
   nido = NULL;

   SUMA_LH( "Need to turn SO->CommonNodeObject to "
            "SO->NodeObjects, and SO->NodeNIDOObjects");
   #if 0
   /* this approach is less flexible, only one nel allowed in CommonNodeObject
   and it is not any faster than the second approach, wich allows for any NIDO
   to be propagated 
   Just don't have both modes on, you'll be drawing similar objects twice */  
   if (SO->NodeObjects) {
      SUMA_Free_Displayable_Object_Vect(SO->NodeObjects, 1);
   }
   SO->NodeObjects = SUMA_Multiply_NodeObjects( SO,  SO->CommonNodeObject);
   #else
   if (SO->NodeNIDOObjects) {
      for (i=0; i<SO->N_Node; ++i) 
         if (SO->NodeNIDOObjects[i]) SUMA_free_NIDO(SO->NodeNIDOObjects[i]);
      SUMA_free(SO->NodeNIDOObjects);   SO->NodeNIDOObjects = NULL;
   }
   SO->NodeNIDOObjects = 
      SUMA_Multiply_NodeNIDOObjects(SO,  SO->CommonNodeObject, NULL, -1);
   #endif
   
   SUMA_RETURN(YUP);
}


/*! 
   Call the function engine, with debug turned on.      20 Oct 2003 [rickr]
*/
SUMA_Boolean SUMA_LoadSpec (SUMA_SurfSpecFile *Spec, SUMA_DO *dov, 
                            int *N_dov, char *VolParName)
{/* SUMA_LoadSpec */
   static char FuncName[]={"SUMA_LoadSpec"};

   SUMA_ENTRY;

   SUMA_RETURN( SUMA_LoadSpec_eng(Spec, dov, N_dov, VolParName, 
                                  1, SUMAg_CF->DsetList) );

}/* SUMA_LoadSpec */

/* - appended _eng to engine function name             20 Oct 2003 [rickr]
 * - added debug parameter
 * - only output non-error info when debug flag is set
 * - debug level 1, slight detail, level 2 more detail
*/
/*! 
   Loads the surfaces specified in Spec and stores them in DOv
*/
static int LoadPacify = 0;
void SetLoadPacify(int k) { LoadPacify = k; }
int  GetLoadPacify(void) { return LoadPacify; }

SUMA_Boolean SUMA_LoadSpec_eng (
                  SUMA_SurfSpecFile *Spec, SUMA_DO *dov, int *N_dov, 
                  char *VolParName, int debug, DList *DsetList)
{/* SUMA_LoadSpec_eng */
   static char FuncName[]={"SUMA_LoadSpec_eng"};
   int i, k;
   char *tmpid, *tmpVolParName = NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Axis *EyeAxis;
   SUMA_OVERLAYS *NewColPlane=NULL;
   SUMA_Boolean SurfIn = NOPE;
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;
   
   if (LocalHead) debug = 1;
   
   if ( debug )
       SUMA_S_Notev("Expecting to read %d surfaces, %d DOs.\n", 
                     Spec->N_Surfs, Spec->N_DO);
   for (i=0; i<Spec->N_Surfs; ++i) { /* first loop across mappable surfaces */
      /*locate and load all Mappable surfaces */
      if (Spec->LocalDomainParent[i][0] == '\0') {
         /* assume surface is local domain parent, otherwise, 
         surface controller would crash      ZSS  July 13 2010 */
         if (debug) 
            SUMA_S_Warnv(
               "MappingRef field unavailable for %s in spec file, "
               "Assuming MappingRef = SAME\n"
               "You might have problems linking to volume.\n",
                SO->Label);
         sprintf(Spec->LocalDomainParent[i], "SAME");
      } 
      if (SUMA_iswordin(Spec->LocalDomainParent[i],"SAME") == 1) { 
         /* Mappable surfaces */
         if ( debug || LoadPacify) { /* turned this back on as a pacifier */
	         fprintf (SUMA_STDERR,"\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv");
	         fprintf (SUMA_STDERR,
		               "Surface #%d/%d(Local Domain Parent), loading ...\n",
                     i+1, Spec->N_Surfs );
	      }

         if (Spec->VolParName[i][0] != '\0') {
            SUMA_LH("Using Volume Parent Specified in Spec File.\n"
                    "This overrides -sv option.");
            tmpVolParName = Spec->VolParName[i];
         }else {
            tmpVolParName = VolParName;
         }
         
         SO = SUMA_Load_Spec_Surf(Spec, i, tmpVolParName, debug);
         if (SO) SurfIn = YUP;
         else {
            SurfIn = NOPE;
            SUMA_SL_Err("Failed to read surface.");
            SUMA_RETURN(NOPE);
         }
         
         /* store the surface's idcode pointer for use in 
            non mappable bloc below */
            Spec->IDcode[i] = SO->idcode_str;
         
         /* check if surface read was unique 
         it's inefficient to check after the surface is read, 
         but idcode is generated in the read routine 
         and users should not be making this mistake too often 
         Search for similar comment elsewhere in the code once
         a better remedy is found*/
         if (SUMA_existSO (SO->idcode_str, dov, *N_dov)) {
            /* Check the filename match, to get around ID collision 
               This modification is no guarantee that collisions
               won't occur but it is a start until I figure out
               the problem with hashcode */
            char *name=NULL, *mname=NULL;
            SUMA_SurfaceObject *SOm = NULL;
            SOm = SUMA_findSOp_inDOv(SO->idcode_str, dov, *N_dov);
            mname = SUMA_SurfaceFileName(SOm, 1);
            name = SUMA_SurfaceFileName(SO, 1);
            if (mname && name && strcmp(mname, name)) {
               char *stmp;
               /* give SO a new ID */
               stmp = SUMA_append_replace_string(name, SO->idcode_str,"_",0);
               SUMA_ifree(SO->idcode_str);
               SUMA_NEW_ID(SO->idcode_str, stmp);
               SUMA_ifree(stmp);
            }
            SUMA_ifree(name); SUMA_ifree(mname);
         }
         if (SUMA_existSO (SO->idcode_str, dov, *N_dov)) {
            SUMA_SurfaceObject *SOm = NULL;
            SOm = SUMA_findSOp_inDOv(SO->idcode_str, dov, *N_dov);
            SUMA_S_Errv("Surface %s %s (id %s) is in dov already as %s!\n",
                        SPEC_NAME_DBG(Spec,i), SPEC_TOPO_DBG(Spec,i),
                        SO->idcode_str, SOm->Label);
               
            /* free SO */
            if (!SUMA_Free_Surface_Object (SO)) {
               fprintf(SUMA_STDERR,"Error %s: Error freeing SO.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            SO = NULL;
            SurfIn = NOPE;
         } else {
            if (!SUMA_PrepAddmappableSO(SO, dov, N_dov, debug, DsetList)) {
               SUMA_SL_Err("Failed in SUMA_PrepAddmappableSO.");
               SUMA_RETURN(NOPE);
            }
         }  
         SurfIn = NOPE;
         if (SO && Spec->LabelDset[i][0] != '\0') {
            SUMA_LHv("Will need to load label dset %s for %s\n",
                         Spec->LabelDset[i], SO->Label);
            /* load it straight to SO */
            if (!(SUMA_LoadDsetOntoSO_eng(Spec->LabelDset[i], (void *)SO,
                                    -1, 0, 0, &NewColPlane))) {
               SUMA_S_Errv("Failed to read %s\n", Spec->LabelDset[i]);
               SUMA_RETURN(NOPE);
            }
            /* do some deeds on this overlay */
            NewColPlane->GlobalOpacity = 
                  SUMA_floatEnv("SUMA_LabelDsetOpacity", 0.2);
            if (  SUMA_isEnv("SUMA_ShowLabelDsetAtStartup","col") ||
                  SUMA_isEnv("SUMA_ShowLabelDsetAtStartup","yes"))
               NewColPlane->ShowMode = SW_SurfCont_DsetViewCol;
            else if (  SUMA_isEnv("SUMA_ShowLabelDsetAtStartup","con") ) {
               NewColPlane->ShowMode = SW_SurfCont_DsetViewCon;
            }else if (  SUMA_isEnv("SUMA_ShowLabelDsetAtStartup","c&c") ) {
               NewColPlane->ShowMode = SW_SurfCont_DsetViewCaC;
            }else if (  SUMA_isEnv("SUMA_ShowLabelDsetAtStartup","xxx") ||
                  SUMA_isEnv("SUMA_ShowLabelDsetAtStartup","no")) {
               NewColPlane->ShowMode = SW_SurfCont_DsetViewXXX;
            }else{ /* show as contours only */
               NewColPlane->ShowMode = SW_SurfCont_DsetViewCon;
            }
            /* create contours for this monster */
            SUMA_ContourateDsetOverlay(NewColPlane, NULL);
            
            /* move that plane down the stack, nice to have convexity stay
            on top */
            SUMA_MovePlaneDown((SUMA_ALL_DO *)SO, NewColPlane->Name);
            
            NewColPlane=NULL;          /* don't let anyone here use it */
         }
         if (SO && Spec->NodeMarker[i][0] != '\0') {
            SUMA_LHv("Will load NodeMarker %s for %s\n",
                         Spec->NodeMarker[i], SO->Label);
            if (!(SUMA_Load_SO_NodeMarker(SO, Spec->NodeMarker[i]))) {
               SUMA_S_Errv("Failed to loa NodeMarker %s onto %s\n"
                           "Plodding on nonetheless.\n",
                           Spec->NodeMarker[i], SO->Label);
            }
            SUMA_LHv("NodeMarker %s loaded\n", Spec->NodeMarker[i]);
         }
         /* Any dsets identically named? If so, load them */
         if (SUMA_isEnv("SUMA_AutoLoad_Matching_Dset","YES"))
                                          SUMA_AutoLoad_SO_Dsets(SO);
      }/* Mappable surfaces */
   }/* first loop across mappable surfaces */

   for (i=0; i<Spec->N_Surfs; ++i) { /* Now locate and load all 
                                       NON Mappable surfaces */

      if (Spec->VolParName[i][0] != '\0') {
         if (VolParName) {
            SUMA_LH("Using Volume Parent Specified in Spec File.\n"
                     "This overrides -sv option.");
         }
         tmpVolParName = Spec->VolParName[i];
      }else {
         tmpVolParName = VolParName;
      }

      if (SUMA_iswordin(Spec->LocalDomainParent[i],"SAME") != 1) { 
               /* Non Mappable surfaces */
	      if ( debug  || LoadPacify) { \
            fprintf (SUMA_STDERR,"\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv");
            fprintf (SUMA_STDERR,
   		            "Surface #%d/%d (mappable via Local Domain Parent), "
                     "loading ...\n",i+1, Spec->N_Surfs);
	      }

         SO = SUMA_Load_Spec_Surf(Spec, i, tmpVolParName, debug);
         if (SO) SurfIn = YUP;
         else {
            SurfIn = NOPE;
            SUMA_SL_Err("Failed to read surface.");
            SUMA_RETURN(NOPE);
         }
         


         /* check if surface read was unique 
            it's inefficient to check after the surface is read, 
            but idcode is generated in the read routine 
            and users should not be making this mistake too often */
            if (SUMA_existSO (SO->idcode_str, dov, *N_dov)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Surface %d is specifed more than once.\n"
                        "Multiple copies ignored.\n", FuncName, i);
               /* free SO */
               if (!SUMA_Free_Surface_Object (SO)) {
                  fprintf( SUMA_STDERR,
                           "Error %s: Error freeing SO.\n", FuncName);
                  SUMA_RETURN (NOPE);
               }
               SurfIn = NOPE;
            }

         /* if the surface is loaded OK, 
            and it has not been loaded previously, register it */
         if (SurfIn) {
            Spec->IDcode[i] = SO->idcode_str; 
                              /* add its ID to its Spec entry */
            /* Create a Mesh Axis for the surface */
            SO->MeshAxis = SUMA_Alloc_Axis ("Surface Mesh Axis", AO_type);
            if (SO->MeshAxis == NULL) {
               fprintf( SUMA_STDERR,
                        "Error %s: Error Allocating axis\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            /* Change the defaults of Mesh axis to fit standard  */
            SUMA_MeshAxisStandard (SO->MeshAxis, (SUMA_ALL_DO *)SO);
            /*turn off the viewing for the axis */
            SO->ShowMeshAxis = NOPE;

            /* Store it into dov */
            if (!SUMA_AddDO(dov, N_dov, (void *)SO,  SO_type, SUMA_WORLD)) {
               fprintf(SUMA_STDERR,"Error %s: Error Adding DO\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            
         /* set its MappingRef id to NULL if none is specified */
            if (Spec->LocalDomainParent[i][0] == '\0') {
               /* This should not happen after July 13 2010 fix above */
               SO->LocalDomainParentID = NULL; /* no known MapRef_idcode */
               SUMA_S_Warnv(
                  "MappingRef field unavailable for %s in spec file ",
                   SO->Label);
            } else {
               /* make sure that specified Mapping ref had been loaded */
                  int j = 0, ifound = -1;
                  while (j < Spec->N_Surfs) {
                     if (LocalHead) {
                        fprintf( SUMA_STDERR,
                                 "%s-voila%d/%d:\n%s\n%s\n%s\n%s\n", 
                                 FuncName, j, Spec->N_Surfs,
                                 Spec->LocalDomainParent[i], Spec->CoordFile[j],
                                 Spec->TopoFile[j], Spec->SurfaceFile[j]); 
                     }
                     if (strcmp( Spec->LocalDomainParent[i], 
                                 Spec->CoordFile[j]) == 0 || 
                         strcmp( Spec->LocalDomainParent[i], 
                                 Spec->TopoFile[j]) == 0 ||  
                         strcmp( Spec->LocalDomainParent[i],
                                 Spec->SurfaceFile[j]) == 0) {
                        /* found a match */
                        ifound = j;
                        j = Spec->N_Surfs + 1;   
                     }
                     ++j;
                  }
               if (ifound >= 0) { /* found */
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,
                              "ifound = %d, i = %d\n"
                              "Spec->LocalDomainParent[i]:->%s<-\n", 
                              ifound, i, Spec->LocalDomainParent[i]);
                  if (!SUMA_existSO (Spec->IDcode[ifound], dov, *N_dov)) {
                     fprintf( SUMA_STDERR,
                              "MappingRef unavailable, "
                              "that should not happen here.\n");
                     SO->LocalDomainParentID = NULL;
                     /* showme the contents */
                     if (!SUMA_ShowSpecStruct (Spec, NULL, 3)) {
                        SUMA_SL_Err("Failed in SUMA_ShowSpecStruct\n");
                        exit(1);
                     }
                  } else {
                     /*fprintf(SUMA_STDERR,
                              "MappingRef found in mappable surfaces\n");*/
                     SO->LocalDomainParentID = 
                           (char *)SUMA_calloc( strlen(Spec->IDcode[ifound])+1,
                                                sizeof(char));
                     if (SO->LocalDomainParentID == NULL) {
                        fprintf(SUMA_STDERR,
                           "Error %s: Failed to allocate for "
                           "SO->LocalDomainParentID. \n"
                           "That is pretty bad.\n", FuncName);
                        SUMA_RETURN (NOPE);
                     }
                     SO->LocalDomainParentID = 
                        strcpy(SO->LocalDomainParentID, Spec->IDcode[ifound]);
                  }
               } else {
                  if (debug) 
                     SUMA_S_Warnv(
                        "MappingRef field unavailable for %s in spec file, "
                        "You might have problems linking to volume.\n",
                         SO->Label);
                  SO->LocalDomainParentID = NULL;
               }
            }
            
            /* create the colorlist vector and calculate the surface metrics 
               with the possibility of inheriting from the mapping reference */
            {
               SUMA_SurfaceObject *SOinh = NULL;
               int ifound = -1; 
               
               if (SO->LocalDomainParentID) {   
                  ifound =  SUMA_findSO_inDOv ( SO->LocalDomainParentID, 
                                                dov, *N_dov);
                  if (ifound < 0) {
                     SOinh = NULL;
                  }else {
                     SOinh = (SUMA_SurfaceObject *)(dov[ifound].OP);
                  }
               } else SOinh = NULL;
               
               /* deal with surface controller */
               if (SOinh) {
                  #if SUMA_SEPARATE_SURF_CONTROLLERS
                  /* leave controllers separate */ 
                  if (!SO->SurfCont) {
                     SO->SurfCont = 
                        SUMA_CreateSurfContStruct(SO->idcode_str, SO_type); 
                  } else {
                     SUMA_S_Note("Surface Controller Exists Already (b)\n");
                  }
                  #else
                  /* create a link to the surface controller pointer */
                  if (!SO->SurfCont) {
                     SO->SurfCont = (SUMA_X_SurfCont*)
                                    SUMA_LinkToPointer((void *)SOinh->SurfCont);
                  } else {
                     SUMA_S_Note("Surface Controller Exists Already (c)\n");
                  }
                  #endif
               } else {
                  /* brand new one */
                  if (!SO->SurfCont) {
                     SO->SurfCont = SUMA_CreateSurfContStruct(SO->idcode_str, 
                                                              SO_type);
                  } else {
                     SUMA_S_Note("Surface Controller Exists Already (d)\n");
                  }
               }

               
               if (!SUMA_SurfaceMetrics_eng (SO, "EdgeList, MemberFace", 
                                             SOinh, debug, DsetList)) {
                  fprintf (SUMA_STDERR,
                           "Error %s: Failed in SUMA_SurfaceMetrics.\n", 
                           FuncName);
                  SUMA_RETURN (NOPE);
               }
            }  
               
            SurfIn = NOPE;
         }
         if (Spec->LabelDset[i][0] != '\0') { /* I do not think this should be */
            SUMA_S_Notev("Label dsets should only be attached to \n"
                         "surfaces with LocalDomainParent = SAME).\n"
                         "LabelDset %s is ignored.\n",
                         Spec->LabelDset[i]);
         }
         if (SO && Spec->NodeMarker[i][0] != '\0') {
            SUMA_S_Notev("Will need to load NodeMarker %s for non mappable %s\n",
                         Spec->NodeMarker[i], SO->Label);
         }
         if (SUMA_isEnv("SUMA_AutoLoad_Matching_Dset","YES"))
                                        SUMA_AutoLoad_SO_Dsets(SO);

      }/* Non Mappable surfaces */

   }/*locate and load all NON Mappable surfaces */
   
   SUMA_LHv("Have %d DOs to load\n", Spec->N_DO);
   for (i=0; i<Spec->N_DO; ++i) {
      switch ((SUMA_DO_Types)Spec->DO_type[i]) {
         case TRACT_type: {
            SUMA_LoadSegDO (Spec->DO_name[i], NULL );
            break; }
         case MASK_type: {
            SUMA_LoadMaskDO (Spec->DO_name[i], NULL );
            break; }
         case VO_type: {
            SUMA_LoadVolDO (Spec->DO_name[i], SUMA_WORLD, NULL,1); 
            break; }
         case GDSET_type:
            SUMA_LHv("Loading graph dset %s\n",Spec->DO_name[i]);
            /* Expecting it to be a graph dset */
            if (!(SUMA_LoadDsetOntoSO_eng(Spec->DO_name[i], 
                                          NULL, 1, 1, 1, NULL))) {
               SUMA_S_Errv("Failed to load %s\n", Spec->DO_name[i]);
            }
            break;
         case CDOM_type:
            SUMA_LoadCIFTIDO (Spec->DO_name[i], 
                              SUMA_WORLD, NULL, 1, 1, 1, 1, NULL);
            break;
         default:
            SUMA_S_Errv("Bad or unexpected type %s for %s\n",
                  SUMA_ObjectTypeCode2ObjectTypeName(Spec->DO_type[i]),
                     Spec->DO_name[i]);
            break;
      }
   }
   
   SUMA_RETURN (YUP);
}/* SUMA_LoadSpec_eng */


/*!
   SUMA_SurfaceMetrics - call the engine with debug set    20 Oct 2003 [rickr]
*/
SUMA_Boolean SUMA_SurfaceMetrics(SUMA_SurfaceObject *SO,
                                  const char *Metrics, 
                                  SUMA_SurfaceObject *SOinh)
{
   static char FuncName[]={"SUMA_SurfaceMetrics"};
   
   SUMA_ENTRY;

   SUMA_RETURN(SUMA_SurfaceMetrics_eng(SO, Metrics, SOinh, 1, 
                                       SUMAg_CF->DsetList));
}


/* - appended _eng to engine function name                 20 Oct 2003 [rickr]
 * - added debug parameter
 * - only output non-error info when debug flag is set
*/
/*!
   calculate surface properties
   
   ans = SUMA_SurfaceMetrics_eng (SO, Metrics, SOinh, debug, DList *DsetList)
   \param SO (SUMA_SurfaceObject *)
   \param Metrics (const char *) list of parameters to compute. Supported parameters are (case sensitive):
      "Convexity", "PolyArea", "Curvature", "EdgeList", "MemberFace", "CheckWind"
      You can specify more than one parameter "Convexity, PolyArea"
      if the field of a certain parameter is not NULL then it is assumed that 
      this parameter was computed at an earlier time and will not be recalculated.
      Some parameters require the computation of others and that's done automatically.
   \param SOinh (SUMA_SurfaceObject *) Some of the metrics can be inherited from SOinh (done through inodes)
      if things make sense. SOinh is typically the Mapping Reference SO. Pass NULL not to use this feature.
      Currently, only EL and FN can use this feature if the number of nodes, facesets match and SOinh is the 
      mapping reference of SO
   \param debug (int) flag specifying whether to output non-error info
   \param DsetList (DList *)  pointer to list where computed elements are to be stored
                              as datasets. For the moment, this pointer can be NULL and
                              if that is the case then nothing will get stored as a dataset.
   \return ans (SUMA_Boolean) NOPE = failure
   
   Convexity : Fills Cx field in SO, An inode is also created
   
   EdgeList also runs SUMA_Build_FirstNeighb
   
   Curvature requires also PolyArea, FaceNeighb and EdgeList
   CheckWind requires EdgeList
   
      
*/
SUMA_Boolean SUMA_SurfaceMetrics_eng (
         SUMA_SurfaceObject *SO, 
         const char *Metrics, 
         SUMA_SurfaceObject *SOinh, 
         int debug, 
         DList *DsetList)
{
   static char FuncName[]={"SUMA_SurfaceMetrics_eng"};
   float *Cx=NULL, *SOCx = NULL;
   SUMA_Boolean DoConv, DoArea, DoCurv, DoEL, DoMF, DoWind, DoDW, 
                LocalHead = NOPE;
   int i = 0;
   
   SUMA_ENTRY;

   if (debug > 1)
      fprintf (SUMA_STDERR,
               "%s: Calculating surface metrics, please be patient...\n", 
               FuncName);
   
   if (!DsetList) {
      SUMA_SL_Err("DsetList now is a must.");
      SUMA_RETURN(NOPE);
   }
   DoConv = DoArea = DoCurv = DoEL = DoMF = DoWind = NOPE;
   DoDW = YUP; /* No need to skimp on this one, costs nothing */
   
   if (SUMA_iswordin (Metrics, "Convexity")) DoConv = YUP;
   if (SUMA_iswordin (Metrics, "PolyArea")) DoArea = YUP;
   if (SUMA_iswordin (Metrics, "Curvature")) DoCurv = YUP;
   if (SUMA_iswordin (Metrics, "EdgeList")) DoEL = YUP;
   if (SUMA_iswordin (Metrics, "MemberFace")) DoMF = YUP;
   if (SUMA_iswordin (Metrics, "CheckWind")) DoWind = YUP;
   
   /* check for input inconsistencies and warn */
   if (!DoConv && !DoArea && !DoCurv && !DoEL  && !DoMF && !DoWind && !DoDW) {
      if (debug) fprintf ( SUMA_STDERR,
                           "Warning %s: Nothing to do.\n", FuncName);
      SUMA_RETURN (YUP);
   }
   
   if (DoConv && (SOCx = (float *)SUMA_GetCx (SO->idcode_str, DsetList, 0))) {
      if (debug) fprintf ( SUMA_STDERR,
                           "Warning %s: SOCx != NULL \n"
                           "and thus appears to have been precomputed.\n",
                           FuncName);
      DoConv = NOPE;
   }
   
   if (DoArea && SO->PolyArea != NULL) {
      if (debug) fprintf ( SUMA_STDERR,
                           "Warning %s: SO->PolyArea != NULL and "
                           "thus appears to have been precomputed.\n",
                           FuncName);
      DoArea = NOPE;
   }
   
   if (DoCurv && SO->SC != NULL) {
      if (debug) fprintf ( SUMA_STDERR,
                           "Warning %s: SO->SC != NULL and thus "
                           "appears to have been precomputed.\n", 
                           FuncName);
      DoCurv = NOPE;
   }
   
   if (DoMF && SO->MF != NULL) {
      if (debug) fprintf ( SUMA_STDERR,
                           "Warning %s: SO->MF != NULL and thus "
                           "appears to have been precomputed.\n", 
                           FuncName);
      DoMF = NOPE;
   }
   
   if (DoEL && (SO->EL != NULL || SO->FN != NULL)) {
      if (debug) fprintf ( SUMA_STDERR,
                           "Warning %s: SO->EL != NULL || SO->FN != NULL "
                           "and thus appears to have been precomputed.\n",
                           FuncName);
      DoEL = NOPE;
   }
   
   if (DoEL && SOinh) {
      if (strcmp(SO->LocalDomainParentID, SOinh->idcode_str)) {
         SUMA_SL_Warn(  "Cannot inherit Edge List\n"
                        "and First Neightbor.\n"
                        "Cause: idcode mismatch.\n"
                        "Independent lists will\n"
                        "be created.\n" );
         SOinh = NULL;
      }  else if (SO->N_Node != SOinh->N_Node || 
                  SO->N_FaceSet != SOinh->N_FaceSet) {
         SUMA_SL_Note(  "(IGNORE for surface with cuts)\n"
                        "Cannot inherit Edge List\n"
                        "and First Neightbor.\n"
                        "Cause: Node number mismatch.\n"
                        "Independent lists will\n"
                        "be created.\n");
         SOinh = NULL;      
      }
   }
   
   if (DoMF && SOinh) {
      if (strcmp(SO->LocalDomainParentID, SOinh->idcode_str)) {
         SUMA_SL_Warn(  "Cannot inherit MemberFaceSet\n"
                        "Cause: idcode mismatch.\n"
                        "Independent lists will\n"
                        "be created.\n" );
         SOinh = NULL;
      } else if ( SO->N_Node != SOinh->N_Node || 
                  SO->N_FaceSet != SOinh->N_FaceSet) {
         SUMA_SL_Warn(  "(IGNORE for surface patches)\n"
                        "Cannot inherit MemberFaceSet\n"
                        "Cause: Node number mismatch.\n"
                        "Independent lists will\n"
                        "be created.\n");
         SOinh = NULL;      
      }
   }
    
   /* prerequisits */
   if (DoCurv) {
      DoArea = YUP;
      DoEL = YUP;
   }
   
   if (DoWind) {
      DoEL = YUP;
   }

   if (DoConv && (!SO->EL || !SO->FN)) {
      DoEL = YUP;
   }
   
   /* the computations */
   if (DoArea) {
      /* create the triangle Area  */
      if (SO->NodeDim == 3) {
         if (debug) 
            fprintf( SUMA_STDOUT, 
                     "%s: Calculating triangle areas ...\n", FuncName); 
            fflush(SUMA_STDOUT); 
         SO->PolyArea = SUMA_TriSurf3v (  SO->NodeList, 
                                          SO->FaceSetList, SO->N_FaceSet);
      } else {
         if (debug) 
            fprintf( SUMA_STDOUT, 
                     "%s: Calculating polygon areas ...\n", FuncName); 
            fflush(SUMA_STDOUT); 
         SO->PolyArea = SUMA_PolySurf3 (  SO->NodeList, SO->N_Node, 
                                          SO->FaceSetList, SO->N_FaceSet, 
                                          SO->NodeDim, SO->FaceNormList, NOPE);
         #if 0
            /* a test of the functions for calculating areas */
            {
               int ji, in0, in1, in2;
               float *n0, *n1, *n2, A;
               for (ji=0; ji<SO->N_FaceSet; ++ji) {
                  in0 = SO->FaceSetList[3*ji];
                  in1 = SO->FaceSetList[3*ji+1];
                  in2 = SO->FaceSetList[3*ji+2];
                  n0 = &(SO->NodeList[3*in0]);
                  n1 = &(SO->NodeList[3*in1]);
                  n2 = &(SO->NodeList[3*in2]);
                  A = SUMA_TriSurf3 (n0, n1, n2);
                  if (abs(A - SO->PolyArea[ji]) > 0.00001) {
                     fprintf (SUMA_STDERR, 
    "Error %s: Failed comparing SUMA_TriSurf3 to SUMA_PolySurf3. A = %f vs %f.\n"
    "Tri = [ %f, %f, %f; %f, %f, %f; %f, %f, %f]\n", 
                        FuncName, A, SO->PolyArea[ji], 
                        n0[0], n0[1], n0[2], 
                        n1[0], n1[1], n1[2], 
                        n2[0], n2[1], n2[2]);
                  }else fprintf (SUMA_STDERR, "-");

                  SUMA_TRI_AREA (n0, n1, n2, A);
                  if (abs(A - SO->PolyArea[ji]) > 0.00001) {
                     fprintf (SUMA_STDERR, 
      "Error %s: Failed comparing SUMA_TRI_AREA to SUMA_PolySurf3. "
      "%f vs %f Exiting.\n", 
                        FuncName, A, SO->PolyArea[ji]);
                  }else fprintf (SUMA_STDERR, ".");
               }  
            }
         #endif

      }
      if (SO->PolyArea == NULL) {
         fprintf(SUMA_STDERR,
                  "Error %s: Error in SUMA_PolySurf3 or SUMA_TriSurf3v\n", 
                  FuncName);
      }
   }
   
   if (DoEL) {
      if (!SOinh) {
         /* create the edge list, it's nice and dandy */
         if (LocalHead) 
            fprintf(SUMA_STDOUT, "%s: Making Edge list ....\n", FuncName); 
         if (SO->EL) {
            fprintf (SUMA_STDERR,
                     "Warning %s:"
                     " SO->FN appears to have been computed before. \n", 
                     FuncName); 
         } else {
            SO->EL = SUMA_Make_Edge_List_eng (SO->FaceSetList, SO->N_FaceSet, 
                                              SO->N_Node, SO->NodeList, debug, 
                                              SO->idcode_str);
            if (SO->EL == NULL) {
               fprintf( SUMA_STDERR, 
                        "Error %s: Failed in SUMA_Make_Edge_List.\n"
                        " Neighbor list will not be created\n", FuncName);
            } else {
               if (LocalHead) 
                  fprintf( SUMA_STDOUT, 
                           "%s: Making Node Neighbor list ....\n", FuncName); 
               /* create the node neighbor list */
               if (SO->FN) {
                  fprintf (SUMA_STDERR,
                           "Warning %s:" 
                           " SO->FN appears to have been computed before. \n", 
                           FuncName); 
               } else {
                  SO->FN = SUMA_Build_FirstNeighb (SO->EL, SO->N_Node, 
                                                   SO->idcode_str, debug);   
                  if (SO->FN == NULL) {
                     fprintf( SUMA_STDERR, 
                              "Error %s: Failed in SUMA_Build_FirstNeighb.\n", 
                              FuncName);
                  }
               } 
            }
         }
      } else {
         if (LocalHead) 
            fprintf( SUMA_STDOUT, 
                     "%s: Linking Edge List and First Neighbor Lits ...\n", 
                     FuncName);
         SO->FN = (SUMA_NODE_FIRST_NEIGHB*)SUMA_LinkToPointer((void *)SOinh->FN);
         SO->EL = (SUMA_EDGE_LIST*)SUMA_LinkToPointer((void *)SOinh->EL);
      }
   }
   
   if (DoConv) {
      /* calculate convexity */
      if (LocalHead) 
         fprintf(SUMA_STDOUT, "%s: Calculating convexity ...\n", FuncName);
      Cx = SUMA_Convexity(SO->NodeList, SO->N_Node, SO->NodeNormList, 
                          SO->FN, NULL);
      if (Cx == NULL) {
         fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Convexity\n", FuncName);
      }   
            
      /* flip sign of convexity if it's a SureFit Surface */
      if (  (SO->normdir == 0 && (SO->FileType == SUMA_SUREFIT)) /* guess */
            || SO->normdir == -1  /* You know they'z got to be flipped */
         ) {
         for (i=0; i < SO->N_Node; ++i) {
            Cx[i] = -Cx[i];
         }
      }
      
      #if 0
      { 
         /* smooth the estimate twice*/
         float *attr_sm;
         attr_sm = SUMA_SmoothAttr_Neighb (  Cx, SO->N_Node, NULL, 
                                             SO->FN, 1, NULL);
         if (attr_sm == NULL) {
               fprintf( stderr,
                        "Error %s: Failed in SUMA_SmoothAttr_Neighb\n", 
                        FuncName);
         }   else {
            Cx = SUMA_SmoothAttr_Neighb ( attr_sm, SO->N_Node, Cx, 
                                          SO->FN, 1, NULL);
            if (attr_sm) SUMA_free(attr_sm);
         }
      }
      #else 
         /* smooth the estimate as much as specified*/
         {
            char *eee = getenv("SUMA_NumConvSmooth");
            if (eee) {
               int N_smooth = (int)strtod(eee, NULL);
               if (N_smooth > 1) {
                  Cx = SUMA_SmoothAttr_Neighb_Rec (Cx, SO->N_Node, Cx, 
                                                   SO->FN, 1, N_smooth, NULL, 1);
               } else {
                  Cx = SUMA_SmoothAttr_Neighb_Rec (Cx, SO->N_Node, Cx, 
                                                   SO->FN, 1, 5, NULL, 1);
               }
            }   
         }
      #endif
      
      if (Cx == NULL) {
         fprintf (SUMA_STDERR, 
                  "Error %s: Failed in SUMA_SmoothAttr_Neighb\n", FuncName);
      } 
      
      /* create a dataset of the convexity */
      if (DsetList){ /* put the convexity as a DataSet */
         SUMA_DSET *dset = NULL;
         char *name_tmp=NULL;
         if ((name_tmp = SUMA_SurfaceFileName(SO, 1))) {
            /* Go with this baby, maybe someday modify
               fuction above to return full path, making it
               more robust */
            name_tmp = SUMA_append_replace_string("Convexity_",name_tmp,"",2);
         } else if (SO->Label) {
            name_tmp = SUMA_append_string("Convexity_",SO->Label);
         } else if (SO->idcode_str) {
            name_tmp = SUMA_append_string("Convexity_",SO->idcode_str);
         } else {
            name_tmp = SUMA_append_string("Convexity_","Give_Peace_A_Chance");
         }
         dset = SUMA_CreateDsetPointer(name_tmp, /*   no file name, but specify a
                                                      name anyway _COD is 
                                                      computed on demand*/
                                       SUMA_NODE_CONVEXITY,
                                       NULL, /* let function create ID code */
                                       SO->idcode_str, /* domain owner */
                                       SO->N_Node);
         SUMA_free(name_tmp); name_tmp = NULL;
         if (!SUMA_InsertDsetPointer(&dset, DsetList, 0)) {
            SUMA_SL_Err("Failed to insert dset into list");
            SUMA_RETURN(NOPE);
         }
         if (!SUMA_AddDsetNelCol (  dset, "convexity", SUMA_NODE_CX, 
                                    (void *)Cx, NULL ,1)) {
            SUMA_SL_Err("Failed in SUMA_AddNelCol");
            SUMA_RETURN(NOPE);
         }
         
         SUMA_free(Cx); Cx = NULL; /* Cx is safe and sound in DsetList */
         
      }
   } /* DoConv */
   
   
   if (DoWind){   
      int trouble;
      /* make sure winding is consistent */
      if (!SUMA_MakeConsistent ( SO->FaceSetList, SO->N_FaceSet, SO->EL, 
                                 1, &trouble)) {
         fprintf( SUMA_STDERR,
                  "Error %s: Failed in SUMA_MakeConsistent.\n", FuncName);
      }else {
         if (LocalHead) fprintf(SUMA_STDERR,"%s: Eeeexcellent.\n", FuncName);
      }
      if (trouble) {
         SUMA_SL_Note(  "Even if winding was made consistent,\n"
                        "Pre-computed normals and normals-related\n"
                        "measures and edge lists will need to be\n"
                        "recalculated.\n"
                        "See also SurfQual and \n"
                        "ConvertSurface's -make_consistent option.\n");
      }
   }

   
   if (DoCurv) {
      /* calculate the curvature */
      if (LocalHead) 
         fprintf(SUMA_STDOUT, "%s: Calculating curvature ...\n", FuncName);
      SO->SC = SUMA_Surface_Curvature (SO->NodeList, SO->N_Node, 
                                       SO->NodeNormList, SO->PolyArea, 
                                       SO->N_FaceSet, SO->FN, SO->EL, NULL, 
                                       debug);
   }
   
   
   if (DoMF) {
      if (!SOinh) {
         /* determine the MemberFaceSets */
         if (LocalHead) 
            fprintf(SUMA_STDOUT, "%s: Determining MemberFaceSets  ...\n", 
                    FuncName);
         SO->MF = SUMA_MemberFaceSets(SO->N_Node, SO->FaceSetList, SO->N_FaceSet,                                       SO->FaceSetDim, SO->idcode_str);
         if (SO->MF->NodeMemberOfFaceSet == NULL) {
            fprintf( SUMA_STDERR,
                     "Error %s: Error in SUMA_MemberFaceSets\n", FuncName);
            SUMA_RETURN (NOPE);/* do not free MF, that's done when SO is freed */
         }else {
         }
      } else { /* inherit */
         if (LocalHead) 
            fprintf(SUMA_STDOUT, "%s: Linking Member Facesets ...\n", FuncName);
         SO->MF = (SUMA_MEMBER_FACE_SETS*)SUMA_LinkToPointer((void*)SOinh->MF);
      }
   }

   /* Initialize drawing masks */
   if (DoDW) {
      if (!SOinh) {
         SO->DW = (SUMA_DRAW_MASKS *)SUMA_calloc(1,sizeof(SUMA_DRAW_MASKS));
         SO->DW->do_type = not_DO_type;
         SO->DW->LinkedPtrType = SUMA_LINKED_DRAW_MASKS_TYPE;
         SO->DW->N_links = 0;
         if (SO->idcode_str) sprintf(SO->DW->owner_id, "%s", SO->idcode_str);
         else SO->DW->owner_id[0] = '\0';
         SUMA_EmptyDrawMasks(SO->DW);
      } else {
         if (LocalHead) 
               fprintf(SUMA_STDOUT, "%s: Linking Drawing Masks ...\n", FuncName);
         SO->DW = (SUMA_DRAW_MASKS *)SUMA_LinkToPointer((void*)SOinh->DW);
      }
   }
   SUMA_RETURN (YUP);
}




/*! function to return a string containing the name of the files 
defining a surface object

   ans = SUMA_SurfaceFileName (SO, MitPath);
   \param SO (SUMA_SurfaceObject *) the surface object
   \param MitPath (SUMA_Boolean) if YUP then path is included
   \ret ans (char *) containing the name of the file from which the surface
      was loaded. If the surface is freesurfer format, the filename is 
      something like rh.smooth.asc. If it's a SureFit surface then you'll 
      get both .coord and .topo xxx.coord__yyy.topo
      ans is allocated in the function, of course, and must be freed after use
*/

char * SUMA_SurfaceFileName (SUMA_SurfaceObject * SO, SUMA_Boolean MitPath)
{
   static char FuncName[]={"SUMA_SurfaceFileName"};
   char *Name=NULL;
   int nalloc=0;
   
   SUMA_ENTRY;

   /* check if recognizable type */
   switch (SO->FileType) {
      case SUMA_VEC:
      case SUMA_SUREFIT:
         if (!SO->Name_coord.Path  || !SO->Name_coord.FileName ||
             !SO->Name_topo.Path   || !SO->Name_topo.FileName) {
            SUMA_RETURN(NULL);
         }
         if (MitPath) nalloc = 
            strlen(SO->Name_coord.Path) + 
            strlen(SO->Name_coord.FileName) +
            strlen(SO->Name_topo.Path) + 
            strlen(SO->Name_topo.FileName) + 5;
         else nalloc =  strlen(SO->Name_coord.FileName) +
                        strlen(SO->Name_topo.FileName) + 5;
         break;
      case SUMA_FT_NOT_SPECIFIED:
      case SUMA_INVENTOR_GENERIC:
      case SUMA_FREE_SURFER:
      case SUMA_FREE_SURFER_PATCH:
      case SUMA_BRAIN_VOYAGER:
      case SUMA_OPENDX_MESH:
      case SUMA_OBJ_MESH:
      case SUMA_BYU:
      case SUMA_GIFTI:
      case SUMA_PREDEFINED:
      case SUMA_MNI_OBJ:
      case SUMA_STL:
      case SUMA_PLY:
         if (!SO->Name.Path || !SO->Name.FileName ) {
            SUMA_RETURN(NULL);
         }
         if (MitPath) 
            nalloc = strlen(SO->Name.Path) + strlen(SO->Name.FileName) + 5;
         else nalloc = strlen(SO->Name.FileName) + 5;
         break;
      default:
         SUMA_error_message(FuncName, "SO_FileType not supported", 0);
         SUMA_RETURN (NULL);
         break;
   } 

   Name = (char *) SUMA_calloc (nalloc, sizeof(char));
   if (!Name) {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate for Name.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   switch (SO->FileType) {
      case SUMA_FT_NOT_SPECIFIED:
      case SUMA_INVENTOR_GENERIC:
      case SUMA_FREE_SURFER:
      case SUMA_FREE_SURFER_PATCH:
      case SUMA_STL:
      case SUMA_PLY:
      case SUMA_OPENDX_MESH:
      case SUMA_OBJ_MESH:
      case SUMA_BYU:
      case SUMA_GIFTI:
      case SUMA_PREDEFINED:
      case SUMA_MNI_OBJ:
      case SUMA_BRAIN_VOYAGER:
         if (MitPath) sprintf(Name,"%s%s", SO->Name.Path, SO->Name.FileName);
         else sprintf(Name,"%s", SO->Name.FileName);
         break;
      case SUMA_SUREFIT:
         if (MitPath) sprintf(Name,"%s%s__%s%s",
                              SO->Name_coord.Path, SO->Name_coord.FileName, 
                              SO->Name_topo.Path, SO->Name_topo.FileName);
         else sprintf(Name,"%s__%s", 
                        SO->Name_coord.FileName, SO->Name_topo.FileName);
         break;
      case SUMA_VEC:
         if (MitPath) sprintf(Name,"%s%s__%s%s", 
                              SO->Name_coord.Path, SO->Name_coord.FileName, 
                              SO->Name_topo.Path, SO->Name_topo.FileName);
         else sprintf(Name,"%s__%s", 
            SO->Name_coord.FileName, SO->Name_topo.FileName);
         break;
      case SUMA_N_SO_FILE_TYPE:
      case SUMA_CMAP_SO:
      case SUMA_FT_ERROR:
         break;
   } 
   SUMA_RETURN (Name);
   
}

/*!
   Guess if a surface is anaomically correct from the name. 
*/
char SUMA_GuessAnatCorrect(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_GuessAnatCorrect"};
   
   SUMA_ENTRY;

   switch (SO->FileType) {
      case SUMA_INVENTOR_GENERIC:
      case SUMA_FREE_SURFER:
      case SUMA_FREE_SURFER_PATCH:
      case SUMA_OPENDX_MESH:
      case SUMA_OBJ_MESH:
      case SUMA_STL:
      case SUMA_PLY:
      case SUMA_BYU:
      case SUMA_MNI_OBJ:
      case SUMA_PREDEFINED:
      case SUMA_BRAIN_VOYAGER:
         if (  SUMA_iswordin (SO->Name.FileName, ".white") == 1 || 
               SUMA_iswordin (SO->Name.FileName, ".smoothwm") == 1 ||
               SUMA_iswordin (SO->Name.FileName, ".pial") == 1 ||
               SUMA_iswordin (SO->Name.FileName, ".orig") == 1 ||
               SUMA_iswordin (SO->Name.FileName, ".fiducial") == 1 ||
               SUMA_iswordin_ci (SO->Name.FileName, "_WM") == 1 || 
               SUMA_iswordin_ci (SO->Name.FileName, "_GM") == 1 || 
               ( SUMA_iswordin (SO->Name.FileName, "_RECO") == 1 && 
               !(SUMA_iswordin (SO->Name.FileName, "_inf") == 1) )
               ) {
            SUMA_RETURN('Y');
         } else {
            SUMA_RETURN('N');
         }
         break;
      case SUMA_SUREFIT:
      case SUMA_VEC:
         if (  SUMA_iswordin (SO->Name_coord.FileName, ".white") == 1 || 
               SUMA_iswordin (SO->Name_coord.FileName, ".smoothwm") == 1 ||
               SUMA_iswordin (SO->Name_coord.FileName, ".pial") == 1 ||
               SUMA_iswordin (SO->Name_coord.FileName, ".orig") == 1 ||
               SUMA_iswordin (SO->Name_coord.FileName, ".fiducial") == 1 ||
               SUMA_iswordin_ci (SO->Name_coord.FileName, ".Fiducial") == 1 ||
               SUMA_iswordin (SO->Name_coord.FileName, ".Raw") == 1 ||
               SUMA_iswordin (SO->Name.FileName, "_WM") == 1 ||
               SUMA_iswordin (SO->Name.FileName, "_GM") == 1
               ) {
            SUMA_RETURN('Y');
         } else {
            SUMA_RETURN('N');
         }
         break;
      case SUMA_N_SO_FILE_TYPE:
      case SUMA_FT_NOT_SPECIFIED:
      case SUMA_CMAP_SO:
      case SUMA_FT_ERROR:
         break;
      case SUMA_GIFTI:
         if (SO->aSO) {
            if (  SUMA_iswordsame_ci (  SUMA_NI_AttrOfNamedElement(
                                                      SO->aSO, 
                                                      "Node_XYZ", 
                                                      "GeometricType"),
                                        "Reconstruction") == 1    ||
                  SUMA_iswordsame_ci (  SUMA_NI_AttrOfNamedElement(
                                                      SO->aSO, 
                                                      "Node_XYZ", 
                                                      "GeometricType"),
                                        "Anatomical") == 1        ) {
               SUMA_RETURN('Y');
            }else {
               SUMA_RETURN('N');
            }
         } else {
            SUMA_RETURN('N');
         }
         break;
   } 
   
   SUMA_RETURN('\0');
}

SUMA_SO_SIDE SUMA_GuessSide(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_GuessSide"};
   char *cc=NULL;
   
   SUMA_ENTRY;
    
   switch (SO->FileType) {
      case SUMA_INVENTOR_GENERIC:
         break;
      case SUMA_FREE_SURFER:
      case SUMA_FREE_SURFER_PATCH:
         if (SUMA_iswordin (SO->Name.FileName, "lh") == 1) {
            SUMA_RETURN(SUMA_LEFT);
         } else if (SUMA_iswordin (SO->Name.FileName, "rh") == 1) {
                     SUMA_RETURN(SUMA_RIGHT);
                  }
         /* try some more */
         if (SUMA_iswordin_ci (SO->Name.FileName, "_lh") == 1 || 
             SUMA_iswordin_ci (SO->Name.FileName, ".lh") == 1 || 
             SUMA_iswordin_ci (SO->Name.FileName, "lh_") == 1 || 
             SUMA_iswordin_ci (SO->Name.FileName, "lh.") == 1) {
            SUMA_RETURN(SUMA_LEFT);
         } else if (SUMA_iswordin_ci (SO->Name.FileName, "_rh") == 1|| 
                    SUMA_iswordin_ci (SO->Name.FileName, ".rh") == 1 || 
                    SUMA_iswordin_ci (SO->Name.FileName, "rh_") == 1 || 
                    SUMA_iswordin_ci (SO->Name.FileName, "rh.") == 1) {
                     SUMA_RETURN(SUMA_RIGHT);
         }
         break;
      case SUMA_BRAIN_VOYAGER:
         if (SUMA_iswordin (SO->Name.FileName, "_LH") == 1) {
            SUMA_RETURN(SUMA_LEFT);
         } else if (SUMA_iswordin (SO->Name.FileName, "_RH") == 1) {
                     SUMA_RETURN(SUMA_RIGHT);
                  }
         break;
      case SUMA_SUREFIT:
         if (SUMA_iswordin_ci (SO->Name_coord.FileName, "left") == 1 ||
             SUMA_iswordin (SO->Name_coord.FileName, ".L.") == 1) {
            SUMA_RETURN(SUMA_LEFT);
         } else if (SUMA_iswordin_ci (SO->Name_coord.FileName, "right") == 1 ||
             SUMA_iswordin (SO->Name_coord.FileName, ".R.") == 1) {
             SUMA_RETURN(SUMA_RIGHT);
                }
         break;
      case SUMA_VEC:
         if (SUMA_iswordin (SO->Name_coord.FileName, "lh") == 1 ||
             SUMA_iswordin (SO->Name_coord.FileName, "left") == 1) {
               SUMA_RETURN(SUMA_LEFT);
         } else if (SUMA_iswordin (SO->Name_coord.FileName, "rh") == 1 ||
                     SUMA_iswordin (SO->Name_coord.FileName, "right") == 1) {
                     SUMA_RETURN(SUMA_RIGHT);
               }
         break;
      case SUMA_FT_NOT_SPECIFIED:
      case SUMA_CMAP_SO:
      case SUMA_N_SO_FILE_TYPE:
      case SUMA_FT_ERROR:
         break;
      case SUMA_OPENDX_MESH:
      case SUMA_OBJ_MESH:
      case SUMA_BYU:
      case SUMA_MNI_OBJ:
      case SUMA_PREDEFINED:
      case SUMA_STL:
      case SUMA_PLY:
         if (SUMA_iswordin (SO->Name.FileName, "lh") == 1 ||
             SUMA_iswordin (SO->Name.FileName, "left") == 1) {
               SUMA_RETURN(SUMA_LEFT);
         } else if (SUMA_iswordin (SO->Name.FileName, "rh") == 1 ||
                  SUMA_iswordin (SO->Name.FileName, "right") == 1) { 
                     SUMA_RETURN(SUMA_RIGHT);
               }
         break;
      case SUMA_GIFTI:
         if (SO->aSO) {
            cc = SUMA_NI_AttrOfNamedElement( SO->aSO, 
                                             "Node_XYZ",
                                             "AnatomicalStructurePrimary");
            if (!cc || !strcmp(cc,"Unknown")) {
               if (SUMA_iswordin (SO->Name.FileName, "lh") == 1 ) {
                  SUMA_RETURN(SUMA_LEFT);
               } else if (SUMA_iswordin (SO->Name.FileName, "rh") == 1 ) {
                  SUMA_RETURN(SUMA_RIGHT);
               } else if (SUMA_iswordin (SO->Name_coord.FileName, "lh") == 1 ) {
                  SUMA_RETURN(SUMA_LEFT);
               } else if (SUMA_iswordin (SO->Name_coord.FileName, "rh") == 1 ) {
                  SUMA_RETURN(SUMA_RIGHT);
               } else {
                  SUMA_RETURN (SUMA_NO_SIDE);
               }
            } else {
               SUMA_RETURN(SUMA_giiStringToNumSide(cc));
            }
         } else {
            SUMA_RETURN(SUMA_NO_SIDE);
         }
         break;
   } 
   
   SUMA_RETURN (SUMA_NO_SIDE);
}

/*!
   tol is the percent tolerance for accepting a surface as a sphere if
   its radius fluctuates by tol percent. 
*/
int SUMA_SetSphereParams(SUMA_SurfaceObject *SO, float tol)
{
   static char FuncName[]={"SUMA_SetSphereParams"};
   double cent[3], centmed[3], RAD, RAD0, RAD1, rad;
   int i, i3;
   double r[3], ra=0.0;
   char *cc=NULL;
   SUMA_GEOM_TYPE isSphere = SUMA_GEOM_NOT_SET;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
     
   isSphere = SUMA_GEOM_NOT_SET;
   if (tol < 0.0) tol = 0.2;
   
   SUMA_LHv("Setting spheriosity for %s (EmbedDim=%d)\n", 
      SO->Label ? SO->Label:"NULL", SO->EmbedDim);
   
   /* Can't rely on SO->EmbedDim for this function. 
      It is not set when you get here, and you could 
      get the default of 3, but check it anyway
   */
   if (SO->EmbedDim > 0 && SO->EmbedDim < 3) SUMA_RETURN(SUMA_GEOM_IRREGULAR);
   
   /* has this been determined ? */
   if (SUMA_IS_GEOM_SYMM(SO->isSphere)) {
      SUMA_LHv("%s is labeled as %d already\n", SO->Label, SO->isSphere);
      isSphere = SO->isSphere;
   }
   
   if (isSphere == SUMA_GEOM_NOT_SET) {  /* try to guess from name */
      SUMA_LHv("Trying to guess from name %s\n", SO->Name.FileName);
      switch (SO->FileType) {
         case SUMA_INVENTOR_GENERIC:
            break;
         case SUMA_FREE_SURFER:
         case SUMA_FREE_SURFER_PATCH:
            if (  SUMA_iswordin_ci (SO->Name.FileName, "sphere.reg") == 1 ||
                  SUMA_iswordin (SO->Name.FileName, "sphere") == 1   ) {
               isSphere = SUMA_GEOM_SPHERE;
            } 
            break;
         case SUMA_BRAIN_VOYAGER:
             break;
         case SUMA_BYU:
             break;
         case SUMA_SUREFIT:
            if (SUMA_iswordin_ci (SO->Name_coord.FileName, "sphere") == 1 ) {
               isSphere = SUMA_GEOM_SPHERE;
            } 
            break;
         case SUMA_VEC:
            break;
         case SUMA_FT_NOT_SPECIFIED:
         case SUMA_CMAP_SO:
         case SUMA_N_SO_FILE_TYPE:
         case SUMA_FT_ERROR:
            break;
         case SUMA_OPENDX_MESH:
         case SUMA_OBJ_MESH:
         case SUMA_MNI_OBJ:
         case SUMA_STL:
         case SUMA_PLY:
            break;
         case SUMA_PREDEFINED:
            if (  !strncmp(SO->Name.FileName, "ld",2) ) {
               isSphere = SUMA_GEOM_SPHERE;
            } 
            break;
         case SUMA_GIFTI:
            if (SO->aSO) {
               if ((cc = SUMA_NI_AttrOfNamedElement( SO->aSO, 
                                                "Node_XYZ", "GeometricType"))) {
                  if (SUMA_iswordsame_ci(cc,"spherical")) {
                     isSphere = SUMA_GEOM_SPHERE;
                  }
               } else {
                  if (  SUMA_iswordin_ci (SO->Name.FileName, "sphere.reg") == 1 
                      ||SUMA_iswordin (SO->Name.FileName, "sphere") == 1   ) {
                     isSphere = SUMA_GEOM_SPHERE;
                  } 
               }
            } else {
               if (  SUMA_iswordin_ci (SO->Name.FileName, "sphere.reg") == 1 
                      ||SUMA_iswordin (SO->Name.FileName, "sphere") == 1   ) {
                     isSphere = SUMA_GEOM_SPHERE;
               } 
            }
            break; 
     } 
      
      /* the quick way, make sure bounding box is not that of a flat surface*/
      SUMA_LH("Trying to guess from aspect ratio");
      if (!SUMA_isSODimInitialized(SO)) { 
         if (!SUMA_SetSODims(SO)) {
            SUMA_S_Err("Failed to set dims!");
            SUMA_RETURN(NOPE);
         }
      }else {
         SUMA_LH("SODim initialized already");
      }
      ra = 0.0;
      for (i=0;i<3;++i) { r[i] = SO->MaxDims[i]-SO->MinDims[i]; ra += r[i]; }
      ra /= 3.0;
      
      if (  r[0] < 0.001 || r[1] < 0.001 || r[2] < 0.001 ||
            r[0]/ra < 0.8 || r[1]/ra < 0.8 || r[2]/ra < 0.8) {
         SUMA_LHv("too distorted bounding box dimensions=[%f %f %f]\n", 
                  r[0], r[1], r[2]);
         isSphere  = SUMA_GEOM_IRREGULAR;
      }
   }

   
    
   if (  isSphere ==  SUMA_GEOM_NOT_SET || 
         (SUMA_IS_GEOM_SYMM(isSphere) && SO->SphereRadius < 0.0) ) {  
                           /* need to figure out the hard way
                              or need to fill up params */
      if (isSphere ==  SUMA_GEOM_NOT_SET) { 
         SUMA_LHv("The hard way (tol = %f)\n", tol);
      } else {
         SUMA_LHv("Need to set radius and center. tol = %f\n", tol);
      }
      /* the hard way */
      if (!SUMA_GetCenterOfSphereSurface(SO, 500, cent, centmed)) {
         SUMA_S_Warn("Failed to guess at spheriosity.");
         SUMA_RETURN(NOPE);
      }
      /* is this center inside the bounding box or are we fitting
      spheres to flat surfaces */ 
      if (  centmed[0] <= SO->MinDims[0] || centmed[0] >= SO->MaxDims[0] ||
            centmed[1] <= SO->MinDims[1] || centmed[1] >= SO->MaxDims[1] ||
            centmed[2] <= SO->MinDims[2] || centmed[2] >= SO->MaxDims[2] ) {
         SUMA_LH("Failed center test, outside boundig box. Flat surface?\n");
            isSphere  = SUMA_GEOM_IRREGULAR;
            goto DONE;
      }
      /* have center, verify that all nodes are within 1/1000 of Rad */
      SUMA_LHv("Have a center of [%f   %f   %f] for %s\n", 
               centmed[0] , centmed[1], centmed[2] , 
               SUMA_CHECK_NULL_STR(SO->Label)) ;
      RAD = sqrt( SUMA_POW2( SO->NodeList[0] - centmed[0] ) +
                  SUMA_POW2( SO->NodeList[1] - centmed[1] ) +
                  SUMA_POW2( SO->NodeList[2] - centmed[2] ) );
      RAD0 = RAD * (100.0-tol)/100.0;
      RAD1 = RAD * (100.0+tol)/100.0;
      isSphere = SUMA_GEOM_SPHERE;
      for (i=1; i<SO->N_Node && isSphere == SUMA_GEOM_SPHERE; ++i) {
         i3 = 3*i;
         rad = sqrt( SUMA_POW2( SO->NodeList[i3  ] - centmed[0] ) +
                     SUMA_POW2( SO->NodeList[i3+1] - centmed[1] ) +
                     SUMA_POW2( SO->NodeList[i3+2] - centmed[2] ) );   
         if (rad < RAD0 || rad > RAD1) {
            /* no cigar */
            SUMA_LHv("Failed radius test:\n"
                     "Rad range [%f  %f]\n"
                     "Rad           %f\n", RAD0, RAD1, rad);
            isSphere  = SUMA_GEOM_IRREGULAR;
         }  
      }
      if (isSphere == SUMA_GEOM_SPHERE) {
         SO->SphereRadius = RAD;
         SO->SphereCenter[0] = centmed[0];
         SO->SphereCenter[1] = centmed[1];
         SO->SphereCenter[2] = centmed[2];
         SUMA_LHv("A sphere it is: \n"
                  "r = %f\n"
                  "c = [%f   %f   %f]\n", SO->SphereRadius,
                  SO->SphereCenter[0], SO->SphereCenter[1], SO->SphereCenter[2]);
      }else{
         SUMA_LH("A sphere it is NOT, assuming it is irregular");
      }
   } else {
      SUMA_LHv("isSphere is %d, "
               "SUMA_IS_GEOM_SYMM(isSphere) = %d, SO->SphereRadius=%f\n",
               isSphere , SUMA_IS_GEOM_SYMM(isSphere), SO->SphereRadius);
   }  
   
   DONE:
   SO->isSphere = isSphere;
   SUMA_RETURN (YUP);
}

/*---------------------------------------------------------------------------
 * SUMA_spec_select_surfs	  - restrict spec results to given names
 *								[rickr]
 * for each name in list
 *   - verify that it is in the spec file
 *   - verify that it is unique in the spec file
 * restrict the spec contents to the given name list
 *
 * return:
 *   (-1) on failure
 *   new N_Surfs on success
 *---------------------------------------------------------------------------
*/
int SUMA_spec_select_surfs( SUMA_SurfSpecFile * spec, char ** names, int nnames,
		       int debug )
{
    static char FuncName[]={"SUMA_spec_select_surfs"};
    char * nfile;
    int    name, surf, name_ind;
    
    SUMA_ENTRY;
    
    if ( ! spec || ! names )
    {
	fprintf(stderr,"** SUMA_spec_select_surfs: invalid params (%p,%p)\n",
		spec, names);
	SUMA_RETURN( -1 );
    }

    if ( debug > 1 )
	fprintf(stderr, "-- select surfs: searching %d names...\n", nnames);

    if ( nnames <= 0 )
	SUMA_RETURN( 0 );

    /* first, check for existence and uniquenes in list */
    for ( name = 0; name < nnames; name++ )
    {
	if ( ! names[name] )	/* then end the process */
	{
	    nnames = name;
	    break;
	}

	name_ind = SUMA_unique_name_ind(spec, names[name]);

	if ( name_ind < 0 )
	{
	    if ( name_ind == -1 )
		fprintf(stderr,"** surface name '%s' not found\n",names[name]);
	    SUMA_RETURN( -1 );
	}

	if ( debug > 1 )
	    fprintf(stderr, "-- select surfs: found name '%s'\n", names[name]);

	if ( name_ind != name )
	    SUMA_swap_spec_entries(spec, name, name_ind, debug);
    }

    /* now set N_Surfs and N_Groups */
    spec->N_Surfs = nnames;

    if ( debug > 1 )
	fprintf(stderr, "-- select surfs: returning %d names\n", nnames);

    SUMA_RETURN( nnames );
}

/*---------------------------------------------------------------------------
 * SUMA_spec_set_map_refs	  - set *all* mapping refs to SAME
 * 							[rickr]
 *---------------------------------------------------------------------------
*/
int SUMA_spec_set_map_refs( SUMA_SurfSpecFile * spec, int debug )
{
    int sc;

    for (sc = 0; sc < spec->N_Surfs; sc++ )
    {
	if ( ! strstr(spec->MappingRef[sc],"SAME") )
	{
	    if ( debug > 0 )
		fprintf(stderr,"-- map ref: replace '%s' with '%s'\n",
			spec->MappingRef[sc], "./SAME");
	    strcpy(spec->MappingRef[sc], "./SAME");
	}
	else if ( debug > 2 )
	    fprintf(stderr,"-- mr: have good map ref '%s'\n",
		    spec->MappingRef[sc]);
    }

    return 0;
}

/*---------------------------------------------------------------------------
 * SUMA_swap_spec_entries	  - swap entries for the 2 given indices
 * 								[rickr]
 * return:
 *    0 on success
 *   -1 on failure
 *---------------------------------------------------------------------------
*/
int SUMA_swap_spec_entries( SUMA_SurfSpecFile * spec, int i0, int i1, int debug)
{
    char * cpsave;
    char   cssave[SUMA_MAX_NAME_LENGTH];
    int    isave, c;

    if ( !spec || (i0 < 0) || (i0 >= spec->N_Surfs) ||
	          (i1 < 0) || (i1 >= spec->N_Surfs) )
    {
	fprintf(stderr,"** swap_spec_entries: bad params (%p,%d,%d)\n",
		spec, i0, i1);
	return -1;
    }

    if ( debug > 2 )
	fprintf(stderr,"-- swapping spec entries %d and %d\n", i0, i1);

    cssave[SUMA_MAX_NAME_LENGTH-1] = '\0';		/* to be safe */

    swap_strings(spec->SurfaceType[i0], spec->SurfaceType[i1],
	    cssave, SUMA_MAX_LABEL_LENGTH);
    swap_strings(spec->SurfaceFormat[i0], spec->SurfaceFormat[i1],
	    cssave, SUMA_MAX_LABEL_LENGTH);
    swap_strings(spec->TopoFile[i0], spec->TopoFile[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);
    swap_strings(spec->CoordFile[i0], spec->CoordFile[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);
    swap_strings(spec->MappingRef[i0], spec->MappingRef[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);
    swap_strings(spec->AnatCorrect[i0], spec->AnatCorrect[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);
    swap_strings(spec->Hemisphere[i0], spec->Hemisphere[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);
    swap_strings(spec->DomainGrandParentID[i0], spec->DomainGrandParentID[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);     
    swap_strings(spec->OriginatorID[i0], spec->OriginatorID[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);
    swap_strings(spec->LocalCurvatureParent[i0], spec->LocalCurvatureParent[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);
    swap_strings(spec->LocalDomainParent[i0], spec->LocalDomainParent[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);
    swap_strings(spec->SureFitVolParam[i0], spec->SureFitVolParam[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);
    swap_strings(spec->SurfaceFile[i0], spec->SurfaceFile[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);
    swap_strings(spec->VolParName[i0], spec->VolParName[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);
    
    cpsave           = spec->IDcode[i0];	/* (char *)IDcode */
    spec->IDcode[i0] = spec->IDcode[i1];
    spec->IDcode[i1] = cpsave;

    swap_strings(spec->State[i0], spec->State[i1],
	    cssave, SUMA_MAX_LABEL_LENGTH);
    swap_strings(spec->Group[i0], spec->Group[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);
    swap_strings(spec->SurfaceLabel[i0], spec->SurfaceLabel[i1],
	    cssave, SUMA_MAX_NAME_LENGTH);

    isave              = spec->EmbedDim[i0];
    spec->EmbedDim[i0] = spec->EmbedDim[i1];
    spec->EmbedDim[i1] = isave;

    /* leave N_Surfs, N_States, N_Groups, StateList, SpecFilePath */

    return 0;
}

int SUMA_copy_spec_entries( SUMA_SurfSpecFile * spec0, SUMA_SurfSpecFile *spec1,
                            int i0, int i1, int debug)
{
    static char FuncName[]={"SUMA_copy_spec_entries"};
    if ( !spec0 || !spec1 || 
         (i0 < 0) || (i0 >= spec0->N_Surfs) ||
	      (i1 < 0) || (i1 >= spec1->N_Surfs) )
    {
	fprintf(stderr,"** copy_spec_entries: bad params (%p,%d,%p,%d)\n",
		spec0, i0, spec1, i1);
	return -1;
    }

    if ( debug > 2 )
	fprintf(stderr,"-- copying spec entries from spec0[%d] to spec1[%d]\n", 
                  i0, i1);

    copy_strings(spec0->SurfaceType[i0], spec1->SurfaceType[i1],
	              SUMA_MAX_LABEL_LENGTH);
    copy_strings(spec0->SurfaceFormat[i0], spec1->SurfaceFormat[i1],
	              SUMA_MAX_LABEL_LENGTH);
    copy_strings(spec0->TopoFile[i0], spec1->TopoFile[i1],
	              SUMA_MAX_FP_NAME_LENGTH);
    copy_strings(spec0->CoordFile[i0], spec1->CoordFile[i1],
	              SUMA_MAX_NAME_LENGTH);
    copy_strings(spec0->MappingRef[i0], spec1->MappingRef[i1],
	              SUMA_MAX_FP_NAME_LENGTH);
    copy_strings(spec0->AnatCorrect[i0], spec1->AnatCorrect[i1],
	              SUMA_MAX_LABEL_LENGTH);
    copy_strings(spec0->Hemisphere[i0], spec1->Hemisphere[i1],
	              SUMA_MAX_LABEL_LENGTH);
    copy_strings(spec0->DomainGrandParentID[i0], spec1->DomainGrandParentID[i1],
	              SUMA_MAX_LABEL_LENGTH);     
    copy_strings(spec0->OriginatorID[i0], spec1->OriginatorID[i1],
	              SUMA_MAX_LABEL_LENGTH);
    copy_strings(spec0->LocalCurvatureParent[i0],spec1->LocalCurvatureParent[i1],
	              SUMA_MAX_FP_NAME_LENGTH);
    copy_strings(spec0->LocalDomainParent[i0], spec1->LocalDomainParent[i1],
	              SUMA_MAX_FP_NAME_LENGTH);
    copy_strings(spec0->SureFitVolParam[i0], spec1->SureFitVolParam[i1],
	              SUMA_MAX_FP_NAME_LENGTH);
    copy_strings(spec0->SurfaceFile[i0], spec1->SurfaceFile[i1],
	              SUMA_MAX_FP_NAME_LENGTH);
    copy_strings(spec0->VolParName[i0], spec1->VolParName[i1],
	              SUMA_MAX_FP_NAME_LENGTH);
    
    spec1->IDcode[i1] = spec0->IDcode[i0]; /* always pointer copy for this one */

    copy_strings(spec0->State[i0], spec1->State[i1],
	              SUMA_MAX_LABEL_LENGTH);
    copy_strings(spec0->Group[i0], spec1->Group[i1],
	              SUMA_MAX_LABEL_LENGTH);
     copy_strings(spec0->SurfaceLabel[i0], spec1->SurfaceLabel[i1],
	              SUMA_MAX_LABEL_LENGTH);

    spec1->EmbedDim[i1] = spec0->EmbedDim[i0];

    /* leave N_Surfs, N_States, N_Groups, StateList, SpecFilePath */

    return 0;
}

/*---------------------------------------------------------------------------
 * swap_strings	  		- swap the two strings using the given space
 * 								[rickr]
 * return:
 *   (-1) on failure
 *   new N_Surfs on success
 *---------------------------------------------------------------------------
*/
int swap_strings( char * s0, char * s1, char * space, int len )
{
    if ( ! s0 || ! s1 || ! space || len < 1 )
    {
	fprintf(stderr,"** swap_strings: invalid params (%p,%p,%p,%d)\n",
		s0, s1, space, len);
    }

    s0   [len-1] = '\0';		/* now safe using strcpy */
    s1   [len-1] = '\0';
    space[len-1] = '\0';

    strcpy(space, s0);
    strcpy(s0,    s1);
    strcpy(s1,    space);

    return 0;
}
int copy_strings( char * s0, char * s1, int len )
{
    if ( ! s0 || ! s1 || len < 1 )
    {
	fprintf(stderr,"** copy_strings: invalid params (%p,%p,%d)\n",
		s0, s1, len);
    }

    s0   [len-1] = '\0';		/* now safe using strcpy */
    s1   [len-1] = '\0';

    strcpy(s1,    s0);

    return 0;
}

/*---------------------------------------------------------------------------
 * SUMA_unique_name_ind		  - check that name exists uniquely [rickr]
 *
 * return:
 
 *   -1    on "not found"
 *   -2    on "multiple matches"
 *   -3    on "bad, horrible failure"
 *---------------------------------------------------------------------------
*/
int SUMA_unique_name_ind( SUMA_SurfSpecFile * spec, char * sname )
{
    char * nfile;
    int    surf, index = -1;

    if ( ! spec || ! sname )
    {
	fprintf(stderr,"** unique_name_ind: bad params (%p, %p)\n",spec,sname);
	return -3;
    }

    for ( surf = 0; surf < spec->N_Surfs; surf++ )
    {
	nfile = SUMA_coord_file(spec, surf);

	if ( ! nfile )
	{
	    fprintf(stderr,"** surf %d, no coord file\n", surf);
	    return -3;
	}

	/* we have a match */
	if ( strstr(nfile, sname) )
	{
	    if ( index >= 0 )
	    {
		fprintf(stderr,"** surf name %d, '%s': multiple matches\n"
			"   '%s' and '%s'\n",
			surf, sname, nfile, SUMA_coord_file(spec,index));
		return -2;
	    }

	    index = surf;
	}
    }

    return index;
}

/* Like SUMA_unique_name_ind, but return a pointer copy to the
   coordfile name instead.
   An empty string is retuned in error cases or no match */
char * SUMA_unique_name( SUMA_SurfSpecFile * spec, char * sname )
{
    char * nfile;
    int    surf, index = -1;

    if ( ! spec || ! sname )
    {
	fprintf(stderr,"** unique_name_ind: bad params (%p, %p)\n",spec,sname);
	return ("");
    }

    for ( surf = 0; surf < spec->N_Surfs; surf++ )
    {
	nfile = SUMA_coord_file(spec, surf);

	if ( ! nfile )
	{
	    fprintf(stderr,"** surf %d, no coord file\n", surf);
	    return ("");
	}

	/* we have a match */
	if ( strstr(nfile, sname) )
	{
	    if ( index >= 0 )
	    {
		fprintf(stderr,"** surf name %d, '%s': multiple matches\n"
			"   '%s' and '%s'\n",
			surf, sname, nfile, SUMA_coord_file(spec,index));
		return ("");
	    }

	    index = surf;
	}
    }
    
    if (!(nfile = SUMA_coord_file(spec,index))) return("");
    return (nfile);
}

/*---------------------------------------------------------------------------
 * SUMA_coord_file	  - based on the surf type, return coord file
 *								[rickr]
 * return:
 *   on success, pointer to coord file
 *   on any failure, NULL
 *---------------------------------------------------------------------------
*/
char * SUMA_coord_file( SUMA_SurfSpecFile * spec, int index )
{
    char * rp;

    if ( ! spec || (index < 0) )
    {
	fprintf(stderr,"** coord_file: bad params (%p,%d)\n", spec, index);
        return NULL;
    }

    /* SurfaceType field must match the TypeCodes              13 Nov 2007 */
    switch( SUMA_SurfaceTypeCode((spec->SurfaceType[index])) ){
        case SUMA_FT_NOT_SPECIFIED:
            return NULL;
        case SUMA_VEC:
        case SUMA_SUREFIT:
            return spec->CoordFile[index];
        default: /* FreeSurfer, Ply, etc. */
            return spec->SurfaceFile[index];
    }
}

/*!
   return 1 if ld# surface, par takes #
          2 if rd# surface, par takes #
          3 if template spec, par is useless
          4 if template surface, par is useless
          0 not a prededined surface name
   
   \sa SUMA_IS_LOADABLE_SO_NAME
*/
int SUMA_is_predefined_SO_name(char *name, int *upar,
                               char **pdspec, char **pdsv, char **pdsname) 
{
   static char FuncName[]={"SUMA_is_predefined_SO_name"};
   int ldv = 0, tp = 0, par, StdOK=0, ii=0;
   NI_str_array *nisa=NULL;
   char *template=NULL, *leftover=NULL, *svname=NULL,
         *sname=NULL, *ss=NULL, stmp[64], *specname, *spref=NULL;
   SUMA_SO_SIDE side = SUMA_SIDE_ERROR;
   SUMA_SurfSpecFile spec;
   SUMA_Boolean LocalHead = NOPE;
     
   SUMA_ENTRY;
   
   if (!name) SUMA_RETURN(tp);
   if (  (pdspec && *pdspec) || (pdsv && *pdsv) || (pdsname && *pdsname)) {
      SUMA_S_Err("Must sent pointer to null char * pointer, or just NULL");
      SUMA_RETURN(tp);
   }
   
   if (upar) *upar = -1;
   
   if (!(nisa = SUMA_comp_str_2_NI_str_ar(name, ":"))) SUMA_RETURN(tp);
   
   if (nisa->num == 0) {
      /* this should not happen... */
      SUMA_free_NI_str_array(nisa); SUMA_RETURN(tp);
   }
   
   par = -1; tp = 0; 
   template = NULL; side = SUMA_LR; StdOK = 1;
   leftover = NULL;
   for (ii=0; ii<nisa->num && StdOK; ++ii) {
      if (  (!strncmp(nisa->str[ii],"ld",2) || !strncmp(nisa->str[ii],"rd",2)) &&
            (strlen(nisa->str[ii]) <= 5) ) { /* Look for something like ld60 */
            ldv = (int)atoi(nisa->str[ii]+2);
         if (*nisa->str[ii]=='r' && ldv >= 0 && ldv <=100) {
            par = ldv;
            tp = 2;  
         }
         if (*nisa->str[ii]=='l' && ldv >= 0 && ldv <=1000) {
            par = ldv;
            tp = 1;  
         }
         if (par == -1) { /* not that kind of a block, and 
                             hence not a standard name, leave */
            StdOK = 0;
         }
      } else if ( !strcmp(nisa->str[ii],"MNI_N27") ) {/* A template name */
         template = "suma_MNI_N27";  
         spref = "MNI_N27";   
      } else if (!strcmp(nisa->str[ii],"TT_N27") ) {/* A template name */
         template = "suma_TT_N27";     
         spref = "TT_N27";   
      } else if ( !strcasecmp(nisa->str[ii],"l") ||
                  !strcasecmp(nisa->str[ii],"lh") ||
                  !strcasecmp(nisa->str[ii],"left") ) { /* side */
         side = SUMA_LEFT;
      } else if ( !strcasecmp(nisa->str[ii],"r") ||
                  !strcasecmp(nisa->str[ii],"rh") ||
                  !strcasecmp(nisa->str[ii],"right") ) { /* side */
         side = SUMA_RIGHT;
      } else if ( !strcasecmp(nisa->str[ii],"lr") ||
                  !strcasecmp(nisa->str[ii],"both") ||
                  !strcasecmp(nisa->str[ii],"rl") ) {  /* side */
         side = SUMA_LR;           
      } else if (!leftover) { /* state/surf name, potentially */
         if (nisa->str[ii] && nisa->str[ii][0] != '\0') leftover = nisa->str[ii];
      } else { /* Uncool, get out */
         StdOK = 0;
      }
   }
   
   if (!template && leftover) { /* Not a standard name */
      SUMA_free_NI_str_array(nisa); nisa = NULL;
      if (upar) *upar = -1;
      SUMA_RETURN(0);
   }
   
   if (!template) {
      SUMA_free_NI_str_array(nisa); nisa = NULL;
      if (upar) *upar = par;
      SUMA_LHv("Command line string >>%s<< parsed to:\n"
               "par %d, tp = %d\n",
               name, par, tp);   
      SUMA_RETURN(tp);
   } else { /* now it gets complicated */
      if (par > 0) {
         sprintf(stmp, "std.%d.", par);
      } else {
         stmp[0]='\0'; 
      }
      specname = SUMA_append_replace_string(template,stmp,"/",0);
      switch (side) {
         case SUMA_LEFT:
            specname = SUMA_append_replace_string(specname,"_lh.spec",spref,1);
            break;
         case SUMA_RIGHT:
            specname = SUMA_append_replace_string(specname,"_rh.spec",spref,1);
            break;
         case SUMA_LR:
            specname = SUMA_append_replace_string(specname,"_both.spec",spref,1);
            break;
      }
      ss = find_afni_file(specname, 0, NULL);
      if (ss[0] == '\0') {
         SUMA_S_Errv("Spec file %s not found despite effort\n", specname);
         SUMA_ifree(specname); specname = NULL;
         SUMA_free_NI_str_array(nisa); nisa = NULL;
         if (upar) *upar = -1;   
         SUMA_RETURN(0);
      } else {
         SUMA_free(specname); specname = SUMA_copy_string(ss);
         tp = 3;
      }
   
      svname = SUMA_append_replace_string(template,spref,"/",0);
      svname = SUMA_append_replace_string(svname,"_SurfVol.nii","",1);
      ss = find_afni_file(svname, 0, NULL);
      if (ss[0] == '\0') {/* try one more time */
         svname = SUMA_append_replace_string(svname,".gz","",1);
         ss = find_afni_file(svname, 0, NULL);
      }
      if (ss[0] == '\0') {
         SUMA_S_Warnv("No sv found for %s\n", specname);
         SUMA_ifree(svname);
      } else {
         SUMA_free(svname); svname=SUMA_copy_string(ss);
      }
   }

   /* So now we have a template spec, do we have a request for a particular 
      surface ? */
   if (leftover) {
      /* load the spec, identify the surface and set its name */
      SUMA_AllocSpecFields(&spec);
      if (!SUMA_Read_SpecFile (specname, &spec)) {
         SUMA_S_Errv("Failed to read SpecFile %s\n", specname);
         SUMA_ifree(specname); specname = NULL;
         SUMA_free_NI_str_array(nisa); nisa = NULL;
         if (upar) *upar = -1; 
          SUMA_FreeSpecFields(&spec); 
         SUMA_RETURN(0);
      }
      ss = SUMA_unique_name( &spec, leftover );
      SUMA_LH("dd %s", ss);
      if (ss[0]=='\0') {
         SUMA_S_Errv("Failed to get %s from %s\n", leftover, specname);
         SUMA_ifree(specname); specname = NULL;
         SUMA_free_NI_str_array(nisa); nisa = NULL;
         if (upar) *upar = -1;   
         SUMA_FreeSpecFields(&spec); 
         SUMA_RETURN(0);
      }
      /* OK, now we have a full surface name */
      sname = SUMA_copy_string(ss);
      SUMA_FreeSpecFields(&spec); 
      
      tp = 4;
   }
   
   SUMA_LHv("Command line string >>%s<< parsed to:\n"
            "specname %s, svname %s, sname %s, par %d, side %d, tp %d\n",
            name, specname?specname:"NULL", svname?svname:"NULL",
            sname?sname:"NULL", par, side, tp);
   
   if (pdspec) *pdspec = specname;
   else SUMA_ifree(specname);
   if (pdsv) *pdsv = svname;
   else SUMA_ifree(svname);
   if (pdsname) *pdsname = sname;
   else SUMA_ifree(sname); 
           
   SUMA_RETURN(tp);
}

#define SUMA_CHECK_INPUT_SURF(name, topo, ok) {  \
   ok = 0;  \
   if (SUMA_is_predefined_SO_name(name, NULL, NULL, NULL, NULL)) {\
      ok = 1; \
   } else if (SUMA_filexists(name)) {   \
      if  (!(topo)) { ok = 1; }   \
      else { if (SUMA_filexists(topo)) {ok = 1;}  } \
   } \
   if (!ok) {  \
      if (topo) { \
         fprintf(SUMA_STDERR,"Error %s:\nCould not locate surface %s %s\n", FuncName, name, topo); \
      } else { \
         fprintf(SUMA_STDERR,"Error %s:\nCould not locate surface %s \n", FuncName, name); \
      }  \
   }  \
}

#define SUMA_BLANK_NEW_SPEC_SURF(spec)  {\
         spec->IDcode[spec->N_Surfs]= NULL;  \
         spec->SurfaceLabel[spec->N_Surfs][0] = '\0'; \
         spec->EmbedDim[spec->N_Surfs] = 3;  \
         strcpy(spec->AnatCorrect[spec->N_Surfs], "Y");  \
         spec->Hemisphere[spec->N_Surfs][0] = '\0';   \
         spec->DomainGrandParentID[spec->N_Surfs][0] = '\0';   \
         strcpy(spec->LocalDomainParent[spec->N_Surfs],"SAME");  \
         spec->LocalCurvatureParent[spec->N_Surfs][0] = '\0';  \
         spec->OriginatorID[spec->N_Surfs][0] = '\0'; \
}

void SUMA_Show_IO_args(SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_Show_IO_args"};
   int i;
   
   SUMA_ENTRY;
   
   if (!ps) { fprintf(SUMA_STDERR,"NULL ps\n"); SUMA_RETURNe; }
   
   fprintf(SUMA_STDERR,"%saccepting -t* options\n",
      ps->accept_t ? "":"not");
   fprintf(SUMA_STDERR,"%saccepting -surf_* options\n",
      ps->accept_s ? "":"not");
   fprintf(SUMA_STDERR,"%saccepting -i_* options\n",
      ps->accept_i ? "":"not");
   fprintf(SUMA_STDERR,"%saccepting -ipar_* options\n",
      ps->accept_ipar ? "":"not");
   fprintf(SUMA_STDERR,"%saccepting -o_* options\n",
      ps->accept_o ? "":"not");
   fprintf(SUMA_STDERR,"%saccepting -spec option\n",
      ps->accept_spec ? "":"not");
   fprintf(SUMA_STDERR,"%saccepting -sv option\n",
      ps->accept_sv ? "":"not");
   fprintf(SUMA_STDERR,"%saccepting -talk_suma options\n",
      ps->accept_talk_suma ? "":"not");
   fprintf(SUMA_STDERR,"%saccepting -mask options\n",
      ps->accept_mask ? "":"not");
   fprintf(SUMA_STDERR,"%saccepting -cmap options\n",
      ps->accept_cmap ? "":"not");
   fprintf(SUMA_STDERR,"%saccepting -dset options\n",
      ps->accept_dset ? "":"not");
   fprintf(SUMA_STDERR,
            "Check for input surface files: %d\n", ps->check_input_surf);
   fprintf(SUMA_STDERR,"%d sv:\n", ps->N_sv);
   if (ps->N_sv) {
      for (i=0; i<ps->N_sv; ++i) {
         fprintf(SUMA_STDERR,"   %d: %s\n", i, ps->sv[i]);
      }
   }
   fprintf(SUMA_STDERR,"%d vp:\n", ps->N_vp);
   if (ps->N_vp) {
      for (i=0; i<ps->N_vp; ++i) {   
         fprintf(SUMA_STDERR,"   %d: %s\n", i, ps->vp[i]);
      }
   }
   fprintf(SUMA_STDERR,"bmaskname: %s\n", CHECK_NULL_STR(ps->bmaskname));
   fprintf(SUMA_STDERR,"nmaskname: %s\n", CHECK_NULL_STR(ps->nmaskname));
   fprintf(SUMA_STDERR,"cmask: %s\n", CHECK_NULL_STR(ps->cmask));
   
   fprintf(SUMA_STDERR,"%d spec names:\n", ps->N_spec_names);
   for (i=0; i<ps->N_spec_names; ++i) {
      fprintf(SUMA_STDERR,"   %d: %s\n", i, ps->spec_names[i]);
   }
   fprintf(SUMA_STDERR,"%d s_surfnames from %s\n",
                ps->s_N_surfnames, ps->spec_names[0]);
   for (i=0; i<ps->s_N_surfnames; ++i) {
      fprintf(SUMA_STDERR,"   %d: %s\n", i, ps->s_surfnames[i]);
   }
   fprintf(SUMA_STDERR,"%d i_surfnames\n", ps->i_N_surfnames);
   for (i=0; i<ps->i_N_surfnames; ++i) {
      if (ps->i_surftopo[i]) 
         fprintf(SUMA_STDERR,
                  "   %d: %s %s %s %s\n", 
                  i, ps->i_group[i], ps->i_state[i], 
                  ps->i_surfnames[i], ps->i_surftopo[i]);
      else fprintf(SUMA_STDERR,
                  "   %d: %s %s %s\n", 
                  i, ps->i_group[i], ps->i_state[i],ps->i_surfnames[i]);
   }
   fprintf(SUMA_STDERR,"%d ipar_surfnames\n", ps->ipar_N_surfnames);
   for (i=0; i<ps->ipar_N_surfnames; ++i) {
      if (ps->ipar_surftopo[i]) 
         fprintf(SUMA_STDERR,
                  "   %d: %s %s %s %s\n", 
                  i, ps->ipar_group[i], ps->ipar_state[i], 
                  ps->ipar_surfnames[i], ps->ipar_surftopo[i]);
      else fprintf(SUMA_STDERR,
                   "   %d: %s %s %s\n", 
                   i, ps->ipar_group[i], ps->ipar_state[i], 
                   ps->ipar_surfnames[i]);
   }
   fprintf(SUMA_STDERR,"%d o_surfnames\n", ps->o_N_surfnames);
   for (i=0; i<ps->o_N_surfnames; ++i) {
      if (ps->o_surftopo[i]) 
         fprintf(SUMA_STDERR,
                  "   %d: %s %s %s %s\n", 
                  i, ps->o_group[i], ps->o_state[i], 
                  ps->o_surfnames[i], ps->o_surftopo[i]);
      else fprintf(SUMA_STDERR,
                  "   %d: %s %s %s\n", 
                  i, ps->o_group[i], ps->o_state[i], ps->o_surfnames[i]);
   }
   fprintf(SUMA_STDERR,"%d t_surfnames\n", ps->t_N_surfnames);
   for (i=0; i<ps->t_N_surfnames; ++i) {
      if (ps->t_surftopo[i]) 
         fprintf(SUMA_STDERR,
                  "   %d: %s %s %s %s\n", 
                  i, ps->o_group[i], ps->t_state[i], 
                  ps->t_surfnames[i], ps->t_surftopo[i]);
      else fprintf(SUMA_STDERR,
                  "   %d: %s %s %s\n", 
                  i, ps->o_group[i], ps->t_state[i], ps->t_surfnames[i]);
   }
   
   if (ps->accept_talk_suma) {
      if (!ps->cs) {
         fprintf(SUMA_STDERR,"No Talking to SUMA requested\n");
      } else {
         fprintf(SUMA_STDERR,"Talking to SUMA requested.\n");
      }
   } 
   fprintf( SUMA_STDERR,
            "%d arguments on command line (+checked, -not checked):\n"
            , ps->N_args);
   for (i=0; i<ps->N_args; ++i) {
      if (ps->arg_checked[i]) 
         fprintf(SUMA_STDERR,
                  " %d+   ",i);
      else fprintf(SUMA_STDERR," %d-   ",i);
   }
   fprintf(SUMA_STDERR,"\n");
      
   SUMA_RETURNe;
}

/*!
   Take a vector of independent surfaces and get a Spec file that goes
   with them
*/
SUMA_SurfSpecFile *SUMA_SOGroup_2_Spec(SUMA_SurfaceObject **SOv, int N_SOv)
{
   static char FuncName[]={"SUMA_SOGroup_2_Spec"};
   SUMA_SurfSpecFile *spec = NULL;
   int i, nspec;
   char si[100];
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ps = SUMA_CreateGenericArgParse("-i;");
   ps->check_input_surf = 0;
   ps->i_N_surfnames = N_SOv;
   for (i=0; i<ps->i_N_surfnames; ++i) {
      sprintf(si, "s_%d\n", i);
      if (SOv[i]->Label) ps->i_surfnames[i] = SUMA_copy_string(SOv[i]->Label); 
      else ps->i_surfnames[i] =  SUMA_copy_string(si);
      if (SOv[i]->State) ps->i_state[i] = SUMA_copy_string(SOv[i]->State);
      if (SOv[i]->Group) ps->i_group[i] = SUMA_copy_string(SOv[i]->Group);
      ps->i_FT[i] = SUMA_FT_NOT_SPECIFIED; ps->i_FF[i] = SUMA_FF_NOT_SPECIFIED; 
   }
   
   spec = SUMA_IO_args_2_spec(ps, &nspec);
   if (nspec != 1) {
      SUMA_S_Err( "Expecting one spec struct here!\n"
                  "Trouble might befall you ahead.");
   }
   SUMA_FreeGenericArgParse(ps); ps = NULL;

   SUMA_RETURN(spec);  
}

SUMA_SurfSpecFile *SUMA_IO_args_2_spec(SUMA_GENERIC_ARGV_PARSE *ps, int *nspec)
{
   static char FuncName[]={"SUMA_IO_args_2_spec"};
   int i=0, ispec0;
   byte ok;
   char sbuf[SUMA_MAX_LABEL_LENGTH+1];
   static char defgroup[]={SUMA_DEF_GROUP_NAME};
   SUMA_SurfSpecFile *spec = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   /* first look for virtual spec */
   *nspec = 1;
   spec = (SUMA_SurfSpecFile *)SUMA_malloc(sizeof(SUMA_SurfSpecFile));
   /* initialize the spec ZSS Jan 9 06*/
   if (!SUMA_AllocSpecFields(spec)) {
      SUMA_S_Err("Failed to initialize spec\n" );
      SUMA_RETURN(NULL);
   }

   spec->N_Surfs = 0;
   spec->N_States = 0;
   spec->N_DO = 0;
   spec->N_Groups = 1;
   strcpy(spec->SpecFilePath, "./");
   strcpy(spec->SpecFileName, "FromCommandLine.spec");   
   SUMA_LHv("Accept_do: %d, %d DOs\n", ps->accept_do, ps->N_DO);
   if (ps->accept_do) {
      SUMA_LH("Processing -tract/-graph/-vol");
      if (ps->N_DO+spec->N_DO >= SUMA_MAX_N_DO_SPEC) {
         SUMA_S_Err("Too many DOs to work with.\n"); 
         *nspec = 0; SUMA_RETURN(spec); 
      }
      for (i=0; i<ps->N_DO; ++i) {
         spec->DO_type[spec->N_DO] = ps->DO_type[i];
         strcpy(spec->DO_name[spec->N_DO], ps->DO_name[i]);
         ++spec->N_DO;
      }
   }
   
   if (ps->accept_i) {
      SUMA_LH("Processing -i");
      if (ps->i_N_surfnames+spec->N_Surfs >= SUMA_MAX_N_SURFACE_SPEC) { 
         SUMA_S_Err("Too many surfaces to work with.\n"); 
         *nspec = 0; SUMA_RETURN(spec); 
      }
      for (i=0; i<ps->i_N_surfnames; ++i) {
         if (ps->check_input_surf) { 
            SUMA_CHECK_INPUT_SURF(ps->i_surfnames[i], ps->i_surftopo[i], ok);
            if (!ok) { 
               SUMA_free(spec); spec = NULL; *nspec = 0; SUMA_RETURN(spec); 
            }
         }
         strcpy(  spec->SurfaceType[spec->N_Surfs], 
                  SUMA_SurfaceTypeString (ps->i_FT[i]));
         if (  ps->i_FF[i] == SUMA_BINARY || 
               ps->i_FF[i] == SUMA_BINARY_LE || 
               ps->i_FF[i] == SUMA_BINARY_BE) 
            strcpy(spec->SurfaceFormat[spec->N_Surfs], "BINARY");
         else if (ps->i_FF[i] == SUMA_XML_SURF ||
                  ps->i_FF[i] == SUMA_XML_ASCII_SURF ||
                  ps->i_FF[i] == SUMA_XML_B64_SURF ||
                  ps->i_FF[i] == SUMA_XML_B64GZ_SURF ) 
               strcpy(spec->SurfaceFormat[spec->N_Surfs], "XML");
         else strcpy(spec->SurfaceFormat[spec->N_Surfs], "ASCII");
         if (ps->i_FT[i] == SUMA_SUREFIT || ps->i_FT[i] == SUMA_VEC) {
            strcpy(spec->TopoFile[spec->N_Surfs], ps->i_surftopo[i]);
            strcpy(spec->CoordFile[spec->N_Surfs], ps->i_surfnames[i]);
            if (ps->vp[i]) 
               strcpy(spec->SureFitVolParam[spec->N_Surfs], ps->vp[i]);
         } else {
            strcpy(spec->SurfaceFile[spec->N_Surfs], ps->i_surfnames[i]);
         }
         if (ps->sv[i]) 
            strcpy(spec->VolParName[spec->N_Surfs], ps->sv[i]); 
         else spec->VolParName[spec->N_Surfs][0] = '\0';
         if (ps->i_state[i])  { 
            strcpy(spec->State[spec->N_Surfs], ps->i_state[i]); 
            ++spec->N_States;
         } else { 
            if (!ps->onestate) {
               sprintf(spec->State[spec->N_Surfs], "iS_%d", spec->N_States); 
               ++spec->N_States; 
            } else {
               sprintf(spec->State[spec->N_Surfs], "iS");
               spec->N_States = 1;
            }  
         }
         if (ps->anatomical == 1) sprintf(spec->AnatCorrect[spec->N_Surfs],"Y");
         else if (ps->anatomical == -1) 
            sprintf(spec->AnatCorrect[spec->N_Surfs],"N");
         if (ps->i_group[i])  { 
            strcpy(spec->Group[spec->N_Surfs], ps->i_group[i]); 
         } else { strcpy(spec->Group[spec->N_Surfs], defgroup);  }
         SUMA_BLANK_NEW_SPEC_SURF(spec);
         ++spec->N_Surfs;
      }
   }
   
   if (ps->accept_ipar) {
      SUMA_LH("Processing -ipar");
      if (ps->ipar_N_surfnames+spec->N_Surfs >= SUMA_MAX_N_SURFACE_SPEC) { 
         SUMA_S_Err("Too many surfaces to work with.\n"); 
         *nspec = 0; 
         SUMA_RETURN(spec); 
      }
      for (i=0; i<ps->ipar_N_surfnames; ++i) {
         if (ps->check_input_surf) { 
            SUMA_CHECK_INPUT_SURF(  ps->ipar_surfnames[i], 
                                    ps->ipar_surftopo[i], ok);
            if (!ok) { 
               SUMA_free(spec); spec = NULL; *nspec = 0; SUMA_RETURN(spec); 
            }
         }
         strcpy(  spec->SurfaceType[spec->N_Surfs], 
                  SUMA_SurfaceTypeString (ps->ipar_FT[i]));
         if (   ps->ipar_FF[i] == SUMA_BINARY || ps->ipar_FF[i] == SUMA_BINARY_LE
             || ps->ipar_FF[i] == SUMA_BINARY_BE) 
            strcpy(spec->SurfaceFormat[spec->N_Surfs], "BINARY");
         else if (ps->ipar_FF[i] == SUMA_XML_SURF || 
                  ps->ipar_FF[i] == SUMA_XML_ASCII_SURF ||
                  ps->ipar_FF[i] == SUMA_XML_B64_SURF ||
                  ps->ipar_FF[i] == SUMA_XML_B64GZ_SURF ) 
               strcpy(spec->SurfaceFormat[spec->N_Surfs], "XML");
         else strcpy(spec->SurfaceFormat[spec->N_Surfs], "ASCII");
         if (ps->ipar_FT[i] == SUMA_SUREFIT || ps->ipar_FT[i] == SUMA_VEC) {
            strcpy(spec->TopoFile[spec->N_Surfs], ps->ipar_surftopo[i]);
            strcpy(spec->CoordFile[spec->N_Surfs], ps->ipar_surfnames[i]);
            if (ps->vp[i]) 
               strcpy(spec->SureFitVolParam[spec->N_Surfs], ps->vp[i]);
         } else {
            strcpy(spec->SurfaceFile[spec->N_Surfs], ps->ipar_surfnames[i]);
         }
         if (ps->sv[i]) 
            strcpy(spec->VolParName[spec->N_Surfs], ps->sv[i]); 
         else spec->VolParName[spec->N_Surfs][0] = '\0';
         if (ps->ipar_state[i])  { 
            strcpy(spec->State[spec->N_Surfs], ps->ipar_state[i]); 
            ++spec->N_States;
         } else { 
            sprintf(spec->State[spec->N_Surfs], "iS_%d", spec->N_States); 
            ++spec->N_States; 
         }
         if (ps->ipar_group[i])  { 
            strcpy(spec->Group[spec->N_Surfs], ps->ipar_group[i]); 
         } else { strcpy(spec->Group[spec->N_Surfs], defgroup);  }
         SUMA_BLANK_NEW_SPEC_SURF(spec);
         ++spec->N_Surfs;
      }
   }
   
   if (ps->accept_t) {
      SUMA_LH("Processing -t");
      if (ps->t_N_surfnames+spec->N_Surfs >= SUMA_MAX_N_SURFACE_SPEC) { 
         SUMA_S_Err("Too many surfaces to work with.\n"); 
         *nspec = 0; 
         SUMA_RETURN(spec); 
      }
      for (i=0; i<ps->t_N_surfnames; ++i) {  
         if (ps->check_input_surf) { 
            SUMA_CHECK_INPUT_SURF(ps->t_surfnames[i], ps->t_surftopo[i], ok);
            if (!ok) { 
               SUMA_free(spec); spec = NULL; *nspec = 0; 
               SUMA_RETURN(spec); 
            }
         }
         strcpy(  spec->SurfaceType[spec->N_Surfs], 
                  SUMA_SurfaceTypeString (ps->t_FT[i]));
         if (  ps->t_FF[i] == SUMA_BINARY || ps->t_FF[i] == SUMA_BINARY_LE 
            || ps->t_FF[i] == SUMA_BINARY_BE) 
               strcpy(spec->SurfaceFormat[spec->N_Surfs], "BINARY");
         else if (ps->t_FF[i] == SUMA_XML_SURF || 
                  ps->t_FF[i] == SUMA_XML_ASCII_SURF ||
                  ps->t_FF[i] == SUMA_XML_B64_SURF ||
                  ps->t_FF[i] == SUMA_XML_B64GZ_SURF )
               strcpy(spec->SurfaceFormat[spec->N_Surfs], "XML");
         else strcpy(spec->SurfaceFormat[spec->N_Surfs], "ASCII");
         if (ps->t_FT[i] == SUMA_SUREFIT || ps->t_FT[i] == SUMA_VEC) {
            strcpy(spec->TopoFile[spec->N_Surfs], ps->t_surftopo[i]);
            strcpy(spec->CoordFile[spec->N_Surfs], ps->t_surfnames[i]);
            if (ps->vp[i]) 
               strcpy(spec->SureFitVolParam[spec->N_Surfs], ps->vp[i]);
         } else {
            strcpy(spec->SurfaceFile[spec->N_Surfs], ps->t_surfnames[i]);
         }
         if (ps->sv[i]) strcpy(spec->VolParName[spec->N_Surfs], ps->sv[i]); 
         else spec->VolParName[spec->N_Surfs][0] = '\0';
         if (ps->t_state[i])  { 
            strcpy(spec->State[spec->N_Surfs], ps->t_state[i]); 
            ++spec->N_States;
         } else { 
            sprintf(spec->State[spec->N_Surfs], "iS_%d", spec->N_States); 
            ++spec->N_States; 
         }
         if (ps->anatomical == 1) sprintf(spec->AnatCorrect[spec->N_Surfs],"Y");
         else if (ps->anatomical == -1) 
            sprintf(spec->AnatCorrect[spec->N_Surfs],"N");
         if (ps->t_group[i])  { 
            strcpy(spec->Group[spec->N_Surfs], ps->t_group[i]); 
         } else { strcpy(spec->Group[spec->N_Surfs], defgroup);  }
         SUMA_BLANK_NEW_SPEC_SURF(spec);
         ++spec->N_Surfs;
      }
   }
   
   SUMA_LH("Working States");
   
   /* now create the states list */
   if (spec->N_Surfs || spec->N_DO) {
      if (spec->N_Surfs) {
         spec->N_States = 1;
         sprintf(spec->StateList, "%s|", spec->State[0]);
         for (i=1; i<spec->N_Surfs; ++i) {
            sprintf(sbuf,"%s|",spec->State[i]); 
            if (!SUMA_iswordin(spec->StateList, sbuf)) { 
               sprintf(spec->StateList, "%s|", spec->State[i]); 
               ++spec->N_States; 
            }
         }
         if (LocalHead) 
            fprintf( SUMA_STDERR,"%s:\n%d distinct states\n%s\n", 
                     FuncName, spec->N_States, spec->StateList);
      }
      
      if (spec->N_DO) {
         spec->N_States = 1;
         sprintf(spec->StateList, "ANY|");
      }
       
      if (spec->Group[0][0] == '\0') {
         /* Perhaps only DOs loaded, label the group */
         strcpy(spec->Group[0], "ANY");
      }

      ispec0 = *nspec;
   } else {
      if (LocalHead) fprintf(SUMA_STDERR,"%s:\n no surfs\n", FuncName);
      /* free Spec */
      {  
         int k=0; 
         for (k=0; k<*nspec; ++k) {
            if (!SUMA_FreeSpecFields(&(spec[k]))) { 
               SUMA_S_Err("Failed to free spec fields"); 
            } 
         }
      }
      SUMA_free(spec); spec = NULL; *nspec = 0; ispec0 = 0;
   }
    
   /* Now see if you have explicity define specs on command line */
   if (ps->accept_spec || ps->accept_s) {
      SUMA_LHv("Working Specs, %d %d\n"
               "ispec0 = %d, ps->N_spec_names = %d\n"
               "i=%d\n", 
               ps->accept_spec, ps->accept_s,
               ispec0, ps->N_spec_names,
               i);
      if (ps->N_spec_names) {
         *nspec = ispec0 + ps->N_spec_names;
         spec = (SUMA_SurfSpecFile *)
                  SUMA_realloc(  spec, 
                                 *nspec * sizeof(SUMA_SurfSpecFile));
         SUMA_LH("Here");
         for (i=0; i<ps->N_spec_names; ++i) {   
            if (!SUMA_AllocSpecFields(&(spec[i+ispec0]))) { 
               SUMA_S_Err("Failed to init spec fields"); 
            }
            if (!SUMA_Read_SpecFile (ps->spec_names[i], &(spec[i+ispec0]))) {
               SUMA_SL_Err("Failed to read SpecFile");
               {  
                  int k=0; 
                  for (k=0; k<*nspec; ++k) {
                     if (!SUMA_FreeSpecFields(&(spec[k]))) { 
                        SUMA_S_Err("Failed to free spec fields"); 
                     } 
                  }
               }
               SUMA_free(spec); spec = NULL; *nspec = 0; 
               SUMA_RETURN(spec);
            }
         }
         /* do we have a set of surfaces to read here ? 
            only works with one spec  */
         SUMA_LHv("Have %d surf_\n",ps->s_N_surfnames);
         if (ps->s_N_surfnames) {
            int n_read;
            if (ps->N_spec_names > 1) {
               SUMA_S_Err( "Cannot deal with multiple -spec on \n"
                           "command line combined with -surf_ selectors.");
               {  
                  int k=0; 
                  for (k=0; k<*nspec; ++k) {
                     if (!SUMA_FreeSpecFields(&(spec[k]))) { 
                        SUMA_S_Err("Failed to free spec fields"); 
                     } 
                  }
               }
               SUMA_free(spec); spec = NULL; *nspec = 0; 
               SUMA_RETURN(spec);
            }
            /* purify the spec */
            n_read = SUMA_spec_select_surfs( &(spec[0+ispec0]), 
                                             ps->s_surfnames, 
                                             ps->s_N_surfnames, LocalHead);
            if (n_read < 1) {
               SUMA_S_Err("Failed to find surfaces in spec file");
               SUMA_free(spec); spec = NULL; *nspec = 0; 
               SUMA_RETURN(spec);            
            }
            if (LocalHead) {
               fprintf( SUMA_STDERR,
                        "%s (%s:%d): Selected in %d surfaces\n", 
                        FuncName, __FILE__, __LINE__, n_read);
            }
         }
         
      }
   }
   
   if (LocalHead) { 
      fprintf( SUMA_STDERR,
               "%s: About to return, have %d spec files.\n", FuncName, *nspec);
   }
   SUMA_RETURN(spec);
}

/* 
Current state of loading CIFTI

Load dset, 
Prep domains
Breakup dset into parts and load them individually onto their respective domains
need some sort of unifying 'yoking' marker for dsets, domains, and overlays so that when user touches them it is known that user is dealing with CIFTI object

Yoking can be done much like how left/right yoking in suma is handled, except there should be no guess work involved.

Note that 'what' the user clicked on involve knowing the domain overwhich they clicked in conjunction with the data displayed on that domain. -- do we need this distinction?
*/
SUMA_Boolean SUMA_LoadCIFTIDO (char *fname, 
                        SUMA_DO_CoordUnits coord_type, SUMA_DSET **odset, 
                        int OkAdopt, int SetupOverlay, int LaunchDisplay, 
                        int MakeOverlayCurrent, SUMA_OVERLAYS **used_over)
{
   static char FuncName[]={"SUMA_LoadCIFTIDO"};
   SUMA_CIFTI_SAUX *CSaux=NULL;
   SUMA_DSET *cdset=NULL, *sddset=NULL;
   SUMA_CIFTI_DO *CO = NULL;
   SUMA_ALL_DO *asdo = NULL;
   SUMA_DSET_FORMAT tff = SUMA_NIML;
   DList *list=NULL;
   SUMA_LIST_WIDGET *LW=NULL;
   SUMA_DSET *dsetpre = NULL;
   SUMA_OVERLAYS *NewColPlane = NULL,  *colplanepre = NULL;
   int OverInd=-1, OKdup=-1, loc[2], pre_exist=0, isd=0;
   char *dsetcmap=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!fname) SUMA_RETURN(NOPE);
   if (coord_type != SUMA_NORM_SCREEN_UNIT &&
       coord_type != SUMA_WORLD) coord_type = SUMA_WORLD;
       
   if (!(cdset = SUMA_LoadDset_eng( fname, &tff, 1 ))) {
      SUMA_S_Errv("Failed to open %s\n", fname);
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE);
   }     

   if (!SUMA_isCIFTIDsetNgr(cdset->ngr)) {
      SUMA_S_Err("All is bad that starts bad, or is it?\nNot a CIFTIcle.");
      SUMA_RETURN(NOPE);
   }
      
   /* Swaps ?*/
   if (odset) {
      if (*odset == NULL) {
         *odset = cdset;
      } else {
         /* Replace any existing parts of *odset, with those from cdset */
         if (!SUMA_FreeDsetContent(*odset)) {
            SUMA_S_Err("Failed to free dset content");
            SUMA_RETURN(NOPE);
         }
         (*odset)->ngr = cdset->ngr; cdset->ngr = NULL;
         SUMA_FreeDset(cdset);
         cdset = *odset; 
      }
   } 

   /* Does this dset have a built in colormap?
      If it does, then loadit into SCM */
   if (!SUMA_Insert_Cmap_of_Dset(cdset)) {
      SUMA_S_Err("Failed to insert Cmap");
      SUMA_FreeDset(cdset); cdset = NULL;
      
      SUMA_RETURN(NOPE);
   }


   /* Get domains info from Ngr and create elementary dsets*/
   if (!(SUMA_CIFTI_DomainsFromNgr( cdset, SUMAg_CF->DsetList,
      	             	      	    SUMAg_CF->Allow_Dset_Replace, NULL))) {
      SUMA_S_Err("Failed to get domains from Ngr");
      SUMA_FreeDset(cdset);
      SUMA_RETURN(NOPE);
   }
   
   /* Can we pile onto an existing domain? (think multiple
      datasets over the same surface)                     */
   
   CO = NULL;
   if (OkAdopt && (CO = SUMA_CIFTI_find_matching_domain(cdset, NULL, -1))) {
      SUMA_LH("Adopting CO");
   } else {
      SUMA_LH("Create CO from cdset");
      if (!(CO=SUMA_CIFTI_DO_from_dset(cdset))) {
         SUMA_S_Err("Failed to create DO from dset");
         SUMA_FreeDset(cdset);
         SUMA_RETURN(NOPE);
      }
   }

   SUMA_LH("Do we really need a separate controller for CIFTI?"
      	   "Leave it for now, kill CSaux later if we stick with"
	   "current setup...");
   if (!(CSaux = CDO_CSAUX(CO))) {
      SUMA_S_Warn("That is weird, should this happen?");
      if (!SUMA_AddCIFTISaux(CO)) {
         SUMA_S_Err("Failed to create Saux struct");
         SUMA_RETURN(NOPE);   
      }   
      CSaux = CDO_CSAUX(CO);
   }
   
   /* add the dset to the list SUMAg_CF->DsetList, the elementary datasets were
      added inside SUMA_CIFTI_DomainsFromNgr */
   dsetpre = cdset;
   if (LocalHead) {
      fprintf( SUMA_STDERR,
               "%s: New dset (%s) has pointer %p\n", 
               FuncName, SDSET_LABEL(cdset), cdset); 
   }
   if (!SUMA_InsertDsetPointer(  &cdset, SUMAg_CF->DsetList, 
                                 SUMAg_CF->Allow_Dset_Replace)) {
      SUMA_SLP_Err("Failed to add new dset to list");
      /* is there not a function to replace a dset yet? */
      SUMA_FreeDset(cdset); cdset = NULL;
      SUMA_RETURN(NOPE);
   }
   if (LocalHead) {
      fprintf( SUMA_STDERR,
               "%s: Now dset (%s) is  pointer %p\n", 
               FuncName, SDSET_LABEL(cdset), cdset); 
   }
   
   /* 
   Note that there is currently no solid way
   of attaching a dataset to a displayable object.
   That has not been needed so far. There is a meek 
   attempt to do something with SUMA_SetParent_DsetToLoad()
   but only for surface-based datasets, and ONLY for generating
   an ID that is based on a combo of filename and parent surface.
   
   Should we need to find the DO that defines the domain of 
   a particular CIFTI dataset, we might need to write a function
   to search which CDOM_type object contains an overlay for the 
   dataset in question.
   */
   
   /* There is no one overlay a CIFTI dataset (or should there be a shell 
   overlay at some point that mereley points to each of the subdomains?
   I don't know if that is needed yet. For now I will create a separate
   overlay for each of the elementary datasets */
   if (SetupOverlay) {
      if (cdset != dsetpre) { /* dset was pre-existing in the list */
      	 pre_exist = 1;
      } else pre_exist = 0;
   
      
      SUMA_LH("Setting up overlay for %d CIFTI elmentary datasets",
      	       cdset->Aux->N_doms);
      
      for (isd=0; isd<cdset->Aux->N_doms; ++isd) {
      	 OverInd = -1; 
      	 {
         asdo = SUMA_CIFTI_subdom_ado(CO, isd);
	 if (!(sddset = DSET_FIND(cdset->Aux->doms[isd]->edset_id))) {
	    SUMA_S_Err("Should have found that dset (id %s), "
	               "returning with potential leaks!", 
		       cdset->Aux->doms[isd]->edset_id);
	    SUMA_RETURN(NOPE);
	 }
	 if (pre_exist) { /* Parent dset was pre-existing in the list, 
	             	     assuming the 
	             	     same for all elementrary datasets */
            if (LocalHead) {
               fprintf( SUMA_STDERR,
                        "%s: assuming dset %s (%dth from %s) (%p) pre-existing, "
                        "finding its pre-existing overlays.\n", 
                        FuncName, SDSET_LABEL(sddset), isd,
			SDSET_LABEL(cdset), sddset); 
            }
            if (!(colplanepre = SUMA_Fetch_OverlayPointerByDset (
                                           asdo, sddset, &OverInd))) {
               SUMA_SLP_Err("Failed to fetch existing dset's "
                            "overlay pointer");
               SUMA_RETURN(NOPE);
            }
            /* Here you'd remove coord bias if you end up using it */
            /* and set flag to recompute clusters if you support clustering */
            OKdup = 1;
         } else { /* cdset and therefore its babies are considered new */
            SUMA_LH("New overlay for %s", SDSET_LABEL(sddset));
            colplanepre = NULL;
            /* The overlay index for that plane is SO->N_Overlays */
            OverInd = SUMA_ADO_N_Overlays(asdo);
            OKdup = 0;
         }

         /* set up the colormap for this dset */
	 if (!(NewColPlane = SUMA_CreateOverlayPointer ( SDSET_FILENAME(sddset), 
                                                   sddset, ADO_ID(asdo), 
                                                   colplanepre))) {
	    SUMA_S_Err("Failed in SUMA_CreateOverlayPointer for %s\n",
	               SDSET_LABEL(sddset));
            SUMA_RETURN(NOPE);
         }

            if (SetupOverlay < 0) {
               SUMA_LH("Have not pondered how to do 'background' for the "
                       "volume part of a CIFTI domain...");
               NewColPlane->isBackGrnd = YUP;
            } else NewColPlane->isBackGrnd = NOPE;     

            /* Add this plane to Overlays */
            SUMA_LH("Adding new plane to Overlays");
            if (!SUMA_AddNewPlane (asdo, NewColPlane, SUMAg_DOv, 
                                   SUMAg_N_DOv, OKdup)) {
               SUMA_SL_Err("Failed in SUMA_AddNewPlane");
               SUMA_FreeOverlayPointer(NewColPlane);
               SUMA_S_Warn("Usually I would delete loaded dset, "
	             	   "but here I would have to delete elementary beasts "
			   "also. Leaving it out for now");

               SUMA_RETURN(NOPE);
            }
      	 }

      	 /* Match old settings? */
      	 SUMA_LH("Settings");
      	 if (colplanepre == NewColPlane) { /* old col plane found for this dset*/
            /* Don't change settings. Before Aug 2012, it would reset as below */
      	 } else if ((SurfCont = SUMA_ADO_Cont(asdo)) &&
                 SUMA_PreserveOverlaySettings(SurfCont->curColPlane,
                                              NewColPlane)) {
                           /* attempt to preserve current situation */
            SUMA_OVERLAYS *settingPlane = NULL;
            settingPlane = SurfCont->curColPlane;
            NewColPlane->GlobalOpacity = settingPlane->GlobalOpacity;
            NewColPlane->ShowMode = settingPlane->ShowMode;
            NewColPlane->OptScl->BrightFact = settingPlane->OptScl->BrightFact;
            NewColPlane->OptScl->find = settingPlane->OptScl->find;
            NewColPlane->OptScl->tind = settingPlane->OptScl->tind;
            NewColPlane->OptScl->bind = settingPlane->OptScl->bind;
            NewColPlane->OptScl->UseThr = settingPlane->OptScl->UseThr;
            NewColPlane->OptScl->UseBrt = settingPlane->OptScl->UseBrt;
            NewColPlane->OptScl->ThrMode = settingPlane->OptScl->ThrMode;
            NewColPlane->OptScl->ThreshRange[0] = 
                                	  settingPlane->OptScl->ThreshRange[0];
            NewColPlane->OptScl->ThreshRange[1] = 
                                	  settingPlane->OptScl->ThreshRange[1];
            NewColPlane->OptScl->BrightRange[0] = 
                                	  settingPlane->OptScl->BrightRange[0];
            NewColPlane->OptScl->BrightRange[1] = 
                                	  settingPlane->OptScl->BrightRange[1];
            NewColPlane->OptScl->BrightMap[0] = 
                                	  settingPlane->OptScl->BrightMap[0];
            NewColPlane->OptScl->BrightMap[1] = 
                                	  settingPlane->OptScl->BrightMap[1];
            NewColPlane->SymIrange = settingPlane->SymIrange;
            NewColPlane->OptScl->IntRange[0] = settingPlane->OptScl->IntRange[0];
            NewColPlane->OptScl->IntRange[1] = settingPlane->OptScl->IntRange[1];
            dsetcmap = NI_get_attribute(sddset->ngr,"SRT_use_this_cmap");
            if (dsetcmap) {
               SUMA_STRING_REPLACE(NewColPlane->cmapname, dsetcmap);
            } else {
               SUMA_STRING_REPLACE(NewColPlane->cmapname, 
                                   settingPlane->cmapname);
            }         
            NewColPlane->OptScl->Clusterize = settingPlane->OptScl->Clusterize;
            NewColPlane->OptScl->ClustOpt->AreaLim = 
               settingPlane->OptScl->ClustOpt->AreaLim;
            NewColPlane->OptScl->ClustOpt->DistLim = 
               settingPlane->OptScl->ClustOpt->DistLim;
	 } else {
            SUMA_LH("New settings");
            /* set the opacity, index column and the range */
            NewColPlane->GlobalOpacity = YUP;
            NewColPlane->ShowMode = SW_SurfCont_DsetViewCol;
            if (!colplanepre) {/* only set this if first time creating plane*/
               NewColPlane->OptScl->BrightFact = 0.8;
            }
            NewColPlane->OptScl->find = 0;
            NewColPlane->OptScl->tind = 0;
            NewColPlane->OptScl->bind = 0;
            SUMA_GetDsetColRange(sddset, 0, NewColPlane->OptScl->IntRange, loc);
            if (NewColPlane->SymIrange) {
               NewColPlane->OptScl->IntRange[0] = 
        	  -fabs(SUMA_MAX_PAIR( NewColPlane->OptScl->IntRange[0],
                                       NewColPlane->OptScl->IntRange[1]));
               NewColPlane->OptScl->IntRange[1] = 
        	  -NewColPlane->OptScl->IntRange[0];
            }

            /* stick a colormap onto that plane ? */
            dsetcmap = NI_get_attribute(sddset->ngr,"SRT_use_this_cmap");
            if (dsetcmap) {
               SUMA_STRING_REPLACE(NewColPlane->cmapname, dsetcmap);
            } else {
               /* don't worry, there's a default one */
            }
	 }
	 if (NewColPlane->OptScl->Clusterize) 
            NewColPlane->OptScl->RecomputeClust = 1;
	 /* colorize the plane */
	 SUMA_LH("Colorizing Plane");
	 SUMA_ColorizePlane(NewColPlane);

	 /* SUMA_Show_ColorOverlayPlanes(&NewColPlane, 1, 1); */

	 if (SurfCont && MakeOverlayCurrent) 
            SurfCont->curColPlane = SUMA_ADO_Overlay(asdo, OverInd); 
      }
   }
      
   for (isd=0; isd<cdset->Aux->N_doms; ++isd) {
      asdo = SUMA_CIFTI_subdom_ado(CO, isd);
      if (!(sddset = DSET_FIND(cdset->Aux->doms[isd]->edset_id))) {
	 SUMA_S_Err("Should had found that dset, returning with potential  "
	            "leaks!");
	 SUMA_RETURN(NOPE);
      }
      /* Need to get OverInd again */
      if (!(SUMA_Fetch_OverlayPointerByDset ( asdo, sddset, &OverInd))) {
         SUMA_SLP_Err("How can this possibly happen?");
         SUMA_RETURN(NOPE);
      }      
      if ((SurfCont = SUMA_ADO_Cont(asdo)) && LaunchDisplay) {
	 SUMA_LHv("Remix Redisplay %s\n", ADO_LABEL(asdo));
	 /* remix-redisplay  for surface */
	 if (!SUMA_Remixedisplay (asdo)) {
            SUMA_RETURN(NOPE);
	 }
      	 
	 SUMA_LH("Refreshing Dset list");            
	 /*update the list widget if open */
	 LW = SurfCont->SwitchDsetlst;
	 if (LW) {
            if (!LW->isShaded) SUMA_RefreshDsetList (asdo);  
	 } 

	 SUMA_LH("Refreshing sub-brick selectors");            
	 /* if lists for switching sub-bricks are not shaded, update them too */
	 if (SurfCont->SwitchIntMenu) {
            if ((LW = SurfCont->SwitchIntMenu->lw) && !LW->isShaded) {
               SUMA_DsetColSelectList(asdo, 0, 0, 1);
            }
            if ((LW = SurfCont->SwitchThrMenu->lw) && !LW->isShaded) {
               SUMA_DsetColSelectList(asdo, 1, 0, 1);
            }
            if ((LW = SurfCont->SwitchBrtMenu->lw) && !LW->isShaded) {
               SUMA_DsetColSelectList(asdo, 2, 0, 1);
            }

            if (LocalHead) 
               fprintf (SUMA_STDERR,
	       
                	"%s: Updating Dset frame, OverInd=%d\n", 
                	FuncName, OverInd);
            /* update the Dset frame */
            if (OverInd >= 0)        
               SUMA_InitializeColPlaneShell(asdo, 
                                            SUMA_ADO_Overlay(asdo, OverInd));
	 }
      }
   }

   if (used_over) {
      SUMA_S_Warn( "Not sure how to use this for CIFTI, at the moment have"
      	          "multiple overlays per CO, returning last thing in hand.");
      *used_over = SUMA_ADO_Overlay(asdo, OverInd);
   }
   
   SUMA_RETURN(YUP);
      
}

/* Create CIFTI displayable object from the CIFTI dataset 
In the current incarnation (Tuesday Aug. 11 2015), the CIFTI DO
will not be a fully fledged DO such as a surface or a volume. It 
is a bucket that contains references to "elementary" DOs (to parallel
what happens to a multi-domain CIFTI dataset). It does not look like
it will get its own controller. I am no longer seeing the need for 
that.
*/
SUMA_CIFTI_DO * SUMA_CIFTI_DO_from_dset(SUMA_DSET *cdset)
{
   static char FuncName[]={"SUMA_CIFTI_DO_from_dset"};
   int k;
   char *ss=NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_CIFTI_DO *CO=NULL;
   SUMA_DSET *sddset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMA_isCIFTIDset(cdset)) {
      SUMA_S_Err("Not for this you don't");
      SUMA_RETURN(NULL);
   }
   if (!cdset || !cdset->Aux || !cdset->Aux->N_doms) {
      SUMA_S_Err("Junk in the house");
      SUMA_RETURN(NULL);
   }
   ss = SUMA_ar_string("DO_", ADO_LABEL((SUMA_ALL_DO *)cdset),"",0);
   CO = SUMA_CreateCIFTIObject(ss); SUMA_ifree(ss);
   
   if (CO->N_subdoms < 0) CO->N_subdoms = 0;
   for (k=0; k<cdset->Aux->N_doms; ++k) {
      if (!cdset->Aux->doms[k]->Source) {
         SUMA_S_Err("Null source");
         SUMA_FreeCIFTIObject(CO);
         SUMA_RETURN(NULL);
      }      
      
      SUMA_LH("Loading source[%d]=%s", k, cdset->Aux->doms[k]->Source); 
      switch (cdset->Aux->doms[k]->ModelType) {
         case SO_type: {
	    ado = (SUMA_ALL_DO *)
               SUMA_Load_Surface_Object_eng(
                     cdset->Aux->doms[k]->Source, 
                     SUMA_FT_NOT_SPECIFIED, SUMA_FF_NOT_SPECIFIED,
                     NULL, 2);
	    SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
	    if (!SUMA_ADO_Label(ado)) {
	       SO->Label = SUMA_copy_string(cdset->Aux->doms[k]->Source);
	    }
	    SUMA_ifree(SO->State);
	    SO->State = SUMA_copy_string("ANY_ANATOMICAL");
	    SUMA_ifree(SO->Group);
	    SO->Group = SUMA_copy_string("ANY");
	    if (1) {
	       SUMA_LH("Adding CO %s SOs to objects list", 
	             	ADO_LABEL((SUMA_ALL_DO *)CO));
	       if (!SUMA_PrepAddmappableSO(SO, SUMAg_DOv, &SUMAg_N_DOv, 
	             	      	           1, SUMAg_CF->DsetList)) {
                  SUMA_SL_Err("Failed in SUMA_PrepAddmappableSO.");
                  SUMA_FreeCIFTIObject(CO); CO = NULL;
        	  SUMA_RETURN(NULL);
               }
	    }
	    /* make domain parent of matching dset be SO */
	    if (!(sddset = DSET_FIND(cdset->Aux->doms[k]->edset_id))) {
	       SUMA_S_Err("Should have found that dset (id %s)!", 
		       cdset->Aux->doms[k]->edset_id);
	    } else {
	       NI_set_attribute(sddset->ngr, 
	             	        "domain_parent_idcode", ADO_ID(ado));
            }
	    break; }
         case VO_type: {
            SUMA_VolumeObject *VO=NULL;
	    SUMA_VOL_SAUX *VSaux = NULL;
            SUMA_LH("This requires some additional thinking:\n"
                        "1-All is needed for the volume is the grid.\n"
                        "  So might want to have LoadVolDO create a \n"
                        "  dummy volume from just a grid string (AFNI has\n"
                        "  such utilities.\n"
                        "  The actual data in the volume is now loaded by\n"
                        "  default but it is useless because it is \n"
                        "  to be trumped by the data in the CIFTI dataset.\n"
                        "2-Even if loading volume, might want to have an\n"
                        "  autocrop at loading option. See AFNI convenience\n"
                        "  function: THD_autobbox()\n The smaller the grid \n"
			"  the faster the volume rendering.\n");
            if (SUMA_LoadVolDO(cdset->Aux->doms[k]->Source, SUMA_WORLD, &VO, 1)){
	       ado = (SUMA_ALL_DO *)VO; 
               /* Change the state of the volume so that it is no longer 
	       of the default ANY_ANATOMICAL state. (See comment for string
	       'State' definition in SUMA_VOL_SAUX */
	       if (!(VSaux = SUMA_ADO_VSaux(ado))){
	          SUMA_S_Err("No VSaux?");
		  SUMA_FreeCIFTIObject(CO); CO = NULL;
		  SUMA_RETURN(NULL);
	       }
	       SUMA_ifree(VSaux->State);
	       VSaux->State = SUMA_copy_string("ANY_ANATOMICAL");
	       VO = NULL;

	       VSaux->ShowVrSlc = 1; /* easier for debugging */
            } 
            break; }
         default:
            SUMA_S_Err("Not ready for domain %d (%s) with CIFTI",
               cdset->Aux->doms[k]->ModelType,
               SUMA_ObjectTypeCode2ObjectTypeName(
                                             cdset->Aux->doms[k]->ModelType));
            SUMA_FreeCIFTIObject(CO); CO = NULL;
            SUMA_RETURN(NULL);
            break;
      }
      if (!ado) {
         SUMA_S_Err("Failed to load %s\n", cdset->Aux->doms[k]->Source);
         SUMA_FreeCIFTIObject(CO); CO = NULL;
         SUMA_RETURN(NULL);     
      } else {
        ++CO->N_subdoms;
         CO->subdoms_id = (char **)SUMA_realloc(CO->subdoms_id, 
                                                CO->N_subdoms*sizeof(char *));
         CO->subdoms_id[CO->N_subdoms-1] = SUMA_copy_string(ADO_ID(ado)); 
	 ado = NULL;
      }
   }
   
   if (1) {
      SUMA_LH("Adding CO %s to objects list", ADO_LABEL((SUMA_ALL_DO *)CO));
      if (!SUMA_AddDO(SUMAg_DOv, &(SUMAg_N_DOv), (void *)CO,  
               CDOM_type, SUMA_WORLD)) {
         fprintf(SUMA_STDERR,"Error %s: Error Adding DO\n", FuncName);
         SUMA_FreeCIFTIObject(CO); CO = NULL;
         SUMA_RETURN(NULL);
      }
   }

   
   SUMA_RETURN(CO);
}

/* Search all DOs for a CIFTIObject that can be the domain
for a certain CIFTI dataset.

For now, subdomains indices do not have to match. Might want to
enforce that. */
SUMA_CIFTI_DO *SUMA_CIFTI_find_matching_domain(SUMA_DSET *cdset, 
                                               SUMA_DO *dov, int N_dov) 
{
   static char FuncName[]={"SUMA_CIFTI_find_matching_domain"};
   SUMA_CIFTI_DO *CO=NULL;
   int i, f, k;
   char *sid;
   
   SUMA_ENTRY;
   
   if (!dov) { dov = SUMAg_DOv; N_dov = SUMAg_N_DOv; }
   
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == CDOM_type) {
         CO = (SUMA_CIFTI_DO *)dov[i].OP;
         for (f=0,k=0; k<cdset->Aux->N_doms; ++k) {
            sid = SUMA_CIFTI_find_sub_domain(CO, 
                                 cdset->Aux->doms[k]->ModelType,
                                 cdset->Aux->doms[k]->ModelSide,
                                 cdset->Aux->doms[k]->Max_N_Data, NULL);
            if (sid) {++f;}
         }
         if (f == cdset->Aux->N_doms) {
            SUMA_RETURN(CO);
         }
      }
   }
   SUMA_RETURN(NULL);
}

/* Search the sub-domains of a CIFTIObject to match desired parameters */
char *SUMA_CIFTI_find_sub_domain(SUMA_CIFTI_DO *CO, SUMA_DO_Types ModelType,
                               SUMA_SO_SIDE ModelSide,
                               int Max_N_Data,
                               int *k)
{
   static char FuncName[]={"SUMA_CIFTI_find_sub_domain"};
   char *sid = NULL;
   int i;
   SUMA_ALL_DO *ado=NULL;
   
   SUMA_ENTRY;
   if (k) *k = -1;
   
   for (i=0; i<CO->N_subdoms; ++i) {
      ado = SUMA_CIFTI_subdom_ado(CO, i);
      if ( ado->do_type == ModelType &&
          (    ModelType != SO_type || 
               ModelSide == ((SUMA_SurfaceObject *)(ado))->Side ) &&
          Max_N_Data == SUMA_ADO_N_Datum(ado) ) {
         if (k) *k = i;
         SUMA_RETURN(ADO_ID((SUMA_ALL_DO*)CO));   
      }
   }
   
   SUMA_RETURN(NULL);
}

/* Search all displayable objects for a CIFTI object containing a particular 
   domain. */
SUMA_CIFTI_DO *SUMA_find_CIFTI_subdom_container(char *SD_id, int *ksubdom, 
      	             	      	                SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_find_CIFTI_subdom_container"};
   int i, k;
   SUMA_CIFTI_DO *CO=NULL;
   
   SUMA_ENTRY;
   
   if (!dov) {
      dov = SUMAg_DOv;
      N_dov = SUMAg_N_DOv;
   }
   
   for (i=0; i<N_dov; ++i) {
      switch (dov[i].ObjectType) {
      	 case CDOM_type:
	    CO = (SUMA_CIFTI_DO *)dov[i].OP;
	    for (k=0; k<CO->N_subdoms; ++k) {
	       if (CO->subdoms_id[k] && !strcmp(SD_id, CO->subdoms_id[k])) {
	          /* got it */
		  if (ksubdom) *ksubdom = k;
		  SUMA_RETURN(CO);
	       }
	    }
	    break;
      }
   }
   
   SUMA_RETURN(NULL);
}

int SUMA_CIFTI_SubDomFullOffset(SUMA_CIFTI_DO *CO, int ksub)
{
   static char FuncName[]={"SUMA_CIFTI_SubDomFullOffset"};
   int i, N=0;
   
   for (i=1; i<=ksub; ++i) {
      N += SUMA_ADO_N_Datum(SUMA_CIFTI_subdom_ado(CO,i-1));
   }
   return(N);
}

SUMA_ALL_DO *SUMA_CIFTI_subdom_ado(SUMA_CIFTI_DO *CO, int ksub)
{
   static char FuncName[]={"SUMA_CIFTI_subdom_ado"};
   SUMA_ALL_DO *ado=NULL;
   
   SUMA_ENTRY;
   
   if (CO && ksub >=0 && ksub < CO->N_subdoms && CO->subdoms_id[ksub]) {
      ado = SUMA_whichADOg(CO->subdoms_id[ksub]);
   }
   
   SUMA_RETURN(ado);
}

