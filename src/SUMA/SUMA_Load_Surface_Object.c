    
/* Header FILES */
   
#include "SUMA_suma.h"


/*#define  DO_SCALE_RANGE   *//*!< scale node coordinates to 0 <--> 100. DO NOT USE IT, OBSOLETE*/
#ifndef DO_SCALE_RANGE
   #define DO_SCALE 319.7   /*!< scale node coordinates by specified factor. Useful for tesscon coordinate system in iv files*/
#endif

#undef STAND_ALONE

#if defined SUMA_Read_SpecFile_STAND_ALONE
#define STAND_ALONE
#elif defined SUMA_Load_Surface_Object_STAND_ALONE
#define STAND_ALONE
#elif defined SUMA_SurfaceMetrics_STAND_ALONE
#define STAND_ALONE 
#endif

#ifdef STAND_ALONE
/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs stored in SVv */
SUMA_DO *SUMAg_DOv;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF; /*!< Global pointer to structure containing info common to all viewers */
#else
extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  
#endif

/* CODE */

   
/*!
   \brief Function to write surface objects to disk in various formats
   ans = SUMA_Save_Surface_Object (void * F_name, SUMA_SurfaceObject *SO, SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF);
   \param F_name (void *)
         For SUMA_INVENTOR_GENERIC F_name is (char *) containing path (if any) and filename of surface
         For SUMA_SUREFIT F_name is (SUMA_SFname *) containing full topo and coord names, with path (if any)
         For SUMA_FREE_SURFER F_name is  (char *) name of .asc file (with path)
         For SUMA_VEC (a dumb ascii format), F_name is (SUMA_SFname *) containing the nodelist file in name_coord 
          and facesetlist file in name_topo (path included).
         For SUMA_PLY (char *) name of .ply file (with path)
   \param   SO_FT (SUMA_SO_File_Type) file type to be read (inventor, free surfer , Surefit )
   \param   SO_FF (SUMA_SO_File_Format) Ascii or Binary (only ascii at the moment, except for .ply files)

   \sa SUMA_Load_Surface_Object()
   
   NOTE:
   Vertex coordinates are written as in SO->NodeList
   The Volume Parent transformation is not undone. 
   For SureFit surfaces, the volume param shift is not undone.
*/
SUMA_Boolean SUMA_Save_Surface_Object (void * F_name, SUMA_SurfaceObject *SO, SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF)
{/*SUMA_Save_Surface_Object*/
   static char FuncName[]={"SUMA_Save_Surface_Object"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   
   switch (SO_FT) {
      case SUMA_PLY:
         if (!SUMA_Ply_Write ((char *)F_name, SO)) {
            fprintf (SUMA_STDERR, "Error %s: Failed to write PLY surface.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_FREE_SURFER:
         if (SO_FF != SUMA_ASCII) {
            fprintf (SUMA_STDERR, "Error %s: Only ASCII supported for Free Surfer surfaces.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         if (!SUMA_FS_Write ((char *)F_name, SO, "#Output of SUMA_SurfaceConvert")) {
            fprintf (SUMA_STDERR, "Error %s: Failed to write FreeSurfer surface.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_SUREFIT:
         if (SO_FF != SUMA_ASCII) {
            fprintf (SUMA_STDERR, "Error %s: Only ASCII supported for SureFit surfaces.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         if (!SUMA_SureFit_Write ((SUMA_SFname *)F_name, SO)) {
            fprintf (SUMA_STDERR, "Error %s: Failed to write SureFit surface.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_VEC:
         if (SO_FF != SUMA_ASCII) {
            fprintf (SUMA_STDERR, "Error %s: Only ASCII supported for vec surfaces.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         if (!SUMA_VEC_Write ((SUMA_SFname *)F_name, SO)) {
            fprintf (SUMA_STDERR, "Error %s: Failed to write vec surface.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         break;
      case SUMA_INVENTOR_GENERIC:
         fprintf (SUMA_STDERR, "Error %s: Not ready to deal with inventor surfaces.\n", FuncName);
         SUMA_RETURN (NOPE);
         break;
      case SUMA_FT_NOT_SPECIFIED:
      default:
         fprintf (SUMA_STDERR, "Error %s: Bad surface type.\n", FuncName);
         SUMA_RETURN (NOPE);
   
   }
   
   SUMA_RETURN (YUP);
}
   
/*!
\brief
      SO = SUMA_Load_Surface_Object ( SO_FileName, SO_FT, SO_FF, char *VolParName)
   
   
Input paramters : 
\param   (void *) SO_FileName 
         For SUMA_INVENTOR_GENERIC SO_FileName is (char *) containing path (if any) and filename of surface
         For SUMA_SUREFIT SO_FileName is (SUMA_SFname *) containing full topo and coord names, with path (if any)
         For SUMA_FREE_SURFER SO_FileName is  (char *) name of .asc file (with path)
         For SUMA_VEC (a dumb ascii format), SO_FileName is (SUMA_SFname *) containing the nodelist file in name_coord 
          and facesetlist file in name_topo (path included).
         For SUMA_PLY (char *) name of .ply file (with path)
\param   SO_FT (SUMA_SO_File_Type) file type to be read (inventor, free surfer , Surefit )
\param   SO_FF (SUMA_SO_File_Format) Ascii or Binary (only ascii at the moment, except for .ply files)
\param   VolParName (char *) filename (+path) of parent volume, pass NULL for none
         If you pass NULL, no transformation is applied to the coordinates read. 
   
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
   SO->ShowSelectedNode
   SO->ShowSelectedFaceSet
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
SUMA_SurfaceObject * SUMA_Load_Surface_Object (void *SO_FileName_vp, SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF, char *VolParName)
{/*SUMA_Load_Surface_Object*/
   char FuncName[]={"SUMA_Load_Surface_Object"};
   char stmp[1000], *SO_FileName=NULL;
   SUMA_SFname *SF_FileName; 
   int k, ND, id;
   SUMA_SureFit_struct *SF;
   SUMA_FreeSurfer_struct *FS;
   SUMA_SurfaceObject *SO;
   SUMA_SURF_NORM SN;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* Allocate and initialize SUMA_SurfaceObject Pointer */
   SO = SUMA_Alloc_SurfObject_Struct(1);
   
   /* check if recognizable type */
   switch (SO_FT) {
      case SUMA_INVENTOR_GENERIC:
         break;
      case SUMA_SUREFIT:
         break;
      case SUMA_FREE_SURFER:
         break;
      case SUMA_PLY:
         break;
      case SUMA_VEC:
         break;
      default:
         SUMA_error_message(FuncName, "SO_FileType not supported", 0);
         SUMA_RETURN (NULL);
         break;
   } /* SO_FT*/

   
   /* proceed for reading */
   switch (SO_FT) {
      case SUMA_FT_NOT_SPECIFIED:
         fprintf (SUMA_STDERR,"Error %s: No File Type specified.\n", FuncName);
         SUMA_RETURN(NULL);
         
      case SUMA_PLY:
         if (!SUMA_Ply_Read ((char *)SO_FileName_vp, SO)) {
            fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Ply_Read.\n", FuncName);
            SUMA_RETURN(NULL);
         }
         SO->idcode_str = UNIQ_hashcode((char *)SO_FileName_vp); 
         
         /* change coordinates to align them with volparent data set, if possible */
         if (VolParName != NULL) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf(SUMA_STDERR,"Error %s: Failed to load parent volume attributes.\n", FuncName);
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

         break;
         
      case SUMA_INVENTOR_GENERIC:
         SO_FileName = (char *)SO_FileName_vp;
         /* You need to split name into path and name ... */
         fprintf(stdout,"%s\n", SO_FileName);
         SO->Name = SUMA_StripPath(SO_FileName);
         /* check for file existence  */
         if (!SUMA_filexists(SO_FileName)) {
            sprintf(stmp,"File %s not found!", SO_FileName);
            SUMA_error_message(FuncName, stmp, 0);
            SUMA_RETURN (NULL);
         }
         SO->FileType = SO_FT;
         SO->FileFormat = SO_FF;
         SO->NodeDim = 3; /* This must be automated */
         SO->NodeList = SUMA_IV_XYZextract (SO_FileName, &(SO->N_Node), 0);
         if (SO->NodeList == NULL) {
            SUMA_error_message(FuncName,"SUMA_IV_XYZextract failed!",0);
            SUMA_RETURN(NULL);
         }
         SO->FaceSetList = SUMA_IV_FaceSetsextract (SO_FileName, &(SO->N_FaceSet));
         if (SO->FaceSetList == NULL) {
            SUMA_error_message(FuncName,"SUMA_IV_FaceSetsextract failed!",0);
            SUMA_RETURN(NULL);
         }
         SO->FaceSetDim = 3; /*This must also be automated */
         SO->idcode_str = UNIQ_hashcode(SO_FileName); 
         break;
         
      case SUMA_FREE_SURFER:
         /* Allocate for FS */
         FS = (SUMA_FreeSurfer_struct *) SUMA_malloc(sizeof(SUMA_FreeSurfer_struct));   
         if (FS == NULL) {
            fprintf(SUMA_STDERR,"Error %s: Failed to allocate for FS\n", FuncName);
            SUMA_RETURN (NULL);
         }
         SO->Name = SUMA_StripPath((char*)SO_FileName_vp);
         SO->FileType = SO_FT;
         SO->FileFormat = SO_FF;
         SO->NodeDim = 3; /* This must be automated */
         /*read the surface file */
         if (!SUMA_FreeSurfer_Read ((char*)SO_FileName_vp, FS)) {
            fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_FreeSurfer_Read.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         
         SUMA_Show_FreeSurfer (FS, NULL);
         /* save the juice and clean up the rest */
         SO->N_Node = FS->N_Node;
         /* Save the pointers to NodeList and FaceSetList and clear what is left of FS structure at the end */
         SO->NodeList = FS->NodeList;
         FS->NodeList = NULL;
         SO->FaceSetList = FS->FaceSetList;
         SO->N_FaceSet = FS->N_FaceSet;
         FS->FaceSetList = NULL;
         SO->FaceSetDim = 3; /*This must also be automated */
         
         
         /* change coordinates to align them with volparent data set, if possible */
         if (VolParName != NULL) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf(SUMA_STDERR,"Error %s: Failed to load parent volume attributes.\n", FuncName);
            } else {

               if (!SUMA_Align_to_VolPar (SO, (void*)FS)) SO->SUMA_VolPar_Aligned = NOPE;
                  else {
                     SO->SUMA_VolPar_Aligned = YUP;
                     /*SUMA_Show_VolPar(SO->VolPar, NULL);*/
                  }
         }
         } else { 
            SO->SUMA_VolPar_Aligned = NOPE;
         }
         
         /* free FS */
         if (!SUMA_Free_FreeSurfer (FS)) {
            fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Free_FreeSurfer.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         
         /* create the IDcode */
         SO->idcode_str = UNIQ_hashcode(SO_FileName_vp);
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Assigned idcode_str:%s:.\n", FuncName, SO->idcode_str);
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
            fprintf(SUMA_STDERR,"Error %s: Could not find %s\n", FuncName, SF_FileName->name_coord);
            SUMA_RETURN (NULL);
         }
         if (!SUMA_filexists(SF_FileName->name_topo)) {
            fprintf(SUMA_STDERR,"Error %s: Could not find %s\n", FuncName, SF_FileName->name_topo);
            SUMA_RETURN (NULL);
         }
         
         #if 0
         /* THE OLDE WAY */
         /* check number of elements */
         SO->N_Node = SUMA_float_file_size (SF_FileName->name_coord);
         if ((SO->N_Node %3)) {
            fprintf(SUMA_STDERR,"Error %s: Number of elements (%d) in vertex file %s is not multiple of 3.\n", 
               FuncName, SO->N_Node, SF_FileName->name_coord);
            SUMA_RETURN (NULL);
         }
         SO->N_Node /= 3;
         SO->N_FaceSet = SUMA_float_file_size (SF_FileName->name_topo);
         if ((SO->N_FaceSet % 3)) {
            fprintf(SUMA_STDERR,"Error %s: Number of elements (%d) in faceset file %s is not multiple of 3.\n", 
               FuncName, SO->N_Node, SF_FileName->name_topo);
            SUMA_RETURN (NULL);
         }
         SO->N_FaceSet /= 3;
         SO->FaceSetDim = 3;
         
         SO->NodeList = (float *)SUMA_calloc (SO->N_Node*SO->NodeDim, sizeof(float));
         SO->FaceSetList = (int *) SUMA_calloc (SO->N_FaceSet*SO->FaceSetDim, sizeof(int));
         if (!SO->NodeList || !SO->FaceSetList) {
            fprintf(SUMA_STDERR,"Error %s: Failed to allocate for NodeList or FaceSetList.\n", FuncName);
            if (SO->NodeList) SUMA_free(SO->NodeList);
            if (SO->FaceSetList) SUMA_free(SO->FaceSetList);
            SUMA_RETURN (NULL);
         }
         SUMA_Read_file (SO->NodeList, SF_FileName->name_coord, SO->N_Node*SO->NodeDim);
         SUMA_Read_dfile (SO->FaceSetList, SF_FileName->name_topo, SO->N_FaceSet*SO->FaceSetDim);
        
         #else
         /* the im_read_1D way */
         {
            MRI_IMAGE *im = NULL;
            float *far=NULL;
            int icnt;
            
            im = mri_read_1D (SF_FileName->name_coord);
            if (!im) {
               SUMA_SLP_Err("Failed to read 1D file");
               SUMA_RETURN(NULL);
            }
            far = MRI_FLOAT_PTR(im);
            SO->N_Node = im->nx;
            SO->NodeDim = im->ny;
            if (!SO->N_Node) {
               SUMA_SL_Err("Empty file");
               SUMA_RETURN(NULL);
            }
            if (SO->NodeDim !=  3 ) {
               SUMA_SL_Err("File must have\n"
                           "3 columns.");
               mri_free(im); im = NULL;   /* done with that baby */
               SUMA_RETURN(NULL);
            }
            
            SO->NodeList = (float *)SUMA_calloc (SO->N_Node*SO->NodeDim, sizeof(float));
            if (!SO->NodeList) {
               fprintf(SUMA_STDERR,"Error %s: Failed to allocate for NodeList.\n", FuncName);
               if (SO->NodeList) SUMA_free(SO->NodeList);
               if (SO->FaceSetList) SUMA_free(SO->FaceSetList);
               SUMA_RETURN (NULL);
            }
            
            for (icnt=0; icnt < SO->N_Node; ++icnt) {
               SO->NodeList[3*icnt] = far[icnt];
               SO->NodeList[3*icnt+1] = far[icnt+SO->N_Node];
               SO->NodeList[3*icnt+2] = far[icnt+2*SO->N_Node];
            }   
            if (LocalHead) {
               fprintf (SUMA_STDERR,"%s: SO->NodeList\n Node 0: %f, %f, %f \n Node %d: %f, %f, %f \n",
                  FuncName,
                  SO->NodeList[0], SO->NodeList[1], SO->NodeList[2], SO->N_Node -1, 
                  SO->NodeList[3*(SO->N_Node-1)], SO->NodeList[3*(SO->N_Node-1)+1], SO->NodeList[3*(SO->N_Node-1)+2]);
            }
            mri_free(im); im = NULL;
            
            im = mri_read_1D (SF_FileName->name_topo);
            if (!im) {
               SUMA_SLP_Err("Failed to read 1D file");
               SUMA_RETURN(NULL);
            }
            far = MRI_FLOAT_PTR(im);
            SO->N_FaceSet = im->nx;
            SO->FaceSetDim = im->ny;
            if (!SO->N_FaceSet) {
               SUMA_SL_Err("Empty file");
               SUMA_RETURN(NULL);
            }
            if (SO->FaceSetDim !=  3 ) {
               SUMA_SL_Err("File must have\n"
                           "3 columns.");
               mri_free(im); im = NULL;   /* done with that baby */
               SUMA_RETURN(NULL);
            }
            
            SO->FaceSetList = (int *)SUMA_calloc (SO->N_FaceSet*SO->FaceSetDim, sizeof(int));
            if (!SO->FaceSetList) {
               fprintf(SUMA_STDERR,"Error %s: Failed to allocate for FaceSetList.\n", FuncName);
               if (SO->NodeList) SUMA_free(SO->NodeList);
               if (SO->FaceSetList) SUMA_free(SO->FaceSetList);
               SUMA_RETURN (NULL);
            }
            
            for (icnt=0; icnt < SO->N_FaceSet; ++icnt) {
               SO->FaceSetList[3*icnt] = (int)far[icnt];
               SO->FaceSetList[3*icnt+1] = (int)far[icnt+SO->N_FaceSet];
               SO->FaceSetList[3*icnt+2] = (int)far[icnt+2*SO->N_FaceSet];
            }   
            
            if (LocalHead) {
               fprintf (SUMA_STDERR,"%s: SO->FaceSetList\n Node 0: %d, %d, %d \n Node %d: %d, %d, %d \n",
                  FuncName,
                  SO->FaceSetList[0], SO->FaceSetList[1], SO->FaceSetList[2], SO->N_FaceSet -1, 
                  SO->FaceSetList[3*(SO->N_FaceSet-1)], SO->FaceSetList[3*(SO->N_FaceSet-1)+1], SO->FaceSetList[3*(SO->N_FaceSet-1)+2]);
            } 
            mri_free(im); im = NULL;
            
         }
         #endif
                  
         sprintf (stmp, "%s%s", SF_FileName->name_coord, SF_FileName->name_topo);
         SO->idcode_str = UNIQ_hashcode(stmp);
         
         /* change coordinates to align them with volparent data set, if possible */
         if (VolParName != NULL) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf(SUMA_STDERR,"Error %s: Failed to load parent volume attributes.\n", FuncName);
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

         break;
         
      case SUMA_SUREFIT:
         /* Allocate for SF */
         SF = (SUMA_SureFit_struct *) SUMA_malloc(sizeof(SUMA_SureFit_struct));   
         if (SF == NULL) {
            fprintf(SUMA_STDERR,"Error %s: Failed to allocate for SF\n", FuncName);
            SUMA_RETURN (NULL);
         }
         SF_FileName = (SUMA_SFname *)SO_FileName_vp;
         /* form the topo and the coord names */
         SO->Name_coord = SUMA_StripPath(SF_FileName->name_coord);
         SO->Name_topo = SUMA_StripPath(SF_FileName->name_topo);
         SO->FileType = SO_FT;
         SO->FileFormat = SO_FF;
         SO->NodeDim = 3; /* This must be automated */
         /*read the coordinate file */
         if (!SUMA_SureFit_Read_Coord (SF_FileName->name_coord, SF)) {
            fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SureFit_Read_Coord.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         /* copy the pertinent data to SO */
         SO->N_Node = SF->N_Node;
         /* Save the pointers to NodeList and FaceSetList and clear what is left of SF structure at the end */
         SO->NodeList = SF->NodeList;
         SF->NodeList = NULL;
         
         /*read the topology file */
         if (!SUMA_SureFit_Read_Topo (SF_FileName->name_topo, SF)) {
            fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SureFit_Read_Topo.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         /* read the param file */
         if (strlen(SF_FileName->name_param)){
            if (!SUMA_Read_SureFit_Param(SF_FileName->name_param, SF)) {
               fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SureFit_Read_Topo.\n", FuncName);
            }
         } else {
            if (VolParName != NULL) {
               fprintf(SUMA_STDERR,"Error %s: Volume Parent specified without .param file.\nParent Volume Alignment will not be done.", FuncName);
            }
         }
         
         /* copy the pertinent data to SO */
         SO->FaceSetList = SF->FaceSetList;
         SO->N_FaceSet = SF->N_FaceSet;
         SF->FaceSetList = NULL;
         SO->FaceSetDim = 3; /*This must also be automated */

         /* change coordinates to align them with volparent data set, if possible */
         if (VolParName != NULL && strlen(SF_FileName->name_param)) {
            SO->VolPar = SUMA_VolPar_Attr (VolParName);
            if (SO->VolPar == NULL) {
               fprintf(SUMA_STDERR,"Error %s: Failed to load parent volume attributes.\n", FuncName);
            } else {
               /*SUMA_Show_VolPar(SO->VolPar, NULL);*/

               if (!SUMA_Align_to_VolPar (SO, (void *)SF)) SO->SUMA_VolPar_Aligned = NOPE;
                  else SO->SUMA_VolPar_Aligned = YUP;
               }
         } else { 
            SO->SUMA_VolPar_Aligned = NOPE;
         }
         
         /* free SF */
         if (!SUMA_Free_SureFit (SF)) {
            fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Free_SureFit.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         
         sprintf (stmp, "%s%s", SF_FileName->name_coord, SF_FileName->name_topo);
         SO->idcode_str = UNIQ_hashcode(stmp);
         break;
   } /* SO_FileType*/

   /* Calculate Min, Max, Mean */
   
   SUMA_MIN_MAX_SUM_VECMAT_COL (SO->NodeList, SO->N_Node, SO->NodeDim, SO->MinDims, SO->MaxDims, SO->Center);
     
   SO->Center[0] /= SO->N_Node;
   SO->Center[1] /= SO->N_Node;
   SO->Center[2] /= SO->N_Node;

   SUMA_MIN_VEC (SO->MinDims, 3, SO->aMinDims );
   SUMA_MAX_VEC (SO->MaxDims, 3, SO->aMaxDims);

   #ifdef DO_SCALE_RANGE
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
   if ((SO->aMaxDims - SO->aMinDims) > SUMA_TESSCON_DIFF_FLAG) {
      fprintf (stdout,"\n\nWARNING %s:\n Assuming %s to be in tesscon units, scaling down by %f.\n\aYou might have abnormally large or small freakish vertex coordinates\n\n",\
         FuncName, SO_FileName, SUMA_TESSCON_TO_MM);
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
   } 
   #endif
    
   
   /* Calculate SurfaceNormals */
   SN = SUMA_SurfNorm(SO->NodeList,  SO->N_Node, SO->FaceSetList, SO->N_FaceSet );
   SO->NodeNormList = SN.NodeNormList;
   SO->FaceNormList = SN.FaceNormList;

   /*create the structures for GL rendering */
   /*The data is being duplicated at the moment and perhaps I should just stick with the 1D stuf */
   SO->glar_NodeList = (GLfloat *) SO->NodeList; /* just copy the pointer, not the data */
   SO->glar_FaceSetList = (GLint *) SO->FaceSetList; /* just copy the pointer, not the data */
   SO->glar_FaceNormList = (GLfloat *) SO->FaceNormList; /* just copy the pointer, not the data */
   SO->glar_NodeNormList = (GLfloat *) SO->NodeNormList; /* just copy the pointer, not the data */

   /* a surface object does contribute to the rotation center of the viewer displaying it */
   SO->RotationWeight = SO->N_Node;
   SO->ViewCenterWeight = SO->N_Node;
   
   /* No selections yet, but make the preps */
      SO->ShowSelectedNode = YUP;
      SO->ShowSelectedFaceSet = YUP;
      SO->SelectedFaceSet = -1;
      SO->SelectedNode = -1;
      /* create the ball object*/
      SO->NodeMarker = SUMA_Alloc_SphereMarker ();
      if (SO->NodeMarker == NULL) {
         fprintf(SUMA_STDERR,"Error%s: Could not allocate for SO->NodeMarker\n", FuncName);
         SUMA_Free_Surface_Object (SO);
         SUMA_RETURN (NULL);
      }
      /* create the FaceSetMarker object */
      SO->FaceSetMarker = SUMA_Alloc_FaceSetMarker();
      if (SO->FaceSetMarker == NULL) {
         fprintf(SUMA_STDERR,"Error%s: Could not allocate for SO->FaceSetMarker\n", FuncName);
         SUMA_Free_Surface_Object (SO);
         SUMA_RETURN (NULL);
      }
   SUMA_RETURN (SO);
   
}/*SUMA_Load_Surface_Object*/

 
/*!
   SUMA_Boolean SUMA_ParseLHS_RHS (char *s, char *lhs, char *rhs)
   
   Parses S of the form "lhs = rhs" 
   blanks are necessary around the = sign
   s, lhs and rhs must be allocated for
   
   \param s (char *) "joe = fred"
   \param lhs (char *) "joe"
   \param rhs (char *) returned "fred"
   \ret YUP/NOPE for goodness, badness
   
*/
SUMA_Boolean SUMA_ParseLHS_RHS (char *s, char *lhs, char *rhs)
{
   static char FuncName[]={"SUMA_ParseLHS_RHS"};
   char *st;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (s == NULL) {
      fprintf(SUMA_STDERR,"Error %s: NULL s\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   st = strtok(s, " \0=");
   if (SUMA_iswordin (st,"=") == 1) { /* no blanks it seems */
      /*fprintf(SUMA_STDERR,"NO BLANK, st:%s\n", st);*/
      fprintf(SUMA_STDERR,"Error %s: Bad file format. Perhaps no blanks before = sign after LHS argument %s.\n", FuncName, lhs);
      SUMA_RETURN (NOPE);
   } else { /* skip the next blank to = */
      st = strtok(NULL, " \0=");
      if (SUMA_iswordin (st,"=")!=1) {
         fprintf(SUMA_STDERR,"Error %s: Bad file format. Perhaps no blanks around = after LHS argument %s.\n", FuncName, lhs);
         SUMA_RETURN (NOPE);
      }
   }
   /* get the rhs */
   st = strtok(NULL, " \0=");
   if (st == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Bad file format. Perhaps no blanks after = after LHS argument %s.\n", FuncName, lhs);
      SUMA_RETURN (NOPE);
   } else {
      sprintf(rhs,"%s", st);
      /*fprintf(SUMA_STDERR,"RHS: %s\n", rhs);*/
   }
   SUMA_RETURN (YUP); 
}

/*! 
   Function to read the surface specs file.
   \param fname (char *) name of the specs file
   \param Spec (SUMA_SurfSpecFile *) pre-allocated pointer to SUMA_SurfSpecFile structure )
   \ret YUP, good, NOPE, not good
*/
SUMA_Boolean SUMA_Read_SpecFile (char *f_name, SUMA_SurfSpecFile * Spec)
{/* SUMA_Read_SpecFile */
   static char FuncName[]={"SUMA_Read_SpecFile"};
   char s[SUMA_MAX_DIR_LENGTH], stmp[SUMA_MAX_DIR_LENGTH],  stmp2[SUMA_MAX_DIR_LENGTH], c;
   int ex, skp, evl, i;
   FILE *sf_file;
   SUMA_FileName SpecName;
   SUMA_Boolean OKread_SurfaceFormat, OKread_SurfaceType, OKread_TopoFile, OKread_CoordFile;
   SUMA_Boolean OKread_MappingRef, OKread_SureFitVolParam, OKread_FreeSurferSurface, OKread_InventorSurface;
   SUMA_Boolean OKread_Group, OKread_State, OKread_EmbedDim, OKread_SurfaceVolume, OKread_SurfaceLabel;
   char DupWarn[]={"Bad format in specfile (you may need a NewSurface line). Duplicate specification of"};
   char NewSurfWarn[]={"Bad format in specfile. You must start with NewSurface line before any other field."};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /*make sure file is there */
   if (!SUMA_filexists(f_name)) {
      fprintf(SUMA_STDERR,"Error %s: File %s does not exist or cannot be read.\n", FuncName, f_name);
      SUMA_RETURN (NOPE);
   }
   Spec->N_Surfs = 0;
   
   /* set the path for the spec file */
   SpecName = SUMA_StripPath (f_name);
   if (strlen(SpecName.Path) > SUMA_MAX_DIR_LENGTH-1) {
      fprintf(SUMA_STDERR,"Error %s: Path of specfile > %d charcters.\n", FuncName, SUMA_MAX_DIR_LENGTH-1);
      SUMA_RETURN (NOPE);
   }
   sprintf(Spec->SpecFilePath,"%s", SpecName.Path);
   /* free SpecName since it's not used elsewhere */
   if (SpecName.Path) SUMA_free(SpecName.Path);
   if (SpecName.FileName) SUMA_free(SpecName.FileName);

   /*read the thing*/
   sf_file = fopen (f_name,"r");
   if (sf_file == NULL)
      {
         fprintf(SUMA_STDERR,"Error %s: Could not open file for read\n", FuncName);
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
   /*fprintf(SUMA_STDERR,"Read %s\n", s);*/
   OKread_Group = YUP; /* it is OK to read a group before a new surface is declared */
   OKread_SurfaceFormat = OKread_SurfaceType = OKread_TopoFile = OKread_CoordFile = NOPE;
   OKread_MappingRef = OKread_SureFitVolParam = OKread_FreeSurferSurface = OKread_InventorSurface = NOPE;
   OKread_State = OKread_EmbedDim = OKread_SurfaceVolume = OKread_SurfaceLabel = NOPE ;
   
   Spec->StateList[0] = '\0';
   Spec->Group[0][0] = '\0';
   Spec->N_Surfs = Spec->N_States = Spec->N_Groups = 0;
   while (ex !=EOF) {
      evl = SUMA_iswordin (s,"#");
      if (evl != 1) { /* not a comment */
         /*fprintf(SUMA_STDERR,"Not a comment: %s\n", s);*/
         skp = 0;
         sprintf(stmp,"NewSurface");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            if(Spec->N_Surfs >= SUMA_MAX_N_SURFACE_SPEC) {
               fprintf(SUMA_STDERR,"Error %s: Cannot read in more than %d new surfaces.\n", FuncName, SUMA_MAX_N_SURFACE_SPEC);
               SUMA_RETURN (NOPE);
            }
            Spec->N_Surfs += 1;
            /*fprintf(SUMA_STDERR,"Found New Surface, N = %d\n", Spec->N_Surfs);*/
            /* initialize some of the fields */
            if (Spec->N_Surfs == 1) { /* first surface, initialize to empty */
               sprintf(Spec->SurfaceFormat[Spec->N_Surfs-1],"ASCII");
               Spec->SurfaceType[Spec->N_Surfs-1][0] = '\0';
               Spec->TopoFile[Spec->N_Surfs-1][0] = Spec->CoordFile[Spec->N_Surfs-1][0] = '\0';
               Spec->MappingRef[Spec->N_Surfs-1][0] = '\0';
               Spec->SureFitVolParam[Spec->N_Surfs-1][0] = '\0';
               Spec->FreeSurferSurface[Spec->N_Surfs-1][0] = Spec->InventorSurface[Spec->N_Surfs-1][0] = '\0';
               Spec->State[Spec->N_Surfs-1][0] = '\0';
               Spec->IDcode[Spec->N_Surfs-1] = NULL; /* this field is set in LoadSpec function */
               Spec->EmbedDim[Spec->N_Surfs-1] = 3;
               Spec->VolParName[Spec->N_Surfs-1][0] = '\0';
               Spec->SurfaceLabel[Spec->N_Surfs-1][0] = '\0';
            } else { 
               /* make sure important fields have been filled */
               if (Spec->SurfaceType[Spec->N_Surfs-2][0] == '\0') {
                  fprintf(SUMA_STDERR,"Error %s: Failed to specify surface type for surface %d\n", FuncName, Spec->N_Surfs-2);
                  SUMA_RETURN (NOPE);
               }
               /* initilize SOME of the fields to previous one */
               strcpy(Spec->SurfaceFormat[Spec->N_Surfs-1], Spec->SurfaceFormat[Spec->N_Surfs-2]);
               strcpy(Spec->SurfaceType[Spec->N_Surfs-1], Spec->SurfaceType[Spec->N_Surfs-2]);
               strcpy(Spec->TopoFile[Spec->N_Surfs-1], Spec->TopoFile[Spec->N_Surfs-2]);
               strcpy(Spec->MappingRef[Spec->N_Surfs-1], Spec->MappingRef[Spec->N_Surfs-2]);
               strcpy(Spec->SureFitVolParam[Spec->N_Surfs-1], Spec->SureFitVolParam[Spec->N_Surfs-2]);
               strcpy(Spec->VolParName[Spec->N_Surfs-1],Spec->VolParName[Spec->N_Surfs-2]);
               Spec->IDcode[Spec->N_Surfs-1] = NULL; /* this field is set in LoadSpec function */
               Spec->SurfaceLabel[Spec->N_Surfs-1][0] = '\0';
               strcpy(Spec->Group[Spec->N_Surfs-1], Spec->Group[Spec->N_Surfs-2]);
               strcpy(Spec->State[Spec->N_Surfs-1], Spec->State[Spec->N_Surfs-2]);
               Spec->EmbedDim[Spec->N_Surfs-1] = Spec->EmbedDim[Spec->N_Surfs-2];
               /* only Spec->CoordFile, Spec->FreeSurferSurface or Spec->InventorSurface MUST be specified with a new surface */
            } 
            OKread_SurfaceFormat = OKread_SurfaceType = OKread_TopoFile = OKread_CoordFile = YUP;
            OKread_MappingRef = OKread_SureFitVolParam = OKread_FreeSurferSurface = OKread_InventorSurface = YUP;
            OKread_Group = OKread_State = OKread_EmbedDim = OKread_SurfaceLabel = OKread_SurfaceVolume = YUP;
            skp = 1;
         }
         
         sprintf(stmp,"StateDef");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found a state definition, parse it */
            if (!SUMA_ParseLHS_RHS (s, stmp, Spec->State[Spec->N_Surfs-1])) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            if (Spec->N_States == 0) {
               /* first state, add it to the list of states */
               sprintf(Spec->StateList, "%s|", Spec->State[Spec->N_Surfs-1]);
               Spec->N_States += 1;
            } else  {
               if (strcmp(Spec->StateList, Spec->State[Spec->N_Surfs-1]) == 0) {
                  /* it's a duplicate, complain and get outa here */
                  fprintf(SUMA_STDERR,"Error %s: Duplicate StateDef (%s).\n", FuncName, Spec->State[Spec->N_Surfs-1]);
                  SUMA_RETURN (NOPE);
               } else {
                  /* a new one, add it to the list and increment States counter */
                  sprintf(Spec->StateList, "%s%s|", Spec->StateList, Spec->State[Spec->N_Surfs-1]);
                  Spec->N_States += 1;
               }
            }
            skp = 1;
         }
         
         sprintf(stmp,"Group");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found a state definition, parse it */
            /*fprintf(SUMA_STDERR,"%s: Found %s.\n", FuncName, stmp);*/
            if (Spec->N_Surfs < 1) { /* no surfaces have been defined yet, group goes for all */
               if (!SUMA_ParseLHS_RHS (s, stmp, Spec->Group[0])) {
                  fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
                  SUMA_RETURN (NOPE);
               }
            }
            else {
               if (!SUMA_ParseLHS_RHS (s, stmp, Spec->Group[Spec->N_Surfs-1])) {
                  fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
                  SUMA_RETURN (NOPE);
               }
            }

            Spec->N_Groups += 1;
            
            if (!OKread_Group) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_Group = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"EmbedDimension");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /* found surface embedding dimension, parse it */
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            Spec->EmbedDim[Spec->N_Surfs-1] = atoi(stmp2);
            if (Spec->EmbedDim[Spec->N_Surfs-1] < 2 || Spec->EmbedDim[Spec->N_Surfs-1] > 3) {
               fprintf(SUMA_STDERR,"Error %s: Bad Embedding dimension %d. Only 2 and 3 allowed.\n", \
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
            if (!SUMA_ParseLHS_RHS (s, stmp, Spec->State[Spec->N_Surfs-1])) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            /* make sure it is in the StateList */
            if (SUMA_iswordin (Spec->StateList, Spec->State[Spec->N_Surfs-1]) != 1) {
               fprintf(SUMA_STDERR,"Error %s: State %s was not predefined in StateDef.\nStateDef List (| delimited) = %s \n",\
                FuncName, Spec->State[Spec->N_Surfs-1], Spec->StateList);
               SUMA_RETURN (NOPE);
            }
            if (!OKread_State) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
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
            
            if (!SUMA_ParseLHS_RHS (s, stmp, Spec->SurfaceFormat[Spec->N_Surfs-1])) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            
            if (!OKread_SurfaceFormat) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_SurfaceFormat = NOPE;
            }
            skp = 1;
            /*fprintf(SUMA_STDERR,"%s\n", Spec->SurfaceFormat[Spec->N_Surfs-1]);*/
         }
         
         sprintf(stmp,"SurfaceType");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            /*fprintf(SUMA_STDERR,"Found %s\n", stmp);*/
            if (!SUMA_ParseLHS_RHS (s, stmp, Spec->SurfaceType[Spec->N_Surfs-1])) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
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
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            sprintf(Spec->TopoFile[Spec->N_Surfs-1], "%s%s", Spec->SpecFilePath, stmp2);
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
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            sprintf(Spec->TopoFile[Spec->N_Surfs-1], "%s%s", Spec->SpecFilePath, stmp2);
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
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            sprintf (Spec->CoordFile[Spec->N_Surfs-1], "%s%s", Spec->SpecFilePath, stmp2);
            
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
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            sprintf (Spec->CoordFile[Spec->N_Surfs-1], "%s%s", Spec->SpecFilePath, stmp2);
            
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
            /*fprintf(SUMA_STDERR,"Found %s\n", stmp);*/
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            sprintf (Spec->MappingRef[Spec->N_Surfs-1], "%s%s", Spec->SpecFilePath, stmp2);
            if (!OKread_MappingRef) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_MappingRef = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"SureFitVolParam");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /*fprintf(SUMA_STDERR,"Found %s\n", stmp);*/
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            sprintf (Spec->SureFitVolParam[Spec->N_Surfs-1], "%s%s", Spec->SpecFilePath, stmp2);
            
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
            /*fprintf(SUMA_STDERR,"Found %s\n", stmp);*/
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            sprintf (Spec->FreeSurferSurface[Spec->N_Surfs-1], "%s%s", Spec->SpecFilePath, stmp2);
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
            /*fprintf(SUMA_STDERR,"Found %s\n", stmp);*/
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            sprintf (Spec->FreeSurferSurface[Spec->N_Surfs-1], "%s%s", Spec->SpecFilePath, stmp2);
            if (!OKread_FreeSurferSurface) {
               fprintf(SUMA_STDERR,"Error %s: %s %s\n", FuncName, DupWarn, stmp);
               SUMA_RETURN (NOPE);
            } else {
               OKread_FreeSurferSurface = NOPE;
            }
            skp = 1;
         }
         
         sprintf(stmp,"InventorSurface");
         if (!skp && SUMA_iswordin (s, stmp) == 1) {
            /*fprintf(SUMA_STDERR,"Found %s\n", stmp);*/
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            sprintf(Spec->InventorSurface[Spec->N_Surfs-1], "%s%s", Spec->SpecFilePath, stmp2);
            
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
            /*fprintf(SUMA_STDERR,"Found %s\n", stmp);*/
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            
            fprintf(SUMA_STDOUT,"Warning %s: Found SurfaceVolume in Spec File, Name must include path to volume.\n", FuncName);
            
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
            /*fprintf(SUMA_STDERR,"Found %s\n", stmp);*/
            if (Spec->N_Surfs < 1) {
               fprintf(SUMA_STDERR,"Error %s: %s\n", FuncName, NewSurfWarn);
               SUMA_RETURN (NOPE);
            }
            if (!SUMA_ParseLHS_RHS (s, stmp, stmp2)) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ParseLHS_RHS.\n", FuncName);
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
            fprintf(SUMA_STDERR,"Error %s: Your spec file contains uncommented gibberish:\n%s\nPlease deal with it.\n", \
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
      /*fprintf(SUMA_STDERR,"Read %s\n", s); */
   }
   fclose (sf_file);
   /* make sure last entry was good */
   if (Spec->SurfaceType[Spec->N_Surfs-1][0] == '\0') {
      fprintf(SUMA_STDERR,"Error %s: Failed to specify surface type for surface %d\n", FuncName, Spec->N_Surfs-1);
      SUMA_RETURN (NOPE);
   }

   SUMA_RETURN (YUP); 
}/* SUMA_Read_SpecFile */

/*! 
   Loads the surfaces specified in Spec and stores them in DOv
*/
SUMA_Boolean SUMA_LoadSpec (SUMA_SurfSpecFile *Spec, SUMA_DO *dov, int *N_dov, char *VolParName)
{/* SUMA_LoadSpec */
   static char FuncName[]={"SUMA_LoadSpec"};
   int i, k;
   char *tmpid, *tmpVolParName = NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Axis *EyeAxis;
   SUMA_SFname *SF_name;
   SUMA_OVERLAYS *NewColPlane=NULL;
   SUMA_INODE *NewColPlane_Inode = NULL;
   SUMA_Boolean brk, SurfIn=NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   fprintf (SUMA_STDERR, "Expecting to read %d surfaces.\n", Spec->N_Surfs);
   for (i=0; i<Spec->N_Surfs; ++i) { /* first loop across mappable surfaces */
      /*locate and load all Mappable surfaces */
      if (SUMA_iswordin(Spec->MappingRef[i],"SAME") == 1) { /* Mappable surfaces */
         fprintf (SUMA_STDERR,"\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv");
         fprintf (SUMA_STDERR,"Surface #%d (directly mappable), loading ...\n",i);

         if (Spec->VolParName[i][0] != '\0') {
            fprintf (SUMA_STDOUT, "Warning %s: Using Volume Parent Specified in Spec File. This overrides -sv option.\n", FuncName);
            tmpVolParName = Spec->VolParName[i];
         }else {
            tmpVolParName = VolParName;
         }

         brk = NOPE;
         
         if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "SureFit") == 1) {/* load surefit surface */
            SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
            sprintf(SF_name->name_coord,"%s", Spec->CoordFile[i]); ;
            sprintf(SF_name->name_topo,"%s", Spec->TopoFile[i]); 
            if (!strlen(Spec->SureFitVolParam[i])) { /* initialize to empty string */
               SF_name->name_param[0] = '\0'; 
            }
            else {
               sprintf(SF_name->name_param,"%s", Spec->SureFitVolParam[i]);
            }

            /* Load The Surface */
            if (SUMA_iswordin(Spec->SurfaceFormat[i], "ASCII") == 1) {
               SO = SUMA_Load_Surface_Object ((void *)SF_name, SUMA_SUREFIT, SUMA_ASCII, tmpVolParName);
            } else {
               fprintf(SUMA_STDERR,"Error %s: Only ASCII surfaces can be read for now.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            if (SO == NULL)   {
               fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
               SUMA_RETURN(NOPE);
            }

            SUMA_free(SF_name); 

            SurfIn = YUP;         
            brk = YUP;
         }/* load surefit surface */ 
         
         if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "1D") == 1) {/* load 1D surface */
            SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
            sprintf(SF_name->name_coord,"%s", Spec->CoordFile[i]); ;
            sprintf(SF_name->name_topo,"%s", Spec->TopoFile[i]); 
            SF_name->name_param[0] = '\0';
            

            /* Load The Surface */
            if (SUMA_iswordin(Spec->SurfaceFormat[i], "ASCII") == 1) {
               SO = SUMA_Load_Surface_Object ((void *)SF_name, SUMA_VEC, SUMA_ASCII, tmpVolParName);
            } else {
               fprintf(SUMA_STDERR,"Error %s: Only ASCII allowed for 1D files.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            if (SO == NULL)   {
               fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
               SUMA_RETURN(NOPE);
            }

            SUMA_free(SF_name); 

            SurfIn = YUP;         
            brk = YUP;
         }/* load 1D surface */
         
         if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "FreeSurfer") == 1) {/* load FreeSurfer surface */
            
            if (SUMA_iswordin(Spec->SurfaceFormat[i], "ASCII") == 1)
               SO = SUMA_Load_Surface_Object ((void *)Spec->FreeSurferSurface[i], SUMA_FREE_SURFER, SUMA_ASCII, tmpVolParName);
            else {
               fprintf(SUMA_STDERR,"Error %s: Only ASCII surfaces can be read for now.\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            if (SO == NULL)   {
               fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            SurfIn = YUP;
            brk = YUP;
         } /* load FreeSurfer surface */

         if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "Ply") == 1) {/* load Ply format surface */
            
            SO = SUMA_Load_Surface_Object ((void *)Spec->FreeSurferSurface[i], SUMA_PLY, SUMA_FF_NOT_SPECIFIED, tmpVolParName);
            
            if (SO == NULL)   {
               fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            SurfIn = YUP;
            brk = YUP;
         } /* load Ply format surface */
         
         if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "GenericInventor") == 1) {/* load generic inventor format surface */
            if (tmpVolParName != NULL) {
               fprintf(SUMA_STDERR,"Error %s: Sorry, but Parent volumes are not supported for generic inventor surfaces.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            if (SUMA_iswordin(Spec->SurfaceFormat[i], "ASCII") == 1)
               SO = SUMA_Load_Surface_Object ((void *)Spec->InventorSurface[i], SUMA_INVENTOR_GENERIC, SUMA_ASCII, NULL);
            else {
               fprintf(SUMA_STDERR,"Error %s: Only ASCII surfaces can be read for now.\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            if (SO == NULL)   {
               fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            SurfIn = YUP;

            brk = YUP;
         }
         
         if (!brk) {
            fprintf(SUMA_STDERR,"Error %s: Unknown SurfaceFormat %s.\n", FuncName, Spec->SurfaceType[i]);
            SUMA_RETURN(NOPE);
         }
         
         if (!SurfIn) {
            fprintf(SUMA_STDERR,"Error %s: Failed to read input surface.\n", FuncName);
            SUMA_RETURN(NOPE);
         }

         /* store the surface's idcode pointer for use in non mappable bloc below */
            Spec->IDcode[i] = SO->idcode_str;

         /* set its MappingRef id to its own */
            SO->MapRef_idcode_str = (char *)SUMA_calloc(strlen(SO->idcode_str)+1, sizeof(char));
            if (SO->MapRef_idcode_str == NULL) {
               fprintf(SUMA_STDERR,"Error %s: Failed to allocate for SO->MapRef_idcode_str. That is pretty bad.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            SO->MapRef_idcode_str = strcpy(SO->MapRef_idcode_str, SO->idcode_str);

         /* check if surface read was unique 
            it's inefficient to check after the surface is read, but idcode is generated in the read routine 
            and users should not be making this mistake too often */
            if (SUMA_existSO (SO->idcode_str, dov, *N_dov)) {
               fprintf(SUMA_STDERR,"Error %s: Surface %d is specifed more than once, multiple copies ignored.\n", FuncName, i);
               /* free SO */
               if (!SUMA_Free_Surface_Object (SO)) {
                  fprintf(SUMA_STDERR,"Error %s: Error freeing SO.\n", FuncName);
                  SUMA_RETURN (NOPE);
               }
               SurfIn = NOPE;
            }
         
         /* if the surface is loaded OK, and it has not been loaded previously, register it */
         if (SurfIn) {
            if (!SUMA_SurfaceMetrics (SO, "Convexity, EdgeList, MemberFace", NULL)) {
               fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            #if SUMA_CHECK_WINDING
            if (!SUMA_SurfaceMetrics (SO, "CheckWind", NULL)) {
               fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            #endif
   

            /* Now create the color map that goes with Cx */
            { /* MOST OF THIS BLOCK SHOULD BE TURNED TO A FUNCTION */
               SUMA_COLOR_MAP *CM;
               SUMA_SCALE_TO_MAP_OPT * OptScl;
               SUMA_STANDARD_CMAP MapType;
               SUMA_COLOR_SCALED_VECT * SV;
               float ClipRange[2], *Vsort;
               
               /* create the color mapping of Cx (SUMA_CMAP_MATLAB_DEF_BYR64)*/
               CM = SUMA_GetStandardMap (SUMA_CMAP_nGRAY20);
               if (CM == NULL) {
                  fprintf (SUMA_STDERR,"Error %s: Could not get standard colormap.\n", FuncName); 
                  SUMA_RETURN (NOPE);
               }

               /* get the options for creating the scaled color mapping */
               OptScl = SUMA_ScaleToMapOptInit();
               if (!OptScl) {
                  fprintf (SUMA_STDERR,"Error %s: Could not get scaling option structure.\n", FuncName);
                  SUMA_RETURN (NOPE); 
               }

               /* work the options a bit */
               OptScl->ApplyClip = YUP;
               ClipRange[0] = 5; ClipRange[1] = 95; /* percentile clipping range*/ 
               Vsort = SUMA_PercRange (SO->Cx, NULL, SO->N_Node, ClipRange, ClipRange); 
               OptScl->ClipRange[0] = ClipRange[0]; OptScl->ClipRange[1] = ClipRange[1];

               OptScl->BrightFact = SUMA_DIM_CONVEXITY_COLOR_FACTOR;

               /* map the values in SO->Cx to the colormap */
               SV = SUMA_Create_ColorScaledVect(SO->N_Node);/* allocate space for the result */
               if (!SV) {
                  fprintf (SUMA_STDERR,"Error %s: Could not allocate for SV.\n", FuncName);
                  SUMA_RETURN (NOPE);
               }

               /* finally ! */
               /*fprintf (SUMA_STDERR,"%s: 1st color in map %f %f %f\n", FuncName, CM->M[0][0], CM->M[0][1],CM->M[0][2]);*/
               if (!SUMA_ScaleToMap (SO->Cx, SO->N_Node, Vsort[0], Vsort[SO->N_Node-1], CM, OptScl, SV)) {
                  fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_ScaleToMap.\n", FuncName);
                  SUMA_RETURN (NOPE);
               }


               /* create an overlay plane */
               NewColPlane = SUMA_CreateOverlayPointer (SO->N_Node, "Convexity");
               if (!NewColPlane) {
                  fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateOverlayPointer.\n", FuncName);
                  SUMA_RETURN (NOPE);
               } 

               /* make an Inode for the overlay */
               NewColPlane_Inode = SUMA_CreateInode ((void *)NewColPlane, SO->idcode_str);
               if (!NewColPlane_Inode) {
                  fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInode\n", FuncName);
                  SUMA_RETURN (NOPE);
               }

               /* Now place the color map in the Coloroverlay structure */
               NewColPlane->ColMat = SV->cM; SV->cM = NULL; /* this way the color matrix will not be freed */
               NewColPlane->N_NodeDef = SO->N_Node;
               NewColPlane->GlobalOpacity = SUMA_CONVEXITY_COLORPLANE_OPACITY;
               NewColPlane->Show = YUP;
               NewColPlane->BrightMod = YUP;

               /* Add this plane to SO->Overlays */
               if (!SUMA_AddNewPlane (SO, NewColPlane, NewColPlane_Inode)) {
                  SUMA_SL_Crit("Failed in SUMA_AddNewPlane");
                  SUMA_FreeOverlayPointer(NewColPlane);
                  SUMA_RETURN (NOPE);
               }

               
               /* free */
               if (Vsort) SUMA_free(Vsort);
               if (CM) SUMA_Free_ColorMap (CM);
                if (OptScl) SUMA_free(OptScl);
               if (SV) SUMA_Free_ColorScaledVect (SV);
            }
            
            /* new surface loaded, do the deeds */
            /* assign its Group and State and Side*/
            SO->Group = (char *)SUMA_calloc(strlen(Spec->Group[i])+1, sizeof(char));
            SO->State = (char *)SUMA_calloc(strlen(Spec->State[i])+1, sizeof(char));
            if (Spec->SurfaceLabel[i][0] == '\0') {
               SO->Label = SUMA_SurfaceFileName (SO, NOPE);
               if (!SO->Label) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to create Label.\n", FuncName);
                  SUMA_RETURN(NOPE);
               }
            } else {
               SO->Label = (char *)SUMA_calloc(strlen(Spec->SurfaceLabel[i])+1, sizeof(char));
               if (!SO->Label) {
                  fprintf(SUMA_STDERR,"Error %s: Error allocating lameness.\n", FuncName);
                  SUMA_RETURN (NOPE);
               }
               SO->Label = strcpy(SO->Label, Spec->SurfaceLabel[i]);
            }
            
            
            if (!SO->Group || !SO->State || !SO->Label) {
               fprintf(SUMA_STDERR,"Error %s: Error allocating lameness.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            SO->Group = strcpy(SO->Group, Spec->Group[i]);
            SO->State = strcpy(SO->State, Spec->State[i]);
            SO->EmbedDim = Spec->EmbedDim[i];
            SO->Side = SUMA_GuessSide (SO);
            
            /* Create a Mesh Axis for the surface */
            SO->MeshAxis = SUMA_Alloc_Axis ("Surface Mesh Axis");
            if (SO->MeshAxis == NULL) {
               fprintf(SUMA_STDERR,"Error %s: Error Allocating axis\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            /* Change the defaults of Mesh axis to fit standard  */
            SUMA_MeshAxisStandard (SO->MeshAxis, SO);
            /*turn on the viewing for the axis */
            SO->ShowMeshAxis = NOPE;

            /* Store it into dov */
            if (!SUMA_AddDO(dov, N_dov, (void *)SO,  SO_type, SUMA_LOCAL)) {
               fprintf(SUMA_STDERR,"Error %s: Error Adding DO\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            

            SurfIn = NOPE;
         }
      }/* Mappable surfaces */
   }/* first loop across mappable surfaces */

   for (i=0; i<Spec->N_Surfs; ++i) { /* Now locate and load all NON Mappable surfaces */

      if (Spec->VolParName[i][0] != '\0') {
         fprintf (SUMA_STDOUT, "Warning %s: Using Volume Parent Specified in Spec File. This overrides -sv option.\n", FuncName);
         tmpVolParName = Spec->VolParName[i];
      }else {
         tmpVolParName = VolParName;
      }

      if (SUMA_iswordin(Spec->MappingRef[i],"SAME") != 1) { /* Non Mappable surfaces */
         fprintf (SUMA_STDERR,"\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv");
         fprintf (SUMA_STDERR,"Surface #%d (mappable via MappingRef), loading ...\n",i);
         
         brk = NOPE;
         if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "SureFit") == 1) {/* load surefit surface */
            SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
            sprintf(SF_name->name_coord,"%s", Spec->CoordFile[i]); ;
            sprintf(SF_name->name_topo,"%s", Spec->TopoFile[i]); 
            if (!strlen(Spec->SureFitVolParam[i])) { /* initialize to empty string */
               SF_name->name_param[0] = '\0'; 
            }
            else {
               sprintf(SF_name->name_param,"%s", Spec->SureFitVolParam[i]);
            }

            /* Load The Surface */
            if (SUMA_iswordin(Spec->SurfaceFormat[i], "ASCII") == 1) {
               SO = SUMA_Load_Surface_Object ((void *)SF_name, SUMA_SUREFIT, SUMA_ASCII, tmpVolParName);
            } else {
               fprintf(SUMA_STDERR,"Error %s: Only ASCII surfaces can be read for now.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            if (SO == NULL)   {
               fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
               SUMA_RETURN(NOPE);
            }

            SUMA_free(SF_name); 

            SurfIn = YUP;         
            brk = YUP;
         }/* load surefit surface */ 

         if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "1D") == 1) {/* load 1D surface */
            SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
            sprintf(SF_name->name_coord,"%s", Spec->CoordFile[i]); ;
            sprintf(SF_name->name_topo,"%s", Spec->TopoFile[i]); 
            SF_name->name_param[0] = '\0';
            

            /* Load The Surface */
            if (SUMA_iswordin(Spec->SurfaceFormat[i], "ASCII") == 1) {
               SO = SUMA_Load_Surface_Object ((void *)SF_name, SUMA_VEC, SUMA_ASCII, tmpVolParName);
            } else {
               fprintf(SUMA_STDERR,"Error %s: Only ASCII allowed for 1D files.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            if (SO == NULL)   {
               fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
               SUMA_RETURN(NOPE);
            }

            SUMA_free(SF_name); 

            SurfIn = YUP;         
            brk = YUP;
         }/* load 1D surface */
         

         if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "FreeSurfer") == 1) {/* load FreeSurfer surface */
            
            if (SUMA_iswordin(Spec->SurfaceFormat[i], "ASCII") == 1)
               SO = SUMA_Load_Surface_Object ((void *)Spec->FreeSurferSurface[i], SUMA_FREE_SURFER, SUMA_ASCII, tmpVolParName);
            else {
               fprintf(SUMA_STDERR,"Error %s: Only ASCII surfaces can be read for now.\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            if (SO == NULL)   {
               fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            SurfIn = YUP;
            brk = YUP;
         } /* load FreeSurfer surface */

         if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "Ply") == 1) {/* load Ply format surface */
            
            SO = SUMA_Load_Surface_Object ((void *)Spec->FreeSurferSurface[i], SUMA_PLY, SUMA_FF_NOT_SPECIFIED, tmpVolParName);
            
            if (SO == NULL)   {
               fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            SurfIn = YUP;
            brk = YUP;
         } /* load Ply format surface */
         
         if (!brk && SUMA_iswordin(Spec->SurfaceType[i], "GenericInventor") == 1) {/* load generic inventor format surface */
            if (tmpVolParName != NULL) {
               fprintf(SUMA_STDERR,"Error %s: Sorry, but Parent volumes are not supported for generic inventor surfaces.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            if (SUMA_iswordin(Spec->SurfaceFormat[i], "ASCII") == 1)
               SO = SUMA_Load_Surface_Object ((void *)Spec->InventorSurface[i], SUMA_INVENTOR_GENERIC, SUMA_ASCII, NULL);
            else {
               fprintf(SUMA_STDERR,"Error %s: Only ASCII surfaces can be read for now.\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            if (SO == NULL)   {
               fprintf(SUMA_STDERR,"Error %s: could not load SO\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            SurfIn = YUP;

            brk = YUP;
         }


         /* check if surface read was unique 
            it's inefficient to check after the surface is read, but idcode is generated in the read routine 
            and users should not be making this mistake too often */
            if (SUMA_existSO (SO->idcode_str, dov, *N_dov)) {
               fprintf(SUMA_STDERR,"Error %s: Surface %d is specifed more than once, multiple copies ignored.\n", FuncName, i);
               /* free SO */
               if (!SUMA_Free_Surface_Object (SO)) {
                  fprintf(SUMA_STDERR,"Error %s: Error freeing SO.\n", FuncName);
                  SUMA_RETURN (NOPE);
               }
               SurfIn = NOPE;
            }

         /* if the surface is loaded OK, and it has not been loaded previously, register it */
         if (SurfIn) {
            /* assign its Group and State */
            SO->Group = (char *)SUMA_calloc(strlen(Spec->Group[i])+1, sizeof(char));
            SO->State = (char *)SUMA_calloc(strlen(Spec->State[i])+1, sizeof(char));

            if (Spec->SurfaceLabel[i][0] == '\0') {
               SO->Label = SUMA_SurfaceFileName (SO, NOPE);
               if (!SO->Label) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to create Label.\n", FuncName);
                  SUMA_RETURN(NOPE);
               }
            } else {
               SO->Label = (char *)SUMA_calloc(strlen(Spec->SurfaceLabel[i])+1, sizeof(char));
               if (!SO->Label) {
                  fprintf(SUMA_STDERR,"Error %s: Error allocating lameness.\n", FuncName);
                  SUMA_RETURN (NOPE);
               }
               SO->Label = strcpy(SO->Label, Spec->SurfaceLabel[i]);
            }
            
            if (!SO->Group || !SO->State) {
               fprintf(SUMA_STDERR,"Error %s: Error allocating lameness.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            SO->Group = strcpy(SO->Group, Spec->Group[i]);
            SO->State = strcpy(SO->State, Spec->State[i]);
            SO->EmbedDim = Spec->EmbedDim[i];
            SO->Side = SUMA_GuessSide (SO);

            /* Create a Mesh Axis for the surface */
            SO->MeshAxis = SUMA_Alloc_Axis ("Surface Mesh Axis");
            if (SO->MeshAxis == NULL) {
               fprintf(SUMA_STDERR,"Error %s: Error Allocating axis\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            /* Change the defaults of Mesh axis to fit standard  */
            SUMA_MeshAxisStandard (SO->MeshAxis, SO);
            /*turn on the viewing for the axis */
            SO->ShowMeshAxis = NOPE;

            /* Store it into dov */
            if (!SUMA_AddDO(dov, N_dov, (void *)SO,  SO_type, SUMA_LOCAL)) {
               fprintf(SUMA_STDERR,"Error %s: Error Adding DO\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            
         /* set its MappingRef id to NULL if none is specified */
            if (Spec->MappingRef[i][0] == '\0') {
               SO->MapRef_idcode_str = NULL; /* no known MapRef_idcode */
               fprintf(SUMA_STDERR,"No Mapping Ref specified.\n");
            } else {
               /* make sure that specified Mapping ref had been loaded */
                  int j = 0, ifound = -1;
                  while (j <=Spec->N_Surfs) {
                     /*fprintf(SUMA_STDERR,"%s\n%s\n%s\n%s\n%s\n", \
                        Spec->MappingRef[i], Spec->CoordFile[j], Spec->TopoFile[j],\
                        Spec->InventorSurface[j], Spec->FreeSurferSurface[j]);*/ 
                     if (strcmp(Spec->MappingRef[i], Spec->CoordFile[j]) == 0 || \
                         strcmp(Spec->MappingRef[i], Spec->TopoFile[j]) == 0 ||  \
                         strcmp(Spec->MappingRef[i], Spec->InventorSurface[j]) == 0 || \
                         strcmp(Spec->MappingRef[i], Spec->FreeSurferSurface[j]) == 0) {
                        /* found a match */
                        ifound = j;
                        j = Spec->N_Surfs + 1;   
                     }
                     ++j;
                  }
               if (ifound >= 0) { /* found */
                  /*fprintf (SUMA_STDERR,"ifound = %d, i = %d\nSpec->MappingRef[i]:->%s<-\n", ifound, i, Spec->MappingRef[i]);*/
                  if (!SUMA_existSO (Spec->IDcode[ifound], dov, *N_dov)) {
                     fprintf(SUMA_STDERR,"MappingRef unavailable, that should not happen here.\n");
                     SO->MapRef_idcode_str = NULL;
                  } else {
                     /*fprintf(SUMA_STDERR,"MappingRef found in mappable surfaces\n");*/
                     SO->MapRef_idcode_str = (char *)SUMA_calloc(strlen(Spec->IDcode[ifound])+1, sizeof(char));
                     if (SO->MapRef_idcode_str == NULL) {
                        fprintf(SUMA_STDERR,"Error %s: Failed to allocate for SO->MapRef_idcode_str. That is pretty bad.\n", FuncName);
                        SUMA_RETURN (NOPE);
                     }
                     SO->MapRef_idcode_str = strcpy(SO->MapRef_idcode_str, Spec->IDcode[ifound]);
                  }
               } else {
                  fprintf(SUMA_STDERR,"MappingRef unavailable, you won't be able to link to afni.\n");
                  SO->MapRef_idcode_str = NULL;
               }
            }
            
            /* create the colorlist vector and calculate the surface metrics with the possibility of inheriting from the mapping reference */
            {
               SUMA_SurfaceObject *SOinh = NULL;
               int ifound = -1; 
               
               if (SO->MapRef_idcode_str) {   
                  ifound =  SUMA_findSO_inDOv (SO->MapRef_idcode_str, dov, *N_dov);
                  if (ifound < 0) {
                     SOinh = NULL;
                  }else {
                     SOinh = (SUMA_SurfaceObject *)(dov[ifound].OP);
                  }
               } else SOinh = NULL;
         
               
               if (!SUMA_SurfaceMetrics (SO, "EdgeList, MemberFace", SOinh)) {
                  fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
                  SUMA_RETURN (NOPE);
               }
            }  
               
            

            SurfIn = NOPE;
         }
      }/* Non Mappable surfaces */

   }/*locate and load all NON Mappable surfaces */

   SUMA_RETURN (YUP);
}/* SUMA_LoadSpec */


/*!
   calculate surface properties
   
   ans = SUMA_SurfaceMetrics (SO, Metrics, SOinh)
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
   \return ans (SUMA_Boolean) NOPE = failure
   
   Convexity : Fills Cx field in SO, An inode is also created
   
   EdgeList also runs SUMA_Build_FirstNeighb
   
   Curvature requires also PolyArea, FaceNeighb and EdgeList
   CheckWind requires EdgeList
   
      
*/
SUMA_Boolean SUMA_SurfaceMetrics (SUMA_SurfaceObject *SO, const char *Metrics, SUMA_SurfaceObject *SOinh)
{
   static char FuncName[]={"SUMA_SurfaceMetrics"};
   SUMA_Boolean DoConv, DoArea, DoCurv, DoEL, DoMF, DoWind, LocalHead = NOPE;
   int i = 0;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   fprintf (SUMA_STDERR,"%s: Calculating surface metrics, please be patient...\n", FuncName);
   
   DoConv = DoArea = DoCurv = DoEL = DoMF = DoWind = NOPE;
   
   if (SUMA_iswordin (Metrics, "Convexity")) DoConv = YUP;
   if (SUMA_iswordin (Metrics, "PolyArea")) DoArea = YUP;
   if (SUMA_iswordin (Metrics, "Curvature")) DoCurv = YUP;
   if (SUMA_iswordin (Metrics, "EdgeList")) DoEL = YUP;
   if (SUMA_iswordin (Metrics, "MemberFace")) DoMF = YUP;
   if (SUMA_iswordin (Metrics, "CheckWind")) DoWind = YUP;
   
   /* check for input inconsistencies and warn */
   if (!DoConv && !DoArea && !DoCurv && !DoEL  && !DoMF && !DoWind) {
      fprintf (SUMA_STDERR,"Warning %s: Nothing to do.\n", FuncName);
      SUMA_RETURN (YUP);
   }
   
   if (DoConv && SO->Cx != NULL) {
      fprintf (SUMA_STDERR,"Warning %s: SO->Cx != NULL and thus appears to have been precomputed.\n", FuncName);
      DoConv = NOPE;
   }
   
   if (DoArea && SO->PolyArea != NULL) {
      fprintf (SUMA_STDERR,"Warning %s: SO->PolyArea != NULL and thus appears to have been precomputed.\n", FuncName);
      DoArea = NOPE;
   }
   
   if (DoCurv && SO->SC != NULL) {
      fprintf (SUMA_STDERR,"Warning %s: SO->SC != NULL and thus appears to have been precomputed.\n", FuncName);
      DoCurv = NOPE;
   }
   
   if (DoMF && SO->MF != NULL) {
      fprintf (SUMA_STDERR,"Warning %s: SO->MF != NULL and thus appears to have been precomputed.\n", FuncName);
      DoMF = NOPE;
   }
   
   if (DoEL && (SO->EL != NULL || SO->FN != NULL)) {
      fprintf (SUMA_STDERR,"Warning %s: SO->EL != NULL || SO->FN != NULL and thus appears to have been precomputed.\n", FuncName);
      DoEL = NOPE;
   }
   
   if (DoEL && SOinh) {
      if (strcmp(SO->MapRef_idcode_str, SOinh->idcode_str)) {
         SUMA_SL_Warn(  "Cannot inherit Edge List\n"
                        "and First Neightbor.\n"
                        "Cause: idcode mismatch.\n"
                        "Independent lists will\n"
                        "be created." );
         SOinh = NULL;
      }else if (!SOinh->EL_Inode || !SOinh->FN_Inode){
         SUMA_SL_Warn(  "Cannot inherit Edge List\n"
                        "and First Neightbor.\n"
                        "Cause: NULL inodes.\n"
                        "Independent lists will\n"
                        "be created.");
         SOinh = NULL;
      }else if (SO->N_Node != SOinh->N_Node || SO->N_FaceSet != SOinh->N_FaceSet) {
         SUMA_SL_Warn(  "(IGNORE for surface patches)\n"
                        "Cannot inherit Edge List\n"
                        "and First Neightbor.\n"
                        "Cause: Node number mismatch.\n"
                        "Independent lists will\n"
                        "be created.");
         SOinh = NULL;      
      }
   }
   
   if (DoMF && SOinh) {
      if (strcmp(SO->MapRef_idcode_str, SOinh->idcode_str)) {
         SUMA_SL_Warn(  "Cannot inherit MemberFaceSet\n"
                        "Cause: idcode mismatch.\n"
                        "Independent lists will\n"
                        "be created." );
         SOinh = NULL;
      }else if (!SOinh->MF_Inode){
         SUMA_SL_Warn(  "Cannot inherit MemberFaceSet\n"
                        "Cause: NULL inodes.\n"
                        "Independent lists will\n"
                        "be created.");
         SOinh = NULL;
      }else if (SO->N_Node != SOinh->N_Node || SO->N_FaceSet != SOinh->N_FaceSet) {
         SUMA_SL_Warn(  "(IGNORE for surface patches)\n"
                        "Cannot inherit MemberFaceSet\n"
                        "Cause: Node number mismatch.\n"
                        "Independent lists will\n"
                        "be created.");
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

   if (DoConv) {
      DoEL = YUP;
   }
   
   /* the computations */
   if (DoArea) {
      /* create the triangle Area  */
      if (SO->NodeDim == 3) {
         fprintf(SUMA_STDOUT, "%s: Calculating triangle areas ...\n", FuncName); 
         SO->PolyArea = SUMA_TriSurf3v (SO->NodeList, SO->FaceSetList, SO->N_FaceSet);
      } else {
         fprintf(SUMA_STDOUT, "%s: Calculating polygon areas ...\n", FuncName); 
         SO->PolyArea = SUMA_PolySurf3 (SO->NodeList, SO->N_Node, SO->FaceSetList, SO->N_FaceSet, SO->NodeDim, SO->FaceNormList, NOPE);
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
                     fprintf (SUMA_STDERR, "Error %s: Failed comparing SUMA_TriSurf3 to SUMA_PolySurf3. A = %f vs %f.\nTri = [ %f, %f, %f; %f, %f, %f; %f, %f, %f]\n", 
                        FuncName, A, SO->PolyArea[ji], n0[0], n0[1], n0[2], n1[0], n1[1], n1[2], n2[0], n2[1], n2[2]);
                  }else fprintf (SUMA_STDERR, "-");

                  SUMA_TRI_AREA (n0, n1, n2, A);
                  if (abs(A - SO->PolyArea[ji]) > 0.00001) {
                     fprintf (SUMA_STDERR, "Error %s: Failed comparing SUMA_TRI_AREA to SUMA_PolySurf3. %f vs %f Exiting.\n", 
                        FuncName, A, SO->PolyArea[ji]);
                  }else fprintf (SUMA_STDERR, ".");
               }  
            }
         #endif

      }
      if (SO->PolyArea == NULL) {
         fprintf(SUMA_STDERR,"Error %s: Error in SUMA_PolySurf3 or SUMA_TriSurf3v\n", FuncName);
      }
   }
   
   if (DoEL) {
      if (!SOinh) {
         /* create the edge list, it's nice and dandy */
         if (LocalHead) fprintf(SUMA_STDOUT, "%s: Making Edge list ....\n", FuncName); 
         SO->EL = SUMA_Make_Edge_List (SO->FaceSetList, SO->N_FaceSet, SO->N_Node, SO->NodeList);
         if (SO->EL == NULL) {
            fprintf(SUMA_STDERR, "Error %s: Failed in SUMA_Make_Edge_List. Neighbor list will not be created\n", FuncName);
            SO->EL_Inode = NULL;
         } else {
            /* create EL_Inode */
            SO->EL_Inode = SUMA_CreateInode ((void *)SO->EL, SO->idcode_str);
            if (!SO->EL_Inode) {
               fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInode\n", FuncName);
            }
            if (LocalHead) fprintf(SUMA_STDOUT, "%s: Making Node Neighbor list ....\n", FuncName); 
            /* create the node neighbor list */
            SO->FN = SUMA_Build_FirstNeighb (SO->EL, SO->N_Node);   
            if (SO->FN == NULL) {
               fprintf(SUMA_STDERR, "Error %s: Failed in SUMA_Build_FirstNeighb.\n", FuncName);
               SO->FN_Inode = NULL;
            } else {
               /* create FN_Inode */
               SO->FN_Inode = SUMA_CreateInode ((void *)SO->FN, SO->idcode_str);
               if (!SO->FN_Inode) {
                  fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInode\n", FuncName);
               }
            }
         }
      } else {
         if (LocalHead) fprintf(SUMA_STDOUT, "%s: Linking Edge List and First Neighbor Lits ...\n", FuncName);
         SO->EL_Inode = SUMA_CreateInodeLink (SO->EL_Inode, SOinh->EL_Inode);
         if (!SO->EL_Inode) {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInodeLink\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         SO->EL = SOinh->EL;
         SO->FN_Inode = SUMA_CreateInodeLink (SO->FN_Inode, SOinh->FN_Inode);
         if (!SO->FN_Inode) {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInodeLink\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         SO->FN = SOinh->FN;   
      }
   }
   
   if (DoConv) {
      /* calculate convexity */
      if (LocalHead) fprintf(SUMA_STDOUT, "%s: Calculating convexity ...\n", FuncName);
      SO->Cx = SUMA_Convexity   (SO->NodeList, SO->N_Node, SO->NodeNormList, SO->FN);
      if (SO->Cx == NULL) {
         fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Convexity\n", FuncName);
         SO->Cx_Inode = NULL;
      }   
            
      /* flip sign of convexity if it's a SureFit Surface */
      if (SO->FileType == SUMA_SUREFIT) {
         for (i=0; i < SO->N_Node; ++i) {
            SO->Cx[i] = -SO->Cx[i];
         }
      }
      
      {
         float *attr_sm;
         /* smooth estimate twice */
         attr_sm = SUMA_SmoothAttr_Neighb (SO->Cx, SO->N_Node, NULL, SO->FN);
         if (attr_sm == NULL) {
               fprintf(stderr,"Error %s: Failed in SUMA_SmoothAttr_Neighb\n", FuncName);
         }   else {
            SO->Cx = SUMA_SmoothAttr_Neighb (attr_sm, SO->N_Node, SO->Cx, SO->FN);
            if (attr_sm) SUMA_free(attr_sm);
         }
      }
      if (SO->Cx == NULL) {
         fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_SmoothAttr_Neighb\n", FuncName);
         SO->Cx_Inode = NULL;
      } else {
         /* create Cx_Inode */
         SO->Cx_Inode = SUMA_CreateInode ((void *)SO->Cx, SO->idcode_str);
         if (!SO->Cx_Inode) {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInode\n", FuncName);
         }
      }
   } /* DoConv */
   
   if (DoWind){   
      /* check to make sure winding is consistent */
      if (!SUMA_MakeConsistent (SO->FaceSetList, SO->N_FaceSet, SO->EL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_MakeConsistent.\n", FuncName);
      }else {
         if (LocalHead) fprintf(SUMA_STDERR,"%s: Eeeexcellent.\n", FuncName);
      }
   }

   
   if (DoCurv) {
      /* calculate the curvature */
      if (LocalHead) fprintf(SUMA_STDOUT, "%s: Calculating curvature ...\n", FuncName);
      SO->SC = SUMA_Surface_Curvature (SO->NodeList, SO->N_Node, SO->NodeNormList, SO->PolyArea, SO->N_FaceSet, SO->FN, SO->EL);
   }
   
   
   if (DoMF) {
      if (!SOinh) {
         /* determine the MemberFaceSets */
         if (LocalHead) fprintf(SUMA_STDOUT, "%s: Determining MemberFaceSets  ...\n", FuncName);
         SO->MF = SUMA_MemberFaceSets(SO->N_Node, SO->FaceSetList, SO->N_FaceSet, SO->FaceSetDim);
         if (SO->MF->NodeMemberOfFaceSet == NULL) {
            fprintf(SUMA_STDERR,"Error %s: Error in SUMA_MemberFaceSets\n", FuncName);
            SO->MF = NULL;
            SO->MF_Inode = NULL;
         }else {
            SO->MF_Inode = SUMA_CreateInode ((void *)SO->MF, SO->idcode_str);
            if (!SO->MF_Inode) {
               fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInode\n", FuncName);
            }
         }
      } else { /* inherit */
         if (LocalHead) fprintf(SUMA_STDOUT, "%s: Linking Member Facesets ...\n", FuncName);
         SO->MF_Inode  = SUMA_CreateInodeLink (SO->MF_Inode, SOinh->MF_Inode);
         if (!SO->MF_Inode) {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInodeLink\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         SO->MF = SOinh->MF;  
      }
   }

   SUMA_RETURN (YUP);
}

#ifdef SUMA_SurfaceMetrics_STAND_ALONE

void usage_SUMA_SurfaceMetrics ()
   {
      printf ("\n\33[1mUsage: \33[0m SurfaceMetrics <-Metric1> [[-Metric2] ...] \n"
              "\t<-i_TYPE inSurf> [<-sv SurfaceVolume [VolParam for sf surfaces]>]\n"
              "\t[-tlrc] [<-prefix prefix>]\n\n");
      printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \t Mon May 19 15:41:12 EDT 2003\n\n");   
   }

int main (int argc,char *argv[])
{/* Main */
   char FuncName[]={"Main_SUMA_SurfaceMetrics"};
   char  *OutName=NULL, *OutPrefix = NULL, *if_name = NULL, 
         *if_name2 = NULL, *sv_name = NULL, *vp_name = NULL,
         *tlrc_name = NULL;
   SUMA_STRING *MetricList = NULL;
   int i, n1, n2, n1_3, n2_3, kar, nt;
   double edgeL2;
   FILE *fout=NULL;
   SUMA_SO_File_Type iType = SUMA_FT_NOT_SPECIFIED;
   SUMA_SurfaceObject *SO = NULL;   
   SUMA_SFname *SF_name = NULL;
   void *SO_name = NULL;   
   THD_warp *warp=NULL ;
   THD_3dim_dataset *aset=NULL;
   SUMA_Boolean   brk, Do_tlrc, Do_conv, Do_curv, 
                  Do_area, Do_edges, LocalHead = NOPE;  
   
   
	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
   
   if (argc < 4)
       {
          usage_SUMA_SurfaceMetrics ();
          exit (1);
       }
   
   MetricList = SUMA_StringAppend (NULL, NULL);
   kar = 1;
	brk = NOPE;
   Do_tlrc = NOPE;
   Do_conv = NOPE;
   Do_area = NOPE;
   Do_curv = NOPE;
   Do_edges = NOPE;
   OutPrefix = NULL;
   
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_SurfaceMetrics();
          exit (1);
		}
		
		if (!brk && (strcmp(argv[kar], "-i_fs") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -i_fs ");
				exit (1);
			}
			if_name = argv[kar];
         iType = SUMA_FREE_SURFER;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_sf") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 argument after -i_sf");
				exit (1);
			}
			if_name = argv[kar]; kar ++;
         if_name2 = argv[kar];
         iType = SUMA_SUREFIT;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_vec") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 argument after -i_vec");
				exit (1);
			}
			if_name = argv[kar]; kar ++;
         if_name2 = argv[kar];
         iType = SUMA_VEC;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_ply") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -i_ply ");
				exit (1);
			}
			if_name = argv[kar];
         iType = SUMA_PLY;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -prefix ");
				exit (1);
			}
			OutPrefix = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-sv") == 0)) {
         if (iType == SUMA_FT_NOT_SPECIFIED) {
            fprintf (SUMA_STDERR, " -sv option must be preceeded by -i_TYPE option.");
            exit(1);
         }
         kar ++;
			if (iType == SUMA_SUREFIT) {
            if (kar+1 >= argc)  {
		  		   fprintf (SUMA_STDERR, "need 2 argument after -sv (SurfaceVolume and VolumeParent)");
				   exit (1);
			   }
            sv_name = argv[kar]; kar ++;
            vp_name = argv[kar];
         } else {
            if (kar >= argc)  {
		  		   fprintf (SUMA_STDERR, "need argument after -sv ");
				   exit (1);
			   }
			   sv_name = argv[kar];
         }
			brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-tlrc") == 0)) {
         Do_tlrc = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-conv") == 0)) {
         Do_conv = YUP;
         MetricList = SUMA_StringAppend (MetricList, "Convexity "); 
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-area") == 0)) {
         Do_area = YUP;
         MetricList = SUMA_StringAppend (MetricList, "PolyArea "); 
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-curv") == 0)) {
         Do_curv = YUP;
         MetricList = SUMA_StringAppend (MetricList, "Curvature "); 
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-edges") == 0)) {
         Do_edges = YUP;
         MetricList = SUMA_StringAppend (MetricList, "EdgeList "); 
         brk = YUP;
      }
      
      if (!brk) {
			fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }

   /* clean MetricList */
   MetricList = SUMA_StringAppend (MetricList, NULL); 
   
   /* sanity checks */
   if (!MetricList) {
      SUMA_S_Err("No Metrics specified.\nNothing to do.\n");
      exit(1);
   }
   
   if (!if_name) {
      SUMA_S_Err("Input surface not specified.\n");
      exit(1);
   }
   
   if (iType == SUMA_FT_NOT_SPECIFIED) {
      SUMA_S_Err("Input type not recognized.\n");
      exit(1);
   }
   
   if (iType == SUMA_SUREFIT) {
      if (!if_name2) {
         SUMA_S_Err("Input SureFit surface incorrectly specified.\n");
         exit(1);
      }
      if (sv_name && !vp_name) {
         SUMA_S_Err("VolParent must specified with -sv potion for SureFit surfaces. \n");
         exit(1);
      }
   }
   
   if (iType == SUMA_VEC) {
      if (!if_name2) {
         SUMA_S_Err("Input vec surface incorrectly specified.\n");
         exit(1);
      }
   }
   
   /* test for existence of input files */
   if (!SUMA_filexists(if_name)) {
      fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, if_name);
      exit(1);
   }
   
   if (if_name2) {
      if (!SUMA_filexists(if_name2)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, if_name2);
         exit(1);
      }
   }

   if (sv_name) {
      if (!SUMA_filexists(sv_name)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, sv_name);
         exit(1);
      }
   }
   
   if (Do_tlrc && !sv_name) {
      fprintf (SUMA_STDERR,"Error %s: -tlrc must be used with -sv option.\n", FuncName);
      exit(1);
   }
   
   if (vp_name) {
      if (!SUMA_filexists(vp_name)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, vp_name);
         exit(1);
      }
   }

   /* read the surface */
   
   /* prepare the name of the surface object to read*/
   switch (iType) {
      case SUMA_SUREFIT:
         SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
         sprintf(SF_name->name_coord,"%s", if_name);
         sprintf(SF_name->name_topo,"%s", if_name2); 
         if (!vp_name) { /* initialize to empty string */
            SF_name->name_param[0] = '\0'; 
         }
         else {
            sprintf(SF_name->name_param,"%s", vp_name);
         }
         SO_name = (void *)SF_name;
         fprintf (SUMA_STDOUT,"Reading %s and %s...\n", SF_name->name_coord, SF_name->name_topo);
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_SUREFIT, SUMA_ASCII, sv_name);
         break;
      case SUMA_VEC:
         SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
         sprintf(SF_name->name_coord,"%s", if_name);
         sprintf(SF_name->name_topo,"%s", if_name2); 
         SO_name = (void *)SF_name;
         fprintf (SUMA_STDOUT,"Reading %s and %s...\n", SF_name->name_coord, SF_name->name_topo);
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_VEC, SUMA_ASCII, sv_name);
         break;
      case SUMA_FREE_SURFER:
         SO_name = (void *)if_name; 
         fprintf (SUMA_STDOUT,"Reading %s ...\n",if_name);
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_FREE_SURFER, SUMA_ASCII, sv_name);
         break;  
      case SUMA_PLY:
         SO_name = (void *)if_name; 
         fprintf (SUMA_STDOUT,"Reading %s ...\n",if_name);
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_PLY, SUMA_FF_NOT_SPECIFIED, sv_name);
         break;  
      default:
         fprintf (SUMA_STDERR,"Error %s: Bad format.\n", FuncName);
         exit(1);
   }
   
   if (!SO) {
      fprintf (SUMA_STDERR,"Error %s: Failed to read input surface.\n", FuncName);
      exit (1);
   }
   
   if (Do_tlrc) {
      fprintf (SUMA_STDOUT,"Performing talairach transform...\n");

      /* form the tlrc version of the surface volume */
      tlrc_name = (char *) SUMA_calloc (strlen(SO->VolPar->dirname)+strlen(SO->VolPar->prefix)+60, sizeof(char));
      sprintf (tlrc_name, "%s%s+tlrc.HEAD", SO->VolPar->dirname, SO->VolPar->prefix);
      if (!SUMA_filexists(tlrc_name)) {
         fprintf (SUMA_STDERR,"Error %s: %s not found.\n", FuncName, tlrc_name);
         exit(1);
      }
      
      /* read the tlrc header */
      aset = THD_open_dataset(tlrc_name) ;
      if( !ISVALID_DSET(aset) ){
         fprintf (SUMA_STDERR,"Error %s: %s is not a valid data set.\n", FuncName, tlrc_name) ;
         exit(1);
      }
      if( aset->warp == NULL ){
         fprintf (SUMA_STDERR,"Error %s: tlrc_name does not contain a talairach transform.\n", FuncName);
         exit(1);
      }
      
      warp = aset->warp ;
      
      /* now warp the coordinates, one node at a time */
      if (!SUMA_AFNI_forward_warp_xyz(warp, SO->NodeList, SO->N_Node)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_AFNI_forward_warp_xyz.\n", FuncName);
         exit(1);
      }

      
      
   }
   
   /* create the surface label*/
   SO->Label = SUMA_SurfaceFileName (SO, NOPE);
   if (!SO->Label) {
      SUMA_S_Err("Failed to create Label");
      exit(1);
   }

   if (LocalHead) SUMA_Print_Surface_Object (SO, stderr);
   
   /* Now do the deed */
   SUMA_LH (MetricList->s);
   
   if (!SUMA_SurfaceMetrics (SO, MetricList->s, NULL)) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
      exit(1);
   }
   
   SUMA_LH ("Done with Metrics");
   
   /* output time */
   if (!OutPrefix) {
      OutPrefix = SUMA_copy_string(SO->Label);
   }
   
   OutName = (char*) SUMA_malloc((strlen(OutPrefix) + 30) * sizeof(char));
   
   
   if (Do_edges) {
      
      SUMA_S_Note("Writing edges...");
      
      if (!SO->EL) {
         SUMA_S_Err("Edge list not computed.");
         exit(1);
      }
      
      sprintf(OutName, "%s.edges", OutPrefix);
      if (SUMA_filexists(OutName)) {
         SUMA_S_Err("Edge output file exists.\nWill not overwrite.");
         exit(1);
      }
      
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#Edge List\n");
      fprintf (fout,"#eI = Edge Index\n");
      fprintf (fout,"#n1 = Node 1\n");
      fprintf (fout,"#n2 = Node 2\n");
      fprintf (fout,"#nt = Number of triangles containing edge\n"); 
      fprintf (fout,"#eL = Edge Length\n");
      fprintf (fout,"#eI\tn1\tn2\tnt\teL\n\n");
      for (i=0; i < SO->EL->N_EL; ++i) {
         if (SO->EL->ELps[i][2] >= 0) {
            n1 = SO->EL->EL[i][0];
            n2 = SO->EL->EL[i][1];
            nt = SO->EL->ELps[i][2];
            n1_3 = 3 * n1;
            n2_3 = 3 * n2;
            edgeL2 = ( (SO->NodeList[n2_3] - SO->NodeList[n1_3]) * (SO->NodeList[n2_3] - SO->NodeList[n1_3]) ) +
                     ( (SO->NodeList[n2_3+1] - SO->NodeList[n1_3+1]) * (SO->NodeList[n2_3+1] - SO->NodeList[n1_3+1]) ) +
                     ( (SO->NodeList[n2_3+2] - SO->NodeList[n1_3+2]) * (SO->NodeList[n2_3+2] - SO->NodeList[n1_3+2]) ); 
                     
            fprintf (fout,"%d\t%d\t%d\t%d\t%f\n",
                  i, n1, n2, nt, sqrt(edgeL2));
                  
         }   
      }
      fclose(fout); fout = NULL;
      
   }
   
   if (Do_area) {
      SUMA_S_Note("Writing areas...");
      
      if (!SO->PolyArea) {
         SUMA_S_Err("Areas not computed");
         exit(1);
      }  
      
      sprintf(OutName, "%s.area", OutPrefix);
      if (SUMA_filexists(OutName)) {
         SUMA_S_Err("Area output file exists.\nWill not overwrite.");
         exit(1);
      }
      
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }
      
      fprintf (fout,"#FaceSet Area\n");
      fprintf (fout,"#fI = FaceSet Index\n");
      fprintf (fout,"#fA = FaceSet Area\n");
      fprintf (fout,"#fI\t#fA\n\n");
      
      for (i=0; i < SO->N_FaceSet; ++i) {
         fprintf (fout,"%d\t%f\n", i, SO->PolyArea[i]);
      }  
      
      fclose(fout); fout = NULL;
   }
   
   if (Do_curv) {
      SUMA_S_Note("Writing curvatures ...");
      
      if (!SO->SC) {
         SUMA_S_Err("Curvatures not computed");
         exit(1);
      }
      
      sprintf(OutName, "%s.curv", OutPrefix);
      if (SUMA_filexists(OutName)) {
         SUMA_S_Err("Curvature output file exists.\nWill not overwrite.");
         exit(1);
      }
      
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }  
      
      fprintf (fout,"#Curvature\n");
      fprintf (fout,"#nI = Node Index\n");
      fprintf (fout,"#T1 = 1 x 3 vector of 1st principal direction of surface\n");
      fprintf (fout,"#T2 = 1 x 3 vector of 2nd principal direction of surface\n");
      fprintf (fout,"#Kp1 = curvature along T1\n");
      fprintf (fout,"#Kp2 = curvature along T2\n");
      fprintf (fout,"#nI\tT1[0]\tT1[1]\tT1[2]\tT2[0]\tT2[1]\tT2[2]\tKp1\tKp2\n\n");
      
      for (i=0; i < SO->N_Node; ++i) {
         fprintf (fout,"%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\n",
            i, SO->SC->T1[i][0], SO->SC->T1[i][1], SO->SC->T1[i][2], 
            SO->SC->T2[i][0], SO->SC->T2[i][1], SO->SC->T2[i][2],
            SO->SC->Kp1[i], SO->SC->Kp2[i] );
      }
      
      fclose(fout); fout = NULL;
   }
   
   if (Do_conv) {
      SUMA_S_Note("Writing convexities ...");
      
      if (!SO->Cx) {
         SUMA_S_Err("Convexities not computed");
         exit(1);
      }
      
      sprintf(OutName, "%s.conv", OutPrefix);
      if (SUMA_filexists(OutName)) {
         SUMA_S_Err("Convexities output file exists.\nWill not overwrite.");
         exit(1);
      }
      
      fout = fopen(OutName,"w");
      if (!fout) {
         SUMA_S_Err("Failed to open file for writing.\nCheck your permissions.\n");
         exit(1);
      }  
      
      fprintf (fout,"#Convexity\n");
      fprintf (fout,"nI = Node Index\n");
      fprintf (fout,"C = Convexity\n");
      fprintf (fout,"#nI\tC\n\n");
      
      for (i=0; i < SO->N_Node; ++i) {
         fprintf (fout,"%d\t%f\n", i, SO->Cx[i]);
      }
      
      fclose(fout); fout = NULL;
   }   
   
   SUMA_LH("Clean up");
   /* clean up */
   if (MetricList) SUMA_free(MetricList);
   if (OutPrefix) SUMA_free(OutPrefix);
   if (OutName) SUMA_free(OutName);   
   if (SO) SUMA_Free_Surface_Object(SO);
   
   return(0);
} /* Main */
#endif

#ifdef SUMA_Read_SpecFile_STAND_ALONE

void usage_SUMA_Read_SpecFile ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA_Read_SpecFile <fname> \n");
          printf ("\t <fname> Filename of Surface Specs file\n");
          printf ("To compile: \ngcc -DSUMA_Read_SpecFile_STAND_ALONE -Wall -o SUMA_Load_Surface_Object  SUMA_Load_Surface_Object.c ");
          printf ("SUMA_lib.a libmri.a -I/usr/X11R6/include -I./ -L/usr/lib -L/usr/X11R6/lib -lm \n");
          printf ("\t\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \n");
          exit (0);
  }/*Usage*/
  
int main (int argc,char *argv[])
{/* Main */
   char FuncName[]={"Main_SUMA_Read_SpecFile"};
   SUMA_SurfSpecFile Spec;   
   
   if (argc < 2)
       {
          usage_SUMA_Read_SpecFile ();
          exit (1);
       }

   /* allocate space for CommonFields structure */
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
      exit(1);
   }
   

   if (!SUMA_Read_SpecFile (argv[1], &Spec)) {
      fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
      if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
      return (1);
   }   else    {      
      if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
      return (0);
   }
} /* Main */

#endif

#ifdef SUMA_Load_Surface_Object_STAND_ALONE

void usage_SUMA_Load_Surface_Object_STAND_ALONE ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA_Load_Surface_Object <SurfName> [<Type> <format>]\n");
          printf ("\t <SurfName> Filename of Surface Object\n");
          printf ("\t <Type>: 2 (hard coded at the moment for SUMA_INVENTOR_GENERIC)\n");
          printf ("\t <format>: 0 (hard coded at the moment for SUMA_ASCII\n"); 
          printf ("To compile: \ngcc -DSUMA_Load_Surface_Object_STAND_ALONE -Wall -o SUMA_Load_Surface_Object SUMA_Load_Surface_Object.c ");
          printf ("SUMA_lib.a  -I/usr/X11R6/include -I./ -L/usr/lib -L/usr/X11R6/lib -lm \n");
          printf ("-lGL -lGLU -lGLw -lXmu -lXm -lXt -lXext -lX11 -lMesaGLw -lMesaGLw\n");
          printf ("\t\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \tWed Jan 23 15:18:12 EST 2002 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   char FuncName[100]; 
   SUMA_SurfaceObject *SO;
   
   /* allocate space for CommonFields structure */
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
      exit(1);
   }
   
   /* initialize Main function name for verbose output */
   sprintf (FuncName,"SUMA_Load_Surface_Object-Main-");
   
   
   
   if (argc < 2)
       {
          usage_SUMA_Load_Surface_Object_STAND_ALONE ();
          exit (1);
       }
   
   SO = SUMA_Load_Surface_Object((void *)argv[1], SUMA_INVENTOR_GENERIC, SUMA_ASCII, NULL);
   SUMA_Print_Surface_Object (SO, stdout);
   SUMA_Free_Surface_Object (SO);

   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

   return (0);
}/* Main */
#endif

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* check if recognizable type */
   switch (SO->FileType) {
      case SUMA_INVENTOR_GENERIC:
      case SUMA_FT_NOT_SPECIFIED:
         SUMA_error_message(FuncName, "SO_FileType not specified", 0);
         SUMA_RETURN (NULL);
         break;
      case SUMA_VEC:
         if (MitPath) nalloc = strlen(SO->Name_coord.Path) + strlen(SO->Name_coord.FileName) \
                           +    strlen(SO->Name_topo.Path) + strlen(SO->Name_topo.FileName) + 5;
         else nalloc = strlen(SO->Name_coord.FileName) \
                     +   strlen(SO->Name_topo.FileName) + 5;
         break;
      case SUMA_FREE_SURFER:
         if (MitPath) nalloc = strlen(SO->Name.Path) + strlen(SO->Name.FileName) + 5;
         else nalloc = strlen(SO->Name.FileName) + 5;
         break;
      case SUMA_SUREFIT:
         if (MitPath) nalloc = strlen(SO->Name_coord.Path) + strlen(SO->Name_coord.FileName) \
                           +    strlen(SO->Name_topo.Path) + strlen(SO->Name_topo.FileName) + 5;
         else nalloc = strlen(SO->Name_coord.FileName) \
                     +   strlen(SO->Name_topo.FileName) + 5;
         break;
      case SUMA_PLY:
         if (MitPath) nalloc = strlen(SO->Name.Path) + strlen(SO->Name.FileName) + 5;
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
      case SUMA_INVENTOR_GENERIC:
      case SUMA_FREE_SURFER:
         if (MitPath) sprintf(Name,"%s%s", SO->Name.Path, SO->Name.FileName);
         else sprintf(Name,"%s", SO->Name.FileName);
         break;
      case SUMA_SUREFIT:
         if (MitPath) sprintf(Name,"%s%s__%s%s", SO->Name_coord.Path, SO->Name_coord.FileName, \
                              SO->Name_topo.Path, SO->Name_topo.FileName);
         else sprintf(Name,"%s__%s", SO->Name_coord.FileName, SO->Name_topo.FileName);
         break;
      case SUMA_VEC:
         if (MitPath) sprintf(Name,"%s%s__%s%s", SO->Name_coord.Path, SO->Name_coord.FileName, \
                              SO->Name_topo.Path, SO->Name_topo.FileName);
         else sprintf(Name,"%s__%s", SO->Name_coord.FileName, SO->Name_topo.FileName);
         break;
      case SUMA_FT_NOT_SPECIFIED:
         break;
      case SUMA_PLY:
         if (MitPath) sprintf(Name,"%s%s", SO->Name.Path, SO->Name.FileName);
         else sprintf(Name,"%s", SO->Name.FileName);
         break;
   } 
   SUMA_RETURN (Name);
   
}

SUMA_SO_SIDE SUMA_GuessSide(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_GuessSide"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   switch (SO->FileType) {
      case SUMA_INVENTOR_GENERIC:
         break;
      case SUMA_FREE_SURFER:
         if (SUMA_iswordin (SO->Name.FileName, "lh")) {
            SUMA_RETURN(SUMA_LEFT);
         } else if (SUMA_iswordin (SO->Name.FileName, "rh")) {
                     SUMA_RETURN(SUMA_RIGHT);
                  }
         break;
      case SUMA_SUREFIT:
         if (SUMA_iswordin (SO->Name_coord.FileName, "left")) {
            SUMA_RETURN(SUMA_LEFT);
         } else if (SUMA_iswordin (SO->Name_coord.FileName, "right")) {
                        SUMA_RETURN(SUMA_RIGHT);
                  }
         break;
      case SUMA_VEC:
         if (SUMA_iswordin (SO->Name_coord.FileName, "lh") ||
             SUMA_iswordin (SO->Name_coord.FileName, "left")) {
               SUMA_RETURN(SUMA_LEFT);
         } else if (SUMA_iswordin (SO->Name_coord.FileName, "rh") ||
                     SUMA_iswordin (SO->Name_coord.FileName, "right")) {
                     SUMA_RETURN(SUMA_RIGHT);
               }
         break;
      case SUMA_FT_NOT_SPECIFIED:
         break;
      case SUMA_PLY:
         if (SUMA_iswordin (SO->Name.FileName, "lh") ||
             SUMA_iswordin (SO->Name.FileName, "left")) {
               SUMA_RETURN(SUMA_LEFT);
         } else if (SUMA_iswordin (SO->Name.FileName, "rh") ||
                  SUMA_iswordin (SO->Name.FileName, "right")) { 
                     SUMA_RETURN(SUMA_RIGHT);
               }
         break;
   } 
   
   SUMA_RETURN (SUMA_NO_SIDE);
}
