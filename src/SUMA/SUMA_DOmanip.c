#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 
extern SUMA_DO *SUMAg_DOv;   
extern int SUMAg_N_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv;

/*! functions dealing with Drawable Object Manipulation */

/*!
   This function Links one Inode to another.
   This is different from CreateInode in that no new node is being allocated for, only a link is created.
   SUMA_CreateInodeLink (SUMA_INODE * FromIN, SUMA_INODE *ToIN)
   
   FromIN = SUMA_CreateInodeLink (FromIN, ToIN);
   
   \param FromIN (SUMA_INODE *) this is the pointer to the IDnode wich will be linked to ToIN.
          As silly as this sounds, this should be a pointer to NULL otherwise the link will not
          be established. The use of this function will help reduce the risk of linking pointers
          that are pointing to allocated space.
   \param ToIN (SUMA_INODE *) pointer where the link is going
   \ret FromIN (SUMA_INODE *) if all is well, FromIN = ToIN
      otherwise, NULL is returned
      
      All this function does is check the FromIN is NULL and ToIN isn't and then it calls SUMA_AddLink (ToIN)
   \sa SUMA_BreakInodeLink
   \sa SUMA_isInodeLink
*/
SUMA_INODE *SUMA_CreateInodeLink (SUMA_INODE * FromIN, SUMA_INODE *ToIN)
{
   static char FuncName[] = {"SUMA_CreateInodeLink"};
   
   SUMA_ENTRY;

   if (FromIN) {
      fprintf (SUMA_STDERR,"Error %s: FromIN Inode is not NULL. \n\tFromIN pointer is left undisturbed.\n", FuncName);
      SUMA_RETURN(FromIN);
   }
   if (!ToIN) {
      fprintf (SUMA_STDERR,"Error %s: ToIN is NULL.\n\t Can't link to NULL, returning NULL.\n", FuncName);
      SUMA_RETURN(NULL); 
   }
   
   /* add a link to ToIN */
   if (!SUMA_AddLink (ToIN)) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_AddLink.\n", FuncName);
      SUMA_RETURN(NULL);
   }
   
   /* now return the pointer to be linked to */
   SUMA_RETURN(ToIN);

}

/*! 
   This function determines if an Inode in a SO is a link or not.
   an Inode is a link if IN->ParentIDcode and HolderIDcode are different. 
   The function is nothing more than a strcmp.
   
   SUMA_Boolean SUMA_isInodeLink (SUMA_INODE *IN, const char *HolderIDcode)

    returns NOPE if IN == NULL or IN->ParentIDcode != HolderIDcode
*/
SUMA_Boolean SUMA_isInodeLink (SUMA_INODE *IN, const char *HolderIDcode)
{
   static char FuncName[] = {"SUMA_isInodeLink"};
   
   SUMA_ENTRY;

   if (!IN) {
      fprintf (SUMA_STDERR, "Warning %s: IN is null.\n", FuncName); 
      SUMA_RETURN(NOPE);
   } 
   if (!strcmp(IN->ParentIDcode, HolderIDcode)) SUMA_RETURN(YUP);
      
   SUMA_RETURN(NOPE);
}

/*! 
   function to remove the link of one IDnode to the next 
   SUMA_INODE * SUMA_BreakInodeLink (SUMA_INODE *IN, const char *HolderIDcode);
   \param IN (SUMA_INODE *) the linked inode
   \param HolderIDcode (const char *) the ID code that holds/contains IN
   \ret NULL if the link was broken or IN == NULL
      IN if IN is not a link but an actuak Inode (meaning IN->ParentIDcode == HolderIDcode
      

*/   
SUMA_INODE * SUMA_BreakInodeLink (SUMA_INODE *IN, const char *HolderIDcode) 
{
   static char FuncName[] = {"SUMA_BreakInodeLink"};

   SUMA_ENTRY;

   if (!IN) {
      fprintf (SUMA_STDERR, "Warning %s: IN is null, nothing to do.\n", FuncName); 
      SUMA_RETURN(NULL);
   }
   if (!SUMA_isInodeLink (IN, HolderIDcode)) {
      fprintf (SUMA_STDERR, "Error %s: Inode IN is not a link. Nothing done.\n", FuncName);
      SUMA_RETURN(IN);
   } 
   
   /* release the link */
   if (SUMA_ReleaseLink (IN) < 0) {
      fprintf (SUMA_STDERR, "Error %s: IN has no links. Nothing done.\n", FuncName);
      SUMA_RETURN(IN);
   }
   
   /* OK, link released, not return NULL */
   SUMA_RETURN(NULL);
}

/*! 
   This function decrements the N_link field in IN by 1 and returns the resultant N_link value
   a -1 is returned if IN->N_link = 0 or IN == NULL


   ans = SUMA_ReleaseLink (IN);

   \param IN (SUMA_INODE *) pointer to SUMA_INODE structure
   \ret ans (int) value of IN->N_link
   
   \sa SUMA_AddLink
*/
int SUMA_ReleaseLink (SUMA_INODE * IN) 
{
   static char FuncName[]={"SUMA_ReleaseLink"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!IN) {
      /* This typically happens when A link was never created in the first place.
      It used to be an error message but now it is just a warning because some
      programs compute things like SO->Cx without creating an inode with it ...*/
      fprintf (SUMA_STDERR,"Warning %s: Inode is null. Returning -1.\n", FuncName);
      SUMA_RETURN(-1);
   }
   if (!IN->N_link) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: No links. Returning -1.\n", FuncName);
      /* You do not want to return a 0 because freeing is done when no links remain 
      THIS STUPID SYSTEM SHOULD BE ELIMINATED IN FAVOR OF THE METHOD USED FOR OVERLAYS*/
      SUMA_RETURN(-1);
   }
   else {
      IN->N_link--;
      SUMA_RETURN(IN->N_link);
   }
}
   
/*! 
   This function increments the N_link field in IN by 1 and returns the resultant N_link value
   a zero is returned in case of an error


   ans = SUMA_AddLink (IN);

   \param IN (SUMA_INODE *) pointer to SUMA_INODE structure
   \ret ans (int) value of IN->N_link
   \sa SUMA_ReleaseLink
*/
int SUMA_AddLink (SUMA_INODE * IN) 
{
   static char FuncName[]={"SUMA_AddLink"};

   SUMA_ENTRY;

   if (!IN) {
      fprintf (SUMA_STDERR,"Error %s: Inode is null.\n", FuncName);
      
      SUMA_RETURN(0);
   } else {
      IN->N_link++;
      SUMA_RETURN(IN->N_link);
   }
}

/*! 
   Function to create a SUMA_INODE structure 
   ans = SUMA_CreateInode (data, ParentIDcode);
   
   \param data (void *) pointer to data location
   \param ParentIDcode (char[SUMA_IDCODE_LENGTH]) containing the IDcode of the creator of the data
   \ret ans (SUMA_INODE *) pointer to SUMA_INODE for data
      NULL if error is encountered
*/

SUMA_INODE *SUMA_CreateInode (void *data, char *ID)
{
   static char FuncName[]={"SUMA_CreateInode"};
   SUMA_INODE *IN;
   
   SUMA_ENTRY;

   IN = (SUMA_INODE *)SUMA_malloc (sizeof(SUMA_INODE));
   if (IN == NULL) {
      fprintf (SUMA_STDERR,"Error %s: failed to allocate for Inode.\n", FuncName);
      SUMA_RETURN(NULL);
   }
   
   IN->data = data;
   strcpy (IN->ParentIDcode, ID);
   IN->N_link = 0;
   
   SUMA_RETURN(IN);
}

/*!
Create a Displayable Object data structure 
*/
SUMA_DO *SUMA_Alloc_DisplayObject_Struct (int N)
{
   static char FuncName[]={"SUMA_Alloc_DisplayObject_Struct"};
   SUMA_DO *dov;
   
   SUMA_ENTRY;

   dov = (SUMA_DO *)SUMA_malloc(sizeof(SUMA_DO)*N);
   if (dov == NULL) {
      SUMA_alloc_problem("SUMA_Alloc_DisplayObject_Struct: could not allocate memory for SO");
   }
   SUMA_RETURN(dov);
}/*SUMA_Alloc_DisplayObject_Struct*/


/*!
Free a Displayable Object data structure 
*/
SUMA_Boolean SUMA_Free_Displayable_Object (SUMA_DO *dov)
{
   static char FuncName[]={"SUMA_Free_Displayable_Object"};

   SUMA_ENTRY;

   switch (dov->ObjectType) {
      case SO_type:
         if (!SUMA_Free_Surface_Object ((SUMA_SurfaceObject *)dov->OP)) {
            fprintf(SUMA_STDERR,"Error SUMA_Free_Displayable_Object, could not free surface\n");
         }
         break;
      case ROIdO_type:
         if (!SUMA_freeDrawnROI ((SUMA_DRAWN_ROI *)dov->OP)) {
            fprintf(SUMA_STDERR,"Error SUMA_freeDrawnROI, could not free  ROI.\n");
         }
         break;
      case ROIO_type:
         if (!SUMA_freeROI ((SUMA_ROI *)dov->OP)) {
            fprintf(SUMA_STDERR,"Error SUMA_freeROI, could not free  ROI.\n");
         }
         break;
      case ONBV_type:
      case NBV_type:
      case OLS_type:
      case LS_type:
         SUMA_free_SegmentDO ((SUMA_SegmentDO *)dov->OP);
         break;
      case AO_type:
         SUMA_Free_Axis((SUMA_Axis*)dov->OP);
         break;
      case GO_type:
         fprintf(SUMA_STDERR,"Error SUMA_Free_Displayable_Object, Not trained to free GO objects\n");
         break;
      case type_not_set:
      case no_type:
         fprintf(SUMA_STDERR,"Error SUMA_Free_Displayable_Object, no free no_type or type_not_set\n");
         break;
      case NBSP_type:
      case SP_type:
         SUMA_free_SphereDO ((SUMA_SphereDO *)dov->OP);
         break;
      case PL_type:
         SUMA_free_PlaneDO ((SUMA_PlaneDO *)dov->OP);
         break;
      case NIDO_type:
         SUMA_free_NIDO((SUMA_NIDO*)dov->OP);
         
   }   

   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_Free_Displayable_Object_Vect (SUMA_DO *dov, int N)
{
   static char FuncName[] = {"SUMA_Free_Displayable_Object_Vect"};
   int i;
   SUMA_Boolean Ret = YUP;
   
   SUMA_ENTRY;

   for (i=0; i < N; ++i) {
      if (&dov[i] != NULL) {
         Ret = Ret * SUMA_Free_Displayable_Object (&dov[i]);
      }
   }

   if (dov) SUMA_free(dov);
   SUMA_RETURN(Ret);

}   

/*!
   Find a DO's index  by idcodestring 
*/
int SUMA_FindDOi_byID(SUMA_DO *dov, int N_dov, char *idcode_str)
{
   static char FuncName[] = {"SUMA_FindDOi_byID"};
   int i;
   void *op;
   SUMA_ALL_DO *ado=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dov || !idcode_str) {
      SUMA_RETURN(-1);
   }
    
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType > no_type) {
         ado = (SUMA_ALL_DO *)dov[i].OP;
         SUMA_LHv("ado %p: Object %d/%d type: %d\n", 
                  ado, i, N_dov, dov[i].ObjectType); 
         SUMA_LHv("ado->idcode_str= %s\n", 
                  SUMA_CHECK_NULL_STR(ado->idcode_str));
         SUMA_LHv("idcode_str= %s\n", 
                  SUMA_CHECK_NULL_STR(idcode_str)); 
         if (ado->idcode_str && strcmp(ado->idcode_str, idcode_str) == 0) {
            SUMA_RETURN(i);
         } 
      } else {
         SUMA_SL_Warn("Strange, no type for DO");
      }
   }
   if (LocalHead) SUMA_Show_DOv (dov, N_dov, NULL); 
   SUMA_RETURN(-1);
}
/*!
Add a displayable object to dov
*/
SUMA_Boolean SUMA_AddDO(SUMA_DO *dov, int *N_dov, void *op, SUMA_DO_Types DO_Type, SUMA_DO_CoordType DO_CoordType)
{
   static char FuncName[] = {"SUMA_AddDO"};
   SUMA_ALL_DO *ado=NULL;
   static int nm=0;
   void *eo=NULL;
   int ieo;
   
   SUMA_ENTRY;

   ado = (SUMA_ALL_DO *)op;
   if (!ado->idcode_str) {
      SUMA_error_message (FuncName, "Need an idcode_str for do",0);
      SUMA_RETURN(NOPE);
   }
   
   /* Does that baby exist? */
   if ((ieo = SUMA_FindDOi_byID(dov, *N_dov, ado->idcode_str)) >= 0) {
      if (DO_Type == SO_type) {
         SUMA_SLP_Err("Surface exists, cannot be replaced this way.");
         SUMA_RETURN(NOPE);
      }
      if (!(nm % 300)) {
         SUMA_SLP_Note("Object exists and will be replaced.\nMessage shown intermittently");
      }
      ++nm;
      /* free olde one */
      if (!SUMA_Free_Displayable_Object(&(dov[ieo]))) {
         SUMA_SL_Err("Failed to free displayable object");
         SUMA_RETURN(NOPE);
      }
      dov[ieo].OP = op;
      dov[ieo].ObjectType = DO_Type;
      dov[ieo].CoordType = DO_CoordType;
   } else {
      /* Addington */
      /* make sure you did not exceed allocated space */
      if (*N_dov >= SUMA_MAX_DISPLAYABLE_OBJECTS) {
         SUMA_error_message (FuncName, "Reached limit of DOv storage",0);
         SUMA_RETURN(NOPE);
      }
      dov[*N_dov].OP = op;
      dov[*N_dov].ObjectType = DO_Type;
      dov[*N_dov].CoordType = DO_CoordType;
      *N_dov = *N_dov+1;
   
   }
   
   
   
   
   SUMA_RETURN(YUP);
}

/*!
   \brief Remove a displayable object from dov
   success = SUMA_RemoveDO(dov, N_dov, op, Free_op);
   
   \param dov (SUMA_DO*) vector containing displayable objects
   \param N_dov (int *) number of elements in dov
   \param op (void *) pointer to object sought
   \param Free_op (SUMA_Boolean) Flag for freeing space allocated for op's data.
         Freeing is done via SUMA_Free_Displayable_Object()
   \return success (SUMA_Boolean)  flag. 
*/
SUMA_Boolean SUMA_RemoveDO(SUMA_DO *dov, int *N_dov, void *op, SUMA_Boolean Free_op)
{
   static char FuncName[] = {"SUMA_RemoveDO"};
   int i;
   SUMA_Boolean LocalHead = NOPE, Found=NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   for (i=0; i<*N_dov; ++i) {
      if (dov[i].OP == op) {
         Found = YUP;
         if (LocalHead) fprintf (SUMA_STDERR,"%s: found object. Removing it from dov.\n", FuncName);
         if (Free_op) {
            if (LocalHead) fprintf (SUMA_STDERR,"%s: Freeing object.\n", FuncName);
            if (!SUMA_Free_Displayable_Object (&dov[i])) {
               SUMA_SLP_Crit("Failed to free displayable object.");
               SUMA_RETURN(NOPE);
            }
         }
         *N_dov = *N_dov-1;
         dov[i].OP = dov[*N_dov].OP;
         dov[i].ObjectType = dov[*N_dov].ObjectType;
         dov[i].CoordType = dov[*N_dov].CoordType;
      }
   }

   
   if (Found) {
      SUMA_RETURN(YUP);
   } else {
      SUMA_RETURN(NOPE);
   }
}

/*!
Register a DO with surface viewer RegisteredDO vector 
if dov_id is present in cSV, nothing is done
if not found then dov_id is registered at the end of cSV->RegisteredDO 

When a DO is registered, a ColorList is created if the DO is a surface object.
and N_DO is updated
*/
SUMA_Boolean SUMA_RegisterDO(int dov_id, SUMA_SurfaceViewer *cSV)
{
   int i;
   static char FuncName[]={"SUMA_RegisterDO"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead && SUMA_WhichSV(cSV, SUMAg_SVv, SUMA_MAX_SURF_VIEWERS) != 0) {
      fprintf(SUMA_STDERR,"%s: Muted for viewer[%c]\n", FuncName, 65+SUMA_WhichSV(cSV, SUMAg_SVv, SUMA_MAX_SURF_VIEWERS) );
      /* turn off the LocalHead, too much output*/
      LocalHead = NOPE;
   }  
    
   /* check to see if dov_id exists */
   i = 0;
   while (i < cSV->N_DO) {
      if (cSV->RegisteredDO[i] == dov_id) {
         /* found do nothing, return */
         SUMA_RETURN(YUP);
      }
      ++i;
   }
   cSV->RegisteredDO[cSV->N_DO] = dov_id;
   cSV->N_DO += 1;
   
   /* Now add the ColorList, if DO is a surface object */
   if (SUMA_isSO(SUMAg_DOv[dov_id])) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Adding color list...\n", FuncName);
      /* add the ColorList */
      if (!SUMA_FillColorList (cSV, (SUMA_SurfaceObject *)SUMAg_DOv[dov_id].OP)) {
         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_FillColorList.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
   }
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Back from SUMA_FillColorList. (%s/%d).\n", \
         FuncName, cSV->ColList[0].idcode_str, cSV->N_ColList);
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: RegisteredDO is now:\n", FuncName);
      for (i=0; i< cSV->N_DO; ++i) {
         fprintf(SUMA_STDERR,"RegisteredDO[%d] = %d\t", i, cSV->RegisteredDO[i]);
      }
      fprintf(SUMA_STDERR,"\n");
   }

   /* update the title bar */
   SUMA_UpdateViewerTitle(cSV);   

   SUMA_RETURN(YUP); 
}
/*!
remove DO with I.D. dov_id from RegisteredDO list of that current viewer
removal of dov_id element is done by replacing it with the last entry in RegisteredDO
list. If not found, nothing happens.
*/
SUMA_Boolean SUMA_UnRegisterDO(int dov_id, SUMA_SurfaceViewer *cSV)
{
   int i;
   static char FuncName[]={"SUMA_UnRegisterDO"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   /* check to see if dov_id exists */
   i = 0;
   while (i < cSV->N_DO) {
      if (cSV->RegisteredDO[i] == dov_id) {
         /* found, replace it by the last in the list */
         cSV->RegisteredDO[i] = cSV->RegisteredDO[cSV->N_DO-1];
         /*remove the last element of the list */
         cSV->RegisteredDO[cSV->N_DO-1] = 0;
         cSV->N_DO -= 1; 
         
         /* empty the ColorList for this surface */
         if (SUMA_isSO(SUMAg_DOv[dov_id])) {
            SUMA_SurfaceObject *SO = NULL;
            if (LocalHead) fprintf (SUMA_STDERR,"%s: Emptying ColorList ...\n", FuncName);
            SO = (SUMA_SurfaceObject *)SUMAg_DOv[dov_id].OP;
            if (!SUMA_EmptyColorList (cSV, SO->idcode_str)) {
               fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_EmptyColorList\n", FuncName);
               SUMA_RETURN(NOPE);
            }
         }
         
         if (LocalHead) {
            fprintf (SUMA_STDERR,"%s: RegisteredDO is now:\n", FuncName);
            for (i=0; i< cSV->N_DO; ++i) {
               fprintf(SUMA_STDERR,"RegisteredDO[%d] = %d\t", i, cSV->RegisteredDO[i]);
            }
            fprintf(SUMA_STDERR,"\n");
         }

         /* update the title bar */
         SUMA_UpdateViewerTitle(cSV);   


         SUMA_RETURN(YUP);
      }
      ++i;
   }
   /* Not found, nothing happens */
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: RegisteredDO is now:\n", FuncName);
      for (i=0; i< cSV->N_DO; ++i) {
         fprintf(SUMA_STDERR,"RegisteredDO[%d] = %d\t", i, cSV->RegisteredDO[i]);
      }
      fprintf(SUMA_STDERR,"\n");
   }
   
   SUMA_RETURN(YUP); 
}

char *SUMA_DOv_Info (SUMA_DO *dov, int N_dov, int detail)
{
   static char FuncName[]={"SUMA_DOv_Info"};
   int i;
   SUMA_COL_TYPE ctp;
   char *s=NULL, stmp[200];
   SUMA_SurfaceObject *so_op=NULL;   
   SUMA_STRING *SS=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (dov) {
      SS = SUMA_StringAppend_va(SS, "\nDOv contents (%d elements):\n", N_dov);
      for (i=0; i < N_dov; ++i) {
         switch (dov[i].ObjectType) {
            case SO_type:
               so_op = (SUMA_SurfaceObject *)dov[i].OP;
               #if 0
               if (so_op->FileType != SUMA_SUREFIT) {
                  SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tName: %s/%s\n\tType: %d, Axis Attachment %d\n",\
                     i,  SUMA_CHECK_NULL_STR(so_op->Name.Path), SUMA_CHECK_NULL_STR(so_op->Name.FileName),\
                     dov[i].ObjectType, dov[i].CoordType);
               } else {
                  SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tNameCoord: %s/%s\n\tNameTopo: %s/%s\n\tType: %d, Axis Attachment %d\n",\
                     i, SUMA_CHECK_NULL_STR(so_op->Name_coord.Path), SUMA_CHECK_NULL_STR(so_op->Name_coord.FileName),\
                     so_op->Name_topo.Path, so_op->Name_topo.FileName,\
                     dov[i].ObjectType, dov[i].CoordType);
               } 
               #else
                  SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tName: %s\n\tType: %d, Axis Attachment %d\n",\
                     i,  SUMA_CHECK_NULL_STR(so_op->Label), \
                     dov[i].ObjectType, dov[i].CoordType);
               #endif  
               break;
            case AO_type:
               {
                  SUMA_Axis* ao;
                  ao = (SUMA_Axis*) dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tAxis Object\n\tType: %d, Axis Attachment %d\n", i,dov[i].ObjectType, dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,"\tName: %s\n\tidcode: %s\n", ao->Label, ao->idcode_str);
               }
               break;
            case OLS_type:
               {
                  SUMA_SegmentDO *sdo=NULL;

                  sdo = (SUMA_SegmentDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tOriented Line Segment Object\n\tType: %d, Axis Attachment %d\n", i,dov[i].ObjectType, dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,"\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case ONBV_type:
               {
                  SUMA_SegmentDO *sdo=NULL;

                  sdo = (SUMA_SegmentDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tNode-Based Ball-Vector\n\tType: %d, Axis Attachment %d\n", i,dov[i].ObjectType, dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,"\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case NBV_type:
               {
                  SUMA_SegmentDO *sdo=NULL;

                  sdo = (SUMA_SegmentDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tNode-Based Vector\n\tType: %d, Axis Attachment %d\n", i,dov[i].ObjectType, dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,"\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case LS_type:
               {
                  SUMA_SegmentDO *sdo=NULL;

                  sdo = (SUMA_SegmentDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tLine Segment Object\n\tType: %d, Axis Attachment %d\n", i,dov[i].ObjectType, dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,"\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case NBSP_type:
               {
                  SUMA_SphereDO *sdo=NULL;

                  sdo = (SUMA_SphereDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tNode-Based Sphere Object\n\tType: %d, Axis Attachment %d\n", i,dov[i].ObjectType, dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,"\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case SP_type:
               {
                  SUMA_SphereDO *sdo=NULL;

                  sdo = (SUMA_SphereDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tSphere Object\n\tType: %d, Axis Attachment %d\n", i,dov[i].ObjectType, dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,"\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case PL_type:
               {
                  SUMA_PlaneDO *pdo=NULL;

                  pdo = (SUMA_PlaneDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tPlane Object\n\tType: %d, Axis Attachment %d\n", i,dov[i].ObjectType, dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,"\tLabel: %s\n\tidcode: %s\n", pdo->Label, pdo->idcode_str);

               }
               break;
            case ROIdO_type:
               {
                  SUMA_DRAWN_ROI *dROI = NULL;

                  dROI = (SUMA_DRAWN_ROI *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tLine Segment Object\n\tType: %d, Axis Attachment %d\n", i,dov[i].ObjectType, dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,"\tLabel: %s\n\tidcode: %s\n", dROI->Label, dROI->idcode_str);
               }  
               break;
            case ROIO_type:
               {
                  SUMA_ROI *ROI = NULL;

                  ROI = (SUMA_ROI *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tLine Segment Object\n\tType: %d, Axis Attachment %d\n", i,dov[i].ObjectType, dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,"\tLabel: %s\n\tidcode: %s\n", ROI->Label, ROI->idcode_str);
               }  
               break;
            case GO_type:
               SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tGrid Object\n", i);
               break;
            default:
               SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tUnknown Type!\n", i);
               break;
         }
      }
      
   } else {
      SS = SUMA_StringAppend(SS, "NULL DO.");
   }
   
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(s);
}

/*!
print out the data contained in dov 
*/
void SUMA_Show_DOv (SUMA_DO *dov, int N_dov, FILE *Out)
{
   int i;
   char *si=NULL;
   static char FuncName[]={"SUMA_Show_DOv"};
   
   SUMA_ENTRY;

   if (Out == NULL) Out = stdout;
   
   si = SUMA_DOv_Info(dov, N_dov, 0);
   
   fprintf(Out,"%s\n", si);
   
   if (si) SUMA_free(si); si = NULL;
   
   SUMA_RETURNe;
}

/*!
returns a vector of indices into dov for DO that meet DO_Type
You should free the returned pointer once you're done with it
N contains the number of elements found
*/
int * SUMA_GetDO_Type(SUMA_DO *dov, int N_dov, SUMA_DO_Types DO_Type, int *N)
{
   static char FuncName[]={"SUMA_GetDO_Type"};
   int *do_id, i;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;

   *N = 0;

   do_id = (int *)SUMA_calloc (SUMA_MAX_DISPLAYABLE_OBJECTS, sizeof(int));

   if (do_id == NULL) {
      fprintf(stderr,"Error SUMA_GetDO_Type: Could not allocate for do_id\n");
      SUMA_RETURN(NULL);
   }
      i = 0;
      while (i < N_dov) {
         if (dov[i].ObjectType == DO_Type) {
            do_id[*N] = i;
            *N = *N + 1;
         }
      ++i;
      }
      SUMA_RETURN(do_id);
}

/*!
   searches all SO_type DO objects for idcode
   YUP if found, NOPE if not
*/
SUMA_Boolean SUMA_existSO(char *idcode, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_existSO"};
   SUMA_SurfaceObject *SO;
   int i;
   
   SUMA_ENTRY;

   if (idcode == NULL) {
      fprintf(SUMA_STDERR,"Warning SUMA_existSO: NULL idcode.\n");
      SUMA_RETURN (NOPE);
   }
   for (i=0; i< N_dov; ++i) {
      if (dov[i].ObjectType == SO_type) {
         SO = (SUMA_SurfaceObject *)dov[i].OP;
         if (strcmp(idcode, SO->idcode_str)== 0) {
            SUMA_RETURN (YUP);
         }
      }
   }
   SUMA_RETURN(NOPE);
}

/*!
   searches all DO objects with an idcode_str for idcode
   
   YUP if found, NOPE if not
*/
SUMA_Boolean SUMA_existDO(char *idcode, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_existDO"};
   int i;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_DRAWN_ROI *dROI = NULL;
   SUMA_ROI *ROI = NULL;
   SUMA_SegmentDO *sdo = NULL;
   SUMA_Axis *sax = NULL;
   SUMA_SphereDO *spdo=NULL;
   SUMA_ENTRY;

   if (idcode == NULL) {
      fprintf(SUMA_STDERR,"Warning %s: NULL idcode.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   for (i=0; i< N_dov; ++i) {
      switch (dov[i].ObjectType) {
         case (SO_type):
            SO = (SUMA_SurfaceObject *)dov[i].OP;
            if (strcmp(idcode, SO->idcode_str)== 0) {
               SUMA_RETURN (YUP);
            }
            break;
         case (ROIdO_type):
            dROI = (SUMA_DRAWN_ROI *)dov[i].OP;
            if (strcmp(idcode, dROI->idcode_str)== 0) {
               SUMA_RETURN (YUP);
            }
            break;
         case (ROIO_type):
            ROI = (SUMA_ROI *)dov[i].OP;
            if (strcmp(idcode, ROI->idcode_str)== 0) {
               SUMA_RETURN (YUP);
            }
            break;
         case (AO_type):
            sax = (SUMA_Axis *)dov[i].OP;
            if (strcmp(idcode, sax->idcode_str)== 0) {
               SUMA_RETURN (YUP);
            }
            break;
         case (SP_type):
            spdo = (SUMA_SphereDO *)dov[i].OP;
            if (strcmp(idcode, spdo->idcode_str)== 0) {
               SUMA_RETURN (YUP);
            }
            break;
         case (OLS_type):
         case (LS_type):
            sdo = (SUMA_SegmentDO *)dov[i].OP;
            if (strcmp(idcode, sdo->idcode_str)== 0) {
               SUMA_RETURN (YUP);
            }
            break;
         default:
            fprintf(SUMA_STDERR,"Warning %s: Object type %d not checked.\n", FuncName, dov[i].ObjectType);
            break;
      }
   }
   SUMA_RETURN(NOPE);
}

/*!
   searches for DO object with an idcode_str equal to idcode
   It returns the DO's index into dov
   -1 if nothing was found
   
*/
int SUMA_whichDO(char *idcode, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_whichDO"};
   int i;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_DRAWN_ROI *dROI = NULL;
   SUMA_ROI *ROI = NULL;
   SUMA_SegmentDO *sdo = NULL;
   SUMA_Axis *sax = NULL;
   SUMA_SphereDO *spdo=NULL;
   
   SUMA_ENTRY;

   if (SUMA_IS_EMPTY_STR_ATTR(idcode)) { /* might come in as ~ at times */
      fprintf(SUMA_STDERR,"Warning %s: NULL idcode.\n", FuncName);
      SUMA_RETURN (-1);
   }
   for (i=0; i< N_dov; ++i) {
      switch (dov[i].ObjectType) {
         case (SO_type):
            SO = (SUMA_SurfaceObject *)dov[i].OP;
            if (strcmp(idcode, SO->idcode_str)== 0) {
               SUMA_RETURN (i);
            }
            break;
         case (ROIdO_type):
            dROI = (SUMA_DRAWN_ROI *)dov[i].OP;
            if (strcmp(idcode, dROI->idcode_str)== 0) {
               SUMA_RETURN (i);
            }
            break;
         case (ROIO_type):
            ROI = (SUMA_ROI *)dov[i].OP;
            if (strcmp(idcode, ROI->idcode_str)== 0) {
               SUMA_RETURN (i);
            }
            break;
         case (AO_type):
            sax = (SUMA_Axis *)dov[i].OP;
            if (strcmp(idcode, sax->idcode_str)== 0) {
               SUMA_RETURN (i);
            }
            break;
         case (SP_type):
            spdo = (SUMA_SphereDO *)dov[i].OP;
            if (strcmp(idcode, spdo->idcode_str)== 0) {
               SUMA_RETURN (i);
            }
            break;
         case (OLS_type):
         case (LS_type):
            sdo = (SUMA_SegmentDO *)dov[i].OP;
            if (strcmp(idcode, sdo->idcode_str)== 0) {
               SUMA_RETURN (i);
            }
            break;
         default:
            fprintf(SUMA_STDERR,"Warning %s: Object type %d not checked.\n", FuncName, dov[i].ObjectType);
            break;
      }
   }
   SUMA_RETURN(-1);
}

/*!
   ans = SUMA_findSO_inDOv(idcode, dov, N_dov);
   searches all SO_type DO objects for idcode
   
   \param idcode (char *) idcode of SO you are searching for
   \param dov (SUMA_DO*) pointer to vector of Displayable Objects, typically SUMAg_DOv
   \param N_dov (int) number of DOs in dov
   \return ans (int) index into dov of object with matching idcode 
       -1 if not found
  \sa SUMA_findSOp_inDOv
*/
int SUMA_findSO_inDOv(char *idcode, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_findSO_inDOv"};
   SUMA_SurfaceObject *SO;
   int i;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;

   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == SO_type) {
         SO = (SUMA_SurfaceObject *)dov[i].OP;
         if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: Comparing \n\t:%s:to\n\t:%s:\n", 
                     FuncName, idcode, SO->idcode_str);
         if (strcmp(idcode, SO->idcode_str)== 0) {
            SUMA_RETURN (i);
         }
      }
   }
   SUMA_RETURN(-1);
}

/*!
   
  SO = SUMA_findSOp_inDOv(char *idcode, SUMA_DO *dov, int N_dov)
   searches all SO_type DO objects for idcode
   
   \param idcode (char *) idcode of SO you are searching for
   \param dov (SUMA_DO*) pointer to vector of Displayable Objects, 
                         typically SUMAg_DOv
   \param N_dov (int) number of DOs in dov
   \return SO (SUMA_SurfaceObject *) pointer of SO with matching idcode 
       NULL if not found
   \sa SUMA_findSO_inDOv
*/
SUMA_SurfaceObject * SUMA_findSOp_inDOv(char *idcode, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_findSOp_inDOv"};
   SUMA_SurfaceObject *SO;
   int i;
   
   SUMA_ENTRY;

   if (!idcode) SUMA_RETURN(NULL);
   
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == SO_type) {
         SO = (SUMA_SurfaceObject *)dov[i].OP;
         if (strcmp(idcode, SO->idcode_str)== 0) {
            SUMA_RETURN (SO);
         }
      }
   }
   SUMA_RETURN(NULL);
}

SUMA_Boolean  SUMA_is_ID_4_SO(char *idcode, SUMA_SurfaceObject **SOp)
{
   static char FuncName[]={"SUMA_is_ID_4_SO"};
   SUMA_SurfaceObject *SO=NULL;
   
   SUMA_ENTRY;
   
   if (SOp) *SOp = NULL;
   if (!idcode) SUMA_RETURN(NOPE);
   
   SO = SUMA_findSOp_inDOv(idcode, SUMAg_DOv, SUMAg_N_DOv);
   
   if (SO) {
      if (SOp) *SOp = SO;
      SUMA_RETURN(YUP);
   }
   
   SUMA_RETURN(NOPE);   
}


/*!
   Returns an index into dov of a surface domain parent with the largest
   number of nodes
*/
int SUMA_BiggestLocalDomainParent(SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_BiggestLocalDomainParent"};
   SUMA_SurfaceObject *SO;
   int i, imax = -1, MaxNode=-1;
   
   SUMA_ENTRY;
   MaxNode = -1;
   imax = -1;
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == SO_type) {
         SO = (SUMA_SurfaceObject *)dov[i].OP;
         if (SUMA_isLocalDomainParent(SO)) {
            if (SO->N_Node > MaxNode) {
               imax = i;
               MaxNode= SO->N_Node;
            }
         }
      }
   }
   
   SUMA_RETURN(imax);
}



SUMA_SurfaceObject * SUMA_findanySOp_inDOv(SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_findanySOp_inDOv"};
   SUMA_SurfaceObject *SO;
   int i;
   
   SUMA_ENTRY;
   
   SO = NULL;
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == SO_type) {
         SO = (SUMA_SurfaceObject *)dov[i].OP;
         SUMA_RETURN (SO);
      }
   }
   
   SUMA_RETURN(NULL);
}


char *SUMA_find_SOLabel_from_idcode (char *idcode, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_find_SOLabel_from_idcode"};
   SUMA_SurfaceObject *SO;
   int i;
   
   SUMA_ENTRY;
   
   if (!idcode) SUMA_RETURN(NULL);
   
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == SO_type) {
         SO = (SUMA_SurfaceObject *)dov[i].OP;
         if (strcmp(idcode, SO->idcode_str)== 0) {
            SUMA_RETURN (SO->Label);
         }
      }
   }
   SUMA_RETURN(NULL);
}

char *SUMA_find_SOidcode_from_label (char *label, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_find_SOidcode_from_label"};
   SUMA_SurfaceObject *SO;
   int i;
   char *found = NULL;
   
   SUMA_ENTRY;
   
   if (!label) SUMA_RETURN(NULL);
   
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == SO_type) {
         SO = (SUMA_SurfaceObject *)dov[i].OP;
         if (strcmp(label, SO->Label)== 0) {
            if (!found) { found = SO->idcode_str; }
            else {
               SUMA_S_Errv("More than one surface with label %s found.\n", label);
               SUMA_RETURN(NULL);
            }
         }
      }
   }
   SUMA_RETURN(found);
}

/*!
   
  SO = SUMA_find_named_SOp_inDOv(char *idcode, SUMA_DO *dov, int N_dov)
   searches all SO_type DO objects for idcode
   
   \param coordname (char *) filename of SO (without path) that you are searching for.
                             If surface is specified by 2 files, then use the coord file
                             name.  
   \param dov (SUMA_DO*) pointer to vector of Displayable Objects, typically SUMAg_DOv
   \param N_dov (int) number of DOs in dov
   \return SO (SUMA_SurfaceObject *) pointer of SO with matching idcode 
       NULL if not found
   \sa SUMA_findSO_inDOv
   \sa SUMA_findSOp_inDOv
*/
SUMA_SurfaceObject * SUMA_find_named_SOp_inDOv( char *coordnamei, 
                                                SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_find_named_SOp_inDOv"};
   SUMA_SurfaceObject *SO = NULL, *SOf = NULL;
   SUMA_STRING *SS=NULL;
   char *stmp=NULL, *coordname=NULL;
   int i;
   SUMA_FileName sf;
   
   SUMA_ENTRY;
   
   if (!coordnamei || !dov) SUMA_RETURN(NULL);
   
   /* remove the path from coordname, to be sure */
   sf = SUMA_StripPath(coordnamei);
   if (!sf.FileName) SUMA_RETURN(NULL);
   else coordname = sf.FileName;
   
   i=0;
   SOf = NULL;
   while (i<N_dov) {
      if (dov[i].ObjectType == SO_type) {
         SO = (SUMA_SurfaceObject *)dov[i].OP;
         switch(SO->FileType) {
            case SUMA_SUREFIT:
            case SUMA_VEC:
               if (strstr(SO->Name_coord.FileName, coordname)) {
                  if (SOf) {
                     SS = SUMA_StringAppend_va(NULL, NULL);
                     SS = SUMA_StringAppend_va(SS, 
                                    "Error %s:\n"
                                    "Surface name %s\n"
                                    "is not a unique identifier.\n"
                                    "Found %s and %s so far.\n"
                                    "Be more specific.\n", FuncName, 
                                    coordname, SOf->Name_coord.FileName,
                                    SO->Name_coord.FileName);
                     SUMA_SS2S(SS, stmp);
                     SUMA_SL_Err(stmp); if (stmp) SUMA_free(stmp); stmp = NULL;
                     if (sf.FileName) SUMA_free(sf.FileName); 
                        sf.FileName = NULL;
                     if (sf.Path) SUMA_free(sf.Path); 
                        sf.Path = NULL;
                     SUMA_RETURN(NULL);
                  }
                  SOf = SO;
               }
               break;
            case SUMA_FREE_SURFER:
            case SUMA_FREE_SURFER_PATCH:
            case SUMA_INVENTOR_GENERIC:
            case SUMA_OPENDX_MESH:
            case SUMA_BRAIN_VOYAGER:
            case SUMA_BYU:
            case SUMA_GIFTI:
            case SUMA_MNI_OBJ:
            case SUMA_PLY: 
               if (strstr(SO->Name.FileName, coordname)) {
                  if (SOf) {
                     SS = SUMA_StringAppend_va(NULL, NULL);
                     SS = SUMA_StringAppend_va(SS, 
                                    "Error %s:\n"
                                    "Surface name %s\n"
                                    "is not a unique identifier.\n"
                                    "Found %s and %s so far.\n"
                                    "Be more specific.\n", FuncName, 
                                    coordname, SOf->Name_coord.FileName,
                                    SO->Name_coord.FileName);
                     SUMA_SS2S(SS, stmp);
                     SUMA_SL_Err(stmp); if (stmp) SUMA_free(stmp); stmp = NULL;
                     if (sf.FileName) SUMA_free(sf.FileName); 
                        sf.FileName = NULL;
                     if (sf.Path) SUMA_free(sf.Path); 
                        sf.Path = NULL;                     SUMA_RETURN(NULL);
                  }
                  SOf = SO;
               }
               break;
            default: 
               SUMA_SL_Err("Type not supported.");
               SUMA_RETURN(NULL);
         }
      }
      ++i;
   }
   
   if (sf.FileName) SUMA_free(sf.FileName); 
      sf.FileName = NULL;
   if (sf.Path) SUMA_free(sf.Path); 
      sf.Path = NULL;   
      
   SUMA_RETURN(SOf);
}

/*!
   determines if a Surface Object is mappable (ie LocalDomainParentID != NULL)
   ans = SUMA_ismappable (SUMA_SurfaceObject *SO)
   \param SO (SUMA_SurfaceObject *)
   \ret YUP/NOPE
*/
SUMA_Boolean SUMA_ismappable (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_ismappable"};
   
   SUMA_ENTRY;

   if (SO->LocalDomainParentID != NULL) {
      /* SO is mappable */
      SUMA_RETURN (YUP);
   }
    
   SUMA_RETURN (NOPE);

}

/*!
   Left here temporarily for backward compatibility
   
   use the more appropriately named SUMA_isLocalDomainParent
*/
SUMA_Boolean SUMA_isINHmappable (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_isINHmappable"};
   
   SUMA_ENTRY;
   
   SUMA_RETURN(SUMA_isLocalDomainParent(SO));
}

/*!
   determines if a Surface Object is a local domain parent (ie LocalDomainParentID == idcode_str)
   (used to be called inherently mappable) 
   ans = SUMA_isLocalDomainParent (SUMA_SurfaceObject *SO)
   \param SO (SUMA_SurfaceObject *)
   \ret YUP/NOPE
   
   -- Used to be called SUMA_isINHmappable
*/
SUMA_Boolean SUMA_isLocalDomainParent (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_isLocalDomainParent"};
   
   SUMA_ENTRY;

   if (SO->LocalDomainParentID == NULL) {
      SUMA_RETURN (NOPE);
   }
   if (strcmp(SO->LocalDomainParentID, SO->idcode_str) == 0) {
      /* SO is the local domain parent */
      SUMA_RETURN (YUP);
   } 
   SUMA_RETURN (NOPE);
}

/*!
   \brief ans = SUMA_isRelated (SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2, int level);
   
   returns YUP if SO1 and SO2 are related at level 1 or 2
   level 1 means nuclear family (share the same parent)
   level 2 means extended family (share a grandparent, ultimately this could be
            the grand icosahedron duc
            Surfaces must be the of the same hemi side since both hemispheres 
            can have the same grandparent, use level 3 if you want to go across 
            hemis.)
   level 3 is like level 2 with no care for what side of the brain we're working 
            with
   
   For more definitions on relationships:
   
   \sa SUMA_WhatAreYouToMe
*/

SUMA_Boolean SUMA_isRelated ( SUMA_SurfaceObject *SO1, 
                              SUMA_SurfaceObject *SO2, int level)
{
   static char FuncName[]={"SUMA_isRelated"};
   SUMA_DOMAIN_KINSHIPS kin;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   kin =  SUMA_WhatAreYouToMe (SO1, SO2);
   switch (level) {
      case 3: /* anything goes */
         if (  (kin > SUMA_DOMAINS_NOT_RELATED) ) SUMA_RETURN(YUP);
         break;
      case 2: /* share an ancestor, but same side */
         if (  (kin > SUMA_DOMAINS_NOT_RELATED) ) {
               if ( (SO1->Side == SO2->Side) ) {
                  SUMA_RETURN(YUP);
               } else {
                  SUMA_S_Note( "Surfaces appear related at level 2 "
                               "but sides are not the same.\n"                                                   "Kinship level is being ignored.\n");
                  if (SO1->Side < SUMA_LR || SO2->Side < SUMA_LR) {
                     SUMA_S_Note("Surface sides are not clearly defined. "
                                 "If this is in error, consider adding \n"
                                 "Hemisphere = R  (or L or B) in the spec file\n"
                                 "to make sure surfaces sides are correctly "
                                 "labeled.\n");
                  }
               }
         }
         break;
      case 1: /* nuclear family only */
         if (  (kin > SUMA_DOMAINS_NOT_RELATED) && 
               (kin < SUMA_NUCELAR_FAMILY ) ) {
               if (SO1->Side == SO2->Side) {
                   SUMA_RETURN(YUP); 
                     /* last condition is not really necessary but it 
                     don't hoyt...*/
               } else {
                  SUMA_S_Note( "Surfaces appear related at level 2 "
                               "but sides are not the same.\n"                                                   "Kinship level is being ignored.\n");
                  if (SO1->Side < SUMA_LR || SO2->Side < SUMA_LR) {
                     SUMA_S_Note("Surface sides are not clearly defined. "
                                 "If this is in error, consider adding \n"
                                 "Hemisphere = R  (or L or B) in the spec file\n"
                                 "to make sure surfaces sides are correctly "
                                 "labeled.\n");
                  }
               }
         }
         break;
      default:
         SUMA_SL_Err("Bad value for level.");   
         break;
   }
   SUMA_RETURN(NOPE);
}

/*!
   \brief finds a contralateral surface to SO that is of the same
   Group AND State
*/
SUMA_SurfaceObject *SUMA_Contralateral_SO(SUMA_SurfaceObject *SO,
                                          SUMA_DO *dov, int N_dov) 
{
   static char FuncName[]={"SUMA_Contralateral_SO"};
   SUMA_SurfaceObject *SOC=NULL;
   int findside = SUMA_SIDE_ERROR;
   int i;
   
   SUMA_ENTRY;

   if (!SO) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(SOC);
   }
   if (!SO->Group) {
      SUMA_S_Err("Need SO->Group");
      SUMA_RETURN(SOC);
   }
   
   if (SO->Side != SUMA_LEFT && SO->Side != SUMA_RIGHT) {
      if (SO->Side < SUMA_LR) {
         SUMA_S_Warn("Surface sides are not clearly defined. "
                     "If this is in error, consider adding \n"
                     "Hemisphere = R  (or L or B) in the spec file\n"
                     "to make sure surfaces sides are correctly "
                     "labeled.\n");   
      }
      SUMA_RETURN(SOC);
   }
      
   if (SO->Side == SUMA_LEFT) findside = SUMA_RIGHT;
   else findside = SUMA_LEFT;
   
   for (i=0; i<N_dov; ++i) {
      if (SUMA_isSO_G(dov[i], SO->Group)) { 
         SOC = (SUMA_SurfaceObject *)dov[i].OP;
         if (SOC->Side == findside && !strcmp(SOC->State, SO->State) ) break;
         else SOC = NULL;   
      }
   }
   
   if (SOC && SUMA_isRelated(SOC, SO, 1)) {
      SUMA_S_Warn("Unexpected surface pair with same localdomainparent.\n"
                  "Good Luck To You");
   }
   /*
      if (SOC) SOC = SUMA_findSOp_inDOv( SOC->LocalDomainParentID, 
                                      dov, N_dov);
   */

   
   SUMA_RETURN(SOC);
}

SUMA_DSET * SUMA_Contralateral_dset(SUMA_DSET *dset, SUMA_SurfaceObject *SO, 
                                    SUMA_SurfaceObject**SOCp)
{
   static char FuncName[]={"SUMA_Contralateral_dset"};
   SUMA_DSET *cdset=NULL, *dd=NULL;
   DListElmt *el=NULL;
   SUMA_SurfaceObject *SOC=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset) { 
      SUMA_S_Err("NULL input");
      SUMA_RETURN(cdset);
   }
   
   if (!SO) {
      if (!(SO = 
            SUMA_findSOp_inDOv(  SUMA_sdset_idmdom(dset), 
                                 SUMAg_DOv, SUMAg_N_DOv))){
         SUMA_S_Err("Can't find dset's domain parent");
         SUMA_RETURN(cdset);
      }
   }
   
   if (!(SOC = SUMA_Contralateral_SO(SO, SUMAg_DOv, SUMAg_N_DOv))) {
      /* Nothing there, return */
      SUMA_RETURN(cdset);
   }
   
   SUMA_LH("Have contralateral surface to consider\n");
   el = dlist_head(SUMAg_CF->DsetList);
   while (el) {
      dd = (SUMA_DSET*)el->data;
      if (SUMA_isDsetRelated(dd,SOC)) {
         SUMA_LHv("Have Dset %s related to SOC\n", SDSET_LABEL(dd));
         /* Does dd relate to dset ? */
         if (  SUMA_isSameDsetColTypes(dset, dd) ) {
            if (!cdset) {
               cdset = dd;
            }else {
               SUMA_S_Warn("More than one dset matches\n"
                           "Returning NULL");
               SUMA_RETURN(NULL);
            }
         } 
      }
      el = dlist_next(el);
   }
   
   if (SOCp) *SOCp=SOC;
   SUMA_RETURN(cdset);
}
/*!
   \brief ans = SUMA_WhatAreYouToMe (SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2);
   returns a code for the kinship between two surfaces:
    
   SO1->idcode_str = SO2->idcode_str (in this case SO1 = SO2) or 
   SO1->LocalDomainParentID = SO2->idcode_str (SO2 is the mapping reference of SO1) or
   SO1->idcode_str = SO2->LocalDomainParentID (SO1 is the mapping reference of SO2)
   SO1->LocalDomainParentID = SO2->LocalDomainParentID (SO1 and SO2 have the same mapping reference) or 
   SO1->DomainGrandParentID = SO2->idcode_str (SO2 is the granddaddy of SO1) or
   SO1->idcode_str = SO2->DomainGrandParentID (SO1 is the granddaddy of SO2) or
   SO1->DomainGrandParentID = SO2->DomainGrandParentID (SO1 and SO2 have the same granddaddy) 
   
   \sa definition of SUMA_DOMAIN_KINSHIPS and
   \sa SUMA_DomainKinships_String
*/
SUMA_DOMAIN_KINSHIPS SUMA_WhatAreYouToMe (SUMA_SurfaceObject *SO1, 
                                          SUMA_SurfaceObject *SO2)
{
   static char FuncName[]={"SUMA_WhatAreYouToMe"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO1 || !SO2 || !SO1->idcode_str || !SO2->idcode_str) {
      fprintf (SUMA_STDERR, "Error %s: NULL SOs or SO->idcode_str.\n", FuncName);
      SUMA_RETURN (SUMA_DOMAINS_NOT_RELATED);
   }
   
   if (strcmp (SO1->idcode_str, SO2->idcode_str) == 0) {
      /* SO1 = SO2 */
      SUMA_LH(SUMA_DomainKinships_String (SUMA_SO1_is_SO2));
      SUMA_RETURN (SUMA_SO1_is_SO2);
   }
   
   if (SO1->LocalDomainParentID) {
      if (strcmp (SO1->LocalDomainParentID, SO2->idcode_str) == 0) {
         /* SO2 is the local domain parent of SO1 */
         SUMA_LH(SUMA_DomainKinships_String (SUMA_SO2_is_LDPSO1));
         SUMA_RETURN (SUMA_SO2_is_LDPSO1);
      }
   }
   
   if (SO2->LocalDomainParentID) {
      if (strcmp (SO1->idcode_str, SO2->LocalDomainParentID) == 0) {
          /* SO1 is the local domain parent of SO2 */
          SUMA_LH(SUMA_DomainKinships_String (SUMA_SO1_is_LDPSO2));
          SUMA_RETURN (SUMA_SO1_is_LDPSO2);
      }
   }
   
   if (SO1->LocalDomainParentID && SO2->LocalDomainParentID) {
      if (strcmp (SO1->LocalDomainParentID, SO2->LocalDomainParentID) == 0) {
         /* SO1 and SO2 have the same local domain parent */
         SUMA_LH(SUMA_DomainKinships_String (SUMA_LDPSO1_is_LDPSO2));
         SUMA_RETURN (SUMA_LDPSO1_is_LDPSO2);
      }
   }
   
   if (SO1->DomainGrandParentID && SO2->idcode_str) {
      if (strcmp (SO1->DomainGrandParentID, SO2->idcode_str) == 0) {
         /* SO2 is the grand daddy of SO1 */
         SUMA_LH(SUMA_DomainKinships_String (SUMA_SO2_is_GPSO1));
         SUMA_RETURN (SUMA_SO2_is_GPSO1);
      }
   }
   
   if (SO1->idcode_str && SO2->DomainGrandParentID) {
      if (strcmp (SO1->idcode_str, SO2->DomainGrandParentID) == 0) {
         /* SO1 is the grand daddy of SO2 */
         SUMA_LH(SUMA_DomainKinships_String (SUMA_SO1_is_GPSO2));
         SUMA_RETURN (SUMA_SO1_is_GPSO2);
      }
   }
   
   if (SO1->DomainGrandParentID && SO2->DomainGrandParentID) {
      if (strcmp (SO1->DomainGrandParentID, SO2->DomainGrandParentID) == 0) {
         /* SO1 and SO2 have the same grand daddy */
         SUMA_LH(SUMA_DomainKinships_String (SUMA_GPSO1_is_GPSO2));
         SUMA_RETURN (SUMA_GPSO1_is_GPSO2);
      }
   }
   
   SUMA_LH(SUMA_DomainKinships_String (SUMA_DOMAINS_NOT_RELATED));
   SUMA_RETURN (SUMA_DOMAINS_NOT_RELATED);
   
}

/*!
   \brief SUMA_Boolean SUMA_isSO_G (SUMA_DO DO, char *Group) 
   returns YUP if DO is a surface object
   and is a part of the group Group
   
   \sa SUMA_isSO
*/
SUMA_Boolean SUMA_isSO_G (SUMA_DO DO, char *Group) 
{
   static char FuncName[]={"SUMA_isSO_G"};
   SUMA_SurfaceObject *SO = NULL;
   
   SUMA_ENTRY;
 
   if (!Group) {
      SUMA_SL_Err("Null Group");
      SUMA_RETURN(NOPE);
   }
   
   if (SUMA_isSO(DO)) {
      SO = (SUMA_SurfaceObject *)DO.OP;
      if (!SO->Group) {
         SUMA_SL_Err("Surface has no group, imbecile");
         SUMA_RETURN(NOPE);
      }
      if (strcmp(SO->Group, Group)) { SUMA_RETURN(NOPE); }
      else { SUMA_RETURN(YUP); }
  }
   
   SUMA_RETURN(NOPE);
}
/*!
   \brief SUMA_Boolean SUMA_isSO (SUMA_DO DO) 
   returns YUP if DO is of SO_type
   ans = SUMA_isSO (DO) ;
   
   \sa SUMA_isSO_G (SUMA_DO DO, char *Group)
*/
SUMA_Boolean SUMA_isSO (SUMA_DO DO) 
{
   static char FuncName[]={"SUMA_isSO"};
   
   SUMA_ENTRY;

   if (DO.ObjectType == SO_type) {
      SUMA_RETURN (YUP);
   }
   SUMA_RETURN (NOPE);
}

/*!
   \brief Returns a list of the ROIs loaded into dov. 
   
   \param dov (SUMA_DO *) pointer to vector of DOs
   \param N_dov (int) number of DOs in dov
   \param SortByLabel (SUMA_Boolean) if YUP then returned strings are sorted by their ROI's labels
            (The parents must be part of dov). If Nope then strings are sorted by the labels of
             the ROI's parent.
   \return clist (SUMA_ASSEMBLE_LIST_STRUCT *) pointer to structure containing results
   
   \sa SUMA_FreeAssembleListStruct
   \sa SUMA_CreateAssembleListStruct
   
*/
SUMA_ASSEMBLE_LIST_STRUCT * SUMA_AssembleAllROIList (SUMA_DO * dov, int N_dov, SUMA_Boolean SortByLabel) 
{
   static char FuncName[]={"SUMA_AssembleAllROIList"};
   int i=-1, N_clist=-1; 
   DList *list=NULL, *listop = NULL;
   DListElmt *Elm = NULL, *Elmop = NULL;
   char Label[SUMA_MAX_NAME_LENGTH], Parent_Label[SUMA_MAX_NAME_LENGTH], *store=NULL;
   SUMA_SurfaceObject *SO = NULL;
   char **clist=NULL;
   void **oplist=NULL;
   SUMA_DRAWN_ROI *ROI=NULL;
   SUMA_ASSEMBLE_LIST_STRUCT *clist_str = NULL;
   SUMA_Boolean Found = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   list = (DList *)SUMA_malloc(sizeof(DList));
   listop = (DList *)SUMA_malloc(sizeof(DList));
   
   clist = NULL;
   N_clist = -1;
   
   dlist_init(list, NULL);
   dlist_init(listop, NULL);
   for (i=0; i < N_dov; ++i) {
      if (dov[i].ObjectType == ROIdO_type) {
         ROI = (SUMA_DRAWN_ROI *)dov[i].OP;
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Found an ROI %s\n", FuncName, ROI->Label);
         if (!ROI->Label) sprintf (Label,"NULL");
         else sprintf (Label,"%s", ROI->Label);
         if (!ROI->Parent_idcode_str) sprintf (Parent_Label,"NULL");
         else {
            SO = SUMA_findSOp_inDOv(ROI->Parent_idcode_str, dov, N_dov);
            if (!SO) sprintf (Parent_Label,"Unknown");
            else if (!SO->Label) sprintf (Parent_Label,"Empty");
            else sprintf (Parent_Label,"%s", SO->Label);
         }
         /* Now allocate space for that label */
         store = (char *)SUMA_calloc(strlen(Label)+strlen(Parent_Label)+5, sizeof(char));
         if (SortByLabel) {
            sprintf(store,"%s:%s", Label, Parent_Label);
         } else  {
            sprintf(store,"%s:%s", Parent_Label, Label);
         }
         
         /* now place it in the list by aplhpabetical order */
         if (!list->size) {
            dlist_ins_next(list, dlist_tail(list), (void*)store);
            dlist_ins_next(listop, dlist_tail(listop), (void*)ROI);
         }else { /* must sort first */
            Elm = NULL;
            Elmop = NULL;
            do {
               Found = NOPE;
               if (!Elm) {
                  Elm = dlist_head(list);
                  Elmop = dlist_head(listop);
               } else {
                  Elm = dlist_next(Elm);
                  Elmop = dlist_next(Elmop);
               }
               
               if (strcmp(store, (char*)Elm->data) <= 0) {
                  dlist_ins_prev(list, Elm, (void *)store);
                  dlist_ins_prev(listop, Elmop, (void *)ROI);
                  Found = YUP;
               } else if (Elm == dlist_tail(list)) {
                  /* reached the end, append */
                  dlist_ins_next(list, Elm, (void *)store);
                  dlist_ins_next(listop, Elmop, (void *)ROI);
                  Found = YUP;
               }
            } while (!Found);
         }
         
      }
   }

   if (!list->size) { /* Nothing found */
      N_clist = 0;
      
   }else {
   
      Elm = NULL;
      Elmop = NULL;
      clist = (char **)SUMA_calloc(list->size, sizeof(char *));
      oplist = (void **)SUMA_calloc(list->size, sizeof(void*));
      for (i=0; i< list->size; ++i) {
         if (!Elm) {
            Elm = dlist_head(list);
            Elmop = dlist_head(listop);
         } else {
            Elm = dlist_next(Elm);
            Elmop = dlist_next(Elmop);
         }
         clist[i] = (char*)Elm->data;
         oplist[i] = Elmop->data;
      }

      N_clist = list->size;
      /* destroy list */
      dlist_destroy(list);SUMA_free(list);
      dlist_destroy(listop);SUMA_free(listop);
      
      
   }
   
   clist_str = SUMA_CreateAssembleListStruct();
   clist_str->clist = clist;
   clist_str->oplist = oplist;
   clist_str->N_clist = N_clist;
   
   /* return */
   SUMA_RETURN (clist_str);  
}

/*!
   \brief Creates an SUMA_ASSEMBLE_LIST_STRUCT *structure
   \sa SUMA_FreeAssembleListStruct
*/
SUMA_ASSEMBLE_LIST_STRUCT *SUMA_CreateAssembleListStruct(void)
{
   static char FuncName[]={"SUMA_CreateAssembleListStruct"};
   SUMA_ASSEMBLE_LIST_STRUCT *str=NULL;
   
   SUMA_ENTRY;
   
   str = (SUMA_ASSEMBLE_LIST_STRUCT *)SUMA_malloc(sizeof(SUMA_ASSEMBLE_LIST_STRUCT));
   str->clist = NULL;
   str->N_clist = -1;
   str->oplist = NULL;
   
   SUMA_RETURN(str);
}
/*!
   \brief frees SUMA_ASSEMBLE_LIST_STRUCT *
   \param SUMA_ASSEMBLE_LIST_STRUCT *str
   
   \return NULL always
   
   -This function frees each string in clist. BUT NOT pointers in oplist (for obvious reasons)
*/
SUMA_ASSEMBLE_LIST_STRUCT *SUMA_FreeAssembleListStruct(SUMA_ASSEMBLE_LIST_STRUCT *str) 
{
   static char FuncName[]={"SUMA_FreeAssembleListStruct"};
   int i;
   
   SUMA_ENTRY;
   
   if (!str) SUMA_RETURN(NULL);
   
   if (str->clist) {
      for (i=0; i < str->N_clist; ++i)
         if (str->clist[i]) SUMA_free(str->clist[i]);
      SUMA_free(str->clist);
   }
   if (str->oplist) SUMA_free(str->oplist);
   SUMA_free(str);
   
   SUMA_RETURN(NULL);
}


/*!
\brief Returns an ROI that is related to SO and is in InCreation (being actively drawn) DrawStatus 
 There should only be one ROI in creation at any one time for a group of related surfaces. 
 
 ROI = SUMA_
 ROI_InCreation (SO,  dov,  N_dov);
 
 \param SO (SUMA_SurfaceObject *) pointer to surface object
 \param dov (SUMA_DO *) pointer to vector of DOs (typically SUMAg_DOv)
 \param N_dov (int) number of elements in dov
 \return ROI (SUMA_DRAWN_ROI *) pointer to ROI object, NULL if no such object is found.
 
*/
SUMA_DRAWN_ROI * SUMA_FetchROI_InCreation (SUMA_SurfaceObject *SO, SUMA_DO * dov, int N_dov) 
{
   int i;
   SUMA_DRAWN_ROI *ROI = NULL;
   static char FuncName[]={"SUMA_FetchROI_InCreation"};
   
   SUMA_ENTRY;
   
   for (i=0; i < N_dov; ++i) {
      if (dov[i].ObjectType == ROIdO_type) {
         ROI = (SUMA_DRAWN_ROI *)dov[i].OP;
         if (ROI->DrawStatus == SUMA_ROI_InCreation) {
            if (SUMA_isdROIrelated (ROI, SO)) {
               /* found an ROI, should be the only one, return */
               SUMA_RETURN (ROI);
            }
         }
      }
   }
   SUMA_RETURN (NULL);
}

/*!
\brief Returns YUP if the surface: NBV(or NBSP)->Parent_idcode_str is the same as SO->idcode_str.
NOPE otherwise

ans = SUMA_isNBDOrelated (NBDO, SO);

\param NBV (SUMA_NB_DO * ) pointer to NBV/NBSP object
\param SO (SUMA_SurfaceObject *) pointer to surface object
\return ans (SUMA_Boolean) YUP/NOPE
\sa SUMA_isNIDOrelated
*/
SUMA_Boolean SUMA_isNBDOrelated (SUMA_NB_DO *SDO, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_isNBDOrelated"};
   SUMA_SurfaceObject *SO_NB = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* Just compare the idcodes, not allowing for kinship yet but code below could do that */
   if (strcmp(SO->idcode_str, SDO->Parent_idcode_str) == 0) {
      SUMA_RETURN(YUP);
   } else {
      SUMA_RETURN(NOPE);
   }
   
   if (LocalHead) {
      fprintf (SUMA_STDERR, "%s: %s SO->LocalDomainParentID\n", FuncName, SO->LocalDomainParentID);
      fprintf (SUMA_STDERR, "%s: %s SDO->Parent_idcode_str\n", FuncName, SDO->Parent_idcode_str);
      fprintf (SUMA_STDERR, "%s: %s SO->idcode_str\n", FuncName, SO->idcode_str);
   }
   
   /* find the pointer to the surface having for an idcode_str: ROI->Parent_idcode_str */
   SO_NB = SUMA_findSOp_inDOv(SDO->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   
   if (!SO_NB) {
      SUMA_SL_Err("Could not find surface of SDO->Parent_idcode_str");
      SUMA_RETURN (NOPE);
   }
   
   if ( SUMA_isRelated (SO, SO_NB, 1)) { /*  relationship of the 1st order only */ 
      SUMA_RETURN (YUP);
   }
   
   /* If you get here, you have SO_NB so return happily */
   SUMA_RETURN (NOPE);
}

SUMA_Boolean SUMA_isNIDO_SurfBased(SUMA_NIDO *nido)
{
   static char FuncName[]={"SUMA_isNIDO_SurfBased"};
   char *atr=NULL;

   SUMA_ENTRY;

   atr = NI_get_attribute(nido->ngr,"bond");
   if (!atr) SUMA_RETURN(NOPE);

   if (atr[0] == 's') SUMA_RETURN(YUP);

   SUMA_RETURN(NOPE);
}

/*!
   If you make changes here, look into  SUMA_isNBDOrelated  
   */
SUMA_Boolean SUMA_isNIDOrelated (SUMA_NIDO *SDO, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_isNIDOrelated"};
   SUMA_SurfaceObject *SO_NB = NULL;
   char *Parent_idcode_str=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* Just compare the idcodes, not allowing 
   for kinship yet but code below could do that */
   if ((Parent_idcode_str = NI_get_attribute(SDO->ngr, "Parent_idcode_str")) &&
       strcmp(SO->idcode_str, Parent_idcode_str) == 0) {
      SUMA_RETURN(YUP);
   } else {
      SUMA_RETURN(NOPE);
   }
   
   if (LocalHead) {
      fprintf (SUMA_STDERR, "%s: %s SO->LocalDomainParentID\n", FuncName, SO->LocalDomainParentID);
      fprintf (SUMA_STDERR, "%s: %s SDO->Parent_idcode_str\n", FuncName, Parent_idcode_str);
      fprintf (SUMA_STDERR, "%s: %s SO->idcode_str\n", FuncName, SO->idcode_str);
   }
   
   /* find the pointer to the surface having for an idcode_str: ROI->Parent_idcode_str */
   SO_NB = SUMA_findSOp_inDOv(Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   
   if (!SO_NB) {
      SUMA_SL_Err("Could not find surface of SDO->Parent_idcode_str");
      SUMA_RETURN (NOPE);
   }
   
   if ( SUMA_isRelated (SO, SO_NB, 1)) { /*  relationship of the 1st order only */ 
      SUMA_RETURN (YUP);
   }
   
   /* If you get here, you have SO_NB so return happily */
   SUMA_RETURN (NOPE);
}



/*!
\brief Returns YUP if the surface: dROI->Parent_idcode_str is related to SO->idcode_str or SO->LocalDomainParentID.
NOPE otherwise

ans = SUMA_isdROIrelated (dROI, SO);

\param ROI (SUMA_DRAWN_ROI *) pointer to drawn ROI
\param SO (SUMA_SurfaceObject *) pointer to surface object
\return ans (SUMA_Boolean) YUP/NOPE

*/
SUMA_Boolean SUMA_isdROIrelated (SUMA_DRAWN_ROI *ROI, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_isdROIrelated"};
   SUMA_SurfaceObject *SO_ROI = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !ROI) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   if (LocalHead) {
      fprintf (SUMA_STDERR, 
               "%s: %s SO->LocalDomainParentID\n", 
               FuncName, SO->LocalDomainParentID);
      fprintf (SUMA_STDERR, 
               "%s: %s ROI->Parent_idcode_str\n", 
               FuncName, ROI->Parent_idcode_str);
      fprintf (SUMA_STDERR, 
               "%s: %s SO->idcode_str\n", 
               FuncName, SO->idcode_str);
   }
   
   /* find the pointer to the surface having for an 
      idcode_str: ROI->Parent_idcode_str */
   SO_ROI = SUMA_findSOp_inDOv(ROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   
   
   if (!SO_ROI) {
      SUMA_SL_Err("Could not find surface of ROI->Parent_idcode_str");
      SUMA_RETURN(NOPE);
   }
   
   if (SUMA_isRelated (SO, SO_ROI, 1)) { 
      /* relationship of the 1st order only */ 
      SUMA_RETURN (YUP);
   }

   SUMA_RETURN (NOPE);
}

/*!
\brief Returns YUP if if the surface: ROI->Parent_idcode_str is related to SO->idcode_str or SO->LocalDomainParentID.
NOPE otherwise

ans = SUMA_isROIrelated (ROI, SO);

\param ROI (SUMA_ROI *) pointer to ROI
\param SO (SUMA_SurfaceObject *) pointer to surface object
\return ans (SUMA_Boolean) YUP/NOPE

*/
SUMA_Boolean SUMA_isROIrelated (SUMA_ROI *ROI, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_isROIrelated"};
   SUMA_SurfaceObject *SO_ROI = NULL;
   
   SUMA_ENTRY;
   
   /* find the pointer to the surface having for an idcode_str: ROI->Parent_idcode_str */
   SO_ROI = SUMA_findSOp_inDOv(ROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   
   if (!SO_ROI) {
      SUMA_SL_Err("Could not find surface of ROI->Parent_idcode_str");
      SUMA_RETURN (NOPE);
   }
   
   if (SUMA_isRelated (SO, SO_ROI, 1)) { /* relationship of the 1st order only */
      SUMA_RETURN (YUP);
   }

   SUMA_RETURN (NOPE);
}

/*!
\brief Return a mask of the nodes belonging to any ROI of a certain surface
   Mask = SUMA_Build_Mask_AllROI (dov, N_dov, SO, Mask, N_added);

\param dov (SUMA_DO*) pointer to vector of displayable objects to consider
\param N_dov (int) number of elements in dov
\param SO (SUMA_SurfaceObject *)
\param Mask (int *) pointer to mask vector.
         0 if node belongs to no ROI
         n if node belongs to n ROIs
         Pass NULL if you're calling this function for the first time.
\param N_added (int *)integer containing the total number of nodes added to Mask.
      That would be the sum of the number of nodes found in all ROIs.
      Duplicate nodes are counted twice. If you want the number of nodes without
      the duplicates, you need to count non-zero values in Mask. 
\return Mask (int *) pointer to modified mask vector. 
*/
int * SUMA_Build_Mask_AllROI (SUMA_DO *dov, int N_do, SUMA_SurfaceObject *SO, int *Mask, int *N_added)
{
   static char FuncName[]={"SUMA_Build_Mask_AllROI"};
   int Npart = 0,i;
   SUMA_DRAWN_ROI *D_ROI=NULL;
   SUMA_ROI *ROI = NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   *N_added = -1;
   
   if (!Mask) { /* allocate for it */
      Mask = (int *)SUMA_calloc(SO->N_Node, sizeof(int));
      if (!Mask) {
         SUMA_S_Err ("Failed to allocate for Mask.");
         SUMA_RETURN(NULL);
      }
   }
   
   for (i=0; i < N_do; ++i) {
      switch (dov[i].ObjectType) { /* case Object Type */
         case ROIdO_type:
            D_ROI = (SUMA_DRAWN_ROI *)dov[i].OP;
            if (SUMA_isdROIrelated (D_ROI, SO)) {
               if (LocalHead) fprintf (SUMA_STDERR, "%s: Found a drawn ROI, building mask...\n", FuncName);

               Npart = SUMA_Build_Mask_DrawnROI (D_ROI, Mask);
               if (Npart < 0) {
                  SUMA_S_Err ("Badness in SUMA_Build_Mask_DrawnROI");
                  if (Mask) SUMA_free(Mask);
                  *N_added = -1;
                  SUMA_RETURN(NULL);
               }else {
                  *N_added = *N_added + Npart;
                  if (LocalHead) fprintf (SUMA_STDERR, "%s: %d nodes found in that ROI.\n", FuncName, Npart);
               }
            }
            break;
         case ROIO_type:
            ROI = (SUMA_ROI *)dov[i].OP;
            if (SUMA_isROIrelated (ROI, SO)) {
               SUMA_S_Err ("Not dealing with regular ROIs yet");
            }
            break;
         default:
            /* not an ROI */
            break;   
      }
   }
   
   SUMA_RETURN(Mask);
}

/*!
\brief Return a mask of the nodes belonging to a drawn ROI of a certain surface
   N_added = SUMA_Build_Mask_DrawnROI (dROI, Mask);
   
\param dROI (SUMA_DRAWN_ROI *) the pointer to the ROI structure
\param Mask (int *) pointer to mask vector.
         0 if node belongs to no ROI
         n if node belongs to n ROIs
      It is the calling function's responsability to make sure enough space is allocated for
      Mask and that cleanup is properly handled.
\return N_added (int *)integer containing the number of nodes found in dROI. 
      This variable is set to -1 when trouble occurs.  
*/ 
int SUMA_Build_Mask_DrawnROI (SUMA_DRAWN_ROI *D_ROI, int *Mask)
{
   static char FuncName[]={"SUMA_Build_Mask_DrawnROI"};
   DListElmt *NextElm=NULL;
   int ii, N_added;
   SUMA_ROI_DATUM *ROId=NULL;
   
   SUMA_ENTRY;
   
   N_added = -1;
   
   if (!Mask) { 
      SUMA_S_Err ("Mask is NULL");
      SUMA_RETURN(N_added);
   }
   
   if (!D_ROI->ROIstrokelist) {
     N_added = 0;
     SUMA_RETURN(N_added); 
   }
   
   if (!dlist_size(D_ROI->ROIstrokelist)) {
     N_added = 0;
     SUMA_RETURN(N_added); 
   }
   
   /* start with the first element */
   NextElm = NULL;
   do {
      if (!NextElm) {
         NextElm = dlist_head(D_ROI->ROIstrokelist);
      }else {
         NextElm = dlist_next(NextElm);
      }
      ROId = (SUMA_ROI_DATUM *)NextElm->data;
      if (ROId->N_n) {
         for (ii = 0; ii < ROId->N_n; ++ii) {
            ++Mask[ROId->nPath[ii]];
            ++N_added;
         }
      }
   } while (NextElm != dlist_tail(D_ROI->ROIstrokelist));
               
   SUMA_RETURN (N_added);
}

/*!
   \brief deletes an ROI from the list of drawn objects
*/
SUMA_Boolean SUMA_DeleteROI (SUMA_DRAWN_ROI *ROI) 
{
   static char FuncName[]={"SUMA_DeleteROI"};
   SUMA_ASSEMBLE_LIST_STRUCT *ALS = NULL;
   SUMA_DRAWN_ROI *NextROI=NULL;
   int i;
   SUMA_Boolean WasCurrent = NOPE, Shaded = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!ROI) {
      SUMA_LH("Null ROI");
      SUMA_RETURN(YUP);
   }
   
   /* form a list of the current ROI available for editing */

   /* assemble the ROI list */
   ALS = SUMA_AssembleAllROIList (SUMAg_DOv, SUMAg_N_DOv, YUP);

   NextROI = NULL;
   if (ALS) {
      if (ALS->N_clist)  {
         i=0;
         while (!NextROI && i<ALS->N_clist) {
            if (ALS->oplist[i] != (void*)ROI) 
               NextROI = (SUMA_DRAWN_ROI *)ALS->oplist[i];
            ++i;
         }  
      }
      SUMA_FreeAssembleListStruct(ALS);
   }

   /* check to see if ROI being deleted is current one */
   if (ROI == SUMAg_CF->X->DrawROI->curDrawnROI) {
      WasCurrent = YUP;
   }else {
      WasCurrent = NOPE;
   }
   
   /* Close the ROIlist window if it is open */
   SUMA_IS_DRAW_ROI_SWITCH_ROI_SHADED(Shaded);
   if (!Shaded) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Closing switch ROI window ...\n", FuncName);
      SUMA_cb_CloseSwitchROI(NULL, (XtPointer) SUMAg_CF->X->DrawROI->SwitchROIlst, NULL);
   }

   /* remove ROI for SUMAg_DO and clear the ROI structure*/
   if (!SUMA_RemoveDO(SUMAg_DOv, &SUMAg_N_DOv, (void *)ROI, YUP)) {
      SUMA_SLP_Err("Failed to remove DO from list.");
      SUMA_RETURN(NOPE);
   }

   if (WasCurrent) {
      SUMAg_CF->X->DrawROI->curDrawnROI = NextROI;

      /* reinitialize the draw ROI window */
      SUMA_InitializeDrawROIWindow(SUMAg_CF->X->DrawROI->curDrawnROI);
   }
   
   SUMA_RETURN(YUP);
}

/*!
   \brief return  +1 if surface is typically the outer layer surface used in Vol2Surf
                  -1 if surface is typically the inner layer surface used in Vol2Surf
                   0 if no comment 
*/
int SUMA_isTypicalSOforVolSurf (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_isTypicalSOforVolSurf"};
   SUMA_ENTRY;
   
   switch (SO->FileType) {
      case SUMA_PLY:
      case SUMA_VEC:
      case SUMA_FREE_SURFER: 
         if (SUMA_iswordin (SO->Name.FileName, "smoothwm")) SUMA_RETURN(-1);
         else if (SUMA_iswordin (SO->Name.FileName, "pial")) SUMA_RETURN(1);
         else SUMA_RETURN(0);
         break;
      default:
         SUMA_RETURN(0);
         break;
   }
   
   SUMA_RETURN(0);
}


SUMA_XFORM *SUMA_Find_XformByID(char *idcode_str)
{
   static char FuncName[]={"SUMA_Find_XformByID"};
   SUMA_XFORM *xf = NULL, *xft=NULL;
   DListElmt *el=NULL;
   DList *lst = SUMAg_CF->xforms;
   
   SUMA_ENTRY;
   
   if (!lst || !idcode_str) SUMA_RETURN(xf);
   
   el = dlist_head(lst);
   
   while (el && !xf) {
      xft = (SUMA_XFORM *)el->data;
      if (!strcmp(xft->idcode_str,idcode_str)) {
         xf = xft; break;
      }   
      el = dlist_next(el);
   } 
   
   SUMA_RETURN(xf);
}
   

/*!
   Find a transform with a particular name AND a parent with a matching idcode
*/
SUMA_XFORM *SUMA_Find_XformByParent(char *name, char *parent_idcode, int *iloc)
{
   static char FuncName[]={"SUMA_Find_XformByParent"};
   SUMA_XFORM *xf = NULL, *xft=NULL;
   DListElmt *el=NULL;
   DList *lst = SUMAg_CF->xforms;
   int i;
    
   SUMA_ENTRY;
   
   if (!lst || !name || !parent_idcode) SUMA_RETURN(xf);
   
   el = dlist_head(lst);
   
   while (el && !xf) {
      xft = (SUMA_XFORM *)el->data;
      if (!strcmp(xft->name,name)) {
         for (i=0; i<xft->N_parents; ++i) {
            if (!strcmp(xft->parents[i], parent_idcode)) {
               xf = xft; 
               if (iloc) *iloc = i; 
               break;
            }
         }
      }   
      el = dlist_next(el);
   }  
   
   SUMA_RETURN(xf);
}

SUMA_XFORM *SUMA_Find_XformByChild(char *name, char *child_idcode, int *iloc)
{
   static char FuncName[]={"SUMA_Find_XformByChild"};
   SUMA_XFORM *xf = NULL, *xft=NULL;
   DListElmt *el=NULL;
   DList *lst = SUMAg_CF->xforms;
   int i;
    
   SUMA_ENTRY;
   
   if (!lst || !name || !child_idcode) SUMA_RETURN(xf);
   
   el = dlist_head(lst);
   
   while (el && !xf) {
      xft = (SUMA_XFORM *)el->data;
      if (!strcmp(xft->name,name)) {
         for (i=0; i<xft->N_children; ++i) {
            if (!strcmp(xft->children[i], child_idcode)) {
               xf = xft; 
               if (iloc) *iloc = i; 
               break;
            }
         }
      }   
      el = dlist_next(el);
   }  
   
   SUMA_RETURN(xf);
}


void SUMA_Show_Xforms (DList *dl, FILE *Out, int detail)
{
   static char FuncName[]={"SUMA_Show_Xforms"};
   char *s = NULL;  
   
   SUMA_ENTRY;

   if (Out == NULL) Out = stdout;
   
   s = SUMA_Xforms_Info (dl, detail);
   
   if (s) {
      fprintf(Out, "%s", s);
      SUMA_free(s); s = NULL;
   }else {
      SUMA_SL_Err("Failed in SUMA_Xforms_Info");
   }
   
   SUMA_RETURNe;
}

char *SUMA_Xforms_Info(DList *dl, int detail) 
{
   static char FuncName[]={"SUMA_Xforms_Info"};
   DListElmt *el=NULL;
   char *s = NULL;
   SUMA_STRING *SS = NULL;
   SUMA_XFORM *xf=NULL;
   SUMA_DSET *dset=NULL;
   SUMA_SurfaceObject *SO=NULL;
   int i;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
  
   if (!dl) {
      SS = SUMA_StringAppend(SS,"NULL Overlay Xforms List\n");
      SUMA_SS2S(SS, s);
      SUMA_RETURN(s);
   } else {
      SS = SUMA_StringAppend_va(SS,"%d Overlay Xforms in list.\n", 
                                 dlist_size(dl));
   }
   
   el = dlist_head(dl);
   while(el) {
      xf = (SUMA_XFORM *)el->data;
      SS = SUMA_StringAppend_va( SS,"Xform name: %s, id: %s\n"
                                    "           active (1=Y, -1=N): %d\n",
                                 xf->name, xf->idcode_str,
                                 xf->active);
      for (i=0; i<xf->N_parents; ++i) {
         SS = SUMA_StringAppend_va( SS,
                                    "  parent %d:  %s\n",
                                    i, xf->parents[i]);
         if (SUMA_is_ID_4_SO(xf->parents[i], &SO)) {
            SS = SUMA_StringAppend_va( SS,
                                    "     SO labeled %s \n",
                                    CHECK_NULL_STR(SO->Label));
         } else if (SUMA_is_ID_4_DSET(xf->parents[i], &dset)) {
            SS = SUMA_StringAppend_va( SS,
                                    "     DSET labeled %s \n",
                                    CHECK_NULL_STR(SDSET_LABEL(dset)));
         } else {
            SS = SUMA_StringAppend_va( SS,
                                    "     %s is neither SO, not DSET.\n",
                                    xf->parents[i]);
         }
         if (SUMA_is_ID_4_SO(xf->parents_domain[i], &SO)) {
            SS = SUMA_StringAppend_va( SS,
                                    "  parent_domain: %s, labeled %s\n",
                                    xf->parents_domain[i],
                                    CHECK_NULL_STR(SO->Label));
         } else {
            SS = SUMA_StringAppend_va( SS,
                                    "  parent_domain: %s, Not found!\n",
                                    xf->parents_domain[i]);
         }
                                 
      }
      
      for (i=0; i<xf->N_children; ++i) {
         SS = SUMA_StringAppend_va( SS,
                                    "  child %d:  %s\n",
                                    i, xf->children[i]);
         if (SUMA_is_ID_4_SO(xf->children[i], &SO)) {
            SS = SUMA_StringAppend_va( SS,
                                    "     SO labeled %s \n",
                                    CHECK_NULL_STR(SO->Label));
         } else if (SUMA_is_ID_4_DSET(xf->parents[i], &dset)) {
            SS = SUMA_StringAppend_va( SS,
                                    "     DSET labeled %s \n",
                                    CHECK_NULL_STR(SDSET_LABEL(dset)));
         } else {
            SS = SUMA_StringAppend_va( SS,
                                    "     %s is neither SO, not DSET.\n",
                                    xf->children[i]);
         }
      }
      
      if (xf->XformOpts) {
         s = SUMA_NI_nel_Info((NI_element *)xf->XformOpts, detail);
         SS = SUMA_StringAppend_va( SS,
                                    "  XformOpts is:\n"
                                    "%s\n",
                                    s);
         SUMA_free(s);
      } else {
         SS = SUMA_StringAppend_va( SS,
                                    "  XformOpts is NULL"); 
      }
        
      if (xf->gui) {
         SS = SUMA_StringAppend_va( SS,
                                    "     GUI is not null");
      } else {
         SS = SUMA_StringAppend_va( SS,
                                    "     GUI is null");
      }
      
      SS = SUMA_StringAppend(SS, "\n");
      
      el = dlist_next(el);
   }
   
   SUMA_SS2S(SS, s);

   SUMA_RETURN(s);
}

void SUMA_Show_Callbacks (DList *dl, FILE *Out, int detail)
{
   static char FuncName[]={"SUMA_Show_Callbacks"};
   char *s = NULL;  
   
   SUMA_ENTRY;

   if (Out == NULL) Out = stdout;
   
   s = SUMA_Callbacks_Info (dl, detail);
   
   if (s) {
      fprintf(Out, "%s", s);
      SUMA_free(s); s = NULL;
   }else {
      SUMA_SL_Err("Failed in SUMA_Callbacks_Info");
   }
   
   SUMA_RETURNe;
}

char *SUMA_Callbacks_Info(DList *dl, int detail)
{
   static char FuncName[]={"SUMA_Callbacks_Info"};
   char *s = NULL;
   DListElmt *el=NULL;
   NI_group *ngr=NULL;
   NI_element *nel = NULL;
   SUMA_STRING *SS = NULL;
   SUMA_DSET *dset=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_CALLBACK *cb=NULL;
   SUMA_XFORM *xf=NULL;
   int i;

   SUMA_ENTRY;
   SS = SUMA_StringAppend(NULL, NULL);
  
   if (!dl) {
      SS = SUMA_StringAppend(SS,"NULL Callbacks List\n");
      SUMA_SS2S(SS, s);
      SUMA_RETURN(s);
   } else {
      SS = SUMA_StringAppend_va(SS,"%d Callbacks in list.\n", 
                                 dlist_size(dl));
   }
   
   el = dlist_head(dl);
   while(el) {
      cb = (SUMA_CALLBACK *)el->data;
      xf = SUMA_Find_XformByID(cb->creator_xform);
      SS = SUMA_StringAppend_va( SS,"CB trigger event: %d \n"
                                    "           active (1=Y, -1=N): %d\n"
                                    "           pending (1=Y, 0=N): %d\n"
                                    "           trigger source %d\n"
                                    , cb->event
                                    , cb->active, cb->pending
                                    , cb->trigger_source );
      if (xf) {
         SS = SUMA_StringAppend_va( SS,"   Creator Xform: %s\n", xf->name);
      } else {
         SS = SUMA_StringAppend_va( SS,"   No creator xform found.\n");
      }
      for (i=0; i<cb->N_parents; ++i) {
         SS = SUMA_StringAppend_va( SS,
                                    "  parent %d:  %s\n",
                                    i, cb->parents[i]);
                               
         if (SUMA_is_ID_4_SO(cb->parents[i], &SO)) {
            SS = SUMA_StringAppend_va( SS,
                                    "     SO labeled %s \n",
                                    CHECK_NULL_STR(SO->Label));
         } else if (SUMA_is_ID_4_DSET(cb->parents[i], &dset)) {
            SS = SUMA_StringAppend_va( SS,
                                    "     DSET labeled %s \n",
                                    CHECK_NULL_STR(SDSET_LABEL(dset)));
         } else {
            SS = SUMA_StringAppend_va( SS,
                                    "     %s is neither SO, not DSET.\n",
                                    cb->parents[i]);
         }
         if (SUMA_is_ID_4_SO(cb->parents_domain[i], &SO)) {
            SS = SUMA_StringAppend_va( SS,
                                    "  parent_domain: %s, labeled %s\n",
                                    cb->parents_domain[i],
                                    CHECK_NULL_STR(SO->Label));
         } else {
            SS = SUMA_StringAppend_va( SS,
                                    "  parent_domain: %s, Not found!\n",
                                    cb->parents_domain[i]);
         }
      }

      SS = SUMA_StringAppend_va( SS, 
                                 "  Function Name %s (%p)\n", 
                                 cb->FunctionName, cb->FunctionPtr);
      
      s = SUMA_NI_nel_Info((NI_element *)cb->FunctionInput, detail);
      SS = SUMA_StringAppend_va( SS, 
                                 "  Function Params:\n%s\n-----\n",
                                 s); SUMA_free(s); s = NULL;
                                                
      SS = SUMA_StringAppend(SS, "\n");
      
      if (detail > 1) {
         SUMA_S_Note("Detailed nel view\n");
         SUMA_ShowNel(nel);
      }
      el = dlist_next(el);
   }

   
   SUMA_SS2S(SS, s);

   SUMA_RETURN(s);
}



SUMA_Boolean SUMA_SetXformActive(SUMA_XFORM *xf, int active, int fromgui)
{
   static char FuncName[]={"SUMA_SetXformActive"};
   SUMA_CALLBACK *cb;
   DListElmt *el=NULL;
   DList *dl=SUMAg_CF->callbacks;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   if (!xf) SUMA_RETURN(NOPE);
   
   xf->active = active;
   
   if (!xf->gui) {
      /* create GUI */
      SUMA_CreateXformInterface(xf);
   } else if (!fromgui){
      /* Raise GUI */
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: raising Xform GUI window \n", FuncName);
      XMapRaised(SUMAg_CF->X->DPY_controller1, 
                  XtWindow(xf->gui->AppShell));
   }
   
   if (0 && !fromgui) { /* not sure why I have this here. 
                           SUMA_InitializeXformInterface is called in SUMA_D_Key,
                           which is the function called in both GUI and non GUI 
                           modes... */
      /* initialize the gui */
      SUMA_InitializeXformInterface(xf);
   }
   
   if (!dl) SUMA_RETURN(YUP);
   
   /* Now reflect that in all callbacks that were created by xform */
   el = dlist_head(dl);
   while (el) {
      cb = (SUMA_CALLBACK *)el->data;
      if (!strcmp(cb->creator_xform, xf->idcode_str)) {
         cb->active = active; 
         if (!cb->active < 1) { 
            SUMA_SetCallbackPending(cb, 0, SES_Empty);
         }
      }
      el = dlist_next(el);
   }
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_SetXformShowPreProc(SUMA_XFORM *xf, int ShowPreProc, 
                                      int fromgui)
{
   static char FuncName[]={"SUMA_SetXformShowPreProc"};
   SUMA_CALLBACK *cb;
   DListElmt *el=NULL;
   DList *dl=SUMAg_CF->callbacks;
   NI_element *dotopt=NULL;
   int ii=0;
   SUMA_DSET *in_dset=NULL, *pp_dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   if (!xf) SUMA_RETURN(NOPE);
   
   xf->ShowPreProc = ShowPreProc;
   
   if (!xf->gui) {
      /* create GUI */
      SUMA_CreateXformInterface(xf);
   } else if (!fromgui){
      /* Raise GUI */
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: raising Xform GUI window \n", FuncName);
      XMapRaised(SUMAg_CF->X->DPY_controller1, 
                  XtWindow(xf->gui->AppShell));
   }
   
   if (!fromgui) {
      /* initialize the gui */
      SUMA_InitializeXformInterface(xf);
   }
   
   if (!dl) SUMA_RETURN(YUP);
   
   /* Now do something about showing */
   if (!strcmp(xf->name,"Dot")) {
      if (!(dotopt = SUMA_FindNgrNamedElement(xf->XformOpts, "dotopts"))) {
         SUMA_S_Err("dotopt not found");
         SUMA_RETURN(NOPE);
      }
      for (ii=0; ii<xf->N_parents; ++ii) {
         if (!SUMA_is_ID_4_DSET(xf->parents[ii], &in_dset)) {
            SUMA_S_Err("Parent not found");
            SUMA_RETURN(NOPE);
         }
         if (!(pp_dset = SUMA_GetDotPreprocessedDset(in_dset, dotopt))){
            SUMA_S_Err("PreProcParent not found");
            SUMA_RETURN(NOPE);
         }
         /* now make it visible */
         /* For this to work, you'll need to have a version of the
         block under LoadDsetFile beginning from OverInd = -1;
         To make matters more exciting, you'll need to update this sucker,
         any time that the preprocessing is redone because of changes in 
         orts or bandpass, etc. So leave this feature out for now */  
      }
   } else {
      SUMA_S_Errv("Don't know how to do this for %s\n", xf->name);
      SUMA_RETURN(NOPE);
   }
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_is_XformParent (SUMA_XFORM *xf, char *id, int *iloc)
{
   static char FuncName[]={"SUMA_is_XformParent"};
   int ii;
   
   SUMA_ENTRY;
   
   if (!xf || !id) SUMA_RETURN(NOPE);
   
   for (ii=0; ii<xf->N_parents; ++ii) {
      if (!strcmp(xf->parents[ii],id)) {
         if (iloc) *iloc=ii;
         SUMA_RETURN(YUP);
      }
   }
   
   SUMA_RETURN(NOPE);
}

SUMA_Boolean SUMA_AddXformParent (SUMA_XFORM *xf, 
                                  char *parent_idcode, char *parent_domain)
{
   static char FuncName[]={"SUMA_AddXformParent"};
   SUMA_DSET *dset=NULL;
   
   SUMA_ENTRY;
   
   if (!xf || !parent_idcode) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   if (SUMA_is_XformParent(xf, parent_idcode, NULL)) {
      SUMA_S_Err("Parent exists");
      SUMA_RETURN(NOPE);
   }
   
   strcpy(xf->parents[xf->N_parents], parent_idcode); 
   if (!parent_domain) {
      if (SUMA_is_ID_4_DSET(parent_idcode, &dset)) {
         strcpy( xf->parents_domain[xf->N_parents],
                 SDSET_IDMDOM(dset));
      } else {
         xf->parents_domain[xf->N_parents][0] = '\0';
      }
   } else {
      strcpy( xf->parents_domain[xf->N_parents],
              parent_domain);
   }
   
   
   ++xf->N_parents;
   SUMA_RETURN(YUP);
   
}

SUMA_Boolean SUMA_is_XformChild (SUMA_XFORM *xf, char *id, int *iloc)
{
   static char FuncName[]={"SUMA_is_XformChild"};
   int ii;
   
   SUMA_ENTRY;
   
   if (!xf || !id) SUMA_RETURN(NOPE);
   
   for (ii=0; ii<xf->N_children; ++ii) {
      if (!strcmp(xf->children[ii],id)) {
         if (iloc) *iloc=ii;
         SUMA_RETURN(YUP);
      }
   }
   
   SUMA_RETURN(NOPE);
}

SUMA_Boolean SUMA_AddXformChild (SUMA_XFORM *xf, 
                                 char *child_idcode)
{
   static char FuncName[]={"SUMA_AddXformChild"};
   SUMA_DSET *dset=NULL;
   int ii;
   
   SUMA_ENTRY;
   
   if (!xf || !child_idcode) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   if (SUMA_is_XformChild(xf, child_idcode, NULL)) {
      SUMA_S_Err("Child exists");
      SUMA_RETURN(NOPE);
   }
   
   strcpy(xf->children[xf->N_children], child_idcode); 
   
   ++xf->N_children;
   SUMA_RETURN(YUP);
   
}


SUMA_XFORM *SUMA_NewXform(char *name, char *parent_idcode, char *parent_domain)
{
   static char FuncName[]={"SUMA_NewXform"};
   SUMA_XFORM *xf = NULL;
   
   SUMA_ENTRY;
   if (!name || !parent_idcode) SUMA_RETURN(xf);
   
   if (  !SUMA_is_ID_4_SO(parent_idcode, NULL) && 
         !SUMA_is_ID_4_DSET(parent_idcode, NULL) ) {
      SUMA_S_Err("Invalid parent_idcode");
      SUMA_RETURN(xf);     
   }
   
   if (SUMA_Find_XformByParent("Dot", parent_idcode, NULL)) {
      SUMA_S_Err("An xform exists already");
      SUMA_RETURN(xf);
   }
   
   xf = (SUMA_XFORM *)SUMA_calloc(1, sizeof(SUMA_XFORM));
 
   snprintf(xf->name, 127*sizeof(char), "%s", name);
   UNIQ_idcode_fill(xf->idcode_str);
   
   if (!SUMA_AddXformParent(xf,parent_idcode, parent_domain)) {
      SUMA_S_Err("Failed to add parent");
      SUMA_FreeXform(xf); xf = NULL;
      SUMA_RETURN(xf); 
   }

   
   xf->N_children = 0;
   
   xf->active = 0;
   xf->ShowPreProc = 0;
   
   xf->XformOpts = NI_new_group_element();
   NI_rename_group(xf->XformOpts, "XformOpts");
   
   dlist_ins_next(SUMAg_CF->xforms, dlist_tail(SUMAg_CF->xforms), xf);
   
   SUMA_RETURN(xf);  
} 

void SUMA_FreeXform(void *data)
{
   static char FuncName[]={"SUMA_FreeXform"};
   SUMA_XFORM *xf = (SUMA_XFORM *)data;
   
   SUMA_ENTRY;
   
   if (xf) {
      if (xf->XformOpts) NI_free(xf->XformOpts); 
      if (xf->gui) SUMA_FreeXformInterface(xf->gui);
      SUMA_free(xf);
   }
   
   SUMA_RETURNe;
}

SUMA_GENERIC_XFORM_INTERFACE * SUMA_NewXformInterface(
                                 SUMA_XFORM *xf)
{
   static char FuncName[]={"SUMA_NewXformInterface"};
   SUMA_GENERIC_XFORM_INTERFACE *gui=NULL;
   
   SUMA_ENTRY;
   
   
   gui = (SUMA_GENERIC_XFORM_INTERFACE *)
            SUMA_calloc(1,sizeof(SUMA_GENERIC_XFORM_INTERFACE));
   
   gui->AF0 = 
      (SUMA_ARROW_TEXT_FIELD *)calloc(1, sizeof(SUMA_ARROW_TEXT_FIELD));
   gui->AF1 = 
      (SUMA_ARROW_TEXT_FIELD *)calloc(1, sizeof(SUMA_ARROW_TEXT_FIELD));
   gui->AF2 = 
      (SUMA_ARROW_TEXT_FIELD *)calloc(1, sizeof(SUMA_ARROW_TEXT_FIELD));
  
   SUMA_RETURN(gui);
}

void SUMA_FreeXformInterface(SUMA_GENERIC_XFORM_INTERFACE *gui)
{
   static char FuncName[]={"SUMA_FreeXformInterface"};
   
   SUMA_ENTRY;
   if (gui) {
      if (gui->AF0) SUMA_free(gui->AF0);
      if (gui->AF1) SUMA_free(gui->AF1);
      if (gui->AF2) SUMA_free(gui->AF2);
      
      SUMA_free(gui);
   }
   SUMA_RETURNe;
}

SUMA_Boolean SUMA_is_CallbackParent (SUMA_CALLBACK *cb, char *id, int *iloc)
{
   static char FuncName[]={"SUMA_is_CallbackParent"};
   int ii;
   
   SUMA_ENTRY;
   
   if (!cb || !id) SUMA_RETURN(NOPE);
   
   for (ii=0; ii<cb->N_parents; ++ii) {
      if (!strcmp(cb->parents[ii],id)) {
         if (iloc) *iloc=ii;
         SUMA_RETURN(YUP);
      }
   }
   
   SUMA_RETURN(NOPE);
}


/*!
   Find a callback with a particular name AND a parent with a matching idcode
*/
SUMA_CALLBACK *SUMA_Find_CallbackByParent(char *FunctionName, 
                                       char *parent_idcode, int *iloc)
{
   static char FuncName[]={"SUMA_Find_CallbackByParent"};
   SUMA_CALLBACK *cb = NULL, *cbt=NULL;
   DListElmt *el=NULL;
   DList *lst = SUMAg_CF->callbacks;
   int i;
    
   SUMA_ENTRY;
   
   if (!lst || !FunctionName || !parent_idcode) SUMA_RETURN(cb);
   
   el = dlist_head(lst);
   
   while (el && !cb) {
      cbt = (SUMA_CALLBACK *)el->data;
      if (!strcmp(cbt->FunctionName, FunctionName)) {
         for (i=0; i<cbt->N_parents; ++i) {
            if (!strcmp(cbt->parents[i], parent_idcode)) {
               cb = cbt; 
               if (iloc) *iloc = i;
               break;
            }
         }
      }   
      el = dlist_next(el);
   }  
   
   SUMA_RETURN(cb);
}

SUMA_CALLBACK *SUMA_Find_CallbackByCreatorXformID(char *creator_xform_idcode)
{
   static char FuncName[]={"SUMA_Find_CallbackByCreatorXformID"};
   SUMA_CALLBACK *cb = NULL, *cbf=NULL;
   DListElmt *el=NULL;
   DList *lst = SUMAg_CF->callbacks;
   int i, found=0;
    
   SUMA_ENTRY;
   
   if (!lst || !creator_xform_idcode) SUMA_RETURN(cbf);
   
   el = dlist_head(lst);
   cbf=NULL;
   while (el && !cbf) {
      cb = (SUMA_CALLBACK *)el->data;
      if (!strcmp(cb->creator_xform, creator_xform_idcode)) {
         ++found; cbf = cb;   
      }   
      el = dlist_next(el);
   }  
   if (found > 1) {
      SUMA_S_Errv("%d callbacks found\n" 
                  "write a new function to return them all\n",
                  found); 
   }
   SUMA_RETURN(cbf);
}

SUMA_Boolean SUMA_AddCallbackParent (SUMA_CALLBACK *cb, 
                                     char *parent_idcode,
                                     char *parent_domain)
{
   static char FuncName[]={"SUMA_AddCallbackParent"};
   SUMA_DSET *dset=NULL;
   
   SUMA_ENTRY;
   
   if (!cb || !parent_idcode) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   
   if (SUMA_is_CallbackParent(cb, parent_idcode, NULL)) {
      SUMA_S_Err("Parent exists");
      SUMA_RETURN(NOPE);
   }
   
   strcpy(cb->parents[cb->N_parents], parent_idcode);    
   if (!parent_domain) {
      if (SUMA_is_ID_4_DSET(parent_idcode, &dset)) {
         strcpy( cb->parents_domain[cb->N_parents],
                 SDSET_IDMDOM(dset));
      } else {
         cb->parents_domain[cb->N_parents][0] = '\0';
      }
   } else {
      strcpy( cb->parents_domain[cb->N_parents],
              parent_domain);
   }
   
   ++cb->N_parents;
   SUMA_RETURN(YUP);
   
}

SUMA_Boolean SUMA_SetCallbackPending (SUMA_CALLBACK *cb, 
                                      SUMA_Boolean pen, SUMA_ENGINE_SOURCE src) 
{
   static char FuncName[]={"SUMA_SetCallbackPending"};
   
   SUMA_ENTRY;
   
   if (!cb) SUMA_RETURN(NOPE);
   
   if (cb->active < 1 && pen) {
      SUMA_S_Notev("Callback %s inactive. Pending flag not set\n", 
                  cb->FunctionName);
      SUMA_RETURN(YUP);
   }
   
   if (src <= SES_Empty && pen) {
      SUMA_S_Errv("Source %d is not appropriate.\n",src);
      SUMA_RETURN(NOPE);
   }
   
   if (cb->pending && pen) {
      SUMA_S_Errv("Callback %s is already pending. \n", 
                  cb->FunctionName);
      SUMA_RETURN(NOPE);
   }
   
   cb->pending = pen;
   if (!pen) cb->trigger_source = SES_Empty;
   else cb->trigger_source = pen;
   
   SUMA_RETURN(YUP);
}

SUMA_CALLBACK *SUMA_NewCallback  (char *FunctionName, 
                               SUMA_CALLBACK_ACTIVATE_EVENTS event, 
                               void *FunctionPtr,
                               char *parent_idcode,
                               char *parent_domain, 
                               char *creator_xform)
{
   static char FuncName[]={"SUMA_NewCallback"};
   SUMA_XFORM *xf = NULL;
   NI_element *nel=NULL;
   SUMA_CALLBACK *cb = NULL;
   char stmp[256];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Checks");
   if (!parent_idcode || !FunctionName ||
       strlen(FunctionName) > 125  ) SUMA_RETURN(cb);
   
   if (  !SUMA_is_ID_4_SO(parent_idcode, NULL) && 
         !SUMA_is_ID_4_DSET(parent_idcode, NULL) ) {
      SUMA_S_Err("Invalid parent_idcode");
      SUMA_RETURN(cb);     
   }
 
   if (SUMA_Find_CallbackByParent(FunctionName, parent_idcode, NULL)) {
      SUMA_S_Err("A callback exists already");
      SUMA_RETURN(cb);
   }
   
   SUMA_LH("New callback");
   cb = (SUMA_CALLBACK *)calloc(1,sizeof(SUMA_CALLBACK));
   
   cb->event = event;
   cb->active = 0;
   cb->pending = 0;
   cb->trigger_source = SES_Empty;
   strcpy(cb->FunctionName, FunctionName); 
   cb->FunctionPtr = FunctionPtr;
   
   SUMA_LH("Func input");
   cb->FunctionInput = NI_new_group_element();
   SUMA_LH(FunctionName);
   snprintf(stmp,sizeof(char)*64,"input.%s",FunctionName);
   NI_rename_group(cb->FunctionInput, stmp);
   
   SUMA_LH("defaults");
   /* Set defaults for generic parameters */
   nel = NI_new_data_element("event_parameters", 0); 
   NI_add_to_group(cb->FunctionInput, nel);
   NI_SET_INT(nel,"event.new_node", -1);
   NI_set_attribute(nel, "event.SO_idcode", "");
   NI_set_attribute(nel,"event.overlay_name", "");
  
   SUMA_LH("Adding parent"); 
   if (!SUMA_AddCallbackParent(cb, parent_idcode, parent_domain)){
      SUMA_S_Err("Failed to add parent");
      SUMA_RETURN(NOPE);
   }
   
      
   if (creator_xform) {
      if (!(xf=SUMA_Find_XformByID(creator_xform))) {
         SUMA_S_Err("Failed to find xform");
         SUMA_RETURN(NOPE);
      }
      strcpy(cb->creator_xform, creator_xform);   
   }
      
   dlist_ins_next(SUMAg_CF->callbacks, dlist_tail(SUMAg_CF->callbacks), cb);
 
   SUMA_RETURN(cb);  
}  


void SUMA_FreeCallback(void *data)
{
   static char FuncName[]={"SUMA_FreeCallback"};
   SUMA_CALLBACK *cb = (SUMA_CALLBACK *)data;
   
   SUMA_ENTRY;
   
   if (cb) {
      if (cb->FunctionInput) {
         NI_free_element(cb->FunctionInput);
      }
      SUMA_free(cb);
   }
   
   SUMA_RETURNe;
}

SUMA_Boolean SUMA_FlushCallbackEventParameters (SUMA_CALLBACK *cb) 
{
   static char FuncName[]={"SUMA_FlushCallbackEventParameters"};
   NI_element *nelpars=NULL;
   
   SUMA_ENTRY;
   
   if (!cb ||
       !(nelpars = SUMA_FindNgrNamedElement(
                     cb->FunctionInput, "event_parameters"))) {
      SUMA_S_Err("NULL cb or Bad callback content");
      SUMA_RETURN(NOPE);
   }
   
   switch (cb->event) {
      case SUMA_NEW_NODE_ACTIVATE_EVENT:
         SUMA_XFORM_SAVE_FLUSH_EVENT(nelpars);
         break;
      case SUMA_ERROR_ACTIVATE_EVENT: 
      case SUMA_NO_ACTIVATE_EVENT:
      case SUMA_N_ACTIVATE_EVENTS:
         SUMA_S_Warn("This should not come up");
         break;
      default:
         SUMA_S_Err("Seriously off folks");
         SUMA_RETURN(NOPE);
         break;
   }
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_ExecuteCallback(SUMA_CALLBACK *cb, 
                                  int refresh, SUMA_SurfaceObject *SO,
                                  int doall) 
{
   static char FuncName[]={"SUMA_ExecuteCallback"};
   SUMA_SurfaceObject *curSO=NULL, *targetSO=NULL;
   SUMA_OVERLAYS *targetSover=NULL;
   int i, jj=0;
   SUMA_DSET *targetDset=NULL;
   SUMA_Boolean LocalHead = NOPE;
  
   SUMA_ENTRY;
   
   cb->FunctionPtr((void *)cb);  

   SUMA_SetCallbackPending(cb, 0, SES_Empty);
   
   /* flush event specific parameters */
   SUMA_FlushCallbackEventParameters(cb);
   
   if (refresh) {/* Now decide on what needs refreshing */
      if (!SO) {
         curSO = NULL;
      } else {
         curSO = *(SO->SurfCont->curSOp);
      }           
      for (i=0; i<cb->N_parents; ++i) {
         if (SUMA_is_ID_4_DSET(cb->parents[i], &targetDset)) {
            targetSO = SUMA_findSOp_inDOv(cb->parents_domain[i],
                                          SUMAg_DOv, SUMAg_N_DOv);
            if (!targetSO) {
               if (SO) {
                  SUMA_S_Warn("Could not find targetSO, using SO instead");
                  targetSO = SO;
               } else {
                  SUMA_S_Err("Don't know what do do here");
                  SUMA_RETURNe;
               }
            }
            /* refresh overlay and SO for this callback */
            targetSover = SUMA_Fetch_OverlayPointerByDset(
                                 targetSO->Overlays, 
                                 targetSO->N_Overlays,
                                 targetDset,
                                 &jj);
            SUMA_LHv("Colorizing %s\n", targetSover->Name);
            SUMA_ColorizePlane(targetSover);
            SUMA_LHv("Setting remix flag for %s\n",targetSO->Label);
            if (!SUMA_SetRemixFlag( targetSO->idcode_str, 
                                    SUMAg_SVv, SUMAg_N_SVv)) {
               SUMA_SLP_Err("Failed in SUMA_SetRemixFlag.\n");
               SUMA_RETURN(NOPE);
            }
            if (doall || curSO != targetSO) {
               SUMA_UPDATE_ALL_NODE_GUI_FIELDS(targetSO);
               SUMA_RemixRedisplay(targetSO);
            } else {
               /* Update and Remix will be done for curSO 
                  by the function who called this one */
            }
            /* it is possible that the callback caused a change 
               in the p value so update it to be sure, 
               but I am not fond of doing this all the time,
               every time there is a button click triggering the
               event .... */
            SUMA_LHv("Updating threshold at %f\n", 
                     targetSO->SurfCont->curColPlane->OptScl->ThreshRange[0]);
            SUMA_UpdatePvalueField( targetSO,
                     targetSO->SurfCont->curColPlane->OptScl->ThreshRange[0]);
         } else if (SUMA_is_ID_4_SO(cb->parents[i], &targetSO)) {
            SUMA_S_Note("Got surface, don't know \n"
                        "what to do in case like this yet\n");
         } else {
            SUMA_S_Err("Dunno what to do with such an object...");
         }   
      }

   }
   SUMA_RETURN(YUP);
}

