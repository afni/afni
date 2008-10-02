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
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Comparing \n\t:%s:to\n\t:%s:\n", \
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
   \param dov (SUMA_DO*) pointer to vector of Displayable Objects, typically SUMAg_DOv
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
   level 2 means extended family (share a grandparent, ultimately this could be the grand icosahedron duc
                                 Surfaces must be the of the same hemi side since both hemispheres can have
                                 the same grandparent, use level 3 if you want to go across hemis.)
   level 3 is like level 2 with no care for what side of the brain we're working with
   For more definitions on relationships:
   
   \sa SUMA_WhatAreYouToMe
*/

SUMA_Boolean SUMA_isRelated ( SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2, int level)
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
                  if (SO1->Side < SUMA_LR || SO2->Side < SUMA_LR) {
                     SUMA_S_Note("Surfaces appear related at level 2 but sides are not the same.\n"
                                 "Kinship level is being ignored.\n");
                     SUMA_S_Note("Surface sides are not clearly defined, if this is in error\n"
                              "Consider adding Hemisphere = R  (or L or B) in the spec file\n"
                              "to make sure surfaces sides are correctly labeled.\n");
                  }
               }
         }
         break;
      case 1: /* nuclear family only */
         if (  (kin > SUMA_DOMAINS_NOT_RELATED) && 
               (kin < SUMA_NUCELAR_FAMILY ) ) {
               if (SO1->Side == SO2->Side) {
                   SUMA_RETURN(YUP); /* last condition is not really necessary but it don't hoyt...*/
               } else {
                  SUMA_S_Note("Surfaces appear related at level 2 but sides are not the same.\n"
                                 "Kinship level is being ignored.\n");
                  if (SO1->Side < SUMA_LR || SO2->Side < SUMA_LR) {
                     SUMA_S_Note("Surface sides are not clearly defined, if this is in error\n"
                                 "Consider adding Hemisphere = R  (or L or B) in the spec file\n"
                                 "to make sure surfaces sides are correctly labeled.\n");
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
SUMA_DOMAIN_KINSHIPS SUMA_WhatAreYouToMe (SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2)
{
   static char FuncName[]={"SUMA_WhatAreYouToMe"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO1->idcode_str || !SO2->idcode_str) {
      fprintf (SUMA_STDERR, "Error %s: NULL idcode_str.\n", FuncName);
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

