#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 
extern SUMA_DO *SUMAg_DOv;   
extern int SUMAg_N_DOv; 

/*! functions dealing with Drawable Object Manipulation */

/*!
   This function Links one Inode to another.
   This is different from CreateInode in that no new node is being allocated for, only a link is created.
   SUMA_CreateInodeLink (SUMA_INODE * FromIN, SUMA_Inode *ToIN)
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (!IN) {
      fprintf (SUMA_STDERR,"Error %s: Inode is null. Returning -1.\n", FuncName);
      SUMA_RETURN(-1);
   }
   if (!IN->N_link) {
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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
      case LS_type:
         SUMA_free_SegmentDO ((SUMA_SegmentDO *)dov->OP);
         break;
      case AO_type:
         fprintf(SUMA_STDERR,"Error SUMA_Free_Displayable_Object, Not trained to free AO objects\n");
         break;
      case GO_type:
         fprintf(SUMA_STDERR,"Error SUMA_Free_Displayable_Object, Not trained to free GO objects\n");
         break;
         
   }   

   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_Free_Displayable_Object_Vect (SUMA_DO *dov, int N)
{
   static char FuncName[] = {"SUMA_Free_Displayable_Object_Vect"};
   int i;
   SUMA_Boolean Ret = YUP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   for (i=0; i < N; ++i) {
      if (&dov[i] != NULL) {
         Ret = Ret * SUMA_Free_Displayable_Object (&dov[i]);
      }
   }
   if (dov) SUMA_free(dov);
   SUMA_RETURN(Ret);

}   
/*!
Add a displayable object to dov
*/
SUMA_Boolean SUMA_AddDO(SUMA_DO *dov, int *N_dov, void *op, SUMA_DO_Types DO_Type, SUMA_DO_CoordType DO_CoordType)
{
   static char FuncName[] = {"SUMA_AddDO"};
   SUMA_Boolean Shaded=NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* make sure you did not exceed allocated space */
   if (*N_dov >= SUMA_MAX_DISPLAYABLE_OBJECTS) {
      SUMA_error_message (FuncName, "Reached limit of DOv storage",0);
      SUMA_RETURN(NOPE);
   }
   dov[*N_dov].OP = op;
   dov[*N_dov].ObjectType = DO_Type;
   dov[*N_dov].CoordType = DO_CoordType;
   *N_dov = *N_dov+1;
   
   /* Make some updates if needed */
   
   /* is the Switch ROI window open ? */
   SUMA_IS_DRAW_ROI_SWITCH_ROI_SHADED(Shaded);
   if (!Shaded) {
      SUMA_cb_DrawROI_SwitchROI (NULL, (XtPointer) SUMAg_CF->X->DrawROI->SwitchROIlst, NULL);
   }
   
   SUMA_RETURN(YUP);
}

/*!
Register a DO with surface viewer ShowDO vector 
if dov_id is present in cSV, nothing is done
if not found then dov_id is registered at the end of cSV->ShowDO 

When a DO is registered, a ColorList is created if the DO is a surface object.
and N_DO is updated
*/
SUMA_Boolean SUMA_RegisterDO(int dov_id, SUMA_SurfaceViewer *cSV)
{
   int i;
   static char FuncName[]={"SUMA_RegisterDO"};
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* check to see if dov_id exists */
   i = 0;
   while (i < cSV->N_DO) {
      if (cSV->ShowDO[i] == dov_id) {
         /* found do nothing, return */
         SUMA_RETURN(YUP);
      }
      ++i;
   }
   cSV->ShowDO[cSV->N_DO] = dov_id;
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
      fprintf (SUMA_STDERR,"%s: ShowDO is now:\n", FuncName);
      for (i=0; i< cSV->N_DO; ++i) {
         fprintf(SUMA_STDERR,"ShowDO[%d] = %d\t", i, cSV->ShowDO[i]);
      }
      fprintf(SUMA_STDERR,"\n");
   }

   SUMA_RETURN(YUP); 
}
/*!
remove DO with I.D. dov_id from ShowDO list of that current viewer
removal of dov_id element is done by replacing it with the last entry in ShowDO
list. If not found, nothing happens.
*/
SUMA_Boolean SUMA_UnRegisterDO(int dov_id, SUMA_SurfaceViewer *cSV)
{
   int i;
   static char FuncName[]={"SUMA_UnRegisterDO"};
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* check to see if dov_id exists */
   i = 0;
   while (i < cSV->N_DO) {
      if (cSV->ShowDO[i] == dov_id) {
         /* found, replace it by the last in the list */
         cSV->ShowDO[i] = cSV->ShowDO[cSV->N_DO-1];
         /*remove the last element of the list */
         cSV->ShowDO[cSV->N_DO-1] = 0;
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
            fprintf (SUMA_STDERR,"%s: ShowDO is now:\n", FuncName);
            for (i=0; i< cSV->N_DO; ++i) {
               fprintf(SUMA_STDERR,"ShowDO[%d] = %d\t", i, cSV->ShowDO[i]);
            }
            fprintf(SUMA_STDERR,"\n");
         }

         
         SUMA_RETURN(YUP);
      }
      ++i;
   }
   /* Not found, nothing happens */
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: ShowDO is now:\n", FuncName);
      for (i=0; i< cSV->N_DO; ++i) {
         fprintf(SUMA_STDERR,"ShowDO[%d] = %d\t", i, cSV->ShowDO[i]);
      }
      fprintf(SUMA_STDERR,"\n");
   }
   
   SUMA_RETURN(YUP); 
}

/*!
print out the data contained in dov 
*/
void SUMA_Show_DOv (SUMA_DO *dov, int N_dov, FILE *Out)
{
   int i;
   SUMA_SurfaceObject *so_op;
   static char FuncName[]={"SUMA_Show_DOv"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (Out == NULL) Out = stdout;
   fprintf(Out,"\nDOv contents (%d elements):\n", N_dov);
   for (i=0; i < N_dov; ++i) {
      switch (dov[i].ObjectType) {
         case SO_type:
            so_op = (SUMA_SurfaceObject *)dov[i].OP;
            if (so_op->FileType != SUMA_SUREFIT) {
               fprintf(Out,"DOv ID: %d\n\tName: %s/%s\n\tType: %d, Axis Attachment %d\n",\
                  i, so_op->Name.Path, so_op->Name.FileName,\
                  dov[i].ObjectType, dov[i].CoordType);
            } else {
               fprintf(Out,"DOv ID: %d\n\tNameCoord: %s/%s\n\tNameTopo: %s/%s\n\tType: %d, Axis Attachment %d\n",\
                  i, so_op->Name_coord.Path, so_op->Name_coord.FileName,\
                  so_op->Name_topo.Path, so_op->Name_topo.FileName,\
                  dov[i].ObjectType, dov[i].CoordType);
            }   
            break;
         case AO_type:
            {
               SUMA_Axis* ao;
               ao = (SUMA_Axis*) dov[i].OP;
               fprintf(Out,"DOv ID: %d\n\tAxis Object\n\tType: %d, Axis Attachment %d\n", i,dov[i].ObjectType, dov[i].CoordType);
               fprintf(Out,"\tName: %s\tidcode: %s\n", ao->Name, ao->idcode_str);
            }
            break;
         case GO_type:
            fprintf(Out,"DOv ID: %d\n\tGrid Object\n", i);
            break;
         default:
            fprintf(Out,"DOv ID: %d\n\tUnknown Type!\n", i);
            break;
      }
   }
   SUMA_RETURNe;
}

/*!
returns a vector of indices into dov for DO that meet DO_Type
You should free the returned pointer once you're done with it
N contains the number of elements found
*/
int * SUMA_GetDO_Type(SUMA_DO *dov, int N_dov, SUMA_DO_Types DO_Type, int *N)
{
   int *do_id, i;
   static char FuncName[]={"SUMA_GetDO_Type"};
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
SUMA_Boolean SUMA_existDO(char *idcode, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_existDO"};
   SUMA_SurfaceObject *SO;
   int i;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (idcode == NULL) {
      fprintf(SUMA_STDERR,"Warning SUMA_existDO: NULL idcode.\n");
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
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   static char FuncName[]={"SUMA_findSO_inDOv"};
   SUMA_SurfaceObject *SO;
   int i;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   determines if a Surface Object is mappable (ie MapRef_idcode_str != NULL)
   ans = SUMA_ismappable (SUMA_SurfaceObject *SO)
   \param SO (SUMA_SurfaceObject *)
   \ret YUP/NOPE
*/
SUMA_Boolean SUMA_ismappable (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_ismappable"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (SO->MapRef_idcode_str != NULL) {
      /* SO is mappable */
      SUMA_RETURN (YUP);
   } 
   SUMA_RETURN (NOPE);

}

/*!
   determines if a Surface Object is inherently mappable (ie MapRef_idcode_str == idcode_str)
   ans = SUMA_isINHmappable (SUMA_SurfaceObject *SO)
   \param SO (SUMA_SurfaceObject *)
   \ret YUP/NOPE
*/
SUMA_Boolean SUMA_isINHmappable (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_isINHmappable"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (SO->MapRef_idcode_str == NULL) {
      SUMA_RETURN (NOPE);
   }
   if (strcmp(SO->MapRef_idcode_str, SO->idcode_str) == 0) {
      /* SO is inherently mappable */
      SUMA_RETURN (YUP);
   } 
   SUMA_RETURN (NOPE);
}

/*!
   \brief ans = SUMA_isRelated (SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2);
   returns YUP if SO1 and SO2 are related, ie: 
   SO1->idcode_str = SO2->idcode_str (in this case SO1 = SO2) or 
   SO1->MapRef_idcode_str = SO2->idcode_str (SO2 is the mapping reference of SO1) or
   SO1->idcode_str = SO2->MapRef_idcode_str (SO1 is the mapping reference of SO2)
   SO1->MapRef_idcode_str = SO2->MapRef_idcode_str (SO1 and SO2 have the same mapping reference) or 
*/
SUMA_Boolean SUMA_isRelated (SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2)
{
   static char FuncName[]={"SUMA_isRelated"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!SO1->idcode_str || !SO2->idcode_str) {
      fprintf (SUMA_STDERR, "Error %s: NULL idcode_str.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (strcmp (SO1->idcode_str, SO2->idcode_str) == 0) {
      /* SO1 = SO2 */
      SUMA_RETURN (YUP);
   }
   
   if (SO1->MapRef_idcode_str) {
      if (strcmp (SO1->MapRef_idcode_str, SO2->idcode_str) == 0) {
         /* SO2 is the mapping reference of SO1 */
         SUMA_RETURN (YUP);
      }
   }
   
   if (SO2->MapRef_idcode_str) {
      if (strcmp (SO1->idcode_str, SO2->MapRef_idcode_str) == 0) {
          /* SO1 is the mapping reference of SO2 */
          SUMA_RETURN (YUP);
      }
   }
   
   if (SO1->MapRef_idcode_str && SO2->MapRef_idcode_str) {
      if (strcmp (SO1->MapRef_idcode_str, SO2->MapRef_idcode_str) == 0) {
         /* SO1 and SO2 have the same mapping reference */
         SUMA_RETURN (YUP);
      }
   }
   
   SUMA_RETURN (NOPE);
   
}

/*!
SUMA_Boolean SUMA_isSO (SUMA_DO DO) 
   returns YUP if DO is of SO_type
   ans = SUMA_isSO (DO) ;
*/
SUMA_Boolean SUMA_isSO (SUMA_DO DO) 
{
   static char FuncName[]={"SUMA_isSO"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   SUMA_Boolean Found = NOPE, LocalHead = NOPE;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
      dlist_destroy(list);
      dlist_destroy(listop);
      SUMA_free(list);
      SUMA_free(listop);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
\brief Returns YUP if the surface: dROI->Parent_idcode_str is related to SO->idcode_str or SO->MapRef_idcode_str.
NOPE otherwise

ans = SUMA_isdROIrelated (dROI, SO);

\param ROI (SUMA_DRAWN_ROI *) pointer to drawn ROI
\param SO (SUMA_SurfaceObject *) pointer to surface object
\return ans (SUMA_Boolean) YUP/NOPE

*/
SUMA_Boolean SUMA_isdROIrelated (SUMA_DRAWN_ROI *ROI, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_isdROIrelated"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_SurfaceObject *SO_ROI = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (LocalHead) {
      fprintf (SUMA_STDERR, "%s: %s SO->MapRef_idcode_str\n", FuncName, SO->MapRef_idcode_str);
      fprintf (SUMA_STDERR, "%s: %s ROI->Parent_idcode_str\n", FuncName, ROI->Parent_idcode_str);
      fprintf (SUMA_STDERR, "%s: %s SO->idcode_str\n", FuncName, SO->idcode_str);
   }
   
   /* find the pointer to the surface having for an idcode_str: ROI->Parent_idcode_str */
   SO_ROI = SUMA_findSOp_inDOv(ROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   
   if (!SO_ROI) {
      SUMA_SL_Err("Could not find surface of ROI->Parent_idcode_str");
      SUMA_RETURN (NOPE);
   }
   
   if (SUMA_isRelated (SO, SO_ROI)) {
      SUMA_RETURN (YUP);
   }

   SUMA_RETURN (NOPE);
}

/*!
\brief Returns YUP if if the surface: dROI->Parent_idcode_str is related to SO->idcode_str or SO->MapRef_idcode_str.
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /* find the pointer to the surface having for an idcode_str: ROI->Parent_idcode_str */
   SO_ROI = SUMA_findSOp_inDOv(ROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   
   if (!SO_ROI) {
      SUMA_SL_Err("Could not find surface of ROI->Parent_idcode_str");
      SUMA_RETURN (NOPE);
   }
   
   if (SUMA_isRelated (SO, SO_ROI)) {
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
   SUMA_Boolean LocalHead = YUP;
    
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
