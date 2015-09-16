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
      case VO_type:
         if (!SUMA_FreeVolumeObject ((SUMA_VolumeObject *)dov->OP)) {
            SUMA_S_Err("could not free volume");
         }
         break;
      case SO_type:
         if (!SUMA_Free_Surface_Object ((SUMA_SurfaceObject *)dov->OP)) {
            fprintf(SUMA_STDERR,
               "Error SUMA_Free_Displayable_Object, could not free surface\n");
         }
         break;
      case ROIdO_type:
         if (!SUMA_freeDrawnROI ((SUMA_DRAWN_ROI *)dov->OP)) {
            fprintf(SUMA_STDERR,
               "Error SUMA_freeDrawnROI, could not free  ROI.\n");
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
      case ODIR_type:
      case DIR_type:
      case NBLS_type:
      case NBOLS_type:
         SUMA_free_SegmentDO ((SUMA_SegmentDO *)dov->OP);
         break;
      case AO_type:
         SUMA_Free_Axis((SUMA_Axis*)dov->OP);
         break;
      case GO_type:
         fprintf(SUMA_STDERR,
                  "Error SUMA_Free_Displayable_Object, "
                  "Not trained to free GO objects\n");
         break;
      case not_DO_type:
         /* not a DO, leave it to beaver */
         break;
      case NOT_SET_type:
         fprintf(SUMA_STDERR,
                  "Error SUMA_Free_Displayable_Object, "
                  "no free NOT_SET_type\n");
         break;
      case PNT_type:
      case NBSP_type:
      case SP_type:
         SUMA_free_SphereDO ((SUMA_SphereDO *)dov->OP);
         break;
      case PL_type:
         SUMA_free_PlaneDO ((SUMA_PlaneDO *)dov->OP);
         break;
      case NIDO_type:
         SUMA_free_NIDO((SUMA_NIDO*)dov->OP);
         break;
      case CDOM_type:
         SUMA_FreeCIFTIObject((SUMA_CIFTI_DO *)dov->OP);
         break;
      case NBT_type:
      case SBT_type:
      case DBT_type:
         /* those types are not used */
         SUMA_S_Warnv("Type %d should not be in  use!\n", dov->ObjectType);
         break;
      case GDSET_type:
      case MD_DSET_type:
      case ANY_DSET_type:
         SUMA_FreeDset(dov->OP);
         break;
      case TRACT_type:
         SUMA_free_TractDO(dov->OP);
         break;
      case MASK_type:
         SUMA_free_MaskDO(dov->OP);
         break;
      case GRAPH_LINK_type:
         SUMA_free_GraphLinkDO(dov->OP);
         break;
      default:
         SUMA_S_Errv("Type %d not accounted for!\n", dov->ObjectType);
         break;   
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
   
   SUMA_LHv("idcode %s\n", idcode_str);
   if (!dov || !idcode_str) {
      SUMA_RETURN(-1);
   }
    
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType > not_DO_type) {
         ado = (SUMA_ALL_DO *)dov[i].OP;
         SUMA_LHv("ado %p: Object %d/%d type: %d\n", 
                  ado, i, N_dov, dov[i].ObjectType); 
         SUMA_LHv("ado->idcode_str= %s\n", 
                  SUMA_CHECK_NULL_STR(SUMA_ADO_idcode(ado)));
         SUMA_LHv("idcode_str= %s\n", 
                  SUMA_CHECK_NULL_STR(idcode_str)); 
         if (SUMA_ADO_idcode(ado) && 
             strcmp(SUMA_ADO_idcode(ado), idcode_str) == 0) {
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
SUMA_Boolean SUMA_AddDO(SUMA_DO *dov, int *N_dov, void *op, 
                        SUMA_DO_Types DO_Type, SUMA_DO_CoordType DO_CoordType)
{
   static char FuncName[] = {"SUMA_AddDO"};
   SUMA_ALL_DO *ado=NULL;
   static int nm=0;
   void *eo=NULL;
   int ieo;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   ado = (SUMA_ALL_DO *)op;
   if (!SUMA_ADO_idcode(ado)) {
      SUMA_error_message (FuncName, "Need an idcode_str for do",0);
      SUMA_RETURN(NOPE);
   }
   if (DO_Type <= not_DO_type || DO_Type >= N_DO_TYPES) {
      SUMA_S_Errv("DO_type %d not valid\n", DO_Type);
      SUMA_RETURN(NOPE);
   }
   /* Does that baby exist? */
   if ((ieo = SUMA_FindDOi_byID(dov, *N_dov, SUMA_ADO_idcode(ado))) >= 0) {
      if (DO_Type == SO_type) {
         SUMA_SLP_Err("Surface exists, cannot be replaced this way.");
         SUMA_RETURN(NOPE);
      }
      if (DO_Type == VO_type) {
         SUMA_S_Warn("Replacing volume object, might get complicated...");
      }
      if (LocalHead || 
          (!(nm % 300) && !SUMA_ADO_isLabel(ado,"AHorseWithNoName"))) {
         SUMA_SL_Note( "Object %s existed as %s and will be replaced.\n"
                        "Message shown intermittently", 
                        ADO_LABEL(ado), ADO_LABEL((SUMA_ALL_DO *)(dov[ieo].OP)));
         ++nm;
      }
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
SUMA_Boolean SUMA_RemoveDO(SUMA_DO *dov, int *N_dov, void *op, 
                           SUMA_Boolean Free_op)
{
   static char FuncName[] = {"SUMA_RemoveDO"};
   int i;
   SUMA_Boolean Found=NOPE, State=YUP;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   if (LocalHead) {
      SUMA_LH("Before deletion, %d objects", *N_dov);
      for (i=0; i<*N_dov; ++i) {
         fprintf(SUMA_STDERR,"%d: Label %s Type %s\n",
                  i, ADO_LABEL((SUMA_ALL_DO*)dov[i].OP),
                  ADO_TNAME((SUMA_ALL_DO*)dov[i].OP));
      }
   }
   for (i=0; i<*N_dov; ++i) {
      if (dov[i].OP == op) {
         Found = YUP;
         SUMA_LH("found object. Removing it from dov.");
         if (Free_op) {
            if (LocalHead) SUMA_S_Err("Freeing object.");
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

   if (LocalHead) {
      SUMA_LH("At exit, %d objects", *N_dov);
      for (i=0; i<*N_dov; ++i) {
         fprintf(SUMA_STDERR,"%d: Label %s Type %s\n",
                  i, ADO_LABEL((SUMA_ALL_DO*)dov[i].OP),
                  ADO_TNAME((SUMA_ALL_DO*)dov[i].OP));
      }
   }
   
   if (Found) {
      State=YUP;
      /* Refresh all things indexing dov */
      if (!SUMA_AllSV_RegisteredDO_Refresh()) {
         SUMA_S_Err("Failed to refresh all registDO vectors");
         State=NOPE;
      }
      if (!SUMA_AllViewState_MembsRefresh()) {
         SUMA_S_Err("Failed to refresh all viewstate hist vectors");
         State=NOPE;
      }
      SUMA_RETURN(State);
   } else {
      SUMA_RETURN(NOPE);
   }
}

/*
   A function that must be called each time order of objects in 
   SUMAg_DOv is disturbed 
*/
SUMA_Boolean SUMA_SV_RegisteredDO_Refresh(SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_SV_RegisteredDO_Refresh"};
   int ii=0, found = -1;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   if (!sv) SUMA_RETURN(NOPE);
   
   ii = 0;
   while (ii < sv->N_DO) {
      if ( sv->RegistDO && 
          (found = SUMA_whichDOg(sv->RegistDO[ii].idcode_str)) >= 0) {
         /* A good thing, refresh index mapping */
         sv->RegistDO[ii].dov_ind = found;
      } else {
         SUMA_LH("A bad entry in RegistDO at index %d/%d, cleaning", 
                     ii, sv->N_DO);
         if (ii != sv->N_DO-1) {
            strcpy(sv->RegistDO[ii].idcode_str, 
                sv->RegistDO[sv->N_DO-1].idcode_str);
         }
         sv->RegistDO[ii].dov_ind = sv->RegistDO[sv->N_DO-1].dov_ind;
         sv->N_DO = sv->N_DO-1;
      }
      ++ii;
   }
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_AllSV_RegisteredDO_Refresh(void) 
{
   static char FuncName[]={"SUMA_AllSV_RegisteredDO_Refresh"};
   int i;
   SUMA_Boolean state = YUP;
   
   SUMA_ENTRY;
   
   state = YUP;
   for (i=0; i<SUMAg_N_SVv; ++i) {
      if ((SUMAg_SVv+i) && 
          !SUMA_SV_RegisteredDO_Refresh(SUMAg_SVv+i)) state = NOPE;
   }
   
   SUMA_RETURN(state);
}

int SUMA_FindFirst_dov_ind (SUMA_DO_LOCATOR *x0, SUMA_DO_LOCATOR *x1, int val)
{/*SUMA_FindFirst_dov_ind*/
   SUMA_DO_LOCATOR *xi=x0;
   while(x0<x1) if ((*x0).dov_ind==val) return((int)(x0-xi)); else ++x0;
   return(-1);
}

/*!
Register a DO with surface viewer RegisteredDO vector 
if dov_id is present in cSV, nothing is done
if not found then dov_id is registered at the end of cSV->RegisteredDO 

When a DO is registered, a ColorList is created if the DO is a surface object.
and N_DO is updated
*/
SUMA_Boolean SUMA_RegisterDO(int dov_id, SUMA_SurfaceViewer *cSVu)
{
   static char FuncName[]={"SUMA_RegisterDO"};
   int i, is, icsvmin=0, icsvmax=0, icsv=0;
   char *sid=NULL;
   SUMA_SurfaceViewer *cSV=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      SUMA_DUMP_TRACE("at RegisterDO");
   }
   
   if (dov_id < 0) {
      SUMA_S_Err("A negative dov_id!");
      SUMA_DUMP_TRACE("Negative dov_id bro? What gives?");
      SUMA_RETURN(NOPE);
   }
   
   if (!cSVu) { /* Do this for all viewers */
      SUMA_LHv("Working all %d svs (%d open X-realized)\n", 
               SUMA_MAX_SURF_VIEWERS, SUMAg_N_SVv);
      icsvmin = 0; icsvmax=SUMAg_N_SVv;
   } else {
      icsvmin = SUMA_WhichSV(cSVu, SUMAg_SVv, SUMA_MAX_SURF_VIEWERS);
      if (icsvmin >=0) icsvmax = icsvmin+1;
      else {
         SUMA_DUMP_TRACE("No SV???");
         SUMA_S_Err("Could not find sv!");
         SUMA_RETURN(NOPE);
      }
      SUMA_LHv("Working from sv %d to %d\n", icsvmin, icsvmax);
   }
   
   icsv = icsvmin;
   while (icsv < icsvmax) {
      cSV = &(SUMAg_SVv[icsv]);
      SUMA_LH("Process for viewer %d, %p, [%c]", icsv, cSV, 65+icsv);
      #if 0
      if (LocalHead) {
         /* scan for trouble */
         for (i=0; i<cSV->N_DO; ++i) {
            if (cSV->RegistDO[i].dov_ind < 0) {
               SUMA_DUMP_TRACE("What the what?");
               exit(1);
            }
         }
      }
      #endif

      if (LocalHead && 
          SUMA_WhichSV(cSV, SUMAg_SVv, SUMA_MAX_SURF_VIEWERS) != 0) {
         fprintf(SUMA_STDERR,"%s: Muted for viewer %p [%c]\n", 
              FuncName, cSV, 
              65+SUMA_WhichSV(cSV, SUMAg_SVv, SUMA_MAX_SURF_VIEWERS) );
         /* turn off the LocalHead, too much output*/
         LocalHead = NOPE;
      }  

      
      switch (iDO_type(dov_id)) {
         case SO_type: /* add it regardless. This may need revisiting if you 
                          begin to load surfaces interactively, not from
                          the initial startup */
            /* check to see if dov_id exists */
            if (SUMA_FindFirst_dov_ind(cSV->RegistDO,
                                         cSV->RegistDO+cSV->N_DO,
                                         dov_id) >= 0) { /* found, do nothing */
               goto NEXT_CSV;
            }
            /* Not yet registered so add it */
            cSV->RegistDO[cSV->N_DO].dov_ind = dov_id;
            sid = iDO_idcode(dov_id);
            strcpy(cSV->RegistDO[cSV->N_DO].idcode_str,sid);
            cSV->N_DO += 1;
            
            /* Now add the ColorList, if DO is a surface object */
            if (SUMA_isSO(SUMAg_DOv[dov_id])) {
               if (LocalHead) 
                  fprintf (SUMA_STDERR,"%s: Adding color list...\n", FuncName);
               /* add the ColorList */
               if (!SUMA_FillColorList (cSV,
                              (SUMA_ALL_DO *)SUMAg_DOv[dov_id].OP)) {
                  fprintf(SUMA_STDERR,
                          "Error %s: Failed in SUMA_FillColorList.\n", FuncName);
                  SUMA_RETURN (NOPE);
               }
            }
            SUMA_LHv("Back from SUMA_FillColorList. (%s/%d).\n", 
                   cSV->ColList[0]->idcode_str, cSV->N_ColList);
            break;
         case GRAPH_LINK_type:
            {
            SUMA_GraphLinkDO *GLDO=(SUMA_GraphLinkDO *)(SUMAg_DOv[dov_id].OP);
            SUMA_LHv("With GLDO %s, variant %s, Anat Correctedness %s\n", 
                     DO_label(GLDO), GLDO->variant,
                     SUMA_isDO_AnatCorrect(&(SUMAg_DOv[dov_id]))?"YES":"NO");
            if (SUMA_isDO_AnatCorrect(&(SUMAg_DOv[dov_id]))) {
               if (SUMA_FirstGoodAnatCorrState(cSV) < 0) { 
                                            /* have nothing yet, add one */
                  SUMA_LH("Not one anatomical state to be found for sv %p [%c],"
                          " adding one", cSV, 
                          SUMA_WhichSVc(cSV, SUMAg_SVv, SUMA_MAX_SURF_VIEWERS));
                  if (SUMA_Which_iDO_State(dov_id, cSV, 1) < 0) {
                     SUMA_S_Err("State could not be added!!!");
                     SUMA_RETURN (NOPE);
                  }   
               }
               
               /* Register it also in all states of VSv that are AnatCorrect */
               for (is=0; is < cSV->N_VSv; ++is) {
                  if (cSV->VSv[is].AnatCorrect && 
                     SUMA_FindFirst_dov_ind(cSV->VSv[is].MembDO,
                                  cSV->VSv[is].MembDO+cSV->VSv[is].N_MembDO,
                                  dov_id) < 0) {
                     cSV->VSv[is].N_MembDO += 1;
                     cSV->VSv[is].MembDO = 
                        (SUMA_DO_LOCATOR *)SUMA_realloc(cSV->VSv[is].MembDO,
                              cSV->VSv[is].N_MembDO*sizeof(SUMA_DO_LOCATOR));
                     cSV->VSv[is].MembDO[cSV->VSv[is].N_MembDO-1].dov_ind = 
                                                                        dov_id;
                     sid = iDO_idcode(dov_id);
                     strcpy(
                        cSV->VSv[is].MembDO[cSV->VSv[is].N_MembDO-1].idcode_str,
                        sid);
                  }
               }
               /* if the current state is anatomical, register object also 
                  in cSV->RegistDO */
               if (SUMA_isViewerStateAnatomical(cSV)) {
                  if (SUMA_FindFirst_dov_ind(cSV->RegistDO,
                                         cSV->RegistDO+cSV->N_DO,
                                         dov_id) < 0) {
                     cSV->RegistDO[cSV->N_DO].dov_ind = dov_id;
                     sid = iDO_idcode(dov_id);
                     strcpy(cSV->RegistDO[cSV->N_DO].idcode_str,sid);
                     cSV->N_DO += 1;
                  } else {
                     SUMA_LHv("   GLDO %s, %s already in the bag at"
                              " cSV->RegisteredDO[%d]\n", 
                                 DO_label(GLDO), GLDO->variant,
                                 SUMA_FindFirst_dov_ind(cSV->RegistDO,
                                         cSV->RegistDO+cSV->N_DO,
                                         dov_id));
                  }
                  SUMA_LH("Adding color list (if necessary)...");
                  /* add the ColorList */
                  if (!SUMA_FillColorList (cSV,
                                 (SUMA_ALL_DO *)SUMAg_DOv[dov_id].OP)) {
                     SUMA_S_Err("Failed in SUMA_FillColorList.");
                     SUMA_RETURN (NOPE);
                  }
 
               } else {
                  SUMA_LHv(" Viewer state (%s) not anatomical for GLDO %s, %s\n",
                           cSV->State, DO_label(GLDO), GLDO->variant);
               } 
            } else {
               is = SUMA_Which_iDO_State(dov_id, cSV, 1);
               /* If we are in the proper state add it to RegisteredDO*/
               SUMA_LHv("For GLDO  %s\n      State:%s,Group:%s found.\n"
                        "Comparing to current viewer state of %s\n", 
                        GLDO->Label, SUMA_iDO_state(dov_id),
                        SUMA_iDO_group(dov_id), cSV->State);
               if (!strcmp(cSV->State, SUMA_iDO_state(dov_id))) {
                  if (SUMA_FindFirst_dov_ind(cSV->RegistDO,
                                      cSV->RegistDO+cSV->N_DO,
                                      dov_id) < 0) {/* not present, add it */
                     SUMA_LHv("   GLDO %s, %s has been appended\n",
                           DO_label(GLDO), GLDO->variant);
                  cSV->RegistDO[cSV->N_DO].dov_ind = dov_id;
                  sid = iDO_idcode(dov_id);
                  strcpy(cSV->RegistDO[cSV->N_DO].idcode_str,sid);
                  cSV->N_DO += 1; 
                  } else {
                     SUMA_LHv("   GLDO %s, %s already in the bag at"
                           " cSV->RegisteredDO[%d]\n", 
                              DO_label(GLDO), GLDO->variant,
                              SUMA_FindFirst_dov_ind(cSV->RegistDO,
                                      cSV->RegistDO+cSV->N_DO,
                                      dov_id));
                  }
                  SUMA_LH("Adding color list.");
                  /* add the ColorList */
                  if (!SUMA_FillColorList (cSV,
                                 (SUMA_ALL_DO *)SUMAg_DOv[dov_id].OP)) {
                     SUMA_S_Err("Failed in SUMA_FillColorList.");
                     SUMA_RETURN (NOPE);
                  }
               }
            }
            }
            break;
         case MASK_type:
         case VO_type:
	 case CDOM_type:
         case TRACT_type:
            {
	    SUMA_ALL_DO *ADO=(SUMA_ALL_DO *)(SUMAg_DOv[dov_id].OP);
            SUMA_LHv("With ADO %s, Anat Correctedness %s\n", 
                     ADO_LABEL(ADO),
                     SUMA_isDO_AnatCorrect(&(SUMAg_DOv[dov_id]))?"YES":"NO");
            if (SUMA_isDO_AnatCorrect(&(SUMAg_DOv[dov_id]))) {
               if (SUMA_FirstGoodAnatCorrState(cSV) < 0) { 
                     SUMA_LH("Not one anatomical state to be found, adding one");
                  if (SUMA_Which_iDO_State(dov_id, cSV, 1) < 0) {
                     SUMA_S_Err("State could not be added!!!");
                     SUMA_RETURN (NOPE);
                  }
               } 
               /* Register it also in all states of VSv that are AnatCorrect */
               for (is=0; is < cSV->N_VSv; ++is) {
                  if (cSV->VSv[is].AnatCorrect && 
                     SUMA_FindFirst_dov_ind(cSV->VSv[is].MembDO,
                                  cSV->VSv[is].MembDO+cSV->VSv[is].N_MembDO,
                                  dov_id) < 0) {
                     cSV->VSv[is].N_MembDO += 1;
                     cSV->VSv[is].MembDO = 
                        (SUMA_DO_LOCATOR *)SUMA_realloc(cSV->VSv[is].MembDO,
                                 cSV->VSv[is].N_MembDO*sizeof(SUMA_DO_LOCATOR));   
                     cSV->VSv[is].MembDO[cSV->VSv[is].N_MembDO-1].dov_ind = 
                                                                        dov_id;
                     sid = iDO_idcode(dov_id);
                     strcpy(
                        cSV->VSv[is].MembDO[cSV->VSv[is].N_MembDO-1].idcode_str,
                        sid);
                  }
               }
               /* if the current state is anatomical, register object also 
                  in cSV->RegistDO */
               if (SUMA_isViewerStateAnatomical(cSV)) {
                  if (SUMA_FindFirst_dov_ind(cSV->RegistDO,
                                         cSV->RegistDO+cSV->N_DO,
                                         dov_id) < 0) {
                     cSV->RegistDO[cSV->N_DO].dov_ind = dov_id;
                     sid = iDO_idcode(dov_id);
                     strcpy(cSV->RegistDO[cSV->N_DO].idcode_str,sid);
                     cSV->N_DO += 1;
                  } else {
                     SUMA_LHv("   ADO %s, already in the bag at"
                              " cSV->RegisteredDO[%d]\n", 
                                 ADO_LABEL(ADO),
                                 SUMA_FindFirst_dov_ind(cSV->RegistDO,
                                         cSV->RegistDO+cSV->N_DO,
                                         dov_id));
                  }
                  SUMA_LH("Adding color list for %s (id %s), viewer %p %d [%c]", 
                           iDO_label(dov_id), iDO_idcode(dov_id), cSV,
                           SUMA_WhichSV(cSV, SUMAg_SVv, SUMA_MAX_SURF_VIEWERS),
                           SUMA_WhichSVc(cSV, SUMAg_SVv, SUMA_MAX_SURF_VIEWERS));
                  /* add the ColorList */
                  if (!SUMA_FillColorList (cSV,
                                 (SUMA_ALL_DO *)SUMAg_DOv[dov_id].OP)) {
                     SUMA_S_Err("Failed in SUMA_FillColorList.");
                     SUMA_RETURN (NOPE);
                  }
 
               } else {
                  SUMA_LHv(" Viewer state (%s) not anatomical for ADO %s\n",
                           cSV->State, ADO_LABEL(ADO));
               } 
            } else {
               SUMA_S_Err("Tracts are supposed to be anatomically correct\n"
                          "If this is not an error, you'll need to put in\n"
                          "a block here that parallels what is not with \n"
                          "anatomically incorrect GLDOs above.");
               SUMA_RETURN(NOPE);
            }
            }
            break;
         default:
            SUMA_LHv("Just adding DO %s, no states, nothing\n",
                     iDO_label(dov_id));
            if (SUMA_FindFirst_dov_ind(cSV->RegistDO,
                                         cSV->RegistDO+cSV->N_DO,
                                         dov_id) < 0){
               /* just add for now */
               cSV->RegistDO[cSV->N_DO].dov_ind = dov_id;
               sid = iDO_idcode(dov_id);
               strcpy(cSV->RegistDO[cSV->N_DO].idcode_str,sid);
               cSV->N_DO += 1;
            }
            break;
      }
      if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: RegisteredDO is now:\n", FuncName);
         for (i=0; i< cSV->N_DO; ++i) {
            fprintf(SUMA_STDERR,
                        "RegisteredDO[%d] = %d , type=%d (%s) label %s\t", i, 
                        cSV->RegistDO[i].dov_ind, 
                        iDO_type(cSV->RegistDO[i].dov_ind),
        SUMA_ObjectTypeCode2ObjectTypeName(iDO_type(cSV->RegistDO[i].dov_ind)),
                        iDO_label(cSV->RegistDO[i].dov_ind));
         }
         fprintf(SUMA_STDERR,"\n");
      }

      /* update the title bar */
      SUMA_UpdateViewerTitle(cSV);   
   
      NEXT_CSV:
      ++icsv;
   }
   
   
   SUMA_RETURN(YUP); 
}
/*!
remove DO with I.D. dov_id from RegisteredDO list of that current viewer
removal of dov_id element is done by replacing it with the last entry in RegistDO
list. If not found, nothing happens.
*/
SUMA_Boolean SUMA_UnRegisterDO_idcode(char *idcode_str, SUMA_SurfaceViewer *cSV)
{
   static char FuncName[]={"SUMA_UnRegisterDO_idcode"};
   int id = SUMA_FindDOi_byID(SUMAg_DOv, SUMAg_N_DOv, idcode_str), isv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Unregistering %s, iid %d (%s), sv %p", 
            idcode_str, id, iDO_label(id), cSV);
   if (id >= 0) {
      if (cSV) {
         SUMA_RETURN(SUMA_UnRegisterDO(id, cSV));
      } else {
         for (isv=0; isv<SUMAg_N_SVv; ++isv) {
            SUMA_UnRegisterDO(id,SUMAg_SVv+isv);
         }
         SUMA_RETURN(YUP);
      }
   }   
   SUMA_RETURN(YUP);
} 

SUMA_Boolean SUMA_UnRegisterDO(int dov_id, SUMA_SurfaceViewer *cSV)
{
   int i;
   static char FuncName[]={"SUMA_UnRegisterDO"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!cSV) SUMA_RETURN(NOPE);
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: RegistDO begins (target %d -- %s):\n", 
               FuncName, dov_id, ADO_LABEL((SUMA_ALL_DO*)SUMAg_DOv[dov_id].OP));
      for (i=0; i< cSV->N_DO; ++i) {
         fprintf(SUMA_STDERR,"RegistDO[%d] = %d in DOv (%s)\n", 
            i, cSV->RegistDO[i].dov_ind, 
            ADO_LABEL((SUMA_ALL_DO*)SUMAg_DOv[cSV->RegistDO[i].dov_ind].OP));
      }
      fprintf(SUMA_STDERR,"\n");
   }
   
   /* check to see if dov_id exists */
   i = 0;
   while (i < cSV->N_DO) {
      if (cSV->RegistDO[i].dov_ind == dov_id) {
         SUMA_LH("Removing %d", dov_id);
         /* found, replace it by the last in the list */
         cSV->RegistDO[i].dov_ind = cSV->RegistDO[cSV->N_DO-1].dov_ind;
         if (i != cSV->N_DO-1) {
            strcpy(cSV->RegistDO[i].idcode_str, 
                   cSV->RegistDO[cSV->N_DO-1].idcode_str);
         }
         /*remove the last element of the list */
         cSV->RegistDO[cSV->N_DO-1].dov_ind = -1;
         cSV->RegistDO[cSV->N_DO-1].idcode_str[0]='\0';
         cSV->N_DO -= 1; 
         
         /* empty the ColorList for this surface */
         ado = iDO_ADO(dov_id);
         switch (ado->do_type) {
            case SO_type:
               SUMA_LH("Emptying ColorList ...");
               if (!SUMA_EmptyColorList (cSV, ADO_ID(ado))) {
                  SUMA_S_Err("Failed in SUMA_EmptyColorList\n");
                  SUMA_RETURN(NOPE);
               }
               break;
            case GRAPH_LINK_type: {
               SUMA_DSET *dset=SUMA_find_GLDO_Dset((SUMA_GraphLinkDO *)ado);
               if (dset) SUMA_RETURN(SUMA_EmptyColorList(cSV,SDSET_ID(dset)));
               } break;
            default:
               if (!SUMA_EmptyColorList (cSV, ADO_ID(ado))) {
                  SUMA_S_Err("Failed to empty color list for %s, type %s\n", 
                              ADO_LABEL(ado), ADO_TNAME(ado));
                  SUMA_RETURN(NOPE);
               }
               break;
         }
         
         if (LocalHead) {
            fprintf (SUMA_STDERR,"%s: RegistDO is now:\n", FuncName);
            for (i=0; i< cSV->N_DO; ++i) {
               fprintf(SUMA_STDERR,"RegistDO[%d] = %d in DOv (%s)\n", 
                        i, cSV->RegistDO[i].dov_ind,
               ADO_LABEL((SUMA_ALL_DO*)SUMAg_DOv[cSV->RegistDO[i].dov_ind].OP));
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
      fprintf (SUMA_STDERR,"%s: Nothing found, registeredDO still:\n", FuncName);
      for (i=0; i< cSV->N_DO; ++i) {
         fprintf(SUMA_STDERR,"RegistDO[%d] = %d\t", i, cSV->RegistDO[i].dov_ind);
      }
      fprintf(SUMA_STDERR,"\n");
      SUMA_DUMP_TRACE("Why nothing found?");
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
   SUMA_VolumeObject *vo_op=NULL; 
   SUMA_CIFTI_DO *co=NULL;  
   SUMA_STRING *SS=NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (dov) {
      SS = SUMA_StringAppend_va(SS, "\nDOv contents (%d elements):\n", N_dov);
      for (i=0; i < N_dov; ++i) {
         switch (dov[i].ObjectType) {
            case VO_type:
               vo_op = (SUMA_VolumeObject *)dov[i].OP;
               SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tName: %s\n"
                     "\tType: %d (%s), Axis Attachment %d\n",
                     i,  SUMA_CHECK_NULL_STR(vo_op->Label), 
                     dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
               break;
            case CDOM_type:
               co = (SUMA_CIFTI_DO *)dov[i].OP;
               SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tName: %s\n"
                     "\tType: %d (%s), Axis Attachment %d\n",
                     i,  SUMA_CHECK_NULL_STR(co->Label), 
                     dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
               break;
            case SO_type:
               so_op = (SUMA_SurfaceObject *)dov[i].OP;
               #if 0
               if (so_op->FileType != SUMA_SUREFIT) {
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tName: %s/%s\n"
                     "\tType: %d (%s), Axis Attachment %d\n",
                     i,  SUMA_CHECK_NULL_STR(so_op->Name.Path), 
                     SUMA_CHECK_NULL_STR(so_op->Name.FileName),
                     dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
               } else {
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tNameCoord: %s/%s\n"
                     "\tNameTopo: %s/%s\n\tType: %d (%s), Axis Attachment %d\n",
                     i, SUMA_CHECK_NULL_STR(so_op->Name_coord.Path), 
                     SUMA_CHECK_NULL_STR(so_op->Name_coord.FileName),
                     so_op->Name_topo.Path, so_op->Name_topo.FileName,
                     dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
               } 
               #else
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tName: %s\n"
                     "\tType: %d (%s), Axis Attachment %d\n",
                     i,  SUMA_CHECK_NULL_STR(so_op->Label), 
                     dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
               #endif  
               break;
            case AO_type:
               {
                  SUMA_Axis* ao;
                  SUMA_LH("HERE\n");
                  ao = (SUMA_Axis*) dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tAxis Object\n"
                     "\tType: %d (%s), Axis Attachment %d\n", 
                     i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType),
                     dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,
                     "\tName: %s\n\tidcode: %s\n", ao->Label, ao->idcode_str);
               }
               break;
            case OLS_type:
               {
                  SUMA_SegmentDO *sdo=NULL;

                  sdo = (SUMA_SegmentDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tOriented Line Segment Object\n"
                     "\tType: %d (%s), Axis Attachment %d\n", 
                     i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,
                     "\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case DIR_type:
               {
                  SUMA_SegmentDO *sdo=NULL;

                  sdo = (SUMA_SegmentDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tDirection Object\n"
                     "\tType: %d (%s), Axis Attachment %d\n", 
                     i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,
                     "\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);
               }
               break;
            case ODIR_type:
               {
                  SUMA_SegmentDO *sdo=NULL;

                  sdo = (SUMA_SegmentDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tOriented Direction Object\n"
                     "\tType: %d (%s), Axis Attachment %d\n", 
                     i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,
                     "\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case ONBV_type:
               {
                  SUMA_SegmentDO *sdo=NULL;

                  sdo = (SUMA_SegmentDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tNode-Based Ball-Vector\n"
                     "\tType: %d (%s), Axis Attachment %d\n", 
                        i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,
                     "\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case NBV_type:
               {
                  SUMA_SegmentDO *sdo=NULL;

                  sdo = (SUMA_SegmentDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tNode-Based Vector\n"
                     "\tType: %d (%s), Axis Attachment %d\n", 
                     i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,
                     "\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case LS_type:
               {
                  SUMA_SegmentDO *sdo=NULL;

                  sdo = (SUMA_SegmentDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tLine Segment Object\n"
                     "\tType: %d (%s), Axis Attachment %d\n", 
                     i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,
                     "\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case NBSP_type:
               {
                  SUMA_SphereDO *sdo=NULL;

                  sdo = (SUMA_SphereDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tNode-Based Sphere Object\n"
                     "\tType: %d (%s), Axis Attachment %d\n", 
                     i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,
                     "\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case SP_type:
               {
                  SUMA_SphereDO *sdo=NULL;

                  sdo = (SUMA_SphereDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tSphere Object\n"
                     "\tType: %d (%s), Axis Attachment %d\n", 
                     i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,
                     "\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case PNT_type:
               {
                  SUMA_SphereDO *sdo=NULL;

                  sdo = (SUMA_SphereDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tPoint Object\n"
                     "\tType: %d (%s), Axis Attachment %d\n", 
                     i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,
                     "\tLabel: %s\n\tidcode: %s\n", sdo->Label, sdo->idcode_str);

               }
               break;
            case PL_type:
               {
                  SUMA_PlaneDO *pdo=NULL;

                  pdo = (SUMA_PlaneDO *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tPlane Object\n"
                     "\tType: %d (%s), Axis Attachment %d\n", 
                     i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,
                     "\tLabel: %s\n\tidcode: %s\n", pdo->Label, pdo->idcode_str);

               }
               break;
            case ROIdO_type:
               {
                  SUMA_DRAWN_ROI *dROI = NULL;

                  dROI = (SUMA_DRAWN_ROI *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tLine Segment Object\n"
                     "\tType: %d (%s), Axis Attachment %d\n", 
                     i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,
                     "\tLabel: %s\n\tidcode: %s\n", 
                     dROI->Label, dROI->idcode_str);
               }  
               break;
            case ROIO_type:
               {
                  SUMA_ROI *ROI = NULL;

                  ROI = (SUMA_ROI *)dov[i].OP;
                  SS = SUMA_StringAppend_va(SS,
                     "DOv ID: %d\n\tLine Segment Object\n"
                     "\tType: %d (%s), Axis Attachment %d\n", 
                     i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
                  SS = SUMA_StringAppend_va(SS,"\tLabel: %s\n\tidcode: %s\n", 
                           ROI->Label, ROI->idcode_str);
               }  
               break;
            case GO_type:
               SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n\tGrid Object\n", i);
               break;
            case NIDO_type:
               SS = SUMA_StringAppend_va(SS,
                        "DOv ID: %d\n\tNIDO  Object\n"
                        "\tType: %d (%s), Axis Attachment %d\n", 
                        i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
               break;
            case GRAPH_LINK_type: {
               SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)dov[i].OP;
               SUMA_DSET *dset = SUMA_find_GLDO_Dset(gldo);
               SS = SUMA_StringAppend_va(SS,
                        "DOv ID: %d\n\tGLDO Label: %s, id: %s%s\n"
                        "\tType: %d (%s), Axis Attachment %d, Variant: %s,\n"
                        "\tParent:  %s (id %s)\n", 
                        i, gldo->Label, gldo->idcode_str,
                     strcmp(gldo->variant,"TheShadow")?
                           "":"(id always same as Parent's)",
                     dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType, 
                     gldo->variant, SDSET_LABEL(dset), gldo->Parent_idcode_str);
               break; }
            case TRACT_type: {
               SS = SUMA_StringAppend_va(SS,
                        "DOv ID: %d\n\tTract  Object\n"
                        "\tType: %d (%s), Axis Attachment %d\n", 
                        i,dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
               break; }
            case MASK_type: {
               SS = SUMA_StringAppend_va(SS,
                        "DOv ID: %d\tLabel: %s\n\tMask  Object\n"
                        "\tType: %d (%s), Axis Attachment %d\n", 
                        i, ADO_LABEL((SUMA_ALL_DO *)dov[i].OP), 
                        dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType), 
                     dov[i].CoordType);
               break; }
            default:
               SS = SUMA_StringAppend_va(SS,"DOv ID: %d\n"
                                            "\tUnknown Type (%d) %s!\n",
                                             i, dov[i].ObjectType, 
                     SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType)
                     );
               break;
         }
      }
      
   } else {
      SS = SUMA_StringAppend(SS, "NULL DO.");
   }
   
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(s);
}

char *SUMA_TractDOInfo (SUMA_TractDO *tdo, int detail)
{
   static char FuncName[]={"SUMA_TractDOInfo"};
   int i;
   SUMA_COL_TYPE ctp;
   char *s=NULL, stmp[200];
   SUMA_STRING *SS=NULL;
   NI_group *ngr=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (tdo) {
      SS = SUMA_StringAppend_va(SS, "Tract %p\n", tdo);
      s = SUMA_Taylor_Network_Info(tdo->net, 2,5);
      SS = SUMA_StringAppend(SS,s);
      SUMA_ifree(s);
   } else {
      SS = SUMA_StringAppend(SS, "NULL Tract.");
   }
   
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(s);
}

char *SUMA_MaskDOInfo (SUMA_MaskDO *mdo, int detail)
{
   static char FuncName[]={"SUMA_MaskDOInfo"};
   int i;
   SUMA_COL_TYPE ctp;
   char *s=NULL, stmp[200];
   SUMA_STRING *SS=NULL;
   NI_group *ngr=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (mdo) {
      SS = SUMA_StringAppend_va(SS, "Mask %p\n", mdo);
      SS = SUMA_StringAppend(SS,"No info for masks yet.");
      SUMA_ifree(s);
   } else {
      SS = SUMA_StringAppend(SS, "NULL Mask.");
   }
   
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(s);
}

char *SUMA_VolumeObjectInfo (SUMA_VolumeObject *vo, int detail)
{
   static char FuncName[]={"SUMA_VolumeObjectInfo"};
   int i;
   SUMA_COL_TYPE ctp;
   char *s=NULL, stmp[200];
   SUMA_STRING *SS=NULL;
   NI_group *ngr=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (vo) {
      SS = SUMA_StringAppend_va(SS, "VolumeObject %p\n", vo);
      SS = SUMA_StringAppend(SS,"No info for volumes yet.");
      SUMA_ifree(s);
   } else {
      SS = SUMA_StringAppend(SS, "NULL VO.");
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
   searches all VO_type DO objects for idcode
   YUP if found, NOPE if not
*/
SUMA_Boolean SUMA_existVO(char *idcode, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_existVO"};
   SUMA_VolumeObject *VO;
   int i;
   
   SUMA_ENTRY;

   if (idcode == NULL) {
      fprintf(SUMA_STDERR,"Warning SUMA_existVO: NULL idcode.\n");
      SUMA_RETURN (NOPE);
   }
   for (i=0; i< N_dov; ++i) {
      if (dov[i].ObjectType == VO_type) {
         VO = (SUMA_VolumeObject *)dov[i].OP;
         if (strcmp(idcode, VO->idcode_str)== 0) {
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
   SUMA_VolumeObject *VO = NULL;
   SUMA_DRAWN_ROI *dROI = NULL;
   SUMA_ROI *ROI = NULL;
   SUMA_SegmentDO *sdo = NULL;
   SUMA_Axis *sax = NULL;
   SUMA_SphereDO *spdo=NULL;
   SUMA_NIDO *nido=NULL;
   SUMA_ENTRY;

   if (idcode == NULL) {
      fprintf(SUMA_STDERR,"Warning %s: NULL idcode.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   for (i=0; i< N_dov; ++i) {
      switch (dov[i].ObjectType) {
         case (VO_type):
            VO = (SUMA_VolumeObject *)dov[i].OP;
            if (strcmp(idcode, VO->idcode_str)== 0) {
               SUMA_RETURN (YUP);
            }
            break;
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
         case (NIDO_type):
            nido = (SUMA_NIDO *)dov[i].OP;
            if (strcmp(idcode, nido->idcode_str)== 0) {
               SUMA_RETURN (i);
            }
            break;
         default:
            SUMA_S_Warnv("Object type %d not checked.\n", dov[i].ObjectType);
            break;
      }
   }
   SUMA_RETURN(NOPE);
}

char *SUMA_DO_dbg_info(char *idcode)
{
   static char FuncName[]={"SUMA_DO_dbg_info"};
   static int icall=0;
   static char Ret[10][500];
   char *s=NULL;
   int doid;
   SUMA_ALL_DO *ado=NULL;
   
   SUMA_ENTRY;
   ++icall; if (icall > 9) icall = 0;
   s = (char *)Ret[icall];
   s[0] = '\0';
   
   if (!idcode) {
      snprintf(s,499,"NULL idcode passed");
   } else if ( (doid = SUMA_whichDOg(idcode)) < 0) {
      snprintf(s,499,"id %s not found in global list.", idcode);
   } else {
      ado = iDO_ADO(doid);
      snprintf(s,499,"id %s: %s %s",
               idcode, ADO_LABEL(ado), ADO_TNAME(ado));
   }
   SUMA_RETURN(s);
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
   SUMA_VolumeObject *VO = NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_DRAWN_ROI *dROI = NULL;
   SUMA_ROI *ROI = NULL;
   SUMA_SegmentDO *sdo = NULL;
   SUMA_Axis *sax = NULL;
   SUMA_SphereDO *spdo=NULL;
   SUMA_NIDO *nido=NULL;
   SUMA_GraphLinkDO *gldo =NULL;
   SUMA_TractDO *tdo = NULL;
   SUMA_MaskDO *mdo=NULL;
   SUMA_CIFTI_DO *CO=NULL;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;

   if (SUMA_IS_EMPTY_STR_ATTR(idcode)) { /* might come in as ~ at times */
      fprintf(SUMA_STDERR,"Warning %s: NULL idcode.\n", FuncName);
      if (LocalHead) SUMA_DUMP_TRACE("NUULL id");
      SUMA_RETURN (-1);
   }
   for (i=0; i< N_dov; ++i) {
      switch (dov[i].ObjectType) {
         case (VO_type):
            VO = (SUMA_VolumeObject *)dov[i].OP;
            if (strcmp(idcode, VO->idcode_str)== 0) {
               SUMA_RETURN (i);
            }
            break;
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
         case (NIDO_type):
            nido = (SUMA_NIDO *)dov[i].OP;
            if (strcmp(idcode, nido->idcode_str)== 0) {
               SUMA_RETURN (i);
            }
            break;
         case (GRAPH_LINK_type):
            gldo = (SUMA_GraphLinkDO *)dov[i].OP;
            if (strcmp(idcode, gldo->idcode_str)== 0) {
               SUMA_RETURN (i);
            }
            break;
         case TRACT_type:
            tdo = (SUMA_TractDO *)dov[i].OP;
            if (strcmp(idcode, tdo->idcode_str)== 0) {
               SUMA_RETURN (i);
            }
            break;
         case MASK_type:
            mdo = (SUMA_MaskDO *)dov[i].OP;
            if (strcmp(idcode, mdo->idcode_str)== 0) {
               SUMA_RETURN (i);
            }
            break;
         case CDOM_type:
	    CO = (SUMA_CIFTI_DO *)dov[i].OP;
	    if (strcmp(idcode, CO->idcode_str)== 0) {
               SUMA_RETURN (i);
            }
            break;
	 default:
            SUMA_S_Warnv("Object type %d (%s) not checked.\n", 
               dov[i].ObjectType,
               SUMA_ObjectTypeCode2ObjectTypeName(dov[i].ObjectType));
            break;
      }
   }
   SUMA_RETURN(-1);
}

SUMA_ALL_DO* SUMA_whichADO(char *idcode, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_whichADO"};
   int ido = SUMA_whichDO(idcode, dov, N_dov);
   
   if (ido < 0) return(NULL);
   return((SUMA_ALL_DO *)dov[ido].OP);
}

/*!
   ans = SUMA_findSO_inDOv(idcode, dov, N_dov);
   searches all SO_type DO objects for idcode
   
   \param idcode (char *) idcode of SO you are searching for
   \param dov (SUMA_DO*) pointer to vector of Displayable Objects, 
                         typically SUMAg_DOv
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
   ans = SUMA_findVO_inDOv(idcode, dov, N_dov);
   searches all VO_type DO objects for idcode
   
   \param idcode (char *) idcode of VO you are searching for
   \param dov (SUMA_DO*) pointer to vector of Displayable Objects, 
                         typically SUMAg_DOv
   \param N_dov (int) number of DOs in dov
   \return ans (int) index into dov of object with matching idcode 
       -1 if not found
  \sa SUMA_findVOp_inDOv
*/
int SUMA_findVO_inDOv(char *idcode, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_findVO_inDOv"};
   SUMA_VolumeObject *VO;
   int i;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;

   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == VO_type) {
         VO = (SUMA_VolumeObject *)dov[i].OP;
         if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: Comparing \n\t:%s:to\n\t:%s:\n", 
                     FuncName, idcode, VO->idcode_str);
         if (strcmp(idcode, VO->idcode_str)== 0) {
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

/*!
   
  VO = SUMA_findVOp_inDOv(char *idcode, SUMA_DO *dov, int N_dov)
   searches all VO_type DO objects for idcode
   
   \param idcode (char *) idcode of VO you are searching for
   \param dov (SUMA_DO*) pointer to vector of Displayable Objects, 
                         typically SUMAg_DOv
   \param N_dov (int) number of DOs in dov
   \return VO (SUMA_VolumeObject *) pointer of VO with matching idcode 
       NULL if not found
   \sa SUMA_findVO_inDOv
*/
SUMA_VolumeObject * SUMA_findVOp_inDOv(char *idcode, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_findVOp_inDOv"};
   SUMA_VolumeObject *VO;
   int i;
   
   SUMA_ENTRY;

   if (!idcode) SUMA_RETURN(NULL);
   
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == VO_type) {
         VO = (SUMA_VolumeObject *)dov[i].OP;
         if (strcmp(idcode, VO->idcode_str)== 0) {
            SUMA_RETURN (VO);
         }
      }
   }
   SUMA_RETURN(NULL);
}

/*!
   
  SO = SUMA_FindSOp_inDOv_from_N_Node(
                        int N_Node, SUMA_SO_SIDE side, 
                        int check_unique, int return_parent,
                        SUMA_DO *dov, int N_dov)
   searches all SO_type DO objects for a surface that best accommodates
   a mesh with N_Node node .
   
   \param N_Node (int) Number of nodes forming the mesh
   \param side (SUMA_SO_SIDE) surface must be of the same side as 'side'
   \param check_unique (int): if 1, then be sure there is only one match
   \param return_parent (int): return local domain parent for that surface
   \param dov (SUMA_DO*) pointer to vector of Displayable Objects, 
                         typically SUMAg_DOv
   \param N_dov (int) number of DOs in dov
   \return SO (SUMA_SurfaceObject *) pointer of SO with matching criteria
   
   Search logic:
      For each surface found:
         if SO->N_Node == N_Node && same side the return 
       
       NULL if not found
   \sa SUMA_findSO_inDOv
*/

SUMA_SurfaceObject *SUMA_FindSOp_inDOv_from_N_Node(
                        int N_Node, SUMA_SO_SIDE side, 
                        int check_unique, int return_parent,
                        SUMA_DO *dov, int N_dov) 
{
   static char FuncName[]={"SUMA_FindSOp_inDOv_from_N_Node"};
   int nFound=0, i = 0;
   SUMA_SurfaceObject *SO=NULL, *tSO=NULL;
   
   SUMA_ENTRY;
   
   i=0;
   while ((!nFound || check_unique) && i < N_dov) {
      if (dov[i].ObjectType == SO_type) {
         tSO = (SUMA_SurfaceObject *)dov[i].OP;
         if (return_parent && 
             !SUMA_isLocalDomainParent(tSO)) { /* Need parent only */
            if (!(tSO = SUMA_findSOp_inDOv(
                           tSO->LocalDomainParentID, dov, N_dov))) {
               goto NEXT;
            }
         }
         if (  tSO != SO /* Happens often if you are picking parents */&&
               tSO->N_Node == N_Node) { /* candidate */
            if (side == SUMA_RIGHT || side == SUMA_LEFT || side == SUMA_LR) {
               if (tSO->Side != side) {
                  goto NEXT;
               } 
            }
            /* Looks like we made it */
            if (!SO) SO = tSO; /* only find the first */
            ++nFound;
         }
      }
      NEXT:
      ++i;   
   }
   
   if (check_unique && nFound > 1) {
      if (check_unique > 0) { /* error */
         SUMA_SLP_Err("More than 1 SO candidate found"); 
      } else { /* warning */
         SUMA_SLP_Warn("More than 1 SO candidate found. Returning first."); 
      }
   }
   
   SUMA_RETURN(SO);
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

int SUMA_isSurfaceOfSide(SUMA_SurfaceObject *SO, SUMA_SO_SIDE ss)
{
   if (!SO) return(0);
   if (SO->Side == ss) return(1);
   if (ss == SUMA_NO_SIDE || ss == SUMA_SIDE_ERROR) return(1);
   return(0);
}

int SUMA_BiggestLocalDomainParent_Side(SUMA_DO *dov, int N_dov, SUMA_SO_SIDE ss)
{
   static char FuncName[]={"SUMA_BiggestLocalDomainParent_Side"};
   SUMA_SurfaceObject *SO;
   int i, imax = -1, MaxNode=-1;
   
   SUMA_ENTRY;
   MaxNode = -1;
   imax = -1;
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == SO_type) {
         SO = (SUMA_SurfaceObject *)dov[i].OP;
         if (SUMA_isLocalDomainParent(SO) && SUMA_isSurfaceOfSide(SO,ss)) {
            if (SO->N_Node > MaxNode) {
               imax = i;
               MaxNode= SO->N_Node;
            }
         }
      }
   }
   SUMA_RETURN(imax);
}

/* Try to find any idcode_str, not complete yet */
void *SUMA_find_any_object(char *idcode_str, SUMA_DO_Types *do_type)
{
   static char FuncName[]={"SUMA_find_any_object"};
   int i;
   void *PP=NULL;
   
   SUMA_ENTRY;
   
   if (!idcode_str) SUMA_RETURN(PP);
   if (do_type) *do_type = NOT_SET_type;
   if ((PP = SUMA_FindDset_s(idcode_str, SUMAg_CF->DsetList))) {
      if (do_type) {
         if (SUMA_isGraphDset((SUMA_DSET *)PP)) *do_type = GDSET_type;
	 else if (SUMA_isMD_Dset((SUMA_DSET *)PP)) *do_type = MD_DSET_type;
         else *do_type = ANY_DSET_type;
      }
      SUMA_RETURN(PP);
   } else if ((PP = SUMA_findSOp_inDOv (idcode_str, SUMAg_DOv, SUMAg_N_DOv))){
      if (do_type) *do_type = SO_type;
      SUMA_RETURN(PP);
   } else if ((PP = SUMA_findVOp_inDOv (idcode_str, SUMAg_DOv, SUMAg_N_DOv))){
      if (do_type) *do_type = VO_type;
      SUMA_RETURN(PP);
   } else if ((i = SUMA_FindDOi_byID(SUMAg_DOv, SUMAg_N_DOv,idcode_str))>=0) {
      PP = (SUMAg_DOv[i].OP);
      if (do_type) *do_type = SUMAg_DOv[i].ObjectType;
      SUMA_RETURN(PP);
   } else {
      /* Can still search in overlay planes, ROIs, etc. 
         But wait until we need them .. */    
   }
   
   SUMA_RETURN(NULL);

}

SUMA_SurfaceObject * SUMA_findanySOp(int *dov_id) {
   return(SUMA_findanySOp_inDOv(SUMAg_DOv, SUMAg_N_DOv, dov_id));
}
SUMA_SurfaceObject * SUMA_findanySOp_inDOv(SUMA_DO *dov, int N_dov, int *dov_id)
{
   static char FuncName[]={"SUMA_findanySOp_inDOv"};
   SUMA_SurfaceObject *SO;
   int i;
   
   SUMA_ENTRY;
   
   if (dov_id) *dov_id = -1;
   SO = NULL;
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == SO_type) {
         SO = (SUMA_SurfaceObject *)dov[i].OP;
         if (dov_id) *dov_id = i;
         SUMA_RETURN (SO);
      }
   }
   
   SUMA_RETURN(NULL);
}

SUMA_VolumeObject * SUMA_findanyVOp(int *dov_id) {
   return(SUMA_findanyVOp_inDOv(SUMAg_DOv, SUMAg_N_DOv, dov_id));
}
SUMA_VolumeObject * SUMA_findanyVOp_inDOv(SUMA_DO *dov, int N_dov, int *dov_id)
{
   static char FuncName[]={"SUMA_findanyVOp_inDOv"};
   SUMA_VolumeObject *VO;
   int i;
   
   SUMA_ENTRY;
   
   if (dov_id) *dov_id = -1;
   VO = NULL;
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == VO_type) {
         VO = (SUMA_VolumeObject *)dov[i].OP;
         if (dov_id) *dov_id = i;
         SUMA_RETURN (VO);
      }
   }
   
   SUMA_RETURN(NULL);
}

SUMA_TractDO * SUMA_findanyTDOp(int *dov_id) {
   return(SUMA_findanyTDOp_inDOv(SUMAg_DOv, SUMAg_N_DOv, dov_id));
}

SUMA_TractDO * SUMA_findanyTDOp_inDOv(SUMA_DO *dov, int N_dov, int *dov_id)
{
   static char FuncName[]={"SUMA_findanyTDOp_inDOv"};
   SUMA_TractDO *TDO;
   int i;
   
   SUMA_ENTRY;
   
   if (dov_id) *dov_id = -1;
   TDO = NULL;
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == TRACT_type) {
         TDO = (SUMA_TractDO *)dov[i].OP;
         if (dov_id) *dov_id = i;
         SUMA_RETURN (TDO);
      }
   }
   
   SUMA_RETURN(NULL);
}

SUMA_MaskDO * SUMA_findanyMDOp(int *dov_id) 
{
   return(SUMA_findanyMDOp_inDOv(SUMAg_DOv, SUMAg_N_DOv, dov_id));
}

SUMA_MaskDO * SUMA_findanyMDOp_inDOv(SUMA_DO *dov, int N_dov, int *dov_id)
{
   static char FuncName[]={"SUMA_findanyMDOp_inDOv"};
   SUMA_MaskDO *MDO;
   int i;
   
   SUMA_ENTRY;
   
   if (dov_id) *dov_id = -1;
   MDO = NULL;
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == MASK_type) {
         MDO = (SUMA_MaskDO *)dov[i].OP;
         if (dov_id) *dov_id = i;
         SUMA_RETURN (MDO);
      }
   }
   
   SUMA_RETURN(NULL);
}

SUMA_MaskDO * SUMA_findShadowMDOp_inDOv(SUMA_DO *dov, int N_dov, int *dov_id)
{
   static char FuncName[]={"SUMA_findShadowMDOp_inDOv"};
   SUMA_MaskDO *MDO;
   int i;
   
   SUMA_ENTRY;
   
   if (dov_id) *dov_id = -1;
   MDO = NULL;
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == MASK_type) {
         MDO = (SUMA_MaskDO *)dov[i].OP;
         if (MDO_IS_SHADOW(MDO)) {
            if (dov_id) *dov_id = i;
            SUMA_RETURN (MDO);
         }
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

char *SUMA_find_VOLabel_from_idcode (char *idcode, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_find_VOLabel_from_idcode"};
   SUMA_VolumeObject *VO;
   int i;
   
   SUMA_ENTRY;
   
   if (!idcode) SUMA_RETURN(NULL);
   
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == VO_type) {
         VO = (SUMA_VolumeObject *)dov[i].OP;
         if (strcmp(idcode, VO->idcode_str)== 0) {
            SUMA_RETURN (VO->Label);
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
               SUMA_S_Errv("More than one surface with label %s found.\n",    
                           label);
               SUMA_RETURN(NULL);
            }
         }
      }
   }
   
   if (!found) { /* try less stringent search */
      for (i=0; i<N_dov; ++i) {
         if (dov[i].ObjectType == SO_type) {
            SO = (SUMA_SurfaceObject *)dov[i].OP;
            if (SUMA_iswordin(SO->Label, label)) {
               if (!found) { found = SO->idcode_str; }
               else {
                  SUMA_S_Errv(
               "Found more than one surface with labels patially matching %s.\n"
               "For example: surfaces %s, and %s .\n",    
                              label,
                              SUMA_find_SOLabel_from_idcode(found, dov, N_dov),
                              SO->Label);
                  SUMA_RETURN(NULL);
               }
            }
         }
      }
   }
   
   if (!found) { /* even less stringent search */
      for (i=0; i<N_dov; ++i) {
         if (dov[i].ObjectType == SO_type) {
            SO = (SUMA_SurfaceObject *)dov[i].OP;
            if (SUMA_iswordin_ci(SO->Label, label)) {
               if (!found) { found = SO->idcode_str; }
               else {
                  SUMA_S_Errv(
               "Found more than one surface with labels patially matching %s.\n"
               "For example: surfaces %s, and %s .\n",    
                              label,
                              SUMA_find_SOLabel_from_idcode(found, dov, N_dov),
                              SO->Label);
                  SUMA_RETURN(NULL);
               }
            }
         }
      }
   }
   SUMA_RETURN(found);
}

char *SUMA_find_ADOidcode_from_label (char *label, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_find_ADOidcode_from_label"};
   SUMA_ALL_DO *ADO;
   int i;
   char *found = NULL;
   
   SUMA_ENTRY;
   
   if (!label) SUMA_RETURN(NULL);
   
   for (i=0; i<N_dov; ++i) {
      {
         ADO = (SUMA_ALL_DO *)dov[i].OP;
         if (strcmp(label, ADO_LABEL(ADO))== 0) {
            if (!found) { found = ADO_ID(ADO); }
            else {
               SUMA_S_Errv("More than one ADO with label %s found.\n",    
                           label);
               SUMA_RETURN(NULL);
            }
         }
      }
   }
   
   if (!found) { /* try less stringent search */
      for (i=0; i<N_dov; ++i) {
         {
            ADO = (SUMA_ALL_DO *)dov[i].OP;
            if (SUMA_iswordin(ADO_LABEL(ADO), label)) {
               if (!found) { found = ADO_ID(ADO); }
               else {
                  SUMA_S_Errv(
               "Found more than one ADO with labels partially matching %s.\n"
               "For example: surfaces %s, and %s .\n",    
                              label,
                              ADO_LABEL(SUMA_whichADO(found, dov, N_dov)),
                              ADO_LABEL(ADO));
                  SUMA_RETURN(NULL);
               }
            }
         }
      }
   }
   
   if (!found) { /* even less stringent search */
      for (i=0; i<N_dov; ++i) {
         {
            ADO = (SUMA_ALL_DO *)dov[i].OP;
            if (SUMA_iswordin_ci(ADO_LABEL(ADO), label)) {
               if (!found) { found = ADO_ID(ADO); }
               else {
                  SUMA_S_Errv(
               "Found more than one surface with labels patially matching %s.\n"
               "For example: surfaces %s, and %s .\n",    
                              label,
                              ADO_LABEL(SUMA_whichADO(found, dov, N_dov)),
                              ADO_LABEL(ADO));
                  SUMA_RETURN(NULL);
               }
            }
         }
      }
   }
   SUMA_RETURN(found);
}

char *SUMA_find_VOidcode_from_label (char *label, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_find_VOidcode_from_label"};
   SUMA_VolumeObject *VO;
   int i;
   char *found = NULL;
   
   SUMA_ENTRY;
   
   if (!label) SUMA_RETURN(NULL);
   
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == VO_type) {
         VO = (SUMA_VolumeObject *)dov[i].OP;
         if (strcmp(label, VO->Label)== 0) {
            if (!found) { found = VO->idcode_str; }
            else {
               SUMA_S_Errv("More than one volume with label %s found.\n",    
                           label);
               SUMA_RETURN(NULL);
            }
         }
      }
   }
   
   if (!found) { /* try less stringent search */
      for (i=0; i<N_dov; ++i) {
         if (dov[i].ObjectType == VO_type) {
            VO = (SUMA_VolumeObject *)dov[i].OP;
            if (SUMA_iswordin(VO->Label, label)) {
               if (!found) { found = VO->idcode_str; }
               else {
                  SUMA_S_Errv(
               "Found more than one volume with labels patially matching %s.\n"
               "For example: volumes %s, and %s .\n",    
                              label,
                              SUMA_find_VOLabel_from_idcode(found, dov, N_dov),
                              VO->Label);
                  SUMA_RETURN(NULL);
               }
            }
         }
      }
   }
   
   if (!found) { /* even less stringent search */
      for (i=0; i<N_dov; ++i) {
         if (dov[i].ObjectType == VO_type) {
            VO = (SUMA_VolumeObject *)dov[i].OP;
            if (SUMA_iswordin_ci(VO->Label, label)) {
               if (!found) { found = VO->idcode_str; }
               else {
                  SUMA_S_Errv(
               "Found more than one volume with labels patially matching %s.\n"
               "For example: volumes %s, and %s .\n",    
                              label,
                              SUMA_find_VOLabel_from_idcode(found, dov, N_dov),
                              VO->Label);
                  SUMA_RETURN(NULL);
               }
            }
         }
      }
   }
   SUMA_RETURN(found);
}

/*!
   
  VO = SUMA_find_named_VOp_inDOv(char *filename, SUMA_DO *dov, int N_dov)
   searches all VO_type DO objects constructed from a dset from file filename
   
   \param filename (char *) filename of VO (as returned by DSET_HEADNAME) 
   \param dov (SUMA_DO*) pointer to vector of Displayable Objects, 
                        typically SUMAg_DOv
   \param N_dov (int) number of DOs in dov
   \return VO (SUMA_VolumeObject *) pointer of VO with matching idcode 
       NULL if not found
   \sa SUMA_findVO_inDOv
   \sa SUMA_findVOp_inDOv
*/
SUMA_VolumeObject * SUMA_find_named_VOp_inDOv( char *filename, 
                                                SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_find_named_VOp_inDOv"};
   SUMA_VolumeObject *VO = NULL, *VOf = NULL;
   SUMA_STRING *SS=NULL;
   char *stmp=NULL, *coordname=NULL;
   int i;
   SUMA_FileName sf;
   
   SUMA_ENTRY;
   
   if (!filename || !dov) SUMA_RETURN(NULL);
   
   
   i=0;
   VOf = NULL;
   while (i<N_dov) {
      if (dov[i].ObjectType == VO_type) {
         VO = (SUMA_VolumeObject *)dov[i].OP;
         if (VO->VE && VO->VE[0] &&
             !strcmp(filename, SUMA_VE_Headname(VO->VE, 0))) {
            if (VOf) {
               SUMA_S_Errv("Volume name %s\n"
                           "is not a unique identifier.\n",
                           filename);
               SUMA_RETURN(NULL);
            }
            VOf = VO;
         }
      }
      ++i;
   }
   
   SUMA_RETURN(VOf);
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
                     SUMA_SL_Err("%s",stmp); 
                     if (stmp) SUMA_free(stmp); stmp = NULL;
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
            case SUMA_OBJ_MESH:
            case SUMA_OPENDX_MESH:
            case SUMA_PREDEFINED:
            case SUMA_BRAIN_VOYAGER:
            case SUMA_BYU:
            case SUMA_GIFTI:
            case SUMA_MNI_OBJ:
            case SUMA_STL:
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
                     SUMA_SL_Err("%s",stmp); 
                     if (stmp) SUMA_free(stmp); stmp = NULL;
                     if (sf.FileName) SUMA_free(sf.FileName); 
                        sf.FileName = NULL;
                     if (sf.Path) SUMA_free(sf.Path); 
                        sf.Path = NULL;                     SUMA_RETURN(NULL);
                  }
                  SOf = SO;
               }
               break;
            default: 
               SUMA_SL_Err("Filetype not supported.");
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


SUMA_Boolean SUMA_isRelated( SUMA_ALL_DO *ado1, SUMA_ALL_DO *ado2 , int level)
{
   static char FuncName[]={"SUMA_isRelated"};
   char *p1=NULL, *p2=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado1 || !ado2) SUMA_RETURN(NOPE);
   
   if (LocalHead) SUMA_DUMP_TRACE("Relatives!");
   
   if (ado1 == ado2) SUMA_RETURN(YUP);
   
   switch (ado1->do_type) {
      case SO_type:
         if (ado1->do_type != ado2->do_type) SUMA_RETURN(NOPE); 
         SUMA_RETURN(SUMA_isRelated_SO((SUMA_SurfaceObject *)ado1,
                                       (SUMA_SurfaceObject *)ado2,level));
         break;
      case GDSET_type:
      case GRAPH_LINK_type:
         if (ado2->do_type != GDSET_type && ado2->do_type != GRAPH_LINK_type) 
                                                         SUMA_RETURN(NOPE); 
         if ((p1=SUMA_ADO_Parent_idcode(ado1)) && 
             (p2=SUMA_ADO_Parent_idcode(ado2)) &&
             !strcmp(p1,p2)) {
            SUMA_RETURN(YUP);
         }
         SUMA_RETURN(NOPE);
         break;
      case CDOM_type:
         SUMA_S_Err("Fill me with love. Some day we might have 'isotopic' CIFTI "
                    "domains. Let us leave complications alone for now");
         SUMA_RETURN(NOPE);
         break;
      case VO_type:
      case TRACT_type:
         if (ado2->do_type != ado1->do_type) SUMA_RETURN(NOPE);
         if ((p1=SUMA_ADO_Parent_idcode(ado1)) && 
             (p2=SUMA_ADO_Parent_idcode(ado2)) &&
             !strcmp(p1,p2)) {
            SUMA_RETURN(YUP);
         } else if (!p1 && !p2 && !strcmp(ADO_ID(ado1),ADO_ID(ado2))) {
            SUMA_RETURN(YUP);
         }
         SUMA_RETURN(NOPE);
         break;
      case MASK_type:
         if (ado2->do_type == ado1->do_type ||
             ado2->do_type == TRACT_type) {
            SUMA_RETURN(YUP);
         }
         SUMA_RETURN(NOPE);
         break;
      default:
         SUMA_S_Errv("Not ready to deal with type %s\n",
                     SUMA_ObjectTypeCode2ObjectTypeName(ado1->do_type));
         SUMA_RETURN(NOPE);
   }
   SUMA_RETURN(NOPE);
}

/*!
   \brief ans = SUMA_isRelated_SO (SUMA_SurfaceObject *SO1, 
                                   SUMA_SurfaceObject *SO2, int level);
   
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
SUMA_Boolean SUMA_isRelated_SO ( SUMA_SurfaceObject *SO1, 
                                 SUMA_SurfaceObject *SO2, int level)
{
   static char FuncName[]={"SUMA_isRelated_SO"};
   SUMA_DOMAIN_KINSHIPS kin;
   static int iwarn=0;
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
                  if (!(iwarn % 25)) {
                     SUMA_S_Note( "Surfaces appear related at level 2 "
                               "but sides are not the same.\n"                                                   "Kinship level is being ignored.\n"
                               "(Message shown intermittenly)\n");
                  } 
                  if ((SO1->Side < SUMA_LR || SO2->Side < SUMA_LR)) {
                     SUMA_S_Note("Surface sides are not clearly defined. "
                                 "If this is in error, consider adding \n"
                                 "Hemisphere = R  (or L or B) in the spec file\n"
                                 "to make sure surfaces sides are correctly "
                                 "labeled.\n");
                  }
                  ++iwarn;
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
   static int iwarn=0;
   
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
         if (!iwarn) {
            SUMA_S_Warn("Surface sides are not clearly defined. "
                     "If this is in error, consider adding \n"
                     "Hemisphere = R  (or L or B) in the spec file\n"
                     "to make sure surfaces sides are correctly "
                     "labeled.\n"
                     "Similar warnings will be muted\n");   
            ++iwarn;
         }
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
   
   if (SOC && SUMA_isRelated_SO(SOC, SO, 1)) {
      SUMA_S_Warn("Unexpected surface pair with same localdomainparent.\n"
                  "Good Luck To You");
   }
   /*
      if (SOC) SOC = SUMA_findSOp_inDOv( SOC->LocalDomainParentID, 
                                      dov, N_dov);
   */

   
   SUMA_RETURN(SOC);
}

SUMA_Boolean SUMA_isContralateral_name(char *s1, char *s2) 
{
   static char FuncName[]={"SUMA_isContralateral_name"};
   char *sd=NULL, *sc1=NULL, *sc2=NULL;
   int ic1, ic2;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   if (s1 && s2 && strstr(s1,"FuncAfni_") && strstr(s2,"FuncAfni_") &&
       (sc1 = strstr(s1,TMP_NAME_SEP)) && (sc2 = strstr(s2,TMP_NAME_SEP)) ) {
      /* temporarily mask the idcode part */
      ic1 = sc1-s1; ic2 = sc2 - s2;
      s1[ic1] = '\0';
      s2[ic2] = '\0';   
   }
   sd = SUMA_StringDiff(s1,s2);
   /* now put things back */
   if (sc1 && sc2) {
      s1[ic1] = sc1[0];
      s2[ic2] = sc2[0];
   }
   sc1 = sc2 = NULL;
   
   SUMA_LHv("Diff of \n%s and \n%s is \n%s\n", 
            CHECK_NULL_STR(s1), CHECK_NULL_STR(s2), CHECK_NULL_STR(sd));
   
   if (!sd || sd[0] == '\0') SUMA_RETURN(NOPE);
   
   /* If name is for live dsets from AFNI, make sure you cut after idcode_str */
   /* check for l or r only */
   if (sd[0] != 'l' && sd[0] != 'L' && sd[0] != 'r' && sd[0] != 'R') {
      /* not begginning with l or r */
      SUMA_free(sd); SUMA_RETURN(NOPE);
   } else if (sd[1] == '\0') { /* make sure it is only l or r */
      SUMA_free(sd); SUMA_RETURN(YUP);
   }
   if (strstr(s1,"GRP_ICORR") && strstr(s2,"GRP_ICORR")) {
      /* special treatment */
      if (strncasecmp(sd,"left",4) && strncasecmp(sd,"right",5)) {
         SUMA_free(sd); SUMA_RETURN(NOPE);
      }  
   } else {
      /* not left and not right? */
      if (strcasecmp(sd,"left") && strcasecmp(sd,"right")) {
         SUMA_free(sd); SUMA_RETURN(NOPE);
      }
   }
   SUMA_free(sd);
   SUMA_RETURN(YUP);
}

char *SUMA_Contralateral_file(char *f1) 
{
   static char FuncName[]={"SUMA_Contralateral_file"};
   char *f1C=NULL, *ff1=NULL;
   int ii=0;
   THD_string_array *sar=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!f1) SUMA_RETURN(f1C);
   
   if (!(sar=THD_get_all_files(SUMA_FnameGet(f1, "pa", SUMAg_CF->cwd),0))){
      SUMA_RETURN(f1C);
   }
   
   ff1 = SUMA_FnameGet(f1, "F", SUMAg_CF->cwd);
   for( ii=0 ; ii < sar->num ; ii++ ) {
      SUMA_LHv("%s vs\n"
               "%s\n", sar->ar[ii], ff1);
      if (SUMA_isContralateral_name(sar->ar[ii], ff1)) {
         if (!f1C) {
            f1C = SUMA_copy_string(sar->ar[ii]);
         } else {
            /* ambiguous contralateral files names */
            SUMA_S_Warnv("Found more than 1 contralateral candidates for %s\n"
                         "%s and %s\n",
                         ff1, f1C, sar->ar[ii]);
            DESTROY_SARR(sar) ; SUMA_free(f1C); f1C = NULL;
            SUMA_RETURN(f1C);
         }  
      }
   }

   DESTROY_SARR(sar) ; sar = NULL;
   if (f1C) {
      SUMA_LHv("%s is matched by\n%s\n",
            f1C, f1);
   } else {
      SUMA_LHv("No cigar for %s\n", f1);
   }
   SUMA_RETURN(f1C);
}  

SUMA_DSET * SUMA_Contralateral_dset(SUMA_DSET *dset, SUMA_SurfaceObject *SO, 
                                    SUMA_SurfaceObject**SOCp)
{
   static char FuncName[]={"SUMA_Contralateral_dset"};
   SUMA_DSET *cdset=NULL, *dd=NULL;
   DListElmt *el=NULL;
   char *namediff=NULL;
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
         if (  SUMA_isContralateral_name(SDSET_FILENAME(dset),
                                         SDSET_FILENAME(dd)) &&
               SUMA_isSameDsetColTypes(dset, dd) ) {
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

SUMA_OVERLAYS *SUMA_Contralateral_overlay(SUMA_OVERLAYS *over,
                                          SUMA_SurfaceObject *SO, 
                                    SUMA_SurfaceObject**SOCp)
{
   static char FuncName[]={"SUMA_Contralateral_overlay"};
   SUMA_DSET *dsetC=NULL, *dset=NULL, *dd=NULL;
   DListElmt *el=NULL;
   SUMA_OVERLAYS *overC=NULL;
   int OverInd = -1;
   char *namediff=NULL;
   SUMA_SurfaceObject *SOC=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!over || !over->dset_link) { 
      SUMA_S_Errv("NULL input (%p) or NULL dset_link (%p)",
                  over, over ? over->dset_link:NULL);
      SUMA_RETURN(overC);
   }
   
   dset = over->dset_link;
   if (!SO) {
      if (!(SO = 
            SUMA_findSOp_inDOv(  SUMA_sdset_idmdom(dset), 
                                 SUMAg_DOv, SUMAg_N_DOv))){
         SUMA_S_Err("Can't find dset's domain parent");
         SUMA_RETURN(overC);
      }
   }
   
   if (!(SOC = SUMA_Contralateral_SO(SO, SUMAg_DOv, SUMAg_N_DOv))) {
      /* Nothing there, return */
      SUMA_RETURN(overC);
   }
   
   SUMA_LH("Have contralateral surface to consider");
   el = dlist_head(SUMAg_CF->DsetList);
   while (el) {
      dd = (SUMA_DSET*)el->data;
      if (SUMA_isDsetRelated(dd,SOC)) {
         SUMA_LHv("Have Dset %s (filename %s) related to SOC\n", 
                  SDSET_LABEL(dd), SDSET_FILENAME(dd));
         /* Does dd relate to dset ? */
         if (  SUMA_isContralateral_name(SDSET_FILENAME(dset),
                                          SDSET_FILENAME(dd)) &&
               SUMA_isSameDsetColTypes(dset, dd) ) {
            if (!dsetC) {
               dsetC = dd;
            }else {
               SUMA_S_Warn("More than one dset matches\n"
                           "Returning NULL");
               SUMA_RETURN(NULL);
            }
         } 
      }
      el = dlist_next(el);
   }
   if (!dsetC) {
      SUMA_LH("Quest failed, returning");
      SUMA_RETURN(overC);
   }
   if (!(overC=SUMA_Fetch_OverlayPointerByDset ((SUMA_ALL_DO *)SOC, 
                                                dsetC, &OverInd))) {
      SUMA_S_Err("Failed oh failed to find overlay for contralateral dset");            SUMA_RETURN(NULL);      
   }
   
   if (!SUMA_isADO_Cont_Realized((SUMA_ALL_DO *)SOC)) {
      if (!(SUMA_OpenCloseSurfaceCont(NULL, (SUMA_ALL_DO *)SOC, NULL))) {
         SUMA_S_Err("Could not ensure controller is ready");
         SOC = NULL; overC=NULL;
      } 
   }
   if (SOCp) *SOCp=SOC;
   SUMA_RETURN(overC);
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
   
   if (!SO1 && !SO2) {
      SUMA_S_Err("Total NULLness");
      SUMA_RETURN (SUMA_DOMAINS_NOT_RELATED);
   }
   
   if (!SO1 || !SO2) {
      SUMA_LH("One NULL SO"); /* Don't die, it is ok */
      SUMA_RETURN (SUMA_DOMAINS_NOT_RELATED);
   }
   
   if (!SO1->idcode_str || !SO2->idcode_str) {
      SUMA_S_Err("NULL SO->idcode_str.");
      if (LocalHead) SUMA_DUMP_TRACE("Strange init");
      SUMA_RETURN (SUMA_DOMAINS_NOT_RELATED);
   }
   
   if (SO1 == SO2 ||
       strcmp (SO1->idcode_str, SO2->idcode_str) == 0) {
      /* SO1 = SO2 */
      SUMA_LH("%s",SUMA_DomainKinships_String (SUMA_SO1_is_SO2));
      SUMA_RETURN (SUMA_SO1_is_SO2);
   }
   
   if (SO1->LocalDomainParentID) {
      if (strcmp (SO1->LocalDomainParentID, SO2->idcode_str) == 0) {
         /* SO2 is the local domain parent of SO1 */
         SUMA_LH("%s",SUMA_DomainKinships_String (SUMA_SO2_is_LDPSO1));
         SUMA_RETURN (SUMA_SO2_is_LDPSO1);
      }
   }
   
   if (SO2->LocalDomainParentID) {
      if (strcmp (SO1->idcode_str, SO2->LocalDomainParentID) == 0) {
          /* SO1 is the local domain parent of SO2 */
          SUMA_LH("%s",SUMA_DomainKinships_String (SUMA_SO1_is_LDPSO2));
          SUMA_RETURN (SUMA_SO1_is_LDPSO2);
      }
   }
   
   if (SO1->LocalDomainParentID && SO2->LocalDomainParentID) {
      if (strcmp (SO1->LocalDomainParentID, SO2->LocalDomainParentID) == 0) {
         /* SO1 and SO2 have the same local domain parent */
         SUMA_LH("%s",SUMA_DomainKinships_String (SUMA_LDPSO1_is_LDPSO2));
         SUMA_RETURN (SUMA_LDPSO1_is_LDPSO2);
      }
   }
   
   if (SO1->DomainGrandParentID && SO2->idcode_str) {
      if (strcmp (SO1->DomainGrandParentID, SO2->idcode_str) == 0) {
         /* SO2 is the grand daddy of SO1 */
         SUMA_LH("%s",SUMA_DomainKinships_String (SUMA_SO2_is_GPSO1));
         SUMA_RETURN (SUMA_SO2_is_GPSO1);
      }
   }
   
   if (SO1->idcode_str && SO2->DomainGrandParentID) {
      if (strcmp (SO1->idcode_str, SO2->DomainGrandParentID) == 0) {
         /* SO1 is the grand daddy of SO2 */
         SUMA_LH("%s",SUMA_DomainKinships_String (SUMA_SO1_is_GPSO2));
         SUMA_RETURN (SUMA_SO1_is_GPSO2);
      }
   }
   
   if (SO1->DomainGrandParentID && SO2->DomainGrandParentID) {
      if (strcmp (SO1->DomainGrandParentID, SO2->DomainGrandParentID) == 0) {
         /* SO1 and SO2 have the same grand daddy */
         SUMA_LH("%s", SUMA_DomainKinships_String (SUMA_GPSO1_is_GPSO2));
         SUMA_RETURN (SUMA_GPSO1_is_GPSO2);
      }
   }
   if (SO1->N_Node == SO2->N_Node) {
      SUMA_LH("%s", SUMA_DomainKinships_String (SUMA_N_NODE_SAME));
      SUMA_RETURN (SUMA_N_NODE_SAME);
   }
  
   SUMA_LH("%s", SUMA_DomainKinships_String (SUMA_DOMAINS_NOT_RELATED));
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
   \brief SUMA_Boolean SUMA_isVO (SUMA_DO DO) 
   returns YUP if DO is of VO_type
   ans = SUMA_isVO (DO) ;
   
   \sa SUMA_isVO_G (SUMA_DO DO, char *Group)
*/
SUMA_Boolean SUMA_isVO (SUMA_DO DO) 
{
   static char FuncName[]={"SUMA_isVO"};
   
   SUMA_ENTRY;

   if (DO.ObjectType == VO_type) {
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
SUMA_ASSEMBLE_LIST_STRUCT * SUMA_AssembleAllROIList (SUMA_DO * dov, int N_dov, 
                                                      SUMA_Boolean SortByLabel) 
{
   static char FuncName[]={"SUMA_AssembleAllROIList"};
   int i=-1, N_clist=-1; 
   DList *list=NULL, *listop = NULL;
   DListElmt *Elm = NULL, *Elmop = NULL;
   char Label[SUMA_MAX_NAME_LENGTH], 
        Parent_Label[SUMA_MAX_NAME_LENGTH], *store=NULL;
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
         if (LocalHead) 
            fprintf (SUMA_STDERR, "%s: Found an ROI %s\n", FuncName, ROI->Label);
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
         store = (char *)SUMA_calloc(strlen(Label)+strlen(Parent_Label)+5, 
                                       sizeof(char));
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
   str->content_id = NULL;
   SUMA_RETURN(str);
}
/*!
   \brief frees SUMA_ASSEMBLE_LIST_STRUCT *
   \param SUMA_ASSEMBLE_LIST_STRUCT *str
   
   \return NULL always
   
   -This function frees each string in clist. BUT NOT pointers in oplist (for obvious reasons)
*/
SUMA_ASSEMBLE_LIST_STRUCT *SUMA_FreeAssembleListStruct(
                                                SUMA_ASSEMBLE_LIST_STRUCT *str) 
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
   if (str->content_id) SUMA_free(str->content_id);
   
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
SUMA_DRAWN_ROI * SUMA_FetchROI_InCreation (SUMA_SurfaceObject *SO, 
                                           SUMA_DO * dov, int N_dov) 
{
   int i;
   SUMA_DRAWN_ROI *ROI = NULL;
   static char FuncName[]={"SUMA_FetchROI_InCreation"};
   
   SUMA_ENTRY;
   
   for (i=0; i < N_dov; ++i) {
      if (dov[i].ObjectType == ROIdO_type) {
         ROI = (SUMA_DRAWN_ROI *)dov[i].OP;
         if (ROI->DrawStatus == SUMA_ROI_InCreation) {
            if (SUMA_isdROIrelated (ROI, (SUMA_ALL_DO *)SO)) {
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
   
   /* Just compare the idcodes, not allowing for kinship yet but 
      code below could do that */
   if (strcmp(SO->idcode_str, SDO->Parent_idcode_str) == 0) {
      SUMA_RETURN(YUP);
   } else {
      SUMA_RETURN(NOPE);
   }
   
   SUMA_LHv(" %s SO->LocalDomainParentID\n"
            " %s SDO->Parent_idcode_str\n"
            " %s SO->idcode_str\n", 
      SO->LocalDomainParentID, SDO->Parent_idcode_str, SO->idcode_str);
   
   /* find the pointer to the surface having for an idcode_str: 
         ROI->Parent_idcode_str */
   SO_NB = SUMA_findSOp_inDOv(SDO->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   
   if (!SO_NB) {
      SUMA_SL_Err("Could not find surface of SDO->Parent_idcode_str");
      SUMA_RETURN (NOPE);
   }
   
   if (SUMA_isRelated_SO(SO, SO_NB, 1)){ /* relationship of the 1st order only */
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
   
   SUMA_LHv(" %s SO->LocalDomainParentID\n"
            " %s SDO->Parent_idcode_str\n"
            "  %s SO->idcode_str\n", 
            SO->LocalDomainParentID, Parent_idcode_str, SO->idcode_str);
   
   /* find the pointer to the surface having for an idcode_str: 
      ROI->Parent_idcode_str */
   SO_NB = SUMA_findSOp_inDOv(Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   
   if (!SO_NB) {
      SUMA_SL_Err("Could not find surface of SDO->Parent_idcode_str");
      SUMA_RETURN (NOPE);
   }
   
   if (SUMA_isRelated_SO(SO, SO_NB, 1)){ /*relationship of the 1st order only */
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
SUMA_Boolean SUMA_isdROIrelated (SUMA_DRAWN_ROI *ROI, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_isdROIrelated"};
   SUMA_SurfaceObject *SO_ROI = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado || !ROI) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   switch (ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO = (SUMA_SurfaceObject *)ado;
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
         SO_ROI = SUMA_findSOp_inDOv(ROI->Parent_idcode_str, 
                                     SUMAg_DOv, SUMAg_N_DOv);

         if (!SO_ROI) {
            SUMA_SL_Err("Could not find surface of ROI->Parent_idcode_str");
            SUMA_RETURN(NOPE);
         }

         if (SUMA_isRelated_SO(SO, SO_ROI, 1)) { 
            /* relationship of the 1st order only */ 
            SUMA_RETURN (YUP);
         }
         break; }
      default:
         SUMA_S_Errv("Not ready for %s\n",
             SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         break;
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
   
   /* find the pointer to the surface having for an idcode_str: 
      ROI->Parent_idcode_str */
   SO_ROI = SUMA_findSOp_inDOv(ROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   
   if (!SO_ROI) {
      SUMA_SL_Err("Could not find surface of ROI->Parent_idcode_str");
      SUMA_RETURN (NOPE);
   }
   
   if (SUMA_isRelated_SO(SO, SO_ROI, 1)){/* relationship of the 1st order only */
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
int * SUMA_Build_Mask_AllROI (SUMA_DO *dov, int N_do, SUMA_SurfaceObject *SO, 
                              int *Mask, int *N_added)
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
            if (SUMA_isdROIrelated (D_ROI, (SUMA_ALL_DO *)SO)) {
               SUMA_LH("Found a drawn ROI, building mask...");

               Npart = SUMA_Build_Mask_DrawnROI (D_ROI, Mask);
               if (Npart < 0) {
                  SUMA_S_Err ("Badness in SUMA_Build_Mask_DrawnROI");
                  if (Mask) SUMA_free(Mask);
                  *N_added = -1;
                  SUMA_RETURN(NULL);
               }else {
                  *N_added = *N_added + Npart;
                  SUMA_LHv("%d nodes found in that ROI.\n", Npart);
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
      SUMA_LH("Closing switch ROI window ...");
      SUMA_cb_CloseSwitchROI(NULL, 
                  (XtPointer) SUMAg_CF->X->DrawROI->SwitchROIlst, NULL);
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
      case SUMA_STL:
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

/*!
   \brief Allow users to exclude certain states from being sent
   to AFNI 
   SUMA_ExcludeFromToAfni
*/
int SUMA_ExcludeFromSendToAfni (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_ExcludeFromSendToAfni"};
   
   SUMA_ENTRY;
   
   if (SUMA_EnvEquals("SUMA_DoNotSendStates", SO->State, 1, " ,;")) 
      RETURN(1);
   
   RETURN(0);  
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
      if (xf->XformOpts) NI_free_element(xf->XformOpts); 
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
   snprintf(stmp,sizeof(char)*64,"input.%s",FunctionName);
   NI_rename_group(cb->FunctionInput, stmp);
   
   SUMA_LH("defaults");
   /* Set defaults for generic parameters */
   nel = NI_new_data_element("event_parameters", 0); 
   NI_add_to_group(cb->FunctionInput, nel);
   NI_SET_INT(nel,"event.new_node", -1);
   NI_set_attribute(nel, "event.DO_idcode", "");
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
                                  int refresh, SUMA_ALL_DO *ado,
                                  int doall) 
{
   static char FuncName[]={"SUMA_ExecuteCallback"};
   SUMA_SurfaceObject *curSO=NULL, *targetSO=NULL;
   SUMA_OVERLAYS *targetSover=NULL;
   int i, jj=0;
   SUMA_DSET *targetDset=NULL;
   SUMA_Boolean LocalHead = NOPE;
  
   SUMA_ENTRY;
   
   SUMA_LH("Calling FunctionPtr(cb)");
   cb->FunctionPtr((void *)cb);  

   SUMA_LH("Set callback pending");
   SUMA_SetCallbackPending(cb, 0, SES_Empty);
   
   /* flush event specific parameters */
   SUMA_FlushCallbackEventParameters(cb);
   SUMA_LH("Flushing done");
   if (refresh) {/* Now decide on what needs refreshing */
      if (!ado) {
         curSO = NULL;
      } else {
         curSO = SUMA_Cont_SO(SUMA_ADO_Cont(ado));
      }
      SUMA_LH("curSO = %p, ado = %p", curSO, ado);         
      for (i=0; i<cb->N_parents; ++i) {
         if (SUMA_is_ID_4_DSET(cb->parents[i], &targetDset)) {
            targetSO = SUMA_findSOp_inDOv(cb->parents_domain[i],
                                          SUMAg_DOv, SUMAg_N_DOv);
            if (!targetSO) {
               if (ado && ado->do_type == SO_type) {
                  SUMA_S_Warn("Could not find targetSO, using SO instead");
                  targetSO = (SUMA_SurfaceObject *)ado;
               } else {
                  SUMA_S_Err("Don't know what do do here");
                  SUMA_RETURN(NOPE);
               }
            }
            /* refresh overlay and SO for this callback */
            targetSover = SUMA_Fetch_OverlayPointerByDset(
                                 (SUMA_ALL_DO*)targetSO,
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
               SUMA_UPDATE_ALL_NODE_GUI_FIELDS((SUMA_ALL_DO*)targetSO);
               SUMA_Remixedisplay((SUMA_ALL_DO*)targetSO);
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
            SUMA_UpdatePvalueField( (SUMA_ALL_DO *)targetSO,
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

