/* ZSS: July 21/06 This file should be used with SUMA_COMPILED defined
suma_datasets.c is now in afni's directory and is in libmri.a

*/
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>
#include "mrilib.h"
#include "niml.h"
#include "../niml/niml_private.h"
#include "xutil.h"


#include "SUMA_suma.h"


#if defined SUMA_COMPILED
   extern SUMA_CommonFields *SUMAg_CF;
   extern int SUMAg_N_DOv; 
   extern SUMA_DO *SUMAg_DOv;
#endif 

int SUMA_WriteDset_NameCheck_s (char *Name, SUMA_DSET *dset, SUMA_DSET_FORMAT form, int verb, char **NameOutp) 
{
   int exists = SUMA_WriteDset_NameCheck_eng(Name, dset, form, verb, NameOutp);
   WorkErrLog_s();
   return(exists);
} 


/*!
   \brief parse command line arguments for input/output debugging and
   memory debugging. Use no fancies in this function!
   
   This function is to be called after SUMAg_CF has been created,
   if #ifdef SUMA_COMPILED 
   
   Default for iotrace = 0
               memtrace = 1 
   Those defaults are common to all apps 
   
*/
void SUMA_ParseInput_basics_s (char *argv[], int argc) /* for suma programs */ 
{

   static char FuncName[]={"SUMA_ParseInput_basics_s"};

   if (!SUMA_ParseInput_basics_eng (argv, argc)) return; 
      
   if (get_Doiotrace()) { SUMA_INOUT_NOTIFY_ON; } 
   if (get_Domemtrace()) { SUMA_MEMTRACE_ON; }

   /* some more special ones */
   #ifdef USE_TRACING
      if (get_Doiotrace() == 2) { DBG_trace = 2; } 
   #endif
   
   return;
}

/* A function to executes the proper macros for message generation. This one here is for suma programs */
void WorkErrLog_s(void)
{
   static char FuncName[MAX_ERRLOG_FUNCNAME]={"WorkErrLog_s"};
   SUMA_Boolean LocalHead = NOPE;
   DListElmt *del=NULL;
   SUMA_ERRLOG *el=NULL;
   char mmm[256];
   del = SUMA_PopErrLog(NULL);
   while (del) {
      el = (SUMA_ERRLOG *)del->data;
      sprintf(FuncName, "%s", el->FuncName); 
           if (!strcmp(el->macroname,"L_Err")) { SUMA_L_Err(el->msg); }
      else if (!strcmp(el->macroname,"SL_Err")) { SUMA_SL_Err(el->msg); }
      else if (!strcmp(el->macroname,"SLP_Err")) { SUMA_SLP_Err(el->msg); }
      else if (!strcmp(el->macroname,"L_Warn")) { SUMA_L_Warn(el->msg); }
      else if (!strcmp(el->macroname,"SL_Warn")) { SUMA_SL_Warn(el->msg); }
      else if (!strcmp(el->macroname,"SLP_Warn")) { SUMA_SLP_Warn(el->msg); }
      else if (!strcmp(el->macroname,"L_Note")) { SUMA_L_Note(el->msg); }
      else if (!strcmp(el->macroname,"SL_Note")) { SUMA_SL_Note(el->msg); }
      else if (!strcmp(el->macroname,"SLP_Note")) { SUMA_SLP_Note(el->msg); }
      else if (!strcmp(el->macroname,"L_Crit")) { SUMA_L_Crit(el->msg); }
      else if (!strcmp(el->macroname,"SL_Crit")) { SUMA_SL_Crit(el->msg); }
      else if (!strcmp(el->macroname,"SLP_Crit")) { SUMA_SLP_Crit(el->msg); }
      else {
         snprintf(mmm, 255*sizeof(char), "Bad macroname %s", el->macroname); 
         sprintf(FuncName, "%s", "WorkErrLog_ns"); SUMA_S_Err(mmm);
      }
      del = SUMA_PopErrLog(del);
   }
}


/*!
   \brief look for a dataset with a particular idcode
*/
SUMA_DSET * SUMA_FindDset_s (char *idcode, DList *DsetList)
{
   SUMA_DSET *dset = SUMA_FindDset_eng (idcode, DsetList, NULL);
   WorkErrLog_s();
   return(dset);
}
DListElmt * SUMA_FindDsetEl_s (char *idcode, DList *DsetList)
{
   DListElmt *el=NULL;
   SUMA_DSET *dset = SUMA_FindDset_eng (idcode, DsetList, &el);
   WorkErrLog_s();
   return(el);
}

SUMA_Boolean  SUMA_is_ID_4_DSET(char *idcode, SUMA_DSET **dsetp)
{
   static char FuncName[]={"SUMA_is_ID_4_DSET"};
   SUMA_DSET *dset=NULL;
   
   SUMA_ENTRY;
   
   if (dsetp) *dsetp = NULL;
   if (!idcode) SUMA_RETURN(NOPE);
   
   dset = SUMA_FindDset_s(idcode, SUMAg_CF->DsetList);
   
   if (dset) {
      if (dsetp) *dsetp = dset;
      SUMA_RETURN(YUP);
   }
   
   SUMA_RETURN(NOPE);   
}


/*!
   \brief Returns the index of the node for which 
   data exists in row row of  Dset.
   Set N_Node to SO->N_Node in the function call whenever
   appropriate, it helps the function go faster 
   in certain instances. You can't get SO inside this
   function from MeshParent_idcode of nel because this file 
   is not to know about surface objects.
   Set N_Node to -1 if you don't want to use it 
*/
int SUMA_GetNodeIndex_FromNodeRow_s(SUMA_DSET *dset, int row, int N_Node)
{
   int i=SUMA_GetNodeIndex_FromNodeRow_eng(dset, row, N_Node);
   WorkErrLog_s();
   return(i);
}

/*!
   \brief j = SUMA_GetNodeRow_FromNodeIndex_s( dset, i);
   Returns the row index of a node in the columns
   of a data set. In other terms, node i's data are in 
   row j of the columns in nel 
   for N_Node, see comments in  SUMA_GetNodeIndex_FromNodeRow_s
   \sa SUMA_GetNodeIndex_FromNodeRow_s
*/
int SUMA_GetNodeRow_FromNodeIndex_s(SUMA_DSET *dset, int node, int N_Node)
{
   int i=SUMA_GetNodeRow_FromNodeIndex_eng(dset, node, N_Node);
   WorkErrLog_s();
   return(i);
}


/*!
   \brief Load a surface-based dataset from disk
   
   \param Name (char *) THe name of the file
   \param form (SUMA_DSET_FORMAT *) The format of the file
                                  can choose SUMA_NO_DSET_FORMAT
                                  and have the function attempt 
                                  to guess. In that case the function
                                  will set the value of form
   \return (SUMA_DSET *) dset 
   The datset does not get associated with a surface (owner_id[0] = '\0')
   You'll have to do this manually later on if you wish
   You typically want to insert that dataset into SUMA's DsetList list...
*/
SUMA_DSET *SUMA_LoadDset_s (char *Name, SUMA_DSET_FORMAT *form, int verb)
{  
   SUMA_DSET *dset = SUMA_LoadDset_eng (Name, form,  verb);
   WorkErrLog_s();
   return(dset);
}

/*!
   \brief writes a dataset to disk
   \param Name (char *) Name of output file. 
   \param dset (SUMA_DSET *) Le dataset
   \param form (SUMA_DSET_FORMAT ) Le format
   \return OutName (char *)The name used for the output file 
                           (you have to free that one yourself)
                           NULL if things went bad.
   - Be careful, this function will not change the idcode of the
   dataset being written. You'll have to do that manually.
*/
char * SUMA_WriteDset_s (char *Name, SUMA_DSET *dset, SUMA_DSET_FORMAT form, int overwrite, int verb) 
{
   char *c=SUMA_WriteDset_eng (Name, dset, form, overwrite, verb);
   WorkErrLog_s();
   return(c);
} 

/*!
   \brief Removes the standard extension from a dataset filename
   \param Name (char *) name 
   \param form SUMA_DSET_FORMAT
   \return (char *) no_extension (you have to free that one with SUMA_free)
*/
char *SUMA_RemoveDsetExtension_s (char*Name, SUMA_DSET_FORMAT form)
{
   char *c=SUMA_RemoveDsetExtension_eng (Name,  form);
   WorkErrLog_s();
   return(c);
}

/*!
   \brief A function to create a dataset out of MRI_FLOAT_PTR(im)
         that is typically used to read in a 1D file   
   \param FullName (char *) the filename
   \param dset_id (char *) if null, SUMA_CreateDsetPointer will create one
   \param dom_id (char *) domain idcode null if you have none
   \param farp (float **) pointer to float vector. If far = MRI_FLOAT_PTR(im);
                           then pass farp is &far . I want the pointer
                           so that I can set it to NULL if the pointer
                           is copied instead of the data (i.e. ptr_cpy ! = 0)
   \param vec_len (int) That would be im->nx
   \param vec_num (int) That would be im->ny
   \param ptr_cpy (int) 0 if you want to copy the values in *farp, 
                        1 if you want to make a pointer copy. In that case
                        (not supported yet, *farp is set to NULL)
   \return dset (SUMA_DSET *) NULL if trouble, of course. 
*/
SUMA_DSET *SUMA_far2dset_s( char *FullName, char *dset_id, char *dom_id, 
                                 float **farp, int vec_len, int vec_num, 
                                 int ptr_cpy) 
{
   SUMA_DSET *dset = SUMA_far2dset_eng( FullName, dset_id, dom_id, 
                                 farp, vec_len, vec_num, 
                                 ptr_cpy);
   WorkErrLog_s();
   return(dset);
}

/*!

   \brief Load a surface-based data set of the DX format
   \param Name (char *) name or prefix of dataset
   \param verb (int) level of verbosity. 0 mute, 1 normal, 2 dramatic perhaps
   \return dset (SUMA_DSET *)
   
*/
SUMA_DSET *SUMA_LoadDXDset_s (char *Name, int verb)
{
   SUMA_DSET *dset=SUMA_LoadDXDset_eng (Name,  verb);
   WorkErrLog_s();
   return(dset);
}

/*!

   \brief Load a surface-based data set of the 1D format
   \param Name (char *) name or prefix of dataset
   \param verb (int) level of verbosity. 0 mute, 1 normal, 2 dramatic perhaps
   \return dset (SUMA_DSET *)
   
*/
SUMA_DSET *SUMA_Load1DDset_s (char *oName, int verb)
{
   SUMA_DSET *dset=SUMA_Load1DDset_eng (oName,  verb);
   WorkErrLog_s();
   return(dset);
}

/*!
   \brief a convenience function to return a 1D file in a float array
   \param oName (char *) name of 1D file, can use '[]' if you like.
   \param ncol (int *) to hold the number of columns in the file.
   \param nrow (int *) to hold the number of rows in the file.
   \param RowMajor (int) 0 keep result in column major   xxxxxx yyyyyy zzzzzz
                        1 turn results to row major       xyz xyz xyz xyz 
   \return far(float *), the float array. Should be freed with free but SUMA_free 
   would work too. 
*/
float *SUMA_Load1D_s (char *oName, int *ncol, int *nrow, int RowMajor, int verb)
{
   float *far=SUMA_Load1D_eng (oName, ncol, nrow,  RowMajor,  verb);
   WorkErrLog_s();
   return(far);
}

