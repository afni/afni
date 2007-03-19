/* It should be possible to compile this file without
the SUMA_COMPILED being defined
Without SUMA_COMPILED,  SUMA_DataSets_NS.o can be linked 
from AFNI without trouble.
See SUMA_Makefile_NoDev
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


#include "suma_suma.h"


#if defined SUMA_COMPILED
   extern SUMA_CommonFields *SUMAg_CF;
   extern int SUMAg_N_DOv; 
   extern SUMA_DO *SUMAg_DOv;
#endif 

static int verv[] = { SUMA_VERSION_VECTOR }; 
float SUMA_LatestVersionNumber(void)
{
   return((float)verv[0]/10000.0);
}

#define NEL_DSET_TYPE(a) (NI_get_attribute((a), "dset_type"))
#define NEL_DATA_TYPE(a) (NI_get_attribute((a), "data_type"))

/* A bunch of functions to track errors from non-suma and suma programs */
static DList *list=NULL;

void SUMA_FreeErrLog ( void *data)
{
   SUMA_ERRLOG *el=NULL; 
   if (!data) return;
   el = (SUMA_ERRLOG *)data;
   SUMA_free(el);
   return;
}

void SUMA_PushErrLog(char *macroname, char *msg, char *fname)
{
   SUMA_ERRLOG *el=NULL;
   if (!list) {
      list = (DList *)SUMA_malloc(sizeof(DList));
      dlist_init(list, SUMA_FreeErrLog); 
   }
   el = (SUMA_ERRLOG*)SUMA_malloc(sizeof(SUMA_ERRLOG));
   snprintf(el->macroname, 39*sizeof(char),"%s", macroname);
   snprintf(el->msg, (MAX_ERRLOG_MSG-1)*sizeof(char), "%s", msg);
   snprintf(el->FuncName, (MAX_ERRLOG_FUNCNAME-1)*sizeof(char), "%s", fname);
   dlist_ins_next(list, dlist_tail(list), (void *)el);
   return;
}

DListElmt* SUMA_PopErrLog(DListElmt *eldone)
{
   static char FuncName[]={"SUMA_PopErrLog"};
   if (!list) return(NULL);
   if (!eldone) return(dlist_head(list));
   else {
      if (eldone == dlist_tail(list)) { /* all done, get out completely*/
         dlist_destroy(list);
         SUMA_free(list); list = NULL;
         return(NULL);
      } else {
         return(dlist_next(eldone));
      }
   }
   SUMA_S_Err("should not be here !");
   return(NULL);
}

/* A function to executes the proper macros for message generation. This one here is for non-suma programs */
void WorkErrLog_ns(void)
{
   static char FuncName[MAX_ERRLOG_FUNCNAME]={"WorkErrLog_ns"};
   int LocalHead = 0;
   DListElmt *del=NULL;
   SUMA_ERRLOG *el=NULL;
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
      else if (!strcmp(el->macroname,"SL_Note")) { SUMA_L_Note(el->msg); }
      else if (!strcmp(el->macroname,"SLP_Note")) { SUMA_SLP_Note(el->msg); }
      else if (!strcmp(el->macroname,"L_Crit")) { SUMA_L_Crit(el->msg); }
      else if (!strcmp(el->macroname,"SL_Crit")) { SUMA_SL_Crit(el->msg); }
      else if (!strcmp(el->macroname,"SLP_Crit")) { SUMA_SLP_Crit(el->msg); }
      else {
         sprintf(FuncName, "%s", "WorkErrLog_ns"); SUMA_S_Err("Bad macroname");
      }
      del = SUMA_PopErrLog(del);
   }
}
 
/*! ********** Begin Multiplexed Vectors Functions ************* */
/*!
   \brief A function to create a multiplexed vector structure
   \param tp (SUMA_VARTYPE) type of data in array. Only SUMA_double for now.
   \param N_dims (int) Number of array dimensions
   \param *dims (int) Size of each dimension
   \param first_dim_first (byte) 1 = first dimension first (column major, for a matrix) 
                                 only 1 is accepted at the moment
   \return mxv (SUMA_MX_VEC *) the structure with enough memory allocated to the pointer
                               of interest. (mxv->dv for SUMA_double).
   - Use SUMA_FreeMxVec to free mxv
   - see macros mxvd* to handle data in mxv
*/
SUMA_MX_VEC *SUMA_NewMxNullVec(SUMA_VARTYPE tp, int N_dims, int *dims, byte first_dim_first)
{
   static char FuncName[]={"SUMA_NewMxNullVec"};
   SUMA_MX_VEC *mxv=NULL;
   int n_vals=0, i = 0;
   
   SUMA_ENTRY;
   
   if (first_dim_first != 1) {
      SUMA_SL_Err("first_dim_first must be 1 for now");
      SUMA_RETURN(NULL);
   }
   
   if (N_dims < 1) {
      SUMA_SL_Err("N_dims < 1");
      SUMA_RETURN(NULL);
   } else if (N_dims > SUMA_MX_VEC_MAX_DIMS-1) {
      SUMA_SL_Err("N_dims > 49");
      SUMA_RETURN(NULL);
   }
   if (!dims) {
      SUMA_SL_Err("NULL dims");
      SUMA_RETURN(NULL);
   }
   mxv = (SUMA_MX_VEC *)SUMA_malloc(sizeof(SUMA_MX_VEC));
   mxv->fdf = 1;
   mxv->bv = NULL;
   mxv->sv = NULL;
   mxv->iv = NULL;
   mxv->fv = NULL;
   mxv->dv = NULL;
   mxv->cv = NULL;
   mxv->v = NULL;
   mxv->m = NULL;
   mxv->N_dims = N_dims;
   mxv->N_vals = dims[0]; 
   mxv->dims[0] = dims[0];
   for (i=1; i<N_dims; ++i) {
      mxv->N_vals = mxv->N_vals * dims[i]; 
      mxv->dims[i] = dims[i];
   }
   for (i=N_dims; i< SUMA_MX_VEC_MAX_DIMS; ++i) mxv->dims[i] = 1;
   if (mxv->N_vals <= 0) {
      SUMA_SL_Err("Negative dims");
      SUMA_free(mxv); SUMA_RETURN(NULL);
   }
   
   mxv->tp = tp;
   
   /* mutlipliers for first_dim_first */
   mxv->fdfm[0] = mxv->dims[0];
   for (i=1; i<N_dims-1; ++i) {
      mxv->fdfm[i] = mxv->fdfm[i-1]*mxv->dims[i];
   }
   
   SUMA_RETURN(mxv);
   
}

int SUMA_MxVecInit(SUMA_MX_VEC *mxv, void *val)
{
   static char FuncName[]={"SUMA_MxVecInit"};
   int i, ii;
   byte bb;
   short ss;
   float ff;
   complex cc;
   double dd;
   
   SUMA_ENTRY;
   
   if (!mxv->v) {
      SUMA_S_Err("null vector pointer");
      SUMA_RETURN(0);
   }
   switch (mxv->tp) {
      case SUMA_byte:
         bb = *((byte *)val);
         mxv->bv = (byte *)mxv->v; 
         for (i=0;i<mxv->N_vals;++i) mxv->bv[i] = bb;
         break;
      case SUMA_short:
         ss = *((short *)val);
         mxv->sv = (short *)mxv->v;         
         for (i=0;i<mxv->N_vals;++i) mxv->sv[i] = ss;
         break;
      case SUMA_int:
         ii = *((int *)val);
         mxv->iv = (int *)mxv->v;         
         for (i=0;i<mxv->N_vals;++i) mxv->iv[i] = ii;
         break;
      case SUMA_float:
         ff = *((float *)val);
         mxv->fv = (float *)mxv->v;         
         for (i=0;i<mxv->N_vals;++i) mxv->fv[i] = ff;
         break;
      case SUMA_double:
         dd = *((double *)val);
         mxv->dv = (double *)mxv->v;         
         for (i=0;i<mxv->N_vals;++i)  mxv->dv[i] = 1.0; 
         break;
      case SUMA_complex:
         cc = *((complex *)val);
         mxv->cv = (complex *)mxv->v;         
         for (i=0; i<mxv->N_vals; ++i) { mxv->cv[i].r = cc.r; mxv->cv[i].i = cc.i; } 
         break;
      default:
         SUMA_SL_Err("Bad type");
         SUMA_RETURN(0);
   }
   
   SUMA_RETURN(1); 
}
int SUMA_NewMxAllocVec(SUMA_MX_VEC *mxv) 
{
   static char FuncName[]={"SUMA_NewMxAllocVec"};
   int i, ip;
   
   SUMA_ENTRY;
   
   if (mxv->v || mxv->bv || mxv->sv || mxv->iv || mxv->fv || mxv->dv || mxv->cv || mxv->m) {
      SUMA_S_Err("Non null vector pointers");
      SUMA_RETURN(0);
   }
   
   switch (mxv->tp) {
      case SUMA_byte:
         mxv->v = SUMA_calloc(mxv->N_vals, sizeof(byte));
         mxv->bv = (byte *)mxv->v; 
         break;
      case SUMA_short:
         mxv->v = SUMA_calloc(mxv->N_vals, sizeof(short));
         mxv->sv = (short *)mxv->v;         
         break;
      case SUMA_int:
         mxv->v = SUMA_calloc(mxv->N_vals, sizeof(int));
         mxv->iv = (int *)mxv->v;         
         break;
      case SUMA_float:
         mxv->v = SUMA_calloc(mxv->N_vals, sizeof(float));
         mxv->fv = (float *)mxv->v;         
         break;
      case SUMA_double:
         mxv->v = SUMA_calloc(mxv->N_vals, sizeof(double));
         mxv->dv = (double *)mxv->v;         
         break;
      case SUMA_complex:
         mxv->v = SUMA_calloc(mxv->N_vals, sizeof(complex));
         mxv->cv = (complex *)mxv->v; 
         if (mxv->v) {
            for (i=0; i<mxv->N_vals; ++i) { 
               mxv->cv[i].r = 0.0; mxv->cv[i].i = 0.0; 
            }
         }
         break;
      default:
         SUMA_SL_Err("Bad type");
         SUMA_RETURN(0);
   }
   
   if (!mxv->v) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(0);
   }
   mxv->m = NULL; /* just to be sure */
   SUMA_RETURN(1);
   
#if 0 /* a block that used to be under SUMA_complex case. The cause for the crash was a memory block, returned by  SUMA_calloc
that was not contiguous! The request was for 13gB...*/
{
complex * cp;
         mxv->v = SUMA_calloc(mxv->N_vals, sizeof(complex));
         mxv->cv = (complex *)mxv->v; 
 cp = (complex *)mxv->v; 

         if (mxv->v) {
            fprintf(SUMA_STDERR,"About to reset %d values\n", mxv->N_vals);
            ip = -1;
            for (i=0; i<mxv->N_vals; ++i) { 
               if (i-ip != 1) {
                  fprintf(SUMA_STDERR,"Difference of %d at %d!\n", i-ip, i);
               } else {
                  ip = i;
               }
               /* fprintf(SUMA_STDERR,"%d ooo ", i); 
               mxv->cv[i].r = 0.0; mxv->cv[i].i = 0.0;  */
if( &mxv->cv[i] - mxv->cv != i )
{
fprintf(SUMA_STDERR,"pointer Difference %d = (%p,%p) at %d!\n",
	&mxv->cv[i] - mxv->cv,
	&mxv->cv[i], mxv->cv,i);
fprintf(SUMA_STDERR, "sizeof(complex *) = %lu\n", sizeof(complex *));
}
mxv->cv[i].r = 0.0; mxv->cv[i].i = 0.0;
cp++;
            } 
            SUMA_S_Note("There");
         }
         break;
}
#endif

}

SUMA_MX_VEC *SUMA_NewMxVec(SUMA_VARTYPE tp, int N_dims, int *dims, byte first_dim_first)
{
   static char FuncName[]={"SUMA_NewMxVec"};
   SUMA_MX_VEC *mxv=NULL;
   int n_vals=0, i = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* blank holder */
   mxv = SUMA_NewMxNullVec(tp, N_dims,  dims,  first_dim_first);
   if (LocalHead) SUMA_ShowMxVec(mxv, 1, NULL, "\nmxv in NewMx\n");
   /* allocator */
   if (!SUMA_NewMxAllocVec(mxv)) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_free(mxv); SUMA_RETURN(NULL);
   }
   
   SUMA_RETURN(mxv);
} 

SUMA_MX_VEC *SUMA_VecToMxVec(SUMA_VARTYPE tp, int N_dims, int *dims, byte first_dim_first, void *vec)
{
   static char FuncName[]={"SUMA_VecToMxVec"};
   SUMA_MX_VEC *mxv=NULL;
   int n_vals=0, i = 0;
   
   SUMA_ENTRY;
   
   /* blank holder */
   mxv = SUMA_NewMxNullVec(tp, N_dims,  dims,  first_dim_first);
   
   if (!vec) SUMA_RETURN(mxv);
   
   mxv->v = vec;
   switch (mxv->tp) {
      case SUMA_byte:
         mxv->bv = (byte *)mxv->v; 
         break;
      case SUMA_short:
         mxv->sv = (short *)mxv->v;         
         break;
      case SUMA_int:
         mxv->iv = (int *)mxv->v;         
         break;
      case SUMA_float:
         mxv->fv = (float *)mxv->v;         
         break;
      case SUMA_double:
         mxv->dv = (double *)mxv->v;         
         break;
      case SUMA_complex:
         mxv->cv = (complex *)mxv->v;         
         break;
      default:
         SUMA_SL_Err("Bad type");
         SUMA_free(mxv); SUMA_RETURN(NULL);
   }
      
   SUMA_RETURN(mxv);
} 


SUMA_MX_VEC *SUMA_FreeMxVec(SUMA_MX_VEC *mxv)
{
   static char FuncName[]={"SUMA_FreeMxVec"};
   int i;
   SUMA_ENTRY;
   
   if (mxv) {
      if (mxv->v) SUMA_free(mxv->v);
      if (mxv->m) {
         if (mxv->m->elts) {
            for (i=0; i<mxv->m->rows; ++i) if (mxv->m->elts[i]) SUMA_free(mxv->m->elts[i]);
            SUMA_free(mxv->m->elts);
         }
         SUMA_free(mxv->m); 
      }
      mxv->m = NULL;
      SUMA_free(mxv);
   }
   
   SUMA_RETURN(NULL);
}

void SUMA_ShowMxVec (SUMA_MX_VEC *mxv, int detail, FILE *out, char *title)
{
   static char FuncName[]={"SUMA_ShowMxVec"};
   char *si = NULL;
   
   SUMA_ENTRY;
   
   if (!out) out = SUMA_STDERR;
   
   si = SUMA_MxVec_Info(mxv, detail, title);
   
   fprintf(out,"%s\n", si);
   
   if (si) SUMA_free(si); si = NULL;
   
   SUMA_RETURNe;
   
}
/*!
   \brief Function to return info on SUMA_DSET
   
   - You must free the returned string on your own
   \sa SUMA_ShowDset
*/
char *SUMA_MxVec_Info (SUMA_MX_VEC *mxv, int detail, char *title)
{
   static char FuncName[]={"SUMA_MxVec_Info"};
   int i, imx = 5, j;
   SUMA_COL_TYPE ctp;
   char *s=NULL, stmp[200];
   SUMA_STRING *SS=NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (mxv) {
      SUMA_LH("Have mxv");
      if (title) SS = SUMA_StringAppend_va(SS, "%s", title); 
      SS = SUMA_StringAppend_va(SS, "mxv: %p\n"
                                    "data type: %d (%s)\n"
                                    "fdf: %d\n"
                                    "N_dims: %d\n"
                                    "N_vals: %d\n"
                                    , mxv, mxv->tp, SUMA_VarType2CTypeName(mxv->tp), mxv->fdf, mxv->N_dims, mxv->N_vals);
      if (mxv->m) {
         SUMA_LH("Working m");
         SS = SUMA_StringAppend_va(SS, "m is setup (rows: %d, cols: %d)\n", mxv->m->rows, mxv->m->cols);
         SUMA_LH("Working m");
         for (i=0; i < mxv->m->rows && i < imx; ++i) {
            for (j=0; j < mxv->m->cols && j < imx; ++j) {
               SUMA_LH("elts[][]\n");
               SS = SUMA_StringAppend_va(SS,"%g   ", mxv->m->elts[i][j]);
            }
            if (mxv->m->cols > imx) SS = SUMA_StringAppend(SS,"...\n");
            else SS = SUMA_StringAppend(SS,"\n");
         }
         if (mxv->m->rows > imx) SS = SUMA_StringAppend(SS,"...  ...   ...   ...   ...\n");
         else SS = SUMA_StringAppend(SS,"\n");
         SUMA_LH("Done with m");
      } else {
         SUMA_LH("NULL m");
         SS = SUMA_StringAppend(SS, "m is NULL\n");
      }
      SUMA_LH("dims time");
      SS = SUMA_StringAppend_va(SS, "dims: ");
      for (i=0; i<mxv->N_dims; ++i) {
         SS = SUMA_StringAppend_va(SS, "%d ", mxv->dims[i]);
      }
      SS = SUMA_StringAppend_va(SS, "\n");
      
      if (mxv->v) {
         if (detail < 0) {
            imx = mxv->N_vals;
         } else {
            imx = 5*detail;
         }
         s = SUMA_ShowMeSome( mxv->v, 
                              mxv->tp,
                              mxv->N_vals, imx, NULL);
         SS = SUMA_StringAppend_va(SS, "         %s\n", s); SUMA_free(s); s = NULL;
      } else SS = SUMA_StringAppend_va(SS, "         NULL\n");
   } else {
      SS = SUMA_StringAppend(SS, "NULL mxv.");
   }
   
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(s);
}

/*!
   Creates a NI group to store surface data 
   N_el is the number of data elements stored in each column
   N_el can be the number of nodes for example. 
*/
SUMA_Boolean SUMA_NewDsetGrp (SUMA_DSET *dset, SUMA_DSET_TYPE dtp, 
                           char* MeshParent_idcode, 
                          char * geometry_parent_idcode, int N_el, 
                          char *filename, char *thisidcode)
{
   static char FuncName[]={"SUMA_NewDsetGrp"};
   char idcode[SUMA_IDCODE_LENGTH], *namecode, *dname;
   
   SUMA_ENTRY;
   
   if (!dset) { SUMA_SL_Err("NULL dset"); SUMA_RETURN(NOPE); }
   if (dset->N_links != 0) { SUMA_SL_Err("Not expected here, N_links != 0"); SUMA_RETURN(NOPE); }
   
   dset->ngr = NI_new_group_element();
   if (0) {
      NI_rename_group(dset->ngr, SUMA_Dset_Type_Name(dtp));
   } else { /* trying to go the AFNI route */
      NI_rename_group(dset->ngr, "AFNI_dataset");
      NI_set_attribute (dset->ngr, "dset_type", SUMA_Dset_Type_Name(dtp));
   }
   
   /* assign an idcode */
   if (!thisidcode) {
      if (!filename) {
         UNIQ_idcode_fill(idcode);
         NI_set_attribute (dset->ngr, "self_idcode", idcode); /* create one *//* changed from idcode March 31 */
      } else { 
         namecode = UNIQ_hashcode(filename);  /* from filename */
         NI_set_attribute (dset->ngr, "self_idcode", namecode); SUMA_free(namecode);
      }
   } else {
      NI_set_attribute (dset->ngr, "self_idcode", thisidcode);
   }

   /* set the idcodes of the parents */
   if (MeshParent_idcode) {
      NI_set_attribute (dset->ngr, "domain_parent_idcode", MeshParent_idcode); 
   } else {
      NI_set_attribute (dset->ngr, "domain_parent_idcode", NULL); /* don't use SUMA_EMPTY_ATTR unless you must NULL is nice*/
   }
   if (geometry_parent_idcode) {
      NI_set_attribute (dset->ngr, "geometry_parent_idcode", geometry_parent_idcode);
   } else {
      NI_set_attribute (dset->ngr, "geometry_parent_idcode", NULL);
   }
  
   if (filename) NI_set_attribute (dset->ngr, "filename", filename);
   
   /* Now add the data element */
   dname = SUMA_append_string(SUMA_Dset_Type_Name(dtp), "_data");
   if (0) {
      dset->dnel = NI_new_data_element(dname, N_el); 
   } else { /* trying to go the AFNI route */
      dset->dnel = NI_new_data_element("SPARSE_DATA", N_el); 
      NI_set_attribute (dset->dnel, "data_type", dname); 
   }
   
   /* SUMA_ShowNel((void*)dset->dnel); */
   
   SUMA_free(dname); dname = NULL;
   NI_add_to_group(dset->ngr, dset->dnel);
   
   /* Now add the node index element */
   dname = SUMA_append_string(SUMA_Dset_Type_Name(dtp), "_node_indices");
   dset->inel = NI_new_data_element("INDEX_LIST", N_el); 
   NI_set_attribute (dset->inel, "data_type", dname); 
   SUMA_free(dname); dname = NULL;
   NI_add_to_group(dset->ngr, dset->inel);
   
   /* SUMA_ShowNel((void*)dset->inel); */
   SUMA_RETURN(YUP);  
}
                          
static byte SUMA_ALLOW_NEL_USE;
void SUMA_allow_nel_use(int al)
{
   if (al) SUMA_ALLOW_NEL_USE = 1;
   else SUMA_ALLOW_NEL_USE = 0;
   
   return;
} 

/*!
   Creates a NI elem. to store surface data 
   N_el is the number of data elements stored in each column
   N_el can be the number of nodes for example
   Do not use this function to create dsets anymore, use
   SUMA_NewDsetGrp
*/
NI_element * SUMA_NewNel (SUMA_DSET_TYPE dtp, char* MeshParent_idcode, 
                          char * geometry_parent_idcode, int N_el, 
                          char *filename, char *thisidcode)
{
   static char FuncName[]={"SUMA_NewNel"};
   NI_element *nel=NULL;
   char idcode[SUMA_IDCODE_LENGTH], *namecode;
   
   SUMA_ENTRY;

   if (!SUMA_ALLOW_NEL_USE) SUMA_SL_Warn("Obsolete, perhaps. Check on caller.");
   
   nel = NI_new_data_element(SUMA_Dset_Type_Name(dtp), N_el);
   
   /* assign an idcode */
   if (!thisidcode) {
      if (!filename) {
         UNIQ_idcode_fill(idcode);
         NI_set_attribute (nel, "self_idcode", idcode); /* create one *//* changed from idcode March 31 */
      } else { 
         namecode = UNIQ_hashcode(filename);  /* from filename */
         NI_set_attribute (nel, "self_idcode", namecode); SUMA_free(namecode);
      }
   } else {
      NI_set_attribute (nel, "self_idcode", thisidcode);
   }
   
   
   /* set the idcodes of the parents */
   if (MeshParent_idcode) {
      NI_set_attribute (nel, "domain_parent_idcode", MeshParent_idcode); /* changed from MeshParent_idcode March 31 */
   } else {
      NI_set_attribute (nel, "domain_parent_idcode", SUMA_EMPTY_ATTR);
   }
   if (geometry_parent_idcode) {
      NI_set_attribute (nel, "geometry_parent_idcode", geometry_parent_idcode);
   } else {
      NI_set_attribute (nel, "geometry_parent_idcode", SUMA_EMPTY_ATTR);
   }
  
   if (filename) NI_set_attribute (nel, "filename", filename);
   
   SUMA_allow_nel_use(0); 
   SUMA_RETURN(nel);  
}

/*!
   \brief Returns A COPY of the label of a column in a NI_element
   NULL in case of error 
   YOU SHOULD FREE THIS POINTER when you're done with it
*/
char *SUMA_DsetColLabelCopy(SUMA_DSET *dset, int i, int addcolnum)
{
   static char FuncName[]={"SUMA_DsetColLabelCopy"};
   char Name[500], *lbl = NULL, **sc=NULL, *s=NULL;
   NI_element *nelb=NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->dnel) { SUMA_RETURN(NULL); }
   if (i < 0 || i >= SDSET_VECNUM(dset)) { SUMA_RETURN(NULL); }
   
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_LABS");
   SUMA_NEL_GET_STRING(nelb, 0, 0, lbl); /* sc is a pointer copy here, do not free */
   lbl = SUMA_Get_Sub_String(lbl, SUMA_NI_CSS, i);
   sprintf(Name, "%d: ", i);
   if (lbl) { 
      if (addcolnum) s = SUMA_append_string(Name, lbl); 
      else s = SUMA_copy_string(lbl); 
      SUMA_free(lbl); lbl = NULL;
      SUMA_RETURN(s);
   }
   
   /* no label, try the name of the nel */
   lbl = NI_get_attribute(dset->ngr, "label");
   if (lbl) {
      if (addcolnum) SUMA_RETURN(SUMA_append_string(Name, lbl));
      else SUMA_RETURN(SUMA_copy_string(lbl));
   }
   lbl = NI_get_attribute(dset->ngr, "filename");
   if (lbl) {
      if (addcolnum) SUMA_RETURN(SUMA_append_string(Name, lbl));
      else SUMA_RETURN(SUMA_copy_string(lbl));
   }
   
   if (NEL_DSET_TYPE(dset->ngr)) {
      if (addcolnum) SUMA_RETURN(SUMA_append_string(Name, NEL_DSET_TYPE(dset->ngr))); 
      else SUMA_RETURN(SUMA_copy_string(NEL_DSET_TYPE(dset->ngr)));
   }
   
   /* give me a bone */
   if (addcolnum) SUMA_RETURN(SUMA_append_string(Name, "bone"));
   else  SUMA_RETURN(SUMA_copy_string("bone"));
}
/*!
   \brief Returns A COPY of the label of a column in a NI_element
   NULL in case of error 
   YOU SHOULD FREE THIS POINTER when you're done with it
   
*/
char *SUMA_ColLabelCopy(NI_element *nel, int i, int addcolnum)
{
   static char FuncName[]={"SUMA_ColLabelCopy"};
   char Name[500], *lbl;
   
   SUMA_ENTRY;
   
   /* SUMA_SL_Warn("Obsolete, use new version."); it is still used for converting old types to new one */

   if (i < 0) { SUMA_RETURN(NULL); }
   if (!nel) { SUMA_RETURN(NULL); }
   
   sprintf(Name, "LabelCol_%d", i);
   lbl = NI_get_attribute(nel, Name);
   sprintf(Name, "%d: ", i);
   if (lbl) {
      if (addcolnum) SUMA_RETURN(SUMA_append_string(Name, lbl));
      else SUMA_RETURN(SUMA_copy_string(lbl));
   }
   /* no label, try the name of the nel */
   lbl = NI_get_attribute(nel, "label");
   if (lbl) {
      if (addcolnum) SUMA_RETURN(SUMA_append_string(Name, lbl));
      else SUMA_RETURN(SUMA_copy_string(lbl));
   }
   lbl = NI_get_attribute(nel, "filename");
   if (lbl) {
      if (addcolnum) SUMA_RETURN(SUMA_append_string(Name, lbl));
      else SUMA_RETURN(SUMA_copy_string(lbl));
   }
   
   if (nel->name) {
      if (addcolnum) SUMA_RETURN(SUMA_append_string(Name, nel->name)); 
      else SUMA_RETURN(SUMA_copy_string(nel->name));
   }
   
   /* give me a bone */
   if (addcolnum) SUMA_RETURN(SUMA_append_string(Name, "bone"));
   else  SUMA_RETURN(SUMA_copy_string("bone"));
}

NI_element *SUMA_FindDsetDataElement(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_FindDsetDataElement"};
   char *attname=NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr) { SUMA_SL_Err("NUll input "); SUMA_RETURN(NULL); }
   attname = SUMA_append_string(NEL_DSET_TYPE(dset->ngr),"_data");
   
   SUMA_RETURN(SUMA_FindNgrDataElement(dset->ngr, "SPARSE_DATA", attname));
}

NI_element *SUMA_FindDsetNodeIndexElement(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_FindDsetNodeIndexElement"};
   char *attname=NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr) { SUMA_SL_Err("NUll input "); SUMA_RETURN(NULL); }
   attname = SUMA_append_string(NEL_DSET_TYPE(dset->ngr),"_node_indices");

   SUMA_RETURN(SUMA_FindNgrDataElement(dset->ngr, "INDEX_LIST", attname));
}


NI_element *SUMA_FindNgrDataElement(NI_group *ngr, char *nelname, char *typename)
{
   static char FuncName[]={"SUMA_FindNgrDataElement"};
   NI_element *nel = NULL;
   char *rs=NULL;
   static int nwarn = 0;
   int ip;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!ngr || !typename || !nelname) { SUMA_SL_Err("NUll input "); SUMA_RETURN(nel); }
   
   /* search for an element of type nelname that is also of data_type typename */
   for( ip=0 ; ip < ngr->part_num ; ip++ ){ 
      switch( ngr->part_typ[ip] ){
         /*-- a sub-group ==> recursion! --*/
         case NI_GROUP_TYPE:
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)ngr->part[ip] ;
            if (LocalHead)  {
               fprintf(SUMA_STDERR,"%s:  Looking for %s   name=%s vec_len=%d vec_filled=%d, vec_num=%d\n", FuncName,\
                        typename, nel->name, nel->vec_len, nel->vec_filled, nel->vec_num );
            }
            if (!strcmp(nelname, nel->name)) {/* now in keeping with AFNI */
               rs = NI_get_attribute(nel, "data_type");
               if (rs) { 
                  if (!strcmp(typename, rs)) SUMA_RETURN(nel);   
               } else {
                  if (!(nwarn % 25)) {
                     fprintf(SUMA_STDERR, "Notice %s:\n"
                                          "NIML Dset's index column named %s \n"
                                          "is without any data_type attribute.\n"
                                          "Could not verify that element's \n"
                                          "   data_type = %s .\n"
                                          "Ignore notice for dsets created by AFNI.\n"
                                          "Notice is shown intermittently in \n"
                                          "SUMA GUI mode.\n", FuncName, nel->name, typename);
                     nwarn = 0;
                  }
                  ++nwarn;
                  SUMA_RETURN(nel); 
               }
            }
            /* cancel plans if you get here */
            nel = NULL;
            break;
         default:
            SUMA_SL_Err("Don't know what to make of this group element, ignoring.");
            break;
      }
   }

   SUMA_RETURN(nel);

}

NI_element *SUMA_FindDsetAttributeElement(SUMA_DSET *dset, char *attname)
{
   static char FuncName[]={"SUMA_FindDsetAttributeElement"};
   int ip;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !attname) { SUMA_SL_Err("NUll input "); SUMA_RETURN(NULL); }
   if (!dset->ngr) { SUMA_SL_Err("NUll input "); SUMA_RETURN(NULL); }


   SUMA_RETURN(SUMA_FindNgrAttributeElement(dset->ngr, attname));
}

NI_element *SUMA_FindNgrAttributeElement(NI_group *ngr, char *attname)
{
   static char FuncName[]={"SUMA_FindNgrAttributeElement"};
   NI_element *nel = NULL;
   char *rs=NULL;
   int ip;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ngr || !attname) { SUMA_SL_Err("NUll input "); SUMA_RETURN(nel); }
  /* now read the elements in this group */
   for( ip=0 ; ip < ngr->part_num ; ip++ ){ 
      switch( ngr->part_typ[ip] ){
         /*-- a sub-group ==> recursion! --*/
         case NI_GROUP_TYPE:
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)ngr->part[ip] ;
            if (LocalHead)  {
               fprintf(SUMA_STDERR,"%s:  Looking for %s   name=%s vec_len=%d vec_filled=%d, vec_num=%d\n", FuncName,\
                        attname, nel->name, nel->vec_len, nel->vec_filled, nel->vec_num );
            }
            if (!strcmp("AFNI_atr", nel->name)) {/* now in keeping with AFNI */
               rs = NI_get_attribute(nel, "atr_name");
               if (rs) { 
                  if (!strcmp(attname, rs)) SUMA_RETURN(nel);   
               }
            }
            /* cancel plans if you get here */
            nel = NULL;
            break;
         default:
            SUMA_SL_Err("Don't know what to make of this group element, ignoring.");
            break;
      }
   }


   SUMA_RETURN(nel);
}

NI_element *SUMA_FindNgrNamedElement(NI_group *ngr, char *elname)
{
   static char FuncName[]={"SUMA_FindNgrNamedElement"};
   NI_element *nel = NULL;
   char *rs=NULL;
   int ip;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ngr || !elname) { SUMA_SL_Err("NUll input "); SUMA_RETURN(nel); }
  /* now read the elements in this group */
   for( ip=0 ; ip < ngr->part_num ; ip++ ){ 
      switch( ngr->part_typ[ip] ){
         /*-- a sub-group ==> recursion! --*/
         case NI_GROUP_TYPE:
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)ngr->part[ip] ;
            if (LocalHead)  {
               fprintf(SUMA_STDERR,"%s:  Looking for %s   name=%s vec_len=%d vec_filled=%d, vec_num=%d\n", FuncName,\
                        elname, nel->name, nel->vec_len, nel->vec_filled, nel->vec_num );
            }
            if (!strcmp(elname, nel->name)) SUMA_RETURN(nel);   
            /* cancel plans if you get here */
            nel = NULL;
            break;
         default:
            SUMA_SL_Err("Don't know what to make of this group element, ignoring.");
            break;
      }
   }


   SUMA_RETURN(nel);
}



/*!
   Add an attribute element to a data set, to be called after adding a column to the data element
   The old version of this is SUMA_AddColAttr
   \sa SUMA_AddDsetNodeIndexColAttr for special SUMA_NODE_INDEX ctp
*/
int SUMA_AddDsetColAttr (SUMA_DSET *dset, char *col_label, SUMA_COL_TYPE ctp, void *col_attr, int col_index)
{
   static char FuncName[]={"SUMA_AddDsetColAttr"};
   NI_element *nelb = NULL;
   char Name[500], Attr[500], *attrstr=NULL;
   
   SUMA_ENTRY;
   
   if (ctp == SUMA_NODE_INDEX) {
      SUMA_RETURN(SUMA_AddDsetNodeIndexColAttr (dset, col_label, ctp, col_attr));
   }
   
   if (!dset) SUMA_RETURN(0);
   if (col_index < 0) col_index = SDSET_VECNUM(dset)-1;
   if (col_index < 0 || !SDSET_VECNUM(dset) ) { SUMA_SL_Err("No columns in data set's data element!"); SUMA_RETURN(0); }
   if (SDSET_VECNUM(dset) <= col_index) { SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   
   /* has the column label element been added yet ?*/
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_LABS");
   if (!nelb) { /* need to form this element */
      nelb = NI_new_data_element("AFNI_atr", SDSET_VECNUM(dset));
      NI_set_attribute(nelb,"atr_name", "COLMS_LABS");
      NI_add_column_stride ( nelb, NI_STRING, NULL, 1 );
      NI_add_to_group(dset->ngr, nelb);
   } 
   SUMA_AddColAtt_CompString(nelb, col_index, col_label, SUMA_NI_CSS);

   /* has the column type element been added yet ? */
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_TYPE");
   if (!nelb) { /* need to form this element */
      nelb = NI_new_data_element("AFNI_atr", SDSET_VECNUM(dset));
      NI_set_attribute(nelb,"atr_name", "COLMS_TYPE");
      NI_add_column_stride ( nelb, NI_STRING, NULL, 1 );
      NI_add_to_group(dset->ngr, nelb);
   } 
   /* set the type */
   SUMA_AddColAtt_CompString(nelb, col_index, SUMA_Col_Type_Name(ctp), SUMA_NI_CSS);
   
   /* set the Col_STATSYM (SPECIAL CASE: USE THE ; rather than the ~ . The story is complicated) */
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_STATSYM");
   if (!nelb) { /* need to form this element */
      nelb = NI_new_data_element("AFNI_atr", SDSET_VECNUM(dset));
      NI_set_attribute(nelb,"atr_name", "COLMS_STATSYM");
      NI_add_column_stride ( nelb, NI_STRING, NULL, 1 );
      NI_add_to_group(dset->ngr, nelb);
   } 
   /* set the attribute string */   
   switch (ctp) {
      
      default:
         attrstr = SUMA_copy_string("none");
         break;  
      
      case SUMA_NODE_XCORR:
         if (col_attr){  
            float *pars = (float *)col_attr;
            SUMA_S_Note(NI_stat_encode(NI_STAT_CORREL, pars[0], pars[1], pars[2]));
            attrstr = SUMA_copy_string( NI_stat_encode(NI_STAT_CORREL, pars[0], pars[1], pars[2]));
         }
         break;  

   }
   
   SUMA_AddColAtt_CompString(nelb, col_index, attrstr, SUMA_NI_CSS); /* See confusing note following AFNI_NI_CSS in SUMA_DataSets.h */
   
   if (attrstr) SUMA_free(attrstr); attrstr = NULL;

   
   SUMA_RETURN(1);   
}
/*!
   \brief a special version of SUMA_AddDsetColAttr for node index column
*/
int SUMA_AddDsetNodeIndexColAttr (SUMA_DSET *dset, char *col_label, SUMA_COL_TYPE ctp, void *col_attr)
{
   static char FuncName[]={"SUMA_AddDsetNodeIndexColAttr"};
   NI_element *nelb = NULL;
   char Name[500], Attr[500], *attrstr=NULL;
   
   SUMA_ENTRY;
   
   if (ctp != SUMA_NODE_INDEX) {
      SUMA_S_Err("Don't call me like that");
      SUMA_RETURN(0); 
   }
   
   if (!dset || !dset->inel || !SDSET_NODEINDLEN(dset)) SUMA_RETURN(0);
   
   NI_set_attribute(dset->inel, "COLMS_LABS", col_label);
   NI_set_attribute(dset->inel, "COLMS_TYPE", SUMA_Col_Type_Name(ctp));
      
   SUMA_RETURN(1);   
}

/*!
   
   Adds an attribute to nel for that explains the last added column's 
   contents. You should call this function after each SUMA_AddNelCol call 
   col_attr (void *) is a pointer to a structure containing special
   attributes of the data in the last column added. At the moment,
   this pointer is not being used, but you can imagine needing it if
   you have to store certain stats or parameters that go with each column.
       
*/
int SUMA_AddColAttr (NI_element *nel, char *col_label, SUMA_COL_TYPE ctp, void *col_attr, int col_index)
{
   static char FuncName[]={"SUMA_AddColAttr"};
   char Name[500], Attr[500];
   
   SUMA_ENTRY;
   
   if (!SUMA_ALLOW_NEL_USE) SUMA_SL_Warn("Obsolete, use new version.");

   if (!nel) SUMA_RETURN(0);
   if (col_index < 0) col_index = nel->vec_num-1;
   if (col_index < 0 || !nel->vec_num ) { SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); }
   if (nel->vec_num <= col_index) { SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   
   /* if a label is specified, set it */
   if (col_label) {
      sprintf(Name, "LabelCol_%d", col_index);
      NI_set_attribute ( nel, Name, col_label);
   }
   /* save the type of the column */
   sprintf(Name, "TypeCol_%d", col_index);
   NI_set_attribute ( nel, Name, SUMA_Col_Type_Name(ctp));   
   
   sprintf(Attr, "AttrCol_%d", col_index);
   switch (ctp) {
      case SUMA_NODE_BYTE:
         NI_set_attribute ( nel, Attr, NULL);
         break;
         
      case SUMA_NODE_DOUBLE:
         NI_set_attribute ( nel, Attr, NULL);
         break;
         
      case SUMA_NODE_INDEX:
         /* form the string of attributes for this column */
         NI_set_attribute ( nel, Attr, NULL);
         break;
     
      case SUMA_NODE_INT:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_SHORT:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_ILABEL:
         NI_set_attribute ( nel, Attr, NULL);
         break;   
      
      case SUMA_NODE_X:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_Y:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_Z:
         NI_set_attribute ( nel, Attr, NULL);
         break; 
         
      case SUMA_NODE_R:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_G:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_B:
         NI_set_attribute ( nel, Attr, NULL);
         break;    
       
      case SUMA_NODE_A:
         NI_set_attribute ( nel, Attr, NULL);
         break;
         
      case SUMA_NODE_Rb:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_Gb:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_Bb:
         NI_set_attribute ( nel, Attr, NULL);
         break;    
       
      case SUMA_NODE_Ab:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_FLOAT:
         NI_set_attribute ( nel, Attr, NULL);
         break;     
      
      case SUMA_NODE_3C:
         NI_set_attribute ( nel, Attr, NULL);
         break;  
         
      case SUMA_NODE_STRING:
         NI_set_attribute ( nel, Attr, NULL);
         break;     
      
      case SUMA_NODE_CX:
         NI_set_attribute ( nel, Attr, NULL);
         break;  
      
      default:
         NI_set_attribute ( nel, Attr, NULL);
         break;          
   }
   
   SUMA_allow_nel_use(0);

   SUMA_RETURN(1);   
}

/*!
   Adds some generic attributes.
   For the moment, the range is added for numeric columns 
   if col_index is -1, then it is assumed that the attributes are for the latest column added (vec_num -1)
   \sa SUMA_AddGenDsetNodeIndexColAttr for the special case of the node index column
*/
int SUMA_AddGenDsetColAttr (SUMA_DSET *dset, SUMA_COL_TYPE ctp, void *col, int stride, int col_index) 
{
   static char FuncName[]={"SUMA_AddGenDsetColAttr"};
   char Name[500], **junk, *stmp, *curstring = NULL;
   float amin = 0.0, amax = 0.0, *fv;
   int aminloc = -1, amaxloc = -1, *iv;
   byte *bv;
   NI_element *nelb = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (ctp == SUMA_NODE_INDEX) {
      SUMA_RETURN(SUMA_AddGenDsetNodeIndexColAttr (dset, ctp, col, stride) );
   }
   
   if (!dset || !dset->dnel) { SUMA_SL_Err("Null input"); SUMA_RETURN(0); } 
   if (col_index < 0) col_index = SDSET_VECNUM(dset)-1;
   if (col_index < 0 || !SDSET_VECNUM(dset) ) { SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); }
   if (SDSET_VECNUM(dset) <= col_index) { SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   
   /* does the range attribute element exist ? */
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_RANGE");
   if (!nelb) { /* need to form this element */
      SUMA_LH("Need to create ranges element");
      nelb = NI_new_data_element("AFNI_atr", 1); /* one long string */
      NI_set_attribute(nelb, "atr_name", "COLMS_RANGE");
      NI_add_to_group(dset->ngr, nelb);
      #if 0 /* trying to work with NI_insert */
      { int i ; junk = (char **)SUMA_malloc(sizeof(char*)*300); 
         for (i=0; i< 300; ++i) junk[i] = (char *)SUMA_malloc(300 * sizeof(char));
         for (i=0; i<50; ++i) sprintf(junk[i], "Hello Baby Joannne ro"); }
      NI_add_column_stride ( nelb, NI_STRING, junk, 1 );
      #else
         NI_add_column_stride ( nelb, NI_STRING, NULL, 1 );
      #endif
      if (LocalHead) SUMA_ShowNel((void*)nelb);
      curstring = NULL;
   } else {
      SUMA_LH("Ranges element found, getting previous string");
      SUMA_NEL_GET_STRING(nelb, 0, 0, curstring);
   }
   if (!col) { 
      SUMA_LH("No data");
      /* Do not complain, that is not a bad thing.
      People can use this to allocate for a column
      without filling it up */
      stmp = SUMA_copy_string("0 0 -1 -1");
   } else { 
      SUMA_LH("Calculating range");
      switch (SUMA_ColType2TypeCast(ctp)) {
         case SUMA_int:
            iv = (int *)col;
            SUMA_MIN_MAX_VEC_STRIDE(iv ,SDSET_VECFILLED(dset), amin, amax, aminloc, amaxloc, stride);
            snprintf(Name, 500*sizeof(char),"%d %d %d %d", (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_float:
            fv = (float *)col;
            SUMA_MIN_MAX_VEC_STRIDE(fv ,SDSET_VECFILLED(dset), amin, amax, aminloc, amaxloc, stride);
            snprintf(Name, 500*sizeof(char),"%f %f %d %d", amin, amax, aminloc, amaxloc);
            break;
         case SUMA_byte:
            bv = (byte *)col;
            SUMA_MIN_MAX_VEC_STRIDE(bv ,SDSET_VECFILLED(dset), amin, amax, aminloc, amaxloc, stride);
            snprintf(Name, 500*sizeof(char),"%d %d %d %d", (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_string:
            Name[0] = '\0';
            break;
         default:
            fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
            SUMA_RETURN(0);
            break; 
      }
      stmp = SUMA_copy_string(Name);
   }
   
   
   SUMA_AddColAtt_CompString(nelb, col_index, stmp, SUMA_NI_CSS);
   if (LocalHead) SUMA_ShowNel((void*)nelb);
   SUMA_free(stmp); stmp = NULL;
   SUMA_RETURN(1);  
}
/*!
   \brief A special case of  SUMA_AddGenDsetColAttr for node indices
*/
int SUMA_AddGenDsetNodeIndexColAttr (SUMA_DSET *dset, SUMA_COL_TYPE ctp, void *col, int stride) 
{
   static char FuncName[]={"SUMA_AddGenDsetNodeIndexColAttr"};
   char Name[500], **junk, *stmp=NULL;
   float amin = 0.0, amax = 0.0, *fv;
   int aminloc = -1, amaxloc = -1, *iv;
   byte *bv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!dset || !dset->inel || SDSET_NODEINDLEN(dset) < 1) { 
      SUMA_SL_Err("Null input"); 
      SUMA_ShowNel(dset->inel); 
      SUMA_DUMP_TRACE("Bad dset->inel, dumping trace for debug:"); 
      SUMA_RETURN(0); 
   } 
   
   if (!col) { 
      SUMA_LH("No data");
      /* Do not complain, that is not a bad thing.
      People can use this to allocate for a column
      without filling it up */
      stmp = SUMA_copy_string("0 0 -1 -1");
   } else { 
      SUMA_LH("Calculating range");
      switch (SUMA_ColType2TypeCast(ctp)) {
         case SUMA_int:
            iv = (int *)col;
            SUMA_MIN_MAX_VEC_STRIDE(iv ,SDSET_VECFILLED(dset), amin, amax, aminloc, amaxloc, stride);
            snprintf(Name, 500*sizeof(char),"%d %d %d %d", (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_float:
            fv = (float *)col;
            SUMA_MIN_MAX_VEC_STRIDE(fv ,SDSET_VECFILLED(dset), amin, amax, aminloc, amaxloc, stride);
            snprintf(Name, 500*sizeof(char),"%f %f %d %d", amin, amax, aminloc, amaxloc);
            break;
         case SUMA_byte:
            bv = (byte *)col;
            SUMA_MIN_MAX_VEC_STRIDE(bv ,SDSET_VECFILLED(dset), amin, amax, aminloc, amaxloc, stride);
            snprintf(Name, 500*sizeof(char),"%d %d %d %d", (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_string:
            Name[0] = '\0';
            break;
         default:
            fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
            SUMA_RETURN(0);
            break; 
      }
      stmp = SUMA_copy_string(Name);
   }
   
   NI_set_attribute(dset->inel, "COLMS_RANGE", stmp);
   
   if (stmp) SUMA_free(stmp); stmp = NULL;
   SUMA_RETURN(1);  
}


/*!
   Adds some generic attributes.
   For the moment, the range is added for numeric columns 
   if col_index is -1, then it is assumed that the attributes are for the latest column added (vec_num -1)
*/
int SUMA_AddGenColAttr (NI_element *nel, SUMA_COL_TYPE ctp, void *col, int stride, int col_index) 
{
   static char FuncName[]={"SUMA_AddGenColAttr"};
   static char stmp[500], Name[500];
   float amin = 0.0, amax = 0.0, *fv;
   int aminloc = -1, amaxloc = -1, *iv;
   byte *bv;

   SUMA_ENTRY;
   
   if (!SUMA_ALLOW_NEL_USE) SUMA_SL_Warn("Obsolete, use new version.");

   if (!nel) { SUMA_SL_Err("Null Nel"); SUMA_RETURN(0); }
   if (col_index < 0) col_index = nel->vec_num-1;
   if (col_index < 0 || !nel->vec_num ) { SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); }
   if (nel->vec_num <= col_index) { SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   
   sprintf(Name, "RangeCol_%d", col_index);

   if (!col) { 
      /* Do not complain, that is not a bad thing.
      People can use this to allocate for a column
      without filling it up */
      sprintf(stmp, "0 0 -1 -1");
   } else {
      switch (SUMA_ColType2TypeCast(ctp)) {
         case SUMA_int:
            iv = (int *)col;
            SUMA_MIN_MAX_VEC_STRIDE(iv ,nel->vec_filled, amin, amax, aminloc, amaxloc, stride);
            snprintf(stmp, 500*sizeof(char),"%d %d %d %d", (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_float:
            fv = (float *)col;
            SUMA_MIN_MAX_VEC_STRIDE(fv ,nel->vec_filled, amin, amax, aminloc, amaxloc, stride);
            snprintf(stmp, 500*sizeof(char),"%f %f %d %d", amin, amax, aminloc, amaxloc);
            break;
         case SUMA_byte:
            bv = (byte *)col;
            SUMA_MIN_MAX_VEC_STRIDE(bv ,nel->vec_filled, amin, amax, aminloc, amaxloc, stride);
            snprintf(stmp, 500*sizeof(char),"%d %d %d %d", (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_string:
            stmp[0] = '\0';
            break;
         default:
            fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
            SUMA_RETURN(0);
            break; 
      }
   }
   
   NI_set_attribute ( nel, Name, stmp);
   
   SUMA_allow_nel_use(0); 
   SUMA_RETURN(1);  
}


/*!
   \brief Gets the column range values
   col_index can be -1 if you want the attributes of the last column
*/
int SUMA_GetDsetColRange(SUMA_DSET *dset, int col_index, float range[2], int loc[2])
{
   static char FuncName[]={"SUMA_GetDsetColRange"};
   char *rs = NULL, **sc=NULL, Name[500];
   float nums[4];
   NI_element *nelb = NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->dnel) { SUMA_SL_Err("Null input"); SUMA_RETURN(0); }
   if (col_index < 0) col_index = SDSET_VECNUM(dset)-1;
   if (col_index < 0 || !SDSET_VECNUM(dset) ) { SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); }
   if (SDSET_VECNUM(dset) <= col_index) { SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_RANGE");
   if (!nelb) { SUMA_SL_Err("Failed to find column range attribute"); SUMA_RETURN(0); }
   
   SUMA_NEL_GET_STRING(nelb, 0, 0, rs); /* rs is a pointer copy here, do not free */
   rs = SUMA_Get_Sub_String(rs, SUMA_NI_CSS, col_index);
   if (!rs) { SUMA_SL_Err("No range field."); SUMA_RETURN(0); }
   if (SUMA_StringToNum(rs, nums, 4) != 4) { SUMA_SL_Err("Failed to read 4 nums from range."); SUMA_RETURN(0); }
   range[0] = nums[0]; range[1] = nums[1]; 
   loc[0] = (int)nums[2]; loc[1] = (int)nums[3];
   SUMA_free(rs); rs = NULL;   
   SUMA_RETURN(1);
}

int SUMA_GetDsetNodeIndexColRange(SUMA_DSET *dset, float range[2], int loc[2], int addifmissing)
{
   static char FuncName[]={"SUMA_GetDsetNodeIndexColRange"};
   char *rs = NULL, Name[500];
   float nums[4];
   
   SUMA_ENTRY;
   
   if (!dset || !dset->inel) { SUMA_SL_Err("Null input"); SUMA_RETURN(0); }
   
   rs = NI_get_attribute(dset->inel, "COLMS_RANGE");
   if (!rs) { 
      if (!addifmissing) {
         SUMA_SL_Err("No range field."); SUMA_RETURN(0); 
      }else {
         if (!SUMA_AddGenDsetNodeIndexColAttr (dset, SUMA_NODE_INDEX, SDSET_NODE_INDEX_COL(dset), 1)) {
            SUMA_SL_Err("Could not add range field."); SUMA_RETURN(0); 
         }
         rs = NI_get_attribute(dset->inel, "COLMS_RANGE"); 
      }
   }
   if (SUMA_StringToNum(rs, nums, 4) != 4) { SUMA_SL_Err("Failed to read 4 nums from range."); SUMA_RETURN(0); }
   range[0] = nums[0]; range[1] = nums[1]; 
   loc[0] = (int)nums[2]; loc[1] = (int)nums[3];
   SUMA_RETURN(1);
}


/*!
   \brief Gets the column range values
   col_index can be -1 if you want the attributes of the last column
*/
int SUMA_GetColRange(NI_element *nel, int col_index, float range[2], int loc[2])
{
   static char FuncName[]={"SUMA_GetColRange"};
   char *rs = NULL, Name[500];
   float nums[4];
   
   SUMA_ENTRY;
   
   SUMA_SL_Warn("Obsolete, use new version.");

   if (!nel) { SUMA_SL_Err("Null Nel"); SUMA_RETURN(0); }
   if (col_index < 0) col_index = nel->vec_num-1;
   if (col_index < 0 || !nel->vec_num ) { SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); }
   if (nel->vec_num <= col_index) { SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   
   
   sprintf(Name, "RangeCol_%d", col_index);
   rs = NI_get_attribute(nel, Name);
   
   if (!rs) { SUMA_SL_Err("No range field."); SUMA_RETURN(0); }
   if (SUMA_StringToNum(rs, nums, 4) != 4) { SUMA_SL_Err("Failed to read 4 nums from range."); SUMA_RETURN(0); }
   range[0] = nums[0]; range[1] = nums[1]; 
   loc[0] = (int)nums[2]; loc[1] = (int)nums[3];
      
   SUMA_RETURN(1);
}
/*!
   Adds a column to dset->dnel (new version of SUMA_AddNelCol)
   The vectors added are nel->vec_len long so col should contain at least
   nel->vec_len * stride elements.
   
   What to do when filling up to nel->vec_filled only ? Does one need to 
   write another version of NI_add_column_stride ? (see file niml/niml_element)
   Is the use of vec_filled what I think it is ?
   
   Mar 23 04: Made modifications to NI_add_column and _stride
   so that data are copied up to nel->vec_filled instead of 
   nel->vec_len if nel->vec_filled is > 0 and < vec_len
   
   Tue Aug 29 10:42:26 EDT 2006: SUMA_NODE_INDEX is handled separately, it is 
   an element in the group 
   
   If you wish to allocate space for a column (nel->vec_len long)
   then pass NULL for col
   
   Can also use SUMA_InsertDsetNelCol, tiz more flexible.
*/
int SUMA_AddDsetNelCol ( SUMA_DSET *dset, char *col_label, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride)
{
   static char FuncName[]={"SUMA_AddDsetNelCol"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_RETURN(SUMA_InsertDsetNelCol(dset, col_label, ctp, col, col_attr, stride, -1));
}

int SUMA_InsertDsetNelCol ( SUMA_DSET *dset, char *col_label, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride, int icol)
{
   static char FuncName[]={"SUMA_InsertDsetNelCol"};
   int *iv, is_sorted;
   NI_element *nelb=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (ctp == SUMA_NODE_INDEX) {
      SUMA_RETURN(SUMA_AddDsetNelIndexCol ( dset, col_label, ctp, col, col_attr, stride));
   }
   if (icol != -1) {
      SUMA_S_Err("Function not ready to deal with attribute insertion yet. See bottom of function");
      SUMA_RETURN(0); 
   }
   
   if (!dset || !dset->dnel) { SUMA_SL_Err("Null input"); SUMA_RETURN(0); }
   if (!col) { 
      /* Do not complain, that is not a bad thing.
      People can use this to allocate for a column
      without filling it up */
      /* SUMA_SL_Err("Null Col"); SUMA_RETURN(0); */
   }
   
   SUMA_LH("Checking Type");
   if (LocalHead && !col) {
      SUMA_ShowNel((void*)dset->dnel);
   }
   switch (SUMA_ColType2TypeCast(ctp)) {
      case SUMA_int:
         NI_insert_column_stride ( dset->dnel, NI_INT, (int *)col, stride, icol);
         break;
      case SUMA_float:
         NI_insert_column_stride ( dset->dnel, NI_FLOAT, (float *)col, stride, icol );      
         break;
      case SUMA_byte:
         NI_insert_column_stride ( dset->dnel, NI_BYTE, (byte *)col, stride, icol );      
         break;
      case SUMA_double:
         NI_insert_column_stride ( dset->dnel, NI_DOUBLE, (double *)col, stride, icol );      
         break;
      case SUMA_string:
         NI_insert_column_stride ( dset->dnel, NI_STRING, (char **)col, stride, icol );
         break;
      default:
         fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   if (LocalHead && !col) {
      SUMA_ShowNel((void*)dset->dnel);
   }
   #if 0 /* Now handled in SUMA_AddDsetNelIndexCol */
   if (ctp == SUMA_NODE_INDEX) {
      if (col) {
         SUMA_LH("Check sortedness");
         /* need to check for sortedness of list */
         iv = (int *)col;
         SUMA_IS_SORTED_UP(iv, SDSET_VECFILLED(dset), is_sorted);
         iv = NULL;
         if (is_sorted) {
            NI_set_attribute(dset->dnel, "sorted_node_def", "Yes");   
         } else {
            NI_set_attribute(dset->dnel, "sorted_node_def", "No");   
         }
      } else {
         NI_set_attribute(dset->dnel, "sorted_node_def", "Unknown");
      }
   }
   #endif
   
   SUMA_LH("Cheking generic attributes");
   
   /* set some generic attributes. 
   You must redo it for all columns from icol 
   to the very end but that is not a pleasant task 
   because the functions below cannot insert. Only append (icol = -1)
   or replace. You'll need to find a solution for inserts before 
   allowing column insertion.  */
   SUMA_AddGenDsetColAttr (dset, ctp, col, stride, icol);
   /* add the attributes of that column */
   SUMA_AddDsetColAttr (dset, col_label, ctp, col_attr, icol);
   
   SUMA_RETURN(1);
}

/*!
   \brief A version of SUMA_AddDsetNelCol that is specific for adding node indices
*/
int SUMA_AddDsetNelIndexCol ( SUMA_DSET *dset, char *col_label, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride)
{
   static char FuncName[]={"SUMA_AddDsetNelIndexCol"};
   int *iv, is_sorted;
   NI_element *nelb=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      SUMA_DUMP_TRACE("Trace at entry");
   }
   if (!dset || !dset->inel || !SDSET_NODEINDLEN(dset)) { 
      SUMA_SL_Err("Null input"); 
      SUMA_DUMP_TRACE("Bad dset->inel, dumping trace for debug:"); 
      SUMA_RETURN(0); 
   }
   if (!col) { 
      /* Do not complain, that is not a bad thing.
      People can use this to allocate for a column
      without filling it up */
      SUMA_LH("Null Col"); 
   }else {
      SUMA_LH("Non NuLL Col");
   }
   
   SUMA_LH("Checking Type");
   
   switch (SUMA_ColType2TypeCast(ctp)) { /* overkill, all we use is int here, but keeping the code anyway ...*/
      case SUMA_int:
         NI_add_column_stride ( dset->inel, NI_INT, col, stride);
         break;
      case SUMA_float:
         NI_add_column_stride ( dset->inel, NI_FLOAT, col, stride );      
         break;
      case SUMA_byte:
         NI_add_column_stride ( dset->inel, NI_BYTE, col, stride );      
         break;
      case SUMA_double:
         NI_add_column_stride ( dset->inel, NI_DOUBLE, col, stride );      
         break;
      case SUMA_string:
         NI_add_column_stride ( dset->inel, NI_STRING, col, stride );
         break;
      default:
         fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   
   if (ctp == SUMA_NODE_INDEX) { /* of course it is SUMA_NODE_INDEX*/
      if (col) {
         SUMA_LH("Check sortedness");
         /* need to check for sortedness of list */
         iv = (int *)col;
         SUMA_IS_SORTED_UP(iv, SDSET_VECFILLED(dset), is_sorted);
         iv = NULL;
         if (is_sorted) {
            NI_set_attribute(dset->inel, "sorted_node_def", "Yes");   
         } else {
            NI_set_attribute(dset->inel, "sorted_node_def", "No");   
         }
      } else {
         NI_set_attribute(dset->inel, "sorted_node_def", "Unknown");
      }
   }
   
   
   SUMA_LH("Cheking generic attributes");
   /* set some generic attributes */
   SUMA_AddGenDsetColAttr (dset, ctp, col, stride, -1);
   /* add the attributes of that column */
   SUMA_AddDsetColAttr (dset, col_label, ctp, col_attr, -1);
   
   if (LocalHead) SUMA_ShowNel((void*)dset->inel);
   
   
   SUMA_RETURN(1);
}
/*!
   Adds a column to Nel
   The vectors added are nel->vec_len long so col should contain at least
   nel->vec_len * stride elements.
   
   What to do when filling up to nel->vec_filled only ? Does one need to 
   write another version of NI_add_column_stride ? (see file niml/niml_element)
   Is the use of vec_filled what I think it is ?
   
   Mar 23 04: Made modifications to NI_add_column and _stride
   so that data are copied up to nel->vec_filled instead of 
   nel->vec_len if nel->vec_filled is > 0 and < vec_len
   
   If you wish to allocate space for a column (nel->vec_len long)
   then pass NULL for col
*/

int SUMA_AddNelCol ( NI_element *nel, char *col_label, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride)
{
   static char FuncName[]={"SUMA_AddNelCol"};
   int *iv, is_sorted;
   SUMA_ENTRY;
   
   if (!SUMA_ALLOW_NEL_USE) SUMA_SL_Warn("Obsolete, use new version.");
   if (!nel) { SUMA_SL_Err("Null Nel"); SUMA_RETURN(0); }
   if (!col) { 
      /* Do not complain, that is not a bad thing.
      People can use this to allocate for a column
      without filling it up */
      /* SUMA_SL_Err("Null Col"); SUMA_RETURN(0); */
   }
   
   switch (SUMA_ColType2TypeCast(ctp)) {
      case SUMA_int:
         NI_add_column_stride ( nel, NI_INT, (int *)col, stride);
         break;
      case SUMA_float:
         NI_add_column_stride ( nel, NI_FLOAT, (float *)col, stride );      
         break;
      case SUMA_byte:
         NI_add_column_stride ( nel, NI_BYTE, (byte *)col, stride );      
         break;
      case SUMA_double:
         NI_add_column_stride ( nel, NI_DOUBLE, (double *)col, stride );      
         break;
      case SUMA_string:
         NI_add_column_stride ( nel, NI_STRING, (char **)col, stride );
         break;
      default:
         fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   
   if (ctp == SUMA_NODE_INDEX) {
      if (col) {
         /* need to check for sortedness of list */
         iv = (int *)col;
         SUMA_IS_SORTED_UP(iv, nel->vec_filled, is_sorted)
         iv = NULL;
         if (is_sorted) {
            NI_set_attribute(nel, "sorted_node_def", "Yes");   
         } else {
            NI_set_attribute(nel, "sorted_node_def", "No");   
         }
      } else {
         NI_set_attribute(nel, "sorted_node_def", "Unknown");
      }
   }

   /* set some generic attributes */
   SUMA_allow_nel_use(1);
   SUMA_AddGenColAttr (nel, ctp, col, stride, -1);
   /* add the attributes of that column */
   SUMA_allow_nel_use(1);
   SUMA_AddColAttr (nel, col_label, ctp, col_attr, -1);
   
   SUMA_allow_nel_use(0);
   SUMA_RETURN(1);
}

/*!
   \brief See SUMA_MaskedCopyofDset, for all parameters not listed here.
   SUMA_DSET * SUMA_MaskedByNodeIndexCopyofDset(SUMA_DSET *odset, 
                                                int *indexlist, int N_indexlist, 
                                                byte *colmask, int masked_only, int keep_node_index)
   
   \param indexlist (int *) a list of N_indexlist node indices to retain in output dset.
   \param N_indexlist (int)
   These two params are used instead of rowmask in SUMA_MaskedCopyofDset
*/ 
SUMA_DSET * SUMA_MaskedByNodeIndexCopyofDset(SUMA_DSET *odset, int *indexlist, int N_indexlist, byte *colmask, 
                                             int masked_only, int keep_node_index)
{
   static char FuncName[]={"SUMA_MaskedByNodeIndexCopyofDset"};
   SUMA_DSET *dset_m = NULL;
   byte *Tb = NULL;
   int *indexmap = NULL, j=0;
   
   SUMA_ENTRY;
   
   if (!(indexmap = SUMA_CreateNodeIndexToRowIndexMap(odset))) {
      SUMA_S_Err("Failed to get indexmap");
      SUMA_RETURN(NULL);
   }
    
   Tb = (byte *) SUMA_calloc(SDSET_VECLEN(odset), sizeof(byte));
   for (j=0; j<N_indexlist; ++j) {
      if (indexlist[j] < SDSET_VECFILLED(odset)) { 
         Tb[indexmap[indexlist[j]]] = 1;
      } else {
         SUMA_S_Warn("Nodes in indexlist exceed odset->dnel->vec_filled\n"
                     "Such nodes will be ignored but may indicate more serious trouble\n"
                     "Warning will not be repeated in this call.");
      }
   }
   SUMA_free(indexmap); indexmap = NULL;
   if (!(dset_m = SUMA_MaskedCopyofDset(odset, Tb, colmask, masked_only, keep_node_index ))) {
      SUMA_S_Err("Failed to mask dset by node indices");
      SUMA_free(Tb); Tb = NULL;
      SUMA_RETURN(NULL);
   }
   
   SUMA_RETURN(dset_m);
}
 
/*! 
   \brief creates a new dataset that has a copy of each row in odset 
   wherever rowmask[irow] is not zero. 
   \param odset (SUMA_DSET *) input dataset
   \param rowmask (byte *) [nel->vec_len x 1] vector specifying which rows to preserve
                                             If rowmask[irow] is 1 then this row is copied
                                             into ndset. 
                                             If rowmask[irow] = 0 then the row is either
                                             skipped (see masked_only) or set to 0 in its entirety 
                                             (see keep_node_index for exception).
                                             If rowmask == NULL then all rows are copied
   \param colmask (byte *) [nel->vec_num x 1] vector specifying which volumns to operate on.
                                             If colmask[icol] is 1 then values in this column 
                                             are copied. 
                                             If colmask == NULL then all columns are copied
   \param masked_only (int)   If 1 then the output dataset is only to contain
                              those rows where rowmask[irow] = 1
                              If 0 then all rows are output but with column entries set to 0
                              for all rows where rowmask[irow] = 0. One column might be
                              exempt from nulling if it meets the requirements on Schedule B form suma654.233 
                              or if it is of the type SUMA_NODE_INDEX and keep_node_index is set to 1. 
                              
   \param keep_node_index (int) If 1, then preserves the node index column (SUMA_NODE_INDEX) from being masked.
                                Makes sense to use it when masked_only == 0. 
   \param ndset (SUMA_DSET *) Copy of dataset with masking applied.
   
   - You might want to have a version that replaces columns in odset with the masked data
   as opposed to copying them. I think I do something like this with the drawn ROI dataset...
   
   \sa SUMA_Copy_Part_Column, SUMA_MaskedByNodeIndexCopyofDset
*/  
SUMA_DSET * SUMA_MaskedCopyofDset(SUMA_DSET *odset, byte *rowmask, byte *colmask, int masked_only, int keep_node_index)
{
   static char FuncName[]={"SUMA_MaskedCopyofDset"};
   int n_incopy = -1, i;
   char *new_name=NULL, idcode[SUMA_IDCODE_LENGTH], *lblcp;
   SUMA_DSET *ndset=NULL;
   NI_rowtype *rt=NULL, *rti=NULL;
   SUMA_COL_TYPE ctp = SUMA_ERROR_COL_TYPE;
   void *ncol=NULL, *ncoli=NULL, *ind=NULL;
   int DoThisCol=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!odset) { SUMA_SL_Err("Null input"); SUMA_RETURN(NULL); }
   if (!odset->dnel) { SUMA_SL_Err("Null dnel"); SUMA_RETURN(NULL); }
   if (!SUMA_is_AllNumeric_dset(odset)) {
      SUMA_SL_Err("Function does not deal with data sets containing non-numeric columns");
      SUMA_RETURN(NULL);
   }
   if (0 && LocalHead) {
      SUMA_ShowNel((void*)odset->dnel);
   }
   for (i=0; i < SDSET_VECNUM(odset); ++i) {
      if (!colmask) DoThisCol = 1;
      else DoThisCol = colmask[i];
      if (DoThisCol) {
         if (LocalHead) fprintf(SUMA_STDERR,"%s:\nProcessing column %d\n", FuncName, i);
         ctp = SUMA_TypeOfDsetColNumb(odset, i); 
         rt = NI_rowtype_find_code(SUMA_ColType2TypeCast(ctp)) ; 
         if( rt == NULL || ROWTYPE_is_varsize(rt)) {
            SUMA_SL_Err("Could not recognize rowtype, or rowtype is of variable size."); SUMA_RETURN(NULL);
         }
         if (ctp == SUMA_NODE_INDEX && keep_node_index && !masked_only) {
            SUMA_S_Err("This should not be anymore. Skipping...");
            /* preserve all rows */
            /* ncol = SUMA_Copy_Part_Column(odset->dnel->vec[i], rt, SDSET_VECLEN(odset), NULL, masked_only, &n_incopy); */
         } else {
            SUMA_LH("Preserving rows in mask only");
            ncol = SUMA_Copy_Part_Column(odset->dnel->vec[i], rt, SDSET_VECLEN(odset), rowmask, masked_only, &n_incopy);  
         }
         if (!ncol) {
            SUMA_SL_Err("No data got copied.");
            SUMA_RETURN(ndset);
         }
         if (!ndset) {
            new_name = SUMA_append_string(NI_get_attribute(odset->ngr,"filename"),"copy");
            UNIQ_idcode_fill(idcode); 
            ndset =  SUMA_CreateDsetPointer( new_name, SUMA_Dset_Type(NEL_DSET_TYPE(odset->ngr)), idcode, NI_get_attribute(odset->ngr,"domain_parent_idcode"),  n_incopy ); 
            SUMA_free(new_name); new_name = NULL;
            /* good time to deal with node index element */
            if ((ind = SDSET_NODE_INDEX_COL(odset))) {
               rti = NI_rowtype_find_code(SUMA_ColType2TypeCast(SUMA_NODE_INDEX)) ;
               if (keep_node_index && !masked_only) {
                  /* preserve all rows */
                  ncoli = SUMA_Copy_Part_Column(ind, rti, SDSET_VECLEN(odset), NULL, masked_only, &n_incopy);
               } else {
                  ncoli = SUMA_Copy_Part_Column(ind, rti, SDSET_VECLEN(odset), rowmask, masked_only, &n_incopy);  
               }
               if (!ncoli) {
                  SUMA_SL_Err("No index data got copied.");
                  SUMA_RETURN(ndset);
               }
               if (!SUMA_AddDsetNelCol (ndset, NI_get_attribute(odset->inel,"COLMS_LABS"), SUMA_NODE_INDEX, ncoli, NULL ,1)) {
                  SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
                  SUMA_FreeDset((void*)ndset); ndset = NULL;
                  SUMA_RETURN(ndset);
               }
               if (lblcp) SUMA_free(lblcp); lblcp = NULL; 
            }
         }
         /* add the column */
         SUMA_LH("Getting the label");
         lblcp = SUMA_DsetColLabelCopy(odset, i, 0);
         SUMA_LH("Inserting the column");
         if (!SUMA_AddDsetNelCol (ndset, lblcp, ctp, ncol, NULL ,1)) {
            SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
            SUMA_FreeDset((void*)ndset); ndset = NULL;
            SUMA_RETURN(ndset);
         } 
         if (lblcp) SUMA_free(lblcp); lblcp = NULL;
      } else {
         if (LocalHead) fprintf(SUMA_STDERR,"%s:\nSkipping column %d\n", FuncName, i);
      }
   }
   
   SUMA_RETURN(ndset);
}

/*!
   see help for SUMA_MaskedCopyofDset
      
*/
void *SUMA_Copy_Part_Column(void *col, NI_rowtype *rt, int N_col, byte *rowmask, int masked_only, int *n_incopy)
{
   static char FuncName[]={"SUMA_Copy_Part_Column"};
   int n_alloc = -1, i, j, cnt;
   char *ndat = NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
    
   *n_incopy = -1;
   n_alloc = 0;
   if (masked_only) {
      if (rowmask) {
         for (i=0; i<N_col; ++i) {
            if (rowmask[i]) ++n_alloc;
         }
      } else { /* copy entire column ... */
         n_alloc = N_col;
      }
   } else {
      n_alloc = N_col;
   }  
   
   if (!n_alloc) {
      SUMA_SL_Err("No values to go in column...");
      SUMA_RETURN(NULL);
   }
   
   /* allocate for result */
         ndat = (char *)SUMA_malloc(sizeof(char) *  rt->size * n_alloc ) ;
         if (!ndat) { SUMA_SL_Crit("Failed to allocate for ndat"); SUMA_RETURN(NULL); }
         /* Now to copy the proper values */
         if (!masked_only) {
            SUMA_LH("All copy");
            memcpy( ndat , (char *)col , rt->size * n_alloc ) ;
            if (LocalHead && rt->code == NI_FLOAT) {
               float *fv=(float*)ndat, *cv=(float *)col;
               fprintf(SUMA_STDERR, "i=0; %f, %f\ni=1; %f, %f\n", fv[0], cv[0], fv[1], cv[1]);
            }
            /* reset values that are not in mask */
            if (rowmask) {
               switch(rt->code) {
                  case NI_BYTE:
                     for (j=0; j<N_col; ++j) if (!rowmask[j]) {SUMA_ASSIGN_VALUE_IN_VEC (ndat, j, byte, 0);}
                     break;
                  case NI_SHORT:
                     for (j=0; j<N_col; ++j) if (!rowmask[j]) {SUMA_ASSIGN_VALUE_IN_VEC (ndat, j, short, 0);}
                     break;
                  case NI_INT:
                     for (j=0; j<N_col; ++j) if (!rowmask[j]) {SUMA_ASSIGN_VALUE_IN_VEC (ndat, j, int, 0);}
                     break;
                  case NI_FLOAT:
                     for (j=0; j<N_col; ++j) if (!rowmask[j]) {SUMA_ASSIGN_VALUE_IN_VEC (ndat, j, float, 0);}
                     break;
                  case NI_DOUBLE:
                     for (j=0; j<N_col; ++j) if (!rowmask[j]) {SUMA_ASSIGN_VALUE_IN_VEC (ndat, j, double, 0);}
                     break;
                  default:
                     SUMA_SL_Warn("Type not allowed for masking operation, skipping.");
                     break;
               }
            } 
         } else {
            if (rowmask) {
               /* copy good values, one at a time */
               SUMA_LH("Masked only copy");
               cnt = 0;
               switch(rt->code) {
                  case NI_BYTE:
                     for (j=0; j<N_col; ++j) if (rowmask[j]) {SUMA_COPY_VALUE_IN_VEC(col, ndat, j, cnt, byte, byte); ++cnt;}
                     break;
                  case NI_SHORT:
                     for (j=0; j<N_col; ++j) if (rowmask[j]) {SUMA_COPY_VALUE_IN_VEC(col, ndat, j, cnt, short, short); ++cnt;}
                     break;
                  case NI_INT:
                     for (j=0; j<N_col; ++j) if (rowmask[j]) {SUMA_COPY_VALUE_IN_VEC(col, ndat, j, cnt, int, int); ++cnt;}
                     break;
                  case NI_FLOAT:
                     for (j=0; j<N_col; ++j) if (rowmask[j]) {SUMA_COPY_VALUE_IN_VEC(col, ndat, j, cnt, float, float); ++cnt;}
                     break;
                  case NI_DOUBLE:
                     for (j=0; j<N_col; ++j) if (rowmask[j]) {SUMA_COPY_VALUE_IN_VEC(col, ndat, j, cnt, double, double); ++cnt;}
                     break;
                  default:
                     SUMA_SL_Warn("Type not allowed for masking operation, skipping.");
                     break;
               }
            } else {
               /* wants a copy of everything */
               memcpy( ndat , (char *)col , rt->size * n_alloc ) ;
            } 
            
         }
   *n_incopy = n_alloc;
   SUMA_LH("Returning");
   SUMA_RETURN((void *)ndat);
}
/*!
   \brief Function to fill the contents of a pre-existing column 
   created with SUMA_AddDsetNelCol.  
   if vec_filled > 0 && vec_filled <= vec_len, filling is done
   up to vec_filled.
   vec_filled must be set BEFORE YOU CALL THIS FUNCTION
   New version of SUMA_FillNelCol
*/
int SUMA_FillDsetNelCol (SUMA_DSET *dset, char *col_label, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride) 
{  
   static char FuncName[]={"SUMA_FillDsetNelCol"};
   int icol = -1, is_sorted;
   int *iv, N_i;
   
   SUMA_ENTRY;
   
   if (ctp == SUMA_NODE_INDEX) {
      SUMA_RETURN(SUMA_FillDsetNelNodeIndexCol (dset, col_label, ctp, col, col_attr, stride));
   }
   
   /* find the index into vec of the column of type ctp,
      complain if you find more than 1 */
   iv = SUMA_GetDsetColIndex (dset, ctp, &N_i);
   if (N_i != 1) {
      SUMA_SL_Err("Found more than one column.\n");
      SUMA_RETURN(-1);
   }
   icol = iv[0];
   SUMA_free(iv); iv = NULL;
   
   /* Now use the function NI_fill_column_stride
   that I have yet to write in nim_element.c
   (a modification of NI_add_column_stride) and
   you're all set */ 
   switch (SUMA_ColType2TypeCast(ctp)) {
      case SUMA_int:
         NI_fill_column_stride ( dset->dnel, NI_INT, (int *)col, icol, stride);
         break;
      case SUMA_float:
         NI_fill_column_stride ( dset->dnel, NI_FLOAT, (float *)col, icol, stride );      
         break;
      case SUMA_byte:
         NI_fill_column_stride ( dset->dnel, NI_BYTE, (byte *)col, icol, stride );      
         break;
      case SUMA_string:
         NI_fill_column_stride ( dset->dnel, NI_STRING, (char **)col, icol, stride );
         break;
      case SUMA_double:
         NI_fill_column_stride ( dset->dnel, NI_DOUBLE, (double *)col, icol, stride );
         break;
      default:
         fprintf  (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   
   #if 0 /* Now handled in SUMA_FillDsetNelNodeIndexCol */
   if (ctp == SUMA_NODE_INDEX) { 
      if (col) {
         /* need to check for sortedness of list */
         iv = (int *)col;
         SUMA_IS_SORTED_UP(iv, SDSET_VECFILLED(dset), is_sorted)
         iv = NULL;
         if (is_sorted) {
            NI_set_attribute(dset->dnel, "sorted_node_def", "Yes");   
         } else {
            NI_set_attribute(dset->dnel, "sorted_node_def", "No");   
         }
      } else {
         NI_set_attribute(dset->dnel, "sorted_node_def", "Unknown");
      }
   }
   #endif
   /* set some generic attributes */
   SUMA_AddGenDsetColAttr (dset, ctp, col, stride, icol);
   /* add the attributes of that column */
   SUMA_AddDsetColAttr (dset, col_label, ctp, col_attr, icol);
   
   SUMA_RETURN(1);
}
int SUMA_FillDsetNelNodeIndexCol (SUMA_DSET *dset, char *col_label, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride) 
{  
   static char FuncName[]={"SUMA_FillDsetNelNodeIndexCol"};
   int is_sorted, *iv=NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->inel) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(0);
   }   
   
   if (ctp != SUMA_NODE_INDEX) {
      SUMA_S_Err("Not for you!");
      SUMA_RETURN(0);
   }
   
   /* Now use the function NI_fill_column_stride
   that I have yet to write in nim_element.c
   (a modification of NI_add_column_stride) and
   you're all set */ 
   switch (SUMA_ColType2TypeCast(ctp)) { 
      case SUMA_int:
         NI_fill_column_stride ( dset->inel, NI_INT, col, 0, stride);
         break;
      case SUMA_float:
         NI_fill_column_stride ( dset->inel, NI_FLOAT, col, 0, stride );      
         break;
      case SUMA_byte:
         NI_fill_column_stride ( dset->inel, NI_BYTE, col, 0, stride );      
         break;
      case SUMA_string:
         NI_fill_column_stride ( dset->inel, NI_STRING, col, 0, stride );
         break;
      case SUMA_double:
         NI_fill_column_stride ( dset->inel, NI_DOUBLE, col, 0, stride );
         break;
      default:
         fprintf  (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   
   if (ctp == SUMA_NODE_INDEX) { /* of course it is! */
      if (col) {
         /* need to check for sortedness of list */
         iv = (int *)col;
         SUMA_IS_SORTED_UP(iv, SDSET_VECFILLED(dset), is_sorted)
         iv = NULL;
         if (is_sorted) {
            NI_set_attribute(dset->inel, "sorted_node_def", "Yes");   
         } else {
            NI_set_attribute(dset->inel, "sorted_node_def", "No");   
         }
         /* set some generic attributes */
         SUMA_AddGenDsetColAttr (dset, ctp, col, stride, -1);
         /* add the attributes of that column */
         SUMA_AddDsetColAttr (dset, col_label, ctp, col_attr, -1);
      } else {
         NI_set_attribute(dset->inel, "sorted_node_def", "Unknown");
      }
   }
   
   
   SUMA_RETURN(1);
}
/*!
   \brief Function to fill the contents of a pre-existing column 
   created with SUMA_AddNelCol.  
   if vec_filled > 0 && vec_filled <= vec_len, filling is done
   up to vec_filled.
   vec_filled must be set BEFORE YOU CALL THIS FUNCTION
*/
int SUMA_FillNelCol (NI_element *nel, char *col_label, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride) 
{  
   static char FuncName[]={"SUMA_FillNelCol"};
   int icol = -1;
   int *iv, N_i, is_sorted;

   SUMA_ENTRY;

   if (!SUMA_ALLOW_NEL_USE) SUMA_SL_Warn("Obsolete, use new version.");
   
   /* find the index into vec of the column of type ctp,
      complain if you find more than 1 */
   iv = SUMA_GetColIndex (nel, ctp, &N_i);
   if (N_i != 1) {
      SUMA_SL_Err("Found more than one column.\n");
      SUMA_RETURN(-1);
   }
   icol = iv[0];
   SUMA_free(iv); iv = NULL;
   
   /* Now use the function NI_fill_column_stride
   that I have yet to write in nim_element.c
   (a modification of NI_add_column_stride) and
   you're all set */ 
   switch (SUMA_ColType2TypeCast(ctp)) {
      case SUMA_int:
         NI_fill_column_stride ( nel, NI_INT, (int *)col, icol, stride);
         break;
      case SUMA_float:
         NI_fill_column_stride ( nel, NI_FLOAT, (float *)col, icol, stride );      
         break;
      case SUMA_byte:
         NI_fill_column_stride ( nel, NI_BYTE, (byte *)col, icol, stride );      
         break;
      case SUMA_string:
         NI_fill_column_stride ( nel, NI_STRING, (char **)col, icol, stride );
         break;
      case SUMA_double:
         NI_fill_column_stride ( nel, NI_DOUBLE, (double *)col, icol, stride );
         break;
      default:
         fprintf  (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   
   if (ctp == SUMA_NODE_INDEX) {
      if (col) {
         /* need to check for sortedness of list */
         iv = (int *)col;
         SUMA_IS_SORTED_UP(iv, nel->vec_filled, is_sorted)
         iv = NULL;
         if (is_sorted) {
            NI_set_attribute(nel, "sorted_node_def", "Yes");   
         } else {
            NI_set_attribute(nel, "sorted_node_def", "No");   
         }
      } else {
         NI_set_attribute(nel, "sorted_node_def", "Unknown");
      }
   }
   
   /* set some generic attributes */
   SUMA_AddGenColAttr (nel, ctp, col, stride, icol);
   /* add the attributes of that column */
   SUMA_AddColAttr (nel, col_label, ctp, col_attr, icol);
   
   SUMA_allow_nel_use(0); 
   SUMA_RETURN(1);
}

SUMA_VARTYPE SUMA_CTypeName2VarType (char *vt)
{
   static char FuncName[]={"SUMA_CTypeName2VarType"};
   
   SUMA_ENTRY;
   
   if (!vt) SUMA_RETURN(SUMA_notypeset);
   
   if (strstr(vt,"int")) SUMA_RETURN(SUMA_int);
   if (strstr(vt,"float")) SUMA_RETURN(SUMA_float);
   if (strstr(vt,"byte")) SUMA_RETURN(SUMA_byte);
   if (strstr(vt,"double")) SUMA_RETURN(SUMA_double);
   if (strstr(vt,"short")) SUMA_RETURN(SUMA_short);
   if (strstr(vt,"complex")) SUMA_RETURN(SUMA_complex);
   
   SUMA_RETURN(SUMA_notypeset);
}

const char *SUMA_VarType2CTypeName (SUMA_VARTYPE vt)
{
   static char FuncName[]={"SUMA_VarType2CTypeName"};
   
   SUMA_ENTRY;
   
   switch (vt) {
      case SUMA_int:
         SUMA_RETURN("int");
         break;
      case SUMA_float:
         SUMA_RETURN("float");
         break;
      case SUMA_byte:
         SUMA_RETURN("byte");
         break;
      case SUMA_double:
         SUMA_RETURN("double");
         break;
      case SUMA_short:
         SUMA_RETURN("short");
         break;
      case SUMA_complex:
         SUMA_RETURN("complex");
         break;
      default:
         SUMA_RETURN("dunno");
   }
   
}

int SUMA_SizeOf(SUMA_VARTYPE vt)
{
   static char FuncName[]={"SUMA_SizeOf"};
   
   SUMA_ENTRY;
   
   switch (vt) {
      case SUMA_int:
         SUMA_RETURN(sizeof(int));
         break;
      case SUMA_float:
         SUMA_RETURN(sizeof(float));
         break;
      case SUMA_byte:
         SUMA_RETURN(sizeof(byte));
         break;
      case SUMA_double:
         SUMA_RETURN(sizeof(double));
         break;
      case SUMA_short:
         SUMA_RETURN(sizeof(short));
         break;
      case SUMA_complex:
         SUMA_RETURN(sizeof(complex));
         break;
      default:
         SUMA_RETURN(-1);
   }
   
}

SUMA_COL_TYPE SUMA_VarType2ColType (char *vt)
{
   static char FuncName[]={"SUMA_VarType2ColType"};
   
   SUMA_ENTRY;
   
   if (!vt) SUMA_RETURN(SUMA_notypeset);
   
   if (strstr(vt,"int")) SUMA_RETURN(SUMA_NODE_INT);
   if (strstr(vt,"float")) SUMA_RETURN(SUMA_NODE_FLOAT);
   if (strstr(vt,"byte")) SUMA_RETURN(SUMA_NODE_BYTE);
   if (strstr(vt,"double")) SUMA_RETURN(SUMA_NODE_DOUBLE);
   if (strstr(vt,"short")) SUMA_RETURN(SUMA_NODE_SHORT);
   
   SUMA_RETURN(SUMA_ERROR_COL_TYPE);
}
 
SUMA_VARTYPE SUMA_ColType2TypeCast (SUMA_COL_TYPE ctp) 
{
   static char FuncName[]={"SUMA_ColType2TypeCast"};
   
   SUMA_ENTRY;
   
   switch (ctp) {
      case SUMA_NODE_INT:
      case SUMA_NODE_ILABEL:
      case SUMA_NODE_INDEX:
         SUMA_RETURN(SUMA_int);
         break;
      case SUMA_NODE_FLOAT:
      case SUMA_NODE_CX:
      case SUMA_NODE_X:
      case SUMA_NODE_Y:
      case SUMA_NODE_Z:
      case SUMA_NODE_R:
      case SUMA_NODE_G:
      case SUMA_NODE_B:
      case SUMA_NODE_A:
      case SUMA_NODE_XCORR:
      case SUMA_NODE_3C:
         SUMA_RETURN(SUMA_float);      
         break;
      case SUMA_NODE_BYTE:
      case SUMA_NODE_Rb:
      case SUMA_NODE_Gb:
      case SUMA_NODE_Bb:
      case SUMA_NODE_Ab:
         SUMA_RETURN(SUMA_byte);          
         break;
      case SUMA_NODE_STRING:
         SUMA_RETURN(SUMA_string);
         break;
      default:
         fprintf  (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(SUMA_notypeset);
         break; 
   }
   
   SUMA_RETURN(SUMA_notypeset);
}
char * SUMA_Dset_Format_Name (SUMA_DSET_FORMAT fr)
{
   static char FuncName[]={"SUMA_Dset_Format_Name"};
   
   SUMA_ENTRY;
   
   switch(fr) {
      case SUMA_ERROR_DSET_FORMAT:
         SUMA_RETURN ("Error_Dset_Format");
         break;
      case SUMA_NO_DSET_FORMAT:
         SUMA_RETURN ("Dset_Format_Undefined");
         break;
      case SUMA_ASCII_NIML:
         SUMA_RETURN ("Ascii_Niml");
         break;
      case SUMA_BINARY_NIML:
         SUMA_RETURN ("Binary_Niml");
         break;
      case SUMA_NIML:
         SUMA_RETURN ("Niml");
         break;
      case SUMA_1D:
         SUMA_RETURN ("Afni_1D");
         break;
      case SUMA_ASCII_OPEN_DX_DSET:
         SUMA_RETURN ("Ascii_OpenDX_dset");
         break;
      default:
         SUMA_RETURN("Cowabonga-gimlauron");
         break;
   }   
   
}

SUMA_DSET_FORMAT SUMA_Dset_Format (char *Name)
{
   static char FuncName[]={"SUMA_Dset_Format"};

   SUMA_ENTRY;
   
   if (!strcmp(Name,"Error_Dset_Format")) SUMA_RETURN (SUMA_ERROR_DSET_FORMAT);
   if (!strcmp(Name,"Dset_Format_Undefined")) SUMA_RETURN (SUMA_NO_DSET_FORMAT);
   if (!strcmp(Name,"Ascii_Niml")) SUMA_RETURN (SUMA_ASCII_NIML);
   if (!strcmp(Name,"Binary_Niml")) SUMA_RETURN (SUMA_BINARY_NIML);
   if (!strcmp(Name,"Niml")) SUMA_RETURN (SUMA_NIML);
   if (!strcmp(Name,"Afni_1D")) SUMA_RETURN (SUMA_1D);
   if (!strcmp(Name,"Ascii_OpenDX_dset")) SUMA_RETURN (SUMA_ASCII_OPEN_DX_DSET);
   SUMA_RETURN(SUMA_ERROR_DSET_FORMAT);
}

char * SUMA_Dset_Type_Name (SUMA_DSET_TYPE tp)
{
   static char FuncName[]={"SUMA_Dset_Type_Name"};
   
   SUMA_ENTRY;
   
   switch (tp) {
      case SUMA_NO_DSET_TYPE:
         SUMA_RETURN("Dset_Type_Undefined");
         break;
      case SUMA_ERROR_DSET_TYPE:
         SUMA_RETURN("Error_Dset_Type");
         break;
      case SUMA_NODE_BUCKET:
         SUMA_RETURN("Node_Bucket");
         break;
      case SUMA_AFNI_NODE_BUCKET:
         SUMA_RETURN("AFNI_3D_dataset");
         break;
      case SUMA_NODE_ROI:
         SUMA_RETURN("Node_ROI");
         break;
      case SUMA_NODE_RGB:
         SUMA_RETURN("Node_RGB");
         break;
      case SUMA_NODE_RGBA:
         SUMA_RETURN("Node_RGBA");
         break;
      case SUMA_NODE_RGBb:
         SUMA_RETURN("Node_RGBb");
         break;
      case SUMA_NODE_RGBAb:
         SUMA_RETURN("Node_RGBAb");
         break;
      case SUMA_NODE_XYZ:
         SUMA_RETURN("Node_XYZ");
         break;
      case SUMA_NEW_NODE_XYZ:
         SUMA_RETURN("NewNode_XYZ");
         break;
      case SUMA_VIEWER_SETTING:
         SUMA_RETURN("Viewer_Visual_Setting");
         break;
      case SUMA_NODE_CONVEXITY:
         SUMA_RETURN("Node_Convexity");
         break;
      case SUMA_NEW_MESH_IJK:
         SUMA_RETURN("NewMesh_IJK");
         break;
      case SUMA_MESH_IJK:
         SUMA_RETURN("Mesh_IJK");
         break;
      case SUMA_PREP_NEW_SURFACE:
         SUMA_RETURN("PrepNewSurface");
         break;
      case SUMA_SURFACE_VOLUME_PARENT:
         SUMA_RETURN("SurfaceVolumeParent");
         break;
      case SUMA_SURFACE_OBJECT:
         SUMA_RETURN("SurfaceObject");
         break;
      default:
         SUMA_RETURN("Cowabonga-gothdo");
         break;
   }
}

SUMA_DSET_TYPE SUMA_Dset_Type (char *Name)
{
   static char FuncName[]={"SUMA_Dset_Type"};
   
   SUMA_ENTRY;
   if (!Name) { SUMA_S_Err("Null Name"); SUMA_RETURN(SUMA_NO_DSET_TYPE); }
   if (!strcmp(Name,"Dset_Type_Undefined")) SUMA_RETURN (SUMA_NO_DSET_TYPE);
   if (!strcmp(Name,"Error_Dset_Type")) SUMA_RETURN (SUMA_ERROR_DSET_TYPE);
   if (!strcmp(Name,"Node_Bucket")) SUMA_RETURN (SUMA_NODE_BUCKET);
   if (!strcmp(Name,"Node_ROI")) SUMA_RETURN (SUMA_NODE_ROI);
   if (!strcmp(Name,"Node_RGB")) SUMA_RETURN (SUMA_NODE_RGB);
   if (!strcmp(Name,"Node_RGBA")) SUMA_RETURN (SUMA_NODE_RGBA);
   if (!strcmp(Name,"Node_RGBb")) SUMA_RETURN (SUMA_NODE_RGBb);
   if (!strcmp(Name,"Node_RGBAb")) SUMA_RETURN (SUMA_NODE_RGBAb);
   if (!strcmp(Name,"Node_XYZ")) SUMA_RETURN (SUMA_NODE_XYZ);
   if (!strcmp(Name,"NewNode_XYZ")) SUMA_RETURN (SUMA_NEW_NODE_XYZ);
   if (!strcmp(Name,"Viewer_Visual_Setting")) SUMA_RETURN (SUMA_VIEWER_SETTING);
   if (!strcmp(Name,"Cowabonga")) SUMA_RETURN (SUMA_ERROR_DSET_TYPE);
   if (!strcmp(Name,"Node_Convexity")) SUMA_RETURN (SUMA_NODE_CONVEXITY);
   if (!strcmp(Name,"AFNI_3D_dataset")) SUMA_RETURN (SUMA_AFNI_NODE_BUCKET);
   if (!strcmp(Name,"NewMesh_IJK")) SUMA_RETURN (SUMA_NEW_MESH_IJK);
   if (!strcmp(Name,"Mesh_IJK")) SUMA_RETURN (SUMA_MESH_IJK);
   if (!strcmp(Name,"PrepNewSurface")) SUMA_RETURN (SUMA_PREP_NEW_SURFACE);
   if (!strcmp(Name,"SurfaceVolumeParent")) SUMA_RETURN (SUMA_SURFACE_VOLUME_PARENT);
   if (!strcmp(Name,"SurfaceObject")) SUMA_RETURN (SUMA_SURFACE_OBJECT);
   SUMA_RETURN (SUMA_ERROR_DSET_TYPE);
}

char * SUMA_Col_Type_Name (SUMA_COL_TYPE tp)
{
   static char FuncName[]={"SUMA_Col_Type_Name"};
   
   SUMA_ENTRY;
   
   switch (tp) {
      case SUMA_NO_COL_TYPE:
         SUMA_RETURN("Col_Type_Undefined");
         break;
      case SUMA_ERROR_COL_TYPE:
         SUMA_RETURN ("Error_Col_Type");
         break;
      case SUMA_NODE_INT:
         SUMA_RETURN("Generic_Int");
         break;
      case SUMA_NODE_INDEX:
         SUMA_RETURN("Node_Index");
         break;
      case SUMA_NODE_ILABEL:
         SUMA_RETURN("Node_Index_Label");
         break;
      case SUMA_NODE_FLOAT:
         SUMA_RETURN("Generic_Float");
         break;
      case SUMA_NODE_SHORT:
         SUMA_RETURN("Generic_Short");
         break;
      case SUMA_NODE_3C:
         SUMA_RETURN("XYZ_triplets");
         break;
      case SUMA_NODE_X:
         SUMA_RETURN("X_coord");
         break;
      case SUMA_NODE_Y:
         SUMA_RETURN("Y_coord");
         break;
      case SUMA_NODE_Z:
         SUMA_RETURN("Z_coord");
         break;
      case SUMA_NODE_R:
         SUMA_RETURN("R_col");
         break;
      case SUMA_NODE_G:
         SUMA_RETURN("G_col");
         break;
      case SUMA_NODE_B:
         SUMA_RETURN("B_col");
         break;
      case SUMA_NODE_STRING:
         SUMA_RETURN("Generic_String");
         break;
      case SUMA_NODE_CX:
         SUMA_RETURN("Convexity");
         break;
      case SUMA_NODE_BYTE:
         SUMA_RETURN("Generic_Byte");
         break;
      case SUMA_NODE_DOUBLE:
         SUMA_RETURN("Generic_Double");
         break;
      case SUMA_NODE_XCORR:
         SUMA_RETURN("Cross_Corr_Coeff");
         break;
      default:
         SUMA_RETURN("Cowabonga-Jo");
         break;
   }
   
}

/*!
   For daily use, call       ctp = SUMA_TypeOfColNumb(nel, i); 

*/
SUMA_COL_TYPE SUMA_Col_Type (char *Name)
{
   static char FuncName[]={"SUMA_Col_Type"};
   
   SUMA_ENTRY;
   if (!Name)  { SUMA_SL_Err("NULL Name");  SUMA_RETURN (SUMA_ERROR_COL_TYPE); }
   if (!strcmp(Name,"Col_Type_Undefined")) SUMA_RETURN (SUMA_NO_COL_TYPE);
   if (!strcmp(Name,"Error_Col_Type")) SUMA_RETURN (SUMA_ERROR_COL_TYPE);
   if (!strcmp(Name,"Generic_Int")) SUMA_RETURN (SUMA_NODE_INT);
   if (!strcmp(Name,"Generic_Short")) SUMA_RETURN (SUMA_NODE_SHORT);
   if (!strcmp(Name,"Node_Index")) SUMA_RETURN (SUMA_NODE_INDEX);
   if (!strcmp(Name,"Node_Index_Label")) SUMA_RETURN (SUMA_NODE_ILABEL);
   if (!strcmp(Name,"Generic_Float")) SUMA_RETURN (SUMA_NODE_FLOAT);
   if (!strcmp(Name,"XYZ_triplets")) SUMA_RETURN (SUMA_NODE_3C);
   if (!strcmp(Name,"X_coord")) SUMA_RETURN (SUMA_NODE_X);
   if (!strcmp(Name,"Y_coord")) SUMA_RETURN (SUMA_NODE_Y);
   if (!strcmp(Name,"Z_coord")) SUMA_RETURN (SUMA_NODE_Z);
   if (!strcmp(Name,"R_col")) SUMA_RETURN (SUMA_NODE_R);
   if (!strcmp(Name,"G_col")) SUMA_RETURN (SUMA_NODE_G);
   if (!strcmp(Name,"B_col")) SUMA_RETURN (SUMA_NODE_B);
   if (!strcmp(Name,"Generic_String")) SUMA_RETURN (SUMA_NODE_STRING);
   if (!strcmp(Name,"Generic_Byte")) SUMA_RETURN (SUMA_NODE_BYTE);
   if (!strcmp(Name,"Generic_Double")) SUMA_RETURN (SUMA_NODE_DOUBLE);
   if (!strcmp(Name,"Convexity")) SUMA_RETURN (SUMA_NODE_CX);
   if (!strcmp(Name,"Cross_Corr_Coeff")) SUMA_RETURN (SUMA_NODE_XCORR);
   /* if (!strcmp(Name,"")) SUMA_RETURN (); */
   SUMA_RETURN (SUMA_ERROR_COL_TYPE);

}


int SUMA_ShowNel (void *nel)
{
   static char FuncName[]={"SUMA_ShowNel"};
   NI_stream nstdout;
   NI_element *el=NULL;
   
   SUMA_ENTRY;
   
   nstdout = NI_stream_open( "fd:1","w");
   if( nstdout == NULL ){ 
      fprintf(stderr,"%s: Can't open fd:1\n", FuncName); 
      SUMA_RETURN(0); 
   }
   fprintf (stdout, "\n***********nel extra info ************\n");
   el = (NI_element *)nel;
   if (el->type == NI_ELEMENT_TYPE) {
      fprintf (stdout,  "\n    Element type.\n"
                        "      vec_len   = %d\n"
                        "      vec_num   = %d\n"
                        "      vec_filled= %d\n", el->vec_len, el->vec_num, el->vec_filled);
   } else {
      fprintf (stdout,  "\n    Group type.\n");
   } 
   fprintf (stdout, "\n***********nel stdout begin***********\n");
   NI_write_element( nstdout , nel , NI_TEXT_MODE ) ;
   fprintf (stdout, "\n***********nel stdout end  ***********\n");
   NI_stream_close(nstdout);
   
   SUMA_RETURN(1);
}


int *SUMA_GetDsetColIndex (SUMA_DSET *dset, SUMA_COL_TYPE tp, int *N_i)
{
   static char FuncName[]={"SUMA_GetDsetColIndex"};
   int *iv=NULL, i=0;
   char stmp[500], *atr;
   int ctp;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (tp == SUMA_NODE_INDEX) {
      SUMA_S_Err("Function cannot be called for column type SUMA_NODE_INDEX");
      SUMA_RETURN(NULL);
   }
   
   if (!dset || !dset->dnel) { SUMA_SL_Err ("NULL input"); SUMA_RETURN(NULL); }
   *N_i = -1;
   iv = (int *)SUMA_calloc(SDSET_VECNUM(dset), sizeof(int));
   if (!iv) {
      SUMA_RETURN(NULL);
   }   
   
   *N_i = 0;
   for (i=0; i < SDSET_VECNUM(dset); ++i) {
      ctp = SUMA_TypeOfDsetColNumb(dset, i);
      if (ctp == tp) {
         iv[*N_i] = i;
         *N_i = *N_i + 1;
      }
   }
   
   if (!*N_i) { SUMA_free(iv); iv = NULL; }
   SUMA_RETURN(iv);
}

int *SUMA_GetColIndex (NI_element *nel, SUMA_COL_TYPE tp, int *N_i)
{
   static char FuncName[]={"SUMA_GetColIndex"};
   int *iv=NULL, i=0;
   char stmp[500], *atr;
   int ctp;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_SL_Warn("Obsolete, use new version.");

   if (!nel) { SUMA_SL_Err ("NULL nel"); SUMA_RETURN(NULL); }
   *N_i = -1;
   iv = (int *)SUMA_calloc(nel->vec_num, sizeof(int));
   if (!iv) {
      SUMA_RETURN(NULL);
   }   
   
   *N_i = 0;
   for (i=0; i < nel->vec_num; ++i) {
      ctp = SUMA_TypeOfColNumb(nel, i);
      if (ctp == tp) {
         iv[*N_i] = i;
         *N_i = *N_i + 1;
      }
   }
   
   if (!*N_i) { SUMA_free(iv); iv = NULL; }
   SUMA_RETURN(iv);
}

/*!
   \brief returns a string with a history note
   \param CallingFunc (char *) name of function / program calling
   \param N_arg (int) number of arguments in arg
   \param arg (char **) vector of strings 
   \param sold (char *) old history note
   \return histoire (char *) 
*/

char *SUMA_HistString (char *CallingFunc, int N_arg, char **arg, char *sold)
{
   static char FuncName[]={"SUMA_HistString"}; 
   char *stmp=NULL, *ch=NULL, *chold = NULL, *cdate=NULL, *cname=NULL, *cuser=NULL, *cn = NULL;
   int idate , iname , iuser ;
   int N_tot, i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!arg) SUMA_RETURN(NULL);
   if (!arg[0]) SUMA_RETURN(NULL);
   if (!N_arg) SUMA_RETURN(NULL);
   
   #if 0
      if (sold) stmp = SUMA_append_string (sold, "\n");

      if (CallingFunc) {
         stmp = SUMA_append_replace_string (stmp, CallingFunc, "",1);
         stmp = SUMA_append_replace_string (stmp, ":", " ", 1);
      }

      for (i=0; i < N_arg; ++i) 
         stmp = SUMA_append_replace_string (stmp, arg[i], " ", 1);
   #else
      /* Based on tross_Make_History and tross_Append_History */
      if (LocalHead) fprintf(SUMA_STDERR,"CF: %s, N_arg=%d\n", CallingFunc, N_arg);
      cn = tross_commandline( CallingFunc, N_arg, arg);
      if (LocalHead) fprintf(SUMA_STDERR,"cn: %s\n", cn);
      cdate = tross_datetime() ; idate = strlen(cdate) ;
      cname = tross_hostname() ; iname = strlen(cname) ;  
      cuser = tross_username() ; iuser = strlen(cuser) ;  
      if (sold) { /* sold is already trossized */
         chold = tross_Expand_String(sold) ; if( chold == NULL ) SUMA_RETURN(NULL) ;
         chold = AFREALL( chold, char, 
		               strlen(chold)+idate+iuser+iname+strlen(cn)+12 ) ;

         strcat(chold,"\n") ;
         strcat(chold,"[") ;  strcat(chold,cuser) ; strcat(chold,"@") ;
                              strcat(chold,cname) ; strcat(chold,": ") ;
                              strcat(chold,cdate) ;
         strcat(chold,"] ") ;
         strcat(chold,cn) ;
         SUMA_LH(chold);
         ch = tross_Encode_String(chold) ; if( ch == NULL ){ free(chold); SUMA_RETURN(NULL); }
         stmp = SUMA_copy_string(ch);
         free(chold) ; free(ch);
      } else {
         chold = AFMALL(char, idate+iuser+iname+strlen(cn)+12 ) ;
         sprintf(chold,"[%s@%s: %s] %s",cuser,cname,cdate,cn) ;
         SUMA_LH(chold);
         ch = tross_Encode_String(chold) ; if( ch == NULL ){ free(chold); SUMA_RETURN(NULL); }
         stmp = SUMA_copy_string(ch);
         free(chold) ;  free(ch); 
      }
   #endif   
   SUMA_RETURN(stmp);
}   

/*! 
   \brief returns pointer to history string.
   DO NOT FREE THIS POINTER, it is a copy
   of the pointer in one of ngr's elements.
   NULL if no history or no history elements 
   were found
*/
char * SUMA_GetNgrHist(NI_group *ngr)
{
   static char FuncName[]={"SUMA_GetNgrHist"};
   char **sv, *shist = NULL;
   NI_element *nelb = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   nelb = SUMA_FindNgrAttributeElement(ngr, "HISTORY_NOTE");
   if (nelb) {
      sv = (char **)nelb->vec[0];   
      shist = (char *)sv[0];   
   } else {
      shist = NULL;
   }
   SUMA_RETURN(shist);
}

/*!
   \brief adds a history element note to the ni-group
   
   \param dset (NI_group *)
   \param CallingFunc (char *) name of function / program calling
   \param N_arg (int) number of arguments in arg
   \param arg (char **) vector of strings 
   \return ans (int) 0 Failed
                     1 OK 
*/
int SUMA_AddNgrHist(NI_group *ngr, char *CallingFunc, int N_arg, char **arg)
{
   static char FuncName[]={"SUMA_AddNgrHist"}; 
   char *stmp=NULL, *sold=NULL, **sv=NULL;
   int N_tot, i;
   NI_element *nelb = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!arg) SUMA_RETURN(0);
   if (!arg[0]) SUMA_RETURN(0);
   if (!ngr) SUMA_RETURN(0);
   if (!N_arg) SUMA_RETURN(0);
   
   /* get former history element, if any and old string */
   nelb = SUMA_FindNgrAttributeElement(ngr, "HISTORY_NOTE");
   if (nelb) {
      sv = (char **)nelb->vec[0];   
      sold = (char *)sv[0];   
   } else {
      sold = NULL;
   }
  
   /* form the new string */
   stmp = SUMA_HistString (CallingFunc, N_arg, arg, sold);
   
   
   if (stmp) {
      if (nelb) { /* element existed */
         /* now need to replace the old string with the new one */
         if (sold) { /* clean the old pos */
            NI_free(sold); sold = sv[0] = NULL;
         }
      } else { /* element never existed */
         nelb = NI_new_data_element("AFNI_atr", 1);
         NI_set_attribute(nelb, "atr_name", "HISTORY_NOTE");
         NI_add_column_stride ( nelb, NI_STRING, NULL, 1 );
         NI_add_to_group(ngr, nelb);
      }
      /* now add the new string */
      SUMA_NEL_REPLACE_STRING(nelb, 0, 0, (void*)stmp);
      SUMA_free(stmp); stmp = NULL;
   } else {
      SUMA_SL_Err("Failed to create string!");
      SUMA_RETURN(0);
   }
   
   SUMA_RETURN(1);
}

/*!
   \brief adds a history note to the ni-element
   
   \param nel (NI_element *)
   \param CallingFunc (char *) name of function / program calling
   \param N_arg (int) number of arguments in arg
   \param arg (char **) vector of strings 
   \return ans (int) 0 Failed
                     1 OK 
*/
int SUMA_AddNelHist(NI_element *nel, char *CallingFunc, int N_arg, char **arg)
{
   static char FuncName[]={"SUMA_AddNelHist"}; 
   char *stmp=NULL, *sold=NULL;
   int N_tot, i;
   
   SUMA_ENTRY;
   
   SUMA_SL_Warn("Obsolete, use new version.");

   if (!arg) SUMA_RETURN(0);
   if (!arg[0]) SUMA_RETURN(0);
   if (!nel) SUMA_RETURN(0);
   if (!N_arg) SUMA_RETURN(0);
   
   sold = NI_get_attribute(nel, "HISTORY_NOTE");
   stmp = SUMA_HistString (CallingFunc, N_arg, arg, sold);
   
  if (stmp) {
      NI_set_attribute ( nel, "HISTORY_NOTE", stmp);
      SUMA_free(stmp);
   }
   
   SUMA_RETURN(1);
}


      

/*** 
   Functions to deal with SUMA's datasets
   They don't quite belong in SUMA_DataSets.c because
   they are specific to SUMA
***/

/*!
   \brief look for a dataset with a particular idcode
*/
SUMA_DSET * SUMA_FindDset_ns (char *idcode, DList *DsetList)
{
   SUMA_DSET *dset = SUMA_FindDset_eng (idcode, DsetList);
   WorkErrLog_ns();
   return(dset);
}

SUMA_DSET * SUMA_FindDset_eng (char *idcode, DList *DsetList)
{
   static char FuncName[]={"SUMA_FindDset_eng"};
   SUMA_DSET *dset = NULL, *dsetf = NULL;
   char *dsetid;
   DListElmt *el=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
    
   dsetf = NULL;
   if (!DsetList) { SUMA_SL_Err("NULL DsetList"); SUMA_RETURN(dsetf); }
   if (!DsetList->size) { SUMA_RETURN(dsetf); }
   if (!idcode) { SUMA_SL_Err("NULL idcode"); SUMA_RETURN(dsetf); }
   el = NULL;
   do { 
      if (!el) el = dlist_head(DsetList);
      else el = dlist_next(el);
      dset = (SUMA_DSET *)el->data;
      if (!dset) {
         SUMA_PushErrLog("SLP_Err", 
                        "Unexpected NULL dset element in list!\nPlease report this occurrence to saadz@mail.nih.gov.",
                        FuncName);
      } else {   
         #ifdef OLD_DSET      /* before dsets were NI_groups */
         if (dset->nel) {
            dsetid = NI_get_attribute(dset->nel, "idcode"); /* obsolete */
            if (!dsetid) dsetid = NI_get_attribute(dset->nel, "self_idcode");
            if (dsetid) {
               if (!strcmp(dsetid, idcode))  dsetf = dset; /* match */
            } 
         }
         #else 
         if (dset->ngr) {
            dsetid = NI_get_attribute(dset->ngr, "idcode"); /* obsolete */
            if (!dsetid) dsetid = NI_get_attribute(dset->ngr, "self_idcode");
            if (dsetid) {
               if (!strcmp(dsetid, idcode))  dsetf = dset; /* match */
            } 
         }
         #endif
      } 
   } while ( (el != dlist_tail(DsetList)) && !dsetf ); 
   SUMA_RETURN(dsetf);
}

/*!
   \brief Function to free a Dset 
   
   - YOU SHOULD NOT FREE individual dsets yourself
   That is done by the dlist_destroy function
*/
void SUMA_FreeDset(void *vp)
{
   static char FuncName[]={"SUMA_FreeDset"};
   int i;
   SUMA_DSET *dset;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   dset = (SUMA_DSET *)vp;
   
   if (!dset) SUMA_RETURNe;
   if (dset->N_links) {
      SUMA_SL_Err("dset structure has links to it.\n"
                  "structure not freed.\n"
                  "That is a now a memory leak.\n");
      SUMA_ShowDset (dset, 0, NULL);            
      SUMA_RETURNe;
   }
   
   #ifdef OLD_DSET /* before ngr was used */
   if (dset->nel) NI_free_element(dset->nel); dset->nel = NULL; /* you can keep ni_free from freeing a nel->vec[i] 
                                                          vector by copying nel->vec[i] to a pointer then
                                                          setting nel->vec[i] = NULL */ 
   #else
   dset->dnel = NULL; /* this one was a pointer copy to an element inside ngr */
   dset->inel = NULL;
   if (dset->ngr) NI_free_element(dset->ngr); dset->ngr = NULL; /* you can keep ni_free from freeing a nel->vec[i] 
                                                          vector by copying nel->vec[i] to a pointer then
                                                          setting nel->vec[i] = NULL */ 
   #endif
   #if 0
   if (dset->filename) SUMA_free(dset->filename);
   if (dset->Label) SUMA_free(dset->Label);
   if (dset->idcode) SUMA_free(dset->idcode);
   if (dset->domain_idcode) SUMA_free(dset->domain_idcode);
   if (dset->NodeDef) SUMA_free(dset->NodeDef);
   if (dset->N_mx) SUMA_free(dset->N_mx);
   if (dset->data) {
      for (i=0; i<dset->N_sub; ++i) {
         if (dset->data[i]) SUMA_free(dset->data[i]);
      }
   }
   if (dset->tp) SUMA_free(dset->tp);
   #endif
   
   SUMA_free(dset); dset = NULL;
   
   SUMA_RETURNe;
}

/*!
   \brief Pointer linking and unliking functions.
   They are to be used for all pointer linking
   in SUMA. For now they are just used for the datasets
   but I plan to move all "inode" stuff in here.
   
   These functions are generalized versions of SUMA_LinkToDset
   and SUMA_UnlinkFromDset
*/
void *SUMA_LinkToPointer(void *ptr)
{
   static char FuncName[]={"SUMA_LinkToPointer"};
   SUMA_LinkedPtr *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ptr) {
      SUMA_SL_Err("NULL pointer");
      SUMA_RETURN(NULL);
   }
   dset = (SUMA_LinkedPtr *)ptr;
   if (LocalHead) fprintf(SUMA_STDERR,"%s:\n Link Requested to pointer %p. \n"
                                      "LinkedPtrType = %d, owner_id = %s\n"
                                      "N_links was %d\n", 
                                      FuncName, dset, dset->LinkedPtrType, dset->owner_id, dset->N_links);
   dset->N_links = dset->N_links + 1;
   
   SUMA_RETURN((void *)dset);
}
void *SUMA_UnlinkFromPointer(void *ptr)
{
   static char FuncName[]={"SUMA_UnlinkFromPointer"};
   SUMA_LinkedPtr *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!ptr) {
      SUMA_SL_Err("NULL pointer");
      SUMA_RETURN(NULL);
   }
   dset = (SUMA_LinkedPtr *)ptr;
   if (LocalHead) fprintf(SUMA_STDERR, "%s:\n Unlink Requested from pointer %p.\n"
                                       "LinkedPtrType = %d, owner_id = %s\n"
                                       "N_links was %d\n", 
                                       FuncName, dset, dset->LinkedPtrType, dset->owner_id, dset->N_links);
   if (dset->N_links > 0) dset->N_links = dset->N_links - 1;
   else if (dset->N_links == 0) { SUMA_SL_Err("N_links ==0\nThis should not happen here.\n");   SUMA_RETURN(NULL); }
   
   SUMA_RETURN(NULL);
}
#if 0
SUMA_DSET *SUMA_LinkToDset(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_LinkToDset"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s:\n Link Requested to dset %p. N_links was %d\n", FuncName, dset, dset->N_links);
   dset->N_links = dset->N_links + 1;
   
   SUMA_RETURN(dset);
}
SUMA_DSET *SUMA_UnlinkFromDset(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_UnlinkFromDset"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s:\n Unink Requested from dset %p. N_links was %d\n", FuncName, dset, dset->N_links);
   if (dset->N_links > 0) dset->N_links = dset->N_links - 1;
   else if (dset->N_links == 0) { SUMA_SL_Err("N_links ==0\nThis should not happen here.\n");   SUMA_RETURN(NULL); }
   
   SUMA_RETURN(NULL);
}
#endif

SUMA_DSET * SUMA_NewDsetPointer(void)
{
   static char FuncName[]={"SUMA_NewDsetPointer"};
   SUMA_DSET *dset = NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;

   dset = (SUMA_DSET *)SUMA_malloc(sizeof(SUMA_DSET));
   if (!dset) {
      SUMA_SL_Err("Failed to allocate for dset");
      SUMA_RETURN(dset);
   }
   if (LocalHead) fprintf(SUMA_STDERR,"%s:\n dset %p allocated.\n", FuncName, dset);
   /* initialize */
   #ifdef OLD_DSET
   dset->nel = NULL;
   #else
   dset->inel = NULL;
   dset->dnel = NULL;
   dset->ngr = NULL;
   #endif
   dset->N_links = 0;
   dset->owner_id[0] = '\0';
   dset->LinkedPtrType = SUMA_LINKED_DSET_TYPE;
   SUMA_RETURN(dset);
}

/*!
   \brief a function to put a ni_group (ngr) element into a SUMA_DSET pointer
   To free dset along with the ngr in it use SUMA_FreeDset.
   To preserve ngr:
      ngr_keep = dset->ngr; dset->ngr = NULL; SUMA_Free(dset);
*/
SUMA_DSET * SUMA_ngr_2_dset(NI_group *nini)
{
   static char FuncName[]={"SUMA_ngr_2_dset"};
   SUMA_DSET *dset=NULL;
   char *dname = NULL;
   
   SUMA_ENTRY;

   if (!(dset = SUMA_NewDsetPointer())) {
      SUMA_SL_Err("Failed to create dset pointer");
      SUMA_RETURN(NULL);
   }

   dset->ngr = (NI_group *)nini; nini = NULL;
   dset->dnel = SUMA_FindDsetDataElement(dset);
   dset->inel = SUMA_FindDsetNodeIndexElement(dset);
   if (!dset->dnel) {
      SUMA_SL_Warn("Failed to find dset data element");
   } 
   if (!dset->inel || !SDSET_NODEINDNUM(dset)) {
      /* If a dset had an empty node index holder at write time,
      the inel is written with vecnum set to 0. This would make
      any attempt to add columns later on fail.
      */
      SUMA_S_Note("NIML dset with no valid node index element");
      NI_remove_from_group(dset->ngr, dset->inel);
      NI_free(dset->inel); dset->inel = NULL;
      /* Now add the new and proper node index element holder*/
      if (dset->dnel) {
         SUMA_S_Note("Adding empty holder");
         dname = SUMA_append_string(
               NEL_DSET_TYPE(dset->ngr), "_node_indices");
         dset->inel = NI_new_data_element("INDEX_LIST", SDSET_VECLEN(dset)); 
         NI_set_attribute (dset->inel, "data_type", dname); 
         SUMA_free(dname); dname = NULL;
         NI_add_to_group(dset->ngr, dset->inel);
      }
   }
      

   SUMA_RETURN(dset);
}  

/*!
   \brief assign a label to a dset
   pass NULL for lbl if you want the function to guess at creating one

   Note: Label Length is truncated to 20 chars
*/
SUMA_Boolean SUMA_LabelDset(SUMA_DSET *dset, char *lbl)
{
   static char FuncName[]={"SUMA_LabelDset"};
   char *Label=NULL, *tmp=NULL;
   SUMA_PARSED_NAME *pn=NULL;
   SUMA_Boolean ok;
   
   SUMA_ENTRY;
   
   ok = YUP; 
   if (lbl) {
      Label = SUMA_truncate_string(lbl, 20); 
   } else if ( (tmp = NI_get_attribute(dset->ngr,"filename")) ) {
      pn = SUMA_ParseFname(tmp, NULL);
      if (pn) {
         Label = SUMA_truncate_string(pn->FileName_NoExt, 20);
         SUMA_Free_Parsed_Name(pn); pn = NULL;
      } else {
         NI_set_attribute(dset->ngr, "label", "Bad No label"); 
         ok = NOPE;
      }
   } else {
      NI_set_attribute(dset->ngr, "label", "No label"); 
      ok = NOPE;
   }
   SUMA_free(Label); Label = NULL;
   
   SUMA_RETURN(ok);
}   

/*!
   Rename a dataset and take care of idcode and relabeling, if necessary 
*/
SUMA_Boolean SUMA_RenameDset(SUMA_DSET *dset, char *filename) 
{
   static char FuncName[]={"SUMA_RenameDset"};
   char *ofname=NULL, *ofnameid = NULL, *olabel = NULL;
   char  *fnameid=NULL, *label=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!filename) {
      SUMA_S_Err("No filename");
      SUMA_RETURN(NOPE);
   }
   
   /* Do we have file name already ? */
   if (!(ofname = SUMA_copy_string(NI_get_attribute(dset->ngr,"filename")))) {
      ofname = SUMA_copy_string(filename); /* old is new */
   }
   
   /* put the new name in */
   NI_set_attribute(dset->ngr, "filename", filename);
   
   /* what would the new id be based on the new name? */
   SUMA_NEW_ID(fnameid, filename);
   /* what would the old id be based on the olde name? */
   SUMA_NEW_ID(ofnameid, ofname);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"Old: %s, %s\n"
                          "New: %s, %s\n", 
                          ofnameid, ofname,
                          fnameid, filename);
   }
   /* was the olde id based on the old fname? */
   if (SDSET_ID(dset)) {
      if (strcmp(SDSET_ID(dset), ofnameid) == 0) {
         SUMA_LH("Id based on old name");
         /* need to recreate id based on new name*/
         NI_set_attribute (dset->ngr, "self_idcode", fnameid);
      } else { /* id was not based on name, do nothing */
         SUMA_LH("Id not based on old name");
      }
   } else {
      SUMA_S_Warn("dset with no id, what gives?");
   }
   
   /* what about the label ? */
   if ((olabel = SDSET_LABEL(dset))) {
      /* have label already */
      if (strstr(ofname, olabel)) { /* old label was based on old file name, relabel with new filename */
         SUMA_LabelDset(dset, filename);
      }
   } else { /* no label, put new one in */
      SUMA_LabelDset(dset, filename);
   }
   
   if (fnameid) SUMA_free(fnameid); fnameid = NULL;
   if (ofnameid) SUMA_free(ofnameid); ofnameid = NULL;
   if (ofname) SUMA_free(ofname); ofname = NULL;
   
   SUMA_RETURN(YUP);
}

/*!
   \brief Function to allocate and initialize a dataset and add 
   it to the list of data sets .
   
   dset = SUMA_CreateDsetPointer (
                              char *filename, char *idcode,
                              char *domain_idcode, int N_Alloc
                              ) ;    
   \param filename (char *): Name of dset, typically, filename with path
   \param idcode (char *): identifier to use for dset.
                               If idcode is NULL then a new one is
                               generated from filename.
   \param domain_idcode(char *): idcode of domain. (used for both MeshDomain 
                              and geometry domain)
   \return dset (SUMA_DSET *): Element of DsetList containing dataset that 
                              was created by the function.

   -  This function does the following:
      xxxxxxx
      
   \sa SUMA_AddNelCol
   \sa SUMA_InsertDsetPointer
   \sa SUMA_NewDsetPointer
*/
SUMA_DSET * SUMA_CreateDsetPointer (  
                              char *filename, SUMA_DSET_TYPE tp,
                              char *idcode,
                              char *domain_idcode,
                              int N_Alloc 
                              ) 
{
   static char FuncName[]={"SUMA_CreateDsetPointer"};
   int ilist = -1, i = -1;
   char  *locid=NULL;
   SUMA_DSET *dset=NULL;
   DListElmt *Elm = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   #if 0  /* not required anymore */
      if (!filename) { SUMA_SL_Err("Need Dset filename"); SUMA_RETURN(dset); }
   #endif
   #if 0
   if (N_Alloc != N_NodeDef) {
      SUMA_SL_Err("Not ready to deal with N_Alloc != N_NodeDef");
      SUMA_RETURN(dset);
   }
   #endif
   
   if (!idcode) { /* No id is given yet */
      if (filename) {
         SUMA_NEW_ID(locid, filename);   /* form one from the filename */
         if (LocalHead) fprintf(SUMA_STDERR,"%s: Using filename %s to create IDcode %s.\n", FuncName, filename, locid); 
      } else {
         SUMA_NEW_ID(locid, NULL); 
      } 
   }else {
      locid = SUMA_copy_string(idcode);
   }
   
   dset = SUMA_NewDsetPointer();
   if (!SUMA_NewDsetGrp (dset, tp, domain_idcode, domain_idcode, N_Alloc, filename, locid)) {
      SUMA_SL_Crit("Failed to create dset.\n");
      SUMA_RETURN(0);
   }

   SUMA_LabelDset(dset, filename);
   
   if (locid) SUMA_free(locid); locid = NULL;
   SUMA_RETURN(dset);
}    

/*!
   \brief inserts a dataset pointer into the DsetList, 
   if *dset to be inserted has the same ID as a pre-existing one (old_dset) and replace is 1, then:
      the contents of old_dset are freed
      the contents of dset are copied into old_dset
      the pointers of dset are set to null
      *dset is freed
      *dset is set to old_dset 
   with those of the new one. The value of dsetp is set to the one found in the list.
   
   \param dset (SUMA_DSET **)
   \param DsetList (DList *): List of dset objects.
*/   
int SUMA_InsertDsetPointer (SUMA_DSET **dsetp, DList *DsetList, int replace)
{
   static char FuncName[]={"SUMA_InsertDsetPointer"};
   char *s=NULL, stmp[200];
   SUMA_DSET *dprev=NULL, *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!DsetList)  { SUMA_SL_Err("Need Dset List"); SUMA_RETURN(0); }
   if (!dsetp) { SUMA_SL_Err("dsetp is NULL"); SUMA_RETURN(0); }
   else dset = *dsetp;  /* dset is the new pointer */
   
   if (!dset->dnel) { SUMA_SL_Err("dset->nel is NULL\nNothing to do"); SUMA_RETURN(0); }

   s= SDSET_ID(dset); if (!s) { SUMA_SL_Err("dset has no idcode.\n"); SUMA_RETURN(0); }
   if ((dprev = SUMA_FindDset_ns (s,  DsetList))) {
      SUMA_LH("Dset exists");
      if (replace) {
         SUMA_LH("Replacing");
         sprintf(stmp,  "Dset with similar idcode (%s)\n"
                        "found in list. Trying replacement.\n", s);
         SUMA_SL_Note(stmp);
         #ifdef OLD_DSET /* before ngr was used */
         if (dprev->nel) NI_free_element(dprev->nel); dprev->nel = NULL; 
         dprev->nel = dset->nel;
         dset->nel = NULL;
         #else
         dprev->inel = NULL; 
         dprev->dnel = NULL; 
         if (dprev->ngr) NI_free_element(dprev->ngr); dprev->ngr = NULL; 
         dprev->ngr = dset->ngr;
         dprev->dnel = dset->dnel;
         dprev->inel = dset->inel;
         /* set dset's pointers to null */
         dset->ngr = NULL;
         dset->dnel = NULL;
         dset->inel = NULL;
         #endif
         SUMA_free(dset);
         /* now make the switch so that on return, dset becomes the previous dset*/
         *dsetp = dprev;
      } else {
         SUMA_LH("Not Replacing");
         sprintf(stmp,  "Dset with similar idcode (%s)\n"
                        "found in list. \n"
                        "Replacement option is turned off.\n"
                        "Set 'SUMA_AllowDsetReplacement = YES'\n"
                        "in ~/.sumarc to allow replacement.\n", s);
         SUMA_SL_Err(stmp);
         SUMA_RETURN(0);
      }
   } else {
      SUMA_LH("New dset, inserting");
      /* insert into list */
      if (dlist_ins_next(DsetList, dlist_tail(DsetList), (void *)dset) < 0) {
         SUMA_SL_Err("Failed to insert dset into list");
         SUMA_FreeDset(dset); dset = NULL; *dsetp = NULL;
         SUMA_RETURN(0);
      }
   }
   
   SUMA_RETURN(1);
}

char *SUMA_ShowMeSome (void *dt, SUMA_VARTYPE tp, int N_dt, int mxshow, char *title)
{
   static char FuncName[]={"SUMA_ShowMeSome"};
   int i, imx, firsthalf, secondhalf;
   double *dtd;
   int *dti;
   byte *dtb;
   char **dts;
   float *dtf;
   char *s=NULL;
   complex *dtc = NULL;
   SUMA_STRING *SS=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (mxshow > N_dt) mxshow = N_dt;
   
   if (mxshow < 0) SUMA_RETURN(s);
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (title) {
      SS = SUMA_StringAppend_va(SS, "%s", title);
   }
   
   firsthalf = mxshow / 2;
   secondhalf = mxshow - firsthalf;
   if (LocalHead) fprintf(SUMA_STDERR,"%s: tp=%d, SUMA_double=%d, SUMA_float=%d, SUMA_int=%d, SUMA_byte=%d, SUMA_short=%d\n", 
                           FuncName, tp, SUMA_double, SUMA_float, SUMA_int, SUMA_byte, SUMA_short);
   if (mxshow) {
      switch (tp) {
         case SUMA_double:
            dtd = (double*)dt;
            for (i=0; i < firsthalf; ++i) SS = SUMA_StringAppend_va(SS, "%lf, ", dtd[i]);
            if (mxshow < N_dt) SS = SUMA_StringAppend_va(SS, "..., ");
            if (secondhalf > 1) { for (i=SUMA_MAX_PAIR(N_dt-secondhalf, firsthalf); i<N_dt-1; ++i) SS = SUMA_StringAppend_va(SS, "%lf, ", dtd[i]); }
            SS = SUMA_StringAppend_va(SS, "%lf", dtd[N_dt-1]);
            break;
         case SUMA_float:
            dtf = (float*)dt;
            for (i=0; i < firsthalf; ++i) SS = SUMA_StringAppend_va(SS, "%f, ", dtf[i]);
            if (mxshow < N_dt) SS = SUMA_StringAppend_va(SS, "..., ");
            if (secondhalf > 1) { for (i=SUMA_MAX_PAIR(N_dt-secondhalf, firsthalf); i<N_dt-1; ++i) SS = SUMA_StringAppend_va(SS, "%f, ", dtf[i]); }
            SS = SUMA_StringAppend_va(SS, "%f", dtf[N_dt-1]);
            break;
         case SUMA_int:
            dti = (int*)dt;
            for (i=0; i < firsthalf; ++i) SS = SUMA_StringAppend_va(SS, "%d, ", dti[i]);
            if (mxshow < N_dt) SS = SUMA_StringAppend_va(SS, "..., ");
            if (secondhalf > 1) { for (i=SUMA_MAX_PAIR(N_dt-secondhalf, firsthalf); i<N_dt-1; ++i) SS = SUMA_StringAppend_va(SS, "%d, ", dti[i]); }
            SS = SUMA_StringAppend_va(SS, "%d", dti[N_dt-1]);
            break;
         case SUMA_byte:
            dtb = (byte*)dt;
            for (i=0; i < firsthalf; ++i) SS = SUMA_StringAppend_va(SS, "%d, ", dtb[i]);
            if (mxshow < N_dt) SS = SUMA_StringAppend_va(SS, "..., ");
            if (secondhalf > 1) { for (i=SUMA_MAX_PAIR(N_dt-secondhalf, firsthalf); i<N_dt-1; ++i) SS = SUMA_StringAppend_va(SS, "%d, ", dtb[i]); }
            SS = SUMA_StringAppend_va(SS, "%d", dtb[N_dt-1]);
            break;
         case SUMA_string:
            dts = (char **)dt;
            for (i=0; i < firsthalf; ++i) SS = SUMA_StringAppend_va(SS, "%s, ", dts[i]);
            if (mxshow < N_dt) SS = SUMA_StringAppend_va(SS, "..., ");
            if (secondhalf > 1) { for (i=SUMA_MAX_PAIR(N_dt-secondhalf, firsthalf); i<N_dt-1; ++i) SS = SUMA_StringAppend_va(SS, "%s, ", dts[i]); }
            SS = SUMA_StringAppend_va(SS, "%s", dts[N_dt-1]);
            break;
         case SUMA_complex:
            dtc = (complex *)dt; 
            for (i=0; i < firsthalf; ++i) SS = SUMA_StringAppend_va(SS, "(%f %+fi), ", dtc[i].r, dtc[i].i);
            if (mxshow < N_dt) SS = SUMA_StringAppend_va(SS, "..., ");
            if (secondhalf > 1) { for (i=SUMA_MAX_PAIR(N_dt-secondhalf, firsthalf); i<N_dt-1; ++i) SS = SUMA_StringAppend_va(SS, "(%f %+fi), ", dtc[i].r, dtc[i].i); }
            SS = SUMA_StringAppend_va(SS, "(%f %+fi)", dtc[N_dt-1].r, dtc[N_dt-1].i);
            break;
         default:
            SS = SUMA_StringAppend_va(SS, "Type not supported.");
      }
   } else {
      SS = SUMA_StringAppend_va(SS, "Empty vector.");
   }  

   SUMA_SS2S(SS,s);
   
   SUMA_RETURN(s);
}

void SUMA_ShowDset (SUMA_DSET *dset, int detail, FILE *out)
{
   static char FuncName[]={"SUMA_ShowDset"};
   char *si = NULL;
   
   SUMA_ENTRY;
   
   if (!out) out = SUMA_STDERR;
   
   si = SUMA_DsetInfo(dset, detail);
   
   fprintf(out,"%s\n", si);
   
   if (si) SUMA_free(si); si = NULL;
   
   SUMA_RETURNe;
   
}
/*!
   \brief Function to return info on SUMA_DSET
   
   - You must free the returned string on your own
   \sa SUMA_ShowDset
*/
char *SUMA_DsetInfo (SUMA_DSET *dset, int detail)
{
   static char FuncName[]={"SUMA_DsetInfo"};
   int i;
   SUMA_COL_TYPE ctp;
   char *s=NULL, stmp[200];
   SUMA_STRING *SS=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (dset) {
      SS = SUMA_StringAppend_va(SS, "Dset %p\n", dset);
      SS = SUMA_StringAppend_va(SS, "Number of Links: %d\n", dset->N_links);
      if (dset->dnel) {
         SS = SUMA_StringAppend_va(SS, "Dset Name: %s (%d)\n", 
            dset->dnel->name, SUMA_Dset_Type(dset->dnel->name));
         if (SDSET_FILENAME(dset)) 
            SS = SUMA_StringAppend_va(SS, "filename: %s\n", SDSET_FILENAME(dset));
         else SS = SUMA_StringAppend_va(SS, "filename: NULL\n");
         if (SDSET_LABEL(dset)) 
            SS = SUMA_StringAppend_va(SS, "label: %s\n", SDSET_LABEL(dset));
         else SS = SUMA_StringAppend_va(SS, "label: NULL\n");
         if (SDSET_ID(dset)) 
            SS = SUMA_StringAppend_va(SS, "self_idcode (idcode): %s\n", SDSET_ID(dset));
         else SS = SUMA_StringAppend_va(SS, "self_idcode (idcode): NULL\n");
         if (SDSET_IDGDOM(dset)) 
            SS = SUMA_StringAppend_va(SS, "geometry_parent_idcode: %s\n", SDSET_IDGDOM(dset));
         else SS = SUMA_StringAppend_va(SS, "geometry_parent_idcode: NULL\n");
         if (SDSET_IDMDOM(dset)) 
            SS = SUMA_StringAppend_va(SS, "domain_parent_idcode (MeshParent_idcode): %s\n", SDSET_IDMDOM(dset));
         else SS = SUMA_StringAppend_va(SS, "domain_parent_idcode ( MeshParent_idcode): NULL\n");
         

         /* where is the node index (NodeDef) column ? */
         SS = SUMA_StringAppend_va(SS, "Node Index (NodeDef) Element:\n");
         if (!dset->inel) {
            SS = SUMA_StringAppend_va(SS, "NULL inel\n");
         } else {
            SS = SUMA_StringAppend_va(SS, "\tinel->vec_len = %d\n"
                                          "\tinel->vec_num = %d\n"
                                          "\tinel->vec_filled = %d\n", 
                                          dset->inel->vec_len, dset->inel->vec_num, dset->inel->vec_filled);
         }   
         {  int *ind;
            ind = SUMA_GetNodeDef (dset);
            if (!ind) {
               SS = SUMA_StringAppend_va(SS, "\tNode Index Column pointer is NULL.\n");
            } else {
               SS = SUMA_StringAppend_va(SS, "\tNode Index Column found.\n");
               if (SDSET_SORTED(dset)) 
                  SS = SUMA_StringAppend_va(SS, "\tsorted_node_def: %s\n", SDSET_SORTED(dset));
               else SS = SUMA_StringAppend_va(SS, "\tsorted_node_def: NULL\n");
               s = SUMA_ShowMeSome((void*)(  ind), 
                                             SUMA_ColType2TypeCast (SUMA_NODE_INDEX) 
                                             , SDSET_VECLEN(dset), 5, NULL);
               SS = SUMA_StringAppend_va(SS, "         %s\n", s); SUMA_free(s); s = NULL;
            }
         }
         SS = SUMA_StringAppend_va(SS, "Data Element:\n");
         SS = SUMA_StringAppend_va(SS, "dnel->vec_num (N_subsets): %d\n", SDSET_VECNUM(dset));
         SS = SUMA_StringAppend_va(SS, "dnel->vec_filled (N_NodeDef): %d\n", SDSET_VECFILLED(dset));
         SS = SUMA_StringAppend_va(SS, "dnel->vec_len (N_Alloc): %d\n", SDSET_VECLEN(dset));
         for (i=0; i < SDSET_VECNUM(dset); ++i) {
            SS = SUMA_StringAppend_va(SS, "vec[%d]:\n", i);
            sprintf (stmp,"TypeCol_%d", i);
            ctp = SUMA_TypeOfDsetColNumb(dset, i); 
            SS = SUMA_StringAppend_va(SS, "\tColumn %d's type: %s\n",
                                       i, SUMA_Col_Type_Name(ctp));
            #if 1
            sprintf(stmp,"attrCol_%d", i);
            s = SUMA_AttrOfDsetColNumb(dset, i);
            if (s && s[0] != '\0') {
               SS = SUMA_StringAppend_va(SS, "\tColumn %d's attribute: %s\n", 
                                       i, s ); 
            } else {
               SS = SUMA_StringAppend_va(SS, "\tColumn %d's attribute does not exist.\n", 
                                       i );
            }
            if (s) SUMA_free(s); s = NULL;
            s = SUMA_DsetColLabelCopy(dset, i, 0);
            if (s && s[0] != '\0') {
               SS = SUMA_StringAppend_va(SS, "\tColumn %d's label: %s\n", 
                                       i, s ); 
            } else {
               SS = SUMA_StringAppend_va(SS, "\tColumn %d's label does not exist.\n", 
                                       i );
            }
            if (s) SUMA_free(s); s = NULL;
            if (dset->dnel->vec[i]) {
               s = SUMA_ShowMeSome((void*)(  dset->dnel->vec[i]), 
                                             SUMA_ColType2TypeCast (ctp) 
                                             , SDSET_VECLEN(dset), 5, NULL);
               SS = SUMA_StringAppend_va(SS, "         %s\n", s); SUMA_free(s); s = NULL;
            } else SS = SUMA_StringAppend_va(SS, "         NULL\n");
            #endif
         }
         if (detail) { /* write the entire element to SS */
            NI_stream ns = NI_stream_open("str:", "w");
            NI_write_element(ns, dset->ngr, NI_TEXT_MODE);
            SS = SUMA_StringAppend(SS, "\n Full NI group in text mode:\n"); 
            SS = SUMA_StringAppend(SS, NI_stream_getbuf(ns)); /* don't use StringAppend_va because it does not all 
                                                                the concatenation of very long strings. */
            SS = SUMA_StringAppend(SS, "\n");
            NI_stream_close(ns);
         }
      } else {
         SS = SUMA_StringAppend(SS, "NULL dset->dnel.");
      }
   } else {
      SS = SUMA_StringAppend(SS, "NULL dset.");
   }
   
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(s);
}

/*!
   \brief Returns a pointer to the column containing NodeDef
   (if it exists, in dset). Do not free this pointer!
*/
int * SUMA_GetNodeDef(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_GetNodeDef"};
   int *ind = NULL;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   ind = SDSET_NODE_INDEX_COL(dset);
   
   if (!ind) {
      SUMA_LH("No Node Def found.");
      SUMA_RETURN(NULL);
   } else {
      SUMA_LH("Node Def Column found.");
      /* a healthy check */
      if (  SDSET_VECLEN(dset) > SDSET_NODEINDLEN(dset) ||              /* The SDSET_VECFILLED(dset) < SDSET_NODEINDFILLED(dset) */
            SDSET_VECFILLED(dset) > SDSET_NODEINDFILLED(dset) ) {       /* can occur when drawing ROI as the node index column */
         SUMA_SL_Err(   "Veclen and/or vecfilled for\n"                 /* is created to be as large as the number of nodes in the */
                        "node indices is less \n"                       /* surface but the number of columns for the RGB data grows */
                        "than that of dset data!");                     /* with each drawing stroke. */
         SUMA_DUMP_TRACE("Discrepancy in veclen, dumping trace:\n");
      }
   }
   
   SUMA_RETURN(ind); 
}

/*!
   \brief Returns index (i) of the column containing NodeDef
   (if it exists, in dset). 
   To get the NodeDef pointer use:
   if (i>=0) NodeDef = (int *)(dset->dnel->vec[i]);
   
*/
int SUMA_GetNodeDefColIndex(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_GetNodeDefColIndex"};
   int *iv, N_i, i = -1;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   SUMA_S_Err("Function is obsolete. Make do with SUMA_GetNodeDef.\n");
   SUMA_RETURN(-1);
   
   i = -1;
   
   iv = SUMA_GetDsetColIndex (dset, SUMA_NODE_INDEX, &N_i);
   if (!iv) {
      SUMA_LH("No such column found.");
      SUMA_RETURN(-1);
   } else {
      SUMA_LH("Column found.");
      i = iv[0];
      
      if (N_i > 1) {
         SUMA_SL_Warn("Found more than one node index vector.\nReturning first one found.\n");
      }
      SUMA_free(iv); iv = NULL;
   }
   
   SUMA_RETURN(i); 
}


/*!
   \brief Look for datasets statifying the following:
      type SUMA_NODE_CONVEXITY 
      idcode_str for a geometry domain and a mesh domain
   \param ReturnDsetPointer if 1 then return pointer is to dset element (SUMA_DSET *)
                            if 0 then return pointer is to Cx (float *)
   \return A POINTER copy to Cx (float *)
*/    
void * SUMA_GetCx(char *idcode_str, DList *DsetList, int ReturnDsetPointer) 
{
   static char FuncName[]={"SUMA_GetCx"};
   float *Cx = NULL;
   char *tp_name, *idg, *idm;
   int *iv = NULL, N_i=-1, N_found = -1;
   DListElmt *el;
   SUMA_DSET *dset=NULL;
 
   SUMA_ENTRY;

   if (!dlist_size(DsetList)) SUMA_RETURN(Cx);
   if (!idcode_str)  SUMA_RETURN(Cx);
   tp_name = SUMA_Dset_Type_Name(SUMA_NODE_CONVEXITY);

   el = NULL;
   Cx = NULL;
   N_found = 0;
   do {
      if (!el) el = dlist_head(DsetList);
      else el = el->next;
      dset = (SUMA_DSET *)el->data;
      if (dset->dnel) {
         if (strcmp(SDSET_TYPE_NAME(dset), tp_name) == 0) {
            /* matched type, now look for matching domain */
            idg = SDSET_IDGDOM(dset); idm = SDSET_IDMDOM(dset);
            if (idg && idm) {
               if (!strcmp(SDSET_IDGDOM(dset), idcode_str)) {
                  if (!N_found) {
                     /* find the column of type SUMA_NODE_CX */
                     iv = SUMA_GetDsetColIndex (dset, SUMA_NODE_CX, &N_i);
                     if (!iv) { SUMA_SL_Err("SUMA_NODE_CX not found."); SUMA_RETURN(NULL); }
                     if (N_i != 1) { SUMA_SL_Err("more than 1 SUMA_NODE_CX found."); SUMA_RETURN(NULL); }
                     Cx = (float *)dset->dnel->vec[iv[0]];
                     SUMA_free(iv); iv = NULL;
                  }
                  ++ N_found;
               }
            }
         } 
      }

   }  while (el != dlist_tail(DsetList));

   if (N_found > 1) {
      SUMA_SL_Warn ("More than one convexity dataset found.\nReturning first one encountered.");
   }

   if (ReturnDsetPointer) {SUMA_RETURN((void*)dset);}
   else {SUMA_RETURN((void *)Cx);}
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
int SUMA_GetNodeIndex_FromNodeRow_ns(SUMA_DSET *dset, int row, int N_Node)
{
   int i=SUMA_GetNodeIndex_FromNodeRow_eng(dset, row, N_Node);
   WorkErrLog_ns();
   return(i);
}

int SUMA_GetNodeIndex_FromNodeRow_eng(SUMA_DSET *dset, int row, int N_Node)
{
   static char FuncName[]={"SUMA_GetNodeIndex_FromNodeRow_eng"};
   static int WarnCount;
   int Found = -1, i, *NodeDef=NULL;
   double dval=0.0;
   char *str=NULL;
   NI_element *nel = dset->dnel;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (row < 0) SUMA_RETURN(-1);
   
   if (N_Node >= 0  && row >= N_Node) {
      SUMA_PushErrLog(  "SL_Err", 
                        "row index >= N_Node\n"
                        "Will somebody please think of the children!",
                        FuncName);
      SUMA_RETURN(-1);
   } 
   if (row >= nel->vec_len) {
      SUMA_PushErrLog(  "SL_Err", 
                        "row index >= nel->vec_len\n"
                        "Bad logic!",
                        FuncName);
      SUMA_RETURN(-1);
   } 
   
   #if 0 
   /*(DO NOT DELETE)*/
   /* This would fail if data are not ordered such that row(i) is the data for node i */
   /* try the fast one */
   SUMA_LH( "Trying the fast one");
   if (nel->vec_len == nel->vec_filled && nel->vec_len == N_Node) {
      SUMA_RETURN(row);
   }
   #endif
   
   SUMA_LH( "Trying the slow mo");
   /* does this dset have a column index ? */
   NodeDef = SUMA_GetNodeDef (dset);
   
   if (NodeDef) {
      SUMA_LH( "Col. Index found");
      if (row >= nel->vec_filled) {
         SUMA_PushErrLog(  "SL_Err", "row >= nel->vec_filled.\n", FuncName);
         SUMA_RETURN(-1);
      } else {
         SUMA_RETURN(NodeDef[row]);
      }
   } 
   
   /* last resort, assume that data are ordered properly (see commented out section above)*/
   if (nel->vec_len == nel->vec_filled && nel->vec_len == N_Node) {
      if (0 && !(WarnCount % 25 - 1)) {
         SUMA_PushErrLog(  "SLP_Warn", "Assuming ith row of data\n"
                     "corresponds to node i.\n"
                     "You'll get trash if this is not true.\n"
                     "This warning is shown intermittently.", FuncName);
      } ++ WarnCount; 
      SUMA_RETURN(row);
   }
      
   SUMA_PushErrLog(  "SL_Err", "No way to get column index.", FuncName);
      
   /* bad news lews, this node is not in this Dset */ 
   SUMA_RETURN(-1);
}


/*!
   \brief j = SUMA_GetNodeRow_FromNodeIndex( dset, i);
   Returns the row index of a node in the columns
   of a data set. In other terms, node i's data are in 
   row j of the columns in nel 
   for N_Node, see comments in  SUMA_GetNodeIndex_FromNodeRow
   \sa SUMA_GetNodeIndex_FromNodeRow
*/
int SUMA_GetNodeRow_FromNodeIndex_ns(SUMA_DSET *dset, int node, int N_Node)
{
   int i = SUMA_GetNodeRow_FromNodeIndex_eng(dset,  node,  N_Node);
   WorkErrLog_ns();
   return(i);
}
int SUMA_GetNodeRow_FromNodeIndex_eng(SUMA_DSET *dset, int node, int N_Node)
{
   static char FuncName[]={"SUMA_GetNodeRow_FromNodeIndex_eng"};
   static int WarnCount;
   int Found = -1, i, *NodeDef=NULL;
   double dval=0.0;
   char *str=NULL;
   NI_element *nel = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
      
   
   if (!dset || node < 0 || (N_Node >=0 && node >= N_Node)) {
      /* turn this warning off once you confirm that Shane's bug is gone */
      if (LocalHead) SUMA_PushErrLog("S_Warn", "Strange input, returning -1.", FuncName);
      SUMA_RETURN(-1);
   }
   
   nel = dset->dnel;
   if (!nel) {
      SUMA_PushErrLog("SL_Err", "Nasty dset", FuncName);
      SUMA_RETURN(-1);
   }
   #if 0
   /* DO NOT DELETE, SEE BELOW */
   /* try the fast one */
   /* This would fail if data are not ordered such that row(i) is the data for node i */
   SUMA_LH( "Trying the fast one");
   if (nel->vec_len == nel->vec_filled && nel->vec_len == N_Node) {
      SUMA_RETURN(node);
   }
   #endif
   
   SUMA_LH("Trying the slow mo");
   /* does this dset have a column index ? */
   NodeDef = SUMA_GetNodeDef (dset);
   
   if (NodeDef) {
      SUMA_LH("Col. Index found");
      if (nel->vec_filled > node) { /* bug here (used to be < !) discovered thanks to Rosanna and Shane */
         if (node == NodeDef[node]) {
            SUMA_LH("Got lucky");
            SUMA_RETURN(node);
         }
      }
      /* explicit search */
      SUMA_LH("Explicit");
      if (nel->vec_filled > N_Node) {
         SUMA_PushErrLog("SL_Err","Unexpected error nel->vec_filled > N_Node", FuncName);
         SUMA_RETURN(-1);
      }
      for (i=0; i<nel->vec_filled; ++i) {
         if (NodeDef[i] == node) SUMA_RETURN(i);
      }
   } else {
      SUMA_LH("No Col. Index found");
   }
   
   /* last resort, assume that data are ordered properly (see commented out section above)*/
   if (nel->vec_len == nel->vec_filled && nel->vec_len == N_Node) {
      if (0 && !(WarnCount % 25 - 1)) {
         SUMA_PushErrLog("SLP_Err", "Assuming ith row of data\n"
                     "corresponds to node i.\n"
                     "You'll get trash if this is not true.\n"
                     "This warning is shown intermittently.", FuncName);
      } ++ WarnCount;       
      SUMA_RETURN(node);
   }
      
   /* bad news lews, this node is not in this Dset */ 
   SUMA_RETURN(-1);
}

/*!
   returns indexmap, such that indexmap[node] = row which means that
   data for node 'node' is in row 'row' of the dset
   if indexmap[node] = -1 then the node does not exist in the dset
   free indexmap with SUMA_free
*/
int *SUMA_CreateNodeIndexToRowIndexMap(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_CreateNodeIndexToRowIndexMap"};
   int *indexmap=NULL, j=0, *nip=NULL;
   
   SUMA_ENTRY;
   
   if (!(nip = SUMA_GetNodeDef(dset))) {
      SUMA_S_Err("Failed to find node index column in dset");
      SUMA_RETURN(indexmap);
   }
 
   /* find the mapping of a node index to a row */
   indexmap = (int *)SUMA_calloc(SDSET_VECLEN(dset), sizeof(int));
   if (!indexmap) {
      SUMA_S_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   for (j=0; j<SDSET_VECLEN(dset); ++j)  indexmap[j] = -1;
   for (j=0; j<SDSET_VECFILLED(dset); ++j) indexmap[nip[j]] = j; /* indexmap[node] hold the row in the dset that contains data for node */
      
   SUMA_RETURN(indexmap);
}
/*!
   \brief Returns the value from column ind, row ival
   The value is stored in a double variable 0.0  in case of error. 
   \sa SUMA_GetValInCol
*/
double SUMA_GetValInCol2(NI_element *nel, int ind, int ival) 
{
   static char FuncName[]={"SUMA_GetValInCol2"};
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   byte *bv = NULL;
   double *dv = NULL, dval = 0.0;
   float *fv=NULL;
   int *iv = NULL;
   char **cv = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   SUMA_SL_Warn("Obsolete, check caller");
   if (!nel) { SUMA_SL_Err("NULL input"); SUMA_RETURN(0.0); }

   if (ind < 0 || ind > nel->vec_num - 1) {
      SUMA_SL_Err("Bad index");
      SUMA_RETURN(0.0);
   }

   if (ival >= nel->vec_len) {
      SUMA_SL_Err("ival too large");
      SUMA_RETURN(0.0);
   }

   ctp = SUMA_TypeOfColNumb(nel, ind); 

   vtp = SUMA_ColType2TypeCast (ctp) ;
   switch (vtp) {
      case SUMA_byte:
         bv = (byte *)nel->vec[ind];
         dval = (double)bv[ival];
         break;
      case SUMA_int:
         iv = (int *)nel->vec[ind];
         dval = (double)iv[ival];
         break;
      case SUMA_float:
         fv = (float *)nel->vec[ind];
         dval = (double)fv[ival];
         break;
      case SUMA_double:
         dv = (double *)nel->vec[ind];
         dval = (double)dv[ival];
         break;
      default:
         SUMA_SL_Err("This type is not supported.\n");
         SUMA_RETURN(0.0);
         break;
   }

   SUMA_RETURN(dval);
}

/*!
   \brief Returns the value from column ind, row ival
   The value is stored in a double variable and a string
   version is returned. NULL in case of error. You are to free
   the returned string.
   \sa SUMA_GetValInCol2
*/
char * SUMA_GetValInCol(NI_element *nel, int ind, int ival, double *dval) 
{
   static char FuncName[]={"SUMA_GetValInCol"};
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   byte *bv = NULL;
   double *dv = NULL;
   float *fv=NULL;
   int *iv = NULL;
   char  *str=NULL, **cv = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   SUMA_SL_Warn("Obsolete, check caller");
   if (!nel || !dval) { SUMA_SL_Err("NULL input"); SUMA_RETURN(NULL); }

   if (ind < 0 || ind > nel->vec_num - 1) {
      SUMA_SL_Err("Bad index");
      SUMA_RETURN(NULL);
   }

   if (ival >= nel->vec_len) {
      SUMA_SL_Err("ival too large");
      SUMA_RETURN(NULL);
   }

   ctp = SUMA_TypeOfColNumb(nel, ind); 

   vtp = SUMA_ColType2TypeCast (ctp) ;
   switch (vtp) {
      case SUMA_byte:
         str = (char *)SUMA_malloc(50*sizeof(char));
         bv = (byte *)nel->vec[ind];
         sprintf(str,"%d",bv[ival]);
         *dval = (double)bv[ival];
         break;
      case SUMA_int:
         str = (char *)SUMA_malloc(50*sizeof(char));
         iv = (int *)nel->vec[ind];
         sprintf(str,"%d",iv[ival]);
         *dval = (double)iv[ival];
         break;
      case SUMA_float:
         str = (char *)SUMA_malloc(50*sizeof(char));
         fv = (float *)nel->vec[ind];
         sprintf(str,"%f",fv[ival]);
         *dval = (double)fv[ival];
         break;
      case SUMA_double:
         str = (char *)SUMA_malloc(50*sizeof(char));
         dv = (double *)nel->vec[ind];
         sprintf(str,"%f",dv[ival]);
         *dval = (double)dv[ival];
         break;
      case SUMA_string:
         cv = (char **)nel->vec[ind];
         *dval = 0.0;
         str = SUMA_copy_string((char*)cv[ival]);
         break;
      default:
         SUMA_SL_Err("This type is not supported yet.\n");
         SUMA_RETURN(NULL);
         break;
   }

   SUMA_LH(str);
   SUMA_RETURN(str);
}

/*!
   \brief Returns the value from column ind, row ival
   The value is stored in a double variable 0.0  in case of error. 
   \sa SUMA_GetDsetValInCol
*/
double SUMA_GetDsetValInCol2(SUMA_DSET *dset, int ind, int ival) 
{
   static char FuncName[]={"SUMA_GetDsetValInCol2"};
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   byte *bv = NULL;
   double *dv = NULL, dval = 0.0;
   float *fv=NULL;
   int *iv = NULL;
   char **cv = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!dset->dnel) { SUMA_SL_Err("NULL input"); SUMA_RETURN(0.0); }

   if (ind < 0 || ind > SDSET_VECNUM(dset) - 1) {
      SUMA_SL_Err("Bad index");
      SUMA_RETURN(0.0);
   }

   if (ival >= SDSET_VECLEN(dset)) {
      SUMA_SL_Err("ival too large");
      SUMA_RETURN(0.0);
   }

   ctp = SUMA_TypeOfDsetColNumb(dset, ind); 

   vtp = SUMA_ColType2TypeCast (ctp) ;
   switch (vtp) {
      case SUMA_byte:
         bv = (byte *)dset->dnel->vec[ind];
         dval = (double)bv[ival];
         break;
      case SUMA_int:
         iv = (int *)dset->dnel->vec[ind];
         dval = (double)iv[ival];
         break;
      case SUMA_float:
         fv = (float *)dset->dnel->vec[ind];
         dval = (double)fv[ival];
         break;
      case SUMA_double:
         dv = (double *)dset->dnel->vec[ind];
         dval = (double)dv[ival];
         break;
      default:
         SUMA_SL_Err("This type is not supported.\n");
         SUMA_RETURN(0.0);
         break;
   }

   SUMA_RETURN(dval);
}

/*!
   \brief Returns the value from column ind, row ival
   The value is stored in a double variable and a string
   version is returned. NULL in case of error. You are to free
   the returned string.
   \sa SUMA_GetDsetValInCol2
*/
char * SUMA_GetDsetValInCol(SUMA_DSET *dset, int ind, int ival, double *dval) 
{
   static char FuncName[]={"SUMA_GetDsetValInCol"};
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   byte *bv = NULL;
   double *dv = NULL;
   float *fv=NULL;
   int *iv = NULL;
   char  *str=NULL, **cv = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!dset->dnel || !dval) { SUMA_SL_Err("NULL input"); SUMA_RETURN(NULL); }

   if (ind < 0 || ind > SDSET_VECNUM(dset) - 1) {
      SUMA_SL_Err("Bad index");
      SUMA_RETURN(NULL);
   }

   if (ival >= SDSET_VECLEN(dset)) {
      SUMA_SL_Err("ival too large");
      SUMA_RETURN(NULL);
   }

   ctp = SUMA_TypeOfDsetColNumb(dset, ind); 

   vtp = SUMA_ColType2TypeCast (ctp) ;
   switch (vtp) {
      case SUMA_byte:
         str = (char *)SUMA_malloc(50*sizeof(char));
         bv = (byte *)dset->dnel->vec[ind];
         sprintf(str,"%d",bv[ival]);
         *dval = (double)bv[ival];
         break;
      case SUMA_int:
         str = (char *)SUMA_malloc(50*sizeof(char));
         iv = (int *)dset->dnel->vec[ind];
         sprintf(str,"%d",iv[ival]);
         *dval = (double)iv[ival];
         break;
      case SUMA_float:
         str = (char *)SUMA_malloc(50*sizeof(char));
         fv = (float *)dset->dnel->vec[ind];
         sprintf(str,"%f",fv[ival]);
         *dval = (double)fv[ival];
         break;
      case SUMA_double:
         str = (char *)SUMA_malloc(50*sizeof(char));
         dv = (double *)dset->dnel->vec[ind];
         sprintf(str,"%f",dv[ival]);
         *dval = (double)dv[ival];
         break;
      case SUMA_string:
         cv = (char **)dset->dnel->vec[ind];
         *dval = 0.0;
         str = SUMA_copy_string((char*)cv[ival]);
         break;
      default:
         SUMA_SL_Err("This type is not supported yet.\n");
         SUMA_RETURN(NULL);
         break;
   }

   SUMA_LH(str);
   SUMA_RETURN(str);
}

/*!
   \brief Copies the contents of a NI_element column into
   a new float vector
   V = SUMA_Col2Float (nel,  ind,  FilledOnly);
   
   \param nel (NI_element *)
   \param ind (int) index of column to be copied
   \param FilledOnly (int) 0 = allocate for and read all of the column 
                              (up to nel->vec_len)
                           1 = allocate for and read the filled portion 
                               of the column (up to nel->vec_filled)
   \return V (float *) vector (allocated by the function) containing
                     the column's contents.
 */
float * SUMA_Col2Float (NI_element *nel, int ind, int FilledOnly)
{
   static char FuncName[]={"SUMA_Col2Float"};
   int i = -1, N_read = -1, *iv = NULL;
   float *V=NULL, *fv = NULL;
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   
   SUMA_ENTRY;
   
   SUMA_SL_Err("Obsolete, check caller");
   SUMA_RETURN(NULL);
   
   if (!nel) { SUMA_RETURN(NULL); }
   
   if (ind < 0 || ind > nel->vec_num - 1) {
      SUMA_SL_Err("Bad index");
      SUMA_RETURN(NULL);
   }
   
   if (FilledOnly) {
      N_read = nel->vec_filled;
   } else {
      N_read = nel->vec_len;
   }
   
   ctp = SUMA_TypeOfColNumb(nel, ind); 

   V = (float *)SUMA_malloc(sizeof(float)*N_read);
   if (!V) { SUMA_SL_Crit("Failed to allocate for V."); SUMA_RETURN(NULL); }
   vtp = SUMA_ColType2TypeCast (ctp) ;
   switch (vtp) {
      case SUMA_int:
         iv = (int *)nel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (float)iv[i];
         break;
      case SUMA_float:
         fv = (float *)nel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = fv[i];
         break;
      default:
         SUMA_SL_Err("This type is not supported.\n");
         SUMA_free(V);
         SUMA_RETURN(NULL);
         break;
   }
   
   SUMA_RETURN(V);
}

/* 
given a 1D filename with one column of node indices, create a binary mask for
a surface of N_Node nodes.
if omask is specified, then omask is modified and returned per the operator "oper"
oper can be "or" (or "") or "and"
*/ 
byte *SUMA_load_1D_n_mask(char *name, int N_Node, byte *omask, const char *oper, int *N_inmask)
{
   static char FuncName[]={"SUMA_load_1D_n_mask"};
   int kk;
   float *far=NULL;
   MRI_IMAGE * im=NULL;
   byte *out=NULL;

   SUMA_ENTRY;
   
   if (*N_inmask) *N_inmask = -1;
   if (!name) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NULL); 
   }

   im = mri_read_1D (name);
   if (!im) {
      SUMA_S_Err("Failed to read mask file");  
      SUMA_RETURN(NULL); 
   }
   far = MRI_FLOAT_PTR(im);

   if (!im->nx) {
      SUMA_S_Err("Empty file");  
      goto CLEANUP;
   }
   
   if (im->ny != 1 ) {
      SUMA_S_Err("nmask file must have\n"
                  " 1 column.");
      fprintf(SUMA_STDERR,"Have %d columns!\n", im->ny);
      goto CLEANUP;
   }
   if (!omask) {
      out = (byte *)SUMA_calloc(N_Node, sizeof(byte));
      if (!out) {
         SUMA_S_Crit("Failed to allocate"); goto CLEANUP;
      }
   } else {
      out = omask;
   }
   if (omask) {
      if (!oper || oper[0] == '\0' || (oper && strstr(oper, "or"))) {
         for (kk=0; kk<im->nx; ++kk) {
            if (far[kk] < 0 || far[kk] >= N_Node) {
               SUMA_S_Err( "Bad indices in mask file.\n"
                           "Values either < 0 or >= number\n"
                           "of nodes in surface.");
               if (out && out != omask) SUMA_free(out); out = NULL;
               goto CLEANUP;
            }
            out[(int)far[kk]] = 1;   
         }
      } else if (oper && strstr(oper, "and")) {
         for (kk=0; kk<im->nx; ++kk) {
            if (far[kk] < 0 || far[kk] >= N_Node) {
               SUMA_S_Err( "Bad indices in mask file.\n"
                           "Values either < 0 or >= number\n"
                           "of nodes in surface.");
               if (out && out != omask) SUMA_free(out); out = NULL;
               goto CLEANUP;
            }
            out[(int)far[kk]] += 1; 
            if (out[(int)far[kk]] > 2) {
               SUMA_S_Err("input mask is not binary!\n");
               if (out && out != omask) SUMA_free(out); out = NULL;
               goto CLEANUP;   
            }   
         }
         for (kk=0; kk < N_Node; ++kk) {
            if (out[kk] > 1) out[kk] = 1;
            else out[kk] = 0;
         }
      } else {
         SUMA_S_Errv("Bad operator %s\n");
         if (out && out != omask) SUMA_free(out); out = NULL;
         goto CLEANUP;   

      }
   } else {
      for (kk=0; kk<im->nx; ++kk) {
         if (far[kk] < 0 || far[kk] >= N_Node) {
            SUMA_S_Err( "Bad indices in mask file.\n"
                        "Values either < 0 or >= number\n"
                        "of nodes in surface.");
            if (out && out != omask) SUMA_free(out); out = NULL;
            goto CLEANUP;
         }
         out[(int)far[kk]] = 1;   
      }
   }

   if (*N_inmask) {
      *N_inmask = 0;
      for (kk=0; kk<N_Node; ++kk) if (out[kk]) ++(*N_inmask);
   }
   
   CLEANUP:
   mri_free(im); im = NULL;
   SUMA_RETURN(out);
}

/* 
given a 1D filename with one column of 0s and non zeros, create a binary mask for
a surface of N_Node nodes. 
if omask is specified, then omask is modified and returned per the operator "oper"
oper can be "or" (or "") or "and"
*/ 
byte *SUMA_load_1D_b_mask(char *name, int N_Node, byte *omask, const char *oper, int *N_inmask)
{
   static char FuncName[]={"SUMA_load_1D_b_mask"};
   int kk;
   float *far=NULL;
   MRI_IMAGE * im=NULL;
   byte *out=NULL;

   SUMA_ENTRY;
   
   if (*N_inmask) *N_inmask = -1;
   
   if (!name) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NULL); 
   }

   im = mri_read_1D (name);
   if (!im) {
      SUMA_S_Err("Failed to read mask file");  
      SUMA_RETURN(NULL); 
   }
   far = MRI_FLOAT_PTR(im);

   if (!im->nx) {
      SUMA_S_Err("Empty file");  
      goto CLEANUP;
   }
   if (im->ny != 1 ) {
      SUMA_S_Err("nmask file must have\n"
                  " 1 column.");
      fprintf(SUMA_STDERR,"Have %d columns!\n", im->ny);
      goto CLEANUP;
   }

   if (im->nx != N_Node) {
      SUMA_S_Err( "Number of rows in mask file is not \n"
                  "equal to number of nodes in surface.\n");  
      goto CLEANUP;
   }

   if (!omask) {
      out = (byte *)SUMA_calloc(N_Node, sizeof(byte));
      if (!out) {
         SUMA_S_Crit("Failed to allocate"); goto CLEANUP;
      }
   } else {
      out = omask;
   }

   if (omask) {
      if (!oper || oper[0] == '\0' || (oper && strstr(oper, "or"))) {
         for (kk=0; kk<im->nx; ++kk) {
            if ((int)far[kk]) {
               out[kk] = 1; 
               /* fprintf (SUMA_STDERR,"%d   ", kk);  */
            }
         }
      } else if (oper && strstr(oper, "and")) {
         for (kk=0; kk<im->nx; ++kk) {
            if ((int)far[kk]&&out[kk]) out[kk] = 1;
            else out[kk] = 0;
         }
      } else {
         SUMA_S_Errv("Bad operator %s\n");
         if (out && out != omask) SUMA_free(out); out = NULL;
         goto CLEANUP;   

      }
   } else {
      for (kk=0; kk<im->nx; ++kk) {
         if ((int)far[kk]) { out[kk] = 1; }
      }
   }
   
   if (*N_inmask) {
      *N_inmask = 0;
      for (kk=0; kk<N_Node; ++kk) if (out[kk]) ++(*N_inmask);
   }
   CLEANUP:
   mri_free(im); im = NULL;
   SUMA_RETURN(out);
}

byte *SUMA_get_c_mask(char *mask, int N_Node, byte *omask, const char *oper, int *N_inmask)
{
   static char FuncName[]={"SUMA_get_c_mask"};
   int    clen, ninmask, i, kk;
	char * cmd;
   byte *bmask=NULL, *out=NULL;
   
   SUMA_ENTRY;
   
   if (*N_inmask) *N_inmask = -1;
   if (!mask) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(bmask);
   }
   
   clen = strlen( mask );
	/* save original cmask command, as EDT_calcmask() is destructive */
	cmd = (char *)malloc((clen + 1) * sizeof(char));
	strcpy( cmd,  mask);

	bmask = EDT_calcmask( cmd, &ninmask );

	free( cmd );		cmd = NULL;	   /* free EDT_calcmask() string */

	if ( bmask == NULL ) {
	   SUMA_S_Err("Failed to compute mask from -cmask option");
      SUMA_RETURN(NULL);
	} 
	if ( ninmask != N_Node ) {
	   SUMA_S_Err("Input and cmask datasets do not have "
		            "the same dimensions\n" );
	   SUMA_free(bmask); bmask=NULL;
	   SUMA_RETURN(NULL);
	}
   
   if (!omask) {
      out = bmask;
   } else {
      out = omask;
      if (!oper || oper[0] == '\0' || (oper && strstr(oper, "or"))) {
         for (kk=0; kk<ninmask; ++kk) {
            if (bmask[kk]) {
               out[kk] = 1; 
               /* fprintf (SUMA_STDERR,"%d   ", kk);  */
            }
         }
      } else if (oper && strstr(oper, "and")) {
         for (kk=0; kk<ninmask; ++kk) {
            if (bmask[kk]&&out[kk]) out[kk] = 1;
            else out[kk] = 0;
         }
      } else {
         SUMA_S_Errv("Bad operator %s\n");
         if (out && out != omask) { SUMA_free(out); out = NULL; }
         else { if (out) SUMA_free(out); out = NULL; }
         goto CLEANUP;   
      }
   }
   
   if (*N_inmask) {
      *N_inmask = 0;
      for (kk=0; kk<N_Node; ++kk) if (out[kk]) ++(*N_inmask);
   }
   
   CLEANUP:
   if (bmask && out != bmask) free(bmask); bmask=NULL;
   
   SUMA_RETURN(out);
}

/*!
   \brief Load and combine all masks on command line
   mask = SUMA_load_all_command_masks(char *bmaskname, char *nmaskname, char *cmask, int N_Node, int *N_inmask);
   N_Node is the number of nodes on the recipient surface
   N_inmask will contain the number of nodes in the mask (if it is -1 then an error occurred)
   mask is a vector (N_Node) of 1 for nodes in the mask (N_inmask of them) and 0 for nodes not in mask.
*/   
byte * SUMA_load_all_command_masks(char *bmaskname, char *nmaskname, char *cmask, int N_Node, int *N_inmask)
{
   static char FuncName[]={"SUMA_load_all_command_masks"};
   byte *nmask=NULL;

   SUMA_ENTRY;

   *N_inmask = -1;   /* indicates an error */
   
   if (bmaskname) {
      if (!(nmask = SUMA_load_1D_b_mask(bmaskname, N_Node, nmask, "and", N_inmask))) {
         SUMA_S_Err("Failed loading mask");
         if (nmask) SUMA_free(nmask); nmask=NULL; SUMA_RETURN(nmask);
      }
   }
   if (cmask) {
      if (!(nmask = SUMA_get_c_mask(cmask, N_Node, nmask, "and", N_inmask))) {
         SUMA_S_Err("Failed loading mask");
         if (nmask) SUMA_free(nmask); nmask=NULL; SUMA_RETURN(nmask);
      }
   }
   if (nmaskname) {
      if (!(nmask = SUMA_load_1D_n_mask(nmaskname, N_Node, nmask, "and", N_inmask))) {
         SUMA_S_Err("Failed loading mask");
         if (nmask) SUMA_free(nmask); nmask=NULL; SUMA_RETURN(nmask);
      }
   }
   
   
   if (*N_inmask < 0) *N_inmask = 0; /* Remove error flag, even if nmask is NULL (no mask to speak of) */
   
   SUMA_RETURN(nmask);
}

/*!
   \brief Create a byte * mask of indices in ind_list
   \param ind_list (int *) Pointer to vector of n_ind_list (node) indices
   \param N_ind_list (int)
   \param N_mask (int) length of returned byte mask. Values in ind_list >= N_mask will
                       generate a warning and will be ignored
   \param N_inmask (int *) Pointer to integer that will contain number of 1 values in bmask
                           that would be a number equal to N_ind_list if all values in ind_list are < N_mask
   \return bmask (byte *) the byte mask with bmask[j] = 1 where j is an int found in ind_list.
*/
byte * SUMA_indexlist_2_bytemask(int *ind_list, int N_ind_list, int N_mask, int *N_inmask)   
{
   static char FuncName[]={"SUMA_indexlist_2_bytemask"};
   int i = 0, cnt; 
   byte *bmask = NULL;

   SUMA_ENTRY; 

   cnt = -1; 
   if (!ind_list) {
      SUMA_S_Err("NULL ind_list");
      goto BYE;
   }

   if (!(bmask = (byte *)SUMA_calloc(N_mask, sizeof(byte)))) { 
      SUMA_SL_Crit("Failed to allocate (macro)"); 
      goto BYE;
   }

   cnt = 0; 
   for (i=0; i<N_ind_list; ++i) {
      if (ind_list[i] < N_mask) {
         bmask[ind_list[i]] = 1;
         ++cnt;
      } else {
         SUMA_S_Warn("Values in ind_list exceed N_mask!\nIndices ignored.");
      }
   }

   BYE:
   if (N_inmask) *N_inmask = cnt;
   SUMA_RETURN(bmask);
}

/*!
   \brief Make the contents of a sparse node-based data column (vector)
   into a full column.
   \param vp (float **) pointer to the vector (v=*vp) containing values.
                        when the function returns, v would be a full column,
                        possibly a new vector altogether. Old sparse v 
                        would get freed inside this function.
   \param N_v (int) number of values in v
   \param mask_val (float) value to assign in full vector for empty locations
   \param bmp (byte **) if bmp != NULL then a mask is returned in *bmp that
                        indicates which locations in returned v have values in them.
   \param SO (SUMA_SurfaceObject *) 
   \return  YUP: OK
            NOPE: NOK 
*/
SUMA_Boolean SUMA_MakeSparseColumnFullSorted(float **vp, int N_v, float mask_val, byte **bmp, SUMA_DSET *dset, int N_Node)
{
   static char FuncName[]={"SUMA_MakeSparseColumnFullSorted"};
   float range[2], *v=NULL, *vn=NULL;
   byte *bm = NULL;
   int  loc[2], *nip=NULL, i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!vp || !*vp) {
      SUMA_S_Err("NULL vp!");
      SUMA_RETURN(NOPE);
   }
   
   /* the column vector */
   v = *vp; 
   
   /* get the node index column */
   if (!(nip = SUMA_GetNodeDef(dset))) {
      /* No node index column, check based on number of values */
      if (N_v == N_Node) {
         SUMA_LH("Fullness established based on number of nodes and values in v");
         SUMA_RETURN(YUP);
      } else {
         SUMA_S_Err("Not full and cannot fill it");
         SUMA_RETURN(NOPE);
      }
   } else {
      /* make sure column range does not exceed N_Node */
      if (!SUMA_GetDsetNodeIndexColRange(dset, range, loc, 1)) {
         SUMA_S_Err("Failed to get nodedef range");
         SUMA_RETURN(NOPE);
      }
      
      if (range[0] < 0 || range[1] >= N_Node) {
         SUMA_S_Err("Node index range is outside [0, N_Node[");
         SUMA_RETURN(NOPE);
      }
      
      /* have node index */
      if (N_v != SDSET_VECFILLED(dset)) {   /* oops, not good */
         SUMA_S_Err("Number of values in v not equal to vec_filled.\nCannot proceed.");
         SUMA_RETURN(NOPE);
      }
      
      /* number of values is same as filled values, good, now proceed */
      if (  SDSET_VECFILLED(dset) == N_Node && 
            nip[SDSET_VECFILLED(dset)-1] == N_Node -1 &&
            nip[SDSET_VECFILLED(dset)-1-SDSET_VECFILLED(dset)/2] == N_Node -1 -SDSET_VECFILLED(dset)/2 &&
            nip[SDSET_VECFILLED(dset)-1-SDSET_VECFILLED(dset)/3] == N_Node -1 -SDSET_VECFILLED(dset)/3 &&
            SDSET_IS_SORTED(dset)) {   /* likely a full column */
         SUMA_LH("Fullness established by loose examination of node defintion column.");
         SUMA_RETURN(YUP);   
      }
      /* if you get here then you do not have a full list or the list is not sorted*/
      vn = (float *)SUMA_malloc(N_Node * sizeof(float));
      if (bmp) {
         if (*bmp) {
            SUMA_S_Err("*bmp must be NULL");
            SUMA_RETURN(NOPE);
         }
         bm = (byte *)SUMA_calloc(N_Node, sizeof(byte));
      } else bm = NULL;
      
      if (!vn || (bmp && !bm)) {
         SUMA_S_Crit("Failed to allocate");
         SUMA_RETURN(NOPE);
      }
      for (i=0; i<N_Node; ++i) {
         vn[i] = mask_val; /* if (bm) bm[i] = 0; */
      }
      for (i=0; i<SDSET_VECFILLED(dset); ++i) {
         vn[nip[i]] = v[i]; 
         if (bm) bm[nip[i]] = 1;
      }
      
      
      /* Now free old v */
      SUMA_free(v); v = NULL;
      
      /* stow away for return */
      if (bmp) { *bmp = bm; bm = NULL; }  
      *vp = vn; vn = NULL;
      
      SUMA_RETURN(YUP);
   }
   
   /* Should not get here */
   SUMA_RETURN(NOPE);
}
 
/*!
   \brief Copies the contents of a float vector into a NI_element column 
   SUMA_Float2DsetCol (dset,  ind,  V, FilledOnly);
   
   \param nel (NI_element *)
   \param ind (int) index of column to be filled with values in V
   \param V (float *) vector containing the column's contents (see number of values copied below).
   \param FilledOnly (int) 0 = copy from index 0 to dset->dnel->vec_len or SDSET_VECLEN(dset)
                           1 = copy from index 0 to dset->dnel->vec_filled or SDSET_VECFILLED(dset)
   
   The values in V replace those in nel!
   No new columns in nel are created.
   
   \sa SUMA_DsetCol2Float 
*/
int SUMA_Float2DsetCol (SUMA_DSET *dset, int ind, float *V, int FilledOnly)
{
   static char FuncName[]={"SUMA_Float2DsetCol"};
   int i = -1, N_read = -1, *iv = NULL, *nip=NULL;
   float *fv = NULL;
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   
   SUMA_ENTRY;
      
   if (!dset) { SUMA_RETURN(0); }
   
   if (ind < 0 || ind > SDSET_VECNUM(dset) - 1) {
      SUMA_SL_Err("Bad index");
      SUMA_RETURN(0);
   }
   
   if (FilledOnly) {
      N_read = SDSET_VECFILLED(dset);
   } else {
      N_read = SDSET_VECLEN(dset);
   }
   
   ctp = SUMA_TypeOfDsetColNumb(dset, ind); 
   vtp = SUMA_ColType2TypeCast (ctp) ;
   nip = SUMA_GetNodeDef(dset);
   switch (vtp) {
      case SUMA_int:
         iv = (int *)dset->dnel->vec[ind];
         if (nip)  for (i=0; i<N_read; ++i) iv[i] = (int)V[nip[i]];
         else for (i=0; i<N_read; ++i) iv[i] = (int)V[i];
         break;
      case SUMA_float:
         fv = (float *)dset->dnel->vec[ind];
         if (nip)  for (i=0; i<N_read; ++i) fv[i] = V[nip[i]];
         else for (i=0; i<N_read; ++i) fv[i] = V[i];
         break;
      default:
         SUMA_SL_Err("This type is not supported.\n");
         SUMA_RETURN(0);
         break;
   }
   
   /* reset generic attributes */
   SUMA_AddGenDsetColAttr (dset, ctp, dset->dnel->vec[ind], 1, ind);
  
   SUMA_RETURN(1);
}

/*!
   \brief Copies the contents of a NI_element column into
   a new float vector
   V = SUMA_DsetCol2Float (dset,  ind,  FilledOnly);
   
   \param nel (NI_element *)
   \param ind (int) index of column to be copied
   \param FilledOnly (int) 0 = allocate for and read all of the column 
                              (up to nel->vec_len)
                           1 = allocate for and read the filled portion 
                               of the column (up to nel->vec_filled)
   \return V (float *) vector (allocated by the function) containing
                     the column's contents.
   \sa SUMA_Float2DsetCol
 */
float * SUMA_DsetCol2Float (SUMA_DSET *dset, int ind, int FilledOnly)
{
   static char FuncName[]={"SUMA_DsetCol2Float"};
   int i = -1, N_read = -1, *iv = NULL;
   float *V=NULL, *fv = NULL;
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   
   SUMA_ENTRY;
      
   if (!dset) { SUMA_RETURN(NULL); }
   
   if (ind < 0 || ind > SDSET_VECNUM(dset) - 1) {
      SUMA_SL_Err("Bad index");
      SUMA_RETURN(NULL);
   }
   
   if (FilledOnly) {
      N_read = SDSET_VECFILLED(dset);
   } else {
      N_read = SDSET_VECLEN(dset);
   }
   
   ctp = SUMA_TypeOfDsetColNumb(dset, ind); 

   V = (float *)SUMA_malloc(sizeof(float)*N_read);
   if (!V) { SUMA_SL_Crit("Failed to allocate for V."); SUMA_RETURN(NULL); }
   vtp = SUMA_ColType2TypeCast (ctp) ;
   switch (vtp) {
      case SUMA_int:
         iv = (int *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (float)iv[i];
         break;
      case SUMA_float:
         fv = (float *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = fv[i];
         break;
      default:
         SUMA_SL_Err("This type is not supported.\n");
         SUMA_free(V);
         SUMA_RETURN(NULL);
         break;
   }
      
   SUMA_RETURN(V);
}

/*!
   a function to return the string attribute of a column
   Free with SUMA_free
*/
char * SUMA_AttrOfDsetColNumb(SUMA_DSET *dset, int ind)
{
   static char FuncName[]={"SUMA_AttrOfDsetColNumb"};
   char *cnm = NULL;
   NI_element *nelb=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset) {  
      SUMA_SL_Err("NULL NI element");
      SUMA_RETURN(cnm);
   }
   if (ind < 0 || ind > (SDSET_VECNUM(dset) - 1)) {
      SUMA_SL_Err("Bad index");
      SUMA_RETURN(cnm);
   }
   
   /* try SUMA's */
   nelb = SUMA_FindDsetAttributeElement(dset, "ColumnsAttributes");
   if (nelb) {
      SUMA_NEL_GET_STRING(nelb, 0, 0, cnm); /* cnm is a pointer copy here, do not free */
      cnm = SUMA_Get_Sub_String(cnm, SUMA_NI_CSS, ind);
      SUMA_RETURN(cnm);
   }

   
   
   if (LocalHead) { SUMA_SL_Warn("Failed to find attribute"); }
   SUMA_RETURN(NULL);
}

/*!
   a wrapper to faciliate getting column types from 
   both SUMA and AFNI formatted niml elements
   \sa SUMA_Col_Type
   NOTE, this function will repeatedly parse the entire string
   for column types. That's both stupid and inefficient.
   Write an efficient version called  SUMA_TypesOfDsetColNumb next ...
*/
SUMA_COL_TYPE SUMA_TypeOfDsetColNumb(SUMA_DSET *dset, int ind) 
{
   static char FuncName[]={"SUMA_TypeOfDsetColNumb"};
   int *ctpv = NULL;
   char *cnm = NULL, **sc = NULL;
   int_array *iar = NULL;
   SUMA_COL_TYPE ctp = SUMA_ERROR_COL_TYPE;
   char stmp[100];
   NI_element *nelb=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset) {  
      SUMA_SL_Err("NULL NI element");
      SUMA_RETURN(ctp);
   }
   if (ind < 0 || ind > (SDSET_VECNUM(dset) - 1)) {
      SUMA_SL_Err("Bad index");
      SUMA_RETURN(ctp);
   }
   
   /* try SUMA's */
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_TYPE");
   if (nelb) {
      SUMA_LH("Fetching Type a la suma");
      SUMA_NEL_GET_STRING(nelb, 0, 0, cnm); /* cnm is a pointer copy here, do not free */
      cnm = SUMA_Get_Sub_String(cnm, SUMA_NI_CSS, ind);
   }
   if (cnm) {
      ctp = SUMA_Col_Type(cnm); SUMA_free(cnm); cnm = NULL;
      SUMA_RETURN(ctp);
   }
   
   /* try AFNI's */
   SUMA_LH("Fetching Type a la afni");
   cnm = NI_get_attribute(dset->dnel, "ni_type");
   if (cnm) {
      SUMA_LH(cnm);
      iar = decode_type_string( cnm ); 
      if (iar) {
         ctp = iar->ar[ind];   /* this is not the same as SUMA's column type, it is just data type */
         NI_free(iar->ar); NI_free(iar); iar = NULL;
         switch(ctp) {
            case SUMA_int:
               ctp = SUMA_NODE_INT;
               break;
            case SUMA_float:
               ctp = SUMA_NODE_FLOAT;
               break;
            case SUMA_byte:
               ctp = SUMA_NODE_BYTE;
               break;
            case SUMA_short:
               ctp = SUMA_NODE_SHORT;
               break;
            default:
               SUMA_SL_Err("AFNI column type not supported at the moment.\n");
               ctp = SUMA_ERROR_COL_TYPE;
               break;
         }
         SUMA_RETURN(ctp);
      }
   }
   
   SUMA_SL_Err("Failed to determine type");
   SUMA_RETURN(ctp);
}

SUMA_Boolean SUMA_AddNodeIndexColumn(SUMA_DSET *dset, int N_Node) 
{
   static char FuncName[]={"SUMA_AddNodeIndexColumn"};
   float range[2];
   int *ind=NULL, i, N_i;
   float *T = NULL;
   int *Ti = NULL;
   byte *vis=NULL;
   SUMA_Boolean OKfirstCol = NOPE;
   SUMA_Boolean LocalHead = NOPE;
       
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(NOPE);
   /* check for obvious insult */
   if (SDSET_VECLEN(dset) > N_Node) {
      SUMA_SL_Err("more values in dset than nodes in surface.");
      SUMA_RETURN(NOPE);
   }
   /* Check for Col Index*/
   ind = SUMA_GetNodeDef (dset);
   if (!ind) {
      SUMA_LH("No node index column");
      /* would the first column work ? */
      T = SUMA_DsetCol2Float (dset, 0, 0);
      OKfirstCol = NOPE;
      if (!T) { SUMA_LH("First column does not cut it"); OKfirstCol = NOPE;}
      else {
         Ti = (int *)SUMA_malloc(sizeof(int)*SDSET_VECLEN(dset));
         vis = (byte *)SUMA_calloc(N_Node, sizeof(byte));
         SUMA_LH("Testing if node indices can be in 1st column...");
         /* check if all values are ints and if they are within 0 and N_Node -1 
            and that there are no duplicate entries */
         i=0;
         OKfirstCol = YUP;
         while (i <SDSET_VECLEN(dset) && OKfirstCol) {
            Ti[i] = (int)T[i];
            if ( (T[i] != Ti[i]) || (T[i] < 0) || (Ti[i] >= N_Node) || vis[Ti[i]] ) OKfirstCol = NOPE;
            else if ( Ti[i] < N_Node ) vis[Ti[i]] = 1;
            ++i;
         }
         SUMA_free(vis); vis = NULL;
         
         if (!OKfirstCol) { 
            SUMA_SLP_Note( "Assuming node indexing\n"
                           "is explicit. \n"
                           "1st row is for node 0\n"
                           "2nd is for node 1, etc.\n" );
            for (i=0; i <SDSET_VECLEN(dset); ++i) Ti[i]=i;
            OKfirstCol = YUP;
         }else{
            char Name[500], Attr[500];
            SUMA_SL_Note("Used column 0 as node indices.\nAdded a node index column nonetheless.");
            /* You can't just change the label and the type of the 0th column to be SUMA_NODE_INDEX because
            this column is float, not ints. Duplicate ahead...*/
         }
         
      }
      
      if (!OKfirstCol) {
         SUMA_LH("No node index could be created");
         if (Ti) SUMA_free(Ti); Ti = NULL;
         SUMA_RETURN(NOPE);
      }
      
      /* Now add Ti to the dataset as a node index column ... */
      /* if you change the column label's string ("Node Index (inferred)") 
      make sure you change SUMA_isColumn_inferred accordingly */
      if (!SUMA_AddDsetNelCol (dset, "Node Index (inferred)", SUMA_NODE_INDEX, (void *)Ti, NULL, 1)) {
         SUMA_SL_Err("Failed to add column");
         if (Ti) SUMA_free(Ti); Ti = NULL;
         SUMA_RETURN(NOPE);
      }
      
      /* all done */
      SUMA_LH("Added the index column, ciao");
      if (Ti) SUMA_free(Ti); Ti = NULL;
      SUMA_RETURN(YUP);
   } else {
      SUMA_LH("Node index column found, element might be empty however, use SUMA_PopulateDsetNodeIndexNel to fill it.");
      /* Nothing to do, return on a positive note */
      SUMA_RETURN(YUP);      
   } 
   
   SUMA_SL_Err("why are you here ?");
   SUMA_RETURN(NOPE);
}

SUMA_Boolean SUMA_PopulateDsetNodeIndexNel(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_PopulateDsetNodeIndexNel"};
   int *Ti = NULL; 
   int i;
   SUMA_ENTRY;

   if (!dset ) {
      SUMA_S_Err("NULL input dset");
   }
   
   if (!dset->inel) {
      SUMA_S_Err("NULL dset->inel");
   }
   
   if (dset->inel && dset->inel->vec_num) {
      SUMA_S_Note("Dset has node indices. Will not alter list.\n");
   } else {
      SUMA_S_Note( "Assuming node indexing\n"
                     "is explicit. \n"
                     "1st row is for node 0\n"
                     "2nd is for node 1, etc.\n" );
      Ti = (int *) SUMA_calloc(SDSET_VECLEN(dset), sizeof(int));
      for (i=0; i <SDSET_VECLEN(dset); ++i) Ti[i]=i;
      if (!SUMA_AddDsetNelCol (dset, "Node Index (inferred)", SUMA_NODE_INDEX, (void *)Ti, NULL, 1)) {
         SUMA_S_Err("Failed to add column");
         SUMA_RETURN(NOPE);
      }
      SUMA_free(Ti); Ti = NULL; 
   }

   SUMA_RETURN(YUP);
}


/*!
   a wrapper to faciliate getting column types from 
   both SUMA and AFNI formatted niml elements
   \sa SUMA_Col_Type
*/
SUMA_COL_TYPE SUMA_TypeOfColNumb(NI_element *nel, int ind) 
{
   static char FuncName[]={"SUMA_TypeOfColNumb"};
   int *ctpv = NULL;
   char *cnm = NULL;
   int_array *iar = NULL;
   SUMA_COL_TYPE ctp = SUMA_ERROR_COL_TYPE;
   char stmp[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* SUMA_SL_Warn("Obsolete, use new version."); still needed to convert old format to new one */
   
   if (!nel) {  
      SUMA_SL_Err("NULL NI element");
      SUMA_RETURN(ctp);
   }
   if (ind < 0 || ind > (nel->vec_num - 1)) {
      SUMA_SL_Err("Bad index");
      SUMA_RETURN(ctp);
   }
   
   /* try SUMA's */
   snprintf (stmp,50*sizeof(char),"TypeCol_%d", ind);
   cnm = NI_get_attribute(nel, stmp);
   if (cnm) {
      SUMA_RETURN(SUMA_Col_Type(cnm));
   }
   
   /* try AFNI's */
   cnm = NI_get_attribute(nel, "ni_type");
   if (cnm) {
      SUMA_LH(cnm);
      iar = decode_type_string( cnm ); 
      if (iar) {
         ctp = iar->ar[ind];   /* this is not the same as SUMA's column type, it is just data type */
         NI_free(iar->ar); NI_free(iar); iar = NULL;
         switch(ctp) {
            case SUMA_int:
               ctp = SUMA_NODE_INT;
               break;
            case SUMA_float:
               ctp = SUMA_NODE_FLOAT;
               break;
            case SUMA_byte:
               ctp = SUMA_NODE_BYTE;
               break;
            case SUMA_short:
               ctp = SUMA_NODE_SHORT;
               break;
            default:
               SUMA_SL_Err("AFNI column type not supported at the moment.\n");
               ctp = SUMA_ERROR_COL_TYPE;
               break;
         }
         SUMA_RETURN(ctp);
      }
   }
   
   SUMA_SL_Err("Failed to determine type");
   SUMA_RETURN(ctp);
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
SUMA_DSET *SUMA_LoadDset_ns (char *Name, SUMA_DSET_FORMAT *form, int verb)
{  
   SUMA_DSET *dset = SUMA_LoadDset_eng (Name, form,  verb);
   WorkErrLog_ns();
   return(dset);
}
SUMA_DSET *SUMA_LoadDset_eng (char *iName, SUMA_DSET_FORMAT *form, int verb)
{  
   static char FuncName[]={"SUMA_LoadDset_eng"};
   SUMA_DSET *dset = NULL, *dset_c = NULL;
   int *RowSel, *ColSel, *NodeSel, i;
   char *Name=NULL;
   byte *b_ColSel = NULL, *b_RowSel = NULL;
   SUMA_PARSED_NAME *pn=NULL;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!iName) { SUMA_SL_Err("NULL Name"); goto GOODBYE; }
   
   /* parse the name please. */
   RowSel = ColSel = NodeSel = NULL;
   if (!(pn = SUMA_ParseFname(iName, NULL))) {
      SUMA_SL_Err("Failed to parse name"); goto GOODBYE;
   }
   if (pn->NodeSelect[0]!= '\0' && pn->RowSelect[0] != '\0') {
      SUMA_SL_Err("Cannot use Node and Column selectors simultaneously");
      goto GOODBYE; 
   }
   
   /* Form the name */
   Name = SUMA_append_string(pn->Path, pn->FileName);
   
   if (*form == SUMA_NO_DSET_FORMAT) {      /* attempt to guess from extension */
      SUMA_LH("Attempting to guess dset format from extension\n");
      *form = SUMA_GuessFormatFromExtension(Name);
   }
   
   
   switch (*form) {
      case SUMA_NIML:
      case SUMA_ASCII_NIML:
      case SUMA_BINARY_NIML:
         SUMA_LH("Loading NIML Dset");
         dset = SUMA_LoadNimlDset(Name, verb);
         break;
      case SUMA_1D:
         SUMA_LH("Loading 1D Dset");
         dset = SUMA_Load1DDset_ns(Name, verb);
         break;
      case SUMA_ASCII_OPEN_DX_DSET:
         SUMA_LH("Loading DX Dset");
         dset = SUMA_LoadDXDset_ns(Name, verb);
         break;
      case SUMA_NO_DSET_FORMAT:
         if (!dset) { SUMA_LH("Trying NIML Dset"); dset = SUMA_LoadNimlDset(Name, 0); *form = SUMA_NIML; }
         if (!dset) { SUMA_LH("Trying 1D Dset"); dset = SUMA_Load1DDset_ns(Name, 0); *form = SUMA_1D; }
         if (!dset) { SUMA_LH("Trying DX Dset"); dset = SUMA_LoadDXDset_ns(Name, 0); *form = SUMA_ASCII_OPEN_DX_DSET; }
         break;
      default:
         if (verb) SUMA_PushErrLog("SLP_Err", "Bad format specification", FuncName);
         goto GOODBYE;   
   }
   
   if (!dset) {
      if (verb) SUMA_PushErrLog("SL_Err","Failed to read dset", FuncName);
      goto GOODBYE;    
   }
   if (LocalHead) {
      SUMA_ShowParsedFname(pn, NULL);
      SUMA_ShowDset(dset, 0, NULL);
   } 
   
   /* do we have column, row, or node selectors? */
   if (pn->NodeSelect[0]!= '\0') {
      /* go get the selector lists */
      NodeSel = MCW_get_intlist( SDSET_VECFILLED(dset) , pn->NodeSelect ) ;
      if (!NodeSel || !NodeSel[0]) {
         SUMA_SL_Err("Failed to get node selection");
         SUMA_FreeDset(dset); dset = NULL;
         goto GOODBYE;      
      }
      if (LocalHead) {
         SUMA_LH("NodeSelection");
         for (i=0; i<=NodeSel[0]; ++i) fprintf(SUMA_STDERR,"   %d", NodeSel[i]);
         fprintf(SUMA_STDERR,"\n");
      }
   }
   if (pn->ColSelect[0]!= '\0') {
      /* go get the selector lists */
      ColSel = MCW_get_intlist( SDSET_VECFILLED(dset) , pn->ColSelect ) ;
      if (!ColSel || !ColSel[0]) {
         SUMA_SL_Err("Failed to get node selection");
         SUMA_FreeDset(dset); dset = NULL;
         goto GOODBYE;     
      }
      if (LocalHead) {
         SUMA_LH("ColSelection");
         for (i=0; i<=ColSel[0]; ++i) fprintf(SUMA_STDERR,"   %d", ColSel[i]);
         fprintf(SUMA_STDERR,"\n");
      }
   }
   if (pn->RowSelect[0]!= '\0') {
      /* go get the selector lists */
      RowSel = MCW_get_intlist( SDSET_VECFILLED(dset) , pn->RowSelect ) ;
      if (!RowSel || !RowSel[0]) {
         SUMA_SL_Err("Failed to get node selection");
         SUMA_FreeDset(dset); dset = NULL;
         goto GOODBYE;            
      }
      if (LocalHead) {
         SUMA_LH("RowSelection");
         for (i=0; i<=RowSel[0]; ++i) fprintf(SUMA_STDERR,"   %d", RowSel[i]);
         fprintf(SUMA_STDERR,"\n");
      }
   }
   
   if (pn->RangeSelect[0]!= '\0') {
      SUMA_SL_Err("Range selection not allowed with SUMA dsets.");
      SUMA_FreeDset(dset); dset = NULL;
      goto GOODBYE; 
   }
   
   if (ColSel) {
      if (!(b_ColSel =  SUMA_indexlist_2_bytemask((ColSel+1), ColSel[0], SDSET_VECFILLED(dset), NULL))) {
         SUMA_SL_Err("Failed in creating byte mask");
         SUMA_FreeDset(dset); dset = NULL;
         goto GOODBYE;
      }
   } else b_ColSel = NULL;
   
   if (RowSel) {
      if (!(b_RowSel =  SUMA_indexlist_2_bytemask((RowSel+1), RowSel[0], SDSET_VECFILLED(dset), NULL))) {
         SUMA_SL_Err("Failed in creating byte mask");
         SUMA_FreeDset(dset); dset = NULL;
         goto GOODBYE;
      }
   } else b_RowSel = NULL;
   
   if (NodeSel) {
      if (!(dset_c = SUMA_MaskedByNodeIndexCopyofDset(dset, (NodeSel+1), NodeSel[0], b_ColSel, 1, 0))) {
         SUMA_SL_Err("Failed in creating masked dset");
         SUMA_FreeDset(dset); dset = NULL;
         goto GOODBYE;
      }
      SUMA_FreeDset(dset); dset = dset_c; dset_c = NULL; 
   }else if (RowSel || ColSel) {
      if (!(dset_c = SUMA_MaskedCopyofDset(dset, b_RowSel, b_ColSel, 1, 0))) {
         SUMA_SL_Err("Failed in creating masked dset");
         SUMA_FreeDset(dset); dset = NULL;
         goto GOODBYE;
      }
      SUMA_FreeDset(dset); dset = dset_c; dset_c = NULL;
   }
   
   GOODBYE:
   if (b_ColSel) SUMA_free(b_ColSel); b_ColSel = NULL;
   if (b_RowSel) SUMA_free(b_RowSel); b_RowSel = NULL;
   if (ColSel) free(ColSel); ColSel = NULL;
   if (NodeSel) free (NodeSel); NodeSel = NULL;
   if (RowSel)   free(RowSel); RowSel = NULL;
   if (pn) SUMA_Free_Parsed_Name(pn); pn = NULL;
   SUMA_RETURN(dset);
}

static int AddIndex_1D = 0;
void SUMA_SetAddIndex_1D(int v) { AddIndex_1D=v; return; }
int SUMA_GetAddIndex_1D(void) { return(AddIndex_1D); }

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
char * SUMA_WriteDset_ns (char *Name, SUMA_DSET *dset, SUMA_DSET_FORMAT form, int overwrite, int verb) 
{
   char *c=SUMA_WriteDset_eng (Name, dset, form, overwrite, verb);
   WorkErrLog_ns();
   return(c);
} 
char * SUMA_WriteDset_eng (char *Name, SUMA_DSET *dset, SUMA_DSET_FORMAT form, int overwrite, int verb) 
{
   static char FuncName[]={"SUMA_WriteDset_eng"};
   char *PrefOut = NULL, *NameOut = NULL, *strmname=NULL, stmp[500], *eee=NULL, *oName=NULL;
   int flg = 0, exists = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset) { SUMA_PushErrLog("SL_Err", "NULL dset", FuncName); SUMA_RETURN(NameOut); }
   if (LocalHead) {
      SUMA_LH("About to write dset");
      SUMA_ShowDset(dset, 0, NULL);
   }  
   if (!dset->ngr) { SUMA_PushErrLog("SL_Err","NULL dset->ngr", FuncName); SUMA_RETURN(NameOut); }
   if (!Name) { 
      if (!(Name=NI_get_attribute(dset->ngr, "filename"))) {
         SUMA_PushErrLog("SL_Err","NULL Name", FuncName); SUMA_RETURN(NameOut); 
      }
   } else {
      /* call rename dset */
      if (!SUMA_RenameDset(dset, Name)) {
         SUMA_PushErrLog("SL_Err","Failed to rename dset", FuncName); SUMA_RETURN(NameOut);
      }
   }  
   PrefOut = SUMA_RemoveDsetExtension_ns(Name, form);
   if (!PrefOut) { SUMA_PushErrLog("SL_Err","Failed to write dset", FuncName); SUMA_RETURN(NameOut); }
   
   /* take care of ambiguous NIML form */
   if (form == SUMA_NIML) {
      if (AFNI_yesenv("AFNI_NIML_TEXT_DATA")) {
         form = SUMA_ASCII_NIML;
      } else {
         form = SUMA_BINARY_NIML;
      }
   }
   
   exists = 0;
   switch (form) {
      case SUMA_NIML:
      case SUMA_ASCII_NIML:
      case SUMA_BINARY_NIML:
         NameOut = SUMA_Extension(PrefOut, ".niml.dset", NOPE);
         if (!overwrite &&  SUMA_filexists(NameOut)) {
            exists = 1;
         } else {
            strmname = SUMA_append_string("file:",NameOut);
            NI_set_attribute(dset->ngr,"filename", NameOut);
            if (form == SUMA_ASCII_NIML) { 
              SUMA_LH("Writing NIML, ASCII..."); SUMA_LH(strmname);  NEL_WRITE_TX (dset->ngr, strmname, flg);  SUMA_LH("DONE.");
            } else { 
              SUMA_LH("Writing NIML, BINARY..."); SUMA_LH(strmname); NEL_WRITE_BI (dset->ngr, strmname, flg); SUMA_LH("DONE.");
            }
            if (!flg) {
               SUMA_PushErrLog("SL_Err","Failed to write niml element", FuncName);
            } else {
               SUMA_LH("DONE.");
            }
         }
         break;
      case SUMA_NIML_STDERR:
      case SUMA_NIML_STDOUT:
         if (form == SUMA_NIML_STDOUT) { 
           SUMA_LH("Writing NIML, STDOUT..."); SUMA_LH(strmname); NEL_WRITE_TX (dset->ngr, "stdout:", flg);  SUMA_LH("DONE.");
         } else { 
           SUMA_LH("Writing NIML, STDERR..."); SUMA_LH(strmname); NEL_WRITE_TX (dset->ngr, "stderr:", flg); SUMA_LH("DONE.");
         }
         if (!flg) {
            SUMA_PushErrLog("SL_Err","Failed to write element", FuncName);
         } else {
            SUMA_LH("DONE.");
         }
         break;
      case SUMA_1D:
         NameOut = SUMA_Extension(PrefOut, ".1D.dset", NOPE);
         if (!overwrite &&  SUMA_filexists(NameOut)) {
            exists = 1;
         } else {
            NI_set_attribute(dset->ngr,"filename", NameOut);
            strmname = SUMA_append_string("file:",NameOut);
	         SUMA_LH("Writing 1D..."); SUMA_LH(strmname); 
            DSET_WRITE_1D (dset, strmname, flg, AddIndex_1D);
            if (!flg) {
               SUMA_PushErrLog("SL_Err","Output file not written", FuncName);
            } else {
               SUMA_LH("DONE.");
            }
         } 
         break;
      case SUMA_1D_PURE:
         NameOut = SUMA_Extension(PrefOut, ".1D.dset", NOPE);
         if (!overwrite &&  SUMA_filexists(NameOut)) {
            exists = 1;
         } else {
            NI_set_attribute(dset->ngr,"filename", NameOut);
            strmname = SUMA_copy_string(NameOut);
	         SUMA_LH("Writing 1D pure..."); SUMA_LH(strmname); 
            DSET_WRITE_1D_PURE (dset, strmname, flg, AddIndex_1D);
            if (!flg) {
               SUMA_PushErrLog("SL_Err","Output file not written", FuncName);
            } else {
               SUMA_LH("DONE.");
            }
         } 
         break;
      case SUMA_1D_STDOUT:
         DSET_WRITE_1D (dset, "stdout:", flg, AddIndex_1D);
         if (!flg) {
            SUMA_PushErrLog("SL_Err","Output file not written", FuncName);
         } else {
            SUMA_LH("DONE.");
         }
         break;
      case SUMA_1D_STDERR:
         DSET_WRITE_1D (dset, "stderr:", flg, AddIndex_1D);
         if (!flg) {
            SUMA_PushErrLog("SL_Err","Output file not written", FuncName);
         } else {
            SUMA_LH("DONE.");
         }
         break;
      case SUMA_NO_DSET_FORMAT:
         SUMA_PushErrLog("SLP_Err","Must specify output format", FuncName);
         break;
      default:
         SUMA_PushErrLog("SLP_Err","Bad format specification", FuncName);
         break;
   }
   
   if (exists) {
      snprintf(stmp, 500*sizeof(char), "Output file %s exists.\n Will not overwrite.", NameOut);
      SUMA_PushErrLog("SLP_Err",stmp, FuncName);
      if (NameOut) SUMA_free(NameOut); NameOut = NULL;
   } else if (!NameOut || !flg) {
      if (verb) SUMA_PushErrLog("SLP_Err","Failed writing dataset.", FuncName);
      if (NameOut) SUMA_free(NameOut); NameOut = NULL;
   }
   
   if (PrefOut) SUMA_free(PrefOut); PrefOut = NULL;
   if (strmname) SUMA_free(strmname); strmname = NULL;
   SUMA_RETURN(NameOut);
}

/*!
   \brief Guess file format from extension
   \param Name (char *) name 
   \return form SUMA_DSET_FORMAT
*/
SUMA_DSET_FORMAT SUMA_GuessFormatFromExtension(char *Name)
{
   static char FuncName[]={"SUMA_GuessFormatFromExtension"};
   SUMA_DSET_FORMAT form = SUMA_NO_DSET_FORMAT;
     
   SUMA_ENTRY;
   
   if (!Name) { SUMA_SL_Err("NULL Name"); SUMA_RETURN(form); }
   if (SUMA_isExtension(Name, ".niml.dset")) form = SUMA_NIML;
   if (SUMA_isExtension(Name, ".1D.dset")) form = SUMA_1D;
   if (SUMA_isExtension(Name, ".niml.cmap")) form = SUMA_NIML;
   if (SUMA_isExtension(Name, ".1D.cmap")) form = SUMA_1D; 
   if (SUMA_isExtension(Name, ".dx")) form = SUMA_ASCII_OPEN_DX_DSET; 
   if (SUMA_isExtension(Name, ".dx.dset")) form = SUMA_ASCII_OPEN_DX_DSET; 
   SUMA_RETURN(form);
}

const char *SUMA_ExtensionOfDsetFormat (SUMA_DSET_FORMAT form)
{
   static char FuncName[]={"SUMA_ExtensionOfDsetFormat"};
   
   SUMA_ENTRY;
   
   switch (form) {
      case SUMA_NIML:
      case SUMA_ASCII_NIML:
      case SUMA_BINARY_NIML:
         SUMA_RETURN(".niml.dset");
      case SUMA_1D:
         SUMA_RETURN(".1D.dset");
      case SUMA_ASCII_OPEN_DX_DSET:
         SUMA_RETURN(".dx.dset");
      default:
         SUMA_RETURN(""); 
   }
   
   SUMA_RETURN("huh");
}

/*!
   \brief Removes the standard extension from a dataset filename
   \param Name (char *) name 
   \param form SUMA_DSET_FORMAT
   \return (char *) no_extension (you have to free that one with SUMA_free)
*/
char *SUMA_RemoveDsetExtension_ns (char*Name, SUMA_DSET_FORMAT form)
{
   char *c=SUMA_RemoveDsetExtension_eng (Name,  form);
   WorkErrLog_ns();
   return(c);
}
char *SUMA_RemoveDsetExtension_eng (char*Name, SUMA_DSET_FORMAT form)
{
   static char FuncName[]={"SUMA_RemoveDsetExtension_eng"};
   char *noex = NULL, *tmp = NULL;
   
   SUMA_ENTRY;
   
   if (!Name) { SUMA_SL_Err("NULL Name"); SUMA_RETURN(NULL); }
  
   switch (form) {
      case SUMA_NIML:
      case SUMA_ASCII_NIML:
      case SUMA_BINARY_NIML:
         noex  =  SUMA_Extension(Name, ".niml.dset", YUP);
         break;
      case SUMA_1D:
      case SUMA_1D_PURE:
         tmp  =  SUMA_Extension(Name, ".1D", YUP);
         noex  =  SUMA_Extension(tmp, ".1D.dset", YUP); SUMA_free(tmp); tmp = NULL;
         break;
      case SUMA_ASCII_OPEN_DX_DSET:
         tmp  =  SUMA_Extension(Name, ".dx", YUP);
         noex  =  SUMA_Extension(tmp, ".dx.dset", YUP); SUMA_free(tmp); tmp = NULL;
         break;
      case SUMA_NO_DSET_FORMAT:
         tmp  =  SUMA_Extension(Name, ".1D", YUP);
         noex = SUMA_Extension(tmp, ".1D.dset", YUP); SUMA_free(tmp); tmp = NULL; tmp = noex;
         noex = SUMA_Extension(tmp, ".niml.dset", YUP); SUMA_free(tmp); tmp = NULL;
         break;
      case SUMA_NIML_STDOUT:
      case SUMA_NIML_STDERR:
      case SUMA_1D_STDOUT:
      case SUMA_1D_STDERR:
         noex = SUMA_copy_string(Name);
         break;
      default:
         SUMA_PushErrLog("SLP_Err","Bad format specification", FuncName);
         break;
   }
   
   SUMA_RETURN(noex);
}

/*!
   \brief a function to turn the old dataset NI_element to the new
   dataset NI_group structure. Only essentials are preserved. Some
   of the attributes will remain empty in the data element, no simple
   way at the moment to remove attributes from a nel
*/
NI_group *SUMA_oDsetNel2nDsetNgr(NI_element *nel) 
{
   static char FuncName[]={"SUMA_oDsetNel2nDsetNgr"};
   NI_group *ngr = NULL;
   NI_element *nelb = NULL;
   char *idcode=NULL, *dname=NULL, *col_label = NULL, *stmp=NULL;
   int ctp, i;
   NI_element *dnel = NULL, *inel = NULL;
   SUMA_DSET dset; /* dummy */
   SUMA_ENTRY;

   ngr = NI_new_group_element();
   NI_rename_group(ngr, nel->name);

   /* copy the ID */
   idcode = NI_get_attribute(nel,"idcode");
   if (!idcode) idcode = NI_get_attribute(nel,"ni_idcode");
   if (idcode) {
      NI_set_attribute(ngr, "self_idcode", idcode);
   } else {
      SUMA_NEW_ID(idcode, NULL);
      NI_set_attribute(ngr, "self_idcode", idcode); SUMA_free(idcode); idcode = NULL;
   }
   /* the domain parent */
   idcode = NI_get_attribute(nel,"DomParent_idcode");
   if (idcode) {
      NI_set_attribute(ngr, "domain_parent_idcode", idcode);
   } else {
      NI_set_attribute(ngr, "domain_parent_idcode", NULL); 
   }
   
   /* the geometry domain parent */
   idcode = NI_get_attribute(nel,"geometry_parent_idcode");
   if (idcode) {
      NI_set_attribute(ngr, "geometry_parent_idcode", idcode);
   } else {
      NI_set_attribute(ngr, "geometry_parent_idcode", NULL); 
   }
   
   /* form the data nel */
   dname = SUMA_append_string(NEL_DSET_TYPE(ngr), "_data");
   dnel = NI_new_data_element("SPARSE_DATA", nel->vec_len); 
   NI_set_attribute (dnel, "data_type", dname); 
   SUMA_free(dname); dname = NULL;
   NI_add_to_group(ngr, dnel);

   /* Now add the node index element */
   dname = SUMA_append_string(NEL_DSET_TYPE(ngr), "_node_indices");
   inel = NI_new_data_element("INDEX_LIST", nel->vec_len); 
   NI_set_attribute (inel, "data_type", dname); 
   SUMA_free(dname); dname = NULL;
   NI_add_to_group(ngr, inel);

   /* now, manually add the columns' spots */
   for (i=0; i<nel->vec_num; ++ i) {
      ctp = SUMA_TypeOfColNumb(nel, i);
      if (ctp != SUMA_NODE_INDEX) {
         switch (SUMA_ColType2TypeCast(ctp)) {  /* MUST use the old function SUMA_TypeOfColNumb here ! */
            case SUMA_int:
               NI_add_column_stride ( dnel, NI_INT, NULL, 1);
               break;
            case SUMA_float:
               NI_add_column_stride ( dnel, NI_FLOAT, NULL, 1 );      
               break;
            case SUMA_byte:
               NI_add_column_stride ( dnel, NI_BYTE, NULL, 1 );      
               break;
            case SUMA_double:
               NI_add_column_stride ( dnel, NI_DOUBLE, NULL, 1 );      
               break;
            case SUMA_string:
               NI_add_column_stride ( dnel, NI_STRING, NULL, 1 );
               break;
            default:
               fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
               NI_free_element(ngr);
               SUMA_RETURN(NULL);
               break;
         } 
         /* copy the vector's pointer */
         dnel->vec[i] = nel->vec[i]; nel->vec[i] = NULL;
         /* set some generic attributes */
         dset.dnel = dnel; dset.ngr = ngr;
         SUMA_AddGenDsetColAttr (&dset, ctp, dnel->vec[i], 1, -1);
         /* add the attributes of that column */
         col_label = SUMA_ColLabelCopy(nel, i, 0);
         SUMA_AddDsetColAttr (&dset, col_label, ctp, NULL, -1);
         if (col_label) SUMA_free(col_label); col_label = NULL;
      } else {
         switch (SUMA_ColType2TypeCast(ctp)) {  /* MUST use the old function SUMA_TypeOfColNumb here ! */
            case SUMA_int:
               NI_add_column_stride ( inel, NI_INT, NULL, 1);
               break;
            case SUMA_float:
               NI_add_column_stride ( inel, NI_FLOAT, NULL, 1 );      
               break;
            case SUMA_byte:
               NI_add_column_stride ( inel, NI_BYTE, NULL, 1 );      
               break;
            case SUMA_double:
               NI_add_column_stride ( inel, NI_DOUBLE, NULL, 1 );      
               break;
            case SUMA_string:
               NI_add_column_stride ( inel, NI_STRING, NULL, 1 );
               break;
            default:
               fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
               NI_free_element(ngr);
               SUMA_RETURN(NULL);
               break;
         }
         inel->vec[i] = nel->vec[i]; nel->vec[i] = NULL;
         /* set some generic attributes */
         dset.inel = inel; dset.ngr = ngr; 
         SUMA_AddGenDsetColAttr (&dset, ctp, inel->vec[0], 1, -1);
      }      
   }   
   
   /* add the history note */
   stmp = NI_get_attribute(nel, "HISTORY_NOTE");
   if (stmp) {
      nelb = NI_new_data_element("AFNI_atr", 1);
      NI_set_attribute(nelb,"atr_name", "HISTORY_NOTE");
      NI_add_column_stride ( nelb, NI_STRING, NULL, 1 );
      NI_add_to_group(ngr, nelb);
      /* now add the new string */
      SUMA_NEL_REPLACE_STRING(nelb, 0, 0, (void*)stmp);
   }
   
   
   SUMA_RETURN(ngr);
}

/*!

   \brief Load a surface-based data set of the niml format
   \param Name (char *) name or prefix of dataset
   \param verb (int) level of verbosity. 0 mute, 1 normal, 2 dramatic perhaps
   \return dset (SUMA_DSET *)
   
   - Reads one ni element only
*/
SUMA_DSET *SUMA_LoadNimlDset (char *Name, int verb)
{
   static char FuncName[]={"SUMA_LoadNimlDset"};
   char *FullName = NULL, *niname = NULL;
   NI_stream ns = NULL;
   void *nini=NULL;
   SUMA_DSET *dset=NULL;
   int tt;
   SUMA_Boolean iselement = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!Name) { SUMA_SL_Err("Null Name"); SUMA_RETURN(dset); }
   
   /* work the name */
   if (!SUMA_filexists(Name)) {
      /* try the extension game */
      FullName = SUMA_Extension(Name, ".niml.dset", NOPE);
      if (!SUMA_filexists(FullName)) {
         if (verb)  { SUMA_SL_Err("Failed to find dset file."); }
         if (FullName) SUMA_free(FullName); FullName = NULL;
         SUMA_RETURN(dset);
      }
   }else {
      FullName = SUMA_copy_string(Name);
   }
   
   /* got the name, now load it */
   niname = SUMA_append_string("file:", FullName);
   
   ns = NI_stream_open(niname, "r");
   if (!ns) {
      SUMA_SL_Crit("Failed to open NI stream for reading.");
      if (FullName) SUMA_free(FullName); FullName = NULL;
      SUMA_RETURN(dset);
   }
   
   nini = NI_read_element(ns, 1) ; 
   NI_stream_close( ns ) ; ns = NULL;
   tt = NI_element_type(nini);
    
   SUMA_LH("Checking on nini type");
   /* check if group or element */
   if(tt == NI_GROUP_TYPE) {
      iselement = NOPE; 
      SUMA_LH("Dealing with group");
   } else if (tt == NI_ELEMENT_TYPE) { 
      iselement = YUP; 
      SUMA_LH("Dealing with element");
   } else {
      fprintf(SUMA_STDERR, "Note %s: %s has no element and no group. \n"
                           "Perhaps it is a .1D read in as a niml dset.\n", FuncName, Name);
      SUMA_RETURN(NULL);
   }
   
   
   if (iselement) {
      dset = SUMA_NewDsetPointer();
      dset->ngr = SUMA_oDsetNel2nDsetNgr((NI_element *)nini);
      dset->dnel = SUMA_FindDsetDataElement(dset);
      dset->inel = SUMA_FindDsetNodeIndexElement(dset);
      if (!dset->dnel) {
         SUMA_SL_Warn("Failed to find dset data element");
      }
      if (!dset->inel) {
         SUMA_SL_Warn("Failed to find dset node index element");
      }
      NI_free_element((NI_element *)nini);
      #ifdef OLD_DSET
      if (!nel) {
         if (verb) { SUMA_SL_Err("Failed to read dset."); }
      } else {
         /* Now store that baby in Dset */
         dset = SUMA_NewDsetPointer();
         dset->nel = (NI_element *)nel; nini = NULL;
      }
      #endif
   } else {
      if (!(dset = SUMA_ngr_2_dset((NI_group *)nini))) {
         SUMA_SL_Err("Failed to go from ngr to dset");
         SUMA_RETURN(NULL);
      }
   }
   
   /* make sure inel is initialized*/
   if (!dset->inel || !SDSET_NODEINDLEN(dset)) { 
      SUMA_SL_Err("Bad dset->inel\nOld niml dset?"); 
      SUMA_ShowDset(dset,0, NULL); 
      SUMA_DUMP_TRACE("Bad dset->inel, dumping trace for debug:");
      SUMA_FreeDset(dset); dset = NULL; 
      SUMA_RETURN(dset); 
   }
   
   /* done, clean up and out you go */
   if (niname) SUMA_free(niname); niname = NULL;      
   if (FullName) SUMA_free(FullName); FullName = NULL;
   SUMA_RETURN(dset);
}

/*!
   \brief get the data values 
*/
SUMA_Boolean SUMA_OpenDx_Object_Data(char *op, int nchar, SUMA_OPEN_DX_STRUCT *dx)
{
   static char FuncName[]={"SUMA_OpenDx_Object_Data"};
   int i, Found = 0, ival;
   char *op_end, cend, *op2, *sval;
   char *op_orig;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the data */
   op_orig = op; /* hide this pointer from the evils that will befall it */
   cend = op_orig[nchar-1]; op_orig[nchar-1] = '\0';
   op_end = op_orig + nchar - 1; 
   if (LocalHead) { /* potential for huge dump if you set show to nchar! */
         int j, show;
         show = 500; /* could also use nchar */
         fprintf(SUMA_STDERR,"%s Object\n", FuncName);
         j=0; while (op[j] && j<500) { fprintf(SUMA_STDERR,"%c", op[j]); ++j; }
         fprintf(SUMA_STDERR,"\n");
   }
   SUMA_ADVANCE_PAST(op,op_end,"data",Found,1);
   sval = NULL;
   if (Found) {
      /* get the data's info */
      SUMA_GET_BETWEEN_BLANKS(op, op_end, op2);
      if (op2 == op) {
         SUMA_LH("Empty data?");
         dx->data=NULL;
      } else {
         SUMA_COPY_TO_STRING(op, op2, sval);
         dx->data = sval; sval = NULL;
      }
      op = op2;
      /* now fill datap if possible*/
      if (dx->data && strstr(dx->data,"follows")){
         int nread=0;
         SUMA_LH("data inside");
         if (LocalHead) { /* potential for huge dump! */
            int j, show;
            show = 500; /* could also use nchar */
            fprintf(SUMA_STDERR,"%s Object\n", FuncName);
            j=0; while (op[j] && j<500) { fprintf(SUMA_STDERR,"%c", op[j]); ++j; }
            fprintf(SUMA_STDERR,"\n");
         }  
         dx->datap = SUMA_strtol_vec(op, dx->items*SUMA_NCOL_OPENDX(dx), &nread, SUMA_CTypeName2VarType (dx->type));
         if (LocalHead) {
            fprintf(SUMA_STDERR,"%s: Read %d/%d values\n", FuncName, nread, dx->items*SUMA_NCOL_OPENDX(dx));
         }
         if (nread != dx->items*SUMA_NCOL_OPENDX(dx)) {
            fprintf(SUMA_STDERR,"Error %s: read in %d values, expected %d \n", FuncName, nread, dx->items*SUMA_NCOL_OPENDX(dx));
            op_orig[nchar-1] =  cend;
            SUMA_RETURN(NOPE);
         }
      }else {
         SUMA_LH("data does not follow");
         if (LocalHead) {
            for (i=0; i < 500; ++i) { fprintf(SUMA_STDERR,"%c", op[i]); } fprintf(SUMA_STDERR,"\n"); fflush(SUMA_STDERR);
         }
         /* ? have file name ? */
         if (strstr(dx->data,"file")) {
            SUMA_GET_BETWEEN_BLANKS(op, op_end, op2);
            if (op2 > op) {
               SUMA_free(dx->data); /* free the "file" string */
               SUMA_COPY_TO_STRING(op, op2, sval);
               dx->data = sval; sval = NULL;
               /* search backwards for a comma */
               i=strlen(dx->data)-1; Found = -1;
               while(i>=0 && Found <0) { if (dx->data[i] == ',') Found = i; --i; }
               if (Found >= 0) { 
                  dx->data_off = SUMA_copy_string(&(dx->data[Found+1]));
                  dx->data[Found]='\0';
               }  
               /* see if you have some byte order or binary business */
               dx->data_format = 0; /* ascii, default*/
               op = op_orig; SUMA_ADVANCE_PAST(op,op_end,"binary",Found,1);
               if (Found) {
                  dx->data_format = MSB_FIRST; /* default */
               }
               /* endianness, regardless of what was above, "binary" might not occur */
               op = op_orig; SUMA_ADVANCE_PAST(op,op_end,"msb",Found,1);
               if (Found) { dx->data_format = MSB_FIRST; }
               else {
                  op = op_orig; SUMA_ADVANCE_PAST(op,op_end,"lsb",Found,1);
                  if (Found) { dx->data_format = LSB_FIRST; }
               }
            }
         }
      }
   } else {
      SUMA_LH("No data for this object");
   }
      
   op_orig[nchar-1] =  cend;
   SUMA_RETURN(YUP);
}


/*!
   \brief return values of an attribute from an OpenDX object string
*/  
SUMA_Boolean SUMA_OpenDx_Object_Attr(char *op, int nchar, SUMA_OPEN_DX_STRUCT *dx)
{
   static char FuncName[]={"SUMA_OpenDx_Object_Attr"};
   int i, Found, ival,imax;
   char *op_end, cend, *op2, *sval;
   char *op_orig;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the attributes */
   
   op_orig = op; /* hide this pointer from the evils that will befall it */
   cend = op_orig[nchar-1]; op_orig[nchar-1] = '\0';
   op_end = op_orig + nchar - 1; 
   
   SUMA_ADVANCE_PAST(op,op_end,"attribute",Found, 1);
   sval = NULL;
   while (Found) {
      /* get the attribute's name */
      SUMA_GET_BETWEEN_BLANKS(op, op_end, op2);
      if (op2 == op) {
         SUMA_LH("Empty attribute?");
      } else {
         imax = op2 - op;
         if (imax > 5000) {
            SUMA_SL_Err("Unexpectedly large field!");
            op_orig[nchar-1] =  cend;
            SUMA_RETURN(NOPE);
         }else if (imax < 0) {
            SUMA_SL_Err("Negative imax!");
            op_orig[nchar-1] =  cend;
            SUMA_RETURN(NOPE);
         }
         sval = (char *)SUMA_calloc(imax + 2, sizeof(char));

         for (i=0; i < imax; ++i) sval[i] = op[i];
         sval[imax] = '\0';
         dx->attr_name[dx->n_attr] = sval;
      }
      op = op2;
      /* look for attribute string */
      SUMA_ADVANCE_PAST(op,op_end,"string",Found,1);
      if (Found) {
         SUMA_GET_BETWEEN_BLANKS(op, op_end, op2);
         if (op2 == op) {
            SUMA_LH("Empty string?");
         } else {
            imax = op2 - op;
            if (imax > 5000) {
               SUMA_SL_Err("Unexpectedly large field!");
               op_orig[nchar-1] =  cend;
               SUMA_RETURN(NOPE);
            }else if (imax < 0) {
               SUMA_SL_Err("Negative imax!");
               op_orig[nchar-1] =  cend;
               SUMA_RETURN(NOPE);
            }
            sval = (char *)SUMA_calloc(imax + 2, sizeof(char));

            for (i=0; i < imax; ++i) sval[i] = op[i];
            sval[imax] = '\0';
            dx->attr_string[dx->n_attr] = sval;
         }   
      }
      ++dx->n_attr;
      /* look for next attribute */
      op = op2;
      SUMA_ADVANCE_PAST(op,op_end,"attribute",Found,1);
   }
      
   op_orig[nchar-1] =  cend;
   SUMA_RETURN(YUP);
}

/*!
   \brief return values of a header field from an OpenDX object string
   If you expect an int back like when attr is "rank" or "shape"
      ans should be type cast to (int *) before use: int ival; ival = *((int *)ans); 
   If you expect a bunch of numbers like for "counts" "origin" "delta"
      then ans is (SUMA_IVEC *) or (SUMA_FVEC*) or (SUMA_DVEC *)
      These are freed with SUMA_free(ans->v); SUMA_free(ans);
   else ans should be type cast to (char **) before use: 
      char *sval; sval = *((char **)ans); then free sval with SUMA_free(sval); 
      
*/  
void * SUMA_OpenDx_Object_Header_Field(char *op, int nchar, const char *attr, char **opeofield)
{
   static char FuncName[]={"SUMA_OpenDx_Object_Header_Field"};
   void *ans=NULL;
   int i, Found, ival, imax, nread;
   char *op_end = NULL, cend, *op2, *sval, *pp1, *pp2;
   char *op_orig;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (opeofield) *opeofield = op;
   
   if (!attr) SUMA_RETURN(ans);
   op_orig = op; /* hide this pointer from the evils that will befall it */
   cend = op_orig[nchar-1]; op_orig[nchar-1] = '\0';
   /* do we have a data section to signal end of header? */
   op_end = NULL;
   pp1 = strstr(op, "data"); 
   if (pp1) {
      pp2 = strstr(op, "follows");
      if (pp2) op_end = pp2;
   }
   if (!op_end) op_end = op_orig + nchar - 1; /* op_end all the way at end */
   
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: Object of %d chars, looking for >>>%s<<<\n", FuncName, nchar, attr );
   }
   
  /* get the header field's value name */
   SUMA_ADVANCE_PAST(op,op_end,attr,Found,1);
   if (Found) {
      SUMA_GET_BETWEEN_BLANKS(op, op_end, op2);
      if (op2 == op) {
         SUMA_LH("No field");
      } else {
         /* get the numeric fields changed*/
         if (strstr(attr,"rank") || strstr(attr,"shape") || strstr(attr,"items")) { /* rank, shape (matrix's second dim) are integer vals */
            ival = (int)strtol(op, NULL , 10);
            ans = (void*)&ival;
         } else if (strstr(attr,"counts")) { /* are a series integer vals */
            ans = SUMA_AdvancePastNumbers(op, &op2, SUMA_int);
         } else if (strstr(attr,"origin") || strstr(attr,"delta")) { /* are integer vals */
            ans = SUMA_AdvancePastNumbers(op, &op2, SUMA_float);
         } else  { /* strings*/
            imax = op2 - op;
            if (imax > 5000) {
               SUMA_SL_Err("Unexpectedly large field!");
               op_orig[nchar-1] =  cend;
               SUMA_RETURN(ans);
            }else if (imax < 0) {
               SUMA_SL_Err("Negative imax!");
               op_orig[nchar-1] =  cend;
               SUMA_RETURN(ans);
            }
            sval = (char *)SUMA_calloc(imax + 2, sizeof(char));
            
            for (i=0; i < imax; ++i) sval[i] = op[i];
            sval[imax] = '\0';
            ans = (void*)&sval;
         }    
         if (LocalHead) {
            fprintf(SUMA_STDERR,"%s: attribute >>>%s<<< is:\n", FuncName, attr);
            i = 0;
            while (op[i] && op+i != op2) {
               fprintf(SUMA_STDERR,"%c", op[i]); 
               ++i;   
            }
            fprintf(SUMA_STDERR,"\n");
         }
         
      }
      op = op2; /* advance op */
   } else {
      if (strstr(attr,"class")) {
         /* it looks like "class" is sometimes omitted, look for string field, which is a class */
         SUMA_ADVANCE_PAST(op,op_end,"field", Found,1);
         if (Found) sval = SUMA_copy_string("field");
         ans = (void*)&sval;
      } else {
         SUMA_LH("No such attribute");
      }
   }
      
   op_orig[nchar-1] =  cend;
   if (opeofield) *opeofield = op; 
   SUMA_RETURN(ans);
}

/*!
   \brief return values of an attribute from an OpenDX object string
*/  
SUMA_Boolean SUMA_OpenDx_Object_Components(char *op, int nchar, SUMA_OPEN_DX_STRUCT *dx)
{
   static char FuncName[]={"SUMA_OpenDx_Object_Components"};
   int i, Found, ival,imax;
   char *op_end, cend, *op2, *sval;
   char *op_orig;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* get the attributes */
   
   op_orig = op; /* hide this pointer from the evils that will befall it */
   cend = op_orig[nchar-1]; op_orig[nchar-1] = '\0';
   op_end = op_orig + nchar - 1; 
   
   SUMA_ADVANCE_PAST(op,op_end,"component",Found,1);
   while (Found) {
      /* get the attribute's name */
      SUMA_GET_BETWEEN_BLANKS(op, op_end, op2);
      if (op2 == op) {
         SUMA_LH("Empty component?");
      } else {
         imax = op2 - op;
         if (imax > 5000) {
            SUMA_SL_Err("Unexpectedly large field!");
            op_orig[nchar-1] =  cend;
            SUMA_RETURN(NOPE);
         }else if (imax < 0) {
            SUMA_SL_Err("Negative imax!");
            op_orig[nchar-1] =  cend;
            SUMA_RETURN(NOPE);
         }
         sval = (char *)SUMA_calloc(imax + 2, sizeof(char));

         for (i=0; i < imax; ++i) sval[i] = op[i];
         sval[imax] = '\0';
         dx->comp_name[dx->n_comp] = sval;
      }
      op = op2;
      /* look for attribute string */
      SUMA_ADVANCE_PAST(op,op_end,"value",Found,1);
      if (Found) {
         SUMA_GET_BETWEEN_BLANKS(op, op_end, op2);
         if (op2 == op) {
            SUMA_SL_Err("No value!");
         } else {
            imax = op2 - op;
            if (imax > 5000) {
               SUMA_SL_Err("Unexpectedly large field!");
               op_orig[nchar-1] =  cend;
               SUMA_RETURN(NOPE);
            }else if (imax < 0) {
               SUMA_SL_Err("Negative imax!");
               op_orig[nchar-1] =  cend;
               SUMA_RETURN(NOPE);
            }
            sval = (char *)SUMA_calloc(imax + 2, sizeof(char));

            for (i=0; i < imax; ++i) sval[i] = op[i];
            sval[imax] = '\0';
            dx->comp_value[dx->n_comp] = sval;
         }   
      }else { /* try for non-existing "value" */
         SUMA_GET_BETWEEN_BLANKS(op, op_end, op2);
         if (op2 == op) {
            SUMA_SL_Err("No value at all");
         } else {
            imax = op2 - op;
            if (imax > 5000) {
               SUMA_SL_Err("Unexpectedly large field!");
               op_orig[nchar-1] =  cend;
               SUMA_RETURN(NOPE);
            }else if (imax < 0) {
               SUMA_SL_Err("Negative imax!");
               op_orig[nchar-1] =  cend;
               SUMA_RETURN(NOPE);
            }
            sval = (char *)SUMA_calloc(imax + 2, sizeof(char));

            for (i=0; i < imax; ++i) sval[i] = op[i];
            sval[imax] = '\0';
            dx->comp_value[dx->n_comp] = sval;
         }   
      }
      ++dx->n_comp;
      /* look for next component */
      op = op2;
      SUMA_ADVANCE_PAST(op,op_end,"component",Found,1);
   }
      
   op_orig[nchar-1] =  cend;
   SUMA_RETURN(YUP);
}

/*!
   \brief A function to create a dataset out of an OpenDX object
   \param FullName (char *) the filename
   \param dset_id (char *) if null, SUMA_CreateDsetPointer will create one
   \param dom_id (char *) domain idcode null if you have none
   \param dx (SUMA_OPEN_DX_STRUCT *) pointer to OpenDX object
   \return dset (SUMA_DSET *) NULL if trouble, of course. 
*/
SUMA_DSET *SUMA_OpenDX2dset( char *FullName, char *dset_id, char *dom_id, 
                                SUMA_OPEN_DX_STRUCT *dx ) 
{
   static char FuncName[]={"SUMA_OpenDX2dset"};
   SUMA_DSET *dset = NULL;
   int i = 0;

   SUMA_ENTRY;
   
   if (!FullName) { SUMA_SL_Err("Need a FullName"); SUMA_RETURN(dset); }
   if (!dx) { SUMA_SL_Err("NULL dx"); SUMA_RETURN(dset); }
   
   dset = SUMA_CreateDsetPointer( FullName, SUMA_NODE_BUCKET, dset_id, dom_id,  dx->items); 
   
   /* now add the columns */
   
   for (i=0; i<SUMA_NCOL_OPENDX(dx); ++i) {
      if (!SUMA_AddDsetNelCol (dset, "dx_col", SUMA_VarType2ColType (dx->type), dx->datap+i, NULL , SUMA_NCOL_OPENDX(dx))) {
         SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
         SUMA_FreeDset((void*)dset); dset = NULL;
         SUMA_RETURN(dset);
      }
   }
   

   SUMA_RETURN(dset);
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
SUMA_DSET *SUMA_far2dset_ns( char *FullName, char *dset_id, char *dom_id, 
                                 float **farp, int vec_len, int vec_num, 
                                 int ptr_cpy) 
{
   SUMA_DSET *dset = SUMA_far2dset_eng( FullName, dset_id, dom_id, 
                                 farp, vec_len, vec_num, 
                                 ptr_cpy);
   WorkErrLog_ns();
   return(dset);
}
SUMA_DSET *SUMA_far2dset_eng( char *FullName, char *dset_id, char *dom_id, 
                                 float **farp, int vec_len, int vec_num, 
                                 int ptr_cpy) 
{
   static char FuncName[]={"SUMA_far2dset_eng"};
   SUMA_DSET *dset = NULL;
   int i = 0;
   float *far = NULL;

   SUMA_ENTRY;
   
   if (!FullName) { SUMA_PushErrLog("SL_Err", "Need a FullName", FuncName); SUMA_RETURN(dset); }
   if (!farp) { SUMA_PushErrLog("SL_Err", "NULL farp", FuncName); SUMA_RETURN(dset); }
   far = *farp;
   if (!far) { SUMA_PushErrLog("SL_Err", "NULL *farp", FuncName); SUMA_RETURN(dset); }
   if (vec_len < 0 || vec_num < 0) { SUMA_PushErrLog("SL_Err", "Negative vec_len or vec_num", FuncName); SUMA_RETURN(dset); }
   if (ptr_cpy) { SUMA_PushErrLog("SL_Err", "Pointer copy not supported yet", FuncName); SUMA_RETURN(dset); }
   
   if (vec_num > 200 * vec_len || vec_num > 50000) { /* a warning for MSB's mishap */
      char *eee = getenv("SUMA_1D_Transponse_Warn");
      int Warn = 1;
      static int nwarn = 0;
      Warn = 1;
      if (eee) {
         if (strcmp(eee,"NO") == 0) Warn = 0; /* stay out of this */
      }  
      if (Warn) {
         if (!nwarn) {
            SUMA_PushErrLog("SLP_Warn", "Unusual 1D file dimensions.\n"
                        "Number of rows (nodes) much less\n"
                        "than number of columns (sub-bricks).\n"
                        "This warning is put up in case\n"
                        "you have the dataset incorrectly \n"
                        "transposed for some reason. Should\n"
                        "you need to transpose it again, use \n"
                        "the program 1dtranspose .\n"
                        "1D files where the number of columns\n"
                        "is much larger than the number of \n"
                        "rows will take a long time to load \n"
                        "and a longer time, much longer a times,\n"
                        "to have the X interface initialized.\n"
                        "The read operation was cancelled this\n"
                        "time, read the file again if you think\n"
                        "the file you are reading is properly \n"
                        "formatted. This warning will\n"
                        "no be shown again in this session.\n"
                        "Set the environment variable \n"
                        "SUMA_1D_Transponse_Warn = NO\n"
                        "in .sumarc if you do not want to see\n"
                        "this warning ever again.\n"
                        , FuncName);
            /* return the first time with NULL */
            ++nwarn; SUMA_RETURN(NULL);
         } 
      }
   }
   dset = SUMA_CreateDsetPointer( FullName, SUMA_NODE_BUCKET, dset_id, dom_id,  vec_len  ); 
   
   /* now add the columns */
   for (i=0; i<vec_num; ++i) {
      if (!SUMA_AddDsetNelCol (dset, "numeric", SUMA_NODE_FLOAT, (void *)(&(far[i*vec_len])), NULL ,1)) {
         SUMA_PushErrLog("SL_Crit", "Failed in SUMA_AddDsetNelCol", FuncName);
         SUMA_FreeDset((void*)dset); dset = NULL;
         SUMA_RETURN(dset);
      }
   }

   if (ptr_cpy) *farp = NULL;

   SUMA_RETURN(dset);
}

int SUMA_is_AllNumeric_dset(SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_is_AllNumeric_dset"};
   int ctp, vtp, i;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(0);
   
   for (i=0; i<SDSET_VECNUM(dset); ++i) {
      ctp = SUMA_TypeOfDsetColNumb(dset, i); 
      vtp = SUMA_ColType2TypeCast(ctp) ;
      if (vtp < SUMA_byte || vtp > SUMA_double) SUMA_RETURN(0);
   }
   
   SUMA_RETURN(1);
}

/*!
   \brief returns the indices of columns that are numeric
   and likely considered as data
   free returned pointer with SUMA_free
*/
int * SUMA_FindNumericDataDsetCols(SUMA_DSET *dset, int *N_icols)
{
   static char FuncName[]={"SUMA_FindNumericDataDsetCols"};
   int *icols = NULL, i, ctp, vtp;
   
   SUMA_ENTRY;
   
   *N_icols = -1;
   
   if (!dset || !dset->dnel) SUMA_RETURN(NULL);
   
   icols = (int *)SUMA_calloc(SDSET_VECNUM(dset), sizeof(int));
   if (!icols) {
      SUMA_S_Err("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   
   *N_icols = 0;
   for (i=0; i<SDSET_VECNUM(dset); ++i) {
      ctp = SUMA_TypeOfDsetColNumb(dset, i); 
      if (  ctp == SUMA_NODE_INDEX ||
            ctp == SUMA_NODE_ILABEL ||
            ctp == SUMA_NODE_STRING ) 
         continue;
      vtp = SUMA_ColType2TypeCast(ctp) ;
      if (vtp < SUMA_byte || vtp > SUMA_double) continue;
      
      icols[*N_icols] = i;
      ++(*N_icols);
   }
   
   SUMA_RETURN(icols);
} 

int SUMA_is_AllNumeric_ngr(NI_group *ngr) 
{
   static char FuncName[]={"SUMA_is_AllNumeric_ngr"};
   int ctp, vtp, i;
   NI_element *nelb;
   char *sname=NULL;
   SUMA_DSET dset;
   
   SUMA_ENTRY;
   
   if (!ngr) SUMA_RETURN(0);
   
   sname = SUMA_append_string(NEL_DSET_TYPE(ngr),"_data");
   nelb = SUMA_FindNgrDataElement(ngr, "SPARSE_DATA", sname);
   SUMA_free(sname); sname = NULL;
   dset.ngr = ngr;
   dset.dnel = nelb;
   
   /* this one's overkill perhaps ...*/
   sname = SUMA_append_string(NEL_DSET_TYPE(ngr),"_node_indices");
   nelb = SUMA_FindNgrDataElement(ngr, "INDEX_LIST", sname);
   SUMA_free(sname); sname = NULL;
   dset.inel = nelb;
   
   for (i=0; i<dset.dnel->vec_num; ++i) {
      ctp = SUMA_TypeOfDsetColNumb(&dset, i); 
      vtp = SUMA_ColType2TypeCast(ctp) ;
      if (vtp < SUMA_byte || vtp > SUMA_double) SUMA_RETURN(0);
   }
   
   SUMA_RETURN(1);
}

int SUMA_is_AllNumeric_nel(NI_element *nel) 
{
   static char FuncName[]={"SUMA_is_AllNumeric_nel"};
   int ctp, vtp, i;
   
   SUMA_ENTRY;
   
   SUMA_SL_Warn("Obsolete, perhaps. Check on caller.");

   if (!nel) SUMA_RETURN(0);
   
   for (i=0; i<nel->vec_num; ++i) {
      ctp = SUMA_TypeOfColNumb(nel, i); 
      vtp = SUMA_ColType2TypeCast(ctp) ;
      if (vtp < SUMA_byte || vtp > SUMA_double) SUMA_RETURN(0);
   }
   
   SUMA_RETURN(1);
}

static char *ParentOfDsetToLoad = NULL;

/*! Used to provide an identifier for the surface on which a dset will
be attached. Remember to reset this pointer to NULL after loading the dset 
This is not used (yet) for assigning a SO parent to the dset*/
void SUMA_SetParent_DsetToLoad(char *parent)
{
   ParentOfDsetToLoad = parent;
   return;
}

SUMA_OPEN_DX_STRUCT *SUMA_Alloc_OpenDX_Struct(void)
{
   static char FuncName[]={"SUMA_Alloc_OpenDX_Struct"};
   int i;
   SUMA_OPEN_DX_STRUCT *dx = NULL;
   
   SUMA_ENTRY;
   
   dx = (SUMA_OPEN_DX_STRUCT *)SUMA_malloc(sizeof(SUMA_OPEN_DX_STRUCT));
   dx->rank = 0;
   dx->shape = 0;
   dx->items = 0;
   dx->bad_data = 0;
   dx->object = NULL;
   dx->class = NULL;
   dx->type = NULL;
   dx->data = NULL;
   dx->data_format = 0;
   dx->data_off = NULL;
   dx->datap = NULL;
   dx->n_comp = 0;
   dx->counts = NULL;
   dx->n_counts = 0;
   dx->origin = NULL;
   dx->n_origin = 0;
   dx->delta = NULL;
   dx->n_delta = 0;
   for (i=0; i<SUMA_MAX_OPEN_DX_FIELD_COMPONENTS; ++i) { dx->comp_name[i] = dx->comp_value[i] =NULL; }
   dx->n_attr = 0;
   for (i=0; i<SUMA_MAX_OPEN_DX_FIELD_ATTRIBUTES; ++i) { dx->attr_name[i] = dx->attr_string[i] =NULL; }
   SUMA_RETURN(dx);
}

SUMA_OPEN_DX_STRUCT *SUMA_Free_OpenDX_Struct(SUMA_OPEN_DX_STRUCT *dx)
{
   static char FuncName[]={"SUMA_Free_OpenDX_Struct"};
   int i;
   
   SUMA_ENTRY;
   
   if (!dx) SUMA_RETURN(dx);
   if (dx->object) SUMA_free(dx->object); dx->object = NULL;
   if (dx->class) SUMA_free(dx->class); dx->class = NULL;
   if (dx->data) SUMA_free(dx->data); dx->data = NULL;
   if (dx->data_off) SUMA_free(dx->data_off); dx->data_off = NULL;
   if (dx->datap) {
      if ( SUMA_OK_OPENDX_DATA_TYPE(SUMA_CTypeName2VarType (dx->type)) ) {
         SUMA_free(dx->datap); dx->datap = NULL;
      } else {
         SUMA_SL_Warn("Do not know how to free datap.\nYou now possibly have a leak on your hands.");
      }
   }
   if (dx->type) SUMA_free(dx->type); dx->type = NULL;
   for (i=0; i<SUMA_MAX_OPEN_DX_FIELD_COMPONENTS; ++i) { 
      if (dx->comp_name[i]) SUMA_free(dx->comp_name[i]); dx->comp_name[i] = NULL;
      if (dx->comp_value[i]) SUMA_free(dx->comp_value[i]); dx->comp_value[i] =NULL; 
   }
   for (i=0; i<SUMA_MAX_OPEN_DX_FIELD_ATTRIBUTES; ++i) { 
      if (dx->attr_name[i]) SUMA_free(dx->attr_name[i]); dx->attr_name[i] = NULL;
      if (dx->attr_string[i]) SUMA_free(dx->attr_string[i]); dx->attr_string[i] =NULL; 
   }
   if (dx->origin) SUMA_free(dx->origin); 
   if (dx->delta) SUMA_free(dx->delta);
   if (dx->counts) SUMA_free(dx->counts);
   SUMA_free(dx); dx = NULL;
   SUMA_RETURN(dx);
}

void SUMA_Show_OpenDX_Struct(SUMA_OPEN_DX_STRUCT **dxv, int N_dxv, FILE *out)
{
   static char FuncName[]={"SUMA_Show_OpenDX_Struct"};
   int i, idx;
   SUMA_STRING *SS=NULL;
   char *s = NULL;
   SUMA_OPEN_DX_STRUCT *dx=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);  
   if (!dxv) SS = SUMA_StringAppend(SS, "NULL dxv\n");
   for (idx = 0; idx < N_dxv; ++idx) {
      dx = dxv[idx];
      SS = SUMA_StringAppend_va(SS, "Object Struct %d/%d\n", idx+1, N_dxv);
      if (!dx) SS = SUMA_StringAppend(SS, "NULL dx\n");
      else {
         if (dx->object) SS = SUMA_StringAppend_va(SS, "object: %s\n", dx->object);
         else SS = SUMA_StringAppend_va(SS, "object: NULL\n"); 
         if (dx->class) SS = SUMA_StringAppend_va(SS, "class: %s\n", dx->class);
         else SS = SUMA_StringAppend_va(SS, "class: NULL\n"); 
         if (dx->type) SS = SUMA_StringAppend_va(SS, "type: %s\n", dx->type);
         else SS = SUMA_StringAppend_va(SS, "type: NULL\n"); 
         if (dx->rank) SS = SUMA_StringAppend_va(SS, "rank: %d\n", dx->rank);
         else SS = SUMA_StringAppend_va(SS, "rank: 0\n"); 
         if (dx->shape) SS = SUMA_StringAppend_va(SS, "shape: %d\n", dx->shape);
         else SS = SUMA_StringAppend_va(SS, "shape: 0\n"); 
         if (dx->items) SS = SUMA_StringAppend_va(SS, "items: %d\n", dx->items);
         else SS = SUMA_StringAppend_va(SS, "items: 0\n"); 
         if (dx->counts) {
            SS = SUMA_StringAppend_va(SS, "counts: (%d vals)\n", dx->n_counts);
            s = SUMA_ShowMeSome(dx->counts, SUMA_int, dx->n_counts, 5, NULL);
            SS = SUMA_StringAppend_va(SS, "\t%s\n", s); SUMA_free(s); s = NULL;
         } else SS = SUMA_StringAppend_va(SS, "counts: NULL\n");
         if (dx->origin) {
            SS = SUMA_StringAppend_va(SS, "origin: (%d vals)\n", dx->n_origin);
            s = SUMA_ShowMeSome(dx->origin, SUMA_float, dx->n_origin, 5, NULL);
            SS = SUMA_StringAppend_va(SS, "\t%s\n", s); SUMA_free(s); s = NULL;
         } else SS = SUMA_StringAppend_va(SS, "origin: NULL\n");
         if (dx->delta) {
            SS = SUMA_StringAppend_va(SS, "delta: (%d vals)\n", dx->n_delta);
            s = SUMA_ShowMeSome(dx->delta, SUMA_float, dx->n_delta, 9, NULL);
            SS = SUMA_StringAppend_va(SS, "\t%s\n", s); SUMA_free(s); s = NULL;
         }else SS = SUMA_StringAppend_va(SS, "delta: NULL\n");
         
         if (dx->data) SS = SUMA_StringAppend_va(SS, "data: %s (Data load error %d)\n", dx->data, dx->bad_data);
         else SS = SUMA_StringAppend_va(SS, "data: NULL\n"); 
         if (dx->data_off) SS = SUMA_StringAppend_va(SS, "data_off: %s \n", dx->data_off);
         else SS = SUMA_StringAppend_va(SS, "data_off: NULL\n"); 
         SS = SUMA_StringAppend_va(SS, "data_format: %d \n", dx->data_format);
         if (dx->datap) {
            s = SUMA_ShowMeSome(dx->datap, SUMA_CTypeName2VarType(dx->type), dx->items * SUMA_NCOL_OPENDX(dx), 5, NULL);
            SS = SUMA_StringAppend_va(SS, "\t%s\n", s); SUMA_free(s); s = NULL;
         }
         if (dx->n_comp) {
            SS = SUMA_StringAppend_va(SS, "components: %d\n", dx->n_comp);
            for (i=0; i<dx->n_comp; ++i) {  
               if (dx->comp_name[i]) SS = SUMA_StringAppend_va(SS, "\tname: %s\t", dx->comp_name[i]);
               else SS = SUMA_StringAppend_va(SS, "\tname: NULL\t"); 
               if (dx->comp_value[i]) SS = SUMA_StringAppend_va(SS, "\ttype: %s\n", dx->comp_value[i]);
               else SS = SUMA_StringAppend_va(SS, "\ttype: NULL\n"); 
            }    
         } else {
            SS = SUMA_StringAppend_va(SS, "components: %d\n", dx->n_comp); 
         }
         if (dx->n_attr) {
            SS = SUMA_StringAppend_va(SS, "attributes: %d\n", dx->n_attr);
            for (i=0; i<dx->n_attr; ++i) {  
               if (dx->attr_name[i]) SS = SUMA_StringAppend_va(SS, "\tname: %s\t", dx->attr_name[i]);
               else SS = SUMA_StringAppend_va(SS, "\tname: NULL\t"); 
               if (dx->attr_string[i]) SS = SUMA_StringAppend_va(SS, "\tstring: %s\n", dx->attr_string[i]);
               else SS = SUMA_StringAppend_va(SS, "\tstring: NULL\n"); 
            }    
         } else {
            SS = SUMA_StringAppend_va(SS, "attributes: %d\n", dx->n_attr); 
         }
      }
   }
   
   SUMA_SS2S(SS,s);
   if (!out) fprintf(stdout, "%s", s);
   else fprintf(out, "%s", s);
   
   SUMA_free(s); s = NULL;
   
   SUMA_RETURNe;
}
/*!
   \sa http://opendx.npaci.edu/docs/html/pages/usrgu068.htm#Header_417
*/
SUMA_OPEN_DX_STRUCT **SUMA_OpenDX_Read(char *fname, int *nobj)
{
   static char FuncName[]={"SUMA_OpenDX_Read"};
   int nread = 0, i = 0,  iop, *ivalp=NULL, shft=0;
   char *fl=NULL, **opv = NULL, *op = NULL,*sbuf=NULL, **svalp=NULL, *ope;
   int *nchar = NULL;
   SUMA_OPEN_DX_STRUCT **dxv=NULL;
   SUMA_FVEC *fvec=NULL;
   SUMA_IVEC *ivec=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   *nobj = 0;
   
   SUMA_LH("Sucking file");
   nread = SUMA_suck_file( fname , &fl ) ;
   if (!fl) {
      SUMA_SL_Err("Failed to read file.");
      SUMA_RETURN(dxv);
   }

   if (LocalHead) fprintf(SUMA_STDERR,"%s: Read in %d chars\n", FuncName, nread);
   
   opv = (char **)SUMA_calloc(SUMA_MAX_OPEN_DX_OBJECTS, sizeof(char*));
   nchar = (int*)SUMA_calloc(SUMA_MAX_OPEN_DX_OBJECTS, sizeof(int));
   dxv = (SUMA_OPEN_DX_STRUCT **)SUMA_calloc(SUMA_MAX_OPEN_DX_OBJECTS, sizeof(SUMA_OPEN_DX_STRUCT *));
   
   /* now search for the first "object" */
   op = fl;
   iop = 0;
   shft = 0;
   do {
      op = strstr((op+shft), "object");
      if (op) {
         opv[iop] = op;
         if (iop) nchar[iop-1] = opv[iop] - opv[iop-1];
         if (LocalHead) {
            fprintf(SUMA_STDERR,"%s: Object found.\n", FuncName);
            i = 0;
            while (i<20 && op[i] !='\0') { fprintf(SUMA_STDERR,"%c",op[i]); ++i; } fprintf(SUMA_STDERR,"\n"); 
         }
         
         /* must skip beyond first "object" for next pass*/
         shft = strlen("object");
         ++iop;
      }
   } while (op && iop < SUMA_MAX_OPEN_DX_OBJECTS);
   if (iop >= SUMA_MAX_OPEN_DX_OBJECTS) {
      SUMA_SL_Warn("Too many objects, processing first SUMA_MAX_OPEN_DX_OBJECTS only"); 
   }
   
   if (iop) {/* find the length of the last object */
      op = opv[iop-1];
      while (*op !='\0') { ++op; }
      nchar[iop-1] = op - opv[iop-1];
   }
   
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: %d Objects found.\n", FuncName, iop);
   }
   
   for (i=0; i<iop; ++i) { /* process each object's header field and data*/
      if ( 0 && LocalHead) { /* potentially huge dump if nmax is not controlled*/
         int j, nmax;
         nmax = 500; /* could also use nchar[i]*/
         fprintf(SUMA_STDERR,"%s Object %d\n", FuncName, i);
         op = opv[i]; for (j=0; j<nmax; ++j) fprintf(SUMA_STDERR,"%c", op[j]);
         fprintf(SUMA_STDERR,"\n");
      }
      /* get the class */
      dxv[i] = SUMA_Alloc_OpenDX_Struct();
      ivalp = (int *)SUMA_OpenDx_Object_Header_Field(opv[i], nchar[i], "rank", NULL);
      if (ivalp) dxv[i]->rank = *ivalp;
      ivalp = (int *)SUMA_OpenDx_Object_Header_Field(opv[i], nchar[i], "shape", NULL);
      if (ivalp) dxv[i]->shape = *ivalp;
      ivalp = (int *)SUMA_OpenDx_Object_Header_Field(opv[i], nchar[i], "items", NULL);
      if (ivalp) dxv[i]->items = *ivalp;
      svalp = (char **)SUMA_OpenDx_Object_Header_Field(opv[i], nchar[i], "object", NULL);
      if (svalp) dxv[i]->object = *svalp;
      svalp = (char **)SUMA_OpenDx_Object_Header_Field(opv[i], nchar[i], "class", NULL);
      if (svalp) dxv[i]->class = *svalp;
      svalp = (char **)SUMA_OpenDx_Object_Header_Field(opv[i], nchar[i], "type", NULL);
      if (svalp) dxv[i]->type = *svalp;
      ivec = (SUMA_IVEC*)SUMA_OpenDx_Object_Header_Field(opv[i], nchar[i], "counts", NULL);
      if (ivec) { dxv[i]->counts = ivec->v; dxv[i]->n_counts = ivec->n; SUMA_free(ivec); ivec = NULL;} 
      fvec = (SUMA_FVEC*)SUMA_OpenDx_Object_Header_Field(opv[i], nchar[i], "origin", NULL);
      if (fvec) { dxv[i]->origin = fvec->v; dxv[i]->n_origin = fvec->n; SUMA_free(fvec); fvec = NULL;} 
      { /* get the deltas */
         int j, k;
         char *rf=opv[i];
         j=0; 
         while (j<dxv[i]->n_counts) {
            fvec = (SUMA_FVEC*)SUMA_OpenDx_Object_Header_Field(rf, nchar[i], "delta", &ope);
            if (fvec && fvec->v) { 
               if (fvec->n < dxv[i]->n_counts) { SUMA_SL_Warn("Bad assumption about delta field.\nExpect disasters!"); }
               if (fvec->n > dxv[i]->n_counts) { 
                  SUMA_SL_Err("More values in delta that counts! Limiting to counts.\nExpect tragedy."); 
                  fvec->n = dxv[i]->n_counts;
               }
               if (j==0) { /* allocate */ 
                  dxv[i]->n_delta = dxv[i]->n_counts*dxv[i]->n_counts; 
                  dxv[i]->delta = (float *)SUMA_calloc(dxv[i]->n_delta, sizeof(float));
               }
               for (k=0; k<fvec->n; ++k) { 
                  dxv[i]->delta[(j*dxv[i]->n_counts) + k] = fvec->v[k];  
               }
               SUMA_free(fvec->v); SUMA_free(fvec); fvec = NULL;
            } else { /* get out */
               if (j) {
                  SUMA_SL_Warn("Expect as many deltas as counts!\nThat was not the case.");
               } else { /* OK, no deltas at all */ }
               j = dxv[i]->n_counts;
            }
            ++j; rf = ope;
         }
      } 
      /* now for the data */
      if (!SUMA_OpenDx_Object_Data(opv[i], nchar[i], dxv[i])) {
         SUMA_SL_Err("Failed to get data");
         dxv[i]->bad_data = 1;
      }
   }

   for (i=0; i<iop; ++i) { /* process each object's attributes*/
      if (!SUMA_OpenDx_Object_Attr(opv[i], nchar[i], dxv[i])) {
         SUMA_SL_Err("Failed in SUMA_OpenDx_Object_Attr");
      }
      if (!SUMA_OpenDx_Object_Components(opv[i], nchar[i], dxv[i])) {
         SUMA_SL_Err("Failed in SUMA_OpenDx_Object_Components");
      }
   }

   if (LocalHead) {
         SUMA_Show_OpenDX_Struct(dxv, iop, NULL); fflush(stdout);
   }
   
   if (opv) SUMA_free(opv); opv = NULL;
   if (nchar) SUMA_free(nchar); nchar = NULL;
   if (fl) SUMA_free(fl); fl = NULL; /* added Mon May 9 05, ZSS */
   *nobj = iop;
   SUMA_RETURN(dxv); 
}

/*!

   \brief Load a surface-based data set of the DX format
   \param Name (char *) name or prefix of dataset
   \param verb (int) level of verbosity. 0 mute, 1 normal, 2 dramatic perhaps
   \return dset (SUMA_DSET *)
   
*/
SUMA_DSET *SUMA_LoadDXDset_ns (char *Name, int verb)
{
   SUMA_DSET *dset=SUMA_LoadDXDset_eng (Name,  verb);
   WorkErrLog_ns();
   return(dset);
}
SUMA_DSET *SUMA_LoadDXDset_eng (char *Name, int verb)
{
   static char FuncName[]={"SUMA_LoadDXDset_eng"};
   char *FullName = NULL;
   MRI_IMAGE *im = NULL;
   float *far=NULL;
   int i, ndxv=0;
   char *idcode = NULL, *name=NULL;
   SUMA_DSET *dset=NULL;
   SUMA_OPEN_DX_STRUCT **dxv=NULL, *dx=NULL;
   SUMA_ENTRY;
   
   if (!Name) { SUMA_PushErrLog("SL_Err", "Null Name", FuncName); SUMA_RETURN(dset); }
   
   /* work the name */
   if (!SUMA_filexists(Name)) {
      /* try the extension game */
      FullName = SUMA_Extension(Name, ".dx.dset", NOPE);
      if (!SUMA_filexists(FullName)) {
         SUMA_free(FullName); FullName = NULL;
         FullName = SUMA_Extension(Name, ".dx", NOPE);
         if (!SUMA_filexists(FullName)) {
            if (verb)  { SUMA_PushErrLog("SL_Err", "Failed to find dset file.", FuncName); }
            goto CLEAN_EXIT;
         }
      }
   }else {
      FullName = SUMA_copy_string(Name);
   }
   
   /* load the objects */
   if (!(dxv = SUMA_OpenDX_Read(FullName, &ndxv))) {
      if (verb) SUMA_PushErrLog("SL_Err", "Failed to read OpenDx File", FuncName);
      goto CLEAN_EXIT;
   }

   /* if more than one, warn that only one is to be used */
   if (ndxv < 1) {
      SUMA_PushErrLog("SL_Err", "no objects in file", FuncName);
      goto CLEAN_EXIT;
   }else if (ndxv > 1) {
      SUMA_SL_Warn("More than one object found in file.\nUsing first applicable one.");
   }
   /* find the object that is appropriate */
   i=0; dx = NULL;
   while (i<ndxv && !dx) {
      if (dxv[i]->datap && dxv[i]->items) {
         dx = dxv[i];
      }
      ++i;
   }
   if (!dx) {
      SUMA_PushErrLog("SL_Err", "No appropriate objects found", FuncName);
      SUMA_Show_OpenDX_Struct(dxv, ndxv, NULL); fflush(stdout);
      goto CLEAN_EXIT;
   }
   /* transfer contents to dset */
   if (ParentOfDsetToLoad) name = SUMA_append_string(ParentOfDsetToLoad, FullName);
   else if (FullName) name = SUMA_copy_string(FullName);
   else name = SUMA_copy_string("wow");
   SUMA_NEW_ID(idcode, name);
   SUMA_free(name); name = NULL;
   dset = SUMA_OpenDX2dset(FullName, idcode, NULL, dx);
   if (idcode) SUMA_free(idcode); idcode = NULL;
   if (!dset) {
      SUMA_PushErrLog("SLP_Err", "Failed in SUMA_OpenDX2dset\n", FuncName);
      goto CLEAN_EXIT;
   }
   
   
   CLEAN_EXIT:
   if (FullName) SUMA_free(FullName); FullName = NULL;
   for (i=0; i<ndxv; ++i) {
      dxv[i] = SUMA_Free_OpenDX_Struct(dxv[i]);
   }
   if (dxv) SUMA_free(dxv); dxv = NULL;
   SUMA_RETURN(dset);
} 
/*!

   \brief Load a surface-based data set of the 1D format
   \param Name (char *) name or prefix of dataset
   \param verb (int) level of verbosity. 0 mute, 1 normal, 2 dramatic perhaps
   \return dset (SUMA_DSET *)
   
*/
SUMA_DSET *SUMA_Load1DDset_ns (char *oName, int verb)
{
   SUMA_DSET *dset=SUMA_Load1DDset_eng (oName,  verb);
   WorkErrLog_ns();
   return(dset);
}
SUMA_DSET *SUMA_Load1DDset_eng (char *oName, int verb)
{
   static char FuncName[]={"SUMA_Load1DDset_eng"};
   char *FullName = NULL;
   MRI_IMAGE *im = NULL;
   float *far=NULL;
   int i;
   char *idcode = NULL, *name=NULL, *nstrip = NULL;
   SUMA_DSET *dset=NULL;
   
   SUMA_ENTRY;
   
   if (!oName) { SUMA_PushErrLog("SL_Err", "Null Name", FuncName); SUMA_RETURN(dset); }
   
   /* SUMA_S_Note(oName); */
   /* remove [] if existing */
   nstrip = SUMA_copy_string(oName);
   for (i=0; i<strlen(nstrip); ++i) if (nstrip[i] == '[') { nstrip[i] = '\0'; break; }
   
   /* work the name */
   if (!SUMA_filexists(nstrip)) {
      /* try the extension game */
      FullName = SUMA_Extension(nstrip, ".1D.dset", NOPE);
      if (!SUMA_filexists(FullName)) {
         if (verb)  { SUMA_PushErrLog("SL_Err", "Failed to find dset file.", FuncName); }
         if (FullName) SUMA_free(FullName); FullName = NULL;
         SUMA_RETURN(dset);
      }
   }else {
      FullName = SUMA_copy_string(nstrip);
   }
   /* got the name, now read it */
   im = mri_read_1D (oName);
   if (!im) {
      if (verb) SUMA_PushErrLog("SLP_Err", "Failed to read file", FuncName);
      if (FullName) SUMA_free(FullName); FullName = NULL;
      SUMA_RETURN(NULL);
   }   
   
   /* form a good id */
   if (ParentOfDsetToLoad) name = SUMA_append_string(ParentOfDsetToLoad, FullName);
   else if (FullName) name = SUMA_copy_string(FullName);
   else name = SUMA_copy_string("wow");
   SUMA_NEW_ID(idcode, name);
   SUMA_free(name); name = NULL;
   far = MRI_FLOAT_PTR(im);
   dset = SUMA_far2dset_ns(FullName, idcode, NULL, &far, im->nx, im->ny, 0);
   if (idcode) SUMA_free(idcode); idcode = NULL;
   if (!dset) {
      SUMA_PushErrLog("SLP_Err", "Failed in SUMA_far2dset\n", FuncName);
      if (im) mri_free(im); im = NULL;
      if (FullName) SUMA_free(FullName); FullName = NULL; 
      SUMA_RETURN(NULL);
   }
   
   /* done, clean up and out you go */
   if (im) mri_free(im); im = NULL; 
   if (FullName) SUMA_free(FullName); FullName = NULL;
   if (nstrip) SUMA_free(nstrip); nstrip = NULL;
   SUMA_RETURN(dset);
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
float *SUMA_Load1D_ns (char *oName, int *ncol, int *nrow, int RowMajor, int verb)
{
   float *far=SUMA_Load1D_eng (oName, ncol, nrow,  RowMajor,  verb);
   WorkErrLog_ns();
   return(far);
}
float *SUMA_Load1D_eng (char *oName, int *ncol, int *nrow, int RowMajor, int verb)
{
   static char FuncName[]={"SUMA_Load1D_eng"};
   char *FullName = NULL;
   MRI_IMAGE *im = NULL, *imt = NULL;
   float *far=NULL;
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!oName) { SUMA_PushErrLog("SL_Err", "Null Name", FuncName); SUMA_RETURN(NULL); }
   
   /* got the name, now read it */
   im = mri_read_1D (oName);
   if (!im) {
      if (verb) SUMA_PushErrLog("SLP_Err","Failed to read file", FuncName);
      SUMA_RETURN(NULL);
   }   
   *ncol = im->ny;
   *nrow = im->nx;
   if (LocalHead) fprintf(SUMA_STDERR, "Read %s, found %d cols, %d rows\n", oName, *ncol, *nrow);
   if (RowMajor) {
      imt = mri_transpose(im); mri_free(im); im = imt; imt = NULL;
   }   
   
   far = MRI_FLOAT_PTR(im);
   /* make sure that pointer is gone from im, or risk hell */
   mri_clear_data_pointer(im) ;
      
   /* done, clean up and out you go */
   if (im) mri_free(im); im = NULL; 

   SUMA_RETURN(far);
}
double *SUMA_LoadDouble1D_eng (char *oName, int *ncol, int *nrow, int RowMajor, int verb)
{
   static char FuncName[]={"SUMA_LoadDouble1D_eng"};
   char *FullName = NULL;
   MRI_IMAGE *im = NULL, *imt = NULL;
   double *far=NULL;
   int i;
   
   SUMA_ENTRY;
   
   if (!oName) { SUMA_PushErrLog("SL_Err", "Null Name", FuncName); SUMA_RETURN(NULL); }
   
   /* got the name, now read it */
   im = mri_read_double_1D (oName);
   if (!im) {
      if (verb) SUMA_PushErrLog("SLP_Err","Failed to read file", FuncName);
      SUMA_RETURN(NULL);
   }   
   *ncol = im->ny;
   *nrow = im->nx;
   
   if (RowMajor) {
      imt = mri_transpose(im); mri_free(im); im = imt; imt = NULL;
   }   
   
   far = MRI_DOUBLE_PTR(im);
   /* make sure that pointer is gone from im, or risk hell */
   mri_clear_data_pointer(im) ;
      
   /* done, clean up and out you go */
   if (im) mri_free(im); im = NULL; 

   SUMA_RETURN(far);
}

complex *SUMA_LoadComplex1D_eng (char *oName, int *ncol, int *nrow, int RowMajor, int verb)
{
   static char FuncName[]={"SUMA_LoadComplex1D_eng"};
   char *FullName = NULL;
   MRI_IMAGE *im = NULL, *imt = NULL;
   complex *far=NULL;
   int i;
   
   SUMA_ENTRY;
   
   if (!oName) { SUMA_PushErrLog("SL_Err", "Null Name", FuncName); SUMA_RETURN(NULL); }
   
   /* got the name, now read it */
   im = mri_read_complex_1D (oName);
   if (!im) {
      if (verb) SUMA_PushErrLog("SLP_Err","Failed to read file", FuncName);
      SUMA_RETURN(NULL);
   }   
   *ncol = im->ny;
   *nrow = im->nx;
   
   if (RowMajor) {
      imt = mri_transpose(im); mri_free(im); im = imt; imt = NULL;
   }   
   
   far = MRI_COMPLEX_PTR(im);
   /* make sure that pointer is gone from im, or risk hell */
   mri_clear_data_pointer(im) ;
      
   /* done, clean up and out you go */
   if (im) mri_free(im); im = NULL; 

   SUMA_RETURN(far);
}
/*!
   \brief Replaces a dataset's idcode with a new one
*/
SUMA_Boolean SUMA_NewDsetID (SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_NewDsetID"};
   char stmp[SUMA_IDCODE_LENGTH];
   
   SUMA_ENTRY;
   
   UNIQ_idcode_fill(stmp);
   
   if (!dset) SUMA_RETURN(NOPE);
   if (!dset->ngr) SUMA_RETURN(NOPE);
   
   NI_set_attribute(dset->ngr, "self_idcode", stmp);
   
   SUMA_RETURN(YUP);
}

/*!
   \brief return a niml dset's ID,
    substitute for macro  SDSET_ID
*/
char* SUMA_sdset_id(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_sdset_id"};
   char *id=NULL;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(id);
   #ifdef OLD_DSET
   if (!dset->nel) SUMA_RETURN(id);
   
   id = NI_get_attribute(dset->nel,"self_idcode"); 
   if (!id) id = NI_get_attribute(dset->nel,"idcode"); /* the olde way */ 
   #else
   if (!dset->ngr) SUMA_RETURN(id);
   
   id = NI_get_attribute(dset->ngr,"self_idcode"); 
   if (!id) id = NI_get_attribute(dset->ngr,"idcode"); /* the olde way */ 
   #endif
   SUMA_RETURN(id);
}


/*!
   \brief return a niml dset's mesh parent ID , substitute for macro 
   SDSET_IDMDOM
*/
char* SUMA_sdset_idmdom(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_sdset_idmdom"};
   char *id=NULL;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(id);
   #ifdef OLD_DSET
   if (!dset->nel) SUMA_RETURN(id);
   
   id = NI_get_attribute(dset->nel,"domain_parent_idcode"); 
   if (SUMA_IS_EMPTY_STR_ATTR(id)) id = NI_get_attribute(dset->nel,"MeshParent_idcode"); /* the olde way */ 
   #else
   if (!dset->ngr) SUMA_RETURN(SUMA_EMPTY_ATTR);
   
   id = NI_get_attribute(dset->ngr,"domain_parent_idcode"); 
   #endif
   SUMA_RETURN(id);
}

   








/*********************** BEGIN Miscellaneous support functions **************************** */
/* A few functions that are useful without having to link and include all of SUMA's structures */
 
/*!
   \brief load the environment varaibles first from 
   $HOME/.sumarc and $HOME/.afnirc
   if HOME is not defined then try .afnirc and .sumarc
   Shameless wrapper for AFNI_process_environ

   No fanices here, this function is called before CommonFields
*/
void SUMA_process_environ(void)
{
   static char FuncName[]={"SUMA_process_environ"};
   struct stat stbuf;
   char *sumarc = NULL, *homeenv=NULL;
   SUMA_Boolean LocalHead = NOPE;

   sumarc = (char *)malloc(sizeof(char)*(SUMA_MAX_NAME_LENGTH+SUMA_MAX_DIR_LENGTH+1));

   /* load the environment variables from .sumarc and .afnirc*/
   homeenv = getenv("HOME");

   if (!homeenv) sprintf(sumarc, ".sumarc");
   else sprintf(sumarc,"%s/.sumarc", homeenv);
   if (stat(sumarc, &stbuf) != -1) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Loading %s ...\n", FuncName, sumarc);
      AFNI_process_environ(sumarc); 
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: No rc files found.\n", FuncName);
   }

   if (!homeenv) sprintf(sumarc, ".afnirc");
   else sprintf(sumarc,"%s/.afnirc", homeenv);
   if (stat(sumarc, &stbuf) != -1) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Loading %s ...\n", FuncName, sumarc);
      AFNI_process_environ(sumarc); 
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: No rc files found.\n", FuncName);
   }   

   if (sumarc) free(sumarc); sumarc = NULL; /* allocated before CommonFields */
   return;
}

char *SUMA_help_basics()
{
   SUMA_STRING *SS = NULL;
   char *s=NULL;
   static char FuncName[]={"SUMA_help_basics"};
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   SS = SUMA_StringAppend(SS,
                  "   [-novolreg|-noxform]: Ignore any Rotate, Volreg, Tagalign, \n"
                  "                or WarpDrive transformations present in \n"
                  "                the Surface Volume.\n"
                  "  Common Debugging Options:\n"
                  "   [-trace]: Turns on In/Out debug and Memory tracing.\n"
                  "             For speeding up the tracing log, I recommend \n"
                  "             you redirect stdout to a file when using this option.\n"
                  "             For example, if you were running suma you would use:\n"
                  "             suma -spec lh.spec -sv ... > TraceFile\n"
                  "             This option replaces the old -iodbg and -memdbg.\n"
                  "   [-TRACE]: Turns on extreme tracing.\n"
                  "   [-nomall]: Turn off memory tracing.\n"
                  "   [-yesmall]: Turn on memory tracing (default).\n"
                  "  NOTE: For programs that output results to stdout\n"
                  "    (that is to your shell/screen), the debugging info\n"
                  "    might get mixed up with your results.\n" 
                  " \n");
   SUMA_SS2S(SS,s);               
   SUMA_RETURN(s);
}

char *SUMA_help_talk()
{
   SUMA_STRING *SS = NULL;
   char *s=NULL;
   static char FuncName[]={"SUMA_help_talk"};
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   SS = SUMA_StringAppend(SS,
                  "  SUMA communication options:\n"
                  "      -talk_suma: Send progress with each iteration to SUMA.\n"
                  "      -refresh_rate rps: Maximum number of updates to SUMA per second.\n"
                  "                         The default is the maximum speed.\n"
                  "      -send_kth kth: Send the kth element to SUMA (default is 1).\n"
                  "                     This allows you to cut down on the number of elements\n"
                  "                     being sent to SUMA.\n" 
                  "      -sh <SumaHost>: Name (or IP address) of the computer running SUMA.\n"
                  "                      This parameter is optional, the default is 127.0.0.1 \n"
                  "      -ni_text: Use NI_TEXT_MODE for data transmission.\n"
                  "      -ni_binary: Use NI_BINARY_MODE for data transmission.\n"
                  "                  (default is ni_binary).\n"
                  "      -feed_afni: Send updates to AFNI via SUMA's talk.\n"
                  "\n");
   SUMA_SS2S(SS,s);               
   SUMA_RETURN(s);
}
char *SUMA_help_dset()
{
   SUMA_STRING *SS = NULL;
   char *s=NULL;
   static char FuncName[]={"SUMA_help_dset"};
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   SS = SUMA_StringAppend(SS,
      "  SUMA dataset input options:\n"
      "      -input DSET: Read DSET1 as input.\n"
      "                   In programs accepting multiple input datasets\n"
      "                   you can use -input DSET1 -input DSET2 or \n"
      "                   input DSET1 DSET2 ...\n"
      "       NOTE: Selecting subsets of a dataset:\n"
      "             Much like in AFNI, you can select subsets of a dataset\n"
      "             by adding qualifiers to DSET.\n"
      "           Append #SEL# to select certain nodes.\n"
      "           Append [SEL] to select certain columns.\n"
      "           Append {SEL} to select certain rows.\n"
      "           The format of SEL is the same as in AFNI, see section:\n"
      "           'INPUT DATASET NAMES' in 3dcalc -help for details.\n"
                  "\n");
   SUMA_SS2S(SS,s);               
   SUMA_RETURN(s);
}

char *SUMA_help_mask()
{
   SUMA_STRING *SS = NULL;
   char *s=NULL;
   static char FuncName[]={"SUMA_help_mask"};
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   SS = SUMA_StringAppend(SS,
                  " SUMA mask options:\n"
                  "      -n_mask INDEXMASK: Apply operations to nodes listed in\n"
                  "                            INDEXMASK  only. \n"
                  "      -b_mask BINARYMASK: Similar to -n_mask, except that BINARYMASK\n"
                  "                          contains 1 for nodes to filter and 0 for nodes to be ignored.\n"
                  "                          The number of rows in filter_binary_mask must be equal to the\n"
                  "                          number of nodes forming the surface.\n"
                  "      -c_mask EXPR: Masking based on the result of EXPR. \n"
                  "                    Use like afni's -cmask options. See explanation in 3dmaskdump -help \n"
                  "                    and examples in output of 3dVol2Surf -help\n"
                  "      NOTE: Unless stated otherwise, if n_mask, b_mask and c_mask are used simultaneously\n"
                  "            , the resultant mask is the intersection (AND operation) of all masks.\n"
                  "\n");
   SUMA_SS2S(SS,s);     
   SUMA_RETURN(s);
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
static int Domemtrace=0, Doiotrace=0, IgnoreXforms=0;
int get_Domemtrace(void) {
   return (Domemtrace);
}
void set_Domemtrace(int s) {
   Domemtrace = s;
   return;
}
int get_Doiotrace(void) {
   return (Doiotrace);
}
void set_Doiotrace(int s) {
   Doiotrace = s;
   return;
}
int get_IgnoreXforms(void) {
   return (IgnoreXforms);
}
void set_IgnoreXforms(int s) {
   IgnoreXforms = s;
   return;
}


/*!
   No fancy macros and allocation here please
*/
int SUMA_ParseInput_basics_eng (char *argv[], int argc) 
{

   static char FuncName[]={"SUMA_ParseInput_basics_eng"};
   int brk = 0;
   int kar;

   if (!argv) return (0);
   if (argc < 2) return (0);

   kar = 1;
   brk = 0;
   set_Domemtrace(1);
   set_Doiotrace(0);
   while (kar < argc) { /* loop accross tracing and debugging command line options */
		if ((strcmp(argv[kar], "-memdbg") == 0) ||
          (strcmp(argv[kar], "-yesmall") == 0) ) {
			fprintf(SUMA_STDOUT,"Warning %s:  running in memory trace mode.\n", FuncName);
			set_Domemtrace(1);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-nomall") == 0)) {
			fprintf(SUMA_STDOUT,"Warning %s:  turning off memory trace mode.\n", FuncName);
			set_Domemtrace(0);
			brk = 1;
		}

      if (!brk && ( (strcmp(argv[kar], "-trace") == 0) ||
                   (strcmp(argv[kar], "-iodbg") == 0)) ){
			fprintf(SUMA_STDERR,"Warning %s: SUMA running in I/O trace mode.\n", FuncName);
			set_Doiotrace(1);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-TRACE") == 0)) {
			fprintf(SUMA_STDERR,"Warning %s: SUMA running in detailed I/O trace mode.\n", FuncName);
			set_Doiotrace(2);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-novolreg") == 0 || strcmp(argv[kar], "-noxform") == 0)) {
			set_IgnoreXforms(1);
         brk = 1;
		}
      
      brk = 0;
      kar ++;
   }
       
   return (1);
}

/*!
   No fancy macros and allocation here please
*/
void SUMA_ParseInput_basics_ns(char *argv[], int argc) /* for non-suma programs */
{
   static char FuncName[]={"SUMA_ParseInput_basics_ns"};
   
   if (!argv) return;
   if (argc < 2) return;
   
   if (!SUMA_ParseInput_basics_eng (argv, argc)) return;
   
   if (get_Doiotrace()) { SUMA_INOUT_NOTIFY_ON; } 
   if (get_Domemtrace()) { SUMA_MEMTRACE_ON; } 

   /* some more special ones */
   #ifdef USE_TRACING
      if (get_Doiotrace() == 2) { DBG_trace = 2; } 
   #endif
   
   return;

}

/*!**
   
Purpose : 
   
   splits a path/filename into its path and filename components
   
Usage : 
		Ans = SUMA_StripPath (Name)
   
   
Input paramters : 
\param   Name (char *) something like /hello/something
   
Returns : 
\return   ans (SUMA_FileName) .Path (char *) and .FileName (char *)
   
Support : 
\sa  SUMA_define.h 

NOTE: SUMA_ParseFname() is better than this function	
   
To Compile as stand alone:
gcc -DSUMA_StripPath_STAND_ALONE -Wall -o $1 $1.c -SUMA_lib.a -I/usr/X11R6/include -I./
***/
SUMA_FileName SUMA_StripPath (char *FileName)
{/*SUMA_StripPath*/
   static char FuncName[] = {"SUMA_StripPath"},  PathDelimiter[]={"/"}; 
   int i, j, NotFound=1, N_FileName;
	SUMA_FileName NewName;
	
	N_FileName = strlen(FileName);
	if (N_FileName ){
		i = N_FileName -1;
		while (i > -1 && NotFound) {
			if (FileName[i] == PathDelimiter[0]) NotFound = 0;
			--i;
		}
		if (!NotFound && i > -1) {
			NewName.Path = (char *)SUMA_malloc(sizeof(char)*(N_FileName+1));
			NewName.FileName = (char *)SUMA_malloc(sizeof(char)*(N_FileName+1));
			if (NewName.Path == NULL || NewName.FileName == NULL) {
				SUMA_SL_Err("Failed to allocate");
            return (NewName);
			}
			for (j=0; j<=i+1; ++j) {
				NewName.Path[j] = FileName[j];
			}
         NewName.Path[j] = '\0';
         
			/*fprintf(stdout,"jbegin=%d/%d\n", i+2, N_FileName);*/
			for (j=i+2; j < N_FileName; ++j) NewName.FileName[j-i-2] = FileName[j];
         NewName.FileName[j-i-2] = '\0';
         
			/* fprintf(stdout,"All Path (%d chars)/%d: %s\n", (i+2),  strlen(NewName.Path), NewName.Path);
			fprintf(stdout,"All FileName (%d chars)/%d: %s\n", (N_FileName-i-2), strlen(NewName.FileName), NewName.FileName); */
		}
		else {
			NewName.Path = (char *)SUMA_malloc(sizeof(char)*(N_FileName+1));
			NewName.FileName = (char *)SUMA_malloc(sizeof(char)*(N_FileName+1));
			if (NewName.Path == NULL || NewName.FileName == NULL) {
				SUMA_SL_Err("Failed to allocate");
            return (NewName);
			}
			sprintf(NewName.Path,"./");		
			sprintf(NewName.FileName,"%s", FileName);
		}
	}
	else {
		NewName.Path = NULL;
		NewName.FileName = NULL;
	}
	return (NewName);
}/*SUMA_StripPath*/

SUMA_Boolean SUMA_ShowParsedFname(SUMA_PARSED_NAME *pn, FILE *out)
{
   static char FuncName[]={"SUMA_ShowParsedFname"};
   char *s=NULL;
   SUMA_STRING *SS=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!out) out = SUMA_STDOUT;
   
   SS = SUMA_StringAppend(NULL, NULL);
   if (!pn) {
      SS = SUMA_StringAppend_va(SS, "NULL parsed name");
   } else {
      SS = SUMA_StringAppend_va(SS, "AbsPath       :%s\n", pn->AbsPath);
      SS = SUMA_StringAppend_va(SS, "Path          :%s\n", pn->Path);
      SS = SUMA_StringAppend_va(SS, "FileName      :%s\n", pn->FileName);
      SS = SUMA_StringAppend_va(SS, "Ext           :%s\n", pn->Ext);
      SS = SUMA_StringAppend_va(SS, "FileName_NoExt:%s\n", pn->FileName_NoExt);
      SS = SUMA_StringAppend_va(SS, "Col. Selector :%s\n", pn->ColSelect);
      SS = SUMA_StringAppend_va(SS, "Node Selector :%s\n", pn->NodeSelect);
      SS = SUMA_StringAppend_va(SS, "Row Selector  :%s\n", pn->RowSelect);
      SS = SUMA_StringAppend_va(SS, "Range Selector:%s\n", pn->RangeSelect);
      SS = SUMA_StringAppend_va(SS, "FullName      :%s\n", pn->FullName);
   }

   SUMA_SS2S(SS,s);
   
   fprintf(out, "%s", s); SUMA_free(s); s= NULL;
   fflush(out);
   
   SUMA_RETURN(YUP);
}

char *SUMA_getcwd(void) 
{
   static char FuncName[]={"SUMA_getcwd"};
   char *cwd = NULL;
   
   SUMA_ENTRY;
   
   cwd = (char *)SUMA_malloc(sizeof(char)*(SUMA_MAX_DIR_LENGTH+1));
   getcwd(cwd, SUMA_MAX_DIR_LENGTH);
   
   SUMA_RETURN(cwd);
}

/*!
   \brief ans = SUMA_ParseFname (FileName, cwd);
   parses a file name into its elements
   \param FileName (char *) obvious ...
   \return ans (SUMA_PARSED_NAME *) pointer to structure with following fields:
      .FileName (char *) containing filename without path and without selectors (see below). 
                        if empty .FileName[0] = '\0'
      .Path (char *) containing path including last slash.
                     If no path exists, Path is "./" 
      .AbsPath (char *) containing absolute path, which is 
                        the same as .Path if .Path starts with '/'
                        or cwd/.Path if .Path does not start with '/'
                        AND cwd is specified. 
                        If cwd is null then it is determined inside the function.
      .Ext (char *) containing extension including the dot.
                    If no extension exists, Ext[0] = '\0'
      .FileName_NoExt (char *) filename without extension.
      .NodeSelect (char *) Node selector part; between ## ; The () were already taken
      .ColSelect (char *) Column selector part; between []
      .RowSelect (char *) Row selector part; between {}
      .RangeSelect (char *) Range selector part; between <> 
      
      \sa SUMA_Free_Parsed_Name, SUMA_ShowParsedFname
*/
SUMA_PARSED_NAME * SUMA_ParseFname (char *FileName, char *ucwd)
{/*SUMA_ParseFname*/
   static char FuncName[]={"SUMA_ParseFname"};
   char PathDelimiter='/';
   char *cwd=NULL; 
   int i, j, iExt , iFile, iPath, iColSel, iRowSel, iNodeSel, iRangeSel, N_FileName, iFirstSel, nc;
	SUMA_PARSED_NAME *NewName = NULL;
   SUMA_Boolean FoundPath = NOPE, FoundExt, FoundFile, FoundRowSel , FoundNodeSel, FoundColSel, FoundRangeSel;
	SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   
   if (!FileName) {
      SUMA_S_Err("Null input");
      SUMA_RETURN(NULL);
   }
   /* work cwd */
   if (ucwd) {
      if (ucwd[0] != '/') {
         SUMA_S_Err("Current working directory must start with '/'");
         SUMA_RETURN(NULL);
      }
      cwd = SUMA_copy_string(ucwd);
   } else {
      cwd = SUMA_getcwd();
      if (cwd[0] != '/') {
         SUMA_S_Err("STRANGE! Current working directory must start with '/'");
         SUMA_free(cwd); cwd = NULL;
         SUMA_RETURN(NULL);
      }
   }
   nc = strlen(cwd);
   if (cwd[nc-1] == PathDelimiter) { cwd[nc-1] = '\0'; --nc; } 
   
   SUMA_LH("Checking chars");
	N_FileName = strlen(FileName);
   iExt = N_FileName;
   iPath = -1;
   iFile = 0;
   iColSel = -1;
   iRowSel = -1;
   iNodeSel = -1;
   iRangeSel = -1;
   iFirstSel = N_FileName;
   FoundPath = NOPE;
   FoundExt = NOPE;
   FoundRowSel = NOPE;
   FoundNodeSel = NOPE;
   FoundColSel = NOPE;
   FoundRangeSel = NOPE;
	if (N_FileName ){
		NewName = (SUMA_PARSED_NAME *) SUMA_malloc(sizeof(SUMA_PARSED_NAME));
      
      i = N_FileName -1;
		while (i > -1 && !FoundPath) {
			if (FileName[i] == '.' && !FoundExt && 
             !(FoundColSel && iColSel < 0) && /* Not whilst reading column selection */
             !(FoundRowSel && iRowSel < 0) && /* Not whilst reading row selection */
             !(FoundNodeSel && iNodeSel < 0) && /* Not whilst reading node selection */
             !(FoundRangeSel && iRangeSel < 0) /* Not whilst reading range selection */ ) {
            iExt = i;
            FoundExt = YUP;
         } else if (FileName[i] == PathDelimiter) {
            FoundPath = YUP;
            iPath = i;
            iFile = i+1;
         } else if (FileName[i] == ']') {
            FoundColSel = YUP;
         } else if (FileName[i] == '[' && FoundColSel) {
            iColSel = i; if (iColSel < iFirstSel) iFirstSel = iColSel; 
         } else if (FileName[i] == '}') {
            FoundRowSel = YUP;
         } else if (FileName[i] == '{' && FoundRowSel) {
            iRowSel = i; if (iRowSel < iFirstSel) iFirstSel = iRowSel;
         } else if (FileName[i] == '#' && !FoundNodeSel) {
            FoundNodeSel = YUP;
         } else if (FileName[i] == '#' && FoundNodeSel) {
            iNodeSel = i; if (iNodeSel < iFirstSel) iFirstSel = iNodeSel;
         } else if (FileName[i] == '>') {
            FoundRangeSel = YUP;
         } else if (FileName[i] == '<' && FoundRangeSel) {
            iRangeSel = i; if (iRangeSel < iFirstSel) iFirstSel = iRangeSel;
         }
			--i;
		}
      
      if (FoundColSel && iColSel < 0) {
         FoundColSel = NOPE; /* need both of [ ] */
      }
      if (FoundRowSel && iRowSel < 0) {
         FoundRowSel = NOPE; /* need both of { } */
      }
      if (FoundNodeSel && iNodeSel < 0) {
         FoundNodeSel = NOPE; /* need both of { } */
      }
      if (FoundRangeSel && iRangeSel < 0) {
         FoundRangeSel = NOPE; /* need both of # # */
      }
      
      if (iFile == iExt) {
         /* .file, not an extension */
         FoundExt = NOPE;
      }
      
      if (iFile ==  N_FileName) FoundFile = NOPE;
      else FoundFile = YUP;
      
      SUMA_LH("Storing stuff");
      
      if (FoundPath) {
         NewName->Path = (char *)SUMA_malloc(sizeof(char)*(iPath+2));
         for (i=0; i<= iPath; ++i) NewName->Path[i] = FileName[i];
         NewName->Path[i] = '\0';
      }else {
         NewName->Path = (char *)SUMA_malloc(sizeof(char)*(3));
         sprintf(NewName->Path, "./");
      }
      if (NewName->Path[0] == '/') {
         NewName->AbsPath = SUMA_copy_string(NewName->Path);
      } else {
         char *ptmp = NewName->Path;
         if (ptmp[0] == '.') {
            if (strstr(NewName->Path,"./") && ptmp[1] == '/') ptmp = ptmp+2;
            else ptmp = ptmp+1;   
         } 
         NewName->AbsPath = SUMA_append_replace_string(cwd, ptmp, "/", 0);
         ptmp = NULL;
      }
      if (FoundFile) {
         NewName->FileName = (char *)SUMA_malloc(sizeof(char)*(N_FileName - iFile + 2));
         for (i=iFile; i< iFirstSel; ++i) NewName->FileName[i-iFile] = FileName[i];
         NewName->FileName[i-iFile] = '\0';
      }else {
         NewName->FileName = (char *)SUMA_malloc(sizeof(char));
         NewName->FileName[0] = '\0';
      }      
		
      if (FoundExt) {
		   NewName->FileName_NoExt = (char *)SUMA_malloc(sizeof(char)*(N_FileName - iFile +2));
         NewName->Ext = (char *)SUMA_malloc(sizeof(char)*(N_FileName - iExt+2));
         for (i=iFile; i< iExt; ++i) NewName->FileName_NoExt[i-iFile] = FileName[i];
         NewName->FileName_NoExt[i-iFile] = '\0';
         for (i=iExt; i < iFirstSel; ++i) NewName->Ext[i-iExt] = FileName[i];
         NewName->Ext[i-iExt] = '\0';
      } else {
         NewName->FileName_NoExt = (char *)SUMA_malloc(sizeof(char));
         NewName->Ext = (char *)SUMA_malloc(sizeof(char));
         NewName->FileName_NoExt[0] = '\0';
         NewName->Ext[0] = '\0';
      }
      
      if (FoundNodeSel) {
         NewName->NodeSelect = (char *)SUMA_malloc(sizeof(char)*(N_FileName - iNodeSel + 2));
         for (i=iNodeSel; i< N_FileName; ++i) { 
            NewName->NodeSelect[i-iNodeSel] = FileName[i];
            if (FileName[i] == '#' && i >iNodeSel) { ++i; break; }
         }
         NewName->NodeSelect[i-iNodeSel] = '\0';
      }else {
         NewName->NodeSelect = (char *)SUMA_malloc(sizeof(char));
         NewName->NodeSelect[0] = '\0';
      }      
		
      if (FoundRowSel) {
         NewName->RowSelect = (char *)SUMA_malloc(sizeof(char)*(N_FileName - iRowSel + 2));
         for (i=iRowSel; i< N_FileName; ++i) {
            NewName->RowSelect[i-iRowSel] = FileName[i];
            if (FileName[i] == '}') { ++i; break; }
         }
         NewName->RowSelect[i-iRowSel] = '\0';
      }else {
         NewName->RowSelect = (char *)SUMA_malloc(sizeof(char));
         NewName->RowSelect[0] = '\0';
      }
      
      if (FoundColSel) {
         NewName->ColSelect = (char *)SUMA_malloc(sizeof(char)*(N_FileName - iColSel + 2));
         for (i=iColSel; i< N_FileName; ++i) {
            NewName->ColSelect[i-iColSel] = FileName[i];
            if (FileName[i] == ']') { ++i; break; }
         }
         NewName->ColSelect[i-iColSel] = '\0';
      }else {
         NewName->ColSelect = (char *)SUMA_malloc(sizeof(char));
         NewName->ColSelect[0] = '\0';
      }
      
      if (FoundRangeSel) {
         NewName->RangeSelect = (char *)SUMA_malloc(sizeof(char)*(N_FileName - iRangeSel + 2));
         for (i=iRangeSel; i< N_FileName; ++i) {
            NewName->RangeSelect[i-iRangeSel] = FileName[i];
            if (FileName[i] == '>') { ++i; break; }
         }
         NewName->RangeSelect[i-iRangeSel] = '\0';
      }else {
         NewName->RangeSelect = (char *)SUMA_malloc(sizeof(char));
         NewName->RangeSelect[0] = '\0';
      }
      
      NewName->FullName=NULL;
      NewName->FullName=SUMA_append_replace_string(NewName->FullName, NewName->AbsPath, "", 1);
      NewName->FullName=SUMA_append_replace_string(NewName->FullName, NewName->FileName, "", 1);
      NewName->FullName=SUMA_append_replace_string(NewName->FullName, NewName->NodeSelect, "", 1);
      NewName->FullName=SUMA_append_replace_string(NewName->FullName, NewName->RowSelect, "", 1);
      NewName->FullName=SUMA_append_replace_string(NewName->FullName, NewName->ColSelect, "", 1);
      
	}
   
   if (LocalHead) {
      SUMA_ShowParsedFname(NewName, NULL);
   }
   if (cwd) SUMA_free(cwd);
   
	SUMA_RETURN (NewName);
}/*SUMA_ParseFname*/


/*!
   \brief ans = SUMA_isExtension(filename, ext);
      YUP if filename has the extension ext
*/
SUMA_Boolean SUMA_isExtension(char *filename, char *ext)
{
   static char FuncName[]={"SUMA_isExtension"}; 
   int cnt, N_ext, N_filename;
      
   SUMA_ENTRY;

   if (!filename) SUMA_RETURN(NOPE);
   if (!ext) SUMA_RETURN(NOPE);
   N_ext = strlen(ext);
   N_filename = strlen(filename);
   if (N_ext > N_filename) SUMA_RETURN(NOPE);

   cnt = 1;
   while (cnt <= N_ext) {
      if (filename[N_filename-cnt] != ext[N_ext-cnt]) SUMA_RETURN(NOPE);
      ++cnt; 
   } 
   
   SUMA_RETURN(YUP);
}

/*!
   \brief ans = SUMA_Extension(filename, ext, Remove);
      removes or enforces an arbitrary extension from/to a filename
   
   \param filename(char *) input filename
   \param ext (char *) extension
   \param Remove (SUMA_Boolean) YUP = Remove extension if found
                                      Do nothing if it is not there already 
                                NOPE = Add extension if not there
                                       Do nothing if it is there already    
   \returns ans (char*) containing modified filename 
  
   - You must free ans on your own
   Examples:
      {
      char *ans=NULL;
      ans = SUMA_Extension("Junk.niml.roi", ".niml.roi", YUP);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("Junk.niml.roi", ".niml.roi", NOPE);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("Junk.niml.roi", ".niml.roxi", NOPE);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("Junk.niml.roi", ".niml.roxi", YUP);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("Junk.niml.roi", "", YUP);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension(".roi", "Junk.niml.roi", NOPE);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("", "", NOPE);
      SUMA_LH(ans); SUMA_free(ans);
      
      exit(1);
    }

*/

char *SUMA_Extension(char *filename, char *ext, SUMA_Boolean Remove)
{
   static char FuncName[]={"SUMA_Extension"}; 
   char *ans = NULL;
   int i, next, nfilename, ifile;
   SUMA_Boolean NoMatch = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!filename) SUMA_RETURN(NULL);
   nfilename = strlen(filename);
   
   if (!ext) {
      ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
      ans = strcpy(ans,filename);
      SUMA_RETURN(ans);
   }
   next = strlen(ext);
   
   if (next > nfilename && Remove) {
      ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
      ans = strcpy(ans,filename);
      SUMA_RETURN(ans);
   }
   #if 0
   if (nfilename < next || next < 1 || nfilename < 1) {
      ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
      ans = strcpy(ans,filename);
      SUMA_RETURN(ans);
   }
   #endif
   
   
   ifile = nfilename - next;
   if (ifile > 0) {
      NoMatch = NOPE;
      i = 0;
      do {
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Comparing %c %c\n", FuncName, filename[ifile+i], ext[i]);
         if (filename[ifile+i] != ext[i]) NoMatch = YUP;
         ++i;
      }  while (ifile < nfilename && i < next && !NoMatch);
   } else {
      NoMatch = YUP;
   }
   
   if (NoMatch) {
      if (Remove) { /* nothing to do */
         SUMA_LH("NoMatch, nothing to do");
         ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
         ans = strcpy(ans,filename);
         SUMA_RETURN(ans);
      } else { /* add extension */
         SUMA_LH("NoMatch, adding extension");
         ans = (char *)SUMA_malloc((nfilename+next+1)*sizeof(char));
         sprintf(ans,"%s%s", filename, ext);
         SUMA_RETURN(ans);
      }
   }else {
      if (Remove) { /* remove it */
         SUMA_LH("Match, removing extension");
         ans = (char *)SUMA_malloc((nfilename - next+2)*sizeof(char));
         for (i=0; i< nfilename - next; ++i)  ans[i] = filename[i];
         ans[nfilename - next] = '\0'; /* for good measure */
      } else { /* nothing to do */
         SUMA_LH("Match, nothing to do");
         ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
         ans = strcpy(ans,filename);
         SUMA_RETURN(ans);
      }
   }
   
   SUMA_RETURN (ans);

}   
void *SUMA_Free_Parsed_Name(SUMA_PARSED_NAME *Test) 
{
   static char FuncName[]={"SUMA_Free_Parsed_Name"}; 

   SUMA_ENTRY;

   if (!Test) SUMA_RETURN (NULL);
   if (Test->AbsPath) SUMA_free(Test->AbsPath);
   if (Test->Path) SUMA_free(Test->Path);
   if (Test->FileName) SUMA_free(Test->FileName);
   if (Test->FullName) SUMA_free(Test->FullName);
   if (Test->Ext) SUMA_free(Test->Ext);
   if (Test->FileName_NoExt) SUMA_free(Test->FileName_NoExt);
   if (Test->RowSelect) SUMA_free(Test->RowSelect);
   if (Test->ColSelect) SUMA_free(Test->ColSelect);
   if (Test->NodeSelect) SUMA_free(Test->NodeSelect);
   if (Test->RangeSelect) SUMA_free(Test->RangeSelect);
   
   SUMA_free(Test);
   
   SUMA_RETURN (NULL);
}


/*! Taken from filexists 
returns 1 if file can be read/found
*/
int SUMA_filexists (char *f_name)
{/*SUMA_filexists*/
    FILE *outfile;
    static char FuncName[]={"SUMA_filexists"};
   
   SUMA_ENTRY;

    outfile = fopen (f_name,"r");
    if (outfile == NULL) {
       SUMA_RETURN(0); 
   }
    else {
       fclose (outfile); 
   }
    
   SUMA_RETURN(1);
       
}/*SUMA_filexists*/


/*!
   \brief function that tests whether a string contains N numbers
   
   WARNING: This function will deform s ! 
   
   \param str (char *) null terminated string
   \param N (void *) This is an integer in disguise
   \return 1: If str is NULL or N numbers were found in str
   
   \sa SUMA_isNumString
*/
int SUMA_CleanNumString (char *s, void *p)
{
   static char FuncName[]={"SUMA_CleanNumString"};
   char *endp, *strtp;
   int nd, N;
   int eos, FoundTip;
   double d;
   int LocalHead = 0;
   
   SUMA_ENTRY;
   
   if (!s) SUMA_RETURN(1); 
   
   N = (int)p;
   
   /* clean s by removing trailing junk then replacing non characters by space*/
   if (LocalHead) fprintf (stderr, "%s: string begins:%s:\n", FuncName, s);
   FoundTip = 0;
   for (nd=strlen(s)-1; nd >=0; --nd) {
      if (!isdigit(s[nd]) && s[nd] != '.'  && s[nd] != '-' && s[nd] != '+') {
         if (!FoundTip) {
            s[nd]= '\0'; /* remove */
         } else {
            s[nd] = ' '; /* blank */
         }
      }else {
         FoundTip = 1;
      }
   }
   
   if (LocalHead) fprintf (stderr, "%s: string now:%s:\n", FuncName, s);
   if (strlen(s) == 1 && (s[0] == '+' || s[0] == '-' || s[0] == '.')) {
      SUMA_RETURN(0);
   }
   
   /* parse s */
   strtp = s;
   endp = NULL;
   nd = 0;
   eos = 0;
   while (!eos) {
      d = strtod(strtp, &endp);
      if (LocalHead) fprintf (stderr, "%s: value %f, ERANGE: %d, EDOM %d, errno %d\n", FuncName, d, ERANGE, EDOM, errno); 
      

      if (endp == strtp && *endp=='\0') { 
         eos = 1;
      } else {
         strtp = endp;
         ++nd;
         if (nd > N && nd > 1000) {
            SUMA_SL_Err("Fishy fish");
            fprintf (stderr, "%s: >>>%s<<<", FuncName, s);
            SUMA_RETURN(0);
         }
      }
   }
   
   if (LocalHead) fprintf (stderr,"%s: Read %d/%d values.\n", FuncName, nd,N);
   if (N != nd) {
      SUMA_RETURN(0);
   } else {
      SUMA_RETURN(1);
   }
   
}   
/*
   Much like SUMA_CleanNumString, but leaves s untouched
*/
int SUMA_isNumString (char *s, void *p) 
{
   static char FuncName[]={"SUMA_isNumString"};
   int ans;
   char *sc;
   
   SUMA_ENTRY;
   
   sc = SUMA_copy_string(s);
   ans = SUMA_CleanNumString(sc,p);
   if(sc) SUMA_free(sc); sc = NULL;
   SUMA_RETURN(ans);
}

/*!
   \brief function that parses a string of numbers into a float vector
   
   \param str (char *) null terminated string
   \param fv (float*) vector where values will be stored
   \param N (int) This is the number of values desired
   \return int: This is the number of values read. 
      The function will not register in fv more than N values 
      (to keep from running over preallocated space), but it
      will return the full number of values found.
      
      -1 in case of error
   \sa SUMA_CleanNumString
   \sa SUMA_strtol_vec
   \sa SUMA_AdvancePastNumbers
*/
int SUMA_StringToNum (char *s, float *fv, int N)
{
   static char FuncName[]={"SUMA_StringToNum"};
   char *endp, *strtp;
   int nd;
   int eos, FoundTip;
   double d;
   int LocalHead = 0;
   
   SUMA_ENTRY;
   
   if (!s) SUMA_RETURN(0); 
     
   /* clean s by removing trailing junk then replacing non characters by space*/
   FoundTip = 0;
   for (nd=strlen(s)-1; nd >=0; --nd) {
      if (!isdigit(s[nd]) && s[nd] != '.' && s[nd] != '-' && s[nd] != '+') {
         if (!FoundTip) {
            s[nd]= '\0'; /* remove */
         } else {
            s[nd] = ' '; /* blank */
         }
      }else {
         FoundTip = 1;
      }
   }
   
   if (LocalHead) fprintf (stderr, "%s: string now:%s:\n", FuncName, s);
   
   /* parse s */
   strtp = s;
   endp = NULL;
   nd = 0;
   eos = 0;
   while (!eos) {
      d = strtod(strtp, &endp);
      if (LocalHead) fprintf (stderr, "%s: value %f, ERANGE: %d, EDOM %d, errno %d\n", FuncName, d, ERANGE, EDOM, errno); 
      
      if (endp == strtp && *endp=='\0') { 
         eos = 1;
      } else {
         if (nd < N) fv[nd] = (float)d;
         strtp = endp;
         ++nd;
         if (nd > N && nd >1000) {
            SUMA_SL_Err("Something's fishy");
            fprintf (stderr, "s = >>>%s<<<\nnd = %d\n", s, nd);
            SUMA_RETURN(-1);
         }
      }
   }
   
   if (LocalHead) fprintf (stderr,"%s: Read %d/%d values.\n", FuncName, nd, N);
   
   SUMA_RETURN(nd);
   
}   

/*!
   \brief forces a string to be of a certain length.
   If truncation is necessary, ... are inserted at 
   the end of the string.
   
   You need to free the returned pointer
*/
char *SUMA_set_string_length(char *buf, char cp, int n)
{
   static char FuncName[]={"SUMA_set_string_length"};
   char *lbl=NULL, *lbl30=NULL;
   
   SUMA_ENTRY;
   
   if (!buf) SUMA_RETURN(NULL);
   
   lbl = SUMA_truncate_string (buf, n);
   if (!lbl) {
      SUMA_SL_Err("Failed to truncate");
      SUMA_RETURN(NULL);
   }
         
   if (strlen(lbl) != n) {
      lbl30 = SUMA_pad_string(lbl, ' ', n, 1); 
      SUMA_free(lbl); lbl = NULL;
   } else {
      lbl30 = lbl; lbl = NULL;
   }
   
   SUMA_RETURN(lbl30);
}

/*!
   \brief padds a string to a certain length.
   You can use this function to crop a string to 
   the specified number of characters n
   Padding is done with character cp
   The original string is not modified.
    
   s_tr = SUMA_pad_string(s1, cp, n, add2end);
   
   \sa SUMA_pad_str
   \sa SUMA_truncate_string
   - free returned pointer with: if(s_tr) SUMA_free(s_tr);
*/
char *SUMA_pad_string(char *buf, char cp, int n, int add2end)
{
   static char FuncName[]={"SUMA_pad_string"};
   char *atr = NULL;
   int i, ib, nb;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!buf) SUMA_RETURN(NULL);
   
   atr = (char *) SUMA_calloc(n+2, sizeof(char));
   nb = strlen(buf);
   
   if (add2end) { /* add to end */
      i=0;
      while (i < n) {
         if (i<nb) atr[i] = buf[i];
         else atr[i] = cp;
         ++i;
      }
      atr[i] = '\0';
   } else {
      atr[n] = '\0';
      i = n -1; 
      ib = nb - 1;
      while (i >= 0) {
         if (ib >=0) atr[i] = buf[ib];
         else atr[i] = cp;
         --i; --ib;
      }
      
   }
   
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\nin\t:%s:\nout\t:%s:\n", FuncName, buf, atr);
   }
   SUMA_RETURN(atr);  
}

/*!
   \brief truncates a string to a certain length.
   Adds ... as the last characters of the string
   The original string is not modified.
    
   s_tr = SUMA_truncate_string(s1, n);
   
   - free returned pointer with: if(s_tr) SUMA_free(s_tr);
*/
char *SUMA_truncate_string(char *buf, int n)
{
   static char FuncName[]={"SUMA_truncate_string"};
   char *atr = NULL;
   int i;

   SUMA_ENTRY;
   
   if (!buf) SUMA_RETURN(NULL);
   
   if (n < 5) {
      fprintf(stderr,"Error %s:\nNot worth the effort. N < 5.", FuncName);
      SUMA_RETURN(NULL);
   }
   
   if (strlen(buf) <= n) {
      atr = (char *) SUMA_calloc(strlen(buf)+2, sizeof(char));
      sprintf(atr, "%s", buf);
      SUMA_RETURN (atr);
   }else {
      atr = (char *) SUMA_calloc(n+3, sizeof(char));
      i=0;
      while (i < n - 3) {
         atr[i] = buf[i];
         ++i;
      }
      atr[i] = atr[i+1] = atr[i+2] = '.';
      atr[i+3] = '\0';
   }
   
   SUMA_RETURN(atr);  
}

/*!
   \brief returns a copy of a null terminated string . 
   s_cp = SUMA_copy_string(s1);
   
   - free returned pointer with: if(s_cp) SUMA_free(s_cp);
*/
char *SUMA_copy_string(char *buf)
{
   static char FuncName[]={"SUMA_copy_string"};
   char *atr = NULL;
   int i;
   
   SUMA_ENTRY;
   
   if (!buf) SUMA_RETURN(NULL);
   
   atr = (char *) SUMA_calloc(strlen(buf)+2, sizeof(char));
   
   i=0;
   while (buf[i]) {
      atr[i] = buf[i];
      ++i;
   }
   atr[i] = '\0';
   
   SUMA_RETURN(atr);  
}

/*!
   \brief appends two null terminated strings.
   
   s_ap = SUMA_append_string(s1, s2);
  
   - s1 and s2 are copied into a new string
   -free returned pointer with:  if(s_ap) SUMA_free(s_ap);
   - None of the strings passed to the function
   are freed.
   
   \sa SUMA_append_replace_string
*/
char * SUMA_append_string(char *s1, char *s2)
{
   static char FuncName[]={"SUMA_append_string"};
   char *atr = NULL;
   int i,cnt, N_s2, N_s1;

   
   SUMA_ENTRY;
   
   if (!s1 && !s2) SUMA_RETURN(NULL);
   if (!s1) N_s1 = 0;
   else N_s1 = strlen(s1);
   
   if (!s2) N_s2 = 0;
   else N_s2 = strlen(s2);
   
   atr = (char *) SUMA_calloc(N_s1+N_s2+2, sizeof(char));
   
   /* copy first string */
   cnt = 0;
   if (N_s1){
      i=0;
      while (s1[i]) {
         atr[cnt] = s1[i];
         ++i;
         ++cnt;
      }
   }   
   if (N_s2) {
      i=0;
      while (s2[i]) {   
         atr[cnt] = s2[i];
         ++i;
         ++cnt;
      }
   }
   atr[cnt] = '\0';
   
   SUMA_RETURN(atr);  
}    


/*!
   \brief appends two null terminated strings.
   
   s_ap = SUMA_append_replace_string(s1, s2, spc, whichTofree);
  
  \param s1 (char *) string 1
  \param s2 (char *) string 2
  \param spc (char *) spacing string
  \param whichTofree (int) 0 free none, 
                           1 free s1
                           2 free s2
                           3 free s1 and s2
   \return s_ap (char *) a string formed by "%s%s%s", s1, spc, s2
   
   - s1 and s2 are copied into a new string with spc in between
   - s1 (but not s2 or spc ) IS FREED inside this function
   -free returned pointer with:  if(s_ap) SUMA_free(s_ap);
   
   \sa SUMA_append_string
*/
char * SUMA_append_replace_string(char *s1, char *s2, char *Spc, int whichTofree)
{
   static char FuncName[]={"SUMA_append_replace_string"};
   char *atr = NULL;
   int i,cnt, N_s2, N_s1, N_Spc=0;

   
   SUMA_ENTRY;
   
   if (!s1 && !s2) SUMA_RETURN(NULL);
   
   if (!s1) N_s1 = 0;
   else N_s1 = strlen(s1);
   
   if (!s2) N_s2 = 0;
   else N_s2 = strlen(s2);
   
   if (!Spc) N_Spc = 0;
   else N_Spc = strlen(Spc);
   
   atr = (char *) SUMA_calloc(N_s1+N_s2+N_Spc+2, sizeof(char));
   
   /* copy first string */
   i=0;
   cnt = 0;
   if (s1) {
      while (s1[i]) {
         atr[cnt] = s1[i];
         ++i;
         ++cnt;
      }
   }
     
   i=0;
   if (Spc) {
      while (Spc[i]) {
         atr[cnt] = Spc[i];
         ++i;
         ++cnt;
      }
   }
   
   i=0;
   if (s2) {
      while (s2[i]) {   
         atr[cnt] = s2[i];
         ++i;
         ++cnt;
      }
   }
   atr[cnt] = '\0';
   
   switch (whichTofree) {
      case 0:
         break;
      case 1:
         if (s1) free(s1);
         break;
      case 2:
         if (s2) free(s2);
         break;
      case 3:
         if (s1) free(s1);
         if (s2) free(s2);
         break;
      default: 
         fprintf(stderr, "Error %s:\nBad freeing parameter\n"
                         "No variables were freed.\n",
                         FuncName);
         break;
   }  

   SUMA_RETURN(atr);  
}   
 
char * SUMA_append_replace_num(char *s1, char *form, double num, SUMA_VARTYPE tp, int whichTofree)
{
   static char FuncName[]={"SUMA_append_replace_num"};
   char *atr = NULL, sbuf[500];
   int i,cnt, N_s2, N_s1, N_Spc=0;

   
   SUMA_ENTRY;
   
   if (!form) SUMA_RETURN(NULL);
   if (!s1 && !form) SUMA_RETURN(NULL);
   if (whichTofree > 1) {
      SUMA_S_Err("Can only free s1");
      SUMA_RETURN(NULL);
   }
   if (!s1) N_s1 = 0;
   else N_s1 = strlen(s1);
   
   switch(tp) {
      case SUMA_short:
      case SUMA_int:
         snprintf(sbuf, 450, form, (int)num);
         break;
      case SUMA_float:
      case SUMA_double:
         snprintf(sbuf, 450, form, (double)num);
         break;
      default: 
         snprintf(sbuf, 450, "NUM_FORMAT_ERROR");
         break; 
   }  
         
   /* fprintf(SUMA_STDERR,"%s: Have %lf num, form:>%s<, sbuf>%s<\n", FuncName, num, form, sbuf); */
      
   atr = SUMA_append_replace_string(s1, sbuf, "", whichTofree);
   

   SUMA_RETURN(atr);  
}    

/*!
   \brief Appends newstring to string in SS->s while taking care of resizing space allocated for s
   
   \param SS (SUMA_STRING *) pointer to string structure
   \param newstring (char *) pointer to string to add to SS
   \return SS (SUMA_STRING *) pointer to string structure with SS->s now containing newstring
   - When SS is null, 1000 characters are allocated for s (initialization) and s[0] = '\0';
   - When newstring is NULL, space allocated for SS->s is resized to the correct dimension and 
   a null character is placed at the end.
   \sa SUMA_SS2S
*/
SUMA_STRING * SUMA_StringAppend (SUMA_STRING *SS, char *newstring)
{
   static char FuncName[]={"SUMA_StringAppend"};
   int N_inc = 0, N_cur = 0;
   int N_chunk = 1000;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SS) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Allocating for SS.\n", FuncName);
      SS = (SUMA_STRING *) SUMA_malloc (sizeof(SUMA_STRING));
      SS->s = (char *) SUMA_calloc (N_chunk, sizeof(char));
      SS->s[0] = '\0';
      SS->N_alloc = N_chunk;
      SUMA_RETURN (SS);
   }
   
   if (newstring) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Appending to SS->s.\n", FuncName);
      N_inc = strlen (newstring);
      N_cur = strlen (SS->s);
      if (SS->N_alloc < N_cur+N_inc+1) { /* must reallocate */
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Must reallocate for SS->s.\n", FuncName);
         SS->N_alloc = N_cur+N_inc+N_chunk+1;
         SS->s = (char *)SUMA_realloc (SS->s, sizeof(char)*SS->N_alloc);
         if (!SS->s) {
            fprintf (SUMA_STDERR, "Error %s: Failed to reallocate for s.\n", FuncName);
            SUMA_RETURN (NULL);
         }
      }
      /* append */
      sprintf (SS->s, "%s%s", SS->s, newstring);
   }else {
      /* shrink SS->s to small size */
      N_cur = strlen (SS->s);
      if (SS->N_alloc > N_cur+1) {
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Shrink realloc for SS->s.\n", FuncName);
         SS->N_alloc = N_cur+1;
         SS->s = (char *)SUMA_realloc (SS->s, sizeof(char)*SS->N_alloc);
         if (!SS->s) {
            fprintf (SUMA_STDERR, "Error %s: Failed to reallocate for s.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         /*put a null at the end */
         SS->s[SS->N_alloc-1] = '\0';
      }
   }
   
   SUMA_RETURN (SS);

}

/*!
   \brief Appends newstring to string in SS->s while taking care of resizing space allocated for s
   A variable argument version of SUMA_StringAppend
   
   \param SS (SUMA_STRING *) pointer to string structure
   \param newstring (char *) pointer to string to add to SS
   \param ..... the remaining parameters a la printf manner
   \return SS (SUMA_STRING *) pointer to string structure with SS->s now containing newstring
   - When SS is null, 1000 characters are allocated for s (initialization) and s[0] = '\0';
   - When newstring is NULL, space allocated for SS->s is resized to the correct dimension and 
   a null character is placed at the end.
   
   - For this function, the formatted length of newstring should not be > than MAX_APPEND-1 
   If that occurs, the string will be trunctated and no one should get hurt
   
   NOTE: DO NOT SEND NULL pointers in the variable argument parts or crashes will occur on SUN
   Such NULL pointers do not result in null vararg_ptr and cause a seg fault in vsnprintf
   
   \sa SUMA_StringAppend
   \sa SUMA_SS2S
*/

#define MAX_APPEND 3000

SUMA_STRING * SUMA_StringAppend_va (SUMA_STRING *SS, char *newstring, ... )
{
   static char FuncName[]={"SUMA_StringAppend_va"};
   char sbuf[MAX_APPEND];
   int nout;
   va_list vararg_ptr ;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SS) {
      SUMA_LH("NULL SS");
      /* let the other one handle this */
      SUMA_RETURN (SUMA_StringAppend(SS,newstring));
   }
   
   if (newstring) {
      SUMA_LH("newstring ...");
      /* form the newstring and send it to the olde SUMA_StringAppend */
      va_start( vararg_ptr ,  newstring) ;
      if (strlen(newstring) >= MAX_APPEND -1 ) {
         SUMA_SL_Err("newstring too long.\nCannot use SUMA_StringAppend_va");
         SUMA_RETURN(SUMA_StringAppend(SS,"Error SUMA_StringAppend_va: ***string too long to add ***"));
      }
      if (LocalHead) {
         SUMA_LH("Calling vsnprintf");
         if (vararg_ptr) {
            SUMA_LH("Non NULL vararg_ptr");
         } else {
            SUMA_LH("NULL vararg_ptr");
         }
      }
      nout = vsnprintf (sbuf, MAX_APPEND * sizeof(char), newstring, vararg_ptr); 
      if (LocalHead) fprintf(SUMA_STDERR,"%s:\n Calling va_end, nout = %d\n", FuncName, nout);
      va_end(vararg_ptr);  /* cleanup */
      
      if (nout < 0) {
         SUMA_SL_Err("Error reported by  vsnprintf");
         SUMA_RETURN(SUMA_StringAppend(SS,"Error SUMA_StringAppend_va: ***Error reported by  vsnprintf"));
      }
      if (nout >= MAX_APPEND) {
         SUMA_SL_Warn("String trunctated by vsnprintf");
         SUMA_StringAppend(SS,sbuf);
         SUMA_RETURN(SUMA_StringAppend(SS,"WARNING: ***Previous string trunctated because of its length. ***"));
      }
      SUMA_LH("Calling StringAppend");
      SUMA_RETURN (SUMA_StringAppend(SS,sbuf));
   }else {
      SUMA_LH("NULL newstring");
      /* let the other one handle this */
      SUMA_RETURN (SUMA_StringAppend(SS,newstring));
   }
   
   /* should not be here */
   SUMA_RETURN (NULL);

}


void SUMA_sigfunc(int sig)   /** signal handler for fatal errors **/
{
   char * sname ;
   static volatile int fff=0 ;
   if( fff ) _exit(1) ; else fff = 1 ;
   switch(sig){
      default:      sname = "unknown" ; break ;
      case SIGINT:  sname = "SIGINT(ctrl+c)"  ; break ;
      case SIGPIPE: sname = "SIGPIPE(broken pipe)" ; break ;
      case SIGSEGV: sname = "SIGSEGV(access outside limits)" ; break ;
      case SIGBUS:  sname = "SIGBUS(access violation)"  ; break ;
      case SIGTERM: sname = "SIGTERM(termination requested)" ; break ;
   }
   fprintf(stderr,"\nFatal Signal %d (%s) received\n",sig,sname); fflush(stderr);
   TRACEBACK ;
   fprintf(stderr,"*** Program Abort ***\nSUMA Version %.2f\nCompile Date: %s\n", SUMA_LatestVersionNumber(), __DATE__) ; fflush(stderr) ;
   exit(1) ;
}

/*--------------------------------------------------------------------*/
/*! My version of Bob's Decode a single string into a bunch of strings, separated
    by characters from the list in sep.
    - Passing sep in as NULL means to use "," as the separator.
    - In each sub-string, leading and trailing blanks will be excised.
    - This can result in 0 length strings (e.g., "1,,2," will result
      in the second and fourth output strings having 0 length).
   \sa SUMA_NI_get_ith_string
----------------------------------------------------------------------*/

NI_str_array * SUMA_NI_decode_string_list( char *ss , char *sep )
{
   static char FuncName[]={"SUMA_NI_decode_string_list"};
   NI_str_array *sar ;
   int num , nn,id,jd , lss ;

   if( ss == NULL || ss[0] == '\0' ) return NULL ; /* bad input */

   if( sep == NULL || sep[0] == '\0' ) sep = "," ;  /* default sep */

   sar = NI_malloc(NI_str_array, sizeof(NI_str_array)) ;  /* create output */
   sar->num = 0 ; sar->str = NULL ;

   /* scan for sub-strings */

   lss = NI_strlen(ss) ;
   num = id = 0 ;
   while( id < lss ){

      /* skip current position ahead over whitespace */

      while( id < lss && isspace(ss[id]) ) id++ ;
      if( id == lss ) break ;                           /* ran out of string */

      jd = id ;               /* save current position (start of new string) */

      /* skip ahead until ss[id] is a separator [or a space - 10 Dec 2002 ZSS I don't like that one, 
                              gives me funny looking results with legitimate spaces ,
                              line below was: while( id < lss && strchr(sep,ss[id]) == NULL  && !isspace(ss[id])) id++; ] */

      while( id < lss && strchr(sep,ss[id]) == NULL ) id++;
      if( id == jd ){ id++; continue; }    /* is only a separator? */

      /* new sub-string runs from ss[jd] to ss[id-1] */

      sar->str = NI_realloc( sar->str , char*, sizeof(char *)*(num+1) ) ;

      nn = id-jd ;                                   /* length of sub-string */
#if 0
      while( nn > 0 && isspace(ss[jd+nn-1]) ) nn-- ; /* clip trailing blanks */
#endif
      sar->str[num] = NI_malloc(char, (nn+1)*sizeof(char)) ;              /* make output string  */
      if( nn > 0 ) memcpy(sar->str[num],ss+jd,nn) ;  /* copy sub-string    */
      sar->str[num++][nn] = '\0' ;                   /* terminate output  */

      id++ ;                                         /* skip separator  */
   }

   sar->num = num ; return sar ;
}

/*--------------------------------------------------------------------*/
/*! \brief Returns a copy of the ith string in a string list. 
\sa SUMA_NI_decode_string_list ( on which this function is based)
----------------------------------------------------------------------*/

char  * SUMA_NI_get_ith_string( char *ss , char *sep, int i )
{
   static char FuncName[]={"SUMA_NI_get_ith_string"};
   char *str =NULL;
   int num , nn,id,jd , lss ;
   
   SUMA_ENTRY;
   
   if( ss == NULL || ss[0] == '\0' || i<0) SUMA_RETURN( NULL ) ; /* bad input */

   if( sep == NULL || sep[0] == '\0' ) sep = "," ;  /* default sep */


   /* scan for sub-strings */

   lss = NI_strlen(ss) ;
   num = id = 0 ;
   while( id < lss ){

      /* skip current position ahead over whitespace */

      while( id < lss && isspace(ss[id]) ) id++ ;
      if( id == lss ) break ;                           /* ran out of string */

      jd = id ;               /* save current position (start of new string) */

      /* skip ahead until ss[id] is a separator [or a space - 10 Dec 2002 ZSS I don't like that one, 
                              gives me funny looking results with legitimate spaces ,
                              line below was: while( id < lss && strchr(sep,ss[id]) == NULL  && !isspace(ss[id])) id++; ] */

      while( id < lss && strchr(sep,ss[id]) == NULL ) id++;
      if( id == jd ){ id++; continue; }    /* is only a separator? */



      nn = id-jd ;                                   /* length of sub-string */
      
      if (i==num) { /* that is the one I want */
         /* new sub-string runs from ss[jd] to ss[id-1] */
         str = (char *) SUMA_malloc( sizeof(char )*(nn+1) ) ;
         if( nn > 0 ) memcpy(str,ss+jd,nn) ;  /* copy sub-string    */
         str[nn] = '\0' ;                   /* terminate output  */
         SUMA_RETURN(str);
      } 
      ++num;   
      id++ ;                                         /* skip separator  */
   }

   /* not found, return with NULL */
   SUMA_RETURN( str );
}
void SUMA_Show_NI_str_ar(NI_str_array *nisa, FILE *out)
{
   static char FuncName[]={"SUMA_Show_NI_str_ar"};
   int i;
   char *s=NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   if (!out) out = SUMA_STDOUT;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (!nisa) SS = SUMA_StringAppend_va(SS, "NULL struct");
   else {
      SS = SUMA_StringAppend_va(SS, "%d strings:\n", nisa->num);
      for (i=0; i<nisa->num; ++i) {
         SS = SUMA_StringAppend_va(SS, "\t%d->>>%s<<<\n", i, nisa->str[i]);
      }
   }
   
   SUMA_SS2S(SS,s);
   
   fprintf(out, "%s", s); SUMA_free(s); s= NULL;
   fflush(out);
   SUMA_RETURNe;
}

/*!
   \brief take a bunch of strings stored in NI_str_array
   and turn them into a composite string
   Free result with SUMA_free
   \sa SUMA_NI_str_ar_2_comp_str
*/
char *SUMA_NI_str_ar_2_comp_str (NI_str_array *nisa, char *sep)
{
   static char FuncName[]={"SUMA_NI_str_array_2_string"};
   char *ar = NULL, *s=NULL;
   int i, nsep, k, ns, cnt, Nchars = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) SUMA_Show_NI_str_ar(nisa, NULL);
   
   if (!nisa) SUMA_RETURN(NULL);
   
   if (sep) nsep = strlen(sep);
   else nsep = 0;
   
   /* what's the total number of chars ? */
   for (i=0; i<nisa->num; ++i) {
      if (nisa->str[i]) { Nchars += (strlen(nisa->str[i])+nsep+1) ; } /* be safe allocate a bit more ...*/
      else Nchars += (nsep+1); /* for separator */
   }
   
   ar = (char *)SUMA_malloc(sizeof(char)*Nchars);
   
   cnt = 0;
   for (i=0; i<nisa->num; ++i) { 
      s = nisa->str[i];
      if (s) {
         ns = strlen(s); 
      } else {
         ns = 0;
      }
      k = 0;
      while (k < ns) { ar[cnt] = s[k]; ++k; ++cnt; }
      k = 0;
      while (k < nsep) { ar[cnt] = sep[k]; ++k; ++cnt; }
   }
   ar[cnt] = '\0'; /* le bouchon */
   
   SUMA_RETURN(ar);
}

/*!
   \brief Inverse of SUMA_NI_str_ar_2_comp_str
   free output with SUMA_free_NI_str_array
*/
NI_str_array *SUMA_comp_str_2_NI_str_ar(char *s, char *sep)
{
   static char FuncName[]={"SUMA_comp_str_2_NI_str_ar"};
   NI_str_array *nisa = NULL;
   
   SUMA_ENTRY;
   
   if (!s) SUMA_RETURN(nisa);
   
   nisa = SUMA_NI_decode_string_list(s, sep);
   
   SUMA_RETURN(nisa);
}

NI_str_array *SUMA_free_NI_str_array(NI_str_array *nisa)
{
   static char FuncName[]={"SUMA_free_NI_str_array"}; 
   int i;
   
   SUMA_ENTRY;
   
   if (nisa) {
      for (i=0; i<nisa->num; ++i) {
         if (nisa->str[i]) NI_free(nisa->str[i]); nisa->str[i] = NULL;
      }
      NI_free(nisa); nisa = NULL;
   }
   
   SUMA_RETURN(nisa);
}

/*!
   \brief returns the iith string in a sep separated composite string cs 
   free result with SUMA_free
*/
char *SUMA_Get_Sub_String(char *cs, char *sep, int ii)
{
   static char FuncName[]={"SUMA_Get_Sub_String"};
   NI_str_array *nisa=NULL;
   char *s = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (ii < 0) { SUMA_SL_Err("Bad index"); SUMA_RETURN(s); }
   if (!cs) { SUMA_SL_Err("NULL input"); SUMA_RETURN(s); }
   #if 0 /* old slow way */
      nisa = SUMA_comp_str_2_NI_str_ar(cs, sep);
      if (LocalHead) SUMA_Show_NI_str_ar(nisa, NULL);
      if (!nisa) { SUMA_SL_Err("Failed in SUMA_comp_str_2_NI_str_ar"); SUMA_RETURN(s); }
      if (ii >= nisa->num) { /* SUMA_SL_Warn("not enough strings"); */ SUMA_RETURN(s); }
      s = SUMA_copy_string(nisa->str[ii]);
      SUMA_free_NI_str_array(nisa); nisa = NULL;
   #else 
      s = SUMA_NI_get_ith_string( cs , sep, ii );

   #endif
   SUMA_RETURN(s);
}

/*!
   \brief replace the col th string attribute in a one-string nel
*/
int SUMA_AddColAtt_CompString(NI_element *nel, int col, char *lbl, char *sep)
{
   static char FuncName[]={"SUMA_AddColAtt_CompString"};
   NI_str_array *nisa = NULL;
   char *cs=NULL, *ns=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!nel) { SUMA_SL_Err("NULL element"); SUMA_RETURN(NOPE); }
   if (col < 0) { SUMA_SL_Err("Negative column index"); SUMA_RETURN(NOPE); }
   
   SUMA_NEL_GET_STRING(nel, 0, 0, cs); /* composite string */
   
   if (!cs) { /* first baby, put it in */
      if (LocalHead) fprintf(SUMA_STDERR,"%s: 1st string: %s\n", FuncName, lbl);
      SUMA_NEL_REPLACE_STRING(nel, 0, 0, lbl); 
      SUMA_RETURN(YUP);
   }
   

   nisa = SUMA_comp_str_2_NI_str_ar(cs, sep);
   if (!nisa) { SUMA_SL_Err("Failed in SUMA_comp_str_2_NI_str_ar"); SUMA_RETURN(NOPE); }
   
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: col = %d, nisa->num = %d\n", FuncName, col, nisa->num);
   }
   if (col > nisa->num) { SUMA_SL_Err("col > nisa->num"); SUMA_DUMP_TRACE("nisa nisa"); SUMA_RETURN(NOPE); }
   
   if (col == nisa->num) { /* add at the end */
      if (LocalHead) fprintf(SUMA_STDERR,"%s: append %s to end of %s\n", FuncName, lbl, cs); 
      ns = SUMA_append_replace_string(cs, lbl, sep, 0);
      SUMA_NEL_REPLACE_STRING(nel, 0, 0, ns);   
   } else { /* add in middle */
      if (nisa->str[col]) NI_free(nisa->str[col]); nisa->str[col] = NULL;
      if (lbl) {
         nisa->str[col] = (char*)NI_malloc(char, (strlen(lbl)+1)*sizeof(char));
         strcpy( nisa->str[col],  lbl ); 
         if (LocalHead) fprintf(SUMA_STDERR,"%s: inserted %s at location %d\n", FuncName, lbl, col);
         ns = SUMA_NI_str_ar_2_comp_str(nisa, sep);
         if (LocalHead) fprintf(SUMA_STDERR,"%s: final string is %s\n", FuncName, ns);
         SUMA_NEL_REPLACE_STRING(nel, 0, 0, ns); 
      }
   }
   if (ns) SUMA_free(ns); ns = NULL;
   if (nisa) SUMA_free_NI_str_array(nisa); nisa = NULL;
   
   SUMA_RETURN(YUP);
}


/*! Swap the 4 bytes pointed to by ppp: abcd -> dcba. */

void SUMA_swap_4(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;

   b0 = *pntr; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   *pntr = b3; *(pntr+1) = b2; *(pntr+2) = b1; *(pntr+3) = b0;
}

/*---------------------------------------------------------------*/

/*! Swap the 8 bytes pointed to by ppp: abcdefgh -> hgfedcba. */

void SUMA_swap_8(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;
   unsigned char b4, b5, b6, b7;

   b0 = *pntr    ; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   b4 = *(pntr+4); b5 = *(pntr+5); b6 = *(pntr+6); b7 = *(pntr+7);

   *pntr     = b7; *(pntr+1) = b6; *(pntr+2) = b5; *(pntr+3) = b4;
   *(pntr+4) = b3; *(pntr+5) = b2; *(pntr+6) = b1; *(pntr+7) = b0;
}

/*---------------------------------------------------------------*/

/*! Swap the 2 bytes pointed to by ppp: ab -> ba. */

void SUMA_swap_2(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1;

   b0 = *pntr; b1 = *(pntr+1);
   *pntr = b1; *(pntr+1) = b0;
}

/*!
   \brief 
   \param endian (int)  0 : Don't know, do nothing
                        MSB_FIRST
                        LSB_FIRST
*/
void *SUMA_BinarySuck(char *fname, SUMA_VARTYPE data_type, int endian, int start, int end, int *nvals_read)
{
   static char FuncName[]={"SUMA_BinarySuck"};
   int bs, End, chnk, read_n, N_alloc, ex;
   FILE *fp=NULL;
   void *ans = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   *nvals_read = 0;
   ans = NULL;

   if (!SUMA_filexists(fname)) { SUMA_SL_Err("File not found or could not be read"); goto CLEAN_EXIT; }
   if (start < 0) { SUMA_SL_Err("Neg start val!"); goto CLEAN_EXIT; }

   /* byte swapping? */
   bs = 0;
   SUMA_WHAT_ENDIAN(End);
   if (endian && endian != End) bs = 1;         

   /* open file */
   fp = fopen(fname,"r");
   if (!fp) { SUMA_SL_Err("Failed to open file for read op."); goto CLEAN_EXIT; }

   /* skip top, if need be */
   if (start) fseek(fp, start, SEEK_SET);

   /* size of each value */
   chnk = SUMA_SizeOf(data_type);
   if (chnk <= 0) {  SUMA_SL_Err("Bad data type"); goto CLEAN_EXIT; }

   /* how many values to read ? */
   read_n = -1;
   if (end > 0) {
      read_n = (end-start)/chnk;
      if (read_n < 0) { SUMA_SL_Err("Bad end, start values"); goto CLEAN_EXIT;  }
   }

   /* now start reading until you reach eof or end byte */
   if (read_n >= 0)  N_alloc = read_n;
   else N_alloc = ( THD_filesize( fname ) - start) / (unsigned long)chnk;
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Expecting to read %d values\n", FuncName, N_alloc);
   
   ex = 0;
   switch (data_type) {
      case SUMA_float:
         {
            float *vec = (float *)SUMA_malloc(N_alloc * sizeof(float));
            if (!vec) { SUMA_SL_Err("Failed to allocate"); goto CLEAN_EXIT;  }
            SUMA_LH("Reading floats");
            ex = fread((void*)vec, chnk, N_alloc, fp);
            if (ex != N_alloc) { SUMA_SL_Err("Failed to read all data!"); SUMA_free(vec); goto CLEAN_EXIT; }
            if (bs) { SUMA_LH("swapping");  SUMA_SWAP_VEC(vec,N_alloc,chnk); }
            ans = (void*)vec;
         }
         break;
      case SUMA_int:
         {
            int *vec = (int *)SUMA_malloc(N_alloc * sizeof(int));
            if (!vec) { SUMA_SL_Err("Failed to allocate"); goto CLEAN_EXIT;  }
            SUMA_LH("Reading ints");
            ex = fread((void*)vec, chnk, N_alloc, fp);
            if (ex != N_alloc) { SUMA_SL_Err("Failed to read all data!"); SUMA_free(vec); goto CLEAN_EXIT; }
            if (bs) { SUMA_LH("swapping");  SUMA_SWAP_VEC(vec,N_alloc,chnk); }
            ans = (void*)vec;
         }
         break;
      case SUMA_byte:
         {
            byte *vec = (byte *)SUMA_malloc(N_alloc * sizeof(byte));
            if (!vec) { SUMA_SL_Err("Failed to allocate"); goto CLEAN_EXIT;  }
            SUMA_LH("Reading bytes");
            ex = fread((void*)vec, chnk, N_alloc, fp);
            if (ex != N_alloc) { SUMA_SL_Err("Failed to read all data!"); SUMA_free(vec); goto CLEAN_EXIT; }
            if (bs) { SUMA_LH("swapping 1 byte numbers!? Nothing to do loco!");  }
            ans = (void*)vec;
         }
         break;
      case SUMA_short:
         {
            short *vec = (short *)SUMA_malloc(N_alloc * sizeof(short));
            if (!vec) { SUMA_SL_Err("Failed to allocate"); goto CLEAN_EXIT;  }
            SUMA_LH("Reading shorts");
            ex = fread((void*)vec, chnk, N_alloc, fp);
            if (ex != N_alloc) { SUMA_SL_Err("Failed to read all data!"); SUMA_free(vec); goto CLEAN_EXIT; }
            if (bs) { SUMA_LH("swapping");  SUMA_SWAP_VEC(vec,N_alloc,chnk); }
            ans = (void*)vec;
         }
         break;
      default:
         SUMA_SL_Err("data type not supported");
         break;
   }
   
   *nvals_read = ex;

   CLEAN_EXIT:
   if (fp) fclose(fp);
   
   SUMA_RETURN(ans);
}

/*!
   \brief A function to suck in an ascii file
   Shamelessly stolen from Bob's suck_file
   \sa SUMA_file_suck
*/
int SUMA_suck_file( char *fname , char **fbuf )
{
   static char FuncName[]={"SUMA_suck_file"};
   int  fd , ii ;
   unsigned long len; 
   char * buf ;

   SUMA_ENTRY;
   
   if( fname == NULL || fname[0] == '\0' || fbuf == NULL ) SUMA_RETURN(0) ;

   len = THD_filesize( fname ) ;
   if( len <= 0 ) SUMA_RETURN(0) ;

   buf = (char *) SUMA_malloc( sizeof(char) * (len+4) ) ;
   if( buf == NULL ) SUMA_RETURN(0) ;

   fd = open( fname , O_RDONLY ) ;
   if( fd < 0 ) SUMA_RETURN(0) ;

   ii = read( fd , buf , len ) ;
   close( fd ) ;
   if( ii <= 0 ){ SUMA_free(buf) ; SUMA_RETURN(0); }
   *fbuf = buf ; SUMA_RETURN(ii) ;
}
/*!
   \brief Another version of SUMA_suck_file that hopes to
   avoid the silly error on OSX 
   \sa SUMA_suck_file
*/
char * SUMA_file_suck( char *fname , int *nread )
{
   static char FuncName[]={"SUMA_file_suck"};
   int  fd , ii = 0;
   unsigned long len; 
   char * buf = NULL;

   SUMA_ENTRY;
   
   *nread = 0;
   if( fname == NULL || fname[0] == '\0') SUMA_RETURN(0) ;

   len = THD_filesize( fname ) ;
   if( len <= 0 ) SUMA_RETURN(buf) ;

   buf = (char *) SUMA_malloc( sizeof(char) * (len+4) ) ;
   if( buf == NULL ) SUMA_RETURN(buf) ;

   fd = open( fname , O_RDONLY ) ;
   if( fd < 0 ) SUMA_RETURN(buf) ;

   ii = read( fd , buf , len ) ;
   close( fd ) ;
   if( ii <= 0 ){ SUMA_free(buf) ; buf = NULL; SUMA_RETURN(buf); }
   *nread = ii ; SUMA_RETURN(buf) ;
}

/*!
   \brief Reads in a sequence of numbers of an undetermined length
   Not for reading in large numbers of numbers!
   \param op (char *) pointing to the beginning of a 
                     blank delimited series of numbers
   \param opend (char **) if not NULL, *opend will contain the value
                           of op at the end of successful reads
   \param tp (SUMA_VARTYPE) SUMA_int, SUMA_float, SUMA_double supported 
                           at the moment
   \return ans (void*) if  tp == SUMA_int then ans is (SUMA_IVEC *)
                           tp == SUMA_float then ans is (SUMA_FVEC *)
                           tp == SUMA_double then ans is (SUMA_DVEC *)
   \sa SUMA_strtol_vec  
   \sa SUMA_SringToNum
*/
void *SUMA_AdvancePastNumbers(char *op, char **opend, SUMA_VARTYPE tp)
{
   static char FuncName[]={"SUMA_AdvancePastNumbers"};
   double *d=NULL, db;
   int nrealloc = 0, Chunk = 100, nalloc = 0;
   int Found = 0, i, nread;
   void *ans;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   nread = 0;
   Found = 1;
   while (Found) {
      SUMA_ADVANCE_PAST_NUM(op, db, Found);
      if (Found) {
         if (nread == nalloc) {
            nalloc += Chunk; ++nrealloc;
            d = (double*)SUMA_realloc(d, nalloc*sizeof(double));
            if (!d) { SUMA_SL_Crit("Failed to allocate"); if (d) SUMA_free(d); d = NULL; SUMA_RETURN(NULL); }
            if (!(nrealloc % 10)) { SUMA_SL_Warn("Too much reallocation, improper use of function?"); }
         }
         d[nread] = db;
         ++(nread);
      }
   } 
   
   if (LocalHead) { 
      fprintf(SUMA_STDERR,"%s: Found %d numbers:\n", FuncName, nread);
      for (i=0; i<nread; ++i) fprintf(SUMA_STDERR,"%f\t", d[i]);
      fprintf(SUMA_STDERR,"\n");
   }
   
   if (opend) *opend = op;
   
   ans = NULL;
   switch (tp) {
      case SUMA_int:
         {
            SUMA_IVEC *ivec= (SUMA_IVEC *)SUMA_malloc(sizeof(SUMA_IVEC));
            ivec->v = (int *)SUMA_calloc(nread,sizeof(int));
            ivec->n = nread;
            for (i=0; i<nread; ++i) ivec->v[i] = (int)d[i];
            ans = (void *)ivec;
         }
         break;
      case SUMA_float:
         {
            SUMA_FVEC *fvec= (SUMA_FVEC *)SUMA_malloc(sizeof(SUMA_FVEC));
            fvec->v = (float *)SUMA_calloc(nread,sizeof(float));
            fvec->n = nread;
            for (i=0; i<nread; ++i) fvec->v[i] = (float)d[i];
            ans = (void *)fvec;
         }
         break;
      case SUMA_double:
         {
            SUMA_DVEC *dvec= (SUMA_DVEC *)SUMA_malloc(sizeof(SUMA_DVEC));
            dvec->v = (double *)SUMA_calloc(nread,sizeof(double));
            dvec->n = nread;
            for (i=0; i<nread; ++i) dvec->v[i] = (double)d[i];
            ans = (void *)dvec;
         }
         break;
      case SUMA_notypeset:
         SUMA_SL_Err("Type not set");
         ans = NULL;
         break;   
      default:
         SUMA_SL_Err("Type not supported by this function");
         ans = NULL;
         break;   
         
   }
   if (d) SUMA_free(d); d = NULL;
   
   SUMA_RETURN(ans);
   
}   
/*!
   \brief change a character string of numbers to a vector of values.
   op must be NULL terminated!
   
   \sa SUMA_AdvancePastNumbers
   \sa SUMA_StringToNum
*/
void *SUMA_strtol_vec(char *op, int nvals, int *nread, SUMA_VARTYPE vtp)
{
   static char FuncName[]={"SUMA_strtol_vec"};
   void *ans = NULL;
   long lv;
   double dv;
   char *endptr=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   *nread = 0;
   if (!SUMA_OK_OPENDX_DATA_TYPE(vtp)) {
      SUMA_SL_Err("Bad type");
      SUMA_RETURN(ans);
   }
   
   ans = NULL;
   switch (vtp) {
      case SUMA_byte:
         {
            byte *bvec=NULL;
            bvec = (byte *)SUMA_calloc(nvals,sizeof(byte));
            lv = strtol(op, &endptr, 10);
            while (endptr && endptr != op && *nread < nvals) {
               bvec[*nread] = (byte)lv;
               /* if (LocalHead) fprintf(SUMA_STDERR,">>>%d<<<\t", bvec[*nread]);  */
               ++(*nread);
               op = endptr;
               lv = strtol(op, &endptr, 10);
            }
            ans = (void *)bvec;
         }
         break;
      case SUMA_int:
         {
            int *ivec=NULL;
            ivec = (int *)SUMA_calloc(nvals,sizeof(int));
            lv = strtol(op, &endptr, 10);
            while (endptr && endptr != op && *nread < nvals) {
               ivec[*nread] = (int)lv;
               /* if (LocalHead) fprintf(SUMA_STDERR,">>>%d<<<\t", ivec[*nread]);  */
               ++(*nread);
               op = endptr;
               lv = strtol(op, &endptr, 10);
            }
            ans = (void *)ivec;
         }
         break;
      case SUMA_float:
         {
            float *fvec=NULL;
            fvec = (float *)SUMA_calloc(nvals,sizeof(float));
            dv = strtod(op, &endptr);
            while (endptr && endptr != op && *nread < nvals) {
               fvec[*nread] = (float)dv;
               /* if (LocalHead) fprintf(SUMA_STDERR,">>>%f<<<\t", fvec[*nread]); */
               ++(*nread);
               op = endptr;
               dv = strtod(op, &endptr);
            }
            ans = (void *)fvec;
         }
         break;
      case SUMA_double:
         {
            double *dvec=NULL;
            dvec = (double *)SUMA_calloc(nvals,sizeof(double));
            dv = strtod(op, &endptr);
            while (endptr && endptr != op && *nread < nvals) {
               dvec[*nread] = (double)dv;
               /* if (LocalHead) fprintf(SUMA_STDERR,">>>%f<<<\t", dvec[*nread]); */
               ++(*nread);
               op = endptr;
               dv = strtod(op, &endptr);
            }
            ans = (void *)dvec;
         }
         break;
      case SUMA_notypeset:
         SUMA_SL_Err("Type not set");
         ans = NULL;
         break;   
      default:
         SUMA_SL_Err("Type not supported by this function");
         ans = NULL;
         break;   
         
   }
   
   SUMA_RETURN(ans);
}

/*********************** END Miscellaneous support functions **************************** */
