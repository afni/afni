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

#include "suma_objs.h" /* 21 Apr 2020 */
/*------------------------------------------------------------*/

extern int selenium_close(void) ;

int SUMA_AddDsetNelIndexCol ( SUMA_DSET *dset, char *col_label, 
                               SUMA_COL_TYPE ctp, void *col, 
                               void *col_attr, int stride) ;


#if defined SUMA_COMPILED
   extern SUMA_CommonFields *SUMAg_CF;
   extern int SUMAg_N_DOv; 
   extern SUMA_DO *SUMAg_DOv;
#endif 

extern int * UniqueInt (int *y, int ysz, int *kunq, int Sorted );

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
      list = (DList *)SUMA_calloc(1,sizeof(DList));
      dlist_init(list, SUMA_FreeErrLog); 
   }
   el = (SUMA_ERRLOG*)SUMA_calloc(1,sizeof(SUMA_ERRLOG));
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
   char mmm[256];
   del = SUMA_PopErrLog(NULL);
   while (del) {
      el = (SUMA_ERRLOG *)del->data;
      sprintf(FuncName, "%s", el->FuncName); 
           if (!strcmp(el->macroname,"L_Err")) { SUMA_L_Err("%s", el->msg); }
      else if (!strcmp(el->macroname,"SL_Err")) { SUMA_SL_Err("%s", el->msg); }
      else if (!strcmp(el->macroname,"SLP_Err")) { SUMA_SLP_Err("%s", el->msg); }
      else if (!strcmp(el->macroname,"L_Warn")) { SUMA_L_Warn("%s", el->msg); }
      else if (!strcmp(el->macroname,"SL_Warn")) { SUMA_SL_Warn("%s", el->msg); }
      else if (!strcmp(el->macroname,"SLP_Warn")){SUMA_SLP_Warn("%s", el->msg); }
      else if (!strcmp(el->macroname,"L_Note")) { SUMA_L_Note("%s", el->msg); }
      else if (!strcmp(el->macroname,"SL_Note")) { SUMA_L_Note("%s", el->msg); }
      else if (!strcmp(el->macroname,"SLP_Note")){SUMA_SLP_Note("%s", el->msg); }
      else if (!strcmp(el->macroname,"L_Crit")) { SUMA_L_Crit("%s", el->msg); }
      else if (!strcmp(el->macroname,"SL_Crit")) { SUMA_SL_Crit("%s", el->msg); }
      else if (!strcmp(el->macroname,"SLP_Crit")){SUMA_SLP_Crit("%s", el->msg); }
      else {
         snprintf(mmm, 255*sizeof(char), "Bad macroname %s", el->macroname); 
         sprintf(FuncName, "%s", "WorkErrLog_ns"); SUMA_S_Err("%s",mmm);
      }
      del = SUMA_PopErrLog(del);
   }
}
 

/*!
   Creates a NI group to store surface data 
   N_el is the number of data elements stored in each column
   N_el can be the number of nodes for example. 
   N_eel is the number of edges for graph type datasets
*/
SUMA_Boolean SUMA_NewDsetGrp (SUMA_DSET *dset, SUMA_DSET_TYPE dtp, 
                           char* MeshParent_idcode, 
                          char * geometry_parent_idcode, 
                          int N_el, int N_eel,
                          char *filename, char *thisidcode)
{
   static char FuncName[]={"SUMA_NewDsetGrp"};
   char idcode[SUMA_IDCODE_LENGTH], *namecode, *dname;
   
   SUMA_ENTRY;
   
   if (!dset) { SUMA_SL_Err("NULL dset"); SUMA_RETURN(NOPE); }
   if (dset->N_links != 0) { 
      SUMA_SL_Err("Not expected here, N_links != 0"); 
      SUMA_RETURN(NOPE); 
   }
   
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
         NI_set_attribute (dset->ngr, "self_idcode", idcode); 
                  /* create one *//* changed from idcode March 31 */
      } else { 
         namecode = UNIQ_hashcode(filename);  /* from filename */
         NI_set_attribute (dset->ngr, "self_idcode", namecode); 
               SUMA_free(namecode);
      }
   } else {
      NI_set_attribute (dset->ngr, "self_idcode", thisidcode);
   }

   /* set the idcodes of the parents */
   if (MeshParent_idcode) {
      NI_set_attribute (dset->ngr, "domain_parent_idcode", MeshParent_idcode); 
   } else {
      NI_set_attribute (dset->ngr, "domain_parent_idcode", NULL); 
               /* don't use SUMA_EMPTY_ATTR unless you must NULL is nice*/
   }
   if (geometry_parent_idcode) {
      NI_set_attribute (dset->ngr, "geometry_parent_idcode", 
                        geometry_parent_idcode);
   } else {
      NI_set_attribute (dset->ngr, "geometry_parent_idcode", NULL);
   }
  
   if (filename) NI_set_attribute (dset->ngr, "filename", filename);
   
   /* add the data element */
   dname = SUMA_append_string(SUMA_Dset_Type_Name(dtp), "_data");
   dset->dnel = NI_new_data_element("SPARSE_DATA", N_el); 
   NI_set_attribute (dset->dnel, "data_type", dname); 
   SUMA_free(dname); dname = NULL;
   NI_add_to_group(dset->ngr, dset->dnel);
   
   /* Now add the node/edge  index element */
   if (SUMA_isGraphDset(dset)) 
         dname = SUMA_append_string(SUMA_Dset_Type_Name(dtp), "_edge_indices");
   else  dname = SUMA_append_string(SUMA_Dset_Type_Name(dtp), "_node_indices");
   dset->inel = NI_new_data_element("INDEX_LIST", N_el); 
   NI_set_attribute (dset->inel, "data_type", dname); 
   SUMA_free(dname); dname = NULL;
   NI_add_to_group(dset->ngr, dset->inel);

   if (SUMA_isGraphDset(dset)) {
      SUMA_S_Warn("Not bothering with point data yet. "
                  "If you don't end up adding this here, get rid of N_eel"
                  "Also not sure if I want to add data only or data and "
                  "indices. Consider also initializing to 0 N_eel, then"
                  "use NI_alter_veclen");
      if (0) {
         /* point (just to distinguish from node) data */
      dname = SUMA_append_string(SUMA_Dset_Type_Name(dtp), "_pdata");
      dset->pdnel = NI_new_data_element("P_SPARSE_DATA", N_eel); 
      NI_set_attribute (dset->pdnel, "data_type", dname);
      SUMA_free(dname); dname = NULL;
      NI_add_to_group(dset->ngr, dset->pdnel);
      }
   }

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
   
   if (!(nel = NI_new_data_element(SUMA_Dset_Type_Name(dtp), N_el))) {
      SUMA_S_Err("Failed to create nel");
      fprintf(SUMA_STDERR,"Had N_el = %d\n", N_el);
   }
   
   /* assign an idcode */
   if (!thisidcode) {
      if (!filename) {
         UNIQ_idcode_fill(idcode);
         NI_set_attribute (nel, "self_idcode", idcode); /* create one */
      } else { 
         namecode = UNIQ_hashcode(filename);  /* from filename */
         NI_set_attribute (nel, "self_idcode", namecode); SUMA_free(namecode);
      }
   } else {
      NI_set_attribute (nel, "self_idcode", thisidcode);
   }
   

   /* set the idcodes of the parents */
   if (MeshParent_idcode) {
      NI_set_attribute (nel, "domain_parent_idcode", MeshParent_idcode); 
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
   \brief Returns a copy of a string attribute for 
   one column. Yours to free when done with it.
*/
char *SUMA_DsetColStringAttrCopy(SUMA_DSET *dset, int i, 
                                 int addcolnum, char *attrname)
{
   static char FuncName[]={"SUMA_DsetColStringAttrCopy"};
   char Name[500], *lbl = NULL, **sc=NULL, *s=NULL;
   NI_element *nelb=NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->dnel || !attrname) { SUMA_RETURN(NULL); }
   if (i < 0 || i >= SDSET_VECNUM(dset)) { SUMA_RETURN(NULL); }
   
   if (!(nelb = SUMA_FindDsetAttributeElement(dset, attrname))) {
      SUMA_RETURN(NULL);
   }
   SUMA_NEL_GET_STRING(nelb, 0, 0, lbl); 
      /* sc is a pointer copy here, do not free */
   lbl = SUMA_Get_Sub_String(lbl, SUMA_NI_CSS, i);
   sprintf(Name, "%d: ", i);
   if (lbl) { 
      if (addcolnum) s = SUMA_append_string(Name, lbl); 
      else s = SUMA_copy_string(lbl); 
      SUMA_free(lbl); lbl = NULL;
   }
   
   SUMA_RETURN(s);
}

/* A conveninence function to get a column's label.
   Do not free what you get back. Labels are truncated at 64 chars.
 */
char *SUMA_DsetColLabel(SUMA_DSET *dset, int i) 
{
   static char ans[5][65]={""};
   static int n=0;
   char *ss=NULL;
   
   ++n; if (n>4) n=0;
   ans[n][0]='\0';
   
   if (!(ss = SUMA_DsetColLabelCopy(dset, i,0))) {
      return(ans[n]);
   }
   snprintf(ans[n],64*sizeof(char),"%s",ss);
   SUMA_free(ss); 
   return(ans[n]);
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
   
   if (!(nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_LABS"))) {
      if (!addcolnum) {
         SUMA_RETURN(SUMA_copy_string("no_label"));
      } else {
         sprintf(Name, "%d: ", i);
         SUMA_RETURN(SUMA_append_string(Name,"no_label"));
      }
   }
   SUMA_NEL_GET_STRING(nelb, 0, 0, lbl); 
                     /* lbl is a pointer copy here, do not free */
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

int SUMA_FindDsetColLabeled(SUMA_DSET *dset, char *label) 
{
   static char FuncName[]={"SUMA_FindDsetColLabeled"};
   int ind=-1;
   NI_element *nelb=NULL;
   char *lbl=NULL, *str=NULL;
   
   SUMA_ENTRY;
   
   if (!label || !dset || 
       !(nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_LABS"))) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(-1);    
   }
   SUMA_NEL_GET_STRING(nelb, 0, 0, lbl); 
   
   if (strstr(lbl,label)) {/* have something */
      for (ind=0; ind<SDSET_VECNUM(dset); ++ind) {
         if ((str = SUMA_DsetColLabelCopy(dset, ind, 0))) {
            if (!strcmp(str,label)) {
               SUMA_free(str); SUMA_RETURN(ind);
            } else SUMA_free(str);
         }
      }
   }
   
   SUMA_RETURN(-1);
}

/*!
   Return a NULL terminated vector of strings containing a copy 
   of all labels 
   free with SUMA_FreeAllDsetColLabels
*/
char **SUMA_AllDsetColLabels(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_AllDsetColLabels"};   
   char ** AllLabels=NULL;
   int ii;
   
   if (!dset) return(NULL);
   
   AllLabels = (char **)SUMA_calloc(SDSET_VECNUM(dset)+1,sizeof(char*));
   
   for (ii=0; ii<SDSET_VECNUM(dset); ++ii)
      AllLabels[ii] = SUMA_DsetColLabelCopy(dset, ii, 0);
      
   AllLabels[SDSET_VECNUM(dset)] = NULL;
   
   return(AllLabels);
}

char **SUMA_FreeAllDsetColLabels(char **AllLabels)
{
   static char FuncName[]={"SUMA_FreeAllDsetColLabels"}; 
   int ii=0;
   
   if (!AllLabels) return(NULL);
   
   while (AllLabels[ii]) {
      SUMA_free(AllLabels[ii]); 
      ++ii;
   }
   SUMA_free(AllLabels); 
   return(NULL);
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
   NI_element *nel=NULL;
    
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr) { SUMA_SL_Err("NUll input "); SUMA_RETURN(NULL); }
   attname = SUMA_append_string(NEL_DSET_TYPE(dset->ngr),"_data");
   
   nel = SUMA_FindNgrDataElement(dset->ngr, "SPARSE_DATA", attname);
   SUMA_free(attname);
   SUMA_RETURN(nel);
}


NI_element *SUMA_FindDsetDatumIndexElement(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_FindDsetDatumIndexElement"};
   char *attname=NULL;
   NI_element *nel=NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr) { SUMA_SL_Err("NUll input "); SUMA_RETURN(NULL); }
   if (SUMA_isGraphDset(dset)) 
        attname = SUMA_append_string(NEL_DSET_TYPE(dset->ngr),"_edge_indices");
   else attname = SUMA_append_string(NEL_DSET_TYPE(dset->ngr),"_node_indices");
   
   nel = SUMA_FindNgrDataElement(dset->ngr, "INDEX_LIST", attname);
   SUMA_free(attname);
   SUMA_RETURN(nel);
}

NI_element *SUMA_FindSDsetNodeIndexElement(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_FindSDsetNodeIndexElement"};
   char *attname=NULL;
   NI_element *nel=NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr) { SUMA_SL_Err("NUll input "); SUMA_RETURN(NULL); }
   attname = SUMA_append_string(NEL_DSET_TYPE(dset->ngr),"_node_indices");
   
   nel = SUMA_FindNgrDataElement(dset->ngr, "INDEX_LIST", attname);
   SUMA_free(attname);
   SUMA_RETURN(nel);
}

NI_element *SUMA_FindGDsetEdgeIndexElement(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_FindGDsetEdgeIndexElement"};
   char *attname=NULL;
   NI_element *nel=NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr) { SUMA_SL_Err("NUll input "); SUMA_RETURN(NULL); }
   attname = SUMA_append_string(NEL_DSET_TYPE(dset->ngr),"_edge_indices");
   
   nel = SUMA_FindNgrDataElement(dset->ngr, "INDEX_LIST", attname);
   SUMA_free(attname);
   SUMA_RETURN(nel);
}


NI_element *SUMA_FindNgrDataElement(
         NI_group *ngr, char *nelname, char *typename)
{
   static char FuncName[]={"SUMA_FindNgrDataElement"};
   NI_element *nel = NULL;
   char *rs=NULL;
   static int nwarn = 0;
   int ip;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!ngr || !typename || !nelname) { 
      SUMA_SL_Err("NUll input "); SUMA_RETURN(nel); 
   }
   
   /* search for an element of type nelname 
      that is also of data_type typename */
   for( ip=0 ; ip < ngr->part_num ; ip++ ){ 
      /* nel = (NI_element *)ngr->part[ip] ;
         SUMA_LHv("Working part %d/%d, name %s\n",
                  ip, ngr->part_num, nel->name); */
      switch( ngr->part_typ[ip] ){
         /*-- a sub-group ==> recursion! --*/
         case NI_GROUP_TYPE:
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)ngr->part[ip] ;
            if (LocalHead)  {
               fprintf(SUMA_STDERR,
                        "%s:  Looking for %s,%s   name=%s vec_len=%d "
                        "vec_filled=%d, vec_num=%d\n",
                        FuncName,
                        nelname, typename, nel->name, nel->vec_len, 
                        nel->vec_filled, nel->vec_num );
            }
            if (!strcmp(nelname, nel->name)) {/* now in keeping with AFNI */
               rs = NI_get_attribute(nel, "data_type");
               if (rs) { 
                  if (!strcmp(typename, rs)) SUMA_RETURN(nel);   
               } else {
                  if ( 0 && !(nwarn % 25)) {
                     fprintf(SUMA_STDERR, 
                     "Notice %s:\n"
                     "  NIML Dset's index column named %s \n"
                     "  is without any data_type attribute.\n"
                     "  Could not verify that element's \n"
                     "     data_type = %s .\n"
                     "  Ignore notice for dsets created by AFNI.\n"
                     "  Notice is shown intermittently in \n"
                     "  SUMA GUI mode.\n", FuncName, nel->name, typename);
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
            SUMA_SL_Err("Don't know what to make of this "
                        "group element, ignoring.");
            break;
      }
   }

   SUMA_RETURN(nel);
}

/*!
   Check if this is an attribute for all columns
   It has to be of type string and its name must begin with COLMS_
*/
SUMA_Boolean SUMA_isMultiColumnAttr(NI_element *nel)
{
   static char FuncName[]={"SUMA_isMultiColumnAttr"};
   NI_rowtype *rti=NULL;
   char *mm=NULL;

   SUMA_ENTRY;

   if (strcmp("AFNI_atr", nel->name)) SUMA_RETURN(NOPE);

   rti = NI_rowtype_find_code(nel->vec_typ[0]) ;
   if (rti->code != NI_STRING) SUMA_RETURN(NOPE);

   /* now check the name */ 
   mm = NI_get_attribute(nel, "atr_name");
   if (!mm || strncmp("COLMS_",mm, 6)) SUMA_RETURN(NOPE); 

   SUMA_RETURN(YUP);
}

/* Check to see if this an attribute for just one column
like FDRCURVE_000002
*/
SUMA_Boolean SUMA_isSingleColumnAttr(NI_element *nel, int *icolb, char *rtname)
{
   static char FuncName[]={"SUMA_isSingleColumnAttr"};
   NI_rowtype *rti=NULL;
   char *mm, mbuf[128], mbuf2[128];
   int ic=-1, found = -1, icol=-1;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (icolb) *icolb = icol;
   if (rtname) rtname[0] = '\0';

   if (strcmp("AFNI_atr", nel->name)) SUMA_RETURN(NOPE);

   mm = NI_get_attribute(nel, "atr_name");
   if (!mm ) SUMA_RETURN(NOPE); 

   /* search backwards for the first _ */
   ic = strlen(mm)-1;
   found = 0;
   while (!found && ic >=0) {
      if (mm[ic]=='_') found = ic+1;
      else --ic;
   }
   if (!found) SUMA_RETURN(NOPE);   /* no numbers here */
   /* make sure all is numbers past found */
   ic = found;
   while (SUMA_IS_DIGIT(*(mm+ic))) ++ic;
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: ic = %d, strlen = %d\n",
               FuncName, ic, (int)strlen(mm)); 
   }
   if (ic != strlen(mm)) SUMA_RETURN(NOPE);   /* not all digits after _ */

   /* now turn that potential number to int and then do comparison again*/

   icol = (int)atof((&(mm[found])));
   for (ic=0; ic<found-1; ++ic) mbuf2[ic] = mm[ic];
   mbuf2[ic] = '\0';
   sprintf(mbuf, "%s_%06d", mbuf2, icol);
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n"
                           "  >%s<\n"
                           "  >%s<\n"
                           "  >%s<\n",
               FuncName, (&(mm[found])), mbuf,mm);
   }
   if (strcmp(mbuf,mm)) SUMA_RETURN(NOPE); 

   if (icolb) *icolb = icol;
   if (rtname) sprintf(rtname, "%s", mbuf2);
   
   SUMA_RETURN(YUP);
}

/*!
   Check to see if this is an attribute for no column in particular
   like HISTORY
*/
SUMA_Boolean SUMA_isDsetwideColumnAttr(NI_element *nel) {
   static char FuncName[]={"SUMA_isDsetwideColumnAttr"};
   int icol=-1;

   SUMA_ENTRY;

   if (strcmp("AFNI_atr", nel->name)) SUMA_RETURN(NOPE);

   if (SUMA_isMultiColumnAttr(nel)) {
      SUMA_RETURN(NOPE);
   }
   if (SUMA_isSingleColumnAttr(nel, NULL, NULL)) {
      SUMA_RETURN(NOPE);
   }

   SUMA_RETURN(YUP);
}

/*!
   A stupid check to be sure this is an AFNI_atr
*/
SUMA_Boolean SUMA_isDsetNelAttr(NI_element *nel) {
   static char FuncName[]={"SUMA_isDsetNelAttr"};

   SUMA_ENTRY;

   if (!nel || !nel->name) SUMA_RETURN(NOPE);
   if (!strcmp("AFNI_atr", nel->name)) SUMA_RETURN(YUP);

   SUMA_RETURN(NOPE);
}
/*!
   Parses an attribute name and fills the following:
   tp --> 0 not an attribute
         1 SingleColumn
         2 MultiColumn
         3 Dsetwide
   icol --> the column number in case of tp = 1
   rtname --> the attribute name for tp != 1
               the attribute name's root for tp = 1
         */
SUMA_Boolean SUMA_ParseAttrName(NI_element *nel, int *tp, 
                                 int *icol, char *rtname)
{
   static char FuncName[]={"SUMA_ParseAttrName"};         
   int ltp = -1, licol = -1;
   char lrtname[128]={"error"};

   SUMA_ENTRY;

   if (!nel || (!tp && !icol && !rtname)) {
      SUMA_RETURN(NOPE);
   }

   if (SUMA_isDsetwideColumnAttr(nel)) {
      ltp = 3;
      strcpy(lrtname,NI_get_attribute(nel,"atr_name"));
   } else if (SUMA_isMultiColumnAttr(nel)) {
      ltp = 2;
      strcpy(lrtname,NI_get_attribute(nel,"atr_name"));
   } else if (SUMA_isSingleColumnAttr(nel, &licol, lrtname)) {
      ltp = 1;
   } else {
      ltp = 0;
      lrtname[0] = '\0';
   }

   if (tp) *tp = ltp;
   if (icol) *icol = licol;
   if (rtname) strcpy(rtname, lrtname);

   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_FindInAttrList(char **attrlist, char *attr,
                                 int icol, int *imatch)
{
   static char FuncName[]={"SUMA_FindInAttrList"};
   char nmbuf[128]={""};
   int iattrlist = -1;

   SUMA_ENTRY;
   
   
   if (!attrlist || !attr) SUMA_RETURN(NOPE);
   
   iattrlist = 0;
   while (attrlist[iattrlist] && iattrlist >= 0) {
      if (!strcmp(attrlist[iattrlist],attr)) { 
         if (imatch) *imatch = iattrlist;
         iattrlist = -1; /* a sign to quit */
      } else {
         ++iattrlist;
      }   
   } 
   if (iattrlist >= 0 && icol >= 0) {
      /* try for single-column attribute match */
      iattrlist = 0;
      while (attrlist[iattrlist] && iattrlist >= 0) {
         sprintf(nmbuf,"%s_%06d",attrlist[iattrlist],icol) ;
         if (!strcmp(nmbuf,attr)) { 
            /* a name match */
            if (imatch) *imatch = iattrlist;
            iattrlist = -1; /* a sign to quit */
         } else {
            ++iattrlist;
         }   
      }
   }
   
   if (iattrlist == -1) SUMA_RETURN(YUP);
   else SUMA_RETURN(NOPE);
}

/*!
   Makes a copy of a NI colormap */   
NI_group *SUMA_NICmapToNICmap(NI_group *ngr)
{
   static char FuncName[]={"SUMA_NICmapToNICmap"};
   int i;
   char *colname=NULL;
   NI_group *ngro=NULL;
   SUMA_DSET dset, *odset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("In");
   if (!ngr) SUMA_RETURN(ngro);
   
   /* shoehorn into dset struct */
   dset.ngr = ngr;
   dset.inel = dset.dnel = NULL;
   dset.dnel = SUMA_FindDsetDataElement(&dset);
   
   if (SDSET_TYPE((&dset)) != SUMA_LABEL_TABLE_OBJECT) {
      SUMA_S_Err("Not a colormap object");
      SUMA_RETURN(ngro);
   }
   
   /* make a copy of the meat */
   odset = SUMA_CreateDsetPointer(NI_get_attribute(ngr,"Name"), 
                                 SUMA_LABEL_TABLE_OBJECT, 
                                 NULL, NULL, SDSET_VECLEN((&dset)));
   
   /* Go for it */
   for (i=0; i<SDSET_VECNUM((&dset)); ++i) {
      colname = SUMA_DsetColLabelCopy(&dset, i, 0);
      if (!SUMA_AddDsetNelCol(odset, colname, SDSET_COLTYPE((&dset),i), 
                              SDSET_VEC((&dset),i), NULL, 1)){
            SUMA_S_Err("Failed to add R");
            SUMA_FreeDset(odset); odset = NULL;
            SUMA_RETURN(ngro);
      }
      if (colname) SUMA_free(colname); colname=NULL;
   }   
    
   /* the little people */
   NI_set_attribute(odset->ngr,"Name",
                    NI_get_attribute(dset.ngr,"Name"));
   NI_set_attribute(odset->ngr,"flipped",
                    NI_get_attribute(dset.ngr,"flipped"));
   NI_set_attribute(odset->ngr,"Sgn",
                    NI_get_attribute(dset.ngr,"Sgn"));
   NI_set_attribute(odset->ngr,"top_frac",
                    NI_get_attribute(dset.ngr,"top_frac"));
   NI_set_attribute(odset->ngr,"M0",
                    NI_get_attribute(dset.ngr,"M0"));
                    
   /* So this is not really a dset, but it was nice to make use of
   dset utility functions. Now cleanup a little */
   /* remove ugly inel */
   NI_remove_from_group(odset->ngr, odset->inel);
   
   /* grab ngr from dset, it is all we need */
   ngro = odset->ngr; odset->ngr = NULL; 
   
   /* change name from AFNI_dataset to AFNI_labeltable */
   NI_rename_group(ngro, "AFNI_labeltable");
   /* get rid of dset */
   odset->dnel = NULL; SUMA_FreeDset(odset); odset=NULL;
   
   SUMA_RETURN(ngro);
}

/*!
   Copies attributes from one dset to another
   src: Source dset
   dest: destination dset
   attrlist: NULL terminated list of attribute names to copy
   isrc: Column from which attributes should be copied. 
         This is useful for string type attributes
   idest: Column into which attributes are going
   
   Usually you run this function separately for Dsetwide attributes,
   setting both of isrc and idest to -1
   Or you run it for SingleColumn or MultiColumn attributes with both 
   isrc and idest properly set.
   
*/
SUMA_Boolean SUMA_CopyDsetAttributes ( SUMA_DSET *src, SUMA_DSET *dest,
                                       char **attrlist, 
                                       int isrc, int idest )
{
   static char FuncName[]={"SUMA_CopyDsetAttributes"};
   NI_element *nel=NULL, **nelcp=NULL, *nelt=NULL;
   NI_group *ngri=NULL, *ngro=NULL, *nicmap=NULL;
   char *rs=NULL, nmbuf[256], nmbuf2[256],*nm=NULL, *src_string=NULL;
   int ip=0, iattrlist=-1, inelcp=-1, N_ip=-1, tp=-1, icolsrc=-1, ic=-1;
   SUMA_Boolean ans = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!src || !src->ngr || !dest || !dest->ngr) SUMA_RETURN(NOPE);
   
   /* now read the elements in this group */
   ngri = src->ngr;
   /* allocate for enough attributes to copy */
   nelcp = (NI_element **)SUMA_calloc(ngri->part_num, sizeof(NI_element *));
   inelcp = 0;
   for( ip=0 ; ip < ngri->part_num ; ip++ ){ 
      switch( ngri->part_typ[ip] ){
         /*-- a sub-group ==> recursion! --*/
         case NI_GROUP_TYPE: /* not quite attributes, but oh well...*/
            nicmap = (NI_group *)ngri->part[ip] ;
            SUMA_LH("Have group named %s",nicmap->name);
            if (  !attrlist || 
                  SUMA_FindInAttrList(attrlist,nicmap->name, isrc, &iattrlist)) {
               SUMA_LH("Will copy it");
               /* make a copy */
               nicmap = SUMA_NICmapToNICmap(nicmap); 
               if (LocalHead) {
                  SUMA_ShowNel(nicmap);
               }
               NI_add_to_group(dest->ngr, nicmap);      
            }
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)ngri->part[ip] ;
            nm = NI_get_attribute(nel, "atr_name");
            if (LocalHead > 1)  {
               fprintf(SUMA_STDERR,
                        "%s:  src Col %d\n"
                        "     nel_name:atr_name =%s:%s\n"
                        "     vec_len=%6d vec_filled=%6d, vec_num=%4d", 
                        FuncName, isrc, 
                        nel->name ? nel->name : "NULL", 
                        nm ? nm : "NULL ",
                        nel->vec_len, nel->vec_filled, nel->vec_num );
            }
            if (SUMA_isDsetNelAttr(nel)) {
               if( (rs = NI_get_attribute(nel, "atr_name")) ) { 
                  #if 0
                  if (!attrlist) { 
                     /* No list, copy everything */
                     nelcp[inelcp] = nel; ++inelcp; 
                     if (LocalHead > 1)  {
                        fprintf( SUMA_STDERR,
                                 "      +++++ Added %s (indiscriminate)\n", 
                                 rs);
                     } 
                  } else {
                     /* do we have a perfect name match? */
                     iattrlist = 0;
                     while (attrlist[iattrlist] && iattrlist >= 0) {
                        if (!strcmp(attrlist[iattrlist],rs)) { 
                           /* have a perfect name match */
                           nelcp[inelcp] = nel; ++inelcp;
                           iattrlist = -1; /* a sign to quit */
                        } else {
                           ++iattrlist;
                        }   
                     }
                     
                     /* if we do not have a perfect match, 
                        try the number padded ones */
                     if (iattrlist >= 0) {
                        iattrlist = 0;
                        while (attrlist[iattrlist] && iattrlist >= 0) {
                           sprintf(nmbuf,"%s_%06d",attrlist[iattrlist],isrc) ;
                           if (!strcmp(nmbuf,rs)) { 
                              /* have a perfect name match */
                              nelcp[inelcp] = nel; ++inelcp;
                              iattrlist = -1; /* a sign to quit */
                           } else {
                              ++iattrlist;
                           }   
                        }
                     }
                     if (iattrlist < 0) {
                        if (LocalHead > 1)  {
                           fprintf( SUMA_STDERR,
                                    "          +++++ Added %s \n", 
                                    rs);
                        } 
                     }
                  }
                  #else
                  if (  !attrlist || 
                        SUMA_FindInAttrList(attrlist,rs, isrc, &iattrlist)) {
                     nelcp[inelcp] = nel; ++inelcp;
                     if (LocalHead > 1) {
                        if (!attrlist) {
                           fprintf( SUMA_STDERR,
                                    "  +++++ Added %s (indiscriminate)\n", 
                                    rs);
                        } else {
                           fprintf( SUMA_STDERR,
                                    "  +++++ Added %s \n", 
                                    rs);
                        } 
                     }  
                  }  else {
                      if (LocalHead > 1) {     
                           fprintf( SUMA_STDERR,
                                    "  ----- Not added\n"
                                    );
                      }
                  }
                  #endif 
               }
            } else if (nel == src->dnel) {
               if (  !attrlist || 
                     SUMA_FindInAttrList(attrlist,"TR",isrc, &iattrlist)) {
                  double TR=-1.0;
                  /* check on ni_timetep */
                  if (SUMA_is_TimeSeries_dset(src, &TR)) {
                     if (LocalHead > 1) {
                        fprintf( SUMA_STDERR,
                                 "  +++++ Added TR of %f\n",
                                 TR);
                     }
                     if (!SUMA_SetDsetTR(dest, TR)) {
                        SUMA_S_Warn("Could not set TR for output");
                     }
                  }
               } else {
                  if (LocalHead > 1) {
                        fprintf( SUMA_STDERR,
                                 "  ooooo Has no attributes\n"
                                 );
                  }
               } 
            } else {
               if (LocalHead > 1) {
                        fprintf( SUMA_STDERR,
                                 "  ooooo Not an attribute\n"
                                 );
               }
            } 
            break;
         default:
            SUMA_SL_Err("Don't know what to make of this\n"
                        " group element, ignoring.");
            goto OUT;
            break;
      }
   }
   if (LocalHead)  {
      fprintf( SUMA_STDERR,
               "\n"
               "%s: Src Col %d\n"
               "  Have %d NEL attributes to add to new dset Col %d.\n"
               , FuncName, isrc, inelcp, idest);
   }
   
   /* Now you go through the different attributes and add them */
   N_ip = inelcp;
   for (ip=0; ip<N_ip; ++ip) {
      nel = nelcp[ip];
      if (!SUMA_ParseAttrName(nel, &tp, &icolsrc, nmbuf)) {
         SUMA_S_Err("Should not happen!");
         goto OUT;
      }
      if (icolsrc >= 0 && icolsrc != isrc) {
         SUMA_S_Err("Weird index mismatch"); /* This should not be */
         goto OUT;
      }
      /* name of nel in new dset */
      if (tp == 1) { sprintf(nmbuf2,"%s_%06d", nmbuf, idest); }
      else strcpy(nmbuf2, nmbuf);
      if (LocalHead) {
         fprintf(SUMA_STDERR,
                  "%s:\n"
                  "  Have to add %s, type %d, col %d(old) to col %d (new):\n"
                  "  Attr name in destination dset: %s\n"
                  "  (isMulti %d) (isSingle %d) (isDsetwide %d)\n"
                  , FuncName, nmbuf, tp, isrc, idest
                  , nmbuf2
                  , SUMA_isMultiColumnAttr(nel)
                  , SUMA_isSingleColumnAttr(nel, NULL, NULL)
                  , SUMA_isDsetwideColumnAttr(nel) ); 
      }
      if (tp == 3 && isrc != -1) {
         SUMA_S_Err("Cannot add Dsetwide attributes with source column != -1");
         goto OUT;
      }
      /* Do we have this nel in target?*/
      nelt = SUMA_FindNgrAttributeElement(dest->ngr, 
                                          nmbuf2);
      if (nelt) {
         if (tp == 1 || tp == 3) {
            /* single-column or Dsetwide
               pre-existing attribute, kill it*/
            if (LocalHead) {
               fprintf(SUMA_STDERR,
                        "%s:\n"
                        "  Killing pre-existing %s attribute\n"
                        , FuncName, nmbuf2);
            }   
            NI_remove_from_group(dest->ngr, (void *)nelt); 
            NI_free_element(nelt); nelt = NULL;
         } else {
            if (LocalHead) {
               fprintf(SUMA_STDERR,
                        "%s:\n"
                        "  Keeping multi-column %s attribute\n"
                        , FuncName, nmbuf2);
            }   
         }
      }
      
      if (!nelt) {/* make new one, just like in src dset*/
         nelt = NI_new_data_element("AFNI_atr", nel->vec_len);
         NI_set_attribute(nelt, "atr_name", nmbuf2);
         for (ic=0; ic<nel->vec_num; ++ic) {
            NI_add_column_stride (nelt, nel->vec_typ[0], NULL, 1);
            NI_add_to_group(dest->ngr, nelt);
         }
      } 
      
      /* Now nelt exists, need put the data in there */
      if (tp == 2) { /* here we have a MultiColumn string type */
         if (isrc < 0 || idest < 0) {
            SUMA_S_Err("Should not be here!"); goto OUT;
         }
         src_string = 
            SUMA_GetDsetColStringAttr( src, isrc, 
                                       NI_get_attribute(nel, "atr_name"));

         if (LocalHead) {
               fprintf(SUMA_STDERR,
                        "%s:\n"
                        "  Inserting string %s at col %d in target's %s\n"
                        , FuncName, src_string, idest, nmbuf2);
         }
         SUMA_AddColAtt_CompString( nelt, idest,
                                    src_string,  
                                    SUMA_NI_CSS, 0);
         if (src_string) SUMA_free(src_string); src_string = NULL;     
      } else { /* just copy the whole thing */
         /* Obviously not a valid operation for something like 
         HISTORY_NOTE where appending rather than 
         overwriting is needed */
         if (LocalHead) {
               fprintf(SUMA_STDERR,
                        "%s:\n"
                        "  Copy source attribute contents\n"
                        , FuncName);
         }
         for (ic=0; ic<nel->vec_num; ++ic) {
            NI_fill_column_stride(nelt, nel->vec_typ[ic],
                                  nel->vec[ic], ic, 1);  
         }
      }                                    
   }
   
   ans = YUP;
   
   OUT:
   SUMA_free(nelcp); nelcp = NULL;
   SUMA_RETURN(YUP);
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
      if (LocalHead)  {
         fprintf( SUMA_STDERR,
                  "%s: %d/%d = %d\n", 
                  FuncName, ip, ngr->part_num, ngr->part_typ[ip]);
      }
      switch( ngr->part_typ[ip] ){
         /*-- a sub-group ==> recursion! --*/
         case NI_GROUP_TYPE:
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)ngr->part[ip] ;
            if (LocalHead)  {
               fprintf(SUMA_STDERR,
                        "%s:  Looking for %s   name=%s (%d/%d)\n"
                        "vec_len=%d vec_filled=%d, vec_num=%d\n", 
                        FuncName,
                        attname, nel->name, ip, ngr->part_num-1,
                        nel->vec_len, nel->vec_filled, nel->vec_num );
            }
            if (!strcmp("AFNI_atr", nel->name)) {/* now in keeping with AFNI */
               rs = NI_get_attribute(nel, "atr_name");
               if (rs) { 
                  if (!strcmp(attname, rs)) {
                     SUMA_RETURN(nel);   
                  }
               }
            }
            /* cancel plans if you get here */
            nel = NULL;
            break;
         default:
            SUMA_SL_Err(
               "Don't know what to make of this group element, ignoring.");
            if (LocalHead) {
               fprintf(SUMA_STDERR,"%s: type = %d (know of %d and %d only)!\n",
                                    FuncName, ngr->part_typ[ip], 
                                    NI_GROUP_TYPE, NI_ELEMENT_TYPE);
            }
            break;
      }
   }

   SUMA_RETURN(nel);
}

/*! \brief Return the attribute that contains the set of unique values
            in a column 
*/
NI_element * SUMA_GetUniqueValsAttr(SUMA_DSET *dset, int icol)
{
   static char FuncName[]={"SUMA_GetUniqueValsAttr"};
   char aname[256];
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY; 
   
   if (!dset || !dset->ngr) SUMA_RETURN(nel);

   sprintf(aname, "UNIQUE_VALS_%06d", icol);
   nel = SUMA_FindDsetAttributeElement (dset, aname);

   SUMA_RETURN(nel);
}

NI_element * SUMA_GetUniqueIndicesAttr(SUMA_DSET *dset, int iindex)
{
   static char FuncName[]={"SUMA_GetUniqueIndicesAttr"};
   char aname[256];
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY; 
   
   if (!dset || !dset->ngr) SUMA_RETURN(nel);

   sprintf(aname, "UNIQUE_INDICES_%06d", iindex);
   nel = SUMA_FindDsetAttributeElement (dset, aname);

   SUMA_RETURN(nel);
}

int * SUMA_GetUniqueIndicesVec(SUMA_DSET *dset, int iindex)
{
   static char FuncName[]={"SUMA_GetUniqueIndicesVec"};
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY; 
   
   if (!(nel = SUMA_GetUniqueIndicesAttr(dset, iindex))) SUMA_RETURN(NULL);

   SUMA_RETURN((int *)nel->vec[0]);
}

int * SUMA_GetDatasetDimensions(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_GetDatasetDimensions"};
   NI_element *nel=NULL;
   static int v[10][5];
   static int ncall = 0;
   
   SUMA_ENTRY;
   
   ++ncall; if (ncall > 9) ncall = 0;
   
   if (!dset) {
      v[ncall][0] = -1; SUMA_RETURN(v[ncall]);
   }
    
   if (!(nel = SUMA_FindDsetAttributeElement (dset, "DATASET_DIMENSIONS"))) {
      v[ncall][0] = SDSET_VECLEN(dset); 
      v[ncall][1] = 0; v[ncall][2] = 0; v[ncall][3] = 0; v[ncall][4] = 0;  
      SUMA_RETURN(v[ncall]);
   }
   
   SUMA_RETURN((int *)nel->vec[0]);
}

float * SUMA_GetDatasetFactors(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_GetDatasetFactors"};
   NI_element *nel=NULL;
   
   SUMA_ENTRY;
   
   
   if (!dset) {
      SUMA_RETURN(NULL);
   }
    
   if (!(nel = SUMA_FindDsetAttributeElement (dset, "BRICK_FLOAT_FACS"))) {
      SUMA_RETURN(NULL);
   }
   SUMA_RETURN((float *)nel->vec[0]);
}

float SUMA_GetBrickFactor(SUMA_DSET *dset, int ii)
{
   static char FuncName[]={"SUMA_GetBrickFactor"};
   float *vv = NULL;
   
   SUMA_ENTRY;
   
   if (!dset || ii < 0 || ii >= SDSET_VECNUM(dset)) SUMA_RETURN(0.0);
   
   if (!(vv = SUMA_GetDatasetFactors(dset))) SUMA_RETURN(0.0);
   
   SUMA_RETURN(vv[ii]);
}

MRI_TYPE SUMA_GetBrickType(SUMA_DSET *dset, int ii)
{
   static char FuncName[]={"SUMA_GetBrickType"};
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   
   SUMA_ENTRY;
   
   if (!dset || ii < 0 || ii >= SDSET_VECNUM(dset)) SUMA_RETURN(0.0);
   
   ctp = SUMA_TypeOfDsetColNumb(dset, ii); 
   vtp = SUMA_ColType2TypeCast(ctp) ;   
   switch (vtp) {
      case SUMA_short:
         SUMA_RETURN(MRI_short);
      case SUMA_byte:
         SUMA_RETURN(MRI_byte);
      case SUMA_float:
         SUMA_RETURN(MRI_float);
      case SUMA_complex:
         SUMA_RETURN(MRI_complex);
      case SUMA_double:
         SUMA_RETURN(MRI_double);
      default:
         SUMA_RETURN(-1);
   }
   
   SUMA_RETURN(-1);
}

float * SUMA_GetDatasetI2X(SUMA_DSET *dset, float M[4][4])
{
   static char FuncName[]={"SUMA_GetDatasetI2X"};
   NI_element *nel=NULL;
   float *v;
   
   SUMA_ENTRY;
   
   
   if (!dset) {
      SUMA_RETURN(NULL);
   }
    
   if (!(nel = SUMA_FindDsetAttributeElement (dset, "IJK_TO_DICOM_REAL"))) {
      SUMA_RETURN(NULL);
   }

   if ((v = (float *)nel->vec[0])) {
      V12_TO_AFF44(M, v);
   }
   SUMA_RETURN(v);
}

int SUMA_isVolDataset(SUMA_DSET *dset)
{
   int *dims = 0;
   
   if (!dset) return(0);
   if ((dims = SUMA_GetDatasetDimensions(dset))) {
      if (dims[1]>1) return(1);
   }
   return(0);
}

NI_element * SUMA_GetAtlasLabelTable(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_GetAtlasLabelTable"};
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   /* This is for volumes only */
   nel = SUMA_FindDsetAttributeElement (dset, "ATLAS_LABEL_TABLE");
   SUMA_LH("ATLAS_LABEL_TABLE  nel:%p", nel);
   SUMA_RETURN(nel);
}

NI_element * SUMA_GetValueLabelTable(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_GetValueLabelTable"};
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   /* This is for volumes only */
   nel = SUMA_FindDsetAttributeElement (dset, "VALUE_LABEL_DTABLE");
   SUMA_LH("VALUE_LABEL_DTABLE  nel:%p", nel);
   SUMA_RETURN(nel);
}


/*! \brief Add an attribute that contains the set of unique values
            in a column 
*/

SUMA_Boolean SUMA_SetUniqueValsAttr(SUMA_DSET *dset, int icol, byte replace)
{
   static char FuncName[]={"SUMA_SetUniqueValsAttr"};
   int *unq=NULL, N_unq=0, i=0;
   char aname[256];
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr) SUMA_RETURN(NOPE);
   
   sprintf(aname, "UNIQUE_VALS_%06d", icol);
   nel = SUMA_FindDsetAttributeElement (dset, aname);
   if (nel) {
      if (!replace) SUMA_RETURN(YUP);
      else {
         /* adios */
         NI_remove_from_group(dset->ngr, nel); NI_free_element(nel); nel=NULL;
      }
   }
   if (nel) {
      SUMA_S_Err("Should not be here"); 
      SUMA_RETURN(NOPE);
   }
   
   switch(SUMA_ColType2TypeCast(SUMA_TypeOfDsetColNumb(dset, icol))) {
      case  SUMA_byte: {
         byte *bunq=NULL;
         if (!(bunq =  UniqueByte ( SDSET_VEC(dset, icol), SDSET_VECLEN(dset), 
                                       &N_unq, 0))) {
            SUMA_S_Err("Failed to get unique values");
            SUMA_RETURN(NOPE);
         }
         unq = (int *)SUMA_malloc(N_unq*sizeof(int));
         for (i=0; i<N_unq; ++i) unq[i] = bunq[i];
         SUMA_ifree(bunq);
         break; }
      case SUMA_int: 
         if (!(unq =  UniqueInt ( SDSET_VEC(dset, icol), SDSET_VECLEN(dset), 
                                       &N_unq, 0))) {
            SUMA_S_Err("Failed to get unique values");
            SUMA_RETURN(NOPE);
         }
         break;
      case SUMA_short: {
         short *sunq=NULL;
         if (!(sunq =  UniqueShort ( SDSET_VEC(dset, icol), SDSET_VECLEN(dset), 
                                     &N_unq, 0))) {
            SUMA_S_Err("Failed to get unique values");
            SUMA_RETURN(NOPE);
         }
         unq = (int *)SUMA_malloc(N_unq*sizeof(int));
         for (i=0; i<N_unq; ++i) unq[i] = sunq[i];
         SUMA_ifree(sunq);
         break; }
      default:
         SUMA_S_Err("Bad column type %d for unique values",
                  SUMA_TypeOfDsetColNumb(dset, icol));
         SUMA_RETURN(NOPE);
         break;
   }
   nel = NI_new_data_element("AFNI_atr", N_unq);
   NI_set_attribute(nel, "atr_name", aname);
   NI_add_column_stride(nel, NI_INT, (void *)unq, 1);
   free(unq); unq=NULL;
   
   NI_add_to_group(dset->ngr, nel);
   
   SUMA_RETURN(YUP);
}            

SUMA_Boolean SUMA_SetUniqueIndicesAttr(SUMA_DSET *dset, byte replace)
{
   static char FuncName[]={"SUMA_SetUniqueIndicesAttr"};
   int *unq=NULL, N_unq=0, icol=0;
   char aname[256];
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr || !dset->inel) SUMA_RETURN(NOPE);

   for (icol = 0; icol < dset->inel->vec_num; ++icol) {
      SUMA_LHv("Working col %d\n", icol);
      sprintf(aname, "UNIQUE_INDICES_%06d", icol);
      nel = SUMA_FindDsetAttributeElement (dset, aname);
      if (nel) {
         if (!replace) SUMA_RETURN(YUP);
         else {
            /* adios */
            NI_remove_from_group(dset->ngr, nel); NI_free_element(nel); nel=NULL;
         }
      }
      if (nel) {
         SUMA_S_Err("Should not be here"); 
         SUMA_RETURN(NOPE);
      }
      if (!(unq =  UniqueInt ( dset->inel->vec[icol], dset->inel->vec_len, 
                                    &N_unq, 0))) {
         SUMA_S_Err("Failed to get unique values");
         SUMA_RETURN(NOPE);
      }
      
      SUMA_LHv("%d unique values for col %d\n", N_unq, icol);
      nel = NI_new_data_element("AFNI_atr", N_unq);
      NI_set_attribute(nel, "atr_name", aname);
      NI_add_column_stride(nel, NI_INT, (void *)unq, 1);
      free(unq); unq=NULL;
   
      NI_add_to_group(dset->ngr, nel);
   }
   SUMA_RETURN(YUP);
}            

/*!
   Add an attribute element to a data set, to be called after adding a column to the data element
   The old version of this is SUMA_AddColAttr
   \sa SUMA_AddDsetNodeIndexColAttr for special SUMA_NODE_INDEX ctp
*/
int SUMA_AddDsetColAttr (  SUMA_DSET *dset, char *col_label, 
                           SUMA_COL_TYPE ctp, void *col_attr, 
                           int col_index, int insert_mode)
{
   static char FuncName[]={"SUMA_AddDsetColAttr"};
   NI_element *nelb = NULL;
   char Name[500], Attr[500], *attrstr=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (SUMA_IS_DATUM_INDEX_COL(ctp) || SUMA_IS_MD_DATUM_INDEX_COL(ctp)) {
      SUMA_RETURN(
         SUMA_AddDsetNodeIndexColAttr (dset, col_label, ctp, col_attr));
   }
   
   if (SUMA_isGraphDset(dset) && SUMA_IS_GNODE_IXYZ_COL(ctp)) {
      SUMA_RETURN(SUMA_AddGDsetNodeXYZColAttr (dset, col_label, ctp, col_attr));
   }
   
   if (!dset) SUMA_RETURN(0);
   if (col_index < 0) col_index = SDSET_VECNUM(dset)-1;
   if (col_index < 0 || !SDSET_VECNUM(dset) ) { 
      SUMA_SL_Err("No columns in data set's data element!"); SUMA_RETURN(0); }
   if (SDSET_VECNUM(dset) <= col_index) { 
      SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   
   /* has the column label element been added yet ?*/
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_LABS");
   if (!nelb) { /* need to form this element */
      nelb = NI_new_data_element("AFNI_atr", 1);
      NI_set_attribute(nelb,"atr_name", "COLMS_LABS");
      NI_add_column_stride ( nelb, NI_STRING, NULL, 1 );
      NI_add_to_group(dset->ngr, nelb);
   } 
   SUMA_AddColAtt_CompString( nelb, col_index, col_label, 
                              SUMA_NI_CSS, insert_mode);

   /* has the column type element been added yet ? */
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_TYPE");
   if (!nelb) { /* need to form this element */
      nelb = NI_new_data_element("AFNI_atr", 1);
      NI_set_attribute(nelb,"atr_name", "COLMS_TYPE");
      NI_add_column_stride ( nelb, NI_STRING, NULL, 1 );
      NI_add_to_group(dset->ngr, nelb);
   } 
   /* set the type */
   SUMA_AddColAtt_CompString( nelb, col_index, SUMA_Col_Type_Name(ctp), 
                              SUMA_NI_CSS, insert_mode);
   
   /* set the Col_STATSYM 
      (SPECIAL CASE: USE THE ; rather than the ~ . The story is complicated) */
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_STATSYM");
   if (!nelb) { /* need to form this element */
      nelb = NI_new_data_element("AFNI_atr", 1);
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
            SUMA_LH("%s",
               NI_stat_encode(NI_STAT_CORREL, pars[0], pars[1], pars[2]));
            attrstr = SUMA_copy_string( 
                        NI_stat_encode(NI_STAT_CORREL, 
                                       pars[0], pars[1], pars[2]));
         } else {
            attrstr = SUMA_copy_string("none");
         }
         break;
      
      case SUMA_NODE_ZSCORE:
         attrstr = SUMA_copy_string( 
                        NI_stat_encode(NI_STAT_ZSCORE, 0.0 , 0.0, 0.0));  

         break;
   }
   
   SUMA_AddColAtt_CompString( nelb, col_index, attrstr, 
                              SUMA_NI_CSS, insert_mode); 
            /* See confusing note following AFNI_NI_CSS in SUMA_DataSets.h */
   if (attrstr) SUMA_free(attrstr); attrstr = NULL;
   
   SUMA_RETURN(1);   
}

/*!
   \brief a special version of SUMA_AddDsetColAttr for node index column
*/
int SUMA_AddDsetNodeIndexColAttr (SUMA_DSET *dset, char *col_label, 
                                  SUMA_COL_TYPE ctp, void *col_attr)
{
   static char FuncName[]={"SUMA_AddDsetNodeIndexColAttr"};
   NI_element *nelb = NULL;
   char *attrstr=NULL, *stmp=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMA_IS_DATUM_INDEX_COL(ctp) && !SUMA_IS_MD_DATUM_INDEX_COL(ctp)) {
      SUMA_S_Err("Don't call me like that");
      SUMA_RETURN(0); 
   }
   
   if (!dset || !dset->inel || !SDSET_NODEINDLEN(dset)) SUMA_RETURN(0);
   
   stmp = SUMA_copy_string(NI_get_attribute(dset->inel,"COLMS_LABS"));
   if (!SUMA_Set_Sub_String(&stmp, SUMA_NI_CSS, 
                            SUMA_DATUM_INDEX_CTP2COL(ctp), col_label)) {
      SUMA_S_Warnv("Failed to set substring for COLMS_LABS at %d\n", 
                   SUMA_DATUM_INDEX_CTP2COL(ctp)); 
   }
   if (stmp) {
      NI_set_attribute(dset->inel, "COLMS_LABS", stmp); 
      SUMA_free(stmp); stmp=NULL;
   }
   
   stmp = SUMA_copy_string(NI_get_attribute(dset->inel,"COLMS_TYPE"));
   if (!SUMA_Set_Sub_String(&stmp, SUMA_NI_CSS, SUMA_DATUM_INDEX_CTP2COL(ctp), 
                            SUMA_Col_Type_Name(ctp))) {
      SUMA_S_Warnv("Failed to set substring for COLMS_TYPE at %d\n", 
                   SUMA_DATUM_INDEX_CTP2COL(ctp)); 
   }
   if (stmp) {
      NI_set_attribute(dset->inel, "COLMS_TYPE", stmp); 
      SUMA_free(stmp); stmp=NULL;
   }
      
   SUMA_RETURN(1);   
}

int SUMA_AddGDsetNodeXYZColAttr (SUMA_DSET *dset, char *col_label, 
                                SUMA_COL_TYPE ctp, void *col_attr)
{
   static char FuncName[]={"SUMA_AddGDsetNodeXYZColAttr"};
   NI_element *xyznel = NULL;
   char *attrstr=NULL, *stmp=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMA_IS_GNODE_IXYZ_COL(ctp)) {
      SUMA_S_Err("Don't call me like that");
      SUMA_RETURN(0); 
   }
   
   if (!dset || !SDSET_NODEINDLEN(dset)) SUMA_RETURN(0);
   
   if (!SUMA_isGraphDset(dset)) {
      SUMA_SL_Err("Null or bad input"); 
      SUMA_RETURN(0); 
   }
   
   if (!(xyznel = SUMA_FindGDsetNodeListElement(dset))) {
      SUMA_S_Err("No nodelist element");
      SUMA_RETURN(0); 
   }
   
   stmp = SUMA_copy_string(NI_get_attribute(xyznel,"COLMS_LABS"));
   if (!SUMA_Set_Sub_String(&stmp, SUMA_NI_CSS, 
                            SUMA_GNODE_IXYZ_CTP2COL(ctp), col_label)) {
      SUMA_S_Warnv("Failed to set substring for COLMS_LABS at %d\n", 
                   SUMA_IS_GNODE_IXYZ_COL(ctp)); 
   }
   if (stmp) {
      NI_set_attribute(xyznel, "COLMS_LABS", stmp); 
      SUMA_free(stmp); stmp=NULL;
   }
   
   stmp = SUMA_copy_string(NI_get_attribute(xyznel,"COLMS_TYPE"));
   if (!SUMA_Set_Sub_String(&stmp, SUMA_NI_CSS, SUMA_GNODE_IXYZ_CTP2COL(ctp), 
                            SUMA_Col_Type_Name(ctp))) {
      SUMA_S_Warnv("Failed to set substring for COLMS_TYPE at %d\n", 
                   SUMA_IS_GNODE_IXYZ_COL(ctp)); 
   }
   if (stmp) {
      NI_set_attribute(xyznel, "COLMS_TYPE", stmp); 
      SUMA_free(stmp); stmp=NULL;
   }
      
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
int SUMA_AddColAttr (NI_element *nel, char *col_label, SUMA_COL_TYPE ctp, 
                     void *col_attr, int col_index)
{
   static char FuncName[]={"SUMA_AddColAttr"};
   char Name[500], Attr[500];
   
   SUMA_ENTRY;
   
   if (!SUMA_ALLOW_NEL_USE) SUMA_SL_Warn("Obsolete, use new version.");

   if (!nel) SUMA_RETURN(0);
   if (col_index < 0) col_index = nel->vec_num-1;
   if (col_index < 0 || !nel->vec_num ) { 
      SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); 
   }
   if (nel->vec_num <= col_index) { 
      SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); 
   }
   
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
         
      case SUMA_NODE_COMPLEX:
         NI_set_attribute ( nel, Attr, NULL);
         break;
         
      case SUMA_NODE_INDEX:
         /* form the string of attributes for this column */
         NI_set_attribute ( nel, Attr, NULL);
         break;
     
      case SUMA_MD_NODE_INDEX:
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
      
      case SUMA_GNODE_IGROUP:
         NI_set_attribute ( nel, Attr, NULL);
         break;   
      
      case SUMA_NODE_SLABEL:
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
      
      case SUMA_NODE_VFR:
         NI_set_attribute ( nel, Attr, NULL);
         break;  
      
      case SUMA_NODE_PHASE:
         NI_set_attribute ( nel, Attr, NULL);
         break;  
      
      case SUMA_NODE_AREA:
         NI_set_attribute ( nel, Attr, NULL);
         break;  
      
      case SUMA_NODE_VOLUME:
         NI_set_attribute ( nel, Attr, NULL);
         break;  

      case SUMA_NODE_THICKNESS:
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
   if col_index is -1, then it is assumed that the attributes are 
      for the latest column added (vec_num -1)
   \sa SUMA_AddGenDsetNodeIndexColAttr for the special case of the 
      node index column
*/
int SUMA_AddGenDsetColAttr (  SUMA_DSET *dset, SUMA_COL_TYPE ctp, 
                              void *col, int stride, int col_index, 
                              int insert_mode) 
{
   static char FuncName[]={"SUMA_AddGenDsetColAttr"};
   char **junk, *stmp, *curstring = NULL;
   NI_element *nelb = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (SUMA_IS_DATUM_INDEX_COL(ctp) || SUMA_IS_MD_DATUM_INDEX_COL(ctp)) {
      SUMA_RETURN(SUMA_AddGenDsetNodeIndexColAttr (dset, ctp, col, stride) );
   }
   
   if (SUMA_isGraphDset(dset) && SUMA_IS_GNODE_IXYZ_COL(ctp)) {
      SUMA_RETURN(SUMA_AddGenGDsetNodeXYZColAttr (dset, ctp, col, stride));
   }
   
   if (!dset || !dset->dnel) { SUMA_SL_Err("Null input"); SUMA_RETURN(0); } 
   if (col_index < 0) col_index = SDSET_VECNUM(dset)-1;
   if (col_index < 0 || !SDSET_VECNUM(dset) ) { 
      SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); }
   if (SDSET_VECNUM(dset) <= col_index) { 
      SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   
   /* does the range attribute element exist ? */
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_RANGE");
   if (!nelb) { /* need to form this element */
      SUMA_LH("Need to create ranges element");
      nelb = NI_new_data_element("AFNI_atr", 1); /* one long string */
      NI_set_attribute(nelb, "atr_name", "COLMS_RANGE");
      NI_add_to_group(dset->ngr, nelb);
      #if 0 /* trying to work with NI_insert */
      { int i ; junk = (char **)SUMA_calloc(300,sizeof(char*)); 
         for (i=0; i< 300; ++i) 
            junk[i] = (char *)SUMA_calloc(300, sizeof(char));
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
      if (!(stmp = SUMA_CreateDsetColRangeCompString(dset, col_index, ctp))) {
         SUMA_S_Err("Failed to calculate range");
         SUMA_RETURN(0);
      }
   }  
   
   
   SUMA_AddColAtt_CompString(nelb, col_index, stmp, SUMA_NI_CSS, insert_mode);
   if (LocalHead) SUMA_ShowNel((void*)nelb);
   SUMA_free(stmp); stmp = NULL;
   SUMA_RETURN(1);  
}

int SUMA_UpdateDsetColRange(SUMA_DSET *dset, int icol) 
{
   static char FuncName[]={"SUMA_UpdateDsetColRange"};
   int ic=0, istrt=0, iend=0;
   char *sbuf=NULL;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(0);
   if (icol < 0) { istrt=0; iend=SDSET_VECNUM(dset); }
   else { istrt = icol; iend=icol+1; }
   if (istrt < 0 || istrt > SDSET_VECNUM(dset)) SUMA_RETURN(0);
   if (iend  < 0 || iend > SDSET_VECNUM(dset)) SUMA_RETURN(0);
   
   for (ic=istrt; ic<iend; ++ic) {
      if(!(sbuf = SUMA_CreateDsetColRangeCompString(dset, ic, 
                              SUMA_TypeOfDsetColNumb(dset,ic)))) {
         SUMA_S_Err("Failed to calculate range");
         SUMA_RETURN(0);
      }else {
         NI_element *nelb = 
            SUMA_FindDsetAttributeElement(dset, "COLMS_RANGE");
         SUMA_LH("%s", sbuf);
         SUMA_AddColAtt_CompString(nelb, ic, sbuf, SUMA_NI_CSS,0);
         SUMA_free(sbuf); sbuf=NULL;
      }
   }
   SUMA_RETURN(1);
}

int SUMA_UpdateDsetColLabel(SUMA_DSET *dset, int icol, char *label) 
{
   static char FuncName[]={"SUMA_UpdateDsetColLabel"};
   int ic=0, istrt=0, iend=0;
   char *sbuf=NULL;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
      
   if (!dset || !label) SUMA_RETURN(0);
   if (icol < 0) { istrt=0; iend=SDSET_VECNUM(dset); }
   else { istrt = icol; iend=icol+1; }
   if (istrt < 0 || istrt > SDSET_VECNUM(dset)) SUMA_RETURN(0);
   if (iend  < 0 || iend > SDSET_VECNUM(dset)) SUMA_RETURN(0);
   
   for (ic=istrt; ic<iend; ++ic) {
      {
         NI_element *nelb = 
            SUMA_FindDsetAttributeElement(dset, "COLMS_LABS");
         SUMA_LH("%d: %s", ic, label);
         SUMA_AddColAtt_CompString(nelb, ic, label, SUMA_NI_CSS,0);
      }
   }
   SUMA_RETURN(1);
}

/*!
   \brief A special case of  SUMA_AddGenDsetColAttr for node indices
   
   Now OK with Graph Dsets
*/
int SUMA_AddGenDsetNodeIndexColAttr (SUMA_DSET *dset, SUMA_COL_TYPE ctp, 
                                     void *col, int stride) 
{
   static char FuncName[]={"SUMA_AddGenDsetNodeIndexColAttr"};
   char Name[500], **junk, *stmp=NULL;
   float amin = 0.0, amax = 0.0, *fv;
   int aminloc = -1, amaxloc = -1, *iv, icol=0, N_col;
   byte *bv;
   complex *cv = NULL;
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
      if (SUMA_isGraphDset(dset)) {
         snprintf(Name, 500*sizeof(char),"0 0 -1 -1%s0 0 -1 -1%s0 0 -1 -1", 
                  SUMA_NI_CSS, SUMA_NI_CSS);
         stmp = SUMA_copy_string(Name);
      } if (SUMA_isCIFTIDset(dset)) {
         SUMA_S_Err("This needs to be done per domain also, "
                    "so we're not there yet. ");
         SUMA_RETURN(0);
      } else {
         stmp = SUMA_copy_string("0 0 -1 -1");
      }
   } else { 
      if (LocalHead && SUMA_isCIFTIDset(dset)) {
         SUMA_S_Warn("Caution, needs verification for ciftination");
      }
      SUMA_LH("Calculating indrange");
      icol = SUMA_DATUM_INDEX_CTP2COL(ctp);
      N_col = stride*SDSET_VECFILLED(dset);
      stmp = SUMA_copy_string(NI_get_attribute(dset->inel,"COLMS_RANGE"));
      switch (SUMA_ColType2TypeCast(ctp)) {
         case SUMA_int:           
            iv = (int *)col;
            SUMA_MIN_MAX_VEC_STRIDE(iv, N_col, amin, amax, 
                                    aminloc, amaxloc, stride);
            snprintf(Name, 500*sizeof(char),"%d %d %d %d", 
                     (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_float:
            fv = (float *)col;
            SUMA_MIN_MAX_VEC_STRIDE(fv , N_col, amin, amax, 
                                    aminloc, amaxloc, stride);
            snprintf(Name, 500*sizeof(char),"%f %f %d %d", 
                           amin, amax, aminloc, amaxloc);
            break;
         case SUMA_byte:
            bv = (byte *)col;
            SUMA_MIN_MAX_VEC_STRIDE(bv , N_col, amin, amax, 
                                    aminloc, amaxloc, stride);
            snprintf(Name, 500*sizeof(char),"%d %d %d %d", 
                           (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_string:
            Name[0] = '\0';
            break;
         case SUMA_complex:
            cv = (complex *)col;
            SUMA_MIN_MAX_CVEC_STRIDE(cv , N_col, amin, amax, 
                                    aminloc, amaxloc, stride, 0);
            snprintf(Name, 500*sizeof(char),"%d %d %d %d", 
                           (int)amin, (int)amax, aminloc, amaxloc);
            break;
         default:
            fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
            SUMA_RETURN(0);
            break; 
      }
      if (!SUMA_Set_Sub_String(&stmp, SUMA_NI_CSS, icol,Name)) {
         fprintf (stderr,"Error %s: Failed to set substring to %s\n", 
                        FuncName, Name);
         SUMA_RETURN(0);
      }

   }
   
   NI_set_attribute(dset->inel, "COLMS_RANGE", stmp);
   
   if (stmp) SUMA_free(stmp); stmp = NULL;
   SUMA_RETURN(1);  
}

int SUMA_AddGenGDsetNodeXYZColAttr (SUMA_DSET *dset, SUMA_COL_TYPE ctp, 
                                     void *col, int stride) 
{
   static char FuncName[]={"SUMA_AddGenGDsetNodeXYZColAttr"};
   char Name[500], **junk, *stmp=NULL;
   float amin = 0.0, amax = 0.0, *fv;
   int aminloc = -1, amaxloc = -1, *iv, icol=0, N_col=0;
   byte *bv;
   complex *cv = NULL;
   NI_element *xyznel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!dset || SDSET_NODEINDLEN(dset) < 1) { 
      SUMA_SL_Err("Null input"); 
      SUMA_ShowNel(dset->inel); 
      SUMA_DUMP_TRACE("Bad dset->inel, dumping trace for debug:"); 
      SUMA_RETURN(0); 
   } 

   if (!SUMA_isGraphDset(dset)) {
      SUMA_SL_Err("Null or bad input"); 
      SUMA_RETURN(0); 
   }
   
   if (!(xyznel = SUMA_FindGDsetNodeListElement(dset))) {
      SUMA_S_Err("No nodelist element");
      SUMA_RETURN(0); 
   }
   
   if (dset->Aux->matrix_shape == MAT_HEEHAW) {
      SUMA_GDSET_Set_Aux_matrix_shape(dset);
   }

   if (!col) { 
      SUMA_LH("No data");
      /* Do not complain, that is not a bad thing.
      People can use this to allocate for a column
      without filling it up */
      if (SUMA_isGraphDset(dset)) {
         snprintf(Name, 500*sizeof(char),
                  "0 0 -1 -1%s0 0 -1 -1%s0 0 -1 -1", SUMA_NI_CSS, SUMA_NI_CSS);
         stmp = SUMA_copy_string(Name);
      } else {
         stmp = SUMA_copy_string("0 0 -1 -1");
      }
   } else { 
      SUMA_LH("Calculating XYZrange");
      icol = SUMA_GNODE_IXYZ_CTP2COL(ctp);
      N_col = GDSET_N_SEG_POINTS(dset)*stride;
      stmp = SUMA_copy_string(NI_get_attribute(xyznel,"COLMS_RANGE"));
      switch (SUMA_ColType2TypeCast(ctp)) {
         case SUMA_int:           
            iv = (int *)col;
            SUMA_MIN_MAX_VEC_STRIDE(iv ,N_col, amin, amax, 
                                    aminloc, amaxloc, stride);
            snprintf(Name, 500*sizeof(char),"%d %d %d %d", 
                     (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_float:
            fv = (float *)col;
            SUMA_MIN_MAX_VEC_STRIDE(fv ,N_col, amin, amax, 
                                    aminloc, amaxloc, stride);
            snprintf(Name, 500*sizeof(char),"%f %f %d %d", 
                           amin, amax, aminloc, amaxloc);
            break;
         case SUMA_byte:
            bv = (byte *)col;
            SUMA_MIN_MAX_VEC_STRIDE(bv ,N_col, amin, amax, 
                                    aminloc, amaxloc, stride);
            snprintf(Name, 500*sizeof(char),"%d %d %d %d", 
                           (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_string:
            Name[0] = '\0';
            break;
         case SUMA_complex:
            cv = (complex *)col;
            SUMA_MIN_MAX_CVEC_STRIDE(cv ,N_col, amin, amax, 
                                    aminloc, amaxloc, stride, 0);
            snprintf(Name, 500*sizeof(char),"%d %d %d %d", 
                           (int)amin, (int)amax, aminloc, amaxloc);
            break;
         default:
            fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
            SUMA_RETURN(0);
            break; 
      }
      if (!SUMA_Set_Sub_String(&stmp, SUMA_NI_CSS, icol,Name)) {
         fprintf (stderr,"Error %s: Failed to set substring to %s\n", 
            FuncName, Name);
         SUMA_RETURN(0);
      }

   }
   
   NI_set_attribute(xyznel, "COLMS_RANGE", stmp);
   
   if (stmp) SUMA_free(stmp); stmp = NULL;
   SUMA_RETURN(1);  
}



/*!
   Adds some generic attributes.
   For the moment, the range is added for numeric columns 
   if col_index is -1, then it is assumed that the attributes are for the latest     column added (vec_num -1)
*/
int SUMA_AddGenColAttr (NI_element *nel, SUMA_COL_TYPE ctp, void *col, 
                        int stride, int col_index) 
{
   static char FuncName[]={"SUMA_AddGenColAttr"};
   static char stmp[500], Name[500];
   float amin = 0.0, amax = 0.0, *fv;
   int aminloc = -1, amaxloc = -1, *iv, N_col=0;
   complex *cv = NULL;
   byte *bv;

   SUMA_ENTRY;
   
   if (!SUMA_ALLOW_NEL_USE) SUMA_SL_Warn("Obsolete, use new version.");

   if (!nel) { SUMA_SL_Err("Null Nel"); SUMA_RETURN(0); }
   if (col_index < 0) col_index = nel->vec_num-1;
   if (col_index < 0 || !nel->vec_num ) { 
      SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); }
   if (nel->vec_num <= col_index) { 
      SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   
   sprintf(Name, "RangeCol_%d", col_index);

   if (!col) { 
      /* Do not complain, that is not a bad thing.
         People can use this to allocate for a column
         without filling it up */
      sprintf(stmp, "0 0 -1 -1");
   } else {
      N_col = stride*nel->vec_filled;
      switch (SUMA_ColType2TypeCast(ctp)) {
         case SUMA_int:
            iv = (int *)col;
            SUMA_MIN_MAX_VEC_STRIDE(iv ,N_col, amin, amax, 
                                    aminloc, amaxloc, stride);
            snprintf(stmp, 500*sizeof(char),"%d %d %d %d", 
                           (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_float:
            fv = (float *)col;
            SUMA_MIN_MAX_VEC_STRIDE(fv ,N_col, amin, amax, 
                                    aminloc, amaxloc, stride);
            snprintf(stmp, 500*sizeof(char),"%f %f %d %d", 
                           amin, amax, aminloc, amaxloc);
            break;
         case SUMA_byte:
            bv = (byte *)col;
            SUMA_MIN_MAX_VEC_STRIDE(bv ,N_col, amin, amax, 
                                    aminloc, amaxloc, stride);
            snprintf(stmp, 500*sizeof(char),"%d %d %d %d", 
                           (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_string:
            stmp[0] = '\0';
            break;
         case SUMA_complex:
            cv = (complex *)col;
            SUMA_MIN_MAX_CVEC_STRIDE(cv ,N_col, amin, amax, 
                                    aminloc, amaxloc, stride, 0);
            snprintf(stmp, 500*sizeof(char),"%d %d %d %d", 
                           (int)amin, (int)amax, aminloc, amaxloc);
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
int SUMA_GetDsetColRange(  SUMA_DSET *dset, int col_index, 
                           double range[2], int loc[2])
{
   static char FuncName[]={"SUMA_GetDsetColRange"};
   char *rs = NULL, **sc=NULL, Name[500];
   double nums[4];
   NI_element *nelb = NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->dnel) { SUMA_SL_Err("Null input"); SUMA_RETURN(0); }
   if (col_index < 0) col_index = SDSET_VECNUM(dset)-1;
   if (col_index < 0 || !SDSET_VECNUM(dset) ) { 
      SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); }
   if (SDSET_VECNUM(dset) <= col_index) { 
      SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_RANGE");
   if (!nelb) { 
      SUMA_SL_Err("Failed to find column range attribute"); SUMA_RETURN(0); }
   
   SUMA_NEL_GET_STRING(nelb, 0, 0, rs); 
         /* rs is a pointer copy here, do not free */
   rs = SUMA_Get_Sub_String(rs, SUMA_NI_CSS, col_index);
   if (!rs) { SUMA_SL_Err("No range field."); SUMA_RETURN(0); }
   if (SUMA_StringToNum(rs, (void *)nums, 4, 2) != 4) { 
      SUMA_SL_Err("Failed to read 4 nums from range."); SUMA_RETURN(0); }
   range[0] = nums[0]; range[1] = nums[1]; 
   loc[0] = (int)nums[2]; loc[1] = (int)nums[3];
   SUMA_free(rs); rs = NULL;   
   SUMA_RETURN(1);
}

char * SUMA_CreateDsetColRangeCompString( SUMA_DSET *dset, int col_index, 
                                          SUMA_COL_TYPE ctp)
{
   static char FuncName[]={"SUMA_CreateDsetColRangeCompString"};
   char Name[500]={""};
   char *stmp=NULL;
   float *fv=NULL;
   double *dv = NULL, amin = 0.0, amax = 0.0;
   complex *cv = NULL;
   int aminloc = -1, amaxloc = -1, *iv, kk;
   byte *bv;
   NI_element *nelb = NULL;
   void *col=NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->dnel) { SUMA_SL_Err("Null input"); SUMA_RETURN(0); }
   if (col_index < 0) col_index = SDSET_VECNUM(dset)-1;
   if (col_index < 0 || !SDSET_VECNUM(dset) ) { 
      SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); }
   if (SDSET_VECNUM(dset) <= col_index) { 
      SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   if (ctp <= SUMA_NO_COL_TYPE) {
      /* sometimes this function is called after a dset is created 
      and the type is set already in dset but the user does not want to 
      bother sending the type in. Since dset is fully
      formed by then, ctp can be recovered from dset */
      if (  (ctp = SUMA_TypeOfDsetColNumb(dset, col_index)) == 
            SUMA_ERROR_COL_TYPE ){
         SUMA_SL_Err("ctp not set and cannot be obtained.\n"
                     "You must specify column type if dset\n"
                     "not fully formed yet"); SUMA_RETURN(0);
      }
   }
   col = dset->dnel->vec[col_index];
   
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_RANGE");
   if (!nelb) { 
      SUMA_SL_Err("Failed to find column range attribute"); SUMA_RETURN(0); }
      
      switch (SUMA_ColType2TypeCast(ctp)) {
         case SUMA_int:
            iv = (int *)col;
            SUMA_MIN_MAX_VEC_STRIDE(iv ,SDSET_VECFILLED(dset), amin, amax, 
                                    aminloc, amaxloc, 1);
            snprintf(Name, 500*sizeof(char),
                     "%d %d %d %d", (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_float:
            fv = (float *)col;
            SUMA_MIN_MAX_VEC_STRIDE(fv ,SDSET_VECFILLED(dset), amin, amax, 
                                    aminloc, amaxloc, 1);
            snprintf(Name, 500*sizeof(char),
                           "%f %f %d %d", amin, amax, aminloc, amaxloc);
            break;
         case SUMA_byte:
            bv = (byte *)col;
            SUMA_MIN_MAX_VEC_STRIDE(bv ,SDSET_VECFILLED(dset), amin, amax, 
                                    aminloc, amaxloc, 1);
            snprintf(Name, 500*sizeof(char),
                           "%d %d %d %d", 
                           (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_double:
            dv = (double *)col;
            SUMA_MIN_MAX_VEC_STRIDE(dv ,SDSET_VECFILLED(dset), amin, amax, 
                                    aminloc, amaxloc, 1);
            snprintf(Name, 500*sizeof(char),
                           "%f %f %d %d", amin, amax, aminloc, amaxloc);
            break;
         case SUMA_string:
            snprintf(Name, 500*sizeof(char),
                           "0 0 -1 -1");
            break;
         case SUMA_complex:
            cv = (complex *)col;
            SUMA_MIN_MAX_CVEC_STRIDE(cv ,SDSET_VECFILLED(dset), amin, amax, 
                                    aminloc, amaxloc, 1, 0);
            snprintf(Name, 500*sizeof(char),
                           "%f %f %d %d", amin, amax, aminloc, amaxloc);
            break;
         default:
            fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
            SUMA_RETURN(0);
            break; 
      }
      stmp = SUMA_copy_string(Name);
      
   SUMA_RETURN(stmp);
}

/*!
   Upon returning, sets the statistical parameters from a
   certain dset column
*/
int SUMA_GetDsetColStatAttr(  SUMA_DSET *dset, int col_index, 
                              int *statcode,
                              float *p1, float *p2, float *p3)
{
   static char FuncName[]={"SUMA_GetDsetColStatAttr"};
   char *rs = NULL, **sc=NULL, Name[500];
   float nums[4];
   NI_element *nelb = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   *statcode = -1;
   *p1 = *p2 = *p3 = -1.0;
   
   if (!dset || !dset->dnel) { SUMA_SL_Err("Null input"); SUMA_RETURN(0); }
   if (col_index < 0) col_index = SDSET_VECNUM(dset)-1;
   if (col_index < 0 || !SDSET_VECNUM(dset) ) { 
      SUMA_SL_Err("No columns in data set!"); 
      SUMA_RETURN(0); 
   }
   if (SDSET_VECNUM(dset) <= col_index) { 
      SUMA_SL_Err("col_index >= nel->vec_num!"); 
      SUMA_RETURN(0); 
   }
   
   nelb = SUMA_FindDsetAttributeElement(dset, "COLMS_STATSYM");
   if (!nelb) { 
      if (LocalHead) {
         SUMA_S_Warn("Failed to find column range attribute"); 
      }
      SUMA_RETURN(0); 
   }
   
   SUMA_NEL_GET_STRING(nelb, 0, 0, rs); /* rs is a pointer copy here */
   
   rs = SUMA_Get_Sub_String(rs, SUMA_NI_CSS, col_index);
   if (!rs) { SUMA_SL_Err("No stat field."); SUMA_RETURN(0); }
   NI_stat_decode(rs, statcode, p1, p2, p3);
   SUMA_free(rs); rs = NULL;   
   
   SUMA_RETURN(1);
}

char * SUMA_GetDsetColStringAttr( SUMA_DSET *dset, int col_index, 
                                    char *attrname)
{
   static char FuncName[]={"SUMA_GetDsetColStringAttr"};
   char *rs = NULL;
   NI_element *nelb = NULL;
   SUMA_ENTRY;
   
   if (!dset || !dset->dnel) { SUMA_SL_Err("Null input"); SUMA_RETURN(NULL); }
   if (col_index < 0) col_index = SDSET_VECNUM(dset)-1;
   if (col_index < 0 || !SDSET_VECNUM(dset) ) { 
      SUMA_SL_Err("No columns in data set!"); 
      SUMA_RETURN(NULL); 
   }
   if (SDSET_VECNUM(dset) <= col_index) { 
      SUMA_SL_Err("col_index >= nel->vec_num!"); 
      SUMA_RETURN(NULL); 
   }
   
   nelb = SUMA_FindDsetAttributeElement(dset, attrname);
   if (!nelb) { 
      SUMA_SL_Err("Failed to find  attribute"); 
      SUMA_RETURN(NULL); 
   }
   SUMA_NEL_GET_STRING(nelb, 0, 0, rs); /* rs is a pointer copy here */
   
   rs = SUMA_Get_Sub_String(rs, SUMA_NI_CSS, col_index);
   
   SUMA_RETURN(rs);
}

char * SUMA_GetNgrColStringAttr( NI_group *ngr, int col_index, 
                                 char *attrname)
{
   static char FuncName[]={"SUMA_GetNgrColStringAttr"};
   char *rs = NULL;
   NI_element *nelb = NULL;
   SUMA_ENTRY;
   
   if (!ngr) { SUMA_SL_Err("Null input"); SUMA_RETURN(NULL); }
   if (col_index < 0 ) { 
      SUMA_SL_Err("Bad Col Index"); 
      SUMA_RETURN(NULL); 
   }
   
   nelb = SUMA_FindNgrAttributeElement(ngr, attrname);
   if (!nelb) { 
      SUMA_SL_Err("Failed to find  attribute"); 
      SUMA_RETURN(NULL); 
   }
   SUMA_NEL_GET_STRING(nelb, 0, 0, rs); /* rs is a pointer copy here */
   
   rs = SUMA_Get_Sub_String(rs, SUMA_NI_CSS, col_index);
   
   SUMA_RETURN(rs);
}

int SUMA_GetDsetNodeIndexColRange_eng(  SUMA_DSET *dset, 
                                    double range[2], int loc[2], 
                                    int addifmissing, int ii)
{
   static char FuncName[]={"SUMA_GetDsetNodeIndexColRange_eng"};
   char *rs = NULL, Name[500];
   double nums[4];
   char *ctmp=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   range[0] = 0; range[1] = 0; 
   loc[0] = -1; loc[1] = -1;
   
   if (!dset || !dset->inel) { SUMA_SL_Err("Null input"); SUMA_RETURN(0); }
   if (SUMA_isCIFTIDset(dset)) {
      SUMA_SL_Err("Cannot answer this question without adding domain as"
                  "an extra parameter");
      SUMA_RETURN(0);
   }
   if (ii != 0) {
      if (!SUMA_isGraphDset(dset)) {
         SUMA_S_Errv("Should not ask for %d unless with graph dset",ii);
         SUMA_RETURN(0);
      }
      if (ii>2 || ii<0) {
         SUMA_S_Errv("What is %d for?",ii);
         SUMA_RETURN(0);
      }
   }
   
   if (SUMA_isGraphDset(dset) && dset->inel->vec_num == 0) {
      SUMA_LH("Here is hoping we're standard");
      switch (dset->Aux->matrix_shape) {
         case MAT_HEEHAW:
         default:
            SUMA_S_Err("Should not happen here");
            SUMA_RETURN(NOPE);
         case MAT_TRI:
         case MAT_TRI_DIAG:
         case MAT_FULL:
            if (ii==0) {
               range[0]=loc[0]=0; range[1]=loc[1]=SDSET_VECLEN(dset)-1;
            } else if (ii==1 || ii==2) {
               range[0]=loc[0]=0; range[1]=loc[1]=SDSET_MATRIX_SZ0(dset)-1;
            }
            break;
         case MAT_SPARSE:
            SUMA_S_Err("Sparse matrix and no inel?");
            SUMA_RETURN(0);
      }
      SUMA_RETURN(1);
   }
   
   rs = NI_get_attribute(dset->inel, "COLMS_RANGE");
   if (!rs) { 
      if (!addifmissing) {
         SUMA_SL_Err("No range field."); SUMA_RETURN(0); 
      }else {
         if (SUMA_isGraphDset(dset)) {
            if (!SUMA_AddGenDsetNodeIndexColAttr (dset, SUMA_EDGE_P1_INDEX, 
                                             SDSET_EDGE_P1_INDEX_COL(dset), 1)) {
               SUMA_SL_Err("Could not add range field for P1."); SUMA_RETURN(0); 
            }
            if (!SUMA_AddGenDsetNodeIndexColAttr (dset, SUMA_EDGE_P2_INDEX, 
                                             SDSET_EDGE_P2_INDEX_COL(dset), 1)) {
               SUMA_SL_Err("Could not add range field for P2."); SUMA_RETURN(0); 
            }
         }
         if (!SUMA_AddGenDsetNodeIndexColAttr (dset, SUMA_NODE_INDEX, 
                                             SDSET_NODE_INDEX_COL(dset), 1)) {
            SUMA_SL_Err("Could not add range field."); SUMA_RETURN(0); 
         }
         rs = NI_get_attribute(dset->inel, "COLMS_RANGE"); 
      }
   }
   if (!SUMA_isGraphDset(dset)){
      if (SUMA_StringToNum(rs, (void *)nums, 4, 2) != 4) { 
         SUMA_SL_Err("Failed to read 4 nums from range."); 
         SUMA_RETURN(0); 
      }
   } else {
      ctmp = SUMA_Get_Sub_String(rs, SUMA_NI_CSS, ii);
      if (SUMA_StringToNum(ctmp, (void *)nums, 4, 2) != 4) { 
         SUMA_SL_Err("Failed to read 4 nums from range."); 
         SUMA_RETURN(0); 
      } 
   }
   range[0] = nums[0]; range[1] = nums[1]; 
   loc[0] = (int)nums[2]; loc[1] = (int)nums[3];
   SUMA_RETURN(1);
}

int SUMA_GetDsetNodeIndexColRange(  SUMA_DSET *dset, 
                                    double range[2], int loc[2], 
                                    int addifmissing)
{
   return(SUMA_GetDsetNodeIndexColRange_eng(
            dset, range, loc, addifmissing,0));
}


/*!
   \brief Gets the column range values
   col_index can be -1 if you want the attributes of the last column
*/
int SUMA_GetColRange(NI_element *nel, int col_index, double range[2], int loc[2])
{
   static char FuncName[]={"SUMA_GetColRange"};
   char *rs = NULL, Name[500];
   double nums[4];
   
   SUMA_ENTRY;
   
   SUMA_SL_Warn("Obsolete, use new version.");

   if (!nel) { SUMA_SL_Err("Null Nel"); SUMA_RETURN(0); }
   if (col_index < 0) col_index = nel->vec_num-1;
   if (col_index < 0 || !nel->vec_num ) { 
      SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); }
   if (nel->vec_num <= col_index) { 
      SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   
   
   sprintf(Name, "RangeCol_%d", col_index);
   rs = NI_get_attribute(nel, Name);
   
   if (!rs) { SUMA_SL_Err("No range field."); SUMA_RETURN(0); }
   if (SUMA_StringToNum(rs, (void *)nums, 4, 2) != 4) { 
      SUMA_SL_Err("Failed to read 4 nums from range."); SUMA_RETURN(0); }
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
int SUMA_AddDsetNelCol (   SUMA_DSET *dset, char *col_label, 
                           SUMA_COL_TYPE ctp, void *col, 
                           void *col_attr, int stride)
{
   static char FuncName[]={"SUMA_AddDsetNelCol"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_RETURN(SUMA_InsertDsetNelCol(  dset, col_label, ctp, 
                                       col, col_attr, stride, -1));
}

int SUMA_InsertDsetNelCol ( SUMA_DSET *dset, char *col_label, 
                            SUMA_COL_TYPE ctp, void *col, 
                            void *col_attr, int stride, int icol)
{
   static char FuncName[]={"SUMA_InsertDsetNelCol"};
   int *iv, is_sorted;
   NI_element *nelb=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (SUMA_IS_DATUM_INDEX_COL(ctp)) {
      SUMA_RETURN(SUMA_AddDsetNelIndexCol (  dset, col_label, ctp, 
                                             col, col_attr, stride));
   }
   if (SUMA_isGraphDset(dset) && SUMA_IS_GNODE_IXYZ_COL(ctp)) {
      SUMA_RETURN(SUMA_AddGDsetNelXYZCol (dset, col_label, ctp, col, 
                                          col_attr, stride));
   }
   if (icol != -1) {
      /*
      SUMA_S_Err("Function not ready to deal with attribute insertion yet."
                 " See bottom of function");
      SUMA_RETURN(0); 
      */
      SUMA_LH("insertion mode");
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
         NI_insert_column_stride (  dset->dnel, NI_INT, 
                                    (int *)col, stride, icol);
         break;
      case SUMA_float:
         NI_insert_column_stride (  dset->dnel, NI_FLOAT, 
                                    (float *)col, stride, icol );      
         break;
      case SUMA_byte:
         NI_insert_column_stride (  dset->dnel, NI_BYTE, 
                                    (byte *)col, stride, icol );      
         break;
      case SUMA_double:
         NI_insert_column_stride (  dset->dnel, NI_DOUBLE, 
                                    (double *)col, stride, icol );      
         break;
      case SUMA_string:
         NI_insert_column_stride (  dset->dnel, NI_STRING, 
                                    (char **)col, stride, icol );
         break;
      case SUMA_complex:
         NI_insert_column_stride (  dset->dnel, NI_COMPLEX, 
                                    (complex *)col, stride, icol );
         break;
      default:
         fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   if (LocalHead && !col) {
      SUMA_ShowNel((void*)dset->dnel);
   }
   
   SUMA_LH("Cheking generic attributes");
   
   /* set some generic attributes. 
   You must redo it for all columns from icol 
   to the very end but that is not a pleasant task 
   because the functions below cannot insert. Only append (icol = -1)
   or replace. You'll need to find a solution for inserts before 
   allowing column insertion.  
   That is now fixed, Oct 07 */
   SUMA_LH("Before SUMA_AddGenDsetColAttr"); 
   SUMA_AddGenDsetColAttr (dset, ctp, col, stride, icol, 1);
   /* add the attributes of that column */
   SUMA_LH("Before SUMA_AddDsetColAttr"); 
   SUMA_AddDsetColAttr (dset, col_label, ctp, col_attr, icol, 1);
   SUMA_LH("Before Returning"); 

   SUMA_RETURN(1);
}

/*! 
   A convenient form of adding a default node index column
*/
int SUMA_AddDsetIndexCol(SUMA_DSET *dset, int *icolu, int *icolp1, int *icolp2) 
{
   static char FuncName[]={"SUMA_AddDsetIndexCol"};
   int ii=0, *icol=NULL;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(NOPE);
   if (SDSET_VECLEN(dset) <= 0) SUMA_RETURN(NOPE);
   if (!icolu) {
      if (SUMA_isGraphDset(dset)) { /* sorry no guessing */
         SUMA_RETURN(YUP);
      }
      if (SUMA_isCIFTIDset(dset)) { /* no guessing either */
         SUMA_RETURN(YUP);
      }
      if (!(icol = (int *)SUMA_malloc(sizeof(int)*SDSET_VECLEN(dset)))) {
         SUMA_S_Err("Failed to icolate");
         SUMA_RETURN(NOPE); 
      }
      for (ii=0; ii<SDSET_VECLEN(dset); ++ii) icol[ii]=ii;
   } else {
      if (SUMA_isGraphDset(dset) && (!icolp1 || !icolp2)) {
         SUMA_S_Err("Uncool for a graph");
         SUMA_RETURN(NOPE); 
      }
      icol = icolu;
   }
   if (SUMA_isCIFTIDset(dset)) {
      if (!SUMA_AddDsetNelIndexCol (dset, "MD node index", 
                                    SUMA_MD_NODE_INDEX, (void *)icol,
                                    NULL, 1)) {
         SUMA_S_Err("Failed to add default multi domain index col");
         SUMA_RETURN(NOPE);                             
      }
   } else {
      if (!SUMA_AddDsetNelIndexCol (dset, "node index", 
                                    SUMA_NODE_INDEX, (void *)icol,
                                    NULL, 1)) {
         SUMA_S_Err("Failed to add default index col");
         SUMA_RETURN(NOPE);                             
      }
   }
   if (icolp1) {
      if (!SUMA_AddDsetNelIndexCol (dset, "P1 index", 
                                    SUMA_EDGE_P1_INDEX, (void *)icolp1,
                                    NULL, 1)) {
         SUMA_S_Err("Failed to add p1 index col");
         SUMA_RETURN(NOPE);                             
      }
   }
   if (icolp2) {
      if (!SUMA_AddDsetNelIndexCol (dset, "P2 index", 
                                    SUMA_EDGE_P2_INDEX, (void *)icolp2,
                                    NULL, 1)) {
         SUMA_S_Err("Failed to add p2 index col");
         SUMA_RETURN(NOPE);                             
      }
   }
   if (!icolu) SUMA_free(icol);
    
   SUMA_RETURN(YUP);    
}
/*!
   \brief A version of SUMA_AddDsetNelCol that is specific for adding node indices
   \sa SUMA_AddDsetIndexCol();
*/
int SUMA_AddDsetNelIndexCol ( SUMA_DSET *dset, char *col_label, 
                               SUMA_COL_TYPE ctp, void *col, 
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
   if (ctp == SUMA_MD_NODE_INDEX) {
      SUMA_S_Err("This function cannot handle multiple domain indices\n"
                 "You might want to call SUMA_CIFTI_Set_Domains()\n");
      SUMA_RETURN(0);
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
   
   switch (SUMA_ColType2TypeCast(ctp)) { /* overkill, all we use is int here, 
                                            but keeping the code anyway ...*/
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
      case SUMA_complex:
         NI_add_column_stride ( dset->inel, NI_COMPLEX, col, stride );
         break;
      default:
         fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   
   if (ctp == SUMA_NODE_INDEX) { /* Can also be SUMA_EDGE_P[12]_INDEX*/
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
   
   
   
   SUMA_LH("Setting index attributes");
   /* set some generic attributes */
   SUMA_AddGenDsetColAttr (dset, ctp, col, stride, -1, 0);
   /* add the attributes of that column */
   SUMA_AddDsetColAttr (dset, col_label?col_label:"node index", 
                        ctp, col_attr, -1, 0);
   if (LocalHead) SUMA_ShowNel((void*)dset->inel);
   
   
   SUMA_RETURN(1);
}

int SUMA_AddGDsetNelXYZCol ( SUMA_DSET *dset, char *col_label, 
                               SUMA_COL_TYPE ctp, void *col, 
                               void *col_attr, int stride)
{
   static char FuncName[]={"SUMA_AddGDsetNelXYZCol"};
   int *iv, is_sorted;
   NI_element *xyznel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      SUMA_DUMP_TRACE("Trace at entry");
   }
   if (!SUMA_isGraphDset(dset)) {
      SUMA_SL_Err("Null or bad input"); 
      SUMA_RETURN(0); 
   }
   
   if (!(xyznel = SUMA_FindGDsetNodeListElement(dset))) {
      SUMA_S_Err("No nodelist element");
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
   
   SUMA_LHv("Checking Type %s, stride %d\n",
            SUMA_Col_Type_Name(ctp), stride);
   
   switch (SUMA_ColType2TypeCast(ctp)) { /* overkill, all we use is int here, 
                                            but keeping the code anyway ...*/
      case SUMA_int:
         NI_add_column_stride (xyznel, NI_INT, col, stride);
         break;
      case SUMA_float:
         NI_add_column_stride (xyznel, NI_FLOAT, col, stride );      
         break;
      case SUMA_byte:
         NI_add_column_stride (xyznel, NI_BYTE, col, stride );      
         break;
      case SUMA_double:
         NI_add_column_stride (xyznel, NI_DOUBLE, col, stride );      
         break;
      case SUMA_string:
         NI_add_column_stride (xyznel, NI_STRING, col, stride );
         break;
      case SUMA_complex:
         NI_add_column_stride (xyznel, NI_COMPLEX, col, stride );
         break;
      default:
         fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
      
   
   SUMA_LH("Cheking generic attributes");
   /* set some generic attributes */
   SUMA_AddGenDsetColAttr (dset, ctp, col, stride, -1, 0);
   /* add the attributes of that column */
   SUMA_AddDsetColAttr (dset, col_label, ctp, col_attr, -1, 0);
   
   if (LocalHead) SUMA_ShowNel((void*)xyznel);
   
   
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

int SUMA_AddNelCol ( NI_element *nel, char *col_label, SUMA_COL_TYPE ctp, 
                     void *col, 
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
      case SUMA_complex:
         NI_add_column_stride ( nel, NI_COMPLEX, (complex *)col, stride );
         break;
      default:
         fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   
   if (ctp == SUMA_NODE_INDEX) { /* No need to woryy about sorting for SUMA_EDGE_P[12]_INDEX */
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
SUMA_DSET * SUMA_MaskedByNodeIndexCopyofDset(
      SUMA_DSET *odset, int *indexlist, 
      int N_indexlist, byte *colmask, 
      int masked_only, int keep_node_index)
{
   static char FuncName[]={"SUMA_MaskedByNodeIndexCopyofDset"};
   SUMA_DSET *dset_m = NULL;
   byte *Tb = NULL;
   int *indexmap = NULL, j=0;
   double range[2];
   int loc[2];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!(indexmap = SUMA_CreateNodeIndexToRowIndexMap(odset, -1, range))) {
      SUMA_S_Err("Failed to get indexmap");
      SUMA_RETURN(NULL);
   }
    
   Tb = (byte *) SUMA_calloc(SDSET_VECLEN(odset), sizeof(byte));
   for (j=0; j<N_indexlist; ++j) {
      if (  indexmap[indexlist[j]] >=0 && 
            indexmap[indexlist[j]] < SDSET_VECFILLED(odset) &&
            indexlist[j] <= (int) range[1]) { 
         Tb[indexmap[indexlist[j]]] = 1;
      } else {
         SUMA_S_Warn("Nodes in indexlist exceed odset->dnel->vec_filled\n"
                     "Such nodes will be ignored but may indicate \n"
                     "more serious trouble.\n"
                     "Warning will not be repeated in this call.");
      }
   }
   if (LocalHead) {
      for (j=0; j<N_indexlist; ++j) { 
         fprintf(stderr,"indexlist[%d]=%d\n", j,indexlist[j]); 
      }
      for (j=0; j<SDSET_VECLEN(odset); ++j) {
         if (Tb[j]) fprintf(stderr,"Tb[%d/%d]=%d\n",
                            j, SDSET_VECLEN(odset), Tb[j]);  
      }
   }
   SUMA_free(indexmap); indexmap = NULL;
   
   if (!(dset_m = SUMA_MaskedCopyofDset(  odset, Tb, colmask, 
                                          masked_only, keep_node_index ))) {
      SUMA_S_Err("Failed to mask dset by node indices");
      SUMA_free(Tb); Tb = NULL;
      SUMA_RETURN(NULL);
   }
   
   SUMA_RETURN(dset_m);
}

SUMA_DSET * SUMA_MaskedByOrderedNodeIndexCopyofDset(
      SUMA_DSET *odset, int *indexlist_orig, 
      int N_indexlist_orig, byte *colmask, 
      int masked_only, int keep_node_index)
{
   static char FuncName[]={"SUMA_MaskedByOrderedNodeIndexCopyofDset"};
   SUMA_DSET *dset_uo = NULL;
   int *rowofnode = NULL, i=0, c=0, nn=0,rownodebuf=0, *nl=NULL, nlbuf=0;
   NI_rowtype *rt = NULL;
   SUMA_COL_TYPE ctp = SUMA_ERROR_COL_TYPE;
   void *vcol=NULL;
   byte bbuf=0, *bcol=NULL;
   short sbuf=0, *scol=NULL; 
   int ibuf=0, *icol=NULL; 
   float fbuf=0, *fcol=NULL; 
   double dbuf=0, *dcol=NULL, range[2];
   int *indexlist=NULL, N_indexlist=-1; 
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   dset_uo = SUMA_MaskedByNodeIndexCopyofDset(
               odset, indexlist_orig, N_indexlist_orig, 
               colmask, masked_only,keep_node_index);
   
   /* now we have the dset we want, but the oder is shuffled. */
   if (!(rowofnode = SUMA_CreateNodeIndexToRowIndexMap(dset_uo, -1, range))) {
      SUMA_S_Err("Failed to create map");
      SUMA_RETURN(NULL);
   }
   if (!(nl = SDSET_NODE_INDEX_COL(dset_uo))) {
      SUMA_S_Err("Need a node index to do this");
      SUMA_RETURN(NULL);
   }
   /* trim index list of bad indices */
   if (SDSET_VECLEN(dset_uo) < N_indexlist_orig) {
      indexlist = (int *)SUMA_calloc(SDSET_VECLEN(dset_uo), sizeof(int));
      N_indexlist = 0;
      while (i < N_indexlist_orig) {
         if (  indexlist_orig[i] >= (int)range[0] && 
               indexlist_orig[i] <= (int)range[1]) {
            if (N_indexlist < SDSET_VECLEN(dset_uo)) {
               indexlist[N_indexlist] = indexlist_orig[i]; ++N_indexlist;
            } else {
               SUMA_S_Err("Unexpected N_indexlist > SDSET_VECLEN");
               SUMA_RETURN(NULL);
            }
         } 
         ++i;
      }
   } else {
      indexlist = indexlist_orig;
      N_indexlist = N_indexlist_orig;
   }
   
   /* get a buffer dset that has one row only. */
   for (i=0; i<N_indexlist; ++i) {
      nn = indexlist[i];
      if (rowofnode[nn] == i) { /* all is good */
      } else {
         for (c=0; c<SDSET_VECNUM(dset_uo); ++c) {
            bcol=NULL; bbuf=0; 
            scol=NULL; sbuf=0;
            icol=NULL; ibuf=0;
            fcol=NULL; fbuf=0.0;
            dcol=NULL; dbuf=0.0;
            vcol = SDSET_VEC(dset_uo, c);
            ctp = SUMA_TypeOfDsetColNumb(dset_uo, c); 
            rt = NI_rowtype_find_code(SUMA_ColType2TypeCast(ctp)) ; 
            /* need to put data from node nn in row i */
            switch(rt->code) {
               case NI_BYTE:
                  bcol = (byte*)vcol;
                  bbuf = bcol[i];
                  bcol[i] = bcol[rowofnode[nn]]; 
                  bcol[rowofnode[nn]] = bbuf;
                  break;
               case NI_SHORT:
                  scol = (short*)vcol;
                  sbuf = scol[i];
                  scol[i] = scol[rowofnode[nn]]; 
                  scol[rowofnode[nn]] = sbuf;
                  break;
               case NI_INT:
                  icol = (int*)vcol;
                  ibuf = icol[i];
                  icol[i] = icol[rowofnode[nn]]; 
                  icol[rowofnode[nn]] = ibuf;
                  break;
               case NI_FLOAT:
                  fcol = (float*)vcol;
                  fbuf = fcol[i];
                  fcol[i] = fcol[rowofnode[nn]]; 
                  fcol[rowofnode[nn]] = fbuf;
                  break;
               case NI_DOUBLE:
                  dcol = (double*)vcol;
                  dbuf = dcol[i];
                  dcol[i] = dcol[rowofnode[nn]]; 
                  dcol[rowofnode[nn]] = dbuf;
                  break;
               default:
               SUMA_SL_Warn(
                  "Type not allowed for padding operation, skipping");
               break;
            }
         }/* for each column c */
         rownodebuf = rowofnode[nn];
         rowofnode[nn] = i;
         rowofnode[nl[i]] = rownodebuf;
         nlbuf = nl[i];
         nl[i] = nn;
         nl[rownodebuf] = nlbuf;
      }
   } 
   if (indexlist_orig !=  indexlist) SUMA_free(indexlist); indexlist = NULL;
   if (rowofnode) SUMA_free(rowofnode); rowofnode=NULL;
   SUMA_RETURN(dset_uo);
}
 
/*! 
   \brief creates a new dataset that has a copy of each row in odset 
   wherever rowmask[irow] is not zero. 
   \param odset (SUMA_DSET *) input dataset
   \param rowmask (byte *) [nel->vec_len x 1] 
               vector specifying which rows to preserve
               If rowmask[irow] is 1 then this row is copied
               into ndset. 
               If rowmask[irow] = 0 then the row is either
               skipped (see masked_only) or set to 0 in its entirety 
               (see keep_node_index for exception).
               If rowmask == NULL then all rows are copied
   \param colmask (byte *) [nel->vec_num x 1] 
               vector specifying which volumns to operate on.
               If colmask[icol] is 1 then values in this column 
               are copied. 
               If colmask == NULL then all columns are copied
   \param masked_only (int)   If 1 then the output dataset is only to contain
               those rows where rowmask[irow] = 1
               If 0 then all rows are output but with column entries set to 0
               for all rows where rowmask[irow] = 0. One column might be
               exempt from nulling if it meets the requirements on 
               Schedule B form suma654.233 
               or if it is of the type SUMA_NODE_INDEX and keep_node_index 
               is set to 1. 

   \param keep_node_index (int) 
               If 1, then preserves the node index column (SUMA_NODE_INDEX) 
               from being masked.
               Makes sense to use it when masked_only == 0. 
   \param ndset (SUMA_DSET *) Copy of dataset with masking applied.
   
   - You might want to have a version that replaces columns in odset with 
   the masked data
   as opposed to copying them. 
   I think I do something like this with the drawn ROI dataset...
   
   This function should now work with Graph Dsets 
   
   \sa SUMA_Copy_Part_Column, SUMA_MaskedByNodeIndexCopyofDset
*/  
SUMA_DSET * SUMA_EmptyCopyofDset (  SUMA_DSET *odset, 
                                    byte *rowmask,
                                    int masked_only, int keep_node_index)
{
   static char FuncName[]={"SUMA_EmptyCopyofDset"};
   SUMA_DSET *ndset=NULL;
   int i, n_incopy=-1;
   NI_rowtype *rti=NULL;
   void *ind=NULL, *indp1=NULL, *indp2=NULL, *ncoli = NULL, 
        *ncolp1=NULL, *ncolp2=NULL;
   char *new_name=NULL, idcode[SUMA_IDCODE_LENGTH], *lblcp=NULL;   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!odset) { SUMA_SL_Err("Null input"); SUMA_RETURN(NULL); }
   if (LocalHead && SUMA_isCIFTIDset(odset)) {
      SUMA_S_Warn("Not fully tested for CIFTI dsets. Returned dataset is"
      	          "a single domain one.");
   }
   ind = SDSET_NODE_INDEX_COL(odset);
   if (rowmask && !ind) {
      SUMA_S_Note("Will force population of node index element");
      SUMA_PopulateDsetNodeIndexNel(odset,0);
   }
   /* deal with node index element */
   if ((ind = SDSET_NODE_INDEX_COL(odset))) {
      rti = NI_rowtype_find_code(SUMA_ColType2TypeCast(SUMA_NODE_INDEX)) ;
      if (SUMA_isGraphDset(odset)) {
          indp1 = SDSET_EDGE_P1_INDEX_COL(odset);
          indp2 = SDSET_EDGE_P2_INDEX_COL(odset);
          if (!indp1 || !indp2) {
            SUMA_S_Err("This should not happen here, bad index element?");
            SUMA_RETURN(ndset);
          }
      }
      if (0 && LocalHead) {
         char *ss=NULL;
         SUMA_LH("Pre copy");
         SUMA_ShowNel((void*)odset->inel);
         ss = SUMA_ShowMeSome(ind, SUMA_int, SDSET_VECLEN(odset), -1, NULL);
         SUMA_S_Note("%s",ss); if (ss) SUMA_free(ss); ss = NULL;
      }
      if (keep_node_index && !masked_only) {
         /* preserve all rows */
         ncoli = SUMA_Copy_Part_Column(
                  ind, rti, SDSET_VECLEN(odset), 
                  NULL, masked_only, &n_incopy);
         if (SUMA_isGraphDset(odset)) {
            ncolp1 = SUMA_Copy_Part_Column(
                  indp1, rti, SDSET_VECLEN(odset), 
                  NULL, masked_only, &n_incopy);
            ncolp2 = SUMA_Copy_Part_Column(
                  indp2, rti, SDSET_VECLEN(odset), 
                  NULL, masked_only, &n_incopy);
         }         
      } else {
         ncoli = SUMA_Copy_Part_Column(
                  ind, rti, SDSET_VECLEN(odset), 
                  rowmask, masked_only, &n_incopy); 
         if (SUMA_isGraphDset(odset)) { 
            ncolp1 = SUMA_Copy_Part_Column(
                  indp1, rti, SDSET_VECLEN(odset), 
                  rowmask, masked_only, &n_incopy); 
            ncolp2 = SUMA_Copy_Part_Column(
                  indp2, rti, SDSET_VECLEN(odset), 
                  rowmask, masked_only, &n_incopy); 
         }
      }
      if (!ncoli) {
         SUMA_SL_Err("No index data got copied.");
         SUMA_RETURN(ndset);
      }
      if (SUMA_isGraphDset(odset) && (!ncolp1 || !ncolp2)) {
         SUMA_SL_Err("No point index data got copied.");
         SUMA_RETURN(ndset);
      }
      if (0 && LocalHead) {
         char *ss=NULL;
         SUMA_LH("Post copy");
         SUMA_ShowNel((void*)odset->inel);
         ss = SUMA_ShowMeSome(ncoli, SUMA_int, n_incopy, -1, NULL);
         SUMA_S_Note("%s",ss); if (ss) SUMA_free(ss); ss = NULL;
      }
   } else { /* no node index to work with */
      if (keep_node_index && !masked_only) {
         n_incopy = SDSET_VECLEN(odset);
      } else {
         if (rowmask) {
            n_incopy = 0;
            for (i=0; i<SDSET_VECLEN(odset); ++i) if (rowmask[i]) ++n_incopy;
         } else {
            n_incopy = SDSET_VECLEN(odset);
         }
      }
   }
   SUMA_LH("Forming new dset");
   
   /* Now create a new dset */   
   new_name = SUMA_append_string( NI_get_attribute(odset->ngr,"filename"),
                                  "copy");
   UNIQ_idcode_fill(idcode); 
   ndset =  SUMA_CreateDsetPointer( 
                              new_name, 
                              SUMA_Dset_Type(NEL_DSET_TYPE(odset->ngr)), 
                              idcode, 
                              NI_get_attribute(odset->ngr,
                              "domain_parent_idcode"),
                              n_incopy ); 
   SUMA_free(new_name); new_name = NULL;
   if (ncoli) {      
      SUMA_LH("Adding ncoli");
      if (!SUMA_AddDsetNelCol (ndset, 
                     "node index",
                     SUMA_NODE_INDEX, ncoli, NULL ,1)) {
         SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
         SUMA_FreeDset((void*)ndset); ndset = NULL;
         SUMA_RETURN(ndset);
      }
      SUMA_free(ncoli); ncoli=NULL;
      if (ncolp1 && ncolp2) {
         if (!SUMA_AddDsetNelCol (ndset, 
                     "P1 index",
                     SUMA_EDGE_P1_INDEX, ncolp1, NULL ,1)) {
            SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
            SUMA_FreeDset((void*)ndset); ndset = NULL;
            SUMA_RETURN(ndset);
         }
         if (!SUMA_AddDsetNelCol (ndset, 
                     "P2 index",
                     SUMA_EDGE_P2_INDEX, ncolp2, NULL ,1)) {
            SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
            SUMA_FreeDset((void*)ndset); ndset = NULL;
            SUMA_RETURN(ndset);
         }
         SUMA_free(ncolp1); ncolp1=NULL;
         SUMA_free(ncolp2); ncolp2=NULL;
      }
   }
   
   
   /* Here you would use SUMA_CopyDsetAttributes
   with attr_list consisting of attributes that 
   are Dsetwide. To do so, set isrc and idest to -1
   For now, there is no such attributes that need to
   be copied. */
    
   SUMA_RETURN(ndset);
}
/* 
   Scan dataset for nans/infs and replace them with 0
*/
int SUMA_FloatScanDset ( SUMA_DSET *odset, int doNan, int doInf, int zerout, 
                         int fixrange )
{
   static char FuncName[]={"SUMA_FloatScanDset"};
   int i, j, nfixed=0, nfixed_total = 0, nwarn=0;
   NI_rowtype *rt = NULL;   
   void *ndat=NULL;
   SUMA_COL_TYPE ctp = SUMA_ERROR_COL_TYPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   SUMA_LH("IN");
   if (!odset) { SUMA_SL_Err("Null input"); SUMA_RETURN(nfixed_total); }
   if (!doNan && !doInf) {
      SUMA_SL_Err("Nothing to do"); SUMA_RETURN(nfixed_total); 
   }
   if (fixrange) zerout = 1; /* no point in fixing range without zeroing out */
   
   /* scan each column in dset, get a full copy back */
   nfixed_total = 0;
   for (i=0; i < SDSET_VECNUM(odset); ++i) {
         nfixed = 0;
         ctp = SUMA_TypeOfDsetColNumb(odset, i); 
         rt = NI_rowtype_find_code(SUMA_ColType2TypeCast(ctp)) ; 
         if( rt == NULL || ROWTYPE_is_varsize(rt)) {
            SUMA_S_Warn("Could not recognize rowtype, or rowtype is of "
                        "variable size. Column will be skipped."); 
            continue;
         }
         
         /* scan */
         switch (rt->code) {
            case NI_BYTE:
            case NI_SHORT:
            case NI_INT:
               /* No illegal bits */
               break;
            case NI_FLOAT:
               { 
                  float *fv=(float *)odset->dnel->vec[i];
                  for (j=0; j<SDSET_VECFILLED(odset); ++j) 
                     if (doNan && isnan(fv[j])) {
                        ++nfixed;
                        if (zerout) fv[j]=0.0;
                     } else if (doInf && isinf(fv[j])) {
                        ++nfixed;
                        if (zerout) fv[j]=0.0;
                     }
               }
               break;
            case NI_DOUBLE:
               { 
                  double *dv=(double *)odset->dnel->vec[i];
                  for (j=0; j<SDSET_VECFILLED(odset); ++j) 
                     if (doNan && isnan(dv[j])) {
                        ++nfixed;
                        if (zerout) dv[j]=0.0;
                     } else if (doInf && isinf(dv[j])) {
                        ++nfixed;
                        if (zerout) dv[j]=0.0;
                     }
               }
               break;
            case NI_COMPLEX:
               { 
                  complex *cxv=(complex *)odset->dnel->vec[i];
                  for (j=0; j<SDSET_VECFILLED(odset); ++j) 
                     if (doNan && (isnan(cxv[j].r) || isnan(cxv[j].i))) {
                        ++nfixed;
                        if (zerout) { cxv[j].r=0.0; cxv[j].i=0.0; }
                     } else if (doInf && (isinf(cxv[j].r) || isinf(cxv[j].i))) {
                        ++nfixed;
                        if (zerout) { cxv[j].r=0.0; cxv[j].i=0.0; }
                     }
               }
               break;
            default:
               /* nothing to check here either */
               break;
         }
         if (nfixed && fixrange) {
            if (!nwarn) {
               SUMA_S_Note("Recomputing range for column with inf or nans\n"
                        "Message muted for other columns");
            }
            ++nwarn;
            SUMA_UpdateDsetColRange(odset, i);
         }  
         nfixed_total += nfixed;
   }
   SUMA_LH("OUT");
   SUMA_RETURN(nfixed_total);
}

/* Return a full and node_index-sorted version of the input dataset.
*/
SUMA_DSET * SUMA_PaddedCopyofDset ( SUMA_DSET *odset, int MaxNodeIndex )
{
   static char FuncName[]={"SUMA_PaddedCopyofDset"};
   SUMA_DSET *ndset=NULL;
   int i, n_incopy=-1, j, *indold = NULL, *indnew = NULL, N_inmask;
   NI_rowtype *rti=NULL, *rt = NULL;
   char *new_name=NULL, idcode[SUMA_IDCODE_LENGTH], *lblcp=NULL;   
   void *ndat=NULL;
   SUMA_COL_TYPE ctp = SUMA_ERROR_COL_TYPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!odset) { SUMA_SL_Err("Null input"); SUMA_RETURN(NULL); }
   if (SUMA_isCIFTIDset(odset)) { 
      SUMA_SL_Err("Not ready for CIFTI"); SUMA_RETURN(NULL); 
   }
   /* deal with node index element */
   if ((indold = SDSET_NODE_INDEX_COL(odset))) {
      rti = NI_rowtype_find_code(SUMA_ColType2TypeCast(SUMA_NODE_INDEX)) ;
   } else { /* no node index to work with */
      SUMA_S_Err("No node index element!");
      SUMA_RETURN(NULL);
   }
   
   DSET_MAX_NODE_INDEX(odset, i);
   if (MaxNodeIndex < i) {
      SUMA_S_Errv("MaxNodeIndex =%d but dset has nodes up to %d\n",
                  MaxNodeIndex, i);
      SUMA_RETURN(NULL);
   } 
   
   /* form new indices */
   if (!(indnew = (int *)SUMA_calloc(MaxNodeIndex+1, sizeof(int)))) {
      SUMA_S_Crit("Failed to allocate for indnew!");
      SUMA_RETURN(NULL);
   }
   for (i=0; i<MaxNodeIndex+1; ++i) indnew[i] = i;
   SUMA_LH("Forming new dset");
   
   /* Now create a new dset */   
   new_name = SUMA_append_string( NI_get_attribute(odset->ngr,"filename"),
                                  "padded");
   UNIQ_idcode_fill(idcode); 
   ndset =  SUMA_CreateDsetPointer( 
                              new_name, 
                              SUMA_Dset_Type(NEL_DSET_TYPE(odset->ngr)), 
                              idcode, 
                              NI_get_attribute(odset->ngr,
                              "domain_parent_idcode"),
                              MaxNodeIndex+1); 
   SUMA_free(new_name); new_name = NULL;
   

   SUMA_COPY_DSETWIDE_ATTRIBUTES(odset, ndset);

   SUMA_LH("Adding indnew");
   if (!SUMA_AddDsetNelCol (ndset, 
                  NI_get_attribute(ndset->inel,"COLMS_LABS"),
                  SUMA_NODE_INDEX, (void *)indnew, NULL ,1)) {
      SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
      SUMA_FreeDset((void*)ndset); ndset = NULL;
      SUMA_RETURN(ndset);
   }
   
   /* for each column in dset, get a full copy back */
   for (i=0; i < SDSET_VECNUM(odset); ++i) {
         ctp = SUMA_TypeOfDsetColNumb(odset, i); 
         rt = NI_rowtype_find_code(SUMA_ColType2TypeCast(ctp)) ; 
         if( rt == NULL || ROWTYPE_is_varsize(rt)) {
            SUMA_S_Warn("Could not recognize rowtype, or rowtype is of "
                        "variable size. Column will be skipped."); 
            continue;
         }
         /* Allocate */
         switch (rt->code) {
            case NI_BYTE:
               ndat = SUMA_calloc(MaxNodeIndex+1, sizeof(byte));
               break;
             case NI_SHORT:
               ndat = SUMA_calloc(MaxNodeIndex+1, sizeof(short));
               break;
             case NI_INT:
               ndat = SUMA_calloc(MaxNodeIndex+1, sizeof(int));
               break;
             case NI_FLOAT:
               ndat = SUMA_calloc(MaxNodeIndex+1, sizeof(float));
               break;
             case NI_DOUBLE:
               ndat = SUMA_calloc(MaxNodeIndex+1, sizeof(double));
               break;
             case NI_COMPLEX:
               ndat = SUMA_calloc(MaxNodeIndex+1, sizeof(complex));
               break;
             default:
               SUMA_SL_Warn(
                  "Type not allowed for padding operation, skipping");
               break;
         }
         /* fill up */
         SUMA_LH("Preserving rows in mask only");
         switch (rt->code) {
            case NI_BYTE:
               { 
                  byte *bv=(byte *)odset->dnel->vec[i];
                  for (j=0; j<SDSET_VECFILLED(odset); ++j) 
                     if (indold[j] <= MaxNodeIndex){
                        SUMA_ASSIGN_VALUE_IN_VEC ( ndat, indold[j], 
                                                   byte, bv[j]);
                     }
               }
               break;
            case NI_SHORT:
               { 
                  short *sv=(short *)odset->dnel->vec[i];
                  for (j=0; j<SDSET_VECFILLED(odset); ++j) 
                     if (indold[j] <= MaxNodeIndex) {
                        SUMA_ASSIGN_VALUE_IN_VEC ( ndat, indold[j], 
                                                   short, sv[j]);
                     }
               }
               break;
            case NI_INT:
               { 
                  int *iv=(int *)odset->dnel->vec[i];
                  for (j=0; j<SDSET_VECFILLED(odset); ++j) 
                     if (indold[j] <= MaxNodeIndex) {
                        SUMA_ASSIGN_VALUE_IN_VEC ( ndat, indold[j], 
                                                int, iv[j]);
                     }
               }
               break;
            case NI_FLOAT:
               { 
                  float *fv=(float *)odset->dnel->vec[i];
                  for (j=0; j<SDSET_VECFILLED(odset); ++j) 
                     if (indold[j] <= MaxNodeIndex) {
                        SUMA_ASSIGN_VALUE_IN_VEC ( ndat, indold[j], 
                                                   float, fv[j]);
                     }
               }
               break;
            case NI_DOUBLE:
               { 
                  double *dv=(double *)odset->dnel->vec[i];
                  for (j=0; j<SDSET_VECFILLED(odset); ++j) 
                     if (indold[j] <= MaxNodeIndex) {
                        SUMA_ASSIGN_VALUE_IN_VEC ( ndat, indold[j], 
                                                   double, dv[j]);
                     }
               }
               break;
            case NI_COMPLEX:
               { 
                  complex *cxx=NULL, *cxv=(complex *)odset->dnel->vec[i];
                  for (j=0; j<SDSET_VECFILLED(odset); ++j) 
                     if (indold[j] <= MaxNodeIndex) {
                        cxx = (complex *)ndat;
                        cxx[indold[j]].r = cxv[j].r;
                        cxx[indold[j]].i = cxv[j].i;
                     }
               }
               break;
            default:
               SUMA_SL_Warn(
                  "Type not allowed for masking operation, skipping.");
               break;
         }
         
         switch (rt->code) {
            case NI_BYTE:
            case NI_SHORT:
            case NI_INT:
            case NI_FLOAT:
            case NI_DOUBLE:
            case NI_COMPLEX:
               /* add the column */
               SUMA_LH("Getting the label");
               lblcp = SUMA_DsetColLabelCopy(odset, i, 0);
               SUMA_LH("Inserting the column");
               if (!SUMA_AddDsetNelCol (  ndset, lblcp, ctp, 
                                          ndat, NULL ,1)) {
                  SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
                  SUMA_FreeDset((void*)ndset); ndset = NULL;
                  SUMA_RETURN(ndset);
               } 
               if (lblcp) SUMA_free(lblcp); lblcp = NULL;
               /* Adding other columnar attributes */
               SUMA_COPY_DSET_COL_ATTRIBUTES(odset, ndset, 
                                             i, SDSET_VECNUM(ndset)-1);

               break;
            default:
               SUMA_SL_Warn(
                  "Type not allowed for masking operation, skipping.");
               break;
         }
   }
   
   SUMA_RETURN(ndset);
}

/* Make a new copy of a dataset forcing all the columns to of type vtp */
SUMA_DSET *SUMA_CoercedCopyofDset( SUMA_DSET *odset, SUMA_VARTYPE vtp,
                                   byte *colmask)
{
   static char FuncName[]={"SUMA_CoercedCopyofDset"};
   int i = -1;
   int *coli=NULL;
   float *colf=NULL;
   void *colv=NULL;
   char *lblcp=NULL;
   SUMA_DSET *ndset=NULL;
   NI_rowtype *rt=NULL;
   SUMA_COL_TYPE ctp = SUMA_ERROR_COL_TYPE;
   int DoThisCol=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!odset) { SUMA_SL_Err("Null input"); SUMA_RETURN(NULL); }
   if (!odset->dnel) { SUMA_SL_Err("Null dnel"); SUMA_RETURN(NULL); }
   if (!SUMA_is_AllNumeric_dset(odset)) {
      SUMA_SL_Err("Function does not deal with data sets containing "
                  "non-numeric columns");
      SUMA_RETURN(NULL);
   }
   if (vtp != SUMA_int && vtp != SUMA_float) {
      SUMA_S_Err("Only SUMA_int and SUMA_float supported");
      SUMA_RETURN(NULL);
   }
   if (0 && LocalHead) {
      SUMA_ShowNel((void*)odset->dnel);
   }
   
   if (!ndset) {
      ndset = SUMA_EmptyCopyofDset( odset, NULL, 
                                    0, 1);

      SUMA_COPY_DSETWIDE_ATTRIBUTES(odset, ndset);
   }
   for (i=0; i < SDSET_VECNUM(odset); ++i) {
      if (!colmask) DoThisCol = 1;
      else DoThisCol = colmask[i];
      if (DoThisCol) {
         if (LocalHead) fprintf(SUMA_STDERR,"%s:\nProcessing column %d\n", 
                                 FuncName, i);
         ctp = SUMA_TypeOfDsetColNumb(odset, i); 
         rt = NI_rowtype_find_code(SUMA_ColType2TypeCast(ctp)) ; 
         if( rt == NULL || ROWTYPE_is_varsize(rt)) {
            SUMA_SL_Err("Could not recognize rowtype, or rowtype is of "
                        "variable size."); SUMA_RETURN(NULL);
         }
         coli=NULL;
         colf=NULL;
         if (SUMA_IS_DATUM_INDEX_COL(ctp)) {
            SUMA_S_Err("This should not be anymore. Skipping...");
         } else {
             switch(vtp) {
               case SUMA_int:
                  if (!(coli = SUMA_DsetCol2Int (odset, i, 1))) {
                     SUMA_S_Errv("Failed to convert col. %d to int (%d) type", 
                                 i, vtp);
                     SUMA_FreeDset(ndset); SUMA_RETURN(NULL);
                  }
                  ctp = SUMA_NODE_INT;
                  colv = (void *)coli;
                  break;
               case SUMA_float:
                  if (!(colf = SUMA_DsetCol2Float (odset, i, 1))) {
                     SUMA_S_Errv("Failed to convert col. %d to float (%d) type", 
                                 i, vtp);
                     SUMA_FreeDset(ndset); SUMA_RETURN(NULL);
                  }
                  ctp = SUMA_NODE_FLOAT;
                  colv = (void *)colf;
                  break;
               default:
                  SUMA_S_Errv("Not ready for type %d\n", vtp);
                  SUMA_FreeDset(ndset); SUMA_RETURN(NULL);
                  break;
            }
         }
         if (!colv) {
            SUMA_SL_Err("No data got copied.");
            SUMA_FreeDset(ndset); SUMA_RETURN(NULL);
         }
         /* add the column */
         SUMA_LH("Getting the label");
         lblcp = SUMA_DsetColLabelCopy(odset, i, 0);         
         SUMA_LH("Inserting the column");
         if (!SUMA_AddDsetNelCol (ndset, lblcp, ctp, colv, NULL ,1)) {
            SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
            SUMA_FreeDset((void*)ndset); ndset = NULL;
            SUMA_RETURN(ndset);
         } 
         SUMA_LH("Insertion finished");
         if (lblcp) SUMA_free(lblcp); lblcp = NULL;
         /* Adding other columnar attributes */
         SUMA_COPY_DSET_COL_ATTRIBUTES(odset, ndset, 
                                       i, SDSET_VECNUM(ndset)-1);
      } else {
         if (LocalHead) 
            fprintf(SUMA_STDERR,"%s:\nSkipping column %d\n", FuncName, i);
      }
   }
   
   SUMA_RETURN(ndset);
}

SUMA_DSET * SUMA_MaskedCopyofDset(  SUMA_DSET *odset, 
                                    byte *rowmask, byte *colmask, 
                                    int masked_only, int keep_node_index)
{
   static char FuncName[]={"SUMA_MaskedCopyofDset"};
   int n_incopy = -1, i=-1;
   char *lblcp=NULL;
   SUMA_DSET *ndset=NULL;
   NI_rowtype *rt=NULL;
   SUMA_COL_TYPE ctp = SUMA_ERROR_COL_TYPE;
   void *ncol=NULL;
   int DoThisCol=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!odset) { SUMA_SL_Err("Null input"); SUMA_RETURN(NULL); }
   if (!odset->dnel) { SUMA_SL_Err("Null dnel"); SUMA_RETURN(NULL); }
   if (!SUMA_is_AllNumeric_dset(odset)) {
      SUMA_SL_Err("Function does not deal with data sets containing "
                  "non-numeric columns");
      SUMA_RETURN(NULL);
   }
   if (0 && LocalHead) {
      SUMA_ShowNel((void*)odset->dnel);
   }
   
   for (i=0; i < SDSET_VECNUM(odset); ++i) {
      if (!colmask) DoThisCol = 1;
      else DoThisCol = colmask[i];
      if (DoThisCol) {
         if (LocalHead) fprintf(SUMA_STDERR,"%s:\nProcessing column %d\n", 
                                 FuncName, i);
         ctp = SUMA_TypeOfDsetColNumb(odset, i); 
         rt = NI_rowtype_find_code(SUMA_ColType2TypeCast(ctp)) ; 
         if( rt == NULL || ROWTYPE_is_varsize(rt)) {
            SUMA_SL_Err("Could not recognize rowtype, or rowtype is of "
                        "variable size."); SUMA_RETURN(NULL);
         }
         if (SUMA_IS_DATUM_INDEX_COL(ctp) && keep_node_index && !masked_only) {
            SUMA_S_Err("This should not be anymore. Skipping...");
            /* preserve all rows */
            /* ncol = SUMA_Copy_Part_Column(odset->dnel->vec[i], rt, 
                     SDSET_VECLEN(odset), NULL, masked_only, &n_incopy); */
         } else {
            SUMA_LH("Preserving rows in mask only");
            ncol = SUMA_Copy_Part_Column(
                                 odset->dnel->vec[i], rt,
                                 SDSET_VECLEN(odset), rowmask, 
                                 masked_only, &n_incopy);  
         }
         if (!ncol) {
            SUMA_SL_Err("No data got copied.");
            SUMA_RETURN(ndset);
         }
         if (!ndset) {
            ndset = SUMA_EmptyCopyofDset( odset, rowmask, 
                                          masked_only, keep_node_index);
            
            SUMA_COPY_DSETWIDE_ATTRIBUTES(odset, ndset);
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
         SUMA_LH("Insertion finished");
         if (lblcp) SUMA_free(lblcp); lblcp = NULL;
         /* Adding other columnar attributes */
         SUMA_COPY_DSET_COL_ATTRIBUTES(odset, ndset, 
                                       i, SDSET_VECNUM(ndset)-1);
         /* Free ncol, was not done before Apr. 29 2013*/
         SUMA_free(ncol); ncol=NULL;
      } else {
         if (LocalHead) 
            fprintf(SUMA_STDERR,"%s:\nSkipping column %d\n", FuncName, i);
      }
   }
   
   SUMA_RETURN(ndset);
}

/* vertically catenate columns, creating a new dataset*/
SUMA_DSET * SUMA_VcatDset(SUMA_DSET *odset, 
                           byte *rowmask, byte *colmask, 
                           int masked_only, int keep_node_index)
{
   static char FuncName[]={"SUMA_VcatDset"};
   int n_incopy = -1, i=-1;
   char *lblcp=NULL, *new_name, idcode[SUMA_IDCODE_LENGTH];
   SUMA_DSET *ndset=NULL;
   NI_rowtype *rt=NULL;
   SUMA_COL_TYPE ctp = SUMA_ERROR_COL_TYPE;
   void *ncol=NULL;
   int DoThisCol=0, rt_ncol=-1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!odset) { SUMA_SL_Err("Null input"); SUMA_RETURN(NULL); }
   if (!odset->dnel) { SUMA_SL_Err("Null dnel"); SUMA_RETURN(NULL); }
   if (!SUMA_is_AllNumeric_dset(odset)) {
      SUMA_SL_Err("Function does not deal with data sets containing "
                  "non-numeric columns");
      SUMA_RETURN(NULL);
   }
   if (!SUMA_is_AllConsistentColType_dset(odset, SUMA_ERROR_COL_TYPE)) {
      SUMA_S_Err("Must have same column types");
      SUMA_RETURN(NULL);
   }
   if (0 && LocalHead) {
      SUMA_ShowNel((void*)odset->dnel);
   }
   
   for (i=0; i < SDSET_VECNUM(odset); ++i) {
      if (!colmask) DoThisCol = 1;
      else DoThisCol = colmask[i];
      if (DoThisCol) {
         if (LocalHead) fprintf(SUMA_STDERR,"%s:\nProcessing column %d\n", 
                                 FuncName, i);
         ctp = SUMA_TypeOfDsetColNumb(odset, i); 
         rt = NI_rowtype_find_code(SUMA_ColType2TypeCast(ctp)) ; 
         if( rt == NULL || ROWTYPE_is_varsize(rt)) {
            SUMA_SL_Err("Could not recognize rowtype, or rowtype is of "
                        "variable size."); SUMA_RETURN(NULL);
         }
         if (SUMA_IS_DATUM_INDEX_COL(ctp) && keep_node_index && !masked_only) {
            SUMA_S_Err("This should not be anymore. Skipping...");
            /* preserve all rows */
            /* ncol = SUMA_Copy_Part_Column(odset->dnel->vec[i], rt, 
                     SDSET_VECLEN(odset), NULL, masked_only, &n_incopy); */
         } else {
            SUMA_LH("Preserving rows in mask only");
            if (!SUMA_Append_Copy_Part_Column(odset->dnel->vec[i], rt,
                                 SDSET_VECLEN(odset), rowmask, 
                                 masked_only,
                                 &ncol, &rt_ncol, &n_incopy)) {
               SUMA_SL_Err("Failed in vertical catenation");
               SUMA_RETURN(NULL);   
            }
            if (!ncol) {
               SUMA_SL_Err("No data got catenated.");
               SUMA_RETURN(ndset);
            }
         }
      } else {
         if (LocalHead) 
            fprintf(SUMA_STDERR,"%s:\nSkipping column %d\n", FuncName, i);
      }
   }
         
   /* Put the catenated results in here */    
   /* Now create a new dset */   
   new_name = SUMA_append_string( NI_get_attribute(odset->ngr,"filename"),
                                  "copy");
   UNIQ_idcode_fill(idcode); 
   ndset =  SUMA_CreateDsetPointer( 
                              new_name, 
                              SUMA_Dset_Type(NEL_DSET_TYPE(odset->ngr)), 
                              idcode, 
                              NI_get_attribute(odset->ngr,
                              "domain_parent_idcode"),
                              n_incopy ); 
   SUMA_free(new_name); new_name = NULL;
   
   
   /* That's for TR and LabelTable, should be irrelevant for vertical catenation
      SUMA_COPY_DSETWIDE_ATTRIBUTES(odset, ndset); */
   
   /* add the catenated monster*/
   lblcp = SUMA_append_replace_string(
                     SUMA_DsetColLabelCopy(odset, 0, 0),"...","",1);         
   if (!SUMA_AddDsetNelCol (ndset, lblcp, ctp, ncol, NULL ,1)) {
      SUMA_SL_Crit("Failed in SUMA_AddDsetNelCol");
      SUMA_FreeDset((void*)ndset); ndset = NULL;
      SUMA_RETURN(ndset);
   } 
   if (lblcp) SUMA_free(lblcp); lblcp = NULL;
   SUMA_free(ncol); ncol = NULL;
   
   /* copy stat sym (see SUMA_COPY_DSET_COL_ATTRIBUTES but don't go with 
      all that the macro copies */
   {
      char *ATR_LIST[64] = {  "COLMS_STATSYM",  NULL }; 
      if (!SUMA_CopyDsetAttributes (odset, ndset, ATR_LIST, 0, 0)) {   
         SUMA_S_Err("Failed to copy dset attributes");   
      }  
   }

   SUMA_RETURN(ndset);
}

/* A vertically catenating version of SUMA_Copy_Part_Column */
SUMA_Boolean SUMA_Append_Copy_Part_Column(void *col, NI_rowtype *rt, int N_col,
                                    byte *rowmask, int masked_only, 
                                    void **appendhere, int *append_rowtype_code, 
                                    int *n_inappendhere) 
{
   static char FuncName[]={"SUMA_Append_Copy_Part_Column"};
   SUMA_Boolean LocalHead = NOPE;
   int n_incopy=0;
   char *ndat=NULL;
   void *vv=NULL;
   SUMA_ENTRY;
   
   if (!appendhere || !append_rowtype_code || !n_inappendhere) {
      /* foul input */
      SUMA_S_Err("NULL return carriers.");
      SUMA_RETURN(NOPE); 
   }
   if (!ROWTYPE_is_basic_code(rt->code)) {
       SUMA_S_Err("Not good for non-basic codes");
       SUMA_RETURN(NOPE);   
   }
   if (!*appendhere) { /* first time */
      *appendhere = SUMA_Copy_Part_Column(col, rt, N_col, rowmask, 
                                          masked_only, &n_incopy);   
      *append_rowtype_code = rt->code;
      *n_inappendhere = n_incopy;
      SUMA_RETURN(YUP);
   }
   
   /* second time around */
   if (!ROWTYPE_is_basic_code(*append_rowtype_code)) {
      SUMA_S_Err("Bad type on second call, what is this?");
      SUMA_RETURN(NOPE);
   }
   
   if ( *append_rowtype_code != rt->code ) {
      SUMA_S_Err("Type mismatch");
      SUMA_RETURN(NOPE);     
   }
   
   ndat = (char *)SUMA_Copy_Part_Column(col, rt, N_col, rowmask, 
                                        masked_only, &n_incopy);   
   /* OK, now realloc. be careful incrementing (*appendhere), 
      that vector can store variables that are of different size than char */
   *appendhere = (char *)SUMA_realloc(*appendhere, 
                           rt->size * (n_incopy+*n_inappendhere) *sizeof(char));
   memcpy((char*)(*appendhere)+(*n_inappendhere*rt->size/sizeof(char)), 
          ndat, n_incopy*rt->size*sizeof(char));
   SUMA_free(ndat); ndat=NULL;
   *n_inappendhere = *n_inappendhere+n_incopy;
      
   SUMA_RETURN(YUP); 
}

/*!
   see help for SUMA_MaskedCopyofDset
      
*/
void *SUMA_Copy_Part_Column(void *col, NI_rowtype *rt, int N_col, 
                            byte *rowmask, int masked_only, int *n_incopy)
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
         ndat = (char *)SUMA_calloc(rt->size * n_alloc ,sizeof(char)  ) ;
         if (!ndat) { 
            SUMA_SL_Crit("Failed to allocate for ndat"); 
            SUMA_RETURN(NULL); 
         }
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
   
   Now OK for Graph Dsets
*/
int SUMA_FillDsetNelCol (SUMA_DSET *dset, char *col_label, 
                     SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride) 
{  
   static char FuncName[]={"SUMA_FillDsetNelCol"};
   int icol = -1, is_sorted;
   int *iv, N_i;
   
   SUMA_ENTRY;
   
   if (SUMA_IS_DATUM_INDEX_COL(ctp)) {
      SUMA_RETURN(SUMA_FillDsetNelNodeIndexCol (dset, col_label, ctp, 
                                                col, col_attr, stride));
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
         NI_fill_column_stride ( dset->dnel, NI_FLOAT, (float *)col, 
                                 icol, stride );      
         break;
      case SUMA_byte:
         NI_fill_column_stride ( dset->dnel, NI_BYTE, (byte *)col, 
                                 icol, stride );      
         break;
      case SUMA_string:
         NI_fill_column_stride ( dset->dnel, NI_STRING, (char **)col, 
                                 icol, stride );
         break;
      case SUMA_double:
         NI_fill_column_stride ( dset->dnel, NI_DOUBLE, (double *)col, 
                                 icol, stride );
         break;
      case SUMA_complex:
         NI_fill_column_stride ( dset->dnel, NI_COMPLEX, (complex *)col, 
                                 icol, stride );
         break;
      default:
         fprintf  (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   
   /* set some generic attributes */
   SUMA_AddGenDsetColAttr (dset, ctp, col, stride, icol, 0);
   /* add the attributes of that column */
   SUMA_AddDsetColAttr (dset, col_label, ctp, col_attr, icol, 0 );
   
   SUMA_RETURN(1);
}

/* Function now OK for Graph Dsets */
int SUMA_FillDsetNelNodeIndexCol (SUMA_DSET *dset, char *col_label, 
                                  SUMA_COL_TYPE ctp, void *col, 
                                  void *col_attr, int stride) 
{  
   static char FuncName[]={"SUMA_FillDsetNelNodeIndexCol"};
   int is_sorted, *iv=NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->inel) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(0);
   }   
   
   if (!SUMA_IS_DATUM_INDEX_COL(ctp) && !SUMA_IS_MD_DATUM_INDEX_COL(ctp)) {
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
      case SUMA_complex:
         NI_fill_column_stride ( dset->inel, NI_COMPLEX, col, 0, stride );
         break;
      default:
         fprintf  (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   
   if (ctp == SUMA_NODE_INDEX) { /* Nothing to do if SUMA_EDGE_P[12]_INDEX */
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
         SUMA_AddGenDsetColAttr (dset, ctp, col, stride, -1, 0);
         /* add the attributes of that column */
         SUMA_AddDsetColAttr (dset, col_label, ctp, col_attr, -1, 0);
      } else {
         NI_set_attribute(dset->inel, "sorted_node_def", "Unknown");
      }
   } else if (ctp == SUMA_MD_NODE_INDEX) {
      if (col) {
         SUMA_S_Warn("Not ready to determine sorting for MD indices");
         NI_set_attribute(dset->inel, "sorted_node_def", "No");   
         
         /* set some generic attributes */
         SUMA_AddGenDsetColAttr (dset, ctp, col, stride, -1, 0);
         /* add the attributes of that column */
         SUMA_AddDsetColAttr (dset, col_label, ctp, col_attr, -1, 0);
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
int SUMA_FillNelCol (NI_element *nel, char *col_label, SUMA_COL_TYPE ctp, 
                     void *col, 
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
      case SUMA_complex:
         NI_fill_column_stride ( nel, NI_COMPLEX, (complex *)col, icol, stride );
         break;
      default:
         fprintf  (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   
   if (ctp == SUMA_NODE_INDEX) { /* No need for sorting flag for SUMA_EDGE_P[12]_INDEX */
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
   
   if (!vt) SUMA_RETURN(((SUMA_COL_TYPE)SUMA_notypeset));
   
   if (strstr(vt,"int")) SUMA_RETURN(SUMA_NODE_INT);
   if (strstr(vt,"float")) SUMA_RETURN(SUMA_NODE_FLOAT);
   if (strstr(vt,"byte")) SUMA_RETURN(SUMA_NODE_BYTE);
   if (strstr(vt,"double")) SUMA_RETURN(SUMA_NODE_DOUBLE);
   if (strstr(vt,"short")) SUMA_RETURN(SUMA_NODE_SHORT);
   if (strstr(vt,"complex")) SUMA_RETURN(SUMA_NODE_COMPLEX);
   
   SUMA_RETURN(SUMA_ERROR_COL_TYPE);
}
 
SUMA_VARTYPE SUMA_ColType2TypeCast (SUMA_COL_TYPE ctp) 
{
   static char FuncName[]={"SUMA_ColType2TypeCast"};
   
   SUMA_ENTRY;
   
   switch (ctp) {
      case SUMA_NODE_INT:
      case SUMA_NODE_ILABEL:
      case SUMA_EDGE_P1_INDEX:
      case SUMA_EDGE_P2_INDEX:
      case SUMA_GNODE_INDEX:
      case SUMA_GNODE_IGROUP:
      case SUMA_MD_NODE_INDEX:
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
      case SUMA_NODE_ZSCORE:
      case SUMA_NODE_3C:
      case SUMA_NODE_VFR:
      case SUMA_NODE_PHASE:
      case SUMA_NODE_AREA:
      case SUMA_NODE_VOLUME:
      case SUMA_NODE_THICKNESS:
         SUMA_RETURN(SUMA_float);      
         break;
      case SUMA_NODE_BYTE:
      case SUMA_NODE_Rb:
      case SUMA_NODE_Gb:
      case SUMA_NODE_Bb:
      case SUMA_NODE_Ab:
         SUMA_RETURN(SUMA_byte);          
         break;
      case SUMA_NODE_DOUBLE:
         SUMA_RETURN(SUMA_double);          
         break;
      case SUMA_NODE_COMPLEX:
         SUMA_RETURN(SUMA_complex);          
         break;
      case SUMA_NODE_SLABEL:
      case SUMA_NODE_STRING:
         SUMA_RETURN(SUMA_string);
         break;
      case SUMA_NODE_SHORT:
         SUMA_RETURN(SUMA_short);
         break;
      default:
         fprintf  (stderr,"Error %s: Bad column type %d (Max allowed %d)\n", 
                     FuncName, ctp, SUMA_N_COL_TYPES-1);
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
      case SUMA_XML_DSET:
         SUMA_RETURN ("GIFTI");
         break;
      case SUMA_XML_B64_DSET:
         SUMA_RETURN ("GIFTI_Base64");
         break;
      case SUMA_XML_B64GZ_DSET:
         SUMA_RETURN ("GIFTI_Base64_GZIPPED");
         break;
      case SUMA_XML_ASCII_DSET:
         SUMA_RETURN ("GIFTI_ASCII");
         break;
      case SUMA_ASCII_OPEN_DX_DSET:
         SUMA_RETURN ("Ascii_OpenDX_dset");
         break;
      case SUMA_N_DSET_FORMATS:
         SUMA_RETURN("Number_of_formats");
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
   if (!strcmp(Name,"GIFTI")) SUMA_RETURN (SUMA_XML_DSET);
   if (!strcmp(Name,"GIFTI_Base64_GZIPPED")) SUMA_RETURN (SUMA_XML_B64GZ_DSET);
   if (!strcmp(Name,"GIFTI_Base64")) SUMA_RETURN (SUMA_XML_B64_DSET);
   if (!strcmp(Name,"GIFTI_ASCII")) SUMA_RETURN (SUMA_XML_ASCII_DSET);
   if (!strcmp(Name,"Number_of_formats")) SUMA_RETURN (SUMA_N_DSET_FORMATS);
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
      case SUMA_VOXEL_BUCKET:
         SUMA_RETURN("Voxel_Bucket");
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
      case SUMA_ENGINE_INSTRUCTION:
         SUMA_RETURN("EngineInstruction");
         break;   
      case SUMA_SEGMENT_OBJECT:
         SUMA_RETURN("SegmentObject");
         break;
      case SUMA_LABEL_TABLE_OBJECT:
         SUMA_RETURN("LabelTableObject");
         break;   
      case SUMA_NODE_LABEL:
         SUMA_RETURN("Node_Label");
         break;   
      case SUMA_GRAPH_BUCKET:
         SUMA_RETURN("Graph_Bucket");
         break;
      case SUMA_TRACT_BUCKET:
         SUMA_RETURN("Tract_Bucket");
         break;
      case SUMA_CIFTI_BUCKET:
         SUMA_RETURN("CIFTI_Bucket");
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
   if (!strcmp(Name,"Voxel_Bucket")) SUMA_RETURN (SUMA_VOXEL_BUCKET);
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
   if (!strcmp(Name,"SurfaceVolumeParent")) 
      SUMA_RETURN (SUMA_SURFACE_VOLUME_PARENT);
   if (!strcmp(Name,"SurfaceObject")) SUMA_RETURN (SUMA_SURFACE_OBJECT);
   if (!strcmp(Name,"EngineInstruction")) SUMA_RETURN (SUMA_ENGINE_INSTRUCTION);
   if (!strcmp(Name,"SegmentObject")) SUMA_RETURN (SUMA_SEGMENT_OBJECT);
   if (!strcmp(Name,"LabelTableObject")) SUMA_RETURN (SUMA_LABEL_TABLE_OBJECT);
   if (!strcmp(Name,"Node_Label")) SUMA_RETURN (SUMA_NODE_LABEL);
   if (!strcmp(Name,"Tract_Bucket")) SUMA_RETURN (SUMA_TRACT_BUCKET);
   if (!strcmp(Name,"Graph_Bucket")) SUMA_RETURN (SUMA_GRAPH_BUCKET);
   if (!strcmp(Name,"CIFTI_Bucket")) SUMA_RETURN (SUMA_CIFTI_BUCKET);
   
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
      case SUMA_MD_NODE_INDEX:
         SUMA_RETURN("MD_Node_Index");
         break;
      case SUMA_GNODE_INDEX:
         SUMA_RETURN("GNode_Index");
         break;
      case SUMA_GNODE_IGROUP:
         SUMA_RETURN("GNode_Group");
         break;
      case SUMA_EDGE_P1_INDEX:
         SUMA_RETURN("Edge_P1_Index");
         break;
      case SUMA_EDGE_P2_INDEX:
         SUMA_RETURN("Edge_P2_Index");
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
      case SUMA_NODE_A:
         SUMA_RETURN("A_col");
         break;
      case SUMA_NODE_Rb:
         SUMA_RETURN("Rb_col");
         break;
      case SUMA_NODE_Gb:
         SUMA_RETURN("Gb_col");
         break;
      case SUMA_NODE_Bb:
         SUMA_RETURN("Bb_col");
         break;
      case SUMA_NODE_Ab:
         SUMA_RETURN("Ab_col");
         break;
      case SUMA_NODE_SLABEL:
         SUMA_RETURN("Node_String_Label");
         break;
      case SUMA_NODE_STRING:
         SUMA_RETURN("Generic_String");
         break;
      case SUMA_NODE_COMPLEX:
         SUMA_RETURN("Generic_Complex");
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
      case SUMA_NODE_ZSCORE:
         SUMA_RETURN("Z_score");
         break;
      case SUMA_NODE_VFR:
         SUMA_RETURN("VFR");
         break;
      case SUMA_NODE_PHASE:
         SUMA_RETURN("Phase");
         break;
      case SUMA_NODE_AREA:
         SUMA_RETURN("Area");
         break;
      case SUMA_NODE_VOLUME:
         SUMA_RETURN("Volume");
         break;
      case SUMA_NODE_THICKNESS:
         SUMA_RETURN("Thickness");
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
   if (!strcmp(Name,"MD_Node_Index")) SUMA_RETURN (SUMA_MD_NODE_INDEX);
   if (!strcmp(Name,"GNode_Index")) SUMA_RETURN (SUMA_GNODE_INDEX);
   if (!strcmp(Name,"GNode_Group")) SUMA_RETURN (SUMA_GNODE_IGROUP);
   if (!strcmp(Name,"Edge_P1_Index")) SUMA_RETURN (SUMA_EDGE_P1_INDEX);
   if (!strcmp(Name,"Edge_P2_Index")) SUMA_RETURN (SUMA_EDGE_P2_INDEX);
   if (!strcmp(Name,"Node_Index_Label")) SUMA_RETURN (SUMA_NODE_ILABEL);
   if (!strcmp(Name,"Node_String_Label")) SUMA_RETURN (SUMA_NODE_SLABEL);
   if (!strcmp(Name,"Generic_Float")) SUMA_RETURN (SUMA_NODE_FLOAT);
   if (!strcmp(Name,"XYZ_triplets")) SUMA_RETURN (SUMA_NODE_3C);
   if (!strcmp(Name,"X_coord")) SUMA_RETURN (SUMA_NODE_X);
   if (!strcmp(Name,"Y_coord")) SUMA_RETURN (SUMA_NODE_Y);
   if (!strcmp(Name,"Z_coord")) SUMA_RETURN (SUMA_NODE_Z);
   if (!strcmp(Name,"R_col")) SUMA_RETURN (SUMA_NODE_R);
   if (!strcmp(Name,"G_col")) SUMA_RETURN (SUMA_NODE_G);
   if (!strcmp(Name,"B_col")) SUMA_RETURN (SUMA_NODE_B);
   if (!strcmp(Name,"A_col")) SUMA_RETURN (SUMA_NODE_A);
   if (!strcmp(Name,"Rb_col")) SUMA_RETURN (SUMA_NODE_Rb);
   if (!strcmp(Name,"Gb_col")) SUMA_RETURN (SUMA_NODE_Gb);
   if (!strcmp(Name,"Bb_col")) SUMA_RETURN (SUMA_NODE_Bb);
   if (!strcmp(Name,"Ab_col")) SUMA_RETURN (SUMA_NODE_Ab);
   if (!strcmp(Name,"Generic_String")) SUMA_RETURN (SUMA_NODE_STRING);
   if (!strcmp(Name,"Generic_Byte")) SUMA_RETURN (SUMA_NODE_BYTE);
   if (!strcmp(Name,"Generic_Double")) SUMA_RETURN (SUMA_NODE_DOUBLE);
   if (!strcmp(Name,"Generic_Complex")) SUMA_RETURN (SUMA_NODE_COMPLEX);
   if (!strcmp(Name,"Convexity")) SUMA_RETURN (SUMA_NODE_CX);
   if (!strcmp(Name,"Cross_Corr_Coeff")) SUMA_RETURN (SUMA_NODE_XCORR);
   if (!strcmp(Name,"Z_score")) SUMA_RETURN (SUMA_NODE_ZSCORE);
   if (!strcmp(Name,"VFR")) SUMA_RETURN (SUMA_NODE_VFR);
   if (!strcmp(Name,"Phase")) SUMA_RETURN (SUMA_NODE_PHASE);
   if (!strcmp(Name,"Area")) SUMA_RETURN (SUMA_NODE_AREA); 
   if (!strcmp(Name,"Volume")) SUMA_RETURN (SUMA_NODE_VOLUME); 
   if (!strcmp(Name,"Thickness")) SUMA_RETURN (SUMA_NODE_THICKNESS); 
   /* if (!strcmp(Name,"")) SUMA_RETURN (); */
   SUMA_RETURN (SUMA_ERROR_COL_TYPE);

}


int SUMA_ShowNel (void *nel)
{
   static char FuncName[]={"SUMA_ShowNel"};
   NI_stream nstdout;
   NI_element *el=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) SUMA_DUMP_TRACE("Who just called ShowNel?");
   
   nstdout = NI_stream_open( "fd:1","w");
   if( nstdout == NULL ){ 
      fprintf(stderr,"%s: Can't open fd:1\n", FuncName); 
      SUMA_RETURN(0); 
   }
   if (!nel) {
      fprintf (stdout, "\n***********NULL nel  ************\n");
      SUMA_RETURN(0); 
   }
   fprintf (stdout, "\n***********nel extra info ************\n");
   el = (NI_element *)nel;
   if (el->type == NI_ELEMENT_TYPE) {
      fprintf (stdout,  "\n    Element type.\n"
                        "      vec_len   = %d\n"
                        "      vec_num   = %d\n"
                        "      vec_filled= %d\n", 
                        el->vec_len, el->vec_num, el->vec_filled);
   } else {
      fprintf (stdout,  "\n    Group type.\n");
   } 
   fprintf (stdout, "\n***********nel stdout begin***********\n");
   NI_write_element( nstdout , nel , NI_TEXT_MODE ) ;
   fprintf (stdout, "\n***********nel stdout end  ***********\n");
   NI_stream_close(nstdout);
   
   SUMA_RETURN(1);
}

/* A generic function to write a nel to a string */
char *SUMA_NI_nel_Info (NI_element *nel, int detail)
{
   static char FuncName[]={"SUMA_NI_nel_Info"};
   NI_stream ns=NULL;
   char *s=NULL;
   
   SUMA_ENTRY;
   
   if (!nel) s = SUMA_copy_string("NULL nel");
   else {
      ns = NI_stream_open( "str:" , "w" ) ;
      (void) NI_write_element( ns , nel , NI_TEXT_MODE&NI_HEADERONLY_FLAG) ;
      s = SUMA_copy_string( NI_stream_getbuf(ns) ) ;
      NI_stream_close( ns ) ;
   }
   
   SUMA_RETURN(s);
}



int *SUMA_GetDsetColIndex (SUMA_DSET *dset, SUMA_COL_TYPE tp, int *N_i)
{
   static char FuncName[]={"SUMA_GetDsetColIndex"};
   int *iv=NULL, i=0;
   char stmp[500], *atr;
   int ctp;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (SUMA_IS_DATUM_INDEX_COL(tp)) {
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
   char  *stmp=NULL, *ch=NULL, *chold = NULL, 
         *cdate=NULL, *cname=NULL, *cuser=NULL, *cn = NULL;
   int idate , iname , iuser ;
   int N_tot, i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!arg) SUMA_RETURN(NULL);
   if (!arg[0]) SUMA_RETURN(NULL);
   if (!N_arg) SUMA_RETURN(NULL);
   
      /* Based on tross_Make_History and tross_Append_History */
      if (LocalHead) 
         fprintf(SUMA_STDERR,"CF: %s, N_arg=%d\n", CallingFunc, N_arg);
      cn = tross_commandline( CallingFunc, N_arg, arg);
      if (LocalHead) fprintf(SUMA_STDERR,"cn: %s\n", cn);
      cdate = tross_datetime() ; idate = strlen(cdate) ;
      cname = tross_hostname() ; iname = strlen(cname) ;  
      cuser = tross_username() ; iuser = strlen(cuser) ;  
      if (sold) { /* sold is already trossized */
         chold = tross_Expand_String(sold) ; 
         if( chold == NULL ) SUMA_RETURN(NULL) ;
         chold = AFREALL( chold, char, 
		               strlen(chold)+idate+iuser+iname+strlen(cn)+12 ) ;

         strcat(chold,"\n") ;
         strcat(chold,"[") ;  strcat(chold,cuser) ; strcat(chold,"@") ;
                              strcat(chold,cname) ; strcat(chold,": ") ;
                              strcat(chold,cdate) ;
         strcat(chold,"] ") ;
         strcat(chold,cn) ;
         SUMA_LH("%s",chold);
         ch = tross_Encode_String(chold) ; 
         if( ch == NULL ){ free(chold); SUMA_RETURN(NULL); }
         stmp = SUMA_copy_string(ch);
         free(chold) ; free(ch);
      } else {
         chold = AFMALL(char, idate+iuser+iname+strlen(cn)+12 ) ;
         sprintf(chold,"[%s@%s: %s] %s",cuser,cname,cdate,cn) ;
         SUMA_LH("%s",chold);
         ch = tross_Encode_String(chold) ; 
         if( ch == NULL ){ free(chold); SUMA_RETURN(NULL); }
         stmp = SUMA_copy_string(ch);
         free(chold) ;  free(ch); 
         free(cuser); free(cname); free(cdate);
      }

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

int SUMA_RemoveDsetHist(SUMA_DSET *dset) {
   if (!dset||!dset->ngr) return(0);
   return(SUMA_RemoveNgrHist(dset->ngr));
}

int SUMA_RemoveNgrHist(NI_group *ngr)
{
   static char FuncName[]={"SUMA_RemoveNgrHist"}; 
   NI_element *nelb = NULL;
   
   SUMA_ENTRY;
   
   if (!ngr) SUMA_RETURN(0);
   
   /* get former history element, if any and old string */
   nelb = SUMA_FindNgrAttributeElement(ngr, "HISTORY_NOTE");
   if (nelb) {
      NI_remove_from_group(ngr, nelb);
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
   SUMA_DSET *dset = SUMA_FindDset_eng (idcode, DsetList, NULL, NULL);
   WorkErrLog_ns();
   return(dset);
}
SUMA_DSET * SUMA_FindDset2_ns (char *idcode, DList *DsetList, char *itype)
{
   SUMA_DSET *dset = SUMA_FindDset_eng (idcode, DsetList, NULL, itype);
   WorkErrLog_ns();
   return(dset);
}

DListElmt * SUMA_FindDsetEl_ns (char *idcode, DList *DsetList)
{
   DListElmt *el=NULL;
   SUMA_DSET *dset = SUMA_FindDset_eng (idcode, DsetList, &el, NULL);
   WorkErrLog_ns();
   return(el);
}

SUMA_DSET * SUMA_FindDset_eng (char *idcode, DList *DsetList, 
                               DListElmt **elp, char *idtype)
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
   if (elp) *elp=NULL;
   el = NULL;
   do { 
      if (LocalHead) 
         fprintf(SUMA_STDOUT,
               "%s: \n      checking element out of %d for %s, idtype =%s\n", 
               FuncName, dlist_size(DsetList), idcode, idtype?idtype:"NULL");
      if (!el) el = dlist_head(DsetList);
      else el = dlist_next(el);
      dset = (SUMA_DSET *)el->data;
      if (!dset) {
        SUMA_LH("no dset in el->data!");
        SUMA_PushErrLog("SLP_Err", 
                        "Unexpected NULL dset element in list!\n"
                        "Please report this occurrence to saadz@mail.nih.gov.",
                        FuncName);
      } else {   
         SUMA_LH("Have dset in el->data, checking");
         #ifdef OLD_DSET      /* before dsets were NI_groups */
         if (idtype) {
            SUMA_S_Err("non-null idtype search not supported with old dsets");
            SUMA_RETURN(dsetf);
         }
         if (dset->nel) {
            SUMA_LH("dset is nel type");
            dsetid = NI_get_attribute(dset->nel, "idcode"); /* obsolete */
            if (!dsetid) dsetid = NI_get_attribute(dset->nel, "self_idcode");
            if (dsetid) {
               if (!strcmp(dsetid, idcode))  {/* match */
                  dsetf = dset; 
                  if (elp) *elp=el;
               }
            } 
         }
         #else 
         if (dset->ngr) {
            SUMA_LH("dset is ngr type, ID=%s, Label=%s, user idtype is %s", 
	            SDSET_ID(dset), SDSET_LABEL(dset), CNS(idtype));
            if (!idtype || !strcmp(idtype,"self_idcode")) {
               dsetid = NI_get_attribute(dset->ngr, "idcode"); /* obsolete */
               if (!dsetid) dsetid = NI_get_attribute(dset->ngr, "self_idcode");
               if (dsetid) {
                  if (!strcmp(dsetid, idcode))  { /* match */
                     dsetf = dset;
                     if (elp) *elp=el;
                  }
               }
            } else if (idtype) {
               if (!strcmp(idtype,"filename")) {
                  if (!strcmp(CHECK_NULL_STR(SDSET_FILENAME(dset)), idcode))  { 
                        dsetf = dset;
                        if (elp) *elp=el;
                  }
               } else if (!strcmp(idtype,"label")) {
                  if (!strcmp(CHECK_NULL_STR(SDSET_LABEL(dset)), idcode))  { 
                        dsetf = dset;
                        if (elp) *elp=el;
                  }
               } else if (!strcmp(idtype,"isGraphDset")) {
                  if (SUMA_isGraphDset(dset)) {
                     dsetf = dset;
                     if (elp) *elp=el;
                  }
               }
            }
         }
         #endif
      }
      if (LocalHead) fprintf(SUMA_STDOUT,"%s: About to loopback:\n"
                                          "el=%p, tail=%p, dsetf=%p, idtype=%s\n"
                                          , FuncName, el, dlist_tail(DsetList), 
                                          dsetf, idtype?idtype:"NULL");
   } while ( (el != dlist_tail(DsetList)) && !dsetf ); 
   
   SUMA_LHv("dsetf = %p, returning.\n\n", dsetf);
   
   SUMA_RETURN(dsetf);
}

#define CHECK_DUPS {\
   if (dsetf && dsetf != dset) { \
      fprintf (SUMA_STDERR,"Error %s:\n"  \
                           "More than one dataset found satisfying"  \
                           "criteria: %s\n",    \
                            FuncName, criteria);   \
      SUMA_ShowDset(dset, 0, NULL); \
      SUMA_ShowDset(dsetf, 0, NULL);   \
      SUMA_RETURN(NULL);   \
   }  \
                  }\

SUMA_DSET * SUMA_FindDsetLoose (
               SUMA_DSET *dsetin, 
               DList *DsetList, 
               char *criteria)
{
   static char FuncName[]={"SUMA_FindDsetLoose"};
   SUMA_DSET *dsetf = NULL, *dset= NULL;
   int ifound=0, newmatch, totmatch, icrit;
   char *dsetid=NULL, *s=NULL;
   DListElmt *el=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
    
   dsetf = NULL;
   if (!DsetList) { SUMA_SL_Err("NULL DsetList"); SUMA_RETURN(dsetf); }
   if (!DsetList->size) { SUMA_RETURN(dsetf); }
   if (!dsetin) { SUMA_SL_Err("NULL dset"); SUMA_RETURN(dsetf); }
   totmatch = 0;
   do { 
      if (LocalHead) fprintf( SUMA_STDOUT,
                              "%s: \n      checking element out of %d\n", 
                              FuncName, dlist_size(DsetList));
      if (!el) el = dlist_head(DsetList);
      else el = dlist_next(el);
      dset = (SUMA_DSET *)el->data;
      if (!dset) {
        SUMA_LH("dset not found");
        SUMA_SL_Err( "Unexpected NULL dset element in list!\n"
                     "Please report this occurrence to saadz@mail.nih.gov." );
      } else {   
         SUMA_LH("dset found");
         
         newmatch = 0; icrit = 0;
         while (  !newmatch && 
                  (s = SUMA_NI_get_ith_string( criteria , "|", icrit ))) {
            if (!strcmp(s,"self_idcode")) { 
               #ifdef OLD_DSET      /* before dsets were NI_groups */
               if (dset->nel) {
                  SUMA_LH("dset is nel type");
                  dsetid = NI_get_attribute(dset->nel, "idcode"); /* obsolete */
                  if (!dsetid) 
                     dsetid = NI_get_attribute(dset->nel, "self_idcode");
                  if (dsetid) {
                     if (!strcmp(dsetid, SDSET_ID(dsetin)))  {/* match */
                        CHECK_DUPS;
                        dsetf = dset; 
                        newmatch = 1; ++totmatch;
                     }
                  } 
               }
               #else 
               if (dset->ngr) {
                  SUMA_LH("dset is ngr type");
                  dsetid = NI_get_attribute(dset->ngr, "idcode"); /* obsolete */
                  if (!dsetid) 
                     dsetid = NI_get_attribute(dset->ngr, "self_idcode");
                  if (dsetid) {
                     if (!strcmp(dsetid, SDSET_ID(dsetin)))  { /* match */
                        CHECK_DUPS;
                        dsetf = dset;
                        newmatch = 1; ++totmatch;
                     }
                  } 
               }
               #endif
            } else if (!strcmp(s,"filename")) { 
               #ifdef OLD_DSET      /* before dsets were NI_groups */
               if (dset->nel) {
                  SUMA_LH("dset is nel type");
                  dsetid = SDSET_FILENAME(dset);
                  if (dsetid) {
                     if (!strcmp(dsetid, SDSET_FILENAME(dsetin)))  { /* match */
                        CHECK_DUPS;
                        dsetf = dset;
                        newmatch = 1; ++totmatch;
                     }
                  } 
               }
               #else 
               if (dset->ngr) {
                  SUMA_LH("dset is ngr type");
                  dsetid = SDSET_FILENAME(dset);
                  if (dsetid) {
                     if (!strcmp(   dsetid,
                                    CHECK_NULL_STR(SDSET_FILENAME(dsetin))))  {
                                    /* match */
                        CHECK_DUPS;
                        dsetf = dset;
                        newmatch = 1; ++totmatch;
                     }
                  } 
               } 
               #endif
            }else if (!strcmp(s,"label")) { 
               #ifdef OLD_DSET      /* before dsets were NI_groups */
               SUMA_S_Err("Search by label not supported for old dsets");
               #else 
               if (dset->ngr) {
                  SUMA_LH("dset is ngr type");
                  dsetid = SDSET_LABEL(dset);
                  if (dsetid) {
                     if (!strcmp(   dsetid,
                                    CHECK_NULL_STR(SDSET_LABEL(dsetin))))  {
                                    /* match */
                        CHECK_DUPS;
                        dsetf = dset;
                        newmatch = 1; ++totmatch;
                     }
                  } 
               } 
               #endif
            }  else {
               SUMA_S_Err("Bad Aux.");
            }
            ++icrit;
         } 
      }
      if (LocalHead) fprintf(SUMA_STDOUT,"%s: About to loopback:\n"
                                          "el=%p, tail=%p, dsetf=%p\n"
                                          , FuncName, el, 
                                          dlist_tail(DsetList), dsetf);
   } while ( (el != dlist_tail(DsetList))); 
   SUMA_RETURN(dsetf);
}

/*!
   \brief Function to free a Dset 
   
   - YOU SHOULD NOT FREE individual dsets yourself if they are inserted 
   into DsetList, that is done by the dlist_destroy function
*/
void SUMA_FreeDset(void *vp)
{
   static char FuncName[]={"SUMA_FreeDset"};
   SUMA_DSET *dset;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   dset = (SUMA_DSET *)vp;
   
   if (!dset) SUMA_RETURNe;
   SUMA_LH("Check Link");
   if (dset->N_links) {
      SUMA_SL_Err("dset structure has links to it.\n"
                  "structure not freed.\n"
                  "That is a now a memory leak.\n");
      SUMA_ShowDset (dset, 0, NULL);            
      SUMA_RETURNe;
   }
   
   if (SUMA_isCIFTIDset(dset)) {
      SUMA_S_Warn("Note that objects defining domain of the dataset\n"
                  "being deleted are not being freed\n");
   }
   if (!SUMA_FreeDsetContent(dset)) {
      SUMA_S_Err("Failed to free content, proceeding, "
                 "but danger danger Will Robinson!");
   }
   
   SUMA_LH("The last straw");
   SUMA_free(dset); dset = NULL;
   
   SUMA_RETURNe;
}

SUMA_Boolean SUMA_FreeDsetContent (SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_FreeDsetContent"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   #ifdef OLD_DSET /* before ngr was used */
   SUMA_LH("Old dset");
   if (dset->nel) 
      NI_free_element(dset->nel); dset->nel = NULL; 
         /* you can keep ni_free from freeing a nel->vec[i] 
         vector by copying nel->vec[i] to a pointer then
         setting nel->vec[i] = NULL */ 
   #else
   SUMA_LH("New Treatment");
   dset->dnel = NULL; /* this one was a pointer copy to an element inside ngr */
   dset->inel = NULL;
   dset->pdnel = dset->pinel = NULL; 
   if (dset->ngr)
      SUMA_LH("Freeing NGR"); 
      NI_free_element(dset->ngr); dset->ngr = NULL; 
         /* you can keep ni_free from freeing a nel->vec[i] 
         vector by copying nel->vec[i] to a pointer then
         setting nel->vec[i] = NULL */ 
   #endif

   if (dset->Aux) { /* free auxiliary data */
      if (dset->Aux->Saux) {
         if (!dset->Aux->FreeSaux) {
            SUMA_S_Err("You're leaky, you're leaky");
         } else dset->Aux->FreeSaux(dset->Aux->Saux);
         dset->Aux->Saux =NULL; /* pointer freed in freeing function */
      }
      SUMA_CIFTI_Free_Doms(dset);
      SUMA_ifree(dset->Aux);
   }
   
   SUMA_RETURN(YUP);
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
                             "LinkedPtrType = %d, owner_id = %s, do_type = %d\n"
                                      "N_links was %d\n", 
                                      FuncName, dset, 
                             dset->LinkedPtrType, dset->owner_id, dset->do_type, 
                                      dset->N_links);
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
   if (LocalHead) 
      fprintf(SUMA_STDERR, "%s:\n Unlink Requested from pointer %p.\n"
                           "LinkedPtrType = %d, owner_id = %s, do_type = %d\n"
                           "N_links was %d\n", 
                           FuncName, dset, 
                           dset->LinkedPtrType, dset->owner_id, dset->do_type, 
                           dset->N_links);
   if (dset->N_links > 0) dset->N_links = dset->N_links - 1;
   else if (dset->N_links == 0) { 
      SUMA_SL_Err("N_links ==0\nThis should not happen here.\n");   
      SUMA_RETURN(NULL); 
   }
   
   SUMA_RETURN(NULL);
}

SUMA_DSET * SUMA_NewDsetPointer(void)
{
   static char FuncName[]={"SUMA_NewDsetPointer"};
   SUMA_DSET *dset = NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;

   dset = (SUMA_DSET *)SUMA_calloc(1,sizeof(SUMA_DSET));
   if (!dset) {
      SUMA_SL_Err("Failed to allocate for dset");
      SUMA_RETURN(dset);
   }
   if (LocalHead) 
      fprintf(SUMA_STDERR,"%s:\n dset %p allocated.\n", FuncName, dset);
   /* initialize */
   #ifdef OLD_DSET
   dset->nel = NULL;
   #else
   dset->inel = NULL;
   dset->dnel = NULL;
   dset->pdnel = dset->pinel = NULL;
   dset->ngr = NULL;
   #endif
   dset->N_links = 0;
   dset->owner_id[0] = '\0';
   dset->LinkedPtrType = SUMA_LINKED_DSET_TYPE;
   dset->Aux = NULL;
   dset->do_type = ANY_DSET_type; 
   SUMA_RETURN(dset);
}

/* Change a dataset with one integer valued column to a 
  Label type dset. Note that dset itself is modified.
*/
int SUMA_dset_to_Label_dset(SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_dset_to_Label_dset"};
   int ctp, vtp, i, *unq=NULL, N_unq=0;
   NI_group *NIcmap=NULL, *ngr=NULL;
   char stmp[256], *lbli=NULL, *attname=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->dnel || !dset->inel) SUMA_RETURN(0);
   
   if (!SUMA_is_AllConsistentCastType_dset(dset, SUMA_int)) { 
      SUMA_LH("Not all columns are type castable to SUMA_int");
      SUMA_RETURN(0); 
   }
   
   if (SUMA_is_Label_dset(dset, NULL)) { 
      SUMA_LH("is label already");
   } else {
      /* Must have one column that is an integer value */
      for (i=0; i<1; ++i) {
         ctp = SUMA_TypeOfDsetColNumb(dset, i); 
         if (LocalHead) {
            fprintf(SUMA_STDERR,"%s: ctp(%d) = %d (%d)\n",
                  FuncName, i, ctp, SUMA_NODE_ILABEL);
         }
         if (ctp != SUMA_NODE_ILABEL) {
            if (SUMA_ColType2TypeCast(ctp) != SUMA_int) {
               SUMA_S_Err( "Cannot make the change. "
                           "Only integer columns supported");
               SUMA_RETURN(0);
            }
            /* change the column type */
            lbli = SUMA_DsetColLabelCopy(dset, i, 0);
            if (!SUMA_AddDsetColAttr ( dset, lbli , 
                                 SUMA_NODE_ILABEL, NULL, 
                                 i, 1)) {
               SUMA_S_Err("Failed chaning attribute");
               SUMA_RETURN(0);
            }
            if (lbli) SUMA_free(lbli); lbli = NULL;
         }
      }
      /* and dset_type */
      NI_set_attribute( dset->ngr,"dset_type", 
                        SUMA_Dset_Type_Name(SUMA_NODE_LABEL));
      attname = SUMA_append_string(SDSET_TYPE_NAME(dset),"_data");
      NI_set_attribute( dset->dnel,"data_type",
                        attname);
      SUMA_free(attname); attname = NULL;
      attname = SUMA_append_string(SDSET_TYPE_NAME(dset),"_node_indices");
      NI_set_attribute( dset->inel,"data_type",
                        attname);
      SUMA_free(attname); attname = NULL;
   }
      
   SUMA_RETURN(1);
}

SUMA_Boolean SUMA_Reset_NodeIndex_Element(SUMA_DSET *dset, NI_element **inel) 
{
   static char FuncName[]={"SUMA_Reset_NodeIndex_Element"};
   char *dname=NULL;
   
   SUMA_ENTRY;
   
   
   if (!dset) SUMA_RETURN(NOPE);

   if (SUMA_isGraphDset(dset)) {
      if (inel && *inel) {
         if ((*inel)->vec_num != 3) {
            SUMA_S_Errv("You failed the basic test of 3, "
                        "bad inel with %d cols\n",
                     (*inel)->vec_num);
            SUMA_RETURN(NOPE);
         }
      }
      dname = SUMA_append_string(
            NEL_DSET_TYPE(dset->ngr), "_edge_indices");
   } else {
      if (inel && *inel) {
         if ((*inel)->vec_num != 1) {
            SUMA_S_Errv("You failed the basic test of 1, "
                        "bad inel with %d cols\n",
                     (*inel)->vec_num);
            SUMA_RETURN(NOPE);
         }
      }
      dname = SUMA_append_string(
            NEL_DSET_TYPE(dset->ngr), "_node_indices");
   }
   if (dset->inel) {
      NI_remove_from_group(dset->ngr, dset->inel);
      NI_free_element(dset->inel); dset->inel = NULL;
   }

   if (!inel || !*inel) {
      dset->inel = NI_new_data_element("INDEX_LIST", SDSET_VECLEN(dset)); 
   } else {
      dset->inel = *inel; *inel = NULL;
   }
   
   NI_set_attribute (dset->inel, "data_type", dname); 
   SUMA_free(dname); dname = NULL;
   NI_add_to_group(dset->ngr, dset->inel);
           
   SUMA_RETURN(YUP);
   
}

/* Create/initialize Auxiliary structure */
SUMA_Boolean SUMA_Add_Dset_Aux(SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_Add_Dset_Aux"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr) {
      SUMA_S_Err("Have nothing to work with");
      SUMA_RETURN(NOPE);
   }

   /* SUMA_DUMP_TRACE("Auxing\n"); */

   if (dset->Aux) {
      SUMA_LH("Have Aux already, nothing done");
      SUMA_RETURN(YUP);
   }  
   /* Is this a gaph dset? */
   if (SUMA_isGraphDsetNgr(dset->ngr)) {
      dset->Aux = (SUMA_DSET_AUX *)SUMA_calloc(1,sizeof(SUMA_DSET_AUX));
      dset->Aux->matrix_shape = MAT_HEEHAW;
      dset->Aux->isGraph = GRAPH_DSET;
   } else if (SUMA_isTractDsetNgr(dset->ngr)) {
      dset->Aux = (SUMA_DSET_AUX *)SUMA_calloc(1,sizeof(SUMA_DSET_AUX));
      dset->Aux->matrix_shape = MAT_NA;
      dset->Aux->isGraph = TRACT_DSET;
   } else if (SUMA_isCIFTIDsetNgr(dset->ngr)) {
      dset->Aux = (SUMA_DSET_AUX *)SUMA_calloc(1,sizeof(SUMA_DSET_AUX));
      dset->Aux->matrix_shape = MAT_NA;
      dset->Aux->isGraph = CIFTI_DSET;
   } else { /* you should always have Aux allocated, default to surf_dset...*/
      dset->Aux = (SUMA_DSET_AUX *)SUMA_calloc(1,sizeof(SUMA_DSET_AUX));
      dset->Aux->isGraph = SURF_DSET;
      dset->Aux->matrix_shape = MAT_NA;
   }

   SUMA_RETURN(YUP);
}

/*!
   \brief a function to put a ni_group (ngr) element into a SUMA_DSET pointer
   To free dset along with the ngr in it use SUMA_FreeDset.
   To preserve ngr:
      ngr_keep = dset->ngr; dset->ngr = NULL; SUMA_Free(dset);
*/
SUMA_DSET * SUMA_ngr_2_dset(NI_group *nini, int warn)
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
   dset->inel = SUMA_FindDsetDatumIndexElement(dset);
   if (!dset->dnel) {
      SUMA_SL_Warn("Failed to find dset data element");
   }
   if (!dset->inel || !SDSET_NODEINDNUM(dset)) {
      /* If a dset had an empty node index holder at write time,
      the inel is written with vecnum set to 0. This would make
      any attempt to add columns later on fail.
      */
      if (warn && !SUMA_isGraphDsetNgr(dset->ngr)) {
         SUMA_S_Note("NIML dset with no valid node index element");
      }
      NI_remove_from_group(dset->ngr, dset->inel);
      NI_free_element(dset->inel); dset->inel = NULL;
      /* Now add the new and proper node index element holder*/
      if (dset->dnel) {
         if (warn && !SUMA_isGraphDsetNgr(dset->ngr)) {
            SUMA_S_Note("Adding empty holder\n");
         }
         SUMA_Reset_NodeIndex_Element(dset, NULL);
      }
   }
   
   /* Got a label table ? */
   if (SUMA_NI_Cmap_of_Dset(dset)) { /* make sure it is a label dset then */
      if (!SUMA_dset_to_Label_dset(dset)) {
         SUMA_S_Err("Failed to turn dset into a labeled one.");
      }  
   }
   
   /* Set Aux (could have been set already with earlier calls 
      via SUMA_isGraphDset() ) */
   if (!dset->Aux && !SUMA_Add_Dset_Aux(dset)) {
      SUMA_S_Err("Failed to add Aux");
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
   
   if (!dset) {
      SUMA_S_Err("NULL Input");
      SUMA_RETURN(NOPE);
   }
   ok = YUP; 
   if (lbl) {
      Label = SUMA_truncate_string(lbl, 20);
      NI_set_attribute(dset->ngr, "label", Label);  
   } else if ( (tmp = NI_get_attribute(dset->ngr,"filename")) ) {
      pn = SUMA_ParseFname(tmp, NULL);
      if (pn) {
         Label = SUMA_truncate_string(pn->FileName_NoExt, 20);
         SUMA_Free_Parsed_Name(pn); pn = NULL;
         NI_set_attribute(dset->ngr, "label", Label);  
      } else {
         NI_set_attribute(dset->ngr, "label", "Bad No label"); 
         ok = NOPE;
      }
   } else {
      NI_set_attribute(dset->ngr, "label", "No label"); 
      ok = NOPE;
   }
   if (Label) SUMA_free(Label); Label = NULL;
    
   SUMA_RETURN(ok);
}   

/*!
   Rename a dataset and take care of idcode and relabeling, if necessary 
*/
SUMA_Boolean SUMA_RenameDset(SUMA_DSET *dset, char *filename, int autoid) 
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
   
   if (autoid) {
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
   }
   
   /* what about the label ? */
   if ((olabel = SDSET_LABEL(dset))) {
      /* have label already */
      if (strstr(ofname, olabel)) { 
         /* old label was based on old file name, relabel with new filename */
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
   \sa SUMA_CreateFullDsetPointer
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
     
   if (!idcode) { /* No id is given yet */
      if (filename) {
         SUMA_NEW_ID(locid, filename);   /* form one from the filename */
         if (LocalHead) 
            fprintf(SUMA_STDERR,
               "%s: Using filename %s to create IDcode %s.\n", 
               FuncName, filename, locid); 
      } else { 
         SUMA_NEW_ID(locid, NULL); 
      } 
   }else {
      locid = SUMA_copy_string(idcode);
   }
   
   dset = SUMA_NewDsetPointer();
   if (!SUMA_NewDsetGrp (dset, tp, domain_idcode, domain_idcode, 
                         N_Alloc, 0, filename, locid)) {
      SUMA_SL_Crit("Failed to create dset.\n");
      SUMA_RETURN(0);
   }

   SUMA_LabelDset(dset, filename);
   
   if (locid) SUMA_free(locid); locid = NULL;
   SUMA_RETURN(dset);
}    

/*!
   An easier way to create a full dset, complete with node index column
*/
SUMA_DSET * SUMA_CreateFullDsetPointer (  
                              char *filename, SUMA_DSET_TYPE tp,
                              char *idcode,
                              char *domain_idcode,
                              int N_Alloc ) 
{
   static char FuncName[]={"SUMA_CreateFullDsetPointer"};
   SUMA_DSET *dset=NULL;
   int ii, *col=NULL;
   
   SUMA_ENTRY;
   
   dset = SUMA_CreateDsetPointer (filename, tp, idcode, domain_idcode,N_Alloc);
   if (!dset) SUMA_RETURN(dset);
   if (!N_Alloc) SUMA_RETURN(dset);
   
   if (!(col = (int*)SUMA_malloc(sizeof(int)*N_Alloc))) {
      SUMA_S_Err("Failed to allocate node index column");
      SUMA_FreeDset(dset); dset=NULL;
   } else {
      for (ii=0; ii<N_Alloc; ++ii) col[ii]=ii;
      if (tp == SUMA_CIFTI_BUCKET) {
         if (!SUMA_AddDsetNelCol ( dset, "MD Node Index", 
                                   SUMA_MD_NODE_INDEX, (void *)col, NULL, 1)) {
            SUMA_S_Err("Failed to add MD node index column");
            SUMA_FreeDset(dset); dset=NULL;
         }
      } else {
         if (!SUMA_AddDsetNelCol ( dset, "Node Index", 
                                   SUMA_NODE_INDEX, (void *)col, NULL, 1)) {
            SUMA_S_Err("Failed to add node index column");
            SUMA_FreeDset(dset); dset=NULL;
         }
      }
   }
   
   if (col) SUMA_free(col); col = NULL;
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
   char *s=NULL, stmp[200]={"uninitialized"};
   SUMA_DSET *dprev=NULL, *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("About to insert dset pointer %p", dsetp ? *dsetp:NULL);
   if (!DsetList)  { SUMA_SL_Err("Need Dset List"); SUMA_RETURN(0); }
   if (!dsetp) { SUMA_SL_Err("dsetp is NULL"); SUMA_RETURN(0); }
   else dset = *dsetp;  /* dset is the new pointer */
   
   if (SUMA_isCIFTIDset(dset) && !dset->dnel) {
      SUMA_S_Warn("As things stand, dset->dnel, and other goodies might be NULL"
      	          "and would cause an error in this function below.\n"
		  "For now I will do nothing and return a positive result\n"
		  "depite having done nothing.\n"
		  "I am pretty sure we will want the MD dset present in the\n"
		  "DsetList however.");
      SUMA_RETURN(1);
   }
   if (!dset->dnel) { 
      SUMA_SL_Err("dset->nel is NULL\nNothing to do"); 
      SUMA_RETURN(0); 
   }

   if (!(s= SDSET_ID(dset))) { 
      SUMA_SL_Err("dset has no idcode.\n"); 
      SUMA_RETURN(0); 
   }
   
   /* check if surface read was unique 
   it's inefficient to check after the surface is read, 
   but idcode is generated in the read routine 
   and users should not be making this mistake too often 
   Search for similar comment elsewhere in the code once
   a better remedy is found*/
   if ((dprev = SUMA_FindDset_ns (s,  DsetList))) {
      /* Check the filename match, to get around ID collision 
         This modification is no guarantee that collisions
         won't occur but it is a start until I figure out
         the problem with hashcode */
      char *name=NULL, *mname=NULL;
      SUMA_LH("Hash code collision of (%s) with dset %p (%s)", 
               SDSET_LABEL(dset), dprev, SDSET_LABEL(dprev));
      if (!(mname = SDSET_FILENAME(dprev))) mname = "NULLITY";
      if (!(name = SDSET_FILENAME(dset))) name = "NULLITY";
      if (name && mname && strcmp(name, mname)) {
         char *stimpy;
         /* give dset a new ID */
         stimpy = SUMA_append_replace_string(name, SDSET_ID(dset),"_",0);
         SUMA_NewDsetID2(dset, stimpy);
         SUMA_ifree(stimpy);
         s= SDSET_ID(dset);
      }
      dprev=NULL;
   }

   if ((dprev = SUMA_FindDset_ns (s,  DsetList))) {
      snprintf(stmp, 198, "Dset %s has similar idcode (%s) in list as \n"
                     "dset %s. Trying replacement.\n", 
                     SUMA_sdset_filename(dset), s, SUMA_sdset_filename(dprev));
   } else if (!dprev && replace) { /* try a looser search */
      SUMA_LH("ID match not found, trying filename match");
      dprev = SUMA_FindDsetLoose(dset, DsetList,"filename");
      if (dprev) {
         if (!AFNI_yesenv("SUMA_AllowFilenameDsetMatch")) {
            SUMA_S_Warn("Attempting to load a new dataset with a unique ID\n"
                        "and a pre-existing name in the list.\n"
                        "This causes trouble unless you are willing to \n"
                        "replace pre-existing dataset of the same name.\n"
                        "If so, you need to set the environment variable: \n"
                        "SUMA_AllowFilenameDsetMatch = YES\n"
                        "in your ~/.sumarc or ~/.afnirc files.\n");
             SUMA_RETURN(0);
         } else {
            snprintf(stmp, 198,  
                  "Dset match based on filename, although IDs did not match.\n"
                  "Allowing replacement per SUMA_AllowFilenameDsetMatch\n"
                  "environment variable setting.\n");
         } 
      } 
   } 
   
   
   
   if (dprev) {
      SUMA_LH("Dset exists");
      if (replace || AFNI_yesenv("SUMA_AllowFilenameDsetMatch")) {
         SUMA_LH("Replacing");
         SUMA_SL_Note("%s",stmp);
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
         /* Put a flag up to state that all overlays of this dataset need 
            to have column copied refreshed */
         NI_set_attribute(dprev->dnel,"ResetOverlay_Vecs", "yes");
         /*make the switch so that on return, dset becomes the previous dset*/
         *dsetp = dprev;
      } else {
         SUMA_LH("Not Replacing");
         snprintf(stmp, 198, "Dset with similar idcode (%s)\n"
                        "found in list. \n"
                        "Replacement option is turned off.\n"
                        "Set 'SUMA_AllowDsetReplacement = YES'\n"
                        "in ~/.sumarc to allow replacement.\n", s);
         SUMA_SL_Err("%s",stmp);
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

int SUMA_DeleteDsetPointer (SUMA_DSET **dsetp, DList *DsetList)
{
   static char FuncName[]={"SUMA_DeleteDsetPointer"};
   char *s=NULL, stmp[200];
   SUMA_DSET *dprev=NULL, *dset=NULL;
   DListElmt *el=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!DsetList)  { SUMA_SL_Err("Need Dset List"); SUMA_RETURN(0); }
   if (!dsetp) { SUMA_SL_Err("dsetp is NULL"); SUMA_RETURN(0); }
   else dset = *dsetp;  /* dset is the new pointer */
   
   if (!dset->dnel) { SUMA_SL_Err("dset->nel is NULL\nNothing to do"); SUMA_RETURN(0); }

   s= SDSET_ID(dset); if (!s) { SUMA_SL_Err("dset has no idcode.\n"); SUMA_RETURN(0); }
   if ((el = SUMA_FindDsetEl_ns (s,  DsetList))) {
      SUMA_LH("Dset exists, removing element");
      dlist_remove(DsetList, el, (void *)&dprev);
      if (dset != dprev) { SUMA_S_Crit("This is confusing..."); SUMA_RETURN(0);}
      
      SUMA_LH("   and freeing dset");
      SUMA_FreeDset(dset); dset = NULL;
      *dsetp = NULL;      
   } else {
      SUMA_LH("Dset not in list, no action taken");
   }
   
   SUMA_RETURN(1);
}

char *SUMA_ShowMeSome ( void *dt, SUMA_VARTYPE tp, int N_dt, 
                        int mxshow, char *title)
{
   static char FuncName[]={"SUMA_ShowMeSome"};
   int i, imx, firsthalf, secondhalf;
   double *dtd;
   int *dti;
   short *dth;
   byte *dtb;
   char **dts;
   float *dtf;
   char *s=NULL;
   complex *dtc = NULL;
   SUMA_STRING *SS=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (mxshow > N_dt) mxshow = N_dt;
   
   if (mxshow < 0) mxshow = N_dt;
   if (mxshow < 0) SUMA_RETURN(s);
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (title) {
      SS = SUMA_StringAppend_va(SS, "%s", title);
   }
   
   
   firsthalf = mxshow / 2;
   secondhalf = mxshow - firsthalf;
   if (LocalHead) 
      fprintf(SUMA_STDERR,
               "%s: tp=%d, SUMA_complex = %d, SUMA_double=%d, SUMA_float=%d, "
               "SUMA_int=%d, SUMA_byte=%d, SUMA_short=%d\n", 
               FuncName, tp, SUMA_complex, SUMA_double, SUMA_float, 
               SUMA_int, SUMA_byte, SUMA_short);
   if (mxshow && dt) {
      switch (tp) {
         case SUMA_double:
            dtd = (double*)dt;
            for (i=0; i < firsthalf; ++i) 
                  SS = SUMA_StringAppend_va(SS, "%lf, ", dtd[i]);
            if (mxshow < N_dt) SS = SUMA_StringAppend_va(SS, "..., ");
            if (secondhalf > 1) { 
               for (i=SUMA_MAX_PAIR(N_dt-secondhalf, firsthalf); i<N_dt-1; ++i) 
                     SS = SUMA_StringAppend_va(SS, "%lf, ", dtd[i]); 
            }
            SS = SUMA_StringAppend_va(SS, "%lf", dtd[N_dt-1]);
            break;
         case SUMA_float:
            dtf = (float*)dt;
            for (i=0; i < firsthalf; ++i) 
               SS = SUMA_StringAppend_va(SS, "%f, ", dtf[i]);
            if (mxshow < N_dt) SS = SUMA_StringAppend_va(SS, "..., ");
            if (secondhalf > 1) { 
               for (i=SUMA_MAX_PAIR(N_dt-secondhalf, firsthalf); i<N_dt-1; ++i) 
                  SS = SUMA_StringAppend_va(SS, "%f, ", dtf[i]); 
            }
            SS = SUMA_StringAppend_va(SS, "%f", dtf[N_dt-1]);
            break;
         case SUMA_int:
            dti = (int*)dt;
            for (i=0; i < firsthalf; ++i) 
               SS = SUMA_StringAppend_va(SS, "%d, ", dti[i]);
            if (mxshow < N_dt) SS = SUMA_StringAppend_va(SS, "..., ");
            if (secondhalf > 1) { 
               for (i=SUMA_MAX_PAIR(N_dt-secondhalf, firsthalf); i<N_dt-1; ++i) 
                  SS = SUMA_StringAppend_va(SS, "%d, ", dti[i]); 
            }
            SS = SUMA_StringAppend_va(SS, "%d", dti[N_dt-1]);
            break;
         case SUMA_byte:
            dtb = (byte*)dt;
            for (i=0; i < firsthalf; ++i) 
               SS = SUMA_StringAppend_va(SS, "%d, ", dtb[i]);
            if (mxshow < N_dt) SS = SUMA_StringAppend_va(SS, "..., ");
            if (secondhalf > 1) { 
               for (i=SUMA_MAX_PAIR(N_dt-secondhalf, firsthalf); i<N_dt-1; ++i) 
                  SS = SUMA_StringAppend_va(SS, "%d, ", dtb[i]); 
            }
            SS = SUMA_StringAppend_va(SS, "%d", dtb[N_dt-1]);
            break;
         case SUMA_short:
            dth = (short*)dt;
            for (i=0; i < firsthalf; ++i) 
               SS = SUMA_StringAppend_va(SS, "%d, ", dth[i]);
            if (mxshow < N_dt) SS = SUMA_StringAppend_va(SS, "..., ");
            if (secondhalf > 1) { 
               for (i=SUMA_MAX_PAIR(N_dt-secondhalf, firsthalf); i<N_dt-1; ++i) 
                  SS = SUMA_StringAppend_va(SS, "%d, ", dth[i]); 
            }
            SS = SUMA_StringAppend_va(SS, "%d", dth[N_dt-1]);
            break;
         case SUMA_string:
            dts = (char **)dt;
            for (i=0; i < firsthalf; ++i) 
               SS = SUMA_StringAppend_va(SS, "%s, ", dts[i]);
            if (mxshow < N_dt) SS = SUMA_StringAppend_va(SS, "..., ");
            if (secondhalf > 1) { 
               for (i=SUMA_MAX_PAIR(N_dt-secondhalf, firsthalf); i<N_dt-1; ++i) 
                  SS = SUMA_StringAppend_va(SS, "%s, ", dts[i]); 
            }
            SS = SUMA_StringAppend_va(SS, "%s", dts[N_dt-1]);
            break;
         case SUMA_complex:
            dtc = (complex *)dt; 
            for (i=0; i < firsthalf; ++i) 
               SS = SUMA_StringAppend_va(SS, "(%f %+fi), ", dtc[i].r, dtc[i].i);
            if (mxshow < N_dt) SS = SUMA_StringAppend_va(SS, "..., ");
            if (secondhalf > 1) { 
               for (i=SUMA_MAX_PAIR(N_dt-secondhalf, firsthalf); i<N_dt-1; ++i) 
                  SS = SUMA_StringAppend_va(SS, "(%f %+fi), ", 
                                                dtc[i].r, dtc[i].i); 
            }
            SS = SUMA_StringAppend_va( SS, "(%f %+fi)", 
                                       dtc[N_dt-1].r, dtc[N_dt-1].i);
            break;
         default:
            SS = SUMA_StringAppend_va(SS, "Column type not supported.");
      }
   } else {
      if (!dt) {
        SUMA_StringAppend(SS,"NULL vector.");
      } else {
         SS = SUMA_StringAppend_va(SS, "Empty vector.");
      }
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
   NI_group *ngr=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (dset) {
      SS = SUMA_StringAppend_va(SS, "Dset %p, datum level %d (0..%d possible)\n",
                                 dset,  SUMA_sdset_datum_level(dset),
                                 SUMA_N_LEV_DAT);
      SS = SUMA_StringAppend_va(SS, "Number of Links: %d\n", dset->N_links);
      if (dset->dnel) {
         SS = SUMA_StringAppend_va(SS, 
            "Dset Name: %s (%d), isGraph %d, isCIFTI (MultiDomain)%d\n", 
            dset->dnel->name, SUMA_Dset_Type(dset->dnel->name),
            SUMA_isGraphDset(dset), SUMA_isCIFTIDset(dset));
         if ((s = NI_get_attribute(dset->ngr,"MD_parent_ID"))) {
	    SS = SUMA_StringAppend_va(SS, 
	          "Elemantary dataset created from multi domain parent:\n"
		  "   ID: %s\n"
		  "   Label: %s\n"
		  "   Domain Index: %s",
		  s, NI_get_attribute(dset->ngr,"MD_parent_label"),
		  NI_get_attribute(dset->ngr,"MD_parent_subdomain_index"));
	    s = NULL;
	 }
	 if (SDSET_FILENAME(dset)) 
            SS = SUMA_StringAppend_va( SS, "filename: %s\n", 
                                       SDSET_FILENAME(dset));
         else SS = SUMA_StringAppend_va(SS, "filename: NULL\n");
         if (SDSET_LABEL(dset)) 
            SS = SUMA_StringAppend_va(SS, "label: %s\n", SDSET_LABEL(dset));
         else SS = SUMA_StringAppend_va(SS, "label: NULL\n");
         if (SDSET_ID(dset)) 
            SS = SUMA_StringAppend_va( SS, "self_idcode (idcode): %s\n", 
                                       SDSET_ID(dset));
         else SS = SUMA_StringAppend_va(SS, "self_idcode (idcode): NULL\n");
         if (SDSET_IDGDOM(dset)) 
            SS = SUMA_StringAppend_va( SS, "geometry_parent_idcode: %s\n", 
                                       SDSET_IDGDOM(dset));
         else SS = SUMA_StringAppend_va(SS, "geometry_parent_idcode: NULL\n");
         if (SDSET_IDMDOM(dset)) 
            SS = SUMA_StringAppend_va(SS, 
                        "domain_parent_idcode (MeshParent_idcode): %s\n", 
                        SDSET_IDMDOM(dset));
         else SS = SUMA_StringAppend_va(SS, 
                        "domain_parent_idcode ( MeshParent_idcode): NULL\n");
         

         /* where is the node index (NodeDef) column ? */
         SS = SUMA_StringAppend_va(SS, "Node Index (NodeDef) Element:\n");
         if (!dset->inel) {
            SS = SUMA_StringAppend_va(SS, "NULL inel\n");
         } else {
            SS = SUMA_StringAppend_va(SS, "\tinel->vec_len = %d\n"
                                          "\tinel->vec_num = %d\n"
                                          "\tinel->vec_filled = %d\n", 
                                          dset->inel->vec_len, 
                                          dset->inel->vec_num, 
                                          dset->inel->vec_filled);
         }   
         {  int *ind;
            ind = SUMA_GetNodeDef (dset);
            if (!ind) {
               SS = SUMA_StringAppend_va(SS, 
                           "\tNode Index Column pointer is NULL.\n");
            } else {
               SS = SUMA_StringAppend_va(SS, "\tNode Index Column found.\n");
               if (SDSET_SORTED(dset)) 
                  SS = SUMA_StringAppend_va(SS, 
                     "\tsorted_node_def: %s\n", SDSET_SORTED(dset));
               else SS = SUMA_StringAppend_va(SS, "\tsorted_node_def: NULL\n");
               s = SUMA_ShowMeSome((void*)(  ind), 
                                   SUMA_ColType2TypeCast (SUMA_NODE_INDEX) 
                                  , SDSET_VECLEN(dset), 5, NULL);
               SS = SUMA_StringAppend_va(SS, "         %s\n", s); 
                                    SUMA_free(s); s = NULL;
            }
         }
         
         if (!dset->Aux) SS = SUMA_StringAppend_va(SS, "Aux struct is NULL");
         else {
            SS = SUMA_StringAppend_va(SS, "Saux: %p\n", 
                           dset->Aux->Saux?dset->Aux->Saux:NULL); 
            SS = SUMA_StringAppend_va(SS, "matrix_shape: %d", 
                                       dset->Aux->matrix_shape);
            SS = SUMA_StringAppend_va(SS, 
               "matrix_max_index: %ld, matrix_size: %ld %ld, matrix_2M: %ld\b", 
                  dset->Aux->matrix_max_index, 
                  dset->Aux->matrix_size[0], dset->Aux->matrix_size[1],
                  dset->Aux->matrix_2M);
            SS = SUMA_StringAppend_va(SS, 
               "range_edge_index: %ld %ld, range_node_index: %ld %ld\n"
               "N_seg_nodes: %ld, N_all_nodes: %ld\n",
               dset->Aux->range_edge_index[0], dset->Aux->range_edge_index[1],
               dset->Aux->range_node_index[0], dset->Aux->range_node_index[1],
               dset->Aux->N_seg_nodes, dset->Aux->N_all_nodes);
            SS = SUMA_StringAppend_va(SS, "%d domains:\n", dset->Aux->N_doms);
            if (dset->Aux->doms) {
               for (i=0; i<dset->Aux->N_doms; ++i) {
                  if (dset->Aux->doms[i]) {
                     
		     SS = SUMA_StringAppend_va(SS, 
                           "   dom[%d]:\n"
			   "      edset_id %s\n"
                           "      Source: %s\n"
                           "      IndexOffset: %d\n"
                           "      IndexCount: %d\n"
                           "      Max_N_Data: %d\n"
                           "      ModelType: %d %s\n"
                           "      Range: %d %d %d %d\n",
                     i, CNS(dset->Aux->doms[i]->edset_id),
		     SUMA_CHECK_NULL_STR(dset->Aux->doms[i]->Source),
                     dset->Aux->doms[i]->IndexOffset, 
                     dset->Aux->doms[i]->IndexCount,
                     dset->Aux->doms[i]->Max_N_Data,
                     dset->Aux->doms[i]->ModelType,
            SUMA_ObjectTypeCode2ObjectTypeName(dset->Aux->doms[i]->ModelType),
                     dset->Aux->doms[i]->Range[0], dset->Aux->doms[i]->Range[1],
                     dset->Aux->doms[i]->Range[2], dset->Aux->doms[i]->Range[3]);
                  } else {
                     SS = SUMA_StringAppend_va(SS, "   dom[%d]=NULL\n", i);
                  }
               }
            } else {
               SS = SUMA_StringAppend_va(SS, "dset->Aux->doms is NULL");
            }
         }
         SS = SUMA_StringAppend_va(SS, "Data Element:\n");
         SS = SUMA_StringAppend_va(SS, 
                  "dnel->vec_num (N_subsets): %d\n", SDSET_VECNUM(dset));
         SS = SUMA_StringAppend_va(SS, 
                  "dnel->vec_filled (N_NodeDef): %d\n", SDSET_VECFILLED(dset));
         SS = SUMA_StringAppend_va(SS, 
                  "dnel->vec_len (N_Alloc): %d\n", SDSET_VECLEN(dset));
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
               SS = SUMA_StringAppend_va(SS, 
                                     "\tColumn %d's attribute: %s\n", 
                                       i, s ); 
            } else {
               SS = SUMA_StringAppend_va(SS, 
                                    "\tColumn %d's attribute does not exist.\n", 
                                       i );
            }
            if (s) SUMA_free(s); s = NULL;
            s = SUMA_DsetColLabelCopy(dset, i, 0);
            if (s && s[0] != '\0') {
               SS = SUMA_StringAppend_va(SS, 
                                       "\tColumn %d's label: %s\n", 
                                       i, s ); 
            } else {
               SS = SUMA_StringAppend_va(SS, 
                                       "\tColumn %d's label does not exist.\n", 
                                       i );
            }
            if (s) SUMA_free(s); s = NULL;
            if (dset->dnel->vec[i]) {
               s = SUMA_ShowMeSome((void*)(  dset->dnel->vec[i]), 
                                             SUMA_ColType2TypeCast (ctp) 
                                             , SDSET_VECLEN(dset), 5, NULL);
               SS = SUMA_StringAppend_va(SS, "         %s\n", s); 
               SUMA_free(s); s = NULL;
            } else SS = SUMA_StringAppend_va(SS, "         NULL\n");
            #endif
         }
         
         if (detail) { /* write the entire element to SS */
            NI_stream ns = NI_stream_open("str:", "w");
            NI_write_element(ns, dset->ngr, NI_TEXT_MODE);
            SS = SUMA_StringAppend(SS, "\n Full NI group in text mode:\n"); 
            SS = SUMA_StringAppend(SS, NI_stream_getbuf(ns)); 
               /* don't use StringAppend_va because it does not 
                  allow very long strings. */
            SS = SUMA_StringAppend(SS, "\n");
            NI_stream_close(ns);
         }
      } else {
         SS = SUMA_StringAppend(SS, "NULL dset->dnel.");
      }
      /* got cmap? */
      if ((ngr = SUMA_NI_Cmap_of_Dset(dset))) {
            NI_stream ns = NI_stream_open("str:", "w");
            NI_write_element(ns, ngr, NI_TEXT_MODE);
            SS = SUMA_StringAppend(SS, "\n Internal colormap info:\n"); 
            SS = SUMA_StringAppend(SS, NI_stream_getbuf(ns)); 
            SS = SUMA_StringAppend(SS, "\n");
            NI_stream_close(ns);
      } else {
         SS = SUMA_StringAppend(SS, "No internal colormap.");
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
               /* The SDSET_VECFILLED(dset) < SDSET_NODEINDFILLED(dset) */
               /* can occur when drawing ROI as the node index column */
               /* is created to be as large as the number of nodes in the */
               /* surface but the number of columns for the RGB data grows */
               /* with each drawing stroke. */  
      if (  SDSET_VECLEN(dset) > SDSET_NODEINDLEN(dset) ||              
            SDSET_VECFILLED(dset) > SDSET_NODEINDFILLED(dset) ) {       
         SUMA_SL_Err(   "Veclen and/or vecfilled for\n"                 
                        "node indices is less \n"                       
                        "than that of dset data!");                     
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
         SUMA_SL_Warn("Found more than one node index vector.\n"
                      "Returning first one found.\n");
      }
      SUMA_free(iv); iv = NULL;
   }
   
   SUMA_RETURN(i); 
}

/*!
   \brief Look for datasets satisfying the following:
      type SUMA_NODE_CONVEXITY 
      idcode_str for a geometry domain and a mesh domain
   \param ReturnDsetPointer if 1 then return pointer is to 
                                       dset element (SUMA_DSET *)
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
                     if (!iv) { 
                        SUMA_SL_Err("SUMA_NODE_CX not found."); 
                        SUMA_RETURN(NULL); }
                     if (N_i != 1) { 
                        SUMA_SL_Err("more than 1 SUMA_NODE_CX found."); 
                        SUMA_RETURN(NULL); }
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
      SUMA_SL_Warn ( "More than one convexity dataset found.\n"
                     "Returning first one encountered.");
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
   /* This would fail if data are not ordered such that row(i) 
      is the data for node i */
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
   
   /* last resort, assume that data are ordered properly 
      (see commented out section above)*/
   if ((nel->vec_len == nel->vec_filled && 
       (nel->vec_len == N_Node || N_Node == -1))) {
               /* N_Node can be set to -1 when function is called on
                  non-SOs . In that case the N_Node match should not
                  be enforced */
      if (0 && !(WarnCount % 25 - 1)) {
         SUMA_PushErrLog(  "SLP_Warn", "Assuming ith row of data\n"
                     "corresponds to node i.\n"
                     "You'll get trash if this is not true.\n"
                     "This warning is shown intermittently.", FuncName);
      } ++ WarnCount; 
      SUMA_RETURN(row);
   }
   
   fprintf(stderr,"row %d vec_len %d vec_filled %d N_Node %d\n",
                row, nel->vec_len, nel->vec_filled, N_Node);
   SUMA_DUMP_TRACE("???");   
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
int SUMA_GetNodeRow_FromNodeIndex_eng(SUMA_DSET *dset, int node, 
                                      int Max_Node_Index)
{
   static char FuncName[]={"SUMA_GetNodeRow_FromNodeIndex_eng"};
   static int WarnCount;
   int Found = -1, i, *NodeDef=NULL;
   double dval=0.0;
   char *str=NULL;
   char mmm[256];
   NI_element *nel = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
      
   
   if (!dset || node < 0 || (Max_Node_Index >=0 && node > Max_Node_Index)) {
      /* turn this warning off once you confirm that Shane's bug is gone */
      /* Note that -1 is a flag for an unitialized node, so it is benign */
      if (LocalHead) {
         snprintf(mmm,
                 255*sizeof(char),
            "Strange input, dset %p, node %d, Max Node Index %d. returning -1.",
                 dset, node, Max_Node_Index);
         
         SUMA_PushErrLog("SL_Warn", mmm, FuncName);
         SUMA_DUMP_TRACE("At the strange input");
      }
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
   /* This would fail if data are not ordered such that row(i) is 
      the data for node i */
   SUMA_LH( "Trying the fast one");
   if (nel->vec_len == nel->vec_filled && nel->vec_len == (Max_Node_Index+1)) {
      SUMA_RETURN(node);
   }
   #endif
   
   SUMA_LH("Trying the slow mo");
   /* does this dset have a column index ? */
   NodeDef = SUMA_GetNodeDef (dset);
   
   if (NodeDef) {
      SUMA_LH("Col. Index found, node %d, vec_filled %d", node, nel->vec_filled);
      if (nel->vec_filled > node) { 
         /* bug here (used to be < !) discovered thanks to Rosanna and Shane */
         if (node == NodeDef[node]) {
            SUMA_LH("Got lucky");
            SUMA_RETURN(node);
         }
      }
      /* explicit search */
      SUMA_LH("Explicit");
      if (Max_Node_Index >= 0 && nel->vec_filled > (Max_Node_Index+1)) { 
                                 /* Sometimes Max_Node_Index is not set (-1)*/
         if (LocalHead) {
            fprintf( SUMA_STDERR,
                  "Error %s: nel->vec_filled (%d) > (%d) Max_Node_Index+1\n",
                  FuncName, nel->vec_filled, Max_Node_Index+1); 
         }
         SUMA_PushErrLog("SL_Err",
                         "Unexpected error nel->vec_filled > Max_Node_Index+1", 
                         FuncName);
         SUMA_RETURN(-1);
      }
      
      if (SDSET_IS_SORTED(dset)) {
         SUMA_LH("A sorted beast");
         SUMA_RETURN(SUMA_ibinFind(NodeDef, nel->vec_filled, node));
      } else {
         SUMA_LH("A long slow search through %d data\n",nel->vec_filled); 
         for (i=0; i<nel->vec_filled; ++i) {
            if (NodeDef[i] == node) {
               SUMA_RETURN(i);
            }
         }
      }
   } else {
      SUMA_LH("No Col. Index found");
   }
   
   /* last resort, assume that data are ordered properly 
      (see commented out section above)*/
   if (Max_Node_Index < 0 && SUMA_isVolDataset(dset)) {
      /* By now, we know we do not have explicit node indices */
      Max_Node_Index = nel->vec_len -1;
   }
   if (nel->vec_len == nel->vec_filled && nel->vec_len == Max_Node_Index+1) {
      SUMA_LH("Off the deep end");
      if (0 && !(WarnCount % 25 - 1)) {
         SUMA_PushErrLog("SLP_Err", "Assuming ith row of data\n"
                     "corresponds to node i.\n"
                     "You'll get trash if this is not true.\n"
                     "This warning is shown intermittently.", FuncName);
      } ++ WarnCount;       
      SUMA_RETURN(node);
   } else {
      if (LocalHead) {
         SUMA_LH("Govt Shutdown");
         SUMA_ShowDset(dset, 0, NULL);
      }
   }
      
   /* bad news lews, this node is not in this Dset */ 
   SUMA_RETURN(-1);
}

/*!
   returns indexmap, such that indexmap[node] = row which means that
   data for node 'node' is in row 'row' of the dset
   if indexmap[node] = -1 then the node does not exist in the dset
   indexmap is as large as the MAX(largest index in the node list in dset , 
                                   maxind, SDSET_VECLEN(dset))
   free indexmap with SUMA_free
   
   This function should be OK for Graph Dsets.
*/
int *SUMA_CreateNodeIndexToRowIndexMap(SUMA_DSET *dset, int maxind, 
                                       double *range)
{
   static char FuncName[]={"SUMA_CreateNodeIndexToRowIndexMap"};
   int *indexmap=NULL, j=0, *nip=NULL;
   int maxn = -1, loc[2];
   double rangel[2];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!range) range = rangel;
   
   if (!(nip = SUMA_GetNodeDef(dset))) {
      SUMA_S_Err("Failed to find node index column in dset");
      SUMA_RETURN(indexmap);
   }
   
   /* get the range of valid nodes */
   if (!SUMA_GetDsetNodeIndexColRange(dset, range, loc, 1)) {
      SUMA_S_Err("Failed to get node range!");
      SUMA_RETURN(NULL);  
   }

   maxn = SUMA_MAX_PAIR(maxind+1, SDSET_VECLEN(dset));
   maxn = SUMA_MAX_PAIR(maxn, range[1]+1);
   
   /* find the mapping of a node index to a row */
   indexmap = (int *)SUMA_calloc(maxn, sizeof(int));
   if (!indexmap) {
      SUMA_S_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   for (j=0; j<maxn; ++j)  indexmap[j] = -1;
   
   for (j=0; j<SDSET_VECFILLED(dset); ++j) 
      indexmap[nip[j]] = j; /* indexmap[node] hold the 
                              row in the dset that contains 
                              data for node */
   if (LocalHead) {
      for (j=0; j<SDSET_VECFILLED(dset); ++j) {
          fprintf(stderr,"node %d is in row %d\n", 
                     nip[j],
                     indexmap[nip[j]]);   
      }
   }
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
   complex *cmv = NULL;
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
      case SUMA_complex: /* Should be a flag for what to return here, someday*/
         cmv = (complex *)nel->vec[ind];
         dval = (double)CABS(cmv[ival]);
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
   complex *cmv=NULL;
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
      case SUMA_complex:
         str = (char *)SUMA_malloc(100*sizeof(char));
         cmv = (complex *)nel->vec[ind];
         *dval = CABS(cmv[ival]);
         sprintf(str,"%f i%f",cmv[ival].r, cmv[ival].i);
         break;
      default:
         SUMA_SL_Err("This type is not supported yet.\n");
         SUMA_RETURN(NULL);
         break;
   }

   SUMA_LH("%s",str);
   SUMA_RETURN(str);
}

/*
   returns a vector of values at a particular node index in a dset
   dset: is the dset in question
   ind (int *): An optional vector of columns to use.
               Default is NULL = all columns
   nind (int): Number of values in ind.
               Not useful when ind = NULL
   node (int): Index of node in question
   iNodeMax (int): If you know the maximum number of nodes forming the domain
                  of the dset, typically SO->iNodeMax then pass it for speed.
                  Otherwise, pass -1
   N_ret (int *): Pointer to int to contain the number of values in result
                  vector
   Returns:
      result (double*) a vector of N_ret values at node node
   
   \sa SUMA_GetDsetAllNodeValsInCols
   \sa SUMA_Dset2VecArray
*/
void *SUMA_GetDsetAllNodeValsInCols2(SUMA_DSET *dset, 
                                       int *ind, int nind, 
                                       int node, int iNodeMax,
                                       int *N_ret,
                                       SUMA_VARTYPE tp)
{
   static char FuncName[]={"SUMA_GetDsetAllNodeValsInCols2"};
   double *resd = NULL;
   float *resf = NULL;
   int *resi=NULL;
   void *resv=NULL;
   int noderow=-1, i=-1;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!dset || !dset->dnel) { SUMA_SL_Err("NULL input"); SUMA_RETURN(resv); }
   noderow = SUMA_GetNodeRow_FromNodeIndex_eng (dset, node, iNodeMax);
   if (noderow < 0) {   SUMA_LH("Could not get node row\n"
                                " This can happen in benign cases"); 
                        SUMA_RETURN(resv); }
   if ( tp != SUMA_float &&
        tp != SUMA_double &&
        tp != SUMA_int) { SUMA_SL_Err("Bad otype"); SUMA_RETURN(resv); } 
   if (ind) {
      if (nind <= 0) {
         SUMA_SL_Err("no columns selected"); SUMA_RETURN(resv); 
      }
      if (nind > SDSET_VECNUM(dset)) {
         SUMA_SL_Err("More columns than in dset"); SUMA_RETURN(resv); 
      }
      switch (tp) {
         case SUMA_double:
            if (!(resd = (double*)SUMA_calloc(nind, sizeof(double)))) {
               SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(resv); 
            }
            for (i=0; i<nind; ++i) {
               if (ind[i] >= 0 && ind[i]<SDSET_VECNUM(dset)) {
                  resd[i] = SUMA_GetDsetValInCol2(dset, ind[i], noderow);
               } else {
                  resd[i] = 0.0; 
               }
            }
            resv = (void *)resd;
            break;
         case SUMA_float:
            if (!(resf = (float*)SUMA_calloc(nind, sizeof(float)))) {
               SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(resv); 
            }
            for (i=0; i<nind; ++i) {
               if (ind[i] >= 0 && ind[i]<SDSET_VECNUM(dset)) {
                  resf[i] = (float)SUMA_GetDsetValInCol2(dset, ind[i], noderow);
               } else {
                  resf[i] = 0.0; 
               }
            }
            resv = (void *)resf;
            break;
         case SUMA_int:
            if (!(resi = (int*)SUMA_calloc(nind, sizeof(int)))) {
               SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(resv); 
            }
            for (i=0; i<nind; ++i) {
               if (ind[i] >= 0 && ind[i]<SDSET_VECNUM(dset)) {
                  resi[i] = (int)SUMA_GetDsetValInCol2(dset, ind[i], noderow);
               } else {
                  resi[i] = 0; 
               }
            }
            resv = (void *)resi;
            break;
         default:
            SUMA_SL_Err("Bad otype, why here?"); SUMA_RETURN(resv);
            break;
      }
      *N_ret =  nind;  
   } else {
      switch (tp) {
         case SUMA_double:
            if (!(resd = (double*)SUMA_calloc(SDSET_VECNUM(dset),
                                                sizeof(double)))) {
               SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(resv); 
            }
            for (i=0; i<SDSET_VECNUM(dset); ++i) {
               resd[i] = SUMA_GetDsetValInCol2(dset, i, noderow);
            }
            resv = (void *)resd;
            break;
         case SUMA_float:
            if (!(resf = (float*)SUMA_calloc(SDSET_VECNUM(dset),
                                                sizeof(float)))) {
               SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(resv); 
            }
            for (i=0; i<SDSET_VECNUM(dset); ++i) {
               resf[i] = (float)SUMA_GetDsetValInCol2(dset, i, noderow);
            }
            resv = (void *)resf;
            break;
         case SUMA_int:
            if (!(resi = (int*)SUMA_calloc(SDSET_VECNUM(dset),
                                                sizeof(int)))) {
               SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(resv); 
            }
            for (i=0; i<SDSET_VECNUM(dset); ++i) {
               resi[i] = (int)SUMA_GetDsetValInCol2(dset, i, noderow);
            }
            resv = (void *)resi;
            break;
         default:
            SUMA_SL_Err("Bad otype, why here too?"); SUMA_RETURN(resv);
            break;
      }
      *N_ret = SDSET_VECNUM(dset);
   }
   
   SUMA_RETURN(resv);
}

/* 
This function is very similar to SUMA_GetDsetAllNodeValsInCols2
Except it operates on multiple nodes.
What you get back is is a double pointer of VARTYPE with N_ret values
in each row and N_Node rows

\sa SUMA_VecArray2Dset
*/

void **SUMA_Dset2VecArray(SUMA_DSET *dset, 
                        int *ind, int nind, 
                        int *node, int N_Node,
                        int iNodeMax,
                        int *N_ret,
                        SUMA_VARTYPE tp)
{
   static char FuncName[]={"SUMA_Dset2VecArray"};
   double **resd = NULL, *dc=NULL;
   float **resf = NULL, *fc=NULL;
   int **resi=NULL, icol=0, i, c, *ic=NULL;
   short **ress=NULL, *sc=NULL;
   byte **resb=NULL, *bc=NULL;
   void **resv=NULL;
   int *rowofnode=NULL;
   double range[2];
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!dset || !dset->dnel) { SUMA_SL_Err("NULL input"); SUMA_RETURN(resv); }
   rowofnode = SUMA_CreateNodeIndexToRowIndexMap(dset, iNodeMax, range);

   if (!node) {
      node = SDSET_NODE_INDEX_COL(dset);
      N_Node = SDSET_VECLEN(dset);
   } else {
      /* make sure node indices do not exceed dset range */
      for (i=0; i<N_Node; ++i) {
         if (ind[i] > (int)range[1] || ind[i] < (int)range[0]) {
            SUMA_S_Err("Node indices out or range");
            SUMA_RETURN(resv);
         }
      }
   }
   if (!node) {
      SUMA_S_Err("Nothing to work with");
      SUMA_RETURN(resv);
   }
   if (!ind) {
      nind = SDSET_VECNUM(dset);
   }
   if ( tp == SUMA_complex) {
      SUMA_S_Err("No support for complex types yet");
      SUMA_RETURN(resv);
   }
   if ( tp != SUMA_float &&
        tp != SUMA_double &&
        tp != SUMA_int&&
        tp != SUMA_short &&
        tp != SUMA_byte ) { SUMA_SL_Err("Bad otype"); SUMA_RETURN(resv); } 
      
   if (nind <= 0) {
      SUMA_SL_Err("no columns selected"); SUMA_RETURN(resv); 
   }
   if (nind > SDSET_VECNUM(dset)) {
      SUMA_SL_Err("More columns than in dset"); SUMA_RETURN(resv); 
   }
   switch (tp) {
      case SUMA_double:
         resd=NULL; resf=NULL; resi=NULL; ress=NULL; resb=NULL;
         resd = (double**)SUMA_calloc(N_Node, sizeof(double*));
         for (i=0; i<N_Node; ++i) {
            if (!(resd[i] = (double*)SUMA_calloc(nind, sizeof(double)))) {
               SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(resv); 
            }
         }
         for (c=0; c<nind; ++c) { /* for each column */
            if (ind) icol = ind[c];
            else icol = c;
            dc=NULL; fc=NULL; ic=NULL; sc=NULL; bc=NULL;
            switch (SUMA_ColType2TypeCast(
                           SUMA_TypeOfDsetColNumb(dset, icol))) {
               case SUMA_double:
                  dc = (double *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resd[i][c] = (double)dc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_float:
                  fc = (float *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resd[i][c] = (double)fc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_int:
                  ic = (int *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resd[i][c] = (double)ic[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_short:
                  sc = (short *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resd[i][c] = (double)sc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_byte:
                  bc = (byte *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resd[i][c] = (double)bc[rowofnode[node[i]]];
                  }
                  break;
               default:
                  SUMA_S_Err("Var Type not supported."
                             "Memory leak now present");
                  SUMA_RETURN(NULL); 
            }
         } /* for each column */
         resv = (void **)resd;
         break;   

      case SUMA_float:
         resd=NULL; resf=NULL; resi=NULL; ress=NULL; resb=NULL;
         resf = (float**)SUMA_calloc(N_Node, sizeof(float*));
         for (i=0; i<N_Node; ++i) {
            if (!(resf[i] = (float*)SUMA_calloc(nind, sizeof(float)))) {
               SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(resv); 
            }
         }
         for (c=0; c<nind; ++c) { /* for each column */
            if (ind) icol = ind[c];
            else icol = c;
            dc=NULL; fc=NULL; ic=NULL; sc=NULL; bc=NULL;
            switch (SUMA_ColType2TypeCast(
                           SUMA_TypeOfDsetColNumb(dset, icol))) {
               case SUMA_double:
                  dc = (double *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resf[i][c] = (float)dc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_float:
                  fc = (float *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resf[i][c] = (float)fc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_int:
                  ic = (int *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resf[i][c] = (float)ic[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_short:
                  sc = (short *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resf[i][c] = (float)sc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_byte:
                  bc = (byte *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resf[i][c] = (float)bc[rowofnode[node[i]]];
                  }
                  break;
               default:
                  SUMA_S_Err("Type not supported."
                             "Memory leak now present");
                  SUMA_RETURN(NULL); 
            }
         } /* for each column */
         resv = (void **)resf;
         break;   
      case SUMA_int:
         resd=NULL; resi=NULL; resi=NULL; ress=NULL; resb=NULL;
         resi = (int**)SUMA_calloc(N_Node, sizeof(int*));
         for (i=0; i<N_Node; ++i) {
            if (!(resi[i] = (int*)SUMA_calloc(nind, sizeof(int)))) {
               SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(resv); 
            }
         }
         for (c=0; c<nind; ++c) { /* for each column */
            if (ind) icol = ind[c];
            else icol = c;
            dc=NULL; fc=NULL; ic=NULL; sc=NULL; bc=NULL;
            switch (SUMA_ColType2TypeCast(
                           SUMA_TypeOfDsetColNumb(dset, icol))) {
               case SUMA_double:
                  dc = (double *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resi[i][c] = (int)dc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_float:
                  fc = (float *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resi[i][c] = (int)fc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_int:
                  ic = (int *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resi[i][c] = (int)ic[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_short:
                  sc = (short *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resi[i][c] = (int)sc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_byte:
                  bc = (byte *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resi[i][c] = (int)bc[rowofnode[node[i]]];
                  }
                  break;
               default:
                  SUMA_S_Err("Type not supported."
                             "Memory leak now present");
                  SUMA_RETURN(NULL); 
            }
         } /* for each column */
         resv = (void **)resi;
         break;   
      case SUMA_short:
         resd=NULL; ress=NULL; ress=NULL; ress=NULL; resb=NULL;
         ress = (short**)SUMA_calloc(N_Node, sizeof(short*));
         for (i=0; i<N_Node; ++i) {
            if (!(ress[i] = (short*)SUMA_calloc(nind, sizeof(short)))) {
               SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(resv); 
            }
         }
         for (c=0; c<nind; ++c) { /* for each column */
            if (ind) icol = ind[c];
            else icol = c;
            dc=NULL; fc=NULL; ic=NULL; sc=NULL; bc=NULL;
            switch (SUMA_ColType2TypeCast(
                           SUMA_TypeOfDsetColNumb(dset, icol))) {
               case SUMA_double:
                  dc = (double *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     ress[i][c] = (short)dc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_float:
                  fc = (float *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     ress[i][c] = (short)fc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_int:
                  ic = (int *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     ress[i][c] = (short)ic[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_short:
                  sc = (short *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     ress[i][c] = (short)sc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_byte:
                  bc = (byte *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     ress[i][c] = (short)bc[rowofnode[node[i]]];
                  }
                  break;
               default:
                  SUMA_S_Err("Type not supported."
                             "Memory leak now present");
                  SUMA_RETURN(NULL); 
            }
         } /* for each column */
         resv = (void **)ress;
         break;      
      case SUMA_byte:
         resd=NULL; resb=NULL; resb=NULL; resb=NULL; resb=NULL;
         resb = (byte**)SUMA_calloc(N_Node, sizeof(byte*));
         for (i=0; i<N_Node; ++i) {
            if (!(resb[i] = (byte*)SUMA_calloc(nind, sizeof(byte)))) {
               SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(resv); 
            }
         }
         for (c=0; c<nind; ++c) { /* for each column */
            if (ind) icol = ind[c];
            else icol = c;
            dc=NULL; fc=NULL; ic=NULL; sc=NULL; bc=NULL;
            switch (SUMA_ColType2TypeCast(
                           SUMA_TypeOfDsetColNumb(dset, icol))) {
               case SUMA_double:
                  dc = (double *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resb[i][c] = (byte)dc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_float:
                  fc = (float *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resb[i][c] = (byte)fc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_int:
                  ic = (int *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resb[i][c] = (byte)ic[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_short:
                  sc = (short *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resb[i][c] = (byte)sc[rowofnode[node[i]]];
                  }
                  break;
               case SUMA_byte:
                  bc = (byte *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     resb[i][c] = (byte)bc[rowofnode[node[i]]];
                  }
                  break;
               default:
                  SUMA_S_Err("Type not supported."
                             "Memory leak now present");
                  SUMA_RETURN(NULL); 
            }
         } /* for each column */
         resv = (void **)resb;
         break;
      default:
         SUMA_SL_Err("Bad otype, why here?"); SUMA_RETURN(resv);
         break;
   } /* switch on type of output */

   *N_ret =  nind;  
   if (rowofnode) SUMA_free(rowofnode); rowofnode = NULL;
   
   SUMA_RETURN(resv);
} 

/*!
   Reverse of   SUMA_Dset2VecArray
*/ 
SUMA_DSET *SUMA_VecArray2Dset(void **resv,
                        SUMA_DSET *usethisdset, 
                        int *ind, int nind, 
                        int *node, int N_Node,
                        int iNodeMax,
                        SUMA_VARTYPE tp)
{
   static char FuncName[]={"SUMA_VecArray2Dset"};
   double **resd = NULL, *dc=NULL;
   float **resf = NULL, *fc=NULL;
   int **resi=NULL, icol=0, i, c, *ic=NULL;
   short **ress=NULL, *sc=NULL;
   byte **resb=NULL, *bc=NULL;
   int *rowofnode=NULL;
   double range[2];
   SUMA_DSET *dset=NULL;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!usethisdset) {
      SUMA_S_Err("Not ready to create a new dset from scratch");
   } else {
      dset = usethisdset;  /* put the stuff in here */
   }

   if (!dset || !dset->dnel) { SUMA_SL_Err("NULL input"); SUMA_RETURN(dset); }
   rowofnode = SUMA_CreateNodeIndexToRowIndexMap(dset, iNodeMax, range);
   
   if (!node) {
      node = SDSET_NODE_INDEX_COL(dset);
      N_Node = SDSET_VECLEN(dset);
   } else {
      /* make sure node indices do not exceed dset range */
      for (i=0; i<N_Node; ++i) {
         if (ind[i] > (int)range[1] || ind[i] < (int)range[0]) {
            SUMA_S_Err("Node indices out or range");
            SUMA_RETURN(dset);
         }
      }
   }
   if (!node) {
      SUMA_S_Err("Nothing to work with");
      SUMA_RETURN(dset);
   }
   if (!ind) {
      nind = SDSET_VECNUM(dset);
   }
   if (tp == SUMA_complex) {
      SUMA_S_Err("Complex type not supported");
      SUMA_RETURN(dset); 
   }
   if ( tp != SUMA_float &&
        tp != SUMA_double &&
        tp != SUMA_int &&
        tp != SUMA_short &&
        tp != SUMA_byte ) { SUMA_SL_Err("Bad otype"); SUMA_RETURN(dset); } 
      
   if (nind <= 0) {
      SUMA_SL_Err("no columns selected"); SUMA_RETURN(dset); 
   }
   if (nind > SDSET_VECNUM(dset)) {
      SUMA_SL_Err("More columns than in dset"); SUMA_RETURN(dset); 
   }
   switch (tp) {
      case SUMA_double:
         resd=NULL; resf=NULL; resi=NULL; ress=NULL; resb=NULL;
         resd = (double**)resv;
         for (c=0; c<nind; ++c) { /* for each column */
            if (ind) icol = ind[c];
            else icol = c;
            dc=NULL; fc=NULL; ic=NULL; sc=NULL; bc=NULL;
            switch (SUMA_ColType2TypeCast(
                           SUMA_TypeOfDsetColNumb(dset, icol))) {
               case SUMA_double:
                  dc = (double *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     dc[rowofnode[node[i]]] = (double)resd[i][c];
                  }
                  break;
               case SUMA_float:
                  fc = (float *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     fc[rowofnode[node[i]]] = (float)resd[i][c];
                  }
                  break;
               case SUMA_int:
                  ic = (int *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     ic[rowofnode[node[i]]] = (int)resd[i][c];
                  }
                  break;
               case SUMA_short:
                  sc = (short *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     sc[rowofnode[node[i]]] = (short)resd[i][c];
                  }
                  break;
               case SUMA_byte:
                  bc = (byte *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     bc[rowofnode[node[i]]] = (byte)resd[i][c];
                  }
                  break;
               default:
                  SUMA_S_Err("Type not supported."
                             "Memory leak now present");
                  SUMA_RETURN(NULL); 
            }
         } /* for each column */
         break;   

      case SUMA_float:
         resd=NULL; resf=NULL; resi=NULL; ress=NULL; resb=NULL;
         resf = (float**)resv;
         for (c=0; c<nind; ++c) { /* for each column */
            if (ind) icol = ind[c];
            else icol = c;
            dc=NULL; fc=NULL; ic=NULL; sc=NULL; bc=NULL;
            switch (SUMA_ColType2TypeCast(
                           SUMA_TypeOfDsetColNumb(dset, icol))) {
               case SUMA_double:
                  dc = (double *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     dc[rowofnode[node[i]]] = (double)resf[i][c];
                  }
                  break;
               case SUMA_float:
                  fc = (float *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     fc[rowofnode[node[i]]] = (float)resf[i][c];
                  }
                  break;
               case SUMA_int:
                  ic = (int *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     ic[rowofnode[node[i]]] = (int)resf[i][c];
                  }
                  break;
               case SUMA_short:
                  sc = (short *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     sc[rowofnode[node[i]]] = (short)resf[i][c];
                  }
                  break;
               case SUMA_byte:
                  bc = (byte *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     bc[rowofnode[node[i]]] = (byte)resf[i][c];
                  }
                  break;
               default:
                  SUMA_S_Err("Type not supported."
                             "Memory leak now present");
                  SUMA_RETURN(NULL); 
            }
         } /* for each column */
         break;   
      case SUMA_int:
         resd=NULL; resi=NULL; resi=NULL; ress=NULL; resb=NULL;
         resi = (int **)resv;
         for (c=0; c<nind; ++c) { /* for each column */
            if (ind) icol = ind[c];
            else icol = c;
            dc=NULL; fc=NULL; ic=NULL; sc=NULL; bc=NULL;
            switch (SUMA_ColType2TypeCast(
                           SUMA_TypeOfDsetColNumb(dset, icol))) {
               case SUMA_double:
                  dc = (double *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     dc[rowofnode[node[i]]] = (double)resi[i][c];
                  }
                  break;
               case SUMA_float:
                  fc = (float *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     fc[rowofnode[node[i]]] = (float)resi[i][c];
                  }
                  break;
               case SUMA_int:
                  ic = (int *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     ic[rowofnode[node[i]]] = (int)resi[i][c];
                  }
                  break;
               case SUMA_short:
                  sc = (short *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     sc[rowofnode[node[i]]] = (short)resi[i][c];
                  }
                  break;
               case SUMA_byte:
                  bc = (byte *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     bc[rowofnode[node[i]]] = (byte)resi[i][c];
                  }
                  break;
               default:
                  SUMA_S_Err("Type not supported."
                             "Memory leak now present");
                  SUMA_RETURN(NULL); 
            }
         } /* for each column */
         break;   
      case SUMA_short:
         resd=NULL; ress=NULL; ress=NULL; ress=NULL; resb=NULL;
         ress = (short **)resv;
         for (c=0; c<nind; ++c) { /* for each column */
            if (ind) icol = ind[c];
            else icol = c;
            dc=NULL; fc=NULL; ic=NULL; sc=NULL; bc=NULL;
            switch (SUMA_ColType2TypeCast(
                           SUMA_TypeOfDsetColNumb(dset, icol))) {
               case SUMA_double:
                  dc = (double *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     dc[rowofnode[node[i]]] = (double)ress[i][c];
                  }
                  break;
               case SUMA_float:
                  fc = (float *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     fc[rowofnode[node[i]]] = (float)ress[i][c];
                  }
                  break;
               case SUMA_int:
                  ic = (int *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     ic[rowofnode[node[i]]] = (int)ress[i][c];
                  }
                  break;
               case SUMA_short:
                  sc = (short *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     sc[rowofnode[node[i]]] = (short)ress[i][c];
                  }
                  break;
               case SUMA_byte:
                  bc = (byte *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     bc[rowofnode[node[i]]] = (byte)ress[i][c];
                  }
                  break;
               default:
                  SUMA_S_Err("Type not supported."
                             "Memory leak now present");
                  SUMA_RETURN(NULL); 
            }
         } /* for each column */
         break;      
      case SUMA_byte:
         resd=NULL; resb=NULL; resb=NULL; resb=NULL; resb=NULL;
         resb = (byte**)resv;
         for (c=0; c<nind; ++c) { /* for each column */
            if (ind) icol = ind[c];
            else icol = c;
            dc=NULL; fc=NULL; ic=NULL; sc=NULL; bc=NULL;
            switch (SUMA_ColType2TypeCast(
                           SUMA_TypeOfDsetColNumb(dset, icol))) {
               case SUMA_double:
                  dc = (double *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     dc[rowofnode[node[i]]] = (double)resb[i][c];
                  }
                  break;
               case SUMA_float:
                  fc = (float *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     fc[rowofnode[node[i]]] = (float)resb[i][c];
                  }
                  break;
               case SUMA_int:
                  ic = (int *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     ic[rowofnode[node[i]]] = (int)resb[i][c];
                  }
                  break;
               case SUMA_short:
                  sc = (short *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     sc[rowofnode[node[i]]] = (short)resb[i][c];
                  }
                  break;
               case SUMA_byte:
                  bc = (byte *)SDSET_VEC(dset,icol);
                  for (i=0; i<N_Node; ++i) {
                     bc[rowofnode[node[i]]] = (byte)resb[i][c];
                  }
                  break;
               default:
                  SUMA_S_Err("Type not supported."
                             "Memory leak now present");
                  SUMA_RETURN(NULL); 
            }
         } /* for each column */
         break;
      default:
         SUMA_SL_Err("Bad otype, why here?"); SUMA_RETURN(dset);
         break;
   } /* switch on type of output */

   if (rowofnode) SUMA_free(rowofnode); rowofnode = NULL;
   
   SUMA_RETURN(dset);
} 
                                
/* returns the value at a paricular column and particular
   node in double format.
   If you know the maximum number of nodes forming the domain
   of the dset, typically SO->N_Node then pass it for speed.
   Otherwise, pass -1 for last param.
*/
double SUMA_GetDsetNodeValInCol2(SUMA_DSET *dset, int ind, 
                                 int node, int Max_Node_Index)
{
   int ival = SUMA_GetNodeRow_FromNodeIndex_eng (dset, node, Max_Node_Index);
   if (ival < 0) return(0.0);
   else return( SUMA_GetDsetValInCol2( dset, ind, ival ) );
} 
/*!
   \brief Returns the value from column ind, row ival
   The value is stored in a double variable 
   0.0  in case of error. 
   \sa SUMA_GetDsetValInCol and SUMA_GetDsetNodeValInCol2
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
   complex *cmv=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!dset || !dset->dnel) { SUMA_SL_Err("NULL input"); SUMA_RETURN(0.0); }

   if (ind < 0 || ind > SDSET_VECNUM(dset) - 1) {
      SUMA_SL_Err("Bad column index");
      SUMA_RETURN(0.0);
   }

   if (ival < 0) {
      SUMA_SL_Err("ival < 0");
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
      case SUMA_complex:
         cmv = (complex *)dset->dnel->vec[ind];
         dval = CABS(cmv[ival]);
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
   short *shv=NULL;
   double *dv = NULL;
   float *fv=NULL;
   int *iv = NULL;
   char  *str=NULL, **cv = NULL;
   complex *cmv=NULL;
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
   if (LocalHead) {
      char stmp[1000]={""};
      snprintf(stmp, 998,
            "%s:\n"
            "dset %p, label %s, filen %s\n"
            "ind %d, ival %d\n"
            "ctp %d, vtp %d (b %d, i %d, f %d, d %d)\n",
            FuncName,
            dset, SDSET_LABEL(dset), SDSET_FILENAME(dset),
            ind, ival, 
            ctp, vtp, SUMA_byte, SUMA_int, SUMA_float, SUMA_double);
      SUMA_LH("%s", stmp);
   }
   switch (vtp) {
      case SUMA_byte:
         str = (char *)SUMA_malloc(50*sizeof(char));
         bv = (byte *)dset->dnel->vec[ind];
         sprintf(str,"%d",bv[ival]);
         *dval = (double)bv[ival];
         break;
      case SUMA_short:
         str = (char *)SUMA_malloc(50*sizeof(char));
         shv = (short *)dset->dnel->vec[ind];
         sprintf(str,"%d",shv[ival]);
         *dval = (double)shv[ival];
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
      case SUMA_complex:
         str = (char *)SUMA_malloc(100*sizeof(char));
         cmv = (complex *)dset->dnel->vec[ind];
         sprintf(str,"%f i%f",cmv[ival].r, cmv[ival].i);
         *dval = CABS(cmv[ival]);
         break;
      default:
         SUMA_SL_Err("This type is not supported yet.\n");
         SUMA_RETURN(NULL);
         break;
   }

   SUMA_LH("%s",str);
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

   V = (float *)SUMA_calloc(N_read,sizeof(float));
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
byte *SUMA_load_1D_n_mask(char *name, int N_Node, byte *omask, 
                          const char *oper, int *N_inmask)
{
   static char FuncName[]={"SUMA_load_1D_n_mask"};
   int kk;
   float *far=NULL;
   MRI_IMAGE * im=NULL;
   byte *out=NULL;

   SUMA_ENTRY;
   
   if (N_inmask) *N_inmask = -1;
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
         SUMA_S_Err("Bad operator\n");
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

   if (N_inmask) {
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
byte *SUMA_load_1D_b_mask(char *name, int N_Node, byte *omask, 
                          const char *oper, int *N_inmask)
{
   static char FuncName[]={"SUMA_load_1D_b_mask"};
   int kk;
   float *far=NULL;
   MRI_IMAGE * im=NULL;
   byte *out=NULL;

   SUMA_ENTRY;
   
   if (N_inmask) *N_inmask = -1;
   
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
         SUMA_S_Err("Bad operator\n");
         if (out && out != omask) SUMA_free(out); out = NULL;
         goto CLEANUP;   

      }
   } else {
      for (kk=0; kk<im->nx; ++kk) {
         if ((int)far[kk]) { out[kk] = 1; }
      }
   }
   
   if (N_inmask) {
      *N_inmask = 0;
      for (kk=0; kk<N_Node; ++kk) if (out[kk]) ++(*N_inmask);
   }
   CLEANUP:
   mri_free(im); im = NULL;
   SUMA_RETURN(out);
}

byte *SUMA_get_c_mask(char *mask, int N_Node, byte *omask, 
                      const char *oper, int *N_inmask)
{
   static char FuncName[]={"SUMA_get_c_mask"};
   int    clen, ninmask, i, kk;
	char * cmd;
   byte *bmask=NULL, *out=NULL;
   
   SUMA_ENTRY;
   
   if (N_inmask) *N_inmask = -1;
   if (!mask) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(bmask);
   }
   
   clen = strlen( mask );
	/* save original cmask command, as EDT_calcmask() is destructive */
	cmd = (char *)calloc((clen + 1), sizeof(char));
	strcpy( cmd,  mask);

	
   bmask = EDT_calcmask( cmd, &ninmask, N_Node );

	free( cmd );		cmd = NULL;	   /* free EDT_calcmask() string */

	if ( bmask == NULL ) {
	   SUMA_S_Err("Failed to compute mask from -cmask option");
      SUMA_RETURN(NULL);
	} 
   
   #if 0 /* a little check */
   for (kk=0; kk<ninmask; ++kk) {
      if (bmask[kk]) fprintf(stderr," %d/%d %d\n", kk, ninmask, bmask[kk]);
   }
   #endif
   
	if ( ninmask != N_Node ) {
	   SUMA_S_Err("Input and cmask datasets do not have "
		            "the same dimensions\n" );
	   fprintf(SUMA_STDERR,"Have %d in mask and %d nodes\n", ninmask, N_Node);
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
         SUMA_S_Err("Bad operator\n");
         if (out && out != omask) { SUMA_free(out); out = NULL; }
         else { if (out) SUMA_free(out); out = NULL; }
         goto CLEANUP;   
      }
   }
   
   if (N_inmask) {
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
byte * SUMA_load_all_command_masks(
   char *bmaskname, char *nmaskname, 
   char *cmask, int N_Node, int *N_inmask)
{
   static char FuncName[]={"SUMA_load_all_command_masks"};
   byte *nmask=NULL;

   SUMA_ENTRY;

   *N_inmask = -1;   /* indicates an error */
   
   if (bmaskname) {
      if (!(nmask = SUMA_load_1D_b_mask(
                        bmaskname, N_Node, nmask, "and", N_inmask))) {
         SUMA_S_Err("Failed loading mask");
         if (nmask) SUMA_free(nmask); nmask=NULL; SUMA_RETURN(nmask);
      }
   }
   if (cmask) {
      if (!(nmask = SUMA_get_c_mask(
                        cmask, N_Node, nmask, "and", N_inmask))) {
         SUMA_S_Err("Failed loading mask");
         if (nmask) SUMA_free(nmask); nmask=NULL; SUMA_RETURN(nmask);
      }
   }
   if (nmaskname) {
      if (!(nmask = SUMA_load_1D_n_mask(
                        nmaskname, N_Node, nmask, "and", N_inmask))) {
         SUMA_S_Err("Failed loading mask");
         if (nmask) SUMA_free(nmask); nmask=NULL; SUMA_RETURN(nmask);
      }
   }
   
   
   if (*N_inmask < 0) 
      *N_inmask = 0; /* Remove error flag, 
                        even if nmask is NULL (no mask to speak of) */
   
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
byte * SUMA_indexlist_2_bytemask(   
      int *ind_list, int N_ind_list, 
      int N_mask, int *N_inmask)   
{
   static char FuncName[]={"SUMA_indexlist_2_bytemask"};
   int i = 0, cnt, ign = 0; 
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
         if (!ign) {
            SUMA_S_Warn("Values in ind_list exceed N_mask!\n"); 
         }
         ++ign;
      }
   }

   if (ign) {
      fprintf(SUMA_STDERR,
   "%s:   %d values in indexlist ignored because they are >= N_mask of %d\n",
         FuncName, ign, N_mask);
   }
   
   BYE:
   if (N_inmask) *N_inmask = cnt;
   SUMA_RETURN(bmask);
}

byte * SUMA_Meshbmask_2_IndexListbmask(
   byte *Mbmask, int N_Mbmask, int *ind_list, int N_ind_list, int *N_ILbmask)
{
   static char FuncName[]={"SUMA_Meshbmask_2_IndexListbmask"};
   int i = 0, cnt, ign = 0; 
   byte *ILbmask = NULL;

   SUMA_ENTRY; 

   cnt = -1; 
   if (!ind_list) {
      SUMA_S_Err("NULL ind_list");
      goto BYE;
   }
   
   if (!(ILbmask = (byte *)SUMA_calloc(N_ind_list, sizeof(byte)))) { 
      SUMA_SL_Crit("Failed to allocate (macro)"); 
      goto BYE;
   }
   
   if (!Mbmask) { /* default, take whole thing */
      memset(ILbmask, 1, sizeof(byte)*N_ind_list);
      cnt = N_ind_list; 
      goto BYE;
   }
   
   for (i=0; i<N_ind_list; ++i) {
      if (ind_list[i] < N_Mbmask) {
         if (Mbmask[ind_list[i]]) {
            ILbmask[i]=1; ++cnt;
         }
      } else {
         if (!ign) {
            SUMA_S_Warn("Values in ind_list exceed N_mask!\n"); 
         }
         ++ign;
      }
   }
   
   if (ign) {
      fprintf(SUMA_STDERR,
   "%s:   %d values in indexlist ignored because they are >= N_mask of %d\n",
         FuncName, ign, N_Mbmask);
   }
   
   BYE:
   if (N_ILbmask) *N_ILbmask = cnt;
   SUMA_RETURN(ILbmask);
}

/*!
   \brief c = SUMA_DsetCol2FloatFullSortedColumn (
                        dset, ico, nmaskp, fillval,
                        N_Node, N_inmaskp, MergeMask);
           Returns a float copy 'c' column 'ico' in a dset
           If 'dset' is sparse, the copy is filled such that
           c[i] has the value for node i. 
   \param dset (SUMA_DSET *) Dataset pointer
   \param ico (int) column desired from dset
   \param nmaskp (byte **) pointer to node mask vector.

                 If you have a node mask nmask (byte *) that
                 you want applied to 'c' then pass &nmask to
                 this function. nmask is internally modified
                 so that nmask[i] is set to 0 if node 'i' does 
                 have entries in dset.

                 If you do not have a node mask (i.e. you want
                 to use all nodes) then you can set nmask = NULL
                 and pass &nmask to this function nonetheless.
                 When the function returns, it will have set
                 nmask to reflect the nodes present in dset.

                 The previous two conditions apply the first time 
                 you are calling this function with a particular 
                 sparse dset. For subsequent calls, your nmask
                 will not change, so you can pass NULL for nmaskp.
                 
                 Note that if you specify no mask to begin with 
                 and dset is not sparse, then  *nmaskp will
                 be NULL on return
   \param  fillval (float) value to put in c[i] where i is a node
                 not present in the dset.
   \param  N_Node (int) Total number of nodes in surface where dset
                        is defined
   \param  N_inmask (int *) To contain the total number of nodes in 
                            *nmaskp. If nmaskp was set to NULL then
                            *N_inmask = -1
   \param MergeMask (SUMA_Boolean) A flag to indicate that node mask
                                   merger is needed. Set to YUP the 
                                   first time you call this function
                                   for a particular dataset. 
   \sa   SUMA_MakeSparseColumnFullSorted and   SUMA_DsetCol2Float 
   \sa   SUMA_DsetCol2DoubleFullSortedColumn   
   
   MAKE SURE YOU ALSO MODIFY Double version of this function when you make
   changes here
*/       
float * SUMA_DsetCol2FloatFullSortedColumn(  
            SUMA_DSET *dset, int ico, byte **nmaskp, 
            float fillval, int N_Node, int *N_inmask, 
            SUMA_Boolean MergeMask)
{
   static char FuncName[]={"SUMA_DsetCol2FloatFullSortedColumn"};
   float *fin_orig = NULL;
   byte *locmask = NULL;
   byte *nmask = NULL;
   int n=0, N_nmask=0, jj=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (nmaskp) nmask = *nmaskp;
   if (N_inmask) *N_inmask =  -1;

   /* get a float copy of the data column */
   fin_orig = SUMA_DsetCol2Float (dset, ico , 1);
   if (!fin_orig) {
      SUMA_SL_Crit("Failed to get copy of column. Woe to thee!");
      SUMA_RETURN(NULL);
   }

   /* make sure column is not sparse, one value per node */
   if (MergeMask) { /* masks may need merging */
      SUMA_LH( "Mask merger");
      locmask = NULL;
      if (!SUMA_MakeSparseColumnFullSorted(
               &fin_orig, SDSET_VECFILLED(dset), fillval, 
               &locmask, dset, N_Node)) {
         SUMA_S_Err("Failed to get full column vector");
         SUMA_RETURN(NULL);
      }
      if (locmask) {
         SUMA_LH( "Something was filled in SUMA_MakeSparseColumnFullSorted\n" );
         /* something was filled in good old SUMA_MakeSparseColumnFullSorted */
         if (nmask) {   /* combine locmask with nmask */
            SUMA_LH( "Merging masks\n" );
            for (jj=0; jj < N_Node; ++jj) { 
               if (nmask[jj] && !locmask[jj]) nmask[jj] = 0; 
            }   
         } else { nmask = locmask; }
      } 
      if (nmask) {
         N_nmask = 0;
         for (n=0; n<N_Node; ++n) { if (nmask[n]) ++ N_nmask; }
         if (LocalHead) 
            fprintf( SUMA_STDERR, 
                     "%s: Node mask has %d nodes in mask)\n", 
                     FuncName, N_nmask);
         if (!N_nmask) {
            SUMA_S_Warn("Empty mask, nothing to do");
         }
      } else {
         N_nmask = N_Node;
      }
      if (nmaskp) { /* return the mask that we have created or modified*/
         *nmaskp = nmask; 
         if (locmask && nmask != locmask) { /* free locally created mask */
            SUMA_free(locmask);
         }
         locmask = NULL;
      }
      if (N_inmask) *N_inmask = N_nmask; 
   } else {
      SUMA_LH( "going to SUMA_MakeSparseColumnFullSorted");
      if (!SUMA_MakeSparseColumnFullSorted(
               &fin_orig, SDSET_VECFILLED(dset), 
               fillval, NULL, dset, N_Node)) {
         SUMA_S_Err("Failed to get full column vector");
         SUMA_RETURN(NULL);
      }
      /* no need for reworking nmask and locmask on this pass...*/
      if (N_inmask) *N_inmask =  -1; /* nothing to do */
   }


   SUMA_RETURN(fin_orig);
}
/*! double precision version of SUMA_DsetCol2FloatFullSortedColumn */
double * SUMA_DsetCol2DoubleFullSortedColumn(  
            SUMA_DSET *dset, int ico, byte **nmaskp, 
            double fillval, int N_Node, int *N_inmask, 
            SUMA_Boolean MergeMask)
{
   static char FuncName[]={"SUMA_DsetCol2DoubleFullSortedColumn"};
   double *fin_orig = NULL;
   byte *locmask = NULL;
   byte *nmask = NULL;
   int n=0, N_nmask=0, jj=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (nmaskp) nmask = *nmaskp;
   *N_inmask =  -1;

   /* get a double copy of the data column */
   fin_orig = SUMA_DsetCol2Double (dset, ico , 1);
   if (!fin_orig) {
      SUMA_SL_Crit("Failed to get copy of column. Woe to thee!");
      SUMA_RETURN(NULL);
   }

   /* make sure column is not sparse, one value per node */
   if (MergeMask) { /* masks may need merging */
      SUMA_LH( "Mask merger");
      locmask = NULL;
      if (!SUMA_MakeSparseDoubleColumnFullSorted(
               &fin_orig, SDSET_VECFILLED(dset), fillval, 
               &locmask, dset, N_Node)) {
         SUMA_S_Err("Failed to get full column vector");
         SUMA_RETURN(NULL);
      }
      if (locmask) {
         SUMA_LH( "Something was filled in"
                  "SUMA_MakeSparseDoubleColumnFullSorted\n" );
         /* something was filled in good old 
            SUMA_MakeSparseDoubleColumnFullSorted */
         if (nmask) {   /* combine locmask with nmask */
            SUMA_LH( "Merging masks\n" );
            for (jj=0; jj < N_Node; ++jj) { 
               if (nmask[jj] && !locmask[jj]) nmask[jj] = 0; 
            }   
         } else { nmask = locmask; }
      } 
      if (nmask) {
         N_nmask = 0;
         for (n=0; n<N_Node; ++n) { if (nmask[n]) ++ N_nmask; }
         if (LocalHead) 
            fprintf( SUMA_STDERR, 
                     "%s: Node mask has %d nodes in mask)\n", 
                     FuncName, N_nmask);
         if (!N_nmask) {
            SUMA_S_Warn("Empty mask, nothing to do");
         }
      } else {
         N_nmask = N_Node;
      }
      if (nmaskp) { /* return the mask that we have created or modified*/
         *nmaskp = nmask; 
         if (locmask && nmask != locmask) { /* free locally created mask */
            SUMA_free(locmask);
         }
         locmask = NULL;
      }
      *N_inmask = N_nmask; 
   } else {
      SUMA_LH( "going to SUMA_MakeSparseDoubleColumnFullSorted");
      if (!SUMA_MakeSparseDoubleColumnFullSorted(
               &fin_orig, SDSET_VECFILLED(dset), 
               fillval, NULL, dset, N_Node)) {
         SUMA_S_Err("Failed to get full column vector");
         SUMA_RETURN(NULL);
      }
      /* no need for reworking nmask and locmask on this pass...*/
      *N_inmask =  -1; /* nothing to do */
   }


   SUMA_RETURN(fin_orig);
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
   \return  YUP: OK
            NOPE: NOK 
            
   \sa SUMA_DsetCol2FloatFullSortedColumn 
   MAKE SURE YOU ALSO MODIFY Double version of this function when you make
   changes here 
*/
SUMA_Boolean SUMA_MakeSparseColumnFullSorted (
      float **vp, int N_v, 
      float mask_val, byte **bmp, 
      SUMA_DSET *dset, int N_Node)
{
   static char FuncName[]={"SUMA_MakeSparseColumnFullSorted"};
   double range[2];
   float *vc=NULL, *vr=NULL;
   byte *bm = NULL;
   int  loc[2], *nip=NULL, i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if ((!vp || !*vp) && !bmp) { /* OK to come in with no data, just to get bmp */
      SUMA_S_Err("Too NULL an input, need at least bmp or vp!");
      SUMA_RETURN(NOPE);
   }
   
   /* the column vector */
   if (vp) vc = *vp; 
   
   /* get the node index column */
   if (!(nip = SUMA_GetNodeDef(dset))) {
      /* No node index column, check based on number of values */
      if (N_v == N_Node) {
         SUMA_LH( "Fullness established based on \n"
                  "number of nodes and values in v");
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
         SUMA_S_Err( "Number of values in v not equal to vec_filled.\n"
                     "Cannot proceed.");
         SUMA_RETURN(NOPE);
      }
      
      /* number of values is same as filled values, good, now proceed */
      if (  SDSET_VECFILLED(dset) == N_Node && 
            nip[SDSET_VECFILLED(dset)-1] == N_Node -1 &&
            nip[SDSET_VECFILLED(dset)-1-SDSET_VECFILLED(dset)/2] == 
                  N_Node -1 -SDSET_VECFILLED(dset)/2 &&
            nip[SDSET_VECFILLED(dset)-1-SDSET_VECFILLED(dset)/3] == 
                  N_Node -1 -SDSET_VECFILLED(dset)/3 &&
            SDSET_IS_SORTED(dset)) {   /* likely a full column */
         SUMA_LH( "Fullness established by loose examination \n"
                  "of node defintion column.");
         SUMA_RETURN(YUP);   
      }
      /* if you get here then you do not have a 
         full list or the list is not sorted*/
      SUMA_LH("Creating vr");
      if (vc) vr = (float *)SUMA_calloc(N_Node, sizeof(float));
      if (bmp) {
         if (*bmp) {
            SUMA_S_Err("*bmp must be NULL");
            SUMA_RETURN(NOPE);
         }
         bm = (byte *)SUMA_calloc(N_Node, sizeof(byte));
      } else bm = NULL;
      
      if ((vc && !vr) || (bmp && !bm)) {
         SUMA_S_Crit("Failed to allocate");
         SUMA_RETURN(NOPE);
      }
      if (vr) {
         for (i=0; i<N_Node; ++i) {
            vr[i] = mask_val; /* if (bm) bm[i] = 0; */
         }
      }
      for (i=0; i<SDSET_VECFILLED(dset); ++i) {
         if (vc) vr[nip[i]] = vc[i]; 
         if (bm) bm[nip[i]] = 1;
      }
      
      
      /* Now free old v */
      if (vc) SUMA_free(vc); vc = NULL;
      
      /* stow away for return */
      if (bmp) { *bmp = bm; bm = NULL; }  
      if (vp) *vp = vr; vr = NULL;
      
      SUMA_RETURN(YUP);
   }
   
   /* Should not get here */
   SUMA_RETURN(NOPE);
}

/*! double precision version of SUMA_MakeSparseColumnFullSorted */
SUMA_Boolean SUMA_MakeSparseDoubleColumnFullSorted (
      double **vp, int N_v, 
      double mask_val, byte **bmp, 
      SUMA_DSET *dset, int N_Node)
{
   static char FuncName[]={"SUMA_MakeSparseDoubleColumnFullSorted"};
   double *vc=NULL, *vr=NULL;
   byte *bm = NULL; 
   double range[2];
   int  loc[2], *nip=NULL, i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if ((!vp || !*vp) && !bmp) { /* OK if user just wants update of bmp */
      SUMA_S_Err("Too NULL an input, need at least bmp or vp!");
      SUMA_RETURN(NOPE);
   }
   
   /* the column vector */
   if (vp) vc = *vp; 
   
   /* get the node index column */
   if (!(nip = SUMA_GetNodeDef(dset))) {
      /* No node index column, check based on number of values */
      if (N_v == N_Node) {
         SUMA_LH( "Fullness established based on \n"
                  "number of nodes and values in v");
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
         SUMA_S_Err( "Number of values in v not equal to vec_filled.\n"
                     "Cannot proceed.");
         SUMA_RETURN(NOPE);
      }
      
      /* number of values is same as filled values, good, now proceed */
      if (  SDSET_VECFILLED(dset) == N_Node && 
            nip[SDSET_VECFILLED(dset)-1] == N_Node -1 &&
            nip[SDSET_VECFILLED(dset)-1-SDSET_VECFILLED(dset)/2] == 
                  N_Node -1 -SDSET_VECFILLED(dset)/2 &&
            nip[SDSET_VECFILLED(dset)-1-SDSET_VECFILLED(dset)/3] == 
                  N_Node -1 -SDSET_VECFILLED(dset)/3 &&
            SDSET_IS_SORTED(dset)) {   /* likely a full column */
         SUMA_LH( "Fullness established by loose examination \n"
                  "of node defintion column.");
         SUMA_RETURN(YUP);   
      }
      /* if you get here then you do not have a 
         full list or the list is not sorted*/
      SUMA_LH("Creating vr");
      if (vc) vr = (double *)SUMA_calloc(N_Node, sizeof(double));
      if (bmp) {
         if (*bmp) {
            SUMA_S_Err("*bmp must be NULL");
            SUMA_RETURN(NOPE);
         }
         bm = (byte *)SUMA_calloc(N_Node, sizeof(byte));
      } else bm = NULL;
      
      if ((vc && !vr) || (bmp && !bm)) {
         SUMA_S_Crit("Failed to allocate");
         SUMA_RETURN(NOPE);
      }
      if (vr) {
         for (i=0; i<N_Node; ++i) {
            vr[i] = mask_val; /* if (bm) bm[i] = 0; */
         }
      }
      for (i=0; i<SDSET_VECFILLED(dset); ++i) {
         if (vc) vr[nip[i]] = vc[i]; 
         if (bm) bm[nip[i]] = 1;
      }
      
      
      /* Now free old v */
      if (vc) SUMA_free(vc); vc = NULL;
      
      /* stow away for return */
      if (bmp) { *bmp = bm; bm = NULL; }  
      if (vp) *vp = vr; vr = NULL;
      
      SUMA_RETURN(YUP);
   }
   
   /* Should not get here */
   SUMA_RETURN(NOPE);
}
 
/*!
   \brief Copies the contents of a float vector into a NI_element column 
   SUMA_Float2DsetCol (dset,  ind,  V, FilledOnly, replacemask);
   
   \param nel (NI_element *)
   \param ind (int) index of column to be filled with values in V
   \param V (float *) vector containing the column's contents (see number of values copied below).
   \param FilledOnly (int) 0 = copy from index 0 to dset->dnel->vec_len or SDSET_VECLEN(dset)
                           1 = copy from index 0 to dset->dnel->vec_filled or SDSET_VECFILLED(dset)
   \param replacemask (byte *) if not null, then copy value in index i only IF replacemask[i] == 1
                               if null, copy all indices i
   The values in V replace those in nel!
   No new columns in nel are created.
   
   \sa SUMA_DsetCol2Float , SUMA_Vec2DsetCol
*/
void SUMA_BadOptimizerBadBad(void){ return; }

#define SUMA_COL_FILL(vv, VV, tp){\
   if (!replacemask) {\
      if (nip) { \
         for (i=0; i<N_read; ++i) { vv[i] = (tp)VV[nip[i]]; } \
      } else { \
         for (i=0; i<N_read; ++i) { vv[i] = (tp)VV[i]; } \
      }\
   } else { \
     if (nip) { \
         for (i=0; i<N_read; ++i)  { \
            if (replacemask[nip[i]]) { \
               vv[i] = (tp)VV[nip[i]]; \
                /* That call was turned off again because 
                  this macro misbehaved again Jan. 2011.
                  Putting brackets at all spots seems to have
                  fixed the problem, and that call below seems
                  unnecessary for now.    ZSS Jan 04 2011 */   \
               /* SUMA_BadOptimizerBadBad();  Dec. 03 07 */ \
            }  \
         }  \
     } else {  \
         for (i=0; i<N_read; ++i) { \
            if (replacemask[i]) { \
               vv[i] = (tp)VV[i];  /* (int) --> (tp) ZSS Jan 04 2011 */ \
            } \
         }  \
     }   \
   }  \
}

int SUMA_Float2DsetCol (SUMA_DSET *dset, int ind, 
                        float *V, int FilledOnly, 
                        byte *replacemask)
{
   static char FuncName[]={"SUMA_Float2DsetCol"};
   int i = -1, N_read = -1, *iv = NULL, *nip=NULL, nnn=-1, NodeDBG=0;
   float *fv = NULL;
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
      
   if (!dset) { SUMA_RETURN(0); }
   
   if (ind < 0 || ind > SDSET_VECNUM(dset) - 1) {
      SUMA_S_Errv("Bad col index (%d/%d)\n", ind, SDSET_VECNUM(dset) - 1);
      SUMA_RETURN(0);
   }
   
   if (FilledOnly) {
      N_read = SDSET_VECFILLED(dset);
   } else {
      N_read = SDSET_VECLEN(dset);
   }
   
   if (LocalHead) SUMA_ShowDset(dset, 0, NULL);

   #if 0
      /* MACROS below have given me hell with optimizer related troubles
         Code left here for sanity checks */
   NodeDBG =  136029;     
   if (LocalHead) fprintf(stderr,"%s: Pre replacement, node %d=%f\n"
                     "New value %f, mask = %d\n",
               FuncName, NodeDBG, 
               SUMA_GetDsetNodeValInCol2(dset,ind, 
                                          NodeDBG, -1),
               V[NodeDBG], replacemask ? replacemask[NodeDBG]:1);
   #endif
   
   ctp = SUMA_TypeOfDsetColNumb(dset, ind); 
   vtp = SUMA_ColType2TypeCast (ctp) ;
   nip = SUMA_GetNodeDef(dset);
   switch (vtp) {
      case SUMA_int:
         iv = (int *)dset->dnel->vec[ind];
         SUMA_COL_FILL(iv, V, int);
         break;
      case SUMA_float:
         fv = (float *)dset->dnel->vec[ind];
         #if 1 /* Assuming MACRO is not causing trouble */
            SUMA_COL_FILL(fv, V, float);
         #else /* Try this block to test optimizer bug */
   if (!replacemask) {
      if (nip) { for (i=0; i<N_read; ++i) { fv[i] = (float)V[nip[i]]; } }
      else { for (i=0; i<N_read; ++i) { fv[i] = (float)V[i]; } }
   } else { 
     if (nip) { 
         for (i=0; i<N_read; ++i)  { 
            if (replacemask[nip[i]]) { 
               fv[i] = (float)V[nip[i]]; 
                /* That call was turned off again because 
                  this macro misbehaved again Jan. 2011.
                  Putting brackets at all spots seems to have
                  fixed the problem, and that call below seems
                  unnecessary for now.    ZSS Jan 04 2011 */   
               /* SUMA_BadOptimizerBadBad();  Dec. 03 07 */ 
            }  
         }  
     } else {  
         for (i=0; i<N_read; ++i) { 
            if (replacemask[i]) { 
               fv[i] = (float)V[i];  /* (int) --> (float) ZSS Jan 04 2011 */ 
            } 
         }  
     }   
   }  
         #endif
         break;
      default:
         SUMA_SL_Err("This type is not supported.\n");
         SUMA_RETURN(0);
         break;
   }
   
   #if 0
   if (LocalHead) fprintf(stderr,"%s: Post replacement, node %d=%f\n"
                     "New value %f, mask = %d\n",
               FuncName, NodeDBG, 
               SUMA_GetDsetNodeValInCol2(dset,ind, 
                                          NodeDBG, -1),
               V[NodeDBG], replacemask ? replacemask[NodeDBG]:1);
   #endif
   /* reset generic attributes */
   SUMA_AddGenDsetColAttr (dset, ctp, dset->dnel->vec[ind], 1, ind, 0);
  
   SUMA_RETURN(1);
}
/*!
   \brief Copies the contents of a vector into a NI_element column 
   SUMA_Vec2DsetCol (dset,  ind,  V, Vtp, FilledOnly, replacemask);
   
   \param nel (NI_element *)
   \param ind (int) index of column to be filled with values in V
   \param V (void *) vector containing the column's contents 
               (see number of values copied below).
   \param Vtp (SUMA_VARTYPE) type of data in V
   \param FilledOnly (int) 0 = copy from index 0 to dset->dnel->vec_len 
                                                 or SDSET_VECLEN(dset)
                           1 = copy from index 0 to dset->dnel->vec_filled 
                                                 or SDSET_VECFILLED(dset)
   \param replacemask (byte *) if not null, then copy value in index i 
                                             only IF replacemask[i] == 1
                               if null, copy all indices i
   The values in V replace those in nel!
   No new columns in nel are created.
   
   \sa SUMA_DsetCol2Float 
*/
int SUMA_Vec2DsetCol (SUMA_DSET *dset, int ind, 
                        void *V, SUMA_VARTYPE Vtp,
                        int FilledOnly, 
                        byte *replacemask)
{
   static char FuncName[]={"SUMA_Vec2DsetCol"};
   int i = -1, N_read = -1, *iv = NULL, *iV=NULL, *nip=NULL, nnn=-1;
   float *fv = NULL, *fV=NULL;
   double *dV=NULL;
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
      
   if (!dset) { SUMA_RETURN(0); }
   if (!V) {
      SUMA_S_Err("NULL V");
      SUMA_RETURN(0);
   }
   if (ind < 0 || ind > SDSET_VECNUM(dset) - 1) {
      SUMA_SL_Err("Bad col index");
      SUMA_RETURN(0);
   }
   
   if (FilledOnly) {
      N_read = SDSET_VECFILLED(dset);
   } else {
      N_read = SDSET_VECLEN(dset);
   }
   
   if (LocalHead) SUMA_ShowDset(dset, 0, NULL);
   ctp = SUMA_TypeOfDsetColNumb(dset, ind); 
   vtp = SUMA_ColType2TypeCast (ctp) ;
   nip = SUMA_GetNodeDef(dset);
   switch (Vtp) {
     case SUMA_int:
         iV = (int *)V;
         switch (vtp) {
            case SUMA_int:
               iv = (int *)dset->dnel->vec[ind];
               SUMA_COL_FILL(iv, iV, int);
               break;
            case SUMA_float:
               fv = (float *)dset->dnel->vec[ind];
               SUMA_COL_FILL(fv, iV, float);
               break;
            default:
               SUMA_SL_Err("This type is not supported.\n");
               SUMA_RETURN(0);
               break;
         }
         break;
      case SUMA_float:
         fV = (float *)V;
         switch (vtp) {
            case SUMA_int:
               iv = (int *)dset->dnel->vec[ind];
               SUMA_COL_FILL(iv, fV, int);
               break;
            case SUMA_float:
               fv = (float *)dset->dnel->vec[ind];
               SUMA_COL_FILL(fv, fV, float);
               break;
            default:
               SUMA_SL_Err("This type is not supported.\n");
               SUMA_RETURN(0);
               break;
         }
         break;
      case SUMA_double:
         dV = (double *)V;
         switch (vtp) {
            case SUMA_int:
               iv = (int *)dset->dnel->vec[ind];
               SUMA_COL_FILL(iv, dV, int);
               break;
            case SUMA_float:
               fv = (float *)dset->dnel->vec[ind];
               SUMA_COL_FILL(fv, dV, float);
               break;
            default:
               SUMA_SL_Err("This type is not supported.\n");
               SUMA_RETURN(0);
               break;
         }
         break;
      
      default:
         SUMA_SL_Err("This type is not supported.\n");
         SUMA_RETURN(0);
         break;
   }   
  
   /* reset generic attributes */
   SUMA_AddGenDsetColAttr (dset, ctp, dset->dnel->vec[ind], 1, ind, 0);
   if (LocalHead) SUMA_ShowDset(dset, 0, NULL);
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
   float *V=NULL, *fv = NULL, fac;
   byte *bv=NULL;
   double *dv=NULL;
   short *sv=NULL;
   complex *cmv=NULL;
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   SUMA_Boolean LocalHead = NOPE;
   
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
   fac = SUMA_GetBrickFactor(dset, ind); if (fac == 0.0f) fac = 1.0;
   SUMA_LH("Col fac (if any) %f\n", fac);
   V = (float *)SUMA_calloc(N_read, sizeof(float));
   if (!V) { SUMA_SL_Crit("Failed to allocate for V."); SUMA_RETURN(NULL); }
   vtp = SUMA_ColType2TypeCast (ctp) ;
   switch (vtp) {
      case SUMA_byte:
         bv = (byte *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (float)(bv[i]*fac);
         break;
      case SUMA_int:
         iv = (int *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (float)iv[i];
         break;
      case SUMA_short:
         sv = (short *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (float)(sv[i]*fac);
         break;
      case SUMA_float:
         fv = (float *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = fv[i];
         break;
      case SUMA_double:
         dv = (double *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (float)dv[i];
         break;
      case SUMA_complex:
         cmv = (complex *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (float)CABS(cmv[i]);
         break;
      default:
         SUMA_SL_Err("This type is not supported.\n");
         SUMA_free(V);
         SUMA_RETURN(NULL);
         break;
   }
   
   SUMA_RETURN(V);
}
int * SUMA_DsetCol2Int (SUMA_DSET *dset, int ind, int FilledOnly)
{
   static char FuncName[]={"SUMA_DsetCol2Int"};
   int i = -1, *V=NULL, N_read = -1, *iv = NULL;
   float *fv = NULL;
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   SUMA_Boolean LocalHead = NOPE;
   
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

   V = (int *)SUMA_calloc(N_read, sizeof(int));
   if (!V) { SUMA_SL_Crit("Failed to allocate for V."); SUMA_RETURN(NULL); }
   vtp = SUMA_ColType2TypeCast (ctp) ;
   switch (vtp) {
      case SUMA_int:
         iv = (int *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (int)iv[i];
         break;
      case SUMA_float:
         fv = (float *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (int)fv[i];
         break;
      default:
         SUMA_SL_Err("This type is not supported.\n");
         SUMA_free(V);
         SUMA_RETURN(NULL);
         break;
   }
   
   SUMA_RETURN(V);
}
double * SUMA_DsetCol2Double (SUMA_DSET *dset, int ind, int FilledOnly)
{
   static char FuncName[]={"SUMA_DsetCol2Double"};
   int i = -1, N_read = -1, *iv = NULL;
   double *V=NULL;
   float *fv = NULL;
   double *dv=NULL;
   byte *bv=NULL;
   complex *cv=NULL;
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   SUMA_Boolean LocalHead = NOPE;
   
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

   V = (double *)SUMA_calloc(N_read, sizeof(double));
   if (!V) { SUMA_SL_Crit("Failed to allocate for V."); SUMA_RETURN(NULL); }
   vtp = SUMA_ColType2TypeCast (ctp) ;
   switch (vtp) {
      case SUMA_byte:
         bv = (byte *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (float)bv[i];
         break;
      case SUMA_int:
         iv = (int *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (float)iv[i];
         break;
      case SUMA_float:
         fv = (float *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = fv[i];
         break;
      case SUMA_double:
         dv = (double *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (float)dv[i];
         break;
      case SUMA_complex:
         cv = (complex *)dset->dnel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (float)CABS(cv[i]);
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
      SUMA_NEL_GET_STRING(nelb, 0, 0, cnm); 
         /* cnm is a pointer copy here, do not free */
      cnm = SUMA_Get_Sub_String(cnm, SUMA_NI_CSS, ind);
      /* SUMA_LH(cnm); */
   }
   if (cnm) {
      ctp = SUMA_Col_Type(cnm); SUMA_free(cnm); cnm = NULL;
      SUMA_RETURN(ctp);
   }
   
   /* try AFNI's */
   SUMA_LH("Fetching Type a la afni");
   cnm = NI_get_attribute(dset->dnel, "ni_type");
   if (cnm) {
      SUMA_LH("%s",cnm);
      iar = decode_type_string( cnm ); 
      if (iar) {
         ctp = iar->ar[ind];   
            /* this is not the same as SUMA's column type, 
               it is just data type */
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
            case SUMA_complex:
               ctp = SUMA_NODE_COMPLEX;
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

SUMA_Boolean SUMA_isSameDsetColTypes(SUMA_DSET *dset1, SUMA_DSET *dset2) 
{
   static char FuncName[]={"SUMA_isSameDsetColTypes"};
   int *ctpv = NULL, ind = 0;
   char *cnm1 = NULL, *cnm2=NULL, **sc = NULL;
   int_array *iar1 = NULL, *iar2 = NULL;
   char stmp[100];
   NI_element *nelb1=NULL;
   NI_element *nelb2=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset1 || !dset2) {  
      SUMA_SL_Err("NULL Dsets");
      SUMA_RETURN(NOPE);
   }
   SUMA_LHv("Checking sb numbers %d vs %d\n", 
         SDSET_VECNUM(dset1), SDSET_VECNUM(dset2));
   if (SDSET_VECNUM(dset1) != SDSET_VECNUM(dset2)) {
      SUMA_RETURN(NOPE);
   }
      
   /* try SUMA's */
   nelb1 = SUMA_FindDsetAttributeElement(dset1, "COLMS_TYPE");
   nelb2 = SUMA_FindDsetAttributeElement(dset2, "COLMS_TYPE");
   if (nelb1 && nelb2) {
      SUMA_NEL_GET_STRING(nelb1, 0, 0, cnm1);
      SUMA_NEL_GET_STRING(nelb2, 0, 0, cnm2); 
         /* cnm is a pointer copy here, do not free */
   }
   
   if (cnm1 && cnm2) {
      SUMA_LHv("Types a la SUMA\n%s\nvs.\n%s\n",
         cnm1,cnm2); 
      if (strcmp(cnm1,cnm2)) {/* wholesale comparison */
         SUMA_RETURN(NOPE);
      } else {
         SUMA_RETURN(YUP); 
      }
   }
    
   /* try AFNI's */
   SUMA_LH("Fetching Type a la afni");
   cnm1 = NI_get_attribute(dset1->dnel, "ni_type");
   cnm2 = NI_get_attribute(dset2->dnel, "ni_type");
   if (cnm1 && cnm2) {
      SUMA_LH("%s %s", cnm1, cnm2);
      iar1 = decode_type_string( cnm1 ); 
      iar2 = decode_type_string( cnm1 ); 
      if (iar1 && iar2) {
         for (ind=0; ind<SDSET_VECNUM(dset1); ++ind) {
            if (iar1->ar[ind] != iar2->ar[ind])  SUMA_RETURN(NOPE);
            NI_free(iar1->ar); NI_free(iar1); iar1 = NULL;
            NI_free(iar2->ar); NI_free(iar2); iar2 = NULL;
         }
         SUMA_RETURN(YUP); /* all columns matched */
      } else {
         SUMA_RETURN(NOPE);
      }
   }
   
   SUMA_SL_Err("Failed to determine types");
   SUMA_RETURN(NOPE);
}

SUMA_Boolean SUMA_AddNodeIndexColumn(SUMA_DSET *dset, int N_Node) 
{
   static char FuncName[]={"SUMA_AddNodeIndexColumn"};
   double range[2];
   int *ind=NULL, i, N_i;
   float *T = NULL;
   int *Ti = NULL;
   byte *vis=NULL;
   SUMA_Boolean OKfirstCol = NOPE;
   SUMA_Boolean LocalHead = NOPE;
       
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(NOPE);
   if (SUMA_isCIFTIDset(dset)) {
      SUMA_S_Err("Have yet to work out the logic for this type");
      SUMA_RETURN(NOPE);
   }
   /* check for obvious insult */
   if (SDSET_VECLEN(dset) > N_Node) {
      SUMA_SL_Err("more values in dset than nodes in surface.");
      SUMA_RETURN(NOPE);
   }
   /* Check for Col Index*/
   ind = SUMA_GetNodeDef (dset);
   if (!ind) {
      SUMA_LH("No node index column");
      if (SUMA_isGraphDset(dset)) {
         if (dset->Aux->matrix_shape == MAT_SPARSE) {
            SUMA_S_Err("No gessing possible");
            SUMA_RETURN(NOPE);
         }
         SUMA_LH("Graph dset with explicit indexing possible, nothing to do."
                 "You can if you insist add an explicit indexing element, but"
                 "that strikes me as a waste. "
                 "You would do it as with the Populate function");
         SUMA_RETURN(YUP);
      }
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
            if (  (T[i] != Ti[i]) || (T[i] < 0) || 
                   (Ti[i] >= N_Node) || vis[Ti[i]] ) OKfirstCol = NOPE;
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
            SUMA_SL_Note("Used column 0 as node indices.\n"
                         "Added a node index column nonetheless.");
            /* You can't just change the label and the type 
            of the 0th column to be SUMA_NODE_INDEX because
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
      if (!SUMA_AddDsetNelCol (dset, "Node Index (inferred)", 
                                SUMA_NODE_INDEX, (void *)Ti, NULL, 1)) {
         SUMA_SL_Err("Failed to add column");
         if (Ti) SUMA_free(Ti); Ti = NULL;
         SUMA_RETURN(NOPE);
      }
      
      /* all done */
      SUMA_LH("Added the index column, ciao");
      if (Ti) SUMA_free(Ti); Ti = NULL;
      SUMA_RETURN(YUP);
   } else {
      SUMA_LH( "Node index column found, element might be empty however.\n"
               "Use SUMA_PopulateDsetNodeIndexNel to fill it.");
      /* Nothing to do, return on a positive note */
      SUMA_RETURN(YUP);      
   } 
   
   SUMA_SL_Err("why are you here ?");
   SUMA_RETURN(NOPE);
}

/* Function now OK with Graph Datasets */
SUMA_Boolean SUMA_PopulateDsetNodeIndexNel(SUMA_DSET *dset, int verb)
{
   static char FuncName[]={"SUMA_PopulateDsetNodeIndexNel"};
   int *Ti = NULL, *P1=NULL, *P2=NULL; 
   int i, j, n;
   
   SUMA_ENTRY;

   if (!dset ) {
      SUMA_S_Err("NULL input dset");
   }
   if (SUMA_isCIFTIDset(dset)) {
      SUMA_S_Err("Have yet to work out the logic for this type");
      SUMA_RETURN(NOPE);
   }
   if (!dset->inel) {
      SUMA_S_Err("NULL dset->inel");
   }
   
   if (dset->inel && dset->inel->vec_num) {
      if (verb > 1) SUMA_S_Note("Dset has node indices. Will not alter list.\n");
   } else {
      if (SUMA_isGraphDset(dset)) {
         if (verb > 1) 
            SUMA_S_Notev("Graph Dset matrix %ldx%ld, shape %d, veclen %d\n", 
                          dset->Aux->matrix_size[0], dset->Aux->matrix_size[1], 
                          dset->Aux->matrix_shape, SDSET_VECLEN(dset));
         if (dset->Aux->matrix_shape == MAT_HEEHAW ||
             dset->Aux->matrix_shape == MAT_NA ) {
            if (verb > 2) SUMA_S_Notev("Going for a setting (%s)\n", 
                              NI_get_attribute(dset->dnel,"matrix_shape"));
            if (!SUMA_GDSET_Set_Aux_matrix_shape(dset)) {
               SUMA_S_Err("Failed to set matrix shape");
               SUMA_RETURN(NOPE);
            }   
            if (verb > 2) 
               SUMA_S_Notev("Now have Graph Dset matrix %ldx%ld "
                            "for %d elements, shape now %d\n", 
                            dset->Aux->matrix_size[0], dset->Aux->matrix_size[1],
                            SDSET_VECLEN(dset),dset->Aux->matrix_shape);
         }
         Ti = (int *) SUMA_calloc(SDSET_VECLEN(dset), sizeof(int));
         P1 = (int *) SUMA_calloc(SDSET_VECLEN(dset), sizeof(int));
         P2 = (int *) SUMA_calloc(SDSET_VECLEN(dset), sizeof(int));
         for (i=0; i <SDSET_VECLEN(dset); ++i) Ti[i]=i;
         switch (dset->Aux->matrix_shape) {
            case MAT_HEEHAW:
            default:
               SUMA_S_Err("Should not happen");
               SUMA_RETURN(NOPE);
            case MAT_TRI:
               for (j=0, n=0; j <dset->Aux->matrix_size[0]; ++j) {
               for (i=j+1; i <dset->Aux->matrix_size[0]; ++i) {
                  P1[n]=i;
                  P2[n]=j;
                  ++n;
               } }
               break;
            case MAT_TRI_DIAG:
               for (j=0, n=0; j <dset->Aux->matrix_size[0]; ++j) {
               for (i=j; i <dset->Aux->matrix_size[0]; ++i) {
                  P1[n]=i;
                  P2[n]=j;
                  ++n;
               } }
               break;
            case MAT_FULL:
               for (j=0, n=0; j <dset->Aux->matrix_size[0]; ++j) {
               for (i=0; i <dset->Aux->matrix_size[0]; ++i) {
                  P1[n]=i;
                  P2[n]=j;
                  ++n;
               } }
               break;
            case MAT_SPARSE:
               SUMA_S_Err("Should not have entered parent else");
               SUMA_RETURN(NOPE);
         }
         
         if (!SUMA_AddDsetNelCol (  dset, "Edge Index (inferred)", 
                                    SUMA_NODE_INDEX, (void *)Ti, NULL, 1)) {
            SUMA_S_Err("Failed to add column");
            SUMA_RETURN(NOPE);
         }
         SUMA_free(Ti); Ti = NULL;
         if (!SUMA_AddDsetNelCol (  dset, "P1 Index (inferred)", 
                                    SUMA_EDGE_P1_INDEX, (void *)P1, NULL, 1)) {
            SUMA_S_Err("Failed to add column");
            SUMA_RETURN(NOPE);
         }
         SUMA_free(P1); P1 = NULL; 
         if (!SUMA_AddDsetNelCol (  dset, "P2 Index (inferred)", 
                                    SUMA_EDGE_P2_INDEX, (void *)P2, NULL, 1)) {
            SUMA_S_Err("Failed to add column");
            SUMA_RETURN(NOPE);
         }
         SUMA_free(P2); P2 = NULL; 
      } else {
         if (verb) SUMA_S_Note( "Assuming node indexing\n"
                        "is explicit. \n"
                        "1st row is for node 0\n"
                        "2nd is for node 1, etc.\n" );
         Ti = (int *) SUMA_calloc(SDSET_VECLEN(dset), sizeof(int));
         for (i=0; i <SDSET_VECLEN(dset); ++i) Ti[i]=i;
         if (!SUMA_AddDsetNelCol (  dset, "Node Index (inferred)", 
                                    SUMA_NODE_INDEX, (void *)Ti, NULL, 1)) {
            SUMA_S_Err("Failed to add column");
            SUMA_RETURN(NOPE);
         }
         SUMA_free(Ti); Ti = NULL; 
      }
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
   
   /* SUMA_SL_Warn("Obsolete, use new version."); 
         still needed to convert old format to new one */
   
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
   if (!cnm) NI_set_ni_type_atr(nel);
   cnm = NI_get_attribute(nel, "ni_type");
   
   if (cnm) {
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
            case SUMA_complex:
               ctp = SUMA_NODE_COMPLEX;
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
   int *RowSel=NULL, *ColSel=NULL, *NodeSel=NULL, i, nfixed=0;
   char *Name=NULL;
   double range[2];
   int loc[2];
   byte *b_ColSel = NULL, *b_RowSel = NULL;
   SUMA_DSET_FORMAT fform = SUMA_NO_DSET_FORMAT;
   SUMA_PARSED_NAME *pn=NULL;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!iName) { SUMA_SL_Err("NULL Name"); goto GOODBYE; }
   if (!form) form = &fform;
   
   if (LocalHead) {
      fprintf(stderr,"%s: Have %s to work with\n", FuncName, iName);
   }
   /* parse the name please. */
   RowSel = ColSel = NodeSel = NULL;
   if (!(pn = SUMA_ParseFname(iName, NULL))) {
      SUMA_SL_Err("Failed to parse name"); goto GOODBYE;
   }
   
   if (pn->NodeSelect[0]!= '\0' && pn->RowSelect[0] != '\0') {
      SUMA_SL_Err("Cannot use Node and Row selectors simultaneously");
      goto GOODBYE; 
   }
   
   /* Form the name */
   Name = SUMA_append_string(pn->Path, pn->FileName);
   
   if (*form == SUMA_NO_DSET_FORMAT) {  /* attempt to guess from extension */
      SUMA_LH("Attempting to guess dset format from extension\n");
      *form = SUMA_GuessFormatFromExtension(Name, NULL);
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
      case SUMA_XML_DSET:
      case SUMA_XML_ASCII_DSET:
      case SUMA_XML_B64_DSET:
      case SUMA_XML_B64GZ_DSET:
         SUMA_LH("Loading GIFTI Dset");
         dset = SUMA_LoadGIFTIDset(Name, verb);
         break;
      case SUMA_NO_DSET_FORMAT:
         if (!dset) { 
            SUMA_LH("Trying NIML Dset"); 
            dset = SUMA_LoadNimlDset(Name, 0); *form = SUMA_NIML; 
         }
         if (!dset) { 
            SUMA_LH("Trying 1D Dset"); 
            dset = SUMA_Load1DDset_ns(Name, 0); *form = SUMA_1D; 
         }
         if (!dset) { 
            SUMA_LH("Trying DX Dset"); 
            dset = SUMA_LoadDXDset_ns(Name, 0); *form = SUMA_ASCII_OPEN_DX_DSET;
         }
         break;
      default:
         if (verb) 
            SUMA_PushErrLog("SLP_Err", "Bad format specification", FuncName);
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
   /* Update filename field in dset */
   NI_set_attribute(dset->ngr, "filename", Name);
   
   /* repair floats */
   if (!SUMA_isEnv("AFNI_FLOATSCAN","NO")) { 
      if (nfixed = SUMA_FloatScanDset(dset, 1, 1, 1, 0)) {
         if (verb) fprintf(SUMA_STDOUT,
             "++    Notice %s: %d NAN or INF values were set to 0 in dset\n\n",
                        FuncName,  nfixed);
      }
   }
   
   /* do we need to substitute with node indices only? */
   if (pn->only_index) {
      SUMA_DSET *ndset=NULL;
      void *ind=NULL;
      if (!(ind = SDSET_NODE_INDEX_COL(dset))) {
         SUMA_FreeDset(dset); dset = NULL;
         SUMA_SL_Err( "No node index column to work with.\n"
                     "This feature is meaningless with .1D\n"
                     "datasets. You can use the node\n"
                     "column's index directly for that.\n");
         goto GOODBYE;   
      }
      ndset =  SUMA_EmptyCopyofDset( dset, 
                                     NULL, 1, 0 ); 
      if (!SUMA_InsertDsetNelCol (  ndset, "Node Index Copy", 
                                    SUMA_NODE_INT, ind,
                                    NULL ,1, 0)) {
            SUMA_S_Err("Failed to insert column");
      }
      if (SUMA_isGraphDset(dset)) {
         if (!(ind = SDSET_EDGE_P1_INDEX_COL(dset))){
            SUMA_FreeDset(dset); dset = NULL;
            SUMA_SL_Err( "This should not happen in ind above was OK.\n");
            goto GOODBYE;   
         }
         if (!SUMA_InsertDsetNelCol (  ndset, "Edge P1 Index Copy", 
                                    SUMA_NODE_INT, ind,
                                    NULL ,1, 0)) {
            SUMA_S_Err("Failed to insert column");
         }
         if (!(ind = SDSET_EDGE_P2_INDEX_COL(dset))){
            SUMA_FreeDset(dset); dset = NULL;
            SUMA_SL_Err( "This really should not happen in ind above was OK.\n");
            goto GOODBYE;   
         }
         if (!SUMA_InsertDsetNelCol (  ndset, "Edge P2 Index Copy", 
                                    SUMA_NODE_INT, ind,
                                    NULL ,1, 0)) {
            SUMA_S_Err("Failed to insert column");
         }
      }
      SUMA_FreeDset(dset); dset = ndset; ndset = NULL;
   }
   
   /* do we have column, row, or node selectors? */
   if (pn->NodeSelect[0]!= '\0') {
      /* go get the selector lists */
      if (!SUMA_GetDsetNodeIndexColRange(dset, range, loc, 1)) {
         SUMA_SL_Err("Can't get node index column range!");
         goto GOODBYE;   
      }
      NodeSel = MCW_get_intlist( (int)range[1] , 
                                  pn->NodeSelect ) ;
      if (!NodeSel || !NodeSel[0]) {
         SUMA_SL_Err("Failed to get node selection");
         SUMA_FreeDset(dset); dset = NULL;
         goto GOODBYE;      
      }
      if (LocalHead) {
         SUMA_LH("NodeSelection");
         fprintf(SUMA_STDERR,"   %d:", NodeSel[0]);
         for (i=1; i<=NodeSel[0]; ++i) fprintf(SUMA_STDERR,"   %d", NodeSel[i]);
         fprintf(SUMA_STDERR,"\n");
      }
   }
   if (pn->ColSelect[0]!= '\0') {
      char **AllLabels=NULL;
      if (pn->only_index) {
         SUMA_S_Err("Only index selection not allowed with column selection.");
         goto GOODBYE; 
      }
      /* go get the selector lists */
      AllLabels = SUMA_AllDsetColLabels(dset);
      ColSel = MCW_get_labels_intlist( AllLabels, SDSET_VECNUM(dset) , 
                                       pn->ColSelect ) ;
      AllLabels = SUMA_FreeAllDsetColLabels(AllLabels);
      
      if (!ColSel || !ColSel[0]) {
         SUMA_SL_Err("Failed to get column selection");
         SUMA_FreeDset(dset); dset = NULL;
         goto GOODBYE;     
      }
      if (LocalHead) {
         SUMA_LH("ColSelection");
         fprintf(SUMA_STDERR,"   %d:", ColSel[0]);
         for (i=1; i<=ColSel[0]; ++i) fprintf(SUMA_STDERR,"   %d", ColSel[i]);
         fprintf(SUMA_STDERR,"\n");
      }
   }
   if (pn->RowSelect[0]!= '\0') {
      /* go get the selector lists */
      RowSel = MCW_get_intlist( SDSET_VECFILLED(dset) , pn->RowSelect ) ;
      if (!RowSel || !RowSel[0]) {
         SUMA_SL_Err("Failed to get row selection");
         SUMA_FreeDset(dset); dset = NULL;
         goto GOODBYE;            
      }
      if (LocalHead) {
         SUMA_LH("RowSelection");
         fprintf(SUMA_STDERR,"   %d:", RowSel[0]);
         for (i=1; i<=RowSel[0]; ++i) fprintf(SUMA_STDERR,"   %d", RowSel[i]);
         fprintf(SUMA_STDERR,"\n");
      }
   }
   
   if (pn->RangeSelect[0]!= '\0') {
      SUMA_SL_Err("Range selection not allowed with SUMA dsets.");
      SUMA_FreeDset(dset); dset = NULL;
      goto GOODBYE; 
   }
   
   if (ColSel) {
      if (!(b_ColSel =  
         SUMA_indexlist_2_bytemask((ColSel+1), ColSel[0], 
                                    SDSET_VECNUM(dset), NULL))) {
         SUMA_SL_Err("Failed in creating byte mask");
         SUMA_FreeDset(dset); dset = NULL;
         goto GOODBYE;
      }
   } else b_ColSel = NULL;
   
   if (RowSel) {
      if (!(b_RowSel =  SUMA_indexlist_2_bytemask((RowSel+1), RowSel[0], 
                                                   SDSET_VECFILLED(dset), 
                                                   NULL))) {
         SUMA_SL_Err("Failed in creating byte mask");
         SUMA_FreeDset(dset); dset = NULL;
         goto GOODBYE;
      }
   } else b_RowSel = NULL;
   
   if (NodeSel) {
      if (!(dset_c = SUMA_MaskedByNodeIndexCopyofDset(dset, (NodeSel+1), 
                                                      NodeSel[0], b_ColSel, 1, 
                                                      0))) {
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
   
   /* set do_type */
   if (SUMA_isGraphDset(dset)) dset->do_type = GDSET_type;
   else if (SUMA_isCIFTIDset(dset)) {
      SUMA_LH("Found no need to flag CIFTI dset as anything other\n"
              "than generic dataset");
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

int SUMA_WriteDset_NameCheck_ns (
            char *Name, SUMA_DSET *dset, 
            SUMA_DSET_FORMAT form, int verb, char **NameOutp) 
{
   int exists = SUMA_WriteDset_NameCheck_eng(Name, dset, form, verb, NameOutp);
   WorkErrLog_ns();
   return(exists);
} 

int SUMA_WriteDset_NameCheck_eng (  char *Name, SUMA_DSET *dset, 
                                    SUMA_DSET_FORMAT form, 
                                    int verb, char **NameOutp) 
{
   static char FuncName[]={"SUMA_WriteDset_NameCheck_eng"};
   int exists = 0;
   char *PrefOut=NULL, *NameOut=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!Name && dset && dset->ngr && !SUMA_IS_DSET_STDXXX_FORMAT(form)) {
      if (!(Name=NI_get_attribute(dset->ngr, "filename"))) {
      SUMA_PushErrLog("SL_Err","NULL Name", FuncName); SUMA_RETURN(-1); 
      }
   }

   if (!SUMA_IS_DSET_STDXXX_FORMAT(form)) {
      PrefOut = SUMA_RemoveDsetExtension_ns(Name, form);
      if (!PrefOut) { 
         SUMA_PushErrLog(  "SL_Err",
                           "Failed clean dset name", FuncName); 
         SUMA_RETURN(-1);
      }
      SUMA_LH("%s", PrefOut);
   }
   
   if (form == SUMA_NO_DSET_FORMAT) {
      form = SUMA_GuessFormatFromExtension(Name, NULL);
   }
   switch (form) {
      case SUMA_XML_DSET:
      case SUMA_XML_ASCII_DSET:
      case SUMA_XML_B64_DSET:
      case SUMA_XML_B64GZ_DSET:
         if (SUMA_isExtension(Name,".gii")) { /* allow .gii only */
            NameOut = SUMA_Extension(PrefOut, ".gii", NOPE);
         } else {
            NameOut = SUMA_Extension(PrefOut, ".gii.dset", NOPE);
         }
         if (SUMA_filexists(NameOut)) {
            exists = 1;
         } 
         break;
      case SUMA_NIML:
      case SUMA_ASCII_NIML:
      case SUMA_BINARY_NIML:
         NameOut = SUMA_Extension(PrefOut, ".niml.dset", NOPE);
         if (SUMA_filexists(NameOut)) {
            exists = 1;
         } 
         break;
      case SUMA_1D:
         NameOut = SUMA_Extension(PrefOut, ".1D.dset", NOPE);
         if (SUMA_filexists(NameOut)) {
            exists = 1;
         } 
         break;
      case SUMA_1D_PURE:
         NameOut = SUMA_Extension(PrefOut, ".1D.dset", NOPE);
         if (SUMA_filexists(NameOut)) {
            exists = 1;
         } 
         break;
      case SUMA_1D_PURE_TRANSPOSE:
         NameOut = SUMA_Extension(PrefOut, ".1D.dset", NOPE);
         if (SUMA_filexists(NameOut)) {
            exists = 1;
         } 
         break;
      case SUMA_NIML_STDERR:
      case SUMA_NIML_STDOUT:
      case SUMA_1D_PURE_STDOUT:
      case SUMA_1D_PURE_STDERR:
      case SUMA_1D_PURE_STDOUT_TRANSPOSE:
      case SUMA_1D_PURE_STDERR_TRANSPOSE:
      case SUMA_1D_STDOUT:
      case SUMA_1D_STDERR:
         break;
      case SUMA_NO_DSET_FORMAT:
         /* try them all */
         exists = -1;
         {  
            SUMA_DSET_FORMAT aform=SUMA_NO_DSET_FORMAT+1;
            while (exists != 1 && aform < SUMA_N_DSET_FORMATS) {
               exists = SUMA_WriteDset_NameCheck_eng(
                           Name, dset, aform, verb, &NameOut); 
               if (exists == 1) form=aform;
               ++aform;
            }
         }
         break;
      default:
         SUMA_PushErrLog("SLP_Err","Bad format specification", FuncName);
         exists = -1;
         break;
   }



   if (NameOutp && !SUMA_IS_DSET_STDXXX_FORMAT(form)) {
      SUMA_LH("%s", NameOut);
      *NameOutp = NameOut; NameOut = NULL;
   } else {
      if (NameOut) SUMA_free(NameOut); NameOut = NULL;
   }
   if (PrefOut) SUMA_free(PrefOut); PrefOut = NULL;
   SUMA_RETURN(exists);
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
char * SUMA_WriteDset_ns ( char *Name, SUMA_DSET *dset, 
                           SUMA_DSET_FORMAT form, 
                           int overwrite, int verb) 
{
   char *c=SUMA_WriteDset_eng (Name, dset, form, overwrite, verb, 1);
   WorkErrLog_ns();
   return(c);
} 


char * SUMA_WriteDset_eng (char *Name, SUMA_DSET *dset, 
                           SUMA_DSET_FORMAT form, 
                           int overwrite, int verb,
                           int rename_autoid) 
{
   static char FuncName[]={"SUMA_WriteDset_eng"};
   char *NameOut = NULL, *strmname=NULL, stmp[500], *eee=NULL, *oName=NULL;
   int flg = 0, exists = 0, goutmode=-1, eoutmode = -1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) verb = 3;
   if (!dset) { 
      SUMA_PushErrLog("SL_Err", "NULL dset", FuncName); 
      SUMA_RETURN(NameOut); 
   }
   if (LocalHead) {
      SUMA_LH("About to write dset to %s", Name);
      SUMA_ShowDset(dset, 0, NULL);
   }  
   if (!dset->ngr) { 
      SUMA_PushErrLog("SL_Err","NULL dset->ngr", FuncName); 
      SUMA_RETURN(NameOut); 
   }
   
   if (!Name) { 
      if (!(Name=SDSET_FILENAME(dset))) {
         SUMA_PushErrLog("SL_Err","NULL Name and no filename", 
                         FuncName); SUMA_RETURN(NameOut); 
      }
   } else if (Name != SDSET_FILENAME(dset)) /* yes, a pointer comparison */{
      /* call rename dset, unless they passed SDSET_FILENAME(dset) for name!*/
      if (!SUMA_RenameDset(dset, Name, rename_autoid)) {
         SUMA_PushErrLog(  "SL_Err",
                           "Failed to rename dset", 
                           FuncName); 
         SUMA_RETURN(NameOut);
      }
   }
     
   if (form == SUMA_NO_DSET_FORMAT) {
      form = SUMA_GuessFormatFromExtension (Name, "lazybum.niml.dset");
   }
   
   if (( exists = 
         SUMA_WriteDset_NameCheck_ns (Name, dset, form, verb, &NameOut) ) < 0) {
      SUMA_PushErrLog("SLP_Err","Failed to check name", FuncName);
      SUMA_RETURN(NameOut);
   }

   /* take care of ambiguous NIML form */
   if (form == SUMA_NIML) {
      if (AFNI_yesenv("AFNI_NIML_TEXT_DATA")) {
         form = SUMA_ASCII_NIML;
      } else {
         form = SUMA_BINARY_NIML;
      }
   }
   
   /* turn off outmode, me no use it */
   if (dset->ngr->outmode >= 0) { 
      goutmode = dset->ngr->outmode; 
      dset->ngr->outmode = -1; 
   }
   if (dset->dnel->outmode >= 0) { 
      eoutmode = dset->dnel->outmode; 
      dset->dnel->outmode = -1; 
   }
   

   if (exists && overwrite) {
      exists = 0;
      if (verb) 
         fprintf( SUMA_STDOUT, 
                  "Notice %s: Overwriting existing file %s ...\n", 
                  FuncName, NameOut);
   }
   
   if (!exists) {
      switch (form) {
         case SUMA_XML_DSET:
         case SUMA_XML_ASCII_DSET:
         case SUMA_XML_B64_DSET:
         case SUMA_XML_B64GZ_DSET:
            {
               int aaa;
               if (form == SUMA_XML_B64GZ_DSET) 
                  aaa = NI_write_gifti(dset->ngr, NameOut,3); 
                        /* GIFTI_ENCODING_B64GZ */
               else if (form == SUMA_XML_B64_DSET) 
                  aaa = NI_write_gifti(dset->ngr, NameOut,2); 
                        /* GIFTI_ENCODING_B64 */
               else if (form == SUMA_XML_ASCII_DSET)
                  aaa = NI_write_gifti(dset->ngr, NameOut,1); 
                        /* GIFTI_ENCODING_ASCII */
               else aaa = NI_write_gifti(dset->ngr, NameOut, 0); 
               if (aaa){
                  SUMA_PushErrLog(  "SL_Err",
                                    "Failed to write GIFTI dset", FuncName);
                  flg = 0;
               } else flg = 1;
            }
            break;
         case SUMA_NIML:
         case SUMA_ASCII_NIML:
         case SUMA_BINARY_NIML:
            {
               strmname = SUMA_append_string("file:",NameOut);
               NI_set_attribute(dset->ngr,"filename", NameOut);
               if (form == SUMA_ASCII_NIML) { 
                 SUMA_LH("Writing NIML, ASCII...%s",strmname);  
                 NEL_WRITE_TX (dset->ngr, strmname, flg);  
                 SUMA_LH("DONE.");
               } else { 
                 SUMA_LH("Writing NIML, BINARY...%s",strmname); 
                 NEL_WRITE_BI (dset->ngr, strmname, flg); 
                 SUMA_LH("DONE.");
               }
               if (!flg) {
                  SUMA_PushErrLog(  "SL_Err",
                                    "Failed to write niml element", FuncName);
               } else {
                  SUMA_LH("DONE.");
               }
            }
            break;
         case SUMA_NIML_STDERR:
         case SUMA_NIML_STDOUT:
            if (form == SUMA_NIML_STDOUT) { 
              SUMA_LH("Writing NIML, STDOUT...%s", strmname); 
              NEL_WRITE_TX (dset->ngr, "stdout:", flg);  
              SUMA_LH("DONE.");
            } else { 
              SUMA_LH("Writing NIML, STDERR...%s", strmname); 
              NEL_WRITE_TX (dset->ngr, "stderr:", flg); 
              SUMA_LH("DONE.");
            }
            if (!flg) {
               SUMA_PushErrLog("SL_Err","Failed to write element", FuncName);
            } else {
               SUMA_LH("DONE.");
            }
            break;
         case SUMA_1D:
            {
               NI_set_attribute(dset->ngr,"filename", NameOut);
               strmname = SUMA_append_string("file:",NameOut);
	            SUMA_LH("Writing 1D...%s", strmname); 
               DSET_WRITE_1D (dset, strmname, flg, AddIndex_1D);
               if (!flg) {
                  SUMA_PushErrLog("SL_Err","Output file not written", FuncName);
               } else {
                  SUMA_LH("DONE.");
               }
            } 
            break;
         case SUMA_1D_PURE:
            {
               NI_set_attribute(dset->ngr,"filename", NameOut);
               strmname = SUMA_copy_string(NameOut);
	            SUMA_LH("Writing 1D pure...%s", strmname); 
               DSET_WRITE_1D_PURE (dset, strmname, flg, AddIndex_1D);
               if (!flg) {
                  SUMA_PushErrLog("SL_Err","Output file not written", FuncName);
               } else {
                  SUMA_LH("DONE.");
               }
            } 
            break;
         case SUMA_1D_PURE_TRANSPOSE:
            {
               NI_set_attribute(dset->ngr,"filename", NameOut);
               strmname = SUMA_copy_string(NameOut);
	            SUMA_LH("Writing 1D pure transpose...%s", strmname); 
               DSET_WRITE_1D_PURE_TRANSPOSE (dset, strmname, flg, AddIndex_1D);
               if (!flg) {
                  SUMA_PushErrLog("SL_Err","Output file not written", FuncName);
               } else {
                  SUMA_LH("DONE.");
               }
            } 
            break;
         case SUMA_1D_PURE_STDOUT:
            {
               NI_set_attribute(dset->ngr,"filename", "stdout");
	            SUMA_LH("Writing 1D pure...%s", strmname); 
               DSET_WRITE_1D_PURE (dset, "stdout", flg, AddIndex_1D);
               {
                  SUMA_LH("DONE.");
               }
            } 
            break;
         case SUMA_1D_PURE_STDERR:
            {
               NI_set_attribute(dset->ngr,"filename", "stderr");
	            SUMA_LH("Writing 1D pure...%s", strmname); 
               DSET_WRITE_1D_PURE (dset, "stderr", flg, AddIndex_1D);
               {
                  SUMA_LH("DONE.");
               }
            } 
            break;
         case SUMA_1D_PURE_STDOUT_TRANSPOSE:
            {
               NI_set_attribute(dset->ngr,"filename","stdout");
	            SUMA_LH("Writing 1D pure transpose...%s", strmname)
               DSET_WRITE_1D_PURE_TRANSPOSE (dset, "stdout", flg, AddIndex_1D);
               {
                  SUMA_LH("DONE.");
               }
            } 
            break;
         case SUMA_1D_PURE_STDERR_TRANSPOSE:
            {
               NI_set_attribute(dset->ngr,"filename","stderr");
	            SUMA_LH("Writing 1D pure transpose...%s", strmname);
               DSET_WRITE_1D_PURE_TRANSPOSE (dset, "stderr", flg, AddIndex_1D);
               {
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
      
      if (!NameOut || !flg) {
         if (verb) 
            SUMA_PushErrLog("SLP_Err","Failed writing dataset.", FuncName);
         if (NameOut) SUMA_free(NameOut); NameOut = NULL;
      }
   } else { 
      snprintf(stmp, 500*sizeof(char), 
               "Output file %s exists.\n Will not overwrite.", NameOut);
      SUMA_PushErrLog("SLP_Err",stmp, FuncName);
      if (NameOut) SUMA_free(NameOut); NameOut = NULL;
   }  
   
   if (strmname) SUMA_free(strmname); strmname = NULL;

   if (goutmode >= 0) { dset->ngr->outmode = goutmode ; }
   if (eoutmode >= 0) { dset->dnel->outmode = eoutmode ; }

   SUMA_RETURN(NameOut);
}

/*!
   \brief Guess file format from extension
   \param Name (char *) name 
   \return form SUMA_DSET_FORMAT
*/
SUMA_DSET_FORMAT SUMA_GuessFormatFromExtension_core(char *Name)
{
   static char FuncName[]={"SUMA_GuessFormatFromExtension_core"};
   SUMA_DSET_FORMAT form = SUMA_NO_DSET_FORMAT;
   SUMA_PARSED_NAME *fn=NULL;
   SUMA_Boolean LocalHead = NOPE;
     
   SUMA_ENTRY;
   
   if (!Name) { SUMA_RETURN(form); }

   fn=SUMA_ParseFname (Name, NULL);
   if (LocalHead) SUMA_ShowParsedFname(fn, NULL);
   
   if (SUMA_isExtension(fn->FileName, ".niml.dset") ||
       SUMA_isExtension(fn->FileName, ".niml.do") ||
       SUMA_isExtension(fn->FileName, ".niml.mo") ||
       SUMA_isExtension(fn->FileName, ".niml.tract") ) 
      form = SUMA_NIML;
   else if (  SUMA_isExtension(fn->FileName, ".gii.dset") ||
         SUMA_isExtension(fn->FileName, ".gii") ) 
      form = SUMA_XML_DSET;
   else if (SUMA_isExtension(fn->FileName, ".1D.dset")) 
      form = SUMA_1D;
   else if (SUMA_isExtension(fn->FileName, ".niml.cmap")) 
      form = SUMA_NIML;
   else if (SUMA_isExtension(fn->FileName, ".1D.cmap")) 
      form = SUMA_1D; 
   else if (SUMA_isExtension(fn->FileName, ".dx.dset")) 
      form = SUMA_ASCII_OPEN_DX_DSET; 
   else if (SUMA_isExtension(fn->FileName, ".dx")) 
      form = SUMA_ASCII_OPEN_DX_DSET;
   else if (SUMA_isExtension(fn->FileName, ".1D")) 
      form = SUMA_1D; 
   
   SUMA_Free_Parsed_Name(fn); fn = NULL;
    
   SUMA_RETURN(form);
}

SUMA_DSET_FORMAT SUMA_GuessFormatFromExtension(char *Name, char *fallbackname)
{
   static char FuncName[]={"SUMA_GuessFormatFromExtension"};
   SUMA_DSET_FORMAT form = SUMA_NO_DSET_FORMAT;
     
   SUMA_ENTRY;
   
   if (!Name && fallbackname) {Name = fallbackname;}
   
   form = SUMA_GuessFormatFromExtension_core(Name);
   if (form <= SUMA_NO_DSET_FORMAT && fallbackname && Name != fallbackname ) { /* try with fallback */
      form = SUMA_GuessFormatFromExtension_core(fallbackname);
   }
   
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
      case SUMA_XML_DSET:
      case SUMA_XML_ASCII_DSET:
      case SUMA_XML_B64_DSET:
      case SUMA_XML_B64GZ_DSET:
         SUMA_RETURN(".gii.dset");
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
   char *c=SUMA_RemoveDsetExtension_eng (Name,  &form);
   WorkErrLog_ns();
   return(c);
}

/* Removes the standard extension && sets the format upon return
   if the format was SUMA_NO_DSET_FORMAT */
char *SUMA_RemoveDsetExtension_eng (char*Name, SUMA_DSET_FORMAT *form)
{
   static char FuncName[]={"SUMA_RemoveDsetExtension_eng"};
   char *noex = NULL, *tmp = NULL;
   SUMA_DSET_FORMAT formo=*form;
   
   SUMA_ENTRY;
   
   if (!Name) { SUMA_SL_Err("NULL Name"); SUMA_RETURN(NULL); }
  
   switch (*form) {
      case SUMA_NIML:
      case SUMA_ASCII_NIML:
      case SUMA_BINARY_NIML:
         noex  =  SUMA_Extension(Name, ".niml.dset", YUP);
         break;
      case SUMA_1D:
      case SUMA_1D_PURE:
      case SUMA_1D_PURE_TRANSPOSE:
         tmp  =  SUMA_Extension(Name, ".1D", YUP);
         noex  =  SUMA_Extension(tmp, ".1D.dset", YUP); 
            SUMA_free(tmp); tmp = NULL;
         break;
      case SUMA_ASCII_OPEN_DX_DSET:
         tmp  =  SUMA_Extension(Name, ".dx", YUP);
         noex  =  SUMA_Extension(tmp, ".dx.dset", YUP); 
            SUMA_free(tmp); tmp = NULL;
         break;
      case SUMA_NO_DSET_FORMAT:
         tmp  =  SUMA_Extension(Name, ".1D", YUP);
            if (strcmp(tmp,Name)) formo=SUMA_1D;
         noex = SUMA_Extension(tmp, ".1D.dset", YUP); 
            if (strcmp(noex,tmp)) formo=SUMA_1D;
            SUMA_free(tmp); tmp = NULL; tmp = noex;
         noex = SUMA_Extension(tmp, ".niml.dset", YUP); 
            if (strcmp(noex,tmp)) formo=SUMA_NIML;
            SUMA_free(tmp); tmp = NULL; tmp = noex;
         noex  =  SUMA_Extension(tmp, ".gii", YUP);
            if (strcmp(noex,tmp)) formo=SUMA_XML_DSET;
            SUMA_free(tmp); tmp = NULL; tmp = noex;
         noex  =  SUMA_Extension(tmp, ".gii.dset", YUP); 
            if (strcmp(noex,tmp)) formo=SUMA_XML_DSET;
            SUMA_free(tmp); tmp = NULL; tmp = noex;
         noex  =  SUMA_Extension(tmp, ".dx", YUP);
            if (strcmp(noex,tmp)) formo=SUMA_ASCII_OPEN_DX_DSET;
            SUMA_free(tmp); tmp = NULL; tmp = noex;
         noex  =  SUMA_Extension(tmp, ".dx.dset", YUP); 
            if (strcmp(noex,tmp)) formo=SUMA_ASCII_OPEN_DX_DSET;
            SUMA_free(tmp); tmp = NULL;
         break;
      case SUMA_XML_DSET:
      case SUMA_XML_ASCII_DSET:
      case SUMA_XML_B64_DSET:
      case SUMA_XML_B64GZ_DSET:
         tmp  =  SUMA_Extension(Name, ".gii", YUP);
         noex  =  SUMA_Extension(tmp, ".gii.dset", YUP); 
            SUMA_free(tmp); tmp = NULL;
         break;
      case SUMA_NIML_STDOUT:
      case SUMA_NIML_STDERR:
      case SUMA_1D_STDOUT:
      case SUMA_1D_STDERR:
      case SUMA_1D_PURE_STDOUT:
      case SUMA_1D_PURE_STDERR:
      case SUMA_1D_PURE_STDOUT_TRANSPOSE:
      case SUMA_1D_PURE_STDERR_TRANSPOSE:
         noex = SUMA_copy_string(Name);
         break;
      default:
         SUMA_PushErrLog("SLP_Err","Bad format specification", FuncName);
         break;
   }
   
   *form=formo;
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
   int ctp, i, iiidata=0, iiinel = 0;
   NI_element *dnel = NULL, *inel = NULL;
   SUMA_DSET dset; /* dummy */
   SUMA_Boolean LocalHead = NOPE;
   
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
      NI_set_attribute(ngr, "self_idcode", idcode);
      SUMA_free(idcode); idcode = NULL;
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
   SUMA_LH("Doing dnel");
   dname = SUMA_append_string(NEL_DSET_TYPE(ngr), "_data");
   dnel = NI_new_data_element("SPARSE_DATA", nel->vec_len); 
   NI_set_attribute (dnel, "data_type", dname); 
   SUMA_free(dname); dname = NULL;
   NI_add_to_group(ngr, dnel);

   /* Now add the node index element */
   SUMA_LH("Doing inel");
   dname = SUMA_append_string(NEL_DSET_TYPE(ngr), "_node_indices");
   inel = NI_new_data_element("INDEX_LIST", nel->vec_len); 
   NI_set_attribute (inel, "data_type", dname); 
   SUMA_free(dname); dname = NULL;
   NI_add_to_group(ngr, inel);

   /* now, manually add the columns' spots */
   for (i=0; i<nel->vec_num; ++ i) {
      SUMA_LH("Adding cols"); 
      ctp = SUMA_TypeOfColNumb(nel, i);
      if (ctp != SUMA_NODE_INDEX) {
         switch (SUMA_ColType2TypeCast(ctp)) {  
            /* MUST use the old function SUMA_TypeOfColNumb here ! */
            case SUMA_int:
               SUMA_LH("int");
               NI_add_column_stride ( dnel, NI_INT, NULL, 1);
               break;
            case SUMA_float:
               SUMA_LH("float");
               NI_add_column_stride ( dnel, NI_FLOAT, NULL, 1 );      
               break;
            case SUMA_byte:
               SUMA_LH("byte");
               NI_add_column_stride ( dnel, NI_BYTE, NULL, 1 );      
               break;
            case SUMA_double:
               SUMA_LH("double");
               NI_add_column_stride ( dnel, NI_DOUBLE, NULL, 1 );      
               break;
            case SUMA_string:
               SUMA_LH("String");
               NI_add_column_stride ( dnel, NI_STRING, NULL, 1 );
               break;
            case SUMA_complex:
               SUMA_LH("Complex");
               NI_add_column_stride ( dnel, NI_COMPLEX, NULL, 1 );
               break;
            default:
               fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
               NI_free_element(ngr);
               SUMA_RETURN(NULL);
               break;
         } 
         /* copy the vector's pointer */
         dnel->vec[iiidata] = nel->vec[i]; nel->vec[i] = NULL;
         /* set some generic attributes */
         dset.dnel = dnel; dset.ngr = ngr; dset.inel = inel;
         SUMA_AddGenDsetColAttr (&dset, ctp, dnel->vec[iiidata], 1, -1, 0);
         /* add the attributes of that column */
         col_label = SUMA_ColLabelCopy(nel, i, 0);
         SUMA_AddDsetColAttr (&dset, col_label, ctp, NULL, -1, 0);
         if (col_label) SUMA_free(col_label); col_label = NULL;
         ++iiidata;
      } else {
         if (iiinel > 0) {
            SUMA_S_Err("Have inel column already...");
            NI_free_element(ngr); ngr = NULL;
            SUMA_RETURN(NULL);
         }
         SUMA_LH("In the else");
         switch (SUMA_ColType2TypeCast(ctp)) {  
            /* MUST use the old function SUMA_TypeOfColNumb here ! */
            case SUMA_int:
               SUMA_LH("int");
               NI_add_column_stride ( inel, NI_INT, NULL, 1);
               break;
            case SUMA_float:
               SUMA_LH("float");
               NI_add_column_stride ( inel, NI_FLOAT, NULL, 1 );      
               break;
            case SUMA_byte:
               SUMA_LH("byte");
               NI_add_column_stride ( inel, NI_BYTE, NULL, 1 );      
               break;
            case SUMA_double:
               SUMA_LH("doouble");
               NI_add_column_stride ( inel, NI_DOUBLE, NULL, 1 );      
               break;
            case SUMA_string:
               NI_add_column_stride ( inel, NI_STRING, NULL, 1 );
               break;
            case SUMA_complex:
               NI_add_column_stride ( inel, NI_COMPLEX, NULL, 1 );
               break;
            default:
               fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
               NI_free_element(ngr);
               SUMA_RETURN(NULL);
               break;
         }
         inel->vec[0] = nel->vec[i]; nel->vec[i] = NULL;
         /* set some generic attributes */
         dset.inel = inel; dset.ngr = ngr; dset.dnel = dnel; 
         SUMA_AddGenDsetColAttr (&dset, ctp, inel->vec[0], 1, -1, 0);
         ++iiinel;
      }      
   }   
   
   
   /* add the history note */
   SUMA_LH("History Note");
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

SUMA_DSET *SUMA_LoadGIFTIDset (char *Name, int verb)
{
   static char FuncName[]={"SUMA_LoadGIFTIDset"};
   char *FullName = NULL, *niname = NULL;
   SUMA_DSET *dset=NULL;
   NI_group *ngr = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!Name) { SUMA_SL_Err("Null Name"); SUMA_RETURN(dset); }
   
   /* work the name */
   if (!SUMA_filexists(Name)) {
      /* try the extension game */
      FullName = SUMA_Extension(Name, ".gii.dset", NOPE);
      if (!SUMA_filexists(FullName)) {
         if (verb)  { SUMA_SL_Err("Failed to find dset file."); }
         if (FullName) SUMA_free(FullName); FullName = NULL;
         SUMA_RETURN(dset);
      }
   }else {
      FullName = SUMA_copy_string(Name);
   }   
   
   ngr = NI_read_gifti(Name, 1);
   if( !ngr ) {
      if (verb)  { SUMA_SL_Err("Failed to read dset file."); }
      SUMA_RETURN(dset);
   }
   
   /* add filename and label */
   if (!NI_get_attribute(ngr,"filename")) 
      NI_set_attribute(ngr,"filename", FullName);
   if (!NI_get_attribute(ngr,"label")) 
      NI_set_attribute(ngr,"label", SUMA_FnameGet(FullName,"fne", NULL));
   
   if (!(dset = SUMA_ngr_2_dset(ngr, 0))) {
      SUMA_SL_Err("Failed to go from ngr to dset");
      SUMA_RETURN(NULL);
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
   if (FullName) SUMA_free(FullName); FullName = NULL;
   SUMA_RETURN(dset);
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
                           "Perhaps it is a .1D read in as a niml dset.\n",
                            FuncName, Name);
      SUMA_RETURN(NULL);
   }
   
   
   if (iselement) {
      dset = SUMA_NewDsetPointer();
      dset->ngr = SUMA_oDsetNel2nDsetNgr((NI_element *)nini);
      dset->dnel = SUMA_FindDsetDataElement(dset);
      dset->inel = SUMA_FindDsetDatumIndexElement(dset);
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
      if (!(dset = SUMA_ngr_2_dset((NI_group *)nini, verb))) {
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
SUMA_Boolean SUMA_OpenDx_Object_Data(char *op, int nchar, 
                                     SUMA_OPEN_DX_STRUCT *dx)
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
         dx->datap = SUMA_strtol_vec(op, dx->items*SUMA_NCOL_OPENDX(dx), &nread, SUMA_CTypeName2VarType (dx->type), NULL);
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
      if (!SUMA_AddDsetNelCol (dset, "dx_col", 
                           SUMA_VarType2ColType (dx->type), 
                           (char *)dx->datap+i, NULL , SUMA_NCOL_OPENDX(dx))) {
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
   
   if (!FullName) { 
      SUMA_PushErrLog("SL_Err", "Need a FullName", FuncName); 
      SUMA_RETURN(dset); 
   }
   if (!farp) { 
      SUMA_PushErrLog("SL_Err", "NULL farp", FuncName); SUMA_RETURN(dset); 
   }
   far = *farp;
   if (!far) { 
      SUMA_PushErrLog("SL_Err", "NULL *farp", FuncName); SUMA_RETURN(dset); 
   }
   if (vec_len < 0 || vec_num < 0) { 
      SUMA_PushErrLog("SL_Err", "Negative vec_len or vec_num", FuncName);
      SUMA_RETURN(dset); 
   }
   if (ptr_cpy) { 
      SUMA_PushErrLog("SL_Err", "Pointer copy not supported yet", FuncName); 
      SUMA_RETURN(dset); 
   }
   
   if (vec_num > 200 * vec_len || vec_num > 50000){/* warning for MSB's mishap */
      char *eee = getenv("SUMA_1D_Transpose_Warn");
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
                        "SUMA_1D_Transpose_Warn = NO\n"
                        "in .sumarc if you do not want to see\n"
                        "this warning ever again.\n"
                        , FuncName);
            /* return the first time with NULL */
            ++nwarn; SUMA_RETURN(NULL);
         } 
      }
   }
   dset = SUMA_CreateDsetPointer( FullName, SUMA_NODE_BUCKET, 
                                  dset_id, dom_id,  vec_len  ); 
   
   /* now add the columns */
   for (i=0; i<vec_num; ++i) {
      if (!SUMA_AddDsetNelCol (dset, "numeric", SUMA_NODE_FLOAT, 
                               (void *)(&(far[i*vec_len])), NULL ,1)) {
         SUMA_PushErrLog("SL_Crit", "Failed in SUMA_AddDsetNelCol", FuncName);
         SUMA_FreeDset((void*)dset); dset = NULL;
         SUMA_RETURN(dset);
      }
   }
      
   if (ptr_cpy) *farp = NULL;

   SUMA_RETURN(dset);
}

SUMA_DSET *SUMA_iar2dset_ns( char *FullName, char *dset_id, char *dom_id, 
                              int **iarp, int vec_len, int vec_num, 
                              int ptr_cpy) 
{
   SUMA_DSET *dset = SUMA_iar2dset_eng( FullName, dset_id, dom_id, 
                                 iarp, vec_len, vec_num, 
                                 ptr_cpy);
   WorkErrLog_ns();
   return(dset);
}
SUMA_DSET *SUMA_iar2dset_eng( char *FullName, char *dset_id, char *dom_id, 
                                 int **iarp, int vec_len, int vec_num, 
                                 int ptr_cpy) 
{
   static char FuncName[]={"SUMA_iar2dset_eng"};
   SUMA_DSET *dset = NULL;
   int i = 0;
   int *iar = NULL;

   SUMA_ENTRY;
   
   if (!FullName) { 
      SUMA_PushErrLog("SL_Err", "Need a FullName", FuncName); 
      SUMA_RETURN(dset); 
   }
   if (!iarp) { 
      SUMA_PushErrLog("SL_Err", "NULL iarp", FuncName); SUMA_RETURN(dset); 
   }
   iar = *iarp;
   if (!iar) { 
      SUMA_PushErrLog("SL_Err", "NULL *iarp", FuncName); SUMA_RETURN(dset); 
   }
   if (vec_len < 0 || vec_num < 0) { 
      SUMA_PushErrLog("SL_Err", "Negative vec_len or vec_num", FuncName);
      SUMA_RETURN(dset); 
   }
   if (ptr_cpy) { 
      SUMA_PushErrLog("SL_Err", "Pointer copy not supported yet", FuncName); 
      SUMA_RETURN(dset); 
   }
   
   dset = SUMA_CreateDsetPointer( FullName, SUMA_NODE_BUCKET, dset_id, 
                                  dom_id,  vec_len  ); 
   
   /* now add the columns */
   for (i=0; i<vec_num; ++i) {
      if (!SUMA_AddDsetNelCol (dset, "numeric", SUMA_NODE_INT, 
                               (void *)(&(iar[i*vec_len])), NULL ,1)) {
         SUMA_PushErrLog("SL_Crit", "Failed in SUMA_AddDsetNelCol", FuncName);
         SUMA_FreeDset((void*)dset); dset = NULL;
         SUMA_RETURN(dset);
      }
   }
      
   if (ptr_cpy) *iarp = NULL;

   SUMA_RETURN(dset);
}

int SUMA_is_AllConsistentNumeric_dset(SUMA_DSET *dset, SUMA_VARTYPE *vtpp) 
{
   static char FuncName[]={"SUMA_is_AllConsistentNumeric_dset"};
   int ctp, vtp, vtpc = SUMA_notypeset, i;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(0);
   
   for (i=0; i<SDSET_VECNUM(dset); ++i) {
      ctp = SUMA_TypeOfDsetColNumb(dset, i); 
      vtp = SUMA_ColType2TypeCast(ctp) ;
      if (vtp < SUMA_byte || vtp > SUMA_double) SUMA_RETURN(0);
      if (i==0) { vtpc = vtp; }
      else if (vtp != vtpc) SUMA_RETURN(0);
   }
   
   if (vtpp) *vtpp = vtpc;
   SUMA_RETURN(1);
}

/*! 
   Are all columns of the same column type ctpi
   If you just care for a consistency test, set ctpi to SUMA_ERROR_COL_TYPE
*/
int SUMA_is_AllConsistentColType_dset(SUMA_DSET *dset, SUMA_COL_TYPE ctpi) 
{
   static char FuncName[]={"SUMA_is_AllConsistentColType_dset"};
   int ctp0 = SUMA_ERROR_COL_TYPE, ctp, i;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(0);
   
   for (i=0; i<SDSET_VECNUM(dset); ++i) {
      ctp = SUMA_TypeOfDsetColNumb(dset, i); 
      /* SUMA_S_Note("Dset %s: Type of col %d = [%d, %s], target [%d %s]",
                  SDSET_LABEL(dset), i, ctp, SUMA_Col_Type_Name(ctp),
                  ctpi, SUMA_Col_Type_Name(ctpi)); */
      if (ctpi>SUMA_ERROR_COL_TYPE && ctp != ctpi) SUMA_RETURN(0);
      if (i==0) { ctp0 = ctp; }
      else if (ctp0 != ctp) SUMA_RETURN(0);
   }
   SUMA_RETURN(1);
}

int SUMA_GetConsistentColType_dset(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_GetConsistentColType_dset"};
   int ctp0 = SUMA_ERROR_COL_TYPE, ctp, i;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(SUMA_ERROR_COL_TYPE);
   
   for (i=0; i<SDSET_VECNUM(dset); ++i) {
      ctp = SUMA_TypeOfDsetColNumb(dset, i); 
      if (i==0) { ctp0 = ctp; }
      else if (ctp0 != ctp) SUMA_RETURN(SUMA_ERROR_COL_TYPE);
   }
   SUMA_RETURN(ctp0);
}

int SUMA_is_AllConsistentCastType_dset(SUMA_DSET *dset, int typecast) 
{
   static char FuncName[]={"SUMA_is_AllConsistentCastType_dset"};
   int ctp, vtp, i;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(0);
   
   for (i=0; i<SDSET_VECNUM(dset); ++i) {
      ctp = SUMA_TypeOfDsetColNumb(dset, i); 
      vtp = SUMA_ColType2TypeCast(ctp) ;
      if (vtp != typecast) SUMA_RETURN(0);
   }
   
   SUMA_RETURN(1);
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
   \brief requirements to be a label dset:
   1- 1 column of data with type SUMA_NODE_ILABEL
   that is it for now.
*/
int SUMA_is_Label_dset(SUMA_DSET *dset, NI_group **NIcmap) 
{
   static char FuncName[]={"SUMA_is_Label_dset"};
   int ctp, vtp, i;
   NI_group *ngr=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(0);
   
   if (SUMA_isVolDataset(dset)) {
      if(SUMA_GetAtlasLabelTable(dset) || SUMA_GetValueLabelTable(dset)) {
         SUMA_RETURN(1);
      } 
   }
   
   SUMA_LH( "All Cons Type = %d : %d\n"
            "SDSET_TYPE (%s) = %d\n",
            SUMA_NODE_ILABEL,
            SUMA_is_AllConsistentColType_dset(dset, SUMA_NODE_ILABEL),
            SDSET_LABEL(dset),
            SDSET_TYPE (dset));
            
   if (!SUMA_is_AllConsistentColType_dset(dset, SUMA_NODE_ILABEL)) 
      SUMA_RETURN(0);
   
   /* Check on the dset_type attribute.  
      This check is needed to tell the difference between
      NODE_ROI datasets that are treated as regular datasets,
      and NODE_LABEL which are rendered differently.
      Both types have a dset column that is of type 
      SUMA_NODE_ILABEL*/
   
   if (SDSET_TYPE(dset) != SUMA_NODE_LABEL) { SUMA_RETURN(0); }  
   
   /* Does the dset have a colormap ?*/
   if ((ngr = SUMA_NI_Cmap_of_Dset(dset))) {
      /* OK, have colormap */
      if (NIcmap) *NIcmap = ngr;
   } else {
      /* do not reject this yet */
      if (NIcmap) *NIcmap = NULL;
   }
   
   SUMA_LH("dset %s considered Label_dset", SDSET_LABEL(dset));
   SUMA_RETURN(1);
}

/* sort of like is_Label_dset, but here we just worry
about one data column. We also require that there be 
a colormap in the dataset somewhere. This was added
to handle GIFTI datasets that are labels but that seem 
to get translate to one column of labels and other columns
of generic_ints. The dataset came from the HCP's Workbench
and I am not sure if the problem is one of translation on our
part or one of GIFTI formatting from Workbench. 
For now, this function would allow for the proper display of
such a dataset.   ZSS Feb. 20 2014 */
int SUMA_is_Label_dset_col(SUMA_DSET *dset, int icol) 
{
   static char FuncName[]={"SUMA_is_Label_dset_col"};
   int ctp, vtp, i;
   NI_group *ngr=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || icol < 0) SUMA_RETURN(0);
   
   if (SUMA_TypeOfDsetColNumb(dset, icol) != SUMA_NODE_ILABEL) SUMA_RETURN(0); 
   if (SDSET_TYPE(dset) != SUMA_NODE_LABEL) { SUMA_RETURN(0); }  
   
   /* Does the dset have a colormap ?*/
   if (!(ngr = SUMA_NI_Cmap_of_Dset(dset))) {
      /* reject out of caution */
      SUMA_RETURN(0);
   }
   
   SUMA_RETURN(1);
}


/*! 
   \brief requirements to be a VFR dset:
   1- 1st column of data with type SUMA_NODE_VFR
*/
int SUMA_is_VFR_dset(SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_is_VFR_dset"};
   int ctp, vtp, i;
   NI_group *ngr=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(0);
   
   i = 0;
   ctp = SUMA_TypeOfDsetColNumb(dset, i);    
   if (LocalHead) {
         fprintf(SUMA_STDERR,"%s: ctp(%d) = %d (%d)\n",
               FuncName, i, ctp, SUMA_NODE_VFR);
   }
   if (ctp == SUMA_NODE_VFR) SUMA_RETURN(1);
      
      
   SUMA_RETURN(0);
}

/*! 
   \brief requirements to be a phase dset:
   1- 1st column of data with type SUMA_NODE_PHASE
*/
int SUMA_is_Phase_dset(SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_is_Phase_dset"};
   int ctp, vtp, i;
   NI_group *ngr=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(0);
   
   i = 0;
      ctp = SUMA_TypeOfDsetColNumb(dset, i); 
      if (LocalHead) {
         fprintf(SUMA_STDERR,"%s: ctp(%d) = %d (%d)\n",
               FuncName, i, ctp, SUMA_NODE_PHASE);
      }
      if (ctp == SUMA_NODE_PHASE) SUMA_RETURN(1);
      
   SUMA_RETURN(0);
}

/*! 
   \brief requirements to be a retinotopically derived angle:
   Checks labels of columns
*/
int SUMA_is_RetinoAngle_dset(SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_is_RetinoAngle_dset"};
   int ctp, vtp, i;
   NI_group *ngr=NULL;
   char *lblcp=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(0);
   
   /* check based on label, rather than column type.
   Dset is created with 3d program and it is a pain 
   to add the column type there at the moment */
    
   i = 0;   
   lblcp = SUMA_DsetColLabelCopy(dset, 0, 0);
   if (strstr(lblcp, "Polar Angle")) i = 1;
   else if (strstr(lblcp, "Eccentricity")) i = 1;
   else if (strncmp(lblcp, "Phz@", 4) == 0) i = 1;  
   else if (strncmp(lblcp, "Phz_Delay", 5) == 0) i = 1;
   
   SUMA_free(lblcp);
   SUMA_RETURN(i);
}



NI_group *SUMA_NI_Cmap_of_Dset(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_NI_Cmap_of_Dset"};
   NI_group *ngr=NULL;
   NI_element *nel=NULL;
   char *s=NULL;
   int ip=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr) SUMA_RETURN(NULL);
   
   for( ip=0 ; ip < dset->ngr->part_num ; ip++ ){ 
      switch( dset->ngr->part_typ[ip] ){
         case NI_GROUP_TYPE:
            ngr = (NI_group *)dset->ngr->part[ip];
            if (LocalHead)  {
                     fprintf( SUMA_STDERR,
                              "%s:  Looking for %s   in group %s \n",
                              FuncName, "AFNI_labeltable", ngr->name);
            }
            if (!strcmp("AFNI_labeltable", ngr->name)) {
               if (!NI_get_attribute(ngr,"Name")) {
                  s = SUMA_append_string("LT_", SDSET_LABEL(dset));
                  NI_set_attribute(ngr,"Name",s); SUMA_free(s); s= NULL; 
               }  
               SUMA_RETURN(ngr);
            }
            ngr=NULL; /* not good, keep looking */
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)dset->ngr->part[ip] ;
            break;
         default:
            SUMA_SL_Err("Don't know what to make of this group element\n"
                        "ignoring.");
            break;
      }
   }

   SUMA_RETURN(ngr);
}

/* Column of unique values, in Label dset DO NOT FREE */
int * SUMA_UniqueValuesInLabelDset(SUMA_DSET *dset, int *N_unq)
{
   static char FuncName[]={"SUMA_UniqueValuesInLabelDset"};
   NI_element *nel=NULL;
   
   SUMA_ENTRY;
   
   if (!dset || !N_unq) {
      SUMA_RETURN(NULL);
   }
   *N_unq = 0;
   
   if (!SUMA_is_Label_dset(dset, NULL)) {
      SUMA_RETURN(NULL);
   }
   
   nel = SUMA_GetUniqueValsAttr(dset, 0);
   if (!nel) {
      SUMA_SetUniqueValsAttr(dset, 0, 0);
   }
   nel = SUMA_GetUniqueValsAttr(dset, 0);
   if (!nel) {
      SUMA_RETURN(NULL);
   }
   
   *N_unq = nel->vec_len;
   
   SUMA_RETURN((int *)nel->vec[0]);
}

int SUMA_is_TimeSeries_dset(SUMA_DSET *dset, double *TRp) 
{
   static char FuncName[]={"SUMA_is_TimeSeries_dset"};
   char *mm=NULL;
   double TR=-1.0;
   
   SUMA_ENTRY;
   
   if (TRp) *TRp = TR;
   if (!SUMA_is_AllNumeric_dset(dset)) SUMA_RETURN(0);
   if (!dset->dnel) SUMA_RETURN(0);
   
   
   mm = NI_get_attribute(dset->dnel, "ni_timestep");
   if ( !mm ) SUMA_RETURN(0); 
   TR = strtod(mm, NULL);
   if (TR > 100) { /* likely a bug in dsets prior to Sep 19 09 */
      SUMA_S_Warn("ni_timestep may be incorrectly specified in msec.\n"
                  "Time units should be in sec.");
      if (TR > 360) { /* just cut the damned thing down */            
         SUMA_S_Warn("TR > 360, reduced it by a factor of 1000.\n");
         TR *= 0.001;
      }
   }
   if (TRp) *TRp = TR;
   if ( mm && TR >= 0.0) SUMA_RETURN(1); 
   
   SUMA_RETURN(0);
}

SUMA_Boolean SUMA_SetDsetTR(SUMA_DSET *dset, double TR)
{
   static char FuncName[]={"SUMA_SetDsetTR"};
   char ccc[32];

   SUMA_ENTRY;
   
   if (TR < 0 || !dset || !dset->dnel) SUMA_RETURN(NOPE);
   if (!SUMA_is_AllNumeric_dset(dset)) SUMA_RETURN(NOPE);
   sprintf(ccc,"%f", TR);
   NI_set_attribute(dset->dnel, "ni_timestep", ccc);
   SUMA_RETURN(YUP);
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
      if (  SUMA_IS_DATUM_INDEX_COL(ctp) ||
            ctp == SUMA_NODE_ILABEL ||
            ctp == SUMA_NODE_SLABEL ||
            ctp == SUMA_NODE_STRING ||
            ctp == SUMA_GNODE_IGROUP ) 
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
   if (SUMA_isGraphDsetNgr(ngr))
        sname = SUMA_append_string(NEL_DSET_TYPE(ngr),"_edge_indices");
   else sname = SUMA_append_string(NEL_DSET_TYPE(ngr),"_node_indices");
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
   
   /* 
      Macro NEL_WRITE_1D still needs this ...
      
      SUMA_SL_Warn("Obsolete, perhaps. Check on caller.");
   */
   
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
   
   dx = (SUMA_OPEN_DX_STRUCT *)SUMA_calloc(1,sizeof(SUMA_OPEN_DX_STRUCT));
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
   for (i=0; i<SUMA_MAX_OPEN_DX_FIELD_COMPONENTS; ++i) { 
      dx->comp_name[i] = dx->comp_value[i] =NULL; }
   dx->n_attr = 0;
   for (i=0; i<SUMA_MAX_OPEN_DX_FIELD_ATTRIBUTES; ++i) { 
      dx->attr_name[i] = dx->attr_string[i] =NULL; }
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
         SUMA_SL_Warn("Do not know how to free datap.\n"
                      "You now possibly have a leak on your hands.");
      }
   }
   if (dx->type) SUMA_free(dx->type); dx->type = NULL;
   for (i=0; i<SUMA_MAX_OPEN_DX_FIELD_COMPONENTS; ++i) { 
      if (dx->comp_name[i]) SUMA_free(dx->comp_name[i]); dx->comp_name[i] = NULL;
      if (dx->comp_value[i]) SUMA_free(dx->comp_value[i]); 
      dx->comp_value[i] =NULL; 
   }
   for (i=0; i<SUMA_MAX_OPEN_DX_FIELD_ATTRIBUTES; ++i) { 
      if (dx->attr_name[i]) SUMA_free(dx->attr_name[i]); dx->attr_name[i] = NULL;
      if (dx->attr_string[i]) SUMA_free(dx->attr_string[i]); 
      dx->attr_string[i] =NULL; 
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
         if (dx->object) 
            SS = SUMA_StringAppend_va(SS, "object: %s\n", dx->object);
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
   
   if (!oName) { 
      SUMA_PushErrLog("SL_Err", "Null Name", FuncName); SUMA_RETURN(dset); }
   
   /* SUMA_S_Note(oName); */
   /* remove [] if existing */
   nstrip = SUMA_copy_string(oName);
   for (i=0; i<strlen(nstrip); ++i) 
      if (nstrip[i] == '[') { nstrip[i] = '\0'; break; }
   
   /* work the name */
   if (!SUMA_filexists(nstrip)) {
      /* try the extension game */
      FullName = SUMA_Extension(nstrip, ".1D.dset", NOPE);
      if (!SUMA_filexists(FullName)) {
         if (verb)  { 
            SUMA_PushErrLog("SL_Err", "Failed to find dset file.", FuncName); }
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
   if (ParentOfDsetToLoad) 
      name = SUMA_append_string(ParentOfDsetToLoad, FullName);
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
float *SUMA_Load1D_ns (char *oName, int *ncol, int *nrow, int RowMajor, 
                       int verb)
{
   float *far=SUMA_Load1D_eng (oName, ncol, nrow,  RowMajor,  verb);
   WorkErrLog_ns();
   return(far);
}
float *SUMA_Load1D_eng (char *oName, int *ncol, int *nrow, int RowMajor, 
                        int verb)
{
   static char FuncName[]={"SUMA_Load1D_eng"};
   char *FullName = NULL;
   MRI_IMAGE *im = NULL, *imt = NULL;
   float *far=NULL;
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!oName) { 
      SUMA_PushErrLog("SL_Err", "Null Name", FuncName); SUMA_RETURN(NULL); }
   
   /* got the name, now read it */
   im = mri_read_1D (oName);
   if (!im) {
      if (verb) SUMA_PushErrLog("SLP_Err","Failed to read file", FuncName);
      SUMA_RETURN(NULL);
   }   
   *ncol = im->ny;
   *nrow = im->nx;
   if (LocalHead) fprintf(SUMA_STDERR, 
                     "Read %s, found %d cols, %d rows\n", oName, *ncol, *nrow);
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
   \brief Return a dataset's data size 
*/
long SUMA_sdset_dnel_size(SUMA_DSET *dset)
{
   int ii=0;
   long jj=0;
   
   if (!dset || !dset->dnel) SUMA_RETURN(-1); 
   for( jj=ii=0 ; ii < dset->dnel->vec_num ; ii++ )
      jj += NI_size_column( NI_rowtype_find_code(dset->dnel->vec_typ[ii]) ,
                            dset->dnel->vec_len , dset->dnel->vec[ii] ) ;
   return(jj);
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

SUMA_Boolean SUMA_NewDsetID2 (SUMA_DSET *dset, char *str)
{
   static char FuncName[]={"SUMA_NewDsetID2"};
   char *namecode;
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(NOPE);
   if (!dset->ngr) SUMA_RETURN(NOPE);
   if (str) {
      namecode = UNIQ_hashcode(str);  /* from str */
      NI_set_attribute (dset->ngr, "self_idcode", namecode); 
      SUMA_free(namecode); namecode = NULL;
   } else if (NI_get_attribute(dset->ngr, "filename")) {
      namecode = UNIQ_hashcode(NI_get_attribute(dset->ngr, "filename")); 
               /* from filename */
      NI_set_attribute (dset->ngr, "self_idcode", namecode); 
      SUMA_free(namecode); namecode = NULL;
   } else {
      SUMA_NewDsetID (dset);
   }
   
   
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

char *SUMA_sdset_label(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_sdset_label"};
   char *s;
      
   if (!dset || !dset->ngr || !(s=NI_get_attribute(dset->ngr,"label"))) {
      return("");
   }
   return(s);
}

char *SUMA_sdset_filename(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_sdset_filename"};
   char *s;
      
   if (!dset || !dset->ngr || !(s=NI_get_attribute(dset->ngr,"filename"))) {
      return("");
   }
   return(s);
}


SUMA_DATUM_LEVEL SUMA_sdset_datum_level(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_sdset_datum_level"};
   SUMA_DATUM_LEVEL dtlvl = SUMA_ELEM_DAT;
   
   if (!dset || !dset->ngr) return(dtlvl);
   NI_GET_INT(dset->ngr, "Datum_Level", dtlvl);
   if (!NI_GOT) dtlvl = SUMA_ELEM_DAT;
   
   return(dtlvl);
}

SUMA_Boolean SUMA_sdset_set_datum_level(SUMA_DSET *dset, SUMA_DATUM_LEVEL lvl)
{
   static char FuncName[]={"SUMA_sdset_set_datum_level"};
   
   if (!dset || !dset->ngr ||
         lvl < SUMA_ELEM_DAT || lvl >= SUMA_N_LEV_DAT) return(NOPE);
   
   NI_SET_INT(dset->ngr, "Datum_Level", (int)lvl);
   return(YUP);
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

/* Return orientation code if available */
char *SUMA_Dset_orcode(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_Dset_orcode"};
   static int ncall = 0;
   int *iv=NULL;
   NI_element *nelb = NULL;
   static char ccc[10][3];
   char *orcode;
   
   SUMA_ENTRY;
   
   ++ncall; if (ncall > 9) ncall = 0;
   orcode = (char *)ccc[ncall];
   orcode[0] = orcode[1] = orcode[2] = 'X'; orcode[3] = '\0';
   if (!dset) SUMA_RETURN(orcode);   
   if (!(nelb = SUMA_FindDsetAttributeElement(dset, "ORIENT_SPECIFIC"))) {
      SUMA_S_Err("No ORIENT SPECIFIC");
      SUMA_RETURN(orcode); 
   }
   iv = (int *)nelb->vec[0];
   orcode[0] = ORIENT_typestr[iv[0]][0] ;
   orcode[1] = ORIENT_typestr[iv[1]][0] ;
   orcode[2] = ORIENT_typestr[iv[2]][0] ;
   orcode[3] = '\0';
   
   SUMA_RETURN(orcode);
}


/*! A function to change SUMA_DSET * to THD_3dim_dataset *
This would work for float only dsets, function is in its
infancy
   \param dsetp (SUMA_DSET **) Pointer to a dset pointer
                             *dsetp will be transformed to THD_3dim_dataset *
                             The contents of SPARSE_DATA inside dset will get
                             wiped out regardless of cleardset, once they are
                             copied into adset. 
   \param copy_data  (int) if != 0 then the columns are populated.
                           Otherwise you get attributes and indices, no more.                      
   \param cleardset (int) Call SUMA_FreeDset on *dsetp and set *dsetp to NULL.
                          Only allowed if copy_data is set.
   \return adset (THD_3dim_dataset *) an AFNI dataset version of *dsetp
   
   \sa SUMA_afnidset2sumadset
*/
THD_3dim_dataset *SUMA_sumadset2afnidset(
      SUMA_DSET **dsetp, int copy_data, int cleardset)
{
   static char FuncName[]={"SUMA_sumadset2afnidset"};
   SUMA_DSET *dset = NULL;
   THD_3dim_dataset *newset=NULL;
   int rv = -1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dsetp) { SUMA_S_Err("Null of Null you are!"); SUMA_RETURN(newset); }
   dset = *dsetp;
   if (!dset) { SUMA_S_Err("NULL *dsetp."); SUMA_RETURN(newset); }
   
   if (!copy_data && cleardset) {
      SUMA_S_Err(
         "Requesting no datacopy and cleardset. \n"
         "That's a combo I can refuse.\n");
      SUMA_RETURN(newset); 
   }
   if (!SUMA_is_AllNumeric_dset(dset)) { 
      SUMA_S_Err("*dsetp is not all numeric"); 
      SUMA_RETURN(newset);   
   }
   if (!dset->ngr || !dset->dnel) {
      fprintf( SUMA_STDERR,
               "Error %s: NULL dset contents: ngr=%p , dnel=%p\n", 
               FuncName, dset->ngr, dset->dnel);
      SUMA_RETURN(newset); 
   }
   
   if (!dset->inel || !dset->inel->vec_num) {
      if (!SUMA_PopulateDsetNodeIndexNel(dset,1)) {
            SUMA_S_Err("Failed to add node index column");
            exit(1);
      }
   }
   set_ni_globs_from_env();
   if (LocalHead) set_gni_debug(1);
   newset = THD_ni_surf_dset_to_afni(dset->ngr, 0);
   if (copy_data) {
      SUMA_LH("Copying data");
      rv = THD_add_sparse_data(newset->dblk->parent, dset->ngr);
      if( rv <= 0 ){
          fprintf(SUMA_STDERR,
                  "Error %s: add sdata returned %d for '%s'\n",
                  FuncName, rv, SDSET_LABEL(dset));
          DSET_delete(newset); newset=NULL;
          SUMA_RETURN(newset);
      }
      else if( rv < newset->dblk->nvals ){
          fprintf(SUMA_STDERR,
                  "Error %s: loaded only %d vols for '%s'\n",
                  FuncName, rv,SDSET_LABEL(dset));
          DSET_delete(newset); newset=NULL;
          SUMA_RETURN(newset);
      }
   }
   SUMA_LH("Clearification, perhaps");
   if (cleardset) { SUMA_FreeDset(dset); *dsetp = NULL; }
    
   SUMA_RETURN(newset);
}    

/*! A function to change THD_3dim_dataset * to SUMA_DSET *
This would work for float only dsets, function is in its
infancy
   \param dsetp (THD_3dim_dataset **) Pointer to a dset pointer
                             *dsetp will be transformed to SUMA_DSET *
   \param copy_data  (int) if != 0 then the SPARSE_DATA element is populated.
                           Otherwise you get attributes and indices, no more.                      
   \param cleardset (int) Call DSET_delete on *dsetp and set *dsetp to NULL.
                          Only allowed if copy_data is set.
   \param floatize (int) if 1, floatize the input, unless it is complex.
                            0, leave as is.
                           -1, abide by the default (floatize is on),
                               and by the value of env var: AFNI_NSD_TO_FLOAT
                               which would be ignored otherwise.
   \return sdset (SUMA_DSET *) a SUMA dataset version of *dsetp
   
   \sa SUMA_sumadset2afnidset
*/
SUMA_DSET *SUMA_afnidset2sumadset(
      THD_3dim_dataset **dsetp, 
      int copy_data, int cleardset, int floatize)
{
   static char FuncName[]={"SUMA_afnidset2sumadset"};
   int ntf, vv;
   THD_3dim_dataset *dset = NULL;
   SUMA_DSET *newset=NULL;
   NI_group *ngr=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dsetp) { SUMA_S_Err("Null of Null you are!"); SUMA_RETURN(newset); }
   if (!copy_data && cleardset) {
      SUMA_S_Err( "Requesting no datacopy and cleardset. \n"
                  "That's a combo I can refuse.\n");
      SUMA_RETURN(newset); 
   }
   dset = *dsetp;
   if (!dset) { SUMA_S_Err("NULL *dsetp."); SUMA_RETURN(newset); }

   if (is_Dset_Atlasy(dset,NULL)) {
      SUMA_LH("This is an atlas");
   } else {
      SUMA_LH("No atlas");
   }
   if (floatize == -1) {
      set_ni_globs_from_env();
      ngr = THD_dset_to_ni_surf_dset(dset, copy_data);
   } else {
      ntf = get_gni_to_float(); set_gni_to_float(floatize);
      ngr = THD_dset_to_ni_surf_dset(dset, copy_data);
      set_gni_to_float(ntf);
   }
   if (!ngr) {
      SUMA_S_Err("Failed in THD_dset_to_ni_surf_dset");
      SUMA_RETURN(newset);
   }else {
      /* Keep track of HEADNAME of input */
      NI_set_attribute(ngr,"DSET_HEADNAME", DSET_HEADNAME(dset));
      if (!(newset = SUMA_ngr_2_dset(ngr, LocalHead))) {
         SUMA_S_Err("Failed to go from ngr to dset");
         SUMA_RETURN(newset);
      }
   }
   
   if (cleardset) { 
      SUMA_LH("Clearing dset ..."); 
      DSET_delete(dset); *dsetp = NULL; 
   }
    
   SUMA_RETURN(newset);
}    


float SUMA_fdrcurve_zval( SUMA_DSET *dset , int iv , float thresh )
{
   static char FuncName[]={"SUMA_fdrcurve_zval"};
   floatvec *fv = NULL ; 
   float val =0.0, *v=NULL ;
   NI_element *nelb=NULL;
   int nv=-1;
   char name[100]={""};
   
   SUMA_ENTRY;

   if( !dset || iv < 0 || iv >= SDSET_VECNUM(dset) ) SUMA_RETURN(0.0f) ;
   
   sprintf(name,"FDRCURVE_%06d",iv) ;
   nelb = SUMA_FindNgrAttributeElement(dset->ngr, name);
   if (!nelb || !nelb->vec_num) SUMA_RETURN(0.0f) ;

   v = (float *)nelb->vec[0];
   nv = nelb->vec_len - 2 ;
   MAKE_floatvec(fv,nv) ;
   fv->x0 = v[0] ; fv->dx = v[1] ;
   memcpy( fv->ar , v + 2 , sizeof(float)*nv ) ;   
   val = interp_floatvec(fv,thresh);
   KILL_floatvec(fv);
   
   SUMA_RETURN ( val ) ;
}


SUMA_DSET_FORMAT SUMA_FormatFromFormString(char *arg) {
   static char FuncName[]={"SUMA_FormatFromFormString"};
   
   SUMA_DSET_FORMAT oform = SUMA_ERROR_DSET_FORMAT;
   
   SUMA_ENTRY;
   
   if (!arg) {
      SUMA_RETURN(oform);
   } else if ((strcmp(arg, "1d") == 0)) {
     oform = SUMA_1D;
   } else if ((strcmp(arg, "1dp") == 0)) {
      oform = SUMA_1D_PURE;
   } else if ((strcmp(arg, "1dpt") == 0)) {
      oform = SUMA_1D_PURE_TRANSPOSE;
   } else if ((strcmp(arg, "1d_stderr") == 0)) {
      oform = SUMA_1D_STDERR;
   } else if ((strcmp(arg, "1dp_stderr") == 0)) {
      oform = SUMA_1D_PURE_STDERR;
   } else if ((strcmp(arg, "1dpt_stderr") == 0)) {
      oform = SUMA_1D_PURE_STDERR_TRANSPOSE;
   } else if ((strcmp(arg, "1d_stdout") == 0)) {
      oform = SUMA_1D_STDOUT;
   } else if ((strcmp(arg, "1dp_stdout") == 0)) {
      oform = SUMA_1D_PURE_STDOUT;
   } else if ((strcmp(arg, "1dpt_stdout") == 0)) {
      oform = SUMA_1D_PURE_STDOUT_TRANSPOSE;
   } else if ((strcmp(arg, "niml_stderr") == 0)) {
      oform = SUMA_NIML_STDERR;
   } else if ((strcmp(arg, "niml_stdout") == 0)) {
      oform = SUMA_NIML_STDOUT;
   } else if (  (
         (strcmp(arg, "niml") == 0) ||
         (strcmp(arg, "nii") == 0) ) )  {
      oform = SUMA_NIML;
   } else if (  (
         (strncmp(arg, "niml_asc", 8) == 0)||
         (strncmp(arg, "nii_asc", 7) == 0) ||
         (strncmp(arg, "ni_as",5) == 0) ) ) {
      oform = SUMA_ASCII_NIML;
   } else if (  (
         (strncmp(arg, "niml_bi", 7) == 0)||
         (strncmp(arg, "nii_bi", 6) == 0) ||
         (strncmp(arg, "ni_bi", 5) == 0) ) ) {
      oform = SUMA_BINARY_NIML;
   } else if (  (
         (strncmp(arg, "gii", 3) == 0) ||
         (strncmp(arg, "gifti", 5) == 0) ) ) {
      if (AFNI_strcasestr(arg,"asc"))
         oform = SUMA_XML_ASCII_DSET;
      else if (AFNI_strcasestr(arg,"b64gz"))
         oform = SUMA_XML_B64GZ_DSET;
      else if (AFNI_strcasestr(arg,"b64"))
         oform = SUMA_XML_B64_DSET;
      else oform = SUMA_XML_DSET;
   }
   
   SUMA_RETURN(oform);
}




/*! 
   Form the final output name from prefix.
   free returned full name
*/
char *SUMA_OutputDsetFileStatus(char *prefix, char *inname, 
                            SUMA_DSET_FORMAT *oformp, 
                            char *pre, char *app, int *exists)
{
   static char FuncName[]={"SUMA_OutputDsetFileStatus"};
   SUMA_PARSED_NAME *Test=NULL;
   char *opref=NULL;
   SUMA_DSET_FORMAT oform = SUMA_NO_DSET_FORMAT;
   
   SUMA_ENTRY;
   
   if (oformp) oform = *oformp;
   
   /* settle on best oform */
   if (oform == SUMA_NO_DSET_FORMAT) {
      oform = SUMA_GuessFormatFromExtension(prefix, NULL);
      if (oform == SUMA_NO_DSET_FORMAT && inname) {
         Test = SUMA_ParseFname(inname, NULL);
         oform = SUMA_GuessFormatFromExtension(Test->HeadName, NULL);
         Test = SUMA_Free_Parsed_Name(Test);
      }
   }
   if (oform == SUMA_NO_DSET_FORMAT) oform = SUMA_NIML;
   
   /* remove possible extensions from prefix */
   opref = SUMA_RemoveDsetExtension_ns(prefix, oform);
   
   if (app) {
      Test = SUMA_ParseModifyName(opref, "append", app, NULL);
      SUMA_free(opref); opref = SUMA_copy_string(Test->HeadName);
      Test = SUMA_Free_Parsed_Name(Test);
   }
   if (pre) {
      Test = SUMA_ParseModifyName(opref, "prepend", pre, NULL);
      SUMA_free(opref); opref = SUMA_copy_string(Test->HeadName);
      Test = SUMA_Free_Parsed_Name(Test);
   }
   opref = SUMA_append_replace_string(opref,
                  (char *)SUMA_ExtensionOfDsetFormat(oform),"",1);
   if (exists) {
      if (THD_is_file(opref)) *exists = 1;
      else *exists = 0;
   }
   
   if (oformp) *oformp = oform;
   
   SUMA_RETURN(opref);
}


/* Note that this function is not getting called anymore.
DBG_sigfunc() in debugtrace.h is being called instead */
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
   fprintf(stderr,"*** SUMA Abort ***\nCompile Date: %s\n",
                          __DATE__) ; fflush(stderr) ;
   selenium_close(); /* close any selenium opened browser windows if open */
   
   if( sig != SIGINT && sig != SIGTERM ){  /* add crashlog [14 Apr 2015] */
     FILE *dfp ; char *home , fname[1024] ;
     home = THD_homedir(0) ;
     strcat(fname,"/.afni.crashlog") ;
     fprintf(stderr,"** If you report this crash to the AFNI message\n"
                    "** board, please copy the error messages EXACTLY.\n"
                    "** Crash log recorded in: %s\n",
                     fname ) ;
     
     dfp = fopen( fname , "a" ) ;
     if( dfp != NULL ){
       fprintf(dfp,
         "\n*********-----------------------------------------------*********") ;
       fprintf(dfp,"\nFatal Signal %d (%s) received\n",sig,sname); 
       fflush(stderr);
#ifdef USE_TRACING
       DBG_tfp = dfp ; DBG_traceback() ; DBG_tfp = stderr ;
#endif
       fprintf(stderr,"*** SUMA Abort ***\nCompile Date: %s\n",
                          __DATE__) ; fflush(stderr) ;
#ifdef SHSTRING
       fprintf(dfp,"** [[Precompiled binary " SHSTRING ": " __DATE__ "]]\n") ;
#endif
       fprintf(dfp,"** SUMA Program Tragically Lost **\n") ;
       fclose(dfp) ;
     }
   }
   exit(sig) ;
}



/*!
   \brief replace the col th string attribute in a one-string nel
   
   Warning: 
   This function gets non-linearly slower for dsets with very large numbers
   of columns. This is going to hurt some day.... The problem is that there 
   are numerous instances of string allocating, copying, appending, freeing,
   over and over and over.
   
*/
int SUMA_AddColAtt_CompString(NI_element *nel, int col, 
                              char *lbl, char *sep, int insertmode)
{
   static char FuncName[]={"SUMA_AddColAtt_CompString"};
   NI_str_array *nisa = NULL;
   char *cs=NULL, *ns=NULL;
   int i, num_string_components=0;
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
   
   #if 0
   if (!(col % 1000)) { 
      LocalHead=YUP;
      SUMA_LHv("%d\n", col); 
   }
   #endif
   num_string_components = SUMA_NI_get_num_strings(cs, sep);
   SUMA_LHv("num_string_components=%d in cs=%s (sep=%s), col=%d, lbl=%s,"
            "insertmode=%d\n",
            num_string_components, cs, sep, col, lbl, insertmode);
    
   if (num_string_components < 0 || col != num_string_components) {
      nisa = SUMA_comp_str_2_NI_str_ar(cs, sep);
      if (!nisa) { 
         SUMA_SL_Err("Failed in SUMA_comp_str_2_NI_str_ar"); SUMA_RETURN(NOPE); }

      if (LocalHead) {
         fprintf(SUMA_STDERR, 
                  "%s: col = %d, num_string_components = %d nisa->num = %d\n", 
                   FuncName, col, num_string_components, nisa->num);
      }
      if (col > nisa->num) { 
         SUMA_SL_Err("col > nisa->num"); SUMA_DUMP_TRACE("nisa nisa"); 
         SUMA_RETURN(NOPE); }
      num_string_components = nisa->num;
   } 
   
   if (col == num_string_components) { /* add at the end */
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: append %s to end of string, memcheck: %s\n", 
                  FuncName, lbl, NI_malloc_status());
      #if 0 /* this gets real slow when adding column after column for 
               dsets with a very large number of columns */
      ns = SUMA_append_replace_string(cs, lbl, sep, 0);
      SUMA_NEL_REPLACE_STRING(nel, 0, 0, ns);   
      #else /* ZSS: Nov 26 08 modify the pointer directly 
               (still slow, but less so...)*/
      { 
      char **rc;
      int n0, n1, n2, nalloc;
      if (cs) n0 = strlen(cs); else n0 = 0; 
      if (sep) n1 = strlen(sep); else n1 = 0; 
      if (lbl) n2 = strlen(lbl); else n2 = 0;
      nalloc = SUMA_NI_get_int(nel,"alloc_max");
      SUMA_LHv("nalloc now %d versus %d with cs=%s and lbl=%s\n", 
               nalloc, n0+n1+n2+1, CHECK_NULL_STR(cs), CHECK_NULL_STR(lbl));
      if (nalloc < (n0+n1+n2+1)) { /* and a little faster July 2012 
                                    still need to go faster, but 
                                    bottle neck may be elsewhere 
                                    Note that if you add then readjust
                                    by editing the mid section, all this 
                                    speed up effort becomes a real bad idea
                                    See what I do with alloc_max in the
                                    next blocs below. 
                                    Need to get away from single composite
                                    strings, but that wouldstop being compatible
                                    with AFNI. Perhaps make the change at read, 
                                    and undo it just before write.
                                    */
         nalloc += (n0+n1+n2+1)+8192;
         cs = (char *)NI_realloc(cs, char,
                  nalloc*sizeof(char));
         SUMA_NI_set_int(nel,"alloc_max", nalloc);
      }

      if (!SUMA_isExtension(cs, sep)) {
         i = 0; while(sep[i]) { cs[n0++] = sep[i]; ++i; }            
      }
      if (n2) {
         i = 0; while(lbl[i]) { cs[n0++] = lbl[i]; ++i; }
      }
      cs[n0] = '\0';
      if (!(rc = (char **)(nel->vec[0]))) {
         SUMA_S_Err("Unexpected null nel->vec[0]");
         SUMA_RETURN(NOPE); 
      }
      rc[0] = cs; 
      
      if (0) {
         SUMA_LHv("cs (%d strings) now: %s\n", 
                     SUMA_NI_get_num_strings(cs, sep), cs);
      } else {
         SUMA_LHv("cs (%d strings), memcheck:%s\n", 
                     SUMA_NI_get_num_strings(cs, sep), NI_malloc_status());
      }
      }
      #endif
   } else if (!insertmode) { /* REPLACE! in middle */
      if (nisa->str[col]) NI_free(nisa->str[col]); nisa->str[col] = NULL;
      if (lbl) {
         nisa->str[col] = (char*)NI_malloc(char, (strlen(lbl)+1)*sizeof(char));
         strcpy( nisa->str[col],  lbl ); 
         if (LocalHead) 
            fprintf( SUMA_STDERR,
                     "%s: replaced %s at location %d (%d)\n", 
                     FuncName, lbl, col, nisa->num);
         ns = SUMA_NI_str_ar_2_comp_str(nisa, sep);
         if (LocalHead)
            fprintf(SUMA_STDERR,"%s: final string is %s\n", FuncName, ns);
         SUMA_NEL_REPLACE_STRING(nel, 0, 0, ns);
         /* Make sure alloc_max is reset, this is a new pointer and the
         pre-allocation done above is of no further use */
         SUMA_NI_set_int(nel,"alloc_max", strlen(ns));
         /* SUMA_DUMP_TRACE(NI_malloc_status()); */
      }
   } else { /* insert in middle */
      int n2;
      if (lbl) n2 = strlen(lbl); else n2 = 0;
      nisa->str = NI_realloc( nisa->str, char*, sizeof(char *)*(nisa->num+1) ) ;
      /* now move all above col to end */
      for (i=nisa->num-1; i >= col; --i) nisa->str[i+1] = nisa->str[i];
      nisa->str[col] = (char*)NI_malloc(char, (n2+1)*sizeof(char));
      if (lbl) strcpy( nisa->str[col],  lbl ); 
      else nisa->str[col][0] = '\0';
      ++nisa->num;
      if (LocalHead) 
         fprintf( SUMA_STDERR,
                  "%s: inserted %s at location %d\n", FuncName, lbl, col);
      ns = SUMA_NI_str_ar_2_comp_str(nisa, sep);
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: final string is %s\n", FuncName, ns);
      SUMA_NEL_REPLACE_STRING(nel, 0, 0, ns);
      SUMA_NI_set_int(nel,"alloc_max", strlen(ns));
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
void *SUMA_BinarySuck(char *fname, SUMA_VARTYPE data_type, int endian, 
                      int start, int end, int *nvals_read)
{
   static char FuncName[]={"SUMA_BinarySuck"};
   int bs, End, chnk, read_n, N_alloc, ex;
   FILE *fp=NULL;
   void *ans = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   *nvals_read = 0;
   ans = NULL;

   if (!SUMA_filexists(fname)) { 
      SUMA_SL_Err("File not found or could not be read"); goto CLEAN_EXIT; 
   }
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
   if (LocalHead) 
      fprintf(SUMA_STDERR,"%s: Expecting to read %d values\n", 
                           FuncName, N_alloc);
   
   ex = 0;
   switch (data_type) {
      case SUMA_float:
         {
            float *vec = (float *)SUMA_malloc(N_alloc * sizeof(float));
            if (!vec) { SUMA_SL_Err("Failed to allocate"); goto CLEAN_EXIT;  }
            SUMA_LH("Reading floats");
            ex = fread((void*)vec, chnk, N_alloc, fp);
            if (ex != N_alloc) { 
               SUMA_SL_Err("Failed to read all data!"); SUMA_free(vec); 
               goto CLEAN_EXIT; 
            }
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
            if (ex != N_alloc) { 
               SUMA_SL_Err("Failed to read all data!"); SUMA_free(vec); 
               goto CLEAN_EXIT; 
            }
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
            if (ex != N_alloc) { 
               SUMA_SL_Err("Failed to read all data!"); SUMA_free(vec); 
               goto CLEAN_EXIT; 
            }
            if (bs) { SUMA_LH("swapping 1 byte numbers!?");  }
            ans = (void*)vec;
         }
         break;
      case SUMA_short:
         {
            short *vec = (short *)SUMA_malloc(N_alloc * sizeof(short));
            if (!vec) { SUMA_SL_Err("Failed to allocate"); goto CLEAN_EXIT;  }
            SUMA_LH("Reading shorts");
            ex = fread((void*)vec, chnk, N_alloc, fp);
            if (ex != N_alloc) { 
               SUMA_SL_Err("Failed to read all data!"); 
               SUMA_free(vec); goto CLEAN_EXIT; 
            }
            if (bs) { SUMA_LH("swapping");  SUMA_SWAP_VEC(vec,N_alloc,chnk); }
            ans = (void*)vec;
         }
         break;
      case SUMA_complex:
         SUMA_SL_Err("Complex data type not supported");
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
   *fbuf = buf ; 
   buf[ii] = '\0';
   SUMA_RETURN(ii) ;
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
   if( fname == NULL || fname[0] == '\0') SUMA_RETURN(NULL) ;

   len = THD_filesize( fname ) ;
   if( len <= 0 ) SUMA_RETURN(buf) ;

   buf = (char *) SUMA_malloc( sizeof(char) * (len+4) ) ;
   if( buf == NULL ) SUMA_RETURN(buf) ;

   fd = open( fname , O_RDONLY ) ;
   if( fd < 0 ) SUMA_RETURN(buf) ;

   ii = read( fd , buf , len ) ;
   close( fd ) ;
   if( ii <= 0 ){ SUMA_free(buf) ; buf = NULL; SUMA_RETURN(buf); }
   *nread = ii ; buf[ii]='\0'; SUMA_RETURN(buf) ;
}


/************** END Miscellaneous support function    ************** */

/******** BEGIN functions for surface structure  ******************** */
SUMA_SO_SIDE SUMA_giiStringToNumSide(char *cc)
{
   static char FuncName[]={"SUMA_giiStringToNumSide"};
   char *hasright=NULL, *hasleft=NULL;
   SUMA_ENTRY;
   
   if (!cc) SUMA_RETURN(SUMA_NO_SIDE);
   
   deblank_name(cc);
   hasleft = AFNI_strcasestr(cc,"Left");
   hasright = AFNI_strcasestr(cc,"Right");
   if ( hasleft  && !hasright ) {
      SUMA_RETURN(SUMA_LEFT);
   } else if ( hasright && !hasleft ) {
      SUMA_RETURN(SUMA_RIGHT);
   } else if (    hasleft && hasright ) {
      SUMA_RETURN(SUMA_LR);
   }
   
   SUMA_RETURN(SUMA_NO_SIDE);         
}

      /* see some basic functions in suma_afni_surface.c */
void SUMA_ShowAfniSurfaceObject( NI_group *aSO, FILE *out, 
                                 int detail, char *title)
{
   static char FuncName[]={"SUMA_ShowAfniSurfaceObject"};
   char *s=NULL;
   SUMA_ENTRY;
   
   if (!out) out = stdout;
   
   s = SUMA_AfniSurfaceObject_Info(aSO, detail, title);
   fprintf(out,"%s",s);
   SUMA_free(s); s = NULL;
   SUMA_RETURNe;
}

char *SUMA_AfniSurfaceObject_Info(NI_group *aSO, 
                                  int detail, char *title)
{
   static char FuncName[]={"SUMA_AfniSurfaceObject_Info"};
   int i=0, j=0, n=0;
   char *s=NULL, stmp[200];
   float *fv=NULL;
   int *iv=NULL;
   double xform[4][4];
   NI_element *nelxyz=NULL;
   NI_element *nelijk=NULL;
   NI_element *nelxform=NULL;
   NI_element *nelnormals=NULL;
   NI_element *nel=NULL;
   SUMA_STRING *SS=NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (title) SS = SUMA_StringAppend(SS, title);
   if (!aSO) {
      SS = SUMA_StringAppend(SS, "NULL Afni Surface Object\n");
   } else {
      nelxyz = SUMA_FindNgrNamedElement(aSO, "Node_XYZ");
      nelijk = SUMA_FindNgrNamedElement(aSO, "Mesh_IJK");
      nelnormals = SUMA_FindNgrNamedElement(aSO, "Node_Normals");
      nelxform = SUMA_FindNgrNamedElement(aSO, "Coord_System");
      
      if (!nelxyz) {
         SS = SUMA_StringAppend(SS, "NULL ps\n");
      } else {
         NI_GET_INT(nelxyz,"NodeDim",n);
         SS = SUMA_StringAppend_va(SS,   "Have %d (%dD) nodes:\n", 
                           n > 0 ? nelxyz->vec_len/n:-1, n);
         if (!nelxyz->vec_num) {
            SS = SUMA_StringAppend(SS, "   NULL NodeList\n");
         } else {
            fv = (float *)nelxyz->vec[0];
            for (i=0; i<SUMA_MIN_PAIR(5,nelxyz->vec_len/n); ++i) {
               SS = SUMA_StringAppend_va(SS, "   %6d: ", i); 
               for (j=0; j<n; ++j) 
                  SS = SUMA_StringAppend_va(SS, 
                           "%4.3f\t", 
                           fv[n*i+j]);
               SS = SUMA_StringAppend(SS, "\n");
            }
         }
      }
      if (!nelijk) {
         SS = SUMA_StringAppend(SS, "NULL tr\n");
      } else {
         NI_GET_INT(nelijk,"FaceSetDim",n);
         SS = SUMA_StringAppend_va(SS,   "Have %d (%dD) polygons:\n", 
                           n > 0 ? nelxyz->vec_len/n:-1, n);
         if (!nelijk->vec_num) {
            SS = SUMA_StringAppend(SS, "   NULL FaceSetList\n");
         } else {
            iv = (int *)nelijk->vec[0];
            for (i=0; i<SUMA_MIN_PAIR(5,nelxyz->vec_len/n); ++i) {
               SS = SUMA_StringAppend_va(SS, "   %6d: ", i); 
               for (j=0; j<n; ++j) 
                  SS = SUMA_StringAppend_va(SS, 
                           "%6d\t",
                           iv[n*i+j]);
               SS = SUMA_StringAppend(SS, "\n");
            }
         }
      }

      if (!nelnormals) {
         SS = SUMA_StringAppend(SS, "No Node Normals\n");
      } else {
         if (nelxyz) {
            NI_GET_INT(nelxyz,"NodeDim",n);
            SS = SUMA_StringAppend_va(SS,   "Have %d (%dD) node normals:\n", 
                                             n > 0 ? nelnormals->vec_len/n:-1,
                                             n); 
         if (!nelnormals->vec_num) {
               SS = SUMA_StringAppend(SS, "NULL Node Normals Vector\n");
            } else {
               fv = (float *)nelnormals->vec[0];
               for (i=0; i<SUMA_MIN_PAIR(5,nelxyz->vec_len/n); ++i) {
                  SS = SUMA_StringAppend_va(SS, "   %6d: ", i); 
                  for (j=0; j<n; ++j) 
                     SS = SUMA_StringAppend_va(SS, 
                              "%4.3f\t", 
                              fv[n*i+j]);
                     SS = SUMA_StringAppend(SS, "\n");
               }
            }
         } else {
            SS = SUMA_StringAppend(SS, 
                     "Freakish structure, no pointset to go with nornals\n");
         }
      }
      
      if (!nelxform) {
         SS = SUMA_StringAppend(SS, "No xform\n");
      } else {
         double *dv=NULL;
         int k;
         SS = SUMA_StringAppend_va(SS, "dataspace: %s\n"
                                       "xformspace: %s\n", 
                  SUMA_CHECK_NULL_STR(NI_get_attribute(nelxform, "dataspace")),
                  SUMA_CHECK_NULL_STR(NI_get_attribute(nelxform, "xformspace")));
         dv = (double *)nelxform->vec[0];
         for (i=0; i<4;++i) {
            k = 4*i;
            SS = SUMA_StringAppend_va(SS, "  %.4f   %.4f   %.4f   %.4f\n",
                                     dv[k], dv[k+1], dv[k+2], dv[k+3]);
         }
      }
   }
   
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(s);
}


/******** END functions for surface structure  ******************** */

/************************ BEGIN GICOR functions  ******************** */

/* A function to initialize the setup structure for GroupInCorr     */
/* bmode: a flag for batch mode to save dset    17 Aug 2012 [rickr] */
int SUMA_init_GISET_setup(NI_stream nsg , NI_element *nel, GICOR_setup *giset,
                          int bmode)
{
   static char FuncName[]={"SUMA_init_GISET_setup"};
   char *atr=NULL , *pre=NULL, *s=NULL, sbuf[256];
   THD_3dim_dataset *tdset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   giset->ns    = nsg ;  /* save socket for I/O back to 3dGroupInCorr */
   giset->ready = 0 ;    /* not ready yet */

   /* set various parameters from the NIML header */

   atr = NI_get_attribute( nel , "ndset_A" ) ; 
      if( atr == NULL )        SUMA_RETURN(NOPE);
   giset->ndset_A = (int)strtod(atr,NULL) ;    
      if( giset->ndset_A < 2 ) SUMA_RETURN(NOPE);

   atr = NI_get_attribute( nel , "ndset_B" ) ; 
      if( atr == NULL )        SUMA_RETURN(NOPE);
   giset->ndset_B = (int)strtod(atr,NULL) ;

   atr = NI_get_attribute( nel , "nvec" ) ;  
      if( atr == NULL )        SUMA_RETURN(NOPE);
   giset->nvec = (int)strtod(atr,NULL) ;       
      if( giset->nvec < 2 )    SUMA_RETURN(NOPE);
   
   atr = NI_get_attribute( nel , "geometry_string" ); 
      if( atr == NULL ) {
         SUMA_S_Err("No geometry string");
         SUMA_RETURN(NOPE);
      }
      pre = SUMA_copy_string(atr) ;
      tdset = EDIT_geometry_constructor( pre , "GrpInCorr" ) ;
      if( tdset == NULL ) {
         snprintf(sbuf, 256, 
            "ERROR %s: Could not construct dset from %s\n",FuncName, pre);
         SUMA_S_Err("%s", sbuf);
         SUMA_RETURN(NOPE) ;
      }
      giset->nvox = DSET_NVOX(tdset) ;
      /* if batch mode, store dataset           17 Aug 2012 [rickr] */
      if( bmode ) giset->dset = tdset;
      else { DSET_delete(tdset); tdset=NULL; }
      SUMA_free(pre); pre=NULL;
      
   if( giset->nvox < 2 )    SUMA_RETURN(NOPE);

   atr = NI_get_attribute( nel , "seedrad" ) ;
   if( atr != NULL ) giset->seedrad = (float)strtod(atr,NULL) ;

   atr = NI_get_attribute( nel , "ttest_opcode" ) ;
   if( atr != NULL ) giset->ttest_opcode = (int)strtod(atr,NULL) ;

   /* How many dsets? */
   if (LocalHead) {
      snprintf(sbuf, 256,
            "LH %s: attr=%s\nval0=%s,val1=%s\n", 
            FuncName, NI_get_attribute(nel,"LRpair_nnode"),
            SUMA_NI_get_ith_string(NI_get_attribute(nel,"LRpair_nnode"),",",0),
            SUMA_NI_get_ith_string(NI_get_attribute(nel,"LRpair_nnode"),",",1));
      SUMA_LH("%s",sbuf);
   }
   if ((s=SUMA_NI_get_ith_string(
               NI_get_attribute(nel,"LRpair_nnode"),",",0))) {
      giset->nnode_domain[0] = (int)strtol(s, NULL, 10);
      SUMA_free(s); s = NULL;
      if ((s=SUMA_NI_get_ith_string(
               NI_get_attribute(nel,"LRpair_nnode"),",",1))) {
         giset->nnode_domain[1] = (int)strtol(s, NULL, 10);
         SUMA_free(s); s = NULL;
      }
   } else {
      giset->nnode_domain[0] = giset->nvox; 
      giset->nnode_domain[1] = 0; 
   }
               
   if ((s=SUMA_NI_get_ith_string(
               NI_get_attribute(nel,"LRpair_ninmask"),",",0))) {
      giset->nnode_mask[0] = (int)strtol(s, NULL, 10);
      SUMA_free(s); s = NULL;
      if ((s=SUMA_NI_get_ith_string(
               NI_get_attribute(nel,"LRpair_ninmask"),",",1))) {
         giset->nnode_mask[1] = (int)strtol(s, NULL, 10);
         SUMA_free(s); s = NULL;
      }
   } else {
      giset->nnode_mask[0] = giset->nnode_domain[0]; 
      giset->nnode_mask[1] = giset->nnode_domain[1];
   }
   
   /* list of voxels to expect from each 3dGroupInCorr data */
   if( nel->vec_len == 0 || nel->vec_num == 0 || nel->vec == NULL ){  /* all */
     giset->ivec = NULL ; giset->nivec = 0 ;
      INFO_message("DEBUG: GICOR_setup_func has ivec=NULL") ; 
   } else {                                     /* make index list of voxels */
     int ii , nn , *iv=(int *)nel->vec[0] ;
     giset->ivec = (int *)calloc(sizeof(int),giset->nvec) ;
     nn = MIN(giset->nvec,nel->vec_len) ; giset->nivec = nn ;
     for( ii=0 ; ii < nn ; ii++ ) giset->ivec[ii] = iv[ii] ;
     INFO_message("DEBUG: GICOR_setup_func has ivec=int[%d]",nn) ; 
   }

   if (NI_get_attribute(nel, "target_labels")) {
      giset->brick_labels = strdup(NI_get_attribute(nel, "target_labels"));
   }
   
   atr = NI_get_attribute( nel , "target_nvals" ) ;
   if( atr != NULL ){ 
      giset->nvals = (int)strtod(atr,NULL); 
      giset->nvals = SUMA_MAX_PAIR(1,giset->nvals); 
   }
   
   SUMA_RETURN(YUP);
}

int SUMA_PopulateDsetsFromGICORnel(NI_element *nel, GICOR_setup *giset, 
                                   SUMA_DSET **sdsetv)
{
   static char FuncName[]={"SUMA_PopulateDsetsFromGICORnel"}; 
   char *sbuf=NULL;
   float *neldar , *nelzar , *dsdar , *dszar ;
   int nn , id=0, ipair=0, nvec=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!nel || !giset || !sdsetv) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   } 
   
   for (id=0; id < 2; ++id) {
      for (ipair=0; ipair < nel->vec_num/2; ++ipair) {
         neldar = (float *)nel->vec[2*ipair+0] ;  /* delta array */
         nelzar = (float *)nel->vec[2*ipair+1] ;  /* zscore array */
         nvec   = nel->vec_len ;

         if (giset->nnode_domain[id]) {
            dsdar = (float *)SDSET_VEC(sdsetv[id],(2*ipair+0)) ;
            dszar = (float *)SDSET_VEC(sdsetv[id],(2*ipair+1)) ;
            if (LocalHead) {
               sbuf=SUMA_ShowMeSome(dsdar,
                           SUMA_float, SDSET_VECLEN(sdsetv[id]),10,"dsdar:\n");
               SUMA_LHv("pre copy surf%d %s\n",id, sbuf); 
               SUMA_free(sbuf); sbuf=NULL;
            }

            if( giset->ivec == NULL ){  /* all nodes */
               if (giset->nvox != nvec) {
                  SUMA_S_Errv( "nvox=%d, nvec=%d, ivec=NULL\n"
                              "Did not expect that.\n",
                              giset->nvox, nvec);
                  SUMA_RETURN(NOPE) ;
               }
               if (id == 0) {
                  nn = MAX(0, nvec-giset->nnode_domain[1]);
                  SUMA_LHv("Copying %d values from neldar, surf%d\n", 
                           nn, id);
                  if (LocalHead) {   
                     sbuf=SUMA_ShowMeSome(neldar,SUMA_float, nn,10,"neldar:\n");
                     SUMA_LHv("from the tube surf%d: %s\n", id, sbuf); 
                     SUMA_free(sbuf); sbuf=NULL;
                  }
                  memcpy(dsdar,neldar,sizeof(float)*nn) ;
                  memcpy(dszar,nelzar,sizeof(float)*nn) ;
               } else {
                  nn = MAX(0, nvec-giset->nnode_domain[0]);
                  SUMA_LHv("Copying %d values from neldar+%d, surf%d\n", 
                           nn, giset->nnode_domain[0], id);
                  if (LocalHead) {
                     sbuf=SUMA_ShowMeSome((neldar+giset->nnode_domain[0]),
                                          SUMA_float, nn, 10,"neldar:\n");
                     SUMA_LHv("from the tube surf%d: %s\n", id, sbuf); 
                     SUMA_free(sbuf); sbuf=NULL;
                  }
                  memcpy(dsdar,(neldar+giset->nnode_domain[0]),
                           sizeof(float)*nn) ;
                  memcpy(dszar,(nelzar+giset->nnode_domain[0]),
                           sizeof(float)*nn) ;
               }
               if (LocalHead) {
                  sbuf=SUMA_ShowMeSome(dsdar,SUMA_float, nn, 10,"dsdar:\n");
                  SUMA_LHv("post copy surf%d %s\n", id, sbuf); 
                  SUMA_free(sbuf); sbuf=NULL;
               }
            } else { /* Have index vector */
               int *ivec=giset->ivec , kk ;
               nn = MIN( giset->nnode_mask[id] , nvec ) ;
               if (id == 0) {
                  for( kk=0 ; kk < nn ; kk++ ){
                     dsdar[ivec[kk]] = neldar[kk] ; 
                     dszar[ivec[kk]] = nelzar[kk] ;
                  }
               } else {
                  for( kk=0 ; kk < nn ; kk++ ){
                     dsdar[ivec[kk]-giset->nnode_domain[0]] = neldar[kk] ; 
                     dszar[ivec[kk]-giset->nnode_domain[0]] = nelzar[kk] ;
                  }
               }
            }
            SUMA_LH("Updating range\n");
            if (!SUMA_UpdateDsetColRange(sdsetv[id],-1)) {
               SUMA_S_Err("Failed to update range");
               SUMA_RETURN(NOPE);
            }
            /* Put a flag up to state that all overlays of this dataset need 
            to have column copied refreshed */
            if (sdsetv[id]->dnel) { 
               NI_set_attribute(sdsetv[id]->dnel,"ResetOverlay_Vecs", "yes");
            }
         } /* if (giset->nnode_domain[id]) */
      } /* for (ipair ...) */
   }

   SUMA_RETURN(YUP);
}

/************************ END GICOR functions  ********************* */

/************************ GRAPH Dset functions  ******************** */
/* use macro: SUMA_isGraphDset_fast() for speed 
   This one always goes back to the source */
byte SUMA_isGraphDset(SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_isGraphDset"};
   
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr) SUMA_RETURN(NOPE);
   
   if (!dset->Aux) { /* create one, always good */
      if (!SUMA_Add_Dset_Aux(dset)) {
         SUMA_S_Err("Bad news, this should not fail");
         SUMA_RETURN(NOPE);
      }      
   }
   if (SUMA_isGraphDsetNgr(dset->ngr)) {
      dset->Aux->isGraph = GRAPH_DSET;
   }
   
   SUMA_RETURN(dset->Aux->isGraph == GRAPH_DSET);
}

byte SUMA_isGraphDsetNgr(NI_group *ngr)
{
   if (!ngr) return(0);
   switch (SUMA_Dset_Type(NEL_DSET_TYPE(ngr))) {
      case SUMA_GRAPH_BUCKET:
         return(1);
      default:
         return(0);
   }
   return(0);
}

/*!
   Turn a bunch of matrices into a graph dataset.
   vec (float **) A collection of vec_num arrays with each array representing
                  a matrix of length vec_len.
                  Matrix k is stored in vec[k] and has a total of vec_len
                  elements. 4 types of matrices can be stored.
                  1- An MxM square matrix is stored in column major order
                     vec_len = M*M 
                  2- A lower triangular matrix with diagonal is stored in
                     column major order, vec_len = M*(M+1)/2
                  3- A lower triangular matrix  is stored in column major 
                     order with vec_len = M*(M-1)/2
                  4- A sparse M*M matrix of vec_len elements. For this you
                     will need to specify ie, i0, and i1 (see below)
   vec_num (int) : number of matrices
   vec_len (int) : number of elements in vec[k]
   mtype   (char *) : Choose from NULL (let function guess) or "full", 
                      "lower_diag", "lower", or "sparse"
   vec_labs(char **) : labels of each matrix, vec_num of them. Send NULL for
                       default labeling
   The next three parameters are used when the matrices are sparse
   ie (int *): If not NULL, ie contains the 'edge/cell' index of the matrix 
               where entries exist. If null, the edges are numbered from 0
               to vec_len-1
   i0 (int *): row indices
   i1 (int *): column indices
*/
    
SUMA_DSET *SUMA_FloatVec_to_GDSET(float **vec, int vec_num, int vec_len, 
                                  char *mtype,
                                  char **vec_labs, int *ie, int *i0, int *i1)
{
   static char FuncName[]={"SUMA_FloatVec_to_GDSET"};
   SUMA_DSET *dset=NULL;
   char *lab=NULL, sbuf[32];
   int i;
   
   SUMA_ENTRY;
   
   if (!(dset =  SUMA_CreateDsetPointer( FuncName, SUMA_NODE_BUCKET,
                                         NULL, NULL, vec_len ))) {
      SUMA_S_Err("Failed to create dset");
      SUMA_RETURN(dset);
   }
   
   for (i=0; i<vec_num; ++i) {
      if (vec_labs) lab = vec_labs[i];
      else {
         sprintf(sbuf,"Mat%3d",i);
         lab = sbuf;
      }
      if (!SUMA_AddDsetNelCol(dset, lab, 
                              SUMA_NODE_FLOAT, (void *)vec[i], NULL, 1)) {
         SUMA_S_Errv("Failed to add column %d %s\n", i, lab);
         SUMA_FreeDset(dset); dset=NULL;
         SUMA_RETURN(dset);
      }
   }
   
   /* Now turn dset to graph */
   if (!SUMA_Dset_to_GDSET(&dset, mtype, 0, ie, i0, i1)) {
      SUMA_S_Err("Failed to graphize");
      SUMA_FreeDset(dset); dset = NULL;
      SUMA_RETURN(dset);
   }
   
   SUMA_RETURN(dset);
}

/* Turn a node-based dset structure into a graph structure, 
   cunningly still called SUMA_DSET                         */ 
SUMA_Boolean SUMA_Dset_to_GDSET(SUMA_DSET **pdset, char *mtype,
                                int ok_verticalize, 
                                int *ie, int *i0, int *i1)
{
   static char FuncName[]={"SUMA_Dset_to_GDSET"};
   int npts = -1, nseg=0, msz[2];
   SUMA_DSET *tdset=NULL, *dset=NULL;
   float tt=-1.0;
   char *dname=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!pdset) SUMA_RETURN(NOPE);
   dset = *pdset;
   if (!dset || !dset->ngr || !dset->dnel) SUMA_RETURN(NOPE);
   
   SUMA_LH("Matrix type %s", mtype?mtype:"NULL");

   if (!SUMA_isGraphDset(dset)) {
      if (SDSET_VECNUM(dset) == SDSET_VECLEN(dset) &&
          ok_verticalize) {/* vertically catenate all columns.
                           Only recommended if going from 
                           an MxM dataset that is just 
                           one matrix as opposed to M square matrices
                           of dimension sqrt(M) */
         tdset = SUMA_VcatDset(dset, NULL, NULL, 0, 0);
         *pdset = tdset; SUMA_FreeDset(dset); dset=tdset; tdset=NULL;
      }
      
      /* label this thing as a graph */
      NI_set_attribute (dset->ngr, "dset_type", 
                        SUMA_Dset_Type_Name(SUMA_GRAPH_BUCKET));
      /* Set type of dnel or it can't be located */
      dname = SUMA_append_string(SUMA_Dset_Type_Name(SUMA_GRAPH_BUCKET), 
                                                               "_data");
      NI_set_attribute (dset->dnel, "data_type", dname); 
      SUMA_free(dname); dname = NULL;
      
      /* get rid of old inel, if any, and put in a new one for graph dsets*/
      SUMA_Reset_NodeIndex_Element(dset, NULL);
      
      /* figure out the matrix size */
      npts = -1;
      if (mtype) {
         if (!strcmp(mtype,"lower")) {
            if (!((tt = (1+sqrt(1+8*nseg))/2.0) == (int)tt)) {
               SUMA_S_Errv("Can't get lower triangular matrix from %d values\n",
                           nseg);
               SUMA_RETURN(NOPE);
            }
            npts = tt;
         } else if (!strcmp(mtype,"lower_diag")) {
            if (!((tt = (-1+sqrt(1+8*nseg))/2.0) == (int)tt)) {
               SUMA_S_Errv("Can't get lower tri+diag matrix from %d values\n",
                           nseg);
               SUMA_RETURN(NOPE);
            }
            npts = tt;
         } else if (!strcmp(mtype,"full")) {
            if (!((tt = sqrt(SDSET_VECLEN(dset))) == (int)tt)) {
               SUMA_S_Errv("Can't get full square matrix from %d values\n",
                           nseg);
               SUMA_RETURN(NOPE);
            }
            npts = tt;
         } else if (!strcmp(mtype,"sparse")) {
            if (!i0 || !i1) {
               SUMA_S_Err("Must provide i0 and i1 for spare matrices");
               SUMA_RETURN(NOPE);
            }
         }
      }
      /* cases where the whole matrix is packed into 1 column */
      nseg = SDSET_VECLEN(dset);
      if (npts == 1) { /* singletons are full */
         msz[0]= msz[1] = npts;
         NI_SET_INTv(dset->dnel,"matrix_size", msz,2);
         NI_set_attribute(dset->dnel, "matrix_shape",
                          SUMA_matrix_shape_to_matrix_shape_name(MAT_FULL));
      } else if ((mtype && !strcmp(mtype,"lower")) || 
          ((!i0 && !i1) && (tt = (1+sqrt(1+8*nseg))/2.0) == (int)tt)) { 
         if (npts < 0) npts = tt;
         SUMA_LHv("Me thinks it triangular Pa, %d %d\n", 
               SDSET_VECNUM(dset), SDSET_VECLEN(dset));
         /* M*(M-1)/2, lower tri, without the diagonal */
         msz[0]= msz[1] = npts;
         NI_SET_INTv(dset->dnel,"matrix_size", msz,2);
         NI_set_attribute(dset->dnel, "matrix_shape",
                          SUMA_matrix_shape_to_matrix_shape_name(MAT_TRI));
         /* Edges are implicitly defined, example for a 3x3 
            seg 0 --> 1,0
            seg 1 --> 2,0
            seg 2 --> 2,1
         */
      } else if ((mtype && !strcmp(mtype,"lower_diag")) || 
                 ((!i0 && !i1) && (tt = (-1+sqrt(1+8*nseg))/2.0) == (int)tt)) { 
         if (npts < 0) npts = tt;
         SUMA_LHv("Me thinks it triangular Paps, %d %d\n", 
               SDSET_VECNUM(dset), SDSET_VECLEN(dset));
         /* M*(M+1)/2, lower tri, with the diagonal */
         msz[0]= msz[1] = npts;
         NI_SET_INTv(dset->dnel,"matrix_size", msz,2);
         NI_set_attribute(dset->dnel, "matrix_shape",
                           SUMA_matrix_shape_to_matrix_shape_name(MAT_TRI_DIAG));
         /* Edges are implicitly defined, example for a 3x3 
            seg 0 --> 0,0
            seg 1 --> 1,0
            seg 2 --> 2,0
            seg 3 --> 1,1
            seg 4 --> 2,1
            seg 5 --> 2,2
         */
      } else if ((mtype && !strcmp(mtype,"full")) || 
                 ((!i0 && !i1) && (tt = sqrt(SDSET_VECLEN(dset))) == (int)tt)) {
         if (npts < 0) npts = tt;
         SUMA_LHv("A wholesome stick, %d %d\n", 
               SDSET_VECNUM(dset), SDSET_VECLEN(dset));
         /* full matrix, one column */
         msz[0]= msz[1] = npts;
         NI_SET_INTv(dset->dnel,"matrix_size", msz,2);
         NI_set_attribute(dset->dnel, "matrix_shape",
                          SUMA_matrix_shape_to_matrix_shape_name(MAT_FULL));
         /* Edges are implicitly defined, example for a 3x3 
            seg 0 --> 0,0
            seg 1 --> 1,0
            seg 2 --> 2,0
            seg 3 --> 0,1
            seg 4 --> 1,1
            seg 5 --> 2,1
            seg 6 --> 0,2
            seg 7 --> 1,2
            seg 8 --> 2,2
         */
      } else if (i0 && i1) {
         NI_element *nn=NULL;
         int *ieu=NULL, i=0;
         if (!i0 || !i1) {
            SUMA_S_Err("Cannot convert without more information,"
                       "will need an element describing segments");
            SUMA_RETURN(NOPE);
         }
         
         if (!ie) {
            if (!(ieu = (int *)SUMA_calloc(SDSET_VECLEN(dset), sizeof(int)))) {
               SUMA_S_Errv("Failed to allocate for %d elements\n", 
                           SDSET_VECLEN(dset));
               SUMA_RETURN(NOPE);
            }
            for (i=0; i<SDSET_VECLEN(dset); ++i) ieu[i]=i;
            ie = ieu;
         }
         if (!SUMA_AddDsetIndexCol(dset, ie, i0, i1)) {
            SUMA_S_Err("Failed to add edge list");
            SUMA_ifree(ieu); SUMA_RETURN(NOPE);
         }     
         if (ieu) {
            SUMA_free(ieu); 
         }
         
         SUMA_LHv("A sparse business %d %d\n", 
               SDSET_VECNUM(dset), SDSET_VECLEN(dset));
         if (!(SUMA_SetUniqueIndicesAttr(dset, 1))) {
            SUMA_S_Err("Failed to set unique index attributes");
            SUMA_RETURN(NOPE);
         }
         
         SUMA_LH("Get unique indices 1");
         if (!(nn = SUMA_GetUniqueIndicesAttr(dset,1))) {
            SUMA_S_Err("Failed to get unique indices");
            SUMA_RETURN(NOPE);
         }
         
         msz[0]= nn->vec_len;
         SUMA_LH("Get unique indices 2");
         if (!(nn = SUMA_GetUniqueIndicesAttr(dset,2))) {
            SUMA_S_Err("Failed to get unique indices");
            SUMA_RETURN(NOPE);
         }
         
         msz[1]= nn->vec_len;
         if ((tt=SDSET_VECLEN(dset)/(msz[0]*msz[1])) != (int)tt) {
            SUMA_S_Errv( "Number of entries per column (%d) is not an \n"
                         "integer number of unique edges %d x %d\n",
                         SDSET_VECLEN(dset), msz[0], msz[1]);
            SUMA_RETURN(NOPE);
         }
         NI_SET_INTv(dset->dnel,"matrix_size", msz,2);
         NI_set_attribute(dset->dnel, "matrix_shape",
                          SUMA_matrix_shape_to_matrix_shape_name(MAT_SPARSE));
      } else {
         SUMA_S_Err("Don't know how to graphize this set");
         SUMA_RETURN(NOPE);
      }
   } else {
      SUMA_LH("Nothing changed, odset already a graph");
   }
   
   /* set Aux info */
   if (!SUMA_GDSET_Set_Aux_matrix_shape(dset)) {
      SUMA_S_Err("Failed to set matrix shape");
      SUMA_RETURN(NOPE);
   }
   SUMA_LH("Done");
   
   SUMA_RETURN(YUP);
}

SUMA_SQ_MATRIX_SHAPES SUMA_matrix_shape_name_to_matrix_shape(char *name) 
{
   if (!name) return(MAT_UNKNOWN);
   
   if (!strcmp(name,"full")) return(MAT_FULL);
   if (!strcmp(name,"square")) { 
      fprintf(stderr,"square should be replaced by full\n"); return(MAT_FULL);
   }
   if (!strcmp(name,"tri")) return(MAT_TRI);
   if (!strcmp(name,"tri_diag")) return(MAT_TRI_DIAG);
   if (!strcmp(name,"sparse")) return(MAT_SPARSE);
   if (!strcmp(name,"festus")) return(MAT_HEEHAW); /* not set */
   if (!strcmp(name,"NA")) return(MAT_NA);
   
   return(MAT_UNKNOWN);
}

char * SUMA_matrix_shape_to_matrix_shape_name(SUMA_SQ_MATRIX_SHAPES sq) 
{
   switch (sq) {
      case MAT_FULL: return("full");
      case MAT_TRI: return("tri");
      case MAT_TRI_DIAG: return("tri_diag");
      case MAT_SPARSE: return("sparse");
      case MAT_HEEHAW: return("festus");
      case MAT_NA: return("NA");
      default:
         return("unknown");
   }
}

/*! Return the i,j, indices corresponding the to 1D index
    into the compact  columnwise storage of a triangular matrix.
    Use the R test function p2ij in CompactIndexing.R
    for details and tests.
    
    This function does not check if p is < 0 or exceeds the maximum
    index. 
   
   \sa macros SUMA_CItri_ij2p_diag, SUMA_CItri_ij2p, 
   SUMA_CItri_pmax_diag and SUMA_CItri_pmax
*/
byte SUMA_CItri_p2ij(int p, int n, int two_n, byte withdiag, int *i, int *j)
{
   int df;
   double b, D1;
   
   /* if (p<0) {*i=-1; *j=-1; return(0);} */
   if (withdiag) {
      if (p<n) {
         *j = 0;
         *i = p;   
      } /* else if (p > SUMA_CItri_pmax_diag(two_n/2)) {
         *i=-1; *j=-1; return(0);
      } */ else {
         b = (two_n-1);
         D1 = b*b-8*p;
         if (D1 < 0) D1 = 0;
         else D1 = sqrt(D1);
         *j = (int)(b - D1)/2;
            *i = p- (int)(*j*(b-*j))/2;
         df = *j-*i;
         while (df > 0) { /* this should only loop once if at all*/
            *j = *j-1-df/2;
            *i = p- (int)(*j*(b-*j))/2;
            df = *j-*i;
         }
      }
   } else {
      if (p<n-1) {
         *j = 0;
         *i = p+1;
      } /* else if (p > SUMA_CItri_pmax(two_n/2)) {
         *i=-1; *j=-1; return(0);
      } */ else  {
         b = (two_n-3);
         D1 = b*b-8*(p-1);
         if (D1 < 0) D1 = 0;
         else D1 = sqrt(D1);
         *j = (int)(b - D1)/2;
            *i = p- (int)(*j*(b-*j))/2 +1;
         df = *j-*i;
         while (df>=0.0) { /* this should only loop once if at all*/
            *j = *j-1-df/2;
            *i = p- (int)(*j*(b-*j))/2 +1;
            df = *j-*i;
         }
      } 
   }
   
   return(1);
}

/* Populate a few of the heavily used parameters from graph dset into Aux */
SUMA_Boolean SUMA_GDSET_Set_Aux_matrix_shape(SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_GDSET_Set_Aux_matrix_shape"};
   double range[2];
   int loc[2], buff=0;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   if (!dset->dnel) {
      SUMA_S_Err("No data bro.");
      SUMA_RETURN(NOPE);
   }
   if (!dset->Aux) {
      SUMA_S_Err("I expect this to be allocated for already for a graph dset");
      SUMA_RETURN(NOPE);
   }
   if (!SUMA_isGraphDset(dset)) {
      SUMA_S_Err("I expect this function to be run on graph dsets only");
      SUMA_RETURN(NOPE);
   }
   
   dset->Aux->matrix_shape = MAT_HEEHAW;
   dset->Aux->matrix_shape = SUMA_matrix_shape_name_to_matrix_shape(
                              NI_get_attribute(dset->dnel,"matrix_shape"));
   SUMA_LHv("shape: %s (%d), sq=%d\n", 
      NI_get_attribute(dset->dnel,"matrix_shape"), 
      dset->Aux->matrix_shape, MAT_FULL);
   
   SUMA_LHv("size: %s\n", NI_get_attribute(dset->dnel,"matrix_size"));
   NI_GET_INTv(dset->dnel, "matrix_size", dset->Aux->matrix_size, 2, 0);
   if (!NI_GOT) {
      SUMA_S_Err("Missing essential attribute 'matrix_size'");
      SUMA_RETURN(NOPE);
   }
   dset->Aux->matrix_2M = 2*dset->Aux->matrix_size[0];
   
   switch (dset->Aux->matrix_shape) {
      case MAT_FULL:
         dset->Aux->matrix_max_index = 
            dset->Aux->matrix_size[0]*dset->Aux->matrix_size[0]-1;
         dset->Aux->range_edge_index[0] = 0; 
         dset->Aux->range_edge_index[1] = dset->Aux->matrix_max_index;
         dset->Aux->range_node_index[0] = 0;
         dset->Aux->range_node_index[1] = dset->Aux->matrix_size[0]-1;
         dset->Aux->N_seg_nodes = dset->Aux->matrix_size[0];
         if (!SUMA_GDSET_GetPointIndexColumn(dset, 
                                       &buff, NULL)) {
            dset->Aux->N_all_nodes = dset->Aux->N_seg_nodes;
         } else dset->Aux->N_all_nodes = buff;
         break;
      case MAT_TRI:
         dset->Aux->matrix_max_index = 
            (dset->Aux->matrix_size[0]*(dset->Aux->matrix_size[0]-1))/2;
         dset->Aux->range_edge_index[0] = 0; 
         dset->Aux->range_edge_index[1] = dset->Aux->matrix_max_index;
         dset->Aux->range_node_index[0] = 0;
         dset->Aux->range_node_index[1] = dset->Aux->matrix_size[0]-1;
         dset->Aux->N_seg_nodes = dset->Aux->matrix_size[0];
         if (!SUMA_GDSET_GetPointIndexColumn(dset, 
                                       &buff, NULL)) {
            dset->Aux->N_all_nodes = dset->Aux->N_seg_nodes;
         } else dset->Aux->N_all_nodes = buff;
         break;
      case MAT_TRI_DIAG:
         dset->Aux->matrix_max_index = 
            (dset->Aux->matrix_size[0]*(dset->Aux->matrix_size[0]+1))/2;
         dset->Aux->range_edge_index[0] = 0; 
         dset->Aux->range_edge_index[1] = dset->Aux->matrix_max_index;
         dset->Aux->range_node_index[0] = 0;
         dset->Aux->range_node_index[1] = dset->Aux->matrix_size[0]-1;
         dset->Aux->N_seg_nodes = dset->Aux->matrix_size[0];
         if (!SUMA_GDSET_GetPointIndexColumn(dset, 
                                       &buff, NULL)){
            dset->Aux->N_all_nodes = dset->Aux->N_seg_nodes;
         } else dset->Aux->N_all_nodes = buff;
         break;
      case MAT_SPARSE:
         if (!dset->inel) {
            SUMA_S_Err("Don't have inel, badly shaped dataset");
            SUMA_RETURN(NOPE);
         }
         dset->Aux->matrix_max_index = dset->inel->vec_len;

         if (!SUMA_GetDsetNodeIndexColRange_eng(dset, range, loc, 0, 0)) {
            SUMA_LH("Failed to get erange?");
            dset->Aux->range_edge_index[0] = -2; /* error */
            dset->Aux->range_edge_index[1] = -2; /* error */
            SUMA_RETURN(NOPE);
         }
         dset->Aux->range_edge_index[0] = (long int)range[0];
         dset->Aux->range_edge_index[1] = (long int)range[1];
 
         if (!SUMA_GetDsetNodeIndexColRange_eng(dset, range, loc, 0, 1)) {
            SUMA_LH("Failed to get i0range?");
            dset->Aux->range_node_index[0] = -2; /* error */
            dset->Aux->range_node_index[1] = -2; /* error */
            SUMA_RETURN(NOPE);
         }
         dset->Aux->range_node_index[0] = (long int)range[0];
         dset->Aux->range_node_index[1] = (long int)range[1];
         if (!SUMA_GetDsetNodeIndexColRange_eng(dset, range, loc, 0, 2)) {
            SUMA_LH("Failed to get i1range?");
            dset->Aux->range_node_index[0] = -2; /* error */
            dset->Aux->range_node_index[1] = -2; /* error */
            SUMA_RETURN(NOPE);
         }
         if (dset->Aux->range_node_index[0] > (long int)range[0]) 
            dset->Aux->range_node_index[0] = (long int)range[0];
         if (dset->Aux->range_node_index[1] < (long int)range[1]) 
            dset->Aux->range_node_index[1] = (long int)range[1];
         
         /* get the unique set of nodes/points of graph */
         { int *ii=NULL, N_iu=-1, *iu=NULL;
         ii  = SDSET_EDGE_P1_INDEX_COL(dset);
         iu = UniqueInt(ii, SDSET_VECLEN(dset), &N_iu, 0 );
         
         /* catenate second column to unique of first */
         ii  = SDSET_EDGE_P2_INDEX_COL(dset);
         iu = (int *)realloc(iu, (SDSET_VECLEN(dset)+N_iu)*sizeof(int));
         memcpy(iu+N_iu, ii, SDSET_VECLEN(dset)*sizeof(int));
         
         /* find unique of both */
         ii = iu;
         iu = UniqueInt(ii, (SDSET_VECLEN(dset)+N_iu), &N_iu, 0); 
         free(ii);
         free(iu);
         dset->Aux->N_seg_nodes = N_iu;
         }
         
         if (!SUMA_GDSET_GetPointIndexColumn(dset, 
                                       &buff, NULL)){
            dset->Aux->N_all_nodes = dset->Aux->N_seg_nodes;
         } else dset->Aux->N_all_nodes = buff;    
         SUMA_RETURN(YUP);
         break;
   }
   SUMA_RETURN(YUP);
}

/* this function should be fast, no messing around ,
You should have a faster version, no need to bother with HEEHAW, that
should be done before this first call also, no need to check on *i2
if the index map is properly formed ... 

\param dset (SUMA_DSET *) a graph dset
\param si (int) a segment/edge's index
\param i1 (int *)to contain the index of the 1st node forming the edge
\param i2 (int *)to contain the index of the 2nd node forming the edge
\param row (int *) to contain the row in 'inel' where si was listed.
                   This parameter is only set for sparse matrices 
\sa SUMA_GDSET_SegRowToPoints
*/
byte SUMA_GDSET_SegIndexToPoints(SUMA_DSET *dset, int si, 
                                 int *i1, int *i2, int *row)
{
   static char FuncName[]={"SUMA_GDSET_SegIndexToPoints"};
   if (si < 0) return(0);
   *i1 = -1;
   switch (dset->Aux->matrix_shape) {
      case MAT_FULL:
         if (si > dset->Aux->matrix_max_index) return(0);
         *i2 = si / dset->Aux->matrix_size[0];
         if (*i2 >= dset->Aux->matrix_size[0]) return(0);
         *i1 = si % dset->Aux->matrix_size[0]; return(1);
         break;
      case MAT_TRI:
         if (si > dset->Aux->matrix_max_index) return(0);
         SUMA_CItri_p2ij(si, dset->Aux->matrix_size[0], 
                         dset->Aux->matrix_2M, 0, i1, i2);
         return(1);
         break;
      case MAT_TRI_DIAG:
         SUMA_CItri_p2ij(si, dset->Aux->matrix_size[0], 
                         dset->Aux->matrix_2M, 1, i1, i2);
         return(1);
         break;
      case MAT_SPARSE: { int r, *rp;
         /* Use index to figure out row */
         if ((r = SUMA_GetNodeRow_FromNodeIndex_eng(dset, si, -1))> -1) {
            rp = (int*)(dset->inel->vec[1]);
            *i1 = rp[r];
            rp = (int*)(dset->inel->vec[2]);
            *i2 = rp[r];
            if (row) *row = r;
            /* SUMA_S_Notev("Edge index %d, row %d, [%d %d]\n", 
                            si, r, *i1, *i2); */
         }
         return(1);
         break; }
   }
      
   return(0);
}

/*! Go from segment row to points (and index)
\sa SUMA_GDSET_SegIndexToPoints
*/
byte SUMA_GDSET_SegRowToPoints(SUMA_DSET *dset, int ri, 
                                 int *i1, int *i2, int *index)
{
   static char FuncName[]={"SUMA_GDSET_SegRowToPoints"};
   if (ri < 0) return(0);
   *i1 = -1;
   switch (dset->Aux->matrix_shape) {
      case MAT_FULL:
         if (ri > dset->Aux->matrix_max_index) return(0);
         *i2 = ri / dset->Aux->matrix_size[0];
         if (*i2 >= dset->Aux->matrix_size[0]) return(0);
         *i1 = ri % dset->Aux->matrix_size[0]; return(1);
         break;
      case MAT_TRI:
         if (ri > dset->Aux->matrix_max_index) return(0);
         SUMA_CItri_p2ij(ri, dset->Aux->matrix_size[0], 
                         dset->Aux->matrix_2M, 0, i1, i2);
         return(1);
         break;
      case MAT_TRI_DIAG:
         SUMA_CItri_p2ij(ri, dset->Aux->matrix_size[0], 
                         dset->Aux->matrix_2M, 1, i1, i2);
         return(1);
         break;
      case MAT_SPARSE: { int r, *rp;
         /* Use index to figure out row */
            rp = (int*)(dset->inel->vec[1]);
            *i1 = rp[ri];
            rp = (int*)(dset->inel->vec[2]);
            *i2 = rp[ri];
            if (index) {
               rp = (int*)(dset->inel->vec[0]);
               *index = rp[ri];
            }
            /* SUMA_S_Notev("Edge row %d, index %d, row %d, [%d %d]\n", 
                            ri, index ? *index:-1, *i1, *i2); */
         return(1);
         break; }
   }
      
   return(0);
}

/* \sa SUMA_GDSET_PointsToSegRow */
byte SUMA_GDSET_PointsToSegIndex(SUMA_DSET *dset, int i1, int i2, int *si)
{
   static char FuncName[]={"SUMA_GDSET_PointsToSegIndex"};
   
   *si = -1;
   if (i1 < 0 || i2 < 0) return(0);
   switch (dset->Aux->matrix_shape) {
      case MAT_FULL:
         if (i1 >= dset->Aux->matrix_size[0] ||
             i2 >= dset->Aux->matrix_size[0]) return(0);
         *si = i1+i2*dset->Aux->matrix_size[0]; return(1);
         break;
      case MAT_TRI:
         if (i1 >= dset->Aux->matrix_size[0] ||
             i2 >= dset->Aux->matrix_size[0]) return(0);
         *si = SUMA_CItri_ij2p(i1,i2,dset->Aux->matrix_2M);
         return(1);
         break;
      case MAT_TRI_DIAG:
         if (i1 >= dset->Aux->matrix_size[0] ||
             i2 >= dset->Aux->matrix_size[0]) return(0);
         *si = SUMA_CItri_ij2p_diag(i1,i2,dset->Aux->matrix_2M);
         return(1);
         break;
      case MAT_SPARSE: { int is, *p1, *p2, *p0;
         /* This will be a slow slog unless I create a points to edge list */
         p1 = (int*)(dset->inel->vec[1]);
         p2 = (int*)(dset->inel->vec[2]);
         p0 = (int*)(dset->inel->vec[0]);
         for (is=0; is < dset->inel->vec_len; ++is) {
            if (p1[is]==i1 && p2[is]==i2) { *si = p0[is]; return(1); }
         }
         break; }
   }
      
   return(0);
   
}

byte SUMA_GDSET_PointToDiagSegIndex(SUMA_DSET *dset, int i1, int *si)
{
   static char FuncName[]={"SUMA_GDSET_PointToDiagSegIndex"};
   
   *si = -1;
   if (i1 < 0) return(0);
   switch (dset->Aux->matrix_shape) {
      case MAT_FULL:
         if (i1 >= dset->Aux->matrix_size[0]) return(0);
         *si = i1+i1*dset->Aux->matrix_size[0]; return(1);
         break;
      case MAT_TRI:
         /* No diags elems here */
         return(0);
         break;
      case MAT_TRI_DIAG:
         if (i1 >= dset->Aux->matrix_size[0]) return(0);
         *si = SUMA_CItri_ij2p_diag(i1,i1,dset->Aux->matrix_2M);
         return(1);
         break;
      case MAT_SPARSE: { int is, *p1, *p2, *p0;
         /* This will be a slow slog unless I create a points to edge list */
         p1 = (int*)(dset->inel->vec[1]);
         p2 = (int*)(dset->inel->vec[2]);
         p0 = (int*)(dset->inel->vec[0]);
         for (is=0; is < dset->inel->vec_len; ++is) {
            if (p1[is]==i1 && p2[is]==i1) { *si = p0[is]; return(1); }
         }
         break; }
   }
      
   return(0);
   
}

byte SUMA_GDSET_PointToDiagSegRowIndex(SUMA_DSET *dset, int i1, int *ri, int *si)
{
   static char FuncName[]={"SUMA_GDSET_PointToDiagSegRowIndex"};
   
   *si = -1; *ri = -1;
   if (i1 < 0) return(0);
   switch (dset->Aux->matrix_shape) {
      case MAT_FULL:
         if (i1 >= dset->Aux->matrix_size[0]) return(0);
         *si = i1+i1*dset->Aux->matrix_size[0]; 
         *ri = *si; return(1);
         break;
      case MAT_TRI:
         /* No diags elems here */
         return(0);
         break;
      case MAT_TRI_DIAG:
         if (i1 >= dset->Aux->matrix_size[0]) return(0);
         *si = SUMA_CItri_ij2p_diag(i1,i1,dset->Aux->matrix_2M);
         *ri = *si;
         return(1);
         break;
      case MAT_SPARSE: { int is, *p1, *p2, *p0;
         /* This will be a slow slog unless I create a points to edge list */
         p1 = (int*)(dset->inel->vec[1]);
         p2 = (int*)(dset->inel->vec[2]);
         p0 = (int*)(dset->inel->vec[0]);
         for (is=0; is < dset->inel->vec_len; ++is) {
            if (p1[is]==i1 && p2[is]==i1) { 
               *si = p0[is]; 
               *ri = is;
               return(1); 
            }
         }
         break; }
   }
      
   return(0);
   
}

/*
\sa SUMA_GDSET_PointsToSegIndex
*/
byte SUMA_GDSET_PointsToSegRow(SUMA_DSET *dset, int i1, int i2, int *ri)
{
   static char FuncName[]={"SUMA_GDSET_PointsToSegRow"};
   
   *ri = -1;
   if (i1 < 0 || i2 < 0) return(0);
   switch (dset->Aux->matrix_shape) {
      case MAT_FULL:
         if (i1 >= dset->Aux->matrix_size[0] ||
             i2 >= dset->Aux->matrix_size[0]) return(0);
         *ri = i1+i2*dset->Aux->matrix_size[0]; return(1);
         break;
      case MAT_TRI:
         if (i1 >= dset->Aux->matrix_size[0] ||
             i2 >= dset->Aux->matrix_size[0]) return(0);
         *ri = SUMA_CItri_ij2p(i1,i2,dset->Aux->matrix_2M);
         return(1);
         break;
      case MAT_TRI_DIAG:
         if (i1 >= dset->Aux->matrix_size[0] ||
             i2 >= dset->Aux->matrix_size[0]) return(0);
         *ri = SUMA_CItri_ij2p_diag(i1,i2,dset->Aux->matrix_2M);
         return(1);
         break;
      case MAT_SPARSE: { int is, *p1, *p2;
         /* This will be a slow slog unless I create a points to edge list */
         p1 = (int*)(dset->inel->vec[1]);
         p2 = (int*)(dset->inel->vec[2]);
         for (is=0; is < dset->inel->vec_len; ++is) {
            if (p1[is]==i1 && p2[is]==i2) { *ri = is; return(1); }
         }
         break; }
   }
      
   return(0);
   
}

byte SUMA_GDSET_PointToDiagSegRow(SUMA_DSET *dset, int i1, int *ri)
{
   static char FuncName[]={"SUMA_GDSET_PointToDiagSegRow"};
   
   *ri = -1;
   if (i1 < 0) return(0);
   switch (dset->Aux->matrix_shape) {
      case MAT_FULL:
         if (i1 >= dset->Aux->matrix_size[0]) return(0);
         *ri = i1+i1*dset->Aux->matrix_size[0]; return(1);
         break;
      case MAT_TRI:
         /* No diagonal entries here */
         return(0);
         break;
      case MAT_TRI_DIAG:
         if (i1 >= dset->Aux->matrix_size[0]) return(0);
         *ri = SUMA_CItri_ij2p_diag(i1,i1,dset->Aux->matrix_2M);
         return(1);
         break;
      case MAT_SPARSE: { int is, *p1, *p2;
         /* This will be a slow slog unless I create a points to edge list */
         p1 = (int*)(dset->inel->vec[1]);
         p2 = (int*)(dset->inel->vec[2]);
         for (is=0; is < dset->inel->vec_len; ++is) {
            if (p1[is]==i1 && p2[is]==i1) { *ri = is; return(1); }
         }
         break; }
   }
      
   return(0);
   
}

/* From a node index, find the row where that node index is listed
in the node list */
int SUMA_GDSET_NodeIndex_To_Index(SUMA_DSET *dset, int node) 
{
   static char FuncName[]={"SUMA_GDSET_NodeIndex_To_Index"};
   int N_vals, *I;
   if (node < 0) return(node);
   I = SUMA_GDSET_GetPointIndexColumn(dset, &N_vals, NULL);
   if (N_vals == -2) return(-1); /* error */
   if (N_vals == -1) return(node); /* implicit list ! */
   return(SUMA_NodeIndex_To_Index(I, N_vals, node));
}

int SUMA_GDSET_Index_To_NodeIndex(SUMA_DSET *dset, int cinode)
{
   static char FuncName[]={"SUMA_GDSET_Index_To_NodeIndex"};
   int *I=NULL, N_vals;
   if (cinode < 0) return(cinode);
   if ((I=SUMA_GDSET_GetPointIndexColumn(dset, &N_vals,NULL))) {
      if (cinode < N_vals) {
         return(I[cinode]);
      } else {
         SUMA_S_Errv("Bad news, index %d exceeds array length %d...\n",
                     cinode, N_vals);
         return(-1);
      }
   } else {
      if (N_vals == -2) {
         SUMA_S_Err("Badness");
         return(-1);
      } else return(cinode);
   }
}

int SUMA_GDSET_EdgeIndex_To_Row(SUMA_DSET *dset, int ei)
{
   static char FuncName[]={"SUMA_GDSET_EdgeIndex_To_Row"};
   if (ei < 0) return(ei);
   switch (dset->Aux->matrix_shape) {
      case MAT_SPARSE: { int *p0, N=SDSET_VECLEN(dset);
         p0 = (int*)(dset->inel->vec[0]);
         return(SUMA_NodeIndex_To_Index(p0, N, ei)); 
         break; }
      default:
         return(ei);
   }
   return(ei);
}

int SUMA_GDSET_EdgeRow_To_Index(SUMA_DSET *dset, int ri)
{
   static char FuncName[]={"SUMA_GDSET_EdgeRow_To_Index"};
   if (ri < 0 || ri > SDSET_VECLEN(dset)) return(-1);
   switch (dset->Aux->matrix_shape) {
      case MAT_SPARSE: { int *p0;
         p0 = (int*)(dset->inel->vec[0]);
         return(p0[ri]);
         break; }
      default:
         return(ri);
   }
   return(ri);
}

int SUMA_GDSET_Max_Edge_Index(SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_GDSET_Max_Edge_Index"};
   int mm;
   GDSET_MAX_EDGE_INDEX(dset, mm);
   return(mm);
}

/* Get the column containing the ids of points
as they are listed in the coordinates list.

If the list is implicit, N_vals is set to -1 and you get
NULL back. 

If the xyz element is badly formed, N_vals is set to -2 as
an error flag

If there is an explicit indexing column, it is retured and 
N_vals set to the number of values in the column

If nelxyr is not null, it will contain nelxyz on return.
*/
int *SUMA_GDSET_GetPointIndexColumn(SUMA_DSET *dset, int *N_vals, 
                                    NI_element **nelxyzr)
{
   static char FuncName[]={"SUMA_GDSET_GetPointIndexColumn"};
   NI_element *nelxyz=NULL;
   int *I=NULL, iicoord=-1;
   char *cs=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!N_vals) {
      SUMA_S_Err("You cheap date! I need N_vals to be not null");
      SUMA_RETURN(NULL);
   }  
   
   *N_vals = -2; /*use as an error flag */
   if (nelxyzr) *nelxyzr = NULL;
   
   if (!(nelxyz = SUMA_FindGDsetNodeListElement(dset))) {
      /* Just fine if calling function knows what to do 
         about this */
      SUMA_LHv("Failed to find Dset %s's NodeListElement.\n", 
                SDSET_LABEL(dset));
      if (LocalHead) SUMA_DUMP_TRACE("Who dun that?");
      SUMA_RETURN(NULL);
   }
   if (nelxyzr) *nelxyzr = nelxyz;
   
   /* The search by label is overkill since I enforce 'I'
      to be the 1st column ... Oh well. */
   if (!(cs = NI_get_attribute(nelxyz,"COLMS_LABS"))) {
      SUMA_S_Err("What can I say?");
      SUMA_RETURN(NULL);
   }

   if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, 
                                          "Gnode Index"))<0) {
      SUMA_LH("Failed to find I, assuming we have a full list"); 
      *N_vals = -1;
   } else {
      I = (int *)nelxyz->vec[iicoord];
      *N_vals = nelxyz->vec_len; 
   }

   SUMA_RETURN(I);
}

char **SUMA_GDSET_GetPointNamesColumn(SUMA_DSET *dset, int *N_vals, 
                                    NI_element **nelxyzr)
{
   static char FuncName[]={"SUMA_GDSET_GetPointNamesColumn"};
   NI_element *nelxyz=NULL;
   char  **I=NULL;
   int iicoord=-1;
   char *cs=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!N_vals) {
      SUMA_S_Err("You cheap skate! I need N_vals to be not null");
      SUMA_RETURN(NULL);
   }  
   *N_vals = -2; /*use as an error flag */
   if (nelxyzr) *nelxyzr = NULL;
   
   if (!(nelxyz = SUMA_FindGDsetNodeListElement(dset))) {
      SUMA_S_Errv("Failed to find Dset %s's NodeListElement\n", 
                        SDSET_LABEL(dset));
      SUMA_RETURN(NULL);
   }
   if (nelxyzr) *nelxyzr = nelxyz;
   
   /* The search by label is overkill since I enforce 'I'
      to be the 1st column ... Oh well. */
   if (!(cs = NI_get_attribute(nelxyz,"COLMS_LABS"))) {
      SUMA_S_Err("What can I say?");
      SUMA_RETURN(NULL);
   }

   if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, 
                                          "Gnode Label"))<0) {
      SUMA_LH("Failed to find I, assuming we have a full list"); 
      *N_vals = -1;
   } else {
      I = (char **)nelxyz->vec[iicoord];
      *N_vals = nelxyz->vec_len; 
   }

   SUMA_RETURN(I);
}

int *SUMA_GDSET_GetPointGroupColumn(SUMA_DSET *dset, int *N_vals, 
                                      NI_element **nelxyzr)
{
   static char FuncName[]={"SUMA_GDSET_GetPointGroupColumn"};
   NI_element *nelxyz=NULL;
   int *I=NULL;
   int iicoord=-1;
   char *cs=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!N_vals) {
      SUMA_S_Err("You cheap skate! I need N_vals to be not null");
      SUMA_RETURN(NULL);
   }  
   *N_vals = -2; /*use as an error flag */
   if (nelxyzr) *nelxyzr = NULL;
   
   if (!(nelxyz = SUMA_FindGDsetNodeListElement(dset))) {
      SUMA_S_Errv("Failed to find Dset %s's NodeListElement\n", 
                        SDSET_LABEL(dset));
      SUMA_RETURN(NULL);
   }
   if (nelxyzr) *nelxyzr = nelxyz;
   
   /* The search by label is overkill since I enforce 'I'
      to be the 1st column ... Oh well. */
   if (!(cs = NI_get_attribute(nelxyz,"COLMS_LABS"))) {
      SUMA_S_Err("What can I say?");
      SUMA_RETURN(NULL);
   }

   if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, 
                                          "Gnode Group"))<0) {
      SUMA_LH("Failed to find I, assuming we have a full list"); 
      *N_vals = -1;
   } else {
      I = (int *)nelxyz->vec[iicoord];
      *N_vals = nelxyz->vec_len; 
   }

   SUMA_RETURN(I);
}

float *SUMA_GDSET_GetPointColumn_f(SUMA_DSET *dset, int *N_vals, 
                                      NI_element **nelxyzr, char *label)
{
   static char FuncName[]={"SUMA_GDSET_GetPointColumn_f"};
   NI_element *nelxyz=NULL;
   float *I=NULL;
   int iicoord=-1;
   char *cs=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!N_vals) {
      SUMA_S_Err("You cheap skate! I need N_vals to be not null");
      SUMA_RETURN(NULL);
   }  
   *N_vals = -2; /*use as an error flag */
   if (nelxyzr) *nelxyzr = NULL;
   
   if (!(nelxyz = SUMA_FindGDsetNodeListElement(dset))) {
      SUMA_S_Errv("Failed to find Dset %s's NodeListElement\n", 
                        SDSET_LABEL(dset));
      SUMA_RETURN(NULL);
   }
   if (nelxyzr) *nelxyzr = nelxyz;
   
   /* The search by label is overkill since I enforce 'I'
      to be the 1st column ... Oh well. */
   if (!(cs = NI_get_attribute(nelxyz,"COLMS_LABS"))) {
      SUMA_S_Err("What can I say?");
      SUMA_RETURN(NULL);
   }

   if ((iicoord=SUMA_NI_find_in_cs_string(cs, SUMA_NI_CSS, label))<0) {
      SUMA_LH("Failed to find I"); 
      *N_vals = -1;
   } else {
      I = (float *)nelxyz->vec[iicoord];
      *N_vals = nelxyz->vec_len; 
   }

   SUMA_RETURN(I);
}

/* Get the name of an edge in a graph dset
   DO FREE the result */
char *SUMA_GDSET_Edge_Label(SUMA_DSET *dset, int isel, char *pref, char *sep)
{
   static char FuncName[]={"SUMA_GDSET_Edge_Label"};
   int *inde, *ind0, *ind1, i1=0, i2=0;
   char *ans=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || isel < 0) SUMA_RETURN(NULL);
   if (!sep) sep = ",";
   
   if (isel <= SUMA_GDSET_Max_Edge_Index(dset)) {
      SDSET_EDGE_NODE_INDEX_COLS(dset, inde, ind0, ind1);
      if (!ind0 || !ind1 || !inde) {
         SUMA_LH("No explicit node idexing information");
         SUMA_RETURN(NULL);
      }
      if (inde[isel] != isel) {
         SUMA_LHv("Hard way for segment index %d: i1=%d, i2 = %d\n",
                        isel, i1, i2);         
         /* Fetch the indices of the nodes forming the edge */
         if (!SUMA_GDSET_SegIndexToPoints(dset, isel, &i1, &i2, NULL)) {
            SUMA_S_Errv("Failed to locate nodes of edge %d on dset %s\n",
                        isel, SDSET_LABEL(dset));
            SUMA_RETURN(NULL);
         }
      } else { /* the easy way */
         SUMA_LHv("Easy way: inde[%d]=%d [%d %d]\n",
                  isel, inde[isel], ind0[isel], ind1[isel]);
         i1 = ind0[isel];
         i2 = ind1[isel];
      }
      
      if (i1 < 0 || i2 < 0) SUMA_RETURN(NULL);
      
      if (!pref) {
         ans = SUMA_copy_string(SUMA_GDSET_Node_Label(dset, i1));
      } else {
         ans = SUMA_append_replace_string(pref, 
                     SUMA_GDSET_Node_Label(dset, i1),"", 0);
      }
      ans = SUMA_append_replace_string(ans, 
                        SUMA_GDSET_Node_Label(dset, i2), sep ,1);
      
      SUMA_LHv("label for edge %d [%d %d] on %s is: %s from (%s %s)\n", 
                isel, i1, i2, SDSET_LABEL(dset), ans?ans:"NULL",
             SUMA_GDSET_Node_Label(dset, i1), SUMA_GDSET_Node_Label(dset, i2));
   } else {
      SUMA_LHv("isel=%d, veclen=%d, max edge index %d\n", 
               isel, SDSET_VECLEN(dset), SUMA_GDSET_Max_Edge_Index(dset));
   }
   
   SUMA_RETURN(ans);
}

/* Get the name label for a graph dset node 
   Do not free returned string */
char *SUMA_GDSET_Node_Label(SUMA_DSET *dset, int psel)
{
   static char FuncName[]={"SUMA_GDSET_Node_Label"};
   int ii, iname; 
   char **names= NULL;
   
   SUMA_ENTRY;
   
   if (!dset || psel < 0) SUMA_RETURN(NULL);
   
   if (!(names = SUMA_GDSET_GetPointNamesColumn(dset, &ii, NULL))) {
      if (ii == -2) SUMA_S_Err("No names!!");
      SUMA_RETURN(NULL);
   }

   iname = SUMA_GDSET_NodeIndex_To_Index(dset, psel);
   if (iname >= 0) SUMA_RETURN(names[iname]);
   
   SUMA_RETURN(NULL);
}

NI_element *SUMA_FindGDsetNodeListElement(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_FindGDsetNodeListElement"};
   char *attname=NULL;
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr) { SUMA_SL_Err("NUll input "); SUMA_RETURN(NULL); }
   attname = SUMA_append_string(NEL_DSET_TYPE(dset->ngr),"_node_coordinates");
   SUMA_LHv("Seeking NODE_COORDS with %s\n", attname);
   nel = SUMA_FindNgrDataElement(dset->ngr, "NODE_COORDS", attname);
   SUMA_free(attname);
   SUMA_RETURN(nel);
}

/* Add the indices and coordinates of nodes forming graph dataset 
   Note that I may get reordered in this function */
NI_element *SUMA_AddGDsetNodeListElement(SUMA_DSET *dset, 
                                        int *I, float *X, float *Y, float *Z, 
                                        char **names, int *cln, float *cols,
                                        int N_Nodes)
{
   static char FuncName[]={"SUMA_AddGDsetNodeListElement"};
   char *attname=NULL;
   NI_element *nel=NULL;
   int *isort = NULL, dosort = 0, ii, jump=0;
   float *fv=NULL, *fvxyz=NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr) { SUMA_SL_Err("NUll input "); SUMA_RETURN(NULL); }
   if (cln && !cols) {
      SUMA_S_Err("If you specify node grouping, you must also send the colors");
      SUMA_RETURN(NULL);
   }  
   if (!SUMA_isGraphDset(dset)) {
      SUMA_SL_Err("Non graph dset");
      SUMA_RETURN(NULL);
   }
   if (dset->Aux->matrix_shape == MAT_HEEHAW) {
      if (!SUMA_GDSET_Set_Aux_matrix_shape(dset)) {
         SUMA_S_Err("Failed to set matrix shape");
         SUMA_RETURN(NULL);
      }
   }
   if (!(nel=SUMA_FindGDsetNodeListElement(dset))) {
      attname = SUMA_append_string(NEL_DSET_TYPE(dset->ngr),"_node_coordinates");
      nel = NI_new_data_element("NODE_COORDS", N_Nodes); 
      NI_set_attribute (nel, "data_type", attname); 
      SUMA_free(attname); attname = NULL;
      NI_add_to_group(dset->ngr, nel);
   }
   /* Make sure I is sorted */
   if (I) {
      if (nel->vec_num != 0) {
         SUMA_S_Err("Attempting to add I out of order");
         SUMA_RETURN(nel);
      }
      for (ii=1; ii<N_Nodes; ++ii) {
         if (I[ii-1]>I[ii]) {
            dosort = 1;
            break;
         } 
      }
      if (dosort) isort = z_dqsort (I, N_Nodes );
      if (!SUMA_AddDsetNelCol (  dset, "Gnode Index", 
                                    SUMA_GNODE_INDEX, (void *)I, NULL, 1)) {
            SUMA_SL_Err("Failed to add column");
            SUMA_RETURN(nel);  
       }
   } else {
      /* I want it, keep things clear and simple */
      if (nel->vec_num != 0) {
         SUMA_S_Err("Attempting to add I out of order");
         SUMA_RETURN(nel);
      }
      I = (int *)SUMA_calloc(nel->vec_len, sizeof(int));
      for (ii=0; ii<nel->vec_len; ++ii) I[ii]=ii;
      if (!SUMA_AddDsetNelCol (  dset, "Gnode Index", 
                                    SUMA_GNODE_INDEX, (void *)I, NULL, 1)) {
            SUMA_SL_Err("Failed to add column");
            SUMA_RETURN(nel);  
       }
       SUMA_ifree(I);
   }
   
   jump = 1;
   if (X && !Y && !Z) {  /* a trick to specify X Y Z with triplets */
      jump = 3;
      fvxyz  = NULL;
      if (isort) {
         fvxyz  = SUMA_freorder_triplets(X, isort, nel->vec_len);
         X = fvxyz;
         isort = 0; /* sorting now done */
      }  
      Y = X + 1;
      Z = X + 2;
   }
    
   if (X) {
       if (nel->vec_num != 1) {
         SUMA_S_Err("Attempting to add X out of order");
         SUMA_RETURN(nel);
       }
       if (isort) {
         fv = SUMA_freorder(X, isort, nel->vec_len);
         if (!SUMA_AddDsetNelCol (  dset, "Gnode X", 
                                       SUMA_NODE_X, (void *)fv, NULL, jump)) {
               SUMA_SL_Err("Failed to add column");
               SUMA_RETURN(nel);  
          }
          SUMA_ifree(fv);
       } else {
          if (!SUMA_AddDsetNelCol (  dset, "Gnode X", 
                                       SUMA_NODE_X, (void *)X, NULL, jump)) {
               SUMA_SL_Err("Failed to add column");
               SUMA_RETURN(nel);  
          }
       }
   }
   if (Y) {
       if (nel->vec_num != 2) {
         SUMA_S_Err("Attempting to add Y out of order");
         SUMA_RETURN(nel);
       }
       if (isort) {
         fv = SUMA_freorder(Y, isort, nel->vec_len);
         if (!SUMA_AddDsetNelCol (  dset, "Gnode Y", 
                                       SUMA_NODE_Y, (void *)fv, NULL, jump)) {
               SUMA_SL_Err("Failed to add column");
               SUMA_RETURN(nel);  
          }
          SUMA_ifree(fv);
       } else {
          if (!SUMA_AddDsetNelCol (  dset, "Gnode Y", 
                                       SUMA_NODE_Y, (void *)Y, NULL, jump)) {
               SUMA_SL_Err("Failed to add column");
               SUMA_RETURN(nel);  
          }
       }
   }
   if (Z) {
       if (nel->vec_num != 3) {
         SUMA_S_Err("Attempting to add Z out of order");
         SUMA_RETURN(nel);
       }
       if (isort) {
         fv = SUMA_freorder(Z, isort, nel->vec_len);
         if (!SUMA_AddDsetNelCol (  dset, "Gnode Z", 
                                       SUMA_NODE_Z, (void *)fv, NULL, jump)) {
               SUMA_SL_Err("Failed to add column");
               SUMA_RETURN(nel);  
          }
          SUMA_ifree(fv);
       } else {
          if (!SUMA_AddDsetNelCol (  dset, "Gnode Z", 
                                       SUMA_NODE_Z, (void *)Z, NULL, jump)) {
               SUMA_SL_Err("Failed to add column");
               SUMA_RETURN(nel);  
          }
       }
   }
   
   if (!names) {/* Add dummy names */
      /* get I back */
      if ((ii=SUMA_NI_find_in_cs_string(
            NI_get_attribute(nel,"COLMS_LABS"), SUMA_NI_CSS, "Gnode Index"))<0) {
         SUMA_LH("Failed to find I?!"); 
      } else {   
         I = (int *)nel->vec[ii];
         names = (char **)SUMA_calloc(nel->vec_len, sizeof(char*));
         for (ii=0; ii<nel->vec_len; ++ii) {
            names[ii] = (char *)SUMA_calloc(20, sizeof(char));
            sprintf(names[ii],"%d", I[ii]);
         }
      }
      SUMA_LH("Adding default gnode labels");
      if (!SUMA_AddDsetNelCol (  dset, "Gnode Label", 
                                 SUMA_NODE_SLABEL, (void *)names, NULL, 1)) {
         SUMA_SL_Err("Failed to add names column");
         SUMA_RETURN(nel);  
      }
      /* Left here for the lesson:
         Do not free names[ii] after adding names as a column   
                  for (ii=0; ii<nel->vec_len; ++ii) SUMA_ifree(names[ii]); */ 
      SUMA_ifree(names);
   } else {  /* Caller's names */
      SUMA_LH("Adding gnode labels (%s ... %s)", 
               names[0], names[nel->vec_len-1]);
      if (!SUMA_AddDsetNelCol (  dset, "Gnode Label", 
                                 SUMA_NODE_SLABEL, (void *)names, NULL, 1)) {
         SUMA_SL_Err("Failed to add names column");
         SUMA_RETURN(nel);  
      }
   }
   
   
   if (cln) { /* Add group belonging */
      SUMA_LH("Adding gnode groups");
      if (!SUMA_AddDsetNelCol (dset, "Gnode Group", 
                               SUMA_GNODE_IGROUP, (void *)cln,NULL, 1)) {
         SUMA_SL_Err("Failed to add group column");
         SUMA_RETURN(nel);  
      }
      SUMA_LH("Adding gnode group cols R %f..%f, G %f..%f, B %f..%f",
            cols[0], cols[3*(nel->vec_len-1)],
            cols[1], cols[3*(nel->vec_len-1)+1],
            cols[2], cols[3*(nel->vec_len-1)+2]);
      if (!SUMA_AddDsetNelCol (dset, "Gnode R", 
                            SUMA_NODE_R, (void *)cols, NULL, 3)) {
         SUMA_SL_Err("Failed to add group column");
         SUMA_RETURN(nel);  
      }
      if (!SUMA_AddDsetNelCol (dset, "Gnode G", 
                            SUMA_NODE_G, (void *)(cols+1), NULL, 3)) {
         SUMA_SL_Err("Failed to add group column");
         SUMA_RETURN(nel);  
      }
      if (!SUMA_AddDsetNelCol (dset, "Gnode B", 
                            SUMA_NODE_B, (void *)(cols+2), NULL, 3)) {
         SUMA_SL_Err("Failed to add group column");
         SUMA_RETURN(nel);  
      }
   }
   SUMA_ifree(fvxyz);
   SUMA_ifree(isort);
   
   SUMA_RETURN(nel);  
}

SUMA_DO_Types SUMA_ObjectTypeName2ObjectTypeCode(char *cc)
{
   static char FuncName[]={"SUMA_ObjectTypeName2ObjectTypeCode"};
   
   if (!cc) SUMA_RETURN(NOT_SET_type);
   if (!strcmp(cc,"WhatTheWhat")) SUMA_RETURN(NOT_SET_type);
   if (!strcmp(cc,"NOT_SET_type")) SUMA_RETURN(NOT_SET_type);
   if (!strcmp(cc,"not_DO")) SUMA_RETURN(not_DO_type);
   if (!strcmp(cc,"Surface")) SUMA_RETURN(SO_type);
   if (!strcmp(cc,"Axis")) SUMA_RETURN(AO_type);
   if (!strcmp(cc,"ROI_drawn")) SUMA_RETURN(ROIdO_type);
   if (!strcmp(cc,"ROI")) SUMA_RETURN(ROIO_type);
   if (!strcmp(cc,"GO")) SUMA_RETURN(GO_type);
   if (!strcmp(cc,"Line_Segment")) SUMA_RETURN(LS_type);
   if (!strcmp(cc,"Node_Based_Line_Segment")) SUMA_RETURN(NBLS_type);
   if (!strcmp(cc,"Oriented_Line_Segment")) SUMA_RETURN(OLS_type);
   if (!strcmp(cc,"Oriented_Direction")) SUMA_RETURN(ODIR_type);
   if (!strcmp(cc,"Direction")) SUMA_RETURN(DIR_type);
   if (!strcmp(cc,"Point")) SUMA_RETURN(PNT_type);
   if (!strcmp(cc,"Oriented_Node_Based_Line_Segment")) SUMA_RETURN(NBOLS_type);
   if (!strcmp(cc,"Node_Based_Vector")) SUMA_RETURN(NBV_type);
   if (!strcmp(cc,"Oriented_Node_Based_Vector")) SUMA_RETURN(ONBV_type);
   if (!strcmp(cc,"Sphere")) SUMA_RETURN(SP_type);
   if (!strcmp(cc,"Node_Based_Sphere")) SUMA_RETURN(NBSP_type);
   if (!strcmp(cc,"Plane")) SUMA_RETURN(PL_type);
   if (!strcmp(cc,"VO")) SUMA_RETURN(VO_type);
   if (!strcmp(cc,"NBT")) SUMA_RETURN(NBT_type);
   if (!strcmp(cc,"SBT")) SUMA_RETURN(SBT_type);
   if (!strcmp(cc,"GDSET")) SUMA_RETURN(GDSET_type);
   if (!strcmp(cc,"CDOM")) SUMA_RETURN(CDOM_type);
   if (!strcmp(cc,"ANY_DSET")) SUMA_RETURN(ANY_DSET_type);
   if (!strcmp(cc,"MD_DSET")) SUMA_RETURN(MD_DSET_type);
   if (!strcmp(cc,"DBT")) SUMA_RETURN(DBT_type);
   if (!strcmp(cc,"NIDO")) SUMA_RETURN(NIDO_type);
   if (!strcmp(cc,"TRACT")) SUMA_RETURN(TRACT_type);
   if (!strcmp(cc,"MASK")) SUMA_RETURN(MASK_type);
   if (!strcmp(cc,"GRAPH_LINK")) SUMA_RETURN(GRAPH_LINK_type);
   if (!strcmp(cc,"Number_Of_DO_Types")) SUMA_RETURN(N_DO_TYPES);
}

const char *SUMA_ObjectTypeCode2ObjectTypeName(SUMA_DO_Types dd) 
{
   static char FuncName[]={"SUMA_ObjectTypeCode2ObjectTypeName"};
   switch (dd) {
      case NOT_SET_type:
         return("NOT_SET_type");
         break;
      case not_DO_type:
         return("not_DO");
         break;
      case SO_type:
         return("Surface");
         break;
      case AO_type:
         return("Axis");
         break;
      case ROIdO_type:
         return("ROI_drawn");
         break;
      case ROIO_type:
         return("ROI");
         break;
      case GO_type:
         return("GO");
         break;
      case LS_type:
         return("Line_Segment");
         break;
      case NBLS_type:
         return("Node_Based_Line_Segment");
         break;
      case OLS_type:
         return("Oriented_Line_Segment");
         break;
      case ODIR_type:
         return("Oriented_Direction");
         break;
      case DIR_type:
         return("Direction");
         break;
      case PNT_type:
         return("Point");
         break;
      case NBOLS_type:
         return("Oriented_Node_Based_Line_Segment");
         break;
      case NBV_type:
         return("Node_Based_Vector");
         break;
      case ONBV_type:
         return("Oriented_Node_Based_Vector");
         break;
      case SP_type:
         return("Sphere");
         break;
      case NBSP_type:
         return("Node_Based_Sphere");
         break;
      case PL_type:
         return("Plane");
         break;
      case VO_type:
         return("VO");
         break;
      case NBT_type:
         return("NBT");
         break;
      case SBT_type:
         return("SBT");
         break;
      case GDSET_type:
         return("GDSET");
         break;
      case CDOM_type:
         return("CDOM");
         break;
      case ANY_DSET_type:
         return("ANY_DSET");
         break;
      case MD_DSET_type:
         return("MD_DSET");
         break;
      case DBT_type:
         return("DBT");
         break;
      case NIDO_type:
         return("NIDO");
         break;
      case TRACT_type:
         return("TRACT");
         break;
      case MASK_type:
         return("MASK");
         break;
      case GRAPH_LINK_type:
         return("GRAPH_LINK");
         break;
      case N_DO_TYPES:
         return("Number_Of_DO_Types");
         break;
      default:
         return("WhatTheWhat!");
   }
}

/**************** Tract dset functions *************************/

/* This query goes back to the source all the time,
   use SUMA_isTractDset_fast for speed */
byte SUMA_isTractDset(SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_isTractDset"};
   
   SUMA_ENTRY;
   
   if (!dset || !dset->ngr) SUMA_RETURN(NOPE);
   
   if (!dset->Aux) { /* create one, always good */
      if (!SUMA_Add_Dset_Aux(dset)) {
         SUMA_S_Err("Bad news, this should not fail");
         SUMA_RETURN(NOPE);
      }      
   }
   if (SUMA_isTractDsetNgr(dset->ngr)) {
      dset->Aux->isGraph = TRACT_DSET;
   }
   
   SUMA_RETURN(dset->Aux->isGraph == TRACT_DSET);
}

byte SUMA_isTractDsetNgr(NI_group *ngr)
{
   if (!ngr) return(0);
   switch (SUMA_Dset_Type(NEL_DSET_TYPE(ngr))) {
      case SUMA_TRACT_BUCKET:
         return(1);
      default:
         return(0);
   }
   return(0);
}

/**************** CIFTI dset functions *************************/
byte SUMA_isMD_Dset(SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_isMD_Dset"};
   
   SUMA_ENTRY;
   
   if (!dset || !dset->Aux) SUMA_RETURN(NOPE);
   
   SUMA_RETURN(dset->Aux->isGraph == MD_DSET);  /* hopefully  16 Sep 2015 */
}

/* This query goes back to the source all the time,
   use SUMA_isCIFTIDset_fast for speed 
   Logic might need revisiting at some point */
byte SUMA_isCIFTIDset(SUMA_DSET *dset) 
{
   static char FuncName[]={"SUMA_isCIFTIDset"};
   
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(NOPE);
   
   if (!dset->Aux) { /* create one, always good */
      if (!SUMA_Add_Dset_Aux(dset)) {
         SUMA_S_Err("Bad news, this should not fail");
         SUMA_RETURN(NOPE);
      }      
   }
   
   if (dset->Aux->N_doms > 0) { /* no need to to further 
      	             	       with CIFTI dataset that
			       are elementarized, there is
			       no more ngr, etc.*/
      dset->Aux->isGraph = CIFTI_DSET;
      SUMA_RETURN(YUP);
   }
   
   if (dset->ngr && SUMA_isCIFTIDsetNgr(dset->ngr)) {
      dset->Aux->isGraph = CIFTI_DSET;
   }
   
   SUMA_RETURN(dset->Aux->isGraph == CIFTI_DSET);
}

byte SUMA_isCIFTIDsetNgr(NI_group *ngr)
{
   if (!ngr) return(0);
   switch (SUMA_Dset_Type(NEL_DSET_TYPE(ngr))) {
      case SUMA_CIFTI_BUCKET:
         return(1);
      default:
         return(0);
   }
   return(0);
}

SUMA_Boolean SUMA_CIFTI_Set_Domains(SUMA_DSET *dset, int N_doms, 
                                    int *dind, int *dindoff, int *dn,
                                    SUMA_DO_Types *dtp, char **dsrcs)
{
   static char FuncName[]={"SUMA_CIFTI_Set_Domains"};
   char *str = NULL, buff[500]={""};
   int i, k, min, max, imin, imax, sorted, N, *ind=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !dset->inel) {
      SUMA_S_Err("dset or dset->inel is NULL");
      SUMA_RETURN(NOPE);
   }
   
   NI_SET_INT (dset->inel, "N_Domains", N_doms);
   NI_SET_INTv(dset->inel, "Index_Offsets", dindoff, N_doms+1);
   NI_SET_INTv(dset->inel, "Domain_N_Data", dn, N_doms); /* total number of 
                                    nodes, voxels, etc possible in domain.*/
   str = NULL;
   for (i=0; i<N_doms; ++i) {
      str = SUMA_ar_string(
               str, (char *)SUMA_ObjectTypeCode2ObjectTypeName(dtp[i]),";",1);
   }
   NI_SET_STR(dset->inel, "Model_Types", str); SUMA_ifree(str);
   
   
   /* check the indices and set the ranges, location of min and max for
      each domain is relative to the full list */
   str = NULL;
   sorted = 1;
   for (i=0; i<N_doms; ++i) {
      ind = dind+dindoff[i]; N=dindoff[i+1]-dindoff[i];
      min = max = ind[0];
      for (k=0; k<N; ++k) {
         if (sorted && k<N-1 && ind[k] >= ind[k+1]) sorted = 0;
         if (ind[k] < min) min = ind[k]; imin = k+dindoff[i];
         if (ind[k] > max) max = ind[k]; imax = k+dindoff[i];
      }
      snprintf(buff, 500*sizeof(char),"%d %d %d %d", 
                     min, max, imin, imax);
      str = SUMA_ar_string(str, buff, SUMA_NI_CSS, 1);
   }
   NI_set_attribute(dset->inel, "COLMS_RANGE", str); SUMA_ifree(str); 
   
   /* domain sources */
   str = NULL;
   for (i=0; i<N_doms; ++i) {
      str = SUMA_ar_string(str, dsrcs[i],";",1);
   }
   NI_SET_STR(dset->inel, "Domain_Sources", str); SUMA_ifree(str);
   
   /* Set the sorted flag */
   if (sorted) {
      NI_set_attribute(dset->inel, "sorted_node_def", "Yes");
   } else {
      NI_set_attribute(dset->inel, "sorted_node_def", "No");
   }
   
   /* You have yet to put in the indices */
   if (!SDSET_NODEINDNUM(dset)) {
      NI_add_column_stride ( dset->inel, NI_INT, dind, 1);
   } else {
      SUMA_LH("Have inel col of %d vals", dset->inel->vec_len);
      ind = (int *)(dset->inel->vec[0]);
      for (i=0; i<SDSET_VECLEN(dset); ++i) {
         ind[i] = dind[i];
      }
   }
   
   SUMA_RETURN(NOPE);
}

SUMA_Boolean SUMA_CIFTI_Free_Doms(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_CIFTI_Free_Doms"};
   int i;
   SUMA_DSET *edset = NULL;
   
   if (!dset || !dset->Aux) return(NOPE);
   
   if (dset->Aux->doms && dset->Aux->N_doms > 0) {
      for (i=0; i<dset->Aux->N_doms; ++i) {
         if (dset->Aux->doms[i]) {
            SUMA_ifree(dset->Aux->doms[i]->edset_id);
	    SUMA_ifree(dset->Aux->doms[i]->Source);
            SUMA_ifree(dset->Aux->doms[i]);
         }
      }
      SUMA_ifree(dset->Aux->doms);
   }
   dset->Aux->N_doms = -1; dset->Aux->doms = NULL;
   
   return(YUP);
}

/* Take dset->ngr->inel domain information and write them into
   C-struct fields 
   \sa  SUMA_CIFTI_NgrFromDomains */
SUMA_Boolean SUMA_CIFTI_DomainsFromNgr(SUMA_DSET *dset, DList *DsetList, 
      	             	      	       int allowreplace, SUMA_DSET **ret_edset)
{
   static char FuncName[]={"SUMA_CIFTI_DomainsFromNgr"};
   double nums[4];
   int i, k, ibuff[51], jbuff[51];
   char *mtstr=NULL, *rnstr=NULL, *ss=NULL, *dsstr=NULL;
   SUMA_DSET *edset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMA_isCIFTIDset(dset) || !dset->Aux || !dset->inel) {
      SUMA_S_Err("I'm calling my lawyer");
      SUMA_RETURN(NOPE);
   }
   if (dset->Aux->doms) {
      SUMA_S_Note("Have doms already, freeing them but you might not like it"
      	          "if reloading a CIFTI in SUMA...");
      SUMA_CIFTI_Free_Doms(dset);
   }
   if (!DsetList && !ret_edset) {
      SUMA_S_Err("No way to preserve your edset!");
      SUMA_RETURN(NOPE);
   }
   if (ret_edset && *ret_edset) {
      SUMA_S_Err("*ret_edset must be NULL");
      SUMA_RETURN(NOPE);
   }
   
   NI_GET_INT(dset->inel, "N_Domains", dset->Aux->N_doms);
   if (dset->Aux->N_doms > 50) {
      SUMA_S_Err("No setup to deal with so many doms. Fix me");
      dset->Aux->N_doms = -1;
      SUMA_RETURN(NOPE);
   }
   SUMA_LH("%d Domains: %s", dset->Aux->N_doms,
      	    CNS(NI_get_attribute(dset->inel,"Index_Offsets")));
   NI_GET_INTv(dset->inel,"Index_Offsets", ibuff, dset->Aux->N_doms+1, 0);
   NI_GET_INTv(dset->inel,"Domain_N_Data", jbuff, dset->Aux->N_doms, 0);
      
   SUMA_LH("Getting strings");
   NI_GET_STR_CP(dset->inel, "Model_Types", mtstr);
   NI_GET_STR_CP(dset->inel, "Domain_Sources", dsstr);
   NI_GET_STR_CP(dset->inel, "COLMS_RANGE", rnstr);
   if (!mtstr || !rnstr || !dsstr) {
      SUMA_S_Err("Malformation suspected");
      SUMA_RETURN(NOPE);
   }

   SUMA_LH("Creating doms");
   dset->Aux->doms = (SUMA_DSET_DOMAIN **)SUMA_calloc(
                              dset->Aux->N_doms, sizeof(SUMA_DSET_DOMAIN *));
   if ((i = SUMA_StringToNum(rnstr, (void *)nums, 4, 2)) != 
                                                         4*dset->Aux->N_doms) {
      SUMA_SL_Err("Failed to read %d nums from range string %s. Got %d", 
                  4*dset->Aux->N_doms, rnstr, i);
      for (i=0; i<4*dset->Aux->N_doms; ++i) nums[i]=-1; 
   }
   for (i=0; i<dset->Aux->N_doms; ++i) {
      dset->Aux->doms[i] = (SUMA_DSET_DOMAIN *)
                                 SUMA_calloc(1,sizeof(SUMA_DSET_DOMAIN));
      dset->Aux->doms[i]->IndexOffset = ibuff[i];
      dset->Aux->doms[i]->IndexCount  = ibuff[i+1]-ibuff[i];
      dset->Aux->doms[i]->Max_N_Data  = jbuff[i];
      ss = SUMA_Get_Sub_String(mtstr,SUMA_NI_CSS, i);
      dset->Aux->doms[i]->ModelType   = SUMA_ObjectTypeName2ObjectTypeCode(ss);
      SUMA_ifree(ss);
      ss = SUMA_Get_Sub_String(dsstr,SUMA_NI_CSS, i);
      dset->Aux->doms[i]->Source = SUMA_copy_string(ss);
      SUMA_ifree(ss);
      for (k=0; k<4; ++k) dset->Aux->doms[i]->Range[k] = nums[4*i+k]; 
      /* aaaand the elementary dset */
      edset = SUMA_CIFTI_2_edset(dset, i, NULL, DsetList, allowreplace);
      dset->Aux->doms[i]->edset_id = SUMA_copy_string(SDSET_ID(edset)); 
      if (ret_edset) *ret_edset = edset;
      edset = NULL; 
   }
   
   /* just empty the contents of dnel and inel to save space
      but keep the metadata to keep dataset somewhat viable,
      as we will be inserting the multi domain dataset into the
      dataset list */
   SUMA_CIFTI_free_MD_data(dset);

   SUMA_RETURN(YUP);
}

/* A function to free the bulk of data in a CIFTI multi domain
dataset. Do this to save memory, after having created the elementary
datasets */
SUMA_Boolean SUMA_CIFTI_free_MD_data(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_CIFTI_free_MD_data"};
   int i, k;
   NI_element *nel=NULL;
   
   SUMA_ENTRY;
   
   /* assuming the bulk of content is in dnel, inel, etc */
   for (k=0; k<4; ++k) {
           if (k==0) nel = dset->dnel;
      else if (k==1) nel = dset->inel;
      else if (k==2) nel = dset->pdnel;
      else if (k==3) nel = dset->pinel;
      if (nel) {
      	 for (i=0; i<nel->vec_num; ++i) {
      	    NI_remove_column(nel,-1); 
      	 }
      }
   }
   
   SUMA_RETURN(YUP);
}

/* Create a single domain (elementary) dataset from a multi domain (CIFTI) dataset.
Will need something to go the other way around, from a collection of 
elementary dataset back to the MD dataset */
SUMA_DSET *SUMA_CIFTI_2_edset(SUMA_DSET *dset, int i, byte *colmask, 
      	             	      DList *DsetList, int allowreplace)
{
   static char FuncName[]={"SUMA_CIFTI_2_edset"};
   SUMA_DSET *edset = NULL;
   byte *rowmask = NULL;
   char *lbl=NULL, sbuf[64]={""};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!dset || !dset->Aux || !dset->Aux->doms || 
       dset->Aux->N_doms <= i) {
      SUMA_S_Err("Bad input to  SUMA_CIFTI_2_edset");
      SUMA_RETURN(edset);  
   }
   if (!(rowmask = (byte *)SUMA_calloc(SDSET_VECLEN(dset), sizeof(byte)))) {
      SUMA_S_Err("Failed to create rowmask"); 
      SUMA_RETURN(edset);
   }
   SUMA_LH("Setting mask for %d rows at offset %d",
      	   dset->Aux->doms[i]->IndexCount, dset->Aux->doms[i]->IndexOffset);
   memset(rowmask+dset->Aux->doms[i]->IndexOffset, 1, 
      	  dset->Aux->doms[i]->IndexCount*sizeof(byte));
      
   if (!(edset = SUMA_MaskedCopyofDset( dset, rowmask, colmask, 1, 1))) {
      SUMA_S_Err("Failed to extract edset");
   } else {
      /* Label in peculiar fashion */
      sprintf(sbuf, "ED%02d:", i);
      /* change filename */
      lbl = SUMA_ModifyName(SUMA_sdset_filename(dset), "prepend", sbuf, NULL);
      SUMA_LH("Filename now %s", lbl);
      NI_set_attribute(edset->ngr, "filename", lbl); 
      if (LocalHead) {
      	 SUMA_LH("(not) Writing out elementary dset %s for debugging", lbl);
         /* Use the 'no suma' version.  14 Aug 2015 [rickr] */
      	 SUMA_WriteDset_ns(lbl, edset, SUMA_ASCII_NIML, 1,1);
      }
      SUMA_ifree(lbl);
      /* recreate the ID to reflect the filename */
      lbl = UNIQ_hashcode(NI_get_attribute(edset->ngr, "filename")); 
      NI_set_attribute(edset->ngr, "self_idcode", lbl); SUMA_ifree(lbl);
      
      /* and cute label */
      lbl = SUMA_ar_string(sbuf, SUMA_sdset_label(dset),"",0);
      SUMA_LabelDset(edset, lbl); SUMA_ifree(lbl);
      
      /* Put a reference to the multi-domain source */
      NI_set_attribute(edset->ngr,"MD_parent_ID", SDSET_ID(dset));
      NI_set_attribute(edset->ngr,"MD_parent_label", SDSET_LABEL(dset));
      NI_SET_INT(edset->ngr,"MD_parent_subdomain_index", i);
   }
   SUMA_ifree(rowmask);
   
   SUMA_LH("Elementary dset (%s, id %s) has pointer %p \n", 
            SDSET_LABEL(edset), SDSET_ID(edset), edset); 
   
   if (DsetList) {
      if (!SUMA_InsertDsetPointer(  &edset, DsetList, allowreplace)) {
	 SUMA_SLP_Err("Failed to add new dset to list");
	 /* is there not a function to replace a dset yet? */
	 SUMA_FreeDset(edset); edset = NULL;
	 SUMA_RETURN(NULL);
      }
      SUMA_LH("Now dset (%s, id %s) is  pointer %p\n", 
      	       SDSET_LABEL(edset), SDSET_ID(edset), edset); 
   }
   
   SUMA_RETURN(edset);
}

/* Take C-struct domain information and write them into
   dset->ngr->inel element
   \sa SUMA_CIFTI_DomainsFromNgr */
SUMA_Boolean SUMA_CIFTI_NgrFromDomains(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_CIFTI_NgrFromDomains"};
   int dindoff[51], dn[51], i;
   char *dsrcs[51];
   SUMA_DO_Types dtp[51];
    
   if (!SUMA_isCIFTIDset(dset) || !dset->Aux || !dset->Aux->doms) {
      SUMA_S_Err("I'm calling my mom!");
      SUMA_RETURN(NOPE);
   }
   SUMA_S_Warn("Function not ready to take elementary datasets and recreate the"
      	       " multidomain version. See SUMA_CIFTI_2_Edset() ");
   if (dset->Aux->N_doms > 50) {
      SUMA_S_Err("No setup to deal with so many doms. Fix me");
      SUMA_RETURN(NOPE);
   }
   for (i=1; i<dset->Aux->N_doms; ++i) {
      dindoff[i] = dset->Aux->doms[i]->IndexOffset;
      dn[i] = dset->Aux->doms[i]->Max_N_Data;
      dtp[i] = dset->Aux->doms[i]->ModelType;
      dsrcs[i] = dset->Aux->doms[i]->Source;
   }
   SUMA_CIFTI_Set_Domains(dset, dset->Aux->N_doms, SDSET_NODE_INDEX_COL(dset),
                          dindoff, dn, dtp, dsrcs);
   
   SUMA_RETURN(YUP);   
}

