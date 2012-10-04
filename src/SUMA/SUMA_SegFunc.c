#include "SUMA_suma.h"
#include "../avovk/thd_segtools_fNM.h"
#include "SUMA_SegOpts.h"
#include "SUMA_SegFunc.h"

#ifdef USE_OMP
#include <omp.h>
#endif

static int VN = 0;

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[VN%10] ) ;
   if( VN%10 == 9) fprintf(stderr,".") ;
   VN++ ;
}

static int debug = 0;
static int VoxDbg3[3];
static int VoxDbg = -1;
static FILE *VoxDbgOut=NULL;

char *SUMA_hist_variable(SUMA_HIST *hh) 
{
   if (!hh || !hh->label) return(NULL);
   
   return(SUMA_label_variable(hh->label, 'h'));
}
char *SUMA_dist_variable(SUMA_FEAT_DIST *hh) 
{
   if (!hh || !hh->label) return(NULL);
   
   return(SUMA_label_variable(hh->label, 'd'));
}
char *SUMA_hist_conditional(SUMA_HIST *hh) 
{
   if (!hh || !hh->label) return(NULL);
   
   return(SUMA_label_conditional(hh->label, 'h'));
}
char *SUMA_dist_conditional(SUMA_FEAT_DIST *hh) 
{
   if (!hh || !hh->label) return(NULL);
   
   return(SUMA_label_conditional(hh->label, 'd'));
}

/* 
   infer class from label, this is particular to the way you label these dudes
   Do NOT free what is returned
*/
char *SUMA_label_variable(char *label, char c) 
{
   static char feats[10][256];
   static int ii=0;
   int j,k;
   if (!label) return(NULL);
   if (label[0]!=c || label[1]!='(') return(NULL);
   ++ii; if (ii>9) ii = 0;
   feats[ii][0]='\0'; feats[ii][255]='\0';
   j=2; k=0;
   while (label[j]!='\0' && label[j]!='|' && label[j]!=')' && k<255) 
   {
      feats[ii][k]=label[j]; ++k; ++j;
   }
   feats[ii][k]='\0';
   return(feats[ii]);
}

/* 
   infer class from label, this is particular to the way you label these dudes
   Do NOT free what is returned
*/
char *SUMA_label_conditional(char *label, char c) 
{
   static char cls[10][256];
   static int ii=0;
   int j,k;
   if (!label) return(NULL);
   if (label[0]!=c || label[1]!='(') return(NULL);
   ++ii; if (ii>9) ii = 0;
   cls[ii][0]='\0'; cls[ii][255]='\0';
   j=2; k=0;
   while (label[j]!='\0' && label[j]!='|' && label[j]!=')') 
   {
      ++j;
   }
   if (label[j]!='\0') {
      ++j;
      while (label[j]!='\0' && label[j]!=')' && k<255) 
      {
         cls[ii][k]=label[j]; ++k; ++j;
      }
      cls[ii][k]='\0';
   }
   return(cls[ii]);
}

/*
   return histogram filename for a certain variable and conditional
*/
char *SUMA_hist_fname(char *proot, char *variable, char *conditional, 
                      int withext)
{
   static char cls[10][256];
   static int ii=0;
   int j,k;
   
   if (!proot || !variable) return(NULL);
   ++ii; if (ii>9) ii = 0;
   cls[ii][0]='\0'; cls[ii][255]='\0';
   if (conditional) {
      snprintf(cls[ii], 255, "%s/h.%s-G-%s",
               proot, variable, conditional);
   } else {
      snprintf(cls[ii], 255, "%s/h.%s",
               proot, variable);
   }  
   if (withext) {
      strncat(cls[ii],".niml.hist", 255);
   }            
   return(cls[ii]);
}

char *SUMA_corrmat_fname(char *proot, char *conditional, int withext)
{
   static char cls[10][256];
   static int ii=0;
   int j,k;
   
   if (!proot || !conditional) return(NULL);
   ++ii; if (ii>9) ii = 0;
   cls[ii][0]='\0'; cls[ii][255]='\0';
   snprintf(cls[ii], 255, "%s/C.%s",
               proot, conditional);
   if (withext) {
      strncat(cls[ii],".niml.cormat", 255);
   }             
   return(cls[ii]);
}

SUMA_FEAT_DIST *SUMA_find_feature_dist(SUMA_FEAT_DISTS *FDV, 
                                       char *label, char *feature, char *class,
                                       int *ifind)
{
   static char FuncName[]={"SUMA_find_feature_dist"};
   int ff=-1;
   char sbuf[256]={""}, *skey=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!FDV || (!label && !feature) || FDV->N_FD < 1) SUMA_RETURN(NULL);
   if (ifind) *ifind = -1;
   
   if (label) {
      if (feature || class) {
         SUMA_S_Err("Can't use label with feature or class");
         SUMA_RETURN(NULL);
      }
      skey = label; 
   } else {
      if (class) 
         snprintf(sbuf, 255, "d(%s|%s)",feature, class);
      else
         snprintf(sbuf, 255, "d(%s)",feature);
      skey = sbuf;
   }
   ff = 0;
   while (ff < FDV->N_FD) {
      /* SUMA_LHv("%d: skey=%s, label=%s\n", ff, skey, FDV->FD[ff]->label);*/
      if (!strcmp(skey, FDV->FD[ff]->label)) {
         if (ifind) *ifind = ff;
         SUMA_RETURN(FDV->FD[ff]);
      }
      ++ff; 
   }
   
   SUMA_RETURN(NULL);
}

NI_str_array * SUMA_dists_featureset(SUMA_FEAT_DISTS *FDV)
{
   static char FuncName[]={"SUMA_dists_featureset"};
   NI_str_array *sar=NULL;
   int i=0;
   
   SUMA_ENTRY;
   
   if (!FDV) SUMA_RETURN(sar);
   
   for (i=0; i<FDV->N_FD; ++i) {
      sar = SUMA_NI_str_array(sar, SUMA_dist_variable(FDV->FD[i]), "A");
   } 
   
   SUMA_RETURN(sar);
}

NI_str_array * SUMA_dists_classset(SUMA_FEAT_DISTS *FDV)
{
   static char FuncName[]={"SUMA_dists_classset"};
   NI_str_array *sar=NULL;
   int i=0;
   
   SUMA_ENTRY;
   
   if (!FDV) SUMA_RETURN(sar);
   
   for (i=0; i<FDV->N_FD; ++i) {
      sar = SUMA_NI_str_array(sar, SUMA_dist_conditional(FDV->FD[i]), "A");
   } 
   
   SUMA_RETURN(sar);
}

SUMA_FEAT_DISTS *SUMA_grow_feature_dists(SUMA_FEAT_DISTS *FDV)
{
   static char FuncName[]={"SUMA_grow_feature_dists"};
   
   SUMA_ENTRY;
   
   if (!FDV) {
      FDV = (SUMA_FEAT_DISTS *)SUMA_calloc(1, sizeof(SUMA_FEAT_DISTS));
      FDV->N_FD = 0;
   }
   FDV->N_alloc += 50;
   FDV->FD = (SUMA_FEAT_DIST **)SUMA_realloc(FDV->FD, FDV->N_alloc* 
                                                      sizeof(SUMA_FEAT_DIST*));
   
   SUMA_RETURN(FDV);
}

SUMA_FEAT_DIST *SUMA_free_dist(SUMA_FEAT_DIST *FD) 
{
   static char FuncName[]={"SUMA_free_dist"};
   
   SUMA_ENTRY;
   
   if (FD) {
      if (FD->label) SUMA_free(FD->label); FD->label = NULL;
      if (FD->hh) FD->hh = SUMA_Free_hist(FD->hh);
      SUMA_free(FD);
   }
   
   SUMA_RETURN(NULL);
}

SUMA_FEAT_DISTS *SUMA_free_dists(SUMA_FEAT_DISTS *FDV) 
{
   static char FuncName[]={"SUMA_free_dists"};
   int i=0;
   
   SUMA_ENTRY;
   
   if (!FDV) SUMA_RETURN(NULL);
   for (i=0; i<FDV->N_FD; ++i) {
      if (FDV->FD[i]) FDV->FD[i] = SUMA_free_dist(FDV->FD[i]);
   }
   if (FDV->FD) SUMA_free(FDV->FD);
   
   SUMA_free(FDV);
   
   SUMA_RETURN(NULL);
}

SUMA_FEAT_DISTS *SUMA_add_feature_dist(SUMA_FEAT_DISTS *FDV, 
                                       SUMA_FEAT_DIST **FDp,
                                       int append)
{
   static char FuncName[]={"SUMA_add_feature_dist"};
   int ff=-1;
   char sbuf[256]={""};
   SUMA_FEAT_DIST *FD=NULL, *FDo=NULL;
   
   SUMA_ENTRY;
   
   if (!FDp) SUMA_RETURN(FDV);
   if (!(FD = *FDp)) {
      SUMA_RETURN(FDV);
   }
   
   if (!FD->label) {
      SUMA_S_Err("Failed to add FD, no label");
      SUMA_RETURN(FDV);
   }
   if (!FDV || FDV->N_FD>=FDV->N_alloc-1) {
      FDV = SUMA_grow_feature_dists(FDV);
   } 
   if (append) { /* no replacing */
      FDV->FD[FDV->N_FD] = FD;
      FDV->N_FD += 1;
   } else {
      FDo = SUMA_find_feature_dist(FDV, FD->label, NULL, NULL, &ff);
      if (!FDo) {
         FDV->FD[FDV->N_FD] = FD;
         FDV->N_FD += 1;
      } else {
         FDo = SUMA_free_dist(FDo);
         FDV->FD[ff]=FD;
      }
   }
   *FDp = NULL; /* to keep user from freeing by mistake */
   
   SUMA_RETURN(FDV);
}

SUMA_FEAT_DIST *SUMA_hist_To_dist(SUMA_HIST **hhp, char *thislabel)
{
   static char FuncName[]={"SUMA_hist_To_dist"};
   SUMA_FEAT_DIST *FD=NULL;
   SUMA_HIST *hh=NULL;
   char *var=NULL, *cond=NULL;
   
   SUMA_ENTRY;
   
   if (!hhp) SUMA_RETURN(FD);
   hh = *hhp;
   if (!hh->label && !thislabel) {
      SUMA_S_Err("No histogram label");
      SUMA_RETURN(FD);
   }
   FD = (SUMA_FEAT_DIST *) SUMA_calloc(1,sizeof(SUMA_FEAT_DIST));
   FD->tp = SUMA_FEAT_NP;
   FD->hh = hh; *hhp = NULL;
   if (thislabel) FD->label = SUMA_copy_string(thislabel);
   else {
      var = SUMA_hist_variable(FD->hh);
      cond = SUMA_hist_conditional(FD->hh);
      if (!cond || cond[0]=='\0') {
         FD->label = SUMA_append_replace_string("d(",")",var,0);
      } else {
         FD->label = SUMA_append_replace_string("d(","|",var,0);
         FD->label = SUMA_append_replace_string(FD->label,")",cond,1);
      }
   }
   
   SUMA_RETURN(FD);
}

SUMA_FEAT_DISTS *SUMA_TRAIN_DISTS_To_dists(SUMA_FEAT_DISTS *FDV, 
                                           NI_element *ndist)
{
   static char FuncName[]={"SUMA_TRAIN_DISTS_To_dists"};
   SUMA_FEAT_DIST *FD = NULL;
   char **clsv=NULL, **featv=NULL;
   float *shapev=NULL, *ratev=NULL;
   int i = 0;
   char *atr=NULL, atname[256]={""};
   NI_str_array *atrs=NULL;
   
   SUMA_ENTRY;
   
   if (!ndist) {
      SUMA_S_Err("NULL ndist");
      SUMA_RETURN(FDV);
   }
   if (strcmp(ndist->name,"TRAIN_DISTS")) {
      SUMA_S_Errv("nel %s is no good here sir.\n", ndist->name);
      SUMA_RETURN(FDV);
   }
   if (!(atr=NI_get_attribute(ndist,"Dist")) ||
       strcmp(atr, "gamma")) {
      SUMA_S_Err("Dunno what to do with this element");
      SUMA_RETURN(FDV);
   } else { /* have gamma */
      featv = (char **)ndist->vec[0];
      clsv  = (char **)ndist->vec[1]; 
      shapev= (float *)ndist->vec[2];
      ratev = (float *)ndist->vec[3];
      for (i=0; i<ndist->vec_len; ++i) {
         sprintf(atname,"%s_Scale+Shift", featv[i]);
         
         if (!(atr = NI_get_attribute(ndist, atname))) {
            SUMA_S_Errv("Failed to find attribute %s\n", atname);
            RETURN(FDV);
         }
         if (!(atrs = NI_decode_string_list(atr,",")) || atrs->num != 2) {
            SUMA_S_Errv("Failed to find shift+scale on %s\n",atname);
            RETURN(FDV); 
         }
         FD = (SUMA_FEAT_DIST *) SUMA_calloc(1,sizeof(SUMA_FEAT_DIST));
         FD->label = SUMA_append_replace_string("d(","|",featv[i],0);
         FD->label = SUMA_append_replace_string(FD->label,")",clsv[i],1);
         FD->tp = SUMA_FEAT_GAMMA;
         FD->scpar[0] = strtod(atrs->str[0], NULL);
         FD->scpar[1] = strtod(atrs->str[1], NULL);
         FD->par[0] = (double)shapev[i];
         FD->par[1] = (double)ratev[i];
         
         FDV = SUMA_add_feature_dist(FDV, &FD, 0);
      }
   }     
   
   SUMA_RETURN(FDV);
}

SUMA_FEAT_DISTS *SUMA_get_all_dists(char *where) 
{
   static char FuncName[]={"SUMA_get_all_dists"};
   int nfile ; 
   char **flist=NULL;
   char *wilds[]={"*.niml.hist", "*.niml.td", NULL}, 
         *wild=NULL, *allwild=NULL;
   int i;
   NI_element *nel=NULL;
   SUMA_FEAT_DISTS *FDV=NULL;
   SUMA_FEAT_DIST *FD=NULL;
   SUMA_HIST *hh=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!where) SUMA_RETURN(NULL);
   
   if (THD_is_directory(where)) {
      i = 0;
      while (wilds[i] != NULL) {
         wild = SUMA_append_replace_string(where,wilds[i],"/",0);
         allwild = SUMA_append_replace_string(allwild,wild, " ",1);
         SUMA_free(wild); wild = NULL;
         ++i;
      }
   } else {
      allwild = SUMA_copy_string(where);
   }
   if (!allwild) { SUMA_S_Err("No wildness"); SUMA_RETURN(NULL); }
    
   MCW_wildcards( allwild , &nfile , &flist ) ;
   if (nfile <=0) {
      SUMA_S_Errv("No training material under %s \n%s\n", where, allwild);
   } else {
      for (i=0; i<nfile; ++i) {
         if (SUMA_isExtension(flist[i],"niml.td")) {
            SUMA_LHv("Adding TRAIN_DISTS %s\n", flist[i]);
            nel = (NI_element*) Seg_NI_read_file(flist[i]);
            if( !nel || strcmp(nel->name,"TRAIN_DISTS")){
              SUMA_S_Warnv("can't open  %s, or bad type. Ignoring\n",
                            flist[i]) ;
            } else {
               FDV = SUMA_TRAIN_DISTS_To_dists(FDV, nel);
            }
            if (nel) NI_free_element(nel); nel = NULL;
         } else if (SUMA_isExtension(flist[i],"niml.hist")) {
            SUMA_LHv("Adding hist %s\n", flist[i]);
            hh = SUMA_read_hist(flist[i]);
            FD = SUMA_hist_To_dist(&hh,NULL);
            FDV = SUMA_add_feature_dist(FDV, &FD, 0);
         } else {
            SUMA_LHv("Unknow extension for %s, ignoring it.\n",
                  flist[i]);
         }
      }
   }
   MCW_free_wildcards( nfile , flist ) ;   
   if (allwild) { SUMA_free(allwild); allwild = NULL;}
   
   if (LocalHead) {
      SUMA_Show_dists(FDV, NULL, 1);
   }
   SUMA_RETURN(FDV);
}
  
NI_group *SUMA_hist_To_NIhist(SUMA_HIST *hh) 
{
   static char FuncName[]={"SUMA_hist_To_NIhist"};
   NI_group *ngr=NULL;
   NI_element *nel=NULL;
   char *feat=NULL, *class=NULL;
   
   SUMA_ENTRY;
   
   if (!hh) SUMA_RETURN(ngr);
   
   
   ngr = NI_new_group_element();
   NI_rename_group(ngr, hh->label?hh->label:"MrEd");
   #if 0 /* don't add this junk unless necessary */
   if ((feat = SUMA_hist_variable(hh))) {
      NI_set_attribute(ngr,"feature", feat);
   }
   if ((class = SUMA_hist_conditional(hh))) {
      NI_set_attribute(ngr,"class", class);
   }
   #endif
   nel = NI_new_data_element("seg_histogram", hh->K);
   NI_add_to_group(ngr, nel);
   NI_SET_FLOAT(nel,"window",hh->W);
   NI_SET_FLOAT(nel,"min", hh->min);
   NI_SET_FLOAT(nel,"max", hh->max);
   NI_SET_INT(nel,"N_samp", hh->n);
   NI_SET_INT(nel,"N_ignored", hh->N_ignored);
   NI_add_column(nel, NI_FLOAT, hh->b);
   NI_add_column(nel, NI_INT, hh->c);
   NI_add_column(nel, NI_FLOAT, hh->cn);
      /* forgive the redundancy, need it to facilitate plot in R
         without much group handling */
   NI_set_attribute(nel, "xlabel", hh->label?hh->label:"MrEd");
   SUMA_RETURN(ngr);   
}

SUMA_HIST *SUMA_NIhist_To_hist(NI_group *ngr)
{
   static char FuncName[]={"SUMA_NIhist_To_hist"};
   NI_element *nel=NULL;
   SUMA_HIST *hh=NULL;
   
   SUMA_ENTRY;
   
   if (!ngr) SUMA_RETURN(hh);
   nel = SUMA_FindNgrNamedElement(ngr,"seg_histogram");
   if (!nel) {
      /* try the old name */
      nel = SUMA_FindNgrNamedElement(ngr,"histogram");
   }
   if (!nel) SUMA_RETURN(hh);
   
   hh = (SUMA_HIST *)SUMA_calloc(1,sizeof(SUMA_HIST));
   hh->label = SUMA_copy_string(ngr->name);
   hh->K = nel->vec_len;
   NI_GET_FLOAT(nel,"window",hh->W);
   NI_GET_FLOAT(nel,"min", hh->min);
   NI_GET_FLOAT(nel,"max", hh->max);
   NI_GET_INT(nel,"N_samp", hh->n);
   NI_GET_INT(nel,"N_ignored", hh->N_ignored);
   hh->b = (float *)SUMA_calloc(hh->K,sizeof(float));
   hh->c = (int *)SUMA_calloc(hh->K,sizeof(int));
   hh->cn = (float *)SUMA_calloc(hh->K,sizeof(float));
   memcpy(hh->b, nel->vec[0], sizeof(float)*hh->K);
   memcpy(hh->c, nel->vec[1], sizeof(int)*hh->K);
   memcpy(hh->cn, nel->vec[2], sizeof(float)*hh->K);
   
   SUMA_RETURN(hh);
}

int SUMA_write_hist(SUMA_HIST *hh, char *name)
{
   static char FuncName[]={"SUMA_write_hist"};
   char *ff=NULL;
   NI_stream m_ns = NULL;  
   NI_group *ngr = NULL;
   
   SUMA_ENTRY;
   
   if (!hh) SUMA_RETURN(NOPE);
   if (!(ngr = SUMA_hist_To_NIhist(hh))) {
      SUMA_S_Err("Failed to go NII");
      SUMA_RETURN(NOPE);
   }
   
   if (!name) name = ngr->name;
   
   ff = SUMA_Extension(name, ".niml.hist", NOPE);
   ff = SUMA_append_replace_string("file:",ff,"",2);
   
   m_ns = NI_stream_open( ff , "w" ) ;   
   if( m_ns == NULL ) {    
      SUMA_S_Errv ("Failed to open stream %s\n", ff);  
      SUMA_free(ff); ff=NULL;
      SUMA_RETURN(NOPE);
   } else { 
      /* write out the element */   
      if (NI_write_element( m_ns , ngr , 
                            NI_TEXT_MODE ) < 0) { 
         SUMA_S_Err ("Failed to write element");  
         SUMA_free(ff); ff=NULL;
         NI_free_element(ngr); ngr=NULL; 
         NI_stream_close( m_ns ) ; m_ns = NULL;
         SUMA_RETURN(NOPE);
      }  
   }  
   
   if (ff) SUMA_free(ff); ff = NULL;
   /* close the stream */  
   NI_stream_close( m_ns ) ; m_ns = NULL;
   if (ngr) NI_free_element(ngr); ngr=NULL; 
   SUMA_RETURN(YUP);
}

SUMA_HIST *SUMA_read_hist(char *name) 
{
   
   static char FuncName[]={"SUMA_read_hist"};
   char *ff=NULL;
   NI_stream m_ns = NULL;  
   NI_group *ngr = NULL;
   SUMA_HIST *hh = NULL;
   
   SUMA_ENTRY;
   
   if (!name) SUMA_RETURN(hh);
   ff = SUMA_Extension(name, ".niml.hist", NOPE);
   ff = SUMA_append_replace_string("file:",ff,"",2);
   
   m_ns = NI_stream_open( ff , "r" ) ; 
   if( m_ns == NULL ) {    
      SUMA_S_Errv ("Failed to open stream %s for reading\n", ff);  
      SUMA_free(ff); ff=NULL;
      SUMA_RETURN(hh);
   } else { 
      /* read out the element */   
      if (!(ngr = NI_read_element( m_ns , 1 ))) { 
         SUMA_S_Err ("Failed to read element");  
         SUMA_free(ff); ff=NULL;
         NI_stream_close( m_ns ) ; m_ns = NULL;
         SUMA_RETURN(hh);
      }  
   }  
   /* close the stream */  
   NI_stream_close( m_ns ) ; m_ns = NULL;
   if (!(hh = SUMA_NIhist_To_hist(ngr))) {
      SUMA_S_Err("Failed to get hist from NI");
   }
   if (ff) SUMA_free(ff); ff = NULL;
   if (ngr) NI_free_element(ngr); ngr=NULL;  
   SUMA_RETURN(hh);
}


void SUMA_set_SegFunc_debug(int dbg, int vdbg, int *vdbg3, FILE *out) {
   debug = dbg;
   VoxDbg = vdbg;
   if (vdbg3) { 
      VoxDbg3[0] = vdbg3[0]; VoxDbg3[1] = vdbg3[1]; VoxDbg3[2] = vdbg3[2]; 
   }
   if (out) { VoxDbgOut=out; }
   else {VoxDbgOut= SUMA_STDERR;}
} 

int SUMA_Seg_Write_Dset(char *proot, char *prefi, THD_3dim_dataset *dset, 
                        int iter, char *hh) 
{
   static char FuncName[]={"SUMA_Seg_Write_Dset"};
   char pref[512]; 
   int ovw;   
   char *opref = NULL, *oid = NULL, *ohist = NULL;
   
   SUMA_ENTRY;
   
   opref = SUMA_copy_string(DSET_PREFIX(dset)); 
   oid = SUMA_copy_string(DSET_IDCODE_STR(dset));   
   ohist = tross_Get_History(dset); 
   if (proot != NULL) {
      if (iter >=0) { snprintf(pref, 500, "%s/%s.%d", 
                                       proot, prefi, iter); }
      else { snprintf(pref, 500, "%s/%s", proot, prefi); }
   } else { 
      if (iter >=0) snprintf(pref, 500, "%s.%d",  prefi, iter); 
      else snprintf(pref, 500, "%s", prefi); 
   }  
   
   if (debug) SUMA_S_Notev("Writing %s\n", pref);
      
   EDIT_dset_items(  dset , ADN_prefix  , pref, ADN_none);  
   UNIQ_idcode_fill(DSET_IDCODE_STR(dset));/* new id */   
   if (hh) tross_Append_History(dset, hh);/*add history*/ 
     
   DSET_quiet_overwrite(dset);   
   
   EDIT_dset_items(  dset , ADN_prefix  , opref, ADN_none);  
   strcpy(DSET_IDCODE_STR(dset), oid); 
   
   if (ohist) tross_Replace_History(dset, ohist); 
   SUMA_free(opref); SUMA_free(oid); free(ohist); ohist=NULL;
   
   SUMA_RETURN(1);
}

int SUMA_KeyofLabel_Dtable(Dtable *vl_dtable, char *label) {
   static char FuncName[]={"SUMA_KeyofLabel_Dtable"};
   int kk;
   char *str_key=NULL;
   
   SUMA_ENTRY;
   if (!(str_key = findin_Dtable_b(label, vl_dtable))){
      SUMA_S_Errv("Could not find entry in label table for class %s\n",
                 label);
      SUMA_RETURN(-1);
   }
   kk = strtol(str_key,NULL, 10); 
   SUMA_RETURN(kk);
}

Dtable *SUMA_LabelsKeys2Dtable (char **str, int num, int *keys)
{
   static char FuncName[]={"SUMA_LabelsKeys2Dtable"};
   char sval[256];
   int i;
   Dtable *vl_dtable=NULL;
   
   SUMA_ENTRY;
   
   /* make a labeltable */
   vl_dtable = new_Dtable(5);
   for (i=0; i<num; ++i) {
      if (keys) sprintf(sval,"%d", keys[i]);
      else sprintf(sval,"%d", i+1);
      addto_Dtable( sval , str[i] , vl_dtable ) ;
   }
   
   SUMA_RETURN(vl_dtable);
}

int SUMA_SetDsetLabeltable(THD_3dim_dataset *dset, char **labels, 
                           int N_labels, int *keys)
{
   static char FuncName[]={"SUMA_SetDsetLabeltable"};
   char *labeltable_str=NULL;
   SUMA_ENTRY;
   
   labeltable_str = SUMA_LabelsKeys2labeltable_str(labels, N_labels, keys);
   THD_set_string_atr( dset->dblk , "VALUE_LABEL_DTABLE" , labeltable_str );
   free(labeltable_str); labeltable_str=NULL;
   
   SUMA_RETURN(1);
}

char *SUMA_LabelsKeys2labeltable_str(char **str, int num, int *keys)
{
   static char FuncName[]={"SUMA_LabelsKeys2labeltable_str"};
   char *labeltable_str=NULL;
   Dtable *vl_dtable=SUMA_LabelsKeys2Dtable(str, num, keys);
   
   SUMA_ENTRY;
   
   labeltable_str = Dtable_to_nimlstring(vl_dtable, 
                                             "VALUE_LABEL_DTABLE");
   destroy_Dtable(vl_dtable); vl_dtable=NULL;
   
   SUMA_RETURN(labeltable_str);
}

void SUMA_ShowClssKeys(char **str, int num, int *keys)
{
   static char FuncName[]={"SUMA_ShowClssKeys"};
   int i;

   SUMA_ENTRY;

   for (i=0; i<num; ++i) {
      if (keys) fprintf(SUMA_STDERR, "  %s --> %d\n", str[i], keys[i]);
      else fprintf(SUMA_STDERR, "  %s --> %d (assumed)\n", str[i], i+1);
   }  

   SUMA_RETURNe;
}

#if 0
int get_train_pdist_old(SEG_OPTS *Opt, char *feat, char *cls, 
                     double *par, double *scpar) 
{
   char **clsv=NULL, **featv=NULL;
   float *shapev=NULL, *ratev=NULL;
   int i = 0;
   char *atr=NULL, atname[256]={""};
   NI_str_array *atrs=NULL;
   
   ENTRY("get_train_pdist_old");
   
   if (!Opt->ndist) RETURN(0);
   featv = (char **)Opt->ndist->vec[0];
   clsv  = (char **)Opt->ndist->vec[1]; 
   shapev= (float *)Opt->ndist->vec[2];
   ratev = (float *)Opt->ndist->vec[3];
   scpar[0]=1.0; scpar[1]=0.0;
   sprintf(atname,"%s_Scale+Shift", feat);
   atr = NI_get_attribute(Opt->ndist, atname);
   if (!atr) {
      ERROR_message("Failed to find attribute %s", atname);
      RETURN(0);
   }
   atrs = NI_decode_string_list(atr,",") ;
   if (atrs->num == 2) {
      scpar[0] = strtod(atrs->str[0], NULL);
      scpar[1] = strtod(atrs->str[1], NULL);
      NI_delete_str_array(atrs);
   } else {
      ERROR_message("Failed to find scale and shift in %s", atname);
      RETURN(0);
   }
   for (i=0; i<Opt->ndist->vec_len; ++i) {
      /*fprintf(stderr,"%d/%d, %s %s , %f %f\n",
            i,Opt->ndist->vec_len, featv[i], clsv[i], shapev[i], ratev[i]);*/ 
      if (!strcmp(featv[i],feat) && !strcmp(clsv[i],cls)) {
         par[0] = (double)shapev[i]; par[1] = (double)ratev[i]; 
         RETURN(2);
      }
   }
   
   RETURN(0);
}
#endif

double pdfgam(double x,double ash, double brt) 
{
   #ifdef UNS  /* unstable version */
      double an = pow(brt,ash)/gamma(ash)*pow(x,ash-1)*exp(-brt*x);
      return(an);
   #else
      double an = ash*log(brt) - lgamma(ash) + (ash-1)*log(x) - brt*x;
      return(exp(an));
   #endif
}
#define PDFGAM_UNS(x,ash,brt) (pow(brt,ash)/gamma(ash)*pow(x,ash-1)*exp(-brt*x)) 
#define PDFGAM(x,ash,brt) exp((ash*log(brt) - lgamma(ash) + (ash-1)*log(x) - brt*x)) 

/*!
   Estimate the probability of a particular feature's amplitude given a  class
*/
int p_a_GIV_cvfu(SEG_OPTS *Opt, char *feat, char *cls, 
                  THD_3dim_dataset *pout) 
{
   static char FuncName[]={"p_a_GIV_cvfu"};
   static int icomp_note = 0, ifound_note = 0;
   char fpref[256+IDCODE_LEN+32]={""}, bl[256]={""};
   char fsave[256+IDCODE_LEN+32]={""};
   THD_3dim_dataset *pload=NULL;
   short *a=NULL, *p=NULL;
   int ia = 0, i=0;
   float af=0.0; 
   double dp=0.0, da=0.0, hbw = 0.0, pfhbw, pf=0.0;
   SUMA_FEAT_DIST *FD = NULL;
   
   SUMA_ENTRY; 
   
   if (!pout || DSET_BRICK_TYPE(pout,0) != MRI_short) SUMA_RETURN(0);
   
   /* form the temp filename */
   if (Opt->UseTmp) {
      sprintf(fpref, "/tmp/%s.a_GIV_cvfu-%s-%s",
                  Opt->uid, feat, cls);
      sprintf(fsave, "%s+orig.HEAD", fpref);
   }
   if (Opt->UseTmp && (pload = THD_open_dataset( fsave ))) {
      if (Opt->debug > 1) {
         if (Opt->debug > 2 || !ifound_note) {
            SUMA_S_Notev("Found %s %s\n", 
               fsave, Opt->debug <= 2 ? "(further message will be muted)":""); 
            ++ifound_note;
         }
      }
      if (DSET_BRICK_TYPE(pload,0) != MRI_short) SUMA_RETURN(0);
      DSET_mallocize(pload)   ; DSET_load(pload);
      /* swap column and factor */
      SWAP_COL(pload, pout,0)
      /* erase pload and get out */
      DSET_delete(pload);
      SUMA_RETURN(1);
   } else {
      if (Opt->debug > 1) {
         if (Opt->debug > 2 || !icomp_note) {
            SUMA_S_Notev("Must compute %s, %s %s\n",
              feat, cls, Opt->debug <= 2 ? "(further message will be muted)":"");
            ++icomp_note;
         }
      }
      SB_LABEL(Opt->sig,feat, ia);
      if (ia<0) {
         SUMA_S_Errv("Failed to find %s", feat); SUMA_RETURN(0);
      }
      a = (short *)DSET_ARRAY(Opt->sig, ia);
      af = DSET_BRICK_FACTOR(Opt->sig, ia);
      if (!af) af = 1;
      
      p = (short *)DSET_ARRAY(pout, 0);
      pf = 32767.0; /* max p is 1, so stick with this */
      
      if (!(FD = SUMA_find_feature_dist(Opt->FDV, NULL, feat, cls, NULL))) {
         SUMA_S_Errv("Failed to find dist struct for %s %s\n", 
                        feat, cls);
         SUMA_RETURN(0);
      }
      switch(FD->tp){
         case SUMA_FEAT_GAMMA:
            /* compute probs */
            af = af * FD->scpar[0];
            hbw = Opt->binwidth / 2.0;
            pfhbw = pf * hbw ;
            for (i=0; i<DSET_NVOX(Opt->sig); ++i) {
               if (IN_MASK(Opt->cmask,i)) {
                  da = (double)((a[i]*af)+FD->scpar[1]);
                  #if 0
                  /* gold standard see area.gam*/
                  dp = ( gamma_t2p( da-hbw, FD->par[0] , FD->par[1] ) -
                         gamma_t2p( da+hbw, FD->par[0] , FD->par[1] ) ) * pf;
                  #else
                  dp = (PDFGAM((da-hbw), FD->par[0], FD->par[1]) + 
                        PDFGAM((da+hbw), FD->par[0], FD->par[1]) ) *  pfhbw; 
                  #endif
                  if (i == Opt->VoxDbg) {
                     fprintf(Opt->VoxDbgOut,"      a = %d, a_sc = %f\n"
                                           "p(a(%s)=%f|c=%s)=%f\n",
                                           a[i], da, feat, a[i]*af, cls, dp/pf);
                  }
               } else {
                  if (i == Opt->VoxDbg) fprintf(Opt->VoxDbgOut," Vox Masked\n");
                  dp = 0.0;
               }
               p[i] = (short)dp;
            }
            break;
         case SUMA_FEAT_NP:
            /* get prob from histogram */
            for (i=0; i<DSET_NVOX(Opt->sig); ++i) {
               if (IN_MASK(Opt->cmask,i)) {
                  p[i] = (short)(SUMA_hist_freq(FD->hh, (a[i]*af))*pf);
                  if (i == Opt->VoxDbg) {
                     fprintf(Opt->VoxDbgOut, "h(a(%s)=%f|c=%s)=%f\n",
                                           feat, a[i]*af, cls, (float)p[i]/pf);
                  }
               } else {
                  if (i == Opt->VoxDbg) fprintf(Opt->VoxDbgOut," Vox Masked\n");
                  p[i] = 0;
               }
            }
            break;
         default:
            SUMA_S_Errv(
               "Don't much about dist type %d, but I do know that I love you.\n",
               FD->tp);
            break;
      }
      
      EDIT_BRICK_FACTOR(pout,0,1.0/pf);
      if (Opt->UseTmp) {
         if (Opt->debug > 1) INFO_message("Writing %s", fsave);
         UNIQ_idcode_fill(DSET_IDCODE_STR(pout));/* new id */
         EDIT_dset_items( pout, ADN_prefix, fpref, ADN_none );
         sprintf(bl, "p(a(%s)|c=%s)",feat, cls);
         EDIT_BRICK_LABEL(pout,0,bl);
         DSET_quiet_overwrite(pout) ;
      }
      SUMA_RETURN(1);   
   }            
   
   SUMA_RETURN(0);
}

/*!
Estimate the probability of a class, given a particular feature
*/
int p_cv_GIV_afu (SEG_OPTS *Opt, char *feat, 
                  char *cls, double *d) 
{
   static char FuncName[]={"p_cv_GIV_afu"};
   static THD_3dim_dataset *pb=NULL;
   static double *dd=NULL;
   static long long init=0;
   int i,j, ifeat = -1;
   short *a=NULL;
   float af =0.0, pf = 0.0;
   double bb=0.0;
   
   SUMA_ENTRY;
   
   if (cls==NULL) { 
      if (!init) {/* init */
         if (pb) { ERROR_message("Non null pb"); SUMA_RETURN(0); }
         NEW_SHORTY(Opt->sig,1,"p_cv_GIV_afu",pb);
         if (!pb) SUMA_RETURN(0);
         dd = (double *)calloc(DSET_NVOX(Opt->sig), sizeof(double));
         if (!dd) SUMA_RETURN(0);
         init = 1;
      } else { /* clean */
         DSET_delete(pb); pb=NULL; 
         free(dd); dd=NULL; init=0;
      }
      SUMA_RETURN(1);
   }
   
   if (!pb || init==0) { ERROR_message("Not initialized"); SUMA_RETURN(0); }
   if (!d) { ERROR_message("NULL d"); SUMA_RETURN(0); }
   SB_LABEL(Opt->sig,feat, ifeat); 
   if (ifeat < 0) {
      SUMA_S_Errv("Failed to find feature %s\n", feat); SUMA_RETURN(0);
   }
   memset(d, 0, DSET_NVOX(Opt->sig)*sizeof(double));
   for (i=0; i<Opt->clss->num;++i) {
      if (Opt->debug > 2) 
         SUMA_S_Notev(" Calling p_a_GIV_cvfu %d/%d\n", i,Opt->clss->num); 
      if (!(p_a_GIV_cvfu(Opt, feat, Opt->clss->str[i],pb))) {
         SUMA_S_Err("Failed in p_a_GIV_cvfu"); SUMA_RETURN(0);
      }
      a = (short *)DSET_ARRAY(pb,0);
      af = DSET_BRICK_FACTOR(pb,0); if (af==0.0) af=1.0;
      if (!strcmp(Opt->clss->str[i], cls)) {
         for (j=0; j<DSET_NVOX(Opt->sig); ++j) {
            if (IN_MASK(Opt->cmask, j)) {
               bb = Opt->mixfrac[i] * a[j];/* skip af scaling here */
               dd[j] = bb;
               d[j] += bb;
            } else {
               dd[j] = 0.0; 
            }
         }
      } else {
         for (j=0; j<DSET_NVOX(Opt->sig); ++j)  {
            if (IN_MASK(Opt->cmask, j)) {
               d[j] += Opt->mixfrac[i] * a[j];/* skip af scaling here */
            } else {
               /* nothing needed */
            }
         }
      }
   }
   
   for (j=0; j<DSET_NVOX(Opt->sig); ++j) {/* compute ratio, scaling not needed */
      if (IN_MASK(Opt->cmask, j)) {
         d[j] = dd[j]/d[j]; 
         if (j == Opt->VoxDbg) {
            fprintf(Opt->VoxDbgOut,"   p(c=%s|%s=%f)=%f\n",
                                  cls, feat, THD_get_voxel(Opt->sig, j, ifeat), 
                                  d[j]);
         }
         if (isnan(d[j])) d[j] = 0.0;
      } else {
         d[j] = 0.0;
      }
   }
   
   if (Opt->debug > 2) {
      char ff[256];
      FILE *fout=NULL;
      sprintf(ff,"p_cv_GIV_afu.%lld.1D",init);
      fout = fopen(ff,"w");
      SUMA_S_Notev("Writing %s", ff);
      for (j=0; j<DSET_NVOX(Opt->sig); ++j) 
         fprintf(fout,"%f\n",d[j]);
      fclose(fout);
   }
   
   ++init;
   SUMA_RETURN(1);
}

/*!
   Estimate the probability of a class, given all features 
*/
int p_cv_GIV_A (SEG_OPTS *Opt, char *cls, double *dr) 
{
   static char FuncName[]={"p_cv_GIV_A"};
   char fpref[256]={""};
   double pf= 32767.0; /* max p is 1, so stick with this */
   static double *d=NULL, ddd=0.0, wfeat;
   static int init=0;
   int i, j, icls;
   short *a=NULL;
   static THD_3dim_dataset *pcgrec=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;      

   
   if (cls==NULL) { 
      if (!init) {/* init */
         SUMA_LH("Initialization");
         if (d) { SUMA_S_Err("Non null d"); SUMA_RETURN(0); }
         d = (double *)calloc(DSET_NVOX(Opt->sig), sizeof(double));
         if (!d) SUMA_RETURN(0);
         if (!p_cv_GIV_afu (Opt, NULL, NULL, d) ) SUMA_RETURN(0);
         if (Opt->Writepcg_G_au) {
            NEW_SHORTY(Opt->sig, Opt->feats->num,"p_cv_GIV_A_debug",pcgrec);
            if (!pcgrec) SUMA_RETURN(0);
         }
         init = 1;
      } else { /* clean */
         SUMA_LH("Cleanup");
         if (!p_cv_GIV_afu (Opt, NULL, NULL, d) ) SUMA_RETURN(0);
         if (pcgrec) DSET_delete(pcgrec); pcgrec=NULL; 
         free(d); d=NULL; 
         init=0;
      }
      SUMA_RETURN(1);
   }
   
   if (!dr) SUMA_RETURN(0);
   if (init!=1) { SUMA_S_Err("Not initialized"); SUMA_RETURN(0); }
   if (Opt->Writepcg_G_au && !pcgrec) {
      SUMA_S_Err("Want Writepcg_G_au, but no pcgrec. Bad init.");
      SUMA_RETURN(0);
   }
   memset(dr, 0, DSET_NVOX(Opt->sig)*sizeof(double));
   if ((icls = SUMA_NI_str_array_find(cls, Opt->clss, 0, 0)) < 0) {
      SUMA_S_Errv("Failed to find class %s !!!\n", cls);
      SUMA_RETURN(0);
   } 
   SUMA_LH("Looping over features");
   for (i=0; i<Opt->feats->num; ++i) {
      if (Opt->feat_exp) wfeat = Opt->feat_exp[icls][i];
      else wfeat= 0.0;
      if (Opt->debug > 1)  
         SUMA_S_Notev("Calling p_cv_GIV_afu %d/%d\n", i,Opt->feats->num);
      if (!(p_cv_GIV_afu(Opt, Opt->feats->str[i], cls, d))) {
         SUMA_S_Err("Failed in p_cv_GIV_afu"); SUMA_RETURN(0);
      }
      
      if (Opt->Writepcg_G_au) {
         /* stick the results in pcgrec and write it out */
         a = (short *)DSET_ARRAY(pcgrec,i);
         for (j=0; j<DSET_NVOX(Opt->sig); ++j) {
            ddd = d[j]+MINP;
            if (wfeat > 0) ddd = pow(ddd,wfeat);
            if (ddd>1.0) a[j]=(short)pf;
            else a[j] = (short)(pf*(ddd));
         }
         EDIT_BRICK_FACTOR(pcgrec, i,1.0/pf);
         sprintf(fpref, "p(c=%s|a(%s))", cls, Opt->feats->str[i]);
         EDIT_BRICK_LABEL(pcgrec,i,fpref);
      }
   
      for (j=0; j<DSET_NVOX(Opt->sig); ++j) {
         if (IN_MASK(Opt->cmask, j)) {
            #if 0
            if (1) {
               if (d[j] > MINP) dr[j] = dr[j] + log(d[j]);
               else dr[j] = dr[j] + log(MINP); 
               /* if (!(j%1000)) 
                  fprintf(stderr,"dr %f exp(dr) %f \n",  dr[j], exp(dr[j])); */
            } else {
                dr[j] = dr[j] + log(d[j]);
            }
            #else
               /* better just add MINP to all probs Aug. 2012*/
               d[j] += MINP; 
               if (d[j]>1.0) d[j] = 1.0; /* This is not proper,
                                             proper scaling should be done
                                             once all d[j] are computed across
                                             classes */
               if (wfeat>0) dr[j] = dr[j] + wfeat*log(d[j]);
               else dr[j] = dr[j] + log(d[j]);
            #endif
         } else {
            dr[j] = 0.0;
         }
      }
   }
   
   if (Opt->Writepcg_G_au) {
      UNIQ_idcode_fill(DSET_IDCODE_STR(pcgrec));/* new id */
      sprintf(fpref,"%s.p_c%s_G_each_feature", Opt->prefix, cls);
      EDIT_dset_items( pcgrec, ADN_prefix, fpref, ADN_none );
      DSET_quiet_overwrite(pcgrec) ;
   }
   
   if (!Opt->logp) {
      /* undo log and return */
      for (j=0; j<DSET_NVOX(Opt->sig); ++j) {
         if (IN_MASK(Opt->cmask,j)) dr[j] = exp(dr[j]);
      }
   }
   
   if (Opt->VoxDbg >= 0) {
      fprintf(Opt->VoxDbgOut,"      %sp(c=%s|a,ALL f)=%f\n",
                            Opt->logp ? "LOG" : "", 
                            cls, dr[Opt->VoxDbg]);
   }
   SUMA_RETURN(1);
}

int normalize_p(SEG_OPTS *Opt, THD_3dim_dataset *pout) {
   int i, ii, j;
   float bfi[DSET_NVALS(pout)];
   float bfo[DSET_NVALS(pout)];
   double dv[DSET_NVALS(pout)], ddf, sdv;
   double dvo[DSET_NVALS(pout)];

   ENTRY("normalize_p");
   
   for (i=0; i<DSET_NVALS(pout); ++i) bfo[i]=1/32767.0f;

   GET_BFs(pout, bfi);
   for (j=0; j<DSET_NVOX(Opt->sig); ++j) {
      if (IN_MASK(Opt->cmask, j)) {
         GET_VEC_AT_VOX(pout, j, dv, bfi);
         if (Opt->logp) {
            for (i=0; i<DSET_NVALS(pout); ++i) {
               ddf = 1.0;
               for (ii=0; ii<DSET_NVALS(pout); ++ii) {
                  if (ii!=i) ddf += exp(dv[ii]-dv[i]);
               }
               dvo[i] = 1.0f / ddf;
            }
            PUT_VEC_AT_VOX(pout,j,dvo,bfo);
         } else {
            sdv=0.0;
            for (i=0; i<DSET_NVALS(pout); ++i) sdv +=dv[i];
            for (i=0; i<DSET_NVALS(pout); ++i) dvo[i] = dv[i]/sdv;
            PUT_VEC_AT_VOX(pout,j,dvo,bfo);
         }
      }
   }
   PUT_BFs(pout, bfo);
   
   RETURN(1);
}


int is_shorty(THD_3dim_dataset *pset) 
{
   int ii;
   if (!pset) return(0);
   for (ii=0; ii<DSET_NVALS(pset); ++ii) {
      if (DSET_BRICK_TYPE(pset,ii) != MRI_short) return(0);
   }
   return(1);
}

/*!
   Set the floor of the probabilities in a dataset 
*/
int set_p_floor(THD_3dim_dataset *pset, float pfl, byte *cmask)
{
   static char FuncName[]={"set_p_floor"};
   int i, ii, j;
   float bfi[DSET_NVALS(pset)];
   float bfo[DSET_NVALS(pset)];
   double dv[DSET_NVALS(pset)];
   double dvo[DSET_NVALS(pset)];
   double d = 0.0, N = 0.0;
   
   SUMA_ENTRY;
   
   if (!is_shorty(pset)) {
      SUMA_S_Errv("Dset %s not all shorts\n", DSET_PREFIX(pset));
      SUMA_RETURN(0);
   }
   
   N = DSET_NVALS(pset);
   d = pfl/(1.0-pfl*N);
   
   for (i=0; i<DSET_NVALS(pset); ++i) bfo[i]=1/32767.0f;
   
   GET_BFs(pset, bfi);
   for (j=0; j<DSET_NVOX(pset); ++j) {
      if (IN_MASK(cmask, j)) {
         GET_VEC_AT_VOX(pset, j, dv, bfi);
         for (i=0; i<DSET_NVALS(pset); ++i) dvo[i] = (dv[i]+d)/(1.0+N*d);
         PUT_VEC_AT_VOX(pset,j,dvo,bfo);
      }
   }
   PUT_BFs(pset, bfo);
   SUMA_RETURN(1);
}

/*!
   Estimate the probability of each class, given all features 
*/
THD_3dim_dataset *p_C_GIV_A (SEG_OPTS *Opt) 
{
   static char FuncName[]={"p_C_GIV_A"};
   char bl[256]={""};
   int i,j, ii;
   double *d=NULL;
   THD_3dim_dataset *pout=NULL;
   short *p=NULL;
   float pf=0.0;
   
   SUMA_ENTRY;
   
   /* init */
   d = (double *)calloc(DSET_NVOX(Opt->sig), sizeof(double));
   if (!d) SUMA_RETURN(NULL);
   if (!p_cv_GIV_A (Opt, NULL, d)) SUMA_RETURN(NULL);
   NEW_SHORTY(Opt->sig, Opt->clss->num, Opt->prefix, pout);
   if (!pout) SUMA_RETURN(NULL);
   if( !THD_ok_overwrite() && THD_is_file( DSET_HEADNAME(pout) ) ){
      ERROR_exit("Output file %s already exists -- cannot continue!\n",
                  DSET_HEADNAME(pout) ) ;
   }

   /* process */
   for (i=0; i<Opt->clss->num; ++i) {
      if (Opt->debug > -1000)  
         SUMA_S_Notev("Calling p_cv_GIV_A %d/%d\n", i,Opt->clss->num);
      if (!(p_cv_GIV_A (Opt, Opt->clss->str[i],d))) {
         SUMA_S_Err("Failed in p_cv_GIV_A"); SUMA_RETURN(NULL);
      }

      /* and store in output */
      if (!Opt->logp) {
         p = (short *)DSET_ARRAY(pout, i);
         pf = 32767.0f; /* max p is 1, so stick with this */
         for (j=0; j<DSET_NVOX(Opt->sig); ++j) {
            p[j]=(short)(d[j]*pf);
         }
         EDIT_BRICK_FACTOR(pout,i,1.0/pf);
         sprintf(bl, "p(c=%s|A)",Opt->clss->str[i]);
      } else {
         sprintf(bl, "LOGp(c=%s|A)",Opt->clss->str[i]);
         EDIT_substscale_brick(pout, i, MRI_double, d, MRI_short, -1.0);
      }
      EDIT_BRICK_LABEL(pout,i,bl);
   }
   

   if (Opt->rescale_p) {
      /* Now rescale probs so that sum is 1 */
      if (!normalize_p(Opt, pout)) {
         ERROR_exit("Failed to normalize_p!\n",
                  DSET_HEADNAME(pout) ) ;
      }
      /* and redo labels */
      for (i=0; i<Opt->clss->num; ++i) {
         sprintf(bl, "P(c=%s|A)",Opt->clss->str[i]);
         EDIT_BRICK_LABEL(pout,i,bl);
      }
   } else {
      /* labels OK from above*/
   }  
   
   /* clean */
   if (!p_cv_GIV_A (Opt, NULL, d)) {
      SUMA_S_Err("Failed in p_cv_GIV_A cleanup but will proceed");
   }
   free(d); d= NULL;
   
   SUMA_RETURN(pout);
}

int SUMA_LabelToGroupedIndex(char *cls_str, char **group_lbls, int N_group_lbls)
{
   static char FuncName[]={"SUMA_LabelToGroupedKey"};
   int mtch=0, j,  ng=0, jmatch=-1;
   
   SUMA_ENTRY;
   
   mtch = 0;
   for (j=0; j<N_group_lbls; ++j) {
      ng = strlen(group_lbls[j]);
      if (strlen(cls_str) >= ng) {
         if (!strcmp(cls_str, group_lbls[j])) {
               /* ININFO_message("%s --> %s (%d)", 
                                 cls_str, group_lbls[j], j); */
               jmatch = j;
               mtch += 1;
         } else if (!strncmp(cls_str, group_lbls[j], 
                                    strlen(group_lbls[j])) && 
                        ( cls_str[ng] == ',' ||
                          cls_str[ng] == '.' ||
                          cls_str[ng] == '-' ||
                          cls_str[ng] == '_') ) {
               /* ININFO_message("%s --> %s (%d)", 
                              cls_str, group_lbls[j], j); */
               jmatch = j;
               mtch += 1;
         }   
      }
   }
   if (mtch == 0) {
      /* ININFO_message("Could not find match for %s\n", cls_str); */
      SUMA_RETURN(-1);
   }
   if (mtch > 1) {
      /* ERROR_message("Found more than one match"); */
      SUMA_RETURN(-mtch);
   }
   
   SUMA_RETURN(jmatch);
}

int SUMA_LabelToGroupedKey(char *cls_str, char **group_lbls, int N_group_lbls, 
                      int *group_keys) {
   int j = SUMA_LabelToGroupedIndex(cls_str,  group_lbls, N_group_lbls);
   
   if (j<0) return(j);
   else return(group_keys[j]);               
}


int SUMA_GroupLabelMapping (char **clss_lbls , int N_clss_lbls, 
                        char **grpclss_lbls, int N_grpclss_lbls, 
                        int *map, int verb) 
{
   static char FuncName[]={"SUMA_GroupLabelMapping"};
   int j, i;
   
   SUMA_ENTRY;
   
   /* make sure you can map one to the other */
   for (i=0; i<N_clss_lbls; ++i) map[i] = -1;
   {
      for (i=0; i<N_clss_lbls; ++i) {
         j = SUMA_LabelToGroupedIndex(clss_lbls[i], grpclss_lbls, 
                                       N_grpclss_lbls);
         if (j >= 0) { map[i] = j; }
         
      }
   }
   if (verb) {
      for (i=0; i<N_clss_lbls; ++i) {
         if (map[i]>=0) {
            fprintf(stderr,"%s --> %s\n", clss_lbls[i] , grpclss_lbls[map[i]]);
         } else {
            fprintf(stderr,"%s --> NO MATCH\n", clss_lbls[i]);
         }
      }
   }
   SUMA_RETURN(1);
}

/*!
   Regroup classes.
*/
int SUMA_Regroup_classes (SEG_OPTS *Opt,
                     char **clss_lbls,
                     int N_clss_lbls,
                     int *keys, 
                     char **group_clss_lbls,
                     int N_group_clss_lbls,
                     int  * ugroup_keys,
                     byte *cmask,
                     THD_3dim_dataset *pset, 
                     THD_3dim_dataset *cset,
                     THD_3dim_dataset **gpsetp, 
                     THD_3dim_dataset **gcsetp) 
{ 
   static char FuncName[]={"SUMA_Regroup_classes"};
   int i,c, v,gc, imax=0, dtable_key[1024], ckey=0, gkey = 0;
   short *p=NULL, *pg=NULL;
   double max=0.0;
   int igrp[1024], mapverb=0;
   int group_keys[1024];
   THD_3dim_dataset *gpset=NULL,*gcset=NULL;
   char sval[256], *group_labeltable_str=NULL;
   Dtable *vl_dtable=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   /* checks */
   if (group_clss_lbls==NULL) {
      SUMA_S_Errv("Bad input %p \n", 
                  group_clss_lbls);
      SUMA_RETURN(0);
   }
   
   mapverb=0;
   if (!cset && !pset) {
      mapverb = 1;
   } 
   
   /* figure out the mapping between one and the other */
   if (!SUMA_GroupLabelMapping (clss_lbls, N_clss_lbls, 
                                group_clss_lbls, N_group_clss_lbls, 
                                igrp, mapverb)) {
      ERROR_message("Failed to group map");
      SUMA_RETURN(0);
   }
   
   if (!cset && !pset) {
      /* just called to be sure conversion is OK */
      SUMA_RETURN(1);
   } 
   
   if (!gcsetp || *gcsetp) {
      ERROR_message("You must send the address of a null pointer for gcsetp");
      SUMA_RETURN(0);
   }
   if (gpsetp && *gpsetp) {
      ERROR_message("If you send gpsetp it must be the address to null pointer");
      SUMA_RETURN(0);
   }
   
   if (pset && !gpsetp) {
      ERROR_message("Nothing to return grouped probs in");
      SUMA_RETURN(0);
   }
   gcset = *gcsetp;
   NEW_SHORTY(cset, DSET_NVALS(cset), Opt->cgrefix, gcset);

   /* get the key of each group_clss, and form dtable */
   vl_dtable = new_Dtable(5);
   for (i=0; i<N_group_clss_lbls; ++i) {
      if (ugroup_keys) group_keys[i] = ugroup_keys[i];
      else group_keys[i] = i+1;
      sprintf(sval,"%d", group_keys[i]);
      addto_Dtable( sval , group_clss_lbls[i] , vl_dtable ) ;
   }   
   
   p = (short *)DSET_ARRAY(cset,0);
   pg = (short *)DSET_ARRAY(gcset,0);
   
   for (i=0; i<DSET_NVOX(cset); ++i) { pg[i] = p[i]; } /* init */
   for (c=0; c<N_clss_lbls; ++c) {
      ckey = keys[c];
      if ((gkey = SUMA_LabelToGroupedKey(clss_lbls[c],
                        group_clss_lbls, N_group_clss_lbls,group_keys)) < 0) {
      
         /* ERROR_message("Failed to get group key" ); */
         /* that's OK, mask it */
         gkey = 0; /* mask entry */
      } 
      {
         for (i=0; i<DSET_NVOX(cset); ++i) {
            if (p[i] == ckey) {
               pg[i] = gkey;
            }
         }
      }
   }

   EDIT_BRICK_LABEL(gcset,0,"maxprob_labels");
   group_labeltable_str = Dtable_to_nimlstring(vl_dtable, 
                                                "VALUE_LABEL_DTABLE");
   THD_set_string_atr(gcset->dblk , 
                        "VALUE_LABEL_DTABLE" , group_labeltable_str );
   free(group_labeltable_str); 
   *gcsetp = gcset; gcset=NULL;
   
   /* if we have probabilities, need to group those too */
   if (pset && gpsetp) {
      double dv[N_clss_lbls], gdv[N_group_clss_lbls], sgdv;
      float bfi[N_clss_lbls];
      gpset = *gpsetp;
      NEW_SHORTY(pset, N_group_clss_lbls, Opt->pgrefix, gpset);
      GET_BFs(pset, bfi);
      
      for (v=0; v<DSET_NVOX(pset); ++v) {
         if (IN_MASK(cmask, v)) {
            GET_VEC_AT_VOX(pset, v, dv, bfi);
            sgdv=0.0;
            for (gc=0; gc<N_group_clss_lbls; ++gc) {
               gdv[gc] = 0.0; 
               for (c=0; c<N_clss_lbls; ++c) {
                  if (igrp[c] == gc) {
                     if (dv[c] > gdv[gc]) {
                        gdv[gc] = dv[c]; 
                     }
                  }
               }
               sgdv += gdv[gc];
            }
            if (sgdv) {
               for (gc=0; gc<N_group_clss_lbls; ++gc) {
                  gdv[gc] /= sgdv;
               }
            }
            /* can use the same factor from other dset, 
               all values between 0 and 1*/
            PUT_VEC_AT_VOX(gpset, v, gdv, bfi);
         }
      }
      
      PUT_BFs(gpset, bfi);   
      for (gc=0; gc<N_group_clss_lbls; ++gc) {
         sprintf(sval,"p.%s",group_clss_lbls[gc]);
         EDIT_BRICK_LABEL(gpset,gc,sval);
      }
      
      *gpsetp = gpset; gpset=NULL;
   }
   
   SUMA_RETURN(1);
}

int SUMA_SplitClass_ind(int ig, int ks, int N_Glbls, int *Split)
{
   int i, k, l=0;
   for (i=0; i<N_Glbls; ++i) {
      for (k=0; k<Split[i]; ++k) {
         if (i==ig && k==ks) return(l);
         ++l;
      }
   } 
   return(-1);
}

int SUMA_Split_Classes(char **Glbls, int N_Glbls, int *Gkeys, int *Split,
                       THD_3dim_dataset *aset, THD_3dim_dataset *Gcset,
                       byte *cmask, 
                       THD_3dim_dataset **Scsetp, SUMA_CLASS_STAT **Scs,
                       SEG_OPTS *Opt) 
{
   static char FuncName[]={"SUMA_Split_Classes"};
   char **Slbls, snum[64];
   int N_Slbls, *Skeys, *GRkey;
   int i, j, k, l, ijk, smask_count, N_submax;
   short *c=NULL, *ctmp=NULL, *sc=NULL;
   byte *smask=NULL;
   THD_3dim_dataset *Scset=NULL;
   OPT_KMEANS oc;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
      
   /* total number of resultant classes */
   N_Slbls = 0; N_submax=0;
   for (i=0; i<N_Glbls; ++i) {
      N_Slbls += Split[i]; 
      if (Split[i]>N_submax) N_submax = Split[i];
   }   
   
   /* create output */
         /* Using N_Slbls for Slbls was causing a 
            MCW_malloc post-corruption error at free 
            time. This would happen even if nothing
            was done before freeing Slbls (search for BUG below). 
            Valgrind found nothing. 
            Not sure what to do, but adding a +1 seems to
            fix the problem */
   Slbls = (char **)SUMA_calloc(N_Slbls+1, sizeof(char*));   
   Skeys = (int   *)SUMA_calloc(N_Slbls, sizeof(int)  );
   GRkey = (int   *)SUMA_calloc(N_Slbls, sizeof(int)  );
   
   /* Fill the keys */
   l = 0;
   for (i=0; i<N_Glbls; ++i) {
      for (k=0; k<Split[i]; ++k) {
         sprintf(snum,"%02d", k);
         Skeys[l] = 10*(i+(N_submax+1)/10)+k+1;
         Slbls[l] = SUMA_append_replace_string(Glbls[i],snum,".",0);   
         GRkey[l] = Gkeys[i];
         ++l;
      }
   }
   #if 0 /* BUG See BUG above  */
      SUMA_LHv("%d, %d\n", l, N_Slbls);
      for (l=0; l<N_Slbls; ++l) SUMA_ifree(Slbls[l]); 
      SUMA_ifree(Slbls);
      exit(1);
   #endif
      
   if (LocalHead) {
      for (l=0; l<N_Slbls; ++l) {
         fprintf(SUMA_STDERR,"Slbls[%03d]=%s --> %d (parent %d)\n", 
                           l, Slbls[l], Skeys[l], GRkey[l]); 
      }
   } 
   /* a new class stats */
   *Scs =  SUMA_New_Class_Stat(Slbls, N_Slbls, Skeys, 3, NULL);
   
   /* Add GRkey */
   for (l=0; l<N_Slbls; ++l) {
      SUMA_set_Stat(*Scs, Slbls[l], "GRkey",(double)GRkey[l]);
   }
   
   
   /* Here is where you split the actual classes */
   if (Gcset) {
      SUMA_LH("Working Gcset");
      if (*Scsetp == NULL) {
         NEW_SHORTY(Gcset,1,"split.classes",Scset); *Scsetp = Scset;
      } else {
         Scset = *Scsetp;
      }
      smask = (byte *)SUMA_calloc(sizeof(byte), DSET_NVOX(aset));
      c = (short *)DSET_ARRAY(Gcset,0);
      sc = (short *)SUMA_calloc(sizeof(short), DSET_NVOX(aset));
      oc = new_kmeans_oc();
      oc.remap = MAG; oc.verb = 0; oc.distmetric = 'e';
      oc.r = 3;
      for (i=0; i<N_Glbls; ++i) {
         oc.k = Split[i]; 
         snprintf(snum,60,"Split.%s.%02d",Glbls[i],oc.k);
         oc.jobname = SUMA_copy_string(snum);
         for (k=0; k<Split[i]; ++k) {
            l = SUMA_SplitClass_ind(i,k, N_Glbls, Split);
            oc.clabels[k] = Slbls[l];
         }
         smask_count = 0;
         for (ijk=0; ijk<DSET_NVOX(aset); ++ijk) {
            if (IN_MASK(cmask, ijk) && c[ijk] == Gkeys[i]) {
               smask[ijk] = 1; ++smask_count;
            } else smask[ijk] = 0;
         }
         SUMA_LHv("Splitting class %s (%d voxels) into %d\n", 
                  Glbls[i], smask_count, Split[i]);
         if (!(thd_Acluster1 (aset,
               smask, smask_count,
               &Scset,
               NULL,
               NULL,
               oc))) {
            SUMA_S_Err("Failed to split cluster");
            SUMA_RETURN(0);           
         }
         SUMA_ifree(oc.jobname);
         /* Now collect new clusters in new array sc*/
         l = SUMA_SplitClass_ind(i, 0, N_Glbls, Split);
         ctmp = (short *)DSET_ARRAY(Scset,0);
         for (ijk=0; ijk<DSET_NVOX(aset); ++ijk) {
            if (smask[ijk]) {
               sc[ijk] = Skeys[l+ctmp[ijk]-1];
            }
         }  
      } /* for each grouped class i */
      SUMA_ifree(smask);
      /* Now put the new array in Scset */
      EDIT_substitute_brick(Scset,0,MRI_short,sc);


      /* And a proper labeltable */
      if (!SUMA_SetDsetLabeltable(Scset, Slbls, N_Slbls, Skeys)) {
         SUMA_S_Err("Failed to set labeltable");
         SUMA_RETURN(0);
      }

      if (LocalHead) {
         SUMA_Seg_Write_Dset(Opt->proot, "Splitted", 
                          Scset, -1, Opt->hist);
      }
   }
   
   SUMA_LH("Free temps");
   for (l=0;l<N_Slbls; ++l) SUMA_ifree(Slbls[l]);
   SUMA_ifree(Slbls); SUMA_ifree(Skeys); SUMA_ifree(GRkey);

   SUMA_RETURN(1);
}

int SUMA_assign_classes (THD_3dim_dataset *pset, 
                         SUMA_CLASS_STAT *cs,
                         byte *cmask,
                         THD_3dim_dataset **csetp) 
{
   return(SUMA_assign_classes_eng (pset, cs->label, cs->N_label,
                                   cs->keys, cmask, csetp));
}
/*!
   Assign a class given likelihoods.
   Only tested if normalize_p was called 
*/
int SUMA_assign_classes_eng (THD_3dim_dataset *pset, 
                         char **label, int N_label, int *keys,
                         byte *cmask,
                         THD_3dim_dataset **csetp)
{
   static char FuncName[]={"SUMA_assign_classes_eng"};
   int i,j, imax=0, uneven;
   double *d=NULL;
   THD_3dim_dataset *cset=*csetp;
   short *p=NULL;
   double max=0.0;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   /* checks */
   if (!pset || !keys ) {
      SUMA_S_Errv("Bad input %p %p\n", pset, keys);
      SUMA_RETURN(0);
   }
   
   /* init */
   if (!cset) {
      NEW_SHORTY(pset, 1, "assign_classes", cset);
      *csetp = cset;
   }
   
   if (!cset) RETURN(0);

   /* make sure all factors are the same */
   max = DSET_BRICK_FACTOR(pset,0);
   uneven = 0;
   for (j=1; j<DSET_NVALS(pset); ++j) {
      if (max != DSET_BRICK_FACTOR(pset,j)) {
         uneven=1; break;
      }
   }   
   /* process */
   p = (short *)DSET_ARRAY(cset,0);
   if (!uneven) {
      for (j=0; j<DSET_NVOX(pset); ++j) {
         if (IN_MASK(cmask, j)) {
            MAX_AT_VOX(pset, j, imax, max);
            p[j] = keys[imax];  
         } else {
            p[j]=0; 
         }
      }
   }else {
      for (j=0; j<DSET_NVOX(pset); ++j) {
         if (IN_MASK(cmask, j)) {
            MAX_SC_AT_VOX(pset, j, imax, max);
            p[j] = keys[imax];  
         } else {
            p[j]=0; 
         }
      }
   }
   
   if (!SUMA_SetDsetLabeltable(cset, label, N_label, keys)) {
      SUMA_S_Err("Failed to set labeltable");
      SUMA_RETURN(0);
   }
   EDIT_BRICK_LABEL(cset,0,"maxprob_labels");
   
   SUMA_RETURN(1);
}


/*!
   Calculate group mean 
*/
int  group_mean (SEG_OPTS *Opt, THD_3dim_dataset *aset,
                 byte *mm, THD_3dim_dataset *pset, int N_cl,
                 double *M_v, int scl) {
   int i=0, g= 0;
   short *a=NULL, *p=NULL;
   float bf = 1.0;
   char srep[512]={""}, sbuf[64]={""};
   double w=0.0;
   
   ENTRY("group_mean");
   
   if (pset) p=(short*)DSET_ARRAY(pset,0);
   a = (short *)DSET_ARRAY(aset,0);
   bf=DSET_BRICK_FACTOR(aset,0);
   if (bf == 0.0) bf = 1.0;
   
   if (!p) { 
      for (g=0; g<N_cl; ++g) {
         M_v[g] = 0.0; w=0.0;
         for (i=0; i<DSET_NVOX(aset); ++i) {
            if (mm[i] == g+1) {
               M_v[g] += a[i]; ++w;
            }
         }
         M_v[g] = M_v[g]/w;
         if (scl) M_v[g] = bf * M_v[g];
      }
   } else {/* weighted avg */
      for (g=0; g<N_cl; ++g) {
         M_v[g] = 0.0; w=0.0;
         for (i=0; i<DSET_NVOX(aset); ++i) {
            if (mm[i] == g+1) {
               M_v[g] += p[i]*a[i]; w=w+p[i];
            }
         }
         M_v[g] = M_v[g]/w;
         if (scl) M_v[g] = bf * M_v[g];
      }
   }
   
   if (Opt->debug > 1) {
      for (g=0; g<N_cl; ++g) {
         if (scl) sprintf(sbuf,"%d -- %f , (%f)  ", 
                           g+1, M_v[g], M_v[g]/bf);
         else sprintf(sbuf,"%d -- %f , (%f)  ", 
                           g+1, M_v[g]*bf, M_v[g]);
         
         strcat(srep, sbuf);
      }
      INFO_message("%s group means brick scaled , (unscaled): %s\n", 
                  p ? "p-weighted" : "uniform-weight", 
                  srep); 
   }
   
   RETURN(1);
}

 
int SUMA_ijk_to1 (int i, int j, int k, int ni, int nij) {
   return(i+j*ni+k*nij);
}
int SUMA_jik_to1 (int j, int i, int k, int ni, int nij) {
   return(i+j*ni+k*nij);
}
int SUMA_kji_to1 (int k, int j, int i, int ni, int nij) {
   return(i+j*ni+k*nij);
}

/*! Find and fill holes in volume.
The holes are defined as zero voxels that are
surrounded with non-zero ones in at least one of the 
three directions. 
This function is relatively slow, use SUMA_mri_volume_infill_zoom instead.
*/
int SUMA_mri_volume_infill(MRI_IMAGE *imin) 
{
   static char FuncName[]={"SUMA_mri_volume_infill"};
   int Ni, Nj, Nk, Nij, Nijk, i, j, k, v, iter;
   int d, d1b, d1e, d1, d2b, d2e, d2, d3b, d3e, d3;
   int Nfills, trg, cnd;
   float cndval, aprev;
   byte *ba=NULL;
   float *fa=NULL, *sum=NULL;
   byte *hits = NULL;
   int minhits = 0; 
   int maxiter=500;
   int (*I3_to_1)(int, int, int, int, int);
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   Ni = imin->nx; Nj = imin->ny; Nk = imin->nz; 
   Nij = Ni*Nj; Nijk = Nij*Nk;
   
   fa = MRI_FLOAT_PTR(imin);
   
   /* make byte mask */
   ba = (byte *)SUMA_calloc(Nijk, sizeof(byte));
   for (v=0; v<Nijk; ++v) {
      if (SUMA_ABS(fa[v]-0.0f)>0.00001) ba[v] = 1; 
   }
   sum = (float *)SUMA_calloc(Nijk, sizeof(float));
   hits = (byte *)SUMA_calloc(Nijk, sizeof(byte));
   
   iter = 0;
   do {
      for (d=0; d<6; ++d) { /* for each direction */
         switch (d) {
            case 0:
               d1b =  0;  d1e = Ni;  d1 = +1;
               d2b =  0;  d2e = Nj;  d2 = +1;
               d3b =  0;  d3e = Nk;  d3 = +1;
               I3_to_1 = &SUMA_ijk_to1;
               break;
            case 1:
               d1b = Ni;  d1e =  0;  d1 = -1;
               d2b =  0;  d2e = Nj;  d2 = +1;
               d3b =  0;  d3e = Nk;  d3 = +1;
               I3_to_1 = &SUMA_ijk_to1;
               break;
            case 2:
               d1b =  0;  d1e = Nj;  d1 = +1;
               d2b =  0;  d2e = Ni;  d2 = +1;
               d3b =  0;  d3e = Nk;  d3 = +1;
               I3_to_1 = &SUMA_jik_to1;
               break;
            case 3:
               d1b = Nj;  d1e =  0;  d1 = -1;
               d2b =  0;  d2e = Ni;  d2 = +1;
               d3b =  0;  d3e = Nk;  d3 = +1;
               I3_to_1 = &SUMA_jik_to1;
               break;
            case 4:
               d1b =  0;  d1e = Nk;  d1 = +1;
               d2b =  0;  d2e = Nj;  d2 = +1;
               d3b =  0;  d3e = Ni;  d3 = +1;
               I3_to_1 = &SUMA_kji_to1;
               break;
            case 5:
               d1b = Nk;  d1e =  0;  d1 = -1;
               d2b =  0;  d2e = Nj;  d2 = +1;
               d3b =  0;  d3e = Ni;  d3 = +1;
               I3_to_1 = &SUMA_kji_to1;
               break;
            default:
               SUMA_S_Err("palinmoment");
               SUMA_RETURN(0);

         }
         for (k=d3b; k<d3e; k = k+d3) {
            for (j=d2b; j<d2e; j = j+d2) {
               trg = -1; cnd = -1; cndval = 0; aprev = 0; 
               for (i=d1b; i<d1e; i = i+d1) {
                  v = I3_to_1(i,j,k, Ni, Nij);
                  if (trg < 0 && ba[v]) { 
                     trg = 0; /* you're in, target holes*/ 
                  } else if (trg == 0 && !ba[v]) {
                     /* found a candidate on the edge */
                     if (cnd >= 0) { SUMA_S_Err("SHOULD NOT BE"); };
                     cnd = v; cndval = aprev; trg = 1; /* next seek non zero */
                  } else if (trg == 1 && ba[v]) {
                     if (cnd >= 0) { /* have candidate with data after it */
                        sum[cnd] += cndval; /* add candidate value */
                        hits[cnd] += 1;
                     }
                     cnd = -1; cndval = 0; 
                     trg = 0; /* next we seek another hole */  
                  }
                  aprev = fa[v];  
               }
            }
         }
        
      }/* for each direction */
      /* Now put the candidates back into fa */
      Nfills = 0;
      for (v=0; v<Nijk; ++v) {
         if (hits[v]) {
               /* the next three statements used to be conditioned
               on hits[v] >= minhits, but minhits can not be used
               to control which kind of holes get filled. So it 
               is kind of useless */
               fa[v] = sum[v]/(float)hits[v]; 
               ba[v] = 1;
               ++Nfills;
            /* reset for next loop */
            sum[v] = 0.0; hits[v] = 0;
         }
      }
      ++iter;
      SUMA_LHv("Iter %d, N_fills  = %d\n", iter, Nfills);
   } while (Nfills > 0 && iter < maxiter);
   
   if (Nfills > 0) {
      SUMA_S_Warnv("Function stopped because of maximum iter limit of %d. "
                   "Holes may still exist.", maxiter);
   }
   SUMA_ifree(hits); SUMA_ifree(sum); SUMA_ifree(ba);      
   
   SUMA_RETURN(1);
}

/*!   
   Trace rays from voxel n1D in +i and -i directions.
   Return the first non zero value (ta) encountered and the number of
   voxels traveled (da) to get to it. 
   Results are stored in ta[0] and da[0] for +ve direction
   and ta[1] and da[1] for -ve direction.
*/  
int SUMA_ray_i(int n1D, int ni, int nij, float *av, byte *ba, 
               float ta[], int da[])
{
   static char FuncName[]={"SUMA_ray_i"};
   int IJK[3], ii, t1D;
   int hitcode;
   
   SUMA_ENTRY;
   
   ta[0] = ta[1] = 0.0;
   da[0] = da[1] = 0;
   hitcode = 0;
   
   Vox1D2Vox3D(n1D, ni, nij, IJK) 
   
   /* shoot ray in +ve direction */
   ii = IJK[0]; t1D = n1D;
   if (ii < ni && !ba[t1D]) {
      do {
         ++ii; ++t1D;
      } while (ii < ni && !ba[t1D]);  
      if (ii < ni) {
         ta[0] = av[t1D];  /* The value at the non zero in +ve i*/
         da[0] = ii-IJK[0];/* The number of voxels needed to get there*/
         hitcode = hitcode + SUMA_IposBOUND;
      }
   }
   
   /* shoot ray in -ve direction */
   ii = IJK[0]; t1D = n1D;
   if (-1 < ii && !ba[t1D]) {
      do {
         --ii; --t1D;
      } while (-1 < ii && !ba[t1D]);  
      if (-1 < ii) {
         ta[1] = av[t1D];  /* The value at the non zero in -ve i*/
         da[1] = IJK[0]-ii;/* The number of voxels needed to get there*/
         hitcode = hitcode + SUMA_InegBOUND;
      }
   }
   
   SUMA_RETURN(hitcode);
}

/*!   
   Trace rays from voxel n1D in +j and -j directions.
   Return the first non zero value (ta) encountered and the number of
   voxels traveled (da) to get to it. 
   Results are stored in ta[0] and da[0] for +ve direction
   and ta[1] and da[1] for -ve direction.
*/  
int SUMA_ray_j(int n1D, int ni, int nij, int nj, float *av, byte *ba, 
               float ta[], int da[])
{
   static char FuncName[]={"SUMA_ray_j"};
   int IJK[3], jj, t1D;
   int hitcode;
   
   SUMA_ENTRY;
   
   ta[0] = ta[1] = 0.0;
   da[0] = da[1] = 0;
   hitcode = 0;
   
   Vox1D2Vox3D(n1D, ni, nij, IJK) 
   
   /* shoot ray in +ve direction */
   jj = IJK[1]; t1D = n1D;
   if (jj < nj && !ba[t1D]) {
      do {
         ++jj; t1D = IJK[0]+jj*ni+IJK[2]*nij;
      } while (jj < nj && !ba[t1D]);  
      if (jj < nj) {
         ta[0] = av[t1D];  /* The value at the non zero in +ve j*/
         da[0] = jj-IJK[1];/* The number of voxels needed to get there*/
         hitcode = hitcode + SUMA_JposBOUND;
      }
   }
   
   /* shoot ray in -ve direction */
   jj = IJK[1]; t1D = n1D;
   if (-1 < jj && !ba[t1D]) {
      do {
         --jj; t1D = IJK[0]+jj*ni+IJK[2]*nij;
      } while (-1 < jj && !ba[t1D]);  
      if (-1 < jj) {
         ta[1] = av[t1D];  /* The value at the non zero in -ve j*/
         da[1] = IJK[1]-jj;/* The number of voxels needed to get there*/
         hitcode = hitcode + SUMA_JnegBOUND;
      }
   }
   
   SUMA_RETURN(hitcode);
}

/*!   
   Trace rays from voxel n1D in +k and -k directions.
   Return the first non zero value (ta) encountered and the number of
   voxels traveled (da) to get to it. 
   Results are stored in ta[0] and da[0] for +ve direction
   and ta[1] and da[1] for -ve direction.
*/  
int SUMA_ray_k(int n1D, int ni, int nij, int nk, float *av, byte *ba, 
               float ta[], int da[])
{
   static char FuncName[]={"SUMA_ray_k"};
   int IJK[3], kk, t1D;
   int hitcode;
   
   SUMA_ENTRY;
   
   ta[0] = ta[1] = 0.0;
   da[0] = da[1] = 0;
   hitcode = 0;
   
   Vox1D2Vox3D(n1D, ni, nij, IJK); 
   
   /* shoot ray in +ve direction */
   kk = IJK[2]; t1D = n1D;
   if (kk < nk && !ba[t1D]) {
      do {
         ++kk; t1D = IJK[0]+IJK[1]*ni+kk*nij;
      } while (kk < nk && !ba[t1D]);  
      if (kk < nk) {
         ta[0] = av[t1D];  /* The value at the non zero in +ve k*/
         da[0] = kk-IJK[2];/* The number of voxels needed to get there*/
         hitcode = hitcode + SUMA_kposBOUND;
      }
   }
   
   /* shoot ray in -ve direction */
   kk = IJK[2]; t1D = n1D;
   if (-1 < kk && !ba[t1D]) {
      do {
         --kk; t1D = IJK[0]+IJK[1]*ni+kk*nij;
      } while (-1 < kk && !ba[t1D]);  
      if (-1 < kk) {
         ta[1] = av[t1D];  /* The value at the non zero in -ve j*/
         da[1] = IJK[2]-kk;/* The number of voxels needed to get there*/
         hitcode = hitcode + SUMA_knegBOUND;
      }
   }
   
   SUMA_RETURN(hitcode);
}

/*! Find a vole in a volume. A hole voxel is a voxel
    with 0 value that is surrounded in at least one
    dimension by non zero voxels. */
int SUMA_find_hole_voxels( int Ni, int Nj, int Nk, 
                           float *fa, byte *ba, int *holeat)
{
   static char FuncName[]={"SUMA_find_hole_voxels"};
   int nh=0, v, Nij, Nijk;
   int da[2]; 
   float ta[2];
   
   SUMA_ENTRY;
   
   Nij = Ni*Nj; Nijk = Nij*Nk;
   
   nh = 0;
   for (v=0; v<Nijk; ++v) {
      if (ba[v]) continue; /* not a hole! */
      if (SUMA_ray_i(v, Ni, Nij, fa, ba, ta, da) 
                     == SUMA_I_HOLE) {
         holeat[nh] = v; ++nh;
         continue;
      } else if (SUMA_ray_j(v, Ni, Nij, Nj, fa, ba, ta, da) 
                     == SUMA_J_HOLE) {
         holeat[nh] = v; ++nh;
         continue;
      } else if (SUMA_ray_k(v, Ni, Nij, Nk, fa, ba, ta, da) 
                     == SUMA_K_HOLE) {
         holeat[nh] = v; ++nh;
         continue;
      }
   }
   
   SUMA_RETURN(nh);
}

/*!
   Record or return number of hits.
   a (int) action flag.
      0- Free temps.
      1- Add hit key
      2- Return hits of key
      3- Return key of max hits
      4- Reset hit record
      
*/      
int hits_rec(int a, int key)
{
   static int *keys=NULL;
   static int *hits=NULL;
   static int n_alloc=0, n=0;
   int mk = 0, mh = 0, i=0;
   
   if (a==1) { /* add hit */
      if (n>=n_alloc) {
         n_alloc += 100;
         keys = (int *)realloc(keys, n_alloc*sizeof(int));
         hits = (int *)realloc(hits, n_alloc*sizeof(int));
      }
      while (i<n && keys[i]!=key) ++i;
      if (i==n) { keys[i]=key; hits[i]=1; ++n;}
      else { ++hits[i]; }
   } else if (a==2) { /* return hits of key */
      for (i=0; i<n; ++i) {
         if (keys[i] == key) { return(hits[i]); }
      }
      return(-1);
   } else if (a==3 && n) { /* return key of max hits */
      mk = keys[0]; mh = hits[0];
      for (i=0; i<n; ++i) {
         if (hits[i] > mh) { mh = hits[i]; mk = keys[i]; }
      }
      return(mk);
   } else if (a==4) { /* reset */
      n = 0;
   } else if (a==0) { /* free */
      if (keys) free(keys); keys=NULL;
      if (hits) free(hits); hits=NULL;
      n = 0;
      n_alloc = 0;
   } 
   
   return(1);
    
}
/*!   A faster version of SUMA_mri_volume_infill
      linfill == 1 rescans volume for holes with each new iteration
                   Cautious approach, but wastes a lot of time.
              == 0 rescans volume for holes once all existing 
                   holes are filled. Faster, but slightly
                   different model. 
         Relatively few voxels are affected by linfill
*/
                    
int SUMA_mri_volume_infill_zoom(MRI_IMAGE *imin, byte linfill, 
                                int integ, int umaxiter) 
{
   static char FuncName[]={"SUMA_mri_volume_infill_zoom"};
   int Ni, Nj, Nk, Nij, Nijk, v, iter;
   int Nfills, h, nh, hitcode, hitsum, da[2];
   byte *ba=NULL, *nhits=NULL;
   float *fa=NULL, ta[2];
   float *sum=NULL;
   int *holeat = NULL, *intar=NULL;
   int maxiter= 500; 
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (umaxiter > 0) maxiter = umaxiter;
   
   if (integ != 0 && integ != 1) {
      SUMA_S_Warnv("Function only accepts integ of 0 or 1. \n"
                   "Received %d but will proceed with 0.\n",
                   integ);
      integ = 0;
   }
   
   Ni = imin->nx; Nj = imin->ny; Nk = imin->nz; 
   Nij = Ni*Nj; Nijk = Nij*Nk;
   
   fa = MRI_FLOAT_PTR(imin);
   
   /* make byte mask */
   ba = (byte *)SUMA_calloc(Nijk, sizeof(byte));
   for (v=0; v<Nijk; ++v) {
      if (SUMA_ABS(fa[v]-0.0f)>0.00001) ba[v] = 1; 
   }
   holeat = (int *)SUMA_calloc(Nijk, sizeof(int));
   sum = (float *)SUMA_calloc(Nijk, sizeof(float));
   nhits = (byte *)SUMA_calloc(Nijk, sizeof(byte));
   if (integ) intar  = (int *)SUMA_calloc(Nijk, sizeof(int));
   
   if (integ) hits_rec(0, 0); /* clean start */
   
   iter = 0; nh = 0;
   do {
      if (linfill || !iter) {
               /* To ensure that the final volume has no holes in it, 
               you must rescan the whole volume, as opposed to the one 
               pass scan performed only at iteration 0. 
               Two ways to handle this problem. Either repeat a full scan
               with each iteration (linfill = 1) and waste a lot of time.
               With linfill = 1, the function is as slow as the older 
               version SUMA_mri_volume_infill.
               Alternately (linfill = 0), we do a new scan once the first set 
               of holes are filled. That's much faster.  
               The two approaches are not equivalent,
               but very few voxels are affected */

         /* find all hole points */
         nh = SUMA_find_hole_voxels(Ni, Nj, Nk, fa, ba, holeat);
      }
      
      Nfills = 0;
      for (h=0; h<nh; ++h) {
         hitcode = 0; sum[holeat[h]] = 0.0; nhits[holeat[h]]=0;
         hitsum=0;
         if ( (hitcode = SUMA_ray_i(holeat[h], Ni, Nij, fa, ba, ta, da)) == 
               SUMA_I_HOLE) {
            hitsum += hitcode;
            if (da[0] == 1) { /* only process holes at edge */
               sum[holeat[h]] += ta[0]; /* add value at edge voxel */
               if (integ) hits_rec(1, (int)ta[0]);
               ++nhits[holeat[h]];
            }
            if (da[1] == 1) { /* only process holes at edge */
               sum[holeat[h]] += ta[1]; /* add value at edge voxel */
               if (integ) hits_rec(1, (int)ta[1]);
               ++nhits[holeat[h]];
            }
         }
         #if 0
            if (holeat[h] == 1007828) {
               SUMA_S_Notev("Vox %d, iter %d, nhit=%d, hitsum=%d\n"
                            "iRay da=[%d %d], ta=[%f %f]\n",
                     holeat[h], iter, nhits[holeat[h]], hitcode, 
                     da[0], da[1], ta[0], ta[1]);
            }
         #endif
         if ( (hitcode = SUMA_ray_j(holeat[h], Ni, Nij, Nj, fa, ba, ta, da)) == 
               SUMA_J_HOLE) {
            hitsum += hitcode;
            if (da[0] == 1) { /* only process holes at edge */
               sum[holeat[h]] += ta[0]; /* add value at edge voxel */
               if (integ) hits_rec(1, (int)ta[0]);
               ++nhits[holeat[h]];
            }
            if (da[1] == 1) { /* only process holes at edge */
               sum[holeat[h]] += ta[1]; /* add value at edge voxel */
               if (integ) hits_rec(1, (int)ta[1]);
               ++nhits[holeat[h]];
            }
         }
         if ( (hitcode = SUMA_ray_k(holeat[h], Ni, Nij, Nk, fa, ba, ta, da)) == 
               SUMA_K_HOLE) {
            hitsum += hitcode;
            if (da[0] == 1) { /* only process holes at edge */
               sum[holeat[h]] += ta[0]; /* add value at edge voxel */
               if (integ) hits_rec(1, (int)ta[0]);
               ++nhits[holeat[h]];
            }
            if (da[1] == 1) { /* only process holes at edge */
               sum[holeat[h]] += ta[1]; /* add value at edge voxel */
               if (integ) hits_rec(1, (int)ta[1]);
               ++nhits[holeat[h]];
            }
         }
         if (integ) {
            if (nhits[holeat[h]]) intar[holeat[h]] = hits_rec(3,0);
            hits_rec(4,0); /* reset */
         }
         
      }
      /* now update holeat array */
      h = 0;
      while (nh > 0 && h<nh) {
         if (nhits[holeat[h]]) {
            if (integ) {
               fa[holeat[h]] = (float)intar[holeat[h]]; /* get most freq. key */
            } else {
               fa[holeat[h]] = 
                  sum[holeat[h]]/(float)nhits[holeat[h]]; /* assign new value */
            }
            ba[holeat[h]] = 1;   /* mark as filled */
            
            nhits[holeat[h]] = 0;   /* reset */
            sum[holeat[h]] = 0; 
            
            ++Nfills;   /* keep track of numbers filled  */
            
            holeat[h] = holeat[nh-1]; /* reduce list of holes */
            --nh;
         } else {
            ++h;
         }
      }
      ++iter;
      SUMA_LHv("Iter %d, N_fills = %d, %d holes remain\n", iter, Nfills, nh);
      if (nh == 0 && iter < maxiter && !linfill) {
         /* if nh, rerun full scan to ensure no new voxels qualify as holes.
            This is not necessary if linfill is used because a rescan is carried
            out with each iteration */
         nh = SUMA_find_hole_voxels(Ni, Nj, Nk, fa, ba, holeat);
         if (nh) {
            SUMA_LHv("%d more holes in patched version, one more pass\n", nh);
         }
      }
   } while (nh > 0 && iter < maxiter);
   
   if (nh > 0) {
      SUMA_S_Warnv("Function stopped because of maximum iter limit of %d. "
                   "%d holes still exist." , maxiter ,nh);
   }
   
   SUMA_ifree(holeat); SUMA_ifree(ba);      
   hits_rec(0,0); /* cleanup */
   
   SUMA_RETURN(1);
}

/*!   
   A brutish filler function, filling interpolation is crude
*/
                    
int SUMA_mri_volume_infill_solid(MRI_IMAGE *imin, int minhits) 
{
   static char FuncName[]={"SUMA_mri_volume_infill_solid"};
   int Ni, Nj, Nk, Nij, Nijk, v;
   int hitcode, hitsum, da[2];
   byte *ba=NULL;
   float *fa=NULL, *fan=NULL, ta[2];
   float  sI, sK, sJ, nhits=0.0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (minhits <= 0) minhits = 1;
   
   Ni = imin->nx; Nj = imin->ny; Nk = imin->nz; 
   Nij = Ni*Nj; Nijk = Nij*Nk;
   
   fa = MRI_FLOAT_PTR(imin);
   fan = (float *)SUMA_calloc(Nijk, sizeof(float));
   
   /* make byte mask, 1 where there is value, 0 otherwise */
   ba = (byte *)SUMA_calloc(Nijk, sizeof(byte));
   for (v=0; v<Nijk; ++v) {
      if (SUMA_ABS(fa[v]-0.0f)>0.00001) ba[v] = 1; 
   }
   

   for (v=0; v<Nijk; ++v) {
      if (ba[v]) continue; /* not a hole */
      hitcode = 0; nhits=0.0;
      hitsum=0; sI=0.0; sJ=0.0; sK=0.0;
      if ( (hitcode = SUMA_ray_i(v, Ni, Nij, fa, ba, ta, da)) == 
            SUMA_I_HOLE) {
         hitsum += hitcode;
         sI = (ta[0]*da[1]+ta[1]*da[0])/(da[1]+da[0]); 
         ++nhits;
      }
      if ( (hitcode = SUMA_ray_j(v, Ni, Nij, Nj, fa, ba, ta, da)) == 
            SUMA_J_HOLE) {
         hitsum += hitcode;
         sJ = (ta[0]*da[1]+ta[1]*da[0])/(da[1]+da[0]);
         ++nhits;
      }
      if ( (hitcode = SUMA_ray_k(v, Ni, Nij, Nk, fa, ba, ta, da)) == 
            SUMA_K_HOLE) {
         hitsum += hitcode;
         sK = (ta[0]*da[1]+ta[1]*da[0])/(da[1]+da[0]);
         ++nhits;
      }
      if (nhits >= minhits) {
         fan[v] = (sI + sK + sJ) / nhits;
         /* SUMA_LHv("At vox %d: Got me %d hits of code %d and val %f\n",
                  v, (int)nhits, hitsum, fan[v]); */
      }
   }
   
   for (v=0; v<Nijk; ++v) {
      if (!ba[v] && fan[v] != 0.0f) { fa[v] = fan[v]; }
   }   
   
   SUMA_ifree(ba); SUMA_ifree(fan);     
   
   SUMA_RETURN(1);
}

int SUMA_VolumeInFill(THD_3dim_dataset *aset,
                      THD_3dim_dataset **filledp,
                      int method, int integ,
                      int MxIter, int minhits) 
{
   static char FuncName[]={"SUMA_VolumeInFill"};
   float *fa=NULL;
   THD_3dim_dataset *filled = *filledp;   
   MRI_IMAGE *imin=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (minhits > 0 && method != 2) {
      SUMA_S_Err("minhits is only useful with method = 2.\n");
   }
   
   if (integ < 0) { /* figure it out */
      if (is_integral_dset(aset,1)) integ = 1;
      else integ = 0;
   }
      
   /* get data into float image */
   imin = THD_extract_float_brick(0,aset) ;
   if (method == 0) { /* slow */
      if (!SUMA_mri_volume_infill(imin)) {
         SUMA_S_Err("Failed to fill volume");
         SUMA_RETURN(0);
      }
   } else if (method == 1) { /* faster */
      if (!SUMA_mri_volume_infill_zoom(imin, 0, integ, MxIter)) {
         SUMA_S_Err("Failed to fill volume");
         SUMA_RETURN(0);
      }
   } else if (method == 2){ /* solid */
      if (!SUMA_mri_volume_infill_solid(imin, minhits)) {
         SUMA_S_Err("Failed to fill volume");
         SUMA_RETURN(0);
      }
   }
   
   /* put results in dset */   
   fa = MRI_FLOAT_PTR(imin);
   
   /* Put result in output dset */
   if (!filled) {
      filled = EDIT_full_copy(aset, FuncName);
      *filledp = filled;
   }
   EDIT_substscale_brick(  filled, 0, MRI_float, fa, 
                           DSET_BRICK_TYPE(filled,0), -1.0);
   EDIT_BRICK_LABEL(filled,0,"HolesFilled"); 
   if (DSET_BRICK_TYPE(filled,0) == MRI_float) {
      mri_clear_and_free(imin);  /* pointer was recycled */
   } else {
     mri_free(imin);  
   }
   imin = NULL; fa = NULL; 
   if (integ) { /* copy attributes, if any */
      THD_copy_labeltable_atr( filled->dblk,  aset->dblk);          
   }

   SUMA_RETURN(1);
}

/*! 
   A local stat moving average blurring of each sub-brick inside mask .
   This was tested only once and FWHM is not handled properly.
   It just uses a sphere of radius FWHM/2, but it is much slower
   than SUMA_VolumeBlurInMask, so fughet about it
*/
int SUMA_VolumeLSBlurInMask(THD_3dim_dataset *aset, 
                                  byte *cmask,
                                  THD_3dim_dataset **blurredp,
                                  float FWHM, float mxvx) 
{
   static char FuncName[]={"SUMA_VolumeLSBlurInMask"};
   int  sb = 0, nx_in, nxy_in,
         nx, ny, nz, ih, nxyz_o;
   MRI_vectim *vecim=NULL;
   float *mm=NULL, dx , dy , dz, na, redx[3];
   THD_3dim_dataset *blurred= NULL, *tset=NULL;
   MCW_cluster *nbhd=NULL ;
   float *fa=NULL;
   MRI_IMAGE *imin=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_S_Warn("This function should not be used");
   SUMA_RETURN(0);
   
   if( FWHM < 0.0f ){ dx = dy = dz = 1.0f ; FWHM = -FWHM ; }
   else         { dx = fabsf(DSET_DX(aset)) ;
                  dy = fabsf(DSET_DY(aset)) ;
                  dz = fabsf(DSET_DZ(aset)) ; }
   nx = DSET_NX(aset); ny = DSET_NY(aset); nz = DSET_NZ(aset);
   
   na = FWHM/2.0;
   nbhd = MCW_spheremask( dx,dy,dz , na ) ;
   redx[0] = redx[1] = redx[2] = 1.0;
   if (mxvx) {
      if (na/dx > mxvx) redx[0] = na / (mxvx * dx);
      if (na/dy > mxvx) redx[1] = na / (mxvx * dy);
      if (na/dz > mxvx) redx[2] = na / (mxvx * dz);
   }
   

   /* output dset */
   if (!(blurred  = THD_reduced_grid_copy(aset, redx))) {
      SUMA_S_Err("Failed to create output dset");
      SUMA_RETURN(0);
   }
   nxyz_o = DSET_NVOX(blurred);
   if (LocalHead) {
      SUMA_S_Notev("nbhd: %p %d voxels.\n"
                   "redx = %f %f %f, Reduction from %d to %d voxels\n",
                     nbhd, nbhd->num_pt,
                     redx[0], redx[1], redx[2], nx*ny*nz, nxyz_o);
   }
   nx_in = DSET_NX(aset);
   nxy_in = nx_in*DSET_NY(aset);
   for (sb=0; sb<DSET_NVALS(aset); ++sb) {
      if (!mm) mm = (float *)calloc(nxyz_o, sizeof(float));
      imin = THD_extract_float_brick(sb,aset) ;
      fa = MRI_FLOAT_PTR(imin);   /* array of values */
AFNI_OMP_START ;
#pragma omp parallel if( nxyz_o > 500000 ) /* Does not offer much speedups */    
      {
         int  ijk_o, ijk_in, ii, jj, kk, 
                  nhood, *nind=NULL;
         float ws;
         if (!nind) nind = (int *)calloc(nbhd->num_pt, sizeof(int));
#pragma omp for
         for( ijk_o=0 ; ijk_o < nxyz_o ; ijk_o++ ){   /* parallelized loop */
            /* get ii, jj, kk on original resolution */
            DSET_1Dindex_to_regrid_ijk(blurred, ijk_o, aset, &ii, &jj, &kk);
            ijk_in = ii+jj*nx_in+kk*nxy_in;
            if (IN_MASK(cmask,ijk_in)) { /* get a mask for that location */
               nhood = mri_load_nbhd_indices( DSET_BRICK(aset , sb ) ,
                                 cmask , ii,jj,kk , nbhd, nind); 
                              /* nhood will not be constant, 
                                 when you are close to mask's edge */
               ws = (float)nhood+1.0;
               mm[ijk_o] = fa[ijk_in];
               for (ih=0; ih<nhood; ++ih) {
                  mm[ijk_o] += fa[nind[ih]];
               }
               mm[ijk_o] = mm[ijk_o]/ws;
            } /* in mask */
         }
         SUMA_ifree(nind); nind = NULL;
      }
AFNI_OMP_END ;
      /* Stick mm back into dset */
      EDIT_substscale_brick(blurred, sb, MRI_float, mm, 
                            DSET_BRICK_TYPE(blurred,sb), -1.0); 
      if (DSET_BRICK_TYPE(blurred,sb) != MRI_float) free(mm);
      mm = NULL;
      EDIT_BRICK_LABEL(blurred,sb,"LSBlurredInMask");      
      mri_free(imin); imin = NULL;
   } /* sb */
   /* now resample back to original resolution */
   tset = r_new_resam_dset( blurred, aset, 0.0, 0.0, 0.0,
                                 NULL, resam_str2mode("Linear"), NULL, 1, 1);
   if (*blurredp) DSET_delete(*blurredp) ; *blurredp = tset; tset = NULL;
   SUMA_RETURN(1);
}

/*! 
   Blur each sub-brick inside mask.
    if unifac = 0.0 : Auto brick factor for each sub-brick (safest)
              > 0.0 : Use unifac for all sub-brick factors
              = -1.0: Auto factor for 0th brick, other bricks
                     get same factor.
*/
int SUMA_VolumeBlurInMask(THD_3dim_dataset *aset,
                                     byte *cmask,
                                     THD_3dim_dataset **blurredp,
                                     float FWHM, float unifac, 
                                     int speed) 
{
   static char FuncName[]={"SUMA_VolumeBlurInMask"};
   float fac = 0.0, *fa=NULL;
   MRI_IMAGE *imin=NULL;
   int k=0, nfloat_err=0;
   THD_3dim_dataset *blurred = *blurredp;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   /* get data into float image, preserve scale */
   fac = -1.0;
   for (k=0; k<DSET_NVALS(aset); ++k) {
      imin = THD_extract_float_brick(k,aset) ;
      
      if (LocalHead && 
          (nfloat_err = thd_floatscan( imin->nvox , MRI_FLOAT_PTR(imin) ))) {
      WARNING_message("You have %d bad numbers in dset to be blurred\n"
                      "Blurring output might be corrupted.", nfloat_err);
      }
   

      if (speed == 2) {
         /* selection of number of reps is a crapshoot here 
            Need to do better, and take into account voxel 
            resolution and uneven sizes...*/
         mri_blur3D_inmask_NN( imin, cmask, (int)(3.0*FWHM) );
      } else if (speed == 1) {
         mri_blur3D_addfwhm_speedy(imin, cmask, FWHM);
      } else {
         SUMA_S_Note("Going the slow route");
         mri_blur3D_addfwhm(imin, cmask, FWHM);
      }
      
      /* put results in dset */   
      fa = MRI_FLOAT_PTR(imin);

      /* Put result in output dset */
      if (!blurred) {
         blurred = EDIT_full_copy(aset, FuncName);
         *blurredp = blurred;
      }
      
      if (unifac > 0.0) fac = unifac;
      else if (unifac == -1.0) {
         if (k==0) fac = -1.0; /* auto at 1st sub-brick */
         else fac = DSET_BRICK_FACTOR(blurred, k - 1);
      } else fac = -1.0;
      
      SUMA_LHv("aset %s, k %d, fac %f\n", 
            DSET_PREFIX(aset), k, fac);
      EDIT_substscale_brick(blurred, k, MRI_float, fa, 
                              DSET_BRICK_TYPE(blurred,k), fac);
      if (DSET_BRICK_TYPE(blurred,k) == MRI_float) {
         mri_clear_and_free(imin); /* data pointer was recycled */
      } else {
         mri_free(imin);
      }
      imin = NULL; fa = NULL;
      EDIT_BRICK_LABEL(blurred,k,"BlurredInMask"); 
   }
   
   SUMA_RETURN(1);
}

/*! 
   Blur the volume with AFNI's EDIT_blur_volume 
*/
int SUMA_VolumeBlur(THD_3dim_dataset *aset,
                   byte *cmask,
                   THD_3dim_dataset **blurredp,
                   float FWHM) 
{
   static char FuncName[]={"SUMA_VolumeBlur"};
   float *fa=NULL;
   MRI_IMAGE *imin=NULL;
   int k=0, iii, n_avg=0;
   double avg;
   THD_3dim_dataset *blurred = *blurredp;
   EDIT_options *edopt=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!blurred) {
      blurred = EDIT_full_copy(aset, FuncName);
      *blurredp = blurred;
   } 
   
   /* copy data into output and plug masked areas with mean of dset */
   for (k=0; k<DSET_NVALS(aset); ++k) {
      /* padd masked region with average over image */
      imin = THD_extract_float_brick(k,aset) ;
      fa = MRI_FLOAT_PTR(imin);
      for (iii=0; iii<DSET_NVOX(aset); ++iii) {
         avg = 0;
         if (IN_MASK(cmask, iii)) { avg += fa[iii]; ++n_avg; }
         avg /= (double)n_avg;
      }
      for (iii=0; iii<DSET_NVOX(aset); ++iii) {
         if (!IN_MASK(cmask, iii)) fa[iii] = (float)avg;
      }
      EDIT_substscale_brick(blurred, k, 
                     MRI_float, fa,
                     DSET_BRICK_TYPE(blurred,k), DSET_BRICK_FACTOR(aset,k));  
      if (DSET_BRICK_TYPE(blurred,k) == MRI_float) {
         mri_clear_and_free(imin);
      } else {
         mri_free(imin);
      }
      imin = NULL; fa = NULL;
      EDIT_BRICK_LABEL(blurred,k,"BlurredNoMask"); 
   }
   
   edopt = SUMA_BlankAfniEditOptions();
   edopt->blur = FWHM_TO_SIGMA(FWHM);
   if (debug > 1) edopt->verbose = 1;
   EDIT_blur_allow_fir(0) ;
   EDIT_one_dataset( blurred , edopt);
   SUMA_free(edopt);
    
   SUMA_RETURN(1);
}


/*!Energy assigned to the contrast between voxel values a0 and a1. 
   Sign of contrast is irrelevant
   Bigger contrast --> Higher energy
   Denominator is meant to neutralize the effect of bias field.
*/
   /* 
   EDGE_EN1: First pass, seems to work.  */
   #define EDGE_EN1(a1,a0, d0, d1) (SUMA_ABS((a1)-(a0))/((d1)+(d0)+0.01))
   #define EDGE_EN0(a1,a0) (SUMA_ABS((a1)-(a0)))
double SUMA_EdgeEnergy(short *a, float af, short *b, float bf,
                      int Ni, int Nj, int Nk,
                      short *c, short k1, short k2, 
                      byte *mask, SUMA_CLASS_STAT *cs,
                      int method, short *skel, 
                      int *n_en) {
   static char FuncName[]={"SUMA_EdgeEnergy"};
   int ii, jj, kk, Nij, Nijk, off, n1, n0;
   short c1, c2;
   double en = 0.0;
   
   SUMA_ENTRY;
   
   c1 = cs->keys[k1];
   c2 = cs->keys[k2];
   
   *n_en = 0;
   Nij = Ni*Nj; Nijk = Nij*Nk;
   if (skel) memset(skel, 0, Nijk*sizeof(short));

   if (!b) bf = 1.0;
   
   /* The i direction */
   for (kk=0; kk<Nk; ++kk) { for (jj=0; jj<Nj; ++jj) {
      off = jj*Ni+kk*Nij;
      for (ii=1; ii<Ni; ++ii) {
         n1 = ii+off; n0 = n1-1;
         if ( IN_MASK(mask, n1) &&
              IN_MASK(mask, n0) &&
              c[n1] != c[n0]              &&
             (c[n1] == c1 || c[n1] == c2) &&
             (c[n0] == c1 || c[n0] == c2) ) {
            if (skel) {
               skel[n1] = c[n1]; skel[n0] = c[n0];
            }
            
            switch (method) {
               case 1:
                  /* Passing bias estimate in denominator, not
                  particularly exciting since those can be lousy
                  Has been tested in no bias case so far and works ok,
                  also works well in presence of bias field, even when
                  field is ignored. 
                  Looking at edges in presence of bias field shows that 
                  results not all that sensitive to bias. Though edges in 
                  example below are not along boundaries, bias effect should
                  be comparable:
                  3dcalc   -a banat+orig. \
                           -b 'a[-1,0,0,0]' -c 'a[1,0,0,0]' -d 'a[0,-1,0,0]' \
                           -e 'a[0,1,0,0]' -f 'a[0,0,-1,0]' -g 'a[0,0,1,0]' \
                           -h banat.ns+orig. \
                           -expr '(a-(b+c+d+e+f+g)/6)*step(h)' -prefix 'contr'
                  
                  3dcalc   -a banat+orig. \
                           -b 'a[-1,0,0,0]' -c 'a[1,0,0,0]' -d 'a[0,-1,0,0]' \
                           -e 'a[0,1,0,0]' -f 'a[0,0,-1,0]' -g 'a[0,0,1,0]' \
                           -h banat.ns+orig. \
                        -expr '(a-(b+c+d+e+f+g)/6)/(a+(b+c+d+e+f+g)/6)*step(h)'\
                        -prefix 'contr_rat'
                  */
                  if (b) { en += EDGE_EN1(a[n1], a[n0], b[n1], b[n0]); }
                  else   { en += EDGE_EN0(a[n1], a[n0]); }
                  break;
               case 2:
                  /* (a-b)/(a+b) is independent of bias field.
                  However the ratio changes the energy rankings 
                  from (a-b) alone. The energy sum is then multiplied
                  by (Mean(a)+Mean(b). Works OK too, but not better than 1*/
                  en += EDGE_EN1(a[n1], a[n0], a[n1], a[n0]); 
                  break;
            }
            ++(*n_en);
            /*fprintf(stderr,"%d %d, %d, %d, %f, %f, %f\n",
                        a[n1], a[n0], b[n1], b[n0], af, bf,
                        EDGE_EN1(a[n1], a[n0], b ? b[n1]:1.0, b ? b[n0]:1.0)); */
         }
      }
   } }
   
   /* the j direction */
   for (kk=0; kk<Nk; ++kk) { for (ii=0; ii<Ni; ++ii) {
      off = ii+kk*Nij;
      for (jj=1; jj<Nj; ++jj) {
         n1 = (jj*Ni)+off; n0 = n1-Ni;
         if ( IN_MASK(mask, n1) &&
              IN_MASK(mask, n0) &&
              c[n1] != c[n0]              &&
             (c[n1] == c1 || c[n1] == c2) &&
             (c[n0] == c1 || c[n0] == c2) ) {
            if (skel) {
               skel[n1] = c[n1]; skel[n0] = c[n0];
            }
            switch (method) {
               case 1:
                  if (b) { en += EDGE_EN1(a[n1], a[n0], b[n1], b[n0]); }
                  else   { en += EDGE_EN0(a[n1], a[n0]); }
                  break;
               case 2:
                  en += EDGE_EN1(a[n1], a[n0], a[n1], a[n0]);
                  break;
            }      
            ++(*n_en);
         }
      }
   } }
      
   /* the k direction */
   for (ii=0; ii<Ni; ++ii) { for (jj=0; jj<Nj; ++jj) { 
      off = ii+jj*Ni;
      for (kk=1; kk<Nk; ++kk) {
         n1 = (kk*Nij)+off; n0 = n1-Nij;
         if ( IN_MASK(mask, n1) &&
              IN_MASK(mask, n0) &&
              c[n1] != c[n0]              &&
             (c[n1] == c1 || c[n1] == c2) &&
             (c[n0] == c1 || c[n0] == c2) ) {
            if (skel) {
               skel[n1] = c[n1]; skel[n0] = c[n0];
            }
            switch (method) {
               case 1:
                  if (b) { en += EDGE_EN1(a[n1], a[n0], b[n1], b[n0]); }
                  else   { en += EDGE_EN0(a[n1], a[n0]); }
                  break;
               case 2:
                  en += EDGE_EN1(a[n1], a[n0], a[n1], a[n0]);
                  break;
            }  
            ++(*n_en);
         }
      }
   } }
   
   en *= af/bf;
    
   switch (method) {
      case 1:
         en = 2.0* en / SUMA_ABS( SUMA_get_Stat(cs, cs->label[k2], "mean")-
                                  SUMA_get_Stat(cs, cs->label[k1], "mean") );
         break;
      case 2:
         en = en * (SUMA_get_Stat(cs, cs->label[k2], "mean")+
                    SUMA_get_Stat(cs, cs->label[k1], "mean") );
         break;
      default:
         SUMA_S_Errv("Stupid method %d\n", method);
         SUMA_RETURN(0);
   }
   SUMA_RETURN(en);
}

double SUMA_DsetEdgeEnergy(THD_3dim_dataset *aset,
                      THD_3dim_dataset *cset,
                      byte *mask, 
                      THD_3dim_dataset *fset,
                      THD_3dim_dataset *skelset,
                      SUMA_CLASS_STAT *cs, int method,
                      int *UseK, int N_kok)
{
   static char FuncName[]={"SUMA_DsetEdgeEnergy"};
   double en=0.0, env[64];
   short *a=NULL;
   short *f=NULL;
   short *c=NULL;
   short *skel=NULL;
   float af=1.0, ff= 1.0;
   int c1,c2, ke, n_env[64], n_en=0, sum_n_en=0, kc1, kc2;
   char slab[256];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (fset) {
      f = (short *)DSET_ARRAY(fset, 0);
      ff = DSET_BRICK_FACTOR(fset,0); if (ff == 0.0) ff = 1.0;
   } else {
      f = NULL;
   }
   a = (short *)DSET_ARRAY(aset, 0);
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0) af = 1.0;
   c = (short *)DSET_ARRAY(cset,0);
   
   ke = 0; sum_n_en = 0.0;
   for (kc1=0; kc1<N_kok; ++kc1) {
      for (kc2=kc1+1; kc2<N_kok; ++kc2) {
         c1 = UseK[kc1]; c2 = UseK[kc2];
         snprintf(slab,64,"%s-e-%s", cs->label[c1], cs->label[c2]);
         if (skelset) {
            skel = (short *)DSET_ARRAY(skelset, ke);
            EDIT_BRICK_LABEL(skelset,ke,slab);
         }
         
         en = SUMA_EdgeEnergy(a, af, f, ff,
                  DSET_NX(aset), DSET_NY(aset), DSET_NZ(aset),
                  c, c1, c2, mask, cs, method, skel,  &n_en);
         env[ke] = en;
         n_env[ke] = n_en; sum_n_en += n_en; 
         SUMA_LHv("%s %s:     Energy %f, Nedges %d,         E/N = %f\n",  
                           cs->label[c1], cs->label[c2], 
                           en,  n_en,
                           env[ke]/(float)n_en);
         ++ke;    
      }
   }
   
   /* combine energies from each combination, 
      weigh by number of edges involved */

   en = 0;
   for (c1=0; c1<ke; ++c1) {
      en += env[c1]/(double)n_env[c1];
   }
   SUMA_RETURN(en);
}

THD_3dim_dataset *SUMA_estimate_bias_field (SEG_OPTS *Opt, 
                                       int polorder,
                                       THD_3dim_dataset *aset,
                                       THD_3dim_dataset *cset,
                                       THD_3dim_dataset *pset,
                                       THD_3dim_dataset *pout) {
   static char FuncName[]={"SUMA_estimate_bias_field"};
   int i, j,k, dtable_key[1024], N_g[256], N_mm = 0;
   THD_3dim_dataset *dmset=NULL;
   float *dmv=NULL, FWHMbias=0.0;
   short *c=NULL, *a=NULL;
   char *str_lab=NULL;
   byte *mm=NULL;
   NI_str_array *bc=NULL, *bsc=NULL;
   MRI_IMAGE *imout=NULL, *imin=NULL;
   double M_dg=0.0, M_v[256];
   Dtable *vl_dtable=NULL;   
   
   SUMA_ENTRY;
   
   /* checks */
   if (!aset || !cset || !Opt->clss || !Opt->bias_classes) {
      SUMA_S_Errv("Bad input %p %p %p %p\n", 
                  aset, cset, Opt->clss, Opt->bias_classes);
      SUMA_RETURN(NULL);
   }
   
   /* init */
   if (!pout) { 
      NEW_SHORTY(aset, 1, Opt->frefix, pout);
   }
   if (!pout) SUMA_RETURN(NULL);
   if( !THD_ok_overwrite() && THD_is_file( DSET_HEADNAME(pout) ) ){
      SUMA_S_Warnv("Output file %s already exists and not in overwrite mode!\n",
                  DSET_HEADNAME(pout) ) ;
   }

   if (polorder < 0) polorder = Opt->bias_param;
   if (polorder < 0) {
      SUMA_S_Err("Failed to set polorder");
      SUMA_RETURN(NULL);
   }
   
   /* How many groups of classes do we have? */
   vl_dtable = SUMA_LabelsKeys2Dtable( Opt->clss->str, Opt->clss->num,  
                                       Opt->keys);
   c = (short *)DSET_ARRAY(cset, 0);
   a = (short *)DSET_ARRAY(aset, 0);
   mm = (byte *)calloc (DSET_NVOX(cset), sizeof(byte));
   bc = NI_strict_decode_string_list(Opt->bias_classes,";");
   Opt->N_biasgroups = bc->num;
   for (i=0; i<Opt->N_biasgroups; ++i) {
      if (Opt->debug > 1) 
         fprintf(stderr,"   Group %s %d/%d\n", bc->str[i], i+1, bc->num);
      /* which keys belong to this group ? */
      bsc = NI_strict_decode_string_list(bc->str[i],", ");
      for (j=0; j<bsc->num; ++j) {
         if (Opt->debug > 1)
            fprintf(stderr,"     Sub Group %s %d/%d\n", 
                           bsc->str[j], j+1, bsc->num);
         if ((dtable_key[j] = SUMA_KeyofLabel_Dtable(vl_dtable, bsc->str[j]))<0){
            SUMA_S_Errv("Failed to find bias label %s in table", bsc->str[j]); 
            SUMA_RETURN(NULL);
         } 
      }
      NI_delete_str_array(bsc );
      /* mark the class group */
      for (j=0; j<bsc->num; ++j) {
         for (k=0; k<DSET_NVOX(aset);++k) {
            if (  c[k] == dtable_key[j] && 
                  IN_MASK(Opt->cmask,k) )  { mm[k] = i+1; ++N_mm; } 
         }
      }
   }
   NI_delete_str_array(bc );
   destroy_Dtable(vl_dtable); vl_dtable=NULL;
   
   /* store mask for debugging and some stats later on*/
   {
      short *am=NULL;
      if (!Opt->gset) {
         NEW_SHORTY(aset, 1, "bias_estimate_groups", Opt->gset);
      }
      am = (short *)DSET_ARRAY(Opt->gset,0);
      for (i=0; i<DSET_NVOX(aset);++i) {
         am[i]=mm[i];
      }
   }
   if (Opt->debug > 1) {
      DSET_quiet_overwrite(Opt->gset); 
   }
   
   /* calculate the mean (unscaled) for each group */
   if (!group_mean(Opt, aset, mm, Opt->pweight ? pset:NULL, 
                   Opt->N_biasgroups, M_v, 1)) {
      ERROR_exit("Could not calculate scaled mean\n");
   }

   /* Create a demeaned version of the data */
   imin = THD_extract_float_brick(0,aset) ;
   dmv = MRI_FLOAT_PTR(imin) ;
   for (i=0; i<DSET_NVOX(aset);++i) {
      if (mm[i]) {
         dmv[i] /= M_v[mm[i]-1]; 
      } else {
         dmv[i] = 0.0;
      }
   }
   
   
   if (Opt->debug > 1) {/* store scaled intensities */
      THD_3dim_dataset *pbb=NULL;
      short *am=NULL;
      if (!pbb) {
         NEW_SHORTY(aset, 1, "bias_data", pbb);
      }
      am = (short *)DSET_ARRAY(pbb,0);
      for (i=0; i<DSET_NVOX(aset);++i) {
         am[i]=(short)(dmv[i]*10000.0);
      }
      EDIT_BRICK_FACTOR(pbb,0, 1.0/10000.0);
      DSET_quiet_overwrite(pbb); 
      DSET_delete(pbb); pbb=NULL;
   }
   
   FWHMbias = 25;
   
   if (1) {/* fill the thing, then blur*/
      byte *fm = (byte *)SUMA_calloc(DSET_NVOX(aset), sizeof(byte));
      if (Opt->debug > 1) SUMA_S_Note("Filling then blurring");
      SUMA_mri_volume_infill_zoom(imin, 0, 0, -1);
      for (i=0; i<DSET_NVOX(aset);++i) {
         if (SUMA_ABS(dmv[i]-0.0f)>0.00001) fm[i]=1;
      }
      mri_blur3D_addfwhm(imin, fm, FWHMbias); 
      SUMA_ifree(fm);
   } else {/* blur then do polyfit, bad fitting artifacts, not worth it*/ 
      if (Opt->debug > 1) SUMA_S_Note("Blurring then fitting with mri_polyfit");
      mri_blur3D_addfwhm(imin, mm, FWHMbias); 
      /* do the fit */
      mri_polyfit_verb(Opt->debug) ;
      if (!(imout = mri_polyfit( imin, polorder , mm , 0.0 , Opt->fitmeth )))  {
         ERROR_exit("Failed to fit");
      }
      dmv = MRI_FLOAT_PTR(imout) ;   
   }
   

   
   if (Opt->debug > 1) {/* save the fit */
      THD_3dim_dataset *pbb=NULL;
      short *am=NULL;
      if (!pbb) {
         NEW_SHORTY(aset, 1, "bias_estimate", pbb);
      }
      am = (short *)DSET_ARRAY(pbb,0);
      for (i=0; i<DSET_NVOX(aset);++i) {
         am[i]=(short)(dmv[i]*10000.0);
      }
      EDIT_BRICK_FACTOR(pbb,0, 1.0/10000.0);
      DSET_quiet_overwrite(pbb); 
      DSET_delete(pbb); pbb=NULL;
   }
   
   /* calculate average bias */
   M_dg = 0.0;
   for (i=0; i<DSET_NVOX(aset); ++i) { 
      if (mm[i]) M_dg += dmv[i]; 
   }
   M_dg /= (double)N_mm;  /* grand mean */
   
   

   /* scale bias by mean to make output image be closer to input */
   for (i=0; i<DSET_NVOX(aset); ++i) dmv[i] /= M_dg;
      
   /* store */
   EDIT_substscale_brick(pout, 0, MRI_float, dmv, MRI_short, -1.0);
   EDIT_BRICK_LABEL(pout,0,"BiasField");
   
   /* cleanup  */
   mri_free(imin); imin = NULL;
   if (imout) mri_free(imout); imout = NULL;
 

   SUMA_RETURN(pout);
}

int SUMA_Class_k_Label_Locator(SUMA_CLASS_STAT *cs, char *label) 
{
   static char FuncName[]={"SUMA_Class_k_Label_Locator"};
   int k=0;
   
   SUMA_ENTRY;
   
   if (!label) SUMA_RETURN(-1);
   
   while (k < cs->N_label) {
      if (!strcmp(cs->label[k],label)) SUMA_RETURN(k);   
      ++k;
   }
   
   SUMA_RETURN(-1);   
}

int SUMA_Class_k_Key_Locator(SUMA_CLASS_STAT *cs, int kk) 
{
   static char FuncName[]={"SUMA_Class_k_Key_Locator"};
   int k=0;
   
   SUMA_ENTRY;
   
   while (k < cs->N_label) {
      if (k == kk) SUMA_RETURN(k); 
      ++k;  
   }
   
   SUMA_RETURN(-1);   
}

/*!
   Find indices of particular classes in SUMA_CLASS_STAT
   \param cs: The classes statistics structure
   \param action: A string specifying what to do with the
                  'value' string
   \param value: A string of parameters, ';' separated 
                  for the 'action' string. If Value is NULL,
                  everything matches.
      action == "classes_string": Search for classes in 'value'
                "not_classes_string": Search for classes not in 'value'
   \param UseK: A vector of indices into classes matching the search.
                Can be NULL if you just care for the count.
   \return N_UseK: the number of classes found.
                   -1 in error
*/
int SUMA_Class_k_Selector(
   SUMA_CLASS_STAT *cs, char *action, char *value, int *UseK)
{
   static char FuncName[]={"SUMA_Class_k_Selector"};
   NI_str_array *bc=NULL;
   int k, ii, N_kok;
   
   SUMA_ENTRY;
   
   N_kok = -1;
   
   if (!strcmp(action, "classes_string")) {
      if (!value) {
         N_kok=cs->N_label;
         if (UseK) {
            for (k=0; k<N_kok; ++k) UseK[k] = k;
         }
      } else {
         bc = NI_strict_decode_string_list(value,";");
         N_kok=0;
         for (ii=0; ii < bc->num; ++ii) {
            for (k=0; k<cs->N_label; ++k) {
               if (!strcmp(bc->str[ii], cs->label[k])) {
                  if (UseK) { UseK[N_kok] = k; } 
                  ++N_kok; 
               }
            }
         }
         NI_delete_str_array(bc );
      }
      SUMA_RETURN(N_kok);
   }
   
   if (!strcmp(action, "not_classes_string")) {
      if (!value) {
         N_kok=cs->N_label;
         if (UseK) {
            for (k=0; k<N_kok; ++k) UseK[k] = k;
         }
      } else {
         bc = NI_strict_decode_string_list(value,";");
         N_kok=0;
         for (ii=0; ii < bc->num; ++ii) {
            for (k=0; k<cs->N_label; ++k) {
               if (strcmp(bc->str[ii], cs->label[k])) {
                  if (UseK) {
                     UseK[N_kok] = k; 
                  }
                  ++N_kok; 
               }
            }
         }
         NI_delete_str_array(bc );
      }
      SUMA_RETURN(N_kok);
   }
   
   /* See LabelToGroupedIndex to add actions for getting sub-groups */
   
   SUMA_S_Errv("Action %s not supported\n", action);
   
   SUMA_RETURN(-1);
}

int SUMA_estimate_bias_field_Wells (SEG_OPTS *Opt, 
                                       byte *cmask, SUMA_CLASS_STAT *cs,
                                       float fwhm, char *bias_classes,
                                       THD_3dim_dataset *Aset,
                                       THD_3dim_dataset *pstCgALL,
                                       THD_3dim_dataset **Bsetp) 
{
   static char FuncName[]={"SUMA_estimate_bias_field_Wells"};
   int ijk, k, N_kok, kok, *UseK , ii, jj, kk;
   THD_3dim_dataset *Rset=NULL, *Psset=NULL, *Bset = *Bsetp;
   float *fpstCgALL, fAset, fBset, *R, *Ps;
   char *str_lab=NULL;
   MRI_IMAGE *imout=NULL, *imin=NULL;
   double df, sdf, Ai, Gik, Ri, *Mg, *Sg;
   static int iter = 0, iwarn=0;
   struct  timeval tti;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* checks */
   if (!Aset || !pstCgALL || !cs) {
      SUMA_S_Errv("Bad input %p %p %p \n", 
                  Aset, pstCgALL, cs);
      SUMA_RETURN(0);
   }
   
   /* init */
   if (!Bset) { 
      NEW_SHORTY(Aset, 1, "bWells", Bset);
      *Bsetp = Bset;
   }
   
   if (!Bset) SUMA_RETURN(0);
   
   UseK = (int *)SUMA_calloc(cs->N_label, sizeof(int));
   if ((N_kok = SUMA_Class_k_Selector(cs, "classes_string", 
                                          bias_classes, UseK))<0) {
      SUMA_S_Err("Failed to find classes");
      SUMA_RETURN(0);
   }
   
   NEW_FLOATY(Aset, 1, "Rset", Rset); 
   NEW_FLOATY(Aset, 1, "Psset", Psset); 
   
   Mg = SUMA_get_Stats(cs, "meanL");
   Sg = SUMA_get_Stats(cs, "stdvL");
   fpstCgALL = (float*)SUMA_calloc(cs->N_label, sizeof(float));
   GET_BFs(pstCgALL, fpstCgALL);
   fAset = DSET_BRICK_FACTOR(Aset,0); if (fAset == 0.0) fAset = 1.0;
   R = (float *)DSET_ARRAY(Rset,0);
   Ps = (float *)DSET_ARRAY(Psset,0);
   for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
      if (IN_MASK(cmask, ijk)) {
         sdf = 0.0; Ri = 0.0;
         for (kok=0; kok<N_kok; ++kok) {
            k = UseK[kok];
            GSCVAL(pstCgALL, k, ijk, fpstCgALL[k], Gik);
            GSCVAL(Aset, 0, ijk, fAset, Ai);
            if (Ai == 0) { 
               if (!iwarn) {
                  SUMA_S_Warn("Have 0s in dset, this condition is not handled\n"
                           "robustly yet.\n");
                  ++iwarn;
               }
               Ai = 11; 
            }
            df = Gik/(Sg[k]*Sg[k]);
            Ri += (log(Ai)-Mg[k])*df;
            sdf += df;
            if (ijk == Opt->VoxDbg || isnan(Ri)) {
               short *jc=DSET_ARRAY(Aset,0);
               SUMA_1D_2_3D_index(ijk, ii, jj, kk, 
                                  DSET_NX(Aset), DSET_NX(Aset)*DSET_NY(Aset));
               SUMA_S_Notev("Debug or NAN for voxel %d [%d %d %d]\n"
                        "Cls %s, M %f, S %f, Ai %f (fAset=%f * %d)\n"
                        "Gik %f, Ri %f, sdf %f\n",
                        ijk, ii, jj,  kk, 
                        cs->label[k], Mg[k], Sg[k], Ai, fAset, jc[ijk], 
                        Gik, Ri, sdf);
               
               if (isnan(Ri)) {
                  SUMA_Seg_Write_Dset(Opt->proot, "AsetNAN", 
                                       Aset, iter, Opt->hist);    
                  SUMA_S_Err("Have NAN, will return");
                  SUMA_RETURN(0);
               }
            }
         }
         R[ijk] = Ri;
         Ps[ijk] = sdf;
           
      }      
   }

   if (Opt->debug > 1) {/* store scaled intensities */
      SUMA_Seg_Write_Dset(Opt->proot, "Rset-PreBlur", Rset, iter, Opt->hist);   
      SUMA_Seg_Write_Dset(Opt->proot, "Psset-PreBlur", Psset, iter, Opt->hist);   
   }
   
   SUMA_etime (&tti, 0);
   

   /* Blur the two sets */
   if (LocalHead || Opt->debug > 1) {
      SUMA_S_Note("Blurring Psset & Rset");      
   }

if (Opt->blur_meth == SEG_BIM || Opt->blur_meth == SEG_BNN) {
   AFNI_OMP_START ;
   #pragma omp parallel
   {
      THD_3dim_dataset *bb[2];
      int is=0;

      bb[0] = Rset;
      bb[1] = Psset;   
   #pragma omp for
      for (is=0; is<2; ++is) {
         if (!(SUMA_VolumeBlurInMask(bb[is],
                               cmask,
                               bb+is, fwhm, 0.0,
                               Opt->blur_meth == SEG_BIM ? 1:2))) {
            SUMA_S_Err("Failed to blur");
         } 
      }   
   } /* end OpenMP */
   AFNI_OMP_END ;
   
   if (Opt->debug) { SUMA_S_Notev("%f smoothing meth %d duration %f seconds\n", 
                                   fwhm, Opt->blur_meth, SUMA_etime (&tti, 1)); }
} else if (Opt->blur_meth == SEG_LSB) {
   if (!(SUMA_VolumeLSBlurInMask(Rset,
                            cmask,
                            &Rset, fwhm, 5))) {
         SUMA_S_Err("Failed to LSblur");
         SUMA_RETURN(0);
   }
   if (!(SUMA_VolumeLSBlurInMask(Psset,
                            cmask,
                            &Psset, fwhm, 5))) {
         SUMA_S_Err("Failed to LSblur");
         SUMA_RETURN(0);
   }
   if (Opt->debug) { SUMA_S_Notev("%f  smoothing meth %d duration %f seconds\n", 
                                   fwhm, Opt->blur_meth, SUMA_etime (&tti, 1)); }
   
} else if (Opt->blur_meth == SEG_BFT) {
   if (!(SUMA_VolumeBlur(Rset,
                         cmask,
                         &Rset, fwhm))) {
         SUMA_S_Err("Failed to FTblur");
         SUMA_RETURN(0);
   }
   if (!(SUMA_VolumeBlur(Psset,
                         cmask,
                         &Psset, fwhm))) {
         SUMA_S_Err("Failed to FTblur");
         SUMA_RETURN(0);
   }
   if (Opt->debug) { SUMA_S_Notev("%f  smoothing meth %d duration %f seconds\n", 
                                   fwhm, Opt->blur_meth, SUMA_etime (&tti, 1)); }
} else {
   SUMA_S_Err("Bad blur option");
   SUMA_RETURN(0);
}                                   
   if (Opt->debug > 1) {/* store scaled intensities */
      SUMA_Seg_Write_Dset(Opt->proot, "Rset-PostBlur", Rset, iter, Opt->hist);   
      SUMA_Seg_Write_Dset(Opt->proot, "Psset-PostBlur", Psset, iter, Opt->hist); 
   }
   
   fBset = 1.0/10000.0;
   R = (float *)DSET_ARRAY(Rset,0);
   Ps = (float *)DSET_ARRAY(Psset,0);
   for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
      if (IN_MASK(cmask, ijk)) {
         PSCVAL(Bset, 0, ijk, fBset, exp(R[ijk]/Ps[ijk]));
      }
   }
   EDIT_BRICK_FACTOR(Bset, 0, fBset);
      
   if (Opt->debug > 1) {/* store scaled intensities */
      SUMA_Seg_Write_Dset(Opt->proot, "Bset", Bset, iter, Opt->hist);   
   }
   
   
   SUMA_ifree(fpstCgALL); SUMA_ifree(UseK); 
   DSET_delete(Psset); DSET_delete(Rset);
   
   ++iter;
   
   SUMA_RETURN(1);
}

/*!
   Apply bias field.
*/
int SUMA_apply_bias_field (SEG_OPTS *Opt, 
                                       THD_3dim_dataset *aset,
                                       THD_3dim_dataset *fset,
                                       THD_3dim_dataset **xsetp) {
   static char FuncName[]={"SUMA_apply_bias_field"};
   int i;
   float *d=NULL;
   float bf = 1.0, bfa=1.0, bfb=1.0;
   short *b=NULL, *a=NULL;
   THD_3dim_dataset *xset = *xsetp;
   
   SUMA_ENTRY;
   
   /* checks */
   if (!aset || !fset ) {
      SUMA_S_Errv("Bad input %p %p \n", 
                  aset, fset);
      SUMA_RETURN(0);
   }
   
   /* init */
   if (!xset) {
      NEW_SHORTY(aset, 1, Opt->xrefix, xset);
      *xsetp = xset;
   }
   if (!xset) RETURN(0);
   if( !THD_ok_overwrite() && THD_is_file( DSET_HEADNAME(xset) ) ){
      SUMA_S_Warnv("Output file %s already exists and not in overwrite mode!\n",
                  DSET_HEADNAME(xset) ) ;
   }
   
   /* apply the bias field */
   if (Opt->debug > 1) INFO_message("Applying field");
   bfa = DSET_BRICK_FACTOR(aset,0); if (bfa == 0.0) bfa = 1.0;
   bfb = DSET_BRICK_FACTOR(fset,0); if (bfb == 0.0) bfb = 1.0; 
   b = (short *)DSET_ARRAY(fset,0);
   a = (short *)DSET_ARRAY(aset,0);
   d = (float *)calloc(DSET_NVOX(xset), sizeof(float));
   for (i=0; i<DSET_NVOX(xset); ++i) {
      bf = b[i]*bfb;
      if (bf > 0.5 && bf < 2.0) /* Extremists at edges cause mayhem 
                                     with division */
         d[i] = (float)a[i]/bf*bfa; 
      else d[i] = a[i]*bfa;
   }
   EDIT_substscale_brick(xset, 0, MRI_float, d, MRI_short, -1.0);
   EDIT_BRICK_LABEL(xset,0,"BiasCorrected");
   free(d); d = NULL;
   SUMA_RETURN(1);
}

int bias_stats (SEG_OPTS *Opt, 
                THD_3dim_dataset *aset, THD_3dim_dataset *gset, 
                THD_3dim_dataset *xset, int N_cl) {
   int i=0, j = 0;
   short *mm=NULL, *a=NULL, *x=NULL;
   float af=1.0, xf=1.0;
   double n, Asum2, Asum, Xsum2, Xsum, 
         Amean[N_cl], Astd[N_cl], Xmean[N_cl], Xstd[N_cl];
   
   ENTRY("bias_stats");
   
      af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0f) af = 1.0;
      xf = DSET_BRICK_FACTOR(xset,0); if (xf == 0.0f) xf = 1.0;
   a  = (short *)DSET_ARRAY(aset,0);
   x  = (short *)DSET_ARRAY(xset,0);
   mm = (short *)DSET_ARRAY(gset,0);
   for (j=0; j<N_cl; ++j) {
      n = 0;
      Asum2 = 0.0; Asum = 0.0; Xsum2 = 0.0; Xsum = 0.0;
      for (i=0; i<DSET_NVOX(aset); ++i) {                
         if (mm[i] == j+1) {
            Asum2 += a[i]*a[i];
            Asum  += a[i];
            Xsum2 += x[i]*x[i];
            Xsum  += x[i];
            ++n; 
         }   
      }
      Astd[j] = sqrt((Asum2-Asum*Asum/n)/(n-1))*af;
      Xstd[j] = sqrt((Xsum2-Xsum*Xsum/n)/(n-1))*xf;
      Amean[j] = Asum/n*af;
      Xmean[j] = Xsum/n*xf;
      fprintf(stdout,"Group %d, PRE : mean %04.2f   std %04.2f    SNR %04.2f\n"
                     "Group %d, POST: mean %04.2f   std %04.2f    SNR %04.2f\n"
                   , j+1, Amean[j], Astd[j], Amean[j]/Astd[j],
                     j+1, Xmean[j], Xstd[j], Xmean[j]/Xstd[j] );
                  
   }
   
   RETURN(1);
}

int SUMA_Class_stats(THD_3dim_dataset *aset, 
                     THD_3dim_dataset *cset, 
                     byte *cmask, int cmask_count,
                     THD_3dim_dataset *pstCgALL,
                     THD_3dim_dataset *priCgALL,
                     THD_3dim_dataset *gold,
                     SUMA_CLASS_STAT *cs) 
{
   static char FuncName[]={"SUMA_Class_stats"};
   int i=0, j = 0, sb=0, l;   
   short *a=NULL, *c=NULL, *w=NULL;
   float af=1.0, wf=1.0, fpriCgALL;
   double n, Asum2, Asum, Amean, Astd, wsum, ff, *nv=NULL, ww=0.0,
          la, AmeanL, AsumL, AstdL, Asum2L, *w0=NULL, *mixden=NULL;
    
   SUMA_ENTRY;
      
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0f) af = 1.0;
   a  = (short *)DSET_ARRAY(aset,0);
   if (cset) {
      c  = (short *)DSET_ARRAY(cset,0);
      if (DSET_BRICK_FACTOR(cset,0) != 0.0 && DSET_BRICK_FACTOR(cset,0) != 1.0) {
         SUMA_S_Err("Cset factor != 0.0 || != 1.0");
         SUMA_RETURN(0);
      }
   }
   
   if (!pstCgALL) {
      if (!c) {
         SUMA_S_Err("No classes, and no weighting set");
         SUMA_RETURN(0);
      }
      for (j=0; j<cs->N_label; ++j) {
         n = 0;
         Asum2 = 0.0; Asum = 0.0; 
         Asum2L = 0.0; AsumL = 0.0; 
         for (i=0; i<DSET_NVOX(aset); ++i) {                
            if (IN_MASK(cmask,i) && c[i] == cs->keys[j]) {
               Asum2 += a[i]*a[i]; 
               Asum  += a[i];
               la = log(a[i]*af+0.00001); Asum2L += la*la; AsumL += la;
               ++n; 
            }   
         }
         Astd = sqrt((Asum2-Asum*Asum/n)/(n-1))*af;
         Amean = Asum/n*af;
         AstdL = sqrt((Asum2L-AsumL*AsumL/n)/(n-1));
         AmeanL = AsumL/n;
         SUMA_set_Stat(cs, cs->label[j], "num", n);
         SUMA_set_Stat(cs, cs->label[j], "mean", Amean);
         SUMA_set_Stat(cs, cs->label[j], "stdv", Astd);
         SUMA_set_Stat(cs, cs->label[j], "meanL", AmeanL);
         SUMA_set_Stat(cs, cs->label[j], "stdvL", AstdL);
         SUMA_set_Stat(cs, cs->label[j], "mix", n/cmask_count);
      }
   } else {
      if (DSET_NVALS(pstCgALL) != cs->N_label &&
          DSET_NVALS(pstCgALL) != 1) {
         SUMA_S_Errv("Weight set must be 1 or %d sub-bricks. Have %d\n",
                     cs->N_label, DSET_NVALS(pstCgALL));
         SUMA_RETURN(0); 
      }
      mixden = (double *)SUMA_calloc(cs->N_label, sizeof(double));
      for (j=0; j<cs->N_label; ++j) {
         if (DSET_NVALS(pstCgALL) != 1) {
            sb = j;
         } else sb = 1;
         
         if (0 && priCgALL) { /* Need to setup denom for mix frac, 
                           as in Ashburner 2005. 
                           I turned it off for now because 
                           convergence (as deemed from Dice and Bias
                           correction in banat) seems better without it */
            fpriCgALL =  DSET_BRICK_FACTOR(priCgALL, j);
            if (!(w0=SUMA_get_Stats(cs, "mix"))) {
               mixden[j] = 0.0;
               for (i=0; i<DSET_NVOX(aset); ++i) {                
                  if (IN_MASK(cmask,i)) {
                     GSCVAL(priCgALL, j, i, fpriCgALL, ww); 
                     mixden[j] += ww;
                  }
               }
               mixden[j] *= (double)cs->N_label;
            } else {
               mixden[j] = 0.0;
               for (i=0; i<DSET_NVOX(aset); ++i) {
                  if (IN_MASK(cmask,i)) {
                     GSCVAL(priCgALL, j, i, fpriCgALL, ff);
                     Asum = 0.0;
                     for (l=0; l<cs->N_label; ++l) {
                        GSCVAL(priCgALL, l, i, fpriCgALL, ww);
                        Asum += w0[l]*ww;
                     }
                     mixden[j] += ff/Asum; 
                  }   
              } 
            }
         }else {
            mixden[j] = cmask_count;
         }
         wf = DSET_BRICK_FACTOR(pstCgALL,sb); if (wf == 0.0f) wf = 1.0;
         w = (short *)DSET_ARRAY(pstCgALL,sb);
         if (DSET_BRICK_TYPE(pstCgALL,sb) != MRI_short) {
            SUMA_S_Errv("Dset %s is not SHORT!\n", DSET_PREFIX(pstCgALL)); 
            exit(1);
         }
         Asum2 = 0.0; Asum = 0.0; wsum = 0.0;
         Asum2L = 0.0; AsumL = 0.0; 
         for (i=0; i<DSET_NVOX(aset); ++i) {                
            if (IN_MASK(cmask,i)) {
               ww = w[i]*wf;
               Asum  += ww*a[i]; wsum += ww;
               AsumL += ww*log(a[i]*af+0.00001);
            }   
         }
         Amean = Asum/wsum;
         AmeanL = AsumL/wsum;
         n = 0.0;
         Asum2 = 0.0; Asum = 0.0; 
         for (i=0; i<DSET_NVOX(aset); ++i) {                
            if (IN_MASK(cmask,i)) {
               ff = (a[i]-Amean);
               ww = w[i]*wf;
               Asum2  += ww*(ff*ff);
               la = log(a[i]*af+0.00001)-AmeanL;
               Asum2L += ww*(la*la);
               if (c && c[i] == cs->keys[j]) ++n;
            }   
         }
         Astd = sqrt(Asum2/wsum)*af;
         Amean = Amean*af;
         AstdL = sqrt(Asum2L/wsum);
         SUMA_set_Stat(cs, cs->label[j], "num", n);
         SUMA_set_Stat(cs, cs->label[j], "mean", Amean);
         SUMA_set_Stat(cs, cs->label[j], "stdv", Astd);
         SUMA_set_Stat(cs, cs->label[j], "meanL", AmeanL);
         SUMA_set_Stat(cs, cs->label[j], "stdvL", AstdL);
         SUMA_set_Stat(cs, cs->label[j], "mix", wsum/mixden[j]);
      }
      SUMA_ifree(mixden);
   }
   
      
   /* and the dice */
   if (gold && cset) {
      SUMA_CompareSegDsets(gold, cset, cmask, 1, cs );
   }
   
   SUMA_RETURN(1);   
}

int SUMA_Add_Class_Stat(SUMA_CLASS_STAT *cs, char *pname) 
{
   static char FuncName[]={"SUMA_Add_Class_Stat"};
   int i=0;
   
   SUMA_ENTRY;
   
   if (cs->pname) {
      for (i=0; i<cs->nP; ++i) {
         if (!strcmp(cs->pname[i],pname)) SUMA_RETURN(i);
      }
   }
   
   /* nothing found */
   cs->nP = cs->nP+1;
   cs->pname = (char **)SUMA_realloc(cs->pname, sizeof(char*)*cs->nP);
   cs->pname[cs->nP-1] = SUMA_copy_string(pname); 
   cs->Pv = (double **)SUMA_realloc(cs->Pv,sizeof(double*)*cs->nP);
   cs->Pv[cs->nP-1] = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   
   SUMA_RETURN(cs->nP-1);
}

int SUMA_Add_Class_Label(SUMA_CLASS_STAT *cs, char *label, int key) 
{
   static char FuncName[]={"SUMA_Add_Class_Label"};
   int i=0;
   
   SUMA_ENTRY;
   
   for (i=0; i<cs->N_label; ++i) {
      if (!strcmp(cs->label[i],label)) SUMA_RETURN(i);
   }
   
   for (i=0; i<cs->N_label; ++i) {
      if (cs->keys[i]==key) {
         SUMA_S_Errv("key %d for new label %s is in use already for %s\n",
                     key, label, cs->label[i]);
         SUMA_RETURN(0);
      }
   }
   
   /* nothing found */
   cs->N_label = cs->N_label+1;
   cs->label = (char **)SUMA_realloc(cs->label, cs->N_label*sizeof(char *));
   cs->label[cs->N_label-1] = SUMA_copy_string(label);
   
   cs->keys = (int *)SUMA_realloc(cs->keys, cs->N_label*sizeof(int));
   cs->keys[cs->N_label-1] = key;
   
   for (i=0; i<cs->nP; ++i) {
      cs->Pv[i] = (double *)SUMA_realloc(cs->Pv[i],cs->N_label*sizeof(double));
   }
   
   SUMA_RETURN(cs->N_label-1);
}


SUMA_CLASS_STAT *SUMA_New_Class_Stat(char **clssl, int N_clssl, int *keys, 
                                    int nP, NI_str_array *pnames) 
{
   static char FuncName[]={"SUMA_New_Class_Stat"};
   SUMA_CLASS_STAT *cs=NULL;
   int i;

   SUMA_ENTRY;

   cs = (SUMA_CLASS_STAT *) SUMA_calloc(1, sizeof(SUMA_CLASS_STAT));

   if (pnames) {
      if (nP < 0) nP = pnames->num;
      if (nP != pnames->num) {
         SUMA_S_Errv("Mismatch between nP %d and pnames->num %d\n",
                  nP, pnames->num);
         SUMA_RETURN(NULL);
      }
   } else {
      if (nP != 3) {
         SUMA_S_Errv("Can only handle 3 parameters (not %d) without names\n",
                     nP);
         SUMA_RETURN(NULL);
      }
   }
   cs->N_label = N_clssl;
   cs->nP = 0; cs->pname=NULL, cs->Pv=NULL;
   cs->label = (char **)SUMA_calloc(cs->N_label,sizeof(char *));
   cs->keys = (int *)SUMA_calloc(cs->N_label, sizeof(int));
   for (i=0; i<N_clssl; ++i) {
      cs->label[i] = SUMA_copy_string(clssl[i]);
      if (keys) cs->keys[i] = keys[i];
      else cs->keys[i] = i+1;
   }
   for (i=0; i<nP; ++i) {
      if (pnames) {
         if (SUMA_Add_Class_Stat(cs, pnames->str[i]) < 0) {
            SUMA_S_Errv("Failed to add %s\n", pnames->str[i]);
            SUMA_RETURN(NULL); 
         }
      } else {
         switch(i) {
            case 0:
               if (SUMA_Add_Class_Stat(cs, "num") < 0) {
                  SUMA_S_Errv("Failed to add %s\n", pnames->str[i]);
                  SUMA_RETURN(NULL); 
               }
               break;
            case 1:
               if (SUMA_Add_Class_Stat(cs, "mean") < 0) {
                  SUMA_S_Errv("Failed to add %s\n", pnames->str[i]);
                  SUMA_RETURN(NULL); 
               }
               break;
            case 2:
               if (SUMA_Add_Class_Stat(cs, "stdv") < 0) {
                  SUMA_S_Errv("Failed to add %s\n", pnames->str[i]);
                  SUMA_RETURN(NULL); 
               }
               break;
            default:
               SUMA_S_Errv("Can't handle %d\n", i);
               SUMA_RETURN(NULL);   
         }
      }
   }
   SUMA_RETURN(cs);
}  

SUMA_CLASS_STAT *SUMA_Free_Class_Stat(SUMA_CLASS_STAT *cs) 
{
   static char FuncName[]={"SUMA_Free_Class_Stat"};
   int i=0;
   
   SUMA_ENTRY;

   if (cs) {
      if (cs->pname) {
         for (i=0; i<cs->nP; ++i) { 
            SUMA_ifree(cs->pname[i]); 
            SUMA_ifree(cs->Pv[i]);
         }
      }
      SUMA_ifree(cs->Pv);
      SUMA_ifree(cs->pname);
      if (cs->label) {
         for (i=0; i<cs->N_label; ++i) { SUMA_ifree(cs->label[i]); }
      }
      SUMA_ifree(cs->label);
      SUMA_ifree(cs->keys);
   }
   SUMA_RETURN(NULL);   
}

int SUMA_Stat_position (SUMA_CLASS_STAT *cs, char *label, char *pname, 
                        int pp[])
{
   static char FuncName[]={"SUMA_Stat_position"};
   int i=0,k=0;

   SUMA_ENTRY;
   
   pp[0] = pp[1] = -1;

   if (label) {
      for (i=0; i<cs->N_label; ++i) {
         if (!strcmp(cs->label[i], label)) {
            pp[0] = i;
            break;
         }
      }
   }

   
   if (pname) {
      for (k=0; k<cs->nP; ++k) {
         if (!strcmp(cs->pname[k], pname)) {
            pp[1] = k;
            break;
         }
      }
   }
   
   if ( (pp[0] < 0 && label) || (pp[1] < 0 && pname)) SUMA_RETURN(0);

   SUMA_RETURN(1);   
}

double SUMA_get_Stat(SUMA_CLASS_STAT *cs, char *label, char *pname) 
{
   static char FuncName[]={"SUMA_get_Stat"};
   int pp[2];

   SUMA_ENTRY;
   
   if (!SUMA_Stat_position(cs, label, pname, pp)) {
      SUMA_S_Errv("Failed to locate %s of %s\n",
                  pname, label);
      SUMA_RETURN(0.0); 
   }   

   SUMA_RETURN(cs->Pv[pp[1]][pp[0]]);
}

double *SUMA_get_Stats(SUMA_CLASS_STAT *cs,  char *pname)
{
   static char FuncName[]={"SUMA_get_Stats"};
   double *vv=NULL;
   int pp[2];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
      
   if (!SUMA_Stat_position(cs, NULL,  pname, pp)) {
      if (LocalHead) {
         SUMA_S_Notev("Failed to locate %s\n",
                  pname);
      }
      SUMA_RETURN(NULL); 
   }
   SUMA_RETURN(cs->Pv[pp[1]]);
}

int SUMA_set_Stat(SUMA_CLASS_STAT *cs, char *label, char *pname, double val)      {
   static char FuncName[]={"SUMA_set_Stat"};
   int pp[2];

   SUMA_ENTRY;
   SUMA_Stat_position(cs, label, pname, pp);
   if (pp[0] < 0) {
      SUMA_S_Errv("Failed to locate class label %s \n",
                  label);
      SUMA_RETURN(0); 
   }
   if (pp[1] < 0) {
      /* add new stat */
      if ((pp[1] = SUMA_Add_Class_Stat(cs, pname)) < 0) {
         SUMA_S_Errv("Failed to add stat %s\n", pname);
         SUMA_RETURN(0); 
      }
   }
   
   cs->Pv[pp[1]][pp[0]] = val; 
   SUMA_RETURN(1);  
}

int SUMA_show_Class_Stat(SUMA_CLASS_STAT *cs, char *head, char *fout) {
   FILE *ff = NULL;
   int a;
   if (!fout) return(SUMA_dump_Class_Stat(cs, head, SUMA_STDERR));
   else {
      a = 0;
      ff = fopen(fout,"w");
      if (ff) {
         a = SUMA_dump_Class_Stat(cs, head, ff);
         fclose(ff); 
      } 
      return(a);
   }
}

int SUMA_dump_Class_Stat(SUMA_CLASS_STAT *cs, char *head, FILE *Out)
{
   static char FuncName[]={"SUMA_dump_Class_Stat"};
   char *s=NULL;
   
   SUMA_ENTRY;
   s = SUMA_Class_Stat_Info(cs, head);

   if (!Out) Out = SUMA_STDERR;
   
   fprintf(Out,"%s", s);
   
   SUMA_ifree(s);
   
   SUMA_RETURN(1);
} 

char *SUMA_Class_Stat_Info(SUMA_CLASS_STAT *cs, char *head)
{
   static char FuncName[]={"SUMA_Class_Stat_Info"};
   int i, j;
   SUMA_STRING *SS;
   char *s=NULL;
   char buf[36];
   
   SUMA_ENTRY;

   SS = SUMA_StringAppend (NULL, NULL);
   
   if (head) {
      SS = SUMA_StringAppend_va(SS,"%s", head);
   }
   SS = SUMA_StringAppend_va(SS,"%8s %4s   ", "Class", "Key");
   for (j=0; j<cs->nP; ++j) {
      SS = SUMA_StringAppend_va(SS,"%8s   ",cs->pname[j]);
   }
   SS = SUMA_StringAppend_va(SS,"\n");
   for (i=0; i<cs->N_label; ++i) {
      sprintf (buf, "%s", 
               MV_format_fval2(cs->keys[i], 4)); 
      SS = SUMA_StringAppend_va(SS,"%8s %4s   ", 
                                    cs->label[i], buf);
      for (j=0; j<cs->nP; ++j) {
         sprintf (buf, "%s", 
               MV_format_fval2(cs->Pv[j][i], 8));
         SS = SUMA_StringAppend_va(SS,"%8s   ", buf );
      }
      SS = SUMA_StringAppend_va(SS,"\n");
   }

   SUMA_SS2S(SS,s);
   
   SUMA_RETURN(s);
}

int SUMA_MixFrac_from_ClassStat(SUMA_CLASS_STAT *cs, float *mf) 
{
   static char FuncName[]={"SUMA_MixFrac_from_ClassStat"};
   float ss=0.0;
   int i;
   
   SUMA_ENTRY;
   
   for (i=0; i<cs->N_label; ++i) {
      mf[i] = SUMA_get_Stat(cs, cs->label[i], "num");
      ss += mf[i];
   }
   for (i=0; i<cs->N_label; ++i) mf[i] /= ss;
   
   SUMA_RETURN(1);
}

double SUMA_mixopt_2_mixfrac(char *mixopt, char *label, int key, int N_clss,
                             byte *cmask, THD_3dim_dataset *cset) 
{
   static char FuncName[]={"SUMA_mixopt_2_mixfrac"};
   int i, ntot, nkey;
   short *a=NULL;
   double frac=-1.0;
   
   SUMA_ENTRY;
   
 
   if (!mixopt || !strncmp(mixopt,"UNI",3)) {
      frac = 1.0/(double)N_clss;
   } else if (!strcmp(mixopt,"TOY_DEBUG")) {
           if (!strcmp(label, "CSF"))frac = 0.1;
      else if (!strcmp(label, "GM")) frac = 0.45;
      else if (!strcmp(label, "WM")) frac = 0.45;
      else {
         SUMA_S_Errv("Not ready for class %s in mixopt %s\n",
               label, mixopt);
         SUMA_RETURN(-1.0);
      } 
   } else if (!strcmp(mixopt,"WHOLE_BRAIN")) {
           if (!strcmp(label, "CSF"))frac = 0.015;
      else if (!strcmp(label, "GM")) frac = 0.650;
      else if (!strcmp(label, "WM")) frac = 0.335;
      else {
         SUMA_S_Errv("Not ready for class %s in mixopt %s\n",
               label, mixopt);
         SUMA_RETURN(-1.0);
      } 
   } else if (!strcmp(mixopt,"AVG152_BRAIN_MASK")) {
           if (!strcmp(label, "CSF"))frac = 0.155;
      else if (!strcmp(label, "GM")) frac = 0.550;
      else if (!strcmp(label, "WM")) frac = 0.295;
      else {
         SUMA_S_Errv("Not ready for class %s in mixopt %s\n",
               label, mixopt);
         SUMA_RETURN(-1.0);
      } 
   } else if (!strcmp(mixopt,"AVG152p_BRAIN_MASK")) {
           if (!strcmp(label, "CSF"))frac = 0.149;
      else if (!strcmp(label, "GM")) frac = 0.480;
      else if (!strcmp(label, "WM")) frac = 0.371;
      else {
         SUMA_S_Errv("Not ready for class %s in mixopt %s\n",
               label, mixopt);
         SUMA_RETURN(-1.0);
      } 
   } else if (!strcmp(mixopt,"CSET")) {
      if (!cset) {
         SUMA_S_Err("No -cset input to use with CSET");
         SUMA_RETURN(-1.0);
      }
      /* not the most efficient, doing this once at a time, 
      but this is not called often*/
      a = (short *)DSET_BRICK_ARRAY(cset,0);
      ntot = 0; nkey = 0;
      for (i=0; i<DSET_NVOX(cset); ++i) {
         if (IN_MASK(cmask,i)) {
            ++ntot;
            if (key == a[i]) ++nkey;
         }
      }
      if (ntot) frac = (double)nkey/(double)ntot;
   } else {
      SUMA_S_Errv("-mixopt '%s' cannot be interpreted\n", mixopt);
      SUMA_RETURN(-1.0);
   }
   
   SUMA_RETURN(frac);
}
double pdfnorm(double x, double mean, double stdv) {
   double x0=x-mean;
   return(1.0/(SQ2PI*stdv)*exp(-(x0*x0)/(2*stdv*stdv)));
}

THD_3dim_dataset *SUMA_p_Y_GIV_C_B_O(
                           THD_3dim_dataset *aset, THD_3dim_dataset *cset,
                                 byte *cmask, SUMA_CLASS_STAT *cs, 
                                 THD_3dim_dataset *pygc) 
{
   static char FuncName[]={"SUMA_p_Y_GIV_C_B_O"};
   int i, k;
   double x0, mean, stdv, c1, c2, *p=NULL;
   float af=0.0;
   short *a=NULL, *c=NULL;
   THD_3dim_dataset *pout=pygc;
   
   SUMA_ENTRY;
   
   if (!pout) {
      NEW_SHORTY(aset,1,"p_Y_GIV_C_B_O",pout);
   }
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0f) af = 1.0;
   a = (short *)DSET_ARRAY(aset,0);
   c = (short *)DSET_ARRAY(cset,0);
   p = (double *)SUMA_calloc(DSET_NVOX(aset), sizeof(double));
   
   for (k=0; k<cs->N_label; ++k) {
      mean=SUMA_get_Stat(cs, cs->label[k], "mean");
      stdv=SUMA_get_Stat(cs, cs->label[k], "stdv"); 
      c1 = 1.0/(SQ2PI*stdv); c2 = (2*stdv*stdv);
      for (i=0; i<DSET_NVOX(aset); ++i) {
         if (IN_MASK(cmask, i) && c[i] == cs->keys[k]) {
            x0 = (double)a[i]*af - mean;
            p[i] = c1 * exp(-(x0*x0)/c2);
         }
      }
   }
   
   /* put vector back in pout */
   EDIT_substscale_brick(pout, 0, MRI_double, p, MRI_short, -1.0);
   free(p); p = NULL;
   SUMA_RETURN(pout);
}


typedef struct {
   SUMA_CLASS_STAT *cs;
   THD_3dim_dataset *aset;
   THD_3dim_dataset *cset;
   THD_3dim_dataset *Bset;
   THD_3dim_dataset *pstCgALL;
   THD_3dim_dataset *priCgALL;
   THD_3dim_dataset *pCgN;
   float mrfB;
   float Temp;
   byte *cmask;
   int cmask_count;
   int method;
   int *UseK;
   int N_kok;
} EEO_UD; /* user data for SUMA_EdgeEnergy_OptimCost */

static EEO_UD eeoud;

void SUMA_free_eeoud() {
   if (eeoud.pstCgALL) DSET_delete(eeoud.pstCgALL); eeoud.pstCgALL=NULL;
   if (eeoud.UseK) SUMA_ifree(eeoud.UseK); 
   return;
}

void SUMA_set_eeoud(SUMA_CLASS_STAT *cs, THD_3dim_dataset *aset,
                    THD_3dim_dataset *Bset, THD_3dim_dataset *cset,
                    THD_3dim_dataset *priCgAll, THD_3dim_dataset *pCgN,
                    float mrfB, float Temp,
                    byte *cmask, int cmask_count, 
                    int method, char *classes) {
   static char FuncName[]={"SUMA_set_eeoud"};
   
   SUMA_ENTRY;
   
   SUMA_free_eeoud();
   eeoud.cs = cs;
   eeoud.aset = aset;
   eeoud.Bset = Bset;
   eeoud.cset = cset;
   if (!eeoud.cset) {
      SUMA_S_Err("Need cset"); SUMA_RETURNe;
   }  
   eeoud.mrfB = mrfB;
   eeoud.Temp = Temp;
   eeoud.priCgALL = priCgAll;
   eeoud.pstCgALL = NULL;
   eeoud.pCgN = pCgN;
   eeoud.cmask = cmask;
   eeoud.method = method;
   eeoud.UseK = (int *)SUMA_calloc(cs->N_label, sizeof(int));
   if ((eeoud.N_kok = SUMA_Class_k_Selector(eeoud.cs, "classes_string", 
                                          classes, eeoud.UseK))<0) {
      SUMA_S_Err("Failed to find classes");
      SUMA_RETURNe;
   }

   SUMA_RETURNe;
}

void SUMA_EdgeEnergy_Gassign(THD_3dim_dataset *aset, THD_3dim_dataset *fset,
                        byte *cmask, SUMA_CLASS_STAT *cs, int *UseK, int N_kok,
                        double *par, int npar, THD_3dim_dataset *cset)
{
   static char FuncName[]={"SUMA_EdgeEnergy_Gassign"};
   int i, ku;
   double cost = 0.0, eem=0.0, dd=0.0, ee=0.0, mean=0.0, stdv=0.0;
   short *a=NULL, *fb=NULL;
   short *cout=NULL;
   float af, ff=1.0; 
   float aof;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LHv("aset %p, fset %p\n", aset, fset);
   
   a = (short *)DSET_ARRAY(aset,0);
   af = DSET_BRICK_FACTOR(aset, 0); 
   
   if (fset) {
      fb = (short *)DSET_ARRAY(fset,0);
      ff = DSET_BRICK_FACTOR(fset, 0);
   }
   cout = (short *)DSET_ARRAY(cset,0);
    
   if (af == 0.0f) af = 1.0;
   if (ff == 0.0f) ff = 1.0;
   aof = af/ff;
   /* Assign classes based on gaussians into gcset */
   for (i=0; i<DSET_NVOX(aset); ++i) {
      if (IN_MASK(cmask,i)) {
         eem = -1.0;
         for (ku=0; ku<N_kok; ++ku) {
            mean = par[2*ku]; stdv = par[2*ku+1];
            if (fset) dd = ((double)a[i]/fb[i]*aof-mean); 
            else dd = ((double)a[i]*aof-mean);  
            dd *= dd;
            ee = exp(-dd/(2.0*stdv*stdv) -log(stdv));
            if (LocalHead) {
               if (i == 1631822) {
                  fprintf(stderr,
                     "%d: a=%f, f=%f, m=%f, s=%f, k=%d, ee=%f, eem=%f\n",
                     i, a[i]*af, fset ? fb[i]*ff:1.0, mean, stdv, ku, ee, eem);
               }
            }
            if (ee > eem) { eem = ee; cout[i] = cs->keys[UseK[ku]]; }
         }
      }
   }
   
   SUMA_RETURNe;
}

 
double SUMA_EdgeEnergy_OptimCost(int n, double *par) 
{
   static char FuncName[]={"SUMA_EdgeEnergy_OptimCost"};
   static int iter;
   int i;
   double cost;
   THD_3dim_dataset *pstCgALL=NULL;
   THD_3dim_dataset *cset=NULL;
   
   /* put parameters into cs */
   for (i=0; i<eeoud.N_kok; ++i) {
      SUMA_set_Stat(eeoud.cs, eeoud.cs->label[eeoud.UseK[i]], 
                        "mean", par[2*i  ]);
      SUMA_set_Stat(eeoud.cs, eeoud.cs->label[eeoud.UseK[i]], 
                        "stdv", par[2*i+1]);
   }
   
   #if 0
   /* Assign classes based on Gaussians only */
   SUMA_EdgeEnergy_Gassign(eeoud.aset, eeoud.Bset,
                        eeoud.cmask, eeoud.cs, 
                        eeoud.UseK, eeoud.N_kok,
                        par, n, eeoud.cset);
   #else 
   /*SUMA_S_Warn("HAD NO EFFECT on RESULTS whether -edge was used or not\n"
               "Not exactly sure why yet. Either a stupic coding mistake,\n"
               "Or the priors are too powerful. Although the bug comes up\n"
               "even at the simplest command such as: \n"
               "3dSeg   -anat banat+orig -mask anat.ns+orig \n"
               "        -gold goldseg+orig -gold_bias goldbias+orig \n"
               "        -classes 'CSF ; GM ; WM' -Bmrf 0.0 -edge1 \n"
               "        -bias_classes 'GM ; WM' -bias_fwhm 25 \n"
               "-prefix case.A.1nopst.ss -overwrite");
   */
   /* stick par parameters parameters here */
   
   /* compute posterior as would be done in main routine */
   if (!SUMA_pst_C_giv_ALL( eeoud.aset, 
                             eeoud.cmask, eeoud.cmask_count,
                             eeoud.cs, eeoud.priCgALL, eeoud.pCgN, 
                             eeoud.mrfB, eeoud.Temp, 1,
                             &(pstCgALL))) {
         fprintf(stderr,"Error SUMA_EdgeEnergy_OptimCost:\n"
                        "Failed in SUMA_pst_C_giv_ALL\n");
         return(0);
   }
   eeoud.pstCgALL = pstCgALL; pstCgALL = NULL;
   
   /* assign classes */
   if (!(SUMA_assign_classes( eeoud.pstCgALL, eeoud.cs, 
                              eeoud.cmask, &cset))) { 
      fprintf(stderr,"Error SUMA_EdgeEnergy_OptimCost:\n"
                        "Failed in SUMA_assign_classes\n");
      return(0);
   }
   memcpy(DSET_ARRAY(eeoud.cset,0),
          DSET_ARRAY(cset,0), sizeof(short)*DSET_NVOX(cset)); 
   DSET_delete(cset); cset=NULL;
   #endif
   /* call energy function */
   cost = -1.0 * SUMA_DsetEdgeEnergy(eeoud.aset,
                       eeoud.cset,
                       eeoud.cmask, 
                       eeoud.Bset, NULL,
                       eeoud.cs, eeoud.method,
                       eeoud.UseK, eeoud.N_kok);
   
   if (debug) fprintf(SUMA_STDERR,"%cMethod %d. iter %d, Edge Cost %f", 
            0xd, eeoud.method, iter, cost); 
   
   

   ++iter; 
   return(cost);
}

double SUMA_MAP_EdgeEnergy(THD_3dim_dataset *aset, byte *cmask, int cmask_count,
                        THD_3dim_dataset *Bset, SUMA_CLASS_STAT *cs, 
                        THD_3dim_dataset *cset, int method, 
                        THD_3dim_dataset *priCgAll, THD_3dim_dataset *pCgN, 
                        float mrfB, float Temp, 
                        float deltamean, float deltastd,
                        SEG_OPTS * Opt)
{
   static char FuncName[]={"SUMA_MAP_EdgeEnergy"};
   int ncalls = 0, nparmax = 36, npar, i, nrand, ntry, nkeep, maxcall;
   double par[nparmax], bot[nparmax], top[nparmax], cost,
          gap[nparmax], rstart, rend;
   static int icall = 0;
   SUMA_Boolean LocalHead = NOPE;   
   
   SUMA_ENTRY;
   
   /* load user data */
   SUMA_set_eeoud(cs, aset, Bset, cset, priCgAll, pCgN, mrfB, Temp, 
                  cmask, cmask_count, method, "CSF; GM; WM");
                  
   /* load parameters into par, bot, top */
   if (cs->N_label*2 > nparmax) {
      SUMA_S_Err("Too many parameters");
      SUMA_RETURN(cost);
   }
   
   /* initialize parameters and estimate gap between classes.
   For 1st and last class, assume gap is one stdv */
   npar = eeoud.N_kok*2;
   for (i=0; i<eeoud.N_kok; ++i) {
      par[2*i  ] = SUMA_get_Stat(cs, cs->label[eeoud.UseK[i]], "mean");
      par[2*i+1] = SUMA_get_Stat(cs, cs->label[eeoud.UseK[i]], "stdv");
      if (i>0) {
         gap[i] = par[2*i  ] - par[2*(i-1)]; /* mean difference 
                                                     between classes i and i-1 */
         if (gap[i] <= 0) {
            SUMA_S_Err("Classes not sorted by increasing mean");
            SUMA_RETURN(0.0);
         }
      } else {
         gap[0] = SUMA_get_Stat(cs, cs->label[eeoud.UseK[0]], "stdv")*2.0; 
                     /* Can't compute mean difference, 
                      allow something function of stdv */
      }
   }
   gap[eeoud.N_kok] = SUMA_get_Stat(cs, 
                        cs->label[eeoud.UseK[eeoud.N_kok-1]], "stdv")*2.0; 
   
   /* min and max params for mean parameters */
   for (i=0; i<eeoud.N_kok; ++i) {
      if (i==0)   bot[2*i] = SUMA_MAX_PAIR(par[2*i] - gap[i]/10.0, 
                                          (1.0-deltamean/2.0)*par[2*i]);
      else        bot[2*i] = par[2*i] - gap[i]/10.0;
      if (i==eeoud.N_kok-1) 
                  top[2*i] = SUMA_MIN_PAIR(par[2*i]+gap[i+1]/10.0, 
                                          (1.0+deltamean/2.0)*par[2*i]);
      else        top[2*i] = par[2*i] + gap[i+1]/10.0;  
   }
   /* min and max params for stdv parameters */
   for (i=0; i<eeoud.N_kok; ++i) { 
      bot[2*i+1] = (1.0-deltastd/2.0)*par[2*i+1]; 
      top[2*i+1] = (1.0+deltastd/2.0)*par[2*i+1];
   }
   
   if (Opt->debug || LocalHead ) {
      if (Opt->debug > 1 || LocalHead) {
         for (i=0; i<eeoud.N_kok; ++i) {
            fprintf(SUMA_STDERR,
               "Pre Optimization:\n"
            "%3s:  mean [%.3f <- %.3f -> %.3f], stdv [%.3f <- %.3f -> %.3f]\n",
                     cs->label[eeoud.UseK[i]],  
                                    bot[2*i  ], par[2*i  ], top[2*i  ],
                                    bot[2*i+1], par[2*i+1], top[2*i+1]);
         }
      } else {
         fprintf(SUMA_STDERR,
            "Pre Optimization: ");
         for (i=0; i<eeoud.N_kok; ++i) {
            fprintf(SUMA_STDERR,
               "%3s:  mean %.3f, stdv %.3f   ",
               cs->label[eeoud.UseK[i]],  par[2*i  ], par[2*i+1]);
         }
         fprintf(SUMA_STDERR, "\n");
      }
      
      if (Opt->debug > 1){
        SUMA_Seg_Write_Dset(Opt->proot, "PreEdgeOptim", cset, icall, Opt->hist);
      }
   }
   
   nrand = 0; nkeep = 0; ntry = 2;
   rstart = 0.2; rend = 0.05;
   maxcall = 50;
   if ( (ncalls = powell_newuoa_constrained (npar, par, &cost,
                                             bot, top, 
                                             nrand, nkeep, 2,
                                             rstart, rend,
                                             maxcall,
                                    SUMA_EdgeEnergy_OptimCost)) < 0) {
      SUMA_S_Err("Failed in optimization");
      SUMA_RETURN(0);
   }
   if (debug) fprintf(SUMA_STDERR,"\n");
   
   if (Opt->debug || LocalHead ) {
      if (Opt->debug > 1 || LocalHead) {
         for (i=0; i<eeoud.N_kok; ++i) {
            fprintf(SUMA_STDERR,
               "Post Optimization:\n"
            "%3s:  mean [%.3f <- %.3f -> %.3f], stdv [%.3f <- %.3f -> %.3f]\n",
                     cs->label[eeoud.UseK[i]],  
                                    bot[2*i  ], par[2*i  ], top[2*i  ],
                                    bot[2*i+1], par[2*i+1], top[2*i+1]);
         }
      } else {
         fprintf(SUMA_STDERR,
            "Post Optimization: ");
         for (i=0; i<eeoud.N_kok; ++i) {
            fprintf(SUMA_STDERR,
               "%3s:  mean %.3f, stdv %.3f   ",
               cs->label[eeoud.UseK[i]],  par[2*i  ], par[2*i+1]);
         }
         fprintf(SUMA_STDERR, "\n");
      }
      /* write out the classification result based on EdgeEnergy Optimization*/
      if (Opt->debug > 1){
        SUMA_Seg_Write_Dset(Opt->proot, "PostEdgeOptim", 
                            cset, icall, Opt->hist);
      }
   }
   
   /* reload optimized params into cs */
   for (i=0; i<eeoud.N_kok; ++i) {
      SUMA_set_Stat(cs, cs->label[eeoud.UseK[i]], "mean", par[2*i  ]);
      SUMA_set_Stat(cs, cs->label[eeoud.UseK[i]], "stdv", par[2*i+1]);
   }

   SUMA_free_eeoud();
   
   ++icall;
   SUMA_RETURN(cost);
}

/* See Berthod et al. */
int SUMA_MAP_labels(THD_3dim_dataset *aset, 
                        byte *cmask, 
                        SUMA_CLASS_STAT *cs, int neighopt, 
                        THD_3dim_dataset *pC,
                        THD_3dim_dataset **csetp, 
                        THD_3dim_dataset **pCgNp,
                        SEG_OPTS *Opt)
{
   static char FuncName[]={"SUMA_MAP_labels"};
   THD_3dim_dataset *cset = *csetp;
   THD_3dim_dataset *pCgN = *pCgNp;
   float af=0.0, *fpCgN=NULL, *fpC=NULL;
   int iter=0, i=0, k, ni, nj, nk, nij, ijkn[6];
   int Niter = 3, kmin;
   double eG[Niter], e, eG1, eG2, *mv, *sv, dd, *e1=NULL, 
          *e2=NULL, BoT, pp, *wv;
   short *ci=NULL, *co=NULL, *a=NULL;
   
   SUMA_ENTRY;
   
   BoT = Opt->B/Opt->T;
   
   if (neighopt != 4 && neighopt != 6) {
      SUMA_S_Errv("Allowing neighopt of 4 or 6 only. Have %d\n", neighopt);
      SUMA_RETURN(0);
   } 
    
   if (!cset) {
      NEW_SHORTY(aset,1,"MAP_labels",cset);
      *csetp = cset;
   }
   
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0f) af = 1.0;
   a = (short *)DSET_ARRAY(aset,0);
   co = (short *)DSET_ARRAY(cset,0);
   ci = (short *)malloc(sizeof(short)*DSET_NVOX(aset));
   e1 = (double *)calloc(cs->N_label, sizeof(double));
   e2 = (double *)calloc(cs->N_label, sizeof(double));
   fpCgN = (float *)calloc(cs->N_label, sizeof(float));
   fpC = (float *)calloc(cs->N_label, sizeof(float));
   
   if (!pCgN) {
      NEW_SHORTY(aset,cs->N_label, "pCgN", pCgN);
      *pCgNp = pCgN;
      for (k=0; k<cs->N_label; ++k) {
         EDIT_BRICK_FACTOR(pCgN, k, 1/10000.0);
      }
   }
   GET_BFs(pCgN,fpCgN);

   ni = DSET_NX(aset);
   nj = DSET_NY(aset);
   nk = DSET_NZ(aset);
   nij = ni*nj;

   if (pC) { /* The priors for the classes */
      GET_BFs(pC, fpC);
   }
   
   /* get vector of parameters  */
   mv = SUMA_get_Stats(cs, "mean");
   sv = SUMA_get_Stats(cs, "stdv");
   wv = SUMA_get_Stats(cs, "mix");
   
   for (iter=0; iter<Niter; ++iter) {
      memcpy(ci, co, sizeof(short)*DSET_NVOX(aset));
      eG[iter] = 0.0; eG1=0.0; eG2 = 0.0;
      for (i=0; i<DSET_NVOX(aset); ++i) {
         if (IN_MASK(cmask,i)) {
            for(k=0; k<cs->N_label; ++k) {
               /* term 1, energy of y given class */
               dd = ((double)a[i]*af-mv[k]); dd *= dd; 
               e1[k] = (dd/(2.0*sv[k]*sv[k]) + log(sv[k]*SQ2PI ))/Opt->T; 

               /* term 2, energy of label given neighbors */
               GET_NEIGHBS_IN_MASK( cmask, i, 
                                    ni, nj, nk, nij, 
                                    ijkn);
               E_l_GIV_NEIGHBS(ci, ijkn, cs->keys[k], neighopt, e2[k]);
               e2[k] = e2[k]*BoT;
            }
            
            /* modulate e1 by pC and w, if specified*/
            #if 1
            if (i==0 && iter == 0) SUMA_S_Note("Check again");
            /* KEEP or kill? w alone seems OK, pC needs some love, perhaps. 
               Test again */
            if (pC) {
               e = 0.0; dd = 0.0;
               for(k=0; k<cs->N_label; ++k) { 
                  GSCVAL(pC, k, i, fpC[k], dd);
                  e1[k] = exp(-e1[k])*dd*wv[k]; e += dd*wv[k];
               }
               for(k=0; k<cs->N_label; ++k) { e1[k] = -log(e1[k]/e); }
            } else {
               e = 0.0; dd = 0.0;
               for(k=0; k<cs->N_label; ++k) { 
                  e1[k] = exp(-e1[k])*wv[k]; e += wv[k];
               }
               for(k=0; k<cs->N_label; ++k) { e1[k] = -log(e1[k]/e); }
            }
            #endif
            /* find min e */
            e = e1[0]+e2[0]; kmin=0; 
            for(k=1; k<cs->N_label; ++k) { 
               if (e1[k]+e2[k] < e) { 
                  e=e1[k]+e2[k]; kmin=k; 
               } 
            }
            
            if (i == Opt->VoxDbg) {
               int IJK[3], pp;
               Vox1D2Vox3D(i,DSET_NX(aset), DSET_NX(aset)*DSET_NY(aset), IJK);
               fprintf(Opt->VoxDbgOut, "at %d %d %d, a=%d (%f)\n", 
                                       IJK[0], IJK[1], IJK[2],
                                       a[i], a[i]*af);
               fprintf(Opt->VoxDbgOut, "e1:   ");
               for(k=0; k<cs->N_label; ++k) {
                  fprintf(Opt->VoxDbgOut, "%f   ", e1[k]);
               }
               fprintf(Opt->VoxDbgOut, "\n");
               fprintf(Opt->VoxDbgOut, "e2:   ");
               for(k=0; k<cs->N_label; ++k) {
                  fprintf(Opt->VoxDbgOut, "%f   ", e2[k]);
               }
               fprintf(Opt->VoxDbgOut, "\n");
               fprintf(Opt->VoxDbgOut, "e:   ");
               for(k=0; k<cs->N_label; ++k) {
                  if (k!=kmin) fprintf(Opt->VoxDbgOut, "%f   ", e1[k]+e2[k]);
                  else fprintf(Opt->VoxDbgOut, "%f*  ", e1[k]+e2[k]);
               }
               fprintf(Opt->VoxDbgOut, "\n\n");
            } 
            eG1 += e1[kmin];
            eG2 += e2[kmin]; /* <- Not quite, 
                                 cliques get counted more than once
                                 this way. This should be a sum over 
                                 all clicques (Eq. 19 Berthod et al 96),
                                  not over all cliques sums
                                 at each voxel (Sum of Eq.20). Revisit */
            eG[iter] = eG1+eG2;
            co[i] = cs->keys[kmin];
            /* store the prob. for each class p(c|Neighb) */
            pp = 0; 
            for (k=0; k<cs->N_label; ++k) { 
               e2[k] = exp(-e2[k]); /* now e2 is a prob. * scaling factor*/
               pp += e2[k];
            }
            for (k=0; k<cs->N_label; ++k) {
               PSCVAL(pCgN,k, i, fpCgN[k], e2[k]/pp);
            }
         } else {
            co[i] = 0;
         }
      }/* for i */
      if (Opt->debug > 1) SUMA_S_Notev("Iter %d, e=%f\n", iter, eG[iter]);
   } /* for iter */
   
      
   SUMA_ifree(ci);   SUMA_ifree(fpCgN); SUMA_ifree(fpC); 
   SUMA_RETURN(1);
}

/*!
   Posterior distribution of class given the whole enchilada
   
*/
int SUMA_pst_C_giv_ALL(THD_3dim_dataset *aset,  
                                 byte *cmask, int cmask_count,
                                 SUMA_CLASS_STAT *cs,
                                 THD_3dim_dataset *pC, THD_3dim_dataset *pCgN,
                                 float mrfB, float Temp, byte mix, 
                                 THD_3dim_dataset **pcgallp)
{
   static char FuncName[]={"SUMA_pst_C_giv_ALL"};
   short *a=NULL;
   double *p=NULL, *m=NULL, *s=NULL,  *gd, *ds2, *ps=NULL, 
            sp,  BoT, x0, e, PP[64], PG[64], *w=NULL, eN=0.0, pp, pg,
            wconst, wg;
   float af, fpCw, fpC, *fpCgN=NULL;
   int i, k, ni, nj, nk, nij, ijkn[6], shft;
   THD_3dim_dataset *pout = *pcgallp;
   THD_3dim_dataset *pCw=NULL;
   char sbuf[256];
   static int icall=0, iwarn=0;
   
   SUMA_ENTRY;
   
   if (!pout) {
      NEW_SHORTY(aset,cs->N_label,"SUMA_pst_C_giv_ALL",pout);
      *pcgallp = pout;
   }
   
   BoT = mrfB/Temp; /* THIS is not being used ... Check before deleting*/
   
   ni = DSET_NX(aset);
   nj = DSET_NY(aset);
   nk = DSET_NZ(aset);
   nij = ni*nj;
   
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0f) af = 1.0;
   a = (short *)DSET_ARRAY(aset,0);
   p = (double *)SUMA_calloc(cs->N_label*DSET_NVOX(aset), sizeof(double));
   ps = (double *)SUMA_calloc(DSET_NVOX(aset), sizeof(double));
   m = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   s = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   ds2 = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   gd = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   fpCgN = (float*)SUMA_calloc(cs->N_label, sizeof(float));
    
   if (pCgN) {
      GET_BFs(pCgN, fpCgN);
   }

   /* get the global (average) mixing fraction */
   w = SUMA_get_Stats(cs, "mix");
   wconst=1.0/(double)cs->N_label;
   
   /* prepare the voxelwise mixing fraction denominator */
   fpCw = 1/10000.0;
   NEW_SHORTY(aset,cs->N_label,"pCw",pCw); 
   for (k=0; k<cs->N_label; ++k) {
      EDIT_BRICK_FACTOR(pCw,k, fpCw);   
   }
   if (pC) {
      fpC = DSET_BRICK_FACTOR(pC, 0);
      for (i=0; i<DSET_NVOX(aset); ++i) {
         sp = 0.0;
         for (k=0; k<cs->N_label; ++k) {
            if (IN_MASK(cmask, i)) {
               if (mix) wg = w[k];
               else wg = wconst;
               GSCVAL(pC, k, i, fpC, e);
               PP[k] = wg*e;
               if (pCgN) {
                  GSCVAL(pCgN, k, i, fpCgN[k], eN);
                  PP[k] *= eN;
               } 
               sp += PP[k];
            }
         }
         for (k=0; k<cs->N_label; ++k) {
            if (IN_MASK(cmask, i)) {
               PSCVAL(pCw,k,i,fpCw, PP[k]/sp);
            }
         }
      }
   } else {
      /* Just use the global mixing fractions */
      for (k=0; k<cs->N_label; ++k) {
         if (mix) wg = w[k];
         else wg = wconst;
         for (i=0; i<DSET_NVOX(aset); ++i) {
            if (IN_MASK(cmask, i)) {
               if (pCgN) {
                  GSCVAL(pCgN, k, i, fpCgN[k], eN);
                  PSCVAL(pCw,k,i,fpCw, wg*eN);
               } else {
                  PSCVAL(pCw,k,i,fpCw, wg);
               }
            }
         }
      }
   }
   
   
   /* Get class stats */
   for (k=0; k<cs->N_label; ++k) {
      m[k]=SUMA_get_Stat(cs, cs->label[k], "mean");
      s[k]=SUMA_get_Stat(cs, cs->label[k], "stdv");
      ds2[k] = 2.0*s[k]*s[k];
      gd[k] = 1.0/(SQ2PI*s[k]); 
   }
   
   fpCw = DSET_BRICK_FACTOR(pCw, 0);
   for (k=0; k<cs->N_label; ++k) {
      shft = k*DSET_NVOX(aset); PP[k]=0; PG[k]=0; 
      for (i=0; i<DSET_NVOX(aset); ++i) {
         if (IN_MASK(cmask, i)) {
            x0 = (double)a[i]*af - m[k];
            pg = exp(-(x0*x0)/ds2[k])*gd[k];
            
            GSCVAL(pCw, k, i, fpCw, pp); 
           
            p[i+shft] = (pg)*(pp);
            ps[i] += p[i+shft];
               if (i == VoxDbg) { /* store for debugging */
                  PP[k] = pp;
                  PG[k] = pg;
               }
         }
      }
   }
   
   /* and marginalize */
   for (k=0; k<cs->N_label; ++k) {
      shft = k*DSET_NVOX(aset);
      for (i=0; i<DSET_NVOX(aset); ++i) {
         if (IN_MASK(cmask, i)) {
            if (ps[i] == 0.0) {
               p[i+shft] = 1.0/(double)(cs->N_label);
               if (!iwarn) {
                  SUMA_S_Warnv("Have prob. sum of 0.0 at voxel %d\n"
                               "Such voxels should be masked.\n"
                               "Setting all such voxels to %f\n"
                               "Further warnings muted.\n",
                               i, 1.0/cs->N_label);
                  ++iwarn;
               } 
            } else p[i+shft] /= ps[i];
         }
      }     
   }
   SUMA_ifree(ps); SUMA_ifree(fpCgN);
   
   
      if (VoxDbg >= 0) {
         int IJK[3], pp;
         i = VoxDbg;
         Vox1D2Vox3D(i,DSET_NX(aset), DSET_NX(aset)*DSET_NY(aset), IJK);
         fprintf(VoxDbgOut, "at %d %d %d, a=%d (%f)\n", 
                                 IJK[0], IJK[1], IJK[2],
                                 a[i], a[i]*af);
         fprintf(VoxDbgOut, "p(y|params)[]:   ");
         for(k=0; k<cs->N_label; ++k) {
            fprintf(VoxDbgOut, "%f   ", PG[k]);
         }
         fprintf(VoxDbgOut, "\n");
         fprintf(VoxDbgOut, "w[]:   ");
         for(k=0; k<cs->N_label; ++k) {
            fprintf(VoxDbgOut, "%f   ", PP[k]);
         }
         fprintf(VoxDbgOut, "\n\n");
      }   
   
   /* put vector back in pout */
   for (k=0; k<cs->N_label; ++k) {
      EDIT_substscale_brick(pout, k, MRI_double, 
                            (p+k*DSET_NVOX(pout)), MRI_short, -1.0);
      sprintf(sbuf,"P(%s|y)",cs->label[k]);
      EDIT_BRICK_LABEL( pout, k, sbuf);
   }
     
   SUMA_ifree(p);  
   SUMA_ifree(m);  
   SUMA_ifree(s);  
   SUMA_ifree(ds2);  
   SUMA_ifree(gd);  
   DSET_delete(pCw); pCw = NULL;
   
   ++icall;   
   SUMA_RETURN(1);
}

int SUMA_SegInitCset(THD_3dim_dataset *aseti, 
                     THD_3dim_dataset **csetp, 
                     byte *cmask, int cmask_count,
                     char *mixopt, 
                     SUMA_CLASS_STAT *cs,
                     SEG_OPTS *Opt)
{
   static char FuncName[]={"SUMA_SegInitCset"};   
   int ibias = 0, border;
   short *a=NULL;
   double *p=NULL, *m=NULL, *s=NULL,  *gd, *ds2, *ps=NULL, 
            sp,  x0, e, PP[64], PG[64],  pp, pg;
   float af;
   int i, k, shft;
   char sbuf[256];
   THD_3dim_dataset *cset=*csetp, *pstC=NULL, *cset_init=NULL;
   OPT_KMEANS oc;
         
   SUMA_ENTRY;
   
   oc = new_kmeans_oc();
   oc.k = SUMA_Class_k_Selector(cs,
                                "not_classes_string", "OTHER",NULL);
   oc.remap = MAG;
   oc.verb = Opt->debug-1; if (oc.verb < 0) oc.verb = 0;
   oc.distmetric = 'e';
   for (i=0; i<oc.k; ++i) oc.clabels[i] = cs->label[i];
   oc.jobname=SUMA_append_replace_string(Opt->proot?Opt->proot:".", 
                                          FuncName, "/", 0);
   if (!cset) {
      /* Let clustering do the whole deal*/
      oc.r = 3;
      cset_init = NULL;
   } else {
      oc.r = 0;
      /* initialize by cset */
      cset_init = cset;
   }
   if (Opt->debug > 1) {
      SUMA_S_Notev("Calling clustering function %d voxels in mask\n", 
                   cmask_count);
   }
   if (!(thd_Acluster1 (aseti,
               cmask, cmask_count,
               &cset,
               NULL,
               cset_init,
               oc))) {
      SUMA_S_Err("Failed to cluster");
      SUMA_RETURN(0);           
   }   
   if (!cset_init) { /* happens when cset is NULL before thd_Acluster1 call */
      *csetp = cset;
   }
   
   if (Opt->debug > 1) {
      SUMA_S_Note("Stats on clusters");
   }
   /* compute class stats */
   if (!SUMA_Class_stats( aseti, cset, cmask, cmask_count, 
                           NULL, NULL, Opt->gold, cs)) {
      SUMA_S_Err("Failed in class stats");
      SUMA_RETURN(0);
   }

   af = DSET_BRICK_FACTOR(aseti,0); if (af == 0.0f) af = 1.0;
   a = (short *)DSET_ARRAY(aseti,0);
   p = (double *)SUMA_calloc(cs->N_label*DSET_NVOX(aseti), sizeof(double));
   ps = (double *)SUMA_calloc(DSET_NVOX(aseti), sizeof(double));
   m = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   s = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   ds2 = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   gd = (double *)SUMA_calloc(cs->N_label, sizeof(double));

   /* Compute pst(c) = p(y|stats) * mixrac */
   NEW_SHORTY(aseti, cs->N_label, "ini.pstC", pstC);
   for (k=0; k<cs->N_label; ++k) {
      m[k]=SUMA_get_Stat(cs, cs->label[k], "mean");
      s[k]=SUMA_get_Stat(cs, cs->label[k], "stdv");
      ds2[k] = 2.0*s[k]*s[k];
      gd[k] = 1.0/(SQ2PI*s[k]); 
   }
   
   for (k=0; k<cs->N_label; ++k) {
      if (mixopt) {
         pp = SUMA_mixopt_2_mixfrac(mixopt, cs->label[k], 
                             cs->keys[k], cs->N_label,
                             cmask, cset);
         if (pp < 0.0) {
            SUMA_S_Err("Failed to get mixfrac");
            SUMA_RETURN(0);
         }
      } else {
         pp = 1.0/cs->N_label;
      }           
      shft = k*DSET_NVOX(aseti); PP[k]=0; PG[k]=0; 
      for (i=0; i<DSET_NVOX(aseti); ++i) {
         if (IN_MASK(cmask, i)) {
            x0 = (double)a[i]*af - m[k];
            pg = exp(-(x0*x0)/ds2[k])*gd[k];
            p[i+shft] = (pg)*(pp);
            ps[i] += p[i+shft];
               if (i == Opt->VoxDbg) { /* store for debugging */
                  PP[k] = pp;
                  PG[k] = pg;
               }
         }
      }
   }
   
   /* and marginalize */
   for (k=0; k<cs->N_label; ++k) {
      shft = k*DSET_NVOX(aseti);
      for (i=0; i<DSET_NVOX(aseti); ++i) {
         if (IN_MASK(cmask, i)) {
            p[i+shft] /= ps[i];
         }
      }     
   }
   SUMA_ifree(ps); 
   
   /* store p in dset */
   for (k=0; k<cs->N_label; ++k) {
      EDIT_substscale_brick( pstC, k, MRI_double, 
                            (p+k*DSET_NVOX(pstC)), MRI_short, -1.0);
      sprintf(sbuf,"Pinit(%s|y)",cs->label[k]);
      EDIT_BRICK_LABEL( pstC, k, sbuf);
   }

   /* reassign class membership */
   if (!(SUMA_assign_classes( pstC, cs, 
                              cmask, &cset))) { 
      SUMA_S_Err("Failed in assign_classes");
      SUMA_RETURN(0);
   }
   
   /* recompute class stats using this posterior */
   if (!SUMA_Class_stats( aseti, cset, cmask, cmask_count, 
                          pstC, NULL, Opt->gold, cs)) {
      SUMA_S_Err("Failed in class stats");
      SUMA_RETURN(0);
   }

   /* free temps */
   SUMA_ifree(p);  
   SUMA_ifree(m);  
   SUMA_ifree(s);  
   SUMA_ifree(ds2);  
   SUMA_ifree(gd);  
   DSET_delete(pstC); pstC = NULL;
   
   SUMA_RETURN(1); 
}

float SUMA_GetConstFactor(THD_3dim_dataset *pset) {
   int k;
   for (k=1; k<DSET_NVALS(pset); ++k) {
      if (SUMA_ABS(DSET_BRICK_FACTOR(pset,k)-DSET_BRICK_FACTOR(pset,k-1))
               > 0.000001) {
         return(-1.0);
      }
   }
   
   return(DSET_BRICK_FACTOR(pset,0));
}

/*!
   Take an input dset and make sure it is formatted 
   as a shortized probability dset with equal scaling
   factors.
   If the input is a single sub-brick of class indices,
   it creates an binarized prob. dset
*/
int SUMA_ShortizeProbDset(THD_3dim_dataset **csetp, 
                        SUMA_CLASS_STAT *cs, 
                        byte *cmask, int cmask_count, 
                        SEG_OPTS *Opt, 
                        THD_3dim_dataset **psetp)
{
   static char FuncName[]={"SUMA_ClassToProbDset"};
   THD_3dim_dataset *pset = NULL;
   THD_3dim_dataset *cset = *csetp;
   float fpset;
   byte *bb=NULL, shortize=0;
   short *gb=NULL, *C=NULL; 
   int ijk=0, k;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   fpset = 1.0/10000.0;
   
   if (!psetp) {
      SUMA_S_Err("NULL psetp");
      SUMA_RETURN(0);
   }
   pset = *psetp;
   
   if (DSET_NVALS(cset) == 1) { /* gave me classes, make probabilities */
      SUMA_LH("Changing classes to probabilities");
      if (!pset || pset == cset) NEW_SHORTY(cset, cs->N_label, "prior_p", pset);
      if (DSET_NVALS(pset)!=cs->N_label) {
         SUMA_S_Errv("Bad input, have %d vals in pset, need %d\n",
            DSET_NVALS(pset), cs->N_label);
         SUMA_RETURN(0);
      }

      NEW_SHORTY(cset, cs->N_label, "prior_p", pset);
      C = (short *)DSET_ARRAY(cset,0);
      bb = (byte *)SUMA_calloc(DSET_NVOX(cset), sizeof(byte));
      for (k=0; k<cs->N_label; ++k) {
         gb = (short *)DSET_ARRAY(pset,k);
         for (ijk=0; ijk<DSET_NVOX(cset); ++ijk) {
            if (IN_MASK(cmask,ijk) && C[ijk] == cs->keys[k]) {
               gb[ijk] = (short )1.0/fpset; bb[ijk] = 1;
            }
         }
         EDIT_BRICK_FACTOR(pset, k, fpset);
      }
      /* make sure each voxel in mask got something */
      for (k=0; k<cs->N_label; ++k) {
         gb = (short *)DSET_ARRAY(pset,k);
         for (ijk=0; ijk<DSET_NVOX(cset); ++ijk) {
            if (IN_MASK(cmask,ijk) && !bb[ijk]) {
               gb[ijk] = (short )(1.0/fpset/cs->N_label);
            }
         }
      }
      SUMA_ifree(bb); 
   } else {
      /* cset is considered to be a probs. dset */
      pset = cset; *csetp = NULL; /* to guard against multiple copies */
      /* make sure you don't get a bad number of sub-bricks */
      if (DSET_NVALS(pset) != cs->N_label) {
         SUMA_S_Errv( "Bad news in tennis shoes, \n"
                      "have %d sub-bricks in %s and %d labels",
                      DSET_NVALS(pset), DSET_PREFIX(pset), cs->N_label);
         SUMA_RETURN(0);
      }
   }
   
   shortize = 0;
   if (SUMA_GetConstFactor(pset) < 0.0) {  shortize = 1; }
   else {
      for (k=0; k<cs->N_label; ++k) {
         if (DSET_BRICK_TYPE(pset,k) != MRI_short) {
            shortize = 1;
            break;
         }
      }
   }
   
   if (shortize) {
      SUMA_LHv("Shortizing %s\n", DSET_PREFIX(pset));
      if (!SUMA_ShortizeDset(&pset, fpset)) {
         SUMA_S_Err("Failed to shortize");
         SUMA_RETURN(0);
      }
   }

   *psetp=pset;
   SUMA_RETURN(1);
}  

int SUMA_FlattenProb(THD_3dim_dataset *pC, 
                     byte *cmask, int cmask_count, 
                     int mode) 
{
   static char FuncName[]={"SUMA_FlattenProb"};
   int i, k, nbrick=DSET_NVALS(pC);
   double ss, pp;
   float fpC[nbrick];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   GET_BFs(pC,fpC);
   
   switch (mode) {
      case 1:
         for (i=0; i<DSET_NVOX(pC); ++i) {
            if (1 || IN_MASK(cmask,i)) { /* focusing on mask only gives ugly 
                                       edge artifacts, they won't bother with 
                                       segmentation, but they're ugly */
               ss = 0;
               for (k=0; k<nbrick; ++k) {
                  GSCVAL(pC,k,i,fpC[k],pp);
                  ss += pp;
               }
               ss = ss/(double)(nbrick);
               for (k=0; k<nbrick; ++k) {
                  PSCVAL(pC,k,i,fpC[k], ss);
               }
            }
         }
         break;
      default:
         SUMA_S_Err("Not ready for this mode");
         SUMA_RETURN(0);
   }
   
   if (LocalHead) SUMA_Seg_Write_Dset(NULL, "FLAT", pC, -1, NULL); 

   SUMA_RETURN(1);
}

/*!
   Given a prob. dset, add an 'OTHER' class
*/
int SUMA_OtherizeProbDset(THD_3dim_dataset *pC, 
                          byte *cmask, int cmask_count)
{
   static char FuncName[]={"SUMA_OtherizeProbDset"};
   int i, k, nbrick = DSET_NVALS(pC);
   double ss, pp;
   short *cc=NULL;
   float fpC[nbrick+1];
   
   SUMA_ENTRY;
   
   GET_BFs(pC,fpC);
   fpC[nbrick] = 1/10000.0;
   
   cc = (short *)calloc(DSET_NVOX(pC), sizeof(short));
   EDIT_add_brick(pC, MRI_short, fpC[nbrick], cc);
   EDIT_BRICK_LABEL(pC, nbrick, "OTHER");
   for (i=0; i<DSET_NVOX(pC); ++i) {
      if (1 || IN_MASK(cmask,i)) { /* no need to stick to mask here */
         ss = 0;
         for (k=0; k<nbrick; ++k) {
            GSCVAL(pC,k,i,fpC[k],pp);
            ss += pp;
            if (i==1332180) {
               fprintf(stderr,"%d: %f --> %f\n", k, pp, ss);
            }
         }
         ss = (1.0 - SUMA_MIN_PAIR(ss, 1.0));
         PSCVAL(pC, nbrick, i, fpC[nbrick], ss);
         if (i==1332180) {
               GSCVAL(pC,k,i,fpC[nbrick],pp);
               fprintf(stderr,"%d:  --> %f (%f)\n", nbrick,  ss, pp);
            }
      }
   }
   SUMA_RETURN(1);
}

int SUMA_AddOther(  NI_str_array *clss, int **keysp, 
                    byte *cmask, int cmask_count,
                    THD_3dim_dataset *cset, THD_3dim_dataset *pstCgALL,
                    THD_3dim_dataset *pCgA, THD_3dim_dataset *pCgL,
                    SUMA_CLASS_STAT *cs)
{      
   static char FuncName[]={"SUMA_AddOther"};
   int i, mxkey=0;
   int *keys=*keysp;
   short *cc=NULL;
   float fpG;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_S_Warn("Adding OTHER CLASS");
   /* clss */
   clss->num = clss->num+1;
   clss->str = 
      NI_realloc(clss->str, char *, sizeof(char *)*(clss->num));
   clss->str[clss->num-1] = NI_malloc(char, strlen("OTHER")+1);
   strcpy(clss->str[clss->num-1], "OTHER");

   /* keys */
   mxkey = keys[0];
   for (i=1; i<clss->num-1; ++i) {
      if (mxkey < keys[i]) mxkey = keys[i];
   }
   keys = (int *)SUMA_realloc(keys, sizeof(int)*clss->num);
   keys[clss->num-1] = mxkey+1;
   *keysp = keys;
   
   /* cset ? */
   if (cset) {
      cc = DSET_ARRAY(cset,0);
      for (i=0; i<DSET_NVOX(cset); ++i) {
         if (IN_MASK(cmask,i)) {
            if (!cc[i]) cc[i] = mxkey+1;
         }
      }
   }

   /* pstCgALL ? */
   if (pstCgALL) {
      fpG = DSET_BRICK_FACTOR(pstCgALL,0);
      EDIT_add_brick(pstCgALL, MRI_short, fpG, NULL);
      EDIT_substitute_brick( pstCgALL, DSET_NVALS(pstCgALL)-1, 
                              MRI_short, NULL ) ;
      EDIT_BRICK_LABEL(pstCgALL, DSET_NVALS(pstCgALL)-1, "OTHER");
   }

   if (pCgA) {
      if (!SUMA_OtherizeProbDset(pCgA, 
                        cmask, cmask_count)) {
         SUMA_S_Errv("Failed to otherize pCgA %s\n", DSET_PREFIX(pCgA));
         SUMA_RETURN(0);
      }
      if (LocalHead) SUMA_Seg_Write_Dset(NULL, "pCgA-Otherized", pCgA, 
                                         -1, NULL);
   }
   if (pCgL) {
      if (!SUMA_OtherizeProbDset(pCgL, 
                        cmask, cmask_count)) {
         SUMA_S_Errv("Failed to otherize pCgL %s\n", DSET_PREFIX(pCgL));
         SUMA_RETURN(0);
      }
      if (LocalHead) SUMA_Seg_Write_Dset(NULL, "pCgL-Otherized", pCgL, -1, NULL);
   }

   /* cs ? */
   if (cs) {
      if (SUMA_Add_Class_Label(cs, "OTHER", keys[clss->num-1]) < 0) {
         SUMA_S_Err("Failed to SUMA_Add_Class_Label OTHER");
         SUMA_RETURN(0);
      }
   }
      
   SUMA_RETURN(1);      
}

/*!
   A convenience function for SUMA_hist to build a histogram
   for a sub-brick based on a pre-existing histogram
*/
SUMA_HIST *SUMA_dset_hist(THD_3dim_dataset *dset, int ia, 
                          byte *cmask, char *label,
                          SUMA_HIST *href)
{
   static char FuncName[]={"SUMA_dset_hist"};
   int i = 0, N_k = 0;
   float orange[2]={0.0, 0.0}, *fv=NULL;
   SUMA_HIST *hh=NULL;
   
   SUMA_ENTRY;
   
   if (!dset || ia < 0 || ia >= DSET_NVALS(dset)) SUMA_RETURN(hh);
   if (!(fv = THD_extract_to_float(ia, dset))) {
      SUMA_S_Errv("Failed to extract sub-brick %d\n", ia);
      SUMA_RETURN(hh);
   }
   if (cmask) {
      N_k = 0;
      for (i=0; i<DSET_NVOX(dset); ++i) {
         if (cmask[i]) { fv[N_k] = fv[i]; ++N_k; }
      }
   } else {
      N_k = DSET_NVOX(dset);
   }
   if (!label) label = "unloved";
   
   if (href) {
      orange[0] = href->min; orange[1] = href->max;
      hh = SUMA_hist(fv, N_k, href->K, href->W, orange, "lll", 0); 
   } else {
      hh = SUMA_hist(fv, N_k, 0, 0, NULL, "lll",0);
   }
   free(fv); fv = NULL;
   SUMA_RETURN(hh);
}

/*!
   \brief Create histogram from n data values in v
   \param v (float *) vector of values
   \param n (int) number of values in v
   \param Ku (int) if > 0 set the number of bins
   \param Wu (float) if > 0 set the bin width 
               (Ku and Wu are mutually exclusive, Ku takes precedence)
   \param range (float *) if !NULL set min = range[0], max = range[1]
   \param label (char *) what you think it is
   \param ignoreout (int ) if == 0 then values < min are set in bottom bin
                                   and  values > max are set in top bin
                              == 1 then values < min or > max are ignored
*/

SUMA_HIST *SUMA_hist(float *v, int n, int Ku, float Wu, float *range, 
                     char *label, int ignoreout) 
{
   static char FuncName[]={"SUMA_hist"};
   int i=0, minloc, maxloc, ib=0;
   float min=0.0, max=0.0;
   SUMA_HIST *hh=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!v) SUMA_RETURN(hh);
   if (n < 10) {
      SUMA_S_Errv("A hist with n = %d samples!!!\n", n);
      SUMA_RETURN(hh); 
   }
   if (!range) {
      SUMA_MIN_MAX_VEC(v,n,min, max, minloc, maxloc);
      if (min == max) {
         SUMA_S_Errv("Single value of %f in samples. No good.\n", min);
         SUMA_RETURN(hh); 
      }
   } else {
      min = range[0]; max = range[1];
   }
      
   hh = (SUMA_HIST *)SUMA_calloc(1, sizeof(SUMA_HIST));
   hh->max = max;
   hh->min = min;
   hh->n = n;
   if (label) hh->label = SUMA_copy_string(label);
   
   if (Ku > 0) { /* user sets number of bins */
      hh->K = Ku;
      hh->W = (max-min)/(float)hh->K;
   } else if (Wu > 0.0) {
      hh->W = Wu;
      hh->K = (int)ceil((max-min)/hh->W);
   } else {
      hh->K = sqrt(n);
      hh->W = (max-min)/(float)hh->K;
   }
   
   SUMA_LHv("Window %f, Bins %d, min %f max %f\n",
            hh->W, hh->K , min, max);
   /* initialize */
   hh->b = (float *)SUMA_calloc(hh->K, sizeof(float));
   hh->c = (int *)SUMA_calloc(hh->K, sizeof(int));
   hh->cn = (float *)SUMA_calloc(hh->K, sizeof(float));
   
   /* fill it */
   hh->N_ignored = 0;
   if (ignoreout) {
      for (i=0; i<n;++i) {
         if (v[i]>=min && v[i]<=max) {
            ib = (int)((v[i]-min)/hh->W); if (ib==hh->K) ib = hh->K-1;
            ++hh->c[ib];
         } else {
            ++hh->N_ignored;
         }
      }
   } else {
      for (i=0; i<n;++i) {
         ib = (int)((v[i]-min)/hh->W);
         if (ib>=hh->K) ib = hh->K-1;
         else if (ib < 0) ib = 0;
         ++hh->c[ib];
      }
   }
   
   /* for convenience */
   for (i=0; i<hh->K;++i) {
      hh->b[i] = hh->min+(i+0.5)*hh->W;
      hh->cn[i] = (float)hh->c[i]/(float)n;
   }
      
   SUMA_RETURN(hh);
}

SUMA_HIST *SUMA_hist_opt(float *v, int n, int Ku, float Wu, float *range, 
                     char *label, int ignoreout, 
                     float oscfreqthr, char *methods) 
{
   static char FuncName[]={"SUMA_hist_opt"};
   int i=0, minloc, maxloc, ib=0, N_iter=0, N_itermax=10;
   float min=0.0, max=0.0, orange[2]={0.0, 0.0}, mxcn=0.0, osfrac=0.0;
   float minmaxfrac=0.0, oscfracthr=0.0;
   SUMA_HIST *hh=NULL, *hhn=NULL;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   hh = SUMA_hist(v,n,Ku,Wu,range,label,ignoreout);
   if (!hh) SUMA_RETURN(hh);
   if (!methods) {
      methods = "Range|OsciBinWidth";
   }

   if (strstr(methods, "Range")) {
      Ku = 0; /* if set by user, should be loosened because we're controlling
                 range and binwidth here */
      /* try tightening the range */
      if ((float)hh->N_ignored/(float)hh->n > 0.01) {
         SUMA_LHv("For histogram %s, %.2f%% of the samples were\n"
                      "ignored for being outside the range [%f %f]\n"
                      "Trying range tightening\n",
                hh->label,
                100*(float)hh->N_ignored/(float)hh->n, 
                hh->min, hh->max);
         /* try to shrink the range and repeat */
         mxcn = SUMA_hist_perc_freq(hh, 50, 1, NULL);
         i = 0; 
         while (i < hh->K && hh->cn[i]/mxcn < 0.001) ++i;
         orange[0] = hh->b[i]-0.5*hh->W;
         i = hh->K-1;
         while (i > 0 && hh->cn[i]/mxcn < 0.001) --i;
         orange[1] = hh->b[i]+0.5*hh->W;
         if (orange[0] != hh->min || orange[1] != hh->max) {
            /* retry with new range */
            SUMA_LHv("Retrying with n=%d, Ku=%d, Wu=%f, orange=[%f %f]\n",
                     n, Ku, Wu, orange[0], orange[1]);
            if ((hhn = SUMA_hist(v,n,Ku,Wu,orange,label,ignoreout))) {
               SUMA_Free_hist(hh); hh=hhn; hhn=NULL;
            } else {
               SUMA_S_Err("Unexpected error, returning with what I have");
               SUMA_RETURN(hh);
            }
         } 
      } 

      /* Still no good, range has to increase */
      N_iter = 0; N_itermax = 25;
      while (((float)hh->N_ignored/(float)hh->n > 0.01) && N_iter <= N_itermax) {
         if (N_iter < N_itermax) {
            SUMA_LHv("For histogram %s, %.2f%% of the samples were\n"
                         "ignored for being outside the range [%f %f]\n"
                         "Attempting range expansion, iteration %d\n",
                   hh->label,
                   100*(float)hh->N_ignored/(float)hh->n, 
                   hh->min, hh->max, N_iter);
            /* try to increase the range the range and repeat */
            mxcn = SUMA_hist_perc_freq(hh, 50, 1, NULL);
            if (hh->cn[hh->K-1] > hh->cn[0]) orange[1] += hh->K/20.0*hh->W;
            else if (hh->cn[hh->K-1] < hh->cn[0]) orange[0] -= hh->K/20.0*hh->W;
            else {
               orange[0] -= hh->K/20.0*hh->W;
               orange[1] += hh->K/20.0*hh->W;
            }
            if (orange[0] != hh->min || orange[1] != hh->max) {
               /* retry with new range */
               SUMA_LHv("Retry with Ku=%d, Wu=%f, orange=[%f %f], iter %d\n",
                        Ku, Wu, orange[0], orange[1], N_iter);
               if ((hhn = SUMA_hist(v,n,0,Wu,orange,label,ignoreout))) {
                  SUMA_Free_hist(hh); hh=hhn; hhn=NULL;
               } else {
                  SUMA_S_Err("Unexpected error, returning with what I have");
                  SUMA_RETURN(hh);
               }
            }
         } else {
            SUMA_S_Warnv("For histogram %s, %.2f%% of the samples were\n"
                         "ignored for being outside the range [%f %f]\n"
                         "Range expansion halted after %d iterations\n",
                   hh->label,
                   100*(float)hh->N_ignored/(float)hh->n, 
                   hh->min, hh->max, N_itermax);
         }
         ++N_iter; 
      }
   }
   
   /* bin oscillations? */
   if (oscfreqthr >= 0.0 && strstr(methods,"Osci")) {
      if (oscfreqthr == 0.0f) oscfreqthr = 0.3;
      N_iter = 0; N_itermax = 10;
      while ((osfrac = SUMA_hist_oscillation(hh, minmaxfrac, oscfracthr)) 
                        > oscfreqthr &&
             N_iter <= N_itermax) {
         if (N_iter < N_itermax) {
            if (strstr(methods,"OsciSmooth")) { /* by smoothing */
            SUMA_LHv("Histogram %s oscifraq = %f, needs smoothing, "
                         "iter %d\n",
                        hh->label, osfrac, N_iter);
            SUMA_hist_smooth(hh,1);
            } else if (strstr(methods,"OsciBinWidth")){
               SUMA_LHv("Histogram %s oscifraq = %f needs bin adjustment,"
                            " iter %d\n",
                            hh->label,osfrac,  N_iter);
               Wu = hh->W*1.1; orange[0] = hh->min; orange[1] = hh->max;
               /* retry with new width */
               SUMA_LHv("Retry with Ku=%d, Wu=%f, orange=[%f %f], iter %d\n",
                        Ku, Wu, orange[0], orange[1], N_iter);
               if ((hhn = SUMA_hist(v,n,0,Wu,orange,label,ignoreout))) {
                  SUMA_Free_hist(hh); hh=hhn; hhn=NULL;
               } else {
                  SUMA_S_Err("Unexpected error, returning with what I have");
                  SUMA_RETURN(hh);
               }
            } else {
               SUMA_S_Errv("Bad Osci option in %s\n", methods);
               SUMA_RETURN(hh);
            }
         } else {
            SUMA_S_Warnv("Histogram %s oscifraq = %f still needs fixing"
                         " but iterations are exhasuted at %d\n",
                         hh->label,osfrac,  N_itermax); 
         }
         ++N_iter;
      }
   }
   /* you probably want to consider the range again, perhaps loop back
      We'll see if that will be needed.*/
   
   SUMA_RETURN(hh);
}

int SUMA_hist_smooth( SUMA_HIST *hh, int N_iter ) 
{
   static char FuncName[]={"SUMA_hist_smooth"};
   float *fbuf=NULL, *fbufn=NULL;
   int i, iter=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!hh) SUMA_RETURN(NOPE);
   
   if (N_iter == 0) N_iter = 1;
   
   iter = 0;
   while (iter < N_iter) {
      if (!fbuf) fbuf = (float *)SUMA_calloc(hh->K, sizeof(float));
      if (!fbufn) fbufn = (float *)SUMA_calloc(hh->K, sizeof(float)); 

      fbuf[0] = (hh->c[0]+hh->c[1])/2.0;
      fbuf[hh->K-1] = (hh->c[hh->K-1]+hh->c[hh->K-2])/2.0;
      fbufn[0] = (hh->cn[0]+hh->cn[1])/2.0;
      fbufn[hh->K-1] = (hh->cn[hh->K-1]+hh->cn[hh->K-2])/2.0;
      if (fbuf[0] > fbuf[hh->K-1]) {
         hh->min = fbuf[hh->K-1]; hh->max = fbuf[0];
      } else {
         hh->max = fbuf[hh->K-1]; hh->min = fbuf[0];
      }
      for (i=1; i<hh->K-1; ++i) {
         fbuf[i] = (hh->c[i-1]+hh->c[i]+hh->c[i+1])/3.0;
         fbufn[i] = (hh->cn[i-1]+hh->cn[i]+hh->cn[i+1])/3.0;
         if (fbuf[i]>hh->max) hh->max = fbuf[i];
         else if (fbuf[i]<hh->min) hh->min = fbuf[i];
      }
      memcpy(hh->cn, fbufn, hh->K*sizeof(float));
      memcpy(hh->c, fbuf, hh->K*sizeof(float));
      ++iter;
   }
   
   if (hh->isrt) { /* no longer valid */
      SUMA_free(hh->isrt); hh->isrt = NULL;
   }
   if (fbuf) SUMA_free(fbuf); fbuf=NULL;
   if (fbufn) SUMA_free(fbufn); fbufn=NULL;
   
   SUMA_RETURN(YUP);
}

/*!
   Look for oscillation (bad bin width) in histogram
   
   minmaxfrac : Do not look for oscillation if a bin frequency is 
                is less than minmaxfrac*max_frequency(histogram)
                Set to 0.0 to get default of 0.001
   oscfracthr : Do not consider a bin to exhibit oscillation if
                the average of the absolute differences at that location
                divided by the bin's frequency is less than oscfracthr
                Set to 0.0 to get default of 0.3
   Returns the fraction of bins exhibiting oscillation from the total
   number of candidate bins (those exceeding minmaxfrac)
*/
float SUMA_hist_oscillation( SUMA_HIST *hh, 
                             float minmaxfrac, float oscfracthr)
{
   static char FuncName[]={"SUMA_hist_oscillation"};
   int iosc=0,mxosc,i=0;
   double db, df, oscfrac, mx=0.0, oscfreq=0.0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (minmaxfrac==0.0f) minmaxfrac = 0.001;
   if (oscfracthr==0.0f) oscfracthr = 0.3;
   
   mx = SUMA_hist_perc_freq(hh,50,1,NULL);
   if (mx == 0.0f) SUMA_RETURN(YUP);
   
   iosc=0; mxosc=0;
   for (i=1; i<hh->K-1;++i) {
      if (hh->cn[i]/mx > minmaxfrac) {
         ++mxosc;
         db = hh->cn[i] - hh->cn[i-1];
         df = hh->cn[i] - hh->cn[i+1];
         oscfrac = (SUMA_ABS(db)+SUMA_ABS(df))/(2*hh->cn[i]);
         if (db*df > 0 && oscfrac > oscfracthr) {
            SUMA_LHv("Oscfrac at bin %d of %f = %f\n", 
                         i, hh->cn[i], oscfrac);
            ++iosc;
         }
      }
   }
   oscfreq = 0;
   if (mxosc) oscfreq = (float)iosc/mxosc; 
   SUMA_LHv("Osci frac of histogram %s is %f\n", 
                  hh->label, oscfreq);
   
   SUMA_RETURN(oscfreq);
}


float SUMA_hist_freq(SUMA_HIST *hh, float vv)
{
   float a = 0.0;
   int i0;
   if (!hh) return(-1.0);
   if (vv<hh->b[0]) return(hh->cn[0]);
   if (vv>hh->b[hh->K-1]) return(hh->cn[hh->K-1]);
   a = ((vv-hh->b[0])/hh->W);
   i0 = (int)a; a = a-i0;
   return(a*hh->cn[i0+1]+(1.0-a)*hh->cn[i0]);
}

SUMA_HIST *SUMA_Free_hist(SUMA_HIST *hh)
{
   static char FuncName[]={"SUMA_Free_hist"};
   SUMA_ENTRY;
   if (hh) {
      if (hh->b) SUMA_free(hh->b);
      if (hh->c) SUMA_free(hh->c);
      if (hh->cn) SUMA_free(hh->cn);
      if (hh->label) SUMA_free(hh->label);
      if (hh->isrt) SUMA_free(hh->isrt);
      SUMA_free(hh); hh=NULL;
   }
   SUMA_RETURN(NULL);
}

/* 
   Sorts the histogram frequencies and returns the desired percentile
*/
float SUMA_hist_perc_freq(SUMA_HIST *hh, float perc, int norm, int *iperc)
{
   static char FuncName[]={"SUMA_hist_perc_freq"};
   float ff = -1.0, *vvv=NULL;
   int ides=-1;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   if (iperc) *iperc = -1;
   if (!hh) SUMA_RETURN(ff);
   
   /* sort the frequencies */
   if (!hh->isrt) {
      vvv = (float *)SUMA_calloc(hh->K,sizeof(float));
      memcpy(vvv, hh->cn, hh->K*sizeof(float));
      if (!(hh->isrt = SUMA_z_qsort ( vvv , hh->K ))) {
         SUMA_free(vvv);
         SUMA_S_Err("Failed to sort");
         SUMA_RETURN(ff);
      }
      SUMA_free(vvv); vvv=NULL;
   }
   
   /* get the percentile */
   ides = SUMA_ROUND(perc/100.0*hh->K) - 1;
   if (ides < 0) ides = 0;
   else if (ides > hh->K-1) ides = hh->K-1;
   
   if (iperc) *iperc = hh->isrt[ides];
   if (norm) ff = hh->cn[hh->isrt[ides]];
   else ff = hh->cn[hh->isrt[ides]];
   
   SUMA_RETURN(ff);    
}

char *SUMA_hist_info(SUMA_HIST *hh, int norm, int level)
{
   static char FuncName[]={"SUMA_hist_info"};
   int i, mx, nc;
   float gscl=0.0;
   SUMA_STRING *SS=NULL;
   char *sss=NULL, *s=NULL;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
      
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (!hh) SS = SUMA_StringAppend(SS,"NULL hh");
   else {
      mx = 0;
      for (i=0; i<hh->K; ++i) {
         if (hh->c[i]>mx) mx = hh->c[i];
      }
      if (mx > 50) {
         gscl = mx/50.0;
         mx = 50;
      } else gscl = 1.0;
      
      sss = (char *)SUMA_calloc(mx+2, sizeof(char));
      for (i=0; i<mx; ++i) sss[i]='*'; sss[i]='\0';
      
      SS = SUMA_StringAppend_va(SS,"Histog %s, %d bins of width %f,"
                              "N_samp. = %d, N_ignored = %d, range = [%f,%f]\n",
                                    hh->label?hh->label:"NO LABEL",
                                    hh->K, hh->W, 
                                    hh->n, hh->N_ignored, hh->min, hh->max);
      SUMA_LH("About to freq at midrange");
      SS = SUMA_StringAppend_va(SS,"Freq at mid range %f is: %f\n",
               (hh->min+hh->max)/2.0, SUMA_hist_freq(hh,(hh->min+hh->max)/2.0));
      for (i=0; i<hh->K; ++i) {
         if (norm) {
            SS = SUMA_StringAppend_va(SS,"   %.5f, %.5f:", hh->b[i], hh->cn[i]);
         } else {
            SS = SUMA_StringAppend_va(SS,"   %.5f, %8d:", hh->b[i], hh->c[i]);
         }
         nc = (int)((float)hh->c[i]/gscl+0.5);
         sss[nc]='\0';
         SS = SUMA_StringAppend_va(SS,"%s\n", sss); 
         sss[nc]='*';  
      }
      SUMA_free(sss); sss=NULL;
   } 
   
   SUMA_SS2S(SS, s);
   SUMA_RETURN(s);
}

char *SUMA_dist_info(SUMA_FEAT_DIST *FD, int level)
{
   static char FuncName[]={"SUMA_dist_info"};
   int i, mx, nc;
   float gscl=0.0;
   SUMA_STRING *SS=NULL;
   char *sss=NULL, *s=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (!FD) SS = SUMA_StringAppend(SS,"NULL dist struct!");
   else {
      SS = SUMA_StringAppend_va(SS, "Distribution %s\n", FD->label);
      switch (FD->tp) {
         case SUMA_FEAT_GAMMA:
            SS = SUMA_StringAppend_va(SS, "type gamma (shape %f, rate %f)\n"
                                          "feature scale %f, shift %f\n", 
                                          FD->par[0], FD->par[1],
                                          FD->scpar[0], FD->scpar[1]);
            if (FD->hh) {
               sss = SUMA_hist_info(FD->hh, 1, 1);
               SS = SUMA_StringAppend_va(SS, "histogram:\n%s\n", sss);
               SUMA_free(sss); sss = NULL;
            }
            break;
         case SUMA_FEAT_NP:
            SS = SUMA_StringAppend(SS, "type non-parametric\n");
            if (FD->hh) {
               sss = SUMA_hist_info(FD->hh, 1, 1);
               SS = SUMA_StringAppend_va(SS, "%s\n", sss);
               SUMA_free(sss); sss = NULL;
            } else {
               SS = SUMA_StringAppend(SS,"NULL histogram!\n");
            }
            break;
         default:
            SS = SUMA_StringAppend_va(SS,"Not ready for type %d\n", FD->tp);
            break;
      }
   } 
   
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(s);
}

char *SUMA_dists_info(SUMA_FEAT_DISTS *FDV, int level)
{
   static char FuncName[]={"SUMA_dists_info"};
   int i, mx, nc;
   float gscl=0.0;
   SUMA_STRING *SS=NULL;
   char *sss=NULL, *s=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (!FDV) SS = SUMA_StringAppend(SS,"NULL dist struct!");
   else {
      SS = SUMA_StringAppend_va(SS, "%d distributions in FDV.\n", FDV->N_FD);
      for (i=0; i<FDV->N_FD; ++i) {
         SS = SUMA_StringAppend_va(SS, "  Distribution %d/%d for %s\n", 
                                       i, FDV->N_FD, FDV->FD[i]->label);
         if (level) {
            sss = SUMA_dist_info(FDV->FD[i],level);
            SS = SUMA_StringAppend_va(SS, "%s\n", sss);
            SUMA_free(sss); sss = NULL;
         }  
      }
   }
   
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(s);
}

void SUMA_Show_hist(SUMA_HIST *hh, int norm, FILE *out) 
{
   static char FuncName[]={"SUMA_Show_hist"};
   int i, mx, nc;
   float gscl=0.0;
   char  *s=NULL;
   
   SUMA_ENTRY;
   
   if (!out) out = SUMA_STDOUT;
   
   s = SUMA_hist_info(hh, norm, 1);
   
   fprintf(out, "%s\n", s);
   
   SUMA_free(s); s = NULL;

   SUMA_RETURNe;
}

void SUMA_Show_dist(SUMA_FEAT_DIST *FD, FILE *out) 
{
   static char FuncName[]={"SUMA_Show_dist"};
   int i, mx, nc;
   float gscl=0.0;
   char  *s=NULL;
   
   SUMA_ENTRY;
   
   if (!out) out = SUMA_STDOUT;
   
   s = SUMA_dist_info(FD, 1);
   
   fprintf(out, "%s\n", s);
   
   SUMA_free(s); s = NULL;

   SUMA_RETURNe;
}

void SUMA_Show_dists(SUMA_FEAT_DISTS *FDV, FILE *out, int level) 
{
   static char FuncName[]={"SUMA_Show_dists"};
   int i, mx, nc;
   float gscl=0.0;
   char  *s=NULL;
   
   SUMA_ENTRY;
   
   if (!out) out = SUMA_STDOUT;
   
   s = SUMA_dists_info(FDV, level);
   
   fprintf(out, "%s\n", s);
   
   SUMA_free(s); s = NULL;

   SUMA_RETURNe;
}



/*!
   Initialize all voxels in a dset.
   \param aset: Dataset to initialize 
   \param val: Vector of values, one for each sub-brick
               or just one value for all sub-bricks
   \param nval: Either 1 or DSET_NVALS(aset)
   \param cmask
   \param setsf: if 1, then set the scaling factor based on vv
                for dsets that are shorts
   \return 0 bad, 1 good
   
*/
int SUMA_InitDset(THD_3dim_dataset  *aset, float *val, int nval,
                  byte *cmask, byte setsf)
{
   static char FuncName[]={"SUMA_InitDset"};
   int i, k;
   float vv, *fv, fsc;
   
   SUMA_ENTRY;
   
   for (k=0; k<DSET_NVALS(aset); ++k) {
      if (!val) vv = 0.0;
      else if (nval > 1) vv = val[k];
      else vv = *val; 
      fsc = DSET_BRICK_FACTOR(aset,k);
      if (fsc == 0.0) fsc = 1.0;
      switch (DSET_BRICK_TYPE(aset,k)) {
         case MRI_float:
            fv = (float *)DSET_ARRAY(aset,k);
            for (i=0; i<DSET_NVOX(aset); ++i) {
               if (IN_MASK(cmask,i)) {
                  fv[i] = vv;
               }
            }
            break;
         case MRI_short:
            if (setsf) {
               if (vv != 0.0) fsc = vv/32767.0;
               EDIT_BRICK_FACTOR(aset,k,fsc);
            }
            for (i=0; i<DSET_NVOX(aset); ++i) {
               if (IN_MASK(cmask,i)) {
                  PSCVAL(aset, k, i, fsc, 1.0);
               }
            }
            break; 
         default:
            SUMA_S_Errv("Not dealing with type %d\n",
                        DSET_BRICK_TYPE(aset,k)); 
            SUMA_RETURN(0);
      }
   }
   
   SUMA_RETURN(1);
   
}

/*!
   Produce Class priors:
      \param cs : Class stats from which mixing fractions will be used
                  if no Location- or Spatial-based priors are provided
      \param Aset: The anatomical dset, for a grid template
      \param priCgA: Prior of C given A (feature)
      \param wA: Weight of feature priors 
      \param priCgL: Prior of C given L (location)
      \param wL: Weight of Location priors
         wA + wL = 1.0
      \param priCgALLp: Pointer to dset pointer where results will be placed
                        upon the function's return. If *priCgALLp == NULL,
                        a new dset is created, otherwise dset is recycled.
      \param Opt: The catchall structure of options, just for debugging params.
      \return :0 Bad, 1 good
    
    If (priCgA and priCgL) {
      For all k in classes != OTHER
         priCgALLp[k] = (0.05+0.85*priCgA[k])^wA * (0.05+0.85*priCgL[k])^wL / 
                           SUM(priCgALLp[k!=OTHER])
            (0.05+0.85*p) is to keep one prior from nulling another.
      For k == OTHER
         priCgALLp[OTHER] = priCgL[OTHER];
         (and readjust priCgALLp[k!=OTHER] so that SUM(priCgALLp[.]) = 1.0
   } else if (priCgA) {
      priCgALLp[k] = priCgA[k]
   } else if (priCgL) {
      priCgALLp[k] = priCgL[k]
   } else {
      priCgALLp[k] = cs->mix[k] (constant at all voxels)
   }
         
*/
int SUMA_MergeCpriors(SUMA_CLASS_STAT *cs, byte *cmask,
                                      THD_3dim_dataset  *Aset,
                                      THD_3dim_dataset  *priCgA, float wA,
                                      THD_3dim_dataset  *priCgL, float wL,
                                      THD_3dim_dataset  **priCgALLp,
                                      SEG_OPTS *Opt)
{
   static char FuncName[]={"SUMA_MergeCpriors"};
   float *fPCGA = NULL, *fPCGL=NULL, *fpriCgALL=NULL;
   int k, ijk;
   double sdf, df, dfA, dfL, *ggkk=NULL,*mixfrac=NULL;
   THD_3dim_dataset  *priCgALL = *priCgALLp;
   
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   /* merge priors */
   if (!priCgALL) {
      NEW_SHORTY(Aset, cs->N_label, "InitialPriors", priCgALL);
      *priCgALLp = priCgALL;
   } 
   
   fpriCgALL = (float *)SUMA_calloc(cs->N_label, sizeof(float));   
   fPCGA = (float *)SUMA_calloc(cs->N_label, sizeof(float));   
   fPCGL = (float *)SUMA_calloc(cs->N_label, sizeof(float));   
   ggkk   = (double*)SUMA_calloc(cs->N_label, sizeof(double));   
   
   for (k=0; k<cs->N_label; ++k) EDIT_BRICK_FACTOR(priCgALL, k, 1.0/10000.0);
   GET_BFs(priCgALL,fpriCgALL);

   
   if (priCgA) {
      GET_BFs(priCgA,fPCGA);
   }
   if (priCgL) {
      GET_BFs(priCgL, fPCGL); 
   }
   if (priCgA && priCgL) { /* have both types of priors, combine */
       double dfA, dfL;
       int UseK[cs->N_label], N_kok, uk, ko;
       
       if (wA == -1.0) {
         wA = 0.8; wL = 0.2;
       }
       /* find which classes can be merged */
       SUMA_LHv("Mixing %f priCgA & %f priCgL\n",
               wA, wL);
       if ((N_kok = SUMA_Class_k_Selector(cs, 
            "not_classes_string", "OTHER",  UseK))<0) {
         SUMA_S_Err("No classes found"); SUMA_RETURN(0);
       }
       for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
         if (IN_MASK(cmask, ijk)) {
            sdf = 0.0;
            for (uk=0; uk<N_kok; ++uk) {
               k = UseK[uk];
               GSCVAL(priCgA, k, ijk, fPCGA[k], dfA);
               GSCVAL(priCgL, k, ijk, fPCGL[k], dfL);
               /* pad dfA and dfL to guard against multiplying with 0 */
               dfA = 0.05+0.85*dfA; 
               dfL = 0.05+0.85*dfL; 
               ggkk[k] = pow(dfA,wA)*pow(dfL,wL);
               sdf+=ggkk[k];
            }
            for (uk=0; uk<N_kok; ++uk) {
               k = UseK[uk];
               PSCVAL(priCgALL, k, ijk, fpriCgALL[k], (ggkk[k]/sdf));
               if (Opt->VoxDbg == ijk) {
                  GSCVAL(priCgA, k, ijk, fPCGA[k], dfA);
                  GSCVAL(priCgL, k, ijk, fPCGL[k], dfL);
                  SUMA_S_Notev("At %d %d %d:\n"
                               "%s, priCgA=%fx%f, priCgL=%fx%f, pC=%f \n", 
                        Opt->VoxDbg3[0], Opt->VoxDbg3[1], Opt->VoxDbg3[2],
                        cs->label[k], dfA, wA, dfL, wL, ggkk[k]/sdf); 
               }
            }
         }
      }
      ko = SUMA_Class_k_Label_Locator(cs, "OTHER");
      if (ko >= 0) {
         SUMA_S_Note("Imposing OTHER class from pCgL onto pC");
         for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
            if (IN_MASK(cmask, ijk)) {
               GSCVAL(priCgL, ko, ijk, fPCGL[ko], dfL);
               for (uk=0; uk<N_kok; ++uk) {
                  k = UseK[uk];
                  GSCVAL(priCgALL, k, ijk, fpriCgALL[k], df);
                  PSCVAL(priCgALL, k, ijk, fpriCgALL[k], (df - df*dfL));
               }
               PSCVAL(priCgALL, ko, ijk, fpriCgALL[ko],dfL);
            }
         }
      }
   } else {
      if (priCgA) {
         for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
            for (k=0; k<cs->N_label; ++k) {
               GSCVAL(priCgA, k, ijk, fPCGA[k], df);
               PSCVAL(priCgALL, k, ijk, fpriCgALL[k], df);
            }
         }
      } else if (priCgL) {
         for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
            for (k=0; k<cs->N_label; ++k) {
               GSCVAL(priCgL, k, ijk, fPCGL[k], df);
               PSCVAL(priCgALL, k, ijk, fpriCgALL[k], df);
            }
         }
      } else if ((mixfrac=SUMA_get_Stats(cs, "mix"))) {
         for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
            for (k=0; k<cs->N_label; ++k) {
               PSCVAL(priCgALL, k, ijk, fpriCgALL[k], mixfrac[k]);
            }
         }
      } else if ((mixfrac=SUMA_get_Stats(cs, "mix.init"))) {
         for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
            for (k=0; k<cs->N_label; ++k) {
               PSCVAL(priCgALL, k, ijk, fpriCgALL[k], mixfrac[k]);
            }
         }
      } else  {
         for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
            for (k=0; k<cs->N_label; ++k) {
               PSCVAL(priCgALL, k, ijk, fpriCgALL[k], 1.0/(double)cs->N_label);
            }
         }
      }    
   }
   
   SUMA_ifree(fpriCgALL); SUMA_ifree(fPCGA); SUMA_ifree(fPCGL); SUMA_ifree(ggkk);
   
   SUMA_RETURN(1);
}

/*!
   Compare a field bias estimate to the true estimate.
      rat = (ideal/mean_ideal - estimate/mean_estimate) /
               (ideal/mean_ideal)
   \param gold_bias: Reference field bias
   \param bias: Estimate field bias
   \param cmask: mask region
   \param cmask_count: Number of vox. in mask
   \param thresh: Threshold for summary stat
   \param prat: If !NULL, this will contain the ratio
               of the two volumes.
   \param bad_count: The fraction of voxels in the mask where 
                  rat >= thresh
*/
 
double SUMA_CompareBiasDsets(THD_3dim_dataset *gold_bias, THD_3dim_dataset *bias,
                         byte *cmask, int cmask_count, 
                         float thresh, THD_3dim_dataset *prat )
{
   static char FuncName[]={"SUMA_CompareBiasDsets"};
   float fprat = 1.0/3200, fgi, fgd;
   double md, mi, rat, ai, ad, bad_count;
   int ii; 
   SUMA_ENTRY;
   
   if (!gold_bias || !bias) {
      SUMA_RETURN(-1);
   }
   
   fgi = DSET_BRICK_FACTOR(gold_bias, 0);
   fgd = DSET_BRICK_FACTOR(bias, 0);
   md = 0.0; mi = 0.0;
   for (ii=0; ii<DSET_NVOX(bias); ++ii) {
      if ( IN_MASK(cmask, ii) ) {
         GVAL(gold_bias, 0, ii, ai);
         GVAL(bias, 0, ii, ad);
         md += ad;
         mi += ai;
      }
   }
   md *=  ((double)fgd/(double)cmask_count);
   mi *=  ((double)fgi/(double)cmask_count);
   
   bad_count = 0.0;
   for (ii=0; ii<DSET_NVOX(bias); ++ii) {
      if ( IN_MASK(cmask, ii) ) {
         GSCVAL(gold_bias, 0, ii, fgi, ai);
         ai /= mi;
         GSCVAL(bias, 0, ii, fgd, ad);
         ad /= md;
         rat = (ai-ad)/ai;
         if (prat) {
            PSCVAL(prat, 0, ii, fprat, rat);
         }
         if (SUMA_ABS(rat) >= thresh) ++bad_count;
      }
   }
   
   bad_count = bad_count/(double)cmask_count*100.0;
   if (prat) {
      EDIT_BRICK_FACTOR(prat, 0 , fprat);
   }
   
   SUMA_RETURN(bad_count);
}

/*!
   Compute the Dice coefficient between base and segmentation volumes
   \param base: Gold std segmentaion
   \param seg: segmentation
   \param cmask: Restrict all to cmask 
   \param mask_by_base: If (1) then exclude locations where base == 0,
                        even if this location is in cmask
   \param cs: The Dice coefficient is stored as stat "DICE" in cs
Options only used when Seg's classes are split into sub-classes of 
   classes in base. It is assumed that cs contains stats for the 
   all the split classes.
*/
int SUMA_CompareSegDsets(THD_3dim_dataset *base, THD_3dim_dataset *seg,
                         byte *cmask, byte mask_by_base,
                         SUMA_CLASS_STAT *cs)
{
   static char FuncName[]={"SUMA_CompareSegDsets"};
   int ii=0, kk=0, nbb, nss, nmatch, gk=0;
   short *bb=NULL, *ss=NULL, *ssc=NULL;
   float bf = 1.0, sf=1.0;
   double *sp2grp=NULL;
   SUMA_ENTRY;
   
   if (!base) {
      for (kk=0; kk<cs->N_label; ++kk) {
         SUMA_set_Stat(cs, cs->label[kk], "DICE", 0.0);
      }
   }
   
   sf = DSET_BRICK_FACTOR(seg,0); if (sf == 0.0f) sf = 1.0;
   ss = (short *)DSET_ARRAY(seg,0);
   
   sp2grp = SUMA_get_Stats(cs, "GRkey");
   if (sp2grp) { /* have split classes, merge them */
      ssc = (short *)SUMA_calloc(sizeof(short), DSET_NVOX(seg));
      for (kk=0; kk<cs->N_label; ++kk) {
         for (ii=0; ii<DSET_NVOX(seg); ++ii) {
            if (IN_MASK(cmask,ii) && ss[ii]*(int)sf==cs->keys[kk]) {
               ssc[ii] = (int)sp2grp[kk];
            }
         }
      }
      sf = 1.0;
      ss = ssc;
   }
    
   bf = DSET_BRICK_FACTOR(base,0); if (bf == 0.0f) bf = 1.0;
   bb = (short *)DSET_ARRAY(base,0);
   for (kk=0; kk<cs->N_label; ++kk) {
      nmatch = 0; nss=0; nbb=0;
      if (sp2grp) gk = (int)sp2grp[kk];
         else gk = cs->keys[kk];
      for (ii=0; ii<DSET_NVOX(base); ++ii) {
         if ( IN_MASK(cmask, ii) && 
              (!mask_by_base || bb[ii]) ) {
            if ((ss[ii]*(int)sf) == gk) ++nss;
            if ((bb[ii]*(int)bf) == gk) {
               ++nbb;
               if (bb[ii] == ss[ii]) ++nmatch;
            }
         }
      }
      SUMA_set_Stat(cs, cs->label[kk], "DICE", (double)(nmatch*2)/(nss+nbb));
   }
   
   if (ssc) SUMA_free(ssc); ssc=NULL;    
   SUMA_RETURN(0);
}
