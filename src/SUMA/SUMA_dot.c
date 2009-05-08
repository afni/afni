#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  

SUMA_Boolean SUMA_DotXform_MakeOrts( NI_element *dotopt, int ts_len,
                                     int polort, char *ortname)
{
   static char FuncName[]={"SUMA_DotXform_MakeOrts"};
   char stmp[256];
   float *fort=NULL, **refvec=NULL;
   int nort=0, nts = 0 ;
   int suc=0, i=0, nref=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   if (ortname) {
      fort = SUMA_Load1D_s(ortname, &nort, &nts, 0, 0);
      if (!fort) {
         SUMA_S_Err("Could not load orts");
         SUMA_RETURN(NOPE);
      }
      if (nts!= ts_len) {
         SUMA_S_Err("mismatch between polort length and time series length");
         SUMA_RETURN(NOPE);
      }
   } else {
      fort = NULL;
      nort = 0;
   }

   /* form the baseline regressors */
   nref = polort+1;

   if (nref) {
      SUMA_LHv("Building %d polyrefs\n", nref);
      refvec = THD_build_polyref( nref ,
                                  ts_len ) ;
   } else {
      SUMA_LH("No polyrefs");
      refvec = NULL;
      nref = 0;
   }

   /* now add the orts */
   if (fort) {
      SUMA_LHv("Adding %d from fort\n", nort);
      refvec = (float **)SUMA_realloc(refvec, nref+nort*sizeof(float*));
      for (i=0; i<nort; ++i) {
         refvec[i+nref] = SUMA_calloc(nts, sizeof(float));
         memcpy(refvec[i+nref], &(fort[i*nts]), sizeof(float)*nts);
      }
      free(fort); fort = NULL;
      nref += nort;
   } else {
      SUMA_LH("No fort\n");
   }


   /* put a copy of the regressors in dotopt */
   if (dotopt->vec_num) {
      SUMA_S_Warn("Cleaning up dotopt");
      while (dotopt->vec_num) {
         NI_remove_column(dotopt,-1);
      }
   }
   
   SUMA_LHv("Adding %d columns of length %d\n", nref, ts_len);
   /* add the columns here */
   NI_alter_veclen(dotopt, ts_len);
   for (i=0; i<nref; ++i) {
      NI_add_column(dotopt, NI_FLOAT, refvec[i]);
   }

   if (LocalHead) {
      sprintf(stmp,"file:%s.dotopt.1D", FuncName);
      NEL_WRITE_1D(dotopt, stmp, suc);
   }

   /* clean regressors  */
   if (refvec) {
      for (i=0; i<nref; ++i) {
         if (refvec[i]) free(refvec[i]); refvec[i] = NULL;
      }
      free(refvec); refvec=NULL;
   }
   
   /* flag num_ort_parameters as not set */
   NI_SET_INT(dotopt, "num_ort_parameters", -1);
   
   SUMA_RETURN(YUP);
}                        

/* 
   Function to create dotopt NI_element, set some of its
   parameters, AND recreate the vector of regressors.
   
   doptop (NI_element *) if NULL, a new one is created
   ts_len (int): length of regressor time series
   ftop: if > 0 set the top pass frequency
         else   nothing is done
   fbot: if > 0 set the bot pass frequency
         else   nothing is done
   norm: if == 1 set normalize_dset = "y"
         if == 0 set normalize_dset = "n"
         else    do nothing
   prec: if > 0 set precision
         else    do nothing
   polort: if > -2 set polort value and create regressors
           else whatever is in doptopt is used. 
   ortname: if not NULL, load orts from file ortname and add
           the other regressors
   WARNING!: If polort, or ortname need changing, then both
            parameters must be set.
*/
NI_element *SUMA_set_dotopts(NI_element *dotopt, int ts_len,
                             float ftop, float fbot,
                             int norm, int prec,
                             int polort, char *ortname)
{
   static char FuncName[]={"SUMA_set_dotopts"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (ts_len < 2) {
      SUMA_S_Errv("bad ts_len of %d\n", ts_len);
      SUMA_RETURN(dotopt);
   }
   
   if (!dotopt) { /* new with defaults */
      dotopt = NI_new_data_element("dotopts", 0);
      NI_SET_FLOAT (dotopt,"filter_above", 99999999.9);
      NI_SET_FLOAT (dotopt,"filter_below", 0.0); 
      NI_SET_INT(dotopt,"polort",-1);
      NI_SET_INT(dotopt,"prec",1);
   }
   
   /* initialize by input */
   if (ftop > 0) NI_SET_FLOAT(dotopt, "filter_above", ftop); 
   if (fbot >= 0) NI_SET_FLOAT(dotopt, "filter_below", fbot); 
   if (norm == 1) {  
     NI_set_attribute(dotopt, "normalize_dset", "y");   
   } else if (norm == 0) { 
     NI_set_attribute(dotopt, "normalize_dset", "n");   
   }  
   if (prec > 0) NI_SET_INT(dotopt,"numeric_precision", prec);  
   if (polort > -2) NI_SET_INT(dotopt,"polort", polort);
   if (ortname) NI_set_attribute(dotopt,"ortname",ortname);
   
   /* now enforce settings */

   /* get the file-based ort functions */
   ortname = NI_get_attribute(dotopt,"ortname");
   NI_GET_INT(dotopt,"polort", polort);
   if (!NI_GOT) polort = -1;

   if (!SUMA_DotXform_MakeOrts( dotopt, ts_len, polort, ortname)){
      SUMA_S_Err("Failed to make orts");
      SUMA_RETURN(dotopt);
   }
                           
   /* get the filtering option */
   NI_GET_FLOAT(dotopt, "fliter_above", ftop);
   if (!NI_GOT) ftop = 99999999.9; /* Hz, what elese did you expect? */
   NI_GET_FLOAT(dotopt, "filter_below", fbot);
   if (!NI_GOT) fbot = 0;
      
   /* initialize pending to nothing */
   NI_set_attribute(dotopt, "pending", "");

   SUMA_RETURN(dotopt);
     
}
int SUMA_DotXform_GetRecomputeForDset (NI_element *dotopts, char *id)
{
   static char FuncName[]={"SUMA_DotXform_GetRecomputeForDset"};
   int recompute = 0;
   char *cs;
   
   SUMA_ENTRY;
   
   cs = NI_get_attribute(dotopts, "pending");
   if (strstr(cs, id)) SUMA_RETURN(1);
   else SUMA_RETURN(0);
   
}

void SUMA_DotXform_SetPending (NI_element *dotopts, int pending, char *id)
{
   static char FuncName[]={"SUMA_DotXform_SetPending"};
   int ii;
   char stmp[10*SUMA_IDCODE_LENGTH+11]={""};
   char *cs=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
        
   if (!dotopts) {
      SUMA_S_Err("No dotopts");
      SUMA_RETURNe;
   }
   if (!id && pending) {
      SUMA_S_Err("Cannot set pending to 1 with no id");
      SUMA_RETURNe;
   }
   if (!pending) {
      if (!id) {
         /* kill all */
         NI_set_attribute(dotopts, "pending", "");
      } else {
         cs = NI_get_attribute(dotopts, "pending");
         SUMA_LHv("Have pending of \n"
                  ">>>%s<<<\n", cs);
         if (SUMA_Remove_Sub_String(cs, ";", id) == 1) {
            NI_set_attribute(dotopts, "pending", cs);
         }
      }
   } else {
      {
         cs = NI_get_attribute(dotopts,"pending");
         if (cs) {
            if (!strstr(cs, id)) {
               /* add it */
               strcat(stmp, cs);
               strcat(stmp, id);
               NI_set_attribute(dotopts, "pending", stmp);
            } else {/* alread pending */
            }
         } else { /* nothing pending, add it */
            sprintf(stmp, "%s;", id);
            NI_set_attribute(dotopts, "pending", stmp);
         }
      }
   }   
   
   SUMA_RETURNe;
}

SUMA_DSET *SUMA_GetDotPreprocessedDset(SUMA_DSET *in_dset, 
                                       NI_element *dotopt)
{
   static char FuncName[]={"SUMA_GetDotPreprocessedDset"};
   SUMA_DSET *pdset=NULL;
   float **refvec=NULL, ftop=9999999.9, fbot = 0.0;
   int recompute=0, i, N_ort_param=0;
   char stmp[256], *ppid=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* look for pre-existing dset */
   SUMA_LH("Looking for preprocessed dset");
   
   /* first recreate the unique identifier of the preprocessed dset */
   sprintf(stmp,"dot.preprocess.%s", SDSET_ID(in_dset));
   ppid = UNIQ_hashcode(stmp);
   
   recompute = SUMA_DotXform_GetRecomputeForDset(dotopt, SDSET_ID(in_dset));

   /* search if that dset exists already */
   if (!recompute &&
       (pdset = SUMA_FindDset_s(ppid, SUMAg_CF->DsetList))) {
      SUMA_LH("got it!");
      /* got it, get out */
      SUMA_free(ppid); ppid = NULL;
      SUMA_RETURN(pdset);
   } else {/* (re) create the bastard */
      SUMA_LH("Don't got it!, or recompute");
      /* make sure dotopt is ready (detrending vectors are loaded)*/
      if (!dotopt->vec_num) {
         dotopt = SUMA_set_dotopts(dotopt, SDSET_VECNUM(in_dset),
                             -1, -1,
                             -1, -1,
                             -2, NULL);
      }
      
      if (!dotopt->vec_num) {
         SUMA_S_Err("Nothing to do here!");
         goto BAIL;
      }
      
      /* build a refvec */
      refvec = (float **)SUMA_calloc(dotopt->vec_num, sizeof(float *));
      for (i=0; i<dotopt->vec_num; ++i) {
            refvec[i] = (float *)dotopt->vec[i];
      }
      /* now detrend the whole thing */
      NI_GET_FLOAT(dotopt,"filter_below",fbot);
      NI_GET_FLOAT(dotopt,"filter_above",ftop);

      SUMA_LHv("Detrending with %d orts and BP %f %f\n",
               dotopt->vec_num,  fbot, ftop);
      pdset = SUMA_DotDetrendDset(in_dset, refvec, dotopt->vec_num, 
                                  fbot, ftop, 0, &N_ort_param);
      NI_SET_INT(dotopt,"num_ort_parameters", N_ort_param);
      NI_set_attribute(pdset->ngr, "self_idcode", ppid);
      NI_set_attribute(pdset->ngr,"domain_parent_idcode",
                       NI_get_attribute(in_dset,"domain_parent_idcode"));
      NI_set_attribute(pdset->ngr,"geometry_parent_idcode", 
                       NI_get_attribute(in_dset,"geometry_parent_idcode"));
      SUMA_free(ppid); ppid = NULL;
      
      /* put the dset in the global list (allow for replace)*/
      if (!SUMA_InsertDsetPointer(&pdset, SUMAg_CF->DsetList,1)){
         SUMA_S_Err("Failed to insert pointer");
         goto BAIL;
      }
      
      SUMA_DotXform_SetPending (dotopt, 0, SDSET_ID(in_dset));
      
      SUMA_RETURN(pdset);
   }
   
   BAIL:
   if (ppid) SUMA_free(ppid); ppid = NULL;
   if (refvec) {
      free(refvec); refvec=NULL;
   }
   SUMA_RETURN(NULL);
}

double *SUMA_DotPreProcessTimeSeries(float *fv, int N_ts, 
                                     float TR, NI_element *dotopts)
{
   static char FuncName[]={"SUMA_DotPreProcessTimeSeries"};
   float **ort=NULL, ftop=9999999.9, fbot=0.0;
   double *ts=NULL;
   int ii =0;
   
   SUMA_ENTRY;
   
   if (!fv || !dotopts) SUMA_RETURN(NULL);
   
   /* detrend it ? */
   if (NI_YES_ATTR(dotopts,"normalize_dset")) {
      if (dotopts->vec_num) { /* something to ort */
         if (dotopts->vec_len != N_ts) {
            SUMA_S_Err("bad dotopts->vec_len");
            SUMA_RETURNe;
         }
         ort = (float **)SUMA_calloc(dotopts->vec_num, sizeof(float *));
         for (ii=0; ii<dotopts->vec_num; ++ii) 
            ort[ii] = (float *)dotopts->vec[ii];
      } else {
         ort = NULL;
      }
      NI_GET_FLOAT(dotopts, "fliter_above", ftop);
      if (!NI_GOT) ftop = 99999999.9; /* Hz, what elese did you expect? */
      NI_GET_FLOAT(dotopts, "filter_below", fbot);
      if (!NI_GOT) fbot = 0.0;
      THD_bandpass_vectors( N_ts , 1   , &fv ,
                            (float)TR , fbot ,  ftop  ,
                            0 , dotopts->vec_num   , ort  );
      if (ort) free(ort); ort = NULL;
    }  
    
    THD_normalize( N_ts , fv ) ;
   

   /* now stash fv in ts */
   ts = (double *)SUMA_calloc(N_ts, sizeof(double));
   for (ii=0; ii<N_ts; ++ii)  {
      ts[ii] = (double)fv[ii];
   }
   RETURN(ts);
}

void SUMA_dot_product_CB( void *params) 
{
   static char FuncName[]={"SUMA_dot_product_CB"};

   char *SO_idcode=NULL, *ts_dset_id=NULL, 
         *dot_dset_id=NULL, stmp[300];
   SUMA_DSET *in_dset=NULL, *ts_src_dset=NULL; 
   double TR = 0;
   double *ts=NULL; 
   int ts_node=0, N_ts, ii, jj;
   float *fv=NULL;
   SUMA_DSET *out_dset=NULL, *child=NULL, *dt_dset=NULL;
   SUMA_CALLBACK *cb= (SUMA_CALLBACK *)params;
   SUMA_XFORM *xf=NULL;
   NI_element *nelts = NULL, *nelpars=NULL, *dotopts=NULL;
   NI_group *ngr = NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_OVERLAYS *Sover=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (0 && LocalHead) {
      SUMA_LH("Callbacks upon entering");
      SUMA_Show_Callbacks(SUMAg_CF->callbacks, SUMA_STDERR, 1);
   }
   
   if (!cb) {
      SUMA_S_Err("NULL input");
      SUMA_RETURNe;
   }
   
   if (!cb->pending) {
      SUMA_S_Err("Why am I called?")
      SUMA_RETURNe;
   }
   
   if (strcmp(cb->FunctionName, "SUMA_dot_product_CB")) {
      SUMA_S_Errv("Baddilicious: %s\n", cb->FunctionName);
      SUMA_RETURNe;
   }
   
   ngr = cb->FunctionInput;
   if (!ngr) {
      SUMA_S_Err("NULL input");
      SUMA_RETURNe;
   }
   
   /* get some key elements */
   nelts = SUMA_FindNgrDataElement(ngr, "callback.data", "ts_vec");
   
   if (!(nelpars = SUMA_FindNgrNamedElement(ngr, "event_parameters"))) {
      SUMA_S_Err("parameters element not found!");
      SUMA_RETURNe;
   }
   
   /* Go find out, which xform produced this cb */
   if (!(xf=SUMA_Find_XformByID(cb->creator_xform))) {
      SUMA_S_Err("Have no way to reliably select time series dset"
                 "that produced cb");
      SUMA_RETURNe;
   }

   if (!(dotopts = SUMA_FindNgrNamedElement(xf->XformOpts, "dotopts"))) {
      SUMA_S_Err("dotopts element not found!");
      SUMA_RETURNe;
   }

   if (LocalHead) {
      int suc=0;
      sprintf(stmp,"file:%s.nelpars.1D", FuncName);
      NEL_WRITE_1D(nelpars, stmp, suc);
   }
   if (LocalHead) {
      int suc=0;
      sprintf(stmp,"file:%s.dotopts.1D", FuncName);
      NEL_WRITE_1D(dotopts, stmp, suc);
   }
      
   /* get one of the ts_dsets */
   ts_dset_id = SUMA_GetNgrColStringAttr(ngr, 0, "ts_dsets_idcode");
   if (!(SUMA_is_ID_4_DSET(ts_dset_id, &in_dset))) {
      SUMA_S_Err("Could not find ts dset");
      SUMA_RETURNe;
   }
   if (ts_dset_id) SUMA_free(ts_dset_id); ts_dset_id=NULL;
    
   /* See if ts_node is specified */
   NI_GET_INT(nelpars, "event.new_node", ts_node);
   if (!NI_GOT) ts_node = -1;
   
   /* get yer time series kid */
   N_ts = -1;
   if (nelts) {
      if (nelts->vec_len && nelts->vec_len != SDSET_VECNUM(in_dset)) {
         SUMA_S_Errv( "ts_vec exists, but is of different length (%d) \n"
                      "                             than in_dset (%d)\n",
                      nelts->vec_len, SDSET_VECNUM(in_dset));
         SUMA_RETURNe;
      } else if (nelts->vec_len) {
         if (!(SUMA_is_TimeSeries_dset(in_dset, &TR))) {
               TR = 0.0;
         }
         if (ts_node >= 0) {
            SUMA_LH("Have ts_node AND ts. ts takes precedence\n");
         }
         N_ts = nelts->vec_len;
         /* OK, now get your ts */
         fv = (float *)nelts->vec[0];
         if (!(ts = SUMA_DotPreProcessTimeSeries(fv, N_ts,(float)TR, dotopts))) {
            SUMA_S_Err("Failed processing time series");
            SUMA_RETURNe;
         }
      }
   }
      
   if (N_ts < 0) { /* try to get ts from a dset, based on where click happened */
      if (!(SO = SUMA_findSOp_inDOv(NI_get_attribute(nelpars,"event.SO_idcode"),
                                    SUMAg_DOv, SUMAg_N_DOv))) {
         SUMA_S_Errv("Could not find event's SO (%s)\n",
                     NI_get_attribute(nelpars,"event.SO_idcode"));
         SUMA_RETURNe;
      }
      /* find out which overlay was clicked on */
      if (!(Sover = SUMA_Fetch_OverlayPointer(SO->Overlays, SO->N_Overlays,
                                 NI_get_attribute(nelpars,"event.overlay_name"),
                                 &jj))) {
         SUMA_S_Err("Could not find event's overlay");
         SUMA_RETURNe;                          
      }
      
      /* Get the dset that corresponds to this overlay */ 
      child = Sover->dset_link; 
      
      /* find child dset */
      if (!SUMA_is_XformChild(xf, SDSET_ID(child), &jj)) {
         SUMA_S_Err("Failed to find child in xform");
         SUMA_RETURNe;
      }
      
      /* here is the relevant parent dset */
      if (!(SUMA_is_ID_4_DSET(xf->parents[jj], 
                              &ts_src_dset))) {
         SUMA_S_Err("Could not find ts source dset");
         SUMA_RETURNe;
      }
      if (SDSET_VECNUM(in_dset) != SDSET_VECNUM(ts_src_dset)) {
         SUMA_S_Errv("Mismatch in ts length (%d vs %d)\n",
                     SDSET_VECNUM(in_dset), SDSET_VECNUM(ts_src_dset));
      }
      
      if (NI_YES_ATTR(dotopts,"normalize_dset")) {
         SUMA_LHv("Getting time series from node %d on detrended version"
                  "of dset %s\n",
                  ts_node, SDSET_FILENAME(ts_src_dset));
         dt_dset = SUMA_GetDotPreprocessedDset(ts_src_dset, dotopts);
      } else {
         SUMA_LHv("Getting time series from node %d of dset %s\n"
                  "  (normalize_dset=%s)\n",
                  ts_node, SDSET_FILENAME(ts_src_dset), 
                  CHECK_NULL_STR(NI_get_attribute(dotopts,"normalize_dset")));
         dt_dset = ts_src_dset;
      }
      /* get the time series */
      if (!(ts = (double*)SUMA_GetDsetAllNodeValsInCols2(dt_dset, 
                                 NULL, 0, 
                                 ts_node, -1,
                                 &N_ts,
                                 SUMA_double))) { 
         SUMA_S_Err("Failed to extract time series.");
         SUMA_RETURNe;
      }
   }
   if (N_ts < 0) {
      SUMA_S_Err("Nothing to work with here!");
      SUMA_RETURNe;
   }
   
   /* call the function for each of the parents */
   for (ii=0; ii<cb->N_parents; ++ii) {
      if (!(out_dset = SUMA_FindDset_s(cb->parents[ii], SUMAg_CF->DsetList))) {
         SUMA_S_Err("Failed to find out_dset");
         SUMA_RETURNe; 
      }
      ts_dset_id = SUMA_GetNgrColStringAttr(ngr, ii, "ts_dsets_idcode");
      if (!(SUMA_is_ID_4_DSET(ts_dset_id, &in_dset))) {
         SUMA_S_Err("Could not find ts dset");
         SUMA_RETURNe;
      }
      if (ts_dset_id) SUMA_free(ts_dset_id); ts_dset_id=NULL;
      
      if (LocalHead) {
         char stmp[1000];
         if (!SDSET_LABEL(in_dset)) SUMA_LabelDset(in_dset, NULL);
         sprintf(stmp,"%s.ts.%d.1D", SDSET_LABEL(in_dset), ts_node);
         SUMA_WRITE_ARRAY_1D(ts, N_ts, 1, stmp);
      }
      SUMA_LHv("  Calculating dot for %d/%d: \n"
               "  Time series dset: %s\n"
               "  Dot product output in: %s\n", 
               ii, cb->N_parents, 
               SDSET_FILENAME(in_dset),
               SDSET_FILENAME(out_dset) );
      /* call dot product computer */
      if (!SUMA_dot_product(in_dset,ts,&out_dset,dotopts)) {
         SUMA_S_Err("Failed to compute dot product");
         SUMA_RETURNe;
      }
   }
   
   if (ts) SUMA_free(ts); ts = NULL;
   
   /* Now clear callback specific elements for caution 
      This way I'll know if this function 
      is being called repeatedly by mistake*/
   if (nelts) {
      NI_remove_from_group(ngr, nelts); NI_free(nelts); nelts = NULL;
   }
   
   
   SUMA_RETURNe; 
}

SUMA_DSET *SUMA_DotDetrendDset(  SUMA_DSET *in_dset, 
                                 float **refvec, int nref,
                                 float fbot, float ftop,
                                 int qdet, int *num_ort) 
{
   static char FuncName[]={"SUMA_DotDetrendDset"};
   float **fvec=NULL;
   double TR=0;
   int i, N_ret=0, nnort = 0;
   SUMA_DSET *o_dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!refvec || !nref || !in_dset) SUMA_RETURN(NULL);
   
   if (!(SUMA_is_TimeSeries_dset(in_dset, &TR))) {
      TR = 0.0;
   } 
   
   /* turn the dset to a float vector array */
   if (!(fvec = (float **)SUMA_Dset2VecArray( in_dset, 
                                    NULL, -1, /* all columns */
                                    NULL, -1, /* all nodes */
                                    -1, /* don't care to figure out max node */
                                    &N_ret,
                                    SUMA_float /* float me back */))) {
      SUMA_S_Err("Failed to copy surface dset");
      SUMA_RETURN(NULL);  
   }
     
  
   /* detrend */
   nnort = THD_bandpass_vectors (SDSET_VECNUM(in_dset), SDSET_VECLEN(in_dset), 
                                 fvec, (float)TR, fbot, ftop, qdet, nref, 
                                 refvec);
   if (num_ort) *num_ort = nnort;
   
   /* normalize */
   for (i=0; i<SDSET_VECLEN(in_dset); ++i) {
      THD_normalize( SDSET_VECNUM(in_dset) , fvec[i] ) ;
   }
   
   /* make a copy of the input dset */
   o_dset = SUMA_MaskedCopyofDset(in_dset, NULL, NULL, 1, 0);
   
   /* Now fill it with fvec*/
   if (!SUMA_VecArray2Dset((void **)fvec, 
                            o_dset, 
                            NULL, -1,  
                            NULL, -1,
                            -1,
                            SUMA_float)) {
      SUMA_S_Err("Misery");
      SUMA_RETURN(NULL);                          
   }
   
   /* cleanup the large array */
   for (i=0; i<SDSET_VECLEN(in_dset); ++i) {
      SUMA_free(fvec[i]); fvec[i]=NULL;
   }
   SUMA_free(fvec); fvec = NULL;
   
   /* write out dset for good measure? */
   if (LocalHead) {
      SUMA_LH("Writing detrended dset to disk\n");
      SUMA_WriteDset_s(FuncName, o_dset, SUMA_ASCII_NIML, 1, 1);
   }
   
   /* that is it, return */
   SUMA_RETURN(o_dset);
}


SUMA_Boolean SUMA_dot_product(SUMA_DSET *in_dset,
                              double *ts, 
                              SUMA_DSET **out_dsetp,
                              NI_element *dotopt) 
{
   static char FuncName[]={"SUMA_dot_product"};
   SUMA_DSET *dot = NULL;
   static SUMA_DSET *in_dset_last=NULL;
   double *dcol=NULL;
   float *fcol = NULL;
   int ic=0, ir=0;
   byte *bbv=NULL;
   int *iiv=NULL;
   float *ffv=NULL;
   double  *ddv=NULL;
   int prec=1; /* NEVER CHANGE THIS DEFAULT */
   char *s=NULL, *sname=NULL;
   SUMA_VARTYPE vtp = SUMA_notypeset;
   float par[3];
   static SUMA_VARTYPE vtp_last = SUMA_notypeset;
   NI_element *nelb=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
      
   if (  !in_dset || 
         !out_dsetp ||
         !ts   )  {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   
   if (in_dset != in_dset_last) {
      SUMA_LH("Checking consistency");
      if (!SUMA_is_AllConsistentNumeric_dset(in_dset, &vtp)) {
         SUMA_S_Err( "Input dataset is either not all numeric \n"
                     "or has inconsistent types");
         SUMA_RETURN(NOPE);
      }
      in_dset_last = in_dset;
      vtp_last = vtp;
   } else {
      vtp = vtp_last;
   }
   
   /* decide if we need new_dset or not */
   if (*out_dsetp) {
      SUMA_LH("Reusing  dset");
      dot = *out_dsetp;
      if (SUMA_ColType2TypeCast(SUMA_TypeOfDsetColNumb(dot, 0))==SUMA_double) {
         prec = 2;
         dcol = (double*)dot->dnel->vec[0];
         memset((void *)dcol, 0, SDSET_VECLEN(dot)*sizeof(double));
      } else {
         prec = 1;
         fcol = (float *)dot->dnel->vec[0];
         memset((void *)fcol, 0, SDSET_VECLEN(dot)*sizeof(float));
      }
   } else {
      if (dotopt) {
         NI_GET_INT(dotopt,"numeric_precision", prec);
         if (!NI_GOT) prec = 1;
      } else {
         /* NEVER CHANGE THIS , prec is set at initialization */
      }
      if (!SDSET_LABEL(in_dset)) SUMA_LabelDset(in_dset, NULL);
      
      sname = SUMA_append_string("dot.",SDSET_LABEL(in_dset));
      SUMA_LHv("Creating new  dset (%s) with precision %d\n", sname, prec);
      if (!(dot = SUMA_CreateDsetPointer( 
                  sname, SUMA_NODE_BUCKET, NULL, 
                   NI_get_attribute(in_dset->ngr,"domain_parent_idcode"),
                   SDSET_VECLEN(in_dset)  ) ) ){
                   
      }
      SUMA_free(sname); sname=NULL;
      SUMA_LabelDset(dot, NULL);
      
      if (!SUMA_AddDsetNelCol (dot, NI_get_attribute(in_dset->inel,"COLMS_LABS"),
                               SUMA_NODE_INDEX, 
                               (void *)SDSET_NODE_INDEX_COL(in_dset), 
                               NULL,1 )) {
         fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
         exit(1);                    
      }
      if (prec == 2) {
         dcol = (double *) SUMA_calloc(SDSET_VECLEN(in_dset), sizeof(double));
         if (!SUMA_AddDsetNelCol(dot, "dotproduct", SUMA_NODE_DOUBLE, 
                                 (void *)dcol, NULL , 1)) {
            SUMA_S_Err("Failed to add col");
            SUMA_RETURN(NOPE);
         }
         SUMA_free(dcol); dcol = (double*)dot->dnel->vec[0];
      } else {
         fcol = (float *) SUMA_calloc(SDSET_VECLEN(in_dset), sizeof(float));
         if (dotopt && NI_YES_ATTR(dotopt,"normalize_dset")) {
            /* par[2] is not available yet. The 2 is just a place holder */
            par[0] = SDSET_VECNUM(in_dset); par[1] = 1; par[2]=2;
            if (!SUMA_AddDsetNelCol(dot, "XcorrCoef", SUMA_NODE_XCORR, 
                                    (void *)fcol, (void *)par , 1)) {
               SUMA_S_Err("Failed to add col");
               SUMA_RETURN(NOPE);
            }
         } else {
            if (!SUMA_AddDsetNelCol(dot, "dotproduct", SUMA_NODE_FLOAT, 
                                    (void *)fcol, NULL , 1)) {
               SUMA_S_Err("Failed to add col");
               SUMA_RETURN(NOPE);
            }
         }
         SUMA_free(fcol); fcol = (float*)dot->dnel->vec[0];
      } 
      *out_dsetp = dot;
   }
   
   /* preprocess in_dset ? */
   if (dotopt) {
      if (NI_YES_ATTR(dotopt,"normalize_dset")) {
         SUMA_LH("Dotting preprocessed dset");
         /* WARNING: From this point on in_dset will point
                     to another dset.  */
         if (!(in_dset = SUMA_GetDotPreprocessedDset(in_dset, dotopt))) {
            SUMA_S_Err("Failed to get preprocessed dset");
            SUMA_RETURN(NOPE);
         }
      } else {
         SUMA_LH("Dotting initial dset");
      }
   } else {
      SUMA_LH("Dotting initial dset, no dotopt");
   }
   
   SUMA_LHv("Dotting %dx%d values at x%d precision...\n", 
            SDSET_VECNUM(in_dset), SDSET_VECLEN(in_dset), prec);
   switch(vtp) {
      case SUMA_byte:
         for (ic=0; ic<SDSET_VECNUM(in_dset); ++ic) {
            bbv = (byte*)in_dset->dnel->vec[ic];
            if (prec == 2) {
               for (ir=0; ir<SDSET_VECLEN(in_dset); ++ir) 
                  dcol[ir] += ts[ic]*(double)bbv[ir];
            }  else {
               for (ir=0; ir<SDSET_VECLEN(in_dset); ++ir) 
                  fcol[ir] += (float)(ts[ic]*(double)bbv[ir]);
            }
         }
         break;
      case SUMA_int:
         for (ic=0; ic<SDSET_VECNUM(in_dset); ++ic) {
            iiv = (int*)in_dset->dnel->vec[ic];
            if (prec == 2) {
               for (ir=0; ir<SDSET_VECLEN(in_dset); ++ir) 
                  dcol[ir] += ts[ic]*(double)iiv[ir];
            } else {
               for (ir=0; ir<SDSET_VECLEN(in_dset); ++ir) 
                  fcol[ir] += (float)(ts[ic]*(double)iiv[ir]);
            } 
         }
         break;
      case SUMA_float:
         for (ic=0; ic<SDSET_VECNUM(in_dset); ++ic) {
            ffv = (float*)in_dset->dnel->vec[ic];
            if (prec == 2) {
               for (ir=0; ir<SDSET_VECLEN(in_dset); ++ir) 
                  dcol[ir] += ts[ic]*(double)ffv[ir];
            } else {
               for (ir=0; ir<SDSET_VECLEN(in_dset); ++ir) 
                  fcol[ir] += (float)(ts[ic]*(double)ffv[ir]);
            }  
         }
         break;
      case SUMA_double: 
         for (ic=0; ic<SDSET_VECNUM(in_dset); ++ic) {
            ddv = (double*)in_dset->dnel->vec[ic];
            if (prec == 2) {
               for (ir=0; ir<SDSET_VECLEN(in_dset); ++ir) 
                  dcol[ir] += ts[ic]*ddv[ir];
            }  else {
               for (ir=0; ir<SDSET_VECLEN(in_dset); ++ir) 
                  fcol[ir] += (float)(ts[ic]*ddv[ir]);
            } 
         }
         break;
      default:
         SUMA_S_Err("What kind of numeric type is this?");
         SUMA_RETURN(NOPE);
         break;
   }     
   
   SUMA_LH("Updating range\n");
   
   if(!(s = SUMA_CreateDsetColRangeCompString(dot, 0, 
                        SUMA_TypeOfDsetColNumb(dot,0)))) {
         SUMA_S_Err("Failed to calculate range");
         SUMA_RETURN(NOPE);
   }else {
      NI_element *nelb = SUMA_FindDsetAttributeElement(dot, "COLMS_RANGE");
      SUMA_LHv("New range string is %s\n", s);
      SUMA_AddColAtt_CompString(nelb, 0, s, SUMA_NI_CSS,0);
      SUMA_free(s); s=NULL;
   }
   
   /* reset the stat params, just in case */
   if (dotopt && NI_YES_ATTR(dotopt,"normalize_dset") &&
       (nelb = SUMA_FindDsetAttributeElement(dot, "COLMS_STATSYM"))) {
      par[0] = SDSET_VECNUM(in_dset); par[1] = 1;
      NI_GET_INT(dotopt,"num_ort_parameters", par[2]);
      if (!NI_GOT || par[2] == -1.0) {
         SUMA_S_Errv("Failed to get ort parameters (%f), \n"
                     "you can't trust the p values\n ",
                     par[2] );
         par[2] = 2; /* something ... */
      }
      SUMA_AddColAtt_CompString( nelb, 0, 
                                 NI_stat_encode(NI_STAT_CORREL, 
                                                par[0], par[1], par[2]),
                                 SUMA_NI_CSS, 0);
   }
   
   if (0 && LocalHead) {
      ic = 0;
      for (ir=0; ir<SDSET_VECLEN(in_dset) && ic < 10; ++ir) {
         if (prec ==2) {
            if (dcol[ir]) {
               fprintf(SUMA_STDERR,"dcol[%d]=%f\n", ir, dcol[ir]);
               ++ic;
            } 
         } else {
            if (fcol[ir]) {
               fprintf(SUMA_STDERR,"fcol[%d]=%f\n", ir, fcol[ir]);
               ++ic;
            } 
         }
      }
      if (!ic) {
         SUMA_S_Note("No non zero rows encountered!");
      }
      ir = 1;ic = 0;
      DSET_WRITE_1D(dot, "file:dot.1D.dset", ic, ir);
      s = SUMA_WriteDset_s ("dot", dot, SUMA_ASCII_NIML, 1, 1);
      SUMA_free(s);
   }
   
   
   SUMA_RETURN(YUP);
   
}
