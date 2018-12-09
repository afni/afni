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
      SUMA_LHv("Loading %s\n", ortname);
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
   if (nref >= ts_len-3) {
      SUMA_S_Errv("Number of baseline regressors(%d) \n"
                  "is too high compared to the number of samples (%d)\n"
                  , nref, ts_len);
      SUMA_RETURN(NOPE);
   }
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
      SUMA_LHv("Adding %d from fort, length %d to previous %d orts\n",
               nort, nts, nref);
      refvec = (float **)SUMA_realloc(refvec, (nref+nort)*sizeof(float*));
      for (i=0; i<nort; ++i) {
         refvec[i+nref] = (float *)SUMA_calloc(nts, sizeof(float));
         memcpy(refvec[i+nref], &(fort[i*nts]), sizeof(float)*nts);
      }
      free(fort); fort = NULL;
      nref += nort;
   } else {
      SUMA_LH("No fort\n");
   }


   /* put a copy of the regressors in dotopt */
   if (dotopt->vec_num) {
      SUMA_LH("Cleaning up dotopt");
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
      SUMA_LHv("Writing %s\n", stmp);
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
   NI_GET_FLOAT(dotopt, "filter_above", ftop);
   if (!NI_GOT) ftop = 99999999.9; /* Hz, what elese did you expect? */
   NI_GET_FLOAT(dotopt, "filter_below", fbot);
   if (!NI_GOT) fbot = 0;

   /* initialize pending to nothing */
   NI_set_attribute(dotopt, "pending", "");

   if (LocalHead) {
      SUMA_ShowNel(dotopt);
   }
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

      SUMA_LHv("Detrending with polort of 2, %d extra orts and BP %f %f\n",
               dotopt->vec_num,  fbot, ftop);
      pdset = SUMA_DotDetrendDset(in_dset, refvec, dotopt->vec_num,
                                  fbot, ftop, 1, &N_ort_param);
                  /* the '1' flag forces a second order detrending,
                  in addition to orts and smoothing. This is done
                  in keeping with AFNI's implementation */
      NI_SET_INT(dotopt,"num_ort_parameters", N_ort_param);
      NI_set_attribute(pdset->ngr, "self_idcode", ppid);
      NI_set_attribute(pdset->ngr,"domain_parent_idcode",
                        SDSET_IDMDOM(in_dset));
      NI_set_attribute(pdset->ngr,"geometry_parent_idcode",
                        SDSET_IDGDOM(in_dset));
      SUMA_free(ppid); ppid = NULL;

      /* put the dset in the global list (allow for replace)*/
      if (!SUMA_InsertDsetPointer(&pdset, SUMAg_CF->DsetList,1)){
         SUMA_S_Err("Failed to insert pointer");
         goto BAIL;
      }


      #if 1 /* make it displayable */
      {
         SUMA_OVERLAYS *child=NULL;
         SUMA_SurfaceObject *SO=NULL;
         char *stmp=NULL;
         SUMA_LIST_WIDGET *LW = NULL;
         int OverInd=0;

         SUMA_LH("Create its overlay (child)");

         if (!(SO =
            SUMA_findSOp_inDOv(SDSET_IDMDOM(pdset), SUMAg_DOv, SUMAg_N_DOv))) {
            SUMA_S_Errv("Failed to find domain surface '%s'\n",
                        SDSET_IDMDOM(pdset));
            if (LocalHead) {
               SUMA_ShowDset(pdset,   0, NULL);
               SUMA_ShowDset(in_dset, 0, NULL);
            }
            goto BAIL;
         }

         if (!(child = SUMA_Fetch_OverlayPointerByDset (
                              (SUMA_ALL_DO *)SO, pdset, &OverInd))) {
            /* need a new one */
            if (!(SDSET_LABEL(pdset))) SUMA_LabelDset(pdset,NULL);
            stmp = SUMA_append_string("dotprep.", SDSET_LABEL(pdset));
            child = SUMA_CreateOverlayPointer (
                                         stmp, pdset, SO->idcode_str, NULL);
            SUMA_free(stmp); stmp=NULL;
            if (!child) {
               SUMA_S_Err("Failed in CreateOverlayPointer." );
               goto BAIL;
            }
            SUMA_LH("Add overlay to SO");
            /* Add this plane to SO->Overlays */
            if (!SUMA_AddNewPlane ( (SUMA_ALL_DO *)SO, child,
                                    SUMAg_DOv, SUMAg_N_DOv, 0)) {
               SUMA_SL_Crit("Failed in SUMA_AddNewPlane");
               SUMA_FreeOverlayPointer(child);
               goto BAIL;
            }

            /* set the opacity, index column and the range */
            child->GlobalOpacity = YUP;
            child->ShowMode = SW_SurfCont_DsetViewCol;
            child->OptScl->BrightFact = 0.6;

            child->OptScl->find = 0;
            child->OptScl->tind = 0;
            child->OptScl->bind = 0;

            SUMA_LH("Refreshing Dset list");
            /*update the list widget if open */
            LW = SO->SurfCont->SwitchDsetlst;
            if (LW) {
               if (!LW->isShaded) SUMA_RefreshDsetList ((SUMA_ALL_DO *)SO);
            }

            /* this chunk from SUMA_ColPlane_NewOrder */
            if (!SUMA_MovePlaneDown((SUMA_ALL_DO *)SO, child->Name)) {
               SUMA_L_Err("Error in SUMA_MovePlaneUp.");
               goto BAIL;
            }
         }
      }
      #endif

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
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!fv || !dotopts) SUMA_RETURN(NULL);

   /* detrend it ? */
   if (NI_YES_ATTR(dotopts,"normalize_dset")) {
      if (dotopts->vec_num) { /* something to ort */
         if (dotopts->vec_len != N_ts) {
            SUMA_S_Err("bad dotopts->vec_len");
            SUMA_RETURN(NULL);
         }
         ort = (float **)SUMA_calloc(dotopts->vec_num, sizeof(float *));
         for (ii=0; ii<dotopts->vec_num; ++ii)
            ort[ii] = (float *)dotopts->vec[ii];
      } else {
         ort = NULL;
      }
      NI_GET_FLOAT(dotopts, "filter_above", ftop);
      if (!NI_GOT) ftop = 99999999.9; /* Hz, what elese did you expect? */
      NI_GET_FLOAT(dotopts, "filter_below", fbot);
      if (!NI_GOT) fbot = 0.0;

      SUMA_LHv("HERE: fbot: %f\tftop: %f\n", fbot, ftop);
      /* SUMA_ShowNel(dotopts); */

      if (!THD_bandpass_vectors( N_ts , 1   , &fv ,
                                 (float)TR , fbot ,  ftop  ,
                                 0 , dotopts->vec_num   , ort  )) {
         SUMA_S_Err("Bad bandpass call");
         SUMA_RETURN(NULL);
      }
      SUMA_LH("Back from purgatorium");
      if (ort) free(ort); ort = NULL;
    }

    THD_normalize( N_ts , fv ) ;


   /* now stash fv in ts */
   ts = (double *)SUMA_calloc(N_ts, sizeof(double));
   for (ii=0; ii<N_ts; ++ii)  {
      ts[ii] = (double)fv[ii];
   }
   SUMA_RETURN(ts);
}

void SUMA_dot_product_CB( void *params)
{
   static char FuncName[]={"SUMA_dot_product_CB"};

   char *SO_idcode=NULL, *ts_dset_id=NULL,
         *dot_dset_id=NULL, stmp[300], ident[300], prefix[300],
         *p1=NULL, *p2=NULL, *Cside;
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

   if (LocalHead) {
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
      SUMA_LH("Writing %s", stmp);
      NEL_WRITE_1D(nelpars, stmp, suc);
   }
   if (LocalHead) {
      int suc=0;
      sprintf(stmp,"file:%s.dotopts.1D", FuncName);
      SUMA_LH("Writing %s", stmp);
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
      if (!(SO = SUMA_findSOp_inDOv(NI_get_attribute(nelpars,"event.DO_idcode"),
                                    SUMAg_DOv, SUMAg_N_DOv))) {
         SUMA_S_Notev("Could not find event's SO (%s)\n"
                      "Use Shift+Ctrl+Right Click on the surface "
                      "to calculate correlation with new changes\n",
                     NI_get_attribute(nelpars,"event.DO_idcode"));
         /* SUMA_DUMP_TRACE("event.DO_idcode"); */
         SUMA_RETURNe;
      }
      /* find out which overlay was clicked on */
      if (!(Sover = SUMA_Fetch_OverlayPointer((SUMA_ALL_DO*)SO,
                        NI_get_attribute(nelpars,"event.overlay_name"),&jj))) {
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
         SUMA_LH("Writing %s", stmp);
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

      SUMA_LH("Now clearing overlay vecs");
      /* Make sure color overlay datacopies get reset */
      if (!SUMA_DSET_ClearOverlay_Vecs(out_dset)) {
         SUMA_S_Err("Failed to clear overlay copies");
         SUMA_RETURNe;
      }

	   snprintf(ident,298*sizeof(char), "filename:%s", SDSET_FILENAME(out_dset));

      SUMA_LH("Adding to save list");
      {
      	SUMA_PARSED_NAME *p1p, *p2p;
      p1p = SUMA_ParseFname_eng(SDSET_FILENAME(in_dset), SUMAg_CF->cwd, 0);
      p1 = SUMA_RemoveDsetExtension_s(p1p->FileName_NoExt,
                                        SUMA_NO_DSET_FORMAT);
      p2p = SUMA_ParseFname_eng(SDSET_FILENAME(ts_src_dset), SUMAg_CF->cwd, 0);
      p2 = SUMA_RemoveDsetExtension_s(p2p->FileName_NoExt,
                                        SUMA_NO_DSET_FORMAT);
      SUMA_Free_Parsed_Name(p1p); p1p = NULL;
      SUMA_Free_Parsed_Name(p2p); p2p = NULL;
      }

      if (SO->Side == SUMA_LEFT) Cside = "L";
      else if (SO->Side == SUMA_RIGHT) Cside = "R";
      else Cside = "";
      snprintf(prefix,298*sizeof(char),"seed_%d%s.DOT.%s",
                  ts_node, Cside, p1);
      SUMA_free(p1); SUMA_free(p2);
      if (!(SUMA_Add_to_SaveList(&SUMAg_CF->SaveList, "sdset", ident, prefix))) {
         SUMA_S_Warnv("Failed to add to save list %s %s\n", ident, prefix);
      }
   }

   if (ts) SUMA_free(ts); ts = NULL;

   /* Now clear callback specific elements for caution
      This way I'll know if this function
      is being called repeatedly by mistake*/
   SUMA_LH("Clearing callback specific elements");
   if (nelts) {
      NI_remove_from_group(ngr, nelts); NI_free_element(nelts); nelts = NULL;
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

   SUMA_LH("Bandpass/detrending phase");
   /* detrend */
   nnort = THD_bandpass_vectors (SDSET_VECNUM(in_dset), SDSET_VECLEN(in_dset),
                                 fvec, (float)TR, fbot, ftop, qdet, nref,
                                 refvec);
   if (!nnort) {
      SUMA_S_Err("Bad bandpass call, going to hell now.");
      SUMA_RETURN(NULL);
   }
   if (num_ort) *num_ort = nnort;

   /* normalize */
   for (i=0; i<SDSET_VECLEN(in_dset); ++i) {
      THD_normalize( SDSET_VECNUM(in_dset) , fvec[i] ) ;
   }

   SUMA_LH("About to form output dset");
   /* make a copy of the input dset */
   o_dset = SUMA_MaskedCopyofDset(in_dset, NULL, NULL, 1, 0);

   SUMA_LH("Now filling with output");
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

      sname = SUMA_append_string("dot.",SDSET_FILENAME(in_dset));
      SUMA_LHv("Creating new  dset (%s) with precision %d\n", sname, prec);
      if (!(dot = SUMA_CreateDsetPointer(
                  sname, SUMA_NODE_BUCKET, NULL,
                  SDSET_IDMDOM(in_dset),
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
      case SUMA_complex:
         SUMA_S_Err("No support for complex type here");
         SUMA_RETURN(NOPE);
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
      SUMA_LH("Adding ort stuff");
      SUMA_AddColAtt_CompString( nelb, 0,
                                 NI_stat_encode(NI_STAT_CORREL,
                                                par[0], par[1], par[2]),
                                 SUMA_NI_CSS, 0);
   }
   SUMA_LH("Heading out");
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

/* *************************************************************************** */
/* ******************* Functions to deal with Group InstaCorr **************** */
/* *************************************************************************** */

/*! find dsets and their overlay planes that go with giset
if target_name is not null, the function creates new dsets and their
corresponding overlay planes, otherwise, it returns what had been created.
 */
SUMA_Boolean SUMA_GICOR_Dsets(SUMA_SurfaceObject *SOv[],
                              GICOR_setup *giset,
                              char *target_name,
                              DList *DsetList,
                              SUMA_DSET *sdsetv[],
                              SUMA_OVERLAYS *ov[],
                              NI_element *nel)
{
   static char FuncName[]={"SUMA_GICOR_Dsets"};
   char *targetv[2]={NULL, NULL},
        *dset_namev[2]={NULL, NULL}, *atr=NULL;
   int i, ii, *Ti=NULL, ovind = 0, nvals=0, vv=0;
   static char *blab[6] = { "GIC_Delta" , "GIC_Zscore" ,
                            "AAA_Delta" , "AAA_Zscore" ,
                            "BBB_Delta" , "BBB_Zscore"  } ;
   NI_str_array *labar=NULL ;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (target_name &&
       giset->sdset_ID[0][0] != '\0' && giset->sdset_ID[1][0] != '\0') {
       SUMA_S_Warn("Hello anew from 3dGroupInCorr, \n"
                   "Attempting to reuse previous setup...");
       goto CHECK_DSET_AND_OVERLAYS;
   }

   if (target_name) { /* Brand new init, search/create by name */
      SUMA_LHv("Brand new init, target_name=%s\n", target_name);
      if (!nel) {
         SUMA_S_Err("Need GICOR setup nel for creating new dsets");
         SUMA_RETURN(NOPE);
      }
      /* Form the names of the dsets to be created */
      if (SOv[1]) { /* have two surfaces */
         targetv[0] = SUMA_append_string(target_name,".Left");
         targetv[1] = SUMA_append_string(target_name,".Right");
      } else {
         targetv[0] = SUMA_copy_string(target_name);
      }

      /* Now create a dataset for each case */
      for (i=0; i<2; ++i) {
         if (targetv[i]) {
            SUMA_LHv("Working %s\n", targetv[i]);
            /* dset names */
            dset_namev[i] = SUMA_append_string(targetv[i], SOv[i]->idcode_str);
            sdsetv[i] = SUMA_CreateDsetPointer (dset_namev[i],
                                        SUMA_NODE_BUCKET,
                                        NULL,
                                        SOv[i]->idcode_str,
                                        SOv[i]->N_Node);
            sprintf(giset->sdset_ID[i],"%s", SDSET_ID(sdsetv[i]));

            /* insert that element into DaList */
            if (!SUMA_InsertDsetPointer(&sdsetv[i], DsetList, 0)) {
               SUMA_SL_Err("Failed to insert dset into list");
               SUMA_free(dset_namev[i]); SUMA_free(targetv[i]);
               SUMA_RETURN(NOPE);
            }
            /* add the columns */
            Ti = (int *) SUMA_calloc(SDSET_VECLEN(sdsetv[i]), sizeof(int));
            for (ii=0; ii <SDSET_VECLEN(sdsetv[i]); ++ii) Ti[ii]=ii;
            SUMA_AddDsetNelCol (sdsetv[i], "node index",
                                SUMA_NODE_INDEX, Ti, NULL, 1);
            SUMA_free(Ti); Ti=NULL;

            atr = NI_get_attribute( nel , "target_nvals" ) ;
               if( atr == NULL )        SUMA_GIQUIT;
            nvals = (int)strtod(atr,NULL) ;  nvals = MAX(1,nvals);

            atr = NI_get_attribute( nel , "target_labels" ) ;
            if( atr != NULL )
               labar = NI_decode_string_list( atr , ";" ) ;

            for( vv=0 ; vv < nvals ; vv++ ){
               if (labar != NULL && vv < labar->num) atr = labar->str[vv];
               else if (vv < 6) {
                  atr = blab[vv];
               } else {
                  atr = "What the hell is this?";
               }
               if (vv%2 == 0) { /* beta */
                  SUMA_AddDsetNelCol (sdsetv[i], atr,
                                SUMA_NODE_FLOAT, NULL, NULL, 1);
               } else { /* zscore */
                  SUMA_AddDsetNelCol (sdsetv[i], atr,
                                SUMA_NODE_ZSCORE, NULL, NULL, 1);
               }
            }
            if (labar) SUMA_free_NI_str_array(labar); labar=NULL;

            SUMA_LHv("Creating overlays for %s\n", targetv[i]);
            /* create overlays */
            ov[i] = SUMA_CreateOverlayPointer (targetv[i], sdsetv[i],
                                               SOv[i]->idcode_str, NULL);
            if (!ov[i]) {
               SUMA_SL_Err("Failed in SUMA_CreateOverlayPointer.\n");
               SUMA_free(dset_namev[i]); SUMA_free(targetv[i]);
               SUMA_RETURN(NOPE);
            }
            ov[i]->ShowMode = SW_SurfCont_DsetViewCol;
            ov[i]->GlobalOpacity = 0.8;
            ov[i]->isBackGrnd = NOPE;
            ov[i]->OptScl->BrightFact = 0.5;
            ov[i]->OptScl->find = 0;
            ov[i]->OptScl->tind = 1;
            ov[i]->OptScl->bind = 0;
            ov[i]->OptScl->UseThr = 1; /* turn on threshold use */
            ov[i]->SymIrange = 1;   /* Use symmetric range */
            ov[i]->OptScl->AutoIntRange = 0; /* Do not update range */
            ov[i]->OptScl->IntRange[0] = -0.5;  /* set the range */
            ov[i]->OptScl->IntRange[1] =  0.5;
            ov[i]->OptScl->ThreshRange[0] = 2.0;
            ov[i]->OptScl->ThreshRange[1] = 0.0;

            /* Now add the overlay to SOv[i]->Overlays */
            if (!SUMA_AddNewPlane ((SUMA_ALL_DO *)SOv[i], ov[i],
                                    SUMAg_DOv, SUMAg_N_DOv, 1)) {
               SUMA_SL_Crit("Failed in SUMA_AddNewPlane");
               SUMA_FreeOverlayPointer(ov[i]);
               SUMA_free(dset_namev[i]); SUMA_free(targetv[i]);
               SUMA_RETURN (NOPE);
            }
            SUMA_free(dset_namev[i]); dset_namev[i]=NULL;
            SUMA_free(targetv[i]); targetv[i]=NULL;
         }
      }

      /* Done with brand new init */
      SUMA_RETURN(YUP);
   }

   CHECK_DSET_AND_OVERLAYS:
   SUMA_LH("Checking Dset and Overlays.\n");

   { /* just use what is in giset */
      if (giset->sdset_ID[0][0] == '\0') {
         SUMA_S_Err("No ID in sdset_ID. Unexpected happenstance");
         SUMA_RETURN(NOPE);
      }
      if (!(sdsetv[0] = SUMA_FindDset_s(giset->sdset_ID[0], DsetList))) {
         SUMA_S_Err("SDSET for 0 not found");
         SUMA_RETURN(NOPE);
      }
      if (giset->sdset_ID[1][0] != '\0') {
         if (!(sdsetv[1] = SUMA_FindDset_s(giset->sdset_ID[1], DsetList))) {
            SUMA_S_Err("SDSET for 1 not found");
            SUMA_RETURN(NOPE);
         }
      }
      /* fetch the overlays */
      for (i=0; i<2; ++i) {
         if (sdsetv[i]) {
            if (!(ov[i] = SUMA_Fetch_OverlayPointerByDset(
                             (SUMA_ALL_DO *)SOv[i],
                             sdsetv[i], &ovind))) {
               SUMA_S_Err("Failed to find overlay pointer");
               SUMA_RETURN(NOPE);
            }
         }
      }
   }

   /* at this point, we have the relevant dsets in sdsetv,
      and their overlays in ov */
   SUMA_RETURN(YUP);
}

/*! find surfaces appropriate for giset */
SUMA_Boolean SUMA_GICOR_Surfaces(GICOR_setup *giset, SUMA_SurfaceObject *SOv[])
{
   static char FuncName[]={"SUMA_GICOR_Surfaces"};

   SUMA_ENTRY;

   if (!(SOv[0] = SUMA_FindSOp_inDOv_from_N_Node(
                        giset->nnode_domain[0],
                        giset->nnode_domain[1] ? SUMA_LEFT:SUMA_NO_SIDE,
                        1, 1,
                        SUMAg_DOv, SUMAg_N_DOv))) {
      SUMA_S_Errv("Could not find domain parent for a domain of %d nodes\n",
               giset->nnode_domain[0]);
      SUMA_RETURN(NOPE);
   }

   if (giset->nnode_domain[1]) {
      if (!(SOv[1]=SUMA_FindSOp_inDOv_from_N_Node(
                           giset->nnode_domain[1], SUMA_RIGHT,
                           1, 1,
                           SUMAg_DOv, SUMAg_N_DOv))) {
         SUMA_S_Errv("Could not find domain parent for a "
                     "RH domain of %d nodes\n",
                     giset->nnode_domain[1]);
         SUMA_RETURN(NOPE);
      }
   }

   SUMA_RETURN(YUP);
}


/*!
   Function called from SUMA_niml, when GICorr sends a setup element
   This function parallels AFNI's GICOR_setup_func
*/
SUMA_Boolean SUMA_GICOR_setup_func( NI_stream nsg , NI_element *nel )
{
   static char FuncName[]={"SUMA_GICOR_setup_func"};
   GICOR_setup *giset = NULL;
   SUMA_DSET *sdsetv[2]={NULL, NULL};
   SUMA_OVERLAYS *ov[2]={NULL, NULL};
   SUMA_SurfaceObject *SOv[2]={NULL, NULL};
   char *pre=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;


   /* fetch the giset struct */
   giset = SUMAg_CF->giset;
   if( giset != NULL && giset->ready ) SUMA_RETURN(YUP) ;

   if( giset == NULL ){
     giset = (GICOR_setup *)SUMA_calloc(1,sizeof(GICOR_setup)) ;
     SUMAg_CF->giset = giset;
   } else {
     memset(giset,0, sizeof(GICOR_setup)) ;
   }

   if (!SUMA_init_GISET_setup(nsg, nel, giset, 0)) SUMA_GIQUIT;

   /* Now find surfaces that can be the domain */
   if (!SUMA_GICOR_Surfaces(giset, SOv)) {
      SUMA_S_Err("Failed to find surfaces for giset");
      SUMA_RETURN(NOPE);
   }

   /* Now create appropriate dsets */
   pre = NI_get_attribute( nel , "target_name" ) ;
   if( pre == NULL || *pre == '\0' ) pre = "GICorrelletto" ;
   if (!SUMA_GICOR_Dsets(SOv, giset, pre, SUMAg_CF->DsetList,
                         sdsetv, ov, nel)) {
      SUMA_S_Err("Failed to find/create dsets for giset");
      SUMA_RETURN(NOPE);
   }


   giset->ready = 1 ;

   if (LocalHead) {
      SUMA_Show_GISET(giset, NULL, 0);
   }

   SUMA_RETURN(YUP) ;
}

/*! Surface version of AFNI's GICOR_process_dataset*/
SUMA_Boolean SUMA_GICOR_process_dataset( NI_element *nel  )
{
   static char FuncName[]={"SUMA_GICOR_process_dataset"};
   GICOR_setup *giset = SUMAg_CF->giset ;
   int vmul ; float thr ;
   int id=0, ic=0;
   char *seedstr=NULL, *Cside=NULL, ident[300]={""}, prefix[300]={""};
   SUMA_SurfaceObject *SOv[2]={NULL,NULL};
   SUMA_DSET *sdsetv[2]={NULL, NULL};
   SUMA_OVERLAYS *ov[2]={NULL, NULL};
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if( nel == NULL || nel->vec_num < 2 ){  /* should never happen */
     ERROR_message("badly formatted dataset from 3dGroupInCorr!") ;
     SUMA_RETURN(NOPE) ;
   }

   if (nel->vec_num % 2) {
      SUMA_SLP_Err("Number of sub-bricks not multiple of two!");
      SUMA_RETURN(NOPE) ;
   }

   if( giset == NULL ||
       !giset->ready   ){   /* should not happen */

     if( giset != NULL ) giset->ready = 0 ;
     /* AFNI_misc_CB(im3d->vwid->func->gicor_pb,(XtPointer)im3d,NULL) ; */
     SUMA_SLP_Err(" ******* SUMA: *********\n"
                  "  3dGrpInCorr sent data \n"
                  "  but setup isn't ready!\n" ) ;
     SUMA_RETURN(NOPE) ;
   }

   /* get the surfaces, the dsets, and their overlays */
   if (!SUMA_GICOR_Surfaces(giset, SOv)) {
      SUMA_S_Err("Failed to find surfaces for giset");
      SUMA_RETURN(NOPE);
   }
   if (!SUMA_GICOR_Dsets(SOv, giset, NULL, SUMAg_CF->DsetList,
                         sdsetv, ov, NULL)) {
      SUMA_S_Err("Failed to find/create dsets for giset");
      SUMA_RETURN(NOPE);
   }

   /* copy NIML data into dataset */

   SUMA_LH("Populating the dset in question, redisplay, etc.");

   if (!SUMA_PopulateDsetsFromGICORnel(nel, giset, sdsetv)) {
      SUMA_S_Err("Failed to populate. Not fun.");
      SUMA_RETURN(NOPE);
   }

   /* Add dsets to autosave list */
   if (!(seedstr = NI_get_attribute(nel,"boomerang_msg"))) {
      seedstr = "seed_XXXX";
   }
   for (id=0; id<2; ++id) {
      if (sdsetv[id] && SOv[id]) {
         snprintf(ident,298*sizeof(char), "filename:%s",
                                          SDSET_FILENAME(sdsetv[id]));
         if (SOv[id]->Side == SUMA_LEFT) Cside = ".lh";
         else if (SOv[id]->Side == SUMA_RIGHT) Cside = ".rh";
         else Cside = "";
         snprintf(prefix,298*sizeof(char),"seed_%s.GIC%s",
                     seedstr, Cside);
         if (!(SUMA_Add_to_SaveList(&SUMAg_CF->SaveList, "sdset",
                                    ident, prefix))) {
            SUMA_S_Warnv("Failed to add to save list %s %s\n", ident, prefix);
         }
      }
   }

   /* colorize and redisplay */
   for (id=0; id<2; ++id) {
      if (ov[id] && SOv[id]) {
         SOv[id]->SurfCont->curColPlane = ov[id];
         if (!SUMA_OpenCloseSurfaceCont(NULL, (SUMA_ALL_DO*)SOv[id], NULL)) {
            SUMA_SLP_Err("Cannot open Surface Controller!");
            SUMA_RETURN(NOPE);
         }
         if ( SOv[id]->SurfCont->SwitchDsetlst &&
              !SOv[id]->SurfCont->SwitchDsetlst->isShaded ) {
             SUMA_RefreshDsetList ((SUMA_ALL_DO*)SOv[id]);
         }
         SUMA_LHv("Colorizing %d\n", id);
         if (!SUMA_ColorizePlane (ov[id])) {
            SUMA_SLP_Err("Failed to colorize plane.\n");
            SUMA_RETURN(NOPE);
         }
         if (!SUMA_Remixedisplay ((SUMA_ALL_DO*)SOv[id])) {
            SUMA_SLP_Err("Failed to remix redisplay.\n");
            SUMA_RETURN(NOPE);
         }
         SUMA_LHv("Initializing %d\n", id);
         SUMA_UpdateColPlaneShellAsNeeded((SUMA_ALL_DO*)SOv[id]);
      }
   }

   #if 0 /* This is no longer needed because of the modification
            of SE_Redisplay*All* . The last controller to be
            redrawn is now the one in which the pointer last
            resided. I leave this here in case the problem
            resurfaces again ... */
   /* if you have multiple controllers open, force a redisplay
      in the one where the click occurred, to force a reset of
      GL's State variables. Otherwise you may have problems
      selecting nodes repeatedly without going in and out of the window */
   if (SUMAg_CF->PointerLastInViewer >= 0 && SUMAg_N_SVv > 1) {
      SUMA_SurfaceViewer *sv = &(SUMAg_SVv[SUMAg_CF->PointerLastInViewer]);
      if (!sv->isShaded) { /* this should always be true ... */
         SUMA_OpenGLStateReset(SUMAg_DOv, SUMAg_N_DOv, sv);
         SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
      }
   }
   #endif

   if (LocalHead) {
      SUMA_ShowDset(sdsetv[0],0,NULL);
      SUMA_ShowDset(sdsetv[1],0,NULL);
   }

   #if 0
      /* YOU CANNOT DO THIS: Although meshes are isotopic,
         a node index do not necessarily refer to a homologous
         anatomical region on both left and right surfaces. */
   if (SUMAg_CF->Dev) {
      if (SOv[0] && SOv[1] &&
          SOv[0]->N_Node == SOv[1]->N_Node) {
         static float *fv1=NULL, *fv0=NULL;
         static double dot[12];
         int ii=0;

         if (!fv0) fv0= (float *)SUMA_calloc(SOv[0]->N_Node, sizeof(float));
         if (!fv1) fv1= (float *)SUMA_calloc(SOv[1]->N_Node, sizeof(float));
         SUMA_S_Note (  "Cross correlation of left with right "
                        "hemisphere patterns:\n");
         for (ipair=0; ipair < nel->vec_num/2; ++ipair) {
            memcpy(fv0,(SDSET_VEC(sdsetv[0],ipair)),
                              sizeof(float)*SDSET_VECLEN(sdsetv[0])) ;
            memcpy(fv1,(SDSET_VEC(sdsetv[1],ipair)),
                              sizeof(float)*SDSET_VECLEN(sdsetv[1])) ;
            THD_normalize( SDSET_VECLEN(sdsetv[0]) , fv0 ) ;
            THD_normalize( SDSET_VECLEN(sdsetv[1]) , fv1 ) ;
            dot[ipair]=0.0;
            for (ii=0; ii<SDSET_VECLEN(sdsetv[0]); ++ii)
               dot[ipair] += (double)fv0[ii]*(double)fv1[ii];
            fprintf(SUMA_STDERR, "Pair %d: %f\t", ipair, dot[ipair]);
         }
         fprintf(SUMA_STDERR, "\n");
      }
   }
   #endif

   SUMA_RETURN(YUP) ;
}

/*!
   SUMA's version of AFNI_gicor_setref
*/
int SUMA_AFNI_gicor_setref( SUMA_SurfaceObject *SO, int node )
{
   static char FuncName[]={"SUMA_AFNI_gicor_setref"};
   NI_element *nel=NULL;
   char buf[256]={"bise"}, boom[64]={""}, *cside=NULL;
   GICOR_setup *giset = SUMAg_CF->giset ;
   THD_fvec3 iv,jv; THD_ivec3 kv;
   int ijk=-1,ii=0;
   SUMA_SurfaceObject *SOv[2]={NULL, NULL};
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!SO) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(-1) ;
   }

   if (node < 0) { /* OK, return */
      SUMA_LHv("node %d\n", node);
      SUMA_RETURN(0) ;
   }

   if( giset == NULL ||
       !giset->ready   ){   /* should not happen */
     SUMA_LHv("giset=%p, giset->ready=%d\n",
               giset, giset ? giset->ready:-111);
     if( giset != NULL ) giset->ready = 0 ;
     SUMA_RETURN(-1) ;
   }

   /* change node index to proper ijk */
   if (!SUMA_GICOR_Surfaces(giset, SOv)) {
      SUMA_S_Err("Failed to find surfaces for giset");
      SUMA_RETURN(-1);
   }

   if (SUMA_isRelated_SO(SO, SOv[0],1)) {
      ijk = node;
   } else if (SUMA_isRelated_SO(SO, SOv[1],1)) {
      ijk = node+giset->nnode_domain[0];
   } else {
      SUMA_SLP_Warn("Cannot change node to ijk");
      SUMA_RETURN(-1);
   }

   cside = "";
   if (SO->Side == SUMA_LEFT) cside = "L";
   else if (SO->Side == SUMA_RIGHT) cside = "R";
   sprintf(boom,"%d%s", node, cside);
   SUMA_LHv("Node %d --> ijk %d\n", node, ijk);

   /* if socket has gone bad, we're done */

   if( NI_stream_goodcheck(giset->ns,1) < 1 ){
     SUMA_S_Note("Socket socks, toggling off connection");
     SUMAg_CF->Connected_v[SUMA_GICORR_LINE]=NOPE;
     if( giset != NULL ) giset->ready = 0 ;
     SUMA_RETURN(-1) ;
   }

   /* find where we are */

   /* INFO_message("DEBUG: AFNI_gicor_setref called: ijk=%d",ijk) ; */

   if( giset->ivec != NULL ){
     ii = bsearch_int( ijk , giset->nvec , giset->ivec ) ;
     if( ii < 0 ){
       WARNING_message("GrpInCorr set point not in mask from 3dGroupInCorr") ;
       SUMA_RETURN(0) ; /* don't return an error */
     }
   }

   /* send ijk node index to 3dGroupInCorr */
   nel = NI_new_data_element( "SETREF_ijk" , 0 ) ;
   NI_set_attribute(nel, "boomerang_msg", boom);

   sprintf( buf , "%d" , ijk ) ;
   NI_set_attribute( nel , "index" , buf ) ;

   sprintf( buf , "%g" , giset->seedrad ) ;
   NI_set_attribute( nel , "seedrad" , buf ) ;

   sprintf( buf , "%d" , giset->ttest_opcode ) ;
   NI_set_attribute( nel , "ttest_opcode" , buf ) ;

   ii = NI_write_element( giset->ns , nel , NI_TEXT_MODE ) ;
   NI_free_element( nel ) ;
   if( ii <= 0 ){
     ERROR_message("3dGroupInCorr connection has failed?!") ;
     SUMA_RETURN(-1) ;
   }

   SUMA_RETURN(0) ;
}


void SUMA_Show_GISET(GICOR_setup *giset, FILE *out, int verb) {
   static char FuncName[]={"SUMA_Show_GISET"};
   char *s=NULL;

   SUMA_ENTRY;

   s = SUMA_GISET_Info(giset, verb);

   if (!out) {
      out = SUMA_STDOUT;
   }

   fprintf(out, "%s\n", s);

   SUMA_free(s); s = NULL;

   SUMA_RETURNe;
}

char *SUMA_GISET_Info(GICOR_setup *giset, int verb) {
   static char FuncName[]={"SUMA_GISET_Info"};
   char *s=NULL;
   SUMA_STRING *SS=NULL;

   SUMA_ENTRY;

   SS = SUMA_StringAppend_va(NULL, NULL);

   if (giset) {
      SS = SUMA_StringAppend(SS, "   GICORR-setup:\n");
      SS = SUMA_StringAppend_va(SS, "     ready: %d\n"
                                    "     ndset: %d %d, nvec: %d\n"
                                    "     ttestopcode: %d, vmul: %d\n"
                                    "     seedrad: %f\n"
                                    "     ns: %p\n"
                                    "     session: %p, dset: %p (%s)\n"
                                    "     nds:%d, nvox: %d\n"
                                    "     nivec: %d, ivec: %p\n"
                                    "     sdset_ID: %s, %s\n"
                                    "     nnode_domain: %d, %d\n"
                                    "     nnode_mask: %d %d\n",
           giset->ready,
           giset->ndset_A , giset->ndset_B , giset->nvec,
           giset->ttest_opcode , giset->vmul, giset->seedrad,
           giset->ns, giset->session, giset->dset,
           giset->dset ? DSET_PREFIX(giset->dset):"NULL",
           giset->nds, giset->nvox, giset->nivec, giset->ivec,
           giset->sdset_ID[0] ? giset->sdset_ID[0]:"NULL",
           giset->sdset_ID[1] ? giset->sdset_ID[1]:"NULL",
           giset->nnode_domain[0], giset->nnode_domain[1],
           giset->nnode_mask[0], giset->nnode_mask[1]);
   } else {
      SS = SUMA_StringAppend_va(SS, "   GICORR-setup: NULL\n");
   }

   SS = SUMA_StringAppend_va(SS, NULL);
   s = SS->s; SUMA_free(SS); SS= NULL;
   SUMA_RETURN(s);
}
