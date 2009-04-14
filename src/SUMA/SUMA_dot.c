#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  

void SUMA_dot_product_CB( void *params) 
{
   static char FuncName[]={"SUMA_dot_product_CB"};

   char *SO_idcode=NULL, *ts_dset_id=NULL, 
         *dot_dset_id=NULL;
   SUMA_DSET *in_dset=NULL, *ts_src_dset=NULL; 
   double *ts=NULL; 
   int ts_node=0, N_ts, ii, jj;
   float *fv=NULL;
   SUMA_DSET *out_dset=NULL, *child=NULL;
   SUMA_CALLBACK *cb= (SUMA_CALLBACK *)params;
   SUMA_XFORM *xf=NULL;
   NI_element *nelts = NULL, *nelpars=NULL;
   NI_group *ngr = NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_OVERLAYS *Sover=NULL;
   NI_element *dotopt=NULL;
   SUMA_Boolean LocalHead = YUP;
   
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
   
   if (!(nelpars = SUMA_FindNgrNamedElement(ngr, "parameters"))) {
      SUMA_S_Err("parameters element not found!");
      SUMA_RETURNe;
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
         if (ts_node >= 0) {
            SUMA_LH("Have ts_node AND ts. ts takes precedence\n");
         }
         N_ts = nelts->vec_len;
         /* OK, now get your ts */
         ts = (double *)SUMA_calloc(N_ts, sizeof(double));
         fv = (float *)nelts->vec[0];
         for (ii=0; ii<N_ts; ++ii) 
            ts[ii] = (double)fv[ii];
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
      
      /* Go find out, which xform produced this child */
      if (!(xf=SUMA_Find_XformByID(cb->creator_xform))) {
         SUMA_S_Err("Have no way to reliably select time series dset"
                    "that produced child");
         SUMA_RETURNe;
      }
      
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
      SUMA_LHv("Getting time series from node %d on dset %s\n",
               ts_node, SDSET_FILENAME(ts_src_dset));
      /* get the time series */
      if (!(ts = (double*)SUMA_GetDsetAllNodeValsInCols2(ts_src_dset, 
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
      
      if (0 && LocalHead) {
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
      if (!SUMA_dot_product(in_dset,ts,&out_dset,nelpars)) {
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

SUMA_Boolean SUMA_dot_product(SUMA_DSET *in_dset,
                              double *ts, 
                              SUMA_DSET **out_dsetp,
                              NI_element *dotopt) 
{
   static char FuncName[]={"SUMA_dot_product"};
   SUMA_DSET *dot = NULL;
   double *dcol=NULL;
   float *fcol = NULL;
   int ic=0, ir=0;
   byte *bbv=NULL;
   int *iiv=NULL;
   float *ffv=NULL;
   double  *ddv=NULL;
   int prec=1;
   char *s=NULL, *sname=NULL;
   SUMA_VARTYPE vtp = SUMA_notypeset;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
      
   if (  !in_dset || 
         !out_dsetp ||
         !ts   )  {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_LH("Checking consistency");
   if (!SUMA_is_AllConsistentNumeric_dset(in_dset, &vtp)) {
      SUMA_S_Err( "Input dataset is either not all numeric \n"
                  "or has inconsistent types");
      SUMA_RETURN(NOPE);
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
         if (!SUMA_AddDsetNelCol(dot, "dotproduct", SUMA_NODE_FLOAT, 
                                 (void *)fcol, NULL , 1)) {
            SUMA_S_Err("Failed to add col");
            SUMA_RETURN(NOPE);
         }
         SUMA_free(fcol); fcol = (float*)dot->dnel->vec[0];
      } 
      *out_dsetp = dot;
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
