#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  

void SUMA_dot_product_CB( void *params) 
{
   static char FuncName[]={"SUMA_dot_product_CB"};

   char *SO_idcode=NULL, *in_dset_idcode=NULL, *out_dset_idcode=NULL;
   SUMA_DSET *in_dset=NULL; 
   double *ts=NULL; 
   int ts_node=0, N_ts;
   SUMA_DSET *out_dset=NULL;
   NI_element *nel = (NI_element *)params;
   SUMA_SurfaceObject *SO=NULL;
   NI_element *dotopt=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!nel) {
      SUMA_S_Err("NULL input");
      SUMA_RETURNe;
   }
   
   if (strcmp(nel->name,"Dot.CB")) {
      SUMA_S_Errv( "pad parameter structure %s\n"
                  "Expected dot_product_callback\n", nel->name);
      SUMA_RETURNe;
   }
   
   in_dset_idcode = NI_get_attribute(nel, "in_dset_idcode");
   if (!in_dset_idcode) {
      SUMA_S_Err("Failed to get proper in_dset_idcode attribute");
      SUMA_RETURNe;
   }
   if (!(in_dset = SUMA_FindDset_s(in_dset_idcode, SUMAg_CF->DsetList))) {
      SUMA_S_Err("Failed to find in_dset");
      SUMA_RETURNe;  
   }
   
   out_dset_idcode = NI_get_attribute(nel, "out_dset_idcode");
   if (!out_dset_idcode) {
      SUMA_S_Err("Failed to get proper out_dset_idcode attribute");
      SUMA_RETURNe;
   }
   if (!(out_dset = SUMA_FindDset_s(out_dset_idcode, SUMAg_CF->DsetList))) {
      SUMA_S_Err("Failed to find out_dset");
      SUMA_RETURNe; 
   }
   SO_idcode = NI_get_attribute(nel, "SO_idcode");
   if (!SO_idcode) {
      SUMA_S_Err("Failed to get proper SO_idcode attribute");
      SUMA_RETURNe;
   }
   if (!(SO = SUMA_findSOp_inDOv (SO_idcode, SUMAg_DOv, SUMAg_N_DOv))) {
      SUMA_S_Err("Failed to find SO");
      SUMA_RETURNe; 
   }
   NI_GET_INT(nel, "ts_node", ts_node);
   if (!NI_GOT || ts_node < 0 || ts_node > SO->N_Node) {
      SUMA_S_Err("Failed to get proper ts_node attribute");
      SUMA_RETURNe;
   }
   /* get the time series */
   if (!(ts = (double*)SUMA_GetDsetAllNodeValsInCols2(in_dset, 
                              NULL, 0, 
                              ts_node, SO->N_Node, /* test this
                                                    SO->N_Node when
                                                    using patches. 
                                                    Else -1 */
                              &N_ts,
                              SUMA_double))) { 
      SUMA_S_Err("Failed to extract time series.");
      SUMA_RETURNe;
   }
   /* call dot product computer */
   if (!SUMA_dot_product(in_dset,ts,&out_dset,dotopt)) {
      SUMA_S_Err("Failed to compute dot product");
      SUMA_RETURNe;
   }
   
   
   if (dotopt) NI_free(dotopt); dotopt = NULL;
   if (ts) SUMA_free(ts); ts = NULL;
   
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
   char *s=NULL;
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
      SUMA_LHv("Creating new  dset with precision %d\n", prec);
      if (!(dot = SUMA_CreateDsetPointer( 
                  "dotty", SUMA_NODE_BUCKET, NULL, 
                   NI_get_attribute(in_dset->ngr,"domain_parent_idcode"),
                   SDSET_VECLEN(in_dset)  ) ) ){
                   
      }
      SUMA_LabelDset(dot, "dotty");
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
      } else {
         fcol = (float *) SUMA_calloc(SDSET_VECLEN(in_dset), sizeof(float));
         if (!SUMA_AddDsetNelCol(dot, "dotproduct", SUMA_NODE_FLOAT, 
                                 (void *)fcol, NULL , 1)) {
            SUMA_S_Err("Failed to add col");
            SUMA_RETURN(NOPE);
         }
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
