#include "SUMA_suma.h"


SUMA_Boolean SUMA_dot_product(   SUMA_DSET *in_dset, 
                                 float *ts, 
                                 SUMA_DSET **out_dsetp,
                                 SUMA_DOT_OPT opt)
{
   static char FuncName[]={"SUMA_dot_product"};
   SUMA_DSET *dot = NULL;
   double *dcol=NULL;
   SUMA_VARTYPE vtp = SUMA_notypeset;
   
   SUMA_ENTRY;
   
   if (  !in_dset || 
         !out_dsetp ||
         !ts   )  {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   
   if (!SUMA_isAllConsistentNumeric_dset(in_dset, &vtp)) {
      SUMA_S_Err( "Input dataset is either not all numeric \n"
                  "or has inconsistent types");
      SUMA_RETURN(NOPE);
   }
   
   /* decide if we need new_dset or not */
   if (*out_dsetp) {
      dot = *out_dsetp;
      dcol = (double*)dot->dnel->vec[0];
      memset((void *)dcol, 0, SDSET_VECLEN(dot)*sizeof(double));
   } else {
      if (!dot = SUMA_CreateDsetPointer( 
                  "dotty", SUMA_NODE_BUCKET, NULL, 
                   NI_get_attribute(in_dset->ngr,"domain_parent_idcode"),
                   SDEST_VECLEN(in_dset)  ) ) {
                   
      }
      dcol = (double *) SUMA_calloc(SDEST_VECLEN(in_dset), sizeof(double));
      if (!SUMA_AddDsetNelCol(dot, "dotproduct", SUMA_NODE_DOUBLE, 
                              (void *)dcol, NULL , 1)) {
         SUMA_S_Err("Failed to add col");
      }
      *out_dsetp = dot;
   }
   
   switch(vtp) {
      case SUMA_byte:
         for (ic=0; ic<SDSET_VECNUM(in_dset); ++ic) {
            bbv = (byte*)in_dset->dnel->vec[ic];
            for (ir=0; ir<SDSET_VECLEN(in_dset); ++ic) {
               dcol[ir] += ts[ir]*(double)bbv[ir];
            }  
         }
         break;
      case SUMA_int:
         for (ic=0; ic<SDSET_VECNUM(in_dset); ++ic) {
            iiv = (int*)in_dset->dnel->vec[ic];
            for (ir=0; ir<SDSET_VECLEN(in_dset); ++ic) {
               fcol[ir] += ts[ir]*(double)iiv[ir];
            }  
         }
         break;
      case SUMA_float:
         for (ic=0; ic<SDSET_VECNUM(in_dset); ++ic) {
            ffv = (float*)in_dset->dnel->vec[ic];
            for (ir=0; ir<SDSET_VECLEN(in_dset); ++ic) {
               fcol[ir] += ts[ir]*(double)ffv[ir];
            }  
         }
         break;
      case SUMA_double: 
         for (ic=0; ic<SDSET_VECNUM(in_dset); ++ic) {
            ddv = (double*)in_dset->dnel->vec[ic];
            for (ir=0; ir<SDSET_VECLEN(in_dset); ++ic) {
               fcol[ir] += ts[ir]*ddv[ir];
            }  
         }
         break;
      default:
         SUMA_S_Err("What kind of numeric type is this?");
         SUMA_RETURN(NOPE);
         break;
   }     
   
   SUMA_RETURN(YUP);
   
}
