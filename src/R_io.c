#include <R.h>
#include <Rdefines.h>
#include "mrilib.h"


SEXP R_THD_load_dset(SEXP Sfname)
{
   SEXP Rdset, brik, head, names;
   int i=0, ip=0, sb, cnt=0;
   char *fname = NULL, *head_str;
   NI_group *ngr=NULL;
   NI_element *nel=NULL;
   char *listels[2] = {"head","brk"}; /* the brk is on purpose 
                                         for backward compatibility */
   double *dv=NULL;
   float *fv=NULL;
   THD_3dim_dataset *dset = NULL;
   int debug=0;
   
   /* get the filename */
   PROTECT(Sfname = AS_CHARACTER(Sfname));
   fname = R_alloc(strlen(CHAR(STRING_ELT(Sfname,0))), sizeof(char));
   strcpy(fname, CHAR(STRING_ELT(Sfname,0)));
   
   /* open dset */
   dset = THD_open_dataset(fname);
   if (dset) {
      if (debug) INFO_message("Dset %s was loaded 2\n", fname);
    } else {
      ERROR_message("Dset %s could not be loaded\n", fname);
      UNPROTECT(1);
      return(R_NilValue);
   }

   /* form one long header string */
   ngr = THD_nimlize_dsetatr(dset);
   PROTECT(head = allocVector(STRSXP, ngr->part_num));
   for (ip=0,i=0; i<ngr->part_num; ++i) {
      switch( ngr->part_typ[i] ){
         /*-- a sub-group ==> recursion! --*/
         case NI_GROUP_TYPE:
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)ngr->part[i] ;
            head_str = NI_write_element_tostring(nel);
            if (debug > 1) fprintf(stderr,"%s\n", head_str);
            SET_STRING_ELT(head, ip, mkChar(head_str)); ++ip;
            free(head_str);
            break;
         default:
            break;
      }
   }
   
   NI_free(ngr); 
   
   /* form one long array of data */
   PROTECT(brik = NEW_NUMERIC(DSET_NVOX(dset)*DSET_NVALS(dset)));
   dv = NUMERIC_POINTER(brik);
   EDIT_floatize_dataset(dset);
   for (cnt=0, sb=0; sb<DSET_NVALS(dset); ++sb) {
      fv = (float *)DSET_BRICK_ARRAY(dset,sb);
      for (i=0; i<DSET_NVOX(dset); ++i) {
         dv[cnt++] = fv[i]; 
         if (debug > 2) fprintf(stderr,"%f\t", fv[i]);
      }
      if (debug > 2) fprintf(stderr,"\n");
   }
   
   /* done with dset, dump it */
   DSET_delete(dset);
   
   /* form output list */
   PROTECT(names = allocVector(STRSXP,2));
   for (i=0; i<2; ++i) {
      SET_STRING_ELT(names, i, mkChar(listels[i]));
   } 
   PROTECT(Rdset = allocVector(VECSXP,2));
   SET_VECTOR_ELT(Rdset, 0, head);
   SET_VECTOR_ELT(Rdset, 1, brik);
   setAttrib(Rdset, R_NamesSymbol, names);
   
   UNPROTECT(5);
   
   return(Rdset);
}
