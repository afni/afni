#include <R.h>
#include <Rdefines.h>
#include "mrilib.h"

static int odebug;
void set_odebug(int dd) { odebug = dd; }
int get_odebug(void) { return(odebug); }

SEXP getListElement(SEXP list, const char *str)
{
   int debug=0;
   R_len_t i;
   SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
   
   if (!debug) debug = get_odebug();
   
   for (R_len_t i = 0; i < length(list); i++) {
      if (debug) INFO_message("Element %d/%d: named %s\n", 
                     i, length(list), CHAR(STRING_ELT(names, i)));
      if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
         elmt = VECTOR_ELT(list, i);
         break;
      }
   }
   return elmt;
}



SEXP R_THD_load_dset(SEXP Sfname, SEXP Opts)
{
   SEXP Rdset, brik, head, names, opt;
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
   
   if (!debug) debug = get_odebug();
   
   /* get the options list, maybe */
   PROTECT(Opts = AS_LIST(Opts));
   if ((opt = getListElement(Opts,"debug")) != R_NilValue) {
	   debug = (int)INTEGER_VALUE(opt);
      if (debug>2) set_odebug(debug);
	   if (debug) INFO_message("Debug is %d\n", debug);
   }
   
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
      UNPROTECT(2);
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
   
   UNPROTECT(6);
   
   return(Rdset);
}

SEXP R_THD_write_dset(SEXP Sfname, SEXP Sdset, SEXP Opts)
{
   SEXP Rdset, brik, head, names, opt;
   int i=0, ip=0, sb, cnt=0, scale = 1, overwrite=0, addFDR=0;
   char *fname = NULL, *head_str, *stmp=NULL;
   NI_group *ngr=NULL;
   NI_element *nel=NULL;
   char *listels[2] = {"head","brk"}; /* the brk is on purpose 
                                         for backward compatibility */
   double *dv=NULL;
   float *fv=NULL;
   THD_3dim_dataset *dset = NULL;
   int debug=0;
   
   if (!debug) debug = get_odebug();

   /* get the options list, maybe */
   PROTECT(Opts = AS_LIST(Opts));
   if ((opt = getListElement(Opts,"debug")) != R_NilValue) {
	   debug = (int)INTEGER_VALUE(opt);
      if (debug>2) set_odebug(debug);
	   if (debug > 1) INFO_message("Debug is %d\n", debug);
   }
   
   /* get the filename */
   PROTECT(Sfname = AS_CHARACTER(Sfname));
   fname = R_alloc(strlen(CHAR(STRING_ELT(Sfname,0))), sizeof(char));
   strcpy(fname, CHAR(STRING_ELT(Sfname,0)));
   if (debug) INFO_message("Output filename %s\n"
                          , fname);
   
   /* get the dset structure elements */
   PROTECT(Rdset = AS_LIST(Sdset));
   if ((head = AS_CHARACTER(getListElement(Rdset,"head"))) == R_NilValue) {
      ERROR_message("No header found");
      UNPROTECT(3);
      return(R_NilValue);
   }
   if (debug > 1) INFO_message("First head element %s\n"
                          , CHAR(STRING_ELT(head,0)));
   if ((brik = AS_NUMERIC(getListElement(Rdset,"brk"))) == R_NilValue) {
      ERROR_message("No brick found");
      UNPROTECT(3);
      return(R_NilValue);
   }
   dv = NUMERIC_POINTER(brik);
   if (debug > 1) INFO_message("First brik value %f\n"
                          , dv[0]);
   
                          
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "AFNI_dataset" );
   NI_set_attribute(ngr,"AFNI_prefix", fname);
   if ((opt = getListElement(Opts,"idcode")) != R_NilValue) {
   	opt = AS_CHARACTER(opt);
	   stmp = (char *)(CHAR(STRING_ELT(opt,0)));
      if (stmp && !strcmp(stmp,"SET_AT_WRITE_FILENAME")) {
         stmp = UNIQ_hashcode(fname);
         NI_set_attribute(ngr, "AFNI_idcode", stmp);
         free(stmp);
      } else if (stmp && !strcmp(stmp,"SET_AT_WRITE_RANDOM")) {
         stmp = UNIQ_idcode() ;
         NI_set_attribute(ngr, "AFNI_idcode", stmp);
         free(stmp);
      } else if (stmp) {
         NI_set_attribute(ngr, "AFNI_idcode",
			   (char *)(CHAR(STRING_ELT(opt,0)))); 	
      }
   }
   if ((opt = getListElement(Opts,"scale")) != R_NilValue) {
	   scale = (int)INTEGER_VALUE(opt);
	   if (debug > 1) INFO_message("Scale is %d\n", scale);
   }
   if ((opt = getListElement(Opts,"overwrite")) != R_NilValue) {
	   overwrite = (int)INTEGER_VALUE(opt);
      if (debug) INFO_message("overwrite is %d\n", overwrite); 	
      THD_force_ok_overwrite(overwrite) ;
      if (overwrite) THD_set_quiet_overwrite(1);
   }	
   if ((opt = getListElement(Opts,"addFDR")) != R_NilValue) {
	   addFDR = (int)INTEGER_VALUE(opt);
      if (debug) INFO_message("addFDR is %d\n", addFDR); 	
   }
   	
   for (ip=0,i=0; i<length(head); ++i) {
      head_str = (char *)CHAR(STRING_ELT(head,i));
      if (debug>1) {
         INFO_message("Adding %s\n", head_str);
      }
      nel = NI_read_element_fromstring(head_str);
      NI_add_to_group(ngr,nel);
   }
   
   if (debug) INFO_message("Creating dset header\n");
   if (!(dset = THD_niml_to_dataset(ngr, 1))) {
      ERROR_message("Failed to create header");
      UNPROTECT(3);
      return(R_NilValue);
   }
   if (debug > 1) {
         INFO_message("Have header of %d, %d, %d, %d\n", 
                       DSET_NX(dset), DSET_NY(dset), 
                       DSET_NZ(dset), DSET_NVALS(dset));
   }
   
   for (i=0; i<DSET_NVALS(dset); ++i) {
      if ( ( DSET_BRICK_TYPE(dset,i) != MRI_byte && 
      	     DSET_BRICK_TYPE(dset,i) != MRI_short ) ||
	     scale == 0 ) {
      	EDIT_substitute_brick(dset, i, 
                            MRI_double, dv+i*DSET_NVOX(dset));	
      } else {
      	EDIT_substscale_brick(dset, i, 
                            MRI_double, dv+i*DSET_NVOX(dset),
                            DSET_BRICK_TYPE(dset,i), -1.0);
      }                      
   }
   
   /* THD_update_statistics( dset ) ; */
   
   if (addFDR) {
      DSET_BRICK_FDRCURVE_ALLKILL(dset) ;
      DSET_BRICK_MDFCURVE_ALLKILL(dset) ;  /* 22 Oct 2008 */
      if( addFDR > 0 ){
         int  nFDRmask=0;    /* in the future, perhaps allow for a mask */
         byte *FDRmask=NULL; /* to be sent in also, for now, mask is exact */
                             /* 0 voxels . */
         mri_fdr_setmask( (nFDRmask == DSET_NVOX(dset)) ? FDRmask : NULL ) ;
         ip = THD_create_all_fdrcurves(dset) ;
         if( ip > 0 ){
            ININFO_message("created %d FDR curve%s in dataset header",
                           ip,(ip==1)?"\0":"s") ;
         } else {
            ININFO_message("failed to create FDR curves in dataset header") ;
         }
      }
   }
   
   DSET_write(dset); 
  
   UNPROTECT(3);
   return(R_NilValue);  
}
