#include <R.h>
#include <Rdefines.h>
#include "mrilib.h"
#include "suma_suma.h"

static int odebug;
void set_odebug(int dd) { odebug = dd; }
int get_odebug(void) { return(odebug); }

SEXP getListElement(SEXP list, const char *str)
{
   int debug=0;
   R_len_t i;
   SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
   
   if (!debug) debug = get_odebug();
   
   for ( i = 0; i < length(list); i++) {
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
   SEXP Rdset, brik, head, names, opt, node_list=R_NilValue;
   int i=0, ip=0, sb, cnt=0, *iv=NULL, kparts=2;
   char *fname = NULL, *head_str;
   NI_group *ngr=NULL;
   NI_element *nel=NULL;
   char *listels[3] = {"head","brk","index_list"}; /* the brk is on purpose 
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
	   if (debug>1) INFO_message("Debug is %d\n", debug);
   }
   
   /* get the filename */
   PROTECT(Sfname = AS_CHARACTER(Sfname));
   fname = R_alloc(strlen(CHAR(STRING_ELT(Sfname,0)))+1, sizeof(char));
   strcpy(fname, CHAR(STRING_ELT(Sfname,0)));
   
   /* open dset */
   dset = THD_open_dataset(fname);
   if (dset) {
      if (debug > 1) INFO_message("Dset %s was loaded 2\n", fname);
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
   
   NI_free_element(ngr); 
   
   if (debug > 1) fprintf(stderr,"Forming data array of %d elements\n",
                                 DSET_NVOX(dset)*DSET_NVALS(dset));
   /* form one long array of data */
   PROTECT(brik = NEW_NUMERIC(DSET_NVOX(dset)*DSET_NVALS(dset)));
   dv = NUMERIC_POINTER(brik);
   EDIT_floatize_dataset(dset);
   for (cnt=0, sb=0; sb<DSET_NVALS(dset); ++sb) {
      if (!(fv = (float *)DSET_BRICK_ARRAY(dset,sb))) {
         ERROR_message("NULL brick array %d!\n", sb);
         UNPROTECT(4);
         return(R_NilValue);
      }
      if (debug > 1) fprintf(stderr,"Filling sb %d\n", sb);
      for (i=0; i<DSET_NVOX(dset); ++i) {
         dv[cnt++] = fv[i]; 
         if (debug > 1) {
            if (debug > 2 || i<10) {
	 	         fprintf(stderr,"%f\t", fv[i]);
            }
         }
      }
      if (debug == 2) fprintf(stderr,"...\n");
      else if (debug > 2) fprintf(stderr,"\n");
   }
   
   /* how about an index list ? */
   if (dset->dblk->nnodes && dset->dblk->node_list) {
      if (debug > 1) 
         fprintf(stderr,"Copying %d node indices\n", dset->dblk->nnodes);
      PROTECT(node_list = NEW_INTEGER(dset->dblk->nnodes));
      iv = INTEGER_POINTER(node_list);
      memcpy(iv, dset->dblk->node_list, dset->dblk->nnodes*sizeof(int));
      kparts = 3;
   } else {
      kparts = 2;
      if (debug > 1) 
         fprintf(stderr,"No node indices %d %p\n", 
                  dset->dblk->nnodes, dset->dblk->node_list);
   }
   
   /* done with dset, dump it */
   DSET_delete(dset);
   
   /* form output list */
   PROTECT(names = allocVector(STRSXP,kparts));
   for (i=0; i<kparts; ++i) {
      SET_STRING_ELT(names, i, mkChar(listels[i]));
   } 
   PROTECT(Rdset = allocVector(VECSXP,kparts));
   SET_VECTOR_ELT(Rdset, 0, head);
   SET_VECTOR_ELT(Rdset, 1, brik);
   if (node_list != R_NilValue) SET_VECTOR_ELT(Rdset, 2, node_list);
   setAttrib(Rdset, R_NamesSymbol, names);
   
   if (debug > 1) fprintf(stderr,"Unprotecting...\n");
   if (kparts==3) UNPROTECT(7);
   else UNPROTECT(6);
   
   return(Rdset);
}

SEXP R_THD_write_dset(SEXP Sfname, SEXP Sdset, SEXP Opts)
{
   SEXP Rdset, brik, head, names, opt, node_list;
   int i=0, ip=0, sb, cnt=0, scale = 1, overwrite=0, addFDR=0, 
       kparts=2, *iv=NULL;
   char *fname = NULL, *head_str, *stmp=NULL, *hist=NULL;
   NI_group *ngr=NULL;
   NI_element *nel=NULL;
   char *listels[3] = {"head","brk","index_list"}; /* the brk is on purpose 
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
   fname = R_alloc(strlen(CHAR(STRING_ELT(Sfname,0)))+1, sizeof(char));
   strcpy(fname, CHAR(STRING_ELT(Sfname,0)));
   if (debug >1) INFO_message("Output filename %s\n"
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
      if (debug > 1) INFO_message("overwrite is %d\n", overwrite); 	
      THD_force_ok_overwrite(overwrite) ;
      if (overwrite) THD_set_quiet_overwrite(1);
   }	
   if ((opt = getListElement(Opts,"addFDR")) != R_NilValue) {
	   addFDR = (int)INTEGER_VALUE(opt);
      if (debug > 1) INFO_message("addFDR is %d\n", addFDR); 	
   }
   
   PROTECT(opt = getListElement(Opts,"hist"));
   if ( opt != R_NilValue) {
	   opt = AS_CHARACTER(opt);
      hist = R_alloc(strlen(CHAR(STRING_ELT(opt,0)))+1, sizeof(char));
      strcpy(hist, CHAR(STRING_ELT(opt,0))); 
      if (debug > 1) INFO_message("hist is %s\n", hist); 	
   }
   UNPROTECT(1);
   
   for (ip=0,i=0; i<length(head); ++i) {
      head_str = (char *)CHAR(STRING_ELT(head,i));
      if (debug > 1) {
         INFO_message("Adding %s\n", head_str);
      }
      nel = NI_read_element_fromstring(head_str);
      if (!nel->vec) {
         ERROR_message("Empty attribute vector for\n%s\n"
                       "This is not expected.\n",
                       head_str);
         UNPROTECT(3);
         return(R_NilValue);
      }
      NI_add_to_group(ngr,nel);
   }
   
   if (debug > 1) INFO_message("Creating dset header\n");
   if (!(dset = THD_niml_to_dataset(ngr, 1))) {
      ERROR_message("Failed to create header");
      UNPROTECT(3);
      return(R_NilValue);
   }
   if (debug > 2) {
         INFO_message("Have header of %d, %d, %d, %d, scale=%d\n", 
                       DSET_NX(dset), DSET_NY(dset), 
                       DSET_NZ(dset), DSET_NVALS(dset), scale);
   }
   
   for (i=0; i<DSET_NVALS(dset); ++i) {
      if (debug > 2) {
         INFO_message("Putting values in sub-brick %d, type %d\n", 
                       i, DSET_BRICK_TYPE(dset,i));
      }
                            
      if (  ( DSET_BRICK_TYPE(dset,i) == MRI_byte || 
      	     DSET_BRICK_TYPE(dset,i) == MRI_short ) ) {
         EDIT_substscale_brick(dset, i, 
                            MRI_double, dv+i*DSET_NVOX(dset),
                            DSET_BRICK_TYPE(dset,i), scale ? -1.0:1.0);
      } else if ( DSET_BRICK_TYPE(dset,i) == MRI_double ) {
        EDIT_substitute_brick(dset, i, 
                            MRI_double, dv+i*DSET_NVOX(dset));
      } else if ( DSET_BRICK_TYPE(dset,i) == MRI_float ) {
        float *ff=(float*)calloc(DSET_NVOX(dset), sizeof(float));
        double *dvi=dv+i*DSET_NVOX(dset);
        for (ip=0; ip<DSET_NVOX(dset); ++ip) {
         ff[ip] = dvi[ip];
        }
        EDIT_substitute_brick(dset, i, MRI_float, ff);
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
            if (debug) 
               ININFO_message("created %d FDR curve%s in dataset header",
                              ip,(ip==1)?"\0":"s") ;
         } else {
            if (debug) 
               ININFO_message("failed to create FDR curves in dataset header") ;
         }
      }
   }
   
   /* Do we have an index_list? */
   if ((node_list=AS_INTEGER(getListElement(Rdset,"index_list")))!=R_NilValue) {
      iv = INTEGER_POINTER(node_list);
      if (debug > 1) INFO_message("First node index value %d, total (%d)\n", 
                                  iv[0], length(node_list));
      dset->dblk->nnodes = length(node_list);
      dset->dblk->node_list = (int *)XtMalloc(dset->dblk->nnodes * sizeof(int));
      memcpy(dset->dblk->node_list, iv, dset->dblk->nnodes*sizeof(int));
   }
   
   if (hist) {
      tross_Append_History(dset, hist);
   }
   
   DSET_write(dset); 
  
   UNPROTECT(3);
   return(R_NilValue);  
}

SEXP R_SUMA_ParseModifyName(SEXP Sfname, SEXP Swhat, SEXP Sval, SEXP Scwd) 
{
   SEXP Rname = R_NilValue;
   char *fname = NULL, *what=NULL, *val=NULL, *cwd=NULL, *res=NULL;
   int debug=0, nprot=0;
   
   if (!debug) debug = get_odebug();
   if (isNull(Sfname) || isNull(Swhat) || isNull(Sval)) {
      ERROR_message("Null input to R_SUMA_ModifyName");
      return(Rname);
   }
   /* get the filename */
   PROTECT(Sfname = AS_CHARACTER(Sfname)); ++nprot;
   fname = R_alloc(strlen(CHAR(STRING_ELT(Sfname,0)))+1, sizeof(char));
   strcpy(fname, CHAR(STRING_ELT(Sfname,0)));
   if (debug) INFO_message("filename %s\n", fname);
  
   /* get the what */
   PROTECT(Swhat = AS_CHARACTER(Swhat)); ++nprot;
   what = R_alloc(strlen(CHAR(STRING_ELT(Swhat,0)))+1, sizeof(char));
   strcpy(what, CHAR(STRING_ELT(Swhat,0)));
   if (debug) INFO_message("what %s\n", what);

   /* get the val */
   PROTECT(Sval = AS_CHARACTER(Sval)); ++nprot;
   val = R_alloc(strlen(CHAR(STRING_ELT(Sval,0)))+1, sizeof(char));
   strcpy(val, CHAR(STRING_ELT(Sval,0)));
   if (debug) INFO_message("val %s\n", val);

   /* get the cwd */
   if (!isNull(Scwd)) {
      PROTECT(Scwd = AS_CHARACTER(Scwd)); ++nprot;
      cwd = R_alloc(strlen(CHAR(STRING_ELT(Scwd,0)))+1, sizeof(char));
      strcpy(cwd, CHAR(STRING_ELT(Scwd,0)));
      if (debug) INFO_message("cwd %s\n", cwd);
   } 
   
   if (debug) INFO_message("Modifying %s\n", fname);
   if ((res = SUMA_ModifyName(fname, what, val, cwd))) {
      PROTECT(Rname = allocVector(STRSXP, 1)); ++nprot;
      SET_STRING_ELT(Rname, 0, mkChar(res)); 
      SUMA_free(res); res=NULL;
   } else {
      ERROR_message("Call to SUMA_ModifyName %s %s %s failed", fname, what, val);
   }
   UNPROTECT(nprot); 
   
   return(Rname);
}

SEXP R_SUMA_HistString (SEXP SCallingFunc, SEXP Sarg, SEXP Shold) {
   char *CallingFunc=NULL;
   SEXP Rname = R_NilValue;
   char *fname = NULL, *hold=NULL, **arg=NULL, *res=NULL;
   int debug=0, nprot=0, narg=0, i= 0;
   
   if (!debug) debug = get_odebug();
   if (isNull(SCallingFunc) || isNull(Sarg)) {
      ERROR_message("Null input to R_SUMA_HistString");
      return(Rname);
   }
   /* get the executable name */
   PROTECT(SCallingFunc = AS_CHARACTER(SCallingFunc)); ++nprot;
   fname = R_alloc(strlen(CHAR(STRING_ELT(SCallingFunc,0)))+1, sizeof(char));
   strcpy(fname, CHAR(STRING_ELT(SCallingFunc,0)));
   if (debug) INFO_message("filename %s\n", fname);
  
   /* get the arg */
   PROTECT(Sarg = AS_CHARACTER(Sarg)); ++nprot;
   narg = (LENGTH(Sarg));
   arg = (char **)calloc(narg+1, sizeof(char *));
   if (fname) arg[0] = strdup(fname);
   else arg[0] = strdup("UnChevalSansNom");
   for (i=1; i<=narg; ++i) {
      arg[i] = (char *)calloc(strlen(CHAR(STRING_ELT(Sarg,i-1)))+1, 
                              sizeof(char));
      strcpy(arg[i], CHAR(STRING_ELT(Sarg,i-1)));
      if (debug) INFO_message("arg %d/%d %s\t", i, narg, arg[i]);
   }
   
   /* any old history ? */
   if (!isNull(Shold)) {
      PROTECT(Shold = AS_CHARACTER(Shold)); ++nprot;
      hold = R_alloc(strlen(CHAR(STRING_ELT(Shold,0)))+1, sizeof(char));
      strcpy(hold, CHAR(STRING_ELT(Shold,0)));
      if (debug) INFO_message("hold %s\n", hold);
   }
    
   if (( res = SUMA_HistString (fname, narg+1, arg, hold))) {
      PROTECT(Rname = allocVector(STRSXP, 1)); ++nprot;
      SET_STRING_ELT(Rname, 0, mkChar(res)); 
      if (debug) INFO_message("hist is %s\n", res);
      SUMA_free(res); res=NULL;
   } else {
      ERROR_message("Call to SUMA_HistString %s failed", fname);
   }
   
   for (i=0; i<=narg; ++i) { if (arg[i]) free(arg[i]); } free(arg); arg=NULL;
   
   UNPROTECT(nprot); 
   return(Rname);
}
