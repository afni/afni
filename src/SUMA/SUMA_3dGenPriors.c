#define MAIN

#include "SUMA_suma.h"
#include "thd_segtools_fNM.h"
#include "SUMA_SegOpts.h"
#include "SUMA_SegFunc.h"
#include "matrix.h"

static int vn=0 ;

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}


int GenPriors(SEG_OPTS *Opt) 
{
   
   ENTRY("GenPriors");
   
   
   /* get the probability maps */
   if (!Opt->pset && Opt->DO_p) {
      if (!(Opt->pset = p_C_GIV_A(Opt))) {
         ERROR_message("Failed miserably");
         RETURN(0);
      }
   }
      
   /* Get the classes */
   if (!Opt->cset && Opt->crefix && Opt->DO_c) {
      if (!(SUMA_assign_classes_eng(Opt->pset, 
                           Opt->clss->str, Opt->clss->num, Opt->keys, 
                           Opt->cmask, &Opt->cset))) {
         ERROR_message("Failed aimlessly");
         RETURN(0);
      }
      EDIT_dset_items(Opt->cset, ADN_prefix, Opt->crefix, ADN_none);
      if( !THD_ok_overwrite() && THD_is_file( DSET_HEADNAME(Opt->cset) ) ){
      ERROR_exit("Output file %s already exists -- cannot continue!\n",
                  DSET_HEADNAME(Opt->cset) ) ;
      }
   }  
   
   /* group classes ? */
   if (Opt->group_classes) {
      THD_3dim_dataset *gcset=NULL;
      THD_3dim_dataset *gpset=NULL;
      if (!Regroup_classes (Opt, 
                     Opt->group_classes,
                     Opt->group_keys,
                     Opt->pset, 
                     Opt->cset,
                     &gpset, 
                     &gcset) ) {
         ERROR_message("Failed to regroup");
         RETURN(0);
      }
      DSET_write(gpset);
      DSET_write(gcset);
   }
          
   RETURN(1);
}

SEG_OPTS *GenPriors_Default(char *argv[], int argc) 
{
   SEG_OPTS *Opt=NULL;
   
   ENTRY("GenPriors_Default");
   
   Opt = SegOpt_Struct();
   
   Opt->helpfunc = &GenPriors_usage;
   Opt->ps = SUMA_Parse_IO_Args(argc, argv, "-talk;");
   Opt->aset_name = NULL;
   Opt->mset_name = NULL;
   Opt->sig_name = NULL;
   Opt->this_pset_name = NULL;
   Opt->this_cset_name = NULL;
   Opt->ndist_name = NULL;
   Opt->uid[0] = '\0';
   Opt->prefix = NULL;
   Opt->aset = NULL;
   Opt->mset = NULL;
   Opt->gset = NULL;
   Opt->sig = NULL;
   Opt->ndist = NULL;
   Opt->pset = NULL;
   Opt->cset = NULL;
   Opt->debug = 0;
   Opt->idbg = Opt->kdbg = Opt->jdbg = -1;
   Opt->binwidth = 0.01; /* the R function area.gam was used to pick a decent 
                            binwidth. I picked a large one where discrepancy
                            between Reference and Approximation was good. 
                            0.1 is too coarse, 0.001 is overkill*/ 
   Opt->feats=Opt->clss=NULL;
   Opt->keys = NULL;
   Opt->mixfrac=NULL;
   Opt->UseTmp = 1; 
   Opt->logp = 1;
   Opt->VoxDbg = -1;
   Opt->VoxDbgOut = stdout;
   Opt->rescale_p = 1;
   Opt->openmp = 0;
   Opt->labeltable_name = NULL;
   Opt->smode = STORAGE_BY_BRICK;
   Opt->pweight = 1;
   Opt->cmask = NULL;
   Opt->dimcmask = 0;
   Opt->cmask_count=0;
   Opt->mask_bot = 1.0;
   Opt->mask_top = -1.0;
   Opt->DO_p = TRUE;
   Opt->DO_c = TRUE;
   Opt->DO_r = FALSE;
   Opt->group_classes = NULL;
   Opt->group_keys = NULL;
   Opt->fitmeth = SEG_LSQFIT;
   
   RETURN(Opt);
}

int GenPriors_CheckOpts(SEG_OPTS *Opt) 
{
   ENTRY("GenPriors_CheckOpts");
   if (  !Opt->sig_name || 
          !Opt->ndist_name  ) {
      ERROR_message("Missing input");
      RETURN(0);
   }
   
   if (Opt->group_keys && !Opt->group_classes) {
      ERROR_message("Keys but no classes");
      RETURN(0);
   }
   RETURN(1);
}

int main(int argc, char **argv)
{
   static char FuncName[]={"3dGenPriors"};
   SEG_OPTS *Opt=NULL;
   char *atr=NULL;
   int i=0;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;
   
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   Opt = GenPriors_Default(argv,  argc);
   Opt = Seg_ParseInput (Opt, argv,  argc);
   
   if (!GenPriors_CheckOpts(Opt)) {
      ERROR_exit("Failed on option check");
   }
   
   /* load the input data */   
   if (Opt->sig_name && !(Opt->sig = Seg_load_dset( Opt->sig_name ))) {      
      exit(1);
   }
   
   /* Fix VoxDbg */
   if (Opt->VoxDbg >= 0) {
      Vox1D2Vox3D(Opt->VoxDbg, 
                  DSET_NX(Opt->sig), DSET_NX(Opt->sig)*DSET_NY(Opt->sig),
                  Opt->VoxDbg3);
   } else if (Opt->VoxDbg3[0]>=0) {
      Opt->VoxDbg = Opt->VoxDbg3[0] + Opt->VoxDbg3[1]*DSET_NX(Opt->sig) +
                        Opt->VoxDbg3[2]*DSET_NX(Opt->sig)*DSET_NY(Opt->sig);
   }

   if (Opt->ndist_name) {
      Opt->ndist = (NI_element*) Seg_NI_read_file(Opt->ndist_name);
      if( !Opt->ndist || strcmp(Opt->ndist->name,"TRAIN_DISTS")){
        fprintf(stderr,"**ERROR: can't open  %s, or bad type\n",
                        Opt->ndist_name) ;
        exit(1);
      }
      /* create the feature set and the class set */
      if ((atr = NI_get_attribute(Opt->ndist,"FeatureSet"))) {
         Opt->feats = NI_decode_string_list(atr,";,") ;
         if( Opt->feats == NULL || Opt->feats->num != DSET_NVALS(Opt->sig)) {
            ERROR_message("FeatureSet attribute invalid") ;
            exit(1);
         }
      }
      if ((atr = NI_get_attribute(Opt->ndist,"ClassSet"))) {
         Opt->clss = NI_decode_string_list(atr,";,") ;
         if( Opt->clss == NULL ) {
            ERROR_message("ClassSet attribute invalid") ;
            exit(1);
         }
      }
      
      /* make sure we have mixfrac */
      if (!Opt->mixfrac) {
         SUMA_S_Note("Using Equal Mixing Fractions");
         Opt->mixfrac = (float*)calloc(Opt->clss->num, sizeof(float));
         for (i=0; i<Opt->clss->num; ++i) {
            Opt->mixfrac[i] = 1/(float)Opt->clss->num;
         }
      } else {
         /* force one for the moment */
         ERROR_exit("Cannot handle user mixfrac yet");
      }
   }
   
   if (Opt->mset_name) {
      if (!(Opt->mset = Seg_load_dset( Opt->mset_name ))) {      
         exit(1);
      }
   }

   if (Opt->this_pset_name) {
      SUMA_S_Notev("Reading pre-existing %s\n",Opt->this_pset_name);
      if (!(Opt->pset = Seg_load_dset( Opt->this_pset_name ))) {      
         exit(1);
      }
   }
   
   if (Opt->this_cset_name) {
      SUMA_S_Notev("Reading pre-existing %s\n",Opt->this_cset_name);
      if (!(Opt->cset = Seg_load_dset( Opt->this_cset_name ))) {      
         exit(1);
      }
      #if 0
         /* you should not attempt to get clss from the cset
         because that guy might have a labeltable that contains more
         classes than in the training file. */
      if (!Opt->clss) { /* try to get it from this dset */
         if (!Seg_ClssAndKeys_from_dset(Opt->cset, &(Opt->clss), 
                                          &(Opt->clss_keys))) {
            ERROR_message("No clss to be found in cset.\n"
                          "Must specify labeltable");
            exit(1);
         }
      }
      #endif
   }


   if (!Opt->clss) {
      ERROR_message("Must specify -tdist");
      exit(1);
   }
   
   /* labeltable? */
   if (Opt->labeltable_name) {
      Dtable *vl_dtable=NULL;
      char *labeltable_str=NULL;
      int kk=0;
      
      /* read the table */
      if (!(labeltable_str = AFNI_suck_file( Opt->labeltable_name))) {
         ERROR_exit("Failed to read %s", Opt->labeltable_name);
      }
      if (!(vl_dtable = Dtable_from_nimlstring(labeltable_str))) {
         ERROR_exit("Could not parse labeltable");
      }
      /* make sure all classes are in the labeltable */
      for (i=0; i<Opt->clss->num; ++i) {
         if ((kk = SUMA_KeyofLabel_Dtable(vl_dtable, Opt->clss->str[i]))<0){
            ERROR_exit("Key not found in %s for %s ", 
                        Opt->labeltable_name, Opt->clss->str[i]);
         }
         if (Opt->keys) {
            if (Opt->keys[i]!=kk) {
               ERROR_exit("Key mismatch %d %d", Opt->keys[i], kk);
            }
         }   
      }   
      if (!Opt->keys) { /* get them from table */
         Opt->keys = (int *)calloc(Opt->clss->num, sizeof(int));
         for (i=0; i<Opt->clss->num; ++i) {
            if ((kk = SUMA_KeyofLabel_Dtable(vl_dtable, Opt->clss->str[i]))<0){
               ERROR_exit("(should noy happen) Key not found in %s for %s ", 
                           Opt->labeltable_name, Opt->clss->str[i]);
            }
            Opt->keys[i] = kk;
         }
      }
      destroy_Dtable(vl_dtable); vl_dtable=NULL;
   } 
   
   if (!Opt->keys) {
      /* add default keys */
      SUMA_S_Note("Keys not available, assuming defaults");
      Opt->keys = (int *)calloc(Opt->clss->num, sizeof(int));
      for (i=0; i<Opt->clss->num; ++i) {
         Opt->keys[i] = i+1;
      }
   }
   
   /* Show the match between keys and classes */
   SUMA_ShowClssKeys(Opt->clss->str, Opt->clss->num, Opt->keys);
   
   
   
   if (Opt->group_classes) { /* just a check */
      if (!Regroup_classes (Opt, Opt->group_classes, NULL,
                     NULL, NULL, 
                     NULL, NULL)) {
         ERROR_exit("Failed to determine mapping");
      }
   }
   
   
   if (Opt->sig) {
      Opt->cmask = MaskSetup(Opt, Opt->sig, 
                &(Opt->mset), &(Opt->cmask), Opt->dimcmask, 
                Opt->mask_bot, Opt->mask_top, &(Opt->cmask_count));
   }
   
   
   if (Opt->VoxDbg >= 0) {
      fprintf(Opt->VoxDbgOut, "Command:");
      for (i=0; i<argc; ++i) {
         fprintf(Opt->VoxDbgOut, "%s ", argv[i]);
      }
      fprintf(Opt->VoxDbgOut, "\nDebug info for voxel %d\n", Opt->VoxDbg);
   }
   
   /* An inportant sanity check */
   if (Opt->pset && Opt->clss->num != DSET_NVALS(Opt->pset)) {
      ERROR_exit( "Number of classes %d does not "
                  "match number of pset subricks %d\n",
                  Opt->clss->num , DSET_NVALS(Opt->pset));
   }
   
   if (!GenPriors(Opt)) {
      ERROR_exit("Failed in GenPriors");
   }
   
   /* write output */
   if (Opt->pset && !Opt->this_pset_name) {
      DSET_write(Opt->pset);
   }
   if (Opt->cset && !Opt->this_cset_name) {
      DSET_write(Opt->cset);
   }
   
   /* all done, free */
   Opt = free_SegOpts(Opt);
   
   PRINT_COMPILE_DATE ; exit(0);
}
