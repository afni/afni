#include "SUMA_suma.h"
#include "../avovk/thd_segtools_fNM.h"
#include "SUMA_SegOpts.h"
#include "SUMA_SegFunc.h"

int SUMA_KeyofLabel_Dtable(Dtable *vl_dtable, char *label) {
   static char FuncName[]={"SUMA_KeyofLabel_Dtable"};
   int kk;
   char *str_key=NULL;
   if (!(str_key = findin_Dtable_b(label, vl_dtable))){
      SUMA_S_Errv("Could not find entry in label table for class %s\n",
                 label);
      SUMA_RETURN(-1);
   }
   kk = strtol(str_key,NULL, 10); 
   SUMA_RETURN(kk);
}

Dtable *SUMA_LabelsKeys2Dtable (NI_str_array *clss, int *keys)
{
   static char FuncName[]={"SUMA_LabelsKeys2Dtable"};
   char sval[256];
   int i;
   Dtable *vl_dtable=NULL;
   
   SUMA_ENTRY;
   
   /* make a labeltable */
   vl_dtable = new_Dtable(5);
   for (i=0; i<clss->num; ++i) {
      if (keys) sprintf(sval,"%d", keys[i]);
      else sprintf(sval,"%d", i+1);
      addto_Dtable( sval , clss->str[i] , vl_dtable ) ;
   }
   
   SUMA_RETURN(vl_dtable);
}

char *SUMA_LabelsKeys2labeltable_str(NI_str_array *clss, int *keys)
{
   static char FuncName[]={"SUMA_LabelsKeys2labeltable_str"};
   char *labeltable_str=NULL;
   Dtable *vl_dtable=SUMA_LabelsKeys2Dtable(clss, keys);
   
   SUMA_ENTRY;
   
   labeltable_str = Dtable_to_nimlstring(vl_dtable, 
                                             "VALUE_LABEL_DTABLE");
   destroy_Dtable(vl_dtable); vl_dtable=NULL;
   
   SUMA_RETURN(labeltable_str);
}

void SUMA_ShowClssKeys(NI_str_array *clss, int *keys)
{
   static char FuncName[]={"SUMA_ShowClssKeys"};
   int i;

   SUMA_ENTRY;

   for (i=0; i<clss->num; ++i) {
      if (keys) fprintf(SUMA_STDOUT, "  %s --> %d\n", clss->str[i], keys[i]);
      else fprintf(SUMA_STDOUT, "  %s --> %d (assumed)\n", clss->str[i], i+1);
   }  

   SUMA_RETURNe;
}

int get_train_pdist(SEG_OPTS *Opt, char *feat, char *cls, 
                     double *par, double *scpar) 
{
   char **clsv=NULL, **featv=NULL;
   float *shapev=NULL, *ratev=NULL;
   int i = 0;
   char *atr=NULL, atname[256]={""};
   NI_str_array *atrs=NULL;
   
   ENTRY("get_train_pdist");
   
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
   Estimate the probability of feature amplitude given its feature and  class
*/
int p_a_GIV_cvfu(SEG_OPTS *Opt, char *feat, char *cls, 
                  THD_3dim_dataset *pout) 
{
   double par[10], npar = 0, scpar[2];
   char fpref[256+IDCODE_LEN+32]={""};
   char fsave[256+IDCODE_LEN+32]={""};
   THD_3dim_dataset *pload=NULL;
   short *a=NULL, *p=NULL;
   int ia = 0, i=0;
   float af=0.0; 
   double dp=0.0, da=0.0, hbw = 0.0, pfhbw, pf=0.0;
   
   ENTRY("p_a_GIV_cvfu"); 
   
   if (!pout || DSET_BRICK_TYPE(pout,0) != MRI_short) RETURN(0);
   
   /* form the temp filename */
   if (Opt->UseTmp) {
      sprintf(fpref, "/tmp/%s.a_GIV_cvfu-%s-%s",
                  Opt->uid, feat, cls);
      sprintf(fsave, "%s+orig.HEAD", fpref);
   }
   if (Opt->UseTmp && (pload = THD_open_dataset( fsave ))) {
      if (Opt->debug > 1) INFO_message("Found %s", fsave);
      if (DSET_BRICK_TYPE(pload,0) != MRI_short) RETURN(0);
      DSET_mallocize(pload)   ; DSET_load(pload);
      /* swap column and factor */
      SWAP_COL(pload, pout,0)
      /* erase pload and get out */
      DSET_delete(pload);
      RETURN(1);
   } else {
      if (Opt->debug > 1) INFO_message("Must compute %s, %s", feat, cls);
      SB_LABEL(Opt->sig,feat, ia);
      if (ia<0) {
         ERROR_message("Failed to find %s", feat); RETURN(0);
      }
      if (get_train_pdist(Opt, feat, cls, par, scpar)!=2) {
         ERROR_message("Failed to get gamma params for %s, %s", feat, cls); 
         RETURN(0);
      }
      
      a = (short *)DSET_ARRAY(Opt->sig, ia);
      af = DSET_BRICK_FACTOR(Opt->sig, ia);
      if (!af) af = 1;
      
      p = (short *)DSET_ARRAY(pout, 0);
      pf = 32767.0; /* max p is 1, so stick with this */
      /* Now compute probs */
      af = af * scpar[0];
      hbw = Opt->binwidth / 2.0;
      pfhbw = pf * hbw ;
      for (i=0; i<DSET_NVOX(Opt->sig); ++i) {
         if (IN_MASK(Opt->cmask,i)) {
            da = (double)((a[i]*af)+scpar[1]);
            #if 0
            /* gold standard see area.gam*/
            dp = ( gamma_t2p( da-hbw, par[0] , par[1] ) -
                   gamma_t2p( da+hbw , par[0] , par[1] ) ) * pf;
            #else
            dp = (PDFGAM((da-hbw), par[0], par[1]) + 
                  PDFGAM((da+hbw), par[0], par[1]) ) *  pfhbw; 
            #endif
            if (i == Opt->VoxDbg) {
               fprintf(Opt->VoxDbgOut,"      a = %d, a_sc = %f\n"
                                     "p(a|c=%s,f=%s)=%f\n",
                                     a[i], da, cls, feat, dp/pf);
            }
         } else {
            if (i == Opt->VoxDbg) fprintf(Opt->VoxDbgOut," Vox Masked\n");
            dp = 0.0;
         }
         p[i] = (short)dp;
      }

      EDIT_BRICK_FACTOR(pout,0,1.0/pf);
      if (Opt->UseTmp) {
         if (Opt->debug > 1) INFO_message("Writing %s", fsave);
         UNIQ_idcode_fill(DSET_IDCODE_STR(pout));/* new id */
         EDIT_dset_items( pout, ADN_prefix, fpref, ADN_none );
         DSET_overwrite(pout) ;
      }
      RETURN(1);   
   }            
   
   RETURN(0);
}

/*!
Estimate the probability of a class, given a feature
*/
int p_cv_GIV_afu (SEG_OPTS *Opt, char *feat, 
                  char *cls, double *d) {
   static THD_3dim_dataset *pb=NULL;
   static double *dd=NULL;
   static long long init=0;
   int i,j;
   short *a=NULL;
   float af =0.0;
   double bb=0.0;
   
   ENTRY("p_cv_GIV_afu");
   
   if (cls==NULL) { 
      if (!init) {/* init */
         if (pb) { ERROR_message("Non null pb"); RETURN(0); }
         NEW_SHORTY(Opt->sig,1,"p_cv_GIV_afu",pb);
         if (!pb) RETURN(0);
         dd = (double *)calloc(DSET_NVOX(Opt->sig), sizeof(double));
         if (!dd) RETURN(0);
         init = 1;
      } else { /* clean */
         DSET_delete(pb); pb=NULL; 
         free(dd); dd=NULL; init=0;
      }
      RETURN(1);
   }
   
   if (!pb || init==0) { ERROR_message("Not initialized"); RETURN(0); }
   if (!d) { ERROR_message("NULL d"); RETURN(0); }
   
   memset(d, 0, DSET_NVOX(Opt->sig)*sizeof(double));
   for (i=0; i<Opt->clss->num;++i) {
      if (Opt->debug > 1) 
         INFO_message(" Calling p_a_GIV_cvfu %d/%d\n", i,Opt->clss->num); 
      if (!(p_a_GIV_cvfu(Opt, feat, Opt->clss->str[i],pb))) {
         ERROR_message("Failed in p_a_GIV_cvfu"); RETURN(0);
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
            fprintf(Opt->VoxDbgOut,"   p(c=%s|a,f=%s)=%f\n",
                                  cls, feat, d[j]);
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
      INFO_message("Writing %s", ff);
      for (j=0; j<DSET_NVOX(Opt->sig); ++j) 
         fprintf(fout,"%f\n",d[j]);
      fclose(fout);
   }
   ++init;
   RETURN(1);
}

/*!
   Estimate the probability of a class, given all features 
*/
int p_cv_GIV_A (SEG_OPTS *Opt, char *cls, double *dr) 
{
   double pf;
   static double *d=NULL;
   static int init=0;
   int i, j;
   
   ENTRY("p_cv_GIV_A");      

   
   if (cls==NULL) { 
      if (!init) {/* init */
         if (d) { ERROR_message("Non null d"); RETURN(0); }
         d = (double *)calloc(DSET_NVOX(Opt->sig), sizeof(double));
         if (!d) RETURN(0);
         if (!p_cv_GIV_afu (Opt, NULL, NULL, d) ) RETURN(0);
         init = 1;
      } else { /* clean */
         if (!p_cv_GIV_afu (Opt, NULL, NULL, d) ) RETURN(0);
         free(d); d=NULL; 
         init=0;
      }
      RETURN(1);
   }
   
   if (!dr) RETURN(0);
   if (init!=1) { ERROR_message("Not initialized"); RETURN(0); }
   
   memset(dr, 0, DSET_NVOX(Opt->sig)*sizeof(double));
   
   for (i=0; i<Opt->feats->num; ++i) {
      if (Opt->debug > 1)  
         INFO_message("Calling p_cv_GIV_afu %d/%d", i,Opt->feats->num);
      if (!(p_cv_GIV_afu(Opt, Opt->feats->str[i], cls, d))) {
         ERROR_message("Failed in p_cv_GIV_afu"); RETURN(0);
      }
      for (j=0; j<DSET_NVOX(Opt->sig); ++j) {
         if (IN_MASK(Opt->cmask, j)) {
            if (1) {
               if (d[j] > MINP) dr[j] = dr[j] + log(d[j]);
               else dr[j] = dr[j] + log(MINP); 
               /* if (!(j%1000)) 
                  fprintf(stderr,"dr %f exp(dr) %f \n",  dr[j], exp(dr[j])); */
            } else {
                dr[j] = dr[j] + log(d[j]);
            }
         } else {
            dr[j] = 0.0;
         }
      }
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
   RETURN(1);
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

/*!
   Estimate the probability of each class, given all features 
*/
THD_3dim_dataset *p_C_GIV_A (SEG_OPTS *Opt) {
   char bl[256]={""};
   int i,j, ii;
   double *d=NULL;
   THD_3dim_dataset *pout=NULL;
   short *p=NULL;
   float pf=0.0;
   
   ENTRY("p_C_GIV_A");
   
   /* init */
   d = (double *)calloc(DSET_NVOX(Opt->sig), sizeof(double));
   if (!d) RETURN(NULL);
   if (!p_cv_GIV_A (Opt, NULL, d)) RETURN(NULL);
   NEW_SHORTY(Opt->sig, Opt->clss->num, Opt->prefix, pout);
   if (!pout) RETURN(NULL);
   if( !THD_ok_overwrite() && THD_is_file( DSET_HEADNAME(pout) ) ){
      ERROR_exit("Output file %s already exists -- cannot continue!\n",
                  DSET_HEADNAME(pout) ) ;
   }

   /* process */
   for (i=0; i<Opt->clss->num; ++i) {
      if (Opt->debug > -1000)  
         INFO_message("Calling p_cv_GIV_A %d/%d", i,Opt->clss->num);
      if (!(p_cv_GIV_A (Opt, Opt->clss->str[i],d))) {
         ERROR_message("Failed in p_cv_GIV_A"); RETURN(NULL);
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
      ERROR_message("Failed in p_cv_GIV_A cleanup but will proceed");
   }
   free(d); d= NULL;
   
   RETURN(pout);
}

int LabelToGroupedIndex(char *cls_str, NI_str_array *group_clss)
{
   int mtch=0, j,  ng=0, jmatch=-1;
   
   ENTRY("LabelToGroupedKey");
   
   mtch = 0;
   for (j=0; j<group_clss->num; ++j) {
      ng = strlen(group_clss->str[j]);
      if (strlen(cls_str) >= ng) {
         if (!strcmp(cls_str, group_clss->str[j])) {
               /* ININFO_message("%s --> %s (%d)", 
                                 cls_str, group_clss->str[j], j); */
               jmatch = j;
               mtch += 1;
         } else if (!strncmp(cls_str, group_clss->str[j], 
                                    strlen(group_clss->str[j])) && 
                        ( cls_str[ng] == ',' ||
                          cls_str[ng] == '.' ||
                          cls_str[ng] == '-' ||
                          cls_str[ng] == '_') ) {
               /* ININFO_message("%s --> %s (%d)", 
                              cls_str, group_clss->str[j], j); */
               jmatch = j;
               mtch += 1;
         }   
      }
   }
   if (mtch == 0) {
      /* ININFO_message("Could not find match for %s\n", cls_str); */
      RETURN(-1);
   }
   if (mtch > 1) {
      /* ERROR_message("Found more than one match"); */
      RETURN(-mtch);
   }
   
   RETURN(jmatch);
}

int LabelToGroupedKey(char *cls_str, NI_str_array *group_clss, 
                      int *group_keys) {
   int j =    LabelToGroupedIndex(cls_str, group_clss );
   
   if (j<0) return(j);
   else return(group_keys[j]);               
}


int GroupLabelMapping (NI_str_array *clss , NI_str_array *grpclss, 
                        int *map, int verb) 
{
   int j, i;
   
   ENTRY("GroupLabelMapping");
   /* make sure you can map one to the other */
   for (i=0; i<clss->num; ++i) map[i] = -1;
   {
      for (i=0; i<clss->num; ++i) {
         j = LabelToGroupedIndex(clss->str[i], grpclss);
         if (j >= 0) { map[i] = j; }
         
      }
   }
   if (verb) {
      for (i=0; i<clss->num; ++i) {
         if (map[i]>=0) {
            fprintf(stderr,"%s --> %s\n", clss->str[i] , grpclss->str[map[i]]);
         } else {
            fprintf(stderr,"%s --> NO MATCH\n", clss->str[i]);
         }
      }
   }
   RETURN(1);
}

/*!
   Regroup classes.
*/
int Regroup_classes (SEG_OPTS *Opt, 
                     NI_str_array * group_clss,
                     int  * ugroup_keys,
                     THD_3dim_dataset *pset, 
                     THD_3dim_dataset *cset,
                     THD_3dim_dataset **gpsetp, 
                     THD_3dim_dataset **gcsetp) { 
   int i,c, v,gc, imax=0, dtable_key[1024], ckey=0, gkey = 0;
   short *p=NULL, *pg=NULL;
   double max=0.0;
   int igrp[1024], mapverb=0;
   int group_keys[1024];
   THD_3dim_dataset *gpset=NULL,*gcset=NULL;
   char sval[256], *group_labeltable_str=NULL;
   Dtable *vl_dtable=NULL;
   
   ENTRY("Regroup_classes");
   
   /* checks */
   if (group_clss==NULL) {
      ERROR_message("Regroup_classes Bad input %p \n", 
                  group_clss);
      RETURN(0);
   }
   
   mapverb=0;
   if (!cset && !pset) {
      mapverb = 1;
   } 
   
   /* figure out the mapping between one and the other */
   if (!GroupLabelMapping (Opt->clss, group_clss, igrp, mapverb)) {
      ERROR_message("Failed to group map");
      RETURN(0);
   }
   
   if (!cset && !pset) {
      /* just called to be sure conversion is OK */
      RETURN(1);
   } 
   
   if (!gcsetp || *gcsetp) {
      ERROR_message("You must send the address of a null pointer for gcsetp");
      RETURN(0);
   }
   if (gpsetp && *gpsetp) {
      ERROR_message("If you send gpsetp it must be the address to null pointer");
      RETURN(0);
   }
   
   if (pset && !gpsetp) {
      ERROR_message("Nothing to return grouped probs in");
      RETURN(0);
   }
   gcset = *gcsetp;
   NEW_SHORTY(Opt->cset, DSET_NVALS(Opt->cset), Opt->cgrefix, gcset);

   /* get the key of each group_clss, and form dtable */
   vl_dtable = new_Dtable(5);
   for (i=0; i<group_clss->num; ++i) {
      if (ugroup_keys) group_keys[i] = ugroup_keys[i];
      else group_keys[i] = i+1;
      sprintf(sval,"%d", group_keys[i]);
      addto_Dtable( sval , group_clss->str[i] , vl_dtable ) ;
   }   
   
   p = (short *)DSET_ARRAY(cset,0);
   pg = (short *)DSET_ARRAY(gcset,0);
   
   for (i=0; i<DSET_NVOX(cset); ++i) { pg[i] = p[i]; } /* init */
   for (c=0; c<Opt->clss->num; ++c) {
      ckey = Opt->keys[c];
      if ((gkey = LabelToGroupedKey(Opt->clss->str[c],
                                    group_clss,group_keys)) < 0) {
      
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
      double dv[Opt->clss->num], gdv[group_clss->num], sgdv;
      float bfi[Opt->clss->num];
      gpset = *gpsetp;
      NEW_SHORTY(Opt->pset, group_clss->num, Opt->pgrefix, gpset);
      GET_BFs(Opt->pset, bfi);
      for (c=1; c<Opt->clss->num; ++c) {
         if (bfi[c]!=bfi[0]) {
            ERROR_message("Mismatch in float factors ([%d]%f [0]%f)!"
                           "They should all be equal.",c,bfi[c], bfi[0]);
            RETURN(0);
         }
      } 
      for (v=0; v<DSET_NVOX(Opt->pset); ++v) {
         if (IN_MASK(Opt->cmask, v)) {
            GET_VEC_AT_VOX(Opt->pset, v, dv, bfi);
            sgdv=0.0;
            for (gc=0; gc<group_clss->num; ++gc) {
               gdv[gc] = 0.0; 
               for (c=0; c<Opt->clss->num; ++c) {
                  if (igrp[c] == gc) {
                     if (dv[c] > gdv[gc]) {
                        gdv[gc] = dv[c]; 
                     }
                  }
               }
               sgdv += gdv[gc];
            }
            if (sgdv) {
               for (gc=0; gc<group_clss->num; ++gc) {
                  gdv[gc] /= sgdv;
               }
            }
            /* can use the same factor from other dset, 
               all values between 0 and 1*/
            PUT_VEC_AT_VOX(gpset, v, gdv, bfi);
         }
      }
      
      PUT_BFs(gpset, bfi);   
      for (gc=0; gc<group_clss->num; ++gc) {
         sprintf(sval,"p.%s",group_clss->str[gc]);
         EDIT_BRICK_LABEL(gpset,gc,sval);
      }
      
      *gpsetp = gpset; gpset=NULL;
   }
   
   RETURN(1);
}


/*!
   Assign a class given likelihoods.
   Only tested if normalize_p was called 
*/
THD_3dim_dataset *assign_classes (SEG_OPTS *Opt, THD_3dim_dataset *pset) {
   int i,j, imax=0;
   double *d=NULL;
   THD_3dim_dataset *pout=NULL;
   short *p=NULL;
   double max=0.0;
   char *labeltable_str=NULL;
   
   ENTRY("assign_classes");
   
   /* checks */
   if (!pset || !Opt->keys || !Opt->rescale_p) {
      ERROR_exit("Bad input %p %p %d\n", pset, Opt->keys, Opt->rescale_p);
   }
   
   /* init */
   NEW_SHORTY(Opt->sig, 1, Opt->crefix, pout);
   if (!pout) RETURN(NULL);
   if( !THD_ok_overwrite() && THD_is_file( DSET_HEADNAME(pout) ) ){
      ERROR_exit("Output file %s already exists -- cannot continue!\n",
                  DSET_HEADNAME(pout) ) ;
   }

      
   /* process */
   p = (short *)DSET_ARRAY(pout,0);
   for (j=0; j<DSET_NVOX(pset); ++j) {
      if (IN_MASK(Opt->cmask, j)) {
         MAX_AT_VOX(pset, j, imax, max);
         p[j] = Opt->keys[imax];  
      } else {
         p[j]=0; 
      }
   }
   labeltable_str = SUMA_LabelsKeys2labeltable_str(Opt->clss, Opt->keys);
   EDIT_BRICK_LABEL(pout,0,"maxprob_labels");
   THD_set_string_atr( pout->dblk , "VALUE_LABEL_DTABLE" , labeltable_str );
   free(labeltable_str); labeltable_str=NULL;
   
   RETURN(pout);
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
   
   if (Opt->debug) {
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

/*!
   Estimate bias field.
*/
THD_3dim_dataset *estimate_bias_field_NoCross (SEG_OPTS *Opt, 
                                       THD_3dim_dataset *aset,
                                       THD_3dim_dataset *cset,
                                       THD_3dim_dataset *pset) {
   int i,j, k, ni, nij, l, o, N_d[256],
       dtable_key[1024], N_g[256], N_coef=0, N_mm = 0;
   THD_3dim_dataset *pout=NULL;
   short *c=NULL, *a=NULL;
   char *str_lab=NULL;
   byte *mm=NULL;
   NI_str_array *bc=NULL, *bsc=NULL;
   matrix X, XtXiXt;
   vector F, B;
   double *d=NULL, halfN[3], nX[3], M_v[256], M_d[256], M_dg=0.0, max=0.0;
   Dtable *vl_dtable=NULL;
   
   ENTRY("estimate_bias_field_NoCross");
   
   /* checks */
   if (!aset || !cset || !Opt->clss || !Opt->bias_classes) {
      ERROR_exit("Bad input %p %p %p %p\n", 
                  aset, cset, Opt->clss, Opt->bias_classes);
   }
   
   /* init */
   NEW_SHORTY(aset, 1, Opt->frefix, pout);
   if (!pout) RETURN(NULL);
   if( !THD_ok_overwrite() && THD_is_file( DSET_HEADNAME(pout) ) ){
      ERROR_exit("Output file %s already exists -- cannot continue!\n",
                  DSET_HEADNAME(pout) ) ;
   }
   
   /* How many groups of classes do we have? */
   vl_dtable = SUMA_LabelsKeys2Dtable(Opt->clss, Opt->keys);
   c = (short *)DSET_ARRAY(cset, 0);
   a = (short *)DSET_ARRAY(aset, 0);
   mm = (byte *)calloc (DSET_NVOX(cset), sizeof(byte));
   bc = NI_strict_decode_string_list(Opt->bias_classes,";");
   Opt->N_biasgroups = bc->num;
   for (i=0; i<Opt->N_biasgroups; ++i) {
      fprintf(stderr,"   Group %s %d/%d\n", bc->str[i], i+1, bc->num);
      /* which keys belong to this group ? */
      bsc = NI_strict_decode_string_list(bc->str[i],", ");
      for (j=0; j<bsc->num; ++j) {
         fprintf(stderr,"     Sub Group %s %d/%d\n", bsc->str[j], j+1, bsc->num);
         if ((dtable_key[j] = SUMA_KeyofLabel_Dtable(vl_dtable, bsc->str[j]))<0){
            ERROR_exit("Failed to find bias label %s in table", bsc->str[j]); 
         }  
      }
      NI_delete_str_array(bsc );
      /* mark the class group */
      for (j=0; j<bsc->num; ++j) {
         for (k=0; k<DSET_NVOX(aset);++k) {
            if (  c[k] == dtable_key[j] && 
                 (!Opt->cmask || Opt->cmask[k]) )  { mm[k] = i+1; ++N_mm; } 
         }
      }
   }
   NI_delete_str_array(bc );
   destroy_Dtable(vl_dtable); vl_dtable=NULL;
   /* store mask for debugging and some stats later on*/
   {
      short *am=NULL;
      NEW_SHORTY(aset, 1, "bias_estimate_groups", Opt->gset);
      am = (short *)DSET_ARRAY(Opt->gset,0);
      for (i=0; i<DSET_NVOX(aset);++i) {
         am[i]=mm[i];
      }
   }
   if (Opt->debug > 1) {
      DSET_overwrite(Opt->gset); 
   }
   
   /* calculate the mean for each group */
   if (!group_mean(Opt, aset, mm, Opt->pweight ? pset:NULL, 
                   Opt->N_biasgroups, M_v, 0)) {
      ERROR_exit("Could not calculate mean\n");
   }

   /* Now that we have the mask, time to build the matrices */
   N_coef = 3*(Opt->Lpo+1);
   matrix_initialize(&X); matrix_create(N_mm, N_coef, &X);
   matrix_initialize(&XtXiXt); matrix_create(N_mm, N_coef, &XtXiXt);
   vector_initialize(&F); vector_create(N_mm, &F);
   vector_initialize(&B); vector_create(N_coef, &B);
   
   /* Fill F and X */
      /* The division was by 2.0 so that coords go from -1 to 1 in volume. 
         But that caused strong artifacts on the edge of the volume so I 
         resorted to equivalent of padding */
   halfN[0] = DSET_NX(cset)/1.0; 
   halfN[1] = DSET_NY(cset)/1.0;
   halfN[2] = DSET_NY(cset)/1.0;
   ni  = DSET_NX(cset);
   nij = DSET_NX(cset)*DSET_NY(cset);
   k = 0;
   for (j=0; j<Opt->N_biasgroups; ++j) {
      for (i=0; i<DSET_NVOX(cset); ++i) {
         if (mm[i]==j+1) {
            F.elts[k] = a[i]/M_v[j]; /* anat signal, scaled by group average*/
            /* fill row of matrix */
            IND_1D_2_NORM_3D_index(i, nX, ni, nij, halfN);
            l = 0; o = 0;
            while (l < N_coef) {
               X.elts[k][l] = legendre(nX[0], o); ++l;
               X.elts[k][l] = legendre(nX[1], o); ++l;
               X.elts[k][l] = legendre(nX[2], o); ++l;
               ++o;
            }
            if (!(k%100000)) {
               int jj; 
               fprintf(stderr,"j=%d, k=%d/%d/\n", j, k, N_mm);
               for (jj=0; jj<N_coef; ++jj) {
                  fprintf(stderr," %f\t", X.elts[k][jj]);
               }
               fprintf(stderr," \n\n");
            }
            ++k;
         }
      }
   }
   
   /* solve */
   if (Opt->debug) INFO_message("Computing solution");
   matrix_psinv(X, NULL, &XtXiXt);
   vector_multiply(XtXiXt, F, &B); 
   
   /* Show the results */
   vector_print(B);
   
   /* create the output field */
   d = (double *)calloc(DSET_NVOX(pout), sizeof(double));
   for (i=0; i<Opt->N_biasgroups; ++i) { M_d[i]=0.0; N_d[i] = 0; }
   for (i=0; i<DSET_NVOX(pout); ++i) {
      IND_1D_2_NORM_3D_index(i, nX, ni, nij, halfN);
      l=0; o = 0; d[i] = 0.0;
      while (l < N_coef) {
         d[i] += (B.elts[l]*legendre(nX[0], o)); ++l;
         d[i] += (B.elts[l]*legendre(nX[1], o)); ++l;
         d[i] += (B.elts[l]*legendre(nX[2], o)); ++l;
         ++o;
      }
      if (mm[i]) { M_d[mm[i]-1] += d[i]; ++N_d[mm[i]-1]; }
   }
   M_dg = 0.0;
   for (i=0; i<Opt->N_biasgroups; ++i) { 
      M_dg += M_d[i]; 
      M_d[i] /= (float)N_d[i]; 
   }
   M_dg /= N_mm;  /* grand mean */
   
   if (Opt->debug) {
      INFO_message("Average field for all %d groups %f:", 
                     Opt->N_biasgroups, M_dg);
      for (i=0; i<Opt->N_biasgroups; ++i) 
               fprintf(stderr,"      %d --> %f\n", i+1, M_d[i]);
   }
   
   /* scale bias by mean to make output image be closer to input */
   for (i=0; i<DSET_NVOX(pout); ++i) d[i] /= M_dg;
      
   /* store */
   EDIT_substscale_brick(pout, 0, MRI_double, d, MRI_short, -1.0);
   EDIT_BRICK_LABEL(pout,0,"BiasField");

   /* clean */
   matrix_destroy(&X); 
   matrix_destroy(&XtXiXt); 
   vector_destroy(&F);
   vector_destroy(&B);
   

   RETURN(pout);
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
   float *dmv=NULL;
   short *c=NULL, *a=NULL;
   char *str_lab=NULL;
   byte *mm=NULL;
   NI_str_array *bc=NULL, *bsc=NULL;
   MRI_IMAGE *imout , *imin ;
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

   if (polorder < 0) polorder = Opt->Lpo;
   if (polorder < 0) {
      SUMA_S_Err("Failed to set polorder");
      SUMA_RETURN(NULL);
   }
   
   /* How many groups of classes do we have? */
   vl_dtable = SUMA_LabelsKeys2Dtable(Opt->clss, Opt->keys);
   c = (short *)DSET_ARRAY(cset, 0);
   a = (short *)DSET_ARRAY(aset, 0);
   mm = (byte *)calloc (DSET_NVOX(cset), sizeof(byte));
   bc = NI_strict_decode_string_list(Opt->bias_classes,";");
   Opt->N_biasgroups = bc->num;
   for (i=0; i<Opt->N_biasgroups; ++i) {
      fprintf(stderr,"   Group %s %d/%d\n", bc->str[i], i+1, bc->num);
      /* which keys belong to this group ? */
      bsc = NI_strict_decode_string_list(bc->str[i],", ");
      for (j=0; j<bsc->num; ++j) {
         fprintf(stderr,"     Sub Group %s %d/%d\n", bsc->str[j], j+1, bsc->num);
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
      DSET_overwrite(Opt->gset); 
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
      } 
   }

   /* do the fit */
   if (Opt->debug) INFO_message("Computing solution with mri_polyfit");         
   mri_polyfit_verb(Opt->debug) ;
   if (!(imout = mri_polyfit( imin , polorder , mm , 0.0 , Opt->fitmeth )))  {
      ERROR_exit("Failed to fit");
   }
   
   
   /* calculate average bias */
   dmv = MRI_FLOAT_PTR(imout) ;   
   M_dg = 0.0;
   for (i=0; i<DSET_NVOX(aset); ++i) { 
      if (mm[i]) M_dg += dmv[i]; 
   }
   M_dg /= (double)N_mm;  /* grand mean */
   
   
   /* cleanup  */
   mri_free(imin); imin = NULL;
   mri_free(imout); imout = NULL;
   
   /* scale bias by mean to make output image be closer to input */
   for (i=0; i<DSET_NVOX(aset); ++i) dmv[i] /= M_dg;
      
   /* store */
   EDIT_substscale_brick(pout, 0, MRI_float, dmv, MRI_short, -1.0);
   EDIT_BRICK_LABEL(pout,0,"BiasField");
   

   SUMA_RETURN(pout);
}


/*!
   Apply bias field.
*/
THD_3dim_dataset *SUMA_apply_bias_field (SEG_OPTS *Opt, 
                                       THD_3dim_dataset *aset,
                                       THD_3dim_dataset *fset,
                                       THD_3dim_dataset *pout) {
   static char FuncName[]={"SUMA_apply_bias_field"};
   int i;
   float *d=NULL;
   float bf = 1.0, bfa=1.0, bfb=1.0;
   short *b=NULL, *a=NULL;
   
   
   ENTRY("apply_bias_field");
   
   /* checks */
   if (!aset || !fset ) {
      ERROR_exit("Bad input %p %p \n", 
                  aset, fset);
   }
   
   /* init */
   if (!pout) {
      NEW_SHORTY(aset, 1, Opt->xrefix, pout);
   }
   if (!pout) RETURN(NULL);
   if( !THD_ok_overwrite() && THD_is_file( DSET_HEADNAME(pout) ) ){
      SUMA_S_Warnv("Output file %s already exists and not in overwrite mode!\n",
                  DSET_HEADNAME(pout) ) ;
   }
   
   /* apply the bias field */
   if (Opt->debug) INFO_message("Applying field");
   bfa = DSET_BRICK_FACTOR(aset,0); if (bfa == 0.0) bfa = 1.0;
   bfb = DSET_BRICK_FACTOR(fset,0); if (bfb == 0.0) bfb = 1.0; 
   b = (short *)DSET_ARRAY(fset,0);
   a = (short *)DSET_ARRAY(aset,0);
   d = (float *)calloc(DSET_NVOX(pout), sizeof(float));
   for (i=0; i<DSET_NVOX(pout); ++i) {
      bf = b[i]*bfb;
      if (bf > 0.5 && bf < 2.0) /* Extremists at edges cause mayhem 
                                     with division */
         d[i] = (float)a[i]/bf*bfa; 
      else d[i] = a[i]*bfa;
   }
   EDIT_substscale_brick(pout, 0, MRI_float, d, MRI_short, -1.0);
   EDIT_BRICK_LABEL(pout,0,"BiasCorrected");

   RETURN(pout);
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
                     byte *cmask, 
                     THD_3dim_dataset *wset,
                     SUMA_CLASS_STAT *cs) 
{
   static char FuncName[]={"SUMA_Class_stats"};
   int i=0, j = 0;   
   short *a=NULL, *c=NULL, *w=NULL;
   float af=1.0, wf=1.0;
   double n, Asum2, Asum, Amean, Astd, wsum;
    
   SUMA_ENTRY;
      
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0f) af = 1.0;
   if (wset) {
      wf = DSET_BRICK_FACTOR(wset,0); if (wf == 0.0f) wf = 1.0;
      w = (short *)DSET_ARRAY(wset,0);
   }
   a  = (short *)DSET_ARRAY(aset,0);
   c  = (short *)DSET_ARRAY(cset,0);
   
   if (!w) {
      for (j=0; j<cs->N_label; ++j) {
         n = 0;
         Asum2 = 0.0; Asum = 0.0; 
         for (i=0; i<DSET_NVOX(aset); ++i) {                
            if (IN_MASK(cmask,i) && c[i] == cs->keys[j]) {
               Asum2 += a[i]*a[i];
               Asum  += a[i];
               ++n; 
            }   
         }
         Astd = sqrt((Asum2-Asum*Asum/n)/(n-1))*af;
         Amean = Asum/n*af;
         SUMA_set_Stat(cs, cs->label[j], "mean", Amean);
         SUMA_set_Stat(cs, cs->label[j], "stdv", Astd);
      }
   } else {
      for (j=0; j<cs->N_label; ++j) {
         Asum2 = 0.0; Asum = 0.0; wsum = 0.0;
         for (i=0; i<DSET_NVOX(aset); ++i) {                
            if (IN_MASK(cmask,i) && c[i] == cs->keys[j]) {
               Asum  += w[i]*a[i]; wsum += w[i];
            }   
         }
         Amean = Asum/wsum;
         n = 0.0;
         Asum2 = 0.0; Asum = 0.0; wsum = 0.0;
         for (i=0; i<DSET_NVOX(aset); ++i) {                
            if (IN_MASK(cmask,i) && c[i] == cs->keys[j]) {
               n = (a[i]-Amean);
               Asum2  += w[i]*(n*n); wsum += w[i];
            }   
         }
         Astd = sqrt(Asum2/wsum)*af;
         Amean = Amean*af;
         SUMA_set_Stat(cs, cs->label[j], "mean", Amean);
         SUMA_set_Stat(cs, cs->label[j], "stdv", Astd);
      }
   }
   
   SUMA_RETURN(1);   
}

SUMA_CLASS_STAT *SUMA_New_Class_Stat(NI_str_array *clss, int *keys, 
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
      if (nP != 2) {
         SUMA_S_Errv("Can only handle 2 parameters (not %d) without names\n",
                     nP);
         SUMA_RETURN(NULL);
      }
   }
   cs->N_label = clss->num;
   cs->nP = nP;
   cs->label = (char **)SUMA_calloc(cs->N_label,sizeof(char *));
   cs->keys = (int *)SUMA_calloc(cs->N_label, sizeof(int));
   cs->P = (double *)SUMA_calloc(cs->nP*cs->N_label, sizeof(double));
   cs->pname = (char **)SUMA_calloc(cs->nP,sizeof(char *));
   for (i=0; i<clss->num; ++i) {
      cs->label[i] = SUMA_copy_string(clss->str[i]);
      if (keys) cs->keys[i] = keys[i];
      else cs->keys[i] = i+1;
   }
   for (i=0; i<cs->nP; ++i) {
      if (pnames) cs->pname[i] = SUMA_copy_string(pnames->str[i]); 
      else {
         switch(i) {
            case 0:
               cs->pname[i] = SUMA_copy_string("mean");
               break;
            case 1:
               cs->pname[i] = SUMA_copy_string("stdv");
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
         for (i=0; i<cs->nP; ++i) { SUMA_ifree(cs->pname[i]); }
      }
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
   static char FuncName[]={"SUMA_get_Stat"};
   int i=0,k=0;

   pp[0] = pp[1] = -1;

   for (i=0; i<cs->N_label; ++i) {
      if (!strcmp(cs->label[i], label)) {
         pp[0] = i;
         break;
      }
   }


   for (k=0; k<cs->nP; ++k) {
      if (!strcmp(cs->pname[k], pname)) {
         pp[1] = k;
         break;
      }
   }

   if (pp[0] < 0 || pp[1] < 0) SUMA_RETURN(0);

   SUMA_RETURN(1);   
}

double SUMA_get_Stat(SUMA_CLASS_STAT *cs, char *label, char *pname) 
{
   static char FuncName[]={"SUMA_get_Stat"};
   int pp[2];

   if (!SUMA_Stat_position(cs, label, pname, pp)) {
      SUMA_S_Errv("Failed to locate %s of %s\n",
                  pname, label);
      SUMA_RETURN(0.0); 
   }   

   SUMA_RETURN(cs->P[cs->nP*pp[0]+pp[1]]);
}

int SUMA_set_Stat(SUMA_CLASS_STAT *cs, char *label, char *pname, double val)      {
   static char FuncName[]={"SUMA_set_Stat"};
   int pp[2];

   if (!SUMA_Stat_position(cs, label, pname, pp)) {
      SUMA_S_Errv("Failed to locate %s of %s\n",
                  pname, label);
      SUMA_RETURN(0); 
   }
   cs->P[cs->nP*pp[0]+pp[1]] = val; 
   SUMA_RETURN(1);  
}

int SUMA_show_Class_Stat(SUMA_CLASS_STAT *cs, char *head) 
{
   static char FuncName[]={"SUMA_show_Class_Stat"};
   int i, j;

   if (head) fprintf(SUMA_STDOUT,"%s", head);
   for (i=0; i<cs->N_label; ++i) {
      fprintf(SUMA_STDOUT,"Class %s, key %d:\n", cs->label[i], cs->keys[i]);
      for (j=0; j<cs->nP; ++j) {
         fprintf(SUMA_STDOUT,"   %s = %f   \n", cs->pname[j], cs->P[cs->nP*i+j]);
      }
      fprintf(SUMA_STDOUT,"\n");
   }

   SUMA_RETURN(1);
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
   
   SUMA_RETURN(pout);
}

THD_3dim_dataset *SUMA_MAP_labels_old(THD_3dim_dataset *aset, 
                        byte *cmask, THD_3dim_dataset *pygcbo, 
                        SUMA_CLASS_STAT *cs, int neighopt, 
                        THD_3dim_dataset *cset, SEG_OPTS *Opt)
{
   static char FuncName[]={"SUMA_MAP_labels_old"};
   THD_3dim_dataset *pout = cset;
   float af=0.0, pf=0.0;
   int iter=0, i=0, k, ni, nj, nk, nij, ijkn[6];
   int Niter = 3;
   double lP[Niter], m, s, p1m, p2m, dd, *p1=NULL, *p2=NULL, sp1, sp2;
   short *ci=NULL, *co=NULL, *a=NULL, *pyg=NULL;
   
   SUMA_ENTRY;
   
   if (neighopt != 4 && neighopt != 6) {
      SUMA_S_Errv("Allowing neighopt of 4 or 6 only. Have %d\n", neighopt);
      SUMA_RETURN(NULL);
   } 
    
   if (!pout) {
      NEW_SHORTY(aset,1,"MAP_labels",pout);
   }
   
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0f) af = 1.0;
   a = (short *)DSET_ARRAY(aset,0);
   pyg = (short *)DSET_ARRAY(pygcbo,0); /* Don't think this is needed here ... */
   pf = DSET_BRICK_FACTOR(pygcbo,0); if (pf == 0.0f) pf = 1.0;
   co = (short *)DSET_ARRAY(cset,0);
   ci = (short *)malloc(sizeof(short)*DSET_NVOX(aset));
   p1 = (double *)calloc(cs->N_label, sizeof(double));
   p2 = (double *)calloc(cs->N_label, sizeof(double));
   
   ni = DSET_NX(aset);
   nj = DSET_NY(aset);
   nk = DSET_NZ(aset);
   nij = ni*nj;

   for (iter=0; iter<Niter; ++iter) {
      memcpy(ci, co, sizeof(short)*DSET_NVOX(aset));
      lP[iter] = 0.0;
      for (i=0; i<DSET_NVOX(aset); ++i) {
         if (IN_MASK(cmask,i)) {
            sp1 = 0.0; sp2 = 0.0;
            for(k=0; k<cs->N_label; ++k) {
               /* term 1, likelihood based on gaussians */
               m = cs->P[k*cs->nP]; /* mean */
               s = cs->P[k*cs->nP+1]; /* std */
               dd = ((double)a[i]*af-m); dd *= dd; 
               p1[k] = exp(-dd/(2.0*s*s) -log(s))/SQ2PI ; 
               sp1 += p1[k];
               /* term 2, likelihood based on neighbors */
               GET_NEIGHBS_IN_MASK( cmask, i, 
                                    ni, nj, nk, nij, 
                                    ijkn);
               /* calculate the field likelihood */
               P_l_GIV_NEIGHBS_NOTGOOD(ci, ijkn, cs->keys[k], neighopt, p2[k]);
               sp2 += p2[k];
            }
            /* Marginalize ps, get max and corresponding class */
            if (1) {
               p1m=0.0; p2m=0.0; 
               for(k=0; k<cs->N_label; ++k) { 
                  p1[k] /= sp1; p2[k] /= sp2; 
                  if (p1[k]+p2[k] > p1m+p2m) { 
                     p1m = p1[k]; p2m=p2[k]; co[i] = cs->keys[k]; 
                  } 
               }
            }
            if (i == Opt->VoxDbg) {
               fprintf(Opt->VoxDbgOut, "a=%d (%f)\n", a[i], a[i]*af);
               for(k=0; k<cs->N_label; ++k) {
                  fprintf(Opt->VoxDbgOut, "%d-->%d, %f, %f\n",
                        k, cs->keys[k],
                        cs->P[k*cs->nP], cs->P[k*cs->nP+1]);      
               }
               fprintf(Opt->VoxDbgOut, "p1:   ");
               for(k=0; k<cs->N_label; ++k) {
                  fprintf(Opt->VoxDbgOut, "%f   ", p1[k]);
               }
               fprintf(Opt->VoxDbgOut, "\n");
               fprintf(Opt->VoxDbgOut, "p2:   ");
               for(k=0; k<cs->N_label; ++k) {
                  fprintf(Opt->VoxDbgOut, "%f   ", p2[k]);
               }
               fprintf(Opt->VoxDbgOut, "\n");
            } 
            if (p1m+p2m > 0.002) lP[iter] += log((p1m+p2m)/2.0);
            else lP[iter] += log(0.001);
         } else {
            co[i] = 0;
         }
      }/* for i */
      SUMA_S_Notev("Iter %d, lP=%f\n", iter, lP[iter]);
   } /* for iter */
      
   SUMA_ifree(ci);   
   SUMA_RETURN(pout);
}

THD_3dim_dataset *SUMA_MAP_labels(THD_3dim_dataset *aset, 
                        byte *cmask, THD_3dim_dataset *pygcbo, 
                        SUMA_CLASS_STAT *cs, int neighopt, 
                        THD_3dim_dataset *cset, SEG_OPTS *Opt)
{
   static char FuncName[]={"SUMA_MAP_labels"};
   THD_3dim_dataset *pout = cset;
   float af=0.0, pf=0.0;
   int iter=0, i=0, k, ni, nj, nk, nij, ijkn[6];
   int Niter = 3, kmin;
   double eG[Niter], e, eG1, eG2, m, s, dd, *e1=NULL, *e2=NULL, BoT;
   short *ci=NULL, *co=NULL, *a=NULL, *pyg=NULL;
   
   SUMA_ENTRY;
   
   BoT = Opt->B/Opt->T;
   if (neighopt != 4 && neighopt != 6) {
      SUMA_S_Errv("Allowing neighopt of 4 or 6 only. Have %d\n", neighopt);
      SUMA_RETURN(NULL);
   } 
    
   if (!pout) {
      NEW_SHORTY(aset,1,"MAP_labels",pout);
   }
   
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0f) af = 1.0;
   a = (short *)DSET_ARRAY(aset,0);
   pyg = (short *)DSET_ARRAY(pygcbo,0); /* Don't think this is needed here ... */
   pf = DSET_BRICK_FACTOR(pygcbo,0); if (pf == 0.0f) pf = 1.0;
   co = (short *)DSET_ARRAY(cset,0);
   ci = (short *)malloc(sizeof(short)*DSET_NVOX(aset));
   e1 = (double *)calloc(cs->N_label, sizeof(double));
   e2 = (double *)calloc(cs->N_label, sizeof(double));
   
   ni = DSET_NX(aset);
   nj = DSET_NY(aset);
   nk = DSET_NZ(aset);
   nij = ni*nj;

   for (iter=0; iter<Niter; ++iter) {
      memcpy(ci, co, sizeof(short)*DSET_NVOX(aset));
      eG[iter] = 0.0; eG1=0.0; eG2 = 0.0;
      for (i=0; i<DSET_NVOX(aset); ++i) {
         if (IN_MASK(cmask,i)) {
            for(k=0; k<cs->N_label; ++k) {
               /* term 1, energy of y given class */
               m = cs->P[k*cs->nP];    /* mean */
               s = cs->P[k*cs->nP+1];  /* std  */
               dd = ((double)a[i]*af-m); dd *= dd; 
               e1[k] = (dd/(2.0*s*s) + log(s*SQ2PI ))/Opt->T; 

               /* term 2, energy of label given neighbors */
               GET_NEIGHBS_IN_MASK( cmask, i, 
                                    ni, nj, nk, nij, 
                                    ijkn);
               E_l_GIV_NEIGHBS(ci, ijkn, cs->keys[k], neighopt, e2[k], BoT);
            }
            /* find min e */
            if (1) {
               e = e1[0]+e2[0]; kmin=0; 
               for(k=1; k<cs->N_label; ++k) { 
                  if (e1[k]+e2[k] < e) { 
                     e=e1[k]+e2[k]; kmin=k; 
                  } 
               }
            }
            if (i == Opt->VoxDbg) {
               int IJK[3];
               Vox1D2Vox3D(i,DSET_NX(aset), DSET_NX(aset)*DSET_NY(aset), IJK);
               fprintf(Opt->VoxDbgOut, "at %d %d %d, a=%d (%f)\n", 
                                       IJK[0], IJK[1], IJK[2],
                                       a[i], a[i]*af);
               for(k=0; k<cs->N_label; ++k) {
                  fprintf(Opt->VoxDbgOut, "%d-->%d, %f, %f\n",
                        k, cs->keys[k],
                        cs->P[k*cs->nP], cs->P[k*cs->nP+1]);      
               }
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
                  fprintf(Opt->VoxDbgOut, "%f   ", e1[k]+e2[k]);
               }
               fprintf(Opt->VoxDbgOut, "\n");
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
         } else {
            co[i] = 0;
         }
      }/* for i */
      SUMA_S_Notev("Iter %d, e=%f\n", iter, eG[iter]);
   } /* for iter */
      
   SUMA_ifree(ci);   
   SUMA_RETURN(pout);
}

THD_3dim_dataset *SUMA_p_c_GIV_y(THD_3dim_dataset *aset, THD_3dim_dataset *cset, 
                                 byte *cmask, SUMA_CLASS_STAT *cs, int neighopt,
                                 THD_3dim_dataset *pcgy, SEG_OPTS *Opt)
{
   static char FuncName[]={"SUMA_p_c_GIV_y"};
   short *a=NULL, *c=NULL;
   double *p=NULL, *m=NULL, *s=NULL, *pygl, *pcgN, *gd, *ds2, 
            sp, BoT, x0, e;
   float af;
   int i, k, ni, nj, nk, nij, ijkn[6];
   THD_3dim_dataset *pout = pcgy;
   
   SUMA_ENTRY;
   
   if (!pout) {
      NEW_SHORTY(aset,1,"SUMA_p_c_GIV_y",pout);
   }
   
   BoT=Opt->B/Opt->T;

   ni = DSET_NX(aset);
   nj = DSET_NY(aset);
   nk = DSET_NZ(aset);
   nij = ni*nj;
   
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0f) af = 1.0;
   a = (short *)DSET_ARRAY(aset,0);
   c = (short *)DSET_ARRAY(cset,0);
   p = (double *)SUMA_calloc(DSET_NVOX(aset), sizeof(double));
   m = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   s = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   ds2 = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   gd = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   pygl = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   pcgN = (double *)SUMA_calloc(cs->N_label, sizeof(double));
    
   for (k=0; k<cs->N_label; ++k) {
      m[k]=SUMA_get_Stat(cs, cs->label[k], "mean");
      s[k]=SUMA_get_Stat(cs, cs->label[k], "stdv");
      ds2[k] = 2.0*s[k]*s[k];
      gd[k] = 1.0/(SQ2PI*s[k]); 
   }
      
   for (i=0; i<DSET_NVOX(aset); ++i) {
      p[i] = 0.0; sp = 0.0;
      for (k=0; k<cs->N_label; ++k) {
         if (IN_MASK(cmask, i)) {
            x0 = (double)a[i]*af - m[k];
            pygl[k] = exp(-(x0*x0)/ds2[k])*gd[k];
         }
         /* Now get p(c|neighb) */
         GET_NEIGHBS_IN_MASK( cmask, i, 
                              ni, nj, nk, nij, 
                              ijkn);
         E_l_GIV_NEIGHBS(c, ijkn, cs->keys[k], neighopt, e, BoT);
         pcgN[k]=exp(-e); 
         if (cs->keys[k] == c[i]) {
            p[i] = (pygl[k])*(pcgN[k]);
         }
         sp += (pygl[k])*(pcgN[k]);
      }
      p[i]/=sp;
            if (i == Opt->VoxDbg) {
               int IJK[3];
               Vox1D2Vox3D(i,DSET_NX(aset), DSET_NX(aset)*DSET_NY(aset), IJK);
               fprintf(Opt->VoxDbgOut, "at %d %d %d, a=%d (%f)\n", 
                                       IJK[0], IJK[1], IJK[2],
                                       a[i], a[i]*af);
               for(k=0; k<cs->N_label; ++k) {
                  fprintf(Opt->VoxDbgOut, "%d-->%s %d, %f, %f\n",
                        k, cs->label[k], cs->keys[k],
                        cs->P[k*cs->nP], cs->P[k*cs->nP+1]);      
               }
               fprintf(Opt->VoxDbgOut, "pygl[]:   ");
               for(k=0; k<cs->N_label; ++k) {
                  fprintf(Opt->VoxDbgOut, "%f   ", pygl[k]);
               }
               fprintf(Opt->VoxDbgOut, "\n");
               fprintf(Opt->VoxDbgOut, "pcgN[]:   ");
               for(k=0; k<cs->N_label; ++k) {
                  fprintf(Opt->VoxDbgOut, "%f   ", pcgN[k]);
               }
               fprintf(Opt->VoxDbgOut, "\n");
            } 
   }
   
   
   /* put vector back in pout */
   EDIT_substscale_brick(pout, 0, MRI_double, p, MRI_short, -1.0);
     
   SUMA_ifree(m);  
   SUMA_ifree(s);  
   SUMA_ifree(ds2);  
   SUMA_ifree(gd);  
   SUMA_ifree(pygl);  
   SUMA_ifree(pcgN);  
   
   SUMA_RETURN(pout);
}

THD_3dim_dataset *SUMA_SegEnhanceInitCset(THD_3dim_dataset *aseti, 
                                          THD_3dim_dataset *cset, 
                                 byte *cmask, int cmask_count, 
                                 SUMA_CLASS_STAT *cs,
                                 SEG_OPTS *Opt)
{
   static char FuncName[]={"SUMA_SegEnhanceInitCset"};   
   int ibias = 0, border;
   char sinf[256];
   THD_3dim_dataset *pout=cset, *aset=NULL, *fset=NULL, *xset=NULL;
   
   SUMA_ENTRY;
   
   if (!pout) {
      /* initialize randomly, and change oc.k and oc.r to reflect that */
      /* but for now, complain */
      SUMA_S_Err("Need cset");
      SUMA_RETURN(NULL);
   }
   
   /* preserve anatomy */
   aset = EDIT_full_copy(aseti, FuncName);
   
   /* pretreat initial classification, and get
      estimates of bias field and class parameters */ 
   for (ibias=0; ibias<Opt->N_ibias; ++ibias) {
      /* Improve class initializer with kmeans */
      if (Opt->clust_cset_init){ 
         OPT_KMEANS oc = new_kmeans_oc();
         oc.k = 3;
         oc.r = 0;
         oc.remap = NONE;
         oc.verb = 0;
         oc.distmetric = 'e';
         oc.jobname=SUMA_copy_string("3dseg");
         if (Opt->debug > 1) {
            SUMA_SEG_WRITE_DSET("cinit0", cset, ibias);
         }
         if (!(thd_Acluster1 (aset,
                     cmask, cmask_count,
                     &cset,
                     NULL,
                     cset,
                     oc))) {
            SUMA_S_Err("Failed to cluster");
            SUMA_RETURN(NULL);           
         }
         if (Opt->debug > 1) {
            SUMA_SEG_WRITE_DSET("cinit1", cset, ibias);
         }
      }
      
      border = (int)SUMA_MAX_PAIR(2,
                           (float)(ibias+1)/Opt->N_ibias*(float)Opt->Lpo);
      if (border > Opt->Lpo) border=Opt->Lpo;
      SUMA_S_Notev("Bias Field Model, iteration %d/%d Order %d", 
                      ibias+1, Opt->N_ibias, border);
      
      AFNI_FEED(Opt->ps->cs, "class init", ibias, cset);
      
      /* Get the bias field */
      {   
         if (!(fset = 
               SUMA_estimate_bias_field( Opt, border, 
                                    aset, cset, NULL,
                                    fset))) {
            SUMA_S_Err("Failed carelessly");
            SUMA_RETURN(NULL);
         }
      }

      /* Apply the bias field */
      {   
         if (!(xset = 
               SUMA_apply_bias_field(Opt, aset, fset,
                                xset))) {
            SUMA_S_Err("Failed hairlessly");
            SUMA_RETURN(NULL);
         }
      }
      
      bias_stats(Opt, aset, Opt->gset, xset, Opt->N_biasgroups);
      
      /* replace old anat with new anat */
      memcpy(DSET_ARRAY(aset,0), DSET_ARRAY(xset,0),
            DSET_NVOX(aset)*sizeof(short));
      EDIT_BRICK_FACTOR(aset,0, DSET_BRICK_FACTOR(xset, 0));
      
      AFNI_FEED(Opt->ps->cs, "bias corrected", ibias, aset);
      
      /* Get class stats at end of initialization */
      if (!SUMA_Class_stats( aset, cset, cmask, NULL, Opt->cs)) {
         SUMA_S_Err("Failed in class stats");
         SUMA_RETURN(NULL);
      }
      sprintf(sinf, "Class Stat During Seg Enhancement %d :\n", ibias);
      if (Opt->debug) SUMA_show_Class_Stat(Opt->cs, sinf);
      
   }
   
   if (Opt->debug > 1) {
      SUMA_SEG_WRITE_DSET("InitBiasCorrected", xset, -1);
      SUMA_SEG_WRITE_DSET("InitEnhancedClasses", cset, -1);
   }
   
   /* free temps */
   DSET_delete(xset); xset = NULL;
   DSET_delete(aset); aset = NULL;
   DSET_delete(fset); fset = NULL;
   
   SUMA_RETURN(cset); 
}

THD_3dim_dataset *SUMA_kmeans_bias_seg(THD_3dim_dataset *aset, 
                           NI_str_array kmeans_clss, THD_3dim_dataset *cset) {
   static char FuncName[]={"SUMA_kmeans_bias_seg"};
   
   if (!cset) {
      NEW_SHORTY(aset, 1, "kmeany", cset);
   }
   
   SUMA_RETURN(cset);                             
}
