#include "SUMA_suma.h"
#include "../avovk/thd_segtools_fNM.h"
#include "SUMA_SegOpts.h"
#include "SUMA_SegFunc.h"

#ifdef USE_OMP
#include <omp.h>
#endif

static int VN = 0;

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[VN%10] ) ;
   if( VN%10 == 9) fprintf(stderr,".") ;
   VN++ ;
}


int SUMA_KeyofLabel_Dtable(Dtable *vl_dtable, char *label) {
   static char FuncName[]={"SUMA_KeyofLabel_Dtable"};
   int kk;
   char *str_key=NULL;
   
   SUMA_ENTRY;
   if (!(str_key = findin_Dtable_b(label, vl_dtable))){
      SUMA_S_Errv("Could not find entry in label table for class %s\n",
                 label);
      SUMA_RETURN(-1);
   }
   kk = strtol(str_key,NULL, 10); 
   SUMA_RETURN(kk);
}

Dtable *SUMA_LabelsKeys2Dtable (char **str, int num, int *keys)
{
   static char FuncName[]={"SUMA_LabelsKeys2Dtable"};
   char sval[256];
   int i;
   Dtable *vl_dtable=NULL;
   
   SUMA_ENTRY;
   
   /* make a labeltable */
   vl_dtable = new_Dtable(5);
   for (i=0; i<num; ++i) {
      if (keys) sprintf(sval,"%d", keys[i]);
      else sprintf(sval,"%d", i+1);
      addto_Dtable( sval , str[i] , vl_dtable ) ;
   }
   
   SUMA_RETURN(vl_dtable);
}

char *SUMA_LabelsKeys2labeltable_str(char **str, int num, int *keys)
{
   static char FuncName[]={"SUMA_LabelsKeys2labeltable_str"};
   char *labeltable_str=NULL;
   Dtable *vl_dtable=SUMA_LabelsKeys2Dtable(str, num, keys);
   
   SUMA_ENTRY;
   
   labeltable_str = Dtable_to_nimlstring(vl_dtable, 
                                             "VALUE_LABEL_DTABLE");
   destroy_Dtable(vl_dtable); vl_dtable=NULL;
   
   SUMA_RETURN(labeltable_str);
}

void SUMA_ShowClssKeys(char **str, int num, int *keys)
{
   static char FuncName[]={"SUMA_ShowClssKeys"};
   int i;

   SUMA_ENTRY;

   for (i=0; i<num; ++i) {
      if (keys) fprintf(SUMA_STDOUT, "  %s --> %d\n", str[i], keys[i]);
      else fprintf(SUMA_STDOUT, "  %s --> %d (assumed)\n", str[i], i+1);
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
         DSET_quiet_overwrite(pout) ;
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

int SUMA_assign_classes (THD_3dim_dataset *pset, 
                         SUMA_CLASS_STAT *cs,
                         byte *cmask,
                         THD_3dim_dataset **csetp) 
{
   return(SUMA_assign_classes_eng (pset, cs->label, cs->N_label,
                                   cs->keys, cmask, csetp));
}
/*!
   Assign a class given likelihoods.
   Only tested if normalize_p was called 
*/
int SUMA_assign_classes_eng (THD_3dim_dataset *pset, 
                         char **label, int N_label, int *keys,
                         byte *cmask,
                         THD_3dim_dataset **csetp)
{
   static char FuncName[]={"SUMA_assign_classes_eng"};
   int i,j, imax=0, uneven;
   double *d=NULL;
   THD_3dim_dataset *cset=*csetp;
   short *p=NULL;
   double max=0.0;
   char *labeltable_str=NULL;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   /* checks */
   if (!pset || !keys ) {
      SUMA_S_Errv("Bad input %p %p\n", pset, keys);
      SUMA_RETURN(0);
   }
   
   /* init */
   if (!cset) {
      NEW_SHORTY(pset, 1, "assign_classes", cset);
      *csetp = cset;
   }
   
   if (!cset) RETURN(0);

   /* make sure all factors are the same */
   max = DSET_BRICK_FACTOR(pset,0);
   uneven = 0;
   for (j=1; j<DSET_NVALS(pset); ++j) {
      if (max != DSET_BRICK_FACTOR(pset,j)) {
         uneven=1; break;
      }
   }   
   /* process */
   p = (short *)DSET_ARRAY(cset,0);
   if (!uneven) {
      for (j=0; j<DSET_NVOX(pset); ++j) {
         if (IN_MASK(cmask, j)) {
            MAX_AT_VOX(pset, j, imax, max);
            p[j] = keys[imax];  
         } else {
            p[j]=0; 
         }
      }
   }else {
      for (j=0; j<DSET_NVOX(pset); ++j) {
         if (IN_MASK(cmask, j)) {
            MAX_SC_AT_VOX(pset, j, imax, max);
            p[j] = keys[imax];  
         } else {
            p[j]=0; 
         }
      }
   }
   labeltable_str = SUMA_LabelsKeys2labeltable_str(label, N_label, 
                                                   keys);
   EDIT_BRICK_LABEL(cset,0,"maxprob_labels");
   THD_set_string_atr( cset->dblk , "VALUE_LABEL_DTABLE" , labeltable_str );
   free(labeltable_str); labeltable_str=NULL;
   
   SUMA_RETURN(1);
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
   
   if (Opt->debug > 1) {
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

 
int SUMA_ijk_to1 (int i, int j, int k, int ni, int nij) {
   return(i+j*ni+k*nij);
}
int SUMA_jik_to1 (int j, int i, int k, int ni, int nij) {
   return(i+j*ni+k*nij);
}
int SUMA_kji_to1 (int k, int j, int i, int ni, int nij) {
   return(i+j*ni+k*nij);
}

/*! Find and fill holes in volume.
The holes are defined as zero voxels that are
surrounded with non-zero ones in at least one of the 
three directions. 
This function is relatively slow, use SUMA_mri_volume_infill_zoom instead.
*/
int SUMA_mri_volume_infill(MRI_IMAGE *imin) 
{
   static char FuncName[]={"SUMA_mri_volume_infill"};
   int Ni, Nj, Nk, Nij, Nijk, i, j, k, v, iter;
   int d, d1b, d1e, d1, d2b, d2e, d2, d3b, d3e, d3;
   int Nfills, trg, cnd;
   float cndval, aprev;
   byte *ba=NULL;
   float *fa=NULL, *sum=NULL;
   byte *hits = NULL;
   int minhits = 0; 
   int maxiter=500;
   int (*I3_to_1)(int, int, int, int, int);
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   Ni = imin->nx; Nj = imin->ny; Nk = imin->nz; 
   Nij = Ni*Nj; Nijk = Nij*Nk;
   
   fa = MRI_FLOAT_PTR(imin);
   
   /* make byte mask */
   ba = (byte *)SUMA_calloc(Nijk, sizeof(byte));
   for (v=0; v<Nijk; ++v) {
      if (SUMA_ABS(fa[v]-0.0f)>0.00001) ba[v] = 1; 
   }
   sum = (float *)SUMA_calloc(Nijk, sizeof(float));
   hits = (byte *)SUMA_calloc(Nijk, sizeof(byte));
   
   iter = 0;
   do {
      for (d=0; d<6; ++d) { /* for each direction */
         switch (d) {
            case 0:
               d1b =  0;  d1e = Ni;  d1 = +1;
               d2b =  0;  d2e = Nj;  d2 = +1;
               d3b =  0;  d3e = Nk;  d3 = +1;
               I3_to_1 = &SUMA_ijk_to1;
               break;
            case 1:
               d1b = Ni;  d1e =  0;  d1 = -1;
               d2b =  0;  d2e = Nj;  d2 = +1;
               d3b =  0;  d3e = Nk;  d3 = +1;
               I3_to_1 = &SUMA_ijk_to1;
               break;
            case 2:
               d1b =  0;  d1e = Nj;  d1 = +1;
               d2b =  0;  d2e = Ni;  d2 = +1;
               d3b =  0;  d3e = Nk;  d3 = +1;
               I3_to_1 = &SUMA_jik_to1;
               break;
            case 3:
               d1b = Nj;  d1e =  0;  d1 = -1;
               d2b =  0;  d2e = Ni;  d2 = +1;
               d3b =  0;  d3e = Nk;  d3 = +1;
               I3_to_1 = &SUMA_jik_to1;
               break;
            case 4:
               d1b =  0;  d1e = Nk;  d1 = +1;
               d2b =  0;  d2e = Nj;  d2 = +1;
               d3b =  0;  d3e = Ni;  d3 = +1;
               I3_to_1 = &SUMA_kji_to1;
               break;
            case 5:
               d1b = Nk;  d1e =  0;  d1 = -1;
               d2b =  0;  d2e = Nj;  d2 = +1;
               d3b =  0;  d3e = Ni;  d3 = +1;
               I3_to_1 = &SUMA_kji_to1;
               break;
            default:
               SUMA_S_Err("palinmoment");
               SUMA_RETURN(0);

         }
         for (k=d3b; k<d3e; k = k+d3) {
            for (j=d2b; j<d2e; j = j+d2) {
               trg = -1; cnd = -1; cndval = 0; aprev = 0; 
               for (i=d1b; i<d1e; i = i+d1) {
                  v = I3_to_1(i,j,k, Ni, Nij);
                  if (trg < 0 && ba[v]) { 
                     trg = 0; /* you're in, target holes*/ 
                  } else if (trg == 0 && !ba[v]) {
                     /* found a candidate on the edge */
                     if (cnd >= 0) { SUMA_S_Err("SHOULD NOT BE"); };
                     cnd = v; cndval = aprev; trg = 1; /* next seek non zero */
                  } else if (trg == 1 && ba[v]) {
                     if (cnd >= 0) { /* have candidate with data after it */
                        sum[cnd] += cndval; /* add candidate value */
                        hits[cnd] += 1;
                     }
                     cnd = -1; cndval = 0; 
                     trg = 0; /* next we seek another hole */  
                  }
                  aprev = fa[v];  
               }
            }
         }
        
      }/* for each direction */
      /* Now put the candidates back into fa */
      Nfills = 0;
      for (v=0; v<Nijk; ++v) {
         if (hits[v]) {
               /* the next three statements used to be conditioned
               on hits[v] >= minhits, but minhits can not be used
               to control which kind of holes get filled. So it 
               is kind of useless */
               fa[v] = sum[v]/(float)hits[v]; 
               ba[v] = 1;
               ++Nfills;
            /* reset for next loop */
            sum[v] = 0.0; hits[v] = 0;
         }
      }
      ++iter;
      SUMA_LHv("Iter %d, N_fills  = %d\n", iter, Nfills);
   } while (Nfills > 0 && iter < maxiter);
   
   if (Nfills > 0) {
      SUMA_S_Warnv("Function stopped because of maximum iter limit of %d. "
                   "Holes may still exist.", maxiter);
   }
   SUMA_ifree(hits); SUMA_ifree(sum); SUMA_ifree(ba);      
   
   SUMA_RETURN(1);
}

/*!   
   Trace rays from voxel n1D in +i and -i directions.
   Return the first non zero value (ta) encountered and the number of
   voxels traveled (da) to get to it. 
   Results are stored in ta[0] and da[0] for +ve direction
   and ta[1] and da[1] for -ve direction.
*/  
int SUMA_ray_i(int n1D, int ni, int nij, float *av, byte *ba, 
               float ta[], int da[])
{
   static char FuncName[]={"SUMA_ray_i"};
   int IJK[3], ii, t1D;
   int hitcode;
   
   SUMA_ENTRY;
   
   ta[0] = ta[1] = 0.0;
   da[0] = da[1] = 0;
   hitcode = 0;
   
   Vox1D2Vox3D(n1D, ni, nij, IJK) 
   
   /* shoot ray in +ve direction */
   ii = IJK[0]; t1D = n1D;
   if (ii < ni && !ba[t1D]) {
      do {
         ++ii; ++t1D;
      } while (ii < ni && !ba[t1D]);  
      if (ii < ni) {
         ta[0] = av[t1D];  /* The value at the non zero in +ve i*/
         da[0] = ii-IJK[0];/* The number of voxels needed to get there*/
         hitcode = hitcode + SUMA_IposBOUND;
      }
   }
   
   /* shoot ray in -ve direction */
   ii = IJK[0]; t1D = n1D;
   if (-1 < ii && !ba[t1D]) {
      do {
         --ii; --t1D;
      } while (-1 < ii && !ba[t1D]);  
      if (-1 < ii) {
         ta[1] = av[t1D];  /* The value at the non zero in -ve i*/
         da[1] = IJK[0]-ii;/* The number of voxels needed to get there*/
         hitcode = hitcode + SUMA_InegBOUND;
      }
   }
   
   SUMA_RETURN(hitcode);
}

/*!   
   Trace rays from voxel n1D in +j and -j directions.
   Return the first non zero value (ta) encountered and the number of
   voxels traveled (da) to get to it. 
   Results are stored in ta[0] and da[0] for +ve direction
   and ta[1] and da[1] for -ve direction.
*/  
int SUMA_ray_j(int n1D, int ni, int nij, int nj, float *av, byte *ba, 
               float ta[], int da[])
{
   static char FuncName[]={"SUMA_ray_j"};
   int IJK[3], jj, t1D;
   int hitcode;
   
   SUMA_ENTRY;
   
   ta[0] = ta[1] = 0.0;
   da[0] = da[1] = 0;
   hitcode = 0;
   
   Vox1D2Vox3D(n1D, ni, nij, IJK) 
   
   /* shoot ray in +ve direction */
   jj = IJK[1]; t1D = n1D;
   if (jj < nj && !ba[t1D]) {
      do {
         ++jj; t1D = IJK[0]+jj*ni+IJK[2]*nij;
      } while (jj < nj && !ba[t1D]);  
      if (jj < nj) {
         ta[0] = av[t1D];  /* The value at the non zero in +ve j*/
         da[0] = jj-IJK[1];/* The number of voxels needed to get there*/
         hitcode = hitcode + SUMA_JposBOUND;
      }
   }
   
   /* shoot ray in -ve direction */
   jj = IJK[1]; t1D = n1D;
   if (-1 < jj && !ba[t1D]) {
      do {
         --jj; t1D = IJK[0]+jj*ni+IJK[2]*nij;
      } while (-1 < jj && !ba[t1D]);  
      if (-1 < jj) {
         ta[1] = av[t1D];  /* The value at the non zero in -ve j*/
         da[1] = IJK[1]-jj;/* The number of voxels needed to get there*/
         hitcode = hitcode + SUMA_JnegBOUND;
      }
   }
   
   SUMA_RETURN(hitcode);
}

/*!   
   Trace rays from voxel n1D in +k and -k directions.
   Return the first non zero value (ta) encountered and the number of
   voxels traveled (da) to get to it. 
   Results are stored in ta[0] and da[0] for +ve direction
   and ta[1] and da[1] for -ve direction.
*/  
int SUMA_ray_k(int n1D, int ni, int nij, int nk, float *av, byte *ba, 
               float ta[], int da[])
{
   static char FuncName[]={"SUMA_ray_k"};
   int IJK[3], kk, t1D;
   int hitcode;
   
   SUMA_ENTRY;
   
   ta[0] = ta[1] = 0.0;
   da[0] = da[1] = 0;
   hitcode = 0;
   
   Vox1D2Vox3D(n1D, ni, nij, IJK) 
   
   /* shoot ray in +ve direction */
   kk = IJK[2]; t1D = n1D;
   if (kk < nk && !ba[t1D]) {
      do {
         ++kk; t1D = IJK[0]+IJK[1]*ni+kk*nij;
      } while (kk < nk && !ba[t1D]);  
      if (kk < nk) {
         ta[0] = av[t1D];  /* The value at the non zero in +ve k*/
         da[0] = kk-IJK[2];/* The number of voxels needed to get there*/
         hitcode = hitcode + SUMA_kposBOUND;
      }
   }
   
   /* shoot ray in -ve direction */
   kk = IJK[2]; t1D = n1D;
   if (-1 < kk && !ba[t1D]) {
      do {
         --kk; t1D = IJK[0]+IJK[1]*ni+kk*nij;
      } while (-1 < kk && !ba[t1D]);  
      if (-1 < kk) {
         ta[1] = av[t1D];  /* The value at the non zero in -ve j*/
         da[1] = IJK[2]-kk;/* The number of voxels needed to get there*/
         hitcode = hitcode + SUMA_knegBOUND;
      }
   }
   
   SUMA_RETURN(hitcode);
}

/*! Find a vole in a volume. A hole voxel is a voxel
    with 0 value that is surrounded in at least one
    dimension by non zero voxels. */
int SUMA_find_hole_voxels( int Ni, int Nj, int Nk, 
                           float *fa, byte *ba, int *holeat)
{
   static char FuncName[]={"SUMA_find_hole_voxels"};
   int nh=0, v, Nij, Nijk;
   int da[2]; 
   float ta[2];
   
   SUMA_ENTRY;
   
   Nij = Ni*Nj; Nijk = Nij*Nk;
   
   nh = 0;
   for (v=0; v<Nijk; ++v) {
      if (SUMA_ray_i(v, Ni, Nij, fa, ba, ta, da) 
                     == SUMA_I_HOLE) {
         holeat[nh] = v; ++nh;
         continue;
      } else if (SUMA_ray_j(v, Ni, Nij, Nj, fa, ba, ta, da) 
                     == SUMA_J_HOLE) {
         holeat[nh] = v; ++nh;
         continue;
      } else if (SUMA_ray_k(v, Ni, Nij, Nk, fa, ba, ta, da) 
                     == SUMA_K_HOLE) {
         holeat[nh] = v; ++nh;
         continue;
      }
   }
   
   SUMA_RETURN(nh);
}

/*!   A faster version of SUMA_mri_volume_infill
      linfill == 1 rescans volume for holes with each new iteration
                   Cautious approach, but wastes a lot of time.
              == 0 rescans volume for holes once all existing 
                   holes are filled. Faster, but slightly
                   different model. 
         Relatively few voxels are affected by linfill
*/
                    
int SUMA_mri_volume_infill_zoom(MRI_IMAGE *imin, byte linfill) 
{
   static char FuncName[]={"SUMA_mri_volume_infill_zoom"};
   int Ni, Nj, Nk, Nij, Nijk, v, iter;
   int Nfills, h, nh, hitcode, hitsum, da[2];
   byte *ba=NULL, *nhits=NULL;
   float *fa=NULL, ta[2];
   float *sum=NULL;
   int *holeat = NULL;
   int maxiter=500;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   Ni = imin->nx; Nj = imin->ny; Nk = imin->nz; 
   Nij = Ni*Nj; Nijk = Nij*Nk;
   
   fa = MRI_FLOAT_PTR(imin);
   
   /* make byte mask */
   ba = (byte *)SUMA_calloc(Nijk, sizeof(byte));
   for (v=0; v<Nijk; ++v) {
      if (SUMA_ABS(fa[v]-0.0f)>0.00001) ba[v] = 1; 
   }
   holeat = (int *)SUMA_calloc(Nijk, sizeof(int));
   sum = (float *)SUMA_calloc(Nijk, sizeof(float));
   nhits = (byte *)SUMA_calloc(Nijk, sizeof(byte));
   
   
   iter = 0; nh = 0;
   do {
      if (linfill || !iter) {
               /* To ensure that the final volume has no holes in it, 
               you must rescan the whole volume, as opposed to the one 
               pass scan performed only at iteration 0. 
               Two ways to handle this problem. Either repeat a full scan
               with each iteration (linfill = 1) and waste a lot of time.
               With linfill = 1, the function is as slow as the older 
               version SUMA_mri_volume_infill.
               Alternately (linfill = 0), we do a new scan once the first set 
               of holes are filled. That's much faster.  
               The two approaches are not equivalent,
               but very few voxels are affected */

         /* find all hole points */
         nh = SUMA_find_hole_voxels(Ni, Nj, Nk, fa, ba, holeat);
      }
      
      Nfills = 0;
      for (h=0; h<nh; ++h) {
         hitcode = 0; sum[holeat[h]] = 0.0; nhits[holeat[h]]=0;
         hitsum=0;
         if ( (hitcode = SUMA_ray_i(holeat[h], Ni, Nij, fa, ba, ta, da)) == 
               SUMA_I_HOLE) {
            hitsum += hitcode;
            if (da[0] == 1) { /* only process holes at edge */
               sum[holeat[h]] += ta[0]; /* add value at edge voxel */
               ++nhits[holeat[h]];
            }
            if (da[1] == 1) { /* only process holes at edge */
               sum[holeat[h]] += ta[1]; /* add value at edge voxel */
               ++nhits[holeat[h]];
            }
         }
         #if 0
            if (holeat[h] == 1007828) {
               SUMA_S_Notev("Vox %d, iter %d, nhit=%d, hitsum=%d\n"
                            "iRay da=[%d %d], ta=[%f %f]",
                     holeat[h], iter, nhits[holeat[h]], hitcode, 
                     da[0], da[1], ta[0], ta[1]);
            }
         #endif
         if ( (hitcode = SUMA_ray_j(holeat[h], Ni, Nij, Nj, fa, ba, ta, da)) == 
               SUMA_J_HOLE) {
            hitsum += hitcode;
            if (da[0] == 1) { /* only process holes at edge */
               sum[holeat[h]] += ta[0]; /* add value at edge voxel */
               ++nhits[holeat[h]];
            }
            if (da[1] == 1) { /* only process holes at edge */
               sum[holeat[h]] += ta[1]; /* add value at edge voxel */
               ++nhits[holeat[h]];
            }
         }
         if ( (hitcode = SUMA_ray_k(holeat[h], Ni, Nij, Nk, fa, ba, ta, da)) == 
               SUMA_K_HOLE) {
            hitsum += hitcode;
            if (da[0] == 1) { /* only process holes at edge */
               sum[holeat[h]] += ta[0]; /* add value at edge voxel */
               ++nhits[holeat[h]];
            }
            if (da[1] == 1) { /* only process holes at edge */
               sum[holeat[h]] += ta[1]; /* add value at edge voxel */
               ++nhits[holeat[h]];
            }
         }
      }
      /* now update holeat array */
      h = 0;
      while (nh > 0 && h<nh) {
         if (nhits[holeat[h]]) {
            fa[holeat[h]] = 
               sum[holeat[h]]/(float)nhits[holeat[h]]; /* assign new value */
            ba[holeat[h]] = 1;   /* mark as filled */
            
            nhits[holeat[h]] = 0;   /* reset */
            sum[holeat[h]] = 0; 
            
            ++Nfills;   /* keep track of numbers filled  */
            
            holeat[h] = holeat[nh-1]; /* reduce list of holes */
            --nh;
         } else {
            ++h;
         }
      }
      ++iter;
      SUMA_LHv("Iter %d, N_fills = %d, %d holes remain\n", iter, Nfills, nh);
      if (nh == 0 && iter < maxiter && !linfill) {
         /* if nh, rerun full scan to ensure no new voxels qualify as holes.
            This is not necessary if linfill is used because a rescan is carried
            out with each iteration */
         nh = SUMA_find_hole_voxels(Ni, Nj, Nk, fa, ba, holeat);
         if (nh) {
            SUMA_LHv("%d more holes in patched version, one more pass\n", nh);
         }
      }
   } while (nh > 0 && iter < maxiter);
   
   if (nh > 0) {
      SUMA_S_Warnv("Function stopped because of maximum iter limit of %d. "
                  "%d holes still exist.", nh, maxiter);
   }
   
   SUMA_ifree(holeat); SUMA_ifree(ba);      
   
   SUMA_RETURN(1);
}


int SUMA_VolumeInFill(THD_3dim_dataset *aset,
                                     THD_3dim_dataset **filledp,
                                     int method) 
{
   static char FuncName[]={"SUMA_VolumeInFill"};
   float *fa=NULL;
   THD_3dim_dataset *filled = *filledp;   
   MRI_IMAGE *imin=NULL;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
      
   /* get data into float image */
   imin = THD_extract_float_brick(0,aset) ;
   if (method == 0) { /* slow */
      if (!SUMA_mri_volume_infill(imin)) {
         SUMA_S_Err("Failed to fill volume");
         SUMA_RETURN(0);
      }
   } else { /* faster */
      if (!SUMA_mri_volume_infill_zoom(imin, 0)) {
         SUMA_S_Err("Failed to fill volume");
         SUMA_RETURN(0);
      }
   }
   
   /* put results in dset */   
   fa = MRI_FLOAT_PTR(imin);
   
   /* Put result in output dset */
   if (!filled) {
      filled = EDIT_full_copy(aset, FuncName);
      *filledp = filled;
   }
   EDIT_substscale_brick(  filled, 0, MRI_float, fa, 
                           DSET_BRICK_TYPE(filled,0), -1.0);
   EDIT_BRICK_LABEL(filled,0,"HolesFilled"); 
   
   
   SUMA_RETURN(1);
}

/*! 
   A local stat moving average blurring of each sub-brick inside mask .
   This was tested only once and FWHM is not handled properly.
   It just uses a sphere of radius FWHM/2, but it is much slower
   than SUMA_VolumeBlurInMask, so fughet about it
*/
int SUMA_VolumeLSBlurInMask(THD_3dim_dataset *aset, 
                                  byte *cmask,
                                  THD_3dim_dataset **blurredp,
                                  float FWHM) 
{
   static char FuncName[]={"SUMA_VolumeLSBlurInMask"};
   int  sb = 0, ijk, ii, jj, kk, nx, ny, nz, ih, nhood, *nind;
   MRI_vectim *vecim=NULL;
   float faset, *mm=NULL, ws, dx , dy , dz;
   THD_3dim_dataset *blurred= NULL;
   MCW_cluster *nbhd=NULL ;
   short *AA=NULL;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   if( FWHM < 0.0f ){ dx = dy = dz = 1.0f ; FWHM = -FWHM ; }
   else         { dx = fabsf(DSET_DX(aset)) ;
                  dy = fabsf(DSET_DY(aset)) ;
                  dz = fabsf(DSET_DZ(aset)) ; }
   nx = DSET_NX(aset); ny = DSET_NY(aset); nz = DSET_NZ(aset);
   
   nbhd = MCW_spheremask( dx,dy,dz , FWHM/2.0 ) ;
   
   if (LocalHead) {
      SUMA_S_Notev("nbhd: %p\n"
                     "%d voxels.\n",
                     nbhd, nbhd->num_pt);
   }

   /* output dset */
   if (!blurred) {
      blurred = EDIT_full_copy(aset, FuncName);
      *blurredp = blurred;
   }
   
   for (sb=0; sb<DSET_NVALS(aset); ++sb) {
      if (!mm) mm = (float *)calloc(DSET_NVOX(aset), sizeof(float));
      if (!nind) nind = (int *)calloc(nbhd->num_pt, sizeof(int));
      AA = DSET_ARRAY(aset,sb);   /* array of anatomical values */
      faset = DSET_BRICK_FACTOR(aset,sb); if (faset==0.0) faset = 1.0;
      for( kk=0 ; kk < nz ; kk++ ){
         for( jj=0 ; jj < ny ; jj++ ){
            for( ii=0 ; ii < nx ; ii++ ){
               ijk = ii+jj*nx+kk*nx*ny;
               if (! (ijk%100000) ) vstep_print();
               if (IN_MASK(cmask,ijk)) { /* get a mask for that location */
                  nhood = mri_load_nbhd_indices( DSET_BRICK(aset , sb ) ,
                                    cmask , ii,jj,kk , nbhd, nind); 
                                 /* nhood will not be constant, 
                                    when you are close to mask's edge */
                  ws = (float)nhood+1.0;
                  mm[ijk] = AA[ijk];
                  for (ih=0; ih<nhood; ++ih) {
                     mm[ijk] += AA[nind[ih]];
                  }
                  mm[ijk] = mm[ijk]*faset/ws;
               } /* in mask */
            } 
         } 
      } /* kk */
      /* Stick mm back into dset */
      EDIT_substscale_brick(blurred, sb, MRI_float, mm, 
                            DSET_BRICK_TYPE(blurred,sb), -1.0); 
      mm = NULL;
      EDIT_BRICK_LABEL(blurred,sb,"BlurredInMask");      
      SUMA_ifree(nind);
    } /* sb */
   
   SUMA_RETURN(1);
}

/*! 
   Blur each sub-brick inside mask.
    if unifac = 0.0 : Auto brick factor for each sub-brick (safest)
              > 0.0 : Use unifac for all sub-brick factors
              = -1.0: Auto factor for 0th brick, other bricks
                     get same factor.
*/
int SUMA_VolumeBlurInMask(THD_3dim_dataset *aset,
                                     byte *cmask,
                                     THD_3dim_dataset **blurredp,
                                     float FWHM, float unifac) 
{
   static char FuncName[]={"SUMA_VolumeBlurInMask"};
   float fac = 0.0, *fa=NULL;
   MRI_IMAGE *imin=NULL;
   int k=0;
   THD_3dim_dataset *blurred = *blurredp;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   /* get data into float image, preserve scale */
   fac = -1.0;
   for (k=0; k<DSET_NVALS(aset); ++k) {
      imin = THD_extract_float_brick(k,aset) ;

      mri_blur3D_addfwhm(imin, cmask, FWHM);

      /* put results in dset */   
      fa = MRI_FLOAT_PTR(imin);

      /* Put result in output dset */
      if (!blurred) {
         blurred = EDIT_full_copy(aset, FuncName);
         *blurredp = blurred;
      }
      
      if (unifac > 0.0) fac = unifac;
      else if (unifac == -1.0) {
         if (k==0) fac = -1.0; /* auto at 1st sub-brick */
         else fac = DSET_BRICK_FACTOR(blurred, k - 1);
      } else fac = -1.0;
      
      SUMA_LHv("k %d, fac %f\n", k, fac);
      EDIT_substscale_brick(blurred, k, MRI_float, fa, 
                              DSET_BRICK_TYPE(blurred,k), fac);
      
      EDIT_BRICK_LABEL(blurred,k,"BlurredInMask"); 
   }
   
   SUMA_RETURN(1);
}

/* Energy assigned to the contrast between voxel values a0 and a1 
   Sign of contrast is irrelevant, ratio is to neutralize the 
   bias factor, but low intensity values will cause some trouble.
   Biger contast --> Higher energy*/
#define EDGE_EN1(a1,a0, d0, d1) (SUMA_ABS((a1)-(a0))/((d1)+(d0)+0.000001))

double SUMA_EdgeEnergy(short *a, float af, short *b, float bf,
                      int Ni, int Nj, int Nk,
                      short *c, short c1, short c2, 
                      byte *mask,
                      short *skel, 
                      int *n_en) {
   static char FuncName[]={"SUMA_EdgeEnergy"};
   int ii, jj, kk, Nij, Nijk, off, n1, n0;
   double en = 0.0;
   
   SUMA_ENTRY;
   
   *n_en = 0;
   Nij = Ni*Nj; Nijk = Nij*Nk;
   if (skel) memset(skel, 0, Nijk*sizeof(short));
   
   /* The i direction */
   for (kk=0; kk<Nk; ++kk) { for (jj=0; jj<Nj; ++jj) {
      off = jj*Ni+kk*Nij;
      for (ii=1; ii<Ni; ++ii) {
         n1 = ii+off; n0 = n1-1;
         if ( IN_MASK(mask, n1) &&
              IN_MASK(mask, n0) &&
              c[n1] != c[n0]              &&
             (c[n1] == c1 || c[n1] == c2) &&
             (c[n0] == c1 || c[n0] == c2) ) {
            if (skel) {
               skel[n1] = c[n1]; skel[n0] = c[n0];
            }
            en += EDGE_EN1(a[n1], a[n0], b[n1], b[n0]); ++(*n_en);
         }
      }
   } }
   
   /* the j direction */
   for (kk=0; kk<Nk; ++kk) { for (ii=0; ii<Ni; ++ii) {
      off = ii+kk*Nij;
      for (jj=1; jj<Nj; ++jj) {
         n1 = (jj*Ni)+off; n0 = n1-Ni;
         if ( IN_MASK(mask, n1) &&
              IN_MASK(mask, n0) &&
              c[n1] != c[n0]              &&
             (c[n1] == c1 || c[n1] == c2) &&
             (c[n0] == c1 || c[n0] == c2) ) {
            if (skel) {
               skel[n1] = c[n1]; skel[n0] = c[n0];
            }
            en += EDGE_EN1(a[n1], a[n0], b[n1], b[n0]); ++(*n_en);
         }
      }
   } }
      
   /* the k direction */
   for (ii=0; ii<Ni; ++ii) { for (jj=0; jj<Nj; ++jj) { 
      off = ii+jj*Ni;
      for (kk=1; kk<Nk; ++kk) {
         n1 = (kk*Nij)+off; n0 = n1-Nij;
         if ( IN_MASK(mask, n1) &&
              IN_MASK(mask, n0) &&
              c[n1] != c[n0]              &&
             (c[n1] == c1 || c[n1] == c2) &&
             (c[n0] == c1 || c[n0] == c2) ) {
            if (skel) {
               skel[n1] = c[n1]; skel[n0] = c[n0];
            }
            en += EDGE_EN1(a[n1], a[n0], b[n1], b[n0]); ++(*n_en);
         }
      }
   } }
   
   en *= af/bf;
   SUMA_RETURN(en);
}

double SUMA_DsetEdgeEnergy(THD_3dim_dataset *aset,
                      short *c,
                      byte *mask, 
                      THD_3dim_dataset *fset,
                      SUMA_CLASS_STAT *cs)
{
   static char FuncName[]={"SUMA_DsetEdgeEnergy"};
   double en=0.0, env[64];
   short *a=NULL;
   short *skel = NULL;
   short *f=NULL;
   float af=1.0, ff= 1.0;
   int c1,c2, ke, n_env[64], n_en=0, sum_n_en=0;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   if (!fset) {
      SUMA_S_Warn("No bias se, substituting with anatomy");
      fset = aset;
   }
   a = (short *)DSET_ARRAY(aset, 0);
   f = (short *)DSET_ARRAY(fset, 0);
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0) af = 1.0;
   ff = DSET_BRICK_FACTOR(fset,0); if (ff == 0.0) ff = 1.0;
   
   ke = 0; sum_n_en = 0.0;
   for (c1=0; c1<cs->N_label; ++c1) {
      for (c2=c1+1; c2<cs->N_label; ++c2) {
         en = SUMA_EdgeEnergy(a, af, f, af,
                  DSET_NX(aset), DSET_NY(aset), DSET_NZ(aset),
                  c, cs->keys[c1], cs->keys[c2], mask, skel,  &n_en);
         SUMA_LHv("%s %s: Energy %f, Nedges %d\n", 
                           cs->label[c1], cs->label[c2], en, n_en);
         /* Normalize by average intensity */
         env[ke] = en/SUMA_ABS( SUMA_get_Stat(cs, cs->label[c2], "mean")+
                           SUMA_get_Stat(cs, cs->label[c1], "mean") );
         n_env[ke] = n_en; sum_n_en += n_en; 
         ++ke;    
      }
   }
   
   #if 0
   /* combine energies from each combination, 
      weigh by number of edges involved */
   en = 0;
   for (c1=0; c1<ke; ++c1) {
      en += (double)n_env[c1]/(double)sum_n_en*env[c1];
   }
   #else
   en = 0;
   for (c1=0; c1<ke; ++c1) {
      en += env[c1]/(double)n_env[c1];
   }
   #endif
   SUMA_RETURN(en);
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
   float *dmv=NULL, FWHMbias=0.0;
   short *c=NULL, *a=NULL;
   char *str_lab=NULL;
   byte *mm=NULL;
   NI_str_array *bc=NULL, *bsc=NULL;
   MRI_IMAGE *imout=NULL, *imin=NULL;
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

   if (polorder < 0) polorder = Opt->bias_param;
   if (polorder < 0) {
      SUMA_S_Err("Failed to set polorder");
      SUMA_RETURN(NULL);
   }
   
   /* How many groups of classes do we have? */
   vl_dtable = SUMA_LabelsKeys2Dtable( Opt->clss->str, Opt->clss->num,  
                                       Opt->keys);
   c = (short *)DSET_ARRAY(cset, 0);
   a = (short *)DSET_ARRAY(aset, 0);
   mm = (byte *)calloc (DSET_NVOX(cset), sizeof(byte));
   bc = NI_strict_decode_string_list(Opt->bias_classes,";");
   Opt->N_biasgroups = bc->num;
   for (i=0; i<Opt->N_biasgroups; ++i) {
      if (Opt->debug > 1) 
         fprintf(stderr,"   Group %s %d/%d\n", bc->str[i], i+1, bc->num);
      /* which keys belong to this group ? */
      bsc = NI_strict_decode_string_list(bc->str[i],", ");
      for (j=0; j<bsc->num; ++j) {
         if (Opt->debug > 1)
            fprintf(stderr,"     Sub Group %s %d/%d\n", 
                           bsc->str[j], j+1, bsc->num);
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
      DSET_quiet_overwrite(Opt->gset); 
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
      } else {
         dmv[i] = 0.0;
      }
   }
   
   
   if (Opt->debug > 1) {/* store scaled intensities */
      THD_3dim_dataset *pbb=NULL;
      short *am=NULL;
      if (!pbb) {
         NEW_SHORTY(aset, 1, "bias_data", pbb);
      }
      am = (short *)DSET_ARRAY(pbb,0);
      for (i=0; i<DSET_NVOX(aset);++i) {
         am[i]=(short)(dmv[i]*10000.0);
      }
      EDIT_BRICK_FACTOR(pbb,0, 1.0/10000.0);
      DSET_quiet_overwrite(pbb); 
      DSET_delete(pbb); pbb=NULL;
   }
   
   FWHMbias = 25;
   
   if (1) {/* fill the thing, then blur*/
      byte *fm = (byte *)SUMA_calloc(DSET_NVOX(aset), sizeof(byte));
      if (Opt->debug > 1) SUMA_S_Note("Filling then blurring");
      SUMA_mri_volume_infill_zoom(imin, 0);
      for (i=0; i<DSET_NVOX(aset);++i) {
         if (SUMA_ABS(dmv[i]-0.0f)>0.00001) fm[i]=1;
      }
      mri_blur3D_addfwhm(imin, fm, FWHMbias); 
      SUMA_ifree(fm);
   } else {/* blur then do polyfit, bad fitting artifacts, not worth it*/ 
      if (Opt->debug > 1) SUMA_S_Note("Blurring then fitting with mri_polyfit");
      mri_blur3D_addfwhm(imin, mm, FWHMbias); 
      /* do the fit */
      mri_polyfit_verb(Opt->debug) ;
      if (!(imout = mri_polyfit( imin , polorder , mm , 0.0 , Opt->fitmeth )))  {
         ERROR_exit("Failed to fit");
      }
      dmv = MRI_FLOAT_PTR(imout) ;   
   }
   

   
   if (Opt->debug > 1) {/* save the fit */
      THD_3dim_dataset *pbb=NULL;
      short *am=NULL;
      if (!pbb) {
         NEW_SHORTY(aset, 1, "bias_estimate", pbb);
      }
      am = (short *)DSET_ARRAY(pbb,0);
      for (i=0; i<DSET_NVOX(aset);++i) {
         am[i]=(short)(dmv[i]*10000.0);
      }
      EDIT_BRICK_FACTOR(pbb,0, 1.0/10000.0);
      DSET_quiet_overwrite(pbb); 
      DSET_delete(pbb); pbb=NULL;
   }
   
   /* calculate average bias */
   M_dg = 0.0;
   for (i=0; i<DSET_NVOX(aset); ++i) { 
      if (mm[i]) M_dg += dmv[i]; 
   }
   M_dg /= (double)N_mm;  /* grand mean */
   
   
   /* cleanup  */
   mri_free(imin); imin = NULL;
   if (imout) mri_free(imout); imout = NULL;
   
   /* scale bias by mean to make output image be closer to input */
   for (i=0; i<DSET_NVOX(aset); ++i) dmv[i] /= M_dg;
      
   /* store */
   EDIT_substscale_brick(pout, 0, MRI_float, dmv, MRI_short, -1.0);
   EDIT_BRICK_LABEL(pout,0,"BiasField");
   

   SUMA_RETURN(pout);
}

int SUMA_Class_k_Label_Locator(SUMA_CLASS_STAT *cs, char *label) 
{
   static char FuncName[]={"SUMA_Class_k_Label_Locator"};
   int k=0;
   
   SUMA_ENTRY;
   
   if (!label) SUMA_RETURN(-1);
   
   while (k < cs->N_label) {
      if (!strcmp(cs->label[k],label)) SUMA_RETURN(k);   
      ++k;
   }
   
   SUMA_RETURN(-1);   
}

int SUMA_Class_k_Key_Locator(SUMA_CLASS_STAT *cs, int kk) 
{
   static char FuncName[]={"SUMA_Class_k_Key_Locator"};
   int k=0;
   
   SUMA_ENTRY;
   
   while (k < cs->N_label) {
      if (k == kk) SUMA_RETURN(k); 
      ++k;  
   }
   
   SUMA_RETURN(-1);   
}

/*!
   Find indices of particular classes in SUMA_CLASS_STAT
   \param cs: The classes statistics structure
   \param action: A string specifying what to do with the
                  'value' string
   \param value: A string of parameters, ';' separated 
                  for the 'action' string. If Value is NULL,
                  everything matches.
      action == "classes_string": Search for classes in 'value'
                "not_classes_string": Search for classes not in 'value'
   \param UseK: A vector of indices into classes matching the search.
                Can be NULL if you just care for the count.
   \return N_UseK: the number of classes found.
                   -1 in error
*/
int SUMA_Class_k_Selector(
   SUMA_CLASS_STAT *cs, char *action, char *value, int *UseK)
{
   static char FuncName[]={"SUMA_Class_k_Selector"};
   NI_str_array *bc=NULL;
   int k, ii, N_kok;
   
   SUMA_ENTRY;
   
   N_kok = -1;
   
   if (!strcmp(action, "classes_string")) {
      if (!value) {
         N_kok=cs->N_label;
         if (UseK) {
            for (k=0; k<N_kok; ++k) UseK[k] = k;
         }
      } else {
         bc = NI_strict_decode_string_list(value,";");
         N_kok=0;
         for (ii=0; ii < bc->num; ++ii) {
            for (k=0; k<cs->N_label; ++k) {
               if (!strcmp(bc->str[ii], cs->label[k])) {
                  if (UseK) { UseK[N_kok] = k; } 
                  ++N_kok; 
               }
            }
         }
         NI_delete_str_array(bc );
      }
      SUMA_RETURN(N_kok);
   }
   
   if (!strcmp(action, "not_classes_string")) {
      if (!value) {
         N_kok=cs->N_label;
         if (UseK) {
            for (k=0; k<N_kok; ++k) UseK[k] = k;
         }
      } else {
         bc = NI_strict_decode_string_list(value,";");
         N_kok=0;
         for (ii=0; ii < bc->num; ++ii) {
            for (k=0; k<cs->N_label; ++k) {
               if (strcmp(bc->str[ii], cs->label[k])) {
                  if (UseK) {
                     UseK[N_kok] = k; 
                  }
                  ++N_kok; 
               }
            }
         }
         NI_delete_str_array(bc );
      }
      SUMA_RETURN(N_kok);
   }
   
   /* See LabelToGroupedIndex to add actions for getting sub-groups */
   
   SUMA_S_Errv("Action %s not supported\n", action);
   
   SUMA_RETURN(-1);
}

int SUMA_estimate_bias_field_Wells (SEG_OPTS *Opt, 
                                       byte *cmask, SUMA_CLASS_STAT *cs,
                                       float fwhm, char *bias_classes,
                                       THD_3dim_dataset *Aset,
                                       THD_3dim_dataset *pstCgALL,
                                       THD_3dim_dataset **Bsetp) 
{
   static char FuncName[]={"SUMA_estimate_bias_field_Wells"};
   int ijk, k, N_kok, kok, *UseK ;
   THD_3dim_dataset *Rset=NULL, *Psset=NULL, *Bset = *Bsetp;
   float *fpstCgALL, fAset, fBset, *R, *Ps;
   char *str_lab=NULL;
   MRI_IMAGE *imout=NULL, *imin=NULL;
   double df, sdf, Ai, Gik, Ri, *Mg, *Sg;
   static int iter = 0;
   struct  timeval tti;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   /* checks */
   if (!Aset || !pstCgALL || !cs) {
      SUMA_S_Errv("Bad input %p %p %p \n", 
                  Aset, pstCgALL, cs);
      SUMA_RETURN(0);
   }
   
   /* init */
   if (!Bset) { 
      NEW_SHORTY(Aset, 1, "bWells", Bset);
      *Bsetp = Bset;
   }
   
   if (!Bset) SUMA_RETURN(0);
   
   UseK = (int *)SUMA_calloc(cs->N_label, sizeof(int));
   if ((N_kok = SUMA_Class_k_Selector(cs, "classes_string", 
                                          bias_classes, UseK))<0) {
      SUMA_S_Err("Failed to find classes");
      SUMA_RETURN(0);
   }
   
   NEW_FLOATY(Aset, 1, "Rset", Rset); 
   NEW_FLOATY(Aset, 1, "Psset", Psset); 
   
   Mg = SUMA_get_Stats(cs, "meanL");
   Sg = SUMA_get_Stats(cs, "stdvL");
   fpstCgALL = (float*)SUMA_calloc(cs->N_label, sizeof(float));
   GET_BFs(pstCgALL, fpstCgALL);
   fAset = DSET_BRICK_FACTOR(Aset,0);
   R = (float *)DSET_ARRAY(Rset,0);
   Ps = (float *)DSET_ARRAY(Psset,0);
   for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
      if (IN_MASK(cmask, ijk)) {
         sdf = 0.0; Ri = 0.0;
         for (kok=0; kok<N_kok; ++kok) {
            
            k = UseK[kok];
            GSCVAL(pstCgALL, k, ijk, fpstCgALL[k], Gik);
            GSCVAL(Aset, 0, ijk, fAset, Ai);
            df = Gik/(Sg[k]*Sg[k]);
            Ri += (log(Ai)-Mg[k])*df;
            sdf += df;
            if (ijk == Opt->VoxDbg) {
               SUMA_S_Notev("Cls %s, Ri %f, Psinv %f\n",
                           cs->label[k], Ri, sdf);
            }
         }
         R[ijk] = Ri;
         Ps[ijk] = sdf;   
      }      
   }

   if (Opt->debug > 1) {/* store scaled intensities */
      SUMA_SEG_WRITE_DSET("Rset-PreBlur", Rset, iter, Opt->hist);   
      SUMA_SEG_WRITE_DSET("Psset-PreBlur", Psset, iter, Opt->hist);   
   }
   
   SUMA_etime (&tti, 0);
   /* Blur the two sets */
   SUMA_LH("Blur Rset");
   

#ifdef USE_OMP
#pragma omp parallel
{
   THD_3dim_dataset *bb[2];
   int is=0;
   
   bb[0] = Rset;
   bb[1] = Psset;   
#pragma omp for
   for (is=0; is<2; ++is) {
      SUMA_LHv("Blur Set %d\n", is);
      if (!(SUMA_VolumeBlurInMask(bb[is],
                            cmask,
                            bb+is, fwhm, 0.0))) {
         SUMA_S_Err("Failed to blur");
      } 
   }   
} /* end OpenMP */
#endif
   if (1 || Opt->debug) { SUMA_S_Notev("Smoothing duration %f seconds\n", 
                                    SUMA_etime (&tti, 1)); }
                                    
   if (Opt->debug > 1) {/* store scaled intensities */
      SUMA_SEG_WRITE_DSET("Rset-PostBlur", Rset, iter, Opt->hist);   
      SUMA_SEG_WRITE_DSET("Psset-PostBlur", Psset, iter, Opt->hist);   
   }
   
   fBset = 1.0/10000.0;
   R = (float *)DSET_ARRAY(Rset,0);
   Ps = (float *)DSET_ARRAY(Psset,0);
   for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
      if (IN_MASK(cmask, ijk)) {
         PSCVAL(Bset, 0, ijk, fBset, exp(R[ijk]/Ps[ijk]));
      }
   }
   EDIT_BRICK_FACTOR(Bset, 0, fBset);
      
   if (Opt->debug > 1) {/* store scaled intensities */
      SUMA_SEG_WRITE_DSET("Bset", Bset, iter, Opt->hist);   
   }
   
   
   SUMA_ifree(fpstCgALL); SUMA_ifree(UseK); 
   DSET_delete(Psset); DSET_delete(Rset);

   ++iter;
   
   SUMA_RETURN(1);
}

/*!
   Apply bias field.
*/
int SUMA_apply_bias_field (SEG_OPTS *Opt, 
                                       THD_3dim_dataset *aset,
                                       THD_3dim_dataset *fset,
                                       THD_3dim_dataset **xsetp) {
   static char FuncName[]={"SUMA_apply_bias_field"};
   int i;
   float *d=NULL;
   float bf = 1.0, bfa=1.0, bfb=1.0;
   short *b=NULL, *a=NULL;
   THD_3dim_dataset *xset = *xsetp;
   
   SUMA_ENTRY;
   
   /* checks */
   if (!aset || !fset ) {
      SUMA_S_Errv("Bad input %p %p \n", 
                  aset, fset);
      SUMA_RETURN(0);
   }
   
   /* init */
   if (!xset) {
      NEW_SHORTY(aset, 1, Opt->xrefix, xset);
      *xsetp = xset;
   }
   if (!xset) RETURN(0);
   if( !THD_ok_overwrite() && THD_is_file( DSET_HEADNAME(xset) ) ){
      SUMA_S_Warnv("Output file %s already exists and not in overwrite mode!\n",
                  DSET_HEADNAME(xset) ) ;
   }
   
   /* apply the bias field */
   if (Opt->debug > 1) INFO_message("Applying field");
   bfa = DSET_BRICK_FACTOR(aset,0); if (bfa == 0.0) bfa = 1.0;
   bfb = DSET_BRICK_FACTOR(fset,0); if (bfb == 0.0) bfb = 1.0; 
   b = (short *)DSET_ARRAY(fset,0);
   a = (short *)DSET_ARRAY(aset,0);
   d = (float *)calloc(DSET_NVOX(xset), sizeof(float));
   for (i=0; i<DSET_NVOX(xset); ++i) {
      bf = b[i]*bfb;
      if (bf > 0.5 && bf < 2.0) /* Extremists at edges cause mayhem 
                                     with division */
         d[i] = (float)a[i]/bf*bfa; 
      else d[i] = a[i]*bfa;
   }
   EDIT_substscale_brick(xset, 0, MRI_float, d, MRI_short, -1.0);
   EDIT_BRICK_LABEL(xset,0,"BiasCorrected");

   SUMA_RETURN(1);
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
                     byte *cmask, int cmask_count,
                     THD_3dim_dataset *pstCgALL,
                     THD_3dim_dataset *priCgALL,
                     THD_3dim_dataset *gold,
                     SUMA_CLASS_STAT *cs) 
{
   static char FuncName[]={"SUMA_Class_stats"};
   int i=0, j = 0, sb=0, l;   
   short *a=NULL, *c=NULL, *w=NULL;
   float af=1.0, wf=1.0, fpriCgALL;
   double n, Asum2, Asum, Amean, Astd, wsum, ff, *nv=NULL, ww=0.0,
          la, AmeanL, AsumL, AstdL, Asum2L, *w0=NULL, *mixden=NULL;
    
   SUMA_ENTRY;
      
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0f) af = 1.0;
   a  = (short *)DSET_ARRAY(aset,0);
   if (cset) {
      c  = (short *)DSET_ARRAY(cset,0);
      if (DSET_BRICK_FACTOR(cset,0) != 0.0 && DSET_BRICK_FACTOR(cset,0) != 1.0) {
         SUMA_S_Err("Cset factor != 0.0 || != 1.0");
         SUMA_RETURN(0);
      }
   }
   
   if (!pstCgALL) {
      if (!c) {
         SUMA_S_Err("No classes, and no weighting set");
         SUMA_RETURN(0);
      }
      for (j=0; j<cs->N_label; ++j) {
         n = 0;
         Asum2 = 0.0; Asum = 0.0; 
         Asum2L = 0.0; AsumL = 0.0; 
         for (i=0; i<DSET_NVOX(aset); ++i) {                
            if (IN_MASK(cmask,i) && c[i] == cs->keys[j]) {
               Asum2 += a[i]*a[i]; 
               Asum  += a[i];
               la = log(a[i]*af+0.00001); Asum2L += la*la; AsumL += la;
               ++n; 
            }   
         }
         Astd = sqrt((Asum2-Asum*Asum/n)/(n-1))*af;
         Amean = Asum/n*af;
         AstdL = sqrt((Asum2L-AsumL*AsumL/n)/(n-1));
         AmeanL = AsumL/n;
         SUMA_set_Stat(cs, cs->label[j], "num", n);
         SUMA_set_Stat(cs, cs->label[j], "mean", Amean);
         SUMA_set_Stat(cs, cs->label[j], "stdv", Astd);
         SUMA_set_Stat(cs, cs->label[j], "meanL", AmeanL);
         SUMA_set_Stat(cs, cs->label[j], "stdvL", AstdL);
         SUMA_set_Stat(cs, cs->label[j], "mix", n/cmask_count);
      }
   } else {
      if (DSET_NVALS(pstCgALL) != cs->N_label &&
          DSET_NVALS(pstCgALL) != 1) {
         SUMA_S_Errv("Weight set must be 1 or %d sub-bricks. Have %d\n",
                     cs->N_label, DSET_NVALS(pstCgALL));
         SUMA_RETURN(0); 
      }
      mixden = (double *)SUMA_calloc(cs->N_label, sizeof(double));
      for (j=0; j<cs->N_label; ++j) {
         if (DSET_NVALS(pstCgALL) != 1) {
            sb = j;
         } else sb = 1;
         
         if (0 && priCgALL) { /* Need to setup denom for mix frac, 
                           as in Ashburner 2005. 
                           I turned it off for now because 
                           convergence (as deemed from Dice and Bias
                           correction in banat) seems better without it */
            fpriCgALL =  DSET_BRICK_FACTOR(priCgALL, j);
            if (!(w0=SUMA_get_Stats(cs, "mix"))) {
               mixden[j] = 0.0;
               for (i=0; i<DSET_NVOX(aset); ++i) {                
                  if (IN_MASK(cmask,i)) {
                     GSCVAL(priCgALL, j, i, fpriCgALL, ww); 
                     mixden[j] += ww;
                  }
               }
               mixden[j] *= (double)cs->N_label;
            } else {
               mixden[j] = 0.0;
               for (i=0; i<DSET_NVOX(aset); ++i) {
                  if (IN_MASK(cmask,i)) {
                     GSCVAL(priCgALL, j, i, fpriCgALL, ff);
                     Asum = 0.0;
                     for (l=0; l<cs->N_label; ++l) {
                        GSCVAL(priCgALL, l, i, fpriCgALL, ww);
                        Asum += w0[l]*ww;
                     }
                     mixden[j] += ff/Asum; 
                  }   
              } 
            }
         }else {
            mixden[j] = cmask_count;
         }
         wf = DSET_BRICK_FACTOR(pstCgALL,sb); if (wf == 0.0f) wf = 1.0;
         w = (short *)DSET_ARRAY(pstCgALL,sb);

         Asum2 = 0.0; Asum = 0.0; wsum = 0.0;
         Asum2L = 0.0; AsumL = 0.0; 
         for (i=0; i<DSET_NVOX(aset); ++i) {                
            if (IN_MASK(cmask,i)) {
               ww = w[i]*wf;
               Asum  += ww*a[i]; wsum += ww;
               AsumL += ww*log(a[i]*af+0.00001);
            }   
         }
         Amean = Asum/wsum;
         AmeanL = AsumL/wsum;
         n = 0.0;
         Asum2 = 0.0; Asum = 0.0; 
         for (i=0; i<DSET_NVOX(aset); ++i) {                
            if (IN_MASK(cmask,i)) {
               ff = (a[i]-Amean);
               ww = w[i]*wf;
               Asum2  += ww*(ff*ff);
               la = log(a[i]*af+0.00001)-AmeanL;
               Asum2L += ww*(la*la); 
               if (c && c[i] == cs->keys[j]) ++n;
            }   
         }
         Astd = sqrt(Asum2/wsum)*af;
         Amean = Amean*af;
         AstdL = sqrt(Asum2L/wsum);
         SUMA_set_Stat(cs, cs->label[j], "num", n);
         SUMA_set_Stat(cs, cs->label[j], "mean", Amean);
         SUMA_set_Stat(cs, cs->label[j], "stdv", Astd);
         SUMA_set_Stat(cs, cs->label[j], "meanL", AmeanL);
         SUMA_set_Stat(cs, cs->label[j], "stdvL", AstdL);
         SUMA_set_Stat(cs, cs->label[j], "mix", wsum/mixden[j]);
      }
      SUMA_ifree(mixden);
   }
   
      
   /* and the dice */
   if (gold && cset) {
      SUMA_CompareSegDsets(gold, cset, cmask, 1, cs );
   }
   
   SUMA_RETURN(1);   
}

int SUMA_Add_Class_Stat(SUMA_CLASS_STAT *cs, char *pname) 
{
   static char FuncName[]={"SUMA_Add_Class_Stat"};
   int i=0;
   
   SUMA_ENTRY;
   
   if (cs->pname) {
      for (i=0; i<cs->nP; ++i) {
         if (!strcmp(cs->pname[i],pname)) SUMA_RETURN(i);
      }
   }
   
   /* nothing found */
   cs->nP = cs->nP+1;
   cs->pname = (char **)SUMA_realloc(cs->pname, sizeof(char*)*cs->nP);
   cs->pname[cs->nP-1] = SUMA_copy_string(pname); 
   cs->Pv = (double **)SUMA_realloc(cs->Pv,sizeof(double*)*cs->nP);
   cs->Pv[cs->nP-1] = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   
   SUMA_RETURN(cs->nP-1);
}

int SUMA_Add_Class_Label(SUMA_CLASS_STAT *cs, char *label, int key) 
{
   static char FuncName[]={"SUMA_Add_Class_Label"};
   int i=0;
   
   SUMA_ENTRY;
   
   for (i=0; i<cs->N_label; ++i) {
      if (!strcmp(cs->label[i],label)) SUMA_RETURN(i);
   }
   
   for (i=0; i<cs->N_label; ++i) {
      if (cs->keys[i]==key) {
         SUMA_S_Errv("key %d for new label %s is in use already for %s\n",
                     key, label, cs->label[i]);
         SUMA_RETURN(0);
      }
   }
   
   /* nothing found */
   cs->N_label = cs->N_label+1;
   cs->label = (char **)SUMA_realloc(cs->label, cs->N_label*sizeof(char *));
   cs->label[cs->N_label-1] = SUMA_copy_string(label);
   
   cs->keys = (int *)SUMA_realloc(cs->keys, cs->N_label*sizeof(int));
   cs->keys[cs->N_label-1] = key;
   
   for (i=0; i<cs->nP; ++i) {
      cs->Pv[i] = (double *)SUMA_realloc(cs->Pv[i],cs->N_label*sizeof(double));
   }
   
   SUMA_RETURN(cs->N_label-1);
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
      if (nP != 3) {
         SUMA_S_Errv("Can only handle 3 parameters (not %d) without names\n",
                     nP);
         SUMA_RETURN(NULL);
      }
   }
   cs->N_label = clss->num;
   cs->nP = 0; cs->pname=NULL, cs->Pv=NULL;
   cs->label = (char **)SUMA_calloc(cs->N_label,sizeof(char *));
   cs->keys = (int *)SUMA_calloc(cs->N_label, sizeof(int));
   for (i=0; i<clss->num; ++i) {
      cs->label[i] = SUMA_copy_string(clss->str[i]);
      if (keys) cs->keys[i] = keys[i];
      else cs->keys[i] = i+1;
   }
   for (i=0; i<nP; ++i) {
      if (pnames) {
         if (SUMA_Add_Class_Stat(cs, pnames->str[i]) < 0) {
            SUMA_S_Errv("Failed to add %s\n", pnames->str[i]);
            SUMA_RETURN(NULL); 
         }
      } else {
         switch(i) {
            case 0:
               if (SUMA_Add_Class_Stat(cs, "num") < 0) {
                  SUMA_S_Errv("Failed to add %s\n", pnames->str[i]);
                  SUMA_RETURN(NULL); 
               }
               break;
            case 1:
               if (SUMA_Add_Class_Stat(cs, "mean") < 0) {
                  SUMA_S_Errv("Failed to add %s\n", pnames->str[i]);
                  SUMA_RETURN(NULL); 
               }
               break;
            case 2:
               if (SUMA_Add_Class_Stat(cs, "stdv") < 0) {
                  SUMA_S_Errv("Failed to add %s\n", pnames->str[i]);
                  SUMA_RETURN(NULL); 
               }
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
         for (i=0; i<cs->nP; ++i) { 
            SUMA_ifree(cs->pname[i]); 
            SUMA_ifree(cs->Pv[i]);
         }
      }
      SUMA_ifree(cs->Pv);
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
   static char FuncName[]={"SUMA_Stat_position"};
   int i=0,k=0;

   SUMA_ENTRY;
   
   pp[0] = pp[1] = -1;

   if (label) {
      for (i=0; i<cs->N_label; ++i) {
         if (!strcmp(cs->label[i], label)) {
            pp[0] = i;
            break;
         }
      }
   }

   
   if (pname) {
      for (k=0; k<cs->nP; ++k) {
         if (!strcmp(cs->pname[k], pname)) {
            pp[1] = k;
            break;
         }
      }
   }
   
   if ( (pp[0] < 0 && label) || (pp[1] < 0 && pname)) SUMA_RETURN(0);

   SUMA_RETURN(1);   
}

double SUMA_get_Stat(SUMA_CLASS_STAT *cs, char *label, char *pname) 
{
   static char FuncName[]={"SUMA_get_Stat"};
   int pp[2];

   SUMA_ENTRY;
   
   if (!SUMA_Stat_position(cs, label, pname, pp)) {
      SUMA_S_Errv("Failed to locate %s of %s\n",
                  pname, label);
      SUMA_RETURN(0.0); 
   }   

   SUMA_RETURN(cs->Pv[pp[1]][pp[0]]);
}

double *SUMA_get_Stats(SUMA_CLASS_STAT *cs,  char *pname)
{
   static char FuncName[]={"SUMA_get_Stats"};
   double *vv=NULL;
   int pp[2];
   
   SUMA_ENTRY;
      
   if (!SUMA_Stat_position(cs, NULL,  pname, pp)) {
      SUMA_S_Errv("Failed to locate %s\n",
                  pname);
      SUMA_RETURN(NULL); 
   }
   SUMA_RETURN(cs->Pv[pp[1]]);
}

int SUMA_set_Stat(SUMA_CLASS_STAT *cs, char *label, char *pname, double val)      {
   static char FuncName[]={"SUMA_set_Stat"};
   int pp[2];

   SUMA_ENTRY;
   SUMA_Stat_position(cs, label, pname, pp);
   if (pp[0] < 0) {
      SUMA_S_Errv("Failed to locate class label %s \n",
                  label);
      SUMA_RETURN(0); 
   }
   if (pp[1] < 0) {
      /* add new stat */
      if ((pp[1] = SUMA_Add_Class_Stat(cs, pname)) < 0) {
         SUMA_S_Errv("Failed to add stat %s\n", pname);
         SUMA_RETURN(0); 
      }
   }
   
   cs->Pv[pp[1]][pp[0]] = val; 
   SUMA_RETURN(1);  
}

int SUMA_show_Class_Stat(SUMA_CLASS_STAT *cs, char *head) {
   return(SUMA_dump_Class_Stat(cs, head, SUMA_STDOUT));
}

int SUMA_dump_Class_Stat(SUMA_CLASS_STAT *cs, char *head, FILE *Out)
{
   static char FuncName[]={"SUMA_dump_Class_Stat"};
   char *s=NULL;
   
   SUMA_ENTRY;
   s = SUMA_Class_Stat_Info(cs, head);

   if (!Out) Out = SUMA_STDOUT;
   
   fprintf(Out,"%s", s);
   
   SUMA_ifree(s);
   
   SUMA_RETURN(1);
} 

char *SUMA_Class_Stat_Info(SUMA_CLASS_STAT *cs, char *head)
{
   static char FuncName[]={"SUMA_Class_Stat_Info"};
   int i, j;
   SUMA_STRING *SS;
   char *s=NULL;
   char buf[36];
   
   SUMA_ENTRY;

   SS = SUMA_StringAppend (NULL, NULL);
   
   if (head) {
      SS = SUMA_StringAppend_va(SS,"%s", head);
   }
   SS = SUMA_StringAppend_va(SS,"%8s %4s   ", "Class", "Key");
   for (j=0; j<cs->nP; ++j) {
      SS = SUMA_StringAppend_va(SS,"%8s   ",cs->pname[j]);
   }
   SS = SUMA_StringAppend_va(SS,"\n");
   for (i=0; i<cs->N_label; ++i) {
      sprintf (buf, "%s", 
               MV_format_fval2(cs->keys[i], 4)); 
      SS = SUMA_StringAppend_va(SS,"%8s %4s   ", 
                                    cs->label[i], buf);
      for (j=0; j<cs->nP; ++j) {
         sprintf (buf, "%s", 
               MV_format_fval2(cs->Pv[j][i], 8));
         SS = SUMA_StringAppend_va(SS,"%8s   ", buf );
      }
      SS = SUMA_StringAppend_va(SS,"\n");
   }

   SUMA_SS2S(SS,s);
   
   SUMA_RETURN(s);
}

int SUMA_MixFrac_from_ClassStat(SUMA_CLASS_STAT *cs, float *mf) 
{
   static char FuncName[]={"SUMA_MixFrac_from_ClassStat"};
   float ss=0.0;
   int i;
   
   SUMA_ENTRY;
   
   for (i=0; i<cs->N_label; ++i) {
      mf[i] = SUMA_get_Stat(cs, cs->label[i], "num");
      ss += mf[i];
   }
   for (i=0; i<cs->N_label; ++i) mf[i] /= ss;
   
   SUMA_RETURN(1);
}

double SUMA_mixopt_2_mixfrac(char *mixopt, char *label, int key, int N_clss,
                             byte *cmask, THD_3dim_dataset *cset) 
{
   static char FuncName[]={"SUMA_mixopt_2_mixfrac"};
   int i, ntot, nkey;
   short *a=NULL;
   double frac=-1.0;
   
   SUMA_ENTRY;
   
   if (!mixopt || !strncmp(mixopt,"UNI",3)) {
      frac = 1.0/(double)N_clss;
   } else if (!strcmp(mixopt,"TOY_DEBUG")) {
           if (!strcmp(label, "CSF"))frac = 0.1;
      else if (!strcmp(label, "GM")) frac = 0.45;
      else if (!strcmp(label, "WM")) frac = 0.45;
   } else if (!strcmp(mixopt,"WHOLE_BRAIN")) {
           if (!strcmp(label, "CSF"))frac = 0.015;
      else if (!strcmp(label, "GM")) frac = 0.650;
      else if (!strcmp(label, "WM")) frac = 0.335;
   } else if (!strcmp(mixopt,"AVG152_BRAIN_MASK")) {
           if (!strcmp(label, "CSF"))frac = 0.155;
      else if (!strcmp(label, "GM")) frac = 0.550;
      else if (!strcmp(label, "WM")) frac = 0.295;
   } else if (!strcmp(mixopt,"AVG152p_BRAIN_MASK")) {
           if (!strcmp(label, "CSF"))frac = 0.149;
      else if (!strcmp(label, "GM")) frac = 0.480;
      else if (!strcmp(label, "WM")) frac = 0.371;
   } else if (!strcmp(mixopt,"CSET")) {
      if (!cset) {
         SUMA_S_Err("No -cset input to use with CSET");
         SUMA_RETURN(-1.0);
      }
      /* not the most efficient, doing this once at a time, 
      but this is not called often*/
      a = (short *)DSET_BRICK_ARRAY(cset,0);
      ntot = 0; nkey = 0;
      for (i=0; i<DSET_NVOX(cset); ++i) {
         if (IN_MASK(cmask,i)) {
            ++ntot;
            if (key == a[i]) ++nkey;
         }
      }
      if (ntot) frac = (double)nkey/(double)ntot;
   } else {
      SUMA_S_Errv("-mixopt '%s' cannot be interpreted\n", mixopt);
      SUMA_RETURN(-1.0);
   }
   
   SUMA_RETURN(frac);
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
                        byte *cmask, 
                        SUMA_CLASS_STAT *cs, int neighopt, 
                        THD_3dim_dataset *cset, SEG_OPTS *Opt)
{
   static char FuncName[]={"SUMA_MAP_labels_old"};
   THD_3dim_dataset *pout = cset;
   float af=0.0;
   int iter=0, i=0, k, ni, nj, nk, nij, ijkn[6];
   int Niter = 3;
   double lP[Niter], *mv, *sv, p1m, p2m, dd, *p1=NULL, *p2=NULL, sp1, sp2;
   short *ci=NULL, *co=NULL, *a=NULL;
   
   SUMA_ENTRY;
   
   if (neighopt != 4 && neighopt != 6) {
      SUMA_S_Errv("Allowing neighopt of 4 or 6 only. Have %d\n", neighopt);
      SUMA_RETURN(NULL);
   } 
    
   if (!pout) {
      NEW_SHORTY(aset,1,"MAP_labels_old",pout);
   }
   
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0f) af = 1.0;
   a = (short *)DSET_ARRAY(aset,0);
   co = (short *)DSET_ARRAY(cset,0);
   ci = (short *)malloc(sizeof(short)*DSET_NVOX(aset));
   p1 = (double *)calloc(cs->N_label, sizeof(double));
   p2 = (double *)calloc(cs->N_label, sizeof(double));
   
   ni = DSET_NX(aset);
   nj = DSET_NY(aset);
   nk = DSET_NZ(aset);
   nij = ni*nj;
   
   /* get vector of parameters  */
   mv = SUMA_get_Stats(cs, "mean");
   sv = SUMA_get_Stats(cs, "stdv");
   
   for (iter=0; iter<Niter; ++iter) {
      memcpy(ci, co, sizeof(short)*DSET_NVOX(aset));
      lP[iter] = 0.0;
      for (i=0; i<DSET_NVOX(aset); ++i) {
         if (IN_MASK(cmask,i)) {
            sp1 = 0.0; sp2 = 0.0;
            for(k=0; k<cs->N_label; ++k) {
               /* term 1, likelihood based on gaussians */
               dd = ((double)a[i]*af-mv[k]); dd *= dd; 
               p1[k] = exp(-dd/(2.0*sv[k]*sv[k]) -log(sv[k]))/SQ2PI ; 
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
                        cs->Pv[1][k], cs->Pv[2][k]);      
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

typedef struct {
   SUMA_CLASS_STAT *cs;
   THD_3dim_dataset *aset;
   THD_3dim_dataset *fset;
   short *c;
   byte *cmask;
} EEO_UD; /* user data for SUMA_EdgeEnergy_OptimCost */

static EEO_UD eeoud;

void SUMA_free_eeoud() {
   if (eeoud.c) SUMA_ifree(eeoud.c);
   return;
}

void SUMA_set_eeoud(SUMA_CLASS_STAT *cs, THD_3dim_dataset *aset,
                    THD_3dim_dataset *fset,
                    byte *cmask) {
   SUMA_free_eeoud();
   eeoud.cs = cs;
   eeoud.aset = aset;
   eeoud.fset = fset;
   eeoud.c = (short *)SUMA_calloc(DSET_NVOX(aset),sizeof(short));
   eeoud.cmask = cmask;
   
   return;
}

void SUMA_EdgeEnergy_Gassign(THD_3dim_dataset *aset, THD_3dim_dataset *fset,
                        byte *cmask, SUMA_CLASS_STAT *cs, 
                        double *par, int npar, short *cout)
{
   int i, k;
   double cost = 0.0, eem=0.0, dd=0.0, ee=0.0, mean=0.0, stdv=0.0;
   short *a = (short *)DSET_ARRAY(aset,0);
   float af = DSET_BRICK_FACTOR(aset, 0); 
   short *f = (short *)DSET_ARRAY(fset,0);
   float ff = DSET_BRICK_FACTOR(fset, 0); 
   float aof;
   
   if (af == 0.0f) af = 1.0;
   if (ff == 0.0f) ff = 1.0;
   aof = af/ff;
   /* Assign classes based on gaussians into gcset */
   for (i=0; i<DSET_NVOX(aset); ++i) {
      if (IN_MASK(cmask,i)) {
         eem = -1.0;
         for (k=0; k<cs->N_label; ++k) {
            mean = par[2*k]; stdv = par[2*k+1];
            dd = ((double)a[i]/f[i]*aof-mean); dd *= dd;
            ee = exp(-dd/(2.0*stdv*stdv) -log(stdv));
            #if 0
               if (i == 3247) {
                  fprintf(stderr,
                     "%d: a=%f, f=%f, m=%f, s=%f, k=%d, ee=%f, eem=%f\n",
                     iter, a[i]*af, f[i]*ff, mean, stdv, k, ee, eem);
               }
            #endif
            if (ee > eem) { eem = ee; cout[i] = cs->keys[k]; }
         }
      }
   }
   
   return;
}
 
double SUMA_EdgeEnergy_OptimCost(int n, double *par) 
{
   static int iter;
   double cost;
   
   /* Assign classes based on Gaussians */
   SUMA_EdgeEnergy_Gassign(eeoud.aset, eeoud.fset,
                        eeoud.cmask, eeoud.cs, 
                        par, n, eeoud.c);
   
   /* call energy function */
   cost = -1.0 * SUMA_DsetEdgeEnergy(eeoud.aset,
                       eeoud.c,
                       eeoud.cmask, 
                       eeoud.fset,
                       eeoud.cs);
   
   /* fprintf(stderr,"%d, %f\n", iter, cost); */ 
   ++iter; 
   return(cost);
}

double SUMA_MAP_EdgeEnergy(THD_3dim_dataset *aset, byte *cmask, 
                        THD_3dim_dataset *fset, SUMA_CLASS_STAT *cs, 
                        THD_3dim_dataset *cset, SEG_OPTS * Opt, 
                        short *thiscmap)
{
   static char FuncName[]={"SUMA_MAP_EdgeEnergy"};
   int ncalls = 0, nparmax = 36, npar, i, nrand, ntry, nkeep, maxcall;
   double par[nparmax], bot[nparmax], top[nparmax], cost,
          gap[nparmax], rstart, rend;
   static int icall = 0;
   SUMA_Boolean LocalHead = YUP;   
   
   SUMA_ENTRY;
   
   /* load parameters into par, bot, top */
   if (cs->N_label*2 > nparmax) {
      SUMA_S_Err("Too many parameters");
      SUMA_RETURN(cost);
   }
   
   /* initialize parameters and estimate gap between classes.
   For 1st and last class, assume gap is one stdv */
   npar = cs->N_label*2;
   for (i=0; i<cs->N_label; ++i) {
      par[2*i  ] = SUMA_get_Stat(cs, cs->label[i], "mean");
      par[2*i+1] = SUMA_get_Stat(cs, cs->label[i], "stdv");
      if (i>0) {
         gap[i] = par[2*i  ] - par[2*(i-1)]; /* mean difference 
                                                     between classes i and i-1 */
         if (gap[i] <= 0) {
            SUMA_S_Err("Classes not sorted by increasing mean");
            SUMA_RETURN(0.0);
         }
      } else {
         gap[0] = SUMA_get_Stat(cs, cs->label[0], "stdv")*2.0; 
                     /* Can't compute mean difference, 
                      allow something function of stdv */
      }
   }
   gap[cs->N_label] = SUMA_get_Stat(cs, cs->label[cs->N_label-1], "stdv")*2.0; 
   
   /* min and max params for mean parameters */
   for (i=0; i<cs->N_label; ++i) {
      if (i==0)   bot[2*i] = SUMA_MAX_PAIR(par[2*i] - gap[i]/10.0, 0.8*par[2*i]);
      else        bot[2*i] = par[2*i] - gap[i]/10.0;
      if (i==cs->N_label-1) 
                  top[2*i] = SUMA_MIN_PAIR(par[2*i]+gap[i+1]/10.0, 1.2*par[2*i]);
      else        top[2*i] = par[2*i] + gap[i+1]/10.0;  
   }
   /* min and max params for stdv parameters */
   for (i=0; i<cs->N_label; ++i) {
      bot[2*i+1] = 0.8*par[2*i+1];
      top[2*i+1] = 1.2*par[2*i+1];
   }
   
   if (LocalHead) {
      for (i=0; i<cs->N_label; ++i) {
         fprintf(SUMA_STDERR,
            "Pre Optimization:\n"
            "%3s:  mean [%.3f <- %.3f -> %.3f], stdv [%.3f <- %.3f -> %.3f]\n",
                  cs->label[i],  bot[2*i  ], par[2*i  ], top[2*i  ],
                                 bot[2*i+1], par[2*i+1], top[2*i+1]);
      }
      {
        THD_3dim_dataset *pout=NULL; short *cc=NULL;
        NEW_SHORTY(cset, 1, FuncName, pout);
        cc=(short*)DSET_ARRAY(pout,0);
         /* find out what things are like before optimization */
         SUMA_EdgeEnergy_Gassign(Opt->aset, Opt->fset,
                           Opt->cmask, Opt->cs, 
                           par, npar, cc);
        SUMA_SEG_WRITE_DSET("PreEdgeOptim", pout, icall, Opt->hist);
        DSET_delete(pout);
      }
   }
   
   /* load user data */
   SUMA_set_eeoud(Opt->cs, Opt->aset, Opt->fset, Opt->cmask);
   nrand = 0; nkeep = 0; ntry = 2;
   rstart = 0.2; rend = 0.05;
   maxcall = 50;
   if ( (ncalls = powell_newuoa_constrained (npar, par, &cost,
                                             bot, top, 
                                             nrand, nkeep, 2,
                                             rstart, rend,
                                             maxcall,
                                    SUMA_EdgeEnergy_OptimCost)) < 0) {
      SUMA_S_Err("Failed in optimization");
      SUMA_RETURN(0);
   }
   
   if (LocalHead) {
      for (i=0; i<cs->N_label; ++i) {
         fprintf(SUMA_STDERR,
                  "Post Optimization:\n"
                  "%s:  mean [%f <-- %f --> %f], stdv [%f <-- %f --> %f]\n",
                  cs->label[i],  bot[2*i  ], par[2*i  ], top[2*i  ],
                                 bot[2*i+1], par[2*i+1], top[2*i+1]);
      }
      /* write out the classification result based on EdgeEnergy Optimization*/
      {
        THD_3dim_dataset *pout=NULL; short *cc=NULL;
        NEW_SHORTY(cset, 1, FuncName, pout);
        cc=(short *)DSET_ARRAY(pout,0);
        memcpy(cc, eeoud.c, sizeof(short)*DSET_NVOX(cset));
        SUMA_SEG_WRITE_DSET("PostEdgeOptim", pout, icall, Opt->hist);
        DSET_delete(pout);
      }
   }
   
   /* reload optimized params into cs */
   for (i=0; i<cs->N_label; ++i) {
      SUMA_set_Stat(cs, cs->label[i], "mean", par[2*i  ]);
      SUMA_set_Stat(cs, cs->label[i], "stdv", par[2*i+1]);
   }
   if (thiscmap) {
      memcpy(thiscmap, eeoud.c, sizeof(short)*DSET_NVOX(cset));
   }
   SUMA_free_eeoud();
   
   ++icall;
   SUMA_RETURN(cost);
}

/* See Berthod et al. */
int SUMA_MAP_labels(THD_3dim_dataset *aset, 
                        byte *cmask, 
                        SUMA_CLASS_STAT *cs, int neighopt, 
                        THD_3dim_dataset **csetp, 
                        THD_3dim_dataset **pCgNp,
                        SEG_OPTS *Opt)
{
   static char FuncName[]={"SUMA_MAP_labels"};
   THD_3dim_dataset *cset = *csetp;
   THD_3dim_dataset *pCgN = *pCgNp;
   float af=0.0, *fpCgN=NULL;
   int iter=0, i=0, k, ni, nj, nk, nij, ijkn[6];
   int Niter = 3, kmin;
   double eG[Niter], e, eG1, eG2, *mv, *sv, dd, *e1=NULL, *e2=NULL, BoT, pp;
   short *ci=NULL, *co=NULL, *a=NULL;
   
   SUMA_ENTRY;
   
   BoT = Opt->B/Opt->T;
   
   if (neighopt != 4 && neighopt != 6) {
      SUMA_S_Errv("Allowing neighopt of 4 or 6 only. Have %d\n", neighopt);
      SUMA_RETURN(0);
   } 
    
   if (!cset) {
      NEW_SHORTY(aset,1,"MAP_labels",cset);
      *csetp = cset;
   }
   
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0f) af = 1.0;
   a = (short *)DSET_ARRAY(aset,0);
   co = (short *)DSET_ARRAY(cset,0);
   ci = (short *)malloc(sizeof(short)*DSET_NVOX(aset));
   e1 = (double *)calloc(cs->N_label, sizeof(double));
   e2 = (double *)calloc(cs->N_label, sizeof(double));
   fpCgN = (float *)calloc(cs->N_label, sizeof(float));
   
   if (!pCgN) {
      NEW_SHORTY(aset,cs->N_label, "pCgN", pCgN);
      *pCgNp = pCgN;
      for (k=0; k<cs->N_label; ++k) {
         EDIT_BRICK_FACTOR(pCgN, k, 1/10000.0);
      }
   }
   GET_BFs(pCgN,fpCgN);

   ni = DSET_NX(aset);
   nj = DSET_NY(aset);
   nk = DSET_NZ(aset);
   nij = ni*nj;

   /* get vector of parameters  */
   mv = SUMA_get_Stats(cs, "mean");
   sv = SUMA_get_Stats(cs, "stdv");

   for (iter=0; iter<Niter; ++iter) {
      memcpy(ci, co, sizeof(short)*DSET_NVOX(aset));
      eG[iter] = 0.0; eG1=0.0; eG2 = 0.0;
      for (i=0; i<DSET_NVOX(aset); ++i) {
         if (IN_MASK(cmask,i)) {
            for(k=0; k<cs->N_label; ++k) {
               /* term 1, energy of y given class */
               dd = ((double)a[i]*af-mv[k]); dd *= dd; 
               e1[k] = (dd/(2.0*sv[k]*sv[k]) + log(sv[k]*SQ2PI ))/Opt->T; 

               /* term 2, energy of label given neighbors */
               GET_NEIGHBS_IN_MASK( cmask, i, 
                                    ni, nj, nk, nij, 
                                    ijkn);
               E_l_GIV_NEIGHBS(ci, ijkn, cs->keys[k], neighopt, e2[k]);
               e2[k] = e2[k]*BoT;
            }
            /* find min e */
            e = e1[0]+e2[0]; kmin=0; 
            for(k=1; k<cs->N_label; ++k) { 
               if (e1[k]+e2[k] < e) { 
                  e=e1[k]+e2[k]; kmin=k; 
               } 
            }
            
            if (i == Opt->VoxDbg) {
               int IJK[3], pp;
               Vox1D2Vox3D(i,DSET_NX(aset), DSET_NX(aset)*DSET_NY(aset), IJK);
               fprintf(Opt->VoxDbgOut, "at %d %d %d, a=%d (%f)\n", 
                                       IJK[0], IJK[1], IJK[2],
                                       a[i], a[i]*af);
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
                  if (k!=kmin) fprintf(Opt->VoxDbgOut, "%f   ", e1[k]+e2[k]);
                  else fprintf(Opt->VoxDbgOut, "%f*  ", e1[k]+e2[k]);
               }
               fprintf(Opt->VoxDbgOut, "\n\n");
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
            /* store the prob. for each class p(c|Neighb) */
            pp = 0; 
            for (k=0; k<cs->N_label; ++k) { 
               e1[k] = exp(-e1[k]); /* now e1 is a prob. * scaling factor*/
               pp += e1[k];
            }
            for (k=0; k<cs->N_label; ++k) {
               PSCVAL(pCgN,k, i, fpCgN[k], e1[k]/pp);
            }
         } else {
            co[i] = 0;
         }
      }/* for i */
      if (Opt->debug > 1) SUMA_S_Notev("Iter %d, e=%f\n", iter, eG[iter]);
   } /* for iter */
   
      
   SUMA_ifree(ci);   SUMA_ifree(fpCgN);
   SUMA_RETURN(1);
}

/*!
   Posterior distribution of class given the whole enchilada
   
*/
int SUMA_pst_C_giv_ALL(THD_3dim_dataset *aset,  
                                 byte *cmask, int cmask_count,
                                 SUMA_CLASS_STAT *cs,
                                 THD_3dim_dataset *pC, THD_3dim_dataset *pCgN,
                                 THD_3dim_dataset **pcgallp, SEG_OPTS *Opt)
{
   static char FuncName[]={"SUMA_pst_C_giv_ALL"};
   short *a=NULL;
   double *p=NULL, *m=NULL, *s=NULL,  *gd, *ds2, *ps=NULL, 
            sp, BoT, x0, e, PP[64], PG[64], *w=NULL, eN=0.0, pp, pg;
   float af, fpCw, fpC, *fpCgN=NULL;
   int i, k, ni, nj, nk, nij, ijkn[6], shft;
   THD_3dim_dataset *pout = *pcgallp;
   THD_3dim_dataset *pCw=NULL;
   char sbuf[256];
   static int icall=0;
   
   SUMA_ENTRY;
   
   if (!pout) {
      NEW_SHORTY(aset,cs->N_label,"SUMA_pst_C_giv_ALL",pout);
      *pcgallp = pout;
   }
   BoT=Opt->B/Opt->T;
   
   ni = DSET_NX(aset);
   nj = DSET_NY(aset);
   nk = DSET_NZ(aset);
   nij = ni*nj;
   
   af = DSET_BRICK_FACTOR(aset,0); if (af == 0.0f) af = 1.0;
   a = (short *)DSET_ARRAY(aset,0);
   p = (double *)SUMA_calloc(cs->N_label*DSET_NVOX(aset), sizeof(double));
   ps = (double *)SUMA_calloc(DSET_NVOX(aset), sizeof(double));
   m = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   s = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   ds2 = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   gd = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   fpCgN = (float*)SUMA_calloc(cs->N_label, sizeof(float));
    
   if (pCgN) {
      GET_BFs(pCgN, fpCgN);
   }

   /* get the global (average) mixing fraction */
   w = SUMA_get_Stats(cs, "mix");

   /* prepare the voxelwise mixing fraction denominator */
   fpCw = 1/10000.0;
   NEW_SHORTY(aset,cs->N_label,"pCw",pCw); 
   for (k=0; k<cs->N_label; ++k) {
      EDIT_BRICK_FACTOR(pCw,k, fpCw);   
   }
   if (pC) {
      fpC = DSET_BRICK_FACTOR(pC, 0);
      for (i=0; i<DSET_NVOX(aset); ++i) {
         sp = 0.0;
         for (k=0; k<cs->N_label; ++k) {
            if (IN_MASK(cmask, i)) {
               GSCVAL(pC, k, i, fpC, e);
               PP[k] = w[k]*e;
               if (pCgN) {
                  GSCVAL(pCgN, k, i, fpCgN[k], eN);
                  PP[k] *= eN;
               } 
               sp += PP[k];
            }
         }
         for (k=0; k<cs->N_label; ++k) {
            if (IN_MASK(cmask, i)) {
               PSCVAL(pCw,k,i,fpCw, PP[k]/sp);
            }
         }
      }
   } else {
      /* Just use the global mixing fractions */
      for (k=0; k<cs->N_label; ++k) {
         for (i=0; i<DSET_NVOX(aset); ++i) {
            if (IN_MASK(cmask, i)) {
               if (pCgN) {
                  GSCVAL(pCgN, k, i, fpCgN[k], eN);
                  PSCVAL(pCw,k,i,fpCw, w[k]*eN);
               } else {
                  PSCVAL(pCw,k,i,fpCw, w[k]);
               }
            }
         }
      }
   }
   
   
   /* Get class stats */
   for (k=0; k<cs->N_label; ++k) {
      m[k]=SUMA_get_Stat(cs, cs->label[k], "mean");
      s[k]=SUMA_get_Stat(cs, cs->label[k], "stdv");
      ds2[k] = 2.0*s[k]*s[k];
      gd[k] = 1.0/(SQ2PI*s[k]); 
   }
   
   fpCw = DSET_BRICK_FACTOR(pCw, 0);
   for (k=0; k<cs->N_label; ++k) {
      shft = k*DSET_NVOX(aset); PP[k]=0; PG[k]=0; 
      for (i=0; i<DSET_NVOX(aset); ++i) {
         if (IN_MASK(cmask, i)) {
            x0 = (double)a[i]*af - m[k];
            pg = exp(-(x0*x0)/ds2[k])*gd[k];
            
            GSCVAL(pCw, k, i, fpCw, pp); 
           
            p[i+shft] = (pg)*(pp);
            ps[i] += p[i+shft];
               if (i == Opt->VoxDbg) { /* store for debugging */
                  PP[k] = pp;
                  PG[k] = pg;
               }
         }
      }
   }
   
   /* and marginalize */
   for (k=0; k<cs->N_label; ++k) {
      shft = k*DSET_NVOX(aset);
      for (i=0; i<DSET_NVOX(aset); ++i) {
         if (IN_MASK(cmask, i)) {
            p[i+shft] /= ps[i];
         }
      }     
   }
   SUMA_ifree(ps); SUMA_ifree(fpCgN);
   
             
      if (Opt->VoxDbg >= 0) {
         int IJK[3], pp;
         i = Opt->VoxDbg;
         Vox1D2Vox3D(i,DSET_NX(aset), DSET_NX(aset)*DSET_NY(aset), IJK);
         fprintf(Opt->VoxDbgOut, "at %d %d %d, a=%d (%f)\n", 
                                 IJK[0], IJK[1], IJK[2],
                                 a[i], a[i]*af);
         fprintf(Opt->VoxDbgOut, "p(y|params)[]:   ");
         for(k=0; k<cs->N_label; ++k) {
            fprintf(Opt->VoxDbgOut, "%f   ", PG[k]);
         }
         fprintf(Opt->VoxDbgOut, "\n");
         fprintf(Opt->VoxDbgOut, "w[]:   ");
         for(k=0; k<cs->N_label; ++k) {
            fprintf(Opt->VoxDbgOut, "%f   ", PP[k]);
         }
         fprintf(Opt->VoxDbgOut, "\n\n");
      }   
   
   /* put vector back in pout */
   for (k=0; k<cs->N_label; ++k) {
      EDIT_substscale_brick(pout, k, MRI_double, 
                            (p+k*DSET_NVOX(pout)), MRI_short, -1.0);
      sprintf(sbuf,"P(%s|y)",cs->label[k]);
      EDIT_BRICK_LABEL( pout, k, sbuf);
   }
     
   SUMA_ifree(p);  
   SUMA_ifree(m);  
   SUMA_ifree(s);  
   SUMA_ifree(ds2);  
   SUMA_ifree(gd);  
   DSET_delete(pCw); pCw = NULL;
   
   ++icall;   
   SUMA_RETURN(1);
}

int SUMA_SegInitCset(THD_3dim_dataset *aseti, 
                     THD_3dim_dataset **csetp, 
                     byte *cmask, int cmask_count,
                     char *mixopt, 
                     SUMA_CLASS_STAT *cs,
                     SEG_OPTS *Opt)
{
   static char FuncName[]={"SUMA_SegInitCset"};   
   int ibias = 0, border;
   short *a=NULL;
   double *p=NULL, *m=NULL, *s=NULL,  *gd, *ds2, *ps=NULL, 
            sp,  x0, e, PP[64], PG[64],  pp, pg;
   float af;
   int i, k, shft;
   char sbuf[256];
   THD_3dim_dataset *cset=*csetp, *pstC=NULL, *cset_init=NULL;
   OPT_KMEANS oc;
         
   SUMA_ENTRY;
   
   oc = new_kmeans_oc();
   oc.k = SUMA_Class_k_Selector(cs,
                                "not_classes_string", "OTHER",NULL);
   oc.remap = MAG;
   oc.verb = 0;
   oc.distmetric = 'e';
   for (i=0; i<oc.k; ++i) oc.clabels[i] = cs->label[i];
   oc.jobname=SUMA_copy_string(FuncName);
   if (!cset) {
      /* Let clustering do the whole deal*/
      oc.r = 3;
      cset_init = NULL;
   } else {
      oc.r = 0;
      /* initialize by cset */
      cset_init = cset;
   }
   
   if (!(thd_Acluster1 (aseti,
               cmask, cmask_count,
               &cset,
               NULL,
               cset_init,
               oc))) {
      SUMA_S_Err("Failed to cluster");
      SUMA_RETURN(0);           
   }   
   if (!cset_init) { /* happens when cset is NULL before thd_Acluster1 call */
      *csetp = cset;
   }
   
   /* compute class stats */
   if (!SUMA_Class_stats( aseti, cset, cmask, cmask_count, 
                           NULL, NULL, Opt->gold, cs)) {
      SUMA_S_Err("Failed in class stats");
      SUMA_RETURN(0);
   }

   af = DSET_BRICK_FACTOR(aseti,0); if (af == 0.0f) af = 1.0;
   a = (short *)DSET_ARRAY(aseti,0);
   p = (double *)SUMA_calloc(cs->N_label*DSET_NVOX(aseti), sizeof(double));
   ps = (double *)SUMA_calloc(DSET_NVOX(aseti), sizeof(double));
   m = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   s = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   ds2 = (double *)SUMA_calloc(cs->N_label, sizeof(double));
   gd = (double *)SUMA_calloc(cs->N_label, sizeof(double));

   /* Compute pst(c) = p(y|stats) * mixrac */
   NEW_SHORTY(aseti, cs->N_label, "ini.pstC", pstC);
   for (k=0; k<cs->N_label; ++k) {
      m[k]=SUMA_get_Stat(cs, cs->label[k], "mean");
      s[k]=SUMA_get_Stat(cs, cs->label[k], "stdv");
      ds2[k] = 2.0*s[k]*s[k];
      gd[k] = 1.0/(SQ2PI*s[k]); 
   }
   
   for (k=0; k<cs->N_label; ++k) {
      if (mixopt) {
         pp = SUMA_mixopt_2_mixfrac(mixopt, cs->label[k], 
                             cs->keys[k], cs->N_label,
                             cmask, cset);
         if (pp < 0.0) {
            SUMA_S_Err("Failed to get mixfrac");
            SUMA_RETURN(0);
         }
      } else {
         pp = 1.0/cs->N_label;
      }           
      shft = k*DSET_NVOX(aseti); PP[k]=0; PG[k]=0; 
      for (i=0; i<DSET_NVOX(aseti); ++i) {
         if (IN_MASK(cmask, i)) {
            x0 = (double)a[i]*af - m[k];
            pg = exp(-(x0*x0)/ds2[k])*gd[k];
            p[i+shft] = (pg)*(pp);
            ps[i] += p[i+shft];
               if (i == Opt->VoxDbg) { /* store for debugging */
                  PP[k] = pp;
                  PG[k] = pg;
               }
         }
      }
   }
   
   /* and marginalize */
   for (k=0; k<cs->N_label; ++k) {
      shft = k*DSET_NVOX(aseti);
      for (i=0; i<DSET_NVOX(aseti); ++i) {
         if (IN_MASK(cmask, i)) {
            p[i+shft] /= ps[i];
         }
      }     
   }
   SUMA_ifree(ps); 
   
   /* store p in dset */
   for (k=0; k<cs->N_label; ++k) {
      EDIT_substscale_brick( pstC, k, MRI_double, 
                            (p+k*DSET_NVOX(pstC)), MRI_short, -1.0);
      sprintf(sbuf,"Pinit(%s|y)",cs->label[k]);
      EDIT_BRICK_LABEL( pstC, k, sbuf);
   }

   /* reassign class membership */
   if (!(SUMA_assign_classes( pstC, cs, 
                              cmask, &cset))) { 
      SUMA_S_Err("Failed in assign_classes");
      SUMA_RETURN(0);
   }
   
   /* recompute class stats using this posterior */
   if (!SUMA_Class_stats( aseti, cset, cmask, cmask_count, 
                          pstC, NULL, Opt->gold, cs)) {
      SUMA_S_Err("Failed in class stats");
      SUMA_RETURN(0);
   }

   /* free temps */
   SUMA_ifree(p);  
   SUMA_ifree(m);  
   SUMA_ifree(s);  
   SUMA_ifree(ds2);  
   SUMA_ifree(gd);  
   DSET_delete(pstC); pstC = NULL;
   
   SUMA_RETURN(1); 
}

float SUMA_GetConstFactor(THD_3dim_dataset *pset) {
   int k;
   for (k=1; k<DSET_NVALS(pset); ++k) {
      if (SUMA_ABS(DSET_BRICK_FACTOR(pset,k)-DSET_BRICK_FACTOR(pset,k-1))
               > 0.000001) {
         return(-1.0);
      }
   }
   
   return(DSET_BRICK_FACTOR(pset,0));
}

/*!
   Take an input dset and make sure it is formatted 
   as a shortized probability dset with equal scaling
   factors.
   If the input is a single sub-brick of class indices,
   it creates an binarized prob. dset
*/
int SUMA_ShortizeProbDset(THD_3dim_dataset **csetp, 
                        SUMA_CLASS_STAT *cs, 
                        byte *cmask, int cmask_count, 
                        SEG_OPTS *Opt, 
                        THD_3dim_dataset **psetp)
{
   static char FuncName[]={"SUMA_ClassToProbDset"};
   THD_3dim_dataset *pset = NULL;
   THD_3dim_dataset *cset = *csetp;
   float fpset;
   byte *bb=NULL, shortize=0;
   short *gb=NULL, *C=NULL; 
   int ijk=0, k;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   fpset = 1.0/10000.0;
   
   if (!psetp) {
      SUMA_S_Err("NULL psetp");
      SUMA_RETURN(0);
   }
   pset = *psetp;
   
   if (DSET_NVALS(cset) == 1) { /* gave me classes, make probabilities */
      SUMA_LH("Changing classes to probabilities");
      if (!pset || pset == cset) NEW_SHORTY(cset, cs->N_label, "prior_p", pset);
      if (DSET_NVALS(pset)!=cs->N_label) {
         SUMA_S_Errv("Bad input, have %d vals in pset, need %d\n",
            DSET_NVALS(pset), cs->N_label);
         SUMA_RETURN(0);
      }

      NEW_SHORTY(cset, cs->N_label, "prior_p", pset);
      C = (short *)DSET_ARRAY(cset,0);
      bb = (byte *)SUMA_calloc(DSET_NVOX(cset), sizeof(byte));
      for (k=0; k<cs->N_label; ++k) {
         gb = (short *)DSET_ARRAY(pset,k);
         for (ijk=0; ijk<DSET_NVOX(cset); ++ijk) {
            if (IN_MASK(cmask,ijk) && C[ijk] == cs->keys[k]) {
               gb[ijk] = (short )1.0/fpset; bb[ijk] = 1;
            }
         }
         EDIT_BRICK_FACTOR(pset, k, fpset);
      }
      /* make sure each voxel in mask got something */
      for (k=0; k<cs->N_label; ++k) {
         gb = (short *)DSET_ARRAY(pset,k);
         for (ijk=0; ijk<DSET_NVOX(cset); ++ijk) {
            if (IN_MASK(cmask,ijk) && !bb[ijk]) {
               gb[ijk] = (short )(1.0/fpset/cs->N_label);
            }
         }
      }
      SUMA_ifree(bb); 
   } else {
      /* cset is considered to be a probs. dset */
      pset = cset; *csetp = NULL; /* to guard against multiple copies */
      /* make sure you don't get a crappy number of sub-bricks */
      if (DSET_NVALS(pset) != cs->N_label) {
         SUMA_S_Err("Bad news in tennis shoes");
         SUMA_RETURN(0);
      }
   }
   
   shortize = 0;
   if (SUMA_GetConstFactor(pset) < 0.0) {  shortize = 1; }
   else {
      for (k=0; k<cs->N_label; ++k) {
         if (DSET_BRICK_TYPE(pset,k) != MRI_short) {
            shortize = 1;
            break;
         }
      }
   }
   
   if (shortize) {
      SUMA_LHv("Shortizing %s\n", DSET_PREFIX(pset));
      if (!SUMA_ShortizeDset(&pset, fpset)) {
         SUMA_S_Err("Failed to shortize");
         SUMA_RETURN(0);
      }
   }

   *psetp=pset;
   SUMA_RETURN(1);
}  

int SUMA_FlattenProb(THD_3dim_dataset *pC, 
                     byte *cmask, int cmask_count, 
                     int mode) 
{
   static char FuncName[]={"SUMA_FlattenProb"};
   int i, k, nbrick=DSET_NVALS(pC);
   double ss, pp;
   float fpC[nbrick];
   
   SUMA_ENTRY;
   
   GET_BFs(pC,fpC);
   
   switch (mode) {
      case 1:
         for (i=0; i<DSET_NVOX(pC); ++i) {
            if (1 || IN_MASK(cmask,i)) { /* focusing on mask only gives ugly 
                                       edge artifacts, they won't bother with 
                                       segmentation, but they're ugly */
               ss = 0;
               for (k=0; k<nbrick; ++k) {
                  GSCVAL(pC,k,i,fpC[k],pp);
                  ss += pp;
               }
               ss = ss/(double)(nbrick);
               for (k=0; k<nbrick; ++k) {
                  PSCVAL(pC,k,i,fpC[k], ss);
               }
            }
         }
         break;
      default:
         SUMA_S_Err("Not ready for this mode");
         SUMA_RETURN(0);
   }
   
   SUMA_SEG_WRITE_DSET("FLAT", pC, -1, NULL); 

   SUMA_RETURN(1);
}

/*!
   Given a prob. dset, add an 'OTHER' class
*/
int SUMA_OtherizeProbDset(THD_3dim_dataset *pC, 
                          byte *cmask, int cmask_count)
{
   static char FuncName[]={"SUMA_OtherizeProbDset"};
   int i, k, nbrick = DSET_NVALS(pC);
   double ss, pp;
   short *cc=NULL;
   float fpC[nbrick+1];
   
   SUMA_ENTRY;
   
   GET_BFs(pC,fpC);
   fpC[nbrick] = 1/10000.0;
   
   cc = (short *)calloc(DSET_NVOX(pC), sizeof(short));
   EDIT_add_brick(pC, MRI_short, fpC[nbrick], cc);
   EDIT_BRICK_LABEL(pC, nbrick, "OTHER");
   for (i=0; i<DSET_NVOX(pC); ++i) {
      if (1 || IN_MASK(cmask,i)) { /* no need to stick to mask here */
         ss = 0;
         for (k=0; k<nbrick; ++k) {
            GSCVAL(pC,k,i,fpC[k],pp);
            ss += pp;
            if (i==1332180) {
               fprintf(stderr,"%d: %f --> %f\n", k, pp, ss);
            }
         }
         ss = (1.0 - SUMA_MIN_PAIR(ss, 1.0));
         PSCVAL(pC, nbrick, i, fpC[nbrick], ss);
         if (i==1332180) {
               GSCVAL(pC,k,i,fpC[nbrick],pp);
               fprintf(stderr,"%d:  --> %f (%f)\n", nbrick,  ss, pp);
            }
      }
   }
   SUMA_RETURN(1);
}

int SUMA_AddOther(  NI_str_array *clss, int **keysp, 
                    byte *cmask, int cmask_count,
                    THD_3dim_dataset *cset, THD_3dim_dataset *pstCgALL,
                    THD_3dim_dataset *pCgA, THD_3dim_dataset *pCgL,
                    SUMA_CLASS_STAT *cs)
{      
   static char FuncName[]={"SUMA_AddOther"};
   int i, mxkey=0;
   int *keys=*keysp;
   short *cc=NULL;
   float fpG;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   SUMA_S_Warn("Adding OTHER CLASS");
   /* clss */
   clss->num = clss->num+1;
   clss->str = 
      NI_realloc(clss->str, char *, sizeof(char *)*(clss->num));
   clss->str[clss->num-1] = NI_malloc(char, strlen("OTHER")+1);
   strcpy(clss->str[clss->num-1], "OTHER");

   /* keys */
   mxkey = keys[0];
   for (i=1; i<clss->num-1; ++i) {
      if (mxkey < keys[i]) mxkey = keys[i];
   }
   keys = (int *)SUMA_realloc(keys, sizeof(int)*clss->num);
   keys[clss->num-1] = mxkey+1;
   *keysp = keys;
   
   /* cset ? */
   if (cset) {
      cc = DSET_ARRAY(cset,0);
      for (i=0; i<DSET_NVOX(cset); ++i) {
         if (IN_MASK(cmask,i)) {
            if (!cc[i]) cc[i] = mxkey+1;
         }
      }
   }

   /* pstCgALL ? */
   if (pstCgALL) {
      fpG = DSET_BRICK_FACTOR(pstCgALL,0);
      EDIT_add_brick(pstCgALL, MRI_short, fpG, NULL);
      EDIT_substitute_brick( pstCgALL, DSET_NVALS(pstCgALL)-1, 
                              MRI_short, NULL ) ;
      EDIT_BRICK_LABEL(pstCgALL, DSET_NVALS(pstCgALL)-1, "OTHER");
   }

   if (pCgA) {
      if (!SUMA_OtherizeProbDset(pCgA, 
                        cmask, cmask_count)) {
         SUMA_S_Errv("Failed to otherize pCgA %s\n", DSET_PREFIX(pCgA));
         SUMA_RETURN(0);
      }
      if (LocalHead) SUMA_SEG_WRITE_DSET("pCgA-Otherized", pCgA, -1, NULL);
   }
   if (pCgL) {
      if (!SUMA_OtherizeProbDset(pCgL, 
                        cmask, cmask_count)) {
         SUMA_S_Errv("Failed to otherize pCgL %s\n", DSET_PREFIX(pCgL));
         SUMA_RETURN(0);
      }
      if (LocalHead) SUMA_SEG_WRITE_DSET("pCgL-Otherized", pCgL, -1, NULL);
   }

   /* cs ? */
   if (cs) {
      if (SUMA_Add_Class_Label(cs, "OTHER", keys[clss->num-1]) < 0) {
         SUMA_S_Err("Failed to SUMA_Add_Class_Label OTHER");
         SUMA_RETURN(0);
      }
   }
      
   SUMA_RETURN(1);      
}

/*!
   Initialize all voxels in a dset.
   \param aset: Dataset to initialize 
   \param val: Vector of values, one for each sub-brick
               or just one value for all sub-bricks
   \param nval: Either 1 or DSET_NVALS(aset)
   \param cmask
   \param setsf: if 1, then set the scaling factor based on vv
                for dsets that are shorts
   \return 0 bad, 1 good
   
*/
int SUMA_InitDset(THD_3dim_dataset  *aset, float *val, int nval,
                  byte *cmask, byte setsf)
{
   static char FuncName[]={"SUMA_InitDset"};
   int i, k;
   float vv, *fv, fsc;
   
   SUMA_ENTRY;
   
   for (k=0; k<DSET_NVALS(aset); ++k) {
      if (!val) vv = 0.0;
      else if (nval > 1) vv = val[k];
      else vv = *val; 
      fsc = DSET_BRICK_FACTOR(aset,k);
      if (fsc == 0.0) fsc = 1.0;
      switch (DSET_BRICK_TYPE(aset,k)) {
         case MRI_float:
            fv = (float *)DSET_ARRAY(aset,k);
            for (i=0; i<DSET_NVOX(aset); ++i) {
               if (IN_MASK(cmask,i)) {
                  fv[i] = vv;
               }
            }
            break;
         case MRI_short:
            if (setsf) {
               if (vv != 0.0) fsc = 32767.0/vv;
               EDIT_BRICK_FACTOR(aset,k,fsc);
            }
            for (i=0; i<DSET_NVOX(aset); ++i) {
               if (IN_MASK(cmask,i)) {
                  PSCVAL(aset, k, i, fsc, vv);
               }
            }
            break; 
         default:
            SUMA_S_Errv("Not dealing with type %d\n",
                        DSET_BRICK_TYPE(aset,k)); 
            SUMA_RETURN(0);
      }
   }
   
   SUMA_RETURN(1);
   
}

/*!
   Produce Class priors:
      \param cs : Class stats from which mixing fractions will be used
                  if no Location- or Spatial-based priors are provided
      \param Aset: The anatomical dset, for a grid template
      \param priCgA: Prior of C given A (feature)
      \param wA: Weight of feature priors 
      \param priCgL: Prior of C given L (location)
      \param wL: Weight of Location priors
         wA + wL = 1.0
      \param priCgALLp: Pointer to dset pointer where results will be placed
                        upon the function's return. If *priCgALLp == NULL,
                        a new dset is created, otherwise dset is recycled.
      \param Opt: The catchall structure of options, just for debugging params.
      \return :0 Bad, 1 good
    
    If (priCgA and priCgL) {
      For all k in classes != OTHER
         priCgALLp[k] = (0.05+0.85*priCgA[k])^wA * (0.05+0.85*priCgL[k])^wL / 
                           SUM(priCgALLp[k!=OTHER])
            (0.05+0.85*p) is to keep one prior from nulling another.
      For k == OTHER
         priCgALLp[OTHER] = priCgL[OTHER];
         (and readjust priCgALLp[k!=OTHER] so that SUM(priCgALLp[.]) = 1.0
   } else if (priCgA) {
      priCgALLp[k] = priCgA[k]
   } else if (priCgL) {
      priCgALLp[k] = priCgL[k]
   } else {
      priCgALLp[k] = cs->mix[k] (constant at all voxels)
   }
         
*/
int SUMA_MergeCpriors(SUMA_CLASS_STAT *cs, byte *cmask,
                                      THD_3dim_dataset  *Aset,
                                      THD_3dim_dataset  *priCgA, float wA,
                                      THD_3dim_dataset  *priCgL, float wL,
                                      THD_3dim_dataset  **priCgALLp,
                                      SEG_OPTS *Opt)
{
   static char FuncName[]={"SUMA_MergeCpriors"};
   float *fPCGA = NULL, *fPCGL=NULL, *fpriCgALL=NULL;
   int k, ijk;
   double sdf, df, dfA, dfL, *ggkk=NULL,*mixfrac=NULL;
   THD_3dim_dataset  *priCgALL = *priCgALLp;
   
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   /* merge priors */
   if (!priCgALL) {
      NEW_SHORTY(Aset, cs->N_label, "InitialPriors", priCgALL);
      *priCgALLp = priCgALL;
   } 
   
   fpriCgALL = (float *)SUMA_calloc(cs->N_label, sizeof(float));   
   fPCGA = (float *)SUMA_calloc(cs->N_label, sizeof(float));   
   fPCGL = (float *)SUMA_calloc(cs->N_label, sizeof(float));   
   ggkk   = (double*)SUMA_calloc(cs->N_label, sizeof(double));   
   
   for (k=0; k<cs->N_label; ++k) EDIT_BRICK_FACTOR(priCgALL, k, 1.0/10000.0);
   GET_BFs(priCgALL,fpriCgALL);

   
   if (priCgA) {
      GET_BFs(priCgA,fPCGA);
   }
   if (priCgL) {
      GET_BFs(priCgL, fPCGL); 
   }
   if (priCgA && priCgL) { /* have both types of priors, combine */
       double dfA, dfL;
       int UseK[cs->N_label], N_kok, uk, ko;
       
       if (wA == -1.0) {
         wA = 0.8; wL = 0.2;
       }
       /* find which classes can be merged */
       SUMA_LHv("Mixing %f priCgA & %f priCgL\n",
               wA, wL);
       if ((N_kok = SUMA_Class_k_Selector(cs, 
            "not_classes_string", "OTHER",  UseK))<0) {
         SUMA_S_Err("No classes found"); SUMA_RETURN(0);
       }
       for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
         if (IN_MASK(cmask, ijk)) {
            sdf = 0.0;
            for (uk=0; uk<N_kok; ++uk) {
               k = UseK[uk];
               GSCVAL(priCgA, k, ijk, fPCGA[k], dfA);
               GSCVAL(priCgL, k, ijk, fPCGL[k], dfL);
               /* pad dfA and dfL to guard against multiplying with 0 */
               dfA = 0.05+0.85*dfA; 
               dfL = 0.05+0.85*dfL; 
               ggkk[k] = pow(dfA,wA)*pow(dfL,wL);
               sdf+=ggkk[k];
            }
            for (uk=0; uk<N_kok; ++uk) {
               k = UseK[uk];
               PSCVAL(priCgALL, k, ijk, fpriCgALL[k], (ggkk[k]/sdf));
               if (Opt->VoxDbg == ijk) {
                  GSCVAL(priCgA, k, ijk, fPCGA[k], dfA);
                  GSCVAL(priCgL, k, ijk, fPCGL[k], dfL);
                  SUMA_S_Notev("At %d %d %d:\n"
                               "%s, priCgA=%fx%f, priCgL=%fx%f, pC=%f \n", 
                        Opt->VoxDbg3[0], Opt->VoxDbg3[1], Opt->VoxDbg3[2],
                        cs->label[k], dfA, wA, dfL, wL, ggkk[k]/sdf); 
               }
            }
         }
      }
      ko = SUMA_Class_k_Label_Locator(cs, "OTHER");
      if (ko >= 0) {
         SUMA_S_Note("Imposing OTHER class from pCgL onto pC");
         for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
            if (IN_MASK(cmask, ijk)) {
               GSCVAL(priCgL, ko, ijk, fPCGL[ko], dfL);
               for (uk=0; uk<N_kok; ++uk) {
                  k = UseK[uk];
                  GSCVAL(priCgALL, k, ijk, fpriCgALL[k], df);
                  PSCVAL(priCgALL, k, ijk, fpriCgALL[k], (df - df*dfL));
               }
               PSCVAL(priCgALL, ko, ijk, fpriCgALL[ko],dfL);
            }
         }
      }
   } else {
      if (priCgA) {
         for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
            for (k=0; k<cs->N_label; ++k) {
               GSCVAL(priCgA, k, ijk, fPCGA[k], df);
               PSCVAL(priCgALL, k, ijk, fpriCgALL[k], df);
            }
         }
      } else if (priCgL) {
         for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
            for (k=0; k<cs->N_label; ++k) {
               GSCVAL(priCgL, k, ijk, fPCGL[k], df);
               PSCVAL(priCgALL, k, ijk, fpriCgALL[k], df);
            }
         }
      } else if ((mixfrac=SUMA_get_Stats(cs, "mix"))) {
         for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
            for (k=0; k<cs->N_label; ++k) {
               PSCVAL(priCgALL, k, ijk, fpriCgALL[k], mixfrac[k]);
            }
         }
      } else if ((mixfrac=SUMA_get_Stats(cs, "mix.init"))) {
         for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
            for (k=0; k<cs->N_label; ++k) {
               PSCVAL(priCgALL, k, ijk, fpriCgALL[k], mixfrac[k]);
            }
         }
      } else  {
         for (ijk=0; ijk<DSET_NVOX(Aset); ++ijk) {
            for (k=0; k<cs->N_label; ++k) {
               PSCVAL(priCgALL, k, ijk, fpriCgALL[k], 1.0/(double)cs->N_label);
            }
         }
      }    
   }
   
   SUMA_ifree(fpriCgALL); SUMA_ifree(fPCGA); SUMA_ifree(fPCGL); SUMA_ifree(ggkk);
   
   SUMA_RETURN(1);
}

/*!
   Compare a field bias estimate to the true estimate.
      rat = (ideal/mean_ideal - estimate/mean_estimate) /
               (ideal/mean_ideal)
   \param gold_bias: Reference field bias
   \param bias: Estimate field bias
   \param cmask: mask region
   \param cmask_count: Number of vox. in mask
   \param thresh: Threshold for summary stat
   \param prat: If !NULL, this will contain the ratio
               of the two volumes.
   \param bad_count: The fraction of voxels in the mask where 
                  rat >= thresh
*/
 
double SUMA_CompareBiasDsets(THD_3dim_dataset *gold_bias, THD_3dim_dataset *bias,
                         byte *cmask, int cmask_count, 
                         float thresh, THD_3dim_dataset *prat )
{
   static char FuncName[]={"SUMA_CompareBiasDsets"};
   float fprat = 1.0/3200, fgi, fgd;
   double md, mi, rat, ai, ad, bad_count;
   int ii; 
   SUMA_ENTRY;
   
   if (!gold_bias || !bias) {
      SUMA_RETURN(-1);
   }
   
   fgi = DSET_BRICK_FACTOR(gold_bias, 0);
   fgd = DSET_BRICK_FACTOR(bias, 0);
   md = 0.0; mi = 0.0;
   for (ii=0; ii<DSET_NVOX(bias); ++ii) {
      if ( IN_MASK(cmask, ii) ) {
         GVAL(gold_bias, 0, ii, ai);
         GVAL(bias, 0, ii, ad);
         md += ad;
         mi += ai;
      }
   }
   md *=  ((double)fgd/(double)cmask_count);
   mi *=  ((double)fgi/(double)cmask_count);
   
   bad_count = 0.0;
   for (ii=0; ii<DSET_NVOX(bias); ++ii) {
      if ( IN_MASK(cmask, ii) ) {
         GSCVAL(gold_bias, 0, ii, fgi, ai);
         ai /= mi;
         GSCVAL(bias, 0, ii, fgd, ad);
         ad /= md;
         rat = (ai-ad)/ai;
         if (prat) {
            PSCVAL(prat, 0, ii, fprat, rat);
         }
         if (SUMA_ABS(rat) >= thresh) ++bad_count;
      }
   }
   
   bad_count = bad_count/(double)cmask_count*100.0;
   if (prat) {
      EDIT_BRICK_FACTOR(prat, 0 , fprat);
   }
   
   SUMA_RETURN(bad_count);
}

/*!
   Compute the Dice coefficient between base and segmentation volumes
   \param base: Gold std segmentaion
   \param seg: segmentation
   \param cmask: Restrict all to cmask 
   \param mask_by_base: If (1) then exclude locations where base == 0,
                        even if this location is in cmask
   \param cs: The Dice coefficient is stored as stat "DICE" in cs
*/
int SUMA_CompareSegDsets(THD_3dim_dataset *base, THD_3dim_dataset *seg,
                         byte *cmask, byte mask_by_base,
                         SUMA_CLASS_STAT *cs )
{
   static char FuncName[]={"SUMA_CompareSegDsets"};
   int ii=0, kk=0, nbb, nss, nmatch;
   short *bb=NULL, *ss=NULL;
   float bf = 1.0, sf=1.0;
   
   SUMA_ENTRY;
   
   if (!base) {
      for (kk=0; kk<cs->N_label; ++kk) {
         SUMA_set_Stat(cs, cs->label[kk], "DICE", 0.0);
      }
   }
   
   bf = DSET_BRICK_FACTOR(base,0); if (bf == 0.0f) bf = 1.0;
   bb = (short *)DSET_ARRAY(base,0);
   sf = DSET_BRICK_FACTOR(seg,0); if (sf == 0.0f) sf = 1.0;
   ss = (short *)DSET_ARRAY(seg,0);
   for (kk=0; kk<cs->N_label; ++kk) {
      nmatch = 0; nss=0; nbb=0;
      for (ii=0; ii<DSET_NVOX(base); ++ii) {
         if ( IN_MASK(cmask, ii) && 
              (!mask_by_base || bb[ii]) ) {
            if ((ss[ii]*(int)sf) == cs->keys[kk]) ++nss;
            if ((bb[ii]*(int)bf) == cs->keys[kk]) {
               ++nbb;
               if (bb[ii] == ss[ii]) ++nmatch;
            }
         }
      }
      SUMA_set_Stat(cs, cs->label[kk], "DICE", (double)(nmatch*2)/(nss+nbb));
   }   
   SUMA_RETURN(0);
}
