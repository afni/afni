/* Synchronized selection inference. Draws and original-time support are shared;
   complete-null cells contribute to a global max family only after every draw
   has passed. A disk spool bounds map storage; BH's global arrays are budgeted. */
#include "mrilib.h"
#include "thd_insync_infer.h"
#include "thd_permute.h"
#include "thd_phasefft.h"
#include "thd_mapinfer.h"
#include "thd_niftiwrite.h"
#include <limits.h>
#include <float.h>
#include <stdint.h>
#include <unistd.h>
#ifdef USE_OMP
#include <omp.h>
#endif
#define IF_FAIL(...) ERROR_exit("3dInSync selection inference: " __VA_ARGS__)

typedef struct {
   INSYNC_kernel *kernel;
   float *data,*pack,*component,*scratch,*st,*draw,*nulls,*maxg,*maxneg,*maxc;
   int *length,*offset,*rows,*nv,*pc,*sample,**member,*count,*component_count,*component_offset;
   unsigned char *coverage,*keep,*keep2;
   float *original,*surrogate,*tnulls,*maxt;
   complex *spectrum,*fftwork;
} infer_worker;
struct INSYNC_infer {
   const INSYNC_options *o; const INSYNC_schedule *s; INSYNC_infer_grid grid;
   PERM_set *group_set,*condition_set; THD_resample_set *boot_set;
   PERM_result *group_result,*condition_result;
   unsigned char *group_valid,*condition_valid;
   float *group_q,*condition_q,*maxneg;
   int group_cells,condition_cells,maxblocks,ngmap,ncmap,nbmap,nout,contrast0,contrast1,neffect;
   int *label_condition; char **labels;
   double memory,spool_bytes;
   infer_worker *workers;
   FILE *spool,*manifest;
   THD_3dim_dataset *reference;
   THD_timeshift_set **shift; THD_phase_set **phase;
   int *run_used,*subject_at,temporal_cells,ntmap;
   long *run_seed;
   PERM_result *temporal_result;
   unsigned char *temporal_valid;
   float *temporal_q;
   unsigned char *temporal_keep;
};
static void *allocate(size_t n,size_t width)
{
   void *p;
   if(width && n>SIZE_MAX/width) IF_FAIL("allocation size overflow");
   p=calloc(n?n:1,width); if(!p) IF_FAIL("cannot allocate inference storage"); return p;
}
static void output_path(char *p,const INSYNC_infer *x,int id,const char *suffix)
{
   int n=id?snprintf(p,THD_MAX_NAME,"%s.sel%06d.%s",x->o->prefix,id,suffix):snprintf(p,THD_MAX_NAME,"%s.%s",x->o->prefix,suffix);
   if(n>=THD_MAX_NAME) IF_FAIL("output path too long");
}
static void preflight_path(const char *p)
{
   struct stat st;
   if(!lstat(p,&st)){
      if(!S_ISREG(st.st_mode)) IF_FAIL("output is not a regular file: %s",p);
      if(!THD_ok_overwrite()) IF_FAIL("output already exists: %s",p);
      if(access(p,W_OK)) IF_FAIL("output is not writable: %s",p);
   }
}
static void close_checked(FILE *fp)
{ int bad=ferror(fp); if(fclose(fp)) bad=1; if(bad) IF_FAIL("failed writing inference table/spool"); }
static int find_label(char **labels,int n,const char *s)
{ int i; for(i=0;i<n;i++) if(!strcmp(labels[i],s)) return i; return -1; }
static void add_label(INSYNC_infer *x,int *at,int condition,const char *stem,const char *who)
{
   char text[256],safe[64]; int i,j;
   snprintf(text,sizeof(text),"%s_%s",stem,who);
   for(i=0;text[i] && i<63;i++) safe[i]=(isalnum((unsigned char)text[i]) || text[i]=='_' || text[i]=='-' || text[i]=='.')?text[i]:'_';
   safe[i]=0;
   for(j=0;j<*at;j++) if(!strcmp(safe,x->labels[j])) IF_FAIL("inference label collision: %s",safe);
   x->labels[*at]=strdup(safe); if(!x->labels[*at]) IF_FAIL("cannot copy label");
   x->label_condition[(*at)++]=condition;
}
INSYNC_infer *INSYNC_infer_plan(const INSYNC_options *o,const INSYNC_schedule *s,const INSYNC_infer_grid *grid)
{
   INSYNC_infer *x; int i,c,g,id,at=0,*group; PERM_scheme *scheme;
   char path[THD_MAX_NAME],who[256]; double cells;
   if(!(o->nboot || o->nperm || o->do_exact || o->have_contrast || o->ncondperm || o->condition_exact || o->temporal_mode)) return NULL;
   if((o->nperm || o->do_exact) && grid->ngroup!=2) IF_FAIL("group permutation requires exactly two groups");
   if((o->ncondperm || o->condition_exact) && !o->have_contrast) IF_FAIL("condition swaps require -condition_contrast");
   if((double)grid->nsub*(2.*grid->nsub-1)>INT_MAX) IF_FAIL("too many subjects for the paired inference kernel");
   x=allocate(1,sizeof(*x)); x->o=o; x->s=s; x->grid=*grid; x->maxblocks=1;
   if(o->have_contrast){
      x->contrast0=find_label(grid->clabel,grid->ncond,o->contrast_name0);
      x->contrast1=find_label(grid->clabel,grid->ncond,o->contrast_name1);
      if(x->contrast0<0 || x->contrast1<0 || x->contrast0==x->contrast1) IF_FAIL("contrast needs two distinct existing conditions");
      for(id=1;id<=s->nselection;id++){
         int a=0,b=0;
         for(;;){
            while(a<s->nsegment && (s->segment[a].selection_id!=id || s->segment[a].source!=grid->source_at[x->contrast0*grid->nsub])) a++;
            while(b<s->nsegment && (s->segment[b].selection_id!=id || s->segment[b].source!=grid->source_at[x->contrast1*grid->nsub])) b++;
            if(a==s->nsegment || b==s->nsegment){ if(a!=b) IF_FAIL("unmatched condition selections"); break; }
            if(s->segment[a].run!=s->segment[b].run || s->segment[a].original_start!=s->segment[b].original_start || s->segment[a].original_stop!=s->segment[b].original_stop) IF_FAIL("condition contrast requires matched original selection coordinates");
            a++; b++;
         }
      }
   }
   group=allocate(grid->nsub,sizeof(int));
   for(g=0;g<grid->ngroup;g++) for(i=0;i<grid->gcount[g];i++) group[grid->gmember[g][i]]=g;
   if(o->nboot){ x->boot_set=THD_resample_set_build_stratified(grid->nsub,o->nboot,o->seed,group); if(!x->boot_set) IF_FAIL("cannot build bootstrap draws"); }
   if(o->nperm || o->do_exact){
      scheme=THD_perm_scheme_new(grid->nsub); if(!scheme) IF_FAIL("cannot allocate permutation scheme");
      scheme->exchange=PERM_EE; scheme->seed=o->seed; scheme->exact=o->do_exact; scheme->nperm=o->nperm?o->nperm:10000;
      THD_perm_scheme_set_eqclass(scheme,group); x->group_set=THD_perm_set_build(scheme); THD_perm_scheme_free(scheme);
      if(!x->group_set) IF_FAIL("cannot build group assignments");
      /* Equivalence-class enumeration permutes labels into subject slots.
         Invert it before treating entries as subjects in fixed group slots. */
      { int *inverse=allocate(grid->nsub,sizeof(int)),d,k;
        for(d=0;d<x->group_set->nperm;d++){
           int *p=x->group_set->perm+(size_t)d*grid->nsub;
           for(k=0;k<grid->nsub;k++) inverse[p[k]]=k;
           memcpy(p,inverse,grid->nsub*sizeof(int));
        }
        free(inverse);
      }
   }
   if(o->ncondperm || o->condition_exact){
      scheme=THD_perm_scheme_new(grid->nsub); if(!scheme) IF_FAIL("cannot allocate swap scheme");
      scheme->exchange=PERM_ISE; scheme->seed=o->seed+104729L; scheme->exact=o->condition_exact; scheme->nperm=o->ncondperm?o->ncondperm:10000;
      x->condition_set=THD_perm_set_build(scheme); THD_perm_scheme_free(scheme); if(!x->condition_set) IF_FAIL("cannot build paired swaps");
   }
   free(group);
   if(o->temporal_mode){
      int r,t;
      x->shift=allocate(s->nrun,sizeof(*x->shift)); x->phase=allocate(s->nrun,sizeof(*x->phase));
      x->run_used=allocate(s->nrun,sizeof(int)); x->run_seed=allocate(s->nrun,sizeof(long));
      x->subject_at=allocate(s->nsource,sizeof(int));
      for(c=0;c<grid->ncond;c++) for(i=0;i<grid->nsub;i++) x->subject_at[grid->source_at[c*grid->nsub+i]]=i;
      for(r=0;r<s->nrun;r++){
         int begin=s->runstart[r],end=r+1<s->nrun?s->runstart[r+1]:s->ntime;
         for(t=0;t<s->nsegment;t++) if(s->segment[t].original_start<end && s->segment[t].original_stop>=begin){ x->run_used[r]=1; break; }
         if(!x->run_used[r]) continue;
         /* Stable stream keyed by original run ordinal, independent of which
            selections/runs were requested. Keep seeds portable positive 31-bit. */
         x->run_seed[r]=(long)(((uint64_t)o->seed+UINT64_C(1000003)*(r+1))%UINT64_C(2147483647));
         if(o->temporal_mode==INSYNC_TNULL_TIMESHIFT){
            x->shift[r]=THD_timeshift_set_build(grid->nsub,end-begin,o->nnull,o->min_shift,x->run_seed[r]);
            if(!x->shift[r]) IF_FAIL("cannot build shifts for run %d",r+1);
         } else {
            x->phase[r]=THD_phase_set_build(grid->nsub,end-begin,o->nnull,x->run_seed[r]);
            if(!x->phase[r]) IF_FAIL("cannot build phases for run %d",r+1);
         }
      }
   }
   for(id=1;id<=s->nselection;id++) for(c=0;c<grid->ncond;c++){
      int nb=0;
      for(i=0;i<s->nsegment;i++) if(s->segment[i].selection_id==id && s->segment[i].source==grid->source_at[c*grid->nsub]) nb++;
      if(nb>x->maxblocks) x->maxblocks=nb;
   }
   x->neffect=grid->ngroup+(grid->ngroup==2);
   if(x->condition_set && (double)x->condition_set->nperm*x->neffect>INT_MAX) IF_FAIL("too many condition null values per cell");
   cells=(double)s->nselection*grid->nwork;
   if((x->group_set && cells*grid->ncond>INT_MAX) || (x->condition_set && cells*x->neffect>INT_MAX)) IF_FAIL("global inference family exceeds INT_MAX cells; reduce selections or space");
   if(o->temporal_mode && (cells*grid->ncond*grid->ngroup>INT_MAX || (double)o->nnull*grid->ngroup>INT_MAX)) IF_FAIL("temporal family/draw storage exceeds INT_MAX; reduce selections or space");
   if(o->temporal_mode) x->temporal_cells=(int)(cells*grid->ncond*grid->ngroup);
   if(x->group_set) x->group_cells=(int)(cells*grid->ncond);
   if(x->condition_set) x->condition_cells=(int)(cells*x->neffect);
   if((double)grid->ncond*grid->ngroup*4.+grid->ncond*7.+x->neffect*7.+8.*grid->ncond*grid->ngroup>INT_MAX) IF_FAIL("too many inference bricks");
   x->nbmap=o->nboot?4*grid->ncond*grid->ngroup:0; x->ngmap=x->group_set?7*grid->ncond:0;
   x->ncmap=o->have_contrast?(x->condition_set?7:2)*x->neffect:0; x->ntmap=o->temporal_mode?(7+o->temporal_censor)*grid->ncond*grid->ngroup:0; x->nout=x->nbmap+x->ngmap+x->ncmap+x->ntmap;
   x->labels=allocate(x->nout,sizeof(char *)); x->label_condition=allocate(x->nout,sizeof(int));
   if(o->nboot) for(c=0;c<grid->ncond;c++) for(g=0;g<grid->ngroup;g++){
      const char *stem[]={"BootLo","BootHi","BootN","BootValid"};
      snprintf(who,sizeof(who),"%s_%s",grid->clabel[c],grid->glabel[g]); for(i=0;i<4;i++) add_label(x,&at,c,stem[i],who);
   }
   if(x->group_set) for(c=0;c<grid->ncond;c++){
      const char *stem[]={"GroupDiff","P","Z","Q","PFWE","ZFWE","InferValid"};
      snprintf(who,sizeof(who),"%s_%s-%s",grid->clabel[c],grid->glabel[0],grid->glabel[1]); for(i=0;i<7;i++) add_label(x,&at,c,stem[i],who);
   }
   if(o->have_contrast) for(g=0;g<x->neffect;g++){
      const char *stem[]={"Cond","CP","CZ","CQ","CPFWE","CZFWE","CInferValid"};
      snprintf(who,sizeof(who),"%s-%s_%s",o->contrast_name0,o->contrast_name1,g<grid->ngroup?grid->glabel[g]:"GxCond");
      if(x->condition_set) for(i=0;i<7;i++) add_label(x,&at,-1,stem[i],who);
      else { add_label(x,&at,-1,"Cond",who); add_label(x,&at,-1,"CValid",who); }
   }
   if(o->temporal_mode) for(c=0;c<grid->ncond;c++) for(g=0;g<grid->ngroup;g++){
      const char *stem[]={"TemporalISC","TP","TZ","TQ","TPFWE","TZFWE","TInferValid","TNullN"};
      snprintf(who,sizeof(who),"%s_%s",grid->clabel[c],grid->glabel[g]);
      for(i=0;i<7+o->temporal_censor;i++) add_label(x,&at,c,stem[i],who);
   }
   if(o->temporal_mode){ output_path(path,x,0,"temporal_draws.tsv"); preflight_path(path); }
   if(o->temporal_censor){ output_path(path,x,0,"temporal_support.tsv"); preflight_path(path); }
   for(id=1;id<=s->nselection;id++){ output_path(path,x,id,"infer.nii.gz"); preflight_path(path); }
   { const char *suffix[]={"inference.tsv","inference_draws.tsv","inference_roi.tsv"};
      for(i=0;i<3;i++) if(i!=2 || grid->nroi){ output_path(path,x,0,suffix[i]); preflight_path(path); }
   }
   /* PERM_result: 28 bytes/cell; validity and q: 5; global BH sorting and
      temporary compaction: budget another 24/cell. No per-selection correction. */
   x->memory=57.*(x->group_cells+(double)x->condition_cells+x->temporal_cells)+4.*x->nout*(grid->nwork+grid->nvox+grid->nroi);
   {
      double ns=grid->nsub,nt=s->ntime,nb=x->maxblocks;
      double gp=x->group_set?x->group_set->nperm:0,cp=x->condition_set?x->condition_set->nperm:0;
      x->memory+=ns*(o->nboot*4.+(gp+cp)*5.)+4.*(gp*3+cp);
      x->memory+=grid->nthread*(64.*ns*nt+64.*ns*ns+16.*nb*ns*ns+64.*nt+128.*grid->ngroup+64.*nb+4.*o->nboot+16.*gp+4.*cp*(x->neffect+1));
   }
   if(o->temporal_mode){
      double ns=grid->nsub,nt=s->ntime,nd=o->nnull;
      if(o->temporal_censor) x->memory+=nd*nt;
      x->memory+=4.*nd+grid->nthread*(16.*ns*nt+72.*nt+4.*nd*(grid->ngroup+1));
      x->memory+=s->nrun*(2.*sizeof(void *)+sizeof(long)+sizeof(int)+sizeof(THD_phase_set)+sizeof(THD_timeshift_set))+4.*s->nsource;
      if(o->temporal_mode==INSYNC_TNULL_TIMESHIFT) x->memory+=4.*s->nrun*nd*ns;
      if(!o->quiet) INFO_message("3dInSync: temporal joint family %d cells; %d synchronized full-run draws; no relative-lag cache",x->temporal_cells,o->nnull);
   }
   x->spool_bytes=cells*x->nout*sizeof(float);
   if(!o->quiet) INFO_message("3dInSync: synchronized inference: %d group and %d condition family cells; memory %.3f GiB, map spool %.3f GiB; %d bootstrap, %d group, %d swap draws",x->group_cells,x->condition_cells,x->memory/1073741824.,x->spool_bytes/1073741824.,o->nboot,x->group_set?x->group_set->nperm:0,x->condition_set?x->condition_set->nperm:0);
   return x;
}
double INSYNC_infer_memory(const INSYNC_infer *x){ return x?x->memory:0; }
static void init_result(PERM_result **r,unsigned char **valid,float **q,int nc,int nd,int tail)
{
   int i; if(!nc) return;
   *r=THD_perm_result_new(nc,nd); if(!*r) IF_FAIL("cannot allocate global inference result"); (*r)->tail=tail;
   *valid=allocate(nc,1); *q=allocate(nc,sizeof(float));
   for(i=0;i<nd;i++) (*r)->max_null[i]=-FLT_MAX;
}
void INSYNC_infer_begin(INSYNC_infer *x)
{
   const INSYNC_infer_grid *h=&x->grid; char path[THD_MAX_NAME]; FILE *fp; int th,g,i,j;
   x->spool=tmpfile(); if(!x->spool) IF_FAIL("cannot create bounded result spool");
   output_path(path,x,0,"inference.tsv"); x->manifest=fopen(path,"w"); if(!x->manifest) IF_FAIL("cannot create inference manifest");
   fprintf(x->manifest,"# Status: running; final completion follows GLOBAL correction\n# Group family: all selections x conditions x analyzed space units. Condition family: all matched selections x requested effects x analyzed space units.\n# With -atlas the joint family includes voxel AND parcel units; with -roi_only each parcel counts once, not once per painted voxel.\n# Block support is fixed from observed data. Subject inference requires complete components and every null finite. Bootstrap drops unusable draws; intervals need >=10 draws and are pointwise.\n# Condition tests require identical admitted blocks in both conditions; mismatched cells are invalid, not re-aggregated on a different intersection.\n# GroupTail: %d; ConditionTail: %d; group assignments %s; condition swaps %s\n",x->o->tail,x->o->condition_tail,x->group_set?(x->group_set->is_exact?"exact":"sampled"):"none",x->condition_set?(x->condition_set->is_exact?"exact":"sampled"):"none");
   if(x->o->temporal_mode) fprintf(x->manifest,"# Temporal family: all selections x conditions x groups x analyzed space units. Independent subject/run surrogates; synchronized across conditions and selections. Validity follows the declared temporal censor policy. TemporalTail: %d\n",x->o->temporal_tail);
   if(x->o->temporal_mode) fprintf(x->manifest,"# Temporal censor policy: %s\n",x->o->temporal_censor?"joint_shift: move each source mask with its subject/run signal; recompute common support and block eligibility on EVERY draw. TNullN counts finite draws; failed nulls count as exceedances in p/FWE bounds (no skipped draws); observed invalid cells stay invalid. Support TSV reports mask coverage, not voxel signal validity.":"intact: participating runs uncensored; observed block support fixed");
   fprintf(x->manifest,"SelectionID\tFile\tBrick\tLabel\tCondition\n"); fflush(x->manifest);
   output_path(path,x,0,"inference_draws.tsv"); fp=fopen(path,"w"); if(!fp) IF_FAIL("cannot write draw provenance");
   fprintf(fp,"Kind\tDraw\tSlot\tSourceSubject\tSign\n");
   for(g=0;g<3;g++){
      int n=g==0?(x->boot_set?x->boot_set->nresample:0):g==1?(x->group_set?x->group_set->nperm:0):(x->condition_set?x->condition_set->nperm:0);
      for(i=0;i<n;i++) for(j=0;j<h->nsub;j++){
         int si=g==0?x->boot_set->index[(size_t)i*h->nsub+j]:g==1?x->group_set->perm[(size_t)i*h->nsub+j]:j;
         fprintf(fp,"%s\t%d\t%d\t",g==0?"bootstrap":g==1?"group":"condition",i,j);
         INSYNC_selection_token(fp,x->s->source[h->source_at[si]].subject);
         fprintf(fp,"\t%d\n",g==2?x->condition_set->sign[(size_t)i*h->nsub+j]:1);
      }
   }
   close_checked(fp);
   if(x->o->temporal_mode){
      int r,draw,sub;
      output_path(path,x,0,"temporal_draws.tsv"); fp=fopen(path,"w"); if(!fp) IF_FAIL("cannot write temporal provenance");
      fprintf(fp,"# Phase factors use THD_phase_set_factor (splitmix64-v1); DC/Nyquist unchanged; identity draw zero.\n# Offset means output[t]=input[(t+offset) modulo run length]. Conditions share each draw.\nKind\tRun\tStartTR\tNTR\tRunSeed\tDraw\tSubject\tOffset\n");
      for(r=0;r<x->s->nrun;r++) if(x->run_used[r]) for(draw=0;draw<x->o->nnull;draw++) for(sub=0;sub<h->nsub;sub++){
         int nt=(r+1<x->s->nrun?x->s->runstart[r+1]:x->s->ntime)-x->s->runstart[r];
         fprintf(fp,"%s\t%d\t%d\t%d\t%ld\t%d\t",x->shift[r]?"timeshift":"phase",r+1,x->s->runstart[r],nt,x->run_seed[r],draw);
         INSYNC_selection_token(fp,x->s->source[h->source_at[sub]].subject);
         if(x->shift[r]) fprintf(fp,"\t%d\n",x->shift[r]->offset[(size_t)draw*h->nsub+sub]); else fprintf(fp,"\tNA\n");
      }
      close_checked(fp);
   }
   if(x->o->temporal_censor){
      int d,r,t,source,e; size_t nt=x->s->ntime;
      x->temporal_keep=allocate((size_t)x->o->nnull*nt,1); memset(x->temporal_keep,1,(size_t)x->o->nnull*nt);
      for(d=0;d<x->o->nnull;d++) for(r=0;r<x->s->nrun;r++) if(x->run_used[r]){
         int start=x->s->runstart[r],len=(r+1<x->s->nrun?x->s->runstart[r+1]:nt)-start;
         for(source=0;source<x->s->nsource;source++){
            int off=x->shift[r]->offset[(size_t)d*h->nsub+x->subject_at[source]];
            for(t=0;t<len;t++) x->temporal_keep[(size_t)d*nt+start+t]&=x->s->keep[(size_t)source*nt+start+(t+off)%len];
         }
      }
      output_path(path,x,0,"temporal_support.tsv"); fp=fopen(path,"w"); if(!fp) IF_FAIL("cannot write temporal support table");
      fprintf(fp,"# Mask-only coverage for each draw/component; component signal validity can further reduce TNullN. Counts are common across subjects AND conditions. Block gates are recomputed per draw.\nSelectionID\tCondition\tDraw\tSegment\tRun\tStartTR\tStopTR\tNominalN\tRetainedN\tRetainedFrac\tCoveragePass\n");
      for(e=0;e<x->s->nsegment;e++){
         const INSYNC_segment *seg=x->s->segment+e; int c;
         for(c=0;c<h->ncond;c++) if(seg->source==h->source_at[c*h->nsub]) break;
         if(c==h->ncond) continue;
         for(d=0;d<x->o->nnull;d++){
            int kept=0,nominal=seg->original_stop-seg->original_start+1;
            for(t=seg->original_start;t<=seg->original_stop;t++) kept+=x->temporal_keep[(size_t)d*nt+t];
            fprintf(fp,"sel%06d\t",seg->selection_id); INSYNC_selection_token(fp,h->clabel[c]); fprintf(fp,"\t%d\t",d); INSYNC_selection_token(fp,seg->segment);
            fprintf(fp,"\t%d\t%d\t%d\t%d\t%d\t%.9g\t%d\n",seg->run,seg->original_start,seg->original_stop,nominal,kept,(double)kept/nominal,kept>=x->o->min_valid && (double)kept/nominal>=x->o->min_valid_frac);
         }
      }
      close_checked(fp);
   }
   init_result(&x->group_result,&x->group_valid,&x->group_q,x->group_cells,x->group_set?x->group_set->nperm:0,x->o->tail);
   init_result(&x->condition_result,&x->condition_valid,&x->condition_q,x->condition_cells,x->condition_set?x->condition_set->nperm:0,x->o->condition_tail);
   init_result(&x->temporal_result,&x->temporal_valid,&x->temporal_q,x->temporal_cells,x->o->nnull,x->o->temporal_tail);
   if(x->group_set && x->o->tail==INSYNC_TAIL_BI){ x->maxneg=allocate(x->group_set->nperm,sizeof(float)); for(i=0;i<x->group_set->nperm;i++) x->maxneg[i]=-FLT_MAX; }
   x->workers=allocate(h->nthread,sizeof(*x->workers));
   for(th=0;th<h->nthread;th++){
      infer_worker *w=x->workers+th; size_t ns=2*(size_t)h->nsub,nb=x->maxblocks,ng=2*(size_t)h->ngroup;
      int nd=x->group_set?x->group_set->nperm:1,cd=x->condition_set?x->condition_set->nperm*x->neffect:1;
      if(cd>nd) nd=cd;
      if(x->o->temporal_mode){
         size_t n=(size_t)h->nsub*x->s->ntime;
         w->original=allocate(n,sizeof(float)); w->surrogate=allocate(n,sizeof(float));
         if(x->o->temporal_mode==INSYNC_TNULL_PHASE){ w->spectrum=allocate(n,sizeof(complex)); w->fftwork=allocate(x->s->ntime,sizeof(complex)); }
         w->tnulls=allocate((size_t)x->o->nnull*h->ngroup,sizeof(float)); w->maxt=allocate(x->o->nnull,sizeof(float));
         for(i=0;i<x->o->nnull;i++) w->maxt[i]=-FLT_MAX;
      }
      w->kernel=INSYNC_kernel_new((int)ns,x->s->ntime); if(!w->kernel) IF_FAIL("cannot allocate resampling kernel");
      w->data=allocate(ns*x->s->ntime,sizeof(float)); w->pack=allocate(ns*x->s->ntime,sizeof(float));
      w->component=allocate(nb*ns*ns,sizeof(float)); w->scratch=allocate(nb+ns*ns,sizeof(float));
      w->st=allocate(ng,sizeof(float)); w->nv=allocate(ng,sizeof(int)); w->pc=allocate(ng,sizeof(int));
      w->component_count=allocate(ng,sizeof(int)); w->component_offset=allocate(ng,sizeof(int));
      w->sample=allocate(ns,sizeof(int)); w->member=allocate(ng,sizeof(int *)); w->count=allocate(ng,sizeof(int));
      w->length=allocate(nb,sizeof(int)); w->offset=allocate(nb,sizeof(int)); w->rows=allocate(nb,sizeof(int));
      w->coverage=allocate(nb,1); w->keep=allocate(nb,1); w->keep2=allocate(nb,1);
      w->draw=allocate(x->o->nboot,sizeof(float)); w->nulls=allocate(nd,sizeof(float));
      w->maxg=allocate(x->group_set?x->group_set->nperm:0,sizeof(float));
      w->maxneg=allocate(x->group_set?x->group_set->nperm:0,sizeof(float));
      w->maxc=allocate(x->condition_set?x->condition_set->nperm:0,sizeof(float));
      for(i=0;x->group_set && i<x->group_set->nperm;i++) w->maxg[i]=w->maxneg[i]=-FLT_MAX;
      for(i=0;x->condition_set && i<x->condition_set->nperm;i++) w->maxc[i]=-FLT_MAX;
   }
}

/* Pack block data once per analysis cell. Every source uses the same finite
   original-time support. The second condition equals the first for group/CI. */
static int pack_data(INSYNC_infer *x,infer_worker *w,int id,int c,int d,int unit,int aggregate,
                    const void *src,INSYNC_sample_reader get,const unsigned char *common)
{
   const INSYNC_infer_grid *h=&x->grid; int i,b=0,t,si,len,total=0,nr=0,ns=2*h->nsub,nt=x->s->ntime,nominal=0;
   for(i=0;i<x->s->nsegment;i++) if(x->s->segment[i].selection_id==id && x->s->segment[i].source==h->source_at[c*h->nsub]) w->rows[nr++]=i;
   len=0;
   for(i=0;i<nr;i++){
      const INSYNC_segment *e=x->s->segment+w->rows[i]; int begin=aggregate?0:len;
      if(aggregate) len=0;
      nominal+=e->original_stop-e->original_start+1;
      for(t=e->original_start;t<=e->original_stop;t++) if(common[t]){
         int good=1;
         if(x->o->missing_policy==INSYNC_MISSING_COMMON) for(si=0;si<x->s->nsource;si++) if(!isfinite(get(src,si,unit,t))){ good=0; break; }
         if(!good) continue;
         for(si=0;si<h->nsub;si++){
            w->pack[(size_t)si*nt+len]=get(src,h->source_at[c*h->nsub+si],unit,t);
            w->pack[(size_t)(h->nsub+si)*nt+len]=get(src,h->source_at[d*h->nsub+si],unit,t);
         }
         len++;
      }
      if(!aggregate && x->o->segment_center && len>begin) for(si=0;si<ns;si++){
         double mean=0;
         for(t=begin;t<len;t++) mean+=w->pack[(size_t)si*nt+t];
         mean/=len-begin;
         for(t=begin;t<len;t++) w->pack[(size_t)si*nt+t]-=(float)mean;
      }
      if(!aggregate && i<nr-1) continue;
      w->length[b]=len; w->offset[b]=total*ns;
      w->coverage[b]=len>=x->o->min_valid && (double)len/nominal>=x->o->min_valid_frac;
      for(si=0;si<ns;si++) memcpy(w->data+(size_t)total*ns+(size_t)si*len,w->pack+(size_t)si*nt,len*sizeof(float));
      total+=len; b++; if(aggregate) nominal=0;
   }
   return b;
}
static void identity_members(INSYNC_infer *x,infer_worker *w,int paired)
{
   const INSYNC_infer_grid *h=&x->grid; int g,i,pos=0,k;
   for(k=0;k<(paired?2:1);k++) for(g=0;g<h->ngroup;g++){
      int gg=k*h->ngroup+g; w->member[gg]=w->sample+pos; w->count[gg]=h->gcount[g];
      for(i=0;i<h->gcount[g];i++) w->sample[pos++]=h->gmember[g][i]+k*h->nsub;
   }
}
/* fixed=NULL chooses observed complete blocks. Otherwise never change support
   on a resample: one failed retained component invalidates that draw. */
static int estimate(INSYNC_infer *x,infer_worker *w,int nb,int aggregate,int ng,
                    const unsigned char *fixed,unsigned char *chosen)
{
   const INSYNC_options *o=x->o; int ns=2*x->grid.nsub,b,g,i,j,ncomp=0,nkeep=0;
   for(g=0;g<ng;g++){
      int n=0,unique=0;
      w->component_offset[g]=ncomp;
      for(i=0;i<w->count[g];i++){
         for(j=0;j<i;j++) if(w->member[g][i]==w->member[g][j]) break;
         if(j==i) unique++;
         if(o->method==INSYNC_METHOD_LOO) n++;
         else for(j=i+1;j<w->count[g];j++) if(w->member[g][i]!=w->member[g][j]) n++;
      }
      if(unique<2) goto invalid;
      w->component_count[g]=n; ncomp+=n;
   }
   for(b=0;b<nb;b++){
      const float *matrix,*loo; int good=1,p=0;
      if(chosen) chosen[b]=0;
      if(fixed && !fixed[b]) continue;
      if(!w->coverage[b]){ if(fixed) goto invalid; else continue; }
      INSYNC_kernel_eval(w->kernel,w->length[b],w->data+w->offset[b],o->corr_metric,o->method,o->summary,ng,w->count,w->member,w->st,w->nv,w->pc);
      matrix=INSYNC_kernel_matrix(w->kernel); loo=INSYNC_kernel_loo(w->kernel);
      for(g=0;g<ng;g++) for(i=0;i<w->count[g];i++){
         int a=w->member[g][i];
         if(o->method==INSYNC_METHOD_LOO){ float v=loo[a]; w->component[(size_t)b*ncomp+p++]=v; if(!isfinite(v)) good=0; }
         else for(j=i+1;j<w->count[g];j++){
            int c=w->member[g][j]; float v;
            if(a==c) continue;
            v=matrix[(size_t)a*ns+c]; w->component[(size_t)b*ncomp+p++]=v; if(!isfinite(v)) good=0;
         }
      }
      if(!good){ if(fixed) goto invalid; else continue; }
      if(chosen) chosen[b]=1;
      nkeep++;
   }
   if(!nkeep || (aggregate && (nkeep<o->min_blocks || (double)nkeep/nb<o->min_block_frac || (!o->block_drop_common && nkeep!=nb)))) goto invalid;
   for(g=0;g<ng;g++){
      int nc=w->component_count[g],off=w->component_offset[g];
      for(i=0;i<nc;i++){
         int n=0;
         for(b=0;b<nb;b++) if(fixed?fixed[b]:chosen[b]) w->scratch[n++]=w->component[(size_t)b*ncomp+off+i];
         /* Store component aggregates in a disjoint tail; group scratch is
            separate from block scratch and may sort its input in place. */
         w->scratch[nb+i]=n==1?w->scratch[0]:INSYNC_summary(n,w->scratch,o->block_summary,w->scratch,NULL);
      }
      w->st[g]=INSYNC_summary(nc,w->scratch+nb,o->summary,w->scratch+nb,NULL);
   }
   return 1;
invalid:
   for(g=0;g<ng;g++) w->st[g]=NAN;
   return 0;
}
static void record_null(PERM_result *r,unsigned char *valid,int cell,const float *nulls,float *maxpos,float *maxneg,int tail)
{
   int i,neg=(tail==INSYNC_TAIL_BI && nulls[0]<0); float observed=tail==PERM_TAIL_TWO?fabsf(nulls[0]):neg?-nulls[0]:nulls[0];
   for(i=0;i<r->nperm;i++) if(!isfinite(nulls[i])) return;
   valid[cell]=1; r->stat[cell]=nulls[0];
   for(i=0;i<r->nperm;i++){
      float cmp=tail==PERM_TAIL_TWO?fabsf(nulls[i]):neg?-nulls[i]:nulls[i];
      if(cmp>=observed) r->cnt_unc[cell]++;
      if(tail==INSYNC_TAIL_BI){ if(nulls[i]>maxpos[i]) maxpos[i]=nulls[i]; if(-nulls[i]>maxneg[i]) maxneg[i]=-nulls[i]; }
      else if(cmp>maxpos[i]) maxpos[i]=cmp;
   }
}
typedef struct { const INSYNC_infer *x; const float *data; } temporal_reader;
static float temporal_get(const void *opaque,int source,int unit,int time)
{
   const temporal_reader *r=opaque; (void)unit;
   return r->data[(size_t)r->x->subject_at[source]*r->x->s->ntime+time];
}
/* Prepare complete participating runs, never a compressed/windowed spectrum. */
static void temporal_prepare(INSYNC_infer *x,infer_worker *w,int c,int unit,const void *src,INSYNC_sample_reader get)
{
   int r,sub,t,nt=x->s->ntime;
   for(r=0;r<x->s->nrun;r++) if(x->run_used[r]){
      int start=x->s->runstart[r],len=(r+1<x->s->nrun?x->s->runstart[r+1]:nt)-start;
      for(sub=0;sub<x->grid.nsub;sub++){
         float *data=w->original+(size_t)sub*nt+start;
         for(t=0;t<len;t++) data[t]=get(src,x->grid.source_at[c*x->grid.nsub+sub],unit,start+t);
         if(w->spectrum){
            complex *spec=w->spectrum+(size_t)sub*nt+start;
            for(t=0;t<len;t++){ spec[t].r=data[t]; spec[t].i=0; }
            if(THD_fftnf_OMP(len,NULL,&spec[0].r,&spec[0].i,-2,0.0)) IF_FAIL("forward run FFT failed");
         }
      }
   }
}
static void temporal_draw(INSYNC_infer *x,infer_worker *w,int draw)
{
   int r,sub,t,f,nt=x->s->ntime;
   for(r=0;r<x->s->nrun;r++) if(x->run_used[r]){
      int start=x->s->runstart[r],len=(r+1<x->s->nrun?x->s->runstart[r+1]:nt)-start;
      for(sub=0;sub<x->grid.nsub;sub++){
         const float *original=w->original+(size_t)sub*nt+start;
         float *out=w->surrogate+(size_t)sub*nt+start;
         if(!draw){ memcpy(out,original,len*sizeof(float)); continue; }
         if(x->shift[r]){
            int off=x->shift[r]->offset[(size_t)draw*x->grid.nsub+sub];
            for(t=0;t<len;t++) out[t]=original[(t+off)%len];
         } else {
            complex *spec=w->spectrum+(size_t)sub*nt+start,*work=w->fftwork;
            memcpy(work,spec,len*sizeof(complex));
            for(f=1;f<=x->phase[r]->nfreq;f++){
               float co,si; THD_phase_set_factor(x->phase[r],draw,sub,f,&co,&si);
               work[f].r=spec[f].r*co-spec[f].i*si; work[f].i=spec[f].r*si+spec[f].i*co;
               work[len-f].r=work[f].r; work[len-f].i=-work[f].i;
            }
            if(THD_fftnf_OMP(len,NULL,&work[0].r,&work[0].i,2,0.0)) IF_FAIL("inverse run FFT failed");
            for(t=0;t<len;t++) out[t]=work[t].r/(float)len;
         }
      }
   }
}
static void temporal_cell(INSYNC_infer *x,infer_worker *w,int id,int c,int unit,int aggregate,
                          const void *src,INSYNC_sample_reader get,const unsigned char *common,float *maps)
{
   int d,g,nb; temporal_reader reader={x,w->surrogate};
   temporal_prepare(x,w,c,unit,src,get); identity_members(x,w,0);
   for(d=0;d<x->o->nnull;d++){
      temporal_draw(x,w,d);
      nb=pack_data(x,w,id,c,c,unit,aggregate,&reader,temporal_get,x->temporal_keep?x->temporal_keep+(size_t)d*x->s->ntime:common);
      estimate(x,w,nb,aggregate,x->grid.ngroup,(d && !x->o->temporal_censor)?w->keep:NULL,(d && !x->o->temporal_censor)?NULL:w->keep);
      for(g=0;g<x->grid.ngroup;g++) w->tnulls[(size_t)g*x->o->nnull+d]=w->st[g];
   }
   for(g=0;g<x->grid.ngroup;g++){
      int cell=(((id-1)*x->grid.ncond+c)*x->grid.ngroup+g)*x->grid.nwork+unit;
      if(x->o->temporal_censor){
         int n=0,b=x->nbmap+x->ngmap+x->ncmap+8*(c*x->grid.ngroup+g)+7;
         for(d=0;d<x->o->nnull;d++) n+=isfinite(w->tnulls[(size_t)g*x->o->nnull+d])!=0;
         maps[(size_t)b*x->grid.nwork+unit]=n;
      }
      /* Joint masks change support. Never select cells by successful null
         draws: bound every failed null as an exceedance, including maxima.
         An invalid observed statistic still has no estimand and remains invalid. */
      if(x->o->temporal_censor){
         /* Keep the prespecified spatial/selection family, including cells
            invalid in the observed data. Such cells get p=1, but their null
            maxima must not disappear through data-dependent family selection. */
         for(d=0;d<x->o->nnull;d++){
            float v=w->tnulls[(size_t)g*x->o->nnull+d];
            float cmp=!isfinite(v)?FLT_MAX:x->o->temporal_tail==PERM_TAIL_TWO?fabsf(v):v;
            if(cmp>w->maxt[d]) w->maxt[d]=cmp;
            if(d && !isfinite(v)) w->tnulls[(size_t)g*x->o->nnull+d]=FLT_MAX;
         }
      }
      record_null(x->temporal_result,x->temporal_valid,cell,w->tnulls+(size_t)g*x->o->nnull,w->maxt,NULL,x->o->temporal_tail);
   }
}
void INSYNC_infer_selection(INSYNC_infer *x,int id,int aggregate,const void *src,INSYNC_sample_reader get,const unsigned char *common,THD_3dim_dataset *reference)
{
   const INSYNC_infer_grid *h=&x->grid; int unit,i,th;
   float *maps=allocate((size_t)x->nout*h->nwork,sizeof(float));
   if(!x->reference){ x->reference=EDIT_empty_copy(reference); tross_Copy_History(reference,x->reference); }
#ifdef USE_OMP
#pragma omp parallel for schedule(dynamic,8)
#endif
   for(unit=0;unit<h->nwork;unit++){
      int thread=0,c,g,k,nb,draw;
#ifdef USE_OMP
      thread=omp_get_thread_num();
#endif
      infer_worker *w=x->workers+thread;
      for(c=0;c<h->ncond;c++){
         if(!x->boot_set && !x->group_set) break;
         nb=pack_data(x,w,id,c,c,unit,aggregate,src,get,common); identity_members(x,w,0);
         if(!estimate(x,w,nb,aggregate,h->ngroup,NULL,w->keep)) continue;
         if(x->group_set){
            w->nulls[0]=w->st[0]-w->st[1];
            for(draw=1;draw<x->group_set->nperm;draw++){
               int pos=0; const int *pp=x->group_set->perm+(size_t)draw*h->nsub;
               for(g=0;g<h->ngroup;g++){
                  w->member[g]=w->sample+pos; w->count[g]=h->gcount[g];
                  for(k=0;k<h->gcount[g];k++) w->sample[pos++]=pp[h->gmember[g][k]];
               }
               estimate(x,w,nb,aggregate,h->ngroup,w->keep,NULL); w->nulls[draw]=w->st[0]-w->st[1];
            }
            record_null(x->group_result,x->group_valid,((id-1)*h->ncond+c)*h->nwork+unit,w->nulls,w->maxg,w->maxneg,x->o->tail);
         }
         if(x->boot_set) for(g=0;g<h->ngroup;g++){
            int usable=0,slot=4*(c*h->ngroup+g);
            w->member[0]=w->sample; w->count[0]=h->gcount[g];
            for(draw=0;draw<x->boot_set->nresample;draw++){
               const int *ix=x->boot_set->index+(size_t)draw*h->nsub;
               for(k=0;k<h->gcount[g];k++) w->sample[k]=ix[h->gmember[g][k]];
               if(estimate(x,w,nb,aggregate,1,w->keep,NULL)) w->draw[usable++]=w->st[0];
            }
            maps[(size_t)(slot+2)*h->nwork+unit]=usable;
            if(usable>=10){
               maps[(size_t)slot*h->nwork+unit]=INSYNC_percentile(w->draw,usable,.5f*x->o->boot_alpha);
               maps[(size_t)(slot+1)*h->nwork+unit]=INSYNC_percentile(w->draw,usable,1-.5f*x->o->boot_alpha);
               maps[(size_t)(slot+3)*h->nwork+unit]=1;
            }
         }
      }
      if(x->o->temporal_mode) for(c=0;c<h->ncond;c++) temporal_cell(x,w,id,c,unit,aggregate,src,get,common,maps);
      if(x->o->have_contrast){
         int ng=2*h->ngroup,good=1,nul=x->condition_set?x->condition_set->nperm:0;
         /* First require identical observed admitted-block sets, not an
            implicit intersection that changes the contrast estimand. */
         nb=pack_data(x,w,id,x->contrast0,x->contrast0,unit,aggregate,src,get,common); identity_members(x,w,0);
         if(!estimate(x,w,nb,aggregate,h->ngroup,NULL,w->keep)) good=0;
         nb=pack_data(x,w,id,x->contrast1,x->contrast1,unit,aggregate,src,get,common); identity_members(x,w,0);
         if(!estimate(x,w,nb,aggregate,h->ngroup,NULL,w->keep2)) good=0;
         for(k=0;k<nb;k++) if(w->keep[k]!=w->keep2[k]) good=0;
         if(!good) continue;
         nb=pack_data(x,w,id,x->contrast0,x->contrast1,unit,aggregate,src,get,common); identity_members(x,w,1);
         if(!estimate(x,w,nb,aggregate,ng,w->keep,NULL)) continue;
         for(g=0;g<x->neffect;g++){
            int base=x->nbmap+x->ngmap+(nul?7:2)*g;
            float effect=g<h->ngroup?w->st[g]-w->st[h->ngroup+g]:(w->st[0]-w->st[h->ngroup])-(w->st[1]-w->st[h->ngroup+1]);
            maps[(size_t)base*h->nwork+unit]=effect; maps[(size_t)(base+(nul?6:1))*h->nwork+unit]=1;
            if(nul) w->nulls[(size_t)g*nul]=effect;
         }
         for(draw=1;draw<nul;draw++){
            const signed char *sign=x->condition_set->sign+(size_t)draw*h->nsub; int pos=0,side;
            for(side=0;side<2;side++) for(g=0;g<h->ngroup;g++){
               int gg=side*h->ngroup+g; w->member[gg]=w->sample+pos; w->count[gg]=h->gcount[g];
               for(k=0;k<h->gcount[g];k++){ int si=h->gmember[g][k]; w->sample[pos++]=si+((sign[si]<0)^side)*h->nsub; }
            }
            estimate(x,w,nb,aggregate,ng,w->keep,NULL);
            for(g=0;g<x->neffect;g++) w->nulls[(size_t)g*nul+draw]=g<h->ngroup?w->st[g]-w->st[h->ngroup+g]:(w->st[0]-w->st[h->ngroup])-(w->st[1]-w->st[h->ngroup+1]);
         }
         if(nul) for(g=0;g<x->neffect;g++) record_null(x->condition_result,x->condition_valid,((id-1)*x->neffect+g)*h->nwork+unit,w->nulls+(size_t)g*nul,w->maxc,NULL,x->o->condition_tail);
      }
   }
   for(th=0;th<h->nthread;th++){
      infer_worker *w=x->workers+th;
      for(i=0;x->group_set && i<x->group_set->nperm;i++){
         if(w->maxg[i]>x->group_result->max_null[i]) x->group_result->max_null[i]=w->maxg[i];
         if(x->maxneg && w->maxneg[i]>x->maxneg[i]) x->maxneg[i]=w->maxneg[i];
      }
      for(i=0;i<x->o->nnull;i++) if(w->maxt[i]>x->temporal_result->max_null[i]) x->temporal_result->max_null[i]=w->maxt[i];
      for(i=0;x->condition_set && i<x->condition_set->nperm;i++) if(w->maxc[i]>x->condition_result->max_null[i]) x->condition_result->max_null[i]=w->maxc[i];
   }
   if(fwrite(maps,sizeof(float),(size_t)x->nout*h->nwork,x->spool)!=(size_t)x->nout*h->nwork) IF_FAIL("failed writing inference spool (check temporary disk space)");
   free(maps);
   if(!x->o->quiet) INFO_message("3dInSync: inference selection %d/%d accumulated; global corrections pending",id,x->s->nselection);
}
static int float_cmp(const void *a,const void *b){ float x=*(const float *)a,y=*(const float *)b; return (x>y)-(x<y); }
static void finish_family(PERM_result *r,unsigned char *valid,float *q,float *negative,int tail,const char *what,int full_family)
{
   int i,n=0; if(!r) return;
   for(i=0;i<r->nelem;i++) if(valid[i]) n++;
   if(!n) IF_FAIL("no %s cells in %s family",full_family?"observed-valid":"complete finite-null",what);
   if(tail!=INSYNC_TAIL_BI) THD_perm_result_finish(r,valid);
   else {
      qsort(r->max_null,r->nperm,sizeof(float),float_cmp); qsort(negative,r->nperm,sizeof(float),float_cmp);
      for(i=0;i<r->nelem;i++){
         if(!valid[i]){ r->p_unc[i]=r->p_fwe[i]=1; continue; }
         r->p_unc[i]=fminf(1,2.f*r->cnt_unc[i]/r->nperm);
         r->p_fwe[i]=fminf(1,2*THD_perm_emp_pvalue(r->stat[i]<0?negative:r->max_null,r->nperm,fabsf(r->stat[i])));
         r->z_unc[i]=THD_perm_signed_z(r->p_unc[i],r->stat[i],PERM_TAIL_TWO); r->z_fwe[i]=THD_perm_signed_z(r->p_fwe[i],r->stat[i],PERM_TAIL_TWO);
      }
   }
   THD_bh_fdr_masked(r->nelem,r->p_unc,full_family?NULL:valid,q);
}
void INSYNC_infer_finish(INSYNC_infer *x,FILE *brick_index)
{
   const INSYNC_infer_grid *h=&x->grid; int id,b,w,i,g; FILE *roi=NULL;
   char path[THD_MAX_NAME]; float *maps=allocate((size_t)x->nout*h->nwork,sizeof(float));
   if(x->temporal_result && x->o->temporal_censor){
      int valid=0,failed=0;
      for(i=0;i<x->o->nnull;i++) failed+=x->temporal_result->max_null[i]==FLT_MAX;
      fprintf(x->manifest,"# JointShift: %d/%d draws have an unresolved family maximum; these count as exceedances for FWE. BH includes observed-invalid cells at p=1.\n",failed,x->o->nnull);
      for(i=0;i<x->temporal_cells;i++) valid+=x->temporal_valid[i]!=0;
      INFO_message("3dInSync: joint_shift observed-valid cells %d/%d; %d invalid; failed null draws count as exceedances (inspect TNullN and temporal_support.tsv)",valid,x->temporal_cells,x->temporal_cells-valid);
   }
   finish_family(x->temporal_result,x->temporal_valid,x->temporal_q,NULL,x->o->temporal_tail,"temporal",x->o->temporal_censor);
   finish_family(x->group_result,x->group_valid,x->group_q,x->maxneg,x->o->tail,"group",0);
   finish_family(x->condition_result,x->condition_valid,x->condition_q,NULL,x->o->condition_tail,"condition",0);
   if(fflush(x->spool) || fseeko(x->spool,0,SEEK_SET)) IF_FAIL("cannot rewind inference spool");
   if(h->nroi){ output_path(path,x,0,"inference_roi.tsv"); roi=fopen(path,"w"); if(!roi) IF_FAIL("cannot write inference ROI table"); fprintf(roi,"SelectionID\tROI\tROI_Label\tNVoxel\tBrick\tLabel\tValue\n"); }
   for(id=1;id<=x->s->nselection;id++){
      THD_3dim_dataset *out=EDIT_empty_copy(x->reference); float **full=allocate(x->nout,sizeof(float *)); niftiwr_opts_t options;
      if(fread(maps,sizeof(float),(size_t)x->nout*h->nwork,x->spool)!=(size_t)x->nout*h->nwork) IF_FAIL("truncated inference spool");
      for(g=0;g<3;g++){
         PERM_result *r=g==2?x->temporal_result:g?x->condition_result:x->group_result;
         unsigned char *valid=g==2?x->temporal_valid:g?x->condition_valid:x->group_valid;
         float *q=g==2?x->temporal_q:g?x->condition_q:x->group_q;
         int n=g==2?h->ncond*h->ngroup:g?x->neffect:h->ncond,start=g==2?x->nbmap+x->ngmap+x->ncmap:g?x->nbmap+x->ngmap:x->nbmap;
         if(!r) continue;
         for(i=0;i<n;i++) for(w=0;w<h->nwork;w++){
            int e=((id-1)*n+i)*h->nwork+w,base=start+(g==2?7+x->o->temporal_censor:7)*i;
            float value[7]={r->stat[e],r->p_unc[e],r->z_unc[e],q[e],r->p_fwe[e],r->z_fwe[e],valid[e]};
            for(b=0;b<7;b++) maps[(size_t)(base+b)*h->nwork+w]=value[b];
         }
      }
      output_path(path,x,id,"infer.nii.gz");
      EDIT_dset_items(out,ADN_prefix,path,ADN_nvals,x->nout,ADN_ntt,0,ADN_brick_fac,NULL,ADN_type,HEAD_FUNC_TYPE,ADN_func_type,FUNC_BUCK_TYPE,ADN_datum_all,MRI_float,ADN_none); tross_Copy_History(x->reference,out);
      for(b=0;b<x->nout;b++){
         full[b]=allocate(h->nvox,sizeof(float));
         for(w=0;w<h->nvoxelwork;w++) full[b][h->widx[w]]=maps[(size_t)b*h->nwork+w];
         if(h->nroi) for(w=0;w<h->nroi;w++){
            int r=h->roi_ids[w],unit=h->nvoxelwork+w,v; float value=maps[(size_t)b*h->nwork+unit]; char label[128];
            snprintf(label,sizeof(label),"ROI%d",h->rois->val[r]);
            if(x->o->roi_only) for(v=0;v<h->rois->vox[r].nar;v++) full[b][h->rois->vox[r].ar[v]]=value;
            fprintf(roi,"sel%06d\t%d\t",id,h->rois->val[r]); INSYNC_selection_token(roi,h->rois->lab && h->rois->lab[r]?h->rois->lab[r]:label);
            fprintf(roi,"\t%d\t%d\t%s\t%.9g\n",h->rois->vox[r].nar,b,x->labels[b],value);
         }
         EDIT_substitute_brick(out,b,MRI_float,full[b]); EDIT_BRICK_TO_NOSTAT(out,b); EDIT_BRICK_LABEL(out,b,x->labels[b]);
         if(!strncmp(x->labels[b],"TZ_",3) || !strncmp(x->labels[b],"TZFWE_",6) || !strncmp(x->labels[b],"Z_",2) || !strncmp(x->labels[b],"ZFWE_",5) || !strncmp(x->labels[b],"CZ_",3) || !strncmp(x->labels[b],"CZFWE_",6)) EDIT_BRICK_TO_FIZT(out,b);
         fprintf(x->manifest,"sel%06d\t",id); INSYNC_selection_token(x->manifest,path); fprintf(x->manifest,"\t%d\t%s\t",b,x->labels[b]); INSYNC_selection_token(x->manifest,x->label_condition[b]<0?"contrast":h->clabel[x->label_condition[b]]); fputc('\n',x->manifest);
         fprintf(brick_index,"sel%06d\tinference\t",id); INSYNC_selection_token(brick_index,path); fprintf(brick_index,"\t%d\t%s\n",b,x->labels[b]);
      }
      THD_set_dataset_attributes(out); options=(niftiwr_opts_t){0,path};
      if(THD_write_nifti(out,options) || !THD_is_file(path)) IF_FAIL("cannot write corrected inference bucket");
      DSET_delete(out); free(full);
   }
   if(roi) close_checked(roi);
   close_checked(x->spool);
   fprintf(x->manifest,"# Status: complete\n"); close_checked(x->manifest); free(maps);
   for(i=0;i<h->nthread;i++){
      infer_worker *w=x->workers+i; INSYNC_kernel_free(w->kernel); free(w->data); free(w->pack); free(w->component); free(w->scratch); free(w->st); free(w->draw); free(w->nulls); free(w->maxg); free(w->maxneg); free(w->maxc); free(w->length); free(w->offset); free(w->rows); free(w->nv); free(w->pc); free(w->sample); free(w->member); free(w->count); free(w->component_count); free(w->component_offset); free(w->coverage); free(w->keep); free(w->keep2); free(w->original); free(w->surrogate); free(w->tnulls); free(w->maxt); free(w->spectrum); free(w->fftwork);
   }
   if(x->run_used) for(i=0;i<x->s->nrun;i++){ THD_timeshift_set_free(x->shift[i]); THD_phase_set_free(x->phase[i]); }
   free(x->shift); free(x->phase); free(x->run_used); free(x->run_seed); free(x->subject_at);
   THD_perm_result_free(x->temporal_result); free(x->temporal_valid); free(x->temporal_q); free(x->temporal_keep);
   free(x->workers); free(x->group_valid); free(x->condition_valid); free(x->group_q); free(x->condition_q); free(x->maxneg);
   THD_perm_result_free(x->group_result); THD_perm_result_free(x->condition_result); THD_perm_set_free(x->group_set); THD_perm_set_free(x->condition_set); THD_resample_set_free(x->boot_set);
   for(b=0;b<x->nout;b++) free(x->labels[b]);
   free(x->labels); free(x->label_condition); DSET_delete(x->reference); free(x);
}
