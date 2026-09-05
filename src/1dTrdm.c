#include "mrilib.h"
#include "thd_datatable.h"
#include "thd_mapinfer.h"
#include "thd_simmatrix.h"

#ifdef USE_OMP
# include <omp.h>
#endif

/*----------------------------------------------------------------------------
  1dTrdm: labeled temporal representational-dissimilarity movies.

  The application owns generic observation/time/feature axes and delegates the
  numerical RDM kernels to thd_simmatrix. Cross-temporal RDM recurrence and
  cross-time crossnobis are separate descriptive products; neither is decoding
  train/test generalization.
                                                    -- P Molfese, Aug 2026
------------------------------------------------------------------------------*/

#define TRDM_VERSION "4"
#define REDUCE_MEAN   1
#define REDUCE_CONCAT 2
#define CENTER_NONE   0
#define CENTER_SUBJ   1
#define CENTER_PART   2
#define MET_CROSSNOBIS 100
#define TNULL_SUBJECTS   1
#define TNULL_CONDITIONS 2

typedef struct {
   int nrow,nsub,ncond,has_part ;
   char **subj,**obs,**cond,**part,**file ;
   int *isub,*icond,*ipart,*npart,*order ;
   char ***part_lab ;
   float **x ;
   char *source ;
} TRDM_obs ;

typedef struct {
   int n ; int *index ; double *value ; char **label ; char *unit,*source ;
} TRDM_time ;

typedef struct {
   int n ; char **label ; char *source ;
} TRDM_feat ;

typedef struct {
   int n ; char **label ; int *nfeat,**feat ; char *source ;
} TRDM_neigh ;

typedef struct {
   int nwin,nperm,is_exact,null_type,cmp ;
   float *effect,*stat,*p,*q,*pfwe,*rfit,*zfit ;
   char *model_file,*axis_file ;
} TRDM_infer ;

static void usage_1dTrdm(int detail)
{
   (void)detail;
   printf(
"1dTrdm: labeled temporal representational-dissimilarity movies\n"
"\n"
"OVERVIEW\n"
"--------\n"
"1dTrdm follows how the geometry of experimental conditions changes over\n"
"time. At each latency or time window, it averages independently estimated\n"
"observations into condition patterns and converts those patterns into a\n"
"representational dissimilarity matrix (RDM). The resulting RDM movie shows\n"
"when conditions become distinguishable, when a hypothesized organization\n"
"emerges, and whether an earlier representational geometry later recurs.\n"
"\n"
"This organization is especially useful for EEG and MEG analyses. Each input\n"
"observation can be an epoched trial or independently estimated trial pattern,\n"
"with time along rows and sensors, source vertices, frequency features, or\n"
"other measurements along columns. The same contract can also describe ECoG,\n"
"intracranial recordings, time-resolved fMRI beta estimates, or any labeled\n"
"feature-by-time data. 1dTrdm operates on prepared numeric matrices: artifact\n"
"handling, epoching, baseline correction, and first-level trial estimation\n"
"remain in the acquisition/preprocessing software best suited to the modality.\n"
"\n"
"There are four related questions the program can address:\n"
"\n"
"  1. RDM movie       | What is the condition geometry at each time/window?\n"
"  2. Model inference | When does that geometry match a labeled model RDM?\n"
"  3. RDM dynamics    | Does a representational geometry persist or recur?\n"
"  4. Cross-time      | Does a condition contrast generalize across times\n"
"     crossnobis      | when estimated in independent partitions?\n"
"\n"
"The last two are symmetric descriptive RSA products. They are not classifier\n"
"train-time x test-time decoding. Subject sign flips or synchronized condition\n"
"relabeling provide corrected inference for the one-time fixed-model series;\n"
"cross-temporal inference is not performed in this release.\n"
"\n"
"Usage:\n"
"  1dTrdm -obs_table OBS.txt -time_axis TIME.txt -feature_axis FEAT.txt \\\n"
"         -metric corr|cosine|euclid|crossnobis -prefix OUT [options]\n"
"\n"
"OBS.txt has one independently estimated observation per row:\n"
"\n"
"  Subj  Observation  Condition  Partition  InputFile\n"
"  s01   tr0001       face       run1       s01_tr0001.1D\n"
"\n"
"Subj, Observation, Condition and InputFile are required. Observation IDs are\n"
"unique within subject. Partition is optional for ordinary RDMs and required\n"
"for crossnobis. InputFile is time rows x feature columns; every input must\n"
"have the same finite shape. Relative paths resolve beside OBS.txt.\n"
"\n"
"TIME.txt is a strict four-column table with one row per input time sample:\n"
"\n"
"  time_index  time_value  time_unit  time_label\n"
"  0           -0.100      s          -100ms\n"
"  1            0.000      s          0ms\n"
"\n"
"Indices are zero-based/contiguous, values finite/strictly increasing, one unit\n"
"applies to the axis, and labels are unique. FEAT.txt has at least one unique\n"
"feature_label column. Optional metadata are retained but never treated as\n"
"implicit adjacency; -feature_neighborhoods supplies the explicit graph.\n"
"\n"
"NEIGH.txt is a strict two-column membership graph; overlaps are allowed:\n"
"\n"
"  Neighborhood   Feature\n"
"  left_temporal  MEG0111\n"
"  left_temporal  MEG0121\n"
"  posterior      MEG0121\n"
"\n"
"Each Feature must match FEAT.txt. Neighborhood and membership row order do\n"
"not affect results. No coordinate system or column adjacency is inferred.\n"
"\n"
"Metrics:\n"
"  corr        1 - Pearson correlation between condition patterns\n"
"  cosine      1 - cosine similarity\n"
"  euclid      Euclidean distance\n"
"  crossnobis  crossvalidated squared Euclidean over independent partitions;\n"
"              every subject x partition must contain every condition\n"
"\n"
"Options:\n"
"  -window_width N       samples per output window [1]\n"
"  -window_step N        samples between window starts [1]\n"
"  -window_reduce mean|concat\n"
"                        mean features over samples [default], or concatenate\n"
"                        sample x feature values in time-major order\n"
"  -center_conditions none|subject|partition\n"
"                        none [default]; subject is ordinary-RDM cocktail-blank\n"
"                        removal after condition averaging; partition applies\n"
"                        the same operation separately in every crossnobis fold\n"
"  -feature_neighborhoods NEIGH.txt\n"
"                        add an explicit, possibly overlapping feature search;\n"
"                        NEIGH.txt has exactly Neighborhood Feature columns,\n"
"                        with one membership per row. Feature labels must occur\n"
"                        in FEAT.txt; row order and column adjacency are ignored.\n"
"  -model_series_out independent\n"
"                        also average subject RDMs and write OUT.model_series.1D\n"
"                        plus per-time matrices for 3dRSA. The literal word\n"
"                        'independent' is a required assertion that these\n"
"                        subjects are independent of the downstream 3dRSA sample.\n"
"                        Same-subject fusion needs LOO/subject-indexed models and\n"
"                        is not made valid by group averaging. This all-feature\n"
"                        bridge is rejected with -feature_neighborhoods.\n"
"  -rdm_dynamics pearson|spearman\n"
"                        correlate each subject's RDM triangles across every\n"
"                        pair of output windows (representational recurrence)\n"
"  -cross_time_crossnobis\n"
"                        for -metric crossnobis, estimate each condition dyad\n"
"                        across pairs of windows using ordered independent\n"
"                        partition pairs; diagonal equals the primary RDM\n"
"  -model_mat MODEL.1D -model_conditions MODEL_CONDITIONS.1D\n"
"                        compare each subject/window RDM with one labeled fixed\n"
"                        condition-dissimilarity model and run temporal inference\n"
"  -compare spearman|pearson\n"
"                        RDM-triangle comparison [spearman]\n"
"  -temporal_null subjects|conditions\n"
"                        subjects [default] tests a population using synchronized\n"
"                        subject sign flips and the one-sample t of Fisher-z fits;\n"
"                        conditions applies one condition relabeling to every\n"
"                        subject/window and tests the fixed observed sample\n"
"  -nperm N              requested null draws [10000]; small groups are exact\n"
"  -seed S               random-draw seed [1234567]\n"
"  -subject_matrices yes|no\n"
"                        also write each subject/window square matrix [yes]\n"
"  -jobs N               OpenMP workers when available [runtime default]\n"
"  -quiet                suppress informational messages\n"
"  -help                 show this help\n"
"\n"
"OUTPUTS\n"
"-------\n"
"Output file                              | Written when       | Contents\n"
"-----------------------------------------|--------------------|-----------------------------\n"
"OUT.trdm.1D                              | always             | subject x time x RDM dyads\n"
"OUT.trdm.meta                            | always             | estimator/axis provenance\n"
"OUT.trdm.time.1D                         | always             | windows and member bounds\n"
"OUT.trdm.conditions.1D                   | always             | lexical condition axis\n"
"OUT.trdm.features.1D                     | always             | input feature axis\n"
"OUT.trdm.counts.1D                       | always             | observations per estimate\n"
"OUT.trdm.neighborhoods.1D                | -feature_...       | neighborhood RDM movie\n"
"OUT.trdm.neighborhood_axis.1D            | -feature_...       | graph memberships/axis\n"
"OUT_s####_<SUBJ>_t####.1D                | matrices=yes       | subject/window square RDM\n"
"-----------------------------------------|--------------------|-----------------------------\n"
"OUT.trdm.fits.1D                         | -model_mat         | subject fixed-model fits\n"
"OUT.trdm.inference.1D                    | -model_mat         | time-family p/q/max-FWE\n"
"OUT.model_series.1D                      | independent bridge | ordered RDM list for 3dRSA\n"
"OUT_group_t####.1D                       | independent bridge | group-mean fixed model RDM\n"
"-----------------------------------------|--------------------|-----------------------------\n"
"OUT.trdm.dynamics.1D                     | -rdm_dynamics      | unique time x time recurrence\n"
"OUT_s####_<SUBJ>_dynamics.1D             | dynamics+matrices  | mirrored recurrence matrix\n"
"OUT.trdm.cross_time_crossnobis.1D        | -cross_time_...    | time x time x condition dyad\n"
"OUT.trdm.neighborhood_{fits,inference}.1D| graph + model      | joint time x graph inference\n"
"OUT.trdm.neighborhood_dynamics.1D        | graph + dynamics   | recurrence within each graph\n"
"OUT.trdm.neighborhood_cross_time_...1D   | graph + cross-time | crossnobis within each graph\n"
"\n"
"MODEL_CONDITIONS.1D is a strict two-column table in MODEL.1D row order:\n"
"  ConditionIndex Condition\n"
"  0              face\n"
"Labels must match the observation conditions exactly; arbitrary model order\n"
"is realigned by label. Inference Effect is tanh(mean subject Fisher-z fit).\n"
"For the subject null Stat is its one-sample t; for the condition null Stat is\n"
"mean Fisher z. P is two-sided empirical. Q and PFWE span the complete time\n"
"family, or the complete time x neighborhood family when a graph is supplied;\n"
"PFWE uses one synchronized maximum-statistic null over every declared cell.\n"
"\n"
"EXAMPLE: EEG/MEG RDM MOVIE -> fMRI RSA\n"
"--------------------------------------\n"
"First, turn an independently sampled EEG/MEG observation set into an ordered\n"
"fixed RDM series. The literal 'independent' assertion documents that these\n"
"subjects are not the subjects in the later fMRI analysis:\n"
"\n"
"  1dTrdm -obs_table meg_observations.txt \\\n"
"    -time_axis meg_time.txt -feature_axis meg_sensors.txt \\\n"
"    -metric crossnobis -center_conditions partition \\\n"
"    -model_series_out independent -prefix meg_rdm\n"
"\n"
"Then use the generated ordered list as a time-resolved fixed model in 3dRSA:\n"
"\n"
"  3dRSA -mode RSA -dataTableFile fmri_conditions.txt \\\n"
"    -mask gray_mask+tlrc -model_series meg_rdm.model_series.1D \\\n"
"    -metric spearman -nperm 10000 -prefix meg_fmri_fusion\n"
"\n"
"3dRSA then owns the joint time x ROI/searchlight inference family. If the\n"
"EEG/MEG and fMRI subjects overlap, do not use the group-mean bridge: a valid\n"
"same-subject fusion requires leave-one-subject or subject-indexed model RDMs\n"
"so that a subject does not help construct the model used to test that subject.\n"
"\n"
"The primary RDMs are subject-level. 1dTrdm does not estimate trial responses,\n"
"read raw FIF/SET/vendor formats, or implement decoding train/test\n"
"generalization. Cross-temporal outputs are descriptive in this release.\n"
   ) ;
   PRINT_COMPILE_DATE ;
}

static int strptr_cmp(const void *a,const void *b)
{
   return strcmp(*(char * const *)a,*(char * const *)b) ;
}

static char *path_dir(char *p)
{
   char *d=strdup(p),*s ; if(d==NULL) return NULL ;
   s=strrchr(d,'/') ; if(s!=NULL){ if(s==d) s[1]='\0'; else *s='\0'; }
   else strcpy(d,".") ; return d ;
}

static char *resolve_path(char *base,char *p)
{
   char *d,*out ; size_t n ;
   if(p[0]=='/') return strdup(p) ;
   d=path_dir(base) ; n=strlen(d)+strlen(p)+2 ; out=(char *)malloc(n) ;
   snprintf(out,n,"%s/%s",d,p) ; free(d) ; return out ;
}

static char *safe_label(char *s)
{
   char *o=strdup(s) ; int i ;
   for(i=0;o[i]!='\0';i++) if(!isalnum((unsigned char)o[i]) && o[i]!='_' && o[i]!='-') o[i]='_' ;
   return o ;
}

static int find_label(char **a,int n,char *s)
{
   int i ; for(i=0;i<n;i++) if(strcmp(a[i],s)==0) return i ; return -1 ;
}

static int record_after(TRDM_obs *o,int ia,int ib)
{
   int z ;
   if(o->isub[ia]!=o->isub[ib])return o->isub[ia]>o->isub[ib];
   if(o->ipart[ia]!=o->ipart[ib])return o->ipart[ia]>o->ipart[ib];
   if(o->icond[ia]!=o->icond[ib])return o->icond[ia]>o->icond[ib];
   z=strcmp(o->obs[ia],o->obs[ib]);if(z!=0)return z>0;
   return strcmp(o->file[ia],o->file[ib])>0;
}

static void make_record_order(TRDM_obs *o)
{
   int i,j,k;o->order=(int *)malloc(sizeof(int)*o->nrow);
   for(i=0;i<o->nrow;i++)o->order[i]=i;
   /* Stable insertion sort keeps reduction order independent of table rows. */
   for(i=1;i<o->nrow;i++){
     k=o->order[i];j=i-1;
     while(j>=0&&record_after(o,o->order[j],k)){o->order[j+1]=o->order[j];j--;}
     o->order[j+1]=k;
   }
}

static void unique_sorted(char **raw,int n,char ***out,int *nout)
{
   char **a=NULL ; int i,k=0 ;
   for(i=0;i<n;i++) if(find_label(a,k,raw[i])<0){
     a=(char **)realloc(a,sizeof(char *)*(k+1)); a[k++]=strdup(raw[i]);
   }
   qsort(a,k,sizeof(char *),strptr_cmp) ; *out=a ; *nout=k ;
}

static void free_obs(TRDM_obs *o)
{
   int i,s ; if(o==NULL) return ;
   for(i=0;i<o->nrow;i++){
     free(o->obs[i]); free(o->part[i]); free(o->file[i]); free(o->x?o->x[i]:NULL);
   }
   for(i=0;i<o->nsub;i++) free(o->subj[i]);
   for(i=0;i<o->ncond;i++) free(o->cond[i]);
   if(o->part_lab) for(s=0;s<o->nsub;s++){
     for(i=0;i<o->npart[s];i++) free(o->part_lab[s][i]); free(o->part_lab[s]);
   }
   free(o->subj);free(o->obs);free(o->cond);free(o->part);free(o->file);
   free(o->isub);free(o->icond);free(o->ipart);free(o->npart);free(o->part_lab);free(o->order);
   free(o->x);free(o->source);free(o);
}

static void free_time(TRDM_time *t)
{
   int i;if(!t)return;for(i=0;i<t->n;i++)free(t->label[i]);free(t->label);
   free(t->index);free(t->value);free(t->unit);free(t->source);free(t);
}

static void free_feat(TRDM_feat *f)
{
   int i;if(!f)return;for(i=0;i<f->n;i++)free(f->label[i]);free(f->label);
   free(f->source);free(f);
}

static void free_neigh(TRDM_neigh *g)
{
   int i;if(!g)return;for(i=0;i<g->n;i++){free(g->label[i]);free(g->feat[i]);}
   free(g->label);free(g->nfeat);free(g->feat);free(g->source);free(g);
}

static TRDM_time *read_time_axis(char *fname)
{
   THD_datatable *dt=THD_read_datatable_file(fname); TRDM_time *t ;
   int ii,ci,cv,cu,cl ; char *end,*cell ;
   if(dt==NULL||dt->nrow<1)ERROR_exit("1dTrdm: cannot read a nonempty -time_axis '%s'",fname);
   ci=THD_datatable_column(dt,"time_index");cv=THD_datatable_column(dt,"time_value");
   cu=THD_datatable_column(dt,"time_unit");cl=THD_datatable_column(dt,"time_label");
   if(ci<0||cv<0||cu<0||cl<0||dt->ncol!=4)
     ERROR_exit("1dTrdm: -time_axis '%s' needs exactly time_index time_value time_unit time_label",fname);
   t=(TRDM_time *)calloc(1,sizeof(TRDM_time));t->n=dt->nrow;t->source=strdup(fname);
   t->index=(int *)malloc(sizeof(int)*t->n);t->value=(double *)malloc(sizeof(double)*t->n);
   t->label=(char **)calloc(t->n,sizeof(char *));t->unit=strdup(DT_CELL(dt,0,cu));
   for(ii=0;ii<t->n;ii++){
     long ix;double v;cell=DT_CELL(dt,ii,ci);ix=strtol(cell,&end,10);
     if(end==cell||*end!='\0'||ix<0||ix>INT_MAX)
       ERROR_exit("1dTrdm: -time_axis '%s' row %d has invalid time_index",fname,ii+1);
     if(ix!=ii) ERROR_exit("1dTrdm: -time_axis '%s' row %d has time_index %ld; need %d",fname,ii+1,ix,ii);
     cell=DT_CELL(dt,ii,cv);v=strtod(cell,&end);
     if(end==cell||*end!='\0'||!isfinite(v))
       ERROR_exit("1dTrdm: -time_axis '%s' row %d has non-finite/invalid time_value",fname,ii+1);
     if(ii>0&&v<=t->value[ii-1]) ERROR_exit("1dTrdm: -time_axis values must be strictly increasing");
     if(strcmp(t->unit,DT_CELL(dt,ii,cu))!=0) ERROR_exit("1dTrdm: -time_axis mixes units '%s' and '%s'",t->unit,DT_CELL(dt,ii,cu));
     if(find_label(t->label,ii,DT_CELL(dt,ii,cl))>=0) ERROR_exit("1dTrdm: duplicate time_label '%s'",DT_CELL(dt,ii,cl));
     if(strlen(DT_CELL(dt,ii,cl))>511)ERROR_exit("1dTrdm: time_label at row %d exceeds 511 characters",ii+1);
     t->index[ii]=(int)ix;t->value[ii]=v;t->label[ii]=strdup(DT_CELL(dt,ii,cl));
   }
   THD_free_datatable(dt);return t;
}

static TRDM_feat *read_feat_axis(char *fname)
{
   THD_datatable *dt=THD_read_datatable_file(fname);TRDM_feat *f;int c,ii;
   if(dt==NULL||dt->nrow<1)ERROR_exit("1dTrdm: cannot read a nonempty -feature_axis '%s'",fname);
   c=THD_datatable_column(dt,"feature_label");
   if(c<0)ERROR_exit("1dTrdm: -feature_axis '%s' needs a feature_label column",fname);
   f=(TRDM_feat *)calloc(1,sizeof(TRDM_feat));f->n=dt->nrow;f->source=strdup(fname);
   f->label=(char **)calloc(f->n,sizeof(char *));
   for(ii=0;ii<f->n;ii++){
     if(find_label(f->label,ii,DT_CELL(dt,ii,c))>=0)ERROR_exit("1dTrdm: duplicate feature_label '%s'",DT_CELL(dt,ii,c));
     f->label[ii]=strdup(DT_CELL(dt,ii,c));
   }
   THD_free_datatable(dt);return f;
}

static int int_cmp(const void *a,const void *b)
{
   int x=*(const int *)a,y=*(const int *)b;return (x>y)-(x<y);
}

static TRDM_neigh *read_neighborhoods(char *fname,TRDM_feat *f)
{
   THD_datatable *dt=THD_read_datatable_file(fname);TRDM_neigh *g;char **raw;
   int cn,cf,i,k;
   if(dt==NULL||dt->nrow<1)ERROR_exit("1dTrdm: cannot read a nonempty -feature_neighborhoods '%s'",fname);
   cn=THD_datatable_column(dt,"Neighborhood");cf=THD_datatable_column(dt,"Feature");
   if(cn<0||cf<0||dt->ncol!=2)
     ERROR_exit("1dTrdm: -feature_neighborhoods '%s' needs exactly Neighborhood Feature",fname);
   raw=(char **)malloc(sizeof(char *)*dt->nrow);for(i=0;i<dt->nrow;i++)raw[i]=DT_CELL(dt,i,cn);
   g=(TRDM_neigh *)calloc(1,sizeof(TRDM_neigh));g->source=strdup(fname);
   unique_sorted(raw,dt->nrow,&g->label,&g->n);free(raw);
   g->nfeat=(int *)calloc(g->n,sizeof(int));g->feat=(int **)calloc(g->n,sizeof(int *));
   for(i=0;i<dt->nrow;i++){
     int n=find_label(g->label,g->n,DT_CELL(dt,i,cn));
     int fi=find_label(f->label,f->n,DT_CELL(dt,i,cf));
     if(fi<0)ERROR_exit("1dTrdm: neighborhood '%s' names unknown Feature '%s'",DT_CELL(dt,i,cn),DT_CELL(dt,i,cf));
     for(k=0;k<g->nfeat[n];k++)if(g->feat[n][k]==fi)
       ERROR_exit("1dTrdm: neighborhood '%s' repeats Feature '%s'",g->label[n],f->label[fi]);
     g->feat[n]=(int *)realloc(g->feat[n],sizeof(int)*(g->nfeat[n]+1));g->feat[n][g->nfeat[n]++]=fi;
   }
   for(i=0;i<g->n;i++){
     char *a=safe_label(g->label[i]);qsort(g->feat[i],g->nfeat[i],sizeof(int),int_cmp);
     for(k=0;k<i;k++){char *b=safe_label(g->label[k]);if(strcmp(a,b)==0)
       ERROR_exit("1dTrdm: neighborhoods '%s' and '%s' collide in output filenames",g->label[k],g->label[i]);free(b);}
     free(a);
   }
   THD_free_datatable(dt);return g;
}

static THD_simmat *read_model_aligned(char *mfile,char *afile,TRDM_obs *o)
{
   THD_datatable *dt=THD_read_datatable_file(afile);THD_simmat *raw,*out;
   char **lab;int ci,cl,i,j;char *end,*cell;
   if(dt==NULL||dt->nrow<1)ERROR_exit("1dTrdm: cannot read a nonempty -model_conditions '%s'",afile);
   ci=THD_datatable_column(dt,"ConditionIndex");cl=THD_datatable_column(dt,"Condition");
   if(ci<0||cl<0||dt->ncol!=2)
     ERROR_exit("1dTrdm: -model_conditions '%s' needs exactly ConditionIndex Condition",afile);
   if(dt->nrow!=o->ncond)ERROR_exit("1dTrdm: model condition axis has %d rows; observation axis has %d",dt->nrow,o->ncond);
   lab=(char **)calloc(dt->nrow,sizeof(char *));
   for(i=0;i<dt->nrow;i++){
     long ix;cell=DT_CELL(dt,i,ci);ix=strtol(cell,&end,10);
     if(end==cell||*end!='\0'||ix!=i)ERROR_exit("1dTrdm: model ConditionIndex row %d must be %d",i+1,i);
     if(find_label(lab,i,DT_CELL(dt,i,cl))>=0)ERROR_exit("1dTrdm: duplicate model Condition '%s'",DT_CELL(dt,i,cl));
     lab[i]=strdup(DT_CELL(dt,i,cl));
   }
   for(i=0;i<o->ncond;i++)if(find_label(lab,dt->nrow,o->cond[i])<0)
     ERROR_exit("1dTrdm: model condition axis is missing observation Condition '%s'",o->cond[i]);
   raw=THD_simmat_read_1D(mfile,dt->nrow);out=THD_simmat_new(o->ncond);out->is_dist=1;
   for(i=0;i<o->ncond;i++)for(j=0;j<o->ncond;j++){
     int a=find_label(lab,dt->nrow,o->cond[i]),b=find_label(lab,dt->nrow,o->cond[j]);
     out->mat[(size_t)i*o->ncond+j]=raw->mat[(size_t)a*raw->n+b];
   }
   snprintf(out->name,sizeof(out->name),"%s",mfile);
   for(i=0;i<dt->nrow;i++)free(lab[i]);free(lab);THD_free_datatable(dt);THD_simmat_free(raw);return out;
}

static TRDM_obs *read_obs(char *fname,int need_part)
{
   THD_datatable *dt=THD_read_datatable_file(fname);TRDM_obs *o;
   int cs,co,cc,cp,ii,s;char **raws,**rawc;
   if(dt==NULL||dt->nrow<1)ERROR_exit("1dTrdm: cannot read a nonempty -obs_table '%s'",fname);
   cs=THD_datatable_column(dt,"Subj");co=THD_datatable_column(dt,"Observation");
   cc=THD_datatable_column(dt,"Condition");cp=THD_datatable_column(dt,"Partition");
   if(cs<0||co<0||cc<0||dt->icol_input<0)
     ERROR_exit("1dTrdm: -obs_table '%s' needs Subj Observation Condition InputFile",fname);
   if(need_part&&cp<0)ERROR_exit("1dTrdm: crossnobis needs a Partition column in -obs_table");
   o=(TRDM_obs *)calloc(1,sizeof(TRDM_obs));o->nrow=dt->nrow;o->source=strdup(fname);o->has_part=(cp>=0);
   o->obs=(char **)calloc(o->nrow,sizeof(char *));o->part=(char **)calloc(o->nrow,sizeof(char *));
   o->file=(char **)calloc(o->nrow,sizeof(char *));o->isub=(int *)malloc(sizeof(int)*o->nrow);
   o->icond=(int *)malloc(sizeof(int)*o->nrow);o->ipart=(int *)malloc(sizeof(int)*o->nrow);
   raws=(char **)malloc(sizeof(char *)*o->nrow);rawc=(char **)malloc(sizeof(char *)*o->nrow);
   for(ii=0;ii<o->nrow;ii++){raws[ii]=DT_CELL(dt,ii,cs);rawc[ii]=DT_CELL(dt,ii,cc);}
   unique_sorted(raws,o->nrow,&o->subj,&o->nsub);unique_sorted(rawc,o->nrow,&o->cond,&o->ncond);
   if(o->ncond<2)ERROR_exit("1dTrdm: observation table defines %d condition; need at least 2",o->ncond);
   for(ii=0;ii<o->nrow;ii++){
     o->isub[ii]=find_label(o->subj,o->nsub,raws[ii]);o->icond[ii]=find_label(o->cond,o->ncond,rawc[ii]);
     o->obs[ii]=strdup(DT_CELL(dt,ii,co));o->part[ii]=strdup(cp>=0?DT_CELL(dt,ii,cp):"-");
     o->file[ii]=resolve_path(fname,dt->fname[ii]);
     for(s=0;s<ii;s++)if(o->isub[s]==o->isub[ii]&&strcmp(o->obs[s],o->obs[ii])==0)
       ERROR_exit("1dTrdm: subject '%s' repeats Observation '%s'",o->subj[o->isub[ii]],o->obs[ii]);
   }
   o->npart=(int *)calloc(o->nsub,sizeof(int));o->part_lab=(char ***)calloc(o->nsub,sizeof(char **));
   if(cp>=0)for(s=0;s<o->nsub;s++){
     char **rr=NULL;int n=0;for(ii=0;ii<o->nrow;ii++)if(o->isub[ii]==s){rr=(char **)realloc(rr,sizeof(char *)*(n+1));rr[n++]=o->part[ii];}
     unique_sorted(rr,n,&o->part_lab[s],&o->npart[s]);free(rr);
   }
   for(ii=0;ii<o->nrow;ii++)o->ipart[ii]=(cp>=0)?find_label(o->part_lab[o->isub[ii]],o->npart[o->isub[ii]],o->part[ii]):-1;
   make_record_order(o);
   free(raws);free(rawc);THD_free_datatable(dt);return o;
}

static void validate_ordinary(TRDM_obs *o)
{
   int s,c,i;
   for(s=0;s<o->nsub;s++)for(c=0;c<o->ncond;c++){
     int n=0;for(i=0;i<o->nrow;i++)if(o->isub[i]==s&&o->icond[i]==c)n++;
     if(n<1)ERROR_exit("1dTrdm: subject '%s' has no observation for condition '%s'",o->subj[s],o->cond[c]);
   }
}

static void load_obs_data(TRDM_obs *o,int nt,int nf)
{
   int i,t,f;o->x=(float **)calloc(o->nrow,sizeof(float *));
   for(i=0;i<o->nrow;i++){
     MRI_IMAGE *im=mri_read_1D(o->file[i]);float *a;
     if(im==NULL)ERROR_exit("1dTrdm: cannot read observation InputFile '%s'",o->file[i]);
     if(im->nx!=nt||im->ny!=nf)ERROR_exit("1dTrdm: InputFile '%s' is %d time rows x %d features; need %d x %d",o->file[i],im->nx,im->ny,nt,nf);
     a=MRI_FLOAT_PTR(im);o->x[i]=(float *)malloc(sizeof(float)*(size_t)nt*nf);
     for(t=0;t<nt;t++)for(f=0;f<nf;f++){
       float v=a[(size_t)f*nt+t];if(!isfinite(v))ERROR_exit("1dTrdm: InputFile '%s' has a non-finite cell at time %d feature %d",o->file[i],t,f);
       o->x[i][(size_t)t*nf+f]=v;
     }
     mri_free(im);
   }
}

static void validate_crossnobis(TRDM_obs *o)
{
   int s,p,c,i;for(s=0;s<o->nsub;s++){
     if(o->npart[s]<2)ERROR_exit("1dTrdm: subject '%s' has %d partition; crossnobis needs at least 2",o->subj[s],o->npart[s]);
     for(p=0;p<o->npart[s];p++)for(c=0;c<o->ncond;c++){
       int n=0;for(i=0;i<o->nrow;i++)if(o->isub[i]==s&&o->ipart[i]==p&&o->icond[i]==c)n++;
       if(n<1)ERROR_exit("1dTrdm: balanced crossnobis needs subject '%s', partition '%s', condition '%s'",o->subj[s],o->part_lab[s][p],o->cond[c]);
     }
   }
}

static void window_vec(float *x,int nf0,int start,int width,int reduce,
                       int *find,int nfind,float *out)
{
   int t,f;if(reduce==REDUCE_CONCAT){
     for(t=0;t<width;t++)for(f=0;f<nfind;f++)
       out[(size_t)t*nfind+f]=x[(size_t)(start+t)*nf0+(find?find[f]:f)];
   }else for(f=0;f<nfind;f++){
     int ff=find?find[f]:f;double z=0.0;
     for(t=0;t<width;t++)z+=x[(size_t)(start+t)*nf0+ff];out[f]=(float)(z/width);
   }
}

static void center_patterns(float *F,int nc,int nf)
{
   int c,f;for(f=0;f<nf;f++){double m=0.0;for(c=0;c<nc;c++)m+=F[(size_t)c*nf+f];m/=nc;for(c=0;c<nc;c++)F[(size_t)c*nf+f]-=(float)m;}
}

static float **make_partition_patterns(TRDM_obs *o,int s,int start,int width,
                                        int reduce,int center,int nf0,int *find,
                                        int nfind,int *nf_out)
{
   int nc=o->ncond,nf=(reduce==REDUCE_CONCAT)?width*nfind:nfind,np=o->npart[s];
   int i,c,p,f,**cnt=(int **)calloc(np,sizeof(int *));
   float **pat=(float **)calloc(np,sizeof(float *));
   float *tmp=(float *)malloc(sizeof(float)*nf);
   for(p=0;p<np;p++){
     cnt[p]=(int *)calloc(nc,sizeof(int));
     pat[p]=(float *)calloc((size_t)nc*nf,sizeof(float));
   }
   for(i=0;i<o->nrow;i++){
     int j=o->order[i];
     if(o->isub[j]!=s)continue;
     p=o->ipart[j];c=o->icond[j];
     window_vec(o->x[j],nf0,start,width,reduce,find,nfind,tmp);
     for(f=0;f<nf;f++)pat[p][(size_t)c*nf+f]+=tmp[f];
     cnt[p][c]++;
   }
   for(p=0;p<np;p++){
     for(c=0;c<nc;c++){
       float z=1.0f/cnt[p][c];
       for(f=0;f<nf;f++)pat[p][(size_t)c*nf+f]*=z;
     }
     if(center==CENTER_PART)center_patterns(pat[p],nc,nf);
     free(cnt[p]);
   }
   free(cnt);free(tmp);*nf_out=nf;return pat;
}

static void free_partition_patterns(float **pat,int np)
{
   int p;if(!pat)return;for(p=0;p<np;p++)free(pat[p]);free(pat);
}

static THD_simmat *build_one(TRDM_obs *o,int s,int start,int width,int reduce,
                             int metric,int center,int nf0,int *find,int nfind)
{
   int nc=o->ncond,nf=(reduce==REDUCE_CONCAT)?width*nfind:nfind,i,c,p;float *tmp=(float *)malloc(sizeof(float)*nf);
   THD_simmat *sm=NULL;
   if(metric!=MET_CROSSNOBIS){
     float *F=(float *)calloc((size_t)nc*nf,sizeof(float));int *cnt=(int *)calloc(nc,sizeof(int));
     for(i=0;i<o->nrow;i++){int j=o->order[i];if(o->isub[j]==s){c=o->icond[j];window_vec(o->x[j],nf0,start,width,reduce,find,nfind,tmp);for(p=0;p<nf;p++)F[(size_t)c*nf+p]+=tmp[p];cnt[c]++;}}
     for(c=0;c<nc;c++){float z=1.0f/cnt[c];for(p=0;p<nf;p++)F[(size_t)c*nf+p]*=z;}
     if(center==CENTER_SUBJ)center_patterns(F,nc,nf);
     sm=THD_simmat_from_features(nc,nf,F,metric);free(F);free(cnt);
     if(sm&&metric!=SIM_EUCLID){for(i=0;i<nc;i++)for(c=0;c<nc;c++)sm->mat[(size_t)i*nc+c]=(i==c)?0.0f:1.0f-sm->mat[(size_t)i*nc+c];sm->is_dist=1;}
   }else{
     int np=o->npart[s],nfpat;float **pat=make_partition_patterns(o,s,start,width,reduce,center,nf0,find,nfind,&nfpat);
     sm=THD_simmat_crossnobis(nc,np,nfpat,pat);
     free_partition_patterns(pat,np);
   }
   free(tmp);return sm;
}

static THD_simmat *build_dynamics(THD_simmat **rdm,int nwin,int ncond,int cmp)
{
   int a,b,m=THD_NTRI(ncond);THD_simmat *out=THD_simmat_new(nwin);
   float *ta=(float *)malloc(sizeof(float)*m),*tb=(float *)malloc(sizeof(float)*m);
   float *sa=(float *)malloc(sizeof(float)*m),*sb=(float *)malloc(sizeof(float)*m);
   for(a=0;a<nwin;a++){
     THD_simmat_to_tri(rdm[a],ta);
     for(b=a;b<nwin;b++){
       float r;THD_simmat_to_tri(rdm[b],tb);
       r=THD_tri_corr(m,ta,tb,cmp,sa,sb);
       out->mat[(size_t)a*nwin+b]=out->mat[(size_t)b*nwin+a]=r;
     }
   }
   out->is_dist=0;free(ta);free(tb);free(sa);free(sb);return out;
}

static float cross_time_crossnobis_value(float **pa,float **pb,int np,int nc,
                                          int nf,int ca,int cb)
{
   int p,q,f;double z=0.0;
   for(p=0;p<np;p++)for(q=0;q<np;q++)if(p!=q){
     double dot=0.0;
     for(f=0;f<nf;f++){
       double da=(double)pa[p][(size_t)ca*nf+f]-pa[p][(size_t)cb*nf+f];
       double db=(double)pb[q][(size_t)ca*nf+f]-pb[q][(size_t)cb*nf+f];
       dot+=da*db;
     }
     z+=dot;
   }
   return (float)(z/((double)np*(np-1)*nf));
}

static void write_matrix_checked(char *fname,THD_simmat *sm)
{
   if(!THD_ok_overwrite()&&THD_is_file(fname))ERROR_exit("1dTrdm: output file '%s' already exists",fname);
   if(!THD_simmat_write_1D(fname,sm))ERROR_exit("1dTrdm: cannot write '%s'",fname);
}

static void check_available(char *fname)
{
   if(!THD_ok_overwrite()&&THD_is_file(fname))ERROR_exit("1dTrdm: output file '%s' already exists",fname);
}

static void preflight_outputs(char *prefix,TRDM_obs *o,int nwin,int series,int matrices,
                              int infer,int dynamics,int cross_time,TRDM_neigh *g)
{
   char fn[THD_MAX_NAME];int s,t;
   char *suffix[]={".trdm.1D",".trdm.time.1D",".trdm.conditions.1D",
                   ".trdm.features.1D",".trdm.counts.1D",".trdm.meta"};
   for(t=0;t<6;t++){snprintf(fn,sizeof(fn),"%s%s",prefix,suffix[t]);check_available(fn);}
   if(matrices)for(s=0;s<o->nsub;s++){
     char *ss=safe_label(o->subj[s]);
     for(t=0;t<nwin;t++){snprintf(fn,sizeof(fn),"%s_s%04d_%s_t%04d.1D",prefix,s,ss,t);check_available(fn);}
     free(ss);
   }
   if(series){
     snprintf(fn,sizeof(fn),"%s.model_series.1D",prefix);check_available(fn);
     for(t=0;t<nwin;t++){snprintf(fn,sizeof(fn),"%s_group_t%04d.1D",prefix,t);check_available(fn);}
   }
   if(infer&&!g){snprintf(fn,sizeof(fn),"%s.trdm.fits.1D",prefix);check_available(fn);snprintf(fn,sizeof(fn),"%s.trdm.inference.1D",prefix);check_available(fn);}
   if(dynamics){
     snprintf(fn,sizeof(fn),"%s.trdm.dynamics.1D",prefix);check_available(fn);
     if(matrices)for(s=0;s<o->nsub;s++){
       char *ss=safe_label(o->subj[s]);
       snprintf(fn,sizeof(fn),"%s_s%04d_%s_dynamics.1D",prefix,s,ss);check_available(fn);free(ss);
     }
   }
   if(cross_time){snprintf(fn,sizeof(fn),"%s.trdm.cross_time_crossnobis.1D",prefix);check_available(fn);}
   if(g){
     snprintf(fn,sizeof(fn),"%s.trdm.neighborhoods.1D",prefix);check_available(fn);
     snprintf(fn,sizeof(fn),"%s.trdm.neighborhood_axis.1D",prefix);check_available(fn);
     if(infer){snprintf(fn,sizeof(fn),"%s.trdm.neighborhood_fits.1D",prefix);check_available(fn);snprintf(fn,sizeof(fn),"%s.trdm.neighborhood_inference.1D",prefix);check_available(fn);}
     if(dynamics){snprintf(fn,sizeof(fn),"%s.trdm.neighborhood_dynamics.1D",prefix);check_available(fn);}
     if(cross_time){snprintf(fn,sizeof(fn),"%s.trdm.neighborhood_cross_time_crossnobis.1D",prefix);check_available(fn);}
     if(matrices)for(s=0;s<o->nsub;s++){
       char *ss=safe_label(o->subj[s]);int n;
       for(n=0;n<g->n;n++){
         char *gg=safe_label(g->label[n]);
         for(t=0;t<nwin;t++){snprintf(fn,sizeof(fn),"%s_s%04d_%s_n%04d_%s_t%04d.1D",prefix,s,ss,n,gg,t);check_available(fn);}
         if(dynamics){snprintf(fn,sizeof(fn),"%s_s%04d_%s_n%04d_%s_dynamics.1D",prefix,s,ss,n,gg);check_available(fn);}
         free(gg);
       }
       free(ss);
     }
   }
}

static FILE *open_output(char *fname)
{
   FILE *fp;
   if(!THD_ok_overwrite()&&THD_is_file(fname))ERROR_exit("1dTrdm: output file '%s' already exists",fname);
   fp=fopen(fname,"w");if(fp==NULL)ERROR_exit("1dTrdm: cannot write '%s'",fname);return fp;
}

static int positive_option(char *s,char *opt)
{
   char *end;long v=strtol(s,&end,10);
   if(end==s||*end!='\0'||v<1||v>INT_MAX)ERROR_exit("1dTrdm: %s needs a positive integer",opt);
   return (int)v;
}

static long long_option(char *s,char *opt)
{
   char *end;long v=strtol(s,&end,10);
   if(end==s||*end!='\0')ERROR_exit("1dTrdm: %s needs an integer",opt);return v;
}

static float trdm_fisher(float r)
{
   if(r < -0.999329f)return -4.0f;if(r > 0.999329f)return 4.0f;return atanhf(r);
}

static void free_infer(TRDM_infer *x)
{
   if(!x)return;free(x->effect);free(x->stat);free(x->p);free(x->q);free(x->pfwe);
   free(x->rfit);free(x->zfit);free(x->model_file);free(x->axis_file);free(x);
}

static TRDM_infer *run_inference(THD_simmat ***rdm,TRDM_obs *o,int nwin,
                                  THD_simmat *model,int cmp,int null_type,
                                  int nperm,long seed,char *mfile,char *afile,
                                  char *family,int quiet)
{
   TRDM_infer *x;PERM_scheme *sch;PERM_set *set;float *mt,*tri,*sc1,*sc2,*nul,*mx;
   int m=THD_NTRI(o->ncond),s,t,p,np;double mm=0.0,m2=0.0;
   if(o->ncond<3)ERROR_exit("1dTrdm: temporal model inference needs at least 3 conditions");
   if(null_type==TNULL_SUBJECTS&&o->nsub<2)ERROR_exit("1dTrdm: -temporal_null subjects needs at least 2 subjects");
   mt=(float *)malloc(sizeof(float)*m);tri=(float *)malloc(sizeof(float)*m);
   sc1=(float *)malloc(sizeof(float)*m);sc2=(float *)malloc(sizeof(float)*m);THD_simmat_to_tri(model,mt);
   for(p=0;p<m;p++){mm+=mt[p];m2+=(double)mt[p]*mt[p];}
   if(m2-mm*mm/m<=1.0e-20)ERROR_exit("1dTrdm: -model_mat has a constant dissimilarity triangle");
   x=(TRDM_infer *)calloc(1,sizeof(TRDM_infer));x->nwin=nwin;x->null_type=null_type;x->cmp=cmp;
   x->effect=(float *)calloc(nwin,sizeof(float));x->stat=(float *)calloc(nwin,sizeof(float));
   x->p=(float *)calloc(nwin,sizeof(float));x->q=(float *)calloc(nwin,sizeof(float));x->pfwe=(float *)calloc(nwin,sizeof(float));
   x->rfit=(float *)calloc((size_t)o->nsub*nwin,sizeof(float));x->zfit=(float *)calloc((size_t)o->nsub*nwin,sizeof(float));
   x->model_file=strdup(mfile);x->axis_file=strdup(afile);
   for(s=0;s<o->nsub;s++)for(t=0;t<nwin;t++){
     float r;THD_simmat_to_tri(rdm[s][t],tri);r=THD_tri_corr(m,tri,mt,cmp,sc1,sc2);
     x->rfit[(size_t)s*nwin+t]=r;x->zfit[(size_t)s*nwin+t]=trdm_fisher(r);
   }
   for(t=0;t<nwin;t++){double z=0.0;for(s=0;s<o->nsub;s++)z+=x->zfit[(size_t)s*nwin+t];x->effect[t]=tanhf((float)(z/o->nsub));}
   free(tri);free(sc1);free(sc2);
   sch=THD_perm_scheme_new(null_type==TNULL_SUBJECTS?o->nsub:o->ncond);
   if(sch==NULL)ERROR_exit("1dTrdm: cannot build temporal inference scheme");
   sch->exchange=(null_type==TNULL_SUBJECTS)?PERM_ISE:PERM_EE;sch->exact=0;sch->nperm=nperm;sch->seed=seed;
   set=THD_perm_set_build(sch);THD_perm_scheme_free(sch);if(set==NULL)ERROR_exit("1dTrdm: cannot build temporal inference relabelings");
   x->nperm=np=set->nperm;x->is_exact=set->is_exact;nul=(float *)calloc((size_t)nwin*np,sizeof(float));
   if(null_type==TNULL_SUBJECTS){
#ifdef USE_OMP
# pragma omp parallel for schedule(static)
#endif
     for(t=0;t<nwin;t++){
       float *v=(float *)malloc(sizeof(float)*o->nsub);THD_permstat ps;int ss;
       for(ss=0;ss<o->nsub;ss++)v[ss]=x->zfit[(size_t)ss*nwin+t];
       ps=THD_signflip_t(o->nsub,v,set,nul+(size_t)t*np);x->stat[t]=ps.stat;x->p[t]=ps.pval;free(v);
     }
   }else{
#ifdef USE_OMP
# pragma omp parallel for schedule(static)
#endif
     for(t=0;t<nwin;t++){
       float *yt=(float *)malloc(sizeof(float)*m),*a=(float *)malloc(sizeof(float)*m),*b=(float *)malloc(sizeof(float)*m);int pp,ss,k;
       double zo=0.0;for(ss=0;ss<o->nsub;ss++)zo+=x->zfit[(size_t)ss*nwin+t];x->stat[t]=(float)(zo/o->nsub);
       for(pp=0;pp<np;pp++){
         double z=0.0;int *perm=set->perm+(size_t)pp*o->ncond;
         for(ss=0;ss<o->nsub;ss++){float r;THD_simmat_to_tri_perm(rdm[ss][t],perm,yt);r=THD_tri_corr(m,yt,mt,cmp,a,b);z+=trdm_fisher(r);}
         nul[(size_t)t*np+pp]=fabsf((float)(z/o->nsub));
       }
       {float ao=fabsf(x->stat[t]),tol=64.0f*FLT_EPSILON*(1.0f+ao);
        for(k=0,pp=0;pp<np;pp++){
          float *v=nul+(size_t)t*np+pp;if(fabsf(*v-ao)<=tol)*v=ao;if(*v>=ao)k++;
        }
        x->p[t]=(float)k/np;}
       free(yt);free(a);free(b);
     }
   }
   mx=(float *)calloc(np,sizeof(float));
   for(t=0;t<nwin;t++)THD_max_accum(np,mx,nul+(size_t)t*np);
   for(t=0;t<nwin;t++){int n=0;for(p=0;p<np;p++)if(mx[p]>=fabsf(x->stat[t]))n++;x->pfwe[t]=(float)n/np;}
   THD_bh_fdr(nwin,x->p,x->q);
   if(!quiet)INFO_message("1dTrdm: temporal %s null uses %d %s relabelings over %d-cell %s BH/max-FWE family",
                          null_type==TNULL_SUBJECTS?"subject":"condition",np,set->is_exact?"exact":"sampled",nwin,family);
   free(mt);free(nul);free(mx);THD_perm_set_free(set);return x;
}

int main(int argc,char **argv)
{
   char *obsfile=NULL,*timefile=NULL,*featfile=NULL,*neighfile=NULL,*prefix=NULL,*metric_s=NULL,*modelfile=NULL,*modelaxis=NULL;
   int metric=0,width=1,step=1,reduce=REDUCE_MEAN,center=CENTER_NONE,series=0,quiet=0,jobs=0,matrices=1;
   int cmp=CMP_SPEARMAN,dyn_cmp=0,cross_time=0,null_type=TNULL_SUBJECTS,nperm=10000,infer_opt=0;
   long seed=1234567L;
   int i,s,t,c,a,b,nwin,nfout;TRDM_obs *o;TRDM_time *ta;TRDM_feat *fa;TRDM_neigh *ng=NULL;
   THD_simmat ***rdm,****nrdm=NULL,**dyn=NULL,***ndyn=NULL,*model=NULL;TRDM_infer *inf=NULL,*ninf=NULL;
   int *wstart,*wend;double *wvalue;char **wlabel;
   mainENTRY("1dTrdm main");machdep();
   if(argc<2){usage_1dTrdm(1);return 0;}
   for(i=1;i<argc;){
     if(strcmp(argv[i],"-help")==0||strcmp(argv[i],"-h")==0){usage_1dTrdm(2);return 0;}
     if(strcmp(argv[i],"-obs_table")==0){if(++i>=argc)ERROR_exit("need FILE after -obs_table");obsfile=argv[i++];continue;}
     if(strcmp(argv[i],"-time_axis")==0){if(++i>=argc)ERROR_exit("need FILE after -time_axis");timefile=argv[i++];continue;}
     if(strcmp(argv[i],"-feature_axis")==0){if(++i>=argc)ERROR_exit("need FILE after -feature_axis");featfile=argv[i++];continue;}
     if(strcmp(argv[i],"-feature_neighborhoods")==0){if(++i>=argc)ERROR_exit("need FILE after -feature_neighborhoods");neighfile=argv[i++];continue;}
     if(strcmp(argv[i],"-metric")==0){if(++i>=argc)ERROR_exit("need metric after -metric");metric_s=argv[i++];continue;}
     if(strcmp(argv[i],"-prefix")==0){if(++i>=argc)ERROR_exit("need prefix after -prefix");prefix=argv[i++];continue;}
     if(strcmp(argv[i],"-window_width")==0){if(++i>=argc)ERROR_exit("need N after -window_width");width=positive_option(argv[i++],"-window_width");continue;}
     if(strcmp(argv[i],"-window_step")==0){if(++i>=argc)ERROR_exit("need N after -window_step");step=positive_option(argv[i++],"-window_step");continue;}
     if(strcmp(argv[i],"-window_reduce")==0){if(++i>=argc)ERROR_exit("need mean|concat after -window_reduce");if(strcmp(argv[i],"mean")==0)reduce=REDUCE_MEAN;else if(strcmp(argv[i],"concat")==0)reduce=REDUCE_CONCAT;else ERROR_exit("-window_reduce must be mean or concat");i++;continue;}
     if(strcmp(argv[i],"-center_conditions")==0){if(++i>=argc)ERROR_exit("need none|subject|partition");if(strcmp(argv[i],"none")==0)center=CENTER_NONE;else if(strcmp(argv[i],"subject")==0)center=CENTER_SUBJ;else if(strcmp(argv[i],"partition")==0)center=CENTER_PART;else ERROR_exit("-center_conditions must be none, subject, or partition");i++;continue;}
     if(strcmp(argv[i],"-model_series_out")==0){if(++i>=argc||strcmp(argv[i],"independent")!=0)ERROR_exit("-model_series_out requires the literal assertion 'independent'");series=1;i++;continue;}
     if(strcmp(argv[i],"-rdm_dynamics")==0){if(++i>=argc)ERROR_exit("need pearson|spearman after -rdm_dynamics");if(strcmp(argv[i],"pearson")==0)dyn_cmp=CMP_PEARSON;else if(strcmp(argv[i],"spearman")==0)dyn_cmp=CMP_SPEARMAN;else ERROR_exit("-rdm_dynamics must be pearson or spearman");i++;continue;}
     if(strcmp(argv[i],"-cross_time_crossnobis")==0){cross_time=1;i++;continue;}
     if(strcmp(argv[i],"-model_mat")==0){if(++i>=argc)ERROR_exit("need FILE after -model_mat");modelfile=argv[i++];continue;}
     if(strcmp(argv[i],"-model_conditions")==0){if(++i>=argc)ERROR_exit("need FILE after -model_conditions");modelaxis=argv[i++];continue;}
     if(strcmp(argv[i],"-compare")==0){if(++i>=argc)ERROR_exit("need spearman|pearson after -compare");if(strcmp(argv[i],"spearman")==0)cmp=CMP_SPEARMAN;else if(strcmp(argv[i],"pearson")==0)cmp=CMP_PEARSON;else ERROR_exit("-compare must be spearman or pearson");infer_opt=1;i++;continue;}
     if(strcmp(argv[i],"-temporal_null")==0){if(++i>=argc)ERROR_exit("need subjects|conditions after -temporal_null");if(strcmp(argv[i],"subjects")==0)null_type=TNULL_SUBJECTS;else if(strcmp(argv[i],"conditions")==0)null_type=TNULL_CONDITIONS;else ERROR_exit("-temporal_null must be subjects or conditions");infer_opt=1;i++;continue;}
     if(strcmp(argv[i],"-nperm")==0){if(++i>=argc)ERROR_exit("need N after -nperm");nperm=positive_option(argv[i++],"-nperm");infer_opt=1;continue;}
     if(strcmp(argv[i],"-seed")==0){if(++i>=argc)ERROR_exit("need S after -seed");seed=long_option(argv[i++],"-seed");infer_opt=1;continue;}
     if(strcmp(argv[i],"-subject_matrices")==0){if(++i>=argc)ERROR_exit("need yes|no after -subject_matrices");if(strcmp(argv[i],"yes")==0)matrices=1;else if(strcmp(argv[i],"no")==0)matrices=0;else ERROR_exit("-subject_matrices must be yes or no");i++;continue;}
     if(strcmp(argv[i],"-jobs")==0){if(++i>=argc)ERROR_exit("need N after -jobs");jobs=positive_option(argv[i++],"-jobs");continue;}
     if(strcmp(argv[i],"-quiet")==0){quiet=1;i++;continue;}
     ERROR_message("1dTrdm: illegal option '%s'",argv[i]);suggest_best_prog_option(argv[0],argv[i]);return 1;
   }
   if(!obsfile||!timefile||!featfile||!metric_s||!prefix)ERROR_exit("1dTrdm: need -obs_table, -time_axis, -feature_axis, -metric, and -prefix");
   if((modelfile==NULL)!=(modelaxis==NULL))ERROR_exit("1dTrdm: -model_mat and -model_conditions are required together");
   if(infer_opt&&modelfile==NULL)ERROR_exit("1dTrdm: -compare, -temporal_null, -nperm, and -seed require -model_mat/-model_conditions");
   if(strcmp(metric_s,"corr")==0)metric=SIM_PEARSON;else if(strcmp(metric_s,"cosine")==0)metric=SIM_COSINE;else if(strcmp(metric_s,"euclid")==0)metric=SIM_EUCLID;else if(strcmp(metric_s,"crossnobis")==0)metric=MET_CROSSNOBIS;else ERROR_exit("1dTrdm: -metric must be corr, cosine, euclid, or crossnobis");
   if(metric==MET_CROSSNOBIS&&center==CENTER_SUBJ)ERROR_exit("1dTrdm: crossnobis condition centering is partition, not subject");
   if(metric!=MET_CROSSNOBIS&&center==CENTER_PART)ERROR_exit("1dTrdm: partition centering applies only to crossnobis");
   if(cross_time&&metric!=MET_CROSSNOBIS)ERROR_exit("1dTrdm: -cross_time_crossnobis requires -metric crossnobis");
   ta=read_time_axis(timefile);fa=read_feat_axis(featfile);if(neighfile)ng=read_neighborhoods(neighfile,fa);o=read_obs(obsfile,metric==MET_CROSSNOBIS);
   if(modelfile)model=read_model_aligned(modelfile,modelaxis,o);
   if(width>ta->n)ERROR_exit("1dTrdm: window width %d exceeds %d input samples",width,ta->n);
   if(fa->n>INT_MAX/width)ERROR_exit("1dTrdm: window_width x feature count exceeds the supported integer range");
   if(metric!=MET_CROSSNOBIS&&((reduce==REDUCE_CONCAT)?width*fa->n:fa->n)<2)ERROR_exit("1dTrdm: ordinary RDM metrics need at least 2 output features");
   if(ng&&metric!=MET_CROSSNOBIS)for(i=0;i<ng->n;i++)if(((reduce==REDUCE_CONCAT)?width*ng->nfeat[i]:ng->nfeat[i])<2)
     ERROR_exit("1dTrdm: neighborhood '%s' has too few output features for an ordinary RDM",ng->label[i]);
   load_obs_data(o,ta->n,fa->n);if(metric==MET_CROSSNOBIS)validate_crossnobis(o);else validate_ordinary(o);
   nwin=1+(ta->n-width)/step;nfout=(reduce==REDUCE_CONCAT)?width*fa->n:fa->n;
   if(ng&&ng->n>INT_MAX/nwin)ERROR_exit("1dTrdm: neighborhood x window cell count exceeds the supported integer range");
   if(series&&nwin<2)ERROR_exit("1dTrdm: -model_series_out needs at least 2 output windows for 3dRSA -model_series");
   if(series&&ng)ERROR_exit("1dTrdm: -model_series_out is an all-feature bridge and cannot be combined with -feature_neighborhoods");
   if(dyn_cmp&&nwin<2)ERROR_exit("1dTrdm: -rdm_dynamics needs at least 2 output windows");
   if(dyn_cmp&&o->ncond<3)ERROR_exit("1dTrdm: -rdm_dynamics needs at least 3 conditions");
   if(cross_time&&nwin<2)ERROR_exit("1dTrdm: -cross_time_crossnobis needs at least 2 output windows");
   wstart=(int *)malloc(sizeof(int)*nwin);wend=(int *)malloc(sizeof(int)*nwin);wvalue=(double *)malloc(sizeof(double)*nwin);wlabel=(char **)calloc(nwin,sizeof(char *));
   for(t=0;t<nwin;t++){int k,st=t*step;double v=0.0;char z[32];wstart[t]=st;wend[t]=st+width-1;for(k=st;k<st+width;k++)v+=ta->value[k];wvalue[t]=v/width;if(width==1)wlabel[t]=strdup(ta->label[st]);else{snprintf(z,sizeof(z),"w%04d",t);wlabel[t]=strdup(z);}}
#ifdef USE_OMP
   if(jobs>0)omp_set_num_threads(jobs);
#else
   if(jobs>1&&!quiet)WARNING_message("1dTrdm: built without OpenMP; -jobs ignored");
#endif
   rdm=(THD_simmat ***)calloc(o->nsub,sizeof(THD_simmat **));for(s=0;s<o->nsub;s++)rdm[s]=(THD_simmat **)calloc(nwin,sizeof(THD_simmat *));
#ifdef USE_OMP
# pragma omp parallel for schedule(static)
#endif
   for(i=0;i<o->nsub*nwin;i++){
     int ss=i/nwin,tt=i%nwin;
     rdm[ss][tt]=build_one(o,ss,wstart[tt],width,reduce,metric,center,fa->n,NULL,fa->n);
     if(rdm[ss][tt]==NULL)ERROR_exit("1dTrdm: RDM construction failed for subject %s window %d",o->subj[ss],tt);
   }
   if(dyn_cmp){dyn=(THD_simmat **)calloc(o->nsub,sizeof(THD_simmat *));for(s=0;s<o->nsub;s++)dyn[s]=build_dynamics(rdm[s],nwin,o->ncond,dyn_cmp);}
   if(ng){
     nrdm=(THD_simmat ****)calloc(o->nsub,sizeof(THD_simmat ***));
     for(s=0;s<o->nsub;s++){nrdm[s]=(THD_simmat ***)calloc(ng->n,sizeof(THD_simmat **));for(c=0;c<ng->n;c++)nrdm[s][c]=(THD_simmat **)calloc(nwin,sizeof(THD_simmat *));}
#ifdef USE_OMP
# pragma omp parallel for schedule(static)
#endif
     for(i=0;i<o->nsub*ng->n*nwin;i++){
       int ss=i/(ng->n*nwin),z=i%(ng->n*nwin),gg=z/nwin,tt=z%nwin;
       nrdm[ss][gg][tt]=build_one(o,ss,wstart[tt],width,reduce,metric,center,fa->n,ng->feat[gg],ng->nfeat[gg]);
       if(nrdm[ss][gg][tt]==NULL)ERROR_exit("1dTrdm: RDM construction failed for subject %s neighborhood %s window %d",o->subj[ss],ng->label[gg],tt);
     }
     if(dyn_cmp){ndyn=(THD_simmat ***)calloc(o->nsub,sizeof(THD_simmat **));for(s=0;s<o->nsub;s++){ndyn[s]=(THD_simmat **)calloc(ng->n,sizeof(THD_simmat *));for(c=0;c<ng->n;c++)ndyn[s][c]=build_dynamics(nrdm[s][c],nwin,o->ncond,dyn_cmp);}}
   }
   if(model&&ng){THD_simmat ***flat=(THD_simmat ***)calloc(o->nsub,sizeof(THD_simmat **));for(s=0;s<o->nsub;s++){flat[s]=(THD_simmat **)calloc(ng->n*nwin,sizeof(THD_simmat *));for(c=0;c<ng->n;c++)for(t=0;t<nwin;t++)flat[s][c*nwin+t]=nrdm[s][c][t];}ninf=run_inference(flat,o,ng->n*nwin,model,cmp,null_type,nperm,seed,modelfile,modelaxis,"time-x-neighborhood",quiet);for(s=0;s<o->nsub;s++)free(flat[s]);free(flat);}
   else if(model)inf=run_inference(rdm,o,nwin,model,cmp,null_type,nperm,seed,modelfile,modelaxis,"time",quiet);
   preflight_outputs(prefix,o,nwin,series,matrices,(inf!=NULL||ninf!=NULL),dyn_cmp!=0,cross_time,ng);
   {char fn[THD_MAX_NAME];FILE *fp;
    snprintf(fn,sizeof(fn),"%s.trdm.1D",prefix);fp=open_output(fn);
    fprintf(fp,"# 1dTrdm output version %s\n# observation table: %s\n# time axis: %s\n# feature axis: %s\n",TRDM_VERSION,obsfile,timefile,featfile);
    fprintf(fp,"# estimator: %s; windows width=%d step=%d reduction=%s; output features=%d\n",metric_s,width,step,(reduce==REDUCE_MEAN)?"mean":"concat",nfout);
    fprintf(fp,"# condition centering: %s\n# subjects=%d conditions=%d input_time=%d input_features=%d output_windows=%d\n",center==CENTER_NONE?"none":center==CENTER_SUBJ?"subject":"partition",o->nsub,o->ncond,ta->n,fa->n,nwin);
    fprintf(fp,"Subj TimeIndex TimeValue TimeUnit TimeLabel ConditionA ConditionB Dissimilarity\n");
    for(s=0;s<o->nsub;s++)for(t=0;t<nwin;t++)for(a=0;a<o->ncond;a++)for(b=a+1;b<o->ncond;b++)fprintf(fp,"%s %d %.12g %s %s %s %s %.9g\n",o->subj[s],t,wvalue[t],ta->unit,wlabel[t],o->cond[a],o->cond[b],rdm[s][t]->mat[(size_t)a*o->ncond+b]);
    fclose(fp);
    snprintf(fn,sizeof(fn),"%s.trdm.time.1D",prefix);fp=open_output(fn);fprintf(fp,"TimeIndex TimeValue TimeUnit TimeLabel StartIndex EndIndex StartLabel EndLabel\n");for(t=0;t<nwin;t++)fprintf(fp,"%d %.12g %s %s %d %d %s %s\n",t,wvalue[t],ta->unit,wlabel[t],wstart[t],wend[t],ta->label[wstart[t]],ta->label[wend[t]]);fclose(fp);
    snprintf(fn,sizeof(fn),"%s.trdm.conditions.1D",prefix);fp=open_output(fn);fprintf(fp,"ConditionIndex Condition\n");for(c=0;c<o->ncond;c++)fprintf(fp,"%d %s\n",c,o->cond[c]);fclose(fp);
    snprintf(fn,sizeof(fn),"%s.trdm.features.1D",prefix);fp=open_output(fn);fprintf(fp,"FeatureIndex FeatureLabel\n");for(c=0;c<fa->n;c++)fprintf(fp,"%d %s\n",c,fa->label[c]);fclose(fp);
    snprintf(fn,sizeof(fn),"%s.trdm.counts.1D",prefix);fp=open_output(fn);fprintf(fp,"Subj Condition Partition Observations\n");
    for(s=0;s<o->nsub;s++)for(c=0;c<o->ncond;c++){
      if(metric==MET_CROSSNOBIS){int p;for(p=0;p<o->npart[s];p++){int n=0;for(i=0;i<o->nrow;i++)if(o->isub[i]==s&&o->icond[i]==c&&o->ipart[i]==p)n++;fprintf(fp,"%s %s %s %d\n",o->subj[s],o->cond[c],o->part_lab[s][p],n);}}
      else{int n=0;for(i=0;i<o->nrow;i++)if(o->isub[i]==s&&o->icond[i]==c)n++;fprintf(fp,"%s %s all %d\n",o->subj[s],o->cond[c],n);}
    }
    fclose(fp);
    snprintf(fn,sizeof(fn),"%s.trdm.meta",prefix);fp=open_output(fn);fprintf(fp,"format 1dTrdm\nversion %s\nobservation_table %s\ntime_axis %s\nfeature_axis %s\nmetric %s\nwindow_width %d\nwindow_step %d\nwindow_reduce %s\ncondition_centering %s\nsubjects %d\nconditions %d\ninput_time %d\ninput_features %d\noutput_windows %d\nsubject_matrices %s\nmodel_series %s\ninference %s\n",TRDM_VERSION,obsfile,timefile,featfile,metric_s,width,step,reduce==REDUCE_MEAN?"mean":"concat",center==CENTER_NONE?"none":center==CENTER_SUBJ?"subject":"partition",o->nsub,o->ncond,ta->n,fa->n,nwin,matrices?"written":"not written",series?"independent-sample group mean":"not written",inf?"temporal model comparison":ninf?"time-x-neighborhood model comparison":"not requested");
    fprintf(fp,"feature_neighborhoods %s\nneighborhood_count %d\nneighborhood_overlap allowed\nneighborhood_column_adjacency ignored\n",ng?neighfile:"not requested",ng?ng->n:0);
    if(ng)fprintf(fp,"neighborhood_time_cells %d\nneighborhood_cross_temporal_unique_cells %d\n",ng->n*nwin,ng->n*nwin*(nwin+1)/2);
    fprintf(fp,"rdm_dynamics %s\ncross_time_crossnobis %s\ncross_temporal_symmetry unique-triangle-A<=B\ncross_temporal_unique_cells %d\ncross_temporal_inference descriptive-only-not-performed\n",dyn_cmp==CMP_PEARSON?"pearson":dyn_cmp==CMP_SPEARMAN?"spearman":"not requested",cross_time?"balanced-ordered-independent-partition-pairs":"not requested",nwin*(nwin+1)/2);
    if(inf)fprintf(fp,"model_matrix %s\nmodel_conditions %s\ncomparison %s\ntemporal_null %s\ntested_population %s\ntail two-sided\nnperm_requested %d\nnperm_used %d\nrelabelings %s\nseed %ld\nfamily time\nfamily_size %d\nmultiple_testing BH-and-maxFWE-over-complete-time-family\n",modelfile,modelaxis,cmp==CMP_SPEARMAN?"spearman":"pearson",null_type==TNULL_SUBJECTS?"subjects":"conditions",null_type==TNULL_SUBJECTS?"population-subjects":"fixed-observed-subject-condition-sample",nperm,inf->nperm,inf->is_exact?"exact":"sampled",seed,nwin);
    if(ninf)fprintf(fp,"model_matrix %s\nmodel_conditions %s\ncomparison %s\ntemporal_null %s\ntested_population %s\ntail two-sided\nnperm_requested %d\nnperm_used %d\nrelabelings %s\nseed %ld\nfamily time-x-neighborhood\nfamily_size %d\nmultiple_testing BH-and-maxFWE-over-complete-time-x-neighborhood-family\n",modelfile,modelaxis,cmp==CMP_SPEARMAN?"spearman":"pearson",null_type==TNULL_SUBJECTS?"subjects":"conditions",null_type==TNULL_SUBJECTS?"population-subjects":"fixed-observed-subject-condition-sample",nperm,ninf->nperm,ninf->is_exact?"exact":"sampled",seed,ng->n*nwin);
    fclose(fp);
    if(inf){
      snprintf(fn,sizeof(fn),"%s.trdm.fits.1D",prefix);fp=open_output(fn);
      fprintf(fp,"# model_matrix: %s\n# model_conditions: %s\n# comparison: %s\nSubj TimeIndex TimeValue TimeUnit TimeLabel Fit FisherZ\n",modelfile,modelaxis,cmp==CMP_SPEARMAN?"spearman":"pearson");
      for(s=0;s<o->nsub;s++)for(t=0;t<nwin;t++)fprintf(fp,"%s %d %.12g %s %s %.9g %.9g\n",o->subj[s],t,wvalue[t],ta->unit,wlabel[t],inf->rfit[(size_t)s*nwin+t],inf->zfit[(size_t)s*nwin+t]);fclose(fp);
      snprintf(fn,sizeof(fn),"%s.trdm.inference.1D",prefix);fp=open_output(fn);
      fprintf(fp,"# model_matrix: %s\n# model_conditions: %s\n# comparison: %s\n# temporal_null: %s\n# tested_population: %s\n# two-sided; requested_nperm=%d used_nperm=%d relabelings=%s seed=%ld\n# multiplicity: BH FDR and max-statistic FWE over one family of %d output windows\n",modelfile,modelaxis,cmp==CMP_SPEARMAN?"spearman":"pearson",null_type==TNULL_SUBJECTS?"subjects":"conditions",null_type==TNULL_SUBJECTS?"population subjects":"fixed observed subject/condition sample",nperm,inf->nperm,inf->is_exact?"exact":"sampled",seed,nwin);
      fprintf(fp,"TimeIndex TimeValue TimeUnit TimeLabel Effect Stat P Q PFWE\n");
      for(t=0;t<nwin;t++)fprintf(fp,"%d %.12g %s %s %.9g %.9g %.9g %.9g %.9g\n",t,wvalue[t],ta->unit,wlabel[t],inf->effect[t],inf->stat[t],inf->p[t],inf->q[t],inf->pfwe[t]);fclose(fp);
    }
   }
   if(ng){char fn[THD_MAX_NAME];FILE *fp;
     snprintf(fn,sizeof(fn),"%s.trdm.neighborhood_axis.1D",prefix);fp=open_output(fn);
     fprintf(fp,"NeighborhoodIndex Neighborhood FeatureIndex Feature\n");
     for(c=0;c<ng->n;c++)for(i=0;i<ng->nfeat[c];i++)fprintf(fp,"%d %s %d %s\n",c,ng->label[c],ng->feat[c][i],fa->label[ng->feat[c][i]]);fclose(fp);
     snprintf(fn,sizeof(fn),"%s.trdm.neighborhoods.1D",prefix);fp=open_output(fn);
     fprintf(fp,"# explicit feature neighborhoods: %s; overlaps allowed; feature adjacency ignored\n",neighfile);
     fprintf(fp,"Subj NeighborhoodIndex Neighborhood TimeIndex TimeValue TimeUnit TimeLabel ConditionA ConditionB Dissimilarity\n");
     for(s=0;s<o->nsub;s++)for(c=0;c<ng->n;c++)for(t=0;t<nwin;t++)for(a=0;a<o->ncond;a++)for(b=a+1;b<o->ncond;b++)
       fprintf(fp,"%s %d %s %d %.12g %s %s %s %s %.9g\n",o->subj[s],c,ng->label[c],t,wvalue[t],ta->unit,wlabel[t],o->cond[a],o->cond[b],nrdm[s][c][t]->mat[(size_t)a*o->ncond+b]);
     fclose(fp);
     if(ninf){
       snprintf(fn,sizeof(fn),"%s.trdm.neighborhood_fits.1D",prefix);fp=open_output(fn);
       fprintf(fp,"# model_matrix: %s\n# model_conditions: %s\n# comparison: %s\nSubj NeighborhoodIndex Neighborhood TimeIndex TimeValue TimeUnit TimeLabel Fit FisherZ\n",modelfile,modelaxis,cmp==CMP_SPEARMAN?"spearman":"pearson");
       for(s=0;s<o->nsub;s++)for(c=0;c<ng->n;c++)for(t=0;t<nwin;t++){int z=c*nwin+t;fprintf(fp,"%s %d %s %d %.12g %s %s %.9g %.9g\n",o->subj[s],c,ng->label[c],t,wvalue[t],ta->unit,wlabel[t],ninf->rfit[(size_t)s*ng->n*nwin+z],ninf->zfit[(size_t)s*ng->n*nwin+z]);}fclose(fp);
       snprintf(fn,sizeof(fn),"%s.trdm.neighborhood_inference.1D",prefix);fp=open_output(fn);
       fprintf(fp,"# model_matrix: %s\n# model_conditions: %s\n# comparison: %s\n# temporal_null: %s\n# tested_population: %s\n# two-sided; requested_nperm=%d used_nperm=%d relabelings=%s seed=%ld\n# multiplicity: BH FDR and synchronized max-statistic FWE over one family of %d time x neighborhood cells\n",modelfile,modelaxis,cmp==CMP_SPEARMAN?"spearman":"pearson",null_type==TNULL_SUBJECTS?"subjects":"conditions",null_type==TNULL_SUBJECTS?"population subjects":"fixed observed subject/condition sample",nperm,ninf->nperm,ninf->is_exact?"exact":"sampled",seed,ng->n*nwin);
       fprintf(fp,"NeighborhoodIndex Neighborhood TimeIndex TimeValue TimeUnit TimeLabel Effect Stat P Q PFWE\n");
       for(c=0;c<ng->n;c++)for(t=0;t<nwin;t++){int z=c*nwin+t;fprintf(fp,"%d %s %d %.12g %s %s %.9g %.9g %.9g %.9g %.9g\n",c,ng->label[c],t,wvalue[t],ta->unit,wlabel[t],ninf->effect[z],ninf->stat[z],ninf->p[z],ninf->q[z],ninf->pfwe[z]);}fclose(fp);
     }
   }
   if(dyn_cmp){char fn[THD_MAX_NAME];FILE *fp;snprintf(fn,sizeof(fn),"%s.trdm.dynamics.1D",prefix);fp=open_output(fn);
     fprintf(fp,"# RDM-triangle recurrence; comparison=%s; canonical unique time cells A<=B\n",dyn_cmp==CMP_PEARSON?"pearson":"spearman");
     fprintf(fp,"# descriptive only; no cross-temporal inference performed\nSubj TimeAIndex TimeAValue TimeAUnit TimeALabel TimeBIndex TimeBValue TimeBUnit TimeBLabel Similarity\n");
     for(s=0;s<o->nsub;s++)for(a=0;a<nwin;a++)for(b=a;b<nwin;b++)
       fprintf(fp,"%s %d %.12g %s %s %d %.12g %s %s %.9g\n",o->subj[s],a,wvalue[a],ta->unit,wlabel[a],b,wvalue[b],ta->unit,wlabel[b],dyn[s]->mat[(size_t)a*nwin+b]);
     fclose(fp);
   }
   if(cross_time){char fn[THD_MAX_NAME];FILE *fp;snprintf(fn,sizeof(fn),"%s.trdm.cross_time_crossnobis.1D",prefix);fp=open_output(fn);
     fprintf(fp,"# cross-time crossnobis; ordered independent partition pairs; canonical unique time cells A<=B\n");
     fprintf(fp,"# diagonal is exactly the primary crossnobis RDM; descriptive only; no cross-temporal inference performed\n");
     fprintf(fp,"Subj TimeAIndex TimeAValue TimeAUnit TimeALabel TimeBIndex TimeBValue TimeBUnit TimeBLabel ConditionA ConditionB Crossnobis\n");
     for(s=0;s<o->nsub;s++){
       float ***pat=(float ***)calloc(nwin,sizeof(float **));int nfpat=0,np=o->npart[s];
       for(t=0;t<nwin;t++){int z;pat[t]=make_partition_patterns(o,s,wstart[t],width,reduce,center,fa->n,NULL,fa->n,&z);if(t==0)nfpat=z;}
       for(a=0;a<nwin;a++)for(b=a;b<nwin;b++)for(c=0;c<o->ncond;c++){int d;for(d=c+1;d<o->ncond;d++){
         float v=(a==b)?rdm[s][a]->mat[(size_t)c*o->ncond+d]:cross_time_crossnobis_value(pat[a],pat[b],np,o->ncond,nfpat,c,d);
         fprintf(fp,"%s %d %.12g %s %s %d %.12g %s %s %s %s %.9g\n",o->subj[s],a,wvalue[a],ta->unit,wlabel[a],b,wvalue[b],ta->unit,wlabel[b],o->cond[c],o->cond[d],v);
       }}
       for(t=0;t<nwin;t++)free_partition_patterns(pat[t],np);free(pat);
     }
     fclose(fp);
   }
   if(ng&&dyn_cmp){char fn[THD_MAX_NAME];FILE *fp;snprintf(fn,sizeof(fn),"%s.trdm.neighborhood_dynamics.1D",prefix);fp=open_output(fn);
     fprintf(fp,"# within-neighborhood RDM-triangle recurrence; comparison=%s; canonical A<=B\n",dyn_cmp==CMP_PEARSON?"pearson":"spearman");
     fprintf(fp,"Subj NeighborhoodIndex Neighborhood TimeAIndex TimeAValue TimeAUnit TimeALabel TimeBIndex TimeBValue TimeBUnit TimeBLabel Similarity\n");
     for(s=0;s<o->nsub;s++)for(c=0;c<ng->n;c++)for(a=0;a<nwin;a++)for(b=a;b<nwin;b++)
       fprintf(fp,"%s %d %s %d %.12g %s %s %d %.12g %s %s %.9g\n",o->subj[s],c,ng->label[c],a,wvalue[a],ta->unit,wlabel[a],b,wvalue[b],ta->unit,wlabel[b],ndyn[s][c]->mat[(size_t)a*nwin+b]);
     fclose(fp);
   }
   if(ng&&cross_time){char fn[THD_MAX_NAME];FILE *fp;snprintf(fn,sizeof(fn),"%s.trdm.neighborhood_cross_time_crossnobis.1D",prefix);fp=open_output(fn);
     fprintf(fp,"# within-neighborhood cross-time crossnobis; ordered independent partition pairs; canonical A<=B\n");
     fprintf(fp,"Subj NeighborhoodIndex Neighborhood TimeAIndex TimeAValue TimeAUnit TimeALabel TimeBIndex TimeBValue TimeBUnit TimeBLabel ConditionA ConditionB Crossnobis\n");
     for(s=0;s<o->nsub;s++)for(c=0;c<ng->n;c++){
       float ***pat=(float ***)calloc(nwin,sizeof(float **));int nfpat=0,np=o->npart[s];
       for(t=0;t<nwin;t++){int z;pat[t]=make_partition_patterns(o,s,wstart[t],width,reduce,center,fa->n,ng->feat[c],ng->nfeat[c],&z);if(t==0)nfpat=z;}
       for(a=0;a<nwin;a++)for(b=a;b<nwin;b++){int ca;for(ca=0;ca<o->ncond;ca++){int cb;for(cb=ca+1;cb<o->ncond;cb++){
         float v=(a==b)?nrdm[s][c][a]->mat[(size_t)ca*o->ncond+cb]:cross_time_crossnobis_value(pat[a],pat[b],np,o->ncond,nfpat,ca,cb);
         fprintf(fp,"%s %d %s %d %.12g %s %s %d %.12g %s %s %s %s %.9g\n",o->subj[s],c,ng->label[c],a,wvalue[a],ta->unit,wlabel[a],b,wvalue[b],ta->unit,wlabel[b],o->cond[ca],o->cond[cb],v);
       }}}
       for(t=0;t<nwin;t++)free_partition_patterns(pat[t],np);free(pat);
     }
     fclose(fp);
   }
   if(matrices)for(s=0;s<o->nsub;s++){char *ss=safe_label(o->subj[s]);for(t=0;t<nwin;t++){char fn[THD_MAX_NAME];snprintf(fn,sizeof(fn),"%s_s%04d_%s_t%04d.1D",prefix,s,ss,t);write_matrix_checked(fn,rdm[s][t]);}free(ss);}
   if(matrices&&dyn_cmp)for(s=0;s<o->nsub;s++){char fn[THD_MAX_NAME],*ss=safe_label(o->subj[s]);snprintf(fn,sizeof(fn),"%s_s%04d_%s_dynamics.1D",prefix,s,ss);write_matrix_checked(fn,dyn[s]);free(ss);}
   if(matrices&&ng)for(s=0;s<o->nsub;s++){char *ss=safe_label(o->subj[s]);for(c=0;c<ng->n;c++){char *gg=safe_label(ng->label[c]);for(t=0;t<nwin;t++){char fn[THD_MAX_NAME];snprintf(fn,sizeof(fn),"%s_s%04d_%s_n%04d_%s_t%04d.1D",prefix,s,ss,c,gg,t);write_matrix_checked(fn,nrdm[s][c][t]);}if(dyn_cmp){char fn[THD_MAX_NAME];snprintf(fn,sizeof(fn),"%s_s%04d_%s_n%04d_%s_dynamics.1D",prefix,s,ss,c,gg);write_matrix_checked(fn,ndyn[s][c]);}free(gg);}free(ss);}
   if(series){char list[THD_MAX_NAME];FILE *lf;snprintf(list,sizeof(list),"%s.model_series.1D",prefix);lf=open_output(list);fprintf(lf,"# independent-sample group mean from 1dTrdm; do not use for same-subject fusion\nTime ModelFile\n");for(t=0;t<nwin;t++){THD_simmat *gm=THD_simmat_new(o->ncond);char fn[THD_MAX_NAME],*base;for(a=0;a<o->ncond*o->ncond;a++){double z=0.0;for(s=0;s<o->nsub;s++)z+=rdm[s][t]->mat[a];gm->mat[a]=(float)(z/o->nsub);}gm->is_dist=1;snprintf(fn,sizeof(fn),"%s_group_t%04d.1D",prefix,t);write_matrix_checked(fn,gm);base=strrchr(fn,'/');fprintf(lf,"%s %s\n",wlabel[t],base?base+1:fn);THD_simmat_free(gm);}fclose(lf);}
   if(!quiet)INFO_message("1dTrdm: wrote %d subject x %d window RDMs, %d conditions, to prefix %s%s%s%s%s%s",o->nsub,nwin,o->ncond,prefix,series?" plus independent-sample model_series":"",(inf||ninf)?" plus temporal inference":"",dyn_cmp?" plus RDM dynamics":"",cross_time?" plus cross-time crossnobis":"",ng?" plus feature neighborhoods":"");
   for(s=0;s<o->nsub;s++){for(t=0;t<nwin;t++)THD_simmat_free(rdm[s][t]);free(rdm[s]);}free(rdm);
   if(dyn){for(s=0;s<o->nsub;s++)THD_simmat_free(dyn[s]);free(dyn);}
   if(nrdm){for(s=0;s<o->nsub;s++){for(c=0;c<ng->n;c++){for(t=0;t<nwin;t++)THD_simmat_free(nrdm[s][c][t]);free(nrdm[s][c]);}free(nrdm[s]);}free(nrdm);}
   if(ndyn){for(s=0;s<o->nsub;s++){for(c=0;c<ng->n;c++)THD_simmat_free(ndyn[s][c]);free(ndyn[s]);}free(ndyn);}
   for(t=0;t<nwin;t++)free(wlabel[t]);free(wlabel);free(wstart);free(wend);free(wvalue);
   THD_simmat_free(model);free_infer(inf);free_infer(ninf);free_obs(o);free_time(ta);free_feat(fa);free_neigh(ng);return 0;
}
