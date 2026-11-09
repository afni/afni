/* M6 model bundles: absolute shell working directories, local dataset selectors,
   one crossed-subject model per selection and ordered condition pairing. */
#include "mrilib.h"
#include "thd_insync_model.h"
#include "thd_niftiwrite.h"
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <unistd.h>
#include <fcntl.h>
#define MODEL_FAIL(...) ERROR_exit("3dInSync model export: " __VA_ARGS__)

static void *alloc(size_t n,size_t size)
{
   void *p;
   if(size && n>SIZE_MAX/size) MODEL_FAIL("allocation overflow");
   p=calloc(n?n:1,size); if(!p) MODEL_FAIL("cannot allocate model metadata"); return p;
}
static void shell(FILE *fp,const char *s)
{
   fputc('\'',fp);
   for(;*s;s++) if(*s=='\'') fputs("'\\''",fp); else fputc(*s,fp);
   fputc('\'',fp);
}
/* The R reader uses scan(character), then AFNI token parsing. Restrict identity
   and covariate tokens instead of assuming shell quoting solves table syntax. */
static void token_check(const char *s,const char *what,int name)
{
   const unsigned char *p=(const unsigned char *)s;
   if(!*p || !strcmp(s,"NA") || !strcmp(s,"NaN") || !strcmp(s,"Inf")) MODEL_FAIL("unrepresentable %s token '%s'",what,s);
   if(name && !(isalpha(*p) || *p=='_')) MODEL_FAIL("invalid %s name '%s'",what,s);
   for(;*p;p++) if(!(isalnum(*p) || *p=='_' || *p=='.' || (!name && (*p=='-' || *p=='+')))) MODEL_FAIL("unrepresentable %s token '%s'",what,s);
}
static int ignored(const INSYNC_options *o,THD_datatable *t,int c)
{
   const char *names[]={"Xmat","XmatFull","Selection","Segment","Run","StartTR","StopTR","Censor",o->group_column,o->condition_column,o->censor_column};
   int i;
   if(c==t->icol_subj || c==t->icol_input) return 1;
   for(i=0;i<11;i++) if(names[i] && !strcasecmp(names[i],t->cname[c])) return 1;
   return 0;
}
static void path(char *dst,const INSYNC_model *m,const char *base)
{
   if(snprintf(dst,THD_MAX_NAME,"%s/%s",m->directory,base)>=THD_MAX_NAME) MODEL_FAIL("bundle path too long");
}
static void jobbase(char *dst,int id,int c,int d)
{ snprintf(dst,96,"sel%06d.c%03d_c%03d",id,c+1,d+1); }
static void check_path(const char *name)
{
   struct stat st;
   if(!lstat(name,&st)){
      if(!S_ISREG(st.st_mode)) MODEL_FAIL("output is not a regular file: %s",name);
      if(!THD_ok_overwrite()) MODEL_FAIL("output already exists: %s",name);
      if(access(name,W_OK)) MODEL_FAIL("output is not writable: %s",name);
   }
}
static int same_path(const char *a,const char *b)
{
   char pa[PATH_MAX],pb[PATH_MAX],scratch[PATH_MAX],*slash;
   const char *input[2]={a,b}; char *out[2]={pa,pb}; int i;
   for(i=0;i<2;i++){
      if(!input[i]) return 0;
      if(realpath(input[i],out[i])) continue;
      if(strlen(input[i])>=sizeof(scratch)) MODEL_FAIL("output path too long");
      strcpy(scratch,input[i]); slash=strrchr(scratch,'/');
      if(slash){
         char base[PATH_MAX]; strcpy(base,slash+1);
         if(slash==scratch) slash[1]=0; else *slash=0;
         if(!realpath(scratch,out[i])) return 0;
         if(strlen(out[i])+strlen(base)+2>PATH_MAX) MODEL_FAIL("output path too long");
         strcat(out[i],"/"); strcat(out[i],base);
      } else {
         if(!getcwd(out[i],PATH_MAX)) MODEL_FAIL("cannot resolve current directory");
         if(strlen(out[i])+strlen(scratch)+2>PATH_MAX) MODEL_FAIL("output path too long");
         strcat(out[i],"/"); strcat(out[i],scratch);
      }
   }
   return !strcmp(pa,pb);
}
static void check_destination(INSYNC_model *m,const char *name)
{
   int i;
   check_path(name);
   if(same_path(name,m->options->matrix_name)) MODEL_FAIL("ROI matrix output collides with model bundle: %s",name);
   for(i=0;i<m->schedule->nsource;i++) if(same_path(name,m->schedule->source[i].file)) MODEL_FAIL("model output collides with input: %s",name);
}
static FILE *openfile(INSYNC_model *m,const char *base)
{
   char name[THD_MAX_NAME]; FILE *fp; path(name,m,base);
   fp=fopen(name,"w"); if(!fp) MODEL_FAIL("cannot write %s",name); return fp;
}
static void closefile(FILE *fp)
{ int bad=ferror(fp); if(fclose(fp)) bad=1; if(bad) MODEL_FAIL("failed writing bundle text"); }
static int finite_number(const char *s)
{ char *end; double v; errno=0; v=strtod(s,&end); return *s && !*end && isfinite(v) && !errno; }
static void numeric_list(const char *s,int comma,int expected,const char *what)
{
   char *copy=strdup(s),*p=copy,*end; int n=0; double value;
   if(!copy) MODEL_FAIL("cannot allocate %s",what);
   while(*p){
      while(isspace((unsigned char)*p)) p++;
      errno=0; value=strtod(p,&end);
      if(end==p || errno || !isfinite(value)) MODEL_FAIL("%s needs finite numeric values",what);
      n++; p=end; while(isspace((unsigned char)*p)) p++;
      if(comma && *p){ if(*p!=',' || !p[1]) MODEL_FAIL("%s needs a comma-separated list",what); p++; }
      else if(!comma && *p && !isspace((unsigned char)end[0])) MODEL_FAIL("%s needs whitespace-separated weights",what);
   }
   free(copy);
   if(!n || (expected>=0 && n!=expected)) MODEL_FAIL("%s count does not match qVars",what);
}
INSYNC_model *INSYNC_model_preflight(const INSYNC_options *o,THD_datatable *t,const INSYNC_schedule *s,int ns,int nc,const int *sources,char **conditions)
{
   INSYNC_model *m; int i,j,k,id,c,d,ncols=8,nq=0; char **cols;
   char parent[PATH_MAX],resolved[PATH_MAX],base[THD_MAX_NAME],name[THD_MAX_NAME],*slash;
   const char *reserved[]={"Subj1","Subj2","Group1","Group2","GroupPair","cond","cond1","cond2","InputFile"};
   struct stat st;
   if(!o->pair_prefix) return NULL;
   m=alloc(1,sizeof(*m));
   if(!getcwd(m->source_directory,sizeof(m->source_directory))) MODEL_FAIL("cannot record source working directory");
   m->options=o; m->table=t; m->schedule=s; m->nsub=ns; m->ncond=nc; m->sources=sources; m->conditions=conditions;
   if((double)ns*(2.*ns-1)>INT_MAX || ns>INT_MAX/2) MODEL_FAIL("too many subjects/pair bricks");
   if(snprintf(parent,sizeof(parent),"%s.isc",o->pair_prefix)>=(int)sizeof(parent)) MODEL_FAIL("bundle path too long");
   /* Resolve only the existing parent; creating output is deferred. */
   slash=strrchr(parent,'/');
   if(slash){ MCW_strncpy(base,slash+1,sizeof(base)); if(slash==parent) slash[1]=0; else *slash=0; }
   else { MCW_strncpy(base,parent,sizeof(base)); strcpy(parent,"."); }
   if(!*base || !realpath(parent,resolved) || access(resolved,W_OK|X_OK)) MODEL_FAIL("bundle parent must exist and be writable");
   if(snprintf(m->directory,sizeof(m->directory),"%s/%s",resolved,base)>=(int)sizeof(m->directory)) MODEL_FAIL("bundle path too long");
   if(strchr(m->directory,'\n') || strchr(m->directory,'\r') || strchr(m->directory,'\\')) MODEL_FAIL("bundle path cannot contain newline/CR/backslash");
   if(!lstat(m->directory,&st)){
      if(!S_ISDIR(st.st_mode) || S_ISLNK(st.st_mode) || access(m->directory,W_OK|X_OK)) MODEL_FAIL("bundle destination must be a writable non-symlink directory");
   }
   m->covariates=alloc(t->ncol,sizeof(int)); cols=alloc(9+2*(size_t)t->ncol,sizeof(char *));
   for(i=0;i<9;i++) cols[i]=strdup(reserved[i]);
   ncols=9;
   for(i=0;i<t->ncol;i++) if(!ignored(o,t,i)){
      token_check(t->cname[i],"covariate",1);
      /* A pre-suffixed source name is ambiguous with an exported role column. */
      for(j=0;j<ncols;j++) if(!strcmp(t->cname[i],cols[j])) MODEL_FAIL("source covariate '%s' collides with an exported column",t->cname[i]);
      m->covariates[m->ncov++]=i;
      for(j=1;j<=2;j++){
         if(snprintf(name,sizeof(name),"%s%d",t->cname[i],j)>=(int)sizeof(name)) MODEL_FAIL("covariate name too long");
         for(k=0;k<ncols;k++) if(!strcmp(name,cols[k])) MODEL_FAIL("generated covariate column collision: %s",name);
         for(k=0;k<t->ncol;k++) if(k!=i && !strcmp(name,t->cname[k])) MODEL_FAIL("source/generated covariate column collision: %s",name);
         cols[ncols++]=strdup(name);
      }
      for(j=0;j<s->nsource;j++) token_check(DT_CELL(t,s->source[j].row,i),"covariate value",0);
   }
   for(i=0;i<s->nsource;i++){
      token_check(s->source[i].subject,"subject",0); token_check(s->source[i].group,"group",0); token_check(s->source[i].condition,"condition",0);
      /* Separators used below must not create ambiguous pair categories. */
      if(strstr(s->source[i].group,"__") || strstr(s->source[i].condition,"__")) MODEL_FAIL("group/condition tokens cannot contain '__' in model exports");
   }
   if(o->isc_qvars){
      char *copy=strdup(o->isc_qvars),*p=copy,*next; int *seen=alloc(ncols,sizeof(int));
      if(!copy) MODEL_FAIL("cannot allocate qVars");
      do {
         next=strchr(p,','); if(next) *next=0;
         token_check(p,"qVars",1);
         for(i=9;i<ncols;i++) if(!strcmp(cols[i],p)) break;
         if(i==ncols) MODEL_FAIL("qVars '%s' is not an exported numeric covariate column",p);
         if(seen[i]++) MODEL_FAIL("duplicate qVars '%s'",p);
         for(j=0;j<s->nsource;j++) if(!finite_number(DT_CELL(t,s->source[j].row,m->covariates[(i-9)/2]))) MODEL_FAIL("qVars '%s' has nonnumeric values",p);
         nq++; p=next?next+1:NULL;
      } while(p);
      free(copy); free(seen);
   }
   if(o->isc_centers){ if(!nq) MODEL_FAIL("qVarCenters requires qVars"); numeric_list(o->isc_centers,1,nq,"qVarCenters"); }
   for(i=0;i<o->isc_nglt;i++){
      token_check(o->isc_glt_names[i],"GLT label",0);
      for(j=0;j<i;j++) if(!strcmp(o->isc_glt_names[i],o->isc_glt_names[j])) MODEL_FAIL("duplicate GLT label");
      numeric_list(o->isc_glt_weights[i],0,-1,"GLT weights");
   }
   for(i=0;i<ncols;i++) free(cols[i]);
   free(cols);
   /* Every ordered cross-condition job uses exactly matched original block
      coordinates. Differences cannot be hidden by equal packed lengths. */
   for(id=1;id<=s->nselection;id++) for(c=1;c<nc;c++){
      int a=0,b=0;
      for(;;){
         while(a<s->nsegment && (s->segment[a].selection_id!=id || s->segment[a].source!=sources[0])) a++;
         while(b<s->nsegment && (s->segment[b].selection_id!=id || s->segment[b].source!=sources[c*ns])) b++;
         if(a==s->nsegment || b==s->nsegment){ if(a!=b) MODEL_FAIL("unmatched condition selections for sel%06d",id); break; }
         if(s->segment[a].original_start!=s->segment[b].original_start || s->segment[a].original_stop!=s->segment[b].original_stop || s->segment[a].run!=s->segment[b].run) MODEL_FAIL("unmatched original coordinates across conditions for sel%06d",id);
         a++; b++;
      }
   }
   { const char *files[]={"jobs.tsv","README.txt","run_all.sh","schedule.selections.tsv","schedule.selection_summary.txt"};
      for(i=0;i<5;i++){ path(name,m,files[i]); check_destination(m,name); }
   }
   for(id=1;id<=s->nselection;id++) for(c=0;c<nc;c++) for(d=0;d<nc;d++){
      const char *suffix[]={"pairs.nii.gz","mask.nii.gz","support.nii.gz","table.txt","sh","fit.nii.gz"}; char job[96]; jobbase(job,id,c,d);
      for(i=0;i<6;i++){
         snprintf(base,sizeof(base),"%s.%s",job,suffix[i]); path(name,m,base);
         if(i==5 && THD_is_file(name)) MODEL_FAIL("fit output already exists; choose a fresh model prefix: %s",name);
         check_destination(m,name);
      }
   }
   return m;
}
void INSYNC_model_begin(INSYNC_model *m)
{
   FILE *fp; INSYNC_options report=*m->options; char prefix[THD_MAX_NAME];
   if(mkdir(m->directory,0777) && errno!=EEXIST) MODEL_FAIL("cannot create %s",m->directory);
   m->index=openfile(m,"jobs.tsv");
   fprintf(m->index,"# Status: running; only final Status: complete certifies this bundle\n"); fflush(m->index);
   fprintf(m->index,"SelectionID\tCondition1\tCondition2\tPairs\tMask\tSupport\tTable\tScript\tFitPrefix\tValidVoxels\n");
   m->master=openfile(m,"run_all.sh"); fprintf(m->master,"#!/bin/sh\nset -eu\ncd -- "); shell(m->master,m->directory); fputc('\n',m->master);
   /* Prevent accidental fitting from a partial bundle even when an earlier
      successful export existed at this path. */
   fputs("grep -q '^# Status: complete$' jobs.tsv || { echo 'Incomplete 3dInSync model bundle' >&2; exit 1; }\n",m->master);
   fp=openfile(m,"README.txt");
   fprintf(fp,"Source working directory: %s\n",m->source_directory);
   fprintf(fp,"3dInSync selection 3dISC bundles\n\nRun with: sh run_all.sh (scripts are never automatically executed).\nOne fit per selection and ORDERED condition pairing, one row per unique subject pair.\nSubjects use stable lexical order; cross-condition r is corr(Subj1/cond1, Subj2/cond2), not an average of both orientations.\nDefault model: 1+(1|Subj1)+(1|Subj2). It estimates an intercept, not a group difference.\nPair maps are Fisher-z of final pair-r (clamp exact endpoints to +/-0.999999).\nBinary masks require every modeled pair finite. Zero placeholders outside the mask must never be included.\nExport is pairwise regardless of the descriptive estimator. Block admission requires every modeled pair finite; the equal-weight aggregate uses the same admitted blocks for all modeled edges.\nSupport bricks: standalone/pool = ValidTR; aggregate = RetainedTR, Eligible, Weight for each ordered schedule segment, repeating per block. Weights are zero when the aggregate fails.\nEach job table references only local numeric filenames; scripts cd to the absolute bundle directory. Moving the bundle requires updating these cd paths.\nIndependent fits do not test changes across selections or correct across windows/voxels. Joint repeated-window models are not generated.\nUser models/GLTs are passed as arguments; R validates formulas, rank and GLT dimensions. qVars are numeric exported covariate1/covariate2 columns only.\n3dISC duplicates dyads and swaps Subj1/Subj2 internally, but does not swap custom covariate1/covariate2 fields. They are lexical pair-slot values; symmetric transforms are NOT inferred. Use explicit scientific model expressions and verify their meaning.\nOriginal source/control metadata are in schedule.selections.tsv; Xmat, Censor and selection controls are excluded from model covariates.\n"); closefile(fp);
   path(prefix,m,"schedule"); report.prefix=prefix; report.quiet=1; INSYNC_schedule_report(&report,m->table,(INSYNC_schedule *)m->schedule);
}
void INSYNC_model_job(INSYNC_model *m,int id,int c,int d,THD_3dim_dataset *reference,float **maps,float *mask,float **support,int nsupport,int nvalid,const int *rows)
{
   char job[96],base[THD_MAX_NAME],name[THD_MAX_NAME],pairs[128],table[128],script[128],maskfile[128],supportfile[128],fit[128];
   int i,j,k,p=0,saved_directory,nedge=m->nsub*(m->nsub-1)/2; THD_3dim_dataset *out; FILE *fp;
   const INSYNC_options *o=m->options;
   if(!nvalid) MODEL_FAIL("sel%06d condition pair %d/%d has zero valid model support; bundle remains incomplete",id,c+1,d+1);
   jobbase(job,id,c,d); snprintf(pairs,sizeof(pairs),"%s.pairs.nii.gz",job); snprintf(maskfile,sizeof(maskfile),"%s.mask.nii.gz",job);
   snprintf(supportfile,sizeof(supportfile),"%s.support.nii.gz",job); snprintf(table,sizeof(table),"%s.table.txt",job);
   snprintf(script,sizeof(script),"%s.sh",job); snprintf(fit,sizeof(fit),"%s.fit.nii.gz",job);
   /* AFNI's NIfTI writer rejects whitespace/apostrophes in the prefix itself.
      Write safe local basenames inside the quoted bundle directory instead. */
   saved_directory=open(".",O_RDONLY);
   if(saved_directory<0 || chdir(m->directory)) MODEL_FAIL("cannot enter bundle directory");
   for(k=0;k<3;k++){
      int nb=k==0?nedge:k==1?1:nsupport;
      path(name,m,k==0?pairs:k==1?maskfile:supportfile); out=EDIT_empty_copy(reference);
      EDIT_dset_items(out,ADN_prefix,k==0?pairs:k==1?maskfile:supportfile,ADN_nvals,nb,ADN_ntt,0,ADN_brick_fac,NULL,ADN_type,HEAD_FUNC_TYPE,ADN_func_type,FUNC_BUCK_TYPE,ADN_datum_all,MRI_float,ADN_none);
      tross_Copy_History(reference,out);
      for(i=0;i<nb;i++){
         float *data=k==0?maps[i]:k==1?mask:support[i];
         EDIT_substitute_brick(out,i,MRI_float,data); EDIT_BRICK_TO_NOSTAT(out,i);
         if(k==0) snprintf(base,sizeof(base),"Z_pair%06d",i+1);
         else if(k==1) strcpy(base,"ModelValid");
         else if(nb==1) strcpy(base,"ValidTR");
         else snprintf(base,sizeof(base),"B%d_%s",i/3+1,i%3==0?"RetainedTR":i%3==1?"Eligible":"Weight");
         EDIT_BRICK_LABEL(out,i,base);
      }
      {
         niftiwr_opts_t write_options={0,k==0?pairs:k==1?maskfile:supportfile};
         THD_set_dataset_attributes(out);
         if(THD_write_nifti(out,write_options) || !THD_is_file(name)) MODEL_FAIL("failed writing %s",name);
      }
      /* Caller owns arrays until all three datasets have been written. */
      for(i=0;i<nb;i++) mri_clear_data_pointer(DSET_BRICK(out,i));
      DSET_delete(out);
   }
   if(fchdir(saved_directory)) MODEL_FAIL("cannot restore working directory");
   close(saved_directory);
   fp=openfile(m,table);
   fputs("Subj1 Subj2 Group1 Group2 GroupPair cond cond1 cond2",fp);
   for(k=0;k<m->ncov;k++) fprintf(fp," %s1 %s2",m->table->cname[m->covariates[k]],m->table->cname[m->covariates[k]]);
   fputs(" InputFile\n",fp);
   for(i=0;i<m->nsub;i++) for(j=i+1;j<m->nsub;j++,p++){
      const INSYNC_source *a=m->schedule->source+m->sources[c*m->nsub+i],*b=m->schedule->source+m->sources[d*m->nsub+j];
      const char *ga=a->group,*gb=b->group,*ca=m->conditions[c],*cb=m->conditions[d];
      fprintf(fp,"%s %s %s %s %s__%s %s__%s %s %s",a->subject,b->subject,ga,gb,strcmp(ga,gb)<0?ga:gb,strcmp(ga,gb)<0?gb:ga,strcmp(ca,cb)<0?ca:cb,strcmp(ca,cb)<0?cb:ca,ca,cb);
      for(k=0;k<m->ncov;k++) fprintf(fp," %s %s",DT_CELL(m->table,a->row,m->covariates[k]),DT_CELL(m->table,b->row,m->covariates[k]));
      fprintf(fp," %s[%d]\n",pairs,p);
   }
   closefile(fp);
   fp=openfile(m,script); fputs("#!/bin/sh\nset -eu\ncd -- ",fp); shell(fp,m->directory); fputc('\n',fp);
   fputs("grep -q '^# Status: complete$' jobs.tsv || { echo 'Incomplete 3dInSync model bundle' >&2; exit 1; }\n",fp);
   fprintf(fp,"grep -F -q '%s\t' jobs.tsv || { echo 'Job is not in the current bundle' >&2; exit 1; }\n",script);
   fputs("# Independent fit: no across-selection inference or multiplicity correction.\nexec 3dISC -prefix ",fp); shell(fp,fit); fputs(" -mask ",fp); shell(fp,maskfile);
   fputs(" -model ",fp); shell(fp,o->isc_model?o->isc_model:"1+(1|Subj1)+(1|Subj2)"); fprintf(fp," -jobs %d",o->isc_jobs_given?o->isc_jobs:1);
   if(o->isc_qvars){ fputs(" -qVars ",fp); shell(fp,o->isc_qvars); }
   if(o->isc_centers){ fputs(" -qVarCenters ",fp); shell(fp,o->isc_centers); }
   for(i=0;i<o->isc_nglt;i++){ fputs(" -gltCode ",fp); shell(fp,o->isc_glt_names[i]); fputc(' ',fp); shell(fp,o->isc_glt_weights[i]); }
   fputs(" -dataTable ",fp); snprintf(base,sizeof(base),"@%s",table); shell(fp,base); fputc('\n',fp); closefile(fp);
   path(name,m,script); if(chmod(name,0755)) MODEL_FAIL("cannot mark job script executable");
   fprintf(m->master,"sh %s\n",script);
   fprintf(m->index,"sel%06d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%d\n",id,m->conditions[c],m->conditions[d],pairs,maskfile,supportfile,table,script,fit,nvalid);
   /* Support block order is explicit even when segment names differ by condition. */
   if(nsupport>1) for(i=0;i<nsupport/3;i++){
      const INSYNC_segment *e=m->schedule->segment+rows[i];
      fprintf(m->index,"# %s support block %d: run %d original TR %d..%d; bricks %d,%d,%d\n",job,i+1,e->run,e->original_start,e->original_stop,3*i,3*i+1,3*i+2);
   }
   if(fflush(m->index) || ferror(m->master)) MODEL_FAIL("failed writing job index/scripts");
}
void INSYNC_model_finish(INSYNC_model *m)
{
   char name[THD_MAX_NAME];
   closefile(m->master); path(name,m,"run_all.sh");
   if(chmod(name,0755)) MODEL_FAIL("cannot mark master script executable");
   fprintf(m->index,"# Status: complete\n"); closefile(m->index);
   free(m->covariates); free(m);
}
