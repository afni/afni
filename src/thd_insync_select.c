/* M1/M2: normalize source rows and construct original-time schedules.
   Deliberately no DSET_load/THD_get_voxel calls: preflight reads headers only. */
#include "mrilib.h"
#include "thd_insync_select.h"
#define DONT_INCLUDE_ANALYZE_STRUCT
#include "nifti2_io.h"
#include <errno.h>
#include <limits.h>
#include <stdint.h>

#define SEL_FAIL(...) ERROR_exit("3dInSync selection: " __VA_ARGS__)

static void *allocate(size_t n, size_t size)
{
   void *p;
   if(size && n>SIZE_MAX/size) SEL_FAIL("allocation size overflow");
   p=calloc(n?n:1,size);
   if(!p) SEL_FAIL("cannot allocate schedule memory");
   return p;
}

int INSYNC_selection_int(const char *value, int minimum, const char *name)
{
   char *end; long n;
   if(!value || !*value) SEL_FAIL("%s requires an integer",name);
   /* strtol accepts whitespace and signs: our new indices are decimal digits. */
   for(end=(char *)value; *end; end++)
      if(*end<'0' || *end>'9') SEL_FAIL("%s: '%s' is not a decimal integer",name,value);
   errno=0; n=strtol(value,&end,10);
   if(errno || *end || n<minimum || n>INT_MAX)
      SEL_FAIL("%s: integer must be in [%d,%d]",name,minimum,INT_MAX);
   return (int)n;
}

int INSYNC_selection_requested(const INSYNC_options *o, THD_datatable *t)
{
   const char *cols[]={"Selection","Segment","StartTR","StopTR","Xmat","XmatFull"};
   int i;
   if(o->temporal_censor || o->block_summary_given || o->block_missing_given || o->min_blocks_given || o->min_block_frac_given || o->block_weight_given || o->min_valid_given || o->min_valid_frac_given || o->combine_given || o->segment_center_given || o->window || o->step_given || o->run_starts || o->tr_given || o->xmat_blocks || o->xmat_stims || o->xmat_runs || o->xmat_threshold_given || o->xmat_across_runs) return 1;
   if(t) for(i=0;i<6;i++) if(THD_datatable_column(t,(char *)cols[i])>=0) return 1;
   return 0;
}

static int source_compare(const void *a, const void *b)
{
   const INSYNC_source *x=a,*y=b;
   int c=strcmp(x->subject,y->subject);
   return c?c:strcmp(x->condition,y->condition);
}

static int segment_key(const INSYNC_segment *x,const INSYNC_segment *y)
{
   int c;
   if(x->stimulus!=y->stimulus) return x->stimulus<y->stimulus?-1:1;
   c=strcmp(x->label,y->label);
   if(c) return c;
   if(x->window!=y->window) return x->window<y->window?-1:1;
   if(x->run!=y->run) return x->run<y->run?-1:1;
   if(x->start!=y->start) return x->start<y->start?-1:1;
   if(x->stop!=y->stop) return x->stop<y->stop?-1:1;
   return strcmp(x->segment,y->segment);
}

static int by_source(const void *a,const void *b)
{
   const INSYNC_segment *x=a,*y=b;
   return x->source!=y->source?(x->source<y->source?-1:1):segment_key(x,y);
}

static int by_segment_identity(const void *a,const void *b)
{
   const INSYNC_segment *x=a,*y=b;
   int c;
   if(x->source!=y->source) return x->source<y->source?-1:1;
   c=strcmp(x->label,y->label); if(c) return c;
   if(x->run!=y->run) return x->run<y->run?-1:1;
   return strcmp(x->segment,y->segment);
}

/* Compare each subject's sorted nominal schedule to one reference in the
   same condition. This also checks intervals producing zero windows. */
static void xmat_counts(FILE *,const INSYNC_schedule *);

static void validate_nominal(INSYNC_schedule *s)
{
   int i,j,k;
   int *first=allocate(s->nsource,sizeof(int)),*count=allocate(s->nsource,sizeof(int));
   qsort(s->requested,s->nrequested,sizeof(*s->requested),by_segment_identity);
   for(i=1;i<s->nrequested;i++) if(!by_segment_identity(s->requested+i-1,s->requested+i)){
      INSYNC_segment *e=s->requested+i;
      SEL_FAIL("duplicate segment key for Subj %s selection %s, rows %d and %d",s->source[e->source].subject,e->label,s->requested[i-1].row+1,e->row+1);
   }
   qsort(s->requested,s->nrequested,sizeof(*s->requested),by_source);
   for(i=0;i<s->nsource;i++) first[i]=-1;
   for(i=0;i<s->nrequested;i++){
      INSYNC_segment *e=s->requested+i;
      if(first[e->source]<0) first[e->source]=i;
      count[e->source]++;
      if(i){
         INSYNC_segment *previous=s->requested+i-1;
         if(previous->source==e->source && !strcmp(previous->label,e->label) &&
            previous->run==e->run && previous->stop>=e->start)
            SEL_FAIL("overlapping segments within selection %s for Subj %s, rows %d and %d",e->label,s->source[e->source].subject,previous->row+1,e->row+1);
      }
   }
   for(i=0;i<s->nsource;i++) for(j=0;j<i;j++)
      if(!strcmp(s->source[i].condition,s->source[j].condition)){
         if(count[i]!=count[j]) { xmat_counts(stderr,s); SEL_FAIL("Subj %s condition %s has missing/misaligned segments (%d vs %d for Subj %s)",s->source[i].subject,s->source[i].condition,count[i],count[j],s->source[j].subject); }
         for(k=0;k<count[i];k++) if(segment_key(s->requested+first[i]+k,s->requested+first[j]+k)){
            INSYNC_segment *e=s->requested+first[i]+k;
            xmat_counts(stderr,s);
            SEL_FAIL("Subj %s condition %s has missing/misaligned selection %s segment %s run %d at row %d (reference Subj %s row %d)",s->source[i].subject,s->source[i].condition,e->label,e->segment,e->run,e->row+1,s->source[j].subject,s->requested[first[j]+k].row+1);
         }
         break;
      }
   free(first); free(count);
}

/* Aggregates share IDs across constituent segments. Windows never do. */
static int selection_key(const INSYNC_segment *x,const INSYNC_segment *y)
{
   int c;
   if(x->stimulus!=y->stimulus) return x->stimulus<y->stimulus?-1:1;
   c=strcmp(x->label,y->label);
   if(c) return c;
   if(x->window!=y->window) return x->window<y->window?-1:1;
   return x->window?segment_key(x,y):0;
}

static int by_selection(const void *a,const void *b)
{
   const INSYNC_segment *x=a,*y=b;
   int c=selection_key(x,y);
   return c?c:by_source(a,b);
}

static void add_segment(INSYNC_schedule *s,int *capacity,int source,int row,
                        const char *label,const char *segment,int run,int start,int stop,int window)
{
   INSYNC_segment *e;
   if(s->nsegment==INT_MAX) SEL_FAIL("too many expanded schedule rows");
   if(s->nsegment==*capacity){
      size_t next=*capacity?(size_t)*capacity*2:64;
      void *p;
      if(next>INT_MAX) next=INT_MAX;
      if(next>SIZE_MAX/sizeof(*s->segment)) SEL_FAIL("schedule size overflow");
      p=realloc(s->segment,next*sizeof(*s->segment));
      if(!p) SEL_FAIL("cannot grow schedule");
      s->segment=p; *capacity=(int)next;
   }
   e=s->segment+s->nsegment++;
   *e=(INSYNC_segment){.source=source,.row=row,.run=run,.start=start,.stop=stop,
      .original_start=s->runstart[run-1]+start,.original_stop=s->runstart[run-1]+stop,
      .window=window,.kept=stop-start+1,.own_kept=stop-start+1,.stimulus=-1,.label=strdup(label),.segment=strdup(segment)};
   if(!e->label || !e->segment) SEL_FAIL("cannot copy selection labels");
}

static void expand(INSYNC_schedule *s,int *cap,const INSYNC_options *o,int source,int row,
                   const char *label,const char *segment,int run,int start,int stop)
{
   /* Retain nominal intervals even when they yield zero windows. */
   INSYNC_schedule nominal=*s;
   nominal.segment=s->requested; nominal.nsegment=s->nrequested;
   add_segment(&nominal,&s->requested_capacity,source,row,label,segment,run,start,stop,0);
   s->requested=nominal.segment; s->nrequested=nominal.nsegment;
   if(o->window){
      long long at, last=(long long)stop-o->window+1;
      if(last<start){ s->nempty++; return; }
      for(at=start;at<=last;at+=o->step)
         add_segment(s,cap,source,row,label,segment,run,(int)at,(int)at+o->window-1,1);
   } else add_segment(s,cap,source,row,label,segment,run,start,stop,0);
}

/* AFNI/NIfTI normalizes an absent pixdim[4] to 1. Inspect the raw header
   (without voxel data) so preflight does not invent seconds or reject -tr
   merely because the library supplied that fallback. Preserve DSET_TR_SEC
   for real timing, including adjustments made by dataset selectors. */
static double header_tr(THD_3dim_dataset *d)
{
   double tr=DSET_TR_SEC(d);
   if(d->dblk && d->dblk->diskptr && d->dblk->diskptr->storage_mode==STORAGE_BY_NIFTI){
      int version=0,units=0; double dt=0;
      void *header=nifti_read_header(DSET_BRIKNAME(d),&version,0);
      if(!header) SEL_FAIL("cannot read timing header '%s'",DSET_BRIKNAME(d));
      if(version==1){ nifti_1_header *h=header; dt=h->pixdim[4]; units=h->xyzt_units & 0x38; }
      else if(version==2){ nifti_2_header *h=header; dt=h->pixdim[4]; units=h->xyzt_units & 0x38; }
      free(header);
      if(!isfinite(dt) || dt<0) SEL_FAIL("invalid raw TR in '%s'",DSET_BRIKNAME(d));
      if(dt==0 || (units!=NIFTI_UNITS_SEC && units!=NIFTI_UNITS_MSEC && units!=NIFTI_UNITS_USEC)) return 0;
   }
   return tr;
}

static int same_tr(double a,double b)
{
   return fabs(a-b)<=fmax(1.e-6,1.e-6*fmax(a,b));
}

static void run_starts(INSYNC_schedule *s,const char *list)
{
   char *copy,*p,*next; int n=1,i=0;
   if(!list){ s->runstart=allocate(1,sizeof(int)); s->nrun=1; return; }
   for(p=(char *)list;*p;p++) if(*p==','){
      if(n==INT_MAX) SEL_FAIL("too many runs");
      n++;
   }
   s->runstart=allocate(n,sizeof(int)); s->nrun=n;
   copy=strdup(list); if(!copy) SEL_FAIL("cannot copy run starts");
   for(p=copy;p;p=next){
      next=strchr(p,','); if(next) *next++='\0';
      s->runstart[i]=INSYNC_selection_int(p,0,"-run_starts");
      if((i==0 && s->runstart[i]!=0) || (i>0 && s->runstart[i]<=s->runstart[i-1]) || s->runstart[i]>=s->ntime)
         SEL_FAIL("-run_starts must start at 0 and increase within %d input TRs",s->ntime);
      i++;
   }
   free(copy);
}

static void check_header(THD_3dim_dataset *first,const char *name,const char *kind)
{
   THD_3dim_dataset *d;
   if(!name) return;
   d=THD_open_dataset((char *)name);
   if(!d) SEL_FAIL("cannot open %s '%s'",kind,name);
   if(!EQUIV_GRIDS(first,d)) SEL_FAIL("%s '%s' is off-grid",kind,name);
   DSET_delete(d);
}

/* The shared reader accepts legacy permissive NIML lists. Preflight needs
   exact metadata: never let malformed text become a default zero/index. */
static long metadata_integer(const char **cursor,const char *name,const char *path)
{
   char *end; long value; const char *p=*cursor;
   while(isspace((unsigned char)*p)) p++;
   if(*p<'0' || *p>'9') SEL_FAIL("invalid Xmat %s in '%s'",name,path);
   errno=0; value=strtol(p,&end,10);
   if(errno || value>INT_MAX) SEL_FAIL("invalid Xmat %s in '%s'",name,path);
   while(isspace((unsigned char)*end)) end++;
   *cursor=end; return value;
}

static void metadata_list(const THD_xmat *x,const char *name,int n,const int *expected)
{
   const char *p=THD_xmat_attr(x,name); int count=0;
   if(!p) SEL_FAIL("Xmat '%s' requires %s",x->fname,name);
   for(;;){
      long start=metadata_integer(&p,name,x->fname),stop=start,copies=1,k;
      if(p[0]=='.' && p[1]=='.'){
         p+=2; stop=metadata_integer(&p,name,x->fname);
         if(stop<start) SEL_FAIL("descending Xmat %s in '%s'",name,x->fname);
         copies=stop-start+1;
      } else if(*p=='@'){
         p++; copies=start; start=stop=metadata_integer(&p,name,x->fname);
      }
      if(copies<1 || copies>n-count) SEL_FAIL("invalid Xmat %s length in '%s'",name,x->fname);
      for(k=0;k<copies;k++,count++) if(expected && expected[count]!=(stop==start?start:start+k))
         SEL_FAIL("invalid Xmat %s in '%s'",name,x->fname);
      if(!*p) break;
      if(*p!=',' && *p!=';') SEL_FAIL("invalid Xmat %s in '%s'",name,x->fname);
      p++;
   }
   if(count!=n) SEL_FAIL("invalid Xmat %s length in '%s'",name,x->fname);
}

/* Cached designs are owned once, while source pointers may share them. */
static THD_xmat *design_read(INSYNC_schedule *s,const char *path)
{
   int i,j; char err[1024]; THD_xmat *x;
   for(i=0;i<s->ndesign;i++) if(!strcmp(s->design[i]->fname,path)) return s->design[i];
   x=THD_xmat_read(path,err,sizeof(err));
   if(!x) SEL_FAIL("Xmat '%s': %s",path,err);
   if(!THD_xmat_attr(x,"NRowFull") || !THD_xmat_attr(x,"RunStart") || !THD_xmat_attr(x,"GoodList"))
      SEL_FAIL("Xmat '%s' requires explicit NRowFull, RunStart and GoodList",path);
   {
      const char *value=THD_xmat_attr(x,"NRowFull"); char *end; double n;
      errno=0; n=strtod(value,&end); while(isspace((unsigned char)*end)) end++;
      if(errno || end==value || *end || !isfinite(n) || n!=x->nfull || n<1 || floor(n)!=n)
         SEL_FAIL("invalid Xmat NRowFull in '%s'",path);
      value=THD_xmat_attr(x,"RowTR");
      if(value){
         errno=0; n=strtod(value,&end); while(isspace((unsigned char)*end)) end++;
         if(errno || end==value || *end || !isfinite(n) || n<0) SEL_FAIL("invalid Xmat RowTR in '%s'",path);
      }
   }
   if(!THD_xmat_attr(x,"ColumnLabels")) SEL_FAIL("Xmat '%s' requires ColumnLabels",path);
   if(!isfinite(x->tr) || x->tr<0) SEL_FAIL("invalid Xmat RowTR in '%s'",path);
   metadata_list(x,"GoodList",x->nrow,x->good);
   metadata_list(x,"RunStart",x->nrun,x->runstart);
   {
      NI_str_array *labels=NI_decode_string_list((char *)THD_xmat_attr(x,"ColumnLabels"),";");
      if(!labels || labels->num!=x->ncol) SEL_FAIL("Xmat ColumnLabels count mismatch in '%s'",path);
      NI_delete_str_array(labels);
      if(THD_xmat_attr(x,"StimLabels") || THD_xmat_attr(x,"StimBots") || THD_xmat_attr(x,"StimTops")){
         int *ranges=allocate(x->nstim,sizeof(int));
         if(!x->nstim) SEL_FAIL("invalid Xmat stimulus metadata in '%s'",path);
         for(i=0;i<x->nstim;i++) ranges[i]=x->stim[i].bot;
         metadata_list(x,"StimBots",x->nstim,ranges);
         for(i=0;i<x->nstim;i++) ranges[i]=x->stim[i].top;
         metadata_list(x,"StimTops",x->nstim,ranges); free(ranges);
      }
      if(THD_xmat_attr(x,"Nstim")){
         const char *value=THD_xmat_attr(x,"Nstim"); long n=metadata_integer(&value,"Nstim",path);
         if(*value || n!=x->nstim) SEL_FAIL("Xmat Nstim mismatch in '%s'",path);
      }
   }
   for(i=0;i<x->nstim;i++){
      if(!*x->stim[i].label || x->stim[i].bot<0 || x->stim[i].top<x->stim[i].bot || x->stim[i].top>=x->ncol)
         SEL_FAIL("invalid stimulus range/label in Xmat '%s'",path);
      for(j=0;j<i;j++) if(!strcmp(x->stim[i].label,x->stim[j].label) ||
         (x->stim[i].bot<=x->stim[j].top && x->stim[j].bot<=x->stim[i].top))
         SEL_FAIL("duplicate stimulus label or overlapping column ranges in Xmat '%s'",path);
   }
   for(i=0;i<x->ncol;i++){
      if(!*x->collab[i]) SEL_FAIL("empty column label in Xmat '%s'",path);
      for(j=0;j<i;j++) if(!strcmp(x->collab[i],x->collab[j]))
         SEL_FAIL("duplicate column label in Xmat '%s'",path);
   }
   s->design[s->ndesign++]=x; return x;
}

static int stim_index(const THD_xmat *x,const char *label)
{
   int j; for(j=0;j<x->nstim;j++) if(!strcmp(x->stim[j].label,label)) return j;
   SEL_FAIL("Xmat '%s' is missing stimulus '%s'",x->fname,label); return -1;
}

/* Full and censored counterparts describe the same design, including values
   at retained rows. Across subjects, stimulus columns may be reordered. */
static void matching_full(const THD_xmat *x,const THD_xmat *f)
{
   int i,j;
   if(f->nrow!=f->nfull || x->nfull!=f->nfull || x->ncol!=f->ncol || x->nrun!=f->nrun || x->nstim!=f->nstim)
      SEL_FAIL("XmatFull '%s' does not match Xmat '%s' dimensions/full timeline",f->fname,x->fname);
   for(i=0;i<f->nrow;i++) if(f->good[i]!=i) SEL_FAIL("XmatFull must be uncensored");
   for(i=0;i<x->nrun;i++) if(x->runstart[i]!=f->runstart[i]) SEL_FAIL("XmatFull RunStart mismatch");
   if(x->tr>0 && f->tr>0 && !same_tr(x->tr,f->tr)) SEL_FAIL("XmatFull RowTR mismatch");
   for(i=0;i<x->nstim;i++){
      j=stim_index(f,x->stim[i].label);
      if(x->stim[i].bot!=f->stim[j].bot || x->stim[i].top!=f->stim[j].top) SEL_FAIL("XmatFull stimulus range mismatch");
   }
   for(j=0;j<x->ncol;j++){
      if(strcmp(x->collab[j],f->collab[j])) SEL_FAIL("XmatFull column label mismatch");
      for(i=0;i<x->nrow;i++){
         double a=x->mat[(size_t)j*x->nrow+i],b=f->mat[(size_t)j*f->nrow+x->good[i]];
         if(!isfinite(a) || !isfinite(b) || fabs(a-b)>1.e-6*fmax(1.,fmax(fabs(a),fabs(b))))
            SEL_FAIL("XmatFull retained value mismatch: column %d original TR %d",j,x->good[i]);
      }
   }
}

static void xmat_prepare(INSYNC_schedule *s,const INSYNC_options *o,THD_datatable *t,int xc,int fc)
{
   int i,j,k,reference=0; THD_xmat *ref; char *copy,*p,*next;
   s->xmat_mode=1;
   s->design=allocate((size_t)s->nsource*2,sizeof(*s->design));
   s->xmat=allocate(s->nsource,sizeof(*s->xmat)); s->full=allocate(s->nsource,sizeof(*s->full));
   for(i=0;i<s->nsource;i++){
      INSYNC_source *source=s->source+i; THD_xmat *x,*f;
      if(strchr(source->file,'[') || strchr(source->file,'{')) SEL_FAIL("Xmat inputs require an unselected full timeline: %s",source->file);
      x=s->xmat[i]=design_read(s,DT_CELL(t,source->row,xc));
      f=s->full[i]=fc>=0?design_read(s,DT_CELL(t,source->row,fc)):x;
      if(fc>=0) matching_full(x,f);
      if(x->nfull!=s->ntime) SEL_FAIL("Subj %s: Xmat NRowFull %d differs from input length %d",source->subject,x->nfull,s->ntime);
      if(o->xmat_blocks && f->nrow!=f->nfull) SEL_FAIL("Subj %s: censored Xmat needs matching uncensored XmatFull for block boundaries",source->subject);
      if(source->row<s->source[reference].row) reference=i;
      if(x->tr>0 && f->tr>0 && !same_tr(x->tr,f->tr)) SEL_FAIL("Xmat RowTR mismatch");
      {
         double tr=f->tr>0?f->tr:x->tr;
         if(source->tr>0 && tr>0 && !same_tr(source->tr,tr)) SEL_FAIL("Subj %s: Xmat RowTR conflicts with input TR",source->subject);
         if(source->tr<=0) source->tr=tr;
      }
      for(j=0;j<x->nrow;j++) s->keep[(size_t)i*s->ntime+x->good[j]]=1;
   }
   ref=s->full[reference];
   if(o->run_starts){
      if(s->nrun!=ref->nrun) SEL_FAIL("-run_starts conflicts with Xmat RunStart");
      for(j=0;j<s->nrun;j++) if(s->runstart[j]!=ref->runstart[j]) SEL_FAIL("-run_starts conflicts with Xmat RunStart");
   }
   free(s->runstart); s->nrun=ref->nrun; s->runstart=allocate(s->nrun,sizeof(int));
   memcpy(s->runstart,ref->runstart,s->nrun*sizeof(int));
   s->tr=s->source[0].tr;
   for(i=0;i<s->nsource;i++){
      THD_xmat *x=s->xmat[i];
      if(x->nrun!=s->nrun) SEL_FAIL("Subj %s: Xmat RunStart mismatch",s->source[i].subject);
      for(j=0;j<s->nrun;j++) if(x->runstart[j]!=s->runstart[j]) SEL_FAIL("Subj %s: Xmat RunStart mismatch",s->source[i].subject);
      for(j=0;j<i;j++) if(s->source[i].tr>0 && s->source[j].tr>0 && !same_tr(s->source[i].tr,s->source[j].tr)) SEL_FAIL("Xmat/input TR mismatch across subjects");
      if(s->source[i].tr<=0) s->tr=0;
   }
   s->run_selected=allocate(s->nrun,sizeof(int));
   if(!o->xmat_runs) for(i=0;i<s->nrun;i++) s->run_selected[i]=1;
   else {
      copy=strdup(o->xmat_runs);
      for(p=copy;p;p=next){
         next=strchr(p,','); if(next) *next++=0;
         i=INSYNC_selection_int(p,1,"-xmat_runs")-1;
         if(i>=s->nrun || s->run_selected[i]) SEL_FAIL("unknown or duplicate -xmat_runs entry '%s'",p);
         s->run_selected[i]=1;
      }
      free(copy);
   }
   if(!o->xmat_blocks) return;
   for(i=0;i<s->nsource;i++) if(!THD_xmat_attr(s->full[i],"StimLabels") || !THD_xmat_attr(s->full[i],"StimBots") || !THD_xmat_attr(s->full[i],"StimTops"))
      SEL_FAIL("Xmat block expansion requires StimLabels, StimBots and StimTops");
   {
      int *wanted=allocate(ref->nstim,sizeof(int));
      if(!o->xmat_stims) for(i=0;i<ref->nstim;i++) wanted[i]=1;
      else {
         copy=strdup(o->xmat_stims);
         for(p=copy;p;p=next){
            next=strchr(p,','); if(next) *next++=0;
            if(!*p) SEL_FAIL("empty -xmat_stims entry");
            i=stim_index(ref,p);
            if(wanted[i]) SEL_FAIL("duplicate -xmat_stims entry '%s'",p);
            wanted[i]=1;
         }
         free(copy);
      }
      s->stimlabel=allocate(ref->nstim,sizeof(char *));
      s->stimordinal=allocate(ref->nstim,sizeof(int));
      for(i=0;i<ref->nstim;i++) if(wanted[i]) { s->stimordinal[s->nstim]=i; s->stimlabel[s->nstim++]=strdup(ref->stim[i].label); }
      free(wanted);
   }
   if(!s->nstim) SEL_FAIL("Xmat has no selected stimuli");
   s->overlap=allocate((size_t)s->nsource*s->nrun,sizeof(int));
   /* Validate selected columns before any expansion. */
   for(i=0;i<s->nsource;i++) for(j=0;j<s->nstim;j++){
      THD_xmat *x=s->full[i]; int st=stim_index(x,s->stimlabel[j]); double peak=0;
      if(x->stim[st].bot!=x->stim[st].top) SEL_FAIL("stimulus %s in '%s' has multiple columns; use manual intervals",s->stimlabel[j],x->fname);
      for(k=0;k<x->nrow;k++){
         double v=x->mat[(size_t)x->stim[st].bot*x->nrow+k];
         if(!isfinite(v) || v<0) SEL_FAIL("stimulus %s in '%s' must be finite and nonnegative; use manual intervals",s->stimlabel[j],x->fname);
         if(v>peak) peak=v;
      }
      if(peak<=0) SEL_FAIL("stimulus %s in '%s' has zero peak",s->stimlabel[j],x->fname);
   }
}

static void tag_expansion(INSYNC_schedule *s,int first,int requested,int stim,int block,int aggregate)
{
   int i;
   for(i=first;i<s->nsegment;i++){ s->segment[i].stimulus=stim; s->segment[i].block=block; s->segment[i].aggregate=aggregate; }
   for(i=requested;i<s->nrequested;i++){ s->requested[i].stimulus=stim; s->requested[i].block=block; s->requested[i].aggregate=aggregate; }
}

static void xmat_expand(INSYNC_schedule *s,const INSYNC_options *o,int *cap)
{
   int i,j,r,k;
   for(i=0;i<s->nsource;i++){
      THD_xmat *x=s->full[i]; int *active=allocate(s->ntime,sizeof(int));
      for(j=0;j<s->nstim;j++){
         int st=stim_index(x,s->stimlabel[j]); float *v=x->mat+(size_t)x->stim[st].bot*x->nrow; double peak=0;
         for(k=0;k<s->ntime;k++) if(v[k]>peak) peak=v[k];
         for(r=0;r<s->nrun;r++) if(s->run_selected[r]){
            int begin=s->runstart[r],end=r+1<s->nrun?s->runstart[r+1]:s->ntime,block=0;
            for(k=begin;k<end;){
               int start,stop,a,b; char label[128],segment[64];
               if(v[k]/peak<o->xmat_threshold){ k++; continue; }
               start=k;
               while(k<end && v[k]/peak>=o->xmat_threshold){ active[k]++; k++; }
               stop=k-1; block++;
               /* IDs use the first design's stimulus index, not the filtered
                  position: filtering never renames an existing block. */
               st=s->stimordinal[j];
               snprintf(label,sizeof(label),"stim%04d_run%04d_block%04d",st+1,r+1,block);
               snprintf(segment,sizeof(segment),"run%04d_block%04d",r+1,block);
               a=s->nsegment; b=s->nrequested;
               expand(s,cap,o,i,s->source[i].row,label,segment,r+1,start-begin,stop-begin);
               tag_expansion(s,a,b,j,block,0);
               if(o->xmat_blocks==2){
                  if(o->xmat_across_runs) snprintf(label,sizeof(label),"stim%04d_all_runs",st+1);
                  else snprintf(label,sizeof(label),"stim%04d_run%04d_summary",st+1,r+1);
                  a=s->nsegment; b=s->nrequested;
                  expand(s,cap,o,i,s->source[i].row,label,segment,r+1,start-begin,stop-begin);
                  tag_expansion(s,a,b,j,block,1);
               }
            }
         }
      }
      for(r=0;r<s->nrun;r++) for(k=s->runstart[r];k<(r+1<s->nrun?s->runstart[r+1]:s->ntime);k++)
         if(active[k]>1) s->overlap[(size_t)i*s->nrun+r]++;
      free(active);
   }
}

static void token(FILE *,const char *);
static void xmat_counts(FILE *fp,const INSYNC_schedule *s)
{
   int i,j,r,k;
   if(!s->xmat_mode) return;
   fprintf(fp,"\nXmat expansion: %d unique designs read; response-defined support, not recovered stimulus onsets.\n",s->ndesign);
   if(!s->nstim) return;
   for(i=0;i<s->nsource;i++){
      for(j=0;j<i;j++) if(!strcmp(s->source[i].condition,s->source[j].condition)) break;
      if(j<i) continue;
      for(r=0;r<s->nrun;r++) if(s->run_selected[r]){
         int *total=allocate(s->nstim,sizeof(int)),grand=0;
         fprintf(fp,"\nBlock counts: Condition="); token(fp,s->source[i].condition); fprintf(fp," Run=%d\nSubject\tGroup",r+1);
         for(j=0;j<s->nstim;j++){ fputc('\t',fp); token(fp,s->stimlabel[j]); }
         fprintf(fp,"\tTotal\tOverlapTRs\n");
         for(j=0;j<s->nsource;j++) if(!strcmp(s->source[i].condition,s->source[j].condition)){
            int st,sum=0; token(fp,s->source[j].subject); fputc('\t',fp); token(fp,s->source[j].group);
            for(st=0;st<s->nstim;st++){
               int n=0;
               for(k=0;k<s->nrequested;k++) if(s->requested[k].source==j && s->requested[k].run==r+1 && s->requested[k].stimulus==st && !s->requested[k].aggregate) n++;
               fprintf(fp,"\t%d",n); total[st]+=n; sum+=n;
            }
            fprintf(fp,"\t%d\t%d\n",sum,s->overlap[(size_t)j*s->nrun+r]); grand+=sum;
         }
         fputs("Total\t-",fp); for(j=0;j<s->nstim;j++) fprintf(fp,"\t%d",total[j]); fprintf(fp,"\t%d\t-\n",grand); free(total);
      }
   }
}

void INSYNC_apply_censor(unsigned char *keep,int nt,const char *path)
{
   MRI_IMAGE *im=mri_read_1D((char *)path),*f; float *v; int i;
   if(!im) SEL_FAIL("cannot read Censor '%s'",path);
   f=mri_to_float(im); if(f!=im) mri_free(im);
   if(!f || f->nvox!=nt || (f->nx!=1 && f->ny!=1)) SEL_FAIL("Censor '%s' must be a vector of %d values",path,nt);
   v=MRI_FLOAT_PTR(f);
   for(i=0;i<nt;i++){
      if(!isfinite(v[i]) || (v[i]!=0 && v[i]!=1)) SEL_FAIL("invalid Censor value in '%s' at TR %d",path,i);
      keep[i]&=v[i]!=0;
   }
   mri_free(f);
}

static void censor_counts(INSYNC_schedule *s,const INSYNC_options *o,THD_datatable *t,int cc)
{
   int i,j,k; unsigned char *common=allocate(s->ntime,1), *global=allocate(s->ntime,1);
   memset(common,1,s->ntime); memset(global,1,s->ntime);
   if(o->censor_name) INSYNC_apply_censor(global,s->ntime,o->censor_name);
   for(i=0;i<s->nsource;i++){
      unsigned char *keep=s->keep+(size_t)i*s->ntime;
      if(cc>=0) INSYNC_apply_censor(keep,s->ntime,DT_CELL(t,s->source[i].row,cc));
      /* The global censor belongs to each source mask, too. */
      for(j=0;j<s->ntime;j++) keep[j]&=global[j];
      for(j=0;j<s->ntime;j++) common[j]&=keep[j];
   }
   for(k=0;k<2;k++){
      INSYNC_segment *rows=k?s->requested:s->segment; int n=k?s->nrequested:s->nsegment;
      for(i=0;i<n;i++){
         INSYNC_segment *e=rows+i; e->kept=e->own_kept=0;
         for(j=e->original_start;j<=e->original_stop;j++){
            e->kept+=common[j]; e->own_kept+=s->keep[(size_t)e->source*s->ntime+j];
         }
      }
   }
   free(common); free(global);
}

INSYNC_schedule *INSYNC_schedule_build(const INSYNC_options *o, THD_datatable *t)
{
   INSYNC_schedule *s=allocate(1,sizeof(*s));
   THD_3dim_dataset *first=NULL;
   int sel,seg,run,beg,end,cond,group,manual=0,r,c,i,j,k,cap=0,nsub=0;
   int *row_source,*reps,*starts,*counts;
   double known_tr=0;
   const char *controls[]={"Selection","Segment","Run","StartTR","StopTR"};
   int cols[5],xc,fc,cc;
   if(!t || t->nrow<1 || t->icol_subj<0 || t->icol_input<0)
      SEL_FAIL("preflight requires a nonempty table with explicit Subj and InputFile");
   if(o->combine_pool && (o->block_summary_given || o->block_missing_given || o->min_blocks_given || o->min_block_frac_given || o->block_weight_given)) SEL_FAIL("block-summary options cannot be combined with -combine_blocks pool");
   if(o->segment_center_given && !o->combine_pool) SEL_FAIL("-segment_center requires -combine_blocks pool");
   if(o->step_given && !o->window) SEL_FAIL("-step requires -window");
   if(o->window && (o->window<3 || o->step<1)) SEL_FAIL("invalid window/step");
   if(!THD_filename_ok(o->prefix)) SEL_FAIL("illegal prefix '%s'",o->prefix);
   for(c=0;c<t->ncol;c++) for(j=0;j<c;j++)
      if(!strcasecmp(t->cname[c],t->cname[j])) SEL_FAIL("duplicate column '%s'",t->cname[c]);
   for(i=0;i<5;i++){ cols[i]=THD_datatable_column(t,(char *)controls[i]); manual+=cols[i]>=0; }
   if(manual && manual!=5) SEL_FAIL("manual intervals require Selection Segment Run StartTR StopTR together");
   sel=cols[0]; seg=cols[1]; run=cols[2]; beg=cols[3]; end=cols[4];
   cond=THD_datatable_column(t,o->condition_column); group=THD_datatable_column(t,o->group_column);
   if(o->condition_column_given && cond<0) SEL_FAIL("condition column '%s' absent",o->condition_column);
   if(cond==t->icol_subj || cond==t->icol_input || group==t->icol_subj || group==t->icol_input ||
      (cond>=0 && cond==group)) SEL_FAIL("subject, input, condition, and group columns must be separate");
   for(i=0;i<5;i++) if(cols[i]>=0 && (cols[i]==cond || cols[i]==group)) SEL_FAIL("control column '%s' conflicts with condition/group",controls[i]);
   xc=THD_datatable_column(t,"Xmat"); fc=THD_datatable_column(t,"XmatFull"); cc=THD_datatable_column(t,o->censor_column);
   if(o->censor_column_given && cc<0) SEL_FAIL("censor column absent: %s",o->censor_column);
   if(cc==t->icol_subj || cc==t->icol_input) SEL_FAIL("Censor must be a separate source column");
   if(xc<0 && (fc>=0 || o->xmat_blocks || o->xmat_stims || o->xmat_runs || o->xmat_threshold_given || o->xmat_across_runs)) SEL_FAIL("Xmat column required for Xmat options/XmatFull");
   if(o->xmat_blocks && !o->xmat_threshold_given) SEL_FAIL("-xmat_blocks requires explicit -xmat_threshold");
   if(!o->xmat_blocks && (o->xmat_stims || o->xmat_threshold_given)) SEL_FAIL("-xmat_stims/-xmat_threshold require -xmat_blocks");
   if(o->xmat_across_runs && o->xmat_blocks!=2) SEL_FAIL("-xmat_across_runs requires -xmat_blocks by_stim");
   if(o->window && o->xmat_blocks==2) SEL_FAIL("-window requires -xmat_blocks each, not by_stim");
   if(manual && (o->xmat_blocks || o->xmat_runs)) SEL_FAIL("manual intervals cannot be combined with Xmat block/run selection");
   for(c=0;c<t->ncol;c++) if(c==xc || c==fc || c==cc){
      if(c==cond || c==group) SEL_FAIL("source metadata column '%s' conflicts with condition/group",t->cname[c]);
   }
   if(o->eo.edges || o->eo.rss || o->eo.events || o->eo.matrix || o->eo.nnull)
      SEL_FAIL("selection edge modes are deferred; omit these options");
   if(o->temporal_censor && o->temporal_mode!=INSYNC_TNULL_TIMESHIFT) SEL_FAIL("joint_shift censoring requires -temporal_null timeshift; censored phases are unsupported");
   if(o->temporal_censor && o->use_half) SEL_FAIL("joint_shift censoring requires float32 storage; omit -float16 so excluded artifacts cannot change quantization");
   if(!o->temporal_mode && (o->nnull || o->temporal_tail_given)) SEL_FAIL("-nnull/-temporal_tail require -temporal_null");
   if(o->temporal_mode && o->nnull<20) SEL_FAIL("-temporal_null requires -nnull >= 20");
   if(o->min_shift_given && o->temporal_mode!=INSYNC_TNULL_TIMESHIFT) SEL_FAIL("-min_shift applies only to timeshift");
   if(o->temporal_mode && (o->do_zcensor || o->missing_policy==INSYNC_MISSING_COMMON))
      SEL_FAIL("temporal nulls require intact runs: -zcensor/-missing common await M8a");
   if((o->roi_only || o->roi_sel || o->matrix_name) && !o->atlas_name) SEL_FAIL("ROI options require -atlas");
   s->source=allocate(t->nrow,sizeof(*s->source)); row_source=allocate(t->nrow,sizeof(int));
   for(r=0;r<t->nrow;r++){
      const char *who=t->subj[r],*condition=cond<0?"All":DT_CELL(t,r,cond);
      const char *grp=group<0?"All":DT_CELL(t,r,group);
      if(!*who || !*condition || !*grp || !t->fname[r] || !*t->fname[r]) SEL_FAIL("empty identity/path at table row %d",r+1);
      for(i=0;i<s->nsource;i++) if(!strcmp(who,s->source[i].subject)){
         int previous=s->source[i].row;
         for(c=0;c<t->ncol;c++){
            if(c==cond || c==t->icol_input) continue;
            if((c==xc || c==fc || c==cc) && strcmp(condition,s->source[i].condition)) continue;
            for(j=0;j<5 && c!=cols[j];j++);
            if(j<5) continue;
            if(strcmp(DT_CELL(t,r,c),DT_CELL(t,previous,c)))
               SEL_FAIL("Subj %s row %d: covariate '%s' differs from row %d",who,r+1,t->cname[c],previous+1);
         }
         if(!strcmp(condition,s->source[i].condition)) break;
      }
      if(i<s->nsource){
         if(!manual) SEL_FAIL("duplicate Subj %s condition %s in rows %d and %d",who,condition,s->source[i].row+1,r+1);
         if(strcmp(t->fname[r],s->source[i].file)) SEL_FAIL("Subj %s condition %s row %d changes InputFile",who,condition,r+1);
      } else {
         INSYNC_source *x=s->source+s->nsource++;
         x->row=r; x->subject=strdup(who); x->condition=strdup(condition);
         x->group=strdup(grp); x->file=strdup(t->fname[r]);
         if(!x->subject || !x->condition || !x->group || !x->file) SEL_FAIL("cannot copy source metadata");
      }
   }
   qsort(s->source,s->nsource,sizeof(*s->source),source_compare);
   reps=allocate(s->nsource,sizeof(int));
   for(i=0;i<s->nsource;i++){
      THD_3dim_dataset *d; double tr;
      INSYNC_source *x=s->source+i;
      reps[i]=x->row;
      if(i==0 || strcmp(x->subject,s->source[i-1].subject)) nsub++;
      d=THD_open_dataset(x->file);
      if(!d) SEL_FAIL("row %d Subj %s: cannot open '%s'",x->row+1,x->subject,x->file);
      x->ntime=DSET_NVALS(d); tr=header_tr(d);
      if(!isfinite(tr) || tr<0) SEL_FAIL("invalid TR in '%s'",x->file);
      if(o->tr_given && tr>0 && !same_tr(tr,o->selection_tr)) SEL_FAIL("-tr conflicts with header of '%s'",x->file);
      if(o->tr_given) tr=o->selection_tr;
      if(tr>0 && known_tr>0 && !same_tr(tr,known_tr)) SEL_FAIL("header TR mismatch for Subj %s",x->subject);
      if(tr>0) known_tr=tr;
      x->tr=tr;
      if(!first){ first=d; s->ntime=x->ntime; s->tr=tr; }
      else {
         if(x->ntime!=s->ntime) SEL_FAIL("row %d Subj %s: %d TRs; expected %d",x->row+1,x->subject,x->ntime,s->ntime);
         if(!EQUIV_GRIDS(first,d)) SEL_FAIL("row %d Subj %s is off-grid",x->row+1,x->subject);
         if(tr>0 && s->tr>0 && !same_tr(tr,s->tr)) SEL_FAIL("header TR mismatch for Subj %s",x->subject);
         if(tr<=0 || s->tr<=0) s->tr=0; /* no fabricated shared seconds */
         DSET_delete(d);
      }
      if(x->ntime<3) SEL_FAIL("Subj %s has fewer than 3 TRs",x->subject);
   }
   {
      int reference_count=1, begin=0;
      while(reference_count<s->nsource && !strcmp(s->source[0].subject,s->source[reference_count].subject)) reference_count++;
      while(begin<s->nsource){
         int finish=begin+1;
         while(finish<s->nsource && !strcmp(s->source[begin].subject,s->source[finish].subject)) finish++;
         if(finish-begin!=reference_count) SEL_FAIL("Subj %s has a missing condition (expected %d source conditions)",s->source[begin].subject,reference_count);
         for(j=0;j<reference_count;j++) if(strcmp(s->source[j].condition,s->source[begin+j].condition))
            SEL_FAIL("Subj %s has a missing condition %s",s->source[begin].subject,s->source[j].condition);
         begin=finish;
      }
   }
   if(nsub<3) SEL_FAIL("need at least 3 subjects; found %d",nsub);
   for(i=0;i<s->nsource;i++){
      int members=0;
      for(j=0;j<s->nsource;j++) if(!strcmp(s->source[i].condition,s->source[j].condition) &&
          !strcmp(s->source[i].group,s->source[j].group)) members++;
      if(members<3) SEL_FAIL("group %s condition %s has %d subjects; need 3",s->source[i].group,s->source[i].condition,members);
   }
   check_header(first,o->maskname,"mask"); check_header(first,o->atlas_name,"atlas");
   DSET_delete(first);
   s->sources=THD_datatable_select_rows(t,reps,s->nsource); free(reps);
   run_starts(s,o->run_starts);
   s->keep=allocate(s->nsource,(size_t)s->ntime);
   if(xc>=0) xmat_prepare(s,o,t,xc,fc);
   else memset(s->keep,1,(size_t)s->nsource*s->ntime);
   for(r=0;r<t->nrow;r++){
      for(i=0;i<s->nsource;i++) if(!strcmp(t->subj[r],s->source[i].subject) &&
         !strcmp(cond<0?"All":DT_CELL(t,r,cond),s->source[i].condition)) break;
      row_source[r]=i;
   }
   if(o->xmat_blocks) xmat_expand(s,o,&cap);
   else if(manual){
      for(r=0;r<t->nrow;r++){
         int rr,bb,ee,length;
         if(!*DT_CELL(t,r,sel) || !*DT_CELL(t,r,seg)) SEL_FAIL("empty selection/segment at row %d",r+1);
         rr=INSYNC_selection_int(DT_CELL(t,r,run),1,"Run");
         bb=INSYNC_selection_int(DT_CELL(t,r,beg),0,"StartTR");
         ee=INSYNC_selection_int(DT_CELL(t,r,end),0,"StopTR");
         if(rr>s->nrun) SEL_FAIL("row %d Subj %s: run %d exceeds %d runs",r+1,t->subj[r],rr,s->nrun);
         length=(rr<s->nrun?s->runstart[rr]:s->ntime)-s->runstart[rr-1];
         if(bb>ee || ee>=length) SEL_FAIL("row %d Subj %s selection %s: invalid interval %d..%d in run %d (length %d)",r+1,t->subj[r],DT_CELL(t,r,sel),bb,ee,rr,length);
         expand(s,&cap,o,row_source[r],r,DT_CELL(t,r,sel),DT_CELL(t,r,seg),rr,bb,ee);
      }
   } else for(i=0;i<s->nsource;i++) for(j=0;j<s->nrun;j++){
      if(s->run_selected && !s->run_selected[j]) continue;
      char label[64],segment[64]; int length=(j+1<s->nrun?s->runstart[j+1]:s->ntime)-s->runstart[j];
      snprintf(label,sizeof(label),"run%04d",j+1); snprintf(segment,sizeof(segment),"run%04d",j+1);
      expand(s,&cap,o,i,s->source[i].row,label,segment,j+1,0,length-1);
   }
   free(row_source);
   validate_nominal(s);
   if(!s->nsegment){ xmat_counts(stderr,s); SEL_FAIL("no complete windows/blocks: %d source segments are shorter than -window",s->nempty); }
   qsort(s->segment,s->nsegment,sizeof(*s->segment),by_source);
   starts=allocate(s->nsource,sizeof(int)); counts=allocate(s->nsource,sizeof(int));
   for(i=0;i<s->nsource;i++) starts[i]=-1;
   for(i=0;i<s->nsegment;i++){ k=s->segment[i].source; if(starts[k]<0) starts[k]=i; counts[k]++; }
   for(i=0;i<s->nsource;i++) for(j=0;j<i;j++) if(!strcmp(s->source[i].condition,s->source[j].condition)){
      if(counts[i]!=counts[j]) SEL_FAIL("expanded counts differ for Subj %s",s->source[i].subject);
      for(k=0;k<counts[i];k++) if(segment_key(s->segment+starts[i]+k,s->segment+starts[j]+k))
         SEL_FAIL("expanded schedule differs for Subj %s",s->source[i].subject);
      break;
   }
   free(starts); free(counts);
   qsort(s->requested,s->nrequested,sizeof(*s->requested),by_source);
   qsort(s->segment,s->nsegment,sizeof(*s->segment),by_selection);
   for(i=0;i<s->nsegment;i++){
      if(!i || selection_key(s->segment+i-1,s->segment+i)) s->nselection++;
      s->segment[i].selection_id=s->nselection;
   }
   if(o->block_summary_given || o->block_missing_given || o->min_blocks_given || o->min_block_frac_given || o->block_weight_given){
      int aggregate=0;
      for(i=0;i<s->nsegment && !aggregate;i++){
         if(s->segment[i].aggregate) aggregate=1;
         for(j=i+1;j<s->nsegment && s->segment[j].selection_id==s->segment[i].selection_id;j++) if(s->segment[j].source==s->segment[i].source) aggregate=1;
      }
      if(!aggregate) SEL_FAIL("block-summary options require an aggregate selection");
   }
   if(o->whole_summary){
      int id;
      if(s->nselection==INT_MAX) SEL_FAIL("too many selections for whole summary");
      id=++s->nselection;
      for(i=0;i<s->nsource;i++){
         INSYNC_schedule nominal;
         add_segment(s,&cap,i,s->source[i].row,"whole_input","whole_input",1,0,s->ntime-1,0);
         s->segment[s->nsegment-1].run=0; s->segment[s->nsegment-1].selection_id=id;
         nominal=*s; nominal.segment=s->requested; nominal.nsegment=s->nrequested;
         add_segment(&nominal,&s->requested_capacity,i,s->source[i].row,"whole_input","whole_input",1,0,s->ntime-1,0);
         nominal.segment[nominal.nsegment-1].run=0;
         s->requested=nominal.segment; s->nrequested=nominal.nsegment;
      }
   }
   censor_counts(s,o,t,cc);
   if(o->temporal_mode){
      int rr,ss,tt,used;
      for(rr=0;rr<s->nrun;rr++){
         int begin=s->runstart[rr],end=rr+1<s->nrun?s->runstart[rr+1]:s->ntime;
         used=0;
         for(i=0;i<s->nsegment;i++) if(s->segment[i].original_start<end && s->segment[i].original_stop>=begin){ used=1; break; }
         if(!used) continue;
         if(end-begin<3 || (o->temporal_mode==INSYNC_TNULL_TIMESHIFT && (long long)end-begin-2LL*o->min_shift+1<2))
            SEL_FAIL("run %d too short for temporal null/min_shift (%d TRs)",rr+1,end-begin);
         for(ss=0;ss<s->nsource;ss++) for(tt=begin;tt<end;tt++) if(!o->temporal_censor && !s->keep[(size_t)ss*s->ntime+tt])
            SEL_FAIL("temporal nulls require intact runs: run %d Subj %s censored TR %d; censor support awaits M8a",rr+1,s->source[ss].subject,tt);
      }
   }
   return s;
}

void INSYNC_schedule_free(INSYNC_schedule *s)
{
   int i; if(!s) return;
   for(i=0;i<s->nsource;i++){
      free(s->source[i].subject); free(s->source[i].condition); free(s->source[i].group); free(s->source[i].file);
   }
   for(i=0;i<s->nsegment;i++){ free(s->segment[i].label); free(s->segment[i].segment); }
   for(i=0;i<s->nrequested;i++){ free(s->requested[i].label); free(s->requested[i].segment); }
   for(i=0;i<s->ndesign;i++) THD_xmat_free(s->design[i]);
   for(i=0;i<s->nstim;i++) free(s->stimlabel[i]);
   free(s->stimlabel); free(s->stimordinal); free(s->design); free(s->xmat); free(s->full);
   free(s->run_selected); free(s->overlap); free(s->keep);
   free(s->requested);
   THD_free_datatable(s->sources); free(s->source); free(s->segment); free(s->runstart); free(s);
}

/* TSV escapes are documented in the schema; never permit an embedded newline
   or tab to create an extra record. Raw input labels are preserved on decode. */
static void token(FILE *fp,const char *value)
{
   for(;*value;value++) switch(*value){
      case '\\': fputs("\\\\",fp); break;
      case '\t': fputs("\\t",fp); break;
      case '\n': fputs("\\n",fp); break;
      case '\r': fputs("\\r",fp); break;
      default: fputc((unsigned char)*value,fp);
   }
}

int INSYNC_schedule_report(const INSYNC_options *o, THD_datatable *t, INSYNC_schedule *s)
{
   char manifest[THD_MAX_NAME],summary[THD_MAX_NAME]; FILE *fp,*report;
   int i,j,cc=THD_datatable_column(t,o->censor_column);
   if(snprintf(manifest,sizeof(manifest),"%s.selections.tsv",o->prefix)>=(int)sizeof(manifest) ||
      snprintf(summary,sizeof(summary),"%s.selection_summary.txt",o->prefix)>=(int)sizeof(summary)) SEL_FAIL("output path too long");
   if(!THD_ok_overwrite() && (THD_is_file(manifest) || THD_is_file(summary))) SEL_FAIL("preflight output already exists for prefix '%s'",o->prefix);
   fp=fopen(manifest,"w"); if(!fp) SEL_FAIL("cannot write '%s'",manifest);
   report=fopen(summary,"w"); if(!report){ fclose(fp); remove(manifest); SEL_FAIL("cannot write '%s'",summary); }
   fprintf(fp,"# 3dInSync selection schema 3; TSV escapes: backslash, tab, newline, CR\n");
   fprintf(fp,"# BlockSummary: %s; BlockMissing: %s; BlockWeight: equal; MinBlocks: %d; MinBlockFraction: %.17g\n",o->block_summary==INSYNC_SUMMARY_MEDIAN?"median":"fisher_mean",o->block_drop_common?"drop_common":"require_all",o->min_blocks,o->min_block_frac);
   fprintf(fp,"# GlobalCensor: "); token(fp,o->censor_name?o->censor_name:"NA"); fputc('\n',fp);
   if(o->xmat_blocks) fprintf(fp,"# XmatThreshold: %.17g; inclusive full-design peak fraction; overlap allowed; Blocks: %s; AcrossRuns: %d\n",o->xmat_threshold,o->xmat_blocks==2?"by_stim":"each",o->xmat_across_runs);
   fprintf(fp,"Subject\tCondition\tGroup\tSelectionID\tSelectionLabel\tSegmentID\tRole\tRun\tRunStartTR\tStartTR\tStopTR\tOriginalStartTR\tOriginalStopTR\tNominalN\tCensorKeptN\tTRSeconds\tSourceRow\tInputFile\tPolicy\tStimulus\tBlock\tSourceKeptN\tXmat\tXmatFull\tCensor\tParentSelectionID\tComponentSelectionID\n");
   for(i=0;i<s->nsegment;i++){
      INSYNC_segment *e=s->segment+i; INSYNC_source *x=s->source+e->source;
      token(fp,x->subject); fputc('\t',fp); token(fp,x->condition); fputc('\t',fp); token(fp,x->group);
      fprintf(fp,"\tsel%06d\t",e->selection_id); token(fp,e->label); fputc('\t',fp); token(fp,e->segment);
      fprintf(fp,"\t%s\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t",e->run==0?"whole_input":e->window?"window":e->aggregate?"block_aggregate_component":e->stimulus>=0?"block":"segment",e->run,e->run?s->runstart[e->run-1]:0,e->start,e->stop,e->original_start,e->original_stop,e->stop-e->start+1,e->kept);
      if(x->tr>0) fprintf(fp,"%.9g",x->tr); else fputs("NA",fp);
      fprintf(fp,"\t%d\t",e->row+1); token(fp,x->file);
      fprintf(fp,"\t%s\t",e->run==0?"whole_input":e->window?"sliding_complete":o->combine_pool?"pool":"block_summary");
      token(fp,e->stimulus>=0?s->stimlabel[e->stimulus]:"NA");
      fprintf(fp,"\t%d\t%d\t",e->block,e->own_kept);
      token(fp,s->xmat_mode?s->xmat[e->source]->fname:"NA"); fputc('\t',fp);
      token(fp,s->xmat_mode && s->full[e->source]->nrow==s->full[e->source]->nfull?s->full[e->source]->fname:"NA"); fputc('\t',fp);
      token(fp,cc>=0?DT_CELL(t,x->row,cc):"NA");
      if(e->parent_id) fprintf(fp,"\tsel%06d",e->parent_id); else fputs("\tNA",fp);
      if(e->component_id) fprintf(fp,"\tsel%06d\n",e->component_id); else fputs("\tNA\n",fp);
   }
   fprintf(report,"3dInSync selection preflight (schema 3)\nInput rows: %d\nUnique subject/condition sources: %d\nUnique selection IDs: %d\nExpanded source-segment rows: %d\nShort source segments producing zero windows: %d\n",t->nrow,s->nsource,s->nselection,s->nsegment,s->nempty);
   if(s->xmat_mode){
      fprintf(report,"Xmat threshold: ");
      if(o->xmat_blocks) fprintf(report,"%.17g of full-design peak; inclusive; overlap allowed\n",o->xmat_threshold);
      else fprintf(report,"NA (metadata only)\n");
      fprintf(report,"CensorKeptN uses the common intersection across all sources; SourceKeptN uses that source's GoodList, Censor and global censor.\n");
      xmat_counts(report,s);
   }
   if(o->check_table) fprintf(report,"No image samples loaded. Header geometry checked; mask contents, ROI labels, zcensor, and voxel/ROI nonfinite validity require image data and remain pending.\nCounts below are generated selections/segments, not independent observations.\n");
   else fprintf(report,"Schedule counts describe input censor metadata; final data-dependent counts and validity are in result buckets. The saved common censor vector includes zcensor.\n");
   fprintf(report,"Subject\tCondition\tGroup\tSelections\tSegments\tKeptMin\tKeptMax\n");
   for(i=0;i<s->nsource;i++){
      int selections=0,segments=0,last=-1,lo=INT_MAX,hi=0;
      for(j=0;j<s->nsegment;j++) if(s->segment[j].source==i){
         INSYNC_segment *e=s->segment+j; segments++;
         if(e->selection_id!=last){ selections++; last=e->selection_id; }
         if(e->kept<lo) lo=e->kept;
         if(e->kept>hi) hi=e->kept;
      }
      token(report,s->source[i].subject); fputc('\t',report); token(report,s->source[i].condition);
      fputc('\t',report); token(report,s->source[i].group);
      fprintf(report,"\t%d\t%d\t%d\t%d\n",selections,segments,segments?lo:0,hi);
   }
   fprintf(report,"\nSource censor coverage (full original timeline)\nSubject\tCondition\tNominalN\tGoodListKeptN\tSourceKeptN\tAdditionalExcludedN\n");
   for(i=0;i<s->nsource;i++){
      int kept=0, good=s->xmat_mode?s->xmat[i]->nrow:s->ntime;
      for(j=0;j<s->ntime;j++) kept+=s->keep[(size_t)i*s->ntime+j];
      token(report,s->source[i].subject); fputc('\t',report); token(report,s->source[i].condition);
      fprintf(report,"\t%d\t%d\t%d\t%d\n",s->ntime,good,kept,good-kept);
   }
   fprintf(report,"\nRequested intervals (before window expansion; nominal coordinates)\nSubject\tCondition\tSelection\tSegment\tRun\tStartTR\tStopTR\tNominalN\tCensorKeptN\tGeneratedRows\tSourceKeptN\n");
   for(i=0;i<s->nrequested;i++){
      INSYNC_segment *e=s->requested+i; INSYNC_source *x=s->source+e->source;
      int generated=0;
      for(j=0;j<s->nsegment;j++){
         INSYNC_segment *w=s->segment+j;
         if(w->source==e->source && w->run==e->run && !strcmp(w->label,e->label) &&
            !strcmp(w->segment,e->segment)) generated++;
      }
      token(report,x->subject); fputc('\t',report); token(report,x->condition); fputc('\t',report);
      token(report,e->label); fputc('\t',report); token(report,e->segment);
      fprintf(report,"\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n",e->run,e->start,e->stop,e->stop-e->start+1,e->kept,generated,e->own_kept);
   }
   {
      int failed=ferror(fp)|ferror(report);
      if(fclose(fp)) failed=1;
      if(fclose(report)) failed=1;
      if(failed){ remove(manifest); remove(summary); SEL_FAIL("failed writing preflight output"); }
   }
   if(o->show_table) THD_datatable_print(s->sources,stdout);
   if(!o->quiet){
      char line[4096]; report=fopen(summary,"r");
      if(report){ while(fgets(line,sizeof(line),report)) fputs(line,stdout); fclose(report); }
      INFO_message("3dInSync: wrote %s and %s (schedule metadata)",manifest,summary);
   }
   return 0;
}

int INSYNC_check_table(const INSYNC_options *o, THD_datatable *t)
{
   INSYNC_schedule *s=INSYNC_schedule_build(o,t);
   int status=INSYNC_schedule_report(o,t,s);
   INSYNC_schedule_free(s); return status;
}

void INSYNC_selection_token(FILE *fp,const char *value){ token(fp,value); }
