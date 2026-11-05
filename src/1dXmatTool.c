/*
 * Command-line operations on AFNI design matrices. thd_xmat owns matrix
 * parsing, extraction, metadata remapping and numerical diagnostics; this
 * file supplies CLI policy, operation-specific checks and output formatting.
 * No operation estimates betas or reconstructs an HRF from event timings.
 *
 * Index conventions are important throughout:
 *   - User-facing run numbers are one-based; internal run indices are zero-based.
 *   - Matrix rows are retained observations. good[row] maps each row to its
 *     original, full-timeline TR; subtract runstart[run] for a run-local TR.
 *   - Matrix columns and beta-volume selectors are zero-based. mat is stored
 *     column-major: mat[column * nrow + row].
 *
 * Most operations report on stdout. -show_trs reserves stdout exclusively for
 * a shell-substitutable selector and sends errors to stderr. Status 1 denotes
 * an operational/input error; validation failures and comparison differences
 * use status 2. Output files are created exclusively, never overwritten.
 */
#include "mrilib.h"
#include "thd_xmat.h"
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

/* Print the public CLI contract, including indexing and downstream examples. */
static void usage(void)
{
   puts(
"1dXmatTool: manipulate AFNI X.xmat.1D design matrices\n"
"\n"
"Split a multi-run 3dDeconvolve design into run-local matrices:\n"
"  1dXmatTool -infile X.xmat.1D -uncensored X.nocensor.xmat.1D \\\n"
"             -split_runs -prefix split/X\n"
"\n"
"Operations (one at a time; -describe and -validate may be combined):\n"
"  -describe / -show  Full-matrix and per-run rank, DF, censor/stimulus summary.\n"
"  -validate         As above; exit 2 for failed checks, 0 for pass.\n"
"  -for lss|reml     With validation: check per-run LSS trial-model ranks, or\n"
"                    full/per-run GLM rank and residual degrees of freedom.\n"
"  -rank_tol T       Relative singular-value cutoff on unit-norm columns\n"
"                    (default 1e-7). Duplicate pairs use 1-abs(cosine)<=T.\n"
"  -json FILE        Optional JSON report with describe/validate.\n"
"  -select_columns LIST  Zero-based column indices, e.g. 0..3,8,10.\n"
"  -select_labels LIST   Exact comma-separated ColumnLabels.\n"
"  -select_stimuli LIST  Exact comma-separated StimLabels.\n"
"  -select_groups LIST   baseline (-1), nuisance (0), stimuli (>0), or group IDs.\n"
"                    Selections combine by union; original column order stays.\n"
"                    Writes PREFIX.xmat.1D, .rows.1D and .columns.1D.\n"
"  -drop_glts        Required for selection when input contains GLTs.\n"
"  -compare FILE     Match unique column labels/full TR indices; distinguish\n"
"                    reorder, censor/run/stimulus changes and changed values.\n"
"                    Exit 2 for differences, 0 for equivalence (reordering OK).\n"
"  -atol A -rtol R   Comparison tolerance A+R*max(abs(a),abs(b)); default 1e-6.\n"
"TR selector query (stdout only; no output files):\n"
"  -show_trs         Print one comma-separated AFNI sub-brick selector.\n"
"                    Requires -runs N (one one-based run), one selection option,\n"
"                    and exactly one explicit threshold. Diagnostics use stderr.\n"
"  -select_stimuli LABEL / -select_labels LABEL / -select_columns INDEX\n"
"                    Must resolve to exactly ONE design column. Multi-basis or\n"
"                    IM stimulus families require an explicit column; no sum,\n"
"                    absolute magnitude, winner selection or HRF shift is added.\n"
"  -tr_above VALUE   Select retained TRs with ideal value strictly > VALUE.\n"
"  -tr_above_frac F  Select ideal > F * positive peak, with 0 <= F < 1.\n"
"                    Peak is within the selected run/window BEFORE censoring.\n"
"                    Censored matrices require a matching -uncensored reference\n"
"                    for fractional thresholds. Zero/negative-only peaks fail.\n"
"  -tr_window A..B   Optional zero-based, inclusive run-local TR window.\n"
"                    Use explicit hemodynamically aligned block windows; block\n"
"                    identities are not inferred from a condition regressor.\n"
"                    Without a window, all qualifying blocks in the run combine.\n"
"                    Output indices refer to the original full-length run, never\n"
"                    compressed censored rows. Censored TRs are always excluded.\n"
"                    Empty, ambiguous or invalid queries fail with empty stdout.\n"
"                    Cannot combine -show_trs with reports or file-writing modes.\n"
"                                                                               \n"
"Example (Bash; check selection before invoking 3dTstat):\n"
"  trs=$(1dXmatTool -infile X.xmat.1D -uncensored X.nocensor.xmat.1D \\\n"
"        -show_trs -runs 1 -select_stimuli face \\\n"
"        -tr_above_frac 0.5 -tr_window 8..15) || exit 1\n"
"  3dTstat -mean -prefix block01.nii.gz \"cleaned.r01.nii.gz[$trs]\"\n"
"\n"
"  -export_manifest  Write PREFIX.trials.1D from an IM design plus explicit events.\n"
"  -events FILE      Header/table: Run Volume Condition [Item]; complete unique\n"
"                    keys; Run one-based, Volume zero-based within each run.\n"
"  -subject ID       Subject label for the manifest.\n"
"  -beta FILE        Single-run beta bucket, or use -beta_pattern for many runs.\n"
"  -beta_pattern P   One {run} placeholder, replaced by 01,02,... (quote it).\n"
"  -manifest_format manifest|selected  Default manifest: whole InputFile plus\n"
"                    Volume for 3dMVPA -trial_manifest. selected: InputFile[n]\n"
"                    for direct 3dMVPA/3dRSA/3dSRM tables. Files need not yet exist.\n"
"                    Conditions are supplied by you, never guessed from IM labels.\n"
"Inputs/splitting:\n"
"  -infile FILE       NIML-comment matrix from 3dDeconvolve.\n"
"  -split_runs        Extract runs, preserving values and column order.\n"
"  -prefix PREFIX    Output prefix; its directory must already exist.\n"
"Optional:\n"
"  -uncensored FILE  Matching full-row design, recommended with censoring.\n"
"                    Determines column support before censoring. Dimensions,\n"
"                    labels, stimuli, run starts and retained values must match.\n"
"  -runs LIST        One-based run numbers, e.g. '1,3..5' (default: all).\n"
"  -zero_tol T       Relative support threshold, 0 <= T < 1 (default: 1e-6).\n"
"                    A column's peak within a run must exceed T times its\n"
"                    global peak. Use 0 for exact nonzero support.\n"
"  -help             This help.\n"
"\n"
"Outputs (r01 denotes the original, one-based run number):\n"
"  PREFIX.r01.xmat.1D    Run matrix with local GoodList, NRowFull, RunStart=0,\n"
"                       ColumnGroups and remapped stimulus/Basis metadata.\n"
"  PREFIX.r01.rows.1D   Row, SourceRow, RunTR, SourceTR (all zero-based).\n"
"  PREFIX.r01.columns.1D  Every input column, including dropped columns;\n"
"                       numeric columns only; labels in # ColumnLabel comments.\n"
"                       local Column/Stim/IMVolume are -1 when unavailable.\n"
"                       IMVolume is within the corresponding IM stimulus,\n"
"                       and is the LSS volume when there is one IM stimulus.\n"
"  PREFIX.runs.1D       Original run number, start TR, full length, retained\n"
"                       rows and columns; output paths are in # RunFiles comments.\n"
"Comment text escapes backslash, tab, newline and CR as \\\\, \\t, \\n and \\r.\n"
"All mappings refer to the immediate input file. Matrix headers also record\n"
"XmatSource* indices and the support file/tolerance. No files are overwritten.\n"
"All selected runs are checked before writing. Existing outputs cause failure.\n"
"\n"
"Policy:\n"
"* Requires explicit RunStart, NRowFull and GoodList. No timing is guessed.\n"
"* Drops columns inactive in the selected run; retains shared nuisance columns.\n"
"  Does not refit, normalize or demean. Values retain thd_xmat float precision;\n"
"  original double-precision text is not preserved beyond that precision.\n"
"* IM columns must belong to exactly one run. Zero/cross-run IM support, or\n"
"  loss of an IM column's support after censoring, is an error. Fully censored\n"
"  runs also fail. Without -uncensored, support is known only in retained rows.\n"
"* Rebuilds stimulus ranges; preserves column labels and per-stimulus Basis\n"
"  attributes. Drops GLTs and ColumnMasks (and records their removal).\n"
"* Extraction does not establish full rank or satisfy every downstream tool.\n"
"  3dLSS still requires exactly one IM stimulus and sufficient observations.\n"
"\n"
"Example downstream use (run 1, full-length per-run scaled EPI):\n"
"  3dLSS -matrix split/X.r01.xmat.1D \\\n"
"        -input pb03.SUBJ.r01.scale+orig -prefix LSS.r01\n"
"Use the matching run's FULL time series: 3dLSS applies the local GoodList.\n"
"Do not concatenate censored rows or feed the entire multi-run dataset.\n"
"This tool splits an existing design; it does not create new regressors.\n"
   );
}
/* Parse a comma/range list of unique one-based values in [1, limit].
 * Ascending and descending inclusive ranges preserve the requested order.
 * Unlike NI_decode_int_list, reject trailing junk and repeated values; this
 * prevents an accidental duplicate run from producing colliding output files.
 * The caller owns the returned NI_int_array; NULL signals invalid input/OOM.
 */
static NI_int_array *parse_runs(const char *p,int limit)
{
   NI_int_array *a=calloc(1,sizeof(*a));
   int *seen=calloc((size_t)limit,sizeof(int));
   if(!a || !seen){free(a);free(seen);return NULL;}
   a->ar=malloc(sizeof(int)*(size_t)limit);
   if(!a->ar){free(a);free(seen);return NULL;}
   while(*p){
      char *end; long first,last,v; int step;
      while(isspace((unsigned char)*p)) p++;
      errno=0; first=strtol(p,&end,10);
      if(errno || p==end || first<1 || first>limit) goto bad;
      p=end; last=first;
      if(p[0]=='.' && p[1]=='.'){
         p+=2; errno=0; last=strtol(p,&end,10);
         if(errno || p==end || last<1 || last>limit) goto bad;
         p=end;
      }
      while(isspace((unsigned char)*p)) p++;
      if(*p && *p!=',') goto bad;
      /* The bitset both rejects overlap between ranges and bounds the number
       * of emitted entries by limit, the allocated array capacity. */
      step=last>=first?1:-1;
      for(v=first;;v+=step){
         if(seen[v-1]) goto bad;
         seen[v-1]=1; a->ar[a->num++]=(int)v;
         if(v==last) break;
      }
      if(*p==','){p++;if(!*p) goto bad;}
   }
   free(seen);return a;
bad:
   free(seen);NI_delete_int_array(a);return NULL;
}
/* Allocate a run-specific output path. run is the original one-based number,
 * not the position of that run in a requested subset. Caller frees it. */
static char *outname(const char *prefix,int run,const char *suffix)
{
   size_t n=strlen(prefix)+strlen(suffix)+40; char *p=malloc(n);
   if(p) snprintf(p,n,"%s.r%02d.%s",prefix,run,suffix);
   return p;
}
/* Escape labels/paths embedded in comment records without introducing real
 * tabs or newlines. This is comment notation, not shell or JSON quoting. */
static void comment_text(FILE *f,const char *p)
{
   for(;*p;p++) switch(*p){
      case '\\':fputs("\\\\",f);break; case '\t':fputs("\\t",f);break;
      case '\n':fputs("\\n",f);break; case '\r':fputs("\\r",f);break;
      default:fputc(*p,f);
   }
}
/* Parsed operation settings. String pointers borrow argv storage; numeric
 * defaults are set in main. tr_threshold_set distinguishes an explicit zero
 * threshold from an omitted option. Selectors are shared by column extraction
 * and TR queries, but the latter must resolve to exactly one design column.
 */
typedef struct {
   int describe,validate,select,manifest,drop_glts;
   int show_trs,tr_fraction,tr_threshold_set;
   double tr_threshold;
   const char *tr_window;
   const char *compare,*json,*for_tool,*columns,*labels,*stimuli,*groups;
   const char *events,*subject,*beta,*beta_pattern,*manifest_format;
   double rank_tol,atol,rtol;
} XT_options;

/* Return the zero-based stimulus whose inclusive column range contains c;
 * -1 means that the column is outside every stimulus family. */
static int xt_stim(const THD_xmat *a,int c)
{
   int s;for(s=0;s<a->nstim;s++)if(c>=a->stim[s].bot && c<=a->stim[s].top)return s;return -1;
}
/* Test the recorded modeling option rather than guessing IM status from a
 * column label. Callers supply an existing stimulus index, or -1. */
static int xt_is_im(const THD_xmat *a,int s)
{return s>=0 && a->stim[s].option && !strcmp(a->stim[s].option,"-stim_times_IM");}
/* Create a new output stream with O_EXCL. This enforces no-overwrite even
 * if another process creates the path between a preflight check and open.
 * A failed fdopen removes only the file this call just created. */
static FILE *xt_open(const char *path)
{
   int fd=open(path,O_WRONLY|O_CREAT|O_EXCL,0666);FILE *f;
   if(fd<0){fprintf(stderr,"** cannot create new output %s: %s\n",path,strerror(errno));return NULL;}
   f=fdopen(fd,"w");if(!f){close(fd);unlink(path);}return f;
}
/* Flush/close an owned output and remove it on write failure so a truncated
 * report cannot be mistaken for a successful result. Return 1 on success. */
static int xt_close(FILE *f,const char *path)
{
   int bad=ferror(f);if(fclose(f))bad=1;
   if(bad){unlink(path);fprintf(stderr,"** failed writing %s\n",path);return 0;}return 1;
}
/* Write one JSON string, including quotes. Escape control bytes explicitly;
 * NULL is represented by the empty string, not JSON null. */
static void xt_json_text(FILE *f,const char *s)
{
   const unsigned char *p=(const unsigned char *)(s?s:"");fputc('"',f);
   for(;*p;p++){
      if(*p=='"' || *p=='\\')fprintf(f,"\\%c",*p);
      else if(*p<32)fprintf(f,"\\u%04x",*p);else fputc(*p,f);
   }fputc('"',f);
}
/* Cross-check redundant stimulus metadata after the shared reader parses it.
 * Range membership, positive ColumnGroups and BasisColumns must agree before
 * downstream operations assign scientific meaning to a column. This checks
 * consistency of attributes that exist, not completeness of all headers.
 * Return 0 with a diagnostic in err on inconsistency.
 */
static int xt_metadata(const THD_xmat *a,char *err,int nerr)
{
   const char *v=THD_xmat_attr(a,"Nstim");int s;
   if(v && atoi(v)!=a->nstim){snprintf(err,nerr,"Nstim does not match stimulus ranges");return 0;}
   for(s=0;s<a->nattr;s++){
      const char *k=a->attr_name[s],*p=strchr(k,'_');
      if(!strncasecmp(k,"Basis",5) && p && (atoi(p+1)<1 || atoi(p+1)>a->nstim)){
         snprintf(err,nerr,"Basis attribute refers to an unknown stimulus");return 0;
      }
   }
   v=THD_xmat_attr(a,"ColumnGroups");
   if(v){
      NI_int_array *g=NI_decode_int_list((char *)v,",");int valid=g && g->num==a->ncol;
      if(valid)for(s=0;s<a->ncol;s++){
         int st=xt_stim(a,s);
         /* Positive groups encode one-based stimulus IDs; nonpositive groups
          * describe baseline/nuisance roles and are not stimulus IDs. */
         if(g->ar[s]>0 && g->ar[s]!=st+1)valid=0;
      }
      if(g)NI_delete_int_array(g);
      if(!valid){snprintf(err,nerr,"ColumnGroups disagrees with columns/stimulus ranges");return 0;}
   }
   for(s=0;s<a->nstim;s++){
      int b,t,n=0;char k[64];snprintf(k,sizeof(k),"BasisColumns_%06d",s+1);v=THD_xmat_attr(a,k);
      /* %n lets us reject trailing characters after the two range endpoints. */
      if(v && (sscanf(v,"%d:%d%n",&b,&t,&n)!=2 || v[n] || b!=a->stim[s].bot || t!=a->stim[s].top)){
         snprintf(err,nerr,"%s does not match stimulus range",k);return 0;
      }
   }return 1;
}
/* Check the rank of every reduced LSS design for one extracted run.
 * Each trial model retains all non-IM columns and replaces the IM family with
 * two columns: the sum of all other trials and the singled-out trial.
 * This is an estimability preflight, not a fit to imaging data.
 * z borrows a's metadata; only z.mat and z.collab are newly owned, so do not
 * pass this shallow copy to THD_xmat_free.
 */
static int xt_lss(const THD_xmat *a,double tol,char *err,int nerr)
{
   THD_xmat z=*a;THD_xmat_diagnostic d;int s,im=-1,c,r,j,k,nt,nn,bad=0;
   for(s=0;s<a->nstim;s++)if(xt_is_im(a,s)){if(im>=0){snprintf(err,nerr,"LSS requires exactly one IM family");return 0;}im=s;}
   if(im<0){snprintf(err,nerr,"LSS requires an IM family");return 0;}
   nt=a->stim[im].top-a->stim[im].bot+1;nn=a->ncol-nt;
   if(nt<2 || a->nrow<a->ncol+2){snprintf(err,nerr,"LSS needs >=2 IM columns and nrow>=ncol+2");return 0;}
   z.ncol=nn+2;z.nstim=0;z.stim=NULL;z.mat=calloc((size_t)z.nrow*z.ncol,sizeof(float));
   z.collab=calloc((size_t)z.ncol,sizeof(char *));
   if(!z.mat || !z.collab){free(z.mat);free(z.collab);return 0;}
   for(c=0;c<z.ncol;c++)z.collab[c]="LSS";
   /* The unchanged baseline/nuisance block is copied once. Only the last two
    * columns need rebuilding as the singled-out trial moves through the run. */
   for(c=0,k=0;c<a->ncol;c++)if(c<a->stim[im].bot || c>a->stim[im].top)
      memcpy(z.mat+(size_t)k++*z.nrow,a->mat+(size_t)c*a->nrow,sizeof(float)*a->nrow);
   for(j=0;j<nt;j++){
      for(r=0;r<a->nrow;r++){
         double sum=0;for(c=0;c<nt;c++)if(c!=j)sum+=a->mat[(size_t)(a->stim[im].bot+c)*a->nrow+r];
         z.mat[(size_t)nn*z.nrow+r]=(float)sum;
         z.mat[(size_t)(nn+1)*z.nrow+r]=a->mat[(size_t)(a->stim[im].bot+j)*a->nrow+r];
      }
      if(!THD_xmat_diagnostics(&z,tol,&d,err,nerr) || d.rank<z.ncol)bad++;
   }
   free(z.mat);free(z.collab);
   if(bad)snprintf(err,nerr,"%d LSS trial model(s) rank deficient at requested tolerance",bad);
   return bad==0;
}
/* Describe full/run-local designs and optionally enforce LSS/REML checks.
 * Extraction uses the same support/censor rules as -split_runs, so diagnostics
 * describe the matrices a downstream command would actually receive.
 * Return 1 for setup/report I/O errors; invalid designs give status 2 only in
 * validation mode. Description mode still reports issues without failing.
 */
static int xt_report(const THD_xmat *a,const THD_xmat *u,const XT_options *o,double zero_tol)
{
   THD_xmat_diagnostic d;FILE *json=NULL;char err[1024]="";int run,s,ok,failed=0;
   if(!xt_metadata(a,err,sizeof(err)) || !THD_xmat_diagnostics(a,o->rank_tol,&d,err,sizeof(err))){fprintf(stderr,"** %s\n",err);return 1;}
   if(o->json){json=xt_open(o->json);if(!json)return 1;}
   printf("# 1dXmatTool %s; rank uses unit-norm columns, tolerance %.9g\n",o->validate?"validation":"description",o->rank_tol);
   printf("# Matrix: %s\n# FullModel Rows=%d Columns=%d FullRows=%d Runs=%d Stimuli=%d Rank=%d DF=%d Condition=%.9g ZeroColumns=%d DuplicatePairs=%d MaxAbsCosine=%.9g\n",
      a->fname,a->nrow,a->ncol,a->nfull,a->nrun,a->nstim,d.rank,d.residual_df,d.condition,d.zero_columns,d.duplicate_pairs,d.max_abs_cosine);
   for(s=0;s<a->nstim;s++)printf("# Stimulus %d %s columns=%d..%d option=%s\n",s,a->stim[s].label,a->stim[s].bot,a->stim[s].top,a->stim[s].option?a->stim[s].option:"none");
   if(!u && a->nrow<a->nfull)printf("# Support is inferred only from retained rows; supply -uncensored to detect support hidden by censoring.\n");
   /* LSS is judged by its reduced trial models below, not by requiring the
    * unreduced all-trial model to have full rank. REML also has a row minimum. */
   if(!o->for_tool || strcmp(o->for_tool,"lss"))if(d.rank<a->ncol || d.residual_df<1)failed=1;
   if(o->for_tool && !strcmp(o->for_tool,"reml") && a->nrow<9)failed=1;
   if(json){
      fprintf(json,"{\n  \"input\": ");xt_json_text(json,a->fname);
      fprintf(json,",\n  \"rows\": %d, \"columns\": %d, \"full_rows\": %d, \"rank\": %d, \"residual_df\": %d, \"zero_columns\": %d, \"duplicate_pairs\": %d, \"condition\": ",a->nrow,a->ncol,a->nfull,d.rank,d.residual_df,d.zero_columns,d.duplicate_pairs);
      /* JSON has no NaN/Infinity literals; null marks an undefined condition
       * number while the text report may display the floating-point value. */
      if(isfinite(d.condition))fprintf(json,"%.17g",d.condition);else fputs("null",json);
      fprintf(json,",\n  \"rank_tolerance\": %.17g, \"runs\": [\n",o->rank_tol);
   }
   puts("# Columns: Run Start Length NKept NColumns Rank ResidualDF Condition ZeroColumns DuplicatePairs Valid");
   for(run=0;run<a->nrun;run++){
      THD_xmat_run_map *map=NULL;THD_xmat *x=THD_xmat_extract_run(a,u,run,zero_tol,&map,err,sizeof(err));
      THD_xmat_diagnostic rd={0};int nr=0,nc=0;ok=x!=NULL;
      if(ok){nr=x->nrow;nc=x->ncol;ok=THD_xmat_diagnostics(x,o->rank_tol,&rd,err,sizeof(err));}
      if(ok && o->for_tool && !strcmp(o->for_tool,"lss"))ok=xt_lss(x,o->rank_tol,err,sizeof(err));
      else if(ok && (rd.rank<nc || rd.residual_df<1)){ok=0;snprintf(err,sizeof(err),"rank deficient or no residual degrees of freedom");}
      if(ok && o->for_tool && !strcmp(o->for_tool,"reml") && x->nrow<9){ok=0;snprintf(err,sizeof(err),"3dREMLfit requires at least 9 rows");}
      if(!ok){failed=1;printf("# RunIssue %d %s\n",run+1,err);}
      printf("%d %d %d %d %d %d %d %.9g %d %d %d\n",run+1,a->runstart[run],THD_xmat_run_length(a,run),nr,nc,x?rd.rank:-1,x?rd.residual_df:-1,x?rd.condition:NAN,rd.zero_columns,rd.duplicate_pairs,ok);
      if(json){
         fprintf(json,"    %s{\"run\": %d, \"start\": %d, \"length\": %d, \"retained\": %d, \"columns\": %d, \"rank\": %d, \"residual_df\": %d, \"valid\": %s, \"issue\": ",run?",":"",run+1,a->runstart[run],THD_xmat_run_length(a,run),nr,nc,x?rd.rank:-1,x?rd.residual_df:-1,ok?"true":"false");
         xt_json_text(json,ok?"":err);fputs("}\n",json);
      }
      THD_xmat_free(x);THD_xmat_run_map_free(map);
   }
   if(o->for_tool)printf("# %s preflight checks matrix structure/estimability; dataset, spatial and runtime checks remain with the fitting program.\n",o->for_tool);
   if(json){fprintf(json,"  ], \"valid\": %s\n}\n",failed?"false":"true");if(!xt_close(json,o->json))return 1;}
   if(o->validate)printf("# Validation %s\n",failed?"FAIL":"PASS");
   return o->validate && failed?2:0;
}
/* Compare matrix meaning independently of column ordering. Unique labels
 * identify columns, and GoodList identifies rows on the original timeline.
 * Values are compared only on shared TRs; censor/run/stimulus differences are
 * reported separately. Reordering alone is informational, not a difference.
 * Return 0 for equivalence, 2 for differences, or 1 for invalid inputs.
 */
static int xt_compare(const THD_xmat *a,const XT_options *o)
{
   THD_xmat *b;NI_int_array *ga=NULL,*gb=NULL;const char *v;
   char err[1024];int c,j,r,t,missing=0,reorder=0,changed=0,runs=1,stim=1,censor=1,labels=1,groups=1;double maximum=0;
   b=THD_xmat_read(o->compare,err,sizeof(err));if(!b){fprintf(stderr,"** %s\n",err);return 1;}
   if(!xt_metadata(a,err,sizeof(err)) || !xt_metadata(b,err,sizeof(err))){fprintf(stderr,"** %s\n",err);THD_xmat_free(b);return 1;}
   if(a->nfull!=b->nfull || a->tr!=b->tr || a->nrun!=b->nrun)runs=0;
   if(runs)for(r=0;r<a->nrun;r++)if(a->runstart[r]!=b->runstart[r])runs=0;
   if(a->nrow!=b->nrow)censor=0;
   if(censor)for(r=0;r<a->nrow;r++)if(a->good[r]!=b->good[r])censor=0;
   /* Unique labels are necessary for unambiguous identity matching. */
   for(c=0;c<a->ncol;c++)for(j=0;j<c;j++)if(!strcmp(a->collab[c],a->collab[j]))labels=0;
   for(c=0;c<b->ncol;c++)for(j=0;j<c;j++)if(!strcmp(b->collab[c],b->collab[j]))labels=0;
   if(!labels){fprintf(stderr,"** compare requires unique ColumnLabels in both matrices\n");THD_xmat_free(b);return 1;}
   v=THD_xmat_attr(a,"ColumnGroups");if(v)ga=NI_decode_int_list((char *)v,",");
   v=THD_xmat_attr(b,"ColumnGroups");if(v)gb=NI_decode_int_list((char *)v,",");
   if((ga==NULL)!=(gb==NULL))groups=0;
   puts("# Match columns by label and rows by full-timeline GoodList index.");
   puts("# Columns: Column OtherColumn CommonRows ChangedValues MaxAbsDifference");
   for(c=0;c<a->ncol;c++){
      int diff=0,common=0,sa,sb;double md=0;
      for(j=0;j<b->ncol;j++)if(!strcmp(a->collab[c],b->collab[j]))break;
      if(j==b->ncol){missing++;printf("%d -1 0 0 nan\n",c);continue;}
      if(c!=j)reorder=1;sa=xt_stim(a,c);sb=xt_stim(b,j);
      /* Baseline/nuisance group codes must match literally. Positive group
       * numbers can change with stimulus ordering; compare stimulus metadata
       * by identity below instead of comparing those numbers directly. */
      if(ga && gb && (ga->ar[c]<=0 || gb->ar[j]<=0) && ga->ar[c]!=gb->ar[j])groups=0;
      if((sa<0)!=(sb<0))stim=0;
      else if(sa>=0 && (strcmp(a->stim[sa].label,b->stim[sb].label) || strcmp(a->stim[sa].option?a->stim[sa].option:"",b->stim[sb].option?b->stim[sb].option:"") || strcmp(a->stim[sa].formula?a->stim[sa].formula:"",b->stim[sb].formula?b->stim[sb].formula:"")))stim=0;
      /* Merge the sorted GoodLists: different censor patterns must never pair
       * observations merely because they occupy the same compressed row. */
      for(r=0,t=0;r<a->nrow && t<b->nrow;){
         if(a->good[r]<b->good[t]){r++;continue;}if(a->good[r]>b->good[t]){t++;continue;}
         {double av=a->mat[(size_t)c*a->nrow+r],bv=b->mat[(size_t)j*b->nrow+t],d=fabs(av-bv);
          /* Symmetric relative tolerance avoids choosing either file as the
           * reference scale; nonfinite values always count as changed. */
          if(!isfinite(av) || !isfinite(bv) || d>o->atol+o->rtol*MAX(fabs(av),fabs(bv)))diff++;
          if(d>md)md=d;common++;}r++;t++;
      }
      changed+=diff;if(md>maximum)maximum=md;printf("%d %d %d %d %.17g\n",c,j,common,diff,md);
   }
   if(a->ncol!=b->ncol)missing++;
   printf("# SameColumns %d\n# ColumnOrderChanged %d\n# SameCensor %d\n# SameRunsAndTR %d\n# SameStimulusMetadata %d\n# ChangedValues %d\n# MaxAbsDifference %.17g\n",missing==0,reorder,censor,runs,stim,changed,maximum);
   printf("# SameColumnGroups %d\n",groups);
   if(ga)NI_delete_int_array(ga);if(gb)NI_delete_int_array(gb);
   THD_xmat_free(b);return missing || !censor || !runs || !stim || !groups || changed?2:0;
}
/* Union exact column/stimulus label matches into the caller's keep mask.
 * Each requested name must exist. A stimulus can mark several basis or IM
 * columns; callers decide whether multiple marked columns are acceptable.
 */
static int xt_mark_names(const THD_xmat *a,const char *list,int stimuli,unsigned char *keep)
{
   NI_str_array *v=NI_decode_string_list((char *)list,",");int i,c,s,found;
   if(!v || !v->num){if(v)NI_delete_str_array(v);return 0;}
   for(i=0;i<v->num;i++){
      found=0;for(c=0;c<a->ncol;c++){
         s=xt_stim(a,c);
         if((!stimuli && !strcmp(v->str[i],a->collab[c])) || (stimuli && s>=0 && !strcmp(v->str[i],a->stim[s].label))){keep[c]=1;found=1;}
      }
      if(!found){fprintf(stderr,"** unknown %s label: %s\n",stimuli?"stimulus":"column",v->str[i]);NI_delete_str_array(v);return 0;}
   }NI_delete_str_array(v);return 1;
}
/* Resolve one ideal-response column and return retained run-local TRs on
 * stdout. The optional full-row reference determines a pre-censor peak, but
 * only rows present in a can enter the final selector. The window applies to
 * both peak estimation and selection; it does not infer block identities.
 * Buffer the selection until all checks pass so an invalid/empty query cannot
 * leave a plausible partial selector in command substitution. Return 0/1.
 */
static int xt_show_trs(const THD_xmat *a,const THD_xmat *u,const XT_options *o,const char *runlist)
{
   NI_int_array *runs=NULL;unsigned char *keep=NULL;int c,col=-1,count=0,r,run,start,length,lo=0,hi,n=0;
   int *selected=NULL;double peak=0,threshold=o->tr_threshold;char err[1024]="";
   const THD_xmat *ref=u?u:a;
#define TR_FAIL(msg) do{snprintf(err,sizeof(err),"%s",msg);goto bad;}while(0)
   if(!THD_xmat_attr(a,"RunStart") || !THD_xmat_attr(a,"GoodList") || !THD_xmat_attr(a,"NRowFull"))
      TR_FAIL("-show_trs requires explicit RunStart, GoodList and NRowFull");
   if(!xt_metadata(a,err,sizeof(err)))goto bad;
   runs=runlist?parse_runs(runlist,a->nrun):NULL;
   if(!runs || runs->num!=1)TR_FAIL("-show_trs requires exactly one run via -runs N");
   /* Keep the run's original offset even when earlier TRs were censored.
    * lo/hi live in full-length run coordinates, not retained-row coordinates. */
   run=runs->ar[0]-1;start=a->runstart[run];length=THD_xmat_run_length(a,run);hi=length-1;
   if(o->tr_window){
      const char *p=o->tr_window;char *end;long first,last;
      errno=0;first=strtol(p,&end,10);
      if(errno || end==p || first<0 || first>=length || strncmp(end,"..",2))TR_FAIL("-tr_window must be START..END within the selected run (zero-based, inclusive)");
      p=end+2;errno=0;last=strtol(p,&end,10);
      if(errno || end==p || *end || last<first || last>=length)TR_FAIL("-tr_window must be START..END within the selected run (zero-based, inclusive)");
      lo=(int)first;hi=(int)last;
   }
   keep=calloc((size_t)a->ncol,1);selected=malloc(sizeof(int)*(size_t)a->nrow);
   if(!keep || !selected)TR_FAIL("cannot allocate TR selection");
   if(o->columns){
      char *end;long v;errno=0;v=strtol(o->columns,&end,10);
      if(errno || end==o->columns || *end || v<0 || v>=a->ncol)TR_FAIL("-show_trs -select_columns needs one zero-based column index");
      keep[v]=1;
   }
   if(o->labels && !xt_mark_names(a,o->labels,0,keep))goto bad;
   if(o->stimuli && !xt_mark_names(a,o->stimuli,1,keep))goto bad;
   for(c=0;c<a->ncol;c++)if(keep[c]){col=c;count++;}
   /* Do not silently add basis columns or choose among trials in an IM family:
    * those choices would change the meaning of the requested ideal response. */
   if(count!=1)TR_FAIL("-show_trs needs exactly one design column; use -select_labels or -select_columns for multi-column stimuli");
   /* Verify that u really is the full version of a, including every retained
    * value, before letting its otherwise unseen rows set the threshold. */
   if(u){
      if(!xt_metadata(u,err,sizeof(err)))goto bad;
      if(u->nfull!=a->nfull || u->nrow!=a->nfull || u->ncol!=a->ncol || u->nrun!=a->nrun || u->tr!=a->tr || u->nstim!=a->nstim)
         TR_FAIL("uncensored reference dimensions, runs, stimuli or TR do not match");
      for(r=0;r<u->nrow;r++)if(u->good[r]!=r)TR_FAIL("uncensored reference must contain every original TR");
      for(r=0;r<a->nrun;r++)if(u->runstart[r]!=a->runstart[r])TR_FAIL("uncensored reference RunStart does not match");
      for(r=0;r<a->nstim;r++)if(strcmp(a->stim[r].label,u->stim[r].label) || a->stim[r].bot!=u->stim[r].bot || a->stim[r].top!=u->stim[r].top)
         TR_FAIL("uncensored reference stimulus ranges/labels do not match");
      for(c=0;c<a->ncol;c++){
         if(strcmp(a->collab[c],u->collab[c]))TR_FAIL("uncensored reference column labels do not match");
         for(r=0;r<a->nrow;r++)if(a->mat[(size_t)c*a->nrow+r]!=u->mat[(size_t)c*u->nrow+a->good[r]])
            TR_FAIL("uncensored reference differs at a retained matrix value");
      }
   }
   /* A censored peak would lower the threshold and admit different TRs.
    * Requiring the full reference makes fractional selection reproducible. */
   if(o->tr_fraction && a->nrow<a->nfull && !u)
      TR_FAIL("fractional selection on a censored matrix requires -uncensored to define the pre-censor peak");
   /* Find the positive (not absolute) peak in the explicit run/window.
    * For absolute thresholds this pass still checks relevant values are finite. */
   for(r=0;r<ref->nrow;r++){
      int t=ref->good[r]-start;double v=ref->mat[(size_t)col*ref->nrow+r];
      if(t<lo || t>hi)continue;
      if(!isfinite(v))TR_FAIL("selected ideal contains nonfinite values");
      if(v>peak)peak=v;
   }
   if(o->tr_fraction){
      if(peak<=0)TR_FAIL("selected run/window has no positive ideal-response peak");
      threshold*=peak;
   }
   /* Walk only retained observations. Subtracting start converts GoodList's
    * concatenated-timeline TRs into selectors for the original per-run EPI.
    * Strict > intentionally excludes values exactly equal to the threshold. */
   for(r=0;r<a->nrow;r++){
      int t=a->good[r]-start;double v=a->mat[(size_t)col*a->nrow+r];
      if(t>=lo && t<=hi && v>threshold)selected[n++]=t;
   }
   if(!n)TR_FAIL("no retained TRs exceed the threshold in the selected run/window");
   for(r=0;r<n;r++)printf("%s%d",r?",":"",selected[r]);
   putchar('\n');free(keep);free(selected);NI_delete_int_array(runs);
   return ferror(stdout)?1:0;
bad:
   if(*err)fprintf(stderr,"** %s\n",err);
   free(keep);free(selected);if(runs)NI_delete_int_array(runs);return 1;
#undef TR_FAIL
}

/* Write a column subset plus provenance maps. All requested selection types
 * combine by union; the shared selector preserves original column order and
 * remaps dependent metadata. keep is a mask, not a requested output order.
 * Track ownership of the matrix output so cleanup never removes an existing
 * file when exclusive creation fails. Return 0 on success, 1 on failure.
 */
static int xt_select(const THD_xmat *a,const XT_options *o,const char *prefix)
{
   unsigned char *keep=calloc((size_t)a->ncol,1);THD_xmat *x=NULL;THD_xmat_run_map *m=NULL;
   char err[1024]="",*paths[3]={NULL,NULL,NULL};int c,i,ok=0,wrote=0;
   if(!keep || !prefix){fprintf(stderr,"** selection requires -prefix\n");free(keep);return 1;}
   if(!xt_metadata(a,err,sizeof(err)))goto done;
   if(o->columns){ /* parse_runs is strict and one-based; translate each token below. */
      char *p=strdup(o->columns),*q=p;NI_int_array *v;char *one=NULL;
      while(*q){char *end;long n;errno=0;n=strtol(q,&end,10);
         if(errno || end==q || n<0 || n>=a->ncol){free(p);snprintf(err,sizeof(err),"invalid zero-based -select_columns");goto done;}
         one=THD_zzprintf(one,"%ld",n+1);q=end;
         if(q[0]=='.' && q[1]=='.'){one=THD_zzprintf(one,"..");q+=2;}
         else if(*q==','){one=THD_zzprintf(one,",");q++;}
         else if(*q){free(p);free(one);snprintf(err,sizeof(err),"invalid -select_columns");goto done;}
      }
      v=one?parse_runs(one,a->ncol):NULL;free(one);free(p);
      if(!v || !v->num){if(v)NI_delete_int_array(v);snprintf(err,sizeof(err),"invalid -select_columns");goto done;}
      for(i=0;i<v->num;i++)keep[v->ar[i]-1]=1;NI_delete_int_array(v);
   }
   if(o->labels && !xt_mark_names(a,o->labels,0,keep))goto done;
   if(o->stimuli && !xt_mark_names(a,o->stimuli,1,keep))goto done;
   if(o->groups){
      const char *v=THD_xmat_attr(a,"ColumnGroups");NI_int_array *g=v?NI_decode_int_list((char *)v,","):NULL;
      NI_str_array *groups=NI_decode_string_list((char *)o->groups,",");
      if(!g || g->num!=a->ncol || !groups){if(g)NI_delete_int_array(g);if(groups)NI_delete_str_array(groups);snprintf(err,sizeof(err),"group selection needs valid ColumnGroups");goto done;}
      for(i=0;i<groups->num;i++){
         char *end;long n=strtol(groups->str[i],&end,10);int all=!strcmp(groups->str[i],"stimuli");
         if(!strcmp(groups->str[i],"baseline"))n=-1;
         else if(!strcmp(groups->str[i],"nuisance"))n=0;
         else if(!all && (end==groups->str[i] || *end)){snprintf(err,sizeof(err),"unknown group: %s",groups->str[i]);NI_delete_int_array(g);NI_delete_str_array(groups);goto done;}
         {int found=0;for(c=0;c<a->ncol;c++)if(all?g->ar[c]>0:g->ar[c]==n){keep[c]=1;found=1;}
          if(!found){snprintf(err,sizeof(err),"group selects no columns: %s",groups->str[i]);NI_delete_int_array(g);NI_delete_str_array(groups);goto done;}}
      }NI_delete_int_array(g);NI_delete_str_array(groups);
   }
   /* The shared routine also checks GLTs: dropping dependent contrast metadata
    * requires explicit consent through -drop_glts rather than silent rewriting. */
   x=THD_xmat_select_columns(a,keep,o->drop_glts,&m,err,sizeof(err));if(!x)goto done;
   paths[0]=THD_zzprintf(NULL,"%s.xmat.1D",prefix);paths[1]=THD_zzprintf(NULL,"%s.rows.1D",prefix);paths[2]=THD_zzprintf(NULL,"%s.columns.1D",prefix);
   if(!THD_xmat_write(x,paths[0],err,sizeof(err)))goto done;wrote=1;
   if(!THD_xmat_write_run_map(a,x,m,paths[1],paths[2],err,sizeof(err)))goto done;
   printf("# Selected %d/%d columns -> %s\n",x->ncol,a->ncol,paths[0]);ok=1;
done:
   if(!ok){if(wrote)unlink(paths[0]);fprintf(stderr,"** selection: %s\n",*err?err:"invalid selection");}
   for(i=0;i<3;i++)free(paths[i]);free(keep);THD_xmat_free(x);THD_xmat_run_map_free(m);return !ok;
}
/* Allocate a beta-bucket name without opening it (the bucket may be produced
 * later). Expand exactly one literal {run} into a padded one-based run number;
 * reject ambiguous/missing placeholders. Caller frees the returned string. */
static char *xt_beta_name(const XT_options *o,int run)
{
   const char *p=o->beta_pattern,*at;
   if(o->beta)return strdup(o->beta);
   if(!p || !(at=strstr(p,"{run}")) || strstr(at+5,"{run}"))return NULL;
   return THD_zzprintf(NULL,"%.*s%02d%s",(int)(at-p),p,run,at+5);
}
/* Parse an entire nonnegative integer field for event keys. Reject trailing
 * text and overflow instead of accepting atoi-style partial conversions. */
static int xt_integer(const char *s,int *n)
{
   char *e;long v;errno=0;v=strtol(s,&e,10);if(errno || e==s || *e || v<0 || v>INT_MAX)return 0;*n=(int)v;return 1;
}
/* Join extracted IM columns to explicit event labels using (Run, Volume).
 * Volume counts IM betas within each run; it is not a time-point index or a
 * column number in the full design. Require a bijection between event rows
 * and extracted trials so missing, duplicate or surplus labels cannot silently
 * shift trial identities. Emit either whole-bucket + Volume rows or selected
 * InputFile[volume] rows. Any failure removes the partially written manifest.
 */
static int xt_manifest(const THD_xmat *a,const THD_xmat *u,const XT_options *o,const char *prefix,double tol)
{
   THD_datatable *events=NULL;FILE *fp=NULL;int ri,vi,ci,ii,run,r,c,im=-1,ok=0,total=0;
   int *seen=NULL;char *path=NULL,err[1024]="";
   if(!prefix || !o->events || !o->subject || (!o->beta && !o->beta_pattern) || (o->beta && o->beta_pattern)){
      fprintf(stderr,"** export needs -prefix -events -subject and exactly one of -beta/-beta_pattern\n");return 1;
   }
   if(o->beta && a->nrun!=1){fprintf(stderr,"** -beta requires a single-run matrix; use -beta_pattern for per-run buckets\n");return 1;}
   for(r=0;r<a->nstim;r++)if(xt_is_im(a,r)){if(im>=0){fprintf(stderr,"** export requires exactly one IM family\n");return 1;}im=r;}
   if(im<0){fprintf(stderr,"** export requires an IM family\n");return 1;}
   events=THD_read_datatable_file((char *)o->events);
   ri=THD_datatable_column(events,"Run");vi=THD_datatable_column(events,"Volume");ci=THD_datatable_column(events,"Condition");ii=THD_datatable_column(events,"Item");
   if(ri<0 || vi<0 || ci<0){snprintf(err,sizeof(err),"events need Run Volume Condition (optional Item)");goto done;}
   seen=calloc((size_t)events->nrow,sizeof(int));
   /* Validate event keys before producing any output. */
   for(r=0;r<events->nrow;r++){
      int rr,vv,j,rr2,vv2;
      if(!xt_integer(DT_CELL(events,r,ri),&rr) || rr<1 || rr>a->nrun || !xt_integer(DT_CELL(events,r,vi),&vv) || !*DT_CELL(events,r,ci)){
         snprintf(err,sizeof(err),"invalid events Run/Volume/Condition on row %d",r+1);goto done;}
      for(j=0;j<r;j++){xt_integer(DT_CELL(events,j,ri),&rr2);xt_integer(DT_CELL(events,j,vi),&vv2);
         if(rr==rr2 && vv==vv2){snprintf(err,sizeof(err),"duplicate Run/Volume in events");goto done;}}
   }
   path=THD_zzprintf(NULL,"%s.trials.1D",prefix);fp=xt_open(path);if(!fp)goto done;
   fprintf(fp,"# 1dXmatTool trial manifest; Run is one-based, Volume/XColumn are zero-based.\n");
   fprintf(fp,"# Columns: Sample Subj Run Item Condition Volume %s InputFile\n",
      o->manifest_format && !strcmp(o->manifest_format,"selected")?"SourceXmat SourceColumn":"Xmat XColumn");
   for(run=0;run<a->nrun;run++){
      THD_xmat_run_map *m=NULL;THD_xmat *x=THD_xmat_extract_run(a,u,run,tol,&m,err,sizeof(err));char *beta=xt_beta_name(o,run+1);int local=0;
      if(!x || !beta){THD_xmat_free(x);THD_xmat_run_map_free(m);free(beta);if(!*err)snprintf(err,sizeof(err),"beta pattern must contain exactly one {run}");goto done;}
      /* Map extracted columns back to the source IM range. local advances
       * only for IM columns, skipping nuisance/baseline columns in x. */
      for(c=0;c<x->ncol;c++)if(m->columns[c]>=a->stim[im].bot && m->columns[c]<=a->stim[im].top){
         for(r=0;r<events->nrow;r++){int rr,vv;xt_integer(DT_CELL(events,r,ri),&rr);xt_integer(DT_CELL(events,r,vi),&vv);if(rr==run+1 && vv==local)break;}
         if(r==events->nrow){snprintf(err,sizeof(err),"missing event for run %d volume %d",run+1,local);THD_xmat_free(x);THD_xmat_run_map_free(m);free(beta);goto done;}
         seen[r]=1;total++;
         {char *sample=THD_zzprintf(NULL,"%s_r%02d_v%04d",o->subject,run+1,local),*sel=NULL;
          THD_1D_print_token(fp,sample);fputc(' ',fp);THD_1D_print_token(fp,o->subject);
          fprintf(fp," %d ",run+1);THD_1D_print_token(fp,ii>=0?DT_CELL(events,r,ii):a->collab[m->columns[c]]);
          fputc(' ',fp);THD_1D_print_token(fp,DT_CELL(events,r,ci));fprintf(fp," %d ",local);THD_1D_print_token(fp,a->fname);
          /* Record the ORIGINAL matrix column beside the run-local beta index;
           * these two indices deliberately refer to different coordinate systems. */
          fprintf(fp," %d ",m->columns[c]);
          if(o->manifest_format && !strcmp(o->manifest_format,"selected"))sel=THD_zzprintf(NULL,"%s[%d]",beta,local);
          THD_1D_print_token(fp,sel?sel:beta);fputc('\n',fp);free(sel);free(sample);}
         local++;
      }
      THD_xmat_free(x);THD_xmat_run_map_free(m);free(beta);
   }
   /* Having found every design trial is insufficient: extra event rows would
    * otherwise leave incorrect or stale metadata unnoticed. */
   for(r=0;r<events->nrow;r++)if(!seen[r]){snprintf(err,sizeof(err),"events contain unmatched rows");goto done;}
   ok=xt_close(fp,path);fp=NULL;
   if(ok)printf("# Exported %d trials -> %s\n",total,path);
done:
   if(fp){fclose(fp);unlink(path);}if(!ok)fprintf(stderr,"** manifest: %s\n",*err?err:"cannot write output");
   free(path);free(seen);THD_free_datatable(events);return !ok;
}

/* Parse and validate one operation, then dispatch to its implementation.
 * Run splitting stays here because it stages a multi-file output set: prepare
 * every run, check all paths, write, then roll back owned outputs on failure.
 */
int main(int argc,char **argv)
{
   XT_options ops={0};
   const char *in=NULL,*unc=NULL,*prefix=NULL,*runlist=NULL;
   THD_xmat *a=NULL,*u=NULL,**x=NULL; THD_xmat_run_map **maps=NULL;
   NI_int_array *runs=NULL; char **paths=NULL,*summary=NULL,err[1024]="";
   double tol=THD_XMAT_SPLIT_TOL; int split=0,i,j,n=0,done=0,rc=1,fd=-1,summary_owned=0;
   FILE *fp=NULL; struct stat st;
   ops.rank_tol=1.e-7;ops.atol=1.e-6;ops.rtol=1.e-6;
   if(argc==1){usage();return 0;}
   /* Handle argument-free switches first; all remaining branches consume a
    * value. Options store borrowed argv pointers for the lifetime of main. */
   for(i=1;i<argc;i++){
      if(!strcmp(argv[i],"-help") || !strcmp(argv[i],"--help")){usage();return 0;}
      if(!strcmp(argv[i],"-describe") || !strcmp(argv[i],"-show")){ops.describe=1;continue;}
      if(!strcmp(argv[i],"-show_trs")){ops.show_trs=1;continue;}
      if(!strcmp(argv[i],"-validate")){ops.validate=1;continue;}
      if(!strcmp(argv[i],"-export_manifest")){ops.manifest=1;continue;}
      if(!strcmp(argv[i],"-drop_glts")){ops.drop_glts=1;continue;}
      if(!strcmp(argv[i],"-split_runs")){split=1;continue;}
      if(i+1>=argc){fprintf(stderr,"** missing argument after %s\n",argv[i]);return 1;}
      if(!strcmp(argv[i],"-infile")) in=argv[++i];
      else if(!strcmp(argv[i],"-uncensored")) unc=argv[++i];
      else if(!strcmp(argv[i],"-prefix")) prefix=argv[++i];
      else if(!strcmp(argv[i],"-runs")) runlist=argv[++i];
      else if(!strcmp(argv[i],"-tr_window")) ops.tr_window=argv[++i];
      else if(!strcmp(argv[i],"-tr_above") || !strcmp(argv[i],"-tr_above_frac")){
         const char *opt=argv[i],*value=argv[++i];char *end;double d;
         if(ops.tr_threshold_set){fprintf(stderr,"** choose exactly one TR threshold option\n");return 1;}
         ops.tr_fraction=!strcmp(opt,"-tr_above_frac");errno=0;d=strtod(value,&end);
         if(errno || end==value || *end || !isfinite(d) || (ops.tr_fraction && (d<0 || d>=1))){fprintf(stderr,"** invalid TR threshold; fraction must be in [0,1)\n");return 1;}
         ops.tr_threshold=d;ops.tr_threshold_set=1;
      }
      else if(!strcmp(argv[i],"-compare")) ops.compare=argv[++i];
      else if(!strcmp(argv[i],"-json")) ops.json=argv[++i];
      else if(!strcmp(argv[i],"-for")) ops.for_tool=argv[++i];
      else if(!strcmp(argv[i],"-select_columns")) ops.columns=argv[++i];
      else if(!strcmp(argv[i],"-select_labels")) ops.labels=argv[++i];
      else if(!strcmp(argv[i],"-select_stimuli")) ops.stimuli=argv[++i];
      else if(!strcmp(argv[i],"-select_groups")) ops.groups=argv[++i];
      else if(!strcmp(argv[i],"-events")) ops.events=argv[++i];
      else if(!strcmp(argv[i],"-subject")) ops.subject=argv[++i];
      else if(!strcmp(argv[i],"-beta")) ops.beta=argv[++i];
      else if(!strcmp(argv[i],"-beta_pattern")) ops.beta_pattern=argv[++i];
      else if(!strcmp(argv[i],"-manifest_format")) ops.manifest_format=argv[++i];
      else if(!strcmp(argv[i],"-rank_tol") || !strcmp(argv[i],"-atol") || !strcmp(argv[i],"-rtol")){
         const char *opt=argv[i],*v=argv[++i];char *end;double d;errno=0;d=strtod(v,&end);
         if(errno || end==v || *end || !isfinite(d) || d<0 || (!strcmp(opt,"-rank_tol") && (d<=0 || d>=1))){fprintf(stderr,"** invalid tolerance\n");return 1;}
         if(!strcmp(opt,"-rank_tol"))ops.rank_tol=d;else if(!strcmp(opt,"-atol"))ops.atol=d;else ops.rtol=d;
      }
      else if(!strcmp(argv[i],"-zero_tol")){
         char *end; const char *v=argv[++i]; errno=0; tol=strtod(v,&end);
         if(errno || end==v || *end || !isfinite(tol) || tol<0 || tol>=1){
            fprintf(stderr,"** -zero_tol must be a number in [0,1)\n");return 1;
         }
      }else{fprintf(stderr,"** unknown option: %s\n",argv[i]);return 1;}
   }
#define XT_FAIL(MSG) do{snprintf(err,sizeof(err),"%s",MSG);goto cleanup;}while(0)
   /* The same -select_* arguments identify the ideal in a TR query, rather
    * than requesting a second, file-writing column-selection operation. */
   ops.select=!ops.show_trs && (ops.columns || ops.labels || ops.stimuli || ops.groups);
   if(ops.show_trs && (!ops.tr_threshold_set || ops.groups || prefix || ( (ops.columns!=NULL)+(ops.labels!=NULL)+(ops.stimuli!=NULL)!=1 )))
      XT_FAIL("-show_trs requires one threshold and one column/stimulus selection; no -prefix or -select_groups");
   if(!ops.show_trs && (ops.tr_threshold_set || ops.tr_window))XT_FAIL("TR threshold/window options require -show_trs");
   if(!in) XT_FAIL("requires -infile (see -help)");
   if(ops.show_trs+split+ops.select+ops.manifest+(ops.compare!=NULL)+((ops.describe||ops.validate)!=0)!=1)
      XT_FAIL("choose one operation: split, describe/validate, select, compare, export_manifest, or show_trs");
   if(ops.for_tool && strcmp(ops.for_tool,"lss") && strcmp(ops.for_tool,"reml"))XT_FAIL("-for must be lss or reml");
   if(ops.manifest_format && strcmp(ops.manifest_format,"selected") && strcmp(ops.manifest_format,"manifest"))XT_FAIL("-manifest_format must be manifest or selected");
   if(ops.json && !(ops.describe||ops.validate))XT_FAIL("-json is available with describe/validate");
   if(runlist && !split && !ops.show_trs)XT_FAIL("-runs applies to -split_runs or -show_trs");
   if(prefix && !*prefix)XT_FAIL("-prefix cannot be empty");
   if(ops.for_tool && !(ops.describe||ops.validate))XT_FAIL("-for applies to describe/validate");
   if(ops.drop_glts && !ops.select)XT_FAIL("-drop_glts applies to column selection");
   if((ops.events || ops.subject || ops.beta || ops.beta_pattern || ops.manifest_format) && !ops.manifest)
      XT_FAIL("event/beta options apply to -export_manifest");
   if(unc && (ops.select || ops.compare))XT_FAIL("-uncensored applies to split, describe/validate and export_manifest");
   /* Read each matrix once. Operation-specific compatibility checks occur
    * after dispatch; the shared reader owns all parsed matrix/header storage. */
   a=THD_xmat_read(in,err,sizeof(err)); if(!a) goto cleanup;
   if(unc){u=THD_xmat_read(unc,err,sizeof(err));if(!u) goto cleanup;}
   if(ops.show_trs)rc=xt_show_trs(a,u,&ops,runlist);
   else if(ops.describe||ops.validate)rc=xt_report(a,u,&ops,tol);
   else if(ops.compare)rc=xt_compare(a,&ops);
   else if(ops.select)rc=xt_select(a,&ops,prefix);
   else if(ops.manifest)rc=xt_manifest(a,u,&ops,prefix,tol);
   if(!split){THD_xmat_free(a);THD_xmat_free(u);return rc;}
   /* Run-splitting phase 1: build all extracted designs and provenance paths
    * before creating files, so an invalid later run leaves no earlier outputs. */
   if(!prefix || !*prefix)XT_FAIL("split requires -prefix");
   if(runlist){
      runs=parse_runs(runlist,a->nrun);
      if(!runs || runs->num<1) XT_FAIL("invalid -runs list: use unique run numbers in range, e.g. 1,3..5");
   }else{
      runs=calloc(1,sizeof(*runs)); runs->num=a->nrun;
      runs->ar=malloc(sizeof(int)*(size_t)a->nrun);
      for(i=0;i<a->nrun;i++) runs->ar[i]=i+1;
   }
   n=runs->num;
   x=calloc((size_t)n,sizeof(*x)); maps=calloc((size_t)n,sizeof(*maps));
   paths=calloc((size_t)n*3,sizeof(*paths)); summary=malloc(strlen(prefix)+16);
   if(!x || !maps || !paths || !summary) XT_FAIL("out of memory");
   sprintf(summary,"%s.runs.1D",prefix);
   if(!u && a->nrow<a->nfull)
      fprintf(stderr,"++ Support inferred from retained rows only; use -uncensored for pre-censor support.\n");
   for(i=0;i<n;i++){
      if(runs->ar[i]<1 || runs->ar[i]>a->nrun) XT_FAIL("-runs index outside the input run range");
      for(j=0;j<i;j++) if(runs->ar[i]==runs->ar[j]) XT_FAIL("duplicate run in -runs");
      x[i]=THD_xmat_extract_run(a,u,runs->ar[i]-1,tol,maps+i,err,sizeof(err));
      if(!x[i]) goto cleanup;
      paths[3*i]=outname(prefix,runs->ar[i],"xmat.1D");
      paths[3*i+1]=outname(prefix,runs->ar[i],"rows.1D");
      paths[3*i+2]=outname(prefix,runs->ar[i],"columns.1D");
      if(!paths[3*i] || !paths[3*i+1] || !paths[3*i+2]) XT_FAIL("out of memory");
   }
   /* Preflight includes the summary and every run's matrix/row/column files.
    * lstat also catches dangling symlinks; exclusive writers remain the final
    * protection against races after this check. */
   for(i=0;i<=3*n;i++){
      const char *p=i==3*n?summary:paths[i];
      if(!lstat(p,&st)){snprintf(err,sizeof(err),"output already exists: %s",p);goto cleanup;}
      if(errno!=ENOENT){snprintf(err,sizeof(err),"cannot access output path: %s",p);goto cleanup;}
   }
   /* Phase 2: create the summary exclusively, then publish each run's files.
    * paths has three entries per run; done counts successfully owned entries
    * for rollback. The shared map writer cleans up its own partial pair. */
   fd=open(summary,O_WRONLY|O_CREAT|O_EXCL,0666);
   if(fd<0) XT_FAIL("cannot create summary (prefix directory must exist)");
   summary_owned=1; fp=fdopen(fd,"w");
   if(!fp){close(fd);XT_FAIL("cannot open summary stream");}
   fputs("# Columns: Run Start Length NKept NColumns\n",fp);
   for(i=0;i<n;i++){
      if(!THD_xmat_write(x[i],paths[3*i],err,sizeof(err))) goto cleanup;
      done=3*i+1;
      if(!THD_xmat_write_run_map(a,x[i],maps[i],paths[3*i+1],paths[3*i+2],err,sizeof(err))) goto cleanup;
      done=3*i+3;
      fprintf(fp,"# RunFiles %d ",runs->ar[i]);
      comment_text(fp,paths[3*i]); fputc(' ',fp);comment_text(fp,paths[3*i+1]);fputc(' ',fp);comment_text(fp,paths[3*i+2]);fputc('\n',fp);
      fprintf(fp,"%d %d %d %d %d\n",runs->ar[i],maps[i]->start,x[i]->nfull,x[i]->nrow,x[i]->ncol);
   }
   j=ferror(fp); if(fclose(fp)) j=1; fp=NULL;
   if(j) XT_FAIL("failed writing summary");
   for(i=0;i<n;i++) fprintf(stderr,"++ run %d: %d/%d rows, %d/%d columns -> %s\n",
      runs->ar[i],x[i]->nrow,x[i]->nfull,x[i]->ncol,a->ncol,paths[3*i]);
   fprintf(stderr,"++ Wrote %d run(s); provenance: %s\n",n,summary); rc=0;
/* Remove only files created by this invocation. In particular, a rejected
 * pre-existing summary is never owned and must survive failure cleanup. */
cleanup:
   if(fp) fclose(fp);
   if(rc){
      fprintf(stderr,"** 1dXmatTool: %s\n",err);
      for(i=0;i<done;i++) unlink(paths[i]);
      if(summary_owned) unlink(summary);
   }
   for(i=0;i<n;i++){
      if(x) THD_xmat_free(x[i]); if(maps) THD_xmat_run_map_free(maps[i]);
   }
   if(paths){for(i=0;i<3*n;i++) free(paths[i]);free(paths);}
   free(x);free(maps);free(summary);
   if(runs) NI_delete_int_array(runs); THD_xmat_free(a);THD_xmat_free(u);
   return rc;
#undef XT_FAIL
}
