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
#include <stdarg.h>

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
"  -force_inferred_metadata  With describe/validate only: allow support reports\n"
"                    using reader defaults for missing RunStart, GoodList or\n"
"                    NRowFull. Warns and labels diagnostics as inferred. Does\n"
"                    not repair metadata or bypass extraction/validation errors.\n"
"  -warn_support_frac F  With describe/validate, warn when the retained IM\n"
"                    support-row fraction is below F (0..1). Opt-in only;\n"
"                    never changes validity or exit status. Needs full support.\n"
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
"\n"
"Beta-sidecar input (Picard or an existing LSS trial manifest):\n"
"  -beta_manifest FILE  Alternative to -infile; use with -export_manifest.\n"
"  -subject ID -prefix PREFIX  Required. Uses actual beta Volume, not XColumn.\n"
"  -manifest_run_base 0|1  Input Run convention; default 0 for Picard.\n"
"                         Use 1 for a previous 1dXmatTool LSS manifest.\n"
"  -input_base DIR     Base for relative paths; default: manifest directory.\n"
"                     For Picard prefixes with directories, pass its original cwd.\n"
"  -events FILE        Optional annotations: Volume Condition [Run Trial Item Basis].\n"
"                     Volume is the actual whole-bucket beta index; Run is ONE-based.\n"
"                     Complete unique Volume keys required; single bucket only.\n"
"                     Annotations fill missing identities; contradictions fail.\n"
"  -runs LIST          Select ONE-based runs, e.g. 1 or 1,3..5. No renumbering.\n"
"  -manifest_format manifest|selected|rsa|rsa_runwise\n"
"      manifest: PREFIX.trials.1D, whole InputFile + Volume for 3dMVPA.\n"
"      selected: PREFIX.trials.1D, InputFile[Volume] for direct tables.\n"
"      rsa: PREFIX.rsa.1D, Subj Condition InputFile; one beta per condition.\n"
"      rsa_runwise: PREFIX.runwise.1D and PREFIX.rNN.trials.1D Trial/Condition\n"
"                   files for 3dRSA -runwiseTable. Requires >=2 runs, explicit\n"
"                   unique Trial IDs, one coefficient/trial and one bucket/run.\n"
"                   Repeated conditions are averaged within runs by RSA, not here.\n"
"      Both RSA formats also write PREFIX.identities.1D with full provenance.\n"
"  Exported Run is ONE-based; SourceRun retains the input value (-1 if absent).\n"
"  Missing Run/Trial/Basis remain -1; conditions are never inferred from labels.\n"
"  CandidateManifest and DesignIDFile/Volume are retained, without a fictitious\n"
"  single Xmat. Dataset headers/volume bounds and fixed XColumn bounds are checked.\n"
"  Runs must be <=1000000. Paths are made absolute; outputs never overwrite files.\n"
"  No beta images, residuals, fits, HRF choices or averages are computed here.\n"
"  Runwise export does not establish independent runs or prevent selection leakage.\n"
"  It exports unwhitened crossnobis inputs; ResidFile/whitening is not inferred.\n"
"\n"
"Example after 3dPicard -write_manifest (from Picard's working directory):\n"
"  1dXmatTool -beta_manifest fitted.betas.tsv -export_manifest \\\n"
"    -subject s01 -input_base . -manifest_format manifest -prefix mvpa\n"
"  3dMVPA -mode classify -trial_manifest mvpa.trials.1D \\\n"
"    -target_column Condition -cv_column Run -mask mask.nii -prefix decoded\n"
"  1dXmatTool -beta_manifest fitted.betas.tsv -export_manifest \\\n"
"    -subject s01 -input_base . -manifest_format rsa_runwise -prefix rsa\n"
"  Add -events annotations.1D when a fixed-design sidecar lacks identities.\n"
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
"Dangers and diagnostics:\n"
"* Use -uncensored X.nocensor.xmat.1D for censored designs, including\n"
"  -validate -for lss and -validate -for reml. Quote paths containing spaces\n"
"  or parentheses. Without a reference, pre-censor fractions are unknown.\n"
"* Fully censored IM: error 'loses its support after censoring'. Without a\n"
"  reference: 'trial may be fully censored'. Zero in the full design: check\n"
"  onsets/basis. Cross-run support is a separate ownership error.\n"
"* Partial loss: -describe -warn_support_frac 0.5 can warn 'support fraction\n"
"  0.25, peak fraction 0.015'. This is a heuristic, not a power/quality test.\n"
"  Support rows have abs(value)>zero_tol*full_peak; peak fraction uses abs\n"
"  peaks. Zero full peak gives unknown ratios. No default warning threshold.\n"
"* IMCounts reports assigned IM columns per run (trial counts for one-column\n"
"  IM bases). Original counts can differ legitimately; censoring losses warn,\n"
"  e.g. 'run 4: 11 of 12 assigned IM columns retained after censoring'.\n"
"* Event mismatch: 'missing event for run R volume V' or 'events contain\n"
"  unmatched rows'. Check run/volume indexing and event/design correspondence;\n"
"  censoring alone does not establish the cause. Lost IMs fail extraction.\n"
"* Failed extraction preserves NKept; unavailable derived diagnostics use -1\n"
"  (condition: nan). Unknown support ratios use unknown / JSON null.\n"
"  Missing timeline metadata suppresses support/count diagnostics and input\n"
"  run rows by default (JSON null support/counts; empty runs). Explicit force\n"
"  permits inferred reports only: one run, consecutive rows, stored row count\n"
"  are the respective defaults. These assumptions cannot recover censoring.\n"
"  -describe warns but exits 0; -validate exits 2 for failed checks.\n"
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
"  attributes. Input stimulus IDs may be sparse; output IDs are compact.\n"
"  Drops GLTs and ColumnMasks (and records their removal).\n"
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
/* Fill caller-owned arrays with unique one-based runs; reject trailing garbage. */
static int parse_run_values(const char *p,int limit,NI_int_array *a,int *seen)
{
   while(*p){
      char *end; long first,last,v; int step;
      while(isspace((unsigned char)*p)) p++;
      errno=0; first=strtol(p,&end,10);
      if(errno || p==end || first<1 || first>limit) return 0;
      p=end; last=first;
      if(p[0]=='.' && p[1]=='.'){
         p+=2; errno=0; last=strtol(p,&end,10);
         if(errno || p==end || last<1 || last>limit) return 0;
         p=end;
      }
      while(isspace((unsigned char)*p)) p++;
      if(*p && *p!=',') return 0;
      /* The bitset both rejects overlap between ranges and bounds the number
       * of emitted entries by limit, the allocated array capacity. */
      step=last>=first?1:-1;
      for(v=first;;v+=step){
         if(seen[v-1]) return 0;
         seen[v-1]=1; a->ar[a->num++]=(int)v;
         if(v==last) break;
      }
      if(*p==','){p++;if(!*p) return 0;}
   }
   return 1;
}

/* Parse unique one-based values in [1, limit], preserving ascending or
   descending range order. Reject trailing junk and overlapping selections.
   Caller owns the returned array; NULL signals invalid input or allocation failure. */
static NI_int_array *parse_runs(const char *p,int limit)
{
   NI_int_array *a=calloc(1,sizeof(*a));
   int *seen=calloc((size_t)limit,sizeof(int)),ok=0;
   if(a && seen){
      a->ar=malloc(sizeof(int)*(size_t)limit);
      if(a->ar) ok=parse_run_values(p,limit,a,seen);
   }
   free(seen);
   if(!ok){if(a)NI_delete_int_array(a);return NULL;}
   return a;
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
   double warn_support_frac;
   int warn_support_set,force_inferred_metadata;
   const char *tr_window;
   const char *compare,*json,*for_tool,*columns,*labels,*stimuli,*groups;
   const char *events,*subject,*beta,*beta_pattern,*manifest_format;
   const char *beta_manifest,*input_base;
   int manifest_run_base,manifest_run_base_set;
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
/* Keep CLI inspection and library operations on the same metadata rules. */
static int xt_metadata(const THD_xmat *a,char *err,int nerr)
{
   return THD_xmat_validate_metadata(a,err,nerr);
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
typedef struct {
   int full,kept,owner;
   double peak,kept_peak;
} XT_support;

/* Summarize one IM column against ref using full-timeline row indices.
 * owner=-1 denotes zero or cross-run support. a/ref are borrowed matrices. */
static XT_support xt_column_support(const THD_xmat *a,const THD_xmat *ref,
                                    int c,double tol)
{
   XT_support out={0,0,-1,0,0};int r,mixed=0;
   for(r=0;r<ref->nrow;r++)out.peak=MAX(out.peak,fabs(ref->mat[(size_t)c*ref->nrow+r]));
   for(r=0;r<ref->nrow;r++)if(fabs(ref->mat[(size_t)c*ref->nrow+r])>tol*out.peak){
      int run=THD_xmat_run_of(ref,ref->good[r]);
      out.full++;
      if(out.owner<0)out.owner=run;else if(out.owner!=run)mixed=1;
   }
   for(r=0;r<a->nrow;r++){
      double z=fabs(a->mat[(size_t)c*a->nrow+r]);
      out.kept_peak=MAX(out.kept_peak,z);if(z>tol*out.peak)out.kept++;
   }
   if(mixed)out.owner=-1;
   return out;
}

/* Reader defaults are useful for ordinary matrices but cannot establish the
 * original run/censor timeline. Diagnostics require all three explicit fields. */
static int xt_explicit_timeline(const THD_xmat *a)
{
   return THD_xmat_attr(a,"RunStart") && THD_xmat_attr(a,"GoodList") &&
          THD_xmat_attr(a,"NRowFull");
}

/* Support is |value| > zero_tol * full-column peak. Counts describe sampled
 * design support, not statistical power. Without a valid full reference,
 * pre-censor fractions and original counts are unknown. No matrices mutate. */
static void xt_support_report(const THD_xmat *a,const THD_xmat *u,
                             const XT_options *o,double tol,FILE *json)
{
   const THD_xmat *ref=u?u:a;
   int known=u || a->nrow==a->nfull,c,run,first=1,previous=-1,unequal=0;
   if(json)fputs("  \"im_support\": [",json);
   for(c=0;c<a->ncol;c++)if(xt_is_im(a,xt_stim(a,c))){
      XT_support support=xt_column_support(a,ref,c,tol);
      int full=support.full,kept=support.kept,owner=support.owner;
      double fraction=0,ratio=0;
      if(known && full){fraction=(double)kept/full;ratio=support.kept_peak/support.peak;}
      printf("# IMSupport Column=%d Run=%d RetainedSupportRows=%d FullSupportRows=%d SupportFraction=",
             c,owner<0?-1:owner+1,kept,known?full:-1);
      if(known && full)printf("%.9g PeakFraction=%.9g\n",fraction,ratio);
      else puts("unknown PeakFraction=unknown");
      if(o->warn_support_set && known && full && fraction<o->warn_support_frac)
         fprintf(stderr,"# WARNING: IM column %d (%s), run %d: support fraction %.6g, peak fraction %.6g (support threshold %.6g).\n",
                 c,a->collab[c],owner<0?-1:owner+1,fraction,ratio,o->warn_support_frac);
      if(json){
         fprintf(json,"%s{\"column\": %d, \"label\": ",first?"":",",c);xt_json_text(json,a->collab[c]);
         fputs(", \"run\": ",json);if(owner<0)fputs("null",json);else fprintf(json,"%d",owner+1);
         fprintf(json,", \"retained_support_rows\": %d, \"full_support_rows\": ",kept);
         if(known)fprintf(json,"%d",full);else fputs("null",json);
         fputs(", \"support_fraction\": ",json);if(known && full)fprintf(json,"%.17g",fraction);else fputs("null",json);
         fputs(", \"peak_fraction\": ",json);if(known && full)fprintf(json,"%.17g",ratio);else fputs("null",json);
         fputc('}',json);first=0;
      }
   }
   if(json)fputs("],\n  \"im_counts\": [",json);
   for(run=0;run<a->nrun;run++){
      int original=0,retained=0,ambiguous=0;
      for(c=0;c<a->ncol;c++)if(xt_is_im(a,xt_stim(a,c))){
         XT_support support=xt_column_support(a,ref,c,tol);
         if(support.owner<0){ambiguous=1;continue;}
         if(support.owner!=run)continue;
         original++;retained+=support.kept>0;
      }
      printf("# IMCounts Run=%d Retained=%d Original=%d%s\n",run+1,retained,known?original:-1,
             ambiguous?" (unassigned IM columns exist)":"");
      if(known && original>retained)
         fprintf(stderr,"# WARNING: run %d: %d of %d assigned IM columns retained after censoring.\n",run+1,retained,original);
      if(known && !ambiguous){if(previous>=0 && previous!=original)unequal=1;previous=original;}
      if(json){fprintf(json,"%s{\"run\": %d, \"retained\": %d, \"original\": ",run?",":"",run+1,retained);
         if(known)fprintf(json,"%d",original);else fputs("null",json);
         fprintf(json,", \"unassigned_columns\": %s}",ambiguous?"true":"false");}
   }
   if(unequal)puts("# IMCounts: original counts differ across runs; unequal trial counts can be intentional.");
   if(json)fputs("],\n",json);
}

/* Describe full/run-local designs and optionally enforce LSS/REML checks.
 * Extraction uses the same support/censor rules as -split_runs, so diagnostics
 * describe the matrices a downstream command would actually receive.
 * Return 1 for setup/report I/O errors; invalid designs give status 2 only in
 * validation mode. Description mode still reports issues without failing.
 */
static int xt_report(const THD_xmat *a,const THD_xmat *u,const XT_options *o,double zero_tol)
{
   THD_xmat_diagnostic d;FILE *json=NULL;char err[1024]="";int run,s,ok,failed=0,failed_runs=0,reference_ok=1;
   int input_timeline=xt_explicit_timeline(a);
   int explicit_timeline=input_timeline && (!u || xt_explicit_timeline(u));
   int inferred=!explicit_timeline && o->force_inferred_metadata;
   if(!xt_metadata(a,err,sizeof(err)) || !THD_xmat_diagnostics(a,o->rank_tol,&d,err,sizeof(err))){fprintf(stderr,"** %s\n",err);return 1;}
   if(u)reference_ok=THD_xmat_validate_reference(a,u,err,sizeof(err));
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
      fprintf(json,",\n  \"rank_tolerance\": %.17g, \"support_zero_tolerance\": %.17g,\n",o->rank_tol,zero_tol);
   }
   if(!reference_ok)fprintf(stderr,"# WARNING: reference mismatch; support comparisons unavailable.\n");
   if(!explicit_timeline){
      fprintf(stderr,"# WARNING: %s lacks explicit RunStart, GoodList or NRowFull; %s\n",
              !input_timeline?"input":"uncensored reference",
              inferred?"-force_inferred_metadata uses reader defaults for diagnostics only; run/censor assignments may be wrong.":
                       "timeline-dependent diagnostics unavailable. Use -force_inferred_metadata only to inspect reader assumptions.");
      if(inferred)puts("# INFERRED METADATA: missing RunStart assumes one run; missing GoodList assumes consecutive rows; missing NRowFull assumes the stored row count.");
   }
   if(json)fprintf(json,"  \"timeline_metadata_explicit\": %s, \"forced_inferred_metadata\": %s,\n",
                   explicit_timeline?"true":"false",inferred?"true":"false");
   if(reference_ok && (explicit_timeline || inferred))xt_support_report(a,u,o,zero_tol,json);
   else{
      puts("# IMSupport/IMCounts unavailable: explicit timeline metadata and a matching reference (if supplied) are required.");
      if(json)fputs("  \"im_support\": null, \"im_counts\": null,\n",json);
   }
   if(!input_timeline){
      failed=1;
      puts("# RunIssue: run extraction requires explicit RunStart, NRowFull and GoodList; force does not waive this requirement.");
   }
   if(json)fputs("  \"runs\": [\n",json);
   puts("# Columns: Run Start Length NKept NColumns Rank ResidualDF Condition ZeroColumns DuplicatePairs Valid");
   for(run=0;(input_timeline || inferred) && run<a->nrun;run++){
      THD_xmat_run_map *map=NULL;THD_xmat *x=THD_xmat_extract_run(a,u,run,zero_tol,&map,err,sizeof(err));
      THD_xmat_diagnostic rd={0};int nr=0,nc=-1,r,diagnosed=0;ok=x!=NULL;
      for(r=0;r<a->nrow;r++)if(THD_xmat_run_of(a,a->good[r])==run)nr++;
      if(ok){nr=x->nrow;nc=x->ncol;ok=THD_xmat_diagnostics(x,o->rank_tol,&rd,err,sizeof(err));diagnosed=ok;}
      if(ok && o->for_tool && !strcmp(o->for_tool,"lss"))ok=xt_lss(x,o->rank_tol,err,sizeof(err));
      else if(ok && (rd.rank<nc || rd.residual_df<1)){ok=0;snprintf(err,sizeof(err),"rank deficient or no residual degrees of freedom");}
      if(ok && o->for_tool && !strcmp(o->for_tool,"reml") && x->nrow<9){ok=0;snprintf(err,sizeof(err),"3dREMLfit requires at least 9 rows");}
      if(!ok){failed=1;failed_runs++;printf("# RunIssue %d %s\n",run+1,err);}
      printf("%d %d %d %d %d %d %d %.9g %d %d %d\n",run+1,a->runstart[run],THD_xmat_run_length(a,run),nr,nc,diagnosed?rd.rank:-1,diagnosed?rd.residual_df:-1,diagnosed?rd.condition:NAN,diagnosed?rd.zero_columns:-1,diagnosed?rd.duplicate_pairs:-1,ok);
      if(json){
         fprintf(json,"    %s{\"run\": %d, \"start\": %d, \"length\": %d, \"retained\": %d, \"columns\": %d, \"rank\": %d, \"residual_df\": %d, \"valid\": %s, \"issue\": ",run?",":"",run+1,a->runstart[run],THD_xmat_run_length(a,run),nr,nc,diagnosed?rd.rank:-1,diagnosed?rd.residual_df:-1,ok?"true":"false");
         xt_json_text(json,ok?"":err);fputs("}\n",json);
      }
      THD_xmat_free(x);THD_xmat_run_map_free(map);
   }
   if(o->for_tool)printf("# %s preflight checks matrix structure/estimability; dataset, spatial and runtime checks remain with the fitting program.\n",o->for_tool);
   if(json){fprintf(json,"  ], \"valid\": %s\n}\n",failed?"false":"true");if(!xt_close(json,o->json))return 1;}
   if(o->validate)printf("# Validation %s\n",failed?"FAIL":"PASS");
   if(failed)fprintf(stderr,"# WARNING: design checks failed (%d failing run(s); full-model or metadata checks may also fail). -validate returns 2 for validation failures.\n",failed_runs);
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
/* Release the buffered TR selector on both success and failure. Diagnostics
 * are emitted only for selection failures; stdout errors retain their status. */
static int xt_finish_trs(unsigned char *keep,int *selected,NI_int_array *runs,
                         const char *err,int status)
{
   if(status && *err)fprintf(stderr,"** %s\n",err);
   free(keep);free(selected);if(runs)NI_delete_int_array(runs);
   return status;
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
   if(!THD_xmat_attr(a,"RunStart") || !THD_xmat_attr(a,"GoodList") || !THD_xmat_attr(a,"NRowFull"))
      return xt_finish_trs(keep,selected,runs,"-show_trs requires explicit RunStart, GoodList and NRowFull",1);
   if(!xt_metadata(a,err,sizeof(err)))return xt_finish_trs(keep,selected,runs,err,1);
   runs=runlist?parse_runs(runlist,a->nrun):NULL;
   if(!runs || runs->num!=1)return xt_finish_trs(keep,selected,runs,"-show_trs requires exactly one run via -runs N",1);
   /* Keep the run's original offset even when earlier TRs were censored.
    * lo/hi live in full-length run coordinates, not retained-row coordinates. */
   run=runs->ar[0]-1;start=a->runstart[run];length=THD_xmat_run_length(a,run);hi=length-1;
   if(o->tr_window){
      const char *p=o->tr_window;char *end;long first,last;
      errno=0;first=strtol(p,&end,10);
      if(errno || end==p || first<0 || first>=length || strncmp(end,"..",2))return xt_finish_trs(keep,selected,runs,"-tr_window must be START..END within the selected run (zero-based, inclusive)",1);
      p=end+2;errno=0;last=strtol(p,&end,10);
      if(errno || end==p || *end || last<first || last>=length)return xt_finish_trs(keep,selected,runs,"-tr_window must be START..END within the selected run (zero-based, inclusive)",1);
      lo=(int)first;hi=(int)last;
   }
   keep=calloc((size_t)a->ncol,1);selected=malloc(sizeof(int)*(size_t)a->nrow);
   if(!keep || !selected)return xt_finish_trs(keep,selected,runs,"cannot allocate TR selection",1);
   if(o->columns){
      char *end;long v;errno=0;v=strtol(o->columns,&end,10);
      if(errno || end==o->columns || *end || v<0 || v>=a->ncol)return xt_finish_trs(keep,selected,runs,"-show_trs -select_columns needs one zero-based column index",1);
      keep[v]=1;
   }
   if(o->labels && !xt_mark_names(a,o->labels,0,keep))return xt_finish_trs(keep,selected,runs,err,1);
   if(o->stimuli && !xt_mark_names(a,o->stimuli,1,keep))return xt_finish_trs(keep,selected,runs,err,1);
   for(c=0;c<a->ncol;c++)if(keep[c]){col=c;count++;}
   /* Do not silently add basis columns or choose among trials in an IM family:
    * those choices would change the meaning of the requested ideal response. */
   if(count!=1)return xt_finish_trs(keep,selected,runs,"-show_trs needs exactly one design column; use -select_labels or -select_columns for multi-column stimuli",1);
   /* Verify that u really is the full version of a, including every retained
    * value, before letting its otherwise unseen rows set the threshold. */
   if(u){
      if(!xt_metadata(u,err,sizeof(err)))return xt_finish_trs(keep,selected,runs,err,1);
      if(u->nfull!=a->nfull || u->nrow!=a->nfull || u->ncol!=a->ncol || u->nrun!=a->nrun || u->tr!=a->tr || u->nstim!=a->nstim)
         return xt_finish_trs(keep,selected,runs,"uncensored reference dimensions, runs, stimuli or TR do not match",1);
      for(r=0;r<u->nrow;r++)if(u->good[r]!=r)return xt_finish_trs(keep,selected,runs,"uncensored reference must contain every original TR",1);
      for(r=0;r<a->nrun;r++)if(u->runstart[r]!=a->runstart[r])return xt_finish_trs(keep,selected,runs,"uncensored reference RunStart does not match",1);
      for(r=0;r<a->nstim;r++)if(strcmp(a->stim[r].label,u->stim[r].label) || a->stim[r].bot!=u->stim[r].bot || a->stim[r].top!=u->stim[r].top)
         return xt_finish_trs(keep,selected,runs,"uncensored reference stimulus ranges/labels do not match",1);
      for(c=0;c<a->ncol;c++){
         if(strcmp(a->collab[c],u->collab[c]))return xt_finish_trs(keep,selected,runs,"uncensored reference column labels do not match",1);
         for(r=0;r<a->nrow;r++)if(a->mat[(size_t)c*a->nrow+r]!=u->mat[(size_t)c*u->nrow+a->good[r]])
            return xt_finish_trs(keep,selected,runs,"uncensored reference differs at a retained matrix value",1);
      }
   }
   /* A censored peak would lower the threshold and admit different TRs.
    * Requiring the full reference makes fractional selection reproducible. */
   if(o->tr_fraction && a->nrow<a->nfull && !u)
      return xt_finish_trs(keep,selected,runs,"fractional selection on a censored matrix requires -uncensored to define the pre-censor peak",1);
   /* Find the positive (not absolute) peak in the explicit run/window.
    * For absolute thresholds this pass still checks relevant values are finite. */
   for(r=0;r<ref->nrow;r++){
      int t=ref->good[r]-start;double v=ref->mat[(size_t)col*ref->nrow+r];
      if(t<lo || t>hi)continue;
      if(!isfinite(v))return xt_finish_trs(keep,selected,runs,"selected ideal contains nonfinite values",1);
      if(v>peak)peak=v;
   }
   if(o->tr_fraction){
      if(peak<=0)return xt_finish_trs(keep,selected,runs,"selected run/window has no positive ideal-response peak",1);
      threshold*=peak;
   }
   /* Walk only retained observations. Subtracting start converts GoodList's
    * concatenated-timeline TRs into selectors for the original per-run EPI.
    * Strict > intentionally excludes values exactly equal to the threshold. */
   for(r=0;r<a->nrow;r++){
      int t=a->good[r]-start;double v=a->mat[(size_t)col*a->nrow+r];
      if(t>=lo && t<=hi && v>threshold)selected[n++]=t;
   }
   if(!n)return xt_finish_trs(keep,selected,runs,"no retained TRs exceed the threshold in the selected run/window",1);
   for(r=0;r<n;r++)printf("%s%d",r?",":"",selected[r]);
   putchar('\n');
   return xt_finish_trs(keep,selected,runs,err,ferror(stdout)?1:0);
}

/* Translate zero-based column ranges to the strict one-based run parser.
   On success, add columns to the caller's mask; always release parsing storage. */
static int xt_mark_columns(const THD_xmat *a,const char *columns,
                           unsigned char *keep,char *err,int nerr)
{
   const char *q=columns;
   NI_int_array *v=NULL;char *one=NULL;int i,ok=1;
   while(*q && ok){
      char *end;long n;errno=0;n=strtol(q,&end,10);
      if(errno || end==q || n<0 || n>=a->ncol){
         snprintf(err,nerr,"invalid zero-based -select_columns");ok=0;
      }else{
         one=THD_zzprintf(one,"%ld",n+1);q=end;
         if(q[0]=='.' && q[1]=='.'){one=THD_zzprintf(one,"..");q+=2;}
         else if(*q==','){one=THD_zzprintf(one,",");q++;}
         else if(*q){snprintf(err,nerr,"invalid -select_columns");ok=0;}
      }
   }
   if(ok){
      v=one?parse_runs(one,a->ncol):NULL;
      if(!v || !v->num){snprintf(err,nerr,"invalid -select_columns");ok=0;}
      else for(i=0;i<v->num;i++)keep[v->ar[i]-1]=1;
   }
   if(v)NI_delete_int_array(v);
   free(one);return ok;
}

/* Resolve group names/numbers into the caller's mask, releasing decoded lists. */
static int xt_mark_groups(const THD_xmat *a,const char *list,
                          unsigned char *keep,char *err,int nerr)
{
   const char *v=THD_xmat_attr(a,"ColumnGroups");
   NI_int_array *g=v?NI_decode_int_list((char *)v,","):NULL;
   NI_str_array *groups=NI_decode_string_list((char *)list,",");
   int i,c,ok=g && g->num==a->ncol && groups;
   if(!ok)snprintf(err,nerr,"group selection needs valid ColumnGroups");
   for(i=0;ok && i<groups->num;i++){
      char *end;long n=strtol(groups->str[i],&end,10);
      int all=!strcmp(groups->str[i],"stimuli"),found=0;
      if(!strcmp(groups->str[i],"baseline"))n=-1;
      else if(!strcmp(groups->str[i],"nuisance"))n=0;
      else if(!all && (end==groups->str[i] || *end)){
         snprintf(err,nerr,"unknown group: %s",groups->str[i]);ok=0;
      }
      if(ok){
         for(c=0;c<a->ncol;c++)if(all?g->ar[c]>0:g->ar[c]==n){keep[c]=1;found=1;}
         if(!found){snprintf(err,nerr,"group selects no columns: %s",groups->str[i]);ok=0;}
      }
   }
   if(g)NI_delete_int_array(g);
   if(groups)NI_delete_str_array(groups);
   return ok;
}

/* Select and write a matrix plus provenance; roll back the matrix if maps fail. */
static int xt_select(const THD_xmat *a,const XT_options *o,const char *prefix)
{
   unsigned char *keep=calloc((size_t)a->ncol,1);THD_xmat *x=NULL;THD_xmat_run_map *m=NULL;
   char err[1024]="",*paths[3]={NULL,NULL,NULL};int i,ok,wrote=0;
   if(!keep || !prefix){fprintf(stderr,"** selection requires -prefix\n");free(keep);return 1;}
   ok=xt_metadata(a,err,sizeof(err));
   if(ok && o->columns)ok=xt_mark_columns(a,o->columns,keep,err,sizeof(err));
   if(ok && o->labels)ok=xt_mark_names(a,o->labels,0,keep);
   if(ok && o->stimuli)ok=xt_mark_names(a,o->stimuli,1,keep);
   if(ok && o->groups)ok=xt_mark_groups(a,o->groups,keep,err,sizeof(err));
   if(ok){x=THD_xmat_select_columns(a,keep,o->drop_glts,&m,err,sizeof(err));ok=x!=NULL;}
   if(ok){
      paths[0]=THD_zzprintf(NULL,"%s.xmat.1D",prefix);paths[1]=THD_zzprintf(NULL,"%s.rows.1D",prefix);paths[2]=THD_zzprintf(NULL,"%s.columns.1D",prefix);
      wrote=THD_xmat_write(x,paths[0],err,sizeof(err));
      ok=wrote && THD_xmat_write_run_map(a,x,m,paths[1],paths[2],err,sizeof(err));
   }
   if(ok)printf("# Selected %d/%d columns -> %s\n",x->ncol,a->ncol,paths[0]);
   else{if(wrote)unlink(paths[0]);fprintf(stderr,"** selection: %s\n",*err?err:"invalid selection");}
   for(i=0;i<3;i++)free(paths[i]);
   free(keep);THD_xmat_free(x);THD_xmat_run_map_free(m);return !ok;
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
/* Validate all event keys before opening a manifest output file. */
static int xt_events_valid(const THD_xmat *a,const THD_datatable *events,
                           int ri,int vi,int ci,char *err,int nerr)
{
   int r;
   if(ri<0 || vi<0 || ci<0){
      snprintf(err,nerr,"events need Run Volume Condition (optional Item)");return 0;
   }
   for(r=0;r<events->nrow;r++){
      int rr,vv,j,rr2,vv2;
      if(!xt_integer(DT_CELL(events,r,ri),&rr) || rr<1 || rr>a->nrun || !xt_integer(DT_CELL(events,r,vi),&vv) || !*DT_CELL(events,r,ci)){
         snprintf(err,nerr,"invalid events Run/Volume/Condition on row %d",r+1);return 0;}
      for(j=0;j<r;j++){xt_integer(DT_CELL(events,j,ri),&rr2);xt_integer(DT_CELL(events,j,vi),&vv2);
         if(rr==rr2 && vv==vv2){snprintf(err,nerr,"duplicate Run/Volume in events");return 0;}}
   }
   return 1;
}

/* Write one run using borrowed resources, marking matched rows in seen. */
static int xt_manifest_run(const THD_xmat *a,const THD_xmat *x,
   const THD_xmat_run_map *m,const XT_options *o,const THD_datatable *events,
   int ri,int vi,int ci,int ii,int im,int run,const char *beta,FILE *fp,
   int *seen,int *total,char *err,int nerr)
{
   int c,r,local=0;
   for(c=0;c<x->ncol;c++)if(m->columns[c]>=a->stim[im].bot && m->columns[c]<=a->stim[im].top){
      for(r=0;r<events->nrow;r++){int rr,vv;xt_integer(DT_CELL(events,r,ri),&rr);xt_integer(DT_CELL(events,r,vi),&vv);if(rr==run+1 && vv==local)break;}
      if(r==events->nrow){snprintf(err,nerr,"missing event for run %d volume %d; check run/volume indexing and event-to-design correspondence",run+1,local);return 0;}
      seen[r]=1;(*total)++;
      {char *sample=THD_zzprintf(NULL,"%s_r%02d_v%04d",o->subject,run+1,local),*sel=NULL;
       THD_1D_print_token(fp,sample);fputc(' ',fp);THD_1D_print_token(fp,o->subject);
       fprintf(fp," %d ",run+1);THD_1D_print_token(fp,ii>=0?DT_CELL(events,r,ii):a->collab[m->columns[c]]);
       fputc(' ',fp);THD_1D_print_token(fp,DT_CELL(events,r,ci));fprintf(fp," %d ",local);THD_1D_print_token(fp,a->fname);
       fprintf(fp," %d ",m->columns[c]);
       if(o->manifest_format && !strcmp(o->manifest_format,"selected"))sel=THD_zzprintf(NULL,"%s[%d]",beta,local);
       THD_1D_print_token(fp,sel?sel:beta);fputc('\n',fp);free(sel);free(sample);}
      local++;
   }
   return 1;
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
   THD_datatable *events=NULL;FILE *fp=NULL;int ri,vi,ci,ii,run,r,im=-1,ok=0,total=0;
   int *seen=NULL;char *path=NULL,err[1024]="";
   if(!prefix || !o->events || !o->subject || (!o->beta && !o->beta_pattern) || (o->beta && o->beta_pattern)){
      fprintf(stderr,"** export needs -prefix -events -subject and exactly one of -beta/-beta_pattern\n");return 1;
   }
   if(o->beta && a->nrun!=1){fprintf(stderr,"** -beta requires a single-run matrix; use -beta_pattern for per-run buckets\n");return 1;}
   for(r=0;r<a->nstim;r++)if(xt_is_im(a,r)){if(im>=0){fprintf(stderr,"** export requires exactly one IM family\n");return 1;}im=r;}
   if(im<0){fprintf(stderr,"** export requires an IM family\n");return 1;}
   events=THD_read_datatable_file((char *)o->events);
   ri=THD_datatable_column(events,"Run");vi=THD_datatable_column(events,"Volume");ci=THD_datatable_column(events,"Condition");ii=THD_datatable_column(events,"Item");
   ok=xt_events_valid(a,events,ri,vi,ci,err,sizeof(err));
   if(ok){
      seen=calloc((size_t)events->nrow,sizeof(int));
      if(!seen){snprintf(err,sizeof(err),"out of memory");ok=0;}
   }
   if(ok){path=THD_zzprintf(NULL,"%s.trials.1D",prefix);fp=xt_open(path);ok=fp!=NULL;}
   if(ok){
      fprintf(fp,"# 1dXmatTool trial manifest; Run is one-based, Volume/XColumn are zero-based.\n");
      fprintf(fp,"# Columns: Sample Subj Run Item Condition Volume %s InputFile\n",
         o->manifest_format && !strcmp(o->manifest_format,"selected")?"SourceXmat SourceColumn":"Xmat XColumn");
   }
   for(run=0;ok && run<a->nrun;run++){
      THD_xmat_run_map *m=NULL;
      THD_xmat *x=THD_xmat_extract_run(a,u,run,tol,&m,err,sizeof(err));
      char *beta=xt_beta_name(o,run+1);
      if(!x || !beta){
         if(!*err)snprintf(err,sizeof(err),"beta pattern must contain exactly one {run}");
         ok=0;
      }else ok=xt_manifest_run(a,x,m,o,events,ri,vi,ci,ii,im,run,beta,fp,
                               seen,&total,err,sizeof(err));
      THD_xmat_free(x);THD_xmat_run_map_free(m);free(beta);
   }
   /* Having found every design trial is insufficient: extra event rows would
    * otherwise leave incorrect or stale metadata unnoticed. */
   for(r=0;ok && r<events->nrow;r++)if(!seen[r]){
      snprintf(err,sizeof(err),"events contain unmatched rows: row %d, run %s volume %s; check run/volume indexing and event-to-design correspondence",
               r+1,DT_CELL(events,r,ri),DT_CELL(events,r,vi));ok=0;
   }
   if(ok){ok=xt_close(fp,path);fp=NULL;}
   if(ok)printf("# Exported %d trials -> %s\n",total,path);
   if(fp){fclose(fp);unlink(path);}if(!ok)fprintf(stderr,"** manifest: %s\n",*err?err:"cannot write output");
   free(path);free(seen);THD_free_datatable(events);return !ok;
}

/* ========================================================================
 * FITTED-BETA TABLE EXPORT
 *
 * Keep this in the command rather than thd_xmat: subject annotations, output
 * naming and MVPA/RSA table layouts are CLI policy, not matrix mathematics.
 * A fitted Volume is NOT its source design column. Voxelwise HRF selection
 * retains a candidate catalog and ID map instead of a fictitious single Xmat.
 *
 * All option strings borrow argv storage. The exporter owns its working
 * storage and tables; normalized rows borrow table cells, so tables are freed
 * after rows. The existing AFNI table reader/token writer supplies quoting.
 * No fitting, image copying or trial averaging occurs during table export.
 * ======================================================================== */
typedef struct {
   const char *manifest, *events, *subject, *prefix, *format, *input_base, *runs;
   int run_base; /* Input manifest convention: Picard=0, LSS export=1. */
} XT_beta_options;

#define XB_MAX_RUN 1000000

typedef struct {
   int source, volume, run, source_run, trial, basis, keep;
   const char *condition, *item, *input;
} XB_row;

typedef struct {
   THD_datatable *table, *events;
   XB_row *row;
   int nrow, max_run, candidate;
   char *base;
} XB_table;

/* Each output owns a temporary sibling file. link() publishes without replacing
 * a destination; dev/ino checks prevent rollback from removing someone else's
 * replacement. The main table is published last, after all of its companions.
 */
typedef struct {
   char *path, *temporary;
   FILE *stream;
   int published;
   dev_t device;
   ino_t inode;
} XB_output;

static int xb_error(char *err, const char *format, ...)
{
   va_list ap;
   va_start(ap, format); vsnprintf(err, 1024, format, ap); va_end(ap);
   return 0;
}

/* Integers are exact identifiers, so atoi()'s acceptance of trailing text is
 * inappropriate. Only optional identities admit the explicit -1 sentinel.
 */
static int xb_integer(const char *s, int missing, int *value)
{
   char *end; long v;
   if(!s) { *value=-1; return missing; }
   errno=0; v=strtol(s,&end,10);
   if(errno || end==s || *end || v < (missing?-1:0) || v>INT_MAX) return 0;
   *value=(int)v; return 1;
}

static const char *xb_cell(const THD_datatable *t, int row, const char *name)
{
   int c=t?THD_datatable_column((THD_datatable *)t,(char *)name):-1;
   return c<0?NULL:DT_CELL(t,row,c);
}

/* Reject duplicate headers and control bytes before any output is created.
 * Ordinary spaces and quotes are preserved by THD_1D_print_token below.
 */
static int xb_check_table(const THD_datatable *t, char *err)
{
   int c,j,r;
   for(c=0;c<t->ncol;c++) {
      for(j=0;j<c;j++) if(!strcmp(t->cname[c],t->cname[j]))
         return xb_error(err,"duplicate column '%s'",t->cname[c]);
      for(r=-1;r<t->nrow;r++) {
         const unsigned char *s=(unsigned char *)(r<0?t->cname[c]:DT_CELL(t,r,c));
         for(;*s;s++) if(*s<32 || *s==127)
            return xb_error(err,"control character in table column '%s'",t->cname[c]);
      }
   }
   return 1;
}

/* Dataset names may be AFNI prefixes without a real file at that exact path.
 * Make them absolute without calling realpath on the dataset itself. Opening
 * the header below validates the AFNI name and the actual sub-brick range.
 * The base directory is explicit; never guess between two existing datasets.
 */
static char *xb_path(const char *base, const char *name)
{
   if(!name || !*name || !strcmp(name,"-") || !strcmp(name,"NA")) return NULL;
   return name[0]=='/'?strdup(name):THD_zzprintf(NULL,"%s/%s",base,name);
}

/* Normalize path cells in place. XB_row borrows the resulting owned strings.
 * We use raw cells rather than the reader's cached fname/val arrays afterward.
 */
static int xb_paths(XB_table *b, char *err)
{
   const char *names[]={"InputFile","Xmat","SourceXmat","CandidateManifest","DesignIDFile"};
   int k,c,r;
   for(k=0;k<5;k++) {
      c=THD_datatable_column(b->table,(char *)names[k]);
      if(c<0) continue;
      for(r=0;r<b->nrow;r++) {
         char *path=xb_path(b->base,DT_CELL(b->table,r,c));
         if(!path) return xb_error(err,"missing %s on row %d",names[k],r+1);
         free(DT_CELL(b->table,r,c)); DT_CELL(b->table,r,c)=path;
         if(k==3 && access(path,R_OK))
            return xb_error(err,"cannot read candidate catalog %s",path);
      }
   }
   return 1;
}

/* An events table supplements missing semantic identities by ACTUAL beta Volume,
 * never by label or matrix-column position. One bucket is required for this join:
 * otherwise Volume=0 could refer to several different datasets. Run in events
 * is always one-based, even when the source manifest uses Picard's zero base.
 */
static int xb_event_row(XB_table *b, int volume, int *seen, char *err)
{
   int r,found=-1,v;
   for(r=0;b->events && r<b->events->nrow;r++) {
      if(!xb_integer(xb_cell(b->events,r,"Volume"),0,&v)) {
         xb_error(err,"events need a nonnegative Volume on every row"); return -2;
      }
      if(v==volume) {
         if(found>=0) { xb_error(err,"duplicate events Volume %d",volume); return -2; }
         found=r;
      }
   }
   if(b->events && found<0) {
      xb_error(err,"missing event for beta Volume %d",volume); return -2;
   }
   if(found>=0) seen[found]=1;
   return found;
}

/* Merge explicit metadata, rejecting contradictions instead of silently letting
 * an annotation table relabel a known trial. -1 is a missing identity, not run 0.
 */
static int xb_identity(XB_table *b, int r, int e, const char *name,
                       int run_base, int *value, char *err)
{
   const char *s=xb_cell(b->table,r,name), *t=e<0?NULL:xb_cell(b->events,e,name);
   int original,annotation;
   if(!xb_integer(s,1,&original) || !xb_integer(t,1,&annotation))
      return xb_error(err,"invalid %s on beta row %d",name,r+1);
   if(!strcmp(name,"Run")) {
      if(original>=0) {
         if(original<run_base || original>XB_MAX_RUN+run_base-1)
            return xb_error(err,"manifest Run is outside the declared run-base/range");
         original=original-run_base+1;
      }
      if(t && annotation!=-1 && (annotation<1 || annotation>XB_MAX_RUN))
         return xb_error(err,"events Run must be one-based");
   }
   if(original>=0 && annotation>=0 && original!=annotation)
      return xb_error(err,"events %s contradicts beta row %d",name,r+1);
   *value=original>=0?original:annotation;
   return 1;
}

/* Header-only dataset reads catch stale or out-of-range mappings without loading
 * voxel data. Cache the most recently used dataset: Picard usually has one bucket.
 */
static int xb_volume(const char *path, int volume, THD_3dim_dataset **cache,
                     const char **last, char *err)
{
   if(strchr(path,'[') || strchr(path,']'))
      return xb_error(err,"manifest paths must name whole datasets, not selectors");
   if(!*last || strcmp(*last,path)) {
      if(*cache) DSET_delete(*cache);
      *cache=THD_open_dataset((char *)path); *last=path;
      if(!*cache) return xb_error(err,"cannot open beta/DesignID dataset %s",path);
   }
   if(volume<0 || volume>=DSET_NVALS(*cache))
      return xb_error(err,"Volume %d is out of range in %s",volume,path);
   return 1;
}

/* Validate ALL source rows before filtering. An invalid excluded row must not
 * hide a duplicate identity or a malformed mapping in the supplied manifest.
 */
static int xb_rows(XB_table *b, const XT_beta_options *o, char *err)
{
   THD_3dim_dataset *ds=NULL,*id_ds=NULL;
   THD_xmat *xm=NULL;
   const char *last=NULL,*id_last=NULL,*xlast=NULL;
   int *seen=NULL,r,j,e,ok=1,c,has_xmat,has_column;
   if(!xb_cell(b->table,0,"InputFile") || !xb_cell(b->table,0,"Volume"))
      return xb_error(err,"beta manifest requires InputFile and Volume");
   b->candidate=xb_cell(b->table,0,"CandidateManifest")!=NULL;
   has_xmat=xb_cell(b->table,0,"Xmat")!=NULL;
   has_column=xb_cell(b->table,0,"XColumn")!=NULL;
   if((has_xmat && xb_cell(b->table,0,"SourceXmat")) ||
      (has_column && xb_cell(b->table,0,"SourceColumn")))
      return xb_error(err,"ambiguous simultaneous source and design-column metadata");
   if(has_xmat!=has_column || (b->candidate && has_xmat))
      return xb_error(err,"use paired Xmat/XColumn OR candidate provenance, not both");
   if(b->candidate && (!xb_cell(b->table,0,"Column") ||
      !xb_cell(b->table,0,"DesignIDFile") || !xb_cell(b->table,0,"DesignIDVolume")))
      return xb_error(err,"candidate manifest needs Column, DesignIDFile and DesignIDVolume");
   if(!b->candidate && (xb_cell(b->table,0,"DesignIDFile") || xb_cell(b->table,0,"DesignIDVolume")))
      return xb_error(err,"DesignID metadata requires CandidateManifest");
   if(b->events) {
      seen=calloc((size_t)b->events->nrow,sizeof(int));
      if(!seen) return xb_error(err,"out of memory");
   }
   for(r=0;ok && r<b->nrow;r++) {
      XB_row *v=b->row+r;
      const char *s,*annotation;
      v->source=r; v->input=xb_cell(b->table,r,"InputFile");
      if(b->events && strcmp(v->input,xb_cell(b->table,0,"InputFile"))) {
         ok=xb_error(err,"-events Volume joins require a single beta bucket"); break;
      }
      if(!xb_integer(xb_cell(b->table,r,"Volume"),0,&v->volume) ||
         !xb_volume(v->input,v->volume,&ds,&last,err)) {
         if(!*err) xb_error(err,"invalid beta Volume on row %d",r+1);
         ok=0; break;
      }
      for(j=0;j<r;j++) if(v->volume==b->row[j].volume && !strcmp(v->input,b->row[j].input))
         ok=xb_error(err,"duplicate InputFile/Volume on beta row %d",r+1);
      if(!ok) break;
      e=xb_event_row(b,v->volume,seen,err);
      if(e==-2) { ok=0; break; }
      s=xb_cell(b->table,r,"Subj");
      if(s && strcmp(s,o->subject)) { ok=xb_error(err,"manifest Subj disagrees with -subject"); break; }
      s=xb_cell(b->table,r,"Condition");
      annotation=e<0?NULL:xb_cell(b->events,e,"Condition");
      if(s && (!*s || !strcmp(s,"-1") || !strcmp(s,"-"))) s=NULL;
      if(s && annotation && strcmp(s,annotation)) {
         ok=xb_error(err,"events Condition contradicts beta row %d",r+1); break;
      }
      v->condition=s?s:annotation;
      if(!v->condition || !*v->condition || !strcmp(v->condition,"-1") || !strcmp(v->condition,"-")) {
         ok=xb_error(err,"missing Condition; supply -events with Volume Condition [Run Trial Item]"); break;
      }
      if(!xb_identity(b,r,e,"Run",o->run_base,&v->run,err) ||
         !xb_identity(b,r,e,"Trial",o->run_base,&v->trial,err) ||
         !xb_identity(b,r,e,"Basis",o->run_base,&v->basis,err)) { ok=0; break; }
      if(!xb_integer(xb_cell(b->table,r,"Run"),1,&v->source_run)) { ok=0; break; }
      b->max_run=MAX(b->max_run,v->run);
      s=xb_cell(b->table,r,"Item"); annotation=e<0?NULL:xb_cell(b->events,e,"Item");
      if(s && strcmp(s,"-") && annotation && strcmp(s,annotation)) {
         ok=xb_error(err,"events Item contradicts beta row %d",r+1); break;
      }
      v->item=(s && strcmp(s,"-"))?s:(annotation?annotation:"-");
      for(j=0;j<r;j++) if(v->trial>=0 && v->trial==b->row[j].trial &&
         (v->basis==b->row[j].basis || v->run!=b->row[j].run ||
          strcmp(v->condition,b->row[j].condition) || strcmp(v->item,b->row[j].item)))
         ok=xb_error(err,"duplicate or inconsistent Trial/Basis identity");
      if(has_xmat && ok) {
         s=xb_cell(b->table,r,"Xmat");
         if(!xlast || strcmp(xlast,s)) {
            THD_xmat_free(xm); xm=THD_xmat_read(s,err,1024); xlast=s;
         }
         if(!xm) ok=0;
         else if(!xb_integer(xb_cell(b->table,r,"XColumn"),0,&c) || c>=xm->ncol)
            ok=xb_error(err,"XColumn out of range on beta row %d",r+1);
      }
      if(b->candidate && ok) {
         if(!xb_integer(xb_cell(b->table,r,"Column"),0,&c) ||
            !xb_integer(xb_cell(b->table,r,"DesignIDVolume"),0,&c))
            ok=xb_error(err,"invalid candidate Column/DesignIDVolume");
         else ok=xb_volume(xb_cell(b->table,r,"DesignIDFile"),c,&id_ds,&id_last,err);
      }
   }
   for(r=0;ok && b->events && r<b->events->nrow;r++)
      if(!seen[r]) ok=xb_error(err,"events contain unmatched rows");
   free(seen); THD_xmat_free(xm);
   if(ds) DSET_delete(ds);
   if(id_ds) DSET_delete(id_ds);
   return ok;
}

/* Select existing one-based runs. Bound range traversal by the largest validated
 * run rather than allowing an arbitrarily large command-line integer loop.
 */
static int xb_select(XB_table *b, const char *list, char *err)
{
   int r,n=0,*seen=NULL;
   if(list) {
      const char *p=list;
      if(!*p || b->max_run<1) return xb_error(err,"-runs requires known run identities");
      seen=calloc((size_t)b->max_run+1,sizeof(int));
      if(!seen) return xb_error(err,"out of memory");
      while(*p) {
         char *end; long first,last,v; int step,found;
         errno=0; first=strtol(p,&end,10);
         if(errno || end==p || first<1 || first>b->max_run) break;
         p=end; last=first;
         if(p[0]=='.' && p[1]=='.') {
            p+=2; errno=0; last=strtol(p,&end,10);
            if(errno || end==p || last<1 || last>b->max_run) break;
            p=end;
         }
         if(*p && *p!=',') break;
         step=last>=first?1:-1;
         for(v=first;;v+=step) {
            found=0;
            for(r=0;r<b->nrow;r++) if(b->row[r].run==v) found=1;
            if(!found || seen[v]) { free(seen); return xb_error(err,"-runs has an absent or repeated run"); }
            seen[v]=1;
            if(v==last) break;
         }
         if(*p==',') { p++; if(!*p) { free(seen); return xb_error(err,"trailing comma in -runs"); } }
      }
      if(*p) { free(seen); return xb_error(err,"invalid one-based -runs list"); }
   }
   for(r=0;r<b->nrow;r++) {
      b->row[r].keep=!list || (b->row[r].run>0 && seen[b->row[r].run]);
      n+=b->row[r].keep;
   }
   free(seen);
   return n>0?1:xb_error(err,"selection contains no betas");
}

/* Format-specific contracts are checked before creating any file. Classic RSA
 * requires one subject/condition cell. Runwise RSA accepts repeats but requires
 * explicit unique trial IDs and one coefficient per trial, on independent runs.
 */
static int xb_format(const XB_table *b, int rsa, int runwise, char *err)
{
   int r,j,nrun=0;
   for(r=0;r<b->nrow;r++) if(b->row[r].keep) {
      const XB_row *v=b->row+r;
      if(runwise && (v->run<1 || v->trial<0))
         return xb_error(err,"rsa_runwise requires explicit Run and Trial for every selected beta");
      for(j=0;j<r;j++) if(b->row[j].keep) {
         const XB_row *w=b->row+j;
         if(rsa && !strcmp(v->condition,w->condition))
            return xb_error(err,"rsa needs one beta per condition; select a run or use rsa_runwise");
         if(runwise && v->trial==w->trial)
            return xb_error(err,"rsa_runwise needs one coefficient per unique Trial; multibasis trials require selection first");
         if(runwise && v->run==w->run && strcmp(v->input,w->input))
            return xb_error(err,"rsa_runwise needs one beta bucket per run (selectors may share one whole-session bucket)");
      }
      for(j=0;j<r;j++) if(b->row[j].keep && b->row[j].run==v->run) break;
      if(j==r) nrun++;
   }
   return runwise && nrun<2?xb_error(err,"rsa_runwise requires at least two selected runs"):1;
}

static int xb_core(const char *name)
{
   const char *core[]={"Sample","Subj","Run","Item","Condition","Trial","Basis","Volume","SourceRun","InputFile"};
   int i; for(i=0;i<10;i++) if(!strcmp(core[i],name)) return 1;
   return 0;
}

/* Use the established AFNI token writer rather than a second escaping scheme.
 * Extra provenance columns survive unchanged, except resolved absolute paths.
 * Selected tables rename Xmat fields to prevent downstream automatic expansion.
 */
static int xb_write_identities(FILE *f, const XB_table *b, const XT_beta_options *o, int selected, char *err)
{
   int c,r;
   fputs("# SourceBetaManifest ",f); THD_1D_print_token(f,b->table->source); fputc('\n',f);
   fputs("# Run is one-based; SourceRun retains the input manifest index (-1 if absent).\n",f);
   fputs("# Columns: Sample Subj Run Item Condition Trial Basis Volume SourceRun",f);
   for(c=0;c<b->table->ncol;c++) if(!xb_core(b->table->cname[c])) {
      const char *name=b->table->cname[c];
      if(selected && !strcmp(name,"Xmat")) name="SourceXmat";
      if(selected && !strcmp(name,"XColumn")) name="SourceColumn";
      fputc(' ',f); THD_1D_print_token(f,name);
   }
   fputs(" InputFile\n",f);
   for(r=0;r<b->nrow;r++) if(b->row[r].keep) {
      const XB_row *v=b->row+r;
      const char *source_sample=xb_cell(b->table,r,"Sample");
      char *sample=source_sample?strdup(source_sample):THD_zzprintf(NULL,"%s_b%06d",o->subject,r);
      if(!sample) return xb_error(err,"out of memory writing sample identities");
      THD_1D_print_token(f,sample); free(sample); fputc(' ',f);
      THD_1D_print_token(f,o->subject); fprintf(f," %d ",v->run);
      THD_1D_print_token(f,v->item); fputc(' ',f); THD_1D_print_token(f,v->condition);
      fprintf(f," %d %d %d %d",v->trial,v->basis,v->volume,v->source_run);
      for(c=0;c<b->table->ncol;c++) if(!xb_core(b->table->cname[c])) {
         fputc(' ',f); THD_1D_print_token(f,DT_CELL(b->table,r,c));
      }
      fputc(' ',f);
      if(selected) {
         char *path=THD_zzprintf(NULL,"%s[%d]",v->input,v->volume);
         if(!path) return xb_error(err,"out of memory writing a beta selector");
         THD_1D_print_token(f,path); free(path);
      } else THD_1D_print_token(f,v->input);
      fputc('\n',f);
   }
   return 1;
}

/* Stage all files, then publish companions before the primary table. On any
 * failure remove only the inodes created by this invocation. No output is
 * touched until the entire input and all destination names have been checked.
 */
static int xb_open_outputs(XB_output *out, int n, char *err)
{
   int i,fd; struct stat st;
   for(i=0;i<n;i++) {
      if(!out[i].path) return xb_error(err,"out of memory");
      if(!lstat(out[i].path,&st)) return xb_error(err,"output already exists: %s",out[i].path);
      if(errno!=ENOENT) return xb_error(err,"cannot access output: %s",out[i].path);
   }
   for(i=0;i<n;i++) {
      out[i].temporary=THD_zzprintf(NULL,"%s.tmpXXXXXX",out[i].path);
      if(!out[i].temporary) return xb_error(err,"out of memory");
      fd=mkstemp(out[i].temporary);
      if(fd<0) return xb_error(err,"cannot stage output %s",out[i].path);
      if(fstat(fd,&st)) { close(fd); return xb_error(err,"cannot stat staged output"); }
      out[i].device=st.st_dev; out[i].inode=st.st_ino;
      out[i].stream=fdopen(fd,"w");
      if(!out[i].stream) { close(fd); return xb_error(err,"cannot open output stream"); }
   }
   return 1;
}

static int xb_commit(XB_output *out, int n, char *err)
{
   int i,bad;
   for(i=0;i<n;i++) {
      bad=ferror(out[i].stream);
      if(fclose(out[i].stream)) bad=1;
      out[i].stream=NULL;
      if(bad) return xb_error(err,"failed writing %s",out[i].path);
   }
   for(i=n-1;i>=0;i--) {
      if(link(out[i].temporary,out[i].path)) return xb_error(err,"cannot publish without replacement: %s",out[i].path);
      out[i].published=1;
   }
   return 1;
}

static void xb_free_outputs(XB_output *out, int n, int success)
{
   int i; struct stat st;
   for(i=0;out && i<n;i++) {
      if(out[i].stream) fclose(out[i].stream);
      if(!success && out[i].published && !lstat(out[i].path,&st) &&
         st.st_dev==out[i].device && st.st_ino==out[i].inode) unlink(out[i].path);
      if(out[i].temporary && !lstat(out[i].temporary,&st) &&
         st.st_dev==out[i].device && st.st_ino==out[i].inode) unlink(out[i].temporary);
      free(out[i].temporary); free(out[i].path);
   }
   free(out);
}

static int xb_write(XB_table *b, const XT_beta_options *o, char *err)
{
   int rsa=!strcmp(o->format,"rsa"),runwise=!strcmp(o->format,"rsa_runwise");
   int selected=!strcmp(o->format,"selected"),n=1,r,j,k=0,ok=0;
   int *runs=calloc((size_t)b->nrow,sizeof(int));
   XB_output *out=NULL;
   if(!runs) return xb_error(err,"out of memory");
   if(!xb_format(b,rsa,runwise,err)) { free(runs); return 0; }
   if(runwise) for(r=0;r<b->nrow;r++) if(b->row[r].keep) {
      for(j=0;j<k;j++) if(runs[j]==b->row[r].run) break;
      if(j==k) runs[k++]=b->row[r].run;
   }
   if(rsa || runwise) n=2+k;
   out=calloc((size_t)n,sizeof(*out));
   if(!out) { free(runs); return xb_error(err,"out of memory"); }
   out[0].path=THD_zzprintf(NULL,"%s.%s.1D",o->prefix,rsa?"rsa":runwise?"runwise":"trials");
   if(n>1) out[1].path=THD_zzprintf(NULL,"%s.identities.1D",o->prefix);
   for(j=0;j<k;j++) out[j+2].path=THD_zzprintf(NULL,"%s.r%02d.trials.1D",o->prefix,runs[j]);
   ok=xb_open_outputs(out,n,err);
   if(ok && !rsa && !runwise) ok=xb_write_identities(out[0].stream,b,o,selected,err);
   if(ok && (rsa || runwise)) ok=xb_write_identities(out[1].stream,b,o,0,err);
   if(ok && rsa) {
      FILE *f=out[0].stream;
      fputs("# Columns: Subj Condition InputFile\n",f);
      for(r=0;ok && r<b->nrow;r++) if(b->row[r].keep) {
         char *path=THD_zzprintf(NULL,"%s[%d]",b->row[r].input,b->row[r].volume);
         if(!path) { ok=xb_error(err,"out of memory writing a beta selector"); break; }
         THD_1D_print_token(f,o->subject); fputc(' ',f);
         THD_1D_print_token(f,b->row[r].condition); fputc(' ',f);
         THD_1D_print_token(f,path); fputc('\n',f); free(path);
      }
   }
   if(ok && runwise) {
      FILE *f=out[0].stream;
      fputs("# Columns: Subj Run TrialFile InputFile\n",f);
      for(j=0;j<k && ok;j++) {
         char *selector=NULL,*trial_path; int count=0;
         FILE *trial=out[j+2].stream;
         fputs("# Columns: Trial Condition\n",trial);
         for(r=0;r<b->nrow;r++) if(b->row[r].keep && b->row[r].run==runs[j]) {
            const XB_row *v=b->row+r;
            if(!count) selector=THD_zzprintf(NULL,"%s[",v->input);
            selector=THD_zzprintf(selector,"%s%d",count?",":"",v->volume); count++;
            fprintf(trial,"%d ",v->trial); THD_1D_print_token(trial,v->condition); fputc('\n',trial);
         }
         selector=THD_zzprintf(selector,"]");
         /* Staged companions do not exist at their final names yet. Resolve the
          * existing parent directory, then append the intended basename. */
         { char *parent=strdup(out[j+2].path),*slash=strrchr(parent,'/'),*absolute;
           const char *leaf=slash?slash+1:out[j+2].path;
           if(slash) *slash='\0';
           absolute=realpath(slash?(*parent?parent:"/"):".",NULL);
           trial_path=absolute?THD_zzprintf(NULL,"%s/%s",absolute,leaf):NULL;
           free(absolute); free(parent); }
         if(!selector || !trial_path) ok=xb_error(err,"cannot resolve runwise output paths");
         else {
            THD_1D_print_token(f,o->subject); fprintf(f," %d ",runs[j]);
            THD_1D_print_token(f,trial_path); fputc(' ',f); THD_1D_print_token(f,selector); fputc('\n',f);
         }
         free(trial_path); free(selector);
      }
   }
   if(ok) ok=xb_commit(out,n,err);
   if(ok) fprintf(stderr,"++ Exported beta tables -> %s\n",out[0].path);
   xb_free_outputs(out,n,ok); free(runs);
   return ok;
}

static int xt_export_betas(const XT_beta_options *o)
{
   XB_table b={0}; char err[1024]="",*manifest=NULL; int ok=0;
   const unsigned char *s=(const unsigned char *)o->subject;
   if(!s || !*s) return fprintf(stderr,"** beta export requires a nonempty -subject\n"),1;
   for(;*s;s++) if(*s<32 || *s==127) return fprintf(stderr,"** control character in -subject\n"),1;
   for(s=(const unsigned char *)o->prefix;*s;s++)
      if(*s<32 || *s==127) return fprintf(stderr,"** control character in -prefix\n"),1;
   manifest=realpath(o->manifest,NULL);
   if(!manifest) xb_error(err,"cannot resolve beta manifest %s",o->manifest);
   else {
      if(o->input_base) b.base=realpath(o->input_base,NULL);
      else { b.base=strdup(manifest);
             if(b.base) { char *slash=strrchr(b.base,'/');
                if(slash==b.base) slash[1]='\0'; else *slash='\0'; } }
      if(!b.base) xb_error(err,"cannot resolve -input_base");
      else {
         struct stat st;
         if(stat(b.base,&st) || !S_ISDIR(st.st_mode)) xb_error(err,"input base must be a directory");
         else {
            b.table=THD_read_datatable_file(manifest);
            if(o->events) b.events=THD_read_datatable_file((char *)o->events);
            b.nrow=b.table->nrow; b.row=calloc((size_t)b.nrow,sizeof(*b.row));
            if(!b.row) xb_error(err,"out of memory");
            else ok=xb_check_table(b.table,err) && (!b.events || xb_check_table(b.events,err)) &&
                    xb_paths(&b,err) && xb_rows(&b,o,err) && xb_select(&b,o->runs,err) && xb_write(&b,o,err);
         }
      }
   }
   if(!ok) fprintf(stderr,"** beta export: %s\n",*err?err:"invalid input");
   free(b.row); THD_free_datatable(b.table); THD_free_datatable(b.events);
   free(b.base); free(manifest); return !ok;
}

/* Validate operation combinations before acquiring input or output resources. */
static const char *xt_options_error(const XT_options *ops,const char *in,
   const char *unc,const char *prefix,const char *runlist,int split)
{
   if(ops->show_trs && (!ops->tr_threshold_set || ops->groups || prefix ||
      ((ops->columns!=NULL)+(ops->labels!=NULL)+(ops->stimuli!=NULL)!=1)))
      return "-show_trs requires one threshold and one column/stimulus selection; no -prefix or -select_groups";
   if(!ops->show_trs && (ops->tr_threshold_set || ops->tr_window))return "TR threshold/window options require -show_trs";
   /* Beta sidecars are a separate input route; do not demand an unrelated
    * design matrix for a voxelwise mixture of candidate designs. */
   if(ops->beta_manifest) {
      if(!ops->manifest || !ops->subject || !prefix || !*prefix)
         return "-beta_manifest requires -export_manifest -subject -prefix";
      if(in || unc || split || ops->show_trs || ops->describe || ops->validate ||
         ops->select || ops->compare || ops->beta || ops->beta_pattern ||
         ops->drop_glts || ops->json || ops->for_tool)
         return "-beta_manifest cannot accompany matrix operations or -beta/-beta_pattern";
      if(ops->manifest_format && strcmp(ops->manifest_format,"manifest") &&
         strcmp(ops->manifest_format,"selected") && strcmp(ops->manifest_format,"rsa") &&
         strcmp(ops->manifest_format,"rsa_runwise"))
         return "beta manifest format must be manifest, selected, rsa or rsa_runwise";
      return NULL;
   }
   if(ops->input_base || ops->manifest_run_base_set)
      return "-input_base/-manifest_run_base require -beta_manifest";
   if(!in) return "requires -infile (see -help)";
   if(ops->show_trs+split+ops->select+ops->manifest+(ops->compare!=NULL)+((ops->describe||ops->validate)!=0)!=1)
      return "choose one operation: split, describe/validate, select, compare, export_manifest, or show_trs";
   if(ops->for_tool && strcmp(ops->for_tool,"lss") && strcmp(ops->for_tool,"reml"))return "-for must be lss or reml";
   if(ops->manifest_format && strcmp(ops->manifest_format,"selected") && strcmp(ops->manifest_format,"manifest"))return "-manifest_format must be manifest or selected";
   if(ops->json && !(ops->describe||ops->validate))return "-json is available with describe/validate";
   if(runlist && !split && !ops->show_trs)return "-runs applies to -split_runs or -show_trs";
   if(prefix && !*prefix)return "-prefix cannot be empty";
   if(ops->for_tool && !(ops->describe||ops->validate))return "-for applies to describe/validate";
   if(ops->drop_glts && !ops->select)return "-drop_glts applies to column selection";
   if((ops->events || ops->subject || ops->beta || ops->beta_pattern || ops->manifest_format) && !ops->manifest)
      return "event/beta options apply to -export_manifest";
   if(unc && (ops->select || ops->compare))return "-uncensored applies to split, describe/validate and export_manifest";
   return NULL;
}

/* Resources owned by one split operation, including only outputs we created. */
typedef struct {
   THD_xmat **x;
   THD_xmat_run_map **maps;
   NI_int_array *runs;
   char **paths,*summary;
   FILE *fp;
   int n,done,summary_owned;
} XT_split;

static int xt_error(char *err,int nerr,const char *message)
{
   snprintf(err,nerr,"%s",message);return 1;
}

/* Prepare and write run subsets. The caller releases resources and rolls back
   completed files on failure; done tracks precisely which paths we own. */
static int xt_split_write(const THD_xmat *a,const THD_xmat *u,
   const char *prefix,const char *runlist,double tol,XT_split *out,
   char *err,int nerr)
{
   int i,j,fd;struct stat st;
   if(!prefix || !*prefix)return xt_error(err,nerr,"split requires -prefix");
   if(runlist){
      out->runs=parse_runs(runlist,a->nrun);
      if(!out->runs || out->runs->num<1) return xt_error(err,nerr,"invalid -runs list: use unique run numbers in range, e.g. 1,3..5");
   }else{
      out->runs=calloc(1,sizeof(*out->runs));
      if(!out->runs)return xt_error(err,nerr,"out of memory");
      out->runs->num=a->nrun;
      out->runs->ar=malloc(sizeof(int)*(size_t)a->nrun);
      if(!out->runs->ar)return xt_error(err,nerr,"out of memory");
      for(i=0;i<a->nrun;i++) out->runs->ar[i]=i+1;
   }
   out->n=out->runs->num;
   out->x=calloc((size_t)out->n,sizeof(*out->x)); out->maps=calloc((size_t)out->n,sizeof(*out->maps));
   out->paths=calloc((size_t)out->n*3,sizeof(*out->paths)); out->summary=malloc(strlen(prefix)+16);
   if(!out->x || !out->maps || !out->paths || !out->summary) return xt_error(err,nerr,"out of memory");
   sprintf(out->summary,"%s.runs.1D",prefix);
   if(!u && a->nrow<a->nfull)
      fprintf(stderr,"# WARNING: support inferred from retained rows only; use -uncensored for pre-censor support.\n");
   for(i=0;i<out->n;i++){
      if(out->runs->ar[i]<1 || out->runs->ar[i]>a->nrun) return xt_error(err,nerr,"-runs index outside the input run range");
      for(j=0;j<i;j++) if(out->runs->ar[i]==out->runs->ar[j]) return xt_error(err,nerr,"duplicate run in -runs");
      out->x[i]=THD_xmat_extract_run(a,u,out->runs->ar[i]-1,tol,out->maps+i,err,nerr);
      if(!out->x[i]) return 1;
      out->paths[3*i]=outname(prefix,out->runs->ar[i],"xmat.1D");
      out->paths[3*i+1]=outname(prefix,out->runs->ar[i],"rows.1D");
      out->paths[3*i+2]=outname(prefix,out->runs->ar[i],"columns.1D");
      if(!out->paths[3*i] || !out->paths[3*i+1] || !out->paths[3*i+2]) return xt_error(err,nerr,"out of memory");
   }
   for(i=0;i<=3*out->n;i++){
      const char *p=i==3*out->n?out->summary:out->paths[i];
      if(!lstat(p,&st)){snprintf(err,nerr,"output already exists: %s",p);return 1;}
      if(errno!=ENOENT){snprintf(err,nerr,"cannot access output path: %s",p);return 1;}
   }
   fd=open(out->summary,O_WRONLY|O_CREAT|O_EXCL,0666);
   if(fd<0) return xt_error(err,nerr,"cannot create summary (prefix directory must exist)");
   out->summary_owned=1; out->fp=fdopen(fd,"w");
   if(!out->fp){close(fd);return xt_error(err,nerr,"cannot open summary stream");}
   fputs("# Columns: Run Start Length NKept NColumns\n",out->fp);
   for(i=0;i<out->n;i++){
      if(!THD_xmat_write(out->x[i],out->paths[3*i],err,nerr)) return 1;
      out->done=3*i+1;
      if(!THD_xmat_write_run_map(a,out->x[i],out->maps[i],out->paths[3*i+1],out->paths[3*i+2],err,nerr)) return 1;
      out->done=3*i+3;
      fprintf(out->fp,"# RunFiles %d ",out->runs->ar[i]);
      comment_text(out->fp,out->paths[3*i]); fputc(' ',out->fp);comment_text(out->fp,out->paths[3*i+1]);fputc(' ',out->fp);comment_text(out->fp,out->paths[3*i+2]);fputc('\n',out->fp);
      fprintf(out->fp,"%d %d %d %d %d\n",out->runs->ar[i],out->maps[i]->start,out->x[i]->nfull,out->x[i]->nrow,out->x[i]->ncol);
   }
   j=ferror(out->fp); if(fclose(out->fp)) j=1; out->fp=NULL;
   if(j) return xt_error(err,nerr,"failed writing summary");
   for(i=0;i<out->n;i++) fprintf(stderr,"++ run %d: %d/%d rows, %d/%d columns -> %s\n",
      out->runs->ar[i],out->x[i]->nrow,out->x[i]->nfull,out->x[i]->ncol,a->ncol,out->paths[3*i]);
   fprintf(stderr,"++ Wrote %d run(s); provenance: %s\n",out->n,out->summary); return 0;
}

/* Release split storage once, removing only newly created outputs on failure. */
static int xt_split_runs(const THD_xmat *a,const THD_xmat *u,
   const char *prefix,const char *runlist,double tol)
{
   XT_split out={0};char err[1024]="";int i;
   int rc=xt_split_write(a,u,prefix,runlist,tol,&out,err,sizeof(err));
   if(out.fp)fclose(out.fp);
   if(rc){
      fprintf(stderr,"** 1dXmatTool: %s\n",err);
      for(i=0;i<out.done;i++)unlink(out.paths[i]);
      if(out.summary_owned)unlink(out.summary);
   }
   for(i=0;i<out.n;i++){
      if(out.x)THD_xmat_free(out.x[i]);
      if(out.maps)THD_xmat_run_map_free(out.maps[i]);
   }
   if(out.paths){for(i=0;i<3*out.n;i++)free(out.paths[i]);free(out.paths);}
   free(out.x);free(out.maps);free(out.summary);
   if(out.runs)NI_delete_int_array(out.runs);
   return rc;
}

int main(int argc,char **argv)
{
   XT_options ops={0};
   const char *in=NULL,*unc=NULL,*prefix=NULL,*runlist=NULL;
   THD_xmat *a=NULL,*u=NULL;char err[1024]="";const char *problem;
   double tol=THD_XMAT_SPLIT_TOL;int split=0,i,rc=1;
   ops.rank_tol=1.e-7;ops.atol=1.e-6;ops.rtol=1.e-6;
   if(argc==1){usage();return 0;}
   /* Handle argument-free switches first; all remaining branches consume a
    * value. Options store borrowed argv pointers for the lifetime of main. */
   for(i=1;i<argc;i++){
      if(!strcmp(argv[i],"-help") || !strcmp(argv[i],"--help")){usage();return 0;}
      if(!strcmp(argv[i],"-describe") || !strcmp(argv[i],"-show")){ops.describe=1;continue;}
      if(!strcmp(argv[i],"-show_trs")){ops.show_trs=1;continue;}
      if(!strcmp(argv[i],"-force_inferred_metadata")){ops.force_inferred_metadata=1;continue;}
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
      else if(!strcmp(argv[i],"-beta_manifest")) ops.beta_manifest=argv[++i];
      else if(!strcmp(argv[i],"-input_base")) ops.input_base=argv[++i];
      else if(!strcmp(argv[i],"-manifest_run_base")) {
         const char *v=argv[++i];
         if(strcmp(v,"0") && strcmp(v,"1")) { fprintf(stderr,"** -manifest_run_base must be 0 or 1\n"); return 1; }
         ops.manifest_run_base=atoi(v); ops.manifest_run_base_set=1;
      }
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
      else if(!strcmp(argv[i],"-warn_support_frac")){
         const char *v=argv[++i];char *end;double d;errno=0;d=strtod(v,&end);
         if(errno || end==v || *end || !isfinite(d) || d<0 || d>1){
            fprintf(stderr,"** -warn_support_frac must be in [0,1]\n");return 1;
         }
         ops.warn_support_frac=d;ops.warn_support_set=1;
      }
      else if(!strcmp(argv[i],"-zero_tol")){
         char *end; const char *v=argv[++i]; errno=0; tol=strtod(v,&end);
         if(errno || end==v || *end || !isfinite(tol) || tol<0 || tol>=1){
            fprintf(stderr,"** -zero_tol must be a number in [0,1)\n");return 1;
         }
      }else{fprintf(stderr,"** unknown option: %s\n",argv[i]);return 1;}
   }
   /* In a TR query the selectors identify the ideal, not a second operation. */
   ops.select=!ops.show_trs && (ops.columns || ops.labels || ops.stimuli || ops.groups);
   if(ops.force_inferred_metadata && !(ops.describe || ops.validate)){
      fprintf(stderr,"** -force_inferred_metadata requires -describe or -validate\n");return 1;
   }
   if(ops.warn_support_set && !(ops.describe || ops.validate)){
      fprintf(stderr,"** -warn_support_frac requires -describe or -validate\n");return 1;
   }
   problem=xt_options_error(&ops,in,unc,prefix,runlist,split);
   if(problem){fprintf(stderr,"** 1dXmatTool: %s\n",problem);return 1;}
   if(ops.beta_manifest) {
      XT_beta_options beta_options={ops.beta_manifest,ops.events,ops.subject,prefix,
         ops.manifest_format?ops.manifest_format:"manifest",ops.input_base,runlist,ops.manifest_run_base};
      return xt_export_betas(&beta_options);
   }
   a=THD_xmat_read(in,err,sizeof(err));
   if(a && unc)u=THD_xmat_read(unc,err,sizeof(err));
   if(a && !unc && a->nrow<a->nfull && (ops.describe || ops.validate || ops.manifest))
      fprintf(stderr,"# WARNING: support is inferred only from retained rows; supply -uncensored for pre-censor support.\n");
   if(!a || (unc && !u))fprintf(stderr,"** 1dXmatTool: %s\n",err);
   else if(split)rc=xt_split_runs(a,u,prefix,runlist,tol);
   else if(ops.show_trs)rc=xt_show_trs(a,u,&ops,runlist);
   else if(ops.describe||ops.validate)rc=xt_report(a,u,&ops,tol);
   else if(ops.compare)rc=xt_compare(a,&ops);
   else if(ops.select)rc=xt_select(a,&ops,prefix);
   else if(ops.manifest)rc=xt_manifest(a,u,&ops,prefix,tol);
   THD_xmat_free(a);THD_xmat_free(u);
   return rc;
}
