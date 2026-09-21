#include "mrilib.h"
#include "thd_datatable.h"
#include "thd_insync.h"
#include "thd_mapinfer.h"
#include "thd_patterns.h"
#include "thd_phasefft.h"
#include "thd_permute.h"
#include "thd_simmatrix.h"

#include <float.h>
#include <errno.h>
#include <limits.h>

#ifdef USE_OMP
#include <omp.h>
#endif

#define PROGRAM_NAME "3dInSync"
#define METHOD_PAIRWISE 1
#define METHOD_LOO      2
#define TNULL_NONE      0
#define TNULL_TIMESHIFT 1
#define TNULL_PHASE     2
#define MISSING_ERROR   0
#define MISSING_COMMON  1
#define PROGRESS_AUTO   0
#define PROGRESS_BAR    1
#define PROGRESS_LINE   2
#define PROGRESS_OFF    3
#define INSYNC_TAIL_BI  2

static char *insync_options[] = {
   "-prefix", "-mask", "-isc_method", "-summary", "-group_column",
   "-nboot", "-boot_alpha", "-nperm", "-exact", "-seed", "-tail",
   "-1sided", "-2sided", "-bisided",
   "-temporal_null", "-nnull", "-min_shift", "-temporal_tail",
   "-condition_column", "-condition_contrast", "-ncondperm",
   "-condition_exact", "-condition_tail", "-save_pairwise",
   "-correlation", "-censor", "-missing", "-atlas", "-roi_sel",
   "-save_matrix", "-memory_limit", "-memory_override", "-quiet", "-progress",
   "-dataTable", "-dataTableFile", "-show_table", "-help", "-h", NULL
} ;

/*! Print the complete command-line help and AFNI/OpenMP build information. */
static void usage_3dInSync(void)
{
   printf(
"\n"
"Usage: 3dInSync [options]                                      ~1~\n"
"\n"
"Compute voxelwise intersubject correlation (ISC) directly from aligned\n"
"subject time-series datasets. Subject resampling, temporal-null inference,\n"
"paired condition contrasts, and a 3dISC pair-map bridge are available.\n"
"\n"
"The input datasets must have the same spatial grid and number of time\n"
"points.  They should already be preprocessed, censored consistently, and\n"
"aligned in stimulus time.  3dInSync does not regress nuisance signals or\n"
"alter temporal alignment.\n"
"\n"
"Required input:                                             ~1~\n"
"\n"
"  -dataTable @TABLE\n"
"  -dataTableFile TABLE\n"
"  -dataTable Subj Group InputFile s01 control s01+tlrc ...\n"
"\n"
"      TABLE requires an InputFile column. A short table has one row per\n"
"      subject. A long table has one row per Subj x Condition cell, with every\n"
"      cell present exactly once. If a Group column is present, one\n"
"      ISC map is produced per group; otherwise all rows form one group.\n"
"      Exactly two groups also produce a first-minus-second difference map.\n"
"      Group order is order of first appearance in TABLE.\n"
"\n"
"Options:                                                    ~1~\n"
"\n"
"  -prefix PREFIX       Output dataset prefix [InSync].\n"
"\n"
"  -mask MASK           Analyze nonzero voxels in MASK.  Without -mask, all\n"
"                       voxels are analyzed.\n"
"\n"
"  -isc_method METHOD   pairwise : correlate every subject pair [default]\n"
"                       loo      : correlate each subject with the mean of\n"
"                                  the other subjects in the same group\n"
"\n"
"  -summary METHOD      median      : median ISC [default]\n"
"                       fisher_mean : mean in Fisher-z space, returned as r\n"
"\n"
"  -correlation METHOD  pearson  : ordinary Pearson correlation [default]\n"
"                       spearman : average-rank Spearman correlation. This is\n"
"                                  robust to monotone amplitude distortions;\n"
"                                  -summary median remains the robust default\n"
"                                  across subject pairs.\n"
"\n"
"  -censor FILE         One common 0/1 censor vector with one value per input\n"
"                       time point. Retained time points are identical for all\n"
"                       subjects, conditions, voxels, and ROIs. At least three\n"
"                       must remain. Censoring cannot be combined with temporal\n"
"                       nulls, whose circular timeline would become ambiguous.\n"
"\n"
"  -missing POLICY      error  : reject nonfinite retained input data [default]\n"
"                       common : at each voxel/ROI, drop a time point from all\n"
"                                subjects and conditions if any input is\n"
"                                nonfinite there. Adds a ValidTR output map and\n"
"                                cannot be combined with temporal nulls.\n"
"\n"
"  -atlas ATLAS         Also compute ISC from each atlas parcel's mean time\n"
"                       series and write PREFIX.roi.1D. Positive integer atlas\n"
"                       values define ROIs; -mask, when present, intersects them.\n"
"                       These are descriptive ROI estimates and matrices; the\n"
"                       voxelwise resampling maps are not averaged or repainted.\n"
"\n"
"  -roi_sel LIST        Restrict -atlas to an AFNI integer label list.\n"
"\n"
"  -save_matrix FILE    With -atlas, save long-form ROI subject matrices with\n"
"                       Condition, ROI, Subj1, Subj2, groups, and correlation.\n"
"\n"
"  -memory_limit G      Refuse an estimated peak above G GiB. Without this\n"
"                       option the limit is 80%% of detected physical RAM.\n"
"\n"
"  -memory_override     Continue after the memory estimate exceeds its limit.\n"
"\n"
"  -progress M          auto, bar, line, or off [auto].\n"
"  -quiet               Suppress progress and informational diagnostics.\n"
"\n"
"  -group_column NAME   Data-table group column [Group].  If NAME is absent,\n"
"                       all subjects are analyzed together.\n"
"\n"
"  -condition_column NAME\n"
"                       Long-table condition column [Condition]. If present,\n"
"                       every Subj x Condition cell is required exactly once.\n"
"\n"
"  -condition_contrast C1 C2\n"
"                       Report ISC(C1)-ISC(C2) in every group and, for two\n"
"                       groups, G1(C1-C2)-G2(C1-C2).\n"
"\n"
"  -ncondperm P         Use P synchronized within-subject C1/C2 swaps for\n"
"                       condition inference. P must be at least 20.\n"
"\n"
"  -condition_exact     Enumerate all condition swaps when feasible; otherwise\n"
"                       sample -ncondperm P swaps [default 10000].\n"
"\n"
"  -condition_tail two|one\n"
"                       Two-sided or upper-tailed condition tests [two].\n"
"\n"
"  -save_pairwise PREFIX\n"
"                       Write a Fisher-z pair-map bucket. A short table also\n"
"                       gets PREFIX.3dISC.txt. A repeated-condition table gets\n"
"                       one runnable PREFIX.C1__C2.3dISC.txt per ordered\n"
"                       condition pairing plus PREFIX.3dISC.all.txt. Splitting\n"
"                       is required by current 3dISC's one-row-per-pair rule.\n"
"\n"
"  -nboot B             Draw B subject-bootstrap samples independently within\n"
"                       each group and output percentile confidence limits.\n"
"                       B must be at least 100. [no bootstrap]\n"
"\n"
"  -boot_alpha A        Two-sided bootstrap interval error rate [0.05], giving\n"
"                       the 100*(1-A) percent interval.\n"
"\n"
"  -nperm P             For exactly two independent groups, use P synchronized\n"
"                       subject-label permutations.  Small permutation groups\n"
"                       are enumerated exactly when P reaches their size.\n"
"                       [no permutation inference]\n"
"\n"
"  -exact               Request exact enumeration of group assignments when\n"
"                       feasible.  If the exact group is above the library cap,\n"
"                       sample -nperm P assignments instead (default 10000).\n"
"\n"
"  -seed S              Reproducible permutation/bootstrap seed [1234567].\n"
"\n"
"  -tail 1sided|2sided|bisided\n"
"                       Sidedness for the group-difference permutation test\n"
"                       [2sided]. The tested effect is first group minus\n"
"                       second group. 1sided tests the positive direction.\n"
"                       2sided uses one pooled absolute-value null. bisided\n"
"                       corrects positive and negative tails against separate\n"
"                       max-statistic nulls and reports doubled tail p-values.\n"
"                       The older one/two spellings remain accepted.\n"
"\n"
"  -1sided             Short form of -tail 1sided.\n"
"  -2sided             Short form of -tail 2sided.\n"
"  -bisided            Short form of -tail bisided.\n"
"\n"
"  -temporal_null TYPE  Test each group's ISC against disrupted temporal\n"
"                       alignment.  TYPE is one of:\n"
"                         timeshift : independent circular subject shifts\n"
"                         phase     : independent Fourier phase randomization\n"
"                       This is distinct from the -nperm group-difference test.\n"
"\n"
"  -nnull N             Number of synchronized temporal-null sets, including\n"
"                       identity set 0.  N must be at least 20.\n"
"\n"
"  -min_shift K         For timeshift, require circular distance of at least K\n"
"                       time points from zero [1].  Choose K from the temporal\n"
"                       autocorrelation scale.  Identity set 0 is retained for\n"
"                       empirical p-values; every random set obeys K.\n"
"\n"
"  -temporal_tail one|two\n"
"                       Upper-tailed positive ISC [one] or two-sided temporal\n"
"                       inference.\n"
"\n"
"  -show_table          Print the parsed data table before analysis.\n"
"\n"
"Output:                                                     ~1~\n"
"\n"
"  Each condition/group contributes an ISC_CONDITION_GROUP brick and an\n"
"  N_CONDITION_GROUP brick (condition is omitted for a short table). N is the\n"
"  number of finite subject-pair correlations (pairwise) or subject ISC\n"
"  values (loo) at that voxel.  With two groups, ISC_A-B is also written.\n"
"  Voxels with no valid estimate are stored as zero with N=0.\n"
"\n"
"  -nboot adds BootLo_LABEL and BootHi_LABEL maps.  Duplicate copies of one\n"
"  subject do not create self-pairs in pairwise ISC.\n"
"\n"
"  -nperm adds P, Z, Q, PFWE, and ZFWE maps for ISC_A-B per condition. Q and\n"
"  PFWE cover one synchronized condition x in-mask family. Z and ZFWE are\n"
"  signed normal equivalents of P and PFWE.  Subjects are permuted as whole\n"
"  rows and columns of the pooled matrix; pairwise edges are never shuffled.\n"
"\n"
"  -temporal_null adds TP, TZ, TQ, TPFWE, and TZFWE per condition/group. TQ\n"
"  and max-FWE jointly cover condition x group x in-mask voxel. The\n"
"  same seeded surrogate set is reused everywhere.  Timeshift preserves each\n"
"  complete time series; phase preserves its mean and power spectrum.\n"
"\n"
"Example:                                                    ~1~\n"
"\n"
"  3dInSync -prefix movie_isc -mask brain+tlrc \134\n"
"            -isc_method pairwise -summary median \134\n"
"            -nboot 2000 -nperm 10000 -seed 314159 \134\n"
"            -temporal_null timeshift -nnull 5000 -min_shift 10 \134\n"
"            -dataTableFile subjects.txt\n"
"\n"
"  # subjects.txt\n"
"  Subj  Group    InputFile\n"
"  s01   control  s01_movie_scale+tlrc\n"
"  s02   control  s02_movie_scale+tlrc\n"
"  s21   patient  s21_movie_scale+tlrc\n"
"  s22   patient  s22_movie_scale+tlrc\n"
"\n"
"Inference scope:                                          ~1~\n"
"\n"
"  Bootstrap intervals quantify subject-sampling uncertainty within groups.\n"
"  Label permutation tests an independent two-group difference under subject\n"
"  exchangeability.  Temporal nulls instead test whether within-group ISC\n"
"  exceeds chance temporal alignment; they do not replace label permutation\n"
"  for a group difference. Condition inference swaps labels within subjects\n"
"  and never breaks pairing. Pair-map export uses Fisher z for 3dISC.\n"
"\n"
"Scope boundary:                                            ~1~\n"
"\n"
"  This program's analysis is static ISC. Dynamic/windowed ISC, ISFC, and ISPS\n"
"  require distinct estimands, outputs, and null models and are not accepted as\n"
"  aliases here. They remain candidates for explicit future modes.\n"
"\n") ;
   PRINT_AFNI_OMP_USAGE(PROGRAM_NAME,NULL) ;
   PRINT_COMPILE_DATE ;
}

/*! Append one newly encountered group label to the owned label array.
    The caller is responsible for eventually freeing the duplicated string. */
static void insync_add_group( char *label, char ***labels, int *nlabel )
{
   *labels=(char **)realloc(*labels,sizeof(char *)*(*nlabel+1)) ;
   (*labels)[*nlabel]=strdup(label) ; (*nlabel)++ ;
}

/*! Return the zero-based index of label, or -1 when it has not been seen. */
static int insync_find_group( char *label, char **labels, int nlabel )
{
   int ii ;
   for( ii=0 ; ii<nlabel ; ii++ ) if( strcmp(label,labels[ii])==0 ) return ii ;
   return -1 ;
}

/*! Build an AFNI-safe sub-brick label from a statistic stem and effect label. */
static void insync_brick_label( char *dst, size_t ndst, char *stem, char *label )
{
   size_t ii,jj=0 ; char tmp[128] ;
   snprintf(tmp,sizeof(tmp),"%s_%s",stem,label) ;
   for( ii=0 ; tmp[ii]!='\0' && jj+1<ndst ; ii++ ){
     unsigned char cc=(unsigned char)tmp[ii] ;
     dst[jj++]=(isalnum(cc) || cc=='_' || cc=='-' || cc=='.') ? (char)cc : '_' ;
   }
   dst[jj]='\0' ;
}

/*! Copy src into a whitespace-free token suitable for labels and filenames. */
static void insync_safe_token( char *dst, size_t ndst, const char *src )
{
   size_t ii,jj=0 ;
   if( ndst<1 ) return ;
   for( ii=0 ; src!=NULL && src[ii]!='\0' && jj+1<ndst ; ii++ ){
     unsigned char cc=(unsigned char)src[ii] ;
     dst[jj++]=(isalnum(cc) || cc=='_' || cc=='-' || cc=='.') ? (char)cc : '_' ;
   }
   dst[jj]='\0' ;
}

/*! Convert a correlation to Fisher z after clipping exact endpoint values. */
static float insync_fisher_z( float rr )
{
   if( !isfinite(rr) ) return 0.0f ;
   if( rr>=1.0f ) rr=0.999999f ; else if( rr<=-1.0f ) rr=-0.999999f ;
   return atanhf(rr) ;
}

/*! Write the 3dISC-compatible pair-table header, including user covariates. */
static void insync_3disc_header( FILE *fp, THD_datatable *tab,
                                 int icol_group, int icol_condition )
{
   int jj ;
   fprintf(fp,"Subj1 Subj2 Group1 Group2 GroupPair cond cond1 cond2") ;
   for( jj=0 ; jj<tab->ncol ; jj++ )
     if( jj!=tab->icol_subj && jj!=tab->icol_input &&
         jj!=icol_group && jj!=icol_condition )
       fprintf(fp," %s1 %s2",tab->cname[jj],tab->cname[jj]) ;
   fprintf(fp," InputFile\n") ;
}

/*! Write one subject-pair/condition-pair row pointing at a pair-map brick.
    Subject-level covariates are repeated with 1/2 suffixes for 3dISC. */
static void insync_3disc_row( FILE *fp, THD_datatable *tab,
                              THD_datatable_index *cindex,
                              int *rowmap, int ncond, char **clabel,
                              int *group, char **glabel,
                              int c1, int c2, int s1, int s2,
                              int icol_group, const char *dataset_name,
                              size_t brick )
{
   const char *a=cindex?cindex->level[0][s1]:tab->subj[s1] ;
   const char *b=cindex?cindex->level[0][s2]:tab->subj[s2] ;
   int ra=rowmap[s1*ncond],rb=rowmap[s2*ncond],jj ;
   int icol_condition=cindex?cindex->icol[1]:-1 ;
   char gp[256],cp[256] ;
   snprintf(gp,sizeof(gp),"%s__%s",glabel[group[s1]],glabel[group[s2]]) ;
   if( c1<=c2 ) snprintf(cp,sizeof(cp),"%s__%s",clabel[c1],clabel[c2]) ;
   else         snprintf(cp,sizeof(cp),"%s__%s",clabel[c2],clabel[c1]) ;
   fprintf(fp,"%s %s %s %s %s %s %s %s",a,b,glabel[group[s1]],
           glabel[group[s2]],gp,cp,clabel[c1],clabel[c2]) ;
   for( jj=0 ; jj<tab->ncol ; jj++ )
     if( jj!=tab->icol_subj && jj!=tab->icol_input &&
         jj!=icol_group && jj!=icol_condition )
       fprintf(fp," %s %s",DT_CELL(tab,ra,jj),DT_CELL(tab,rb,jj)) ;
   fprintf(fp," %s[%lu]\n",dataset_name,(unsigned long)brick) ;
}

/*! Thread-safe progress state shared by the OpenMP voxel workers. */
typedef struct {
   int enabled,mode,is_tty,total,done,last_bucket ;
   double start,last_report ;
} INSYNC_progress ;

/*! Initialize progress rendering and choose bar versus line output for auto. */
static void insync_progress_init( INSYNC_progress *p, int mode, int quiet,
                                  int total )
{
   memset(p,0,sizeof(*p)) ; p->enabled=!quiet && mode!=PROGRESS_OFF && total>0 ;
   p->is_tty=isatty(fileno(stderr)) ; p->total=total ;
   p->mode=(mode==PROGRESS_AUTO)?(p->is_tty?PROGRESS_BAR:PROGRESS_LINE):mode ;
   p->start=p->last_report=0.001*(double)NI_clock_time() ;
   if( !p->enabled ) return ;
   if( p->mode==PROGRESS_BAR ){
     fprintf(stderr,"++ 3dInSync [--------------------] 0/%d",total) ;
     fflush(stderr) ;
   } else INFO_message(PROGRAM_NAME ": inference 0/%d voxels complete",total) ;
}

/*! Atomically count a completed voxel and occasionally render an update. */
static void insync_progress_advance( INSYNC_progress *p )
{
   int done,bucket ; double now,elapsed,rate ;
   if( !p->enabled ) return ;
#ifdef USE_OMP
#pragma omp atomic capture
   done=++p->done ;
#else
   done=++p->done ;
#endif
#ifdef USE_OMP
#pragma omp critical(insync_progress_render)
#endif
   {
     now=0.001*(double)NI_clock_time() ; bucket=(10*done)/p->total ;
     if( done==p->total || (p->mode==PROGRESS_BAR && now-p->last_report>=1.0) ||
         (p->mode==PROGRESS_LINE && bucket>p->last_bucket) ){
       int ii,fill=(20*done)/p->total ;
       elapsed=now-p->start ; rate=(elapsed>0.0)?done/elapsed:0.0 ;
       if( p->mode==PROGRESS_BAR ){
         fprintf(stderr,"\r++ 3dInSync [") ;
         for( ii=0 ; ii<20 ; ii++ ) fputc(ii<fill?'#':'-',stderr) ;
         fprintf(stderr,"] %d/%d (%.1f%%), %.2f voxels/s",done,p->total,
                 100.0*done/p->total,rate) ;
         if( p->is_tty ) fputs("\033[K",stderr) ;
         if( done==p->total ) fputc('\n',stderr) ;
         fflush(stderr) ;
       } else INFO_message(PROGRAM_NAME ": inference %d/%d voxels complete "
                           "(%.1f%%, %.2f voxels/s)",done,p->total,
                           100.0*done/p->total,rate) ;
       p->last_report=now ; p->last_bucket=bucket ;
     }
   }
}

/*! Average input data within each selected atlas parcel and write descriptive
    group ISC summaries plus, when requested, subject correlation matrices.
    Censoring, missing-data policy, correlation metric, and mask intersection
    are kept identical to the voxelwise analysis. */
static void insync_write_roi_outputs(
   const char *atlas_name, const char *roi_sel, const char *prefix,
   const char *matrix_name, THD_3dim_dataset *first,
   THD_3dim_dataset **dset, int nsub, int ncond,
   int ntime_input, int ntime, int *time_index, byte *mask,
   int missing_policy, int corr_metric, int method, int summary,
   int ngroup, int *group, int *gcount, int **gmember, char **glabel,
   THD_datatable *tab, THD_datatable_index *cindex, int *rowmap,
   char **clabel, int quiet )
{
   THD_3dim_dataset *aset ; THD_roilist *rl ; THD_simmat *sm ;
   FILE *sfp,*mfp=NULL ; char sname[THD_MAX_NAME] ;
   int nvox,ndset=nsub*ncond,rr,cc,ss,tt,gg,aa,bb,vv,nwritten=0 ;
   size_t nedge=(size_t)nsub*(nsub-1)/2,nscratch=(nedge>(size_t)nsub)?nedge:(size_t)nsub ;
   float *data,*ref,*values,*scratch,*rank1=NULL,*rank2=NULL ;
   byte *goodtime=(missing_policy==MISSING_COMMON)?(byte *)malloc((size_t)ntime):NULL ;

   aset=THD_open_dataset((char *)atlas_name) ; CHECK_OPEN_ERROR(aset,atlas_name) ;
   DSET_load(aset) ; CHECK_LOAD_ERROR(aset) ;
   if( !EQUIV_GRIDS(first,aset) )
     ERROR_exit(PROGRAM_NAME ": -atlas is not on the input grid") ;
   nvox=DSET_NVOX(aset) ;
   for( vv=0 ; vv<nvox ; vv++ ){
     float x=THD_get_voxel(aset,vv,0) ;
     if( !isfinite(x) || (x>0.0f && x!=floorf(x)) )
       ERROR_exit(PROGRAM_NAME ": atlas '%s' has invalid label %.7g at voxel %d; "
                  "positive labels must be finite integers",atlas_name,x,vv) ;
   }
   rl=THD_roilist_from_dset(aset,(char *)roi_sel) ;
   if( rl==NULL ) ERROR_exit(PROGRAM_NAME ": -atlas/-roi_sel selected no positive ROIs") ;
   snprintf(sname,sizeof(sname),"%s.roi.1D",prefix) ;
   if( !THD_ok_overwrite() && THD_is_file(sname) )
     ERROR_exit(PROGRAM_NAME ": ROI summary '%s' already exists",sname) ;
   sfp=fopen(sname,"w") ;
   if( sfp==NULL ) ERROR_exit(PROGRAM_NAME ": cannot write ROI summary '%s'",sname) ;
   if( matrix_name!=NULL ){
     if( !THD_ok_overwrite() && THD_is_file((char *)matrix_name) )
       ERROR_exit(PROGRAM_NAME ": ROI matrix '%s' already exists",matrix_name) ;
     mfp=fopen(matrix_name,"w") ;
     if( mfp==NULL ) ERROR_exit(PROGRAM_NAME ": cannot write ROI matrix '%s'",matrix_name) ;
   }
   fprintf(sfp,"Condition ROI ROI_Label Group ISC N ValidTR NVoxel Correlation Estimator Summary\n") ;
   if( mfp!=NULL )
     fprintf(mfp,"Condition ROI ROI_Label Subj1 Subj2 Group1 Group2 Correlation\n") ;

   data=(float *)malloc(sizeof(float)*(size_t)ndset*ntime) ;
   ref=(float *)malloc(sizeof(float)*(size_t)ntime) ;
   values=(float *)malloc(sizeof(float)*(size_t)nsub) ;
   scratch=(float *)malloc(sizeof(float)*nscratch) ;
   if( corr_metric==SIM_SPEARMAN ){
     rank1=(float *)malloc(sizeof(float)*(size_t)ntime) ;
     rank2=(float *)malloc(sizeof(float)*(size_t)ntime) ;
   }
   sm=THD_simmat_new(nsub) ;
   if( data==NULL || ref==NULL || values==NULL || scratch==NULL || sm==NULL ||
       (missing_policy==MISSING_COMMON && goodtime==NULL) ||
       (corr_metric==SIM_SPEARMAN && (rank1==NULL || rank2==NULL)) )
     ERROR_exit(PROGRAM_NAME ": cannot allocate atlas ROI buffers") ;

   /* Each ROI is reduced to one mean time series per subject and condition.
      The mask is intersected here rather than modifying the atlas labels. */
   for( rr=0 ; rr<rl->nroi ; rr++ ){
     int nrv=0,nt=ntime ; char rlab[128] ;
     for( vv=0 ; vv<rl->vox[rr].nar ; vv++ ){
       int iv=rl->vox[rr].ar[vv] ;
       if( mask==NULL || mask[iv] ) nrv++ ;
     }
     if( nrv<1 ) continue ;
     if( rl->lab!=NULL && rl->lab[rr]!=NULL )
       insync_safe_token(rlab,sizeof(rlab),rl->lab[rr]) ;
     else snprintf(rlab,sizeof(rlab),"ROI%d",rl->val[rr]) ;

     for( cc=0 ; cc<ncond ; cc++ ) for( ss=0 ; ss<nsub ; ss++ ){
       int dd=cc*nsub+ss ;
       for( tt=0 ; tt<ntime ; tt++ ){
         double sum=0.0 ; int bad=0,nv=0,ot=time_index[tt] ;
         for( vv=0 ; vv<rl->vox[rr].nar ; vv++ ){
           int iv=rl->vox[rr].ar[vv] ; float x ;
           if( mask!=NULL && !mask[iv] ) continue ;
           x=THD_get_voxel(dset[ss*ncond+cc],iv,ot) ;
           if( !isfinite(x) ){ bad=1 ; break ; }
           sum+=x ; nv++ ;
         }
         if( bad && missing_policy==MISSING_ERROR )
           ERROR_exit(PROGRAM_NAME ": nonfinite atlas input for ROI %d, Subj %s, "
                      "condition %s, time %d",rl->val[rr],
                      cindex?cindex->level[0][ss]:tab->subj[ss],clabel[cc],ot+1) ;
         data[(size_t)dd*ntime+tt]=(bad || nv<1)?NAN:(float)(sum/nv) ;
       }
     }
     if( missing_policy==MISSING_COMMON ){
       int dd,ngood=0 ;

       /* Compress all ROI series with one shared complete-case time index;
          this prevents subjects from being correlated on different samples. */
       for( tt=0 ; tt<ntime ; tt++ ){
         int good=1 ;
         for( dd=0 ; dd<ndset ; dd++ )
           if( !isfinite(data[(size_t)dd*ntime+tt]) ){ good=0 ; break ; }
         goodtime[tt]=(byte)good ; if( good ) ngood++ ;
       }
       if( ngood>=1 ) for( dd=0 ; dd<ndset ; dd++ ){
         int pos=0 ; float *src=data+(size_t)dd*ntime,*dst=data+(size_t)dd*ngood ;
         for( tt=0 ; tt<ntime ; tt++ ) if( goodtime[tt] ) dst[pos++]=src[tt] ;
       }
       nt=ngood ;
     }

     /* Compute the same estimator used voxelwise.  Matrix export remains
        pairwise descriptive output even when the requested summary is LOO. */
     for( cc=0 ; cc<ncond ; cc++ ){
       float *block=data+(size_t)cc*nsub*nt ; int pair_ok=0 ;
       if( nt>=3 )
         pair_ok=!THD_simmat_fill_from_features(sm,nt,block,corr_metric,rank1,rank2) ;
       for( gg=0 ; gg<ngroup ; gg++ ){
         float stat=NAN ; int nv=0 ;
         if( nt>=3 && method==METHOD_PAIRWISE && pair_ok )
           stat=INSYNC_pairwise_indexed(nsub,sm->mat,gcount[gg],gmember[gg],
                                        summary,scratch,&nv) ;
         else if( nt>=3 && method==METHOD_LOO )
           stat=INSYNC_loo_indexed_metric(nsub,nt,block,gcount[gg],gmember[gg],
                  summary,corr_metric,ref,values,rank1,rank2,&nv) ;
         fprintf(sfp,"%s %d %s %s %.9g %d %d %d %s %s %s\n",
                 clabel[cc],rl->val[rr],rlab,glabel[gg],
                 isfinite(stat)?stat:0.0f,nv,nt,nrv,
                 corr_metric==SIM_PEARSON?"pearson":"spearman",
                 method==METHOD_PAIRWISE?"pairwise":"loo",
                 summary==INSYNC_SUMMARY_MEDIAN?"median":"fisher_mean") ;
       }
       if( mfp!=NULL && pair_ok ){
         for( aa=0 ; aa<nsub ; aa++ ) for( bb=aa+1 ; bb<nsub ; bb++ ){
           const char *sa=cindex?cindex->level[0][aa]:tab->subj[aa] ;
           const char *sb=cindex?cindex->level[0][bb]:tab->subj[bb] ;
           fprintf(mfp,"%s %d %s %s %s %s %s %.9g\n",clabel[cc],
                   rl->val[rr],rlab,sa,sb,glabel[group[aa]],glabel[group[bb]],
                   sm->mat[(size_t)aa*nsub+bb]) ;
         }
       }
     }
     nwritten++ ;
   }
   fclose(sfp) ; if( mfp!=NULL ) fclose(mfp) ;
   if( !quiet ) INFO_message(PROGRAM_NAME ": wrote %d atlas ROI summaries to %s%s%s",
                 nwritten,sname,matrix_name?" and matrices to ":"",
                 matrix_name?matrix_name:"") ;
   free(data); free(ref); free(values); free(scratch); free(rank1); free(rank2);
   free(goodtime); THD_simmat_free(sm); THD_roilist_free(rl); DSET_delete(aset);
   (void)ntime_input ; (void)rowmap ;
}

/*! Cache the Fourier spectrum of every subject's real time series. */
static int insync_phase_prepare( int nsub, int ntime, float *series,
                                 complex *spectrum )
{
   int ss,tt ;
   if( series==NULL || spectrum==NULL ) return 1 ;
   for( ss=0 ; ss<nsub ; ss++ ){
     for( tt=0 ; tt<ntime ; tt++ ){
       spectrum[(size_t)ss*ntime+tt].r=series[(size_t)ss*ntime+tt] ;
       spectrum[(size_t)ss*ntime+tt].i=0.0f ;
     }
     if( THD_fftnf_OMP(ntime,NULL,&spectrum[(size_t)ss*ntime].r,
                       &spectrum[(size_t)ss*ntime].i,-2,0.0) ) return 1 ;
   }
   return 0 ;
}

/*! Materialize one real-valued phase surrogate from cached spectra. */
static int insync_phase_draw( THD_phase_set *pset, int iphase,
                              complex *spectrum, complex *work, float *series )
{
   int ss,tt,ff,ntime,nsub ;
   if( pset==NULL || spectrum==NULL || work==NULL || series==NULL ) return 1 ;
   ntime=pset->ntime ; nsub=pset->nobs ;
   for( ss=0 ; ss<nsub ; ss++ ){
     complex *src=spectrum+(size_t)ss*ntime ;
     memcpy(work,src,sizeof(complex)*(size_t)ntime) ;
     /* Apply seeded unit-complex factors to positive frequencies and mirror
        them onto negative frequencies to preserve a real inverse transform. */
     for( ff=1 ; ff<=pset->nfreq ; ff++ ){
       float co,si,ar=src[ff].r,ai=src[ff].i ;
       THD_phase_set_factor(pset,iphase,ss,ff,&co,&si) ;
       work[ff].r=ar*co-ai*si ; work[ff].i=ar*si+ai*co ;
       work[ntime-ff].r=work[ff].r ; work[ntime-ff].i=-work[ff].i ;
     }
     if( THD_fftnf_OMP(ntime,NULL,&work[0].r,&work[0].i,+2,0.0) ) return 1 ;
     for( tt=0 ; tt<ntime ; tt++ )
       series[(size_t)ss*ntime+tt]=work[tt].r/(float)ntime ;
   }
   return 0 ;
}

/*! Materialize one circularly shifted subject-major data block. */
static void insync_shift_draw( int nsub, int ntime, const float *data,
                               const int *offset, float *series )
{
   int ss,tt ;
   for( ss=0 ; ss<nsub ; ss++ ) for( tt=0 ; tt<ntime ; tt++ )
     series[(size_t)ss*ntime+tt]
       =data[(size_t)ss*ntime+(tt+offset[ss])%ntime] ;
}

/*! qsort comparator for ascending float-valued empirical null arrays. */
static int insync_float_compare( const void *a, const void *b )
{
   float aa=*((const float *)a),bb=*((const float *)b) ;
   return (aa>bb)-(aa<bb) ;
}

/*! Finish an AFNI-style bisided test.  Each sign is compared with its own
    max-statistic null, then the single-tail probabilities are doubled so the
    reported values stay on the conventional two-sided p-value scale. */
static void insync_finish_bisided( PERM_result *pr, byte *valid,
                                   float *max_null_neg )
{
   int ii ;
   if( pr==NULL || max_null_neg==NULL ) return ;
   qsort(pr->max_null,pr->nperm,sizeof(float),insync_float_compare) ;
   qsort(max_null_neg,pr->nperm,sizeof(float),insync_float_compare) ;
   for( ii=0 ; ii<pr->nelem ; ii++ ){
     float cmp,punc,pfwe,*null_dist ;
     if( valid!=NULL && !valid[ii] ){
       pr->p_unc[ii]=pr->p_fwe[ii]=1.0f ;
       pr->z_unc[ii]=pr->z_fwe[ii]=0.0f ;
       continue ;
     }
     cmp=(pr->stat[ii]<0.0f)?-pr->stat[ii]:pr->stat[ii] ;
     null_dist=(pr->stat[ii]<0.0f)?max_null_neg:pr->max_null ;
     punc=2.0f*(float)pr->cnt_unc[ii]/(float)pr->nperm ;
     pfwe=2.0f*THD_perm_emp_pvalue(null_dist,pr->nperm,cmp) ;
     pr->p_unc[ii]=(punc>1.0f)?1.0f:punc ;
     pr->p_fwe[ii]=(pfwe>1.0f)?1.0f:pfwe ;
     pr->z_unc[ii]=THD_perm_signed_z(pr->p_unc[ii],pr->stat[ii],PERM_TAIL_TWO) ;
     pr->z_fwe[ii]=THD_perm_signed_z(pr->p_fwe[ii],pr->stat[ii],PERM_TAIL_TWO) ;
   }
}

/*! Parse the command line, organize subjects and conditions, run voxelwise
    ISC and requested null models, then write image, ROI, and 3dISC outputs. */
int main( int argc, char **argv )
{
   /* Core analysis and reporting options. */
   int nopt=1 ;
   int method=METHOD_PAIRWISE ;
   int summary=INSYNC_SUMMARY_MEDIAN ;
   int show_table=0 ;
   int corr_metric=SIM_PEARSON ;
   int missing_policy=MISSING_ERROR ;
   int quiet=0 ;
   int progress_mode=PROGRESS_AUTO ;
   int memory_override=0 ;
   int memory_limit_given=0 ;
   double memory_limit_gib=0.0 ;

   /* Resampling and inference options. */
   int nboot=0 ;
   int nperm=0 ;
   int do_exact=0 ;
   int tail=PERM_TAIL_TWO ;
   int temporal_mode=TNULL_NONE ;
   int nnull=0 ;
   int min_shift=1 ;
   int min_shift_given=0 ;
   int temporal_tail=PERM_TAIL_ONE ;
   int temporal_tail_given=0 ;
   int ncondperm=0 ;
   int condition_exact=0 ;
   int condition_tail=PERM_TAIL_TWO ;
   long seed=1234567L ;
   float boot_alpha=0.05f ;

   /* Input column names, contrasts, and optional output names. */
   int condition_column_given=0 ;
   int have_contrast=0 ;
   int contrast_c0=-1 ;
   int contrast_c1=-1 ;
   char *prefix="InSync" ;
   char *maskname=NULL ;
   char *group_column="Group" ;
   char *condition_column="Condition" ;
   char *contrast_name0=NULL ;
   char *contrast_name1=NULL ;
   char *pair_prefix=NULL ;
   char *censor_name=NULL ;
   char *atlas_name=NULL ;
   char *roi_sel=NULL ;
   char *matrix_name=NULL ;

   /* Parsed table, dataset, and mask state. */
   THD_datatable *tab=NULL ;
   THD_datatable_index *cindex=NULL ;
   THD_3dim_dataset **dset=NULL ;
   THD_3dim_dataset *first=NULL ;
   THD_3dim_dataset *mset=NULL ;
   THD_3dim_dataset *out=NULL ;
   THD_3dim_dataset *pairout=NULL ;
   byte *mask=NULL ;
   int *rowmap=NULL ;
   int *time_index=NULL ;
   byte *censor_keep=NULL ;

   /* Subject, condition, and group organization. */
   int *group=NULL ;
   int *gcount=NULL ;
   int **gmember=NULL ;
   char **glabel=NULL ;
   char **clabel=NULL ;
   int nsub ;
   int ngroup=0 ;
   int ncond=1 ;
   int ndset ;
   int ntime ;
   int ntime_input ;
   int nvox ;
   int icol_group ;
   size_t nedge=0 ;

   /* Bootstrap, permutation, and temporal-null state. */
   THD_resample_set *bset=NULL ;
   PERM_scheme *pscheme=NULL ;
   PERM_scheme *cscheme=NULL ;
   PERM_set *pset=NULL ;
   PERM_set *cset=NULL ;
   PERM_result *presult=NULL ;
   PERM_result *cresult=NULL ;
   PERM_result *tresult=NULL ;
   THD_timeshift_set *tset=NULL ;
   THD_phase_set *phset=NULL ;
   unsigned char *tsneed=NULL ;
   byte *infer_valid=NULL ;
   byte *temporal_valid=NULL ;
   byte *condition_valid=NULL ;
   float *group_max_neg=NULL ;

   /* Output layout and backing arrays. */
   int nout ;
   int validtr_slot=-1 ;
   int diff_slot=-1 ;
   int boot_slot=-1 ;
   int perm_slot=-1 ;
   int temporal_slot=-1 ;
   int contrast_slot=-1 ;
   int condperm_slot=-1 ;
   int neffect=0 ;
   float **outval=NULL ;
   float *qval=NULL ;
   float *temporal_qval=NULL ;
   float *condition_qval=NULL ;
   float *pairval=NULL ;
   size_t npairmap=0 ;

   /* Shared loop indices, progress state, and diagnostic counters. */
   int ii ;
   int jj ;
   int vv ;
   int kk ;
   int cc ;
   int nmask ;
   long long invalid_total=0 ;
   long long boot_invalid_total=0 ;
   long long temporal_invalid_total=0 ;
   long long condition_invalid_total=0 ;
   long long bad_data=LLONG_MAX ;
   INSYNC_progress progress ;

   if( argc<2 ){ usage_3dInSync() ; return 0 ; }
   mainENTRY(PROGRAM_NAME " main") ; machdep() ; AFNI_SETUP_OMP(0) ;
   AFNI_logger(PROGRAM_NAME,argc,argv) ; PRINT_VERSION(PROGRAM_NAME) ;
   THD_check_AFNI_version(PROGRAM_NAME) ;

   /* Parse first, then validate option combinations as one contract below.
      This keeps parsing order from changing program semantics. */
   while( nopt<argc && argv[nopt][0]=='-' ){
     if( strcasecmp(argv[nopt],"-help")==0 || strcasecmp(argv[nopt],"-h")==0 ){
       usage_3dInSync() ; return 0 ;
     }
     if( strcasecmp(argv[nopt],"-prefix")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an argument after -prefix") ;
       prefix=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-mask")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a dataset after -mask") ;
       maskname=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-isc_method")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need pairwise or loo after -isc_method") ;
       if( strcasecmp(argv[nopt],"pairwise")==0 ) method=METHOD_PAIRWISE ;
       else if( strcasecmp(argv[nopt],"loo")==0 ) method=METHOD_LOO ;
       else ERROR_exit(PROGRAM_NAME ": -isc_method must be pairwise or loo") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-summary")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need median or fisher_mean after -summary") ;
       if( strcasecmp(argv[nopt],"median")==0 ) summary=INSYNC_SUMMARY_MEDIAN ;
       else if( strcasecmp(argv[nopt],"fisher_mean")==0 ||
                strcasecmp(argv[nopt],"mean")==0 ) summary=INSYNC_SUMMARY_FISHER_MEAN ;
       else ERROR_exit(PROGRAM_NAME ": -summary must be median or fisher_mean") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-correlation")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need pearson or spearman after -correlation") ;
       if( strcasecmp(argv[nopt],"pearson")==0 ||
           strcasecmp(argv[nopt],"corr")==0 ) corr_metric=SIM_PEARSON ;
       else if( strcasecmp(argv[nopt],"spearman")==0 ||
                strcasecmp(argv[nopt],"scorr")==0 ) corr_metric=SIM_SPEARMAN ;
       else ERROR_exit(PROGRAM_NAME ": -correlation must be pearson or spearman") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-censor")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a 1D file after -censor") ;
       censor_name=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-missing")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need error or common after -missing") ;
       if( strcasecmp(argv[nopt],"error")==0 ) missing_policy=MISSING_ERROR ;
       else if( strcasecmp(argv[nopt],"common")==0 ) missing_policy=MISSING_COMMON ;
       else ERROR_exit(PROGRAM_NAME ": -missing must be error or common") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-atlas")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a dataset after -atlas") ;
       atlas_name=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-roi_sel")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a label list after -roi_sel") ;
       roi_sel=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-save_matrix")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a filename after -save_matrix") ;
       matrix_name=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-memory_limit")==0 ){
       char *ep ;
       double x ;
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need GiB after -memory_limit") ;
       errno=0 ; x=strtod(argv[nopt++],&ep) ;
       if( errno==ERANGE || ep==argv[nopt-1] || *ep!='\0' || !isfinite(x) || x<=0.0 )
         ERROR_exit(PROGRAM_NAME ": -memory_limit must be a positive finite GiB value") ;
       memory_limit_gib=x ; memory_limit_given=1 ; continue ;
     }
     if( strcasecmp(argv[nopt],"-memory_override")==0 ){
       memory_override=1 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-quiet")==0 ){
       quiet=1 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-progress")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need auto, bar, line, or off after -progress") ;
       if( strcasecmp(argv[nopt],"auto")==0 ) progress_mode=PROGRESS_AUTO ;
       else if( strcasecmp(argv[nopt],"bar")==0 ) progress_mode=PROGRESS_BAR ;
       else if( strcasecmp(argv[nopt],"line")==0 ) progress_mode=PROGRESS_LINE ;
       else if( strcasecmp(argv[nopt],"off")==0 ) progress_mode=PROGRESS_OFF ;
       else ERROR_exit(PROGRAM_NAME ": -progress must be auto, bar, line, or off") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-group_column")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a name after -group_column") ;
       group_column=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-condition_column")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a name after -condition_column") ;
       condition_column=argv[nopt++] ; condition_column_given=1 ; continue ;
     }
     if( strcasecmp(argv[nopt],"-condition_contrast")==0 ){
       if( have_contrast ) ERROR_exit(PROGRAM_NAME ": give -condition_contrast only once") ;
       if( nopt+2>=argc ) ERROR_exit(PROGRAM_NAME ": need two labels after -condition_contrast") ;
       contrast_name0=argv[++nopt] ; contrast_name1=argv[++nopt] ; nopt++ ;
       have_contrast=1 ; continue ;
     }
     if( strcasecmp(argv[nopt],"-ncondperm")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an integer after -ncondperm") ;
       ncondperm=(int)strtol(argv[nopt++],NULL,10) ;
       if( ncondperm<20 ) ERROR_exit(PROGRAM_NAME ": -ncondperm must be at least 20") ;
       continue ;
     }
     if( strcasecmp(argv[nopt],"-condition_exact")==0 ){
       condition_exact=1 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-condition_tail")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need two or one after -condition_tail") ;
       if( strcasecmp(argv[nopt],"two")==0 || strcasecmp(argv[nopt],"two-sided")==0 )
         condition_tail=PERM_TAIL_TWO ;
       else if( strcasecmp(argv[nopt],"one")==0 || strcasecmp(argv[nopt],"upper")==0 ||
                strcasecmp(argv[nopt],"one-sided")==0 ) condition_tail=PERM_TAIL_ONE ;
       else ERROR_exit(PROGRAM_NAME ": -condition_tail must be two or one") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-save_pairwise")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a prefix after -save_pairwise") ;
       pair_prefix=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-nboot")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an integer after -nboot") ;
       nboot=(int)strtol(argv[nopt++],NULL,10) ;
       if( nboot<100 ) ERROR_exit(PROGRAM_NAME ": -nboot must be at least 100") ;
       continue ;
     }
     if( strcasecmp(argv[nopt],"-boot_alpha")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a number after -boot_alpha") ;
       boot_alpha=(float)strtod(argv[nopt++],NULL) ;
       if( !(boot_alpha>0.0f && boot_alpha<1.0f) )
         ERROR_exit(PROGRAM_NAME ": -boot_alpha must be between 0 and 1") ;
       continue ;
     }
     if( strcasecmp(argv[nopt],"-nperm")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an integer after -nperm") ;
       nperm=(int)strtol(argv[nopt++],NULL,10) ;
       if( nperm<20 ) ERROR_exit(PROGRAM_NAME ": -nperm must be at least 20") ;
       continue ;
     }
     if( strcasecmp(argv[nopt],"-exact")==0 ){ do_exact=1 ; nopt++ ; continue ; }
     if( strcasecmp(argv[nopt],"-seed")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an integer after -seed") ;
       seed=strtol(argv[nopt++],NULL,10) ; continue ;
     }
     if( strcasecmp(argv[nopt],"-1sided")==0 ){
       tail=PERM_TAIL_ONE ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-2sided")==0 ){
       tail=PERM_TAIL_TWO ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-bisided")==0 ){
       tail=INSYNC_TAIL_BI ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-tail")==0 ){
       if( ++nopt>=argc )
         ERROR_exit(PROGRAM_NAME ": need 1sided, 2sided, or bisided after -tail") ;
       if( strcasecmp(argv[nopt],"2sided")==0 || strcasecmp(argv[nopt],"two")==0 ||
           strcasecmp(argv[nopt],"two-sided")==0 )
         tail=PERM_TAIL_TWO ;
       else if( strcasecmp(argv[nopt],"1sided")==0 || strcasecmp(argv[nopt],"one")==0 ||
                strcasecmp(argv[nopt],"upper")==0 ||
                strcasecmp(argv[nopt],"one-sided")==0 ) tail=PERM_TAIL_ONE ;
       else if( strcasecmp(argv[nopt],"bisided")==0 ) tail=INSYNC_TAIL_BI ;
       else ERROR_exit(PROGRAM_NAME ": -tail must be 1sided, 2sided, or bisided") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-temporal_null")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need timeshift or phase after -temporal_null") ;
       if( strcasecmp(argv[nopt],"timeshift")==0 || strcasecmp(argv[nopt],"shift")==0 )
         temporal_mode=TNULL_TIMESHIFT ;
       else if( strcasecmp(argv[nopt],"phase")==0 ) temporal_mode=TNULL_PHASE ;
       else ERROR_exit(PROGRAM_NAME ": -temporal_null must be timeshift or phase") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-nnull")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an integer after -nnull") ;
       nnull=(int)strtol(argv[nopt++],NULL,10) ;
       if( nnull<20 ) ERROR_exit(PROGRAM_NAME ": -nnull must be at least 20") ;
       continue ;
     }
     if( strcasecmp(argv[nopt],"-min_shift")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an integer after -min_shift") ;
       min_shift=(int)strtol(argv[nopt++],NULL,10) ; min_shift_given=1 ;
       if( min_shift<1 ) ERROR_exit(PROGRAM_NAME ": -min_shift must be positive") ;
       continue ;
     }
     if( strcasecmp(argv[nopt],"-temporal_tail")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need one or two after -temporal_tail") ;
       if( strcasecmp(argv[nopt],"one")==0 || strcasecmp(argv[nopt],"upper")==0 ||
           strcasecmp(argv[nopt],"one-sided")==0 ) temporal_tail=PERM_TAIL_ONE ;
       else if( strcasecmp(argv[nopt],"two")==0 || strcasecmp(argv[nopt],"two-sided")==0 )
         temporal_tail=PERM_TAIL_TWO ;
       else ERROR_exit(PROGRAM_NAME ": -temporal_tail must be one or two") ;
       temporal_tail_given=1 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-show_table")==0 ){ show_table=1 ; nopt++ ; continue ; }
     if( strcasecmp(argv[nopt],"-dataTableFile")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a file after -dataTableFile") ;
       if( tab!=NULL ) ERROR_exit(PROGRAM_NAME ": the data table was given twice") ;
       tab=THD_read_datatable_file(argv[nopt++]) ; continue ;
     }
     if( strcasecmp(argv[nopt],"-dataTable")==0 ){
       int nused=0 ;
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a table after -dataTable") ;
       if( tab!=NULL ) ERROR_exit(PROGRAM_NAME ": the data table was given twice") ;
       tab=THD_read_datatable_args(argc,argv,nopt,insync_options,&nused) ;
       nopt+=nused ; continue ;
     }
     ERROR_exit(PROGRAM_NAME ": unknown option '%s'",argv[nopt]) ;
   }

   /* Cross-option constraints protect estimands that require a common intact
      timeline or a paired condition design. */
   if( nopt<argc ) ERROR_exit(PROGRAM_NAME ": unexpected argument '%s'",argv[nopt]) ;
   if( temporal_mode==TNULL_NONE && nnull>0 ) ERROR_exit(PROGRAM_NAME ": -nnull requires -temporal_null") ;
   if( temporal_mode!=TNULL_NONE && nnull<20 ) ERROR_exit(PROGRAM_NAME ": -temporal_null requires -nnull >= 20") ;
   if( temporal_mode!=TNULL_TIMESHIFT && min_shift_given ) ERROR_exit(PROGRAM_NAME ": -min_shift applies only to timeshift") ;
   if( temporal_mode==TNULL_NONE && temporal_tail_given ) ERROR_exit(PROGRAM_NAME ": -temporal_tail requires -temporal_null") ;
   if( temporal_mode!=TNULL_NONE && censor_name!=NULL )
     ERROR_exit(PROGRAM_NAME ": -censor cannot be combined with a temporal null; "
                "circular shifts/phases require an intact timeline") ;
   if( temporal_mode!=TNULL_NONE && missing_policy==MISSING_COMMON )
     ERROR_exit(PROGRAM_NAME ": -missing common cannot be combined with a temporal null") ;
   if( roi_sel!=NULL && atlas_name==NULL )
     ERROR_exit(PROGRAM_NAME ": -roi_sel requires -atlas") ;
   if( matrix_name!=NULL && atlas_name==NULL )
     ERROR_exit(PROGRAM_NAME ": -save_matrix requires -atlas") ;
   if( (ncondperm>0 || condition_exact) && !have_contrast )
     ERROR_exit(PROGRAM_NAME ": -ncondperm/-condition_exact require -condition_contrast C1 C2") ;
   if( !THD_filename_ok(prefix) ) ERROR_exit(PROGRAM_NAME ": illegal -prefix '%s'",prefix) ;
   if( pair_prefix!=NULL && !THD_filename_ok(pair_prefix) )
     ERROR_exit(PROGRAM_NAME ": illegal -save_pairwise prefix '%s'",pair_prefix) ;
   if( tab==NULL ) ERROR_exit(PROGRAM_NAME ": -dataTable or -dataTableFile is required") ;
   if( tab->icol_input<0 || tab->fname==NULL ) ERROR_exit(PROGRAM_NAME ": data table requires InputFile") ;
   if( show_table ) THD_datatable_print(tab,stdout) ;

   /* Convert either table shape into the same subject-major, condition-minor
      row map.  Long tables must be a complete Cartesian product. */
   cc=THD_datatable_column(tab,condition_column) ;
   if( cc>=0 ){
     char *cols[2]={"Subj",condition_column} ;
     if( tab->icol_subj<0 ) ERROR_exit(PROGRAM_NAME ": a long condition table requires Subj") ;
     if( cc==tab->icol_subj || cc==tab->icol_input )
       ERROR_exit(PROGRAM_NAME ": -condition_column must name a separate column") ;
     cindex=THD_datatable_index_columns(tab,2,cols,NULL,NULL) ;
     nsub=cindex->nlevel[0] ; ncond=cindex->nlevel[1] ; clabel=cindex->level[1] ;
     if( ncond<2 ) ERROR_exit(PROGRAM_NAME ": condition column '%s' has only one level",condition_column) ;
     rowmap=(int *)malloc(sizeof(int)*(size_t)cindex->ncell) ;
     memcpy(rowmap,cindex->row_of,sizeof(int)*(size_t)cindex->ncell) ;
     for( ii=0 ; ii<nsub ; ii++ ){
       int rr0=rowmap[ii*ncond] ;
       for( cc=1 ; cc<ncond ; cc++ ){
         int rr=rowmap[ii*ncond+cc] ;
         for( jj=0 ; jj<tab->ncol ; jj++ )
           if( jj!=tab->icol_subj && jj!=tab->icol_input && jj!=cindex->icol[1] &&
               strcmp(DT_CELL(tab,rr0,jj),DT_CELL(tab,rr,jj))!=0 )
             ERROR_exit(PROGRAM_NAME ": column '%s' changes within Subj %s across conditions",
                        tab->cname[jj],cindex->level[0][ii]) ;
       }
     }
   } else {
     if( condition_column_given ) ERROR_exit(PROGRAM_NAME ": -condition_column '%s' is absent",condition_column) ;
     nsub=tab->nrow ; ncond=1 ; clabel=(char **)calloc(1,sizeof(char *)) ;
     clabel[0]=strdup("All") ; rowmap=(int *)malloc(sizeof(int)*(size_t)nsub) ;
     for( ii=0 ; ii<nsub ; ii++ ) rowmap[ii]=ii ;
     for( ii=0 ; ii<nsub ; ii++ ) for( jj=ii+1 ; jj<nsub ; jj++ )
       if( strcmp(tab->subj[ii],tab->subj[jj])==0 )
         ERROR_exit(PROGRAM_NAME ": duplicate Subj '%s' in rows %d and %d",tab->subj[ii],ii+1,jj+1) ;
   }
   if( nsub<3 ) ERROR_exit(PROGRAM_NAME ": need at least 3 subjects; found %d",nsub) ;
   ndset=nsub*ncond ;

   if( have_contrast ){
     for( cc=0 ; cc<ncond ; cc++ ){
       if( strcmp(clabel[cc],contrast_name0)==0 ) contrast_c0=cc ;
       if( strcmp(clabel[cc],contrast_name1)==0 ) contrast_c1=cc ;
     }
     if( contrast_c0<0 || contrast_c1<0 || contrast_c0==contrast_c1 )
       ERROR_exit(PROGRAM_NAME ": condition contrast '%s' vs '%s' does not name two table levels",
                  contrast_name0,contrast_name1) ;
   }
   /* Text outputs are not protected by DSET_write(), so fail early under
      AFNI's normal no-overwrite policy before doing expensive computation. */
   if( !THD_ok_overwrite() ){
     char fname[THD_MAX_NAME] ;
     if( atlas_name!=NULL ){
       snprintf(fname,sizeof(fname),"%s.roi.1D",prefix) ;
       if( THD_is_file(fname) )
         ERROR_exit(PROGRAM_NAME ": ROI summary '%s' already exists",fname) ;
       if( matrix_name!=NULL && THD_is_file(matrix_name) )
         ERROR_exit(PROGRAM_NAME ": ROI matrix '%s' already exists",matrix_name) ;
     }
     if( pair_prefix!=NULL ){
       snprintf(fname,sizeof(fname),ncond==1?"%s.3dISC.txt":"%s.3dISC.all.txt",
                pair_prefix) ;
       if( THD_is_file(fname) )
         ERROR_exit(PROGRAM_NAME ": 3dISC table '%s' already exists",fname) ;
       if( ncond>1 ) for( ii=0 ; ii<ncond ; ii++ ) for( jj=0 ; jj<ncond ; jj++ ){
         char ca[64] ;
         char cb[64] ;
         insync_safe_token(ca,sizeof(ca),clabel[ii]) ;
         insync_safe_token(cb,sizeof(cb),clabel[jj]) ;
         snprintf(fname,sizeof(fname),"%s.%s__%s.3dISC.txt",pair_prefix,ca,cb) ;
         if( THD_is_file(fname) )
           ERROR_exit(PROGRAM_NAME ": 3dISC table '%s' already exists",fname) ;
       }
     }
   }

   /* Preserve order of first appearance for group labels.  That order also
      defines the sign of all two-group effects: first minus second. */
   group=(int *)calloc((size_t)nsub,sizeof(int)) ;
   icol_group=THD_datatable_column(tab,group_column) ;
   if( icol_group<0 ) insync_add_group("All",&glabel,&ngroup) ;
   else for( ii=0 ; ii<nsub ; ii++ ){
     char *lab=DT_CELL(tab,rowmap[ii*ncond],icol_group) ;
     int gg=insync_find_group(lab,glabel,ngroup) ;
     if( gg<0 ){ insync_add_group(lab,&glabel,&ngroup) ; gg=ngroup-1 ; }
     group[ii]=gg ;
   }
   gcount=(int *)calloc((size_t)ngroup,sizeof(int)) ;
   for( ii=0 ; ii<nsub ; ii++ ) gcount[group[ii]]++ ;
   for( kk=0 ; kk<ngroup ; kk++ ){
     if( gcount[kk]<3 ) ERROR_exit(PROGRAM_NAME ": group '%s' has %d subjects; need at least 3",glabel[kk],gcount[kk]) ;
     if( !quiet ) INFO_message(PROGRAM_NAME ": group %d '%s': %d subjects",
                               kk+1,glabel[kk],gcount[kk]) ;
   }
   if( (nperm>0 || do_exact) && ngroup!=2 ) ERROR_exit(PROGRAM_NAME ": group permutation requires exactly two groups") ;
   gmember=(int **)calloc((size_t)ngroup,sizeof(int *)) ;
   for( kk=0 ; kk<ngroup ; kk++ ){
     int pos=0 ;
     gmember[kk]=(int *)malloc(sizeof(int)*(size_t)gcount[kk]) ;
     for( ii=0 ; ii<nsub ; ii++ ) if( group[ii]==kk ) gmember[kk][pos++]=ii ;
   }

   /* Build synchronized resampling sets once.  Reusing the same draws at
      every voxel makes the max-statistic family correction well defined. */
   if( nboot>0 ){
     bset=THD_resample_set_build_stratified(nsub,nboot,seed,group) ;
     if( bset==NULL ) ERROR_exit(PROGRAM_NAME ": cannot build bootstrap draws") ;
   }
   if( nperm>0 || do_exact ){
     pscheme=THD_perm_scheme_new(nsub) ; pscheme->exchange=PERM_EE ; pscheme->seed=seed ;
     pscheme->exact=do_exact ; pscheme->nperm=(nperm>0)?nperm:10000 ;
     if( THD_perm_scheme_set_eqclass(pscheme,group)!=ngroup ) ERROR_exit(PROGRAM_NAME ": cannot encode groups") ;
     pset=THD_perm_set_build(pscheme) ;
     if( pset==NULL ) ERROR_exit(PROGRAM_NAME ": cannot build group permutations") ;
     if( !quiet ) INFO_message(PROGRAM_NAME ": using %d %s group assignments",
                               pset->nperm,pset->is_exact?"exact":"sampled") ;
   }
   if( ncondperm>0 || condition_exact ){
     cscheme=THD_perm_scheme_new(nsub) ; cscheme->exchange=PERM_ISE ;
     cscheme->seed=seed+104729L ; cscheme->exact=condition_exact ;
     cscheme->nperm=(ncondperm>0)?ncondperm:10000 ; cset=THD_perm_set_build(cscheme) ;
     if( cset==NULL ) ERROR_exit(PROGRAM_NAME ": cannot build within-subject condition swaps") ;
     if( !quiet ) INFO_message(PROGRAM_NAME ": using %d %s within-subject condition swaps",
                               cset->nperm,cset->is_exact?"exact":"sampled") ;
   }

   dset=(THD_3dim_dataset **)calloc((size_t)ndset,sizeof(THD_3dim_dataset *)) ;
   for( ii=0 ; ii<nsub ; ii++ ) for( cc=0 ; cc<ncond ; cc++ ){
     int dd=ii*ncond+cc ;
     int rr=rowmap[dd] ;
     const char *who=cindex?cindex->level[0][ii]:tab->subj[ii] ;
     dset[dd]=THD_open_dataset(tab->fname[rr]) ; CHECK_OPEN_ERROR(dset[dd],tab->fname[rr]) ;
     if( dd==0 ){
       first=dset[dd] ; ntime=DSET_NVALS(first) ; nvox=DSET_NVOX(first) ;
       if( ntime<3 ) ERROR_exit(PROGRAM_NAME ": input datasets need at least 3 time points") ;
     } else {
       if( !EQUIV_GRIDS(first,dset[dd]) ) ERROR_exit(PROGRAM_NAME ": input for Subj %s, condition %s is off-grid",who,clabel[cc]) ;
       if( DSET_NVALS(dset[dd])!=ntime ) ERROR_exit(PROGRAM_NAME ": input for Subj %s, condition %s has %d time points; expected %d",who,clabel[cc],DSET_NVALS(dset[dd]),ntime) ;
     }
   }

   /* Represent common censoring as a retained-time lookup.  Subject datasets
      remain uncompressed on disk and are sampled through this index. */
   ntime_input=ntime ; time_index=(int *)malloc(sizeof(int)*(size_t)ntime_input) ;
   censor_keep=(byte *)calloc((size_t)ntime_input,sizeof(byte)) ;
   if( time_index==NULL || censor_keep==NULL )
     ERROR_exit(PROGRAM_NAME ": cannot allocate common-censor index") ;
   if( censor_name!=NULL ){
     MRI_IMAGE *cim=mri_read_1D(censor_name) ;
     MRI_IMAGE *fim ;
     float *car ;
     int nkeep=0 ;
     if( cim==NULL ) ERROR_exit(PROGRAM_NAME ": cannot read -censor file '%s'",censor_name) ;
     fim=mri_to_float(cim) ; if( fim!=cim ) mri_free(cim) ;
     if( fim->nvox!=ntime_input )
       ERROR_exit(PROGRAM_NAME ": -censor has %d values; expected %d",
                  fim->nvox,ntime_input) ;
     car=MRI_FLOAT_PTR(fim) ;
     for( ii=0 ; ii<ntime_input ; ii++ ){
       if( !isfinite(car[ii]) || (car[ii]!=0.0f && car[ii]!=1.0f) )
         ERROR_exit(PROGRAM_NAME ": -censor value %d is %.7g; expected finite 0 or 1",
                    ii+1,car[ii]) ;
       if( car[ii]!=0.0f ){ censor_keep[ii]=1 ; time_index[nkeep++]=ii ; }
     }
     mri_free(fim) ;
     if( nkeep<3 ) ERROR_exit(PROGRAM_NAME ": -censor retains %d time points; need at least 3",nkeep) ;
     ntime=nkeep ;
     if( !quiet ) INFO_message(PROGRAM_NAME ": common censor retains %d of %d time points",
                               ntime,ntime_input) ;
   } else {
     for( ii=0 ; ii<ntime_input ; ii++ ){ censor_keep[ii]=1 ; time_index[ii]=ii ; }
   }

   /* Temporal null sets preserve each subject as the exchangeable unit.  For
      pairwise timeshifts, pre-index only lags that a requested draw uses. */
   nedge=(size_t)nsub*(nsub-1)/2 ;
   if( temporal_mode==TNULL_TIMESHIFT ){
     tset=THD_timeshift_set_build(nsub,ntime,nnull,min_shift,seed) ;
     if( tset==NULL ) ERROR_exit(PROGRAM_NAME ": cannot build temporal shifts") ;
     if( method==METHOD_PAIRWISE ){
       tsneed=(unsigned char *)calloc(nedge*(size_t)ntime,sizeof(unsigned char)) ;
       if( tsneed==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate temporal lag index") ;
       for( ii=0 ; ii<nnull ; ii++ ){
         int aa ;
         int bb ;
         int pair=0 ;
         int *off=tset->offset+(size_t)ii*nsub ;
         for( aa=0 ; aa<nsub ; aa++ ) for( bb=aa+1 ; bb<nsub ; bb++,pair++ ){
           int lag=(off[bb]-off[aa])%ntime ;
           if( lag<0 ) lag+=ntime ;
           tsneed[(size_t)pair*ntime+lag]=1 ;
         }
       }
     }
   } else if( temporal_mode==TNULL_PHASE ){
     phset=THD_phase_set_build(nsub,ntime,nnull,seed) ;
     if( phset==NULL ) ERROR_exit(PROGRAM_NAME ": cannot build phase-randomization set") ;
   }

   if( maskname!=NULL ){
     mset=THD_open_dataset(maskname) ; CHECK_OPEN_ERROR(mset,maskname) ; DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
     if( !EQUIV_GRIDS(first,mset) ) ERROR_exit(PROGRAM_NAME ": mask is not on the input grid") ;
     mask=THD_makemask(mset,0,0.5f,0.0f) ;
     if( mask==NULL || THD_countmask(nvox,mask)<1 ) ERROR_exit(PROGRAM_NAME ": mask is empty") ;
     DSET_delete(mset) ; mset=NULL ;
   }
   nmask=(mask!=NULL)?THD_countmask(nvox,mask):nvox ;

   /* Assign every optional output family a contiguous sub-brick range.  The
      same offsets are later used both for computation and AFNI labels. */
   nout=2*ncond*ngroup ;
   if( ngroup==2 ){ diff_slot=nout ; nout+=ncond ; }
   if( nboot>0 ){ boot_slot=nout ; nout+=2*ncond*ngroup ; }
   if( pset!=NULL ){ perm_slot=nout ; nout+=5*ncond ; }
   if( tset!=NULL || phset!=NULL ){ temporal_slot=nout ; nout+=5*ncond*ngroup ; }
   if( have_contrast ){
     neffect=ngroup+((ngroup==2)?1:0) ; contrast_slot=nout ; nout+=neffect ;
     if( cset!=NULL ){ condperm_slot=nout ; nout+=5*neffect ; }
   }
   if( censor_name!=NULL || missing_policy==MISSING_COMMON ){
     validtr_slot=nout ; nout++ ;
   }
   npairmap=(pair_prefix!=NULL)?(size_t)ncond*ncond*nedge:0 ;

   {
     const double GIB=1073741824.0 ;
     const double FS=(double)sizeof(float) ;
     THD_memory_plan me ;
     double warn ;

     /* Refuse an unsafe allocation plan before loading the large subject
        datasets.  Per-worker scratch is multiplied by the OpenMP thread cap. */
     memset(&me,0,sizeof(me)) ;
#ifdef USE_OMP
     me.nthread=omp_get_max_threads() ;
#else
     me.nthread=1 ;
#endif
     if( me.nthread<1 ) me.nthread=1 ;
     me.input=(double)ndset*nvox*ntime_input*FS ;
     if( atlas_name!=NULL ){
       me.geometry+=(double)nvox*FS ;
       me.shared+=FS*((double)ndset*ntime+nsub*nsub+2.0*nsub+3.0*ntime) ;
     }
     me.shared=(double)nsub*((bset?bset->nresample:0)*sizeof(int)
              +(pset?pset->nperm:0)*(sizeof(int)+sizeof(signed char))
              +(cset?cset->nperm:0)*(sizeof(int)+sizeof(signed char))) ;
     if( pset!=NULL && tail==INSYNC_TAIL_BI )
       me.shared+=FS*pset->nperm ;
     me.output=(double)nout*nvox*FS+(double)npairmap*nvox*FS ;
     me.per_thread=FS*((double)ndset*ntime+2.0*nsub*nsub+
                       2.0*nsub*ntime+2.0*nsub+2.0*ntime+
                       (double)nedge*ntime) ;
     if( pset ) me.per_thread+=FS*(2.0+(tail==INSYNC_TAIL_BI))*pset->nperm ;
     if( cset ) me.per_thread+=FS*((double)neffect*cset->nperm+cset->nperm) ;
     if( tset || phset ) me.per_thread+=FS*((double)ngroup*nnull+nnull+
                                            (double)nsub*ntime) ;
     me.system=(double)AFNI_get_memsize() ;
     me.limit=memory_limit_given?memory_limit_gib*GIB:
              ((me.system>0.0)?0.80*me.system:0.0) ;
     THD_memory_plan_finish(&me) ;
     warn=(me.system>0.0)?0.50*me.system:0.0 ;
     if( !quiet && (me.total>warn || memory_limit_given || pair_prefix!=NULL) ){
       if( me.limit>0.0 )
         INFO_message(PROGRAM_NAME ": memory preflight estimates %.3f GiB peak "
                      "(%.3f input + %.3f output/shared + %d x %.3f worker); "
                      "limit %.3f GiB",me.total/GIB,me.input/GIB,
                      (me.output+me.shared)/GIB,me.nthread,me.per_thread/GIB,
                      me.limit/GIB) ;
       else
         INFO_message(PROGRAM_NAME ": memory preflight estimates %.3f GiB peak; "
                      "physical RAM is unavailable",me.total/GIB) ;
     }
     if( me.limit>0.0 && me.total>me.limit ){
       if( !memory_override )
         ERROR_exit(PROGRAM_NAME ": estimated peak %.3f GiB exceeds %.3f GiB limit; "
                    "reduce threads/output, set -memory_limit, or acknowledge with "
                    "-memory_override",me.total/GIB,me.limit/GIB) ;
       WARNING_message(PROGRAM_NAME ": estimated peak exceeds memory limit; "
                       "continuing because -memory_override was given") ;
     }
   }

   /* The potentially dominant subject datasets are intentionally loaded only
      after the memory contract has been checked. */
   for( ii=0 ; ii<ndset ; ii++ ){
     DSET_load(dset[ii]) ; CHECK_LOAD_ERROR(dset[ii]) ;
   }

   /* Allocate shared output maps and empirical-null accumulators. */
   outval=(float **)calloc((size_t)nout,sizeof(float *)) ;
   for( kk=0 ; kk<nout ; kk++ ){
     outval[kk]=(float *)calloc((size_t)nvox,sizeof(float)) ;
     if( outval[kk]==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate output map %d",kk) ;
   }
   if( pset!=NULL ){
     int nelem=ncond*nvox ;
     infer_valid=(byte *)calloc((size_t)nelem,sizeof(byte)) ;
     presult=THD_perm_result_new(nelem,pset->nperm) ;
     if( tail==INSYNC_TAIL_BI )
       group_max_neg=(float *)malloc(sizeof(float)*(size_t)pset->nperm) ;
     if( presult==NULL || infer_valid==NULL ||
         (tail==INSYNC_TAIL_BI && group_max_neg==NULL) )
       ERROR_exit(PROGRAM_NAME ": cannot allocate group-permutation results") ;
     presult->tail=tail ;
     for( ii=0 ; ii<pset->nperm ; ii++ ) presult->max_null[ii]=-FLT_MAX ;
     if( group_max_neg!=NULL ) for( ii=0 ; ii<pset->nperm ; ii++ )
       group_max_neg[ii]=-FLT_MAX ;
   }
   if( tset!=NULL || phset!=NULL ){
     int nelem=ncond*ngroup*nvox ;
     temporal_valid=(byte *)calloc((size_t)nelem,sizeof(byte)) ;
     tresult=THD_perm_result_new(nelem,nnull) ; tresult->tail=temporal_tail ;
     for( ii=0 ; ii<nnull ; ii++ ) tresult->max_null[ii]=-FLT_MAX ;
   }
   if( cset!=NULL ){
     int nelem=neffect*nvox ;
     condition_valid=(byte *)calloc((size_t)nelem,sizeof(byte)) ;
     cresult=THD_perm_result_new(nelem,cset->nperm) ; cresult->tail=condition_tail ;
     for( ii=0 ; ii<cset->nperm ; ii++ ) cresult->max_null[ii]=-FLT_MAX ;
   }
   if( pair_prefix!=NULL ){
     size_t total ;
     npairmap=(size_t)ncond*ncond*nedge ;
     if( npairmap>(size_t)INT_MAX ) ERROR_exit(PROGRAM_NAME ": too many pair-map bricks") ;
     if( npairmap>0 && (size_t)nvox>((size_t)-1)/npairmap ) ERROR_exit(PROGRAM_NAME ": pair-map array is too large") ;
     total=npairmap*(size_t)nvox ; pairval=(float *)calloc(total,sizeof(float)) ;
     if( pairval==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate %lu pair-map values",(unsigned long)total) ;
   }
   insync_progress_init(&progress,progress_mode,quiet,nmask) ;

#ifdef USE_OMP
#pragma omp parallel reduction(+:invalid_total,boot_invalid_total,temporal_invalid_total,condition_invalid_total) reduction(min:bad_data)
#endif
   {
     /* Every worker owns all temporary arrays used for one voxel.  Only final
        image writes target disjoint voxels; max-null arrays merge afterward. */
     int si ;
     int tt ;
     int gg ;
     int ci ;
     size_t nscratch=(nedge>(size_t)nsub)?nedge:(size_t)nsub ;
     float *data=(float *)malloc(sizeof(float)*(size_t)ndset*ntime) ;
     float *scratch=(float *)malloc(sizeof(float)*nscratch) ;
     float *ref=(float *)malloc(sizeof(float)*(size_t)ntime) ;
     float *values=(float *)malloc(sizeof(float)*(size_t)nsub) ;
     float *rank1=(corr_metric==SIM_SPEARMAN)?(float *)malloc(sizeof(float)*(size_t)ntime):NULL ;
     float *rank2=(corr_metric==SIM_SPEARMAN)?(float *)malloc(sizeof(float)*(size_t)ntime):NULL ;
     int *sample=(int *)malloc(sizeof(int)*(size_t)nsub) ;
     int *sel0=(int *)malloc(sizeof(int)*(size_t)nsub) ;
     int *sel1=(int *)malloc(sizeof(int)*(size_t)nsub) ;
     byte *goodtime=(missing_policy==MISSING_COMMON)?(byte *)malloc((size_t)ntime):NULL ;
     float *bootdraw=(nboot>0)?(float *)malloc(sizeof(float)*(size_t)nboot):NULL ;
     float *permnull=(pset!=NULL)?(float *)malloc(sizeof(float)*(size_t)pset->nperm):NULL ;
     float *my_max=(pset!=NULL)?(float *)malloc(sizeof(float)*(size_t)pset->nperm):NULL ;
     float *my_max_neg=(pset!=NULL && tail==INSYNC_TAIL_BI)
                        ?(float *)malloc(sizeof(float)*(size_t)pset->nperm):NULL ;
     float *tnull=(tresult!=NULL)?(float *)malloc(sizeof(float)*(size_t)ngroup*nnull):NULL ;
     float *t_mymax=(tresult!=NULL)?(float *)malloc(sizeof(float)*(size_t)nnull):NULL ;
     float *surrogate=(tresult!=NULL)?(float *)malloc(sizeof(float)*(size_t)nsub*ntime):NULL ;
     float *lagtab=(tset!=NULL && method==METHOD_PAIRWISE)?(float *)malloc(sizeof(float)*nedge*ntime):NULL ;
     float *lagprep=(tset!=NULL && method==METHOD_PAIRWISE)?(float *)malloc(sizeof(float)*(size_t)nsub*ntime):NULL ;
     float *lagnorm=(tset!=NULL && method==METHOD_PAIRWISE)?(float *)malloc(sizeof(float)*(size_t)nsub):NULL ;
     complex *phspec=(phset!=NULL)?(complex *)malloc(sizeof(complex)*(size_t)nsub*ntime):NULL ;
     complex *phwork=(phset!=NULL)?(complex *)malloc(sizeof(complex)*(size_t)ntime):NULL ;
     float *cdata=(have_contrast)?(float *)malloc(sizeof(float)*(size_t)2*nsub*ntime):NULL ;
     float *ceffect=(have_contrast)?(float *)malloc(sizeof(float)*(size_t)neffect):NULL ;
     float *geffect=(have_contrast)?(float *)malloc(sizeof(float)*(size_t)ngroup):NULL ;
     float *condnull=(cset!=NULL)?(float *)malloc(sizeof(float)*(size_t)neffect*cset->nperm):NULL ;
     float *c_mymax=(cset!=NULL)?(float *)malloc(sizeof(float)*(size_t)cset->nperm):NULL ;
     THD_simmat *sm=THD_simmat_new(nsub) ;
     THD_simmat *nullsm=(tresult!=NULL && method==METHOD_PAIRWISE)?THD_simmat_new(nsub):NULL ;
     THD_simmat *csm=(have_contrast && method==METHOD_PAIRWISE)?THD_simmat_new(2*nsub):NULL ;
     THD_simmat *pairsm=(pairval!=NULL)?THD_simmat_new(ndset):NULL ;

     if( data==NULL || scratch==NULL || ref==NULL || values==NULL || sample==NULL ||
         sel0==NULL || sel1==NULL || sm==NULL || (nboot>0 && bootdraw==NULL) ||
         (corr_metric==SIM_SPEARMAN && (rank1==NULL || rank2==NULL)) ||
         (missing_policy==MISSING_COMMON && goodtime==NULL) ||
         (pset!=NULL && (permnull==NULL || my_max==NULL ||
                          (tail==INSYNC_TAIL_BI && my_max_neg==NULL))) ||
         (tresult!=NULL && (tnull==NULL || t_mymax==NULL || surrogate==NULL)) ||
         (tset!=NULL && method==METHOD_PAIRWISE && (lagtab==NULL || lagprep==NULL || lagnorm==NULL || nullsm==NULL)) ||
         (phset!=NULL && (phspec==NULL || phwork==NULL || (method==METHOD_PAIRWISE && nullsm==NULL))) ||
         (have_contrast && (cdata==NULL || ceffect==NULL || geffect==NULL ||
                            (method==METHOD_PAIRWISE && csm==NULL))) ||
         (cset!=NULL && (condnull==NULL || c_mymax==NULL)) || (pairval!=NULL && pairsm==NULL) )
       ERROR_exit(PROGRAM_NAME ": cannot allocate per-worker buffers") ;
     if( pset!=NULL ) for( si=0 ; si<pset->nperm ; si++ ) my_max[si]=-FLT_MAX ;
     if( my_max_neg!=NULL ) for( si=0 ; si<pset->nperm ; si++ )
       my_max_neg[si]=-FLT_MAX ;
     if( tresult!=NULL ) for( si=0 ; si<nnull ; si++ ) t_mymax[si]=-FLT_MAX ;
     if( cset!=NULL ) for( si=0 ; si<cset->nperm ; si++ ) c_mymax[si]=-FLT_MAX ;

#ifdef USE_OMP
#pragma omp for schedule(dynamic,32)
#endif
     for( vv=0 ; vv<nvox ; vv++ ){
       int nt=ntime ;
       int hasbad=0 ;
       if( mask!=NULL && !mask[vv] ) continue ;

       /* Materialize this voxel as condition-major, subject-major time
          series so all estimators and null models share one memory layout. */
       for( ci=0 ; ci<ncond ; ci++ ) for( si=0 ; si<nsub ; si++ ) for( tt=0 ; tt<ntime ; tt++ )
       {
         int dd=ci*nsub+si ;
         int ot=time_index[tt] ;
         float x=THD_get_voxel(dset[si*ncond+ci],vv,ot) ;
         data[(size_t)dd*ntime+tt]=x ;
         if( missing_policy==MISSING_ERROR && !isfinite(x) ){
           long long code=((long long)vv*ndset+dd)*ntime_input+ot ;
           if( code<bad_data ) bad_data=code ;
           hasbad=1 ;
         }
       }
       if( hasbad ){
         invalid_total+=(long long)ncond*ngroup ;
         insync_progress_advance(&progress) ;
         continue ;
       }
       if( missing_policy==MISSING_COMMON ){
         int dd ;
         int ngood=0 ;

         /* A time point survives only if it is finite for every subject and
            condition, keeping every correlation on the same local timeline. */
         for( tt=0 ; tt<ntime ; tt++ ){
           int good=1 ;
           for( dd=0 ; dd<ndset ; dd++ )
             if( !isfinite(data[(size_t)dd*ntime+tt]) ){ good=0 ; break ; }
           goodtime[tt]=(byte)good ; if( good ) ngood++ ;
         }
         if( ngood>=1 ) for( dd=0 ; dd<ndset ; dd++ ){
           int pos=0 ;
           float *src=data+(size_t)dd*ntime ;
           float *dst=data+(size_t)dd*ngood ;
           for( tt=0 ; tt<ntime ; tt++ ) if( goodtime[tt] ) dst[pos++]=src[tt] ;
         }
         nt=ngood ;
       }
       if( validtr_slot>=0 ) outval[validtr_slot][vv]=(float)nt ;
       if( nt<3 ){
         invalid_total+=(long long)ncond*ngroup ;
         insync_progress_advance(&progress) ;
         continue ;
       }

       if( pairval!=NULL && !THD_simmat_fill_from_features(pairsm,nt,data,corr_metric,rank1,rank2) ){
         int aa ;
         int bb ;
         int pair=0 ;
         for( aa=0 ; aa<nsub ; aa++ ) for( bb=aa+1 ; bb<nsub ; bb++,pair++ )
           for( ci=0 ; ci<ncond ; ci++ ) for( cc=0 ; cc<ncond ; cc++ ){
             size_t pm=((size_t)ci*ncond+cc)*nedge+pair ;
             float rr=pairsm->mat[((size_t)ci*nsub+aa)*ndset+(size_t)cc*nsub+bb] ;
             pairval[pm*(size_t)nvox+vv]=insync_fisher_z(rr) ;
           }
       }

       for( ci=0 ; ci<ncond ; ci++ ){
         float *cblock=data+(size_t)ci*nsub*nt ;
         float gstat[2]={NAN,NAN} ;
         int pair_ok=1 ;
         if( (method==METHOD_PAIRWISE || pairval!=NULL) &&
             THD_simmat_fill_from_features(sm,nt,cblock,corr_metric,rank1,rank2) ) pair_ok=0 ;
         for( gg=0 ; gg<ngroup ; gg++ ){
           float stat ;
           int nv=0 ;
           int bs=2*(ci*ngroup+gg) ;
           if( method==METHOD_PAIRWISE && pair_ok ) stat=INSYNC_pairwise_indexed(nsub,sm->mat,gcount[gg],gmember[gg],summary,scratch,&nv) ;
           else if( method==METHOD_PAIRWISE ) stat=NAN ;
           else stat=INSYNC_loo_indexed_metric(nsub,nt,cblock,gcount[gg],gmember[gg],
                     summary,corr_metric,ref,values,rank1,rank2,&nv) ;
           if( isfinite(stat) ) outval[bs][vv]=stat ; else invalid_total++ ;
           outval[bs+1][vv]=(float)nv ; if( tresult!=NULL ) tnull[(size_t)gg*nnull]=stat ;
           if( gg<2 ) gstat[gg]=stat ;
         }
         if( diff_slot>=0 && isfinite(gstat[0]) && isfinite(gstat[1]) ) outval[diff_slot+ci][vv]=gstat[0]-gstat[1] ;

         if( nboot>0 ) for( gg=0 ; gg<ngroup ; gg++ ){
           int bb ;
           int ndraw=0 ;
           int nm=gcount[gg] ;
           if( method==METHOD_PAIRWISE && !pair_ok ){ boot_invalid_total++ ; continue ; }
           for( bb=0 ; bb<nboot ; bb++ ){
             int aa ;
             int bb2 ;
             int nu=0 ;
             int *ix=bset->index+(size_t)bb*nsub ;
             float st ;

             /* Pairwise bootstrap samples may repeat subjects.  Count unique
                identities and never create a subject's self-edge. */
             for( aa=0 ; aa<nm ; aa++ ) sample[aa]=ix[gmember[gg][aa]] ;
             for( aa=0 ; aa<nm ; aa++ ){
               for( bb2=0 ; bb2<aa ; bb2++ ) if( sample[bb2]==sample[aa] ) break ;
               if( bb2==aa ) nu++ ;
             }
             if( nu<2 ) continue ;
             if( method==METHOD_PAIRWISE ) st=INSYNC_pairwise_indexed(nsub,sm->mat,nm,sample,summary,scratch,NULL) ;
             else st=INSYNC_loo_indexed_metric(nsub,nt,cblock,nm,sample,summary,
                      corr_metric,ref,values,rank1,rank2,NULL) ;
             if( isfinite(st) ) bootdraw[ndraw++]=st ;
           }
           if( ndraw>=10 ){
             int bs=boot_slot+2*(ci*ngroup+gg) ;
             outval[bs][vv]=INSYNC_percentile(bootdraw,ndraw,0.5f*boot_alpha) ;
             outval[bs+1][vv]=INSYNC_percentile(bootdraw,ndraw,1.0f-0.5f*boot_alpha) ;
           } else boot_invalid_total++ ;
         }

         if( tresult!=NULL ){
           int ss ;
           int prep_good=!(method==METHOD_PAIRWISE && !pair_ok) ;

           /* The identity surrogate occupies slot zero.  Remaining slots are
              generated from the same synchronized null set at every voxel. */
           if( prep_good && tset!=NULL && method==METHOD_PAIRWISE &&
               THD_simmat_lag_table(nsub,nt,cblock,corr_metric,tsneed,lagtab,lagprep,lagnorm) ) prep_good=0 ;
           if( prep_good && phset!=NULL && insync_phase_prepare(nsub,nt,cblock,phspec) ) prep_good=0 ;
           for( ss=1 ; prep_good && ss<nnull ; ss++ ){
             if( tset!=NULL ){
               int *off=tset->offset+(size_t)ss*nsub ;
               if( method==METHOD_PAIRWISE ){
                 if( THD_simmat_from_lag_table(nullsm,nt,lagtab,off) ){ prep_good=0 ; break ; }
               } else insync_shift_draw(nsub,nt,cblock,off,surrogate) ;
             } else {
               if( insync_phase_draw(phset,ss,phspec,phwork,surrogate) ){ prep_good=0 ; break ; }
               if( method==METHOD_PAIRWISE && THD_simmat_fill_from_features(nullsm,nt,surrogate,corr_metric,rank1,rank2) ){ prep_good=0 ; break ; }
             }
             for( gg=0 ; gg<ngroup ; gg++ )
               tnull[(size_t)gg*nnull+ss]=(method==METHOD_PAIRWISE)
                 ?INSYNC_pairwise_indexed(nsub,nullsm->mat,gcount[gg],gmember[gg],summary,scratch,NULL)
                 :INSYNC_loo_indexed_metric(nsub,nt,surrogate,gcount[gg],gmember[gg],
                    summary,corr_metric,ref,values,rank1,rank2,NULL) ;
           }
           for( gg=0 ; gg<ngroup ; gg++ ){
             int good=prep_good ;
             int e=(ci*ngroup+gg)*nvox+vv ;
             float obs=tnull[(size_t)gg*nnull] ;
             if( !isfinite(obs) ) good=0 ;
             for( ss=1 ; good && ss<nnull ; ss++ ) if( !isfinite(tnull[(size_t)gg*nnull+ss]) ) good=0 ;
             if( good ){
               float ocmp=(temporal_tail==PERM_TAIL_TWO)?fabsf(obs):obs ;
               temporal_valid[e]=1 ; tresult->stat[e]=obs ;
               for( ss=0 ; ss<nnull ; ss++ ){
                 float st=tnull[(size_t)gg*nnull+ss] ;
                 float cmp=(temporal_tail==PERM_TAIL_TWO)?fabsf(st):st ;
                 if( cmp>=ocmp ) tresult->cnt_unc[e]++ ; if( cmp>t_mymax[ss] ) t_mymax[ss]=cmp ;
               }
             } else temporal_invalid_total++ ;
           }
         }

         if( pset!=NULL && isfinite(gstat[0]) && isfinite(gstat[1]) ){
           int ip ;
           int good=1 ;
           int e=ci*nvox+vv ;
           float obs=gstat[0]-gstat[1] ;

           /* Relabel whole subjects, then reconstruct each group from the
              pooled matrix/time-series block.  Pairwise edges are not the
              exchangeable observations. */
           for( ip=0 ; ip<pset->nperm ; ip++ ){
             int pos0=0 ;
             int pos1=gcount[0] ;
             int *pp=pset->perm+(size_t)ip*nsub ;
             float st0 ;
             float st1 ;
             for( si=0 ; si<nsub ; si++ ) if( group[si]==0 ) sample[pos0++]=pp[si] ; else sample[pos1++]=pp[si] ;
             if( method==METHOD_PAIRWISE ){
               st0=INSYNC_pairwise_indexed(nsub,sm->mat,gcount[0],sample,summary,scratch,NULL) ;
               st1=INSYNC_pairwise_indexed(nsub,sm->mat,gcount[1],sample+gcount[0],summary,scratch,NULL) ;
             } else {
               st0=INSYNC_loo_indexed_metric(nsub,nt,cblock,gcount[0],sample,summary,
                       corr_metric,ref,values,rank1,rank2,NULL) ;
               st1=INSYNC_loo_indexed_metric(nsub,nt,cblock,gcount[1],sample+gcount[0],
                       summary,corr_metric,ref,values,rank1,rank2,NULL) ;
             }
             if( !isfinite(st0) || !isfinite(st1) ){ good=0 ; break ; } permnull[ip]=st0-st1 ;
           }
           if( good ){
             int neg=(tail==INSYNC_TAIL_BI && obs<0.0f) ;
             float ocmp=(tail==PERM_TAIL_TWO)?fabsf(obs):(neg?-obs:obs) ;
             infer_valid[e]=1 ; presult->stat[e]=obs ;
             for( ip=0 ; ip<pset->nperm ; ip++ ){
               float st=permnull[ip] ;
               float cmp=(tail==PERM_TAIL_TWO)?fabsf(st):(neg?-st:st) ;
               if( cmp>=ocmp ) presult->cnt_unc[e]++ ;
               if( tail==INSYNC_TAIL_BI ){
                 /* AFNI bisided inference keeps direction-specific maxima;
                    a negative observed effect is ranked against -minimum. */
                 if( st>my_max[ip] ) my_max[ip]=st ;
                 if( -st>my_max_neg[ip] ) my_max_neg[ip]=-st ;
               } else if( cmp>my_max[ip] ) my_max[ip]=cmp ;
             }
           }
         }
       }

       if( have_contrast ){
         int ip ;
         int e ;

         /* Condition effects retain subject pairing.  The optional null
            swaps C1/C2 within each subject using synchronized sign patterns. */
         for( e=0 ; e<neffect ; e++ ) ceffect[e]=NAN ;
         memcpy(cdata,data+(size_t)contrast_c0*nsub*nt,sizeof(float)*(size_t)nsub*nt) ;
         memcpy(cdata+(size_t)nsub*nt,data+(size_t)contrast_c1*nsub*nt,sizeof(float)*(size_t)nsub*nt) ;
         if( method!=METHOD_PAIRWISE || !THD_simmat_fill_from_features(csm,nt,cdata,corr_metric,rank1,rank2) ){
           for( gg=0 ; gg<ngroup ; gg++ ){
             int mm=gcount[gg] ;
             float a ;
             float b ;
             for( si=0 ; si<mm ; si++ ){ sel0[si]=gmember[gg][si] ; sel1[si]=nsub+gmember[gg][si] ; }
             if( method==METHOD_PAIRWISE ){
               a=INSYNC_pairwise_indexed(2*nsub,csm->mat,mm,sel0,summary,scratch,NULL) ;
               b=INSYNC_pairwise_indexed(2*nsub,csm->mat,mm,sel1,summary,scratch,NULL) ;
             } else {
               a=INSYNC_loo_indexed_metric(2*nsub,nt,cdata,mm,sel0,summary,
                    corr_metric,ref,values,rank1,rank2,NULL) ;
               b=INSYNC_loo_indexed_metric(2*nsub,nt,cdata,mm,sel1,summary,
                    corr_metric,ref,values,rank1,rank2,NULL) ;
             }
             ceffect[gg]=a-b ; if( isfinite(ceffect[gg]) ) outval[contrast_slot+gg][vv]=ceffect[gg] ;
             else condition_invalid_total++ ;
           }
           if( ngroup==2 ){
             ceffect[ngroup]=ceffect[0]-ceffect[1] ;
             if( isfinite(ceffect[ngroup]) ) outval[contrast_slot+ngroup][vv]=ceffect[ngroup] ;
           }
           if( cset!=NULL ){
             int allgood=1 ;
             for( ip=0 ; ip<cset->nperm ; ip++ ){
               signed char *sgn=cset->sign+(size_t)ip*nsub ;
               for( gg=0 ; gg<ngroup ; gg++ ) geffect[gg]=NAN ;
               for( gg=0 ; gg<ngroup ; gg++ ){
                 int mm=gcount[gg] ;
                 float a ;
                 float b ;
                 for( si=0 ; si<mm ; si++ ){
                   int sj=gmember[gg][si] ;
                   int sw=(sgn[sj]<0) ;
                   sel0[si]=sj+sw*nsub ; sel1[si]=sj+(1-sw)*nsub ;
                 }
                 if( method==METHOD_PAIRWISE ){
                   a=INSYNC_pairwise_indexed(2*nsub,csm->mat,mm,sel0,summary,scratch,NULL) ;
                   b=INSYNC_pairwise_indexed(2*nsub,csm->mat,mm,sel1,summary,scratch,NULL) ;
                 } else {
                   a=INSYNC_loo_indexed_metric(2*nsub,nt,cdata,mm,sel0,summary,
                        corr_metric,ref,values,rank1,rank2,NULL) ;
                   b=INSYNC_loo_indexed_metric(2*nsub,nt,cdata,mm,sel1,summary,
                        corr_metric,ref,values,rank1,rank2,NULL) ;
                 }
                 geffect[gg]=a-b ;
                 condnull[(size_t)gg*cset->nperm+ip]=geffect[gg] ;
                 if( !isfinite(geffect[gg]) ) allgood=0 ;
               }
               if( ngroup==2 )
                 condnull[(size_t)ngroup*cset->nperm+ip]=geffect[0]-geffect[1] ;
             }
             for( e=0 ; e<neffect ; e++ ){
               int good=allgood ;
               int elem=e*nvox+vv ;
               float obs=ceffect[e] ;
               if( !isfinite(obs) ) good=0 ;
               if( good ){
                 float ocmp=(condition_tail==PERM_TAIL_TWO)?fabsf(obs):obs ;
                 condition_valid[elem]=1 ; cresult->stat[elem]=obs ;
                 for( ip=0 ; ip<cset->nperm ; ip++ ){
                   float st=condnull[(size_t)e*cset->nperm+ip] ;
                   float cmp=(condition_tail==PERM_TAIL_TWO)?fabsf(st):st ;
                   if( cmp>=ocmp ) cresult->cnt_unc[elem]++ ; if( cmp>c_mymax[ip] ) c_mymax[ip]=cmp ;
                 }
               }
             }
           }
         }
       }
       insync_progress_advance(&progress) ;
     }

     if( pset!=NULL ){
#ifdef USE_OMP
#pragma omp critical(insync_group_max)
#endif
       { for( si=0 ; si<pset->nperm ; si++ ){
           if( my_max[si]>presult->max_null[si] ) presult->max_null[si]=my_max[si] ;
           if( my_max_neg!=NULL && my_max_neg[si]>group_max_neg[si] )
             group_max_neg[si]=my_max_neg[si] ;
         } }
     }
     if( tresult!=NULL ){
#ifdef USE_OMP
#pragma omp critical(insync_temporal_max)
#endif
       { for( si=0 ; si<nnull ; si++ ) if( t_mymax[si]>tresult->max_null[si] ) tresult->max_null[si]=t_mymax[si] ; }
     }
     if( cset!=NULL ){
#ifdef USE_OMP
#pragma omp critical(insync_condition_max)
#endif
       { for( si=0 ; si<cset->nperm ; si++ ) if( c_mymax[si]>cresult->max_null[si] ) cresult->max_null[si]=c_mymax[si] ; }
     }
     free(data); free(scratch); free(ref); free(values); free(rank1); free(rank2);
     free(sample); free(sel0); free(sel1); free(goodtime);
     free(bootdraw); free(permnull); free(my_max); free(my_max_neg);
     free(tnull); free(t_mymax); free(surrogate);
     free(lagtab); free(lagprep); free(lagnorm); free(phspec); free(phwork);
     free(cdata); free(ceffect); free(geffect); free(condnull); free(c_mymax);
     THD_simmat_free(sm); THD_simmat_free(nullsm); THD_simmat_free(csm); THD_simmat_free(pairsm);
   }

   if( bad_data!=LLONG_MAX ){
     long long q=bad_data ;
     int ot=(int)(q%ntime_input) ;
     int dd ;
     int ci ;
     int si ;
     int rr ;
     q/=ntime_input ; dd=(int)(q%ndset) ; ci=dd/nsub ; si=dd%nsub ;
     rr=rowmap[si*ncond+ci] ;
     ERROR_exit(PROGRAM_NAME ": nonfinite retained input at voxel %lld, time %d, "
                "Subj %s, condition %s, dataset '%s'; use -missing common only "
                "when a synchronized complete-case estimand is intended",
                q/ndset,ot+1,cindex?cindex->level[0][si]:tab->subj[si],
                clabel[ci],tab->fname[rr]) ;
   }

   /* Convert empirical exceedance counts and synchronized max-null arrays to
      uncorrected p, signed z, BH-FDR q, and max-FWE output maps. */
   if( presult!=NULL ){
     int nelem=ncond*nvox ;
     int nvalid=0 ;
     for( ii=0 ; ii<nelem ; ii++ ) if( infer_valid[ii] ) nvalid++ ;
     if( nvalid<1 ) ERROR_exit(PROGRAM_NAME ": no valid group-permutation cells") ;
     for( ii=0 ; ii<pset->nperm ; ii++ ) if( presult->max_null[ii]==-FLT_MAX ) presult->max_null[ii]=0.0f ;
     if( tail==INSYNC_TAIL_BI ){
       for( ii=0 ; ii<pset->nperm ; ii++ )
         if( group_max_neg[ii]==-FLT_MAX ) group_max_neg[ii]=0.0f ;
       insync_finish_bisided(presult,infer_valid,group_max_neg) ;
     } else THD_perm_result_finish(presult,infer_valid) ;
     qval=(float *)malloc(sizeof(float)*(size_t)nelem) ;
     THD_bh_fdr_masked(nelem,presult->p_unc,infer_valid,qval) ;
     for( cc=0 ; cc<ncond ; cc++ ){
       size_t off=(size_t)cc*nvox ;
       size_t nb=sizeof(float)*(size_t)nvox ;
       int bs=perm_slot+5*cc ;
       memcpy(outval[bs],presult->p_unc+off,nb); memcpy(outval[bs+1],presult->z_unc+off,nb);
       memcpy(outval[bs+2],qval+off,nb); memcpy(outval[bs+3],presult->p_fwe+off,nb); memcpy(outval[bs+4],presult->z_fwe+off,nb);
     }
     if( !quiet ) INFO_message(PROGRAM_NAME ": group-permutation family contains "
                               "%d valid condition-by-voxel cells",nvalid) ;
   }
   if( tresult!=NULL ){
     int nelem=ncond*ngroup*nvox ;
     int nvalid=0 ;
     for( ii=0 ; ii<nelem ; ii++ ) if( temporal_valid[ii] ) nvalid++ ;
     if( nvalid<1 ) ERROR_exit(PROGRAM_NAME ": no valid temporal-null cells") ;
     for( ii=0 ; ii<nnull ; ii++ ) if( tresult->max_null[ii]==-FLT_MAX ) tresult->max_null[ii]=0.0f ;
     THD_perm_result_finish(tresult,temporal_valid) ; temporal_qval=(float *)malloc(sizeof(float)*(size_t)nelem) ;
     THD_bh_fdr_masked(nelem,tresult->p_unc,temporal_valid,temporal_qval) ;
     for( cc=0 ; cc<ncond ; cc++ ) for( kk=0 ; kk<ngroup ; kk++ ){
       size_t off=(size_t)(cc*ngroup+kk)*nvox ;
       size_t nb=sizeof(float)*(size_t)nvox ;
       int bs=temporal_slot+5*(cc*ngroup+kk) ;
       memcpy(outval[bs],tresult->p_unc+off,nb); memcpy(outval[bs+1],tresult->z_unc+off,nb);
       memcpy(outval[bs+2],temporal_qval+off,nb); memcpy(outval[bs+3],tresult->p_fwe+off,nb); memcpy(outval[bs+4],tresult->z_fwe+off,nb);
     }
     if( !quiet ) INFO_message(PROGRAM_NAME ": temporal family contains %d valid "
                               "condition-by-group-by-voxel cells",nvalid) ;
   }
   if( cresult!=NULL ){
     int nelem=neffect*nvox ;
     int nvalid=0 ;
     for( ii=0 ; ii<nelem ; ii++ ) if( condition_valid[ii] ) nvalid++ ;
     if( nvalid<1 ) ERROR_exit(PROGRAM_NAME ": no valid condition-swap cells") ;
     for( ii=0 ; ii<cset->nperm ; ii++ ) if( cresult->max_null[ii]==-FLT_MAX ) cresult->max_null[ii]=0.0f ;
     THD_perm_result_finish(cresult,condition_valid) ; condition_qval=(float *)malloc(sizeof(float)*(size_t)nelem) ;
     THD_bh_fdr_masked(nelem,cresult->p_unc,condition_valid,condition_qval) ;
     for( kk=0 ; kk<neffect ; kk++ ){
       size_t off=(size_t)kk*nvox ;
       size_t nb=sizeof(float)*(size_t)nvox ;
       int bs=condperm_slot+5*kk ;
       memcpy(outval[bs],cresult->p_unc+off,nb); memcpy(outval[bs+1],cresult->z_unc+off,nb);
       memcpy(outval[bs+2],condition_qval+off,nb); memcpy(outval[bs+3],cresult->p_fwe+off,nb); memcpy(outval[bs+4],cresult->z_fwe+off,nb);
     }
     if( !quiet ) INFO_message(PROGRAM_NAME ": condition-swap family contains "
                               "%d valid effect-by-voxel cells",nvalid) ;
   }

   /* Assemble one AFNI bucket after all parallel work is complete.  Z-like
      bricks get FIZT metadata so AFNI can display their p-value thresholds. */
   out=EDIT_empty_copy(first) ;
   EDIT_dset_items(out,ADN_prefix,prefix,ADN_nvals,nout,ADN_ntt,0,ADN_brick_fac,NULL,
                   ADN_type,HEAD_FUNC_TYPE,ADN_func_type,FUNC_BUCK_TYPE,ADN_datum_all,MRI_float,ADN_none) ;
   tross_Copy_History(first,out) ; tross_Make_History(PROGRAM_NAME,argc,argv,out) ;
   for( cc=0 ; cc<ncond ; cc++ ) for( kk=0 ; kk<ngroup ; kk++ ){
     char who[256] ;
     char label[64] ;
     int bs=2*(cc*ngroup+kk) ;
     if( ncond>1 ) snprintf(who,sizeof(who),"%s_%s",clabel[cc],glabel[kk]) ; else snprintf(who,sizeof(who),"%s",glabel[kk]) ;
     insync_brick_label(label,sizeof(label),"ISC",who); EDIT_substitute_brick(out,bs,MRI_float,outval[bs]); EDIT_BRICK_LABEL(out,bs,label); EDIT_BRICK_TO_NOSTAT(out,bs);
     insync_brick_label(label,sizeof(label),"N",who); EDIT_substitute_brick(out,bs+1,MRI_float,outval[bs+1]); EDIT_BRICK_LABEL(out,bs+1,label); EDIT_BRICK_TO_NOSTAT(out,bs+1);
   }
   if( diff_slot>=0 ) for( cc=0 ; cc<ncond ; cc++ ){
     char who[256] ;
     char label[64] ;
     char both[160] ;
     snprintf(both,sizeof(both),"%s-%s",glabel[0],glabel[1]) ;
     if( ncond>1 ) snprintf(who,sizeof(who),"%s_%s",clabel[cc],both); else snprintf(who,sizeof(who),"%s",both);
     insync_brick_label(label,sizeof(label),"ISC",who); EDIT_substitute_brick(out,diff_slot+cc,MRI_float,outval[diff_slot+cc]); EDIT_BRICK_LABEL(out,diff_slot+cc,label); EDIT_BRICK_TO_NOSTAT(out,diff_slot+cc);
   }
   if( nboot>0 ) for( cc=0 ; cc<ncond ; cc++ ) for( kk=0 ; kk<ngroup ; kk++ ){
     char who[256] ;
     char label[64] ;
     int bs=boot_slot+2*(cc*ngroup+kk) ;
     if( ncond>1 ) snprintf(who,sizeof(who),"%s_%s",clabel[cc],glabel[kk]); else snprintf(who,sizeof(who),"%s",glabel[kk]);
     insync_brick_label(label,sizeof(label),"BootLo",who); EDIT_substitute_brick(out,bs,MRI_float,outval[bs]); EDIT_BRICK_LABEL(out,bs,label); EDIT_BRICK_TO_NOSTAT(out,bs);
     insync_brick_label(label,sizeof(label),"BootHi",who); EDIT_substitute_brick(out,bs+1,MRI_float,outval[bs+1]); EDIT_BRICK_LABEL(out,bs+1,label); EDIT_BRICK_TO_NOSTAT(out,bs+1);
   }
   if( pset!=NULL ) for( cc=0 ; cc<ncond ; cc++ ){
     static char *stem[5]={"P","Z","Q","PFWE","ZFWE"} ;
     char who[256] ;
     char label[64] ;
     int bs=perm_slot+5*cc ;
     if( ncond>1 ) snprintf(who,sizeof(who),"%s_%s-%s",clabel[cc],glabel[0],glabel[1]); else snprintf(who,sizeof(who),"%s-%s",glabel[0],glabel[1]);
     for( kk=0 ; kk<5 ; kk++ ){ insync_brick_label(label,sizeof(label),stem[kk],who); EDIT_substitute_brick(out,bs+kk,MRI_float,outval[bs+kk]); EDIT_BRICK_LABEL(out,bs+kk,label); if(kk==1||kk==4)EDIT_BRICK_TO_FIZT(out,bs+kk);else EDIT_BRICK_TO_NOSTAT(out,bs+kk); }
   }
   if( tresult!=NULL ) for( cc=0 ; cc<ncond ; cc++ ) for( kk=0 ; kk<ngroup ; kk++ ){
     static char *stem[5]={"TP","TZ","TQ","TPFWE","TZFWE"} ;
     char who[256] ;
     char label[64] ;
     int jj2 ;
     int bs=temporal_slot+5*(cc*ngroup+kk) ;
     if( ncond>1 ) snprintf(who,sizeof(who),"%s_%s",clabel[cc],glabel[kk]); else snprintf(who,sizeof(who),"%s",glabel[kk]);
     for( jj2=0 ; jj2<5 ; jj2++ ){ insync_brick_label(label,sizeof(label),stem[jj2],who); EDIT_substitute_brick(out,bs+jj2,MRI_float,outval[bs+jj2]); EDIT_BRICK_LABEL(out,bs+jj2,label); if(jj2==1||jj2==4)EDIT_BRICK_TO_FIZT(out,bs+jj2);else EDIT_BRICK_TO_NOSTAT(out,bs+jj2); }
   }
   if( have_contrast ) for( kk=0 ; kk<neffect ; kk++ ){
     char who[320] ;
     char label[64] ;
     if( kk<ngroup ) snprintf(who,sizeof(who),"%s-%s_%s",contrast_name0,contrast_name1,glabel[kk]) ;
     else snprintf(who,sizeof(who),"%s-%s_%s-%s",glabel[0],glabel[1],contrast_name0,contrast_name1) ;
     insync_brick_label(label,sizeof(label),(kk<ngroup)?"Cond":"GxCond",who); EDIT_substitute_brick(out,contrast_slot+kk,MRI_float,outval[contrast_slot+kk]); EDIT_BRICK_LABEL(out,contrast_slot+kk,label); EDIT_BRICK_TO_NOSTAT(out,contrast_slot+kk);
     if( cset!=NULL ){
       static char *stem[5]={"CP","CZ","CQ","CPFWE","CZFWE"} ;
       int jj2 ;
       int bs=condperm_slot+5*kk ;
       for( jj2=0 ; jj2<5 ; jj2++ ){ insync_brick_label(label,sizeof(label),stem[jj2],who); EDIT_substitute_brick(out,bs+jj2,MRI_float,outval[bs+jj2]); EDIT_BRICK_LABEL(out,bs+jj2,label); if(jj2==1||jj2==4)EDIT_BRICK_TO_FIZT(out,bs+jj2);else EDIT_BRICK_TO_NOSTAT(out,bs+jj2); }
     }
   }
   if( validtr_slot>=0 ){
     EDIT_substitute_brick(out,validtr_slot,MRI_float,outval[validtr_slot]) ;
     EDIT_BRICK_LABEL(out,validtr_slot,"ValidTR") ;
     EDIT_BRICK_TO_NOSTAT(out,validtr_slot) ;
   }
   DSET_write(out) ; if( !quiet ) WROTE_DSET(out) ;

   if( pairval!=NULL ){
     FILE *fp ;
     char table_name[THD_MAX_NAME] ;
     char dataset_name[THD_MAX_NAME] ;
     size_t pm=0 ;

     /* Pair maps are stored in condition-pair-major, subject-edge-minor
        order.  The emitted tables point directly to those brick selectors. */
     pairout=EDIT_empty_copy(first) ;
     EDIT_dset_items(pairout,ADN_prefix,pair_prefix,ADN_nvals,(int)npairmap,ADN_ntt,0,ADN_brick_fac,NULL,
                     ADN_type,HEAD_FUNC_TYPE,ADN_func_type,FUNC_BUCK_TYPE,ADN_datum_all,MRI_float,ADN_none) ;
     tross_Copy_History(first,pairout); tross_Make_History(PROGRAM_NAME,argc,argv,pairout);
     for( cc=0 ; cc<ncond ; cc++ ) for( jj=0 ; jj<ncond ; jj++ ){
       int aa ;
       int bb ;
       int pair=0 ;
       for( aa=0 ; aa<nsub ; aa++ ) for( bb=aa+1 ; bb<nsub ; bb++,pair++,pm++ ){
         char la[64] ;
         char lb[64] ;
         char lc0[64] ;
         char lc1[64] ;
         char label[64] ;
         char who[256] ;
         const char *sa=cindex?cindex->level[0][aa]:tab->subj[aa] ;
         const char *sb=cindex?cindex->level[0][bb]:tab->subj[bb] ;
         insync_safe_token(la,sizeof(la),sa); insync_safe_token(lb,sizeof(lb),sb); insync_safe_token(lc0,sizeof(lc0),clabel[cc]); insync_safe_token(lc1,sizeof(lc1),clabel[jj]);
         snprintf(who,sizeof(who),"%s_%s_%s_%s",lc0,lc1,la,lb); insync_brick_label(label,sizeof(label),"Z",who);
         EDIT_substitute_brick(pairout,(int)pm,MRI_float,pairval+pm*(size_t)nvox); EDIT_BRICK_LABEL(pairout,(int)pm,label); EDIT_BRICK_TO_NOSTAT(pairout,(int)pm);
       }
     }
     DSET_write(pairout); if( !quiet ) WROTE_DSET(pairout);
     snprintf(dataset_name,sizeof(dataset_name),"%s",DSET_BRIKNAME(pairout));
     snprintf(table_name,sizeof(table_name),ncond==1?"%s.3dISC.txt":"%s.3dISC.all.txt",
              pair_prefix) ;
     if( !THD_ok_overwrite() && THD_is_file(table_name) )
       ERROR_exit(PROGRAM_NAME ": 3dISC table '%s' already exists",table_name) ;
     fp=fopen(table_name,"w");
     if( fp==NULL ) ERROR_exit(PROGRAM_NAME ": cannot write '%s'",table_name) ;
     insync_3disc_header(fp,tab,icol_group,cindex?cindex->icol[1]:-1) ;
     pm=0 ;
     for( cc=0 ; cc<ncond ; cc++ ) for( jj=0 ; jj<ncond ; jj++ ){
       int aa ;
       int bb ;
       for( aa=0 ; aa<nsub ; aa++ ) for( bb=aa+1 ; bb<nsub ; bb++,pm++ ){
         insync_3disc_row(fp,tab,cindex,rowmap,ncond,clabel,group,glabel,
                          cc,jj,aa,bb,icol_group,dataset_name,pm) ;
       }
     }
     fclose(fp) ;

     /* Current 3dISC requires exactly one row for every unique subject pair.
        A repeated-condition table therefore has to be split by ordered
        condition pairing. Keep the complete table above for provenance and
        write directly runnable nchoose2-row tables here. */
     if( ncond>1 ) for( cc=0 ; cc<ncond ; cc++ ) for( jj=0 ; jj<ncond ; jj++ ){
       int aa ;
       int bb ;
       char ca[64] ;
       char cb[64] ;
       insync_safe_token(ca,sizeof(ca),clabel[cc]) ;
       insync_safe_token(cb,sizeof(cb),clabel[jj]) ;
       snprintf(table_name,sizeof(table_name),"%s.%s__%s.3dISC.txt",
                pair_prefix,ca,cb) ;
       if( !THD_ok_overwrite() && THD_is_file(table_name) )
         ERROR_exit(PROGRAM_NAME ": 3dISC table '%s' already exists",table_name) ;
       fp=fopen(table_name,"w") ;
       if( fp==NULL ) ERROR_exit(PROGRAM_NAME ": cannot write '%s'",table_name) ;
       insync_3disc_header(fp,tab,icol_group,cindex->icol[1]) ;
       pm=((size_t)cc*ncond+jj)*nedge ;
       for( aa=0 ; aa<nsub ; aa++ ) for( bb=aa+1 ; bb<nsub ; bb++,pm++ )
         insync_3disc_row(fp,tab,cindex,rowmap,ncond,clabel,group,glabel,
                          cc,jj,aa,bb,icol_group,dataset_name,pm) ;
       fclose(fp) ;
     }
     if( !quiet ) INFO_message(PROGRAM_NAME ": wrote %lu Fisher-z pair maps and %s",
                    (unsigned long)npairmap,
                    (ncond==1)?"one ready-to-run 3dISC table":
                               "one ready-to-run 3dISC table per condition pairing") ;
   }

   if( atlas_name!=NULL )
     insync_write_roi_outputs(atlas_name,roi_sel,prefix,matrix_name,first,dset,
       nsub,ncond,ntime_input,ntime,time_index,mask,missing_policy,corr_metric,
       method,summary,ngroup,group,gcount,gmember,glabel,tab,cindex,rowmap,
       clabel,quiet) ;

   if( !quiet ) INFO_message(PROGRAM_NAME ": estimator=%s correlation=%s "
                "summary=%s, %d subjects, %d condition%s, %d retained time points",
                (method==METHOD_PAIRWISE)?"pairwise":"loo",
                (corr_metric==SIM_PEARSON)?"pearson":"spearman",
                (summary==INSYNC_SUMMARY_MEDIAN)?"median":"fisher_mean",
                nsub,ncond,(ncond==1)?"":"s",ntime) ;
   if( invalid_total>0 ) WARNING_message(PROGRAM_NAME ": %lld group estimates were invalid and stored as zero",invalid_total) ;
   if( boot_invalid_total>0 ) WARNING_message(PROGRAM_NAME ": %lld bootstrap intervals had fewer than 10 usable draws",boot_invalid_total) ;
   if( temporal_invalid_total>0 ) WARNING_message(PROGRAM_NAME ": %lld temporal tests were invalid",temporal_invalid_total) ;
   if( condition_invalid_total>0 ) WARNING_message(PROGRAM_NAME ": %lld planned condition effects were invalid",condition_invalid_total) ;
   return 0 ;
}
