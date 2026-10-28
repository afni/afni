/* Command-line interface; dataset processing lives in thd_insync.c. */
#include "mrilib.h"
#include "thd_insync.h"
#include "thd_permute.h"
#include "thd_simmatrix.h"

#include <errno.h>

#ifdef USE_OMP
#include <omp.h>
#endif

#define PROGRAM_NAME "3dInSync"

static char *insync_options[] = {
   "-prefix", "-mask", "-isc_method", "-summary", "-group_column",
   "-nboot", "-boot_alpha", "-nperm", "-exact", "-seed", "-tail",
   "-1sided", "-2sided", "-bisided",
   "-temporal_null", "-nnull", "-min_shift", "-temporal_tail",
   "-condition_column", "-condition_contrast", "-ncondperm",
   "-condition_exact", "-condition_tail", "-save_pairwise",
   "-correlation", "-censor", "-zcensor", "-zcensor_warn", "-missing", "-atlas", "-roi_sel", "-roi_only", "-float16",
   "-save_matrix", "-edges", "-edges_matrix", "-edges_rss", "-edges_events", "-edges_nnull",
   "-memory_limit", "-memory_override", "-quiet", "-progress",
   "-dataTable", "-dataTableFile", "-show_table", "-help", "-h", NULL
} ;

/*! Print the complete command-line help and AFNI/OpenMP build information. */
static void usage_3dInSync(void)
{
   printf(
"\n"
"Usage: 3dInSync [options]                                      ~1~\n"
"\n"
"==========================================================================\n"
"NOTE: For most intersubject correlation analyses, use 3dISC.  Its\n"
"linear mixed-effects framework (with crossed random effects for subject\n"
"pairs) is the established way to model ISC with covariates, groups, and\n"
"repeated conditions, and it should be your first choice.\n"
"\n"
"3dInSync is still useful for several reasons, which is why it can also\n"
"write ready-to-run 3dISC commands (see the pair-map options below):\n"
"  * It starts from time series, so you do not need to compute and assemble\n"
"    the subject-pair correlation maps that 3dISC requires as input.\n"
"  * It offers nonparametric inference that 3dISC does not: temporal nulls,\n"
"    subject bootstrap, and group permutation.  These are the resampling\n"
"    approaches commonly used by other ISC packages (for example BrainIAK\n"
"    and the ISC toolbox), so 3dInSync lets you run familiar nonparametric\n"
"    tests within AFNI and compare them with those tools.\n"
"  * It offers alternatives such as leave-one-out ISC, Spearman correlation,\n"
"    fisher_mean/median summaries, and ROI-wise output from an atlas.\n"
"  * It is a quick way to inspect ISC maps or sanity-check an analysis, and\n"
"    to compare results against a 3dISC model of the same data.\n"
"==========================================================================\n"
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
"                       voxels are analyzed.  Output datasets follow the first\n"
"                       input's view (+orig, +tlrc) and template space, unless\n"
"                       MASK (or, with no -mask, -atlas) is in a different\n"
"                       view; then they take MASK's, as 3dRSA does.  So\n"
"                       template-space data that are labeled +orig, analyzed\n"
"                       with a +tlrc mask, are written +tlrc, and subject-space\n"
"                       data with a subject-space mask stay +orig.\n"
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
"  -zcensor             Omit a time point when any subject/condition input\n"
"                       volume is all zero in the analysis mask. This creates\n"
"                       one common retained timeline, like -censor.\n"
"\n"
"  -zcensor_warn PCT    Warn for each subject/condition input with more than\n"
"                       PCT percent all-zero volumes [20].\n"
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
"  -roi_only            Requires -atlas.  Run the ENTIRE analysis (bootstrap,\n"
"                       permutation, temporal null, condition contrasts, pair\n"
"                       maps) on the atlas-parcel mean time series, with one\n"
"                       'voxel' per ROI, instead of on voxels.  Each ROI mean\n"
"                       is formed as the datasets are read, so only\n"
"                       subjects x ROIs x time points are held in memory\n"
"                       (megabytes, not the full 4D grid) and FDR/FWE families\n"
"                       are the ROIs.  -mask, when present, intersects the\n"
"                       atlas first.  The bucket is painted back onto the\n"
"                       input grid (every voxel of an ROI gets its ROI value)\n"
"                       and PREFIX.roi.1D lists every statistic, one row per\n"
"                       ROI.  Not combined with -save_matrix or -edges*.\n"
"\n"
"  -float16             Hold the subject data in memory as 16-bit floats, half\n"
"                       the size of the default 32-bit.  Each voxel series is\n"
"                       stored as a z-score, with its mean and SD kept in\n"
"                       32 bits, so rounding error is relative to each voxel's\n"
"                       own variability (about 1e-3 of its SD per sample; ISC\n"
"                       differs from 32-bit storage by about 1e-4 or less).\n"
"                       Use it when the default would not fit;\n"
"                       do not mix results from the two storage modes within\n"
"                       one analysis.  Without -mask this stores every grid\n"
"                       voxel.\n"
"\n"
"  Memory.  Subject data are held in the smallest of: the datasets as stored\n"
"  on disk (no -mask), only the in-mask voxels (with -mask, chosen\n"
"  automatically when smaller), or -roi_only ROI series.  Datasets are read\n"
"  one at a time, so peak memory while loading is the compact store plus a\n"
"  single dataset.  The preflight estimate reports which representation was\n"
"  chosen and its size, using the datatype actually stored.\n"
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
"Edge (cofluctuation) ISC:                                  ~1~\n"
"\n"
"  An edge time series is the moment-by-moment product of two ROI signals,\n"
"  z_i(t)*z_j(t), where each ROI series is z-scored within subject (sample\n"
"  SD). Its mean over time is the ordinary Pearson r of the pair. These\n"
"  options ask whether subjects share those moment-to-moment cofluctuations.\n"
"  All need -atlas. The ROI series for every subject and condition are\n"
"  reduced to one common retained timeline (see -censor, -zcensor, -missing).\n"
"\n"
"  -edges               ISC of the edge series of every ROI pair, with the\n"
"                       -isc_method, -summary, -correlation and group settings\n"
"                       used elsewhere. Writes PREFIX.edge.1D: one row per\n"
"                       condition, group and ROI pair, with the pair's ISC and\n"
"                       the ISC of each of its two ROIs (Node1ISC, Node2ISC)\n"
"                       side by side so edge synchrony can be read against\n"
"                       node synchrony.\n"
"\n"
"  -edges_matrix        With -edges, also write one square matrix file per\n"
"                       condition and group, PREFIX_edge_[COND_]GROUP.netcc, in\n"
"                       the 3dNetCorr .netcc layout for tools that read it. CC\n"
"                       is edge ISC (r) and FZ its Fisher z. The DIAGONAL holds\n"
"                       NODE ISC, not 1.0. With -edges_nnull it adds EX (Excess),\n"
"                       P and PFWE matrices. The tables carry the same numbers.\n"
"\n"
"  -edges_rss           ISC of the global cofluctuation amplitude, RSS(t) =\n"
"                       sqrt(sum over pairs of (z_i z_j)^2): do subjects'\n"
"                       whole-brain cofluctuation rise and fall together?\n"
"                       Adds RSS_ISC and RSS_N columns to PREFIX.frames.1D.\n"
"\n"
"  -edges_events FRAC   In each subject, call the FRAC fraction of frames with\n"
"                       the highest RSS(t) 'events' [FRAC in (0,0.5]]. For each\n"
"                       frame count how many subjects of a group have an event\n"
"                       there. Adds NHigh, NSubj and Frac to PREFIX.frames.1D.\n"
"\n"
"  PREFIX.frames.1D has one row per condition, group and retained frame, with\n"
"  MeanRSS (the subject-mean amplitude) and the columns for the options given.\n"
"  The group-level RSS values repeat down each condition/group block.\n"
"\n"
"  -edges_nnull N       Surrogate sets (at least 20, identity set included) for\n"
"                       the nulls below. -min_shift and -seed apply. Cost grows\n"
"                       with N times the number of edges times the number of\n"
"                       subject pairs: use -roi_sel or a coarse atlas first.\n"
"                         -edges       : ROI j of each pair is circularly\n"
"                                        shifted by ONE offset common to all\n"
"                                        subjects, so each ROI's own ISC is\n"
"                                        unchanged while the within-subject\n"
"                                        i-j coupling is broken. Adds NullMean,\n"
"                                        Excess (= ISC - NullMean), P, Z, Q,\n"
"                                        PFWE and ZFWE to PREFIX.edge.1D. P and\n"
"                                        PFWE cover condition x group x edge.\n"
"                         -edges_rss,\n"
"                         -edges_events: each subject is shifted independently.\n"
"                                        Adds RSS_NullMean and RSS_P, and the\n"
"                                        per-frame EvNullMean, EvP, EvZ, EvQ,\n"
"                                        EvPFWE and EvZFWE (max over frames,\n"
"                                        conditions and groups), to the frames\n"
"                                        table.\n"
"\n"
"  Cautions for edge ISC:\n"
"    * Edge ISC is largely inherited from node ISC. If ROIs i and j are each\n"
"      synchronized across subjects, their product is too, even when their\n"
"      coupling is not shared. Always read edge ISC next to Node1ISC and\n"
"      Node2ISC. 'Excess' under -edges_nnull is the quantity that\n"
"      speaks to shared coupling beyond the nodes.\n"
"    * Edge series are products, so they are heavy-tailed and dominated by\n"
"      high-amplitude frames. That is often the point; -correlation spearman\n"
"      gives a robust alternative.\n"
"    * These are stimulus-locked measures. They need subjects aligned in\n"
"      stimulus time and are not meaningful for unlocked data such as rest.\n"
"    * ROI series are z-scored over the retained frames only, so edge values\n"
"      depend on censoring. A constant ROI series gives a zero edge.\n"
"    * Group-label permutation and condition contrasts are not yet applied to\n"
"      edges, and edge analyses do not yet support the 3dISC bridge\n"
"      (-save_pairwise applies to the voxelwise maps only).\n"
"\n"
"  Reading edge ISC against the node ISCs (Excess = ISC - NullMean):\n"
"    * A rough expectation for edge ISC from the nodes alone is the PRODUCT of\n"
"      the two node ISCs when their shared drivers are independent; NullMean is\n"
"      the empirical version of that. An edge ISC below either node ISC is\n"
"      therefore normal.\n"
"    * One node high, the other near zero, edge near zero: expected. The\n"
"      product of a shared signal and an idiosyncratic one is idiosyncratic.\n"
"    * Both nodes high, Excess near zero: both regions follow the stimulus, each\n"
"      by its own driver, and the drivers are not time-locked to each other.\n"
"      This is shared activity, not shared coupling. A clearly negative Excess\n"
"      is unusual: look at timing or polarity differences across subjects.\n"
"    * Edge high, nodes low or negative: the regions cofluctuate at the same\n"
"      moments in everyone though each region's own course differs. Possible\n"
"      causes are response polarity or latency differing across subjects (sign\n"
"      cancels in the product), stimulus-locked changes in coupling strength, or\n"
"      a shared artifact. Check -correlation spearman and the frames table.\n"
"    * Excess clearly positive (small P, PFWE): the relationship between the\n"
"      regions is time-locked across subjects beyond what their separate\n"
"      synchrony gives. Read this as time-locked coupling, not as proof of\n"
"      functional connectivity. Rule out shared nuisance signals (motion,\n"
"      respiration, global signal, arousal), spatial blur between neighboring\n"
"      ROIs, and a few extreme frames. The null breaks only i-j timing.\n"
"    * Edge high and Excess near zero: the edge only passes along the nodes'\n"
"      synchrony. Report the nodes, not the edge.\n"
"    * With a median over pairs, negative node ISC can simply mean the sample's\n"
"      majority polarity is opposite. Do not read it as anti-synchrony.\n"
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
"  This program's analysis is static ISC, plus the explicit edge modes above.\n"
"  Dynamic/windowed ISC, ISFC, and ISPS require distinct estimands, outputs,\n"
"  and null models and are not accepted as aliases here. They remain\n"
"  candidates for explicit future modes.\n"
"\n") ;
   PRINT_AFNI_OMP_USAGE(PROGRAM_NAME,NULL) ;
   PRINT_COMPILE_DATE ;
}

/*! Parse the command line and hand the analysis to the dataset driver. */
int main( int argc, char **argv )
{
   int nopt=1 ;
   THD_datatable *tab=NULL ;
   INSYNC_options opts ;

   INSYNC_options_default(&opts) ;
   if( argc<2 ){ usage_3dInSync() ; return 0 ; }
   mainENTRY(PROGRAM_NAME " main") ; machdep() ; AFNI_SETUP_OMP(0) ;
   AFNI_logger(PROGRAM_NAME,argc,argv) ; PRINT_VERSION(PROGRAM_NAME) ;
   THD_check_AFNI_version(PROGRAM_NAME) ;

   /* Parse first, then validate option combinations together in INSYNC_run.
      This keeps parsing order from changing program semantics. */
   while( nopt<argc && argv[nopt][0]=='-' ){
     if( strcasecmp(argv[nopt],"-help")==0 || strcasecmp(argv[nopt],"-h")==0 ){
       usage_3dInSync() ; return 0 ;
     }
     if( strcasecmp(argv[nopt],"-prefix")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an argument after -prefix") ;
       opts.prefix=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-mask")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a dataset after -mask") ;
       opts.maskname=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-isc_method")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need pairwise or loo after -isc_method") ;
       if( strcasecmp(argv[nopt],"pairwise")==0 ) opts.method=INSYNC_METHOD_PAIRWISE ;
       else if( strcasecmp(argv[nopt],"loo")==0 ) opts.method=INSYNC_METHOD_LOO ;
       else ERROR_exit(PROGRAM_NAME ": -isc_method must be pairwise or loo") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-summary")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need median or fisher_mean after -summary") ;
       if( strcasecmp(argv[nopt],"median")==0 ) opts.summary=INSYNC_SUMMARY_MEDIAN ;
       else if( strcasecmp(argv[nopt],"fisher_mean")==0 ||
                strcasecmp(argv[nopt],"mean")==0 ) opts.summary=INSYNC_SUMMARY_FISHER_MEAN ;
       else ERROR_exit(PROGRAM_NAME ": -summary must be median or fisher_mean") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-correlation")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need pearson or spearman after -correlation") ;
       if( strcasecmp(argv[nopt],"pearson")==0 ||
           strcasecmp(argv[nopt],"corr")==0 ) opts.corr_metric=SIM_PEARSON ;
       else if( strcasecmp(argv[nopt],"spearman")==0 ||
                strcasecmp(argv[nopt],"scorr")==0 ) opts.corr_metric=SIM_SPEARMAN ;
       else ERROR_exit(PROGRAM_NAME ": -correlation must be pearson or spearman") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-censor")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a 1D file after -censor") ;
       opts.censor_name=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-zcensor")==0 ){
       opts.do_zcensor=1 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-zcensor_warn")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a percentage after -zcensor_warn") ;
       opts.zcensor_warn=(float)strtod(argv[nopt++],NULL) ; opts.zcensor_warn_given=1 ; continue ;
     }
     if( strcasecmp(argv[nopt],"-missing")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need error or common after -missing") ;
       if( strcasecmp(argv[nopt],"error")==0 ) opts.missing_policy=INSYNC_MISSING_ERROR ;
       else if( strcasecmp(argv[nopt],"common")==0 ) opts.missing_policy=INSYNC_MISSING_COMMON ;
       else ERROR_exit(PROGRAM_NAME ": -missing must be error or common") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-atlas")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a dataset after -atlas") ;
       opts.atlas_name=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-roi_sel")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a label list after -roi_sel") ;
       opts.roi_sel=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-roi_only")==0 ){ opts.roi_only=1 ; nopt++ ; continue ; }
     if( strcasecmp(argv[nopt],"-float16")==0 ){ opts.use_half=1 ; nopt++ ; continue ; }
     if( strcasecmp(argv[nopt],"-save_matrix")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a filename after -save_matrix") ;
       opts.matrix_name=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-edges")==0 ){ opts.eo.edges=1 ; nopt++ ; continue ; }
     if( strcasecmp(argv[nopt],"-edges_matrix")==0 ){ opts.eo.matrix=1 ; nopt++ ; continue ; }
     if( strcasecmp(argv[nopt],"-edges_rss")==0 ){ opts.eo.rss=1 ; nopt++ ; continue ; }
     if( strcasecmp(argv[nopt],"-edges_events")==0 ){
       char *ep ;
       double x ;
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a fraction after -edges_events") ;
       x=strtod(argv[nopt++],&ep) ;
       if( ep==argv[nopt-1] || *ep!='\0' || !(x>0.0 && x<=0.5) )
         ERROR_exit(PROGRAM_NAME ": -edges_events FRAC must be in (0,0.5]") ;
       opts.eo.events=1 ; opts.eo.event_frac=(float)x ; continue ;
     }
     if( strcasecmp(argv[nopt],"-edges_nnull")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an integer after -edges_nnull") ;
       opts.eo.nnull=(int)strtol(argv[nopt++],NULL,10) ;
       if( opts.eo.nnull<20 ) ERROR_exit(PROGRAM_NAME ": -edges_nnull must be at least 20") ;
       continue ;
     }
     if( strcasecmp(argv[nopt],"-memory_limit")==0 ){
       char *ep ;
       double x ;
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need GiB after -memory_limit") ;
       errno=0 ; x=strtod(argv[nopt++],&ep) ;
       if( errno==ERANGE || ep==argv[nopt-1] || *ep!='\0' || !isfinite(x) || x<=0.0 )
         ERROR_exit(PROGRAM_NAME ": -memory_limit must be a positive finite GiB value") ;
       opts.memory_limit_gib=x ; opts.memory_limit_given=1 ; continue ;
     }
     if( strcasecmp(argv[nopt],"-memory_override")==0 ){
       opts.memory_override=1 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-quiet")==0 ){
       opts.quiet=1 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-progress")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need auto, bar, line, or off after -progress") ;
       if( strcasecmp(argv[nopt],"auto")==0 ) opts.progress_mode=INSYNC_PROGRESS_AUTO ;
       else if( strcasecmp(argv[nopt],"bar")==0 ) opts.progress_mode=INSYNC_PROGRESS_BAR ;
       else if( strcasecmp(argv[nopt],"line")==0 ) opts.progress_mode=INSYNC_PROGRESS_LINE ;
       else if( strcasecmp(argv[nopt],"off")==0 ) opts.progress_mode=INSYNC_PROGRESS_OFF ;
       else ERROR_exit(PROGRAM_NAME ": -progress must be auto, bar, line, or off") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-group_column")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a name after -group_column") ;
       opts.group_column=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-condition_column")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a name after -condition_column") ;
       opts.condition_column=argv[nopt++] ; opts.condition_column_given=1 ; continue ;
     }
     if( strcasecmp(argv[nopt],"-condition_contrast")==0 ){
       if( opts.have_contrast ) ERROR_exit(PROGRAM_NAME ": give -condition_contrast only once") ;
       if( nopt+2>=argc ) ERROR_exit(PROGRAM_NAME ": need two labels after -condition_contrast") ;
       opts.contrast_name0=argv[++nopt] ; opts.contrast_name1=argv[++nopt] ; nopt++ ;
       opts.have_contrast=1 ; continue ;
     }
     if( strcasecmp(argv[nopt],"-ncondperm")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an integer after -ncondperm") ;
       opts.ncondperm=(int)strtol(argv[nopt++],NULL,10) ;
       if( opts.ncondperm<20 ) ERROR_exit(PROGRAM_NAME ": -ncondperm must be at least 20") ;
       continue ;
     }
     if( strcasecmp(argv[nopt],"-condition_exact")==0 ){
       opts.condition_exact=1 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-condition_tail")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need two or one after -condition_tail") ;
       if( strcasecmp(argv[nopt],"two")==0 || strcasecmp(argv[nopt],"two-sided")==0 )
         opts.condition_tail=PERM_TAIL_TWO ;
       else if( strcasecmp(argv[nopt],"one")==0 || strcasecmp(argv[nopt],"upper")==0 ||
                strcasecmp(argv[nopt],"one-sided")==0 ) opts.condition_tail=PERM_TAIL_ONE ;
       else ERROR_exit(PROGRAM_NAME ": -condition_tail must be two or one") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-save_pairwise")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a prefix after -save_pairwise") ;
       opts.pair_prefix=argv[nopt++] ; continue ;
     }
     if( strcasecmp(argv[nopt],"-nboot")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an integer after -nboot") ;
       opts.nboot=(int)strtol(argv[nopt++],NULL,10) ;
       if( opts.nboot<100 ) ERROR_exit(PROGRAM_NAME ": -nboot must be at least 100") ;
       continue ;
     }
     if( strcasecmp(argv[nopt],"-boot_alpha")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a number after -boot_alpha") ;
       opts.boot_alpha=(float)strtod(argv[nopt++],NULL) ;
       if( !(opts.boot_alpha>0.0f && opts.boot_alpha<1.0f) )
         ERROR_exit(PROGRAM_NAME ": -boot_alpha must be between 0 and 1") ;
       continue ;
     }
     if( strcasecmp(argv[nopt],"-nperm")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an integer after -nperm") ;
       opts.nperm=(int)strtol(argv[nopt++],NULL,10) ;
       if( opts.nperm<20 ) ERROR_exit(PROGRAM_NAME ": -nperm must be at least 20") ;
       continue ;
     }
     if( strcasecmp(argv[nopt],"-exact")==0 ){ opts.do_exact=1 ; nopt++ ; continue ; }
     if( strcasecmp(argv[nopt],"-seed")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an integer after -seed") ;
       opts.seed=strtol(argv[nopt++],NULL,10) ; continue ;
     }
     if( strcasecmp(argv[nopt],"-1sided")==0 ){
       opts.tail=PERM_TAIL_ONE ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-2sided")==0 ){
       opts.tail=PERM_TAIL_TWO ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-bisided")==0 ){
       opts.tail=INSYNC_TAIL_BI ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-tail")==0 ){
       if( ++nopt>=argc )
         ERROR_exit(PROGRAM_NAME ": need 1sided, 2sided, or bisided after -tail") ;
       if( strcasecmp(argv[nopt],"2sided")==0 || strcasecmp(argv[nopt],"two")==0 ||
           strcasecmp(argv[nopt],"two-sided")==0 )
         opts.tail=PERM_TAIL_TWO ;
       else if( strcasecmp(argv[nopt],"1sided")==0 || strcasecmp(argv[nopt],"one")==0 ||
                strcasecmp(argv[nopt],"upper")==0 ||
                strcasecmp(argv[nopt],"one-sided")==0 ) opts.tail=PERM_TAIL_ONE ;
       else if( strcasecmp(argv[nopt],"bisided")==0 ) opts.tail=INSYNC_TAIL_BI ;
       else ERROR_exit(PROGRAM_NAME ": -tail must be 1sided, 2sided, or bisided") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-temporal_null")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need timeshift or phase after -temporal_null") ;
       if( strcasecmp(argv[nopt],"timeshift")==0 || strcasecmp(argv[nopt],"shift")==0 )
         opts.temporal_mode=INSYNC_TNULL_TIMESHIFT ;
       else if( strcasecmp(argv[nopt],"phase")==0 ) opts.temporal_mode=INSYNC_TNULL_PHASE ;
       else ERROR_exit(PROGRAM_NAME ": -temporal_null must be timeshift or phase") ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-nnull")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an integer after -nnull") ;
       opts.nnull=(int)strtol(argv[nopt++],NULL,10) ;
       if( opts.nnull<20 ) ERROR_exit(PROGRAM_NAME ": -nnull must be at least 20") ;
       continue ;
     }
     if( strcasecmp(argv[nopt],"-min_shift")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need an integer after -min_shift") ;
       opts.min_shift=(int)strtol(argv[nopt++],NULL,10) ; opts.min_shift_given=1 ;
       if( opts.min_shift<1 ) ERROR_exit(PROGRAM_NAME ": -min_shift must be positive") ;
       continue ;
     }
     if( strcasecmp(argv[nopt],"-temporal_tail")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need one or two after -temporal_tail") ;
       if( strcasecmp(argv[nopt],"one")==0 || strcasecmp(argv[nopt],"upper")==0 ||
           strcasecmp(argv[nopt],"one-sided")==0 ) opts.temporal_tail=PERM_TAIL_ONE ;
       else if( strcasecmp(argv[nopt],"two")==0 || strcasecmp(argv[nopt],"two-sided")==0 )
         opts.temporal_tail=PERM_TAIL_TWO ;
       else ERROR_exit(PROGRAM_NAME ": -temporal_tail must be one or two") ;
       opts.temporal_tail_given=1 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-show_table")==0 ){ opts.show_table=1 ; nopt++ ; continue ; }
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

   if( nopt<argc ) ERROR_exit(PROGRAM_NAME ": unexpected argument '%s'",argv[nopt]) ;
   return INSYNC_run(&opts,tab,argc,argv) ;
}
