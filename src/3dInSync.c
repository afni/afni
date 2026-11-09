/* Command-line interface; dataset processing lives in thd_insync.c. */
#include "mrilib.h"
#include "thd_insync.h"
#include "thd_insync_select.h"
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
   "-temporal_null", "-temporal_censor", "-nnull", "-min_shift", "-temporal_tail",
   "-condition_column", "-condition_contrast", "-ncondperm",
   "-condition_exact", "-condition_tail", "-save_pairwise",
   "-isc_model", "-isc_qVars", "-isc_qVarCenters", "-isc_gltCode", "-isc_jobs",
   "-correlation", "-censor", "-censor_column", "-zcensor", "-zcensor_warn", "-missing", "-atlas", "-roi_sel", "-roi_only", "-float16",
   "-save_matrix", "-edges", "-edges_matrix", "-edges_rss", "-edges_events", "-edges_nnull",
   "-memory_limit", "-memory_override", "-quiet", "-progress",
   "-check_table", "-whole_summary", "-window", "-step", "-run_starts", "-tr",
   "-block_summary", "-block_missing", "-min_blocks", "-min_block_frac", "-block_weight",
   "-min_valid", "-min_valid_frac", "-combine_blocks", "-segment_center",
   "-xmat_stims", "-xmat_blocks", "-xmat_threshold", "-xmat_across_runs", "-xmat_runs",
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
"                       must remain. Censored temporal nulls require explicit\n"
"                       -temporal_censor joint_shift (see censoring example below).\n"
"\n"
"  -censor_column NAME  Optional per-source censor column [Censor]. Files are\n"
"                       finite 0/1 vectors on the full input timeline. Intersect\n"
"                       all subjects/conditions with global -censor/-zcensor.\n"
"                       Static ISC keeps its output layout and inference;\n"
"                       temporal censoring uses explicit joint_shift mode. Save\n"
"                       PREFIX.censor.tsv and PREFIX.common_censor.1D.\n"
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
"                       Run/block/window analyses write PREFIX.isc/ bundles; see below.\n"
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
"Numbered examples: whole input, runs, blocks, and sliding windows ~1~\n"
"\n"
"  Examples 1-2 and 5a use the same basic subjects.txt table:\n"
"  Subj Group InputFile\n"
"  s01  A     s01/bold.nii.gz\n"
"  s02  A     s02/bold.nii.gz\n"
"  s03  A     s03/bold.nii.gz\n"
"  s04  B     s04/bold.nii.gz\n"
"  s05  B     s05/bold.nii.gz\n"
"  s06  B     s06/bold.nii.gz\n"
"  Include every subject, with at least three subjects per group. Subjects must\n"
"  share stimulus time. Inputs retain their full original timelines.\n"
"\n"
"  1. Original whole-run / whole-input ISC\n"
"\n"
"     3dInSync -prefix whole_isc -isc_method pairwise -summary median \\\n"
"              -dataTableFile subjects.txt\n"
"\n"
"     One estimate across the entire input timeline; no Xmat or window options\n"
"     are needed. If inputs contain concatenated runs, this uses all their TRs\n"
"     together. It does not average separate run correlations.\n"
"     Optional subject inference: add -nboot 2000 and/or -nperm 10000 -seed 314159.\n"
"     The group-permutation example requires the two groups shown above.\n"
"\n"
"     1b. Whole-input ISC with per-subject censor files\n"
"\n"
"     3dInSync -prefix whole_censored -nboot 2000 \\\n"
"              -dataTableFile subjects_censored.txt\n"
"\n"
"     Add a Censor column to the SAME main table, for example:\n"
"     Subj Group Censor        InputFile\n"
"     s01  A     s01.censor.1D s01/bold.nii.gz\n"
"     s02  A     s02.censor.1D s02/bold.nii.gz\n"
"     s03  A     s03.censor.1D s03/bold.nii.gz\n"
"     # Continue with the other subjects if using two groups.\n"
"     A 1 retains a TR; a 0 censors it. Common support intersects source masks.\n"
"\n"
"  2. Separate ISC for each run\n"
"\n"
"     3dInSync -run_starts 0,200 -prefix run_isc \\\n"
"              -dataTableFile subjects.txt\n"
"\n"
"     For two concatenated runs, run 1 starts at original TR 0 and run 2 at TR\n"
"     200. Each run gets its own estimate. Supply the actual zero-based starts.\n"
"     Add -whole_summary to ALSO obtain the whole-input estimate from example 1.\n"
"     An Xmat table can supply run boundaries instead of -run_starts.\n"
"\n"
"  Examples 3-4 and 5b use subjects_xmat.txt (one row per subject):\n"
"  Subj Group Xmat                  InputFile\n"
"  s01  A     s01/X.nocensor.xmat.1D s01/bold.nii.gz\n"
"  s02  A     s02/X.nocensor.xmat.1D s02/bold.nii.gz\n"
"  s03  A     s03/X.nocensor.xmat.1D s03/bold.nii.gz\n"
"  s04  B     s04/X.nocensor.xmat.1D s04/bold.nii.gz\n"
"  s05  B     s05/X.nocensor.xmat.1D s05/bold.nii.gz\n"
"  s06  B     s06/X.nocensor.xmat.1D s06/bold.nii.gz\n"
"  Here Vis and Aud are example StimLabels; substitute your design's labels.\n"
"  Blocks are modeled-response support above the chosen peak fraction, not\n"
"  recovered stimulus onset/duration files. The full design defines boundaries.\n"
"  See Xmat-defined runs and blocks below for regressors and censor inputs.\n"
"\n"
"  3. Separate ISC for each block WITHIN ONE selected run\n"
"\n"
"     3dInSync -xmat_blocks each -xmat_stims Vis,Aud -xmat_runs 1 \\\n"
"              -xmat_threshold 0.5 -prefix run1_blocks \\\n"
"              -dataTableFile subjects_xmat.txt\n"
"\n"
"     One estimate per individual Vis/Aud block in run 1. No sliding occurs:\n"
"     each estimate uses that whole block's retained samples. Run IDs are\n"
"     one-based. Choose the response threshold explicitly for your design.\n"
"\n"
"  4. Separate ISC for each block IN EACH run\n"
"\n"
"     3dInSync -xmat_blocks each -xmat_stims Vis,Aud \\\n"
"              -xmat_threshold 0.5 -prefix all_run_blocks \\\n"
"              -dataTableFile subjects_xmat.txt\n"
"\n"
"     Omitting -xmat_runs selects all runs; -xmat_runs 1,2 selects a subset.\n"
"     Each individual block retains its run identity. Blocks are not combined.\n"
"\n"
"     4b. Also summarize same-type blocks separately for each run\n"
"\n"
"     3dInSync -xmat_blocks by_stim -xmat_stims Vis,Aud \\\n"
"              -xmat_threshold 0.5 -block_summary median -prefix type_per_run \\\n"
"              -dataTableFile subjects_xmat.txt\n"
"\n"
"     Retain individual block results AND add one Vis and one Aud aggregate per\n"
"     run. Correlate within blocks, summarize corresponding pair/LOO components\n"
"     across blocks, then summarize within groups. This is not concatenation.\n"
"     Add -xmat_across_runs only if the desired type aggregate spans runs.\n"
"\n"
"  5. Sliding-window ISC\n"
"\n"
"     5a. Slide separately within each run\n"
"\n"
"     3dInSync -run_starts 0,200 -window 15 -step 1 -prefix sliding_runs \\\n"
"              -dataTableFile subjects.txt\n"
"\n"
"     Windows have 15 TRs and advance by 1 TR. With TR=2 seconds, these are\n"
"     30-second windows advancing every 2 seconds. No window crosses a run join.\n"
"\n"
"     5b. Slide within each individual block, separately in each run\n"
"\n"
"     3dInSync -xmat_blocks each -xmat_stims Vis,Aud -xmat_threshold 0.5 \\\n"
"              -window 15 -step 1 -prefix sliding_blocks \\\n"
"              -dataTableFile subjects_xmat.txt\n"
"\n"
"     Restart the sliding schedule at each block's beginning. Windows never\n"
"     cross block/run boundaries. Blocks shorter than 15 TRs contribute no\n"
"     windows; incomplete trailing windows are omitted and short blocks reported.\n"
"     Add -xmat_runs 1 to restrict this example to one run. Sliding windows\n"
"     require each, not by_stim; this does not average matched window positions\n"
"     across repeated blocks.\n"
"\n"
"  6. Age-covariate modeling WITH 3dISC, using pair maps FROM 3dInSync\n"
"\n"
"     This is a two-program workflow: 3dInSync computes and exports ISC pair maps;\n"
"     3dISC fits the mixed-effects model, age effect, and contrasts using R.\n"
"     3dInSync itself does not fit a general covariate model. Its -isc_* options\n"
"     configure the generated 3dISC commands; they do not fit the model directly.\n"
"\n"
"     Keep subject-level covariates in the main data table. This example models\n"
"     one cohort; there is no Group column or group contrast.\n"
"\n"
"     # subjects_age.txt\n"
"     Subj Age InputFile\n"
"     s01  20  s01/bold.nii.gz\n"
"     s02  22  s02/bold.nii.gz\n"
"     s03  24  s03/bold.nii.gz\n"
"     s04  26  s04/bold.nii.gz\n"
"     s05  28  s05/bold.nii.gz\n"
"     s06  30  s06/bold.nii.gz\n"
"\n"
"     # Step 1: compute pair maps and write 3dISC inputs/commands.\n"
"     3dInSync -run_starts 0,200 -prefix age_runs -save_pairwise age_models \\\n"
"              -isc_model 'I(Age1+Age2)+(1|Subj1)+(1|Subj2)' \\\n"
"              -isc_qVars Age1,Age2 -isc_qVarCenters 25,25 \\\n"
"              -isc_gltCode intercept '1 0' -isc_gltCode age_sum '0 1' \\\n"
"              -dataTableFile subjects_age.txt\n"
"\n"
"     # Step 2: call 3dISC through the generated scripts to fit the models.\n"
"     sh age_models.isc/run_all.sh\n"
"\n"
"     Age is copied into Age1/Age2 for the two members of each exported pair.\n"
"     Center each age at 25 years, then model their SUM. The intercept is at\n"
"     pair mean age 25; age_sum tests the slope per unit of the centered sum.\n"
"     I(Age1+Age2) is symmetric: exchanging pair members leaves it unchanged.\n"
"     Choose the centering age to suit the study; 25 is illustrative.\n"
"\n"
"     run_all.sh calls 3dISC once per requested run/block/window/summary. 3dISC\n"
"     does the covariate modeling; 3dInSync's export step does not execute R.\n"
"     Adding Age to the table alone does not adjust 3dInSync's ISC estimates or\n"
"     permutation tests. 3dISC and its R dependencies must be available for step 2.\n"
"\n"
"     Use -run_starts 0 for a single whole-input model. For sliding windows, add\n"
"     -window 15 -step 1. For block models, use the Xmat options from examples\n"
"     3-4 and include Age alongside Xmat in the same main data table.\n"
"\n"
"  For examples 2-5, add -check_table to inspect expanded intervals before\n"
"  estimation. Remove it to compute ISC. Runs, blocks, and windows have separate\n"
"  result buckets plus a manifest; -whole_summary adds a full-input estimate.\n"
"  Add -atlas parcels.nii.gz -roi_only to analyze parcel mean time series.\n"
"  See the inference options below to add subject or temporal inference.\n"
"  Explicit interval tables are an alternative to Xmat-defined blocks; their\n"
"  Selection/Segment/Run/StartTR/StopTR columns are described below.\n"
"\n"
"Optional run/block/window estimates:             ~1~\n"
"\n"
"  Whole-input ISC above needs no window options or Xmat. The following\n"
"  options estimate ISC for runs, blocks, windows, and summaries. Add -check_table to\n"
"  validate schedules without loading BOLD samples or computing estimates.\n"
"\n"
"  In output tables, a 'selection' means one requested run, block, sliding\n"
"  window, or combined-block/whole-input summary. SelectionID identifies that\n"
"  analysis period; it does not mean selecting significant findings. Column\n"
"  names (Selection, SelectionID) and filenames keep this technical label.\n"
"\n"
"  -check_table         Validate table, headers, and requested time intervals;\n"
"                       write PREFIX.selections.tsv and PREFIX.selection_summary.txt.\n"
"  -whole_summary       Also estimate the full original input timeline, with its\n"
"                       censor mask. Separate whole_input role; not a window mean.\n"
"  -window W            Complete sliding windows of W TRs (integer >=3).\n"
"  -step S              Window step in TRs [1]; requires -window.\n"
"  -run_starts LIST      Zero-based run starts, e.g. 0,100; first must be 0.\n"
"  -tr SEC              Supply missing TR in seconds; reject header conflicts.\n"
"  -min_valid N         Minimum retained samples per estimate [3], integer >=3.\n"
"  -min_valid_frac F    Minimum retained/nominal fraction [0], in [0,1].\n"
"                       Both gates apply after censoring and common missingness.\n"
"  -combine_blocks block_summary|pool\n"
"                       block_summary [default]: correlate within each block,\n"
"                       summarize corresponding pair/LOO values across blocks,\n"
"                       then apply the existing group -summary.\n"
"                       pool: concatenate retained segments before correlation.\n"
"  -block_summary median|fisher_mean  Across-block summary [median]; independent\n"
"                       of the group -summary. A single admitted block is identity.\n"
"  -block_missing require_all|drop_common  [require_all]. A block needs the\n"
"                       per-block -min_valid count/fraction and finite values\n"
"                       for every required pair/LOO subject in every group.\n"
"                       drop_common removes that block from ALL groups' aggregates\n"
"                       at the voxel; require_all invalidates the aggregate.\n"
"  -min_blocks N        Minimum admitted blocks per aggregate [1], integer >=1.\n"
"  -min_block_frac F    Minimum admitted/nominal block fraction [0], in [0,1].\n"
"  -block_weight equal  Equal weights only; coverage weighting is not implemented.\n"
"                       Block-summary options cannot be combined with pool.\n"
"  -segment_center none|mean  With pool only: center each retained segment [none].\n"
"\n"
"  3dInSync -window 30 -step 10 -run_starts 0,100 \134\n"
"           -prefix movie_windows -dataTableFile subjects.txt\n"
"  One result bucket per run/block/window/summary: PREFIX.sel000001.nii.gz, etc.\n"
"  One-dimensional inputs use .1D buckets. PREFIX.results.tsv lists completed\n"
"  outputs and ends with Status: complete only after all requested analyses finish.\n"
"  Individual components are retained alongside block summaries and pools.\n"
"  Block-summary buckets append BlocksNominal, BlocksEligible, BlocksUsed per\n"
"  condition. Every contributing edge/LOO subject uses exactly BlocksUsed blocks.\n"
"  PREFIX.selXXXXXX.blocks.nii.gz (or .1D) records each block's retained TRs,\n"
"  eligibility and final weight; PREFIX.block_support.tsv maps these bricks to\n"
"  original block IDs/coordinates. Weights are 1/BlocksUsed, or zero if unused.\n"
"  Aggregate ValidTR is the total over admitted blocks, zero if the aggregate\n"
"  fails. Single-block outputs retain their own descriptive validity rules.\n"
"  Support can differ across voxels/conditions; paired-condition contrasts\n"
"  require identical coordinates and admitted support (see inference below).\n"
"  Original condition/group ISC,N brick order is followed by ValidTR, Valid,\n"
"  PairComplete, and difference-validity maps. Invalid ISC is stored as zero\n"
"  with Valid=0; a real zero has Valid=1. Constant series invalidate correlations.\n"
"  PairComplete tests the within-group pair graph; it is not export permission.\n"
"  -missing error checks the requested union after common censoring; unselected\n"
"  samples are ignored except with temporal nulls, which check full source runs.\n"
"  -missing common filters across all sources/conditions.\n"
"  -zcensor keeps its global raw-volume rule. Save the final common censor vector.\n"
"\n"
"  Subject-based inference for runs, blocks, windows, and summaries:\n"
"  Add confidence intervals and group/condition tests to the ISC estimates.\n"
"  Use -nboot, -nperm/-exact, or -condition_contrast with -ncondperm/-condition_exact.\n"
"  Resampling draws are shared across requested runs/blocks/windows/summaries.\n"
"  Group FWE/FDR covers all these results x conditions x space. Condition FWE/FDR\n"
"  covers all matched results x effects x space. With -atlas, voxel AND parcel\n"
"  tests form one family; -roi_only counts each parcel once. Bootstrap CIs are pointwise.\n"
"  Results: .selXXXXXX.infer.nii.gz, .inference.tsv, .inference_draws.tsv and\n"
"  (with atlas) .inference_roi.tsv; see .results.tsv for the completion marker.\n"
"  Observed admitted blocks stay fixed during resampling; incomplete components\n"
"  or any invalid null invalidate that test cell. Paired conditions must have\n"
"  identical coordinates and admitted blocks. BootN/BootValid report usable draws\n"
"  (at least 10 for CI). Invalid inference cells have p/q=1 and InferValid=0.\n"
"  Global FDR memory is budgeted; output maps use a temporary disk spool.\n"
"\n"
"  Temporal-surrogate tests for runs, blocks, windows, and summaries:\n"
"    3dInSync -window 30 -step 10 -run_starts 0,200 \134\n"
"             -temporal_null timeshift -nnull 1000 -min_shift 10 \134\n"
"             -whole_summary -prefix window_nulls -dataTableFile subjects.txt\n"
"  Independent subject/run shifts or phases are generated on FULL original runs\n"
"  before extracting windows/blocks. Never shift across a run join. Same draws\n"
"  are reused across conditions, runs/blocks/windows/summaries and space. Draw 0 is the\n"
"  original data. -min_shift is checked against each participating run length.\n"
"  Supports individual windows/blocks, pooled segments (including mean centering),\n"
"  component-first median/Fisher block summaries, and multi-run -whole_summary.\n"
"  Fixed observed block support and require_all/drop_common apply to each draw.\n"
"  TemporalISC/TP/TZ/TQ/TPFWE/TZFWE/TInferValid appear in the inference buckets.\n"
"  Temporal FWE/FDR covers all requested runs/blocks/windows/summaries x conditions\n"
"  x groups x space, separately from group/condition tests. -temporal_tail applies.\n"
"  .temporal_draws.tsv records run seeds and shifts; phase factors use the seeded\n"
"  THD_phase_set_factor generator. Phases preserve each run's DC and Nyquist.\n"
"  All samples in participating runs must be finite and retained, including\n"
"  samples outside requested windows. All-one censor masks are allowed; actual\n"
"  censoring requires the explicit joint_shift option below. Nonparticipating\n"
"  runs may be censored. A full-input summary makes every run participate.\n"
"\n"
"  Temporal-surrogate tests with censoring (explicit opt-in):\n"
"    3dInSync -window 30 -run_starts 0,200 -temporal_null timeshift \134\n"
"             -temporal_censor joint_shift -nnull 1000 -prefix censored_windows \134\n"
"             -dataTableFile subjects_with_Censor.txt\n"
"  joint_shift moves each subject/run signal AND its source censor masks by\n"
"  the same offset. Global -censor is incorporated into each source mask before\n"
"  shifting. Intersect shifted masks across ALL subjects/conditions on every\n"
"  draw, then reapply count/fraction and block-admission gates. Never compress\n"
"  the timeline. Windows, block summaries, pooling and whole summaries work.\n"
"  No-window calls use separate result buckets (one per run).\n"
"  Retained TRs and admitted blocks can differ from those in the observed estimate.\n"
"  Every failed null draw counts as an exceedance in conservative TP/TPFWE\n"
"  bounds. No skipping or retrying. FWE uses the prespecified family including\n"
"  unresolved cells; BH includes observed-invalid cells with p=1. TNullN reports\n"
"  finite draws including identity; TInferValid indicates an estimable OBSERVED\n"
"  cell here, not a complete-null cell. Z/q are derived from the p-value bounds.\n"
"  .temporal_support.tsv reports per-draw/component common counts/fractions.\n"
"  Its component coverage flag is diagnostic; pooling gates the combined count.\n"
"  Check TNullN and the unresolved-maxima count: one unreliable component can\n"
"  make family-wide inference very conservative. This loss is not hidden.\n"
"  Excluded source frames may contain nonfinite/artifact values; own-retained\n"
"  samples must be finite throughout each participating run. Use float32 storage.\n"
"  Censored phase inference, -zcensor, -missing common, alternative censor\n"
"  cohorts, and coverage weights are not supported by this first release.\n"
"  Joint shifts assume a suitable runwise signal/mask null; they do not correct\n"
"  arbitrary task- or motion-related informative missingness.\n"
"\n"
"  ROI estimates for runs, blocks, windows, and summaries:\n"
"    3dInSync -window 30 -step 10 -atlas parcels.nii.gz -roi_only \134\n"
"             -whole_summary -prefix roi_windows -dataTableFile subjects.txt\n"
"  -atlas retains voxelwise buckets and also estimates each mask-intersected\n"
"  parcel mean time series using the same time intervals and estimator. -roi_only skips\n"
"  voxelwise estimation and expands parcel results onto the atlas grid.\n"
"  PREFIX.roi.tsv contains every ROI result/support brick with SelectionID,\n"
"  Run, Stimulus, Condition, ROI, label, voxel count and value. Run=mixed denotes\n"
"  a summary spanning runs (whole_input always uses Run=all).\n"
"  PREFIX.bricks.tsv maps every file and zero-based brick selector to its label.\n"
"  -save_matrix FILE writes ROI pair correlations for each run/block/window/summary,\n"
"  including cross-group pairs, even with LOO summaries. Invalid pairs are nan, Valid=0.\n"
"  Aggregate matrices use the same admitted blocks as the requested estimator.\n"
"  These are descriptive r values; they are not 3dISC model bundles.\n"
"  Atlas run/block/window analyses use float32; -float16 is ignored with a notice.\n"
"  Source data are read once. One run/block/window/summary's result/support buffers\n"
"  are resident at a time; scheduling metadata grows with the requested intervals.\n"
"  Memory estimates include ROI matrices, full-grid outputs and worker storage.\n"
"  Invalid output names, label collisions and overwrite conflicts fail before\n"
"  BOLD loading. Only final Status: complete in results.tsv certifies the bundle.\n"
"\n"
"  3dInSync -check_table -window 30 -step 10 -run_starts 0,100 \134\n"
"           -prefix movie_schedule -dataTableFile subjects.txt\n"
"\n"
"  For explicit intervals, repeat subject rows with these extra columns:\n"
"  Subj Group Selection Segment Run StartTR StopTR InputFile\n"
"  s01  A     Vis_all   b01     1   30      59     s01_movie.nii.gz\n"
"  s01  A     Vis_all   b02     1   90      119    s01_movie.nii.gz\n"
"  # Repeat the same interval rows for every other subject.\n"
"  Run is one-based; StartTR/StopTR are zero-based, inclusive, within run.\n"
"  A repeated Selection combines nonoverlapping segments. Sliding windows\n"
"  stay inside each segment/run; short segments produce no windows (reported).\n"
"  IDs/manifest order are canonical; original input row numbers are retained.\n"
"  -censor can report retained counts without shifting any boundaries.\n"
"  zcensor/nonfinite checks require image data and are reported as pending.\n"
"  Xmat and optional Censor columns can supply run/block metadata (see below).\n"
"  -check_table does not validate or run inference or pair-map exports.\n"
"\n"
"Xmat-defined runs and blocks (optional):            ~1~\n"
"\n"
"  One row per subject, or per subject/Condition:\n"
"  Subj Group Xmat                  InputFile\n"
"  s01  A     s01/X.nocensor.xmat.1D s01/bold.nii.gz\n"
"  s02  A     s02/X.nocensor.xmat.1D s02/bold.nii.gz\n"
"  # Include all subjects (at least three per group).\n"
"\n"
"  -xmat_blocks each|by_stim\n"
"                       each: one estimate per response-defined block.\n"
"                       by_stim: also aggregate blocks of each type per run.\n"
"                       Individual blocks are retained alongside aggregates.\n"
"  -xmat_stims LIST      Exact comma-separated StimLabels [all]; design order.\n"
"  -xmat_threshold F     REQUIRED for blocks: 0 < F <= 1 of full-design peak.\n"
"  -xmat_runs LIST       One-based comma-separated runs [all].\n"
"  -xmat_across_runs     With by_stim, aggregate each type across selected runs.\n"
"\n"
"  3dInSync -check_table -xmat_blocks by_stim -xmat_stims Vis,Aud \134\n"
"           -xmat_threshold 0.5 -prefix blocks -dataTableFile subjects.txt\n"
"\n"
"  Blocks follow modeled response support, not original stimulus timings.\n"
"  Selected stimuli need one finite, nonnegative column each. Overlapping\n"
"  stimuli retain shared TRs; overlap counts and subject-by-stimulus tables\n"
"  are printed. Censor holes never split blocks. A full design is required:\n"
"  if Xmat is censored, add an XmatFull column naming its uncensored design.\n"
"  Optional Censor files are intersected with GoodList and common -censor;\n"
"  per-subject Censor files are also supported in ordinary whole-run ISC.\n"
"  Source files must retain the full timeline (no subbrick selectors).\n"
"  Xmat alone supplies run/TR/censor metadata without selecting blocks.\n"
"  -window/-step may subdivide each block, but cannot be combined with by_stim.\n"
"  Use manual intervals or Xmat block expansion to define the analysis periods.\n"
"  Omit -check_table to estimate individual blocks AND the requested summaries.\n"
"  No ISC estimation is performed by -check_table.\n"
"\n"
"3dISC model bundles for runs, blocks, windows, and summaries: ~1~\n"
"\n"
"  -save_pairwise PREFIX  For run/block/window analyses, write PREFIX.isc/ with one\n"
"                       Fisher-z pair bucket, common-valid mask, support bucket,\n"
"                       data table and script per run/block/window/summary and ordered\n"
"                       condition pairing. Includes cross-group subject pairs.\n"
"  -isc_model FORMULA    Shared 3dISC formula [1+(1|Subj1)+(1|Subj2)].\n"
"                       The default tests an intercept, not a group difference.\n"
"  -isc_qVars LIST       Comma-separated exported numeric columns, e.g. Age1,Age2.\n"
"  -isc_qVarCenters LIST Comma-separated finite centers, in qVars order. If omitted,\n"
"                       3dISC uses its normal centering. Requires -isc_qVars.\n"
"  -isc_gltCode NAME WEIGHTS  Repeat for each GLT; quote the numeric weights.\n"
"                       Names must be unique. R checks model rank/GLT dimensions.\n"
"  -isc_jobs N           Jobs for each generated fit [1], positive integer.\n"
"                       -isc_* options require a run/block/window analysis and -save_pairwise.\n"
"\n"
"  3dInSync -window 30 -step 10 -save_pairwise movie_models \134\n"
"           -prefix movie_windows -dataTableFile subjects.txt\n"
"  # Later, explicitly run the generated models:\n"
"  sh movie_models.isc/run_all.sh\n"
"\n"
"  Model scripts are generated, never run automatically. run_all.sh stops on\n"
"  failure; each script requires a completed jobs.tsv. Scripts work from another\n"
"  working directory. They cd to the absolute bundle directory; after moving a\n"
"  bundle update those paths. All dataset/table references inside it are local.\n"
"  Xmat/Censor/interval-control columns are excluded from model covariates. Genuine\n"
"  covariates retain both copies, e.g. Age1,Age2. Column collisions, unrepresentable\n"
"  R table tokens and nonnumeric qVars fail before BOLD loading. Subject order is\n"
"  lexical. GroupPair uses lexically sorted group labels; no contrast is inferred.\n"
"  3dISC swaps subject IDs internally, but does not swap custom covariate copies.\n"
"  They are pair-slot values. A symmetric model expression such as I(Age1+Age2)\n"
"  must be chosen explicitly; no mean/difference/similarity transform is inferred.\n"
"  Cross-condition r is corr(Subj1/cond1,Subj2/cond2), not an average of orientations.\n"
"  Cross-condition jobs require matching original block/sample coordinates.\n"
"  Export is pairwise even for LOO descriptions. Block admission requires every\n"
"  modeled edge finite; aggregation then uses the same admitted blocks for all\n"
"  edges. Job-specific support maps record actual retained counts and weights.\n"
"  Fisher z is applied AFTER the final pair-r aggregate; exact endpoints are\n"
"  clamped to +/-0.999999. Model masks require every modeled edge to be finite.\n"
"  Zero-support jobs fail with an incomplete bundle; masked zero placeholders\n"
"  must not be fitted. -roi_only exports parcel-mean pair maps on the atlas grid;\n"
"  -atlas without -roi_only exports voxelwise pair maps. Outputs are NIfTI even\n"
"  for 1D inputs. One result/condition-pair export job is held in memory at a time.\n"
"  Separate fits neither test changes across windows nor correct across result buckets.\n"
"  Joint repeated-window mixed models remain deferred; 3dInSync resampling tests are available.\n"
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
"  This program supports whole-input, per-run, per-block and sliding-window ISC,\n"
"  block summaries, subject-based and temporal-surrogate inference, and edge modes.\n"
"  Additional ISFC and ISPS analyses remain candidates for explicit later modes.\n"
"\n"
"References:                                                 ~1~\n"
"\n"
"Selected methodological references and related ISC approaches.\n"
"The options above specify the methods implemented by 3dInSync.\n"
"\n"
"* Hasson, Nir, Levy, Fuhrmann & Malach (2004), Science 303(5664):1634-1640.\n"
"  [original ISC] Intersubject synchronization of cortical activity during\n"
"  natural vision. https://doi.org/10.1126/science.1089506\n"
"* Hasson, Malach & Heeger (2010), Trends Cogn Sci 14(1):40-48. [ISC review]\n"
"  Reliability of cortical activity during natural stimulation.\n"
"  https://doi.org/10.1016/j.tics.2009.10.011\n"
"* Kauppi, Jaaskelainen, Sams & Tohka (2010), Front Neuroinform 4:5.\n"
"  [frequency-specific ISC, resampling inference] Inter-subject correlation of\n"
"  brain hemodynamic responses during watching a movie: localization in space\n"
"  and frequency. https://doi.org/10.3389/fninf.2010.00005\n"
"* Kauppi, Pajula & Tohka (2014), Front Neuroinform 8:2. [ISC Toolbox]\n"
"  A versatile software package for inter-subject correlation based analyses\n"
"  of fMRI. https://doi.org/10.3389/fninf.2014.00002\n"
"* Nastase, Gazzola, Hasson & Keysers (2019), Soc Cogn Affect Neurosci\n"
"  14(6):667-685. [ISC tutorial/best practices]\n"
"  Measuring shared responses across subjects using intersubject correlation.\n"
"  https://doi.org/10.1093/scan/nsz037\n"
"* Chen, Shin, Taylor, Glen, Reynolds, Israel & Cox (2016), NeuroImage\n"
"  142:248-259. [subject-wise resampling and false-positive control]\n"
"  Untangling the relatedness among correlations, Part I: nonparametric\n"
"  approaches to inter-subject correlation analysis at the group level.\n"
"  https://doi.org/10.1016/j.neuroimage.2016.05.023\n"
"* Chen, Taylor, Shin, Reynolds & Cox (2017), NeuroImage 147:825-840.\n"
"  [LME group analysis; basis of 3dISC] Untangling the relatedness among\n"
"  correlations, Part II: inter-subject correlation group analysis through\n"
"  linear mixed-effects modeling. https://doi.org/10.1016/j.neuroimage.2016.08.029\n"
"* Chen, Taylor, Qu, Molfese, Bandettini, Cox & Finn (2020), NeuroImage\n"
"  216:116474. [Bayesian multilevel ISC background]\n"
"  Untangling the relatedness among correlations, Part III: inter-subject\n"
"  correlation analysis through Bayesian multilevel modeling for naturalistic\n"
"  scanning. https://doi.org/10.1016/j.neuroimage.2019.116474\n"
"* Pajula & Tohka (2016), Comput Intell Neurosci 2016:2094601. [sample size]\n"
"  How many is enough? Effect of sample size in inter-subject correlation\n"
"  analysis of fMRI. https://doi.org/10.1155/2016/2094601\n"
"* Simony et al. (2016), Nature Communications 7:12141. [ISFC background]\n"
"  Dynamic reconfiguration of the default mode network during narrative\n"
"  comprehension. https://doi.org/10.1038/ncomms12141\n"
"* Prichard & Theiler (1994), Phys Rev Lett 73(7):951-954.\n"
"  [phase-randomized surrogates] Generating surrogate data for time series\n"
"  with several simultaneously measured variables.\n"
"  https://doi.org/10.1103/PhysRevLett.73.951\n"
"* Winkler et al. (2014), NeuroImage 92:381-397.\n"
"  Permutation inference for the general linear model.\n"
"  [permutation inference and exchangeability]\n"
"  https://doi.org/10.1016/j.neuroimage.2014.01.060\n"
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
     if(!strcasecmp(argv[nopt],"-block_summary")){
       if(opts.block_summary_given) ERROR_exit("3dInSync: -block_summary given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: -block_summary needs median or fisher_mean");
       if(!strcmp(argv[nopt],"median")) opts.block_summary=INSYNC_SUMMARY_MEDIAN;
       else if(!strcmp(argv[nopt],"fisher_mean")) opts.block_summary=INSYNC_SUMMARY_FISHER_MEAN;
       else ERROR_exit("3dInSync: -block_summary must be median or fisher_mean");
       opts.block_summary_given=1; nopt++; continue;
     }
     if(!strcasecmp(argv[nopt],"-block_missing")){
       if(opts.block_missing_given) ERROR_exit("3dInSync: -block_missing given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: -block_missing needs require_all or drop_common");
       if(!strcmp(argv[nopt],"drop_common")) opts.block_drop_common=1;
       else if(strcmp(argv[nopt],"require_all")) ERROR_exit("3dInSync: -block_missing must be require_all or drop_common");
       opts.block_missing_given=1; nopt++; continue;
     }
     if(!strcasecmp(argv[nopt],"-min_blocks")){
       if(opts.min_blocks_given) ERROR_exit("3dInSync: -min_blocks given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: -min_blocks needs an integer");
       opts.min_blocks=INSYNC_selection_int(argv[nopt++],1,"-min_blocks"); opts.min_blocks_given=1; continue;
     }
     if(!strcasecmp(argv[nopt],"-min_block_frac")){
       char *end; double f;
       if(opts.min_block_frac_given) ERROR_exit("3dInSync: -min_block_frac given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: -min_block_frac needs a fraction");
       errno=0; f=strtod(argv[nopt],&end);
       if(errno || end==argv[nopt] || *end || !isfinite(f) || f<0 || f>1) ERROR_exit("3dInSync: -min_block_frac must be in [0,1]");
       opts.min_block_frac=f; opts.min_block_frac_given=1; nopt++; continue;
     }
     if(!strcasecmp(argv[nopt],"-block_weight")){
       if(opts.block_weight_given) ERROR_exit("3dInSync: -block_weight given twice");
       if(++nopt>=argc || strcmp(argv[nopt],"equal")) ERROR_exit("3dInSync: only -block_weight equal is implemented; coverage weighting is deferred to M4a");
       opts.block_weight_given=1; nopt++; continue;
     }
     if(!strcasecmp(argv[nopt],"-min_valid")){
       if(opts.min_valid_given) ERROR_exit("3dInSync: -min_valid given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: -min_valid needs an integer");
       opts.min_valid=INSYNC_selection_int(argv[nopt++],3,"-min_valid"); opts.min_valid_given=1; continue;
     }
     if(!strcasecmp(argv[nopt],"-min_valid_frac")){
       char *end; double f;
       if(opts.min_valid_frac_given) ERROR_exit("3dInSync: -min_valid_frac given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: -min_valid_frac needs a fraction");
       errno=0; f=strtod(argv[nopt],&end);
       if(errno || end==argv[nopt] || *end || !isfinite(f) || f<0 || f>1) ERROR_exit("3dInSync: -min_valid_frac must be in [0,1]");
       opts.min_valid_frac=f; opts.min_valid_frac_given=1; nopt++; continue;
     }
     if(!strcasecmp(argv[nopt],"-combine_blocks")){
       if(opts.combine_given) ERROR_exit("3dInSync: -combine_blocks given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: -combine_blocks needs block_summary or pool");
       if(!strcmp(argv[nopt],"pool")) opts.combine_pool=1;
       else if(strcmp(argv[nopt],"block_summary")) ERROR_exit("3dInSync: -combine_blocks must be block_summary or pool");
       opts.combine_given=1; nopt++; continue;
     }
     if(!strcasecmp(argv[nopt],"-segment_center")){
       if(opts.segment_center_given) ERROR_exit("3dInSync: -segment_center given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: -segment_center needs none or mean");
       if(!strcmp(argv[nopt],"mean")) opts.segment_center=1;
       else if(strcmp(argv[nopt],"none")) ERROR_exit("3dInSync: -segment_center must be none or mean");
       opts.segment_center_given=1; nopt++; continue;
     }
     if( !strcasecmp(argv[nopt],"-xmat_stims") || !strcasecmp(argv[nopt],"-xmat_runs") ){
       char **dst=!strcasecmp(argv[nopt],"-xmat_stims")?&opts.xmat_stims:&opts.xmat_runs;
       char *name=argv[nopt];
       if(*dst) ERROR_exit("3dInSync: %s given twice",name);
       if(++nopt>=argc) ERROR_exit("3dInSync: need a comma list after %s",name);
       *dst=argv[nopt++]; continue;
     }
     if( !strcasecmp(argv[nopt],"-xmat_blocks") ){
       if(opts.xmat_blocks) ERROR_exit("3dInSync: -xmat_blocks given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: need each or by_stim after -xmat_blocks");
       if(!strcmp(argv[nopt],"each")) opts.xmat_blocks=1;
       else if(!strcmp(argv[nopt],"by_stim")) opts.xmat_blocks=2;
       else ERROR_exit("3dInSync: -xmat_blocks must be each or by_stim");
       nopt++; continue;
     }
     if( !strcasecmp(argv[nopt],"-xmat_across_runs") ){
       if(opts.xmat_across_runs) ERROR_exit("3dInSync: -xmat_across_runs given twice");
       opts.xmat_across_runs=1; nopt++; continue;
     }
     if( !strcasecmp(argv[nopt],"-xmat_threshold") ){
       char *end; double value;
       if(opts.xmat_threshold_given) ERROR_exit("3dInSync: -xmat_threshold given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: need a fraction after -xmat_threshold");
       errno=0; value=strtod(argv[nopt],&end);
       if(errno || end==argv[nopt] || *end || !isfinite(value) || value<=0 || value>1)
         ERROR_exit("3dInSync: -xmat_threshold must satisfy 0 < F <= 1");
       opts.xmat_threshold=value; opts.xmat_threshold_given=1; nopt++; continue;
     }
     if(!strcasecmp(argv[nopt],"-whole_summary")){
       if(opts.whole_summary) ERROR_exit("3dInSync: -whole_summary given twice");
       opts.whole_summary=1; nopt++; continue;
     }
     if( !strcasecmp(argv[nopt],"-check_table") ){
       if( opts.check_table ) ERROR_exit("3dInSync: -check_table given twice") ;
       opts.check_table=1 ; nopt++ ; continue ;
     }
     if( !strcasecmp(argv[nopt],"-window") || !strcasecmp(argv[nopt],"-step") ){
       int win=!strcasecmp(argv[nopt],"-window"); char *name=argv[nopt];
       if( (win && opts.window) || (!win && opts.step_given) )
         ERROR_exit("3dInSync: %s given twice",name);
       if(++nopt>=argc) ERROR_exit("3dInSync: need integer TRs after %s",name);
       if(win) opts.window=INSYNC_selection_int(argv[nopt++],3,name);
       else { opts.step=INSYNC_selection_int(argv[nopt++],1,name); opts.step_given=1; }
       continue;
     }
     if( !strcasecmp(argv[nopt],"-run_starts") ){
       if(opts.run_starts) ERROR_exit("3dInSync: -run_starts given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: need a comma list after -run_starts");
       opts.run_starts=argv[nopt++]; continue;
     }
     if( !strcasecmp(argv[nopt],"-tr") ){
       char *end; double value;
       if(opts.tr_given) ERROR_exit("3dInSync: -tr given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: need seconds after -tr");
       errno=0; value=strtod(argv[nopt],&end);
       if(errno || end==argv[nopt] || *end || !isfinite(value) || value<=0)
         ERROR_exit("3dInSync: -tr must be positive finite seconds");
       opts.selection_tr=value; opts.tr_given=1; nopt++; continue;
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
     if(!strcasecmp(argv[nopt],"-censor_column")){
       if(opts.censor_column_given) ERROR_exit("3dInSync: -censor_column given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: -censor_column needs a name");
       opts.censor_column=argv[nopt++]; opts.censor_column_given=1; continue;
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
     if(!strcasecmp(argv[nopt],"-isc_model") || !strcasecmp(argv[nopt],"-isc_qVars") || !strcasecmp(argv[nopt],"-isc_qVarCenters")){
       char **dest=!strcasecmp(argv[nopt],"-isc_model")?&opts.isc_model:!strcasecmp(argv[nopt],"-isc_qVars")?&opts.isc_qvars:&opts.isc_centers;
       if(*dest) ERROR_exit("3dInSync: %s given twice",argv[nopt]);
       if(nopt+1>=argc || !*argv[nopt+1]) ERROR_exit("3dInSync: %s needs a nonempty argument",argv[nopt]);
       *dest=argv[nopt+1]; nopt+=2; continue;
     }
     if(!strcasecmp(argv[nopt],"-isc_jobs")){
       if(opts.isc_jobs_given) ERROR_exit("3dInSync: -isc_jobs given twice");
       if(++nopt>=argc) ERROR_exit("3dInSync: -isc_jobs needs an integer");
       opts.isc_jobs=INSYNC_selection_int(argv[nopt++],1,"-isc_jobs"); opts.isc_jobs_given=1; continue;
     }
     if(!strcasecmp(argv[nopt],"-isc_gltCode")){
       int n=opts.isc_nglt;
       if(nopt+2>=argc) ERROR_exit("3dInSync: -isc_gltCode needs NAME and quoted WEIGHTS");
       if(n>=1000) ERROR_exit("3dInSync: too many GLTs");
       opts.isc_glt_names=realloc(opts.isc_glt_names,(n+1)*sizeof(char *));
       opts.isc_glt_weights=realloc(opts.isc_glt_weights,(n+1)*sizeof(char *));
       if(!opts.isc_glt_names || !opts.isc_glt_weights) ERROR_exit("3dInSync: cannot allocate GLTs");
       opts.isc_glt_names[n]=argv[nopt+1]; opts.isc_glt_weights[n]=argv[nopt+2]; opts.isc_nglt++; nopt+=3; continue;
     }
     if( strcasecmp(argv[nopt],"-save_pairwise")==0 ){
       if( ++nopt>=argc ) ERROR_exit(PROGRAM_NAME ": need a prefix after -save_pairwise") ;
       if(opts.pair_prefix) ERROR_exit("3dInSync: -save_pairwise given twice");
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
     if( strcasecmp(argv[nopt],"-temporal_censor")==0 ){
       if(opts.temporal_censor) ERROR_exit("3dInSync: -temporal_censor given twice");
       if(++nopt>=argc || strcmp(argv[nopt],"joint_shift")) ERROR_exit("3dInSync: -temporal_censor requires joint_shift; fixed-mask and censored phase nulls are not supported");
       opts.temporal_censor=1; nopt++; continue;
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
   if(opts.isc_model || opts.isc_qvars || opts.isc_centers || opts.isc_nglt || opts.isc_jobs_given){
      if(!opts.pair_prefix || !INSYNC_selection_requested(&opts,tab)) ERROR_exit("3dInSync: -isc_* options require a selection request and -save_pairwise PREFIX");
   }
   return INSYNC_run(&opts,tab,argc,argv) ;
}
