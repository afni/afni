#include "mrilib.h"
#include "thd_datatable.h"
#include "thd_mapinfer.h"
#include "thd_srm.h"
#include "thd_crossval.h"
#include "thd_xmat.h"
#include "thd_permute.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#define PROGRAM_NAME "3dSRM"

/*----------------------------------------------------------------------------
  How to read this file
  =====================

  3dSRM is the command-line driver around the dataset-agnostic Shared Response
  Model numerics in thd_srm.c / thd_srm.h (read the header comment of
  thd_srm.h first: it defines the model X_i ~= W_i S + noise, the matrix
  layouts, and the gauge).  Everything in THIS file is about getting data in
  and results out: tables, datasets, censoring, normalization, cross-validation
  bookkeeping, scores, and output files.  No SRM math lives here.

  Program flow (main, at the bottom of the file)
    1. Options are parsed into an srm_opts.  Many combinations are refused
       early with a message saying what to do instead.
    2. The data table (-dataTable / -dataTableFile; thd_datatable.h) is read.
       One row per subject (x run, or x item).  Optional expansions: an Xmat
       column on the item axis becomes one row per stimulus column (-axis item),
       and -lss expands LSS event tables.
    3. Two side paths leave main early:
         -model_in : place NEW subjects into a saved model (srm_run_model_in).
         -apply_W  : apply each subject's saved W to OTHER data of that subject
                     (srm_run_apply_W).
    4. The data stage (functions named srm_data_*), in this order:
         table   -> subjects, runs, groups, masks           (srm_data_table / _items)
         shape   -> open headers: lengths, grids, masks      (srm_data_shape / _items)
         read    -> masked time series onto the input timeline (srm_data_read / _items)
         censor  -> each subject's censored points: interp, drop, or missing
         timeline-> keep the retained time points, pack the columns
         normalize-> z-score each feature within each run, drop constant features
       With -compress (the default when every subject has more voxels than time
       points) the stage runs one subject at a time and each subject's data X
       (voxels x T) is replaced by a small factor R (rank x T) with R'R = X'X
       (srm_data_compress_stream).
    5. The fit (SRM_fit_multistart), canonicalized so components are ordered
       by variance (SRM_canonicalize).
    6. Optional held-out scoring (-cv run | loso), possibly per group
       (-cv_fit, -balance), per condition (-score_by), with permutation tests
       (-nperm) and reliability (-reliability).
    7. Outputs: PREFIX.S.1D, .objective.1D, .rho2.1D, .fit.txt, .scores*.txt,
       the model file PREFIX.srm (SRM_save_model), and per-subject projections,
       W maps, reconstructions and R^2 maps.

  A short glossary for readers new to neuroimaging
    dataset     An AFNI 3D+time file (BRIK/HEAD or NIfTI): a 3D grid of voxels,
                with one volume (also called a sub-brick) per time point or per
                statistic.  THD_open_dataset / DSET_* macros access it.
    voxel       One grid location (a "feature" here).  A subject's matrix X has
                one row per voxel inside the mask, one column per sample.
    mask        A dataset whose nonzero voxels are the ones analyzed.  s->vox[f]
                is the grid index of feature f.
    grid        The geometry (dimensions, voxel size, orientation) of a dataset;
                EQUIV_GRIDS checks two datasets share it.
    TR          Repetition time: seconds between successive volumes.
    run         One continuous scan.  Runs are concatenated in time; z-scoring,
                interpolation and -cv folds respect the run boundaries.
    censoring   Dropping time points with too much head motion.  afni_proc.py
                writes a 0/1 censor file and, in its residual dataset ("errts"),
                sets censored volumes to zero.  Its regression design
                X.xmat.1D (thd_xmat.h) records RunStart (where runs begin in a
                concatenated input) and GoodList (the uncensored time points).
    time vs item axis
                -axis time: columns are time points of one stimulus that every
                subject experienced in sync (a movie).  -axis item: columns are
                trial or condition betas, matched across subjects by label.
    z-scoring   Per feature: subtract the mean and divide by the standard
                deviation, here separately within each run (-zscore run).

  The main structs
    srm_opts   every command-line option (one instance, in main).
    srm_subj   one subject: table rows, mask voxels, data x (nvox x T), the
               censoring masks, and (-compress) the factor R in place of x.
    srm_data   the whole table's data stage: subjects, runs, the input and the
               retained timelines, the item keys.
    srm_ctx    what the cross-validation code needs: subjects, T, K, params,
               the -score_by factors.  Subsets of subjects get their own
               shallow srm_ctx (srm_ctx_subset).
    srm_folds  the held-out sets (runs, time blocks, or item levels); either one
               common plan (thd_crossval.h) or each subject's own folds.
    srm_cvres  the held-out scores [subject][fold][metric], per-level scores,
               held-out projections, voxelwise R^2.
------------------------------------------------------------------------------*/

/*! Threads the fits may use (for the memory estimate). */
static int srm_omp_threads( void )
{
#ifdef USE_OMP
   return omp_get_max_threads() ;
#else
   return 1 ;
#endif
}


static char *srm_options[] = {
   "-prefix", "-nfeatures", "-algorithm", "-niter", "-tol", "-seed", "-nstart",
   "-mask", "-run_column", "-group_column", "-zscore",
   "-censor", "-zcensor", "-zcensor_warn",
   "-axis", "-lss", "-item_column", "-repeated_stimulus", "-time_labels",
   "-model_in", "-update_S", "-run_coupling", "-cv_adapt", "-apply_W", "-score_by", "-cv_fit", "-balance", "-reliability", "-compress", "-no_compress",
   "-nsplit", "-nperm", "-cv", "-cv_blocks", "-cv_metrics", "-match_window", "-loso_iter", "-loso_start",
   "-save_W", "-save_recon", "-save_r2", "-proj_format", "-no_projected",
   "-memory_limit", "-memory_override", "-quiet", "-progress",
   "-dataTable", "-dataTableFile", "-show_table", "-help", "-h", NULL
} ;

static void usage_3dSRM(void)
{
   printf(
"\n"
"Usage: 3dSRM [options]                                         ~1~\n"
"\n"
"Fit a Shared Response Model (SRM; Chen et al., NIPS 2015) to time-locked\n"
"data from several subjects, and measure how well it generalizes.\n"
"\n"
"Each subject i contributes a matrix X_i (features x time). SRM learns one\n"
"shared response S (k x time) and, for each subject, a mapping W_i\n"
"(features x k, orthonormal columns) so that\n"
"\n"
"      X_i  ~=  W_i S  +  noise\n"
"\n"
"The subjects need not have the same number or arrangement of features.\n"
"Subject i's voxels (or EEG channels) are read out into the same k-dimensional\n"
"space, so no voxel-to-voxel correspondence across subjects is required.\n"
"Only the columns must correspond. The columns can be time points of a\n"
"stimulus every subject experienced at the same time (a movie, a story), or\n"
"items (trial or condition betas) matched across subjects by a label, which\n"
"is the right choice when trial order was randomized. See -axis. SRM is\n"
"meaningless for resting-state data.\n"
"\n"
"Typical uses:\n"
"  * Subject-wise k x time series in a common space (the 'projected' output)\n"
"    for 3dRSA, 3dInSync, decoding, or a group t-test of a held-out score.\n"
"  * Denoising: only activity shared across subjects survives the bottleneck.\n"
"  * Placing new subjects into an existing space, now or months later, from a\n"
"    saved model and without the original subjects' data (see -model_in).\n"
"\n"
"Required input:                                             ~1~\n"
"\n"
"  -dataTable @TABLE\n"
"  -dataTableFile TABLE\n"
"  -dataTable Subj Group InputFile s01 control s01_movie+tlrc ...\n"
"\n"
"      TABLE needs Subj and InputFile columns, plus optional columns:\n"
"        Run    : with one row per Subj x Run, each subject's runs are\n"
"                 concatenated in time in the order the runs first appear.\n"
"                 Every subject must have the same set of runs, each of the\n"
"                 same length. See -run_column.\n"
"        Item   : item label, with -axis item. See -item_column.\n"
"        Xmat   : the subject's X.xmat.1D (from afni_proc.py or 3dDeconvolve),\n"
"                 when one InputFile holds all of the subject's runs, such as\n"
"                 afni_proc.py's errts. Its RunStart splits the input into\n"
"                 runs, and its NRowFull must equal the input's length. See\n"
"                 -concat. With -axis item it gives the items instead: see\n"
"                 'Items from afni_proc.py output' below. Either way each\n"
"                 subject's Xmat is summarized in numeric columns that\n"
"                 travel to scores.txt as covariates: Xmat.TR, Xmat.nrun,\n"
"                 Xmat.ntime, Xmat.ncensored, Xmat.frac_censored, Xmat.nstim.\n"
"        Censor : the subject's censor file (1 keep, 0 censored), covering all\n"
"                 of that subject's runs. See -cenmode.\n"
"        Mask   : a mask dataset for that subject, on that subject's own\n"
"                 grid. Subjects may then have different voxel counts and\n"
"                 different grids. A subject's Mask must be the same on all\n"
"                 their rows.\n"
"        Group  : subject group, used to label and compare scores. See\n"
"                 -group_column.\n"
"        others : any other column can be named in -score_by. Numeric\n"
"                 columns are carried to the output tables, so covariates\n"
"                 such as motion travel with the scores.\n"
"\n"
"      InputFile is an AFNI dataset, or a .1D file. A .1D file is read as\n"
"      time x features (one row per time point, one column per channel or\n"
"      region), the usual AFNI orientation. Datasets and .1D files can be\n"
"      mixed in one table, which is how non-voxel data (EEG band power at TR\n"
"      resolution, parcel means) enter the same model. Every input must have\n"
"      the same number of time points.\n"
"\n"
"  -nfeatures K[,K2,...]\n"
"                       Dimension k of the shared space. Required, except with\n"
"                       -model_in, which takes it from the model. K must not\n"
"                       exceed the smallest feature count or the number of\n"
"                       time points. Several values require -cv and produce a\n"
"                       comparison table (PREFIX.nfeat.txt) and no model\n"
"                       outputs; rerun with the chosen K for those.\n"
"\n"
"Model options:                                              ~1~\n"
"\n"
"  -prefix PREFIX       Output prefix [SRM].\n"
"\n"
"  -algorithm ALG       prob : probabilistic SRM, fitted by EM [default].\n"
"                              Estimates a noise variance for every subject,\n"
"                              so noisier subjects (or noisier modalities)\n"
"                              count for less in S.\n"
"                       det  : deterministic SRM, alternating Procrustes\n"
"                              steps. Subjects are weighted by their feature\n"
"                              count and variance only.\n"
"\n"
"  -niter N             Maximum iterations [500].\n"
"  -tol T               Stop when the relative change in the objective falls\n"
"                       below T [1e-7].\n"
"\n"
"                       SRM converges slowly on real data, which is why the\n"
"                       defaults are generous. For 22 subjects hearing a story\n"
"                       (1000 voxels each, 15 components), the shared response\n"
"                       after 10 iterations, BrainIAK's default, differed from\n"
"                       the converged one by 38%%, after 20 by 30%%, after 100 by\n"
"                       15%%, and after 200 by 4%%. Full convergence (1e-13) took\n"
"                       about 550 iterations. -tol 1e-7 stops near 170 and\n"
"                       -tol 1e-8 near 250. A fit stopped earlier is not\n"
"                       wrong, only unconverged, and it depends on the seed.\n"
"                       Whether a converged or an earlier fit predicts held-out\n"
"                       data better is an empirical question: -cv answers it.\n"
"                       For a quick look, -niter 20 is enough.\n"
"\n"
"  -seed S              Seed for the random starting W_i [1234567]. Until a fit\n"
"                       has converged, its subspaces depend on the seed.\n"
"                       Compare fits with rotation-invariant quantities, not\n"
"                       component by component.\n"
"  -nstart N            Fit from N different random starts and keep the one\n"
"                       with the best objective [1]. Starts run one after\n"
"                       another, so the cost is N times as much. The\n"
"                       objective of every start is in PREFIX.fit.txt: if they\n"
"                       agree to the tolerance you ran at, they found the same\n"
"                       optimum. If they differ by more, run longer first.\n"
"                       Only then is a difference evidence of a local optimum.\n"
"                       In tests, both models reached the same optimum from\n"
"                       every start on real data. One hard synthetic problem\n"
"                       (noise differing 6-fold between subjects) stalled the\n"
"                       deterministic model at a slightly worse stationary\n"
"                       point from one start, so N of 3 to 5 is cheap\n"
"                       insurance when the data are that uneven.\n"
"\n"
"  -run_coupling L      shared : one W per subject for all of their runs\n"
"                                [default]\n"
"                       L >= 0 : a separate W for each subject and run, each\n"
"                                tied to a per-subject anchor W. The tie is a\n"
"                                prior worth L time points of data: L=0 fits\n"
"                                every run on its own, L equal to a run's\n"
"                                length weighs the anchor as much as that\n"
"                                run's data, and L much larger than the runs\n"
"                                is nearly 'shared'. Try values from about\n"
"                                0.2 to 2 times a typical run length. Requires\n"
"                                runs, and -axis time. The fit starts from the\n"
"                                shared-W fit (so it takes about twice as long),\n"
"                                and a very\n"
"                                large L stays close to it: use 'shared'\n"
"                                instead.\n"
"\n"
"                                Runs not in the fit (held-out runs, new data)\n"
"                                use the anchor. See -cv_adapt. In-sample, a\n"
"                                smaller L always fits better, because a W per\n"
"                                run has more freedom; whether it generalizes\n"
"                                is what -cv shows. With L=0 the runs can fall\n"
"                                into different orientations of S.\n"
"\n"
"Data options:                                               ~1~\n"
"\n"
"  -mask MASK           A mask for every subject lacking a Mask column. All\n"
"                       such subjects then must share MASK's grid. Datasets\n"
"                       need a mask from one source or the other; .1D inputs\n"
"                       never do. Voxels with zero variance in any run (over\n"
"                       all samples, with -zscore all) are dropped for that\n"
"                       subject and counted in the log.\n"
"\n"
"  -run_column NAME     Run column in the table [Run]. If absent, each subject\n"
"                       is a single run, unless -concat or an Xmat column\n"
"                       says where runs start inside each input.\n"
"  -concat SPEC         Run starts within each subject's one input, as in\n"
"                       3dDeconvolve and 3dTproject: the time-point index at\n"
"                       which each run begins, starting with 0, as a 1D file or\n"
"                       '1D: 0 300 600'. The same for every subject. For one\n"
"                       input per subject that holds several runs (afni_proc.py's\n"
"                       errts.SUBJ+tlrc, say); without it such an input is one\n"
"                       long run, so z-scoring and -cv would ignore the\n"
"                       boundaries. An Xmat column does the same per subject,\n"
"                       from RunStart (every subject must still have the same\n"
"                       runs). Not with a Run column.\n"
"  -group_column NAME   Group column in the table [Group]. Only used to label\n"
"                       the output tables.\n"
"\n"
"  -zscore MODE         run  : z-score every feature separately within each run\n"
"                              before fitting [default with -axis time]\n"
"                       all  : z-score every feature over all of the subject's\n"
"                              samples together [default with -axis item:\n"
"                              betas from one GLM share a baseline]\n"
"                       none : use the data as given (it should already be\n"
"                              centered and scaled)\n"
"\n"
"  -censor FILE         One common 0/1 vector with one value per input time\n"
"                       point. Retained time points are identical for all\n"
"                       subjects. Applied before z-scoring. -axis time only.\n"
"  -zcensor             Treat a subject's volumes that are all zero in the\n"
"                       mask as censored for that subject (afni_proc.py's errts\n"
"                       zeroes its censored volumes). -axis time only (an\n"
"                       all-zero item is always an error).\n"
"\n"
"  Each subject's own censoring comes from any of: a Censor column (one 0/1\n"
"  file per subject, 1 = keep, covering all of the subject's runs in order,\n"
"  such as afni_proc.py's censor_SUBJ_combined_2.1D), the GoodList of an\n"
"  Xmat column, and -zcensor. What a censored time point becomes:\n"
"\n"
"  -cenmode MODE        interp  : filled in, for that subject only, by linear\n"
"                                 interpolation between the nearest kept time\n"
"                                 points of the same run (the nearest one at a\n"
"                                 run's edge), as 3dTproject -cenmode NTRP.\n"
"                                 Everyone keeps every time point [default:\n"
"                                 every subject's projected series stays\n"
"                                 complete, ready for 3dInSync or 3dRSA].\n"
"                       drop    : the time point is dropped for EVERYONE. Each\n"
"                                 subject's censoring is then lost by all: 30\n"
"                                 subjects with 5%% censored at random keep only\n"
"                                 about a fifth of the time points.\n"
"                       missing : that subject is left out at that time point\n"
"                                 only. The fit uses each subject where it was\n"
"                                 observed (deterministic: S at each time point\n"
"                                 is the mean of the subjects observed there;\n"
"                                 probabilistic: the posterior uses their\n"
"                                 precision only, and each subject's noise\n"
"                                 variance its own observed samples). Z-scoring\n"
"                                 is over observed samples. Scores use the\n"
"                                 samples where the subject, and someone else\n"
"                                 for the reference, were observed; segment\n"
"                                 matching skips windows that hold a missing\n"
"                                 sample. The more exact model, but PROJECTED\n"
"                                 series are 0 at a subject's missing time\n"
"                                 points. Each run needs at least\n"
"                                 3 observed time points per subject (and, with\n"
"                                 -run_coupling 0, k). With -run_coupling, each\n"
"                                 run's W uses its observed samples (the anchor\n"
"                                 fills in the rest). A saved model keeps how\n"
"                                 many subjects were observed at each time point,\n"
"                                 so -update_S adds new subjects (with or without\n"
"                                 missing points) correctly. -apply_W z-scores\n"
"                                 (or centers) the new data over observed samples\n"
"                                 and writes 0 at missing ones.\n"
"                       -censor (common to everyone) always drops. With\n"
"                       -model_in the model's timeline is fixed, so drop is an\n"
"                       error there.\n"
"  -zcensor_warn PCT    Warn for each subject with more than PCT percent of\n"
"                       its time points censored [20].\n"
"\n"
"  3dSRM does not regress nuisance signals, filter, or change temporal\n"
"  alignment. Inputs should already be preprocessed and aligned to the\n"
"  stimulus. For EEG-derived inputs, the HRF and any lag must be handled\n"
"  before the data reach this program.\n"
"\n"
"Time-locked or item-indexed:                                ~1~\n"
"\n"
"  -axis time           Columns are the time points of a stimulus presented\n"
"                       identically to everyone [default]. With one row per\n"
"                       Subj x Run, runs are concatenated.\n"
"  -axis item           Columns are items. The table has one row per Subj x\n"
"                       Item, whose InputFile is a single volume (or a\n"
"                       sub-brick selector such as betas+tlrc'[7]' or\n"
"                       betas+tlrc'[face#0_Coef]'), or a .1D file with one\n"
"                       row. Every subject must have every item exactly once,\n"
"                       in any row order; items are matched by label, not\n"
"                       position. Missing, repeated, or extra items are errors\n"
"                       that name the Subj and Item. A dataset that holds many\n"
"                       items is read once per subject, however many rows\n"
"                       select from it. An item that is zero everywhere in the\n"
"                       mask (usually a failed or missing estimate) is an error.\n"
"\n"
"  -item_column NAME    Item-label column [Item]. Labels may not contain ':'.\n"
"\n"
"  Items from afni_proc.py output. A table with one row per subject, an Xmat\n"
"  column (the X.xmat.1D of the regression) and InputFile the stats bucket\n"
"  (stats.SUBJ+tlrc, or the REML one), and no Item column, is expanded to one\n"
"  item per stimulus column of the Xmat: a condition with one column is one\n"
"  item named by its label (house), and one with several columns gives an\n"
"  item per column, named as 3dDeconvolve names it (face#0, face#1, ...: the\n"
"  trials of -stim_times_IM, or the lags of a TENT basis). Each reads the\n"
"  bucket's LABEL#n_Coef volume. In a table file '#' starts a comment only at\n"
"  the start of a word, so face#0 and stats+tlrc[face#0_Coef] can be written\n"
"  as they are, or quoted as a whole (\"face#0\"). The new rows carry\n"
"  the subject's other columns (Group, Mask, covariates), a Condition column\n"
"  (the stimulus label, for -fold_column Condition: leave one condition out),\n"
"  and, when every item's regressor lies inside one run, a Run column, so -cv\n"
"  run works on trials. -show_table prints the expanded table. The Xmat knows\n"
"  each trial's condition and order, not which stimulus it was: trial items\n"
"  match across subjects only if everyone had the same order; otherwise list\n"
"  the items in the table yourself.\n"
"  LSS trial betas and explicit stimulus identities:\n"
"  -lss                With -axis item, InputFile is an existing 3dLSS output\n"
"                      dataset, Xmat is the design used by 3dLSS, and Events\n"
"                      names an event-to-item table. This reads LSS betas; it\n"
"                      does not run 3dLSS or fit a GLM to raw BOLD data.\n"
"\n"
"  The subject table has Subj Xmat Events InputFile, plus optional subject\n"
"  metadata (Group, Mask, covariates). Each Events file has a header with:\n"
"    Trial Item [Condition Repeat Run other_metadata ...]\n"
"  Trial is the ZERO-BASED LSS volume index, not a TR, onset, original Xmat\n"
"  column, or one-based trial number. Map every volume exactly once. Rows and\n"
"  columns may be reordered; all Events files must have the same columns.\n"
"  Item is the actual shared stimulus identity, not its presentation order.\n"
"  -item_column and -repeat_column select alternative metadata column names.\n"
"  Repeated items need matching repeats across subjects, as described below.\n"
"  Condition, if wanted, comes from Events: one IM regressor family can hold\n"
"  several experimental categories. Additional columns can define folds or\n"
"  score groups. Subject and event column names must not overlap. All paths\n"
"  are relative to the working directory, not the table's directory.\n"
"\n"
"  The Xmat must describe exactly one -stim_times_IM family, as for 3dLSS.\n"
"  Its IM column count and the dataset's ordered LABEL#000, LABEL#001, ...\n"
"  labels must agree. Use the original, unselected LSS dataset. Renamed or\n"
"  selected datasets can instead use the explicit item-table method below.\n"
"  -xmat_stims is not used with -lss. Xmat summary covariates are retained.\n"
"  If neither table supplies Run (or -run_column's name), it is inferred as\n"
"  1, 2, ... only when every IM regressor lies wholly in one run, accounting\n"
"  for GoodList censoring. Otherwise supply Run explicitly for run CV.\n"
"  For separate per-run LSS datasets, supply the actual Run on each subject\n"
"  row (or in Events); each separate Xmat otherwise starts its runs at 1.\n"
"  Use -zscore run for separate run fits. No censor vector is applied to\n"
"  beta volumes. Trial mappings must be complete; use an explicit item table\n"
"  to analyze a common subset, keeping the same item/repeat keys for everyone.\n"
"\n"
"  New way (s01.events.txt and s02.events.txt can have different trial orders):\n"
"    # lss_subjects.txt\n"
"    Subj Xmat          Events          InputFile\n"
"    s01  s01.X.xmat.1D  s01.events.txt  s01.LSS+tlrc\n"
"    s02  s02.X.xmat.1D  s02.events.txt  s02.LSS+tlrc\n"
"\n"
"    # s01.events.txt\n"
"    Trial Item      Condition\n"
"    0     face_017  face\n"
"    1     house_042 house\n"
"\n"
"    # s02.events.txt\n"
"    Trial Item      Condition\n"
"    0     house_042 house\n"
"    1     face_017  face\n"
"    # ...one row per remaining trial in EACH file...\n"
"\n"
"    3dSRM -axis item -lss -dataTableFile lss_subjects.txt -show_table\n"
"    3dSRM -prefix task -axis item -lss -nfeatures 5 -cv run \\\n"
"          -mask roi_mask+tlrc -dataTableFile lss_subjects.txt\n"
"\n"
"  Old way (still supported; no -lss, Xmat, or Events columns):\n"
"    # explicit_items.txt\n"
"    Subj Item      Condition Run InputFile\n"
"    s01  face_017  face      1   s01.LSS+tlrc[0]\n"
"    s01  house_042 house     1   s01.LSS+tlrc[1]\n"
"    s02  house_042 house     1   s02.LSS+tlrc[0]\n"
"    s02  face_017  face      1   s02.LSS+tlrc[1]\n"
"    # ...one row per remaining item/repeat for every subject...\n"
"\n"
"    3dSRM -prefix task -axis item -nfeatures 5 -cv run \\\n"
"          -mask roi_mask+tlrc -dataTableFile explicit_items.txt\n"
"\n"
"  These commands assume a full table with multiple runs and enough training\n"
"  items/voxels for 5 features; the two displayed trials only illustrate the\n"
"  mapping. Use held-out runs/items to evaluate alignment. LSS estimates may\n"
"  mix neighboring trial responses, so run-wise validation is a useful start.\n"
"  Identical Item labels must mean the same stimulus across people; neither\n"
"  the Xmat nor matching LSS volume numbers establishes stimulus identity.\n"
"\n"
"  -xmat_stims LIST     Comma list of the stimulus labels to use [all].\n"
"  -repeat_column NAME  Repeat-label column [Repeat], used if present: it names\n"
"                       an item's repeats (1, 2, ... or early, late).\n"
"\n"
"  Normalization. Betas from one GLM over all runs (the usual afni_proc.py\n"
"  analysis, including trial-wise betas from -stim_times_IM) share one\n"
"  baseline, so the default, -zscore all, z-scores each feature over all of\n"
"  the subject's items together. With betas from a separate GLM per run, use\n"
"  -zscore run, which works over the items of each of the subject's own runs\n"
"  (and needs a Run column). Each run then needs at least 3 items, and\n"
"  z-scores over fewer than about 10 are noisy (a warning says so).\n"
"\n"
"  Runs. A Run column orders repeats and can define the -cv folds. Subjects\n"
"  need not share runs: when trial order was randomized, an item may be in run\n"
"  2 for one subject and run 3 for another (the folds then come from\n"
"  -fold_column).\n"
"\n"
"  Repeats. An item measured more than once (separate betas: trial-wise, or\n"
"  one per run) gives a column per repeat, keyed Item:1, Item:2, ... in run\n"
"  order (runs ordered as they first appear in the table, as with -axis time)\n"
"  and then table order, so two repeats in the same run are numbered by their\n"
"  rows. A Repeat column names them instead (Item:R; see -repeat_column),\n"
"  which does not depend on row order. The model is free to use the repeats,\n"
"  and they are what within-subject reliability is computed from (see\n"
"  -reliability within). Every subject must have the same repeats of each\n"
"  item.\n"
"\n"
"  What changes with -axis item: runs are not concatenated in time, there is\n"
"  no temporal adjacency, so 'match' becomes item identification (the held-out\n"
"  item must be the best correlate of its own counterpart among all items;\n"
"  the correlation is across the k components, so it is weak when k is a\n"
"  handful: compare k values with -nfeatures a,b,c),\n"
"  and the folds of -cv are runs, or the levels of -fold_column. When those\n"
"  differ between subjects for an item (randomized run order), each subject's\n"
"  folds are its own: -cv loso fits the group without the subject on every\n"
"  item; -cv run refits everyone on every item with each subject's own\n"
"  held-out items missing for that subject only (see -cenmode missing).\n"
"  Options about time are errors with -axis item, each saying what to do\n"
"  instead: -censor, -zcensor, -cv_blocks, -match_window, -time_labels,\n"
"  -repeated_stimulus, and -run_coupling. The\n"
"  outputs list the item keys: in the header of PREFIX.S.1D (one row per item)\n"
"  and as the volume labels of PREFIX.proj.SUBJ.\n"
"\n"
"  -fold_column NAME    The table column whose levels are the folds of -cv run\n"
"                       [Run]. Naming a category column gives\n"
"                       leave-one-category-out: the model must then generalize\n"
"                       to a kind of item it never saw, a stricter test.\n"
"\n"
"  -time_labels NAME FILE\n"
"                       With -axis time, one numeric label per retained time\n"
"                       point (0 = unlabelled), to score by stimulus content\n"
"                       such as speech/no speech or scene type. NAME is the\n"
"                       factor name for -score_by. May be given repeatedly.\n"
"                       One label per input time point also works (then\n"
"                       censored ones are skipped).\n"
"\n"
"  Condition from the Xmat. With an Xmat column on the time axis, -score_by\n"
"  Condition labels each time point with the stimulus whose regressor, scaled\n"
"  to its own peak, is largest there, if it reaches -xmat_label_frac of that\n"
"  peak [0.5]; other time points, and censored ones, are unlabelled. The\n"
"  regressors are already convolved, so the labels follow the BOLD response.\n"
"  -xmat_stims chooses the stimuli here too.\n"
"  -xmat_label_frac F   See above [0.5].\n"
"\n"
"  Which one? If every subject saw the same stimuli in the same order, time\n"
"  mode uses the full time series. If trial order differed, only item mode is\n"
"  valid. Time mode cannot align subjects whose events are not simultaneous.\n"
"\n""Mapping new subjects into a saved model:                   ~1~\n"
"\n"
"  Every fit writes PREFIX.srm. It holds the shared response S, the noise\n"
"  variances, and exactly how the data were put on the model's timeline (the\n"
"  censored time points, the run lengths, and the z-scoring). It does not hold\n"
"  any subject's W_i or data, so it is small and can be passed between sites.\n"
"  It is plain text (about 1.2 MB for 30 components and 1310 time points),\n"
"  written to a temporary name and renamed, so an interrupted run never leaves\n"
"  half a model. The numbers are stored with enough digits to be read back\n"
"  exactly, and a model that is read and then updated gives exactly the result\n"
"  that updating the original would have. The format is described in thd_srm.h;\n"
"  keys and sections this version does not know are skipped, and a file from a\n"
"  newer version is refused with a message, not misread. A damaged or\n"
"  truncated file is reported as such.\n"
"\n"
"  -model_in MODEL.srm  Do not fit S. Instead estimate a W for every subject in\n"
"                       the table, against the S in MODEL.srm. Each subject\n"
"                       needs the same stimulus and the same number of time\n"
"                       points and runs the model was built from; this is\n"
"                       checked first, and the data are censored and z-scored\n"
"                       as the model's were. Their feature counts, masks, and\n"
"                       grids can be anything with at least k features.\n"
"\n"
"                       The model defines -nfeatures, -algorithm, -seed,\n"
"                       -run_coupling, -censor, -zcensor, and -zscore; giving\n"
"                       any of them with -model_in is an error, and so is\n"
"                       -niter or -tol unless -update_S is given (they then\n"
"                       control that update). -cv loso is also an error,\n"
"                       since there is no group fit to leave anyone out of.\n"
"                       -cv none or -cv run are allowed, and mean something\n"
"                       different here: see below.\n"
"\n"
"                       A subject who is already in the model is a warning\n"
"                       (their scores are optimistic: S contains them), and an\n"
"                       error with -update_S. A model made with -run_coupling\n"
"                       places new subjects with one W for all their runs,\n"
"                       unless -update_S, which gives them a W per run.\n"
"\n"
"                       A model fit with -axis item needs -axis item here too\n"
"                       (and a time-axis model needs it absent). Each new\n"
"                       subject must have exactly the model's item keys, in\n"
"                       any table order (Item, or Item:n for repeats, as the\n"
"                       fit made them; a mismatch names the first missing or\n"
"                       extra key). -cv run then holds out the levels of\n"
"                       -fold_column (or Run) and scores item identification.\n"
"                       Those levels may differ between subjects (randomized\n"
"                       runs): S is fixed, so each subject's own held-out\n"
"                       items are simply left out of their own W.\n"
"\n"
"  A new subject's W is fitted to their own data, so its fit to that data is\n"
"  flattering by construction. Use -cv run with -model_in: W is then estimated\n"
"  from all of the subject's runs but one (or blocks but one), and the subject\n"
"  is scored on the part left out. The scores are\n"
"  analogous to -cv loso on the original subjects, but use the full reference\n"
"  group rather than a reference group with one subject removed.\n"
"\n"
"  -update_S            ** EXPERIMENTAL **\n"
"                       Let S absorb the new subjects, and write a new\n"
"                       PREFIX.srm that includes them. Without this option the\n"
"                       saved model is untouched and S does not move.\n"
"                       It is an approximation to a refit (see below), and its\n"
"                       behavior and the model file's sums may change.\n"
"\n"
"                       The model keeps running sums of its subjects' projected\n"
"                       data (weighted by their noise variances), so S can be\n"
"                       updated from the new subjects alone, with no access to\n"
"                       the original data. This is the point of the model file:\n"
"                       a site can add its subjects to a shared model without\n"
"                       receiving or sending any subject's data.\n"
"\n"
"                       What it is, and is not. The old subjects' W stay as\n"
"                       they were fitted; only S and the new subjects' W move.\n"
"                       That is an approximation to refitting everyone. The\n"
"                       subjects added together in one run are treated jointly,\n"
"                       so their order within the table cannot matter; subjects\n"
"                       added in different runs can matter, slightly (S differed\n"
"                       by about 3%% between arrival orders in tests).\n"
"\n"
"                       What it costs, measured on 22 subjects hearing a story\n"
"                       (30 components, fit on two runs, scored on a third by\n"
"                       the correlation of each subject's projected data with the\n"
"                       shared response from everyone else). Adding 1, 3, and 6\n"
"                       subjects to 21, 19, and 16 gave a drift of 0.003, 0.015,\n"
"                       and 0.038. Against refitting everyone from scratch, the\n"
"                       EXISTING subjects' scores were lower by 1.6%%, 1.2%%, and\n"
"                       5.0%% (their W had not been refitted); the added subjects'\n"
"                       scores were no lower. Updating S brought it 16-25%% closer\n"
"                       to the refit S than leaving it frozen, but did not raise\n"
"                       held-out scores above those of simply mapping the new\n"
"                       subjects into the frozen S. So the update keeps the\n"
"                       model's S current and the model growing; it is not a\n"
"                       substitute for a refit. The loss was 1-2%% up to adding\n"
"                       about a sixth of the original subjects and 5%% at a third,\n"
"                       so refit from the data when drift reaches a few percent.\n"
"                       These are one data set and one split: differences of\n"
"                       0.001 score of 0.13 are within noise.\n"
"\n"
"                       The gauge of S is never changed by an update, so the\n"
"                       components and the projected series of earlier\n"
"                       analyses stay valid. Scores for the new subjects are\n"
"                       always computed BEFORE the update, from the model they\n"
"                       were not part of (-cv with -model_in).\n"
"\n"
"                       The model file contains sums over subjects of\n"
"                       projected data. With only a few subjects those sums are\n"
"                       not anonymous, so treat a model built from fewer than\n"
"                       about five as if it contained their data.\n"
"\n"
"                       An updated model is written to PREFIX.srm; it will not\n"
"                       overwrite the -model_in file unless it is the same name\n"
"                       and -overwrite is given.\n"
"\n"
"  Without -update_S, adding subjects never changes old results.\n"
"\n"
"Applying a subject's W to other data from the same subject: ~1~\n"
"\n"
"  The cleanest way to analyze a task is to align on data that are independent\n"
"  of what you test: fit on a movie (or on some runs), then carry each subject\n"
"  over to the task data with that subject's own W. Nothing about the task\n"
"  labels then touches the alignment.\n"
"\n"
"  -apply_W PREFIX      Use the W datasets from an earlier -save_W run (the\n"
"                       PREFIX.W.SUBJ datasets and PREFIX.srm). For each table\n"
"                       subject, find their W, take the new InputFile data on\n"
"                       W's own grid and voxels (no mask needed or accepted),\n"
"                       z-score as the model did, and write the data in the\n"
"                       shared space (PREFIX.proj.SUBJ). The new data may have\n"
"                       any length, order, and -axis; nothing is fitted. A\n"
"                       subject in the table but not in the model is an error.\n"
"                       Subjects may differ in length from one another. A\n"
"                       feature with no variance in a run of the new data is\n"
"                       zeroed. With -zscore none in the earlier run, each\n"
"                       feature is centered on the new data, since the means\n"
"                       of the old data are not saved. -mask, -cv and -save_W\n"
"                       are errors here.\n"
"\n"
"                       With -axis item, each subject is read from their own\n"
"                       rows, so subjects may have different items (a dropped\n"
"                       trial, randomized stimuli); each needs at least 3. The\n"
"                       projection's volumes (or the 1D header) carry the\n"
"                       item keys. If the model z-scored within runs and the\n"
"                       table has no Run column, the items are z-scored over\n"
"                       all of the subject's items instead, and this is said.\n"
"\n"
"                       The projected series of different subjects can then go\n"
"                       to 3dRSA, 3dInSync, or a classifier. Since W is\n"
"                       fitted on other data, an effect found there cannot come\n"
"                       from the alignment.\n"
"\n"
"                       Requires that the earlier run used -save_W.\n"
"\n""Validation:                                                 ~1~\n"
"\n"
"  An SRM fits every subject's data with nfeatures x voxels free parameters.\n"
"  Anything measured on the data it was fit to is optimistic. These options\n"
"  evaluate on data the fit never saw.\n"
"\n"
"  -cv MODE             none : fit once to everything. Reported scores are\n"
"                              in-sample and are labelled as such [default].\n"
"                       run  : leave one run out. For each run, fit on the\n"
"                              other runs, then score every subject on the\n"
"                              held-out run (or -cv_blocks block). With\n"
"                              -model_in, S is fixed, and only each subject's\n"
"                              own W is refitted without the held-out run.\n"
"                       loso : leave one subject out. For each subject, fit S\n"
"                              on all OTHER subjects and every time point.\n"
"                              Then estimate that subject's W from their own\n"
"                              training runs and score them on a held-out run.\n"
"                              With cold starts, their held-out data touch\n"
"                              neither S nor any fitted W used for scoring, so\n"
"                              the score is a clean estimate of how well a new\n"
"                              person maps into an existing space. Folds\n"
"                              within the subject are leave-one-run-out, or\n"
"                              -cv_blocks contiguous blocks if there is one run.\n"
"                              Uses independent cold starts by default; costs\n"
"                              one full group fit per subject (-niter, -tol,\n"
"                              -nstart). Written projections are aligned to the\n"
"                              pooled fit AFTER scoring; this output alignment\n"
"                              itself depends on all subjects.\n"
"\n"
"  -cv_blocks B         Contiguous time blocks used as folds in place of runs\n"
"                       when the data have a single run [2]. Time points near a\n"
"                       block edge are autocorrelated with the other side, so\n"
"                       scores are a little optimistic; separate runs are\n"
"                       better.\n"
"  -loso_start MODE     cold [default]: fit each leave-out model independently\n"
"                       on the other subjects, using -niter, -tol and -nstart.\n"
"                       Score in each leave-out model's own canonical basis.\n"
"                       legacy: reproduce the old full-model warm start, with\n"
"                       -loso_iter steps. WARNING: that start already contains\n"
"                       the held-out subject's influence and can inflate scores.\n"
"                       Requires -cv loso in fit mode (time or item axis).\n"
"  -loso_iter M         Iterations for -loso_start legacy only [5]. Ignored for\n"
"                       cold starts. More legacy iterations do not guarantee\n"
"                       independence from the held-out subject.\n"
"\n"
"  -cv_metrics LIST     Comma list from: isc, match, r2 [all three].\n"
"                       isc   : correlation of the subject's projected held-out\n"
"                               data with the shared response estimated from the\n"
"                               other subjects, per component, and their mean\n"
"                               in Fisher-z space (returned as r).\n"
"                       match : time-segment matching accuracy (Chen et al.),\n"
"                               as in BrainIAK's SRM example. Each window of\n"
"                               -match_window TRs of the subject's projected\n"
"                               data is matched to the best-correlated window\n"
"                               of the others' shared response, among its own\n"
"                               counterpart and every window that does not\n"
"                               overlap it. With -axis item it becomes item\n"
"                               identification: each held-out item must be the\n"
"                               best correlate of its own counterpart among\n"
"                               all items. Chance is reported alongside.\n"
"                       r2    : fraction of the subject's held-out variance\n"
"                               reconstructed from W_i and that shared\n"
"                               response. On voxel data this is usually tiny\n"
"                               or slightly negative, because variance that is\n"
"                               specific to a subject dominates (about -0.001\n"
"                               for 5000 voxels of story data, where segment\n"
"                               matching reached 12%% against 0.3%% chance).\n"
"                               Use it to compare methods or settings on the\n"
"                               same data, not as a measure of fit; isc and\n"
"                               match are the informative scores.\n"
"  -match_window N      Segment length in TRs for 'match' [9].\n"
"\n"
"  -cv_adapt FRAC       With -run_coupling L and -cv: the held-out run has no W\n"
"                       of its own, so it uses the anchor. FRAC in (0,1) instead\n"
"                       estimates that run's W from its first FRAC of samples,\n"
"                       tied to the anchor with the same L, and scores only the\n"
"                       rest. The samples used to estimate W are never scored.\n"
"                       isc, match, and r2 are then the run W's scores, and\n"
"                       isc_anchor, match_anchor, and r2_anchor beside them are\n"
"                       the anchor's on the same samples, so the comparison\n"
"                       shows whether a run-specific W helps.\n"
"                       The S of those first samples: with -cv loso, the group\n"
"                       fit's (it saw the run); with -cv run, the mean of every\n"
"                       subject's anchor projection of them, and every subject\n"
"                       gets a run W, so each is scored against the others'\n"
"                       run-W projections (and the anchor scores against the\n"
"                       others' anchors). PREFIX.proj stays the anchor's.\n"
"                       With -cv_fit and -balance both columns are averaged\n"
"                       over the fits that scored the subject, and under\n"
"                       -cv_fit reference the other groups' subjects (placed\n"
"                       in the reference fit's S) adapt as -cv loso does [0:\n"
"                       off].\n"
"\n"
"Reliability, and comparing groups or conditions:            ~1~\n"
"\n"
"  A low score for a group or a condition has three possible causes: its\n"
"  signal is unreliable, its signal is reliable but not shared with the others,\n"
"  or the model was built mostly from someone else. These options separate\n"
"  them.\n"
"\n"
"  -score_by NAMES      Comma list of table columns (or -time_labels names)\n"
"                       whose levels define subsets of the columns. Every\n"
"                       -cv metric is computed separately for each level, for\n"
"                       every subject, so you get faces vs houses within each\n"
"                       subject and then within each group. The model is\n"
"                       still fitted to all samples. Needs -cv.\n"
"                       Where the levels come from:\n"
"                         -axis item: a table column (Category, or Condition\n"
"                           from an Xmat table), read on each subject's rows.\n"
"                         -axis time: a -time_labels NAME; Condition, from\n"
"                           each subject's Xmat (below); or a table column,\n"
"                           its value on each run's row (Run itself, say).\n"
"                       Within a level, isc and r2 use the subject's held-out\n"
"                       samples of that level, and matching is among them only:\n"
"                       windows that lie wholly inside the level, or the\n"
"                       level's items. Chance is reported for each level.\n"
"                       Per-subject columns (Group, covariates) are not\n"
"                       levels: scores.txt carries them already.\n"
"\n"
"  -cv_fit MODE         How the -cv models are fit (the written model, S and the\n"
"                       maps, is always the fit to everyone):\n"
"                       pooled    : one model from everyone [default]\n"
"                       within    : one model per Group, fit only to that\n"
"                                   group, with -cv run or loso inside it.\n"
"                                   Asks: does this group have a shared\n"
"                                   response at all?\n"
"                       reference:LABEL\n"
"                                 : fit on group LABEL only, then place the\n"
"                                   other groups' subjects in that model (with\n"
"                                   their W estimated from their own training\n"
"                                   folds). Asks: do they use the reference\n"
"                                   group's space? Reference subjects are scored\n"
"                                   leave-one-subject-out, the others as new\n"
"                                   subjects. Reference members use N-1 training\n"
"                                   subjects; outside members use N. Both exclude\n"
"                                   the scored subject with cold starts, but the\n"
"                                   training sample sizes still differ.\n"
"                                   Needs -cv loso.\n"
"                       Held-out projections are rotated into the gauge of the\n"
"                       fit to everyone, so all subjects' stay comparable.\n"
"  -balance B           Groups of unequal size bias scores: a model built from\n"
"                       40 people maps its members better than one built from\n"
"                       12. With -balance, every group fit is repeated on B\n"
"                       random subsets the size of the smallest group (with\n"
"                       -cv_fit pooled: B subsets with that many from every\n"
"                       group), and each subject's scores are averaged over the\n"
"                       fits that scored it [off]. A group that is already the\n"
"                       smallest is fitted once. -seed sets the draws, which are\n"
"                       listed in PREFIX.balance.txt. A subject no subset drew\n"
"                       gets NA (a warning says so: raise B). With -balance the\n"
"                       projected outputs are the full fit's, in-sample: each\n"
"                       subject was scored by several models.\n"
"\n"
"  -reliability LIST    Comma list from: within, split.\n"
"                       within : a subject's own reliability, computed from\n"
"                                repeats of the same stimulus (the same Item in\n"
"                                different runs; or with -axis time and\n"
"                                -repeated_stimulus, the runs). It is the\n"
"                                correlation of the two halves' patterns\n"
"                                (r_pattern across voxels, r_series across\n"
"                                time or items), with the Spearman-Brown\n"
"                                correction, in the subject's own voxel space\n"
"                                with no model involved. It bounds what the\n"
"                                shared-space score can reach. The halves are\n"
"                                the odd and the even repeats (or runs),\n"
"                                averaged; r_pattern needs 2 repeated items (or\n"
"                                time points), r_series 3. Per -score_by level\n"
"                                too (an item counts in its first repeat's\n"
"                                level). Needs -cv: it goes into scores.txt\n"
"                                (rel_pattern, rel_series, and rel_ratio =\n"
"                                isc / rel_pattern).\n"
"                       split  : group-level. Randomly split each group's\n"
"                                subjects in two, fit S to each half, and\n"
"                                compare them with a rotation-invariant\n"
"                                subspace similarity. A group with no stable\n"
"                                shared response gives halves that disagree.\n"
"                                The similarity is the mean cos^2 of the\n"
"                                principal angles between the two S (1 = the\n"
"                                same space). Halves are half the group\n"
"                                (rounded down) against the rest; a group needs\n"
"                                4 subjects. PREFIX.reliability.txt summarizes,\n"
"                                PREFIX.reliability_splits.txt lists each\n"
"                                split. Does not need -cv.\n"
"  -nsplit N            Number of random splits for 'split' [100]. Each is two\n"
"                       group fits, warm-started from the full fit.\n"
"  -repeated_stimulus   With -axis time: every run shows the same stimulus. The\n"
"                       runs are averaged for the fit (after z-scoring each), and\n"
"                       their agreement (odd runs against even runs) gives\n"
"                       'within'. The runs must keep the same time points. The\n"
"                       fit then has one run, so -cv uses -cv_blocks, and the\n"
"                       saved model describes the averaged run. With\n"
"                       -cenmode missing, each time point is averaged over the\n"
"                       runs where the subject was observed (missing only if\n"
"                       missing in every run), and 'within' uses the time\n"
"                       points both halves observed. With -score_by, the\n"
"                       levels are read on the runs before averaging and must\n"
"                       agree in every repeat (so Run itself is an error);\n"
"                       -time_labels may then also give one label per time\n"
"                       point of the stimulus. Not with -run_coupling (there\n"
"                       is no run left to have its own W).\n"
"\n"
"  How to read them together (all subjects scored on held-out data):\n"
"    low within, low shared  : the group or condition is simply noisy.\n"
"    high within, low shared : each person is reliable but they do not\n"
"                              share a response, or the model does not\n"
"                              speak their language. Compare -cv_fit within\n"
"                              against reference.\n"
"    high within, high shared: SRM works for them.\n"
"  'Within' needs real repeats. Without them it is not estimated, and a low\n"
"  score cannot be attributed.\n"
"\n"
"  -nperm P             Test group and condition differences in the scores by\n"
"                       permutation, written to PREFIX.tests.txt [0 = none]:\n"
"                         two groups : group labels permuted over subjects\n"
"                         two levels of a -score_by factor : level labels\n"
"                                      swapped within each subject\n"
"                         both : the group x level interaction, by permuting\n"
"                                the within-subject differences between groups\n"
"                       P must be at least 20. -seed applies. Subjects are the\n"
"                       exchangeable units; the folds of one subject are never\n"
"                       split. Every score is tested (isc, match or ident, r2;\n"
"                       overall and per level for the group test), with Welch's\n"
"                       t for groups and a one-sample t for differences; p is\n"
"                       two-sided, and p_fwe is family-wise over the rows of one\n"
"                       test (max-statistic). When the relabelings number P or\n"
"                       fewer, all of them are used (exact). For anything more\n"
"                       elaborate (covariates, three groups) take\n"
"                       PREFIX.scores.txt to R.\n"
"\n""Outputs:                                                    ~1~\n"
"\n"
"  PREFIX.srm           The saved model (see above). With -model_in no new model\n"
"                       is written.\n"
"  PREFIX.S.1D          Shared response, time x k (one column per component).\n"
"                       Components are ordered by variance and sign-fixed, so\n"
"                       repeated fits are comparable up to noise.\n"
"  PREFIX.rho2.1D       Noise variance per subject (-algorithm prob).\n"
"  PREFIX.objective.1D  Objective per iteration. For prob it is the marginal\n"
"                       log-likelihood and rises; for det it is the summed\n"
"                       squared error and falls.\n"
"  PREFIX.fit.txt       One row per subject: features used, features dropped,\n"
"                       rho2, and the log of any censoring. With -update_S it\n"
"                       also gives the model's drift and number of updates.\n"
"  PREFIX.coupling.txt  With -run_coupling L: per subject and run, how far that\n"
"                       run's W is from the anchor (0 = identical, 2 =\n"
"                       orthogonal, as |W_ir - W_i|^2 / nfeatures) and the\n"
"                       gain in reconstruction R^2 over the anchor in the fit\n"
"                       data. Large deviations show where the subject's\n"
"                       mapping really moved.\n"
"\n"
"  Unless -no_projected:\n"
"  PREFIX.proj.SUBJ     Each subject's data in the shared space, W_i' X_i.\n"
"                       With -cv run or loso these are the held-out\n"
"                       projections concatenated across folds; with -cv none\n"
"                       they are in-sample.\n"
"  -proj_format FMT     dset : a dataset with k x 1 x 1 voxels and one volume\n"
"                              per time point, directly usable as input by\n"
"                              3dInSync (per-component ISC) and 3dRSA [default]\n"
"                       1D   : time x k text file\n"
"\n"
"  With -cv run or loso:\n"
"  PREFIX.scores.txt    One row per Subj x level of each -score_by factor (and\n"
"                       'all'), averaged over folds. Columns: Subj, Group,\n"
"                       Factor, Level, nsamp, isc, match (or ident), r2, and\n"
"                       with -reliability within: rel_pattern, rel_series, and\n"
"                       the ratio of the shared score to rel_pattern. Numeric\n"
"                       table columns are carried along. One score per subject\n"
"                       per condition is the right input for a group\n"
"                       comparison.\n"
"  PREFIX.scores_folds.txt\n"
"                       The same, one row per Subj x fold (x -score_by level,\n"
"                       with Factor and Level columns as in scores.txt).\n"
"  PREFIX.balance.txt   With -balance: the subjects of every fit.\n"
"  PREFIX.reliability.txt\n"
"                       With -reliability split: per Group, the subspace\n"
"                       similarity of the two halves over the splits (mean,\n"
"                       and 2.5 and 97.5 percentiles).\n"
"  PREFIX.reliability_splits.txt\n"
"                       With -reliability split: each split's two halves and\n"
"                       their similarity.\n"
"  PREFIX.tests.txt     With -nperm: the tests above, one row per score: Test,\n"
"                       Factor, Score, Contrast, N1, N2, effect, t, p, p_fwe,\n"
"                       nperm, exact.\n"
"\n"
"  Optional, since they are large:\n"
"  -save_W              PREFIX.W.SUBJ: k volumes per subject, on that subject's\n"
"                       grid. These are the spatial maps of the components.\n"
"                       A .1D subject gets one row per column of the input;\n"
"                       the rows of dropped (constant) features are zero.\n"
"  -save_recon          PREFIX.recon.SUBJ: W_i S, the denoised reconstruction of\n"
"                       the subject's time series.\n"
"  -save_r2             PREFIX.r2.SUBJ: voxelwise reconstruction R^2 for each\n"
"                       subject (held-out under -cv). Maps on a common grid\n"
"                       can go to 3dttest++.\n"
"\n"
"Resources:                                                  ~1~\n"
"\n"
"  -memory_limit G      Refuse an estimated peak above G GiB. Without this\n"
"                       option the limit is 80%% of detected physical RAM.\n"
"  -memory_override     Continue after the memory estimate exceeds its limit.\n"
"  -no_compress         Hold every subject's data whole (voxels x T), as\n"
"                       before -compress became the default. The results are\n"
"                       the same up to rounding; it needs far more memory.\n"
"  -compress            [the default when every subject has more features\n"
"                       than time points (or items), outside -model_in and\n"
"                       -apply_W; given explicitly, it is used regardless]\n"
"                       Hold each subject as an exact T x T (at most) factor\n"
"                       R of its prepared data X, with R'R = X'X (its Gram\n"
"                       matrix: see below), instead of\n"
"                       voxels x T. Everything the fit and -cv do with X goes\n"
"                       through X'X, and W always lies in the span of X, so\n"
"                       S, the scores, the projections, rho2 and the model\n"
"                       are those of -no_compress, up to rounding (about\n"
"                       1e-7; the fit starts where the uncompressed one would).\n"
"                       Subjects are read, censored, z-scored and compressed\n"
"                       one at a time, so the data are never all in memory:\n"
"                       for whole-brain naturalistic data this is the\n"
"                       difference between tens of GB and a few. -save_W and\n"
"                       -save_recon read each subject again at the end (W on\n"
"                       the voxels is X M, M small). With -nstart > 1 only\n"
"                       the first start is the uncompressed one's, so the\n"
"                       best of several starts can differ from -no_compress.\n"
"                       -save_r2 notes each scored fold's W and reference\n"
"                       and makes the voxelwise R^2 when the subject is read\n"
"                       again. Item reliability (-reliability within) is made\n"
"                       from each subject's voxels before they are\n"
"                       compressed. -cenmode drop with -zcensor reads every\n"
"                       subject once more first, to find the zero volumes.\n"
"                       Not with -model_in or -apply_W (they hold only the\n"
"                       new subjects).\n"
"                       Saves memory when subjects have more features than\n"
"                       time points (or items).\n"
"  -progress M          auto, bar, line, or off [auto].\n"
"  -quiet               Suppress progress and informational diagnostics.\n"
"  -show_table          Print the parsed table and exit.\n"
"\n"
"  Memory, compressed (the default when subjects have more features than time\n"
"  points): one subject's data while it is read, plus one input on its full\n"
"  grid as floats, plus each subject's factor (at most T x T x 4 bytes).\n"
"  With -no_compress: the sum over subjects of voxels x time x 4 bytes, plus\n"
"  voxels x k x 4 bytes per subject for W, plus the larger of the copies -cv\n"
"  run makes of the largest fold (about (T - shortest run + longest run) / T\n"
"  of the data; -cv loso and -model_in copy one subject at a time) and one\n"
"  input being read. The estimate is printed when it is over half of\n"
"  physical memory, or whenever -memory_limit is given. For example, 22\n"
"  subjects, 161k voxels, three runs (526, 452, 332 TRs), k = 50, -cv run:\n"
"  3.7 GiB and 3.9 min by default; 38.0 GiB and 6.6 min with -no_compress.\n"
"  Most of the time goes to reading, and then to matrix products, run\n"
"  subject-parallel under OpenMP.\n"
"\n"
"  A Gram matrix is X'X for data X (voxels x time points): entry (s,t) is the\n"
"  dot product of the brain patterns at time points s and t, summed over\n"
"  voxels, so it is T x T however many voxels there are.\n"
"  When every subject has at least T+k voxels (T time points), the fit forms\n"
"  each subject's (T+k) x (T+k) Gram matrix once and iterates on that, so an\n"
"  iteration costs about (T+k) x T x k per subject instead of voxels x T x k,\n"
"  and W is formed from the data at the end. This adds (T+k)^2 x 8 bytes per\n"
"  subject. The fit is the same either way, up to rounding. Setting\n"
"  AFNI_SRM_KERNEL to VOXEL or GRAM forces one path (for testing).\n"
"\n"
"Examples:                                                   ~1~\n"
"\n"
"  Replace the sample dataset names below with your own inputs. Inline\n"
"  -dataTable entries go last in the command; keep the trailing backslashes\n"
"  on every line except the last row. For -dataTableFile, save the displayed\n"
"  header and rows as plain text, without backslashes or shell quotes.\n"
"  Time-series examples assume synchronized movie runs, with enough time\n"
"  points and retained voxels for the requested number of features.\n"
"\n"
"  Example 1: Fit, with held-out run scores\n"
"\n"
"  3dSRM -prefix movie_srm -nfeatures 50 -cv run \\\n"
"        -mask brain+tlrc \\\n"
"        -dataTable Subj Group   Run InputFile \\\n"
"                   s01 control r1  s01_r1+tlrc \\\n"
"                   s01 control r2  s01_r2+tlrc \\\n"
"                   s02 control r1  s02_r1+tlrc \\\n"
"                   s02 control r2  s02_r2+tlrc\n"
"\n"
"  This writes movie_srm.srm for Examples 3 and 6. Example 8 shows how to\n"
"  choose the number of features before fitting a final model.\n"
"\n"
"  Example 2: Subjects in native space, each with their own mask\n"
"\n"
"  3dSRM -prefix native_srm -nfeatures 30 -cv loso \\\n"
"        -dataTableFile native.txt\n"
"\n"
"  Contents of native.txt (at least 3 subjects for leave-one-subject-out):\n"
"    Subj Run Mask          InputFile\n"
"    s01  r1  s01_mask+orig s01_r1+orig\n"
"    s01  r2  s01_mask+orig s01_r2+orig\n"
"    s02  r1  s02_mask+orig s02_r1+orig\n"
"    s02  r2  s02_mask+orig s02_r2+orig\n"
"    s03  r1  s03_mask+orig s03_r1+orig\n"
"    s03  r2  s03_mask+orig s03_r2+orig\n"
"\n"
"  Example 3: Place new subjects in the model from Example 1 and score them\n"
"\n"
"  The new subjects must have experienced the same movie runs, with the same\n"
"  time-point correspondence as in Example 1.\n"
"\n"
"  3dSRM -prefix new_subj -model_in movie_srm.srm -cv run \\\n"
"        -mask brain+tlrc \\\n"
"        -dataTable Subj Run InputFile \\\n"
"                   s03 r1  s03_r1+tlrc \\\n"
"                   s03 r2  s03_r2+tlrc \\\n"
"                   s04 r1  s04_r1+tlrc \\\n"
"                   s04 r2  s04_r2+tlrc\n"
"\n"
"  Example 4: Item-indexed betas, scored by category, controls as reference\n"
"\n"
"  3dSRM -prefix faces -axis item -nfeatures 20 -cv loso \\\n"
"        -mask brain+tlrc -score_by Category -cv_fit reference:control \\\n"
"        -balance 50 -reliability within,split -nperm 5000 \\\n"
"        -dataTableFile betas.txt\n"
"\n"
"  Sample rows from betas.txt:\n"
"    Subj Group   Item      Category Run InputFile\n"
"    s01  control face_017  face     r1  s01_betas_r1+tlrc[0]\n"
"    s01  control face_023  face     r1  s01_betas_r1+tlrc[1]\n"
"    s01  control house_042 house    r1  s01_betas_r1+tlrc[2]\n"
"    s01  control face_017  face     r2  s01_betas_r2+tlrc[0]\n"
"    s01  control face_023  face     r2  s01_betas_r2+tlrc[1]\n"
"    s01  control house_042 house    r2  s01_betas_r2+tlrc[2]\n"
"    s05  patient house_042 house    r1  s05_betas_r1+tlrc[0]\n"
"    s05  patient face_017  face     r1  s05_betas_r1+tlrc[1]\n"
"    s05  patient face_023  face     r1  s05_betas_r1+tlrc[2]\n"
"    s05  patient face_023  face     r2  s05_betas_r2+tlrc[0]\n"
"    s05  patient house_042 house    r2  s05_betas_r2+tlrc[1]\n"
"    s05  patient face_017  face     r2  s05_betas_r2+tlrc[2]\n"
"\n"
"  These are excerpts: include all subjects and items in the actual file.\n"
"  Use at least 4 subjects per group for split reliability, enough training\n"
"  items and voxels for 20 features, and repeated items in each category for\n"
"  within-subject reliability (at least 3 per category for rel_series).\n"
"  Item labels match stimuli across subjects even when volume order differs.\n"
"  The reference-group comparison requires -cv loso.\n"
"\n"
"  Example 5: Align on the movie, then carry each subject over to the task\n"
"\n"
"  Fit the movie data from Example 1 again with -save_W, using a new prefix:\n"
"\n"
"  3dSRM -prefix movie -nfeatures 50 -save_W -mask brain+tlrc \\\n"
"        -dataTable Subj Run InputFile \\\n"
"                   s01 r1  s01_r1+tlrc \\\n"
"                   s01 r2  s01_r2+tlrc \\\n"
"                   s02 r1  s02_r1+tlrc \\\n"
"                   s02 r2  s02_r2+tlrc\n"
"\n"
"  Apply those maps to task betas from the same subjects, on each subject's\n"
"  movie-data grid. Quote dataset selectors in an inline table so the shell\n"
"  passes the brackets to 3dSRM. Add rows for the remaining task items.\n"
"\n"
"  3dSRM -prefix task -axis item -apply_W movie \\\n"
"        -dataTable Subj Item      InputFile \\\n"
"                   s01 face_017  's01_task+tlrc[0]' \\\n"
"                   s01 face_023  's01_task+tlrc[1]' \\\n"
"                   s01 house_042 's01_task+tlrc[2]' \\\n"
"                   s02 house_042 's02_task+tlrc[0]' \\\n"
"                   s02 face_017  's02_task+tlrc[1]' \\\n"
"                   s02 face_023  's02_task+tlrc[2]'\n"
"\n"
"  Example 6: Add another site's subjects to the model from Example 1\n"
"\n"
"  As in Example 3, these subjects saw the same movie runs. -update_S updates\n"
"  the shared response using the new subjects; the original data stay local.\n"
"\n"
"  3dSRM -prefix movie_v2 -model_in movie_srm.srm -update_S -cv run \\\n"
"        -mask brain+tlrc \\\n"
"        -dataTable Subj Run InputFile \\\n"
"                   s05 r1  s05_r1+tlrc \\\n"
"                   s05 r2  s05_r2+tlrc \\\n"
"                   s06 r1  s06_r1+tlrc \\\n"
"                   s06 r2  s06_r2+tlrc\n"
"\n"
"  Example 7: Allow each run its own W, tied to the subject's anchor\n"
"\n"
"  Use the same data as Example 1 to compare shared W with run-specific W:\n"
"\n"
"  3dSRM -prefix movie_rc -nfeatures 50 -run_coupling 200 -cv run \\\n"
"        -cv_adapt 0.25 -mask brain+tlrc \\\n"
"        -dataTable Subj Group   Run InputFile \\\n"
"                   s01 control r1  s01_r1+tlrc \\\n"
"                   s01 control r2  s01_r2+tlrc \\\n"
"                   s02 control r1  s02_r1+tlrc \\\n"
"                   s02 control r2  s02_r2+tlrc\n"
"\n"
"  Example 8: Choose k using held-out scores\n"
"\n"
"  Save the table from Example 1 as subjects.txt to reuse it across commands:\n"
"    Subj Group   Run InputFile\n"
"    s01  control r1  s01_r1+tlrc\n"
"    s01  control r2  s01_r2+tlrc\n"
"    s02  control r1  s02_r1+tlrc\n"
"    s02  control r2  s02_r2+tlrc\n"
"\n"
"  3dSRM -prefix scan -nfeatures 10,20,30,50,80 -cv run \\\n"
"        -mask brain+tlrc -dataTableFile subjects.txt\n"
"\n"
"  Inspect scan.nfeat.txt, then rerun Example 1 with the chosen -nfeatures\n"
"  value and a fresh prefix to write the model and projected data.\n"
"\n"
"Reading the results:                                        ~1~\n"
"\n"
"  * The components have no fixed meaning. Any rotation of S, applied with the\n"
"    opposite rotation to every W_i, fits equally well, and other seeds find\n"
"    others. Do not test component 3's map across subjects, or compare\n"
"    component numbers between runs of the program.\n"
"  * Quantities that do not depend on that rotation are safe: the reconstruction\n"
"    R^2, the mean projected-space correlation, and segment-matching accuracy.\n"
"    Use their held-out versions for inference.\n"
"  * Under -cv loso every subject's score comes from a model that never saw\n"
"    them, so the scores are fair to compare between subjects and groups.\n"
"    Under -cv run the subjects all contributed to the model that scores them\n"
"    on other runs, which suits comparing methods, k, or ROIs, but is less\n"
"    clean for between-subject inference.\n"
"  * To compare groups, use -cv_fit and -balance. A pooled model is built more\n"
"    from the larger or less noisy group and maps that group better, with\n"
"    nothing to do with the groups differing in function.\n"
"  * The model fits samples in proportion to their variance, so a condition\n"
"    with a large response can dominate and a weak one be fitted poorly even\n"
"    in everyone. Score by condition, and check that a weak condition is weak\n"
"    in 'within' too before reading it as a failure of SRM.\n"
"  * If you will RSA the aligned task data, align on other data (-apply_W),\n"
"    or score with held-out folds. SRM maximizes cross-subject agreement of\n"
"    its own columns, which inflates their shared structure.\n"
"  * Larger k always fits the training data better. Choose k by held-out\n"
"    score, not by training fit.\n"
"\n"
"Scope boundary:                                             ~1~\n"
"\n"
"  Version 1 fits and validates SRM on time-locked or item-indexed columns,\n"
"  maps new subjects into a saved model and optionally updates the model's S\n"
"  from them, carries a fitted W to other data, allows a separate run-wise W\n"
"  tied to a subject anchor, and scores reliability by subject, group, and\n"
"  condition. Not yet provided: a prior on S that depends on condition,\n"
"  exact (data-based) refitting from a model alone, searchlight or\n"
"  atlas-parcel looping, robust SRM, and time-lagged or convolved fusion\n"
"  of EEG with fMRI. The fitting library\n"
"  (thd_srm) is written so that these can be added without changing the\n"
"  options above.\n"
"\n") ;
   PRINT_AFNI_OMP_USAGE(PROGRAM_NAME,NULL) ;
   PRINT_COMPILE_DATE ;
}


/*============================================================================
  Options
==============================================================================*/

#define GIB 1073741824.0

#define PROG_AUTO 0
#define PROG_BAR  1
#define SRM_CEN_INTERP 0          /* filled in, within the run, from the subject's neighbors */
#define SRM_CEN_DROP   1          /* dropped for everyone */
#define SRM_CEN_MISSING 2         /* missing for that subject only (SRM_params.obs) */

#define PROG_LINE 2
#define PROG_OFF  3

typedef struct {
   char   *prefix ;
   int     nk , *klist ;             /* -nfeatures */
   SRM_params par ;                  /* algorithm, niter, tol, seed, coupling, ... */
   int     nstart ;
   char   *maskname , *run_col , *group_col , *censor_name ;
   int     zscore ;                  /* SRM_ZSCORE_* */
   int     zcensor ; float zcensor_warn ; int zcensor_warn_given ;
   int     cenmode ; int cenmode_given ;  /* SRM_CEN_*: what a subject's own censored time points become */
   int     axis_item ; char *item_col ; int item_col_given ; char *repeat_col ;
   int     lss ;                    /* LSS beta datasets plus Events and Xmat */
   char   *xmat_stims ;              /* -xmat_stims: the conditions an Xmat table expands to (all) */
   double  xmat_label_frac ;         /* -xmat_label_frac: a TR is a condition's at this fraction of its peak */
   int     repeated_stimulus ;
   int     compress ;                /* each subject held as R, R'R = X'X (the default when it saves memory) */
   int     compress_given , no_compress ;
   char   *cvmode ;                  /* none | run | loso */
   int     cv_blocks , match_window , loso_iter ; int cv_blocks_given , match_window_given ;
   int     loso_legacy , loso_start_given , loso_iter_given ;
   char   *cv_metrics , *fold_col , *score_by , *cv_fit ;
   int     balance , nsplit , nperm ;
   char   *reliability ; int rel_within , rel_split ;
   double  cv_adapt ; int cv_adapt_given ;
   char   *model_in ; int update_S ;
   char   *apply_W ;
   int     save_W , save_recon , save_r2 , no_projected ;
   int     proj_dset ;               /* 1: dataset, 0: 1D */
   double  memory_limit_gib ; int memory_limit_given , memory_override ;
   int     quiet , progress ;
   int     show_table ;
   int     ntl ; char **tl_name , **tl_file ;
   char   *concat ;                  /* -concat: run starts within each subject's one input */
   int     given_algo , given_niter , given_tol , given_seed , given_zscore , given_coupling , given_mask ;
} srm_opts ;

/*! Fill o with the program defaults.  AFNI_SRM_KERNEL (VOXEL or GRAM) in the
    environment forces one of the library's two fitting kernels, for testing. */
static void srm_opts_default( srm_opts *o )
{
   memset(o,0,sizeof(*o)) ;
   SRM_params_default(&o->par) ;
   { char *ek=my_getenv("AFNI_SRM_KERNEL") ;      /* see Resources in the help */
     if( ek!=NULL && strcasecmp(ek,"VOXEL")==0 ) o->par.kernel=SRM_KERNEL_VOXEL ;
     else if( ek!=NULL && strcasecmp(ek,"GRAM")==0 ) o->par.kernel=SRM_KERNEL_GRAM ; }
   o->prefix=strdup("SRM") ;
   o->nstart=1 ; o->zscore=SRM_ZSCORE_RUN ; o->zcensor_warn=20.0f ;
   o->item_col=strdup("Item") ; o->repeat_col=strdup("Repeat") ; o->xmat_label_frac=0.5 ; o->run_col=strdup("Run") ; o->group_col=strdup("Group") ;
   o->cvmode=strdup("none") ; o->cv_blocks=2 ; o->match_window=9 ; o->loso_iter=5 ;
   o->proj_dset=1 ; o->progress=PROG_AUTO ;
}

/*! Comma list of positive integers (each 1..100000), e.g. "-nfeatures 10,20,50".
    Returns a malloc'ed array and its length in *n, or NULL if any entry is bad. */
static int * srm_parse_intlist( const char *s , int *n )
{
   char *copy=strdup(s) , *tok ; int *v=NULL , nn=0 ;
   for( tok=strtok(copy,",") ; tok!=NULL ; tok=strtok(NULL,",") ){
     char *end ; long x=strtol(tok,&end,10) ;
     if( *tok=='\0' || *end!='\0' || x<1 || x>100000 ){ free(copy) ; free(v) ; return NULL ; }
     v=(int *)realloc(v,sizeof(int)*(nn+1)) ; v[nn++]=(int)x ;
   }
   free(copy) ; *n=nn ;
   return v ;
}

/*============================================================================
  Progress: a bar like 3dRSA's and 3dInSync's, labelled by what is going on
==============================================================================*/

typedef struct {
   int enabled , mode , is_tty , total , done , last_bucket ;
   double start , last ;
   char label[24] , unit[24] ;
} srm_prog ;

/*! Start a progress display: a redrawn bar on a terminal (PROG_BAR), or
    periodic INFO lines (PROG_LINE, e.g. when stderr goes to a log).  Disabled
    by -quiet, -progress off, or total <= 0. */
static void srm_prog_begin( srm_prog *p , int mode , int quiet , const char *label ,
                            const char *unit , int total )
{
   memset(p,0,sizeof(*p)) ;
   p->enabled=!quiet && mode!=PROG_OFF && total>0 ;
   p->is_tty=isatty(fileno(stderr)) ; p->total=total ;
   p->mode=(mode==PROG_AUTO) ? (p->is_tty ? PROG_BAR : PROG_LINE) : mode ;
   p->start=p->last=0.001*(double)NI_clock_time() ;
   p->last_bucket=-1 ;
   snprintf(p->label,sizeof(p->label),"%s",label) ; snprintf(p->unit,sizeof(p->unit),"%s",unit) ;
   if( p->enabled && p->mode==PROG_BAR ){
     fprintf(stderr,"++ %-8s [--------------------] 0/%d %s",p->label,total,p->unit) ;
     fflush(stderr) ;
   }
}

/*! Report done (absolute, 0..total).  Bars repaint at most once a second;
    line mode reports every tenth. */
static void srm_prog_set( srm_prog *p , int done , const char *extra )
{
   double now ; int bucket ;
   if( !p->enabled ) return ;
   if( done>p->total ) done=p->total ;
   p->done=done ;
   now=0.001*(double)NI_clock_time() ; bucket=(10*done)/p->total ;
   if( done==p->total || (p->mode==PROG_BAR && now-p->last>=1.0) ||
       (p->mode==PROG_LINE && bucket>p->last_bucket) ){
     int ii , fill=(20*done)/p->total ; char tm[48] ; double el=now-p->start ;
     if( done<p->total && 10*done>=p->total && done>0 )
       snprintf(tm,sizeof(tm)," [%.0fs, about %.0fs to go]",el,el*(p->total-done)/done) ;
     else snprintf(tm,sizeof(tm)," [%.0fs]",el) ;
     if( p->mode==PROG_BAR ){
       fprintf(stderr,"\r++ %-8s [",p->label) ;
       for( ii=0 ; ii<20 ; ii++ ) fputc(ii<fill ? '#' : '-',stderr) ;
       fprintf(stderr,"] %d/%d %s%s%s%s",done,p->total,p->unit,extra?", ":"",extra?extra:"",tm) ;
       if( p->is_tty ) fputs("\033[K",stderr) ;
       if( done==p->total ) fputc('\n',stderr) ;
       fflush(stderr) ;
     } else {
       INFO_message("%s: %d/%d %s%s%s%s",p->label,done,p->total,p->unit,extra?", ":"",extra?extra:"",tm) ;
     }
     p->last=now ; p->last_bucket=bucket ;
   }
}

/*! End a bar that stopped short of its total (a fit that converged early). */
static void srm_prog_end( srm_prog *p )
{
   if( !p->enabled || p->done>=p->total ) return ;
   if( p->mode==PROG_BAR ){
     if( p->is_tty ) fputs("\033[K",stderr) ;
     fputc('\n',stderr) ; fflush(stderr) ;
   }
}

/* progress for the fit: iterations of the current start.  srm_fit_progress is
   handed to the library as SRM_params.progress, and called once per iteration
   with the objective; iteration 0 marks the beginning of a new random start. */
typedef struct { srm_prog *bar ; int istart , nstart ; double first_obj ; const char *tag ; } fit_prog ;

static void srm_fit_progress( int it , int niter , double obj , void *vctx )
{
   fit_prog *c=(fit_prog *)vctx ; char extra[96] ;
   if( it==0 ){ c->istart++ ; c->first_obj=obj ; }
   if( c->nstart>1 ) snprintf(extra,sizeof(extra),"%s%sstart %d/%d, objective %.8g",c->tag?c->tag:"",c->tag?", ":"",c->istart,c->nstart,obj) ;
   else              snprintf(extra,sizeof(extra),"%s%sobjective %.8g",c->tag?c->tag:"",c->tag?", ":"",obj) ;
   srm_prog_set(c->bar,it,extra) ;
}

/*============================================================================
  Subjects and their data
==============================================================================*/

/* One subject.  Life cycle of x: srm_data_read fills it as nvox_mask x Tin
   (every input time point); srm_data_timeline packs it to the retained T
   columns; srm_data_normalize z-scores it and drops constant rows (nvox <=
   nvox_mask, vox[] compacted to match).  With -compress, x is then replaced by
   R (rank x T, R'R = X'X), nvox becomes the rank, and ndim remembers how many
   voxels R stands for.  All matrices are row-major, feature f's series at
   x + f*T, as in thd_srm.h. */
typedef struct {
   char   *label ;
   int     group ;
   int     nrun ;
   int    *row ;                     /* table row of each run, in run order */
   char   *mask_name ;               /* this subject's Mask, or NULL */
   int     is1D ;
   THD_3dim_dataset *geom ;          /* first dataset: geometry for outputs */
   int     nvox_mask , nvox , ndrop ;
   int    *vox ;                     /* grid index of each feature (datasets) */
   float  *x ;                       /* nvox x T, retained timeline, normalized */
   float  *xt ;                      /* spare */
   /* -axis item: column j is the model's item j */
   int    *irow , *irun ;            /* [nitem] its table row, and the run it is in (0 with no Run column) */
   int    *ifile , *ibrick ;         /* [nitem] which of file[] holds it, and the sub-brick there */
   int     nrow_item ;               /* -apply_W -axis item: the subject's rows of the table, in irow */
   char  **file ; int nfile ;        /* the distinct datasets (selector removed), each read once */
   /* one input holding all runs (-concat, or an Xmat column) */
   char   *xmat ;                    /* this subject's X.xmat.1D, or NULL */
   char   *censor_file ;             /* this subject's Censor column (1 keep, 0 censored), or NULL */
   byte   *zflag ;                   /* [Tin] volumes all zero in the mask (-zcensor), or NULL */
   int     ncens ;                   /* the subject's own censored time points */
   byte   *miss_in ;                 /* -cenmode missing: [Tin] 1 where missing, or NULL */
   byte   *obs ;                     /* the same on the retained timeline, 1 = observed, or NULL */
   int    *good , ngood ;            /* its GoodList: the uncensored time points (NULL: all) */
   /* -compress: x is R (nvox = rank rows, R'R = X'X) standing for ndim features */
   int     compressed ;
   double  ndim ;                    /* the features it stands for (the voxels used) */
   float  *w0 ; int w0K ;            /* the fit's random start carried into R (rank x w0K), or NULL */
   int    *vox0 , nvox_mask0 ;       /* the feature list before dropping, to read the subject again */
} srm_subj ;

/*! The subjects of one fit (sb[sel[j]], j < n; sel NULL: sb[j]) with K components: each starts from the random
    start of its own index in the whole table (orig[], the whole-table index of each entry of sb; NULL: its
    position), whatever subset the fit is of; with -compress, also the voxel counts and those starts carried
    into R.  Set into par; srm_cmp_free afterwards.

    Why the stream matters: the library draws subject i's random starting W
    from (seed, stream i).  Giving each subject its whole-table index as the
    stream means a subject starts identically in the full fit and in every
    subset or fold fit, so differences between fits are not start noise.  Why
    w0: a compressed subject is R = U'X; the uncompressed fit's start W0 is
    carried into that space as U'W0 (SRM_compress_ex), so the compressed fit
    follows the uncompressed one step for step.  The w0 stored on the subject
    was made for the largest K; the first K columns are the start for K
    components because the random orthonormal start is built column by column
    (Gram-Schmidt), so its first K columns are the K-column start. */
typedef struct { double *ndim ; float **w0 ; int *stream ; int n ; SRM_params *par ; } srm_cmp ;

static void srm_cmp_set( const srm_subj *sb , const int *orig , const int *sel , int n , int K , SRM_params *par , srm_cmp *c )
{
   int j , f , q ;
   memset(c,0,sizeof(*c)) ;
   if( n<1 ) return ;
   c->stream=(int *)malloc(sizeof(int)*(size_t)n) ; c->par=par ;
   for( j=0 ; j<n ; j++ ){ int e=(sel!=NULL) ? sel[j] : j ; c->stream[j]=(orig!=NULL) ? orig[e] : e ; }
   par->stream=c->stream ;
   if( !sb[0].compressed ) return ;
   c->n=n ; c->ndim=(double *)malloc(sizeof(double)*(size_t)n) ; c->w0=(float **)calloc((size_t)n,sizeof(float *)) ;
   for( j=0 ; j<n ; j++ ){
     const srm_subj *s=sb+(sel!=NULL ? sel[j] : j) ;
     c->ndim[j]=s->ndim ;
     if( s->w0!=NULL && K<=s->w0K ){                    /* the first K columns: the start for K components */
       c->w0[j]=(float *)malloc(sizeof(float)*(size_t)s->nvox*K) ;
       for( f=0 ; f<s->nvox ; f++ ) for( q=0 ; q<K ; q++ ) c->w0[j][(size_t)f*K+q]=s->w0[(size_t)f*s->w0K+q] ;
     }
   }
   par->ndim=c->ndim ; par->w0=(float * const *)c->w0 ; c->par=par ;
}

/*! Undo srm_cmp_set: free its arrays and clear the borrowed pointers in par. */
static void srm_cmp_free( srm_cmp *c )
{
   int j ;
   if( c->w0!=NULL ) for( j=0 ; j<c->n ; j++ ) free(c->w0[j]) ;
   if( c->par!=NULL ){ c->par->ndim=NULL ; c->par->w0=NULL ; c->par->stream=NULL ; }
   free(c->w0) ; free(c->ndim) ; free(c->stream) ; memset(c,0,sizeof(*c)) ;
}


/*! 1 if an input name is a text matrix: "1D:" inline syntax, or a name ending in
    .1D (ignoring any [..] column selector). */
static int srm_is1D( const char *name )
{
   const char *br=strchr(name,'[') ; size_t n=br ? (size_t)(br-name) : strlen(name) ;
   if( strncmp(name,"1D:",3)==0 ) return 1 ;
   return n>3 && strncasecmp(name+n-3,".1D",3)==0 ;
}

/*! File-name-safe form of a label. */
static void srm_safe( char *dst , size_t nd , const char *src )
{
   size_t i ;
   for( i=0 ; i+1<nd && src[i] ; i++ ){
     char c=src[i] ;
     dst[i]=(isalnum((unsigned char)c) || c=='_' || c=='-' || c=='.') ? c : '_' ;
   }
   dst[i]='\0' ;
}

/*! Does a dataset with this prefix already exist, under any of AFNI's names
    (+orig/+tlrc/+acpc BRIK/HEAD, .nii, .nii.gz, or the name itself)?  If so and
    found!=NULL, its openable name goes in found. */
static int srm_dset_find( const char *prefix , char *found , size_t nfound )
{
   static const char *suf[]={"+orig.HEAD","+tlrc.HEAD","+acpc.HEAD",".nii",".nii.gz",NULL} ;
   char fn[THD_MAX_NAME] ; int ii ;
   for( ii=0 ; suf[ii]!=NULL ; ii++ ){
     snprintf(fn,sizeof(fn),"%s%s",prefix,suf[ii]) ;
     if( THD_is_file(fn) ){ if( found!=NULL ) snprintf(found,nfound,"%s",fn) ; return 1 ; }
   }
   if( THD_is_file((char *)prefix) ){ if( found!=NULL ) snprintf(found,nfound,"%s",prefix) ; return 1 ; }
   return 0 ;
}

static int srm_dset_exists( const char *prefix ){ return srm_dset_find(prefix,NULL,0) ; }

/* A mask read once and shared by every subject that names it (subjects often
   share one -mask).  mask[] is 0/1 over the grid's nvox voxels, nmask of them on. */
typedef struct { char *name ; byte *mask ; int nvox , nmask ; THD_3dim_dataset *ds ; } mask_entry ;

/*! The cached mask for name, loading it the first time (nonzero voxels, from
    THD_makemask).  The cache array may be realloc'ed, so a returned pointer is
    valid only until the next new mask is added.  ERROR_exit on an unreadable or
    empty mask. */
static mask_entry * srm_mask_get( mask_entry **cache , int *ncache , const char *name )
{
   int ii ; mask_entry *e ;
   for( ii=0 ; ii<*ncache ; ii++ ) if( strcmp((*cache)[ii].name,name)==0 ) return (*cache)+ii ;
   *cache=(mask_entry *)realloc(*cache,sizeof(mask_entry)*(*ncache+1)) ;
   e=(*cache)+*ncache ; (*ncache)++ ;
   e->name=strdup(name) ;
   e->ds=THD_open_dataset((char *)name) ; CHECK_OPEN_ERROR(e->ds,name) ;
   DSET_load(e->ds) ; CHECK_LOAD_ERROR(e->ds) ;
   e->nvox=DSET_NVOX(e->ds) ;
   e->mask=THD_makemask(e->ds,0,0.5f,0.0f) ;
   if( e->mask==NULL ) ERROR_exit("cannot make a mask from '%s'",name) ;
   e->nmask=THD_countmask(e->nvox,e->mask) ;
   if( e->nmask<1 ) ERROR_exit("mask '%s' has no nonzero voxels",name) ;
   return e ;
}

/*! Read dataset ds (masked voxels vox[0..nvox-1]) into x, a nvox x ldx array,
    columns col0 ... col0+nt-1.  Also flags, for -zcensor, volumes that are all
    zero in the mask.  Returns the number of non-finite values met (and the
    first one's location) so the caller can name the culprit.

    Volume by volume: THD_extract_float_brick returns volume tt as floats with
    the brick's scale factor applied (datasets are often stored as shorts plus
    a factor), and the masked voxels are scattered into column col0+tt of x.
    Non-finite values are counted and stored as 0 here; callers treat any as an
    error.  The dataset's bricks are unloaded at the end to free memory. */
static int srm_read_dset( THD_3dim_dataset *ds , const int *vox , int nvox , float *x ,
                          int ldx , int col0 , byte *zero_flag , int *bad_t )
{
   int nt=DSET_NVALS(ds) , tt , nbad=0 ;
   DSET_load(ds) ; CHECK_LOAD_ERROR(ds) ;
   *bad_t=-1 ;
   for( tt=0 ; tt<nt ; tt++ ){
     MRI_IMAGE *fim=THD_extract_float_brick(tt,ds) ;
     float *far ; int vv , nz=0 ;
     if( fim==NULL ) ERROR_exit("cannot read volume %d of '%s'",tt,DSET_BRIKNAME(ds)) ;
     far=MRI_FLOAT_PTR(fim) ;
     for( vv=0 ; vv<nvox ; vv++ ){
       float f=far[vox[vv]] ;
       if( !isfinite(f) ){ nbad++ ; if( *bad_t<0 ) *bad_t=tt ; f=0.0f ; }
       if( f!=0.0f ) nz=1 ;
       x[(size_t)vv*ldx+col0+tt]=f ;
     }
     if( zero_flag!=NULL ) zero_flag[col0+tt]=(byte)!nz ;
     mri_free(fim) ;
   }
   DSET_unload(ds) ;
   return nbad ;
}

/*============================================================================
  Output helpers
==============================================================================*/

/*! Write a (nrow x ncol, row-major) as text, one row per line, after an
    optional '# comment' line.  ERROR_exit if the file cannot be opened. */
static void srm_write_matrix_1D( const char *fname , const float *a , int nrow , int ncol ,
                                 const char *comment )
{
   FILE *fp=fopen(fname,"w") ; int i , j ;
   if( fp==NULL ) ERROR_exit("cannot write '%s'",fname) ;
   if( comment!=NULL ) fprintf(fp,"# %s\n",comment) ;
   for( i=0 ; i<nrow ; i++ ){
     for( j=0 ; j<ncol ; j++ ) fprintf(fp,"%s%.7g",j?" ":"",a[(size_t)i*ncol+j]) ;
     fputc('\n',fp) ;
   }
   fclose(fp) ;
}

/*! Transposed write: a is ncol x nrow row-major (components by time); the file
    has one row per time point. */
static void srm_write_transposed_1D( const char *fname , const float *a , int ncomp , int ntime ,
                                     const char *comment )
{
   FILE *fp=fopen(fname,"w") ; int i , j ;
   if( fp==NULL ) ERROR_exit("cannot write '%s'",fname) ;
   if( comment!=NULL ) fprintf(fp,"# %s\n",comment) ;
   for( j=0 ; j<ntime ; j++ ){
     for( i=0 ; i<ncomp ; i++ ) fprintf(fp,"%s%.7g",i?" ":"",a[(size_t)i*ntime+j]) ;
     fputc('\n',fp) ;
   }
   fclose(fp) ;
}

/*! Dataset of nbrick volumes on geom's grid, written as prefix.  vols[b] is
    the full-grid (DSET_NVOX) float buffer for brick b; the dataset takes
    ownership of each buffer (the caller frees only the vols array).  Bricks
    are labelled label_stem1, label_stem2, ... and marked as non-statistics. */
static void srm_write_grid_dset( THD_3dim_dataset *geom , const char *prefix , int nbrick ,
                                 float **vols , const char *label_stem , int argc , char **argv )
{
   THD_3dim_dataset *out=EDIT_empty_copy(geom) ; int b ; char lab[64] ;
   EDIT_dset_items(out,ADN_prefix,prefix,ADN_nvals,nbrick,ADN_ntt,0,ADN_brick_fac,NULL,
                   ADN_type,HEAD_FUNC_TYPE,ADN_func_type,FUNC_BUCK_TYPE,ADN_datum_all,MRI_float,ADN_none) ;
   tross_Copy_History(geom,out) ; tross_Make_History("3dSRM",argc,argv,out) ;
   for( b=0 ; b<nbrick ; b++ ){
     EDIT_substitute_brick(out,b,MRI_float,vols[b]) ;
     snprintf(lab,sizeof(lab),"%s%d",label_stem,b+1) ;
     EDIT_BRICK_LABEL(out,b,lab) ; EDIT_BRICK_TO_NOSTAT(out,b) ;
   }
   DSET_write(out) ; WROTE_DSET(out) ; DSET_delete(out) ;
}

/*============================================================================
  Cross-validation

  Vocabulary.  A "fold" is a set of held-out columns (a run, a time block, or
  the items of one level of a table column).  For each fold a model is fit
  without it, every subject's held-out data are projected into the shared
  space (P = W_i' x), and each subject is scored against a REFERENCE built
  only from the other subjects' held-out projections: the leave-one-out shared
  response.  Three scores (SRM_score_view in thd_srm.c):
    isc   per-component correlation of P with the reference, Fisher-z averaged;
    match time-segment matching (or item identification on the item axis);
    r2    reconstruction R^2 of the subject's held-out voxels from W_i times
          the reference.  For a probabilistic model r2 uses the posterior-mean
          reference (ref_r, weighted by 1/rho2 and shrunk by Sigma_s, the same
          scale as the fitted S); isc and match use the plain mean (ref_u),
          since correlation does not care about scale.
==============================================================================*/

#define M_ISC SRM_SCORE_ISC
#define M_MATCH SRM_SCORE_MATCH
#define M_CHANCE SRM_SCORE_CHANCE
#define M_R2 SRM_SCORE_R2
#define M_NSAMP SRM_SCORE_NSAMP
#define NMET SRM_SCORE_COUNT

typedef SRM_score_request srm_want ;   /* ident: 'match' is item identification */

typedef struct {
   srm_opts *o ; srm_subj *sb ; int nsub , nrun , T , K ;
   int anymiss ;                          /* some subject has missing time points (s->obs) */
   /* -score_by: factors, their levels, and each subject's level at each column (-1: none) */
   int nfac , L ;                         /* factors, and levels in all */
   char **fac_name ; int *fac_nlev , *fac_off ; char ***fac_lev ;
   int ***lab ;                           /* [nfac][nsub][T] */
   double *rel ;                          /* -reliability within: [nsub][1+L][2] rel_pattern, rel_series, or NULL */
   int *seglen ; char **runname ; char **glabel ; int ngroup ;
   SRM_params par ;                       /* algorithm, K, center, coupling ... */
   int quiet , progress ;
   int *sidx ;                            /* the whole table's index of each subject (in a subset), or NULL */
} srm_ctx ;

typedef struct {
   THD_foldplan *plan ;                   /* owned common plan; subsets borrow it */
   int nfold , by_blocks , items ;
   int **col , *len ;                     /* aliases of plan->test/ntest for common folds */
   int ***scol , **slen ;                 /* or each subject's own (-cv loso on randomized items), or NULL */
   char **name ;
   const char *unit ;                     /* "time points" or "items" */
} srm_folds ;

typedef struct {
   int nsub , nfold ;
   double *score ;                        /* [nsub][nfold][NMET] */
   double *lscore ;                       /* [nsub][nfold][L][NMET] the -score_by levels, or NULL */
   int L ;
   float **proj ;                         /* [nsub] K x T held-out projections, or NULL */
   float **vr2 ;                          /* [nsub] mean voxelwise held-out R^2, or NULL */
   int r2defer , r2id ;                   /* -compress -save_r2: the voxelwise R^2 is noted (srm_r2_note) and made on
                                             the voxels when each subject is read again; r2id tells fits apart */
   double *ascore ;                       /* -cv_adapt: [nsub][nfold][NMET] the anchor's scores on the same
                                             samples as score (which are the run's own W's), or NULL */
} srm_cvres ;

/*! -cv_metrics LIST -> which scores to compute (NULL: all three).  "ident" is
    accepted for "match"; whether match means item identification is set later
    from -axis. */
static srm_want srm_parse_metrics( const char *s )
{
   srm_want w ; char *copy , *tok ;
   memset(&w,0,sizeof(w)) ;
   if( s==NULL ){ w.isc=w.match=w.r2=1 ; return w ; }
   copy=strdup(s) ;
   for( tok=strtok(copy,",") ; tok!=NULL ; tok=strtok(NULL,",") ){
     if( strcasecmp(tok,"isc")==0 ) w.isc=1 ;
     else if( strcasecmp(tok,"match")==0 || strcasecmp(tok,"ident")==0 ) w.match=1 ;
     else if( strcasecmp(tok,"r2")==0 ) w.r2=1 ;
     else ERROR_exit("-cv_metrics: '%s' is not isc, match, or r2",tok) ;
   }
   free(copy) ;
   if( !w.isc && !w.match && !w.r2 ) ERROR_exit("-cv_metrics names no metric") ;
   return w ;
}

/*! Common folds own one plan. The legacy field names are borrowed aliases;
    subject-specific folds retain their existing representation for now.
    assignment[t] (t < T) is the fold of column t; the plan (thd_crossval.h)
    lists each fold's test columns and its training complement, in order. */
static void srm_common_folds( srm_folds *f , int T , const int *assignment , char **names , int nf )
{
   char err[512];
   f->plan=THD_foldplan_from_assignments(T,nf,assignment,(const char * const *)names,err,sizeof(err));
   if(f->plan==NULL)ERROR_exit("cannot build SRM folds: %s",err);
   f->nfold=f->plan->nfold; f->col=f->plan->test; f->len=f->plan->ntest; f->name=f->plan->name;
}

/*! Folds of the time axis: the runs, or with one run, contiguous blocks of time.
    With blocks, the first T % blocks blocks get one extra time point, so
    block lengths differ by at most one. */
static void srm_make_folds( srm_folds *f , int nrun , const int *seglen , char **runname , int T , int blocks )
{
   int ii,jj,acc=0,nf=(nrun>=2)?nrun:blocks;
   int *assignment; char **names;
   memset(f,0,sizeof(*f)); f->unit="time points"; f->by_blocks=(nrun<2);
   if(T<2 || nf<2 || nf>T)ERROR_exit("SRM folds need 2 <= number of folds <= number of time points");
   assignment=(int *)malloc(sizeof(int)*(size_t)T);
   names=(char **)calloc((size_t)nf,sizeof(char *));
   if(!assignment || !names)ERROR_exit("out of memory building SRM folds");
   for(ii=0;ii<nf;ii++){
      int len=(nrun>=2)?seglen[ii]:T/blocks+(ii<T%blocks); char nm[24];
      if(len<1 || len>T-acc)ERROR_exit("invalid run length building SRM folds");
      if(nrun>=2)names[ii]=strdup(runname[ii]);
      else{snprintf(nm,sizeof(nm),"block%d",ii+1);names[ii]=strdup(nm);}
      if(!names[ii])ERROR_exit("out of memory naming SRM folds");
      for(jj=0;jj<len;jj++)assignment[acc++]=ii;
   }
   if(acc!=T)ERROR_exit("SRM run lengths do not cover the retained timeline");
   srm_common_folds(f,T,assignment,names,nf);
   for(ii=0;ii<nf;ii++)free(names[ii]);
   free(names);free(assignment);
}

/*! Free a srm_folds: the common plan (which owns col/len/name), or the legacy
    arrays, plus any per-subject folds (scol/slen, NULL-terminated by subject). */
static void srm_free_folds( srm_folds *f )
{
   int ii ;
   if(f->plan!=NULL) THD_foldplan_free(f->plan);
   else {
     for( ii=0 ; ii<f->nfold ; ii++ ){ free(f->name[ii]) ; if( f->col!=NULL ) free(f->col[ii]) ; }
     free(f->name) ; free(f->col) ; free(f->len) ;
   }
   if( f->scol!=NULL ){ int ss ; for( ss=0 ; f->scol[ss]!=NULL ; ss++ ){
       for( ii=0 ; ii<f->nfold ; ii++ ) free(f->scol[ss][ii]) ; free(f->scol[ss]) ; free(f->slen[ss]) ; }
     free(f->scol) ; free(f->slen) ; }
}

/*! Columns col[0..len-1] of x (V x T), in that order, as a new V x len array.
    This is how a fold's held-out (or training) data are cut out of a subject's
    full matrix: each row is gathered independently.  ERROR_exit if out of memory. */
static float * srm_cols_in( const float *x , int V , int T , const int *col , int len )
{
   float *y=(float *)malloc(sizeof(float)*(size_t)V*(size_t)len) ; int f , t ;
   if( y==NULL ) ERROR_exit("out of memory building a fold (%.2f GiB)",4.0*V*(double)len/GIB) ;
   for( f=0 ; f<V ; f++ ){
     const float *src=x+(size_t)f*T ; float *dst=y+(size_t)f*len ;
     for( t=0 ; t<len ; t++ ) dst[t]=src[col[t]] ;
   }
   return y ;
}

/*! srm_cols_in into a caller's buffer y (V x len). */
static void srm_cols_in_into( const float *x , int V , int T , const int *col , int len , float *y )
{
   int f , t ;
   for( f=0 ; f<V ; f++ ){
     const float *src=x+(size_t)f*T ; float *dst=y+(size_t)f*len ;
     for( t=0 ; t<len ; t++ ) dst[t]=src[col[t]] ;
   }
}

/*! The complement of srm_cols_in: every column of x (V x T) NOT in col[0..len-1],
    in time order, as a new V x (T-len) array (a fold's training data). */
static float * srm_cols_out( const float *x , int V , int T , const int *col , int len )
{
   int Tr=T-len , f , t , q ; byte *out=(byte *)calloc((size_t)T,1) ;
   float *y=(float *)malloc(sizeof(float)*(size_t)V*(size_t)Tr) ;
   if( y==NULL || out==NULL ) ERROR_exit("out of memory building a fold (%.2f GiB)",4.0*V*(double)Tr/GIB) ;
   for( t=0 ; t<len ; t++ ) out[col[t]]=1 ;
   for( f=0 ; f<V ; f++ ){
     const float *src=x+(size_t)f*T ; float *dst=y+(size_t)f*Tr ;
     for( t=0,q=0 ; t<T ; t++ ) if( !out[t] ) dst[q++]=src[t] ;
   }
   free(out) ;
   return y ;
}

/*! A 0/1 mask over T columns restricted to col[0..len-1] (out 0), or to the others (out 1); NULL stays NULL. */
static unsigned char * srm_mask_cols( const unsigned char *m , int T , const int *col , int len , int out )
{
   unsigned char *y , *in ; int t , q=0 ;
   if( m==NULL ) return NULL ;
   if( !out ){
     y=(unsigned char *)malloc((size_t)len) ;
     for( t=0 ; t<len ; t++ ) y[t]=m[col[t]] ;
     return y ;
   }
   in=(unsigned char *)calloc((size_t)T,1) ; y=(unsigned char *)malloc((size_t)(T-len>0?T-len:1)) ;
   for( t=0 ; t<len ; t++ ) in[col[t]]=1 ;
   for( t=0 ; t<T ; t++ ) if( !in[t] ) y[q++]=m[t] ;
   free(in) ;
   return y ;
}

/*! Gather training columns from a common plan, or the legacy own-fold complement. */
static float *srm_training_cols( const srm_folds *f , int ff , const float *x , int V , int T , const int *col , int len )
{
   return f->plan ? srm_cols_in(x,V,T,f->plan->train[ff],f->plan->ntrain[ff])
                  : srm_cols_out(x,V,T,col,len);
}

/*! Allocate r for nsub subjects x nfold folds: score[] zeroed; per-level scores,
    -cv_adapt anchor scores start as NaN (= not scored).  want_proj: room for each
    subject's K x T held-out projection.  want_vr2: voxelwise R^2, either summed
    here directly, or, for compressed subjects (whose rows are not voxels),
    deferred to srm_r2_note / srm_r2_make with an id telling this fit apart. */
static void srm_cvres_alloc( srm_cvres *r , int nsub , int nfold , int want_proj , int want_vr2 , const srm_ctx *cx )
{
   int ii ;
   memset(r,0,sizeof(*r)) ;
   r->nsub=nsub ; r->nfold=nfold ;
   r->score=(double *)calloc((size_t)nsub*nfold*NMET,sizeof(double)) ;
   r->L=cx->L ;
   if( cx->L>0 ){
     size_t q , nl=(size_t)nsub*nfold*cx->L*NMET ;
     r->lscore=(double *)malloc(sizeof(double)*nl) ;
     for( q=0 ; q<nl ; q++ ) r->lscore[q]=NAN ;
   }
   if( want_proj ){
     r->proj=(float **)calloc((size_t)nsub,sizeof(float *)) ;
     for( ii=0 ; ii<nsub ; ii++ ) r->proj[ii]=(float *)calloc((size_t)cx->K*cx->T,sizeof(float)) ;
   }
   if( cx->o->cv_adapt>0.0 ){
     size_t q , na=(size_t)nsub*nfold*NMET ;
     r->ascore=(double *)malloc(sizeof(double)*na) ;
     for( q=0 ; q<na ; q++ ) r->ascore[q]=NAN ;
   }
   if( want_vr2 && cx->sb[0].compressed ){            /* rows of R are not voxels: noted, made later */
     static int nid=0 ;
     r->r2defer=1 ; r->r2id=++nid ;
   } else if( want_vr2 ){
     r->vr2=(float **)calloc((size_t)nsub,sizeof(float *)) ;
     for( ii=0 ; ii<nsub ; ii++ ) r->vr2[ii]=(float *)calloc((size_t)cx->sb[ii].nvox,sizeof(float)) ;
   }
}

static void srm_cvres_free( srm_cvres *r )
{
   int ii ;
   if( r->proj!=NULL ){ for( ii=0 ; ii<r->nsub ; ii++ ) free(r->proj[ii]) ; free(r->proj) ; }
   if( r->vr2!=NULL ){ for( ii=0 ; ii<r->nsub ; ii++ ) free(r->vr2[ii]) ; free(r->vr2) ; }
   free(r->score) ; free(r->lscore) ; free(r->ascore) ;
}

/*============================================================================
  -compress -save_r2: a fold's voxelwise R^2 needs that fold's W on the voxels.  Each scored fold notes what
  it needs (its W and means in R, the reference it reconstructs from, the held-out columns); when the subject
  is read again, W = X M and the R^2 is made on the voxels exactly as SRM_score_view would have.

  The jobs live in a file-scope list (srm_r2jobs), appended by srm_r2_note during scoring and consumed by
  srm_r2_make while srm_write_outputs_reread re-reads each subject.  The math: a compressed subject is
  R = U'X with U = X R'(RR')^-1 orthonormal; a W_R fitted in R's space is W = U W_R on the voxels, which
  SRM_compress_map/expand compute as X M without forming U.  The list is not freed (process lifetime).
==============================================================================*/

typedef struct {
   int subj , rid , nfold , len , r , K ;
   int *col ; unsigned char *valid ;
   float *W , *mu , *ref ;
} srm_r2job ;

static srm_r2job *srm_r2jobs=NULL ; static int srm_nr2job=0 ;

/*! Note one scored fold for the deferred voxelwise R^2 (does nothing unless res->r2defer): copies of the
    fold's held-out columns col[0..len-1] (and validity mask), of view iview's W and means in m (in R's
    space), and of the reconstruction reference ref_r (K x len). */
static void srm_r2_note( const srm_ctx *cx , const srm_cvres *res , int ii , int nfold , const int *col , int len ,
                         const unsigned char *valid , const SRM_model *m , int iview , const float *ref_r )
{
   srm_r2job *j ; int r=m->nvox[iview] , K=m->nfeat ;
   if( res==NULL || !res->r2defer ) return ;
   srm_r2jobs=(srm_r2job *)realloc(srm_r2jobs,sizeof(srm_r2job)*(size_t)(srm_nr2job+1)) ;
   j=srm_r2jobs+srm_nr2job++ ; memset(j,0,sizeof(*j)) ;
   j->subj=(cx->sidx!=NULL) ? cx->sidx[ii] : ii ; j->rid=res->r2id ; j->nfold=nfold ; j->len=len ; j->r=r ; j->K=K ;
   j->col=(int *)malloc(sizeof(int)*(size_t)len) ; memcpy(j->col,col,sizeof(int)*(size_t)len) ;
   if( valid!=NULL ){ j->valid=(unsigned char *)malloc((size_t)len) ; memcpy(j->valid,valid,(size_t)len) ; }
   j->W=(float *)malloc(sizeof(float)*(size_t)r*K) ; memcpy(j->W,m->w[iview],sizeof(float)*(size_t)r*K) ;
   if( m->mu!=NULL && m->mu[iview]!=NULL ){ j->mu=(float *)malloc(sizeof(float)*(size_t)r) ; memcpy(j->mu,m->mu[iview],sizeof(float)*(size_t)r) ; }
   j->ref=(float *)malloc(sizeof(float)*(size_t)K*len) ; memcpy(j->ref,ref_r,sizeof(float)*(size_t)K*len) ;
}

/*! The mean voxelwise held-out R^2 of whole-table subject ss (V features, x V x T on the voxels, R its factor):
    over the folds of each fit, then over the fits that scored it, as the default accumulates it.  NULL if no
    fold of this subject was noted. */
static float * srm_r2_make( int ss , const float *x , int V , int T , const float *R , int rank )
{
   int q , nfit=0 , *rids=NULL ; float *out=NULL , *vr=(float *)malloc(sizeof(float)*(size_t)V) ; double *acc=NULL ;
   for( q=0 ; q<srm_nr2job ; q++ ){
     srm_r2job *j=srm_r2jobs+q ; int f , t , n=0 , *idx , rc , K=j->K , seen , z ;
     double *M , r2 ; float *Wv , *muv=NULL , *xg , *rg ; SRM_model *mv ;
     if( j->subj!=ss ) continue ;
     /* count distinct fits (rid) that scored this subject: each fit's folds are averaged (1/nfold), then the
        fits are averaged (1/nfit), as the uncompressed path accumulates them */
     for( seen=0,z=0 ; z<nfit ; z++ ) if( rids[z]==j->rid ) seen=1 ;
     if( !seen ){ rids=(int *)realloc(rids,sizeof(int)*(size_t)(nfit+1)) ; rids[nfit++]=j->rid ; }
     if( acc==NULL ) acc=(double *)calloc((size_t)V,sizeof(double)) ;
     idx=(int *)malloc(sizeof(int)*(size_t)j->len) ;
     for( t=0 ; t<j->len ; t++ ) if( j->valid==NULL || j->valid[t] ) idx[n++]=t ;
     if( n<3 && j->valid!=NULL ){ free(idx) ; continue ; }   /* as SRM_score_view: too few to score, 0 */
     M=(double *)malloc(sizeof(double)*(size_t)T*K) ; Wv=(float *)malloc(sizeof(float)*(size_t)V*K) ;
     /* the fold's W (and means) on the voxels: W = X M, M = R'(RR')^-1 W_R */
     rc=SRM_compress_map(R,rank,T,j->W,K,M) ;
     if( rc==SRM_OK ) rc=SRM_compress_expand(x,V,T,M,K,Wv) ;
     if( rc==SRM_OK && j->mu!=NULL ){
       muv=(float *)malloc(sizeof(float)*(size_t)V) ;
       rc=SRM_compress_map(R,rank,T,j->mu,1,M) ;
       if( rc==SRM_OK ) rc=SRM_compress_expand(x,V,T,M,1,muv) ;
     }
     if( rc!=SRM_OK ) ERROR_exit("-save_r2: a fold's W on the voxels: %s",SRM_strerror(rc)) ;
     xg=(float *)malloc(sizeof(float)*(size_t)V*n) ; rg=(float *)malloc(sizeof(float)*(size_t)K*n) ;
     for( f=0 ; f<V ; f++ ) for( t=0 ; t<n ; t++ ) xg[(size_t)f*n+t]=x[(size_t)f*T+j->col[idx[t]]] ;
     for( f=0 ; f<K ; f++ ) for( t=0 ; t<n ; t++ ) rg[(size_t)f*n+t]=j->ref[(size_t)f*j->len+idx[t]] ;
     mv=SRM_model_from_w(V,K,Wv,muv) ;
     rc=SRM_reconstruction_r2(mv,0,xg,rg,n,&r2,vr) ;
     if( rc!=SRM_OK ) for( f=0 ; f<V ; f++ ) vr[f]=0.0f ;
     for( f=0 ; f<V ; f++ ) acc[f]+=(double)vr[f]/j->nfold ;
     SRM_free_model(mv) ; free(M) ; free(Wv) ; free(muv) ; free(xg) ; free(rg) ; free(idx) ;
   }
   if( acc!=NULL ){
     int f ; out=(float *)malloc(sizeof(float)*(size_t)V) ;
     for( f=0 ; f<V ; f++ ) out[f]=(float)(acc[f]/nfit) ;
   }
   free(acc) ; free(rids) ; free(vr) ;
   return out ;
}

/*! The -score_by levels of subject ii in fold ff: each level's held-out columns (where valid), scored as
    SRM_score_view does.  Matching is among the level's own samples (windows inside it, or its items).
    The level is passed to SRM_score_view as a validity mask m[t] (valid AND label == level), so the
    whole-fold arrays are reused; windows that cross out of the level are skipped there. */
static void srm_score_levels( const srm_ctx *cx , srm_cvres *res , int ii , int ff , int nfold , const int *col ,
                              int len , const unsigned char *valid , const SRM_model *model , int iview ,
                              const float *xte , const float *P , const float *ref_u , const float *ref_r , srm_want w )
{
   int fa , l , t ; unsigned char *m ;
   if( cx->L==0 || res->lscore==NULL ) return ;
   m=(unsigned char *)malloc((size_t)len) ;
   for( fa=0 ; fa<cx->nfac ; fa++ ) for( l=0 ; l<cx->fac_nlev[fa] ; l++ ){
     double *sc=res->lscore+(((size_t)ii*nfold+ff)*cx->L+cx->fac_off[fa]+l)*NMET ;
     for( t=0 ; t<len ; t++ ) m[t]=(unsigned char)((valid==NULL || valid[t]) && cx->lab[fa][ii][col[t]]==l) ;
     if( SRM_score_view(model,iview,xte,P,ref_u,ref_r,cx->K,len,cx->o->match_window,w,m,sc,NULL)!=SRM_OK ){
       int q ; for( q=0 ; q<NMET ; q++ ) if( q!=M_NSAMP ) sc[q]=NAN ;
     }
   }
   free(m) ;
}

/*! ERROR_exit unless every fold has at least 3 columns and, for time-segment matching with window win,
    at least 2*win+1 (so a window has non-overlapping candidates to be confused with). */
static void srm_check_fold_lengths( const srm_folds *f , srm_want w , int win )
{
   int ii , minlen=1<<30 ;
   for( ii=0 ; ii<f->nfold ; ii++ ) if( f->len[ii]<minlen ) minlen=f->len[ii] ;
   if( minlen<3 ) ERROR_exit("the smallest fold (%s) has %d %s; need at least 3",f->name[0],minlen,f->unit) ;
   if( f->items ) return ;                       /* item identification has no window */
   if( w.match && minlen<2*win+1 )
     ERROR_exit("time-segment matching with a %d-TR window needs folds of at least %d time points; the smallest has %d "
                "(use a smaller -match_window, or drop 'match' from -cv_metrics)",win,2*win+1,minlen) ;
}

/*! -cv_adapt: of a held-out run's len samples, how many (the first) estimate the run's own W; the rest are scored. */
static int srm_adapt_n( double frac , int len ){ return (int)(frac*len) ; }

/*! Columns a .. a+n-1 of a V x T matrix. */
static float * srm_cols_range( const float *x , int V , int T , int a , int n )
{
   int *idx=(int *)malloc(sizeof(int)*(size_t)n) , t ; float *y ;
   for( t=0 ; t<n ; t++ ) idx[t]=a+t ;
   y=srm_cols_in(x,V,T,idx,n) ; free(idx) ;
   return y ;
}

/*! -cv_adapt: a held-out run's own W for one subject, from its first na samples xa (V x na; the means mu
    are removed here) against the shared response sa of those samples, tied to the anchor with the fit's
    lambda (SRM_fit_view_coupled).  Returned as a one-view model, for projecting and scoring the rest. */
static SRM_model * srm_adapt_view( const float *sa , int K , int na , const float *xa , int V , const float *anchor ,
                                   const float *mu , double lambda , const char *who , const unsigned char *oa )
{
   SRM_model *mv=NULL ;
   int rc=SRM_adapt_view(sa,K,na,xa,V,anchor,mu,lambda,oa,&mv) ;
   if( rc!=SRM_OK ) ERROR_exit("-cv_adapt: could not estimate the held-out run's W for Subj %s: %s",who,SRM_strerror(rc)) ;
   return mv ;
}

/*! -cv_adapt where the held-out run's S is known (-cv loso; new subjects against a fixed model): subject ii's
    run W from the run's first samples against ste there, tied to its anchor Wf; the rest scored through it
    (sc) and through the anchor mv (res->ascore). */
static int srm_adapt_known_s( const srm_ctx *cx , srm_cvres *res , int ii , int ff , int nfold , const int *col , int len ,
                              const float *ste , const float *xte , const float *P , const float *Wf , const float *mu ,
                              const SRM_model *mv , srm_want w , double *sc , float *vr )
{
   int K=cx->K , V=cx->sb[ii].nvox , na=srm_adapt_n(cx->o->cv_adapt,len) , nr=len-na , rc ;
   float *sa=srm_cols_range(ste,K,len,0,na) , *sr=srm_cols_range(ste,K,len,na,nr) , *xa=srm_cols_range(xte,V,len,0,na) ;
   float *xr=srm_cols_range(xte,V,len,na,nr) , *Pq=srm_cols_range(P,K,len,na,nr) , *Pr=(float *)malloc(sizeof(float)*(size_t)K*nr) ;
   unsigned char *ot=srm_mask_cols(cx->sb[ii].obs,cx->T,col,len,0) , *vr_=(ot!=NULL) ? ot+na : NULL ;   /* observed, or NULL */
   SRM_model *ma=srm_adapt_view(sa,K,na,xa,V,Wf,mu,cx->par.coupling,cx->sb[ii].label,ot) ;
   rc=SRM_project(ma,0,xr,nr,Pr) ;
   if( rc==SRM_OK ) rc=SRM_score_view(ma,0,xr,Pr,sr,sr,K,nr,cx->o->match_window,w,vr_,sc,vr) ;
   if( rc==SRM_OK ) srm_r2_note(cx,res,ii,nfold,col+na,nr,vr_,ma,0,sr) ;
   if( rc==SRM_OK ) srm_score_levels(cx,res,ii,ff,nfold,col+na,nr,vr_,ma,0,xr,Pr,sr,sr,w) ;
   if( rc==SRM_OK ) rc=SRM_score_view(mv,0,xr,Pq,sr,sr,K,nr,cx->o->match_window,w,vr_,res->ascore+((size_t)ii*nfold+ff)*NMET,NULL) ;
   SRM_free_model(ma) ; free(sa) ; free(sr) ; free(xa) ; free(xr) ; free(Pq) ; free(Pr) ; free(ot) ;
   return rc ;
}

/*! -cv_adapt under -cv run.  Run ff (columns col, len of them) was held out of the fit mf; xte and P are
    every subject's data there and its anchor projection (K x len).  The run's first na samples have no S
    from the fit, so theirs is the mean of every subject's anchor projection of them; each subject's run W
    is estimated against it, and the remaining samples are scored twice, against everyone else's
    projections made the same way: through the run Ws (res->score) and through the anchors (res->ascore). */
static void srm_adapt_run_scores( const srm_ctx *cx , const SRM_model *mf , int ff , int nfold , const int *col , int len ,
                                  float **xte , float **P , srm_want w , srm_cvres *res )
{
   int nsub=cx->nsub , K=cx->K , na=srm_adapt_n(cx->o->cv_adapt,len) , nr=len-na , ii , jj , q , rc , pass ;
   int *all=(int *)malloc(sizeof(int)*(size_t)nsub) , *oth=(int *)malloc(sizeof(int)*(size_t)nsub) ;
   float **Pa=(float **)malloc(sizeof(float *)*(size_t)nsub) , **Pq=(float **)malloc(sizeof(float *)*(size_t)nsub) ;
   float **Pr=(float **)malloc(sizeof(float *)*(size_t)nsub) , **xr=(float **)malloc(sizeof(float *)*(size_t)nsub) ;
   float **po=(float **)malloc(sizeof(float *)*(size_t)nsub) , *sa=(float *)malloc(sizeof(float)*(size_t)K*na) ;
   float *ref_u=(float *)malloc(sizeof(float)*(size_t)K*nr) , *ref_r=(float *)malloc(sizeof(float)*(size_t)K*nr) ;
   SRM_model **mv=(SRM_model **)malloc(sizeof(SRM_model *)*(size_t)nsub) ;
   unsigned char **oa=(unsigned char **)calloc((size_t)nsub,sizeof(unsigned char *)) ;     /* -cenmode missing: observed, */
   unsigned char **orr=(unsigned char **)calloc((size_t)nsub,sizeof(unsigned char *)) ;    /* first samples and the rest  */
   const unsigned char **oo=(const unsigned char **)calloc((size_t)nsub,sizeof(unsigned char *)) ;
   unsigned char *valid=NULL ;
   for( jj=0 ; jj<nsub ; jj++ ){
     int V=mf->nvox[jj] ;
     Pa[jj]=srm_cols_range(P[jj],K,len,0,na) ; Pq[jj]=srm_cols_range(P[jj],K,len,na,nr) ;
     xr[jj]=srm_cols_range(xte[jj],V,len,na,nr) ; all[jj]=jj ;
     if( cx->sb[jj].obs!=NULL ){
       /* -cenmode missing: this subject's observed flags over the fold, split into the first na and the rest */
       unsigned char *ot=srm_mask_cols(cx->sb[jj].obs,cx->T,col,len,0) ;
       oa[jj]=(unsigned char *)malloc((size_t)(na>0?na:1)) ; orr[jj]=(unsigned char *)malloc((size_t)nr) ;
       memcpy(oa[jj],ot,(size_t)na) ; memcpy(orr[jj],ot+na,(size_t)nr) ; free(ot) ;
     }
   }
   /* the run's first na samples were not in the fit, so they have no S: use the plain mean of every subject's
      anchor projection there (unweighted, weighted=0) as the target the run-specific W's are estimated against */
   rc=SRM_shared_from_projections_obs(mf,nsub,all,Pa,cx->anymiss ? (const unsigned char * const *)oa : NULL,na,0,sa) ;
   if( rc!=SRM_OK ) ERROR_exit("-cv_adapt: the shared response of run %d's first samples: %s",ff+1,SRM_strerror(rc)) ;
   if( cx->anymiss ) valid=(unsigned char *)malloc((size_t)nr) ;
   for( jj=0 ; jj<nsub ; jj++ ){
     int V=mf->nvox[jj] ; float *xa=srm_cols_range(xte[jj],V,len,0,na) ;
     mv[jj]=srm_adapt_view(sa,K,na,xa,V,mf->w[jj],mf->mu!=NULL ? mf->mu[jj] : NULL,cx->par.coupling,cx->sb[jj].label,oa[jj]) ;
     free(xa) ;
     Pr[jj]=(float *)malloc(sizeof(float)*(size_t)K*nr) ;
     rc=SRM_project(mv[jj],0,xr[jj],nr,Pr[jj]) ;
     if( rc!=SRM_OK ) ERROR_exit("projection failed for Subj %s: %s",cx->sb[jj].label,SRM_strerror(rc)) ;
   }
   for( ii=0 ; ii<nsub ; ii++ ){
     int no=0 ; float *vr=(res->vr2!=NULL) ? (float *)malloc(sizeof(float)*(size_t)mf->nvox[ii]) : NULL ;
     for( jj=0 ; jj<nsub ; jj++ ) if( jj!=ii ){ oo[no]=orr[jj] ; oth[no++]=jj ; }
     if( valid!=NULL ){                               /* scored where the subject was observed and someone else was */
       int t ;
       for( t=0 ; t<nr ; t++ ){
         int seen=0 ;
         for( q=0 ; q<no && !seen ; q++ ) seen=(oo[q]==NULL || oo[q][t]) ;
         valid[t]=(byte)(seen && (orr[ii]==NULL || orr[ii][t])) ;
       }
     }
     for( pass=0 ; pass<2 ; pass++ ){                 /* 0: the run's own Ws; 1: the anchors, on the same samples */
       float **src = (pass==0) ? Pr : Pq ; const unsigned char * const *ooc=(valid!=NULL) ? (const unsigned char * const *)oo : NULL ;
       double *sc=((pass==0) ? res->score : res->ascore)+((size_t)ii*nfold+ff)*NMET ;
       for( q=0 ; q<no ; q++ ) po[q]=src[oth[q]] ;
       rc=SRM_shared_from_projections_obs(mf,no,oth,po,ooc,nr,0,ref_u) ;
       if( rc==SRM_OK ) rc=SRM_shared_from_projections_obs(mf,no,oth,po,ooc,nr,1,ref_r) ;
       if( rc==SRM_OK ) rc = (pass==0) ? SRM_score_view(mv[ii],0,xr[ii],Pr[ii],ref_u,ref_r,K,nr,cx->o->match_window,w,valid,sc,vr)
                                       : SRM_score_view(mf,ii,xr[ii],Pq[ii],ref_u,ref_r,K,nr,cx->o->match_window,w,valid,sc,NULL) ;
       if( rc==SRM_OK && pass==0 ) srm_score_levels(cx,res,ii,ff,nfold,col+na,nr,valid,mv[ii],0,xr[ii],Pr[ii],ref_u,ref_r,w) ;
       if( rc==SRM_OK && pass==0 ) srm_r2_note(cx,res,ii,nfold,col+na,nr,valid,mv[ii],0,ref_r) ;
       if( rc!=SRM_OK ) ERROR_exit("scoring Subj %s on run %d failed: %s",cx->sb[ii].label,ff+1,SRM_strerror(rc)) ;
     }
     if( vr!=NULL ){ for( q=0 ; q<mf->nvox[ii] ; q++ ) res->vr2[ii][q]+=vr[q]/(float)nfold ; free(vr) ; }
   }
   for( jj=0 ; jj<nsub ; jj++ ){ free(Pa[jj]) ; free(Pq[jj]) ; free(Pr[jj]) ; free(xr[jj]) ; SRM_free_model(mv[jj]) ; free(oa[jj]) ; free(orr[jj]) ; }
   free(Pa) ; free(Pq) ; free(Pr) ; free(xr) ; free(po) ; free(sa) ; free(ref_u) ; free(ref_r) ; free(mv) ; free(all) ; free(oth) ;
   free(oa) ; free(orr) ; free(oo) ; free(valid) ;
}

/*! -cv run: leave one run (or block) out, refitting the group each time.

    For each fold ff:
      1. gather every subject's training columns (xtr) and held-out columns (xte);
      2. fit a fresh model mf to the training columns only (random start, same
         per-subject streams as the full fit; nothing from the full fit is used);
      3. project every subject's held-out data, P[ii] = W_ii' (xte - mu);
      4. score subject ii against the leave-one-out reference of the others' P;
      5. keep P (and the fold's S) to assemble the held-out projection output.
    Every subject contributed to mf (through their training runs), which is why
    the help calls -cv run less clean than loso for between-subject inference;
    but no held-out column touches any fit. */
static void srm_cv_by_runs( const srm_ctx *cx , const SRM_model *mfull , const srm_folds *f , srm_want w ,
                            srm_cvres *res )
{
   int nsub=cx->nsub , K=cx->K , T=cx->T , ff , ii , jj ;
   const srm_opts *o=cx->o ;
   float **S_tr=(float **)calloc((size_t)f->nfold,sizeof(float *)) ;      /* each fold's training S, for the gauge */
   float ***Pf=(float ***)calloc((size_t)f->nfold,sizeof(float **)) ;     /* [fold][subject] projections */
   int *nvox=(int *)malloc(sizeof(int)*(size_t)nsub) ;

   /* each subject's training and test columns live in one pair of buffers, sized for the largest fold and
      reused: a new pair per fold, of a different size each time, is not given back to the system promptly
      (macOS), and the peak then climbs fold by fold by most of a copy of the data */
   int maxlen=0 , maxtr=0 ;
   float **xtr=(float **)malloc(sizeof(float *)*(size_t)nsub) , **xte=(float **)malloc(sizeof(float *)*(size_t)nsub) ;
   for( ff=0 ; ff<f->nfold ; ff++ ){ if( f->len[ff]>maxlen ) maxlen=f->len[ff] ; if( T-f->len[ff]>maxtr ) maxtr=T-f->len[ff] ; }
   for( ii=0 ; ii<nsub ; ii++ ){
     nvox[ii]=cx->sb[ii].nvox ;
     xtr[ii]=(float *)malloc(sizeof(float)*(size_t)nvox[ii]*(size_t)maxtr) ;
     xte[ii]=(float *)malloc(sizeof(float)*(size_t)nvox[ii]*(size_t)maxlen) ;
     if( xtr[ii]==NULL || xte[ii]==NULL )
       ERROR_exit("out of memory for the -cv folds (%.2f GiB per copy of the data)",4.0*nvox[ii]*(double)(maxtr+maxlen)*nsub/GIB) ;
   }
   for( ff=0 ; ff<f->nfold ; ff++ ){
     int len=f->len[ff] , *col=f->col[ff] , Ttr=T-len ;
     SRM_params par=cx->par ; SRM_model *mf=NULL ; SRM_fitinfo fi ; srm_prog bar ; fit_prog fp ;
     /* segtr: the remaining runs' lengths for -run_coupling.  Fixed size: with more than 257 runs (256 left
        after holding one out) it would overflow; no check guards this */
     int segtr[256] , rc , *all=(int *)malloc(sizeof(int)*(size_t)nsub) ;
     float **P=(float **)malloc(sizeof(float *)*(size_t)nsub) ;
     float *ref_u=(float *)malloc(sizeof(float)*(size_t)K*len) , *ref_r=(float *)malloc(sizeof(float)*(size_t)K*len) ;
     char tag[96] ;

     unsigned char **otr=NULL , **ote=NULL ;
     for( ii=0 ; ii<nsub ; ii++ ){
       srm_cols_in_into(cx->sb[ii].x,nvox[ii],T,f->plan->train[ff],Ttr,xtr[ii]) ;
       srm_cols_in_into(cx->sb[ii].x,nvox[ii],T,col,len,xte[ii]) ;
       all[ii]=ii ;
     }
     if( cx->anymiss ){                               /* each subject's observed samples, in and out of the fold */
       /* otr: observed flags on the training columns, for the fit (SRM_params.obs); ote: on the held-out
          columns, for scoring.  A NULL entry means the subject was observed everywhere. */
       otr=(unsigned char **)calloc((size_t)nsub,sizeof(unsigned char *)) ; ote=(unsigned char **)calloc((size_t)nsub,sizeof(unsigned char *)) ;
       for( ii=0 ; ii<nsub ; ii++ ){ otr[ii]=srm_mask_cols(cx->sb[ii].obs,T,f->plan->train[ff],Ttr,0) ; ote[ii]=srm_mask_cols(cx->sb[ii].obs,T,col,len,0) ; }
       par.obs=(const unsigned char * const *)otr ;
     }
     if( par.coupling!=SRM_COUPLING_SHARED ){        /* the runs that remain, with their lengths */
       int q=0 ;
       if( f->by_blocks ) ERROR_exit("-run_coupling needs runs; with one run -cv uses time blocks") ;
       for( jj=0 ; jj<cx->nrun ; jj++ ) if( jj!=ff ) segtr[q++]=cx->seglen[jj] ;
       if( q<2 ) ERROR_exit("-run_coupling with -cv run needs at least 3 runs (2 remain when one is held out)") ;
       par.nseg=q ; par.seglen=segtr ;
     }
     snprintf(tag,sizeof(tag),"%s %d/%d",f->items?"fold":f->by_blocks?"block":"run",ff+1,f->nfold) ;
     srm_prog_begin(&bar,cx->progress,cx->quiet,"fold","iterations",par.niter) ;
     fp.bar=&bar ; fp.istart=0 ; fp.nstart=o->nstart ; fp.first_obj=0.0 ; fp.tag=tag ;
     par.progress=srm_fit_progress ; par.progress_ctx=&fp ;
     { srm_cmp cm ; srm_cmp_set(cx->sb,cx->sidx,NULL,nsub,K,&par,&cm) ;
       rc=SRM_fit_multistart(nsub,Ttr,nvox,xtr,&par,o->nstart,&mf,&fi,NULL,NULL) ;
       srm_cmp_free(&cm) ; }
     srm_prog_end(&bar) ;
     if( rc!=SRM_OK ) ERROR_exit("the fit for %s failed: %s",tag,SRM_strerror(rc)) ;
     if( !cx->quiet ) ININFO_message("%s (%s): %s after %d iterations",tag,f->name[ff],fi.converged?"converged":"iteration limit",fi.niter_done) ;
     rc=SRM_canonicalize(mf) ;
     if( rc!=SRM_OK ) ERROR_exit("could not canonicalize the fit for %s: %s",tag,SRM_strerror(rc)) ;
     /* Score in the fold model's own canonical basis (ISC is component-wise). */

     for( ii=0 ; ii<nsub ; ii++ ){                    /* each subject's held-out data in the shared space, once */
       P[ii]=(float *)malloc(sizeof(float)*(size_t)K*len) ;
       rc=SRM_project(mf,ii,xte[ii],len,P[ii]) ;
       if( rc!=SRM_OK ) ERROR_exit("projection failed for Subj %s: %s",cx->sb[ii].label,SRM_strerror(rc)) ;
     }
     if( o->cv_adapt>0.0 ) srm_adapt_run_scores(cx,mf,ff,f->nfold,col,len,xte,P,w,res) ;
     else
     for( ii=0 ; ii<nsub ; ii++ ){                    /* the reference for subject ii: everyone else */
       int *oth=(int *)malloc(sizeof(int)*(size_t)nsub) , no=0 ; float **po=(float **)malloc(sizeof(float *)*(size_t)nsub) ;
       double *sc=res->score+((size_t)ii*f->nfold+ff)*NMET ; float *vr=(res->vr2!=NULL) ? (float *)malloc(sizeof(float)*(size_t)nvox[ii]) : NULL ;
       const unsigned char **oo=NULL ; unsigned char *valid=NULL ;
       for( jj=0 ; jj<nsub ; jj++ ) if( jj!=ii ){ oth[no]=jj ; po[no]=P[jj] ; no++ ; }
       if( cx->anymiss ){                             /* scored where the subject was observed and someone else was */
         int t , q ;
         oo=(const unsigned char **)malloc(sizeof(unsigned char *)*(size_t)nsub) ;
         for( q=0 ; q<no ; q++ ) oo[q]=ote[oth[q]] ;
         valid=(unsigned char *)malloc((size_t)len) ;
         for( t=0 ; t<len ; t++ ){
           int seen=0 ;
           for( q=0 ; q<no && !seen ; q++ ) seen=(oo[q]==NULL || oo[q][t]) ;
           valid[t]=(byte)(seen && (ote[ii]==NULL || ote[ii][t])) ;
         }
       }
       rc=SRM_shared_from_projections_obs(mf,no,oth,po,(const unsigned char * const *)oo,len,0,ref_u) ;
       if( rc==SRM_OK ) rc=SRM_shared_from_projections_obs(mf,no,oth,po,(const unsigned char * const *)oo,len,1,ref_r) ;
       if( rc==SRM_OK ) rc=SRM_score_view(mf,ii,xte[ii],P[ii],ref_u,ref_r,K,len,o->match_window,w,valid,sc,vr) ;
       if( rc==SRM_OK ) srm_r2_note(cx,res,ii,f->nfold,col,len,valid,mf,ii,ref_r) ;
       if( rc==SRM_OK ) srm_score_levels(cx,res,ii,ff,f->nfold,col,len,valid,mf,ii,xte[ii],P[ii],ref_u,ref_r,w) ;
       free(oo) ; free(valid) ;
       if( rc!=SRM_OK ) ERROR_exit("scoring Subj %s on %s failed: %s",cx->sb[ii].label,tag,SRM_strerror(rc)) ;
       if( vr!=NULL ){ int q ; for( q=0 ; q<nvox[ii] ; q++ ) res->vr2[ii][q]+=vr[q]/(float)f->nfold ; free(vr) ; }
       free(oth) ; free(po) ;
     }
     if( res->proj!=NULL ){
       Pf[ff]=P ; S_tr[ff]=(float *)malloc(sizeof(float)*(size_t)K*Ttr) ; memcpy(S_tr[ff],mf->s,sizeof(float)*(size_t)K*Ttr) ;
     } else { for( ii=0 ; ii<nsub ; ii++ ) free(P[ii]) ; free(P) ; }
     if( otr!=NULL ){ for( ii=0 ; ii<nsub ; ii++ ){ free(otr[ii]) ; free(ote[ii]) ; } free(otr) ; free(ote) ; }
     free(all) ; free(ref_u) ; free(ref_r) ;
     SRM_free_fitinfo(&fi) ; SRM_free_model(mf) ;
   }

   /* held-out projections in one gauge: each fold's S aligned to the full fit's, same rotation for all subjects.
      This changes labels of components, never the scores (they were computed before this rotation).
      Each fold model has its own arbitrary rotation of the shared space.  Rm is the orthogonal K x K matrix that
      best maps the fold's training S onto the full fit's S on the same columns (Procrustes); applying it to the
      fold's projections puts every fold's stretch of the output series in the full fit's coordinates. */
   if( res->proj!=NULL ){
     for( ff=0 ; ff<f->nfold ; ff++ ){
       int len=f->len[ff] , *col=f->col[ff] , Ttr=T-len ; float R[1]={0} ;
       float *Sfull_tr=srm_cols_in(mfull->s,K,T,f->plan->train[ff],Ttr) ; float *Rm=(float *)malloc(sizeof(float)*(size_t)K*K) ;
       int rc=SRM_procrustes_rotation(S_tr[ff],Sfull_tr,K,Ttr,Rm) ;
       (void)R ;
       if( rc!=SRM_OK ) ERROR_exit("could not align the gauge of %s: %s",f->name[ff],SRM_strerror(rc)) ;
       for( ii=0 ; ii<nsub ; ii++ ){
         int c , d , t ;
         for( c=0 ; c<K ; c++ ) for( t=0 ; t<len ; t++ ){
           double v=0.0 ; for( d=0 ; d<K ; d++ ) v+=(double)Rm[c*K+d]*Pf[ff][ii][d*len+t] ;
           res->proj[ii][(size_t)c*T+col[t]]=(float)v ;
         }
         free(Pf[ff][ii]) ;
       }
       free(Pf[ff]) ; free(S_tr[ff]) ; free(Rm) ; free(Sfull_tr) ;
     }
   }
   for( ii=0 ; ii<nsub ; ii++ ){ free(xtr[ii]) ; free(xte[ii]) ; }
   free(xtr) ; free(xte) ;
   free(S_tr) ; free(Pf) ; free(nvox) ;
}

/*! -cv run with each subject's own folds (randomized items): every refit sees every item, but a subject's
    own held-out items are missing for that subject (SRM_params.obs).  The subject is then scored on those
    items against everyone else's projections of the same items.
    (Used when randomized run order makes each subject's folds differ, so no common set of columns can be
    removed for everyone.  The fold's held-out items enter the fit through the OTHER subjects, which is fine:
    the scored subject's own held-out data never do.) */
static void srm_cv_by_runs_own( const srm_ctx *cx , const SRM_model *mfull , const srm_folds *f , srm_want w ,
                                srm_cvres *res )
{
   int nsub=cx->nsub , K=cx->K , T=cx->T , ff , ii , jj ;
   const srm_opts *o=cx->o ;
   int *nvox=(int *)malloc(sizeof(int)*(size_t)nsub) ; float **xv=(float **)malloc(sizeof(float *)*(size_t)nsub) ;
   unsigned char **ob=(unsigned char **)malloc(sizeof(unsigned char *)*(size_t)nsub) ;
   for( ii=0 ; ii<nsub ; ii++ ){ nvox[ii]=cx->sb[ii].nvox ; xv[ii]=cx->sb[ii].x ; ob[ii]=(unsigned char *)malloc((size_t)T) ; }
   for( ff=0 ; ff<f->nfold ; ff++ ){
     SRM_params par=cx->par ; SRM_model *mf=NULL ; SRM_fitinfo fi ; srm_prog bar ; fit_prog fp ; char tag[96] ;
     float *Rm=(float *)malloc(sizeof(float)*(size_t)K*K) ; int rc , t ;
     for( ii=0 ; ii<nsub ; ii++ ){
       memset(ob[ii],1,(size_t)T) ;
       for( t=0 ; t<f->slen[ii][ff] ; t++ ) ob[ii][f->scol[ii][ff][t]]=0 ;
     }
     par.obs=(const unsigned char * const *)ob ;
     snprintf(tag,sizeof(tag),"fold %d/%d",ff+1,f->nfold) ;
     srm_prog_begin(&bar,cx->progress,cx->quiet,"fold","iterations",par.niter) ;
     fp.bar=&bar ; fp.istart=0 ; fp.nstart=o->nstart ; fp.first_obj=0.0 ; fp.tag=tag ;
     par.progress=srm_fit_progress ; par.progress_ctx=&fp ;
     { srm_cmp cm ; srm_cmp_set(cx->sb,cx->sidx,NULL,nsub,K,&par,&cm) ;
       rc=SRM_fit_multistart(nsub,T,nvox,xv,&par,o->nstart,&mf,&fi,NULL,NULL) ;
       srm_cmp_free(&cm) ; }
     srm_prog_end(&bar) ;
     if( rc!=SRM_OK ) ERROR_exit("the fit for %s failed: %s",tag,SRM_strerror(rc)) ;
     if( !cx->quiet ) ININFO_message("%s (%s, each subject's own items missing for them): %s after %d iterations",tag,f->name[ff],
                                     fi.converged?"converged":"iteration limit",fi.niter_done) ;
     rc=SRM_canonicalize(mf) ;
     if( rc==SRM_OK ) rc=SRM_procrustes_rotation(mf->s,mfull->s,K,T,Rm) ;
     if( rc!=SRM_OK ) ERROR_exit("could not put the fit for %s in the full fit's gauge: %s",tag,SRM_strerror(rc)) ;
     for( ii=0 ; ii<nsub ; ii++ ){                     /* subject ii on its own held-out items */
       int len=f->slen[ii][ff] , *col=f->scol[ii][ff] , no=0 , c , d ;
       float **P=(float **)malloc(sizeof(float *)*(size_t)nsub) ; int *oth=(int *)malloc(sizeof(int)*(size_t)nsub) ;
       float *ref_u=(float *)malloc(sizeof(float)*(size_t)K*len) , *ref_r=(float *)malloc(sizeof(float)*(size_t)K*len) ;
       float *xte=srm_cols_in(cx->sb[ii].x,nvox[ii],T,col,len) ;
       double *sc=res->score+((size_t)ii*f->nfold+ff)*NMET ; float *vr=(res->vr2!=NULL) ? (float *)malloc(sizeof(float)*(size_t)nvox[ii]) : NULL ;
       for( jj=0 ; jj<nsub ; jj++ ){
         float *xj=srm_cols_in(cx->sb[jj].x,nvox[jj],T,col,len) ;
         P[jj]=(float *)malloc(sizeof(float)*(size_t)K*len) ;
         rc=SRM_project(mf,jj,xj,len,P[jj]) ; free(xj) ;
         if( rc!=SRM_OK ) ERROR_exit("projection failed for Subj %s: %s",cx->sb[jj].label,SRM_strerror(rc)) ;
       }
       for( jj=0 ; jj<nsub ; jj++ ) if( jj!=ii ) oth[no++]=jj ;
       { float **po=(float **)malloc(sizeof(float *)*(size_t)nsub) ; int q ;
         for( q=0 ; q<no ; q++ ) po[q]=P[oth[q]] ;
         rc=SRM_shared_from_projections(mf,no,oth,po,len,0,ref_u) ;
         if( rc==SRM_OK ) rc=SRM_shared_from_projections(mf,no,oth,po,len,1,ref_r) ;
         free(po) ; }
       if( rc==SRM_OK ) rc=SRM_score_view(mf,ii,xte,P[ii],ref_u,ref_r,K,len,o->match_window,w,NULL,sc,vr) ;
       if( rc==SRM_OK ) srm_r2_note(cx,res,ii,f->nfold,col,len,NULL,mf,ii,ref_r) ;
       if( rc==SRM_OK ) srm_score_levels(cx,res,ii,ff,f->nfold,col,len,NULL,mf,ii,xte,P[ii],ref_u,ref_r,w) ;
       if( rc!=SRM_OK ) ERROR_exit("scoring Subj %s on %s failed: %s",cx->sb[ii].label,tag,SRM_strerror(rc)) ;
       if( vr!=NULL ){ int q ; for( q=0 ; q<nvox[ii] ; q++ ) res->vr2[ii][q]+=vr[q]/(float)f->nfold ; free(vr) ; }
       if( res->proj!=NULL )                            /* in the full fit's gauge */
         for( c=0 ; c<K ; c++ ) for( t=0 ; t<len ; t++ ){
           double v=0.0 ; for( d=0 ; d<K ; d++ ) v+=(double)Rm[c*K+d]*P[ii][d*len+t] ;
           res->proj[ii][(size_t)c*T+col[t]]=(float)v ;
         }
       for( jj=0 ; jj<nsub ; jj++ ) free(P[jj]) ;
       free(P) ; free(oth) ; free(ref_u) ; free(ref_r) ; free(xte) ;
     }
     SRM_free_fitinfo(&fi) ; SRM_free_model(mf) ; free(Rm) ;
   }
   for( ii=0 ; ii<nsub ; ii++ ) free(ob[ii]) ;
   free(ob) ; free(nvox) ; free(xv) ;
}

/*! A subject's W against a fixed S (k x Ttr) from its training columns (x: V x Ttr, the data without the
    fold's columns col[0..len-1] of a T-column timeline), using only the samples it was observed at.
    The observed mask is cut to the training columns from the plan's train[] list when given (common folds),
    else as the complement of col[] (per-subject folds).  W = polar(X_tr S_tr'), the Procrustes solution. */
static int srm_fit_view_obs( const float *str , int K , int Ttr , const float *xtr , int V , int center ,
                             const unsigned char *obs , int T , const int *col , int len , float *Wf , float *mu ,
                             const int *train )
{
   unsigned char *ot=train ? srm_mask_cols(obs,T,train,Ttr,0) : srm_mask_cols(obs,T,col,len,1) ;
   int rc=SRM_fit_view_obs(str,K,Ttr,xtr,V,center,ot,Wf,NULL,mu) ;
   free(ot) ;
   return rc ;
}

/*! -cv loso: fit the other subjects from an independent random start, using the
    full iteration/tolerance/multistart settings.  Estimate subject ii's W from
    their training columns against that S, then score their held-out columns.
    The leave-out model is canonicalized independently before scoring.
    No fitted parameters from subject ii enter the default leave-out fit.
    mfull supplies only the gauge of written projections (after scoring).

    Explicit -loso_start legacy reproduces the old full-model warm start with
    -loso_iter steps.  The retained subjects' parameters still depend on ii,
    including their held-out data, so those scores can be optimistic. */
static void srm_cv_loso( const srm_ctx *cx , const SRM_model *mfull , const srm_folds *f , srm_want w ,
                         srm_cvres *res )
{
   int nsub=cx->nsub , K=cx->K , T=cx->T , ii , jj , ff ; srm_prog bar ;
   const srm_opts *o=cx->o ;
   if( o->loso_legacy )
     WARNING_message("-loso_start legacy uses a fit that included the held-out subject; scores can be optimistic") ;
   else if( !cx->quiet )
     INFO_message("loso: independent cold starts, up to %d iterations, %d start(s) per subject",cx->par.niter,o->nstart) ;
   srm_prog_begin(&bar,cx->progress,cx->quiet,"loso","subjects",nsub) ;
   for( ii=0 ; ii<nsub ; ii++ ){
     SRM_params par=cx->par ; SRM_model *mdrop=NULL , *ml=NULL ; SRM_fitinfo fi ;
     int *nv=(int *)malloc(sizeof(int)*(size_t)(nsub-1)) ; float **xl=(float **)malloc(sizeof(float *)*(size_t)(nsub-1)) ;
     int q=0 , rc ; float *Ralign=(float *)malloc(sizeof(float)*(size_t)K*K) ;
     const unsigned char **ol=(const unsigned char **)calloc((size_t)nsub,sizeof(unsigned char *)) ;
     for( jj=0 ; jj<nsub ; jj++ ) if( jj!=ii ){ nv[q]=cx->sb[jj].nvox ; xl[q]=cx->sb[jj].x ; ol[q]=cx->sb[jj].obs ; q++ ; }
     if( cx->anymiss ) par.obs=(const unsigned char * const *)ol ;
     par.progress=NULL ; par.progress_ctx=NULL ;
     par.init=SRM_INIT_RANDOM ; par.init_model=NULL ;
     if( o->loso_legacy ){
       mdrop=SRM_model_copy(mfull,ii) ;
       if( mdrop==NULL ) ERROR_exit("out of memory copying the model for leave-one-out") ;
       par.init=SRM_INIT_MODEL ; par.init_model=mdrop ; par.niter=o->loso_iter ;
     }
     if( par.coupling!=SRM_COUPLING_SHARED ){ par.nseg=cx->nrun ; par.seglen=cx->seglen ; }
     { srm_cmp cm ; int *oth=(int *)malloc(sizeof(int)*(size_t)nsub) , no=0 ;
       for( jj=0 ; jj<nsub ; jj++ ) if( jj!=ii ) oth[no++]=jj ;
       srm_cmp_set(cx->sb,cx->sidx,oth,no,K,&par,&cm) ;
       if( o->loso_legacy ) rc=SRM_fit(nsub-1,T,nv,xl,&par,&ml,&fi) ;
       else rc=SRM_fit_multistart(nsub-1,T,nv,xl,&par,o->nstart,&ml,&fi,NULL,NULL) ;
       srm_cmp_free(&cm) ; free(oth) ; }
     if( rc!=SRM_OK ) ERROR_exit("the leave-one-out fit without Subj %s failed: %s",cx->sb[ii].label,SRM_strerror(rc)) ;
     if( !o->loso_legacy && !fi.converged && par.tol>0.0 && !cx->quiet )
       WARNING_message("leave-one-out fit without Subj %s reached %d iterations without convergence",cx->sb[ii].label,fi.niter_done) ;
     SRM_free_fitinfo(&fi) ; SRM_free_model(mdrop) ;
     if( !o->loso_legacy ){
       /* ISC averages component-wise correlations, which depend on the basis.
          Use the leave-out model's own canonical basis, as in run CV; aligning
          to mfull before scoring would reintroduce the excluded subject. */
       rc=SRM_canonicalize(ml) ;
       if( rc!=SRM_OK ) ERROR_exit("could not canonicalize the leave-one-out fit for Subj %s: %s",cx->sb[ii].label,SRM_strerror(rc)) ;
     }
     rc=SRM_procrustes_rotation(ml->s,mfull->s,K,T,Ralign) ;   /* for the projection output's gauge only */
     if( rc!=SRM_OK ) ERROR_exit("could not align the gauge for Subj %s: %s",cx->sb[ii].label,SRM_strerror(rc)) ;

     for( ff=0 ; ff<f->nfold ; ff++ ){                 /* folds within this subject */
       int len=(f->slen!=NULL) ? f->slen[ii][ff] : f->len[ff] , *col=(f->scol!=NULL) ? f->scol[ii][ff] : f->col[ff] ;
       int Ttr=T-len , V=cx->sb[ii].nvox ;
       float *xtr=srm_training_cols(f,ff,cx->sb[ii].x,V,T,col,len) , *xte=srm_cols_in(cx->sb[ii].x,V,T,col,len) ;
       float *str=srm_training_cols(f,ff,ml->s,K,T,col,len) , *ste=srm_cols_in(ml->s,K,T,col,len) ;
       float *Wf=(float *)malloc(sizeof(float)*(size_t)V*K) , *P=(float *)malloc(sizeof(float)*(size_t)K*len) ;
       float *mu=cx->par.center ? (float *)malloc(sizeof(float)*(size_t)V) : NULL ; SRM_model *mv ;
       double *sc=res->score+((size_t)ii*f->nfold+ff)*NMET ; float *vr=(res->vr2!=NULL) ? (float *)malloc(sizeof(float)*(size_t)V) : NULL ;
       rc=srm_fit_view_obs(str,K,Ttr,xtr,V,cx->par.center,cx->sb[ii].obs,T,col,len,Wf,mu,f->plan ? f->plan->train[ff] : NULL) ;   /* W from the training columns only */
       if( rc!=SRM_OK ) ERROR_exit("Subj %s, %s: could not estimate W: %s",cx->sb[ii].label,f->name[ff],SRM_strerror(rc)) ;
       mv=SRM_model_from_w(V,K,Wf,mu) ;
       rc=SRM_project(mv,0,xte,len,P) ;
       if( o->cv_adapt>0.0 )                           /* the run's own W from its first samples; both scored on the rest */
         rc=srm_adapt_known_s(cx,res,ii,ff,f->nfold,col,len,ste,xte,P,Wf,mu,mv,w,sc,vr) ;
       else
       { unsigned char *valid=srm_mask_cols(cx->sb[ii].obs,T,col,len,0) ;
         if( rc==SRM_OK ) rc=SRM_score_view(mv,0,xte,P,ste,ste,K,len,o->match_window,w,valid,sc,vr) ;
         if( rc==SRM_OK ) srm_r2_note(cx,res,ii,f->nfold,col,len,valid,mv,0,ste) ;
         if( rc==SRM_OK ) srm_score_levels(cx,res,ii,ff,f->nfold,col,len,valid,mv,0,xte,P,ste,ste,w) ;
         free(valid) ; }
       if( rc!=SRM_OK ) ERROR_exit("Subj %s, %s: scoring failed: %s",cx->sb[ii].label,f->name[ff],SRM_strerror(rc)) ;
       if( vr!=NULL ){ int z ; for( z=0 ; z<V ; z++ ) res->vr2[ii][z]+=vr[z]/(float)f->nfold ; free(vr) ; }
       if( res->proj!=NULL ){
         int c , d , t ;
         for( c=0 ; c<K ; c++ ) for( t=0 ; t<len ; t++ ){
           double v=0.0 ; for( d=0 ; d<K ; d++ ) v+=(double)Ralign[c*K+d]*P[d*len+t] ;
           res->proj[ii][(size_t)c*T+col[t]]=(float)v ;
         }
       }
       SRM_free_model(mv) ; free(xtr) ; free(xte) ; free(str) ; free(ste) ; free(Wf) ; free(P) ; free(mu) ;
     }
     SRM_free_model(ml) ; free(nv) ; free(xl) ; free(Ralign) ; free(ol) ;
     { char extra[128] ; snprintf(extra,sizeof(extra),"left out Subj %s, %d folds scored",cx->sb[ii].label,f->nfold) ;
       srm_prog_set(&bar,ii+1,extra) ; }
   }
}

/*! -cv with -model_in: S is fixed.  Each subject's W is estimated from the training columns only
    and the subject is scored on the held-out columns against the model's own S there.  The model
    never saw the subject, and the held-out columns touch neither W nor S.  (Also used under -cv_fit
    reference for the non-reference groups, against the reference group's fit.)  No gauge rotation is
    needed: everything is already in the fixed model's coordinates. */
static void srm_cv_frozen( const srm_ctx *cx , const SRM_model *m , const srm_folds *f , srm_want w ,
                           srm_cvres *res )
{
   int nsub=cx->nsub , K=cx->K , T=cx->T , ii , ff ; srm_prog bar ;
   const srm_opts *o=cx->o ;
   srm_prog_begin(&bar,cx->progress,cx->quiet,"folds","subjects",nsub) ;
   for( ii=0 ; ii<nsub ; ii++ ){
     for( ff=0 ; ff<f->nfold ; ff++ ){
       int len=(f->scol!=NULL) ? f->slen[ii][ff] : f->len[ff] , *col=(f->scol!=NULL) ? f->scol[ii][ff] : f->col[ff] ;
       int Ttr=T-len , V=cx->sb[ii].nvox , rc ;           /* a subject's own folds (randomized items) are fine: S is fixed */
       float *xtr=srm_training_cols(f,ff,cx->sb[ii].x,V,T,col,len) , *xte=srm_cols_in(cx->sb[ii].x,V,T,col,len) ;
       float *str=srm_training_cols(f,ff,m->s,K,T,col,len) , *ste=srm_cols_in(m->s,K,T,col,len) ;
       float *Wf=(float *)malloc(sizeof(float)*(size_t)V*K) , *P=(float *)malloc(sizeof(float)*(size_t)K*len) ;
       float *mu=cx->par.center ? (float *)malloc(sizeof(float)*(size_t)V) : NULL ; SRM_model *mv ;
       double *sc=res->score+((size_t)ii*f->nfold+ff)*NMET ; float *vr=(res->vr2!=NULL) ? (float *)malloc(sizeof(float)*(size_t)V) : NULL ;
       rc=srm_fit_view_obs(str,K,Ttr,xtr,V,cx->par.center,cx->sb[ii].obs,T,col,len,Wf,mu,f->plan ? f->plan->train[ff] : NULL) ;
       if( rc!=SRM_OK ) ERROR_exit("Subj %s, %s: could not estimate W: %s",cx->sb[ii].label,f->name[ff],SRM_strerror(rc)) ;
       mv=SRM_model_from_w(V,K,Wf,mu) ;
       if( mv==NULL ) ERROR_exit("out of memory scoring Subj %s",cx->sb[ii].label) ;
       rc=SRM_project(mv,0,xte,len,P) ;
       if( rc==SRM_OK && o->cv_adapt>0.0 )               /* the run's own W from its first samples; both scored on the rest */
         rc=srm_adapt_known_s(cx,res,ii,ff,f->nfold,col,len,ste,xte,P,Wf,mu,mv,w,sc,vr) ;
       else
       { unsigned char *valid=srm_mask_cols(cx->sb[ii].obs,T,col,len,0) ;
         if( rc==SRM_OK ) rc=SRM_score_view(mv,0,xte,P,ste,ste,K,len,o->match_window,w,valid,sc,vr) ;
         if( rc==SRM_OK ) srm_r2_note(cx,res,ii,f->nfold,col,len,valid,mv,0,ste) ;
         if( rc==SRM_OK ) srm_score_levels(cx,res,ii,ff,f->nfold,col,len,valid,mv,0,xte,P,ste,ste,w) ;
         free(valid) ; }
       if( rc!=SRM_OK ) ERROR_exit("Subj %s, %s: scoring failed: %s",cx->sb[ii].label,f->name[ff],SRM_strerror(rc)) ;
       if( vr!=NULL ){ int z ; for( z=0 ; z<V ; z++ ) res->vr2[ii][z]+=vr[z]/(float)f->nfold ; free(vr) ; }
       if( res->proj!=NULL ){
         int c , t ;
         for( c=0 ; c<K ; c++ ) for( t=0 ; t<len ; t++ ) res->proj[ii][(size_t)c*T+col[t]]=P[(size_t)c*len+t] ;
       }
       SRM_free_model(mv) ; free(xtr) ; free(xte) ; free(str) ; free(ste) ; free(Wf) ; free(P) ; free(mu) ;
     }
     srm_prog_set(&bar,ii+1,NULL) ;
   }
}

/*============================================================================
  -cv_fit and -balance: the CV models fit to a group, or to subsets of the subjects.  The scheme inside
  (-cv run or loso) is the ordinary one, run on a context restricted to those subjects; their scores are
  added into the whole table and averaged over the fits that scored them.  Held-out projections are
  rotated into the gauge of the pooled fit, so every subject's are comparable.
==============================================================================*/

/*! The context of a subset of the subjects (sel[0..n-1], ascending).  Shallow: free with srm_ctx_subset_free. */
static srm_ctx srm_ctx_subset( const srm_ctx *cx , const int *sel , int n )
{
   srm_ctx c=*cx ; int j , fa ;
   c.nsub=n ; c.anymiss=0 ;
   c.sb=(srm_subj *)malloc(sizeof(srm_subj)*(size_t)n) ; c.sidx=(int *)malloc(sizeof(int)*(size_t)n) ;
   for( j=0 ; j<n ; j++ ){ c.sb[j]=cx->sb[sel[j]] ; c.sidx[j]=(cx->sidx!=NULL) ? cx->sidx[sel[j]] : sel[j] ; if( c.sb[j].obs!=NULL ) c.anymiss=1 ; }
   if( cx->nfac>0 ){
     c.lab=(int ***)malloc(sizeof(int **)*(size_t)cx->nfac) ;
     for( fa=0 ; fa<cx->nfac ; fa++ ){
       c.lab[fa]=(int **)malloc(sizeof(int *)*(size_t)n) ;
       for( j=0 ; j<n ; j++ ) c.lab[fa][j]=cx->lab[fa][sel[j]] ;
     }
   }
   return c ;
}

static void srm_ctx_subset_free( srm_ctx *c )
{
   int fa ;
   if( c->nfac>0 && c->lab!=NULL ){ for( fa=0 ; fa<c->nfac ; fa++ ) free(c->lab[fa]) ; free(c->lab) ; }
   free(c->sb) ; free(c->sidx) ;
}

/*! The folds of a subset: the same, but each subject's own folds (randomized items) follow it.  Shallow. */
static srm_folds srm_folds_subset( const srm_folds *f , const int *sel , int n )
{
   srm_folds g=*f ; int j ;
   if( f->scol!=NULL ){
     g.scol=(int ***)calloc((size_t)n+1,sizeof(int **)) ; g.slen=(int **)calloc((size_t)n+1,sizeof(int *)) ;
     for( j=0 ; j<n ; j++ ){ g.scol[j]=f->scol[sel[j]] ; g.slen[j]=f->slen[sel[j]] ; }
   }
   return g ;
}

static void srm_folds_subset_free( srm_folds *g , const srm_folds *f )
{
   if( f->scol!=NULL ){ free(g->scol) ; free(g->slen) ; }
}

/*! Fit a subset for its in-sample reference and projection gauge.  Default
    LOSO fits independently of this model; legacy LOSO warm-starts from it. */
static SRM_model * srm_fit_subset( const srm_ctx *c , const char *what )
{
   SRM_params par=c->par ; SRM_model *m=NULL ; SRM_fitinfo fi ; int j , rc ;
   int *nv=(int *)malloc(sizeof(int)*(size_t)c->nsub) ; float **xv=(float **)malloc(sizeof(float *)*(size_t)c->nsub) ;
   const unsigned char **ob=(const unsigned char **)malloc(sizeof(unsigned char *)*(size_t)c->nsub) ;
   for( j=0 ; j<c->nsub ; j++ ){ nv[j]=c->sb[j].nvox ; xv[j]=c->sb[j].x ; ob[j]=c->sb[j].obs ; }
   if( c->anymiss ) par.obs=(const unsigned char * const *)ob ;
   par.progress=NULL ; par.progress_ctx=NULL ;
   { srm_cmp cm ; srm_cmp_set(c->sb,c->sidx,NULL,c->nsub,c->K,&par,&cm) ;
     rc=SRM_fit_multistart(c->nsub,c->T,nv,xv,&par,c->o->nstart,&m,&fi,NULL,NULL) ;
     srm_cmp_free(&cm) ; }
   if( rc!=SRM_OK ) ERROR_exit("the fit of %s failed: %s",what,SRM_strerror(rc)) ;
   rc=SRM_canonicalize(m) ;
   if( rc!=SRM_OK ) ERROR_exit("could not canonicalize the fit of %s: %s",what,SRM_strerror(rc)) ;
   SRM_free_fitinfo(&fi) ; free(nv) ; free(xv) ; free(ob) ;
   return m ;
}

/*! The ordinary -cv scheme on a context. */
static void srm_cv_dispatch( const srm_ctx *c , const SRM_model *mfull , const srm_folds *f , srm_want w , srm_cvres *r )
{
   if( strcasecmp(c->o->cvmode,"loso")==0 ) srm_cv_loso(c,mfull,f,w,r) ;
   else if( f->scol!=NULL )                srm_cv_by_runs_own(c,mfull,f,w,r) ;
   else                                    srm_cv_by_runs(c,mfull,f,w,r) ;
}

/*! Running sums over the fits that score a subject.  With -balance B a subject can be scored by several
    subset fits (or by none): each score is summed with a count (ssum/scnt, per subject x fold x metric; lsum/
    lcnt per level; asum/acnt for the -cv_adapt anchor), and srm_cvacc_finish divides.  pdone: the subject's
    held-out projection has been taken (from the first fit that scored it). */
typedef struct { double *ssum , *scnt , *lsum , *lcnt , *vcnt , *asum , *acnt ; int *pdone ; } srm_cvacc ;

/*! Zero-initialized sums sized like r (see srm_cvacc). */
static void srm_cvacc_init( srm_cvacc *a , const srm_cvres *r )
{
   size_t ns=(size_t)r->nsub*r->nfold*NMET , nl=(size_t)r->nsub*r->nfold*(r->L>0?r->L:1)*NMET ;
   a->ssum=(double *)calloc(ns,sizeof(double)) ; a->scnt=(double *)calloc(ns,sizeof(double)) ;
   a->lsum=(double *)calloc(nl,sizeof(double)) ; a->lcnt=(double *)calloc(nl,sizeof(double)) ;
   a->vcnt=(double *)calloc((size_t)r->nsub,sizeof(double)) ; a->pdone=(int *)calloc((size_t)r->nsub,sizeof(int)) ;
   a->asum=(double *)calloc(ns,sizeof(double)) ; a->acnt=(double *)calloc(ns,sizeof(double)) ;   /* -cv_adapt: the anchor's */
}

/*! Add a subset's results (its subject j is whole-table subject sel[j], with c->sb[j].nvox features).  Rg
    (K x K, or NULL) rotates its projections into the pooled gauge; a subject keeps the projections of the
    first fit that scores it.  nsamp is copied, not summed (it is the same in every fit). */
static void srm_cvacc_add( srm_cvacc *a , srm_cvres *r , const srm_cvres *s , const srm_ctx *c , const int *sel ,
                           const float *Rg , int K , int T )
{
   int j , ff , m , l ;
   for( j=0 ; j<s->nsub ; j++ ){
     int i=sel[j] ;
     for( ff=0 ; ff<s->nfold ; ff++ ){
       for( m=0 ; m<NMET ; m++ ){
         double v=s->score[((size_t)j*s->nfold+ff)*NMET+m] ; size_t q=((size_t)i*r->nfold+ff)*NMET+m ;
         if( m==M_NSAMP ){ r->score[q]=v ; if( r->ascore!=NULL ) r->ascore[q]=v ; continue ; }
         if( isfinite(v) ){ a->ssum[q]+=v ; a->scnt[q]+=1.0 ; }
         if( r->ascore!=NULL && s->ascore!=NULL ){
           double u=s->ascore[((size_t)j*s->nfold+ff)*NMET+m] ;
           if( isfinite(u) ){ a->asum[q]+=u ; a->acnt[q]+=1.0 ; }
         }
       }
       for( l=0 ; l<s->L && s->lscore!=NULL ; l++ ) for( m=0 ; m<NMET ; m++ ){
         double v=s->lscore[(((size_t)j*s->nfold+ff)*s->L+l)*NMET+m] ; size_t q=(((size_t)i*r->nfold+ff)*r->L+l)*NMET+m ;
         if( m==M_NSAMP ){ r->lscore[q]=v ; continue ; }
         if( isfinite(v) ){ a->lsum[q]+=v ; a->lcnt[q]+=1.0 ; }
       }
     }
     if( r->vr2!=NULL && s->vr2!=NULL ){                /* voxelwise R^2: summed here, divided at the end */
       int z ; for( z=0 ; z<c->sb[j].nvox ; z++ ) r->vr2[i][z]+=s->vr2[j][z] ;
       a->vcnt[i]+=1.0 ;
     }
     if( r->proj!=NULL && s->proj!=NULL && !a->pdone[i] ){
       int cc , d , t ;
       for( cc=0 ; cc<K ; cc++ ) for( t=0 ; t<T ; t++ ){
         double v=0.0 ;
         if( Rg!=NULL ) for( d=0 ; d<K ; d++ ) v+=(double)Rg[cc*K+d]*s->proj[j][(size_t)d*T+t] ;
         else v=s->proj[j][(size_t)cc*T+t] ;
         r->proj[i][(size_t)cc*T+t]=(float)v ;
       }
       a->pdone[i]=1 ;
     }
   }
}

/*! The means over fits; a subject no fit scored gets NA. */
static void srm_cvacc_finish( srm_cvacc *a , srm_cvres *r , const srm_ctx *cx )
{
   size_t q , ns=(size_t)r->nsub*r->nfold*NMET , nl=(size_t)r->nsub*r->nfold*(r->L>0?r->L:1)*NMET ; int i , z ;
   for( q=0 ; q<ns ; q++ ) if( (int)(q%NMET)!=M_NSAMP ) r->score[q] = (a->scnt[q]>0.0) ? a->ssum[q]/a->scnt[q] : NAN ;
   if( r->ascore!=NULL ) for( q=0 ; q<ns ; q++ ) if( (int)(q%NMET)!=M_NSAMP ) r->ascore[q] = (a->acnt[q]>0.0) ? a->asum[q]/a->acnt[q] : NAN ;
   if( r->lscore!=NULL ) for( q=0 ; q<nl ; q++ ) if( (int)(q%NMET)!=M_NSAMP ) r->lscore[q] = (a->lcnt[q]>0.0) ? a->lsum[q]/a->lcnt[q] : NAN ;
   if( r->vr2!=NULL ) for( i=0 ; i<r->nsub ; i++ ) if( a->vcnt[i]>0.0 ) for( z=0 ; z<cx->sb[i].nvox ; z++ ) r->vr2[i][z]/=(float)a->vcnt[i] ;
   free(a->ssum) ; free(a->scnt) ; free(a->lsum) ; free(a->lcnt) ; free(a->vcnt) ; free(a->pdone) ; free(a->asum) ; free(a->acnt) ;
}

/*! k of the n values a[] at random (seeded), ascending, into out.  Deterministic for a seed, so the
    -balance subsets are reproducible (and listed in PREFIX.balance.txt). */
static void srm_draw( const int *a , int n , int k , unsigned long long seed , int *out )
{
   int *p=(int *)malloc(sizeof(int)*(size_t)n) , i , j , t ; unsigned long long z=seed ;
   for( i=0 ; i<n ; i++ ) p[i]=a[i] ;
   for( i=0 ; i<k ; i++ ){                              /* partial Fisher-Yates with splitmix64 */
     unsigned long long y=(z+=0x9E3779B97F4A7C15ULL) ;
     y=(y^(y>>30))*0xBF58476D1CE4E5B9ULL ; y=(y^(y>>27))*0x94D049BB133111EBULL ; y^=(y>>31) ;
     j=i+(int)(y%(unsigned long long)(n-i)) ; t=p[i] ; p[i]=p[j] ; p[j]=t ;
   }
   for( i=0 ; i<k ; i++ ) out[i]=p[i] ;
   for( i=1 ; i<k ; i++ ){ int v=out[i] ; for( j=i ; j>0 && out[j-1]>v ; j-- ) out[j]=out[j-1] ; out[j]=v ; }
   free(p) ;
}

#define SRM_FIT_POOLED    0
#define SRM_FIT_WITHIN    1
#define SRM_FIT_REFERENCE 2

/*! One fit of -cv_fit/-balance: subjects sel[] (ascending) form the model; with others != NULL (reference),
    sel[] is the reference group, scored leave-one-out, and others[] are scored as new subjects.
    Steps: a subset context and folds (shallow views of cx/f); the subset's full fit m; Rg, the rotation from
    m's gauge to the pooled fit's (for projections only); the ordinary CV scheme inside the subset; its
    scores added into the whole-table accumulator acc.  Reference subjects use
    N-1 reference members; outside subjects use all N reference members. */
static void srm_cv_one_fit( const srm_ctx *cx , const SRM_model *mpool , const srm_folds *f , srm_want w ,
                            srm_cvres *r , srm_cvacc *acc , const int *sel , int n , const int *oth , int no ,
                            int quiet , const char *what )
{
   srm_ctx c=srm_ctx_subset(cx,sel,n) ; srm_folds g=srm_folds_subset(f,sel,n) ; srm_cvres s ;
   SRM_model *m ; float *Rg=(float *)malloc(sizeof(float)*(size_t)cx->K*cx->K) ; int rc ;
   c.quiet=quiet ;
   m=srm_fit_subset(&c,what) ;
   rc=SRM_procrustes_rotation(m->s,mpool->s,cx->K,cx->T,Rg) ;     /* this fit's gauge -> the pooled fit's */
   if( rc!=SRM_OK ) ERROR_exit("could not align the gauge of %s: %s",what,SRM_strerror(rc)) ;
   srm_cvres_alloc(&s,n,f->nfold,r->proj!=NULL,r->vr2!=NULL || r->r2defer,&c) ;
   if( oth!=NULL ) srm_cv_loso(&c,m,&g,w,&s) ; else srm_cv_dispatch(&c,m,&g,w,&s) ;
   srm_cvacc_add(acc,r,&s,&c,sel,Rg,cx->K,cx->T) ;
   srm_cvres_free(&s) ;
   if( oth!=NULL && no>0 ){                             /* everyone else: new subjects in this model */
     srm_ctx co=srm_ctx_subset(cx,oth,no) ; srm_folds go=srm_folds_subset(f,oth,no) ;
     co.quiet=quiet ;
     srm_cvres_alloc(&s,no,f->nfold,r->proj!=NULL,r->vr2!=NULL || r->r2defer,&co) ;
     srm_cv_frozen(&co,m,&go,w,&s) ;
     srm_cvacc_add(acc,r,&s,&co,oth,Rg,cx->K,cx->T) ;
     srm_cvres_free(&s) ; srm_folds_subset_free(&go,f) ; srm_ctx_subset_free(&co) ;
   }
   SRM_free_model(m) ; free(Rg) ; srm_folds_subset_free(&g,f) ; srm_ctx_subset_free(&c) ;
}

/*! -cv_fit within|reference:GROUP and -balance B.  B = 0: each group once.  B > 0: every group fit is repeated on
    B random subsets the size of the smallest group (pooled: B subsets with that many from every group), and a
    subject's scores are averaged over the fits that scored it.  The draws go to bfp (may be NULL). */
static void srm_cv_groups( const srm_ctx *cx , const SRM_model *mpool , const srm_folds *f , srm_want w ,
                           srm_cvres *r , int mode , int refg , int B , FILE *bfp )
{
   int ng=cx->ngroup , g , i , b , nmin=cx->nsub , nfit=0 , done=0 , *ngm , **gm , *sel , *oth ;
   srm_cvacc acc ; srm_prog bar ; char what[256] ;
   unsigned long long seed=(unsigned long long)cx->par.seed*0x2545F4914F6CDD1DULL+17ULL ;
   ngm=(int *)calloc((size_t)ng,sizeof(int)) ; gm=(int **)malloc(sizeof(int *)*(size_t)ng) ;
   for( g=0 ; g<ng ; g++ ){
     gm[g]=(int *)malloc(sizeof(int)*(size_t)cx->nsub) ;
     for( i=0 ; i<cx->nsub ; i++ ) if( cx->sb[i].group==g ) gm[g][ngm[g]++]=i ;
     if( ngm[g]<nmin ) nmin=ngm[g] ;
   }
   sel=(int *)malloc(sizeof(int)*(size_t)cx->nsub) ; oth=(int *)malloc(sizeof(int)*(size_t)cx->nsub) ;
   if( mode==SRM_FIT_POOLED ) nfit=B ;
   else if( mode==SRM_FIT_WITHIN ) for( g=0 ; g<ng ; g++ ) nfit += (B>0 && ngm[g]>nmin) ? B : 1 ;
   else nfit = (B>0 && ngm[refg]>nmin) ? B : 1 ;
   srm_cvacc_init(&acc,r) ;
   if( bfp!=NULL ) fprintf(bfp,"Fit\tGroup\tN\tSubjects\n") ;
   srm_prog_begin(&bar,cx->progress,cx->quiet,B>0?"balance":"groups","fits",nfit) ;
   for( g=0 ; g<(mode==SRM_FIT_POOLED ? 1 : ng) ; g++ ){
     int nrep , n ;
     if( mode==SRM_FIT_REFERENCE && g!=refg ) continue ;
     /* how many fits for this group: B random subsets of size nmin when balancing a group larger than the
        smallest; otherwise the whole group once.  Pooled: B subsets, each nmin from every group. */
     nrep = (mode==SRM_FIT_POOLED) ? B : (B>0 && ngm[g]>nmin) ? B : 1 ;
     for( b=0 ; b<nrep ; b++ ){
       int no=0 , q ;
       if( mode==SRM_FIT_POOLED ){                      /* nmin from every group */
         int h ; n=0 ;
         for( h=0 ; h<ng ; h++ ){ srm_draw(gm[h],ngm[h],nmin,seed+(unsigned long long)(b*1000+h),sel+n) ; n+=nmin ; }
         for( i=1 ; i<n ; i++ ){ int v=sel[i] , k ; for( k=i ; k>0 && sel[k-1]>v ; k-- ) sel[k]=sel[k-1] ; sel[k]=v ; }
       } else if( nrep>1 ){ srm_draw(gm[g],ngm[g],nmin,seed+(unsigned long long)(b*1000+g),sel) ; n=nmin ; }
       else { for( i=0 ; i<ngm[g] ; i++ ) sel[i]=gm[g][i] ; n=ngm[g] ; }
       if( mode==SRM_FIT_REFERENCE ) for( i=0 ; i<cx->nsub ; i++ ) if( cx->sb[i].group!=refg ) oth[no++]=i ;
       snprintf(what,sizeof(what),"%s %s%s%d subjects",mode==SRM_FIT_POOLED?"a balanced subset of":"group",
                mode==SRM_FIT_POOLED?"":cx->glabel[g],mode==SRM_FIT_POOLED?" ":", ",n) ;
       if( bfp!=NULL ){
         fprintf(bfp,"%d\t%s\t%d\t",done+1,mode==SRM_FIT_POOLED?"all":cx->glabel[g],n) ;
         for( q=0 ; q<n ; q++ ) fprintf(bfp,"%s%s",q?",":"",cx->sb[sel[q]].label) ;
         fputc('\n',bfp) ;
       }
       if( !cx->quiet && B==0 )
         INFO_message("-cv_fit %s: %s, a model of its %d subjects%s",mode==SRM_FIT_WITHIN?"within":"reference",
                      cx->glabel[g],n,mode==SRM_FIT_REFERENCE?"; the other groups scored as new subjects in it":"") ;
       srm_cv_one_fit(cx,mpool,f,w,r,&acc,sel,n,mode==SRM_FIT_REFERENCE?oth:NULL,no,cx->quiet || B>0,what) ;
       srm_prog_set(&bar,++done,what) ;
     }
   }
   srm_prog_end(&bar) ;
   { int nnot=0 ; char ls[256] ; int pos=0 ;                    /* subjects no subset contained */
     for( i=0 ; i<cx->nsub ; i++ ) if( !acc.pdone[i] && acc.vcnt[i]==0.0 ){
       int ff , any=0 ;
       for( ff=0 ; ff<r->nfold && !any ; ff++ ) any=(acc.scnt[((size_t)i*r->nfold+ff)*NMET+M_ISC]>0.0 ||
                                                     acc.scnt[((size_t)i*r->nfold+ff)*NMET+M_R2]>0.0 ||
                                                     acc.scnt[((size_t)i*r->nfold+ff)*NMET+M_MATCH]>0.0) ;
       if( !any ){ nnot++ ; if( pos<200 ) pos+=snprintf(ls+pos,sizeof(ls)-pos,"%s%s",pos?", ":"",cx->sb[i].label) ; }
     }
     if( nnot>0 )
       WARNING_message("%d subject%s %s in none of the -balance subsets (%s): %s scores are NA. Raise -balance",
                       nnot,nnot==1?"":"s",nnot==1?"was":"were",ls,nnot==1?"its":"their") ; }
   srm_cvacc_finish(&acc,r,cx) ;
   for( g=0 ; g<ng ; g++ ) free(gm[g]) ;
   free(gm) ; free(ngm) ; free(sel) ; free(oth) ;
}

/*============================================================================
  -reliability split: is there a stable shared response in each group?
==============================================================================*/

/*! A read-only view of model m restricted to views sel[0..n-1], for SRM_INIT_MODEL (shares W, S, sigma_s:
    free with srm_model_view_free, never SRM_free_model). */
static SRM_model srm_model_view( const SRM_model *m , const int *sel , int n )
{
   SRM_model v=*m ; int j ;
   v.nview=n ; v.nvox=(int *)malloc(sizeof(int)*(size_t)n) ; v.w=(float **)malloc(sizeof(float *)*(size_t)n) ;
   v.rho2 = (m->rho2!=NULL) ? (double *)malloc(sizeof(double)*(size_t)n) : NULL ;
   v.mu=NULL ; v.wrun=NULL ; v.a_sum=NULL ;
   for( j=0 ; j<n ; j++ ){ v.nvox[j]=m->nvox[sel[j]] ; v.w[j]=m->w[sel[j]] ; if( v.rho2!=NULL ) v.rho2[j]=m->rho2[sel[j]] ; }
   return v ;
}

static void srm_model_view_free( SRM_model *v ){ free(v->nvox) ; free(v->w) ; free(v->rho2) ; }

/*! S of the subjects sel[], warm-started from the fit to everyone (that fit's W, rho2 and Sigma_s for
    these subjects), iterating up to -niter / -tol.

    CAUTION (review 2026-11): both halves of a split start from the same pooled fit, which contains both
    halves.  With few iterations the two S stay near the pooled S and their similarity is biased upward.
    On NULL data (no shared signal; simulation with thd_srm.c via srm_cli, 12 subjects, k 10, 360 time
    points) the split-half mean cos^2 was 0.053 after 30 iterations against a chance level of about 0.024
    (cold fits), and unbiased (0.025) after 200.  See
    ~/Desktop/3dRSA_Validation/srm_validation/reliability_split_warmstart.py.  The default -niter 500 /
    -tol 1e-7 is probably long enough; small -niter values are not. */
static SRM_model * srm_fit_half( const srm_ctx *cx , const SRM_model *mpool , const int *sel , int n )
{
   SRM_params par=cx->par ; SRM_model *m=NULL , v=srm_model_view(mpool,sel,n) ; SRM_fitinfo fi ; int j , rc ;
   int *nv=(int *)malloc(sizeof(int)*(size_t)n) ; float **xv=(float **)malloc(sizeof(float *)*(size_t)n) ;
   const unsigned char **ob=(const unsigned char **)malloc(sizeof(unsigned char *)*(size_t)n) ; int miss=0 ;
   for( j=0 ; j<n ; j++ ){ nv[j]=cx->sb[sel[j]].nvox ; xv[j]=cx->sb[sel[j]].x ; ob[j]=cx->sb[sel[j]].obs ; miss|=(ob[j]!=NULL) ; }
   if( miss ) par.obs=(const unsigned char * const *)ob ;
   par.init=SRM_INIT_MODEL ; par.init_model=&v ; par.progress=NULL ; par.progress_ctx=NULL ;
   { srm_cmp cm ; srm_cmp_set(cx->sb,cx->sidx,sel,n,cx->K,&par,&cm) ;
     rc=SRM_fit(n,cx->T,nv,xv,&par,&m,&fi) ;
     srm_cmp_free(&cm) ; }
   if( rc!=SRM_OK ) ERROR_exit("a -reliability split fit failed: %s",SRM_strerror(rc)) ;
   SRM_free_fitinfo(&fi) ; srm_model_view_free(&v) ; free(nv) ; free(xv) ; free(ob) ;
   return m ;
}

/*! The p-quantile (0..1) of n sorted values, interpolating linearly between neighbors; NaN if n < 1. */
static double srm_pctile( const double *sorted , int n , double p )   /* linear interpolation */
{
   double x=p*(n-1) ; int i=(int)floor(x) ;
   if( n<1 ) return NAN ;
   if( i>=n-1 ) return sorted[n-1] ;
   return sorted[i]+(x-i)*(sorted[i+1]-sorted[i]) ;
}

static int srm_dcmp( const void *a , const void *b ){ double x=*(const double *)a , y=*(const double *)b ; return (x<y)?-1:(x>y) ; }

/*! For each group: nsplit random halves of its subjects (half the group, rounded down, against the rest), S
    fitted to each, and the subspace similarity of the two (mean cos^2 of their principal angles: 1 = the same
    shared space, near k/T for unrelated ones).  Groups need 4 subjects.  To PREFIX.reliability.txt; the
    similarity of every split goes to PREFIX.reliability_splits.txt. */
static void srm_reliability_split( const srm_ctx *cx , const SRM_model *mpool , int nsplit )
{
   int ng=cx->ngroup , g , i , sp , total=0 , done=0 ; srm_prog bar ; char fn[THD_MAX_NAME] ; FILE *fp , *fs ;
   unsigned long long seed=(unsigned long long)cx->par.seed*0x9E3779B97F4A7C15ULL+29ULL ;
   int *mem=(int *)malloc(sizeof(int)*(size_t)cx->nsub) , *ha=(int *)malloc(sizeof(int)*(size_t)cx->nsub) ;
   int *hb=(int *)malloc(sizeof(int)*(size_t)cx->nsub) ; double *val=(double *)malloc(sizeof(double)*(size_t)nsplit) ;
   for( g=0 ; g<ng ; g++ ){ int n=0 ; for( i=0 ; i<cx->nsub ; i++ ) n+=(cx->sb[i].group==g) ; if( n>=4 ) total+=nsplit ; }
   snprintf(fn,sizeof(fn),"%s.reliability.txt",cx->o->prefix) ;
   fp=fopen(fn,"w") ; if( fp==NULL ) ERROR_exit("cannot write '%s'",fn) ;
   snprintf(fn,sizeof(fn),"%s.reliability_splits.txt",cx->o->prefix) ;
   fs=fopen(fn,"w") ; if( fs==NULL ) ERROR_exit("cannot write '%s'",fn) ;
   fprintf(fp,"# -reliability split: each group's subjects split in two at random %d times; S fitted to each half (warm-started "
              "from the fit to everyone) and the two compared by the mean cos^2 of their principal angles (1 = the same shared "
              "space). %d components, %d %s; seed %u\n",nsplit,cx->K,cx->T,cx->o->axis_item?"items":"time points",cx->par.seed) ;
   fprintf(fp,"Group\tN\tNsplit\tmean\tp2.5\tp97.5\n") ;
   fprintf(fs,"Group\tSplit\tsimilarity\tHalfA\tHalfB\n") ;
   srm_prog_begin(&bar,cx->progress,cx->quiet,"split","halves",total) ;
   for( g=0 ; g<ng ; g++ ){
     int n=0 , na , nb ;
     for( i=0 ; i<cx->nsub ; i++ ) if( cx->sb[i].group==g ) mem[n++]=i ;
     if( n<4 ){
       WARNING_message("-reliability split: group %s has %d subjects; it needs 4 (two per half)",cx->glabel[g],n) ;
       fprintf(fp,"%s\t%d\t0\tNA\tNA\tNA\n",cx->glabel[g],n) ;
       continue ;
     }
     na=n/2 ; nb=n-na ;
     for( sp=0 ; sp<nsplit ; sp++ ){
       SRM_model *ma , *mb ; double mc2=NAN ; int q , k , rc ; char extra[128] ;
       srm_draw(mem,n,na,seed+(unsigned long long)(g*100003+sp),ha) ;
       for( q=0,k=0 ; q<n ; q++ ){ int in=0 , z ; for( z=0 ; z<na ; z++ ) in|=(ha[z]==mem[q]) ; if( !in ) hb[k++]=mem[q] ; }
       ma=srm_fit_half(cx,mpool,ha,na) ; mb=srm_fit_half(cx,mpool,hb,nb) ;
       rc=SRM_shared_similarity(ma->s,mb->s,cx->K,cx->T,&mc2,NULL) ;
       if( rc!=SRM_OK ) ERROR_exit("-reliability split: the similarity failed: %s",SRM_strerror(rc)) ;
       val[sp]=mc2 ;
       fprintf(fs,"%s\t%d\t%.7g\t",cx->glabel[g],sp+1,mc2) ;
       for( q=0 ; q<na ; q++ ) fprintf(fs,"%s%s",q?",":"",cx->sb[ha[q]].label) ;
       fputc('\t',fs) ;
       for( q=0 ; q<nb ; q++ ) fprintf(fs,"%s%s",q?",":"",cx->sb[hb[q]].label) ;
       fputc('\n',fs) ;
       SRM_free_model(ma) ; SRM_free_model(mb) ;
       snprintf(extra,sizeof(extra),"group %s, split %d/%d",cx->glabel[g],sp+1,nsplit) ;
       srm_prog_set(&bar,++done,extra) ;
     }
     { double m=0.0 ; for( sp=0 ; sp<nsplit ; sp++ ) m+=val[sp] ; m/=nsplit ;
       qsort(val,(size_t)nsplit,sizeof(double),srm_dcmp) ;
       fprintf(fp,"%s\t%d\t%d\t%.7g\t%.7g\t%.7g\n",cx->glabel[g],n,nsplit,m,srm_pctile(val,nsplit,0.025),srm_pctile(val,nsplit,0.975)) ;
       if( !cx->quiet ) ININFO_message("-reliability split, group %s (%d subjects): similarity of the halves %.4g [%.4g, %.4g]",
                                       cx->glabel[g],n,m,srm_pctile(val,nsplit,0.025),srm_pctile(val,nsplit,0.975)) ; }
   }
   srm_prog_end(&bar) ;
   fclose(fp) ; fclose(fs) ;
   if( !cx->quiet ) INFO_message("wrote %s.reliability.txt and %s.reliability_splits.txt",cx->o->prefix,cx->o->prefix) ;
   free(mem) ; free(ha) ; free(hb) ; free(val) ;
}

/*! The same for one -score_by level: the mean over folds of metric met at level lv (NaN if never scored). */
static double srm_subj_lmetric( const srm_cvres *r , int ii , int lv , int met )
{
   int ff , n=0 ; double s=0.0 ;
   for( ff=0 ; ff<r->nfold ; ff++ ){
     double v=r->lscore[(((size_t)ii*r->nfold+ff)*r->L+lv)*NMET+met] ;
     if( isfinite(v) ){ s+=v ; n++ ; }
   }
   return n>0 ? s/n : NAN ;
}

/*! Mean over folds of one metric for one subject (NaN if not computed). */
static double srm_subj_metric( const srm_cvres *r , int ii , int met )
{
   int ff , n=0 ; double s=0.0 ;
   for( ff=0 ; ff<r->nfold ; ff++ ){
     double v=r->score[((size_t)ii*r->nfold+ff)*NMET+met] ;
     if( isfinite(v) ){ s+=v ; n++ ; }
   }
   return n>0 ? s/n : NAN ;
}

/*! The same for the anchor's scores under -cv_adapt. */
static double srm_subj_ametric( const srm_cvres *r , int ii , int met )
{
   int ff , n=0 ; double s=0.0 ;
   if( r->ascore==NULL ) return NAN ;
   for( ff=0 ; ff<r->nfold ; ff++ ){
     double v=r->ascore[((size_t)ii*r->nfold+ff)*NMET+met] ;
     if( isfinite(v) ){ s+=v ; n++ ; }
   }
   return n>0 ? s/n : NAN ;
}

/*============================================================================
  -nperm: permutation tests on the per-subject scores (thd_permute.h).  Subjects are the exchangeable units.

  A "family" is one test applied to several scores at once (e.g. group difference in isc, match, r2, overall
  and per level).  Each relabeling (permutation of group labels, or sign flips of within-subject differences)
  is applied to all rows of the family together, and the family-wise p uses the maximum |t| over the rows in
  each relabeling: this controls the chance of any false positive within the family.
==============================================================================*/

typedef struct { const int *grp ; } srm_tstat_ctx ;

/*! Welch t of group 0 minus group 1 (NA values left out); the difference of the means as the effect. */
static float srm_stat_welch( float *y , int n , float *effect , void *ud )
{
   const int *g=((const srm_tstat_ctx *)ud)->grp ; double m[2]={0,0} , v[2]={0,0} ; int c[2]={0,0} , i , k ;
   for( i=0 ; i<n ; i++ ) if( isfinite(y[i]) ){ m[g[i]]+=y[i] ; c[g[i]]++ ; }
   if( c[0]<2 || c[1]<2 ){ if( effect ) *effect=NAN ; return 0.0f ; }
   for( k=0 ; k<2 ; k++ ) m[k]/=c[k] ;
   for( i=0 ; i<n ; i++ ) if( isfinite(y[i]) ){ double d=y[i]-m[g[i]] ; v[g[i]]+=d*d ; }
   for( k=0 ; k<2 ; k++ ) v[k]/=(c[k]-1) ;
   if( effect ) *effect=(float)(m[0]-m[1]) ;
   return (v[0]/c[0]+v[1]/c[1]>0.0) ? (float)((m[0]-m[1])/sqrt(v[0]/c[0]+v[1]/c[1])) : 0.0f ;
}

/*! One-sample t of the values (NA left out); their mean as the effect. */
static float srm_stat_onesample( float *y , int n , float *effect , void *ud )
{
   double m=0.0 , v=0.0 ; int c=0 , i ;
   (void)ud ;
   for( i=0 ; i<n ; i++ ) if( isfinite(y[i]) ){ m+=y[i] ; c++ ; }
   if( c<2 ){ if( effect ) *effect=NAN ; return 0.0f ; }
   m/=c ;
   for( i=0 ; i<n ; i++ ) if( isfinite(y[i]) ){ double d=y[i]-m ; v+=d*d ; }
   v/=(c-1) ;
   if( effect ) *effect=(float)m ;
   return (v>0.0) ? (float)(m/sqrt(v/c)) : 0.0f ;
}

/*! One family of tests: data[subject][element], a scheme, a statistic; rows to fp.
    exchange: PERM_EE permutes labels across subjects (group tests); PERM_ISE flips the sign of each subject's
    value (within-subject differences).  exact=1 lets the library enumerate every relabeling when there are
    no more than nperm of them. */
static void srm_perm_family( FILE *fp , const srm_ctx *cx , float **data , int nel , char **elname , const char *test ,
                             const char *factor , const char *contrast , int exchange , PERM_statfunc sf , void *sd ,
                             int nperm , int *n1 , int *n2 )
{
   PERM_scheme *ps=THD_perm_scheme_new(cx->nsub) ; PERM_set *pset ; PERM_result *pr ; int e ;
   ps->exchange=exchange ; ps->exact=1 ; ps->nperm=nperm ; ps->seed=(long)cx->par.seed ;
   pset=THD_perm_set_build(ps) ; THD_perm_scheme_free(ps) ;
   if( pset==NULL ) ERROR_exit("-nperm: could not build the relabelings for %s",test) ;
   pr=THD_permute_generic(data,cx->nsub,nel,NULL,pset,sf,sd,PERM_TAIL_TWO,NULL,NULL) ;
   if( pr==NULL ) ERROR_exit("-nperm: the %s test failed",test) ;
   for( e=0 ; e<nel ; e++ ){
     fprintf(fp,"%s\t%s\t%s\t%s\t%d\t%d\t",test,factor,elname[e],contrast,n1[e],n2[e]) ;
     if( isfinite(pr->effect[e]) ) fprintf(fp,"%.7g\t%.7g\t%.6g\t%.6g\t%d\t%d\n",pr->effect[e],pr->stat[e],pr->p_unc[e],pr->p_fwe[e],pset->nperm,pset->is_exact) ;
     else fprintf(fp,"NA\tNA\tNA\tNA\t%d\t%d\n",pset->nperm,pset->is_exact) ;
     if( !cx->quiet && isfinite(pr->effect[e]) )
       ININFO_message("%s %s %s (%s): t %.3g, p %.4g (family-wise %.4g)",test,factor,elname[e],contrast,pr->stat[e],pr->p_unc[e],pr->p_fwe[e]) ;
   }
   THD_perm_result_free(pr) ; THD_perm_set_free(pset) ;
}

/*! -nperm P: with two groups, each score (overall and per -score_by level) compared between them, group labels permuted
    over subjects; for each -score_by factor with two levels, the within-subject difference of its levels, the labels
    swapped within each subject (sign flips); and with both, the group x level interaction, group labels permuted over
    the subjects' differences.  Welch or one-sample t; p is two-sided, and p_fwe is family-wise over the rows of a test
    (max-statistic).  To PREFIX.tests.txt. */
static void srm_perm_tests( const srm_ctx *cx , const srm_cvres *r , srm_want w , int nperm )
{
   int mi[3]={M_ISC,M_MATCH,M_R2} , use[3]={w.isc,w.match,w.r2} , nm=0 , m , i , e , lv , fa ;
   const char *mnm[3]={"isc",w.ident?"ident":"match","r2"} ; char fn[THD_MAX_NAME] ; FILE *fp ;
   int *grp=(int *)malloc(sizeof(int)*(size_t)cx->nsub) ; srm_tstat_ctx tc ; int ntest=0 ;
   int maxel=3*(1+cx->L) ; float **data=(float **)malloc(sizeof(float *)*(size_t)cx->nsub) ;
   char **elname=(char **)malloc(sizeof(char *)*(size_t)maxel) ; int *n1=(int *)malloc(sizeof(int)*(size_t)maxel) , *n2=(int *)malloc(sizeof(int)*(size_t)maxel) ;
   for( m=0 ; m<3 ; m++ ) nm+=use[m] ;
   for( i=0 ; i<cx->nsub ; i++ ){ data[i]=(float *)malloc(sizeof(float)*(size_t)maxel) ; grp[i]=cx->sb[i].group ; }
   for( e=0 ; e<maxel ; e++ ) elname[e]=(char *)malloc(THD_MAX_NAME) ;
   tc.grp=grp ;
   snprintf(fn,sizeof(fn),"%s.tests.txt",cx->o->prefix) ;
   fp=fopen(fn,"w") ; if( fp==NULL ) ERROR_exit("cannot write '%s'",fn) ;
   fprintf(fp,"# -nperm %d permutation tests on the per-subject held-out scores (subjects are the exchangeable units; exact when "
              "the relabelings are few enough). t is Welch's (groups) or one-sample (within-subject differences); p two-sided; "
              "p_fwe family-wise over the rows of one test (max-statistic). N1/N2: subjects with a score.\n",nperm) ;
   fprintf(fp,"Test\tFactor\tScore\tContrast\tN1\tN2\teffect\tt\tp\tp_fwe\tnperm\texact\n") ;
   if( cx->ngroup==2 ){                                   /* groups: every score, overall and per level */
     char con[256] ; int nel=0 ;
     snprintf(con,sizeof(con),"%s - %s",cx->glabel[0],cx->glabel[1]) ;
     for( lv=-1 ; lv<cx->L ; lv++ ) for( m=0 ; m<3 ; m++ ) if( use[m] ){
       fa=0 ; if( lv>=0 ) while( fa+1<cx->nfac && lv>=cx->fac_off[fa+1] ) fa++ ;
       if( lv<0 ) snprintf(elname[nel],THD_MAX_NAME,"%s",mnm[m]) ;
       else snprintf(elname[nel],THD_MAX_NAME,"%s[%s=%s]",mnm[m],cx->fac_name[fa],cx->fac_lev[fa][lv-cx->fac_off[fa]]) ;
       n1[nel]=n2[nel]=0 ;
       for( i=0 ; i<cx->nsub ; i++ ){
         double v = (lv<0) ? srm_subj_metric(r,i,mi[m]) : srm_subj_lmetric(r,i,lv,mi[m]) ;
         data[i][nel]=(float)v ; if( isfinite(v) ){ if( grp[i]==0 ) n1[nel]++ ; else n2[nel]++ ; }
       }
       nel++ ;
     }
     srm_perm_family(fp,cx,data,nel,elname,"group","all",con,PERM_EE,srm_stat_welch,&tc,nperm,n1,n2) ; ntest++ ;
   } else if( !cx->quiet && cx->ngroup>2 )
     INFO_message("-nperm: %d groups; the group test compares two (take scores.txt to R for more)",cx->ngroup) ;
   for( fa=0 ; fa<cx->nfac ; fa++ ){                     /* two-level factors: within subjects, and the interaction */
     char con[256] ; int nel=0 , off=cx->fac_off[fa] ;
     if( cx->fac_nlev[fa]!=2 ){
       if( !cx->quiet ) INFO_message("-nperm: -score_by %s has %d levels; the level test compares two",cx->fac_name[fa],cx->fac_nlev[fa]) ;
       continue ;
     }
     snprintf(con,sizeof(con),"%s - %s",cx->fac_lev[fa][0],cx->fac_lev[fa][1]) ;
     for( m=0 ; m<3 ; m++ ) if( use[m] ){
       snprintf(elname[nel],THD_MAX_NAME,"%s",mnm[m]) ; n1[nel]=n2[nel]=0 ;
       for( i=0 ; i<cx->nsub ; i++ ){
         double a=srm_subj_lmetric(r,i,off,mi[m]) , b=srm_subj_lmetric(r,i,off+1,mi[m]) ;
         data[i][nel]=(float)(a-b) ;
         if( isfinite(a-b) ){ if( grp[i]==0 ) n1[nel]++ ; else n2[nel]++ ; }
       }
       nel++ ;
     }
     { int *nall=(int *)malloc(sizeof(int)*(size_t)nel) , *zero=(int *)calloc((size_t)nel,sizeof(int)) ;
       for( e=0 ; e<nel ; e++ ) nall[e]=n1[e]+n2[e] ;
       srm_perm_family(fp,cx,data,nel,elname,"level",cx->fac_name[fa],con,PERM_ISE,srm_stat_onesample,NULL,nperm,nall,zero) ; ntest++ ;
       free(nall) ; free(zero) ; }
     if( cx->ngroup==2 ){
       char con2[512] ; snprintf(con2,sizeof(con2),"(%s): %s - %s",con,cx->glabel[0],cx->glabel[1]) ;
       srm_perm_family(fp,cx,data,nel,elname,"interaction",cx->fac_name[fa],con2,PERM_EE,srm_stat_welch,&tc,nperm,n1,n2) ; ntest++ ;
     }
   }
   fclose(fp) ;
   if( ntest==0 ) WARNING_message("-nperm: nothing to test: it needs two groups, or a -score_by factor with two levels") ;
   else if( !cx->quiet ) INFO_message("wrote %s.tests.txt",cx->o->prefix) ;
   for( i=0 ; i<cx->nsub ; i++ ) free(data[i]) ;
   for( e=0 ; e<maxel ; e++ ) free(elname[e]) ;
   free(data) ; free(elname) ; free(n1) ; free(n2) ; free(grp) ;
}

/*! PREFIX.scores_folds.txt (one row per subject x fold x level, 'all' first) and PREFIX.scores.txt (one row
    per subject x level, the mean over folds, plus reliability columns and the table's numeric covariates).
    Missing scores are written NA.  nsamp in scores.txt is summed over folds. */
static void srm_write_scores( const srm_ctx *cx , const srm_folds *f , const srm_cvres *r , srm_want w ,
                              const char *mode , THD_datatable *tab , int *carry , int ncarry )
{
   char fn[THD_MAX_NAME] ; FILE *fp ; int ii , ff , cc ; const char *mname = w.ident ? "ident" : "match" ;
   snprintf(fn,sizeof(fn),"%s.scores_folds.txt",cx->o->prefix) ;
   fp=fopen(fn,"w") ; if( fp==NULL ) ERROR_exit("cannot write '%s'",fn) ;
   fprintf(fp,"# held-out scores, one row per subject and fold (%s; %d folds)\n",mode,f->nfold) ;
   if( r->ascore!=NULL )
     fprintf(fp,"# -cv_adapt %g: nsamp and the scores are the samples after each held-out run's first %g, through the run's own "
                "W estimated from those; the _anchor columns score the anchor W on the same samples\n",cx->o->cv_adapt,cx->o->cv_adapt) ;
   fprintf(fp,"Subj\tGroup\tFold\tFactor\tLevel\tnsamp\tisc\t%s\tchance\tr2",mname) ;
   if( r->ascore!=NULL ) fprintf(fp,"\tisc_anchor\t%s_anchor\tr2_anchor",mname) ;
   fputc('\n',fp) ;
   for( ii=0 ; ii<cx->nsub ; ii++ ) for( ff=0 ; ff<f->nfold ; ff++ ){
     int lv ;
     for( lv=-1 ; lv<cx->L ; lv++ ){
       const double *sc = (lv<0) ? r->score+((size_t)ii*f->nfold+ff)*NMET
                                 : r->lscore+(((size_t)ii*f->nfold+ff)*cx->L+lv)*NMET ;
       int fa=0 ; if( lv>=0 ) while( fa+1<cx->nfac && lv>=cx->fac_off[fa+1] ) fa++ ;
       fprintf(fp,"%s\t%s\t%s\t%s\t%s\t%d",cx->sb[ii].label,cx->glabel[cx->sb[ii].group],f->name[ff],
               lv<0 ? "all" : cx->fac_name[fa] , lv<0 ? "all" : cx->fac_lev[fa][lv-cx->fac_off[fa]] ,(int)sc[M_NSAMP]) ;
       for( cc=0 ; cc<4 ; cc++ ){ int mi[4]={M_ISC,M_MATCH,M_CHANCE,M_R2} ;
         if( isfinite(sc[mi[cc]]) ) fprintf(fp,"\t%.7g",sc[mi[cc]]) ; else fprintf(fp,"\tNA") ; }
       if( r->ascore!=NULL ){
         const double *as=r->ascore+((size_t)ii*f->nfold+ff)*NMET ;
         for( cc=0 ; cc<3 ; cc++ ){ int mi[3]={M_ISC,M_MATCH,M_R2} ;
           if( lv<0 && isfinite(as[mi[cc]]) ) fprintf(fp,"\t%.7g",as[mi[cc]]) ; else fprintf(fp,"\tNA") ; }
       }
       fputc('\n',fp) ;
     }
   }
   fclose(fp) ;
   snprintf(fn,sizeof(fn),"%s.scores.txt",cx->o->prefix) ;
   fp=fopen(fn,"w") ; if( fp==NULL ) ERROR_exit("cannot write '%s'",fn) ;
   fprintf(fp,"# held-out scores per subject, the mean over folds (%s; %d folds). isc is the mean over components of the "
              "correlation with the others' shared response, in Fisher-z space; %s (chance in the next column); r2 is "
              "reconstruction R^2\n",mode,f->nfold,
              w.ident ? "ident is item identification accuracy: each held-out item must correlate best with its own "
                        "counterpart among the fold's items" : "match is time-segment matching accuracy") ;
   if( r->ascore!=NULL )
     fprintf(fp,"# -cv_adapt %g: the scores are those of each held-out run's own W, on the samples after its first %g; "
                "the _anchor columns score the anchor W on the same samples\n",cx->o->cv_adapt,cx->o->cv_adapt) ;
   fprintf(fp,"Subj\tGroup\tFactor\tLevel\tnsamp\tisc\t%s\tchance\tr2",mname) ;
   if( r->ascore!=NULL ) fprintf(fp,"\tisc_anchor\t%s_anchor\tr2_anchor",mname) ;
   if( cx->rel!=NULL ) fprintf(fp,"\trel_pattern\trel_series\trel_ratio") ;
   for( cc=0 ; cc<ncarry ; cc++ ) fprintf(fp,"\t%s",tab->cname[carry[cc]]) ;
   fputc('\n',fp) ;
   for( ii=0 ; ii<cx->nsub ; ii++ ){
     int lv ;
     for( lv=-1 ; lv<cx->L ; lv++ ){
       int ns=0 , fa=0 ;
       if( lv>=0 ) while( fa+1<cx->nfac && lv>=cx->fac_off[fa+1] ) fa++ ;
       for( ff=0 ; ff<f->nfold ; ff++ )
         ns+=(int)((lv<0) ? r->score[((size_t)ii*f->nfold+ff)*NMET+M_NSAMP]
                          : r->lscore[(((size_t)ii*f->nfold+ff)*cx->L+lv)*NMET+M_NSAMP]) ;
       fprintf(fp,"%s\t%s\t%s\t%s\t%d",cx->sb[ii].label,cx->glabel[cx->sb[ii].group],
               lv<0 ? "all" : cx->fac_name[fa] , lv<0 ? "all" : cx->fac_lev[fa][lv-cx->fac_off[fa]] ,ns) ;
       { int mi[4]={M_ISC,M_MATCH,M_CHANCE,M_R2} ;
         for( cc=0 ; cc<4 ; cc++ ){
           double v = (lv<0) ? srm_subj_metric(r,ii,mi[cc]) : srm_subj_lmetric(r,ii,lv,mi[cc]) ;
           if( isfinite(v) ) fprintf(fp,"\t%.7g",v) ; else fprintf(fp,"\tNA") ; } }
       if( r->ascore!=NULL ){
         int mi[3]={M_ISC,M_MATCH,M_R2} ;
         for( cc=0 ; cc<3 ; cc++ ){
           double v = (lv<0) ? srm_subj_ametric(r,ii,mi[cc]) : NAN ;
           if( isfinite(v) ) fprintf(fp,"\t%.7g",v) ; else fprintf(fp,"\tNA") ; } }
       if( cx->rel!=NULL ){                               /* the subject's own reliability, and the share of it reached */
         const double *rv=cx->rel+2*((size_t)ii*(1+cx->L)+lv+1) ;
         double isc = (lv<0) ? srm_subj_metric(r,ii,M_ISC) : srm_subj_lmetric(r,ii,lv,M_ISC) ;
         double ratio = (isfinite(isc) && isfinite(rv[0]) && rv[0]>0.0) ? isc/rv[0] : NAN ;
         for( cc=0 ; cc<3 ; cc++ ){ double v = (cc<2) ? rv[cc] : ratio ; if( isfinite(v) ) fprintf(fp,"\t%.7g",v) ; else fprintf(fp,"\tNA") ; }
       }
       for( cc=0 ; cc<ncarry ; cc++ ) fprintf(fp,"\t%s",DT_CELL(tab,cx->sb[ii].row[0],carry[cc])) ;
       fputc('\n',fp) ;
     }
   }
   fclose(fp) ;
}

/*! Group means (and standard errors across subjects) for the log, and for -nfeatures. */
static void srm_cv_summary( const srm_ctx *cx , const srm_cvres *r , srm_want w , const char *mode , int nfold )
{
   int g , ii , met ; const char *nm[4]={"isc",w.ident?"ident":"match","chance","r2"} ; int mi[4]={M_ISC,M_MATCH,M_CHANCE,M_R2} ;
   int sel[4]={w.isc,w.match,w.match,w.r2} ;
   if( cx->quiet ) return ;
   INFO_message("held-out scores (%s, %d folds), mean over subjects with its standard error:",mode,nfold) ;
   for( g=-1 ; g<cx->ngroup ; g++ ){
     char line[512] ; int pos=0 , n=0 ;
     if( g>=0 && cx->ngroup<2 ) break ;
     for( ii=0 ; ii<cx->nsub ; ii++ ) n+=(g<0 || cx->sb[ii].group==g) ;
     pos+=snprintf(line,sizeof(line),"%s (%d subjects):",g<0?"all":cx->glabel[g],n) ;
     for( met=0 ; met<4 ; met++ ){
       double s=0.0 , s2=0.0 ; int cnt=0 ;
       if( !sel[met] ) continue ;
       for( ii=0 ; ii<cx->nsub ; ii++ ) if( g<0 || cx->sb[ii].group==g ){
         double v=srm_subj_metric(r,ii,mi[met]) ; if( isfinite(v) ){ s+=v ; s2+=v*v ; cnt++ ; } }
       if( cnt>0 ){
         double mean=s/cnt , sd=(cnt>1) ? sqrt((s2-cnt*mean*mean)/(cnt-1)) : 0.0 ;
         if( met==2 ) pos+=snprintf(line+pos,sizeof(line)-pos,"  %s %.4g",nm[met],mean) ;
         else pos+=snprintf(line+pos,sizeof(line)-pos,"  %s %.4g (%.2g)",nm[met],mean,cnt>1?sd/sqrt((double)cnt):0.0) ;
       }
     }
     ININFO_message("%s",line) ;
   }
   if( r->ascore!=NULL ){                             /* -cv_adapt: the anchor on the same samples */
     char line[512] ; int pos=0 ;
     pos+=snprintf(line,sizeof(line),"the anchor W on the same samples (all):") ;
     for( met=0 ; met<4 ; met++ ){
       double sm=0.0 ; int cnt=0 ;
       if( !sel[met] || met==2 ) continue ;
       for( ii=0 ; ii<cx->nsub ; ii++ ){ double v=srm_subj_ametric(r,ii,mi[met]) ; if( isfinite(v) ){ sm+=v ; cnt++ ; } }
       if( cnt>0 ) pos+=snprintf(line+pos,sizeof(line)-pos,"  %s %.4g",nm[met],sm/cnt) ;
     }
     ININFO_message("%s",line) ;
   }
   if( cx->L>0 ){                                     /* each -score_by level, over all subjects */
     int lv ;
     ININFO_message("by -score_by level (mean over subjects; see %s.scores.txt for each subject):",cx->o->prefix) ;
     for( lv=0 ; lv<cx->L ; lv++ ){
       char line[512] ; int pos=0 , fa=0 ;
       while( fa+1<cx->nfac && lv>=cx->fac_off[fa+1] ) fa++ ;
       pos+=snprintf(line,sizeof(line),"%s=%s:",cx->fac_name[fa],cx->fac_lev[fa][lv-cx->fac_off[fa]]) ;
       for( met=0 ; met<4 ; met++ ){
         double sm=0.0 ; int cnt=0 ;
         if( !sel[met] ) continue ;
         for( ii=0 ; ii<cx->nsub ; ii++ ){ double v=srm_subj_lmetric(r,ii,lv,mi[met]) ; if( isfinite(v) ){ sm+=v ; cnt++ ; } }
         if( cnt>0 ) pos+=snprintf(line+pos,sizeof(line)-pos,"  %s %.4g",nm[met],sm/cnt) ;
         else        pos+=snprintf(line+pos,sizeof(line)-pos,"  %s NA",nm[met]) ;
       }
       ININFO_message("  %s",line) ;
     }
   }
}

/*! A feature to drop: variance zero in some run, for zscore none.  (With z-scoring,
    SRM_zscore_segments flags these itself.)  bad[f] gets 0/1; returns the count. */
static int srm_flag_constant( float *x , int nvox , int ntime , int nseg , const int *seglen ,
                              unsigned char *bad )
{
   int f , nb=0 ;
   for( f=0 ; f<nvox ; f++ ){
     const float *row=x+(size_t)f*ntime ; int rr , c0=0 , isb=0 ;
     for( rr=0 ; rr<nseg && !isb ; c0+=seglen[rr],rr++ ){
       double m=0.0 , v=0.0 ; int t , L=seglen[rr] ;
       for( t=0 ; t<L ; t++ ) m+=row[c0+t] ;
       m/=L ;
       for( t=0 ; t<L ; t++ ){ double d=row[c0+t]-m ; v+=d*d ; }
       if( !(v>0.0) ) isb=1 ;
     }
     bad[f]=(unsigned char)isb ; nb+=isb ;
   }
   return nb ;
}

/*! Keep only the rows with bad[f]==0, packed to the front (in place; vox[], the grid index of each row,
    is packed the same way so maps can be written back to the right voxels).  Returns the rows kept. */
static int srm_compact_rows( float *x , int nvox , int ntime , const unsigned char *bad , int *vox )
{
   int f , q=0 ;
   for( f=0 ; f<nvox ; f++ ) if( !bad[f] ){
     if( q!=f ) memmove(x+(size_t)q*ntime,x+(size_t)f*ntime,sizeof(float)*(size_t)ntime) ;
     if( vox!=NULL ) vox[q]=vox[f] ;
     q++ ;
   }
   return q ;
}

/*============================================================================
  The data stage, as functions: table -> subjects -> reading -> timeline -> normalized.
  Fitting, applying a saved model, and applying a saved W all go through it.
==============================================================================*/

/* The data stage's state.  Two timelines: the INPUT timeline (Tin columns: every volume of every run,
   runs concatenated; c0in[r] and runlen[r] locate run r) and the RETAINED timeline (T columns, after
   -censor / -cenmode drop; time_index[t] is the input column of retained column t, seglen[r] the retained
   length of run r).  On the item axis both are the nitem item columns. */
typedef struct {
   THD_datatable *tab ; THD_datatable_index *dx ;
   int nsub , nrun , have_run , ngroup ;
   char **glabel , **runname ;
   srm_subj *sb ;
   int *runlen , Tin , *c0in ;
   int icol_mask , icol_group , icol_run ;
   mask_entry *mcache ; int nmcache ; mask_entry *gmask ;
   byte *keep ;                        /* [Tin] retained time points */
   byte *zero_any ; int *nzero ;       /* -zcensor bookkeeping */
   int T ; int *seglen , *time_index ; /* retained timeline */
   int axis_item , nitem ;             /* -axis item: T == nitem columns, one per key */
   int concat , *cstart ;              /* runs within one input: their starts (from -concat or Xmat) */
   int icol_xmat , icol_censor ;
   char **item_key ;                   /* [nitem] Item, or Item:n for the n-th repeat */
   double *relpre ;                    /* -repeated_stimulus: [nsub][2] within-subject reliability, before averaging */
   int rep_nrun , rep_T ;              /* -repeated_stimulus: the timeline before averaging (rep_nrun 0: not averaged) */
   int *rep_seglen , *rep_time_index ;
   double *relitems ;                  /* -compress, -axis item -reliability within: as cx->rel, made while reading */
} srm_data ;

/*! Subjects, their runs, groups and masks, from the table.
    With a Run column the table is indexed by (Subj, Run): subject ss's run rr is table row
    dx->row_of[ss*nrun+rr], so every subject must have every run (the index refuses gaps and duplicates).
    Per-subject columns (Mask, Censor, Group) must agree across that subject's rows.  ERROR_exit on any
    inconsistency, or fewer than min_subjects subjects. */
static void srm_data_table( const srm_opts *o , THD_datatable *tab , srm_data *d , int min_subjects )
{
   char *tab_cols[2] ; int ss , rr , jj ;
   d->tab=tab ;
   if( tab->icol_input<0 ) ERROR_exit("the data table needs an InputFile column") ;
   d->icol_run=THD_datatable_column(tab,o->run_col) ;
   d->have_run=(d->icol_run>=0) ;
   if( d->have_run && tab->icol_subj<0 ) ERROR_exit("the table needs a Subj column when it has a '%s' column",o->run_col) ;
   if( d->have_run ){
     tab_cols[0]=tab->cname[tab->icol_subj] ; tab_cols[1]=o->run_col ;
     d->dx=THD_datatable_index_columns(tab,2,tab_cols,NULL,NULL) ;
     d->nsub=d->dx->nlevel[0] ; d->nrun=d->dx->nlevel[1] ;
   } else {
     if( tab->icol_subj>=0 ){
       tab_cols[0]=tab->cname[tab->icol_subj] ;
       d->dx=THD_datatable_index_columns(tab,1,tab_cols,NULL,NULL) ;   /* duplicate subjects are fatal */
       d->nsub=d->dx->nlevel[0] ;
     } else d->nsub=tab->nrow ;
     d->nrun=1 ;
   }
   if( d->nsub<min_subjects ){
     if( min_subjects==2 ) ERROR_exit("SRM needs at least two subjects; the table has %d",d->nsub) ;
     ERROR_exit("the table has %d subject%s; need at least %d",d->nsub,d->nsub==1?"":"s",min_subjects) ;
   }

   d->sb=(srm_subj *)calloc((size_t)d->nsub,sizeof(srm_subj)) ;
   d->icol_xmat=THD_datatable_column(tab,"Xmat") ; d->icol_censor=THD_datatable_column(tab,"Censor") ;
   if( (d->icol_xmat>=0 || o->concat!=NULL) && d->have_run )
     ERROR_exit("%s splits each subject's one input into runs; the table also has a '%s' column. Use one or the other",
                d->icol_xmat>=0 ? "The Xmat column" : "-concat",o->run_col) ;
   if( d->icol_xmat>=0 && o->concat!=NULL ) ERROR_exit("give the runs once: -concat, or the Xmat column, not both") ;
   d->icol_mask=THD_datatable_column(tab,"Mask") ;
   d->icol_group=THD_datatable_column(tab,o->group_col) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     srm_subj *s=d->sb+ss ;
     s->nrun=d->nrun ; s->row=(int *)calloc((size_t)d->nrun,sizeof(int)) ;
     s->label=strdup(d->dx ? d->dx->level[0][ss] : tab->subj[ss]) ;
     for( rr=0 ; rr<d->nrun ; rr++ )
       s->row[rr]=d->have_run ? d->dx->row_of[(size_t)ss*d->nrun+rr] : (d->dx ? d->dx->row_of[ss] : ss) ;
     if( d->icol_mask>=0 ){                             /* one Mask per subject, on all their rows */
       const char *mn=DT_CELL(tab,s->row[0],d->icol_mask) ;
       for( rr=1 ; rr<d->nrun ; rr++ )
         if( strcmp(DT_CELL(tab,s->row[rr],d->icol_mask),mn)!=0 )
           ERROR_exit("Subj %s has different Mask entries on different rows",s->label) ;
       if( mn[0]!='\0' && strcmp(mn,"-")!=0 && strcmp(mn,"NA")!=0 ) s->mask_name=strdup(mn) ;
     }
     if( d->icol_xmat>=0 ){
       const char *xn=DT_CELL(tab,s->row[0],d->icol_xmat) ;
       if( xn[0]!='\0' && strcmp(xn,"-")!=0 && strcmp(xn,"NA")!=0 ) s->xmat=strdup(xn) ;
     }
     if( d->icol_censor>=0 ){                           /* one Censor file per subject, covering every run */
       const char *cn=DT_CELL(tab,s->row[0],d->icol_censor) ;
       for( rr=1 ; rr<d->nrun ; rr++ )
         if( strcmp(DT_CELL(tab,s->row[rr],d->icol_censor),cn)!=0 )
           ERROR_exit("Subj %s has different Censor entries on different rows (one file covers all of a subject's runs)",s->label) ;
       if( cn[0]!='\0' && strcmp(cn,"-")!=0 && strcmp(cn,"NA")!=0 ) s->censor_file=strdup(cn) ;
     }
     if( d->icol_group>=0 ){
       const char *gn=DT_CELL(tab,s->row[0],d->icol_group) ;
       for( rr=1 ; rr<d->nrun ; rr++ )
         if( strcmp(DT_CELL(tab,s->row[rr],d->icol_group),gn)!=0 )
           ERROR_exit("Subj %s has different %s entries on different rows",s->label,o->group_col) ;
       for( jj=0 ; jj<d->ngroup ; jj++ ) if( strcmp(d->glabel[jj],gn)==0 ) break ;
       if( jj==d->ngroup ){ d->glabel=(char **)realloc(d->glabel,sizeof(char *)*(size_t)(d->ngroup+1)) ; d->glabel[d->ngroup++]=strdup(gn) ; }
       s->group=jj ;
     }
   }
   if( d->ngroup==0 ){ d->glabel=(char **)malloc(sizeof(char *)) ; d->glabel[0]=strdup("All") ; d->ngroup=1 ; }
}

/*============================================================================
  Several runs in one input (afni_proc.py's errts): their starts come from -concat, as in
  3dDeconvolve and 3dTproject, or from the subject's X.xmat.1D (RunStart, NRowFull, GoodList).
==============================================================================*/

static void srm_int_list( char *dst , size_t nd , const int *a , int n ) ;

/*! -concat: run starts, as a 1D list ('1D: 0 300 600' or a file); *n receives the count.
    Must start at 0 and strictly increase; ERROR_exit otherwise.  Returns a malloc'ed array. */
static int * srm_read_concat( const char *spec , int *n )
{
   MRI_IMAGE *cim=mri_read_1D((char *)spec) ; float *car ; int *st , ii ;
   if( cim==NULL ) ERROR_exit("cannot read -concat '%s'",spec) ;
   *n=cim->nvox ; car=MRI_FLOAT_PTR(cim) ;
   st=(int *)malloc(sizeof(int)*(size_t)(*n)) ;
   for( ii=0 ; ii<*n ; ii++ ){
     if( !isfinite(car[ii]) || car[ii]!=rintf(car[ii]) || car[ii]<0.0f )
       ERROR_exit("-concat entry %d is %g; run starts are time-point indices (0, then each run's first time point)",ii+1,car[ii]) ;
     st[ii]=(int)rintf(car[ii]) ;
   }
   mri_free(cim) ;
   if( st[0]!=0 ) ERROR_exit("-concat must start at 0 (it lists where each run starts), not %d",st[0]) ;
   for( ii=1 ; ii<*n ; ii++ ) if( st[ii]<=st[ii-1] )
     ERROR_exit("-concat entries must increase: entry %d (%d) is not after entry %d (%d)",ii+1,st[ii],ii,st[ii-1]) ;
   return st ;
}

/*! A subject's X.xmat.1D (thd_xmat.h): run starts, the full length, and (s->good/ngood, when some
    time points were censored) the uncensored ones.  *nrun, *nfull are set. */
static int * srm_read_xmat( srm_subj *s , int *nrun , int *nfull )
{
   char err[512] ; THD_xmat *xm=THD_xmat_read(s->xmat,err,sizeof(err)) ; int *st ;
   if( xm==NULL ) ERROR_exit("Subj %s: %s",s->label,err) ;
   *nfull=xm->nfull ; *nrun=xm->nrun ;
   st=(int *)malloc(sizeof(int)*(size_t)xm->nrun) ; memcpy(st,xm->runstart,sizeof(int)*(size_t)xm->nrun) ;
   if( xm->nrow<xm->nfull ){
     s->ngood=xm->nrow ; s->good=(int *)malloc(sizeof(int)*(size_t)xm->nrow) ; memcpy(s->good,xm->good,sizeof(int)*(size_t)xm->nrow) ;
   }
   THD_xmat_free(xm) ;
   return st ;
}

/*! After srm_data_shape has read each subject's one input: the runs inside it, the same for everyone.
    Run starts come from -concat or from the first Xmat; every other Xmat must agree, and its NRowFull must
    equal the input length (an Xmat from a censored-then-concatenated input would not).  On return nrun and
    runlen[] describe the runs within the single input, and d->concat is set. */
static void srm_data_concat_runs( const srm_opts *o , srm_data *d )
{
   int ss , rr , nr=0 , nt=d->runlen[0] , *st=NULL , ncens=0 ;
   if( o->concat!=NULL ) st=srm_read_concat(o->concat,&nr) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     srm_subj *s=d->sb+ss ; int n2 , *st2 , nfull ;
     if( s->xmat==NULL ) continue ;
     st2=srm_read_xmat(s,&n2,&nfull) ;
     if( nfull!=nt )
       ERROR_exit("Subj %s: '%s' describes %d time points (NRowFull), but the input has %d",s->label,s->xmat,nfull,nt) ;
     if( st==NULL ){ st=st2 ; nr=n2 ; continue ; }
     for( rr=0 ; rr<nr && n2==nr ; rr++ ) if( st2[rr]!=st[rr] ) break ;
     if( n2!=nr || rr<nr ){
       char a1[256] , a2[256] ;
       srm_int_list(a1,sizeof(a1),st,nr) ; srm_int_list(a2,sizeof(a2),st2,n2) ;
       ERROR_exit("Subj %s: the runs in '%s' start at %s, not at %s as for %s; on the time axis every subject needs the "
                  "same runs",s->label,s->xmat,a2,a1,o->concat!=NULL?"-concat":"the first subject with an Xmat") ;
     }
     free(st2) ;
   }
   if( st==NULL ) return ;
   if( st[nr-1]>=nt ) ERROR_exit("the last run starts at time point %d, but the inputs have %d",st[nr-1],nt) ;
   d->concat=1 ; d->cstart=st ; d->nrun=nr ;
   free(d->runlen) ; d->runlen=(int *)malloc(sizeof(int)*(size_t)nr) ;
   for( rr=0 ; rr<nr ; rr++ ) d->runlen[rr]=((rr<nr-1) ? st[rr+1] : nt)-st[rr] ;
   if( !o->quiet ){
     char ls[256] ; int pos=0 ;
     for( rr=0 ; rr<nr && pos<220 ; rr++ ) pos+=snprintf(ls+pos,sizeof(ls)-pos,"%s%d",rr?",":"",d->runlen[rr]) ;
     INFO_message("each input holds %d runs (%s, from %s): lengths %s",nr,o->concat!=NULL?"-concat":"the Xmat column",
                  o->concat!=NULL?o->concat:"RunStart",ls) ;
   }
   for( ss=0 ; ss<d->nsub ; ss++ ) if( d->sb[ss].good!=NULL ) ncens++ ;
   if( ncens>0 && !o->quiet )
     ININFO_message("%d subject%s' Xmat GoodList censors time points (see -cenmode)",ncens,ncens==1?"":"s") ;
}

/*! Open every input (header only for datasets): lengths, grids, masks.  With need_mask == 0
    (applying a saved W) no mask is read; the features come from W.
    Sets each subject's vox[] (the grid indices inside its mask, or 0..ncol-1 for a .1D) and the run
    lengths, which must be identical for every subject: on the time axis column t means the same moment
    of the stimulus for everyone.  The first run's dataset is kept open in s->geom for output geometry. */
static void srm_data_shape( const srm_opts *o , srm_data *d , int need_mask , int Kmin )
{
   int ss , rr , ii , jj ; THD_datatable *tab=d->tab ;
   if( need_mask && o->maskname!=NULL ) d->gmask=srm_mask_get(&d->mcache,&d->nmcache,o->maskname) ;
   d->runlen=(int *)calloc((size_t)d->nrun,sizeof(int)) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     srm_subj *s=d->sb+ss ;
     for( rr=0 ; rr<d->nrun ; rr++ ){
       const char *fn=DT_CELL(tab,s->row[rr],tab->icol_input) ; int nt , is1 ;
       is1=srm_is1D(fn) ;
       if( rr==0 ) s->is1D=is1 ;
       else if( is1!=s->is1D ) ERROR_exit("Subj %s mixes .1D files and datasets across runs",s->label) ;
       if( is1 ){
         MRI_IMAGE *im=mri_read_1D((char *)fn) ;
         if( im==NULL ) ERROR_exit("cannot read 1D file '%s' (Subj %s)",fn,s->label) ;
         nt=im->nx ;
         if( rr==0 ){
           s->nvox_mask=im->ny ;
           if( need_mask ){ s->vox=(int *)malloc(sizeof(int)*(size_t)im->ny) ; for( jj=0 ; jj<im->ny ; jj++ ) s->vox[jj]=jj ; }
         }
         else if( im->ny!=s->nvox_mask ) ERROR_exit("Subj %s: '%s' has %d columns; the first run had %d",s->label,fn,im->ny,s->nvox_mask) ;
         mri_free(im) ;
       } else {
         THD_3dim_dataset *ds=THD_open_dataset((char *)fn) ;
         CHECK_OPEN_ERROR(ds,fn) ;
         nt=DSET_NVALS(ds) ;
         if( rr==0 ){
           s->geom=ds ;
           if( need_mask ){
             mask_entry *me ;
             if( s->mask_name!=NULL ) me=srm_mask_get(&d->mcache,&d->nmcache,s->mask_name) ;
             else if( d->gmask!=NULL ) me=d->gmask ;
             else ERROR_exit("Subj %s: datasets need a mask: give -mask or a Mask column in the table",s->label) ;
             if( !EQUIV_GRIDS(ds,me->ds) )
               ERROR_exit("Subj %s: '%s' is not on the grid of mask '%s'",s->label,fn,me->name) ;
             s->nvox_mask=me->nmask ;
             s->vox=(int *)malloc(sizeof(int)*(size_t)me->nmask) ;
             for( jj=0,ii=0 ; jj<me->nvox ; jj++ ) if( me->mask[jj] ) s->vox[ii++]=jj ;
           }
         } else {
           if( !EQUIV_GRIDS(ds,s->geom) ) ERROR_exit("Subj %s: run %s is off the grid of run 1",s->label,d->have_run?d->dx->level[1][rr]:"?") ;
           DSET_delete(ds) ;
         }
       }
       if( ss==0 ) d->runlen[rr]=nt ;
       else if( nt!=d->runlen[rr] )
         ERROR_exit("Subj %s, run %s has %d time points; the first subject's has %d",s->label,d->have_run?d->dx->level[1][rr]:"1",nt,d->runlen[rr]) ;
     }
     if( need_mask && s->nvox_mask<Kmin ) ERROR_exit("Subj %s has %d features; -nfeatures is %d",s->label,s->nvox_mask,Kmin) ;
   }
   if( !d->have_run && (o->concat!=NULL || d->icol_xmat>=0) ) srm_data_concat_runs(o,d) ;
   d->Tin=0 ;
   for( rr=0 ; rr<d->nrun ; rr++ ){ d->Tin+=d->runlen[rr] ; if( d->runlen[rr]<2 ) ERROR_exit("run %d has %d time points; need at least 2",rr+1,d->runlen[rr]) ; }
   d->c0in=(int *)calloc((size_t)d->nrun,sizeof(int)) ;
   for( rr=1 ; rr<d->nrun ; rr++ ) d->c0in[rr]=d->c0in[rr-1]+d->runlen[rr-1] ;
   d->runname=(char **)malloc(sizeof(char *)*(size_t)d->nrun) ;
   for( rr=0 ; rr<d->nrun ; rr++ ){ char nm[24] ; snprintf(nm,sizeof(nm),"%d",rr+1) ; d->runname[rr]=strdup(d->have_run ? d->dx->level[1][rr] : nm) ; }
   if( !o->quiet ){
     INFO_message("%d subjects, %d group%s, %d run%s, %d time points in all",d->nsub,d->ngroup,d->ngroup>1?"s":"",d->nrun,d->nrun>1?"s":"",d->Tin) ;
     if( d->nrun==1 && !d->have_run )
       ININFO_message("one run: if each input holds several runs (an afni_proc.py errts, say), give -concat or an Xmat column, "
                      "so that z-scoring and -cv respect the run boundaries") ;
     if( d->ngroup>1 ){ for( jj=0 ; jj<d->ngroup ; jj++ ){ int cnt=0 ; for( ss=0 ; ss<d->nsub ; ss++ ) cnt+=(d->sb[ss].group==jj) ;
         ININFO_message("group %s: %d subjects",d->glabel[jj],cnt) ; } }
   }
}

/*! The retained time points: from -censor, or from a saved model's mask (given_keep).
    d->keep[t] (t < Tin) is 1 for retained.  Subjects' own censoring is applied later
    (srm_data_subject_censor), and only -cenmode drop changes keep there. */
static void srm_data_censor( const srm_opts *o , srm_data *d , const unsigned char *given_keep )
{
   int ii ;
   d->keep=(byte *)malloc((size_t)d->Tin) ; memset(d->keep,1,(size_t)d->Tin) ;
   if( given_keep!=NULL ) memcpy(d->keep,given_keep,(size_t)d->Tin) ;
   else if( o->censor_name!=NULL ){
     MRI_IMAGE *cim=mri_read_1D(o->censor_name) , *fim ; float *car ;
     if( cim==NULL ) ERROR_exit("cannot read -censor file '%s'",o->censor_name) ;
     fim=mri_to_float(cim) ; if( fim!=cim ) mri_free(cim) ;
     if( fim->nvox!=d->Tin ) ERROR_exit("-censor has %d values; the data have %d time points",fim->nvox,d->Tin) ;
     car=MRI_FLOAT_PTR(fim) ;
     for( ii=0 ; ii<d->Tin ; ii++ ){
       if( !isfinite(car[ii]) || (car[ii]!=0.0f && car[ii]!=1.0f) )
         ERROR_exit("-censor value %d is %.7g; expected 0 or 1",ii+1,car[ii]) ;
       d->keep[ii]=(byte)(car[ii]!=0.0f) ;
     }
     mri_free(fim) ;
   }
   if( o->zcensor ){
     d->zero_any=(byte *)calloc((size_t)d->Tin,1) ; d->nzero=(int *)calloc((size_t)d->nsub,sizeof(int)) ;
   }
}

/*! Read every subject's masked time series, all runs, onto the input timeline.
    s->x becomes nvox_mask x Tin, run rr at columns c0in[rr]..  (with -concat/Xmat the one input fills all
    columns).  A .1D input is time x features in the file; mri_read_1D returns it with nx = time points,
    ny = features, so ar[f*nt+t] is feature f at time t.  With -zcensor, the all-zero volumes are flagged
    per subject (s->zflag) and in d->zero_any. */
static void srm_data_read( const srm_opts *o , srm_data *d )
{
   srm_prog bar ; int ss , rr , ii ; THD_datatable *tab=d->tab ; int Tin=d->Tin ;
   srm_prog_begin(&bar,o->progress,o->quiet,"reading","subjects",d->nsub) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     srm_subj *s=d->sb+ss ;
     byte *zf = d->zero_any ? (byte *)calloc((size_t)Tin,1) : NULL ;
     s->x=(float *)malloc(sizeof(float)*(size_t)s->nvox_mask*(size_t)Tin) ;
     if( s->x==NULL ) ERROR_exit("cannot allocate %.2f GiB for Subj %s",4.0*s->nvox_mask*(double)Tin/GIB,s->label) ;
     for( rr=0 ; rr<(d->concat ? 1 : d->nrun) ; rr++ ){    /* -concat/Xmat: one input holds every run */
       const char *fn=DT_CELL(tab,s->row[rr],tab->icol_input) ;
       if( s->is1D ){
         MRI_IMAGE *im=mri_read_1D((char *)fn) , *fm ; float *ar ; int nt , f , t ;
         fm=mri_to_float(im) ; ar=MRI_FLOAT_PTR(fm) ; nt=fm->nx ;
         for( f=0 ; f<s->nvox_mask ; f++ ) for( t=0 ; t<nt ; t++ ){
           float v=ar[(size_t)f*nt+t] ;
           if( !isfinite(v) ) ERROR_exit("Subj %s: '%s' has a non-finite value at row %d, column %d",s->label,fn,t+1,f+1) ;
           s->x[(size_t)f*Tin+d->c0in[rr]+t]=v ;
         }
         if( zf!=NULL ) for( t=0 ; t<nt ; t++ ){ int nz=0 ; for( f=0 ; f<s->nvox_mask && !nz ; f++ ) nz=(ar[(size_t)f*nt+t]!=0.0f) ; zf[d->c0in[rr]+t]=(byte)!nz ; }
         if( fm!=im ) mri_free(fm) ;
         mri_free(im) ;
       } else {
         THD_3dim_dataset *ds=(rr==0) ? s->geom : THD_open_dataset((char *)fn) ; int badt ;
         if( ds==NULL ) ERROR_exit("cannot open '%s'",fn) ;
         if( srm_read_dset(ds,s->vox,s->nvox_mask,s->x,Tin,d->c0in[rr],zf,&badt)>0 )
           ERROR_exit("Subj %s: '%s' has non-finite values in the mask (first at volume %d)",s->label,fn,badt+1) ;
         if( rr>0 ) DSET_delete(ds) ;
       }
     }
     if( zf!=NULL ){
       for( ii=0 ; ii<Tin ; ii++ ) if( zf[ii] ){ d->nzero[ss]++ ; d->zero_any[ii]=1 ; }
       s->zflag=zf ;
     }
     { char extra[128] ; snprintf(extra,sizeof(extra),"Subj %s",s->label) ; srm_prog_set(&bar,ss+1,extra) ; }
   }
}

/*! Censored time points (cens[t] != 0) of each of nvox rows, filled by linear interpolation between the
    nearest uncensored points of the same run, or copied from the nearest one at a run's edge (as
    3dTproject -cenmode NTRP).  A run with nothing left is an error. */
static void srm_interp_censored( float *x , int nvox , int Tin , int nrun , const int *c0 , const int *len ,
                                 const byte *cens , const char *label , char * const *runname )
{
   int rr , f , t ;
   for( rr=0 ; rr<nrun ; rr++ ){
     /* prv[t] / nxt[t]: the nearest uncensored time point at or before / at or after t within this run
        (-1 if none).  A censored t between two kept points gets the straight-line value between them;
        at a run's edge (only one side exists) it copies the nearest kept value.  Never crosses runs. */
     int a=c0[rr] , L=len[rr] , *prv=(int *)malloc(sizeof(int)*(size_t)L) , *nxt=(int *)malloc(sizeof(int)*(size_t)L) , last=-1 , any=0 ;
     for( t=0 ; t<L ; t++ ){ if( !cens[a+t] ){ last=t ; any=1 ; } prv[t]=last ; }
     if( !any ) ERROR_exit("Subj %s: every time point of run %s is censored; there is nothing to interpolate from",label,runname[rr]) ;
     for( last=-1,t=L-1 ; t>=0 ; t-- ){ if( !cens[a+t] ) last=t ; nxt[t]=last ; }
     for( t=0 ; t<L ; t++ ) if( cens[a+t] ){
       int p=prv[t] , q=nxt[t] ;
       for( f=0 ; f<nvox ; f++ ){
         float *row=x+(size_t)f*Tin+a ;
         if( p>=0 && q>=0 ){ double w=(double)(t-p)/(double)(q-p) ; row[t]=(float)((1.0-w)*row[p]+w*row[q]) ; }
         else row[t]=row[(p>=0) ? p : q] ;
       }
     }
     free(prv) ; free(nxt) ;
   }
}

/*! Each subject's own censored time points: the Censor column (0 = censored), the Xmat's GoodList, and,
    with -zcensor, volumes that are all zero in the mask (afni_proc.py's errts zeroes censored volumes).
    -cenmode interp fills them in from the subject's neighbors in the same run; drop removes them for
    everyone (allow_drop 0: the timeline belongs to a saved model, so it cannot).
    The three sources are combined (censored if any says so) into cens[t] on the input timeline.  This
    runs BEFORE z-scoring, so the run means and SDs are computed from the filled-in series (interp) or the
    observed samples only (missing, in srm_normalize_obs).  -cenmode missing only records s->miss_in here;
    srm_data_timeline turns it into s->obs on the retained timeline. */
static void srm_data_subject_censor( const srm_opts *o , srm_data *d , int allow_drop )
{
   int ss , t , Tin=d->Tin , nsub_c=0 , tot=0 , lo=Tin , hi=0 ; byte *cens=(byte *)malloc((size_t)Tin) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     srm_subj *s=d->sb+ss ; int n=0 ;
     memset(cens,0,(size_t)Tin) ;
     if( s->zflag!=NULL ) for( t=0 ; t<Tin ; t++ ) if( s->zflag[t] ) cens[t]=1 ;
     if( s->censor_file!=NULL ){
       MRI_IMAGE *cim=mri_read_1D(s->censor_file) , *fim ; float *car ;
       if( cim==NULL ) ERROR_exit("Subj %s: cannot read Censor file '%s'",s->label,s->censor_file) ;
       fim=mri_to_float(cim) ; if( fim!=cim ) mri_free(cim) ;
       if( fim->nvox!=Tin ) ERROR_exit("Subj %s: Censor file '%s' has %d values; the subject's inputs have %d time points (all runs)",
                                       s->label,s->censor_file,fim->nvox,Tin) ;
       car=MRI_FLOAT_PTR(fim) ;
       for( t=0 ; t<Tin ; t++ ){
         if( !isfinite(car[t]) || (car[t]!=0.0f && car[t]!=1.0f) )
           ERROR_exit("Subj %s: Censor file '%s' value %d is %.7g; expected 0 (censored) or 1 (kept)",s->label,s->censor_file,t+1,car[t]) ;
         if( car[t]==0.0f ) cens[t]=1 ;
       }
       mri_free(fim) ;
     }
     if( s->good!=NULL ){
       byte *g=(byte *)calloc((size_t)Tin,1) ; int i ;
       for( i=0 ; i<s->ngood ; i++ ) g[s->good[i]]=1 ;
       for( t=0 ; t<Tin ; t++ ) if( !g[t] ) cens[t]=1 ;
       free(g) ;
     }
     for( t=0 ; t<Tin ; t++ ) n+=cens[t] ;
     s->ncens=n ;
     if( n==0 ) continue ;
     nsub_c++ ; tot+=n ; if( n<lo ) lo=n ; if( n>hi ) hi=n ;
     if( 100.0f*n/(float)Tin>o->zcensor_warn )
       WARNING_message("Subj %s has %d of %d time points (%.1f%%) censored; threshold is %.1f%% (-zcensor_warn)",
                       s->label,n,Tin,100.0f*n/(float)Tin,o->zcensor_warn) ;
     if( o->cenmode==SRM_CEN_INTERP )
       srm_interp_censored(s->x,s->nvox_mask,Tin,d->nrun,d->c0in,d->runlen,cens,s->label,d->runname) ;
     else if( o->cenmode==SRM_CEN_MISSING ){
       s->miss_in=(byte *)malloc((size_t)Tin) ; memcpy(s->miss_in,cens,(size_t)Tin) ;
     } else if( o->cenmode==SRM_CEN_DROP ){
       if( !allow_drop )
         ERROR_exit("Subj %s has censored time points, and -cenmode drop would change the saved model's timeline: use -cenmode interp",s->label) ;
       for( t=0 ; t<Tin ; t++ ) if( cens[t] ) d->keep[t]=0 ;
     }
   }
   free(cens) ;
   if( nsub_c==0 || o->quiet ) return ;
   INFO_message("censoring: %d of %d subjects have censored time points (%d to %d of %d each, %d in all)",
                nsub_c,d->nsub,lo,hi,Tin,tot) ;
   if( o->cenmode==SRM_CEN_INTERP )
     ININFO_message("filled in by linear interpolation within each run (-cenmode interp; drop removes them for everyone)") ;
   else if( o->cenmode==SRM_CEN_MISSING )
     ININFO_message("missing for that subject only (-cenmode missing): the fit and the scores use each subject where it was observed") ;
   else {
     int nk=0 ; for( t=0 ; t<Tin ; t++ ) nk+=(d->keep[t]==0) ;
     ININFO_message("dropped for everyone (-cenmode drop): %d of %d time points are censored in at least one subject",nk,Tin) ;
     if( nk>Tin/4 ) WARNING_message("-cenmode drop loses %.0f%% of the time points: every subject's censoring is lost by "
                                    "all. Consider -cenmode interp",100.0*nk/Tin) ;
   }
}

/*! Apply -zcensor and the censor mask: the retained timeline, its run lengths, packed data.
    Builds time_index[] / seglen[] from d->keep, checks every run keeps at least 3 points, makes each
    -cenmode missing subject's obs[] (1 = observed) on the retained timeline (NULL if nothing is missing),
    and repacks every s->x from Tin to T columns. */
static void srm_data_timeline( const srm_opts *o , srm_data *d , int Kmin )
{
   int ii , rr , ss ; int Tin=d->Tin ;
   (void)ss ;
   d->T=0 ;
   d->seglen=(int *)calloc((size_t)d->nrun,sizeof(int)) ; d->time_index=(int *)malloc(sizeof(int)*(size_t)Tin) ;
   for( rr=0 ; rr<d->nrun ; rr++ ) for( ii=d->c0in[rr] ; ii<d->c0in[rr]+d->runlen[rr] ; ii++ ) if( d->keep[ii] ){ d->time_index[d->T++]=ii ; d->seglen[rr]++ ; }
   for( rr=0 ; rr<d->nrun ; rr++ ) if( d->seglen[rr]<3 )
     ERROR_exit("run %d keeps only %d of %d time points after censoring; need at least 3",rr+1,d->seglen[rr],d->runlen[rr]) ;
   if( d->T<Kmin ) ERROR_exit("%d time points are left after censoring; -nfeatures is %d",d->T,Kmin) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){                   /* -cenmode missing: each subject's observed time points */
     srm_subj *s=d->sb+ss ; int t , c0=0 , any=0 ;
     if( s->miss_in==NULL ) continue ;
     s->obs=(byte *)malloc((size_t)d->T) ;
     for( t=0 ; t<d->T ; t++ ){ s->obs[t]=(byte)!s->miss_in[d->time_index[t]] ; any|=!s->obs[t] ; }
     if( !any ){ free(s->obs) ; s->obs=NULL ; continue ; }
     for( rr=0 ; rr<d->nrun ; c0+=d->seglen[rr],rr++ ){
       int n=0 ;
       for( t=c0 ; t<c0+d->seglen[rr] ; t++ ) n+=s->obs[t] ;
       if( n<3 ) ERROR_exit("Subj %s has %d observed time point%s in run %s; need at least 3 (-cenmode missing)",
                            s->label,n,n==1?"":"s",d->runname[rr]) ;
     }
   }
   if( d->T<Tin && !o->quiet ) INFO_message("%d of %d time points retained after censoring",d->T,Tin) ;
   if( d->T<Tin ){                                    /* pack the columns we keep */
     for( ss=0 ; ss<d->nsub ; ss++ ){
       srm_subj *s=d->sb+ss ;
       float *packed=(float *)malloc(sizeof(float)*(size_t)s->nvox_mask*(size_t)d->T) ; int f , t ;
       if( packed==NULL ) ERROR_exit("cannot allocate the censored data for Subj %s",s->label) ;
       for( f=0 ; f<s->nvox_mask ; f++ ){
         const float *src=s->x+(size_t)f*Tin ; float *dst=packed+(size_t)f*d->T ;
         for( t=0 ; t<d->T ; t++ ) dst[t]=src[d->time_index[t]] ;
       }
       free(s->x) ; s->x=packed ;
     }
   }
}

/*============================================================================
  -axis item: one table row per Subj x Item (x Run), each a single volume or a one-row .1D.
  Columns are keyed by Item, or Item:n for the n-th occurrence (in run order) of an item that a
  subject has more than once; every subject must have the same keys.  A subject's runs need not
  match anyone else's (randomized designs): they only group that subject's items for z-scoring
  and number the repeats.
==============================================================================*/

typedef struct { int row , run , pos ; const char *item ; } srm_irow ;

static int srm_irow_by_run( const void *a , const void *b )      /* run order, then table order */
{
   const srm_irow *x=(const srm_irow *)a , *y=(const srm_irow *)b ;
   if( x->run!=y->run ) return (x->run<y->run) ? -1 : 1 ;
   return (x->row<y->row) ? -1 : (x->row>y->row) ;
}

static int srm_irow_by_item( const void *a , const void *b )     /* item, then position in run order */
{
   const srm_irow *x=(const srm_irow *)a , *y=(const srm_irow *)b ; int c=strcmp(x->item,y->item) ;
   if( c!=0 ) return c ;
   return (x->pos<y->pos) ? -1 : (x->pos>y->pos) ;
}

typedef struct { const char *key ; int idx ; } srm_ikey ;

static int srm_ikey_cmp( const void *a , const void *b )
{
   return strcmp(((const srm_ikey *)a)->key,((const srm_ikey *)b)->key) ;
}

/*! -axis item from a design matrix: one table row per subject (Xmat, InputFile = the stats bucket) becomes
    one row per stimulus column of the Xmat (thd_xmat.h); the rest is the ordinary item axis. */
static THD_datatable * srm_xmat_items( const srm_opts *o , THD_datatable *tab )
{
   int have_run=0 ; THD_datatable *out ;
   out=THD_datatable_expand_xmat_items(tab,"Xmat",o->item_col,o->run_col,o->xmat_stims,&have_run) ;
   if( !o->quiet ){
     INFO_message("items from each subject's Xmat: %d row%s of the table become %d item rows (_Coef volumes of the stats bucket)%s",
                  tab->nrow,tab->nrow==1?"":"s",out->nrow,have_run?"; each item lies in one run, so the runs are known":"") ;
     ININFO_message("-show_table prints the table they make") ;
   }
   return out ;
}

/*! Subjects, runs, groups, masks, and the item keys of every subject, matched to the first
    subject's order.

    Keys.  Each table row is one (subject, item) measurement.  An item a subject has once is keyed by its
    label ("face_017"); an item measured several times gets one column per repeat, keyed "face_017:1",
    "face_017:2", ... numbered in run order then table order (or "face_017:R" from the Repeat column).
    The first subject's keys define the model's columns; every other subject's keys are looked up in a
    sorted copy (canon, bsearch) so column j means the same stimulus for everyone, whatever the row
    order.  A missing, extra, or differently repeated item is an ERROR_exit naming it.  s->irow[j] /
    s->irun[j] record the table row and run of the subject's item in model column j. */
static void srm_data_items( const srm_opts *o , THD_datatable *tab , srm_data *d , int min_subjects )
{
   int icol_item , icol_rep , r , ss , jj , nsub=0 , *subj_of , *run_of , nrep=0 , minrun=-1 ; char **slab=NULL ;
   srm_ikey *canon=NULL ;

   d->tab=tab ; d->axis_item=1 ;
   if( tab->icol_input<0 ) ERROR_exit("the data table needs an InputFile column") ;
   if( tab->icol_subj<0 ) ERROR_exit("-axis item needs a Subj column") ;
   icol_item=THD_datatable_column(tab,o->item_col) ;
   if( icol_item<0 ) ERROR_exit("-axis item needs an item column: the table has no '%s' column (see -item_column)",o->item_col) ;
   d->icol_run=THD_datatable_column(tab,o->run_col) ; d->have_run=(d->icol_run>=0) ;
   d->icol_mask=THD_datatable_column(tab,"Mask") ; d->icol_group=THD_datatable_column(tab,o->group_col) ;
   icol_rep=THD_datatable_column(tab,o->repeat_col) ;
   if( THD_datatable_column(tab,"Censor")>=0 || THD_datatable_column(tab,"Xmat")>=0 )
     ERROR_exit("Censor and Xmat columns are about time points; with -axis item, leave an item out by removing its rows") ;
   if( o->zscore==SRM_ZSCORE_RUN && !d->have_run )
     ERROR_exit("-zscore run works over the items of each run: the table has no '%s' column (the default on the item axis is "
                "-zscore all)",o->run_col) ;

   /* subjects and runs, in order of first appearance */
   subj_of=(int *)malloc(sizeof(int)*(size_t)tab->nrow) ; run_of=(int *)calloc((size_t)tab->nrow,sizeof(int)) ;
   for( r=0 ; r<tab->nrow ; r++ ){
     const char *sl=DT_CELL(tab,r,tab->icol_subj) , *il=DT_CELL(tab,r,icol_item) , *cc ;
     if( il[0]=='\0' ) ERROR_exit("row %d of the table has an empty %s",r+1,o->item_col) ;
     for( cc=il ; *cc!='\0' ; cc++ )
       if( isspace((unsigned char)*cc) || *cc==':' ) ERROR_exit("%s '%s' (row %d): item labels may not contain ':' or spaces",o->item_col,il,r+1) ;
     if( icol_rep>=0 ) for( cc=DT_CELL(tab,r,icol_rep) ; *cc!='\0' ; cc++ )
       if( isspace((unsigned char)*cc) || *cc==':' ) ERROR_exit("%s '%s' (row %d): repeat labels may not contain ':' or spaces",o->repeat_col,DT_CELL(tab,r,icol_rep),r+1) ;
     for( ss=0 ; ss<nsub ; ss++ ) if( strcmp(slab[ss],sl)==0 ) break ;
     if( ss==nsub ){ slab=(char **)realloc(slab,sizeof(char *)*(size_t)(nsub+1)) ; slab[nsub++]=strdup(sl) ; }
     subj_of[r]=ss ;
     if( d->have_run ){
       const char *rl=DT_CELL(tab,r,d->icol_run) ;
       for( jj=0 ; jj<d->nrun ; jj++ ) if( strcmp(d->runname[jj],rl)==0 ) break ;
       if( jj==d->nrun ){ d->runname=(char **)realloc(d->runname,sizeof(char *)*(size_t)(d->nrun+1)) ; d->runname[d->nrun++]=strdup(rl) ; }
       run_of[r]=jj ;
     }
   }
   if( !d->have_run ){ d->nrun=1 ; d->runname=(char **)malloc(sizeof(char *)) ; d->runname[0]=strdup("1") ; }
   d->nsub=nsub ;
   if( nsub<min_subjects ){
     if( min_subjects==2 ) ERROR_exit("SRM needs at least two subjects; the table has %d",nsub) ;
     ERROR_exit("the table has %d subject%s; need at least %d",nsub,nsub==1?"":"s",min_subjects) ;
   }

   d->sb=(srm_subj *)calloc((size_t)nsub,sizeof(srm_subj)) ;
   for( ss=0 ; ss<nsub ; ss++ ){
     srm_subj *s=d->sb+ss ; srm_irow *ir ; int n=0 , k , k0 , *seen=NULL , *perrun ;
     char **key ;
     s->label=slab[ss] ;
     for( r=0 ; r<tab->nrow ; r++ ) n+=(subj_of[r]==ss) ;
     ir=(srm_irow *)malloc(sizeof(srm_irow)*(size_t)n) ; key=(char **)calloc((size_t)n,sizeof(char *)) ;
     for( r=0,k=0 ; r<tab->nrow ; r++ ) if( subj_of[r]==ss ){ ir[k].row=r ; ir[k].run=run_of[r] ; ir[k].item=DT_CELL(tab,r,icol_item) ; k++ ; }
     qsort(ir,(size_t)n,sizeof(srm_irow),srm_irow_by_run) ;
     for( k=0 ; k<n ; k++ ) ir[k].pos=k ;          /* pos: the row's place in run order, kept through re-sorts */

     /* repeats: an item's occurrences are Item:1, Item:2, ... in run order, then table order (or
        Item:R, R from the repeat column) */
     qsort(ir,(size_t)n,sizeof(srm_irow),srm_irow_by_item) ;
     for( k0=0 ; k0<n ; ){
       int k1=k0+1 , q , q2 ;
       while( k1<n && strcmp(ir[k1].item,ir[k0].item)==0 ) k1++ ;
       if( k1-k0>1 && icol_rep>=0 )
         for( q=k0 ; q<k1 ; q++ ) for( q2=q+1 ; q2<k1 ; q2++ )
           if( strcmp(DT_CELL(tab,ir[q].row,icol_rep),DT_CELL(tab,ir[q2].row,icol_rep))==0 )
             ERROR_exit("Subj %s has %s %s with %s %s twice",s->label,o->item_col,ir[q].item,o->repeat_col,DT_CELL(tab,ir[q].row,icol_rep)) ;
       for( q=k0 ; q<k1 ; q++ ){
         char kb[THD_MAX_NAME] ;
         if( k1-k0>1 && icol_rep>=0 ) snprintf(kb,sizeof(kb),"%s:%s",ir[q].item,DT_CELL(tab,ir[q].row,icol_rep)) ;
         else if( k1-k0>1 )           snprintf(kb,sizeof(kb),"%s:%d",ir[q].item,q-k0+1) ;
         else                         snprintf(kb,sizeof(kb),"%s",ir[q].item) ;
         key[ir[q].pos]=strdup(kb) ;
       }
       k0=k1 ;
     }
     qsort(ir,(size_t)n,sizeof(srm_irow),srm_irow_by_run) ;        /* back to run order: ir[k] has key[k] */

     if( ss==0 ){                                                    /* the first subject sets the columns */
       d->nitem=n ; d->item_key=key ; key=NULL ;
       canon=(srm_ikey *)malloc(sizeof(srm_ikey)*(size_t)n) ;
       for( k=0 ; k<n ; k++ ){ canon[k].key=d->item_key[k] ; canon[k].idx=k ; if( strchr(d->item_key[k],':')!=NULL ) nrep++ ; }
       qsort(canon,(size_t)n,sizeof(srm_ikey),srm_ikey_cmp) ;
     }
     s->irow=(int *)malloc(sizeof(int)*(size_t)d->nitem) ; s->irun=(int *)malloc(sizeof(int)*(size_t)d->nitem) ;
     seen=(int *)calloc((size_t)d->nitem,sizeof(int)) ;
     for( k=0 ; k<n ; k++ ){
       srm_ikey want , *hit ; int jc ;
       want.key=(ss==0) ? d->item_key[k] : key[k] ;
       hit=(srm_ikey *)bsearch(&want,canon,(size_t)d->nitem,sizeof(srm_ikey),srm_ikey_cmp) ;
       if( hit==NULL ){
         int mine=0 , theirs=0 , q ; size_t L=strlen(ir[k].item) ;
         for( q=0 ; q<n ; q++ ) mine+=(strcmp(ir[q].item,ir[k].item)==0) ;
         for( q=0 ; q<d->nitem ; q++ )
           theirs+=(strncmp(d->item_key[q],ir[k].item,L)==0 && (d->item_key[q][L]=='\0' || d->item_key[q][L]==':')) ;
         if( theirs==0 ) ERROR_exit("Subj %s has %s %s, which Subj %s does not have",s->label,o->item_col,ir[k].item,d->sb[0].label) ;
         ERROR_exit("Subj %s has %s %s %d time%s, Subj %s has it %d time%s: a repeated item must recur the same "
                    "number of times for every subject",s->label,o->item_col,ir[k].item,mine,mine==1?"":"s",
                    d->sb[0].label,theirs,theirs==1?"":"s") ;
       }
       jc=hit->idx ; seen[jc]=1 ; s->irow[jc]=ir[k].row ; s->irun[jc]=ir[k].run ;
     }
     for( k=0 ; k<d->nitem ; k++ ) if( !seen[k] )
       ERROR_exit("Subj %s is missing %s %s, which Subj %s has%s",s->label,o->item_col,d->item_key[k],d->sb[0].label,
                  strchr(d->item_key[k],':')!=NULL ? " (an item numbered :n recurs; every subject needs the same number of repeats)" : "") ;
     if( key!=NULL ){ for( k=0 ; k<n ; k++ ) free(key[k]) ; free(key) ; }
     free(seen) ; free(ir) ;

     /* the subject's own runs: z-scoring works over the items of each one */
     perrun=(int *)calloc((size_t)d->nrun,sizeof(int)) ;
     for( k=0 ; k<d->nitem ; k++ ) perrun[s->irun[k]]++ ;
     for( k=0 ; k<d->nrun ; k++ ) if( perrun[k]>0 ){
       if( o->zscore==SRM_ZSCORE_RUN && perrun[k]<3 )
         ERROR_exit("Subj %s has %d item%s in run %s: z-scoring within a run needs at least 3 (or use -zscore none)",
                    s->label,perrun[k],perrun[k]==1?"":"s",d->runname[k]) ;
       if( minrun<0 || perrun[k]<minrun ) minrun=perrun[k] ;
     }
     free(perrun) ;

     /* one Mask and one Group per subject */
     s->nrun=1 ; s->row=(int *)malloc(sizeof(int)) ; s->row[0]=s->irow[0] ;
     if( d->icol_mask>=0 ){
       const char *mn=DT_CELL(tab,s->irow[0],d->icol_mask) ;
       for( k=1 ; k<d->nitem ; k++ ) if( strcmp(DT_CELL(tab,s->irow[k],d->icol_mask),mn)!=0 )
         ERROR_exit("Subj %s has different Mask entries on different rows",s->label) ;
       if( mn[0]!='\0' && strcmp(mn,"-")!=0 && strcmp(mn,"NA")!=0 ) s->mask_name=strdup(mn) ;
     }
     if( d->icol_group>=0 ){
       const char *gn=DT_CELL(tab,s->irow[0],d->icol_group) ;
       for( k=1 ; k<d->nitem ; k++ ) if( strcmp(DT_CELL(tab,s->irow[k],d->icol_group),gn)!=0 )
         ERROR_exit("Subj %s has different %s entries on different rows",s->label,o->group_col) ;
       for( jj=0 ; jj<d->ngroup ; jj++ ) if( strcmp(d->glabel[jj],gn)==0 ) break ;
       if( jj==d->ngroup ){ d->glabel=(char **)realloc(d->glabel,sizeof(char *)*(size_t)(d->ngroup+1)) ; d->glabel[d->ngroup++]=strdup(gn) ; }
       s->group=jj ;
     }
   }
   if( d->ngroup==0 ){ d->glabel=(char **)malloc(sizeof(char *)) ; d->glabel[0]=strdup("All") ; d->ngroup=1 ; }
   if( !o->quiet ){
     INFO_message("%d subjects, %d group%s, %d item columns each%s",nsub,d->ngroup,d->ngroup>1?"s":"",d->nitem,
                  nrep>0 ? " (counting each repeat of an item as its own column)" : "") ;
     if( nrep>0 ) ININFO_message("%d columns are repeats, keyed Item:%s",nrep,
                                 icol_rep>=0 ? "R by the repeat column" : "n in run order, then table order") ;
     if( d->have_run ) ININFO_message("%d run%s; %s",d->nrun,d->nrun>1?"s":"",
                                      o->zscore==SRM_ZSCORE_RUN ? "each subject's items are z-scored within their own runs"
                                                                : "they order repeats (and can be the -cv folds)") ;
     ININFO_message("-zscore %s",o->zscore==SRM_ZSCORE_ALL ? "all: each feature over all of the subject's items"
                                 : o->zscore==SRM_ZSCORE_RUN ? "run: each feature over the items of each of the subject's runs"
                                 : "none: the betas as given (centered by the fit)") ;
     if( d->ngroup>1 ){ for( jj=0 ; jj<d->ngroup ; jj++ ){ int cnt=0 ; for( ss=0 ; ss<nsub ; ss++ ) cnt+=(d->sb[ss].group==jj) ;
         ININFO_message("group %s: %d subjects",d->glabel[jj],cnt) ; } }
   }
   if( o->zscore==SRM_ZSCORE_RUN && minrun>=0 && minrun<10 && !o->quiet )
     WARNING_message("a run has only %d items: z-scores over so few items are noisy; consider fewer, larger runs, or -zscore none",minrun) ;
   free(subj_of) ; free(run_of) ; free(slab) ; free(canon) ;
}

/*! Open every item's input once per file (header only): grids, masks, and the sub-brick each
    item selects.  A selector like betas+tlrc'[face#0_Coef]' must pick exactly one volume. */
static void srm_data_shape_items( const srm_opts *o , srm_data *d , int need_mask , int Kmin )
{
   int ss , j , f , ii , n=d->nitem ; THD_datatable *tab=d->tab ;
   if( need_mask && o->maskname!=NULL ) d->gmask=srm_mask_get(&d->mcache,&d->nmcache,o->maskname) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     srm_subj *s=d->sb+ss ;
     s->ifile=(int *)malloc(sizeof(int)*(size_t)n) ; s->ibrick=(int *)calloc((size_t)n,sizeof(int)) ;
     for( j=0 ; j<n ; j++ ){
       const char *fn=DT_CELL(tab,s->irow[j],tab->icol_input) ; int is1=srm_is1D(fn) ;
       if( j==0 ) s->is1D=is1 ;
       else if( is1!=s->is1D ) ERROR_exit("Subj %s mixes .1D files and datasets",s->label) ;
       if( is1 ){
         MRI_IMAGE *im=mri_read_1D((char *)fn) ;
         if( im==NULL ) ERROR_exit("cannot read 1D file '%s' (Subj %s, item %s)",fn,s->label,d->item_key[j]) ;
         if( im->nx!=1 )
           ERROR_exit("Subj %s, item %s: '%s' has %d rows; with -axis item a .1D input is one row (the item's features)",
                      s->label,d->item_key[j],fn,im->nx) ;
         if( j==0 ){
           s->nvox_mask=im->ny ;
           if( need_mask ){ s->vox=(int *)malloc(sizeof(int)*(size_t)im->ny) ; for( ii=0 ; ii<im->ny ; ii++ ) s->vox[ii]=ii ; }
         } else if( im->ny!=s->nvox_mask )
           ERROR_exit("Subj %s, item %s: '%s' has %d columns; the first item had %d",s->label,d->item_key[j],fn,im->ny,s->nvox_mask) ;
         mri_free(im) ; s->ifile[j]=-1 ;
       } else {
         char base[THD_MAX_NAME] ; const char *sel=strchr(fn,'[') ; size_t len=sel!=NULL ? (size_t)(sel-fn) : strlen(fn) ;
         if( strchr(fn,'<')!=NULL || strchr(fn,'{')!=NULL )
           ERROR_exit("Subj %s, item %s: '%s': select the item's volume with [..]; <..> and {..} selectors are not used with -axis item",
                      s->label,d->item_key[j],fn) ;
         if( len>=sizeof(base) ) ERROR_exit("input name too long: '%s'",fn) ;
         memcpy(base,fn,len) ; base[len]='\0' ;
         for( f=0 ; f<s->nfile ; f++ ) if( strcmp(s->file[f],base)==0 ) break ;
         if( f==s->nfile ){ s->file=(char **)realloc(s->file,sizeof(char *)*(size_t)(s->nfile+1)) ; s->file[s->nfile++]=strdup(base) ; }
         s->ifile[j]=f ;
       }
     }
     for( f=0 ; f<s->nfile ; f++ ){
       THD_3dim_dataset *ds=THD_open_dataset(s->file[f]) ;
       CHECK_OPEN_ERROR(ds,s->file[f]) ;
       if( f==0 ){
         s->geom=ds ;
         if( need_mask ){
           mask_entry *me ;
           if( s->mask_name!=NULL ) me=srm_mask_get(&d->mcache,&d->nmcache,s->mask_name) ;
           else if( d->gmask!=NULL ) me=d->gmask ;
           else ERROR_exit("Subj %s: datasets need a mask: give -mask or a Mask column in the table",s->label) ;
           if( !EQUIV_GRIDS(ds,me->ds) ) ERROR_exit("Subj %s: '%s' is not on the grid of mask '%s'",s->label,s->file[f],me->name) ;
           s->nvox_mask=me->nmask ;
           s->vox=(int *)malloc(sizeof(int)*(size_t)me->nmask) ;
           for( j=0,ii=0 ; j<me->nvox ; j++ ) if( me->mask[j] ) s->vox[ii++]=j ;
         }
       } else if( !EQUIV_GRIDS(ds,s->geom) )
         ERROR_exit("Subj %s: '%s' is not on the grid of '%s'",s->label,s->file[f],s->file[0]) ;
       for( j=0 ; j<n ; j++ ) if( s->ifile[j]==f ){
         const char *fn=DT_CELL(tab,s->irow[j],tab->icol_input) , *sel=strchr(fn,'[') ;
         if( sel!=NULL ){
           int *iv=MCW_get_thd_intlist(ds,(char *)sel) ;
           if( iv==NULL || iv[0]!=1 )
             ERROR_exit("Subj %s, item %s: '%s' selects %d volumes; each item needs exactly one",
                        s->label,d->item_key[j],fn,iv==NULL ? 0 : iv[0]) ;
           s->ibrick[j]=iv[1] ; free(iv) ;
         } else if( DSET_NVALS(ds)!=1 )
           ERROR_exit("Subj %s, item %s: '%s' has %d volumes; select the item's one, as in %s'[3]'",
                      s->label,d->item_key[j],fn,DSET_NVALS(ds),fn) ;
       }
       if( f>0 ) DSET_delete(ds) ;
     }
     if( need_mask && s->nvox_mask<Kmin ) ERROR_exit("Subj %s has %d features; -nfeatures is %d",s->label,s->nvox_mask,Kmin) ;
   }
   d->Tin=n ;
}

/*! Read every item, one load per file.  A non-finite value, or an item that is zero everywhere
    in the mask (a failed or missing estimate, usually), is an error naming the Subj and item.
    s->x is nvox_mask x nitem, column j = model item j (sub-brick s->ibrick[j] of file s->ifile[j]). */
static void srm_data_read_items( const srm_opts *o , srm_data *d )
{
   srm_prog bar ; int ss , j , f , v , n=d->nitem , done=0 ; THD_datatable *tab=d->tab ; char extra[200] ;
   srm_prog_begin(&bar,o->progress,o->quiet,"reading","items",d->nsub*n) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     srm_subj *s=d->sb+ss ;
     s->x=(float *)malloc(sizeof(float)*(size_t)s->nvox_mask*(size_t)n) ;
     if( s->x==NULL ) ERROR_exit("cannot allocate %.2f GiB for Subj %s",4.0*s->nvox_mask*(double)n/GIB,s->label) ;
     if( s->is1D ){
       for( j=0 ; j<n ; j++ ){
         const char *fn=DT_CELL(tab,s->irow[j],tab->icol_input) ;
         MRI_IMAGE *im=mri_read_1D((char *)fn) , *fm ; float *ar ; int nz=0 ;
         if( im==NULL ) ERROR_exit("cannot read 1D file '%s' (Subj %s, item %s)",fn,s->label,d->item_key[j]) ;
         fm=mri_to_float(im) ; ar=MRI_FLOAT_PTR(fm) ;
         for( v=0 ; v<s->nvox_mask ; v++ ){
           float a=ar[v] ;
           if( !isfinite(a) ) ERROR_exit("Subj %s, item %s: '%s' has a non-finite value in column %d",s->label,d->item_key[j],fn,v+1) ;
           if( a!=0.0f ) nz=1 ;
           s->x[(size_t)v*n+j]=a ;
         }
         if( fm!=im ) mri_free(fm) ;
         mri_free(im) ;
         if( !nz ) ERROR_exit("Subj %s, item %s: '%s' is all zero (a failed or missing estimate?)",s->label,d->item_key[j],fn) ;
         snprintf(extra,sizeof(extra),"Subj %s (%d/%d), %s",s->label,ss+1,d->nsub,THD_trailname((char *)fn,0)) ;
         srm_prog_set(&bar,++done,extra) ;
       }
     } else for( f=0 ; f<s->nfile ; f++ ){
       THD_3dim_dataset *ds=THD_open_dataset(s->file[f]) ;
       CHECK_OPEN_ERROR(ds,s->file[f]) ;
       DSET_load(ds) ; CHECK_LOAD_ERROR(ds) ;
       for( j=0 ; j<n ; j++ ) if( s->ifile[j]==f ){
         MRI_IMAGE *fim=THD_extract_float_brick(s->ibrick[j],ds) ; float *far ; int nz=0 ;
         if( fim==NULL ) ERROR_exit("cannot read volume %d of '%s' (Subj %s, item %s)",s->ibrick[j],s->file[f],s->label,d->item_key[j]) ;
         far=MRI_FLOAT_PTR(fim) ;
         for( v=0 ; v<s->nvox_mask ; v++ ){
           float a=far[s->vox[v]] ;
           if( !isfinite(a) ) ERROR_exit("Subj %s, item %s: volume %d of '%s' has non-finite values in the mask",s->label,d->item_key[j],s->ibrick[j],s->file[f]) ;
           if( a!=0.0f ) nz=1 ;
           s->x[(size_t)v*n+j]=a ;
         }
         mri_free(fim) ;
         if( !nz ) ERROR_exit("Subj %s, item %s: volume %d of '%s' is all zero in the mask (a failed or missing estimate?)",
                              s->label,d->item_key[j],s->ibrick[j],s->file[f]) ;
         snprintf(extra,sizeof(extra),"Subj %s (%d/%d), %s",s->label,ss+1,d->nsub,THD_trailname(s->file[f],0)) ;
         srm_prog_set(&bar,++done,extra) ;
       }
       DSET_delete(ds) ;
     }
   }
}

/*! The item "timeline": every column kept, in key order. */
static void srm_data_items_timeline( srm_data *d )
{
   int j ;
   d->T=d->nitem ;
   d->keep=(byte *)malloc((size_t)d->T) ; memset(d->keep,1,(size_t)d->T) ;
   d->time_index=(int *)malloc(sizeof(int)*(size_t)d->T) ;
   for( j=0 ; j<d->T ; j++ ) d->time_index[j]=j ;
}

/*! -axis item: z-score every feature over all of the subject's items (-zscore all), or over the
    items of each of the subject's own runs (-zscore run); with -zscore none, only check for features
    that never vary.  A run's items need not be adjacent columns, so they are gathered by run, done
    as segments, and put back.
    perm[] lists the columns run by run (perm[q] = original column), seg[] the number of items per run;
    y = x with columns permuted, z-scored as contiguous segments, then scattered back through perm. */
static int srm_item_normalize( srm_subj *s , int nitem , int nrun , int zscore , unsigned char *bad )
{
   int *perm=(int *)malloc(sizeof(int)*(size_t)nitem) , *seg=(int *)calloc((size_t)nrun+1,sizeof(int)) ;
   int nseg=0 , rr , j , q=0 , f , nb ; float *y ;
   if( zscore==SRM_ZSCORE_RUN ){
     for( rr=0 ; rr<nrun ; rr++ ){
       int c=0 ;
       for( j=0 ; j<nitem ; j++ ) if( s->irun[j]==rr ){ perm[q++]=j ; c++ ; }
       if( c>0 ) seg[nseg++]=c ;
     }
   } else {
     for( j=0 ; j<nitem ; j++ ) perm[j]=j ;
     seg[0]=nitem ; nseg=1 ;
   }
   y=(float *)malloc(sizeof(float)*(size_t)s->nvox_mask*(size_t)nitem) ;
   if( y==NULL ) ERROR_exit("out of memory z-scoring Subj %s",s->label) ;
   for( f=0 ; f<s->nvox_mask ; f++ ){
     const float *xr=s->x+(size_t)f*nitem ; float *yr=y+(size_t)f*nitem ;
     for( j=0 ; j<nitem ; j++ ) yr[j]=xr[perm[j]] ;
   }
   if( zscore!=SRM_ZSCORE_NONE ){
     nb=SRM_zscore_segments(y,s->nvox_mask,nitem,nseg,seg,bad) ;
     if( nb<0 ) ERROR_exit("Subj %s: %s",s->label,SRM_strerror(nb)) ;
   } else nb=srm_flag_constant(y,s->nvox_mask,nitem,nseg,seg,bad) ;
   for( f=0 ; f<s->nvox_mask ; f++ ){
     float *xr=s->x+(size_t)f*nitem ; const float *yr=y+(size_t)f*nitem ;
     for( j=0 ; j<nitem ; j++ ) xr[perm[j]]=yr[j] ;
   }
   free(y) ; free(perm) ; free(seg) ;
   return nb ;
}

/*! Folds that differ between subjects: fold q of subject ss holds out that subject's items whose fold
    column is level q (levels in order of first appearance).  -cv loso: the group fit without the subject
    sees every item; -cv run: each refit sees every item, the subject's own held-out ones missing for it. */
static void srm_make_item_folds_own( const srm_opts *o , const srm_data *d , srm_folds *f , int icol , const char *fc , int loso )
{
   THD_datatable *tab=d->tab ; int ss , j , q , n=d->nitem ;
   for( ss=0 ; ss<d->nsub ; ss++ ) for( j=0 ; j<n ; j++ ){
     const char *v=DT_CELL(tab,d->sb[ss].irow[j],icol) ;
     for( q=0 ; q<f->nfold ; q++ ) if( strcmp(f->name[q],v)==0 ) break ;
     if( q==f->nfold ){ f->name=(char **)realloc(f->name,sizeof(char *)*(size_t)(f->nfold+1)) ; f->name[f->nfold++]=strdup(v) ; }
   }
   if( f->nfold<2 ) ERROR_exit("'%s' has one level (%s); -cv needs at least 2 folds",fc,f->name[0]) ;
   f->scol=(int ***)calloc((size_t)d->nsub+1,sizeof(int **)) ; f->slen=(int **)calloc((size_t)d->nsub+1,sizeof(int *)) ;
   f->len=(int *)calloc((size_t)f->nfold,sizeof(int)) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     f->scol[ss]=(int **)malloc(sizeof(int *)*(size_t)f->nfold) ; f->slen[ss]=(int *)calloc((size_t)f->nfold,sizeof(int)) ;
     for( q=0 ; q<f->nfold ; q++ ){
       int c=0 ;
       f->scol[ss][q]=(int *)malloc(sizeof(int)*(size_t)n) ;
       for( j=0 ; j<n ; j++ ) if( strcmp(DT_CELL(tab,d->sb[ss].irow[j],icol),f->name[q])==0 ) f->scol[ss][q][c++]=j ;
       f->slen[ss][q]=c ;
       if( c<3 ) ERROR_exit("Subj %s has %d item%s in fold %s (%s); each subject's folds need at least 3",
                            d->sb[ss].label,c,c==1?"":"s",f->name[q],fc) ;
       if( c>f->len[q] ) f->len[q]=c ;                 /* the largest, for the checks on what is left to fit */
     }
   }
   if( !o->quiet ){
     INFO_message("-cv %s: '%s' differs between subjects (randomized), so each subject's folds are their own items in each "
                  "of its %d levels",loso==1?"loso":"run",fc,f->nfold) ;
     if( loso==2 ) ININFO_message("the model's S is fixed; each subject's W is estimated without their own items of each level") ;
     else if( loso ) ININFO_message("the group fit without the subject sees every item") ;
     else       ININFO_message("each refit sees every item; a subject's own held-out items are missing for that subject only") ;
   }
}

/*! Folds of the item axis: the levels of -fold_column (the Run column by default), in order of first
    appearance.  loso is 1 for -cv loso, 2 for -cv run against a fixed model (-model_in), else 0.  A fold is a set of item columns, the same for every subject, so the column must give
    an item the same value in every subject; runs do that only when the run assignment was not
    randomized. */
static void srm_make_item_folds( const srm_opts *o , const srm_data *d , srm_folds *f , int loso )
{
   THD_datatable *tab=d->tab ; const char *fc=(o->fold_col!=NULL) ? o->fold_col : o->run_col ;
   int icol=THD_datatable_column(tab,(char *)fc) , j , ss , q , n=d->nitem , *lev ;
   memset(f,0,sizeof(*f)) ; f->items=1 ; f->unit="items" ;
   if( icol<0 ){
     if( o->fold_col!=NULL ) ERROR_exit("-fold_column: the table has no '%s' column",fc) ;
     ERROR_exit("-cv with -axis item needs folds: a '%s' column (when every subject has the same items in each run), "
                "or -fold_column",o->run_col) ;
   }
   /* -cv loso fits the group to everyone else on every item, so each subject's folds can be their own
      (their own runs, when run order was randomized); -cv run refits everyone without a fold, so a fold
      must be the same items for all */
   for( j=0 ; j<n ; j++ ) for( ss=1 ; ss<d->nsub ; ss++ ){
     const char *v0=DT_CELL(tab,d->sb[0].irow[j],icol) , *v=DT_CELL(tab,d->sb[ss].irow[j],icol) ;
     if( strcmp(v,v0)==0 ) continue ;
     srm_make_item_folds_own(o,d,f,icol,fc,loso) ; return ;
   }
   lev=(int *)malloc(sizeof(int)*(size_t)n) ;
   for( j=0 ; j<n ; j++ ){
     const char *v0=DT_CELL(tab,d->sb[0].irow[j],icol) ;
     for( q=0 ; q<f->nfold ; q++ ) if( strcmp(f->name[q],v0)==0 ) break ;
     if( q==f->nfold ){ f->name=(char **)realloc(f->name,sizeof(char *)*(size_t)(f->nfold+1)) ; f->name[f->nfold++]=strdup(v0) ; }
     lev[j]=q ;
   }
   if( f->nfold<2 ) ERROR_exit("%s '%s' has one level (%s); -cv needs at least 2 folds",
                               o->fold_col!=NULL?"-fold_column":"the run column",fc,f->name[0]) ;
   { char **names=f->name; int nf=f->nfold;
     srm_common_folds(f,n,lev,names,nf);
     for(q=0;q<nf;q++)free(names[q]);
     free(names);
   }
   free(lev) ;
}

/*! -cenmode missing: z-score each feature over its observed samples, within each run (or over all
    of them, or with -zscore none only centered there), as SRM_zscore_segments does; missing
    samples become 0.  A feature with no variance in its observed samples of some run is bad.
    Zero at the missing samples is what the library expects (they are never read, and a zero column adds
    nothing to W'X or X S').  Note: with -zscore none, this centers within each run, whereas the ordinary
    path leaves -zscore none data as given (the fit removes one overall mean). */
static int srm_normalize_obs( srm_subj *s , int T , int nrun , const int *seglen , int zscore , unsigned char *bad )
{
   int f , nb=0 ;
   for( f=0 ; f<s->nvox_mask ; f++ ){
     float *row=s->x+(size_t)f*T ; int rr , c0=0 , isb=0 , t , nseg=(zscore==SRM_ZSCORE_ALL) ? 1 : nrun ;
     for( rr=0 ; rr<nseg ; rr++ ){
       int L=(zscore==SRM_ZSCORE_ALL) ? T : seglen[rr] , n=0 ; double mean=0.0 , var=0.0 , sd ;
       for( t=c0 ; t<c0+L ; t++ ) if( s->obs[t] ){ mean+=row[t] ; n++ ; }
       mean/=n ;
       for( t=c0 ; t<c0+L ; t++ ) if( s->obs[t] ){ double dv=row[t]-mean ; var+=dv*dv ; }
       sd=sqrt(var/n) ;
       if( sd==0.0 || sd<=1.0e-7*fabs(mean) ) isb=1 ;
       for( t=c0 ; t<c0+L ; t++ )
         row[t] = !s->obs[t] ? 0.0f : (zscore==SRM_ZSCORE_NONE || isb) ? (float)(row[t]-mean) : (float)((row[t]-mean)/sd) ;
       c0+=L ;
     }
     if( isb ){ memset(row,0,sizeof(float)*(size_t)T) ; nb++ ; }
     bad[f]=(unsigned char)isb ;
   }
   return nb ;
}

/*! z-score within runs and deal with features that never vary: drop them (fitting), or zero them
    and keep their place (applying a saved W, whose rows are fixed).
    Why z-score per run: scanner drift and run-to-run baseline shifts change each voxel's mean and scale
    between runs; normalizing within a run removes that, and uses only that run's own samples (so with
    -zscore run a held-out run's normalization never borrows from the training runs; -zscore all, the item
    axis default, normalizes over all of a subject's items, held-out folds included, an unsupervised step
    that uses no labels).  Why drop constant features: a
    voxel with zero variance (outside the brain, or zero-filled) carries no information and cannot be
    z-scored. */
static void srm_data_normalize( const srm_opts *o , srm_data *d , int zscore , int Kmin , int keep_rows )
{
   int ss , total_drop=0 ; srm_prog bar ; char extra[160] ;
   srm_prog_begin(&bar,o->progress,o->quiet,zscore!=SRM_ZSCORE_NONE ? "z-score" : "checking","subjects",d->nsub) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     srm_subj *s=d->sb+ss ;
     unsigned char *bad=(unsigned char *)calloc((size_t)s->nvox_mask,1) ; int nb ;
     if( d->axis_item ) nb=srm_item_normalize(s,d->T,d->nrun,zscore,bad) ;
     else if( s->obs!=NULL ) nb=srm_normalize_obs(s,d->T,d->nrun,d->seglen,zscore,bad) ;
     else if( zscore!=SRM_ZSCORE_NONE ){
       int one=d->T ;
       nb = (zscore==SRM_ZSCORE_ALL) ? SRM_zscore_segments(s->x,s->nvox_mask,d->T,1,&one,bad)
                                     : SRM_zscore_segments(s->x,s->nvox_mask,d->T,d->nrun,d->seglen,bad) ;
       if( nb<0 ) ERROR_exit("Subj %s: %s",s->label,SRM_strerror(nb)) ;
     } else nb=srm_flag_constant(s->x,s->nvox_mask,d->T,d->nrun,d->seglen,bad) ;
     if( keep_rows ){
       int f ;
       for( f=0 ; f<s->nvox_mask ; f++ ) if( bad[f] ) memset(s->x+(size_t)f*d->T,0,sizeof(float)*(size_t)d->T) ;
       s->nvox=s->nvox_mask ; s->ndrop=nb ; total_drop+=nb ;
     } else {
       s->nvox=srm_compact_rows(s->x,s->nvox_mask,d->T,bad,s->vox) ;
       s->ndrop=s->nvox_mask-s->nvox ; total_drop+=s->ndrop ;
       if( s->nvox<Kmin ) ERROR_exit("Subj %s has %d features with any variance; -nfeatures is %d",s->label,s->nvox,Kmin) ;
       if( s->nvox<s->nvox_mask ){
         float *shrunk=(float *)realloc(s->x,sizeof(float)*(size_t)s->nvox*(size_t)d->T) ;
         if( shrunk!=NULL ) s->x=shrunk ;
       }
     }
     free(bad) ;
     snprintf(extra,sizeof(extra),"Subj %s, %d feature%s without variance so far",s->label,total_drop,total_drop==1?"":"s") ;
     srm_prog_set(&bar,ss+1,extra) ;
   }
   if( !o->quiet && total_drop>0 ){
     if( keep_rows ) INFO_message("%d features have no variance in some run of the new data; they are zeroed",total_drop) ;
     else INFO_message("%d feature%s with no variance in some run %s dropped (see %s.fit.txt)",total_drop,
                       total_drop==1?"":"s",total_drop==1?"was":"were",o->prefix) ;
   }
}

/*============================================================================
  Pieces shared by fitting, -model_in, and -apply_W
==============================================================================*/

/*! Rows of a kept-feature matrix (nkept x ncol) put back on the input's own columns,
    zeros for the features that were dropped. */
static float * srm_expand_rows( const float *a , int nkept , int ncol , const int *vox , int nfull )
{
   float *y=(float *)calloc((size_t)nfull*(size_t)ncol,sizeof(float)) ; int f ;
   if( y==NULL ) ERROR_exit("out of memory writing a matrix (%.2f GiB)",4.0*nfull*(double)ncol/GIB) ;
   for( f=0 ; f<nkept ; f++ ) memcpy(y+(size_t)vox[f]*ncol,a+(size_t)f*ncol,sizeof(float)*(size_t)ncol) ;
   return y ;
}

/*! A 1D comment: head, then the nkey item keys (the rows, in order) on a '# items:' line.  Free it. */
static char * srm_keys_comment( const char *head , char * const *key , int nkey )
{
   size_t len=strlen(head)+16 ; int j ; char *c ;
   for( j=0 ; j<nkey ; j++ ) len+=strlen(key[j])+1 ;
   c=(char *)malloc(len) ;
   strcpy(c,head) ; strcat(c,"\n# items:") ;
   for( j=0 ; j<nkey ; j++ ){ strcat(c," ") ; strcat(c,key[j]) ; }
   return c ;
}

/*! One subject's series in the shared space, K x T: a k x 1 x 1 dataset, or a text file.
    The dataset form treats the K components as K "voxels" on a dummy 1 mm grid, with one volume per
    time point (a time series with the input's TR) or per item (a bucket labelled by item key), so that
    3dInSync and 3dRSA can read it like any dataset. */
static void srm_write_proj( const srm_opts *o , const srm_subj *s , const float *P , int K , int T ,
                            const char *comment , char * const *collab , int argc , char **argv )
{
   char sn[128] , fn[THD_MAX_NAME] ; srm_safe(sn,sizeof(sn),s->label) ;
   if( o->proj_dset ){
     THD_3dim_dataset *out=EDIT_empty_copy(NULL) ; THD_ivec3 ixyz ; THD_fvec3 dxyz , oxyz ; float TR=1.0f ; int t ;
     AFNI_setenv("AFNI_WRITE_1D_AS_PREFIX=YES") ;   /* a k x 1 x 1 dataset is otherwise written as a 1D text file */
     if( s->geom!=NULL && DSET_TR(s->geom)>0.0f ) TR=DSET_TR(s->geom) ;
     LOAD_IVEC3(ixyz,K,1,1) ; LOAD_FVEC3(dxyz,1.0,1.0,1.0) ; LOAD_FVEC3(oxyz,0.0,0.0,0.0) ;
     snprintf(fn,sizeof(fn),"%s.proj.%s",o->prefix,sn) ;
     if( collab!=NULL )                             /* items: a bucket, each volume labelled with its key */
       EDIT_dset_items(out,ADN_nxyz,ixyz,ADN_xyzdel,dxyz,ADN_xyzorg,oxyz,ADN_prefix,fn,ADN_nvals,T,ADN_ntt,0,
                       ADN_datum_all,MRI_float,ADN_none) ;
     else
       EDIT_dset_items(out,ADN_nxyz,ixyz,ADN_xyzdel,dxyz,ADN_xyzorg,oxyz,ADN_prefix,fn,ADN_nvals,T,ADN_ntt,T,
                       ADN_ttdel,TR,ADN_ttorg,0.0,ADN_tunits,UNITS_SEC_TYPE,ADN_datum_all,MRI_float,ADN_none) ;
     tross_Make_History("3dSRM",argc,argv,out) ;
     for( t=0 ; t<T ; t++ ){
       float *col=(float *)malloc(sizeof(float)*(size_t)K) ; int c ;
       for( c=0 ; c<K ; c++ ) col[c]=P[(size_t)c*T+t] ;
       EDIT_substitute_brick(out,t,MRI_float,col) ;
       if( collab!=NULL ) EDIT_BRICK_LABEL(out,t,collab[t]) ;
     }
     DSET_write(out) ; DSET_delete(out) ;
   } else {
     snprintf(fn,sizeof(fn),"%s.proj.%s.1D",o->prefix,sn) ;
     if( collab!=NULL ){
       char *ck=srm_keys_comment(comment,collab,T) ;
       srm_write_transposed_1D(fn,P,K,T,ck) ; free(ck) ;
     } else srm_write_transposed_1D(fn,P,K,T,comment) ;
   }
}

/*! The outputs that exist for every subject: the data in the shared space (held out, when res
    has them), W, voxelwise held-out R^2, and the reconstruction.  Subject ss is view voff+ss of m.
    Maps are written back on the subject's own grid: row f of W (feature f) goes to voxel s->vox[f], and
    voxels outside the mask, or dropped as constant, are 0. */
static void srm_write_subject_outputs( const srm_opts *o , const srm_data *d , const SRM_model *m , int voff ,
                                       const srm_cvres *res , int K , int argc , char **argv )
{
   srm_prog pbar ; int nwork=(!o->no_projected)+(o->save_W)+(o->save_recon)+(o->save_r2) , ss , rc , T=d->T ;
   srm_prog_begin(&pbar,o->progress,o->quiet,"writing","subjects",nwork>0 ? d->nsub : 0) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     const srm_subj *s=d->sb+ss ; char sn[128] , fn[THD_MAX_NAME] ; srm_safe(sn,sizeof(sn),s->label) ;
     if( !o->no_projected ){
       float *P=(float *)malloc(sizeof(float)*(size_t)K*(size_t)T) ; int held=(res!=NULL && res->proj!=NULL) ;
       if( held ) memcpy(P,res->proj[ss],sizeof(float)*(size_t)K*(size_t)T) ;
       else {
         rc=SRM_project(m,voff+ss,s->x,T,P) ;
         if( rc!=SRM_OK ) ERROR_exit("Subj %s: projection failed: %s",s->label,SRM_strerror(rc)) ;
       }
       if( d->axis_item )
         srm_write_proj(o,s,P,K,T,held ? "subject's HELD-OUT items in the shared space (each from a model that never saw it), aligned to the full fit: one row per item, one column per component"
                                       : "subject's items in the shared space: one row per item, one column per component",d->item_key,argc,argv) ;
       else
         srm_write_proj(o,s,P,K,T,held ? "subject's HELD-OUT data in the shared space (each stretch from a model that never saw it), aligned to the full fit: one row per retained time point, one column per component"
                                       : "subject's data in the shared space: one row per retained time point, one column per component",NULL,argc,argv) ;
       free(P) ;
     }
     if( o->save_W ){
       if( s->is1D ){
         float *Wf=srm_expand_rows(m->w[voff+ss],s->nvox,K,s->vox,s->nvox_mask) ;
         snprintf(fn,sizeof(fn),"%s.W.%s.1D",o->prefix,sn) ;
         srm_write_matrix_1D(fn,Wf,s->nvox_mask,K,"mapping W: one row per feature (column of the input; zeros for dropped features), one column per component") ;
         free(Wf) ;
       } else {
         int nvg=DSET_NVOX(s->geom) , c , f ; float **vols=(float **)malloc(sizeof(float *)*(size_t)K) ;
         for( c=0 ; c<K ; c++ ){
           vols[c]=(float *)calloc((size_t)nvg,sizeof(float)) ;
           for( f=0 ; f<s->nvox ; f++ ) vols[c][s->vox[f]]=m->w[voff+ss][(size_t)f*K+c] ;
         }
         snprintf(fn,sizeof(fn),"%s.W.%s",o->prefix,sn) ;
         srm_write_grid_dset(s->geom,fn,K,vols,"comp",argc,argv) ;
         free(vols) ;
       }
     }
     if( o->save_r2 && res!=NULL && res->vr2!=NULL ){
       if( s->is1D ){
         float *rf=srm_expand_rows(res->vr2[ss],s->nvox,1,s->vox,s->nvox_mask) ;
         snprintf(fn,sizeof(fn),"%s.r2.%s.1D",o->prefix,sn) ;
         srm_write_matrix_1D(fn,rf,s->nvox_mask,1,"held-out R^2 of each feature, the mean over folds (0 for dropped features)") ;
         free(rf) ;
       } else {
         int nvg=DSET_NVOX(s->geom) , f ; float **vols=(float **)malloc(sizeof(float *)) ;
         vols[0]=(float *)calloc((size_t)nvg,sizeof(float)) ;
         for( f=0 ; f<s->nvox ; f++ ) vols[0][s->vox[f]]=res->vr2[ss][f] ;
         snprintf(fn,sizeof(fn),"%s.r2.%s",o->prefix,sn) ;
         srm_write_grid_dset(s->geom,fn,1,vols,"r2_",argc,argv) ;
         free(vols) ;
       }
     }
     if( o->save_recon ){
       float *xh=(float *)malloc(sizeof(float)*(size_t)s->nvox*(size_t)T) ;
       rc=SRM_reconstruct(m,voff+ss,m->s,T,xh) ;
       if( rc!=SRM_OK ) ERROR_exit("Subj %s: reconstruction failed: %s",s->label,SRM_strerror(rc)) ;
       if( s->is1D ){
         float *xf=srm_expand_rows(xh,s->nvox,T,s->vox,s->nvox_mask) ;
         snprintf(fn,sizeof(fn),"%s.recon.%s.1D",o->prefix,sn) ;
         srm_write_transposed_1D(fn,xf,s->nvox_mask,T,"W S: one row per retained time point, one column per feature (zeros for dropped features)") ;
         free(xf) ;
       } else {
         int nvg=DSET_NVOX(s->geom) , t , f ; float **vols=(float **)malloc(sizeof(float *)*(size_t)T) ;
         THD_3dim_dataset *out ;
         for( t=0 ; t<T ; t++ ){
           vols[t]=(float *)calloc((size_t)nvg,sizeof(float)) ;
           for( f=0 ; f<s->nvox ; f++ ) vols[t][s->vox[f]]=xh[(size_t)f*T+t] ;
         }
         snprintf(fn,sizeof(fn),"%s.recon.%s",o->prefix,sn) ;
         out=EDIT_empty_copy(s->geom) ;
         EDIT_dset_items(out,ADN_prefix,fn,ADN_nvals,T,ADN_ntt,T,ADN_datum_all,MRI_float,ADN_brick_fac,NULL,ADN_none) ;
         tross_Copy_History(s->geom,out) ; tross_Make_History("3dSRM",argc,argv,out) ;
         for( t=0 ; t<T ; t++ ) EDIT_substitute_brick(out,t,MRI_float,vols[t]) ;
         DSET_write(out) ; DSET_delete(out) ; free(vols) ;
       }
       free(xh) ;
     }
     srm_prog_set(&pbar,ss+1,NULL) ;
   }
}

/*! Refuse to overwrite any per-subject output, before the work is done. */
static void srm_check_subject_outputs( const srm_opts *o , const srm_data *d )
{
   char fn[THD_MAX_NAME] ; int ss ;
   if( THD_ok_overwrite() ) return ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     const srm_subj *s=d->sb+ss ; char sn[128] ; srm_safe(sn,sizeof(sn),s->label) ;
     if( !o->no_projected ){
       if( o->proj_dset ){ snprintf(fn,sizeof(fn),"%s.proj.%s",o->prefix,sn) ; if( srm_dset_exists(fn) ) ERROR_exit("a dataset '%s' already exists (use -overwrite)",fn) ; }
       else { snprintf(fn,sizeof(fn),"%s.proj.%s.1D",o->prefix,sn) ; if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ; }
     }
     if( o->save_W ){ snprintf(fn,sizeof(fn),s->is1D?"%s.W.%s.1D":"%s.W.%s",o->prefix,sn) ;
       if( s->is1D ? THD_is_file(fn) : srm_dset_exists(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ; }
     if( o->save_r2 ){ snprintf(fn,sizeof(fn),s->is1D?"%s.r2.%s.1D":"%s.r2.%s",o->prefix,sn) ;
       if( s->is1D ? THD_is_file(fn) : srm_dset_exists(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ; }
     if( o->save_recon ){ snprintf(fn,sizeof(fn),s->is1D?"%s.recon.%s.1D":"%s.recon.%s",o->prefix,sn) ;
       if( s->is1D ? THD_is_file(fn) : srm_dset_exists(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ; }
   }
}

/*! ERROR_exit if PREFIX+suffix exists and -overwrite was not given. */
static void srm_check_new_file( const srm_opts *o , const char *suffix )
{
   char fn[THD_MAX_NAME] ;
   if( THD_ok_overwrite() ) return ;
   snprintf(fn,sizeof(fn),"%s%s",o->prefix,suffix) ;
   if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ;
}

/*! Peak bookkeeping for a common fold plan, including constructor scratch.
    Use the input timeline as an upper bound before censoring/repeat averaging.
    Subject-specific item folds still use their legacy representation. */
static double srm_common_fold_bytes( const srm_opts *o , const srm_data *d )
{
   int nf=0,j,q,ss; double names=0.0,T=d->Tin;
   if(d->axis_item){
      const char *fc=o->fold_col ? o->fold_col : o->run_col;
      int ic=THD_datatable_column(d->tab,(char *)fc);
      const char **labels;
      if(ic<0 || d->nsub<1)return 0.0; /* input validation reports this later */
      for(j=0;j<d->nitem;j++)for(ss=1;ss<d->nsub;ss++)
         if(strcmp(DT_CELL(d->tab,d->sb[0].irow[j],ic),DT_CELL(d->tab,d->sb[ss].irow[j],ic)))return 0.0;
      labels=(const char **)malloc(sizeof(char *)*(size_t)d->nitem);
      if(!labels)ERROR_exit("out of memory counting SRM folds");
      for(j=0;j<d->nitem;j++){
         const char *v=DT_CELL(d->tab,d->sb[0].irow[j],ic);
         for(q=0;q<nf;q++)if(!strcmp(labels[q],v))break;
         if(q==nf){labels[nf++]=v;names+=strlen(v)+1.0;}
      }
      free(labels);T=d->nitem;
   }else{
      nf=d->nrun>=2 ? d->nrun : o->cv_blocks;
      /* Repeat averaging can change runs into blocks. Bound both counts. */
      if(o->repeated_stimulus && o->cv_blocks>nf)nf=o->cv_blocks;
      names=24.0*nf;
      if(d->runname)for(j=0;j<d->nrun;j++)names+=strlen(d->runname[j])+1.0;
   }
   return sizeof(THD_foldplan)+sizeof(int)*T*(nf+5.0)
          +nf*(4.0*sizeof(int)+4.0*sizeof(void *))+2.0*names+64.0;
}

/*! The memory contract, before the big allocations.
    The estimate has two parts.  input: the data themselves (4 bytes x voxels x Tin summed over subjects),
    plus the larger of (a) the fold copies -cv run makes of every subject's training and test columns for
    the largest fold, (T - shortest run + longest run)/T of the data, and (b) one input being read on its
    full grid.  With -compress the data are factors of at most T x T per subject (doubled under -cv for the
    fold copies), plus one subject held whole while it is read and its T x T Gram matrices.  shared: what
    the library needs for the fit (SRM_estimate_bytes), plus W per run with -run_coupling and a second model
    with -nstart.  THD_memory_plan_finish adds them up; the default limit is 80% of physical memory. */
static void srm_memory_preflight( const srm_opts *o , const srm_data *d , int K , int do_cv , int nsegfit )
{
   THD_memory_plan me ; double inb=0.0 , Wb=0.0 , onemax=0.0 , cvb=0.0 , rdb=0.0 ;
   int *nv0=(int *)malloc(sizeof(int)*(size_t)d->nsub) , ss , rr ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     const srm_subj *s=d->sb+ss ; double b=4.0*s->nvox_mask*(double)d->Tin ;
     nv0[ss]=s->nvox_mask ; inb+=b ; Wb+=4.0*s->nvox_mask*K ; if( b>onemax ) onemax=b ;
     if( !s->is1D && s->geom!=NULL ){                /* reading: one input whole, on its own grid, as floats */
       double g=4.0*DSET_NVOX(s->geom)*(double)(d->axis_item ? 1 : (d->concat ? d->Tin : 0)) ;
       if( !d->axis_item && !d->concat ) for( rr=0 ; rr<d->nrun ; rr++ ){ double gr=4.0*DSET_NVOX(s->geom)*(double)d->runlen[rr] ; if( gr>g ) g=gr ; }
       if( d->axis_item ) g=4.0*DSET_NVOX(s->geom)*(double)DSET_NVALS(s->geom) ;
       if( g>rdb ) rdb=g ;
     }
   }
   /* cross-validation copies: -cv run over runs holds every subject's training and test columns of the largest
      fold, (T - shortest run) + longest run; blocks and item folds about one copy; -cv loso and -model_in one
      subject at a time */
   if( do_cv ){
     if( strcasecmp(o->cvmode,"loso")==0 || o->model_in!=NULL ) cvb=onemax ;
     else if( !d->axis_item && d->nrun>1 && d->runlen!=NULL ){
       int mn=d->runlen[0] , mx=d->runlen[0] ;
       for( rr=1 ; rr<d->nrun ; rr++ ){ if( d->runlen[rr]<mn ) mn=d->runlen[rr] ; if( d->runlen[rr]>mx ) mx=d->runlen[rr] ; }
       cvb=inb*(double)(d->Tin-mn+mx)/(double)d->Tin ;
       if( o->cv_adapt>0.0 ) cvb+=inb*(double)mx/(double)d->Tin ;   /* -cv_adapt: the scored rest of a run, again */
     } else cvb=inb ;
   }
   memset(&me,0,sizeof(me)) ;
   me.input=inb+(cvb>rdb ? cvb : rdb) ;      /* the data, plus the larger of the fold copies and one input being read */
   if( o->compress ){                        /* factors of at most T x T each, plus one subject in full while it is read */
     double Tn=(double)d->Tin , rb=4.0*Tn*Tn*d->nsub ;
     me.input = rb*(do_cv ? 2.0 : 1.0) + 2.0*onemax + rdb + 8.0*Tn*Tn*(srm_omp_threads()+2.0) + 4.0*(onemax/(4.0*Tn))*K ;
     if( o->save_r2 && do_cv ){                /* -save_r2: one fold's held-out columns on the voxels, and its W */
       int mx=d->Tin ; if( !d->axis_item && d->nrun>1 && d->runlen!=NULL ){ mx=0 ; for( rr=0 ; rr<d->nrun ; rr++ ) if( d->runlen[rr]>mx ) mx=d->runlen[rr] ; }
       me.input += onemax*(double)mx/Tn + 2.0*(onemax/(4.0*Tn))*4.0*K ;
     }
     Wb=4.0*Tn*K*d->nsub ;
     for( ss=0 ; ss<d->nsub ; ss++ ) nv0[ss]=(d->sb[ss].nvox_mask<d->Tin) ? d->sb[ss].nvox_mask : d->Tin ;
   }
   me.shared=SRM_estimate_bytes(d->nsub,nv0,d->Tin,K,o->par.algo,0)
            + Wb*(nsegfit>0 ? (double)nsegfit : 0.0) + (o->nstart>1 ? Wb : 0.0) ;
   if( do_cv && !strcasecmp(o->cvmode,"loso") ){
     double state=12.0*K*d->Tin + 4.0*K*K + 4.0*d->nsub ; /* S, a_sum, Sigma, counts */
     double model=Wb*(1.0+nsegfit) + (o->par.center ? Wb/K : 0.0) + state ;
     int retained=(o->cv_fit!=NULL && strcasecmp(o->cv_fit,"pooled")) ? 2 : 1 ;
     /* Pooled model, possibly a subset model, and either the legacy copy or
        a cold coupled fit's shared-W initialization remain live during fitting. */
     me.shared+=retained*model ;
     if( o->loso_legacy ) me.shared+=model ;
     else if( nsegfit>0 ) me.shared+=Wb+(o->par.center ? Wb/K : 0.0)+state ;
     /* The ordinary estimate already includes W for the best multistart fit. */
     if( !o->loso_legacy && o->nstart>1 ) me.shared+=model-Wb ;
   }
   if(do_cv)me.shared+=srm_common_fold_bytes(o,d);
   me.system=(double)AFNI_get_memsize() ;
   me.limit=o->memory_limit_given ? o->memory_limit_gib*GIB : ((me.system>0.0) ? 0.80*me.system : 0.0) ;
   me.nthread=1 ; THD_memory_plan_finish(&me) ;
   if( !o->quiet && (me.total>0.50*me.system || o->memory_limit_given) ){
     if( me.limit>0.0 )
       INFO_message("memory preflight: about %.2f GiB peak (%.2f GiB of data + %.2f GiB for the fit); limit %.2f GiB",
                    me.total/GIB,me.input/GIB,me.shared/GIB,me.limit/GIB) ;
     else INFO_message("memory preflight: about %.2f GiB peak; physical memory is unavailable",me.total/GIB) ;
   }
   if( me.limit>0.0 && me.total>me.limit ){
     if( !o->memory_override )
       ERROR_exit("estimated peak %.2f GiB exceeds the %.2f GiB limit: use fewer voxels or subjects, a smaller -nfeatures, "
                  "set -memory_limit, or acknowledge with -memory_override",me.total/GIB,me.limit/GIB) ;
     WARNING_message("estimated peak exceeds the memory limit; continuing because -memory_override was given") ;
   }
   free(nv0) ;
}

/*! The numeric table columns that travel with the scores (age, motion, ...). */
static int * srm_carry_columns( const THD_datatable *tab , const srm_data *d , int *ncarry )
{
   int *carry=NULL , jj ;
   *ncarry=0 ;
   for( jj=0 ; jj<tab->ncol ; jj++ )
     if( tab->isnum[jj] && jj!=tab->icol_subj && jj!=tab->icol_input && jj!=d->icol_run && jj!=d->icol_mask && jj!=d->icol_group ){
       if( d->axis_item ){                         /* a column that varies over a subject's items is about items */
         int ss , k , same=1 ;
         for( ss=0 ; ss<d->nsub && same ; ss++ ) for( k=1 ; k<d->nitem && same ; k++ )
           same=(strcmp(DT_CELL(tab,d->sb[ss].irow[k],jj),DT_CELL(tab,d->sb[ss].irow[0],jj))==0) ;
         if( !same ) continue ;
       }
       carry=(int *)realloc(carry,sizeof(int)*(size_t)(*ncarry+1)) ; carry[(*ncarry)++]=jj ;
     }
   return carry ;
}

/*! "a,b,c" for messages, truncated with "..." to fit nd bytes. */
static void srm_int_list( char *dst , size_t nd , const int *a , int n )
{
   size_t pos=0 ; int ii ;
   dst[0]='\0' ;
   for( ii=0 ; ii<n && pos+12<nd ; ii++ ) pos+=snprintf(dst+pos,nd-pos,"%s%d",ii?",":"",a[ii]) ;
   if( ii<n && nd>4 ) snprintf(dst+nd-4,4,"...") ;
}

/*============================================================================
  -reliability within, and -repeated_stimulus: a subject's own reliability, from repeats
==============================================================================*/

/*! The split-half reliability of a subject's data between two halves xa, xb (V x n), each correlation corrected to
    the whole by Spearman-Brown.  NaN where it cannot be estimated (fewer than 3 samples for the series). */
static void srm_rel_halves( const float *xa , const float *xb , int V , int n , double *out )
{
   double rp=NAN , rs=NAN ;
   out[0]=out[1]=NAN ;
   if( n<1 || V<3 ) return ;
   if( SRM_split_reliability(xa,xb,V,n,&rp,(n>=3)?&rs:NULL)!=SRM_OK ) return ;
   if( isfinite(rp) ) out[0]=SRM_spearman_brown(rp,2.0) ;
   if( n>=3 && isfinite(rs) ) out[1]=SRM_spearman_brown(rs,2.0) ;
}

/*! -repeated_stimulus: every run shows the same stimulus.  First each subject's reliability between the mean of the
    odd runs and the mean of the even ones (if asked); then the runs are averaged, and the fit sees one run.  The
    runs must keep the same time points (the same censoring in each).
    Run rr occupies retained columns rr*L .. rr*L+L-1 (every run has L).  Each time point of the averaged
    run is the mean over the runs in which the subject was observed there (cnt[t] of them); with
    -reliability within, the odd-numbered and even-numbered runs are first averaged separately and the two
    halves correlated (srm_rel_halves, Spearman-Brown corrected to the full set).  Afterwards the data
    have one run of L columns. */
static void srm_average_repeats( const srm_opts *o , srm_data *d )
{
   int L=d->seglen[0] , rr , ss , t , f , nmiss=0 ;
   if( d->nrun<2 ) ERROR_exit("-repeated_stimulus needs at least two runs of the stimulus; the data have %d",d->nrun) ;
   for( rr=1 ; rr<d->nrun ; rr++ ){
     if( d->seglen[rr]!=L ) ERROR_exit("-repeated_stimulus: run %s keeps %d time points, run %s %d; every repeat must keep the same ones",
                                       d->runname[rr],d->seglen[rr],d->runname[0],L) ;
     for( t=0 ; t<L ; t++ )
       if( d->time_index[rr*L+t]-d->c0in[rr] != d->time_index[t]-d->c0in[0] )
         ERROR_exit("-repeated_stimulus: runs %s and %s keep different time points (censoring); every repeat must keep the same ones",
                    d->runname[0],d->runname[rr]) ;
   }
   /* the timeline before averaging, for -score_by (labels must agree across the repeats) */
   d->rep_nrun=d->nrun ; d->rep_T=d->T ;
   d->rep_seglen=(int *)malloc(sizeof(int)*(size_t)d->nrun) ; memcpy(d->rep_seglen,d->seglen,sizeof(int)*(size_t)d->nrun) ;
   d->rep_time_index=(int *)malloc(sizeof(int)*(size_t)d->T) ; memcpy(d->rep_time_index,d->time_index,sizeof(int)*(size_t)d->T) ;
   if( o->rel_within ) d->relpre=(double *)malloc(sizeof(double)*2*(size_t)d->nsub) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     srm_subj *s=d->sb+ss ; int V=s->nvox , T=d->T ;
     float *avg=(float *)calloc((size_t)V*L,sizeof(float)) ;
     int *cnt=(int *)malloc(sizeof(int)*(size_t)L) ;          /* runs observed at each time point (-cenmode missing) */
     if( avg==NULL ) ERROR_exit("out of memory averaging the repeats of Subj %s",s->label) ;
     for( t=0 ; t<L ; t++ ){ cnt[t]=0 ; for( rr=0 ; rr<d->nrun ; rr++ ) cnt[t]+=(s->obs==NULL || s->obs[rr*L+t]) ; }
     if( o->rel_within ){                              /* odd runs against even runs, then corrected to all */
       float *xa=(float *)calloc((size_t)V*L,sizeof(float)) , *xb=(float *)calloc((size_t)V*L,sizeof(float)) ;
       int *na=(int *)calloc((size_t)L,sizeof(int)) , *nb=(int *)calloc((size_t)L,sizeof(int)) , *idx=(int *)malloc(sizeof(int)*(size_t)L) , ng=0 ;
       for( rr=0 ; rr<d->nrun ; rr++ ){
         float *h = (rr%2==0) ? xa : xb ; int *nh = (rr%2==0) ? na : nb ;
         for( t=0 ; t<L ; t++ ) if( s->obs==NULL || s->obs[rr*L+t] ){
           nh[t]++ ;
           for( f=0 ; f<V ; f++ ) h[(size_t)f*L+t]+=s->x[(size_t)f*T+rr*L+t] ;
         }
       }
       for( t=0 ; t<L ; t++ ) if( na[t]>0 && nb[t]>0 ) idx[ng++]=t ;   /* where both halves were observed */
       for( f=0 ; f<V ; f++ ) for( t=0 ; t<ng ; t++ ){
         xa[(size_t)f*ng+t]=xa[(size_t)f*L+idx[t]]/na[idx[t]] ; xb[(size_t)f*ng+t]=xb[(size_t)f*L+idx[t]]/nb[idx[t]] ;
       }
       if( ng>=3 ) srm_rel_halves(xa,xb,V,ng,d->relpre+2*ss) ;
       else d->relpre[2*ss]=d->relpre[2*ss+1]=NAN ;
       free(xa) ; free(xb) ; free(na) ; free(nb) ; free(idx) ;
     }
     for( rr=0 ; rr<d->nrun ; rr++ ) for( t=0 ; t<L ; t++ ) if( cnt[t]>0 && (s->obs==NULL || s->obs[rr*L+t]) )
       for( f=0 ; f<V ; f++ ) avg[(size_t)f*L+t]+=s->x[(size_t)f*T+rr*L+t]/(float)cnt[t] ;
     free(s->x) ; s->x=avg ;
     if( s->obs!=NULL ){                               /* missing only where every repeat was */
       byte *ob=(byte *)malloc((size_t)L) ; int no=0 , any=0 ;
       for( t=0 ; t<L ; t++ ){ ob[t]=(byte)(cnt[t]>0) ; no+=ob[t] ; any|=!ob[t] ; }
       free(s->obs) ; s->obs=NULL ;
       if( no<3 ) ERROR_exit("-repeated_stimulus: Subj %s is observed at %d time points of the averaged run; need at least 3",s->label,no) ;
       if( any ){ s->obs=ob ; nmiss++ ; } else free(ob) ;
     }
     free(cnt) ;
   }
   if( !o->quiet ){
     INFO_message("-repeated_stimulus: the %d runs (%d time points each) are averaged for the fit",d->nrun,L) ;
     if( o->cenmode==SRM_CEN_MISSING )
       ININFO_message("each time point over the runs where the subject was observed; %d subject%s still missing somewhere "
                      "(missing in every run)",nmiss,nmiss==1?" is":"s are") ;
   }
   d->T=L ; d->nrun=1 ; d->seglen[0]=L ;
}

/*! One subject's data stage, on a one-subject view d1 of d: read, censor, retained timeline, z-score (and,
    with -repeated_stimulus, the average of the repeats).  On return s->x is that subject's nvox x d1->T data. */
static void srm_subject_stage( const srm_opts *oq , srm_data *d , srm_data *d1 , int ss , int K )
{
   *d1=*d ; d1->nsub=1 ; d1->sb=d->sb+ss ; d1->nzero=(d->nzero!=NULL) ? d->nzero+ss : NULL ; d1->relpre=NULL ;
   d1->seglen=NULL ; d1->time_index=NULL ; d1->rep_nrun=0 ; d1->rep_seglen=NULL ; d1->rep_time_index=NULL ;
   if( d->axis_item ){
     d1->keep=NULL ;
     srm_data_read_items(oq,d1) ;
     srm_data_items_timeline(d1) ;
     if( d1->T<K ) ERROR_exit("%d items; -nfeatures is %d",d1->T,K) ;
   } else {
     srm_data_read(oq,d1) ;
     srm_data_subject_censor(oq,d1,1) ;
     srm_data_timeline(oq,d1,K) ;
   }
   srm_data_normalize(oq,d1,oq->zscore,K,0) ;
   if( oq->repeated_stimulus ){
     srm_average_repeats(oq,d1) ;
     if( d1->T<K ) ERROR_exit("%d time points per run; -nfeatures is %d",d1->T,K) ;
   }
}

/*! -compress (srm_data_compress_stream, below): each subject in turn is read, censored, z-scored (its repeats averaged), then replaced by R
    (rank x T, R'R = X'X) and, for the fit's start, the random start carried into R; its voxels are freed
    before the next subject is read, so the data are never all in memory at once.  The timeline and runs
    are the first subject's (every subject's are the same).

    Why R'R = X'X is enough (thd_srm.h, "Compressed views"): everything the fit and the scores do with X
    goes through inner products of its columns (X'X) or through W, and W always lies in the span of X's
    columns.  Writing X = U R with U orthonormal (voxels x rank), a fit on R with ndim = voxels gives
    exactly the fit on X with W = U W_R: same S, rho2, objective, projections.  Column subsets (folds) of
    R keep the property, since R[:,c]'R[:,c] = X[:,c]'X[:,c].  Maps on the voxels (-save_W, -save_recon,
    -save_r2) need U, so those subjects are read again at the end (srm_write_outputs_reread).

    The start: with -nstart 1, SRM_compress_ex also makes U'W0 for the random W0 that the uncompressed fit
    would draw for this subject (stream ss), so the compressed fit begins at the same point. */
/*! The repeated items (keys Item:n), grouped: nb groups, group q's columns cols[q][0..ncol[q]-1], first[q] its
    first.  Free with srm_rep_groups_free. */
typedef struct { int nb , *first , **cols , *ncol ; } srm_repg ;

static srm_repg srm_rep_groups( const srm_data *d )
{
   int n=d->nitem , j , q ; srm_repg g ;
   g.nb=0 ; g.first=(int *)malloc(sizeof(int)*(size_t)n) ; g.cols=(int **)malloc(sizeof(int *)*(size_t)n) ;
   g.ncol=(int *)calloc((size_t)n,sizeof(int)) ;
   for( j=0 ; j<n ; j++ ){                              /* the repeated items, in key order */
     const char *c=strchr(d->item_key[j],':') ; size_t L ;
     if( c==NULL ) continue ;
     L=(size_t)(c-d->item_key[j]) ;
     for( q=0 ; q<g.nb ; q++ ) if( !strncmp(d->item_key[g.first[q]],d->item_key[j],L) && d->item_key[g.first[q]][L]==':' ) break ;
     if( q==g.nb ){ g.first[g.nb]=j ; g.cols[g.nb]=(int *)malloc(sizeof(int)*(size_t)n) ; g.nb++ ; }
     g.cols[q][g.ncol[q]++]=j ;
   }
   if( g.nb==0 ) ERROR_exit("-reliability within needs repeats: no item recurs (an item measured twice, keyed Item:1, Item:2)") ;
   return g ;
}

static void srm_rep_groups_free( srm_repg *g )
{
   int q ; for( q=0 ; q<g->nb ; q++ ) free(g->cols[q]) ;
   free(g->cols) ; free(g->ncol) ; free(g->first) ;
}

static void srm_reliability_items_one( const srm_data *d , srm_ctx *cx , const srm_repg *g , int ss ) ;

static void srm_data_compress_stream( const srm_opts *o , srm_data *d , int K , srm_ctx *lc )
{
   /* see the -compress comment above srm_repg.  -cenmode drop needs everyone's censoring before anyone's
      timeline is fixed, so that case reads the censor information first. */
   srm_opts oq=*o ; srm_prog bar ; int ss , rmin=1<<30 , rmax=0 , ncs=0 , ncens=0 , ndrop=0 ; double full=0.0 , kept=0.0 ;
   srm_data first ;                                   /* the first subject's timeline, put into d after the loop */
   oq.quiet=1 ; memset(&first,0,sizeof(first)) ;
   if( o->cenmode==SRM_CEN_DROP && !d->axis_item ){   /* the timeline must be known first */
     if( o->zcensor ){                                  /* zero volumes drop for everyone: a first read finds them */
       srm_prog pb ;
       if( !o->quiet ) INFO_message("-compress -cenmode drop -zcensor: every subject is read once more, first, to find "
                                    "the all-zero volumes that are dropped for everyone") ;
       srm_prog_begin(&pb,o->progress,o->quiet,"zero volumes","subjects",d->nsub) ;
       for( ss=0 ; ss<d->nsub ; ss++ ){
         srm_data d1=*d ;
         d1.nsub=1 ; d1.sb=d->sb+ss ; d1.nzero=(d->nzero!=NULL) ? d->nzero+ss : NULL ;
         srm_data_read(&oq,&d1) ;                       /* sets the subject's zero flags */
         free(d->sb[ss].x) ; d->sb[ss].x=NULL ;
         if( d->nzero!=NULL ) d->nzero[ss]=0 ;
         srm_prog_set(&pb,ss+1,NULL) ;
       }
       srm_prog_end(&pb) ;
     }
     srm_data_subject_censor(&oq,d,1) ;                /* the Censor files, Xmat GoodLists, and zero volumes */
   }
   if( o->repeated_stimulus && o->rel_within ) d->relpre=(double *)malloc(sizeof(double)*2*(size_t)d->nsub) ;
   srm_prog_begin(&bar,o->progress,o->quiet,"reading and compressing","subjects",d->nsub) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     srm_data d1 ; srm_subj *s=d->sb+ss ; float *R=NULL ; int r=0 , rc ; char extra[160] ;
     s->nvox_mask0=s->nvox_mask ;
     if( s->vox!=NULL ){ s->vox0=(int *)malloc(sizeof(int)*(size_t)s->nvox_mask) ; memcpy(s->vox0,s->vox,sizeof(int)*(size_t)s->nvox_mask) ; }
     free(s->zflag) ; s->zflag=NULL ;                    /* made again by this read */
     srm_subject_stage(&oq,d,&d1,ss,K) ;
     if( lc!=NULL ){                                     /* item reliability needs the voxels: now, before compressing */
       srm_repg g=srm_rep_groups(&d1) ;
       srm_reliability_items_one(&d1,lc,&g,ss) ;
       srm_rep_groups_free(&g) ;
     }
     if( s->ncens>0 ){ ncs++ ; ncens+=s->ncens ; }
     ndrop+=s->ndrop ;
     if( o->nstart==1 && s->nvox>=K ){                  /* R, and the start the uncompressed fit would make, in one pass */
       s->w0=(float *)malloc(sizeof(float)*(size_t)d1.T*K) ;
       rc=SRM_compress_ex(s->x,s->nvox,d1.T,0.0,K,o->par.seed,ss,&R,&r,s->w0) ;
     } else
       rc=SRM_compress(s->x,s->nvox,d1.T,0.0,&R,&r) ;
     if( rc!=SRM_OK ) ERROR_exit("Subj %s: could not compress the data: %s",s->label,SRM_strerror(rc)) ;
     if( r<K ) ERROR_exit("Subj %s: the data have rank %d after z-scoring; -nfeatures is %d",s->label,r,K) ;
     /* from here on s->x is R (r x T), s->nvox the rank, s->ndim the voxel count it stands for */
     if( s->w0!=NULL ){ float *w=(float *)realloc(s->w0,sizeof(float)*(size_t)r*K) ; if( w!=NULL ) s->w0=w ; }
     s->ndim=s->nvox ; s->w0K=K ;
     full+=4.0*s->nvox*(double)d1.T ; kept+=4.0*r*(double)d1.T ;
     free(s->x) ; s->x=R ; s->nvox=r ; s->compressed=1 ;
     if( r<rmin ) rmin=r ;
     if( r>rmax ) rmax=r ;
     if( ss==0 ) first=d1 ;                             /* the timeline, from the first subject */
     else {
       if( d1.T!=first.T ) ERROR_exit("internal error: Subj %s keeps %d time points, Subj %s %d",s->label,d1.T,d->sb[0].label,first.T) ;
       free(d1.seglen) ; free(d1.time_index) ; free(d1.rep_seglen) ; free(d1.rep_time_index) ;
       if( d->axis_item ) free(d1.keep) ;
     }
     if( d1.relpre!=NULL ){ d->relpre[2*ss]=d1.relpre[0] ; d->relpre[2*ss+1]=d1.relpre[1] ; free(d1.relpre) ; }
     snprintf(extra,sizeof(extra),"Subj %s, rank %d",s->label,r) ;
     srm_prog_set(&bar,ss+1,extra) ;
   }
   srm_prog_end(&bar) ;
   d->T=first.T ; d->seglen=first.seglen ; d->time_index=first.time_index ; d->nrun=first.nrun ;
   if( d->axis_item ) d->keep=first.keep ;
   d->rep_nrun=first.rep_nrun ; d->rep_T=first.rep_T ; d->rep_seglen=first.rep_seglen ; d->rep_time_index=first.rep_time_index ;
   if( !o->quiet ){
     INFO_message("-compress: every subject is held as a rank x %d factor of its data (rank %d to %d): %.3g GiB "
                  "in place of %.3g GiB",d->T,rmin,rmax,kept/GIB,full/GIB) ;
     if( ncs>0 ) ININFO_message("censoring: %d of %d subjects have censored time points (%d in all), %s",ncs,d->nsub,ncens,
                                o->cenmode==SRM_CEN_INTERP?"filled in by linear interpolation within each run":
                                o->cenmode==SRM_CEN_MISSING?"missing for that subject only":"dropped for everyone") ;
     if( o->cenmode==SRM_CEN_DROP && !d->axis_item && d->keep!=NULL ){   /* the same account as without -compress */
       int nk=0 , t ;
       for( t=0 ; t<d->Tin ; t++ ) nk+=(d->keep[t]==0) ;
       ININFO_message("dropped for everyone (-cenmode drop): %d of %d time points are censored in at least one subject",nk,d->Tin) ;
       if( nk>d->Tin/4 ) WARNING_message("-cenmode drop loses %.0f%% of the time points: every subject's censoring is lost by "
                                         "all. Consider -cenmode interp",100.0*nk/d->Tin) ;
     }
     if( o->repeated_stimulus ){
       int nmiss=0 ; for( ss=0 ; ss<d->nsub ; ss++ ) nmiss+=(d->sb[ss].obs!=NULL) ;
       INFO_message("-repeated_stimulus: the %d runs (%d time points each) are averaged for the fit",first.rep_nrun,d->T) ;
       if( o->cenmode==SRM_CEN_MISSING )
         ININFO_message("each time point over the runs where the subject was observed; %d subject%s still missing somewhere "
                        "(missing in every run)",nmiss,nmiss==1?" is":"s are") ;
     }
     if( ndrop>0 ) ININFO_message("%d feature%s with no variance in some run %s dropped (see %s.fit.txt)",ndrop,ndrop==1?"":"s",
                                  ndrop==1?"was":"were",o->prefix) ;
     if( o->nstart==1 ) ININFO_message("the fit starts where the uncompressed fit would (the same random start, carried into "
                                       "each subject's factor), so the results are the same up to rounding") ;
   }
}

/*! -reliability within on the item axis: an item's repeats (keys Item:n) split into two halves (odd and even repeats,
    averaged), over the repeated items, overall and in each -score_by level (by its first repeat's level). */
/*! Subject ss's own reliability on the item axis, overall and per -score_by level, into cx->rel (allocated). */
static void srm_reliability_items_one( const srm_data *d , srm_ctx *cx , const srm_repg *g , int ss )
{
   const srm_subj *s=cx->sb+ss ; int V=s->nvox , T=d->T , nb=g->nb , lv , q ;
   float *xa=(float *)malloc(sizeof(float)*(size_t)V*nb) , *xb=(float *)malloc(sizeof(float)*(size_t)V*nb) ;
   for( lv=-1 ; lv<cx->L ; lv++ ){
     int m=0 , fa=0 ;
     if( lv>=0 ) while( fa+1<cx->nfac && lv>=cx->fac_off[fa+1] ) fa++ ;
     for( q=0 ; q<nb ; q++ ){
       int f , r , na=0 , nbb=0 ;
       if( lv>=0 && cx->lab[fa][ss][g->first[q]]!=lv-cx->fac_off[fa] ) continue ;
       for( f=0 ; f<V ; f++ ){ xa[(size_t)f*nb+m]=0.0f ; xb[(size_t)f*nb+m]=0.0f ; }
       for( r=0 ; r<g->ncol[q] ; r++ ){
         float *h = (r%2==0) ? xa : xb ;
         if( r%2==0 ) na++ ; else nbb++ ;
         for( f=0 ; f<V ; f++ ) h[(size_t)f*nb+m]+=s->x[(size_t)f*T+g->cols[q][r]] ;
       }
       for( f=0 ; f<V ; f++ ){ xa[(size_t)f*nb+m]/=na ; xb[(size_t)f*nb+m]/=nbb ; }
       m++ ;
     }
     if( m>0 ){                                        /* the m columns gathered, packed into V x m */
       float *pa=(float *)malloc(sizeof(float)*(size_t)V*m) , *pb=(float *)malloc(sizeof(float)*(size_t)V*m) ; int f , c ;
       for( f=0 ; f<V ; f++ ) for( c=0 ; c<m ; c++ ){ pa[(size_t)f*m+c]=xa[(size_t)f*nb+c] ; pb[(size_t)f*m+c]=xb[(size_t)f*nb+c] ; }
       srm_rel_halves(pa,pb,V,m,cx->rel+2*((size_t)ss*(1+cx->L)+lv+1)) ;
       free(pa) ; free(pb) ;
     }
   }
   free(xa) ; free(xb) ;
}

static void srm_reliability_items( const srm_data *d , srm_ctx *cx )
{
   srm_repg g=srm_rep_groups(d) ; int ss , q ;
   cx->rel=(double *)malloc(sizeof(double)*2*(size_t)cx->nsub*(1+cx->L)) ;
   for( q=0 ; q<2*cx->nsub*(1+cx->L) ; q++ ) cx->rel[q]=NAN ;
   if( d->relitems!=NULL )                              /* -compress: made from each subject's voxels while read */
     memcpy(cx->rel,d->relitems,sizeof(double)*2*(size_t)cx->nsub*(1+cx->L)) ;
   else for( ss=0 ; ss<cx->nsub ; ss++ ) srm_reliability_items_one(d,cx,&g,ss) ;
   if( !cx->quiet ) INFO_message("-reliability within: %d repeated item%s, each split into odd and even repeats",g.nb,g.nb==1?"":"s") ;
   srm_rep_groups_free(&g) ;
}

/*! -reliability within: the item axis from its repeats; the time axis from -repeated_stimulus (computed before the
    runs were averaged). */
static void srm_reliability_within( const srm_data *d , srm_ctx *cx )
{
   int ss , q ;
   if( d->axis_item ){ srm_reliability_items(d,cx) ; return ; }
   cx->rel=(double *)malloc(sizeof(double)*2*(size_t)cx->nsub*(1+cx->L)) ;
   for( q=0 ; q<2*cx->nsub*(1+cx->L) ; q++ ) cx->rel[q]=NAN ;
   for( ss=0 ; ss<cx->nsub && d->relpre!=NULL ; ss++ ){
     cx->rel[2*((size_t)ss*(1+cx->L))]=d->relpre[2*ss] ; cx->rel[2*((size_t)ss*(1+cx->L))+1]=d->relpre[2*ss+1] ;
   }
}

/*============================================================================
  -score_by: the levels of each factor, and every subject's level at each column of the model
==============================================================================*/

/*! The index of level v of factor fa, adding it as a new level if unseen. */
static int srm_level_of( srm_ctx *cx , int fa , const char *v )
{
   int q ;
   for( q=0 ; q<cx->fac_nlev[fa] ; q++ ) if( strcmp(cx->fac_lev[fa][q],v)==0 ) return q ;
   cx->fac_lev[fa]=(char **)realloc(cx->fac_lev[fa],sizeof(char *)*(size_t)(q+1)) ;
   cx->fac_lev[fa][q]=strdup(v) ; cx->fac_nlev[fa]++ ;
   return q ;
}

/*! Condition at each retained time point, from the subject's Xmat (THD_xmat_condition_labels): the
    stimulus whose regressor, scaled to its own peak, is largest there, if it reaches -xmat_label_frac of the
    peak; censored time points are unlabelled.  Levels are added in the stimuli's order. */
static void srm_xmat_time_labels( const srm_opts *o , const srm_data *d , const srm_subj *s , srm_ctx *cx , int fa , int *lab )
{
   char err[512] ; THD_xmat *xm ; unsigned char *want ; int *which , *lev , st , t ;
   if( s->xmat==NULL ) ERROR_exit("-score_by Condition: Subj %s has no Xmat to take the conditions from",s->label) ;
   xm=THD_xmat_read(s->xmat,err,sizeof(err)) ;
   if( xm==NULL ) ERROR_exit("Subj %s: %s",s->label,err) ;
   if( xm->nstim<1 ) ERROR_exit("-score_by Condition: '%s' (Subj %s) has no stimulus columns",s->xmat,s->label) ;
   if( xm->nfull!=d->Tin ) ERROR_exit("Subj %s: '%s' describes %d time points; the input has %d",s->label,s->xmat,xm->nfull,d->Tin) ;
   want=THD_xmat_stims_wanted(xm,o->xmat_stims,NULL) ;
   lev=(int *)malloc(sizeof(int)*(size_t)xm->nstim) ;
   for( st=0 ; st<xm->nstim ; st++ ) lev[st] = want[st] ? srm_level_of(cx,fa,xm->stim[st].label) : -1 ;
   which=THD_xmat_condition_labels(xm,o->xmat_label_frac,want) ;
   for( t=0 ; t<d->T ; t++ ){ int w=which[d->time_index[t]] ; lab[t]=(w>=0) ? lev[w] : -1 ; }
   free(which) ; free(lev) ; free(want) ; THD_xmat_free(xm) ;
}

/*! Build cx's factors from -score_by: on the item axis a table column; on the time axis -time_labels NAME,
    Condition from the Xmat column, or a table column (its value on the row of each run). */
static void srm_build_labels_on( const srm_opts *o , const srm_data *d , srm_ctx *cx ) ;

/*! -score_by levels of every column of every subject.  After -repeated_stimulus the labels are made on the
    timeline before averaging, and every repeat must give a time point the same level.
    Result: cx->lab[factor][subject][column] = level index (-1 = unlabelled); the levels of all factors are
    numbered consecutively (fac_off[fa] is factor fa's first), L in all, which is how lscore is indexed. */
static void srm_build_labels( const srm_opts *o , const srm_data *d , srm_ctx *cx )
{
   srm_data dd ; int fa , ss , t , rr , L=d->T ;
   if( o->score_by==NULL ) return ;
   if( d->rep_nrun==0 ){ srm_build_labels_on(o,d,cx) ; return ; }
   dd=*d ; dd.T=d->rep_T ; dd.nrun=d->rep_nrun ; dd.seglen=d->rep_seglen ; dd.time_index=d->rep_time_index ;
   srm_build_labels_on(o,&dd,cx) ;
   for( fa=0 ; fa<cx->nfac ; fa++ ) for( ss=0 ; ss<d->nsub ; ss++ ){
     int *lab=cx->lab[fa][ss] ;
     for( t=0 ; t<L ; t++ ) for( rr=1 ; rr<d->rep_nrun ; rr++ ) if( lab[rr*L+t]!=lab[t] ){
       const char *a = lab[t]<0 ? "(none)" : cx->fac_lev[fa][lab[t]] , *b = lab[rr*L+t]<0 ? "(none)" : cx->fac_lev[fa][lab[rr*L+t]] ;
       ERROR_exit("-score_by %s with -repeated_stimulus: Subj %s, time point %d of the stimulus is %s in run %s but %s in run %s. "
                  "The runs are averaged, so every repeat must give a time point the same level",
                  cx->fac_name[fa],d->sb[ss].label,t+1,a,d->runname[0],b,d->runname[rr]) ;
     }
   }
}

static void srm_build_labels_on( const srm_opts *o , const srm_data *d , srm_ctx *cx )
{
   char *copy , *nm ; THD_datatable *tab=d->tab ; int T=d->T , fa , ss , t ;
   copy=strdup(o->score_by) ;
   for( nm=strtok(copy,",") ; nm!=NULL ; nm=strtok(NULL,",") ){
     fa=cx->nfac++ ;
     cx->fac_name=(char **)realloc(cx->fac_name,sizeof(char *)*(size_t)cx->nfac) ;
     cx->fac_nlev=(int *)realloc(cx->fac_nlev,sizeof(int)*(size_t)cx->nfac) ;
     cx->fac_lev=(char ***)realloc(cx->fac_lev,sizeof(char **)*(size_t)cx->nfac) ;
     cx->lab=(int ***)realloc(cx->lab,sizeof(int **)*(size_t)cx->nfac) ;
     cx->fac_name[fa]=strdup(nm) ; cx->fac_nlev[fa]=0 ; cx->fac_lev[fa]=NULL ;
     cx->lab[fa]=(int **)malloc(sizeof(int *)*(size_t)d->nsub) ;
     for( ss=0 ; ss<d->nsub ; ss++ ){ cx->lab[fa][ss]=(int *)malloc(sizeof(int)*(size_t)T) ; for( t=0 ; t<T ; t++ ) cx->lab[fa][ss][t]=-1 ; }
     if( !strcasecmp(nm,"Subj") || !strcasecmp(nm,"InputFile") || !strcasecmp(nm,o->group_col) )
       ERROR_exit("-score_by %s: that is per subject, and scores.txt already has it; -score_by takes a factor that varies "
                  "within a subject (conditions, categories, runs)",nm) ;
     if( d->axis_item ){
       int icol=THD_datatable_column(tab,nm) ;
       if( icol<0 ) ERROR_exit("-score_by %s: the table has no such column",nm) ;
       for( ss=0 ; ss<d->nsub ; ss++ ) for( t=0 ; t<T ; t++ )
         cx->lab[fa][ss][t]=srm_level_of(cx,fa,DT_CELL(tab,d->sb[ss].irow[t],icol)) ;
     } else {
       int itl ;
       for( itl=0 ; itl<o->ntl ; itl++ ) if( !strcasecmp(o->tl_name[itl],nm) ) break ;
       if( itl<o->ntl ){                               /* -time_labels NAME FILE: one label per time point */
         MRI_IMAGE *im=mri_read_1D(o->tl_file[itl]) , *fm ; float *ar ;
         if( im==NULL ) ERROR_exit("cannot read -time_labels %s file '%s'",nm,o->tl_file[itl]) ;
         fm=mri_to_float(im) ; if( fm!=im ) mri_free(im) ; ar=MRI_FLOAT_PTR(fm) ;
         { int Lrep = d->rep_nrun>0 ? d->rep_T/d->rep_nrun : 0 ;  /* -repeated_stimulus: once per stimulus time point */
           if( fm->nvox!=d->Tin && fm->nvox!=T && fm->nvox!=Lrep )
             ERROR_exit("-time_labels %s: '%s' has %d values; give one per input time point (%d) or per retained one (%d)%s",
                        nm,o->tl_file[itl],fm->nvox,d->Tin,T,Lrep>0 ? ", or one per time point of the repeated stimulus" : "") ; }
         for( t=0 ; t<T ; t++ ){
           int Lrep = d->rep_nrun>0 ? d->rep_T/d->rep_nrun : 0 ;
           float v=ar[(fm->nvox==d->Tin) ? d->time_index[t] : (fm->nvox==T) ? t : t%Lrep] ; char b[32] ;
           if( !isfinite(v) || v!=rintf(v) ) ERROR_exit("-time_labels %s: value %g is not a whole number (0 = unlabelled)",nm,v) ;
           if( v==0.0f ) continue ;
           snprintf(b,sizeof(b),"%d",(int)v) ;
           { int l=srm_level_of(cx,fa,b) ; for( ss=0 ; ss<d->nsub ; ss++ ) cx->lab[fa][ss][t]=l ; }
         }
         mri_free(fm) ;
       } else if( !strcasecmp(nm,"Condition") && d->icol_xmat>=0 ){
         for( ss=0 ; ss<d->nsub ; ss++ ) srm_xmat_time_labels(o,d,d->sb+ss,cx,fa,cx->lab[fa][ss]) ;
       } else {                                         /* a table column: its value on each run's row */
         int icol=THD_datatable_column(tab,nm) ;
         if( icol<0 ) ERROR_exit("-score_by %s: the table has no such column, and it is not a -time_labels name%s",nm,
                                 !strcasecmp(nm,"Condition") ? " (Condition comes from an Xmat column)" : "") ;
         for( ss=0 ; ss<d->nsub ; ss++ ){
           int rr , c0=0 ;
           for( rr=0 ; rr<d->nrun ; c0+=d->seglen[rr],rr++ ){
             int l=srm_level_of(cx,fa,DT_CELL(tab,d->sb[ss].row[d->concat ? 0 : rr],icol)) ;
             for( t=c0 ; t<c0+d->seglen[rr] ; t++ ) cx->lab[fa][ss][t]=l ;
           }
         }
       }
     }
     if( cx->fac_nlev[fa]==0 ) ERROR_exit("-score_by %s: no column of the model has a level",nm) ;
   }
   free(copy) ;
   cx->fac_off=(int *)malloc(sizeof(int)*(size_t)cx->nfac) ;
   for( cx->L=0,fa=0 ; fa<cx->nfac ; fa++ ){ cx->fac_off[fa]=cx->L ; cx->L+=cx->fac_nlev[fa] ; }
   if( !o->quiet ) for( fa=0 ; fa<cx->nfac ; fa++ ){
     char ls[256] ; int pos=0 , l ;
     for( l=0 ; l<cx->fac_nlev[fa] && pos<200 ; l++ ) pos+=snprintf(ls+pos,sizeof(ls)-pos,"%s%s",l?", ":"",cx->fac_lev[fa][l]) ;
     if( l<cx->fac_nlev[fa] ) snprintf(ls+pos,sizeof(ls)-pos,", ...") ;
     INFO_message("-score_by %s: %d level%s (%s); each subject is scored on its held-out samples of each",
                  cx->fac_name[fa],cx->fac_nlev[fa],cx->fac_nlev[fa]==1?"":"s",ls) ;
   }
}

/*============================================================================
  -model_in: place subjects in a saved model; optionally let S absorb them
==============================================================================*/

/*! -model_in on the item axis: the new subjects' items must be the model's own keys (any table
    order); put every subject's columns in the model's order.  A mismatch names a key. */
static void srm_items_to_model( const srm_opts *o , srm_data *d , const SRM_provenance *pv )
{
   int *perm=(int *)malloc(sizeof(int)*(size_t)d->nitem) , j , ss , n=d->nitem ;
   if( SRM_provenance_match_items(pv,n,d->item_key,perm)!=SRM_OK ){
     int q , miss=0 , extra=0 ; const char *m1=NULL , *e1=NULL ;
     for( q=0 ; q<pv->nitem ; q++ ){
       for( j=0 ; j<n ; j++ ) if( strcmp(d->item_key[j],pv->item_key[q])==0 ) break ;
       if( j==n ){ if( m1==NULL ) m1=pv->item_key[q] ; miss++ ; }
     }
     for( j=0 ; j<n ; j++ ){
       for( q=0 ; q<pv->nitem ; q++ ) if( strcmp(d->item_key[j],pv->item_key[q])==0 ) break ;
       if( q==pv->nitem ){ if( e1==NULL ) e1=d->item_key[j] ; extra++ ; }
     }
     ERROR_exit("the new subjects' items are not the model's: the model has %d item columns, the table %d; %d of the model's "
                "are absent%s%s%s, %d are not in the model%s%s%s. Items are matched by %s (Item:n for an item that recurs)",
                pv->nitem,n,miss,m1?" (first: ":"",m1?m1:"",m1?")":"",extra,e1?" (first: ":"",e1?e1:"",e1?")":"",o->item_col) ;
   }
   for( ss=0 ; ss<d->nsub ; ss++ ){
     srm_subj *s=d->sb+ss ; int *a=(int *)malloc(sizeof(int)*(size_t)n*4) ;
     for( j=0 ; j<n ; j++ ){ a[perm[j]]=s->irow[j] ; a[n+perm[j]]=s->irun[j] ; a[2*n+perm[j]]=s->ifile[j] ; a[3*n+perm[j]]=s->ibrick[j] ; }
     memcpy(s->irow,a,sizeof(int)*(size_t)n) ; memcpy(s->irun,a+n,sizeof(int)*(size_t)n) ;
     memcpy(s->ifile,a+2*n,sizeof(int)*(size_t)n) ; memcpy(s->ibrick,a+3*n,sizeof(int)*(size_t)n) ;
     free(a) ;
   }
   for( j=0 ; j<n ; j++ ){ free(d->item_key[j]) ; d->item_key[j]=strdup(pv->item_key[j]) ; }
   free(perm) ;
}

/*! -model_in: map the table's (new) subjects into a saved model.
    Order matters for honesty: (1) load the model and check the new data match its timeline (same input
    length, runs, retained points); (2) prepare the data exactly as the model's were (its censor mask and
    z-scoring); (3) with -cv, score each new subject against the model AS IT STANDS (srm_cv_frozen: S
    fixed, W from training folds only), before anything changes; (4) then give each new subject a W against
    S (SRM_model_add_view_obs), or with -update_S let S absorb them (SRM_update) and write a new model;
    (5) write the outputs.  The new subjects are views nold.. of the model. */
static void srm_run_model_in( srm_opts *o , THD_datatable *tab , int argc , char **argv )
{
   srm_data d ; SRM_model *m=NULL ; SRM_provenance *pv=NULL ; SRM_fitinfo fi ; SRM_params par ;
   srm_ctx cx ; srm_want want ; srm_folds folds ; srm_cvres res ;
   int rc , ss , rr , q , K , T , nold , nsub , do_cv=(strcasecmp(o->cvmode,"none")!=0) , have_res=0 ;
   int *carry=NULL , ncarry=0 , nupd0 ; double drift0 ;
   memset(&d,0,sizeof(d)) ; memset(&fi,0,sizeof(fi)) ; memset(&want,0,sizeof(want)) ; memset(&folds,0,sizeof(folds)) ; memset(&res,0,sizeof(res)) ;

   rc=SRM_load_model(o->model_in,&m,&pv) ;
   if( rc!=SRM_OK ) ERROR_exit("cannot use the model '%s': %s",o->model_in,SRM_strerror(rc)) ;
   if( pv->axis==SRM_AXIS_ITEM && !o->axis_item )
     ERROR_exit("'%s' is a model on the item axis: give -axis item, and a table with the item column (%s)",o->model_in,o->item_col) ;
   if( pv->axis!=SRM_AXIS_ITEM && o->axis_item )
     ERROR_exit("'%s' is a model on the time axis: new subjects must be time series of the same stimulus (drop -axis item)",o->model_in) ;
   K=m->nfeat ; nold=m->nview ;
   o->par.algo=m->algo ; o->zscore=pv->zscore ;
   if( !o->quiet ){
     if( o->axis_item )
       INFO_message("model %s: %s, %d components, %d subjects, %d items (z-scored %s)",o->model_in,
                    m->algo==SRM_ALGO_PROB?"probabilistic":"deterministic",K,nold,m->ntime,
                    pv->zscore==SRM_ZSCORE_RUN?"within runs":pv->zscore==SRM_ZSCORE_ALL?"over all items":"not at all") ;
     else
     INFO_message("model %s: %s, %d components, %d subjects, %d time points in %d run%s%s",o->model_in,
                  m->algo==SRM_ALGO_PROB?"probabilistic":"deterministic",K,nold,m->ntime,pv->nseg,pv->nseg>1?"s":"",
                  m->coupled?", a W per run":"") ;
     if( m->nupdate>0 ) ININFO_message("it has absorbed new subjects %d time%s; drift %.4g",m->nupdate,m->nupdate>1?"s":"",m->drift) ;
   }

   if( o->axis_item ){                                 /* the model's own items, put in its order */
     srm_data_items(o,tab,&d,1) ;
     srm_data_shape_items(o,&d,1,K) ;
     srm_items_to_model(o,&d,pv) ;
   } else {
   srm_data_table(o,tab,&d,1) ;
   srm_data_shape(o,&d,1,K) ;
   }
   nsub=d.nsub ;

   /*-- the new data must be the model's own stimulus, in the model's own runs --*/
   if( !o->axis_item ){
     int *sl=(int *)calloc((size_t)d.nrun,sizeof(int)) , t ;
     /* sl[rr]: run rr's retained length under the MODEL's keep mask, to compare with the model's seglen */
     rc=SRM_OK ;
     if( d.Tin!=pv->ntime_in || d.nrun!=pv->nseg ) rc=SRM_ERR_MISMATCH ;
     else {
       for( rr=0 ; rr<d.nrun ; rr++ ) for( t=0 ; t<d.runlen[rr] ; t++ ) sl[rr]+=(pv->keep[d.c0in[rr]+t]!=0) ;
       rc=SRM_provenance_check(pv,d.Tin,d.nrun,sl) ;
     }
     if( rc!=SRM_OK ){
       char a[256] , b[256] ;
       srm_int_list(a,sizeof(a),d.runlen,d.nrun) ;
       srm_int_list(b,sizeof(b),pv->seglen,pv->nseg) ;
       ERROR_exit("the new data do not fit the model: they have %d time points in %d run%s (lengths %s); the model was built from "
                  "%d time points in %d run%s (%d kept, per run %s). The stimulus, runs, and time points must be the same",
                  d.Tin,d.nrun,d.nrun>1?"s":"",a,pv->ntime_in,pv->nseg,pv->nseg>1?"s":"",m->ntime,b) ;
     }
     free(sl) ;
     srm_data_censor(o,&d,pv->keep) ;
   }

   /*-- subjects already in the model --*/
   for( ss=0 ; ss<nsub ; ss++ ){
     char sn[128] ; srm_safe(sn,sizeof(sn),d.sb[ss].label) ;
     for( q=0 ; q<pv->nsubj ; q++ ) if( strcmp(sn,pv->label[q])==0 ){
       if( o->update_S ) ERROR_exit("Subj %s is already one of the %d subjects in the model; -update_S would count them twice",d.sb[ss].label,pv->nsubj) ;
       WARNING_message("Subj %s is also a subject the model was fit to: their S already contains this person, so their scores are optimistic",d.sb[ss].label) ;
       break ;
     }
   }

   /*-- do not clobber anything --*/
   if( o->update_S ){ srm_check_new_file(o,".srm") ; srm_check_new_file(o,".S.1D") ; srm_check_new_file(o,".objective.1D") ;
                      if( m->coupled ) srm_check_new_file(o,".coupling.txt") ; }
   srm_check_new_file(o,".fit.txt") ;
   if( m->algo==SRM_ALGO_PROB ) srm_check_new_file(o,".rho2.1D") ;
   if( do_cv ){ srm_check_new_file(o,".scores.txt") ; srm_check_new_file(o,".scores_folds.txt") ; }
   srm_check_subject_outputs(o,&d) ;
   srm_memory_preflight(o,&d,K,do_cv,0) ;

   if( o->axis_item ){
     srm_data_read_items(o,&d) ;
     srm_data_items_timeline(&d) ;
   } else {
     srm_data_read(o,&d) ;
     srm_data_subject_censor(o,&d,0) ;
     srm_data_timeline(o,&d,K) ;
   }
   srm_data_normalize(o,&d,pv->zscore,K,0) ;
   T=d.T ;
   if( T!=m->ntime ) ERROR_exit("internal error: %d %s retained; the model has %d",T,o->axis_item?"items":"time points",m->ntime) ;

   /*-- scores first, from the model as it stands: the new subjects have not touched it --*/
   memset(&cx,0,sizeof(cx)) ;
   cx.o=o ; cx.sb=d.sb ; cx.nsub=nsub ; cx.nrun=d.nrun ; cx.T=T ; cx.K=K ; cx.seglen=d.seglen ; cx.runname=d.runname ;
   for( ss=0 ; ss<nsub ; ss++ ) if( d.sb[ss].obs!=NULL ) cx.anymiss=1 ;
   cx.glabel=d.glabel ; cx.ngroup=d.ngroup ; cx.quiet=o->quiet ; cx.progress=o->progress ;
   par=o->par ; par.nfeat=K ; par.center=m->center ; cx.par=par ;
   if( do_cv ){
     want=srm_parse_metrics(o->cv_metrics) ; want.ident=o->axis_item ;
     if( o->axis_item ) srm_make_item_folds(o,&d,&folds,2) ;
     else               srm_make_folds(&folds,d.nrun,d.seglen,d.runname,T,o->cv_blocks) ;
     srm_check_fold_lengths(&folds,want,o->match_window) ;
     { int ff , big=0 ;
       for( ff=1 ; ff<folds.nfold ; ff++ ) if( folds.len[ff]>folds.len[big] ) big=ff ;
       if( T-folds.len[big]<K ) ERROR_exit("holding out fold %s leaves %d %s to fit W; the model has %d components",
                                           folds.name[big],T-folds.len[big],folds.unit,K) ; }
     if( !o->quiet ){
       if( o->axis_item )
         INFO_message("-cv %s with -model_in: S stays fixed; each subject's W is estimated without the items of each of %d "
                      "levels of %s and scored on them by item identification",o->cvmode,folds.nfold,o->fold_col?o->fold_col:o->run_col) ;
       else
       INFO_message("-cv %s with -model_in: S stays fixed; each subject's W is estimated without each of %d %s and scored on it",
                    o->cvmode,folds.nfold,folds.by_blocks?"time blocks":"runs") ;
       if( folds.by_blocks ) WARNING_message("one run only: the folds are contiguous time blocks. Time points next to a block's edge "
                                             "resemble the other side, so scores are somewhat optimistic; separate runs are better") ;
     }
     carry=srm_carry_columns(tab,&d,&ncarry) ;
     srm_build_labels(o,&d,&cx) ;
     srm_cvres_alloc(&res,nsub,folds.nfold,!o->no_projected,o->save_r2,&cx) ; have_res=1 ;
     srm_cv_frozen(&cx,m,&folds,want,&res) ;
     srm_cv_summary(&cx,&res,want,o->cvmode,folds.nfold) ;
   }

   /*-- then the new subjects join: a W each, and (with -update_S) S moves --*/
   drift0=m->drift ; nupd0=m->nupdate ;
   if( o->update_S ){
     int *nv=(int *)malloc(sizeof(int)*(size_t)nsub) ; float **xv=(float **)malloc(sizeof(float *)*(size_t)nsub) ;
     srm_prog bar ; fit_prog fp ;
     const unsigned char **ov=(const unsigned char **)calloc((size_t)nsub,sizeof(unsigned char *)) ; int anyo=0 ;
     for( ss=0 ; ss<nsub ; ss++ ){ nv[ss]=d.sb[ss].nvox ; xv[ss]=d.sb[ss].x ; ov[ss]=d.sb[ss].obs ; anyo|=(ov[ss]!=NULL) ; }
     par.obs = anyo ? (const unsigned char * const *)ov : NULL ;   /* -cenmode missing: each new subject where observed */
     if( m->coupled ){ par.coupling=m->coupling ; par.nseg=pv->nseg ; par.seglen=pv->seglen ; }
     if( !o->quiet ) INFO_message("updating S with %d new subject%s (up to %d iterations)",nsub,nsub>1?"s":"",par.niter) ;
     srm_prog_begin(&bar,o->progress,o->quiet,"update","iterations",par.niter) ;
     fp.bar=&bar ; fp.istart=0 ; fp.nstart=1 ; fp.first_obj=0.0 ; fp.tag=NULL ;
     par.progress=srm_fit_progress ; par.progress_ctx=&fp ;
     rc=SRM_update(m,nsub,nv,xv,&par,&fi) ;
     srm_prog_end(&bar) ;
     if( rc!=SRM_OK ) ERROR_exit("the update failed: %s",SRM_strerror(rc)) ;
     if( !o->quiet ){
       INFO_message("%s after %d iterations; this update moved S by %.4g (the model's drift is now %.4g after %d update%s)",
                    fi.converged?"converged":"stopped at the iteration limit",fi.niter_done,m->drift-drift0,m->drift,m->nupdate,m->nupdate>1?"s":"") ;
       if( m->drift>0.03 ) WARNING_message("the model's drift is %.3g: the oldest subjects' W were fitted to a noticeably different S. "
                                           "Refit from the data when you can",m->drift) ;
     }
     free(nv) ; free(xv) ; free(ov) ; par.obs=NULL ;
   } else {
     srm_prog bar ;
     srm_prog_begin(&bar,o->progress,o->quiet,"mapping","subjects",nsub) ;
     for( ss=0 ; ss<nsub ; ss++ ){
       rc=SRM_model_add_view_obs(m,d.sb[ss].x,d.sb[ss].nvox,d.sb[ss].obs) ;   /* missing points: W, rho2 from the observed */
       if( rc!=SRM_OK ) ERROR_exit("Subj %s: could not estimate W against the model: %s",d.sb[ss].label,SRM_strerror(rc)) ;
       srm_prog_set(&bar,ss+1,NULL) ;
     }
     srm_prog_end(&bar) ;
   }

   /*-- outputs --*/
   {
     char fn[THD_MAX_NAME] ; FILE *fq ; int ii ;
     if( o->update_S ){
       SRM_provenance *pn=pv ;
       snprintf(fn,sizeof(fn),"%s.S.1D",o->prefix) ;
       if( o->axis_item ){
         char c2[256] , *ck ; snprintf(c2,sizeof(c2),"shared response after the update: %d items x %d components",T,K) ;
         ck=srm_keys_comment(c2,d.item_key,T) ; srm_write_transposed_1D(fn,m->s,K,T,ck) ; free(ck) ;
       } else {
         char c2[256] ; snprintf(c2,sizeof(c2),"shared response after the update: %d time points (retained) x %d components",T,K) ;
         srm_write_transposed_1D(fn,m->s,K,T,c2) ; }
       if( !o->quiet ) INFO_message("wrote %s",fn) ;
       snprintf(fn,sizeof(fn),"%s.objective.1D",o->prefix) ;
       fq=fopen(fn,"w") ; if( fq==NULL ) ERROR_exit("cannot write '%s'",fn) ;
       fprintf(fq,"# objective of the restricted problem at the start of each iteration, and at the end (%s); comparable within this file only\n",
               m->algo==SRM_ALGO_PROB?"log-likelihood, rises":"summed squared error, falls") ;
       for( ii=0 ; ii<=fi.niter_done ; ii++ ) fprintf(fq,"%.12g\n",fi.objective[ii]) ;
       fclose(fq) ;
       pn->label=(char **)realloc(pn->label,sizeof(char *)*(size_t)(pn->nsubj+nsub)) ;
       for( ss=0 ; ss<nsub ; ss++ ){ char sn[128] ; srm_safe(sn,sizeof(sn),d.sb[ss].label) ; pn->label[pn->nsubj+ss]=strdup(sn) ; }
       pn->nsubj+=nsub ;
       snprintf(fn,sizeof(fn),"%s.srm",o->prefix) ;
       rc=SRM_save_model(fn,m,pn) ;
       if( rc!=SRM_OK ) ERROR_exit("cannot write the model '%s': %s",fn,SRM_strerror(rc)) ;
       if( !o->quiet ) INFO_message("wrote %s (%d subjects)",fn,m->nview) ;
     }
     if( m->algo==SRM_ALGO_PROB ){
       snprintf(fn,sizeof(fn),"%s.rho2.1D",o->prefix) ;
       fq=fopen(fn,"w") ; if( fq==NULL ) ERROR_exit("cannot write '%s'",fn) ;
       fprintf(fq,"# noise variance per new subject, in the order of the table:") ;
       for( ss=0 ; ss<nsub ; ss++ ) fprintf(fq," %s",d.sb[ss].label) ;
       fputc('\n',fq) ;
       for( ss=0 ; ss<nsub ; ss++ ) fprintf(fq,"%.9g\n",m->rho2[nold+ss]) ;
       fclose(fq) ;
     }
     snprintf(fn,sizeof(fn),"%s.fit.txt",o->prefix) ;
     fq=fopen(fn,"w") ; if( fq==NULL ) ERROR_exit("cannot write '%s'",fn) ;
     if( o->axis_item )
       fprintf(fq,"# 3dSRM -model_in %s: %s, %d components, %d new subjects, %d items\n",
               o->model_in,m->algo==SRM_ALGO_PROB?"probabilistic":"deterministic",K,nsub,T) ;
     else
     fprintf(fq,"# 3dSRM -model_in %s: %s, %d components, %d new subjects, %d time points (%d runs)\n",
             o->model_in,m->algo==SRM_ALGO_PROB?"probabilistic":"deterministic",K,nsub,T,d.nrun) ;
     if( o->update_S )
       fprintf(fq,"# -update_S: iterations %d, converged %d, this update moved S by %.6g; drift %.6g after %d update%s; model now has %d subjects\n",
               fi.niter_done,fi.converged,m->drift-drift0,m->drift,m->nupdate,m->nupdate==1?"":"s",m->nview) ;
     else
       fprintf(fq,"# S was not changed (no -update_S); the model has %d subjects and %d update%s so far\n",nold,nupd0,nupd0==1?"":"s") ;
     if( o->axis_item )
       fprintf(fq,"# zscore %s, items in the model's order\n",pv->zscore==SRM_ZSCORE_RUN?"run":pv->zscore==SRM_ZSCORE_ALL?"all":"none") ;
     else
     fprintf(fq,"# zscore %s, the model's censoring applied: %d of %d input time points dropped\n",
             pv->zscore==SRM_ZSCORE_RUN?"run":pv->zscore==SRM_ZSCORE_ALL?"all":"none",d.Tin-T,d.Tin) ;
     fprintf(fq,"Subj\tGroup\tFeatures_in_mask\tFeatures_used\tFeatures_dropped\trho2\n") ;
     for( ss=0 ; ss<nsub ; ss++ ){
       char r2s[40] ;
       if( m->algo==SRM_ALGO_PROB ) snprintf(r2s,sizeof(r2s),"%.9g",m->rho2[nold+ss]) ; else snprintf(r2s,sizeof(r2s),"NA") ;
       fprintf(fq,"%s\t%s\t%d\t%d\t%d\t%s\n",d.sb[ss].label,d.glabel[d.sb[ss].group],d.sb[ss].nvox_mask,d.sb[ss].nvox,d.sb[ss].ndrop,r2s) ;
     }
     fclose(fq) ;
     if( o->update_S && m->coupled && m->wrun!=NULL ){
       snprintf(fn,sizeof(fn),"%s.coupling.txt",o->prefix) ;
       fq=fopen(fn,"w") ; if( fq==NULL ) ERROR_exit("cannot write '%s'",fn) ;
       fprintf(fq,"# deviation of each run's W from the subject's anchor: |W_ir - W_i|^2 / %d (0 identical, 2 unrelated); lambda %.6g\n",K,m->coupling) ;
       fprintf(fq,"Subj\tGroup\tRun\tDeviation\n") ;
       for( ss=0 ; ss<nsub ; ss++ ) for( rr=0 ; rr<d.nrun ; rr++ ){
         double dv ; SRM_run_deviation(m,nold+ss,rr,&dv) ;
         fprintf(fq,"%s\t%s\t%s\t%.6g\n",d.sb[ss].label,d.glabel[d.sb[ss].group],d.runname[rr],dv) ;
       }
       fclose(fq) ;
     }
     if( have_res ){
       srm_write_scores(&cx,&folds,&res,want,o->cvmode,tab,carry,ncarry) ;
       if( !o->quiet ) INFO_message("wrote %s.scores.txt and %s.scores_folds.txt",o->prefix,o->prefix) ;
     }
   }
   srm_write_subject_outputs(o,&d,m,nold,have_res?&res:NULL,K,argc,argv) ;
   if( have_res ){ srm_cvres_free(&res) ; srm_free_folds(&folds) ; }
   free(carry) ;
   SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; SRM_provenance_free(pv) ;
}

/*============================================================================
  -apply_W: carry each subject's W from an earlier run over to other data
==============================================================================*/

/*! -apply_W -axis item: the subjects, in order of first appearance, and the table rows of each
    (s->irow, s->nrow_item).  Each subject is read later from its own rows, so subjects may have
    different items. */
static void srm_item_subjects( const srm_opts *o , THD_datatable *tab , srm_data *d )
{
   int r , ss ;
   d->tab=tab ; d->axis_item=1 ;
   if( tab->icol_input<0 ) ERROR_exit("the data table needs an InputFile column") ;
   if( tab->icol_subj<0 ) ERROR_exit("-axis item needs a Subj column") ;
   if( THD_datatable_column(tab,o->item_col)<0 )
     ERROR_exit("-axis item needs an item column: the table has no '%s' column (see -item_column)",o->item_col) ;
   for( r=0 ; r<tab->nrow ; r++ ){
     const char *sl=DT_CELL(tab,r,tab->icol_subj) ; srm_subj *s ;
     for( ss=0 ; ss<d->nsub ; ss++ ) if( strcmp(d->sb[ss].label,sl)==0 ) break ;
     if( ss==d->nsub ){
       d->sb=(srm_subj *)realloc(d->sb,sizeof(srm_subj)*(size_t)(d->nsub+1)) ;
       s=d->sb+d->nsub++ ; memset(s,0,sizeof(*s)) ;
       s->label=strdup(sl) ; s->nrun=1 ; s->row=(int *)malloc(sizeof(int)) ; s->row[0]=r ;
     }
     s=d->sb+ss ;
     s->irow=(int *)realloc(s->irow,sizeof(int)*(size_t)(s->nrow_item+1)) ; s->irow[s->nrow_item++]=r ;
   }
}

/*! -apply_W PREFIX: project each table subject's NEW data through their own W saved by an earlier
    -save_W run (PREFIX.W.SUBJ, plus PREFIX.srm for K and the z-scoring).  Nothing is fitted.  W's nonzero
    voxels define the features (no mask), so the new data must be on W's grid.  Each subject is prepared
    one at a time with the model's z-scoring (or, with -zscore none, centered on the new data since the
    old means are not saved) and written as PREFIX.proj.SUBJ; missing samples are written as 0. */
static void srm_run_apply_W( srm_opts *o , THD_datatable *tab , int argc , char **argv )
{
   srm_data d ; SRM_model *m0=NULL ; SRM_provenance *pv=NULL ; int rc , ss , q , K , zs ;
   char fn[THD_MAX_NAME] ; srm_prog bar ;
   memset(&d,0,sizeof(d)) ;
   snprintf(fn,sizeof(fn),"%s.srm",o->apply_W) ;
   rc=SRM_load_model(fn,&m0,&pv) ;
   if( rc!=SRM_OK ) ERROR_exit("-apply_W needs '%s' from the earlier run: %s",fn,SRM_strerror(rc)) ;
   K=m0->nfeat ; o->zscore=zs=pv->zscore ;
   if( o->axis_item ){
     srm_item_subjects(o,tab,&d) ;
     if( zs==SRM_ZSCORE_RUN && THD_datatable_column(tab,o->run_col)<0 ){
       zs=SRM_ZSCORE_ALL ;                              /* the nearest thing: there are no runs to z-score within */
       if( !o->quiet ) INFO_message("the model z-scored within runs; the new items have no '%s' column, so each subject's "
                                    "features are z-scored over all of their items",o->run_col) ;
     }
   } else
   srm_data_table(o,tab,&d,1) ;

   /*-- every subject must be in the model and have a W, before any work --*/
   for( ss=0 ; ss<d.nsub ; ss++ ){
     const srm_subj *s=d.sb+ss ; char sn[128] ; const char *first=DT_CELL(tab,s->row[0],tab->icol_input) ; int is1=srm_is1D(first) , ok ;
     srm_safe(sn,sizeof(sn),s->label) ;
     for( q=0 ; q<pv->nsubj ; q++ ) if( strcmp(sn,pv->label[q])==0 ) break ;
     if( q==pv->nsubj ) ERROR_exit("Subj %s is not in the model '%s' (it has %d subjects)",s->label,fn,pv->nsubj) ;
     snprintf(fn,sizeof(fn),is1?"%s.W.%s.1D":"%s.W.%s",o->apply_W,sn) ;
     ok = is1 ? THD_is_file(fn) : srm_dset_exists(fn) ;
     if( !ok ) ERROR_exit("no W for Subj %s: '%s' was not found (did the earlier run use -save_W?)",s->label,fn) ;
     if( !THD_ok_overwrite() ){
       if( !o->no_projected ){
         if( o->proj_dset ){ snprintf(fn,sizeof(fn),"%s.proj.%s",o->prefix,sn) ; if( srm_dset_exists(fn) ) ERROR_exit("a dataset '%s' already exists (use -overwrite)",fn) ; }
         else { snprintf(fn,sizeof(fn),"%s.proj.%s.1D",o->prefix,sn) ; if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ; }
       }
     }
   }
   if( !o->quiet )
     INFO_message("%s: %d components, %s; applying each of %d subject%s' own W to the new %s",o->apply_W,K,
                  zs==SRM_ZSCORE_RUN?"z-scored within runs":zs==SRM_ZSCORE_ALL?"z-scored over all samples":
                  "not z-scored (each feature is centered on the new data)",d.nsub,"s",o->axis_item?"items":"data") ;

   srm_prog_begin(&bar,o->progress,o->quiet,"applying","subjects",d.nsub) ;
   for( ss=0 ; ss<d.nsub ; ss++ ){
     srm_data d1 ; srm_opts oq=*o ; srm_subj *s=d.sb+ss ; char sn[128] ;
     float *W=NULL , *mu=NULL , *P ; int V , c , f , t ; SRM_model *mv ; THD_datatable *st=NULL ;
     srm_safe(sn,sizeof(sn),s->label) ;
     oq.quiet=1 ; oq.maskname=NULL ; oq.zscore=zs ;
     if( o->axis_item ){                                 /* this subject's own items, from its own rows */
       memset(&d1,0,sizeof(d1)) ;
       st=THD_datatable_select_rows(tab,s->irow,s->nrow_item) ;
       if( st==NULL ) ERROR_exit("Subj %s: cannot take its rows of the table",s->label) ;
       srm_data_items(&oq,st,&d1,1) ;
       if( d1.nitem<3 ) ERROR_exit("Subj %s has %d item%s; z-scoring (or centering) the features needs at least 3",
                                   s->label,d1.nitem,d1.nitem==1?"":"s") ;
       srm_data_shape_items(&oq,&d1,0,0) ;              /* this subject's files and grid; no mask */
       s=d1.sb ;
     } else {
     d1=d ; d1.nsub=1 ; d1.sb=s ; d1.mcache=NULL ; d1.nmcache=0 ; d1.gmask=NULL ;
     d1.runlen=NULL ; d1.c0in=NULL ; d1.runname=NULL ; d1.keep=NULL ; d1.zero_any=NULL ; d1.nzero=NULL ;
     d1.seglen=NULL ; d1.time_index=NULL ; d1.T=0 ; d1.Tin=0 ;
     srm_data_shape(&oq,&d1,0,0) ;                      /* this subject's lengths and grid; no mask */
     }
     if( s->is1D ){
       MRI_IMAGE *im , *fm ; float *ar ; int nx ;
       snprintf(fn,sizeof(fn),"%s.W.%s.1D",o->apply_W,sn) ;
       im=mri_read_1D(fn) ; if( im==NULL ) ERROR_exit("cannot read '%s'",fn) ;
       fm=mri_to_float(im) ; ar=MRI_FLOAT_PTR(fm) ; nx=fm->nx ;
       if( fm->ny!=K ) ERROR_exit("'%s' has %d columns; the model has %d components",fn,fm->ny,K) ;
       if( nx!=s->nvox_mask ) ERROR_exit("Subj %s: W has %d rows but the new data have %d columns",s->label,nx,s->nvox_mask) ;
       /* the 1D image is column-major (component c's column at ar + c*nx); W is row-major V x K */
       V=nx ; W=(float *)malloc(sizeof(float)*(size_t)V*K) ;
       for( f=0 ; f<V ; f++ ) for( c=0 ; c<K ; c++ ) W[(size_t)f*K+c]=ar[(size_t)c*nx+f] ;
       if( fm!=im ) mri_free(fm) ;
       mri_free(im) ;
     } else {
       char wfound[THD_MAX_NAME] ; THD_3dim_dataset *wds ; MRI_IMAGE **bim=(MRI_IMAGE **)malloc(sizeof(MRI_IMAGE *)*(size_t)K) ; float **fb=(float **)malloc(sizeof(float *)*(size_t)K) ; int nvg ; byte *in ;
       snprintf(fn,sizeof(fn),"%s.W.%s",o->apply_W,sn) ;
       if( !srm_dset_find(fn,wfound,sizeof(wfound)) ) ERROR_exit("cannot find the W dataset '%s'",fn) ;
       wds=THD_open_dataset(wfound) ; CHECK_OPEN_ERROR(wds,wfound) ;
       if( DSET_NVALS(wds)!=K ) ERROR_exit("'%s' has %d volumes; the model has %d components",fn,DSET_NVALS(wds),K) ;
       if( !EQUIV_GRIDS(wds,s->geom) )
         ERROR_exit("Subj %s: the new data are not on the grid of '%s' (W is applied on its own grid and voxels)",s->label,fn) ;
       DSET_load(wds) ; nvg=DSET_NVOX(wds) ; in=(byte *)calloc((size_t)nvg,1) ;
       /* features = voxels where any component of W is nonzero (the fit's mask minus dropped voxels) */
       for( c=0 ; c<K ; c++ ){
         bim[c]=THD_extract_float_brick(c,wds) ; fb[c]=MRI_FLOAT_PTR(bim[c]) ;
         for( f=0 ; f<nvg ; f++ ) if( fb[c][f]!=0.0f ) in[f]=1 ;
       }
       for( V=0,f=0 ; f<nvg ; f++ ) V+=in[f] ;
       if( V<K ) ERROR_exit("'%s' has only %d voxels with a nonzero W",fn,V) ;
       s->vox=(int *)malloc(sizeof(int)*(size_t)V) ; s->nvox_mask=V ; W=(float *)malloc(sizeof(float)*(size_t)V*K) ;
       for( q=0,f=0 ; f<nvg ; f++ ) if( in[f] ){
         s->vox[q]=f ; for( c=0 ; c<K ; c++ ) W[(size_t)q*K+c]=fb[c][f] ;
         q++ ;
       }
       for( c=0 ; c<K ; c++ ) mri_free(bim[c]) ;
       free(bim) ; free(fb) ; free(in) ; DSET_delete(wds) ;
     }
     if( o->axis_item ){
       srm_data_read_items(&oq,&d1) ;
       srm_data_items_timeline(&d1) ;
     } else {
       srm_data_censor(&oq,&d1,NULL) ;
       srm_data_read(&oq,&d1) ;
       srm_data_subject_censor(&oq,&d1,1) ;
       srm_data_timeline(&oq,&d1,0) ;
     }
     srm_data_normalize(&oq,&d1,zs,0,1) ;
     t=d1.T ;
     if( zs==SRM_ZSCORE_NONE ){                 /* no mu in the files: center on the new data (where observed) */
       mu=(float *)malloc(sizeof(float)*(size_t)V) ;
       for( f=0 ; f<V ; f++ ){ double a=0.0 ; int z , n=0 ;
         for( z=0 ; z<t ; z++ ) if( s->obs==NULL || s->obs[z] ){ a+=s->x[(size_t)f*t+z] ; n++ ; }
         mu[f]=(float)(a/n) ; }
     }
     mv=SRM_model_from_w(V,K,W,mu) ;
     if( mv==NULL ) ERROR_exit("out of memory applying W for Subj %s",s->label) ;
     P=(float *)malloc(sizeof(float)*(size_t)K*(size_t)t) ;
     rc=SRM_project(mv,0,s->x,t,P) ;
     if( rc!=SRM_OK ) ERROR_exit("Subj %s: projection failed: %s",s->label,SRM_strerror(rc)) ;
     if( s->obs!=NULL ){ int z ; for( z=0 ; z<t ; z++ ) if( !s->obs[z] ) for( c=0 ; c<K ; c++ ) P[(size_t)c*t+z]=0.0f ; }
     s->nvox=V ;
     if( !o->no_projected ){
       if( o->axis_item )
         srm_write_proj(o,s,P,K,t,"subject's NEW items in the shared space, through this subject's own W from the earlier run: one row per item, one column per component",d1.item_key,argc,argv) ;
       else
       srm_write_proj(o,s,P,K,t,s->obs!=NULL ? "subject's NEW data in the shared space, through this subject's own W from the earlier run: one row per retained time point, one column per component; censored (missing, -cenmode missing) rows are 0"
                                              : "subject's NEW data in the shared space, through this subject's own W from the earlier run: one row per retained time point, one column per component",NULL,argc,argv) ;
     }
     SRM_free_model(mv) ; free(W) ; free(mu) ; free(P) ; free(s->x) ; s->x=NULL ;
     if( st!=NULL ){ int j ; for( j=0 ; j<d1.nitem ; j++ ) free(d1.item_key[j]) ; free(d1.item_key) ; THD_free_datatable(st) ; }
     srm_prog_set(&bar,ss+1,NULL) ;
   }
   srm_prog_end(&bar) ;
   SRM_free_model(m0) ; SRM_provenance_free(pv) ;
}

/*! -compress with -save_W or -save_recon: maps on the voxels need the voxels, so each subject is read (and
    prepared) again, one at a time; its W on the voxels is X M with M = SRM_compress_map(R, W_R), and its
    means, when the fit centered, the same way.  Projections are the same either way. */
static void srm_write_outputs_reread( const srm_opts *o , srm_data *d , SRM_model *m , const srm_cvres *res ,
                                      int K , int argc , char **argv )
{
   srm_opts oq=*o ; srm_prog bar ; int ss ;
   oq.quiet=1 ;
   srm_prog_begin(&bar,o->progress,o->quiet,"maps: reading again","subjects",d->nsub) ;
   for( ss=0 ; ss<d->nsub ; ss++ ){
     srm_subj *s=d->sb+ss ; srm_data d1 ; float *R=s->x , *Wv , *muv=NULL , *wR=m->w[ss] , *muR=(m->mu!=NULL) ? m->mu[ss] : NULL ;
     int r=s->nvox , T=d->T , V , nvR=m->nvox[ss] , rc ; double *M ; srm_cvres r1 ; float *vr1=NULL ;
     /* the subject as it was before its features were dropped and it was compressed */
     s->nvox_mask=s->nvox_mask0 ;
     if( s->vox0!=NULL ){ free(s->vox) ; s->vox=(int *)malloc(sizeof(int)*(size_t)s->nvox_mask0) ; memcpy(s->vox,s->vox0,sizeof(int)*(size_t)s->nvox_mask0) ; }
     byte *obs0=s->obs , *miss0=s->miss_in ;            /* kept for the steps after this one */
     s->x=NULL ; s->obs=NULL ; s->miss_in=NULL ;
     srm_subject_stage(&oq,d,&d1,ss,K) ;
     V=s->nvox ;
     if( V!=(int)s->ndim || d1.T!=T ) ERROR_exit("Subj %s: reading again gave %d features and %d time points, not %d and %d",
                                                 s->label,V,d1.T,(int)s->ndim,T) ;
     M=(double *)malloc(sizeof(double)*(size_t)T*K) ;
     rc=SRM_compress_map(R,r,T,wR,K,M) ;
     if( rc!=SRM_OK ) ERROR_exit("Subj %s: W on the voxels: %s",s->label,SRM_strerror(rc)) ;
     Wv=(float *)malloc(sizeof(float)*(size_t)V*K) ;
     SRM_compress_expand(s->x,V,T,M,K,Wv) ;
     if( muR!=NULL ){                                   /* the means, the same way */
       rc=SRM_compress_map(R,r,T,muR,1,M) ;
       if( rc!=SRM_OK ) ERROR_exit("Subj %s: the means on the voxels: %s",s->label,SRM_strerror(rc)) ;
       muv=(float *)malloc(sizeof(float)*(size_t)V) ;
       SRM_compress_expand(s->x,V,T,M,1,muv) ;
     }
     free(M) ;
     m->w[ss]=Wv ; m->nvox[ss]=V ; if( muR!=NULL ) m->mu[ss]=muv ;
     if( res!=NULL ){ r1=*res ; r1.proj=(res->proj!=NULL) ? res->proj+ss : NULL ; r1.vr2=NULL ; }
     if( res!=NULL && o->save_r2 && res->r2defer ){      /* the held-out R^2 of every scored fold, on the voxels */
       vr1=srm_r2_make(ss,s->x,V,T,R,r) ;
       if( vr1!=NULL ) r1.vr2=&vr1 ;
     }
     srm_write_subject_outputs(&oq,&d1,m,ss,res!=NULL ? &r1 : NULL,K,argc,argv) ;
     m->w[ss]=wR ; m->nvox[ss]=nvR ; if( muR!=NULL ) m->mu[ss]=muR ;
     free(Wv) ; free(muv) ; free(vr1) ; vr1=NULL ; free(s->x) ; s->x=R ; s->nvox=r ;
     free(s->obs) ; free(s->miss_in) ; s->obs=obs0 ; s->miss_in=miss0 ;
     free(d1.seglen) ; free(d1.time_index) ; free(d1.relpre) ; free(d1.rep_seglen) ; free(d1.rep_time_index) ;
     if( d->axis_item ) free(d1.keep) ;
     srm_prog_set(&bar,ss+1,NULL) ;
   }
   srm_prog_end(&bar) ;
}

int main( int argc , char **argv )
{
   srm_opts o ;
   int nopt=1 , ii , jj , rr , ss ;
   THD_datatable *tab=NULL ; THD_datatable_index *dx=NULL ;
   srm_data d ;
   int nsub , nrun , have_run , ngroup ; char **glabel=NULL ;
   srm_subj *sb ;
   int *runlen=NULL , Tin=0 , *c0in=NULL , T=0 , *seglen=NULL ;
   int icol_mask , icol_group ;
   byte *censor_keep=NULL ;
   int *time_index=NULL ; char **runname=NULL ;
   int K=0 , do_cv=0 ;

   /*-- startup, the AFNI way: the program is named once, here --*/

   if( argc<2 ){ usage_3dSRM() ; return 0 ; }
   mainENTRY("3dSRM main") ; machdep() ; AFNI_SETUP_OMP(0) ;
   AFNI_logger("3dSRM",argc,argv) ; PRINT_VERSION("3dSRM") ; AUTHOR("P Molfese") ;
   THD_check_AFNI_version("3dSRM") ;

   srm_opts_default(&o) ; memset(&d,0,sizeof(d)) ;

   /*-- options --*/

   while( nopt<argc && argv[nopt][0]=='-' ){
#define NEEDARG(what) do{ if( ++nopt>=argc ) ERROR_exit("need %s after '%s'",(what),argv[nopt-1]) ; }while(0)
#define IS(s) (strcasecmp(argv[nopt],(s))==0)
     if( IS("-help") || IS("-h") ){ usage_3dSRM() ; return 0 ; }
     else if( IS("-prefix") ){ NEEDARG("a prefix") ; free(o.prefix) ; o.prefix=strdup(argv[nopt++]) ; }
     else if( IS("-nfeatures") ){
       NEEDARG("an integer or comma list") ;
       o.klist=srm_parse_intlist(argv[nopt],&o.nk) ;
       if( o.klist==NULL ) ERROR_exit("-nfeatures needs positive integers separated by commas, not '%s'",argv[nopt]) ;
       nopt++ ;
     }
     else if( IS("-algorithm") ){
       NEEDARG("prob or det") ;
       if( strcasecmp(argv[nopt],"prob")==0 ) o.par.algo=SRM_ALGO_PROB ;
       else if( strcasecmp(argv[nopt],"det")==0 ) o.par.algo=SRM_ALGO_DET ;
       else ERROR_exit("-algorithm must be prob or det, not '%s'",argv[nopt]) ;
       o.given_algo=1 ; nopt++ ;
     }
     else if( IS("-niter") ){ NEEDARG("an integer") ; o.par.niter=atoi(argv[nopt++]) ; o.given_niter=1 ;
       if( o.par.niter<1 ) ERROR_exit("-niter must be at least 1") ; }
     else if( IS("-tol") ){ NEEDARG("a number") ; o.par.tol=atof(argv[nopt++]) ; o.given_tol=1 ;
       if( !(o.par.tol>=0.0) ) ERROR_exit("-tol must be a nonnegative number") ; }
     else if( IS("-seed") ){ NEEDARG("an integer") ; o.par.seed=(unsigned int)strtoul(argv[nopt++],NULL,10) ; o.given_seed=1 ; }
     else if( IS("-nstart") ){ NEEDARG("an integer") ; o.nstart=atoi(argv[nopt++]) ;
       if( o.nstart<1 ) ERROR_exit("-nstart must be at least 1") ; }
     else if( IS("-mask") ){ NEEDARG("a dataset") ; o.maskname=argv[nopt++] ; o.given_mask=1 ; }
     else if( IS("-xmat_label_frac") ){ NEEDARG("a fraction") ; o.xmat_label_frac=atof(argv[nopt++]) ;
       if( !(o.xmat_label_frac>0.0 && o.xmat_label_frac<=1.0) ) ERROR_exit("-xmat_label_frac must be in (0,1]") ; }
     else if( IS("-lss") ){ o.lss=1 ; nopt++ ; }
     else if( IS("-xmat_stims") ){ NEEDARG("a comma list of stimulus labels") ; o.xmat_stims=argv[nopt++] ; }
     else if( IS("-concat") ){ NEEDARG("a 1D list of run starts") ; o.concat=argv[nopt++] ; }
     else if( IS("-run_column") ){ NEEDARG("a name") ; free(o.run_col) ; o.run_col=strdup(argv[nopt++]) ; }
     else if( IS("-group_column") ){ NEEDARG("a name") ; free(o.group_col) ; o.group_col=strdup(argv[nopt++]) ; }
     else if( IS("-zscore") ){
       NEEDARG("run, all, or none") ;
       if( strcasecmp(argv[nopt],"run")==0 ) o.zscore=SRM_ZSCORE_RUN ;
       else if( strcasecmp(argv[nopt],"all")==0 ) o.zscore=SRM_ZSCORE_ALL ;
       else if( strcasecmp(argv[nopt],"none")==0 ) o.zscore=SRM_ZSCORE_NONE ;
       else ERROR_exit("-zscore must be run, all, or none, not '%s'",argv[nopt]) ;
       o.given_zscore=1 ; nopt++ ;
     }
     else if( IS("-censor") ){ NEEDARG("a 1D file") ; o.censor_name=argv[nopt++] ; }
     else if( IS("-zcensor") ){ o.zcensor=1 ; nopt++ ; }
     else if( IS("-cenmode") ){
       NEEDARG("interp, drop, or missing") ;
       if( strcasecmp(argv[nopt],"interp")==0 || strcasecmp(argv[nopt],"NTRP")==0 ) o.cenmode=SRM_CEN_INTERP ;
       else if( strcasecmp(argv[nopt],"drop")==0 || strcasecmp(argv[nopt],"KILL")==0 ) o.cenmode=SRM_CEN_DROP ;
       else if( strcasecmp(argv[nopt],"missing")==0 ) o.cenmode=SRM_CEN_MISSING ;
       else ERROR_exit("-cenmode must be interp, drop, or missing, not '%s'",argv[nopt]) ;
       o.cenmode_given=1 ; nopt++ ;
     }
     else if( IS("-zcensor_warn") ){ NEEDARG("a percentage") ; o.zcensor_warn=(float)atof(argv[nopt++]) ; o.zcensor_warn_given=1 ; }
     else if( IS("-run_coupling") ){
       NEEDARG("shared or a nonnegative number") ;
       if( strcasecmp(argv[nopt],"shared")==0 ) o.par.coupling=SRM_COUPLING_SHARED ;
       else { char *end ; double v=strtod(argv[nopt],&end) ;
         if( *end!='\0' || !isfinite(v) || v<0.0 ) ERROR_exit("-run_coupling must be shared or a number >= 0, not '%s'",argv[nopt]) ;
         o.par.coupling=v ; }
       o.given_coupling=1 ; nopt++ ;
     }
     else if( IS("-axis") ){
       NEEDARG("time or item") ;
       if( strcasecmp(argv[nopt],"item")==0 ) o.axis_item=1 ;
       else if( strcasecmp(argv[nopt],"time")==0 ) o.axis_item=0 ;
       else ERROR_exit("-axis must be time or item, not '%s'",argv[nopt]) ;
       nopt++ ;
     }
     else if( IS("-repeat_column") ){ NEEDARG("a name") ; free(o.repeat_col) ; o.repeat_col=strdup(argv[nopt++]) ; o.item_col_given=1 ; }
     else if( IS("-item_column") ){ NEEDARG("a name") ; free(o.item_col) ; o.item_col=strdup(argv[nopt++]) ; o.item_col_given=1 ; }
     else if( IS("-repeated_stimulus") ){ o.repeated_stimulus=1 ; nopt++ ; }
     else if( IS("-compress") ){ o.compress=1 ; o.compress_given=1 ; nopt++ ; }
     else if( IS("-no_compress") ){ o.no_compress=1 ; nopt++ ; }
     else if( IS("-time_labels") ){
       if( nopt+2>=argc ) ERROR_exit("need a name and a file after -time_labels") ;
       o.tl_name=(char **)realloc(o.tl_name,sizeof(char *)*(o.ntl+1)) ; o.tl_file=(char **)realloc(o.tl_file,sizeof(char *)*(o.ntl+1)) ;
       o.tl_name[o.ntl]=argv[nopt+1] ; o.tl_file[o.ntl]=argv[nopt+2] ; o.ntl++ ; nopt+=3 ;
     }
     else if( IS("-model_in") ){ NEEDARG("a model file") ; o.model_in=argv[nopt++] ; }
     else if( IS("-update_S") ){ o.update_S=1 ; nopt++ ; }
     else if( IS("-apply_W") ){ NEEDARG("a prefix") ; o.apply_W=argv[nopt++] ; }
     else if( IS("-cv") ){
       NEEDARG("none, run, or loso") ;
       if( strcasecmp(argv[nopt],"none")!=0 && strcasecmp(argv[nopt],"run")!=0 && strcasecmp(argv[nopt],"loso")!=0 )
         ERROR_exit("-cv must be none, run, or loso, not '%s'",argv[nopt]) ;
       free(o.cvmode) ; o.cvmode=strdup(argv[nopt++]) ;
     }
     else if( IS("-cv_blocks") ){ NEEDARG("an integer") ; o.cv_blocks=atoi(argv[nopt++]) ; o.cv_blocks_given=1 ;
       if( o.cv_blocks<2 ) ERROR_exit("-cv_blocks must be at least 2") ; }
     else if( IS("-cv_metrics") ){ NEEDARG("a comma list") ; o.cv_metrics=argv[nopt++] ; }
     else if( IS("-match_window") ){ NEEDARG("an integer") ; o.match_window=atoi(argv[nopt++]) ; o.match_window_given=1 ;
       if( o.match_window<1 ) ERROR_exit("-match_window must be at least 1") ; }
     else if( IS("-loso_start") ){
       const char *mode ; NEEDARG("cold or legacy") ; mode=argv[nopt++] ; o.loso_start_given=1 ;
       if( !strcasecmp(mode,"cold") ) o.loso_legacy=0 ;
       else if( !strcasecmp(mode,"legacy") ) o.loso_legacy=1 ;
       else ERROR_exit("-loso_start must be cold or legacy") ;
     }
     else if( IS("-loso_iter") ){ NEEDARG("an integer") ; o.loso_iter=atoi(argv[nopt++]) ; o.loso_iter_given=1 ;
       if( o.loso_iter<1 ) ERROR_exit("-loso_iter must be at least 1") ; }
     else if( IS("-fold_column") ){ NEEDARG("a name") ; o.fold_col=argv[nopt++] ; }
     else if( IS("-cv_adapt") ){ NEEDARG("a fraction") ; o.cv_adapt=atof(argv[nopt++]) ; o.cv_adapt_given=1 ;
       if( !(o.cv_adapt>0.0 && o.cv_adapt<1.0) ) ERROR_exit("-cv_adapt must be between 0 and 1") ; }
     else if( IS("-score_by") ){ NEEDARG("a comma list") ; o.score_by=argv[nopt++] ; }
     else if( IS("-cv_fit") ){ NEEDARG("pooled, within, or reference:LABEL") ; o.cv_fit=argv[nopt++] ; }
     else if( IS("-balance") ){ NEEDARG("an integer") ; o.balance=atoi(argv[nopt++]) ; }
     else if( IS("-reliability") ){ NEEDARG("a comma list") ; o.reliability=argv[nopt++] ; }
     else if( IS("-nsplit") ){ NEEDARG("an integer") ; o.nsplit=atoi(argv[nopt++]) ; }
     else if( IS("-nperm") ){ NEEDARG("an integer") ; o.nperm=atoi(argv[nopt++]) ; }
     else if( IS("-save_W") ){ o.save_W=1 ; nopt++ ; }
     else if( IS("-save_recon") ){ o.save_recon=1 ; nopt++ ; }
     else if( IS("-save_r2") ){ o.save_r2=1 ; nopt++ ; }
     else if( IS("-no_projected") ){ o.no_projected=1 ; nopt++ ; }
     else if( IS("-proj_format") ){
       NEEDARG("dset or 1D") ;
       if( strcasecmp(argv[nopt],"dset")==0 ) o.proj_dset=1 ;
       else if( strcasecmp(argv[nopt],"1D")==0 ) o.proj_dset=0 ;
       else ERROR_exit("-proj_format must be dset or 1D, not '%s'",argv[nopt]) ;
       nopt++ ;
     }
     else if( IS("-memory_limit") ){
       NEEDARG("GiB") ; o.memory_limit_gib=atof(argv[nopt++]) ; o.memory_limit_given=1 ;
       if( !(o.memory_limit_gib>0.0) || !isfinite(o.memory_limit_gib) ) ERROR_exit("-memory_limit must be a positive number of GiB") ;
     }
     else if( IS("-memory_override") ){ o.memory_override=1 ; nopt++ ; }
     else if( IS("-quiet") ){ o.quiet=1 ; nopt++ ; }
     else if( IS("-progress") ){
       NEEDARG("auto, bar, line, or off") ;
       if( strcasecmp(argv[nopt],"auto")==0 ) o.progress=PROG_AUTO ;
       else if( strcasecmp(argv[nopt],"bar")==0 ) o.progress=PROG_BAR ;
       else if( strcasecmp(argv[nopt],"line")==0 ) o.progress=PROG_LINE ;
       else if( strcasecmp(argv[nopt],"off")==0 ) o.progress=PROG_OFF ;
       else ERROR_exit("-progress must be auto, bar, line, or off, not '%s'",argv[nopt]) ;
       nopt++ ;
     }
     else if( IS("-show_table") ){ o.show_table=1 ; nopt++ ; }
     else if( IS("-dataTableFile") ){
       NEEDARG("a file") ;
       if( tab!=NULL ) ERROR_exit("the data table was given twice") ;
       tab=THD_read_datatable_file(argv[nopt++]) ;
     }
     else if( IS("-dataTable") ){
       int nused=0 ;
       NEEDARG("a table") ;
       if( tab!=NULL ) ERROR_exit("the data table was given twice") ;
       tab=THD_read_datatable_args(argc,argv,nopt,srm_options,&nused) ;
       nopt+=nused ;
     }
     else ERROR_exit("unknown option '%s'",argv[nopt]) ;
   }
   if( nopt<argc ) ERROR_exit("unexpected argument '%s'",argv[nopt]) ;
   if( tab==NULL ) ERROR_exit("a data table is required: -dataTable or -dataTableFile") ;
   if( o.lss && (!o.axis_item || o.xmat_stims!=NULL) )
     ERROR_exit("-lss needs -axis item and cannot be combined with -xmat_stims") ;
   if( !o.lss && THD_datatable_column(tab,"Events")>=0 )
     ERROR_exit("an Events column requires -lss -axis item") ;
   if( THD_datatable_column(tab,"Xmat")>=0 && !(o.axis_item && THD_datatable_column(tab,o.item_col)>=0) )
     tab=THD_datatable_add_xmat_columns(tab,"Xmat") ;   /* Xmat.TR, Xmat.ncensored, ...: covariates in scores.txt */
   if( o.lss ){
     THD_datatable *expanded=THD_datatable_expand_lss_items(tab,o.item_col,o.run_col) ;
     THD_free_datatable(tab) ; tab=expanded ;
     if( !o.quiet ) INFO_message("LSS event-to-item expansion: %d rows; -show_table prints the item table",tab->nrow) ;
   }
   else if( o.axis_item && THD_datatable_column(tab,"Xmat")>=0 && THD_datatable_column(tab,o.item_col)<0 )
     tab=srm_xmat_items(&o,tab) ;                        /* one row per subject -> one per item */
   else if( o.xmat_stims!=NULL ) ERROR_exit("-xmat_stims needs -axis item and an Xmat column (with no '%s' column)",o.item_col) ;
   if( o.show_table ){ THD_datatable_print(tab,stdout) ; return 0 ; }

   /*-- options that belong to one axis --*/
   if( o.axis_item && !o.given_zscore ) o.zscore=SRM_ZSCORE_ALL ;   /* betas of one GLM share a baseline */
   if( o.axis_item ){
     if( o.censor_name!=NULL )
       ERROR_exit("-censor drops time points; with -axis item, leave an item out by removing its rows (for every subject)") ;
     if( o.zcensor || o.zcensor_warn_given )
       ERROR_exit("-zcensor is for time points; with -axis item an all-zero item is always an error, named by Subj and item") ;
     if( o.cv_blocks_given )
       ERROR_exit("-cv_blocks cuts time into contiguous blocks; items have no order: the folds of -axis item are runs, or the levels of -fold_column") ;
     if( o.match_window_given )
       ERROR_exit("-match_window is the window of segment matching in time; with -axis item each item is identified on its own") ;
     if( o.ntl>0 )
       ERROR_exit("-time_labels label time points; with -axis item put the labels in a table column (for -score_by)") ;
     if( o.repeated_stimulus )
       ERROR_exit("-repeated_stimulus is for -axis time; with -axis item the repeats are the items that recur across runs") ;
     if( o.par.coupling!=SRM_COUPLING_SHARED )
       ERROR_exit("-run_coupling needs the same runs in the same columns for every subject, so it is for -axis time only") ;
   } else if( o.item_col_given ) ERROR_exit("-item_column and -repeat_column need -axis item") ;

   /*-- what this build does not do yet: say so, plainly --*/

   if( o.score_by!=NULL && strcasecmp(o.cvmode,"none")==0 ) ERROR_exit("-score_by needs -cv run or -cv loso: it splits the held-out scores") ;
   { int itl ; for( itl=0 ; itl<o.ntl ; itl++ ){
       int used=0 ; char *copy=(o.score_by!=NULL) ? strdup(o.score_by) : NULL , *tk ;
       for( tk=(copy?strtok(copy,","):NULL) ; tk!=NULL ; tk=strtok(NULL,",") ) used|=!strcasecmp(tk,o.tl_name[itl]) ;
       free(copy) ;
       if( !used ) ERROR_exit("-time_labels %s is used only through -score_by %s",o.tl_name[itl],o.tl_name[itl]) ; } }
   if( o.fold_col!=NULL && !o.axis_item )
     ERROR_exit("-fold_column is for -axis item; with -axis time the folds are the runs (or -cv_blocks)") ;
   if( o.fold_col!=NULL && strcasecmp(o.cvmode,"none")==0 ) ERROR_exit("-fold_column needs -cv run or -cv loso") ;
   if( o.concat!=NULL && o.axis_item ) ERROR_exit("-concat splits a time series into runs; it is for -axis time") ;
   if( o.loso_start_given ){
     if( strcasecmp(o.cvmode,"loso") ) ERROR_exit("-loso_start needs -cv loso") ;
     if( o.model_in!=NULL || o.apply_W!=NULL ) ERROR_exit("-loso_start is for fitting, not -model_in or -apply_W") ;
   }
   if( o.loso_iter_given && !o.loso_legacy && !strcasecmp(o.cvmode,"loso") )
     WARNING_message("-loso_iter is ignored with cold starts; use -niter and -tol (or explicitly -loso_start legacy)") ;
   if( o.cv_adapt_given ){
     if( o.model_in!=NULL || o.apply_W!=NULL ) ERROR_exit("-cv_adapt goes with -run_coupling in a fit: not with -model_in or -apply_W") ;
     if( o.par.coupling==SRM_COUPLING_SHARED )
       ERROR_exit("-cv_adapt needs -run_coupling: with one W for all of a subject's runs there is no run W to estimate") ;
     if( strcasecmp(o.cvmode,"none")==0 ) ERROR_exit("-cv_adapt needs -cv run or -cv loso: it changes how held-out runs are scored") ;
   }
   if( o.reliability!=NULL ){
     char *copy=strdup(o.reliability) , *tk ;
     for( tk=strtok(copy,",") ; tk!=NULL ; tk=strtok(NULL,",") ){
       if( !strcasecmp(tk,"within") ) o.rel_within=1 ;
       else if( !strcasecmp(tk,"split") ) o.rel_split=1 ;
       else ERROR_exit("-reliability: '%s' is not within or split",tk) ;
     }
     free(copy) ;
     if( o.model_in!=NULL || o.apply_W!=NULL ) ERROR_exit("-reliability is about a fit: not with -model_in or -apply_W") ;
     if( o.nk>1 ) ERROR_exit("-reliability with several -nfeatures: choose the number of components first") ;
   }
   if( o.nsplit!=0 && !o.rel_split ) ERROR_exit("-nsplit goes with -reliability split") ;
   if( o.rel_split && o.nsplit==0 ) o.nsplit=100 ;
   if( o.rel_split && o.nsplit<2 ) ERROR_exit("-nsplit must be at least 2") ;
   if( o.nperm!=0 ){
     if( strcasecmp(o.cvmode,"none")==0 ) ERROR_exit("-nperm tests the held-out scores: it needs -cv run or -cv loso") ;
     if( o.nperm<20 ) ERROR_exit("-nperm must be at least 20") ;
     if( o.nk>1 ) ERROR_exit("-nperm with several -nfeatures: choose the number of components first") ;
   }
   if( o.rel_within && strcasecmp(o.cvmode,"none")==0 )
     ERROR_exit("-reliability within goes into scores.txt beside the held-out scores: it needs -cv run or -cv loso") ;
   if( o.rel_within && !o.axis_item && !o.repeated_stimulus )
     ERROR_exit("-reliability within needs repeats: -repeated_stimulus (every run shows the same stimulus), or -axis item with "
                "items measured more than once") ;
   if( o.repeated_stimulus ){
     if( o.par.coupling!=SRM_COUPLING_SHARED ) ERROR_exit("-repeated_stimulus averages the runs, so there is no W per run (-run_coupling)") ;
     if( o.model_in!=NULL || o.apply_W!=NULL ) ERROR_exit("-repeated_stimulus is for fitting: not with -model_in or -apply_W") ;
   }
   if( o.cv_fit!=NULL || o.balance!=0 ){
     if( strcasecmp(o.cvmode,"none")==0 ) ERROR_exit("-cv_fit and -balance need -cv run or -cv loso: they decide how the CV models are fit") ;
     if( o.model_in!=NULL || o.apply_W!=NULL ) ERROR_exit("-cv_fit and -balance fit groups; with -model_in or -apply_W nothing is fitted") ;
     if( o.balance<0 ) ERROR_exit("-balance must be a positive number of subsets") ;
     if( o.cv_fit!=NULL && strcasecmp(o.cv_fit,"pooled") && strcasecmp(o.cv_fit,"within") && strncasecmp(o.cv_fit,"reference:",10) )
       ERROR_exit("-cv_fit must be pooled, within, or reference:GROUP, not '%s'",o.cv_fit) ;
     if( o.cv_fit!=NULL && !strncasecmp(o.cv_fit,"reference:",10) && strcasecmp(o.cvmode,"loso") )
       ERROR_exit("-cv_fit reference scores the reference group leave-one-subject-out and the others as new subjects: use -cv loso") ;
   }
   if( o.compress_given && o.no_compress ) ERROR_exit("give -compress or -no_compress, not both") ;
   if( o.compress ){                                  /* given explicitly */
     if( o.model_in!=NULL || o.apply_W!=NULL )
       ERROR_exit("-compress is for fitting a group: -model_in and -apply_W hold only the new subjects, one at a time") ;
   }
   if( o.update_S && o.model_in==NULL ) ERROR_exit("-update_S needs -model_in") ;
   if( o.model_in!=NULL && o.apply_W!=NULL ) ERROR_exit("-model_in and -apply_W cannot be combined") ;
   if( o.model_in!=NULL || o.apply_W!=NULL ){
     const char *who = o.model_in!=NULL ? "-model_in" : "-apply_W" ;
     const char *from = o.model_in!=NULL ? "the model" : "the earlier run's model" ;
     if( o.nk>0 )             ERROR_exit("-nfeatures is defined by %s; do not give it with %s",from,who) ;
     if( o.given_algo )       ERROR_exit("-algorithm is defined by %s; do not give it with %s",from,who) ;
     if( o.given_zscore )     ERROR_exit("-zscore is defined by %s; do not give it with %s",from,who) ;
     if( o.given_coupling )   ERROR_exit("-run_coupling is defined by %s; do not give it with %s",from,who) ;
     if( o.given_seed )       ERROR_exit("-seed is defined by %s; do not give it with %s",from,who) ;
     if( o.nstart!=1 )        ERROR_exit("-nstart is not used with %s: nothing is fitted from scratch",who) ;
     if( (o.given_niter || o.given_tol) && !o.update_S )
                              ERROR_exit("-niter and -tol are defined by %s; with %s they apply only to -update_S",from,who) ;
     if( o.model_in!=NULL ){
       if( o.censor_name!=NULL ) ERROR_exit("-censor is defined by the model; do not give it with -model_in") ;
       if( o.cenmode==SRM_CEN_DROP ) ERROR_exit("-cenmode drop would change the model's timeline; with -model_in, censored "
                                                "time points are interpolated (-cenmode interp)") ;
       if( strcasecmp(o.cvmode,"loso")==0 )
         ERROR_exit("-cv loso needs a group fit to leave subjects out of; with -model_in use -cv run, which scores each subject on runs the model and their W never saw") ;
     } else {
       if( o.maskname!=NULL )    ERROR_exit("-apply_W takes the voxels from W: do not give -mask") ;
       if( strcasecmp(o.cvmode,"none")!=0 || o.cv_metrics!=NULL || o.save_r2 )
                                 ERROR_exit("-apply_W only projects: it takes no -cv, -cv_metrics or -save_r2") ;
       if( o.save_W || o.save_recon ) ERROR_exit("-apply_W only projects: it takes no -save_W or -save_recon") ;
     }
     if( o.model_in!=NULL ) srm_run_model_in(&o,tab,argc,argv) ; else srm_run_apply_W(&o,tab,argc,argv) ;
     if( !o.quiet ) INFO_message("done") ;
     return 0 ;
   }
   if( o.nk<1 ) ERROR_exit("-nfeatures is required") ;
   do_cv=(strcasecmp(o.cvmode,"none")!=0) ;
   if( o.nk>1 && !do_cv ) ERROR_exit("several -nfeatures values need -cv: they are compared on held-out data") ;
   if( !do_cv && (o.cv_metrics!=NULL || o.save_r2) ) ERROR_exit("-cv_metrics and -save_r2 need -cv run or -cv loso") ;
   for( ii=0 ; ii<o.nk ; ii++ ) if( o.klist[ii]>K ) K=o.klist[ii] ;       /* K is the largest until the loop below */
   if( !isfinite(o.zcensor_warn) || o.zcensor_warn<0.0f || o.zcensor_warn>100.0f ) ERROR_exit("-zcensor_warn must be between 0 and 100") ;

   if( o.par.coupling!=SRM_COUPLING_SHARED && THD_datatable_column(tab,o.run_col)<0 )
     ERROR_exit("-run_coupling needs runs: the table has no '%s' column",o.run_col) ;

   /*-- subjects, runs, masks, and the censor mask --*/

   if( o.axis_item ){
     srm_data_items(&o,tab,&d,2) ;
     srm_data_shape_items(&o,&d,1,K) ;
   } else {
     srm_data_table(&o,tab,&d,2) ;
     srm_data_shape(&o,&d,1,K) ;
     srm_data_censor(&o,&d,NULL) ;
   }
   nsub=d.nsub ; nrun=d.nrun ; sb=d.sb ; Tin=d.Tin ; glabel=d.glabel ; ngroup=d.ngroup ; runname=d.runname ;
   have_run=d.have_run ; dx=d.dx ; icol_mask=d.icol_mask ; icol_group=d.icol_group ; runlen=d.runlen ; c0in=d.c0in ;

   /*-- do not clobber anything: check every output now, not after the work --*/

   if( !THD_ok_overwrite() ){
     char fn[THD_MAX_NAME] ;
     snprintf(fn,sizeof(fn),"%s.S.1D",o.prefix) ;   if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ;
     snprintf(fn,sizeof(fn),"%s.srm",o.prefix) ;    if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ;
     snprintf(fn,sizeof(fn),"%s.fit.txt",o.prefix) ; if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ;
     snprintf(fn,sizeof(fn),"%s.objective.1D",o.prefix) ; if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ;
     if( o.par.algo==SRM_ALGO_PROB ){ snprintf(fn,sizeof(fn),"%s.rho2.1D",o.prefix) ; if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ; }
     if( do_cv ){
       snprintf(fn,sizeof(fn),"%s.scores.txt",o.prefix) ; if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ;
       snprintf(fn,sizeof(fn),"%s.scores_folds.txt",o.prefix) ; if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ;
       if( o.nk>1 ){ snprintf(fn,sizeof(fn),"%s.nfeat.txt",o.prefix) ; if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ; }
       if( o.balance>0 ){ snprintf(fn,sizeof(fn),"%s.balance.txt",o.prefix) ; if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ; }
     }
     if( o.par.coupling!=SRM_COUPLING_SHARED ){ snprintf(fn,sizeof(fn),"%s.coupling.txt",o.prefix) ; if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ; }
     if( o.nperm>0 ){ snprintf(fn,sizeof(fn),"%s.tests.txt",o.prefix) ; if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ; }
     if( o.rel_split ){
       snprintf(fn,sizeof(fn),"%s.reliability.txt",o.prefix) ; if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ;
       snprintf(fn,sizeof(fn),"%s.reliability_splits.txt",o.prefix) ; if( THD_is_file(fn) ) ERROR_exit("'%s' already exists (use -overwrite)",fn) ;
     }
   }
   srm_check_subject_outputs(&o,&d) ;

   /*-- compress (the default) when it saves memory: every subject has more features than time points (or items) --*/
   /* (with fewer voxels than time points R would be no smaller than X, so there is nothing to gain) */
   if( !o.no_compress && !o.compress_given ){
     int ss2 , lim=o.axis_item ? d.nitem : d.Tin , few=0 ;
     for( ss2=0 ; ss2<nsub ; ss2++ ) if( d.sb[ss2].nvox_mask<=lim ) few++ ;
     o.compress=(few==0) ;
     if( few>0 && !o.quiet )
       INFO_message("each subject held whole (not compressed): %d subject%s %s no more features than %s (%d)",
                    few,few==1?"":"s",few==1?"has":"have",o.axis_item?"items":"time points",lim) ;
   }

   /*-- the memory contract, before the big allocations --*/

   srm_memory_preflight(&o,&d,K,do_cv,(o.par.coupling!=SRM_COUPLING_SHARED) ? nrun : 0) ;

   /*-- read everything, then the retained timeline, then normalize --*/

   if( o.compress ){                                  /* one subject at a time, each kept as its factor R */
     srm_ctx lc ; int use_lc=(o.axis_item && o.rel_within) ;
     if( use_lc ){                                       /* the -score_by labels, from the table, for item reliability */
       srm_opts oq=o ; int q ;
       oq.quiet=1 ; memset(&lc,0,sizeof(lc)) ;
       lc.o=&oq ; lc.sb=d.sb ; lc.nsub=nsub ; lc.quiet=1 ; d.T=d.nitem ;
       srm_build_labels(&oq,&d,&lc) ;
       lc.rel=(double *)malloc(sizeof(double)*2*(size_t)nsub*(1+lc.L)) ;
       for( q=0 ; q<2*nsub*(1+lc.L) ; q++ ) lc.rel[q]=NAN ;
     }
     srm_data_compress_stream(&o,&d,K,use_lc ? &lc : NULL) ;
     if( use_lc ) d.relitems=lc.rel ;
     nrun=d.nrun ;
   } else {
   if( o.axis_item ){
     srm_data_read_items(&o,&d) ;
     srm_data_items_timeline(&d) ;
     if( d.T<K ) ERROR_exit("%d items; -nfeatures is %d",d.T,K) ;
   } else {
     srm_data_read(&o,&d) ;
     srm_data_subject_censor(&o,&d,1) ;
     srm_data_timeline(&o,&d,K) ;
   }
   srm_data_normalize(&o,&d,o.zscore,K,0) ;
   if( o.repeated_stimulus ){ srm_average_repeats(&o,&d) ; nrun=d.nrun ; if( d.T<K ) ERROR_exit("%d time points per run; -nfeatures is %d",d.T,K) ; }
   }
   T=d.T ; seglen=d.seglen ; time_index=d.time_index ; censor_keep=d.keep ;

   /*-- the fits, and cross-validation --*/
   {
     srm_ctx cx ; srm_want want ; srm_folds folds ; int ik , rc ;
     int *carry=NULL , ncarry=0 , icol_run=THD_datatable_column(tab,o.run_col) ;
     int *nvox_v=(int *)malloc(sizeof(int)*(size_t)nsub) ; float **xv=(float **)malloc(sizeof(float *)*(size_t)nsub) ;
     double *kbl=NULL ; FILE *kfp=NULL ; char **gl=glabel ;
     memset(&cx,0,sizeof(cx)) ;
     cx.o=&o ; cx.sb=sb ; cx.nsub=nsub ; cx.nrun=nrun ; cx.T=T ; cx.seglen=seglen ; cx.runname=runname ;
     cx.glabel=gl ; cx.ngroup=ngroup ; cx.quiet=o.quiet ; cx.progress=o.progress ;
     const unsigned char **obs_v=(const unsigned char **)calloc((size_t)nsub,sizeof(unsigned char *)) ;
     int fitmode=SRM_FIT_POOLED , refg=-1 ; char cvdesc[256] ;
     for( ss=0 ; ss<nsub ; ss++ ){ nvox_v[ss]=sb[ss].nvox ; xv[ss]=sb[ss].x ; obs_v[ss]=sb[ss].obs ; if( sb[ss].obs!=NULL ) cx.anymiss=1 ; }
     memset(&want,0,sizeof(want)) ; memset(&folds,0,sizeof(folds)) ; snprintf(cvdesc,sizeof(cvdesc),"%s",o.cvmode) ;

     if( do_cv ){
       want=srm_parse_metrics(o.cv_metrics) ; want.ident=o.axis_item ;
       if( o.cv_fit!=NULL && !strcasecmp(o.cv_fit,"within") ) fitmode=SRM_FIT_WITHIN ;
       else if( o.cv_fit!=NULL && !strncasecmp(o.cv_fit,"reference:",10) ){
         fitmode=SRM_FIT_REFERENCE ;
         for( refg=0 ; refg<ngroup ; refg++ ) if( !strcmp(glabel[refg],o.cv_fit+10) ) break ;
         if( refg==ngroup ) ERROR_exit("-cv_fit %s: there is no group '%s' in the '%s' column",o.cv_fit,o.cv_fit+10,o.group_col) ;
       }
       if( (fitmode!=SRM_FIT_POOLED || o.balance>0) && ngroup<2 )
         ERROR_exit("-cv_fit %s and -balance compare groups: the table needs a '%s' column with at least two groups",
                    o.cv_fit?o.cv_fit:"pooled",o.group_col) ;
       { int g , nmin=nsub , need=(strcasecmp(o.cvmode,"loso")==0) ? 3 : 2 ;
         for( g=0 ; g<ngroup ; g++ ){
           int cnt=0 ; for( ss=0 ; ss<nsub ; ss++ ) cnt+=(sb[ss].group==g) ;
           if( cnt<nmin ) nmin=cnt ;
           if( fitmode==SRM_FIT_WITHIN && cnt<need )
             ERROR_exit("-cv_fit within: group %s has %d subjects; -cv %s needs at least %d",glabel[g],cnt,o.cvmode,need) ;
           if( fitmode==SRM_FIT_REFERENCE && g==refg && cnt<3 )
             ERROR_exit("-cv_fit %s: the reference group has %d subjects; leave-one-out needs at least 3",o.cv_fit,cnt) ;
         }
         if( o.balance>0 && nmin<need ) ERROR_exit("-balance: the smallest group has %d subjects; subsets need at least %d",nmin,need) ;
       }
       snprintf(cvdesc,sizeof(cvdesc),"%s%s%s%s",o.cvmode,fitmode==SRM_FIT_POOLED?"":", -cv_fit ",
                fitmode==SRM_FIT_POOLED?"":o.cv_fit,"") ;
       if( o.balance>0 ){ int L=(int)strlen(cvdesc) ; snprintf(cvdesc+L,sizeof(cvdesc)-L,", -balance %d",o.balance) ; }
       if( o.axis_item ) srm_make_item_folds(&o,&d,&folds,strcasecmp(o.cvmode,"loso")==0) ;
       else              srm_make_folds(&folds,nrun,seglen,runname,T,o.cv_blocks) ;
       srm_check_fold_lengths(&folds,want,o.match_window) ;
       if( o.cv_adapt>0.0 ){ int ff ;                  /* each held-out run: enough to estimate its W, and to score */
         for( ff=0 ; ff<folds.nfold ; ff++ ){
           int na=srm_adapt_n(o.cv_adapt,folds.len[ff]) , nr=folds.len[ff]-na , need=(want.match ? 2*o.match_window+1 : 3) ;
           if( na<K ) ERROR_exit("-cv_adapt %g: run %s has %d time points, so its W would be estimated from %d; need at least "
                                 "%d (-nfeatures)",o.cv_adapt,folds.name[ff],folds.len[ff],na,K) ;
           if( nr<need ) ERROR_exit("-cv_adapt %g leaves %d time points of run %s to score; need at least %d%s",o.cv_adapt,nr,
                                    folds.name[ff],need,want.match?" for time-segment matching (see -match_window)":"") ;
         }
         if( !o.quiet ) ININFO_message("-cv_adapt %g: each held-out run's own W is estimated from its first %g of samples, "
                                       "tied to the anchor; the rest are scored, through it and through the anchor",o.cv_adapt,o.cv_adapt) ;
       }
       { int ff , big=0 ;
         for( ff=1 ; ff<folds.nfold ; ff++ ) if( folds.len[ff]>folds.len[big] ) big=ff ;
         if( T-folds.len[big]<K ) ERROR_exit("holding out fold %s leaves %d %s to fit; -nfeatures is %d",
                                             folds.name[big],T-folds.len[big],folds.unit,K) ; }
       if( !o.quiet ){
         if( o.axis_item ){
           char lv[256] ; int ff , pos=0 ;
           for( ff=0 ; ff<folds.nfold && pos<200 ; ff++ ) pos+=snprintf(lv+pos,sizeof(lv)-pos,"%s%s (%d)",ff?", ":"",folds.name[ff],folds.len[ff]) ;
           if( ff<folds.nfold ) snprintf(lv+pos,sizeof(lv)-pos,", ...") ;
           INFO_message("-cv %s: %d folds of the items, from column %s: %s",o.cvmode,folds.nfold,
                        o.fold_col!=NULL?o.fold_col:o.run_col,lv) ;
           ININFO_message("%s; scored by item identification (chance is 1 over the items in the fold)",
                          strcasecmp(o.cvmode,"loso")==0 ? "each subject scored by a model that never saw them" : "the group refitted without each fold") ;
         } else
         INFO_message("-cv %s: %d folds of the %s, %s",o.cvmode,folds.nfold,folds.by_blocks?"time points in blocks":"runs",
                      strcasecmp(o.cvmode,"loso")==0 ? "each subject scored by a model that never saw them" : "the group refitted without each fold") ;
         if( folds.by_blocks ) WARNING_message("one run only: the folds are contiguous time blocks. Time points next to a block's edge "
                                               "resemble the other side, so scores are somewhat optimistic; separate runs are better") ;
       }
       if( o.nk>1 ){
         kbl=(double *)calloc((size_t)o.nk*8,sizeof(double)) ;
       }
       carry=srm_carry_columns(tab,&d,&ncarry) ;
       srm_build_labels(&o,&d,&cx) ;
       if( o.rel_within ) srm_reliability_within(&d,&cx) ;
     }

     for( ik=0 ; ik<o.nk ; ik++ ){
       /* one pass per -nfeatures value: the full fit, its held-out scores, and (only when a single K was
          given) the outputs.  With several K, only the comparison table PREFIX.nfeat.txt is written. */
       SRM_model *m=NULL ; SRM_fitinfo fi ; srm_prog bar ; fit_prog fp ; SRM_params par=o.par ;
       double *start_obj=(double *)calloc((size_t)o.nstart,sizeof(double)) ; int best=0 ;
       srm_cvres res ; int have_res=0 ;
       K=o.klist[ik] ; cx.K=K ;
       par.nfeat=K ; par.center=(o.zscore==SRM_ZSCORE_NONE) ;   /* z-scored data are already centered */
       if( par.coupling!=SRM_COUPLING_SHARED ){ par.nseg=nrun ; par.seglen=seglen ; }
       cx.par=par ;                                   /* the CV fits set their own masks */
       if( cx.anymiss ) par.obs=(const unsigned char * const *)obs_v ;
       if( !o.quiet ){
         INFO_message("%sfitting a %s SRM: %d subjects, %d components, %d %s, up to %d iterations%s",
                      o.nk>1?"[-nfeatures ] ":"",par.algo==SRM_ALGO_PROB?"probabilistic":"deterministic",nsub,K,T,
                      d.axis_item?"items":"time points",par.niter,
                      par.coupling!=SRM_COUPLING_SHARED?", a W per run tied to the subject's anchor":"") ;
         if( par.coupling!=SRM_COUPLING_SHARED ) ININFO_message("it starts from the shared-W fit, then adds the per-run mappings") ;
       }
       srm_prog_begin(&bar,o.progress,o.quiet,"fit","iterations",par.niter) ;
       fp.bar=&bar ; fp.istart=0 ; fp.nstart=o.nstart ; fp.first_obj=0.0 ; fp.tag=NULL ;
       par.progress=srm_fit_progress ; par.progress_ctx=&fp ;
       { srm_cmp cm ; srm_cmp_set(sb,NULL,NULL,nsub,K,&par,&cm) ;
         rc=SRM_fit_multistart(nsub,T,nvox_v,xv,&par,o.nstart,&m,&fi,start_obj,&best) ;
         srm_cmp_free(&cm) ; }
       srm_prog_end(&bar) ;
       if( rc!=SRM_OK ) ERROR_exit("the fit failed: %s",SRM_strerror(rc)) ;
       if( !o.quiet ){
         INFO_message("%s after %d iterations (objective %.10g)",fi.converged?"converged":"stopped at the iteration limit",
                      fi.niter_done,fi.objective[fi.niter_done]) ;
         if( !fi.converged ) ININFO_message("not converged to -tol %.3g: the fit still depends on the seed; raise -niter or relax -tol",par.tol) ;
         if( o.nstart>1 ){
           double lo=start_obj[0] , hi=start_obj[0] ;
           for( ii=1 ; ii<o.nstart ; ii++ ){ if( start_obj[ii]<lo ) lo=start_obj[ii] ; if( start_obj[ii]>hi ) hi=start_obj[ii] ; }
           ININFO_message("best of %d starts: start %d; objectives span %.3g relative",o.nstart,best+1,
                          fabs(hi-lo)/(fabs(start_obj[best])>0.0?fabs(start_obj[best]):1.0)) ;
         }
       }
       rc=SRM_canonicalize(m) ;
       if( rc!=SRM_OK ) ERROR_exit("could not put the model in its canonical gauge: %s",SRM_strerror(rc)) ;
       /* canonical gauge: S rotated to uncorrelated rows ordered by variance, signs fixed (thd_srm.h), so
          component numbers are reproducible; every W is rotated with it, so W S is unchanged */

       /*-- held-out scoring --*/
       if( do_cv ){
         have_res=1 ;
         srm_cvres_alloc(&res,nsub,folds.nfold,!o.no_projected && o.nk==1 && o.balance==0,o.save_r2 && o.nk==1,&cx) ;
         if( fitmode!=SRM_FIT_POOLED || o.balance>0 ){
           FILE *bfp=NULL ;
           if( o.balance>0 && ik==o.nk-1 ){
             char fn[THD_MAX_NAME] ; snprintf(fn,sizeof(fn),"%s.balance.txt",o.prefix) ;
             bfp=fopen(fn,"w") ; if( bfp==NULL ) ERROR_exit("cannot write '%s'",fn) ;
             fprintf(bfp,"# the subsets of -balance %d (-cv_fit %s, seed %u): each line is one fit\n",o.balance,
                     o.cv_fit?o.cv_fit:"pooled",o.par.seed) ;
           }
           srm_cv_groups(&cx,m,&folds,want,&res,fitmode,refg,o.balance,bfp) ;
           if( bfp!=NULL ){ fclose(bfp) ; if( !o.quiet ) INFO_message("wrote %s.balance.txt",o.prefix) ; }
         } else srm_cv_dispatch(&cx,m,&folds,want,&res) ;
         srm_cv_summary(&cx,&res,want,cvdesc,folds.nfold) ;
         if( o.nk>1 ){                                    /* one row per K for the comparison table */
           int met , mi[4]={M_ISC,M_MATCH,M_CHANCE,M_R2} ;
           for( met=0 ; met<4 ; met++ ){
             double s=0.0 , s2=0.0 ; int cnt=0 ;
             for( ss=0 ; ss<nsub ; ss++ ){ double v=srm_subj_metric(&res,ss,mi[met]) ; if( isfinite(v) ){ s+=v ; s2+=v*v ; cnt++ ; } }
             kbl[(size_t)ik*8+2*met]=cnt>0 ? s/cnt : NAN ;
             kbl[(size_t)ik*8+2*met+1]=(cnt>1) ? sqrt(fmax(0.0,(s2-s*s/cnt)/(cnt-1))/cnt) : NAN ;
           }
         }
       }
       if( o.nk>1 ){                                     /* comparing K only: no model outputs */
         if( have_res ) srm_cvres_free(&res) ;
         SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; free(start_obj) ;
         continue ;
       }

       /*-- outputs --*/
       {
         char fn[THD_MAX_NAME] , cm[256] ; FILE *fq ;
         snprintf(fn,sizeof(fn),"%s.S.1D",o.prefix) ;
         if( d.axis_item ){
           char *ck ;
           snprintf(cm,sizeof(cm),"shared response: %d items x %d components, ordered by variance",T,K) ;
           ck=srm_keys_comment(cm,d.item_key,T) ;
           srm_write_transposed_1D(fn,m->s,K,T,ck) ; free(ck) ;
         } else {
           snprintf(cm,sizeof(cm),"shared response: %d time points (retained) x %d components, ordered by variance",T,K) ;
           srm_write_transposed_1D(fn,m->s,K,T,cm) ;
         }
         if( !o.quiet ) INFO_message("wrote %s",fn) ;

         snprintf(fn,sizeof(fn),"%s.objective.1D",o.prefix) ;
         fq=fopen(fn,"w") ; if( fq==NULL ) ERROR_exit("cannot write '%s'",fn) ;
         fprintf(fq,"# objective at the start of each iteration, and at the end (%s)\n",
                 par.algo==SRM_ALGO_PROB?"log-likelihood, rises":"summed squared error, falls") ;
         for( ii=0 ; ii<=fi.niter_done ; ii++ ) fprintf(fq,"%.12g\n",fi.objective[ii]) ;
         fclose(fq) ;

         if( par.algo==SRM_ALGO_PROB ){
           snprintf(fn,sizeof(fn),"%s.rho2.1D",o.prefix) ;
           fq=fopen(fn,"w") ; if( fq==NULL ) ERROR_exit("cannot write '%s'",fn) ;
           fprintf(fq,"# noise variance per subject, in the order of the table:") ;
           for( ss=0 ; ss<nsub ; ss++ ) fprintf(fq," %s",sb[ss].label) ;
           fputc('\n',fq) ;
           for( ss=0 ; ss<nsub ; ss++ ) fprintf(fq,"%.9g\n",m->rho2[ss]) ;
           fclose(fq) ;
         }

         snprintf(fn,sizeof(fn),"%s.fit.txt",o.prefix) ;
         fq=fopen(fn,"w") ; if( fq==NULL ) ERROR_exit("cannot write '%s'",fn) ;
         fprintf(fq,"# 3dSRM fit: %s, %d components, %d subjects, %d %s (%d runs), seed %u\n",
                 par.algo==SRM_ALGO_PROB?"probabilistic":"deterministic",K,nsub,T,d.axis_item?"items":"time points",nrun,par.seed) ;
         fprintf(fq,"# iterations %d, converged %d, tol %.3g, final objective %.12g\n",fi.niter_done,fi.converged,par.tol,fi.objective[fi.niter_done]) ;
         fprintf(fq,"# starts %d, kept start %d, objectives",o.nstart,best+1) ;
         for( ii=0 ; ii<o.nstart ; ii++ ) fprintf(fq," %.12g",start_obj[ii]) ;
         fputc('\n',fq) ;
         if( d.axis_item )
           fprintf(fq,"# axis item: %d item columns; zscore %s\n",T,o.zscore==SRM_ZSCORE_RUN?"run (over the items of each subject's own runs)":
                                                                        o.zscore==SRM_ZSCORE_ALL?"all (over all of each subject's items)":"none") ;
         else
           fprintf(fq,"# zscore %s, censored %d of %d input time points%s\n",o.zscore==SRM_ZSCORE_RUN?"run":o.zscore==SRM_ZSCORE_ALL?"all":"none",
                   o.repeated_stimulus ? 0 : Tin-T,Tin,o.repeated_stimulus ? "; the repeated runs averaged (-repeated_stimulus)" : "") ;
         fprintf(fq,"Subj\tGroup\tFeatures_in_mask\tFeatures_used\tFeatures_dropped\trho2\tCensored\n") ;
         for( ss=0 ; ss<nsub ; ss++ )
           fprintf(fq,"%s\t%s\t%d\t%d\t%d\t%s\t%d\n",sb[ss].label,glabel[sb[ss].group],sb[ss].compressed ? sb[ss].nvox_mask0 : sb[ss].nvox_mask,
                   sb[ss].compressed ? (int)sb[ss].ndim : sb[ss].nvox,sb[ss].ndrop,
                   par.algo==SRM_ALGO_PROB ? (snprintf(cm,sizeof(cm),"%.9g",m->rho2[ss]),cm) : "NA",sb[ss].ncens) ;
         fclose(fq) ;

         if( par.coupling!=SRM_COUPLING_SHARED ){
           snprintf(fn,sizeof(fn),"%s.coupling.txt",o.prefix) ;
           fq=fopen(fn,"w") ; if( fq==NULL ) ERROR_exit("cannot write '%s'",fn) ;
           fprintf(fq,"# deviation of each run's W from the subject's anchor: |W_ir - W_i|^2 / %d (0 identical, 2 unrelated); lambda %.6g\n",K,par.coupling) ;
           fprintf(fq,"Subj\tGroup\tRun\tDeviation\n") ;
           for( ss=0 ; ss<nsub ; ss++ ) for( rr=0 ; rr<nrun ; rr++ ){
             double dv ; SRM_run_deviation(m,ss,rr,&dv) ;
             fprintf(fq,"%s\t%s\t%s\t%.6g\n",sb[ss].label,glabel[sb[ss].group],runname[rr],dv) ;
           }
           fclose(fq) ;
         }
         if( have_res ){
           srm_write_scores(&cx,&folds,&res,want,cvdesc,tab,carry,ncarry) ;
           if( o.nperm>0 ) srm_perm_tests(&cx,&res,want,o.nperm) ;
           if( !o.quiet ) INFO_message("wrote %s.scores.txt and %s.scores_folds.txt",o.prefix,o.prefix) ;
         }
       }

       /* the model: what a later run needs to take in new subjects */
       {
         char fn[THD_MAX_NAME] ; SRM_provenance *pv=SRM_provenance_new() ;
         snprintf(fn,sizeof(fn),"%s.srm",o.prefix) ;
         pv->zscore=o.zscore ;
         if( d.axis_item ){
           pv->axis=SRM_AXIS_ITEM ; pv->nitem=T ; pv->item_key=(char **)malloc(sizeof(char *)*(size_t)T) ;
           for( ii=0 ; ii<T ; ii++ ) pv->item_key[ii]=strdup(d.item_key[ii]) ;
         } else if( o.repeated_stimulus ){             /* the model's timeline is the averaged run */
           pv->axis=SRM_AXIS_TIME ; pv->ntime_in=T ; pv->nseg=1 ; pv->keep=NULL ;
           pv->seglen=(int *)malloc(sizeof(int)) ; pv->seglen[0]=T ;
         } else {
           pv->axis=SRM_AXIS_TIME ; pv->ntime_in=Tin ; pv->nseg=nrun ;
           pv->seglen=(int *)malloc(sizeof(int)*(size_t)nrun) ; memcpy(pv->seglen,seglen,sizeof(int)*(size_t)nrun) ;
           pv->keep=(unsigned char *)malloc((size_t)Tin) ; memcpy(pv->keep,censor_keep,(size_t)Tin) ;
         }
         pv->nsubj=nsub ; pv->label=(char **)malloc(sizeof(char *)*(size_t)nsub) ;
         for( ss=0 ; ss<nsub ; ss++ ){
           char sn[128] ; srm_safe(sn,sizeof(sn),sb[ss].label) ; pv->label[ss]=strdup(sn) ;
         }
         { int *nvr=NULL ;                             /* -compress: the model's subjects have their voxel counts */
           /* the model holds each compressed subject with nvox = rank; swap the real feature count (ndim)
              in for the save, so the file records voxels rather than ranks, and restore it afterwards */
           if( sb[0].compressed ){ nvr=(int *)malloc(sizeof(int)*(size_t)nsub) ;
             for( ss=0 ; ss<nsub ; ss++ ){ nvr[ss]=m->nvox[ss] ; m->nvox[ss]=(int)sb[ss].ndim ; } }
           rc=SRM_save_model(fn,m,pv) ;
           if( nvr!=NULL ){ for( ss=0 ; ss<nsub ; ss++ ) m->nvox[ss]=nvr[ss] ; free(nvr) ; } }
         if( rc!=SRM_OK ) ERROR_exit("cannot write the model '%s': %s",fn,SRM_strerror(rc)) ;
         SRM_provenance_free(pv) ;
         if( !o.quiet ) INFO_message("wrote %s",fn) ;
       }

       /* per subject: the data in the shared space (held out, under -cv), and the maps */
       if( sb[0].compressed && (o.save_W || o.save_recon || o.save_r2) ) srm_write_outputs_reread(&o,&d,m,have_res?&res:NULL,K,argc,argv) ;
       else srm_write_subject_outputs(&o,&d,m,0,have_res?&res:NULL,K,argc,argv) ;
       if( o.rel_split ) srm_reliability_split(&cx,m,o.nsplit) ;
       if( have_res ) srm_cvres_free(&res) ;
       SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; free(start_obj) ;
     }

     /*-- comparing numbers of components --*/
     if( o.nk>1 ){
       char fn[THD_MAX_NAME] ; int best_isc=-1 , best_match=-1 , best_r2=-1 ;
       snprintf(fn,sizeof(fn),"%s.nfeat.txt",o.prefix) ;
       kfp=fopen(fn,"w") ; if( kfp==NULL ) ERROR_exit("cannot write '%s'",fn) ;
       fprintf(kfp,"# held-out scores by number of components (%s; mean over %d subjects, with the standard error across subjects)\n",o.cvmode,nsub) ;
       fprintf(kfp,"nfeatures\tisc\tisc_se\t%s\t%s_se\tchance\tchance_se\tr2\tr2_se\n",want.ident?"ident":"match",want.ident?"ident":"match") ;
       for( ik=0 ; ik<o.nk ; ik++ ){
         int q ;
         fprintf(kfp,"%d",o.klist[ik]) ;
         for( q=0 ; q<8 ; q++ ){ double v=kbl[(size_t)ik*8+q] ; if( isfinite(v) ) fprintf(kfp,"\t%.7g",v) ; else fprintf(kfp,"\tNA") ; }
         fputc('\n',kfp) ;
         if( want.isc   && (best_isc<0   || kbl[(size_t)ik*8+0]>kbl[(size_t)best_isc*8+0]) )   best_isc=ik ;
         if( want.match && (best_match<0 || kbl[(size_t)ik*8+2]>kbl[(size_t)best_match*8+2]) ) best_match=ik ;
         if( want.r2    && (best_r2<0    || kbl[(size_t)ik*8+6]>kbl[(size_t)best_r2*8+6]) )    best_r2=ik ;
       }
       fclose(kfp) ;
       if( !o.quiet ){
         INFO_message("wrote %s",fn) ;
         if( best_isc>=0 )   INFO_message("best -nfeatures by held-out isc: %d",o.klist[best_isc]) ;
         if( best_match>=0 ) INFO_message("best -nfeatures by %s: %d",want.ident?"item identification":"segment matching",o.klist[best_match]) ;
         if( best_r2>=0 )    INFO_message("best -nfeatures by held-out r2: %d",o.klist[best_r2]) ;
         INFO_message("no model was written; rerun with the -nfeatures you choose for the model and its maps") ;
       }
     }
     free(nvox_v) ; free(xv) ; free(obs_v) ;
     if( do_cv ) srm_free_folds(&folds) ;
   }
   if( !o.quiet ) INFO_message("done") ;
   return 0 ;
}
