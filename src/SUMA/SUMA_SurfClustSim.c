#include "SUMA_suma.h"
#include "SUMA_SurfClustSim_core.h"
#include "vol2surf.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#include <errno.h>
#include <limits.h>
#include <stdint.h>

#define SSCS_PROGRAM "SurfClustSim"
#define SSCS_VERSION "0.3.0"
#define SSCS_AUTHOR  "P. Molfese"
#define SSCS_DATE    "20 Aug 2026"
#define SSCS_NMODE 4

enum {
   SSCS_ONE_SIDED = 0,
   SSCS_TWO_SIDED = 1,
   SSCS_BI_SIDED = 2,
   SSCS_LEGACY_SIDED = 3
};

typedef struct {
   int on_surface;
   int compat;
   int compat_explicit;
   int acf_mode;
   double acf_a, acf_b, acf_c;
   int acf_nbasis;
   float acf_radius;
   int fixed_explicit;
   int smooth_niter_given;
   int niter;
   int itersize;
   int smooth_niter;
   int max_smooth_niter;
   int nthreads;
   int do_1D;
   int do_niml;
   int do_maxarea;
   int do_mthresh;
   int selfcheck;
   int selected_sides;
   int side[SSCS_NMODE];
   int verb;
   int surface_nnode;
   int surface_nmask;
   double surface_area;
   float rmm;
   double target_fwhm;
   double sigma;
   unsigned long long seed;
   char *prefix;
   char *surf_mask_name;
   char *vol_mask_name;
   char *map_func;
   int f_steps;
   double *pthr;
   int npthr;
   double *athr;
   int nathr;
} SSCS_OPTIONS;

typedef struct {
   int total;
   int nmark;
   int next_mark;
   int printed;
} SSCS_PROGRESS;

static const double sscs_default_pthr[] = {
   0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005, 0.0002, 0.0001
};
static const double sscs_default_athr[] = {0.10, 0.05, 0.02, 0.01};
static const double sscs_lots_pthr[] = {
   0.10, 0.09, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.015,
   0.01, 0.007, 0.005, 0.003, 0.002, 0.0015, 0.001, 0.0007,
   0.0005, 0.0003, 0.0002, 0.00015, 0.0001, 0.00007, 0.00005,
   0.00003, 0.00002, 0.000015, 0.00001
};
static const double sscs_lots_athr[] = {
   0.10, 0.09, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01
};

static const char *sscs_mode_label(int mode)
{
   switch (mode) {
      case SSCS_ONE_SIDED: return "1sided";
      case SSCS_TWO_SIDED: return "2sided";
      case SSCS_BI_SIDED: return "bisided";
      default: return "legacy";
   }
}

static const char *sscs_mode_description(int mode)
{
   switch (mode) {
      case SSCS_ONE_SIDED:
         return "positive values; upper-tail probability is pthr";
      case SSCS_TWO_SIDED:
         return "positive and negative values clustered together; each tail is pthr/2";
      case SSCS_BI_SIDED:
         return "positive and negative values clustered separately; each tail is pthr/2";
      default:
         return "slow_surf_clustsim compatibility: absolute threshold with each tail pthr";
   }
}

static void sscs_report_openmp(const SSCS_OPTIONS *opt)
{
#ifdef USE_OMP
   int nproc = omp_get_num_procs();
   int nblocks = opt->niter / opt->itersize + (opt->niter % opt->itersize != 0);
   int workers = MIN(opt->nthreads, nblocks);
   INFO_message(
      "OpenMP: enabled; runtime sees %d processor%s; configured for up to "
      "%d thread%s; %d block%s of -itersize %d to distribute, so up to "
      "%d worker%s",
      nproc, nproc == 1 ? "" : "s",
      opt->nthreads, opt->nthreads == 1 ? "" : "s",
      nblocks, nblocks == 1 ? "" : "s", opt->itersize,
      workers, workers == 1 ? "" : "s");
   if (nblocks < opt->nthreads)
      WARNING_message(
         "Only %d block%s for %d thread%s; -niter/-itersize limits "
         "parallelism here.  Raise -niter, or lower -itersize (which in "
         "-compat also changes the adaptive smoothing block, and so the "
         "results).", nblocks, nblocks == 1 ? "" : "s",
         opt->nthreads, opt->nthreads == 1 ? "" : "s");
#else
   (void)opt;
   INFO_message("OpenMP: disabled in this build; simulations used one thread");
#endif
}

static void sscs_progress_begin(SSCS_PROGRESS *progress, int total,
                                const char *label)
{
   progress->total = MAX(total, 1);
   progress->nmark = MIN(progress->total, 50);
   progress->next_mark = 1;
   progress->printed = 0;
   fprintf(stderr, "++   %s: ", label);
   fflush(stderr);
}

static void sscs_progress_update(int completed, int total, void *userdata)
{
   SSCS_PROGRESS *progress = (SSCS_PROGRESS *)userdata;
   (void)total;
   if (!progress) return;
   while (progress->next_mark <= progress->nmark &&
          completed * progress->nmark >=
             progress->next_mark * progress->total) {
      ++progress->printed;
      if (progress->printed % 10 == 0) fputc('.', stderr);
      else fputc('0' + progress->printed % 10, stderr);
      ++progress->next_mark;
      fflush(stderr);
   }
}

static void sscs_progress_end(SSCS_PROGRESS *progress)
{
   if (progress && progress->printed == 0) fputc('.', stderr);
   fputs(" done\n", stderr);
   fflush(stderr);
}

static void sscs_help(void)
{
   printf(
"SurfClustSim - native surface cluster simulation and alpha tables\n"
"========================================================================\n"
"\n"
"PURPOSE\n"
"-------\n"
"SurfClustSim estimates surface-cluster area thresholds under a Gaussian\n"
"noise null hypothesis.  It is a native replacement for the simulation,\n"
"smoothing, SurfClust, and quick.alpha.vals.py stages generated by\n"
"slow_surf_clustsim.py.  Surfaces, smoothing weights, and cluster topology\n"
"are loaded once; simulations run in memory and may use OpenMP.\n"
"\n"
"For each simulated field the program generates noise, applies HEAT_07\n"
"surface smoothing, rescales the field to unit standard deviation, and saves\n"
"the largest cluster area at each nodewise p threshold.  Quantiles of those\n"
"maxima form a C(p,alpha) table: a row is an uncorrected nodewise p threshold\n"
"and a column is a whole-surface corrected alpha level.\n"
"\n"
"SYNOPSIS\n"
"--------\n"
"Surface-domain noise:\n"
"\n"
"  SurfClustSim -spec SPEC -surf_A SURF -on_surface \\\n"
"      SMOOTHING_OPTIONS -niter N -prefix PREFIX [options]\n"
"\n"
"Volume-domain noise mapped between two surfaces:\n"
"\n"
"  SurfClustSim -spec SPEC -surf_A SURF_A -surf_B SURF_B -sv SURFVOL \\\n"
"      -vol_mask MASK SMOOTHING_OPTIONS -niter N -prefix PREFIX [options]\n"
"\n"
"QUICK RECIPE: REPEAT A slow_surf_clustsim.py ANALYSIS\n"
"----------------------------------------------------\n"
"The closest native equivalent to an old surface-domain run with blur=10,\n"
"itersize=10, and pthr_list=(0.05 0.01 0.001) is:\n"
"\n"
"  mkdir -p clust.lh.results.10000\n"
"\n"
"  SurfClustSim                                                     \\\n"
"      -spec fsaverage_SUMA/fsaverage_lh.spec                       \\\n"
"      -surf_A smoothwm -on_surface                                 \\\n"
"      -compat -target_fwhm 10 -itersize 10                         \\\n"
"      -niter 10000 -pthr 0.05 0.01 0.001                           \\\n"
"      -legacy_sided -athr 0.10 0.05 0.02 0.01                     \\\n"
"      -prefix clust.lh.results.10000/surf.lh.clustsim -niml\n"
"\n"
"The historical surf_vol input is not needed when noise is generated directly\n"
"on the surface.  The output of interest is surf.lh.clustsim.legacy.1D\n"
"(and .legacy.niml with -niml).\n"
"\n"
"This is statistical compatibility, not iteration-for-iteration identity.\n"
"The native program uses a different Gaussian random stream.  Also, the old\n"
"quick.alpha.vals.py rounded simulated areas to integers and printed an entire\n"
"area/alpha curve; this program retains continuous areas and directly reports\n"
"the requested alpha quantiles.  Results should agree within Monte Carlo and\n"
"small tabulation differences, not necessarily character for character.\n"
"\n"
"SMOOTHING MODES\n"
"---------------\n"
"Both modes use the same SUMA HEAT_07 weights and iterative smoothing kernel.\n"
"They differ in how the kernel bandwidth and number of passes are selected.\n"
"\n"
"  -compat\n"
"\n"
"    Adaptive, block-level target-FWHM smoothing.  This reproduces the path\n"
"    used by slow_surf_clustsim.py and SurfSmooth with -target_fwhm,\n"
"    -blurmaster, and -detrend_master.\n"
"\n"
"    For every block of B=-itersize fields, the program:\n"
"\n"
"      1. copies the B noise fields to make a smoothing master;\n"
"      2. detrends the master across its B columns;\n"
"      3. repeatedly estimates mean master FWHM and applies one HEAT_07 pass\n"
"         until that mean exceeds -target_fwhm; and\n"
"      4. applies the selected number of passes to the original fields.\n"
"\n"
"    One Niter is therefore selected for the entire block, not independently\n"
"    for every field.  Different blocks can select different values.\n"
"    -target_fwhm is the requested FINAL smoothness, not additional blur.\n"
"\n"
"    If -sigma is omitted, a mesh-aware bandwidth is estimated from target\n"
"    FWHM and mean edge length, as SurfSmooth did.  Supplying -sigma fixes the\n"
"    bandwidth of each pass, but the number of passes remains adaptive.\n"
"    Do not give -Niter with -compat.\n"
"\n"
"    Use -compat when comparing with old slow_surf_clustsim.py results, or\n"
"    whenever reaching a measured target FWHM is more important than speed.\n"
"    It is slower because FWHM is repeatedly re-estimated and the master is\n"
"    smoothed before the original fields are smoothed.\n"
"\n"
"  -fixed\n"
"\n"
"    Fixed-parameter smoothing.  Every field receives exactly K HEAT_07\n"
"    passes with bandwidth S:\n"
"\n"
"      -fixed -sigma S -Niter K\n"
"\n"
"    -target_fwhm is not used in this mode.  Fixed mode does not promise that\n"
"    the final measured smoothness equals a particular value unless S and K\n"
"    have first been calibrated for the same surface, mask, and noise domain.\n"
"    It is faster and its results are independent of -itersize.\n"
"\n"
"    A practical calibration workflow is to run a smaller -compat pilot using\n"
"    the intended surface and mask, inspect the reported sigma and block Niter\n"
"    values, choose a representative Niter (often the median), and then use\n"
"    that sigma/Niter pair in a large -fixed run.  Validate the achieved FWHM\n"
"    before treating the fixed run as interchangeable with adaptive smoothing.\n"
"\n"
"  -sigma S\n"
"\n"
"    Bandwidth of the HEAT_07 kernel for ONE iteration, in surface coordinate\n"
"    units (normally mm).  It is not the final FWHM.  Repeated graph smoothing\n"
"    and surface geometry make the relation between sigma, Niter, and final\n"
"    FWHM nontrivial.  Very small sigma can require thousands of passes and\n"
"    can cause numerical precision warnings on a coarse mesh.\n"
"\n"
"  -target_fwhm F, -blur F\n"
"\n"
"    Requested final FWHM for -compat (default 4).  FWHM is an equivalent\n"
"    Gaussian width in surface coordinate units.  On a curved mesh it is an\n"
"    estimate based on neighboring-node differences.\n"
"\n"
"  -Niter K\n"
"\n"
"    Nonnegative number of HEAT_07 passes for -fixed.  If neither smoothing\n"
"    mode is named, supplying -Niter implies -fixed; otherwise the default is\n"
"    -compat.  -compat and -fixed are mutually exclusive.\n"
"\n"
"  -max_Niter K, -max_smooth_niter K\n"
"\n"
"    Safety ceiling for an adaptive -compat search (default 3000).  A warning\n"
"    is issued if a block reaches this ceiling below the target FWHM.  Raising\n"
"    sigma is often more useful than merely raising this ceiling.\n"
"\n"
"  -acf a b c    (or -ACF)\n"
"  -acf_file FILE\n"
"\n"
"    A third smoothing mode, alternative to -compat and -fixed.  Instead of\n"
"    smoothing white noise to a target FWHM, generate noise whose spatial\n"
"    AUTOCORRELATION matches a requested curve:\n"
"\n"
"      ACF(r) = a*exp(-r*r/(2*b*b)) + (1-a)*exp(-r/c)\n"
"\n"
"    with r a geodesic distance.  -acf_file reads a, b and c from the output\n"
"    of 'SurfFWHM -acf' run on your own data, which is the intended workflow.\n"
"\n"
"    WHY: a FWHM pins the WIDTH of the autocorrelation, not its SHAPE, and\n"
"    cluster extent at high thresholds is governed by the tail.  Smoothing\n"
"    white noise gives a nearly Gaussian tail (a near 1); real data has a\n"
"    much heavier one (a nearer 0.5).  A too-light tail makes simulated\n"
"    clusters too small, so the area threshold comes out too lenient.  This\n"
"    is the same failure that inflated cluster false positives in the volume\n"
"    and the reason 3dClustSim gained -acf.  Measure your data first: if it\n"
"    reports a near 0.9, the ordinary -fixed/-compat noise is already close\n"
"    and this mode buys little.\n"
"\n"
"    HOW: a single heat kernel cannot produce an exponential tail at all, so\n"
"    the noise is built as a weighted sum of INDEPENDENT fields smoothed by\n"
"    differing numbers of passes.  Independent unit-variance components make\n"
"    the mixture's ACF the weighted sum of theirs, so the weights follow from\n"
"    an ordinary least-squares fit to the requested curve, solved once at\n"
"    startup.  The program then GENERATES a few fields, measures their ACF,\n"
"    and reports what it actually achieved -- read that line rather than\n"
"    trusting the fit.  It warns if the generated 'a' misses the request by\n"
"    more than 0.10.\n"
"\n"
"    ACCURACY: expect the generated a within roughly 0.15 of the request, and\n"
"    b and c looser -- both are weakly identified whenever their component\n"
"    carries little weight, so do not read much into them on their own.  The\n"
"    achieved value is printed on the 'ACF verify' line every run: READ IT\n"
"    rather than assuming the request was met, and treat the request as a\n"
"    target the program aims at, not a guarantee.  The generated shape tends\n"
"    to land on the heavy-tailed side of what was asked for (a lower than\n"
"    requested), which is the conservative direction for cluster inference:\n"
"    a heavier tail yields larger null clusters and so a stricter threshold.\n"
"    The\n"
"    generated curve tends to sit slightly below the requested one at large\n"
"    r, because an empirical autocorrelation is biased low at long lags while\n"
"    the target is an exact analytic curve; the true ACF of the noise is\n"
"    likely closer to the request than the report suggests.\n"
"\n"
"    COST: calibration is a fixed startup cost, dominated by measuring the\n"
"    basis autocorrelations, and takes roughly half a minute on a 10k-node\n"
"    surface.  After that the per-simulation cost is small, so this mode\n"
"    suits large -niter: 1000 simulations added under a second to a 27-second\n"
"    calibration in testing.\n"
"\n"
"  -acf_nbasis K\n"
"    Number of basis fields in the mixture (default 5, range 2-16).  Raise it\n"
"    if the achieved 'a' misses the request; more components can follow a\n"
"    more awkward curve, at proportionally more smoothing per simulation.\n"
"\n"
"  -acf_radius R\n"
"    Largest geodesic distance used when fitting and verifying the ACF.  The\n"
"    default reaches a couple of decay lengths of c, which is what lets the\n"
"    fit see the tail at all, capped to keep startup affordable.\n"
"\n"
"  -itersize B\n"
"\n"
"    Number of fields per processing block (default 10).  In -compat this is\n"
"    also the blurmaster length, matching the old script default; changing it\n"
"    can change detrending, selected Niter, and results.  In -fixed it is only\n"
"    a memory/parallel-work batching parameter and does not change results.\n"
"    Blocks are distributed across threads, so -niter/B (the block COUNT) is\n"
"    what bounds parallelism -- a larger B means fewer, bigger blocks and less\n"
"    parallelism, not more.  The default suits both roles; prefer to leave it.\n"
"\n"
"NOISE DOMAIN AND MASKING\n"
"------------------------\n"
"  -on_surface [yes|no]\n"
"\n"
"    Generate independent N(0,1) noise at surface nodes before smoothing.\n"
"    A bare -on_surface means yes.  This matches '-on_surface yes' in the old\n"
"    script and does not require -sv or a surface volume.\n"
"\n"
"  -surf_mask DSET\n"
"\n"
"    Restrict generated noise, FWHM estimation, smoothing, rescaling, and\n"
"    clustering to nonzero surface nodes.  Use the same analysis mask that\n"
"    will be used on real data.  This option can also restrict nodes after\n"
"    volume-to-surface mapping.\n"
"\n"
"  -vol_mask MASK\n"
"\n"
"    Select volume-domain simulation and use MASK both as the noise grid and\n"
"    nonzero voxel mask.  Volume noise is mapped between -surf_A and -surf_B\n"
"    using the coordinate system supplied by -sv SURFVOL.\n"
"\n"
"  -map_func FUNC       3dVol2Surf mapping function (default ave).\n"
"  -f_steps N           Samples between surfaces (default 10).\n"
"                       -nsteps is an alias.\n"
"\n"
"SIMULATION AND CLUSTER OPTIONS\n"
"------------------------------\n"
"  -niter N\n"
"\n"
"    Total number of independent simulated fields (default 1000).  Unlike the\n"
"    generated legacy script, this is not converted to a number of blocks and\n"
"    is not rounded upward.  For a final alpha=0.01 table, 10000 or more runs\n"
"    are advisable: 10000 simulations provide only about 100 observations in\n"
"    a 1%% tail.  More simulations reduce Monte Carlo variation.\n"
"\n"
"  -pthr p1 ... pn, -pval p1 ... pn\n"
"\n"
"    Uncorrected nodewise probability thresholds.  Values must be in (0,.5].\n"
"    Defaults are .05 .02 .01 .005 .002 .001 .0005 .0002 .0001.  Each value\n"
"    is converted internally to a standard-normal threshold according to the\n"
"    selected sidedness.\n"
"\n"
"  -athr a1 ... an\n"
"\n"
"    Corrected whole-surface alpha levels to report.  Defaults are\n"
"    .10 .05 .02 .01.  A table entry C(p,alpha) is the cluster area threshold\n"
"    obtained from the empirical distribution of maximum cluster area.\n"
"\n"
"  -LOTS\n"
"\n"
"    Use dense 3dClustSim-style pthr and athr grids (29 p thresholds and\n"
"    10 alpha levels).  Explicit -pthr or -athr options appearing later can\n"
"    replace the corresponding list.\n"
"\n"
"  -rmm R\n"
"\n"
"    Cluster connection rule (default -1).  A negative integer -N connects\n"
"    nodes within N mesh-edge steps; -1 means immediate neighbors.  A positive\n"
"    R uses SUMA's surface-offset radius in surface coordinate units.  Use the\n"
"    same rule when clustering the real statistical map.\n"
"\n"
"  -seed S              Nonnegative deterministic seed (default 123456789).\n"
"  -nthreads N          OpenMP thread limit; 0 means the runtime maximum.\n"
"                       Results are invariant to thread count.\n"
"  -quiet               Suppress routine information and progress messages.\n"
"                       Warnings and errors are still printed.\n"
"  -verbose, -verb      Show per-block smoothing messages and progress\n"
"                       pacifiers.  By default, routine setup/output messages\n"
"                       and one global progress update every 10 blocks are\n"
"                       printed.\n"
"\n"
"SIDEDNESS: WHAT pthr MEANS\n"
"-------------------------\n"
"By default the program writes 1-sided, 2-sided, and bi-sided tables.  In\n"
"-compat it additionally writes a legacy table.  If any selector below is\n"
"given, only explicitly selected modes are written; selectors may be repeated.\n"
"\n"
"  -1sided, -onesided\n"
"\n"
"    Positive clusters only.  Threshold z=Phi^-1(1-pthr), so pthr is the\n"
"    positive upper-tail probability.\n"
"\n"
"  -2sided, -twosided\n"
"\n"
"    Threshold |z| at Phi^-1(1-pthr/2), so total nodewise probability is pthr.\n"
"    Positive and negative active nodes belong to one binary excursion set and\n"
"    may join the same connected component.  This follows 3dClustSim's\n"
"    2-sided convention.\n"
"\n"
"  -bisided\n"
"\n"
"    Use the same pthr/2 threshold in each tail, but cluster positive and\n"
"    negative nodes separately.  The simulation records the larger maximum\n"
"    from the two signs.\n"
"\n"
"  -legacy_sided\n"
"\n"
"    Match slow_surf_clustsim.py/SurfClust thresholding: threshold |z| at\n"
"    Phi^-1(1-pthr).  Thus EACH tail has probability pthr and the total\n"
"    nodewise false-positive probability is approximately 2*pthr.  Opposite\n"
"    signs can join.  This is intentionally not the modern meaning of a\n"
"    2-sided pthr; use it only for historical compatibility.\n"
"\n"
"OUTPUT\n"
"------\n"
"  -prefix P\n"
"\n"
"    Write P.1sided.1D, P.2sided.1D, P.bisided.1D, and/or P.legacy.1D.\n"
"    The parent directory must already exist.  Each file records the command,\n"
"    surface/mask sizes, smoothing parameters, seed, threads, pthr rows, and\n"
"    athr columns.\n"
"\n"
"  -niml               Also write matching .niml table files.\n"
"  -niml_only          Write only NIML tables.\n"
"  -both               Write both 1D and NIML tables (same as -niml).\n"
"\n"
"  -maxarea_1D\n"
"    Also write the raw per-simulation maximum cluster areas, one file per\n"
"    p-value, named PREFIX.MODE.max.area.PTHR.  Each file holds -niter lines,\n"
"    one area per line, unsorted -- the same layout slow_surf_clustsim.py\n"
"    wrote as z.max.area.PTHR.  Use it to compare against the old pipeline:\n"
"      quick.alpha.vals.py -niter N PREFIX.legacy.max.area.0.001\n"
"    The built-in alpha table is NOT directly comparable to that tool, which\n"
"    rounds areas to whole units and builds an empirical survival curve; the\n"
"    table here uses an interpolated quantile instead.\n"
"    One format difference from slow_surf_clustsim.py: when a simulation has\n"
"    no suprathreshold cluster, that file wrote nothing, so it held FEWER\n"
"    than niter lines.  This writes an explicit 0, always niter lines.  Alpha\n"
"    values still agree because quick.alpha.vals.py normalizes by its -niter\n"
"    argument -- but pass -niter, or it will normalize by the line count and\n"
"    the two pipelines will disagree by the fraction of empty simulations.\n"
"\n"
"  -multithresh   (or -mthresh)\n"
"    Also write a jointly calibrated multi-threshold table, named\n"
"    PREFIX.MODE.mthresh.1D.\n"
"\n"
"    THE IDEA, AND WHERE IT COMES FROM\n"
"    This option is inspired by ETAC (Equitable Thresholding And Clustering),\n"
"    available in the volume via 3dttest++ -ETAC and 3dXClustSim.  ETAC's\n"
"    insight is that picking a single per-node p-threshold before you know\n"
"    what the signal looks like is arbitrary, and the choice changes the\n"
"    answer: a low threshold favours large diffuse clusters, a high one\n"
"    favours small focal ones.  ETAC's answer is to stop choosing.  It tests\n"
"    several thresholds at once and calibrates them together, 'equitably' --\n"
"    meaning each member of the family is tuned to contribute about equally\n"
"    to the overall false positive rate, so no single one dominates.\n"
"    That equitable multi-threshold principle is what this option borrows.\n"
"    A result counts as significant if a cluster survives at ANY of the\n"
"    p-thresholds, and all the cutoffs are chosen together so the probability\n"
"    of that happening under the null is alpha.  Concretely, one shared tail\n"
"    probability q sets every threshold's cutoff, which is what makes the\n"
"    contributions equitable, and q is solved for by bisection.  The\n"
"    arbitrary choice of threshold goes away; the cost is larger cutoffs,\n"
"    typically 15-25%% above the per-threshold table, which is the honest\n"
"    price of testing a whole family.\n"
"\n"
"    HOW TO READ THE TABLE\n"
"    Each COLUMN is one calibrated family, not a menu of alternatives.  To\n"
"    use the alpha column, test EVERY p-threshold listed in it and accept a\n"
"    cluster surviving at any of them; the family then carries the stated\n"
"    false positive rate.  Using a single row on its own is still valid, just\n"
"    conservative -- that cutoff is larger than the per-threshold table's.\n"
"    The achieved null FPR is written into the header so you can see how well\n"
"    the calibration converged.  It needs a few thousand simulations to mean\n"
"    much, and a warning is issued if it lands far from what was requested.\n"
"\n"
"    WHAT THIS DOES *NOT* INHERIT FROM ETAC\n"
"    Real ETAC is substantially more than multi-threshold calibration, and\n"
"    this option should not be mistaken for it.  ETAC additionally offers:\n"
"      * a null built by randomizing/permuting the residuals of your ACTUAL\n"
"        data, rather than from simulated noise.  This is the big one: it is\n"
"        where most of ETAC's advantage over a simulated null comes from,\n"
"        because it assumes nothing about the shape of the spatial\n"
"        autocorrelation.  This program calibrates over the noise it\n"
"        SIMULATES, and so inherits every assumption in that noise model --\n"
"        in particular that smoothing white noise to a target FWHM yields a\n"
"        realistic autocorrelation, which is exactly the assumption that\n"
"        could inflate cluster false positives in the volume literature.\n"
"      * a more general cluster figure of merit than area alone; ETAC can\n"
"        also use sums of powers of |z| within a cluster.  Here it is area.\n"
"      * simultaneous calibration across multiple blur levels.  Here there is\n"
"        one smoothing level per run.\n"
"      * spatially varying thresholds, so that ETAC produces a MAP of cutoffs\n"
"        adapted to local smoothness.  This produces one number per\n"
"        (p, alpha) pair, applied everywhere, and so still assumes the\n"
"        surface is statistically stationary.\n"
"    In short: this borrows ETAC's equitable multi-threshold idea, and with\n"
"    it the benefit of not having to pick a threshold.  It does not deliver\n"
"    ETAC's data-driven null, its generalized merit, its multi-blur\n"
"    calibration, or its spatial adaptivity.  If you can run a permutation\n"
"    test on your data, that remains the better tool.\n"
"\n"
"  -selfcheck\n"
"    Recompute every maximum cluster area with an independent breadth-first\n"
"    search and abort if it disagrees with the threshold-sweep result.  The\n"
"    sweep activates nodes in descending order and merges components with\n"
"    union-find to get every threshold from one pass; the BFS simply floods\n"
"    each thresholded field separately.  They must agree exactly.  This is a\n"
"    correctness check, not a sampling option -- it is roughly an order of\n"
"    magnitude slower, so use it on a small -niter, not a production run.\n"
"\n"
"Table entries are areas in the SQUARED coordinate units of the surface.\n"
"They are mm^2 only when the surface coordinates are expressed in mm.  Apply\n"
"a table only to the same surface geometry, mask, smoothing procedure, noise\n"
"domain, rmm, and sidedness used to generate it.  A cluster is significant at\n"
"the requested (pthr,alpha) when its area meets the corresponding threshold.\n"
"\n"
"The empirical quantiles retain continuous node areas and are monotonized so\n"
"that stricter pthr or alpha settings cannot produce a smaller threshold merely\n"
"from Monte Carlo jitter.\n"
"\n"
"REPRODUCIBILITY AND PERFORMANCE\n"
"-------------------------------\n"
"The random stream is indexed by global simulation number, so results do not\n"
"depend on -nthreads.  Fixed-mode results also do not depend on -itersize.\n"
"Compatibility-mode results can depend on -itersize because the block is the\n"
"adaptive smoothing master.\n"
"\n"
"OpenMP distributes whole blocks across threads, so parallelism is limited by\n"
"the BLOCK COUNT (-niter / -itersize), not by -itersize itself.  Raising\n"
"-itersize therefore REDUCES available parallelism, and in -compat it also\n"
"changes the results; leave it alone unless you mean to.  Memory scales as\n"
"-nthreads * -itersize * nodes, since each thread holds a whole block.\n"
"Volume-mode noise generation is serialized (the vol2surf routines beneath it\n"
"are not reentrant); everything after it runs in parallel.  Final table output\n"
"is serial.  With -verbose, pacifiers are capped at 50 marks per phase.\n"
"\n"
"EXAMPLES\n"
"--------\n"
"1. Historical surface-domain compatibility, one old-style table:\n"
"\n"
"   SurfClustSim -spec std.lh.spec -surf_A smoothwm -on_surface \\\n"
"      -compat -target_fwhm 10 -itersize 10 -niter 10000          \\\n"
"      -pthr .05 .01 .001 -legacy_sided -prefix lh.old -niml\n"
"\n"
"2. New analysis with adaptive FWHM and modern sidedness tables:\n"
"\n"
"   SurfClustSim -spec std.lh.spec -surf_A smoothwm -on_surface \\\n"
"      -compat -target_fwhm 10 -niter 10000                       \\\n"
"      -1sided -2sided -bisided -prefix lh.adaptive -niml\n"
"\n"
"3. Calibrated fixed smoothing:\n"
"\n"
"   SurfClustSim -spec std.lh.spec -surf_A smoothwm -on_surface \\\n"
"      -fixed -sigma S -Niter K -niter 10000 -itersize 32         \\\n"
"      -nthreads 16 -prefix lh.fixed -niml\n"
"\n"
"4. Volume noise mapped between white and pial surfaces:\n"
"\n"
"   SurfClustSim -spec std.lh.spec -surf_A smoothwm -surf_B pial \\\n"
"      -sv SurfVol.nii -vol_mask epi.mask.nii                     \\\n"
"      -compat -target_fwhm 10 -niter 10000 -prefix lh.volume\n"
"\n"
"See also: SurfSmooth, SurfClust, 3dClustSim, slow_surf_clustsim.py\n"
"\n"
"Author:  %s\n"
"Date:    %s\n"
"Version: %s\n", SSCS_AUTHOR, SSCS_DATE, SSCS_VERSION);
   PRINT_AFNI_OMP_USAGE(
      SSCS_PROGRAM,
      "* SurfClustSim also accepts -nthreads N to set a smaller thread limit\n"
      "   for one invocation.  Parallel simulation workers are also limited by\n"
      "   -itersize (default 10).\n");
}

static int sscs_double_desc(const void *aa, const void *bb)
{
   double a = *(const double *)aa, b = *(const double *)bb;
   return a < b ? 1 : a > b ? -1 : 0;
}

static int sscs_float_asc(const void *aa, const void *bb)
{
   float a = *(const float *)aa, b = *(const float *)bb;
   return a < b ? -1 : a > b ? 1 : 0;
}

static void sscs_set_list(double **dest, int *ndest,
                          const double *source, int nsource)
{
   double *copy = (double *)malloc((size_t)nsource * sizeof(double));
   if (!copy) ERROR_exit("Out of memory copying threshold list");
   memcpy(copy, source, (size_t)nsource * sizeof(double));
   /* The alpha-table monotonization and the sweep's nondecreasing-z
      precondition both assume descending thresholds.  User lists are sorted in
      sscs_parse_list(); sort the built-in defaults here too so the invariant
      cannot be broken by an out-of-order edit to the constant arrays. */
   qsort(copy, (size_t)nsource, sizeof(double), sscs_double_desc);
   free(*dest);
   *dest = copy;
   *ndest = nsource;
}

static void sscs_parse_list(int argc, char **argv, int *index,
                            double **values, int *nvalues, const char *option)
{
   int start = *index + 1, stop = start, item;
   double value;
   while (stop < argc && argv[stop][0] != '-') ++stop;
   if (stop == start) ERROR_exit("No values after %s", option);
   free(*values);
   *nvalues = stop - start;
   *values = (double *)malloc((size_t)*nvalues * sizeof(double));
   if (!*values) ERROR_exit("Out of memory parsing %s", option);
   for (item = 0; item < *nvalues; ++item) {
      char *end = NULL;
      errno = 0;
      value = strtod(argv[start + item], &end);
      if (errno || end == argv[start + item] || *end || value <= 0.0 || value > 0.5)
         ERROR_exit("Illegal value '%s' after %s", argv[start + item], option);
      (*values)[item] = value;
   }
   qsort(*values, (size_t)*nvalues, sizeof(double), sscs_double_desc);
   *index = stop - 1;
}

static double sscs_parse_double_arg(const char *text, const char *option)
{
   char *end = NULL;
   double value;
   errno = 0; value = strtod(text, &end);
   if (errno || end == text || *end || !isfinite(value))
      ERROR_exit("Illegal value '%s' after %s", text, option);
   return value;
}

static int sscs_parse_int_arg(const char *text, const char *option)
{
   char *end = NULL;
   long value;
   errno = 0; value = strtol(text, &end, 10);
   if (errno || end == text || *end || value < INT_MIN || value > INT_MAX)
      ERROR_exit("Illegal value '%s' after %s", text, option);
   return (int)value;
}

static unsigned long long sscs_parse_seed_arg(const char *text)
{
   char *end = NULL;
   unsigned long long value;
   errno = 0;
   if (text[0] == '-') ERROR_exit("-seed cannot be negative");
   value = strtoull(text, &end, 10);
   if (errno || end == text || *end) ERROR_exit("Illegal -seed value '%s'", text);
   return value;
}

/* Mixed ACF model, same as SUMA_SurfACF_model but available here without
   pulling the estimator header into the driver. */
static double sscs_acf_model(double a, double b, double c, double r)
{
   return a*exp(-0.5*r*r/(b*b)) + (1.0-a)*exp(-r/c);
}

/* Read ACF parameters from a SurfFWHM -acf report.

   Two shapes are accepted, because both are things a user will reasonably
   point at:
     1. the curve file SurfFWHM writes, whose header carries
          # a=0.5334  b=8.397  c=36.33  effective FWHM=25.35
     2. a file holding the bare "a b c FWHM" line SurfFWHM prints to stdout.

   The header form is tried first and wins, because the curve file's first
   DATA row is "0 1 1 1" (radius 0, ACF 1, model 1, gaussian 1), which would
   otherwise be misread as a=0, b=1, c=1 -- a valid-looking but meaningless
   request.  A bare numeric line is only accepted if it does not look like
   such a row. */
static int sscs_read_acf_file(const char *fname, double *a, double *b,
                              double *c)
{
   FILE *fp = fopen(fname, "r");
   char line[1024];
   int got = 0;
   if (!fp) return 0;

   while (fgets(line, sizeof(line), fp)) {
      char *pa = strstr(line, "a=");
      if (line[0] == '#' && pa) {
         char *pb = strstr(line, "b="), *pc = strstr(line, "c=");
         double va, vb, vc;
         if (pb && pc &&
             sscanf(pa+2, "%lf", &va) == 1 &&
             sscanf(pb+2, "%lf", &vb) == 1 &&
             sscanf(pc+2, "%lf", &vc) == 1 &&
             va >= 0.0 && va <= 1.0 && vb > 0.0 && vc > 0.0) {
            *a = va; *b = vb; *c = vc; got = 1;
            break;
         }
      }
   }

   if (!got) {
      rewind(fp);
      while (fgets(line, sizeof(line), fp)) {
         double va, vb, vc;
         char *p = line;
         while (*p == ' ' || *p == '\t') ++p;
         if (*p == '#' || *p == '\n' || *p == '\0') continue;
         if (sscanf(p, "%lf %lf %lf", &va, &vb, &vc) == 3) {
            /* guard against the curve file's leading "0 1 1" row */
            if (va == 0.0 && vb == 1.0) break;
            if (va >= 0.0 && va <= 1.0 && vb > 0.0 && vc > 0.0) {
               *a = va; *b = vb; *c = vc; got = 1;
            }
            break;
         }
      }
   }
   fclose(fp);
   return got;
}

static void sscs_init_options(SSCS_OPTIONS *opt)

{
   memset(opt, 0, sizeof(*opt));
   opt->on_surface = 1;
   opt->compat = 1;
   opt->niter = 1000;
   opt->itersize = 10;
   opt->smooth_niter = -1;
   opt->max_smooth_niter = 3000;
   opt->acf_nbasis = 5;
   opt->acf_radius = 0.0f;
   opt->nthreads = 0;
   opt->do_1D = 1;
   opt->verb = 1;
   opt->rmm = -1.0f;
   opt->target_fwhm = 4.0;
   opt->sigma = -1.0;
   opt->seed = UINT64_C(123456789);
   opt->prefix = strdup("SurfClustSim");
   opt->map_func = strdup("ave");
   opt->f_steps = 10;
   sscs_set_list(&opt->pthr, &opt->npthr, sscs_default_pthr,
                 (int)(sizeof(sscs_default_pthr) / sizeof(sscs_default_pthr[0])));
   sscs_set_list(&opt->athr, &opt->nathr, sscs_default_athr,
                 (int)(sizeof(sscs_default_athr) / sizeof(sscs_default_athr[0])));
}

static void sscs_select_side(SSCS_OPTIONS *opt, int mode)
{
   if (!opt->selected_sides) {
      memset(opt->side, 0, sizeof(opt->side));
      opt->selected_sides = 1;
   }
   opt->side[mode] = 1;
}

static void sscs_parse_options(int argc, char **argv, SSCS_OPTIONS *opt,
                               SUMA_GENERIC_ARGV_PARSE *ps)
{
   int arg;
   for (arg = 1; arg < argc; ++arg) {
      if (ps && ps->arg_checked[arg]) continue;
      if (!strcmp(argv[arg], "-help") || !strcmp(argv[arg], "-h")) {
         sscs_help(); exit(0);
      } else if (!strcmp(argv[arg], "-ver") || !strcmp(argv[arg], "-version")) {
         printf("%s %s\n", SSCS_PROGRAM, SSCS_VERSION); exit(0);
      } else if (!strcmp(argv[arg], "-on_surface")) {
         opt->on_surface = 1;
         if (arg + 1 < argc && (!strcmp(argv[arg + 1], "yes") ||
                                !strcmp(argv[arg + 1], "no")))
            opt->on_surface = !strcmp(argv[++arg], "yes");
      } else if (!strcmp(argv[arg], "-vol_mask")) {
         if (++arg >= argc) ERROR_exit("Need a dataset after -vol_mask");
         opt->vol_mask_name = argv[arg]; opt->on_surface = 0;
      } else if (!strcmp(argv[arg], "-surf_mask")) {
         if (++arg >= argc) ERROR_exit("Need a dataset after -surf_mask");
         opt->surf_mask_name = argv[arg];
      } else if (!strcmp(argv[arg], "-compat")) {
         opt->compat_explicit = 1;
      } else if (!strcmp(argv[arg], "-fixed")) {
         opt->fixed_explicit = 1;
      } else if (!strcmp(argv[arg], "-target_fwhm") || !strcmp(argv[arg], "-blur")) {
         if (++arg >= argc) ERROR_exit("Need a value after %s", argv[arg - 1]);
         opt->target_fwhm = sscs_parse_double_arg(argv[arg], argv[arg - 1]);
      } else if (!strcmp(argv[arg], "-sigma")) {
         if (++arg >= argc) ERROR_exit("Need a value after -sigma");
         opt->sigma = sscs_parse_double_arg(argv[arg], "-sigma");
      } else if (!strcmp(argv[arg], "-Niter")) {
         if (++arg >= argc) ERROR_exit("Need a value after -Niter");
         opt->smooth_niter = sscs_parse_int_arg(argv[arg], "-Niter");
         opt->smooth_niter_given = 1;
      } else if (!strcmp(argv[arg], "-max_Niter") ||
                 !strcmp(argv[arg], "-max_smooth_niter")) {
         if (++arg >= argc) ERROR_exit("Need a value after %s", argv[arg - 1]);
         opt->max_smooth_niter = sscs_parse_int_arg(argv[arg], argv[arg - 1]);
      } else if (!strcmp(argv[arg], "-niter")) {
         if (++arg >= argc) ERROR_exit("Need a value after -niter");
         opt->niter = sscs_parse_int_arg(argv[arg], "-niter");
      } else if (!strcmp(argv[arg], "-itersize")) {
         if (++arg >= argc) ERROR_exit("Need a value after -itersize");
         opt->itersize = sscs_parse_int_arg(argv[arg], "-itersize");
      } else if (!strcmp(argv[arg], "-rmm")) {
         if (++arg >= argc) ERROR_exit("Need a value after -rmm");
         opt->rmm = (float)sscs_parse_double_arg(argv[arg], "-rmm");
      } else if (!strcmp(argv[arg], "-seed")) {
         if (++arg >= argc) ERROR_exit("Need a value after -seed");
         opt->seed = sscs_parse_seed_arg(argv[arg]);
      } else if (!strcmp(argv[arg], "-nthreads")) {
         if (++arg >= argc) ERROR_exit("Need a value after -nthreads");
         opt->nthreads = sscs_parse_int_arg(argv[arg], "-nthreads");
      } else if (!strcmp(argv[arg], "-prefix")) {
         if (++arg >= argc) ERROR_exit("Need a value after -prefix");
         free(opt->prefix); opt->prefix = strdup(argv[arg]);
      } else if (!strcmp(argv[arg], "-pthr") || !strcmp(argv[arg], "-pval")) {
         sscs_parse_list(argc, argv, &arg, &opt->pthr, &opt->npthr, argv[arg]);
      } else if (!strcmp(argv[arg], "-athr")) {
         sscs_parse_list(argc, argv, &arg, &opt->athr, &opt->nathr, argv[arg]);
      } else if (!strcmp(argv[arg], "-LOTS")) {
         sscs_set_list(&opt->pthr, &opt->npthr, sscs_lots_pthr,
                       (int)(sizeof(sscs_lots_pthr) / sizeof(sscs_lots_pthr[0])));
         sscs_set_list(&opt->athr, &opt->nathr, sscs_lots_athr,
                       (int)(sizeof(sscs_lots_athr) / sizeof(sscs_lots_athr[0])));
      } else if (!strcmp(argv[arg], "-1sided") || !strcmp(argv[arg], "-onesided")) {
         sscs_select_side(opt, SSCS_ONE_SIDED);
      } else if (!strcmp(argv[arg], "-2sided") || !strcmp(argv[arg], "-twosided")) {
         sscs_select_side(opt, SSCS_TWO_SIDED);
      } else if (!strcmp(argv[arg], "-bisided")) {
         sscs_select_side(opt, SSCS_BI_SIDED);
      } else if (!strcmp(argv[arg], "-legacy_sided")) {
         sscs_select_side(opt, SSCS_LEGACY_SIDED);
      } else if (!strcmp(argv[arg], "-niml")) {
         opt->do_niml = 1;
      } else if (!strcmp(argv[arg], "-niml_only")) {
         opt->do_niml = 1; opt->do_1D = 0;
      } else if (!strcmp(argv[arg], "-both")) {
         opt->do_niml = opt->do_1D = 1;
      } else if (!strcmp(argv[arg], "-acf") || !strcmp(argv[arg], "-ACF")) {
         if (arg + 3 >= argc)
            ERROR_exit("Need three values (a b c) after -acf");
         opt->acf_a = sscs_parse_double_arg(argv[++arg], "-acf a");
         opt->acf_b = sscs_parse_double_arg(argv[++arg], "-acf b");
         opt->acf_c = sscs_parse_double_arg(argv[++arg], "-acf c");
         if (opt->acf_a < 0.0 || opt->acf_a > 1.0)
            ERROR_exit("-acf a must lie in [0,1]; got %g", opt->acf_a);
         if (opt->acf_b <= 0.0 || opt->acf_c <= 0.0)
            ERROR_exit("-acf b and c must be positive");
         opt->acf_mode = 1;
         opt->compat = 0;
      } else if (!strcmp(argv[arg], "-acf_file")) {
         if (++arg >= argc) ERROR_exit("Need a filename after -acf_file");
         if (!sscs_read_acf_file(argv[arg], &opt->acf_a, &opt->acf_b,
                                 &opt->acf_c))
            ERROR_exit("Could not read ACF parameters from %s", argv[arg]);
         opt->acf_mode = 1;
         opt->compat = 0;
      } else if (!strcmp(argv[arg], "-acf_nbasis")) {
         if (++arg >= argc) ERROR_exit("Need a value after -acf_nbasis");
         opt->acf_nbasis = sscs_parse_int_arg(argv[arg], "-acf_nbasis");
         if (opt->acf_nbasis < 2 || opt->acf_nbasis > 16)
            ERROR_exit("-acf_nbasis must be between 2 and 16");
      } else if (!strcmp(argv[arg], "-acf_radius")) {
         if (++arg >= argc) ERROR_exit("Need a value after -acf_radius");
         opt->acf_radius = (float)sscs_parse_double_arg(argv[arg],
                                                        "-acf_radius");
      } else if (!strcmp(argv[arg], "-maxarea_1D")) {
         opt->do_maxarea = 1;
      } else if (!strcmp(argv[arg], "-multithresh") ||
                 !strcmp(argv[arg], "-mthresh")) {
         opt->do_mthresh = 1;
      } else if (!strcmp(argv[arg], "-selfcheck")) {
         opt->selfcheck = 1;
      } else if (!strcmp(argv[arg], "-map_func")) {
         if (++arg >= argc) ERROR_exit("Need a value after -map_func");
         free(opt->map_func); opt->map_func = strdup(argv[arg]);
      } else if (!strcmp(argv[arg], "-f_steps") || !strcmp(argv[arg], "-nsteps")) {
         if (++arg >= argc) ERROR_exit("Need a value after -f_steps");
         opt->f_steps = sscs_parse_int_arg(argv[arg], "-f_steps");
      } else if (!strcmp(argv[arg], "-verbose") ||
                 !strcmp(argv[arg], "-verb")) {
         opt->verb = 2;
      } else if (!strcmp(argv[arg], "-quiet")) {
         opt->verb = 0;
      } else {
         ERROR_exit("Unknown option '%s'", argv[arg]);
      }
   }

   if (opt->compat_explicit && opt->fixed_explicit)
      ERROR_exit("-compat and -fixed are mutually exclusive");
   if (opt->fixed_explicit) opt->compat = 0;
   else if (opt->compat_explicit) opt->compat = 1;
   else if (opt->acf_mode) opt->compat = 0;   /* -acf is its own mode */
   else opt->compat = !opt->smooth_niter_given;
   if (opt->compat && opt->smooth_niter_given)
      ERROR_exit("-Niter applies to -fixed; omit it with -compat");

   if (!opt->selected_sides) {
      opt->side[SSCS_ONE_SIDED] = 1;
      opt->side[SSCS_TWO_SIDED] = 1;
      opt->side[SSCS_BI_SIDED] = 1;
      if (opt->compat) opt->side[SSCS_LEGACY_SIDED] = 1;
   }
   if (opt->niter <= 0 || opt->itersize <= 0 || opt->max_smooth_niter <= 0)
      ERROR_exit("-niter, -itersize, and -max_Niter must be positive");
   if (opt->rmm == 0.0f ||
       (opt->rmm < 0.0f && opt->rmm != (float)((int)opt->rmm)))
      ERROR_exit("-rmm must be positive or a negative integer edge count");
   if (opt->f_steps <= 0) ERROR_exit("-f_steps must be positive");
   if (opt->nthreads < 0) ERROR_exit("-nthreads cannot be negative");
   if (opt->acf_mode) {
      /* A third smoothing mode: the basis mixture supplies the smoothing, so
         neither the adaptive search nor a single fixed pass count applies.
         -sigma still sets the kernel the basis ladder is built from, and
         -Niter still positions the ladder, but both have defaults. */
      if (opt->compat_explicit || opt->fixed_explicit)
         ERROR_exit("-acf is its own smoothing mode; do not combine it with "
                    "-compat or -fixed");
   } else if (opt->compat) {
      if (opt->target_fwhm <= 0.0) ERROR_exit("-compat requires -target_fwhm > 0");
   } else if (opt->sigma <= 0.0 || opt->smooth_niter < 0) {
      ERROR_exit("-fixed requires both -sigma S and -Niter K");
   }
   if (!opt->on_surface && !opt->vol_mask_name)
      ERROR_exit("Volume mode requires -vol_mask");
   if (!THD_filename_ok(opt->prefix)) ERROR_exit("Illegal output prefix");
}

static byte *sscs_load_surface_mask(const char *name, SUMA_SurfaceObject *SO)
{
   SUMA_DSET *dset;
   SUMA_DSET_FORMAT form = SUMA_NO_DSET_FORMAT;
   byte *defined = NULL, *mask = NULL;
   float *values = NULL;
   int *cols = NULL, ncols = 0, nmask = 0, node;
   if (!name) return NULL;
   dset = SUMA_LoadDset_s((char *)name, &form, 0);
   if (!dset || !SUMA_OKassign(dset, SO)) goto fail;
   cols = SUMA_FindNumericDataDsetCols(dset, &ncols);
   if (!cols || ncols < 1) goto fail;
   values = SUMA_DsetCol2FloatFullSortedColumn(
      dset, cols[0], &defined, 0.0, SO->N_Node, &nmask, YUP);
   if (!values) goto fail;
   mask = (byte *)calloc((size_t)SO->N_Node, sizeof(byte));
   if (!mask) goto fail;
   for (node = 0; node < SO->N_Node; ++node)
      mask[node] = (!defined || defined[node]) && values[node] != 0.0f;
   free(cols); SUMA_free(values); SUMA_free(defined); SUMA_FreeDset(dset);
   return mask;
fail:
   free(mask); free(cols); SUMA_free(values); SUMA_free(defined);
   if (dset) SUMA_FreeDset(dset);
   return NULL;
}

static int sscs_copy_surface(SUMA_SurfaceObject *SO, SUMA_surface *surface)
{
   int node;
   float *xyz;
   if (!SO || !surface || !SO->NodeList) return 0;
   memset(surface, 0, sizeof(*surface));
   surface->type = SUMA_SURFACE_TYPE;
   surface->num_ixyz = surface->nall_ixyz = SO->N_Node;
   surface->seq = surface->sorted = 1;
   surface->seqbase = 0;
   surface->ixyz = (SUMA_ixyz *)malloc((size_t)SO->N_Node * sizeof(SUMA_ixyz));
   if (!surface->ixyz) return 0;
   xyz = SO->NodeList;
   for (node = 0; node < SO->N_Node; ++node) {
      surface->ixyz[node].id = node;
      surface->ixyz[node].x = *xyz++;
      surface->ixyz[node].y = *xyz++;
      surface->ixyz[node].z = *xyz++;
   }
   surface->xbot = SO->MinDims[0]; surface->xtop = SO->MaxDims[0];
   surface->ybot = SO->MinDims[1]; surface->ytop = SO->MaxDims[1];
   surface->zbot = SO->MinDims[2]; surface->ztop = SO->MaxDims[2];
   surface->xcen = SO->Center[0]; surface->ycen = SO->Center[1];
   surface->zcen = SO->Center[2];
   if (SO->idcode_str) MCW_strncpy(surface->idcode, SO->idcode_str, 32);
   else UNIQ_idcode_fill(surface->idcode);
   MCW_strncpy(surface->label, SO->Label ? SO->Label : "surface", 64);
   return 1;
}

static void sscs_free_surface(SUMA_surface *surface)
{
   if (!surface) return;
   free(surface->ixyz); surface->ixyz = NULL;
   free(surface->norm); surface->norm = NULL;
}

static float **sscs_surface_fields(const SSCS_OPTIONS *opt, int first, int count,
                                   int nnode, const byte *mask)
{
   float **field = (float **)calloc((size_t)count, sizeof(float *));
   int col;
   if (!field) return NULL;
   for (col = 0; col < count; ++col) {
      field[col] = (float *)malloc((size_t)nnode * sizeof(float));
      if (!field[col]) return field;
      SUMA_SurfClustSim_FillNoise(field[col], nnode, mask, opt->seed, first + col);
   }
   return field;
}

/* Generate a block of ACF-matched fields.  Unlike the plain surface path,
   each field arrives already smoothed -- the mixture's components carry their
   own smoothing -- so the caller must not apply another pass. */
static float **sscs_acf_fields(const SSCS_OPTIONS *opt, int first, int count,
                               int nnode, SUMA_SurfaceObject *SO, double **wgt,
                               const SUMA_SURFCLUSTSIM_ACF *acf,
                               const byte *mask,
                               SUMA_SURFCLUSTSIM_WORK *work)
{
   float **field = (float **)calloc((size_t)count, sizeof(float *));
   int col;
   if (!field) return NULL;
   for (col = 0; col < count; ++col) {
      field[col] = (float *)malloc((size_t)nnode * sizeof(float));
      if (!field[col]) return field;
      if (!SUMA_SurfClustSim_ACF_Fill(SO, wgt, acf, field[col], mask,
                                      opt->seed, first + col, work)) {
         free(field[col]); field[col] = NULL;
         return field;
      }
   }
   return field;
}

static float **sscs_volume_fields(const SSCS_OPTIONS *opt, int first, int count,
                                  int nnode, THD_3dim_dataset *mask_dset,
                                  const byte *volmask, SUMA_surface *surf_a,
                                  SUMA_surface *surf_b)
{
   THD_3dim_dataset *noise_dset = NULL;
   v2s_opts_t vopt;
   v2s_results *mapped = NULL;
   float **field = NULL;
   int nvox, col, row, node;

   noise_dset = EDIT_empty_copy(mask_dset);
   if (!noise_dset) goto fail;
   EDIT_dset_items(noise_dset, ADN_nvals, count, ADN_prefix, "SurfClustSimNoise",
                   ADN_none);
   nvox = DSET_NVOX(mask_dset);
   for (col = 0; col < count; ++col) {
      float *brick = (float *)malloc((size_t)nvox * sizeof(float));
      if (!brick) goto fail;
      SUMA_SurfClustSim_FillNoise(brick, nvox, volmask, opt->seed, first + col);
      EDIT_substitute_brick(noise_dset, col, MRI_float, brick);
      EDIT_BRICK_FACTOR(noise_dset, col, 0.0f);
   }

   v2s_fill_sopt_default(&vopt, 2);
   vopt.map = v2s_map_type(opt->map_func);
   if (vopt.map == E_SMAP_INVALID) ERROR_exit("Unknown -map_func '%s'", opt->map_func);
   vopt.gp_index = -1;
   vopt.debug = 0;
   vopt.dnode = -1;
   vopt.f_index = V2S_INDEX_NODE;
   vopt.f_steps = opt->f_steps;
   vopt.first_node = 0;
   vopt.last_node = nnode - 1;
   vopt.skip_cols = 0;
   vopt.oob.show = 1;
   vopt.oob.index = -1;
   vopt.oob.value = 0.0f;
   mapped = opt_vol2surf(noise_dset, &vopt, surf_a, surf_b, NULL);
   if (!mapped || mapped->max_vals != count) goto fail;

   field = (float **)calloc((size_t)count, sizeof(float *));
   if (!field) goto fail;
   for (col = 0; col < count; ++col) {
      field[col] = (float *)calloc((size_t)nnode, sizeof(float));
      if (!field[col]) goto fail;
   }
   for (row = 0; row < mapped->nused; ++row) {
      node = mapped->nodes[row];
      if (node < 0 || node >= nnode) continue;
      for (col = 0; col < count; ++col) field[col][node] = mapped->vals[col][row];
   }
   free_v2s_results(mapped);
   DSET_delete(noise_dset);
   return field;

fail:
   if (mapped) free_v2s_results(mapped);
   if (noise_dset) DSET_delete(noise_dset);
   if (field) {
      for (col = 0; col < count; ++col) free(field[col]);
      free(field);
   }
   return NULL;
}

static void sscs_free_fields(float **field, int count)
{
   int col;
   if (!field) return;
   for (col = 0; col < count; ++col) free(field[col]);
   free(field);
}

static double sscs_zthreshold(double upper_tail_probability)
{
   if (upper_tail_probability < 1.e-15) upper_tail_probability = 1.e-15;
   if (upper_tail_probability > 1.0 - 1.e-15)
      upper_tail_probability = 1.0 - 1.e-15;
   return qginv(upper_tail_probability);
}

/* (1-alpha) quantile of an already-ascending array, linearly interpolated.
   Factored out so the per-threshold and multi-threshold tables cannot drift
   apart in how they define a quantile. */
static double sscs_quantile_sorted(const float *sorted, int niter, double alpha)
{
   double rank, fraction;
   int lower;
   if (niter < 1) return 0.0;
   rank = (1.0 - alpha) * (double)(niter - 1);
   if (rank <= 0.0) return sorted[0];
   lower = (int)floor(rank);
   if (lower >= niter - 1) return sorted[niter - 1];
   fraction = rank - (double)lower;
   return sorted[lower] + fraction * (sorted[lower + 1] - sorted[lower]);
}

static double sscs_area_threshold(const float *maxarea, int niter, double alpha)
{
   float *sorted;
   double value;
   sorted = (float *)malloc((size_t)niter * sizeof(float));
   if (!sorted) ERROR_exit("Out of memory computing alpha table");
   memcpy(sorted, maxarea, (size_t)niter * sizeof(float));
   qsort(sorted, (size_t)niter, sizeof(float), sscs_float_asc);
   value = sscs_quantile_sorted(sorted, niter, alpha);
   free(sorted);
   return value;
}

static double *sscs_make_table(const SSCS_OPTIONS *opt, const float *maxarea)
{
   double *table;
   int pindex, aindex;
   table = (double *)malloc((size_t)opt->npthr * opt->nathr * sizeof(double));
   if (!table) return NULL;
   for (pindex = 0; pindex < opt->npthr; ++pindex)
      for (aindex = 0; aindex < opt->nathr; ++aindex)
         table[(size_t)pindex * opt->nathr + aindex] = sscs_area_threshold(
            maxarea + (size_t)pindex * opt->niter, opt->niter,
            opt->athr[aindex]);

   /* Monte Carlo jitter should not make thresholds reverse direction. */
   for (aindex = 0; aindex < opt->nathr; ++aindex)
      for (pindex = opt->npthr - 2; pindex >= 0; --pindex)
         if (table[(size_t)pindex * opt->nathr + aindex] <
             table[(size_t)(pindex + 1) * opt->nathr + aindex])
            table[(size_t)pindex * opt->nathr + aindex] =
               table[(size_t)(pindex + 1) * opt->nathr + aindex];
   for (pindex = 0; pindex < opt->npthr; ++pindex)
      for (aindex = 1; aindex < opt->nathr; ++aindex)
         if (table[(size_t)pindex * opt->nathr + aindex] <
             table[(size_t)pindex * opt->nathr + aindex - 1])
            table[(size_t)pindex * opt->nathr + aindex] =
               table[(size_t)pindex * opt->nathr + aindex - 1];
   return table;
}

/* ------------------------------------------------------------------------
   Multi-threshold calibration, inspired by ETAC.

   ETAC (Equitable Thresholding And Clustering, in 3dttest++ -ETAC and
   3dXClustSim) observes that choosing one per-node p-threshold ahead of time
   is arbitrary and changes the answer -- low thresholds favour large diffuse
   clusters, high ones favour small focal clusters -- and responds by not
   choosing.  It tests a family of thresholds at once and calibrates them
   "equitably", so each contributes about equally to the overall false
   positive rate.  That principle is what is borrowed here.

   The default table asks, for each p-threshold independently: how big must a
   cluster be so only alpha of null simulations produce one that big?  This
   instead calls a result significant if a cluster survives at ANY threshold,
   and picks all the cutoffs together so the probability of that under the
   null is alpha.

   Parameterization: index the family by a single per-threshold tail
   probability q, so cutoff_p = (1-q) quantile of that threshold's null max
   areas.  Sharing one q across thresholds is what makes the family equitable
   in ETAC's sense.  Raising q lowers every cutoff together and raises the
   union's false positive rate, monotonically, so bisection on q converges.
   The bracket is [alpha/npthr, alpha]: the low end is Bonferroni, guaranteed
   conservative; the high end is q = alpha everywhere, guaranteed liberal once
   there is more than one threshold.

   What is borrowed is the multi-threshold idea alone.  ETAC also provides a
   null from permuted residuals of the real data (the main source of its
   advantage, since it assumes nothing about the spatial autocorrelation), a
   figure of merit more general than cluster area, simultaneous calibration
   over several blur levels, and spatially varying thresholds.  This has none
   of those: it calibrates over the parametric null this program simulates,
   scores clusters by area, uses one blur level, and emits a single number per
   (p, alpha) rather than a map.  Hence -multithresh, not -ETAC.  See
   SurfClusterize_PLAN.md sections 18 and 21 for the full accounting.
   ------------------------------------------------------------------------ */

/* Calibrate one global alpha; writes npthr cutoffs and returns achieved FPR. */
static double sscs_calibrate_multithresh(const float *maxarea, int npthr,
                                         int niter, double alpha,
                                         float **sorted, double *cutoff)
{
   double qlo = alpha / (double)npthr, qhi = alpha, qmid = alpha, fpr = 0.0;
   int step, pindex, iteration;

   for (step = 0; step < 60; ++step) {
      int hits = 0;
      qmid = 0.5 * (qlo + qhi);
      for (pindex = 0; pindex < npthr; ++pindex)
         cutoff[pindex] = sscs_quantile_sorted(sorted[pindex], niter, qmid);
      for (iteration = 0; iteration < niter; ++iteration) {
         for (pindex = 0; pindex < npthr; ++pindex) {
            if ((double)maxarea[(size_t)pindex * niter + iteration]
                > cutoff[pindex]) { ++hits; break; }
         }
      }
      fpr = (double)hits / (double)niter;
      if (fpr > alpha) qhi = qmid; else qlo = qmid;
   }
   /* Land on the conservative side: recompute at the low end of the final
      bracket so the reported family does not exceed the requested alpha. */
   for (pindex = 0; pindex < npthr; ++pindex)
      cutoff[pindex] = sscs_quantile_sorted(sorted[pindex], niter, qlo);
   {
      int hits = 0;
      for (iteration = 0; iteration < niter; ++iteration)
         for (pindex = 0; pindex < npthr; ++pindex)
            if ((double)maxarea[(size_t)pindex * niter + iteration]
                > cutoff[pindex]) { ++hits; break; }
      fpr = (double)hits / (double)niter;
   }
   return fpr;
}

static int sscs_output_mthresh(const SSCS_OPTIONS *opt, int mode,
                               const float *maxarea, const char *commandline,
                               const char *surface_label, double sigma,
                               int niter_min, int niter_max)
{
   char filename[THD_MAX_NAME];
   FILE *out;
   float **sorted = NULL;
   double *cutoff = NULL, *achieved = NULL, *table = NULL;
   int pindex, aindex, ok = 0;

   snprintf(filename, sizeof(filename), "%s.%s.mthresh.1D", opt->prefix,
            sscs_mode_label(mode));
   if (THD_is_file(filename) && !THD_ok_overwrite()) {
      ERROR_message("Output file exists: %s", filename); return 0;
   }

   sorted = (float **)calloc((size_t)opt->npthr, sizeof(float *));
   cutoff = (double *)malloc((size_t)opt->npthr * sizeof(double));
   achieved = (double *)malloc((size_t)opt->nathr * sizeof(double));
   table = (double *)malloc((size_t)opt->npthr * opt->nathr * sizeof(double));
   if (!sorted || !cutoff || !achieved || !table) {
      ERROR_message("Out of memory building multi-threshold table"); goto done;
   }
   for (pindex = 0; pindex < opt->npthr; ++pindex) {
      sorted[pindex] = (float *)malloc((size_t)opt->niter * sizeof(float));
      if (!sorted[pindex]) {
         ERROR_message("Out of memory sorting null areas"); goto done;
      }
      memcpy(sorted[pindex], maxarea + (size_t)pindex * opt->niter,
             (size_t)opt->niter * sizeof(float));
      qsort(sorted[pindex], (size_t)opt->niter, sizeof(float), sscs_float_asc);
   }

   for (aindex = 0; aindex < opt->nathr; ++aindex) {
      achieved[aindex] = sscs_calibrate_multithresh(
         maxarea, opt->npthr, opt->niter, opt->athr[aindex], sorted, cutoff);
      for (pindex = 0; pindex < opt->npthr; ++pindex)
         table[(size_t)pindex * opt->nathr + aindex] = cutoff[pindex];
   }

   out = fopen(filename, "w");
   if (!out) { ERROR_message("Cannot open %s", filename); goto done; }
   fprintf(out,
      "# %s\n# SurfClustSim %s\n# Surface: %s\n# %s\n"
      "# Nodes: %d total, %d in mask; total area: %.9g; noise domain: %s\n"
      "# Smoothing: %s; sigma=%.8g; Niter range=%d..%d; simulations=%d; threads=%d\n"
      "# Connection radius rmm=%g; seed=%llu\n"
      "#\n"
      "# MULTI-THRESHOLD cluster area cutoffs, inspired by ETAC (Equitable\n"
      "# Thresholding And Clustering; see 3dttest++ -ETAC in the volume).\n"
      "# Each COLUMN is one equitably calibrated family, not a set of\n"
      "# alternatives.  To use the alpha column, test EVERY p-threshold in it\n"
      "# and call the result significant if a cluster survives at ANY of them;\n"
      "# the family as a whole then has the stated false positive rate.\n"
      "# Using a single row on its own is valid but conservative: that cutoff\n"
      "# is larger than the one the per-threshold table would give.\n"
      "# NOT equivalent to ETAC: this calibrates over SIMULATED noise, scores\n"
      "# clusters by area only, uses one blur level, and is spatially\n"
      "# stationary.  ETAC additionally permutes your real residuals, supports\n"
      "# richer merits and multiple blurs, and adapts to local smoothness.\n"
      "# See this program's -help under -multithresh.\n"
      "# Achieved null FPR per column (Monte Carlo, %d simulations):\n"
      "#  ", commandline, SSCS_VERSION, surface_label,
      sscs_mode_description(mode), opt->surface_nnode, opt->surface_nmask,
      opt->surface_area,
      opt->on_surface ? "surface" : "volume-to-surface",
      opt->compat ? "compat-adaptive" : "fixed", sigma, niter_min, niter_max,
      opt->niter, opt->nthreads, opt->rmm, opt->seed, opt->niter);
   for (aindex = 0; aindex < opt->nathr; ++aindex)
      fprintf(out, " %7.4f", achieved[aindex]);
   fprintf(out, "\n# pthr   |");
   for (aindex = 0; aindex < opt->nathr; ++aindex)
      fprintf(out, " %7.4f", opt->athr[aindex]);
   fprintf(out, "\n# ------- |");
   for (aindex = 0; aindex < opt->nathr; ++aindex) fprintf(out, " --------");
   fprintf(out, "\n");
   for (pindex = 0; pindex < opt->npthr; ++pindex) {
      fprintf(out, "%9.6f ", opt->pthr[pindex]);
      for (aindex = 0; aindex < opt->nathr; ++aindex)
         fprintf(out, " %8.2f", table[(size_t)pindex * opt->nathr + aindex]);
      fprintf(out, "\n");
   }
   fclose(out);
   if (opt->verb) {
      INFO_message("Wrote %s", filename);
      for (aindex = 0; aindex < opt->nathr; ++aindex)
         if (fabs(achieved[aindex] - opt->athr[aindex])
             > 0.25 * opt->athr[aindex])
            WARNING_message(
               "Multi-threshold family for alpha=%.4g achieved %.4g; with "
               "%d simulations the null max-area distribution is too coarse "
               "to calibrate this finely.  Use more -niter.",
               opt->athr[aindex], achieved[aindex], opt->niter);
   }
   ok = 1;

done:
   if (sorted) {
      for (pindex = 0; pindex < opt->npthr; ++pindex) free(sorted[pindex]);
      free(sorted);
   }
   free(cutoff); free(achieved); free(table);
   return ok;
}

static int sscs_output_1D(const SSCS_OPTIONS *opt, int mode,
                          const float *maxarea, const char *commandline,
                          const char *surface_label, double sigma,
                          int niter_min, int niter_max)
{
   char filename[THD_MAX_NAME];
   FILE *out;
   double *table;
   int pindex, aindex;
   snprintf(filename, sizeof(filename), "%s.%s.1D", opt->prefix,
            sscs_mode_label(mode));
   if (THD_is_file(filename) && !THD_ok_overwrite()) {
      ERROR_message("Output file exists: %s", filename); return 0;
   }
   out = fopen(filename, "w");
   if (!out) { ERROR_message("Cannot open %s", filename); return 0; }
   table = sscs_make_table(opt, maxarea);
   if (!table) { fclose(out); ERROR_message("Out of memory making table"); return 0; }
   fprintf(out,
      "# %s\n# SurfClustSim %s\n# Surface: %s\n# %s\n"
      "# Nodes: %d total, %d in mask; total area: %.9g; noise domain: %s\n"
      "# Smoothing: %s; sigma=%.8g; Niter range=%d..%d; simulations=%d; threads=%d\n"
      "# Connection radius rmm=%g; seed=%llu\n"
      "# CLUSTER AREA THRESHOLD(pthr,alpha) in surface-coordinate units^2\n"
      "# pthr   |", commandline, SSCS_VERSION, surface_label,
      sscs_mode_description(mode), opt->surface_nnode, opt->surface_nmask,
      opt->surface_area,
      opt->on_surface ? "surface" : "volume-to-surface",
      opt->compat ? "compat-adaptive" : "fixed", sigma, niter_min, niter_max,
      opt->niter, opt->nthreads, opt->rmm, opt->seed);
   for (aindex = 0; aindex < opt->nathr; ++aindex)
      fprintf(out, " %7.4f", opt->athr[aindex]);
   fprintf(out, "\n# ------- |");
   for (aindex = 0; aindex < opt->nathr; ++aindex) fprintf(out, " --------");
   fprintf(out, "\n");
   for (pindex = 0; pindex < opt->npthr; ++pindex) {
      fprintf(out, "%9.6f ", opt->pthr[pindex]);
      for (aindex = 0; aindex < opt->nathr; ++aindex)
         fprintf(out, " %8.2f",
                 table[(size_t)pindex * opt->nathr + aindex]);
      fprintf(out, "\n");
   }
   fclose(out); free(table);
   if (opt->verb) INFO_message("Wrote %s", filename);
   return 1;
}

/* Write the raw per-simulation maximum cluster areas, one file per p-value,
   in the layout slow_surf_clustsim.py produced as z.max.area.$pthr: one area
   per line, unsorted.  It differs in one respect -- that script appended a
   line only when a cluster existed ("if ( $maxa != \"\" )"), so its files were
   short by the number of empty simulations; here an empty simulation writes
   an explicit 0 and the file always has exactly niter lines.  Downstream
   alpha values agree as long as quick.alpha.vals.py is given -niter.  This is what makes the two
   pipelines directly comparable -- feed a file to quick.alpha.vals.py and
   diff its table against the old one.  The built-in alpha table cannot be
   compared that way, because quick.alpha.vals.py rounds areas to whole
   units and builds an empirical survival curve, while sscs_make_table()
   uses an interpolated quantile. */
static int sscs_output_maxarea(const SSCS_OPTIONS *opt, int mode,
                               const float *maxarea)
{
   char filename[THD_MAX_NAME];
   FILE *out;
   int pindex, iteration;

   for (pindex = 0; pindex < opt->npthr; ++pindex) {
      const float *column = maxarea + (size_t)pindex * opt->niter;
      snprintf(filename, sizeof(filename), "%s.%s.max.area.%g", opt->prefix,
               sscs_mode_label(mode), opt->pthr[pindex]);
      if (THD_is_file(filename) && !THD_ok_overwrite()) {
         ERROR_message("Output file exists: %s", filename); return 0;
      }
      out = fopen(filename, "w");
      if (!out) { ERROR_message("Cannot open %s", filename); return 0; }
      for (iteration = 0; iteration < opt->niter; ++iteration)
         fprintf(out, "%.6f\n", column[iteration]);
      fclose(out);
      if (opt->verb > 1) INFO_message("Wrote %s", filename);
   }
   if (opt->verb)
      INFO_message("Wrote %d raw max-area file%s (%s.%s.max.area.*)",
                   opt->npthr, opt->npthr == 1 ? "" : "s", opt->prefix,
                   sscs_mode_label(mode));
   return 1;
}

static int sscs_output_niml(const SSCS_OPTIONS *opt, int mode,
                            const float *maxarea, const char *commandline,
                            const char *surface_label, double sigma,
                            int niter_min, int niter_max)
{
   NI_element *nel;
   NI_float_array array;
   float *vector;
   double *table;
   char filename[THD_MAX_NAME], buffer[1024], *encoded;
   int pindex, aindex;

   snprintf(filename, sizeof(filename), "%s.%s.niml", opt->prefix,
            sscs_mode_label(mode));
   if (THD_is_file(filename) && !THD_ok_overwrite()) {
      ERROR_message("Output file exists: %s", filename); return 0;
   }
   nel = NI_new_data_element(SSCS_PROGRAM, opt->npthr);
   vector = (float *)malloc((size_t)MAX(opt->npthr, opt->nathr) * sizeof(float));
   table = sscs_make_table(opt, maxarea);
   if (!nel || !vector || !table) {
      if (nel) NI_free_element(nel);
      free(vector); free(table); return 0;
   }
   for (aindex = 0; aindex < opt->nathr; ++aindex) {
      for (pindex = 0; pindex < opt->npthr; ++pindex)
         vector[pindex] = (float)table[(size_t)pindex * opt->nathr + aindex];
      NI_add_column(nel, NI_FLOAT, vector);
   }
   NI_set_attribute(nel, "commandline", (char *)commandline);
   NI_set_attribute(nel, "surface", (char *)surface_label);
   NI_set_attribute(nel, "thresholding", (char *)sscs_mode_label(mode));
   NI_set_attribute(nel, "thresholding_description",
                    (char *)sscs_mode_description(mode));
   NI_set_attribute(nel, "smoothing", opt->compat ? "compat-adaptive" : "fixed");
   NI_set_attribute(nel, "noise_domain",
                    opt->on_surface ? "surface" : "volume-to-surface");
   if (opt->surf_mask_name)
      NI_set_attribute(nel, "surface_mask", opt->surf_mask_name);
   if (opt->vol_mask_name)
      NI_set_attribute(nel, "volume_mask", opt->vol_mask_name);
   snprintf(buffer, sizeof(buffer), "%d", opt->surface_nnode);
   NI_set_attribute(nel, "surface_nnode", buffer);
   snprintf(buffer, sizeof(buffer), "%d", opt->surface_nmask);
   NI_set_attribute(nel, "surface_mask_count", buffer);
   snprintf(buffer, sizeof(buffer), "%.12g", opt->surface_area);
   NI_set_attribute(nel, "surface_area", buffer);
   snprintf(buffer, sizeof(buffer), "%d", opt->niter);
   NI_set_attribute(nel, "iter", buffer);
   snprintf(buffer, sizeof(buffer), "%.9g", sigma);
   NI_set_attribute(nel, "sigma", buffer);
   snprintf(buffer, sizeof(buffer), "%d,%d", niter_min, niter_max);
   NI_set_attribute(nel, "smoothing_niter_range", buffer);
   snprintf(buffer, sizeof(buffer), "%g", opt->rmm);
   NI_set_attribute(nel, "rmm", buffer);
   snprintf(buffer, sizeof(buffer), "%llu", opt->seed);
   NI_set_attribute(nel, "seed", buffer);
   snprintf(buffer, sizeof(buffer), "%d", opt->nthreads);
   NI_set_attribute(nel, "threads", buffer);
   array.num = opt->npthr;
   for (pindex = 0; pindex < opt->npthr; ++pindex) vector[pindex] = opt->pthr[pindex];
   array.ar = vector; encoded = NI_encode_float_list(&array, ",");
   NI_set_attribute(nel, "pthr", encoded); NI_free(encoded);
   array.num = opt->nathr;
   for (aindex = 0; aindex < opt->nathr; ++aindex) vector[aindex] = opt->athr[aindex];
   array.ar = vector; encoded = NI_encode_float_list(&array, ",");
   NI_set_attribute(nel, "athr", encoded); NI_free(encoded);
   if (NI_write_element_tofile(filename, nel, NI_TEXT_MODE) < 0) {
      ERROR_message("Failed to write %s", filename);
      NI_free_element(nel); free(vector); free(table);
      return 0;
   }
   NI_free_element(nel); free(vector); free(table);
   if (opt->verb) INFO_message("Wrote %s", filename);
   return 1;
}

int main(int argc, char **argv)
{
   static char FuncName[] = SSCS_PROGRAM;
   SSCS_OPTIONS opt;
   SUMA_GENERIC_ARGV_PARSE *ps = NULL;
   SUMA_SurfSpecFile *spec = NULL;
   SUMA_SurfaceObject *SO = NULL, *SOB = NULL;
   SUMA_SURFCLUSTSIM_GRAPH *graph = NULL;
   SUMA_surface volsurf_a, volsurf_b;
   THD_3dim_dataset *volmask_dset = NULL;
   byte *surf_mask = NULL, *volmask = NULL;
   float *node_area = NULL, **field = NULL;
   float *maxarea[SSCS_NMODE] = {NULL, NULL, NULL, NULL};
   double *zthr_one = NULL, *zthr_two = NULL;
   int *smooth_iters = NULL;
   double **wgt = NULL;
   double sigma, node_area_sum = 0.0, face_area_sum = 0.0;
   char *commandline;
   int nspec = 0, block, bcount, col, iteration, mode, zindex;
   int block_number, total_blocks;
   SUMA_SURFCLUSTSIM_ACF *acf_model = NULL;
   int smooth_niter, niter_min, niter_max, hint;
   int block_completed;
   SSCS_PROGRESS adaptive_progress, simulation_progress;
   float achieved_fwhm = -1.0f;
   int status = 0;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   sscs_init_options(&opt);
   if (argc < 2) { sscs_help(); return 0; }

   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct(SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-spec;-i;-sv;-s;");
   sscs_parse_options(argc, argv, &opt, ps);
   if ((double)opt.niter * opt.athr[opt.nathr - 1] < 10.0)
      WARNING_message(
         "Only %.1f expected simulations in the smallest alpha tail (%g); "
         "consider at least %d simulations",
         (double)opt.niter * opt.athr[opt.nathr - 1],
         opt.athr[opt.nathr - 1],
         (int)ceil(10.0 / opt.athr[opt.nathr - 1]));

#ifdef USE_OMP
   omp_set_dynamic(0);
   if (opt.nthreads > 0) omp_set_num_threads(opt.nthreads);
   opt.nthreads = omp_get_max_threads();
#else
   if (opt.nthreads > 1)
      WARNING_message("This build has no OpenMP support; using one thread");
   opt.nthreads = 1;
#endif
   if (opt.verb) sscs_report_openmp(&opt);

   spec = SUMA_IO_args_2_spec(ps, &nspec);
   if (nspec != 1) ERROR_exit("Need exactly one surface specification");
   if (spec->N_Surfs < (opt.on_surface ? 1 : 2))
      ERROR_exit("Need %d selected surface%s", opt.on_surface ? 1 : 2,
                 opt.on_surface ? "" : "s (-surf_A and -surf_B)");
   SO = SUMA_Load_Spec_Surf(spec, 0, ps->N_sv ? ps->sv[0] : NULL, 0);
   if (!SO) ERROR_exit("Failed to load -surf_A");
   if (!opt.on_surface) {
      SOB = SUMA_Load_Spec_Surf(spec, 1, ps->N_sv ? ps->sv[0] : NULL, 0);
      if (!SOB) ERROR_exit("Failed to load -surf_B");
      if (SO->N_Node != SOB->N_Node)
         ERROR_exit("-surf_A and -surf_B have different node counts");
   }
   if (!SUMA_SurfaceMetrics(SO, "EdgeList", NULL))
      ERROR_exit("Failed to construct surface metrics");
   node_area = SUMA_CalculateNodeAreas(SO, NULL);
   if (!node_area) ERROR_exit("Failed to calculate node areas");
   for (zindex = 0; zindex < SO->N_Node; ++zindex) {
      if (node_area[zindex] < 0.0f) ERROR_exit("Negative area at surface node %d", zindex);
      node_area_sum += node_area[zindex];
   }
   if (SO->PolyArea) {
      for (zindex = 0; zindex < SO->N_FaceSet; ++zindex)
         face_area_sum += SO->PolyArea[zindex];
      if (face_area_sum > 0.0 &&
          fabs(node_area_sum - face_area_sum) > 1.e-5 * face_area_sum)
         WARNING_message("Node areas sum to %.9g but face areas sum to %.9g",
                         node_area_sum, face_area_sum);
   }
   opt.surface_area = node_area_sum;
   /* Build the Ziggurat lookup tables once, before anything generates noise.
      They are read-only afterwards and so safe to share across threads. */
   zgaussian2_init((uint32_t)(opt.seed & 0xffffffffu));

   graph = SUMA_SurfClustSim_MakeGraph(SO, opt.rmm);
   if (!graph) ERROR_exit("Failed to construct the clustering graph for rmm=%g", opt.rmm);
   /* SUMA_SurfClustSim_MaxAreasSweep() requires each threshold array to be
      nondecreasing: it walks thresholds downward while activating nodes
      monotonically, and would silently return wrong areas otherwise.  That
      holds here because sscs_parse_options() sorts opt.pthr descending (see
      sscs_double_desc) and sscs_zthreshold() is monotonically decreasing in
      p, so both z arrays come out ascending.  The sweep revalidates this,
      but the coupling is easy to break from a distance -- if you ever change
      how opt.pthr is ordered, this is the other half of the contract. */
   zthr_one = (double *)malloc((size_t)opt.npthr * sizeof(double));
   zthr_two = (double *)malloc((size_t)opt.npthr * sizeof(double));
   if (!zthr_one || !zthr_two) ERROR_exit("Out of memory allocating z thresholds");
   for (zindex = 0; zindex < opt.npthr; ++zindex) {
      zthr_one[zindex] = sscs_zthreshold(opt.pthr[zindex]);
      zthr_two[zindex] = sscs_zthreshold(0.5 * opt.pthr[zindex]);
   }
   for (zindex = 1; zindex < opt.npthr; ++zindex)
      if (zthr_one[zindex] < zthr_one[zindex - 1] ||
          zthr_two[zindex] < zthr_two[zindex - 1])
         ERROR_exit("Internal error: z thresholds are not ascending "
                    "(pthr must be sorted descending)");

   if (opt.surf_mask_name) {
      surf_mask = sscs_load_surface_mask(opt.surf_mask_name, SO);
      if (!surf_mask) ERROR_exit("Failed to load surface mask %s", opt.surf_mask_name);
   }
   opt.surface_nnode = SO->N_Node;
   opt.surface_nmask = surf_mask ? THD_countmask(SO->N_Node, surf_mask) : SO->N_Node;
   memset(&volsurf_a, 0, sizeof(volsurf_a));
   memset(&volsurf_b, 0, sizeof(volsurf_b));
   if (!opt.on_surface) {
      volmask_dset = THD_open_dataset(opt.vol_mask_name);
      if (!volmask_dset) ERROR_exit("Cannot open volume mask %s", opt.vol_mask_name);
      volmask = THD_makemask(volmask_dset, 0, 1.0f, 0.0f);
      if (!volmask || THD_countmask(DSET_NVOX(volmask_dset), volmask) < 2)
         ERROR_exit("Volume mask has fewer than 2 nonzero voxels");
      if (!sscs_copy_surface(SO, &volsurf_a) || !sscs_copy_surface(SOB, &volsurf_b))
         ERROR_exit("Failed to prepare surfaces for volume mapping");
   }

   sigma = opt.sigma;
   if (opt.compat && sigma <= 0.0) {
      hint = -100;
      sigma = SUMA_SigForFWHM(SO->EL->AvgLe, opt.target_fwhm, &hint, NULL);
      if (sigma <= 0.0) ERROR_exit("Could not choose sigma for target FWHM %.4g",
                                   opt.target_fwhm);
      sigma *= SO->EL->AvgLe;
   }
   if (opt.acf_mode) {
      /* Position the basis ladder on the scale the requested ACF actually
         occupies.  Find the effective FWHM of the target curve (where it
         falls to 0.5, doubled), convert to the kernel width that produces it
         -- smoothing a white field with a kernel of width W gives an ACF
         about sqrt(2)*W wide -- and ask SUMA for the sigma and pass count
         that reach it.  The ladder then spans around that. */
      double half = 0.0, step = 0.05 * SO->EL->AvgLe, r, prev = 1.0;
      double target_fwhm_eff, kernel_fwhm;
      for (r = step; r < 500.0 * SO->EL->AvgLe; r += step) {
         double v = sscs_acf_model(opt.acf_a, opt.acf_b, opt.acf_c, r);
         if (v <= 0.5) {
            half = (prev > v) ? r - step + step*(prev-0.5)/(prev-v) : r;
            break;
         }
         prev = v;
      }
      if (half <= 0.0)
         ERROR_exit("Requested ACF never falls below 0.5; check -acf a b c");
      target_fwhm_eff = 2.0 * half;
      kernel_fwhm = target_fwhm_eff / sqrt(2.0);
      if (kernel_fwhm < 2.05 * SO->EL->AvgLe)
         kernel_fwhm = 2.05 * SO->EL->AvgLe;  /* SUMA's minimum ratio */
      hint = -100;
      {  double s0 = SUMA_SigForFWHM(SO->EL->AvgLe, kernel_fwhm, &hint, NULL);
         if (s0 <= 0.0)
            ERROR_exit("Could not choose a kernel for the requested ACF "
                       "(effective FWHM %.4g)", target_fwhm_eff);
         if (sigma <= 0.0) sigma = s0 * SO->EL->AvgLe;
      }
      if (!opt.smooth_niter_given) opt.smooth_niter = (hint > 0) ? hint : 16;
      if (opt.verb)
         INFO_message("ACF target: effective FWHM %.4g; kernel sigma %.4g; "
                      "basis ladder centred on %d passes",
                      target_fwhm_eff, sigma, opt.smooth_niter);
   }
   wgt = SUMA_Chung_Smooth_Weights_07(SO, sigma);
   if (!wgt) ERROR_exit("Failed to construct Chung HEAT_07 weights");

   if (opt.acf_mode) {
      /* Calibrate the basis mixture once.  This measures each basis field's
         autocorrelation and solves for the weights whose combination best
         matches the requested curve, so the per-simulation work afterwards
         is just smoothing and adding. */
      if (opt.verb)
         INFO_message("Calibrating ACF mixture for a=%.4g b=%.4g c=%.4g "
                      "(%d basis fields)",
                      opt.acf_a, opt.acf_b, opt.acf_c, opt.acf_nbasis);
      acf_model = SUMA_SurfClustSim_ACF_Calibrate(
         SO, wgt, surf_mask, opt.acf_a, opt.acf_b, opt.acf_c,
         opt.acf_nbasis, opt.smooth_niter > 0 ? opt.smooth_niter : 16,
         opt.acf_radius, 0.0f, opt.seed, opt.verb);
      if (!acf_model) ERROR_exit("Failed to calibrate the ACF mixture");
      if (opt.verb) {
         int k;
         for (k = 0; k < acf_model->nbasis; ++k)
            INFO_message("  basis %d: %4d passes, weight %.4f",
                         k, acf_model->niter[k], acf_model->weight[k]);
         INFO_message("ACF fit RMS %.5f; %d smoothing passes per simulation",
                      acf_model->fit_rms, acf_model->total_passes);
      }
      if (acf_model->measured_a >= 0.0) {
         if (opt.verb)
            INFO_message("ACF verify: requested a=%.4g b=%.4g c=%.4g -> "
                         "generated a=%.4g b=%.4g c=%.4g (FWHM %.4g)",
                         opt.acf_a, opt.acf_b, opt.acf_c,
                         acf_model->measured_a, acf_model->measured_b,
                         acf_model->measured_c, acf_model->measured_fwhm);
         /* The 'a' parameter is what the whole exercise is about -- it is the
            Gaussian-versus-heavy-tail split -- so hold it to a real tolerance
            and only grumble about b and c, which are weakly identified
            whenever their component carries little weight. */
         if (fabs(acf_model->measured_a - opt.acf_a) > 0.10)
            WARNING_message(
               "Generated noise has a=%.4g but a=%.4g was requested.  The "
               "basis cannot reach the requested shape; try a larger "
               "-acf_nbasis.", acf_model->measured_a, opt.acf_a);
      } else {
         WARNING_message("Could not verify the generated ACF; proceeding "
                         "on the fit alone.");
      }
      if (acf_model->fit_rms > 0.05)
         WARNING_message(
            "ACF mixture fits the requested curve only to RMS %.4f.  The "
            "basis may not span the requested scales: try -acf_nbasis higher.",
            acf_model->fit_rms);
   }

   for (mode = 0; mode < SSCS_NMODE; ++mode)
      if (opt.side[mode]) {
         maxarea[mode] = (float *)calloc((size_t)opt.npthr * opt.niter, sizeof(float));
         if (!maxarea[mode]) ERROR_exit("Out of memory allocating results");
      }
   smooth_iters = (int *)calloc((size_t)opt.niter, sizeof(int));
   if (!smooth_iters) ERROR_exit("Out of memory allocating smoothing records");

   if (opt.verb) {
      INFO_message("Surface has %d nodes and area %.9g; rmm=%g graph has %d directed links",
                   SO->N_Node, node_area_sum, opt.rmm, graph->nedge);
      INFO_message("Running %d simulations in %s mode (sigma=%.6g, threads=%d)",
                   opt.niter, opt.compat ? "compat-adaptive" : "fixed", sigma,
                   opt.nthreads);
   }

   block_number = 0;
   total_blocks = opt.niter / opt.itersize +
                  (opt.niter % opt.itersize != 0);

   /* Parallelize ACROSS blocks, not within one.
      -itersize is a statistical parameter, not a scheduling one: in -compat
      it is the set of fields over which the master is detrended and a single
      adaptive Niter is chosen, exactly as SurfSmooth did when
      slow_surf_clustsim.py handed it $itersize sub-bricks at a time.  Running
      the OpenMP loop over columns inside a block therefore capped the useful
      thread count at -itersize and forced a barrier at every block boundary.
      Distributing whole blocks instead leaves each block's contents -- and so
      every result -- untouched, because noise is seeded from the global
      simulation index rather than from anything about the schedule.
      schedule(dynamic,1) because -compat blocks take unequal time (each runs
      its own adaptive search). */
#ifdef USE_OMP
#pragma omp parallel
#endif
   {
      SUMA_SURFCLUSTSIM_WORK *work = SUMA_SurfClustSim_NewWork(SO->N_Node);
      int myblock;
#ifdef USE_OMP
#pragma omp for schedule(dynamic,1)
#endif
      for (myblock = 0; myblock < total_blocks; ++myblock) {
         int first = myblock * opt.itersize;
         int mycount = MIN(opt.itersize, opt.niter - first);
         int mysmooth_niter, mycol, pindex;
         float myachieved_fwhm = -1.0f;
         float **myfield = NULL;
         int block_ok = (work != NULL);

         if (block_ok) {
            if (opt.acf_mode) {
               /* The mixture IS the smoothing: each component is already
                  smoothed by its own number of passes before being combined,
                  so no further pass is applied below. */
               myfield = sscs_acf_fields(&opt, first, mycount, SO->N_Node,
                                         SO, wgt, acf_model, surf_mask, work);
            } else if (opt.on_surface) {
               myfield = sscs_surface_fields(&opt, first, mycount, SO->N_Node,
                                             surf_mask);
            } else {
               /* opt_vol2surf() and the AFNI dataset routines under it are not
                  reentrant, so volume-mode field generation is serialized.
                  Everything after it still runs in parallel.  See section 14
                  of SurfClusterize_PLAN.md for how to lift this. */
#ifdef USE_OMP
#pragma omp critical(sscs_vol2surf)
#endif
               myfield = sscs_volume_fields(&opt, first, mycount, SO->N_Node,
                                            volmask_dset, volmask,
                                            &volsurf_a, &volsurf_b);
            }
            if (!myfield) block_ok = 0;
            for (mycol = 0; block_ok && mycol < mycount; ++mycol)
               if (!myfield[mycol]) block_ok = 0;
         }

         if (block_ok) {
            if (opt.compat) {
               mysmooth_niter = SUMA_SurfClustSim_ChooseCompatNiter(
                  SO, wgt, myfield, mycount, surf_mask, opt.target_fwhm,
                  opt.max_smooth_niter, &myachieved_fwhm, NULL, NULL);
               if (mysmooth_niter < 0) {
                  ERROR_message("Adaptive smoothing failed in block %d", first);
                  block_ok = 0;
               } else if (mysmooth_niter == opt.max_smooth_niter &&
                          myachieved_fwhm <= opt.target_fwhm) {
#ifdef USE_OMP
#pragma omp critical(sscs_report)
#endif
                  WARNING_message(
                     "Block %d..%d reached -max_Niter %d at FWHM %.4g, below "
                     "target %.4g; consider a larger -sigma or -max_Niter",
                     first + 1, first + mycount, opt.max_smooth_niter,
                     myachieved_fwhm, opt.target_fwhm);
               }
            } else if (opt.acf_mode) {
               mysmooth_niter = 0;   /* already smoothed, per basis */
            } else {
               mysmooth_niter = opt.smooth_niter;
            }
         }

         if (block_ok) {
            for (mycol = 0; mycol < mycount; ++mycol)
               smooth_iters[first + mycol] = mysmooth_niter;
            if (opt.verb > 2) {
#ifdef USE_OMP
#pragma omp critical(sscs_report)
#endif
               INFO_message("Block %d..%d: smoothing Niter=%d (master FWHM %.4g)",
                            first + 1, first + mycount, mysmooth_niter,
                            myachieved_fwhm);
            }
         }

         for (mycol = 0; block_ok && mycol < mycount; ++mycol) {
            double stdev;
            double one_area[opt.npthr], two_area[opt.npthr];
            double pos_bi_area[opt.npthr], neg_bi_area[opt.npthr];
            double legacy_area[opt.npthr];
            int sim_index = first + mycol;
            int worker_ok = SUMA_SurfClustSim_SmoothFixed(
               SO, wgt, mysmooth_niter, myfield[mycol], surf_mask, work) &&
               SUMA_SurfClustSim_Rescale(
                  myfield[mycol], SO->N_Node, surf_mask, &stdev);
            if (worker_ok &&
                (opt.side[SSCS_ONE_SIDED] || opt.side[SSCS_BI_SIDED]))
               worker_ok = SUMA_SurfClustSim_MaxAreasSweep(
                  graph, node_area, myfield[mycol], surf_mask, 1,
                  zthr_one, opt.side[SSCS_ONE_SIDED] ? opt.npthr : 0, one_area,
                  zthr_two, opt.side[SSCS_BI_SIDED] ? opt.npthr : 0, pos_bi_area,
                  work);
            if (worker_ok &&
                (opt.side[SSCS_TWO_SIDED] || opt.side[SSCS_LEGACY_SIDED]))
               worker_ok = SUMA_SurfClustSim_MaxAreasSweep(
                  graph, node_area, myfield[mycol], surf_mask, 0,
                  zthr_two, opt.side[SSCS_TWO_SIDED] ? opt.npthr : 0, two_area,
                  zthr_one, opt.side[SSCS_LEGACY_SIDED] ? opt.npthr : 0,
                  legacy_area, work);
            if (worker_ok && opt.side[SSCS_BI_SIDED])
               worker_ok = SUMA_SurfClustSim_MaxAreasSweep(
                  graph, node_area, myfield[mycol], surf_mask, -1,
                  zthr_two, opt.npthr, neg_bi_area, NULL, 0, NULL, work);
            /* Cross-check the union-find sweep against an independent
               flood fill.  The sweep gets every threshold from one sorted
               pass by exploiting the fact that max cluster area is monotone
               as the threshold falls; the BFS re-floods each threshold from
               scratch.  Any disagreement is a bug in the sweep, so treat it
               as fatal rather than warning and continuing. */
            if (worker_ok && opt.selfcheck) {
               struct { int mode; int sign; const double *swept;
                        const double *zthr; const char *half; } checks[] = {
                  { SSCS_ONE_SIDED,     1, one_area,    zthr_one, "" },
                  { SSCS_TWO_SIDED,     0, two_area,    zthr_two, "" },
                  { SSCS_LEGACY_SIDED,  0, legacy_area, zthr_one, "" },
                  { SSCS_BI_SIDED,      1, pos_bi_area, zthr_two, " (+)" },
                  { SSCS_BI_SIDED,     -1, neg_bi_area, zthr_two, " (-)" } };
               int check, ncheck = (int)(sizeof(checks)/sizeof(checks[0]));
               for (check = 0; check < ncheck && worker_ok; ++check) {
                  if (!opt.side[checks[check].mode]) continue;
                  for (pindex = 0; pindex < opt.npthr; ++pindex) {
                     double bfs = SUMA_SurfClustSim_MaxArea(
                        graph, node_area, myfield[mycol], surf_mask,
                        checks[check].zthr[pindex], checks[check].sign, work);
                     double swept = checks[check].swept[pindex];
                     /* The flood fill and the union-find sweep sum the SAME
                        node areas but in different orders, and floating-point
                        addition is not associative, so a few ULPs of drift are
                        expected and harmless.  Compare within a relative
                        tolerance rather than bitwise -- an exact test turns a
                        rounding difference into a fatal ERROR_exit. */
                     if (fabs(bfs - swept) >
                         1.0e-9 * (fabs(bfs) + fabs(swept)) + 1.0e-30) {
                        ERROR_message(
                           "-selfcheck FAILED: sim %d, %s%s, pthr %g, z %.10g: "
                           "sweep %.10g vs BFS %.10g",
                           sim_index, sscs_mode_label(checks[check].mode),
                           checks[check].half, opt.pthr[pindex],
                           checks[check].zthr[pindex],
                           swept, bfs);
                        worker_ok = 0;
                        break;
                     }
                  }
               }
            }
            if (!worker_ok) {
               block_ok = 0;
            } else {
               for (pindex = 0; pindex < opt.npthr; ++pindex) {
                  size_t result_index = (size_t)pindex * opt.niter + sim_index;
                  if (opt.side[SSCS_ONE_SIDED])
                     maxarea[SSCS_ONE_SIDED][result_index] =
                        (float)one_area[pindex];
                  if (opt.side[SSCS_TWO_SIDED])
                     maxarea[SSCS_TWO_SIDED][result_index] =
                        (float)two_area[pindex];
                  if (opt.side[SSCS_BI_SIDED])
                     maxarea[SSCS_BI_SIDED][result_index] =
                        (float)MAX(pos_bi_area[pindex], neg_bi_area[pindex]);
                  if (opt.side[SSCS_LEGACY_SIDED])
                     maxarea[SSCS_LEGACY_SIDED][result_index] =
                        (float)legacy_area[pindex];
               }
            }
         }

         sscs_free_fields(myfield, mycount);
#ifdef USE_OMP
#pragma omp critical(sscs_report)
#endif
         {
            if (!block_ok) status = 1;
            ++block_number;
            if (opt.verb == 1 &&
                (block_number % 10 == 0 || block_number == total_blocks))
               INFO_message(
                  "Progress: %d/%d blocks (%5.1f%%), %d simulations",
                  block_number, total_blocks,
                  100.0 * (double)block_number / (double)total_blocks,
                  MIN(block_number * opt.itersize, opt.niter));
         }
      }
      SUMA_SurfClustSim_FreeWork(work);
   }
   if (status) ERROR_exit("A simulation worker failed");

   niter_min = niter_max = smooth_iters[0];
   for (iteration = 1; iteration < opt.niter; ++iteration) {
      niter_min = MIN(niter_min, smooth_iters[iteration]);
      niter_max = MAX(niter_max, smooth_iters[iteration]);
   }
   commandline = tross_commandline(FuncName, argc, argv);
   for (mode = 0; mode < SSCS_NMODE; ++mode) if (opt.side[mode]) {
      if (opt.do_1D && !sscs_output_1D(&opt, mode, maxarea[mode], commandline,
                                      SO->Label, sigma, niter_min, niter_max))
         status = 1;
      if (opt.do_niml && !sscs_output_niml(&opt, mode, maxarea[mode], commandline,
                                          SO->Label, sigma, niter_min, niter_max))
         status = 1;
      if (opt.do_maxarea && !sscs_output_maxarea(&opt, mode, maxarea[mode]))
         status = 1;
      if (opt.do_mthresh && !sscs_output_mthresh(&opt, mode, maxarea[mode],
                                          commandline, SO->Label, sigma,
                                          niter_min, niter_max))
         status = 1;
   }
   free(commandline);
   for (mode = 0; mode < SSCS_NMODE; ++mode) free(maxarea[mode]);
   free(zthr_one); free(zthr_two);
   free(smooth_iters);
   SUMA_SurfClustSim_ACF_Free(acf_model);
   SUMA_free2D((char **)wgt, SO->N_Node);
   SUMA_SurfClustSim_FreeGraph(graph);
   SUMA_free(node_area);
   free(surf_mask); free(volmask);
   if (volmask_dset) DSET_delete(volmask_dset);
   sscs_free_surface(&volsurf_a); sscs_free_surface(&volsurf_b);
   free(opt.prefix); free(opt.map_func); free(opt.pthr); free(opt.athr);
   return status;
}
