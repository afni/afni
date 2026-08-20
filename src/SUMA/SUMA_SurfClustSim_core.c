#include "SUMA_SurfClustSim_core.h"
#include "SUMA_SurfACF.h"

#include <float.h>
#include <stdint.h>

/* AFNI's re-entrant Ziggurat Gaussian generator. */
#include "../zgaussian.c"

static uint64_t sscs_splitmix64(uint64_t *state)
{
   uint64_t z;
   *state += UINT64_C(0x9e3779b97f4a7c15);
   z = *state;
   z = (z ^ (z >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
   z = (z ^ (z >> 27)) * UINT64_C(0x94d049bb133111eb);
   return z ^ (z >> 31);
}

static void sscs_seed48(unsigned long long seed, int iteration,
                        unsigned short xran[3])
{
   uint64_t state = (uint64_t)seed ^
      (UINT64_C(0xd1b54a32d192ed03) * (uint64_t)(iteration + 1));
   uint64_t bits = sscs_splitmix64(&state);
   xran[0] = (unsigned short)(bits & 0xffffu);
   xran[1] = (unsigned short)((bits >> 16) & 0xffffu);
   xran[2] = (unsigned short)((bits >> 32) & 0xffffu);
   if (!(xran[0] || xran[1] || xran[2])) xran[0] = 0x330e;
}

void SUMA_SurfClustSim_FillNoise(float *field, int nnode, const byte *mask,
                                 unsigned long long seed, int iteration)
{
   unsigned short xran[3];
   int node;
   sscs_seed48(seed, iteration, xran);
   for (node = 0; node < nnode; ++node) {
      float value = zgaussian_sss(xran);
      field[node] = (!mask || mask[node]) ? value : 0.0f;
   }
}

static int sscs_graph_count(SUMA_SurfaceObject *SO, float rmm,
                            SUMA_GET_OFFSET_STRUCT *off)
{
   int layer, item, count = 0;
   (void)SO;
   if (rmm < 0.0f) {
      int max_layer = -(int)rmm;
      for (layer = 1; layer < off->N_layers && layer <= max_layer; ++layer)
         count += off->layers[layer].N_NodesInLayer;
   } else {
      for (layer = 1; layer < off->N_layers; ++layer)
         for (item = 0; item < off->layers[layer].N_NodesInLayer; ++item)
            if (off->OffVect[off->layers[layer].NodesInLayer[item]] <= rmm)
               ++count;
   }
   return count;
}

static int sscs_graph_copy(SUMA_SurfaceObject *SO, float rmm,
                           SUMA_GET_OFFSET_STRUCT *off, int *out)
{
   int layer, item, count = 0, node;
   (void)SO;
   if (rmm < 0.0f) {
      int max_layer = -(int)rmm;
      for (layer = 1; layer < off->N_layers && layer <= max_layer; ++layer)
         for (item = 0; item < off->layers[layer].N_NodesInLayer; ++item)
            out[count++] = off->layers[layer].NodesInLayer[item];
   } else {
      for (layer = 1; layer < off->N_layers; ++layer)
         for (item = 0; item < off->layers[layer].N_NodesInLayer; ++item) {
            node = off->layers[layer].NodesInLayer[item];
            if (off->OffVect[node] <= rmm) out[count++] = node;
         }
   }
   return count;
}

SUMA_SURFCLUSTSIM_GRAPH *SUMA_SurfClustSim_MakeGraph(
   SUMA_SurfaceObject *SO, float rmm)
{
   SUMA_SURFCLUSTSIM_GRAPH *graph = NULL;
   SUMA_GET_OFFSET_STRUCT *off = NULL;
   int node, item, count;

   if (!SO || !SO->FN || rmm == 0.0f || (-1.0f < rmm && rmm < 0.0f))
      return NULL;

   graph = (SUMA_SURFCLUSTSIM_GRAPH *)calloc(1, sizeof(*graph));
   if (!graph) return NULL;
   graph->nnode = SO->N_Node;
   graph->offset = (int *)calloc((size_t)graph->nnode + 1, sizeof(int));
   if (!graph->offset) goto fail;

   if (rmm == -1.0f) {
      for (node = 0; node < graph->nnode; ++node)
         graph->offset[node + 1] = graph->offset[node] + SO->FN->N_Neighb[node];
      graph->nedge = graph->offset[graph->nnode];
      graph->neighbor = (int *)malloc((size_t)graph->nedge * sizeof(int));
      if (!graph->neighbor) goto fail;
      for (node = 0; node < graph->nnode; ++node)
         for (item = 0; item < SO->FN->N_Neighb[node]; ++item)
            graph->neighbor[graph->offset[node] + item] =
               SO->FN->FirstNeighb[node][item];
      return graph;
   }

   off = SUMA_Initialize_getoffsets(SO->N_Node);
   if (!off) goto fail;
   for (node = 0; node < graph->nnode; ++node) {
      if (!SUMA_getoffsets2(node, SO, rmm, off, NULL, 0)) goto fail;
      count = sscs_graph_count(SO, rmm, off);
      graph->offset[node + 1] = graph->offset[node] + count;
      SUMA_Recycle_getoffsets(off);
   }
   graph->nedge = graph->offset[graph->nnode];
   graph->neighbor = (int *)malloc((size_t)graph->nedge * sizeof(int));
   if (!graph->neighbor) goto fail;
   for (node = 0; node < graph->nnode; ++node) {
      if (!SUMA_getoffsets2(node, SO, rmm, off, NULL, 0)) goto fail;
      count = sscs_graph_copy(SO, rmm, off, graph->neighbor + graph->offset[node]);
      if (count != graph->offset[node + 1] - graph->offset[node]) goto fail;
      SUMA_Recycle_getoffsets(off);
   }
   SUMA_Free_getoffsets(off);
   return graph;

fail:
   if (off) SUMA_Free_getoffsets(off);
   SUMA_SurfClustSim_FreeGraph(graph);
   return NULL;
}

void SUMA_SurfClustSim_FreeGraph(SUMA_SURFCLUSTSIM_GRAPH *graph)
{
   if (!graph) return;
   free(graph->offset);
   free(graph->neighbor);
   free(graph);
}

SUMA_SURFCLUSTSIM_WORK *SUMA_SurfClustSim_NewWork(int nnode)
{
   SUMA_SURFCLUSTSIM_WORK *work;
   if (nnode <= 0) return NULL;
   work = (SUMA_SURFCLUSTSIM_WORK *)calloc(1, sizeof(*work));
   if (!work) return NULL;
   work->active = (byte *)malloc((size_t)nnode * sizeof(byte));
   work->queue = (int *)malloc((size_t)nnode * sizeof(int));
   work->order = (SUMA_SURFCLUSTSIM_NODE_KEY *)malloc(
      (size_t)nnode * sizeof(*work->order));
   work->work_a = (double *)malloc((size_t)nnode * sizeof(double));
   work->work_b = (double *)malloc((size_t)nnode * sizeof(double));
   if (!work->active || !work->queue || !work->order ||
       !work->work_a || !work->work_b) {
      SUMA_SurfClustSim_FreeWork(work);
      return NULL;
   }
   return work;
}

void SUMA_SurfClustSim_FreeWork(SUMA_SURFCLUSTSIM_WORK *work)
{
   if (!work) return;
   free(work->active);
   free(work->queue);
   free(work->order);
   free(work->work_a);
   free(work->work_b);
   free(work);
}

/* ------------------------------------------------------------------------
   FORKED KERNEL -- keep in sync by hand.

   Mirrors the HEAT_07 filtering pass written out inline in
   SUMA_Chung_Smooth_07_dset(), SUMA_GeomComp.c (search for
   "filter iteration for each node in data column k"; the double-precision
   copy).  SUMA has no callable entry point for a single pass -- the loop is
   welded inside a 200-line SUMA_DSET wrapper -- and running it per simulation
   through that wrapper would mean NI element churn in the hot loop.  So it is
   copied rather than called.

   Note that SUMA itself carries two copies of this loop (double here,
   single-precision in SUMA_Chung_Smooth_07_toFWHM_dset()), so the kernel is
   forked four ways in total.  If anyone ever touches the HEAT_07 update rule,
   grep for "FORKED KERNEL" in this file.

   Behaviour that must match exactly:
     - no mask: weights are assumed to sum to 1, no renormalization.
     - mask:    out-of-mask nodes pass through unchanged; in-mask nodes are
                divided by the sum of the weights actually used, so that
                dropping masked neighbours does not shrink the result.
                This is SUMA's strict_mask == 1 behaviour, which is what
                SurfSmooth's callers pass.
   ------------------------------------------------------------------------ */
static void sscs_smooth_double_once(SUMA_SurfaceObject *SO, double **wgt,
                                    const double *in, double *out,
                                    const byte *mask)
{
   int node, item, other;
   double sum, denom;
   for (node = 0; node < SO->N_Node; ++node) {
      if (mask && !mask[node]) {
         out[node] = in[node];
         continue;
      }
      sum = in[node] * wgt[node][0];
      denom = wgt[node][0];
      for (item = 0; item < SO->FN->N_Neighb[node]; ++item) {
         other = SO->FN->FirstNeighb[node][item];
         if (!mask || mask[other]) {
            sum += wgt[node][item + 1] * in[other];
            denom += wgt[node][item + 1];
         }
      }
      out[node] = mask ? sum / denom : sum;
   }
}

/* FORKED KERNEL -- single-precision twin of sscs_smooth_double_once().
   Mirrors the copy inline in SUMA_Chung_Smooth_07_toFWHM_dset(),
   SUMA_GeomComp.c.  Accumulation is in double; only the buffers are float.
   See sscs_smooth_double_once() for the masking rules. */
static void sscs_smooth_float_once(SUMA_SurfaceObject *SO, double **wgt,
                                   const float *in, float *out,
                                   const byte *mask)
{
   int node, item, other;
   double sum, denom;
   for (node = 0; node < SO->N_Node; ++node) {
      if (mask && !mask[node]) {
         out[node] = in[node];
         continue;
      }
      sum = (double)in[node] * wgt[node][0];
      denom = wgt[node][0];
      for (item = 0; item < SO->FN->N_Neighb[node]; ++item) {
         other = SO->FN->FirstNeighb[node][item];
         if (!mask || mask[other]) {
            sum += wgt[node][item + 1] * (double)in[other];
            denom += wgt[node][item + 1];
         }
      }
      out[node] = (float)(mask ? sum / denom : sum);
   }
}

/* ------------------------------------------------------------------------
   FORKED KERNEL -- keep in sync by hand.

   Same 1-difference estimator as SUMA_estimate_FWHM_1dif() in
   SUMA_GeomComp.c, called there with nodup=1.  Two deliberate differences,
   neither of which changes the returned value:

     1. Edges are taken straight from SO->EL, selecting first occurrences via
        ELps[i][2] >= 1 (see SUMA_define.h, "3rd column ... positive for the
        first occurrence of the edge in EL, -1 afterwards").  SUMA instead
        walks FN->FirstNeighb and binary-searches SUMA_FIND_EDGE per
        neighbour, keeping a "visited" byte array to avoid double counting.
        Same set of edges, ~2*log(N_EL) less work per pass, and the sign of
        the difference becomes deterministic (EL[i][0] <= EL[i][1]) instead of
        depending on node traversal order.  Only dfdssum is sign-sensitive,
        and it enters solely as a mean correction that is ~0 for the noise
        this program generates.

     2. SUMA emits a "distribution is possibly random noise" notice whenever
        prob > 0.01, and computes the F-test unconditionally.  Here the F-test
        is evaluated only in the branch that consumes it, and nothing is
        printed -- during the adaptive search the fields ARE random noise, so
        SUMA's version would emit one notice per field per iteration.

   The three-way outcome is identical to SUMA's:
        arg in (0,1) -> FWHM;  arg >= 1 -> -1;  arg <= 0 -> 0.0 if prob > 0.01
        else -1.  The counts < 36 -> varss = 0 rule is preserved.
   ------------------------------------------------------------------------ */
static float sscs_estimate_fwhm_quiet(SUMA_SurfaceObject *SO,
                                      const float *field, const byte *mask)
{
   double sum = 0.0, sumsq = 0.0, var;
   double diffsum = 0.0, diffsq = 0.0, vardiff, dist = 0.0, arg;
   double ratio, prob;
   float par[2];
   int node, edge, first, second, nvalue = 0, nedge = 0;

   if (!SO || !SO->EL || !field) return -1.0f;
   for (node = 0; node < SO->N_Node; ++node) if (!mask || mask[node]) {
      double value = field[node];
      sum += value; sumsq += value * value; ++nvalue;
   }
   if (nvalue < 9 || sumsq <= 0.0) return -1.0f;
   var = (sumsq - sum * sum / (double)nvalue) / (double)(nvalue - 1);
   if (var <= 0.0) return -1.0f;

   for (edge = 0; edge < SO->EL->N_EL; ++edge) {
      double difference;
      if (SO->EL->ELps[edge][2] < 1) continue;
      first = SO->EL->EL[edge][0]; second = SO->EL->EL[edge][1];
      if (mask && (!mask[first] || !mask[second])) continue;
      difference = (double)field[second] - field[first];
      diffsum += difference; diffsq += difference * difference;
      dist += SO->EL->Le[edge]; ++nedge;
   }
   if (nedge <= 0) return -1.0f;
   vardiff = nedge < 36 ? 0.0 :
      (diffsq - diffsum * diffsum / (double)nedge) / (double)(nedge - 1);
   arg = 1.0 - 0.5 * vardiff / var;
   if (arg > 0.0 && arg < 1.0)
      return (float)(2.35482 * sqrt(-1.0 / (4.0 * log(arg))) *
                     (dist / (double)nedge));
   if (arg > 0.0 || vardiff <= 0.0) return -1.0f;

   ratio = MAX(vardiff / (2.0 * var), (2.0 * var) / vardiff);
   par[0] = par[1] = (float)SO->N_Node;
#ifdef USE_OMP
#pragma omp critical(sscs_fwhm_pval)
#endif
   prob = THD_stat_to_pval((float)ratio, NI_STAT_FTEST, par);
   return prob > 0.01 ? 0.0f : -1.0f;
}

int SUMA_SurfClustSim_SmoothFixed(SUMA_SurfaceObject *SO, double **wgt,
                                  int niter, float *field, const byte *mask,
                                  SUMA_SURFCLUSTSIM_WORK *work)
{
   double *in, *out, *tmp;
   int node, iter;
   if (!SO || !wgt || !field || !work || niter < 0) return 0;
   for (node = 0; node < SO->N_Node; ++node)
      work->work_a[node] = (double)field[node];
   in = work->work_a;
   out = work->work_b;
   for (iter = 0; iter < niter; ++iter) {
      sscs_smooth_double_once(SO, wgt, in, out, mask);
      tmp = in; in = out; out = tmp;
   }
   for (node = 0; node < SO->N_Node; ++node) field[node] = (float)in[node];
   return 1;
}

static int sscs_detrend_master(float **field, int nfield, int nnode,
                               const byte *mask)
{
   float **ref = NULL, *series = NULL;
   int order, nref, node, iter, rr;
   double mean, sumsq, scale;

   if (nfield < 3) return 1;
   order = nfield / 30;
   if (order < 1) {
      nref = 1;
      ref = THD_build_polyref(nref, nfield);
   } else {
      nref = 2 * order + 3;
      ref = THD_build_trigref(order, nfield);
   }
   if (!ref) return 0;
   series = (float *)malloc((size_t)nfield * sizeof(float));
   if (!series) goto fail;

   for (node = 0; node < nnode; ++node) {
      if (mask && !mask[node]) continue;
      for (iter = 0; iter < nfield; ++iter) series[iter] = field[iter][node];
      THD_generic_detrend_LSQ(nfield, series, -1, nref, ref, NULL);
      mean = 0.0;
      for (iter = 0; iter < nfield; ++iter) mean += series[iter];
      mean /= (double)nfield;
      sumsq = 0.0;
      for (iter = 0; iter < nfield; ++iter)
         sumsq += ((double)series[iter] - mean) * ((double)series[iter] - mean);
      scale = sumsq > 0.0 ? sqrt(sumsq / (double)(nfield - 1)) : 1.0;
      for (iter = 0; iter < nfield; ++iter)
         field[iter][node] = (float)((double)series[iter] / scale);
   }

   free(series);
   for (rr = 0; rr < nref; ++rr) free(ref[rr]);
   free(ref);
   return 1;

fail:
   for (rr = 0; rr < nref; ++rr) free(ref[rr]);
   free(ref);
   free(series);
   return 0;
}

int SUMA_SurfClustSim_ChooseCompatNiter(
   SUMA_SurfaceObject *SO, double **wgt, float **field, int nfield,
   const byte *mask, double target_fwhm, int max_iter, float *final_fwhm,
   SUMA_SURFCLUSTSIM_PROGRESS_FN progress, void *progress_data)
{
   float **master = NULL, **buffer = NULL, *fwhm = NULL;
   double mean_fwhm;
   int iter = 0, col, nvalid, stop = 0, result = -1;

   if (!SO || !wgt || !field || nfield <= 0 || target_fwhm <= 0.0)
      return -1;
   master = (float **)calloc((size_t)nfield, sizeof(float *));
   buffer = (float **)calloc((size_t)nfield, sizeof(float *));
   fwhm = (float *)malloc((size_t)nfield * sizeof(float));
   if (!master || !buffer || !fwhm) goto fail;
   for (col = 0; col < nfield; ++col) {
      master[col] = (float *)malloc((size_t)SO->N_Node * sizeof(float));
      buffer[col] = (float *)malloc((size_t)SO->N_Node * sizeof(float));
      if (!master[col] || !buffer[col]) goto fail;
      memcpy(master[col], field[col], (size_t)SO->N_Node * sizeof(float));
   }
   if (!sscs_detrend_master(master, nfield, SO->N_Node, mask)) goto fail;

   /* Serial on purpose.  SurfClustSim now parallelizes across blocks, and
      this runs inside one of those blocks, so adding a second level here
      would nest.  Coarser-grained parallelism also balances better: -compat
      blocks need differing numbers of adaptive iterations, which an inner
      loop over columns cannot exploit. */
   for (;;) {
      for (col = 0; col < nfield; ++col)
         fwhm[col] = sscs_estimate_fwhm_quiet(SO, master[col], mask);

      mean_fwhm = 0.0;
      nvalid = 0;
      for (col = 0; col < nfield; ++col) {
         float value = fwhm[col];
         if (value >= 0.0f && isfinite(value)) {
            mean_fwhm += value;
            ++nvalid;
         }
      }
      if (nvalid == 0) {
         stop = 1;
      } else {
         mean_fwhm /= (double)nvalid;
         if (final_fwhm) *final_fwhm = (float)mean_fwhm;
         if (mean_fwhm > target_fwhm || iter == max_iter) {
            result = iter;
            stop = 1;
         }
      }

      if (stop) break;

      for (col = 0; col < nfield; ++col) {
         sscs_smooth_float_once(SO, wgt, master[col], buffer[col], mask);
         memcpy(master[col], buffer[col],
                (size_t)SO->N_Node * sizeof(float));
      }

      ++iter;
      if (progress) progress(iter, max_iter, progress_data);
   }

fail:
   if (master) for (col = 0; col < nfield; ++col) free(master[col]);
   if (buffer) for (col = 0; col < nfield; ++col) free(buffer[col]);
   free(master);
   free(buffer);
   free(fwhm);
   return result;
}

int SUMA_SurfClustSim_Rescale(float *field, int nnode, const byte *mask,
                              double *stdev_out)
{
   double mean = 0.0, sumsq = 0.0, stdev;
   int node, count = 0;
   if (!field || nnode <= 0) return 0;
   for (node = 0; node < nnode; ++node)
      if (!mask || mask[node]) { mean += field[node]; ++count; }
   if (count < 2) return 0;
   mean /= (double)count;
   for (node = 0; node < nnode; ++node)
      if (!mask || mask[node])
         sumsq += ((double)field[node] - mean) * ((double)field[node] - mean);
   stdev = sqrt(sumsq / (double)(count - 1));
   if (!(stdev > DBL_EPSILON) || !isfinite(stdev)) return 0;
   for (node = 0; node < nnode; ++node) field[node] = (float)(field[node] / stdev);
   if (stdev_out) *stdev_out = stdev;
   return 1;
}

/* ======================================================================
   ACF-matched noise generation.  See the SUMA_SURFCLUSTSIM_ACF comment in
   the header for the linearity property this all rests on.
   ====================================================================== */

/* Least-squares state for the weight fit.  Only touched during calibration,
   which is single-threaded startup work, so file scope is safe here (the
   same arrangement ACF_modelE_costfunc uses in mri_fwhm.c). */
static int      sscs_fit_nbasis = 0;
static int      sscs_fit_nbins  = 0;
static double **sscs_fit_basis  = NULL;   /* [nbasis][nbins] measured ACFs */
static double  *sscs_fit_target = NULL;   /* [nbins] target ACF */
static byte    *sscs_fit_use    = NULL;   /* [nbins] bin is usable */

/* Map unconstrained parameters to weights on the simplex: w_k = u_k^2/sum.
   Squaring enforces non-negativity and the normalization enforces the sum,
   so the optimizer can range freely and every point it visits is valid. */
static void sscs_fit_weights(const double *par, double *w, int nbasis)
{
   double total = 0.0;
   int k;
   for (k = 0; k < nbasis; ++k) { w[k] = par[k]*par[k]; total += w[k]; }
   if (total <= 0.0) { for (k = 0; k < nbasis; ++k) w[k] = 1.0/nbasis; return; }
   for (k = 0; k < nbasis; ++k) w[k] /= total;
}

static double sscs_acf_costfunc(int npar, double *par)
{
   double w[64], sum = 0.0, model, diff;
   int ib, k;
   if (npar > 64) return 1.e30;
   sscs_fit_weights(par, w, npar);
   for (ib = 0; ib < sscs_fit_nbins; ++ib) {
      if (!sscs_fit_use[ib]) continue;
      model = 0.0;
      for (k = 0; k < npar; ++k) model += w[k] * sscs_fit_basis[k][ib];
      diff = model - sscs_fit_target[ib];
      sum += diff * diff;
   }
   return sum;
}

void SUMA_SurfClustSim_ACF_Free(SUMA_SURFCLUSTSIM_ACF *acf)
{
   if (!acf) return;
   free(acf->niter); free(acf->weight); free(acf->scale);
   free(acf);
}

int SUMA_SurfClustSim_ACF_Measure(
   SUMA_SurfaceObject *SO, const float *field, const byte *mask,
   float radius, float dr, double *a, double *b, double *c, double *fwhm)
{
   SUMA_GET_OFFSET_STRUCT *off = NULL;
   double *curve = NULL, *nacc = NULL;
   float_quad q;
   int nbins, ok = 0;

   SUMA_SurfACF_defaults(SO, &radius, &dr);
   nbins = SUMA_SurfACF_nbins(radius, dr);
   if (nbins < 5) return 0;
   curve = (double *)calloc(nbins, sizeof(double));
   nacc  = (double *)calloc(nbins, sizeof(double));
   off   = SUMA_Initialize_getoffsets(SO->N_Node);
   if (!curve || !nacc || !off) goto done;
   if (!SUMA_SurfACF_accumulate(SO, field, mask, radius, dr, nbins,
                                curve, nacc, off)) goto done;
   SUMA_SurfACF_finalize(curve, nacc, nbins);
   q = SUMA_SurfACF_fit(curve, nbins, dr);
   if (q.a < 0.0f) goto done;
   if (a)    *a    = q.a;
   if (b)    *b    = q.b;
   if (c)    *c    = q.c;
   if (fwhm) *fwhm = q.d;
   ok = 1;
done:
   free(curve); free(nacc);
   if (off) SUMA_Free_getoffsets(off);
   return ok;
}

/* Build one basis field: white noise smoothed `passes` times. */
static int sscs_acf_basis_field(SUMA_SurfaceObject *SO, double **wgt,
                                float *field, const byte *mask, int passes,
                                unsigned long long seed, int index,
                                SUMA_SURFCLUSTSIM_WORK *work)
{
   SUMA_SurfClustSim_FillNoise(field, SO->N_Node, mask, seed, index);
   return SUMA_SurfClustSim_SmoothFixed(SO, wgt, passes, field, mask, work);
}

static double sscs_field_sd(const float *field, int nnode, const byte *mask)
{
   double mean = 0.0, sumsq = 0.0;
   int n, count = 0;
   for (n = 0; n < nnode; ++n) if (!mask || mask[n]) { mean += field[n]; ++count; }
   if (count < 2) return 0.0;
   mean /= (double)count;
   for (n = 0; n < nnode; ++n) if (!mask || mask[n])
      sumsq += ((double)field[n]-mean)*((double)field[n]-mean);
   return sqrt(sumsq/(double)(count-1));
}

SUMA_SURFCLUSTSIM_ACF *SUMA_SurfClustSim_ACF_Calibrate(
   SUMA_SurfaceObject *SO, double **wgt, const byte *mask,
   double a, double b, double c, int nbasis, int niter_ref,
   float radius, float dr, unsigned long long seed, int verb)
{
   SUMA_SURFCLUSTSIM_ACF *acf = NULL;
   SUMA_SURFCLUSTSIM_WORK *work = NULL;
   SUMA_GET_OFFSET_STRUCT *off = NULL;
   double *nacc = NULL, *par = NULL, *w = NULL;
   float *field = NULL;
   int nbins, ib, k, ok = 0, stride;
   double ratio = 0.0, lo = 0.0;

   if (!SO || !wgt || nbasis < 2 || nbasis > 64 || niter_ref < 1) return NULL;
   SUMA_SurfACF_defaults(SO, &radius, &dr);
   /* The default radius is 20 edge steps, which suits MEASURING a field but
      is far too short for FITTING an exponential tail: at 20 steps a c=36
      component is still at 0.165, so the fit would never see the shape it is
      being asked to reproduce.  Reach out to where the tail has actually
      decayed. */
   if (radius < 2.0f * (float)c) radius = 2.0f * (float)c;
   /* Cost grows with radius^2 and the neighbourhood walk dominates startup,
      so cap it: past a couple of decay lengths the tail contributes little
      to the fit but a great deal to the runtime. */
   if (radius > 45.0f * SO->EL->AvgLe) radius = 45.0f * SO->EL->AvgLe;
   nbins = SUMA_SurfACF_nbins(radius, dr);
   if (nbins < 5) return NULL;

   /* ~4000 centre nodes is plenty for a curve that averages over every pair
      inside each neighbourhood; visiting all of them would dominate startup. */
   stride = SO->N_Node / 1500; if (stride < 1) stride = 1;

   acf = (SUMA_SURFCLUSTSIM_ACF *)calloc(1, sizeof(*acf));
   if (!acf) return NULL;
   acf->nbasis = nbasis;
   acf->niter  = (int    *)calloc(nbasis, sizeof(int));
   acf->weight = (double *)calloc(nbasis, sizeof(double));
   acf->scale  = (double *)calloc(nbasis, sizeof(double));
   acf->target_a = a; acf->target_b = b; acf->target_c = c;
   if (!acf->niter || !acf->weight || !acf->scale) goto done;

   sscs_fit_nbasis = nbasis;
   sscs_fit_nbins  = nbins;
   sscs_fit_basis  = (double **)calloc(nbasis, sizeof(double *));
   sscs_fit_target = (double  *)calloc(nbins,  sizeof(double));
   sscs_fit_use    = (byte    *)calloc(nbins,  sizeof(byte));
   nacc  = (double *)calloc(nbins, sizeof(double));
   par   = (double *)calloc(nbasis, sizeof(double));
   w     = (double *)calloc(nbasis, sizeof(double));
   field = (float  *)malloc((size_t)SO->N_Node * sizeof(float));
   work  = SUMA_SurfClustSim_NewWork(SO->N_Node);
   off   = SUMA_Initialize_getoffsets(SO->N_Node);
   if (!sscs_fit_basis || !sscs_fit_target || !sscs_fit_use || !nacc ||
       !par || !w || !field || !work || !off) goto done;
   for (k = 0; k < nbasis; ++k) {
      sscs_fit_basis[k] = (double *)calloc(nbins, sizeof(double));
      if (!sscs_fit_basis[k]) goto done;
   }

   for (ib = 0; ib < nbins; ++ib)
      sscs_fit_target[ib] = SUMA_SurfACF_model(a, b, c, (double)ib*dr);

   /* Choose the ladder EMPIRICALLY rather than from a formula.

      SUMA_SigForFWHM() offers a pass count, but its own message calls it a
      "wild guess" and it overshoots badly -- it suggested 568 passes where
      about 46 were wanted, which put the whole ladder past the target and
      collapsed the fit onto its narrowest member.  So instead: smooth one
      white field one pass at a time, measure its width after each pass with
      the cheap 1-difference estimator, and record the pass count as the
      width crosses each rung.

      Rungs are set in field-FWHM units.  Smoothing white noise with a kernel
      of width W yields an autocorrelation about sqrt(2)*W wide, so to place
      basis ACFs around the target's effective width d, the field widths want
      to run from roughly 0.35 to 1.8 times d/sqrt(2).  One progressive sweep
      costs n_max passes total and yields every rung. */
   {
      double dtarget = 0.0, prev = 1.0, r, step = 0.05 * SO->EL->AvgLe;
      double *rung = NULL;
      float *probe = NULL;
      int pass, k2, found;

      for (r = step; r < 500.0 * SO->EL->AvgLe; r += step) {
         double v = SUMA_SurfACF_model(a, b, c, r);
         if (v <= 0.5) { dtarget = 2.0 * ((prev > v)
               ? r - step + step*(prev-0.5)/(prev-v) : r); break; }
         prev = v;
      }
      if (dtarget <= 0.0) goto done;

      rung  = (double *)calloc(nbasis, sizeof(double));
      probe = (float  *)malloc((size_t)SO->N_Node * sizeof(float));
      if (!rung || !probe) { free(rung); free(probe); goto done; }
      /* Span the model's OWN two scales rather than a fixed multiple of its
         combined width.  The Gaussian component has ACF FWHM 2.355*b and the
         exponential has 2*c*ln2; dividing by sqrt(2) converts each to the
         field width that produces it.  Running from half the narrow scale to
         1.5x the broad one gives the fit basis reaching both ends, which a
         span keyed to the combined width does not when b and c are far
         apart. */
      {
         double lo_f = 0.5 * (2.355*b) / sqrt(2.0);
         double hi_f = 1.5 * (2.0*c*log(2.0)) / sqrt(2.0);
         if (hi_f <= lo_f * 1.5) hi_f = lo_f * 4.0;
         for (k = 0; k < nbasis; ++k)
            rung[k] = lo_f * pow(hi_f/lo_f, (double)k/(double)(nbasis-1));
      }
      (void)dtarget;

      SUMA_SurfClustSim_FillNoise(probe, SO->N_Node, mask,
                                  seed ^ UINT64_C(0x9e3779b9), 0);
      for (k = 0; k < nbasis; ++k) acf->niter[k] = 0;
      found = 0;
      for (pass = 1; pass <= 20000 && found < nbasis; ++pass) {
         float fw;
         if (!SUMA_SurfClustSim_SmoothFixed(SO, wgt, 1, probe, mask, work)) {
            free(rung); free(probe); goto done;
         }
         fw = sscs_estimate_fwhm_quiet(SO, probe, mask);
         if (!(fw > 0.0f)) continue;
         for (k2 = found; k2 < nbasis && (double)fw >= rung[k2]; ++k2)
            acf->niter[k2] = pass;
         if (k2 > found) found = k2;
      }
      free(rung); free(probe);
      if (found < nbasis) {
         /* never reached the widest rung; fall back to extending the ladder */
         for (k = found; k < nbasis; ++k)
            acf->niter[k] = (acf->niter[k-1] > 0 ? acf->niter[k-1] : 1) * 2;
      }
      for (k = 0; k < nbasis; ++k) {
         if (acf->niter[k] < 1) acf->niter[k] = 1;
         if (k > 0 && acf->niter[k] <= acf->niter[k-1])
            acf->niter[k] = acf->niter[k-1] + 1;  /* keep the bank distinct */
      }
      acf->total_passes = 0;
      for (k = 0; k < nbasis; ++k) acf->total_passes += acf->niter[k];
      (void)lo; (void)ratio; (void)niter_ref;
   }


   /* Measure each basis field's ACF, and its standard deviation so the
      mixture can be assembled from unit-variance components. */
   /* Average each basis curve over several realizations.  One is not enough:
      the broadest members retain few independent modes, so a single draw
      gives a visibly noisy curve, and the weight fit inherits that noise. */
#define SSCS_ACF_NCAL 4
   for (k = 0; k < nbasis; ++k) {
      int rep;
      double sdsum = 0.0;
      memset(nacc, 0, (size_t)nbins*sizeof(double));
      memset(sscs_fit_basis[k], 0, (size_t)nbins*sizeof(double));
      for (rep = 0; rep < SSCS_ACF_NCAL; ++rep) {
         if (!sscs_acf_basis_field(SO, wgt, field, mask, acf->niter[k],
                                   seed ^ UINT64_C(0x5bf03635),
                                   rep * nbasis + k, work)) goto done;
         sdsum += sscs_field_sd(field, SO->N_Node, mask);
         if (!SUMA_SurfACF_accumulate_str(SO, field, mask, radius, dr, nbins,
                                          sscs_fit_basis[k], nacc, off,
                                          stride)) goto done;
      }
      if (!(sdsum > 0.0)) goto done;
      acf->scale[k] = (double)SSCS_ACF_NCAL / sdsum;   /* reporting only */
      SUMA_SurfACF_finalize(sscs_fit_basis[k], nacc, nbins);
      if (verb > 1)
         INFO_message("  ACF basis %d: %d smoothing passes", k, acf->niter[k]);
   }

   /* A bin is usable only where every basis has a measured value. */
   for (ib = 0; ib < nbins; ++ib) {
      sscs_fit_use[ib] = 1;
      for (k = 0; k < nbasis; ++k)
         if (sscs_fit_basis[k][ib] < -0.5) sscs_fit_use[ib] = 0;
   }

   for (k = 0; k < nbasis; ++k) par[k] = 1.0;   /* start at equal weights */
   if (powell_newuoa(nbasis, par, 0.3, 0.0001, 9999, sscs_acf_costfunc) < 0)
      goto done;
   sscs_fit_weights(par, acf->weight, nbasis);

   {  /* residual, and the effective FWHM the fitted mixture actually gives */
      double sum = 0.0, model; int used = 0;
      double *fitted = (double *)calloc(nbins, sizeof(double));
      if (fitted) {
         for (ib = 0; ib < nbins; ++ib) {
            if (!sscs_fit_use[ib]) { fitted[ib] = -1.0; continue; }
            model = 0.0;
            for (k = 0; k < nbasis; ++k)
               model += acf->weight[k] * sscs_fit_basis[k][ib];
            fitted[ib] = model;
            sum += (model - sscs_fit_target[ib])*(model - sscs_fit_target[ib]);
            ++used;
         }
         acf->fit_rms = used ? sqrt(sum/(double)used) : -1.0;
         {  float_quad q = SUMA_SurfACF_fit(fitted, nbins, dr);
            acf->achieved_fwhm = (q.a >= 0.0f) ? q.d : -1.0; }
         free(fitted);
      }
   }

   /* Acceptance test: build fields exactly the way every simulation will
      build them, measure their autocorrelation, and fit the SAME model.
      Matching the basis curves in least squares is not the claim being made
      -- the claim is that the generated noise carries the requested ACF --
      so verify that directly rather than trusting the fit residual.

      Averaged over the same number of realizations the basis curves used:
      comparing a single draw against a 4-draw average would charge the
      generator for estimation noise it did not cause. */
   acf->measured_a = acf->measured_b = acf->measured_c = -1.0;
   acf->measured_fwhm = -1.0;
   {
      float  *check = (float  *)malloc((size_t)SO->N_Node * sizeof(float));
      double *cc    = (double *)calloc(nbins, sizeof(double));
      double *cn    = (double *)calloc(nbins, sizeof(double));
      int rep, nok = 0;

      if (check && cc && cn) {
         for (rep = 0; rep < SSCS_ACF_NCAL; ++rep) {
            if (!SUMA_SurfClustSim_ACF_Fill(SO, wgt, acf, check, mask,
                                            seed ^ UINT64_C(0x2545f491),
                                            rep, work)) break;
            if (SUMA_SurfACF_accumulate_str(SO, check, mask, radius, dr,
                                            nbins, cc, cn, off, stride)) ++nok;
         }
         if (nok > 0) {
            float_quad q;
            SUMA_SurfACF_finalize(cc, cn, nbins);
            q = SUMA_SurfACF_fit(cc, nbins, dr);
            if (q.a >= 0.0f) {
               acf->measured_a = q.a; acf->measured_b = q.b;
               acf->measured_c = q.c; acf->measured_fwhm = q.d;
            }
            if (verb > 1) {   /* diagnostic: are we fitting what we build? */
               FILE *dbg = fopen("SurfClustSim.acfdebug.1D", "w");
               if (dbg) {
                  int kk; double pred;
                  fprintf(dbg, "# generated sd = %.6g (want 1)\n",
                          sscs_field_sd(check, SO->N_Node, mask));
                  for (kk = 0; kk < nbasis; ++kk)
                     fprintf(dbg, "# basis %d: passes %d weight %.5f\n",
                             kk, acf->niter[kk], acf->weight[kk]);
                  fprintf(dbg, "# r target predicted generated usable\n");
                  for (ib = 0; ib < nbins; ++ib) {
                     pred = 0.0;
                     for (kk = 0; kk < nbasis; ++kk)
                        pred += acf->weight[kk] * sscs_fit_basis[kk][ib];
                     fprintf(dbg, " %8.3f %9.5f %9.5f %9.5f %d\n",
                             ib*dr, sscs_fit_target[ib], pred, cc[ib],
                             (int)sscs_fit_use[ib]);
                  }
                  fclose(dbg);
               }
            }
         }
      }
      free(check); free(cc); free(cn);
   }

   ok = 1;

done:
   if (sscs_fit_basis) {
      for (k = 0; k < nbasis; ++k) free(sscs_fit_basis[k]);
      free(sscs_fit_basis); sscs_fit_basis = NULL;
   }
   free(sscs_fit_target); sscs_fit_target = NULL;
   free(sscs_fit_use);    sscs_fit_use    = NULL;
   sscs_fit_nbasis = sscs_fit_nbins = 0;
   free(nacc); free(par); free(w); free(field);
   if (work) SUMA_SurfClustSim_FreeWork(work);
   if (off)  SUMA_Free_getoffsets(off);
   if (!ok) { SUMA_SurfClustSim_ACF_Free(acf); acf = NULL; }
   return acf;
}

int SUMA_SurfClustSim_ACF_Fill(
   SUMA_SurfaceObject *SO, double **wgt, const SUMA_SURFCLUSTSIM_ACF *acf,
   float *field, const byte *mask, unsigned long long seed, int iteration,
   SUMA_SURFCLUSTSIM_WORK *work)
{
   float *component = NULL;
   int k, n, ok = 0;

   if (!SO || !wgt || !acf || !field || !work) return 0;
   component = (float *)malloc((size_t)SO->N_Node * sizeof(float));
   if (!component) return 0;

   memset(field, 0, (size_t)SO->N_Node * sizeof(float));
   for (k = 0; k < acf->nbasis; ++k) {
      double gain;
      if (acf->weight[k] <= 0.0) continue;
      /* Each basis needs its OWN independent white field -- that
         independence is what makes the mixture's ACF the weighted sum of the
         component ACFs.  Offsetting the stream by basis index keeps them
         independent while staying a pure function of the simulation index,
         so results remain reproducible across threads and block sizes. */
      double sd;
      if (!sscs_acf_basis_field(SO, wgt, component, mask, acf->niter[k],
                                seed, iteration * acf->nbasis + k, work))
         goto done;
      /* Normalize by THIS realization's standard deviation, not by the one
         measured at calibration.  A heavily smoothed field retains only the
         lowest spatial modes, so its variance is set by a handful of random
         coefficients and swings widely from realization to realization.
         Using a fixed scale then lets the broad components contribute the
         wrong share, and the mixture comes out narrower than requested. */
      sd = sscs_field_sd(component, SO->N_Node, mask);
      if (!(sd > 0.0)) goto done;
      gain = sqrt(acf->weight[k]) / sd;
      for (n = 0; n < SO->N_Node; ++n)
         field[n] += (float)(gain * (double)component[n]);
   }
   ok = 1;
done:
   free(component);
   return ok;
}

static int sscs_is_active(float value, double threshold, int sign_mode)
{
   if (sign_mode > 0) return value > threshold;
   if (sign_mode < 0) return value < -threshold;
   return value > threshold || value < -threshold;
}

double SUMA_SurfClustSim_MaxArea(
   const SUMA_SURFCLUSTSIM_GRAPH *graph, const float *node_area,
   const float *field, const byte *mask, double threshold, int sign_mode,
   SUMA_SURFCLUSTSIM_WORK *work)
{
   double max_area = 0.0;
   int node, head, tail, edge, other;
   if (!graph || !node_area || !field || !work || threshold < 0.0) return -1.0;
   for (node = 0; node < graph->nnode; ++node)
      work->active[node] = (!mask || mask[node]) &&
                           sscs_is_active(field[node], threshold, sign_mode);

   for (node = 0; node < graph->nnode; ++node) {
      double area = 0.0;
      if (!work->active[node]) continue;
      head = tail = 0;
      work->queue[tail++] = node;
      work->active[node] = 0;
      while (head < tail) {
         int current = work->queue[head++];
         area += node_area[current];
         for (edge = graph->offset[current]; edge < graph->offset[current + 1]; ++edge) {
            other = graph->neighbor[edge];
            if (work->active[other]) {
               work->active[other] = 0;
               work->queue[tail++] = other;
            }
         }
      }
      if (area > max_area) max_area = area;
   }
   return max_area;
}

static int sscs_node_key_desc(const void *aa, const void *bb)
{
   const SUMA_SURFCLUSTSIM_NODE_KEY *a =
      (const SUMA_SURFCLUSTSIM_NODE_KEY *)aa;
   const SUMA_SURFCLUSTSIM_NODE_KEY *b =
      (const SUMA_SURFCLUSTSIM_NODE_KEY *)bb;
   if (a->key > b->key) return -1;
   if (a->key < b->key) return 1;
   return a->node < b->node ? -1 : a->node > b->node ? 1 : 0;
}

static int sscs_union_find_root(int *parent, int node)
{
   int root = node, next;
   while (parent[root] != root) root = parent[root];
   while (parent[node] != node) {
      next = parent[node];
      parent[node] = root;
      node = next;
   }
   return root;
}

static double sscs_union_components(int *parent, double *component_area,
                                    int first, int second)
{
   int root_a = sscs_union_find_root(parent, first);
   int root_b = sscs_union_find_root(parent, second);
   int swap;
   if (root_a == root_b) return component_area[root_a];
   if (root_a > root_b) {
      swap = root_a; root_a = root_b; root_b = swap;
   }
   parent[root_b] = root_a;
   component_area[root_a] += component_area[root_b];
   return component_area[root_a];
}

static int sscs_thresholds_valid(const double *threshold, int count,
                                 const double *output)
{
   int index;
   if (count == 0) return 1;
   if (count < 0 || !threshold || !output) return 0;
   for (index = 0; index < count; ++index)
      if (!isfinite(threshold[index]) || threshold[index] < 0.0 ||
          (index > 0 && threshold[index] < threshold[index - 1]))
         return 0;
   return 1;
}

/* Activate nodes in descending statistic order and maintain connected
   components with union-find.  Each threshold array must be nondecreasing.
   Two arrays allow related sidedness modes to share one node sort and sweep. */
int SUMA_SurfClustSim_MaxAreasSweep(
   const SUMA_SURFCLUSTSIM_GRAPH *graph, const float *node_area,
   const float *field, const byte *mask, int sign_mode,
   const double *threshold_a, int nthreshold_a, double *max_area_a,
   const double *threshold_b, int nthreshold_b, double *max_area_b,
   SUMA_SURFCLUSTSIM_WORK *work)
{
   double minimum_threshold = DBL_MAX, threshold, max_area = 0.0;
   int node, other, edge, norder = 0, order_index = 0;
   int index_a = nthreshold_a - 1, index_b = nthreshold_b - 1;

   if (!graph || !node_area || !field || !work ||
       (sign_mode < -1 || sign_mode > 1) ||
       !sscs_thresholds_valid(threshold_a, nthreshold_a, max_area_a) ||
       !sscs_thresholds_valid(threshold_b, nthreshold_b, max_area_b))
      return 0;
   if (nthreshold_a == 0 && nthreshold_b == 0) return 1;
   if (nthreshold_a > 0) minimum_threshold = threshold_a[0];
   if (nthreshold_b > 0)
      minimum_threshold = MIN(minimum_threshold, threshold_b[0]);

   memset(work->active, 0, (size_t)graph->nnode * sizeof(byte));
   for (node = 0; node < graph->nnode; ++node) {
      float value, key;
      if (mask && !mask[node]) continue;
      value = field[node];
      if (!isfinite(value)) continue;
      key = sign_mode > 0 ? value : sign_mode < 0 ? -value : fabsf(value);
      if ((double)key <= minimum_threshold) continue;
      work->order[norder].key = key;
      work->order[norder].node = node;
      ++norder;
   }
   qsort(work->order, (size_t)norder, sizeof(*work->order), sscs_node_key_desc);

   while (index_a >= 0 || index_b >= 0) {
      if (index_b < 0 ||
          (index_a >= 0 && threshold_a[index_a] >= threshold_b[index_b]))
         threshold = threshold_a[index_a];
      else
         threshold = threshold_b[index_b];

      while (order_index < norder &&
             (double)work->order[order_index].key > threshold) {
         double area;
         node = work->order[order_index++].node;
         work->active[node] = 1;
         work->queue[node] = node;
         work->work_a[node] = node_area[node];
         area = work->work_a[node];
         if (area > max_area) max_area = area;
         for (edge = graph->offset[node]; edge < graph->offset[node + 1]; ++edge) {
            other = graph->neighbor[edge];
            if (!work->active[other]) continue;
            area = sscs_union_components(work->queue, work->work_a, node, other);
            if (area > max_area) max_area = area;
         }
      }

      while (index_a >= 0 && threshold_a[index_a] == threshold)
         max_area_a[index_a--] = max_area;
      while (index_b >= 0 && threshold_b[index_b] == threshold)
         max_area_b[index_b--] = max_area;
   }
   return 1;
}
