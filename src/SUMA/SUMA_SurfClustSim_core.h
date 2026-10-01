#ifndef SUMA_SURFCLUSTSIM_CORE_H
#define SUMA_SURFCLUSTSIM_CORE_H

#include "SUMA_suma.h"

typedef struct {
   int nnode;
   int nedge;
   int *offset;
   int *neighbor;
} SUMA_SURFCLUSTSIM_GRAPH;

typedef struct {
   float key;
   int node;
} SUMA_SURFCLUSTSIM_NODE_KEY;

typedef struct {
   byte *active;
   int *queue;
   SUMA_SURFCLUSTSIM_NODE_KEY *order;
   double *work_a;
   double *work_b;
} SUMA_SURFCLUSTSIM_WORK;

typedef void (*SUMA_SURFCLUSTSIM_PROGRESS_FN)(int completed, int total,
                                              void *userdata);

/* Noise generator matched to a requested autocorrelation function.

   A heat kernel produces an approximately GAUSSIAN autocorrelation, but the
   model that describes real data is Gaussian PLUS an exponential tail, and no
   single heat kernel yields an exponential component at all.  So rather than
   trying to synthesize one kernel, build a bank of independent fields
   smoothed by differing numbers of passes and take a weighted combination.

   The key property that makes this work: if the basis fields are independent
   and each is scaled to unit variance, then for weights summing to 1,

       field = sum_k sqrt(w_k) * B_k   =>   ACF_field = sum_k w_k * ACF_k

   exactly.  The ACF of the mixture is LINEAR in the weights, so matching a
   target curve is an ordinary constrained least-squares fit, and the weights
   can be solved for once at startup and reused for every simulation. */
typedef struct {
   int nbasis;
   int *niter;         /* smoothing passes for each basis field */
   double *weight;     /* w_k, non-negative, summing to 1 */
   double *scale;      /* 1/sd_k, measured at calibration time */
   double target_a, target_b, target_c;
   double fit_rms;     /* RMS mismatch between fitted and target ACF */
   double achieved_fwhm;
   /* What a field actually generated from this mixture measures back as --
      the acceptance test for the whole scheme.  -1 if not verified. */
   double measured_a, measured_b, measured_c, measured_fwhm;
   int total_passes;   /* sum of niter: the per-simulation smoothing cost */
} SUMA_SURFCLUSTSIM_ACF;

SUMA_SURFCLUSTSIM_ACF *SUMA_SurfClustSim_ACF_Calibrate(
   SUMA_SurfaceObject *SO, double **wgt, const byte *mask,
   double a, double b, double c, int nbasis, int niter_ref,
   float radius, float dr, unsigned long long seed, int verb);
void SUMA_SurfClustSim_ACF_Free(SUMA_SURFCLUSTSIM_ACF *acf);

int SUMA_SurfClustSim_ACF_Fill(
   SUMA_SurfaceObject *SO, double **wgt, const SUMA_SURFCLUSTSIM_ACF *acf,
   float *field, const byte *mask, unsigned long long seed, int iteration,
   SUMA_SURFCLUSTSIM_WORK *work);

int SUMA_SurfClustSim_ACF_Measure(
   SUMA_SurfaceObject *SO, const float *field, const byte *mask,
   float radius, float dr, double *a, double *b, double *c, double *fwhm);

SUMA_SURFCLUSTSIM_GRAPH *SUMA_SurfClustSim_MakeGraph(
   SUMA_SurfaceObject *SO, float rmm);
void SUMA_SurfClustSim_FreeGraph(SUMA_SURFCLUSTSIM_GRAPH *graph);

SUMA_SURFCLUSTSIM_WORK *SUMA_SurfClustSim_NewWork(int nnode);
void SUMA_SurfClustSim_FreeWork(SUMA_SURFCLUSTSIM_WORK *work);

void SUMA_SurfClustSim_FillNoise(float *field, int nnode, const byte *mask,
                                 unsigned long long seed, int iteration);

int SUMA_SurfClustSim_SmoothFixed(SUMA_SurfaceObject *SO, double **wgt,
                                  int niter, float *field, const byte *mask,
                                  SUMA_SURFCLUSTSIM_WORK *work);

int SUMA_SurfClustSim_ChooseCompatNiter(
   SUMA_SurfaceObject *SO, double **wgt, float **field, int nfield,
   const byte *mask, double target_fwhm, int max_iter, float *final_fwhm,
   SUMA_SURFCLUSTSIM_PROGRESS_FN progress, void *progress_data);

int SUMA_SurfClustSim_Rescale(float *field, int nnode, const byte *mask,
                              double *stdev_out);

double SUMA_SurfClustSim_MaxArea(
   const SUMA_SURFCLUSTSIM_GRAPH *graph, const float *node_area,
   const float *field, const byte *mask, double threshold, int sign_mode,
   SUMA_SURFCLUSTSIM_WORK *work);

int SUMA_SurfClustSim_MaxAreasSweep(
   const SUMA_SURFCLUSTSIM_GRAPH *graph, const float *node_area,
   const float *field, const byte *mask, int sign_mode,
   const double *threshold_a, int nthreshold_a, double *max_area_a,
   const double *threshold_b, int nthreshold_b, double *max_area_b,
   SUMA_SURFCLUSTSIM_WORK *work);

#endif
