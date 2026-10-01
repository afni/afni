#ifndef SUMA_SURFACF_INCLUDED
#define SUMA_SURFACF_INCLUDED

/* ------------------------------------------------------------------------
   Spatial autocorrelation on a surface, as a function of GEODESIC distance,
   fitted to AFNI's mixed ACF model:

      ACF(r) = a*exp(-r*r/(2*b*b)) + (1-a)*exp(-r/c)

   Header-only with static functions, following the "#include zgaussian.c"
   idiom already used elsewhere in AFNI.  Both SurfFWHM (which measures the
   ACF of real data) and SurfClustSim (which has to generate noise matching a
   requested ACF, and to check that it did) need this, and a header keeps one
   implementation without adding a source file to either program's build or
   touching any shared library.

   Why any of this exists: a FWHM is one number describing the ACF near the
   origin.  Cluster-extent inference depends on the far tail, and real data
   has a much heavier tail than the Gaussian shape a FWHM implicitly assumes.
   That mismatch is what inflated cluster false-positive rates in the volume
   (Eklund, Nichols & Knutsson 2016) and is why 3dClustSim gained -acf.

   The estimator mirrors mri_estimate_ACF() in mri_fwhm.c, substituting
   geodesic neighbourhoods for voxel offsets.  The FIT is not reimplemented:
   ACF_cluster_to_modelE() (mri_fwhm.c) does everything once the radius/ACF
   arrays exist, and none of that is lattice-specific.  Its only such step is
   turning (i,j,k) offsets into radii via CRAD(), so we hand it a
   one-dimensional pseudo-cluster -- bin index in i, zero in j and k, bin
   width as dx -- and CRAD() then yields exactly dr*bin.  Identical model,
   identical optimizer, identical effective-FWHM derivation, and nothing in
   mri_fwhm.c changes.
   ------------------------------------------------------------------------ */

/*! Number of geodesic bins for a given radius and bin width. */
static int SUMA_SurfACF_nbins(float radius, float dr)
{
   if (dr <= 0.0f || radius <= dr) return 0;
   return (int)(radius/dr) + 1;
}

/*! Default radius and bin width for a surface, if the caller has no opinion.
    Scaled by mean intersegment distance rather than assumed millimetres:
    one bin per edge step, out to 20 edge steps. */
static void SUMA_SurfACF_defaults(SUMA_SurfaceObject *SO,
                                  float *radius, float *dr)
{
   if (!SO || !SO->EL || SO->EL->AvgLe <= 0.0f) return;
   if (dr     && *dr     <= 0.0f) *dr     = SO->EL->AvgLe;
   if (radius && *radius <= 0.0f) *radius = 20.0f * SO->EL->AvgLe;
}

/*!
   Accumulate one field's autocorrelation curve into acf_accum[nbins].

   acf_accum and nacc must be caller-allocated and zeroed before the first
   call; each successful call adds one curve to acf_accum and increments the
   per-bin counter nacc, so several fields (or several data columns) can be
   averaged by calling repeatedly and dividing at the end.

   off is a scratch offset structure; pass one in so a loop over many fields
   does not reallocate it.

   stride subsamples the centre nodes: 1 visits every node, n visits every
   nth.  The neighbourhood walk is the expensive part, and the curve is an
   average over a great many node pairs, so a few thousand centres already
   give a smooth estimate.  Use 1 when measuring real data and accuracy
   matters; use a stride when the estimate is an inner step of something else.

   Returns 1 on success, 0 if the field carried no usable variance.
*/
static int SUMA_SurfACF_accumulate_str(SUMA_SurfaceObject *SO,
                                   const float *field,
                                   const byte *mask, float radius, float dr,
                                   int nbins, double *acf_accum, double *nacc,
                                   SUMA_GET_OFFSET_STRUCT *off, int stride)
{
   double fsum=0.0, fsq=0.0, fbar, fvar, arg;
   double *acc=NULL;
   int *cnt=NULL;
   int count=0, n, m, ib, layer, item, ok=0;

   if (!SO || !field || !acf_accum || !nacc || !off || nbins < 5) return 0;

   for (n=0; n < SO->N_Node; ++n) if (!mask || mask[n]) {
      arg = field[n]; fsum += arg; fsq += arg*arg; ++count;
   }
   if (count < 9 || fsq <= 0.0) return 0;
   fbar = fsum/(double)count;
   fvar = (fsq - fsum*fsum/(double)count)/((double)count - 1.0);
   if (fvar <= 0.0) return 0;

   acc = (double *)calloc(nbins, sizeof(double));
   cnt = (int    *)calloc(nbins, sizeof(int));
   if (!acc || !cnt) goto done;

   if (stride < 1) stride = 1;
   for (n=0; n < SO->N_Node; n += stride) {
      if (mask && !mask[n]) continue;
      if (!SUMA_getoffsets2(n, SO, radius, off, NULL, 0)) goto done;
      arg = (double)field[n] - fbar;
      for (layer=1; layer < off->N_layers; ++layer) {
         for (item=0; item < off->layers[layer].N_NodesInLayer; ++item) {
            m = off->layers[layer].NodesInLayer[item];
            if (mask && !mask[m]) continue;
            if (off->OffVect[m] > radius) continue;
            ib = (int)(off->OffVect[m]/dr + 0.5f);
            if (ib <= 0 || ib >= nbins) continue;
            acc[ib] += ((double)field[m] - fbar) * arg;
            cnt[ib] += 1;
         }
      }
      SUMA_Recycle_getoffsets(off);
   }

   /* same guard as mri_estimate_ACF(): ignore thinly populated bins */
   for (ib=1; ib < nbins; ++ib) {
      if (cnt[ib] > 5) {
         acf_accum[ib] += acc[ib] / (fvar * ((double)cnt[ib] - 1.0));
         nacc[ib]      += 1.0;
      }
   }
   acf_accum[0] += 1.0; nacc[0] += 1.0;   /* ACF(0) == 1 by definition */
   ok = 1;

done:
   if (acc) free(acc);
   if (cnt) free(cnt);
   return ok;
}

/*! Convenience wrapper: visit every node. */
static int SUMA_SurfACF_accumulate(SUMA_SurfaceObject *SO, const float *field,
                                   const byte *mask, float radius, float dr,
                                   int nbins, double *acf_accum, double *nacc,
                                   SUMA_GET_OFFSET_STRUCT *off)
{
   return SUMA_SurfACF_accumulate_str(SO, field, mask, radius, dr, nbins,
                                      acf_accum, nacc, off, 1);
}

/*! Average the accumulated curve in place; bins never populated become -1. */
static void SUMA_SurfACF_finalize(double *acf_accum, const double *nacc,
                                  int nbins)
{
   int ib;
   for (ib=0; ib < nbins; ++ib)
      acf_accum[ib] = (nacc[ib] > 0.0) ? acf_accum[ib]/nacc[ib] : -1.0;
}

/*!
   Fit an averaged curve to the mixed model, via AFNI's own fitter.
   Bins holding -1 (never populated) are dropped.  Returns {a,b,c,FWHM}, or
   all -1 on failure.
*/
static float_quad SUMA_SurfACF_fit(const double *acf, int nbins, float dr)
{
   float_quad qout = { -1.0f, -1.0f, -1.0f, -1.0f };
   MCW_cluster *pseudo=NULL;
   int ib;

   if (!acf || nbins < 5 || dr <= 0.0f) return qout;

   INIT_CLUSTER(pseudo);
   if (!pseudo) return qout;
   for (ib=0; ib < nbins; ++ib) {
      if (acf[ib] < -0.5) continue;
      ADDTO_CLUSTER(pseudo, ib, 0, 0, (float)acf[ib]);
   }
   /* dx = dr with j = k = 0 makes CRAD() return dr*bin exactly */
   if (pseudo->num_pt >= 5) qout = ACF_cluster_to_modelE(pseudo, dr, 1.0f, 1.0f);
   KILL_CLUSTER(pseudo);
   return qout;
}

/*! Value of the mixed model at radius r. */
static double SUMA_SurfACF_model(double a, double b, double c, double r)
{
   return a*exp(-0.5*r*r/(b*b)) + (1.0-a)*exp(-r/c);
}

#endif /* SUMA_SURFACF_INCLUDED */
