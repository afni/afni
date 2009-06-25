/***********************************************************************
  retroicor.c: Implementation of Retrospective Image
  Correction for physiological motion effects, using a slightly
  modified version of the RETROICOR algorithm described in:

    Glover, G. H., Li, T., & Ress, D. (2000). Image-based method for
  retrospective correction of physiological motion effects in fMRI:
  RETROICOR. Magnetic Resonance in Medicine, 44, 162-167.

  Fred Tam
  Sunnybrook & Women's College Health Sciences Centre
  April 15, 2002

  Copyright (C) 2002 Sunnybrook & Women's College Health Sciences Centre,
  Toronto, ON, Canada. As part of the AFNI software package, this software
  is distributed under the GNU General Public License, Version 2.
  See the file README.copyright for details.
************************************************************************/

#include <math.h>

#ifndef M_PI
# define M_PI		3.14159265358979323846	/* pi */
#endif

#include "retroicor.h"

/* Find the next local maximum value in the float data sequence that is
   above the given threshold, starting at the given point.

   PRE:
   cdata is an array of float data;
   numpoints is the total number of datapoints in the cdata array;
   startpoint is the index of the first point to consider (zero indexed);
   maxpoint and endpoint are valid non-NULL pointers to integer values;
   threshold is the value which data points must exceed to be considered;
   0 <= startpoint < numpoints;

   POST:
   return value is -1 on error or if there was no data with value > threshold,
   else return value is 0 and *maxpoint is the index of the point with maximum
   value in the next peak after and including startpoint and *endpoint is the
   index of the first point after the peak, where a peak is a contiguous
   series of datapoints with value > threshold;
*/
int _RIC_findNextCardiacPeak(const float * cdata,
			     int numpoints, int startpoint,
			     int * maxpoint, int * endpoint,
			     float threshold) {

    int curidx = startpoint;  /* Index of current point under examination */
    int maxidx;               /* Index of maximum point found so far */

    /* Quick check of arguments */
    if (cdata == NULL || startpoint >= numpoints || startpoint < 0 ||
	maxpoint == NULL || endpoint == NULL) {

	return -1;
    }

    /* Skip over data <= threshold, if any */
    while (cdata[curidx] <= threshold) {
	curidx += 1;

	/* Check if we ran out of data without finding anything > threshold */
	if (curidx >= numpoints) {
	    return -1;
	}
    }

    /* Find the max point in this region > threshold */
    maxidx = curidx;
    while (cdata[curidx] > threshold) {
	/* Check if this is a new max */
	if (cdata[curidx] > cdata[maxidx]) {
	    maxidx = curidx;
	}

	curidx += 1;

	/* Stop if we ran out of data */
	if (curidx >= numpoints) {
	    break;
	}
    }

    *maxpoint = maxidx;
    *endpoint = curidx;
    return 0;
}

/* Implementation Note: Some of the calculations in the loops can be
   factored out, but I choose not to for clarity. This is all O(N) in the
   number of timepoints anyways (I think).
*/
MRI_IMAGE * RIC_ToCardiacPhase(MRI_IMAGE * card, float threshold) {

    int numSamps;            /* Number of samples in vector */
    MRI_IMAGE * cardphase;   /* The cardiac phase vector to return */
    float * cpdata;          /* Pointer to cardiac phase vector data */
    float * cdata;           /* Pointer to cardiac vector data */
    double twoPI = 2 * M_PI; /* 2 * PI  (intermediate value for calculation) */
    double twoPI_t2_t1;      /* 2 * PI / (t_2 - t_1)  (intermediate value) */
    double phase;            /* card phase: 2 * PI / (t_2 - t_1) * (t - t_1) */
    int lastpeakpt = 0;      /* The last cdata timepoint searched for a peak */
    int t = 0;               /* The current time */
    int t_1 = 0;             /* The previous cardiac peak time */
    int t_2;                 /* The next cardiac peak time */

    /* Quick check of arguments */
    if (card == NULL || card->nx < 2 || card->kind != MRI_float) {
	return NULL;
    }

    /* Initialize */
    numSamps = card->nx;
    cardphase = mri_new(numSamps, 1, MRI_float);
    cpdata = MRI_FLOAT_PTR(cardphase);
    cdata = MRI_FLOAT_PTR(card);

    /* Iterate over the cardiac peaks, assuming data start is peak */
    while (_RIC_findNextCardiacPeak(cdata, numSamps, lastpeakpt, &t_2,
				    &lastpeakpt, threshold) == 0) {

	/* Fill in the cardiac phase values between peaks */
	twoPI_t2_t1 = twoPI / (t_2 - t_1);
	phase = 0.0;  /* Since we always have t == t_1 at this point) */
	for ( ; t < t_2; t += 1) {
	    cpdata[t] = phase;
	    phase += twoPI_t2_t1;
	}

	t_1 = t_2;
    }

    /* Fill in any remaining phase values, assuming end of data is peak */
    twoPI_t2_t1 = twoPI / (numSamps - t_1);
    phase = 0.0;
    for ( ; t < numSamps; t += 1) {
	cpdata[t] = phase;
	phase += twoPI_t2_t1;
    }

    return cardphase;
}

MRI_IMAGE * RIC_ToRespPhase(MRI_IMAGE * resp, int winsize) {

    int numSamps;           /* Number of samples in input vector */
    MRI_IMAGE * respphase;    /* The resp phase vector to return */
    float * rpdata;           /* Pointer to resp phase vector data */
    float * rdata;            /* Pointer to resp vector data */
    float * nrdata;           /* Pointer to normalized resp data */
    int curr;                 /* Current resp data vector item */
    int currdist;             /* Distance from current resp data vector item */
    float maxr, minr;         /* Max and min resp data vector values */
    double hist[RIC_HISTSIZE];/* Histogram of resp data values */
    int curb;                 /* Current histogram bin */
    double binfact;           /* Multiply value by this to get bin number */
    double binshift;          /* Subtract from bin # to centre bins on range */
    double leftsum, rightsum; /* Sum of resp values left, right of point */
    double nisPI;             /* Intermediate value: M_PI / numInSamps */

    /* Quick check of arguments */
    if (resp == NULL || resp->nx < 2 || resp->kind != MRI_float || winsize<2) {
	return NULL;
    }

    /* Initialize */
    numSamps = resp->nx;
    nrdata = malloc(sizeof(float) * numSamps);
    if (nrdata == NULL) {
	return NULL;
    }
    respphase = mri_new(numSamps, 1, MRI_float);
    rpdata = MRI_FLOAT_PTR(respphase);
    rdata = MRI_FLOAT_PTR(resp);
    for (curb = 0; curb < RIC_HISTSIZE; curb += 1) {
	hist[curb] = 0.0;
    }

    /* Collect stats */

    /* Find max and min (for normalization and histogram range) */
    maxr = minr = rdata[0];
    for (curr = 1; curr < numSamps; curr += 1) {
	if (rdata[curr] > maxr) {
	    maxr = rdata[curr];
	} else if (rdata[curr] < minr) {
	    minr = rdata[curr];
	}
    }

    /* Normalize the data (just subtract minr to make everything >= 0) */
    for (curr = 0; curr < numSamps; curr += 1) {
	nrdata[curr] = rdata[curr] - minr;
    }
    maxr -= minr;
    minr = 0.0;

    /***** We'll use nrdata from now on instead of rdata *****/

    /* Create histogram (the following makes sense only if all data >= 0) */
    /* Actually this is broken; rounding means the probability of a value
       falling into the first and last buckets is less than it should be
       --but this is good enough and takes few brain cells to program */
    /* Added a fudge factor so that the first and last histogram bins are
       almost as wide as the others but not so wide that rounding the bin
       number could cause it to refer to a nonexistant bin */
    binfact = (RIC_HISTSIZE - 2 * RIC_HISTFUDGE) / maxr;
    binshift = 0.5 - RIC_HISTFUDGE;
    if (binfact <= 0.0 || binshift <= 0.0) {  /* Check just in case! */
	free(nrdata);
	return NULL;
    }
    for (curr = 0; curr < numSamps; curr += 1) {
	hist[(int)rint(nrdata[curr] * binfact - binshift)] += 1.0;
	/* a.k.a. nrdata[curr] / maxr * RIC_HISTSIZE */
    }

    /* Turn the histogram into a vector of percentile values */
    for (curb = 1; curb < RIC_HISTSIZE; curb += 1) {
	hist[curb] += hist[curb - 1];
    }
    /* Actually, make that PI * percentile, since we use that later */
    nisPI = M_PI / numSamps;
    for (curb = 0; curb < RIC_HISTSIZE; curb += 1) {
	hist[curb] *= nisPI;
    }

    /* Iterate over the resp waveform samples, converting each to phase */

    for (curr = 0; curr < numSamps; curr += 1) {
	/* Estimate of dR/dt: Slope of N-point means on either side of point */
	/* Further Simplification: We don't actually need the mean to find
	   the slope, just the sums on either side of the point */
	/* There are better ways to do this but YES WE HAVE NO BANANAS */
	rightsum = leftsum = 0;
	for (currdist = 0; currdist < winsize; currdist += 1) {
	    if (curr + currdist < numSamps) {
		rightsum += nrdata[curr + currdist];
	    } else {
		rightsum += nrdata[curr];
	    }
	    if (curr - currdist >= 0) {
		leftsum += nrdata[curr - currdist];
	    } else {
		leftsum += nrdata[curr];
	    }
	}

	/* Hooray!! Now we get the result! */
	if ((rightsum - leftsum) < 0.0) {   /* Negative slope */
	    rpdata[curr] = - hist[(int)rint(nrdata[curr] * binfact
					    - binshift)];
	} else {                            /* Positive slope */
	    rpdata[curr] =   hist[(int)rint(nrdata[curr] * binfact
					    - binshift)];
	}
    }

    free(nrdata);
    return respphase;
}

/* I know the "== 0.0" is unsafe, but the AFNI docs say that a zero scale
   factor means the data is not scaled. Compare with epsilon? */
#define RIC_CALCVOXELMEANS__DO_VOXSUM(t) {              \
    t * brickdset = DSET_ARRAY(dset, ival);             \
    if (brickdset == NULL) {                            \
        free(avg);                                      \
        return NULL;                                    \
    }                                                   \
    if (scalefactor == 0.0) {                           \
	for (ivox = 0; ivox < nvoxs; ivox += 1) {       \
	    avg[ivox] += brickdset[ivox];               \
	}                                               \
    } else {                                            \
	for (ivox = 0; ivox < nvoxs; ivox += 1) {       \
	    avg[ivox] += brickdset[ivox] * scalefactor; \
	}                                               \
    }                                                   \
}

/* Calculates the average value for each voxel in dset across all timepoints.

   PRE:
   dset is a valid 3D+T dataset with at least 1 timepoint;
   ignore is the number of timepoints to ignore at the beginning of dset;
   0 <= ignore < number of timepoints in dset;

   POST:
   return value is NULL on error,
   else, return value is an array of floats with the same dimensions as the
   subbricks of dset, containing the voxel value averages;

   Note: The caller is responsible for free()ing the returned block of memory
   when done.

   Note2: The complex datatype is not supported, and any such bricks will
   result in an error (NULL return value).
*/
double * RIC_CalcVoxelMeans(const THD_3dim_dataset * dset, int ignore) {

    double * avg;       /* The voxel averages to be returned */
    float scalefactor; /* Current dset brick scaling factor */
    int ival, nvals;   /* Current, number of dset timepoints */
    int ivox, nvoxs;   /* Current, number of dset brick voxels */

    /* Quick check of arguments */
    if (!ISVALID_3DIM_DATASET(dset) || DSET_NVALS(dset) < 1 ||
	ignore < 0 || ignore >= DSET_NVALS(dset)) {

	return NULL;
    }

    /* Initialize */
    DSET_load(dset);
    nvals = DSET_NVALS(dset);
    nvoxs = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;
    avg = malloc(sizeof(double) * nvoxs);
    if (avg == NULL) {
	return NULL;
    }

    /* Calculate the voxel averages; treat matrices as 1-D arrays */

    /* Zero the voxel sums */
    for (ivox = 0; ivox < nvoxs; ivox += 1) {
	avg[ivox] = 0.0;
    }

    /* Sum each voxel across time (and hope there are not too many points) */
    for (ival = ignore; ival < nvals; ival += 1) {
	scalefactor = DSET_BRICK_FACTOR(dset, ival);

	switch (DSET_BRICK_TYPE(dset, ival)) {
	case MRI_short:
	    RIC_CALCVOXELMEANS__DO_VOXSUM(short);
	    break;
	case MRI_byte:
	    RIC_CALCVOXELMEANS__DO_VOXSUM(byte);
	    break;
	case MRI_float:
	    RIC_CALCVOXELMEANS__DO_VOXSUM(float);
	    break;
	default: /* Unsupported datatype */
	    free(avg);
	    return NULL;
	}
    }

    /* Divide by number of timepoints to get average */
    nvals -= ignore;  /* We do not average over the ignored timepoints */
    for (ivox = 0; ivox < nvoxs; ivox += 1) {
	avg[ivox] /= nvals;
    }

    return avg;
}

/* I know the "== 0.0" is unsafe, but the AFNI docs say that a zero scale
   factor means the data is not scaled. Compare with epsilon? */
#define RIC_CALCCOEFFAB__DO_CALCNUMERDENOM(t) {                               \
    /* Load and check the dataset brick */                                    \
    t * brickdset = DSET_ARRAY(dset, ival);				      \
    if (brickdset == NULL) {						      \
	free(newa); free(newb); free(denoma); free(denomb);		      \
	return -1;							      \
    }									      \
									      \
    idenom = 0;								      \
    inumer = 0;								      \
    /* m loop outside voxel loop <- coeff arrays bigger than one volume */    \
    for (m = 1; m <= M; m += 1) {					      \
	slicestart = 0;							      \
	/* Iterate over slices within this TR */			      \
	for (islice = 0; islice < nslices; islice += 1) {		      \
	    sliceend = slicestart + nvoxpers;				      \
									      \
	    /* Get the slice time in milliseconds */			      \
	    slicetime = THD_timeof_slice(ival, islice, dset);		      \
	    switch (dset->taxis->units_type) {				      \
	    case UNITS_MSEC_TYPE: break;				      \
	    case UNITS_SEC_TYPE:  slicetime *= 1000; break;		      \
	    case UNITS_HZ_TYPE:   slicetime = 1000 / slicetime; break;	      \
	    default: free(newa); free(newb); free(denoma); free(denomb);      \
		return -1;						      \
	    }								      \
									      \
	    /* Find phase at slice time and add up the denominators */	      \
	    mp = m * pdata[(int)(slicetime / sampd)];			      \
	    cmp = cos(mp); smp = sin(mp);				      \
	    denoma[idenom] += cmp * cmp; denomb[idenom] += smp * smp;	      \
									      \
	    /* Add up the numerators, using scale factor if necessary */      \
	    if (scalefactor == 0.0) {					      \
		for (ivox = slicestart; ivox < sliceend; ivox += 1) {	      \
		    newa[inumer] += (brickdset[ivox] - avg[ivox]) * cmp;      \
		    newb[inumer] += (brickdset[ivox] - avg[ivox]) * smp;      \
		    inumer += 1;					      \
		}							      \
	    } else {							      \
		for (ivox = slicestart; ivox < sliceend; ivox += 1) {	      \
		    newa[inumer] +=					      \
			((brickdset[ivox]*scalefactor) - avg[ivox]) * cmp;    \
		    newb[inumer] +=					      \
			((brickdset[ivox]*scalefactor) - avg[ivox]) * smp;    \
		    inumer += 1;					      \
		}							      \
	    }								      \
									      \
	    slicestart = sliceend;					      \
	    idenom += 1;						      \
	}								      \
    }									      \
}

int RIC_CalcCoeffAB(THD_3dim_dataset * dset, MRI_IMAGE * phase,
		    const double * avg, double ** a, double ** b,
		    int M, int ignore) {

    float scalefactor;         /* Scaling factor for current dset brick */
    int ival, nvals;           /* Current, number of dset timepoints */
    int ivox, nvoxs;           /* Current, number of voxels in dset subbrick */
    double * newa, * newb;     /* Coefficients a and b, to be calculated */
    float * pdata;             /* Phase data */
    int m;                     /* Current order number (1, M) */
    double mp, cmp, smp;       /* m * current phase; cos(mp); sin(mp) */
    double sampd;              /* Intersample time in ms for phase */
    double slicetime;          /* Time of current slice in milliseconds */
    int slicestart;            /* Voxel index of 1st voxel in current slice */
    int sliceend;              /* End voxel index of current slice */
    double * denoma, * denomb; /* Coeff denominators for each m and slice */
    int idenom, ndenoms;       /* Current, number of coeff denominators */
    int inumer, nnumers;       /* Current, number of coeff numerators */
    int islice, nslices;       /* Current, number of slices in a subbrick */
    int nvoxpers;              /* The number of voxels per slice */

    /* Quick check of arguments */
    if (!ISVALID_3DIM_DATASET(dset) || DSET_NVALS(dset) < 1 ||
	!ISVALID_TIMEAXIS(dset->taxis) ||
	phase == NULL || phase->nx < 1 || phase->ny != 1 ||
	avg == NULL || a == NULL || b == NULL || M < 1 ||
	ignore < 0 || ignore >= DSET_NVALS(dset)) {

	return -1;
    }

    /* Initialize */
    DSET_load(dset);
    nvals = DSET_NVALS(dset);
    nvoxpers = dset->daxes->nxx * dset->daxes->nyy;
    nslices = dset->daxes->nzz;
    nvoxs = nvoxpers * nslices;
    ndenoms = nslices * M;
    nnumers = nvoxs * M;
    sampd = dset->taxis->ttdel * nvals / phase->nx;
    switch (dset->taxis->units_type) {
    case UNITS_MSEC_TYPE: break;
    case UNITS_SEC_TYPE:  sampd *= 1000; break;
    case UNITS_HZ_TYPE:   sampd = 1000 / sampd; break;
    default: return -1;
    }
    pdata = MRI_FLOAT_PTR(phase);
    newa = malloc(sizeof(double) * nnumers);
    if (newa == NULL) {
	return -1;
    }
    newb = malloc(sizeof(double) * nnumers);
    if (newb == NULL) {
	free(newa);
	return -1;
    }
    for (inumer = 0; inumer < nnumers; inumer += 1) {
	newa[inumer] = 0.0; newb[inumer] = 0.0;
    }
    denoma = malloc(sizeof(double) * ndenoms);
    if (denoma == NULL) {
	free(newa); free(newb);
	return -1;
    }
    denomb = malloc(sizeof(double) * ndenoms);
    if (denomb == NULL) {
	free(newa); free(newb); free(denoma);
	return -1;
    }
    for (idenom = 0; idenom < ndenoms; idenom += 1) {
	denoma[idenom] = 0.0; denomb[idenom] = 0.0;
    }

    /* Calculate the coeff numerators and denominators */
    /* Iterate over dset timepoints (TRs) => BRIK read from disk once */
    for (ival = ignore; ival < nvals; ival += 1) {
	scalefactor = DSET_BRICK_FACTOR(dset, ival);

	/* Calculate coeff numerators and denominators over all data */
	switch (DSET_BRICK_TYPE(dset, ival)) {
	case MRI_short:
	    RIC_CALCCOEFFAB__DO_CALCNUMERDENOM(short);
	    break;
	case MRI_byte:
	    RIC_CALCCOEFFAB__DO_CALCNUMERDENOM(byte);
	    break;
	case MRI_float:
	    RIC_CALCCOEFFAB__DO_CALCNUMERDENOM(float);
	    break;
	default: /* Unsupported datatype */
	    free(newa); free(newb); free(denoma); free(denomb);
	    return -1;
	}
    }

    /* Divide the coeff numerators by their denominators */
    idenom = 0;
    inumer = 0;
    /* m loop outside voxel loop <- coeff arrays bigger than one volume */
    for (m = 1; m <= M; m += 1) {
	slicestart = 0;
	/* Iterate over slices */
	for (islice = 0; islice < nslices; islice += 1) {
	    sliceend = slicestart + nvoxpers;
	    /* Divide numerator by denominator */
	    for (ivox = slicestart; ivox < sliceend; ivox += 1) {
		newa[inumer] /= denoma[idenom];
		newb[inumer] /= denomb[idenom];
		inumer += 1;
	    }
	    slicestart = sliceend;
	    idenom += 1;
	}
    }

    *a = newa; *b = newb;
    free(denoma); free(denomb);
    return 0;
}

/* I know the "== 0.0" is unsafe, but the AFNI docs say that a zero scale
   factor means the data is not scaled. Compare with epsilon? */
#define RIC_CORRECTDATASET__DO_CORRECT(t) {				      \
    /* Load and check the dataset brick */                                    \
    t * brickdset = DSET_BRICK_ARRAY(dset, ival);			      \
    if (brickdset == NULL) {						      \
	return -1;							      \
    }									      \
									      \
    icoeff = 0;								      \
    /* m loop outside voxel loop <- coeff arrays bigger than one volume */    \
    for (m = 1; m <= M; m += 1) {					      \
	slicestart = 0;							      \
	/* Iterate over slices within this TR */			      \
	for (islice = 0; islice < nslices; islice += 1) {		      \
	    sliceend = slicestart + nvoxpers;				      \
									      \
	    /* Get the slice time in milliseconds */			      \
	    slicetime = THD_timeof_slice(ival, islice, dset);		      \
	    switch (dset->taxis->units_type) {				      \
	    case UNITS_MSEC_TYPE: break;				      \
	    case UNITS_SEC_TYPE:  slicetime *= 1000; break;		      \
	    case UNITS_HZ_TYPE:   slicetime = 1000 / slicetime; break;	      \
	    default: return -1;						      \
	    }								      \
									      \
	    /* Find phase at slice time and calculate intermediate value */   \
	    mp = m * pdata[(int)(slicetime / sampd)];			      \
	    cmp = cos(mp); smp = sin(mp);				      \
									      \
	    /* Apply the corrections to the voxels */			      \
	    if (scalefactor == 0.0) {					      \
		for (ivox = slicestart; ivox < sliceend; ivox += 1) {	      \
		    brickdset[ivox] -= a[icoeff] * cmp + b[icoeff] * smp;     \
		    icoeff += 1;					      \
		}							      \
	    } else {							      \
		for (ivox = slicestart; ivox < sliceend; ivox += 1) {	      \
		    brickdset[ivox] -= (a[icoeff] * cmp + b[icoeff] * smp)    \
			/ scalefactor;					      \
		    icoeff += 1;					      \
		}							      \
	    }								      \
									      \
	    slicestart = sliceend;					      \
	}								      \
    }									      \
}

int RIC_CorrectDataset(THD_3dim_dataset * dset, MRI_IMAGE * phase,
		       const double * a, const double * b,
		       int M, int ignore) {

    float scalefactor;         /* Scaling factor for current dset brick */
    int ival, nvals;           /* Current, number of dset timepoints */
    int ivox, nvoxs;           /* Current, number of voxels in dset subbrick */
    float * pdata;             /* Phase data */
    int m;                     /* Current order number (1, M) */
    double mp, cmp, smp;       /* m * phase; cos(mp); sin(mp) */
    double sampd;              /* Intersample time in ms for phase */
    double slicetime;          /* Time of current slice in milliseconds */
    int slicestart;            /* Voxel index of 1st voxel in current slice */
    int sliceend;              /* End voxel index of current slice */
    int icoeff, ncoeffs;       /* Current, number of a,b coefficients */
    int islice, nslices;       /* Current, number of slices in a subbrick */
    int nvoxpers;              /* The number of voxels per slice */

    /* Quick check of arguments */
    if (!ISVALID_3DIM_DATASET(dset) || DSET_NVALS(dset) < 1 ||
	!ISVALID_TIMEAXIS(dset->taxis) ||
	phase == NULL || phase->nx < 1 || phase->ny != 1 ||
	a == NULL || b == NULL || M < 1 ||
	ignore < 0 || ignore >= DSET_NVALS(dset)) {

	return -1;
    }

    /* Initialize */
    DSET_load(dset);
    nvals = DSET_NVALS(dset);
    nvoxpers = dset->daxes->nxx * dset->daxes->nyy;
    nslices = dset->daxes->nzz;
    nvoxs = nvoxpers * nslices;
    ncoeffs = nvoxs * M;
    sampd = dset->taxis->ttdel * nvals / phase->nx;
    switch (dset->taxis->units_type) {
    case UNITS_MSEC_TYPE: break;
    case UNITS_SEC_TYPE:  sampd *= 1000; break;
    case UNITS_HZ_TYPE:   sampd = 1000 / sampd; break;
    default: return -1;
    }
    pdata = MRI_FLOAT_PTR(phase);

    /* Correct each subbrick in dset */
    /* Iterate over dset timepoints (TRs) => BRIK read from disk once */
    for (ival = ignore; ival < nvals; ival += 1) {
	scalefactor = DSET_BRICK_FACTOR(dset, ival);

	/* Apply each order of correction to each voxel of the subbrick */
	switch (DSET_BRICK_TYPE(dset, ival)) {
	case MRI_short:
	    RIC_CORRECTDATASET__DO_CORRECT(short);
	    break;
	case MRI_byte:
	    RIC_CORRECTDATASET__DO_CORRECT(byte);
	    break;
	case MRI_float:
	    RIC_CORRECTDATASET__DO_CORRECT(float);
	    break;
	default: /* Unsupported datatype */
	    return -1;
	}
    }

    return 0;
}
