/***********************************************************************
  retroicor.h: Implementation of Retrospective Image
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

#ifndef _SW__RETROICOR_H_
#define _SW__RETROICOR_H_

#include "afni.h"

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

#define RIC_HISTSIZE 100
#define RIC_HISTFUDGE 0.0000001

/* Transform cardiac waveform to cardiac phase:

     P_c(t) = 2 * PI * (t - t_1) / (t_2 - t_1)

   where t_1 is the time of the last R-wave peak before t
     and t_2 is the time of the next R-wave peak after t

   PRE:
   card is the cardiac waveform 1D data;
   card has at least 2 timepoints;
   card contains float data;
   threshold is the value that card data must exceed to be considered part of
   an R-wave peak;

   POST:
   return value is NULL on error,
   else, return value is the phase representation of the entire card dataset
   with the same number of timepoints as card;

   It is the caller's responsibility to call mri_free() on the returned struct.

   Note that in this implementation the start and end of the card data are
   considered to be R-wave peaks (see above P_c formula for why).
*/
MRI_IMAGE * RIC_ToCardiacPhase(MRI_IMAGE * card, float threshold);

/* Transform resp waveform to resp phase:

     P_r(t) = PI * sum(H(b), b = 1 .. rnd(R(T)/R_max)) /
                   sum(H(b), b = 1 .. 100) * sgn(dR/dt)

   where R(t) is the resp signal amplitude normalized to (0, R_max),
         H(b) is the number of occurances in bin b of histogram H
              which divides (0, R_max) into 100 bins, each centred at
	      b * R_max / 100
     and dR/dt is computed by convolving with a 39-point kernel (quadratic)

   PRE:
   resp is the resp waveform 1D data;
   resp has at least 2 timepoints;
   resp contains float data;
   winsize is the size of the window on the resp data used to estimate slope;
   winsize >= 2;

   POST:
   return value is NULL on error,
   else, return value is the phase representation of the entire resp dataset
   with the same number of timepoints as resp;

   It is the caller's responsibility to call mri_free() on the returned struct.

   Implementation note: Convolution is overkill just to get sgn(dR/dt), so
   this implementation estimates the slope with the difference between 
   winsize-point means on either side of each point. Try setting winsize to
   1/2 the sampling rate of resp in Herz. The size of the histogram H is
   defined in RIC_HISTSIZE.
*/
MRI_IMAGE * RIC_ToRespPhase(MRI_IMAGE * resp, int winsize);

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
double * RIC_CalcVoxelMeans(const THD_3dim_dataset * dset, int ignore);

/* Calculate the a and b per-voxel coefficients for the correction:

   a_x_m = sum((y(t_n) - y_u) * cos(m * P_x(t_n)), n = 1..N) /
           sum((cos(m * P_x(t_n))) ^ 2, n = 1..N)

   b_x_m = sum((y(t_n) - y_u) * sin(m * P_x(t_n)), n = 1..N) /
           sum((sin(m * P_x(t_n))) ^ 2, n = 1..N)

   where x is from {c, r}, for cardiac, resp, resp.
         y_u is the average of y over the time series
         m is the order, a value [1, M]

   PRE:
   dset is a valid 3D+T dataset with at least 1 timepoint and valid time axis;
   phase is the cardiac or resp phase data, spanning the same time as dset;
   avg points to an array of voxel averages corresponding to dset (generate
    avg with something like this: "avg = RIC_CalcVoxelMeans(dset, ignore);")
   a and b are non-NULL pointers to double*;
   M is the maximal order of the correction to be made;
   M > 0;
   ignore is the number of timepoints to ignore at the beginning of dset;
   0 <= ignore < number of timepoints in dset;

   POST:
   return value is -1 on error,
   else, return value is 0 and a and b are pointers to arrays containing the
   a and b coefficients described above. The coefficients are stored in the
   same order as the voxels in the dset subbricks, offset by m-1 times the
   size of the subbricks such that a[i + (m - 1) * DSET_NVALS(dset)] is the
   a coefficient for the i'th voxel in dset (zero-based) with order m. Nothing
   is done to dset.

   Note: The caller is responsible for free()ing the returned blocks of memory
   for a and b when done.

   The complex datatype is not supported, and any such bricks will result in
   an error (return value -1).
*/
int RIC_CalcCoeffAB(THD_3dim_dataset * dset, MRI_IMAGE * phase,
		    const double * avg, double ** a, double ** b,
		    int M, int ignore);

/* Apply the correction to a dataset _in_place_:

     y_c(t) = y(t) - y_p(t)

     y_p(t) = sum(a_x_m * cos(m * P_x(t)) + b_x_m * sin(m * P_x(t)), m = 1..M)

   where M = 2 is typical, and x is from {c, r} for cardiac, resp, resp.

   PRE:
   dset is a valid 3D+T dataset with at least 1 timepoint and valid time axis;
   phase is the cardiac or resp phase data, spanning the same time as dset;
   a and b are a and b coefficient arrays, as returned by RIC_CalcCoeffAB();
   a and b have M * (number of voxels in dset subbrick) entries;
   M is the maximal order of the correction to be made;
   M > 0;
   ignore is the number timepoints to ignore at the beginning of dset;
   0 <= ignore < number of timepoints in dset;

   POST:
   return value is -1 on error,
   else, return value is 0 and dset has been corrected as described above.

   The complex datatype is not supported, and any such bricks will result in
   an error (return value -1).

   WARNING WARNING WARNING
   =======================
   The correction is made to dset "in-place" so if you want to keep the
   original dataset, COPY it and pass the copy to this function.
*/
int RIC_CorrectDataset(THD_3dim_dataset * dset, MRI_IMAGE * phase,
		       const double * a, const double * b,
		       int M, int ignore);

#ifdef  __cplusplus
}
#endif

#endif
