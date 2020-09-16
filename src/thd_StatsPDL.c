/* Set of statistical functions written by Peter Lauren.  Started 17-Jul-2020 */

#include <math.h>
#include "mrilib.h"
#include "Tstat.h"
#include "thd_StatsPDL.h"

float getSkewness(float *ts, int npts){
    float  res=0.0f, diff;
    float   mean=getMean(ts, npts);
    float   sigma=getSigma(ts, npts, mean);
    int     ii;

    for (ii=0; ii<npts; ++ii){
        diff=(ts[ii]-mean)/sigma;
        res+=pow(diff,3.0);
    }

    return res/npts;
}

float getKurtosis(float *ts, int npts){
    float  res=0.0f, diff;
    float   mean=getMean(ts, npts);
    float   sigma=getSigma(ts, npts, mean);
    int     ii;

    for (ii=0; ii<npts; ++ii){
        diff=(ts[ii]-mean)/sigma;
        res+=pow(diff,4.0);
    }

    return res/npts;
}

float getSigma(float *ts, int npts, float mean){
    float   res=0.0f, diff;
    int     ii;

    for (ii=0; ii<npts; ++ii){
        diff=ts[ii]-mean;
        res+=diff*diff;
    }

    return sqrt(res/(npts-1));
}

float getMean(float *ts, int npts){
    float   res=0.0f;
    int     ii;

    for (ii=0; ii<npts; ++ii){
        res+=ts[ii];
    }
    return res/npts;
}
