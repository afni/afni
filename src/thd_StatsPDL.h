/* Set of statistical functions written by Peter Lauren.  Started 17-Jul-2020 */

float getMean(float *ts, int npts);
float getSigma(float *ts, int npts, float mean);
float getSkewness(float *ts, int npts);
float getKurtosis(float *ts, int npts);
