/*
  This file contains routines for performing the Daubechies fast wavelet 
  transform analysis of time series data.

  File:    Daubechies.c
  Author:  B. Douglas Ward
  Date:    28 March 2000

*/


/*---------------------------------------------------------------------------*/
/*
  Calculate one iteration of the Daubechies forward FWT in 1-dimension.
*/

void Daubechies_forward_pass_1d (int n, float * s)
{
  int i;
  int npts;
  float * a = NULL;
  float * c = NULL;
  const float h[4] = { 0.683013, 1.18301, 0.316987, -0.183013 };

  npts = powerof2 (n);
  a = (float *) malloc (sizeof(float) * npts/2);
  c = (float *) malloc (sizeof(float) * npts/2);
  
  for (i = 0;  i < npts/2;  i++)
    {
      a[i] = (h[0]*s[(2*i)%npts] + h[1]*s[(2*i+1)%npts] + h[2]*s[(2*i+2)%npts]
	      + h[3]*s[(2*i+3)%npts]) / 2.0;
      c[i] = (h[3]*s[(2*i)%npts] - h[2]*s[(2*i+1)%npts] + h[1]*s[(2*i+2)%npts] 
	      - h[0]*s[(2*i+3)%npts]) / 2.0;
    }

  for (i = 0;  i < npts/2;  i++)
    {
      s[i] = a[i];
      s[i + npts/2] = c[i];
    }

  free (a);   a = NULL;
  free (c);   c = NULL;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the Daubechies forward fast wavelet transform in 1-dimension.
*/

void Daubechies_forward_FWT_1d (int n, float * s)
{
  int m;
  int npts;

  npts = powerof2 (n);

  for (m = n-1;  m >= 0;  m--)
    {
      Daubechies_forward_pass_1d (m+1, s);
      /*
      ts_print (npts, s);
      */
    }
}


/*---------------------------------------------------------------------------*/
/*
  Calculate one iteration of the Daubechies inverse FWT in 1-dimension.
*/

void Daubechies_inverse_pass_1d (int n, float * s)
{
  int i;
  int npts, nptsd2;
  float * a = NULL;
  float * c = NULL;
  float * r = NULL;
  const float h[4] = { 0.683013, 1.18301, 0.316987, -0.183013 };


  npts = powerof2 (n);
  nptsd2 = npts/2;
  a = s;
  c = s+nptsd2;
  r = (float *) malloc (sizeof(float) * npts);
  

  for (i = 0;  i < nptsd2;  i++)
    {
      r[2*i]   = h[2]*a[(i-1+nptsd2)%nptsd2] + h[1]*c[(i-1+nptsd2)%nptsd2] 
	       + h[0]*a[i] + h[3]*c[i];
	       
      r[2*i+1] = h[3]*a[(i-1+nptsd2)%nptsd2] - h[0]*c[(i-1+nptsd2)%nptsd2] 
	       + h[1]*a[i] - h[2]*c[i];
    }


  for (i = 0;  i < npts;  i++)
    {
      s[i] = r[i];
    }

  free (r);   r = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the Daubechies inverse fast wavelet transform in 1-dimension.
*/

void Daubechies_inverse_FWT_1d (int n, float * s)
{
  int m;
  int npts;

  npts = powerof2 (n);

  for (m = 1;  m <=n;  m++)
    {
      Daubechies_inverse_pass_1d (m, s);
      /*
      ts_print (npts, s);
      */
    }
}


/*---------------------------------------------------------------------------*/
/*
  Calculate one iteration of the Daubechies forward FWT in 2-dimensions.
*/

void Daubechies_forward_pass_2d (int n, float ** s)
{
  int i, j;
  int npts;
  float * c = NULL;


  npts = powerof2 (n);

  for (i = 0;  i < npts;  i++)
    {
      Daubechies_forward_pass_1d (n, s[i]);
    }

  c = (float *) malloc (sizeof(float) * npts);

  for (j = 0;  j < npts;  j++)
    {
      for (i = 0;  i < npts;  i++)
	c[i] = s[i][j];
      Daubechies_forward_pass_1d (n, c);
      for (i = 0;  i < npts;  i++)
	s[i][j] = c[i];
    }

  free (c);   c = NULL;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the Daubechies forward fast wavelet transform in 2-dimensions.
*/

void Daubechies_forward_FWT_2d (int n, float ** s)
{
  int m;
  int npts;

  npts = powerof2 (n);

  for (m = n-1;  m >= 0;  m--)
    {
      Daubechies_forward_pass_2d (m+1, s);
    }
}


/*---------------------------------------------------------------------------*/
/*
  Calculate one iteration of the Daubechies inverse FWT in 2-dimensions.
*/

void Daubechies_inverse_pass_2d (int n, float ** s)
{
  int i, j;
  int npts;
  float * c = NULL;


  npts = powerof2 (n);

  for (i = 0;  i < npts;  i++)
    {
      Daubechies_inverse_pass_1d (n, s[i]);
    }

  c = (float *) malloc (sizeof(float) * npts);

  for (j = 0;  j < npts;  j++)
    {
      for (i = 0;  i < npts;  i++)
	c[i] = s[i][j];
      Daubechies_inverse_pass_1d (n, c);
      for (i = 0;  i < npts;  i++)
	s[i][j] = c[i];
    }

  free (c);   c = NULL;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the Daubechies inverse fast wavelet transform in 2-dimensions.
*/

void Daubechies_inverse_FWT_2d (int n, float ** s)
{
  int m;
  int npts;

  npts = powerof2 (n);

  for (m = 1;  m <= n;  m++)
    {
      Daubechies_inverse_pass_2d (m, s);
    }
}


/*---------------------------------------------------------------------------*/







