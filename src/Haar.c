/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/*
  This file contains routines for performing the Haar fast wavelet transform
  analysis of time series data.

  File:    Haar.c
  Author:  B. Douglas Ward
  Date:    28 March 2000

*/

/*---------------------------------------------------------------------------*/
/*
  Calculate Haar in-place forward fast wavelet transform in 1-dimension.
*/

void Haar_ip_FFWT_1d (int n, float * s)
{
  float a;
  float c;
  int i;
  int j;
  int k;
  int l;
  int m;


  i = 1;
  j = 2;
  m = powerof2 (n);

  for (l = n-1;  l >= 0;  l--)
    {
      printf ("l = %d \n", l);
      m /= 2;

      for (k = 0;  k < m;  k++)
	{
	  a = (s[j*k] + s[j*k+i]) / 2.0;
	  c = (s[j*k] - s[j*k+i]) / 2.0;
	  s[j*k] = a;
	  s[j*k+i] = c;
	}
      
      i *= 2;
      j *= 2;
      /*
      ts_print (powerof2(n), s);
      */
    }
}


/*---------------------------------------------------------------------------*/
/*
  Calculate Haar in-place inverse fast wavelet transform in 1-dimension.
*/

void Haar_ip_IFWT_1d (int n, float * s)
{
  float a0;
  float a1;
  int i;
  int j;
  int k;
  int l;
  int m;


  i = powerof2 (n-1);
  j = 2*i;
  m = 1;

  for (l = 1;  l <= n;  l++)
    {
      printf ("l = %d \n", l);

      for (k = 0;  k < m;  k++)
	{
	  a0 = s[j*k] + s[j*k+i];
	  a1 = s[j*k] - s[j*k+i];
	  s[j*k] = a0;
	  s[j*k+i] = a1;
	}
      
      i /= 2;
      j /= 2;
      m *= 2;
      /*
      ts_print (powerof2(n), s);
      */
    }
}


/*---------------------------------------------------------------------------*/
/*
  Calculate one iteration of the Haar forward FWT in 1-dimension.
*/

void Haar_forward_pass_1d (int n, float * s)
{
  int i;
  int npts;
  float * a = NULL;
  float * c = NULL;

  npts = powerof2 (n);
  a = (float *) malloc (sizeof(float) * npts/2);
  c = (float *) malloc (sizeof(float) * npts/2);
  
  for (i = 0;  i < npts/2;  i++)
    {
      a[i] = (s[2*i] + s[2*i+1]) / 2.0;
      c[i] = (s[2*i] - s[2*i+1]) / 2.0;
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
  Calculate the Haar forward fast wavelet transform in 1-dimension.
*/

void Haar_forward_FWT_1d (int n, float * s)
{
  int m;
  int npts;

  npts = powerof2 (n);

  for (m = n-1;  m >= 0;  m--)
    {
      Haar_forward_pass_1d (m+1, s);
      /*
      ts_print (npts, s);
      */
    }
}


/*---------------------------------------------------------------------------*/
/*
  Calculate one iteration of the Haar inverse FWT in 1-dimension.
*/

void Haar_inverse_pass_1d (int n, float * s)
{
  int i;
  int npts;
  float * r = NULL;

  npts = powerof2 (n);
  r = (float *) malloc (sizeof(float) * npts);
  
  for (i = 0;  i < npts/2;  i++)
    {
      r[2*i]   = s[i] + s[i + npts/2];
      r[2*i+1] = s[i] - s[i + npts/2]; 
    }

  for (i = 0;  i < npts;  i++)
    {
      s[i] = r[i];
    }

  free (r);   r = NULL;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the Haar inverse fast wavelet transform in 1-dimension.
*/

void Haar_inverse_FWT_1d (int n, float * s)
{
  int m;
  int npts;

  npts = powerof2 (n);

  for (m = 1;  m <=n;  m++)
    {
      Haar_inverse_pass_1d (m, s);
      /*
      ts_print (npts, s);
      */
    }
}


/*---------------------------------------------------------------------------*/
/*
  Calculate one iteration of the Haar forward FWT in 2-dimensions.
*/

void Haar_forward_pass_2d (int n, float ** s)
{
  int i, j;
  int npts;
  float * c = NULL;


  npts = powerof2 (n);

  for (i = 0;  i < npts;  i++)
    {
      Haar_forward_pass_1d (n, s[i]);
    }

  c = (float *) malloc (sizeof(float) * npts);

  for (j = 0;  j < npts;  j++)
    {
      for (i = 0;  i < npts;  i++)
	c[i] = s[i][j];
      Haar_forward_pass_1d (n, c);
      for (i = 0;  i < npts;  i++)
	s[i][j] = c[i];
    }

  free (c);   c = NULL;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the Haar forward fast wavelet transform in 2-dimensions.
*/

void Haar_forward_FWT_2d (int n, float ** s)
{
  int m;
  int npts;

  npts = powerof2 (n);

  for (m = n-1;  m >= 0;  m--)
    {
      Haar_forward_pass_2d (m+1, s);
    }
}


/*---------------------------------------------------------------------------*/
/*
  Calculate one iteration of the Haar inverse FWT in 2-dimensions.
*/

void Haar_inverse_pass_2d (int n, float ** s)
{
  int i, j;
  int npts;
  float * c = NULL;


  npts = powerof2 (n);

  for (i = 0;  i < npts;  i++)
    {
      Haar_inverse_pass_1d (n, s[i]);
    }

  c = (float *) malloc (sizeof(float) * npts);

  for (j = 0;  j < npts;  j++)
    {
      for (i = 0;  i < npts;  i++)
	c[i] = s[i][j];
      Haar_inverse_pass_1d (n, c);
      for (i = 0;  i < npts;  i++)
	s[i][j] = c[i];
    }

  free (c);   c = NULL;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the Haar inverse fast wavelet transform in 2-dimensions.
*/

void Haar_inverse_FWT_2d (int n, float ** s)
{
  int m;
  int npts;

  npts = powerof2 (n);

  for (m = 1;  m <= n;  m++)
    {
      Haar_inverse_pass_2d (m, s);
    }
}


/*---------------------------------------------------------------------------*/



