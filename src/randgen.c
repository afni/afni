/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  Some utility routines for generating random numbers.

  File:    randgen.c
  Author:  B. Douglas Ward
  Date:    15 February 1999

  Mod:     Correction to rand_normal
  Date:    15 September 1999

  Mod:     Added routine rand_initialize.
  Date:    28 January 2000
*/


/*---------------------------------------------------------------------------*/
/*
  Initialize the random number generator.
*/

void rand_initialize (long int seedval)
{
  srand48 (seedval);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to generate a uniform U(a,b) random variate.
*/

float rand_uniform (float a, float b)
{
  return (a + (float)drand48() * (b-a) );
}


/*---------------------------------------------------------------------------*/
/*
  Routine to generate a normal N(mu,var) random variate.
*/

float rand_normal (float mu, float var)
{
  float u1, u2;
  float r, n;


  u1 = 0.0;
  while (u1 <= 0.0)
    {
      u1 = rand_uniform (0.0, 1.0);
    }
  u2 = rand_uniform (0.0, 1.0);

  r = sqrt(-2.0*log(u1));
  n = r * cos(2.0*PI*u2);

  return (mu + n * sqrt(var));
}


/*---------------------------------------------------------------------------*/
/*
  Routine to generate two independent normal N(mu,var) random variates.
*/

void rand_binormal (float mu, float var, float * n1, float * n2)
{
  float u1, u2;
  float r, sigma;


  u1 = 0.0;
  while (u1 <= 0.0)
    {
      u1 = rand_uniform (0.0, 1.0);
    }
  u2 = rand_uniform (0.0, 1.0);

  r   = sqrt(-2.0*log(u1));
  sigma = sqrt (var);

  *n1 = mu + r * cos(2.0*PI*u2) * sigma;
  *n2 = mu + r * sin(2.0*PI*u2) * sigma;
}


/*---------------------------------------------------------------------------*/



