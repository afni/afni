#ifndef _LSMOOD_HEADER_
#define _LSMOOD_HEADER_


static long lmaxarg1, lmaxarg2;
#define LMAX(a,b) (lmaxarg1 = (a),lmaxarg2 = (b), (lmaxarg1 > lmaxarg2 ? \
                                                   lmaxarg1 : lmaxarg2))
static long lminarg1, lminarg2;
#define LMIN(a,b) (lminarg1 = (a),lminarg2 = (b), (lminarg1 < lminarg2 ? \
                                                   lminarg1 : lminarg2))
#define MOD(a,b)	while (a >= b) a -= b
#define MACC 4
#define moody_SIGN(a,b) ((b) > 0.0 ? fabs(a) : -fabs(a))
static float sqrarg;
#define moody_SQR(a) ((sqrarg = (a)) == 0.0 ? 0.0 : sqrarg*sqrarg)
#define SWAP(a,b) tempr=(a);(a)=(b);(b)=tempr

void LS_calc_workspace_size( unsigned long n,
                             float ofac, 
                             float hifac,
                             unsigned long *nout,
                             unsigned long *ndim);

/*
  see LS_moody.c file for more info...

  The following LS implementation is from the following publicly
  available code by G. Moody:
  https://www.physionet.org/physiotools/wfdb/psd/lomb.c
*/
void fasper( float x[], float y[],
             unsigned long n,
             float ofac, float hifac, float wk1[], float wk2[],
             unsigned long nwk, unsigned long nout, 
             unsigned long *jmax,
             float *prob) ;
void spread( float y, float yy[],
             unsigned long n,
             float x,
             int m);
void avevar( float data[],
             unsigned long n,
             float *ave, float *var);
void realft( float data[],
             unsigned long n,
             int isign);
void four1( float data[],
            unsigned long nn,
            int isign);
void zeromean(float *y, unsigned long n);
void moody_error(char *s);









#endif /* _LSMOOD_HEADER_ */
