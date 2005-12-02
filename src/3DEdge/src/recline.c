/*************************************************************************
 * recline.c - tools for recursive filtering of 1D lines
 *
 * $Id$
 *
 * Copyright©INRIA 1999
 *
 * DESCRIPTION: 
 *
 * Recursive filtering of a line (a 1D array)
 * Filter coefficient are static variables.
 *
 *
 * AUTHOR:
 * Gregoire Malandain (greg@sophia.inria.fr)
 * 
 * CREATION DATE: 
 * June, 9 1998
 *
 * Copyright Gregoire Malandain, INRIA
 *
 * ADDITIONS, CHANGES
 *
 */

#include <recline.h>

static int _VERBOSE_ = 0;

#define EXIT_ON_FAILURE 0
#define EXIT_ON_SUCCESS 1

/*--- denominateur       ---*/
static double sd1 = 0.0, sd2 = 0.0, sd3 = 0.0, sd4 = 0.0;
/*--- numerateur positif ---*/
static double sp0 = 0.0, sp1 = 0.0, sp2 = 0.0, sp3 = 0.0;
/*--- numerateur negatif ---*/
static double sn0 = 0.0, sn1 = 0.0, sn2 = 0.0, sn3 = 0.0, sn4 = 0.0;
/*--- type de filtre en cours ---*/
static recursiveFilterType static_type_filter = UNKNOWN_FILTER;
static derivativeOrder static_derivative = NODERIVATIVE;

void InitRecursiveCoefficients( double x, 
				recursiveFilterType type_filter, 
				derivativeOrder derivative )
{
  char *proc="InitRecursiveCoefficients";
  double ex, k1, k2;
  double a0, a1, c0, c1, omega0, omega1, b0, b1;
  double cos0, sin0, cos1, sin1;
  double sumA=0.0, sumC=0.0, aux;
  
  sd1 = sd2 = sd3 = sd4 = 0.0;
  sp0 = sp1 = sp2 = sp3 = 0.0;
  sn0 = sn1 = sn2 = sn3 = sn4 = 0.0;
  
  static_type_filter = UNKNOWN_FILTER;
  static_derivative  = NODERIVATIVE;
  
  ex = k1 = k2 = 0.0;
  a0 = a1 = c0 = c1 = 0.0;
  b0 = b1 = omega0 = omega1 = 0.0;
  
  /*--- Selon le type de filtrage (filtres de Deriche,
    ou approximation de la gaussienne), x designe
    soit alpha, soit sigma                         ---*/
  
  switch ( type_filter ) {

  case GAUSSIAN_DERICHE :
    
    if ( x < 0.1 ) {
      if ( _VERBOSE_ != 0 ) {
	fprintf( stderr, "%s: improper value of coefficient (should be >= 0.1).\n", proc );
      }
      return;
    }

    switch ( derivative ) {
    default :
      if ( _VERBOSE_ != 0 ) {
	fprintf( stderr, "%s: switch to default coefficients (smoothing).\n", proc );
      }
      derivative = DERIVATIVE_0;
    case DERIVATIVE_0 :
      a0     =  1.68;
      omega0 =  0.6318;
      a1     =  3.735;
      b0     =  1.783;
      c0     = -0.6803;
      omega1 =  1.997;
      c1     = -0.2598;
      b1     =  1.723;
      break;
    case DERIVATIVE_1 :
    case DERIVATIVE_1_CONTOURS :
      a0     =  -0.6472;
      omega0 =  0.6719;
      a1     =  -4.531;
      b0     =  1.527;
      c0     =  0.6494;
      omega1 =  2.072;
      c1     =  0.9557;
      b1     =  1.516;
      break;
    case DERIVATIVE_2 :
      a0     = -1.331;
      omega0 =  0.748;
      a1     =  3.661;
      b0     =  1.24;
      c0     =  0.3225;
      omega1 =  2.166;
      c1     = -1.738;
      b1     =  1.314;
    }
	 
    omega0 /= x;   sin0 = sin( omega0 );   cos0 = cos( omega0 ); 
    omega1 /= x;   sin1 = sin( omega1 );   cos1 = cos( omega1 ); 
    b0 /= x;
    b1 /= x;

    /*--- normalisation ---*/
    switch ( derivative ) {
    default :
    case DERIVATIVE_0 :
      sumA  = 2.0 * a1 * exp( b0 ) * cos0 * cos0 - a0 * sin0 * exp( 2.0 * b0 );
      sumA += a0 * sin0 - 2.0 * a1 * exp( b0 );
      sumA /= ( 2.0 * cos0 * exp( b0 ) - exp( 2.0 * b0 ) - 1 ) * sin0;
      sumC  = 2.0 * c1 * exp( b1 ) * cos1 * cos1 - c0 * sin1 * exp( 2.0 * b1 );
      sumC += c0 * sin1 - 2.0 * c1 * exp( b1 );
      sumC /= ( 2.0 * cos1 * exp( b1 ) - exp( 2.0 * b1 ) - 1 ) * sin1;
      break;
    case DERIVATIVE_1 :
      aux   = exp( 4.0 * b0 ) - 4.0 * cos0 * exp( 3.0 * b0 );
      aux  += 2.0 * exp( 2.0 * b0 ) + 4.0 * cos0 * cos0 * exp( 2.0 * b0 );
      aux  += 1.0 - 4.0 * cos0 * exp( b0 );
      sumA  = a0 * cos0 - a1 * sin0 + a1 * sin0 * exp( 2.0 * b0 );
      sumA += a0 * cos0 * exp( 2.0 * b0 ) - 2.0 * a0 * exp( b0 );
      sumA *= exp( b0 ) / aux;
      aux   = exp( 4.0 * b1 ) - 4.0 * cos1 * exp( 3.0 * b1 );
      aux  += 2.0 * exp( 2.0 * b1 ) + 4.0 * cos1 * cos1 * exp( 2.0 * b1 );
      aux  += 1.0 - 4.0 * cos1 * exp( b1 );
      sumC  = c0 * cos1 - c1 * sin1 + c1 * sin1 * exp( 2.0 * b1 );
      sumC += c0 * cos1 * exp( 2.0 * b1 ) - 2.0 * c0 * exp( b1 );
      sumC *= exp( b1 ) / aux;
      /*--- on multiplie les sommes par 2 car on n'a calcule que des demi-sommes 
	et on change le signe car la somme doit etre egale a -1              ---*/
      sumA *= (-2.0);
      sumC *= (-2.0);
      break;
    case DERIVATIVE_1_CONTOURS :
      /*--- la somme de 1 a l'infini est egale a 1 : cela introduit
	un petit biais (reponse un rien superieur a la hauteur du step).
	Avec une somme de 0 a l'infini, c'est pire                       ---*/
      sumA  = a1 * exp( b0 ) - a1 * cos0 * cos0 * exp( b0 );
      sumA += a0 * cos0 * sin0 * exp( b0 ) - a0 * sin0;
      sumA /= sin0 * ( 2.0 * cos0 * exp( b0 ) - exp( 2.0 * b0 ) - 1 );
      sumC  = c1 * exp( b1 ) - c1 * cos1 * cos1 * exp( b1 );
      sumC += c0 * cos1 * sin1 * exp( b1 ) - c0 * sin1;
      sumC /= sin1 * ( 2.0 * cos1 * exp( b1 ) - exp( 2.0 * b1 ) - 1 );
      break;
    case DERIVATIVE_2 :
      aux   = 12.0 * cos0 * exp( 3.0 * b0 ) - 3.0 * exp( 2.0 * b0 );
      aux  += 8.0 * cos0 * cos0 * cos0 * exp( 3.0 * b0 ) - 12.0 * cos0 * cos0 * exp( 4.0 * b0 );
      aux  -= 3.0 * exp( 4.0 * b0 );
      aux  += 6.0 * cos0 * exp( 5.0 * b0 ) -  exp( 6.0 * b0 ) + 6.0 * cos0 * exp( b0 );
      aux  -= ( 1.0 + 12.0 * cos0 * cos0 * exp( 2.0 * b0 ) );
      sumA  = 4.0 * a0 * sin0 * exp( 3.0 * b0 ) + a1 * cos0 * cos0 * exp( 4.0 * b0 );
      sumA -= ( 4.0 * a0 * sin0 * exp( b0 ) + 6.0 * a1 * cos0 * cos0 * exp( 2.0 * b0 ) );
      sumA += 2.0 * a1 * cos0 * cos0 * cos0 * exp( b0 ) - 2.0 * a1 * cos0 * exp( b0 );
      sumA += 2.0 * a1 * cos0 * cos0 * cos0 * exp( 3.0 * b0 ) - 2.0 * a1 * cos0 * exp( 3.0 * b0 );
      sumA += a1 * cos0 * cos0 - a1 * exp( 4.0 * b0 );
      sumA += 2.0 * a0 * sin0 * cos0 * cos0 * exp( b0 ) - 2.0 * a0 * sin0 * cos0 * cos0 * exp( 3.0 * b0 );
      sumA -= ( a0 * sin0 * cos0 * exp( 4.0 * b0 ) + a1 );
      sumA += 6.0 * a1 * exp( 2.0 * b0 ) + a0 * cos0 * sin0;
      sumA *= 2.0 * exp( b0 ) / ( aux * sin0 );
      aux   = 12.0 * cos1 * exp( 3.0 * b1 ) - 3.0 * exp( 2.0 * b1 );
      aux  += 8.0 * cos1 * cos1 * cos1 * exp( 3.0 * b1 ) - 12.0 * cos1 * cos1 * exp( 4.0 * b1 );
      aux  -= 3.0 * exp( 4.0 * b1 );
      aux  += 6.0 * cos1 * exp( 5.0 * b1 ) -  exp( 6.0 * b1 ) + 6.0 * cos1 * exp( b1 );
      aux  -= ( 1.0 + 12.0 * cos1 * cos1 * exp( 2.0 * b1 ) );
      sumC  = 4.0 * c0 * sin1 * exp( 3.0 * b1 ) + c1 * cos1 * cos1 * exp( 4.0 * b1 );
      sumC -= ( 4.0 * c0 * sin1 * exp( b1 ) + 6.0 * c1 * cos1 * cos1 * exp( 2.0 * b1 ) );
      sumC += 2.0 * c1 * cos1 * cos1 * cos1 * exp( b1 ) - 2.0 * c1 * cos1 * exp( b1 );
      sumC += 2.0 * c1 * cos1 * cos1 * cos1 * exp( 3.0 * b1 ) - 2.0 * c1 * cos1 * exp( 3.0 * b1 );
      sumC += c1 * cos1 * cos1 - c1 * exp( 4.0 * b1 );
      sumC += 2.0 * c0 * sin1 * cos1 * cos1 * exp( b1 ) - 2.0 * c0 * sin1 * cos1 * cos1 * exp( 3.0 * b1 );
      sumC -= ( c0 * sin1 * cos1 * exp( 4.0 * b1 ) + c1 );
      sumC += 6.0 * c1 * exp( 2.0 * b1 ) + c0 * cos1 * sin1;
      sumC *= 2.0 * exp( b1 ) / ( aux * sin1 );
      /*--- on divise les sommes par 2 (la somme doit etre egale a 2) ---*/
      sumA /= 2;
      sumC /= 2;
    }
    a0 /= ( sumA + sumC );
    a1 /= ( sumA + sumC );
    c0 /= ( sumA + sumC );
    c1 /= ( sumA + sumC );
    
    /*--- coefficients du calcul recursif ---*/
    sp0  = a0 + c0;
    sp1  = exp( -b1 ) * (c1 * sin1 - (c0 + 2 * a0) * cos1);
    sp1 += exp( -b0 ) * (a1 * sin0 - (2 * c0 + a0) * cos0);
    sp2  = 2.0 * exp( -b0 - b1 ) * ((a0 + c0) * cos1 * cos0 - cos1 * a1 * sin0 - cos0 * c1 * sin1);
    sp2 += c0 * exp( -2.0 * b0 ) + a0 * exp( -2.0 * b1 );
    sp3  = exp( -b1 - 2.0 * b0 ) * (c1 * sin1 - c0 * cos1);
    sp3 += exp( -b0 - 2.0 * b1 ) * (a1 * sin0 - a0 * cos0);
    
    sd1  = -2.0 * exp( -b1 ) * cos1 - 2.0 * exp( -b0 ) * cos0;
    sd2  = 4.0 * cos1 * cos0 * exp( -b0 - b1 ) + exp( -2.0 * b1 ) + exp( -2.0 * b0 );
    sd3 = -2.0 * cos0 * exp( -b0 - 2.0 * b1 ) - 2.0 * cos1 * exp( -b1 - 2.0 * b0 );
    sd4 = exp( -2.0 * b0 - 2.0 * b1 );
    
    switch ( derivative ) {
    default :
    case DERIVATIVE_0 :
    case DERIVATIVE_2 :
      sn1 = sp1 - sd1 * sp0;
      sn2 = sp2 - sd2 * sp0;
      sn3 = sp3 - sd3 * sp0;
      sn4 = -sd4 * sp0;
      break;
    case DERIVATIVE_1 :
    case DERIVATIVE_1_CONTOURS :
    case DERIVATIVE_3 :
      sn1 = - sp1 + sd1 * sp0;
      sn2 = - sp2 + sd2 * sp0;
      sn3 = - sp3 + sd3 * sp0;
      sn4 = sd4 * sp0;
    }
    
    static_type_filter = type_filter;
    static_derivative  = derivative;
    break;



  default :
    if ( _VERBOSE_ != 0 ) {
      fprintf( stderr, "%s: switch to default recursive filter (Deriche's filters).\n", proc );
    }
    type_filter = ALPHA_DERICHE;
  case ALPHA_DERICHE :

    if ( (x < 0.1) || (x > 1.9) ) {
      if ( _VERBOSE_ != 0 ) {
	fprintf( stderr, "%s: improper value of coefficient (should be >= 0.1 and <= 1.9).\n", proc );
      }
      return;
    }
    ex = exp( (-x) );
    
    switch ( derivative ) {
    default :
      if ( _VERBOSE_ != 0 ) {
	fprintf( stderr, "%s: switch to default coefficients (smoothing).\n", proc );
      }
      derivative = DERIVATIVE_0;
    case DERIVATIVE_0 :
      sp0 = (1.0 - ex) * (1.0 - ex) / (1.0 + 2.0 * x * ex - ex * ex);
      sp1 = sp0 * (x - 1.0) * ex;
      sn1 = sp0 * (x + 1.0) * ex;
      sn2 = (- sp0) * ex * ex;
      sd1 = (- 2.0) * ex;
      sd2 = ex * ex;
      break;
    case DERIVATIVE_1 :
      sp1 = - (1.0 - ex) * (1.0 - ex) * (1.0 - ex) / (2.0 * (1.0 + ex));
      sn1 = (- sp1);
      sd1 = (- 2.0) * ex;
      sd2 = ex * ex;	    
      break;
    case DERIVATIVE_1_CONTOURS :
      sp1 = - (1.0 - ex) * (1.0 - ex);
      sn1 = (- sp1);
      sd1 = (- 2.0) * ex;
      sd2 = ex * ex;	    
      break;
    case DERIVATIVE_2 :
      k1 = (- 2.0) * (1.0 - ex) * (1.0 - ex) * (1.0 - ex);
      k1 /= (1.0 + ex) * (1.0 + ex) * (1.0 + ex);
      k2 = (1.0 - ex * ex) / (2.0 * ex);
      sp0 = k1;
      sp1 = (- k1) * (1.0 + k2) * ex;
      sn1 = k1 * (1.0 - k2) * ex;
      sn2 = (- k1) * ex * ex;
      sd1 = (- 2.0) * ex;
      sd2 = ex * ex;
      break;
    case DERIVATIVE_3 :
      k1 = (1.0 + x) * ex + (x - 1.0);
      k2 = (1.0 - ex) / k1;
      k1 *= (1.0 - ex) * (1.0 - ex) * (1.0 - ex) * (1.0 - ex);
      k1 /= 2.0 * x * x * ex * ex;
      k1 /= ex + 1.0;
      sp0 = k1 * x * (k2 + 1.0);
      sp1 = (- k1) * x * (1.0 + k2 + k2*x) * ex;
      sn0 = (- sp0);
      sn1 = (- sp1);
      sd1 = (- 2.0) * ex;
      sd2 = ex * ex;
    }
    static_type_filter = type_filter;
    static_derivative  = derivative;
  }
}

int RecursiveFilter1D( double *in, 
		       double *out, 
		       double *work1, 
		       double *work2, 
		       int dim )
{
  char *proc="RecursiveFilter1D";
  register double rp0, rp1, rp2, rp3;
  register double rd1, rd2, rd3, rd4;
  register double rn0, rn1, rn2, rn3, rn4;
  register int i;
  register double *w0, *w1, *w2, *w3, *w4;
  register double *d0, *d1, *d2, *d3, *d4;

  if ( static_type_filter == UNKNOWN_FILTER ) {
    if ( _VERBOSE_ != 0 )
      fprintf( stderr, "%s: unknown type of recursive filter.\n", proc );
    return( EXIT_ON_FAILURE );
  }
  if ( static_derivative == NODERIVATIVE ) {
    if ( _VERBOSE_ != 0 )
      fprintf( stderr, "%s: unknown type of derivative.\n", proc );
    return( EXIT_ON_FAILURE );
  }

  rd1 = rd2 = rd3 = rd4 = 0.0;
  rp0 = rp1 = rp2 = rp3 = 0.0;
  rn0 = rn1 = rn2 = rn3 = rn4 = 0.0;
  
  switch( static_type_filter ) {
  case GAUSSIAN_DERICHE :
    /*--- filtrage generique d'ordre 4 ---*/
    rp0 = sp0;   rp1 = sp1;   rp2 = sp2;   rp3 = sp3;
    rd1 = sd1;   rd2 = sd2;   rd3 = sd3;   rd4 = sd4;
    rn1 = sn1;   rn2 = sn2;   rn3 = sn3;   rn4 = sn4;
    
    /* on positionne les pointeurs 
     */
    w4 = work1;   w3 = w4+1;   w2 = w3+1;   w1 = w2+1;   w0 = w1+1;
    d3 = in+1;    d2 = d3+1;   d1 = d2+1;   d0 = d1+1;
    /*--- calcul de y+ ---*/
    *(w4) = rp0 * *(in);
    *(w3) = rp0 * *(d3) + rp1 * *(in)
          - rd1 * *(w4);   
    *(w2) = rp0 * *(d2) + rp1 * *(d3) + rp2 * *(in)
          - rd1 * *(w3) - rd2 * *(w4);
    *(w1) = rp0 * *(d1) + rp1 * *(d2) + rp2 * *(d3) + rp3 * *(in)
          - rd1 * *(w2) - rd2 * *(w3) - rd3 * *(w4);
    for (i=4; i<dim; i++,w0++,w1++,w2++,w3++,w4++,d0++,d1++,d2++,d3++) 
      *(w0) = rp0 * *(d0) + rp1 * *(d1) + rp2 * *(d2) + rp3 * *(d3)
            - rd1 * *(w1) - rd2 * *(w2) - rd3 * *(w3) - rd4 * *(w4);
    
    /* on positionne les pointeurs 
     */
    w4 = work2+dim-1;   w3 = w4-1;   w2 = w3-1;   w1 = w2-1;   w0 = w1-1;
    d4 = in+dim-1;      d3 = d4-1;   d2 = d3-1;   d1 = d2-1;
    /*--- calcul de y- ---*/
    *(w4) = 0;
    *(w3) = rn1 * *(d4);
    *(w2) = rn1 * *(d3) + rn2 * *(d4) 
          - rd1 * *(w3);
    *(w1) = rn1 * *(d2) + rn2 * *(d3) + rn3 * *(d4) 
          - rd1 * *(w2) - rd2 * *(w3);
    for (i=dim-5; i>=0; i--,w0--,w1--,w2--,w3--,w4--,d1--,d2--,d3--,d4--)
      *(w0) = rn1 * *(d1) + rn2 * *(d2) + rn3 * *(d3) + rn4 * *(d4)
	    - rd1 * *(w1) - rd2 * *(w2) - rd3 * *(w3) - rd4 * *(w4);

    /*--- calcul final ---*/
    w1 = work1;   w2 = work2;   d0 = out;
    for (i=0 ; i<dim ; i++,w1++,w2++,d0++)
      *d0 = *w1 + *w2;
    
    break;

  default :
  case ALPHA_DERICHE :
   
    switch( static_derivative ) {
    default :
    case DERIVATIVE_0 :
    case DERIVATIVE_2 :

      rp0 = sp0;   rp1 = sp1;
      rd1 = sd1;   rd2 = sd2;
      rn1 = sn1;   rn2 = sn2;
      
      /* on positionne les pointeurs 
       */
      w2 = work1;   w1 = w2+1;   w0 = w1+1;
      d1 = in+1;    d0 = d1+1;
      /*--- calcul de y+ ---*/
      *(w2) = rp0 * *(in);
      *(w1) = rp0 * *(d1) + rp1 * *(in) 
	    - rd1 * *(w2);     
      for (i=2;  i<dim; i++,w0++,w1++,w2++,d0++,d1++)
	*(w0) = rp0 * *(d0) + rp1 * *(d1)
	      - rd1 * *(w1) - rd2 * *(w2);
      
      w2 = work2+dim-1;   w1 = w2-1;   w0 = w1-1;
      d2 = in+dim-1;      d1 = d2-1;
      /*--- calcul de y- ---*/
      *(w2) = 0.0;
      *(w1) = rn1 * *(d2);
      for (i=dim-3; i>=0; i--,w0--,w1--,w2--,d1--,d2--)
	*(w0) = rn1 * *(d1) + rn2 * *(d2)
	      - rd1 * *(w1) - rd2 * *(w2);
      
      /*--- calcul final ---*/
      w1 = work1;   w2 = work2;   d0 = out;
      for (i=0 ; i<dim ; i++,w1++,w2++,d0++)
	*d0 = *w1 + *w2;
      
      break;
      
    case DERIVATIVE_1 :
    case DERIVATIVE_1_CONTOURS :
      rp1 = sp1;
      rn1 = sn1;
      rd1 = sd1;   rd2 = sd2;
      
      /* on positionne les pointeurs 
       */
      w2 = work1;   w1 = w2+1;   w0 = w1+1;
      d1 = in+1;
      /*--- calcul de y+ ---*/
      *(w2) = 0.0;
      *(w1) = rp1 * *(in);     
      for (i=2;  i<dim; i++,w0++,w1++,w2++,d1++)
	*(w0) = rp1 * *(d1)
	      - rd1 * *(w1) - rd2 * *(w2);
      
      
      w2 = work2+dim-1;   w1 = w2-1;   w0 = w1-1;
      d2 = in+dim-1;      d1 = d2-1;
      /*--- calcul de y- ---*/
      *(w2) = 0.0;
      *(w1) = rn1 * *(d2);
      for (i=dim-3; i>=0; i--,w0--,w1--,w2--,d1--)
	*(w0) = rn1 * *(d1)
	      - rd1 * *(w1) - rd2 * *(w2);
      
      /*--- calcul final ---*/
      w1 = work1;   w2 = work2;   d0 = out;
      for (i=0 ; i<dim ; i++,w1++,w2++,d0++)
	*d0 = *w1 + *w2;
      
      break;

    case DERIVATIVE_3 :
      rp0 = sp0;   rp1 = sp1;
      rd1 = sd1;   rd2 = sd2;
      rn0 = sn0;   rn1 = sn1;
      
      w2 = work1;   w1 = w2+1;   w0 = w1+1;
      d1 = in+1;   d0 = d1+1;
      /*--- calcul de y+ ---*/
      *(w2) = rp0 * *(in);
      *(w1) = rp0 * *(d1) + rp1 * *(in) 
	    - rd1 * *(w2);     
      for (i=2;  i<dim; i++,w0++,w1++,w2++,d0++,d1++)
	*(w0) = rp0 * *(d0) + rp1 * *(d1)
	      - rd1 * *(w1) - rd2 * *(w2);
      
      w2 = work2+dim-1;   w1 = w2-1;   w0 = w1-1;
      d2 = in+dim-1;      d1 = d2-1;   d0 = d1-1;
      /*--- calcul de y- ---*/
      *(w2) = rn0 * *(d2);
      *(w1) = rn0 * *(d1) + rn1 * *(d2) 
	    - rd1 * *(w2);
      for (i=dim-3; i>=0; i--,w0--,w1--,w2--,d0--,d1--)
	*(w0) = rn0 * *(d0) + rn1 * *(d1)
	      - rd1 * *(w1) - rd2 * *(w2);
      
      /*--- calcul final ---*/
      w1 = work1;   w2 = work2;   d0 = out;
      for (i=0 ; i<dim ; i++,w1++,w2++,d0++)
	*d0 = *w1 + *w2;
      
    }
  }
  return( EXIT_ON_SUCCESS );
}

void Recline_verbose ( )
{
  _VERBOSE_ = 1;
}
void Recline_noverbose ( )
{
  _VERBOSE_ = 0;
}
