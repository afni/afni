/*********************************/
/* Principal Components Analysis */
/*********************************/

/*** Modified 1993, 1994 by RW Cox.
     Now reads input matrix from file, instead of data vectors.
***/

/*********************************************************************/
/* Principal Components Analysis or the Karhunen-Loeve expansion is a
   classical method for dimensionality reduction or exploratory data
   analysis.  One reference among many is: F. Murtagh and A. Heck,
   Multivariate Data Analysis, Kluwer Academic, Dordrecht, 1987.

   Author: F. Murtagh Phone:        + 49 89 32006298 (work) + 49 89
                 965307 (home) Earn/Bitnet:  fionn@dgaeso51,
   fim@dgaipp1s,  murtagh@stsci Span:         esomc1::fionn Internet:
   murtagh@scivax.stsci.edu

   F. Murtagh, Munich, 6 June 1989                                   */
/*********************************************************************/

#include <stdio.h>
#include <string.h>
#include <math.h>

#define SIGN(a, b) ( (b) < 0 ? -fabs(a) : fabs(a) )

/**************************************************************************/

void vector_write( char *fff , int nvec , float *vvv )
{
   FILE *fffile ;
   float scale , va ;
   int ii , vi ;

   fffile = fopen( fff , "w" ) ;
   if( fffile == NULL ){
      fprintf( stderr , "cannot open output file %s\n" , fff ) ;
      exit(1) ;
   }
   scale = 0.0 ;
   for( ii=1 ; ii <= nvec ; ii++ ){
      va = fabs(vvv[ii]) ;
      if( va > scale ) scale = va ;
   }
   if( scale > 0.0 ) scale = 10000.0 / scale ;
   for( ii=1 ; ii <= nvec ; ii++ ){
      vi = scale * vvv[ii] + 0.499 ;
      fprintf( fffile , "%6d\n" , vi ) ;
   }
   fclose( fffile ) ;
   return ;
}

/**************************************************************************/

main(argc, argv)
int argc;
char *argv[];

{
FILE *stream;
int  n, m,  i, j, k, k2;
float **data, **matrix(), **symmat, **symmat2, *vector(), *evals, *interm;
void free_matrix(), free_vector(), corcol(), covcol(), scpcol();
void tred2(), tqli();
float in_value;
char option, *strncpy();

int ii, jbot ;
float *vsum ;
double dtemp ;   /* entries on disk are doubles */

int cvtype ;
char prefix[64] = "pcmat" , fname[128] ;

/*********************************************************************
   Get from command line:
   covmat_filename option
*********************************************************************/

   if (argc < 3)
      {
         fprintf( stderr ,
       "Usage: %s covfile option [prefix] \n  where option=R,V,S\n",argv[0]);
         exit(1) ;
      }

   if( argc >= 4 ){
      strcpy( prefix , argv[3] ) ;
   }

   stream = fopen( argv[1] , "r" ) ;
   if( stream == NULL ){fprintf(stderr,"can't open covfile\n");exit(1);}

#define COVERR(n) if(ii<(n)){fprintf(stderr,"read error\n");exit(1);}

   ii = fread( &n      , sizeof(int) , 1 , stream ) ; COVERR(1) ;
   ii = fread( &m      , sizeof(int) , 1 , stream ) ; COVERR(1) ;
   ii = fread( &cvtype , sizeof(int) , 1 , stream ) ; COVERR(1) ;

   option = argv[2][0] ;   /* Analysis option */

   printf("No. of rows: %d, no. of columns: %d.\n",n,m);
   printf("Input file: %s.\n",argv[1]);

   vsum = vector(m) ;

   for( i=1 ; i <= m ; i++ ){
      ii = fread( &dtemp , sizeof(double) , 1 , stream ) ; COVERR(1) ;
      vsum[i] = dtemp / n ;
   }

   strcpy( fname , prefix ) ;  strcat( fname , ".ave" ) ;
   vector_write( fname , m , vsum ) ;

   if( cvtype <= 1 ) exit(0) ;

   /********************************************************************/

   symmat = matrix(m, m);  /* Allocation of correlation (etc.) matrix */
   for( i=1 ; i <= m ; i++ ){
      for( j=1 ; j <= m ; j++ ){
         ii = fread( &dtemp , sizeof(double) , 1 , stream ) ; COVERR(1) ;
         symmat[i][j] = dtemp / n ;
      }
   }
   fclose( stream ) ;

   /* Look at analysis option; branch in accordance with this. */

   switch(option){
      case 'R':
      case 'r':
          printf("Analysis of correlations chosen.\n");
          corcol(vsum, n, m, symmat);
        break;
      case 'V':
      case 'v':
          printf("Analysis of variances-covariances chosen.\n");
          covcol(vsum, n, m, symmat);
        break;
      case 'S':
      case 's':
          printf("Analysis of sums-of-squares-cross-products");
          printf(" matrix chosen.\n");
          scpcol(vsum, n, m, symmat);
        break;
      default:
          printf("Option: %c\n",option);
          printf("For option, please type R, V, or S\n");
          printf("(upper or lower case).\n");
          exit(1);
        break;
   }

/*********************************************************************
    Eigen-reduction
**********************************************************************/

    /* Allocate storage for dummy and new vectors. */

    evals = vector(m);     /* Storage alloc. for vector of eigenvalues */
    interm = vector(m);    /* Storage alloc. for 'intermediate' vector */

    tred2(symmat, m, evals, interm);  /* Triangular decomposition */
    tqli(evals, interm, m, symmat);   /* Reduction of sym. trid. matrix */

    /* evals now contains the eigenvalues,
       columns of symmat now contain the associated eigenvectors. */

     printf("\nEigenvalues:\n");
     jbot = (m<10) ? 1 : m-9 ;
     for (j = m; j >= jbot; j--) {
       printf("%18.5f\n", evals[j]);
     }

     for( i=1 ; i <= 3 ; i++ ){
        for( j=1 ; j <= m ; j++ ){
           interm[j] = symmat[j][m-i+1] ;
        }
        sprintf( fname , "%s.pc%d" , prefix , i ) ;
        vector_write( fname , m , interm ) ;
     }

   exit(0) ;
}

/**  Correlation matrix: creation  ***********************************/

void corcol(vsum, n, m, symmat)
float *vsum, **symmat;
int n, m;
{
float x, *mean, *stddev, *vector();
int i, j, j1, j2;
void covcol() ;

covcol( vsum, n , m , symmat ) ;  /* form covariance matrix first */

/* Allocate storage for mean and std. dev. vectors */

   stddev = vector(m);

   for (j = 1; j <= m; j++) stddev[j] = sqrt( symmat[j][j] ) ;

   for( i=1 ; i <= m ; i++){
      for( j=1 ; j <= m ; j++ ){
         symmat[i][j] /= (stddev[i]*stddev[j]) ;
      }
   }
   return;
}

/**  Covariance matrix: creation  *****************************/

void covcol(vsum, n, m, symmat)
float *vsum, **symmat;
int n, m;
{
float *mean, *vector();
int i, j, j1, j2;

   for( i=1 ; i <= m ; i++ ){
      for( j=1 ; j <= m ; j++ ){
         symmat[i][j] -= vsum[i] * vsum[j] ;
      }
   }
   return;
}

/**  Sums-of-squares-and-cross-products matrix: creation  **************/

void scpcol(data, n, m, symmat)
float **data, **symmat;
int n, m;
/* Create m * m sums-of-cross-products matrix from n * m data matrix. */
{
int i, j1, j2;

return;

}

/**  Error handler  **************************************************/

void erhand(err_msg)
char err_msg[];
/* Error handler */
{
    fprintf(stderr,"Run-time error:\n");
    fprintf(stderr,"%s\n", err_msg);
    fprintf(stderr,"Exiting to system.\n");
    exit(1);
}

/**  Allocation of vector storage  ***********************************/

float *vector(n)
int n;
/* Allocates a float vector with range [1..n]. */
{

    float *v;

    v = (float *) malloc ((unsigned) n*sizeof(float));
    if (!v) erhand("Allocation failure in vector().");
    return v-1;

}

/**  Allocation of float matrix storage  *****************************/

float **matrix(n,m)
int n, m;
/* Allocate a float matrix with range [1..n][1..m]. */
{
    int i;
    float **mat;

    /* Allocate pointers to rows. */
    mat = (float **) malloc((unsigned) (n)*sizeof(float*));
    if (!mat) erhand("Allocation failure 1 in matrix().");
    mat -= 1;

    /* Allocate rows and set pointers to them. */
    for (i = 1; i <= n; i++)
        {
        mat[i] = (float *) malloc((unsigned) (m)*sizeof(float));
        if (!mat[i]) erhand("Allocation failure 2 in matrix().");
        mat[i] -= 1;
        }

     /* Return pointer to array of pointers to rows. */
     return mat;

}

/**  Deallocate vector storage  *********************************/

void free_vector(v,n)
float *v;
int n;
/* Free a float vector allocated by vector(). */
{
   free((char*) (v+1));
}

/**  Deallocate float matrix storage  ***************************/

void free_matrix(mat,n,m)
float **mat;
int n, m;
/* Free a float matrix allocated by matrix(). */
{
   int i;

   for (i = n; i >= 1; i--)
       {
       free ((char*) (mat[i]+1));
       }
   free ((char*) (mat+1));
}

/**  Reduce a real, symmetric matrix to a symmetric, tridiag. matrix. */

void tred2(a, n, d, e)
float **a, *d, *e;
/* float **a, d[], e[]; */
int n;
/* Householder reduction of matrix a to tridiagonal form.
   Algorithm: Martin et al., Num. Math. 11, 181-195, 1968.
   Ref: Smith et al., Matrix Eigensystem Routines -- EISPACK Guide
        Springer-Verlag, 1976, pp. 489-494.
        W H Press et al., Numerical Recipes in C, Cambridge U P,
        1988, pp. 373-374.  */
{
int l, k, j, i;
float scale, hh, h, g, f;

for (i = n; i >= 2; i--)
    {
    l = i - 1;
    h = scale = 0.0;
    if (l > 1)
       {
       for (k = 1; k <= l; k++)
           scale += fabs(a[i][k]);
       if (scale == 0.0)
          e[i] = a[i][l];
       else
          {
          for (k = 1; k <= l; k++)
              {
              a[i][k] /= scale;
              h += a[i][k] * a[i][k];
              }
          f = a[i][l];
          g = f>0 ? -sqrt(h) : sqrt(h);
          e[i] = scale * g;
          h -= f * g;
          a[i][l] = f - g;
          f = 0.0;
          for (j = 1; j <= l; j++)
              {
              a[j][i] = a[i][j]/h;
              g = 0.0;
              for (k = 1; k <= j; k++)
                  g += a[j][k] * a[i][k];
              for (k = j+1; k <= l; k++)
                  g += a[k][j] * a[i][k];
              e[j] = g / h;
              f += e[j] * a[i][j];
              }
          hh = f / (h + h);
          for (j = 1; j <= l; j++)
              {
              f = a[i][j];
              e[j] = g = e[j] - hh * f;
              for (k = 1; k <= j; k++)
                  a[j][k] -= (f * e[k] + g * a[i][k]);
              }
         }
    }
    else
        e[i] = a[i][l];
    d[i] = h;
    }
d[1] = 0.0;
e[1] = 0.0;
for (i = 1; i <= n; i++)
    {
    l = i - 1;
    if (d[i])
       {
       for (j = 1; j <= l; j++)
           {
           g = 0.0;
           for (k = 1; k <= l; k++)
               g += a[i][k] * a[k][j];
           for (k = 1; k <= l; k++)
               a[k][j] -= g * a[k][i];
           }
       }
       d[i] = a[i][i];
       a[i][i] = 1.0;
       for (j = 1; j <= l; j++)
           a[j][i] = a[i][j] = 0.0;
    }
}

/**  Tridiagonal QL algorithm -- Implicit  **********************/

void tqli(d, e, n, z)
float d[], e[], **z;
int n;
{
int m, l, iter, i, k;
float s, r, p, g, f, dd, c, b;
void erhand();

for (i = 2; i <= n; i++)
    e[i-1] = e[i];
e[n] = 0.0;
for (l = 1; l <= n; l++)
    {
    iter = 0;
    do
      {
      for (m = l; m <= n-1; m++)
          {
          dd = fabs(d[m]) + fabs(d[m+1]);
          if (fabs(e[m]) + dd == dd) break;
          }
          if (m != l)
             {
             if (iter++ == 30) erhand("No convergence in TLQI.");
             g = (d[l+1] - d[l]) / (2.0 * e[l]);
             r = sqrt((g * g) + 1.0);
             g = d[m] - d[l] + e[l] / (g + SIGN(r, g));
             s = c = 1.0;
             p = 0.0;
             for (i = m-1; i >= l; i--)
                 {
                 f = s * e[i];
                 b = c * e[i];
                 if (fabs(f) >= fabs(g))
                    {
                    c = g / f;
                    r = sqrt((c * c) + 1.0);
                    e[i+1] = f * r;
                    c *= (s = 1.0/r);
                    }
                 else
                    {
                    s = f / g;
                    r = sqrt((s * s) + 1.0);
                    e[i+1] = g * r;
                    s *= (c = 1.0/r);
                    }
                 g = d[i+1] - p;
                 r = (d[i] - g) * s + 2.0 * c * b;
                 p = s * r;
                 d[i+1] = g + p;
                 g = c * r - b;
                 for (k = 1; k <= n; k++)
                     {
                     f = z[k][i+1];
                     z[k][i+1] = s * z[k][i] + c * f;
                     z[k][i] = c * z[k][i] - s * f;
                     }
                 }
                 d[l] = d[l] - p;
                 e[l] = g;
                 e[m] = 0.0;
             }
          }  while (m != l);
      }
 }

