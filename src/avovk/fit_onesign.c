/*
Main problem: Assuming we had 13 columns always. While this should
have been the case, the chew_medaniso and chew_MADaniso had 8 that
was caused by an error in one of the scripts. I used something to the
effect of *.BRIK instead of *.HEAD. Not all the dsets were read in
because some were .BRIK.gz ! In any case, it is a good idea, to put
the expected number of columns in a variable, rather than have 13 in many 
places in the code. Also, it is a good idea to check to be sure that what
you're reading is what you'd expect. 
 

C code notes: For consistency with AFNI's code, would you please:
   Set your editor to replace tab characters with 3 spaces
   Use / * for comments , especially when covering multiple lines
   DO not exceed 80 characters per line (helps for reading and more
      importantly printing). 
   Better print  debugging to stderr and useful output to stdout 
   Try to avoid defining macros in the middle of a scope
   Be obsessed about initializing all your variables, 
      especially counters and pointers
   Absolutely no variable declarations in the middle of a scope.
      We need to compile on machines that won't accept that.

To compile with AFNI library:
   Download AFNI source tree, say you put it under: AFNI
      cd AFNI/src
   put this .c file there
   compile distribution with make cleanest vastness
   compile this file with:
   
gcc -Wall -Wno-unused fit_onesign.c -lgsl -lgslcblas -lm -o fitanje_1sign -I/sw/include/ -L/sw/lib -I. -Inifti/niftilib -Inifti/nifticdf -Inifti/znzlib -L. -L/sw/lib -L/usr/X11R6/lib -L/usr/local/lib -Wl,-x -Wl,-multiply_defined -Wl,warning -Wl,-bind_at_load -l3DEdge -lmri -lconverted_from_fortran -lmri /sw/lib/libXm.a -lXm -lXmu -lXp -lXpm -lXext -lXt -lX11 -lz -lexpat -lm -lc
    
*/

/* to link to AFNI's loot */
#include "../mrilib.h"

#include <stdio.h>
#include <gsl/gsl_multifit.h>

#define C(i) (gsl_vector_get(c,(i)))

int
main (int argc, char **argv)
{
   int   i=0, j=0, n=0, nl=0, k=0,
         posi=0, posj=0, posk=0, ncol=0, nrow=0;
   double xi=0.0, yi=0.0, yy=0.0, ei=0.0, sumsq=0.0,  med=0.0;
   gsl_matrix *X=NULL, *cov=NULL;
   gsl_vector *y=NULL, *w=NULL, *c=NULL;
   MRI_IMAGE *im = NULL;
   double *dar = NULL;
   gsl_multifit_linear_workspace *work=NULL;
   
   if (argc != 2)
   {
     fprintf (stderr,"usage: fitanje_1sign data > outfile\n");
     exit (-1);
   }

   /* slower than specific code you had but more convenient.
      It allows you to use all the column and row selections
      we can do with filenames. Also, keeps you fron worrying
      about dimensions. 
      The problem with your code was assuming you had 13 columns always 
      That was not the case for stat5_fitcoef. 
      OK, that was caused by a problem in the scripts. That is fixed,
      but I leave this change here anyway. 
      */
   fprintf(stderr,"Patience, reading %s... ", argv[1]);
   im = mri_read_double_1D (argv[1]);
   if (!im) {
      fprintf(stderr,"Error: Failed to read matrix data from %s\n",
                     argv[1]);
      return(-1);
   }
   ncol = im->ny;
   nrow = im->nx;
   fprintf (stderr,"Have %d cols, %d rows\nNow fitting...", ncol, nrow);
   n = ncol-3;
   /* now just get the array and kill the rest */
   dar = MRI_DOUBLE_PTR(im);
   /* make sure that pointer is set to NULL in im, or risk hell */
   mri_clear_data_pointer(im) ;
   if (im) mri_free(im); im = NULL; /* now kill im */
   
   X = gsl_matrix_alloc (n, 5);
   y = gsl_vector_alloc (n);
     
   c = gsl_vector_alloc (5);
   cov = gsl_matrix_alloc (5, 5);
     
   for (i = 0; i < n; i++)  {
      xi = i+1;
      gsl_matrix_set (X, i, 0, 1.0);
      gsl_matrix_set (X, i, 1, xi);
      gsl_matrix_set (X, i, 2, xi*xi);

      gsl_matrix_set (X, i, 3, xi*xi*xi);
      gsl_matrix_set (X, i, 4, xi*xi*xi*xi);
      //    printf ("%lg ",xi);
    }


    /*make header
      printf ("matrvola\n");
      ZSS: By adding # to the text line, 
           I made the output file be a .1D format */
    fprintf(stdout, "#%s_0\t%s_1\t%s_2\t%s_3\t%s_4\n",
                    argv[1],argv[1],argv[1],argv[1],argv[1]);

    // go by lines - signatures
    /* pre-allocate, I think this should be just fine, 
       there should be no need to reinitialize work 
       all the time */   
    work = gsl_multifit_linear_alloc (n, 5);
    for (nl=0; nl<nrow; ++nl) {
      posi = (int)dar[nl];
      posj = (int)dar[nl+  nrow];
      posk = (int)dar[nl+2*nrow];

      for (k = 3; k < ncol; k++) {
         gsl_vector_set (y, k-3, dar[nl+k*nrow]);
      }
        
        
      gsl_multifit_linear (X, y, c, cov,
                           &sumsq, work);
    
                          
      /* printf ( "\n # best fit: Y = %g + %g X + %g X^2 +%g X^3 + %g X^4\n",
                  C(0), C(1), C(2), C(3), C(4));
         printf ("# sumsq = %g\n", sumsq); */

      fprintf (stdout,  "%11g\t%11g\t%11g\t%11g\t%11g\n", 
                        C(0), C(1), C(2), C(3), C(4)); 

      /*
      printf ("# covariance matrix:\n");
      printf ("[ %+.5e, %+.5e, %+.5e  \n",
              COV(0,0), COV(0,1), COV(0,2));
      printf ("  %+.5e, %+.5e, %+.5e  \n", 
              COV(1,0), COV(1,1), COV(1,2));
      printf ("  %+.5e, %+.5e, %+.5e ]\n", 
              COV(2,0), COV(2,1), COV(2,2));
       printf ("# chisq = %g\n", chisq);
      */
      
   }
   gsl_multifit_linear_free (work); work = NULL;
   
   free(dar); dar = NULL; /* done with input array */

    gsl_vector_free (y);
    gsl_vector_free (c);
    gsl_matrix_free (cov);
    gsl_matrix_free (X);
    //gsl_vector_free (w);
    
    fprintf (stderr,"\n");
    return 0;

}

