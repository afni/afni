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
   Use /* for comments , especially when covering multiple lines
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
   
gcc -Wall -Wno-unused fit_onesign.c -lgsl -lgslcblas -lm -o fitanje_1sign -I/sw/include/ -L/sw/lib -I. -Inifti/niftilib -Inifti/nifticdf -Inifti/znzlib -L. -L/sw/lib -L/usr/X11R6/lib -L/usr/local/lib -Wl,-x -Wl,-multiply_defined -Wl,warning -Wl,-bind_at_load -l3DEdge -lmri -lf2c -lmri /sw/lib/libXm.a -lXm -lXmu -lXp -lXpm -lXext -lXt -lX11 -lz -lexpat -lm -lc
    
*/

/* to link to AFNI's loot */
#include "../mrilib.h"

#include <stdio.h>
#include <gsl/gsl_multifit.h>

#define C(i) (gsl_vector_get(c,(i)))
#define POLORDERMAX 30

int
main (int argc, char **argv)
{
   int   i=0, j=0, nl=0, k=0,
         posi=0, posj=0, posk=0, nrow=0,
         verb=1, iarg=0, ncol = 0;
   double xi=0.0, yi=0.0, yy=0.0, ei=0.0, sumsq=0.0,  med=0.0;
   char *prefix = NULL;
   char *input = NULL, *maskname=NULL;
   THD_3dim_dataset *in_set=NULL, *out_set=NULL;
   THD_3dim_dataset *mset =NULL ;
   gsl_matrix *X=NULL, *cov=NULL;
   gsl_vector *y=NULL, *w=NULL, *c=NULL;
   MRI_IMAGE *im = NULL;
   double *dar = NULL;
   float *cbuf=NULL;
   float *dvec = NULL;
   int polorder = 5, nmask=-1, mnx=-1, mny=-1, mnz=-1;
   byte *mask=NULL;
   gsl_multifit_linear_workspace *work=NULL;
   
   mainENTRY("3dfit_onesign"); machdep();
   PRINT_VERSION("3dfit_onesign"); AUTHOR("avovk") ;
   AFNI_logger("3dfit_onesign",argc,argv);

   if (argc < 2)
   {
     fprintf ( stderr,
               "usage: 3dfitanje_1sign -input SIGNATURE -prefix PREFIX\n"
               "                       [-quiet] [-mask MASK] \n"
               "                       [-polorder POLORDER]\n"
               "Mandatory Parameters:\n"
               "   -input SIGNATURE: Signature BRIK or .1D file\n"
               "               (Andrej, note that it works with both types)\n"
               "Optional Parameters:\n"
               "  -prefix PREFIX: Prefix of output. \n"
               "                  If input is .1D, the output will be too.\n"
               "                  Default output prefix is 'fitty' and will\n"
               "                  get overwritten.\n"
               "  -quiet: silent running\n"
               "  -verb:  Increase verbosity.\n"
               "          The more of those you put in, the more the program\n"
               "          outputs info.\n"
               "  -mask MASK: If MASK[i] then voxel i is processed.\n"
               "  -polorder POLORDER: Polynominal order, default is 5\n"
               "");
     exit (-1);
   }


   /*------- read command line args -------*/

   verb = 1; 
   prefix = NULL;
   input = NULL;
   polorder = 5;
   iarg = 1 ; 
   while( iarg < argc ){
      if( strcasecmp(argv[iarg],"-input") == 0 ){  
         if( iarg+1 >= argc )
         ERROR_exit("Need an argument after '%s'",argv[iarg]);
         input = argv[++iarg];
         iarg++ ; continue ;
      }
      if( strcasecmp(argv[iarg],"-prefix") == 0 ){  
         if( iarg+1 >= argc )
         ERROR_exit("Need an argument after '%s'",argv[iarg]);
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) )
            ERROR_exit("Illegal filename prefix '%s'",prefix) ;
         iarg++ ; continue ;
      }
      if( strcasecmp(argv[iarg],"-polorder") == 0 ){  
         if( iarg+1 >= argc )
         ERROR_exit("Need a positive integer after '%s'",argv[iarg]);
         polorder = (int)strtod(argv[++iarg], NULL) ;
         if( polorder < 0 || polorder > POLORDERMAX )
            ERROR_exit("Polort %d outside allowed range of [0..%d]",
                        polorder,POLORDERMAX ) ;
         iarg++ ; continue ;
      }
      if( strcasecmp(argv[iarg],"-mask") == 0 ){
       if( mask != NULL )
         ERROR_exit("Can't have two -mask arguments!") ;
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       maskname = argv[iarg];
       iarg++ ; continue ;
      }
      
      if( strncasecmp(argv[iarg],"-quiet",2) == 0 ){
         verb = 0 ; iarg++ ; continue ;
      }   
      if( strncasecmp(argv[iarg],"-verb",4) == 0 ){
         ++verb ; iarg++ ; continue ;
      }
      ERROR_exit("Unknown argument on command line: '%s'",argv[iarg]) ;
   }
   
   /* checks */
   if (!input) 
      ERROR_exit("Have no input!") ;
   if (!prefix) {
      prefix = "fitty";
      THD_force_ok_overwrite(1) ;   /* don't worry about overwriting */
   }
   
   /* Read in dset */
   if (verb) fprintf(stderr,"Patience, reading %s... ", input);
   in_set = THD_open_dataset(input);
   CHECK_OPEN_ERROR(in_set,input) ;
   ncol = DSET_NVALS(in_set);
   nrow = DSET_NVOX(in_set);
   if (polorder >= DSET_NVALS(in_set)) {
      ERROR_exit("Polynomial order of %d is too high for a series of %d values\n"
                  , polorder, DSET_NVALS(in_set));
   }
   DSET_load(in_set) ; CHECK_LOAD_ERROR(in_set) ;
   
   /* Read in mask */
   if (maskname) {
      mset = THD_open_dataset(maskname) ;
      CHECK_OPEN_ERROR(mset,maskname) ;
      mnx = DSET_NX(mset); mny = DSET_NY(mset); mnz = DSET_NZ(mset);
      DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
      mask = THD_makemask( mset, 0, 1.0f,0.0f ); DSET_delete(mset);
      if( mask == NULL ) ERROR_exit("Can't make mask") ;
      nmask = THD_countmask( mnx*mny*mnz , mask ) ;
      INFO_message("%d voxels in the [%dx%dx%d] mask",nmask, mnx, mny, mnz) ;
      if( nmask < 1 ) ERROR_exit("mask %s is empty?!", maskname) ;
      if (  mask &&
            (mnx != DSET_NX(in_set) || 
             mny != DSET_NY(in_set) || 
             mnz != DSET_NZ(in_set) ) ) {
         ERROR_exit("Dimension mismatch between mask and input dset");      
      }
   }
      
   if (verb) 
      fprintf (stderr,"Have %d cols, %d voxels, %d in mask\n", 
            ncol, nrow, mask ? nmask:nrow);
   
   
   /* prepare output */
   out_set = EDIT_empty_copy(in_set) ;
   EDIT_dset_items(  out_set ,
                     ADN_nvals     , polorder           ,
                     ADN_ntt       , polorder          ,
                     ADN_datum_all , MRI_float      ,
                     ADN_brick_fac , NULL           ,
                     ADN_prefix    , prefix   ,
                     ADN_none ) ;
   tross_Copy_History( in_set , out_set ) ;
   tross_Make_History( "3dfit_onesign" , argc, argv , out_set ) ;

   for( j=0 ; j < polorder ; j++ ) /* create empty bricks to be filled below */
      EDIT_substitute_brick( out_set , j , MRI_float , NULL ) ;


   /* do the fitting */
   if (verb) fprintf (stderr,"Now fitting...\n");
   
   X = gsl_matrix_alloc (ncol, polorder);
   y = gsl_vector_alloc (ncol);
     
   c = gsl_vector_alloc (polorder);
   cov = gsl_matrix_alloc (polorder, polorder);
     
   for (i = 0; i < ncol; i++)  {
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
    if (verb > 1) 
      fprintf(stdout, "#%s_0\t%s_1\t%s_2\t%s_3\t%s_4\n",
                    input,input,input,input,input);

    // go by lines - signatures
    /* pre-allocate, I think this should be just fine, 
       there should be no need to reinitialize work 
       all the time */   
    work = gsl_multifit_linear_alloc (ncol, polorder);

    dvec = (float * )malloc(sizeof(float)*ncol) ;  /* array to hold signature */
    cbuf = (float *)malloc(sizeof(float)*polorder) ;  
                              /* array to hold fit */
    for (nl=0; nl<nrow; ++nl) {
      if (!mask || mask[nl]) {
         posi = -1;
         posj = -1;
         posk = -1;

         THD_extract_array( nl , in_set , 0 , dvec ) ; 
                                    /*get signature from voxel */

         for (k = 0; k < ncol; k++) {
            gsl_vector_set (y, k, dvec[k]);
         }

         gsl_multifit_linear (X, y, c, cov,
                              &sumsq, work);

         /* printf ( "\n # best fit: Y = %g + %g X + %g X^2 +%g X^3 + %g X^4\n",
                     C(0), C(1), C(2), C(3), C(4));
            printf ("# sumsq = %g\n", sumsq); */

         for (i=0;i<polorder;++i) cbuf[i] = (float)C(i);
         THD_insert_series( nl , out_set , polorder , MRI_float , cbuf , 1 ) ; 
                                       /* stick result in output */
         if (verb > 1) 
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
   }
   gsl_multifit_linear_free (work); work = NULL;
   
   /* write the output */
   if( verb ) ININFO_message("\nWriting fit dataset: %s",prefix) ;
   
   DSET_write(out_set); DSET_unload(out_set); 
   DSET_delete(out_set); out_set = NULL;
   free(dvec); dvec = NULL;
   free(cbuf); cbuf = NULL;
    gsl_vector_free (y);
    gsl_vector_free (c);
    gsl_matrix_free (cov);
    gsl_matrix_free (X);
    //gsl_vector_free (w);
    free(dvec); dvec = NULL;
    if (verb) fprintf (stderr,"\n");
    return 0;

}

