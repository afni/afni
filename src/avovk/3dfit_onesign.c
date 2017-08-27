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

#include "thd_segtools_fNM.h"

#define POLORDERMAX 30

int
main (int argc, char **argv)
{
   int   i=0, j=0, nl=0, k=0,
         posi=0, posj=0, posk=0, nrow=0,
         verb=1, iarg=0, ncol = 0;
   char *prefix = NULL;
   char *input = NULL, *maskname=NULL;
   THD_3dim_dataset *in_set=NULL, *out_set=NULL;
   THD_3dim_dataset *mset =NULL ;
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
               "         (Andrej, note that it works with both types\n"
               "          of files, so it completely replaces fitanje_1sign)\n"
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
   
   if (!(out_set = thd_polyfit(in_set, mask, polorder, prefix, verb))) {
      ERROR_exit("Failed to do the fit!");
   }

   
   /* write the output */
   if( verb ) ININFO_message("\nWriting fit dataset: %s",prefix) ;
   tross_Copy_History( in_set , out_set ) ;
   tross_Make_History( "3dfit_onesign" , argc, argv , out_set ) ;

   
   DSET_write(out_set); DSET_unload(out_set); 
   DSET_delete(out_set); out_set = NULL;
  
   if (verb) fprintf (stderr,"\n");
   
   RETURN(0);

}

