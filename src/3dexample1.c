
/* ----------------------------------------------------------------------
 * Just a very basic example program for reading and writing AFNI datasets.
 * Output dataset is input * 2.
 *
 * usage: 3dexample -input INSET+orig -prefix OUTPUT
 *
 * compile example:
 *
 *    gcc -DREAD_WRITE_64 -o 3dexample1 3dexample1.c    \
 *        -I$HOME/abin -L$HOME/abin -lmri -lconverted_from_fortran -lXt -lz -lexpat -lm
 *
 * Author: R Reynolds  19 Sep, 2014
 */

#include "mrilib.h"

int compute_output_dset(THD_3dim_dataset ** dout, THD_3dim_dataset * din,
                        char * oname);
int open_input_dset(THD_3dim_dataset ** din, char * fname);
int process_options(int argc, char *argv[], char ** iname, char ** oname);
int show_help(void);

/*--------------- main routine ---------------*/
int main( int argc, char *argv[] )
{
   THD_3dim_dataset * din, * dout;
   char             * inname, * outname;

   /* process options: a negative return is considered an error */
   if( process_options(argc, argv, &inname, &outname) ) return 1;

   if( open_input_dset(&din, inname) ) return 1;

   if( compute_output_dset(&dout, din, outname) ) return 1;

   DSET_write(dout);

   return 0;  /* success */
}

int compute_output_dset(THD_3dim_dataset ** dout, THD_3dim_dataset * din,
                        char * oname)
{
   float * indata, * outdata;
   int     index, nxyz;

   /* create output data via malloc, need number of voxels */
   nxyz = DSET_NVOX(din);
   outdata = (float *)malloc(nxyz * sizeof(float));
   if( ! outdata ) {
      fprintf(stderr,"** failed to alloc %d voxels for output\n", nxyz);
      return 1;
   }

   /* note input data pointer, from sub-brick #0 of input dataset struct */
   indata = DSET_ARRAY(din, 0);

   /* do the work: just double the values */
   for( index = 0; index < nxyz; index++ )
     outdata[index] = 2.0 * indata[index];

   /* create empty dataset, set output name, attach data (at volume 0) */
   *dout = EDIT_empty_copy(din);
   EDIT_dset_items(*dout, ADN_prefix, oname, ADN_none);
   EDIT_substitute_brick(*dout, 0, MRI_float, outdata);

   return 0;
}

int open_input_dset(THD_3dim_dataset ** din, char * fname)
{
   *din = THD_open_dataset(fname);
   if( ! *din ) {
      fprintf(stderr,"** failed to read input dataset '%s'\n", fname);
      return 1;
   }

   /* refuse to work with anything but float here */
   if( DSET_BRICK_TYPE(*din, 0) != MRI_float ) {
      fprintf(stderr,"** input must be of type float, failing...\n");
      return 1;
   }

   /* data is not automatically read in, do it now */
   DSET_load(*din);

   return 0;
}


/* return 0 on success, else error */
int process_options(int argc, char *argv[], char ** iname, char ** oname)
{
   /* be very simple, require the exact parameter set */
   if( argc != 5 || strcmp(argv[1], "-input") || strcmp(argv[3], "-prefix") ) {
      show_help();
      return 1;  /* error */
   }

   *iname = argv[2]; 
   *oname = argv[4];

   return 0;
}

int show_help(void)
{
   printf("This is a demo AFNI program that multiplies a dataset by 2.\n"
          "See 3dToyProg.c for a more detailed example.\n\n"
          "usage: 3dexample1 -input INSET -prefix PREFIX\n\n");
   return 0;
}

