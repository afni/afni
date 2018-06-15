/*****************************************************************************
  3dretroicor.c: AFNI program to perform Retrospective Image
  Correction for physiological motion effects, using a slightly
  modified version of the RETROICOR algorithm described in:

    Glover, G. H., Li, T., & Ress, D. (2000). Image-based method for
  retrospective correction of physiological motion effects in fMRI:
  RETROICOR. Magnetic Resonance in Medicine, 44, 162-167.

  Adapted from plug_retroicor.c.

  Fred Tam
  Sunnybrook & Women's College Health Sciences Centre
  August 1, 2002

  Copyright (C) 2002 Sunnybrook & Women's College Health Sciences Centre,
  Toronto, ON, Canada. As part of the AFNI software package, this software
  is distributed under the GNU General Public License, Version 2.
  See the file README.copyright for details.
*****************************************************************************/

/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

#include "retroicor.h"

#define TRIC_C_DEF_THRESHOLD 1
#define TRIC_R_DEF_WINSIZE 20
#define TRIC_M_DEF_ORDER 2
#define TRIC_I_DEF_IGNORE 0
#define TRIC_O_DEF_NEWPREFIX "retroicor"

/*****************************************************************************
  Print out a help message to stdout
******************************************************************************/

void TRIC_printhelp(void)
{
    printf("Usage: 3dretroicor [options] dataset\n"
	   "\n"
	   "Performs Retrospective Image Correction for physiological\n"
	   "motion effects, using a slightly modified version of the\n"
	   "RETROICOR algorithm described in:\n"
	   "\n"
	   "  Glover, G. H., Li, T., & Ress, D. (2000). Image-based method\n"
	   "for retrospective correction of physiological motion effects in\n"
	   "fMRI: RETROICOR. Magnetic Resonance in Medicine, 44, 162-167.\n"
	   "\n"
	   "Options (defaults in []'s):\n"
	   "\n"
	   " -ignore    = The number of initial timepoints to ignore in the\n"
	   "              input (These points will be passed through\n"
	   "              uncorrected) [0]\n"
	   " -prefix    = Prefix for new, corrected dataset [retroicor]\n"
           "\n"
           " -card      = 1D cardiac data file for cardiac correction\n"
           " -cardphase = Filename for 1D cardiac phase output\n"
	   " -threshold = Threshold for detection of R-wave peaks in input\n"
           "              (Make sure it's above the background noise level;\n"
	   "              Try 3/4 or 4/5 times range plus minimum) [1]\n"
	   "\n"
	   " -resp      = 1D respiratory waveform data for correction\n"
	   " -respphase = Filename for 1D resp phase output\n"
	   /*-- removed winsize ui
	   " -window    = Window size for input point estimate of slope\n"
	   "              (Try 1/3 or 1/2 times respiratory sampling rate in\n"
	   "              Hz) [50]\n"
	   removed winsize ui --*/
	   "\n"
	   " -order     = The order of the correction (2 is typical;\n"
	   "              higher-order terms yield little improvement\n"
	   "              according to Glover et al.) [2]\n"
           "\n"
           " -help      = Display this message and stop (must be first arg)\n"
	   "\n"
	   "Dataset: 3D+time dataset to process\n"
	   "\n"
	   "** The input dataset and at least one of -card and -resp are\n"
	   "    required.\n"
	   "\n"
	   "NOTES\n"
	   "-----\n"
	   "\n"
	   "The durations of the physiological inputs are assumed to equal\n"
	   "the duration of the dataset. Any constant sampling rate may be\n"
	   "used, but 40 Hz seems to be acceptable. This program's cardiac\n"
	   "peak detection algorithm is rather simplistic, so you might try\n"
	   "using the scanner's cardiac gating output (transform it to a\n"
	   "spike wave if necessary).\n"
	   "\n"
	   "This program uses slice timing information embedded in the\n"
	   "dataset to estimate the proper cardiac/respiratory phase for\n"
	   "each slice. It makes sense to run this program before any\n"
	   "program that may destroy the slice timings (e.g. 3dvolreg for\n"
	   "motion correction).\n"
	   "\n"
	   "Author -- Fred Tam, August 2002\n"
           "\n"
	   MASTER_SHORTHELP_STRING
	   );
}

/*****************************************************************************
  Monolithic Mainline
******************************************************************************/

int main(int argc, char * argv[])
{
   char cphase1d[256] = "\0", rphase1d[256] = "\0";
   char new_prefix[THD_MAX_PREFIX] = TRIC_O_DEF_NEWPREFIX;
   MCW_idcode * idc ;
   THD_3dim_dataset * dset , * new_dset;
   double * avg = NULL;
   double * ca , * cb, * ra, * rb;
   MRI_IMAGE * card = NULL, * resp = NULL;
   MRI_IMAGE * cardphase = NULL, * respphase = NULL;
   float threshold = TRIC_C_DEF_THRESHOLD;
   int ignore = TRIC_I_DEF_IGNORE;
   int M = TRIC_M_DEF_ORDER;
   int winsize = TRIC_R_DEF_WINSIZE;
   float tr;
   int ival, nvals;
   FILE * fp;
   float * cpdata, * rpdata;
   int argi = 1;

   /*-------------------------------------------------------------*/
   /*----- Check arguments to see if they are reasonable-ish -----*/

   if (argc < 2 || strcmp(argv[1], "-help") == 0) {
       TRIC_printhelp();
       exit(0);
   }

   mainENTRY("3dretroicor main"); PRINT_VERSION("3dretroicor") ;
   machdep();
   AFNI_logger("3dretroicor", argc, argv);

   /* Iterate over commandline args */
   while (argi < argc && argv[argi][0] == '-') {

       /*-- ignore --*/

       if (strcmp(argv[argi], "-ignore") == 0) {
	   if (argi + 1 >= argc) {
	       fprintf(stderr, "*** -ignore needs an argument!\n");
	       exit(1);
	   }
	   argi += 1;
	   ignore = atoi(argv[argi]);
	   if (ignore < 0) {
	       fprintf(stderr, "*** %i is not a valid number to ignore!\n",
		       ignore);
	       exit(1);
	   }
	   argi += 1;
	   continue;  /* Skip the rest of the loop and go to the next arg */
       }

       /*-- prefix --*/

       if (strcmp(argv[argi], "-prefix") == 0) {
	   if (argi + 1 >= argc) {
	       fprintf(stderr, "*** -prefix needs an argument!\n");
	       exit(1);
	   }
	   argi += 1;
	   MCW_strncpy(new_prefix, argv[argi], THD_MAX_PREFIX);
	   if (! THD_filename_ok(new_prefix)) {
	       fprintf(stderr, "*** %s is not a valid prefix!\n", new_prefix);
	       exit(1);
	   }
	   argi += 1;
	   continue;  /* Skip the rest of the loop and go to the next arg */
       }

       /*-- card --*/

       if (strcmp(argv[argi], "-card") == 0) {
	   if (argi + 1 >= argc) {
	       fprintf(stderr, "*** -card needs an argument!\n");
	       exit(1);
	   }
	   argi += 1;
	   card = mri_read_1D(argv[argi]);
	   if (card == NULL) {
	       fprintf(stderr, "*** Can't read -card %s\n", argv[argi]);
	       exit(1);
	   }
	   argi += 1;
	   continue;  /* Skip the rest of the loop and go to the next arg */
       }

       /*-- cardphase --*/

       if (strcmp(argv[argi], "-cardphase") == 0) {
	   if (argi + 1 >= argc) {
	       fprintf(stderr, "*** -cardphase needs an argument!\n");
	       exit(1);
	   }
	   argi += 1;
	   MCW_strncpy(cphase1d, argv[argi], 256);
	   if (! THD_filename_ok(cphase1d)) {
	       fprintf(stderr, "*** Bad name argument for -cardphase\n");
	       exit(1);
	   }
	   argi += 1;
	   continue;  /* Skip the rest of the loop and go to the next arg */
       }

       /*-- threshold --*/

       if (strcmp(argv[argi], "-threshold") == 0) {
	   if (argi + 1 >= argc) {
	       fprintf(stderr, "*** -threshold needs an argument!\n");
	       exit(1);
	   }
	   argi += 1;
	   threshold = atof(argv[argi]);
	   argi += 1;
	   continue;  /* Skip the rest of the loop and go to the next arg */
       }

       /*-- resp --*/

       if (strcmp(argv[argi], "-resp") == 0) {
	   if (argi + 1 >= argc) {
	       fprintf(stderr, "*** -resp needs an argument!\n");
	       exit(1);
	   }
	   argi += 1;
	   resp = mri_read_1D(argv[argi]);
	   if (resp == NULL) {
	       fprintf(stderr, "*** Can't read -resp %s\n", argv[argi]);
	       exit(1);
	   }
	   argi += 1;
	   continue;  /* Skip the rest of the loop and go to the next arg */
       }

       /*-- respphase --*/

       if (strcmp(argv[argi], "-respphase") == 0) {
	   if (argi + 1 >= argc) {
	       fprintf(stderr, "*** -respphase needs an argument!\n");
	       exit(1);
	   }
	   argi += 1;
	   MCW_strncpy(rphase1d, argv[argi], 256);
	   if (! THD_filename_ok(rphase1d)) {
	       fprintf(stderr, "*** Bad name argument for -respphase\n");
	       exit(1);
	   }
	   argi += 1;
	   continue;  /* Skip the rest of the loop and go to the next arg */
       }

       /*-- window --*/

       /*-- removed winsize ui --*/
#if 0
       if (strcmp(argv[argi], "-window") == 0) {
	   if (argi + 1 >= argc) {
	       fprintf(stderr, "*** -window needs an argument!\n");
	       exit(1);
	   }
	   argi += 1;
	   winsize = atoi(argv[argi]);
	   argi += 1;
	   continue;  /* Skip the rest of the loop and go to the next arg */
       }
#endif
       /*-- removed winsize ui --*/

       /*-- order --*/

       if (strcmp(argv[argi], "-order") == 0) {
	   if (argi + 1 >= argc) {
	       fprintf(stderr, "*** -order needs an argument!\n");
	       exit(1);
	   }
	   argi += 1;
	   M = atoi(argv[argi]);
	   if (M < 1) {
	       fprintf(stderr, "*** %i is not a valid order number\n", M);
	       exit(1);
	   }
	   argi += 1;
	   continue;  /* Skip the rest of the loop and go to the next arg */
       }
   } /* End iterating over commandline args */

   /* Check that at least one of Cardiac and Resp were selected */
   if (card == NULL && resp == NULL) {
       fprintf(stderr, "*** Need at least one correction (-card, -resp)\n");
       exit(1);
   }

   /*-- Open and check input dataset --*/

   if (argi >= argc) {
       fprintf(stderr, "*** No input dataset!?\n");
       exit(1);
   } else if (argi < argc - 1) {
       fprintf(stderr, "*** Too many input datasets?!\n");
       exit(1);
   }

   dset = THD_open_dataset(argv[argi]);
   if (! ISVALID_3DIM_DATASET(dset)) {
       fprintf(stderr, "*** Can't open dataset %s\n", argv[argi]);
       exit(1);
   }
   if (DSET_NUM_TIMES(dset) < 2) {
       fprintf(stderr, "*** Input dataset is not 3D+time!\n");
       exit(1);
   }

   /*----------------------------------------------------------*/
   /*---------- At this point, the inputs are OK-ish ----------*/

   /*-- copy the image data for editing in place --*/

   new_dset = EDIT_full_copy( dset , new_prefix );
   if( new_dset == NULL ) {
       fprintf(stderr, "*** Error copying dataset\n");
       exit(1);
   }
   tross_Copy_History(dset, new_dset); /* Copy and add to new_dset history */
   tross_Make_History("3dretroicor", argc, argv, new_dset);
   DSET_unload( dset ) ;  /* We won't need the old dataset anymore */

   /*-- calculate cardiac correction coefficients if requested --*/

   if (card != NULL) {
       /*-- convert cardiac waveform to phase --*/
       cardphase = RIC_ToCardiacPhase(card, threshold) ;
       if (cardphase == NULL) {
	   THD_delete_3dim_dataset( new_dset , False ) ;
	   fprintf(stderr, "*** Error transforming cardiac data\n");
	   exit(2);
       }

       /*-- calculate dataset voxel means --*/
       avg = RIC_CalcVoxelMeans(new_dset, ignore);
       if (avg == NULL) {
	   THD_delete_3dim_dataset( new_dset , False ) ;
	   mri_free(cardphase);
	   fprintf(stderr, "*** Error calculating dataset voxel means\n");
	   exit(2);
       }

       /*-- calculate coefficients for each voxel --*/
       if (RIC_CalcCoeffAB(new_dset, cardphase, avg, &ca, &cb, M, ignore)
	   != 0) {

	   THD_delete_3dim_dataset( new_dset , False ) ;
	   mri_free(cardphase);
	   fprintf(stderr, "*** Error calculating cardiac a b coefficients\n");
	   exit(2);
       }
   }

   /*-- calculate respiratory correction coefficients if requested --*/

   if (resp != NULL) {
       /*-- Set winsize to 1/2 sampling rate of resp in Hz --*/
       tr = new_dset->taxis->ttdel;
       switch (new_dset->taxis->units_type) {
       case UNITS_MSEC_TYPE: tr /= 1000; break;
       case UNITS_SEC_TYPE:  break;
       case UNITS_HZ_TYPE:   tr = 1 / tr; break;
       default:
	   THD_delete_3dim_dataset( new_dset , False ) ;
	   fprintf(stderr, "*** Bad time units type in dataset\n");
	   exit(2);
       }
       winsize = ceil(resp->nx / (tr * DSET_NVALS(new_dset)) / 2.0);

       /*-- convert respiratory waveform to phase --*/
       respphase = RIC_ToRespPhase(resp, winsize) ;
       if (respphase == NULL) {
	   THD_delete_3dim_dataset( new_dset , False ) ;
	   fprintf(stderr, "*** Error transforming resp data\n");
	   exit(2);
       }

       /*-- calculate dataset voxel means if not already done --*/
       if (avg == NULL) {
	   avg = RIC_CalcVoxelMeans(new_dset, ignore);
	   if (avg == NULL) {
	       THD_delete_3dim_dataset( new_dset , False ) ;
	       mri_free(respphase);
	       fprintf(stderr, "*** Error calculating dataset voxel means2\n");
	       exit(2);
	   }
       }

       /*-- calculate coefficients for each voxel --*/
       if (RIC_CalcCoeffAB(new_dset, respphase, avg, &ra, &rb, M, ignore)
	   != 0) {

	   THD_delete_3dim_dataset( new_dset , False ) ;
	   mri_free(respphase);
	   fprintf(stderr, "*** Error calculating resp a, b coefficients\n");
	   exit(2);
       }
   }

   /*-- do cardiac correction if requested --*/

   if (card != NULL) {
       /*-- correct the image data --*/
       if (RIC_CorrectDataset(new_dset, cardphase, ca, cb, M, ignore) != 0) {
	   THD_delete_3dim_dataset( new_dset , False ) ;
	   mri_free(cardphase);
	   free(ca); free(cb);
	   fprintf(stderr, "*** Error applying cardiac correction\n");
	   exit(3);
       }

       /*-- if requested, write phase data to file and pass to AFNI --*/
       if ( THD_filename_ok(cphase1d) ) {
	   /* Write the file */
	   fp = fopen(cphase1d, "w");
	   nvals = cardphase->nx;
	   cpdata = MRI_FLOAT_PTR(cardphase);
	   for (ival = 0; ival < nvals; ival += 1) {
	       fprintf(fp, "%f\n", cpdata[ival]);
	   }
	   fclose(fp);
       }

       mri_free(cardphase);
       free(ca); free(cb); free(avg); avg = NULL;
   }

   /*-- do resp correction if requested --*/

   if (resp != NULL) {
       /*-- correct the image data --*/
       if (RIC_CorrectDataset(new_dset, respphase, ra, rb, M, ignore) != 0) {
	   THD_delete_3dim_dataset( new_dset , False ) ;
	   mri_free(respphase);
	   free(ra); free(rb);
	   fprintf(stderr, "*** Error applying resp correction\n");
	   exit(3);
       }

       /*-- if requested, write phase data to file and pass to AFNI --*/
       if ( THD_filename_ok(rphase1d) ) {
	   /* Write the file */
	   fp = fopen(rphase1d, "w");
	   nvals = respphase->nx;
	   rpdata = MRI_FLOAT_PTR(respphase);
	   for (ival = 0; ival < nvals; ival += 1) {
	       fprintf(fp, "%f\n", rpdata[ival]);
	   }
	   fclose(fp);
       }

       mri_free(respphase);
       free(ra); free(rb); if (avg != NULL) free(avg);
   }

   /*-- write out new dataset --*/

   if (DSET_write(new_dset) != False) {
      fprintf(stderr,"++ output dataset: %s\n",DSET_BRIKNAME(new_dset)) ;
      exit(0) ;
   } else {
      fprintf(stderr,
         "** 3dretroicor: Failed to write output!\n" ) ;
      exit(1) ;
   }

   /*-- done successfully!!! --*/

   exit(0);
}
