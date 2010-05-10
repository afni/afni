/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/********************************************************
 * 3dROIstats                                           *
 * T. Ross 5/99                                         *
 *------------------------------------------------------*
 * Code for -summary added by M.S. Beauchamp, 12/1999   *
 *------------------------------------------------------*
 * Code for -numROI added by T. ROss 5/00               * 
 *------------------------------------------------------*
 * Code for -minmax,-nzminmax added by R Reynolds  7/04 *
 *------------------------------------------------------*
 * Fixed minmax initializers           R Reynolds  9/04 *
 *------------------------------------------------------*
 * Code for -mask_f2short              R Reynolds  2/05 *
 *------------------------------------------------------*
 * More info in -help w/examples       R Reynolds 11/05 *
 ********************************************************/

#include "mrilib.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>

#define IMAX 65536
short non_zero[IMAX];		/* Ugly; depends upon sizeof(short)=2 */

void Error_Exit(char *message)
{
    fprintf(stderr, "\n\nError: %s\n", message);
    exit(1);
}


int main(int argc, char *argv[])
{
    THD_3dim_dataset *mask_dset = NULL, *input_dset = NULL ;
    int mask_subbrik = 0;
    int sigma = 0, nzsigma = 0, mean = 1, nzmean = 0, nzcount = 0;
    int debug = 0, quiet = 0, summary = 0;
    int minmax = 0, nzminmax = 0, donzsum = 0;		/* 07 July, 2004 [rickr] */
    short *mask_data;
    int nvox, i, brik;
    int num_ROI, ROI, mask_f2s = 0;
    int force_num_ROI = 0;	/* Added 5/00 */
    int narg = 1;
    double *sum=NULL, *sumsq=NULL, *nzsum=NULL, sig, *sumallbriks=NULL; 
    double  *min=NULL, *max=NULL, *nzsumsq=NULL,
            *nzmin=NULL, *nzmax=NULL;		/* 07 July, 2004 [rickr] */
    long *voxels=NULL, *nzvoxels=NULL;
    float *input_data;
    byte *temp_datab;
    short *temp_datas;
    double *percentile=NULL;
    float *fv = NULL;
    int nfv = 0, perc = 0, nzperc = 0;
    int nobriklab=0 ;  /* 14 Mar 2008 */
    int disp1d=1;   /* ZSS May 2008 */
    byte *roisel=NULL; 
    char zerofill[32]={" "};
    
    if (argc < 3 || strcmp(argv[1], "-help") == 0) {
	printf("Usage: 3dROIstats -mask[n] mset [options] datasets\n"
"\n"
"   Display statistics over masked regions.  The default statistic\n"
"   is the mean.\n"
"\n"
"   There will be one line of output for every sub-brick of every\n"
"   input dataset.  Across each line will be every statistic for\n"
"   every mask value.  For instance, if there 3 mask values (1,2,3),\n"
"   then the columns Mean_1, Mean_2 and Mean_3 will refer to the\n"
"   means across each mask value, respectively.  If 4 statistics are\n"
"   requested, then there will be 12 stats displayed on each line\n"
"   (4 for each mask region), besides the file and sub-brick number.\n"
"\n"
"Examples:\n"
"\n"
"   3dROIstats -mask mask+orig. 'func_slim+orig[1,3,5]'\n"
"\n"
"   3dROIstats -minmax -sigma -mask mask+orig. 'func_slim+orig[1,3,5]'\n"
"\n"
"Options:\n"
"  -mask[n] mset Means to use the dataset 'mset' as a mask:\n"
"                 If n is present, it specifies which sub-brick\n"
"                 in mset to use a la 3dcalc.  Note: do not include\n"
"                 the brackets if specifying a sub-brick, they are\n"
"                 there to indicate that they are optional.  If not\n"
"                 present, 0 is assumed\n"
"                 Voxels with the same nonzero values in 'mset'\n"
"                 will be statisticized from 'dataset'.  This will\n"
"                 be repeated for all the different values in mset.\n"
"                 I.e. all of the 1s in mset are one ROI, as are all\n"
"                 of the 2s, etc.\n"
"                 Note that the mask dataset and the input dataset\n"
"                 must have the same number of voxels and that mset\n"
"                 must be BYTE or SHORT (i.e., float masks won't work\n"
"                 without the -mask_f2short option).\n"
"                 \n"
"  -mask_f2short  Tells the program to convert a float mask to short\n"
"                 integers, by simple rounding.  This option is needed\n"
"                 when the mask dataset is a 1D file, for instance\n"
   "                 (since 1D files are read as floats).\n"
"\n"
"                 Be careful with this, it may not be appropriate to do!\n"
"\n"
"  -numROI n     Forces the assumption that the mask dataset's ROIs are\n"
"                 denoted by 1 to n inclusive.  Normally, the program\n"
"                 figures out the ROIs on its own.  This option is \n"
"                 useful if a) you are certain that the mask dataset\n"
"                 has no values outside the range [0 n], b) there may \n"
"                 be some ROIs missing between [1 n] in the mask data-\n"
"                 set and c) you want those columns in the output any-\n"
"                 way so the output lines up with the output from other\n"
"                 invocations of 3dROIstats.  Confused?  Then don't use\n"
"                 this option!\n"
"  -zerofill ZF   For ROI labels not found, use 'ZF' instead of a blank\n"
"                 in the output file. This option is useless without -numROI\n"
"\n"
"  -roisel SEL.1D Only considers ROIs denoted by values found in SEL.1D\n"
"                 Note that the order of the ROIs as specified in SEL.1D\n"
"                 is not preserved. So an SEL.1D of '2 8 20' produces the\n"
"                 same output as '8 20 2'\n" 
"\n"
"  -debug        Print out debugging information\n"
"  -quiet        Do not print out labels for columns or rows\n"
"  -nomeanout    Do not print out the mean column. Default is \n"
"                to always start with the mean value.\n"
"                This option cannot be used with -summary\n"
"  -nobriklab    Do not print the sub-brick label next to its index\n"
"  -1Dformat     Output results in a 1D format that includes \n"
"                commented labels\n"
"  -1DRformat    Output results in a 1D format that includes \n"
"                uncommented labels. This format does not work well with \n"
"                typical 1D programs, but it is useful for R functions.\n"
"\n"
"The following options specify what stats are computed.  By default\n"
"the mean is always computed.\n"
"\n"
"  -nzmean       Compute the mean using only non_zero voxels.  Implies\n"
"                 the opposite for the normal mean computed\n"
"  -nzsum        Compute the sum using only non_zero voxels.  \n"
"  -nzvoxels     Compute the number of non_zero voxels\n"
"  -minmax       Compute the min/max of all voxels\n"
"  -nzminmax     Compute the min/max of non_zero voxels\n"
"  -sigma        Compute the standard deviation of all voxels\n"
"  -nzsigma      Compute the standard deviation of all non_zero voxels\n"
"  -median       Compute the median of all voxels.\n"
"  -nzmedian     Compute the median of non_zero voxels.\n"
"  -summary      Only output a summary line with the grand mean \n"
"                across all briks in the input dataset. \n"
"                This option cannot be used with -nomeanout.\n"
"\n"
"The output is printed to stdout (the terminal), and can be\n"
"saved to a file using the usual redirection operation '>'.\n"
"\n"
"N.B.: The input datasets and the mask dataset can use sub-brick\n"
"      selectors, as detailed in the output of 3dcalc -help.\n"
	    );

	printf("\n" MASTER_SHORTHELP_STRING);

	PRINT_COMPILE_DATE ; exit(0);
    }

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   machdep() ; 
   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

    /* scan argument list */
   disp1d = 0;
   mean = 1;   /* LEAVE this as the default ZSS March 09 2010 */
    while (narg < argc && argv[narg][0] == '-') {

	if (strncmp(argv[narg], "-mask_f2short", 9) == 0) {
            mask_f2s = 1;   /* convert float mask to short */
	    narg++;
	    continue;
        }
	if (strncmp(argv[narg], "-roisel", 5) == 0) {
	   MRI_IMAGE *im = NULL;
      float *far=NULL;
	    if (narg + 1 >= argc)
		Error_Exit("-roisel option requires a following argument!");
       
       if (!(im = mri_read_1D (argv[++narg]))) {
         Error_Exit("Could not load -roisel file");
       }
       
       /* allocate for roisel */
       roisel = (byte *)calloc(IMAX, sizeof(byte));
       far = MRI_FLOAT_PTR(im);
       for (i=0; i<im->nx*im->ny; ++i) {
         if ((int) far[i]>=0 && (int)far[i] < IMAX) roisel[(int)far[i]] = 1;
         else {
            Error_Exit("ROISEL file has bad mask values");
         }
       }
       mri_clear_data_pointer(im) ;
       mri_free(im); im = NULL; 
       free(far); far=NULL;
      
	    narg++;
	    continue;
	}
   if (strncmp(argv[narg], "-mask", 5) == 0) {
	    if (mask_dset != NULL)
		Error_Exit("Cannot have two -mask options!");

	    if (narg + 1 >= argc)
		Error_Exit("-mask option requires a following argument!");

	    mask_dset = THD_open_dataset(argv[++narg]);		/* 16 Sep 1999 */
       CHECK_OPEN_ERROR(mask_dset,argv[narg]) ;

	    if (DSET_BRICK_TYPE(mask_dset, 0) == MRI_complex)
		Error_Exit("Cannot deal with complex-valued mask dataset!");

	    /* 24 Jan 2000: change narg to narg-1 in this block - RWCox */

	    if (isdigit(argv[narg - 1][5])) {	/* -mask is a subbrik */
		mask_subbrik = (int) atol(argv[narg - 1] + 5);
		if ((mask_subbrik < 0) || (mask_subbrik >= (DSET_NVALS(mask_dset))))
		    Error_Exit("Illegal sub-brisk following the -mask option");
	    }
	    narg++;
	    continue;
	}
/*
   if( strncmp(argv[narg],"-dsub",5) == 0 ){
   if( narg+2 >= argc )
   Error_Exit("-dsub option needs 2 following arguments!\n")  ;

   dmin = (int) strtod( argv[++narg] , NULL ) ;
   dmax = (int) strtod( argv[++narg] , NULL ) ;
   narg++ ; continue ;
   }
 */
	if (strncmp(argv[narg], "-1Dformat", 5) == 0) {
	    disp1d = 1;
	    narg++;
	    continue;
	}
   if (strncmp(argv[narg], "-1DRformat", 5) == 0) {
	    disp1d = 2;
	    narg++;
	    continue;
	}
	if (strncmp(argv[narg], "-sigma", 5) == 0) {
	    sigma = 1;
	    narg++;
	    continue;
	}
	if (strncmp(argv[narg], "-nzsigma", 5) == 0) {
	    nzsigma = 1;
	    narg++;
	    continue;
	}
   if (strncmp(argv[narg], "-median", 5) == 0) {
	    if (nzperc) {
             Error_Exit("perc cannot be used with nzperc");
            }
            perc = 1;
	    narg++;
	    continue;
	}
       if (strncmp(argv[narg], "-nzmedian", 9) == 0) {
	    if (perc) {
             Error_Exit("nzperc cannot be used with perc");
            }
            nzperc = 1;
	    narg++;
	    continue;
	}
	if (strncmp(argv[narg], "-nzmean", 7) == 0) {
	    nzmean = 1;
	    narg++;
	    continue;
	}
	if (strncmp(argv[narg], "-nzsum", 6) == 0) {
	    donzsum = 1;
	    narg++;
	    continue;
	}
	if (strncmp(argv[narg], "-minmax", 5) == 0) {
	    minmax = 1;
	    narg++;
	    continue;
	}
	if (strncmp(argv[narg], "-nzminmax", 5) == 0) {
	    nzminmax = 1;
	    narg++;
	    continue;
	}
	if (strncmp(argv[narg], "-debug", 5) == 0) {
	    debug = 1;
	    narg++;
	    continue;
	}
	if (strncmp(argv[narg], "-quiet", 5) == 0) {
	    quiet = 1;
	    narg++;
	    continue;
	}
	if (strncmp(argv[narg], "-nobriklab", 8) == 0) {  /* 14 Mar 2008 */
	    nobriklab = 1;
	    narg++;
	    continue;
	}
	if (strncmp(argv[narg], "-nomeanout", 8) == 0) {  /* 09 Mar 2010 ZSS*/
	    mean = 0;
	    narg++;
	    continue;
	}
	if (strncmp(argv[narg], "-summary", 5) == 0) {
	    summary = 1;
	    narg++;
	    continue;
	}
	if (strncmp(argv[narg], "-nzvoxels", 5) == 0) {
	    nzcount = 1;
	    narg++;
	    continue;
	}
	/* added 5/00 */
	if (strncmp(argv[narg], "-numROI", 5) == 0) {
	    force_num_ROI = (int) atol(argv[++narg]);
	    narg++;
	    continue;
	}
	if (strncmp(argv[narg], "-zerofill", 5) == 0) {
	    snprintf(zerofill, sizeof(char)*30, "%s", argv[++narg]);
	    narg++;
	    continue;
	}
	Error_Exit("Unknown option");
    }

   if (!mean && summary) {
      Error_Exit("Cannot use -nomeanout with -summary");
   }
    /* Remaining arguements are files */

    if (narg >= argc)
	Error_Exit("No input datasets!?\n");

    if (mask_dset == NULL){          /* 17 March 2005 [rickr] */
        fprintf(stderr,"Error: missing '-mask MASK_DATASET'\n");
        return 1;
    }

    /* check the mask dataset type */
    if (DSET_BRICK_TYPE(mask_dset, 0) == MRI_float && mask_f2s == 0 ){
        fprintf(stderr,"\nError: cannot deal with float-valued mask dataset\n");
        fprintf(stderr,"(consider the -mask_f2short option)\n");
        return 1;
    }


    /* See how many ROIS there are to deal with in the mask */
    for (i = 0; i < IMAX; non_zero[i++] = 0);

    if (roisel) DSET_mallocize(mask_dset); 
    DSET_load(mask_dset);
    if (DSET_ARRAY(mask_dset, mask_subbrik) == NULL)
	Error_Exit("Cannot read in mask dataset BRIK!");

    nvox = DSET_NVOX(mask_dset);
    if (debug)
	fprintf(stderr, "The number of voxels in the mask dataset is %d\n", nvox);

    switch (DSET_BRICK_TYPE(mask_dset, mask_subbrik)) {
    default:
	Error_Exit("Cannot deal with mask dataset datum!");

    case MRI_short:{
	    mask_data = (short *) DSET_ARRAY(mask_dset, mask_subbrik);
	    for (i = 0; i < nvox; i++)
		if (mask_data[i]) {
		  if (!roisel || roisel[mask_data[i]]) {
		    if (debug)
			fprintf(stderr, "Nonzero mask voxel %d value is %d\n", i, mask_data[i]);
		    non_zero[mask_data[i] + 32768] = 1;
        } else {
         if (debug)
			fprintf(stderr, "Ignoring mask voxel %d with value %d not in ROISEL\n", 
                         i, mask_data[i]);
         /* cancel that */
         mask_data[i] = 0;
        }
		}
	    break;
	}

    case MRI_byte:{
	    byte *temp_byte = (byte *) DSET_ARRAY(mask_dset, mask_subbrik);
	    if ((mask_data = (short *) malloc(nvox * sizeof(short))) == NULL)
		 Error_Exit("Memory allocation error");

	    for (i = 0; i < nvox; i++) {
		mask_data[i] = (short) temp_byte[i];
		if (mask_data[i]) {
		  if (!roisel || roisel[mask_data[i]]) {
          if (debug)
			fprintf(stderr, "Nonzero mask voxel %d value is %d\n", i, mask_data[i]);
		    non_zero[mask_data[i] + 32768] = 1;
        } else {
         if (debug)
			fprintf(stderr, "Ignoring mask voxel %d with value %d not in ROISEL\n", 
                         i, mask_data[i]);
         /* cancel that */
         mask_data[i] = 0;
        }
		}
	    }
	    break;
	}

    case MRI_float:{
	    float *temp_float = (float *) DSET_ARRAY(mask_dset, mask_subbrik);
	    if ((mask_data = (short *) malloc(nvox * sizeof(short))) == NULL)
		 Error_Exit("Memory allocation error");

	    for (i = 0; i < nvox; i++) {
		mask_data[i] = (short) temp_float[i];
		if (mask_data[i]) {
		  if (!roisel || roisel[mask_data[i]]) {
		    if (debug)
			fprintf(stderr, "Nonzero mask voxel %d value is %d\n", i, mask_data[i]);
		    non_zero[mask_data[i] + 32768] = 1;
        } else {
         if (debug)
			fprintf(stderr, "Ignoring mask voxel %d with value %d not in ROISEL\n", 
                         i, mask_data[i]);
         /* cancel that */
         mask_data[i] = 0;
        }
		}
	    }
	    break;
	}
    }				/* switch */

    /* Print the header line, while we set up the index array */
    if (!quiet && !summary) {
	   if (disp1d == 1) fprintf(stdout, "#File\tSub-brick\n\t\t#");
      else if (disp1d == 2) fprintf(stdout, "name\t\t");
      else fprintf(stdout, "File\tSub-brick");
    }  
    for (i = 0, num_ROI = 0; i < IMAX; i++)
	if (non_zero[i]) {
	    if (force_num_ROI && (((i - 32768) < 0) || ((i - 32768) > force_num_ROI)))
		Error_Exit("You used the numROI option, yet in the mask there was a\n"
			   "value in the mask outside the range [1 n].\n"
            "Maybe you shouldn't use that option\n");
	    non_zero[i] = num_ROI;
	    num_ROI++;
	    if (!quiet && !summary && !force_num_ROI) {
		if (mean) fprintf(stdout, "\tMean_%d  ", i - 32768);
		if (nzmean)
		    fprintf(stdout, "\tNZMean_%d", i - 32768);
		if (nzcount)
		    fprintf(stdout, "\tNZcount_%d", i - 32768);
		if (sigma)
		    fprintf(stdout, "\tSigma_%d", i - 32768);
		if (nzsigma)
		    fprintf(stdout, "\tNZSigma_%d", i - 32768);
		if (minmax) {
		    fprintf(stdout, "\tMin_%d   ", i - 32768);
		    fprintf(stdout, "\tMax_%d   ", i - 32768);
		}
		if (nzminmax) {
		    fprintf(stdout, "\tNZMin_%d ", i - 32768);
		    fprintf(stdout, "\tNZMax_%d ", i - 32768);
		}
      if (perc) {
         fprintf(stdout, "\tMed_%d ", i - 32768);
      }
      if (nzperc) {
         fprintf(stdout, "\tNZMed_%d ", i - 32768);
      }
      if (donzsum) {
         fprintf(stdout, "\tNZSum_%d ", i - 32768);
      }
	    }
	}
    /* load the non_zero array if the numROI option used - 5/00 */
    if (force_num_ROI) {
	for (i = 1; i <= force_num_ROI; i++) {
	    non_zero[i + 32768] = (i - 1);
	    if (!quiet && !summary) {
		if (mean) fprintf(stdout, "\tMean_%d  ", i );
		if (nzmean)
		    fprintf(stdout, "\tNZMean_%d", i );
		if (nzcount)
		    fprintf(stdout, "\tNZcount_%d", i );
		if (sigma)
		    fprintf(stdout, "\tSigma_%d", i );
		if (nzsigma)
		    fprintf(stdout, "\tNZSigma_%d", i );
		if (minmax) {
		    fprintf(stdout, "\tMin_%d   ", i );
		    fprintf(stdout, "\tMax_%d   ", i );
		}
		if (nzminmax) {
		    fprintf(stdout, "\tNZMin_%d ", i );
		    fprintf(stdout, "\tNZMax_%d ", i );
		}
      if (perc) {
         fprintf(stdout, "\tMed_%d ", i );
      }
      if (nzperc) {
         fprintf(stdout, "\tNZMed_%d ", i );
      }
      if (donzsum) {
         fprintf(stdout, "\tNZSum_%d ", i );
      }
	    }
	}
	num_ROI = force_num_ROI;
    }
    if (!quiet && !summary)
	fprintf(stdout, "\n");

    if (debug)
	fprintf(stderr, "Total Number of ROIs are %d\n", num_ROI);

    /* Now, num_ROI is the number of ROIs from the mask to deal with, 
       and non_zero[mask_data[i]+32768] can be used as in index to 
       a 0..num_ROI array */

    if ((sum = (double *) malloc(num_ROI * sizeof(double))) == NULL)
	 Error_Exit("Memory allocation error");
    if ((voxels = (long *) malloc(num_ROI * sizeof(long))) == NULL)
	 Error_Exit("Memory allocation error");
    if (nzmean || nzcount || nzminmax || donzsum || nzsigma) {
	if ((nzsum = (double *) malloc(num_ROI * sizeof(double))) == NULL)
	     Error_Exit("Memory allocation error");
	if ((nzvoxels = (long *) malloc(num_ROI * sizeof(long))) == NULL)
	     Error_Exit("Memory allocation error");
	if ((nzmin = (double *) malloc(num_ROI * sizeof(double))) == NULL)
	     Error_Exit("Memory allocation error");
	if ((nzmax = (double *) malloc(num_ROI * sizeof(double))) == NULL)
	     Error_Exit("Memory allocation error");
	if ((nzsumsq = (double *) malloc(num_ROI * sizeof(double))) == NULL)
	     Error_Exit("Memory allocation error");
    }
    if ( minmax ) {
	if ((min = (double *) malloc(num_ROI * sizeof(double))) == NULL)
	     Error_Exit("Memory allocation error - min");
	if ((max = (double *) malloc(num_ROI * sizeof(double))) == NULL)
	     Error_Exit("Memory allocation error - max");
    }
    if (sigma)
	if ((sumsq = (double *) malloc(num_ROI * sizeof(double))) == NULL)
	     Error_Exit("Memory allocation error");

    if (summary)
	if ((sumallbriks = (double *) malloc(num_ROI * sizeof(double))) == NULL)
	     Error_Exit("Memory allocation error");

    /* Now, loop over datasets and sub-bricks and compute away */
    for (; narg < argc; narg++) {

	input_dset = THD_open_dataset(argv[narg]);	/* 16 Sep 1999 */

	if (input_dset == NULL) {
	    fprintf(stderr, "Warning: Cannot open input dataset %s\n", argv[narg]);
	    continue;
	}
	if (DSET_NVOX(input_dset) != nvox) {
	    WARNING_message("Input dataset %s is different size than mask - skipping",
                       argv[narg]);
	    continue;
	}
   if( !EQUIV_GRIDS(mask_dset,input_dset) )
     WARNING_message("Input dataset %s grid mismatch from mask",argv[narg]) ;

	DSET_load(input_dset);

	if (summary)
	    for (i = 0; i < num_ROI; i++)
		sumallbriks[i] = 0;

	for (brik = 0; brik < DSET_NVALS(input_dset); brik++) {

	    for (i = 0; i < num_ROI; i++) {
		sum[i] = 0;
		voxels[i] = 0;
		if (nzmean || nzcount || donzsum || nzsigma) {
		    nzsum[i] = 0;
		    nzvoxels[i] = 0;
          nzsumsq[i] = 0;
		}
		if (sigma)
		    sumsq[i] = 0;
	    }

	    switch (DSET_BRICK_TYPE(input_dset, brik)) {

	    case MRI_byte:{
		    float fac = DSET_BRICK_FACTOR(input_dset, brik);
		    if (fac == 0)
			fac = 1.0;
		    temp_datab = (byte *) DSET_ARRAY(input_dset, brik);
		    if ((input_data = (float *) malloc(nvox * sizeof(float))) == NULL)
			 Error_Exit("Memory allocation error");
		    for (i = 0; i < nvox; i++)
			input_data[i] = fac * (float) temp_datab[i];
		    break;
		}

	    case MRI_short:{
		    float fac = DSET_BRICK_FACTOR(input_dset, brik);
		    if (fac == 0)
			fac = 1.0;
		    temp_datas = (short *) DSET_ARRAY(input_dset, brik);
		    if ((input_data = (float *) malloc(nvox * sizeof(float))) == NULL)
			 Error_Exit("Memory allocation error");
		    for (i = 0; i < nvox; i++)
			input_data[i] = fac * (float) temp_datas[i];
		    break;
		}

	    case MRI_float:{
		    float fac = DSET_BRICK_FACTOR(input_dset, brik);
		    input_data = (float *) DSET_ARRAY(input_dset, brik);
		    if (fac == 0)
			fac = 1.0;
		    else
			for (i = 0; i < nvox; input_data[i++] *= fac);
		    break;
		}

	    default:{
		    fprintf(stderr, "Cannot use sub-brick %d for file %s.  Is it complex?\n", brik, argv[narg]);
		    continue;	/* next iteration of loop -> next brick */
		}

	    }			/* switch */

            /* init the min/max values */
	    for (i = 0; i < num_ROI; i++) {
		if ( minmax ) {
		    min[i] =  1e30;    /* oops, don't init outside mask      */
                    max[i] = -1e30;    /* thanks, Shruti  02 Sep 2004 [rickr]*/
		}
		if ( nzminmax ) {
		    nzmin[i] =  1e30;  /* that really big number */
		    nzmax[i] = -1e30;
		}
	    }

	    /* do the stats */

	    
       for (i = 0; i < nvox; i++) {
		if (mask_data[i]) {
		    ROI = non_zero[mask_data[i] + 32768];
          if ((ROI < 0) || (ROI >= num_ROI))
			Error_Exit("Somehow I boned computing how many ROIs existed");

		    if ( minmax ) {
			if (input_data[i] < min[ROI] ) min[ROI] = input_data[i];
			if (input_data[i] > max[ROI] ) max[ROI] = input_data[i];
		    }

		    sum[ROI] += (double) input_data[i];
		    voxels[ROI]++;
		    if (nzmean || nzcount || nzminmax || donzsum || nzsigma) {
			if (input_data[i] != 0.0) {
			    nzsum[ROI] += (double) input_data[i];
			    nzvoxels[ROI]++;
			    if (input_data[i] < nzmin[ROI] )
				nzmin[ROI] = input_data[i];
			    if (input_data[i] > nzmax[ROI] )
				nzmax[ROI] = input_data[i];
             if (nzsigma)
			   nzsumsq[ROI] += input_data[i] * input_data[i];
			}
		    }
		    if (sigma)
			sumsq[ROI] += input_data[i] * input_data[i];
		}
	    }
       /* do the damned median, simple, not fastest implementation but good enough*/
       if (perc || nzperc) {
         percentile = (double *)malloc(sizeof(double)*num_ROI);
         fv = (float *)malloc(sizeof(float)*nvox);
         if (!fv || !percentile) {
            Error_Exit("Failed to allocate for fv");
         }
         for (ROI=0; ROI < num_ROI; ++ROI){ /* ROI */
            nfv = 0;
            for (i = 0; i < nvox; i++) { /* i */
               if (mask_data[i] && ROI == non_zero[mask_data[i] + 32768]) {
                  if (perc) { 
                     fv[nfv] = input_data[i]; ++nfv; 
                  } else { /* non zero only */
                     if (input_data[i] != 0.0) { 
                        fv[nfv] = input_data[i]; ++nfv; 
                     }  
                  }
               }
            }/* i */
            /* get the median */
            percentile[ROI] = (double) qmed_float( nfv , fv ) ;
         } /* ROI */
         free(fv); fv = NULL;
	    }
       
       /* print the next line of results */
	    if (!quiet && !summary){
         if( nobriklab )
		     if (disp1d == 1) fprintf(stdout, "#%s\t%d\n\t\t", argv[narg], brik);
           else if (disp1d == 2) fprintf(stdout, "%s_%d\t\t", argv[narg], brik);
           else fprintf(stdout, "%s\t%d", argv[narg], brik);
         else
           if (disp1d == 1) fprintf(stdout, "#%s\t%d[%-.9s]\n\t\t",   
                           argv[narg],brik,DSET_BRICK_LABEL(input_dset,brik));
           else if (disp1d == 2) fprintf(stdout, "%s_%d[%-.9s]\t\t",   
                           argv[narg],brik,DSET_BRICK_LABEL(input_dset,brik));
           else fprintf(stdout, "%s\t%d[%-.9s]",   /* 14 Mar 2008 */
                           argv[narg],brik,DSET_BRICK_LABEL(input_dset,brik));
       }
	    if (!summary) {
		for (i = 0; i < num_ROI; i++) {
		    if (voxels[i]) {	/* possible if the numROI option is used - 5/00 */
			if (mean) fprintf(stdout, "\t%f", (sum[i] / (double) voxels[i]));
			if (nzmean)
			    fprintf(stdout, "\t%f", nzvoxels[i] ? (nzsum[i] / (double) nzvoxels[i]) : 0.0);
			if (nzcount)
			    fprintf(stdout, "\t%ld", nzvoxels[i]);
			if (sigma) {
			    double mean = sum[i] / (double) voxels[i];
			    sumsq[i] /= (double) voxels[i];
			    if (voxels[i] == 1)
				sig = 1e30;	/* a really big number */
			    else
				sig = sqrt((voxels[i] / (voxels[i] - 1)) * (sumsq[i] - mean * mean));
			    fprintf(stdout, "\t%f", sig);
			}
         if (nzsigma) {
			    double mean = 0.0;
             sig = 0.0;
             if (nzvoxels[i]) {
                  mean = nzsum[i] / (double) nzvoxels[i];
			         nzsumsq[i] /= (double) nzvoxels[i];
			       if (nzvoxels[i] == 1)
				   sig = 1e30;	/* a really big number */
			       else
				   sig = sqrt( (nzvoxels[i] / (nzvoxels[i] - 1)) * 
                           (nzsumsq[i] - mean * mean) );
             } 
			    fprintf(stdout, "\t%f", sig);
			}
			if (minmax) {
			    fprintf(stdout, "\t%f", min[i] );
			    fprintf(stdout, "\t%f", max[i] );
			}
			if (nzminmax) {
			    fprintf(stdout, "\t%f", nzmin[i] );
			    fprintf(stdout, "\t%f", nzmax[i] );
			}
         if (perc || nzperc) {
			    fprintf(stdout, "\t%f", percentile[i] );
			}
         if (donzsum) {
             fprintf(stdout, "\t%f", nzsum[i]);
		   }
          } else {	/* no voxels, so just leave blanks */
			if (mean) fprintf(stdout, "\t%s", zerofill);
			if (nzmean)
			    fprintf(stdout, "\t%s", zerofill);
			if (nzcount)
			    fprintf(stdout, "\t%s", zerofill);
			if (sigma)
			    fprintf(stdout, "\t%s", zerofill);
			if (nzsigma)
			    fprintf(stdout, "\t%s", zerofill);
			if (minmax) {
			    fprintf(stdout, "\t%s", zerofill);
			    fprintf(stdout, "\t%s", zerofill);
			}
			if (nzminmax) {
			    fprintf(stdout, "\t%s", zerofill);
			    fprintf(stdout, "\t%s", zerofill);
			}
         if (perc || nzperc)
			    fprintf(stdout, "\t%s", zerofill);
         if (donzsum)
             fprintf(stdout, "\t%s", zerofill);
		    }
		}		/* loop over ROI for print */

		fprintf(stdout, "\n");
	    } else
		for (i = 0; i < num_ROI; i++) {
		    if (nzmean)
			sumallbriks[i] += nzvoxels[i] ? (double) (nzsum[i] / (double) nzvoxels[i]) : 0.0;
		    else
			sumallbriks[i] += (double) (sum[i] / (double) voxels[i]);
		}


	    if (DSET_BRICK_TYPE(input_dset, brik) != MRI_float)
		free(input_data);

	}			/* loop over sub-bricks in the input dataset */

	if (summary) {
	    fprintf(stdout, "Average Across %i sub-briks in input dataset %s\n", DSET_NVALS(input_dset), argv[narg]);
	    if (nzmean)
		fprintf(stdout, "Non-zero voxels only!\n");
	    fprintf(stdout, "ROI\t");
	    for (i = 0; i < num_ROI; i++)
		fprintf(stdout, "%i\t", i);
	    fprintf(stdout, "\nVoxels\t");
	    for (i = 0; i < num_ROI; i++)
		fprintf(stdout, "%ld\t", nzmean ? nzvoxels[i] : voxels[i]);
	    fprintf(stdout, "\nValue\t");
	    for (i = 0; i < num_ROI; i++)
		fprintf(stdout, "%f\t", (sumallbriks[i] / (double) DSET_NVALS(input_dset)));
	    fprintf(stdout, "\n");
	}
	DSET_unload(input_dset);

    }				/* loop over input files */

    if (DSET_BRICK_TYPE(mask_dset, mask_subbrik) == MRI_byte)
	free(mask_data);
    DSET_unload(mask_dset);

    free(sum);
    free(voxels);
    if (nzmean || nzcount || donzsum) {
	free(nzsum);
	free(nzvoxels);
    }
    if (sigma)
	free(sumsq);
    if (nzsigma)
	free(nzsumsq);
    if (perc || nzperc) {
	if (percentile) free(percentile); percentile = NULL;
	 }
    if (roisel) free(roisel); roisel=NULL;
    exit(0);
}
