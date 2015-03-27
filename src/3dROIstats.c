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
    fprintf(stderr, "\nError 3dROIstats: %s\n", message);
    exit(1);
}

void usage_3dROIstats(int detail) {

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
"                 in the output file. This option is useless without -numROI.\n"
"                 The option -zerofill defaults to '0'.\n"
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
"  -mode       Compute the mode of all voxels. (integral valued sets only)\n"
"  -nzmode     Compute the mode of non_zero voxels.\n"
"  -pcxyz      Compute the principal direction of the voxels in the ROI \n"
"              including the three eigen values. You'll get 12 values out\n"
"              per ROI, per sub-brick, with this option.\n"
"                 pc0x pc0y pc0z pc1x pc1y pc1z pc2x pc2y pc2z eig0 eig1 eig2\n"
"  -nzpcxyz    Same as -pcxyz, but exclude zero valued voxels.\n"
"  -pcxyz+     Same as -pcxyz, but also with FA, MD, Cl, Cp, and Cs computed \n"
"              from the three eigen values.\n"
"              You will get 17 values out per ROI, per sub-brick, beginning\n"
"              with all the values from -pcxyz and -nzpcxyz then followed by\n"
"                 FA MD Cl Cp Cs\n"
"  -nzpcxyz+   Same as -nzpcxyz, but also with FA, MD, Cl, Cp, and Cs.\n"
"  -key        Output the integer key for the ROI in question\n"
"\n"
"The output is printed to stdout (the terminal), and can be\n"
"saved to a file using the usual redirection operation '>'.\n"
"\n"
"N.B.: The input datasets and the mask dataset can use sub-brick\n"
"      selectors, as detailed in the output of 3dcalc -help.\n"
	    );

	printf("\n" MASTER_SHORTHELP_STRING);

	PRINT_COMPILE_DATE ;    
   return;
}

int main(int argc, char *argv[])
{
    THD_3dim_dataset *mask_dset = NULL, *input_dset = NULL ;
    int mask_subbrik = 0;
    int sigma = 0, nzsigma = 0, mean = 1, nzmean = 0, nzcount = 0;
    int debug = 0, quiet = 0, summary = 0;
    char wpc[2] = {""};
    int minmax = 0, nzminmax = 0, donzsum = 0;		/* 07 July, 2004 [rickr] */
    short *mask_data;
    int nvox, i, brik, k;
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
    int *iv=NULL, niv=0, *modes=NULL;
    double *pvec=NULL, *eigv=NULL;
    int nfv = 0, perc = 0, nzperc = 0, mode = 0, nzmode=0, 
        pcxyz=0, nzpcxyz=0, key=0, *keys=NULL;
    int nobriklab=0 ;  /* 14 Mar 2008 */
    int disp1d=1;   /* ZSS May 2008 */
    byte *roisel=NULL;
    char sbuf[257]={""};
    char sklab[128]={""};
    char zerofill[32]={"0"};

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dROIstats");machdep() ; AFNI_logger("3dROIstats",argc,argv) ;

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   /* scan argument list */
   if (argc == 1) { usage_3dROIstats(1); exit(0); } /* Bob's help shortcut */

   disp1d = 0;
   mean = 1;   /* LEAVE this as the default ZSS March 09 2010 */
    while (narg < argc && argv[narg][0] == '-') {

    if (strcmp(argv[narg], "-h") == 0 || strcmp(argv[narg], "-help") == 0) {
      usage_3dROIstats(strlen(argv[narg]) > 3 ? 2:1);
      exit(0);
    }


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
   
   if (strcmp(argv[narg], "-wpc") == 0) {
	    wpc[0] = 'w'; wpc[1] = '\0';
	    narg++;
	    continue;
	}
   
   if (strcmp(argv[narg], "-pcxyz") == 0) {
	    if (nzpcxyz) {
          Error_Exit("mode cannot be used with nzpcxyz");
         }
            pcxyz = 1;
	    narg++;
	    continue;
	}
   if (strcmp(argv[narg], "-nzpcxyz") == 0) {
	    if (pcxyz) {
          Error_Exit("mode cannot be used with pcxyz");
         }
            nzpcxyz = 1;
	    narg++;
	    continue;
	}
   if (strcmp(argv[narg], "-pcxyz+") == 0) {
	    if (nzpcxyz) {
          Error_Exit("mode cannot be used with nzpcxyz");
         }
            pcxyz = 2;
	    narg++;
	    continue;
	}
   if (strcmp(argv[narg], "-nzpcxyz+") == 0) {
	    if (pcxyz) {
          Error_Exit("mode cannot be used with pcxyz");
         }
            nzpcxyz = 2;
	    narg++;
	    continue;
	}
   if (strcmp(argv[narg], "-key") == 0) {
	    key = 1;
	    narg++;
	    continue;
	}
   if (strcmp(argv[narg], "-mode") == 0) {
	    if (nzmode) {
             Error_Exit("mode cannot be used with nzmode");
            }
            mode = 1;
	    narg++;
	    continue;
	}
   if (strcmp(argv[narg], "-nzmode") == 0) {
	    if (mode) {
             Error_Exit("nzmode cannot be used with mode");
            }
            nzmode = 1;
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

	   ERROR_message("Unknown option %s\n", argv[narg]);
      suggest_best_prog_option(argv[0], argv[narg]);
      exit(1);
    }

    if (argc < 3) {
      ERROR_message("Too few options, use -help for details");
      exit(1);
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

    if (roisel) DSET_mallocize(mask_dset);
    DSET_load(mask_dset);
    if (DSET_ARRAY(mask_dset, mask_subbrik) == NULL)
      Error_Exit("Cannot read in mask dataset BRIK!");

    /* check the mask dataset type */
    if( DSET_BRICK_TYPE(mask_dset,mask_subbrik) == MRI_float && mask_f2s == 0 ){
        if( !is_integral_sub_brick(mask_dset,mask_subbrik,1) ){ /* 15 Sep 2010 [RWC] */
          ERROR_message("Cannot deal with float-valued mask dataset\n"
                        "       [consider using the -mask_f2short option]");
          return 1;
        } else {
          mask_f2s = 1 ;
        }
    }


    /* See how many ROIS there are to deal with in the mask */
    for (i = 0; i < IMAX; non_zero[i++] = 0);

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
	    if ( force_num_ROI &&
            (((i - 32768) < 0) || ((i - 32768) > force_num_ROI))){
		   snprintf(sbuf, 256,
               "You used numROI %d, yet in the mask there was a\n"
			      "value (%d) outside the range [1 %d].\n",
               force_num_ROI, (i - 32768), force_num_ROI);
         Error_Exit(sbuf);
       }

	    non_zero[i] = num_ROI;
	    num_ROI++;
	    if (!quiet && !summary && !force_num_ROI) {
          AFNI_get_dset_val_label(mask_dset, (double)(i - 32768), sklab);
          if (sklab[0]=='\0') {
            sprintf(sklab,"%d",i - 32768);
          }
		if (mean) fprintf(stdout, "\tMean_%s  ", sklab);
		if (nzmean)
		    fprintf(stdout, "\tNZMean_%s", sklab);
		if (nzcount)
		    fprintf(stdout, "\tNZcount_%s", sklab);
		if (sigma)
		    fprintf(stdout, "\tSigma_%s", sklab);
		if (nzsigma)
		    fprintf(stdout, "\tNZSigma_%s", sklab);
		if (minmax) {
		    fprintf(stdout, "\tMin_%s   ", sklab);
		    fprintf(stdout, "\tMax_%s   ", sklab);
		}
		if (nzminmax) {
		    fprintf(stdout, "\tNZMin_%s ", sklab);
		    fprintf(stdout, "\tNZMax_%s ", sklab);
		}
      if (perc) {
         fprintf(stdout, "\tMed_%s ", sklab);
      }
      if (nzperc) {
         fprintf(stdout, "\tNZMed_%s ", sklab);
      }
      if (donzsum) {
         fprintf(stdout, "\tNZSum_%s ", sklab);
      }
      if (mode) {
         fprintf(stdout, "\tMod_%s ", sklab);
      }
      if (nzmode) {
         fprintf(stdout, "\tNZMod_%s ", sklab);
      }
      if (pcxyz) {
         for (k=0; k< 3; ++k) fprintf(stdout, "\t%spc0%c_%s ",wpc,'x'+k, sklab);
         for (k=0; k< 3; ++k) fprintf(stdout, "\t%spc1%c_%s ",wpc,'x'+k, sklab);
         for (k=0; k< 3; ++k) fprintf(stdout, "\t%spc2%c_%s ",wpc,'x'+k, sklab);
         for (k=0; k< 3; ++k) fprintf(stdout, "\t%seig%d_%s ",wpc,k, sklab);
         if (pcxyz == 2) {
            fprintf(stdout, "\t%sFA_%s ",wpc,sklab);
            fprintf(stdout, "\t%sMD_%s ",wpc,sklab);
            fprintf(stdout, "\t%sCl_%s ",wpc,sklab);
            fprintf(stdout, "\t%sCp_%s ",wpc,sklab);
            fprintf(stdout, "\t%sCs_%s ",wpc,sklab);
         }
      }
      if (nzpcxyz) {
         for (k=0; k< 3; ++k) fprintf(stdout,"\t%sNZpc0%c_%s ",wpc,'x'+k, sklab);
         for (k=0; k< 3; ++k) fprintf(stdout,"\t%sNZpc1%c_%s ",wpc,'x'+k, sklab);
         for (k=0; k< 3; ++k) fprintf(stdout,"\t%sNZpc2%c_%s ",wpc,'x'+k, sklab);
         for (k=0; k< 3; ++k) fprintf(stdout,"\t%sNZeig%d_%s ",wpc,k, sklab);
         if (nzpcxyz == 2) {
            fprintf(stdout, "\t%sNZFA_%s ",wpc,sklab);
            fprintf(stdout, "\t%sNZMD_%s ",wpc,sklab);
            fprintf(stdout, "\t%sNZCl_%s ",wpc,sklab);
            fprintf(stdout, "\t%sNZCp_%s ",wpc,sklab);
            fprintf(stdout, "\t%sNZCs_%s ",wpc,sklab);
         }
      }
      if (key) {
         fprintf(stdout, "\tKey_%s ", sklab);
      }
	    }
	}
    /* load the non_zero array if the numROI option used - 5/00 */
    if (force_num_ROI) {
	for (i = 1; i <= force_num_ROI; i++) {
	    non_zero[i + 32768] = (i - 1);
	    if (!quiet && !summary) {
          AFNI_get_dset_val_label(mask_dset, (double)(i), sklab);
          if (sklab[0]=='\0') {
            sprintf(sklab,"%d",i);
          }
		if (mean) fprintf(stdout, "\tMean_%s  ", sklab );
		if (nzmean)
		    fprintf(stdout, "\tNZMean_%s", sklab );
		if (nzcount)
		    fprintf(stdout, "\tNZcount_%s", sklab );
		if (sigma)
		    fprintf(stdout, "\tSigma_%s", sklab );
		if (nzsigma)
		    fprintf(stdout, "\tNZSigma_%s", sklab );
		if (minmax) {
		    fprintf(stdout, "\tMin_%s   ", sklab );
		    fprintf(stdout, "\tMax_%s   ", sklab );
		}
		if (nzminmax) {
		    fprintf(stdout, "\tNZMin_%s ", sklab );
		    fprintf(stdout, "\tNZMax_%s ", sklab );
		}
      if (perc) {
         fprintf(stdout, "\tMed_%s ", sklab );
      }
      if (nzperc) {
         fprintf(stdout, "\tNZMed_%s ", sklab );
      }
      if (donzsum) {
         fprintf(stdout, "\tNZSum_%s ", sklab );
      }
      if (mode) {
         fprintf(stdout, "\tMod_%s ", sklab );
      }
      if (nzmode) {
         fprintf(stdout, "\tNZMod_%s ", sklab );
      }
      if (pcxyz) {
         for (k=0; k< 3; ++k) fprintf(stdout, "\t%spc0%c_%s ", wpc,'x'+k, sklab);
         for (k=0; k< 3; ++k) fprintf(stdout, "\t%spc1%c_%s ", wpc,'x'+k, sklab);
         for (k=0; k< 3; ++k) fprintf(stdout, "\t%spc2%c_%s ", wpc,'x'+k, sklab);
         for (k=0; k< 3; ++k) fprintf(stdout, "\t%seig%d_%s ", wpc,k, sklab);
         if (pcxyz == 2) {
            fprintf(stdout, "\t%sFA_%s ", wpc,sklab);
            fprintf(stdout, "\t%sMD_%s ", wpc,sklab);
            fprintf(stdout, "\t%sCl_%s ", wpc,sklab);
            fprintf(stdout, "\t%sCp_%s ", wpc,sklab);
            fprintf(stdout, "\t%sCs_%s ", wpc,sklab);
         }      }
      if (nzpcxyz) {
         for (k=0; k< 3; ++k) fprintf(stdout,"\t%sNZpc0%c_%s ",wpc,'x'+k, sklab);
         for (k=0; k< 3; ++k) fprintf(stdout,"\t%sNZpc0%c_%s ",wpc,'x'+k, sklab);
         for (k=0; k< 3; ++k) fprintf(stdout,"\t%sNZpc2%c_%s ",wpc,'x'+k, sklab);
         for (k=0; k< 3; ++k) fprintf(stdout,"\t%sNZeig%d_%s ",wpc,k, sklab);
         if (nzpcxyz == 2) {
            fprintf(stdout, "\t%sNZFA_%s ", wpc,sklab);
            fprintf(stdout, "\t%sNZMD_%s ", wpc,sklab);
            fprintf(stdout, "\t%sNZCl_%s ", wpc,sklab);
            fprintf(stdout, "\t%sNZCp_%s ", wpc,sklab);
            fprintf(stdout, "\t%sNZCs_%s ", wpc,sklab);
         }
      }
      if (key) {
         fprintf(stdout, "\tKey_%s ", sklab);
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
	    WARNING_message(
         "Dataset %s has %d voxels/nodes while the mask has %d - skipping",
                       argv[narg], DSET_NVOX(input_dset), nvox);
	    continue;
	}
   if( !EQUIV_GRIDS(mask_dset,input_dset) )
     WARNING_message("Input dataset %s grid mismatch from mask.\n"
             "Try the following command for grid comparison:\n"
             " 3dinfo -header_line -prefix -same_all_grid %s %s\n"
             ,argv[narg], 
             DSET_HEADNAME(mask_dset), DSET_HEADNAME(input_dset)) ;

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
		    else {
			DSET_unload(input_dset); 
         DSET_mallocize(input_dset); DSET_load(input_dset);
         input_data = (float *) DSET_ARRAY(input_dset, brik);
         for (i = 0; i < nvox; input_data[i++] *= fac);
		    }
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
       
       /* do the damned median, simple, not fastest implementation 
          but good enough*/
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
       
       /* do the mode */
       if (mode || nzmode) {
         modes = (int *)malloc(sizeof(int)*num_ROI);
         iv = (int *)malloc(sizeof(int)*nvox);
         if (!iv || !modes) {
            Error_Exit("Failed to allocate for iv");
         }
         for (ROI=0; ROI < num_ROI; ++ROI){ /* ROI */
            memset(iv, 0, sizeof(int)*nvox);
            niv = 0;
            for (i = 0; i < nvox; i++) { /* i */
               if (mask_data[i] && ROI == non_zero[mask_data[i] + 32768]) {
                  if (mode) {
                     iv[niv] = (int)input_data[i]; ++niv;
                  } else { /* non zero only */
                     if (input_data[i] != 0.0) {
                        iv[niv] = (int)input_data[i]; ++niv;
                     }
                  }
               }
            }/* i */
            /* get the mode */
            if (niv) {
               modes[ROI] = qmode_int( iv, niv) ;
            } else {
               WARNING_message("ROI %d has no values in it, mode set to 0\n",
                              ROI);
               modes[ROI] = 0;
            }
         } /* ROI */
         free(iv); iv = NULL;
	    }
       
       /* do the XYZ PCA */
       if (pcxyz || nzpcxyz) {
         float *xyzp=NULL, fac=1.0;
         double pc_vec[9], pc_eig[3], trace, sfac;
         int nvi, nvij, N_xyz=0;
         THD_fvec3 fv3;
         THD_ivec3 vi;
         nvi   = DSET_NX(input_dset);
         nvij  = nvi * DSET_NY(input_dset);
         pvec = (double *)malloc(sizeof(double)*num_ROI*9);
         eigv = (double *)malloc(sizeof(double)*num_ROI*3);
         for (ROI=0; ROI < num_ROI; ++ROI){ /* ROI */
            N_xyz = 0;
            for (i = 0; i < nvox; i++) {
               if (mask_data[i] && ROI == non_zero[mask_data[i] + 32768]) {
                  if (pcxyz || input_data[i] != 0.0) ++N_xyz;
               }
            }
            pvec[9*ROI  ] = pvec[9*ROI+1] =  pvec[9*ROI+2] = 
            pvec[9*ROI+3] = pvec[9*ROI+4] =  pvec[9*ROI+5] = 
            pvec[9*ROI+6] = pvec[9*ROI+7] =  pvec[9*ROI+8] = 0.0;
            eigv[3*ROI  ] = eigv[3*ROI+1] =  eigv[3*ROI+2] = 0.0;
            if (N_xyz) {
               xyzp = (float *)malloc(sizeof(float)*N_xyz*3);
               k = 0;
               fac = 1.0; sfac = 1.0;
               for (i = 0; i < nvox; i++) { /* i */
                  if (wpc[0] == 'w') {
                     fac = input_data[i];
                     sfac += fac;
                  } else fac = 1.0;
                  if (mask_data[i] && ROI == non_zero[mask_data[i] + 32768]) {
                     if (pcxyz) {
                        AFNI_1D_to_3D_index(i, vi.ijk[0], vi.ijk[1], vi.ijk[2], 
                                               nvi, nvij);
                        fv3 = THD_3dind_to_dicomm_no_wod(input_dset, vi);
                        xyzp[k        ] = fv3.xyz[0]*fac; 
                        xyzp[k+N_xyz  ] = fv3.xyz[1]*fac; 
                        xyzp[k+N_xyz*2] = fv3.xyz[2]*fac; 
                        ++k;
                     } else { /* non zero only */
                        AFNI_1D_to_3D_index(i, vi.ijk[0], vi.ijk[1], vi.ijk[2], 
                                               nvi, nvij);
                        fv3 = THD_3dind_to_dicomm_no_wod(input_dset, vi);
                        if (input_data[i] != 0.0) {
                           xyzp[k        ] = fv3.xyz[0]*fac; 
                           xyzp[k+N_xyz  ] = fv3.xyz[1]*fac; 
                           xyzp[k+N_xyz*2] = fv3.xyz[2]*fac; 
                           ++k;
                        }
                     }
                  }
               }/* i */
               
               if ((trace = pca_fast3(xyzp, N_xyz, 1, pc_vec, pc_eig)) < 0) {
                  ERROR_message("Failed calculating PC for %dth ROI, \n"
                                "setting all 0\n", ROI);
               } else {
                  pvec[9*ROI+0] = pc_vec[0];
                  pvec[9*ROI+1] = pc_vec[3];
                  pvec[9*ROI+2] = pc_vec[6];
                  pvec[9*ROI+3] = pc_vec[1];
                  pvec[9*ROI+4] = pc_vec[4];
                  pvec[9*ROI+5] = pc_vec[7];
                  pvec[9*ROI+6] = pc_vec[2];
                  pvec[9*ROI+7] = pc_vec[5];
                  pvec[9*ROI+8] = pc_vec[8];
                  for (k=0; k<3; ++k) eigv[3*ROI+k] = pc_eig[k];
               }
               free(xyzp); xyzp=NULL;
            }
         }
       }
       
       if (key) {
         keys = (int*)malloc(sizeof(int)*num_ROI);
         for (ROI=0; ROI < num_ROI; ++ROI){ /* ROI */
            keys[ROI] = -1;
            for (i = 0; i < nvox; i++) {
               if (mask_data[i] && ROI == non_zero[mask_data[i] + 32768]) {
                  keys[ROI] = mask_data[i];
                  break;
               }
            }
         }
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
         if (mode || nzmode) {
			    fprintf(stdout, "\t%d", modes[i] );
			}
         if (pcxyz || nzpcxyz) {
            fprintf(stdout, "\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t\t%f\t%f\t%f",
                  pvec[9*i  ], pvec[9*i+1], pvec[9*i+2],
                  pvec[9*i+3], pvec[9*i+4], pvec[9*i+5],
                  pvec[9*i+6], pvec[9*i+7], pvec[9*i+8],
                  eigv[3*i  ], eigv[3*i+1], eigv[3*i+2]);
            if (pcxyz == 2 || nzpcxyz == 2) {
               double md, fa, cl, cp, cs;
               /* FA and Cl, Cp, Cs */
               md = (eigv[3*i  ]+eigv[3*i+1]+eigv[3*i+2])/3.0;
               fa = sqrt(3*((eigv[3*i  ]-md)*(eigv[3*i  ]-md) +
                            (eigv[3*i+1]-md)*(eigv[3*i+1]-md) +
                            (eigv[3*i+2]-md)*(eigv[3*i+2]-md))) / 
                    sqrt(2* (eigv[3*i  ]*eigv[3*i  ]+
                             eigv[3*i+1]*eigv[3*i+1]+
                             eigv[3*i+2]*eigv[3*i+2]));
               cl = (eigv[3*i  ]-eigv[3*i+1])/(3.0*md);
               cp = (eigv[3*i+1]-eigv[3*i+2])/(3.0*md)*2.0;
               cs = eigv[3*i+2]/md;
               
               fprintf(stdout, "\t%f\t%f\t%f\t%f\t%f",
                                 fa, md, cl, cp, cs );
            }
         }
         if (key) {
            fprintf(stdout, "\t%d", keys[i]);
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
		   if (mode || nzmode)
			    fprintf(stdout, "\t%s", zerofill); 
         if (pcxyz || nzpcxyz) {
            fprintf(stdout, "\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t\t%s\t%s\t%s",
                  zerofill, zerofill, zerofill, 
                  zerofill, zerofill, zerofill, 
                  zerofill, zerofill, zerofill, 
                  zerofill, zerofill, zerofill);
            if (pcxyz == 2 || nzpcxyz == 2) {
               fprintf(stdout, "\t%s\t%s\t%s\t%s\t%s",
                     zerofill, zerofill, zerofill, 
                     zerofill, zerofill);
            }
         }
         if (key) {
            fprintf(stdout, "\t%s", zerofill);
         }
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
    if (mode || nzmode) {
	if (modes) free(modes); modes = NULL;
	 }
    if (pcxyz || nzpcxyz) {
   if (pvec) free(pvec); pvec = NULL;
   if (eigv) free(eigv); eigv = NULL;
    }
    if (keys) free(keys); keys=NULL;
    if (roisel) free(roisel); roisel=NULL;
    exit(0);
}
