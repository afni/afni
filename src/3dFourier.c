/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*******************************************************
 * 3dFourier                                           *
 * T. Ross  and K. Heimerl 7/99                        *
 *******************************************************/

#include "mrilib.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#if !defined(TRUE)
#define TRUE (1==1)
#define FALSE !TRUE
#endif

static void *My_Malloc(size_t);

void Error_Exit(char *message)
{
	fprintf(stderr, "\n\nError in 3dFourier:\n%s\n\nTry 3dFourier -help\n",message);
	exit(1);
}

static int    g_argc ;  /* Jul 2010 */
static char **g_argv ;
#define ARGC g_argc
#define ARGV g_argv

/* include the filter and filter driver */
#include "fourier_filter.c"

/***********************************************************************************/

void  help_message(void)
{
	printf(
		"3dFourier \n"
		"(c) 1999 Medical College of Wisconsin\n"
		"by T. Ross and K. Heimerl\n"
		"Version 0.8 last modified 8-17-99\n\n"
		"Usage: 3dFourier [options] dataset\n\n"
      "* Program to lowpass and/or highpass each voxel time series in a\n"
      "  dataset, via the FFT.\n"
      "* Also see program 3dBandpass for something similar.\n\n"
		"The parameters and options are:\n"
		"	dataset		an afni compatible 3d+time dataset to be operated upon\n"
		"	-prefix name	output name for new 3d+time dataset [default = fourier]\n"
		"	-lowpass f 	low pass filter with a cutoff of f Hz\n"
		"	-highpass f	high pass filter with a cutoff of f Hz\n"
		"	-ignore n	ignore the first n images [default = 1]\n"
/*		"	-autocorr	compute the autocorrelation of the data\n" */
		"	-retrend	Any mean and linear trend are removed before filtering.\n"
		"			This will restore the trend after filtering.\n"
		"\nNote that by combining the lowpass and highpass options, one can construct\n"
		"bandpass and notch filters\n"
	);

        printf("\n" MASTER_SHORTHELP_STRING ) ;
        PRINT_COMPILE_DATE ;
}

/*--------------------------------------------------------------------------*/

int main(int argc, char *argv[])
{
	int j, narg=1, autocorr=FALSE, retrend=FALSE, ignore=1;
	char *prefix=NULL, *err;
	float low_fc = 0.0, high_fc = 0.0;
	THD_3dim_dataset *input=NULL;

	if( argc < 2 || strcmp(argv[1],"-help") == 0 ){ help_message(); exit(0); }

   g_argc = argc ; g_argv = argv ;  /* Jul 2010: save global copies */

   machdep(); PRINT_VERSION("3dFourier"); AFNI_logger("3dFourier",argc,argv);

	/* Loop over arguements and pull out what we need */
	while( narg < argc && argv[narg][0] == '-' ){

		if( strncmp(argv[narg],"-prefix",5) == 0 ) {
			narg++;
			if (narg==argc)
				Error_Exit("-prefix must be followed by a file name");
			j = strlen(argv[narg]);
			prefix =(char *)My_Malloc((j+1)*sizeof(char));
			strcpy(prefix, argv[narg++]);
			continue;
		}

		if (strncmp(argv[narg], "-lowpass", 5) == 0) {
			narg++;
			if (narg==argc)
				Error_Exit("-lowpass must be followed by a frequency");
			low_fc = (float)atof(argv[narg++]);
			continue;
		}

		if (strncmp(argv[narg], "-highpass", 5) == 0) {
			narg++;
			if (narg==argc)
				Error_Exit("-highpass must be followed by a frequency");
			high_fc = (float)atof(argv[narg++]);
			continue;
		}

		if (strncmp(argv[narg], "-ignore", 5) == 0) {
			narg++;
			if (narg==argc)
				Error_Exit("-ignore must be followed by an integer");
			ignore = (int)atol(argv[narg++]);
			if ((ignore<0) || (ignore>10))
				Error_Exit("-ignore must be between 0 and 10, inclusive");
			continue;
		}
/*
		if (strncmp(argv[narg], "-autocorr", 5) == 0) {
			narg++;
			autocorr = TRUE;
			continue;
		}
*/
		if (strncmp(argv[narg], "-retrend", 5) == 0) {
			narg++;
			retrend = TRUE;
			continue;
		}

		if (strncmp(argv[narg], "-help", 5) == 0) {
			help_message();
		}


		Error_Exit("Illegal or unrecoginized option");

	} /* end of while over arguements */

#if 1
   if( low_fc > 0.0f && high_fc > 0.0f && low_fc <= high_fc )
     fprintf(stderr,"** WARNING: lowpass=%f is <= than highpass=%f\n"
                    "** -------: results from 3dFourier are suspect!\n"  ,
             low_fc,high_fc) ;
#endif

	if( narg >= argc )
		Error_Exit("No input datasets!?\n");

	if(prefix==NULL) {
		prefix=(char *)My_Malloc(8*sizeof(char));
		strcpy(prefix,"fourier");
	}

	/* try to open input dataset */

	input = THD_open_dataset( argv[narg] ) ; /* 16 Sep 1999 */
   CHECK_OPEN_ERROR(input,argv[narg]) ;

	if (!DSET_GRAPHABLE(input))
		Error_Exit("Input dataset is not a 3d+time dataset");

	if (DSET_BRICK_TYPE(input, 0)==MRI_complex)
		Error_Exit("Sorry, I can't deal with complex 3d+time datasets");

	err=Fourier_Filter_Driver(input, low_fc, high_fc, ignore, autocorr, retrend, prefix);
	if (err!=NULL)
		Error_Exit(err);

	return 0;  /* All went well exit code */
}
