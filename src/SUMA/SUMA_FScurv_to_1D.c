#include "SUMA_suma.h"

void usage_SUMA_FScurv_to_1D_Main ()

  {/*Usage*/
      static char FuncName[]={"usage_FScurv_to_1D"};
      char * s = NULL;

      SUMA_ENTRY;

          printf ("\n"
                  "Usage:  FScurv_to_1D [-skip_coords] [-output outfile] -input curv_name.asc  \n"
                  "   Reads in a FreeSurfer curvature file and writes it out in 1D format. \n"
                  "   But the format is 1D to begin with, so 'what is that program for?' you ask. \n"
                  "   Not much, I say. It is used to test a SUMA function and also allows you\n"
                  "   to select the node index and data values from the 5 columns of the curv files.\n"
                  "\n"
                  "   -input curv_name.asc: name of ASCII curvature file. To change a curvature file \n"
                  "                     to ASCII, use mris_convert -c curv_name surf curvfile.asc \n"
                  "                     surf is the surface over which the curvfile is defined, like\n"
                  "                     lh.inflated.\n"
                  "   -skip_coords: If specified, the node coordinates are not included in the output.\n"
                  "   -output outfile: If specified, the output goes to a file instead of stdout, \n"
                  "                    which is the screen\n"
                  "\n");
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
       exit (0);
  }/*Usage*/

int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"FScurv_to_1D"};
   int i, j, id, nrows=0, ncols=0, kar;
   float *v = NULL;
   char *outname = NULL;
   char *fname = NULL;
   FILE *outfile=NULL;
   SUMA_Boolean SkipCoords = NOPE, brk, rowmajor;
   SUMA_Boolean LocalHead = NOPE;

	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}

   /* parse command line */
   kar = 1;
   outname = NULL;
   fname = NULL;
   SkipCoords = NOPE;
   rowmajor = YUP;  /* just to test the function's execution */
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_FScurv_to_1D_Main();
          exit (0);
		}
      if (!brk && ( (strcmp(argv[kar], "-skip_coords") == 0) ) ) {
			SkipCoords = YUP;
         brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-output") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -output\n");
				exit (1);
			}
         outname = argv[kar];
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -input\n");
				exit (1);
			}
         fname = argv[kar];
			brk = YUP;
		}
      if (!brk) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {
			brk = NOPE;
			kar ++;
		}
   }

   if (!fname) {
      SUMA_SL_Err("No input file specified.");
      exit(1);
   }
   /* work the output name */
   if (!outname) {
      outfile = SUMA_STDOUT;
   } else {
      outname = SUMA_Extension(outname, ".1D", NOPE); /* outname should be freed at the end */
      if (SUMA_filexists(outname)) {
         fprintf(SUMA_STDERR,"Error %s: Output file %s exists, will not overwrite.\n", FuncName, outname);
         exit(1);
      }
      outfile = fopen(outname, "w");
      if (!outfile) {
         SUMA_SL_Crit("Failed to open file for writing.\n"
                      "Check file permissions.");
         exit(1);
      }
   }

   /* do the deed */
   v = SUMA_readFScurv (fname, &nrows, &ncols, rowmajor, SkipCoords);
   if (!v) {
      SUMA_SL_Err("Failed in SUMA_readFScurv");
      exit(1);
   }

   if (rowmajor) {
      for (i=0; i<nrows; ++i) {
         id = ncols * i;
         fprintf(outfile,"%d\t", (int) v[id]);
         for (j=1; j<ncols; ++j) fprintf(outfile,"%f\t", v[id+j]);
         fprintf(outfile,"\n");
      }

   } else {
      for (i=0; i<nrows; ++i) {
         fprintf(outfile,"%d\t", (int) v[i]);
         for (j=1; j<ncols; ++j) fprintf(outfile,"%f\t", v[i+j*nrows]);
         fprintf(outfile,"\n");
      }
   }

   if (outname) {
      fclose (outfile); outfile = NULL;
      SUMA_free(outname); outname = NULL;
   }
   SUMA_free(v); v = NULL;

   exit(0);
}
