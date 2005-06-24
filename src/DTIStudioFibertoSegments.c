/******************************* DTIStudioFibertoSegments.c **************************************/

#if 0
# format from Hangyi Jiang for DTIStudio fibers
File Header (128 bytes):
char     sFiberFileTag[8] = "FiberDat";
int        nFiberNr;	// number of fibers in this file 
int        nFiberLenMax;	// max-length of fibers 
float     fFiberLenMean;	// mean-length of fibers
int        nImgWidth;		// image dimension 
int        nImgHeight; 
int        nImgSlices;
float	fPixelSizeWidth;	// voxel size 
float	fPixelSizeHeight; 
float	fSliceThickness;

int	enumSliceOrientation;  // orientation:  0=Coronal, 1=Axial, 
2=Sagittal 
int	enumSliceSequencing;  // sequencing:  0=Normal,  1= 
		
Fiber Data (starts from offset 128 bytes) 
for each fiber: 
  int      nLength;    // fiber length; 
  int      nReserved; 
  int      nFiberStartIndex;   // the start-point of the selected fiber 
  int      nFiberEndIndex;     // the end-point of the selected fiber
  XYZ_TRIPLE  xyzFiberChain[];	// the fiber data, in x-y-z format.
then, next fiber.. 
#endif

#include "mrilib.h"

static char outfname[THD_MAX_PREFIX] = "rawxyzseg.dat";
static char infname[THD_MAX_PREFIX];

/*! convert DTIStudio fiber format data to SUMA segment data */
int main( int argc , char * argv[] )
{
   int nopt;
   int i,j;
   FILE *fout, *fin;
   float fxyz[3];
   char fiberheaderstring[32];
   int totalpts=0;
   int npts, nfibers;
   int statcode;

   /*----- Read command line -----*/
   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: DTIStudioFibertoSegments [options] dataset\n"
             "Convert a DTIStudio Fiber file to a SUMA segment file\n"
             "Options:\n"
             "  -output / -prefix = name of the output file (not an AFNI dataset prefix)\n"
             "    the default output name will be rawxyzseg.dat\n\n"
	     );
      exit(0) ;
   }
   fout = NULL;
   mainENTRY("DTIStudioFibertoSegments main"); machdep(); AFNI_logger("DTIStudioFibertoSegments",argc,argv);
   nopt = 1;
   while( nopt < argc && argv[nopt][0] == '-' ){
    if( (strcmp(argv[nopt],"-output") == 0 ) || (strcmp(argv[nopt],"-prefix")==0))
      {
       if (++nopt >= argc)
	 {
	  fprintf (stderr, "*** Error - output / prefix needs an argument!\n");
	  exit (1);
	 }
       MCW_strncpy (outfname, argv[nopt], THD_MAX_PREFIX);	/* change name from default prefix */
       if (!THD_filename_ok (outfname))
	 {
	  fprintf (stderr, "*** Error - %s is not a valid output name!\n", outfname);
	  exit (1);
	 }
       if (THD_is_file(outfname))
        {
	  fprintf (stderr, "*** Error - %s already exists!\n", outfname);
	  exit (1);
	 }
       nopt++; continue;
      }

     fprintf(stderr, "*** Error - unknown option %s\n", argv[nopt]);
     exit(1);
   }
 
   fout = fopen (outfname, "w") ;
   if( fout == NULL ){
     fprintf (stderr, "*** Error - can not create %s for some reason!\n", outfname);
     exit (1);
   }


   /* get fiber file as input and check if we can open it */
   MCW_strncpy (infname, argv[nopt], THD_MAX_PREFIX);	/* get name of fiber file */
   if (!THD_filename_ok (infname))
      {
       fprintf (stderr, "*** Error - %s is not a valid input name!\n", infname);
       exit (1);
      }
   if (!THD_is_file(infname))
      {
       fprintf (stderr, "*** Error - %s does not exist!\n", infname);
       exit (1);
      }
   fin = fopen (infname, "r") ;
   if( fin == NULL ){
      fprintf (stderr, "*** Error - can not open %s for some reason!\n", infname);
      exit (1);
   }
 


   /* read the header */

   statcode = fread(fiberheaderstring, 8, 1, fin);
   if(!strcmp(fiberheaderstring,"FiberDat")) {
     fprintf(stderr, "*** Error - file does not have correct header format\n");
     fclose(fin);
     fclose(fout);
     exit(1);
   }

   statcode = fread(&nfibers, sizeof(int), 1, fin);
   if((statcode<1)||(nfibers<1)) {
     fprintf(stderr, "*** Error - file does not have correct header format\n");
     fclose(fin);
     fclose(fout);
     exit(1);
   }

   fseek(fin, 128, SEEK_SET);    /* go to start of data - 128 byte header */

   fprintf(stderr, "Number of fibers = %d \n", nfibers);
   for(i=0;i<nfibers;i++) {                    /* get all the fibers */
      statcode = fread(&npts,sizeof(int),1, fin);
      if((statcode<1)||(npts<1)) {
         fprintf(stderr, "*** can not read fiber info.\n");
         fclose(fin);
         fclose(fout);
         exit(1);
      }

      totalpts += npts;
      statcode = fseek(fin, 12,  SEEK_CUR); /* skip unused byte,RGB bytes,Start and End point indices */

      for(j=0;j<npts;j++) {                     /* get each point in each fiber */
	statcode = fread(fxyz, sizeof(float),3, fin);      /* xyz floating point triplet */
        if(statcode<3) {
           fprintf(stderr, "*** can not read fiber data.\n");
           fclose(fin);
           fclose(fout);
           exit(1);
        }

        if(j!=0) {     /* after first point put space and write x,y,z, then repeat x,y,z on next line */
	  statcode = fprintf(fout," %10.3f %10.3f %10.3f\n",fxyz[0],fxyz[1],fxyz[2]);
        }
        if(j<(npts-1)) {  /* for all but the last point, write point again (first time for 1st pt*/
	  statcode = fprintf(fout,"%10.3f %10.3f %10.3f",fxyz[0],fxyz[1],fxyz[2]);
        }
        if(statcode==0) {
	   fprintf(stderr, "*** Error - writing output file!\n");
           fclose(fout);
           exit(1);
        }
     }
   }

   fprintf(stderr, "Total number of points = %d \n", totalpts);
   fclose(fout);
   fclose(fin);
   exit(0);
}
