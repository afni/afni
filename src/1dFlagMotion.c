/*****************************************************************************
1dFlagMotion.c - designed to flag motion greater than a user defined 
translation distance and a user defined rotation angle.

Copywrite is under gnu public license, or somesuch.

Written by Allison Nugent, 10/31/2007

May or may not be useful
******************************************************************************/
   
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "mrilib.h"

int main( int argc , char *argv[] )
{
   int narg;			/* counter for input arguments  */
   float MaxTrans=1.5;		/* maximum translation		*/
   float MaxRot=1.25;		/* maximum rotation		*/
   FILE *fp;			/* pointer for input file	*/
   float *roll ;
   float *pitch ;
   float *yaw ;
   float *dS ;
   float *dL ;
   float *dP ;
   float roll_diff ;
   float pitch_diff;
   float yaw_diff;
   float dS_diff;
   float dL_diff;
   float dP_diff;
   int ex;
   int nTR=-1, iwarn = 0;
   int imgcntr=0;
 
/*** Usage ***/

   if( argc < 2 || strncmp(argv[1],"-help",2) == 0 ){

     printf(
     "Usage: 1dFlagMotion [options] MotionParamsFile \n"
     "\n"
     "   Produces an list of time points that have more than a \n"                  
     "   user specified amount of motion relative to the previous \n"
     "   time point. \n " 
     "Options:\n"
     "  -MaxTrans    maximum translation allowed in any direction \n"
     "                 [defaults to 1.5mm]\n"
     "  -MaxRot      maximum rotation allowed in any direction \n "
     "                 [defaults to 1.25 degrees]\n"
     "\n"
     "** The input file must have EXACTLY 6 columns of input, in the order:\n"
     "     roll pitch yaw delta-SI delta-LR delta-AP\n"
     "   (angles in degrees first, then translations in mm)\n"
     "\n"
     "** The program does NOT accept column '[...]' selectors on the input\n"
     "   file name, or comments in the file itself.  As a palliative, if the\n"
     "   input file name is '-', then the input numbers are read from stdin,\n"
     "   so you could do something like the following:\n"
     "     1dcat mfile.1D'[1..6]' | 1dFlagMotion -\n"
     "   e.g., to work with the output from 3dvolreg's '-dfile' option\n"
     "   (where the first column is just the time index).\n"
     "\n"
     "** The output is in a 1D format, with comments on '#' comment lines,\n"
     "   and the list of points exceeding the motion bounds listed being\n"
     "   intercalated on normal (non-comment) lines.\n"
     ) ;

     PRINT_COMPILE_DATE ; exit(0) ;
   }

/*** read arguments ***/

   narg      = 1 ;	/*  start with the first argument */
   while( narg < argc && argv[narg][0] == '-' && argv[narg][1] != '\0' ) { 

   /*** switches ***/

      if( strncmp(argv[narg],"-MaxTrans",5) == 0 ){
         MaxTrans = atof( argv[++narg] ) ;
         if( MaxTrans < 0 ){
            fprintf( stderr, "Illegal value for MaxTrans!\n");
            exit(1) ;
         }
         narg++; continue; 
      }
      
      if( strncmp(argv[narg],"-MaxRot",5) == 0 ){
         MaxRot = atof( argv[++narg] ) ;
         if( MaxRot < 0 || MaxRot > 360 ){
            fprintf( stderr, "Illegal value for MaxRot!\n");
 	    exit(1) ;
 	 }
         narg++; continue; 
      }

    ERROR_exit("Unknown option: '%s'",argv[narg]) ;
  } 
  
  /* open input file */
  
  if( narg >= argc ) {
    fprintf(stderr, "Are you sure you don't want to give me an input motion params file?\n");
    exit(1);
  }
  
  printf("#Max Translation in mm: %f\n", MaxTrans);
  printf("#Max Rotation in Degrees: %f\n", MaxRot);
  
#if 0  /* ZSS: reading 1D file tools. 
                  Some 1D files have comments and evil formatting gmmicks */
  {  
   float *far=NULL, *fff=NULL;
   MRI_IMAGE * im=NULL;
   int k, icol, irow;
   im = mri_read_1D(argv[narg]); /* reads a 1D file and handles column selection etc. */
   if (!im) {
      fprintf( stderr, "Can't open motion params file! \n");
      exit(1);
   }
   far = MRI_FLOAT_PTR(im);   /* gets the pointer to the data array*/
   if (im->nx < 1) { /* error or empty file, 0 or neg number of rows */
      fprintf( stderr, "Number of rows in file is %d!\n", im->nx);
      exit(1);
   } 
   if (im->ny != 6) { /* Forcing 6 here, they may pass input from other motion files 
                        that this code does not deal with, more later */
      fprintf( stderr, 
            "Number of cols in file is %d, need 6 only.\n", im->ny);
      exit(1);
   }
   /* You may want to enforce 6 columns in the input or maybe have special
      flags for 3dvolreg versus warpdrive, etc. */
   /* Now we'll put the individual columns in your arrays */   
   roll = (float*)malloc(im->nx*sizeof(float));
   pitch = (float*)malloc(im->nx*sizeof(float));
   yaw = (float*)malloc(im->nx*sizeof(float));
   dS = (float*)malloc(im->nx*sizeof(float));
   dL = (float*)malloc(im->nx*sizeof(float));
   dP = (float*)malloc(im->nx*sizeof(float));
   /* far contains table or matrix in column major order, can use 
         imt = mri_transpose(im); mri_free(im); im = imt; imt = NULL;
      if you like row major ... */
   nTR = im->nx; /* need this later */
   k=0;
   for (icol=0; icol<im->ny; ++icol) {
      switch (icol){
         case 0:
            fff = roll;
            break;
         case 1:
            fff = pitch;
            break;
         case 2:
            fff = yaw;
            break;
         case 3: 
            fff = dS;
            break;
         case 4:
            fff = dL;
            break;
         case 5:
            fff = dP;
            break;
         default:
            fprintf(stderr,"Column %d  ignored\n",icol);
            break;
      } 
      for (irow=0; irow<im->nx; ++irow) {
         fff[irow] = far[k];
         ++k;             
      }
   }
   if (im) mri_free(im); im = NULL; far = NULL; 
  } 
#else 
  if( strcmp(argv[narg],"-") == 0 ){
    fp = stdin ;
  } else {
    fp = fopen(argv[narg], "r");
    if(fp==NULL) ERROR_exit("Can't open motion params file!") ;
  } 
  
  /* start reading in motion params */ 
 
  roll = (float *)malloc((imgcntr+1)*sizeof(float)); 
  pitch = (float *)malloc((imgcntr+1)*sizeof(float)); 
  yaw = (float *)malloc((imgcntr+1)*sizeof(float)); 
  dS = (float *)malloc((imgcntr+1)*sizeof(float)); 
  dL = (float *)malloc((imgcntr+1)*sizeof(float)); 
  dP = (float *)malloc((imgcntr+1)*sizeof(float)); 

  ex = fscanf(fp, "%f %f %f %f %f %f",
         &roll[imgcntr], &pitch[imgcntr], &yaw[imgcntr], &dS[imgcntr], &dL[imgcntr], &dP[imgcntr]);

  imgcntr=1;

  while (ex > 0 ) { 		/*  open loop to read in motion parameter file */ 
   
    roll = (float *)realloc( (void *)roll, (imgcntr+1)*sizeof(float)); 
    pitch = (float *)realloc( (void *)pitch, (imgcntr+1)*sizeof(float)); 
    yaw = (float *)realloc( (void *)yaw, (imgcntr+1)*sizeof(float)); 
    dS = (float *)realloc( (void *)dS, (imgcntr+1)*sizeof(float)); 
    dL = (float *)realloc( (void *)dL, (imgcntr+1)*sizeof(float)); 
    dP = (float *)realloc( (void *)dP, (imgcntr+1)*sizeof(float)); 

    ex = fscanf(fp, "%f %f %f %f %f %f",
           &roll[imgcntr], &pitch[imgcntr], &yaw[imgcntr], &dS[imgcntr], &dL[imgcntr], &dP[imgcntr]);

    ++imgcntr;

  }
   nTR = imgcntr-1;
   if( fp != stdin ) fclose(fp);

#endif
   /* now do the checking */
   imgcntr = 1;
   iwarn = 0;
   while (imgcntr < nTR) {
       roll_diff = fabs(roll[imgcntr] - roll[imgcntr-1]);
       pitch_diff = fabs(pitch[imgcntr] - pitch[imgcntr-1]);
       yaw_diff = fabs(yaw[imgcntr] - yaw[imgcntr-1]);
       dS_diff = fabs(dS[imgcntr] - dS[imgcntr-1]);
       dL_diff = fabs(dL[imgcntr] - dL[imgcntr-1]);
       dP_diff = fabs(dP[imgcntr] - dP[imgcntr-1]);

       if(roll_diff>=MaxRot || pitch_diff>=MaxRot || yaw_diff>=MaxRot || dS_diff>=MaxTrans || dL_diff>=MaxTrans || dP_diff>=MaxTrans) { 
          if (!iwarn){
              printf("#You may wish to consider censoring the following points: \n\n");
          }
          ++iwarn;
          printf("#roll_diff: %f\t pitch_diff: %f\t yaw_diff: %f \n", roll_diff, pitch_diff, yaw_diff); 
          printf("#dS_diff: %f \t dL_diff: %f\t dP_diff: %f at image:\n%d\n", dS_diff, dL_diff, dP_diff, imgcntr); 

       }
       ++imgcntr;
   }

}
