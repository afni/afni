/********************************************************************************
3dEdge                                                                                  
Justin Knoll 5/23/00                                                                    

Generates an edge-detected mask from the input 3d+time dataset.                          
Needs boundary checking for non-sagittal datasets.                                      

********************************************************************************/

#include "mrilib.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/*******************************************************************************/
void errorExit(char *message) { 
	
	fprintf(stderr, "\n\nError in 3dEdge:\n%s\n\nTry 3dEdge -help\n",message);
	exit(1);
}

/*******************************************************************************/
void helpMessage()
/*******************************************************************************/
{
  printf(
	 "Generates an edge-detected mask from the input dataset.\n"
       	 "Usage: 3dEdge [options] dataset\n"
         "Where the options are:\n"
	 "  -prefix name     The prefix for the filename of the edge-masked\n"
	 "                   dataset.\n"
	 "  -mask name       The filename for the separately output mask.\n"
	 "  -rms float       The floating point value (0 <= rms <= 1) for RMS\n"
	 "                   threshhold. Voxels in the time-averaged volume with\n"
	 "                   >= this fraction of the peak RMS for that volume\n"
	 "                   are provisionally part of the mask, though other\n"
	 "                   tests may subsequently remove them.\n"
	 "  -neighbor float  The floating point value for the neighbor\n"
	 "                   threshhold. Voxels in the RMS-derived mask are\n"
	 "                   removed if less than this fraction of their nearest\n"
	 "                   neighbors are included in the RMS-derived mask,\n"
	 "                   else they are added.\n"
  );
}

/*******************************************************************************/
int edgeCount(int l, int nxx, int nyy, int nvox)
/*
  computes the number of edges the voxel has - e.g. a voxel in the corner of a sub-brick has three
  edges. this is necessary to apply the neighborCount to boundary voxels. 
*/
{
  int count=0;
  if (l < (nxx*nyy)) count++;
  if (l < (nxx*nyy + 1)) count++;

  if (l > (nvox - (nxx*nyy))) count++;
  if (l > (nvox - (nxx*nyy) - 1)) count++;


  if (l % (nxx*nyy) < nxx) count++;
  if (l % (nxx*nyy) < (nxx + 1)) count++;

  if (l % (nxx*nyy) > (nxx * (nyy - 1))) count++;
  if (l % (nxx*nyy) > (nxx * (nyy - 1) - 1)) count++;


  if (((l % (nxx*nyy)) %  nxx) == 0) count++;
  if (((l % (nxx*nyy)) %  nxx) == 1) count++;

  if (((l % (nxx*nyy)) %  nxx) == (nxx - 1)) count++;
  if (((l % (nxx*nyy)) %  nxx) == (nxx - 2)) count++;
 
  return (count*25) - (count*count)-1;
}

/*******************************************************************************/
float neighborCount(unsigned char *mask, int location, int nxx, int nyy, int nvox)
{
  int c=0; /* number of neighbors */
  int i, j, k; /*axes */
  int tmpLocation;
  float percent;  
  for (i=-2 ; i <= 2 ; i++)
    for (j=-2 ; j <= 2 ; j++)
      for (k=-2 ; k <= 2 ; k++)
/* 
   the if condition below is the bitwise OR of the three variables. It is true if at least one variable is not zero.
   if all three are zero, we are at the voxel itself, not a neighbor.
*/
	if (i | j | k)
	  {
	    tmpLocation = location;
	    tmpLocation += i;
	    tmpLocation += (j * nxx);
	    if (location < (nxx*nyy)) 
	      {
		if (k < 0) continue;
		  tmpLocation += (k * nxx * nyy);
	      }
	    else if (location < ((nxx*nyy) + 1))
	      {
		if (k < -1) continue;
		  tmpLocation += (k * nxx * nyy);
	      }
	    else if (location > (nvox - (nxx*nyy)))
	      {
		if (k > 0) continue;
		  tmpLocation += (k * nxx * nyy);
	      }
	    else if (location > (nvox - (nxx*nyy) - 1))
	      {
		if (k > 1) continue;
		  tmpLocation += (k * nxx * nyy);
	      }
	    if (mask[tmpLocation]) 
	      c++;
	  }
  percent = ( (float) c / (124 - edgeCount(location, nxx, nyy, nvox)));
  return percent;
}

/*******************************************************************************/
void recurseSelect(unsigned char *mask, int nxx, int nyy, int nvox, int i)
{
  float x;
  float y;
  float z;

  if (mask[i] == 1)
    {
      mask[i] = 2;
	{
	  recurseSelect(mask, nxx, nyy, nvox, (i + 1));
	  recurseSelect(mask, nxx, nyy, nvox, (i - 1));
      
	  recurseSelect(mask, nxx, nyy, nvox, (i + nxx));
	  recurseSelect(mask, nxx, nyy, nvox, (i - nxx));
      
	  recurseSelect(mask, nxx, nyy, nvox, (i + (nxx * nyy)));
	  recurseSelect(mask, nxx, nyy, nvox, (i - (nxx * nyy)));
	}
    }
  else
    return;
}

/*******************************************************************************/
int centerOfMass(unsigned char *mask, int nxx, int nyy, int nvox)
{
  int i;
  float xMass=0;
  float yMass=0;
  float zMass=0;
  int mass=0;

  for(i=0 ; i < nvox ; i++)
    {
      if (mask[i])
	{
	  xMass += (i % (nxx * nyy) % nxx);
	  yMass += (i % (nxx * nyy) / nxx);
	  zMass += (i / (nxx * nyy));
	  mass++;
	}
    }
  xMass /= mass;
  yMass /= mass;
  zMass /= mass;
  return (int) ((int) (xMass+0.49) + ((int) (yMass+0.49) * nxx) + ((int) (zMass+0.49) * (nxx * nyy))); 
}

/*******************************************************************************/
unsigned char* edgeDetect(THD_3dim_dataset *inputDataset, float rmsThresh, float neighbors)
{
  int nvox, ntimes;
  int nxx, nyy;
  int i,j;
  unsigned char *inputTimeStep_b;  
  short *inputTimeStep_s;  
  float *inputTimeStep_f;
  unsigned char *mask;
  double *sums;
  double rms=0;
  unsigned char *newMask; /* temporary mask used to prevent order-dependent errors in neighbor counting. */
  THD_3dim_dataset *outputDataset=NULL;

  DSET_load(inputDataset);
  
  ntimes = DSET_NUM_TIMES(inputDataset);
  nvox = DSET_NVOX(inputDataset);

  nxx=inputDataset->daxes->nxx;
  nyy=inputDataset->daxes->nyy;
 
  sums = (double *) malloc(nvox * sizeof(double));
  mask = (unsigned char *) malloc(nvox * sizeof(unsigned char));
  newMask = (unsigned char *) malloc(nvox * sizeof(unsigned char));

/*
  the for loop below switches to handle different subbrick types (float, byte, short), and stores the
  square of the values at each voxel in sums[]. 
*/
  for (j=0 ; j < ntimes ; j++)
    {
      switch (DSET_BRICK_TYPE(inputDataset, j))
	{
	case MRI_byte:
	  {
	    inputTimeStep_b = (unsigned char *) DSET_ARRAY(inputDataset, j);
	    for (i=0 ; i < nvox ; i++)
	      {  
		sums[i] += inputTimeStep_b[i]; 
	      }
	    break;
	  }
	case MRI_short:
	  {
	    inputTimeStep_s = (short *) DSET_ARRAY(inputDataset, j);
	    for (i=0 ; i < nvox ; i++)
	      {  
		sums[i] += inputTimeStep_s[i]; 
	      }
	    break;
	  }
	case MRI_float:
	  {
	    inputTimeStep_f = (float *) DSET_ARRAY(inputDataset, j);
            for (i=0 ; i < nvox ; i++)
	      {  
		sums[i] += inputTimeStep_f[i];
	      }
	    break;
	  }
	default:
	  errorExit("Dataset sub-brick is of unrecognized type.");
	}

     /* calculate average of voxels over time */   
    } 
  for (i=0 ; i < nvox ; i++)
    {
      sums[i] /= ntimes;
      rms += sums[i]*sums[i];
    }
  rms /= nvox;
  rms = sqrt(rms);
  

  for (i=0 ; i < nvox ; i++)
    {
      mask[i] = (sums[i] >= (rms * rmsThresh));
    }
/* fill holes and remove outlayers based on number of nearest neighbors */
   for (i = 0 ; i < nvox ; i++)  
     if (neighborCount(mask, i, nxx, nyy, nvox) < neighbors) newMask[i]=0;
     else
       newMask[i]=mask[i];
   for (i = 0 ; i < nvox ; i++) 
     mask[i] = newMask[i];
   for (i = 0 ; i < nvox ; i++)  
      if (neighborCount(mask, i, nxx, nyy, nvox) > neighbors) newMask[i]=1;
   for (i = 0 ; i < nvox ; i++) 
     mask[i] = newMask[i];

   i = centerOfMass(mask, nxx, nyy, nvox);
/*   printf("i=%d\n", i);*/
   recurseSelect(mask, nxx, nyy, nvox, i);

   for (i = 0 ; i < nvox ; i++) 
     mask[i] = mask[i]==2;
   
   free(sums);
   free(newMask);
   return mask;
}

/*******************************************************************************/
int applyMask(THD_3dim_dataset *inputDataset, unsigned char *mask, char *prefix, int argc, char *argv[])
/* argc and argv are needed for to put the command-line args in the history. */
{
  int ierr;
  int i,j;
  int ntimes, nvox;
  double* timeStep;
  THD_3dim_dataset *outputDataset;

  unsigned char *inputTimeStep_b;  
  short *inputTimeStep_s;  
  float *inputTimeStep_f;

  unsigned char *outputTimeStep_b;  
  short *outputTimeStep_s;  
  float *outputTimeStep_f;

  ntimes = DSET_NUM_TIMES(inputDataset);
  nvox = DSET_NVOX(inputDataset);
  outputDataset = EDIT_empty_copy(inputDataset);

  ierr = EDIT_dset_items(outputDataset,
			   ADN_type, HEAD_ANAT_TYPE,     /* dataset type */
			   ADN_func_type, FUNC_FIM_TYPE, /* dataset type */
			   ADN_prefix, prefix,
                         ADN_none);

  for (j=0 ; j < ntimes ; j++)
    {
      switch (DSET_BRICK_TYPE(inputDataset, j))
	{
	case MRI_byte:
	  {
	    inputTimeStep_b = (unsigned char *) DSET_ARRAY(inputDataset, j);
	    outputTimeStep_b = (unsigned char *) malloc(sizeof(unsigned char) * nvox);
	    for (i=0 ; i < nvox ; i++)
	      {  
		if (mask[i])
		  outputTimeStep_b[i] = inputTimeStep_b[i];
		else
		  outputTimeStep_b[i] = 0;
	      }
            EDIT_substitute_brick(outputDataset, j, DSET_BRICK_TYPE(inputDataset, j), outputTimeStep_b);
	    break;
	  }
	case MRI_short:
	  {
	    inputTimeStep_s = (short *) DSET_ARRAY(inputDataset, j);
	    outputTimeStep_s = (short *) malloc(sizeof(short) * nvox);
	    for (i=0 ; i < nvox ; i++)
	      {  
		if (mask[i])
		  outputTimeStep_s[i] = inputTimeStep_s[i];
		else
		  outputTimeStep_s[i] = 0;
	      }
	    EDIT_substitute_brick(outputDataset, j, DSET_BRICK_TYPE(inputDataset, j), outputTimeStep_s);
	    break;
	  }
	case MRI_float:
	  {
	    inputTimeStep_f = (float *) DSET_ARRAY(inputDataset, j);
	    outputTimeStep_f = (float *) malloc(sizeof(inputTimeStep_f));
            for (i=0 ; i < nvox ; i++)
	      {  
		if (mask[i])
		  outputTimeStep_f[i] = inputTimeStep_f[i];
		else
		  outputTimeStep_f[i] = 0;
	      }
            EDIT_substitute_brick(outputDataset, j, DSET_BRICK_TYPE(inputDataset, j), outputTimeStep_f);
      	    break;
	  }
	default:
	  errorExit("Dataset sub-brick is of unrecognized type.");
	}
    } 
    tross_Copy_History( inputDataset, outputDataset );
    tross_Make_History( "3dedge" , argc, argv, outputDataset) ;
    DSET_write(outputDataset);  
/*    DSET_unload(inputDataset);
    DSET_unload(outputDataset); */
}

void saveMask(THD_3dim_dataset *inputDataset, unsigned char *mask, char *prefix)
{
  int ierr;
  THD_3dim_dataset *outputDataset;
  
  outputDataset = EDIT_empty_copy(inputDataset);
  ierr = EDIT_dset_items(outputDataset,
			   ADN_prefix, prefix,      /* prefix */
			   ADN_datum_all, MRI_byte,      /* datum type */
			   ADN_nvals, 1,                 /* number of 3d sub-bricks */
			   ADN_type, HEAD_FUNC_TYPE,     /* dataset type */
			   ADN_func_type, FUNC_FIM_TYPE, /* dataset type */
			   ADN_ntt, 1,                   /* number of time steps */
			 ADN_none);
  EDIT_substitute_brick(outputDataset, 0, MRI_byte, mask);
  DSET_write(outputDataset);
}


THD_3dim_dataset* openDataset(char *name)
{
  THD_3dim_dataset *newDataset = THD_open_dataset(name);

/* check for cases which indicate errors or are not supported. */
  if (newDataset == NULL)
    errorExit("Cannot open new dataset!");
  if (DSET_BRICK_TYPE(newDataset, 0) == MRI_complex)
    errorExit("I can't deal with complex datasets.");
  if (DSET_BRICK_FACTOR(newDataset, 0))
    errorExit("I can't deal with datasets containing scaling factors");
/* else, make it so   */
  return newDataset; 
}

/*******************************************************************************/
int main(int argc, char *argv[])
{
  int narg=1;
  int stringLength;
  float rms=1.0;
  float neighbors=0.5;
  char *prefix = "masked";
  char *maskName = "mask";
  unsigned char *mask;
  THD_3dim_dataset *inputDataset=NULL;
  
  if (argc < 2 || (strcmp(argv[1], "-help") == 0))
    {
      helpMessage();
      exit(1);
    }

  while((narg < argc) && (argv[narg][0] == '-'))
    {
      if (strcmp(argv[narg], "-prefix") == 0)
	{
	  narg++;
	  stringLength = strlen(argv[narg]);
	  prefix = (char *) malloc((stringLength + 1) * sizeof(char));
	  strcpy(prefix, argv[narg++]);
/*	  printf("prefix: %s\n", prefix); */
	  continue;
	}
      if (strcmp(argv[narg], "-mask") == 0)
	{
	  narg++;
	  stringLength = strlen(argv[narg]);
	  maskName = (char *) malloc((stringLength + 1) * sizeof(char));
	  strcpy(maskName, argv[narg++]);
/*	  printf("mask: %s\n", maskName); */
	  continue;
	}
      if (strcmp(argv[narg], "-rms") == 0)
	{
	  narg++;
	  rms = (float)atof(argv[narg++]);
/*	  printf("rms: %f\n", rms);*/
	  continue;
	}
      if (strcmp(argv[narg], "-neighbor") == 0)
	{
	  narg++;
	  neighbors = (float)atof(argv[narg++]);
/*	  printf("neighbors: %f\n", neighbors);*/
	  continue;
	}
      else
	{
	  printf("*** unrecognized option, %s\n", argv[narg]);
	  narg++;
	}
    }
  inputDataset = openDataset(argv[narg]);
  mask = edgeDetect(inputDataset, rms, neighbors);
  if (strcmp(maskName, "mask") != 0)
	saveMask(inputDataset, mask, maskName);
  applyMask(inputDataset, mask, prefix, argc, argv);
  return 0;
}






















