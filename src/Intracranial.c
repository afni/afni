/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This file contains routines used by plug_intracranial and 3dIntracranial.

  File:    Intracranial.c
  Author:  B. Douglas Ward
  Date:    04 June 1999

  Mod:     Correction to initialization in center of mass calculation.
  Date:    11 February 2000

  Mod:     Added option to suppress spatial smoothing of segmentation mask.
  Date:    03 December 2001
*/


/*---------------------------------------------------------------------------*/
/*
  Verify that inputs are acceptable.
*/

int verify_inputs()
{
  char message[MAX_STRING_LENGTH];    /* error message */


  /*----- Check for required datasets -----*/
  if (anat == NULL)
    { 
      sprintf (message, "No anatomical image");
      SI_error (message); 
      return (0);
    }
  if (! ISANAT(anat))
    { 
      sprintf (message, "Input dataset must be anatomical image");
      SI_error (message); 
      return (0);
    }
  if (DSET_BRICK_TYPE(anat,0) != MRI_short)
    {
      sprintf (message, "Anatomical image must have data type: short integer");
      SI_error (message);
      return (0);
    }

#ifdef PLUG_INTRACRANIAL
  if (dset == NULL)
    { 
      sprintf (message, "No output dataset");
      SI_error (message);
      return (0);
    }
  if (DSET_BRICK_TYPE(dset,0) != MRI_short)
    {
      sprintf (message, "Output dataset must have data type: short integer");
      SI_error (message);
      return (0);
    }
#else
  if (prefix_filename == NULL)
    {
      sprintf (message, "Must specify output prefix filename");
      SI_error (message);
      return (0);
    }

  /*----- Check whether output file already exists -----*/
  check_one_output_file (prefix_filename);
#endif


  /*----- Check compatibility of datasets -----*/
#ifdef PLUG_INTRACRANIAL
  if (! EQUIV_DATAXES (anat->daxes, dset->daxes))
    {
      sprintf (message, 
	       "Anatomical image and output dataset are incompatible");
      SI_error (message);
      return (0);
    }
#endif
    

  /*----- Check allowed range of intensity values -----*/
  if (min_val_float >= max_val_float)
    {
      sprintf (message, "min_val >= max_val ???");
      SI_error (message);
      return (0);
    }

  return (1);

}


/*---------------------------------------------------------------------------*/
/*
   Flood filling a byte array:
     nx = 1st dimension
     ny = 2nd dimension
     ix = start point
     jy = end point
     ar = array, with 0's everwhere except 1's as barriers to flooding

   All filled points (starting with ix,jy) will get the value 2.

   This routine was adapted from:  plug_drawdset.
*/

void draw_2dfiller( int nx , int ny , int ix , int jy , byte * ar )
{
   int ii,jj , ip,jp , num ;

#define AR(i,j) ar[(i)+(j)*nx]


   /*----- Test for early termination -----*/
   if (AR(ix,jy) != 0)  return;


   /* fill out in cross from 1st point */

   ip = ix ; jp = jy ; AR(ip,jp) = 2 ;

   for( ii=ip+1; ii < nx && AR(ii,jp) == 0; ii++ ) AR(ii,jp) = 2;
   for( ii=ip-1; ii >= 0 && AR(ii,jp) == 0; ii-- ) AR(ii,jp) = 2;
   for( jj=jp+1; jj < ny && AR(ip,jj) == 0; jj++ ) AR(ip,jj) = 2;
   for( jj=jp-1; jj >= 0 && AR(ip,jj) == 0; jj-- ) AR(ip,jj) = 2;

   /* brute force repetition of the cross technique */

   do {
      num = 0 ;
      for( jp=0 ; jp < ny ; jp++ ){
         for( ip=0 ; ip < nx ; ip++ ){
            if( AR(ip,jp) == 2 ){
               for( ii=ip+1; ii < nx && AR(ii,jp) == 0; ii++ ){ AR(ii,jp) = 2; num++; }
               for( ii=ip-1; ii >= 0 && AR(ii,jp) == 0; ii-- ){ AR(ii,jp) = 2; num++; }
               for( jj=jp+1; jj < ny && AR(ip,jj) == 0; jj++ ){ AR(ip,jj) = 2; num++; }
               for( jj=jp-1; jj >= 0 && AR(ip,jj) == 0; jj-- ){ AR(ip,jj) = 2; num++; }
            }
         }
      }
   } while( num > 0 ) ;

   return ;
}


/*---------------------------------------------------------------------------*/
/*
  Find the center of mass of the brain image.
*/

void center_of_mass (int * cx, int * cy, int * cz)

{
  int nx, ny, nz, nxy, nxyz;               /* dataset dimensions in voxels */
  int ix, jy, kz, ixyz;                    /* voxel indices */
  short * anat_data  = NULL;               /* data from anatomical image */
  float f, sum, sumx, sumy, sumz;          /* sums for calculating means */


  /*----- Progress report -----*/
  if (! quiet)  printf ("\nCalculating center of mass \n");


  /*----- Initialize local variables -----*/
  anat_data  = (short *) DSET_BRICK_ARRAY(anat,0) ;
  nx = DSET_NX(anat);   ny = DSET_NY(anat);   nz = DSET_NZ(anat);
  nxy = nx * ny;   nxyz = nxy * nz;


  /*----- Sum over all voxels -----*/
  sum = 0.0;   sumx = 0.0;   sumy = 0.0;   sumz = 0.0;
  for (kz = 0;  kz < nz;  kz++)
    for (jy = 0;  jy < ny;  jy++)
      for (ix = 0;  ix < nx;  ix++)
	{
	  ixyz = ix + jy*nx + kz*nxy;
	  f = anat_data[ixyz];
	  sum += f;   sumx += ix*f;   sumy += jy*f;   sumz += kz*f;
	}


  /*----- Calculate center of mass -----*/
  *cx = sumx / sum;
  *cy = sumy / sum;
  *cz = sumz / sum;

  if (! quiet)  printf ("Center of mass:   ix = %d   jy = %d   kz = %d  \n", 
			*cx, *cy, *cz);
 
}


/*---------------------------------------------------------------------------*/
/*
  Segment the slices perpendicular to the x-axis.
*/

void segment_x_slices
(
  short * cv,                  /* volume with 1's at non-brain locations */
  int cx,                      /* center slice location */
  Boolean axial_slice          /* true if x-slices are axial slices */
)

{
  const int MAXNPTS = 1000;    /* max. number of random initial test points */
  const int MAXFOUND = 10;     /* max. number of initial search points */

  int nx, ny, nz, nxy, nxz, nyz, nxyz;     /* dataset dimensions in voxels */
  int ix, jy, kz, ixy, ixz, iyz, ixyz;     /* voxel indices */

  int m;                       /* slice index */
  short * anat_data  = NULL;   /* data from anatomical image */
  float z;                     /* voxel gray-scale intensity */
  int nmpts, found;            /* random search counters */
  byte * slice = NULL;         /* current slice brain mask */
  byte * pslice = NULL;        /* previous slice non-brain mask */

  int count;                   /* count of brain voxels in current slice */
  int maxcount = 0;            /* maximum number of brain voxels in a slice */
  float threshold = 0.05;      /* stop if count falls below 5% of maximum */


  /*----- Progress report -----*/
  if (! quiet)  printf ("\nSegmenting slices perpendicular to x-axis \n");


  /*----- Initialize local variables -----*/
  anat_data  = (short *) DSET_BRICK_ARRAY(anat,0) ;
  nx = DSET_NX(anat);   ny = DSET_NY(anat);   nz = DSET_NZ(anat);
  nxy = nx*ny;   nxz = nx*nz;   nyz = ny*nz;   nxyz = nxy*nz;


  /*----- Allocate memory -----*/
  slice = (byte *)  malloc (nyz * sizeof(byte));
  pslice = (byte *) malloc (nyz * sizeof(byte));


  /*----- Loop over x-slices -----*/
  m = 0; 
  while (m <= nx)
    {
      /*----- Start from center slice -----*/
      if (m <= cx)  ix = cx - m;
      else          ix = m-1;
      

      /*----- Initialize mask  -----*/
      if (ix == cx)  
	{
	  if (axial_slice)
	    for (iyz = 0;  iyz < nyz;  iyz++)   pslice[iyz] = 0;
	  else
	    {
	      for (kz = 0;  kz < nz;  kz++)
		for (jy = 0;  jy < ny;  jy++)
		  {
		    iyz = jy + kz*ny;
		    ixyz = ix + jy*nx + kz*nxy;
		    if (cv[ixyz])   pslice[iyz] = 2;
		    else            pslice[iyz] = 0;
		  }
	    }
	}


      /*----- Examine each voxel for possible inclusion in brain -----*/
      for (kz = 0;  kz < nz;  kz++)
	for (jy = 0;  jy < ny;  jy++)
	  {
	    iyz = jy + kz*ny;
	    ixyz = ix + jy*nx + kz*nxy;
	    z = anat_data[ixyz];

	    if (pslice[iyz] == 2)        slice[iyz] = 1;
	    else if (z < min_val_float)  slice[iyz] = 1;
	    else if (z > max_val_float)  slice[iyz] = 1;
	    else                         slice[iyz] = 0;
	  }


      /*----- Fill brain voxels from center out -----*/
      draw_2dfiller (ny, nz, ny/2, nz/2, slice);


      /*----- Use additional random starting points for robustness -----*/
      nmpts = 0;   found = 0;
      while ((nmpts < MAXNPTS) && (found < MAXFOUND))
	{
	  nmpts++;
	  if ((ix == cx) && axial_slice)
	    {
	      jy = (3*ny/8) + (int) (rand_uniform(0.0,1.0) * ny/4);
	      kz = (3*nz/8) + (int) (rand_uniform(0.0,1.0) * nz/4);
	    }
	  else
	    {
	      jy = (int) (rand_uniform(0.0,1.0) * ny);
	      kz = (int) (rand_uniform(0.0,1.0) * nz);
	    }
	  if (slice[jy+kz*ny] != 1)  found++;
	  draw_2dfiller (ny, nz, jy, kz, slice);
	}


      /*----- Filled brain voxels become barriers -----*/
      for (iyz = 0;  iyz < nyz;  iyz++)
	if (slice[iyz] == 2)  pslice[iyz] = 1;
	else	              pslice[iyz] = 0;


      /*----- Now fill non-brain voxels from outside in -----*/
      draw_2dfiller (ny, nz, 0,    0,    pslice);
      draw_2dfiller (ny, nz, ny-1, 0,    pslice);
      draw_2dfiller (ny, nz, 0,    nz-1, pslice);
      draw_2dfiller (ny, nz, ny-1, nz-1, pslice);


      /*----- Save results for this slice -----*/
      count = 0;
      for (kz = 0;  kz < nz;  kz++)
	for (jy = 0;  jy < ny;  jy++)
	  {
	    iyz = jy + kz*ny;
	    ixyz = ix + jy*nx + kz*nxy;
	    if (pslice[iyz] != 2)
	      {
		cv[ixyz] = 0;
		count++;
	      }
	  }


      /*----- Slightly increase size of brain mask for next slice -----*/
      for (kz = 1;  kz < nz-1;  kz++)
	for (jy = 1;  jy < ny-1;  jy++)
	  {
	    iyz = jy + kz*ny;
	    ixyz = ix + jy*nx + kz*nxy;
	    if (!cv[ixyz])  
	      {
		pslice[iyz]    = 0;
		pslice[iyz+1]  = 0;
		pslice[iyz-1]  = 0;
		pslice[iyz-ny] = 0;
		pslice[iyz+ny] = 0;
	      }
	  }


      /*----- Examine count of brain voxels for this slice -----*/
      if (count > maxcount)  maxcount = count;
      if (count < threshold * maxcount)
	{
	  if (m <= cx)  m = cx;
	  else          m = nx;
	}


      /*----- Increment slice index -----*/
      m++;

    }  /* Loop over x-slices */


  /*----- Release memory -----*/
  free (pslice);   pslice = NULL;
  free (slice);    slice = NULL;


  return;

}


/*---------------------------------------------------------------------------*/
/*
  Segment the slices perpendicular to the y-axis.
*/

void segment_y_slices
(
  short * cv,                  /* volume with 1's at non-brain locations */
  int cy,                      /* center slice location */
  Boolean axial_slice          /* true if y-slices are axial slices */
)

{
  const int MAXNPTS = 1000;    /* max. number of random initial test points */
  const int MAXFOUND = 10;     /* max. number of initial search points */

  int nx, ny, nz, nxy, nxz, nyz, nxyz;     /* dataset dimensions in voxels */
  int ix, jy, kz, ixy, ixz, iyz, ixyz;     /* voxel indices */

  int m;                       /* slice index */
  short * anat_data  = NULL;   /* data from anatomical image */
  float z;                     /* voxel gray-scale intensity */
  int nmpts, found;            /* random search counters */
  byte * slice = NULL;         /* current slice brain mask */
  byte * pslice = NULL;        /* previous slice non-brain mask */

  int count;                   /* count of brain voxels in current slice */
  int maxcount = 0;            /* maximum number of brain voxels in a slice */
  float threshold = 0.05;      /* stop if count falls below 5% of maximum */


  /*----- Progress report -----*/
  if (! quiet)  printf ("\nSegmenting slices perpendicular to y-axis \n");


  /*----- Initialize local variables -----*/
  anat_data  = (short *) DSET_BRICK_ARRAY(anat,0) ;
  nx = DSET_NX(anat);   ny = DSET_NY(anat);   nz = DSET_NZ(anat);
  nxy = nx*ny;   nxz = nx*nz;   nyz = ny*nz;   nxyz = nxy*nz;


  /*----- Allocate memory -----*/
  slice = (byte *)  malloc (nxz * sizeof(byte));
  pslice = (byte *) malloc (nxz * sizeof(byte));


  /*----- Loop over y-slices -----*/
  m = 0;
  while (m <= ny)
    {
      /*----- Start from center slice -----*/
      if (m <= cy)  jy = cy - m;
      else          jy = m-1;
      

      /*----- Initialize mask  -----*/
      if (jy == cy) 
	{ 
	  if (axial_slice)
	    for (ixz = 0;  ixz < nxz;  ixz++)   pslice[ixz] = 0;
	  else 
	    { 
	      for (kz = 0;  kz < nz;  kz++)
		for (ix = 0;  ix < nx;  ix++)
		  {
		    ixz = ix + kz*nx;
		    ixyz = ix + jy*nx + kz*nxy;
		    if (cv[ixyz])   pslice[ixz] = 2;
		    else            pslice[ixz] = 0;
		  }
	    }
	}


      /*----- Examine each voxel for possible inclusion in brain -----*/
      for (kz = 0;  kz < nz;  kz++)
	for (ix = 0;  ix < nx;  ix++)
	  {
	    ixz = ix + kz*nx;
	    ixyz = ix + jy*nx + kz*nxy;
	    z = anat_data[ixyz];

	    if (pslice[ixz] == 2)        slice[ixz] = 1;
	    else if (z < min_val_float)  slice[ixz] = 1;
	    else if (z > max_val_float)  slice[ixz] = 1;
	    else                         slice[ixz] = 0;
	  }


      /*----- Fill brain voxels from center out -----*/
      draw_2dfiller (nx, nz, nx/2, nz/2, slice);


      /*----- Use additional random starting points for robustness -----*/
      nmpts = 0;   found = 0;
      while ((nmpts < MAXNPTS) && (found < MAXFOUND))
	{
	  nmpts++;
	  if ((jy == cy) && axial_slice)
	    {
	      ix = (3*nx/8) + (int) (rand_uniform(0.0,1.0) * nx/4);
	      kz = (3*nz/8) + (int) (rand_uniform(0.0,1.0) * nz/4);
	    }
	  else
	    {
	      ix = (int) (rand_uniform(0.0,1.0) * nx);
	      kz = (int) (rand_uniform(0.0,1.0) * nz);
	    }
	  if (slice[ix+kz*nx] != 1)  found++;
	  draw_2dfiller (nx, nz, ix, kz, slice);
	}


      /*----- Filled brain voxels become barriers -----*/
      for (ixz = 0;  ixz < nxz;  ixz++)
	if (slice[ixz] == 2)  pslice[ixz] = 1;
	else	              pslice[ixz] = 0;


      /*----- Now fill non-brain voxels from outside in -----*/
      draw_2dfiller (nx, nz, 0,    0,    pslice);
      draw_2dfiller (nx, nz, nx-1, 0,    pslice);
      draw_2dfiller (nx, nz, 0,    nz-1, pslice);
      draw_2dfiller (nx, nz, nx-1, nz-1, pslice);


      /*----- Save results for this slice -----*/
      count = 0;
      for (kz = 0;  kz < nz;  kz++)
	for (ix = 0;  ix < nx;  ix++)
	  {
	    ixz = ix + kz*nx;
	    ixyz = ix + jy*nx + kz*nxy;
	    if (pslice[ixz] != 2)  
	      {
		cv[ixyz] = 0;
		count++;
	      }
	  }


      /*----- Slightly increase size of brain mask for next slice -----*/
      for (kz = 1;  kz < nz-1;  kz++)
	for (ix = 1;  ix < nx-1;  ix++)
	  {
	    ixz = ix + kz*nx;
	    ixyz = ix + jy*nx + kz*nxy;
	    if (!cv[ixyz])  
	      {
		pslice[ixz]    = 0;
		pslice[ixz+1]  = 0;
		pslice[ixz-1]  = 0;
		pslice[ixz-nx] = 0;
		pslice[ixz+nx] = 0;
	      }
	  }


      /*----- Examine count of brain voxels for this slice -----*/
      if (count > maxcount)  maxcount = count;
      if (count < threshold * maxcount)
	{
	  if (m <= cy)  m = cy;
	  else          m = ny;
	}
      

      /*----- Increment slice index -----*/
      m++;

    }  /* Loop over y-slices */


  /*----- Release memory -----*/
  free (pslice);   pslice = NULL;
  free (slice);    slice = NULL;


  return;

}


/*---------------------------------------------------------------------------*/
/*
  Segment the slices perpendicular to the z-axis.
*/

void segment_z_slices
(
  short * cv,                  /* volume with 1's at non-brain locations */
  int cz,                      /* center slice location */
  Boolean axial_slice          /* true if z-slices are axial slices */
)

{
  const int MAXNPTS = 1000;    /* max. number of random initial test points */
  const int MAXFOUND = 10;     /* max. number of initial search points */

  int nx, ny, nz, nxy, nxz, nyz, nxyz;     /* dataset dimensions in voxels */
  int ix, jy, kz, ixy, ixz, iyz, ixyz;     /* voxel indices */

  int m;                       /* slice index */
  short * anat_data  = NULL;   /* data from anatomical image */
  float z;                     /* voxel gray-scale intensity */
  int nmpts, found;            /* random search counters */
  byte * slice = NULL;         /* current slice brain mask */
  byte * pslice = NULL;        /* previous slice non-brain mask */

  int count;                   /* count of brain voxels in current slice */
  int maxcount = 0;            /* maximum number of brain voxels in a slice */
  float threshold = 0.05;      /* stop if count falls below 5% of maximum */


  /*----- Progress report -----*/
  if (! quiet)  printf ("\nSegmenting slices perpendicular to z-axis \n");


  /*----- Initialize local variables -----*/
  anat_data  = (short *) DSET_BRICK_ARRAY(anat,0) ;
  nx = DSET_NX(anat);   ny = DSET_NY(anat);   nz = DSET_NZ(anat);
  nxy = nx*ny;   nxz = nx*nz;   nyz = ny*nz;   nxyz = nxy*nz;


  /*----- Allocate memory -----*/
  slice = (byte *)  malloc (nxy * sizeof(byte));
  pslice = (byte *) malloc (nxy * sizeof(byte));


  /*----- Loop over z-slices -----*/
  m = 0;
  while (m <= nz)
    {
      /*----- Start from center slice -----*/
      if (m <= cz)  kz = cz - m;
      else          kz = m-1;

      
      /*----- Initialize mask -----*/
      if (kz == cz)  
	{
	  if (axial_slice)
	    for (ixy = 0;  ixy < nxy;  ixy++)   pslice[ixy] = 0;
	  else
	    {
	      for (ix = 0;  ix < nx;  ix++)
		for (jy = 0;  jy < ny;  jy++)
		  {
		    ixy = ix + jy*nx;
		    ixyz = ix + jy*nx + kz*nxy;
		    if (cv[ixyz])   pslice[ixy] = 2;
		    else            pslice[ixy] = 0;
		  }
	    }
	}


      /*----- Examine each voxel for possible inclusion in brain -----*/
      for (jy = 0;  jy < ny;  jy++)
	for (ix = 0;  ix < nx;  ix++)
	  {
	    ixy = ix + jy*nx;
	    ixyz = ix + jy*nx + kz*nxy;
	    z = anat_data[ixyz];

	    if (pslice[ixy] == 2)        slice[ixy] = 1;
	    else if (z < min_val_float)  slice[ixy] = 1;
	    else if (z > max_val_float)  slice[ixy] = 1;
	    else                         slice[ixy] = 0;
	  }


      /*----- Fill brain voxels from center out -----*/
      draw_2dfiller (nx, ny, nx/2, ny/2, slice);


      /*----- Use additional random starting points for robustness -----*/
      nmpts = 0;   found = 0;
      while ((nmpts < MAXNPTS) && (found < MAXFOUND))
	{
	  nmpts++;
	  if ((kz == cz) && axial_slice)
	    {
	      ix = (3*nx/8) + (int) (rand_uniform(0.0,1.0) * nx/4);
	      jy = (3*ny/8) + (int) (rand_uniform(0.0,1.0) * ny/4);
	    }
	  else
	    {
	      ix = (int) (rand_uniform(0.0,1.0) * nx);
	      jy = (int) (rand_uniform(0.0,1.0) * ny);
	    }
	  if (slice[ix+jy*nx] != 1)  found++;
	  draw_2dfiller (nx, ny, ix, jy, slice);
	}


      /*----- Filled brain voxels become barriers -----*/
      for (ixy = 0;  ixy < nxy;  ixy++)
	if (slice[ixy] == 2)  pslice[ixy] = 1;
	else	              pslice[ixy] = 0;


      /*----- Now fill non-brain voxels from outside in -----*/
      draw_2dfiller (nx, ny, 0,    0,    pslice);
      draw_2dfiller (nx, ny, nx-1, 0,    pslice);
      draw_2dfiller (nx, ny, 0,    ny-1, pslice);
      draw_2dfiller (nx, ny, nx-1, ny-1, pslice);


      /*----- Save results for this slice -----*/
      count = 0;
      for (jy = 0;  jy < ny;  jy++)
	for (ix = 0;  ix < nx;  ix++)
	  {
	    ixy = ix + jy*nx;
	    ixyz = ix + jy*nx + kz*nxy;
	    if (pslice[ixy] != 2)   
	      {
		cv[ixyz] = 0;
		count++;
	      }
	  }


      /*----- Slightly increase size of brain mask for next slice -----*/
      for (jy = 1;  jy < ny-1;  jy++)
	for (ix = 1;  ix < nx-1;  ix++)
	  {
	    ixy = ix + jy*nx;
	    ixyz = ix + jy*nx + kz*nxy;
	    if (!cv[ixyz])  
	      {
		pslice[ixy]    = 0;
		pslice[ixy+1]  = 0;
		pslice[ixy-1]  = 0;
		pslice[ixy-nx] = 0;
		pslice[ixy+nx] = 0;
	      }
	  }


      /*----- Examine count of brain voxels for this slice -----*/
      if (count > maxcount)  maxcount = count;
      if (count < threshold * maxcount)
	{
	  if (m <= cz)  m = cz;
	  else          m = nz;
	}


      /*----- Increment slice index -----*/
      m++;

    }  /* Loop over z-slices */


  /*----- Release memory -----*/
  free (pslice);   pslice = NULL;
  free (slice);    slice = NULL;


  return;

}


/*---------------------------------------------------------------------------*/
/*
  Create envelope for segmented image.
*/

void segment_envelope
(
  short * cv,                  /* volume with 1's at non-brain locations */
  int cx, int cy, int cz       /* location of center of mass */
)

{
#define MAXLAT 90
#define MAXLNG 180

  int nx, ny, nz, nxy, nxz, nyz, nxyz;     /* dataset dimensions in voxels */
  int ix, jy, kz, ixy, ixz, iyz, ixyz;     /* voxel indices */

  /* 24 Mar 2004: on Mac OS X, the compiler generates incorrect code
                  for these large arrays when optimization is turned on;
                  instead, use malloc() to get them if AIZE is not defined */

#undef AIZE
#ifndef DARWIN
# define AIZE
#endif

#ifdef  AIZE
  float radius[MAXLAT][MAXLNG];      /* max. radius vector to brain voxel */
  float smradius[MAXLAT][MAXLNG];    /* array of smoothed radius vectors */
#else
  float **radius , **smradius ;
#endif

  int ilat, jlat;              /* radius array latitude index */
  int ilng, jlng;              /* radius array longitude index */
  float deltalat, deltalng;    /* increments in latitude and longitude */
  float lat, clat, slat;       /* voxel latitude */
  float lng, clng, slng;       /* voxel longitude */
  int ir, i, j;                /* indices */
  float rxy;                   /* radius to voxel in xy-plane */
  float r;                     /* radius to voxel from center of brain */
  int nr;                      /* maximum test radius */
  float maxr;                  /* maximum radius to voxel inside the brain */


  /*----- Progress report -----*/
  if (! quiet)  printf ("\nCreating envelope for segmented image \n");

#ifndef AIZE
  radius   = (float **) malloc(sizeof(float *)*MAXLAT) ;
  smradius = (float **) malloc(sizeof(float *)*MAXLAT) ;
  for (ilat = 0;  ilat < MAXLAT;  ilat++){
      radius[ilat] = (float *)malloc(sizeof(float)*MAXLNG) ;
    smradius[ilat] = (float *)malloc(sizeof(float)*MAXLNG) ;
  }
#endif


  /*----- Initialize local variables -----*/
  nx = DSET_NX(anat);   ny = DSET_NY(anat);   nz = DSET_NZ(anat);
  nxy = nx*ny;   nxz = nx*nz;   nyz = ny*nz;   nxyz = nxy*nz;
  deltalat = PI / MAXLAT;
  deltalng = 2.0 * PI / MAXLNG;
  nr = nx;  
  if (ny > nr)  nr = ny;
  if (nz > nr)  nr = nz;

    
  
  /*----- Calculate the radius vector -----*/
  for (ilat = 0;  ilat < MAXLAT;  ilat++)
    {
      lat = ilat * deltalat - PI/2;
      slat = sin(lat);
      clat = cos(lat);
      
      for (ilng = 0;  ilng < MAXLNG;  ilng++)
	{
	  lng = ilng * deltalng;
	  clng = cos(lng);
	  slng = sin(lng);
	  
	  radius[ilat][ilng] = 0.0;
	  for (ir = 0;  ir < nr;  ir++)
	    {
	      ix = cx + ir*clat*clng;
	      jy = cy + ir*clat*slng;
	      kz = cz + ir * slat;

	      if ((ix >= 0) && (ix < nx) && (jy >= 0) && (jy < ny)
		  && (kz >= 0) && (kz < nz))
		{
		  ixyz = ix + jy*nx + kz*nxy;
		  if (! cv[ixyz])  radius[ilat][ilng] = (float) ir; 
		}
	    }
	}
    }
  
  /*----- Smooth the radius vectors -----*/
  for (ilat = 0;  ilat < MAXLAT;  ilat++)
    {
      for (ilng = 0;  ilng < MAXLNG;  ilng++)
	{
	  smradius[ilat][ilng] = 0.0;
	  
	  for (i = -1;  i <= 1;  i++)
	    {
	      jlat = (ilat + i + MAXLAT) % MAXLAT;
	      
	      if (ilat == 0)  jlat = 0;
	      if (ilat == MAXLAT-1)  jlat = MAXLAT-1;
	      
	      for (j = -2;  j <= 2;  j++)
		{
		  jlng = (ilng + j + MAXLNG) % MAXLNG;
		  smradius[ilat][ilng] += radius[jlat][jlng] / 15.0;
		}
	    }
	}
    }

  /*----- Find maximum radius to a brain voxel -----*/
  maxr = 0.0;
  for (ilat = 0;  ilat < MAXLAT;  ilat++)
    for (ilng = 0;  ilng < MAXLNG;  ilng++)    
      if (maxr < smradius[ilat][ilng])  maxr = smradius[ilat][ilng];
	  
  /*----- Smooth the brain mask -----*/
  for (ix = 0;  ix < nx;  ix++){
    for (jy = 0;  jy < ny;  jy++)
      {
	rxy = hypot ((float) (ix-cx), (float) (jy-cy));
	lng = atan2 (jy-cy,  ix-cx);
	ilng = (int) ((lng/deltalng) + MAXLNG) % MAXLNG;

	for (kz = 0;  kz < nz;  kz++)
	  {
	    ixyz = ix + jy*nx + kz*nxy;

	    r = hypot (rxy, (float) (kz-cz));	  

	    if (r < maxr)
	      {
		lat = atan2 (kz-cz, rxy);
		ilat = (int) (((lat+PI/2)/deltalat) + MAXLAT) % MAXLAT;
		
		if (r < smradius[ilat][ilng])  cv[ixyz] = 0;
	      }
	    
	  }
      }
   }

#ifndef AIZE
  for (ilat = 0;  ilat < MAXLAT;  ilat++){
    free(  radius[ilat]) ; free(smradius[ilat]) ;
  }
  free(smradius); free(radius);
#endif
}


/*---------------------------------------------------------------------------*/
/*
  Segment the intracranial voxels
*/

void segment_volume 
(
  short * cv                    /* volume with 1's at non-brain locations */
)

{
  THD_dataxes * daxes;                  /* dataset axes */
  char xxorient, yyorient, zzorient;    /* dataset axes orientations */ 
  int nxyz;                             /* dataset dimension in voxels */
  int ixyz;                             /* voxel indices */
  int cx, cy, cz;                       /* location of center of mass */


  /*----- Initialize local variables -----*/
  daxes = anat->daxes;
  xxorient = ORIENT_typestr[daxes->xxorient][0];
  yyorient = ORIENT_typestr[daxes->yyorient][0];
  zzorient = ORIENT_typestr[daxes->zzorient][0];
  nxyz = DSET_NX(anat) * DSET_NY(anat) * DSET_NZ(anat);


  /*----- Calculate center of mass of brain image -----*/
  center_of_mass (&cx, &cy, &cz);


  /*----- Initialize mask for non-brain voxels -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    cv[ixyz] = 1;


  /*----- Segment the axial slices -----*/
       if ((xxorient == 'S') || (xxorient == 'I'))  segment_x_slices (cv,cx,1);
  else if ((yyorient == 'S') || (yyorient == 'I'))  segment_y_slices (cv,cy,1);
  else if ((zzorient == 'S') || (zzorient == 'I'))  segment_z_slices (cv,cz,1);
  else SI_error ("Unable to determine dataset orientation");
 

  /*----- Segment the sagittal slices -----*/
       if ((xxorient == 'L') || (xxorient == 'R'))  segment_x_slices (cv,cx,0);
  else if ((yyorient == 'L') || (yyorient == 'R'))  segment_y_slices (cv,cy,0);
  else if ((zzorient == 'L') || (zzorient == 'R'))  segment_z_slices (cv,cz,0);
  else SI_error ("Unable to determine dataset orientation");
  
 
  /*----- Segment the coronal slices -----*/       
       if ((xxorient == 'A') || (xxorient == 'P'))  segment_x_slices (cv,cx,0);
  else if ((yyorient == 'A') || (yyorient == 'P'))  segment_y_slices (cv,cy,0);
  else if ((zzorient == 'A') || (zzorient == 'P'))  segment_z_slices (cv,cz,0);
  else SI_error ("Unable to determine dataset orientation");


  /*----- Create envelope for segmented image -----*/
  if (! nosmooth)
    segment_envelope (cv, cx, cy, cz);
 

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Perform voxel connectivity tests.
*/

int connectivity_tests (short * cv)

{
  int nx, ny, nz, nxy, nxyz;        /* voxel counters */
  int ix, jy, kz, ixyz;             /* voxel indices */
  byte * dv = NULL;                 /* volume for intermediate data */


  /*----- Progress report -----*/
  if (! quiet)  printf ("\nPerforming voxel connectivity tests \n");


  /*----- Initialize local variables -----*/
  nx = DSET_NX(anat);   ny = DSET_NY(anat);   nz = DSET_NZ(anat);
  nxy = nx*ny;   nxyz = nxy*nz;


  /*----- Initialize voxel classification indicators -----*/
  dv = (byte *) malloc (nxyz * sizeof(byte));
  MTEST (dv);   if (dv == NULL)  return (0);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)  
    dv[ixyz] = 0;


  /*----- Determine which voxels will leave the target structure -----*/
  if (max_conn_int > -1)
    {
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  if (! cv[ixyz])
	    {
	      IJK_TO_THREE (ixyz, ix, jy, kz, nx, nxy);
	      if (ix > 1)     dv[THREE_TO_IJK(ix-1,jy,kz,nx,nxy)]++;
	      if (ix < nx-1)  dv[THREE_TO_IJK(ix+1,jy,kz,nx,nxy)]++;
	      if (jy > 1)     dv[THREE_TO_IJK(ix,jy-1,kz,nx,nxy)]++;
	      if (jy < ny-1)  dv[THREE_TO_IJK(ix,jy+1,kz,nx,nxy)]++;
	      if (kz > 1)     dv[THREE_TO_IJK(ix,jy,kz-1,nx,nxy)]++;
	      if (kz < nz-1)  dv[THREE_TO_IJK(ix,jy,kz+1,nx,nxy)]++;
	    }
	}

      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  if (dv[ixyz] <= max_conn_int)  cv[ixyz] = 1;
	  dv[ixyz] = 0;
	}
    }


  /*----- Determine which voxels will enter the target structure -----*/
  if (min_conn_int < 7)
    {
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  if (! cv[ixyz])
	    {
	      IJK_TO_THREE (ixyz, ix, jy, kz, nx, nxy);
	      if (ix > 1)     dv[THREE_TO_IJK(ix-1,jy,kz,nx,nxy)]++;
	      if (ix < nx-1)  dv[THREE_TO_IJK(ix+1,jy,kz,nx,nxy)]++;
	      if (jy > 1)     dv[THREE_TO_IJK(ix,jy-1,kz,nx,nxy)]++;
	      if (jy < ny-1)  dv[THREE_TO_IJK(ix,jy+1,kz,nx,nxy)]++;
	      if (kz > 1)     dv[THREE_TO_IJK(ix,jy,kz-1,nx,nxy)]++;	      
	      if (kz < nz-1)  dv[THREE_TO_IJK(ix,jy,kz+1,nx,nxy)]++;
	    }
	}
      
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	if (dv[ixyz] >=  min_conn_int)  cv[ixyz] = 0;
    }


  /*----- Deallocate memory -----*/
  free (dv);   dv = NULL;

  
  return (1);
}


/*---------------------------------------------------------------------------*/
