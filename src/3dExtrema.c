/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This program finds local extrema (minima or maxima) of the input
  dataset values for each sub-brick of the input dataset.  The extrema
  may be determined either for each volume, or for each individual slice.  
  Only those voxels whose corresponding intensity value is greater than 
  the user specified data threshold will be considered.

  File:    3dExtrema.c
  Author:  B. Douglas Ward
  Date:    12 April 2001

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

*/
/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dExtrema"                     /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_INITIAL "12 April 2001" /* date of initial program release */
#define PROGRAM_LATEST "15 August 2001"     /* date of last program revision */


/*---------------------------------------------------------------------------*/
/*
  Include header files and source code files.
*/

#include "mrilib.h"


/*---------------------------------------------------------------------------*/
/*
  Structure declarations 
*/

struct extrema;

typedef struct extrema
{
  float intensity;                     /* (averaged) extrema intensity */
  float * centroid;                    /* (averaged) extrema location */
  int count;                           /* count of extrema in average */
  float sum;                           /* sum of extrema intensities */
  float nearest_dist;                  /* distance to nearest extrema */
  struct extrema * nearest_extrema;    /* pointer to nearest extrema */
  struct extrema * next_extrema;       /* pointer to next extrema in list */
} extrema;


/*-------------------------- global data ------------------------------------*/

#define  EX_MAX_LL 5000      /* maximum number of linked lists of extrema */
#define  EX_MAX_NV 26        /* maximum number of neighboring voxels */
#define  EX_DIMENSION 3      /* 3 dimensional space */

#define  EX_GT 1             /* indices for binary relations */
#define  EX_GE 2
#define  EX_LT 3
#define  EX_LE 4

static THD_coorder EX_cord;  /* get orientation from AFNI_ORIENT environment */

static int EX_nx, EX_ny, EX_nz,           /* dataset dimensions in voxels */
           EX_nxy, EX_nxyz;               

static int        EX_quiet      = 0;      /* flag for suppress screen output */
static int        EX_relation   = 1;      /* flag for binary relation */
static int        EX_maxima     = 1;      /* flag for maxima or minima */
static int        EX_strict     = 1;      /* flag for strict or parial ineq. */
static int        EX_interior   = 1;      /* flag for interior or closure */
static int        EX_slice      = 1;      /* flag for slice or volume */
static int        EX_sort       = 1;      /* flag for sort extrema */
static int        EX_merge      = 1;      /* flag for remove or average */
static float      EX_sep_dist   = 0.0;    /* minimum separation distance */

static float      EX_mask_thr   = 1.0;    /* mask threshold */
static float      EX_data_thr   = 0.0;    /* data threshold */
static int        EX_nbricks    = 0;      /* number of output subbricks */

static extrema *  EX_head_extrema[EX_MAX_LL];    /* linked lists of extrema */
static int        EX_num_ll = 0;                 /* number of linked lists */

static int EX_offset[EX_MAX_NV];   /* relative indices of neighboring voxels */

static char * EX_mask_filename    = NULL;
static char * EX_output_prefix    = NULL;
static char * EX_session          = "./";

static byte * EX_mask = NULL;             /* mask for voxels above thr. */

static char * commandline = NULL;          /* command line for history notes */

static THD_3dim_dataset * EX_dset = NULL;  /* first input dataset */  

/*---------------------------------------------------------------------------*/

/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
if((ptr)==NULL) \
( printf ("Cannot allocate memory \n"),  exit(1) )
     

/*---------------------------------------------------------------------------*/

/** macro to open a dataset and make it ready for processing **/

#define DOPEN(ds,name)                                                        \
do{ int pv ; (ds) = THD_open_dataset((name)) ;                                \
       if( !ISVALID_3DIM_DATASET((ds)) ){                                     \
          fprintf(stderr,"*** Can't open dataset: %s\n",(name)) ; exit(1) ; } \
       THD_load_datablock( (ds)->dblk , NULL ) ;                              \
       pv = DSET_PRINCIPAL_VALUE((ds)) ;                                      \
       if( DSET_ARRAY((ds),pv) == NULL ){                                     \
          fprintf(stderr,"*** Can't access data: %s\n",(name)) ; exit(1); }   \
       if( DSET_BRICK_TYPE((ds),pv) == MRI_complex ){                         \
          fprintf(stderr,"*** Can't use complex data: %s\n",(name)) ; exit(1);        }     \
       break ; } while (0)


/*---------------------------------------------------------------------------*/

/** macro to return pointer to correct location in brick for current processing **/

#define SUB_POINTER(ds,vv,ind,ptr)                                            \
   do{ switch( DSET_BRICK_TYPE((ds),(vv)) ){                                  \
         default: fprintf(stderr,"\n*** Illegal datum! ***\n");exit(1);       \
            case MRI_short:{ short * fim = (short *) DSET_ARRAY((ds),(vv)) ;  \
                            (ptr) = (void *)( fim + (ind) ) ;                 \
            } break ;                                                         \
            case MRI_byte:{ byte * fim = (byte *) DSET_ARRAY((ds),(vv)) ;     \
                            (ptr) = (void *)( fim + (ind) ) ;                 \
            } break ;                                                         \
            case MRI_float:{ float * fim = (float *) DSET_ARRAY((ds),(vv)) ;  \
                             (ptr) = (void *)( fim + (ind) ) ;                \
            } break ; } break ; } while(0)


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void EX_error (char * message)
{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/
/*
  Create an empty extrema.
*/
  
extrema * initialize_extrema ()
{
  extrema * extrema_ptr = NULL;

  extrema_ptr = (extrema *) malloc (sizeof(extrema));
  MTEST (extrema_ptr);
  
  extrema_ptr->intensity = 0.0;
  extrema_ptr->centroid = NULL;
  extrema_ptr->count = 0;
  extrema_ptr->sum = 0.0;
  extrema_ptr->nearest_dist = 0.0;
  extrema_ptr->nearest_extrema = NULL;
  extrema_ptr->next_extrema = NULL;

  return (extrema_ptr);
  
}


/*---------------------------------------------------------------------------*/
/*
  Print the contents of one extrema.
*/

void print_extrema (int index, extrema * extrema_ptr)
{
  int j;
  char str[30];

  sprintf (str, "%5d", index);
  printf ("%5s", str);

  printf ("%15.3f", extrema_ptr->intensity);

  for (j = 0;  j < EX_DIMENSION;  j++)
    printf ("%10.2f", extrema_ptr->centroid[j]);


  printf("%10d", extrema_ptr->count);

  if (extrema_ptr->nearest_dist < 1.0e+20)
    printf ("%10.3f", extrema_ptr->nearest_dist);

  printf ("\n");
}


/*---------------------------------------------------------------------------*/
/*
  Print the contents of all extrema in the linked list.
*/

void print_all_extrema (int ivolume, int islice, extrema * extrema_ptr)
{
  int index;

  printf ("\n");
  if (EX_maxima)
    printf ("Maxima ");
  else
    printf ("Minima ");

  if (EX_slice)
    printf ("for Volume #%d and Slice #%d: \n", ivolume, islice);
  else
    printf ("for Volume #%d: \n", ivolume);


  if (extrema_ptr != NULL)
    {
      printf ("%5s",     "Index");
      printf ("%15s",    "Intensity");
      printf ("%6s[mm]", ORIENT_tinystr[EX_cord.xxor]);
      printf ("%6s[mm]", ORIENT_tinystr[EX_cord.yyor]);
      printf ("%6s[mm]", ORIENT_tinystr[EX_cord.zzor]);
      printf ("%10s",    "Count");
      printf ("%6s[mm]", "Dist");
      printf ("\n");
      
      printf ( "%5s", "-----");
      printf ("%15s", "---------");
      printf ("%10s", "------");
      printf ("%10s", "------");
      printf ("%10s", "------");
      printf ("%10s", "-----");
      printf ("%10s", "--------");
      printf ("\n");
    }

  index = 0;
  while (extrema_ptr != NULL)
    {
      index++;
      print_extrema (index, extrema_ptr);
      extrema_ptr = extrema_ptr->next_extrema;
    }
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the distance between two extrema as the Euclidean distance
  between their centroids.
*/

float extrema_distance (extrema * aextrema, extrema * bextrema)
{
  float sumsqr;
  float delta;
  int i;

  sumsqr = 0.0;
  for (i = 0;  i < EX_DIMENSION;  i++)
    {
      delta = aextrema->centroid[i] - bextrema->centroid[i];
      sumsqr += delta * delta;
    }

  return (sqrt(sumsqr));
  
}


/*---------------------------------------------------------------------------*/
/*
  Find the extrema which is nearest to new_extrema.
  Set the nearest_dist and nearest_extrema structure elements accordingly.
*/
  
void find_nearest_extrema (extrema * new_extrema, extrema * head_extrema)
{
  const float MAX_DIST = 1.0e+30;

  extrema * extrema_ptr = NULL;
  float dist;


  /*----- Initialize nearest extrema elements -----*/
  new_extrema->nearest_dist = MAX_DIST;
  new_extrema->nearest_extrema = NULL;


  extrema_ptr = head_extrema;

  while (extrema_ptr != NULL)
    {
      if (extrema_ptr != new_extrema)
	{
	  dist = extrema_distance (new_extrema, extrema_ptr);

	  if (dist < new_extrema->nearest_dist)
	    {
	      new_extrema->nearest_dist = dist;
	      new_extrema->nearest_extrema = extrema_ptr;
	    }

	  if (dist < extrema_ptr->nearest_dist)
	    {
	      extrema_ptr->nearest_dist = dist;
	      extrema_ptr->nearest_extrema = new_extrema;
	    }
	}

      extrema_ptr = extrema_ptr->next_extrema;
    }
}


/*---------------------------------------------------------------------------*/
/*
  Add a new extrema to the linked list of extrema.
*/

extrema * add_extrema (extrema * new_extrema, extrema * head_extrema)
{

  new_extrema->next_extrema = head_extrema;

  find_nearest_extrema (new_extrema, head_extrema);

  return (new_extrema);
}


/*---------------------------------------------------------------------------*/
/*
  Create a new extrema containing a single voxel, and add to list of extrema.
*/

extrema * new_extrema (float * centroid, float intensity,
		       extrema * head_extrema)
{
  extrema * extrema_ptr = NULL;

  extrema_ptr = initialize_extrema ();

  extrema_ptr->intensity = intensity;
  extrema_ptr->centroid  = centroid;
  extrema_ptr->count     = 1;
  extrema_ptr->sum       = intensity;

  head_extrema = add_extrema (extrema_ptr, head_extrema);

  return (head_extrema);
  
}


/*---------------------------------------------------------------------------*/
/*
  Deallocate memory for this extrema.
*/

void delete_extrema (extrema * extrema_ptr)
{
  if (extrema_ptr != NULL)
    {
      if (extrema_ptr->centroid != NULL)
	{
	  free (extrema_ptr->centroid);
	  extrema_ptr->centroid = NULL;
	}

      free (extrema_ptr);
    }
}


/*---------------------------------------------------------------------------*/
/*
  Remove one extrema from linked list of extrema.
  Reset extrema pointers, and recalculate nearest extrema distances 
  where needed.  Finally, delete the extrema from memory.
*/

extrema * remove_extrema (extrema * aextrema, extrema * head_extrema)
{
  extrema * extrema_ptr = NULL;
  extrema * next_extrema = NULL;
  

  if (head_extrema == NULL)  return;


  /*----- Remove aextrema from list; reset pointers -----*/
  if (head_extrema == aextrema)
    head_extrema = head_extrema->next_extrema;
  else
    {
      extrema_ptr = head_extrema;
      next_extrema = extrema_ptr->next_extrema;
      while (next_extrema != NULL)
	{
	  if (next_extrema == aextrema)
	    extrema_ptr->next_extrema = next_extrema->next_extrema;
	  else
	    extrema_ptr = next_extrema;
	  
	  next_extrema = extrema_ptr->next_extrema;
	}
    }


  /*----- Recalculate distances to nearest extrema -----*/
  if (head_extrema != NULL)
    {
      extrema_ptr = head_extrema;
      while (extrema_ptr != NULL)
	{
	  if (extrema_ptr->nearest_extrema == aextrema) 
	    {
	      find_nearest_extrema (extrema_ptr, head_extrema);
	    }
	  extrema_ptr = extrema_ptr->next_extrema;
	}
    }


  /*----- Delete aextrema -----*/
  delete_extrema (aextrema);

  return (head_extrema);
}


/*---------------------------------------------------------------------------*/
/*
  Average two extrema into one new extrema.
*/

extrema * average_extrema (extrema * aextrema, extrema * bextrema)
{
  extrema * abextrema = NULL;
  int na, nb;
  int i;


  /*----- Create new extrema -----*/
  abextrema = initialize_extrema ();


  /*----- Sum counts of extrema -----*/
  na = aextrema->count;
  nb = bextrema->count;
  abextrema->count = na + nb;


  /*----- Average of extrema intensities -----*/
  abextrema->intensity = 
    (na*aextrema->intensity + nb*bextrema->intensity) / (na+nb);


  abextrema->centroid = (float *) malloc (sizeof(float) * EX_DIMENSION);
  MTEST (abextrema->centroid);


  /*----- Average of extrema locations -----*/
  for (i = 0;  i < EX_DIMENSION;  i++)
    abextrema->centroid[i] = 
      (na*aextrema->centroid[i] + nb*bextrema->centroid[i]) / (na + nb);
  

  return (abextrema);
}


/*---------------------------------------------------------------------------*/
/*
  Weighted average of two extrema into one new extrema.
*/

extrema * weight_extrema (extrema * aextrema, extrema * bextrema)
{
  extrema * abextrema = NULL;
  float suma, sumb;
  int i;


  /*----- Create new extrema -----*/
  abextrema = initialize_extrema ();


  /*----- Sum counts of extrema -----*/
  abextrema->count = aextrema->count + bextrema->count;


  /*----- Sum intensities of extrema -----*/
  suma = aextrema->sum;
  sumb = bextrema->sum;
  abextrema->sum = suma + sumb;


  /*----- Weighted average of extrema intensities -----*/
  abextrema->intensity = 
    (suma*aextrema->intensity + sumb*bextrema->intensity) / (suma + sumb);


  abextrema->centroid = (float *) malloc (sizeof(float) * EX_DIMENSION);
  MTEST (abextrema->centroid);


  /*----- Weighted average of extrema locations -----*/
  for (i = 0;  i < EX_DIMENSION;  i++)
    abextrema->centroid[i] = 
      (suma*aextrema->centroid[i] + sumb*bextrema->centroid[i]) / (suma+sumb);
  

  return (abextrema);
}


/*---------------------------------------------------------------------------*/
/*
  Merge two extrema.
*/
  
extrema * merge_extrema (extrema * aextrema, extrema * bextrema, 
			 extrema * head_extrema)
{
  extrema * abextrema = NULL;


  switch (EX_merge)
    {
    case 1:   /*----- Merge extrema by removing the weakest -----*/

      if ((EX_maxima && ((aextrema->intensity) > (bextrema->intensity)))
	  || (!EX_maxima && ((aextrema->intensity) < (bextrema->intensity))))
	{
	  aextrema->count++;
	  head_extrema = remove_extrema (bextrema, head_extrema);
	}
      else
	{
	  bextrema->count++;	  
	  head_extrema = remove_extrema (aextrema, head_extrema);
	}
      break;


    case 2:   /*----- Merge extrema using average -----*/

      /*----- Merge two extrema into one new extrema -----*/
      abextrema = average_extrema (aextrema, bextrema);

      /*----- Remove the two original extrema from the linked list -----*/
      head_extrema = remove_extrema (aextrema, head_extrema);
      head_extrema = remove_extrema (bextrema, head_extrema);


      /*----- Add the merged extrema to the linked list -----*/
      head_extrema = add_extrema (abextrema, head_extrema);
  
      break;


    case 3:   /*----- Merge extrema using weighted average -----*/
 
      /*----- Merge two extrema into one new extrema -----*/
      abextrema = weight_extrema (aextrema, bextrema);

      /*----- Remove the two original extrema from the linked list -----*/
      head_extrema = remove_extrema (aextrema, head_extrema);
      head_extrema = remove_extrema (bextrema, head_extrema);


      /*----- Add the merged extrema to the linked list -----*/
      head_extrema = add_extrema (abextrema, head_extrema);

      break;

    }

  return (head_extrema);

}


/*---------------------------------------------------------------------------*/
/*
  Sort extrema in order of intensity.
*/

extrema * sort_extrema (extrema * head_extrema)
{
  extrema * i  = NULL; 
  extrema * ip = NULL; 
  extrema * m  = NULL;
  extrema * mp = NULL;
  extrema * j  = NULL;
  extrema * jp = NULL;
  extrema * guard = NULL;


  /*----- Create guard extrema in case head extrema must be replaced -----*/
  guard = initialize_extrema();
  guard->next_extrema = head_extrema;
  ip = guard;

  while (ip->next_extrema != NULL)
    {
      /*----- Initialize search for next largest extrema -----*/
      i = ip->next_extrema;  /* current top of list */
      mp = ip;               /* extrema pointing to next largest extrema */
      m = i;                 /* next largest extrema */
      jp = i;

      /*----- Search through list for next largest extrema -----*/
      while (jp->next_extrema != NULL)
	{
	  j = jp->next_extrema;
	  if ( ((EX_maxima) && ((j->intensity) > (m->intensity)))
	    || ((!EX_maxima) && ((j->intensity) < (m->intensity))) )
	    {
	      mp = jp;
	      m = j;
	    }
	  jp = j;
	}

      /*----- Now move next largest extrema to top of list -----*/
      if (m != i)
	{
	  ip->next_extrema = m;
	  mp->next_extrema = m->next_extrema;
	  m->next_extrema = i;
	  i = m;
	}

      /*----- Move down the list -----*/
      ip = i;
	
    }

  
  /*----- Replace head extrema -----*/
  head_extrema = guard->next_extrema;
  delete_extrema (guard);

  return (head_extrema);
}


/*---------------------------------------------------------------------------*/
/*
  Agglomerate extrema by merging the two extrema which are closest together.
*/

extrema * agglomerate_extrema (extrema * head_extrema, float sep_dist)
{
  const float MAX_DIST = 1.0e+30;

  extrema * extrema_ptr = NULL;
  extrema * aextrema    = NULL;
  extrema * bextrema    = NULL;
  float min_dist;


  min_dist = 0.0;

  while (min_dist < sep_dist)
    {
      /*----- Sort list of extrema -----*/
      if (EX_sort)  head_extrema = sort_extrema (head_extrema);

      /*----- Find the two extrema which are closest together -----*/
      min_dist = MAX_DIST;
      extrema_ptr = head_extrema;
      while (extrema_ptr != NULL)
	{
	  if (extrema_ptr->nearest_dist < min_dist)
	    {
	      min_dist = extrema_ptr->nearest_dist;
	      aextrema = extrema_ptr;
	      bextrema = extrema_ptr->nearest_extrema;
	    } 
	  extrema_ptr = extrema_ptr->next_extrema;
	}
      
      /*
	printf ("min_dist = %f \n", min_dist);
      */


      /*----- Merge these two extrema -----*/
      if (min_dist < sep_dist)
	head_extrema = merge_extrema (aextrema, bextrema, head_extrema);
    }

  return (head_extrema);
}


/*---------------------------------------------------------------------------*/
/*
  Convert from integer voxel coordinates to mm.
*/

float * to_3dmm (int ixyz)
{
  float * location = NULL;
  float x, y, z;
  int ix, jy, kz;
  THD_fvec3 fv;
  THD_ivec3 iv;
  

  location = (float *) malloc (sizeof(float) * EX_DIMENSION);

  IJK_TO_THREE (ixyz, ix, jy, kz, EX_nx, EX_nxy);

  iv.ijk[0] = ix;  iv.ijk[1] = jy;  iv.ijk[2] = kz;

  fv = THD_3dind_to_3dmm (EX_dset, iv);
  
  fv = THD_3dmm_to_dicomm (EX_dset, fv);
  
  x = fv.xyz[0];  y = fv.xyz[1];  z = fv.xyz[2];
  
  THD_dicom_to_coorder (&EX_cord, &x, &y, &z);

  location[0] = x;  location[1] = y;  location[2] = z;

  return (location);
}


/*---------------------------------------------------------------------------*/
/*
  Find all extrema within one volume (or slice).
*/

extrema * find_extrema 
(
  float * fvol,                  /* volume or slice of floating point data */ 
  int num_nv,                    /* number of neighboring voxels */
  int nfirst,                    /* index of first voxel in volume or slice */
  int nlast                      /* index of last voxel in volume or slice */
)

{
  int ix, jy, kz, ixyz;          /* current voxel indices */
  int it, ijk;                   /* neighboring voxel indices   */
  int nx, ny, nz, nxy, nxyz;     /* numbers of voxels */
  float fval;                    /* intensity of current voxel */
  float * location = NULL;       /* current voxel coordinates */
  extrema * head_extrema = NULL; /* linked list of extrema */
  int passed_test;               /* flag for voxel is valid extema */

  
  /*----- Initialize local variables -----*/
  nx = EX_nx;      ny = EX_ny;      nz = EX_nz;  
  nxy = nx * ny;   nxyz = nx*ny*nz;
  
  
  /*----- Loop over voxels -----*/
  for (ixyz = nfirst;  ixyz < nlast;  ixyz++)
    {

      /*----- First, check if voxel is inside the mask -----*/
      if (!EX_mask[ixyz]) continue;

      /*----- Does voxel satisfy data threshold criterion? -----*/
      fval = fvol[ixyz];
      if (fabs(fval) < EX_data_thr)  continue; 
	
      /*----- Begin loop over neighboring voxels -----*/
      it = 0;
      passed_test = 1;
      while ((it < num_nv) && passed_test)
	{
	  ijk = ixyz + EX_offset[it];

	  /*----- Check for valid neighbor index -----*/
	  if ((ijk < nfirst) || (ijk >= nlast))
	    {  
	      if (EX_interior)  passed_test = 0;
	    }
  
	  /*----- Early exit if neighbor is not inside the mask -----*/
	  else if (!EX_mask[ijk])  
	    {
	      if (EX_interior)  passed_test = 0;
	    }
	    
	  /*----- Test binary relation -----*/
	  else
	    switch (EX_relation)
	      {
	      case EX_GT: 
		if (fval <= fvol[ijk])
		  passed_test = 0;  break;
	      case EX_GE: 
		if (fval <  fvol[ijk])
		  passed_test = 0;  break;
	      case EX_LT: 
		if (fval >= fvol[ijk])
		  passed_test = 0;  break;
	      case EX_LE: 
		if (fval >  fvol[ijk])
		  passed_test = 0;  break;
	      }

	  it++;
	}


      /*----- If extrema are found, save relevant information -----*/ 
      if (passed_test) 
	{
	  location = to_3dmm (ixyz);
	  head_extrema = new_extrema (location, fval, head_extrema);
	}
    }
  

  /*----- Agglomerate extrema -----*/
  if (EX_sep_dist > 0.0)  head_extrema = agglomerate_extrema (head_extrema,
							      EX_sep_dist);


  /*----- Sort list of extrema -----*/
  if (EX_sort)  head_extrema = sort_extrema (head_extrema);


  return (head_extrema);
}


/*---------------------------------------------------------------------------*/
/*
  Convert from mm to integer voxel coordinates.
*/

int from_3dmm (float * location)
{
  float x, y, z;
  THD_fvec3 fv;
  int ix, jy, kz;
  THD_ivec3 iv;
  int ixyz;

  x = location[0];  y = location[1];  z = location[2];

  THD_coorder_to_dicom (&EX_cord, &x, &y, &z);

  fv.xyz[0] = x;  fv.xyz[1] = y;  fv.xyz[2] = z;

  fv = THD_dicomm_to_3dmm (EX_dset, fv);
 
  iv = THD_3dmm_to_3dind (EX_dset, fv);
    
  ix = iv.ijk[0];  jy = iv.ijk[1];  kz = iv.ijk[2];
 
  if (ix < 0) ix = 0;   if (ix > EX_nx-1) ix = EX_nx-1;
  if (jy < 0) jy = 0;   if (jy > EX_ny-1) jy = EX_ny-1;
  if (kz < 0) kz = 0;   if (kz > EX_nz-1) kz = EX_nz-1;

  ixyz = THREE_TO_IJK (ix, jy, kz, EX_nx, EX_nxy);

  return (ixyz);

}


/*---------------------------------------------------------------------------*/
/*
  Save voxel contents of this extrema into byte array (sub-brick).
*/

void save_extrema (extrema * extrema_ptr, byte iextrema, byte * bar)
{
  int ixyz;

  ixyz = from_3dmm (extrema_ptr->centroid);

  bar[ixyz] = 1;

}


/*---------------------------------------------------------------------------*/
/*
  Save voxel contents of all extrema into byte array (sub-brick).
*/

void save_all_extrema (extrema * extrema_ptr, byte * bar)
{
  byte iextrema = 0;

  while (extrema_ptr != NULL)
    {
      iextrema++;
      save_extrema (extrema_ptr, iextrema, bar);
      extrema_ptr = extrema_ptr->next_extrema;
    }

}


/*---------------------------------------------------------------------------*/
/*
  Display help file.
*/

void EX_Syntax(void)
{
   printf(
    "This program finds local extrema (minima or maxima) of the input       \n"
    "dataset values for each sub-brick of the input dataset.  The extrema   \n"
    "may be determined either for each volume, or for each individual slice.\n"
    "Only those voxels whose corresponding intensity value is greater than  \n"
    "the user specified data threshold will be considered.                  \n"
    "\nUsage: 3dExtrema  options  datasets                                  \n"
    "where the options are:                                                 \n"
   ) ;

   printf(
    "-prefix pname    = Use 'pname' for the output dataset prefix name.     \n"
    "  OR                 [default = NONE; only screen output]              \n"
    "-output pname                                                          \n"
    "                                                                       \n"
    "-session dir     = Use 'dir' for the output dataset session directory. \n"
    "                     [default='./'=current working directory]          \n"
    "                                                                       \n"
    "-quiet           = Flag to suppress screen output                      \n"
    "                                                                       \n"
    "-mask_file mname = Use mask statistic from file mname.                 \n"
    "                   Note: If file mname contains more than 1 sub-brick, \n"
    "                   the mask sub-brick must be specified!               \n"
    "-mask_thr m        Only voxels whose mask statistic is greater         \n"
    "                   than m in abolute value will be considered.         \n"
    "                                                                       \n"
    "-data_thr d        Only voxels whose value (intensity) is greater      \n"
    "                   than d in abolute value will be considered.         \n"
    "                                                                       \n"
    "-sep_dist d        Min. separation distance [mm] for distinct extrema  \n"
    "                                                                       \n"
    "Choose type of extrema (one and only one choice):                      \n"
    "-minima            Find local minima.                                  \n"
    "-maxima            Find local maxima.                                  \n"
    "                                                                       \n"
    "Choose form of binary relation (one and only one choice):              \n"
    "-strict            >  for maxima,  <  for minima                       \n"
    "-partial           >= for maxima,  <= for minima                       \n"
    "                                                                       \n"
    "Choose boundary criteria (one and only one choice):                    \n"
    "-interior          Extrema must be interior points (not on boundary)   \n"
    "-closure           Extrema may be boudary points                       \n"
    "                                                                       \n"
    "Choose domain for finding extrema (one and only one choice):           \n"
    "-slice             Each slice is considered separately                 \n"
    "-volume            The volume is considered as a whole                 \n"
    "                                                                       \n"
    "Choose option for merging of extrema (one and only one choice):        \n"
    "-remove            Remove all but strongest of neighboring extrema     \n"
    "-average           Replace neighboring extrema by average              \n"
    "-weight            Replace neighboring extrema by weighted average     \n"
    "                                                                       \n"
    "Command line arguments after the above are taken to be input datasets. \n"
    "\n") ;

   printf("\n" MASTER_SHORTHELP_STRING ) ;

   exit(0) ;

}


/*---------------------------------------------------------------------------*/
/*
   Read the arguments, load the global variables

*/

int EX_read_opts( int argc , char * argv[] )
{
   int nopt = 1 ;
   char message[80];        /* error message */


   while( nopt < argc ){

      /**** -prefix prefix ****/
      if( strcmp(argv[nopt],"-prefix") == 0 ||
          strcmp(argv[nopt],"-output") == 0   ){
         nopt++ ;
         if( nopt >= argc ){
            EX_error (" need argument after -prefix!");
         }
	 EX_output_prefix = (char *) malloc (sizeof(char) * THD_MAX_PREFIX); 
         MCW_strncpy( EX_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }


      /**** -session directory ****/
      if( strcmp(argv[nopt],"-session") == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            EX_error (" need argument after -session!"); 
         }
	 EX_session = (char *) malloc (sizeof(char) * THD_MAX_NAME); 
         MCW_strncpy( EX_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }


      /**** -quiet ****/
      if( strcmp(argv[nopt],"-quiet") == 0 ){
         EX_quiet = 1;
         nopt++ ; continue ;
      }


      /**** -minima ****/
      if( strcmp(argv[nopt],"-minima") == 0 ){
         EX_maxima = 0;
         nopt++;  continue;
      }


      /**** -maxima ****/
      if( strcmp(argv[nopt],"-maxima") == 0 ){
         EX_maxima = 1;
         nopt++;  continue;
      }


      /**** -strict ****/
      if( strcmp(argv[nopt],"-strict") == 0 ){
         EX_strict = 1;
         nopt++;  continue;
      }


      /**** -partial ****/
      if( strcmp(argv[nopt],"-partial") == 0 ){
         EX_strict = 0;
         nopt++;  continue;
      }


      /**** -interior ****/
      if( strcmp(argv[nopt],"-interior") == 0 ){
         EX_interior = 1;
         nopt++;  continue;
      }


      /**** -closure ****/
      if( strcmp(argv[nopt],"-closure") == 0 ){
         EX_interior = 0;
         nopt++;  continue;
      }


      /**** -slice ****/
      if( strcmp(argv[nopt],"-slice") == 0 ){
         EX_slice = 1;
         nopt++;  continue;
      }


      /**** -volume ****/
      if( strcmp(argv[nopt],"-volume") == 0 ){
         EX_slice = 0;
         nopt++;  continue;
      }


      /**** -sort ****/
      if( strcmp(argv[nopt],"-sort") == 0 ){
         EX_sort = 1;
         nopt++;  continue;
      }


      /**** -mask_file mname ****/
      if( strcmp(argv[nopt],"-mask_file") == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            EX_error (" need 1 argument after -mask_file"); 
         }

	 EX_mask_filename = (char *) malloc (sizeof(char) * THD_MAX_NAME); 
         MCW_strncpy( EX_mask_filename , argv[nopt++] , THD_MAX_NAME ) ;
	 continue;
      }


      /**** -mask_thr m ****/
      if( strcmp(argv[nopt],"-mask_thr") == 0 ){
	 float fval;
         nopt++ ;
         if( nopt >= argc ){
            EX_error (" need 1 argument after -mask_thr"); 
         }
	 sscanf (argv[nopt], "%f", &fval); 
	 if (fval < 0.0){
            EX_error (" Require mask_thr >= 0.0 ");
         }
	 EX_mask_thr = fval;
	 nopt++;  continue;

      }


      /**** -data_thr d ****/
      if( strcmp(argv[nopt],"-data_thr") == 0 ){
	 float fval;
         nopt++ ;
         if( nopt >= argc ){
            EX_error (" need 1 argument after -data_thr"); 
         }
	 sscanf (argv[nopt], "%f", &fval); 
	 if (fval < 0.0){
            EX_error (" Require data_thr >= 0.0 ");
         }
	 EX_data_thr = fval;
	 nopt++;  continue;

      }
      

      /**** -remove ****/
      if( strcmp(argv[nopt],"-remove") == 0 ){
         EX_merge = 1;
         nopt++;  continue;
      }


      /**** -average ****/
      if( strcmp(argv[nopt],"-average") == 0 ){
         EX_merge = 2;
         nopt++;  continue;
      }


      /**** -weight ****/
      if( strcmp(argv[nopt],"-weight") == 0 ){
         EX_merge = 3;
         nopt++;  continue;
      }


      /**** -sep_dist d ****/
      if( strcmp(argv[nopt],"-sep_dist") == 0 ){
	 float fval;
         nopt++ ;
         if( nopt >= argc ){
            EX_error (" need 1 argument after -sep_dist"); 
         }
	 sscanf (argv[nopt], "%f", &fval); 
	 if (fval < 0.0){
            EX_error (" Require data_thr >= 0.0 ");
         }
	 EX_sep_dist = fval;
	 nopt++;  continue;

      }


     /*----- Invalid option -----*/
      if( argv[nopt][0] == '-' ){
	sprintf (message, " Unknown option: %s ", argv[nopt]);
	EX_error (message);
      }


      /*----- Remaining inputs should be parameter datasets -----*/
      break;


   }  /* end of loop over command line arguments */

   
   /*----- Check for valid input -----*/
   if ((EX_merge == 3) && (EX_maxima == 0))
     EX_error ("-weight option is not compatible with -minima option");


   /*----- Set flag for binary relation -----*/
   if ((EX_maxima == 1) && (EX_strict == 1))  EX_relation = EX_GT;
   if ((EX_maxima == 1) && (EX_strict == 0))  EX_relation = EX_GE;
   if ((EX_maxima == 0) && (EX_strict == 1))  EX_relation = EX_LT;
   if ((EX_maxima == 0) && (EX_strict == 0))  EX_relation = EX_LE;


   return (nopt) ;
}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the program: get all operator inputs; create mask
  for voxels above mask.
*/

void * initialize_program (int argc, char * argv[], int * nopt)
{
  const int MIN_NTHR = 10;    /* minimum number of voxels above threshold */

  int iv;                  /* index number of sub-brick */
  void * vfim = NULL;      /* sub-brick data pointer */
  float * ffim = NULL;     /* sub-brick data in floating point format */
  int ixyz, nthr;          /* voxel indices */
  int nx, ny, nz, nxy, nxyz = 0;     /* numbers of voxels */
  char message[80];        /* error message */


  /*----- Save command line for history notes -----*/
  commandline = tross_commandline( PROGRAM_NAME , argc,argv ) ;


  /*----- Does user request help menu? -----*/
  if( argc < 2 || strcmp(argv[1],"-help") == 0 ) EX_Syntax() ;

  
  /*----- Add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv); 


  /*----- Read input options -----*/
  *nopt = EX_read_opts( argc , argv ) ;


  /*----- Get coordinate directions -----*/
  THD_coorder_fill (my_getenv("AFNI_ORIENT"), &EX_cord);


  /*----- Open the mask dataset -----*/
  if (EX_mask_filename != NULL)
    {
      if (!EX_quiet)  printf ("Reading mask dataset: %s \n", EX_mask_filename);
      DOPEN (EX_dset, EX_mask_filename);

      if (EX_dset == NULL)
	{
	  sprintf (message, "Cannot open mask dataset %s", EX_mask_filename); 
	  EX_error (message);
	}

      if (DSET_NVALS(EX_dset) != 1)
	EX_error ("Must specify single sub-brick for mask data");


      /*----- Get dimensions of mask dataset -----*/
      nx   = DSET_NX(EX_dset);   
      ny   = DSET_NY(EX_dset);   
      nz   = DSET_NZ(EX_dset);
      nxy  = nx * ny;  nxyz = nx*ny*nz;


      /*----- Allocate memory for float data -----*/
      ffim = (float *) malloc (sizeof(float) * nxyz);   MTEST (ffim);


      /*----- Convert mask dataset sub-brick to floats (in ffim) -----*/
      iv = DSET_PRINCIPAL_VALUE (EX_dset);
      SUB_POINTER (EX_dset, iv, 0, vfim);
      EDIT_coerce_scale_type (nxyz, DSET_BRICK_FACTOR(EX_dset,iv),
			      DSET_BRICK_TYPE(EX_dset,iv), vfim,   /* input  */
			      MRI_float                   , ffim); /* output */
  
      
      /*----- Allocate memory for mask volume -----*/
      EX_mask = (byte *) malloc (sizeof(byte) * nxyz);
      MTEST (EX_mask);
      
      
      /*----- Create mask of voxels above mask threshold -----*/
      nthr = 0;
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	if (fabs(ffim[ixyz]) >= EX_mask_thr)  
	  { 
	    EX_mask[ixyz] = 1;
	    nthr++;
	  }
	else
	  EX_mask[ixyz] = 0;

      if (!EX_quiet)  
	printf ("Number of voxels above mask threshold = %d \n", nthr);
      if (nthr < MIN_NTHR)  
	{
	  sprintf (message, 
		   "Only %d voxels above mask threshold.  Cannot continue.", 
		   nthr);
	  EX_error (message);
	}


      /*----- Delete floating point sub-brick -----*/
      if (ffim != NULL) { free (ffim);   ffim = NULL; }

      /*----- Delete mask dataset -----*/
      THD_delete_3dim_dataset (EX_dset, False);  EX_dset = NULL ;

    }


  /*----- Open first input dataset -----*/
  {
    /*----- Check if this is an input option -----*/
    if (argv[*nopt][0] == '-')
      EX_error ("ALL input options must precede ALL input datasets");

    /*----- Open the input dataset -----*/
    if (!EX_quiet)  printf ("Reading input dataset: %s \n", argv[*nopt]);
    EX_dset = THD_open_dataset(argv[*nopt]);

    if (EX_dset == NULL)
      {
	sprintf (message, "Cannot open input dataset %s", argv[*nopt]); 
	EX_error (message);
      }

    /*----- Get dimensions of input dataset -----*/
    if (nxyz == 0)
      {
	nx   = DSET_NX(EX_dset);   
	ny   = DSET_NY(EX_dset);   
	nz   = DSET_NZ(EX_dset);
	nxy  = nx * ny;  nxyz = nx*ny*nz;
      }
    
    /*----- Create mask, if not already done -----*/
    if (EX_mask == NULL)
      {
	/*----- Allocate memory for mask volume -----*/
	EX_mask = (byte *) malloc (sizeof(byte) * nxyz);
	MTEST (EX_mask);
	for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	  EX_mask[ixyz] = 1;
      }

  }

  
  /*----- Save dimensions of dataset for compatibility test -----*/
  EX_nx   = nx;     EX_ny   = ny;     EX_nz   = nz;
  EX_nxy  = nxy;    EX_nxyz = nxyz; 


  /*----- The offset array definitions were borrowed from plug_maxima -----*/
  EX_offset[ 0] = +1;     EX_offset[ 8] = nxy;       EX_offset[17] = -nxy;
  EX_offset[ 1] = -1;     EX_offset[ 9] = nxy+1;     EX_offset[18] = -nxy+1;
  EX_offset[ 2] =  nx;    EX_offset[10] = nxy-1;     EX_offset[19] = -nxy-1; 
  EX_offset[ 3] =  nx+1;  EX_offset[11] = nxy+nx;    EX_offset[20] = -nxy+nx;
  EX_offset[ 4] =  nx-1;  EX_offset[12] = nxy+nx+1;  EX_offset[21] = -nxy+nx+1;
  EX_offset[ 5] = -nx;    EX_offset[13] = nxy+nx-1;  EX_offset[22] = -nxy+nx-1;
  EX_offset[ 6] = -nx+1;  EX_offset[14] = nxy-nx;    EX_offset[23] = -nxy-nx;
  EX_offset[ 7] = -nx-1;  EX_offset[15] = nxy-nx+1;  EX_offset[24] = -nxy-nx+1;
                          EX_offset[16] = nxy-nx-1;  EX_offset[25] = -nxy-nx-1;
  
}


/*---------------------------------------------------------------------------*/
/*
  Process all input datasets.
*/

void process_all_datasets (int argc, char * argv[], int nopt)
{
  THD_3dim_dataset * input_dset=NULL;   /* input dataset(s) */

  int iv;                        /* index number of sub-brick */
  void * vfim = NULL;            /* sub-brick data pointer */
  float * ffim = NULL;           /* sub-brick data in floating point format */
  int ibrick, nbricks;           /* sub-brick indices */
  char message[80];              /* error message */
  int nx, ny, nz, nxy, nxyz;     /* numbers of voxels */
  int kz, nfirst, nlast;         /* range of voxel indices */


  /*----- Initialize local variables -----*/
  nx = EX_nx;  ny = EX_ny;  nz = EX_nz;  
  nxy = nx * ny;  nxyz = nx*ny*nz;
  

  /*----- Allocate memory for float data -----*/
  ffim = (float *) malloc (sizeof(float) * EX_nxyz);   MTEST (ffim);


  /*----- Begin loop over input datasets -----*/
  EX_nbricks = 0;
  while (nopt < argc)
    {
      /*----- Check if this is an input option -----*/
      if (argv[nopt][0] == '-')
	EX_error ("ALL input options must precede ALL input datasets");

      /*----- Open the input dataset -----*/
      if (!EX_quiet)  printf ("Reading input dataset: %s \n", argv[nopt]);
      DOPEN (input_dset, argv[nopt]);

      if (input_dset == NULL)
	{
	  sprintf (message, "Cannot open input dataset %s", argv[nopt]); 
	  EX_error (message);
	}

      /*----- Test for dataset compatibility -----*/
      if ((EX_nx != DSET_NX(input_dset)) || (EX_ny != DSET_NY(input_dset))
	  || (EX_nz != DSET_NZ(input_dset)))
	{
	  sprintf (message, "Input dataset %s has incompatible dimensions",
		   argv[nopt]); 
	  EX_error (message);
	}
	
     
      /*----- Get number of volumes specified for this dataset -----*/
      nbricks = DSET_NVALS(input_dset);


      /*----- Loop over sub-bricks selected from input dataset -----*/
      for (ibrick = 0;  ibrick < nbricks;  ibrick++)
	{
	  if (!EX_quiet)  printf ("Reading volume #%d \n", EX_nbricks);

	  SUB_POINTER (input_dset, ibrick, 0, vfim);
	  EDIT_coerce_scale_type 
	    (EX_nxyz, DSET_BRICK_FACTOR(input_dset,ibrick),
	     DSET_BRICK_TYPE(input_dset,ibrick), vfim,   /* input  */
	     MRI_float                         , ffim);  /* output */
	  
	  if (EX_slice)  /*----- Find extrema slice-by-slice -----*/
	    {
	      for (kz = 0;  kz < nz;  kz++)
		{
		  nfirst = kz*nxy;  nlast = nfirst + nxy;
		  EX_head_extrema[EX_num_ll] 
		    = find_extrema (ffim, 8, nfirst, nlast);
		  EX_num_ll++;
		  if (EX_num_ll >= EX_MAX_LL)  
		    EX_error ("Exceeded Max. Number of Linked Lists");
		}
	    }
	  else           /*----- Find extrema for entire volume -----*/
	    {
	      EX_head_extrema[EX_num_ll] = find_extrema (ffim, 26, 0, nxyz);
	      EX_num_ll++;
	      if (EX_num_ll >= EX_MAX_LL)  
		EX_error ("Exceeded Max. Number of Linked Lists");
	    }

	  /*----- Increment count of sub-bricks -----*/
	  EX_nbricks++;  
	}


      /*----- Delete input dataset -----*/
      THD_delete_3dim_dataset (input_dset, False);  input_dset = NULL ;
     
      nopt++;
    }


  /*----- Delete floating point sub-brick -----*/
  if (ffim != NULL) { free (ffim);   ffim = NULL; }


  if (!EX_quiet)  printf ("Number of volumes = %d \n", EX_nbricks);
  if (EX_nbricks < 1)  EX_error ("No input data?");


}


/*---------------------------------------------------------------------------*/
/*
  Write extrema to output bucket dataset.
*/

void write_bucket ()

{
  THD_3dim_dataset * new_dset = NULL;      /* output bucket dataset */
  int ixyz, nxyz;                  /* voxel indices */
  int kz, nz;                      /* slice indices */
  extrema * head_extrema = NULL;   /* pointer to linked list of extrema */
  byte ** bar = NULL;              /* array of extrema sub-bricks */
  int nbricks;                     /* number of extrema sub-bricks */
  int ibrick;                      /* extrema sub-brick index */
  int ierror;                      /* number of errors in editing data */

  char message[80];           /* error message */


  /*----- Initialize local variables -----*/
  nz = EX_nz;  
  nxyz = EX_nxyz;
  nbricks = EX_nbricks;
  if (!EX_quiet) 
    printf ("\nOutput dataset will have %d sub-bricks\n", nbricks);


  /*-- Make an empty copy of input dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (EX_dset);


  /*----- Record history of dataset -----*/
  tross_Copy_History (EX_dset, new_dset);
  if( commandline != NULL ) tross_Append_History( new_dset , commandline ) ;


  /*----- Modify some structural properties.  Note that the nbricks
          just make empty sub-bricks, without any data attached. -----*/
  ierror = EDIT_dset_items (new_dset,
                            ADN_prefix,          EX_output_prefix,
			    ADN_directory_name,  EX_session,
			    ADN_type,            HEAD_FUNC_TYPE,
			    ADN_func_type,       FUNC_BUCK_TYPE,
                            ADN_ntt,             0,               /* no time */
			    ADN_nvals,           nbricks,
			    ADN_malloc_type,     DATABLOCK_MEM_MALLOC ,  
			    ADN_none ) ;
  
  if( ierror > 0 )
    {
      sprintf (message, 
	      " %d errors in attempting to create bucket dataset! ", 
	      ierror);
      EX_error (message);
    }
  
  if (THD_is_file(DSET_HEADNAME(new_dset))) 
    {
      sprintf (message,
	      " Output dataset file %s already exists--cannot continue! ",
	      DSET_HEADNAME(new_dset));
      EX_error (message);
    }


  /*----- Allocate memory -----*/
  bar  = (byte **) malloc (sizeof(byte *) * nbricks);
  MTEST (bar);
  

  /*----- Save extrema into sub-bricks -----*/
  for (ibrick = 0;  ibrick < nbricks;  ibrick++)
    {
      /*----- allocate memory for output sub-brick -----*/
      bar[ibrick]  = (byte *) malloc (sizeof(byte) * nxyz);
      MTEST (bar[ibrick]);

      /*----- Save extrema into output sub-brick -----*/
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)	   
	bar[ibrick][ixyz] = 0;
      if (EX_slice)
	for (kz = 0;  kz < nz;  kz++)
	  {
	    head_extrema = EX_head_extrema[kz+ibrick*nz];
	    save_all_extrema (head_extrema, bar[ibrick]); 	    
	  }
      else
	{
	  head_extrema = EX_head_extrema[ibrick];
	  save_all_extrema (head_extrema, bar[ibrick]); 
	}

      /*----- attach bar[ib] to be sub-brick #ibrick -----*/
      EDIT_substitute_brick (new_dset, ibrick, MRI_byte, bar[ibrick]);

    }


  /*----- Output the extrema dataset -----*/
  if( !EX_quiet ) printf("Computing sub-brick statistics\n") ;
  THD_load_statistics( new_dset ) ;

  if( !EX_quiet ) printf("Writing output to %s and %s\n",
     DSET_HEADNAME(new_dset) , DSET_BRIKNAME(new_dset) );
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  

  /*----- Deallocate memory for extrema dataset -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
  
}


/*---------------------------------------------------------------------------*/
/*
  Output the results.
*/

void output_results ()
{
  int islice,  ibrick;
  int nslices, nbricks;

    
  /*----- Initialize local variables -----*/
  nbricks = EX_nbricks;
  nslices = EX_nz;


  /*----- Write extrema to screen -----*/
  if (!EX_quiet)
    if (EX_slice)
      for (ibrick = 0;  ibrick < nbricks;  ibrick++)
	{
	  for (islice = 0;  islice < nslices;  islice++)
	    {
	      print_all_extrema (ibrick, islice, 
				 EX_head_extrema[islice + ibrick*nslices]);
	    }
	}
    else
      for (ibrick = 0;  ibrick < nbricks;  ibrick++)
	{
	  print_all_extrema (ibrick, -1, EX_head_extrema[ibrick]);
	}


  /*----- Write extrema to bucket dataset -----*/
  if (EX_output_prefix != NULL)
    write_bucket ();

}


/*---------------------------------------------------------------------------*/

int main( int argc , char * argv[] )

{
  int nopt;
  

  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");


  /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/
  machdep() ; 
  { int new_argc ; char ** new_argv ;
  addto_args( argc , argv , &new_argc , &new_argv ) ;
  if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
  }
  

  /*----- Initialize program:  get all operator inputs; 
    create mask for voxels above mask threshold -----*/
  initialize_program (argc, argv, &nopt);


  /*----- Process all datasets -----*/
  process_all_datasets (argc, argv, nopt);


  /*----- Output the results -----*/
  output_results ();


  exit(0) ;
}


/*---------------------------------------------------------------------------*/


