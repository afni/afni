/*---------------------------------------------------------------------------*/
/*
  This file contains routines for hierarchical clustering of user specified 
  parametric data.


  File:    StatClust.c
  Author:  B. Douglas Ward
  Date:    08 October 1999

  Mod:     Replaced C "pow" function, significantly improving execution speed.
  Date:    11 October 1999


  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.

*/

/*---------------------------------------------------------------------------*/
/*
  Structure declarations 
*/


struct cluster;
struct voxel;


typedef struct cluster
{
  float * centroid;
  int num_voxels;
  struct voxel * voxel_ptr;
  float nearest_dist;
  struct cluster * nearest_cluster;
  struct cluster * next_cluster;
} cluster;


typedef struct voxel
{
  int index;
  struct voxel * next_voxel;
} voxel;


/*---------------------------------------------------------------------------*/
/*
  Create a new voxel having the given index number.
*/

voxel * new_voxel (int index)
{
  voxel * voxel_ptr = NULL;
  
  voxel_ptr = (voxel *) malloc (sizeof(voxel));
  MTEST (voxel_ptr);

  voxel_ptr->index = index;
  voxel_ptr->next_voxel = NULL;
  return (voxel_ptr);
}


/*---------------------------------------------------------------------------*/
/*
  Print the index number for this voxel.
*/

void print_voxel (voxel * voxel_ptr)
{
  if (voxel_ptr != NULL)
    printf ("%d ", voxel_ptr->index);
}


/*---------------------------------------------------------------------------*/
/*
  Print all voxels in the linked list.
*/

void print_all_voxels (voxel * voxel_ptr)
{
  while (voxel_ptr != NULL)
    {
      print_voxel (voxel_ptr);
      voxel_ptr = voxel_ptr->next_voxel;
    }
  printf ("\n");
}


/*---------------------------------------------------------------------------*/
/*
  Create an empty cluster.
*/
  
cluster * initialize_cluster ()
{
  cluster * clust_ptr = NULL;

  clust_ptr = (cluster *) malloc (sizeof(cluster));
  MTEST (clust_ptr);

  clust_ptr->next_cluster = NULL;
  clust_ptr->num_voxels = 0;
  clust_ptr->voxel_ptr = NULL;
  clust_ptr->centroid = NULL;
  clust_ptr->nearest_dist = 0.0;
  clust_ptr->nearest_cluster = NULL;
  return (clust_ptr);
  
}


/*---------------------------------------------------------------------------*/
/*
  Print the contents of one cluster.
*/

void print_cluster (cluster * clust_ptr, char * str, matrix s)
{
  int i;
  vector v, sv;
  vector_initialize (&v);
  vector_initialize (&sv);

  printf ("Cluster %s \n", str);

  if (clust_ptr->voxel_ptr != NULL)
    {
      printf ("# Voxels = %d \n", clust_ptr->num_voxels);

      /*
      printf ("Voxels: ");
      
      print_all_voxels (clust_ptr->voxel_ptr);
      */
    }

  if (clust_ptr->centroid != NULL)
    {
      printf ("Centroid: \n");
      array_to_vector (SC_dimension, clust_ptr->centroid, &v);
      vector_multiply (s, v, &sv);
      vector_print (sv);
    }

  /*
  printf ("Nearest cluster distance = %f \n", clust_ptr->nearest_dist);
  */

  vector_destroy (&v);
  vector_destroy (&sv);
}


/*---------------------------------------------------------------------------*/
/*
  Print the contents of all clusters in the linked list.
*/

void print_all_clusters (cluster * clust_ptr, matrix s)
{
  int iclust = 0;
  char str[30];

  while (clust_ptr != NULL)
    {
      iclust++;
      sprintf (str, "#%d", iclust);
      print_cluster (clust_ptr, str, s);
      printf ("\n");
      clust_ptr = clust_ptr->next_cluster;
    }
  printf ("\n");
}


/*---------------------------------------------------------------------------*/
/*
  Save voxel contents of this cluster into byte array (sub-brick).
*/

void save_cluster (cluster * clust_ptr, byte iclust, byte * bar)
{
  int i;
  voxel * voxel_ptr = NULL;


  voxel_ptr = clust_ptr->voxel_ptr;


  while (voxel_ptr != NULL)
    {
      bar[voxel_ptr->index] = iclust;
      voxel_ptr = voxel_ptr->next_voxel;
    }

}


/*---------------------------------------------------------------------------*/
/*
  Save voxel contents of all clusters into byte array (sub-brick).
*/

void save_all_clusters (cluster * clust_ptr, byte * bar)
{
  byte iclust = 0;

  while (clust_ptr != NULL)
    {
      iclust++;
      save_cluster (clust_ptr, iclust, bar);
      clust_ptr = clust_ptr->next_cluster;
    }

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the distance between two clusters as the Euclidean distance
  between their centroids.
*/

float cluster_distance (cluster * aclust, cluster * bclust)
{
  float sumsqr;
  float delta;
  int i;

  sumsqr = 0.0;
  for (i = 0;  i < SC_dimension;  i++)
    {
      delta = aclust->centroid[i] - bclust->centroid[i];
      sumsqr += delta * delta;
    }

  return ((sumsqr));
  
}


/*---------------------------------------------------------------------------*/
/*
  Find the cluster which is nearest to new_cluster.
  Set the nearest_dist and nearest_cluster structure elements accordingly.
*/
  
void find_nearest_cluster (cluster * new_cluster, cluster * head_cluster)
{
  const float MAX_DIST = 1.0e+30;

  cluster * clust_ptr = NULL;
  float dist;


  /*----- Initialize nearest cluster elements -----*/
  new_cluster->nearest_dist = MAX_DIST;
  new_cluster->nearest_cluster = NULL;


  clust_ptr = head_cluster;

  while (clust_ptr != NULL)
    {
      if (clust_ptr != new_cluster)
	{
	  dist = cluster_distance (new_cluster, clust_ptr);

	  if (dist < new_cluster->nearest_dist)
	    {
	      new_cluster->nearest_dist = dist;
	      new_cluster->nearest_cluster = clust_ptr;
	    }

	  if (dist < clust_ptr->nearest_dist)
	    {
	      clust_ptr->nearest_dist = dist;
	      clust_ptr->nearest_cluster = new_cluster;
	    }
	}

      clust_ptr = clust_ptr->next_cluster;
    }
}


/*---------------------------------------------------------------------------*/
/*
  Add a new cluster to the linked list of clusters.
*/

void add_cluster (cluster * new_cluster, cluster * head_cluster)
{

  new_cluster->next_cluster = head_cluster;

  find_nearest_cluster (new_cluster, head_cluster);

}


/*---------------------------------------------------------------------------*/
/*
  Create a new cluster containing a single voxel, and add to list of clusters.
*/

cluster * new_cluster (int index, float * centroid, cluster * head_clust)
{
  cluster * clust_ptr = NULL;
  voxel * voxel_ptr = NULL;

  clust_ptr = initialize_cluster ();

  clust_ptr->num_voxels = 1;
  clust_ptr->voxel_ptr = new_voxel(index);
  clust_ptr->centroid = centroid;

  add_cluster (clust_ptr, head_clust);

  return (clust_ptr);
  
}


/*---------------------------------------------------------------------------*/
/*
  Deallocate memory for this cluster.
*/

void delete_cluster (cluster * clust_ptr)
{
  if (clust_ptr != NULL)
    {
      clust_ptr->voxel_ptr = NULL;

      if (clust_ptr->centroid != NULL)
	{
	  free (clust_ptr->centroid);
	  clust_ptr->centroid = NULL;
	}

      free (clust_ptr);
    }
}


/*---------------------------------------------------------------------------*/
/*
  Remove two clusters from linked list of clusters.
  Reset cluster pointers, and recalculate nearest cluster distances 
  where needed.  Finally, delete the two clusters from memory.
*/

cluster * remove_clusters (cluster * aclust, cluster * bclust, 
			   cluster * head_clust)
{
  cluster * clust_ptr = NULL;
  cluster * next_clust = NULL;
  

  while ((head_clust != NULL) && 
	 ((head_clust == aclust) || (head_clust == bclust)))
    head_clust = head_clust->next_cluster;


  if (head_clust != NULL)
    {

      clust_ptr = head_clust;
      next_clust = clust_ptr->next_cluster;
      while (next_clust != NULL)
	{
	  if ((next_clust == aclust) || (next_clust == bclust))
	    clust_ptr->next_cluster = next_clust->next_cluster;
	  else
	    clust_ptr = next_clust;
	  
	  next_clust = clust_ptr->next_cluster;
	}


      clust_ptr = head_clust;
      while (clust_ptr != NULL)
	{
	  if ((clust_ptr->nearest_cluster == aclust) 
	      || (clust_ptr->nearest_cluster == bclust))
	    {
	      find_nearest_cluster (clust_ptr, head_clust);
	    }
	  clust_ptr = clust_ptr->next_cluster;
	}
    }


  delete_cluster (aclust);
  delete_cluster (bclust);

  return (head_clust);
}


/*---------------------------------------------------------------------------*/
/*
  Merge two clusters into one new cluster.
*/

cluster * merge_clusters (cluster * aclust, cluster * bclust)
{
  cluster * abclust = NULL;
  voxel * voxel_ptr = NULL;
  int na, nb;
  int i;

  abclust = initialize_cluster ();

  na = aclust->num_voxels;
  nb = bclust->num_voxels;
  abclust->num_voxels = na + nb;

  abclust->centroid = (float *) malloc (sizeof(float) * SC_dimension);
  MTEST (abclust->centroid);

  for (i = 0;  i < SC_dimension;  i++)
    abclust->centroid[i] 
      = (na*aclust->centroid[i] + nb*bclust->centroid[i]) / (na+nb);

  abclust->voxel_ptr = aclust->voxel_ptr;

  voxel_ptr = abclust->voxel_ptr;
  while (voxel_ptr->next_voxel != NULL)
    voxel_ptr = voxel_ptr->next_voxel;
  voxel_ptr->next_voxel = bclust->voxel_ptr;
  
  return (abclust);
}


/*---------------------------------------------------------------------------*/
/*
  Consolidate two clusters.
*/
  
cluster * consolidate_clusters (cluster * aclust, cluster * bclust, 
				cluster * head_clust)
{
  cluster * abclust = NULL;


  /*----- Merge two clusters into one new cluster -----*/
  abclust = merge_clusters (aclust, bclust);


  /*----- Remove the two original clusters from the linked list -----*/
  head_clust = remove_clusters (aclust, bclust, head_clust);


  /*----- Add the merged cluster to the linked list -----*/
  add_cluster (abclust, head_clust);
  

  /*----- Merged cluster is now at the top of the list -----*/
  return (abclust);
}


/*---------------------------------------------------------------------------*/
/*
  Agglomerate clusters by merging the two clusters which are closest together.
*/

cluster * agglomerate_clusters (cluster * head_clust)
{
  const float MAX_DIST = 1.0e+30;

  cluster * clust_ptr = NULL;
  cluster * aclust    = NULL;
  cluster * bclust    = NULL;
  float min_dist;


  /*----- Find the two clusters which are closest together -----*/
  min_dist = MAX_DIST;
  clust_ptr = head_clust;
  while (clust_ptr != NULL)
    {
      if (clust_ptr->nearest_dist < min_dist)
	{
	  min_dist = clust_ptr->nearest_dist;
	  aclust = clust_ptr;
	  bclust = clust_ptr->nearest_cluster;
	} 
      clust_ptr = clust_ptr->next_cluster;
    }

  /*
  printf ("min_dist = %f \n", min_dist);
  */


  /*----- Merge these two clusters -----*/
  head_clust = consolidate_clusters (aclust, bclust, head_clust);


  return (head_clust);
}


/*---------------------------------------------------------------------------*/
/*
  Sort clusters in order of size.
*/

cluster * sort_clusters (cluster * head_clust)
{
  cluster * i  = NULL; 
  cluster * ip = NULL; 
  cluster * m  = NULL;
  cluster * mp = NULL;
  cluster * j  = NULL;
  cluster * jp = NULL;
  cluster * guard = NULL;


  /*----- Create guard cluster in case head cluster must be replaced -----*/
  guard = initialize_cluster();
  guard->next_cluster = head_clust;
  ip = guard;

  while (ip->next_cluster != NULL)
    {
      /*----- Initialize search for next largest cluster -----*/
      i = ip->next_cluster;  /* current top of list */
      mp = ip;               /* cluster pointing to next largest cluster */
      m = i;                 /* next largest cluster */
      jp = i;

      /*----- Search through list for next largest cluster -----*/
      while (jp->next_cluster != NULL)
	{
	  j = jp->next_cluster;
	  if (j->num_voxels > m->num_voxels)
	    {
	      mp = jp;
	      m = j;
	    }
	  jp = j;
	}

      /*----- Now move next largest cluster to top of list -----*/
      if (m != i)
	{
	  ip->next_cluster = m;
	  mp->next_cluster = m->next_cluster;
	  m->next_cluster = i;
	}

      /*----- Move down the list -----*/
      ip = i;
	
    }

  
  /*----- Replace head cluster -----*/
  head_clust = guard->next_cluster;
  delete_cluster (guard);

  return (head_clust);
}


/*---------------------------------------------------------------------------*/
/*
  Calculate covariance matrix for input parameters.
  Return square root of covariance matrix, and its inverse.
*/

void calc_covariance 
(
  THD_3dim_dataset * param_dset,   /* parameters for clustering */
  int * num_voxels,                /* count of voxels above threshold */
  matrix * s,                /* square root of covariance matrix */
  matrix * sinv              /* inverse of square root of covariance matrix */
)

{
  int ts_length;                   /* number of parameter sub-bricks */
  float * ts_array = NULL;         /* parameter array including threshold */
  float * par_array = NULL;        /* parameter array without threshold */
  int ixyz, nxyz;                  /* voxel indices */
  int ip, jp;                      /* parameter indices */
  int ok;                          /* Boolean for successful matrix calc. */

  vector mean;                /* mean parameter vector */ 
  matrix covar;               /* variance-covariance matrix */
  matrix cinv;                /* inverse of covariance matrix */

  char message[80];           /* error message */


  /*----- Initialize vectors and matrices -----*/
  vector_initialize (&mean);
  matrix_initialize (&covar);
  matrix_initialize (&cinv);


  /*----- Initialize local variables -----*/
  nxyz = param_dset->daxes->nxx * param_dset->daxes->nyy 
       * param_dset->daxes->nzz;       
  ts_length = SC_dimension + 1;


  /*----- Allocate space for mean and covariance matrices -----*/
  vector_create (SC_dimension, &mean);
  matrix_create (SC_dimension, SC_dimension, &covar);


  /*----- Set up array to hold vector of parameters -----*/
  ts_array = (float *) malloc (sizeof(float) * ts_length);
  MTEST (ts_array);
  par_array = ts_array + 1;


  /*----- Initialize voxel counter -----*/
  (*num_voxels) = 0;


  /*----- Iterate over all voxels -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      /*----- Extract threshold and parameter data for this voxel -----*/
      extract_ts_array (param_dset, ixyz, ts_array);  

      /*----- Check statistical threshold for this voxel -----*/
      if (fabs(ts_array[0]) > SC_thr)
	{
	  /*----- Count the number of voxels above the threshold -----*/
	  (*num_voxels)++;
	  
	  /*----- Calculate parameter sums and products  -----*/
	  if (SC_statdist)
	    {
	      for (ip = 0;  ip < SC_dimension;  ip++)
		{
		  mean.elts[ip] += par_array[ip];
		  for (jp = 0;  jp < SC_dimension;  jp++)
		    if ((ip == jp) || (SC_statdist == 2))
		      covar.elts[ip][jp] += par_array[ip] * par_array[jp];
		}
	    }
	}
    }

  if (SC_verb) 
    printf ("Number of voxels above threshold = %d \n", *num_voxels);
 
 if ((*num_voxels) < 2)  
    {
      sprintf (message, "Only %d voxels above threshold.  Cannot continue.",
	       *num_voxels);
      SC_error (message);
    }


  if (SC_statdist)
    {
      /*----- Calculate the mean parameter vector -----*/
      for (ip = 0;  ip < SC_dimension;  ip++)
	mean.elts[ip] = mean.elts[ip] / (*num_voxels);
      if (SC_verb)  
	vector_sprint ("Mean parameter vector: ", mean);
      
      /*----- Calculate the covariance matrix -----*/
      for (ip = 0;  ip < SC_dimension;  ip++)
	for (jp = 0;  jp < SC_dimension;  jp++)
	  if ((ip == jp) || (SC_statdist == 2)) 
	    covar.elts[ip][jp] = (covar.elts[ip][jp] 
	      - (*num_voxels) * mean.elts[ip] * mean.elts[jp])
	      / ((*num_voxels) - 1);
      if (SC_verb)
	matrix_sprint ("Parameter covariance matrix: ", covar);

      /*----- Note:  The following sequence of calculations may apppear
              to be inefficient, but it is necessary in order to generate
              an error message in the event of perfectly correlated 
	      input parameters -----*/

      /*----- Calculate inverse of covariance matrix -----*/
      ok = matrix_inverse (covar, &cinv);
      if (! ok)  
	SC_error 
	  ("Unable to calculate inverse of covariance matrix");

      /*----- Calculate square root of inverse covariance matrix -----*/
      ok = matrix_sqrt (cinv, sinv);
      if (! ok)  
	SC_error 
	  ("Unable to calculate square root of inverse of covariance matrix");
  
      /*----- Calculate square root of covariance matrix -----*/
      ok = matrix_inverse (*sinv, s);
      if (! ok)  
	SC_error 
	  ("Unable to calculate square root of covariance matrix");
    }
  
  else
    {
      matrix_identity (SC_dimension, s);
      matrix_identity (SC_dimension, sinv);
    }
  

  /*----- Deallocate memory -----*/
  free (ts_array);   ts_array = NULL;
  vector_destroy (&mean);
  matrix_destroy (&covar);
  matrix_destroy (&cinv);

  
}


/*---------------------------------------------------------------------------*/







