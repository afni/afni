#include "mrilib.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/


#undef  AFNI_DEBUG
#undef  CLUST_DEBUG
#define STATUS(x) /* nada */
#define ENTRY(x)  /* nada */
#define EXRETURN  return
#define RETURN(x) return(x)


/*-----------------------------------------------------------------------------
   Routine to edit cluster array.

      clar        = cluster array to be edited
      edit_clust  = flag to indicate cluster editing option
      dxyz        = volume of a voxel
      vmul        = minimum volume for a cluster

   The edited cluster array is returned in clar.

   Author :  B. D. Ward
   Date   :  10 September 1996

   Modified 09 June 1998 by RWCox to add ECFLAG_ORDER option.
-----------------------------------------------------------------------------*/

void EDIT_cluster_array (MCW_cluster_array * clar, int edit_clust,
                         float dxyz, float vmul)
{
   int iclu;       /* cluster index */
   int nclu;       /* non-empty cluster index */
   int ii;         /* voxel index */
   float
      mag,         /* voxel intensity */
      sum,         /* sum of voxel intensities */
      max,         /* maximum of voxel intensities */
      amax,        /* maximum of absolute voxel intensities */
      smax,        /* signed maximum of absolute voxel intensities */
      mean,        /* mean of voxel intensities */
      size;        /* size of cluster (multiples of vmul) */

   if( edit_clust == ECFLAG_ORDER ){
      SORT_CLARR(clar) ;
   }

   nclu = 0;
   for (iclu = 0; iclu < clar->num_clu; iclu++)
   {
      if ((clar->clar[iclu] != NULL) && (clar->clar[iclu]->num_pt > 0))
      {
         nclu++;

         /* initialization of basic statistics for this cluster */
         sum = max = smax = clar->clar[iclu]->mag[0];
         amax = fabs(smax);

         /* calculate basic statistics for this cluster */
         for (ii = 1; ii < clar->clar[iclu]->num_pt; ii++)
         {
            mag = clar->clar[iclu]->mag[ii];
            switch (edit_clust)
            {
               case ECFLAG_MEAN :
                  sum += mag;  break;
               case ECFLAG_MAX  :
                  if (mag > max)  max = mag;   break;
               case ECFLAG_AMAX :
                  if (fabs(mag) > amax)  amax = fabs(mag);  break;
               case ECFLAG_SMAX :
                  if (fabs(mag) > fabs(smax))  smax = mag;  break;
               case ECFLAG_SIZE : break;
               default          : break;
            }

         }

         /* additional calculations */
         if (edit_clust == ECFLAG_MEAN)
            mean = sum / clar->clar[iclu]->num_pt;
         if (edit_clust == ECFLAG_SIZE)
            size = clar->clar[iclu]->num_pt * dxyz / vmul;

         /* set all voxel intensities in this cluster to the same value */
         for (ii = 0; ii < clar->clar[iclu]->num_pt; ii++)
         {
            switch (edit_clust)
            {
               case ECFLAG_MEAN :  clar->clar[iclu]->mag[ii] = mean;
               case ECFLAG_MAX  :  clar->clar[iclu]->mag[ii] = max;   break;
               case ECFLAG_AMAX :  clar->clar[iclu]->mag[ii] = amax;  break;
               case ECFLAG_SMAX :  clar->clar[iclu]->mag[ii] = smax;  break;
               case ECFLAG_SIZE :  clar->clar[iclu]->mag[ii] = size;  break;
               case ECFLAG_ORDER:  clar->clar[iclu]->mag[ii] = nclu;  break;
               default          :                                     break;
            }
         }
      }
   }  /* iclu */

   return;
}
