#include "mrilib.h"

#undef  AFNI_DEBUG
#undef  CLUST_DEBUG
#define STATUS(x) /* nada */
#define ENTRY(x)  /* nada */
#define EXRETURN  return
#define RETURN(x) return(x)


/*-----------------------------------------------------------------------------
   Routine to filter volume data.

      nx, ny, nz   = number of voxels along each axis
      dx, dy, dz   = voxel dimensions
      fim_type     = volume data type
      vfim         = volume data itself
      filter_opt   = indicates filtering method to be applied
      filter_rmm   = radius of "influence" of neighboring voxels

   The filtered data is returned in vfim.

   Author :  B. D. Ward
   Date   :  11 September 1996
   Mod    :  2 October 1996        Changed memory deallocation.
   Mod    :  9 October 1996        Made changes to improve speed.

   To correct error due to abiguity in identification of clusters,
   voxel coordinates are now stored as 3 separate short integers.
   BDW  06 March 1997

-----------------------------------------------------------------------------*/

void EDIT_filter_volume (int nx, int ny, int nz, float dx, float dy, float dz,
                   int fim_type, void * vfim, int filter_opt, float filter_rmm)
{
   MCW_cluster * mask;                   /* mask for filtering */
   int nxy, nxyz;                        /* dimensions of volume data */
   int mnum;                             /* number of points in mask */
   int i, j, k, ii, jj, kk;              /* voxel indices */
   int ijkvox, ijkma, jma;               /* more voxel indices */
   float * ffim, * ffim_out;             /* floating point fim's */

   float
      mag,                 /* voxel intensity */
      sum,                 /* sum of voxel intensities */
      sumnz,               /* sum of non-zero voxel intensities */
      mean,                /* mean of voxel intensities */
      max,                 /* maximum of voxel intensities */
      amax,                /* maximum of absolute voxel intensities */
      smax;                /* signed maximum of absolute voxel intensities */
   int
      npts, nznpts;        /* number of points in average */

   nxy = nx*ny;  nxyz = nxy*nz;

   /***--- 07 Jan 1998 ---***/
   if( filter_opt == FCFLAG_AVER ){
      if( fim_type != MRI_float ){
         ffim = (float *) malloc (sizeof(float) * nxyz);
         if( ffim == NULL ){
            fprintf(stderr,"EDIT_filter_volume: no workspace!\n") ;
            exit(1) ;
         }
         EDIT_coerce_type (nxyz, fim_type, vfim, MRI_float, ffim);
      } else {
         ffim = (float *) vfim ;
      }
      EDIT_aver_fvol(nx,ny,nz,dx,dy,dz,ffim,filter_rmm) ;
      if( ffim != vfim ){
         EDIT_coerce_autoscale(nxyz, MRI_float, ffim, fim_type, vfim);
         free(ffim) ;
      }
      return ;
   }
   /***--- end 07 Jan 1998 ---***/

   /*--- Make a cluster that is a mask of points closer than filter_rmm ---*/
   mask = MCW_build_mask (nx, ny, nz, dx, dy, dz, filter_rmm);
   if (mask == NULL)
   {
      fprintf (stderr, "Warning: Filter option has no effect. \n");
      return;
   }
   mnum = mask->num_pt;

   /*--- Allocate space for floating point data ---*/
   ffim = (float *) malloc (sizeof(float) * nxyz);
   if (ffim == NULL)
   {
      fprintf (stderr, "\n Error: cannot allocate filter workspace! \n");
      exit(1);
   }
   ffim_out = (float *) malloc (sizeof(float) * nxyz);
   if (ffim_out == NULL)
   {
      fprintf (stderr, "\n Error: cannot allocate filter workspace! \n");
      exit(1);
   }

   /*--- Convert vfim to floating point data ---*/
   EDIT_coerce_type (nxyz, fim_type, vfim, MRI_float, ffim);

   /*--- Iteration over all voxels in vfim ---*/
   for (k = 0; k < nz; k++)
   {
      for (j = 0; j < ny; j++)
      {
         for (i = 0; i < nx; i++)
         {
            /*--- Initialization for filtering of voxel #(i,j,k) ---*/
            ijkvox = THREE_TO_IJK (i, j, k, nx, nxy);
            mag = ffim[ijkvox];
            switch (filter_opt)
            {
               case FCFLAG_MEAN:
                  sum = mag;  npts = 1;  break;
               case FCFLAG_NZMEAN:
                  if (mag != 0.0)
                     {sumnz = mag;   nznpts = 1;}
                  else
                     {sumnz = 0.0;   nznpts = 0;}
                  break;
               case FCFLAG_MAX:
                  max = mag;  break;
               case FCFLAG_AMAX:
                  amax = fabs(mag);  break;
               case FCFLAG_SMAX:
                  smax = mag;  break;
               default:  break;
               }

            /*--- Now iterate over the positions in the mask ---*/
            switch (filter_opt)
	      {
	      case FCFLAG_MEAN:
		for (jma = 0; jma < mnum; jma++)
		  {
		    ii = i + mask->i[jma];
		    jj = j + mask->j[jma];
		    kk = k + mask->k[jma];
		    if (ii < 0   || jj < 0   || kk < 0 ||
			ii >= nx || jj >= ny || kk >= nz)  continue;
		    ijkma = THREE_TO_IJK (ii, jj, kk, nx, nxy);
		    mag = ffim[ijkma];
		    sum += mag;  npts++;
                  }
                  break;
	      case FCFLAG_NZMEAN:
		for (jma = 0; jma < mnum; jma++)
                  {
		    ii = i + mask->i[jma];
		    jj = j + mask->j[jma];
		    kk = k + mask->k[jma];
		    if (ii < 0   || jj < 0   || kk < 0 ||
			ii >= nx || jj >= ny || kk >= nz)  continue;
		    ijkma = THREE_TO_IJK (ii, jj, kk, nx, nxy);
		    mag = ffim[ijkma];
		    if (mag != 0.0)  {sumnz += mag;  nznpts++;}
                  }
		break;
	      case FCFLAG_MAX:
		for (jma = 0; jma < mnum; jma++)
                  {
		    ii = i + mask->i[jma];
		    jj = j + mask->j[jma];
		    kk = k + mask->k[jma];
		    if (ii < 0   || jj < 0   || kk < 0 ||
			ii >= nx || jj >= ny || kk >= nz)  continue;
		    ijkma = THREE_TO_IJK (ii, jj, kk, nx, nxy);
		    mag = ffim[ijkma];
		    if (mag > max)  max = mag;
                  }
                  break;
	      case FCFLAG_AMAX:
		for (jma = 0; jma < mnum; jma++)
                  {
		    ii = i + mask->i[jma];
		    jj = j + mask->j[jma];
		    kk = k + mask->k[jma];
		    if (ii < 0   || jj < 0   || kk < 0 ||
			ii >= nx || jj >= ny || kk >= nz)  continue;
		    ijkma = THREE_TO_IJK (ii, jj, kk, nx, nxy);
		    mag = ffim[ijkma];
		    if (fabs(mag) > amax)  amax = fabs(mag);
                  }
		break;
	      case FCFLAG_SMAX:
		for (jma = 0; jma < mnum; jma++)
                  {
		    ii = i + mask->i[jma];
		    jj = j + mask->j[jma];
		    kk = k + mask->k[jma];
		    if (ii < 0   || jj < 0   || kk < 0 ||
			ii >= nx || jj >= ny || kk >= nz)  continue;
		    ijkma = THREE_TO_IJK (ii, jj, kk, nx, nxy);
		    mag = ffim[ijkma];
		    if (fabs(mag) > fabs(smax))  smax = mag;
                  }
		break;
	      default:  break;
	      }

            /*--- Save statistic for this voxel ---*/
            switch (filter_opt)
	      {
	      case FCFLAG_MEAN:  ffim_out[ijkvox] = sum/npts;   break;
	      case FCFLAG_NZMEAN:
		if (nznpts > 0) ffim_out[ijkvox] = sumnz/nznpts;
		else ffim_out[ijkvox] = 0.0;
		break;
	      case FCFLAG_MAX:   ffim_out[ijkvox] = max;   break;
	      case FCFLAG_AMAX:  ffim_out[ijkvox] = amax;  break;
	      case FCFLAG_SMAX:  ffim_out[ijkvox] = smax;  break;
	      default:  break;
	      }
         }  /* i */
      }  /* j */
   }  /* k */

   /*--- Return the filtered data in the original data type. ---*/
   EDIT_coerce_autoscale(nxyz, MRI_float, ffim_out, fim_type, vfim);

   /*--- clean up ---*/
   KILL_CLUSTER (mask);
   free (ffim);
   free (ffim_out);

   return;
}

/*------------------------------------------------------------------------
   07 Jan 1998:  A more efficient way to take a local average.
                 Creates a larger volume, copies data in, and then
                 can average without worrying about edge overrun.
--------------------------------------------------------------------------*/

void EDIT_aver_fvol( int   nx, int   ny, int   nz,
                     float dx, float dy, float dz, float * fim , float rmm )
{
   MCW_cluster * mask ;
   int i, j, k , ij , ii ;
   int jk,jkadd , nxadd,nyadd,nzadd , nxyz_add , mnum ; 
   float * ffim ;
   int * madd ;
   float fac , sum ;

   /*--- Make a cluster that is a mask of points closer than rmm ---*/

   mask = MCW_build_mask(nx,ny,nz, dx,dy,dz, rmm) ;
   if( mask == NULL || mask->num_pt < 2 ){
      fprintf(stderr,"Warning: EDIT_aver_volume has no effect.\n") ;
      return ;
   }
   mnum = mask->num_pt ;

   /*--- Allocate workspaces ---*/

#if 1
   nxadd = nyadd = nzadd = 1 ;
   for( ii=0 ; ii < mnum ; ii++ ){
      i = abs((int)mask->i[ii]) ; nxadd = MAX(i,nxadd) ;
      j = abs((int)mask->j[ii]) ; nyadd = MAX(j,nyadd) ;
      k = abs((int)mask->k[ii]) ; nzadd = MAX(k,nzadd) ;
   }
#else
   nxadd    = (int)(rmm/dx) ;
   nyadd    = (int)(rmm/dy) ;
   nzadd    = (int)(rmm/dz) ;
#endif

   nxyz_add = (nx+2*nxadd) * (ny+2*nyadd) * (nz+2*nzadd) ;

   ffim = (float *) malloc(sizeof(float) * nxyz_add) ;
   if(ffim == NULL){
      fprintf(stderr,"*** EDIT_aver_volume can't malloc workspace!\n") ;
      fprintf(stderr,"nx=%d ny=%d nz=%d nxadd=%d nyadd=%d nzadd=%d\n",
              nx,ny,nz , nxadd,nyadd,nzadd ) ;
      exit(1) ;
   }
   for( i=0 ; i < nxyz_add ; i++ ) ffim[i] = 0.0 ;

   madd = (int *) malloc( sizeof(int) * (mnum+1) ) ;
   if( madd == NULL ){
      fprintf(stderr,"*** EDIT_aver_volume can't malloc workspace!\n") ;
      exit(1) ;
   }
   madd[0] = 0 ;
   for( ii=0 ; ii < mnum ; ii++ ){
      madd[ii+1] = mask->i[ii] +
                   mask->j[ii] * (nx+2*nxadd) +
                   mask->k[ii] * ((nx+2*nxadd)*(ny+2*nyadd)) ;
   }
   mnum++ ; fac = 1.0 / mnum ;

   KILL_CLUSTER(mask) ;

   /*-- copy data into workspace --*/

   for( k=0 ; k < nz ; k++ ){
      for( j=0 ; j < ny ; j++ ){
         jkadd = j * (nx+2*nxadd) + k * ((nx+2*nxadd)*(ny+2*nyadd)) ;
         jk    = j * nx + k * (nx * ny) ;
         for( i=0 ; i < nx ; i++ ) ffim[i+jkadd] = fim[i+jk] ;
      }
   }

   /*-- average data from workspace back into original array --*/

   for( k=0 ; k < nz ; k++ ){
      for( j=0 ; j < ny ; j++ ){
         jkadd = j * (nx+2*nxadd) + k * ((nx+2*nxadd)*(ny+2*nyadd)) ;
         jk    = j * nx + k * (nx * ny) ;
         for( i=0 ; i < nx ; i++ ){
            sum = 0.0 ; ij = i+jkadd ;
            for( ii=0 ; ii < mnum ; ii++ ) sum += ffim[ij+madd[ii]] ;
            fim[i+jk] = fac * sum ;
         }
      }
   }

   free(ffim); free(madd);
   return;
}
