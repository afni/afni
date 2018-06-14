/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "parser.h"

/*-----------------------------------------------------------------------------
   Routine to filter volume data.

      nx, ny, nz   = number of voxels along each axis
      dx, dy, dz   = voxel dimensions
      fim_type     = volume data type
      vfim         = volume data itself
      filter_opt   = indicates filtering method to be applied
      filter_rmm   = radius of "influence" of neighboring voxels

      fmask        = if non-NULL, is a mask of allowable voxels
      fmclip       = if nonzero, zero out non-fmask voxels at end
      fexpr        = character string for FCFLAG_EXPR

   The filtered data is returned in vfim.

   Author :  B. D. Ward
   Date   :  11 September 1996
   Mod    :  2 October 1996        Changed memory deallocation.
   Mod    :  9 October 1996        Made changes to improve speed.

   To correct error due to abiguity in identification of clusters,
   voxel coordinates are now stored as 3 separate short integers.
   BDW  06 March 1997

   Added fmask and fexpr: RWCox - 09 Aug 2000
   Added Winsor filter:   RWCox - 11 Sep 2000
   Added fmclip:          RWCox - 11 Oct 2007
-----------------------------------------------------------------------------*/

void EDIT_filter_volume (int nx, int ny, int nz, float dx, float dy, float dz,
                   int fim_type, void *vfim, int filter_opt, float filter_rmm,
                   byte *fmask , int fmclip , char *fexpr )
{
   MCW_cluster *mask;                   /* mask for filtering */
   int nxy, nxyz;                       /* dimensions of volume data */
   int mnum;                            /* number of points in mask */
   int i, j, k, ii, jj, kk;             /* voxel indices */
   int ijkvox, ijkma, jma;              /* more voxel indices */
   float *ffim, *ffim_out;              /* floating point fim's */

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

   float wtsum ;           /* 09 Aug 2000: stuff for FCFLAG_EXPR */
   float *wt=NULL ;
   PARSER_code *pcode ;

   int nw=0 , nnw=0 , iw ;     /* 11 Sep 2000: Winsor stuff */
   float *sw=NULL , vw=0.0 ;

   nxy = nx*ny;  nxyz = nxy*nz;

#define GOODVOX(ijk) (fmask==NULL || fmask[ijk]!=0)
#define BADVOX(ijk)  (fmask!=NULL && fmask[ijk]==0)

ENTRY("EDIT_filter_volume") ;

   /* 09 Aug 2000: can't use AVER code if mask is in place */

   if( fmask != NULL && filter_opt == FCFLAG_AVER ) filter_opt = FCFLAG_MEAN ;

   /***--- 07 Jan 1998 ---***/

   if( filter_opt == FCFLAG_AVER ){  /* more efficient code */
      if( fim_type != MRI_float ){
         ffim = (float *) malloc (sizeof(float) * nxyz);
         if( ffim == NULL ){
            fprintf(stderr,"EDIT_filter_volume: no workspace!\n") ;
            EXIT(1) ;
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
      EXRETURN ;
   }

   /***--- end 07 Jan 1998 ---***/

   /*--- Make a cluster that is a mask of points closer than filter_rmm ---*/

   mask = MCW_build_mask (dx, dy, dz, filter_rmm);
   if (mask == NULL)
   {
      fprintf (stderr, "Warning: Filter option has no effect. \n");
      EXRETURN;
   }
   mnum = mask->num_pt;

   /* 09 Aug 2000: evaluate expression weights into wt */

   if( filter_opt == FCFLAG_EXPR ){
      double atoz[26] ;

      if( fexpr == NULL ){
         fprintf(stderr,"*** EDIT_filter_volume: no fexpr for FCFLAG_EXPR!\n");
         EXIT(1) ;
      }

      pcode = PARSER_generate_code( fexpr ) ;
      if( pcode == NULL ){
         fprintf(stderr,"*** EDIT_filter_volume: illegal fexpr!\n"); EXIT(1);
      }

      wt = (float *) malloc(sizeof(float)*(mnum+1)) ;

#undef  II
#undef  II
#undef  KK
#undef  RR
#undef  XX
#undef  YY
#undef  ZZ
#define II  8  /* a=0 b=1 ... i=8 ... z=25 */
#define JJ  9
#define KK 10
#define RR 17
#define XX 23
#define YY 24
#define ZZ 25

      for( ii=0 ; ii < 26 ; ii++ ) atoz[ii] = 0.0 ;

      wt[0] = PARSER_evaluate_one( pcode , atoz ) ;  /* weight at center */

      for (jma = 0; jma < mnum; jma++){              /* rest of weights */
         atoz[II] = mask->i[jma] ;
         atoz[JJ] = mask->j[jma] ;
         atoz[KK] = mask->k[jma] ;
         atoz[XX] = atoz[II] * dx ;
         atoz[YY] = atoz[JJ] * dy ;
         atoz[ZZ] = atoz[KK] * dz ;
         atoz[RR] = sqrt(atoz[XX]*atoz[XX] + atoz[YY]*atoz[YY] + atoz[ZZ]*atoz[ZZ]) ;
         wt[jma+1] = PARSER_evaluate_one( pcode , atoz ) ;
      }
      free(pcode) ;
   }

   /* 11 Sep 2000: setup for Winsorizing */

   if( filter_opt > FCFLAG_WINSOR                 &&
       filter_opt < FCFLAG_WINSOR+FCFLAG_ONE_STEP   ){

      static int first=1 ;

      nw  = filter_opt - FCFLAG_WINSOR ;
      nnw = mnum - nw ;
      filter_opt = FCFLAG_WINSOR ;

      fprintf(stderr,"++ Winsor filter: N=%d nw=%d\n",mnum+1,nw) ;
      if( first || nnw < nw ){
        first = 0 ;
        if( nnw < nw ){
          fprintf(stderr,"** Illegal Winsor parameters - skipping!\n") ;
          EXRETURN ;
        }
      }

      sw = (float *) malloc(sizeof(float)*(mnum+1)) ;

      fmask = NULL ;  /* must disable mask for Winsor */
   }

   /*--- Allocate space for floating point data ---*/

   ffim = (float *) malloc (sizeof(float) * nxyz);
   if (ffim == NULL)
   {
      fprintf (stderr, "\n Error: cannot allocate filter workspace! \n");
      EXIT(1);
   }
   ffim_out = (float *) malloc (sizeof(float) * nxyz);
   if (ffim_out == NULL)
   {
      fprintf (stderr, "\n Error: cannot allocate filter workspace! \n");
      EXIT(1);
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
            npts = nznpts = 0 ;
            sum = sumnz = max = amax = smax = wtsum = 0.0 ;
            if( GOODVOX(ijkvox) ){
              mag = ffim[ijkvox];
              switch (filter_opt)
              {
                 case FCFLAG_MEAN:
                    sum = mag;  npts = 1;  break;
                 case FCFLAG_NZMEAN:
                    if (mag != 0.0)
                       {sumnz = mag;   nznpts = 1;}
                    break;
                 case FCFLAG_MAX:
                    max = mag; npts = 1 ;  break;
                 case FCFLAG_AMAX:
                    amax = fabs(mag); npts = 1 ;  break;
                 case FCFLAG_SMAX:
                    smax = mag; npts = 1 ;  break;
                 case FCFLAG_EXPR:
                    sum = wt[0]*mag ; wtsum = wt[0] ; npts = 1 ; break ;

                 case FCFLAG_WINSOR:
                    for( iw=0 ; iw <= mnum ; iw++ ) sw[iw] = mag ;
                    vw = mag ; break ;
              }
            } else if( fmclip ){   /* 11 Oct 2007 */
               ffim_out[ijkvox] = 0.0f ; continue ;
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
                    if( BADVOX(ijkma) ) continue ;
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
                    if( BADVOX(ijkma) ) continue ;
		    mag = ffim[ijkma];
		    if (mag != 0.0)  {sumnz += mag;  nznpts++;}
                  }
		break;
              case FCFLAG_EXPR:
		for (jma = 0; jma < mnum; jma++)
		  {
		    ii = i + mask->i[jma];
		    jj = j + mask->j[jma];
		    kk = k + mask->k[jma];
		    if (ii < 0   || jj < 0   || kk < 0 ||
			ii >= nx || jj >= ny || kk >= nz)  continue;
		    ijkma = THREE_TO_IJK (ii, jj, kk, nx, nxy);
                    if( BADVOX(ijkma) ) continue ;
		    mag = ffim[ijkma];
		    sum += wt[jma+1]*mag;  npts++; wtsum += wt[jma+1] ;
                  }
              break ;
	      case FCFLAG_MAX:
		for (jma = 0; jma < mnum; jma++)
                  {
		    ii = i + mask->i[jma];
		    jj = j + mask->j[jma];
		    kk = k + mask->k[jma];
		    if (ii < 0   || jj < 0   || kk < 0 ||
			ii >= nx || jj >= ny || kk >= nz)  continue;
		    ijkma = THREE_TO_IJK (ii, jj, kk, nx, nxy);
                    if( BADVOX(ijkma) ) continue ;
		    mag = ffim[ijkma];
		    if (npts == 0 || mag > max)  max = mag;
                    npts++ ;
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
                    if( BADVOX(ijkma) ) continue ;
		    mag = ffim[ijkma];
		    if (npts == 0 || fabs(mag) > amax)  amax = fabs(mag);
                    npts++ ;
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
                    if( BADVOX(ijkma) ) continue ;
		    mag = ffim[ijkma];
		    if (npts == 0 || fabs(mag) > fabs(smax))  smax = mag;
                    npts++ ;
                  }
		break;
              case FCFLAG_WINSOR:
		for (jma = 0; jma < mnum; jma++)
		  {
		    ii = i + mask->i[jma];
		    jj = j + mask->j[jma];
		    kk = k + mask->k[jma];
		    if (ii < 0   || jj < 0   || kk < 0 ||
			ii >= nx || jj >= ny || kk >= nz)  continue;
		    ijkma = THREE_TO_IJK (ii, jj, kk, nx, nxy);
		    sw[jma+1] = ffim[ijkma];
                  }
                  break;

	      default:  break;
	      }

            /*--- Save statistic for this voxel ---*/

            switch (filter_opt)
	      {
	      case FCFLAG_MEAN:
                ffim_out[ijkvox] = (npts > 0)     ? sum/npts     : 0.0 ; break;

	      case FCFLAG_NZMEAN:
                ffim_out[ijkvox] = (nznpts > 0)   ? sumnz/nznpts : 0.0 ; break ;

              case FCFLAG_EXPR:
                ffim_out[ijkvox] = (wtsum != 0.0) ? sum/wtsum    : 0.0 ; break ;

	      case FCFLAG_MAX:  ffim_out[ijkvox] = max ;  break;
	      case FCFLAG_AMAX: ffim_out[ijkvox] = amax;  break;
	      case FCFLAG_SMAX: ffim_out[ijkvox] = smax;  break;

              case FCFLAG_WINSOR:
                 qsort_float( mnum+1 , sw ) ;
                 ffim_out[ijkvox] = (vw < sw[nw]) ? sw[nw]
                                                  : (vw > sw[nnw]) ? sw[nnw]
                                                                   : vw      ;
                 break ;

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
   if( wt != NULL ) free(wt) ;
   if( sw != NULL ) free(sw) ;

   EXRETURN;
}

/*------------------------------------------------------------------------
   07 Jan 1998:  A more efficient way to take a local average.
                 Creates a larger volume, copies data in, and then
                 can average without worrying about edge overrun.
--------------------------------------------------------------------------*/

void EDIT_aver_fvol( int   nx, int   ny, int   nz,
                     float dx, float dy, float dz, float *fim , float rmm )
{
   MCW_cluster *mask ;
   int i, j, k , ij , ii ;
   int jk,jkadd , nxadd,nyadd,nzadd , nxyz_add , mnum ;
   float *ffim ;
   int *madd ;
   float fac , sum ;

ENTRY("EDIT_aver_fvol") ;

   /*--- Make a cluster that is a mask of points closer than rmm ---*/

   mask = MCW_build_mask(dx,dy,dz, rmm) ;
   if( mask == NULL || mask->num_pt < 2 ){
      fprintf(stderr,"Warning: EDIT_aver_volume has no effect.\n") ;
      EXRETURN ;
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
      EXIT(1) ;
   }
   for( i=0 ; i < nxyz_add ; i++ ) ffim[i] = 0.0 ;

   madd = (int *) malloc( sizeof(int) * (mnum+1) ) ;
   if( madd == NULL ){
      fprintf(stderr,"*** EDIT_aver_volume can't malloc workspace!\n") ;
      EXIT(1) ;
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
   EXRETURN;
}
