#include "mrilib.h"

/***** This file is intended to be #include-d into   *****/
/***** places such as 3dClustSimX.c and 3dClustSim.c *****/

/***** Also see program 3dtoXdataset.c for creating such files *****/

/*---------------------------------------------------------------------------*/
/* Stuff for reading z-statistic dataset stored as shorts inside a mask
   -- Basically, a compressed file, since 10000+
      realizations is pretty damn big when stored as 3D float bricks!
   -- And reading them in that way is damn slow, too.
*//*-------------------------------------------------------------------------*/

#define SFAC 0.0002f  /* scale factor to convert shorts to floats */
                      /* 0.0002 * 32767 = 6.5534 = a pretty big z */

/* struct that combines the 3D mask dataset and the arrays of shorts */

typedef struct {
  THD_3dim_dataset  *mask_dset ;                        /* mask dataset */
  byte              *mask_vol ;                          /* mask volume */
  int nvox, ngood ;
  ind_t *ipmask, *jpmask, *kpmask ;             /* indexes in 3D and 1D */
  int   *ijkmask ;                               /* for pts in the mask */
  int   *ijk_to_vec ;    /* map from index in volume to pts in the mask */

  int    nsdat ;                               /* number of input files */
  int    *nvol , nvtot ;  /* length of each file (in volumes) and total */
  short **sdat ;            /* pointer to data from each file (mmap-ed) */
} Xdataset ;

/*----------------------------------------------------------*/
/* create Xdataset struct from 2 files: mask and short data */

Xdataset * open_Xdataset( char *mask_fname, int nsdat, char **sdat_fname )
{
   Xdataset *xds ; int ids,fdes,nvtot,pp,qq ; int64_t fsiz ;

   if( mask_fname == NULL || sdat_fname == NULL )
     ERROR_exit("bad inputs to open_Xdataset") ;

   xds = (Xdataset *)calloc(sizeof(Xdataset),1) ;

   /*--- create the mask ---*/

   xds->mask_dset = THD_open_dataset(mask_fname) ;
   if( xds->mask_dset == NULL )
     ERROR_exit("can't open mask dataset '%s'",mask_fname) ;
   DSET_load(xds->mask_dset) ; CHECK_LOAD_ERROR(xds->mask_dset) ;

   xds->mask_vol = THD_makemask( xds->mask_dset , 0 , 1.0,0.0 ) ;
   if( xds->mask_vol == NULL )
     ERROR_exit("can't use -mask dataset '%s'",mask_fname) ;
   DSET_unload(xds->mask_dset) ;

   xds->nvox  = DSET_NVOX(xds->mask_dset) ;
   xds->ngood = THD_countmask( xds->nvox , xds->mask_vol ) ;

   /* check mask for finger licking goodness */

   if( xds->ngood < 2 )
     ERROR_exit("mask has only %d good voxels -- cannot continue",xds->ngood) ;

   xds->ijkmask = (int *)malloc(sizeof(int)*xds->nvox) ;
   for( pp=qq=0 ; qq < xds->nvox ; qq++ ){
     if( xds->mask_vol[qq] ) xds->ijkmask[pp++] = qq ;
   }

   /*--- open data files with the short-ized and mask-ized data ---*/

   xds->nsdat = nsdat ;
   xds->nvol  = (int *)calloc(sizeof(int),nsdat) ;
   xds->sdat  = (short **)calloc(sizeof(short *),nsdat) ;
   nvtot = 0 ;

   for( ids=0 ; ids < nsdat ; ids++ ){

     if( sdat_fname[ids] == NULL ) ERROR_exit("NULL .sdat filename :-(") ;

     fsiz = (int64_t)THD_filesize(sdat_fname[ids]) ;  /* in bytes */
     if( fsiz == 0 )
       ERROR_exit("can't find any data in file '%s'",sdat_fname[ids]) ;

     xds->nvol[ids] = (int)( fsiz /(sizeof(short)*xds->ngood) ); /* num volumes */
     if( xds->nvol[ids] == 0 )                                   /* in file */
       ERROR_exit("data file '%s' isn't long enough",sdat_fname[ids]) ;
     nvtot += xds->nvol[ids] ;

     fdes = open( sdat_fname[ids] , O_RDONLY ) ; /* open, get file descriptor */
     if( fdes < 0 )
       ERROR_exit("can't open data file '%s'",sdat_fname[ids]) ;

     /* memory map the data file */

     xds->sdat[ids] = (short *)mmap( 0, (size_t)fsiz, PROT_READ, THD_MMAP_FLAG, fdes, 0 ) ;
     close(fdes) ;
     if( xds->sdat[ids] == (short *)(-1) )
       ERROR_exit("can't mmap() data file '%s' -- memory space exhausted?",
                  sdat_fname[ids]) ;

     /* page fault the data into memory */

#if 0
     if( verb )
       INFO_message("mapping %s into memory",sdat_fname[ids]) ;
#endif

#if 0
     { int64_t ii,sum=0 ; char *bdat = (char *)xds->sdat[ids] ;
       for( ii=0 ; ii < fsiz ; ii+=1024 ) sum += (int64_t)bdat[ii] ;
       if( sin((double)sum)==666.0 )
         INFO_message("sum=%g",(double)sum) ; /* never executed */
     }
#endif
   } /* end of loop over input .sdat files */

   xds->nvtot = nvtot ;

   /* e finito */

   return xds ;
}

/*---------------------------------------------------------------------------*/
/* load a 3D array from the masked file of shorts */

void load_from_Xdataset( Xdataset *xds , int ival , float *far )
{
   int ii,jj,qval,qq ; short *spt ;

   if( ival < 0 || ival >= xds->nvtot )
     ERROR_exit("load_from_Xdataset: ival=%d nvol=%d",ival,xds->nvtot) ;

   AAmemset( far , 0 , sizeof(float)*xds->nvox ) ;

   for( qval=ival,qq=0 ; qq < xds->nsdat ; qq++ ){ /* find which to use */
     if( qval < xds->nvol[qq] ) break ;            /* got it! */
     qval -= xds->nvol[qq] ;                       /* try next one */
   }
   if( qq == xds->nsdat )                     /* should not be possible */
     ERROR_exit("load_from_Xdataset[%d] == array overflow :-(",ival) ;

   spt = xds->sdat[qq] + ((size_t)qval)*((size_t)xds->ngood) ;

   for( ii=0 ; ii < xds->ngood ; ii++ ){
     jj = xds->ijkmask[ii] ;    /* if put this directly in the far[] */
     far[jj] = SFAC * spt[ii] ; /* subscript, optimizer problems in icc */
   }
   return ;
}

#undef SFAC
