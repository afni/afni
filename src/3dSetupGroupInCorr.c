#include "mrilib.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset **inset ; int ndset,ids ;
   char *prefix      = "Group"           ;
   char *suffix_head = ".grpincorr.niml" ;
   char *suffix_data = ".grpincorr.data" ;
   char *hfname , *dfname ;
   char atrib[2048] , *buf , *gstr ;
   byte *mask=NULL ;
   int mask_nx,mask_ny,mask_nz,nmask , nx,ny,nz,nvox ;
   NI_element *nel ;
   int nvec , *nvals , *ivec=NULL , iv,kk,nvv , nopt , do_delete=0 ;
   float *fac , *fv , val,top ;
   NI_float_array *facar ; NI_int_array *nvar ;
   short *sv ; MRI_vectim *mv ; FILE *fp ; long long fsize ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dSetupGroupInCorr [options] dataset dataset ...\n"
       "\n"
       "This program is used to pre-process a collection of AFNI\n"
       "3D+time datasets for use with Group InstaCorr (3dGroupInCorr).\n"
       "\n"
       "* All the datasets input here will be treated as one sample\n"
       "  for the t-test performed in 3dGroupInCorr.  If you are going\n"
       "  to do a 2-sample t-test, then you will need to run this\n"
       "  program twice, once for each collection of datasets\n"
       "  (e.g., once for 'control subjects' and once for 'patients').\n"
       "\n"
       "* All datasets must have the same grid layout, since 3dGroupInCorr\n"
       "  will do voxel-by-voxel comparisons.  Usually, this means that\n"
       "  the datasets have been transformed to a standard space; for\n"
       "  example, using the @auto_tlrc script.\n"
       "\n"
       "* All the datasets use the same mask -- only voxels inside\n"
       "  this mask will be stored and processed.  If you do not give the\n"
       "  '-mask' option, then all voxels will be processed -- not usually\n"
       "  a good idea, since non-brain voxels will use up a LOT of memory\n"
       "  and CPU time in 3dGroupInCorr.\n"
       "   + If you use '-mask', you MUST use the same mask dataset\n"
       "     in all runs of 3dSetupGroupInCorr that will be input\n"
       "     at the same time to 3dGroupInCorr -- otherwise, the\n"
       "     computations in that program will make no sense AT ALL!\n"
       "   + This requirement is why there is no '-automask' option.\n"
       "\n"
       "* However, the datasets do NOT all have to have the same number\n"
       "  of time points or time spacing.  But each dataset must have\n"
       "  at least 9 points along the time axis!\n"
       "\n"
       "* The only pre-processing herein for each time series is to L2\n"
       "  normalize it (sum of squares = 1) and scale it to 16 bit shorts.\n"
       "   + You almost certainly want to use 3dBandpass or some other\n"
       "     code to pre-process the datasets BEFORE input to this program.\n"
       "\n"
       "* The outputs from this program are 2 files:\n"
       "   + PREFIX.grpincorr.niml is a text file containing the header\n"
       "     information that describes the data file.  This file is input\n"
       "     to 3dGroupInCorr to define one sample in the t-test.\n"
       "   + PREFIX.grpincorr.data is the data file, which contains\n"
       "     all the time series (in the mask) from all the datasets.\n"
       "   + The data file will usually be huge (gigabytes, perhaps).\n"
       "     You need to be sure you have enough disk space.\n"
       "\n"
       "-------\n"
       "OPTIONS\n"
       "-------\n"
       "  -mask mset     = Mask dataset [highly recommended!]\n"
       "\n"
       "  -prefix PREFIX = Set prefix name of output dataset\n"
       "\n"
       "  -DELETE        = Delete input datasets from disk after\n"
       "                   processing them one at a time into the\n"
       "                   output data file -- this very highly\n"
       "                   destructive option is intended to let\n"
       "                   you save disk space, if absolutely\n"
       "                   necessary.  *** BE CAREFUL OUT THERE! ***\n"
       "                 ++ If you are setting up for 3dGroupInCorr\n"
       "                    in a script that first uses 3dBandpass\n"
       "                    to filter the datasets, then uses this\n"
       "                    program to finish the setup, then you\n"
       "                    COULD use '-DELETE' to remove the\n"
       "                    temporary 3dBandpass outputs as soon\n"
       "                    as they are no longer needed.\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   machdep() ;

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcmp(argv[nopt],"-DELETE") == 0 ){
       do_delete = 1 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need an argument after -prefix!") ;
       prefix = argv[nopt] ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("bad -prefix option!") ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-mask") == 0 ){
       THD_3dim_dataset *mset ;
       if( ++nopt >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[nopt] ) ;
       CHECK_OPEN_ERROR(mset,argv[nopt]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[nopt]) ;
       nmask = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",nmask) ;
       if( nmask < 1 ) ERROR_exit("Mask is too small to process") ;
       nopt++ ; continue ;
     }

     ERROR_exit("Unknown option: '%s'",argv[nopt]) ;
   }

   /*-- don't clobber existing files --*/

   hfname = (char *)malloc(strlen(prefix)+32) ;    /* header filename */
   dfname = (char *)malloc(strlen(prefix)+32) ;    /* data filename */
   strcpy(hfname,prefix) ; strcat(hfname,suffix_head) ;
   strcpy(dfname,prefix) ; strcat(dfname,suffix_data) ;
   if( THD_is_file(hfname) )
     ERROR_exit("Output header file '%s' already exists",hfname) ;
   if( THD_is_file(dfname) )
     ERROR_exit("Output data file '%s' already exists",dfname) ;

   /*-- read input dataset headers --*/

   ndset = argc - nopt ;
   if( ndset < 2 )
     ERROR_exit("Must have at least 2 datasets on the command line!") ;

   inset = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)*ndset) ;
   nvals = (int   *)malloc(sizeof(int)  *ndset) ; /* # time points per dataset */
   fac   = (float *)malloc(sizeof(float)*ndset) ; /* scale factor per dataset */

   for( ids=0 ; ids < ndset ; ids++ ){
     inset[ids] = THD_open_dataset(argv[nopt+ids]) ;    /* read header */
     CHECK_OPEN_ERROR(inset[ids],argv[nopt+ids]) ;      /* fail ==> bail */
     nvals[ids] = DSET_NVALS(inset[ids]) ;              /* save # time points */
     if( ids > 0 && !EQUIV_GRIDS(inset[0],inset[ids]) ) /* check for errors */
       ERROR_exit("Dataset grid %s doesn't match %s" ,
                  argv[nopt+ids] , argv[nopt]         ) ;
     if( nvals[ids] < 9 )
       ERROR_exit("Dataset %s has only %d time points!?" ,
                  argv[nopt+ids] , nvals[ids] ) ;
   }

   nx = DSET_NX(inset[0]) ;
   ny = DSET_NY(inset[0]) ;
   nz = DSET_NZ(inset[0]) ; nvox = nx*ny*nz ;
   if( nvox < 2 ) ERROR_exit("Only 1 voxel in datasets?!") ;

   /*-- check or create mask --*/

   if( mask != NULL ){
     if( mask_nx != nx || mask_ny != ny || mask_nz != nz )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else {
     mask = (byte *)malloc(sizeof(byte)*nvox) ; nmask = nvox ;
     memset(mask,1,sizeof(byte)*nvox) ;
     INFO_message("No mask ==> processing all %d voxels",nvox);
   }

   /* load index vector (if needed) */

   nvec = nmask ;
   if( nvec < nvox ){
     ivec = (int *)calloc(sizeof(int),nmask) ;
     for( iv=kk=0 ; kk < nvox ; kk++ ) if( mask[kk] ) ivec[iv++] = kk ;
   }

   /* open output data file */

   fp = fopen( dfname , "w" ) ;
   if( fp == NULL ) ERROR_exit("Can't open file '%s' for output",dfname) ;

   /*--- loop over datasets, convert to shorts, write out ---*/

   gstr = strdup( EDIT_get_geometry_string(inset[0]) ) ;

   for( ids=0 ; ids < ndset ; ids++ ){
     INFO_message("GroupInCorr-izing dataset %s",DSET_BRIKNAME(inset[ids])) ;

     /* extract all time series in the mask, as floats */

     mv = THD_dset_to_vectim( inset[ids] , mask , 0 ) ;
     if( mv == NULL ){
       fclose(fp) ; remove(dfname) ; ERROR_exit("Can't load dataset!?") ;
     }

     /* save memory by removing this input dataset */
 
     THD_delete_3dim_dataset( inset[ids] , (Boolean)do_delete ) ;
     inset[ids] = NULL ;

     THD_vectim_normalize( mv ) ; /* L2 normalize each time series */

     /* find largest absolute value over all vectors */

     nvv = mv->nvec * mv->nvals ; top = 0.0f ; fv = mv->fvec ;
     for( kk=0 ; kk < nvv ; kk++ ){
       val = fabsf(fv[kk]) ; if( val > top ) top = val ;
     }

     if( top == 0.0f ){
       fclose(fp) ; remove(dfname) ; ERROR_exit("Dataset is all zero?!") ;
     }

     /* scale to 16-bit shorts to save disk and memory space */

     top = 32766.0f / top ; fac[ids] = 1.0f / top ;  /* save scale factor */
     sv = (short *)malloc(sizeof(short)*nvv) ;       /* output array */
     for( kk=0 ; kk < nvv ; kk++ ) sv[kk] = (short)rintf(top*fv[kk]) ;
     VECTIM_destroy(mv) ;

     /* write output array */

     kk = fwrite( sv , sizeof(short) , nvv , fp ) ;
     if( kk < nvv ){
       fclose(fp) ; remove(dfname) ;
       ERROR_exit("Write to '%s' failed -- disk full? permission?",dfname) ;
     }
     free(sv) ;  /* toss this trash */

   } /* end of dataset loop */

   /*--- close data file, print some helpful info ---*/

   fclose(fp) ;
   fsize = THD_filesize(dfname) ;
   INFO_message("Wrote data file %s = %lld bytes (about %s)",
                dfname , fsize , approximate_number_string((double)fsize) ) ;

   /*--- create NIML data element to describe what's in the data file ---*/

   /* if have a mask, then must write data indicating which voxels are in use */
   /* if no mask, then don't write that data, since we can generate as needed */

   if( ivec != NULL ){
     nel = NI_new_data_element( "3dGroupInCorr" , nvec ) ;
     NI_add_column( nel , NI_INT , ivec ) ;
   } else {
     nel = NI_new_data_element( "3dGroupInCorr" , 0 ) ;
   }

   sprintf(atrib,"%d",nvec) ;
   NI_set_attribute( nel , "nvec" , atrib ) ;       /* # time series vectors */

   sprintf(atrib,"%d",ndset) ;
   NI_set_attribute( nel , "ndset" , atrib ) ;                 /* # datasets */

   nvar = (NI_int_array *)malloc(sizeof(NI_int_array)) ;
   nvar->num = ndset ; nvar->ar = nvals ;
   buf = NI_encode_int_list( nvar , "," ) ;
   NI_set_attribute( nel , "nvals" , buf ) ;            /* # TRs per dataset */

   facar = (NI_float_array *)malloc(sizeof(NI_float_array)) ;
   facar->num = ndset ; facar->ar = fac ;
   buf = NI_encode_float_list( facar , "," ) ;
   NI_set_attribute( nel , "fac" , buf ) ;       /* scale factor per dataset */

                                                     /* describe 3D geometry */
   NI_set_attribute( nel , "geometry" , gstr ) ;      /* of what we're about */

   NI_set_attribute( nel , "datafile" , dfname ) ;         /* data file name */

   /*--- write header file ---*/

   kk = NI_write_element_tofile( hfname , nel , NI_TEXT_MODE ) ;
   if( kk < 0 ){     /* should probably never happen */
     remove(dfname) ;
     ERROR_exit("Failed to write head file %s; deleted data file %s" ,
                hfname , dfname ) ;
   }

   INFO_message("Wrote head file %s",hfname) ;
   exit(0) ;
}
