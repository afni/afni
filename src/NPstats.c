/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
  This file contains routines common to the nonparametric statistical
  analysis programs.

  File:    NPstats.c
  Author:  B. Douglas Ward
  Date:    23 July 1997

  Mod:     Added changes for incorporating History notes.
  Date:    08 September 1999

  Mod:     Replaced dataset input code with calls to THD_open_dataset,
           to allow operator selection of individual sub-bricks for input.
  Date:    03 December 1999

  Mod:     Moved routines for sorting numbers and determining ranks to
           separate file ranks.c.
  Date:    31 March 2000

  Mod:     Modified routines write_afni_fizt and write_afni_fict so that 
           all output subbricks will now have the scaled short integer format.
  Date:    14 March 2002
*/


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void NP_error (char * message)
{
   fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
   exit(1);
}


/*---------------------------------------------------------------------------*/

/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
if((ptr)==NULL) \
( NP_error ("Cannot allocate memory") )
     

/*---------------------------------------------------------------------------*/
/*
  Include routines for sorting numbers and determining ranks.
*/

#include "ranks.c"


/*---------------------------------------------------------------------------*/
/*
   Get the dimensions of the 3d AFNI data sets.
*/

void get_dimensions (NP_options * option_data)
{
  
   THD_3dim_dataset * dset=NULL;

   /*----- read first dataset to get dimensions, etc. -----*/

   dset = THD_open_dataset( option_data->first_dataset ) ;
   if( ! ISVALID_3DIM_DATASET(dset) ){
      fprintf(stderr,"*** Unable to open dataset file %s\n", 
              option_data->first_dataset);
      exit(1) ;
   }

   /*----- data set dimensions in voxels -----*/
   option_data->nx = dset->daxes->nxx ;
   option_data->ny = dset->daxes->nyy ;
   option_data->nz = dset->daxes->nzz ;       
   option_data->nxyz = option_data->nx * option_data->ny * option_data->nz ;

   THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;

}


/*---------------------------------------------------------------------------*/
/*
  Check whether one output file already exists.
*/

void check_one_output_file (NP_options * option_data, char * filename)
{
  THD_3dim_dataset * dset=NULL;       /* input afni data set pointer */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int ierror;                         /* number of errors in editing data */
  
  
  /*----- read first dataset -----*/
  dset = THD_open_dataset (option_data->first_dataset ) ;
  if( ! ISVALID_3DIM_DATASET(dset) ){
    fprintf(stderr,"*** Unable to open dataset file %s\n",
	    option_data->first_dataset);
    exit(1) ;
  }
  
  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy( dset ) ;
  
  
  ierror = EDIT_dset_items( new_dset ,
			    ADN_prefix , filename ,
			    ADN_label1 , filename ,
			    ADN_directory_name , option_data->session ,
			    ADN_self_name , filename ,
			    ADN_type , ISHEAD(dset) ? HEAD_FUNC_TYPE : 
                               			      GEN_FUNC_TYPE ,
			    ADN_none ) ;
  
  if( ierror > 0 ){
    fprintf(stderr,
	    "*** %d errors in attempting to create output dataset!\n", ierror ) ;
    exit(1) ;
  }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
    fprintf(stderr,
	    "*** Output dataset file %s already exists--cannot continue!\a\n",
	    new_dset->dblk->diskptr->header_name ) ;
    exit(1) ;
  }
  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/

/** macro to open a dataset and make it ready for processing **/

#define DOPEN(ds,name)                                                        \
do{ int pv ; (ds) = THD_open_dataset((name)) ;                                \
       if( !ISVALID_3DIM_DATASET((ds)) ){                                     \
          fprintf(stderr,"*** Can't open dataset: %s\n",(name)) ; exit(1) ; } \
       if( (ds)->daxes->nxx!=nx || (ds)->daxes->nyy!=ny ||                    \
	   (ds)->daxes->nzz!=nz ){                                            \
          fprintf(stderr,"*** Axes mismatch: %s\n",(name)) ; exit(1) ; }      \
       if( DSET_NVALS((ds)) != 1 ){                                           \
         fprintf(stderr,"*** Must specify 1 sub-brick: %s\n",(name));exit(1);}\
       THD_load_datablock( (ds)->dblk ) ;                                     \
       pv = DSET_PRINCIPAL_VALUE((ds)) ;                                      \
       if( DSET_ARRAY((ds),pv) == NULL ){                                     \
          fprintf(stderr,"*** Can't access data: %s\n",(name)) ; exit(1); }   \
       if( DSET_BRICK_TYPE((ds),pv) == MRI_complex ){                         \
          fprintf(stderr,"*** Can't use complex data: %s\n",(name)) ; exit(1);\
       }                                                                      \
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
  Read one AFNI data set from file 'filename'. 
  The data is converted to floating point (in ffim).
*/

void read_afni_data (NP_options * option_data, char * filename, 
		     int piece_len, int fim_offset,  float * ffim)
{
  int iv;                          /* index number of intensity sub-brick */
  THD_3dim_dataset * dset=NULL;    /* data set pointer */
  void * vfim = NULL;              /* image data pointer */
  int nx, ny, nz, nxyz;            /* data set dimensions in voxels */
  
  nx = option_data->nx;
  ny = option_data->ny;
  nz = option_data->nz;
  nxyz = option_data->nxyz;
  
    
  /*----- read in the data -----*/
  DOPEN (dset, filename) ;
  iv = DSET_PRINCIPAL_VALUE(dset) ;
  
  /*----- convert it to floats (in ffim) -----*/
  SUB_POINTER (dset, iv, fim_offset, vfim) ;
  EDIT_coerce_scale_type (piece_len, DSET_BRICK_FACTOR(dset,iv),
			  DSET_BRICK_TYPE(dset,iv), vfim,      /* input  */
			  MRI_float               ,ffim  ) ;   /* output */
  
  THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
}

/*---------------------------------------------------------------------------*/
/*
  Routine to write one AFNI fizt data set.
  
  This data set is 'fizt' type (intensity + Standard Normal statistic)
  
  The intensity data is in ffim, and the corresponding statistic is in ftr.
  
*/

void write_afni_fizt (int argc, char ** argv, NP_options * option_data,  
		      char * filename, float * ffim,  float * ftr)
{
  int nxyz;                           /* number of voxels */
  int ii;                             /* voxel index */
  THD_3dim_dataset * dset=NULL;       /* input afni data set pointer */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int ierror;                         /* number of errors in editing data */
  int ibuf[32];                       /* integer buffer */
  float fbuf[MAX_STAT_AUX];           /* float buffer */
  float fimfac;                       /* scale factor for short data */
  int output_datum;                   /* data type for output data */
  short * tsp;                        /* 2nd sub-brick data pointer */
  void  * vdif;                       /* 1st sub-brick data pointer */
  int func_type;                      /* afni data set type */
  float top, func_scale_short;        /* parameters for scaling data */
  
  
  /*----- initialize local variables -----*/
  nxyz = option_data->nxyz;
  
  /*----- read first dataset -----*/
  dset = THD_open_dataset (option_data->first_dataset) ;
  if( ! ISVALID_3DIM_DATASET(dset) ){
    fprintf(stderr,"*** Unable to open dataset file %s\n",
	    option_data->first_dataset);
    exit(1) ;
  }
  
  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy( dset ) ;
  
  
  output_datum = MRI_short ;
  ibuf[0] = output_datum ; ibuf[1] = MRI_short ;

  
  func_type = FUNC_ZT_TYPE;
  
  ierror = EDIT_dset_items( new_dset ,
			    ADN_prefix , filename ,
			    ADN_label1 , filename ,
			    ADN_directory_name , option_data->session ,
			    ADN_self_name , filename ,
			    ADN_type , ISHEAD(dset) ? HEAD_FUNC_TYPE : 
			                              GEN_FUNC_TYPE ,
			    ADN_func_type , func_type ,
			    ADN_nvals , FUNC_nvals[func_type] ,
			    ADN_datum_array , ibuf ,
			    ADN_malloc_type, DATABLOCK_MEM_MALLOC ,  
			    ADN_none ) ;
  
  if( ierror > 0 ){
    fprintf(stderr,
          "*** %d errors in attempting to create output dataset!\n", ierror ) ;
    exit(1) ;
  }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
    fprintf(stderr,
	    "*** Output dataset file %s already exists--cannot continue!\a\n",
	    new_dset->dblk->diskptr->header_name ) ;
    exit(1) ;
  }
  
  /*----- deleting exemplar dataset -----*/ 
  THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
  
  
  /*----- allocate memory for output data -----*/
  vdif = (void *)  malloc( mri_datum_size(output_datum) * nxyz ) ;
  tsp  = (short *) malloc( sizeof(short) * nxyz )                ;
  
  /*----- attach bricks to new data set -----*/
  mri_fix_data_pointer (vdif, DSET_BRICK(new_dset,0)); 
  mri_fix_data_pointer (tsp, DSET_BRICK(new_dset,1));  
  
  
  /*----- convert data type to output specification -----*/
  fimfac = EDIT_coerce_autoscale_new (nxyz, 
				      MRI_float, ffim, 
				      output_datum, vdif);
  if (fimfac != 0.0)  fimfac = 1.0 / fimfac;
  
#define TOP_SS  32700
  
  top = TOP_SS/FUNC_ZT_SCALE_SHORT;
  func_scale_short = FUNC_ZT_SCALE_SHORT;
  
  for (ii = 0;  ii < nxyz;  ii++)
    {
      if (ftr[ii] > top)
	tsp[ii] = TOP_SS;
      else  if (ftr[ii] < -top)
	tsp[ii] = -TOP_SS;
      else  if (ftr[ii] >= 0.0)
	tsp[ii] = (short) (func_scale_short * ftr[ii] + 0.5);
      else
	tsp[ii] = (short) (func_scale_short * ftr[ii] - 0.5);
    }
  

  /*----- write afni fizt data set -----*/
  printf("--- Writing 'fizt' dataset into %s\n",DSET_BRIKNAME(new_dset)) ;
  
  for( ii=0 ; ii < MAX_STAT_AUX ; ii++ ) fbuf[ii] = 0.0 ;
  (void) EDIT_dset_items( new_dset , ADN_stat_aux , fbuf , ADN_none ) ;
  
  fbuf[0] = (output_datum == MRI_short && fimfac != 1.0 ) ? fimfac : 0.0 ;
  fbuf[1] = 1.0 / func_scale_short ;
  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;


  /*----- Record history of dataset -----*/  
  tross_Make_History( PROGRAM_NAME , argc , argv , new_dset ) ;
  
  THD_load_statistics( new_dset ) ;
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to write one AFNI fict data set.
  
  This data set is 'fict' type (intensity + Chi-Squared statistic)
  
  The intensity data is in ffim, and the corresponding statistic is in ftr.
  
  dof = degrees of freedom

*/

void write_afni_fict (int argc, char ** argv, NP_options * option_data,  
		      char * filename, float * ffim,  float * ftr,  int dof)
{
  int nxyz;                           /* number of voxels */
  int ii;                             /* voxel index */
  THD_3dim_dataset * dset=NULL;       /* input afni data set pointer */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int ierror;                         /* number of errors in editing data */
  int ibuf[32];                       /* integer buffer */
  float fbuf[MAX_STAT_AUX];           /* float buffer */
  float fimfac;                       /* scale factor for short data */
  int output_datum;                   /* data type for output data */
  short * tsp;                        /* 2nd sub-brick data pointer */
  void  * vdif;                       /* 1st sub-brick data pointer */
  int func_type;                      /* afni data set type */
  float top, func_scale_short;        /* parameters for scaling data */
  
  
  /*----- initialize local variables -----*/
  nxyz = option_data->nxyz;
  
  /*----- read first dataset -----*/
  dset = THD_open_dataset (option_data->first_dataset) ;
  if( ! ISVALID_3DIM_DATASET(dset) ){
    fprintf(stderr,"*** Unable to open dataset file %s\n",
	    option_data->first_dataset);
    exit(1) ;
  }
  
  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy( dset ) ;
  
  
  output_datum = MRI_short ;
  ibuf[0] = output_datum ; ibuf[1] = MRI_short ;

  
  func_type = FUNC_CT_TYPE;
  
  ierror = EDIT_dset_items( new_dset ,
			    ADN_prefix , filename ,
			    ADN_label1 , filename ,
			    ADN_directory_name , option_data->session ,
			    ADN_self_name , filename ,
			    ADN_type , ISHEAD(dset) ? HEAD_FUNC_TYPE : 
			                              GEN_FUNC_TYPE ,
			    ADN_func_type , func_type ,
			    ADN_nvals , FUNC_nvals[func_type] ,
			    ADN_datum_array , ibuf ,
			    ADN_malloc_type, DATABLOCK_MEM_MALLOC ,  
			    ADN_none ) ;
  
  if( ierror > 0 ){
    fprintf(stderr,
          "*** %d errors in attempting to create output dataset!\n", ierror ) ;
    exit(1) ;
  }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
    fprintf(stderr,
	    "*** Output dataset file %s already exists--cannot continue!\a\n",
	    new_dset->dblk->diskptr->header_name ) ;
    exit(1) ;
  }
  
  /*----- deleting exemplar dataset -----*/ 
  THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
  
  
  /*----- allocate memory for output data -----*/
  vdif = (void *)  malloc( mri_datum_size(output_datum) * nxyz ) ;
  tsp  = (short *) malloc( sizeof(short) * nxyz )                ;
  
  /*----- attach bricks to new data set -----*/
  mri_fix_data_pointer (vdif, DSET_BRICK(new_dset,0)); 
  mri_fix_data_pointer (tsp, DSET_BRICK(new_dset,1));  
  
  
  /*----- convert data type to output specification -----*/
  fimfac = EDIT_coerce_autoscale_new (nxyz, 
				      MRI_float, ffim, 
				      output_datum, vdif);
  if (fimfac != 0.0)  fimfac = 1.0 / fimfac;
  
#define TOP_SS  32700
  
  top = TOP_SS/FUNC_CT_SCALE_SHORT;
  func_scale_short = FUNC_CT_SCALE_SHORT;
  
  for (ii = 0;  ii < nxyz;  ii++)
    {
      if (ftr[ii] > top)
	tsp[ii] = TOP_SS;
      else  if (ftr[ii] < -top)
	tsp[ii] = -TOP_SS;
      else  if (ftr[ii] >= 0.0)
	tsp[ii] = (short) (func_scale_short * ftr[ii] + 0.5);
      else
	tsp[ii] = (short) (func_scale_short * ftr[ii] - 0.5);

    }
  

  /*----- write afni fict data set -----*/
  printf("--- Writing 'fict' dataset into %s\n", DSET_BRIKNAME(new_dset) ) ;
  
  fbuf[0] = dof;   
  for( ii=1 ; ii < MAX_STAT_AUX ; ii++ ) fbuf[ii] = 0.0 ;
  (void) EDIT_dset_items( new_dset , ADN_stat_aux , fbuf , ADN_none ) ;
  
  fbuf[0] = (output_datum == MRI_short && fimfac != 1.0 ) ? fimfac : 0.0 ;
  fbuf[1] = 1.0 / func_scale_short ;
  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;


  /*----- Record history of dataset -----*/  
  tross_Make_History( PROGRAM_NAME , argc , argv , new_dset ) ;

  THD_load_statistics( new_dset ) ;
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}



