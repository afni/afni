/*
  This file contains routines common to the nonparametric statistical
  analysis programs.

  File:    NPstats.c
  Author:  B. Douglas Ward
  Date:    23 July 1997

*/


/*---------------------------------------------------------------------------*/
/*
  This software is Copyright 1997 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application. The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.

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
/*
   Get the dimensions of the 3d AFNI data sets.
*/

void get_dimensions (NP_options * option_data)
{
  
   THD_3dim_dataset * dset=NULL;

   /*----- read first dataset to get dimensions, etc. -----*/

   dset = THD_open_one_dataset( option_data->first_dataset ) ;
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
  dset = THD_open_one_dataset (option_data->first_dataset ) ;
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
do{ int pv ; (ds) = THD_open_one_dataset((name)) ;                         \
       if( !ISVALID_3DIM_DATASET((ds)) ){                                     \
          fprintf(stderr,"*** Can't open dataset: %s\n",(name)) ; exit(1) ; } \
       if( (ds)->daxes->nxx!=nx || (ds)->daxes->nyy!=ny || (ds)->daxes->nzz!=nz ){   \
          fprintf(stderr,"*** Axes mismatch: %s\n",(name)) ; exit(1) ; }      \
       if( DSET_NUM_TIMES((ds)) > 1 ){                                        \
         fprintf(stderr,"*** Can't use time-dependent data: %s\n",(name));exit(1); } \
       THD_load_datablock( (ds)->dblk , NULL ) ;                              \
       pv = DSET_PRINCIPAL_VALUE((ds)) ;                                      \
       if( DSET_ARRAY((ds),pv) == NULL ){                                     \
          fprintf(stderr,"*** Can't access data: %s\n",(name)) ; exit(1); }   \
       if( DSET_BRICK_TYPE((ds),pv) == MRI_complex ){                         \
          fprintf(stderr,"*** Can't use complex data: %s\n",(name)) ; exit(1); }     \
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
     
/** macro to test a malloc-ed pointer for validity **/
     
#define MTEST(ptr) \
     if((ptr)==NULL) \
     ( fprintf(stderr,"*** Cannot allocate memory for statistics!\n"         \
	       "*** Try using the -workmem option to reduce memory needs,\n" \
	       "*** or create more swap space in the operating system.\n"    \
	       ), exit(0) )
     

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
  Structure to store list of values, sorted in increasing order.
*/
  
typedef struct node
{
  float fval;             /* floating point value */
  int d;                  /* count of number of occurances of this value */
  struct node * next;     /* link to next node */
} node;


/*---------------------------------------------------------------------------*/
/*
  Print contents of list, starting at smallest value. 
*/

void list_print (node * n, int * count)
{
  int i;

  for (i = 0;  i < n->d;  i++)
    {
      printf (" %6.1f", n->fval);
      *count += 1;
      if (*count % 10 == 0)
	printf ("\n");
    }

  if (n->next != NULL)
    list_print (n->next, count);
}


/*---------------------------------------------------------------------------*/
/*
  Delete the entire list.
*/

void list_delete (node ** n)
{
  if ((*n)->next != NULL)
    list_delete (&((*n)->next));
  free (*n);
  *n = NULL;
}


/*---------------------------------------------------------------------------*/
/*
  Insert one node with value r; reset pointers.
*/

void node_insert (node ** n, float r)
{
  node * ptr;

  ptr = *n;
  *n = (node *) malloc (sizeof(node));
  (*n)->fval = r;
  (*n)->d = 1;
  (*n)->next = ptr;
}


/*---------------------------------------------------------------------------*/
/*
  Add number r to list; if number is already in the list, just increment the
  counter.  Otherwise, insert new node.
*/

void node_addvalue (node ** head, float r)
{
  node ** lastptr;
  node * ptr;


  if (*head == NULL)  node_insert (head, r);
  else
    {
      lastptr = head;
      ptr = *head;

      while ( (ptr->fval < r) && (ptr->next != NULL) )
	{
	  lastptr = &(ptr->next);
	  ptr = ptr->next;
	}
      
      if (ptr->fval > r)
	node_insert (lastptr, r);
      else
	if (ptr->fval == r)
	  ptr->d += 1;
        else
	  node_insert (&(ptr->next), r);
    }
}


/*---------------------------------------------------------------------------*/
/*
  Get rank corresponding to number r.  If ties exist, return average rank.
*/

float node_get_rank (node * head, float r)
{
  node * ptr;
  float rank;

  ptr = head;
  rank = 0.0;

  while (ptr->fval != r)
    {
      rank += ptr->d;
      ptr = ptr->next;
    }

  rank += (ptr->d + 1) / 2.0;
  return (rank);
}


/*---------------------------------------------------------------------------*/
/*
  Return value corresponding to the specified rank.
*/

float node_get_value (node * head, int rank)
{
  node * ptr;
  int k;

  ptr = head;
  k = 0;

  while (k + ptr->d < rank)
    {
      k += ptr->d;
      ptr = ptr->next;
    }

  return (ptr->fval);
}


/*---------------------------------------------------------------------------*/
/*
  Return value corresponding to the median value.
*/

float node_get_median (node * head, int n)
{
  float median;


  if (n % 2)
    median = node_get_value(head, n/2 + 1);
  else
    median = 0.5 * (node_get_value(head, n/2) + 
		    node_get_value(head, n/2 + 1));

  return (median);
}


/*---------------------------------------------------------------------------*/
/*
  Convert one volume to another type, autoscaling:
     nxy   = # voxels
     itype = input datum type
     ivol  = pointer to input volume
     otype = output datum type
     ovol  = pointer to output volume (again, must be pre-malloc-ed)
  Return value is the scaling factor used (0.0 --> no scaling).
*/

float EDIT_coerce_autoscale_new( int nxyz ,
				 int itype,void *ivol , int otype,void *ovol )
{
  float fac=0.0 , top ;
  
  if( MRI_IS_INT_TYPE(otype) ){
    top = MCW_vol_amax( nxyz,1,1 , itype,ivol ) ;
    if (top == 0.0)  fac = 0.0;
    else  fac = MRI_TYPE_maxval[otype]/top;
  }
  
  EDIT_coerce_scale_type( nxyz , fac , itype,ivol , otype,ovol ) ;
  return ( fac );
}


/*---------------------------------------------------------------------------*/
/*
  Routine to write one AFNI fizt data set.
  
  This data set is 'fizt' type (intensity + Standard Normal statistic)
  
  The intensity data is in ffim, and the corresponding statistic is in ftr.
  
*/

void write_afni_fizt (NP_options * option_data,  char * filename,  
                      float * ffim,  float * ftr)
{
  int nxyz;                           /* number of voxels */
  int ii;                             /* voxel index */
  THD_3dim_dataset * dset=NULL;       /* input afni data set pointer */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int iv;                             /* sub-brick index */ 
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
  dset = THD_open_one_dataset (option_data->first_dataset) ;
  if( ! ISVALID_3DIM_DATASET(dset) ){
    fprintf(stderr,"*** Unable to open dataset file %s\n",
	    option_data->first_dataset);
    exit(1) ;
  }
  
  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy( dset ) ;
  
  
  iv = DSET_PRINCIPAL_VALUE(dset) ;
  if( option_data->datum >= 0 ){
    output_datum = option_data->datum ;
  } else {
    output_datum = DSET_BRICK_TYPE(dset,iv) ;
    if( output_datum == MRI_byte ) output_datum = MRI_short ;
  }
  
  
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
  printf("--- Writing 'fizt' dataset into %s\n",
	 new_dset->dblk->diskptr->header_name) ;
  
  for( ii=0 ; ii < MAX_STAT_AUX ; ii++ ) fbuf[ii] = 0.0 ;
  (void) EDIT_dset_items( new_dset , ADN_stat_aux , fbuf , ADN_none ) ;
  
  fbuf[0] = (output_datum == MRI_short && fimfac != 1.0 ) ? fimfac : 0.0 ;
  fbuf[1] = 1.0 / func_scale_short ;
  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;
  
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

void write_afni_fict (NP_options * option_data,  char * filename,  
                      float * ffim,  float * ftr,  int dof)
{
  int nxyz;                           /* number of voxels */
  int ii;                             /* voxel index */
  THD_3dim_dataset * dset=NULL;       /* input afni data set pointer */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int iv;                             /* sub-brick index */ 
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
  dset = THD_open_one_dataset (option_data->first_dataset) ;
  if( ! ISVALID_3DIM_DATASET(dset) ){
    fprintf(stderr,"*** Unable to open dataset file %s\n",
	    option_data->first_dataset);
    exit(1) ;
  }
  
  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy( dset ) ;
  
  
  iv = DSET_PRINCIPAL_VALUE(dset) ;
  if( option_data->datum >= 0 ){
    output_datum = option_data->datum ;
  } else {
    output_datum = DSET_BRICK_TYPE(dset,iv) ;
    if( output_datum == MRI_byte ) output_datum = MRI_short ;
  }
  
  
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
  printf("--- Writing 'fict' dataset into %s\n",
	 new_dset->dblk->diskptr->header_name) ;
  
  fbuf[0] = dof;   
  for( ii=1 ; ii < MAX_STAT_AUX ; ii++ ) fbuf[ii] = 0.0 ;
  (void) EDIT_dset_items( new_dset , ADN_stat_aux , fbuf , ADN_none ) ;
  
  fbuf[0] = (output_datum == MRI_short && fimfac != 1.0 ) ? fimfac : 0.0 ;
  fbuf[1] = 1.0 / func_scale_short ;
  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;
  
  THD_load_statistics( new_dset ) ;
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}



