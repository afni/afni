/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  Program to resample a 'data parent' dataset to the grid defined by an
  'anat parent' dataset.

  File:    adwarp.c
  Author:  B. Douglas Ward
  Date:    02 April 1999

  Mod:     Added changes for incorporating History notes.
  Date:    10 September 1999

  Mod:     Added changes for proper byte ordering on output.
  Date:    23 Nov 1999 - RW Cox

  Mod:     Added -force option, and some checks related to it.
  Date:    13 Dec 1999 - RW Cox

  Mod:     Added 'data parent' sub-brick selector.  Also, allow operator to
           specify separate resampling modes for the functional sub-bricks
	   and the threshold sub-bricks.
  Date:    25 February 2000

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "adwarp.c"                      /* name of this program */
#define PROGRAM_AUTHOR "R. W. Cox and B. D. Ward"          /* program author */
#define PROGRAM_INITIAL "02 April 1999"   /* date of initial program release */
#define PROGRAM_LATEST "15 August 2001"     /* date of last program revision */

/*---------------------------------------------------------------------------*/

#include "afni_warp.h"

#define MAIN

/*---------------------------------------------------------------------------*/

void AFNI_copy_statistics( THD_3dim_dataset *dsold , THD_3dim_dataset *dsnew )
{
   int ibr , nvold , nvnew ;
   THD_statistics *stold , *stnew ;

ENTRY("AFNI_copy_statistics") ;

   if( !ISVALID_3DIM_DATASET(dsold) || !ISVALID_3DIM_DATASET(dsnew) ) EXRETURN ;

   nvold = dsold->dblk->nvals ;
   nvnew = dsnew->dblk->nvals ;
   stold = dsold->stats ;
   stnew = dsnew->stats ;
   if( !ISVALID_STATISTIC(stold) ) EXRETURN ;

   if( stnew == NULL ){
      dsnew->stats  = stnew = myXtNew( THD_statistics ) ;
      stnew->type   = STATISTICS_TYPE ;
      stnew->nbstat = nvnew ;
      stnew->bstat  = (THD_brick_stats *)
                        XtMalloc( sizeof(THD_brick_stats) * nvnew ) ;
      ADDTO_KILL(dsnew->kl,stnew) ;
      stnew->parent = (XtPointer) dsnew ;
   } else {
      stnew->nbstat = nvnew ;
      stnew->bstat  = (THD_brick_stats *)
                        XtRealloc( (char *) stnew->bstat ,
                                   sizeof(THD_brick_stats) * nvnew ) ;
   }

   for( ibr=0 ; ibr < nvnew ; ibr++ ){
      if( ibr < nvold )
         stnew->bstat[ibr] = stold->bstat[ibr] ;
      else
         INVALIDATE_BSTAT(stnew->bstat[ibr]) ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

#define PARENTIZE(ds,par) \
  if( ISVALID_3DIM_DATASET((ds)) ) (ds)->parent = (XtPointer) (par)

/** macro to test a malloc-ed pointer for validity **/
#define MTEST(ptr) \
  if((ptr)==NULL) \
  ( AW_error ("Cannot allocate memory") )

/*---------------------------------------------------------------------------*/

typedef struct adwarp_options
{
  THD_3dim_dataset *aset;       /* anat parent dataset */
  THD_3dim_dataset *dset;       /* data parent dataset */

  char *prefix;                 /* prefix for output file */

  float dxyz;                   /* grid spacing in output dataset (mm) */

  int anat_resam_mode;          /* anatomical dataset resampling mode */
  int thr_resam_mode;           /* threshold sub-brick resampling mode */
  int func_resam_mode;          /* functional sub-brick resampling mode */

  int verbose ;                 /* RWCox: 06 Apr 1999 */
  int force   ;                 /* RWCox: 13 Dec 1999 */

} adwarp_options;


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void AW_error (char *message)
{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to display adwarp help menu.
*/

void display_help_menu()
{
   int ii ;


   printf
     ("Usage: adwarp [options]\n"
      "Resamples a 'data parent' dataset to the grid defined by an\n"
      "'anat parent' dataset.  The anat parent dataset must contain\n"
      "in its .HEAD file the coordinate transformation (warp) needed\n"
      "to bring the data parent dataset to the output grid.  This\n"
      "program provides a batch implementation of the interactive\n"
      "AFNI 'Write' buttons, one dataset at a time.\n"
      "\n"
      "  Example: adwarp -apar anat+tlrc -dpar func+orig\n"
      "\n"
      "  This will create dataset func+tlrc (.HEAD and .BRIK).\n"
      "\n"
      "Options (so to speak):\n"
      "----------------------\n"
      "-apar aset  = Set the anat parent dataset to 'aset'.  This\n"
      "                is a nonoptional option (must be present).\n"
      "\n"
      "-dpar dset  = Set the data parent dataset to 'dset'.  This\n"
      "                is a nonoptional option (must be present).\n"
      "              Note: dset may contain a sub-brick selector,\n"
      "              e.g.,  -dpar 'dset+orig[2,5,7]'             \n"
      "\n"
      "-prefix ppp = Set the prefix for the output dataset to 'ppp'.\n"
      "                The default is the prefix of 'dset'.\n"
      "\n"
      "-dxyz ddd   = Set the grid spacing in the output datset to\n"
      "                'ddd' mm.  The default is 1 mm.\n"
      "\n"
      "-verbose    = Print out progress reports.\n"
      "-force      = Write out result even if it means deleting\n"
      "                an existing dataset.  The default is not\n"
      "                to overwrite.\n"
      "\n"
      "-resam rrr  = Set resampling mode to 'rrr' for all sub-bricks\n"
      "                     --- OR ---                              \n"
      "-thr   rrr  = Set resampling mode to 'rrr' for threshold sub-bricks\n"
      "-func  rrr  = Set resampling mode to 'rrr' for functional sub-bricks\n"
      "\n"
      "The resampling mode 'rrr' must be one of the following:\n"
      ) ;

   for( ii=0 ; ii <= LAST_RESAM_TYPE ; ii++ )
      printf("                 %s = %s\n", RESAM_shortstr[ii],RESAM_typestr[ii] ) ;


   printf
     (
      "\n"
      "NOTE:  The default resampling mode is %s for all sub-bricks. \n" ,
      RESAM_shortstr[1]
      ) ;

   PRINT_COMPILE_DATE ; exit(0) ;
}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the input options.
*/


void initialize_options (adwarp_options *option_data)
{
  option_data->aset = NULL;
  option_data->dset = NULL;
  option_data->prefix = NULL;

  option_data->dxyz = 1.0;

  option_data->anat_resam_mode = 1;
  option_data->thr_resam_mode  = 1;
  option_data->func_resam_mode = 1;

  option_data->verbose = 0 ;  /* 06 Apr 1999 */
  option_data->force   = 0 ;  /* 13 Dec 1999 */
}


/*---------------------------------------------------------------------------*/
/*
  Routine to get user specified input options.
*/

void get_options
(
  int argc,                        /* number of input arguments */
  char ** argv,                    /* array of input arguments */
  adwarp_options *option_data     /* adwarp program options */
)

{
  int nopt = 1;                     /* input option argument counter */
  int ival;                         /* integer input */
  float fval;                       /* float input */
  int ii;                           /* index */
  char message[THD_MAX_NAME];       /* error message */


  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();

  AFNI_logger(PROGRAM_NAME,argc,argv) ;


  /*----- initialize the input options -----*/
  initialize_options (option_data);


  /*----- main loop over input options -----*/
  while (nopt < argc )
    {

      /*-----   -apar aset   -----*/
      if (strncmp(argv[nopt], "-apar", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AW_error ("need argument after -apar ");
	  option_data->aset = THD_open_one_dataset (argv[nopt]);
	  if (! ISVALID_3DIM_DATASET(option_data->aset))
	    AW_error ("Cannot read anat parent dataset.\n") ;
	  nopt++;
	  continue;
	}

      /*-----   -dpar dset   -----*/
      if (strncmp(argv[nopt], "-dpar", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AW_error ("need argument after -dpar ");
	  option_data->dset = THD_open_dataset (argv[nopt]);
	  if (! ISVALID_3DIM_DATASET(option_data->dset))
	    AW_error ("Cannot read data parent dataset.\n") ;
	  nopt++;
	  continue;
	}

      /*-----   -prefix ppp   -----*/
      if (strncmp(argv[nopt], "-prefix", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AW_error ("need argument after -prefix ");
	  option_data->prefix = AFMALL(char,sizeof(char)*THD_MAX_PREFIX);
	  MTEST (option_data->prefix);
	  MCW_strncpy (option_data->prefix, argv[nopt], THD_MAX_PREFIX);

     if( strstr(option_data->prefix,".nii") != NULL ||    /* 06 Apr 2005 */
         strstr(option_data->prefix,".hdr") != NULL   ){  /* 11 Oct 2005 */
       fprintf(stderr,"** You can't use adwarp to create a NIfTI file!\n") ;
       exit(1) ;
     }

	  nopt++;
	  continue;
	}

      /*-----  -verbose  -----*/
      if( strncmp(argv[nopt],"-verbose",5) == 0 ){  /* 06 Apr 1999 */
         option_data->verbose = 1 ;
         nopt++ ; continue ;
      }

      /*-----  -force  -----*/
      if( strncmp(argv[nopt],"-force",5) == 0 ){  /* 06 Apr 1999 */
         option_data->force = 1 ;
         nopt++ ; continue ;
      }

      /*-----   -dxyz ddd   -----*/
      if (strncmp(argv[nopt], "-dxyz", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AW_error ("need argument after -dxyz ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval <= 0.0)  AW_error ("Illegal argument after -dxyz ");
	  option_data->dxyz = fval;
	  nopt++;
	  continue;
	}

      /*-----   -resam rrr  -----*/
      if (strncmp(argv[nopt], "-resam", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AW_error ("need argument after -resam ");
	  ival = -1;
	  for( ii=0 ; ii <= LAST_RESAM_TYPE ; ii++ )
	    if (strncmp(argv[nopt], RESAM_shortstr[ii], 2) == 0)
	      ival = ii;
	  if ((ival < 0) || (ival > LAST_RESAM_TYPE))
	    AW_error ("illegal argument after -resam ");
	  option_data->anat_resam_mode = ival;
	  option_data->thr_resam_mode  = ival;
	  option_data->func_resam_mode = ival;
	  nopt++;
	  continue;
	}


      /*-----   -thr rrr  -----*/
      if (strncmp(argv[nopt], "-thr", 4) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AW_error ("need argument after -thr ");
	  ival = -1;
	  for( ii=0 ; ii <= LAST_RESAM_TYPE ; ii++ )
	    if (strncmp(argv[nopt], RESAM_shortstr[ii], 2) == 0)
	      ival = ii;
	  if ((ival < 0) || (ival > LAST_RESAM_TYPE))
	    AW_error ("illegal argument after -thr ");
	  option_data->thr_resam_mode  = ival;
	  nopt++;
	  continue;
	}


      /*-----   -func rrr  -----*/
      if (strncmp(argv[nopt], "-func", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AW_error ("need argument after -func ");
	  ival = -1;
	  for( ii=0 ; ii <= LAST_RESAM_TYPE ; ii++ )
	    if (strncmp(argv[nopt], RESAM_shortstr[ii], 2) == 0)
	      ival = ii;
	  if ((ival < 0) || (ival > LAST_RESAM_TYPE))
	    AW_error ("illegal argument after -func ");
	  option_data->func_resam_mode  = ival;
	  nopt++;
	  continue;
	}


      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      AW_error (message);

    }


  /*----- Check for required inputs -----*/
  if (option_data->aset == NULL)
    AW_error ("Must specify anat parent dataset");
  if (option_data->dset == NULL)
    AW_error ("Must specify data parent dataset");

  /*-- 13 Dec 1999: check if datasets have the same view --*/

  if( option_data->aset->view_type == option_data->dset->view_type ){
      if( option_data->force ){
         if( option_data->prefix == NULL ){
            fprintf(stderr,
                    "** Error: -apar & -dpar are in same +view!\n"
                    "          This is illegal without the -prefix option!\n") ;
            exit(1) ;
         } else {
            fprintf(stderr,
                    "++ Warning: -apar & -dpar are in same +view!\n") ;
         }
      } else {
         fprintf(stderr,
                 "** Error: -apar & -dpar are in same +view!\n"
                 "          If this is OK, use -force and -prefix options.\n" ) ;
         exit(1) ;
      }
  }


}


/*---------------------------------------------------------------------------*/
/*
   Make a warped dataset whose grid corresponds to the anat_parent and
   whose data comes from the data_parent.
   Note that the assumption is made that data_parent and the warp parent
   of the anat_parent are both in the same coordinate system (up to the
   to_dicomm transformation of their dataxes structs).

   This routine is adapted from AFNI_follower_dataset.
*/

THD_3dim_dataset * adwarp_follower_dataset
(
  adwarp_options   *option_data,    /* adwarp program options */
  THD_3dim_dataset *anat_parent,    /* dataset containing warp information */
  THD_3dim_dataset *data_parent     /* dataset to be warped */
)
{
  THD_3dim_dataset *new_dset ;
  int ii ;

ENTRY("adwarp_follower_dataset") ;

/* sanity checks */

  if( ! ISVALID_3DIM_DATASET(anat_parent) ||
      ! ISVALID_3DIM_DATASET(data_parent)   ) RETURN(NULL) ;

  /* make new dataset, copying appropriate fields from its various parents */

  new_dset = myXtNew( THD_3dim_dataset ) ; INIT_KILL( new_dset->kl ) ;

  new_dset->type      = data_parent->type;        /* same data type */
  new_dset->func_type = data_parent->func_type;
  new_dset->view_type = anat_parent->view_type;   /* but different view type */

  new_dset->anat_parent = anat_parent;            /* what else makes sense? */

  new_dset->tagset = NULL ;  /* Oct 1998 */
  new_dset->Label_Dtable = NULL;                  /* ZSS Feb 26 2010 */
  
  MCW_strncpy( new_dset->anat_parent_name ,
               anat_parent->self_name , THD_MAX_NAME ) ;

  new_dset->anat_parent_idcode = anat_parent->idcode ;

   /* 11/09/94 addition: the data_parent may itself be a warp;
       in this case, we want the true warp parent to be the original data */

  new_dset->warp_parent =  (data_parent->warp_parent != NULL)
                         ? (data_parent->warp_parent) : (data_parent) ;

  MCW_strncpy( new_dset->warp_parent_name ,
               new_dset->warp_parent->self_name , THD_MAX_NAME ) ;

  new_dset->warp_parent_idcode = new_dset->warp_parent->idcode ;

  new_dset->idcode = MCW_new_idcode() ;

  /* make the actual warp from the warp_parent to this dataset */

  new_dset->vox_warp       = NULL ;
  new_dset->warp           = myXtNew( THD_warp ) ;
  *(new_dset->warp)        = IDENTITY_WARP ;  /* start with (Dicom) identity */

  new_dset->self_warp      = NULL ;           /* 26 Aug 2002 */

  /* follow the links backward from desired view to original view */

  AFNI_concatenate_warp( new_dset->warp , anat_parent->warp ) ;
  AFNI_concatenate_warp( new_dset->warp , data_parent->warp ) ;

  /* reset the bounds in the new warp to be the same as in the anat_parent */

  if( ISVALID_WARP(anat_parent->warp) &&
      anat_parent->warp->type == new_dset->warp->type ){

    switch( anat_parent->warp->type ){
	
    case WARP_AFFINE_TYPE:
      COPY_LMAP_BOUNDS( new_dset->warp->rig_bod.warp ,
			anat_parent->warp->rig_bod.warp ) ;
      break ;

    case WARP_TALAIRACH_12_TYPE:
      for( ii=0 ; ii < 12 ; ii++ )
	COPY_LMAP_BOUNDS( new_dset->warp->tal_12.warp[ii] ,
			  anat_parent->warp->tal_12.warp[ii] ) ;
      break ;
    }
  }

  /* make up some names for this new dataset */

  MCW_strncpy( new_dset->self_name  ,
	       new_dset->warp_parent->self_name , THD_MAX_NAME ) ;
  ii = strlen( new_dset->self_name ) ;
  new_dset->self_name[ii++] = '@' ;
  MCW_strncpy( &(new_dset->self_name[ii]) ,
	       VIEW_typestr[new_dset->view_type] , THD_MAX_NAME-ii ) ;

  MCW_strncpy( new_dset->label1 , data_parent->label1 , THD_MAX_LABEL ) ;
  MCW_strncpy( new_dset->label2 , data_parent->label2 , THD_MAX_LABEL ) ;

  /* set the axes for this new dataset
     (same as anatomy parent, since that's the meaning of this routine) */

  new_dset->daxes         = myXtNew( THD_dataxes ) ;  /* copy data axes of */
  *(new_dset->daxes)      = *(anat_parent->daxes) ; /* anatomy parent */

  new_dset->wod_daxes     = NULL ;
  new_dset->wod_flag      = True ;

  /* 06 Aug 1996: added ability to use 3D+t datasets here */

  if( DSET_NUM_TIMES(data_parent) < 2 ){
    new_dset->taxis = NULL ;
  } else {
    new_dset->taxis  = myXtNew( THD_timeaxis ) ;  /* new */
    *(new_dset->taxis) = *(data_parent->taxis) ;  /* copy insides */

    new_dset->taxis->nsl     = 0 ;                      /* no slice stuff */
    new_dset->taxis->toff_sl = NULL ;
    new_dset->taxis->zorg_sl = 0.0 ;
    new_dset->taxis->dz_sl   = 0.0 ;
  }

  /* create a datablock and diskptr, in case the data is ever
     filled into memory (instead of wod) and written to disk */

  new_dset->dblk = myXtNew( THD_datablock ) ; INIT_KILL( new_dset->dblk->kl ) ;

  new_dset->dblk->type        = DATABLOCK_TYPE ;
  new_dset->dblk->nvals       = data_parent->dblk->nvals ;
  new_dset->dblk->malloc_type = DATABLOCK_MEM_UNDEFINED ;
  new_dset->dblk->natr        = new_dset->dblk->natr_alloc  = 0 ;
  new_dset->dblk->atr         = NULL ;
  new_dset->dblk->parent      = (XtPointer) new_dset ;

  if( data_parent->dblk->brick_lab == NULL ){
    THD_init_datablock_labels( new_dset->dblk ) ; /* 30 Nov 1997 */
  } else {
    THD_copy_datablock_auxdata( data_parent->dblk , new_dset->dblk ) ;
  }

  DSET_unlock(new_dset) ;  /* Feb 1998 */

  new_dset->dblk->diskptr               = myXtNew( THD_diskptr ) ;
  new_dset->dblk->diskptr->type         = DISKPTR_TYPE ;
  new_dset->dblk->diskptr->nvals        = data_parent->dblk->nvals ;
  new_dset->dblk->diskptr->rank         = 3 ;
  new_dset->dblk->diskptr->storage_mode = STORAGE_UNDEFINED ;
  new_dset->dblk->diskptr->byte_order   = THD_get_write_order() ;
                                                            /* 25 April 1998 */
  new_dset->dblk->diskptr->dimsizes[0]  = new_dset->daxes->nxx ;
  new_dset->dblk->diskptr->dimsizes[1]  = new_dset->daxes->nyy ;
  new_dset->dblk->diskptr->dimsizes[2]  = new_dset->daxes->nzz ;

  new_dset->dblk->brick_fac   = NULL ;  /* initialized below */
  new_dset->dblk->brick_bytes = NULL ;
  new_dset->dblk->brick       = NULL ;
  THD_init_datablock_brick( new_dset->dblk , -1 , data_parent->dblk ) ;

  new_dset->dblk->master_nvals = 0 ;     /* 11 Jan 1999 */
  new_dset->dblk->master_ival  = NULL ;
  new_dset->dblk->master_bytes = NULL ;

  /* create the names for storage on disk (if ever)
     -- note we put it in the same directory as the data_parent */

  if (option_data->prefix == NULL)
    THD_init_diskptr_names (new_dset->dblk->diskptr,
			    data_parent->dblk->diskptr->directory_name, NULL,
			    data_parent->dblk->diskptr->prefix,
			    new_dset->view_type, True);
  else
    THD_init_diskptr_names (new_dset->dblk->diskptr,
			    data_parent->dblk->diskptr->directory_name, NULL,
			    option_data->prefix,
			    new_dset->view_type, True);


  ADDTO_KILL( new_dset->dblk->kl , new_dset->dblk->diskptr ) ;

  /* oh yeah, set the new_dset kill list,
     copy statistics if available, and NULL out any unused stuff */

  ADDTO_KILL( new_dset->kl , new_dset->warp ) ;
  ADDTO_KILL( new_dset->kl , new_dset->daxes ) ;
  ADDTO_KILL( new_dset->kl , new_dset->dblk ) ;

  new_dset->stats = NULL ;
  AFNI_copy_statistics( data_parent , new_dset ) ;

  INIT_STAT_AUX( new_dset , MAX_STAT_AUX , data_parent->stat_aux ) ;

  new_dset->markers     = NULL ;  /* no markers */
  new_dset->death_mark  = 0 ;     /* don't kill me! */
#ifdef ALLOW_DATASET_VLIST
  new_dset->pts         = NULL ;
#endif

  PARENTIZE(new_dset,data_parent->parent) ;

  new_dset->tcat_list   = NULL ;  /* 03 Aug 2004 */
  new_dset->tcat_num    = 0 ;
  new_dset->tcat_len    = NULL ;

  RETURN(new_dset) ;
}


/*---------------------------------------------------------------------------*/
/*
   (re)compute and (re)write a dataset to disk, with the indicated
   geometric parameters.  Note that the filenames for the save should
   already be initialized in the dset->dblk->diskptr.  Also, note
   that the dataset's "permanent" dataxes will be remade to fit the
   new geometry.

   This routine is adapted from AFNI_refashion_dataset.
*/


Boolean adwarp_refashion_dataset
(
  adwarp_options   *option_data,  /* adwarp program options */
  THD_3dim_dataset *dset,         /* new (output) dataset */
  THD_dataxes      *daxes         /* new dataset axes */
)
{
  THD_datablock *dblk  = dset->dblk ;
  THD_diskptr   *dkptr = dset->dblk->diskptr ;
  Boolean good ;
  int npix , nx,ny,nz,nv , kk , ival , code , nzv , dsiz , isfunc , cmode ;
  MRI_IMAGE *im ;
  void *imar ;
  FILE *far ;
  float brfac_save ;
  int resam_mode;

  int native_order , save_order ;  /* 23 Nov 1999 */

ENTRY("adwarp_refashion_dataset") ;

  /* set up for warp-on-demand */

  dset->wod_daxes         = myXtNew(THD_dataxes) ; /* 02 Nov 1996 */
  dset->wod_daxes->type   = DATAXES_TYPE ;       /* 02 Nov 1996 */
  dset->vox_warp          = myXtNew(THD_warp) ;    /* 02 Nov 1996 */

  dset->self_warp         = NULL ;                 /* 26 Aug 2002 */

  *(dset->wod_daxes)      = *daxes ;            /* copy insides of daxes */
  dset->wod_flag          = True ;              /* mark for warp-on-demand */
  dset->vox_warp->type    = ILLEGAL_TYPE ;      /* mark for recomputation */

  /* copy the new geometric information into various places */

  *(dset->daxes)     = *(daxes) ;               /* Make daxes permanent */
  dkptr->dimsizes[0] = dset->daxes->nxx ;       /* Will cause trouble */
  dkptr->dimsizes[1] = dset->daxes->nyy ;       /* if diskptr and     */
  dkptr->dimsizes[2] = dset->daxes->nzz ;       /* daxes don't match! */

  /* write the header out */
 
  good = THD_write_3dim_dataset( NULL,NULL , dset , False ) ;
  if( !good ){
    fprintf(stderr,"\a\n*** cannot write dataset header ***\n") ;

    RETURN(False) ;
  }
  STATUS("wrote output header file") ;

  /* at this point in time, the output dataset is set and the header
   *   has been written - so we must overwrite from now on */
  putenv("AFNI_DECONFLICT=OVERWRITE") ;  /* 19 Nov 2007 */

  /* purge the datablock that now exists,
     then delete the file on disk that now exists (if any) */

  DSET_unlock( dset ) ; /* Feb 1998 */
  PURGE_DSET( dset ) ;
  COMPRESS_unlink(dkptr->brick_name) ;

  /* refashion its brick data structure,
     which requires first saving in a temporary
     array the datum type for each sub-brick    */

  { int ibr ; int *typ ;
    typ = (int *) XtMalloc( sizeof(int) * dblk->nvals ) ;
    for( ibr=0 ; ibr < dblk->nvals ; ibr++ )
      typ[ibr] = DBLK_BRICK_TYPE(dblk,ibr) ;
    THD_init_datablock_brick( dblk , dblk->nvals , typ ) ;
    myXtFree( typ ) ;
  }

   /*-- 13 Mar 2006: check for free disk space --*/

   { int mm = THD_freemegabytes(dkptr->header_name) ;
     int rr = (int)(dblk->total_bytes/(1024*1024)) ;
     if( rr >= 666 )
       fprintf(stderr,"++ WARNING: output filesize %s will be %d Mbytes!\n"
                      "++ SUGGEST: increase voxel size to save disk space.\n",
               dkptr->brick_name , rr ) ;
     if( mm >= 0 && mm <= rr )
       WARNING_message("Disk space: writing file %s (%d MB),"
                       " but only %d free MB on disk"        ,
               dkptr->brick_name , rr , mm ) ;
   }


  dkptr->storage_mode = STORAGE_UNDEFINED ;       /* just for now */
  dblk->malloc_type   = DATABLOCK_MEM_UNDEFINED ;

  /*--- open the output file
    (N.B.: much of the following code is from THD_write_datablock) ---*/

  /*-- create directory if necessary --*/

  if( ! THD_is_directory(dkptr->directory_name) ){
    kk = mkdir( dkptr->directory_name , THD_MKDIR_MODE ) ;
    if( kk != 0 ){
      fprintf(stderr,
            "\a\n*** cannot mkdir new directory: %s\n",dkptr->directory_name) ;

      RETURN(False) ;
    }
    STATUS("created subdirectory") ;
  }

  /*-- open output file --*/

  if( option_data->verbose )
    fprintf(stderr,"++ Opening output file %s\n",dkptr->brick_name) ;

  cmode = THD_get_write_compression() ;
  far = COMPRESS_fopen_write( dkptr->brick_name , cmode ) ;
  if( far == NULL ){
    fprintf(stderr,
	    "\a\n*** cannot open output file %s\n",dkptr->brick_name) ;

    RETURN(False) ;
  }
  STATUS("created output brick file") ;

  /*--------- now, create each slice and write it out ----------*/

  nx = dset->daxes->nxx ;
  ny = dset->daxes->nyy ;  npix = nx*ny ;
  nz = dset->daxes->nzz ;
  nv = dkptr->nvals     ;  nzv  = nz*nv ;

  isfunc = ISFUNC(dset) ;  /* 09 Dec 1997 */

  if( ! isfunc )
    resam_mode = option_data->anat_resam_mode ;

   native_order = mri_short_order() ;                           /* 23 Nov 1999 */
   save_order   = (dkptr->byte_order > 0) ? dkptr->byte_order
                                          : THD_get_write_order() ;

  for( ival=0 ; ival < nv ; ival++ ){  /* for each sub-brick */

    if( option_data->verbose )
       fprintf(stderr,"++ Start sub-brick %d",ival) ;

    dsiz = mri_datum_size( DSET_BRICK_TYPE(dset,ival) ) ;

    /** force return of unscaled slices for output **/

    brfac_save                   = DBLK_BRICK_FACTOR(dblk,ival) ;
    DBLK_BRICK_FACTOR(dblk,ival) = 0.0 ;

    if( isfunc )
      resam_mode = (DSET_BRICK_STATCODE(dset,ival) > 0)  /* 09 Dec 1997 */
      ? option_data->thr_resam_mode
      : option_data->func_resam_mode ;

    for( kk=0 ; kk < nz ; kk++ ){  /* for each slice */

      im = AFNI_dataset_slice( dset , 3 , kk , ival , resam_mode ) ;
STATUS("have new image") ;

      if( option_data->verbose && kk%7==0 ) fprintf(stderr,".");

      if( im == NULL ){
	fprintf(stderr,"\a\n*** failure to compute dataset slice %d\n",kk) ;
	COMPRESS_fclose(far) ;
	COMPRESS_unlink( dkptr->brick_name ) ;

	RETURN(False) ;
      }

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"writing slice %d: type=%s nx=%d ny=%d\n",
          kk,MRI_TYPE_NAME(im) , im->nx,im->ny ) ;
  STATUS(str) ; }
#endif

        imar = mri_data_pointer(im) ;
         if( save_order != native_order ){                   /* 23 Nov 1999 */
            switch( im->kind ){
               case MRI_short:   mri_swap2(  npix,imar) ; break ;
               case MRI_float:
               case MRI_int:     mri_swap4(  npix,imar) ; break ;
               case MRI_complex: mri_swap4(2*npix,imar) ; break ;
            }
         }
	code = fwrite( imar , dsiz , npix , far ) ;
	mri_free(im) ;
	
	if( code != npix ){
	  fprintf(stderr,
		  "\a\n*** failure to write dataset slice %d (is disk full?)\n",kk) ;
	  COMPRESS_fclose(far) ;
	  COMPRESS_unlink( dkptr->brick_name ) ;
	
	  RETURN(False) ;
	}
	
    } /* end of loop over kk (z-direction) */

    if( option_data->verbose ) fprintf(stderr,"\n");

    /* restore the correct scaling of this sub-brick */

    DBLK_BRICK_FACTOR(dblk,ival) = brfac_save ;

  } /* end of loop over iv (nvals direction) */
  STATUS("all slices written") ;

  /*--------------------- done!!! ---------------------*/

  COMPRESS_fclose(far) ;
  STATUS("output file closed") ;

  /*--- do a little surgery on the dataset's storage flags ---*/

  dkptr->storage_mode = STORAGE_BY_BRICK ;
#if MMAP_THRESHOLD > 0
  dblk->malloc_type   = (dblk->total_bytes > MMAP_THRESHOLD)
                      ? DATABLOCK_MEM_MMAP : DATABLOCK_MEM_MALLOC ;

  if( cmode >= 0 ) dblk->malloc_type = DATABLOCK_MEM_MALLOC ;
  DBLK_mmapfix(dblk) ;  /* 28 Mar 2005 */
#else
  dblk->malloc_type   = DATABLOCK_MEM_MALLOC ;
#endif

  /*--- recompute the statistics and rewrite the header to hold them ---*/

  STATUS("recomputing statistics") ;

  if( option_data->verbose ) fprintf(stderr,"++ Computing statistics\n");
  THD_load_statistics( dset ) ;

  STATUS("rewriting header") ;
  (void) THD_write_3dim_dataset( NULL,NULL , dset , False ) ;

  STATUS("purging datablock") ;

  PURGE_DSET( dset ) ;

  myXtFree(dset->wod_daxes) ; myXtFree(dset->vox_warp) ;  /* 02 Nov 1996 */

  RETURN(True) ;
}


/*---------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
  adwarp_options   *option_data;           /* adwarp program options */
  THD_3dim_dataset *new_dset = NULL;       /* new (output) dataset */
  THD_dataxes new_daxes;                   /* new dataset axes */

  mainENTRY("adwarp main") ; machdep() ; PRINT_VERSION("adwarp") ;
  AUTHOR(PROGRAM_AUTHOR) ;

#if 0
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR);
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");
#endif

  /*----- Allocate memory -----*/
  option_data = (adwarp_options *) malloc (sizeof(adwarp_options));

  /*----- Get user inputs -----*/
  get_options (argc, argv, option_data);

  /*----- Create empty shell of dataset from anat parent and data parent ----*/
  new_dset = adwarp_follower_dataset (option_data, option_data->aset,
				      option_data->dset);

  /*--- 13 Dec 1999: check if output dataset already exists on disk ---*/

  if( THD_is_file(DSET_HEADNAME(new_dset)) ||
      THD_is_file(DSET_BRIKNAME(new_dset))    ){

      if( option_data->force ){
         fprintf(stderr,
                 "++ Warning: overwriting dataset %s and %s\n",
                 DSET_HEADNAME(new_dset), DSET_BRIKNAME(new_dset) ) ;
         putenv("AFNI_DECONFLICT=OVERWRITE") ;  /* 12 Nov 2007 */
      }
   }

/* aside from -force, let the user's AFNI_DECONFLICT variable control this */
/*                                                     19 Nov 2007 [rickr] */
#if 0
else if( THD_deathcon() ){
         fprintf(stderr,
                 "** Error: can't overwrite dataset %s and %s\n"
                 "          unless you use the -force option!\n" ,
                 DSET_HEADNAME(new_dset), DSET_BRIKNAME(new_dset) ) ;
         exit(1) ;
      } else {
         putenv("AFNI_DECONFLICT=YES") ;  /* 12 Nov 2007 */
         THD_deconflict_prefix(new_dset) ;
         WARNING_message("Changed dataset name to '%s' to avoid conflict",
                         DSET_BRIKNAME(new_dset) ) ;
      }
#endif

  /*----- Record history of dataset -----*/
  tross_Copy_History( option_data->dset , new_dset ) ;
  tross_Make_History( PROGRAM_NAME , argc , argv , new_dset ) ;

  /*----- Allow for resampling to a new voxel size -----*/
  new_daxes.type = DATAXES_TYPE;
  THD_edit_dataxes (option_data->dxyz, option_data->aset->daxes, &new_daxes);


  /*----- Fill in the dataset and write out to disk -----*/
  adwarp_refashion_dataset (option_data, new_dset, &new_daxes);

  exit(0) ;
}
