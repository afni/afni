/*---------------------------------------------------------------------------*/
/*
  This program performs 2d image registration of slices contained in an AFNI
  3d+time dataset.  This program was adapted from plug_imreg.c and imreg.c.


  File:    2dImReg.c
  Author:  B. Douglas Ward
  Date:    04 February 1998

*/


/*---------------------------------------------------------------------------*/
/*
  This software is Copyright 1998 by

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

#define PROGRAM_NAME "2dImReg"                       /* name of this program */
#define LAST_MOD_DATE "04 February 1998"         /* date of last program mod */

#define MAX_NAME_LENGTH 80

#include "mrilib.h"

typedef struct IR_options
{
  char * input_filename;
  char * new_prefix;
  char * base_filename;
  int base;
  int nofine;
  float blur;
  float dxy;
  float dphi;
  int debug;
} IR_options;


/*---------------------------------------------------------------------------*/
/*
  Routine to display 2dImReg help menu.
*/

void display_help_menu()
{
  printf 
    (
     "This program performs 2d image registration.  Image alignment is      \n"
     "performed on a slice-by-slice basis for the input 3d+time dataset,    \n"
     "relative to a user specified base image.                              \n"
     "                                                                      \n"
     "Usage:                                                                \n"
     "2dImReg                                                               \n"
     "-input fname           Filename of input 3d+time dataset to process   \n"
     "-prefix pname          Prefix name for output 3d+time dataset         \n"
     "-basefile fname        Filename of 3d+time dataset for base image     \n"
     "                         (default = current input dataset)            \n"
     "-base num              Time index for base image  (0 <= num)          \n"
     "                         (default:  num = 3)                          \n"
     "-nofine                Deactivate fine fit phase of image registration\n"
     "                         (default:  fine fit is active)               \n"
     "-fine blur dxy dphi    Set fine fit parameters                        \n"
     "   where:                                                             \n"
     "     blur = FWHM of blurring prior to registration (in pixels)        \n"
     "               (default:  blur = 1.0)                                 \n"
     "     dxy  = Convergence tolerance for translations (in pixels)        \n"
     "               (default:  dxy  = 0.07)                                \n"
     "     dphi = Convergence tolerance for rotations (in degrees)          \n"
     "               (default:  dphi = 0.21)                                \n"
     "                                                                      \n"
     "-debug                 Lots of additional output to screen            \n"
    );
  
  exit(0);
}


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void IR_error 
(
  char * message               /* error message to be displayed */
)

{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the input options.
*/

void initialize_options
(
  IR_options ** opt
)

{
  (*opt) = (IR_options *) malloc (sizeof(IR_options));

  (*opt)->input_filename = NULL;
  (*opt)->new_prefix = NULL;
  (*opt)->base_filename = NULL;
  (*opt)->base = 3;       
  (*opt)->nofine = 0;
  (*opt)->blur = 1.0;     
  (*opt)->dxy  = 0.07;    
  (*opt)->dphi = 0.21;   
  (*opt)->debug = 0;
}


/*---------------------------------------------------------------------------*/
/*
   Routine to get user specified input options.
*/

void get_user_inputs 
(
  int argc,                       /* number of input arguments */
  char ** argv,                   /* array of input arguments */ 
  IR_options ** option_data
)

{
  int nopt = 1;                   /* input option argument counter */
  int ival;
  float fval;
  char message[80];               /* error message */

  
  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();  
  

   /*----- main loop over input options -----*/
  while (nopt < argc )
    {

      /*-----   -input filename   -----*/
      if (strncmp(argv[nopt], "-input", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  IR_error ("Need argument after -input ");
	  (*option_data)->input_filename 
	    = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy ((*option_data)->input_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
     

      /*-----   -prefix pname   -----*/
      if (strncmp(argv[nopt], "-prefix", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  IR_error ("Need argument after -prefix ");
	  (*option_data)->new_prefix 
	    = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy ((*option_data)->new_prefix, argv[nopt]);

	  nopt++;
	  continue;
	}
     

      /*-----   -basefile filename   -----*/
      if (strncmp(argv[nopt], "-basefile", 9) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  IR_error ("Need argument after -basefile ");
	  (*option_data)->base_filename 
	    = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy ((*option_data)->base_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
     

      /*-----   -base num  -----*/
      if (strncmp(argv[nopt], "-base", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  IR_error ("Need argument after -base ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0) 
	    IR_error ("Illegal argument after -base  ( must be >= 0 ) ");
	  (*option_data)->base = ival;
	  nopt++;
	  continue;
	}

      
      /*-----   -nofine -----*/
      if (strncmp(argv[nopt], "-nofine", 7) == 0)
	{
	  (*option_data)->nofine = 1;
	  nopt++;
	  continue;
	}

           
       /*-----   -fine blur dxy dphi  -----*/
      if (strncmp(argv[nopt], "-fine", 5) == 0)
	{
	  nopt++;
	  if (nopt+2 >= argc)  IR_error ("Need 3 arguments after -fine ");
	  (*option_data)->nofine = 0;

	  sscanf (argv[nopt], "%f", &fval);
	  if (fval <= 0.0) 
	    IR_error ("Illegal argument for blur  ( must be > 0 ) ");
	  (*option_data)->blur = fval;
	  nopt++;

	  sscanf (argv[nopt], "%f", &fval);
	  if (fval <= 0.0)
	    IR_error ("Illegal argument for dxy  ( must be > 0 ) ");
	  (*option_data)->dxy = fval;
	  nopt++;

	  sscanf (argv[nopt], "%f", &fval);
	  if (fval <= 0.0)
	    IR_error ("Illegal argument for dphi  ( must be > 0 ) ");
	  (*option_data)->dphi = fval;
	  nopt++;

	  continue;
	}
      

      /*-----   -debug -----*/
      if (strncmp(argv[nopt], "-debug", 6) == 0)
	{
	  (*option_data)->debug = 1;
	  nopt++;
	  continue;
	}

           
      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      IR_error (message);


    }
      
 
}


/*---------------------------------------------------------------------------*/
/*
  Routine to perform all program initialization.
*/

void initialize_program
(
  int argc,                         /* number of input arguments */
  char ** argv,                     /* array of input arguments */ 
  IR_options ** option_data
)

{

  /*----- Initialize input options -----*/
  initialize_options (option_data);


  /*----- Get user inputs -----*/
  get_user_inputs (argc, argv, option_data);

}


/*---------------------------------------------------------------------------*/
/*
   Routine to make a copy of a dataset, with data attached.
   This routine is copied directly from afni_plugin.c
*/

THD_3dim_dataset * copy_dset( THD_3dim_dataset * dset , char * new_prefix )
{
   THD_3dim_dataset * new_dset ;
   int ival , ityp , nbytes , nvals ;
   void * new_brick , * old_brick ;

   /*-- sanity check --*/

   if( ! ISVALID_3DIM_DATASET(dset) ) return NULL ;

   /*-- make the empty copy --*/

   new_dset = EDIT_empty_copy( dset ) ;

   /*-- change its name? --*/

   if( new_prefix != NULL )
      EDIT_dset_items( new_dset ,
                          ADN_prefix , new_prefix ,
                          ADN_label1 , new_prefix ,
                       ADN_none ) ;

   /*-- make brick(s) for this dataset --*/

   THD_load_datablock( dset->dblk , NULL ) ;  /* make sure old one is in memory */

   nvals = DSET_NVALS(dset) ;

   for( ival=0 ; ival < nvals ; ival++ ){
      ityp      = DSET_BRICK_TYPE(new_dset,ival) ;   /* type of data */
      nbytes    = DSET_BRICK_BYTES(new_dset,ival) ;  /* how much data */
      new_brick = malloc( nbytes ) ;                 /* make room */

      if( new_brick == NULL ){
        THD_delete_3dim_dataset( new_dset , False ) ;
        return NULL ;
      }

      EDIT_substitute_brick( new_dset , ival , ityp , new_brick ) ;

      /*-- copy data from old brick to new brick --*/

      old_brick = DSET_BRICK_ARRAY(dset,ival) ;

      if( old_brick == NULL ){
         THD_delete_3dim_dataset( new_dset , False ) ;
         return NULL ;
      }

      memcpy( new_brick , old_brick , nbytes ) ;
   }

   return new_dset ;
}


/*---------------------------------------------------------------------------*/
/*
  Check whether output file already exists.
*/

void check_output_file 
(
  THD_3dim_dataset * dset,      /* input afni data set pointer */
  char * filename               /* output file name */
)

{
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int ierror;                         /* number of errors in editing data */
  
  
  /*-- make an empty copy of the input dataset --*/
  new_dset = EDIT_empty_copy( dset ) ;
  
  
  ierror = EDIT_dset_items( new_dset ,
			    ADN_prefix , filename ,
			    ADN_label1 , filename ,
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
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/
/*
  Main routine for this program.
  If the return string is not NULL, some error transpired, and
  the program will display the error message.
*/

#define FREEUP(x) do{if((x) != NULL){free((x)); (x)=NULL;}}while(0)

#define FREE_WORKSPACE                             \
  do{ FREEUP(bptr) ; FREEUP(sptr) ; FREEUP(fptr) ; \
      FREEUP(bbase); FREEUP(sbase); FREEUP(fbase); \
      FREEUP(bout) ; FREEUP(sout) ; FREEUP(fout) ; \
      FREEUP(dxar) ; FREEUP(dyar) ; FREEUP(phiar); \
    } while(0) ;


char * IMREG_main (IR_options * opt)
{
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   THD_3dim_dataset * base_dset;               /* base image dataset */
   char * new_prefix ;                         /* string from user */
   int base , ntime , datum , nx,ny,nz , ii,kk , npix ;
   float                      dx,dy,dz ;
   int base_datum;
   MRI_IMARR * ims_in , * ims_out ;
   MRI_IMAGE * im , * imbase ;

   byte   ** bptr = NULL , ** bbase = NULL, ** bout = NULL ;
   short  ** sptr = NULL , ** sbase = NULL, ** sout = NULL ; 
   float  ** fptr = NULL , ** fbase = NULL, ** fout = NULL ;

   float * dxar = NULL , * dyar = NULL , * phiar = NULL ;

   /*--------------------------------------------------------------------*/
   /*----- Check batch command inputs to see if they are reasonable -----*/

   old_dset = THD_open_one_dataset(opt->input_filename) ;   
                                                      /* get ptr to dataset */
   if( old_dset == NULL )
      return "*************************\n"
             "Cannot find Input Dataset\n"
             "*************************"  ;

   ntime = DSET_NUM_TIMES(old_dset) ;
   if( ntime < 2 )
      return "*****************************\n"
             "Dataset has only 1 time point\n"
             "*****************************"  ;

   ii = DSET_NVALS_PER_TIME(old_dset) ;
   if( ii > 1 )
      return "************************************\n"
             "Dataset has > 1 value per time point\n"
             "************************************"  ;

   nx = old_dset->daxes->nxx ; dx = old_dset->daxes->xxdel ;
   ny = old_dset->daxes->nyy ; dy = old_dset->daxes->yydel ; npix = nx*ny ;
   nz = old_dset->daxes->nzz ; dz = old_dset->daxes->zzdel ;

   if( nx != ny || fabs(dx) != fabs(dy) ){

     if (opt->debug)
       fprintf(stderr,"\nIMREG: nx=%d ny=%d nz=%d  dx=%f dy=%f dz=%f\n",
	       nx,ny,nz,dx,dy,dz ) ;

      return "***********************************\n"
             "Dataset does not have square slices\n"
             "***********************************"  ;
   }

   new_prefix = opt->new_prefix;     /* get string item (the output prefix) */
   if (new_prefix == NULL)           /* check if it is OK */
      return "************************\n"
             "Output Prefix is illegal\n"
             "************************"  ;

   /*----- Check whether output file already exists -----*/
   check_output_file (old_dset, new_prefix);


   /*--------- go to "base" input option ---------*/

   if (opt->base_filename == NULL)
     base_dset = old_dset;
   else
     {
       base_dset = THD_open_one_dataset(opt->base_filename) ;   
                                                  /* get ptr to base dataset */
       if( base_dset == NULL )
	 return "************************\n"
	        "Cannot find Base Dataset\n"
	        "************************"  ;

       if ( (nx != base_dset->daxes->nxx) || (dx != base_dset->daxes->xxdel)
	 || (ny != base_dset->daxes->nyy) || (dy != base_dset->daxes->yydel)
	 || (nz != base_dset->daxes->nzz) || (dz != base_dset->daxes->zzdel) )
	 {
	   if (opt->debug)
	       {
		 fprintf(stderr,"\nIMREG: Input Dataset:\n");
		 fprintf(stderr,"nx=%d ny=%d nz=%d  dx=%f dy=%f dz=%f\n",
		     nx,ny,nz,dx,dy,dz ) ;

		 fprintf(stderr,"\nIMREG: Base Dataset:\n");
		 fprintf(stderr,"nx=%d ny=%d nz=%d  dx=%f dy=%f dz=%f\n",
			 base_dset->daxes->nxx,   base_dset->daxes->nyy,
			 base_dset->daxes->nzz,   base_dset->daxes->xxdel,
			 base_dset->daxes->yydel, base_dset->daxes->zzdel) ;
	       }
	   return "*************************************************\n"
	          "Base Dataset is not compatible with Input Dataset\n"
	          "*************************************************"  ;
	 }
     }

   base_datum = DSET_BRICK_TYPE(base_dset,0);

   base = opt->base;
   if( base >= DSET_NUM_TIMES(base_dset))
      return "******************************\n"
             "Base image number is too large\n"
             "******************************"  ;


   /*--------- see if the "fine" input option is present --------*/
   if (opt->nofine)
     mri_align_params( 0 , 0.0,0.0,0.0 , 0.0,0.0,0.0 ) ;
   else{
      float fsig , fdxy , fdph ;
      fsig = opt->blur * 0.42466090;
      fdxy = opt->dxy;
      fdph = opt->dphi;
      mri_align_params( 0 , 0.0,0.0,0.0 , fsig,fdxy,fdph ) ;

      if (opt->debug)
	fprintf(stderr,"Set fine params = %f %f %f\n",fsig,fdxy,fdph) ; 
   }

   /*------------- ready to compute new dataset -----------*/

   if (opt->debug)
     fprintf(stderr,"IMREG: loading dataset\n") ;


   DSET_load( old_dset ) ;
   
   if (opt->base_filename != NULL)
     DSET_load (base_dset);

   /*** 1) Copy the dataset in toto ***/

   if (opt->debug)
     fprintf(stderr,"IMREG: Copying dataset\n") ;


   new_dset = copy_dset( old_dset , new_prefix ) ;
   if( new_dset == NULL )
      return "****************************\n"
             "Failed to copy input dataset\n"
             "****************************"  ;

   /*** 2) Make an array of empty images ***/

   if (opt->debug)
     fprintf(stderr,"IMREG: making empty images\n") ;


   datum = DSET_BRICK_TYPE(new_dset,0) ;

   INIT_IMARR(ims_in) ;
   for( ii=0 ; ii < ntime ; ii++ ){
      im = mri_new_vol_empty( nx , ny , 1 , datum ) ;
      ADDTO_IMARR(ims_in,im) ;
   }

   imbase = mri_new_vol_empty( nx , ny , 1 , base_datum ) ;

   dxar  = (float *) malloc( sizeof(float) * ntime ) ;
   dyar  = (float *) malloc( sizeof(float) * ntime ) ;
   phiar = (float *) malloc( sizeof(float) * ntime ) ;

   /*** 3) Get pointers to sub-bricks in old, base, and new datasets ***/

   if (opt->debug)
     fprintf(stderr,"IMREG: getting input brick pointers\n") ;


   switch( datum ){  /* pointer type depends on input datum type */
      case MRI_byte:
         bptr  = (byte **) malloc( sizeof(byte *) * ntime ) ;
         bout  = (byte **) malloc( sizeof(byte *) * ntime ) ;
         for( ii=0 ; ii < ntime ; ii++ ){
            bptr[ii]  = (byte *) DSET_ARRAY(old_dset,ii) ;
            bout[ii]  = (byte *) DSET_ARRAY(new_dset,ii) ;
         }
      break ;

      case MRI_short:
         sptr  = (short **) malloc( sizeof(short *) * ntime ) ;
         sout  = (short **) malloc( sizeof(short *) * ntime ) ;
         for( ii=0 ; ii < ntime ; ii++ ){
            sptr[ii]  = (short *) DSET_ARRAY(old_dset,ii) ;
            sout[ii]  = (short *) DSET_ARRAY(new_dset,ii) ;
         }

	 if (opt->debug)
	   fprintf(stderr,"IMREG: sptr[0] = %p  sout[0] = %p\n",
		   sptr[0],sout[0]) ;

      break ;

      case MRI_float:
         fptr  = (float **) malloc( sizeof(float *) * ntime ) ;
         fout  = (float **) malloc( sizeof(float *) * ntime ) ;
         for( ii=0 ; ii < ntime ; ii++ ){
            fptr[ii]  = (float *) DSET_ARRAY(old_dset,ii) ;
            fout[ii]  = (float *) DSET_ARRAY(new_dset,ii) ;
         }
      break ;
   }

   switch( base_datum ){  /* pointer type depends on base datum type */
      case MRI_byte:
	 bbase = (byte **) malloc( sizeof(byte *) * ntime ) ;
         for( ii=0 ; ii < ntime ; ii++ ){
	    bbase[ii] = (byte *) DSET_ARRAY(base_dset,ii) ; 
         }
      break ;

      case MRI_short:
	 sbase = (short **) malloc( sizeof(short *) * ntime ) ; 
         for( ii=0 ; ii < ntime ; ii++ ){
	    sbase[ii] = (short *) DSET_ARRAY(base_dset,ii) ;
         }

	 if (opt->debug)
	   fprintf(stderr,"IMREG: sbase[%d] = %p \n", base, sbase[base]) ;

      break ;

      case MRI_float:
	 fbase = (float **) malloc( sizeof(float *) * ntime ) ;
         for( ii=0 ; ii < ntime ; ii++ ){
	    fbase[ii] = (float *) DSET_ARRAY(base_dset,ii) ;
         }
      break ;
   }

   /*** 4) Loop over slices ***/

   for( kk=0 ; kk < nz ; kk++ ){

      /*** 4a) Setup ims_in images to point to input slices ***/

     if (opt->debug)
       fprintf(stderr,"IMREG: slice %d -- setup input images\n",kk) ;


      for( ii=0 ; ii < ntime ; ii++ ){
         im = IMARR_SUBIMAGE(ims_in,ii) ;
         switch( datum ){
            case MRI_byte:  
	      mri_fix_data_pointer( bptr[ii] + kk*npix, im ) ; break ;
            case MRI_short: 
	      mri_fix_data_pointer( sptr[ii] + kk*npix, im ) ; break ;
            case MRI_float: 
	      mri_fix_data_pointer( fptr[ii] + kk*npix, im ) ; break ;
         }
      }

      /*** 4b) Setup im to point to base image ***/

      if (opt->debug)
	fprintf(stderr,"IMREG: slice %d -- setup base image\n",kk) ;


      switch( datum ){
         case MRI_byte:  
	   mri_fix_data_pointer( bbase[base] + kk*npix, imbase ) ; break ;
         case MRI_short: 
	   mri_fix_data_pointer( sbase[base] + kk*npix, imbase ) ; break ;
         case MRI_float: 
	   mri_fix_data_pointer( fbase[base] + kk*npix, imbase ) ; break ;
      }

      /*** 4c) Register this slice at all times ***/

      if (opt->debug)
	fprintf(stderr,"IMREG: slice %d -- register\n",kk) ;


      ims_out = mri_align_dfspace( imbase , NULL , ims_in ,
                                   ALIGN_REGISTER_CODE , dxar,dyar,phiar ) ;

      if( ims_out == NULL )
	fprintf(stderr,"IMREG: mri_align_dfspace return NULL\n") ;

      /*** 4d) Put the output back in on top of the input;
               note that the output is always in MRI_float format ***/

      if (opt->debug)
	fprintf(stderr,"IMREG: slice %d -- put output back into dataset\n",kk);


      for( ii=0 ; ii < ntime ; ii++ ){
         switch( datum ){
           case MRI_byte:
              im = mri_to_mri( MRI_byte , IMARR_SUBIMAGE(ims_out,ii) ) ;
              memcpy( bout[ii] + kk*npix , MRI_BYTE_PTR(im) , 
		      sizeof(byte)*npix ) ;
              mri_free(im) ;
           break ;

           case MRI_short:

	     if (opt->debug)
	       if( ii==0 )
		 fprintf(stderr,"IMREG: conversion to short at ii=%d\n",ii) ;

              im = mri_to_mri( MRI_short , IMARR_SUBIMAGE(ims_out,ii) ) ;

	      if (opt->debug)
		if( ii==0 )
		  fprintf(stderr,"IMREG: copying to %p from %p\n",
			  sout[ii] + kk*npix,MRI_SHORT_PTR(im)) ;


              memcpy( sout[ii] + kk*npix , MRI_SHORT_PTR(im) , 
		      sizeof(short)*npix ) ;

	      if (opt->debug)
		if( ii==0 )
		  fprintf(stderr,"IMREG: freeing\n") ;


              mri_free(im) ;
           break ;

           case MRI_float:
              im = IMARR_SUBIMAGE(ims_out,ii) ;
              memcpy( fout[ii] + kk*npix , MRI_FLOAT_PTR(im) , 
		      sizeof(float)*npix ) ;
           break ;
         }
      }

      /*** 4e) Destroy the output images ***/

      if (opt->debug)
	fprintf(stderr,"IMREG: destroying aligned output\n") ;


      DESTROY_IMARR( ims_out ) ;
   }

   /*** 5) Destroy the empty images and other workspaces ***/

   if (opt->debug)
     fprintf(stderr,"IMREG: destroy workspaces\n") ;


   mri_clear_data_pointer(imbase) ; mri_free(imbase) ;
   for( ii=0 ; ii < ntime ; ii++ ){
      im = IMARR_SUBIMAGE(ims_in,ii) ;
      mri_clear_data_pointer(im) ;
   }
   DESTROY_IMARR(ims_in) ;
   FREE_WORKSPACE ;

   /*------------- write out the new dataset ------------*/

   if (opt->debug)
     fprintf(stderr,"IMREG: write new dataset to disk\n") ;


  THD_load_statistics( new_dset ) ;
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  

  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( old_dset , False ) ; old_dset = NULL ;
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  if (opt->base_filename != NULL)
    THD_delete_3dim_dataset( base_dset , False ) ; base_dset = NULL ;
    


   return NULL ;  /* null string returned means all was OK */
}


/*---------------------------------------------------------------------------*/

void main
(
  int argc,                    /* number of input arguments */
  char ** argv                 /* array of input arguments */ 
)

{
  IR_options * option_data;
  char * chptr;


  /*----- Identify software -----*/
  printf ("\n\nProgram %s \n", PROGRAM_NAME);
  printf ("Last revision: %s \n\n", LAST_MOD_DATE);

  
  /*----- Program initialization -----*/
  initialize_program (argc, argv, &option_data);


  /*----- Register all slices in the dataset -----*/
  chptr = IMREG_main (option_data);


  /*----- Check for processing errors -----*/
  if (chptr != NULL)   printf ("%s \n", chptr);

}
