#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to do what imreg.c does
************************************************************************/

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "Purpose: 2D (slice-wise) registration of 3D+time dataset\n"
  "[See also programs 'imreg' and '2dImReg']\n"
  "\n"
  "Input items to this plugin are:\n"
  "   Datasets:   Input  = 3D+time dataset to process\n"
  "               Output = Prefix for new dataset\n"
  "   Parameters: Base   = Time index for base image\n"
  "----\n"
  "This option is only for experimenting with the parameters\n"
  "of the final (fine) step of the registration algorithm:\n"
  "   Fine Fit:   Blur   = FWHM of blurring prior to registration\n"
  "               Dxy    = Convergence tolerance for translations\n"
  "               Dphi   = Convergence tolerance for rotations\n"
  "Author -- RW Cox"
;

/*----------------- prototypes for internal routines -----------------*/

char * IMREG_main( PLUGIN_interface * ) ;  /* the entry point */

/*---------------------------- global data ---------------------------*/

static PLUGIN_interface * global_plint = NULL ;

/***********************************************************************
   Set up the interface to the user:
    1) Create a new interface using "PLUTO_new_interface";

    2) For each line of inputs, create the line with "PLUTO_add_option"
         (this line of inputs can be optional or mandatory);

    3) For each item on the line, create the item with
        "PLUTO_add_dataset" for a dataset chooser,
        "PLUTO_add_string"  for a string chooser,
        "PLUTO_add_number"  for a number chooser.
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "2D Registration" ,
                                "2D Registration of a 3D+time Dataset" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , IMREG_main  ) ;

   PLUTO_add_hint( plint , "2D Registration of a 3D+time Dataset" ) ;

   global_plint = plint ;  /* make global copy */

   /*--------- 1st line ---------*/

   PLUTO_add_option( plint ,
                     "Datasets" ,  /* label at left of input line */
                     "DAtasets" ,  /* tag to return to plugin */
                     TRUE          /* is this mandatory? */
                   ) ;

   PLUTO_add_dataset(  plint ,
                       "Input" ,          /* label next to button   */
                       ANAT_ALL_MASK ,    /* take any anat datasets */
                       FUNC_FIM_MASK ,    /* only allow fim funcs   */
                       DIMEN_4D_MASK |    /* need 3D+time datasets  */
                       BRICK_ALLREAL_MASK /* need real-valued datasets */
                    ) ;

   PLUTO_add_string( plint ,
                     "Output" ,  /* label next to textfield */
                     0,NULL ,    /* no fixed strings to choose among */
                     19          /* 19 spaces for typing in value */
                   ) ;

   /*---------- 2nd line --------*/

   PLUTO_add_option( plint ,
                     "Parameters" ,  /* label at left of input line */
                     "Parameters" ,  /* tag to return to plugin */
                     TRUE            /* is this mandatory? */
                   ) ;

   PLUTO_add_number( plint ,
                     "Base" ,    /* label next to chooser */
                     0 ,         /* smallest possible value */
                     98 ,        /* largest possible value */
                     0 ,         /* decimal shift (none in this case) */
                     3 ,         /* default value */
                     FALSE       /* allow user to edit value? */
                   ) ;

   /*---------- 3rd line --------*/

   PLUTO_add_option( plint ,
                     "Fine Fit" ,
                     "Fine Fit" ,
                     FALSE
                   ) ;

   PLUTO_add_number( plint , "Blur" , 0 , 40 , 1 , 10 , FALSE ) ;
   PLUTO_add_number( plint , "Dxy"  , 1 , 20 , 2 ,  7 , FALSE ) ;
   PLUTO_add_number( plint , "Dphi" , 1 , 50 , 2 , 21 , FALSE ) ;

   /*--------- done with interface setup ---------*/

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

#define FREEUP(x) do{if((x) != NULL){free((x)); (x)=NULL;}}while(0)

#define FREE_WORKSPACE                             \
  do{ FREEUP(bptr) ; FREEUP(sptr) ; FREEUP(fptr) ; \
      FREEUP(bout) ; FREEUP(sout) ; FREEUP(fout) ; \
      FREEUP(dxar) ; FREEUP(dyar) ; FREEUP(phiar); \
    } while(0) ;


char * IMREG_main( PLUGIN_interface * plint )
{
   MCW_idcode * idc ;                          /* input dataset idcode */
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   char * new_prefix , * str ;                 /* strings from user */
   int base , ntime , datum , nx,ny,nz , ii,kk , npix ;
   float                      dx,dy,dz ;
   MRI_IMARR * ims_in , * ims_out ;
   MRI_IMAGE * im , * imbase ;

   byte   ** bptr = NULL , ** bout = NULL ;
   short  ** sptr = NULL , ** sout = NULL ;
   float  ** fptr = NULL , ** fout = NULL ;

   float * dxar = NULL , * dyar = NULL , * phiar = NULL ;

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   /*--------- go to first input line ---------*/

   PLUTO_next_option(plint) ;

   idc      = PLUTO_get_idcode(plint) ;   /* get dataset item */
   old_dset = PLUTO_find_dset(idc) ;      /* get ptr to dataset */
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

#ifdef IMREG_DEBUG
fprintf(stderr,"\nIMREG: nx=%d ny=%d nz=%d  dx=%f dy=%f dz=%f\n",
        nx,ny,nz,dx,dy,dz ) ;
#endif

      return "***********************************\n"
             "Dataset does not have square slices\n"
             "***********************************"  ;
   }

   new_prefix = PLUTO_get_string(plint) ;   /* get string item (the output prefix) */
   if( ! PLUTO_prefix_ok(new_prefix) )      /* check if it is OK */
      return "************************\n"
             "Output Prefix is illegal\n"
             "************************"  ;

   /*--------- go to next input line ---------*/

   PLUTO_next_option(plint) ;

   base = PLUTO_get_number(plint) ;
   if( base >= ntime )
      return "********************\n"
             "Base value too large\n"
             "********************"  ;

   /*--------- see if the 3rd option line is present --------*/

   str = PLUTO_get_optiontag( plint ) ;
   if( str != NULL ){
      float fsig , fdxy , fdph ;
      fsig = PLUTO_get_number(plint) * 0.42466090 ;
      fdxy = PLUTO_get_number(plint) ;
      fdph = PLUTO_get_number(plint) ;
      mri_align_params( 0 , 0.0,0.0,0.0 , fsig,fdxy,fdph ) ;
/* fprintf(stderr,"Set fine params = %f %f %f\n",fsig,fdxy,fdph) ; */
   }

   /*------------- ready to compute new dataset -----------*/

#ifdef IMREG_DEBUG
fprintf(stderr,"IMREG: loading dataset\n") ;
#endif

   DSET_load( old_dset ) ;

   /*** 1) Copy the dataset in toto ***/

#ifdef IMREG_DEBUG
fprintf(stderr,"IMREG: Copying dataset\n") ;
#endif

   new_dset = PLUTO_copy_dset( old_dset , new_prefix ) ;
   if( new_dset == NULL )
      return "****************************\n"
             "Failed to copy input dataset\n"
             "****************************"  ;

   /*** 2) Make an array of empty images ***/

#ifdef IMREG_DEBUG
fprintf(stderr,"IMREG: making empty images\n") ;
#endif

   datum = DSET_BRICK_TYPE(new_dset,0) ;

   INIT_IMARR(ims_in) ;
   for( ii=0 ; ii < ntime ; ii++ ){
      im = mri_new_vol_empty( nx , ny , 1 , datum ) ;
      ADDTO_IMARR(ims_in,im) ;
   }

   imbase = mri_new_vol_empty( nx , ny , 1 , datum ) ;

   dxar  = (float *) malloc( sizeof(float) * ntime ) ;
   dyar  = (float *) malloc( sizeof(float) * ntime ) ;
   phiar = (float *) malloc( sizeof(float) * ntime ) ;

   /*** 3) Get pointers to sub-bricks in old and new datasets ***/

#ifdef IMREG_DEBUG
fprintf(stderr,"IMREG: getting input brick pointers\n") ;
#endif

   switch( datum ){  /* pointer type depends on input datum type */
      case MRI_byte:
         bptr = (byte **) malloc( sizeof(byte *) * ntime ) ;
         bout = (byte **) malloc( sizeof(byte *) * ntime ) ;
         for( ii=0 ; ii < ntime ; ii++ ){
            bptr[ii] = (byte *) DSET_ARRAY(old_dset,ii) ;
            bout[ii] = (byte *) DSET_ARRAY(new_dset,ii) ;
         }
      break ;

      case MRI_short:
         sptr = (short **) malloc( sizeof(short *) * ntime ) ;
         sout = (short **) malloc( sizeof(short *) * ntime ) ;
         for( ii=0 ; ii < ntime ; ii++ ){
            sptr[ii] = (short *) DSET_ARRAY(old_dset,ii) ;
            sout[ii] = (short *) DSET_ARRAY(new_dset,ii) ;
         }

#ifdef IMREG_DEBUG
fprintf(stderr,"IMREG: sptr[0] = %p  sout[0] = %p\n",sptr[0],sout[0]) ;
#endif

      break ;

      case MRI_float:
         fptr = (float **) malloc( sizeof(float *) * ntime ) ;
         fout = (float **) malloc( sizeof(float *) * ntime ) ;
         for( ii=0 ; ii < ntime ; ii++ ){
            fptr[ii] = (float *) DSET_ARRAY(old_dset,ii) ;
            fout[ii] = (float *) DSET_ARRAY(new_dset,ii) ;
         }
      break ;
   }

   /*** 4) Loop over slices ***/

   PLUTO_popup_meter(plint) ;

   for( kk=0 ; kk < nz ; kk++ ){

      /*** 4a) Setup ims_in images to point to input slices ***/

#ifdef IMREG_DEBUG
fprintf(stderr,"IMREG: slice %d -- setup input images\n",kk) ;
#endif

      for( ii=0 ; ii < ntime ; ii++ ){
         im = IMARR_SUBIMAGE(ims_in,ii) ;
         switch( datum ){
            case MRI_byte:  mri_fix_data_pointer( bptr[ii] + kk*npix, im ) ; break ;
            case MRI_short: mri_fix_data_pointer( sptr[ii] + kk*npix, im ) ; break ;
            case MRI_float: mri_fix_data_pointer( fptr[ii] + kk*npix, im ) ; break ;
         }
      }

      /*** 4b) Setup im to point to base image ***/

#ifdef IMREG_DEBUG
fprintf(stderr,"IMREG: slice %d -- setup base image\n",kk) ;
#endif

      switch( datum ){
         case MRI_byte:  mri_fix_data_pointer( bptr[base] + kk*npix, imbase ) ; break ;
         case MRI_short: mri_fix_data_pointer( sptr[base] + kk*npix, imbase ) ; break ;
         case MRI_float: mri_fix_data_pointer( fptr[base] + kk*npix, imbase ) ; break ;
      }

      /*** 4c) Register this slice at all times ***/

#ifdef IMREG_DEBUG
fprintf(stderr,"IMREG: slice %d -- register\n",kk) ;
#endif

      ims_out = mri_align_dfspace( imbase , NULL , ims_in ,
                                   ALIGN_REGISTER_CODE , dxar,dyar,phiar ) ;

if( ims_out == NULL )
   fprintf(stderr,"IMREG: mri_align_dfspace return NULL\n") ;

      /*** 4d) Put the output back in on top of the input;
               note that the output is always in MRI_float format ***/

#ifdef IMREG_DEBUG
fprintf(stderr,"IMREG: slice %d -- put output back into dataset\n",kk) ;
#endif

      for( ii=0 ; ii < ntime ; ii++ ){
         switch( datum ){
           case MRI_byte:
              im = mri_to_mri( MRI_byte , IMARR_SUBIMAGE(ims_out,ii) ) ;
              memcpy( bout[ii] + kk*npix , MRI_BYTE_PTR(im) , sizeof(byte)*npix ) ;
              mri_free(im) ;
           break ;

           case MRI_short:
#ifdef IMREG_DEBUG
if( ii==0 )fprintf(stderr,"IMREG: conversion to short at ii=%d\n",ii) ;
#endif

              im = mri_to_mri( MRI_short , IMARR_SUBIMAGE(ims_out,ii) ) ;

#ifdef IMREG_DEBUG
if( ii==0 )fprintf(stderr,"IMREG: copying to %p from %p\n",sout[ii] + kk*npix,MRI_SHORT_PTR(im)) ;
#endif

              memcpy( sout[ii] + kk*npix , MRI_SHORT_PTR(im) , sizeof(short)*npix ) ;

#ifdef IMREG_DEBUG
if( ii==0 )fprintf(stderr,"IMREG: freeing\n") ;
#endif

              mri_free(im) ;
           break ;

           case MRI_float:
              im = IMARR_SUBIMAGE(ims_out,ii) ;
              memcpy( fout[ii] + kk*npix , MRI_FLOAT_PTR(im) , sizeof(float)*npix ) ;
           break ;
         }
      }

      PLUTO_set_meter(plint, (100*(kk+1))/nz ) ;

      /*** 4e) Destroy the output images ***/

#ifdef IMREG_DEBUG
fprintf(stderr,"IMREG: destroying aligned output\n") ;
#endif

      DESTROY_IMARR( ims_out ) ;
   }

   /*** 5) Destroy the empty images and other workspaces ***/

#ifdef IMREG_DEBUG
fprintf(stderr,"IMREG: destroy workspaces\n") ;
#endif

   mri_clear_data_pointer(imbase) ; mri_free(imbase) ;
   for( ii=0 ; ii < ntime ; ii++ ){
      im = IMARR_SUBIMAGE(ims_in,ii) ;
      mri_clear_data_pointer(im) ;
   }
   DESTROY_IMARR(ims_in) ;
   FREE_WORKSPACE ;

   /*------------- let AFNI know about the new dataset ------------*/

#ifdef IMREG_DEBUG
fprintf(stderr,"IMREG: send result to AFNI\n") ;
#endif

   PLUTO_add_dset( plint , new_dset , DSET_ACTION_MAKE_CURRENT ) ;

   return NULL ;  /* null string returned means all was OK */
}
