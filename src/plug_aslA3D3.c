/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to compute Functional Imageof a 3D+time dataset.
  This is a moderately complex example, showing how to deal
  with different data types on input and output.
************************************************************************/

/*------------- string to 'help' the user -------------*/

static char helpstring[] =
  " Purpose: Compute 'ASL a3/d3' of a 3D+time dataset.\n"
  " Input items are:\n"
  "   Input = 3D+time dataset to analyze\n"
  "\n"
  "   Output: Prefix = Filename prefix for new dataset\n"
  "           Datum  = How to store results \n"
  "\n"
  "   Ignore Count:    How many time points to ignore at start\n"
  " \n"
  "   Scale:  Scale the a3, d3 calculations\n"
  " \n"
  "   Notes: \n"
  "    a3- three point moving average of the time series (BOLD) \n"
  "                    a3(i)=image(i-1)+2*image(i)+image(i+1) \n"
  "    d3- three point moving difference of the time series (Perfusion) \n"
  "         d3(i)=(-1*i)*(image(i-1)-2*image(i)+1*image(i+1)) \n"
  "\n"
  "    Datum Type- be aware that rounding errors associated with scaling to \n"
  "                byte/short can occur \n"
  "   \n"
  "\n"
  " ASL A3/D3 Version 1.3 Written by Y.Behzadi 8/18/02\n"
  " fixed problem with odd # of time points and sign\n"
  "\n"

;

/*------------- strings for output format -------------*/

static char * type_strings[]
  = { "Float" ,  "Byte" , "Short" , "as Input"  } ;

static char * type_stringsx[]
  = { "a3,d3,avg(d3)" } ;

#define NUM_TYPE_STRINGS (sizeof(type_strings)/sizeof(char *))
#define NUM_TYPE_STRINGSX (sizeof(type_stringsx)/sizeof(char *))

/*--------------- prototypes for internal routines ---------------*/

char * POWER_main( PLUGIN_interface * ) ;  /* the entry point */

#undef ALLOW_TESTING
#ifdef ALLOW_TESTING
PLUGIN_interface * TEST_init(void) ;
char * TEST_main( PLUGIN_interface * ) ;  /* the entry point */
#endif

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

   if( ncall > 1 ) return NULL ;  /* two interfaces */
   CHECK_IF_ALLOWED("ASL","ASL a3/d3") ;  /* 30 Sep 2016 */

#ifdef ALLOW_TESTING
   if( ncall == 1 ) return TEST_init() ;
#else
   if( ncall == 1 ) return NULL ;
#endif

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "ASL a3/d3" ,
                                "ASL a3/d3 of a 3D+time Dataset" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , POWER_main  ) ;

   PLUTO_add_hint( plint , "ASL a3/d3 of a 3D+time Dataset" ) ;

   PLUTO_set_sequence( plint , "A:newdset:statistics" ) ;

   /*--------- 1st line: Input dataset ---------*/

   PLUTO_add_option( plint ,
                     "Input" ,  /* label at left of input line */
                     "Input" ,  /* tag to return to plugin */
                     TRUE       /* is this mandatory? */
                   ) ;

   PLUTO_add_dataset(  plint ,
                       "---->>" ,         /* label next to button   */
                       ANAT_ALL_MASK ,    /* take any anat datasets */
                       FUNC_FIM_MASK ,    /* only allow fim funcs   */
                       DIMEN_4D_MASK |    /* need 3D+time datasets  */
                       BRICK_ALLREAL_MASK /* need real-valued datasets */
                    ) ;

   /*---------- 2nd line: Output dataset ----------*/

   PLUTO_add_option( plint ,
                     "Output" ,  /* label at left of input line */
                     "Output" ,  /* tag to return to plugin */
                     TRUE        /* is this mandatory? */
                   ) ;

   PLUTO_add_string(   plint ,
                       "Prefix" ,  /* label next to textfield */
                       0,NULL ,    /* no fixed strings to choose among */
                       19          /* 19 spaces for typing in value */
                   ) ;

   PLUTO_add_string(   plint ,
                       "Datum" ,          /* label next to chooser button */
                       NUM_TYPE_STRINGS , /* number of strings to choose among */
                       type_strings ,     /* list of strings to choose among */
                       0                  /* index of default string */
                   ) ;

   /*--------- Other lines: Parameters ---------*/

   PLUTO_add_option( plint , "Ignore" , "Ignore" , TRUE ) ;

   PLUTO_add_number( plint ,
                     "Count" ,   /* label next to chooser */
                     0 ,         /* smallest possible value */
                     999 ,       /* largest possible value */
                     0 ,         /* decimal shift (none in this case) */
                     4 ,         /* default value */
                     TRUE        /* allow user to edit value? */
                   ) ;
   PLUTO_add_string(   plint ,
                       "Image Output" ,     /* label next to chooser button */
                       NUM_TYPE_STRINGSX , /* number of strings to choose among */
                       type_stringsx ,     /* list of strings to choose among */
                       0                  /* index of default string */
                   ) ;

   PLUTO_add_option( plint , "Scale" , "Scale" , TRUE ) ;

   PLUTO_add_number( plint ,
                     "Scale Factor" ,   /* label next to chooser */
                     0 ,         /* smallest possible value */
                     999 ,       /* largest possible value */
                     0 ,         /* decimal shift (none in this case) */
                     1 ,         /* default value */
                     TRUE        /* allow user to edit value? */
                   ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

/*------------------ macros to return workspace at exit -------------------*/

#undef  FREEUP
#define FREEUP(x) do{ if((x) != NULL){free((x)); (x)=NULL;} } while(0)

#define FREE_WORKSPACE  do{ FREEUP(bptr) ;      \
                            FREEUP(sptr) ;      \
                            FREEUP(fptr) ;      \
                            FREEUP(foutD3) ;    \
                            FREEUP(this) ;      \
                            FREEUP(tarD3) ;     \
                            FREEUP(tarA3) ;     \
                            FREEUP(taravgD3) ;  \
                            FREEUP(foutA3) ;    \
                            FREEUP(foutavgD3) ; \
                        } while(0)

/*-------------------------------------------------------------------------*/

char * POWER_main( PLUGIN_interface * plint )
{
   MCW_idcode * idc ;                          /* input dataset idcode */
   THD_3dim_dataset * old_dset , * new_dsetD3 , * new_dsetA3, * new_dsetavgD3 ; /* input and output datasets */
   char * new_prefix , * str , * namestr, * filename;                 /* strings from user */
   int   new_datum , ignore , nfft , ninp ,    /* control parameters */
         old_datum , nuse , ntaper , ktbot,
         image_type, scale,OutputFlag ,numT,flip;
  float avFac;

   byte   ** bptr  = NULL ;  /* one of these will be the array of */
   short  ** sptr  = NULL ;  /* pointers to input dataset sub-bricks */
   float  ** fptr  = NULL ;  /* (depending on input datum type) */



   float   * this  = NULL ;  /* array loaded from input dataset */


   float  ** foutD3  = NULL ;  /* will be array of output floats */
   float  ** foutA3  = NULL ;  /* will be array of output floats */
   float  ** foutavgD3  = NULL ;  /* will be array of output floats */

   float   * tarD3   = NULL ;  /* will be array of taper coefficients */
   float   * tarA3   = NULL ;  /* will be array of taper coefficients */
   float   * taravgD3   = NULL ;  /* will be array of taper coefficients */


   /*float   * flip;*/
   float   * numAv;
   float dfreq , pfact , phi , xr,xi , yr,yi ;
   float x0,x1 , y0,y1 , d0fac,d1fac ;
   int   nfreq , nvox , perc , new_units ;
   int   istr , ii,iip , ibot,itop , kk , icx ;       /* temp variables */

   new_prefix = (char *)calloc(100, sizeof(char));
   filename = (char *)calloc(100, sizeof(char));
   str = (char *)calloc(100, sizeof(char));
   namestr = (char *)calloc(100, sizeof(char));
   OutputFlag=0;
   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   /*--------- go to first input line ---------*/

   PLUTO_next_option(plint) ;

   idc      = PLUTO_get_idcode(plint) ;   /* get dataset item */
   old_dset = PLUTO_find_dset(idc) ;      /* get ptr to dataset */
   namestr  = DSET_PREFIX(old_dset) ;


   if( old_dset == NULL )
      return "*************************\n"
             "Cannot find Input Dataset\n"
             "*************************"  ;

   /*--------- go to second input line ---------*/

   PLUTO_next_option(plint) ;

  filename = PLUTO_get_string(plint) ;   /* get string item (the output prefix) */

  sprintf(new_prefix,"%s%s",filename,"_D3");

  if (strcmp(new_prefix,"_D3")==0){
     OutputFlag=1;
     sprintf(new_prefix,"%s%s",namestr,"_D3");
  }


   if (! PLUTO_prefix_ok(new_prefix) ){
     PLUTO_popup_transient(plint,new_prefix);
     return "*************************\n"
             "Output filename already exists\n"
             "*************************"  ;
     }


   PLUTO_popup_transient(plint,"Output file tags set automatically");


   str  = PLUTO_get_string(plint) ;              /* get string item (the datum type) */
   istr = PLUTO_string_index( str ,              /* find it in the list it came from */
                              NUM_TYPE_STRINGS ,
                              type_strings ) ;
   switch( istr ){
      default:
      case 0:
         new_datum = MRI_float ; break ;
	 break ;

      case 1: new_datum = MRI_byte  ; break ;  /* assign type of user's choice */
      case 2: new_datum = MRI_short ; break ;
      case 3: new_datum = DSET_BRICK_TYPE( old_dset , 0 ) ;  /* use old dataset type */
   }

  /*--------- go to next input lines ---------*/

   PLUTO_next_option(plint) ;                 /* skip to next line */
   ignore = PLUTO_get_number(plint) ;         /* get number item (ignore) */




   ninp = DSET_NUM_TIMES(old_dset) ;   /* number of values in input */
   nuse = ninp;              /* number of values to actually use */
   nfreq=nuse;
   nfft=nuse;


   str  = PLUTO_get_string(plint) ;              /* get string item (the datum type) */
   istr = PLUTO_string_index( str ,              /* find it in the list it came from */
                              NUM_TYPE_STRINGSX ,
                              type_stringsx ) ;
   switch( istr ){
      default:
      case 0: image_type = 0; break;
           }

  PLUTO_next_option(plint) ;                 /* skip to next line */
  scale = PLUTO_get_number(plint) ;         /* get number item (scale) */


   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   PLUTO_popup_meter( plint ) ;  /* popup a progress meter */

   /*--------- set up pointers to each sub-brick in the input dataset ---------*/

   DSET_load( old_dset ) ;  /* must be in memory before we get pointers to it */

   old_datum = DSET_BRICK_TYPE( old_dset , 0 ) ; /* get old dataset datum type */

   switch( old_datum ){  /* pointer type depends on input datum type */

      default:
         return "******************************\n"
                "Illegal datum in Input Dataset\n"
                "******************************"  ;

      /** create array of pointers into old dataset sub-bricks **/
      /** Note that we skip the first 'ignore' sub-bricks here **/

      /*--------- input is bytes ----------*/
      /* voxel #i at time #k is bptr[k][i] */
      /* for i=0..nvox-1 and k=0..nuse-1.  */

      case MRI_byte:
         bptr = (byte **) malloc( sizeof(byte *) * nuse ) ;
         if( bptr == NULL ) return "Malloc\nFailure!\n [bptr]" ;
         for( kk=0 ; kk < nuse ; kk++ )
            bptr[kk] = (byte *) DSET_ARRAY(old_dset,kk) ;
      break ;

      /*--------- input is shorts ---------*/
      /* voxel #i at time #k is sptr[k][i] */
      /* for i=0..nvox-1 and k=0..nuse-1.  */

      case MRI_short:
         sptr = (short **) malloc( sizeof(short *) * nuse ) ;
         if( sptr == NULL ) return "Malloc\nFailure!\n [sptr]" ;
         for( kk=0 ; kk < nuse ; kk++ )
            sptr[kk] = (short *) DSET_ARRAY(old_dset,kk) ;
      break ;

      /*--------- input is floats ---------*/
      /* voxel #i at time #k is fptr[k][i] */
      /* for i=0..nvox-1 and k=0..nuse-1.  */

      case MRI_float:
         fptr = (float **) malloc( sizeof(float *) * nuse ) ;
         if( fptr == NULL ) return "Malloc\nFailure!\n [fptr]" ;
         for( kk=0 ; kk < nuse ; kk++ )
            fptr[kk] = (float *) DSET_ARRAY(old_dset,kk) ;
      break ;

   } /* end of switch on input type */

   /*---- allocate space for 2 voxel timeseries and 1 FFT ----*/



   this = (float *)   malloc( sizeof(float) * nuse ) ;   /* input */
   tarD3 = (float *) malloc( sizeof(float) * MAX(nuse,nfreq) ) ;
   tarA3 = (float *) malloc( sizeof(float) * MAX(nuse,nfreq) ) ;
   taravgD3 = (float *) malloc( sizeof(float) * MAX(nuse,nfreq) ) ;
   /*flip = (float *)malloc( sizeof(float) * 1);*/
   numAv = (float *)malloc( sizeof(float) * 1);


  numT=nuse-ignore;

  if (OutputFlag==1)
  sprintf(new_prefix,"%s%s",namestr,"_D3");
  else
  sprintf(new_prefix,"%s%s",filename,"_D3");

  new_dsetD3 = EDIT_empty_copy( old_dset );

  { char * his = PLUTO_commandstring(plint) ;
  tross_Copy_History( old_dset , new_dsetD3 ) ;
  tross_Append_History( new_dsetD3 , his ) ; free(his) ;
  }

   	/*-- edit some of its internal parameters --*/

  ii = EDIT_dset_items(
       new_dsetD3 ,
         ADN_prefix      , new_prefix ,           /* filename prefix */
         ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
         ADN_datum_all   , new_datum ,            /* atomic datum */
	 ADN_nvals	      , numT ,
	 ADN_ntt	,numT,
         ADN_none ) ;



  if (OutputFlag==1)
  sprintf(new_prefix,"%s%s",namestr,"_A3");
  else
  sprintf(new_prefix,"%s%s",filename,"_A3");

  numT=nuse-ignore;
  new_dsetA3 = EDIT_empty_copy( old_dset );

  { char * his = PLUTO_commandstring(plint) ;
  tross_Copy_History( old_dset , new_dsetA3 ) ;
  tross_Append_History( new_dsetA3 , his ) ; free(his) ;
  }

   	/*-- edit some of its internal parameters --*/

  ii = EDIT_dset_items(
       new_dsetA3 ,
         ADN_prefix      , new_prefix ,           /* filename prefix */
         ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
         ADN_datum_all   , new_datum ,            /* atomic datum */
	 ADN_nvals	      , numT,
	 ADN_ntt	,numT,
         ADN_none ) ;



  if (OutputFlag==1)
  sprintf(new_prefix,"%s%s",namestr,"_avgD3");
  else
  sprintf(new_prefix,"%s%s",filename,"_avgD3");

  new_dsetavgD3 = EDIT_empty_copy( old_dset );

  { char * his = PLUTO_commandstring(plint) ;
  tross_Copy_History( old_dset , new_dsetavgD3 ) ;
  tross_Append_History( new_dsetavgD3 , his ) ; free(his) ;
  }

   	/*-- edit some of its internal parameters --*/

  ii = EDIT_dset_items(
        new_dsetavgD3 ,
          ADN_prefix      , new_prefix ,           /* filename prefix */
          ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
          ADN_datum_all   , new_datum ,            /* atomic datum */
	  ADN_nvals	      , 1,
	  ADN_ntt	,1,
          ADN_none ) ;





   /*---------------------- make a new dataset ----------------------*/

/*-------------------making a new dataset------------------------------------*/





   /*------ make floating point output sub-bricks
            (only at the end will scale to byte or shorts)

            Output #ii at freq #kk will go into fout[kk][ii],
            for kk=0..nfreq-1, and for ii=0..nvox-1.          ------*/

   nvox = old_dset->daxes->nxx * old_dset->daxes->nyy * old_dset->daxes->nzz ;

   foutD3 = (float **) malloc( sizeof(float *) * nuse ) ;  /* ptrs to sub-bricks */
   foutA3 = (float **) malloc( sizeof(float *) * nuse ) ;  /* ptrs to sub-bricks */
   foutavgD3 = (float **) malloc( sizeof(float *) * 1 ) ;  /* ptrs to sub-bricks */


   if( foutD3 == NULL | foutA3 == NULL | foutavgD3 == NULL){
      THD_delete_3dim_dataset( new_dsetD3 , False ) ;
      THD_delete_3dim_dataset( new_dsetA3 , False ) ;
      THD_delete_3dim_dataset( new_dsetavgD3 , False ) ;
      FREE_WORKSPACE ;
      return "Malloc\nFailure!\n [fout]" ;
   }

   for( kk=0 ; kk < nfreq ; kk++ ){
      foutD3[kk] = (float *) malloc( sizeof(float) * nvox ) ; /* sub-brick # kk */
      foutA3[kk] = (float *) malloc( sizeof(float) * nvox ) ; /* sub-brick # kk */
      foutavgD3[0] = (float *) malloc( sizeof(float) * nvox ) ; /* sub-brick # kk */
      if( foutD3[kk] == NULL ) break ;
      if( foutA3[kk] == NULL ) break ;
      if( foutavgD3[0] == NULL ) break ;
   }

   if( kk < nfreq ){
      for( ; kk >= 0 ; kk-- ){
       FREEUP(foutD3[kk]) ;
       FREEUP(foutA3[kk]) ;
       FREEUP(foutavgD3[0]) ;
       }/* free all we did get */
      THD_delete_3dim_dataset( new_dsetD3 , False ) ;
      THD_delete_3dim_dataset( new_dsetA3 , False ) ;
      THD_delete_3dim_dataset( new_dsetavgD3 , False ) ;
      FREE_WORKSPACE ;
      return "Malloc\nFailure!\n [arrays]" ;
   }

   { char buf[128] ;
     ii = (nfreq * nvox * sizeof(float)) / (1024*1024) ;
     sprintf( buf , "  \n"
                    "*** 3D+time ASL a3/d3:\n"
                    "*** Using %d MBytes of workspace,\n "
                    "*** with # time points = %d\n" , ii,numT ) ;
     PLUTO_popup_transient( plint , buf ) ;
   }

   /*----------------------------------------------------*/
   /*----- Setup has ended.  Now do some real work. -----*/

   /***** loop over voxels *****/

/* *(flip)=scale; */

*(numAv)= nuse-ignore;

   for( ii=0 ; ii < nvox ; ii ++ ){  /* time series */

      switch( old_datum ){

	case MRI_byte:
            for( kk=0 ; kk < nuse ; kk++ ){
            	this[kk] =  bptr[kk][ii] ;
             }

         break ;

         case MRI_short:
            for( kk=0 ; kk < nuse ; kk++ ){
             this[kk] =  sptr[kk][ii] ;

            }
         break ;

         case MRI_float:
            for( kk=0 ; kk < nuse ; kk++ ){
             this[kk] =  fptr[kk][ii] ;

            }

         break ;
      }

      flip=scale*pow(-1,ignore+1);

      for( kk=0 ; kk < nuse-ignore ; kk++ ){

      		if (kk==nuse-1-ignore){
        		*(*(foutD3+kk)+ii)=
			flip*( *(this+kk+ignore-1)-*(this+kk+ignore) );

			*(*(foutA3+kk)+ii)=
			2*(*(this+kk+ignore-1)+*(this+kk+ignore));


			}
		else if (kk==0){
						/*D3 tag - control*/
        		*(*(foutD3+kk)+ii)=
			flip*( *(this+kk+ignore)-*(this+kk+ignore+1) );

			*(*(foutA3+kk)+ii)=
			2*(*(this+kk+ignore)+*(this+kk+ignore+1));

			}

		else{
			*(*(foutD3+kk)+ii)=
			flip*( 1*(*(this+kk+ignore-1))+-2*(*(this+kk+ignore))+1*(*(this+kk+ignore+1)) );

			*(*(foutA3+kk)+ii)=
			((*(this+kk+ignore-1))+2*(*(this+kk+ignore))+(*(this+kk+ignore+1)));

			flip=-1*flip;


			}


	}



      for( kk=0 ; kk < nuse-ignore ; kk++ )
     *(*(foutavgD3)+ii)= *(*(foutavgD3)+ii)+(*(*(foutD3+kk)+ii));

     *(*(foutavgD3)+ii)=*(*(foutavgD3)+ii) / (*(numAv));


      }

   DSET_unload( old_dset ) ;  /* don't need this no more */

   switch( new_datum ){

      /*** output is floats is the simplest:
           we just have to attach the fout bricks to the dataset ***/

      case MRI_float:
         for( kk=0 ; kk < nuse-ignore ; kk++ )
            EDIT_substitute_brick( new_dsetD3 , kk , MRI_float , foutD3[kk] ) ;
      break ;

      /*** output is shorts:
           we have to create a scaled sub-brick from fout ***/

      case MRI_short:{
         short * boutD3 ;
         float facD3 ;

         for( kk=0 ; kk < nuse-ignore ; kk++ ){  /* loop over sub-bricks */

            /*-- get output sub-brick --*/

            boutD3 = (short *) malloc( sizeof(short) * nvox ) ;
            if( boutD3 == NULL ){
               fprintf(stderr,"\nFinal malloc error in plug_power!\n\a") ;
               EXIT(1) ;
            }

            /*-- find scaling and then scale --*/

            facD3  = MCW_vol_amax( nvox,1,1 , MRI_float , foutD3[kk] ) ;
            if( facD3  > 0.0 ){
               facD3  = 32767.0 / facD3  ;
               EDIT_coerce_scale_type( nvox,facD3  ,
                                       MRI_float,foutD3[kk] , MRI_short,boutD3  ) ;
               facD3  = 1.0 / facD3  ;
            }

            free( foutD3[kk] ) ;  /* don't need this anymore */

            /*-- put output brick into dataset, and store scale factor --*/

            EDIT_substitute_brick( new_dsetD3 , kk , MRI_short , boutD3  ) ;
            tarD3 [kk] = facD3  ;


         }

         /*-- save scale factor array into dataset --*/

         EDIT_dset_items( new_dsetD3 , ADN_brick_fac , tarD3  , ADN_none ) ;

      }
      break ;

      /*** output is bytes (byte = unsigned char)
           we have to create a scaled sub-brick from fout ***/

      case MRI_byte:{
         byte * boutD3  ;
         float facD3  ;

         for( kk=0 ; kk < nuse-ignore ; kk++ ){  /* loop over sub-bricks */

            /*-- get output sub-brick --*/

            boutD3  = (byte *) malloc( sizeof(byte) * nvox ) ;
            if( boutD3  == NULL ){
               fprintf(stderr,"\nFinal malloc error in plug_power!\n\a") ;
               EXIT(1) ;
            }

            /*-- find scaling and then scale --*/

            facD3  = MCW_vol_amax( nvox,1,1 , MRI_float , foutD3[kk] ) ;
            if( facD3  > 0.0 ){
               facD3  = 255.0 / facD3  ;
               EDIT_coerce_scale_type( nvox,facD3  ,
                                       MRI_float,foutD3[kk] , MRI_byte,boutD3 ) ;
               facD3 = 1.0 / facD3  ;
            }

            free( foutD3[kk] ) ;  /* don't need this anymore */

            /*-- put output brick into dataset, and store scale factor --*/

            EDIT_substitute_brick( new_dsetD3 , kk , MRI_byte , boutD3  ) ;
            tarD3 [kk] = facD3  ;


         }

         /*-- save scale factor array into dataset --*/

         EDIT_dset_items( new_dsetD3 , ADN_brick_fac , tarD3  , ADN_none ) ;
      }
      break ;

   } /* end of switch on output data type */


   switch( new_datum ){

      /*** output is floats is the simplest:
           we just have to attach the fout bricks to the dataset ***/

      case MRI_float:
         for( kk=0 ; kk < nuse-ignore ; kk++ )
            EDIT_substitute_brick( new_dsetA3 , kk , MRI_float , foutA3[kk] ) ;
      break ;

      /*** output is shorts:
           we have to create a scaled sub-brick from fout ***/

      case MRI_short:{
         short * boutA3 ;
         float facA3 ;

         for( kk=0 ; kk < nuse-ignore ; kk++ ){  /* loop over sub-bricks */

            /*-- get output sub-brick --*/

            boutA3 = (short *) malloc( sizeof(short) * nvox ) ;
            if( boutA3 == NULL ){
               fprintf(stderr,"\nFinal malloc error in plug_power!\n\a") ;
               EXIT(1) ;
            }

            /*-- find scaling and then scale --*/

            facA3 = MCW_vol_amax( nvox,1,1 , MRI_float , foutA3[kk] ) ;
            if( facA3 > 0.0 ){
               facA3 = 32767.0 / facA3 ;
               EDIT_coerce_scale_type( nvox,facA3 ,
                                       MRI_float,foutA3[kk] , MRI_short,boutA3 ) ;
               facA3 = 1.0 / facA3 ;
            }

            free( foutA3[kk] ) ;  /* don't need this anymore */

            /*-- put output brick into dataset, and store scale factor --*/

            EDIT_substitute_brick( new_dsetA3 , kk , MRI_short , boutA3 ) ;
            tarA3[kk] = facA3 ;


         }

         /*-- save scale factor array into dataset --*/

         EDIT_dset_items( new_dsetA3 , ADN_brick_fac , tarA3 , ADN_none ) ;

      }
      break ;

      /*** output is bytes (byte = unsigned char)
           we have to create a scaled sub-brick from fout ***/

      case MRI_byte:{
         byte * boutA3 ;
         float facA3 ;

         for( kk=0 ; kk < nuse-ignore ; kk++ ){  /* loop over sub-bricks */

            /*-- get output sub-brick --*/

            boutA3 = (byte *) malloc( sizeof(byte) * nvox ) ;
            if( boutA3 == NULL ){
               fprintf(stderr,"\nFinal malloc error in plug_power!\n\a") ;
               EXIT(1) ;
            }

            /*-- find scaling and then scale --*/

            facA3 = MCW_vol_amax( nvox,1,1 , MRI_float , foutA3[kk] ) ;
            if( facA3 > 0.0 ){
               facA3 = 255.0 / facA3 ;
               EDIT_coerce_scale_type( nvox,facA3 ,
                                       MRI_float,foutA3[kk] , MRI_byte,boutA3 ) ;
               facA3 = 1.0 / facA3 ;
            }

            free( foutA3[kk] ) ;  /* don't need this anymore */

            /*-- put output brick into dataset, and store scale factor --*/

            EDIT_substitute_brick( new_dsetA3 , kk , MRI_byte , boutA3 ) ;
            tarA3[kk]= facA3 ;


         }

         /*-- save scale factor array into dataset --*/

         EDIT_dset_items( new_dsetA3 , ADN_brick_fac , tarA3 , ADN_none ) ;
      }
      break ;

   } /* end of switch on output data type */


     switch( new_datum ){

      case MRI_float:{

            EDIT_substitute_brick( new_dsetavgD3 , 0 , MRI_float , foutavgD3[0] ) ;


    }
      break ;

      case MRI_short:{
         short * boutavgD3 ;
         float facavgD3 ;

            boutavgD3 = (short *) malloc( sizeof(short) * nvox ) ;
            if( boutavgD3 == NULL ){
               fprintf(stderr,"\nFinal malloc error in plug_power!\n\a") ;
               EXIT(1) ;
            }

            facavgD3 = MCW_vol_amax( nvox,1,1 , MRI_float , foutavgD3[0] ) ;
            if( facavgD3 > 0.0 ){
               facavgD3 = 32767.0 / facavgD3 ;
               EDIT_coerce_scale_type( nvox,facavgD3 ,
                                       MRI_float,foutavgD3[0] , MRI_short,boutavgD3 ) ;
               facavgD3 = 1.0 / facavgD3 ;
            }



            EDIT_substitute_brick( new_dsetavgD3 , 0 , MRI_short , boutavgD3 ) ;
            taravgD3[0] = facavgD3 ;

             EDIT_dset_items( new_dsetavgD3 , ADN_brick_fac , taravgD3 , ADN_none ) ;



      }
      break ;

      case MRI_byte:{
         byte * boutavgD3 ;
         float facavgD3 ;


            boutavgD3 = (byte *) malloc( sizeof(byte) * nvox ) ;
            if( boutavgD3 == NULL ){
               fprintf(stderr,"\nFinal malloc error in plug_power!\n\a") ;
               EXIT(1) ;
            }

            facavgD3 = MCW_vol_amax( nvox,1,1 , MRI_float , foutavgD3[0] ) ;
            if( facavgD3 > 0.0 ){
               facavgD3 = 255.0 / facavgD3 ;
               EDIT_coerce_scale_type( nvox,facavgD3 ,
                                       MRI_float,foutavgD3[0] , MRI_byte,boutavgD3 ) ;
               facavgD3 = 1.0 / facavgD3 ;
            }



            EDIT_substitute_brick( new_dsetavgD3 , 0 , MRI_byte , boutavgD3 ) ;
            taravgD3[0]= facavgD3 ;

            EDIT_dset_items( new_dsetavgD3 , ADN_brick_fac , taravgD3 , ADN_none ) ;




      }
      break ;

   } /* endasda of switch on output data type */




   /*-------------- Cleanup and go home ----------------*/



   PLUTO_add_dset( plint , new_dsetD3 , DSET_ACTION_NONE ) ;
  PLUTO_add_dset( plint , new_dsetA3 , DSET_ACTION_NONE ) ;
  PLUTO_add_dset( plint , new_dsetavgD3 , DSET_ACTION_NONE ) ;



   FREE_WORKSPACE ;
   free(numAv);


   return NULL ;  /* null string returned means all was OK */
}

#ifdef ALLOW_TESTING
/*****************************************************************************
 -----------------------------------------------------------------------------
           Create the second interface within this plugin.
 -----------------------------------------------------------------------------*/

PLUGIN_interface * TEST_init( void )
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "Testing" ,
                                "Testing, Testing, 1-2-3 ..." ,
                                NULL ,
                                PLUGIN_CALL_VIA_MENU , TEST_main  ) ;

   PLUTO_add_hint( plint , "1-2-3, 1-2-3, ..." ) ;

   PLUTO_add_option( plint ,
                     "Input" ,  /* label at left of input line */
                     "Input" ,  /* tag to return to plugin */
                     TRUE       /* is this mandatory? */
                   ) ;

   PLUTO_add_dataset_list(  plint ,
                            "Datasets" ,       /* label next to button   */
                            ANAT_ALL_MASK ,    /* take any anat datasets */
                            FUNC_FIM_MASK ,    /* only allow fim funcs   */
                            DIMEN_4D_MASK |    /* need 3D+time datasets  */
                            BRICK_ALLREAL_MASK /* need real-valued datasets */
                         ) ;
   return plint ;
}

char * TEST_main( PLUGIN_interface * plint )
{
   MRI_IMAGE * tsim ;
   MCW_idclist * idclist ;
   MCW_idcode * idc ;
   THD_3dim_dataset * dset ;
   char str[256] ;
   int id ;

   /*--------- go to first input line ---------*/

   PLUTO_next_option(plint) ;

   idclist = PLUTO_get_idclist(plint) ;
   if( PLUTO_idclist_count(idclist) == 0 )
      return " \nNo input dataset list!\n " ;

   id = 0 ;
   do {
      idc  = PLUTO_idclist_next(idclist) ;
      dset = PLUTO_find_dset(idc) ;
      if( dset == NULL ) return NULL ;
      id++ ;
      sprintf(str, " \nDataset %d = %s\n nx = %d\n ny = %d\n nz = %d\n " ,
              id , DSET_FILECODE(dset) , dset->daxes->nxx,dset->daxes->nyy,dset->daxes->nzz ) ;

      PLUTO_popup_transient( plint , str ) ;
   } while(1) ;
   return NULL ;
}
#endif  /* ALLOW_TESTING */
