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
  Plugin to do what 3dvolreg.c does.  Most of the code is
  adapted from plug_imreg.c, 3dvolreg.c, and plug_realtime.c.
************************************************************************/

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "Purpose: 3D (volumetric) registration of 3D+time dataset\n"
  "[See also program '3dvolreg']\n"
  "\n"
  "Input items to this plugin are:\n"
  "   Datasets:   Input  = 3D+time dataset to process\n"
  "               Output = Prefix for new dataset\n"
  "\n"
  "   Parameters: Base Brick = Time index for base image\n"
  "               Resampling = Resampling method\n"
  "\n"
  "   Basis:      Dataset = Dataset from which to take base brick\n"
  "                         [if not selected, uses input dataset]\n"
  "\n"
  "   Outputs:    dfile = If not blank, name of file in which to\n"
  "                       save estimated movement parameters plus\n"
  "                       before-and-after residuals;\n"
  "                       see '3dvolreg -help' for details\n"
  "              1Dfile = If not blank, name of file in which to save\n"
  "                       ONLY the estimated movement parameters;\n"
  "                       this file will also be added to the AFNI\n"
  "                       timeseries list, so that it can be used\n"
  "                       as an 'ort' with FIM.\n"
  "               Graph = If 'Yes', will plot the estimated movement\n"
  "                       parameters when the registration is done.\n"
  "\n"
  "WARNING: This plugin can be slow, particularly when registering\n"
  "         large datasets using Fourier resampling.  The algorithm\n"
  "         used will only work to correct for small movements.\n"
  "\n"
  "Author -- RW Cox, November 1998"
;

/*----------------- prototypes for internal routines -----------------*/

char * VOLREG_main( PLUGIN_interface * ) ;  /* the entry point */

/*---------------------------- global data ---------------------------*/

static PLUGIN_interface * global_plint = NULL ;

#define NRESAM 4
static char * REG_resam_strings[NRESAM] = {
   "Cubic" , "Quintic" , "Heptic" , "Fourier"
 } ;

static int REG_resam_ints[NRESAM] = {
   MRI_CUBIC , MRI_QUINTIC , MRI_HEPTIC , MRI_FOURIER
} ;

#define NGRAPH 2
static char * GRAPH_strings[NGRAPH] = { "No" , "Yes" } ;

static int         VL_nbase  = 3 ;
static int         VL_resam  = 3 ;
static int         VL_clipit = 1 ;
static int         VL_intern = 1 ;
static int         VL_graph  = 0 ;
static MRI_IMAGE * VL_imbase = NULL ;

static char VL_dfile[256]  = "\0" ;
static char VL_1Dfile[256] = "\0" ;              /* 14 Apr 2000 */
static char VL_prefix[THD_MAX_PREFIX] = "\0" ;

static THD_3dim_dataset * VL_dset = NULL ;

static int VL_maxite = 9 ;
static float VL_dxy  = 0.05 ;  /* voxels */
static float VL_dph  = 0.07 ;  /* degrees */
static float VL_del  = 0.70 ;  /* voxels */

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

   plint = PLUTO_new_interface( "3D Registration" ,
                                "3D Registration of a 3D+time Dataset" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , VOLREG_main  ) ;

   PLUTO_add_hint( plint , "3D Registration of a 3D+time Dataset" ) ;

   global_plint = plint ;  /* make global copy */

   PLUTO_set_sequence( plint , "A:newdset:reg" ) ;

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
                       DIMEN_ALL_MASK | BRICK_ALLREAL_MASK
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
                     "Base Brick" ,  /* label next to chooser */
                     0 ,             /* smallest possible value */
                     98 ,            /* largest possible value */
                     0 ,             /* decimal shift (none in this case) */
                     VL_nbase ,      /* default value */
                     FALSE           /* allow user to edit value? */
                   ) ;

   PLUTO_add_string( plint, "Resampling", NRESAM, REG_resam_strings, VL_resam ) ;

   /*---------- 3rd line --------*/

   PLUTO_add_option( plint , "Basis" , "Basis" , FALSE ) ;

   PLUTO_add_dataset(  plint ,
                       "Dataset" ,        /* label next to button   */
                       ANAT_ALL_MASK ,    /* take any anat datasets */
                       FUNC_FIM_MASK ,    /* only allow fim funcs   */
                       DIMEN_ALL_MASK | BRICK_ALLREAL_MASK
                    ) ;

   /*---------- 4th line ---------*/

   PLUTO_add_option( plint , "Outputs" , "Outputs" , FALSE ) ;

   PLUTO_add_string( plint ,
                     "dfile" ,   /* label next to textfield */
                     0,NULL ,    /* no fixed strings to choose among */
                     19          /* 19 spaces for typing in value */
                   ) ;

   /* 14 Apr 2000: add 1Dfile */

   PLUTO_add_string( plint ,
                     "1Dfile" ,  /* label next to textfield */
                     0,NULL ,    /* no fixed strings to choose among */
                     19          /* 19 spaces for typing in value */
                   ) ;

   PLUTO_add_string( plint , "Graph" , NGRAPH , GRAPH_strings , VL_graph ) ;

   /*--------- done with interface setup ---------*/

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

#undef FREEUP
#define FREEUP(x) do{if((x) != NULL){free((x)); (x)=NULL;}}while(0)

char * VOLREG_main( PLUGIN_interface * plint )
{
   MCW_idcode * idc ;
   char * str ;
   MRI_3dalign_basis * albase ;
   THD_3dim_dataset * new_dset ;
   MRI_IMAGE * qim , * tim , * fim ;
   float *dx, *dy, *dz, *roll, *yaw, *pitch, *rmsnew, *rmsold, *imb, *tar ;
   float ddx,ddy,ddz , sum ;
   float dxtop,dytop,dztop , rolltop,yawtop,pitchtop ;
   float dxbot,dybot,dzbot , rollbot,yawbot,pitchbot ;
   float dxbar,dybar,dzbar , rollbar,yawbar,pitchbar ;
   int kim,ii , imcount , iha , ax1,ax2,ax3 , hax1,hax2,hax3 ;

   double cputim = COX_cpu_time();

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   /*--------- go to first input line ---------*/

   PLUTO_next_option(plint) ;

   idc     = PLUTO_get_idcode(plint) ;   /* get dataset item */
   VL_dset = PLUTO_find_dset(idc) ;      /* get ptr to dataset */
   if( VL_dset == NULL )
      return "*************************\n"
             "Cannot find Input Dataset\n"
             "*************************"  ;

   ii = DSET_NVALS_PER_TIME(VL_dset) ;
   if( ii > 1 )
      return "************************************\n"
             "Dataset has > 1 value per time point\n"
             "************************************"  ;

   str = PLUTO_get_string(plint) ;   /* get the output prefix */
   if( ! PLUTO_prefix_ok(str) )      /* check if it is OK */
      return "************************\n"
             "Output Prefix is illegal\n"
             "************************"  ;
   strcpy( VL_prefix , str ) ;

   /*--------- go to next input line ---------*/

   PLUTO_next_option(plint) ;

   VL_nbase = PLUTO_get_number(plint) ;

   str      = PLUTO_get_string(plint) ;
   VL_resam = PLUTO_string_index( str , NRESAM , REG_resam_strings ) ;
   VL_resam = REG_resam_ints[VL_resam] ;

   /*--------- see if the other (non-mandatory) option line are present --------*/

   VL_imbase   = NULL ;
   VL_dfile[0] = '\0' ;
   VL_1Dfile[0]= '\0' ;  /* 14 Apr 2000 */
   VL_graph    = 0 ;

   while( 1 ){
      str = PLUTO_get_optiontag( plint ) ; if( str == NULL ) break ;

      if( strcmp(str,"Basis") == 0 ){  /* Alternate basis dataset */
         THD_3dim_dataset * bset ;

         idc  = PLUTO_get_idcode(plint) ;   /* get dataset item */
         bset = PLUTO_find_dset(idc) ;      /* get ptr to dataset */
         if( bset == NULL )
            return "*************************\n"
                   "Cannot find Basis Dataset\n"
                   "*************************"  ;

          if( VL_nbase >= DSET_NVALS(bset) || VL_nbase < 0 )
             return "*****************************************\n"
                    "Base index out of range for Basis dataset\n"
                    "*****************************************"  ;

          if( DSET_NX(bset) != DSET_NX(VL_dset) ||
              DSET_NY(bset) != DSET_NY(VL_dset) || DSET_NZ(bset) != DSET_NZ(VL_dset) )
             return "***************************\n"
                    "Brick size mismatch between\n"
                    " Input and Basis datasets! \n"
                    "***************************"  ;

          DSET_load(bset) ;
          if( ! DSET_LOADED(bset) ) return "************************\n"
                                           "Can't load Basis dataset\n"
                                           "************************"  ;

          VL_imbase = mri_to_float( DSET_BRICK(bset,VL_nbase) ) ;  /* copy this */
          VL_intern = 0 ;
          DSET_unload(bset) ;

      } else if( strcmp(str,"Outputs") == 0 ){  /* Optional outputs */

         str = PLUTO_get_string(plint) ;
         if( str != NULL && str[0] != '\0' ){
            if( THD_filename_ok(str) ) strcpy( VL_dfile , str ) ;
            else                       return "*************************\n"
                                              "dfile name not acceptable\n"
                                              "*************************"  ;
         }

         /* 14 Apr 2000: get 1Dfile */

         str = PLUTO_get_string(plint) ;
         if( str != NULL && str[0] != '\0' ){
            if( THD_filename_ok(str) ) strcpy( VL_1Dfile , str ) ;
            else                       return "**************************\n"
                                              "1Dfile name not acceptable\n"
                                              "**************************"  ;
         }

         str      = PLUTO_get_string(plint) ;
         VL_graph = PLUTO_string_index( str , NGRAPH , GRAPH_strings ) ;
      }

   }

   /*-- must manufacture base image --*/

   if( VL_imbase == NULL ){

      if( DSET_NVALS(VL_dset) < 2 )
         return "******************************************\n"
                "Can't register a 1 brick dataset to itself\n"
                "******************************************"  ;

      if( VL_nbase >= DSET_NVALS(VL_dset) || VL_nbase < 0 )
          return "*****************************************\n"
                 "Base index out of range for Input dataset\n"
                 "*****************************************"  ;

      DSET_load(VL_dset) ;
      if( ! DSET_LOADED(VL_dset) ) return "************************\n"
                                          "Can't load Input dataset\n"
                                          "************************"  ;

      VL_imbase = mri_to_float( DSET_BRICK(VL_dset,VL_nbase) ) ;  /* copy this */
      VL_intern = 1 ;
   }

   VL_imbase->dx = fabs( DSET_DX(VL_dset) ) ;  /* must set the voxel dimensions */
   VL_imbase->dy = fabs( DSET_DY(VL_dset) ) ;
   VL_imbase->dz = fabs( DSET_DZ(VL_dset) ) ;
   imb = MRI_FLOAT_PTR( VL_imbase ) ;          /* need this to compute rms */

   /*------------- set up to compute new dataset -----------*/

   DSET_load( VL_dset ) ;
   if( ! DSET_LOADED(VL_dset) ){
      mri_free( VL_imbase ) ;
      return "************************\n"
             "Can't load Input dataset\n"
             "************************"  ;
   }

   imcount = DSET_NVALS( VL_dset ) ;
   dx      = (float *) malloc( sizeof(float) * imcount ) ;
   dy      = (float *) malloc( sizeof(float) * imcount ) ;
   dz      = (float *) malloc( sizeof(float) * imcount ) ;
   roll    = (float *) malloc( sizeof(float) * imcount ) ;
   pitch   = (float *) malloc( sizeof(float) * imcount ) ;
   yaw     = (float *) malloc( sizeof(float) * imcount ) ;
   rmsnew  = (float *) malloc( sizeof(float) * imcount ) ;
   rmsold  = (float *) malloc( sizeof(float) * imcount ) ;

   iha = THD_handedness( VL_dset )   ;                     /* LH or RH? */
   ax1 = THD_axcode( VL_dset , 'I' ) ; hax1 = ax1 * iha ;  /* roll */
   ax2 = THD_axcode( VL_dset , 'R' ) ; hax2 = ax2 * iha ;  /* pitch */
   ax3 = THD_axcode( VL_dset , 'A' ) ; hax3 = ax3 * iha ;  /* yaw */

   mri_3dalign_params( VL_maxite , VL_dxy , VL_dph , VL_del ,
                       abs(ax1)-1 , abs(ax2)-1 , abs(ax3)-1 , -1 ) ;

   mri_3dalign_method( VL_resam , 0 , 0 , VL_clipit ) ;

   new_dset = EDIT_empty_copy( VL_dset ) ;
   EDIT_dset_items( new_dset , ADN_prefix , VL_prefix , ADN_none ) ;

   { char * his = PLUTO_commandstring(plint) ;
     tross_Copy_History( VL_dset , new_dset ) ;
     tross_Append_History( new_dset , his ) ; free(his) ;
   }

   albase = mri_3dalign_setup( VL_imbase , NULL ) ;

   /*-- loop over sub-bricks and register them --*/

   PLUTO_popup_meter(plint) ;

   dxbar = dybar = dzbar = rollbar = yawbar = pitchbar = 0.0 ;

   for( kim=0 ; kim < imcount ; kim++ ){

      qim     = DSET_BRICK( VL_dset , kim ) ; /* the sub-brick in question */
      fim     = mri_to_float( qim ) ;         /* make a float copy */
      fim->dx = fabs( DSET_DX(VL_dset) ) ;    /* must set voxel dimensions */
      fim->dy = fabs( DSET_DY(VL_dset) ) ;
      fim->dz = fabs( DSET_DZ(VL_dset) ) ;

      /*-- the actual registration --*/

      if( !VL_intern || kim != VL_nbase ){
         tim = mri_3dalign_one( albase , fim ,
                                roll+kim , pitch+kim , yaw+kim ,
                                &ddx     , &ddy      , &ddz     ) ;
      } else {
         tim = mri_to_float( VL_imbase ) ;
         roll[kim] = pitch[kim] = yaw[kim] = ddx = ddy = ddz = 0.0 ;
      }

      /*-- need to massage output parameters --*/

      roll[kim]  *= (180.0/PI) ; if( hax1 < 0 ) roll[kim]  = -roll[kim] ;
      pitch[kim] *= (180.0/PI) ; if( hax2 < 0 ) pitch[kim] = -pitch[kim] ;
      yaw[kim]   *= (180.0/PI) ; if( hax3 < 0 ) yaw[kim]   = -yaw[kim] ;

      switch( new_dset->daxes->xxorient ){
         case ORI_R2L_TYPE: dy[kim] =  ddx ; break ;
         case ORI_L2R_TYPE: dy[kim] = -ddx ; break ;
         case ORI_P2A_TYPE: dz[kim] = -ddx ; break ;
         case ORI_A2P_TYPE: dz[kim] =  ddx ; break ;
         case ORI_I2S_TYPE: dx[kim] =  ddx ; break ;
         case ORI_S2I_TYPE: dx[kim] = -ddx ; break ;
      }

      switch( new_dset->daxes->yyorient ){
         case ORI_R2L_TYPE: dy[kim] =  ddy ; break ;
         case ORI_L2R_TYPE: dy[kim] = -ddy ; break ;
         case ORI_P2A_TYPE: dz[kim] = -ddy ; break ;
         case ORI_A2P_TYPE: dz[kim] =  ddy ; break ;
         case ORI_I2S_TYPE: dx[kim] =  ddy ; break ;
         case ORI_S2I_TYPE: dx[kim] = -ddy ; break ;
      }

      switch( new_dset->daxes->zzorient ){
         case ORI_R2L_TYPE: dy[kim] =  ddz ; break ;
         case ORI_L2R_TYPE: dy[kim] = -ddz ; break ;
         case ORI_P2A_TYPE: dz[kim] = -ddz ; break ;
         case ORI_A2P_TYPE: dz[kim] =  ddz ; break ;
         case ORI_I2S_TYPE: dx[kim] =  ddz ; break ;
         case ORI_S2I_TYPE: dx[kim] = -ddz ; break ;
      }

      /*-- collect statistics --*/

      if( kim == 0 ){
         dxtop    = dxbot    = dx[kim]    ;
         dytop    = dybot    = dy[kim]    ;
         dztop    = dzbot    = dz[kim]    ;
         rolltop  = rollbot  = roll[kim]  ;
         pitchtop = pitchbot = pitch[kim] ;
         yawtop   = yawbot   = yaw[kim]   ;
      } else {
         dxtop    = MAX(dxtop   , dx[kim]   ); dxbot    = MIN(dxbot   , dx[kim]   );
         dytop    = MAX(dytop   , dy[kim]   ); dybot    = MIN(dybot   , dy[kim]   );
         dztop    = MAX(dztop   , dz[kim]   ); dzbot    = MIN(dzbot   , dz[kim]   );
         rolltop  = MAX(rolltop , roll[kim] ); rollbot  = MIN(rollbot , roll[kim] );
         pitchtop = MAX(pitchtop, pitch[kim]); pitchbot = MIN(pitchbot, pitch[kim]);
         yawtop   = MAX(yawtop  , yaw[kim]  ); yawbot   = MIN(yawbot  , yaw[kim]  );
      }

      dxbar   += dx[kim]   ; dybar    += dy[kim]    ; dzbar  += dz[kim]  ;
      rollbar += roll[kim] ; pitchbar += pitch[kim] ; yawbar += yaw[kim] ;

      sum = 0.0 ;
      tar = MRI_FLOAT_PTR(tim) ;
      for( ii=0 ; ii < tim->nvox ; ii++ ) sum += SQR( imb[ii] - tar[ii] ) ;
      rmsnew[kim] = sqrt( sum / tim->nvox ) ;

      sum = 0.0 ;
      tar = MRI_FLOAT_PTR(fim) ;
      for( ii=0 ; ii < fim->nvox ; ii++ ) sum += SQR( imb[ii] - tar[ii] ) ;
      rmsold[kim] = sqrt( sum / fim->nvox ) ;

      mri_free(fim) ;  /* only needed this to compute rmsold */

      /*-- Attach the registered brick to output dataset,
           converting it to the correct type, if necessary
           (the new registered brick in "tim" is stored as floats). --*/

      switch( qim->kind ){

         case MRI_float:
            EDIT_substitute_brick( new_dset , kim , MRI_float , MRI_FLOAT_PTR(tim) ) ;
            mri_fix_data_pointer( NULL , tim ) ; mri_free( tim ) ;
         break ;

         case MRI_short:
            fim = mri_to_short(1.0,tim) ; mri_free( tim ) ;
            EDIT_substitute_brick( new_dset , kim , MRI_short , MRI_SHORT_PTR(fim) ) ;
            mri_fix_data_pointer( NULL , fim ) ; mri_free( fim ) ;
         break ;

         case MRI_byte:
            fim = mri_to_byte(tim) ; mri_free( tim ) ;
            EDIT_substitute_brick( new_dset , kim , MRI_byte , MRI_BYTE_PTR(fim) ) ;
            mri_fix_data_pointer( NULL , fim ) ; mri_free( fim ) ;
         break ;

         /*-- should not ever get here, but who knows? --*/

         default:
            fprintf(stderr,"\n*** Can't register bricks of type %s\n",
                    MRI_TYPE_name[qim->kind] ) ;
            exit(1) ;
      }

      DSET_unload_one( VL_dset , kim ) ;      /* don't need this anymore */

      PLUTO_set_meter(plint, (100*(kim+1))/imcount ) ;

   }  /* end of loop over sub-bricks */

   /*-- done with registration --*/

   mri_3dalign_cleanup( albase ) ;
   mri_free( VL_imbase ) ;
   DSET_unload( VL_dset ) ;

   /*-- show some statistics of the results --*/

   dxbar   /= imcount ; dybar    /= imcount ; dzbar  /= imcount ;
   rollbar /= imcount ; pitchbar /= imcount ; yawbar /= imcount ;

   { char buf[512] ; int nbuf ;

      cputim = COX_cpu_time() - cputim ;
      sprintf(buf,"  \nCPU time for realignment=%.3g s\n\n" , cputim) ;
      nbuf = strlen(buf) ;

      sprintf(buf+nbuf,"Min : roll=%+.3f  pitch=%+.3f  yaw=%+.3f"
                       "  dS=%+.3f  dL=%+.3f  dP=%+.3f\n\n" ,
              rollbot , pitchbot , yawbot , dxbot , dybot , dzbot ) ;
      nbuf = strlen(buf) ;

      sprintf(buf+nbuf,"Mean: roll=%+.3f  pitch=%+.3f  yaw=%+.3f"
                       "  dS=%+.3f  dL=%+.3f  dP=%+.3f\n\n" ,
              rollbar , pitchbar , yawbar , dxbar , dybar , dzbar ) ;
      nbuf = strlen(buf) ;

      sprintf(buf+nbuf,"Max : roll=%+.3f  pitch=%+.3f  yaw=%+.3f"
                       "  dS=%+.3f  dL=%+.3f  dP=%+.3f\n" ,
              rolltop , pitchtop , yawtop , dxtop , dytop , dztop ) ;

      PLUTO_popup_message( plint , buf ) ;
   }

   /*-- save to a file --*/

   if( VL_dfile[0] != '\0' ){
      FILE * fp ;

      if( THD_is_file(VL_dfile) )
         PLUTO_popup_transient( plint , "** Warning:\n"
                                        "** Overwriting dfile" ) ;

      fp = fopen( VL_dfile , "w" ) ;
      for( kim=0 ; kim < imcount ; kim++ )
         fprintf(fp , "%4d %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f  %11.4g %11.4g\n" ,
                 kim , roll[kim], pitch[kim], yaw[kim],
                       dx[kim], dy[kim], dz[kim], rmsold[kim] , rmsnew[kim]  ) ;
      fclose(fp) ;
   }

   if( VL_1Dfile[0] != '\0' ){  /* 14 Apr 2000 */
      FILE * fp ;
      char fn[256] ;
      MRI_IMAGE * tsim ;
      float * tsar ;

      strcpy(fn,VL_1Dfile) ;
      if( strstr(VL_1Dfile,"1D") == NULL ) strcat(fn,".1D") ;
      
      if( THD_is_file(fn) )
         PLUTO_popup_transient( plint , "** Warning:\n"
                                        "** Overwriting 1Dfile" ) ;

      tsim = mri_new( imcount , 6 , MRI_float ) ;
      tsar = MRI_FLOAT_PTR(tsim) ;
      fp = fopen( fn , "w" ) ;
      for( kim=0 ; kim < imcount ; kim++ ){
         fprintf(fp , "%7.3f %7.3f %7.3f %7.3f %7.3f %7.3f\n" ,
                 roll[kim], pitch[kim], yaw[kim],
                 dx[kim]  , dy[kim]   , dz[kim]  ) ;
         tsar[0*imcount+kim] = roll[kim] ;
         tsar[1*imcount+kim] = pitch[kim] ;
         tsar[2*imcount+kim] = yaw[kim] ;
         tsar[3*imcount+kim] = dx[kim] ;
         tsar[4*imcount+kim] = dy[kim] ;
         tsar[5*imcount+kim] = dz[kim] ;
      }
      fclose(fp) ;
      PLUTO_register_timeseries( fn , tsim ) ;
      mri_free(tsim) ;
   }

   /*-- graph --*/

   if( VL_graph && imcount > 1 ){
      float * yar[7] , dt ;
      int nn = imcount ;
      static char * nar[6] = {
         "\\Delta I-S [mm]" , "\\Delta R-L [mm]" , "\\Delta A-P [mm]" ,
         "Roll [\\degree]" , "Pitch [\\degree]" , "Yaw [\\degree]"  } ;

      yar[0] = (float *) malloc( sizeof(float) * nn ) ;
      dt     = DSET_TIMESTEP(VL_dset) ; if( dt <= 0.0 ) dt = 1.0 ;
      for( ii=0 ; ii < nn ; ii++ ) yar[0][ii] = ii * dt ;

      yar[1] = dx   ; yar[2] = dy    ; yar[3] = dz  ;
      yar[4] = roll ; yar[5] = pitch ; yar[6] = yaw ;

      plot_ts_lab( GLOBAL_library.dc->display ,
                   nn , yar[0] , -6 , yar+1 ,
                   "time" , NULL , DSET_FILECODE(VL_dset) , nar , NULL ) ;

      free(yar[0]) ;
   }

   /* done with these statistics */

   FREEUP(dx)     ; FREEUP(dy)     ; FREEUP(dz) ;
   FREEUP(roll)   ; FREEUP(yaw)    ; FREEUP(pitch) ;
   FREEUP(rmsnew) ; FREEUP(rmsold) ;

   /*-- save new dataset to disk --*/

   PLUTO_add_dset( plint , new_dset , DSET_ACTION_MAKE_CURRENT ) ;
   return NULL ;
}
