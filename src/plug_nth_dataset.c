#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to provide extra datasets' timeseries in place of first
************************************************************************/

char * DSETN_main( PLUGIN_interface *) ;
void   DSETN_func( MRI_IMAGE * ) ;

#undef USE_WHERE

static char helpstring[] =
   " Purpose: Control the 'Dataset#N' 1D timeseries function\n"
   "\n"
   " Dataset = a 3D+time dataset from which to extract data\n"
   " Color   = color to overlay in AFNI graph window\n"
   "\n"
   " Usage:\n"
   " 1) Choose in this plugin the 3D+time datasets you want to graph,\n"
   "     and their overlay colors.  Then press 'Run+Keep'.\n"
   " 2) In an AFNI graph window, choose 'Tran 1D' to be 'Dataset#N'.\n"
   "     This will cause the time series chosen here to replace the\n"
   "     time series of the dataset being graphed.\n"
   " 3) Optionally, you can set 'Double Plot' to 'Overlay' in the\n"
   "     AFNI graph window - this will then also show the time series\n"
   "     from the current AFNI graph window.\n"
   "\n"
   " Note that changes in this plugin interface are not communicated\n"
   " to AFNI itself until you press one of the 'Run' buttons.\n"
#ifdef USE_WHERE
   "\n"
   " Justify = Left  means to put timeseries data at start of output array\n"
   "           Right means to put timeseries data at end of output array\n"
   "\n"
   " Fill    = Extend means to copy the last value\n"
   "           Zero   means to fill with zeros\n"
   "\n"
   " Justify and Fill are only used when the number of points in the input\n"
   " dataset doesn't match the number of points in the timeseries being replaced.\n"
   " Let M = length of dataset, N = length of timeseries,\n"
   " ts[] = timeseries array, ds[] = dataset array.\n"
   "\n"
   " Case: M < N (too little data to fill timeseries)\n"
   "    Left: ts[i] = ds[i]    i=0..M-1                        Data goes to left\n"
   "                = ds[M-1]  i=M..N-1 if Extend              edge of timeseries\n"
   "                = 0.0      i=M..N-1 if Zero\n"
   "   Right: ts[i] = ds[0]    i=0..J-1 if Extend (with J=N-M) Data goes to right\n"
   "                = 0.0      i=0..J-1 if Zero                edge of timeseries\n"
   "                = ds[i-J]  i=J..N-1\n"
   " Case: M > N (too much data for timeseries)\n"
   "    Left: ts[i] = ds[i]    i=0..N-1                        Left data to ts[]\n"
   "   Right: ts[i] = ds[i+J]  i=0..N-1 (with J=M-N)           Right data to ts[]\n"
#endif
   "\n"
   " -- RWCox - 29 Mar 2002\n"
;

#define NMAX 9  /* max number of time series */

static THD_3dim_dataset *dset[NMAX] ;
static int               ovc [NMAX] ;

#ifdef USE_WHERE
static int justify = 0 ;
static int fill    = 0 ;
#endif

/***********************************************************************
   Set up the interface to the user
************************************************************************/

static char *lr[2] = { "Left" , "Right" } ;
static char *ez[2] = { "Extend" , "Zero" } ;

PLUGIN_interface * PLUGIN_init( int ncall )
{
   int id ;
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   AFNI_register_nD_function( 1 , "Dataset#N" , DSETN_func , NEEDS_DSET_INDEX|PROCESS_MRI_IMAGE ) ;

   plint = PLUTO_new_interface( "Dataset#N" , "Controls 1D function Dataset#N" , helpstring ,
                                 PLUGIN_CALL_VIA_MENU , DSETN_main  ) ;

   PLUTO_add_hint( plint , "Controls 1D function Dataset#N" ) ;

   PLUTO_set_sequence( plint , "A:funcs:dataset#N" ) ;

   for( id=0 ; id < NMAX ; id++ ){
     PLUTO_add_option( plint , "Input" , "Input" , FALSE ) ;
     PLUTO_add_dataset(plint , "Dataset" ,
                                      ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                      DIMEN_4D_MASK | BRICK_ALLREAL_MASK ) ;
     PLUTO_add_overlaycolor( plint , "Color" ) ;
   }

#ifdef USE_WHERE
   PLUTO_add_option( plint , "Where" , "Where" , FALSE ) ;
   PLUTO_add_string( plint, "Justify", 2, lr, justify ) ;

   PLUTO_add_option( plint , "How" , "How" , FALSE ) ;
   PLUTO_add_string( plint, "Fill", 2, ez, fill ) ;
#endif

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

char * DSETN_main( PLUGIN_interface * plint )
{
   MCW_idcode *idc ;
   char *str , *tag ;
   int id=0 ;

   if( plint == NULL )
      return "***********************\n"
             "DSETN_main:  NULL input\n"
             "***********************"  ;

   for( id=0 ; id < NMAX ; id++ ) dset[id] = NULL ;
   id = 0 ;

   while( (tag=PLUTO_get_optiontag(plint)) != NULL ){

      /* Input */

      if( strcmp(tag,"Input") == 0 ){
        idc      = PLUTO_get_idcode(plint) ;
        dset[id] = PLUTO_find_dset(idc) ;
        ovc [id] = PLUTO_get_overlaycolor(plint) ;
        id++ ; continue ;
      }

#ifdef USE_WHERE
      /* Where */

      if( strcmp(tag,"Where") == 0 ){
        str = PLUTO_get_string(plint) ;
        justify = (strcmp(str,lr[0]) != 0) ;
        continue ;
      }

      /* How */

      if( strcmp(tag,"How") == 0 ){
        str = PLUTO_get_string(plint) ;
        fill = (strcmp(str,ez[0]) != 0) ;
        continue ;
      }
#endif

   }

   PLUTO_force_redisplay() ;
   return NULL ;
}

/*----------------------------------------------------------------------------*/

void DSETN_func( MRI_IMAGE *qim )
{
   int id , ny , nxtop=0 , ijk ;
   MRI_IMARR * tar ;
   MRI_IMAGE * tsim ;
   float *tsar , *dar ;
   int ovi[NMAX] ;
   char str[16+4*NMAX] ;

   INIT_IMARR(tar) ;
   ijk = AFNI_needs_dset_ijk() ;          /* voxel index from AFNI */

   for( id=0 ; id < NMAX ; id++ ){
     if( ISVALID_DSET(dset[id]) ){
       tsim = THD_extract_series( ijk,dset[id], 0 ) ;  /* get data */
       if( tsim == NULL ) continue ;
       ovi[IMARR_COUNT(tar)] = ovc[id] ;
       ADDTO_IMARR(tar,tsim) ;
       nxtop = MAX(nxtop,tsim->nx) ;
     }
   }

   ny = IMARR_COUNT(tar) ;

   if( ny == 0 ){ DESTROY_IMARR(tar); return; }        /* no data */

   if( ny == 1 ){                                  /* one dataset */
      tsim = IMARR_SUBIM(tar,0); FREE_IMARR(tar);
      mri_move_guts(qim,tsim);
      sprintf(str,"color: %d",ovi[0]) ; mri_add_name(str,qim) ;
      return;
   }
                                             /* multiple datasets */

   tsim = mri_new( nxtop , ny , MRI_float ) ;
   tsar = MRI_FLOAT_PTR(tsim) ;
   strcpy(str,"color:") ;
   for( id=0 ; id < ny ; id++ ){
      dar = MRI_FLOAT_PTR( IMARR_SUBIM(tar,id) ) ;
      memcpy( tsar , dar , sizeof(float)*IMARR_SUBIM(tar,id)->nx) ;
      tsar += nxtop ;
      sprintf(str+strlen(str)," %d",ovi[id]) ;
   }

   DESTROY_IMARR(tar) ;
   mri_move_guts(qim,tsim) ; mri_add_name(str,qim) ;
   return;
}
