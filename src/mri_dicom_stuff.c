#include "mrilib.h"
#include "mri_dicom_elist.h"

/*--------------------------------------------------------------------------*/

static char *manf[] = {
    "UNKNOWN"   ,
    "Siemens"   , "GE"        , "Philips"   ,
    "Toshiba"   , "Fonar"     , "Hitachi"   ,
    "Magnaserv" , "Odin"      , "ONI"       ,
    "Bruker"    , "Varian"
} ;

#define NUM_MANF (sizeof(manf)/sizeof(char *))

char *AFD_manufacturer_code_to_string( int code )
{

   code = code - AFD_MAN_OFFSET ;
   if( code <= 0 || code >= NUM_MANF ) return manf[0] ;
   return manf[code] ;
}

int AFD_manufacturer_string_to_code( char *str )
{
   int jj ;
   if( str == NULL || *str == '\0' ) return AFD_MAN_OFFSET ;
   for( jj=1 ; jj < NUM_MANF ; jj++ )
     if( strcasecmp(str,manf[jj]) == 0 ) return AFD_MAN_OFFSET+jj ;
   return AFD_MAN_OFFSET ;
}

/*--------------------------------------------------------------------------*/

#undef  FREEIF
#define FREEIF(x) if((x)!=NULL)free((void *)(x))

void AFD_siemens_info_free( void *aei )
{
   AFD_siemens_info *asi = (AFD_siemens_info *)aei ;

   if( asi == NULL ) return ;

   FREEIF(asi->position_sag) ; FREEIF(asi->position_cor) ;
   FREEIF(asi->position_tra) ; FREEIF(asi->normal_sag)   ;
   FREEIF(asi->normal_cor)   ; FREEIF(asi->normal_tra)   ;
   FREEIF(asi->inplane_rot)  ;

   free((void *)asi) ; return ;
}

/*--------------------------------------------------------------------------*/

void AFD_dicom_header_free( AFD_dicom_header *adh )
{
   if( adh == NULL ) return ;

   FREEIF(adh->filename) ;

   if( adh->extra_info != NULL ){
     int *eit = (int *)adh->extra_info ;

     if( *eit == AFD_EIT_SIEMENS ){
       AFD_siemens_info_free( adh->extra_info ) ;
     } else {
       WARNING_message("Unknown DICOM extra_info type=%d\n",*eit) ;
     }
   }

   free((void *)adh) ; return ;
}

/*--------------------------------------------------------------------------*/

AFD_dicom_header * AFD_scanfor_header( char *ppp )
{
   char *ddd ;
   off_t poff ;
   unsigned int plen ;
   char *epos[NUM_ELIST] ;
   int ii,jj , ee , bpp , datum ;
   int nx,ny,nz , swap , shift=0 ;
   float dx,dy,sp,th,dt ;
   AFD_dicom_header *dh ;

   /** check for sane input **/

   if( ppp == NULL || *ppp == '\0' ) return NULL ;

   /* find positions in header of elements we care about */

   for( ee=0 ; ee < NUM_ELIST ; ee++ )
     epos[ee] = strstr(ppp,elist[ee]) ;

   /* see if the header has the elements we absolutely need */

   if( epos[E_ROWS]           == NULL ||
       epos[E_COLUMNS]        == NULL ||
       epos[E_BITS_ALLOCATED] == NULL   ) return NULL ;

   /* check if we have 1 sample per pixel (can't deal with 3 or 4 now) */

   if( epos[E_SAMPLES_PER_PIXEL] != NULL ){
     ddd = strstr(epos[E_SAMPLES_PER_PIXEL],"//") ;
     ii  = 0 ; sscanf(ddd+2,"%d",&ii) ;
     if( ii != 1 ) return NULL ;
   }

   /* check if photometric interpretation is MONOCHROME (don't like PALETTE) */

   if( epos[E_PHOTOMETRIC_INTERPRETATION] != NULL ){
     ddd = strstr(epos[E_PHOTOMETRIC_INTERPRETATION],"MONOCHROME") ;
     if( ddd == NULL ) return NULL ;
   }

   /*** create emtpy output struct ***/

   dh = calloc( 1 , sizeof(AFD_dicom_header) ) ;

   /* check if we have 8, 16, or 32 bits per pixel */

   ddd = strstr(epos[E_BITS_ALLOCATED],"//") ;
   if( ddd == NULL ){ free(ppp); RETURN(NULL); }
   bpp = 0 ; sscanf(ddd+2,"%d",&bpp) ; dh->nbits = bpp ;

   /* check if Rescale is ordered */

   if( epos[E_RESCALE_INTERCEPT] != NULL && epos[E_RESCALE_SLOPE] != NULL ){
     ddd = strstr(epos[E_RESCALE_INTERCEPT],"//") ;
     sscanf(ddd+2,"%f",&dh->rescale_intercept)    ;
     ddd = strstr(epos[E_RESCALE_SLOPE    ],"//") ;
     sscanf(ddd+2,"%f",&dh->rescale_slope    )    ;
   }

   /* check if Window is ordered */

   if( epos[E_WINDOW_CENTER] != NULL && epos[E_WINDOW_WIDTH] != NULL ){
     ddd = strstr(epos[E_WINDOW_CENTER],"//") ;
     sscanf(ddd+2,"%f",&dh->window_center)    ;
     ddd = strstr(epos[E_WINDOW_WIDTH ],"//") ;
     sscanf(ddd+2,"%f",&dh->window_width ) ;
   }

   /* get image nx & ny */

   ddd = strstr(epos[E_ROWS],"//") ;
   ny = 1 ; sscanf(ddd+2,"%d",&ny) ;

   ddd = strstr(epos[E_COLUMNS],"//") ;
   nx = 1 ; sscanf(ddd+2,"%d",&nx) ;

   /* get number of slices */

   nz = 1 ;
   if( epos[E_NUMBER_OF_FRAMES] != NULL ){
     ddd = strstr(epos[E_NUMBER_OF_FRAMES],"//") ;
     sscanf(ddd+2,"%d",&nz) ;
   }

   dh->ni = nx ; dh->nj = ny ; dh->nk = nz ;

   /*-- try to get dx, dy, dz, dt --*/

   dx = dy = sp = th = dt = 0.0 ;

   /* dx,dy first */

   if( epos[E_PIXEL_SPACING] != NULL ){
     ddd = strstr(epos[E_PIXEL_SPACING],"//") ;
     sscanf( ddd+2 , "%f\\%f" , &dx , &dy ) ;
     if( dy == 0.0 && dx > 0.0 ) dy = dx ;
   }
   if( dx == 0.0 && epos[E_FIELD_OF_VIEW] != NULL ){
     ddd = strstr(epos[E_FIELD_OF_VIEW],"//") ;
     sscanf( ddd+2 , "%f\\%f" , &dx , &dy ) ;
     if( dx > 0.0 ){
       if( dy == 0.0 ) dy = dx ;
       dx /= nx ; dy /= ny ;
     }
   }

  if( epos[E_SLICE_SPACING] != NULL ){                  /* get reported slice spacing */
    ddd = strstr(epos[E_SLICE_SPACING],"//") ;
    if(*(ddd+2)=='\n') sp = 0.0 ;
    else               sscanf( ddd+2 , "%f" , &sp ) ;
  }
  if( epos[E_SLICE_THICKNESS] != NULL ){                /* get reported slice thickness */
    ddd = strstr(epos[E_SLICE_THICKNESS],"//") ;
    if(*(ddd+2)=='\n') th = 0.0 ;
    else               sscanf( ddd+2 , "%f" , &th ) ;
  }

   /* get dt */

   if( epos[E_REPETITION_TIME] != NULL ){
     ddd = strstr(epos[E_REPETITION_TIME],"//") ;
     sscanf( ddd+2 , "%f" , &dt ) ;
     dt *= 0.001 ;   /* ms to s */
   }

   dh->tr = dt ;
   dh->di = dx ;
   dh->dj = dy ;
   dh->slice_spacing = fabs(sp) ;
   dh->slice_thick   = fabs(th) ;

   /* manufacturer */

   if( epos[E_ID_MANUFACTURER] != NULL ){
     char name[128] ;
     ddd = strstr(epos[E_ID_MANUFACTURER],"//") ;
     ddd += 2 ;
     while( isspace(*ddd) ) ddd++ ;  /* skip leading whitespace */
     sscanf( ddd+2 , "%127s" , name ) ;
     dh->manufacturer_code = AFD_manufacturer_string_to_code( name ) ;
     ddd = AFD_manufacturer_code_to_string( dh->manufacturer_code ) ;
     strcpy(dh->manufacturer_string,ddd) ;
   }

   return dh ;
}

/*--------------------------------------------------------------------------*/

MultiFrame_info * AFD_scanfor_MultiFrame( char *ppp )
{
   int nz , jj , ival ;
   char *qqq , *ccc , *ddd , *ttt ;
   float xyz[3] ;
   MultiFrame_info *mfi ;

   /** check for sane input **/

   if( ppp == NULL || *ppp == '\0' ) return NULL ;

   /** must have an indication of multiple frames **/

   ccc = strstr( ppp , elist[E_NUMBER_OF_FRAMES] ) ;
   if( ccc == NULL ) return NULL ;
   ddd = strstr(ccc,"//") ; if( ddd == NULL ) return NULL ;
   nz = (int)strtol(ddd+2,NULL,10) ;
   if( nz <= 1 ) return NULL ;

   /** create output struct **/

   INIT_MultiFrame(mfi,nz) ;

   /** search for stack (==slice) index **/

   ttt = elist[E_STACK_INDEX] ;
   for( qqq=ppp,jj=0 ; jj < nz ; jj++ ){
     ccc = strstr(qqq,ttt) ;
     if( ccc == NULL ){ KILL_MultiFrame(mfi); return NULL; }
     ddd = strstr(ccc+8,"//") ;
     if( ddd == NULL ){ KILL_MultiFrame(mfi); return NULL; }
     ival = (int)strtol(ddd+2,NULL,10) ;
     if( ival <= 0 )  { KILL_MultiFrame(mfi); return NULL; }
     mfi->stack_index[jj] = ival ;
     qqq = ddd+3 ;
   }

   /** time index **/

   ttt = elist[E_TIME_INDEX] ; ccc = strstr(ppp,ttt) ;
   if( ccc == NULL ){
     ttt = elist[E_TIME_INDEX_ID] ; ccc = strstr(ppp,ttt) ;
     if( ccc == NULL ) return 0 ;
   }
   for( qqq=ccc,jj=0 ; jj < nz ; jj++ ){
     ccc = strstr(qqq,ttt) ;
     if( ccc == NULL ){ KILL_MultiFrame(mfi); return NULL; }
     ddd = strstr(ccc+8,"//") ;
     if( ddd == NULL ){ KILL_MultiFrame(mfi); return NULL; }
     ival = (int)strtol(ddd+2,NULL,10) ;
     if( ival <= 0 )  { KILL_MultiFrame(mfi); return NULL; }
     mfi->time_index[jj] = ival ;
     qqq = ddd+3 ;
   }

   /** image position (not strictly required?) **/

   ttt = elist[E_IMAGE_POSITION] ;
   for( qqq=ppp,jj=0 ; jj < nz ; jj++ ){
     ccc = strstr(qqq,ttt) ;
     if( ccc == NULL ){ DELPOS_MultiFrame(mfi); return mfi; }
     ddd = strstr(ccc+8,"//") ;
     if( ddd == NULL ){ DELPOS_MultiFrame(mfi); return mfi; }
     xyz[0] = (float)strtod(ddd+2,&ccc) ;
     xyz[1] = (float)strtod(ccc+1,&ccc) ;
     xyz[2] = (float)strtod(ccc+1,&ccc) ;
     mfi->xpos[jj] = xyz[0]; mfi->ypos[jj] = xyz[1]; mfi->zpos[jj] = xyz[2];
     qqq = ccc ;
   }

   return mfi ;
}
