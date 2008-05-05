#include "mrilib.h"

/*--------------------------------------------------------------------------*/

char *AFD_manufacturer_code_to_string( int code )
{
   static char *manf[] = {
     "UNKNOWN"   ,
     "Siemens"   , "GE"        , "Philips"   ,
     "Toshiba"   , "Fonar"     , "Hitachi"   ,
     "Magnaserv" , "Odin"      , "ONI"       ,
     "Bruker"    , "Varian"
   } ;

   code = code - AFD_MAN_OFFSET ;
   if( code <= 0 || code >= sizeof(manf)/sizeof(char *) ) return manf[0] ;
   return manf[code] ;
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

   if( adh->filename != NULL ) free((void *)adh->filename) ;

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

#include "mri_dicom_elist.h"

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
   sscanf(ddd+2,"%d",&nz) ;
   if( nz <= 1 ) return NULL ;

   /** create output struct **/

   INIT_MultiFrame(mfi,nz) ;

   /** search for stack (==slice) index **/

   ttt = elist[E_STACK_INDEX] ;
   for( qqq=ppp,jj=0 ; jj < nz ; jj++ ){
      ccc = strstr(qqq,ttt) ;
      if( ccc == NULL ){ KILL_MultiFrame(mfi); return NULL; }
      ddd = strstr(ccc,"//") ;
      if( ddd == NULL ){ KILL_MultiFrame(mfi); return NULL; }
      sscanf(ddd+2,"%d",&ival) ;
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
   for( qqq=ppp,jj=0 ; jj < nz ; jj++ ){
      ccc = strstr(qqq,ttt) ;
      if( ccc == NULL ){ KILL_MultiFrame(mfi); return NULL; }
      ddd = strstr(ccc,"//") ;
      if( ddd == NULL ){ KILL_MultiFrame(mfi); return NULL; }
      sscanf(ddd+2,"%d",&ival) ;
      if( ival <= 0 )  { KILL_MultiFrame(mfi); return NULL; }
      mfi->stack_index[jj] = ival ;
      qqq = ddd+3 ;
   }

   /** image position (not strictly required?) **/

   ttt = elist[E_IMAGE_POSITION] ;
   for( qqq=ppp,jj=0 ; jj < nz ; jj++ ){
     ccc = strstr(qqq,ttt) ;
     if( ccc == NULL ){ DELPOS_MultiFrame(mfi); return mfi; }
     ddd = strstr(ccc,"//") ;
     if( ddd == NULL ){ DELPOS_MultiFrame(mfi); return mfi; }
     ival = sscanf(ddd+2,"%f\\%f\\%f",xyz,xyz+1,xyz+2) ;
     if( ival < 3 )   { DELPOS_MultiFrame(mfi); return mfi; }
     mfi->xpos[jj] = xyz[0]; mfi->ypos[jj] = xyz[1]; mfi->zpos[jj] = xyz[2];
     qqq = ddd+3 ;
   }

   return mfi ;
}
