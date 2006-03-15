#include "mrilib.h"

/*--------------------------------------------------------------------------*/

char *AFD_manufacturer_string( int code )
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
