#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/*------------------------------------------------------------------*/

#define CORONAL_NUM 38
static float coronal_yy[] = {
  -100, -95, -90, -85, -80, -75, -70, -65,
   -60, -55, -50, -45, -40, -35, -32, -28,
   -24, -20, -16, -12,  -8,  -4,   0,   4,
     8,  12,  16,  20,  24,  28,  32,  35,
    40,  45,  50,  55,  60,  65
} ;
static char * coronal_ff[] = {
 "tt_corm99.ppm.gz", "tt_corm95.ppm.gz", "tt_corm90.ppm.gz", "tt_corm85.ppm.gz",
 "tt_corm80.ppm.gz", "tt_corm75.ppm.gz", "tt_corm70.ppm.gz", "tt_corm65.ppm.gz",
 "tt_corm60.ppm.gz", "tt_corm55.ppm.gz", "tt_corm50.ppm.gz", "tt_corm45.ppm.gz",
 "tt_corm40.ppm.gz", "tt_corm35.ppm.gz", "tt_corm32.ppm.gz", "tt_corm28.ppm.gz",
 "tt_corm24.ppm.gz", "tt_corm20.ppm.gz", "tt_corm16.ppm.gz", "tt_corm12.ppm.gz",
 "tt_corm08.ppm.gz", "tt_corm04.ppm.gz", "tt_corp00.ppm.gz", "tt_corp04.ppm.gz",
 "tt_corp08.ppm.gz", "tt_corp12.ppm.gz", "tt_corp16.ppm.gz", "tt_corp20.ppm.gz",
 "tt_corp24.ppm.gz", "tt_corp28.ppm.gz", "tt_corp32.ppm.gz", "tt_corp35.ppm.gz",
 "tt_corp40.ppm.gz", "tt_corp45.ppm.gz", "tt_corp50.ppm.gz", "tt_corp55.ppm.gz",
 "tt_corp60.ppm.gz", "tt_corp65.ppm.gz"
} ;

/*------------------------------------------------------------------------------*/

#define SAGITTAL_NUM 18
static float sagittal_xx[] = {
    0,  3,  5,  9, 13, 17, 21, 25, 29, 33,
   37, 41, 43, 47, 51, 55, 59, 61
} ;
static char * sagittal_ff[] = {
 "tt_sag00g.ppm.gz", "tt_sag03.ppm.gz", "tt_sag05.ppm.gz", "tt_sag09.ppm.gz",
 "tt_sag13.ppm.gz", "tt_sag17.ppm.gz", "tt_sag21.ppm.gz", "tt_sag25.ppm.gz",
 "tt_sag29.ppm.gz", "tt_sag33.ppm.gz", "tt_sag37.ppm.gz", "tt_sag41.ppm.gz",
 "tt_sag43.ppm.gz", "tt_sag47.ppm.gz", "tt_sag51.ppm.gz", "tt_sag55.ppm.gz",
 "tt_sag59.ppm.gz", "tt_sag61.ppm.gz"
} ;

/*------------------------------------------------------------------------------*/

#define AXIAL_NUM 27
static float axial_zz[] = {
   -40, -36, -32, -28, -24, -20, -16, -12, -8, -4,
    -1,   1,   4,   8,  12,  16,  20,  24, 28, 32, 35,
    40,  45,  50,  55,  60,  65
} ;
static char * axial_ff[] = {
 "tt_horm40.ppm.gz", "tt_horm36.ppm.gz", "tt_horm32.ppm.gz", "tt_horm28.ppm.gz",
 "tt_horm24.ppm.gz", "tt_horm20.ppm.gz", "tt_horm16.ppm.gz", "tt_horm12.ppm.gz",
 "tt_horm08.ppm.gz", "tt_horm04.ppm.gz", "tt_horm01.ppm.gz", "tt_horp01.ppm.gz",
 "tt_horp04.ppm.gz", "tt_horp08.ppm.gz", "tt_horp12.ppm.gz", "tt_horp16.ppm.gz",
 "tt_horp20.ppm.gz", "tt_horp24.ppm.gz", "tt_horp28.ppm.gz", "tt_horp32.ppm.gz",
 "tt_horp35.ppm.gz", "tt_horp40.ppm.gz", "tt_horp45.ppm.gz", "tt_horp50.ppm.gz",
 "tt_horp55.ppm.gz", "tt_horp60.ppm.gz", "tt_horp65.ppm.gz"
} ;

#define TTAHOME "http://128.231.212.175/TTA/"

/*------------------------------------------------------------------------------*/
/*! (xx,yy,zz) in RAI (Dicom) coords; code is (0=axial,1=sag,2=cor).            */
/*------------------------------------------------------------------------------*/

static char * TTget_URL( int code, float xx, float yy, float zz )
{
   static char nbuf[256] ;
   char *ttahome ;
   int ii,jj,kk ;

   ttahome = getenv("AFNI_TTAHOME") ; if( ttahome == NULL ) ttahome = TTAHOME ;

   switch( code ){
     case 1:                       /* sagittal */
       xx = fabs(xx) ;
       if( xx <= sagittal_xx[0] ){
         ii = 0 ;
       } else if( xx >= sagittal_xx[SAGITTAL_NUM-1] ){
         ii = SAGITTAL_NUM - 1 ;
       } else {
         for( ii=1 ; ii < SAGITTAL_NUM && xx > sagittal_xx[ii] ; ii++ ) ; /* nada */
         if( fabs(xx-sagittal_xx[ii-1]) < fabs(xx-sagittal_xx[ii]) ) ii-- ;
       }
       sprintf(nbuf,"%s%s", ttahome, sagittal_ff[ii] ) ;
     break ;

     case 2:                       /* coronal */
       yy = -yy ;
       if( yy <= coronal_yy[0] ){
          jj = 0 ;
       } else if( yy >= coronal_yy[CORONAL_NUM-1] ){
          jj = CORONAL_NUM - 1 ;
       } else {
          for( jj=1 ; jj < CORONAL_NUM && yy > coronal_yy[jj] ; jj++ ) ; /* nada */
          if( fabs(yy-coronal_yy[jj-1]) < fabs(yy-coronal_yy[jj]) ) jj-- ;
       }
       sprintf(nbuf,"%s%s", ttahome, coronal_ff[jj] ) ;
     break ;

     default:
     case 0:                       /* axial */
       if( zz <= axial_zz[0] ){
          kk = 0 ;
       } else if( zz >= axial_zz[AXIAL_NUM-1] ){
          kk = AXIAL_NUM - 1 ;
       } else {
          for( kk=1 ; kk < AXIAL_NUM && zz > axial_zz[kk] ; kk++ ) ; /* nada */
          if( fabs(zz-axial_zz[kk-1]) < fabs(zz-axial_zz[kk]) ) kk-- ;
       }
       sprintf(nbuf,"%s%s", ttahome, axial_ff[kk] ) ;
     break ;
   }

   return nbuf ;
}

/*------------------------------------------------------------------------------*/

static MRI_IMAGE * TTget_ppm( char *url )
{
   int ii,nn, ch,nch, nx,ny,maxval, id ;
   char buf[32] ;
   byte *ndata=NULL, *bar ;
   MRI_IMAGE *im ;

   nn = NI_read_URL( url, (char **)&ndata ) ;

   if( nn < 40960 || ndata == NULL || ndata[0] != 'P' || ndata[1] != '6' ){
      if( ndata != NULL ) free(ndata) ;
      return NULL ;
   }

   id = 2 ; ch = ndata[id] ;  /* start scan after "P6" */

#define SKIPCOM                                                                      \
    {if(ch == '#') do{ch=ndata[++id];}while(id<nn-1 && ch != '\n');}

#define NUMSCAN(var)                                                                 \
   { SKIPCOM ;                                                                       \
     while( id<nn-1 && !isdigit(ch) ){ch = ndata[++id]; SKIPCOM; }                   \
     for( nch=0 ; id<nn-1 && isdigit(ch) ; nch++,ch=ndata[++id] ) {buf[nch] = ch ;}  \
     buf[nch]='\0';                                                                  \
     var = strtol( buf, NULL, 10 ) ; }

    NUMSCAN(nx) ; if( nx <= 2 || id >= nn-1 ){ free(ndata); return NULL; }
    NUMSCAN(ny) ; if( ny <= 2 || id >= nn-1 ){ free(ndata); return NULL; }
    NUMSCAN(maxval);
    if( maxval <= 7 || maxval > 255 || id >= nn-1 ){ free(ndata); return NULL; }

    id++ ;
    if( nn-id < 3*nx*ny ){ free(ndata); return NULL; }

    im = mri_new( nx, ny, MRI_rgb ) ;
    bar = MRI_RGB_PTR(im) ;
    memcpy( bar, ndata+id, 3*nx*ny ) ;
    free(ndata) ;

    if( maxval < 255 ){
      float fac = 255.4/maxval ;
      for( ii=0 ; ii < 3*nx*ny ; ii++ ) bar[ii] = (byte)( bar[ii]*fac ) ;
    }

    return im ;
}

/*--------------------------------------------------------------------*/

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <arpa/inet.h>

/*----------------------------------------------------------------
   Return the Internet address (in 'dot' format, as a string)
   given the name of the host.  If NULL is returned, some
   error occurrrrred.  The return string is malloc-ed and should
   be free-d someday.
------------------------------------------------------------------*/

static char * xxx_name_to_inet( char * host )
{
   struct hostent * hostp ;
   char * iname = NULL, * str ;
   int ll ;

   if( host == NULL || host[0] == '\0' ) return NULL ;
   hostp = gethostbyname(host) ; if( hostp == NULL ) return NULL ;

   str = inet_ntoa(*((struct in_addr *)(hostp->h_addr))) ;
   if( str == NULL || str[0] == '\0' ) return NULL ;

   ll = strlen(str) ; iname = malloc(ll+1) ; strcpy(iname,str) ;
   return iname ;
}

/*----------------------------------------------------------------*/

static char * self_to_inet( void )
{
   char hname[1048]="localhost" ;
   static char *ipad=NULL ;
   if( ipad == NULL ){ gethostname(hname,1048); ipad = xxx_name_to_inet(hname); }
   return ipad ;
}

/*----------------------------------------------------------------*/

static int is_nih_host( void )
{
   char *ipad = self_to_inet() ;

   if( ipad == NULL ) return 0 ;

   if( strstr(ipad,"128.231.") == ipad ) return 1 ;
   if( strstr(ipad,"130.14." ) == ipad ) return 1 ;
   if( strstr(ipad,"137.187.") == ipad ) return 1 ;
   if( strstr(ipad,"156.40." ) == ipad ) return 1 ;
   if( strstr(ipad,"165.112.") == ipad ) return 1 ;
   if( strstr(ipad,"157.98." ) == ipad ) return 1 ;

   if( strstr(ipad,"129.43."      ) == ipad ) return 1 ;
   if( strstr(ipad,"199.249.158." ) == ipad ) return 1 ;
   if( strstr(ipad,"209.218.0."   ) == ipad ) return 1 ;
   if( strstr(ipad,"130.14."      ) == ipad ) return 1 ;
   if( strstr(ipad,"131.158.140." ) == ipad ) return 1 ;
   if( strstr(ipad,"150.148.11"   ) == ipad ) return 1 ;
   if( strstr(ipad,"150.148.21"   ) == ipad ) return 1 ;

   return 0 ;
}

/*-----------------------------------------------------------------*/

#define NUM_ORIENT_STRINGS 4
static char *orient_strings[4] = { "None", "Axial", "Sagittal", "Coronal" } ;
static int orient = 0 ;

static char * TTget_main( PLUGIN_interface * ) ;

/*-----------------------------------------------------------------*/

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 || !is_nih_host() ) return NULL ;

   plint = PLUTO_new_interface( "TT Atlas",
                                "TT Atlas display",
                                NULL,
                                PLUGIN_CALL_VIA_MENU, TTget_main  ) ;

   PLUTO_add_option( plint, "Mode", "MODE", TRUE ) ;

   PLUTO_add_string( plint, "Orient", NUM_ORIENT_STRINGS, orient_strings, orient ) ;

   return plint ;
}

/*-----------------------------------------------------------------*/

static int recv_key = -1 ;
static Three_D_View *old_im3d = NULL ;
static char old_url[256] = "\0" ;

static void TTget_recv( int why, int np, int * ijk, void * junk ) ;

/*-----------------------------------------------------------------*/

static char * TTget_main( PLUGIN_interface *plint )
{
   char *str ;

   PLUTO_next_option(plint) ;
   str    = PLUTO_get_string(plint) ;
   orient = PLUTO_string_index( str, NUM_ORIENT_STRINGS, orient_strings ) ;

   old_url[0] = '\0' ;

   if( plint->im3d != old_im3d || !IM3D_OPEN(plint->im3d) ){
     if( recv_key >= 0 ){
       AFNI_receive_control( old_im3d,recv_key,EVERYTHING_SHUTDOWN,NULL ) ;
       recv_key = -1 ;
     }
     old_im3d = plint->im3d ;
     if( !IM3D_OPEN(plint->im3d) )
       return "***************************************\n"
              "TTget_main: AFNI controller isn't open!\n"
              "***************************************"   ;
   }

   if( orient == 0 && recv_key >= 0 ){
     AFNI_receive_control( plint->im3d,recv_key,EVERYTHING_SHUTDOWN,NULL ) ;
     recv_key = -1 ;
   } else if( orient > 0 && recv_key < 0 ){
     recv_key = AFNI_receive_init( plint->im3d, RECEIVE_VIEWPOINT_MASK, TTget_recv, NULL ) ;
   }

   return NULL ;
}

/*-----------------------------------------------------------------*/

static void *impop = NULL ;

static void TTget_recv( int why, int np, int * ijk, void * junk )
{
   if( orient == 0          ||
       !IM3D_OPEN(old_im3d) ||
       old_im3d->vinfo->view_type != VIEW_TALAIRACH_TYPE ) return ;

   switch( why ){

      case RECEIVE_VIEWPOINT:{ /*-- change of crosshair location --*/
        MRI_IMAGE *im ;
        char *url ;

        url = TTget_URL( orient-1, old_im3d->vinfo->xi,
                                    old_im3d->vinfo->yj, old_im3d->vinfo->zk ) ;

        if( strcmp(url,old_url) == 0 ) return ;
        strcpy(old_url,url) ;

        im = TTget_ppm( url ) ; if( im == NULL ) return ;

        impop = PLUTO_popup_image( impop, im ) ;
        mri_free(im) ;
      }
      break ;

   }

   return ;
}
