#include "afni.h"

static char *abet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;

static FILE *fpc[MAX_CONTROLLERS] ;

#undef  EPS
#define EPS 0.01  /* threshold for coordinate changes */

/*----------------------------------------------------------------------------*/

static void AFNI_filer_viewpoint_CB( int why, int q, void *qq, void *qqq )
{
   Three_D_View *im3d = (Three_D_View *)qqq ;
   int ic , vv , ii,jj,kk ; float xx,yy,zz ;
   static float xold=-666,yold=-777,zold=-888 ;

ENTRY("AFNI_filer_viewpoint_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   ic = AFNI_controller_index(im3d) ;
   if( ic < 0 || ic >= MAX_CONTROLLERS || fpc[ic] == NULL ) EXRETURN ;

   xx = im3d->vinfo->xi ; yy = im3d->vinfo->yj ; zz = im3d->vinfo->zk ;
   ii = im3d->vinfo->i1 ; jj = im3d->vinfo->j2 ; kk = im3d->vinfo->k3 ;

   if( fabs(xx-xold) < EPS &&
       fabs(yy-yold) < EPS &&
       fabs(zz-zold) < EPS    ) EXRETURN ;  /* too close to old point */

   vv = fprintf( fpc[ic] , "%10.4f %10.4f %10.4f  %d %d %d\n" ,
                 xx,yy,zz , ii,jj,kk ) ;
   if( vv < 0 ){
     ERROR_message("Can't write viewpoint for [%c]",abet[ic]) ;
     fclose(fpc[ic]) ; fpc[ic] = NULL ;
   } else {
     fflush(fpc[ic]) ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void AFNI_coord_filer_setup( Three_D_View *im3d )
{
   char ename[32] , *eval ; int ic ;

ENTRY("AFNI_coord_filer_setup") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   ic = AFNI_controller_index(im3d) ;
   if( ic < 0 || ic >= MAX_CONTROLLERS || fpc[ic] != NULL ) EXRETURN ;

   sprintf(ename,"AFNI_FILE_COORDS_%c",abet[ic]) ;
   eval = my_getenv(ename) ;
   if( eval == NULL || *eval == '\0' ){ fpc[ic] = NULL ; EXRETURN ; }

   if( strcmp(eval,"-") == 0 || strncmp(eval,"stdout",6) == 0 )
     fpc[ic] = stdout ;
   else {
     fpc[ic] = fopen( eval , "w" ) ;
     if( fpc[ic] == NULL ){
       ERROR_message("Unable to open file %s from %s",eval,ename) ;
       EXRETURN ;
     }
   }

   AFNI_receive_init( im3d , RECEIVE_VIEWPOINT_MASK ,
                             AFNI_filer_viewpoint_CB ,
                             im3d , "AFNI_filer_viewpoint_CB" ) ;

   INFO_message("Logging [%c] viewpoint changes to '%s'",abet[ic],eval) ;
   EXRETURN ;
}
