#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to serve as a home for transformation functions
************************************************************************/

char * JUNK_main( PLUGIN_interface * plint ) ;
void lacy9_box_func( int nx , int ny , double dx, double dy, float * ar ) ;
void outer9_box_func( int nx , int ny , double dx, double dy, float * ar ) ;

/***********************************************************************
   Set up the interface to the user: NONE.
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "PlaceHolder" , "PlaceHolder" , NULL ,
                                PLUGIN_CALL_IMMEDIATELY , JUNK_main ) ;

   AFNI_register_2D_function( "Lacy9"   , lacy9_box_func  ) ;
   AFNI_register_2D_function( "Outer9"  , outer9_box_func ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

char * JUNK_main( PLUGIN_interface * plint ){ return NULL ; }

/*----------------------- Sample 2D transformations --------------------*/

static float * atemp = NULL ;
static int    natemp = -666 ;

#define MAKE_ATEMP(nvox)                     \
  do{ if( natemp < (nvox) ){                 \
         if( atemp != NULL ) free(atemp) ;   \
         natemp = (nvox) ;                   \
         atemp  = (float *) malloc( sizeof(float) * natemp ) ; } } while(0)

#define AT(i,j) atemp[(i)+(j)*nx]
#define AR(i,j) ar[(i)+(j)*nx]

void lacy9_box_func( int nx , int ny , double dx, double dy, float * ar )
{
   int ii , jj , nxy , isp , nnn , qqq , imid,jmid ;
   float val ;

   if( nx < 3 || ny < 3 ) return ;

   osfilt9_box_func( nx,ny,dx,dy,ar ) ;  /* smooth */

   /** make space and copy input into it **/

   nxy = nx * ny ;
   MAKE_ATEMP(nxy) ; if( atemp == NULL ) return ;
   for( ii=0 ; ii < nxy ; ii++ ) atemp[ii] = ar[ii] ;

   /** process copy of input back into the input array **/

#undef Z
#undef ZZ
#define Z(x,y)    ( ((x)>(y)) ? 0 : ((x)==(y)) ? 1 : 2 )
#define ZZ(a,b,c) ( Z((a),(b))+Z((c),(b)) >= 3 )

   for( ii=0 ; ii < nxy ; ii++ ) ar[ii] = 0.0 ;

   imid = nx/2 ; jmid = ny/2 ;

   for( jj=1 ; jj < ny-1 ; jj++ ){       /* find local peaks */
     for( ii=1 ; ii < nx-1 ; ii++ ){     /* in each 3x3 cell */

       val = AT(ii,jj) ;

#if 0
       isp =    ZZ( AT(ii-1,jj  ) , val , AT(ii+1,jj  ) )
             || ZZ( AT(ii-1,jj-1) , val , AT(ii+1,jj+1) )
             || ZZ( AT(ii-1,jj+1) , val , AT(ii+1,jj-1) )
             || ZZ( AT(ii  ,jj+1) , val , AT(ii  ,jj-1) ) ;
#else
       if( abs(ii-imid) <= abs(jj-jmid) ){
          isp =    ZZ( AT(ii-1,jj-1) , val , AT(ii+1,jj+1) )
                || ZZ( AT(ii-1,jj+1) , val , AT(ii+1,jj-1) )
                || ZZ( AT(ii  ,jj+1) , val , AT(ii  ,jj-1) ) ;
       } else {
          isp =    ZZ( AT(ii-1,jj  ) , val , AT(ii+1,jj  ) )
                || ZZ( AT(ii-1,jj-1) , val , AT(ii+1,jj+1) )
                || ZZ( AT(ii-1,jj+1) , val , AT(ii+1,jj-1) ) ;
       }
#endif

       if( isp ) ar[ii+jj*nx] = val ;
     }
   }

#define QQQMAX 1
#if QQQMAX > 0
   qqq = 0 ;
   do {
      nnn = 0 ;
      for( ii=0 ; ii < nxy ; ii++ ) atemp[ii] = ar[ii] ;
      for( jj=1 ; jj < ny-1 ; jj++ ){     /* clip off those that */
        for( ii=1 ; ii < nx-1 ; ii++ ){   /* are too isolated    */

           if( AT(ii,jj) != 0.0 ){
              isp =  (AT(ii-1,jj  ) != 0.0) + (AT(ii+1,jj  ) != 0.0)
                   + (AT(ii-1,jj+1) != 0.0) + (AT(ii+1,jj+1) != 0.0)
                   + (AT(ii-1,jj-1) != 0.0) + (AT(ii+1,jj-1) != 0.0)
                   + (AT(ii  ,jj-1) != 0.0) + (AT(ii  ,jj-1) != 0.0) ;

              if( isp < 2 ){ ar[ii+jj*nx] = 0.0 ; nnn++ ; }
           }
        }
      }
      qqq++ ;
   } while( qqq < QQQMAX && nnn > 0 ) ;
#endif

   for( jj=1 ; jj < ny-1 ; jj++ ){       /* remove isolas */
     for( ii=1 ; ii < nx-1 ; ii++ ){

          if(   (AR(ii-1,jj  ) == 0.0) && (AR(ii+1,jj  ) == 0.0)
             && (AR(ii-1,jj+1) == 0.0) && (AR(ii+1,jj+1) == 0.0)
             && (AR(ii-1,jj-1) == 0.0) && (AR(ii+1,jj-1) == 0.0)
             && (AR(ii  ,jj-1) == 0.0) && (AR(ii  ,jj-1) == 0.0) ) AR(ii,jj) = 0.0 ;
     }
   }

   return ;
}

/*------------------------------------------------------------------------*/

void outer9_box_func( int nx , int ny , double dx, double dy, float * ar )
{
   int ii , jj , nxy , nnn , imid,jmid , ib,jb ;
   float xx , yy ;

   if( nx < 3 || ny < 3 ) return ;

   lacy9_box_func( nx,ny,dx,dy,ar ) ;  /* find local peaks */

   /** make space and copy input into it **/

   nxy = nx * ny ;
   MAKE_ATEMP(nxy) ; if( atemp == NULL ) return ;
   for( ii=0 ; ii < nxy ; ii++ ) atemp[ii] = ar[ii] ;
   for( ii=0 ; ii < nxy ; ii++ ) ar[ii] = 0.0 ;

   imid = nx/2 ; jmid = ny/2 ;

#define NRAD 1800
#define DRAD (2.0*PI/NRAD)
#define DD   0.47

   for( nnn=0 ; nnn < NRAD ; nnn++ ){
      dx = DD*cos(nnn*DRAD) ; dy = DD*sin(nnn*DRAD) ;
      xx = imid + 10*dx ; yy = jmid + 10*dy ;
      ib = -1 ; jb = -1 ;

      do { xx += dx ; yy += dy ;
           ii  = xx ; jj  = yy ;
           if( ii < 0 || ii >= nx || jj < 0 || jj > ny ) break ;

           if( AT(ii,jj) != 0.0 ){ ib = ii ; jb = jj ;  AR(ii,jj) = 1.0 ;  }
      } while( 1 ) ;

      if( ib >= 0 && jb >= 0 ) AR(ib,jb) = 2.0 ;
   }

   for( jj=1 ; jj < ny-1 ; jj++ ){       /* remove isolas */
     for( ii=1 ; ii < nx-1 ; ii++ ){

          if(   (AR(ii-1,jj  ) == 0.0) && (AR(ii+1,jj  ) == 0.0)
             && (AR(ii-1,jj+1) == 0.0) && (AR(ii+1,jj+1) == 0.0)
             && (AR(ii-1,jj-1) == 0.0) && (AR(ii+1,jj-1) == 0.0)
             && (AR(ii  ,jj-1) == 0.0) && (AR(ii  ,jj-1) == 0.0) ) AR(ii,jj) = 0.0 ;
     }
   }

   return ;
}
