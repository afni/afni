#include "coxplot.h"
#include <math.h>

#ifndef WAY_BIG
#  define WAY_BIG 1.e+10
#endif

static float p10( float x ) ;  /* prototype */

#undef  NCLR
#define NCLR 4
static float ccc[NCLR][3] = {
  { 0.0 , 0.0 , 0.0 } ,
  { 0.9 , 0.0 , 0.0 } ,
  { 0.0 , 0.7 , 0.0 } ,
  { 0.0 , 0.0 , 0.9 } ,
} ;

#define STGOOD(s) ( (s) != NULL && (s)[0] != '\0' )

#define THIK 0.003
#define SY   0.07

/*----------------------------------------------------------------------
  Return p10 as a power of 10 such that
    p10 <= fabs(x) < 10*p10
  unless x == 0, in which case return 0.
------------------------------------------------------------------------*/

static float p10( float x )
{
   double y ;

   if( x == 0.0 ) return 0.0 ;
   if( x <  0.0 ) x = -x ;
   y = floor(log10(x)+0.000001) ; y = pow( 10.0 , y ) ;
   return (float) y ;
}

/*----------------------------------------------------------------------
  Setup for strip plotting - like ts plotting, but will recycle
  15 Nov 2001 - RWCox
------------------------------------------------------------------------*/

MEM_topshell_data * plot_strip_init( Display * dpy ,
                                     int nx , float dx ,
                                     int ny , float ybot , float ytop ,
                                     char * lab_xxx , char * lab_yyy ,
                                     char * lab_top , char ** nam_yyy ,
                                     void_func * killfunc              )
{
   int ii , jj , np , nnax,nnay , mmax,mmay , yall ;
   float pbot,ptop , xobot,xotop,yobot,yotop , yll,yhh ;
   char str[32] ;
   float *ud ;
   MEM_topshell_data * mp ;
   MEM_plotdata      * mplot ;

   /*-- sanity check --*/

   if( dpy == NULL || ny == 0 || nx < 9 || ybot >= ytop ) return NULL ;

   if( dx <= 0.0 ) dx = 1.0 ;

   yall = (ny > 0) ; if( !yall ) ny = -ny ; if( ny == 1 ) yall = 1 ;

   /*-- data ranges --*/

   ptop = p10(nx) ;
   nnax = rint(nx/ptop) ;
   if( nnax == 1 ) nnax = 10 ;
   mmax = (nnax < 3) ? 10
                     : (nnax < 6) ? 5 : 2 ;

   pbot = p10(ybot) ; ptop = p10(ytop) ; if( ptop < pbot ) ptop = pbot ;
   nnay = rint((ytop-ybot)/ptop) ;
   if( nnay == 1 ) nnay = 10 ;
   mmay = (nnay < 3) ? 10
                     : (nnay < 6) ? 5 : 2 ;

   /*-- setup to plot --*/

   create_memplot_surely( "Striplot" , 1.3 ) ;
   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   set_thick_memplot( 0.0 ) ;
   mplot = get_active_memplot() ;

   /*-- plot labels, if any --*/

   xobot = 0.15 ; xotop = 1.27 ;  /* set objective size of plot */
   yobot = 0.1  ; yotop = 0.95 ;

   if( STGOOD(lab_top) ){ yotop -= 0.02 ; yobot -= 0.01 ; }
   if( nam_yyy != NULL ){ xotop -= 0.16 ; xobot -= 0.02 ; }

   /* x-axis label? */

   if( STGOOD(lab_xxx) )
      plotpak_pwritf( 0.5*(xobot+xotop) , yobot-0.06 , lab_xxx , 16 , 0 , 0 ) ;

   /* y-axis label? */

   if( STGOOD(lab_yyy) )
      plotpak_pwritf( xobot-0.10 , 0.5*(yobot+yotop) , lab_yyy , 16 , 90 , 0 ) ;

   /* label at top? */

   if( STGOOD(lab_top) )
      plotpak_pwritf( xobot+0.01 , yotop+0.01 , lab_top , 18 , 0 , -2 ) ;

   /*-- plot all on same vertical scale --*/

   if( yall ){

      /* do name labels at right? */

      if( nam_yyy != NULL ){
         float yv = yotop ; int sz ;

         for( jj=0 ; jj < ny ; jj++ ){
            if( STGOOD(nam_yyy[jj]) ){
               set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;
               set_thick_memplot( 2*THIK ) ;
               plotpak_line( xotop+0.008 , yv , xotop+0.042 , yv ) ;
               set_thick_memplot( 0.0 ) ;
               set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
               sz = (strlen(nam_yyy[jj]) <= 10) ? 12 : 10 ;
               plotpak_pwritf( xotop+0.048 , yv , nam_yyy[jj] , sz , 0 , -1 ) ;
               yv -= 0.05 ;
            }
         }
      }

      /* plot axes */

      plotpak_set( xobot,xotop , yobot,yotop , 0.0,nx*dx , ybot,ytop , 1 ) ;
      plotpak_periml( nnax,mmax , nnay,mmay ) ;

   } else {  /*-- plot each on separate vertical scale --*/

      float dyo = (yotop-yobot) / ( (1.0+SY) * ny - SY ) ;

      /* name labels at right? */

      if( nam_yyy != NULL ){
         float yv = yotop ; int sz ;

         for( jj=0 ; jj < ny ; jj++ ){
            yll = yobot + jj*(1.0+SY)*dyo ; yhh = yll + dyo ;
            if( STGOOD(nam_yyy[jj]) ){
               set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;
               set_thick_memplot( 2*THIK ) ;
               yv = 0.7*yhh + 0.3*yll ;
               plotpak_line( xotop+0.008 , yv , xotop+0.042 , yv ) ;
               set_thick_memplot( 0.0 ) ;
               set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
               sz = (strlen(nam_yyy[jj]) <= 10) ? 12 : 10 ;
               plotpak_pwritf( xotop+0.048 , yv , nam_yyy[jj] , sz , 0 , -1 ) ;
            }
         }
      }

      /* data each in its own box */

      nnay = 1 ;
      pbot = p10(ybot) ; ptop = p10(ytop) ;
      if( ptop > pbot && pbot > 0.0 ) ptop = pbot ;
      if( ptop != 0.0 ) mmay = floor( (ytop-ybot) / ptop + 0.5 ) ;
      else              mmay = 5 ;   /* shouldn't happen */

           if( mmay == 1 ) mmay = 5 ;
      else if( mmay == 2 ) mmay = 4 ;
      else if( mmay == 3 ) mmay = 6 ;

      for( jj=ny-1 ; jj >= 0 ; jj-- ){
         yll = yobot + jj*(1.0+SY)*dyo ; yhh = yll + dyo ;

         plotpak_set( xobot,xotop , yll,yhh , 0.0,nx*dx , ybot,ytop , 1 ) ;

         plotpak_perimm( nnax,mmax , nnay,mmay , (jj==0) ? 1 : 3 ) ;
         if( ybot < 0.0 && ytop > 0.0 ){
            plotpak_setlin(5) ;
            plotpak_line( 0.0,0.0 , nx*dx,0.0 ) ;
            plotpak_setlin(1) ;
         }
      }
   }

   /*-- open display for this plot --*/

   mp = memplot_to_topshell( dpy , mplot , killfunc ) ;
   if( mp == NULL ) return NULL ;

   /*-- auxiliary data needed by addto --*/

   ud = (float *) calloc( (12+ny) , sizeof(float) ) ;
   ud[0] = xobot ; ud[1] = xotop ; ud[2] = yobot ; ud[3] = yotop ;
   ud[4] = 0.0   ; ud[5] = nx    ; ud[6] = ybot  ; ud[7] = ytop  ;
   ud[8] = ny    ; ud[9] = yall  ;

   ud[10] = MEMPLOT_NLINE(mplot) ;  /* number of init lines */
   ud[11] = -1 ;                    /* current x position   */

   mp->userdata = ud ;

   /*-- plot invalid lines, to be replaced later with valid lines --*/

   /* line connecting ii to ii+1 at the jj-th y level
      is number ud[10] + ii + jj*nx in the memplot structure */

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         ADDTO_MEMPLOT( mplot , 0.0,0.0,0.0,0.0,0.0,-THCODE_INVALID ) ;

   /* and two more for each y (an X at the current point):
      X for the jj-th y level is the two lines numbered
      ud[10] + nx*ny + 2*jj  and ud[10] + nx*ny + 2*jj+1   */

   for( jj=0 ; jj < 2*ny ; jj++ )
      ADDTO_MEMPLOT( mplot , 0.0,0.0,0.0,0.0,0.0,-THCODE_INVALID ) ;

   /*-- exit, stage left --*/

   return mp ;
}

/*-----------------------------------------------------------------------*/

void plot_strip_addto( MEM_topshell_data * mp , int nadd , float **y )
{
   int ii , jj , yall , start , xx , nx , ny ;
   float pbot,ptop , xobot,xotop,yobot,yotop , yll,yhh ;
   float xbot,xtop , ybot,ytop ;
   float dxx , dyy ;
   float * ud ;
   MEM_plotdata *mplot ;

   if( mp == NULL || mp->userdata == NULL ||
      !mp->valid  || nadd <= 0            || y == NULL ) return ;

   ud = (float *) mp->userdata ;
   xobot = ud[0] ; xotop = ud[1] ; yobot = ud[2] ; yotop = ud[3] ;
   xbot  = ud[4] ; xtop  = ud[5] ; ybot  = ud[6] ; ytop  = ud[7] ;
   ny    = ud[8] ; yall  = ud[9] ; start = ud[10]; xx    = ud[11];
   nx    = xtop  ;

   if( nadd > nx ) nadd = nx ;  /* can't add too many points */

   mplot = mp->mp ;
   ii = set_active_memplot( MEMPLOT_IDENT(mplot) ) ;
   if( ii != 0 ) return ;

   dxx = 0.01*nx ;

   /* last x-value plotted was at xx */

   if( yall ){  /*-- all in one big happy box --*/

      dyy = 0.01*(ytop-ybot) ;

      plotpak_set( xobot,xotop , yobot,yotop , xbot,xtop , ybot,ytop , 1 ) ;
      set_thick_memplot( THIK ) ;

      if( xx >= 0 ){  /* connect to last time in */
         for( jj=0 ; jj < ny ; jj++ ){
           insert_at_memplot( start + xx + jj*nx , mplot ) ;
           if( ud[12+jj] < WAY_BIG && y[jj][0] < WAY_BIG ){
             set_color_memplot( ccc[jj%NCLR][0], ccc[jj%NCLR][1], ccc[jj%NCLR][2] ) ;
             plotpak_line( xx , ud[12+jj] , xx+1 , y[jj][0] ) ;
           } else {
             ADDTO_MEMPLOT( mplot , 0.0,0.0,0.0,0.0,0.0,-THCODE_INVALID ) ;
           }
         }
         xx++ ; if( xx == nx ) xx = 0 ; /* start plotting at next point */

      } else {        /* only happens 1st time in */
         xx = 0 ;
      }

      for( ii=1 ; ii < nadd ; ii++ ){
         for( jj=0 ; jj < ny ; jj++ ){
           insert_at_memplot( start + xx + jj*nx , mplot ) ;
           if( y[jj][ii-1] < WAY_BIG && y[jj][ii] < WAY_BIG ){
             set_color_memplot( ccc[jj%NCLR][0],ccc[jj%NCLR][1],ccc[jj%NCLR][2] );
             plotpak_line( xx , y[jj][ii-1] , xx+1 , y[jj][ii] ) ;
           } else {
             ADDTO_MEMPLOT( mplot , 0.0,0.0,0.0,0.0,0.0,-THCODE_INVALID ) ;
           }
         }
         xx++ ; if( xx == nx ) xx = 0 ;
      }

      /* 16 Nov 2001: add X at the end */

      set_thick_memplot( 2*THIK ) ;

      ii = nadd-1 ;
      for( jj=0 ; jj < ny ; jj++ ){
        if( y[jj][ii] < WAY_BIG ){
          set_color_memplot( ccc[jj%NCLR][0],ccc[jj%NCLR][1],ccc[jj%NCLR][2] );
          insert_at_memplot( start + nx*ny + 2*jj , mplot ) ;
          plotpak_line( xx-dxx , y[jj][ii]-dyy , xx+dxx , y[jj][ii]+dyy ) ;
          insert_at_memplot( start + nx*ny + 2*jj+1 , mplot ) ;
          plotpak_line( xx-dxx , y[jj][ii]+dyy , xx+dxx , y[jj][ii]-dyy ) ;
        } else {
          insert_at_memplot( start + nx*ny + 2*jj , mplot ) ;
          ADDTO_MEMPLOT( mplot , 0.0,0.0,0.0,0.0,0.0,-THCODE_INVALID ) ;
          insert_at_memplot( start + nx*ny + 2*jj+1 , mplot ) ;
          ADDTO_MEMPLOT( mplot , 0.0,0.0,0.0,0.0,0.0,-THCODE_INVALID ) ;
        }
      }

   } else {  /*-- each in its own little sad box --*/

      float dyo = (yotop-yobot) / ( (1.0+SY) * ny - SY ) ;

      dyy = 0.01*(ytop-ybot)*ny ;

      set_thick_memplot( THIK ) ;

      if( xx >= 0 ){  /* connect to last time in */
         for( jj=0 ; jj < ny ; jj++ ){
           insert_at_memplot( start + xx + jj*nx , mplot ) ;
           if( ud[12+jj] < WAY_BIG && y[jj][0] < WAY_BIG ){
             yll = yobot + jj*(1.0+SY)*dyo ; yhh = yll + dyo ;
             plotpak_set( xobot,xotop , yll,yhh , xbot,xtop , ybot,ytop , 1 ) ;
             set_color_memplot( ccc[jj%NCLR][0], ccc[jj%NCLR][1], ccc[jj%NCLR][2] ) ;
             plotpak_line( xx , ud[12+jj] , xx+1 , y[jj][0] ) ;
           } else {
             ADDTO_MEMPLOT( mplot , 0.0,0.0,0.0,0.0,0.0,-THCODE_INVALID ) ;
           }
         }
         xx++ ; if( xx == nx ) xx = 0 ; /* start plotting at next point */

      } else {        /* only happens 1st time in */
         xx = 0 ;
      }

      for( ii=1 ; ii < nadd ; ii++ ){
         for( jj=0 ; jj < ny ; jj++ ){
           insert_at_memplot( start + xx + jj*nx , mplot ) ;
           if( y[jj][ii-1] < WAY_BIG && y[jj][ii] < WAY_BIG ){
             yll = yobot + jj*(1.0+SY)*dyo ; yhh = yll + dyo ;
             plotpak_set( xobot,xotop , yll,yhh , xbot,xtop , ybot,ytop , 1 ) ;
             set_color_memplot( ccc[jj%NCLR][0],ccc[jj%NCLR][1],ccc[jj%NCLR][2] );
             plotpak_line( xx , y[jj][ii-1] , xx+1 , y[jj][ii] ) ;
           } else {
             ADDTO_MEMPLOT( mplot , 0.0,0.0,0.0,0.0,0.0,-THCODE_INVALID ) ;
           }
         }
         xx++ ; if( xx == nx ) xx = 0 ;
      }

      /* 16 Nov 2001: add X at the end */

      set_thick_memplot( 2*THIK ) ;

      ii = nadd-1 ;
      for( jj=0 ; jj < ny ; jj++ ){
        if( y[jj][ii] < WAY_BIG ){
          yll = yobot + jj*(1.0+SY)*dyo ; yhh = yll + dyo ;
          plotpak_set( xobot,xotop , yll,yhh , xbot,xtop , ybot,ytop , 1 ) ;
          set_color_memplot( ccc[jj%NCLR][0],ccc[jj%NCLR][1],ccc[jj%NCLR][2] );
          insert_at_memplot( start + nx*ny + 2*jj , mplot ) ;
          plotpak_line( xx-dxx , y[jj][ii]-dyy , xx+dxx , y[jj][ii]+dyy ) ;
          insert_at_memplot( start + nx*ny + 2*jj+1 , mplot ) ;
          plotpak_line( xx-dxx , y[jj][ii]+dyy , xx+dxx , y[jj][ii]-dyy ) ;
        } else {
          insert_at_memplot( start + nx*ny + 2*jj , mplot ) ;
          ADDTO_MEMPLOT( mplot , 0.0,0.0,0.0,0.0,0.0,-THCODE_INVALID ) ;
          insert_at_memplot( start + nx*ny + 2*jj+1 , mplot ) ;
          ADDTO_MEMPLOT( mplot , 0.0,0.0,0.0,0.0,0.0,-THCODE_INVALID ) ;
        }
      }

   }

   /*- reset plot parameters -*/

   insert_at_memplot( -1 , mplot ) ;
   set_thick_memplot( 0.0 ) ;
   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;

   /*- redisplay the plot (all of it, Frank) -*/

   redraw_topshell( mp ) ;

   /*- save some stuff for next time in -*/

   ud[11] = xx ;                      /* last x index plotted */
   for( jj=0 ; jj < ny ; jj++ )       /* last y values plotted */
      ud[12+jj] = y[jj][nadd-1] ;

   return ;
}

/*---------------------------------------------------------------------------
  clear out the data graph from a strip plot, leaving the labels, etc.
  (by voiding out the graph lines from the data, then redrawing)
-----------------------------------------------------------------------------*/

void plot_strip_clear( MEM_topshell_data * mp )
{
   int ii , jj , start , nx , ny ;
   float * ud ;
   MEM_plotdata *mplot ;

   if( mp == NULL || mp->userdata == NULL || !mp->valid  ) return ;

   ud = (float *) mp->userdata ;
   nx = ud[5] ; ny = ud[8] ; start = ud[10] ;

   mplot = mp->mp ;

   for( jj=0 ; jj < ny ; jj++ ){

     for( ii=0 ; ii < nx ; ii++ ){                      /* clear graph lines */
        insert_at_memplot( start + ii + jj*nx , mplot ) ;
        ADDTO_MEMPLOT( mplot , 0.0,0.0,0.0,0.0,0.0,-THCODE_INVALID ) ;
     }
                                                        /* clear X */
     insert_at_memplot( start + nx*ny + 2*jj , mplot ) ;
     ADDTO_MEMPLOT( mplot , 0.0,0.0,0.0,0.0,0.0,-THCODE_INVALID ) ;
     insert_at_memplot( start + nx*ny + 2*jj+1 , mplot ) ;
     ADDTO_MEMPLOT( mplot , 0.0,0.0,0.0,0.0,0.0,-THCODE_INVALID ) ;
   }

   insert_at_memplot( -1 , mplot ) ;  /* reset to normal insertion */
   redraw_topshell( mp ) ;

   ud[11] = -1 ;  /* reset current x index */
   return ;
}
