#include "coxplot.h"
#include <math.h>

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#ifndef WAY_BIG
#  define WAY_BIG 1.e+10
#endif

static float p10( float x ) ;  /* prototype */

#undef  NCLR_MAX
#define NCLR_MAX 19
static float ccc[NCLR_MAX][3] = {
  { 0.0 , 0.0 , 0.0 } ,
  { 0.9 , 0.0 , 0.0 } ,
  { 0.0 , 0.7 , 0.0 } ,
  { 0.0 , 0.0 , 0.9 } ,
  { 0.8 , 0.0 , 0.9 } ,
  { 0.7 , 0.6 , 0.0 } ,
} ;

static int NCLR = 6 ;
static int dont_init_colors=0 ;

static int ilab[4] = { 0,2,3,1 } ;  /* whether to plot labels on axes */

#define STGOOD(s) ( (s) != NULL && (s)[0] != '\0' )

#define SY   0.07

static float THIK = 0.003 ;  /* 27 Mar 2004: changed from a #define */

/*----------------------------------------------------------------------*/
static int xpush=1 , ypush=1 ;

void plot_ts_xypush( int a , int b ){ xpush=a; ypush=b; }  /* 12 Mar 2003 */

static float xxbot,xxtop , yybot,yytop ;
static int   nnaxx=-1,mmaxx=-1 , nnayy=-1,mmayy=-1 ;

void plot_ts_xfix( int nax, int max, float xb, float xt )  /* 22 Jul 2003 */
{
  nnaxx = nax ; mmaxx = max ; xxbot = xb ; xxtop = xt ;
}

void plot_ts_yfix( int nay, int may, float yb, float yt )
{
  nnayy = nay ; mmayy = may ; yybot = yb ; yytop = yt ;
}

/*----------------------------------------------------------------------*/
/* Check to define colors for plotting from environment variables.
------------------------------------------------------------------------*/

static void init_colors(void)
{
   static int first=1 ;
   char ename[32] , *eee ;
   float rf,gf,bf ;
   int ii ;

   if( dont_init_colors ){ first=1 ; return; }
   if( !first ) return ;
   first = 0 ;

   /* init ii to 0 (was 1) to match README.environment: 19 May 2004 [rickr] */

   for( ii=0 ; ii < NCLR_MAX ; ii++ ){
     sprintf(ename,"AFNI_1DPLOT_COLOR_%02d",ii+1) ;
     eee = getenv(ename) ;
     if( eee == NULL && ii < 9 ){    /** 21 Apr 2005: check alternatives **/
       sprintf(ename,"AFNI_1DPLOT_COLOR_%1d",ii+1) ; eee = getenv(ename) ;
     }
     if( eee == NULL && ii <= 9 ){
       sprintf(ename,"AFNI_1DPLOT_COLOR_O%1d",ii+1) ; eee = getenv(ename) ;
     }
     if( eee != NULL && *eee != '\0' ){
       rf=gf=bf = -1.0 ;
       (void)sscanf( eee , "rgbi:%f/%f/%f" , &rf,&gf,&bf ) ;
       if( rf >= 0.0 && rf <= 1.0 &&
           gf >= 0.0 && gf <= 1.0 &&
           bf >= 0.0 && bf <= 1.0   ){

         ccc[ii][0] = rf ; ccc[ii][1] = gf ; ccc[ii][2] = bf ;
         NCLR = ii+1 ;
       } else if( strcasecmp(eee,"green") == 0 ){
         ccc[ii][0] = 0.0f; ccc[ii][1] = 0.8f; ccc[ii][2] = 0.0f; NCLR = ii+1;
       } else if( strcasecmp(eee,"red") == 0 ){
         ccc[ii][0] = 0.8f; ccc[ii][1] = 0.0f; ccc[ii][2] = 0.0f; NCLR = ii+1;
       } else if( strcasecmp(eee,"blue") == 0 ){
         ccc[ii][0] = 0.0f; ccc[ii][1] = 0.0f; ccc[ii][2] = 0.8f; NCLR = ii+1;
       } else if( strcasecmp(eee,"black") == 0 ){
         ccc[ii][0] = 0.0f; ccc[ii][1] = 0.0f; ccc[ii][2] = 0.0f; NCLR = ii+1;
       } else if( strcasecmp(eee,"purple") == 0 || strcasecmp(eee,"violet") == 0 ){
         ccc[ii][0] = 0.8f; ccc[ii][1] = 0.0f; ccc[ii][2] = 0.8f; NCLR = ii+1;
       } else if( strcasecmp(eee,"gold") == 0 || strcasecmp(eee,"yellow") == 0 ){
         ccc[ii][0] = 0.8f; ccc[ii][1] = 0.6f; ccc[ii][2] = 0.0f; NCLR = ii+1;
       } else if( *eee == '#' && *(eee+1) != '\0' ){
         int le=strlen(eee+1) , val , bas , rr,gg,bb ;
         val = (int)strtol( eee+1 , NULL , 16 ) ;
         bas = (le <= 3) ? 16 : 256 ;
         bb  = val % bas ; val = val / bas ; bf  = bb / ((float)bas) ;
         gg  = val % bas ; val = val / bas ; gf  = gg / ((float)bas) ;
         rr  = val % bas ;                   rf  = rr / ((float)bas) ;
         ccc[ii][0] = rf ; ccc[ii][1] = gf ; ccc[ii][2] = bf ; NCLR = ii+1 ;
       } else {
         fprintf(stderr, "** ERROR: %s = %s is not a recognizable color\n", ename,eee ) ;
       }
     }
   }

   eee = getenv("AFNI_1DPLOT_THIK") ;                     /* 27 Mar 2004 */
   if( eee == NULL ) eee = getenv("AFNI_1DPLOT_THICK") ;  /* 15 Apr 2009 */
   if( eee != NULL ){
     rf = (float)strtod(eee,NULL) ;
     if( rf >= 0.0f && rf <= 0.05001f ) THIK = rf ;
     else
       fprintf(stderr,"** ERROR: AFNI_1DPLOT_THIK is not in usable range [0,0.05]\n") ;
   }
}

/*-----------------------------------------------------------------------*/
/* 23 Nov 2007: set colors explicitly */

void plot_ts_setcolors( int ncol , float *rrr , float *ggg , float *bbb )
{
   int ii ;

   if( ncol <= 0 || rrr==NULL || ggg==NULL || bbb==NULL ){
     NCLR = 4 ; dont_init_colors=0 ;
     ccc[0][0] = 0.0 ; ccc[0][1] = 0.0 ; ccc[0][2] = 0.0 ;
     ccc[1][0] = 0.9 ; ccc[1][1] = 0.0 ; ccc[1][2] = 0.0 ;
     ccc[2][0] = 0.0 ; ccc[2][1] = 0.7 ; ccc[2][2] = 0.0 ;
     ccc[3][0] = 0.0 ; ccc[3][1] = 0.0 ; ccc[3][2] = 0.9 ;
   } else {
     if( ncol > NCLR_MAX ) ncol = NCLR_MAX ;
     dont_init_colors = 1 ;
     for( ii=0 ; ii < ncol ; ii++ ){
       ccc[ii][0] = rrr[ii] ; ccc[ii][1] = ggg[ii] ; ccc[ii][2] = bbb[ii] ;
     }
     if( ncol > NCLR ) NCLR = ncol ;
   }
   return ;
}

void plot_ts_setthik( float thk )
{
   THIK = (thk >= 0.0f && thk <= 0.05001f) ? thk : 0.003f ;
}

/*-----------------------------------------------------------------------
  Plot some timeseries data into an in-memory plot structure, which
  must be displayed later in some fashion.
  If array x[] is NULL, then routine will make an x-axis up.

  ymask details what to do with y graphs:
     ymask & TSP_SEPARATE_YBOX    => individual boxes for each y[i][]
     ymask & TSP_SEPARATE_YSCALE  => and individual scales

  27 Jan 1999: all routines are modified to leave the plotpak_set()
               transform for the data at box #0 as the last setting
-------------------------------------------------------------------------*/

MEM_plotdata * plot_ts_mem( int nx , float *x , int ny , int ymask , float **y ,
                            char *lab_xxx , char *lab_yyy , char *lab_top ,
                            char **nam_yyy )
{
   int ii , jj , np , nnax,nnay , mmax,mmay ;
   float *xx , *yy ;
   float xbot,xtop , ybot,ytop , pbot,ptop , xobot,xotop,yobot,yotop ;
   char str[32] ;
   int yall , ysep ;
   float *ylo , *yhi , yll,yhh ;
   MEM_plotdata *mp ;

   /*-- sanity check --*/

   if( nx <= 1 || ny == 0 || y == NULL ) return NULL ;

   init_colors() ;

   /*-- make up an x-axis if none given --*/

   if( x == NULL ){
      xx = (float *) malloc( sizeof(float) * nx ) ;
      for( ii=0 ; ii < nx ; ii++ ) xx[ii] = ii ;
      xbot = 0 ; xtop = nx-1 ;
   } else {
      xx = x ;
      xbot = WAY_BIG ; xtop = -WAY_BIG ;
      for( ii=0 ; ii < nx ; ii++ ){
         if( xx[ii] < xbot && xx[ii] < WAY_BIG ) xbot = xx[ii] ;
         if( xx[ii] > xtop && xx[ii] < WAY_BIG ) xtop = xx[ii] ;
      }
      if( xbot >= xtop ) return NULL ;
   }

   /*-- push range of x outwards --*/

   pbot = p10(xbot) ; ptop = p10(xtop) ; if( ptop < pbot ) ptop = pbot ;
   if( nnaxx >= 0 ){
     nnax = nnaxx ; nnaxx = -1 ;
     mmax = mmaxx ;
     xbot = xxbot ;
     xtop = xxtop ;
   } else if( ptop != 0.0 && xpush > 0 ){
      np = (xtop-xbot) / ptop ;
      switch( np ){
         case 1:  ptop *= 0.1  ; break ;
         case 2:  ptop *= 0.2  ; break ;
         case 3:  ptop *= 0.25 ; break ;
         case 4:
         case 5:  ptop *= 0.5  ; break ;
      }
      xbot = floor( xbot/ptop ) * ptop ;
      xtop =  ceil( xtop/ptop ) * ptop ;
      nnax = floor( (xtop-xbot) / ptop + 0.5 ) ;
      mmax = (nnax < 3) ? 10
                        : (nnax < 6) ? 5 : 2 ;
   } else {
      nnax = 1 ; mmax = 10 ;
      ii = (int)rint(xtop-xbot) ;
      if( fabs(xtop-xbot-ii) < 0.01 && ii <= 200 ) mmax = ii ;
   }

   /*-- find range of y --*/

   yall = (ny == 1) || ((ymask & TSP_SEPARATE_YBOX) == 0) ;
   ysep = (ymask & TSP_SEPARATE_YSCALE) != 0 ;
                                               /* Nov 1998: find range of */
   ylo = (float *) malloc(sizeof(float)*ny) ;  /* each array separately. */
   yhi = (float *) malloc(sizeof(float)*ny) ;

   ybot = WAY_BIG ; ytop = -WAY_BIG ;
   for( jj=0 ; jj < ny ; jj++ ){
      yy  = y[jj] ; yll = WAY_BIG ; yhh = -WAY_BIG ;
      for( ii=0 ; ii < nx ; ii++ ){
         if( yy[ii] < yll && yy[ii] < WAY_BIG ) yll = yy[ii] ;
         if( yy[ii] > yhh && yy[ii] < WAY_BIG ) yhh = yy[ii] ;
      }
      ylo[jj] = yll ; yhi[jj] = yhh ;
      if( ybot > yll ) ybot = yll ;
      if( ytop < yhh ) ytop = yhh ;
      if( yll >= yhh ){                       /* shouldn't happen */
         yhh = yll + 0.05*fabs(yll) + 0.5 ;
         yll = yll - 0.05*fabs(yll) - 0.5 ;
         ylo[jj] = yll ; yhi[jj] = yhh ;
      }
   }
   if( ybot >= ytop ){                       /* shouldn't happen */
      ytop = ybot + 0.05*fabs(ybot) + 0.5 ;
      ybot = ybot - 0.05*fabs(ybot) - 0.5 ;
   }

   /* 30 Dec 1998 */

   if( !ysep ){
     for( jj=0 ; jj < ny ; jj++ ){ ylo[jj] = ybot ; yhi[jj] = ytop ; }
   }

   /*-- push range of y outwards --*/

   pbot = p10(ybot) ; ptop = p10(ytop) ; if( ptop < pbot ) ptop = pbot ;
   if( nnayy >= 0 ){
     nnay = nnayy ; nnayy = -1 ;
     mmay = mmayy ;
     ybot = yybot ;
     ytop = yytop ;
     for( jj=0 ; jj < ny ; jj++ ){ ylo[jj] = ybot ; yhi[jj] = ytop ; }
   } else if( ptop != 0.0 && ypush > 0 ){
      np = (ytop-ybot) / ptop ;
      switch( np ){
         case 1:  ptop *= 0.1  ; break ;
         case 2:  ptop *= 0.2  ; break ;
         case 3:  ptop *= 0.25 ; break ;
         case 4:
         case 5:  ptop *= 0.5  ; break ;
      }
      ybot = floor( ybot/ptop ) * ptop ;
      ytop =  ceil( ytop/ptop ) * ptop ;
      nnay = floor( (ytop-ybot) / ptop + 0.5 ) ;
      mmay = (nnay < 3) ? 10
                        : (nnay < 6) ? 5 : 2 ;
   } else {
      float dif=(ytop-ybot)*0.005f ;
      if( ypush == 0 ){ ybot -= dif ; ytop += dif ; }
      nnay = 1 ; mmay = 10 ;
   }

   for( jj=0 ; jj < ny ; jj++ ){
      pbot = p10(ylo[jj]) ; ptop = p10(yhi[jj]) ; if( ptop < pbot ) ptop = pbot ;
      if( ptop != 0.0 && ypush > 0 ){
         np = (yhi[jj]-ylo[jj]) / ptop ;
         switch( np ){
            case 1:  ptop *= 0.1  ; break ;
            case 2:  ptop *= 0.2  ; break ;
            case 3:  ptop *= 0.25 ; break ;
            case 4:
            case 5:  ptop *= 0.5  ; break ;
         }
         ylo[jj] = floor( ylo[jj]/ptop ) * ptop ;
         yhi[jj] =  ceil( yhi[jj]/ptop ) * ptop ;
      } else if( ypush == 0 ){
        float dif=(yhi[jj]-ylo[jj])*0.005f ;
        ylo[jj] -= dif ; yhi[jj] += dif ;
      }
   }

   /*-- setup to plot --*/

   create_memplot_surely( "tsplot" , 1.3 ) ;
   set_thick_memplot( 0.002f ) ;  /* for labels */

   /*-- plot labels, if any --*/

   xobot = 0.15 ; xotop = 1.27 ;  /* set objective size of plot */
   yobot = 0.1  ; yotop = 0.95 ;

   if( STGOOD(lab_top) ){ yotop -= 0.02 ; yobot -= 0.01 ; }
   if( nam_yyy != NULL ){ xotop -= 0.16 ; xobot -= 0.02 ; }

   /* x-axis label? */

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   if( STGOOD(lab_xxx) )
     plotpak_pwritf( 0.5*(xobot+xotop) , yobot-0.06 , lab_xxx , 16 , 0 , 0 ) ;

   /* y-axis label? */

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   if( STGOOD(lab_yyy) )
     plotpak_pwritf( xobot-0.10 , 0.5*(yobot+yotop) , lab_yyy , 16 , 90 , 0 ) ;

   /* label at top? */

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
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
             set_thick_memplot( THIK ) ;
             plotpak_line( xotop+0.008 , yv , xotop+0.042 , yv ) ;
             set_thick_memplot( 0.002f ) ;
             set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
             sz = (strlen(nam_yyy[jj]) <= 10) ? 12 : 10 ;
             plotpak_pwritf( xotop+0.048 , yv , nam_yyy[jj] , sz , 0 , -1 ) ;
             yv -= 0.05 ;
           }
         }
      }

      /* plot axes */

      set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
      set_thick_memplot( 0.002f ) ;
      plotpak_set( xobot,xotop , yobot,yotop , xbot,xtop , ybot,ytop , 1 ) ;
      plotpak_perimm( nnax,mmax , nnay,mmay , ilab[(nnax>0)+2*(nnay>0)] ) ;

      /* plot data */

      for( jj=0 ; jj < ny ; jj++ ){
         set_thick_memplot( THIK ) ;
         set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;

         yy = y[jj] ;
         for( ii=1 ; ii < nx ; ii++ ){
            if( xx[ii-1] < WAY_BIG && xx[ii] < WAY_BIG &&
                yy[ii-1] < WAY_BIG && yy[ii] < WAY_BIG   )

               plotpak_line( xx[ii-1] , yy[ii-1] , xx[ii] , yy[ii] ) ;
         }
      }
      set_thick_memplot( 0.002f ) ;
      set_color_memplot( 0.0 , 0.0 , 0.0 ) ;

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
               set_thick_memplot( 0.002f ) ;
               set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
               sz = (strlen(nam_yyy[jj]) <= 10) ? 12 : 10 ;
               plotpak_pwritf( xotop+0.048 , yv , nam_yyy[jj] , sz , 0 , -1 ) ;
            }
         }
      }

      /* data each in its own box */

      for( jj=ny-1 ; jj >= 0 ; jj-- ){
         yll = yobot + jj*(1.0+SY)*dyo ; yhh = yll + dyo ;
         plotpak_set( xobot,xotop , yll,yhh , xbot,xtop , ylo[jj],yhi[jj] , 1 ) ;

         if( nnay > 0 ){
           nnay = 1 ;
           pbot = p10(ylo[jj]) ; ptop = p10(yhi[jj]) ;
           if( ptop > pbot && pbot > 0.0 ) ptop = pbot ;
           if( ptop != 0.0 ) mmay = floor( (yhi[jj]-ylo[jj]) / ptop + 0.5 ) ;
           else              mmay = 5 ;   /* shouldn't happen */

                if( mmay == 1 ) mmay = 5 ;
           else if( mmay == 2 ) mmay = 4 ;
           else if( mmay == 3 ) mmay = 6 ;
         }

         set_thick_memplot( 0.002f ) ;
         set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
         plotpak_perimm( nnax,mmax , nnay,mmay , ilab[(nnax>0)*(jj==0)+2*(nnay>0)] ) ;
         if( ylo[jj] < 0.0 && yhi[jj] > 0.0 ){
           set_thick_memplot( 0.0 ) ;
           plotpak_setlin(5) ;
           plotpak_line( xbot,0.0 , xtop,0.0 ) ;
           plotpak_setlin(1) ;
         }

         set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;
         set_thick_memplot( THIK ) ;

         yy = y[jj] ;
         for( ii=1 ; ii < nx ; ii++ ){
            if( xx[ii-1] < WAY_BIG && xx[ii] < WAY_BIG &&
                yy[ii-1] < WAY_BIG && yy[ii] < WAY_BIG   )

               plotpak_line( xx[ii-1] , yy[ii-1] , xx[ii] , yy[ii] ) ;
         }
         set_thick_memplot( 0.002f ) ;
         set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
      }
   }

   /*-- exit, stage left --*/

   set_thick_memplot( 0.0 ) ;
   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;

   if( xx != x ) free(xx) ;
   free(ylo) ; free(yhi) ;

   mp = get_active_memplot() ;
   return mp ;
}

/*-----------------------------------------------------------------------
  Plot some timeseries data into window (linear-linear scales).
  If array x[] is NULL, then routine will make an x-axis up.
  If nx < 0, this is a flag to scale each y[i][] array separately
  If ny < 0, this is a flag to plot each y[i][] array into a separate
  graph; ny > 0 means all in one graph.
-------------------------------------------------------------------------*/

void plot_ts_lab( Display * dpy ,
                  int nx , float * x , int ny , float ** y ,
                  char * lab_xxx , char * lab_yyy , char * lab_top ,
                  char ** nam_yyy , void_func * killfunc )
{
   MEM_plotdata * mp ;
   int ymask = 0 ;

   if( dpy == NULL ) return ;

   if (nx < 0 ) { ymask = ymask | TSP_SEPARATE_YSCALE; nx = -nx; }
   if( ny < 0 ){ ymask = ymask | TSP_SEPARATE_YBOX ; ny = -ny ; }

   mp = plot_ts_mem( nx,x , ny,ymask,y , lab_xxx , lab_yyy , lab_top , nam_yyy ) ;
   if( mp != NULL )
     (void) memplot_to_topshell( dpy , mp , killfunc ) ;

   return ;
}

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

/*----------------------------------------------------------------------*/

MEM_topshell_data * plot_ts_init( Display * dpy ,
                                  float xbot , float xtop ,
                                  int ny , float ybot , float ytop ,
                                  char * lab_xxx , char * lab_yyy ,
                                  char * lab_top , char ** nam_yyy ,
                                  void_func * killfunc              )
{
   int ii , jj , np , nnax,nnay , mmax,mmay , yall ;
   float pbot,ptop , xobot,xotop,yobot,yotop , yll,yhh ;
   char str[32] ;
   float * ud ;
   MEM_topshell_data * mp ;

   /*-- sanity check --*/

   if( dpy == NULL || ny == 0 || xbot >= xtop || ybot >= ytop ) return NULL ;

   init_colors() ;

   /*-- push range of x outwards --*/

   pbot = p10(xbot) ; ptop = p10(xtop) ; if( ptop < pbot ) ptop = pbot ;
   if( ptop != 0.0 && xpush > 0 ){
      np = (xtop-xbot) / ptop ;
      switch( np ){
         case 1:  ptop *= 0.1  ; break ;
         case 2:  ptop *= 0.2  ; break ;
         case 3:  ptop *= 0.25 ; break ;
         case 4:
         case 5:  ptop *= 0.5  ; break ;
      }
      xbot = floor( xbot/ptop ) * ptop ;
      xtop =  ceil( xtop/ptop ) * ptop ;
      nnax = floor( (xtop-xbot) / ptop + 0.5 ) ;
      mmax = (nnax < 3) ? 10
                        : (nnax < 6) ? 5 : 2 ;
   } else {
      nnax = 1 ; mmax = 10 ;
      ii = (int)rint(xtop-xbot) ;
      if( fabs(xtop-xbot-ii) < 0.01 && ii <= 200 ) mmax = ii ;
   }

   /*-- push range of y outwards --*/

   yall = (ny > 0) ; if( !yall ) ny = -ny ;

   pbot = p10(ybot) ; ptop = p10(ytop) ; if( ptop < pbot ) ptop = pbot ;
   if( ptop != 0.0 && ypush > 0){
      np = (ytop-ybot) / ptop ;
      switch( np ){
         case 1:  ptop *= 0.1  ; break ;
         case 2:  ptop *= 0.2  ; break ;
         case 3:  ptop *= 0.25 ; break ;
         case 4:
         case 5:  ptop *= 0.5  ; break ;
      }
      ybot = floor( ybot/ptop ) * ptop ;
      ytop =  ceil( ytop/ptop ) * ptop ;
      nnay = floor( (ytop-ybot) / ptop + 0.5 ) ;
      mmay = (nnay < 3) ? 10
                        : (nnay < 6) ? 5 : 2 ;
   } else {
      float dif=(ytop-ybot)*0.005f ;
      if( ypush == 0 ){ ybot -= dif ; ytop += dif ; }
      nnay = 1 ; mmay = 10 ;
   }

   /*-- setup to plot --*/

   create_memplot_surely( "Tsplot" , 1.3 ) ;
   set_thick_memplot( 0.5*THIK ) ;

   /*-- plot labels, if any --*/

   xobot = 0.15 ; xotop = 1.27 ;  /* set objective size of plot */
   yobot = 0.1  ; yotop = 0.95 ;

   if( STGOOD(lab_top) ){ yotop -= 0.02 ; yobot -= 0.01 ; }
   if( nam_yyy != NULL ){ xotop -= 0.16 ; xobot -= 0.02 ; }

   /* x-axis label? */

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   if( STGOOD(lab_xxx) )
      plotpak_pwritf( 0.5*(xobot+xotop) , yobot-0.06 , lab_xxx , 16 , 0 , 0 ) ;

   /* y-axis label? */

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   if( STGOOD(lab_yyy) )
      plotpak_pwritf( xobot-0.10 , 0.5*(yobot+yotop) , lab_yyy , 16 , 90 , 0 ) ;

   /* label at top? */

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   if( STGOOD(lab_top) )
      plotpak_pwritf( xobot+0.01 , yotop+0.01 , lab_top , 18 , 0 , -2 ) ;

   /*-- plot all on same vertical scale --*/

   ud = (float *) malloc( sizeof(float) * 8 ) ;
   ud[0] = xobot ; ud[1] = xotop ; ud[2] = yobot ; ud[3] = yotop ;
   ud[4] = xbot  ; ud[5] = xtop  ; ud[6] = ybot  ; ud[7] = ytop  ;

   if( yall ){

      /* do name labels at right? */

      if( nam_yyy != NULL ){
         float yv = yotop ; int sz ;

         for( jj=0 ; jj < ny ; jj++ ){
            if( STGOOD(nam_yyy[jj]) ){
               set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;
               set_thick_memplot( 2*THIK ) ;
               plotpak_line( xotop+0.008 , yv , xotop+0.042 , yv ) ;
               set_thick_memplot( 0.5*THIK ) ;
               set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
               sz = (strlen(nam_yyy[jj]) <= 10) ? 12 : 10 ;
               plotpak_pwritf( xotop+0.048 , yv , nam_yyy[jj] , sz , 0 , -1 ) ;
               yv -= 0.05 ;
            }
         }
      }

      /* plot axes */

      set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
      plotpak_set( xobot,xotop , yobot,yotop , xbot,xtop , ybot,ytop , 1 ) ;
      plotpak_perimm( nnax,mmax , nnay,mmay , ilab[(nnax>0)+2*(nnay>0)] ) ;

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
               set_thick_memplot( 0.5*THIK ) ;
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
         plotpak_set( xobot,xotop , yll,yhh , xbot,xtop , ybot,ytop , 1 ) ;
         set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
         plotpak_perimm( nnax,mmax , nnay,mmay , ilab[(nnax>0)*(jj==0)+2*(nnay>0)] ) ;
         if( ybot < 0.0 && ytop > 0.0 ){
            plotpak_setlin(5) ;
            plotpak_line( xbot,0.0 , xtop,0.0 ) ;
            plotpak_setlin(1) ;
         }
      }
   }

   /*-- display --*/

   mp = memplot_to_topshell( dpy , get_active_memplot() , killfunc ) ;
   if( mp == NULL ){ free(ud) ; return NULL; }
   mp->userdata = ud ;

   /*-- exit, stage left --*/

   return mp ;
}

/*----------------------------------------------------------------------*/

void plot_ts_addto( MEM_topshell_data * mp ,
                    int nx , float * x , int ny , float ** y )
{
   int ii , jj , yall , start ;
   float pbot,ptop , xobot,xotop,yobot,yotop , yll,yhh ;
   float xbot,xtop , ybot,ytop ;
   float * yy , * xx ;
   float * ud ;

   if( mp == NULL || mp->userdata == NULL || ! mp->valid ||
       nx <= 1    || ny == 0              || x == NULL   || y == NULL ) return ;

   init_colors() ;

   ud = (float *) mp->userdata ;
   xobot = ud[0] ; xotop = ud[1] ; yobot = ud[2] ; yotop = ud[3] ;
   xbot  = ud[4] ; xtop  = ud[5] ; ybot  = ud[6] ; ytop  = ud[7] ;

   yall = (ny > 0) ; if( !yall ) ny = -ny ;

   ii = set_active_memplot( MEMPLOT_IDENT(mp->mp) ) ;
   if( ii != 0 ) return ;

   start = MEMPLOT_NLINE(mp->mp) ;
   xx = x ;

   if( yall ){  /*-- all in one big happy box --*/

      plotpak_set( xobot,xotop , yobot,yotop , xbot,xtop , ybot,ytop , 1 ) ;
      set_thick_memplot( THIK ) ;

      /* plot data */

      for( jj=0 ; jj < ny ; jj++ ){
         set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;

         yy = y[jj] ;
         for( ii=1 ; ii < nx ; ii++ ){
            if( xx[ii-1] < WAY_BIG && xx[ii] < WAY_BIG &&
                yy[ii-1] < WAY_BIG && yy[ii] < WAY_BIG   )

               plotpak_line( xx[ii-1] , yy[ii-1] , xx[ii] , yy[ii] ) ;
         }
      }
      set_thick_memplot( 0.0 ) ;
      set_color_memplot( 0.0 , 0.0 , 0.0 ) ;

   } else {  /*-- each in its own little sad box --*/

      float dyo = (yotop-yobot) / ( (1.0+SY) * ny - SY ) ;

      set_thick_memplot( THIK ) ;

      for( jj=ny-1 ; jj >= 0 ; jj-- ){
         yll = yobot + jj*(1.0+SY)*dyo ; yhh = yll + dyo ;
         plotpak_set( xobot,xotop , yll,yhh , xbot,xtop , ybot,ytop , 1 ) ;
         set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;

         yy = y[jj] ;
         for( ii=1 ; ii < nx ; ii++ ){
            if( xx[ii-1] < WAY_BIG && xx[ii] < WAY_BIG &&
                yy[ii-1] < WAY_BIG && yy[ii] < WAY_BIG   )

               plotpak_line( xx[ii-1] , yy[ii-1] , xx[ii] , yy[ii] ) ;
         }
      }
      set_thick_memplot( 0.0 ) ;
      set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   }

   memplot_to_X11_sef( XtDisplay(mp->drawing) , XtWindow(mp->drawing) ,
                       mp->mp , start,0,MEMPLOT_FREE_ASPECT ) ;

   return ;
}
