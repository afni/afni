/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "coxplot.h"
#include "display.h"

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

#define STGOOD(s) ( (s) != NULL && (s)[0] != '\0' )

/*-----------------------------------------------------------------
   Plot a scatterplot with ellipses:
     npt  = # of points in x[] and y[]
     x    = x-axis values array
     y    = y-axis values array
     xsig = x standard deviation array
     ysig = y standard deviation array
     corr = correlation coefficient array
     pell = CDF for bivariate normal to be inside ellipse
     xlab } labels for x-axis,
     ylab }            y-axis
     tlab }        and top of graph (NULL => skip this label)
-------------------------------------------------------------------*/

MEM_plotdata * PLOT_scatterellipse( int npt ,
                                    float xlo,float xhi , float ylo,float yhi ,
                                    float * x , float * y ,
                                    float * xsig , float * ysig , float * corr ,
                                    float pell ,
                                    char * xlab , char * ylab , char * tlab )
{
   int ii,jj , np , nnax,mmax , nnay,mmay ;
   float xbot,xtop , ybot,ytop , pbot,ptop ,
         xobot,xotop,yobot,yotop , xa,xb,ya,yb , dx,dy,dt ;
   char str[32] ;
   MEM_plotdata * mp ;
   float xu,yu , xd,yd ;

   if( npt < 2 || x == NULL || y == NULL ) return NULL ;
   if( xsig == NULL || ysig == NULL || corr == NULL ) return NULL ;

   if( pell <= 0.0 || pell >= 1.0 ) pell = 0.5 ;

   pell = -2.0 * log(1.0-pell) ;

   /* find range of data */

   xbot = xtop = x[0] ; ybot = ytop = y[0] ;
   for( ii=0 ; ii < npt ; ii++ ){
      xu = x[ii] + pell*xsig[ii] ;
      xd = x[ii] - pell*xsig[ii] ;
      if( xd < xbot ) xbot = xd ;
      if( xu > xtop ) xtop = xu ;

      yu = y[ii] + pell*ysig[ii] ;
      yd = y[ii] - pell*ysig[ii] ;
      if( yd < ybot ) ybot = yd ;
      if( yu > ytop ) ytop = yu ;
   }
   if( xbot >= xtop || ybot >= ytop ){
      fprintf(stderr,"*** Data has no range in PLOT_scatterellipse!\n\a");
      return NULL ;
   }

   if( xlo < xhi ){ xbot = xlo ; xtop = xhi ; }
   if( ylo < yhi ){ ybot = ylo ; ytop = yhi ; }

   /*-- push range of x outwards --*/

#if 0
   pbot = p10(xbot) ; ptop = p10(xtop) ; if( ptop < pbot ) ptop = pbot ;
   if( ptop != 0.0 ){
      np = (xtop-xbot) / ptop + 0.5 ;
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
   }
#else
      nnax = 1 ; mmax = 10 ;
#endif

   /*-- push range of y outwards --*/

#if 0
   pbot = p10(ybot) ; ptop = p10(ytop) ; if( ptop < pbot ) ptop = pbot ;
   if( ptop != 0.0 ){
      np = (ytop-ybot) / ptop + 0.5 ;
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
      nnay = 1 ; mmay = 10 ;
   }
#else
      nnay = 1 ; mmay = 10 ;
#endif

   /*-- setup to plot --*/

   for( np=0 ; np < 1000 ; np++ ){
      sprintf( str , "sellplot#%03d" , np ) ;
      ii = create_memplot( str , 1.3 ) ;
      if( ii == 0 ) break ;
   }
   if( np == 1000 ) return NULL ;  /* should never happen */

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   set_thick_memplot( 0.0 ) ;

   /*-- plot labels, if any --*/

   xobot = 0.15 ; xotop = 1.27 ;  /* set objective size of plot */
   yobot = 0.1  ; yotop = 0.95 ;

   if( STGOOD(tlab) ){ yotop -= 0.02 ; yobot -= 0.01 ; }

   /* x-axis label? */

   if( STGOOD(xlab) )
      plotpak_pwritf( 0.5*(xobot+xotop) , yobot-0.06 , xlab , 16 , 0 , 0 ) ;

   /* y-axis label? */

   if( STGOOD(ylab) )
      plotpak_pwritf( xobot-0.12 , 0.5*(yobot+yotop) , ylab , 16 , 90 , 0 ) ;

   /* label at top? */

   if( STGOOD(tlab) )
      plotpak_pwritf( xobot+0.01 , yotop+0.01 , tlab , 18 , 0 , -2 ) ;

   /* plot axes */

   plotpak_set( xobot,xotop , yobot,yotop , xbot,xtop , ybot,ytop , 1 ) ;
   plotpak_periml( nnax,mmax , nnay,mmay ) ;

   /* plot data */

#define NELL 64                /* should be divisible by 4 */
#define DTH  (2.0*PI/NELL)

   for( ii=0 ; ii < npt ; ii++ ){
      dx = pell * xsig[ii] ;
      dy = pell * ysig[ii] ;
      dt = asin(corr[ii]) ;
      xb = x[ii] + dx ;
      yb = y[ii] + dy*sin(dt) ;
      for( jj=1 ; jj <= NELL ; jj++ ){
         xa = xb ; ya = yb ;
         xb = x[ii] + dx*cos(jj*DTH) ;
         yb = y[ii] + dy*sin(jj*DTH+dt) ;
         plotpak_line( xa,ya , xb,yb ) ;
      }
   }

   set_color_memplot( 1.0 , 0.0 , 0.0 ) ;
   for( ii=0 ; ii < npt-1 ; ii++ )
     plotpak_line( x[ii],y[ii] , x[ii+1],y[ii+1] ) ;

#define DSQ 0.005

   dx = DSQ*(xtop-xbot) ;
   dy = DSQ*(ytop-ybot) * (xotop-xobot)/(yotop-yobot) ;
   set_color_memplot( 0.0 , 0.0 , 1.0 ) ;
   for( ii=0 ; ii < npt ; ii++ ){
      xa = x[ii] - dx ; xb = x[ii] + dx ;
      ya = y[ii] - dy ; yb = y[ii] + dy ;
      plotpak_line( xa,ya , xa,yb ) ;
      plotpak_line( xa,yb , xb,yb ) ;
      plotpak_line( xb,yb , xb,ya ) ;
      plotpak_line( xb,ya , xa,ya ) ;
   }

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;

   mp = get_active_memplot() ;
   return mp ;
}

/*---- quickie program to look at some graphs - RWCox - Feb 1999 ----*/

#define DEFAULT_NCOLOVR 20

static char * INIT_colovr[DEFAULT_NCOLOVR] = {
   "#ffff00" , "#ffcc00"   , "#ff9900"  , "#ff6900" , "#ff4400" , "#ff0000" ,
   "#0000ff" , "#0044ff"   , "#0069ff"  , "#0099ff" , "#00ccff" , "#00ffff" ,
   "green"   , "limegreen" , "violet"   , "hotpink" ,
   "white"   , "#dddddd"   , "#bbbbbb"  , "black"
} ;

static char * INIT_labovr[DEFAULT_NCOLOVR] = {
   "yellow" , "yell-oran" , "oran-yell" , "orange"   , "oran-red" , "red"   ,
   "dk-blue", "blue"      , "lt-blue1"  , "lt-blue2" , "blue-cyan", "cyan"  ,
   "green"  , "limegreen" , "violet"    , "hotpink"  ,
   "white"  , "gry-dd"    , "gry-bb"    , "black"
} ;

static int nx ;
static float *xx, *yy, *xsig, *ysig, *xycor , pell=0.5 ;

static float xlo=1.0,xhi=-1.0 , ylo=1.0,yhi=-1.0 ;

static MCW_DC * dc ;
static char * title = NULL , * xlabel = NULL , * ylabel = NULL ;

void startup_timeout_CB( XtPointer client_data , XtIntervalId * id ) ;

int main( int argc , char * argv[] )
{
   int iarg , ii , ny , ignore=0 , use=0 , install=0 ;
   char * tsfile , * cpt ;
   char dname[THD_MAX_NAME] , subv[THD_MAX_NAME] ;
   MRI_IMAGE * flim ;
   float * far ;
   XtAppContext app ;
   Widget shell ;
   int cxx=0 , cyy=1 , cxsig=2 , cysig=3 , crho=4 ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dsigplot [options] infile\n"
            "Scatterplots a 5 column *.1D file to the screen.\n"
            "The columns of the input file must be\n"
            "   x  y  sigma_x sigma_y rho_xy\n"
            "\n"
            "Options:\n"
            " -install   = Install a new X11 colormap.\n"
            " -ignore nn = Skip first 'nn' rows in the input file\n"
            "                [default = 0]\n"
            " -use mm    = Plot 'mm' points [default = all of them]\n"
            " -xlabel aa = Put string 'aa' below the x-axis\n"
            "                [default = no axis label]\n"
            " -ylabel aa = Put string 'aa' to the left of the y-axis\n"
            "                [default = no axis label]\n"
            " -pell p    = Set CDF probability contour level for ellipses.\n"
            " -col abcde = 'abcde' is a permutation of '01234', and\n"
            "                specifies the column order for the data,\n"
            "                where a=column index for x\n"
            "                      b=column index for y\n"
            "                      c=column index for sigma_x\n"
            "                      d=column index for sigma_y\n"
            "                      e=column index for rho_xy\n"
            "                [default = 01234]\n"
            "\n"
            " -xrange x1 x2 = Range of x-axis\n"
            " -yrange y1 y2 = Range of y-axis\n"
           ) ;
      exit(0) ;
   }

   machdep() ;

   /* open X11 */

   shell = XtVaAppInitialize(
              &app , "AFNI" , NULL , 0 , &argc , argv , NULL , NULL ) ;
   if( shell == NULL ){
      fprintf(stderr,"** Cannot initialize X11!\n") ; exit(1) ;
   }

   cpt = my_getenv("TMPDIR") ;  /* just for fun */

   /*-- scan arguments that X11 didn't eat --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-xrange") == 0 ){
        xlo = strtod(argv[++iarg],NULL) ;
        xhi = strtod(argv[++iarg],NULL) ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-yrange") == 0 ){
        ylo = strtod(argv[++iarg],NULL) ;
        yhi = strtod(argv[++iarg],NULL) ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-pell") == 0 ){
        pell = strtod(argv[++iarg],NULL) ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-col") == 0 ){
        int ierr=0 ;
        iarg++ ;
        cxx   = argv[iarg][0] - '0' ; if( cxx   < 0 || cxx   > 4 ) ierr++ ;
        cyy   = argv[iarg][1] - '0' ; if( cyy   < 0 || cyy   > 4 ) ierr++ ;
        cxsig = argv[iarg][2] - '0' ; if( cxsig < 0 || cxsig > 4 ) ierr++ ;
        cysig = argv[iarg][3] - '0' ; if( cysig < 0 || cysig > 4 ) ierr++ ;
        crho  = argv[iarg][4] - '0' ; if( crho  < 0 || crho  > 4 ) ierr++ ;
        if( ierr || cxx+cyy+cxsig+cysig+crho != 10 ){
           fprintf(stderr,"*** Illegal argument after -ord!\n");exit(1);
        }
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-install") == 0 ){
        install++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-") == 0 ){  /* skip */
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-title") == 0 ){
        title = argv[++iarg] ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-xlabel") == 0 ){
        xlabel = argv[++iarg] ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-ylabel") == 0 ){
        ylabel = argv[++iarg] ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-ignore") == 0 ){
        ignore = strtod( argv[++iarg] , NULL ) ;
        if( ignore < 0 ){fprintf(stderr,"** Illegal -ignore value!\n");exit(1);}
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-use") == 0 ){
        use = strtod( argv[++iarg] , NULL ) ;
        if( use < 2 ){fprintf(stderr,"** Illegal -use value!\n");exit(1);}
        iarg++ ; continue ;
     }

     fprintf(stderr,"** Unknown option: %s\n",argv[iarg]) ; exit(1) ;
   }

   if( iarg >= argc ){
      fprintf(stderr,"** No tsfile on command line!\n") ; exit(1) ;
   }

   dc = MCW_new_DC( shell , 16 ,
                    DEFAULT_NCOLOVR , INIT_colovr , INIT_labovr ,
                    1.0 , install ) ;

   flim = mri_read_1D( argv[iarg] ) ;
   if( flim == NULL ){
      fprintf(stderr,"** Can't read input file %s\n",argv[iarg]) ;
      exit(1);
   }

   if( flim->ny != 5 ){
      fprintf(stderr,"** Input file doesn't have exactly 5 columns!\n") ;
      exit(1) ;
   }
   far  = MRI_FLOAT_PTR(flim) ;
   nx   = flim->nx ;

   xx    = far + cxx  *nx ;
   yy    = far + cyy  *nx ;
   xsig  = far + cxsig*nx ;
   ysig  = far + cysig*nx ;
   xycor = far + crho *nx ;

   nx = nx - ignore ;  /* cut off the ignored points */

   if( use > 1 && nx > use ) nx = use ;

   /* start X11 */

   (void) XtAppAddTimeOut( app , 123 , startup_timeout_CB , NULL ) ;

   XtAppMainLoop(app) ;
   exit(0) ;
}

/*-----------------------------------------------------------------*/
void killfunc(void * fred){ exit(0) ; }
/*-----------------------------------------------------------------*/

void startup_timeout_CB( XtPointer client_data , XtIntervalId * id )
{
   MEM_plotdata * mp ;

   mp = PLOT_scatterellipse( nx , xlo,xhi,ylo,yhi ,
                             xx,yy,xsig,ysig,xycor ,
                             pell , xlabel,ylabel,title ) ;

   if( mp != NULL )
      (void) memplot_to_topshell( dc->display , mp , killfunc ) ;

   return ;
}
