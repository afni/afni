#include "mrilib.h"
#include "coxplot.h"
#include "display.h"

#define TSGRAY_SEPARATE_YSCALE (1)
#define TSGRAY_FLIP_XY         (1<<1)

/*-----------------------------------------------------------------
   Plot some timeseries in grayscale
     npt     = number of points in each series
     nts     = number of series
     ymask   = operation modifier:
                 TSGRAY_SEPARATE_YSCALE (not implemented)
                 TSGRAY_FLIP_XY
     y[j][i] = i-th point in j-th timeseries,
               for i=0..npt-1, j=0..nts-1
-------------------------------------------------------------------*/

MEM_plotdata * PLOT_tsgray( int npt , int nts , int ymask , float **y )
{
   MEM_plotdata * mp ;
   float ybot,ytop , yfac , dx,dy , val ;
   int ii,jj , flipxy ;
   char str[32] ;

   if( npt < 2 || nts < 1 || y == NULL ) return NULL ;

   /* find range of data */

   ybot = ytop = y[0][0] ;
   for( jj=0 ; jj < nts ; jj++ ){
      for( ii=0 ; ii < npt ; ii++ ){
         val = y[jj][ii] ;
              if( ybot > val ) ybot = val ;
         else if( ytop < val ) ytop = val ;
      }
   }
   if( ybot >= ytop ) return NULL ;
   yfac = 1.0/(ytop-ybot) ;
   dx   = 1.0/npt ;
   dy   = 1.0/nts ;

   create_memplot_surely( "Gplot" , 1.0 ) ;
   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   set_thick_memplot( 0.0 ) ;

   flipxy = (ymask & TSGRAY_FLIP_XY) != 0 ;

   for( jj=0 ; jj < nts ; jj++ ){
      for( ii=0 ; ii < npt ; ii++ ){
         val = yfac*(ytop-y[jj][ii]) ;
         set_color_memplot( val,val,val ) ;
         if( flipxy )
            plotrect_memplot( ii*dx,jj*dy , (ii+1)*dx,(jj+1)*dy ) ;
         else
            plotrect_memplot( jj*dy,1.0-ii*dx , (jj+1)*dy,1.0-(ii+1)*dy ) ;
      }
   }

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   mp = get_active_memplot() ;
   return mp ;
}

/*******************************************************************/

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

void startup_timeout_CB( XtPointer client_data , XtIntervalId * id ) ;

static MCW_DC * dc ;
static int npt , nts , ymask=0 ;
static float **yar ;

/*******************************************************************/

int main( int argc , char * argv[] )
{
   int iarg ;
   int install=0 , ignore=0 , use=0 , jj ;
   char * tsfile , * cpt ;
   char dname[THD_MAX_NAME] , subv[THD_MAX_NAME] ;
   MRI_IMAGE * flim ;
   float *far ;
   XtAppContext app ;
   Widget shell ;
   int out_ps=0 ; /* 28 Feb 2003 */

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dgrayplot [options] tsfile\n"
            "Graphs the columns of a *.1D type time series file to the screen,\n"
            "sort of like 1dplot, but in grayscale.\n"
            "\n"
            "Options:\n"
            " -install   = Install a new X11 colormap (for X11 PseudoColor)\n"
            " -ignore nn = Skip first 'nn' rows in the input file\n"
            "                [default = 0]\n"
            " -flip      = Plot x and y axes interchanged.\n"
            "                [default: data columns plotted DOWN the screen]\n"
            " -use mm    = Plot 'mm' points\n"
            "                [default: all of them]\n"
            " -ps        = Don't draw plot in a window; instead, write it\n"
            "              to stdout in PostScript format.\n"
            "              N.B.: If you view this result in 'gv', you should\n"
            "                    turn 'anti-alias' off, and switch to\n"
            "                    landscape mode.\n"

             "\n"
             TS_HELP_STRING
           ) ;
      exit(0) ;
   }

   machdep() ;

   /* 28 Feb 2003: scan for -ps flage NOW */

   for( jj=1 ; jj < argc ; jj++ )
     if( strcmp(argv[jj],"-ps") == 0 ){ out_ps = 1; break; }

   /* open X11 (if not doing PostScript) */

   if( !out_ps ){
     shell = XtVaAppInitialize(
                &app , "AFNI" , NULL , 0 , &argc , argv , NULL , NULL ) ;
     if( shell == NULL ){
        fprintf(stderr,"** Cannot initialize X11!\n") ; exit(1) ;
     }
   }

   cpt = my_getenv("TMPDIR") ;  /* just for fun */

   /*-- scan arguments that X11 didn't eat --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-ps") == 0 ){   /* 28 Feb 2003: already handled above */
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-flip") == 0 ){
        ymask |= TSGRAY_FLIP_XY ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-install") == 0 ){
        install++ ; iarg++ ; continue ;
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

   if( !out_ps )
     dc = MCW_new_DC( shell , 16 ,
                      DEFAULT_NCOLOVR , INIT_colovr , INIT_labovr ,
                      1.0 , install ) ;

   tsfile = argv[iarg] ;
   flim = mri_read_1D( tsfile ) ;
   if( flim == NULL ){
      fprintf(stderr,"** Can't read input file %s\n",tsfile) ;
      exit(1);
   }
   if( ignore >= flim->nx-1 ) ignore = 0 ;

   if( use == 0 || use > flim->nx-ignore ) use = flim->nx-ignore ;

   far = MRI_FLOAT_PTR(flim) ;
   yar = (float **) malloc( sizeof(float *) * flim->ny ) ;
   for( jj=0 ; jj < flim->ny ; jj++ )
      yar[jj] = far + (jj*flim->nx + ignore) ;

   npt = use ;
   nts = flim->ny ;

   /* start X11 */

   if( !out_ps ){
     (void) XtAppAddTimeOut( app , 123 , startup_timeout_CB , NULL ) ;
     XtAppMainLoop(app) ;  /* never returns */
   }

   /* if get here, plot PostScript to stdout */

   { MEM_plotdata *mp ;
     mp = PLOT_tsgray( npt , nts , ymask , yar ) ;
     memplot_to_postscript( "-" , mp ) ;
   }
   exit(0) ;
}

/*******************************************************************/
/********** These functions are used for the X11 graphing. *********/

/*-----------------------------------------------------------------*/
void killfunc(void * fred){ exit(0) ; }
/*-----------------------------------------------------------------*/

void startup_timeout_CB( XtPointer client_data , XtIntervalId * id )
{
   MEM_plotdata * mp ;

   mp = PLOT_tsgray( npt , nts , ymask , yar ) ;

   if( mp != NULL )
      (void) memplot_to_topshell( dc->display , mp , killfunc ) ;

   return ;
}
