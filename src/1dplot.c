/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "coxplot.h"
#include "display.h"

/*-------------------------------------------------------------------*/
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

static int nx,nts , sep=1 ;
static float ** yar , * xar ;
static MCW_DC * dc ;
static char * title = NULL , * xlabel = NULL , * ylabel = NULL ;

static char * dfile_nar[6] = {
         "Roll [\\degree]" , "Pitch [\\degree]" , "Yaw [\\degree]"    ,
         "\\Delta I-S [mm]" , "\\Delta R-L [mm]" , "\\Delta A-P [mm]"  } ;

static int    nyar = 0 ;
static char * ynar[128] ;
static char ** yname = NULL ;

void startup_timeout_CB( XtPointer client_data , XtIntervalId * id ) ;

/*-----------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int iarg , ii , ny , ignore=0 , use=0 , install=0 ;
   float dx=1.0 , xzero=0.0 ;
   char *cpt ;
   MRI_IMAGE * inim , * flim ;
   float * far ;
   XtAppContext app ;
   Widget shell ;
   int use_stdin=0 ; /* 01 Aug 2001 */
   int out_ps   =0 ; /* 29 Nov 2002 */
   int nopush   =0 ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dplot [options] tsfile ...\n"
            "Graphs the columns of a *.1D type time series file to the screen.\n"
            "\n"
            "Options:\n"
            " -install   = Install a new X11 colormap.\n"
            " -sep       = Plot each column in a separate sub-graph.\n"
            " -one       = Plot all columns together in one big graph.\n"
            "                [default = -sep]\n"
            " -dx xx     = Spacing between points on the x-axis is 'xx'\n"
            "                [default = 1]\n"
            " -xzero zz  = Initial x coordinate is 'zz' [default = 0]\n"
            " -nopush    = Don't 'push' axes ranges outwards.\n"
            " -ignore nn = Skip first 'nn' rows in the input file\n"
            "                [default = 0]\n"
            " -use mm    = Plot 'mm' points [default = all of them]\n"
            " -xlabel aa = Put string 'aa' below the x-axis\n"
            "                [default = no axis label]\n"
            " -ylabel aa = Put string 'aa' to the left of the y-axis\n"
            "                [default = no axis label]\n"
            "\n"
            " -stdin     = Don't read from tsfile; instead, read from\n"
            "              stdin and plot it. You cannot combine input\n"
            "              from stdin and tsfile(s).  If you want to do\n"
            "              so, see program 1dcat.\n"
            "\n"
            " -ps        = Don't draw plot in a window; instead, write it\n"
            "              to stdout in PostScript format.\n"
            "              N.B.: If you view this result in 'gv', you should\n"
            "                    turn 'anti-alias' off, and switch to\n"
            "                    landscape mode.\n"
            "\n"
            " -ynames aa bb ... = Use the strings 'aa', 'bb', etc., as\n"
            "                     labels to the right of the graphs,\n"
            "                     corresponding to each input column.\n"
            "                     These strings CANNOT start with the\n"
            "                     '-' character.\n"
            "\n"
            " -volreg           = Makes the 'ynames' be the same as the\n"
            "                     6 labels used in plug_volreg for\n"
            "                     Roll, Pitch, Yaw, I-S, R-L, and A-P\n"
            "                     movements, in that order.\n"
            "\n"
            "You may also select a subset of columns to display using\n"
            "a tsfile specification like 'fred.1D[0,3,5]', indicating\n"
            "that columns #0, #3, and #5 will be the only ones plotted.\n"
            "For more details on this selection scheme, see the output\n"
            "of '3dcalc -help'.\n"
            "\n"
            "Example: graphing a 'dfile' output by 3dvolreg, when TR=5:\n"
            "   1dplot -volreg -dx 5 -xlabel Time 'dfile[1..6]'\n"
            "\n"
            "You can also input more than one tsfile, in which case the files\n"
            "will all be plotted.  However, if the files have different column\n"
            "lengths, the shortest one will rule.\n"
            "\n"
            "The colors for the line graphs cycle between black, red, green, and\n"
            "blue.  You can alter these colors by setting Unix environment\n"
            "variables of the form AFNI_1DPLOT_COLOR_xx -- cf. README.environment.\n"

            "\n"
            TS_HELP_STRING
           ) ;
      exit(0) ;
   }

   mainENTRY("1dplot main"); machdep();

   /* 29 Nov 2002: scan for -ps */

   for( ii=1 ; ii < argc ; ii++ )
     if( strcmp(argv[ii],"-ps") == 0 ){ out_ps = 1; break; }

   /* open X11 */

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

     if( strcmp(argv[iarg],"-nopush") == 0 ){  /* 12 Mar 2003 */
       plot_ts_xypush( 0 , 0 ) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-ps") == 0 ){   /* 29 Nov 2002: already handled above */
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-install") == 0 ){
        install++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-stdin") == 0 ){  /* 01 Aug 2001 */
        use_stdin++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-") == 0 ){  /* skip */
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-ynames") == 0 ){
        iarg++ ;
        while( iarg < argc && argv[iarg][0] != '-' ){
           ynar[nyar++] = argv[iarg++] ;
        }
        continue ;
     }

     if( strcmp(argv[iarg],"-volreg") == 0 ){
        int ii ;
        for( ii=0 ; ii < 6 ; ii++ ) ynar[nyar++] = dfile_nar[ii] ;
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

     if( strcmp(argv[iarg],"-dx") == 0 ){
        dx = strtod( argv[++iarg] , NULL ) ;
        if( dx <= 0.0 ){fprintf(stderr,"** Illegal -dx value!\n");exit(1);}
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-xzero") == 0 ){
        xzero = strtod( argv[++iarg] , NULL ) ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-sep") == 0 ){
        sep = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-one") == 0 ){
        sep = 0 ; iarg++ ; continue ;
     }

     fprintf(stderr,"** Unknown option: %s\n",argv[iarg]) ; exit(1) ;
   }

   if( iarg >= argc && !use_stdin ){
      fprintf(stderr,"** No tsfile on command line!\n") ; exit(1) ;
   }

   if( !out_ps )
     dc = MCW_new_DC( shell , 16 ,
                      DEFAULT_NCOLOVR , INIT_colovr , INIT_labovr ,
                      1.0 , install ) ;

   if( nyar > 0 ) yname = ynar ;

   /*-- 01 Aug 2001: read from stdin instead of a file --*/

   if( use_stdin ){
     char lbuf[2560] , *cpt ;
     int nval ;
     float val[9] ;

     do{                  /* read lines until 1st char is non-blank and non-# */
       cpt = fgets(lbuf,2560,stdin) ;
       if( cpt==NULL ){ fprintf(stderr,"** Can't read from stdin!\n"); exit(1); }
       for( ii=0 ; cpt[ii] != '\0' && !isspace(cpt[ii]) ; ii++ ) ; /* nada */
     } while( cpt[ii] == '\0' || cpt[ii] == '#' ) ;
     nval = sscanf(lbuf,"%f%f%f%f%f%f%f%f%f",
                   val+0,val+1,val+2,val+3,val+4,val+5,val+6,val+7,val+8) ;
     if( nval < 1 ){
       fprintf(stderr,"** Can't read numbers from stdin!\n"); exit(1);
     }

     nx = nval ; ny = 1 ;
     far = (float *) malloc(sizeof(float)*nval) ;
     memcpy(far,val,sizeof(float)*nx) ;
     while(1){  /* read from stdin */
        cpt = fgets(lbuf,2560,stdin) ;
        if( cpt == NULL ) break ;            /* done */
        for( ii=0 ; cpt[ii] != '\0' && !isspace(cpt[ii]) ; ii++ ) ; /* nada */
        if( cpt[ii] == '\0' || cpt[ii] == '#' ) continue ;          /* skip */
        memset(val,0,sizeof(float)*nx) ;
        nval = sscanf(lbuf,"%f%f%f%f%f%f%f%f%f",
                      val+0,val+1,val+2,val+3,val+4,val+5,val+6,val+7,val+8) ;
        if( nval < 1 ) break ;
        far = (float *) realloc( far , sizeof(float)*(ny+1)*nx ) ;
        memcpy(far+ny*nx,val,sizeof(float)*nx) ;
        ny++ ;
     }
     if( ny < 2 ){
       fprintf(stderr,"** Can't read at least 2 lines from stdin\n"); exit(1);
     }
     flim = mri_new_vol_empty( nx,ny,1 , MRI_float ) ;
     mri_fix_data_pointer( far , flim ) ;
     inim = mri_transpose(flim) ; mri_free(flim) ;

   } else {  /*-- old code: read from a file --*/
             /*-- 05 Mar 2003: or more than 1 file --*/

     if( iarg >= argc ){
       fprintf(stderr,"** No input files on command line?!\n"); exit(1);
     }

     if( iarg == argc-1 ){                 /* only 1 input file */
       inim = mri_read_1D( argv[iarg] ) ;
       if( inim == NULL ){
         fprintf(stderr,"** Can't read input file %s\n",argv[iarg]) ; exit(1);
       }
     } else {                              /* multiple inputs [05 Mar 2003] */
       MRI_IMARR *imar ;                   /* read them & glue into 1 image */
       int iarg_first=iarg, nysum=0, ii,jj,nx ;
       float *far,*iar ;

       INIT_IMARR(imar) ;
       for( ; iarg < argc ; iarg++ ){
         inim = mri_read_1D( argv[iarg] ) ;
         if( inim == NULL ){
           fprintf(stderr,"** Can't read input file %s\n",argv[iarg]) ; exit(1);
         }
         if( iarg == iarg_first || inim->nx < nx ) nx = inim->nx ;
         ADDTO_IMARR(imar,inim) ; nysum += inim->ny ;
       }
       flim = mri_new( nx,nysum, MRI_float ); far = MRI_FLOAT_PTR(flim);
       for( nysum=ii=0 ; ii < imar->num ; ii++ ){
         inim = IMARR_SUBIM(imar,ii) ; iar = MRI_FLOAT_PTR(inim) ;
         for( jj=0 ; jj < inim->ny ; jj++,nysum++ ){
           memcpy( far + nx*nysum , iar + jj*inim->nx , sizeof(float)*nx ) ;
         }
       }
       DESTROY_IMARR(imar) ; inim = flim ;
     }

   } /* end of file input */

   flim = inim ;
   far  = MRI_FLOAT_PTR(flim) ;
   nx   = flim->nx ;
   ny   = flim->ny ;

   /* make x axis */

   xar = (float *) malloc( sizeof(float) * nx ) ;
   for( ii=0 ; ii < nx ; ii++ ) xar[ii] = xzero + dx*ii ;

   /* select data to plot */

   nts = ny ;
   yar = (float **) malloc(sizeof(float *)*nts) ;
   for( ii=0 ; ii < ny ; ii++ ) yar[ii] = far + (ii*nx+ignore) ;

   nx = nx - ignore ;  /* cut off the ignored points */

   if( use > 1 && nx > use ) nx = use ;  /* 29 Nov 1999 */

   /* start X11 */

   if( !out_ps ){
     (void) XtAppAddTimeOut( app , 123 , startup_timeout_CB , NULL ) ;
     XtAppMainLoop(app) ;   /* never returns */
   }

   /* 29 Nov 2002: if here, output PostScript to stdout */

   { MEM_plotdata *mp ;
     int ymask = (sep) ? TSP_SEPARATE_YBOX : 0 ;

     mp = plot_ts_mem( nx,xar , nts,ymask,yar ,
                       xlabel , ylabel , title , yname ) ;

     memplot_to_postscript( "-" , mp ) ;
   }

   exit(0) ;
}

/*-----------------------------------------------------------------*/
void killfunc(void * fred){ exit(0) ; }
/*-----------------------------------------------------------------*/

void startup_timeout_CB( XtPointer client_data , XtIntervalId * id )
{
   int ng ;

   /* make graph */

   ng = (sep) ? (-nts) : (nts) ;

   plot_ts_lab( dc->display , nx , xar , ng , yar ,
                xlabel , ylabel , title , yname , killfunc ) ;

   return ;
}
