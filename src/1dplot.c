/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "coxplot.h"
#include "xim.h"

/*-------------------------------------------------------------------*/
/*---- quickie program to look at some graphs - RWCox - Feb 1999 ----*/

#define DEFAULT_NCOLOVR 20

static char *INIT_colovr[DEFAULT_NCOLOVR] = {
   "#ffff00" , "#ffcc00"   , "#ff9900"  , "#ff6900" , "#ff4400" , "#ff0000" ,
   "#0000ff" , "#0044ff"   , "#0069ff"  , "#0099ff" , "#00ccff" , "#00ffff" ,
   "green"   , "limegreen" , "violet"   , "hotpink" ,
   "white"   , "#dddddd"   , "#bbbbbb"  , "black"
} ;

static char *INIT_labovr[DEFAULT_NCOLOVR] = {
   "yellow" , "yell-oran" , "oran-yell" , "orange"   , "oran-red" , "red"   ,
   "dk-blue", "blue"      , "lt-blue1"  , "lt-blue2" , "blue-cyan", "cyan"  ,
   "green"  , "limegreen" , "violet"    , "hotpink"  ,
   "white"  , "gry-dd"    , "gry-bb"    , "black"
} ;

static int nx,nts , sep=1, sepscl=0;
static float **yar , *xar ;
static MCW_DC *dc ;
static char *title = NULL , *wintitle = NULL, *xlabel = NULL , *ylabel = NULL ;

static char *dfile_nar[6] = {
         "Roll [\\degree]" , "Pitch [\\degree]" , "Yaw [\\degree]"    ,
         "\\Delta I-S [mm]" , "\\Delta R-L [mm]" , "\\Delta A-P [mm]"  } ;

static int    nyar = 0 ;
static char  *ynar[128] ;
static char **yname = NULL ;

void startup_timeout_CB( XtPointer client_data , XtIntervalId *id ) ;

#undef  JPEG_MODE
#undef  PNG_MODE
#define JPEG_MODE     3
#define PNG_MODE      4

/*-----------------------------------------------------------------*/
/* Stuff for censor color overlay boxes (cf. 3dDeconvolve.c) */

static int           num_CENSOR = 0 ;
static int_triple   *abc_CENSOR = NULL ;
static float_triple *rgb_CENSOR = NULL ;

static float_triple  rgb_NOW = { 1.0f , 0.9f , 0.4f } ;

static int  num_blocks = 0 ;
static int *block_list = NULL ;

static int       num_censor_array = 0    ;
static float        *censor_array = NULL ;
static float_triple *censor_rgb   = NULL ;
static float_triple  censor_rgbAA = { 1.0f , 0.9f , 0.4f } ;

/*-----------------------------------------------------------------*/
void usage_1dplot(int detail)
{
   printf(
     "Usage: 1dplot [options] tsfile ...\n"
     "Graphs the columns of a *.1D time series file to the X11 screen.\n"
     "\n"
     "-------\n"
     "OPTIONS\n"
     "-------\n"
     " -install   = Install a new X11 colormap.\n"
     " -sep       = Plot each column in a separate sub-graph.\n"
     " -one       = Plot all columns together in one big graph.\n"
     "                [default = -sep]\n"
     " -sepscl    = Plot each column in a separate sub-graph\n"
     "              and allow each sub-graph to have a different\n"
     "              y-scale.  -sepscl is meaningless with -one!\n"
     " -noline    = Don't plot the connecting lines (also implies '-box').\n"
     " -NOLINE    = Same as '-noline', but will not try to plot values outside\n"
     "              the rectangular box that contains the graph axes.\n"
     " -box       = Plot a small 'box' at each data point, in addition\n"
     "              to the lines connecting the points.\n"
     "             * The box size can be set via the environment variable\n"
     "               AFNI_1DPLOT_BOXSIZE; the value is a fraction of the\n"
     "               overall plot size.  The standard box size is 0.006.\n"
     "               Example with a bigger box:\n"
     "                 1dplot -DAFNI_1DPLOT_BOXSIZE=0.01 -box A.1D\n"
     "\n"
     "           ** The '-norm' options below can be useful for\n"
     "               plotting data with different value ranges on\n"
     "               top of each other using '-one':\n"
     " -norm2     = Independently scale each time series plotted to\n"
     "              have L_2 norm = 1 (sum of squares).\n"
     " -normx     = Independently scale each time series plotted to\n"
     "              have max absolute value = 1 (L_infinity norm).\n"
     " -norm1     = Independently scale each time series plotted to\n"
     "              have max sum of absolute values = 1 (L_1 norm).\n"
     "\n"
     " -x  X.1D   = Use for X axis the data in X.1D.\n"
     "              Note that X.1D should have one column\n"
     "              of the same length as the columns in tsfile. \n"
     " N.B.: -x will override -dx and -xzero; -xaxis still has effects\n"
     " -xl10 X.1D = Use log10(X.1D) as the X axis.\n"
     "\n"
     " -dx xx     = Spacing between points on the x-axis is 'xx'\n"
     "                [default = 1] SYNONYMS: '-dt' and '-del'\n"
     " -xzero zz  = Initial x coordinate is 'zz' [default = 0]\n"
     "                SYNONYMS: '-tzero' and '-start'\n"
     " -nopush    = Don't 'push' axes ranges outwards.\n"
     " -ignore nn = Skip first 'nn' rows in the input file\n"
     "                [default = 0]\n"
     " -use mm    = Plot 'mm' points [default = all of them]\n"
     " -xlabel aa = Put string 'aa' below the x-axis\n"
     "                [default = no axis label]\n"
     " -ylabel aa = Put string 'aa' to the left of the y-axis\n"
     "                [default = no axis label]\n"
     " -plabel pp = Put string 'pp' atop the plot.\n"
     "              Some characters, such as '_' have\n"
     "              special formatting effects. You \n"
     "              can escape that with '\'. For example:\n"
     "        echo 2 4.5 -1 | 1dplot -plabel 'test_underscore' -stdin\n"
     "              versus\n"
     "        echo 2 4.5 -1 | 1dplot -plabel 'test\\_underscore' -stdin\n"
     " -title pp = Same as -plabel, but only works with -ps/-png/-jpg options.\n"
     " -wintitle pp = Set string 'pp' as the title of the frame \n"
     "                containing the plot. Default is based on input.\n"
     #if 0
     "             Use -plabel instead for full interoperability.\n"
     "             [In X11 mode, the X11 startup 'consumes' the '-title' ]\n"
     "             [before the program scans the command line for options]\n"
     #endif
     "\n"
     " -stdin     = Don't read from tsfile; instead, read from\n"
     "              stdin and plot it. You cannot combine input\n"
     "              from stdin and tsfile(s).  If you want to do so,\n"
     "              use program 1dcat first.\n"
     "\n"
     " -ps        = Don't draw plot in a window; instead, write it\n"
     "              to stdout in PostScript format.\n"
     "             * If you view the result in 'gv', you should turn\n"
     "               'anti-alias' off, and switch to landscape mode.\n"
     "             * You can use the 'gs' program to convert PostScript\n"
     "               to other formats; for example, a .bmp file:\n"
     "            1dplot -ps ~/data/verbal/cosall.1D | \n"
     "             gs -r100 -sOutputFile=fred.bmp -sDEVICE=bmp256 -q -dBATCH -\n"
     "\n"
     " -jpg fname  } = Render plot to an image and save to a file named\n"
     " -jpeg fname } = 'fname', in JPEG mode or in PNG mode.\n"
     " -png fname  } = The default image width is 1024 pixels; to change\n"
     "                 this value to 2000 pixels (say), do\n"
     "                   setenv AFNI_1DPLOT_IMSIZE 2000\n"
     "                 before running 1dplot.  Widths over 2000 may start\n"
     "                 to look odd, and will run more slowly.\n"
     "               * PNG files will be smaller than JPEG, and are\n"
     "                 compressed without loss.\n"
     "               * PNG output requires that the netpbm program\n"
     "                 pnmtopng be installed somewhere in your PATH.\n"
     "\n"
     " -pngs SIZE fname } = a convenience function equivalent to\n"
     " -jpgs SIZE fname } = setenv AFNI_1DPLOT_IMSIZE SIZE and \n"
     " -jpegs SIZE fname} = -png (or -jpg) fname\n"
     "\n"
     " -ytran 'expr'   = Transform the data along the y-axis by\n"
     "                   applying the expression to each input value.\n"
     "                   For example:\n"
     "                     -ytran 'log10(z)'\n"
     "                   will take log10 of each input time series value\n"
     "                   before plotting it.\n"
     "                 * The expression should have one variable (any letter\n"
     "                   from a-z will do), which stands for the time series\n"
     "                   data to be transformed.\n"
     "                 * An expression such as 'sqrt(x*x+i)' will use 'x'\n"
     "                   for the time series value and use 'i' for the time\n"
     "                   index (starting at 0) -- in this way, you can use\n"
     "                   time-dependent transformations, if needed.\n"
     "                 * This transformation applies to all input time series\n"
     "                   (at present, there is no way to transform different\n"
     "                   time series in distinct ways inside 1dplot).\n"
     "                 * '-ytran' is applied BEFORE the various '-norm' options.\n"
     "\n"
     " -xaxis b:t:n:m  = Set the x-axis to run from value 'b' to\n"
     "                   value 't', with 'n' major divisions and\n"
     "                   'm' minor tic marks per major division.\n"
     "                   For example:\n"
     "                     -xaxis 0:100:5:20\n"
     "                   Setting 'n' to 0 means no tic marks or labels.\n"
     "\n"
     " -yaxis b:t:n:m  = Similar to above, for the y-axis.  These\n"
     "                   options override the normal autoscaling\n"
     "                   of their respective axes.\n"
     "\n"
     " -ynames a b ... = Use the strings 'a', 'b', etc., as\n"
     "                   labels to the right of the graphs,\n"
     "                   corresponding to each input column.\n"
     "                   These strings CANNOT start with the\n"
     "                   '-' character.\n"
     "             N.B.: Each separate string after '-ynames'\n"
     "                   is taken to be a new label, until the\n"
     "                   end of the command line or until some\n"
     "                   string starts with a '-'.  In particular,\n"
     "                   This means you CANNOT do something like\n"
     "                     1dplot -ynames a b c file.1D\n"
     "                   since the input filename 'file.1D' will\n"
     "                   be used as a label string, not a filename.\n"
     "                   Instead, you must put another option between\n"
     "                   the end of the '-ynames' label list, OR you\n"
     "                   can put a single '-' at the end of the label\n"
     "                   list to signal its end:\n"
     "                     1dplot -ynames a b c - file.1D\n"
     "\n"
     " -volreg         = Makes the 'ynames' be the same as the\n"
     "                   6 labels used in plug_volreg for\n"
     "                   Roll, Pitch, Yaw, I-S, R-L, and A-P\n"
     "                   movements, in that order.\n"
     "\n"
     " -thick          = Each time you give this, it makes the line\n"
     "                   thickness used for plotting a little larger.\n"
     "                   [An alternative to using '-DAFNI_1DPLOT_THIK=...']\n"
     " -THICK          = Twice the power of '-thick' at no extra cost!!\n"
     "\n"
     " -Dname=val      = Set environment variable 'name' to 'val'\n"
     "                   for this run of the program only:\n"
     " 1dplot -DAFNI_1DPLOT_THIK=0.01 -DAFNI_1DPLOT_COLOR_01=blue '1D:3 4 5 3 1 0'\n"
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
     "You can alter the thickness of the lines by setting the variable\n"
     "AFNI_1DPLOT_THIK to a value between 0.00 and 0.05 -- the units are\n"
     "fractions of the page size; of course, you can also use the options\n"
     "'-thick' or '-THICK' if you prefer.\n"
     "\n"
     "----------------\n"
     "RENDERING METHOD\n"
     "----------------\n"
     "On 30 Apr 2012, a new method of rendering the 1dplot graph into an X11\n"
     "window was introduced -- this method uses 'anti-aliasing' to produce\n"
     "smoother-looking lines and characters.  If you want the old coarser-looking\n"
     "rendering method, set environment variable AFNI_1DPLOT_RENDEROLD to YES.\n"
     "\n"
     "The program always uses the new rendering method when drawing to a JPEG\n"
     "or PNG file (which is not and never has been just a screen capture).\n"
     "There is no way to disable the new rendering method for image-file saves.\n"
     "\n"
     "------\n"
     "LABELS\n"
     "------\n"
     "Besides normal alphabetic text, the various labels can include some\n"
     "special characters, using TeX-like escapes starting with '\\'.\n"
     "Also, the '^' and '_' characters denote super- and sub-scripts,\n"
     "respectively.  The following command shows many of the escapes:\n"
     " 1deval -num 100 -expr 'J0(t/4)' | 1dplot -stdin -thick \\\n"
     " -xlabel '\\alpha\\beta\\gamma\\delta\\epsilon\\zeta\\eta^{\\oplus\\dagger}\\times c' \\\n"
     " -ylabel 'Bessel Function \\green J_0(t/4)'     \\\n"
     " -plabel '\\Upsilon\\Phi\\Chi\\Psi\\Omega\\red\\leftrightarrow\\blue\\partial^{2}f/\\partial x^2'\n"
     "\n"
     TS_HELP_STRING
   ) ;

   printf("\n"
     "--------------\n"
     "MARKING BLOCKS (e.g., censored time points)\n"
     "--------------\n"
     "The following options let you mark blocks along the x-axis, by drawing\n"
     "colored vertical boxes over the standard white background.\n"
     " * The intended use is to mark blocks of time points that are censored\n"
     "   out of an analysis, which is why the options are the same as those\n"
     "   in 3dDeconvolve -- but you can mark blocks for any reason, of course.\n"
     " * These options don't do anything when the '-x' option is used to\n"
     "   alter the x-axis spacings.\n"
     " * To see what the various color markings look like, try this silly example:\n"
     "\n"
     "   1deval -num 100 -expr 'lran(2)' > zz.1D\n"
     "   1dplot -thick -censor_RGB red    -CENSORTR 3-8   \\\n"
     "                 -censor_RGB green  -CENSORTR 11-16 \\\n"
     "                 -censor_RGB blue   -CENSORTR 22-27 \\\n"
     "                 -censor_RGB yellow -CENSORTR 34-39 \\\n"
     "                 -censor_RGB violet -CENSORTR 45-50 \\\n"
     "                 -censor_RGB pink   -CENSORTR 55-60 \\\n"
     "                 -censor_RGB gray   -CENSORTR 65-70 \\\n"
     "                 -censor_RGB #2cf   -CENSORTR 75-80 \\\n"
     "          -plabel 'red green blue yellow violet pink gray #2cf' zz.1D &\n"
     "\n"
     " -censor_RGB clr   = set the color used for the marking to 'clr', which\n"
     "                     can be one of the strings below:\n"
     "                       red green blue yellow violet pink gray (OR grey)\n"
     "                   * OR 'clr' can be in the form '#xyz' or '#xxyyzz', where\n"
     "                     'x', 'y', and 'z' are hexadecimal digits -- for example,\n"
     "                     '#2cf' is sort of a cyan color.\n"
     "                   * OR 'clr' can be in the form 'rgbi:rf/gf/bf' where\n"
     "                     each color intensity (rf, gf, bf) is a number between\n"
     "                     0.0 and 1.0 -- e.g., white is 'rgbi:1.0/1.0/1.0'.\n"
     "                     Since the background is white, dark colors don't look\n"
     "                     good here, and will obscure the graphs; for example,\n"
     "                     pink is defined here as 'rgbi:1.0/0.5/0.5'.\n"
     "                   * The default color is (a rather pale) yellow.\n"
     "                   * You can use '-censor_RGB' more than once.  The color\n"
     "                     most recently specified previous on the command line\n"
     "                     is what will be used with the '-censor' and '-CENSORTR'\n"
     "                     options.  This allows you to mark different blocks\n"
     "                     with different colors (e.g., if they were censored\n"
     "                     for different reasons).\n"
     "                   * The feature of allowing multiple '-censor_RGB' options\n"
     "                     means that you must put this option BEFORE the\n"
     "                     relevant '-censor' and/or '-CENSORTR' options.\n"
     "                     Otherwise, you'll get the default yellow color!\n"
     "\n"
     " -censor cname     = cname is the filename of censor .1D time series   \n"
     "                   * This is a file of 1s and 0s, indicating which     \n"
     "                     time points are to be un-marked (1) and which are \n"
     "                     to be marked (0).                                 \n"
     "                   * The option below may be simpler to use!           \n"
     "\n"
     " -CENSORTR clist   = clist is a list of strings that specify time indexes\n"
     "                     to be marked in the graph(s).  Each string is of  \n"
     "                     one of the following forms:                       \n"
     "                           37 => mark global time index #37            \n"
     "                         2:37 => mark time index #37 in run #2         \n"
     "                       37..47 => mark global time indexes #37-47       \n"
     "                       37-47  => same as above                         \n"
     "                     *:0-2    => mark time indexes #0-2 in all runs    \n"
     "                     2:37..47 => mark time indexes #37-47 in run #2    \n"
     "                   * Time indexes within each run start at 0.          \n"
     "                   * Run indexes start at 1 (just be to confusing).    \n"
     "                   * Multiple -CENSORTR options may be used, or        \n"
     "                     multiple -CENSORTR strings can be given at        \n"
     "                     once, separated by spaces or commas.              \n"
     "                   * Each argument on the command line after           \n"
     "                     '-CENSORTR' is treated as a censoring string,     \n"
     "                     until an argument starts with a '-' or an         \n"
     "                     alphabetic character.  This means that if you     \n"
     "                     want to plot a file named '9zork.1D', you may     \n"
     "                     have to do something like                         \n"
     "                       1dplot -CENSORTR 3-7 18-22 - 9zork.1D           \n"
     "                     The stand-alone '-' will stop the processing      \n"
     "                     of censor strings; otherwise, the '9zork.1D'      \n"
     "                     string, since it doesn't start with a letter,     \n"
     "                     would be treated as a censoring string, which     \n"
     "                     you would not like.                               \n"
     "                   * N.B.: 2:37,47 means index #37 in run #2 and       \n"
     "                     global time index 47; it does NOT mean            \n"
     "                     index #37 in run #2 AND index #47 in run #2.      \n"
     "\n"
     " -concat rname      = rname is the filename for list of concatenated runs\n"
     "                      * 'rname' can be in the format                   \n"
     "                          '1D: 0 100 200 300'                          \n"
     "                        which indicates 4 runs, the first of which     \n"
     "                        starts at time index=0, second at index=100,   \n"
     "                        and so on.                                     \n"
     "                      * The ONLY function of '-concat' is for use with \n"
     "                        '-CENSORTR', to be compatible with 3dDeconvolve\n"
     "                          [e.g., for plotting motion parameters from]\n"
     "                          [3dvolreg -1Dfile, where you've cat-enated]\n"
     "                          [the 1D files from separate runs into one ]\n"
     "                          [long file for plotting with this program.]\n"
     "\n"
   ) ;

   PRINT_COMPILE_DATE ;
   return;
}

int main( int argc , char *argv[] )
{
   int iarg , ii , ny , ignore=0 , use=0 , install=0 ;
   float dx=1.0 , xzero=0.0 ;
   char *cpt , *xfile=NULL;   int xl10=0 ;
   MRI_IMAGE *inim , *flim ;
   float *far ;
   XtAppContext app ;
   Widget shell=(Widget)NULL ;
   int use_stdin=0 ; /* 01 Aug 2001 */
   int out_ps   =0 ; /* 29 Nov 2002 */
   int nopush   =0 ;
   int nnax=0,mmax=0 , nnay=0,mmay=0 ;
   float xbot,xtop   , ybot,ytop ; float thik=0.0f ;
   int skip_x11=0 , imsave=0 ; char *imfile=NULL ;
   int do_norm=0 ;   /* 26 Mar 2008 */
   char *ytran=NULL; /* 16 Jun 2009 */
   char autotitle[512]={""}; /* 23 March 2009 */
   float tsbox=0.0f , boxsiz ; int noline=0 ;

   mainENTRY("1dplot main"); machdep();
   PRINT_VERSION("1dplot"); AUTHOR("RWC et al.");

   boxsiz = AFNI_numenv("AFNI_1DPLOT_BOXSIZE") ;
        if( boxsiz <= 0.0f   ) boxsiz = 0.006f ;
   else if( boxsiz <  0.001f ) boxsiz = 0.001f ;
   else if( boxsiz >  0.020f ) boxsiz = 0.020f ;

   /* 29 Nov 2002: scan for things that make us skip X11 */

   for( ii=1 ; ii < argc ; ii++ ){
     if( strcasecmp(argv[ii],"-ps")   == 0 ){ skip_x11 = 1; break; }
     if( strcasecmp(argv[ii],"-jpg")  == 0 ){ skip_x11 = 1; break; }
     if( strcasecmp(argv[ii],"-jpgs") == 0 ){ skip_x11 = 1; break; }
     if( strcasecmp(argv[ii],"-jpeg") == 0 ){ skip_x11 = 1; break; }
     if( strcasecmp(argv[ii],"-jpegs")== 0 ){ skip_x11 = 1; break; }
     if( strcasecmp(argv[ii],"-png")  == 0 ){ skip_x11 = 1; break; }
     if( strcasecmp(argv[ii],"-pngs") == 0 ){ skip_x11 = 1; break; }
     if( strcasecmp(argv[ii],"-help") == 0 ){ skip_x11 = 1; break; }
   }
   if( argc == 1 ) skip_x11 = 1 ;  /* this is because Ziad is trouble */

   if( !skip_x11 ){
     for( ii=1 ; ii < argc ; ii++ ){
       if( strcmp(argv[ii],"-title") == 0 ){
#if 0
         WARNING_message("-title used with X11 plotting: use -plabel instead!") ;
#endif
         title = argv[ii+1] ; break ;
       }
     }
   }

   /* open X11 */

   if( !skip_x11 ){
     shell = XtVaAppInitialize(
                &app , "AFNI" , NULL , 0 , &argc , argv , NULL , NULL ) ;
     if( shell == NULL ) ERROR_exit("Cannot initialize X11!") ;
   }

   cpt = my_getenv("TMPDIR") ;  /* just for fun */

   /*-- scan arguments that X11 didn't eat --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){
      /*-- help? --*/

      if(strcmp(argv[iarg],"-help") == 0 ||
         strcmp(argv[iarg],"-h") == 0) {
         usage_1dplot(strlen(argv[iarg])>3?2:1);
         exit(0) ;
      }

#if 0
     if( strcmp(argv[iarg],"-vbox") == 0 ){   /* HIDDEN: just for testing */
       float xb1,xb2 ;
       xb1 = (float)strtod(argv[++iarg],NULL) ;
       xb2 = (float)strtod(argv[++iarg],NULL) ;
       plot_ts_add_vbox( -1 , xb1,xb2 , 1.0f,1.0f,0.0f ) ;
       iarg++ ; continue ;
     }
#endif

     if( strcmp(argv[iarg],"-") == 0 ){  /* 23 Aug 2006: null option */
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-thi",4) == 0 ){  /* 15 Apr 2009: thickness */
       thik += 0.004f ; if( argv[iarg][1] == 'T' ) thik += 0.004f ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-norm2") == 0 ){  /* 26 Mar 2008 */
       do_norm = 2 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-norm1") == 0 ){
       do_norm = 1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-normx") == 0 ){
       do_norm = 666 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-x") == 0 ){   /* ZSS: April 2007 */
       if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
       xfile = argv[++iarg]; xl10 = 0 ;
       iarg++; continue;
     }
     if( strcasecmp(argv[iarg],"-xl10") == 0 ){
       if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
       xfile = argv[++iarg]; xl10 = 1 ;
       iarg++; continue;
     }

     if( strcmp(argv[iarg],"-xaxis") == 0 ){   /* 22 Jul 2003 */
       if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
       sscanf(argv[++iarg],"%f:%f:%d:%d",&xbot,&xtop,&nnax,&mmax) ;
       if( xbot >= xtop || nnax < 0 || mmax < 1 )
         ERROR_exit("String after -xaxis is illegal!\n") ;

       plot_ts_xfix( nnax,mmax , xbot,xtop ) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-yaxis") == 0 ){   /* 22 Jul 2003 */
       if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
       sscanf(argv[++iarg],"%f:%f:%d:%d",&ybot,&ytop,&nnay,&mmay) ;
       if( ybot >= ytop || nnay < 0 || mmay < 1 )
         ERROR_exit("String after -yaxis is illegal!\n") ;

       plot_ts_yfix( nnay,mmay , ybot,ytop ) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-ytran") == 0 ){   /* 16 Jun 2009 */
       ytran = strdup(argv[++iarg]) ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-nopush") == 0 ){  /* 12 Mar 2003 */
       plot_ts_xypush( 0 , 0 ) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-ps") == 0 ){
        out_ps = 1 ; imsave = 0 ;
        iarg++ ; continue ;
     }

     /*-- image file output --*/

     if( strcasecmp(argv[iarg],"-jpeg") == 0 || strcasecmp(argv[iarg],"-jpg") == 0 ){
        out_ps = 0 ; imsave = JPEG_MODE ;
        iarg++ ; if( iarg >= argc ) ERROR_exit("need argument after '%s'",argv[iarg-1]) ;
        imfile = (char *)malloc(strlen(argv[iarg])+8) ; strcpy(imfile,argv[iarg]) ;
        if( !STRING_HAS_SUFFIX(imfile,".jpg") && !STRING_HAS_SUFFIX(imfile,".JPG") )
          strcat(imfile,".jpg") ;
        iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-jpegs") == 0 ||
         strcasecmp(argv[iarg],"-jpgs" ) == 0   ){
        int isize; static char sss[256]={""} ;
        out_ps = 0 ; imsave = JPEG_MODE ;
        iarg++ ;
        if( iarg+1 >= argc )
          ERROR_exit("need 2 arguments after '%s'",argv[iarg-1]) ;
        isize = (int) strtod(argv[iarg], NULL);
        if (isize < 100 || isize > 9999) {
          ERROR_exit("SIZE value of %d is rather fishy. \n"
                     "Allowed range is between 100 and 9999", isize);
        }
        sprintf(sss,"AFNI_1DPLOT_IMSIZE=%d", isize);
        putenv(sss) ;
        iarg++ ;
        imfile = (char *)malloc(strlen(argv[iarg])+8) ;
        strcpy(imfile,argv[iarg]) ;
        if( !STRING_HAS_SUFFIX(imfile,".jpg") &&
            !STRING_HAS_SUFFIX(imfile,".JPG") )
          strcat(imfile,".jpg") ;
        iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-png") == 0 ){
        out_ps = 0 ; imsave = PNG_MODE ;
        iarg++ ; if( iarg >= argc ) ERROR_exit("need argument after '%s'",argv[iarg-1]) ;
        imfile = (char *)malloc(strlen(argv[iarg])+8) ; strcpy(imfile,argv[iarg]) ;
        if( !STRING_HAS_SUFFIX(imfile,".png") && !STRING_HAS_SUFFIX(imfile,".PNG") )
          strcat(imfile,".png") ;
        iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-pngs") == 0 ){
        int isize; static char sss[256]={""} ;
        out_ps = 0 ; imsave = PNG_MODE ;
        iarg++ ;
        if( iarg+1 >= argc )
          ERROR_exit("need 2 arguments after '%s'",argv[iarg-1]) ;
        isize = (int) strtod(argv[iarg], NULL);
        if (isize < 100 || isize > 9999) {
          ERROR_exit("SIZE value of %d is rather fishy. \n"
                     "Allowed range is between 100 and 9999", isize);
        }
        sprintf(sss,"AFNI_1DPLOT_IMSIZE=%d", isize);
        putenv(sss) ;
        iarg++ ;
        imfile = (char *)malloc(strlen(argv[iarg])+8) ;
        strcpy(imfile,argv[iarg]) ;
        if( !STRING_HAS_SUFFIX(imfile,".png") && !STRING_HAS_SUFFIX(imfile,".PNG") )
          strcat(imfile,".png") ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-install") == 0 ){
       install++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-stdin") == 0 ){  /* 01 Aug 2001 */
       use_stdin++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-ynames") == 0 ){
        if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
        iarg++ ;
        while( iarg < argc && argv[iarg][0] != '-' ){
           ynar[nyar++] = argv[iarg++] ;
        }
        /* 23 Aug 2006: skip next arg if it is "-" */
        if( iarg < argc && strcmp(argv[iarg],"-") == 0 ) iarg++ ;
        continue ;
     }

     if( strcmp(argv[iarg],"-volreg") == 0 ){
        int ii ;
        for( ii=0 ; ii < 6 ; ii++ ) ynar[nyar++] = dfile_nar[ii] ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-plabel") == 0 ){
        if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
        title = argv[++iarg] ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-wintitle") == 0 ){
        if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
        wintitle = argv[++iarg] ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-title") == 0 ){ /* normally eaten by XtVaAppInitialize */
#if 0
        WARNING_message(                     /* unless  using -ps! So keep it here, */
         "Consider using -plabel; -title "   /* it don't hurt. */
         "only works with the -ps / -jpg / -png options"  );
#endif
        if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
        title = argv[++iarg] ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-xlabel") == 0 ){
        if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
        xlabel = argv[++iarg] ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-ylabel") == 0 ){
        if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
        ylabel = argv[++iarg] ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-ignore") == 0 ){
        if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
        ignore = strtod( argv[++iarg] , NULL ) ;
        if( ignore < 0 ) ERROR_exit("Illegal -ignore value!\n") ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-use") == 0 || strcmp(argv[iarg],"-num") == 0 ){
        if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
        use = strtod( argv[++iarg] , NULL ) ;
        if( use < 2 ) ERROR_exit("Illegal -use value!\n") ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-dx" ) == 0 ||
         strcmp(argv[iarg],"-del") == 0 ||
         strcmp(argv[iarg],"-dt" ) == 0   ){

        if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
        dx = strtod( argv[++iarg] , NULL ) ;
        if( dx <= 0.0 ) ERROR_exit("Illegal -dx value!\n");
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-xzero") == 0 || strcmp(argv[iarg],"-start") == 0 ||
         strcmp(argv[iarg],"-tzero") == 0   ){

        if( iarg == argc-1 ) ERROR_exit("need argument after option %s",argv[iarg]) ;
        xzero = strtod( argv[++iarg] , NULL ) ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-sep") == 0 ){
        sep = 1 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-sepsc" ,6) == 0 ||
         strncmp(argv[iarg],"-sep_sc",7) == 0   ){
        sepscl = 1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-one") == 0 ){
        sep = 0 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-boxes",4) == 0 ){
       tsbox = boxsiz ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-noline",4) == 0 ){
       noline = 1 ; tsbox = boxsiz ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-NOLINE",4) == 0 ){
       noline = 2 ; tsbox = boxsiz ; iarg++ ; continue ;
     }

#if 0
     if( strncmp(argv[iarg],"-D",2) == 0 && strchr(argv[iarg],'=') != NULL ){
       (void) AFNI_setenv( argv[iarg]+2 ) ;
       iarg++ ; continue ;
     }
#endif

     /*--- censoring/marking stuff -- for Colm -- 24 Apr 2012 ---*/

     if( strcasecmp(argv[iarg],"-censor_RGB"  ) == 0 ||
         strcasecmp(argv[iarg],"-censor_RBG"  ) == 0 ||
         strcasecmp(argv[iarg],"-censor_GBR"  ) == 0 ||
         strcasecmp(argv[iarg],"-censor_GRB"  ) == 0 ||
         strcasecmp(argv[iarg],"-censor_BRG"  ) == 0 ||
         strcasecmp(argv[iarg],"-censor_BGR"  ) == 0 ||
         strcasecmp(argv[iarg],"-censor_clr"  ) == 0 ||
         strcasecmp(argv[iarg],"-censor_color") == 0   ){
       float rf,gf,bf ; char *eee ;
       if( ++iarg >= argc ) ERROR_exit("need argument after %s",argv[iarg-1]) ;
       rf = gf = bf = -1.0f ; eee = argv[iarg] ;
       (void)sscanf( eee , "rgbi:%f/%f/%f" , &rf,&gf,&bf ) ;
       if( rf >= 0.0f && rf <= 1.0f &&
           gf >= 0.0f && gf <= 1.0f &&
           bf >= 0.0f && bf <= 1.0f   ){
         /* OK -- do nothing more here */
       } else if( strcasecmp(eee,"green") == 0 ){
         rf = 0.5f; gf = 1.0f; bf = 0.5f;
       } else if( strcasecmp(eee,"red") == 0 ){
         rf = 1.0f; gf = 0.3f; bf = 0.3f;
       } else if( strcasecmp(eee,"gray") == 0 || strcasecmp(eee,"grey") == 0 ){
         rf = gf = bf = 0.7654321f ;
       } else if( strcasecmp(eee,"blue") == 0 ){
         rf = 0.55f; gf = 0.55f; bf = 1.0f;
       } else if( strcasecmp(eee,"purple") == 0 || strcasecmp(eee,"violet") == 0 ){
         rf = 1.0f; gf = 0.5f; bf = 1.0f;
       } else if( strcasecmp(eee,"gold") == 0 || strcasecmp(eee,"yellow") == 0 ){
         rf = 1.0f; gf = 0.9f; bf = 0.4f;
       } else if( strcasecmp(eee,"pink") == 0 ){
         rf = 1.0f; gf = 0.5f; bf = 0.5f;
       } else if( *eee == '#' && *(eee+1) != '\0' ){
         int le=strlen(eee+1) , val , bas , rr,gg,bb ;
         val = (int)strtol( eee+1 , NULL , 16 ) ;       /* hexadecimal */
         bas = (le <= 3) ? 16 : 256 ;
         bb  = val % bas ; val = val / bas ; bf  = bb / ((float)bas) ;
         gg  = val % bas ; val = val / bas ; gf  = gg / ((float)bas) ;
         rr  = val % bas ;                   rf  = rr / ((float)bas) ;
       } else {
         WARNING_message("%s is not a recognizable color -- choosing a random color :-)",eee ) ;
         rf = 0.4f * ( 1.0f + (float)drand48() ) ;
         gf = 0.4f * ( 1.0f + (float)drand48() ) ;
         bf = 0.4f * ( 1.0f + (float)drand48() ) ;
       }
       rgb_NOW.a = rf ; rgb_NOW.b = gf ; rgb_NOW.c = bf ;
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-CENSOR",7)   == 0 ||
         strncmp(argv[iarg],"-censorTR",9) == 0   ){

       NI_str_array *nsar ;
       char *src=malloc(1), *cpt, *dpt ;
       int ns, r,a,b ; int_triple rab ;

       if( iarg == argc-1 ) ERROR_exit("need an argument after option %s",argv[iarg]) ;

       *src = '\0' ;   /* cat all following options until starts with '-' */
       for( iarg++ ;
            iarg < argc && argv[iarg][0] != '-'
                        && !isalpha(argv[iarg][0])
                        && strstr(argv[iarg],".1D") == NULL ;
            iarg++ ){
         ns = strlen(argv[iarg]) ; if( ns == 0 ) continue ;
         src = realloc(src,strlen(src)+ns+2) ;
         strcat(src," ") ; strcat(src,argv[iarg]) ;
       }
       if( *src == '\0' ){
         WARNING_message("Bad argument after -CENSORTR") ; continue ;
       }
       nsar = NI_decode_string_list( src , "," ) ; /* break into substrings */
       for( ns=0 ; ns < nsar->num ; ns++ ){ /* loop over substrings */
         cpt = nsar->str[ns] ; dpt = strchr(cpt,':') ; r = 0 ;
         if( *cpt == '\0' ) continue ;   /* skip an empty string */
         if( dpt != NULL ){              /* found 'run:' */
           if( *cpt == '*' ){ /* wildcard = all runs */
             r = -666 ;
           } else {
             r = (int)strtol(cpt,NULL,10) ;
             if( r <= 0 ){  /* skip out */
               WARNING_message("-CENSORTR %s -- run index '%d' is bad! [iarg=%d]",
                             nsar->str[ns],r,iarg);
               continue ;
             }
           }
           cpt = dpt+1 ;  /* skip to character after ':' */
           if( *cpt == '\0' ){  /* skip out */
             WARNING_message("-CENSORTR %s -- no data after run index! [iarg=%d]",
                           nsar->str[ns],iarg);
             continue ;
           }
         }
         a = (int)strtol(cpt,&dpt,10) ;    /* get first index number */
         if( a < 0 ){  /* skip out */
           WARNING_message("-CENSORTR %s -- time index '%d' is bad! [iarg=%d]",
                         nsar->str[ns],a,iarg);
           continue ;
         }
         if( *dpt == '\0' ){  /* no second number */
           b = a ;
         } else {             /* get second number */
           for( dpt++ ; *dpt != '\0' && !isdigit(*dpt) ; dpt++ ) ; /*nada*/
           b = (int)strtol(dpt,NULL,10) ;
           if( b < a || b < 0 ){  /* skip out */
             WARNING_message("-CENSORTR %s -- time indexes '%d' to '%d' is bad! [iarg=%d]",
                           nsar->str[ns],a,b,iarg);
             continue ;
           }
         }
         rgb_CENSOR = (float_triple *)realloc( rgb_CENSOR ,
                                               sizeof(float_triple)*(num_CENSOR+1) );
         rgb_CENSOR[num_CENSOR] = rgb_NOW ;
         abc_CENSOR = (int_triple *)  realloc( abc_CENSOR ,
                                               sizeof(int_triple)*(num_CENSOR+1) );
         rab.i = r; rab.j = a; rab.k = b; abc_CENSOR[num_CENSOR++] = rab ;
       } /* end of loop over -CENSORTR strings */
       NI_delete_str_array(nsar) ; free(src) ;
       continue ;  /* next option */
     }

     /*-----   -concat filename   -----*/

     if( strcmp(argv[iarg],"-concat") == 0 ){
       MRI_IMAGE *cim ; float *car ; int qq ;
       if( ++iarg >= argc ) ERROR_exit("need argument after option %s",argv[iarg-1]) ;
       cim = mri_read_1D(argv[iarg]) ;
       if( cim == NULL ) ERROR_exit("can't read -concat file '%s'",argv[iarg]) ;
       car = MRI_FLOAT_PTR(cim) ;
       num_blocks = cim->nx ;
       block_list = (int *)malloc(sizeof(int)*num_blocks) ;
       for( qq=0 ; qq < num_blocks ; qq++ )
         block_list[qq] = (int)(car[qq]+0.5f) ;
       mri_free(cim) ; iarg++; continue;
     }

      /*-----   -censor filename   -----*/

     if( strcmp(argv[iarg], "-censor") == 0 ){
       MRI_IMAGE *cim ;
       if( ++iarg >= argc ) ERROR_exit("need argument after option %s",argv[iarg-1]) ;
       cim = mri_read_1D(argv[iarg]) ;
       if( cim == NULL ) ERROR_exit("can't read -censor file '%s'",argv[iarg]) ;
       censor_array = MRI_FLOAT_PTR(cim) ;
       num_censor_array = cim->nx ;
       censor_rgbAA = rgb_NOW ;
       iarg++; continue;
     }

     /*--- symplectically stoopid user ---*/

     ERROR_message("Unknown option: %s\n",argv[iarg]) ;
     suggest_best_prog_option(argv[0], argv[iarg]);
     exit(1);
   }

   if( argc < 2 ){ usage_1dplot(0); exit(0) ; }

   if( thik > 0.0f ) plot_ts_setTHIK(thik) ;

   if(sepscl && sep == 0) {
      WARNING_message("Cannot use -sepscl with -one!") ; sepscl=0 ;
   }
   if( iarg >= argc && !use_stdin )
      ERROR_exit("No time series file on command line!\n") ;

   /*-- setup color info --*/

   if( !skip_x11 ){
     dc = MCW_new_DC( shell , 16 ,
                      DEFAULT_NCOLOVR , INIT_colovr , INIT_labovr ,
                      1.0 , install ) ;

     if( !AFNI_yesenv("AFNI_1DPLOT_RENDEROLD") ){  /* 30 Apr 2012 */
       memplot_to_X11_set_DC(dc) ;
       X11_SET_NEW_PLOT ;                           /* cf. xim.h */
       thik += 0.0015f ;
       plot_ts_setthik(0.0015f) ;
     }
   }

   if( nyar > 0 ) yname = ynar ;

   /*-- 01 Aug 2001: read from stdin instead of a file --*/

#define NLBUF 131072
#define NVMAX 10000
   if( use_stdin ){
     char *lbuf , *cpt , *dpt ;
     int   nval ;
     float *val , fff ;

     if (!wintitle) wintitle = "stdin";   /* ZSS Oct 7 09 */

     lbuf = (char * )malloc(sizeof(char )*NLBUF) ;
     val  = (float *)malloc(sizeof(float)*NVMAX) ;

     /** 13 May 2005: modified to read up to NVMAX numbers from stdin,
                      rather than the fixed size array of length 9 of old **/

     do{               /* read lines until 1st char is non-blank and non-# */
       cpt = afni_fgets(lbuf,NLBUF,stdin) ;
       if( cpt==NULL ) ERROR_exit("Can't read from stdin!\n");
       for( ii=0 ; cpt[ii] != '\0' && isspace(cpt[ii]) ; ii++ ) ; /* nada */
     } while( cpt[ii] == '\0' || cpt[ii] == '#' ) ;

     nval = 0 ; cpt = lbuf ;   /* read numbers from lbuf into val */
     while(1){
       fff = strtod(cpt,&dpt) ; if( dpt  == cpt   ) break ;
       val[nval++] = fff ;      if( nval == NVMAX ) break ;
       cpt = dpt; if( *cpt == ','  ) cpt++; if( *cpt == '\0' ) break;
     }
     if( nval < 1 )
       ERROR_exit("Can't read numbers from stdin!\n"
                  "  First line: '%-.30s'\n"       , lbuf) ;

     nx = nval ; ny = 1 ;
     far = (float *) malloc(sizeof(float)*nx) ;
     memcpy(far,val,sizeof(float)*nx) ;
     while(1){  /* read from stdin */
        cpt = afni_fgets(lbuf,NLBUF,stdin) ;
        if( cpt == NULL ) break ;            /* done */
        for( ii=0 ; cpt[ii] != '\0' && isspace(cpt[ii]) ; ii++ ) ; /* nada */
        if( cpt[ii] == '\0' || cpt[ii] == '#' ) continue ;         /* skip */
        memset(val,0,sizeof(float)*nx) ;

        nval = 0 ; cpt = lbuf ;   /* read numbers from lbuf into val */
        while(1){
          fff = strtod(cpt,&dpt) ; if( dpt  == cpt ) break ;
          val[nval++] = fff ;      if( nval == nx  ) break ;
          cpt = dpt; if( *cpt == ','  ) cpt++; if( *cpt == '\0' ) break;
        }
        far = (float *) realloc( far , sizeof(float)*(ny+1)*nx ) ;
        memcpy(far+ny*nx,val,sizeof(float)*nx) ; ny++ ;
     }
     if( ny < 2 && nx < 2 )
       ERROR_exit("Can't read at least 2 lines from stdin\n");

     flim = mri_new_vol_empty( nx,ny,1 , MRI_float ) ;
     mri_fix_data_pointer( far , flim ) ;
     if( ny > 1 ){      /* more than one row ==> transpose (the usual case) */
       inim = mri_transpose(flim) ; mri_free(flim) ;
     } else {           /* only 1 row ==> am OK this way [13 May 2005] */
       inim = flim ;
     }
     free((void *)val); free((void *)lbuf);

   } else {  /*-- old code: read from a file --*/
             /*-- 05 Mar 2003: or more than 1 file --*/

     if( iarg >= argc )
       ERROR_exit("No input files on command line?!\n");  /* bad user?! */


     if( iarg == argc-1 ){                 /* only 1 input file */

       if (!wintitle) wintitle = argv[iarg];   /* ZSS Oct 7 09 */

       inim = mri_read_1D( argv[iarg] ) ;
       if( inim == NULL )
         ERROR_exit("Can't read input file '%s'\n",argv[iarg]) ;

     } else {                              /* multiple inputs [05 Mar 2003] */
       MRI_IMARR *imar ;                   /* read them & glue into 1 image */
       int iarg_first=iarg, nysum=0, ii,jj,nx=1 ;
       int constant = 1;                   /* are nx values constant        */
       float *far,*iar ;

       if (!wintitle) {
         snprintf(autotitle,64*sizeof(char),"%s ...", argv[iarg] );
         wintitle = autotitle;
       }

       INIT_IMARR(imar) ;
       for( ; iarg < argc ; iarg++ ){
         inim = mri_read_1D( argv[iarg] ) ;
         if( inim == NULL )
           ERROR_exit("Can't read input file '%s'\n",argv[iarg]) ;

           if( inim->nx == 1 && inim->ny > 1 ){
             flim = mri_transpose(inim); mri_free(inim); inim = flim;
           }

         /* compute nx as the smallest inim->nx, and note consistency */
         if( iarg == iarg_first || inim->nx < nx ) nx = inim->nx ;
         if( iarg > iarg_first && inim->nx != nx ) constant = 0;

         ADDTO_IMARR(imar,inim) ; nysum += inim->ny ;
       }

       /* if nx varied across images, warn the user  24 May 2011 [rickr] */
       if( ! constant )
          WARNING_message("plot lengths vary, truncating to %d values", nx);

       flim = mri_new( nx,nysum, MRI_float ); far = MRI_FLOAT_PTR(flim);
       for( nysum=ii=0 ; ii < imar->num ; ii++ ){
         inim = IMARR_SUBIM(imar,ii) ; iar = MRI_FLOAT_PTR(inim) ;
         for( jj=0 ; jj < inim->ny ; jj++,nysum++ ){
           /* copy only nx floats, not inim->nx    24 May 2011 [rickr] */
           memcpy( far + nx*nysum , iar + jj*inim->nx , sizeof(float)*nx ) ;
         }
       }
       DESTROY_IMARR(imar) ; inim = flim ;
     }

     if( inim->nx == 1 && inim->ny > 1 ){  /* 13 May 2005 */
       flim = mri_transpose(inim); mri_free(inim); inim = flim;
     }

   } /* end of file input */

   flim = inim ;
   far  = MRI_FLOAT_PTR(flim) ;
   nx   = flim->nx ;
   ny   = flim->ny ;

   if( nx < 2 )
     ERROR_exit("1dplot can't plot curves only 1 point long!\n") ;

   /*--- select data to plot ---*/

   nts = ny ;
   yar = (float **) malloc(sizeof(float *)*nts) ;
   for( ii=0 ; ii < ny ; ii++ ) yar[ii] = far + (ii*nx+ignore) ;

   nx = nx - ignore ;  /* cut off the ignored points */

   if( use > 1 && nx > use ) nx = use ;  /* 29 Nov 1999 */

   /*--- 16 Jun 2009: -ytran transformation? ---*/

   if( ytran != NULL && *ytran != '\0' ){
     int cc ;
     for( ii=0 ; ii < ny ; ii++ ){
       cc = PARSER_1dtran( ytran , nx , yar[ii] ) ;
       if( cc <= 0 ) ERROR_exit("Can't evaluate -ytran expression '%s'",ytran) ;
     }
   }

   /*--- 26 Mar 2008: normalize time series? ---*/

   switch( do_norm ){
     case 2:
      for( ii=0 ; ii < ny ; ii++ ) THD_normalize(nx,yar[ii]) ;
     break ;

     case 1:
      for( ii=0 ; ii < ny ; ii++ ) THD_normL1(nx,yar[ii]) ;
     break ;

     case 666:
      for( ii=0 ; ii < ny ; ii++ ) THD_normmax(nx,yar[ii]) ;
     break ;
   }

   /*--- make x axis ---*/

   if( !xfile ){  /* bog standard uniformly spaced x-axis */

      xar = (float *) malloc( sizeof(float) * nx ) ;
      for( ii=0 ; ii < nx ; ii++ ) xar[ii] = xzero + dx*ii ;

      /** 24 Apr 2012: add vbox stuff for censoring **/

      if( censor_array != NULL || num_CENSOR > 0 ){
        int ic,it ; float_triple clr ;
        if( num_blocks == 0 ){
          num_blocks = 1; block_list = (int *)malloc(sizeof(int)); block_list[0] = 0;
        }
        if( censor_array == NULL ){
          censor_array = (float *)malloc(sizeof(float)*nx) ;
          for( ii=0 ; ii < nx ; ii++ ) censor_array[ii] = 1.0f ;
        } else if( num_censor_array < nx ){
          WARNING_message("-censor array is too short ==> extending it from %d to %d",
                          num_censor_array , nx ) ;
          censor_array = (float *)realloc((void *)censor_array,sizeof(float)*nx) ;
          for( ii=num_censor_array ; ii < nx ; ii++ ) censor_array[ii] = 1.0f ;
          censor_rgbAA = rgb_NOW ;
        }
        censor_rgb = (float_triple *)malloc(sizeof(float_triple)*nx) ;
        for( ii=0 ; ii < nx ; ii++ ) censor_rgb[ii] = censor_rgbAA ;
        if( abc_CENSOR != NULL ){
          int rr , aa,bb , bbot,btop , nblk=num_blocks ;
          for( ic=0 ; ic < num_CENSOR ; ic++ ){  /* loop over CENSOR commands */
            clr = rgb_CENSOR[ic] ;
            rr = abc_CENSOR[ic].i ;
            aa = abc_CENSOR[ic].j ; if( aa < 0  ) continue ;  /* shouldn't happen */
            bb = abc_CENSOR[ic].k ; if( bb < aa ) continue ;  /* shouldn't happen */
            if( rr == -666 ){  /* run = wildcard ==> expand to nblk new triples */
              int_triple rab ;
              abc_CENSOR = (int_triple *)realloc( abc_CENSOR ,
                                                  sizeof(int_triple)*(num_CENSOR+nblk) );
              for( rr=1 ; rr <= nblk ; rr++ ){
                rab.i = rr; rab.j = aa; rab.k = bb; abc_CENSOR[num_CENSOR++] = rab;
              }
              continue ;  /* skip to next one */
            }
            if( rr > 0 ){       /* convert local indexes to global */
              if( rr > nblk ){  /* stupid user */
                WARNING_message("-CENSORTR %d:%d-%d has run index out of range 1..%d",
                                rr,aa,bb , nblk ) ;
                aa = -66666666 ;
              } else {
                bbot = block_list[rr-1] ;        /* start index of block #rr */
                btop = (rr < nblk) ? block_list[rr]-1 : nx-1 ; /* last index */
                if( aa+bbot > btop ){  /* WTF? */
                  WARNING_message(
                   "-CENSORTR %d:%d-%d has start index past end of run (%d) - IGNORING",
                   rr,aa,bb,btop-bbot ) ; aa = -66666666 ;
                } else if( bb+bbot > btop ){  /* oopsie */
                  WARNING_message(
                   "-CENSORTR %d:%d-%d has stop index past end of run (%d) - STOPPING THERE",
                   rr,aa,bb,btop-bbot ) ;
                }
                aa += bbot ; bb += bbot ; if( bb > btop ) bb = btop ;
              }
            } else {           /* global indexes: check for stupidities */
              if( aa >= nx ){
                WARNING_message(
                 "-CENSORTR %d..%d has start index past end of data (%d) - IGNORING",
                 rr,aa,bb,nx-1 ) ; aa = -66666666 ;
              } else if( bb > nx ){
                WARNING_message(
                 "-CENSORTR %d..%d has stop index past end of data (%d) - STOPPING THERE",
                 rr,aa,bb,nx-1 ) ; bb = nx-1 ;
              }
            }
            if( aa < 0  || aa >= nx ) continue ;  /* nothing to do */
            if( bb < aa || bb >= nx ) continue ;
            for( it=aa ; it <= bb ; it++ ){
              censor_array[it] = 0.0f ; censor_rgb[it] = clr ;
            }
          } /* end of loop over CENSOR commands */
          free((void *)abc_CENSOR) ; abc_CENSOR = NULL ; num_CENSOR = 0 ;
        }

        /* at this point, convert censor_array to vboxes */

#undef  FTEQ
#define FTEQ(p,q) ( (p).a==(q).a && (p).b==(q).b && (p).c==(q).c )

        for( ic=0 ; ic < nx ; ){
          if( censor_array[ic] > 0.0f ){ ic++ ; continue ; }
          clr = censor_rgb[ic] ;
          for( it=ic+1 ; it < nx ; it++ ){
            if( censor_array[it] > 0.0f ) break ;
            if( !FTEQ(clr,censor_rgb[it]) ) break ;
          }
          plot_ts_add_vbox( -1 , xar[ic],xar[it-1] , clr.a,clr.b,clr.c ) ;
          ic = it ;
        }
     } /** end of censoring => vboxes */

   } else {   /** -x option was given */

      MRI_IMAGE *inimx ;
      if( censor_array != NULL || num_CENSOR > 0 )
        WARNING_message("-x option used ==> -censor and -CENSORTR are ignored!") ;

      inimx = mri_read_1D( xfile ) ;  /* read x-axis */
      if( inimx == NULL )
         ERROR_exit("Can't read x-axis '-x %s'",xfile) ;
      if (inimx->nx < flim->nx)
         ERROR_exit("Number of rows in '-x %s' fewer than in plot data",xfile) ;
      if (inimx->ny != 1)
         WARNING_message("Using only first column from '-x %s'",xfile) ;
      far = MRI_FLOAT_PTR(inimx);
      xar = (float *) malloc( sizeof(float) * nx ) ;
      for( ii=0 ; ii < nx ; ii++ ) xar[ii] = far[ii+ignore] ;
      mri_free(inimx); inimx=NULL; far = NULL;
      if( xl10 )
        for( ii=0 ; ii < nx ; ii++ ) xar[ii] = log10(fabs(xar[ii])) ;
   }

   plot_ts_dobox(tsbox) ; plot_ts_noline(noline) ; /* 23 May 2011 */

   /*--- start X11 ---*/

   if( !skip_x11 ){
     set_wintitle_memplot(wintitle);  /* ZSS Oct. 7 2009 */

     (void) XtAppAddTimeOut( app , 123 , startup_timeout_CB , NULL ) ;
     XtAppMainLoop(app) ;   /* never returns */
   }

   /*---------------------------------------------------*/
   /* 29 Nov 2002: if here, output PostScript to stdout */
   /* 06 Dec 2007: or write plot to an image file       */

   { MEM_plotdata *mp ;
     int ymask = (sep) ? TSP_SEPARATE_YBOX : 0 ;
     if (sepscl) ymask = ymask | TSP_SEPARATE_YSCALE;

     mp = plot_ts_mem( nx,xar , nts,ymask,yar ,
                       xlabel , ylabel , title , yname ) ;

          if( out_ps )              memplot_to_postscript( "-" , mp ) ;
     else if( imsave == JPEG_MODE ) memplot_to_jpg( imfile , mp ) ;
     else if( imsave == PNG_MODE  ) memplot_to_png( imfile , mp ) ;
     else                           ERROR_message("You shouldn't see this message!") ;
   }

   exit(0) ;
}

/*-----------------------------------------------------------------*/
void killfunc(void *fred){ exit(0) ; }
/*-----------------------------------------------------------------*/

void startup_timeout_CB( XtPointer client_data , XtIntervalId *id )
{
   int ng , ngx;

   /* make graph */

   memplot_topshell_setsaver( ".jpg" , memplot_to_jpg ) ; /* 05 Dec 2007 */
   memplot_topshell_setsaver( ".png" , memplot_to_png ) ;

   ng = (sep) ? (-nts) : (nts) ;
   ngx = (sepscl) ? (-nx) : (nx) ;
   plot_ts_lab( dc->display , ngx , xar , ng , yar ,
                xlabel , ylabel , title , yname , killfunc ) ;

   return ;
}
