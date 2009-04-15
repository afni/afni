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
static char *title = NULL , *xlabel = NULL , *ylabel = NULL ;

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
   int do_norm=0 ; /* 26 Mar 2008 */

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dplot [options] tsfile ...\n"
            "Graphs the columns of a *.1D time series file to the X11 screen.\n"
            "\n"
            "Options:\n"
            " -install   = Install a new X11 colormap.\n"
            " -sep       = Plot each column in a separate sub-graph.\n"
            " -one       = Plot all columns together in one big graph.\n"
            "                [default = -sep]\n"
            " -sepscl    = Plot each column in a separate sub-graph\n"
            "              and allow each sub-graph to have a different\n"
            "              y-scale.  -sepscl is meaningless with -one!\n"
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
            " -xaxis b:t:n:m    = Set the x-axis to run from value 'b' to\n"
            "                     value 't', with 'n' major divisions and\n"
            "                     'm' minor tic marks per major division.\n"
            "                     For example:\n"
            "                       -xaxis 0:100:5:20\n"
            "                     Setting 'n' to 0 means no tic marks or labels.\n"
            "\n"
            " -yaxis b:t:n:m    = Similar to above, for the y-axis.  These\n"
            "                     options override the normal autoscaling\n"
            "                     of their respective axes.\n"
            "\n"
            " -ynames aa bb ... = Use the strings 'aa', 'bb', etc., as\n"
            "                     labels to the right of the graphs,\n"
            "                     corresponding to each input column.\n"
            "                     These strings CANNOT start with the\n"
            "                     '-' character.\n"
            "               N.B.: Each separate string after '-ynames'\n"
            "                     is taken to be a new label, until the\n"
            "                     end of the command line or until some\n"
            "                     string starts with a '-'.  In particular,\n"
            "                     This means you CANNOT do something like\n"
            "                       1dplot -ynames a b c file.1D\n"
            "                     since the input filename 'file.1D' will\n"
            "                     be used as a label string, not a filename.\n"
            "                     Instead, you must put another option between\n"
            "                     the end of the '-ynames' label list, OR you\n"
            "                     can put a single '-' at the end of the label\n"
            "                     list to signal its end:\n"
            "                       1dplot -ynames a b c - file.1D\n"
            "\n"
            " -volreg           = Makes the 'ynames' be the same as the\n"
            "                     6 labels used in plug_volreg for\n"
            "                     Roll, Pitch, Yaw, I-S, R-L, and A-P\n"
            "                     movements, in that order.\n"
            "\n"
            " -thick            = Each time you give this, it makes the line\n"
            "                     thickness used for plotting a little larger.\n"
            "                     [An alternative to using '-DAFNI_1DPLOT_THIK=...']\n"
            "\n"
            " -Dname=val        = Set environment variable 'name' to 'val'\n"
            "                     for this run of the program only:\n"
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
            "fractions of the page size.\n"

            "\n"
            TS_HELP_STRING
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("1dplot main"); machdep(); PRINT_VERSION("1dplot"); AUTHOR("RWC et al.");

   /* 29 Nov 2002: scan for things that make us skip X11 */

   for( ii=1 ; ii < argc ; ii++ ){
     if( strcasecmp(argv[ii],"-ps")   == 0 ){ skip_x11 = 1; break; }
     if( strcasecmp(argv[ii],"-jpg")  == 0 ){ skip_x11 = 1; break; }
     if( strcasecmp(argv[ii],"-jpeg") == 0 ){ skip_x11 = 1; break; }
     if( strcasecmp(argv[ii],"-png")  == 0 ){ skip_x11 = 1; break; }
   }

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

     if( strcmp(argv[iarg],"-") == 0 ){  /* 23 Aug 2006: null option */
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-thi",4) == 0 ){  /* 15 Apr 2009: thickness */
       thik += 0.005f ; iarg++ ; continue ;
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
       xfile = argv[++iarg]; xl10 = 0 ;
       iarg++; continue;
     }
     if( strcasecmp(argv[iarg],"-xl10") == 0 ){
       xfile = argv[++iarg]; xl10 = 1 ;
       iarg++; continue;
     }

     if( strcmp(argv[iarg],"-xaxis") == 0 ){   /* 22 Jul 2003 */
       sscanf(argv[++iarg],"%f:%f:%d:%d",&xbot,&xtop,&nnax,&mmax) ;
       if( xbot >= xtop || nnax < 0 || mmax < 1 )
         ERROR_exit("String after -xaxis is illegal!\n") ;

       plot_ts_xfix( nnax,mmax , xbot,xtop ) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-yaxis") == 0 ){   /* 22 Jul 2003 */
       sscanf(argv[++iarg],"%f:%f:%d:%d",&ybot,&ytop,&nnay,&mmay) ;
       if( ybot >= ytop || nnay < 0 || mmay < 1 )
         ERROR_exit("String after -yaxis is illegal!\n") ;

       plot_ts_yfix( nnay,mmay , ybot,ytop ) ;
       iarg++ ; continue ;
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

     if( strcasecmp(argv[iarg],"-png") == 0 ){
        out_ps = 0 ; imsave = PNG_MODE ;
        iarg++ ; if( iarg >= argc ) ERROR_exit("need argument after '%s'",argv[iarg-1]) ;
        imfile = (char *)malloc(strlen(argv[iarg])+8) ; strcpy(imfile,argv[iarg]) ;
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

     if( strcmp(argv[iarg],"-") == 0 ){  /* skip */
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-ynames") == 0 ){
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
        title = argv[++iarg] ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-title") == 0 ){ /* normally eaten by XtVaAppInitialize */
#if 0
        WARNING_message(                     /* unless  using -ps! So keep it here, */
         "Consider using -plabel; -title "   /* it don't hurt. */
         "only works with the -ps / -jpg / -png options"  );
#endif
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
        if( ignore < 0 ) ERROR_exit("Illegal -ignore value!\n") ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-use") == 0 || strcmp(argv[iarg],"-num") == 0 ){
        use = strtod( argv[++iarg] , NULL ) ;
        if( use < 2 ) ERROR_exit("Illegal -use value!\n") ;
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-dx" ) == 0 ||
         strcmp(argv[iarg],"-del") == 0 ||
         strcmp(argv[iarg],"-dt" ) == 0   ){

        dx = strtod( argv[++iarg] , NULL ) ;
        if( dx <= 0.0 ) ERROR_exit("Illegal -dx value!\n");
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-xzero") == 0 || strcmp(argv[iarg],"-start") == 0 ||
         strcmp(argv[iarg],"-tzero") == 0   ){
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

#if 0
     if( strncmp(argv[iarg],"-D",2) == 0 && strchr(argv[iarg],'=') != NULL ){
       (void) AFNI_setenv( argv[iarg]+2 ) ;
       iarg++ ; continue ;
     }
#endif

     ERROR_exit("Unknown option: %s\n",argv[iarg]) ;
   }

   if( thik > 0.0f ){
     char cmd[128]; sprintf(cmd,"AFNI_1DPLOT_THIK=%.3f",thik); AFNI_setenv(cmd);
   }

   if(sepscl && sep == 0) {
      WARNING_message("Cannot use -sepscl with -one!") ; sepscl=0 ;
   }
   if( iarg >= argc && !use_stdin )
      ERROR_exit("No time series file on command line!\n") ;

   /*-- setup color info --*/

   if( !skip_x11 )
     dc = MCW_new_DC( shell , 16 ,
                      DEFAULT_NCOLOVR , INIT_colovr , INIT_labovr ,
                      1.0 , install ) ;

   if( nyar > 0 ) yname = ynar ;

   /*-- 01 Aug 2001: read from stdin instead of a file --*/

#define NLBUF 131072
#define NVMAX 10000
   if( use_stdin ){
     char *lbuf , *cpt , *dpt ;
     int   nval ;
     float *val , fff ;

     lbuf = (char * )malloc(sizeof(char )*NLBUF) ;
     val  = (float *)malloc(sizeof(float)*NVMAX) ;

     /** 13 May 2005: modified to read up to NVMAX numbers from stdin,
                      rather than the fixed size array of length 9 of old **/

     do{               /* read lines until 1st char is non-blank and non-# */
       cpt = fgets(lbuf,NLBUF,stdin) ;
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
        cpt = fgets(lbuf,NLBUF,stdin) ;
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
       inim = mri_read_1D( argv[iarg] ) ;
       if( inim == NULL )
         ERROR_exit("Can't read input file '%s'\n",argv[iarg]) ;

     } else {                              /* multiple inputs [05 Mar 2003] */
       MRI_IMARR *imar ;                   /* read them & glue into 1 image */
       int iarg_first=iarg, nysum=0, ii,jj,nx=1 ;
       float *far,*iar ;

       INIT_IMARR(imar) ;
       for( ; iarg < argc ; iarg++ ){
         inim = mri_read_1D( argv[iarg] ) ;
         if( inim == NULL )
           ERROR_exit("Can't read input file '%s'\n",argv[iarg]) ;

           if( inim->nx == 1 && inim->ny > 1 ){
             flim = mri_transpose(inim); mri_free(inim); inim = flim;
           }

         if( iarg == iarg_first || inim->nx < nx ) nx = inim->nx ;
         ADDTO_IMARR(imar,inim) ; nysum += inim->ny ;
       }
       flim = mri_new( nx,nysum, MRI_float ); far = MRI_FLOAT_PTR(flim);
       for( nysum=ii=0 ; ii < imar->num ; ii++ ){
         inim = IMARR_SUBIM(imar,ii) ; iar = MRI_FLOAT_PTR(inim) ;
         for( jj=0 ; jj < inim->ny ; jj++,nysum++ ){
           memcpy( far + nx*nysum , iar + jj*inim->nx , sizeof(float)*inim->nx ) ;
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

   switch( do_norm ){  /* 26 Mar 2008 */
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

   if (!xfile) {
      xar = (float *) malloc( sizeof(float) * nx ) ;
      for( ii=0 ; ii < nx ; ii++ ) xar[ii] = xzero + dx*ii ;
   } else {
      /* read xfile */
      MRI_IMAGE *inimx = mri_read_1D( xfile ) ;
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

   /*--- start X11 ---*/

   if( !skip_x11 ){
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
