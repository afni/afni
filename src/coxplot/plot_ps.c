#include <stdio.h>
#include <time.h>
#include <math.h>

/***************************************************
 * routines to do Unix style plot calls to produce *
 * PostScript code written to an output file!      *
 ***************************************************/

/* user callable routines */

void ps_move( int , int ) ;               /* move current position     */
void ps_line( int , int , int , int ) ;   /* draw a line from a to b   */
void ps_cont( int , int ) ;               /* draw a line from current  */
void ps_point( int , int ) ;              /* draw a point              */
void ps_label( char * ) ;                 /* draw a string (Courier)   */
void ps_arc( int , int , int , int , int , int ) ;  /* draw an arc     */
void ps_circle( int , int , int ) ;                 /* draw a circle   */
void ps_erase( void ) ;                             /* new page        */
void ps_linemod( char * ) ;                         /* line styles     */
void ps_space( int , int , int , int ) ;            /* set plot space  */
int  ps_openpl( char * ) ;                          /* open plot file  */
void ps_closepl( void ) ;                           /* close plot file */
void ps_setrgb( float , float , float ) ;           /* set color */
void ps_setwidth( float ) ;                         /* set linewidth */
void ps_rect( int,int,int,int ) ;                   /* filled rectangle */

/* Fortran (via f2c) interface */

void zzpsco_( float *, float *, float *) ;      /* set color   */
void zzpsop_( char *cfl , int ncfl ) ;          /* open file   */
void zzpsli_( int *, int *, int *, int *) ;     /* draw line   */
void zzpsfr_( void ) ;                          /* finish page */
void zzpscl_( void ) ;                          /* close file  */

/* internal routines */

void ps_maybe_stroke( void ) ;
void ps_stroke( void ) ;
int  ps_setfont( void ) ;
void ps_prolog( void ) ;
void ps_epilog( void ) ;
void ps_clear( void ) ;

/* global data */

static int font;
static int error;
static int npages=0;
static int cx,cy;
static int ttcur=0;
static int atcur=0;
static int inpath=0;
static int plot=0;
static double scal=1.0;
static int prolog_not_output = 1 ;
static FILE *psfile = NULL ;
static int psfile_ispipe = 0 ;  /* RWCox */

/* routines */

void ps_maybe_stroke( void )
{ if (inpath>100) ps_stroke();  }

void ps_stroke( void )
{ fprintf( psfile , "S\n") ; atcur=inpath=0 ; }

void ps_move( int ix , int iy )
{
  if( atcur && cx == ix && cy == iy ) return ;
  cx = ix ;
  cy = iy ;
  ttcur=atcur=0;
}

void ps_line( int ix1 , int iy1 , int ix2 , int iy2 )
{ ps_move( ix1 , iy1 ) ;
  ps_cont( ix2 , iy2 ) ;
}

void zzpsli_( int *ix1 , int *iy1 , int *ix2 , int *iy2 )
{ ps_line( *ix1,*iy1 , *ix2,*iy2 ) ; }

void ps_cont( int ix , int iy )
{ if (!inpath) fprintf( psfile , "NP ");
  if (!atcur) fprintf( psfile , "%d %d M\n",cx,cy);
  ps_move( ix , iy ) ;
  fprintf( psfile , "%d %d N\n",cx,cy);
  ttcur=0;
  atcur=plot=1;
  inpath++;
  ps_maybe_stroke() ;
}

void ps_rect( int x1,int y1 , int x2,int y2 )
{
   if( inpath ) ps_stroke() ;
   fprintf( psfile , "NP ");
   fprintf( psfile , "%d %d M ",x1,y1);
   fprintf( psfile , "%d %d N ",x2,y1);
   fprintf( psfile , "%d %d N ",x2,y2);
   fprintf( psfile , "%d %d N ",x1,y2);
   fprintf( psfile , "%d %d N ",x1,y1);
   fprintf( psfile , "F\n") ;
}

void ps_point( int ix , int iy )
{ if (inpath) ps_stroke() ;
  ps_move( ix , iy ) ;
  fprintf( psfile , "%d %d %c\n",cx,cy,'P');
  ttcur=atcur=inpath=0;
  plot=1;
}

void ps_label( char * s )
{ int is ;
  char c ;

  if (inpath) ps_stroke() ;
  if (!ttcur) fprintf( psfile , "%d %d M\n",cx,cy);
  if (!font) font=ps_setfont();
  fprintf( psfile , "(");
  for( is=0,c=s[is] ; (c!='\0')&&(c!='\n') ; ++is,c=s[is] )
  {
    if (c=='(' || c==')' || c=='\\')putchar('\\');
    putchar(c);
  }
  fprintf( psfile , ") T\n");
  ttcur=plot=1;
  atcur=inpath=0;
}

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

void ps_arc( int x , int y , int x1 , int y1 , int x2 , int y2 )
{  double dx , dy ;

   if (inpath) ps_stroke() ;
   dx=x1-x;
   dy=y1-y;
   fprintf( psfile , "%d %d %f ", x, y, sqrt(dx*dx+dy*dy) );
   fprintf( psfile , "%f ", (double)(atan2(dy,dx)/M_PI)*180.0 );
   dx=x2-x;
   dy=y2-y;
   fprintf( psfile , "%f ", (double)(atan2(dy,dx)/M_PI)*180.0 );
   plot=1;
   atcur=inpath=0;
}

void ps_circle( int x , int y , int r )
{  fprintf( psfile , "%d %d %d C\n",x,y,r);
   plot=1;
}

void ps_erase( void )
{ ps_clear() ; }

void zzpsfr_( void )
{ ps_erase() ; }

void ps_linemod( char * s)
{ double pt ;
  pt = 1.0 / scal ;

  if (inpath) ps_stroke() ;  /* draw anything specified before setdash */

  if (strncmp(s,"solid",5) == 0) {
     fprintf( psfile , "[] 0 setdash\n") ;
  } else if( strncmp(s,"dotted",6) == 0 ) {
     fprintf( psfile , "[ %f %f ] 0 setdash\n" , 2.0*pt , 3.0*pt ) ;
  } else if( strncmp(s,"dotdashed",9) == 0 ) {
     fprintf( psfile , "[ %f %f %f %f ] 0 setdash\n" ,
	    2.0*pt , 3.0*pt , 6.0*pt , 3.0*pt ) ;
  } else if( strncmp(s,"shortdashed",11) == 0 ) {
     fprintf( psfile , "[ %f %f ] 0 setdash\n" , 6.0*pt , 3.0*pt ) ;
  } else if( strncmp(s,"longdashed",10) == 0 ) {
     fprintf( psfile , "[ %f %f ] 0 setdash\n" , 9.0*pt , 4.5*pt ) ;
  } else {
     fprintf(stderr,
	     "plotps: linestyle '%s' not implemented.\n",s);
     fprintf( psfile , "[] 0 setdash\n") ;
  }
}
	
void ps_space( int ix1 , int iy1 , int ix2 , int iy2 )
{ if( prolog_not_output ) ps_prolog() ;
  if (inpath) ps_stroke() ;
  fprintf( psfile , "initgraphics\n");
  fprintf( psfile , "1 setlinewidth\n");
  fprintf( psfile , "66 72 translate\n");
  scal=480.0/(ix2-ix1);
  fprintf( psfile , "%f %f scale\n",scal,480.0/(iy2-iy1));
  if (ix1 || iy1) fprintf( psfile , "%d %d translate\n",-ix1, -iy1);
  ps_linemod( "solid" ) ;
  atcur=inpath=font=0;
}

void ps_setwidth( float www )
{ if( inpath ) ps_stroke() ;
  fprintf( psfile , "%f setlinewidth\n" , www ) ;
}

void ps_setrgb( float rrr , float ggg , float bbb )
{ if( inpath ) ps_stroke() ;
  fprintf( psfile , "%f %f %f setrgbcolor\n" , rrr,ggg,bbb ) ;
}

void zzpsco_( float *rrr , float *ggg , float *bbb )
{ ps_setrgb( *rrr,*ggg,*bbb ) ; }

int ps_setfont( void )
{
    fprintf( psfile , "%f SF\n",12.0/scal);
    return 1;
}

int ps_openpl( char *fname )
{
  if( strcmp(fname,"-") == 0 ){           /* 29 Nov 2002: to stdout */
    psfile = stdout ;
    psfile_ispipe = 0 ;
  } else if( fname[0] != '|' ){           /* normal file */
    psfile = fopen( fname , "w" ) ;
    psfile_ispipe = 0 ;
  } else {                                /* open a pipe */
    psfile = popen( fname+1 , "w" ) ;
    psfile_ispipe = 1 ;
  }
  if( psfile == NULL ) return 0 ;
  ps_prolog();
  return 1 ;
}

void zzpsop_( char *cfl , int ncfl )  /* RWCox */
{ int i ;
  char ccc[128] ;

  for( i=0 ; (i < 127) && (i < ncfl) && (cfl[i] != ' ') ; i++ ) {
     ccc[i] = cfl[i] ;
  }
  ccc[i] = '\0' ;
  ps_openpl( ccc ) ; if( psfile == NULL ) return ;
  ps_space( 0,0,4096,4096 ) ;
}

void ps_closepl( void )
{ ps_epilog();

  if( psfile == stdout ){                   /* 29 Nov 2002: don't close stdout */
    fflush(psfile) ;                        /*              just flush it */
  } else {
    if( ! psfile_ispipe ) fclose(psfile) ;
    else                  pclose(psfile) ;  /* RWCox */
  }

  psfile = NULL ; psfile_ispipe = 0 ;
}

static char *prolog_text[] = {
    "%%BoundingBox: 36 36 540 690",
    "%%Title: plotps output",
    "%%Creator: plotps 1.0 (RWCox)",
    "%%Pages: (atend)",
    "%%DocumentFonts: Times-Roman",
    "%%EndComments",
    "/S{stroke}bind def",
    "/F{fill}bind def" ,
    "/NP{newpath}bind def",
    "/M{moveto}bind def",
    "/N{lineto}bind def",
    "/A{NP arc S}bind def",
    "/C{0 360 A}bind def",
    "/P{1 0 360 A}bind def",
    "/T{show}bind def",
    "/CL{showpage}bind def",
    "/SF{/Times-Roman findfont exch scalefont setfont}bind def",
    "%%EndProlog",
    NULL
};

void ps_prolog( void )
{
    time_t tt = time(NULL);
    char **p;

    fprintf( psfile , "%%!PS-Adobe-2.0 EPSF-2.0\n%%%%CreationDate: %s", ctime(&tt));
    for (p = prolog_text;  *p;  p++)
	fprintf(psfile,"%s\n" , *p);
    font=0;
    prolog_not_output = 0 ;
}

void ps_clear( void )
{ if (inpath) ps_stroke() ;
  if (plot)
  {
    fprintf( psfile , "CL\n");
    npages++;
    atcur=inpath=ttcur=plot=0;
  }
}

#undef USE_EOT  /* RWCox */

void ps_epilog( void )
{ ps_clear();
#ifdef USE_EOT
  fprintf( psfile , "%%%%Trailer\n%%%%Pages: %d\n%c", npages,4); /* inc. EOT */
#else
  fprintf( psfile , "%%%%Trailer\n%%%%Pages: %d\n", npages); /* no EOT */
#endif
}

void zzpscl_( void )
{ ps_closepl() ; }
