#include "coxplot.h"
#include <math.h>

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*------------------------------------------------------------------------*/

#ifndef WAY_BIG
#  define WAY_BIG 1.e+10
#endif

static float p10( float x ) ;  /* prototype */

#undef  NCLR_MAX
#define NCLR_MAX 99
static float ccc[NCLR_MAX][3] = {
  { 0.0 , 0.0 , 0.0 } ,
  { 0.9 , 0.0 , 0.0 } ,
  { 0.0 , 0.7 , 0.0 } ,
  { 0.0 , 0.0 , 0.9 } ,
  { 0.8 , 0.0 , 0.9 } ,
  { 0.7 , 0.6 , 0.0 } ,
  { 0.0 , 0.7 , 0.7 }
} ;

static int use_ddd = 0 ;
static int ddd[NCLR_MAX] = { 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                             1,1,1,1,1,1,1,1,1 } ;

static int NCLR = 7 ;
static int dont_init_colors=0 ;

static int ilab[4] = { 0,2,3,1 } ;  /* whether to plot labels on axes */

#define STGOOD(s) ( (s) != NULL && (s)[0] != '\0' )

#define SY   0.07

static float THIK = 0.004f ;  /* 27 Mar 2004: changed from a #define */
static float thik = 0.002f ;  /* 02 Apr 2012: for labels */

static float aspect = 1.3f ;  /* 03 May 2018 */
static int   do_perim = 1 ;
static int   do_naked = 0 ;

#define ASX(xv) ((xv)*aspect/1.3f)

void plot_ts_set_aspect( float asp ){ aspect = asp ; }
void plot_ts_do_perim  ( int   dp  ){ do_perim = dp ; }

void plot_ts_do_naked  ( int   nn  ){
  do_naked = nn ;
  if( do_naked ) do_perim = 0 ;
}

/*----------------------------------------------------------------------*/
static float tsbox  = 0.0f ;      /* 23 May 2011 */
static float noline = 0 ;
static int   dohist = 0 ;         /* 10 Jul 2014 */

void plot_ts_dobox( float a ){ tsbox = a ; }
void plot_ts_noline( int a ){ noline = a ; }
void plot_ts_dohist( int a ){ dohist = a ; }

static void plot_one_diamond( float xx , float yy )
{
   float x , y ;
   plotpak_zzphys( xx , yy , &x , &y ) ;
   plotpak_phline( x+tsbox , y       , x       , y+tsbox ) ;
   plotpak_phline( x       , y+tsbox , x-tsbox , y       ) ;
   plotpak_phline( x-tsbox , y       , x       , y-tsbox ) ;
   plotpak_phline( x       , y-tsbox , x+tsbox , y       ) ;
}

static void plot_one_hexagon( float xx , float yy )
{
   float x , y , da=0.5f*tsbox , db=0.866f*tsbox ;
   plotpak_zzphys( xx , yy , &x , &y ) ;
   plotpak_phline( x    , y+tsbox    , x-db , y+tsbox-da ) ;
   plotpak_phline( x-db , y+tsbox-da , x-db , y-tsbox+da ) ;
   plotpak_phline( x-db , y-tsbox+da , x    , y-tsbox    ) ;
   plotpak_phline( x    , y-tsbox    , x+db , y-tsbox+da ) ;
   plotpak_phline( x+db , y-tsbox+da , x+db , y+tsbox-da ) ;
   plotpak_phline( x+db , y+tsbox-da , x    , y+tsbox    ) ;
}

static void plot_one_square( float xx , float yy )
{
   float x , y ;
   plotpak_zzphys( xx , yy , &x , &y ) ;
   plotpak_phline( x-tsbox , y-tsbox , x+tsbox , y-tsbox ) ;
   plotpak_phline( x+tsbox , y-tsbox , x+tsbox , y+tsbox ) ;
   plotpak_phline( x+tsbox , y+tsbox , x-tsbox , y+tsbox ) ;
   plotpak_phline( x-tsbox , y+tsbox , x-tsbox , y-tsbox ) ;
}

static void plot_one_utriang( float xx , float yy )
{
   float x , y , da=0.5f*tsbox , db=0.866f*tsbox ;
   plotpak_zzphys( xx , yy , &x , &y ) ;
   plotpak_phline( x    , y+tsbox , x-db , y-da    ) ;
   plotpak_phline( x-db , y-da    , x+db , y-da    ) ;
   plotpak_phline( x+db , y-da    , x    , y+tsbox ) ;
}

static void plot_one_dtriang( float xx , float yy )
{
   float x , y , da=0.5f*tsbox , db=0.866f*tsbox ;
   plotpak_zzphys( xx , yy , &x , &y ) ;
   plotpak_phline( x    , y-tsbox , x-db , y+da    ) ;
   plotpak_phline( x-db , y+da    , x+db , y+da    ) ;
   plotpak_phline( x+db , y+da    , x    , y-tsbox ) ;
}

#undef  CIRCNUM
#define CIRCNUM 16
static float circc[CIRCNUM+1] , circs[CIRCNUM+1] ;

static void circ_setup(void)
{
   static int circdone=0 ; int ii ;
   if( circdone ) return ;
   for( ii=0 ; ii < CIRCNUM+1 ; ii++ ){
     circc[ii] = cosf(2.0f*3.141592f*ii/(float)CIRCNUM) ;
     circs[ii] = sinf(2.0f*3.141592f*ii/(float)CIRCNUM) ;
   }
   circdone = 1 ;
}

static void plot_one_circle( float xx , float yy )
{
   float x , y ; int ii ;
   circ_setup() ;
   plotpak_zzphys( xx , yy , &x , &y ) ;
   for( ii=0 ; ii < CIRCNUM ; ii++ )
     plotpak_phline( x+tsbox*circc[ii], y+tsbox*circs[ii], x+tsbox*circc[ii+1], y+tsbox*circs[ii+1] ) ;
}

static void plot_one_stellar( float xx , float yy )
{
   float x , y , dt=0.5f*tsbox ; int ii ;
   circ_setup() ;
   plotpak_zzphys( xx , yy , &x , &y ) ;
   for( ii=0 ; ii < CIRCNUM ; ii+=2 ){
     plotpak_phline( x+tsbox*circc[ii], y+tsbox*circs[ii], x+dt*circc[ii+1], y+dt*circs[ii+1] ) ;
     plotpak_phline( x+dt*circc[ii+1], y+dt*circs[ii+1], x+tsbox*circc[ii+2], y+tsbox*circs[ii+2] ) ;
   }
}

static void plot_onebox( float xx , float yy , int kk )
{
   switch( kk%7 ){  /* choose box shape depending on which curve is being plotted */
     default:
     case 0:  plot_one_diamond(xx,yy) ; break ;
     case 1:  plot_one_circle (xx,yy) ; break ;
     case 2:  plot_one_utriang(xx,yy) ; break ;
     case 3:  plot_one_stellar(xx,yy) ; break ;
     case 4:  plot_one_square (xx,yy) ; break ;
     case 5:  plot_one_dtriang(xx,yy) ; break ;
     case 6:  plot_one_hexagon(xx,yy) ; break ;
   }
}
/*---------------------------------------------------------------------------*/

static int ts_gcd( int m , int n )    /* Euclid's Greatest Common Denominator */
{
  while( m > 0 ){
    if( n > m ){ int t=m; m=n; n=t; } /* swap */
    m -= n ;
  }
  return n ;
}

/*---------------------------------------------------------------------------*/

static int ts_find_relprime( int n )  /* find number relatively prime to n */
{
   int dj , n5=n/5 ;
   if( n5 < 2 ) return 1 ;
   for( dj=n5 ; ts_gcd(n,dj) > 1 ; dj++ ) ; /*nada*/
   return dj ;
}

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
       } else if( strcasecmp(eee,"pink") == 0 ){
         ccc[ii][0] = 0.9f; ccc[ii][1] = 0.3f; ccc[ii][2] = 0.5f; NCLR = ii+1;
       } else if( strcasecmp(eee,"cyan") == 0 ){
         ccc[ii][0] = 0.0f; ccc[ii][1] = 0.7f; ccc[ii][2] = 0.7f; NCLR = ii+1;
       } else if( *eee == '#' && *(eee+1) != '\0' ){
         int le=strlen(eee+1) , val , bas , rr,gg,bb ;
         val = (int)strtol( eee+1 , NULL , 16 ) ;
         bas = (le <= 3) ? 16 : 256 ;
         bb  = val % bas ; val = val / bas ; bf  = bb / ((float)bas) ;
         gg  = val % bas ; val = val / bas ; gf  = gg / ((float)bas) ;
         rr  = val % bas ;                   rf  = rr / ((float)bas) ;
         ccc[ii][0] = rf ; ccc[ii][1] = gf ; ccc[ii][2] = bf ; NCLR = ii+1 ;
       } else {
         int jj = find_color_name(eee,&rf,&gf,&bf) ;
         if( jj >= 0 ){
           ccc[ii][0] = rf ; ccc[ii][1] = gf ; ccc[ii][2] = bf ; NCLR = ii+1 ;
         } else {
           fprintf(stderr, "** ERROR: %s = %s is not a recognizable color\n", ename,eee ) ;
           fprintf(stderr, "   Recognizable color names include\n"
                           "     green red blue black purple gold pink cyan OR #xxxxxx\n"
                           "   where 'xxxxxx' is 6 hex digits for RGB.\n" ) ;
         }
       }
     }
   }

   eee = getenv("AFNI_1DPLOT_THIK") ;                     /* 27 Mar 2004 */
   if( eee == NULL ) eee = getenv("AFNI_1DPLOT_THICK") ;  /* 15 Apr 2009 */
   if( eee != NULL ){
     rf = (float)strtod(eee,NULL) ; plot_ts_setTHIK(rf) ;
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

/*-----------------------------------------------------------------------*/

void plot_ts_setdash( int ndash , int *code )
{
   int ii ;
   if( ndash <= 0 || code == NULL ){ use_ddd = 0 ; return ; }
   if( ndash > NCLR_MAX ) ndash = NCLR_MAX ;
   for( ii=0 ; ii < ndash ; ii++ ) ddd[ii] = code[ii] ;
   use_ddd = 1 ;
   return ;
}

/*-----------------------------------------------------------------------*/

void plot_ts_setTHIK( float thk )  /* for lines */
{
        if( thk < 0.001f ) THIK = 0.001f ;
   else if( thk > 0.020f ) THIK = 0.020f ;
   else                    THIK = thk    ;
}

void plot_ts_setthik( float thk )  /* for labels */
{
        if( thk < 0.000f ) thik = 0.000f ;
   else if( thk > 0.005f ) thik = 0.005f ;
   else                    thik = thk    ;
}

/*-----------------------------------------------------------------------*/

static int   thik_n1 = -666 , thik_n2 = -999 ;
static float thik_12 = 0.002f ;

void plot_ts_setthik_12( int n1, int n2, float thk )
{
   if( n1 < 0 || n2 < n1 ){
     thik_n1 = -666 ; thik_n2 = -999 ;
   } else {
     thik_n1 = n1 ; thik_n2 = n2 ;
          if( thk < 0.000f ) thik_12 = 0.000f ;
     else if( thk > 0.005f ) thik_12 = 0.005f ;
     else                    thik_12 = thk    ;
   }
}

/*-----------------------------------------------------------------------*/

#undef  VBOX
#define VBOX  1

#undef  RBOX
#define RBOX  2   /* 29 Jan 2017 */

#undef  TLIN
#define TLIN  3   /* 19 Sep 2017 */

typedef struct {
  int code ;
  int ygr ;
  float x1 , x2 ;
  float rr , gg , bb ;
  float rb_y1 , rb_y2 , rb_y3 ,  /* RBOX stuff [29 Jan 2017] */
        rb_rr,rb_gg,rb_bb ;
  int   dcode ;                  /* for TLIN: dashcode */
} TS_vbox ;

static int     nvbox = 0 ;
static TS_vbox *vbox = NULL ;

void plot_ts_add_vbox( int ygr , float x1 , float x2 ,
                       float rr , float gg , float bb )
{
   vbox = (TS_vbox *)realloc( (void *)vbox , sizeof(TS_vbox)*(nvbox+1) ) ;
   vbox[nvbox].code = VBOX ;
   vbox[nvbox].ygr  = ygr ;
   vbox[nvbox].x1   = x1 ;
   vbox[nvbox].x2   = x2 ;
   vbox[nvbox].rr   = rr ;
   vbox[nvbox].gg   = gg ;
   vbox[nvbox].bb   = bb ;
   nvbox++ ;
}

void plot_ts_add_rbox( int ygr ,
                       float x1,float y1 , float x2,float y2,float y3 ,
                       float rr,float gg,float bb,
                       float r2,float g2,float b2 )  /* 29 Jan 2017 */
{
   vbox = (TS_vbox *)realloc( (void *)vbox , sizeof(TS_vbox)*(nvbox+1) ) ;
   vbox[nvbox].code = RBOX ;
   vbox[nvbox].ygr  = ygr ;
   vbox[nvbox].x1   = x1 ;
   vbox[nvbox].x2   = x2 ;
   vbox[nvbox].rr   = rr ;
   vbox[nvbox].gg   = gg ;
   vbox[nvbox].bb   = bb ;

   vbox[nvbox].rb_y1 = y1 ;  /* spec for rectangle (RBOX) */
   vbox[nvbox].rb_y2 = y2 ;  /* with filled color (above) */
   vbox[nvbox].rb_y3 = y3 ;  /* with filled color (above) */
   vbox[nvbox].rb_rr = r2 ;  /* and outline around it (color here) */
   vbox[nvbox].rb_gg = g2 ;
   vbox[nvbox].rb_bb = b2 ;
   nvbox++ ;
}

void plot_ts_add_tlin( int ygr ,
                       float x1,float y1 , float x2,float y2 ,
                       float rr,float gg,float bb,int dcode ) /* 19 Sep 2017 */
{
   vbox = (TS_vbox *)realloc( (void *)vbox , sizeof(TS_vbox)*(nvbox+1) ) ;
   vbox[nvbox].code = TLIN ;
   vbox[nvbox].ygr  = ygr ;
   vbox[nvbox].x1   = x1 ;
   vbox[nvbox].x2   = x2 ;
   vbox[nvbox].rr   = rr ;
   vbox[nvbox].gg   = gg ;
   vbox[nvbox].bb   = bb ;

   vbox[nvbox].rb_y1 = y1 ;  /* spec for line (TLIN) */
   vbox[nvbox].rb_y2 = y2 ;  /* with filled color (above) */
   vbox[nvbox].dcode = dcode ; /* dash code */
   nvbox++ ;
}

void plot_ts_clear_vbox(void)
{
   if( vbox != NULL ) free(vbox) ;
   vbox = NULL ; nvbox = 0 ; return ;
}

/*-----------------------------------------------------------------------*/

static int    nsepx = 0 ;
static int   *lsepx = NULL ;
static float **sepx = NULL ;

static float sepx_bot=-1.0f , sepx_top=1.0f ;

void plot_ts_fetch_sepx( int *ns , int **ls , float ***sx )
{
   if( ns != NULL ) *ns = nsepx ;
   if( ls != NULL ) *ls = lsepx ;
   if( sx != NULL ) *sx =  sepx ;
}

void plot_ts_clear_sepx(void)
{
   if( lsepx != NULL ) free(lsepx) ;
   if( nsepx > 0 && sepx != NULL ){
     int kk ;
     for( kk=0 ; kk < nsepx ; kk++ ) free(sepx[kk]) ;
     free(sepx) ;
   }
   nsepx = 0 ; sepx = NULL ; lsepx = NULL ; return ;
}

void plot_ts_add_sepx( int lx , float *x )
{
   int kk ;

   if( lx < 2 || x == NULL ) return ;
   lsepx = (int *   )realloc( lsepx , sizeof(int)    *(nsepx+1) ) ;
    sepx = (float **)realloc(  sepx , sizeof(float *)*(nsepx+1) ) ;
    sepx[nsepx] = (float *)malloc( sizeof(float)*lx ) ;
   lsepx[nsepx] = lx ;
   for( kk=0 ; kk < lx ; kk++ ) sepx[nsepx][kk] = x[kk] ;
   nsepx++ ; return ;
}

static void plot_ts_sepx_getstats(void)
{
   int ss , ii ; float bb , tt ;

   if( nsepx <= 0 || lsepx == NULL || sepx == NULL ) return ;

   bb = tt = sepx[0][0] ;
   for( ss=0 ; ss < nsepx ; ss++ ){
     for( ii=0 ; ii < lsepx[ss] ; ii++ ){
       if( sepx[ss][ii] < bb ) bb = sepx[ss][ii] ;
       if( sepx[ss][ii] > tt ) tt = sepx[ss][ii] ;
     }
   }
   if( bb < tt ){
     sepx_bot = bb ; sepx_top = tt ;
   } else {
     sepx_bot = bb-1.0f ; sepx_top = bb+1.0f ;
   }
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

  21 Oct 2013: if global variable nsepx > 0, then the x[] axis is ignored
-------------------------------------------------------------------------*/

MEM_plotdata * plot_ts_mem( int nx , float *x , int ny , int ymask , float **y ,
                            char *lab_xxx , char *lab_yyy , char *lab_top ,
                            char **nam_yyy )
{
   int ii , jj , np , nnax,nnay , mmax,mmay ;
   float *xx=NULL , *yy=NULL ;
   float xbot,xtop , ybot,ytop , pbot,ptop , xobot,xotop,yobot,yotop ;
   float xabot,xatop ; int xflip=0 ;
   char str[32] ;
   int yall , ysep , ixtop ;
   float *ylo , *yhi , yll,yhh ;
   MEM_plotdata *mp ;
   float xb1,xb2,yb1,yb2,yb3 ; int iv ;
   float thth ;

   /*-- sanity check --*/

   if( nx <= 1 || ny == 0 || y == NULL ) return NULL ;

   init_colors() ;

   if( do_naked ){  /* 03 May 2018 */
     lab_xxx = lab_yyy = NULL ; nam_yyy = NULL ;
   }
   if( do_naked > 1 ){
     lab_top = NULL ;
   }

   /*-- make up an x-axis if none given --*/

   if( nsepx > 0 ){
     plot_ts_sepx_getstats() ;
     xbot = sepx_bot ;
     xtop = sepx_top ;
   } else if( x == NULL ){
      xx = (float *) malloc( sizeof(float) * nx ) ;
      for( ii=0 ; ii < nx ; ii++ ) xx[ii] = ii ;
      xbot = 0 ; xtop = nx-1 ;
   } else {
      xx = x ;
      xbot = WAY_BIG ; xtop = -WAY_BIG ;
      for( ii=0 ; ii < nx ; ii++ ){
         if( xx[ii] < xbot && fabsf(xx[ii]) < WAY_BIG ) xbot = xx[ii] ;
         if( xx[ii] > xtop && fabsf(xx[ii]) < WAY_BIG ) xtop = xx[ii] ;
      }
      if( xbot >= xtop ) return NULL ;
   }

   /*-- push range of x outwards --*/

   pbot = p10(xbot) ; ptop = p10(xtop) ; if( ptop < pbot ) ptop = pbot ;
   if( nnaxx >= 0 ){
     nnax  = nnaxx ;
     mmax  = mmaxx ;
     xabot = xxbot ;
     xatop = xxtop ;
     xbot  = MIN(xabot,xatop) ;
     xtop  = MAX(xabot,xatop) ;
   } else if( ptop != 0.0f && xpush > 0 ){
      np = (xtop-xbot) / ptop ;
      switch( np ){
         case 1:  ptop *= 0.1f  ; break ;
         case 2:  ptop *= 0.2f  ; break ;
         case 3:  ptop *= 0.25f ; break ;
         case 4:
         case 5:  ptop *= 0.5f  ; break ;
      }
      xbot = floor( xbot/ptop ) * ptop ; xabot = xbot ;
      xtop =  ceil( xtop/ptop ) * ptop ; xatop = xtop ;
      nnax = floor( (xtop-xbot) / ptop + 0.5f ) ;
      mmax = (nnax < 3) ? 10
                        : (nnax < 6) ? 5 : 2 ;
   } else {
      nnax = 1 ; mmax = 10 ;
      ii = (int)rint(xtop-xbot) ;
      if( fabs(xtop-xbot-ii) < 0.01 && ii <= 200 ) mmax = ii ;
      xabot = xbot ; xatop = xtop ;
   }
   xflip = (xabot > xatop) ;

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
         if( yy[ii] < yll && fabsf(yy[ii]) < WAY_BIG ) yll = yy[ii] ;
         if( yy[ii] > yhh && fabsf(yy[ii]) < WAY_BIG ) yhh = yy[ii] ;
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
   floatfix(ybot) ; floatfix(ytop) ;
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
     nnay = nnayy ;
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

   if( nnayy < 0 ){
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
   }

   nnaxx = nnayy = -1 ;

   /*-- setup to plot --*/

   create_memplot_surely( "tsplot" , aspect ) ;
   set_thick_memplot( thik ) ;  /* for labels */
#if 0
   thth = get_opacity_memplot() ;
   if( thth != 1.0f ) set_opacity_memplot(thth) ;
#endif

   /*-- plot labels, if any --*/

   if( xflip ){ xobot = 0.03; xotop = 1.15; } /* set objective size of plot */
   else       { xobot = 0.15; xotop = 1.27; }
   yobot = 0.1 ; yotop = 0.95 ;

   if( STGOOD(lab_top) ){ yotop -= 0.02 ; yobot -= 0.01 ; }
   if( nam_yyy != NULL ){
     if( xflip ){
       xobot += 0.16 ; xotop += 0.02 ;
     } else {
       xotop -= 0.16 ; xobot -= 0.02 ;
     }
   }

   if( do_naked ) /* 03 May 2018 */{
     xobot = yobot = 0.0f ; xotop = 1.3f ;
     yotop = (STGOOD(lab_top)) ? 0.89 : 0.98 ;
   }

   /* x-axis label? */

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   if( STGOOD(lab_xxx) )
     plotpak_pwritf( 0.5*(xobot+xotop), yobot-0.06, lab_xxx, 16, 0, 0 ) ;

   /* y-axis label? */

   if( STGOOD(lab_yyy) ){
     if( xflip )
       plotpak_pwritf( xotop+0.10, 0.5*(yobot+yotop), lab_yyy, 16, -90, 0 ) ;
     else
       plotpak_pwritf( xobot-0.10, 0.5*(yobot+yotop), lab_yyy, 16,  90, 0 ) ;
   }

   /* label at top? */

   if( STGOOD(lab_top) && !do_naked ){
     plotpak_pwritf( xobot , yotop+0.01 , lab_top , 18 , 0 , -2 ) ;
   }

   /*-- plot all on same vertical scale --*/

   if( yall ){
#define ADDTO_SBOX(xv,yv,qq) do{ if( nsbox == asbox ){                                      \
                                   asbox += 64 ;                                             \
                                   xsbox = (float *)realloc(xsbox,sizeof(float)*asbox) ;      \
                                   ysbox = (float *)realloc(ysbox,sizeof(float)*asbox) ;       \
                                   jsbox = (int *  )realloc(jsbox,sizeof(float)*asbox) ;        \
                                 }                                                               \
                                 xsbox[nsbox] = (xv); ysbox[nsbox] = (yv); jsbox[nsbox++] = (qq); \
                             } while(0)
      int do_sbox=0, nsbox=0, asbox=0, *jsbox=NULL ; float *xsbox=NULL, *ysbox=NULL ;

      if( noline && tsbox > 0.0f && ny > 1 ){
        char *eee = getenv("AFNI_1DPLOT_RANBOX") ;
        if( eee != NULL && (*eee == 'Y' || *eee == 'y') ) do_sbox = 1 ;
      }

      /* do name labels at right? */

      if( nam_yyy != NULL ){
         float yv = yotop ; int sz,yz ;

         for( jj=0 ; jj < ny ; jj++ ){
           if( STGOOD(nam_yyy[jj]) ){
             set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;
             set_thick_memplot( 1.234f*THIK ) ;
             if( use_ddd ) plotpak_setlin(ddd[jj%NCLR]) ;
             if( xflip )
               plotpak_line( xobot-0.008 , yv , xobot-0.041 , yv ) ;
             else
               plotpak_line( xotop+0.008 , yv , xotop+0.041 , yv ) ;
             if( use_ddd ) plotpak_setlin(1) ;
             if( tsbox > 0.0f ){
               set_thick_memplot(thik);
               if( xflip )
                { plot_onebox(xobot-0.008,yv,jj ); plot_onebox(xobot-0.041,yv,jj ); }
               else
                { plot_onebox(xotop+0.008,yv,jj ); plot_onebox(xotop+0.041,yv,jj ); }
             }
             set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
             yz = strlen(nam_yyy[jj]) ;
                  if( yz <  9 ) sz = 11 ;
             else if( yz < 11 ) sz =  9 ;
             else if( yz < 13 ) sz =  8 ;
             else if( yz < 15 ) sz =  7 ;
             else               sz =  6 ;
             set_thick_memplot( thik*sz/13.9f ) ;
             if( xflip )
               plotpak_pwritf( xobot-0.049 , yv , nam_yyy[jj] , sz , 0 ,  1 ) ;
             else
               plotpak_pwritf( xotop+0.049 , yv , nam_yyy[jj] , sz , 0 , -1 ) ;
             yv -= 0.05 ;
           }
         }
      }

      /* plot axes */

      floatfix(ybot) ; floatfix(ytop) ;
      plotpak_set( ASX(xobot),ASX(xotop) , yobot,yotop ,
                   xabot,xatop , ybot,ytop , 1 ) ;

      /* 24 Apr 2012: add vbox stuff now, before other plotting */

      for( iv=0 ; iv < nvbox ; iv++ ){
        if( vbox[iv].code == VBOX ){
          set_color_memplot( vbox[iv].rr , vbox[iv].gg , vbox[iv].bb ) ;
          set_thick_memplot( 0 ) ;
          xb1 = vbox[iv].x1 ; xb2 = vbox[iv].x2 ;
          yb1 = ybot        ; yb2 = ytop        ;
          zzphys_(&xb1,&yb1); zzphys_(&xb2,&yb2);
          plotfrect_memplot( xb1,yb1 , xb2,yb2 ) ;
        } else if( vbox[iv].code == RBOX ){  /* 15 Jun 2017 */
          float xxb1,yyb1,xxb2,yyb2 ;
          xb1 = vbox[iv].x1    ; xb2 = vbox[iv].x2    ;
          yb1 = vbox[iv].rb_y1 ; yb2 = vbox[iv].rb_y2 ;
          yb3 = vbox[iv].rb_y3 ;  /* 05 Sep 2017 */

          set_color_memplot( vbox[iv].rr , vbox[iv].gg , vbox[iv].bb ) ;
          xxb1 = xb1 ; xxb2 = xb2 ; yyb1 = yb1 ; yyb2 = yb2 ;
          zzphys_(&xxb1,&yyb1); zzphys_(&xxb2,&yyb2);
          set_thick_memplot( 0.0f ) ;
          plotfrect_memplot( xxb1,yyb1 , xxb2,yyb2 ) ;

          set_color_memplot( vbox[iv].rb_rr ,
                             vbox[iv].rb_gg , vbox[iv].rb_bb ) ;
          set_thick_memplot( 0.0f ) ;
          plotpak_line( xb1,yb1 , xb1,yb2 ) ;
          plotpak_line( xb1,yb2 , xb2,yb2 ) ;
          plotpak_line( xb2,yb2 , xb2,yb1 ) ;
          plotpak_line( xb2,yb1 , xb1,yb1 ) ;
          if( (yb3-yb1)*(yb3-yb2) < 0.0f ){      /* 05 Sep 2017 */
            set_thick_memplot( 0.004f ) ;
            plotpak_line( xb1,yb3 , xb2,yb3 ) ;
          }
        } else if( vbox[iv].code == TLIN ){  /* 19 Sep 2017 */
          int dc ;
          xb1 = vbox[iv].x1    ; xb2 = vbox[iv].x2    ;
          yb1 = vbox[iv].rb_y1 ; yb2 = vbox[iv].rb_y2 ;

          set_color_memplot( vbox[iv].rr , vbox[iv].gg , vbox[iv].bb ) ;
          set_thick_memplot( 0.004f ) ;
          dc = vbox[iv].dcode ; if( dc <= 0 ) dc = 1 ; else if( dc > 5 ) dc = 5 ;
          plotpak_setlin(dc) ;
          plotpak_line( xb1,yb1 , xb2,yb2 ) ;
          if( dc != 1 ) plotpak_setlin(1) ; /* solid */
        }
      }

      if( do_perim ){
        set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
        set_thick_memplot( thik ) ;
        plotpak_perimm( nnax,mmax , nnay,mmay , ilab[(nnax>0)+2*(nnay>0)] ) ;
      }

      /* plot data */

      for( jj=0 ; jj < ny ; jj++ ){
         thth = THIK ;
         if( jj >= thik_n1 && jj <= thik_n2 ) thth = thik_12 ;
         set_thick_memplot( thth ) ;
         set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;

         if( nsepx > 0 ){
           int qq = jj ; if( qq >= nsepx ) qq = nsepx-1 ;
           xx = sepx[qq] ; ixtop = lsepx[qq] ; if( ixtop > nx ) ixtop = nx ;
         } else {
           ixtop = nx ;
         }
         yy = y[jj] ;
         if( !noline ){
           if( use_ddd ) plotpak_setlin(ddd[jj%NCLR]) ;
           if( dohist ){
             float xp , yb=ybot ;
             plotpak_line( xx[0],yb , xx[0],yy[0] ) ;
             for( ii=1 ; ii < ixtop ; ii++ ){
               plotpak_line( xx[ii-1],yy[ii-1],xx[ii],yy[ii-1]) ;
               plotpak_line( xx[ii]  ,yy[ii-1],xx[ii],yy[ii]  ) ;
             }
             xp = xx[ixtop-1] + (xx[ixtop-1]-xx[ixtop-2]) ;
             plotpak_line( xx[ixtop-1],yy[ixtop-1] , xp,yy[ixtop-1] ) ;
             plotpak_line( xp         ,yy[ixtop-1] , xp,yb          ) ;
           } else {
             for( ii=1 ; ii < ixtop ; ii++ ){
#if 0
               if( fabsf(xx[ii-1]) < WAY_BIG && fabsf(xx[ii]) < WAY_BIG &&
                   fabsf(yy[ii-1]) < WAY_BIG && fabsf(yy[ii]) < WAY_BIG   )
#else
               if( xx[ii-1] <= xtop && xx[ii] <= xtop &&
                   yy[ii-1] <= ytop && yy[ii] <= ytop &&
                   xx[ii-1] >= xbot && xx[ii] >= xbot &&
                   yy[ii-1] >= ybot && yy[ii] >= ybot   )
#endif
               plotpak_line( xx[ii-1] , yy[ii-1] , xx[ii] , yy[ii] ) ;
             }
           }
           if( use_ddd ) plotpak_setlin(1) ;
         }

         if( tsbox > 0.0f ){
           if( !do_sbox) set_thick_memplot( thik ) ;
           for( ii=0 ; ii < ixtop ; ii++ ){
             if( noline != 2 ||
                 ( xx[ii] >= xbot && xx[ii] <= xtop &&
                   yy[ii] >= ybot && yy[ii] <= ytop   ) ){
               if( do_sbox ) ADDTO_SBOX (xx[ii],yy[ii],jj) ;
               else          plot_onebox(xx[ii],yy[ii],jj) ;
             }
           }
           set_thick_memplot( THIK ) ;
         }
      } /* end of loop over different curve to plot */

      if( do_sbox && nsbox > 0 ){  /* 24 Oct 2013 */
        int qq , ss , ds ;
        set_thick_memplot( thik ) ;
        ds = (4*nsbox)/5 ;
        for( qq=0 ; qq < ds ; qq++ ){
          ss = lrand48() % nsbox ; jj = jsbox[ss] ; if( jj < 0 ) continue ;
          set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;
          plot_onebox(xsbox[ss],ysbox[ss],jj) ; jsbox[ss] = -1 ;
        }
        ds = ts_find_relprime(nsbox) ;
        for( qq=0,ss=nsbox/7 ; qq < nsbox ; qq++,ss=(ss+ds)%nsbox ){
          jj = jsbox[ss] ; if( jj < 0 ) continue ;
          set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;
          plot_onebox(xsbox[ss],ysbox[ss],jj) ; jsbox[ss] = -1 ;
        }
        free(jsbox) ; free(ysbox) ; free(xsbox) ;
      }

      set_thick_memplot( thik ) ;
      set_color_memplot( 0.0 , 0.0 , 0.0 ) ;

   } else {  /*-- plot each on separate vertical scale --*/

      float dyo = (yotop-yobot) / ( (1.0+SY) * ny - SY ) ;

      /* name labels at right? */

      if( nam_yyy != NULL ){
         float yv = yotop ; int sz,yz ;

         for( jj=0 ; jj < ny ; jj++ ){
            yll = yobot + jj*(1.0+SY)*dyo ; yhh = yll + dyo ;
            if( STGOOD(nam_yyy[jj]) ){
               set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;
               set_thick_memplot( 1.234f*THIK ) ;
               yv = 0.7*yhh + 0.3*yll ;
               if( use_ddd ) plotpak_setlin(ddd[jj%NCLR]) ;
               if( xflip )
                 plotpak_line( xobot-0.008 , yv , xobot-0.041 , yv ) ;
               else
                 plotpak_line( xotop+0.008 , yv , xotop+0.041 , yv ) ;
               if( use_ddd ) plotpak_setlin(1) ;
               if( tsbox > 0.0f ){
                 set_thick_memplot(thik);
                 if( xflip )
                   { plot_onebox(xobot-0.008,yv,jj ); plot_onebox(xobot-0.041,yv,jj ); }
                 else
                   { plot_onebox(xotop+0.008,yv,jj ); plot_onebox(xotop+0.041,yv,jj ); }
               }
               set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
               yz = strlen(nam_yyy[jj]) ;
                    if( yz <  9 ) sz = 11 ;
               else if( yz < 11 ) sz =  9 ;
               else if( yz < 13 ) sz =  8 ;
               else if( yz < 15 ) sz =  7 ;
               else               sz =  6 ;
               set_thick_memplot( thik*sz/13.9f ) ;
               if( xflip )
                 plotpak_pwritf( xobot-0.049 , yv , nam_yyy[jj] , sz , 0 ,  1 ) ;
               else
                 plotpak_pwritf( xotop+0.049 , yv , nam_yyy[jj] , sz , 0 , -1 ) ;
            }
         }
      }

      /* data each in its own box */

           if( ny > 19 ) plotpak_labmod( 0 , 5 ) ;  /* 09 Dec 2010 */
      else if( ny >  9 ) plotpak_labmod( 0 , 7 ) ;

      for( jj=ny-1 ; jj >= 0 ; jj-- ){
         yll = yobot + jj*(1.0+SY)*dyo ; yhh = yll + dyo ;
         floatfix(ylo[jj]) ; floatfix(yhi[jj]) ;
         plotpak_set( ASX(xobot),ASX(xotop) , yll,yhh ,
                      xabot,xatop , ylo[jj],yhi[jj] , 1 ) ;

         /* 24 Apr 2012: add vbox stuff now, before other plotting */

         set_thick_memplot(0.0f) ;
         for( iv=0 ; iv < nvbox ; iv++ ){
           if( vbox[iv].ygr == jj || vbox[iv].ygr < 0 ){
             if( vbox[iv].code == VBOX ){
               set_color_memplot( vbox[iv].rr , vbox[iv].gg , vbox[iv].bb ) ;
               xb1 = vbox[iv].x1 ; xb2 = vbox[iv].x2 ;
               yb1 = ylo[jj]     ; yb2 = yhi[jj]     ;
               zzphys_(&xb1,&yb1); zzphys_(&xb2,&yb2);
               plotfrect_memplot( xb1,yb1 , xb2,yb2 );
             }
           }
         }

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

         if( do_perim ){
           set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
           plotpak_perimm( nnax,mmax , nnay,mmay , ilab[(nnax>0)*(jj==0)+2*(nnay>0)] ) ;
         }
         if( ylo[jj] < 0.0 && yhi[jj] > 0.0 ){
           set_thick_memplot( 0.0 ) ;
           plotpak_setlin(3) ;
           plotpak_line( xbot,0.0 , xtop,0.0 ) ;
           plotpak_setlin(1) ;
         }

         set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;
         thth = THIK ;
         if( jj >= thik_n1 && jj <= thik_n2 ) thth = thik_12 ;
         set_thick_memplot( thth ) ;

         if( nsepx > 0 ){
           int qq = jj ; if( qq >= nsepx ) qq = nsepx-1 ;
           xx = sepx[qq] ; ixtop = lsepx[qq] ; if( ixtop > nx ) ixtop = nx ;
         } else {
           ixtop = nx ;
         }
         yy = y[jj] ;
         if( !noline ){
           if( use_ddd ) plotpak_setlin(ddd[jj%NCLR]) ;
           if( dohist ){
             float xp , yb=ylo[jj] ;
             plotpak_line( xx[0],yb , xx[0],yy[0] ) ;
             for( ii=1 ; ii < ixtop ; ii++ ){
               plotpak_line( xx[ii-1],yy[ii-1],xx[ii],yy[ii-1]) ;
               plotpak_line( xx[ii]  ,yy[ii-1],xx[ii],yy[ii]  ) ;
             }
             xp = xx[ixtop-1] + (xx[ixtop-1]-xx[ixtop-2]) ;
             plotpak_line( xx[ixtop-1],yy[ixtop-1] , xp,yy[ixtop-1] ) ;
             plotpak_line( xp         ,yy[ixtop-1] , xp,yb          ) ;
           } else {
             for( ii=1 ; ii < ixtop ; ii++ ){
#if 0
                if( fabsf(xx[ii-1]) < WAY_BIG && fabsf(xx[ii]) < WAY_BIG &&
                    fabsf(yy[ii-1]) < WAY_BIG && fabsf(yy[ii]) < WAY_BIG   )
#else
               if( xx[ii-1] <= xtop    && xx[ii] <= xtop    &&
                   yy[ii-1] <= yhi[jj] && yy[ii] <= yhi[jj] &&
                   xx[ii-1] >= xbot    && xx[ii] >= xbot    &&
                   yy[ii-1] >= ylo[jj] && yy[ii] >= ylo[jj]   )
#endif
                  plotpak_line( xx[ii-1] , yy[ii-1] , xx[ii] , yy[ii] ) ;
             }
           }
           set_thick_memplot( thik ) ;
           if( use_ddd ) plotpak_setlin(1) ;
         }

         if( tsbox > 0.0f ){
           for( ii=0 ; ii < ixtop ; ii++ ){
             if( noline != 2 ||
                 ( xx[ii] >= xbot    && xx[ii] <= xtop    &&
                   yy[ii] >= ylo[jj] && yy[ii] <= yhi[jj]    ) )
               plot_onebox( xx[ii] , yy[ii] , jj ) ;
           }
         }

         set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
      }
   }

   if( STGOOD(lab_top) && do_naked ){
     plotpak_pwritf_phys( xobot+0.05 , yotop+0.02 , lab_top , 18 , 0 , -1 ) ;
   }

   /*-- exit, stage left --*/

   set_thick_memplot( 0.0 ) ;
   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   plot_ts_clear_vbox() ;

   if( nsepx == 0 && xx != x ) free(xx) ;
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

   if (nx < 0 ){ ymask = ymask | TSP_SEPARATE_YSCALE; nx = -nx; }
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

   if( do_naked ){  /* 03 May 2018 */
     lab_xxx = lab_yyy = NULL ; nam_yyy = NULL ;
   }
   if( do_naked > 1 ){
     lab_top = NULL ;
   }

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

   create_memplot_surely( "Tsplot" , aspect ) ;
   set_thick_memplot( thik*1.5f ) ;

   /*-- plot labels, if any --*/

   xobot = 0.15 ; xotop = 1.27 ;  /* set objective size of plot */
   yobot = 0.1  ; yotop = 0.95 ;

   if( STGOOD(lab_top) ){ yotop -= 0.02 ; yobot -= 0.01 ; }
   if( nam_yyy != NULL ){ xotop -= 0.16 ; xobot -= 0.02 ; }

   if( do_naked ) /* 03 May 2018 */{
     xobot = yobot = 0.0f ; xotop = 1.3f ;
     yotop = (STGOOD(lab_top)) ? 0.89 : 0.98 ;
   }

   /* x-axis label? */

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   if( STGOOD(lab_xxx) )
      plotpak_pwritf( 0.5*(xobot+xotop) , yobot-0.06 , lab_xxx , 16 , 0 , 0 ) ;

   /* y-axis label? */

   if( STGOOD(lab_yyy) )
      plotpak_pwritf( xobot-0.10 , 0.5*(yobot+yotop) , lab_yyy , 16 , 90 , 0 ) ;

   /* label at top? */

   if( STGOOD(lab_top) && !do_naked ){
     plotpak_pwritf( xobot , yotop+0.01 , lab_top , 18 , 0 , -2 ) ;
   }

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
               set_thick_memplot( 1.234f*THIK ) ;
               plotpak_line( xotop+0.008 , yv , xotop+0.042 , yv ) ;
               set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
               sz = (strlen(nam_yyy[jj]) <= 10) ? 12 : 9 ;
               set_thick_memplot( thik*sz/13.9f ) ;
               plotpak_pwritf( xotop+0.048 , yv , nam_yyy[jj] , sz , 0 , -1 ) ;
               yv -= 0.05 ;
            }
         }
      }

      /* plot axes */

      floatfix(ybot) ; floatfix(ytop) ;
      plotpak_set( ASX(xobot),ASX(xotop) , yobot,yotop ,
                   xbot,xtop , ybot,ytop , 1 ) ;
      if( do_perim ){
        set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
        plotpak_perimm( nnax,mmax , nnay,mmay , ilab[(nnax>0)+2*(nnay>0)] ) ;
      }

   } else {  /*-- plot each on separate vertical scale --*/

      float dyo = (yotop-yobot) / ( (1.0+SY) * ny - SY ) ;

      /* name labels at right? */

      if( nam_yyy != NULL ){
         float yv = yotop ; int sz ;

         for( jj=0 ; jj < ny ; jj++ ){
            yll = yobot + jj*(1.0+SY)*dyo ; yhh = yll + dyo ;
            if( STGOOD(nam_yyy[jj]) ){
               set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;
               set_thick_memplot( 1.234f*THIK ) ;
               yv = 0.7*yhh + 0.3*yll ;
               plotpak_line( xotop+0.008 , yv , xotop+0.042 , yv ) ;
               set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
               sz = (strlen(nam_yyy[jj]) <= 10) ? 12 : 9 ;
               set_thick_memplot( thik*sz/13.9f ) ;
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
         floatfix(ybot) ; floatfix(ytop) ;
         plotpak_set( ASX(xobot),ASX(xotop) , yll,yhh ,
                      xbot,xtop , ybot,ytop , 1 ) ;
         if( do_perim ){
           set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
           plotpak_perimm( nnax,mmax , nnay,mmay , ilab[(nnax>0)*(jj==0)+2*(nnay>0)] ) ;
         }
         if( ybot < 0.0 && ytop > 0.0 ){
            plotpak_setlin(5) ;
            plotpak_line( xbot,0.0 , xtop,0.0 ) ;
            plotpak_setlin(1) ;
         }
      }
   }

   if( STGOOD(lab_top) && do_naked ){
     plotpak_pwritf_phys( xobot+0.05 , yotop+0.02 , lab_top , 18 , 0 , -1 ) ;
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

      floatfix(ybot) ; floatfix(ytop) ;
      plotpak_set( ASX(xobot),ASX(xotop) , yobot,yotop ,
                   xbot,xtop , ybot,ytop , 1 ) ;
      set_thick_memplot( THIK ) ;

      /* plot data */

      for( jj=0 ; jj < ny ; jj++ ){
         set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;

         yy = y[jj] ;
         for( ii=1 ; ii < nx ; ii++ ){
            if( fabsf(xx[ii-1]) < WAY_BIG && fabsf(xx[ii]) < WAY_BIG &&
                fabsf(yy[ii-1]) < WAY_BIG && fabsf(yy[ii]) < WAY_BIG   )

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
         floatfix(ybot) ; floatfix(ytop) ;
         plotpak_set( ASX(xobot),ASX(xotop) , yll,yhh ,
                      xbot,xtop , ybot,ytop , 1 ) ;
         set_color_memplot( ccc[jj%NCLR][0] , ccc[jj%NCLR][1] , ccc[jj%NCLR][2] ) ;

         yy = y[jj] ;
         for( ii=1 ; ii < nx ; ii++ ){
            if( fabsf(xx[ii-1]) < WAY_BIG && fabsf(xx[ii]) < WAY_BIG &&
                fabsf(yy[ii-1]) < WAY_BIG && fabsf(yy[ii]) < WAY_BIG   )

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

/*-----------------------------------------------------------------------
  Plot a timeseries with error bars, into an in-memory plot structure.
  If array x[] is NULL, then routine will make an x-axis up.
-------------------------------------------------------------------------*/

MEM_plotdata * plot_ts_ebar( int nx , float *x , float *y , float *ey ,
                             char *lab_xxx , char *lab_yyy , char *lab_top )
{
   int ii , jj , np , nnax,nnay , mmax,mmay ;
   float *xx ;
   float xbot,xtop , ybot,ytop , pbot,ptop , xobot,xotop,yobot,yotop ;
   char str[32] ;
   MEM_plotdata *mp ;
   float ymm , ypp , xmm,xpp,xdd ;

   /*-- sanity check --*/

   if( nx <= 1 || y == NULL || ey == NULL ) return NULL ;

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
         if( xx[ii] < xbot && fabsf(xx[ii]) < WAY_BIG ) xbot = xx[ii] ;
         if( xx[ii] > xtop && fabsf(xx[ii]) < WAY_BIG ) xtop = xx[ii] ;
      }
      if( xbot >= xtop ) return NULL ;
   }

   /*-- push range of x outwards --*/

   pbot = p10(xbot) ; ptop = p10(xtop) ; if( ptop < pbot ) ptop = pbot ;
   if( nnaxx >= 0 ){
     nnax = nnaxx ;
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

   ybot = WAY_BIG ; ytop = -WAY_BIG ;
   for( ii=0 ; ii < nx ; ii++ ){
     if( y[ii] < ybot ) ybot = y[ii] ;
     if( y[ii] > ytop ) ytop = y[ii] ;
   }
   floatfix(ybot) ; floatfix(ytop) ;
   if( ybot >= ytop ){                       /* shouldn't happen */
      ytop = ybot + 0.05f*fabsf(ybot) + 0.1f ;
      ybot = ybot - 0.05f*fabsf(ybot) - 0.1f ;
   }

   /*-- push range of y outwards --*/

   pbot = p10(ybot) ; ptop = p10(ytop) ; if( ptop < pbot ) ptop = pbot ;
   if( nnayy >= 0 ){
     nnay = nnayy ;
     mmay = mmayy ;
     ybot = yybot ;
     ytop = yytop ;
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

   if( nnayy < 0 ){
     pbot = p10(ybot) ; ptop = p10(ytop) ; if( ptop < pbot ) ptop = pbot ;
     if( ptop != 0.0 && ypush > 0 ){
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
     } else if( ypush == 0 ){
       float dif=(ytop-ybot)*0.005f ;
       ybot -= dif ; ytop += dif ;
     }
   }

   nnaxx = nnayy = -1 ;

   /*-- setup to plot --*/

   create_memplot_surely( "tsplot" , aspect ) ;
   set_thick_memplot( thik ) ;  /* for labels */

   /*-- plot labels, if any --*/

   xobot = 0.15 ; xotop = 1.27 ;  /* set objective size of plot */
   yobot = 0.1  ; yotop = 0.95 ;

   if( STGOOD(lab_top) ){ yotop -= 0.02 ; yobot -= 0.01 ; }

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

   /*-- start plotting --*/

   /* plot axes */

   floatfix(ybot) ; floatfix(ytop) ;
   plotpak_set( ASX(xobot),ASX(xotop) , yobot,yotop ,
                xbot,xtop , ybot,ytop , 1 ) ;
   if( do_perim ){
     set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
     set_thick_memplot( thik ) ;
     plotpak_perimm( nnax,mmax , nnay,mmay , ilab[(nnax>0)+2*(nnay>0)] ) ;
   }

   xdd = (xtop-xbot)*0.00333f ;

   /* plot data */

   set_thick_memplot( THIK ) ;
   set_color_memplot( ccc[0][0] , ccc[0][1] , ccc[0][2] ) ;

   for( ii=1 ; ii < nx ; ii++ ){
     if( fabsf(xx[ii-1]) < WAY_BIG && fabsf(xx[ii]) < WAY_BIG &&
         fabsf(y [ii-1]) < WAY_BIG && fabsf(y [ii]) < WAY_BIG   )
       plotpak_line( xx[ii-1] , y[ii-1] , xx[ii] , y[ii] ) ;
   }

   set_thick_memplot( 0.0 ) ;
   set_color_memplot( ccc[1][0] , ccc[1][1] , ccc[1][2] ) ;
   for( ii=0 ; ii < nx ; ii++ ){
     if( fabsf(xx[ii]) < WAY_BIG && fabsf(y [ii]) < WAY_BIG &&
         fabsf(ey[ii]) < WAY_BIG &&       ey[ii] != 0.0       ){

       ymm = y[ii] - ey[ii] ; ypp = y[ii] + ey[ii] ;
       xmm = xx[ii] - xdd   ; xpp = xx[ii] + xdd ;

       plotpak_line( xmm,ymm , xpp,ymm ) ;
       plotpak_line( xmm,ypp , xpp,ypp ) ;
       if( nx <= 333 )
         plotpak_line( xx[ii],ymm , xx[ii],ypp ) ;
     }
   }

   /*-- exit, stage left --*/

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   if( xx != x ) free(xx) ;
   mp = get_active_memplot() ;
   return mp ;
}

/*-----------------------------------------------------------------------*/

void plot_ts_ebar_win( Display *dpy ,
                       int nx , float *x , float *y , float *ey ,
                       char *lab_xxx , char *lab_yyy , char *lab_top ,
                       void_func *killfunc )
{
   MEM_plotdata *mp ;
   mp = plot_ts_ebar( nx,x,y,ey,lab_xxx,lab_yyy,lab_top ) ;
   if( mp != NULL )
     (void) memplot_to_topshell( dpy , mp , killfunc ) ;
}
