#define MAIN_COXPLOT_FILE
#include "coxplot.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

static int             num_plotar  = 0 ;
static MEM_plotdata ** plotar      = NULL ;
static int             active_plot = -1 ;

static float           active_color = (float) RGB_TO_COL(1.0,1.0,1.0) ;
static float           active_thick = 0.0 ;

static float           active_opacity = 1.0 ;   /* 22 Jul 2004 */

#define STATUS(str) fprintf(stderr,"** " str "\n")

/*------------------------------------------------------------------------
   Function to return a pointer to an in-memory plot with the given
   ident.  A NULL return value indicates failure.
   A NULL input string will return the current "active" plot.
--------------------------------------------------------------------------*/

MEM_plotdata * find_memplot( char * id )
{
   int ip ;

   if( num_plotar == 0 || plotar == NULL ) return NULL ;

   if( id == NULL || id[0] == '\0' ){
      if( active_plot < 0 || active_plot >= num_plotar ) return NULL ;
      return plotar[active_plot] ;
   }

   for( ip=0 ; ip < num_plotar ; ip++ )
      if( strcmp(plotar[ip]->ident,id) == 0 ) return plotar[ip] ;

   return NULL ;
}

/*------------------------------------------------------------------------
  Function to create an in-memory plot with the given ident and aspect
  ratio (length of x / length of y).  Also sets the "active" plot to be
  this new one.  Nonzero return value indicates an error.
--------------------------------------------------------------------------*/

int create_memplot( char * id , float aspect )
{
   MEM_plotdata * pd ;
   static int plotpak_framed = 0 ;
   real asp ;

   if( find_memplot(id) != NULL ) return 1 ;

   INIT_MEMPLOT(pd,id) ;

   if( plotar == NULL ){
      plotar     = (MEM_plotdata **) malloc( sizeof(MEM_plotdata *) ) ;
      num_plotar = 0 ;
   } else {
      plotar = (MEM_plotdata **)
                  realloc( plotar , sizeof(MEM_plotdata *)*(num_plotar+1) ) ;
   }

   active_plot = num_plotar ;
   plotar[num_plotar++] = pd ;

   ADDTO_MEMPLOT( pd , 1.0,0.0,0.0,0.0 , 0.0 , -THCODE_OPAC ) ;  /* 22 Jul 2004 */

   if( aspect <= 0.0 ) aspect = 1.3 ;
   asp        = aspect ;
   pd->aspect = aspect ;
   memplt_( &asp ) ;                  /* setup PLOTPAK */

   return 0 ;
}

/*------------------------------------------------------------------
   20 Sep 2001: make a plot, fer shur
--------------------------------------------------------------------*/

int create_memplot_surely( char *id , float aspect )
{
   int ii , jj ;
   char str[256] ;

   if( aspect <= 0.0 ) aspect = 1.0 ;  /* backup for stupid users */

   if( id != NULL && id[0] != '\0'){
      ii = create_memplot(id,aspect) ;
      if( ii == 0 ) return 0 ;
   } else {
      id = "ElvisWalksTheEarth" ;
   }

   for( jj=0 ; ; jj++ ){
      sprintf(str,"%.240s_%d",id,jj) ;
      ii = create_memplot(str,aspect) ;
      if( ii == 0 ) return 0 ;
   }

   return 1 ; /* actually, unreachable */
}

/*-------------------------------------------------------------------------
   Function to set the "active" plot to a given ident.
   A nonzero return value indicates an error.
---------------------------------------------------------------------------*/

int set_active_memplot( char * id )
{
   int ip ;

   if( id == NULL || id[0] == '\0' || num_plotar == 0 || plotar == NULL )
      return 1 ;

   for( ip=0 ; ip < num_plotar ; ip++ )
      if( strcmp(plotar[ip]->ident,id) == 0 ){
         real asp = plotar[ip]->aspect ;
         active_plot = ip ;
         memplt_( &asp ) ;    /* re-setup PLOTPAK */
         return 0 ;
      }

   return 1 ;
}

MEM_plotdata * get_active_memplot(void)
{
   return find_memplot(NULL) ;
}

int nline_active_memplot(void)
{
   MEM_plotdata * mp ;
   mp = find_memplot(NULL) ;
   if( mp == NULL ) return 0 ;
   return MEMPLOT_NLINE(mp) ;
}

/*-------------------------------------------------------------------------
  Functions to set line thickness and color.
  Color is given as an RGB triple from [0,1] x [0,1] x [0,1].
  Thickness is given in the same units as coordinates; zero means thin.
---------------------------------------------------------------------------*/

void set_color_memplot( float r , float g , float b )
{
   if( r > 1.0 || g > 1.0 || b > 1.0 ){        /* 22 Mar 2002:     */
      r /= 255.0 ; g /= 255.0 ; b /= 255.0 ;   /* allow for 0..255 */
   }
   if( r < 0.0 ) r = 0.0 ; else if ( r > 1.0 ) r = 1.0 ;
   if( g < 0.0 ) g = 0.0 ; else if ( g > 1.0 ) g = 1.0 ;
   if( b < 0.0 ) b = 0.0 ; else if ( b > 1.0 ) b = 1.0 ;

   active_color = (float) RGB_TO_COL(r,g,b) ;
   return ;
}

/*----- This routine is called from color.f -----*/

void zzmpco_( float * r , float * g , float * b )
{
   set_color_memplot( *r , *g , *b ) ;
   return ;
}

void set_thick_memplot( float th )
{
   if( th < 0.0 ) th = 0.0 ;
   active_thick = th ;
   return ;
}

float get_thick_memplot( void )
{
   return active_thick ;
}

void set_opacity_memplot( float th )  /* 22 Jul 2004 */
{
   MEM_plotdata *mp ;

        if( th < 0.0 ) th = 0.0 ;
   else if( th > 1.0 ) th = 1.0 ;
   active_opacity = th ;

   /* Set opacity for further drawing [22 Jul 2004] */

   if( active_plot < 0 || active_plot >= num_plotar ||
       num_plotar == 0 || plotar == NULL            ||
       plotar[active_plot] == NULL                    ) return ;

   mp = plotar[active_plot] ;
   ADDTO_MEMPLOT( mp , th,0.0,0.0,0.0 , 0.0 , -THCODE_OPAC ) ;
   return ;
}

float get_opacity_memplot( void )
{
   return active_opacity ;
}

/*------------------------------------------------------------------
  where the actual plotting into the memplot is done from
  the coxplot functions
--------------------------------------------------------------------*/

void plotline_memplot( float x1 , float y1 , float x2 , float y2 )
{
   MEM_plotdata * mp ;

   if( active_plot < 0 || active_plot >= num_plotar ||
       num_plotar == 0 || plotar == NULL            ||
       plotar[active_plot] == NULL                    ) return ;

   mp = plotar[active_plot] ;

#if 0
fprintf(stderr,"** plotline_memplot %d: (%f,%f) to (%f,%f)\n",
        MEMPLOT_NLINE(mp) , x1,y1,x2,y2) ;
#endif

   ADDTO_MEMPLOT( mp , x1,y1,x2,y2 , active_color , active_thick ) ;
   return ;
}

void plotrect_memplot( float x1 , float y1 , float x2 , float y2 ) /* 21 Mar 2001 */
{
   MEM_plotdata * mp ;

   if( active_plot < 0 || active_plot >= num_plotar ||
       num_plotar == 0 || plotar == NULL            ||
       plotar[active_plot] == NULL                    ) return ;

   mp = plotar[active_plot] ;

   ADDTO_MEMPLOT( mp , x1,y1,x2,y2 , active_color , -THCODE_RECT ) ;
   return ;
}

void plotcirc_memplot( float x1 , float y1 , float rad ) /* 10 Mar 2002 */
{
   MEM_plotdata * mp ;

   if( active_plot < 0 || active_plot >= num_plotar ||
       num_plotar == 0 || plotar == NULL            ||
       plotar[active_plot] == NULL                    ) return ;

   mp = plotar[active_plot] ;

   ADDTO_MEMPLOT( mp , x1,y1,rad,0.0 , active_color , -THCODE_CIRC ) ;
   return ;
}

/*----- This routine is called from zzphph.f to draw 1 actual line -----*/

void zzmpli_( float * x1 , float * y1 , float * x2 , float * y2 )
{
   plotline_memplot( *x1 , *y1 , *x2 , *y2 ) ;
   return ;
}

/*------------------------------------------------------------------------
   Delete the active in-memory plot.
   After this, there is no "active" plot.
--------------------------------------------------------------------------*/

void delete_active_memplot(void)
{
   int ip ;

   if( active_plot < 0 || active_plot >= num_plotar ||
       num_plotar == 0 || plotar == NULL            ||
       plotar[active_plot] == NULL                    ) return ;

   DESTROY_MEMPLOT( plotar[active_plot] ) ;

   if( num_plotar == 1 ){
      free(plotar) ; plotar = NULL ; num_plotar = 0 ;
   } else {
      for( ip=active_plot+1 ; ip < num_plotar ; ip++ ) plotar[ip-1] = plotar[ip] ;
      num_plotar-- ; plotar[num_plotar] = NULL ;
   }

   active_plot = -1 ;
   return ;
}

/*------------------------------------------------------------------------*/

void delete_memplot( MEM_plotdata * mp )
{
   int ip ;

   if( num_plotar == 0 || plotar == NULL || mp == NULL ) return ;

   for( ip=0 ; ip < num_plotar ; ip++ ) if( plotar[ip] == mp ) break ;

   if( ip < num_plotar ){
           if( active_plot == ip ) active_plot = -1 ;
      else if( active_plot >  ip ) active_plot-- ;

      for( ip++ ; ip < num_plotar ; ip++ ) plotar[ip-1] = plotar[ip] ;

      num_plotar-- ; plotar[num_plotar] = NULL ;
   }

   DESTROY_MEMPLOT( mp ) ;
   return ;
}

/*-----------------------------------------------------------------------
   Scale data inside an memplot -- 26 Feb 2001
      x_new     = sx * x_old + tx
      y_new     = sy * y_old + ty
      thick_new = st * thick_old
-------------------------------------------------------------------------*/

void scale_memplot( float sx , float tx ,
                    float sy , float ty , float st , MEM_plotdata * mp )
{
   int ii,nn ;
   if( mp == NULL ) return ;

   for( nn=ii=0 ; ii < mp->nxyline ; ii++ ){
      mp->xyline[nn] = mp->xyline[nn] * sx + tx ; nn++ ; /* x1 */
      mp->xyline[nn] = mp->xyline[nn] * sy + ty ; nn++ ; /* y1 */
      mp->xyline[nn] = mp->xyline[nn] * sx + tx ; nn++ ; /* x2 */
      mp->xyline[nn] = mp->xyline[nn] * sy + ty ; nn++ ; /* y2 */
                                                  nn++ ; /* color */
      if( mp->xyline[nn] > 0.0 )
        mp->xyline[nn] = mp->xyline[nn] * st    ; nn++ ; /* thick */
   }
   return ;
}

/*-----------------------------------------------------------------------
   Append data from one memplot to another -- 26 Feb 2001
-------------------------------------------------------------------------*/

void append_to_memplot( MEM_plotdata * mp , MEM_plotdata * ap )
{
   int nn , nold ;
   if( mp == NULL || ap == NULL || ap->nxyline <= 0 ) return ;

   nn = mp->nxyline + ap->nxyline ;
   mp->xyline = (float *) realloc(mp->xyline,
                                  sizeof(float)*NXY_MEMPLOT*nn) ;

   memcpy( mp->xyline + NXY_MEMPLOT*mp->nxyline ,
           ap->xyline , sizeof(float)*NXY_MEMPLOT*ap->nxyline ) ;

   mp->nxyline = mp->nxyline_all = nn ;
   return ;
}

/*-----------------------------------------------------------------------
   Make a copy of a memplot; the new one will be the active memplot
   -- 26 Feb 2001 -- RWCox
-------------------------------------------------------------------------*/

MEM_plotdata * copy_memplot( MEM_plotdata * mp )
{
   MEM_plotdata * np ;
   char str[256] ; int nn ;

   if( mp == NULL ) return NULL ;

   /* make a new ID string */

   for( nn=1 ; nn <= 9999 ; nn++ ){
      sprintf(str,"%.240sCopy%04d",mp->ident,nn) ;
      if( find_memplot(str) == NULL ) break ;
   }
   if( nn == 1000 ) return NULL ; /* this is bad (but unlikely) */

   /* make the new memplot */

   nn = create_memplot( str , mp->aspect ) ;
   if( nn ) return NULL ;         /* this is real bad */

   np = find_memplot(NULL) ;      /* is the new one */
   if( np == NULL ) return NULL ; /* shouldn't happen */

   /* copy data from old one into new one */

   nn = np->nxyline = np->nxyline_all = mp->nxyline ;
   np->xyline = (float *) realloc(np->xyline,
                                  sizeof(float)*NXY_MEMPLOT*nn) ;
   memcpy( np->xyline , mp->xyline , sizeof(float)*NXY_MEMPLOT*nn ) ;

   return np ;
}

/*----------------------------------------------------------------------
   Flip a memplot inplace - 30 Aug 2001 - RWCox
     rot    = one of the MRI_ROT_ codes (see coxplot.h)
     mirror = whether to left-right mirror after rotation
------------------------------------------------------------------------*/

void flip_memplot( int rot , int mirror , MEM_plotdata *mp )
{
   int fopt , ii,nn ;
   float xtop , ytop=1.0 , x1,y1,x2,y2 ;

   if( mp == NULL ) return ;                          /* nothing in */
   if( rot == MRI_ROT_0 && mirror == FALSE ) return ; /* do nothing */

   xtop = mp->aspect ;

   fopt = (mirror) ? (rot+MRI_FLMADD) : (rot) ;
   switch( fopt ){

      default: return ;  /* should never happen */

      case MRI_ROT_90:
       for( nn=ii=0 ; ii < mp->nxyline ; ii++,nn+=NXY_MEMPLOT ){
          x1 = mp->xyline[nn  ] ; y1 = mp->xyline[nn+1] ;
          x2 = mp->xyline[nn+2] ; y2 = mp->xyline[nn+3] ;
          mp->xyline[nn  ] = ytop - y1 ;
          mp->xyline[nn+1] = x1 ;
          mp->xyline[nn+2] = ytop - y2 ;
          mp->xyline[nn+3] = x2 ;
       }
      break ;

      case MRI_ROT_180:
       for( nn=ii=0 ; ii < mp->nxyline ; ii++,nn+=NXY_MEMPLOT ){
          x1 = mp->xyline[nn  ] ; y1 = mp->xyline[nn+1] ;
          x2 = mp->xyline[nn+2] ; y2 = mp->xyline[nn+3] ;
          mp->xyline[nn  ] = xtop - x1 ;
          mp->xyline[nn+1] = ytop - y1 ;
          mp->xyline[nn+2] = xtop - x2 ;
          mp->xyline[nn+3] = ytop - y2 ;
       }
      break ;

      case MRI_ROT_270:
       for( nn=ii=0 ; ii < mp->nxyline ; ii++,nn+=NXY_MEMPLOT ){
          x1 = mp->xyline[nn  ] ; y1 = mp->xyline[nn+1] ;
          x2 = mp->xyline[nn+2] ; y2 = mp->xyline[nn+3] ;
          mp->xyline[nn  ] = y1 ;
          mp->xyline[nn+1] = xtop - x1 ;
          mp->xyline[nn+2] = y2 ;
          mp->xyline[nn+3] = xtop - x2 ;
       }
      break ;

      case (MRI_ROT_0+MRI_FLMADD):
       for( nn=ii=0 ; ii < mp->nxyline ; ii++,nn+=NXY_MEMPLOT ){
          x1 = mp->xyline[nn  ] ; y1 = mp->xyline[nn+1] ;
          x2 = mp->xyline[nn+2] ; y2 = mp->xyline[nn+3] ;
          mp->xyline[nn  ] = xtop - x1 ;
          mp->xyline[nn+1] = y1 ;
          mp->xyline[nn+2] = xtop - x2 ;
          mp->xyline[nn+3] = y2 ;
       }
      break ;

      case (MRI_ROT_90+MRI_FLMADD):
       for( nn=ii=0 ; ii < mp->nxyline ; ii++,nn+=NXY_MEMPLOT ){
          x1 = mp->xyline[nn  ] ; y1 = mp->xyline[nn+1] ;
          x2 = mp->xyline[nn+2] ; y2 = mp->xyline[nn+3] ;
          mp->xyline[nn  ] = y1 ;
          mp->xyline[nn+1] = x1 ;
          mp->xyline[nn+2] = y2 ;
          mp->xyline[nn+3] = x2 ;
       }
      break ;

      case (MRI_ROT_180+MRI_FLMADD):
       for( nn=ii=0 ; ii < mp->nxyline ; ii++,nn+=NXY_MEMPLOT ){
          x1 = mp->xyline[nn  ] ; y1 = mp->xyline[nn+1] ;
          x2 = mp->xyline[nn+2] ; y2 = mp->xyline[nn+3] ;
          mp->xyline[nn  ] = x1 ;
          mp->xyline[nn+1] = ytop - y1 ;
          mp->xyline[nn+2] = x2 ;
          mp->xyline[nn+3] = ytop - y2 ;
       }
      break ;

      case (MRI_ROT_270+MRI_FLMADD):
       for( nn=ii=0 ; ii < mp->nxyline ; ii++,nn+=NXY_MEMPLOT ){
          x1 = mp->xyline[nn  ] ; y1 = mp->xyline[nn+1] ;
          x2 = mp->xyline[nn+2] ; y2 = mp->xyline[nn+3] ;
          mp->xyline[nn  ] = ytop - y1 ;
          mp->xyline[nn+1] = xtop - x1 ;
          mp->xyline[nn+2] = ytop - y2 ;
          mp->xyline[nn+3] = xtop - x2 ;
       }
      break ;
   }

   return ;
}

/*---------------------------------------------------------------------------
  Set the insertion point for over-writing -- 15 Nov 2001
-----------------------------------------------------------------------------*/

void insert_at_memplot( int ii , MEM_plotdata *mp )
{
   if( mp != NULL ) mp->insert_at = ii ;
   return ;
}

/*---------------------------------------------------------------------------
  Cut lines nbot..ntop out of a memplot -- 15 Nov 2001
-----------------------------------------------------------------------------*/

void cutlines_memplot( int nbot , int ntop , MEM_plotdata *mp )
{
   if( mp == NULL          ) return ;  /* bad or meaningless stuff */
   if( nbot <  0           ) return ;
   if( ntop >= mp->nxyline ) return ;
   if( nbot > ntop         ) return ;

   if( ntop == mp->nxyline-1 ){  /* just set num lines to nbot */

      mp->nxyline = nbot ;

   } else {                      /* must move things above ntop down */

      memmove( mp->xyline + NXY_MEMPLOT*nbot ,
               mp->xyline + NXY_MEMPLOT*(ntop+1) ,
               sizeof(float)*NXY_MEMPLOT*(mp->nxyline-1-ntop) ) ;

      mp->nxyline -= (ntop-nbot+1) ;

   }
   return ;
}

/*----------------------------------------------------------------------------*/

#ifdef __GNUC__
# define INLINE inline
#else
# define INLINE /*nada*/
#endif

/*----------------------------------------------------------------------------
  Clip a line to a rectangle.  Return is -1 if the line is totally outside.
  Otherwise, return is 0 and *x1in (etc.) is altered to the clipped line.
------------------------------------------------------------------------------*/

static INLINE int clip_line_to_rect( float xclbot , float yclbot ,
                                     float xcltop , float ycltop ,
                                     float *x1in  , float *y1in  ,
                                     float *x2in  , float *y2in   )
{
   float x1=*x1in , y1=*y1in , x2=*x2in , y2=*y2in , dx,dy,slope,temp ;
   int inter=0 ;

   /* Make sure that x1 < x2 by interchanging the points if necessary */

   if( x1 > x2 ){
     temp=x1 ; x1=x2 ; x2=temp;
     temp=y1 ; y1=y2 ; y2=temp; inter=1 ;
   }

   /* if outside entire region, throw line away */

   if( x2 < xclbot || x1 > xcltop ) return -1;

   if( y1 < y2 ){
     if( y2 < yclbot || y1 > ycltop ) return -1;
   } else {
     if( y1 < yclbot || y2 > ycltop ) return -1;
   }

   /* if inside entire region, then do nothing */

   if( x1 >= xclbot && x2 <= xcltop ){
     if( y1 < y2 ){
       if( y1 >= yclbot && y2 <= ycltop ) return 0 ;
     } else {
       if( y2 >= yclbot && y1 <= ycltop ) return 0 ;
     }
   }

   /* Clip line in X direction */

   dx = x2 - x1 ;
   if( dx > 0.0 ){  /* only clip if line has some x range */
     slope = (y2-y1)/dx ;
     if( x1 < xclbot ){  /* intercept of line at left side */
       y1 = y1 + slope*(xclbot-x1) ;
       x1 = xclbot ;
     }
     if( x2 > xcltop ){  /* intercept at right */
       y2 = y2 + slope*(xcltop-x2) ;
       x2 = xcltop ;
     }
   }

   /* Check line again to see if it falls outside of plot region */

   if( y1 < y2 ){
     if( y2 < yclbot || y1 > ycltop ) return -1;
   } else {
     if( y1 < yclbot || y2 > ycltop ) return -1;

     temp=x1 ; x1=x2 ; x2=temp;                 /* make sure y1 <= y2 */
     temp=y1 ; y1=y2 ; y2=temp; inter=!inter ;
   }

   /* Clip y-direction.  To do this, must have y1 <= y2 [supra] */

   dy = y2 - y1 ;
   if( dy > 0.0 ){  /* only clip if line has some Y range */
     slope = (x2-x1)/dy ;
     if( y1 < yclbot ){ /* intercept of line at bottom */
       x1 = x1 + slope*(yclbot-y1) ;
       y1 = yclbot ;
     }
     if( y2 > ycltop ){ /* intercept at top */
       x2 = x2 + slope*(ycltop-y2) ;
       y2 = ycltop ;
     }
   }

   /* Line is now guaranteed to be totally inside the plot region.
      Copy local clipped coordinates to output values and return.
      Note that we must restore points to original input order,
      if they were interchanged at some point above.              */

   if( inter ){
     *x1in = x2 ; *x2in = x1 ; *y1in = y2 ; *y2in = y1 ;
   } else {
     *x1in = x1 ; *y1in = y1 ; *x2in = x2 ; *y2in = y2 ;
   }

   return 0 ;
}

#undef INSIDE
#define INSIDE(x,y)                                                    \
  ( (x) >= xclbot && (x) <= xcltop && (y) >= yclbot && (y) <= ycltop )

/*---------------------------------------------------------------------------
  Clip a memplot to a rectangle, producing a new memplot.
-----------------------------------------------------------------------------*/

MEM_plotdata * clip_memplot( float xclbot, float yclbot,
                             float xcltop, float ycltop , MEM_plotdata *mp )
{
   MEM_plotdata *np ;
   char str[256] ;
   int nn , ii , qq ;
   float x1,y1 , x2,y2 , col,th ;

   if( mp == NULL       ) return NULL ;  /* bad or meaningless stuff */
   if( xclbot >= xcltop ) return NULL ;
   if( yclbot >= ycltop ) return NULL ;

   sprintf(str,"%.240sCopy",mp->ident) ;
   nn = create_memplot_surely( str , mp->aspect ) ;
   np = find_memplot(NULL) ;
   if( np == NULL ) return NULL ; /* shouldn't happen */

   for( nn=ii=0 ; ii < mp->nxyline ; ii++,nn+=NXY_MEMPLOT ){
     x1 = mp->xyline[nn  ] ; y1 = mp->xyline[nn+1] ;
     x2 = mp->xyline[nn+2] ; y2 = mp->xyline[nn+3] ;
     col= mp->xyline[nn+4] ; th = mp->xyline[nn+5] ;

     if( th < 0.0 ){               /** Not a line! */
       int thc = (int)(-th) ;
       switch( thc ){
         case THCODE_RECT:         /* rectangle */
                                   /* both corners inside */
           if( INSIDE(x1,y1) && INSIDE(x2,y2) ){
             ADDTO_MEMPLOT(np,x1,y1,x2,y2,col,th) ;
           }
         break ;

         case THCODE_CIRC:{        /* circle */
                                   /* +/- 1 radius inside */
           float xx,yy , rr=x2 ;
           xx = x1+rr ; if( !INSIDE(xx,y1) ) break ;
           xx = x1-rr ; if( !INSIDE(xx,y1) ) break ;
           yy = y1+rr ; if( !INSIDE(x1,yy) ) break ;
           yy = y1-rr ; if( !INSIDE(x1,yy) ) break ;
           ADDTO_MEMPLOT(np,x1,y1,x2,y2,col,th) ;
         }
         break ;
       }

     } else {                      /** Truly a line! **/

       qq = clip_line_to_rect( xclbot,yclbot , xcltop,ycltop ,
                               &x1,&y1       , &x2,&y2        ) ;
       if( qq == 0 ){
         ADDTO_MEMPLOT(np,x1,y1,x2,y2,col,th) ;
       }
     }
   }

   if( np->nxyline == 0 ) DESTROY_MEMPLOT(np) ;

   return np ;
}

/****************************************************************************
  Functions to interface with PLOTPAK Fortran routines
*****************************************************************************/

/*-------------------------------
  Has no function at this time.
---------------------------------*/
void plotpak_frame(void) { frame_() ; }

/*-----------------------------------------------
   Draws a sequence of lines in one swell foop.
-------------------------------------------------*/
void plotpak_curve( float * x , float * y , int n )
{
   integer nn = n ;
   curve_( (real *) x , (real *) y , &nn ) ;
}

/*---------------------------------------------------
  Establishes first point of a series of lines
  to be drawn one at time using plotpak_vector.
-----------------------------------------------------*/
void plotpak_frstpt( float x , float y )
{
   real xx = x , yy = y ;
   frstpt_( &xx , &yy ) ;
}

/*---------------------------------------------------
  Draws next in the series of lines.
-----------------------------------------------------*/
void plotpak_vector( float x , float y )
{
   real xx=x , yy=y ;
   vector_( &xx , &yy ) ;
}

/*-------------------
   Draws one line.
---------------------*/
void plotpak_line( float x1 , float y1 , float x2 , float y2 )
{
   real xx1 = x1 , yy1 = y1 , xx2 = x2 , yy2 = y2 ;
   line_(&xx1, &yy1, &xx2, &yy2);
}

/*--------------------------------------------------------------
 Converts (x1,y1) from user to memplot coordinates - 20 Nov 2001
----------------------------------------------------------------*/

void plotpak_zzphys( float x1 , float y1 , float *x2 , float *y2 )
{
   real xx1 = x1 , yy1 = y1 ;
   zzphys_( &xx1 , &yy1 ) ;
   if( x2 != NULL ) *x2 = xx1 ;
   if( y2 != NULL ) *y2 = yy1 ;
}

/*--------------------------------------------------------------
 Converts (x1,y1) from memplot to user coordinates - 20 Nov 2001
----------------------------------------------------------------*/

void plotpak_unphys( float x1 , float y1 , float *x2 , float *y2 )
{
   double rr ;
   if( x2 != NULL ){
      rr = (x1 - zzzplt_.betaxx) / zzzplt_.alphxx ;
      if( zzzplt_.ixcoor < 0 ) rr = pow(10.0,rr) ;
      *x2 = rr ;
   }
   if( y2 != NULL ){
      rr = (y1 - zzzplt_.betayy) / zzzplt_.alphyy ;
      if( zzzplt_.iycoor < 0 ) rr = pow(10.0,rr) ;
      *y2 = rr ;
   }
}

/*-----------------------------------------------------------------------------
  Establishes relationship between objective (memplot) coordinates
  (x range is from 0 to aspect, y from 0 to 1.0) and subjective (user)
  coordinates.  The range xs1..xs2 is mapped onto xo1..xo2, and similarly
  for y.  "code" establishes the form of the mapping:
    code = 1 => x linear, y linear
    code = 2 => x linear, y logarithmic
    code = 3 => x log   , y linear
    code = 4 => x log   , y log
-------------------------------------------------------------------------------*/
void plotpak_set( float xo1,float xo2 , float yo1,float yo2 ,
                  float xs1,float xs2 , float ys1,float ys2 , int code )
{
   real xobj1=xo1, xobj2=xo2, yobj1=yo1, yobj2=yo2;
   real xsub1=xs1, xsub2=xs2, ysub1=ys1, ysub2=ys2 ;
   integer ltype = code ;
   set_(&xobj1, &xobj2, &yobj1, &yobj2, &xsub1, &xsub2, &ysub1, &ysub2, &ltype);
}

void plotpak_getset( float *xo1,float *xo2 , float *yo1,float *yo2 ,
                     float *xs1,float *xs2 , float *ys1,float *ys2  )
{
   if( xo1 != NULL ) *xo1 = (float) zzzplt_.xbot ;
   if( xo2 != NULL ) *xo2 = (float) zzzplt_.xtop ;
   if( yo1 != NULL ) *yo1 = (float) zzzplt_.ybot ;
   if( yo1 != NULL ) *yo2 = (float) zzzplt_.ytop ;

   if( xs1 != NULL ) *xs1 = (float) zzzplt_.xmin ;
   if( xs2 != NULL ) *xs2 = (float) zzzplt_.xmax ;
   if( ys1 != NULL ) *ys1 = (float) zzzplt_.ymin ;
   if( ys1 != NULL ) *ys2 = (float) zzzplt_.ymax ;

   return ;
}

/*-----------------------------------------
  Set line type: 1 = solid (default)
                 2 = long dash
                 3 = short dash
                 4 = long - short - short
                 5 = very short
-------------------------------------------*/
void plotpak_setlin( int code )
{
   integer ntype=code ;
   setlin_(&ntype);
}

/*------------------------------------------------
  Set the clipping window (objective coordinates)
--------------------------------------------------*/
void plotpak_setw( float xo1,float xo2 , float yo1,float yo2 )
{
   real xobj1=xo1, xobj2=xo2, yobj1=yo1, yobj2=yo2;
   setw_( &xobj1, &xobj2, &yobj1, &yobj2 ) ;
}

/*------- Plotpak routines that I'm not documenting yet
          see the .f source code, or NCAR manual, if you care -------*/

void plotpak_setdsh( int nd , float * xd )
{
   integer nnd = nd ;
   setdsh_( &nnd , (real *) xd ) ;
}

void plotpak_setfrm( float xo1,float xo2 , float yo1,float yo2 )
{
   real xobj1=xo1, xobj2=xo2, yobj1=yo1, yobj2=yo2;
   setfrm_( &xobj1, &xobj2, &yobj1, &yobj2 ) ;
}

void plotpak_phdot( float x1 , float y1 )
{
   real xx1=x1 , yy1=y1 ;
   phdot_(&xx1,&yy1);
}

void plotpak_phline( float x1 , float y1 , float x2 , float y2 )
{
   real xx1 = x1 , yy1 = y1 , xx2 = x2 , yy2 = y2 ;
   phline_(&xx1, &yy1, &xx2, &yy2);
}

void plotpak_point( float x1 , float y1 )
{
   real xx1=x1 , yy1=y1 ;
   point_(&xx1,&yy1);
}

void plotpak_points( float *x , float *y , int n , int ipen )
{
   integer nn=n , nipen=ipen , zero=0 ;
   points_( (real *)x , (real *)y , &nn , &zero , &nipen ) ;
}

void ppak_garbage_routine(void) ;
void this_is_real_junk(void){ ppak_garbage_routine(); }
