/*-------------------------------------------------------------------------
   Return an array of XColor that define the colormap for the given
   window.  This only works for PseudoColor visuals..
---------------------------------------------------------------------------*/

void xxx_get_colormap( Display * display , Window w , XColor ** xcar , int * ncar )
{
   Status sss ;
   XWindowAttributes xwat ;
   XColor * xcol ;
   VisualID vid ;
   XVisualInfo vinfo , * vin ;
   int count , ii ;

   sss = XGetWindowAttributes( display , w , &xwat ) ;
   if( sss == 0 ){ *xcar = NULL ; *ncar = 0 ; return ; }

   vinfo.visualid = vid = XVisualIDFromVisual(xwat.visual) ;
   vin = XGetVisualInfo( display , VisualIDMask , &vinfo , &count ) ;
   if( count == 0 || vin == NULL ){ *xcar = NULL ; *ncar = 0 ; return ; }

#if defined(__cplusplus) || defined(c_plusplus)
   if( vin->c_class != PseudoColor ){ XFree(vin) ; *xcar = NULL ; *ncar = 0 ; return ; }
#else
   if( vin->class != PseudoColor ){ XFree(vin) ; *xcar = NULL ; *ncar = 0 ; return ; }
#endif

   count = vin->colormap_size ;
   xcol  = (XColor *) malloc( sizefo(XColor) * count ) ;
   for( ii=0 ; ii < count ; ii++ ) xcol[ii].pixel = (Pixel) ii ;

   XQueryColors( display , xwat.colormap , xcol , count ) ;

   fprintf(stderr,"xxx_get_colormap: found %d colors:\n",count) ;
   for( ii=0 ; ii < count ; ii++ )
      fprintf(stderr,"  %3d: r = %x  g = %x  b = %x\n" ,
              ii , (int)xcol[ii].red , (int)xcol[ii].green , (int)xcol[ii].blue ) ;

   *xcar = xcol ; *ncar = count ; XFree(vin) ; return ;
}
