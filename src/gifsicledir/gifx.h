#ifndef GIFX_H
#define GIFX_H
#ifdef __cplusplus
extern "C" {
#endif

/* gifx.h - Functions to turn GIFs in memory into X Pixmaps.
   Copyright (C) 1997-9 Eddie Kohler, eddietwo@lcs.mit.edu
   This file is part of the GIF library.

   The GIF library is free software*. It is distributed under the GNU General
   Public License, version 2 or later; you can copy, distribute, or alter it
   at will, as long as this notice is kept intact and this source code is made
   available. There is no warranty, express or implied.

   *The LZW compression method used by GIFs is patented. Unisys, the patent
   holder, allows the compression algorithm to be used without a license in
   software distributed at no cost to the user. */

#include "gif.h"
#include <X11/Xlib.h>

#define GIFX_COLORMAP_EXTENSION -107


typedef struct Gif_XContext Gif_XContext;
typedef struct Gif_XColormap Gif_XColormap;

struct Gif_XContext {
  
  Display *display;
  int screen_number;
  Drawable drawable;
  Visual *visual;
  u_int16_t depth;
  u_int16_t ncolormap;
  Colormap colormap;
  
  u_int16_t nclosest;
  Gif_Color *closest;
  
  int free_deleted_colormap_pixels;
  Gif_XColormap *xcolormap;
  
  GC image_gc;
  GC mask_gc;
  
  unsigned long transparent_pixel;
  unsigned long foreground_pixel;
  int refcount;
  
};


Gif_XContext *	Gif_NewXContext(Display *, Window);
Gif_XContext *	Gif_NewXContextFromVisual(Display *, int screen_number,
					Visual *, int depth, Colormap);
void		Gif_DeleteXContext(Gif_XContext *);

Pixmap		Gif_XImage(Gif_XContext *, Gif_Stream *, Gif_Image *);
Pixmap		Gif_XImageColormap(Gif_XContext *, Gif_Stream *,
				Gif_Colormap *, Gif_Image *);
Pixmap		Gif_XSubImage(Gif_XContext *, Gif_Stream *, Gif_Image *,
				int l, int t, int w, int h);
Pixmap		Gif_XSubImageColormap(Gif_XContext *, Gif_Image *,
				Gif_Colormap *, int l, int t, int w, int h);

Pixmap		Gif_XMask(Gif_XContext *, Gif_Stream *, Gif_Image *);
Pixmap		Gif_XSubMask(Gif_XContext *, Gif_Image *,
				int l, int t, int w, int h);

Pixmap		Gif_XNextImage(Gif_XContext *, Pixmap last_last, Pixmap last,
			       Gif_Stream *, int n);

int		Gif_XAllocateColors(Gif_XContext *, Gif_Colormap *);
void		Gif_XDeallocateColors(Gif_XContext *, Gif_Colormap *);
unsigned long *	Gif_XClaimStreamColors(Gif_XContext *, Gif_Stream *, int *);


#ifdef __cplusplus
}
#endif
#endif
