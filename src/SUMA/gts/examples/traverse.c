/* GTS - Library for the manipulation of triangulated surfaces
 * Copyright (C) 1999 Stéphane Popinet
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "config.h"
#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#endif /* HAVE_GETOPT_H */
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "gts.h"

GtsRange depth_range;

/* Colormap definition */

typedef struct {
  GPtrArray * colors;
  gboolean reversed;
} Colormap;

Colormap * colormap = NULL;

static GtsColor * color_new (gdouble r, gdouble g, gdouble b)
{
  GtsColor * c = g_malloc (sizeof (GtsColor));
  c->r = r; c->g = g; c->b = b;
  return c;
}

static Colormap * colormap_read (FILE * fptr)
{
  Colormap * cmap = g_malloc (sizeof (Colormap));
  GtsColor c;
  cmap->reversed = FALSE;
  cmap->colors = g_ptr_array_new ();
  while (fscanf (fptr, "%f %f %f", &c.r, &c.g, &c.b) == 3)
    g_ptr_array_add (cmap->colors, color_new (c.r/255., c.g/255., c.b/255.));
  return cmap;
}

static Colormap * colormap_red_blue (void)
{
  Colormap * cmap = g_malloc (sizeof (Colormap));
  cmap->reversed = FALSE;
  cmap->colors = g_ptr_array_new ();
  g_ptr_array_add (cmap->colors, color_new (1.0, 0.0, 0.0));
  g_ptr_array_add (cmap->colors, color_new (0.0, 0.0, 1.0));
  return cmap;
}

static GtsColor colormap_color (Colormap * cmap, gdouble val)
{
  GtsColor c = {1., 1., 1.}, * c1, * c2;
  guint i, n;
  gdouble coef;

  g_return_val_if_fail (cmap != NULL, c);

  if (val > 1.0) val = 1.0;
  else if (val < 0.0) val = 0.0;
  if (cmap->reversed)
    val = 1.0 - val;

  n = cmap->colors->len;
  if (n == 0)
    return c;
  if (n == 1)
    return *((GtsColor *)cmap->colors->pdata[0]);

  i = floor((gdouble)val*(gdouble)(n - 1));
  if (i == n - 1)
    return *((GtsColor *)cmap->colors->pdata[cmap->colors->len - 1]);
  coef = val*(gdouble)(n - 1) - (gdouble)i;
  c1 = cmap->colors->pdata[i];
  c2 = cmap->colors->pdata[i+1];
  c.r = c1->r + coef*(c2->r - c1->r);
  c.g = c1->g + coef*(c2->g - c1->g);
  c.b = c1->b + coef*(c2->b - c1->b);
  return c;
}

/* New Face class definition */

typedef struct _DepthFace         DepthFace;
typedef struct _DepthFaceClass    DepthFaceClass;

struct _DepthFace {
  GtsFaceClass face;

  guint depth;
};

struct _DepthFaceClass {
  GtsFaceClass parent_class;
};

#define DEPTH_FACE(obj)            GTS_OBJECT_CAST (obj,\
					           DepthFace,\
					           depth_face_class ())
#define DEPTH_FACE_CLASS(klass)    GTS_OBJECT_CLASS_CAST (klass,\
						         DepthFaceClass,\
						         depth_face_class())
#define IS_DEPTH_FACE(obj)         (gts_object_is_from_class (obj,\
						   depth_face_class ()))
     
DepthFaceClass * depth_face_class                (void);

static GtsColor depth_face_color (GtsObject * object)
{
  guint depth = DEPTH_FACE (object)->depth;

  return colormap_color (colormap, (gdouble) depth/depth_range.max);
}

static void depth_face_class_init (DepthFaceClass * klass)
{
  /* overload color definition */
  GTS_OBJECT_CLASS (klass)->color = depth_face_color;
}

static void depth_face_init (DepthFace * dface)
{
  dface->depth = 0;
}

DepthFaceClass * depth_face_class (void)
{
  static DepthFaceClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo depth_face_info = {
      "DepthFace",
      sizeof (DepthFace),
      sizeof (DepthFaceClass),
      (GtsObjectClassInitFunc) depth_face_class_init,
      (GtsObjectInitFunc) depth_face_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_face_class ()),
				  &depth_face_info);
  }

  return klass;
}

/* main functions */

static void pick_first_face (GtsFace * f, GtsFace ** first)
{
  if (*first == NULL)
    *first = f;
}

int main (int argc, char * argv[])
{
  GtsSurface * s;
  GtsFile * fp;
  GtsFace * first = NULL;
  int c = 0;
  gboolean verbose = FALSE;

  colormap = colormap_red_blue (); /* default */

  /* parse options using getopt */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"cmap", required_argument, NULL, 'c'},
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hvc:", 
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hvc:"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'c': { /* cmap */
      FILE * fptr = fopen (optarg, "rt");
      if (!fptr) {
	fprintf (stderr, "traverse: cannot open colormap file `%s'.\n",
		 optarg);
	return 1;
      }
      colormap = colormap_read (fptr);
      fclose (fptr);
      break;
    }
    case 'v': /* verbose */
      verbose = TRUE;
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: traverse [OPTION] < file.gts > file.oogl\n"
	     "Output an OOGL (geomview) surface colored according to the (graph) distance\n"
	     "from a random face to the others\n"
	     "\n"
	     "  -c FILE --cmap=FILE  load FILE as colormap\n"
	     "  -v      --verbose    print statistics about the surface\n"
	     "  -h      --help       display this help and exit\n"
	     "\n"
	     "Reports bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `traverse --help' for more information.\n");
      return 1; /* failure */
    }
  }

  s = gts_surface_new (gts_surface_class (),
		       GTS_FACE_CLASS (depth_face_class ()),
		       gts_edge_class (),
		       gts_vertex_class ());
  fp = gts_file_new (stdin);
  if (gts_surface_read (s, fp)) {
    fputs ("traverse: file on standard input is not a valid GTS file\n", 
	   stderr);
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1; /* failure */
  }

  if (verbose)
    gts_surface_print_stats (s, stderr);

  gts_surface_foreach_face (s, (GtsFunc) pick_first_face, &first);
  gts_range_init (&depth_range);
  if (first) {
    GtsSurfaceTraverse * t = gts_surface_traverse_new (s, first);
    GtsFace * f;
    guint level;
    while ((f = gts_surface_traverse_next (t, &level))) {
      DEPTH_FACE (f)->depth = level;
      gts_range_add_value (&depth_range, level);
    }
    gts_surface_traverse_destroy (t);
  }
  gts_range_update (&depth_range);
  if (verbose) {
    fputs ("distance: ", stderr);
    gts_range_print (&depth_range, stderr);
    fputc ('\n', stderr);
  }
  gts_surface_write_oogl (s, stdout);

  return 0;
}
