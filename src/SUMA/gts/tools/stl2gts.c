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

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "config.h"
#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#endif /* HAVE_GETOPT_H */
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "gts.h"

static GPtrArray * stl_read (FILE * fp)
{
  GPtrArray * a = g_ptr_array_new ();
  char tag[6];

  fgets (tag, 6, fp);
  rewind (fp);
  if (!strcmp (tag, "solid")) { /* ASCII file */
    GtsFile * f = gts_file_new (fp);

    while (f->type == GTS_STRING) {
      if (!strcmp (f->token->str, "vertex")) {
	gdouble x, y, z;

	gts_file_next_token (f);
	if (f->type != GTS_INT && f->type != GTS_FLOAT) {
	  fprintf (stderr, 
		   "Input file is not a valid STL file\n"
		   "stdin:%d:%d: expecting a number (x-coordinate)\n",
		   f->line, f->pos);
	  exit (1);
	}
	x = atof (f->token->str);

	gts_file_next_token (f);
	if (f->type != GTS_INT && f->type != GTS_FLOAT) {
	  fprintf (stderr, 
		   "Input file is not a valid STL file\n"
		   "stdin:%d:%d: expecting a number (y-coordinate)\n",
		   f->line, f->pos);
	  exit (1);
	}
	y = atof (f->token->str);

	gts_file_next_token (f);
	if (f->type != GTS_INT && f->type != GTS_FLOAT) {
	  fprintf (stderr, 
		   "Input file is not a valid STL file\n"
		   "stdin:%d:%d: expecting a number (z-coordinate)\n",
		   f->line, f->pos);
	  exit (1);
	}
	z = atof (f->token->str);

	g_ptr_array_add (a, gts_vertex_new (gts_vertex_class (), x, y, z));
      }
      else if (!strcmp (f->token->str, "endsolid"))
	break;
      gts_file_first_token_after (f, '\n');
    }
    gts_file_destroy (f);
  }
  else { /* binary file */
    guint32 nf;
    gchar header[80];
    guint i;

    if (fread (header, sizeof (gchar), 80, fp) != 80) {
      fprintf (stderr, "Input file is not a valid STL file\n"
	       "stdin: incomplete header\n");
      exit (1);
    }
    if (fread (&nf, sizeof (guint32), 1, fp) != 1) {
      fprintf (stderr, "Input file is not a valid STL file\n"
	       "stdin: missing number of facets\n");
      exit (1);
    }
    i = nf;
    while (i > 0) {
      gfloat x, y, z;
      guint j;
      guint16 attbytecount;

      if (fread (&x, sizeof (gfloat), 1, fp) != 1) {
	fprintf (stderr, "Input file is not a valid STL file\n"
	       "stdin: missing normal x-coordinate\n");
	exit (1);
      }
      if (fread (&y, sizeof (gfloat), 1, fp) != 1) {
	fprintf (stderr, "Input file is not a valid STL file\n"
	       "stdin: missing normal y-coordinate\n");
	exit (1);
      }
      if (fread (&z, sizeof (gfloat), 1, fp) != 1) {
	fprintf (stderr, "Input file is not a valid STL file\n"
	       "stdin: missing normal z-coordinate\n");
	exit (1);
      }

      for (j = 0; j < 3; j++) {
	if (fread (&x, sizeof (gfloat), 1, fp) != 1) {
	  fprintf (stderr, "Input file is not a valid STL file\n"
		   "stdin: missing vertex x-coordinate\n");
	  exit (1);
	}
	if (fread (&y, sizeof (gfloat), 1, fp) != 1) {
	  fprintf (stderr, "Input file is not a valid STL file\n"
		   "stdin: missing vertex y-coordinate\n");
	  exit (1);
	}
	if (fread (&z, sizeof (gfloat), 1, fp) != 1) {
	  fprintf (stderr, "Input file is not a valid STL file\n"
		   "stdin: missing vertex z-coordinate\n");
	  exit (1);
	}
	g_ptr_array_add (a, gts_vertex_new (gts_vertex_class (), x, y, z));
      }

      if (fread (&attbytecount, sizeof (guint16), 1, fp) != 1) {
	fprintf (stderr, "Input file is not a valid STL file\n"
	       "stdin: missing attribute byte count\n");
	exit (1);
      }
      if (attbytecount != 0) {
	fprintf (stderr, "Input file is not a valid STL file\n"
	       "stdin: attribute byte count is not zero\n");
	exit (1);
      }
      
      i--;
    }
  }

  return a;
}

static void vertices_merge (GPtrArray * stl, gdouble epsilon)
{
  GPtrArray * array;
  GNode * kdtree;
  guint i;

  array = g_ptr_array_new ();
  for (i = 0; i < stl->len; i++)
    g_ptr_array_add (array, stl->pdata[i]);
  kdtree = gts_kdtree_new (array, NULL);
  g_ptr_array_free (array, TRUE);

  for (i = 0; i < stl->len; i++) {
    GtsVertex * v = stl->pdata[i];

    if (!GTS_OBJECT (v)->reserved) { /* Do something only if v is active */
      GtsBBox * bbox;
      GSList * selected, * j;

      /* build bounding box */
      bbox = gts_bbox_new (gts_bbox_class (),
			   v, 
			   GTS_POINT (v)->x - epsilon,
			   GTS_POINT (v)->y - epsilon,
			   GTS_POINT (v)->z - epsilon,
			   GTS_POINT (v)->x + epsilon,
			   GTS_POINT (v)->y + epsilon,
			   GTS_POINT (v)->z + epsilon);

      /* select vertices which are inside bbox using kdtree */
      j = selected = gts_kdtree_range (kdtree, bbox, NULL);
      while (j) {
	GtsVertex * sv = j->data;

	if (sv != v && !GTS_OBJECT (sv)->reserved)
	  GTS_OBJECT (sv)->reserved = v; /* mark sv as inactive */
	j = j->next;
      }
      g_slist_free (selected);
      gts_object_destroy (GTS_OBJECT (bbox));
    }
  }

  gts_kdtree_destroy (kdtree);

  /* destroy inactive vertices */

  /* we want to control vertex destruction */
  gts_allow_floating_vertices = TRUE;

  for (i = 0; i < stl->len; i++) {
    GtsVertex * v = stl->pdata[i];

    if (GTS_OBJECT (v)->reserved) { /* v is inactive */
      stl->pdata[i] = GTS_OBJECT (v)->reserved;
      gts_object_destroy (GTS_OBJECT (v));
    }
  }

  gts_allow_floating_vertices = FALSE; 
}

static void add_stl (GtsSurface * s, GPtrArray * stl)
{
  guint i;

  for (i = 0; i < stl->len/3; i++) {
    GtsEdge * e1 = GTS_EDGE (gts_vertices_are_connected (stl->pdata[3*i],
							 stl->pdata[3*i + 1]));
    GtsEdge * e2 = GTS_EDGE (gts_vertices_are_connected (stl->pdata[3*i + 1],
							 stl->pdata[3*i + 2]));
    GtsEdge * e3 = GTS_EDGE (gts_vertices_are_connected (stl->pdata[3*i + 2],
							 stl->pdata[3*i]));

    if (e1 == NULL)
      e1 = gts_edge_new (s->edge_class, 
			 stl->pdata[3*i], stl->pdata[3*i + 1]);
    if (e2 == NULL)
      e2 = gts_edge_new (s->edge_class, 
			 stl->pdata[3*i + 1], stl->pdata[3*i + 2]);
    if (e3 == NULL)
      e3 = gts_edge_new (s->edge_class, 
			 stl->pdata[3*i + 2], stl->pdata[3*i]);
    gts_surface_add_face (s, gts_face_new (s->face_class, e1, e2, e3));
  }    
}

int main (int argc, char * argv[])
{
  int c = 0;
  gboolean verbose = FALSE;
  gboolean nomerge = FALSE;
  gboolean revert  = FALSE;
  GPtrArray * stl;
  GtsSurface * s;

  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"revert", no_argument, NULL, 'r'},
      {"nomerge", no_argument, NULL, 'n'},
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hvnr",
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hvnr"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'r': /* revert */
      revert = TRUE;
      break;
    case 'n': /* nomerge */
      nomerge = TRUE;
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: stl2gts [OPTION]... < input.stl > output.gts\n"
	     "Convert an STL file to GTS format.\n"
	     "\n"
	     "  -r,     --revert       revert face normals\n"
	     "  -n,     --nomerge      do not merge vertices\n"
	     "  -v,     --verbose      display surface statistics\n"
	     "  -h,     --help         display this help and exit\n"
	     "\n"
	     "Report bugs to %s\n",
	     GTS_MAINTAINER);
      return 0;
      break;
    case 'v':
      verbose = TRUE;
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `stl2gts --help' for more information.\n");
      return 1;
    }
  }

  stl = stl_read (stdin);
  if (!nomerge)
    vertices_merge (stl, 0.);
  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());
  add_stl (s, stl);
  if (revert)
    gts_surface_foreach_face (s, (GtsFunc) gts_triangle_revert, NULL);
  if (verbose)
    gts_surface_print_stats (s, stderr);
  gts_surface_write (s, stdout);

  return 0;
}
