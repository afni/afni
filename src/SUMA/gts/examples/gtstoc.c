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

#include "gts.h"

static void write_vertex (GtsVertex * v, guint * nv)
{
  printf ("  GtsVertex * v%u = gts_vertex_new (gts_vertex_class (), %g, %g, %g);\n",
	  *nv, GTS_POINT (v)->x, GTS_POINT (v)->y, GTS_POINT (v)->z);
  GTS_OBJECT (v)->reserved = GUINT_TO_POINTER ((*nv)++);
}

static void write_edge (GtsSegment * s, guint * ne)
{
  printf ("  GtsEdge * e%u = gts_edge_new (gts_edge_class (), v%u, v%u);\n",
	  *ne, 
	  GPOINTER_TO_UINT (GTS_OBJECT (s->v1)->reserved),
	  GPOINTER_TO_UINT (GTS_OBJECT (s->v2)->reserved));
  GTS_OBJECT (s)->reserved = GUINT_TO_POINTER ((*ne)++);
}

static void write_face (GtsTriangle * t, guint * nf)
{
  printf ("  GtsFace * f%u = gts_face_new (gts_face_class (),\n"
	  "                                e%u, e%u, e%u);\n",
	  (*nf)++, 
	  GPOINTER_TO_UINT (GTS_OBJECT (t->e1)->reserved),
	  GPOINTER_TO_UINT (GTS_OBJECT (t->e2)->reserved),
	  GPOINTER_TO_UINT (GTS_OBJECT (t->e3)->reserved));
}

int main (int argc, char * argv[])
{
  GtsSurface * s;
  guint i;
  GtsFile * fp;
  guint nv = 1, ne = 1, nf = 1;

  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());
  fp = gts_file_new (stdin);
  if (gts_surface_read (s, fp)) {
    fputs ("gtstoc: file on standard input is not a valid GTS file\n", 
	   stderr);
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1; /* failure */
  }

  printf ("  GtsSurface * surface = gts_surface_new (gts_surface_class (),\n"
	  "                                          gts_face_class (),\n"
	  "                                          gts_edge_class (),\n"
	  "                                          gts_vertex_class ());\n\n");
  gts_surface_foreach_vertex (s, (GtsFunc) write_vertex, &nv);
  printf ("\n");
  gts_surface_foreach_edge (s, (GtsFunc) write_edge, &ne);  
  printf ("\n");
  gts_surface_foreach_face (s, (GtsFunc) write_face, &nf);  
  printf ("  \n");
  for (i = 1; i < nf; i++)
    printf ("  gts_surface_add_face (surface, f%u);\n", i);

  return 0;
}
