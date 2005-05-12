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
  printf ("  0\nVERTEX\n  8\n0\n 10\n%g\n 20\n%g\n 30\n%g\n 70\n192\n",
	  GTS_POINT (v)->x, GTS_POINT (v)->y, GTS_POINT (v)->z); 
  GTS_OBJECT (v)->reserved = GUINT_TO_POINTER ((*nv)++);
}

static void write_face (GtsTriangle * t)
{
  GtsVertex * v1, * v2, * v3;

  gts_triangle_vertices (t, &v1, &v2, &v3);
  printf ("  0\nVERTEX\n  8\n0\n 10\n0.0\n 20\n0.0\n 30\n0.0\n 70\n128\n"
	  " 71\n%u\n 72\n%u\n 73\n%u\n",
	  GPOINTER_TO_UINT (GTS_OBJECT (v1)->reserved),
	  GPOINTER_TO_UINT (GTS_OBJECT (v2)->reserved),
	  GPOINTER_TO_UINT (GTS_OBJECT (v3)->reserved));
}

int main (int argc, char * argv[])
{
  GtsSurface * s;
  GtsFile * fp;
  guint nv = 1;
  GtsSurfaceStats st;

  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());
  fp = gts_file_new (stdin);
  if (gts_surface_read (s, fp)) {
    fputs ("gts2dxf: file on standard input is not a valid GTS file\n", 
	   stderr);
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1; /* failure */
  }

  gts_surface_stats (s, &st);
  printf ("  0\nSECTION\n  2\nHEADER\n  9\n$ACADVER\n  1\n"
	  "AC1009\n  0\nENDSEC\n  0\n"
	  "SECTION\n"
	  "  2\n"
	  "ENTITIES\n"
	  "  0\n"
	  "POLYLINE\n"
          "  8\n0\n"
	  " 66\n1\n"
	  " 10\n0.0\n"
	  " 20\n0.0\n"
	  " 30\n0.0\n"
	  " 70\n64\n"
	  " 71\n%u\n"
	  " 72\n%u\n",
	  st.edges_per_vertex.n, st.n_faces);
  gts_surface_foreach_vertex (s, (GtsFunc) write_vertex, &nv);
  gts_surface_foreach_face (s, (GtsFunc) write_face, NULL);
  printf ("  0\nSEQEND\n  0\nENDSEC\n  0\nEOF\n");
  return 0;
}
