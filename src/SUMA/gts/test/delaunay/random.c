/* GTS - Library for the manipulation of triangulated surfaces
 * Copyright (C) 1999 St�phane Popinet
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
#include "gts.h"

int main (int argc, char * argv[])
{
  guint i, n;
  GtsSurface * surface;
  GSList * l, * vertices = NULL;
  GtsTriangle * t;
  GtsVertex * v1, * v2, * v3;
  GTimer * timer;

  if (argc != 2) {
    fprintf (stderr, "usage: random n\n");
    return 0;
  }

  n = atoi (argv[1]);

  timer = g_timer_new ();
  g_timer_start (timer);
  for (i = 0; i < n; i++)
    vertices = g_slist_prepend (vertices, 
				gts_vertex_new (gts_vertex_class (),
						rand (), rand (), 0.));

  t = gts_triangle_enclosing (gts_triangle_class (), vertices, 100.);
  gts_triangle_vertices (t, &v1, &v2, &v3);
  surface = gts_surface_new (gts_surface_class (),
			     gts_face_class (),
			     gts_edge_class (),
			     gts_vertex_class ());
  gts_surface_add_face (surface, gts_face_new (gts_face_class (),
					       t->e1, t->e2, t->e3));
  g_timer_stop (timer);
  fprintf (stderr, "Input: %g s\n", g_timer_elapsed (timer, NULL));
  g_timer_reset (timer);

  g_timer_start (timer);
  l = vertices;
  while (l) {
    gts_delaunay_add_vertex (surface, l->data, NULL);
    l = l->next;
  }

  gts_allow_floating_vertices = TRUE;
  gts_object_destroy (GTS_OBJECT (v1));
  gts_object_destroy (GTS_OBJECT (v2));
  gts_object_destroy (GTS_OBJECT (v3));
  gts_allow_floating_vertices = FALSE;
  g_timer_stop (timer);

  fprintf (stderr, "Triangulation: %g s speed: %.0f vertex/s\n", 
	   g_timer_elapsed (timer, NULL),
	   g_slist_length (vertices)/g_timer_elapsed (timer, NULL));
  g_timer_reset (timer);

  g_timer_start (timer);
  gts_surface_write (surface, stdout);
  g_timer_stop (timer);

  fprintf (stderr, "Output: %g s\n", g_timer_elapsed (timer, NULL));

  if (gts_delaunay_check (surface)) {
    fprintf (stderr, "WARNING: surface is not Delaunay\n");
    return 0;
  }

  return 1;
}
