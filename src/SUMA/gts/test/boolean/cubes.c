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

#include <stdlib.h>
#include "gts.h"

static void surface_add_box (GtsSurface * s,
			     gdouble x1, gdouble y1, gdouble z1,
			     gdouble x2, gdouble y2, gdouble z2)
{
  GtsVertex * v0 = gts_vertex_new (s->vertex_class, x1, y1, z1);
  GtsVertex * v1 = gts_vertex_new (s->vertex_class, x1, y1, z2);
  GtsVertex * v2 = gts_vertex_new (s->vertex_class, x1, y2, z2);
  GtsVertex * v3 = gts_vertex_new (s->vertex_class, x1, y2, z1);
  GtsVertex * v4 = gts_vertex_new (s->vertex_class, x2, y1, z1);
  GtsVertex * v5 = gts_vertex_new (s->vertex_class, x2, y1, z2);
  GtsVertex * v6 = gts_vertex_new (s->vertex_class, x2, y2, z2);
  GtsVertex * v7 = gts_vertex_new (s->vertex_class, x2, y2, z1);

  GtsEdge * e1 = gts_edge_new (s->edge_class, v0, v1);
  GtsEdge * e2 = gts_edge_new (s->edge_class, v1, v2);
  GtsEdge * e3 = gts_edge_new (s->edge_class, v2, v3);
  GtsEdge * e4 = gts_edge_new (s->edge_class, v3, v0);
  GtsEdge * e5 = gts_edge_new (s->edge_class, v0, v2);

  GtsEdge * e6 = gts_edge_new (s->edge_class, v4, v5);
  GtsEdge * e7 = gts_edge_new (s->edge_class, v5, v6);
  GtsEdge * e8 = gts_edge_new (s->edge_class, v6, v7);
  GtsEdge * e9 = gts_edge_new (s->edge_class, v7, v4);
  GtsEdge * e10 = gts_edge_new (s->edge_class, v4, v6);
  
  GtsEdge * e11 = gts_edge_new (s->edge_class, v3, v7);
  GtsEdge * e12 = gts_edge_new (s->edge_class, v2, v6);
  GtsEdge * e13 = gts_edge_new (s->edge_class, v1, v5);
  GtsEdge * e14 = gts_edge_new (s->edge_class, v0, v4);

  GtsEdge * e15 = gts_edge_new (s->edge_class, v1, v6);
  GtsEdge * e16 = gts_edge_new (s->edge_class, v2, v7);
  GtsEdge * e17 = gts_edge_new (s->edge_class, v3, v4);
  GtsEdge * e18 = gts_edge_new (s->edge_class, v0, v5);

  GtsFaceClass * klass = gts_face_class ();

  gts_surface_add_face (s, gts_face_new (klass, e1, e2, e5));
  gts_surface_add_face (s, gts_face_new (klass, e5, e3, e4));
  gts_surface_add_face (s, gts_face_new (klass, e6, e10, e7));
  gts_surface_add_face (s, gts_face_new (klass, e10, e9, e8));
  gts_surface_add_face (s, gts_face_new (klass, e2, e15, e12));
  gts_surface_add_face (s, gts_face_new (klass, e15, e13, e7));
  gts_surface_add_face (s, gts_face_new (klass, e3, e16, e11));
  gts_surface_add_face (s, gts_face_new (klass, e16, e12, e8));
  gts_surface_add_face (s, gts_face_new (klass, e17, e14, e4));
  gts_surface_add_face (s, gts_face_new (klass, e17, e11, e9));
  gts_surface_add_face (s, gts_face_new (klass, e18, e13, e1));
  gts_surface_add_face (s, gts_face_new (klass, e18, e14, e6));
}

static void failed (GtsSurface * s, GtsSurface * cs, guint n)
{
  gchar name[80];
  FILE * fp;
  
  sprintf (name, "cs.%u.gts", n);
  fp = fopen (name, "wt");
  gts_surface_write (cs, fp);
  fclose (fp);
  
  sprintf (name, "s.%u.gts", n);
  fp = fopen (name, "wt");
  gts_surface_write (s, fp);
  fclose (fp);
}

static void surface_inter (GtsSurface * s, 
			   GNode * stree,
			   gboolean is_open,
			   gdouble x1, gdouble y1, gdouble z1,
			   gdouble x2, gdouble y2, gdouble z2,
			   guint depth, guint maxdepth)
{
  GtsSurface * cs; 
  GNode * ctree;
  GtsSurfaceInter * si;
  gdouble size = (x2 - x1)/2.;
  gboolean closed = TRUE;
  static guint ncalls = 0, ninter = 0, nfailed = 0;

  if (depth > maxdepth)
    return;

  cs = gts_surface_new (gts_surface_class (),
			gts_face_class (),
			gts_edge_class (),
			gts_vertex_class ());
  surface_add_box (cs, x1, y1, z1, x2, y2, z2);
  ctree = gts_bb_tree_surface (cs);
  si = gts_surface_inter_new (gts_surface_inter_class (),
			      cs, s, ctree, stree, FALSE, is_open);
  ncalls++;
  if (!gts_surface_inter_check (si, &closed) ||
      (si->edges && !closed)) {
    failed (s, cs, nfailed++);
    gts_object_destroy (GTS_OBJECT (si));
    gts_object_destroy (GTS_OBJECT (cs));
    gts_bb_tree_destroy (ctree, TRUE);
    return;
  }
  else if (si->edges || depth < 4) {
    if (si->edges) {
      GtsSurface * sb;
      gdouble a = gts_surface_volume (cs), a1;
      
      g_assert (closed);
      sb = gts_surface_new (gts_surface_class (),
			    gts_face_class (),
			    gts_edge_class (),
			    gts_vertex_class ());
      gts_surface_inter_boolean (si, sb, GTS_1_OUT_2);
      gts_object_destroy (GTS_OBJECT (sb));

      sb = gts_surface_new (gts_surface_class (),
			    gts_face_class (),
			    gts_edge_class (),
			    gts_vertex_class ());
      gts_surface_inter_boolean (si, sb, GTS_1_IN_2);
      gts_surface_inter_boolean (si, sb, GTS_2_IN_1);
      a1 = gts_surface_volume (sb)/a;
      gts_object_destroy (GTS_OBJECT (sb));

      if (a1 > 1.001)
	failed (s, cs, nfailed++);
    }

    gts_object_destroy (GTS_OBJECT (cs));
    gts_bb_tree_destroy (ctree, TRUE);
    gts_object_destroy (GTS_OBJECT (si));

    ninter++;

    surface_inter (s, stree, is_open, 
		   x1, y1, z1, x1 + size, y1 + size, z1 + size,
		   depth + 1, maxdepth);
    surface_inter (s, stree,  is_open, 
		   x1 + size, y1, z1, x2, y1 + size, z1 + size,
		   depth + 1, maxdepth);
    surface_inter (s, stree,  is_open, 
		   x1, y1 + size, z1, x1 + size, y2, z1 + size,
		   depth + 1, maxdepth);
    surface_inter (s, stree,  is_open, 
		   x1 + size, y1 + size, z1, x2, y2, z1 + size,
		   depth + 1, maxdepth);
    surface_inter (s, stree,  is_open, 
		   x1, y1, z1 + size, x1 + size, y1 + size, z2,
		   depth + 1, maxdepth);
    surface_inter (s, stree,  is_open, 
		   x1 + size, y1, z1 + size, x2, y1 + size, z2,
		   depth + 1, maxdepth);
    surface_inter (s, stree,  is_open, 
		   x1, y1 + size, z1 + size, x1 + size, y2, z2,
		   depth + 1, maxdepth);
    surface_inter (s, stree,  is_open, 
		   x1 + size, y1 + size, z1 + size, x2, y2, z2,
		   depth + 1, maxdepth);
  }
  else {
    gts_object_destroy (GTS_OBJECT (cs));
    gts_bb_tree_destroy (ctree, TRUE);
    gts_object_destroy (GTS_OBJECT (si));
  }

  printf ("\rncalls: %6u ninter: %6u nfailed: %6u",
	  ncalls, ninter, nfailed);
  fflush (stdout);
}

int main (int argc, char * argv[])
{
  GtsSurface * s;
  GtsFile * fp;
  GNode * stree;
  gboolean is_open;

  s = gts_surface_new (gts_surface_class (),
		       GTS_FACE_CLASS (gts_face_class ()),
		       GTS_EDGE_CLASS (gts_edge_class ()),
		       GTS_VERTEX_CLASS (gts_vertex_class ()));
  fp = gts_file_new (stdin);
  if (gts_surface_read (s, fp)) {
    fprintf (stderr, "cubes: file on standard input is not a valid GTS surface file\n");
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1;
  }
  gts_file_destroy (fp);

  if (!gts_surface_is_orientable (s)) {
    fprintf (stderr, "surface is not orientable\n");
    return 1;
  }
  else if (!gts_surface_is_closed (s)) {
    fprintf (stderr, "surface is not closed\n");
    return 1;
  }
  else if (gts_surface_is_self_intersecting (s)) {
    fprintf (stderr, "surface is self-intersecting\n");
    return 1;
  }

  stree = gts_bb_tree_surface (s);
  is_open = gts_surface_volume (s) < 0. ? TRUE : FALSE;
  surface_inter (s, stree, is_open, -0.5, -0.5, -0.5, 0.5, 0.5, 0.5, 0, 9);
  printf ("\n");
  return 0;
}
