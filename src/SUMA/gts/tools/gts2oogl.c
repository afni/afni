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
#include "config.h"
#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#endif /* HAVE_GETOPT_H */
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "gts.h"

#ifndef PI
# define PI 3.14159265359
#endif

typedef enum { NONE, 
	       QUALITY, 
	       AREA, 
	       EPV, 
	       INCOMP, 
	       FOLD, 
	       HEIGHT, 
	       CURVATURE, 
	       GAUSSIAN,
               COMPONENT } ColorOptions;

ColorOptions color = NONE;
gboolean autoscale = TRUE;
gboolean boundary = FALSE;
gboolean non_manifold = FALSE;
gboolean duplicate = FALSE;
gint flatten = -1;
guint height = 0;
gdouble avg = 0.;
gdouble min = 0.0, max = 1.0;
gdouble maxcosine2 = 1.0;
gboolean verbose = FALSE;
gboolean faces = FALSE;

typedef struct {
  gdouble r, g, b;
} Color;

typedef struct {
  GPtrArray * colors;
  gboolean reversed;
} Colormap;

Colormap * colormap = NULL;

static Color * color_new (gdouble r, gdouble g, gdouble b)
{
  Color * c = g_malloc (sizeof (Color));
  c->r = r; c->g = g; c->b = b;
  return c;
}

static Colormap * colormap_read (FILE * fptr)
{
  Colormap * cmap = g_malloc (sizeof (Colormap));
  Color c;
  cmap->reversed = FALSE;
  cmap->colors = g_ptr_array_new ();
  while (fscanf (fptr, "%lf %lf %lf", &c.r, &c.g, &c.b) == 3)
    g_ptr_array_add (cmap->colors, color_new (c.r/255., c.g/255., c.b/255.));
  return cmap;
}

static Colormap * colormap_jet (void)
{
  Colormap * cmap = g_malloc (sizeof (Colormap));
  gint i;

  cmap->reversed = FALSE;
  cmap->colors = g_ptr_array_new ();
  for (i = 0; i < 127; i++) {
    gdouble r = 
      i <= 46 ? 0. : 
      i >= 111 ? -0.03125*(i - 111) + 1. :
      i >= 78 ? 1. : 
      0.03125*(i - 46);
    gdouble g = 
      i <= 14 || i >= 111 ? 0. : 
      i >= 79 ? -0.03125*(i - 111) : 
      i <= 46 ? 0.03125*(i - 14) : 
      1.;
    gdouble b =
      i >= 79 ? 0. :
      i >= 47 ? -0.03125*(i - 79) :
      i <= 14 ? 0.03125*(i - 14) + 1.:
      1.;

    g_ptr_array_add (cmap->colors, color_new (r, g, b));
  }
  return cmap;
}

static Color colormap_color (Colormap * cmap, gdouble val)
{
  Color c = {1., 1., 1.}, * c1, * c2;
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
    return *((Color *)cmap->colors->pdata[0]);

  i = floor((gdouble)val*(gdouble)(n - 1));
  if (i == n - 1)
    return *((Color *)cmap->colors->pdata[cmap->colors->len - 1]);
  coef = val*(gdouble)(n - 1) - (gdouble)i;
  c1 = cmap->colors->pdata[i];
  c2 = cmap->colors->pdata[i+1];
  c.r = c1->r + coef*(c2->r - c1->r);
  c.g = c1->g + coef*(c2->g - c1->g);
  c.b = c1->b + coef*(c2->b - c1->b);
  return c;
}

static void write_point (GtsPoint * p, FILE * fp)
{
  fprintf (fp, "%g %g %g", 
	   flatten == 0 ? avg : p->x,
	   flatten == 1 ? avg : p->y,
	   flatten == 2 ? avg : p->z);
}

static void foreach_vertex (GtsVertex * v, gpointer * info) 
{
  FILE * fp = info[0];
  GHashTable * hash = info[1];
  guint * nv = info[2];
  GtsSurface * s = info[4];

  write_point (GTS_POINT (v), fp);
  switch (color) {
  case EPV: {
    Color c = colormap_color (colormap,
		((gdouble)g_slist_length (v->segments) - min)/(max - min));
    fprintf (fp, " %g %g %g 1.0\n", c.r, c.g, c.b);
    break;
  }
  case HEIGHT: {
    Color c = colormap_color (colormap,
	       ((&GTS_POINT (v)->x)[height] - avg - min)/(max - min));
    fprintf (fp, " %g %g %g 1.0\n", c.r, c.g, c.b);
    break;
  }
  case CURVATURE: {
    gdouble c = 0.;
    Color color;
    GtsVector n;

    if (!gts_vertex_is_boundary (v, s) &&
	gts_vertex_mean_curvature_normal (v, s, n)) 
      c = gts_vector_norm (n)/2.;
    color = colormap_color (colormap, (c - min)/(max - min));
    fprintf (fp, " %g %g %g 1.0\n", color.r, color.g, color.b);
    break;
  }
  case GAUSSIAN: {
    gdouble gc = 0.;
    Color color;

    if (!gts_vertex_is_boundary (v, s))
      gts_vertex_gaussian_curvature (v, s, &gc);
    color = colormap_color (colormap, (gc - min)/(max - min));
    fprintf (fp, " %g %g %g 1.0\n", color.r, color.g, color.b);
    break;
  }
  default:
    fprintf (fp, "\n");
  }
  g_hash_table_insert (hash, v, GUINT_TO_POINTER (++(*nv)));
}

static void foreach_boundary_edge (GtsEdge * e, gpointer info)
{
  FILE * fp = info;

  if (gts_edge_is_boundary (e, NULL)) {
    fputs ("VECT 1 2 0 2 0 ", fp);
    write_point (GTS_POINT (GTS_SEGMENT (e)->v1), fp);
    fputc (' ', fp);
    write_point (GTS_POINT (GTS_SEGMENT (e)->v2), fp);
    fputc ('\n', fp);
  }
}

static void foreach_feature_edge (GtsEdge * e, gdouble * angle)
{
  if (g_slist_length (e->triangles) == 2 &&
      fabs (gts_triangles_angle (e->triangles->data, e->triangles->next->data))
      < *angle) {
    fputs ("VECT 1 2 0 2 0 ", stdout);
    write_point (GTS_POINT (GTS_SEGMENT (e)->v1), stdout);
    fputc (' ', stdout);
    write_point (GTS_POINT (GTS_SEGMENT (e)->v2), stdout);
    fputc ('\n', stdout);
  }
}

static void foreach_non_manifold_edge (GtsEdge * e, gpointer info)
{
  FILE * fp = info;

  if (g_slist_length (e->triangles) > 2) {
    fputs ("VECT 1 2 0 2 0 ", fp);
    write_point (GTS_POINT (GTS_SEGMENT (e)->v1), fp);
    fputc (' ', fp);
    write_point (GTS_POINT (GTS_SEGMENT (e)->v2), fp);
    fputc ('\n', fp);
  }
}

static void foreach_duplicate_edge (GtsSegment * s, gpointer info)
{
  FILE * fp = info;

  if (gts_segment_is_duplicate (s)) {
    fputs ("VECT 1 2 0 2 0 ", fp);
    write_point (GTS_POINT (s->v1), fp);
    fputc (' ', fp);
    write_point (GTS_POINT (s->v2), fp);
    fputc ('\n', fp);
  }
}

static void foreach_triangle (GtsTriangle * t, gpointer * info)
{
  FILE * fp = info[0];
  GHashTable * hash = info[1];
  guint * nfold = info[3];
  guint p1 = 0, p2 = 0, p3 = 0;

  p1 = GPOINTER_TO_UINT (g_hash_table_lookup (hash, GTS_SEGMENT (t->e1)->v1));
  if (GTS_SEGMENT (t->e1)->v1 == GTS_SEGMENT (t->e2)->v1) {
    p2 = GPOINTER_TO_UINT (g_hash_table_lookup (hash, 
						GTS_SEGMENT (t->e2)->v2));
    p3 = GPOINTER_TO_UINT (g_hash_table_lookup (hash, 
						GTS_SEGMENT (t->e1)->v2));
    }
  else if (GTS_SEGMENT (t->e1)->v2 == GTS_SEGMENT (t->e2)->v2) {
    p2 = GPOINTER_TO_UINT (g_hash_table_lookup (hash, 
						GTS_SEGMENT (t->e1)->v2));
    p3 = GPOINTER_TO_UINT (g_hash_table_lookup (hash, 
						GTS_SEGMENT (t->e2)->v1));
  }
  else if (GTS_SEGMENT (t->e1)->v1 == GTS_SEGMENT (t->e2)->v2) {
    p2 = GPOINTER_TO_UINT (g_hash_table_lookup (hash, 
						GTS_SEGMENT (t->e2)->v1));
    p3 = GPOINTER_TO_UINT (g_hash_table_lookup (hash, 
						GTS_SEGMENT (t->e1)->v2));
  }
  else if (GTS_SEGMENT (t->e1)->v2 == GTS_SEGMENT (t->e2)->v1) {
    p2 = GPOINTER_TO_UINT (g_hash_table_lookup (hash, 
						GTS_SEGMENT (t->e1)->v2));
    p3 = GPOINTER_TO_UINT (g_hash_table_lookup (hash, 
						GTS_SEGMENT (t->e2)->v2));
  }
  else
    g_assert_not_reached ();
  g_return_if_fail (p1 && p2 && p3);
  fprintf (fp, "3 %u %u %u", p1 - 1, p2 - 1, p3 - 1);
  switch (color) {
  case QUALITY: {
    Color c = colormap_color (colormap, 
			      (gts_triangle_quality (t) - min)/(max - min));
    fprintf (fp, " %g %g %g\n", c.r, c.g, c.b);
    break;
  }
  case AREA: {
    Color c = colormap_color (colormap, 
			      (gts_triangle_area (t) - min)/(max - min));
    fprintf (fp, " %g %g %g\n", c.r, c.g, c.b);
    break;
  }
  case INCOMP: {
    gboolean compatible = TRUE;
    GtsEdge * e1 = t->e1, * e2 = t->e2, * e3 = t->e3;
    GSList * i = e1->triangles;
    while (compatible && i) {
      GtsTriangle * t1 = i->data;
      if (t1 != t && !gts_triangles_are_compatible (t, t1, e1))
	compatible = FALSE;
      i = i->next;
    }
    i = e2->triangles;
    while (compatible && i) {
      GtsTriangle * t1 = i->data;
      if (t1 != t && !gts_triangles_are_compatible (t, t1, e2))
	compatible = FALSE;
      i = i->next;
    }
    i = e3->triangles;
    while (compatible && i) {
      GtsTriangle * t1 = i->data;
      if (t1 != t && !gts_triangles_are_compatible (t, t1, e3))
	compatible = FALSE;
      i = i->next;
    }
    if (!compatible)
      fputs (" 1. 0. 0.", fp);
    fputc ('\n', fp);
    break;
  }
  case FOLD: {
    gboolean fold = FALSE;
    GtsEdge * e1 = t->e1, * e2 = t->e2, * e3 = t->e3;
    fold = gts_triangles_are_folded (e1->triangles, 
				     GTS_SEGMENT (e1)->v1,
				     GTS_SEGMENT (e1)->v2,
				     maxcosine2);
    if (!fold)
      fold = gts_triangles_are_folded (e2->triangles, 
				       GTS_SEGMENT (e2)->v1,
				       GTS_SEGMENT (e2)->v2,
				       maxcosine2);
    if (!fold)
      fold = gts_triangles_are_folded (e3->triangles, 
				       GTS_SEGMENT (e3)->v1,
				       GTS_SEGMENT (e3)->v2,
				       maxcosine2);      
    if (fold) {
      (*nfold)++;
      fputs (" 1. 0. 0.", fp);
    }
    fputc ('\n', fp);
  }
  default:
    fputc ('\n', fp);
  }
}

static void min_max_curvature (GtsVertex * v, gpointer * data)
{
  gdouble * min = data[0];
  gdouble * max = data[1];
  GtsSurface * s = data[2];
  GtsVector n;
  gdouble c = 0.;

  if (!gts_vertex_is_boundary (v, s) && 
      gts_vertex_mean_curvature_normal (v, s, n)) 
    c = gts_vector_norm (n)/2.;
  if (c < *min) *min = c;
  if (c > *max) *max = c;
}

static void min_max_gaussian (GtsVertex * v, gpointer * data)
{
  gdouble * min = data[0];
  gdouble * max = data[1];
  GtsSurface * s = data[2];
  gdouble gc = 0.;

  if (!gts_vertex_is_boundary (v, s))
    gts_vertex_gaussian_curvature (v, s, &gc);
  if (gc < *min) *min = gc;
  if (gc > *max) *max = gc;
}

static void oogl_surface (GtsSurface * s, FILE * fptr)
{
  gpointer info[5];
  GtsSurfaceStats stats;
  GtsSurfaceQualityStats qstats;
  guint np = 0, nfold = 0;

  g_return_if_fail (s != NULL);
  g_return_if_fail (fptr != NULL);

  info[0] = fptr;
  info[1] = g_hash_table_new (NULL, NULL);
  info[2] = &np;
  info[3] = &nfold;
  info[4] = s;

  gts_surface_stats (s, &stats);
  switch (color) {
  case AREA:
    if (autoscale) {
      gts_surface_quality_stats (s, &qstats);
      min = qstats.face_area.min;
      max = qstats.face_area.max;
    }
    break;
  case QUALITY:
    if (autoscale) {
      gts_surface_quality_stats (s, &qstats);
      min = qstats.face_quality.min;
      max = qstats.face_quality.max;
    }
    break;
  case EPV:
    if (autoscale) {
      min = stats.edges_per_vertex.min;
      max = stats.edges_per_vertex.max;
    }
    break;
  case HEIGHT:
    if (autoscale) {
      GtsBBox * bbox = gts_bbox_surface (gts_bbox_class (), s);

      min = (&bbox->x1)[height] - avg;
      max = (&bbox->x2)[height] - avg;
      gts_object_destroy (GTS_OBJECT (bbox));
    }
    break;
  case CURVATURE:
    if (autoscale) {
      gpointer data[3];

      min = G_MAXDOUBLE;
      max = - G_MAXDOUBLE;
      data[0] = &min;
      data[1] = &max;
      data[2] = s;
      gts_surface_foreach_vertex (s, (GtsFunc) min_max_curvature, data);
    }
    break;
  case GAUSSIAN:
    if (autoscale) {
      gpointer data[3];

      min = G_MAXDOUBLE;
      max = - G_MAXDOUBLE;
      data[0] = &min;
      data[1] = &max;
      data[2] = s;
      gts_surface_foreach_vertex (s, (GtsFunc) min_max_gaussian, data);
    }
    break;
  default:
    ;
  }
  if (verbose)
    fprintf (stderr, "scalar min: %g max: %g\n", min, max);
  if (max == min)
    max = min + 1.;
  if (color == EPV || color == HEIGHT || 
      color == CURVATURE || color == GAUSSIAN)
    fputs ("COFF ", fptr);
  else
    fputs ("OFF ", fptr);
  fprintf (fptr, "%u %u %u\n",
	   stats.edges_per_vertex.n, 
	   stats.n_faces,
	   stats.faces_per_edge.n);
  gts_surface_foreach_vertex (s, (GtsFunc) foreach_vertex, info);
  gts_surface_foreach_face (s, (GtsFunc) foreach_triangle, info);
  if (verbose && color == FOLD)
    fprintf (stderr, "# Number of folds: %u\n", nfold);

  g_hash_table_destroy (info[1]);
}

static GtsVertex * next_vertex (GtsVertex * v)
{
  GSList * j = v->segments;

  while (j) {
    GtsSegment * s = j->data;

    if (GTS_OBJECT (s)->reserved == s) {
      GTS_OBJECT (s)->reserved = NULL;
      return s->v1 != v ? s->v1 : s->v2;
    }
    j = j->next;
  }

  return NULL;
}

static GSList * chain_segments (GSList * segments)
{
  GSList * chains = NULL, * i;
  
  i = segments;
  while (i) {
    GTS_OBJECT (i->data)->reserved = i->data;
    i = i->next;
  }

  i = segments;
  while (i) {
    GtsSegment * s = i->data;

    if (GTS_OBJECT (s)->reserved == s) {
      GSList * chain = g_slist_prepend (NULL, s->v1);
      GtsVertex * v = s->v2;

      GTS_OBJECT (s)->reserved = NULL;
      while (v) {
	chain = g_slist_prepend (chain, v);
	v = next_vertex (v);
      }
      chains = g_slist_prepend (chains, chain);
    }
    i = i->next;
  }

  return chains;
}

static void write_isolines (GtsSurface * surface, 
			    GNode * stree,
			    gdouble level,
			    FILE * fp,
			    gboolean gnuplot)
{
  GNode * ptree;
  GtsSurface * plane;
  GtsVertex * v1, * v2, * v3;
  GSList * inter, * chains, * i;
  guint nvertices = 0;

  g_return_if_fail (surface != NULL);
  g_return_if_fail (stree != NULL);
  g_return_if_fail (fp != NULL);

  plane = gts_surface_new (gts_surface_class (),
			   gts_face_class (),
			   gts_edge_class (),
			   gts_vertex_class ());
  v1 = gts_vertex_new (gts_vertex_class (), -1e10, -1e10, level);
  v2 = gts_vertex_new (gts_vertex_class (), 1e10, -1e10, level);
  v3 = gts_vertex_new (gts_vertex_class (), 0., 1e10, level);
  gts_surface_add_face (plane, gts_face_new (gts_face_class(),
			       gts_edge_new (gts_edge_class (), v1, v2),
			       gts_edge_new (gts_edge_class (), v2, v3),
			       gts_edge_new (gts_edge_class (), v3, v1)));
  ptree = gts_bb_tree_surface (plane);
  inter = gts_surface_intersection (surface, plane, stree, ptree);
  chains = chain_segments (inter);

  i = chains;
  while (i) {
    GSList * chain = i->data;
    nvertices += g_slist_length (chain);
    i = i->next;
  }

  if (!gnuplot) {
    fprintf (fp, "VECT %d %d 0\n", g_slist_length (chains), nvertices);
    i = chains;
    while (i) {
      GSList * chain = i->data;
      
      fprintf (fp, "%d ", g_slist_length (chain));
      i = i->next;
    }
    i = chains;
    while (i) {
      fprintf (fp, "0 ");
      i = i->next;
    }
    fprintf (fp, "\n");
  }
  i = chains;
  while (i) {
    GSList * chain = i->data;
    GSList * j = chain;
    
    while (j) {
      GtsPoint * p = j->data;

      write_point (p, fp);
      fputc ('\n', fp);
      j = j->next;
    }
    if (gnuplot)
      fputc ('\n', fp);
    g_slist_free (chain);
    i = i->next;
  }
  g_slist_free (chains);
  g_slist_free (inter);
  gts_bb_tree_destroy (ptree, TRUE);
  gts_object_destroy (GTS_OBJECT (plane));
}

static void sum_flatten (GtsPoint * p)
{
  avg += (&p->x)[flatten];
}

static void write_face (GtsTriangle * t)
{
  GtsVertex * v1, * v2, * v3;
  GtsPoint o;
 
  gts_triangle_vertices (t, &v1, &v2, &v3);
  o.x = (GTS_POINT (v1)->x + GTS_POINT (v2)->x + GTS_POINT (v3)->x)/3.;
  o.y = (GTS_POINT (v1)->y + GTS_POINT (v2)->y + GTS_POINT (v3)->y)/3.;
  o.z = (GTS_POINT (v1)->z + GTS_POINT (v2)->z + GTS_POINT (v3)->z)/3.;
  printf ("OFF 3 1 3\n"
	  "%g %g %g\n"
	  "%g %g %g\n"
	  "%g %g %g\n"
	  "3 0 1 2\n",
	  o.x + 0.9*(GTS_POINT (v1)->x - o.x),
	  o.y + 0.9*(GTS_POINT (v1)->y - o.y),
	  o.z + 0.9*(GTS_POINT (v1)->z - o.z),
	  o.x + 0.9*(GTS_POINT (v2)->x - o.x),
	  o.y + 0.9*(GTS_POINT (v2)->y - o.y),
	  o.z + 0.9*(GTS_POINT (v2)->z - o.z),
	  o.x + 0.9*(GTS_POINT (v3)->x - o.x),
	  o.y + 0.9*(GTS_POINT (v3)->y - o.y),
	  o.z + 0.9*(GTS_POINT (v3)->z - o.z));
}

int main (int argc, char * argv[])
{
  GtsSurface * surface;
  gboolean nosurface = FALSE;
  int c = 0;
  guint isolines = 0;
  GtsFile * fp;
  gdouble feature = 0.;
  gdouble isoline[256];
  guint niso = 0;
  gboolean gnuplot = FALSE;

  colormap = colormap_jet (); /* default */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"gnuplot", no_argument, NULL, 'G'},
      {"faces", no_argument, NULL, 't'},
      {"isoline", required_argument, NULL, 'I'},
      {"gaussian", no_argument, NULL, 'g'},
      {"curvature", no_argument, NULL, 'C'},
      {"feature", required_argument, NULL, 'e'},
      {"flatten", required_argument, NULL, 'F'},
      {"component", no_argument, (int *)&color, COMPONENT},
      {"quality", no_argument, (int *)&color, QUALITY},
      {"area", no_argument, (int *)&color, AREA},
      {"incomp", no_argument, (int *)&color, INCOMP},
      {"fold", required_argument, NULL, 'f'},
      {"epv", no_argument, (int *)&color, EPV},
      {"height", required_argument, NULL, 'H'},
      {"boundary", no_argument, &boundary, TRUE},
      {"non-manifold", no_argument, &non_manifold, TRUE},
      {"duplicate", no_argument, &duplicate, TRUE},
      {"isolines", required_argument, NULL, 'i'},
      {"cmap", required_argument, NULL, 'c'},
      {"help", no_argument, NULL, 'h'},
      {"min", required_argument, NULL, 'm'},
      {"max", required_argument, NULL, 'M'},
      {"nosurface", no_argument, NULL, 'n'},
      {"reverse", no_argument, NULL, 'r'},
      {"verbose", no_argument, NULL, 'v'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "c:hm:M:rvf:i:nF:e:H:CgI:tG",
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "c:hm:M:rvf:i:nF:e:H:CgI:tG"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'G': /* gnuplot */
      gnuplot = TRUE;
      break;
    case 't': /* faces */
      faces = TRUE;
      break;
    case 'I': /* isoline */
      isoline[niso++] = atof (optarg);
      break;
    case 'g': /* gaussian */
      color = GAUSSIAN;
      break;
    case 'C': /* curvature */
      color = CURVATURE;
      break;
    case 'H': /* height */
      color = HEIGHT;
      height = atoi (optarg);
      if (height > 2)
	height = 2;
      break;
    case 'e': /* feature */
      feature = atof (optarg)*PI/180.;
      break;
    case 'F': /* flatten */
      flatten = atoi (optarg);
      if (flatten < 0.) flatten = 0;
      else if (flatten > 2) flatten = 2;
      break;
    case 'c': { /* colormap file */
      FILE * fptr = fopen (optarg, "rt");
      if (!fptr) {
	fprintf (stderr, "%s: cannot open colormap file `%s'.\n",
		 argv[0], optarg);
	return 1;
      }
      colormap = colormap_read (fptr);
      fclose (fptr);
      break;
    }
    case 'i': /* isolines */
      isolines = atoi (optarg);
      break;
    case 'f': /* fold */
      color = FOLD;
      maxcosine2 = cos (atof (optarg)*PI/180.);
      maxcosine2 *= maxcosine2;
      break;
    case 'm': /* minimum value */
      autoscale = FALSE;
      min = atof (optarg);
      break;
    case 'M': /* maximum value */
      autoscale = FALSE;
      max = atof (optarg);
      break;
    case 'n': /* nosurface */
      nosurface = TRUE;
      break;
    case 'r': /* reverse colormap */
      colormap->reversed = TRUE;
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: gts2oogl [OPTION]... < input.srf > output.oogl\n"
	     "Convert a GTS file (surface file format generated by the Gts Library)\n"
	     "to OOGL file format (Geomview).\n"
	     "\n"
             "  -G      --gnuplot      writes isolines in gnuplot format\n"   
             "          --components   color faces according to the component\n"             "                         they belong too\n"
	     "          --quality      color faces according to their quality\n"
	     "          --area         color faces according to their area\n"
	     "          --incomp       color incompatible faces\n"
	     "  -f VAL  --fold=VAL     color faces which make an angle smaller\n" 
	     "                         than VAL degrees with any of their neighbors\n"
             "  -t      --faces        output individual faces\n"
	     "          --epv          color vertices according to number of edges per vertex\n"
	     "  -H C    --height=C     color vertices according to their C coordinate\n"
	     "  -g      --gaussian     color vertices according to Gaussian curvature\n"
	     "  -C      --curvature    color vertices according to mean curvature\n"
	     "          --boundary     output boundary edges\n"
             "  -e A    --feature=A    output `feature' edges defined by angle A\n"
             "          --non-manifold output non-manifold edges\n"
             "          --duplicate    output duplicate edges\n"
	     "  -i N    --isolines=N   draw N isolines (levels of constant altitude)\n"
             "  -I L    --isolines=L   draw isoline at level L\n"
	     "          --cmap=FILE    load FILE as colormap\n"
	     "  -m VAL, --min=VAL      use VAL as minimum scaling value\n"
	     "  -M VAL, --max=VAL      use VAL as maximum scaling value\n"
	     "  -r,     --reverse      reverse colormap\n"
             "  -n      --nosurface    do not output surface\n"
	     "  -F C    --flatten=C    set C coordinate to average value\n"
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
      fprintf (stderr, "Try `gts2oogl --help' for more information.\n");
      return 1;
    }
  }

  surface = gts_surface_new (gts_surface_class (),
			     gts_face_class (),
			     gts_edge_class (),
			     gts_vertex_class ());
  fp = gts_file_new (stdin);
  if (gts_surface_read (surface, fp)) {
    fputs ("gts2oogl: the file on standard input is not a valid GTS file\n", 
	   stderr);
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1;
  }
  
  if (verbose)
    gts_surface_print_stats (surface, stderr);

  if (flatten >= 0) {
    gts_surface_foreach_vertex (surface, (GtsFunc) sum_flatten, NULL);
    if (avg != 0.)
      avg /= gts_surface_vertex_number (surface);
  }

  if (boundary || non_manifold || duplicate || 
      isolines || feature > 0. || niso > 0) {
    if (!nosurface) {
      puts ("(geometry \"surface\" { = ");
      oogl_surface (surface, stdout);
      puts ("})");
    }
    if (boundary) {
      if (!nosurface || isolines || niso > 0 || non_manifold || duplicate)
	puts ("(geometry \"boundary\" { = ");
      puts ("LIST {");
      gts_surface_foreach_edge (surface, (GtsFunc) foreach_boundary_edge, 
				stdout);
      puts ("}");
      if (!nosurface || isolines || niso > 0 || non_manifold || duplicate)
	puts ("})");
    }
    if (feature > 0.) {
      if (!nosurface || isolines || niso > 0 || non_manifold || duplicate)
	puts ("(geometry \"feature\" { = ");
      puts ("LIST {");
      gts_surface_foreach_edge (surface, (GtsFunc) foreach_feature_edge, 
				&feature);
      puts ("}");
      if (!nosurface || isolines || niso > 0 || non_manifold || duplicate)
	puts ("})");
    }
    if (non_manifold) {
      if (!nosurface || isolines || niso > 0 || boundary || duplicate)
	puts ("(geometry \"non-manifold\" { = ");
      puts ("LIST {");
      gts_surface_foreach_edge (surface, (GtsFunc) foreach_non_manifold_edge, 
				stdout);
      puts ("}");
      if (!nosurface || isolines || niso > 0 || boundary || duplicate)
	puts ("})");
    }
    if (duplicate) {
      if (!nosurface || isolines || niso > 0 || boundary || non_manifold)
	puts ("(geometry \"duplicate\" { = ");
      puts ("LIST {");
      gts_surface_foreach_edge (surface, (GtsFunc) foreach_duplicate_edge, 
				stdout);
      puts ("}");
      if (!nosurface || isolines || niso > 0 || boundary || non_manifold)
	puts ("})");
    }
    if (isolines > 0) {
      GNode * stree;
      gdouble z, dz;
      GtsBBox * bbox;
      guint i;

      stree = gts_bb_tree_surface (surface);
      bbox = stree->data;
      dz = (bbox->z2 - bbox->z1)/(isolines + 1);
      z = bbox->z1 + dz;
      if (!nosurface || boundary)
	puts ("(geometry \"isolines\" { = ");
      if (!gnuplot)
	puts ("LIST {");
      for (i = 0; i < isolines; i++, z += dz)
	write_isolines (surface, stree, z, stdout, gnuplot);
      if (!gnuplot)
	puts ("}");
      if (!nosurface || boundary)
	puts ("})\n");
      gts_bb_tree_destroy (stree, TRUE);
    }
    else if (niso > 0) {
      GNode * stree;
      GtsBBox * bbox;
      guint i;

      stree = gts_bb_tree_surface (surface);
      bbox = stree->data;
      if (!nosurface || boundary)
	puts ("(geometry \"isolines\" { = ");
      if (!gnuplot)
	puts ("LIST {");
      for (i = 0; i < niso; i++)
	write_isolines (surface, stree, isoline[i], stdout, gnuplot);
      if (!gnuplot)
	puts ("}");
      if (!nosurface || boundary)
	puts ("})\n");
      gts_bb_tree_destroy (stree, TRUE);
    }
  }
  else if (color == COMPONENT) {
    GSList * i = gts_surface_split (surface);

    if (verbose)
      fprintf (stderr, "%u components\n", g_slist_length (i));

    printf ("LIST {\n");
    while (i) {
      gdouble 
	r = rand()/(gdouble)RAND_MAX, 
	g = rand()/(gdouble)RAND_MAX, 
	b = rand()/(gdouble)RAND_MAX;

      printf ("geom { appearance { material {\n"
	      "  ambient %g %g %g\n"
	      "  diffuse %g %g %g\n"
	      "}}\n", 
	      r, g, b, r, g, b);
      gts_surface_write_oogl (i->data, stdout);
      printf ("}\n");
      i = i->next;
    }
    printf ("}\n");
  }
  else if (!nosurface) {
    if (min == max) 
      max = min + 1.;
    if (faces) {
      printf ("LIST {\n");
      gts_surface_foreach_face (surface, (GtsFunc) write_face, NULL);
      printf ("}\n");
    }      
    else	
      oogl_surface (surface, stdout);
  }

  return 0;
}
