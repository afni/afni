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

gdouble min = G_MAXDOUBLE, max = - G_MAXDOUBLE;
gboolean logscale = FALSE;

typedef struct {
  gdouble r, g, b;
} Color;

typedef struct {
  GPtrArray * colors;
  gboolean reversed;
} Colormap;

Colormap * colormap = NULL;

static void build_list (GtsTriangle * t, GSList ** list)
{
  *list = g_slist_prepend (*list, gts_bbox_triangle (gts_bbox_class (), t));
}

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

static Colormap * colormap_red_blue (void)
{
  Colormap * cmap = g_malloc (sizeof (Colormap));
  cmap->reversed = FALSE;
  cmap->colors = g_ptr_array_new ();
  g_ptr_array_add (cmap->colors, color_new (1.0, 0.0, 0.0));
  g_ptr_array_add (cmap->colors, color_new (0.0, 0.0, 1.0));
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

static void foreach_vertex (GtsVertex * v, gpointer * info) 
{
  FILE * fp = info[0];
  GHashTable * hash = info[1];
  guint * nv = info[2];
  GNode * tree = info[3];
  gdouble d = sqrt (gts_bb_tree_point_distance (tree, GTS_POINT (v),
			(GtsBBoxDistFunc) gts_point_triangle_distance2,
			NULL));
  Color c;
  
  if (logscale) {
    c = d > 0.0 ? 
      colormap_color (colormap, (log10 (d) - min)/(max - min)) :
      colormap_color (colormap, 0.0);
  }
  else
    c = colormap_color (colormap, (d - min)/(max - min));

  fprintf (fp, "%g %g %g %g %g %g 1.0\n",
	   GTS_POINT (v)->x, GTS_POINT (v)->y, GTS_POINT (v)->z,
	   c.r, c.g, c.b);

  g_hash_table_insert (hash, v, GUINT_TO_POINTER (++(*nv)));
}

static void foreach_triangle (GtsTriangle * t, gpointer * info)
{
  FILE * fp = info[0];
  GHashTable * hash = info[1];
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
  fprintf (fp, "3 %u %u %u\n", p1 - 1, p2 - 1, p3 - 1);
}

static void oogl_surface (GtsSurface * s, FILE * fptr, GNode * tree)
{
  gpointer info[4];
  GtsSurfaceStats stats;
  guint np = 0;

  g_return_if_fail (s != NULL);
  g_return_if_fail (fptr != NULL);

  info[0] = fptr;
  info[1] = g_hash_table_new (NULL, NULL);
  info[2] = &np;
  info[3] = tree;
  
  gts_surface_stats (s, &stats);
  fprintf (fptr, "COFF %u %u %u\n",
	   stats.edges_per_vertex.n, 
	   stats.n_faces,
	   stats.faces_per_edge.n);
  gts_surface_foreach_vertex (s, (GtsFunc)foreach_vertex, info);
  gts_surface_foreach_face (s, (GtsFunc)foreach_triangle, info);

  g_hash_table_destroy (info[1]);
}

static void build_bbox (GtsPoint * p, GtsBBox * bbox)
{
  if (p->x > bbox->x2)
    bbox->x2 = p->x;
  if (p->x < bbox->x1)
    bbox->x1 = p->x;
  if (p->y > bbox->y2)
    bbox->y2 = p->y;
  if (p->y < bbox->y1)
    bbox->y1 = p->y;
  if (p->z > bbox->z2)
    bbox->z2 = p->z;
  if (p->z < bbox->z1)
    bbox->z1 = p->z;
}

int main (int argc, char * argv[])
{
  GtsSurface * s1, * s2;
  GtsSurfaceStats ss1, ss2;
  GtsSurfaceQualityStats sq1, sq2;
  gboolean image = FALSE;
  gboolean symmetric = FALSE;
  int c = 0;
  FILE * fptr;
  GtsFile * fp;
  GtsRange fd1, fd2, bd1, bd2;
  gdouble delta, v1, v2, l1, l2;
  GtsBBox * bbox;

  colormap = colormap_red_blue (); /* default */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"log", no_argument, NULL, 'l'},
      {"symmetric", no_argument, NULL, 's'},
      {"image", no_argument, NULL, 'i'},
      {"cmap", required_argument, NULL, 'c'},
      {"help", no_argument, NULL, 'h'},
      {"min", required_argument, NULL, 'm'},
      {"max", required_argument, NULL, 'M'},
      {"reverse", no_argument, NULL, 'r'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "c:hm:M:risl",
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "c:hm:M:risl"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'l':
      logscale = TRUE;
      break;
    case 's':
      symmetric = TRUE;
      break;
    case 'i': 
      image = TRUE;
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
    case 'm': /* minimum value */
      min = atof (optarg);
      break;
    case 'M': /* maximum value */
      max = atof (optarg);
      break;
    case 'r': /* reverse colormap */
      colormap->reversed = TRUE;
      break;
    case 'h': /* help */
      fprintf (stderr,
	     "Usage: gtscompare [OPTION]... FILE1 FILE2 DELTA\n"
	     "Compare two GTS files. DELTA is the sampling length expressed as\n"
	     "a fraction of the bounding box diagonal of the second surface.\n"
	     "\n"
	     "  -s,     --symmetric    symmetric statistics\n"
	     "  -i,     --image        output visualisation mesh\n"
	     "  -c FILE --cmap=FILE    load FILE as colormap\n"
	     "  -m VAL, --min=VAL      use VAL as minimum scaling value\n"
	     "  -M VAL, --max=VAL      use VAL as maximum scaling value\n"
	     "  -r,     --reverse      reverse colormap\n"
	     "  -l,     --log          use log scale\n"
	     "  -h,     --help         display this help and exit\n"
	     "\n"
	     "Report bugs to %s\n",
	     GTS_MAINTAINER);
      return 0;
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `gtscompare --help' for more information.\n");
      return 1;
    }
  }

  if (optind + 2 >= argc) {
    fprintf (stderr, "gtscompare: missing FILE1, FILE2 or DELTA argument\n");
    fprintf (stderr, "Try `gtscompare --help' for more information.\n");
    return 1;
  }

  delta = atof (argv[optind + 2]);
  if (delta <= 0. || delta >= 1.0) {
    fprintf (stderr, "gtscompare: DELTA must be in ]0,1[\n");
    fprintf (stderr, "Try `gtscompare --help' for more information.\n");
    return 1;
  }

  if ((fptr = fopen (argv[optind], "rt")) == NULL) {
    fprintf (stderr, "gtscompare: %s: No such file or directory\n", 
	     argv[optind]);
    return 1;
  }
  s1 = gts_surface_new (gts_surface_class (),
			gts_face_class (),
			gts_edge_class (),
			gts_vertex_class ());
  fp = gts_file_new (fptr);
  if (gts_surface_read (s1, fp)) {
    fprintf (stderr, "gtscompare: file `%s' is not a valid GTS file\n", 
	     argv[optind]);
    fprintf (stderr, "%s:%d:%d: %s\n", 
	     argv[optind], fp->line, fp->pos, fp->error);
    gts_file_destroy (fp);
    return 1;
  }
  gts_file_destroy (fp);
  fclose (fptr);

  if ((fptr = fopen (argv[optind + 1], "rt")) == NULL) {
    fprintf (stderr, "gtscompare: %s: No such file or directory\n", 
	     argv[optind + 1]);
    return 1;
  }
  s2 = gts_surface_new (gts_surface_class (),
			gts_face_class (),
			gts_edge_class (),
			gts_vertex_class ());
  fp = gts_file_new (fptr);
  if (gts_surface_read (s2, fp)) {
    fprintf (stderr, "gtscompare: file `%s' is not a valid GTS file\n", 
	     argv[optind + 1]);
    fprintf (stderr, "%s:%d:%d: %s\n", 
	     argv[optind + 1], fp->line, fp->pos, fp->error);
    gts_file_destroy (fp);
    return 1;
  }
  gts_file_destroy (fp);
  fclose (fptr);

  gts_surface_stats (s1, &ss1);
  gts_surface_stats (s2, &ss2);
  gts_surface_quality_stats (s1, &sq1);
  gts_surface_quality_stats (s2, &sq2);
  gts_surface_distance (s1, s2, delta, &fd1, &bd1);
  v1 = gts_surface_volume (s1);
  v2 = gts_surface_volume (s2);
  bbox = gts_bbox_new (GTS_BBOX_CLASS (gts_bbox_class()),
		       GTS_OBJECT (s1),
		       0., 0., 0., 0., 0., 0.);
  bbox->x1 = bbox->y1 = bbox->z1 = G_MAXDOUBLE;
  bbox->x2 = bbox->y2 = bbox->z2 = - G_MAXDOUBLE;
  gts_surface_foreach_vertex (s1, (GtsFunc) build_bbox, bbox);
  l1 = bbox->x1 == G_MAXDOUBLE ? 0.0 : sqrt (gts_bbox_diagonal2 (bbox));

  bbox->x1 = bbox->y1 = bbox->z1 = G_MAXDOUBLE;
  bbox->x2 = bbox->y2 = bbox->z2 = - G_MAXDOUBLE;
  gts_surface_foreach_vertex (s2, (GtsFunc) build_bbox, bbox);
  l2 = bbox->x1 == G_MAXDOUBLE ? 1.0 : sqrt (gts_bbox_diagonal2 (bbox));
  gts_object_destroy (GTS_OBJECT (bbox));

  fprintf (stderr, 
	   "-------------------------------------------------------------\n"
	   "                             surface 1            surface 2\n"
	   "-------------------------------------------------------------\n"
	   "Vertices:             %16u %20u\n"
	   "Edges:                %16u %20u\n"
	   "Faces:                %16u %20u\n"
	   "------------------------ Topology ---------------------------\n"
	   "Incompatible faces:   %16u %20u\n"
	   "Duplicate faces:      %16u %20u\n"
	   "Duplicate edges:      %16u %20u\n"
	   "Boundary edges:       %16u %20u\n"
	   "Non-manifold edges:   %16u %20u\n"
	   "Edge per vertex avg.: %16.2g %20.2g\n"
	   "Edge per vertex max:  %16g %20g\n"
	   "Faces per edge avg.:  %16.2g %20.2g\n"
	   "Faces per edge max:   %16g %20g\n"
	   "------------------------ Geometry ---------------------------\n"
	   "Volume:               %16.4g %20.4g\n"
	   "Area:                 %16.4g %20.4g\n"
	   "BBox diagonal:        %16.4g %20.4g\n"
	   "Face quality min:     %16.4g %20.4g\n"
	   "Face quality avg.:    %16.4g %20.4g\n"
	   "Face quality max:     %16.4g %20.4g\n"
	   "Face area min:        %16.4g %20.4g\n"
	   "Face area avg.:       %16.4g %20.4g\n"
	   "Face area max:        %16.4g %20.4g\n"
	   "Edge length min:      %16.4g %20.4g\n"
	   "Edge length avg.:     %16.4g %20.4g\n"
	   "Edge length max:      %16.4g %20.4g\n",
	   ss1.edges_per_vertex.n,   ss2.edges_per_vertex.n,
	   ss1.faces_per_edge.n,     ss2.faces_per_edge.n,
	   ss1.n_faces,              ss2.n_faces,
	   ss1.n_incompatible_faces, ss2.n_incompatible_faces,
	   ss1.n_duplicate_faces,    ss2.n_duplicate_faces,
	   ss1.n_duplicate_edges,    ss2.n_duplicate_edges,
	   ss1.n_boundary_edges,     ss2.n_boundary_edges,
	   ss1.n_non_manifold_edges, ss2.n_non_manifold_edges,
	   ss1.edges_per_vertex.mean, ss2.edges_per_vertex.mean,
	   ss1.edges_per_vertex.max, ss2.edges_per_vertex.max,
	   ss1.faces_per_edge.mean,   ss2.faces_per_edge.mean,
	   ss1.faces_per_edge.max,   ss2.faces_per_edge.max,
	   v1,                       v2,
	   gts_surface_area (s1),    gts_surface_area (s2),
	   l1,                       l2,
	   sq1.face_quality.min,     sq2.face_quality.min,
	   sq1.face_quality.mean,    sq2.face_quality.mean,
	   sq1.face_quality.max,     sq2.face_quality.max,
	   sq1.face_area.min,        sq2.face_area.min,
	   sq1.face_area.mean,       sq2.face_area.mean,
	   sq1.face_area.max,        sq2.face_area.max,
	   sq1.edge_length.min,      sq2.edge_length.min,
	   sq1.edge_length.mean,     sq2.edge_length.mean,
	   sq1.edge_length.max,      sq2.edge_length.max);

  if (l2 == 0.0) l2 = 1.0;
  if (l1 == 0.0) l1 = 1.0;
  if (symmetric) {
    gts_surface_distance (s2, s1, delta, &fd2, &bd2);
    fprintf (stderr,
	   "------------------ Distance between faces -------------------\n"
	   "Minimum:     %16.4g (%5.2f%%) %12.4g (%5.2f%%)\n"
	   "Average:     %16.4g (%5.2f%%) %12.4g (%5.2f%%)\n"
	   "Deviation:   %16.4g (%5.2f%%) %12.4g (%5.2f%%)\n"
           "Maximum:     %16.4g (%5.2f%%) %12.4g (%5.2f%%)\n",
	     fd1.min, 100.*fd1.min/l1, fd2.min, 100.*fd2.min/l2, 
	     fd1.mean, 100.*fd1.mean/l1, fd2.mean, 100.*fd2.mean/l2, 
	     fd1.stddev, 100.*fd1.stddev/l1, fd2.stddev, 100.*fd2.stddev/l2, 
	     fd1.max, 100.*fd1.max/l1, fd2.max, 100.*fd2.max/l2);
    if (ss1.n_boundary_edges > 0)
      fprintf (stderr,
	   "---------------- Distance between boundaries ----------------\n"
	   "Minimum:     %16.4g (%5.2f%%) %12.4g (%5.2f%%)\n"
	   "Average:     %16.4g (%5.2f%%) %12.4g (%5.2f%%)\n"
	   "Deviation:   %16.4g (%5.2f%%) %12.4g (%5.2f%%)\n"
           "Maximum:     %16.4g (%5.2f%%) %12.4g (%5.2f%%)\n",
	       bd1.min, 100.*bd1.min/l1, bd2.min, 100.*bd2.min/l2, 
	       bd1.mean, 100.*bd1.mean/l1, bd2.mean, 100.*bd2.mean/l2, 
	       bd1.stddev, 100.*bd1.stddev/l1, bd2.stddev, 100.*bd2.stddev/l2, 
	       bd1.max, 100.*bd1.max/l1, bd2.max, 100.*bd2.max/l2);	     
  }
  else {
    fprintf (stderr,
	   "------------------ Distance between faces -------------------\n"
	   "Minimum:     %16.4g (%5.2f%%)\n"
	   "Average:     %16.4g (%5.2f%%)\n"
	   "Deviation:   %16.4g (%5.2f%%)\n"
           "Maximum:     %16.4g (%5.2f%%)\n",
	     fd1.min, 100.*fd1.min/l1,
	     fd1.mean, 100.*fd1.mean/l1,
	     fd1.stddev, 100.*fd1.stddev/l1,
	     fd1.max, 100.*fd1.max/l1);
    if (ss1.n_boundary_edges > 0)
      fprintf (stderr,
           "---------------- Distance between boundaries ----------------\n"
	   "Minimum:     %16.4g (%5.2f%%)\n"
	   "Average:     %16.4g (%5.2f%%)\n"
	   "Deviation:   %16.4g (%5.2f%%)\n"
           "Maximum:     %16.4g (%5.2f%%)\n",
	       bd1.min, 100.*bd1.min/l1,
	       bd1.mean, 100.*bd1.mean/l1,
	       bd1.stddev, 100.*bd1.stddev/l1,
	       bd1.max, 100.*bd1.max/l1);
  }

  if (image) {
    GSList * bboxes = NULL;
    GNode * tree;
    
    gts_surface_foreach_face (s2, (GtsFunc) build_list, &bboxes);
    tree = gts_bb_tree_new (bboxes);
    g_slist_free (bboxes);
  
    if (min == G_MAXDOUBLE) {
      if (logscale)
	min = fd1.min > 0.0 ? log10 (fd1.min) : -10.;
      else
	min = fd1.min;
    }
    if (max == - G_MAXDOUBLE) {
      if (logscale)
	max = fd1.max > 0.0 ? log10 (fd1.max) : 1.;
      else
	max = fd1.max;
    }
    oogl_surface (s1, stdout, tree);
    gts_bb_tree_destroy (tree, TRUE);
  }

  return 0;
}
