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
#include <math.h>
#include "gts.h"

int main (int argc, char * argv[])
{
  GtsSurface * s;
  GtsBBox * bbox;
  gdouble delta;
  GtsPoint * p1, * p2, * p3;
  guint nt;
  GtsRange cluster_stats;
  GtsClusterGrid * cluster_grid;
  
  if (argc != 2) {
    fprintf (stderr, "usage: oocs DELTA < infile > outfile\n");
    return 1;
  }

  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());

  bbox = gts_bbox_new (gts_bbox_class (), s, 0., 0., 0., 0., 0., 0.);
  scanf ("%u", &nt);
  scanf ("%lf %lf %lf", &bbox->x1, &bbox->y1, &bbox->z1);
  scanf ("%lf %lf %lf", &bbox->x2, &bbox->y2, &bbox->z2);
  delta = atof (argv[1])*sqrt (gts_bbox_diagonal2 (bbox));

  cluster_grid = gts_cluster_grid_new (gts_cluster_grid_class (), 
				       gts_cluster_class (), 
				       s, bbox, delta);

  p1 = gts_point_new (gts_point_class (), 0., 0., 0.);
  p2 = gts_point_new (gts_point_class (), 0., 0., 0.);
  p3 = gts_point_new (gts_point_class (), 0., 0., 0.);

  while (scanf ("%lf %lf %lf", &p1->x, &p1->y, &p1->z) == 3 &&
	 scanf ("%lf %lf %lf", &p2->x, &p2->y, &p2->z) == 3 &&
	 scanf ("%lf %lf %lf", &p3->x, &p3->y, &p3->z) == 3)
    gts_cluster_grid_add_triangle (cluster_grid, p1, p2, p3, NULL);
  cluster_stats = gts_cluster_grid_update (cluster_grid);

  gts_object_destroy (GTS_OBJECT (p1));
  gts_object_destroy (GTS_OBJECT (p2));
  gts_object_destroy (GTS_OBJECT (p3));
  gts_object_destroy (GTS_OBJECT (cluster_grid));

  fprintf (stderr, "Initial number of triangles: %u\n", nt);
  fprintf (stderr, "%d clusters of size: min: %g avg: %.1f|%.1f max: %g\n",
	   cluster_stats.n,
	   cluster_stats.min, 
	   cluster_stats.mean, cluster_stats.stddev, 
	   cluster_stats.max);
  gts_surface_print_stats (s, stderr);
  gts_surface_write (s, stdout);

  return 0;
}
