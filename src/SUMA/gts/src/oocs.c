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
#include "gts.h"

static void cluster_destroy (GtsObject * object)
{
  GtsCluster * c = GTS_CLUSTER (object);

  if (c->v && gts_vertex_is_unattached (c->v))
    gts_object_destroy (GTS_OBJECT (c->v));

  /* do not forget to call destroy method of the parent */
  (* GTS_OBJECT_CLASS (gts_cluster_class ())->parent_class->destroy) (object);
}

static void cluster_add (GtsCluster * c, GtsPoint * p, gpointer data)
{
  GtsPoint * cp;

  g_return_if_fail (c != NULL);
  g_return_if_fail (c->v != NULL);
  g_return_if_fail (p != NULL);

  cp = GTS_POINT (c->v);
  
  cp->x += p->x;
  cp->y += p->y;
  cp->z += p->z;
  c->n++;
}

static void cluster_update (GtsCluster * c)
{
  GtsPoint * p;

  g_return_if_fail (c != NULL);
  g_return_if_fail (c->v != NULL);

  if (c->n > 1) {
    p = GTS_POINT (c->v);
    p->x /= c->n;
    p->y /= c->n;
    p->z /= c->n;
  }
}

static void cluster_class_init (GtsClusterClass * klass)
{
  klass->add = cluster_add;
  klass->update = cluster_update;

  GTS_OBJECT_CLASS (klass)->destroy = cluster_destroy;
}

static void cluster_init (GtsCluster * c)
{
  c->v = NULL;
  c->n = 0;
}

/**
 * gts_cluster_class:
 *
 * Returns: the #GtsClusterClass.
 */
GtsClusterClass * gts_cluster_class (void)
{
  static GtsClusterClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo cluster_info = {
      "GtsCluster",
      sizeof (GtsCluster),
      sizeof (GtsClusterClass),
      (GtsObjectClassInitFunc) cluster_class_init,
      (GtsObjectInitFunc) cluster_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), &cluster_info);
  }

  return klass;
}

/**
 * gts_cluster_new:
 * @klass: a #GtsClusterClass.
 * @id: the id of the new cluster.
 * @vklass: a #GtsVertexClass for the representative vertex of the cluster.
 *
 * Returns: a new #GtsCluster.
 */
GtsCluster * gts_cluster_new (GtsClusterClass * klass,
			      GtsClusterId id,
			      GtsVertexClass * vklass)
{
  GtsCluster * c;

  c = GTS_CLUSTER (gts_object_new (GTS_OBJECT_CLASS (klass)));
  c->id = id;
  c->v = gts_vertex_new (vklass, 0., 0., 0.);

  return c;
}

/**
 * gts_cluster_add:
 * @c: a #GtsCluster.
 * @p: a #GtsPoint.
 * @data: data to pass to the add() virtual method of #GtsClusterClass.
 *
 * Adds point @p to cluster @c.
 */
void gts_cluster_add (GtsCluster * c, GtsPoint * p, gpointer data)
{
  g_return_if_fail (c != NULL);
  g_return_if_fail (p != NULL);

  (* GTS_CLUSTER_CLASS (GTS_OBJECT (c)->klass)->add) (c, p, data);
}

/**
 * gts_cluster_update:
 * @c: a #GtsCluster.
 *
 * Updates the position of the vertex representative of all the
 * vertices added to @c.  
 */
void gts_cluster_update (GtsCluster * c)
{
  g_return_if_fail (c != NULL);

  (* GTS_CLUSTER_CLASS (GTS_OBJECT (c)->klass)->update) (c);
}

static void destroy_cluster (GtsClusterId * id, GtsObject * cluster)
{
  gts_object_destroy (cluster);
}

static void cluster_grid_destroy (GtsObject * object)
{
  GtsClusterGrid * cluster_grid = GTS_CLUSTER_GRID (object);

  g_hash_table_foreach (cluster_grid->clusters, 
			(GHFunc) destroy_cluster, NULL);
  g_hash_table_destroy (cluster_grid->clusters);
  
  (* GTS_OBJECT_CLASS (gts_cluster_grid_class ())->parent_class->destroy) 
    (object);
}

static void cluster_grid_class_init (GtsClusterGridClass * klass)
{
  GTS_OBJECT_CLASS (klass)->destroy = cluster_grid_destroy;
}

static gint cluster_id_equal (gconstpointer v1,
			      gconstpointer v2)
{
  const GtsClusterId * id1 = (const GtsClusterId *) v1;
  const GtsClusterId * id2 = (const GtsClusterId *) v2;
  return ((id1->x == id2->x) && (id1->y == id2->y) && (id1->z == id2->z));
}

static guint cluster_id_hash (gconstpointer key)
{
  const GtsClusterId * id = (const GtsClusterId *) key;
  return id->x + id->y + id->z;
}

static void cluster_grid_init (GtsClusterGrid * cluster_grid)
{
  cluster_grid->surface = NULL;
  cluster_grid->bbox = NULL;
  cluster_grid->cluster_class = gts_cluster_class ();
  cluster_grid->clusters = g_hash_table_new (cluster_id_hash,
					      cluster_id_equal);
}

/**
 * gts_cluster_grid_class:
 *
 * Returns: the #GtsClusterGridClass.
 */
GtsClusterGridClass * gts_cluster_grid_class (void)
{
  static GtsClusterGridClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo cluster_grid_info = {
      "GtsClusterGrid",
      sizeof (GtsClusterGrid),
      sizeof (GtsClusterGridClass),
      (GtsObjectClassInitFunc) cluster_grid_class_init,
      (GtsObjectInitFunc) cluster_grid_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), &cluster_grid_info);
  }

  return klass;
}

/**
 * gts_cluster_grid_new:
 * @klass: a #GtsClusterGridClass.
 * @cluster_class: the klass to be used for the vertex clusters.
 * @s: the simplified surface.
 * @bbox: bounding box of the surface to be simplified.
 * @delta: the size of one grid cell of the simplification grid.
 *
 * Returns: a new #GtsClusterGrid.
 */
GtsClusterGrid * gts_cluster_grid_new (GtsClusterGridClass * klass,
				       GtsClusterClass * cluster_class,
				       GtsSurface * s,
				       GtsBBox * bbox,
				       gdouble delta)
{
  GtsClusterGrid * cluster_grid;
  GtsVector size;

  g_return_val_if_fail (klass != NULL, NULL);
  g_return_val_if_fail (cluster_class != NULL, NULL);
  g_return_val_if_fail (s != NULL, NULL);
  g_return_val_if_fail (bbox != NULL, NULL);
  g_return_val_if_fail (delta > 0., NULL);

  size[0] = ceil ((bbox->x2 - bbox->x1)/delta);
  size[1] = ceil ((bbox->y2 - bbox->y1)/delta);
  size[2] = ceil ((bbox->z2 - bbox->z1)/delta);
  g_return_val_if_fail (size[0] <= 2.*G_MAXINT + 2. &&
			size[1] <= 2.*G_MAXINT + 2. &&
			size[2] <= 2.*G_MAXINT + 2., NULL);
  cluster_grid = 
    GTS_CLUSTER_GRID (gts_object_new (GTS_OBJECT_CLASS (klass)));
  cluster_grid->cluster_class = cluster_class;
  cluster_grid->surface = s;
  cluster_grid->bbox = bbox;
  cluster_grid->size[0] = size[0];
  cluster_grid->size[1] = size[1];
  cluster_grid->size[2] = size[2];

  return cluster_grid;
}

static GtsClusterId cluster_index (GtsPoint * p,
				   GtsBBox * bb,
				   GtsVector n)
{
  GtsClusterId id = {0, 0, 0};
  
  g_return_val_if_fail (p->x >= bb->x1 && p->x <= bb->x2, id);
  g_return_val_if_fail (p->y >= bb->y1 && p->y <= bb->y2, id);
  g_return_val_if_fail (p->z >= bb->z1 && p->z <= bb->z2, id);
  
  id.x = (guint) (p->x == bb->x2 ? n[0] - 1. : n[0]*(p->x - bb->x1)/(bb->x2 - bb->x1));
  id.y = (guint) (p->y == bb->y2 ? n[1] - 1. : n[1]*(p->y - bb->y1)/(bb->y2 - bb->y1));
  id.z = (guint) (p->z == bb->z2 ? n[2] - 1. : n[2]*(p->z - bb->z1)/(bb->z2 - bb->z1));

  return id;
}

static GtsCluster * cluster_grid_add_point (GtsClusterGrid * cluster_grid,
					    GtsPoint * p,
					    gpointer data)
{
  GtsClusterId id = cluster_index (p, 
				   cluster_grid->bbox, 
				   cluster_grid->size);
  GtsCluster * c = g_hash_table_lookup (cluster_grid->clusters, &id);

  if (c == NULL) {
    c = gts_cluster_new (cluster_grid->cluster_class, id, 
			 cluster_grid->surface->vertex_class);
    g_hash_table_insert (cluster_grid->clusters, &c->id, c);
  }
  
  gts_cluster_add (c, p, data);
  
  return c;
}

/**
 * gts_cluster_grid_add_triangle:
 * @cluster_grid: a #GtsClusterGrid.
 * @p1: a #GtsPoint.
 * @p2: a #GtsPoint.
 * @p3: a #GtsPoint.
 * @data: user data to pass to the cluster add() method.
 *
 * Adds the triangle defined by @p1, @p2 and @p3 to the respective clusters
 * of @cluster_grid.
 */
void gts_cluster_grid_add_triangle (GtsClusterGrid * cluster_grid,
				    GtsPoint * p1,
				    GtsPoint * p2,
				    GtsPoint * p3,
				    gpointer data)
{
  GtsCluster * c1, * c2, * c3;

  g_return_if_fail (cluster_grid != NULL);
  g_return_if_fail (p1 != NULL);
  g_return_if_fail (p2 != NULL);
  g_return_if_fail (p3 != NULL);
  g_return_if_fail (cluster_grid->surface != NULL);

  c1 = cluster_grid_add_point (cluster_grid, p1, data);
  c2 = cluster_grid_add_point (cluster_grid, p2, data);
  c3 = cluster_grid_add_point (cluster_grid, p3, data);
  
  if (c1 != c2 && c2 != c3 && c3 != c1) {
    GtsVertex * v1, * v2, * v3;
    GtsEdge * e1, * e2, * e3;
    gboolean new_edge = FALSE;
    
    v1 = c1->v; v2 = c2->v; v3 = c3->v;

    if ((e1 = GTS_EDGE (gts_vertices_are_connected (v1, v2))) == NULL) {
      e1 = gts_edge_new (cluster_grid->surface->edge_class, v1, v2);
      new_edge = TRUE;
    }
    if ((e2 = GTS_EDGE (gts_vertices_are_connected (v2, v3))) == NULL) {
      e2 = gts_edge_new (cluster_grid->surface->edge_class, v2, v3);
      new_edge = TRUE;
    }
    if ((e3 = GTS_EDGE (gts_vertices_are_connected (v3, v1))) == NULL) {
      e3 = gts_edge_new (cluster_grid->surface->edge_class, v3, v1);
      new_edge = TRUE;
    }
    if (new_edge || !gts_triangle_use_edges (e1, e2, e3))
      gts_surface_add_face (cluster_grid->surface, 
			    gts_face_new (cluster_grid->surface->face_class, 
					  e1, e2, e3));
  }
}

static void update_cluster (gint * id, GtsCluster * cluster, GtsRange * stats)
{
  gts_cluster_update (cluster);
  gts_range_add_value (stats, cluster->n);
}

/**
 * gts_cluster_grid_update:
 * @cluster_grid: a #GtsClusterGrid.
 *
 * Updates the representative vertices of all the clusters of @cluster_grid.
 *
 * Returns: a #GtsRange describing the statistics for the number of vertices
 * added to each cluster of @cluster_grid.
 */
GtsRange gts_cluster_grid_update (GtsClusterGrid * cluster_grid)
{
  GtsRange stats;

  gts_range_init (&stats);
  g_return_val_if_fail (cluster_grid != NULL, stats);

  g_hash_table_foreach (cluster_grid->clusters, 
			(GHFunc) update_cluster, &stats);
  gts_range_update (&stats);

  return stats;
}
