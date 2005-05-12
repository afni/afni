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


static int compare_x (const void * p1, const void * p2) {
  GtsPoint 
    * pp1 = *((gpointer *) p1),
    * pp2 = *((gpointer *) p2);
  if (pp1->x > pp2->x)
    return 1;
  return -1;
}

static int compare_y (const void * p1, const void * p2) {
  GtsPoint
    * pp1 = *((gpointer *) p1),
    * pp2 = *((gpointer *) p2);
  if (pp1->y > pp2->y)
    return 1;
  return -1;
}

static int compare_z (const void * p1, const void * p2) {
  GtsPoint 
    * pp1 = *((gpointer *) p1),
    * pp2 = *((gpointer *) p2);
  if (pp1->z > pp2->z)
    return 1;
  return -1;
}

/**
 * gts_kdtree_new:
 * @points: an array of #GtsPoint.
 * @compare: always %NULL.
 *
 * Note that the order of the points in array @points is modified by this
 * function.
 * 
 * Returns: a new 3D tree for @points.
 */
GNode * gts_kdtree_new (GPtrArray * points, 
			int (*compare) (const void *, const void *))
{
  guint middle;
  GPtrArray array;
  GNode * node;
  GtsPoint * point;

  g_return_val_if_fail (points != NULL, NULL);
  g_return_val_if_fail (points->len > 0, NULL);

  /* sort the points */
  if (compare == compare_x) compare = compare_y;
  else if (compare == compare_y) compare = compare_z;
  else compare = compare_x;
  qsort (points->pdata, points->len, sizeof (gpointer), compare);

  middle = (points->len - 1)/2;
  point = points->pdata[middle];
  node = g_node_new (point);

  if (points->len > 1) {
    array.len = middle;
    if (array.len > 0) {
      array.pdata = points->pdata;
      g_node_prepend (node, gts_kdtree_new (&array, compare));
    }
    else
      g_node_prepend (node, g_node_new (NULL));
    
    array.len = points->len - middle - 1;
    if (array.len > 0) {
      array.pdata = &(points->pdata[middle + 1]);
      g_node_prepend (node, gts_kdtree_new (&array, compare));
    }
    else
      g_node_prepend (node, g_node_new (NULL));
  }

  return node;
}

/**
 * gts_kdtree_range:
 * @tree: a 3D tree.
 * @bbox: a #GtsBBox.
 * @compare: always %NULL.
 *
 * Returns: a list of #GtsPoint belonging to @tree which are inside @bbox.
 */
GSList * gts_kdtree_range (GNode * tree_3d,
			   GtsBBox * bbox,
			   int (*compare) (const void *, const void *))
{
  GSList * list = NULL;
  GtsPoint * p;
  gdouble left, right, v;
  GNode * node;

  g_return_val_if_fail (tree_3d != NULL, NULL);
  g_return_val_if_fail (bbox != NULL, NULL);

  p = tree_3d->data;
  if (p == NULL)
    return NULL;

  if (gts_bbox_point_is_inside (bbox, p))
    list = g_slist_prepend (list, p);

  if (compare == compare_x) {
    left = bbox->y1; right = bbox->y2; v = p->y;
    compare = compare_y;
  }
  else if (compare == compare_y) {
    left = bbox->z1; right = bbox->z2; v = p->z;
    compare = compare_z;
  }
  else {
    left = bbox->x1; right = bbox->x2; v = p->x;
    compare = compare_x;
  }

  if ((node = tree_3d->children)) {
    if (right >= v)
      list = g_slist_concat (list, gts_kdtree_range (node, bbox, compare));
    node = node->next;
    if (left <= v)
      list = g_slist_concat (list, gts_kdtree_range (node, bbox, compare));
  }
  return list;
}

