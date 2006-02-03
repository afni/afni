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

static void bbox_init (GtsBBox * bbox)
{
  bbox->bounded = NULL;
}

/**
 * gts_bbox_class:
 *
 * Returns: the #GtsBBoxClass.
 */
GtsBBoxClass * gts_bbox_class (void)
{
  static GtsBBoxClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo bbox_info = {
      "GtsBBox",
      sizeof (GtsBBox),
      sizeof (GtsBBoxClass),
      (GtsObjectClassInitFunc) NULL,
      (GtsObjectInitFunc) bbox_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), &bbox_info);
  }

  return klass;
}

/**
 * gts_bbox_set:
 * @bbox: a #GtsBBox.
 * @bounded: the object to be bounded.
 * @x1: x-coordinate of the lower left corner.
 * @y1: y-coordinate of the lower left corner.
 * @z1: z-coordinate of the lower left corner.
 * @x2: x-coordinate of the upper right corner.
 * @y2: y-coordinate of the upper right corner.
 * @z2: z-coordinate of the upper right corner.
 *
 * Sets fields of @bbox.
 */
void gts_bbox_set (GtsBBox * bbox,
		   gpointer bounded,
		   gdouble x1, gdouble y1, gdouble z1,
		   gdouble x2, gdouble y2, gdouble z2)
{
  g_return_if_fail (bbox != NULL);
  g_return_if_fail (x2 >= x1 && y2 >= y1 && z2 >= z1);

  bbox->x1 = x1; bbox->y1 = y1; bbox->z1 = z1;
  bbox->x2 = x2; bbox->y2 = y2; bbox->z2 = z2;
  bbox->bounded = bounded;
}

/**
 * gts_bbox_new:
 * @klass: a #GtsBBoxClass.
 * @bounded: the object to be bounded.
 * @x1: x-coordinate of the lower left corner.
 * @y1: y-coordinate of the lower left corner.
 * @z1: z-coordinate of the lower left corner.
 * @x2: x-coordinate of the upper right corner.
 * @y2: y-coordinate of the upper right corner.
 * @z2: z-coordinate of the upper right corner.
 *
 * Returns: a new #GtsBBox.
 */
GtsBBox * gts_bbox_new (GtsBBoxClass * klass,
			gpointer bounded,
			gdouble x1, gdouble y1, gdouble z1,
			gdouble x2, gdouble y2, gdouble z2)
{
  GtsBBox * bbox;

  g_return_val_if_fail (klass != NULL, NULL);

  bbox = GTS_BBOX (gts_object_new (GTS_OBJECT_CLASS (klass)));
  gts_bbox_set (bbox, bounded, x1, y1, z1, x2, y2, z2);
  return bbox;
}

/**
 * gts_bbox_triangle:
 * @klass: a #GtsBBoxClass.
 * @t: a #GtsTriangle.
 *
 * Returns: a new #GtsBBox bounding box of @t.
 */
GtsBBox * gts_bbox_triangle (GtsBBoxClass * klass,
			     GtsTriangle * t)
{
  GtsBBox * bbox;
  GtsPoint * p;

  g_return_val_if_fail (t != NULL, NULL);
  g_return_val_if_fail (klass != NULL, NULL);

  p = GTS_POINT (GTS_SEGMENT (t->e1)->v1);
  bbox = gts_bbox_new (klass, t, p->x, p->y, p->z, p->x, p->y, p->z);

  p = GTS_POINT (GTS_SEGMENT (t->e1)->v2);
  if (p->x > bbox->x2) bbox->x2 = p->x;
  if (p->x < bbox->x1) bbox->x1 = p->x;
  if (p->y > bbox->y2) bbox->y2 = p->y;
  if (p->y < bbox->y1) bbox->y1 = p->y;
  if (p->z > bbox->z2) bbox->z2 = p->z;
  if (p->z < bbox->z1) bbox->z1 = p->z;
  p = GTS_POINT (gts_triangle_vertex (t));
  if (p->x > bbox->x2) bbox->x2 = p->x;
  if (p->x < bbox->x1) bbox->x1 = p->x;
  if (p->y > bbox->y2) bbox->y2 = p->y;
  if (p->y < bbox->y1) bbox->y1 = p->y;
  if (p->z > bbox->z2) bbox->z2 = p->z;
  if (p->z < bbox->z1) bbox->z1 = p->z;
  
  return bbox;
}

/**
 * gts_bbox_segment:
 * @klass: a #GtsBBoxClass.
 * @s: a #GtsSegment.
 * 
 * Returns: a new #GtsBBox bounding box of @s.
 */
GtsBBox * gts_bbox_segment (GtsBBoxClass * klass, GtsSegment * s)
{
  GtsBBox * bbox;
  GtsPoint * p1, * p2;

  g_return_val_if_fail (s != NULL, NULL);
  g_return_val_if_fail (klass != NULL, NULL);

  bbox = gts_bbox_new (klass, s, 0., 0., 0., 0., 0., 0.);

  p1 = GTS_POINT (s->v1); 
  p2 = GTS_POINT (s->v2);
  if (p1->x > p2->x) {
    bbox->x2 = p1->x; bbox->x1 = p2->x;
  }
  else {
    bbox->x1 = p1->x; bbox->x2 = p2->x;
  }
  if (p1->y > p2->y) {
    bbox->y2 = p1->y; bbox->y1 = p2->y;
  }
  else {
    bbox->y1 = p1->y; bbox->y2 = p2->y;
  }
  if (p1->z > p2->z) {
    bbox->z2 = p1->z; bbox->z1 = p2->z;
  }
  else {
    bbox->z1 = p1->z; bbox->z2 = p2->z;
  }

  return bbox;
}

static void bbox_foreach_vertex (GtsPoint * p, GtsBBox * bb)
{
  if (p->x < bb->x1) bb->x1 = p->x;
  if (p->y < bb->y1) bb->y1 = p->y;
  if (p->z < bb->z1) bb->z1 = p->z;
  if (p->x > bb->x2) bb->x2 = p->x;
  if (p->y > bb->y2) bb->y2 = p->y;
  if (p->z > bb->z2) bb->z2 = p->z;
}

/**
 * gts_bbox_surface:
 * @klass: a #GtsBBoxClass.
 * @surface: a #GtsSurface.
 *
 * Returns: a new #GtsBBox bounding box of @surface.
 */
GtsBBox * gts_bbox_surface (GtsBBoxClass * klass, GtsSurface * surface)
{
  GtsBBox * bbox;

  g_return_val_if_fail (klass != NULL, NULL);
  g_return_val_if_fail (surface != NULL, NULL);

  bbox = gts_bbox_new (klass, surface, 0., 0., 0., 0., 0., 0.);
  bbox->x1 = bbox->y1 = bbox->z1 = G_MAXDOUBLE;
  bbox->x2 = bbox->y2 = bbox->z2 = -G_MAXDOUBLE;

  gts_surface_foreach_vertex (surface, (GtsFunc) bbox_foreach_vertex, bbox);

  return bbox;
}

/**
 * gts_bbox_bboxes:
 * @klass: a #GtsBBoxClass.
 * @bboxes: a list of #GtsBBox.
 * 
 * Returns: a new #GtsBBox bounding box of all the bounding boxes in
 * @bboxes.  
 */
GtsBBox * gts_bbox_bboxes (GtsBBoxClass * klass, GSList * bboxes)
{
  GtsBBox * bbox;
  GtsBBox * bb;

  g_return_val_if_fail (bboxes != NULL, NULL);
  g_return_val_if_fail (klass != NULL, NULL);

  bb = bboxes->data;
  bbox = gts_bbox_new (klass, bboxes, 
		       bb->x1, bb->y1, bb->z1, bb->x2, bb->y2, bb->z2);
  bboxes = bboxes->next;
  while (bboxes) {
    bb = bboxes->data;
    if (bb->x1 < bbox->x1) bbox->x1 = bb->x1;
    if (bb->y1 < bbox->y1) bbox->y1 = bb->y1;
    if (bb->z1 < bbox->z1) bbox->z1 = bb->z1;
    if (bb->x2 > bbox->x2) bbox->x2 = bb->x2;
    if (bb->y2 > bbox->y2) bbox->y2 = bb->y2;
    if (bb->z2 > bbox->z2) bbox->z2 = bb->z2;
    bboxes = bboxes->next;
  }

  return bbox;
}

/**
 * gts_bbox_points:
 * @klass: a #GtsBBoxClass.
 * @points: a list of #GtsPoint.
 *
 * Returns: a new #GtsBBox bounding box of @points.
 */
GtsBBox * gts_bbox_points (GtsBBoxClass * klass, GSList * points)
{
  GtsPoint * p;
  GtsBBox * bbox;
  GSList * i;

  if (points == NULL) 
    return NULL;

  p = points->data;  
  bbox = gts_bbox_new (klass, points, p->x, p->y, p->z, p->x, p->y, p->z);

  i = points->next;
  while (i) {
    p = i->data;
    if (p->x > bbox->x2) 
      bbox->x2 = p->x;
    else if (p->x < bbox->x1) 
      bbox->x1 = p->x;
    if (p->y > bbox->y2) 
      bbox->y2 = p->y;
    else if (p->y < bbox->y1) 
      bbox->y1 = p->y;
    if (p->z > bbox->z2) 
      bbox->z2 = p->z;
    else if (p->z < bbox->z1) 
      bbox->z1 = p->z;
    i = i->next;
  }
  
  return bbox;
}

/**
 * gts_bboxes_are_overlapping:
 * @bb1: a #GtsBBox.
 * @bb2: a #GtsBBox.
 *
 * Returns: %TRUE if the bounding boxes @bb1 and @bb2 are overlapping
 * (including just touching), %FALSE otherwise.
 */
gboolean gts_bboxes_are_overlapping (GtsBBox * bb1, GtsBBox * bb2)
{
  if (bb1 == bb2)
    return TRUE;
  if (bb1->x1 > bb2->x2)
    return FALSE;
  if (bb2->x1 > bb1->x2)
    return FALSE;
  if (bb1->y1 > bb2->y2)
    return FALSE;
  if (bb2->y1 > bb1->y2)
    return FALSE;
  if (bb1->z1 > bb2->z2)
    return FALSE;
  if (bb2->z1 > bb1->z2)
    return FALSE;  
  return TRUE;
}

#define bbox_volume(bb) (((bb)->x2 -\
                          (bb)->x1)*\
                         ((bb)->y2 -\
                          (bb)->y1)*\
                         ((bb)->z2 -\
                          (bb)->z1))

/**
 * gts_bbox_diagonal2:
 * @bb: a #GtsBBox.
 *
 * Returns: the squared length of the diagonal of @bb.
 */
gdouble gts_bbox_diagonal2 (GtsBBox * bb)
{
  gdouble x, y, z;

  g_return_val_if_fail (bb != NULL, 0.);

  x = bb->x2 - bb->x1;
  y = bb->y2 - bb->y1;
  z = bb->z2 - bb->z1;

  return x*x + y*y + z*z;
}

/**
 * gts_bbox_draw:
 * @bb: a #GtsBBox.
 * @fptr: a file pointer.
 * 
 * Writes in file @fptr an OOGL (Geomview) description of @bb.
 */
void gts_bbox_draw (GtsBBox * bb, FILE * fptr)
{
  g_return_if_fail (bb != NULL);

  fprintf (fptr, "OFF 8 6 12\n");
  fprintf (fptr, "%g %g %g\n",
	   bb->x1, bb->y1, bb->z1);
  fprintf (fptr, "%g %g %g\n",
	   bb->x2, bb->y1, bb->z1);
  fprintf (fptr, "%g %g %g\n",
	   bb->x2, bb->y2, bb->z1);
  fprintf (fptr, "%g %g %g\n",
	   bb->x1, bb->y2, bb->z1);
  fprintf (fptr, "%g %g %g\n",
	   bb->x1, bb->y1, bb->z2);
  fprintf (fptr, "%g %g %g\n",
	   bb->x2, bb->y1, bb->z2);
  fprintf (fptr, "%g %g %g\n",
	   bb->x2, bb->y2, bb->z2);
  fprintf (fptr, "%g %g %g\n",
	   bb->x1, bb->y2, bb->z2);
  fputs ("4 3 2 1 0\n"
	 "4 4 5 6 7\n"
	 "4 2 3 7 6\n"
	 "4 0 1 5 4\n"
	 "4 0 4 7 3\n"
	 "4 1 2 6 5\n",
	 fptr);
}

#define MINMAX(x1, x2, xmin, xmax) { if (x1 < x2) { xmin = x1; xmax = x2; }\
                                     else { xmin = x2; xmax = x1; } }

/**
 * gts_bbox_point_distance2:
 * @bb: a #GtsBBox.
 * @p: a #GtsPoint.
 * @min: a pointer on a gdouble.
 * @max: a pointer on a gdouble.
 * 
 * Sets @min and @max to lower and upper bounds for the square of the
 * Euclidean distance between the object contained in @bb and @p. For these
 * bounds to make any sense the bounding box must be "tight" i.e. each of the
 * 6 faces of the box must at least be touched by one point of the bounded
 * object.
 */
void gts_bbox_point_distance2 (GtsBBox * bb, GtsPoint * p,
			       gdouble * min, gdouble * max)
{
  gdouble x1, y1, z1, x2, y2, z2, x, y, z;
  gdouble dmin, dmax, xd1, xd2, yd1, yd2, zd1, zd2;
  gdouble mx, Mx, my, My, mz, Mz;
    
  g_return_if_fail (bb != NULL);
  g_return_if_fail (p != NULL);
  g_return_if_fail (min != NULL);
  g_return_if_fail (max != NULL);

  x1 = bb->x1; y1 = bb->y1; z1 = bb->z1; 
  x2 = bb->x2; y2 = bb->y2; z2 = bb->z2;
  x = p->x; y = p->y; z = p->z;

  xd1 = (x1 - x)*(x1 - x);
  xd2 = (x - x2)*(x - x2);
  yd1 = (y1 - y)*(y1 - y);
  yd2 = (y - y2)*(y - y2);
  zd1 = (z1 - z)*(z1 - z);
  zd2 = (z - z2)*(z - z2);
  
  dmin = x < x1 ? xd1 : x > x2 ? xd2 : 0.0;
  dmin += y < y1 ? yd1 : y > y2 ? yd2 : 0.0;
  dmin += z < z1 ? zd1 : z > z2 ? zd2 : 0.0;

  MINMAX (xd1, xd2, mx, Mx);
  MINMAX (yd1, yd2, my, My);
  MINMAX (zd1, zd2, mz, Mz);
  
  dmax = mx + My + Mz;
  dmax = MIN (dmax, Mx + my + Mz);
  dmax = MIN (dmax, Mx + My + mz);
  
  *min = dmin;
  *max = dmax;
}

/**
 * gts_bbox_is_stabbed:
 * @bb: a #GtsBBox.
 * @p: a #GtsPoint.
 *
 * Returns: %TRUE if the ray starting at @p and ending at (+infty,
 * @p->y, @p->z) intersects with @bb, %FALSE otherwise.
 */
gboolean gts_bbox_is_stabbed (GtsBBox * bb, GtsPoint * p)
{
  g_return_val_if_fail (bb != NULL, FALSE);
  g_return_val_if_fail (p != NULL, FALSE);

  if (p->x > bb->x2 ||
      p->y < bb->y1 || p->y > bb->y2 ||
      p->z < bb->z1 || p->z > bb->z2)
    return FALSE;
  return TRUE;
}

extern int triBoxOverlap (double boxcenter[3],
			  double boxhalfsize[3],
			  double triverts[3][3]);

/**
 * gts_bbox_overlaps_triangle:
 * @bb: a #GtsBBox.
 * @t: a #GtsTriangle.
 *
 * This is a wrapper around the fast overlap test of Tomas
 * Akenine-Moller (http://www.cs.lth.se/home/Tomas_Akenine_Moller/).
 *
 * Returns: %TRUE if @bb overlaps with @t, %FALSE otherwise.
 */
gboolean gts_bbox_overlaps_triangle (GtsBBox * bb, GtsTriangle * t)
{
  double bc[3], bh[3], tv[3][3];
  GtsPoint * p1, * p2, * p3;

  g_return_val_if_fail (bb != NULL, FALSE);
  g_return_val_if_fail (t != NULL, FALSE);

  bc[0] = (bb->x2 + bb->x1)/2.;
  bh[0] = (bb->x2 - bb->x1)/2.;
  bc[1] = (bb->y2 + bb->y1)/2.;
  bh[1] = (bb->y2 - bb->y1)/2.;
  bc[2] = (bb->z2 + bb->z1)/2.;
  bh[2] = (bb->z2 - bb->z1)/2.;
  p1 = GTS_POINT (GTS_SEGMENT (t->e1)->v1);
  p2 = GTS_POINT (GTS_SEGMENT (t->e1)->v2);
  p3 = GTS_POINT (gts_triangle_vertex (t));
  tv[0][0] = p1->x; tv[0][1] = p1->y; tv[0][2] = p1->z;
  tv[1][0] = p2->x; tv[1][1] = p2->y; tv[1][2] = p2->z;
  tv[2][0] = p3->x; tv[2][1] = p3->y; tv[2][2] = p3->z;

  return triBoxOverlap (bc, bh, tv);
}

/**
 * gts_bbox_overlaps_segment:
 * @bb: a #GtsBBox.
 * @s: a #GtsSegment.
 *
 * This functions uses gts_bbox_overlaps_triangle() with a degenerate
 * triangle.
 *
 * Returns: %TRUE if @bb overlaps with @s, %FALSE otherwise.
 */
gboolean gts_bbox_overlaps_segment (GtsBBox * bb, GtsSegment * s)
{
  double bc[3], bh[3], tv[3][3];
  GtsPoint * p1, * p2, * p3;

  g_return_val_if_fail (bb != NULL, FALSE);
  g_return_val_if_fail (s != NULL, FALSE);

  bc[0] = (bb->x2 + bb->x1)/2.;
  bh[0] = (bb->x2 - bb->x1)/2.;
  bc[1] = (bb->y2 + bb->y1)/2.;
  bh[1] = (bb->y2 - bb->y1)/2.;
  bc[2] = (bb->z2 + bb->z1)/2.;
  bh[2] = (bb->z2 - bb->z1)/2.;
  p1 = GTS_POINT (s->v1);
  p2 = GTS_POINT (s->v2);
  p3 = p1;
  tv[0][0] = p1->x; tv[0][1] = p1->y; tv[0][2] = p1->z;
  tv[1][0] = p2->x; tv[1][1] = p2->y; tv[1][2] = p2->z;
  tv[2][0] = p3->x; tv[2][1] = p3->y; tv[2][2] = p3->z;

  return triBoxOverlap (bc, bh, tv);
}

/**
 * gts_bb_tree_new:
 * @bboxes: a list of #GtsBBox.
 *
 * Builds a new hierarchy of bounding boxes for @bboxes. At each
 * level, the GNode->data field contains a #GtsBBox bounding box of
 * all the children. The tree is binary and is built by repeatedly
 * cutting in two approximately equal halves the bounding boxes at
 * each level until a leaf node (i.e. a bounding box given in @bboxes)
 * is reached. In order to minimize the depth of the tree, the cutting
 * direction is always chosen as perpendicular to the longest
 * dimension of the bounding box.
 *
 * Returns: a new hierarchy of bounding boxes.  
 */
GNode * gts_bb_tree_new (GSList * bboxes)
{
  GSList * i, * positive = NULL, * negative = NULL;
  GNode * node;
  GtsBBox * bbox;
  guint dir, np = 0, nn = 0;
  gdouble * p1, * p2;
  gdouble cut;
  
  g_return_val_if_fail (bboxes != NULL, NULL);

  if (bboxes->next == NULL) /* leaf node */
    return g_node_new (bboxes->data);

  bbox = gts_bbox_bboxes (gts_bbox_class (), bboxes);
  node = g_node_new (bbox);

  if (bbox->x2 - bbox->x1 > bbox->y2 - bbox->y1) {
    if (bbox->z2 - bbox->z1 > bbox->x2 - bbox->x1)
      dir = 2;
    else
      dir = 0;
  }
  else if (bbox->z2 - bbox->z1 > bbox->y2 - bbox->y1)
    dir = 2;
  else
    dir = 1;

  p1 = (gdouble *) &bbox->x1;
  p2 = (gdouble *) &bbox->x2;
  cut = (p1[dir] + p2[dir])/2.;
  i = bboxes;
  while (i) {
    bbox = i->data; 
    p1 = (gdouble *) &bbox->x1;
    p2 = (gdouble *) &bbox->x2;
    if ((p1[dir] + p2[dir])/2. > cut) {
      positive = g_slist_prepend (positive, bbox);
      np++;
    }
    else {
      negative = g_slist_prepend (negative, bbox);
      nn++;
    }
    i = i->next;
  }
  if (!positive) {
    GSList * last = g_slist_nth (negative, (nn - 1)/2);
    positive = last->next;
    last->next = NULL;
  }
  else if (!negative) {
    GSList * last = g_slist_nth (positive, (np - 1)/2);
    negative = last->next;
    last->next = NULL;
  }
  g_node_prepend (node, gts_bb_tree_new (positive));
  g_slist_free (positive);
  g_node_prepend (node, gts_bb_tree_new (negative));
  g_slist_free (negative);
  
  return node;
}

static void prepend_triangle_bbox (GtsTriangle * t, GSList ** bboxes)
{
  *bboxes = g_slist_prepend (*bboxes, 
			     gts_bbox_triangle (gts_bbox_class (), t));
}

/**
 * gts_bb_tree_surface:
 * @s: a #GtsSurface.
 *
 * Returns: a new hierarchy of bounding boxes bounding the faces of @s.
 */
GNode * gts_bb_tree_surface (GtsSurface * s)
{
  GSList * bboxes = NULL;
  GNode * tree;

  g_return_val_if_fail (s != NULL, NULL);

  gts_surface_foreach_face (s, (GtsFunc) prepend_triangle_bbox, &bboxes);
  tree = gts_bb_tree_new (bboxes);
  g_slist_free (bboxes);

  return tree;
}

/**
 * gts_bb_tree_stabbed:
 * @tree: a bounding box tree.
 * @p: a #GtsPoint.
 *
 * Returns: a list of bounding boxes, leaves of @tree which are
 * stabbed by the ray defined by @p (see gts_bbox_is_stabbed()).
 */
GSList * gts_bb_tree_stabbed (GNode * tree, GtsPoint * p)
{
  GSList * list = NULL;
  GtsBBox * bb;
  GNode * i;

  g_return_val_if_fail (tree != NULL, NULL);
  g_return_val_if_fail (p != NULL, NULL);

  bb = tree->data;
  if (!gts_bbox_is_stabbed (bb, p))
    return NULL;
  if (tree->children == NULL) /* leaf node */
    return g_slist_prepend (NULL, bb);
  i = tree->children;
  while (i) {
    list = g_slist_concat (list, gts_bb_tree_stabbed (i, p));
    i = i->next;
  }
  return list;
}

/**
 * gts_bb_tree_overlap:
 * @tree: a bounding box tree.
 * @bbox: a #GtsBBox.
 *
 * Returns: a list of bounding boxes, leaves of @tree which overlap @bbox.
 */
GSList * gts_bb_tree_overlap (GNode * tree, GtsBBox * bbox)
{
  GSList * list = NULL;
  GtsBBox * bb;
  GNode * i;

  g_return_val_if_fail (tree != NULL, NULL);
  g_return_val_if_fail (bbox != NULL, NULL);

  bb = tree->data;
  if (!gts_bboxes_are_overlapping (bbox, bb))
    return NULL;
  if (tree->children == NULL) /* leaf node */
    return g_slist_prepend (NULL, bb);
  i = tree->children;
  while (i) {
    list = g_slist_concat (list, gts_bb_tree_overlap (i, bbox));
    i = i->next;
  }
  return list;
}

/**
 * gts_bb_tree_is_overlapping:
 * @tree: a bounding box tree.
 * @bbox: a #GtsBBox.
 *
 * Returns: %TRUE if any leaf of @tree overlaps @bbox, %FALSE otherwise.
 */
gboolean gts_bb_tree_is_overlapping (GNode * tree, GtsBBox * bbox)
{
  GtsBBox * bb;
  GNode * i;

  g_return_val_if_fail (tree != NULL, FALSE);
  g_return_val_if_fail (bbox != NULL, FALSE);

  bb = tree->data;
  if (!gts_bboxes_are_overlapping (bbox, bb))
    return FALSE;
  if (tree->children == NULL) /* leaf node */
    return TRUE;
  i = tree->children;
  while (i) {
    if (gts_bb_tree_is_overlapping (i, bbox))
      return TRUE;
    i = i->next;
  }
  return FALSE;
}

/**
 * gts_bb_tree_traverse_overlapping:
 * @tree1: a bounding box tree.
 * @tree2: a bounding box tree.
 * @func: a #GtsBBTreeTraverseFunc.
 * @data: user data to be passed to @func.
 *
 * Calls @func for each overlapping pair of leaves of @tree1 and @tree2.
 */
void gts_bb_tree_traverse_overlapping (GNode * tree1, GNode * tree2,
				       GtsBBTreeTraverseFunc func,
				       gpointer data)
{
  GtsBBox * bb1, * bb2;

  g_return_if_fail (tree1 != NULL && tree2 != NULL);

  bb1 = tree1->data; bb2 = tree2->data;
  if (!gts_bboxes_are_overlapping (bb1, bb2))
    return;

  if (tree1->children == NULL && tree2->children == NULL)
    (*func) (tree1->data, tree2->data, data);
  else if (tree2->children == NULL || 
	   (tree1->children != NULL && 
	    bbox_volume (bb1) > bbox_volume (bb2))) {
    GNode * i = tree1->children;
    while (i) {
      gts_bb_tree_traverse_overlapping (i, tree2, func, data);
      i = i->next;
    }
  }
  else {
    GNode * i = tree2->children;
    while (i) {
      gts_bb_tree_traverse_overlapping (tree1, i, func, data);
      i = i->next;
    }
  }
}

/**
 * gts_bb_tree_draw:
 * @tree: a bounding box tree.
 * @depth: a specified depth.
 * @fptr: a file pointer.
 *
 * Write in @fptr an OOGL (Geomview) description of @tree for the
 * depth specified by @depth.
 */
void gts_bb_tree_draw (GNode * tree, guint depth, FILE * fptr)
{
  guint d;

  g_return_if_fail (tree != NULL);
  g_return_if_fail (fptr != NULL);

  d = g_node_depth (tree);

  if (d == 1)
    fprintf (fptr, "{ LIST");

  if (d == depth)
    gts_bbox_draw (tree->data, fptr);
  else if (d < depth) {
    GNode * i = tree->children;
    while (i) {
      gts_bb_tree_draw (i, depth, fptr);
      i = i->next;
    }
  }

  if (d == 1)
    fprintf (fptr, "}\n");
}

static void bb_tree_free (GNode * tree, gboolean free_leaves)
{
  GNode * i;

  g_return_if_fail (tree != NULL);

  if (!free_leaves && tree->children == NULL) /* leaf node */
    return;

  gts_object_destroy (tree->data);

  i = tree->children;
  while (i) {
    bb_tree_free (i, free_leaves);
    i = i->next;
  }
}

/**
 * gts_bb_tree_destroy:
 * @tree: a bounding box tree.
 * @free_leaves: if %TRUE the bounding boxes given by the user are freed.
 *
 * Destroys all the bounding boxes created by @tree and destroys the
 * tree itself. If @free_leaves is set to %TRUE, destroys boxes given
 * by the user when creating the tree (i.e. leaves of the tree).  
 */
void gts_bb_tree_destroy (GNode * tree, gboolean free_leaves)
{
  g_return_if_fail (tree != NULL);
  
  bb_tree_free (tree, free_leaves);
  g_node_destroy (tree);
}

static gdouble bb_tree_min_max (GNode * tree,
				GtsPoint * p,
				gdouble min_max,
				GSList ** list)
{
  GNode * tree1, * tree2;
  gdouble min1, max1, min2, max2;

  if (tree->children == NULL) {
    *list = g_slist_prepend (*list, tree->data);
    return min_max;
  }
  tree1 = tree->children;
  gts_bbox_point_distance2 (tree1->data, p, &min1, &max1);
  if (max1 < min_max)
    min_max = max1;

  tree2 = tree1->next;
  gts_bbox_point_distance2 (tree2->data, p, &min2, &max2);
  if (max2 < min_max)
    min_max = max2;

  if (min1 < min2) {
    if (min1 <= min_max) {
      min_max = bb_tree_min_max (tree1, p, min_max, list);
      if (min2 <= min_max)
	min_max = bb_tree_min_max (tree2, p, min_max, list);
    }
  }
  else {
    if (min2 <= min_max) {
      min_max = bb_tree_min_max (tree2, p, min_max, list);
      if (min1 <= min_max)
	min_max = bb_tree_min_max (tree1, p, min_max, list);
    }
  }

  return min_max;
}

/**
 * gts_bb_tree_point_closest_bboxes:
 * @tree: a bounding box tree.
 * @p: a #GtsPoint.
 *
 * Returns: a list of #GtsBBox. One of the bounding boxes is assured to contain
 * the object of @tree closest to @p.
 */
GSList * gts_bb_tree_point_closest_bboxes (GNode * tree, 
					   GtsPoint * p)
{
  gdouble min, min_max;
  GSList * list = NULL, * i, * prev = NULL;

  g_return_val_if_fail (tree != NULL, NULL);
  g_return_val_if_fail (p != NULL, NULL);

  gts_bbox_point_distance2 (tree->data, p, &min, &min_max);
  min_max = bb_tree_min_max (tree, p, min_max, &list);

  i = list;
  while (i) {
    GSList * next = i->next;
    gdouble min, max;

    gts_bbox_point_distance2 (i->data, p, &min, &max);

    if (min > min_max) {
      if (prev == NULL)
	list = next;
      else
	prev->next = next;
      g_slist_free_1 (i);
    }
    else
      prev = i;
    i = next;
  }

  return list;
}

/**
 * gts_bb_tree_point_distance:
 * @tree: a bounding box tree.
 * @p: a #GtsPoint.
 * @distance: a #GtsBBoxDistFunc.
 * @bbox: if not %NULL is set to the bounding box containing the closest 
 * object.
 *
 * Returns: the distance as evaluated by @distance between @p and the closest
 * object in @tree.
 */
gdouble gts_bb_tree_point_distance (GNode * tree, 
				    GtsPoint * p,
				    GtsBBoxDistFunc distance,
				    GtsBBox ** bbox)
{
  GSList * list, * i;
  gdouble dmin = G_MAXDOUBLE;

  g_return_val_if_fail (tree != NULL, dmin);
  g_return_val_if_fail (p != NULL, dmin);
  g_return_val_if_fail (distance != NULL, dmin);

  i = list = gts_bb_tree_point_closest_bboxes (tree, p);
  while (i) {
    gdouble d = (*distance) (p, GTS_BBOX (i->data)->bounded);

    if (fabs (d) < fabs (dmin)) {
      dmin = d;
      if (bbox)
	*bbox = i->data;
    }
    i = i->next;
  }
  g_slist_free (list);

  return dmin;
}

/**
 * gts_bb_tree_point_closest:
 * @tree: a bounding box tree.
 * @p: a #GtsPoint.
 * @closest: a #GtsBBoxClosestFunc.
 * @distance: if not %NULL is set to the distance between @p and the 
 * new #GtsPoint.
 *
 * Returns: a new #GtsPoint, closest point to @p and belonging to an object of
 * @tree.
 */
GtsPoint * gts_bb_tree_point_closest (GNode * tree, 
				      GtsPoint * p,
				      GtsBBoxClosestFunc closest,
				      gdouble * distance)
{
  GSList * list, * i;
  gdouble dmin = G_MAXDOUBLE;
  GtsPoint * np = NULL;

  g_return_val_if_fail (tree != NULL, NULL);
  g_return_val_if_fail (p != NULL, NULL);
  g_return_val_if_fail (closest != NULL, NULL);

  i = list = gts_bb_tree_point_closest_bboxes (tree, p);
  while (i) {
    GtsPoint * tp = (*closest) (p, GTS_BBOX (i->data)->bounded);
    gdouble d = gts_point_distance2 (tp, p);

    if (d < dmin) {
      if (np)
	gts_object_destroy (GTS_OBJECT (np));
      np = tp;
      dmin = d;
    }
    else
      gts_object_destroy (GTS_OBJECT (tp));
    i = i->next;
  }
  g_slist_free (list);

  if (distance)
    *distance = dmin;

  return np;  
}

/**
 * gts_bb_tree_triangle_distance:
 * @tree: a bounding box tree.
 * @t: a #GtsTriangle.
 * @distance: a #GtsBBoxDistFunc.
 * @delta: spatial scale of the sampling to be used.
 * @range: a #GtsRange to be filled with the results.
 * 
 * Given a triangle @t, points are sampled regularly on its surface
 * using @delta as increment. The distance from each of these points
 * to the closest object of @tree is computed using @distance and the
 * gts_bb_tree_point_distance() function. The fields of @range are
 * filled with the number of points sampled, the minimum, average and
 * maximum value and the standard deviation.  
 */
void gts_bb_tree_triangle_distance (GNode * tree,
				    GtsTriangle * t,
				    GtsBBoxDistFunc distance,
				    gdouble delta,
				    GtsRange * range)
{
  GtsPoint * p1, * p2, * p3, * p;
  GtsVector p1p2, p1p3;
  gdouble l1, t1, dt1;
  guint i, n1;

  g_return_if_fail (tree != NULL);
  g_return_if_fail (t != NULL);
  g_return_if_fail (distance != NULL);
  g_return_if_fail (delta > 0.);
  g_return_if_fail (range != NULL);

  gts_triangle_vertices (t, 
			 (GtsVertex **) &p1, 
			 (GtsVertex **) &p2, 
			 (GtsVertex **) &p3);

  gts_vector_init (p1p2, p1, p2);
  gts_vector_init (p1p3, p1, p3);
  gts_range_init (range);
  p = GTS_POINT (gts_object_new (GTS_OBJECT_CLASS (gts_point_class ())));

  l1 = sqrt (gts_vector_scalar (p1p2, p1p2));
  n1 = l1/delta + 1;
  dt1 = 1.0/(gdouble) n1;
  t1 = 0.0;
  for (i = 0; i <= n1; i++, t1 += dt1) {
    gdouble t2 = 1. - t1;
    gdouble x = t2*p1p3[0];
    gdouble y = t2*p1p3[1];
    gdouble z = t2*p1p3[2];
    gdouble l2 = sqrt (x*x + y*y + z*z);
    guint j, n2 = (guint) (l2/delta + 1);
    gdouble dt2 = t2/(gdouble) n2;

    x = t2*p1->x + t1*p2->x;
    y = t2*p1->y + t1*p2->y;
    z = t2*p1->z + t1*p2->z;
    
    t2 = 0.0;
    for (j = 0; j <= n2; j++, t2 += dt2) {
      p->x = x + t2*p1p3[0];
      p->y = y + t2*p1p3[1];
      p->z = z + t2*p1p3[2];

      gts_range_add_value (range,
		    gts_bb_tree_point_distance (tree, p, distance, NULL));
    }
  }

  gts_object_destroy (GTS_OBJECT (p));
  gts_range_update (range);
}

/**
 * gts_bb_tree_segment_distance:
 * @tree: a bounding box tree.
 * @s: a #GtsSegment.
 * @distance: a #GtsBBoxDistFunc.
 * @delta: spatial scale of the sampling to be used.
 * @range: a #GtsRange to be filled with the results.
 * 
 * Given a segment @s, points are sampled regularly on its length
 * using @delta as increment. The distance from each of these points
 * to the closest object of @tree is computed using @distance and the
 * gts_bb_tree_point_distance() function. The fields of @range are
 * filled with the number of points sampled, the minimum, average and
 * maximum value and the standard deviation.  
 */
void gts_bb_tree_segment_distance (GNode * tree,
				   GtsSegment * s,
				   gdouble (*distance) (GtsPoint *, 
							gpointer),
				   gdouble delta,
				   GtsRange * range)
{
  GtsPoint * p1, * p2, * p;
  GtsVector p1p2;
  gdouble l, t, dt;
  guint i, n;

  g_return_if_fail (tree != NULL);
  g_return_if_fail (s != NULL);
  g_return_if_fail (distance != NULL);
  g_return_if_fail (delta > 0.);
  g_return_if_fail (range != NULL);

  p1 = GTS_POINT (s->v1);
  p2 = GTS_POINT (s->v2);

  gts_vector_init (p1p2, p1, p2);
  gts_range_init (range);
  p = GTS_POINT (gts_object_new (GTS_OBJECT_CLASS (gts_point_class())));

  l = sqrt (gts_vector_scalar (p1p2, p1p2));
  n = (guint) (l/delta + 1);
  dt = 1.0/(gdouble) n;
  t = 0.0;
  for (i = 0; i <= n; i++, t += dt) {
    p->x = p1->x + t*p1p2[0];
    p->y = p1->y + t*p1p2[1];
    p->z = p1->z + t*p1p2[2];
    
    gts_range_add_value (range,
			 gts_bb_tree_point_distance (tree, p, distance, NULL));
  }

  gts_object_destroy (GTS_OBJECT (p));
  gts_range_update (range);
}

static void surface_distance_foreach_triangle (GtsTriangle * t, 
					       gpointer * data)
{
  gdouble * delta = data[1];
  GtsRange * range = data[2];
  gdouble * total_area = data[3], area;
  GtsRange range_triangle;

  gts_bb_tree_triangle_distance (data[0], t, data[4], *delta, &range_triangle);

  if (range_triangle.min < range->min)
    range->min = range_triangle.min;
  if (range_triangle.max > range->max)
    range->max = range_triangle.max;
  range->n += range_triangle.n;

  area = gts_triangle_area (t);
  *total_area += area;
  range->sum += area*range_triangle.mean;
  range->sum2 += area*range_triangle.mean*range_triangle.mean;
}

/**
 * gts_bb_tree_surface_distance:
 * @tree: a bounding box tree.
 * @s: a #GtsSurface.
 * @distance: a #GtsBBoxDistFunc.
 * @delta: a sampling increment defined as the percentage of the diagonal
 * of the root bounding box of @tree.
 * @range: a #GtsRange to be filled with the results.
 *
 * Calls gts_bb_tree_triangle_distance() for each face of @s. The
 * fields of @range are filled with the minimum, maximum and average
 * distance. The average distance is defined as the sum of the average
 * distances for each triangle weighthed by their area and divided by
 * the total area of the surface. The standard deviation is defined
 * accordingly. The @n field of @range is filled with the number of
 * sampled points used.  
 */
void gts_bb_tree_surface_distance (GNode * tree,
				   GtsSurface * s,
				   GtsBBoxDistFunc distance,
				   gdouble delta,
				   GtsRange * range)
{
  gpointer data[5];
  gdouble total_area = 0.;

  g_return_if_fail (tree != NULL);
  g_return_if_fail (s != NULL);
  g_return_if_fail (delta > 0. && delta < 1.);
  g_return_if_fail (range != NULL);

  gts_range_init (range);
  delta *= sqrt (gts_bbox_diagonal2 (tree->data));
  data[0] = tree;
  data[1] = &delta;
  data[2] = range;
  data[3] = &total_area;
  data[4] = distance;

  gts_surface_foreach_face (s, 
			    (GtsFunc) surface_distance_foreach_triangle, 
			    data);

  if (total_area > 0.) {
    if (range->sum2 - range->sum*range->sum/total_area >= 0.)
      range->stddev = sqrt ((range->sum2 - range->sum*range->sum/total_area)
			    /total_area);
    else
      range->stddev = 0.;
    range->mean = range->sum/total_area;
  }
  else
    range->min = range->max = range->mean = range->stddev = 0.;
}

static void surface_distance_foreach_boundary (GtsEdge * e,
					       gpointer * data)
{
  gdouble * delta = data[1];
  GtsRange * range = data[2];
  gdouble * total_length = data[3], length;
  GtsRange range_edge;

  if (gts_edge_is_boundary (e, NULL)) {
    GtsSegment * s =  GTS_SEGMENT (e);

    gts_bb_tree_segment_distance (data[0], s, data[4], *delta, &range_edge);

    if (range_edge.min < range->min)
      range->min = range_edge.min;
    if (range_edge.max > range->max)
      range->max = range_edge.max;
    range->n += range_edge.n;
    
    length = gts_point_distance (GTS_POINT (s->v1), GTS_POINT (s->v2));
    *total_length += length;
    range->sum += length*range_edge.mean;
    range->sum2 += length*range_edge.mean*range_edge.mean;
  }
}

/**
 * gts_bb_tree_surface_boundary_distance:
 * @tree: a bounding box tree.
 * @s: a #GtsSurface.
 * @distance: a #GtsBBoxDistFunc.
 * @delta: a sampling increment defined as the percentage of the diagonal
 * of the root bounding box of @tree.
 * @range: a #GtsRange to be filled with the results.
 *
 * Calls gts_bb_tree_segment_distance() for each edge boundary of @s.
 * The fields of @range are filled with the minimum, maximum and
 * average distance. The average distance is defined as the sum of the
 * average distances for each boundary edge weighthed by their length
 * and divided by the total length of the boundaries. The standard
 * deviation is defined accordingly. The @n field of @range is filled
 * with the number of sampled points used.  
 */
void gts_bb_tree_surface_boundary_distance (GNode * tree,
					    GtsSurface * s,
					    gdouble (*distance) (GtsPoint *,
								 gpointer),
					    gdouble delta,
					    GtsRange * range)
{
  gpointer data[5];
  gdouble total_length = 0.;

  g_return_if_fail (tree != NULL);
  g_return_if_fail (s != NULL);
  g_return_if_fail (delta > 0. && delta < 1.);
  g_return_if_fail (range != NULL);

  gts_range_init (range);
  delta *= sqrt (gts_bbox_diagonal2 (tree->data));
  data[0] = tree;
  data[1] = &delta;
  data[2] = range;
  data[3] = &total_length;
  data[4] = distance;

  gts_surface_foreach_edge (s, 
			    (GtsFunc) surface_distance_foreach_boundary, 
			    data);

  if (total_length > 0.) {
    if (range->sum2 - range->sum*range->sum/total_length >= 0.)
      range->stddev = sqrt ((range->sum2 - 
			     range->sum*range->sum/total_length)
			    /total_length);
    else
      range->stddev = 0.;
    range->mean = range->sum/total_length;
  }
  else
    range->min = range->max = range->mean = range->stddev = 0.;
}
