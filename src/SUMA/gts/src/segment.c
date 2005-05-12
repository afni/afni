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

static void segment_destroy (GtsObject * object)
{
  GtsSegment * segment = GTS_SEGMENT (object);
  GtsVertex * v1 = segment->v1;
  GtsVertex * v2 = segment->v2;

  v1->segments = g_slist_remove (v1->segments, segment);
  if (!GTS_OBJECT_DESTROYED (v1) &&
      !gts_allow_floating_vertices && v1->segments == NULL)
    gts_object_destroy (GTS_OBJECT (v1));

  v2->segments = g_slist_remove (v2->segments, segment);
  if (!GTS_OBJECT_DESTROYED (v2) &&
      !gts_allow_floating_vertices && v2->segments == NULL)
    gts_object_destroy (GTS_OBJECT (v2));

  (* GTS_OBJECT_CLASS (gts_segment_class ())->parent_class->destroy) (object);
}

static void segment_class_init (GtsObjectClass * klass)
{
  klass->destroy = segment_destroy;
}

static void segment_init (GtsSegment * segment)
{
  segment->v1 = segment->v2 = NULL;
}

/**
 * gts_segment_class:
 *
 * Returns: the #GtsSegmentClass.
 */
GtsSegmentClass * gts_segment_class (void)
{
  static GtsSegmentClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo segment_info = {
      "GtsSegment",
      sizeof (GtsSegment),
      sizeof (GtsSegmentClass),
      (GtsObjectClassInitFunc) segment_class_init,
      (GtsObjectInitFunc) segment_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), 
				  &segment_info);
  }

  return klass;
}

/**
 * gts_segment_new:
 * @klass: a #GtsSegmentClass.
 * @v1: a #GtsVertex.
 * @v2: another #GtsVertex different from @v1.
 *
 * Returns: a new #GtsSegment linking @v1 and @v2.
 */
GtsSegment * gts_segment_new (GtsSegmentClass * klass, 
			      GtsVertex * v1, GtsVertex * v2)
{
  GtsSegment * s;

  g_return_val_if_fail (v1 != NULL, NULL);
  g_return_val_if_fail (v2 != NULL, NULL);
  g_return_val_if_fail (v1 != v2, NULL);

  s = GTS_SEGMENT (gts_object_new (GTS_OBJECT_CLASS (klass)));
  s->v1 = v1;
  s->v2 = v2;
  v1->segments = g_slist_prepend (v1->segments, s);
  v2->segments = g_slist_prepend (v2->segments, s);
  
  return s;
}

/**
 * gts_segment_is_duplicate:
 * @s: a #GtsSegment.
 *
 * Returns: the first #GtsSegment different from @s which shares the
 * same endpoints or %NULL if there is none.
 */
GtsSegment * gts_segment_is_duplicate (GtsSegment * s)
{
  GSList * i;
  GtsVertex * v2;

  g_return_val_if_fail (s != NULL, NULL);

  v2 = s->v2;
  i = s->v1->segments;
  if (s->v1 == v2) /* s is degenerate: special treatment */
    while (i) {
      GtsSegment * s1 = i->data;
      if (s1 != s && s1->v1 == v2 && s1->v2 == v2)
	return s1;
      i = i->next;
    }
  else /* s is not degenerate */
    while (i) {
      GtsSegment * s1 = i->data;
      if (s1 != s && (s1->v1 == v2 || s1->v2 == v2))
	return s1;
      i = i->next;
    }
  return NULL;
}

/**
 * gts_segments_are_intersecting:
 * @s1: a #GtsSegment.
 * @s2: a #GtsSegment.
 *
 * Returns: %GTS_IN if @s1 and @s2 are intersecting, %GTS_ON if one of the
 * endpoints of @s1 (resp. @s2) lies on @s2 (resp. @s1), %GTS_OUT otherwise.
 */
GtsIntersect gts_segments_are_intersecting (GtsSegment * s1, GtsSegment * s2)
{
  GtsPoint * p1, * p2, * p3, * p4;
  gdouble d1, d2, d3, d4;

  g_return_val_if_fail (s1 != NULL && s2 != NULL, FALSE);

  p1 = GTS_POINT (s1->v1); p2 = GTS_POINT (s1->v2);
  p3 = GTS_POINT (s2->v1); p4 = GTS_POINT (s2->v2);
  d1 = gts_point_orientation (p1, p2, p3);
  d2 = gts_point_orientation (p1, p2, p4);
  if ((d1 > 0.0 && d2 > 0.0) ||
      (d1 < 0.0 && d2 < 0.0))
    return GTS_OUT;
  d3 = gts_point_orientation (p3, p4, p1);
  d4 = gts_point_orientation (p3, p4, p2);
  if ((d3 > 0.0 && d4 > 0.0) ||
      (d3 < 0.0 && d4 < 0.0))
    return GTS_OUT;
  if (d1 == 0.0 || d2 == 0.0 || d3 == 0.0 || d4 == 0.0)
    return GTS_ON;
  return GTS_IN;
}

/**
 * gts_segment_midvertex:
 * @s: a #GtsSegment.
 * @klass: a #GtsVertexClass to be used for the new vertex.
 *
 * Returns: a new #GtsVertex, midvertex of @s.
 */
GtsVertex * gts_segment_midvertex (GtsSegment * s, GtsVertexClass * klass)
{
  GtsPoint * p1, * p2;

  g_return_val_if_fail (s != NULL, NULL);
  g_return_val_if_fail (klass != NULL, NULL);

  p1 = GTS_POINT (s->v1); p2 = GTS_POINT (s->v2);
  return gts_vertex_new (klass,
			 (p1->x + p2->x)/2., 
			 (p1->y + p2->y)/2.,
			 (p1->z + p2->z)/2.);
}

/**
 * gts_segments_from_vertices:
 * @vertices: a list of #GtsVertex.
 * 
 * Returns: a list of unique #GtsSegment which have one of their vertices in 
 * @vertices.
 */
GSList * gts_segments_from_vertices (GSList * vertices)
{
  GHashTable * hash;
  GSList * segments = NULL, * i;

  hash = g_hash_table_new (NULL, NULL);
  i = vertices;
  while (i) {
    GSList * j = GTS_VERTEX (i->data)->segments;
    while (j) {
      GtsSegment * s = j->data;
      if (g_hash_table_lookup (hash, s) == NULL) {
	segments = g_slist_prepend (segments, s);
	g_hash_table_insert (hash, s, i);
      }
      j = j->next;
    }
    i = i->next;
  }
  g_hash_table_destroy (hash);
  return segments;
}

/**
 * gts_segment_is_ok:
 * @s: a #GtsSegment.
 * 
 * Returns: %TRUE if @s is not degenerate (i.e. @s->v1 != @s->v2) and not 
 * duplicate, %FALSE otherwise.
 */
gboolean gts_segment_is_ok (GtsSegment * s)
{
  g_return_val_if_fail (s != NULL, FALSE);
  g_return_val_if_fail (s->v1 != s->v2, FALSE);
  g_return_val_if_fail (!gts_segment_is_duplicate (s), FALSE);
  g_return_val_if_fail (GTS_OBJECT (s)->reserved == NULL, FALSE);
  return TRUE;
}
