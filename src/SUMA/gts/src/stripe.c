/* GTS - Library for the manipulation of triangulated surfaces
 * Copyright (C) 1999-2003  Wagner Toledo Correa, Stéphane Popinet
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

#define PRINT_HEAP_ELEMENTS 0

typedef struct {
  GtsTriangle * t;
  gboolean used;
  GSList * neighbors;
  GtsEHeapPair *pos;
} tri_data_t;

typedef struct {
  GHashTable * ht;
} map_t;

typedef struct {
  map_t * map;
  GtsEHeap * heap;
} heap_t;

static tri_data_t    * tri_data_new (GtsTriangle * t);
static void            tri_data_destroy (tri_data_t * td);
static guint           tri_data_num_unused_neighbors2 (const tri_data_t * td,
						       const map_t * map);
static GHashTable    * tri_data_unused_neighbors2 (const tri_data_t * td,
						   const map_t * map);

static map_t         * map_new (GtsSurface * s);
static void            map_destroy (map_t * map);
static tri_data_t    * map_lookup (const map_t * map, GtsTriangle * t);


static heap_t        * heap_new (GtsSurface * s);
static void            heap_destroy (heap_t * heap);
static gboolean        heap_is_empty (const heap_t * heap);
static GtsTriangle   * heap_top (const heap_t * heap);
static void            heap_remove (heap_t * heap, GtsTriangle * t);

/* helper functions */

static gboolean vertices_are_unique (GtsVertex * v1,
				     GtsVertex * v2,
				     GtsVertex * v3)
{
  g_assert (v1 && v2 && v3);
  return (v1 != v2 && v1 != v3 && v2 != v3);
}

static gboolean vertex_is_one_of (GtsVertex * v,
				  GtsVertex * v1,
				  GtsVertex * v2,
				  GtsVertex * v3)
{
  g_assert (v && v1 && v2 && v3);
  return v == v1 || v == v2 || v == v3;
}

static guint num_shared_vertices (GtsVertex * u1,
				  GtsVertex * u2,
				  GtsVertex * u3,
				  GtsVertex * v1,
				  GtsVertex * v2,
				  GtsVertex * v3)
{
  guint n = 0;
  
  g_assert (u1 && u2 && u3);
  g_assert (v1 && v2 && v3);
  g_assert (vertices_are_unique (u1, u2, u3));
  g_assert (vertices_are_unique (v1, v2, v3));
  
  if (vertex_is_one_of (v1, u1, u2, u3))
    n++;
  if (vertex_is_one_of (v2, u1, u2, u3))
    n++;
  if (vertex_is_one_of (v3, u1, u2, u3))
    n++;
  return n;
}

static gboolean vertices_match (GtsVertex * v1,
				GtsVertex * v2,
				GtsVertex * v3,
				GtsVertex ** v4,
				GtsVertex ** v5,
				GtsVertex ** v6)
{
  guint i;

  g_assert (v4 && v5 && v6);
  g_assert (*v4 && *v5 && *v6);
  g_assert (vertices_are_unique (*v4, *v5, *v6));
  
  for (i = 0; i < 2; i++) {
    if ((!v1 || (v1 == *v4)) &&
	(!v2 || (v2 == *v5)) &&
	(!v3 || (v3 == *v6)))
      return TRUE;
    else {
      GtsVertex * v7 = * v4;

      *v4 = *v5;
      *v5 = *v6;
      *v6 = v7;
    }
  }
  return ((!v1 || (v1 == *v4)) &&
	  (!v2 || (v2 == *v5)) &&
	  (!v3 || (v3 == *v6)));
}

static GtsVertex * non_shared_vertex1 (GtsVertex * u1,
				       GtsVertex * u2,
				       GtsVertex * u3,
				       GtsVertex * v1,
				       GtsVertex * v2,
				       GtsVertex * v3)
{
  GtsVertex * u = NULL;

  g_assert (u1 && u2 && u3);
  g_assert (v1 && v2 && v3);
  g_assert (vertices_are_unique (u1, u2, u3));
  g_assert (vertices_are_unique (v1, v2, v3));
  g_assert (num_shared_vertices (u1, u2, u3, v1, v2, v3) == 2);

  if (!vertex_is_one_of (u1, v1, v2, v3)) {
    g_assert (vertex_is_one_of (u2, v1, v2, v3));
    g_assert (vertex_is_one_of (u3, v1, v2, v3));
    u = u1;
  } else if (!vertex_is_one_of (u2, v1, v2, v3)) {
    g_assert (vertex_is_one_of (u1, v1, v2, v3));
    g_assert (vertex_is_one_of (u3, v1, v2, v3));
    u = u2;
  } else if (!vertex_is_one_of (u3, v1, v2, v3)) {
    g_assert (vertex_is_one_of (u1, v1, v2, v3));
    g_assert (vertex_is_one_of (u2, v1, v2, v3));
    u = u3;
  } else 
    g_assert_not_reached ();

  return u;
}

static void match_vertex (GtsVertex * v,
			  GtsVertex ** v1,
			  GtsVertex ** v2,
			  GtsVertex ** v3)
{
  g_assert (v && v1 && v2 && v3);
  g_assert (*v1 && *v2 && *v3);
  g_assert (vertex_is_one_of (v, *v1, *v2, *v3));
  while (*v1 != v) {
    GtsVertex *v0 = *v1;

    *v1 = *v2;
    *v2 = *v3;
    *v3 = v0;
  }
}

/* tri_data_t functions */

static tri_data_t * tri_data_new (GtsTriangle * t)
{
  tri_data_t * td;
  
  td = g_malloc (sizeof (tri_data_t));
  td->t = t;
  td->used = FALSE;
  td->neighbors = gts_triangle_neighbors (t);
  td->pos = NULL;

  return td;
}

static void tri_data_destroy (tri_data_t * td)
{
  if (!td)
    return;
  g_slist_free (td->neighbors);
  g_free (td);
}

static guint tri_data_num_unused_neighbors2 (const tri_data_t * td,
					     const map_t * map)
{
  GHashTable *h;
  guint n;

  g_assert (td);
  g_assert (map);
  h = tri_data_unused_neighbors2 (td, map);
  n = g_hash_table_size (h);
  g_hash_table_destroy (h);
  return n;
}

static void copy_key_to_array (gpointer key,
			       gpointer value,
			       gpointer user_data)
{
  GtsTriangle * t = key;
  GtsTriangle *** p = user_data;

  (void) value;
  g_assert (t);
  g_assert (p && *p);
  **p = t;
  (*p)++;
}

static gboolean are_neighbors_unique (GHashTable *h)
{
  GtsTriangle ** a;
  GtsTriangle ** p;
  gint i, j, n;		/* guint won't work if n == 0 */

  g_assert (h);
  n = g_hash_table_size (h);
#ifdef DEBUG
  if (n > 9)
    g_warning ("triangle has %d 2-level neighbors", n);
#endif /* DEBUG */
  a = g_malloc(n*sizeof (GtsTriangle *));
  p = a;
  g_hash_table_foreach (h, copy_key_to_array, &p);
  for (i = 0; i < n - 1; i++) {
    g_assert (a[i]);
    for (j = i + 1; j < n; j++) {
      g_assert (a[j]);
      if (a[i] == a[j]) {
	g_free (a);
	return FALSE;
      }
    }
  }
  g_free (a);
  return TRUE;
}

static GHashTable * tri_data_unused_neighbors2 (const tri_data_t * td,
						const map_t * map)
{
  GHashTable * h = g_hash_table_new (NULL, NULL);
  GSList * li;

  g_assert (td);
  g_assert (map);
  for (li = td->neighbors; li != NULL; li = li->next) {
    GtsTriangle * t2 = li->data;
    tri_data_t * td2 = map_lookup (map, t2);
    GSList * lj;

    g_assert (td2);
    if (!td2->used) {
      g_hash_table_insert (h, t2, td2);
      for (lj = td2->neighbors; lj != NULL; lj = lj->next) {
	GtsTriangle * t3 = lj->data;
	tri_data_t * td3 = map_lookup (map, t3);

	g_assert (td3);
	if (td3 != td && !td3->used)
	  g_hash_table_insert (h, t3, td3);
      }
    }
  }
  g_assert (are_neighbors_unique (h));
  return h;
}

#if PRINT_HEAP_ELEMENTS
static void tri_data_print (const tri_data_t * td, FILE * fp)
{
  g_assert (td);
  g_assert (fp);
  fprintf(fp, "td=%p t=%p used=%d pos=%p key=%f\n",
	  td, td->t, td->used, td->pos,
	  td->pos ? td->pos->key : -1.0);
}
#endif /* PRINT_HEAP_ELEMENTS */

/* heap_t functions */

static gdouble triangle_priority (gpointer item, gpointer data)
{
  GtsTriangle * t = item;
  map_t * map = data;
  tri_data_t * td;
  gdouble k;
  
  g_assert (t);
  g_assert (map);
  td = map_lookup (map, t);
  g_assert (td);
  k = tri_data_num_unused_neighbors2 (td, map);
  return k;
}

#if PRINT_HEAP_ELEMENTS
static void print_heap_element (gpointer data, gpointer user_data)
{
  GtsTriangle * t = data;
  map_t * map = user_data;
  tri_data_t * td;
  
  g_assert (t);
  g_assert (map);
  td = map_lookup (map, t);
  g_assert (td);
  g_assert (!td->used);
  g_assert (td->pos);
  tri_data_print (td, stderr);
}
#endif /* PRINT_HEAP_ELEMENTS */

static void insert_entry_into_heap (gpointer key,
				    gpointer value,
				    gpointer user_data)
{
  GtsTriangle * t = key;
  tri_data_t * td = value;
  GtsEHeap * heap = user_data;
  
  g_assert (!td->pos);
  td->pos = gts_eheap_insert (heap, t);
  g_assert (td->pos);
}

static heap_t * heap_new (GtsSurface *s)
{
  heap_t * heap;

  g_assert (s);
  heap = g_malloc (sizeof (heap_t));
  heap->map = map_new (s);
  heap->heap = gts_eheap_new (triangle_priority, heap->map);
  g_hash_table_foreach (heap->map->ht,
			insert_entry_into_heap,
			heap->heap);
#if PRINT_HEAP_ELEMENTS
  gts_eheap_foreach (heap->heap, print_heap_element, heap->map);
#endif /* PRINT_HEAP_ELEMENTS */
  return heap;
}

static void heap_destroy (heap_t * heap)
{
  if (!heap)
    return;
  map_destroy (heap->map);
  gts_eheap_destroy (heap->heap);
  g_free (heap);
}

static gboolean heap_is_empty (const heap_t * heap)
{
  g_assert (heap);
  g_assert (heap->heap);
  return gts_eheap_size (heap->heap) == 0;
}

typedef struct {
  const heap_t * heap;
  double min_key;
} min_key_t;

static GtsTriangle * heap_top (const heap_t * heap)
{
  GtsTriangle * t;
  
  g_assert (heap);
  g_assert (heap->heap);
  t = gts_eheap_top (heap->heap, NULL);
  return t;
}

static void decrease_key (gpointer key, gpointer value, gpointer user_data)
{
  GtsTriangle * t = key;
  tri_data_t * td = value;
  heap_t *heap = user_data;
  gdouble k;
  
  (void) t;
  g_assert (heap);
  g_assert (heap->map);
  g_assert (heap->heap);
  g_assert (td);
  g_assert (!td->used);
  g_assert (td->pos);
  
  k = tri_data_num_unused_neighbors2 (td, heap->map);
  g_assert (k <= td->pos->key);
#ifdef DEBUG
  if (k == td->pos->key)
    g_warning ("same key: %f\n", k);
#endif /* DEBUG */
  if (k != td->pos->key) {
    g_assert (k < td->pos->key);
    g_assert (k >= 0.0);
    gts_eheap_decrease_key (heap->heap, td->pos, k);
  }
}

static void heap_remove (heap_t * heap, GtsTriangle * t)
{
  tri_data_t * td;
  GHashTable * h;
  
  g_assert (heap);
  g_assert (t);
  td = map_lookup (heap->map, t);
  g_assert (td);
  g_assert (!td->used);
  g_assert (td->pos);
  td->used = TRUE;
  gts_eheap_remove (heap->heap, td->pos);
  td->pos = NULL;
  
  /*	fprintf(stderr, "td: %p\n", td); */
  h = tri_data_unused_neighbors2 (td, heap->map);
  g_hash_table_foreach (h, decrease_key, heap);
  g_hash_table_destroy (h);
}

/* map_t functions */

static gint create_map_entry (gpointer item, gpointer data)
{
  GtsTriangle * t = item;
  GHashTable * ht = data;
  tri_data_t * td;

  g_assert (t);
  g_assert (ht);
  td = tri_data_new (t);
  g_hash_table_insert (ht, t, td);
  return 0;
}

static void free_map_entry (gpointer key, gpointer value, gpointer user_data)
{
  GtsTriangle * t = key;
  tri_data_t * td = value;

  (void) user_data;
  g_assert (t);
  g_assert (td);
  g_assert (td->t == t);
  tri_data_destroy (td);
}

static map_t * map_new (GtsSurface * s)
{
  map_t * map;

  map = g_malloc (sizeof (map_t));
  map->ht = g_hash_table_new (NULL, NULL);
  gts_surface_foreach_face (s, create_map_entry, map->ht);
  return map;
}

static void map_destroy (map_t * map)
{
  if (!map)
    return;
  g_hash_table_foreach (map->ht, free_map_entry, NULL);
  g_hash_table_destroy (map->ht);
  g_free (map);
}

static tri_data_t * map_lookup (const map_t * map, GtsTriangle * t)
{
  tri_data_t * td;

  g_assert (map);
  g_assert (map->ht);
  g_assert (t);
  td = g_hash_table_lookup (map->ht, t);
  g_assert (td);
  g_assert (td->t == t);
  return td;
}

/* other helper functions */

static GtsTriangle * find_min_neighbor (heap_t * heap, GtsTriangle * t)
{
  GtsTriangle * min_neighbor = NULL;
  gdouble min_key = G_MAXDOUBLE;
  tri_data_t * td;
  GSList * li;

  g_assert (heap);
  g_assert (t);

  td = map_lookup (heap->map, t);
  for (li = td->neighbors; li != NULL; li = li->next) {
    GtsTriangle * t2 = li->data;
    tri_data_t * td2 = map_lookup (heap->map, t2);
    gdouble k;
    
    g_assert (td2);
    if (td2->used)
      continue;
    g_assert (td2->pos);
    k = td2->pos->key;
    if (k < min_key) {
      min_key = k;
      min_neighbor = t2;
    }
  }
  return min_neighbor;
}

static GtsTriangle * find_neighbor_forward (heap_t * heap,
					    GtsTriangle * t,
					    GtsVertex ** v1,
					    GtsVertex ** v2,
					    GtsVertex ** v3,
					    gboolean left_turn)
{
  GtsTriangle * neighbor = NULL;
  tri_data_t * td;
  GSList * li;

  g_assert (heap);
  g_assert (t);
  g_assert (v1 && v2 && v3);
  g_assert (vertices_are_unique (*v1, *v2, *v3));
  
  td = map_lookup (heap->map, t);
  g_assert (td);
  for (li = td->neighbors; li && !neighbor; li = li->next) {
    GtsTriangle * t2 = li->data;
    tri_data_t * td2 = map_lookup (heap->map, t2);
    GtsVertex * v4, * v5, * v6;
    
    g_assert (td2);
    if (t2 == t || td2->used)
      continue;
    gts_triangle_vertices (t2, &v4, &v5, &v6);
    if (left_turn) {
      if (!vertices_match (*v1, *v3, NULL, &v4, &v5, &v6))
	continue;
    } else {
      if (!vertices_match (*v3, *v2, NULL, &v4, &v5, &v6))
	continue;
    }
    neighbor = t2;
    *v1 = v4;
    *v2 = v5;
    *v3 = v6;
  }
  return neighbor;
}

static GtsTriangle * find_neighbor_backward (heap_t * heap,
					     GtsTriangle * t,
					     GtsVertex ** v1,
					     GtsVertex ** v2,
					     GtsVertex ** v3,
					     gboolean left_turn)
{
  GtsTriangle * neighbor = NULL;
  tri_data_t * td;
  GSList * li;

  g_assert (heap);
  g_assert (t);
  g_assert (v1 && v2 && v3);
  g_assert (vertices_are_unique (*v1, *v2, *v3));

  td = map_lookup (heap->map, t);
  g_assert (td);
  for (li = td->neighbors; li && !neighbor; li = li->next) {
    GtsTriangle * t2 = li->data;
    tri_data_t * td2 = map_lookup (heap->map, t2);
    GtsVertex * v4, * v5, * v6;
    
    g_assert (td2);
    if (t2 == t || td2->used)
      continue;
    gts_triangle_vertices (t2, &v4, &v5, &v6);
    if (left_turn) {
      if (!vertices_match (NULL, *v2, *v1, &v4, &v5, &v6))
	continue;
    } else if (!vertices_match(*v1, NULL, *v2, &v4, &v5, &v6))
      continue;
    neighbor = t2;
    *v1 = v4;
    *v2 = v5;
    *v3 = v6;
  }
  return neighbor;
}

static GSList * grow_strip_forward (heap_t * heap,
				    GSList * strip,
				    GtsTriangle * t,
				    GtsVertex * v1,
				    GtsVertex * v2,
				    GtsVertex * v3)
{
  gboolean left_turn;
  
  g_assert (heap);
  g_assert (g_slist_length(strip) == 2);
  g_assert (t);
  g_assert (v1 && v2 && v3);
  g_assert (vertices_are_unique (v1, v2, v3));

  left_turn = TRUE;
  while ((t = find_neighbor_forward (heap, t, &v1, &v2, &v3, 
				     left_turn)) != NULL) {
    heap_remove (heap, t);
    strip = g_slist_prepend (strip, t);
    left_turn = !left_turn;
  }
  return strip;
}

static GSList * grow_strip_backward (heap_t * heap,
				     GSList * strip,
				     GtsTriangle * t,
				     GtsVertex * v1,
				     GtsVertex * v2,
				     GtsVertex * v3)
{
  /* we have to make sure we add an even number of triangles */
  GtsTriangle * t2;

  g_assert (heap);
  g_assert (g_slist_length(strip) >= 2);
  g_assert (t);
  g_assert (v1 && v2 && v3);
  g_assert (vertices_are_unique (v1, v2, v3));

  while ((t2 = find_neighbor_backward (heap, t, &v1, &v2, &v3,
				       FALSE)) != NULL
	 && (t = find_neighbor_backward (heap, t2, &v1, &v2, &v3,
					 TRUE)) != NULL) {
    heap_remove (heap, t2);
    heap_remove (heap, t);
    strip = g_slist_prepend (strip, t2);
    strip = g_slist_prepend (strip, t);
  }
  return strip;
}

static gboolean find_right_turn (GtsVertex ** v1,
				 GtsVertex ** v2,
				 GtsVertex ** v3,
				 GtsVertex ** v4,
				 GtsVertex ** v5,
				 GtsVertex ** v6)
{
  GtsVertex * v;

  g_assert (v1 && v2 && v3);
  g_assert (v4 && v5 && v6);
  g_assert (vertices_are_unique (*v1, *v2, *v3));
  g_assert (vertices_are_unique (*v4, *v5, *v6));
  g_assert (num_shared_vertices (*v1, *v2, *v3, *v4, *v5, *v6) == 2);

  v = non_shared_vertex1 (*v1, *v2, *v3, *v4, *v5, *v6);
  match_vertex (v, v1, v2, v3);
  match_vertex (*v3, v4, v5, v6);

  g_assert (v1 && v2 && v3);
  g_assert (v4 && v5 && v6);
  g_assert (*v4 == *v3);

  if (*v5 == *v2) {
    g_assert (vertices_are_unique (*v1, *v2, *v3));
    g_assert (vertices_are_unique (*v4, *v5, *v6));
    g_assert (num_shared_vertices (*v1, *v2, *v3,
					*v4, *v5, *v6) == 2);
    return TRUE;
  } else {
#ifdef DEBUG
    g_warning ("couldn't find a right turn");
#endif /* DEBUG */
    return FALSE;
  }
}

/**
 * gts_surface_strip:
 * @s: a #GtsSurface.
 *
 * Decompose @s into triangle strips for fast-rendering.
 *
 * Returns: a list of triangle strips containing all the triangles of @s. 
 * A triangle strip is itself a list of successive triangles having one edge
 * in common.
 */
GSList * gts_surface_strip (GtsSurface *s)
{
  GSList * strips = NULL;
  heap_t * heap;

  g_return_val_if_fail (s != NULL, NULL);

  heap = heap_new (s);
  while (!heap_is_empty (heap)) {
    GtsTriangle * t1, * t2;
    GtsVertex * v1, * v2, * v3, * v4, * v5, * v6;
    GSList * strip = NULL;

    /* remove heap top */
    t1 = heap_top (heap);
    g_assert (t1);
    heap_remove (heap, t1);

    /* start a new strip */
    strip = g_slist_prepend (strip, t1);

    /* find second triangle */
    t2 = find_min_neighbor (heap, t1);
    if (t2) {
      g_assert (t2 != t1);

      /* find right turn */
      gts_triangle_vertices (t1, &v1, &v2, &v3);
      gts_triangle_vertices (t2, &v4, &v5, &v6);
      if (find_right_turn (&v1, &v2, &v3, &v4, &v5, &v6)) {
	heap_remove (heap, t2);
	strip = g_slist_prepend (strip, t2);

	/* grow strip forward */
	strip = grow_strip_forward (heap, strip, t2, v4, v5, v6);

	strip = g_slist_reverse (strip);

	/* grow strip backward */
	strip = grow_strip_backward (heap, strip, t1, v1, v2, v3);
      }
    }
    strips = g_slist_prepend (strips, strip);
  }
  strips = g_slist_reverse (strips);
  heap_destroy (heap);

  return strips;
}
