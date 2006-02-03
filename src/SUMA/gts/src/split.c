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
#include <string.h>
#include "gts.h"

#define DYNAMIC_SPLIT
#define NEW

/* #define DEBUG
   #define DEBUG_HEXPAND
   #define DEBUG_EXPAND */

struct _GtsSplitCFace {
  GtsFace * f;
  GtsTriangle ** a1, ** a2;
};

typedef struct _CFace      CFace;
typedef struct _CFaceClass CFaceClass;

struct _CFace {
  GtsObject object; 

  GtsSplit * parent_split;
  GtsTriangle * t;
  guint flags;
};
/* the size of the CFace structure must be smaller or equal to the size
   of the GtsFace structure as both structures use the same memory location */

struct _CFaceClass {
  GtsObjectClass parent_class;
};

#define IS_CFACE(obj) (gts_object_is_from_class (obj, cface_class ()))
#define CFACE(obj)    ((CFace *) obj)
#define CFACE_ORIENTATION(cf) ((cf)->flags & 0x1)
#define CFACE_ORIENTATION_DIRECT(cf) ((cf)->flags |= 0x1)
#define CFACE_VVS(cf)                ((cf)->flags & 0x2)
#define CFACE_VVS_DIRECT(cf)         ((cf)->flags |= 0x2)
#define CFACE_E1                     0x4
#define CFACE_E2                     0x8
#define CFACE_KEEP_VVS               0x10

#define ROTATE_ORIENT(e, e1, e2, e3)  { if (e1 == e) { e1 = e2; e2 = e3; }\
                                 else if (e2 == e) { e2 = e1; e1 = e3; }\
                                 else g_assert (e3 == e); }
#define SEGMENT_USE_VERTEX(s, v) ((s)->v1 == v || (s)->v2 == v)
#define TRIANGLE_REPLACE_EDGE(t, e, with) { if ((t)->e1 == e)\
					      (t)->e1 = with;\
					    else if ((t)->e2 == e)\
					      (t)->e2 = with;\
					    else {\
					      g_assert ((t)->e3 == e);\
					      (t)->e3 = with;\
					    }\
                                          }

#define HEAP_INSERT_OBJECT(h, e) (GTS_OBJECT (e)->reserved =\
				  gts_eheap_insert (h, e))
#define HEAP_REMOVE_OBJECT(h, e) (gts_eheap_remove (h, GTS_OBJECT (e)->reserved),\
				   GTS_OBJECT (e)->reserved = NULL)

static GtsObjectClass * cface_class (void)
{
  static GtsObjectClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo cface_info = {
      "GtsCFace",
      sizeof (CFace),
      sizeof (CFaceClass),
      (GtsObjectClassInitFunc) NULL,
      (GtsObjectInitFunc) NULL,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), &cface_info);
    g_assert (sizeof (CFace) <= sizeof (GtsFace));
  }

  return klass;
}

/* Replace @e with @with for all the triangles using @e but @f.
   Destroys @e and removes it from @heap (if not %NULL). 
   Returns a triangle using e different from f or %NULL. */
static GtsTriangle * replace_edge_collapse (GtsEdge * e, 
					    GtsEdge * with, 
					    CFace * cf,
					    GtsEHeap * heap
#ifdef DYNAMIC_SPLIT
					    , GtsTriangle *** a1
#endif
#ifdef NEW
					    , guint edge_flag
#endif
					    )
{
  GSList * i;
  GtsTriangle * rt = NULL;
#ifdef DYNAMIC_SPLIT
  guint size;
  GtsTriangle ** a;
#endif

#ifdef NEW
  i = e->triangles;
  e->triangles = NULL;
  size = g_slist_length (i)*sizeof (GtsTriangle *);
  *a1 = a = g_malloc (size > 0 ? size : sizeof (GtsTriangle *));
  while (i) {
    GtsTriangle * t = i->data;
    GSList * next = i->next;
    if (t != ((GtsTriangle *) cf)) {
      if (IS_CFACE (t)) {
	i->next = e->triangles;
	e->triangles = i;
	/* set the edge given by edge_flag (CFACE_E1 or CFACE_E2) */
	GTS_OBJECT (t)->reserved = GUINT_TO_POINTER (edge_flag);
	cf->flags |= CFACE_KEEP_VVS;
      }
      else {
	TRIANGLE_REPLACE_EDGE (t, e, with);
	i->next = with->triangles;
	with->triangles = i;
	rt = t;
	*(a++) = t;
      }
    }
    else
      g_slist_free_1 (i);
    i = next;
  }
  *a = NULL;
  if (!e->triangles) {
    if (heap)
      HEAP_REMOVE_OBJECT (heap, e);
    gts_object_destroy (GTS_OBJECT (e));
  }
#else /* not NEW */
  i = e->triangles;
#ifdef DYNAMIC_SPLIT
  size = g_slist_length (i)*sizeof (GtsTriangle *);
  *a1 = a = g_malloc (size > 0 ? size : sizeof (GtsTriangle *));
#endif
  while (i) {
    GtsTriangle * t = i->data;
    GSList * next = i->next;
    if (t != ((GtsTriangle *) cf)) {
      TRIANGLE_REPLACE_EDGE (t, e, with);
      i->next = with->triangles;
      with->triangles = i;
      rt = t;
#ifdef DYNAMIC_SPLIT
      *(a++) = t;
#endif
    }
    else
      g_slist_free_1 (i);
    i = next;
  }
#ifdef DYNAMIC_SPLIT
  *a = NULL;
#endif
  if (heap)
    HEAP_REMOVE_OBJECT (heap, e);
  e->triangles = NULL;
  gts_object_destroy (GTS_OBJECT (e));
#endif /* NEW */

  return rt;
}

static CFace * cface_new (GtsFace * f,
			  GtsEdge * e,
			  GtsVertex * v1, 
			  GtsVertex * v2,
			  GtsSplit * vs,
			  GtsEHeap * heap,
			  GtsEdgeClass * klass
#ifdef DYNAMIC_SPLIT
			  , GtsSplitCFace * scf
#endif
			  )
{
  CFace * cf;
  GtsVertex * v;
  GtsEdge * e1, * e2, * e3, * vvs;
  GSList * i;
  GtsTriangle * t, * t1 = NULL, * t2 = NULL;
  guint flags;

  g_return_val_if_fail (f != NULL, NULL);
#ifndef NEW
  g_return_val_if_fail (GTS_IS_FACE (f), NULL);
#endif
  g_return_val_if_fail (e != NULL, NULL);
  g_return_val_if_fail (vs != NULL, NULL);

  t = ((GtsTriangle *) f);
  if (heap)
    g_return_val_if_fail (!gts_triangle_is_duplicate (t), NULL);

#ifdef NEW
  /* get CFACE_E1 and CFACE_E2 info */
  flags = GPOINTER_TO_UINT (GTS_OBJECT (f)->reserved);
#endif
  GTS_OBJECT_SET_FLAGS (f, GTS_DESTROYED);

  i = f->surfaces;
  while (i) {
    GSList * next = i->next;
    gts_surface_remove_face (i->data, f);
    i = next;
  }
  g_slist_free (f->surfaces);

  e1 = t->e1; e2 = t->e2; e3 = t->e3;
  ROTATE_ORIENT (e, e1, e2, e3);

  cf = (CFace *) f;
#ifndef NEW
  GTS_OBJECT (cf)->klass = cface_class ();
#else
  cf->flags = flags;
#endif
  gts_object_init (GTS_OBJECT (cf), cface_class ());
  cf->parent_split = vs;

  if (SEGMENT_USE_VERTEX (GTS_SEGMENT (e1), v2)) {
    CFACE_ORIENTATION_DIRECT (cf); /* v1->v2->v */
    e3 = e1; e1 = e2; e2 = e3;
  }
  v = GTS_SEGMENT (e1)->v1 == v1 ?
    GTS_SEGMENT (e1)->v2 : GTS_SEGMENT (e1)->v1;
#ifdef NEW
  if ((cf->flags & CFACE_E1) || (cf->flags & CFACE_E2))
    g_assert ((vvs = GTS_EDGE (gts_vertices_are_connected (vs->v, v))));
  else
#endif
  vvs = gts_edge_new (klass, v, vs->v);

  t1 = replace_edge_collapse (e1, vvs, cf, heap
#ifdef DYNAMIC_SPLIT
			      , &scf->a1
#endif
#ifdef NEW
			      , CFACE_E1
#endif
			      );
  t2 = replace_edge_collapse (e2, vvs, cf, heap
#ifdef DYNAMIC_SPLIT
			      , &scf->a2
#endif
#ifdef NEW
			      , CFACE_E2
#endif
			      );
  t = cf->t = t1 ? t1 : t2;
  g_assert (t);

  /* set up flags necessary to find vvs */
  if (t->e1 == vvs) e2 = t->e2;
  else if (t->e2 == vvs) e2 = t->e3;
  else {
    g_assert (t->e3 == vvs);
    e2 = t->e1;
  }
  if (SEGMENT_USE_VERTEX (GTS_SEGMENT (e2), v))
    CFACE_VVS_DIRECT (cf);

  return cf;
}

static void find_vvs (GtsVertex * vs,
		      GtsTriangle * t,
		      GtsVertex ** v, GtsEdge ** vvs,
		      gboolean orientation)
{
  GtsEdge * e1 = t->e1, * e2 = t->e2, * e3 = t->e3, * tmp;

  if (SEGMENT_USE_VERTEX (GTS_SEGMENT (e2), vs)) {
    tmp = e1; e1 = e2; e2 = e3; e3 = tmp;
  }
  else if (SEGMENT_USE_VERTEX (GTS_SEGMENT (e3), vs)) {
    tmp = e1; e1 = e3; e3 = e2; e2 = tmp;
  }
  else
    g_assert (SEGMENT_USE_VERTEX (GTS_SEGMENT (e1), vs));
  if (SEGMENT_USE_VERTEX (GTS_SEGMENT (e2), vs) ||
      !gts_segments_touch (GTS_SEGMENT (e1), GTS_SEGMENT (e2))) {
    tmp = e1; e1 = e2; e2 = e3; e3 = tmp;
    g_assert (gts_segments_touch (GTS_SEGMENT (e1), GTS_SEGMENT (e2)));
  }

  *vvs = orientation ? e1 : e3;

  if (GTS_SEGMENT (*vvs)->v1 != vs) {
    g_assert (GTS_SEGMENT (*vvs)->v2 == vs);
    *v = GTS_SEGMENT (*vvs)->v1;
  }
  else
    *v = GTS_SEGMENT (*vvs)->v2;
}

static void replace_edge_expand (GtsEdge * e, 
				 GtsEdge * with,
				 GtsTriangle ** a,
				 GtsVertex * v)
{
  GtsTriangle ** i = a, * t;

  while ((t = *(i++))) {
#ifdef DEBUG_EXPAND
    g_assert (!IS_CFACE (t));
    fprintf (stderr, "replacing %p->%d: e: %p->%d with: %p->%d\n",
	     t, id (t), e, id (e), with, id (with));
#endif
    TRIANGLE_REPLACE_EDGE (t, e, with);
    with->triangles = g_slist_prepend (with->triangles, t);
    if (GTS_OBJECT (t)->reserved) {
      /* apart from the triangles having e as an edge, t is the only
	 triangle using v */
      g_assert (GTS_OBJECT (t)->reserved == v);
      GTS_OBJECT (t)->reserved = NULL;
    }
    else
      GTS_OBJECT (t)->reserved = v;
  }
}

static void cface_expand (CFace * cf,
			  GtsTriangle ** a1,
			  GtsTriangle ** a2,
			  GtsEdge * e,
			  GtsVertex * v1, 
			  GtsVertex * v2,
			  GtsVertex * vs,
			  GtsEdgeClass * klass)
{
  GtsVertex * v;
  GtsEdge * e1, * e2, * vvs;
  gboolean orientation;
  guint flags;

  g_return_if_fail (cf != NULL);
  g_return_if_fail (IS_CFACE (cf));
  g_return_if_fail (e != NULL);
  g_return_if_fail (vs != NULL);

  flags = cf->flags;
  orientation = CFACE_ORIENTATION (cf);

  find_vvs (vs, cf->t, &v, &vvs, CFACE_VVS (cf));

#ifdef NEW
  if (flags & CFACE_E1)
    e1 = GTS_EDGE (gts_vertices_are_connected (v1, v));
  else
    e1 = gts_edge_new (klass, v, v1);
  if (flags & CFACE_E2)
    e2 = GTS_EDGE (gts_vertices_are_connected (v2, v));
  else
    e2 = gts_edge_new (klass, v, v2);
#else
  e1 = gts_edge_new (v, v1);
  e2 = gts_edge_new (v, v2);
#endif

  replace_edge_expand (vvs, e1, a1, v1);
  replace_edge_expand (vvs, e2, a2, v2);

#ifdef NEW
  if (!(flags & CFACE_KEEP_VVS)) {
    g_slist_free (vvs->triangles);
    vvs->triangles = NULL;
    gts_object_destroy (GTS_OBJECT (vvs));
  }
#else
  g_slist_free (vvs->triangles);
  vvs->triangles = NULL;
  gts_object_destroy (GTS_OBJECT (vvs));
#endif

  /* gts_face_new : because I am "creating" a face */
  GTS_OBJECT (cf)->klass = GTS_OBJECT_CLASS (gts_face_class ());
  gts_object_init (GTS_OBJECT (cf), GTS_OBJECT (cf)->klass);
  
  if (orientation)
    gts_triangle_set (GTS_TRIANGLE (cf), e, e2, e1);
  else
    gts_triangle_set (GTS_TRIANGLE (cf), e, e1, e2);
}

static void split_destroy (GtsObject * object)
{
  GtsSplit * vs = GTS_SPLIT (object);
  guint i = vs->ncf;
  GtsSplitCFace * cf = vs->cfaces;

  while (i--) {
    if (IS_CFACE (cf->f))
      gts_object_destroy (GTS_OBJECT (cf->f));
    g_free (cf->a1);
    g_free (cf->a2);
    cf++;
  }
  g_free (vs->cfaces);

  if (!gts_allow_floating_vertices && vs->v && vs->v->segments == NULL)
    gts_object_destroy (GTS_OBJECT (vs->v));

  (* GTS_OBJECT_CLASS (gts_split_class ())->parent_class->destroy) (object);
}

static void split_class_init (GtsObjectClass * klass)
{
  klass->destroy = split_destroy;
}

static void split_init (GtsSplit * split)
{
  split->v1 = split->v2 = NULL;
  split->v = NULL;
  split->cfaces = NULL;
  split->ncf = 0;
}

/**
 * gts_split_class:
 *
 * Returns: the #GtsSplitClass.
 */
GtsSplitClass * gts_split_class (void)
{
  static GtsSplitClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo split_info = {
      "GtsSplit",
      sizeof (GtsSplit),
      sizeof (GtsSplitClass),
      (GtsObjectClassInitFunc) split_class_init,
      (GtsObjectInitFunc) split_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), 
				  &split_info);
  }

  return klass;
}

#ifdef DEBUG
static gboolean edge_collapse_is_valid (GtsEdge * e)
{
  GSList * i;

  g_return_val_if_fail (e != NULL, FALSE);
  
  if (gts_segment_is_duplicate (GTS_SEGMENT (e))) {
    g_warning ("collapsing duplicate edge");
    return FALSE;
  }
    
  i = GTS_SEGMENT (e)->v1->segments;
  while (i) {
    GtsEdge * e1 = i->data;
    if (e1 != e && GTS_IS_EDGE (e1)) {
      GtsEdge * e2 = NULL;
      GSList * j = GTS_SEGMENT (e1)->v1 == GTS_SEGMENT (e)->v1 ? 
	GTS_SEGMENT (e1)->v2->segments : GTS_SEGMENT (e1)->v1->segments;
      while (j && !e2) {
	GtsEdge * e1 = j->data;
	if (GTS_IS_EDGE (e1) && 
	    (GTS_SEGMENT (e1)->v1 == GTS_SEGMENT (e)->v2 || 
	     GTS_SEGMENT (e1)->v2 == GTS_SEGMENT (e)->v2))
	  e2 = e1;
	j = j->next;
      }
      if (e2 && !gts_triangle_use_edges (e, e1, e2)) {
	g_warning ("collapsing empty triangle");
	return FALSE;
      }
    }
    i = i->next;
  }

  if (gts_edge_is_boundary (e, NULL)) {
    GtsTriangle * t = e->triangles->data;
    if (gts_edge_is_boundary (t->e1, NULL) &&
	gts_edge_is_boundary (t->e2, NULL) &&
	gts_edge_is_boundary (t->e3, NULL)) {
      g_warning ("collapsing single triangle");
      return FALSE;
    }
  }
  else {
    if (gts_vertex_is_boundary (GTS_SEGMENT (e)->v1, NULL) &&
	gts_vertex_is_boundary (GTS_SEGMENT (e)->v2, NULL)) {
      g_warning ("collapsing two sides of a strip");
      return FALSE;    
    }
    if (gts_edge_belongs_to_tetrahedron (e)) {
      g_warning ("collapsing tetrahedron");
      return FALSE;
    }
  }

  return TRUE;
}
#endif /* DEBUG */

static void print_split (GtsSplit * vs, FILE * fptr)
{
  guint j;
  GtsSplitCFace * cf;

  g_return_if_fail (vs != NULL);
  g_return_if_fail (fptr != NULL);

  fprintf (fptr, "%p: v: %p v1: %p v2: %p ncf: %u cfaces: %p\n",
	   vs, vs->v, vs->v1, vs->v2, vs->ncf, vs->cfaces);
  cf = vs->cfaces;
  j = vs->ncf;
  while (j--) {
    fprintf (stderr, "  f: %p a1: %p a2: %p\n",
	     cf->f, cf->a1, cf->a2);
    cf++;
  }
}

/**
 * gts_split_collapse:
 * @vs: a #GtsSplit.
 * @klass: a #GtsEdgeClass.
 * @heap: a #GtsEHeap or %NULL.
 *
 * Collapses the vertex split @vs. Any new edge created during the process will
 * be of class @klass. If heap is not %NULL, the new edges will be inserted
 * into it and the destroyed edges will be removed from it.
 */
void gts_split_collapse (GtsSplit * vs, 
			 GtsEdgeClass * klass,
			 GtsEHeap * heap)
{
  GtsEdge * e;
  GtsVertex * v, * v1, * v2;
  GSList * i, * end;
#ifdef DYNAMIC_SPLIT
  GtsSplitCFace * cf;
  guint j;
#endif
#ifdef DEBUG
  gboolean invalid = FALSE;
  static guint ninvalid = 0;
#endif

  g_return_if_fail (vs != NULL);
  g_return_if_fail (klass != NULL);

  v = vs->v;

  g_return_if_fail (v->segments == NULL);
  
  /* we don't want to destroy vertices */
  gts_allow_floating_vertices = TRUE;

  v1 = GTS_SPLIT_V1 (vs);
  v2 = GTS_SPLIT_V2 (vs);
  g_assert ((e = GTS_EDGE (gts_vertices_are_connected (v1, v2))));

#ifdef DEBUG
  fprintf (stderr, "collapsing %p: v1: %p v2: %p v: %p\n", vs, v1, v2, v);
  if (!edge_collapse_is_valid (e)) {
    char fname[80];
    FILE * fptr;
    GSList * triangles, * i;

    g_warning ("invalid edge collapse");
    invalid = TRUE;
    sprintf (fname, "invalid.%d", ninvalid);
    fptr = fopen (fname, "wt");
    gts_write_segment (GTS_SEGMENT (e), GTS_POINT (v), fptr);
    triangles = gts_vertex_triangles (v1, NULL);
    triangles = gts_vertex_triangles (v2, triangles);
    i = triangles;
    while (i) {
      gts_write_triangle (i->data, GTS_POINT (v), fptr);
      i = i->next;
    }
    g_slist_free (triangles);
    fclose (fptr);
  }
#endif

  i = e->triangles;
#ifdef DYNAMIC_SPLIT
  cf = vs->cfaces;
  j = vs->ncf;
  while (j--) {
    g_free (cf->a1);
    g_free (cf->a2);
    cf++;
  }
  g_free (vs->cfaces);

  vs->ncf = g_slist_length (i);
  g_assert (vs->ncf > 0);
  cf = vs->cfaces = g_malloc (vs->ncf*sizeof (GtsSplitCFace));
#endif /* DYNAMIC_SPLIT */
#ifdef NEW
  while (i) {
    cf->f = i->data;
    g_assert (GTS_IS_FACE (cf->f));
    GTS_OBJECT (cf->f)->klass = GTS_OBJECT_CLASS (cface_class ());
    cf++;
    i = i->next;
  }
  i = e->triangles;
  cf = vs->cfaces;
  while (i) {
    cface_new (i->data, e, v1, v2, vs, heap, klass, cf);
#ifdef DEBUG
    fprintf (stderr, "cface: %p->%d t: %p->%d a1: ", 
	     cf->f, id (cf->f), CFACE (cf->f)->t, id (CFACE (cf->f)->t));
    {
      GtsTriangle * t, ** a;
      a = cf->a1;
      while ((t = *(a++)))
	fprintf (stderr, "%p->%d ", t, id (t));
      fprintf (stderr, "a2: ");
      a = cf->a2;
      while ((t = *(a++)))
	fprintf (stderr, "%p->%d ", t, id (t));
      fprintf (stderr, "\n");
    }
#endif
    cf++;
    i = i->next;
  }
#else /* not NEW */
  while (i) {
    cface_new (i->data, e, v1, v2, vs, heap
#ifdef DYNAMIC_SPLIT
	       , cf
#endif /* DYNAMIC_SPLIT */
	       );
#ifdef DYNAMIC_SPLIT
    cf->f = i->data;
    cf++;
#endif /* DYNAMIC_SPLIT */
    i = i->next;
  }
#endif /* NEW */
  g_slist_free (e->triangles);
  e->triangles = NULL;
  gts_object_destroy (GTS_OBJECT (e));

  gts_allow_floating_vertices = FALSE;

  end = NULL;
  i = v1->segments;
  while (i) {
    GtsSegment * s = i->data;
    if (s->v1 == v1)
      s->v1 = v;
    else
      s->v2 = v;
    end = i;
    i = i->next;
  }
  if (end) {
    end->next = v->segments;
    v->segments = v1->segments;
    v1->segments = NULL;
  }

  end = NULL;
  i = v2->segments;
  while (i) {
    GtsSegment * s = i->data;
    if (s->v1 == v2)
      s->v1 = v;
    else
      s->v2 = v;
    end = i;
    i = i->next;
  }
  if (end) {
    end->next = v->segments;
    v->segments = v2->segments;
    v2->segments = NULL;
  }

#ifdef DEBUG
  if (invalid) {
    char fname[80];
    FILE * fptr;
    GSList * triangles, * i;
    GtsSurface * surface = NULL;

    sprintf (fname, "invalid_after.%d", ninvalid);
    fptr = fopen (fname, "wt");
    triangles = gts_vertex_triangles (v, NULL);
    i = triangles;
    while (i) {
      GtsTriangle * t = i->data;
      fprintf (stderr, "checking %p->%d\n", t, id (t));
      g_assert (GTS_IS_FACE (t));
      gts_write_triangle (t, GTS_POINT (v), fptr);
      surface = GTS_FACE (t)->surfaces->data;
      if (gts_triangle_is_duplicate (t))
	fprintf (stderr, "%p->%d is duplicate\n", t, id (t));
      if (gts_segment_is_duplicate (GTS_SEGMENT (t->e1)))
	fprintf (stderr, "e1 of %p->%d is duplicate\n", t, id (t));
      if (gts_segment_is_duplicate (GTS_SEGMENT (t->e2)))
	fprintf (stderr, "e2 of %p->%d is duplicate\n", t, id (t));
      if (gts_segment_is_duplicate (GTS_SEGMENT (t->e3)))
	fprintf (stderr, "e3 of %p->%d is duplicate\n", t, id (t));
      i = i->next;
    }
    fclose (fptr);
    g_slist_free (triangles);
#if 0
    gts_split_expand (vs, surface);

    sprintf (fname, "invalid_after_after.%d", ninvalid);
    fptr = fopen (fname, "wt");
    triangles = gts_vertex_triangles (v1, NULL);
    triangles = gts_vertex_triangles (v2, triangles);
    i = triangles;
    while (i) {
      GtsTriangle * t = i->data;
      gts_write_triangle (t, GTS_POINT (v), fptr);
      surface = GTS_FACE (t)->surfaces->data;
      if (gts_triangle_is_duplicate (t))
	fprintf (stderr, "%p->%d is duplicate\n", t, id (t));
      if (gts_segment_is_duplicate (GTS_SEGMENT (t->e1)))
	fprintf (stderr, "e1 of %p->%d is duplicate\n", t, id (t));
      if (gts_segment_is_duplicate (GTS_SEGMENT (t->e2)))
	fprintf (stderr, "e2 of %p->%d is duplicate\n", t, id (t));
      if (gts_segment_is_duplicate (GTS_SEGMENT (t->e3)))
	fprintf (stderr, "e3 of %p->%d is duplicate\n", t, id (t));
      i = i->next;
    }
    fclose (fptr);
    g_slist_free (triangles);

    exit (1);
#endif
    ninvalid++;
  }
#endif
}

/**
 * gts_split_expand:
 * @vs: a #GtsSplit.
 * @s: a #GtsSurface.
 * @klass: a #GtsEdgeClass.
 *
 * Expands the vertex split @vs adding the newly created faces to @s. Any 
 * new edge will be of class @klass.
 */
void gts_split_expand (GtsSplit * vs, 
		       GtsSurface * s,
		       GtsEdgeClass * klass)
{
  GSList * i;
  GtsEdge * e;
  GtsVertex * v, * v1, * v2;  
  gboolean changed = FALSE;
  GtsSplitCFace * cf;
  guint j;
  
  g_return_if_fail (vs != NULL);
  g_return_if_fail (s != NULL);
  g_return_if_fail (klass != NULL);

  /* we don't want to destroy vertices */
  gts_allow_floating_vertices = TRUE;

  v1 = GTS_SPLIT_V1 (vs);
  v2 = GTS_SPLIT_V2 (vs);
  v = vs->v;
#ifdef DEBUG_EXPAND
  fprintf (stderr, "expanding %p->%d: v1: %p->%d v2: %p->%d v: %p->%d\n",
	   vs, id (vs), v1, id (v1), v2, id (v2), v, id (v));
#endif
  e = gts_edge_new (klass, v1, v2);
  cf = vs->cfaces;
  j = vs->ncf;
  while (j--) {
    cface_expand (CFACE (cf->f), cf->a1, cf->a2, e, v1, v2, v, klass);
    gts_surface_add_face (s, cf->f);
    cf++;
  }

  gts_allow_floating_vertices = FALSE;

  /* this part is described by figure "expand.fig" */
  i = v->segments;
  while (i) {
    GtsEdge * e1 = i->data;
    GtsVertex * with = NULL;
    GSList * j = e1->triangles, * next = i->next;
    // fprintf (stderr, "e1: %p->%d\n", e1, id (e1));
    while (j && !with) {
      with = GTS_OBJECT (j->data)->reserved;
      j = j->next;
    }
    if (with) {
      j = e1->triangles;
      while (j) {
	GtsTriangle * t = j->data;
	if (GTS_OBJECT (t)->reserved) {
	  g_assert (GTS_OBJECT (t)->reserved == with);
	  GTS_OBJECT (t)->reserved = NULL;
	}
	else
	  GTS_OBJECT (t)->reserved = with;
	j = j->next;
      }
      if (GTS_SEGMENT (e1)->v1 == v)
	GTS_SEGMENT (e1)->v1 = with;
      else
	GTS_SEGMENT (e1)->v2 = with;

      v->segments = g_slist_remove_link (v->segments, i);
      i->next = with->segments;
      with->segments = i;
      changed = TRUE;
    }
    if (next)
      i = next;
    else {
      /* check for infinite loop (the crossed out case in 
	 figure "expand.fig") */
      g_assert (changed);
      changed = FALSE;
      i = v->segments;
    }
  }
}

static void cface_neighbors (GtsSplitCFace * cf,
			     GtsEdge * e,
			     GtsVertex * v1,
			     GtsVertex * v2)
{
  GtsTriangle * t = GTS_TRIANGLE (cf->f), ** a;
  GtsEdge * e1 = t->e1, * e2 = t->e2, * e3 = t->e3;
  GSList * i;
  guint size;

  ROTATE_ORIENT (e, e1, e2, e3);
  if (SEGMENT_USE_VERTEX (GTS_SEGMENT (e1), v2)) {
    e3 = e1; e1 = e2; e2 = e3;
  }
  
  i = e1->triangles;
  size = g_slist_length (i)*sizeof (GtsTriangle *);
  a = cf->a1 = g_malloc (size > 0 ? size : sizeof (GtsTriangle *));
  while (i) {
    if (i->data != t)
      *(a++) = i->data;
    i = i->next;
  }
  *a = NULL;

  i = e2->triangles;
  size = g_slist_length (i)*sizeof (GtsTriangle *);
  a = cf->a2 = g_malloc (size > 0 ? size : sizeof (GtsTriangle *));
  while (i) {
    if (i->data != t)
      *(a++) = i->data;
    i = i->next;
  }
  *a = NULL;
}

/**
 * gts_split_new:
 * @klass: a #GtsSplitClass.
 * @v: a #GtsVertex.
 * @o1: either a #GtsVertex or a #GtsSplit.
 * @o2: either a #GtsVertex or a #GtsSplit.
 *
 * Creates a new #GtsSplit which would collapse @o1 and @o2 into @v. The 
 * collapse itself is not performed.
 *
 * Returns: the new #GtsSplit.
 */
GtsSplit * gts_split_new (GtsSplitClass * klass,
			  GtsVertex * v,
			  GtsObject * o1,
			  GtsObject * o2)
{
  GtsSplit * vs;
  GtsVertex * v1, * v2;
  GtsEdge * e;
  GSList * i;
  GtsSplitCFace * cf;

  g_return_val_if_fail (klass != NULL, NULL);
  g_return_val_if_fail (v != NULL, NULL);
  g_return_val_if_fail (GTS_IS_SPLIT (o1) || GTS_IS_VERTEX (o1), NULL);
  g_return_val_if_fail (GTS_IS_SPLIT (o2) || GTS_IS_VERTEX (o2), NULL);

  vs = GTS_SPLIT (gts_object_new (GTS_OBJECT_CLASS (klass)));
  vs->v = v;
  vs->v1 = o1;
  vs->v2 = o2;
  v1 = GTS_SPLIT_V1 (vs);
  v2 = GTS_SPLIT_V2 (vs);
#ifdef DYNAMIC_SPLIT
  vs->ncf = 0;
  vs->cfaces = NULL;
#else
  g_assert ((e = GTS_EDGE (gts_vertices_are_connected (v1, v2))));
  i = e->triangles;
  vs->ncf = g_slist_length (i);
  g_assert (vs->ncf > 0);
  cf = vs->cfaces = g_malloc (vs->ncf*sizeof (GtsSplitCFace));
  while (i) {
    cf->f = i->data;
    cface_neighbors (cf, e, v1, v2);
    i = i->next;
    cf++;
  }
#endif
  
  return vs;
}

static gboolean 
split_traverse_pre_order (GtsSplit *           vs,
			  GtsSplitTraverseFunc func,
			  gpointer	       data)
{
  if (func (vs, data))
    return TRUE;
  if (GTS_IS_SPLIT (vs->v1) &&
      split_traverse_pre_order (GTS_SPLIT (vs->v1), func, data))
    return TRUE;
  if (GTS_IS_SPLIT (vs->v2) &&
      split_traverse_pre_order (GTS_SPLIT (vs->v2), func, data))
    return TRUE;
  return FALSE;
}

static gboolean 
split_depth_traverse_pre_order (GtsSplit *             vs,
				guint                  depth,
				GtsSplitTraverseFunc   func,
				gpointer	       data)
{
  if (func (vs, data))
      return TRUE;
    
  depth--;
  if (!depth)
    return FALSE;

  if (GTS_IS_SPLIT (vs->v1) &&
      split_depth_traverse_pre_order (GTS_SPLIT (vs->v1), depth, func, data))
    return TRUE;
  if (GTS_IS_SPLIT (vs->v2) &&
      split_depth_traverse_pre_order (GTS_SPLIT (vs->v2), depth, func, data))
    return TRUE;
  return FALSE;
}

static gboolean 
split_traverse_post_order (GtsSplit *           vs,
			   GtsSplitTraverseFunc func,
			   gpointer	        data)
{
  if (GTS_IS_SPLIT (vs->v1) &&
      split_traverse_post_order (GTS_SPLIT (vs->v1), func, data))
    return TRUE;
  if (GTS_IS_SPLIT (vs->v2) &&
      split_traverse_post_order (GTS_SPLIT (vs->v2), func, data))
    return TRUE;
  if (func (vs, data))
    return TRUE;
  return FALSE;
}

static gboolean
split_depth_traverse_post_order (GtsSplit *           vs,
				 guint                depth,
				 GtsSplitTraverseFunc func,
				 gpointer	      data)
{
  depth--;
  if (depth) {
    if (GTS_IS_SPLIT (vs->v1) &&
	split_depth_traverse_post_order (GTS_SPLIT (vs->v1), 
					 depth, func, data))
      return TRUE;
    if (GTS_IS_SPLIT (vs->v2) &&
	split_depth_traverse_post_order (GTS_SPLIT (vs->v2),
					 depth, func, data))
      return TRUE;
  }
  if (func (vs, data))
    return TRUE;
  return FALSE;
}

/**
 * gts_split_traverse:
 * @root: the #GtsSplit to start the traversal from.
 * @order: the order in which nodes are visited - G_PRE_ORDER or G_POST_ORDER.
 * @depth: the maximum depth of the traversal. Nodes below this depth
 * will not be visited. If depth is -1 all nodes in the tree are
 * visited. If depth is 1, only the root is visited. If depth is 2,
 * the root and its children are visited. And so on.
 * @func: the function to call for each visited #GtsHSplit.
 * @data: user data to pass to the function.
 *
 * Traverses the #GtsSplit tree having @root as root. Calls @func for each
 * #GtsSplit of the tree in the order specified by @order. If order is set
 * to G_PRE_ORDER @func is called for the #GtsSplit then its children, if order
 * is set to G_POST_ORDER @func is called for the children and then for the
 * #GtsSplit.
 */
void gts_split_traverse (GtsSplit *           root,
			 GTraverseType        order,
			 gint                 depth,
			 GtsSplitTraverseFunc func,
			 gpointer             data)
{
  g_return_if_fail (root != NULL);
  g_return_if_fail (func != NULL);
  g_return_if_fail (order < G_LEVEL_ORDER);
  g_return_if_fail (depth == -1 || depth > 0);

  switch (order) {
  case G_PRE_ORDER:
    if (depth < 0)
      split_traverse_pre_order (root, func, data);
    else
      split_depth_traverse_pre_order (root, depth, func, data);
    break;
  case G_POST_ORDER:
    if (depth < 0)
      split_traverse_post_order (root, func, data);
    else
      split_depth_traverse_post_order (root, depth, func, data);
    break;
  default:
    g_assert_not_reached ();
  }
}

/**
 * gts_split_height:
 * @root: a #GtsSplit.
 *
 * Returns: the maximum height of the vertex split tree having @root as root.
 */
guint gts_split_height (GtsSplit * root)
{
  guint height = 0, tmp_height;

  g_return_val_if_fail (root != NULL, 0);

  if (GTS_IS_SPLIT (root->v1)) {
    tmp_height = gts_split_height (GTS_SPLIT (root->v1));
    if (tmp_height > height)
      height = tmp_height;
  }
  if (GTS_IS_SPLIT (root->v2)) {
    tmp_height = gts_split_height (GTS_SPLIT (root->v2));
    if (tmp_height > height)
      height = tmp_height;
  }

  return height + 1;
}

static gboolean list_array_are_identical (GSList * list, 
					  gpointer * array,
					  gpointer excluded)
{
  while (list) {
    gpointer data = list->data;
    if (data != excluded) {
      gboolean found = FALSE;
      gpointer * a = array;
      
      while (!found && *a)
	if (*(a++) == data)
	  found = TRUE;
      if (!found)
	return FALSE;
    }
    list = list->next;
  }
  return TRUE;
}

#ifndef NEW
gboolean gts_split_is_collapsable (GtsSplit * vs)
{
  guint i;
  GtsSplitCFace * cf;
  GtsVertex * v1, * v2;
  GtsEdge * e;

  g_return_val_if_fail (vs != NULL, FALSE);

  v1 = GTS_SPLIT_V1 (vs);
  v2 = GTS_SPLIT_V2 (vs);
  g_return_val_if_fail ((e = GTS_EDGE (gts_vertices_are_connected (v1, v2))),
			FALSE);

#ifdef DYNAMIC_SPLIT
  if (!gts_edge_collapse_is_valid (e))
    return FALSE;
#else 
  i = vs->ncf;
  cf = vs->cfaces;
  while (i--) {
    GtsTriangle * t = GTS_TRIANGLE (cf->f);
    GtsEdge * e1 = t->e1, * e2 = t->e2, * e3 = t->e3;

    ROTATE_ORIENT (e, e1, e2, e3);
    if (SEGMENT_USE_VERTEX (GTS_SEGMENT (e1), v2)) {
      e3 = e1; e1 = e2; e2 = e3;
    }

    if (!list_array_are_identical (e1->triangles, (gpointer *) cf->a1, t))
      return FALSE;
    if (!list_array_are_identical (e2->triangles, (gpointer *) cf->a2, t))
      return FALSE;
    
    cf++;
  }
#endif
  return TRUE;
}
#endif /* not NEW */

#ifdef DEBUG_HEXPAND
static guint expand_level = 0;

static void expand_indent (FILE * fptr)
{
  guint i = expand_level;
  while (i--)
    fputc (' ', fptr);
}
#endif

/**
 * gts_hsplit_force_expand:
 * @hs: a #GtsHSplit.
 * @hsurface: a #GtsHSurface.
 *
 * Forces the expansion of @hs by first expanding all its dependencies not
 * already expanded.
 */
void gts_hsplit_force_expand (GtsHSplit * hs,
			      GtsHSurface * hsurface)
{
  guint i;
  GtsSplitCFace * cf;

  g_return_if_fail (hs != NULL);
  g_return_if_fail (hsurface != NULL);
  g_return_if_fail (hs->nchild == 0);

#ifdef DEBUG_HEXPAND
  expand_level += 2;
#endif

  if (hs->parent && hs->parent->nchild == 0) {
#ifdef DEBUG_HEXPAND
    expand_indent (stderr); 
    fprintf (stderr, "expand parent %p\n", hs->parent);
#endif
    gts_hsplit_force_expand (hs->parent, hsurface);
  }

  i = GTS_SPLIT (hs)->ncf;
  cf = GTS_SPLIT (hs)->cfaces;
  while (i--) {
    GtsTriangle ** j, * t;

    j = cf->a1;
    while ((t = *(j++)))
      if (IS_CFACE (t)) {
#ifdef DEBUG_HEXPAND
	expand_indent (stderr); 
	fprintf (stderr, "expand a1: cf->f: %p t: %p parent_split: %p\n", 
		 cf->f,
		 t,
		 GTS_HSPLIT (CFACE (t)->parent_split));
#endif
	gts_hsplit_force_expand (GTS_HSPLIT (CFACE (t)->parent_split),
				 hsurface);
#ifdef DEBUG_HEXPAND
	g_assert (!IS_CFACE (t));
#endif
      }
    j = cf->a2;
    while ((t = *(j++)))
      if (IS_CFACE (t)) {
#ifdef DEBUG_HEXPAND
	expand_indent (stderr); 
	fprintf (stderr, "expand a2: cf->f: %p t: %p parent_split: %p\n", 
		 cf->f,
		 t,
		 GTS_HSPLIT (CFACE (t)->parent_split));
#endif
	gts_hsplit_force_expand (GTS_HSPLIT (CFACE (t)->parent_split),
				 hsurface);
      }
    cf++;
  }

  gts_hsplit_expand (hs, hsurface);

#ifdef DEBUG_HEXPAND
  expand_level -= 2; 
  expand_indent (stderr); 
  fprintf (stderr, "%p expanded\n", hs);
#endif
}

static void index_object (GtsObject * o, guint * n)
{
  o->reserved = GUINT_TO_POINTER ((*n)++);
}

static void index_face (GtsFace * f, gpointer * data)
{
  guint * nf = data[1];

  g_hash_table_insert (data[0], f, GUINT_TO_POINTER ((*nf)++));
}

/**
 * gts_psurface_write:
 * @ps: a #GtsPSurface.
 * @fptr: a file pointer.
 *
 * Writes to @fptr a GTS progressive surface description.
 */
void gts_psurface_write (GtsPSurface * ps, FILE * fptr)
{
  guint nv = 1;
  guint nf = 1;
  GHashTable * hash;
  gpointer data[2];

  g_return_if_fail (ps != NULL);
  g_return_if_fail (fptr != NULL);
  g_return_if_fail (GTS_PSURFACE_IS_CLOSED (ps));

  while (gts_psurface_remove_vertex (ps))
    ;

  GTS_POINT_CLASS (ps->s->vertex_class)->binary = FALSE;
  gts_surface_write (ps->s, fptr);
  
  gts_surface_foreach_vertex (ps->s, (GtsFunc) index_object, &nv);
  hash = g_hash_table_new (NULL, NULL);
  data[0] = hash;
  data[1] = &nf;
  gts_surface_foreach_face (ps->s, (GtsFunc) index_face, data);

  fprintf (fptr, "%u\n", ps->split->len);
  while (ps->pos) {
    GtsSplit * vs = g_ptr_array_index (ps->split, --ps->pos);
    GtsSplitCFace * scf = vs->cfaces;
    GtsVertex * v1, * v2;
    guint i = vs->ncf;

    fprintf (fptr, "%u %u",
	     GPOINTER_TO_UINT (GTS_OBJECT (vs->v)->reserved),
	     vs->ncf);
    if (GTS_OBJECT (vs)->klass->write)
      (*GTS_OBJECT (vs)->klass->write) (GTS_OBJECT (vs), fptr);
    fputc ('\n', fptr);

    v1 = GTS_IS_SPLIT (vs->v1) ? GTS_SPLIT (vs->v1)->v : GTS_VERTEX (vs->v1);
    GTS_OBJECT (v1)->reserved = GUINT_TO_POINTER (nv++);
    v2 = GTS_IS_SPLIT (vs->v2) ? GTS_SPLIT (vs->v2)->v : GTS_VERTEX (vs->v2);
    GTS_OBJECT (v2)->reserved = GUINT_TO_POINTER (nv++);

    (*GTS_OBJECT (v1)->klass->write) (GTS_OBJECT (v1), fptr);
    fputc ('\n', fptr);

    (*GTS_OBJECT (v2)->klass->write) (GTS_OBJECT (v2), fptr);
    fputc ('\n', fptr);
    
    while (i--) {
      CFace * cf = CFACE (scf->f);
      GtsTriangle ** a, * t;

      fprintf (fptr, "%u %u",
	       GPOINTER_TO_UINT (g_hash_table_lookup (hash, cf->t)),
	       cf->flags);
      if (GTS_OBJECT_CLASS (ps->s->face_class)->write)
	(*GTS_OBJECT_CLASS (ps->s->face_class)->write) (GTS_OBJECT (cf), fptr);
      fputc ('\n', fptr);

      a = scf->a1;
      while ((t = *(a++)))
	fprintf (fptr, "%u ",
		 GPOINTER_TO_UINT (g_hash_table_lookup (hash, t)));
      fprintf (fptr, "\n");

      a = scf->a2;
      while ((t = *(a++)))
	fprintf (fptr, "%u ",
		 GPOINTER_TO_UINT (g_hash_table_lookup (hash, t)));
      fprintf (fptr, "\n");

      g_hash_table_insert (hash, cf, GUINT_TO_POINTER (nf++));

      scf++;
    }

    gts_split_expand (vs, ps->s, ps->s->edge_class);
  }

  gts_surface_foreach_vertex (ps->s, 
			      (GtsFunc) gts_object_reset_reserved, NULL);
  g_hash_table_destroy (hash);
}

static guint surface_read (GtsSurface * surface, 
			   GtsFile * f,
			   GPtrArray * vertices,
			   GPtrArray * faces)
{
  GtsEdge ** edges;
  guint n, nv, ne, nf;

  g_return_val_if_fail (surface != NULL, 1);
  g_return_val_if_fail (f != NULL, 1);
  
  if (f->type != GTS_INT) {
    gts_file_error (f, "expecting an integer (number of vertices)");
    return f->line;
  }
  nv = atoi (f->token->str);

  gts_file_next_token (f);
  if (f->type != GTS_INT) {
    gts_file_error (f, "expecting an integer (number of edges)");
    return f->line;
  }
  ne = atoi (f->token->str);

  gts_file_next_token (f);
  if (f->type != GTS_INT) {
    gts_file_error (f, "expecting an integer (number of faces)");
    return f->line;
  }
  nf = atoi (f->token->str);

  gts_file_next_token (f);
  if (f->type == GTS_STRING) {
    if (f->type != GTS_STRING) {
      gts_file_error (f, "expecting a string (GtsSurfaceClass)");
      return f->line;
    }
    gts_file_next_token (f);
    if (f->type != GTS_STRING) {
      gts_file_error (f, "expecting a string (GtsFaceClass)");
      return f->line;
    }
    gts_file_next_token (f);
    if (f->type != GTS_STRING) {
      gts_file_error (f, "expecting a string (GtsEdgeClass)");
      return f->line;
    }
    gts_file_next_token (f);
    if (f->type != GTS_STRING) {
      gts_file_error (f, "expecting a string (GtsVertexClass)");
      return f->line;
    }
    if (!strcmp (f->token->str, "GtsVertexBinary"))
      GTS_POINT_CLASS (surface->vertex_class)->binary = TRUE;
    else
      gts_file_first_token_after (f, '\n');
  }
  else
    gts_file_first_token_after (f, '\n');

  g_ptr_array_set_size (vertices, nv);
  g_ptr_array_set_size (faces, nf);
  /* allocate nv + 1 just in case nv == 0 */
  edges = g_malloc ((ne + 1)*sizeof (GtsEdge *));
  
  n = 0;
  while (n < nv && f->type != GTS_ERROR) {
    GtsObject * new_vertex =
      gts_object_new (GTS_OBJECT_CLASS (surface->vertex_class));

    (* GTS_OBJECT_CLASS (surface->vertex_class)->read) (&new_vertex, f);
    if (f->type != GTS_ERROR) {
      if (!GTS_POINT_CLASS (surface->vertex_class)->binary)
	gts_file_first_token_after (f, '\n');
      g_ptr_array_index (vertices, n++) = new_vertex;
    }
    else
      gts_object_destroy (new_vertex);
  }
  if (f->type == GTS_ERROR)
    nv = n;
  if (GTS_POINT_CLASS (surface->vertex_class)->binary)
    gts_file_first_token_after (f, '\n');

  n = 0;
  while (n < ne && f->type != GTS_ERROR) {
    guint p1, p2;

    if (f->type != GTS_INT)
      gts_file_error (f, "expecting an integer (first vertex index)");
    else {
      p1 = atoi (f->token->str);
      if (p1 == 0 || p1 > nv)
	gts_file_error (f, "vertex index `%d' is out of range `[1,%d]'", 
			p1, nv);
      else {
	gts_file_next_token (f);
	if (f->type != GTS_INT)
	  gts_file_error (f, "expecting an integer (second vertex index)");
	else {
	  p2 = atoi (f->token->str);
	  if (p2 == 0 || p2 > nv)
	    gts_file_error (f, "vertex index `%d' is out of range `[1,%d]'", 
			    p2, nv);
	  else {
	    GtsEdge * new_edge =
	      gts_edge_new (surface->edge_class,
			    g_ptr_array_index (vertices, p1 - 1),
			    g_ptr_array_index (vertices, p2 - 1));

	    gts_file_next_token (f);
	    if (f->type != '\n')
	      if (GTS_OBJECT_CLASS (surface->edge_class)->read)
		(*GTS_OBJECT_CLASS (surface->edge_class)->read)
		  ((GtsObject **) &new_edge, f);
	    gts_file_first_token_after (f, '\n');
	    edges[n++] = new_edge;
	  }
	}
      }
    }
  }
  if (f->type == GTS_ERROR)
    ne = n;

  n = 0;
  while (n < nf && f->type != GTS_ERROR) {
    guint s1, s2, s3;

    if (f->type != GTS_INT)
      gts_file_error (f, "expecting an integer (first edge index)");
    else {
      s1 = atoi (f->token->str);
      if (s1 == 0 || s1 > ne)
	gts_file_error (f, "edge index `%d' is out of range `[1,%d]'", 
			s1, ne);
      else {
	gts_file_next_token (f);
	if (f->type != GTS_INT)
	  gts_file_error (f, "expecting an integer (second edge index)");
	else {
	  s2 = atoi (f->token->str);
	  if (s2 == 0 || s2 > ne)
	    gts_file_error (f, "edge index `%d' is out of range `[1,%d]'", 
			    s2, ne);
	  else {
	    gts_file_next_token (f);
	    if (f->type != GTS_INT)
	      gts_file_error (f, "expecting an integer (third edge index)");
	    else {
	      s3 = atoi (f->token->str);
	      if (s3 == 0 || s3 > ne)
		gts_file_error (f, "edge index `%d' is out of range `[1,%d]'", 
				s3, ne);
	      else {
		GtsFace * new_face = gts_face_new (surface->face_class,
						   edges[s1 - 1],
						   edges[s2 - 1],
						   edges[s3 - 1]);

		gts_file_next_token (f);
		if (f->type != '\n')
		  if (GTS_OBJECT_CLASS (surface->face_class)->read)
		    (*GTS_OBJECT_CLASS (surface->face_class)->read)
		      ((GtsObject **) &new_face, f);
		gts_file_first_token_after (f, '\n');
		gts_surface_add_face (surface, new_face);
		g_ptr_array_index (faces, n++) = new_face;
	      }
	    }
	  }
	}
      }
    }
  }

  g_free (edges);

  if (f->type == GTS_ERROR) {
    gts_allow_floating_vertices = TRUE;
    while (nv)
      gts_object_destroy (GTS_OBJECT (g_ptr_array_index (vertices, nv-- - 1)));
    gts_allow_floating_vertices = FALSE;    
    return f->line;
  }

  return 0;
}

/**
 * gts_psurface_open:
 * @klass: a #GtsPSurfaceClass.
 * @s: a #GtsSurface.
 * @split_class: a #GtsSplitClass to use for the #GtsSplit.
 * @f: a #GtsFile.
 *
 * Creates a new #GtsPSurface prepared for input from the file @f 
 * containing a valid GTS representation of a progressive surface. The initial
 * shape of the progressive surface is loaded into @s.
 * 
 * Before being usable as such this progressive surface must be closed using
 * gts_psurface_close(). While open however, the functions
 * gts_psurface_get_vertex_number(), gts_psurface_min_vertex_number() and
 * gts_psurface_max_vertex_number() can still be used.
 *
 * Returns: a new #GtsPSurface or %NULL if there was a format error while
 * reading the file, in which case @f contains information about the error.
 */
GtsPSurface * gts_psurface_open (GtsPSurfaceClass * klass,
				 GtsSurface * s,
				 GtsSplitClass * split_class,
				 GtsFile * f)
{
  GtsPSurface * ps;

  g_return_val_if_fail (klass != NULL, NULL);
  g_return_val_if_fail (s != NULL, NULL);
  g_return_val_if_fail (split_class != NULL, NULL);
  g_return_val_if_fail (f != NULL, NULL);

  ps = GTS_PSURFACE (gts_object_new (GTS_OBJECT_CLASS (klass)));
  ps->s = s;
  ps->split_class = split_class;

  ps->vertices = g_ptr_array_new ();
  ps->faces = g_ptr_array_new ();

  if (surface_read (s, f, ps->vertices, ps->faces)) {
    ps->s = NULL;
    gts_object_destroy (GTS_OBJECT (ps));
    return NULL;
  }

  ps->min = gts_surface_vertex_number (ps->s);
  ps->pos = 0;

  if (f->type == GTS_INT) {
    gint ns = atoi (f->token->str);
    
    if (ns > 0) {
      g_ptr_array_set_size (ps->split, ns);
      gts_file_first_token_after (f, '\n');
    }
  }

  return ps;
}

/**
 * gts_psurface_read_vertex:
 * @ps: a #GtsPSurface prealably created with gts_psurface_open().
 * @fp: a #GtsFile.
 *
 * Reads in one vertex split operation from @fp and performs the expansion.
 *
 * If an error occurs while reading the file, the @error field of @fp is set.
 *
 * Returns: the newly created #GtsSplit or %NULL if no vertex split could be
 * read from @fp.
 */
GtsSplit * gts_psurface_read_vertex (GtsPSurface * ps, GtsFile * fp)
{
  guint nv, ncf;
  GtsSplit * vs, * parent;
  GtsSplitCFace * scf;

  g_return_val_if_fail (ps != NULL, NULL);
  g_return_val_if_fail (fp != NULL, NULL);
  g_return_val_if_fail (!GTS_PSURFACE_IS_CLOSED (ps), NULL);
  
  if (ps->pos >= ps->split->len)
    return NULL;

  if (fp->type == GTS_NONE)
    return NULL;
  if (fp->type != GTS_INT) {
    gts_file_error (fp, "expecting an integer (vertex index)");
    return NULL;
  }
  nv = atoi (fp->token->str);
  if (nv == 0 || nv > ps->vertices->len) {
    gts_file_error (fp, "vertex index `%d' is out of range `[1,%d]'",
		    nv, ps->vertices->len);
    return NULL;
  }

  gts_file_next_token (fp);
  if (fp->type != GTS_INT) {
    gts_file_error (fp, "expecting an integer (ncf)");
    return NULL;
  }
  ncf = atoi (fp->token->str);
  
  vs = GTS_SPLIT (gts_object_new (GTS_OBJECT_CLASS (ps->split_class)));

  vs->v = g_ptr_array_index (ps->vertices, nv - 1);
  vs->v1 = vs->v2 = NULL;
  vs->cfaces = NULL;
  vs->ncf = 0;

  gts_file_next_token (fp);
  if (fp->type != '\n')
    if (GTS_OBJECT (vs)->klass->read)
      (* GTS_OBJECT (vs)->klass->read) ((GtsObject **) &vs, fp);
  gts_file_first_token_after (fp, '\n');

  if (fp->type != GTS_ERROR) {
    vs->v1 = gts_object_new (GTS_OBJECT_CLASS (ps->s->vertex_class));
    (* GTS_OBJECT_CLASS (ps->s->vertex_class)->read) (&(vs->v1), fp);
    if (fp->type != GTS_ERROR) {
      vs->v1->reserved = vs;
      g_ptr_array_add (ps->vertices, vs->v1);

      gts_file_first_token_after (fp, '\n');
      
      vs->v2 = gts_object_new (GTS_OBJECT_CLASS (ps->s->vertex_class));
      (*GTS_OBJECT_CLASS (ps->s->vertex_class)->read) (&(vs->v2), fp);
      if (fp->type != GTS_ERROR) {
	vs->v2->reserved = vs;
	g_ptr_array_add (ps->vertices, vs->v2);
	gts_file_first_token_after (fp, '\n');
      }
    }
  }

  if (fp->type != GTS_ERROR) {
    scf = vs->cfaces = g_malloc (sizeof (GtsSplitCFace)*ncf);
    while (fp->type != GTS_ERROR && ncf--) {
      guint it, flags;
      GtsFace * f;
      CFace * cf;
      GPtrArray * a;

      if (fp->type != GTS_INT)
	gts_file_error (fp, "expecting an integer (face index)");
      else {
	it = atoi (fp->token->str);
	if (it == 0 || it > ps->faces->len)
	  gts_file_error (fp, "face index `%d' is out of range `[1,%d]'",
			  it, ps->faces->len);
	else {
	  gts_file_next_token (fp);
	  if (fp->type != GTS_INT)
	    gts_file_error (fp, "expecting an integer (flags)");
	  else {
	    flags = atoi (fp->token->str);
	    f = 
	      GTS_FACE (gts_object_new (GTS_OBJECT_CLASS (ps->s->face_class)));

	    gts_file_next_token (fp);
	    if (fp->type != '\n')
	      if (GTS_OBJECT (f)->klass->read)
		(*GTS_OBJECT (f)->klass->read) ((GtsObject **) &f, fp);
	    gts_file_first_token_after (fp, '\n');
	    if (fp->type != GTS_ERROR) {
	      scf->f = f;

	      cf = (CFace *) f;
	      GTS_OBJECT (cf)->klass = GTS_OBJECT_CLASS (cface_class ());
	      cf->parent_split = vs;
	      cf->t = g_ptr_array_index (ps->faces, it - 1);
	      cf->flags = flags;
	  
	      a = g_ptr_array_new ();
	      do {
		if (fp->type != GTS_INT)
		  gts_file_error (fp, "expecting an integer (face index)");
		else {
		  it = atoi (fp->token->str);
		  if (it > ps->faces->len)
		    gts_file_error (fp, 
				    "face index `%d' is out of range `[1,%d]'",
				    it, ps->faces->len);
		  else {
		    g_ptr_array_add (a, g_ptr_array_index (ps->faces, 
							   it - 1));
		    gts_file_next_token (fp);
		  }
		}
	      } while (fp->type != GTS_ERROR && fp->type != '\n');
	      gts_file_first_token_after (fp, '\n');
	      g_ptr_array_add (a, NULL);
	      scf->a1 = (GtsTriangle **) a->pdata;
	      g_ptr_array_free (a, FALSE);

	      if (fp->type != GTS_ERROR) {
		a = g_ptr_array_new ();
		do {
		  if (fp->type != GTS_INT)
		    gts_file_error (fp, "expecting an integer (face index)");
		  else {
		    it = atoi (fp->token->str);
		    if (it > ps->faces->len)
		      gts_file_error (fp, 
				   "face index `%d' is out of range `[1,%d]'",
				      it, ps->faces->len);
		    else {
		      g_ptr_array_add (a, g_ptr_array_index (ps->faces, 
							     it - 1));
		      gts_file_next_token (fp);
		    }
		  }
		} while (fp->type != GTS_ERROR && fp->type != '\n');
		gts_file_first_token_after (fp, '\n');
		g_ptr_array_add (a, NULL);
		scf->a2 = (GtsTriangle **) a->pdata;
		g_ptr_array_free (a, FALSE);
		
		g_ptr_array_add (ps->faces, f);
	      
		vs->ncf++;
		scf++;
	      }
	    }
	  }
	}
      }
    }
  }

  if (fp->type != GTS_ERROR) {
    if ((parent = GTS_OBJECT (vs->v)->reserved)) {
      GTS_OBJECT (vs->v)->reserved = NULL;
      if (parent->v1 == GTS_OBJECT (vs->v))
	parent->v1 = GTS_OBJECT (vs);
      else {
	g_assert (parent->v2 == GTS_OBJECT (vs->v));
	parent->v2 = GTS_OBJECT (vs);
      }
    }
    g_ptr_array_index (ps->split, ps->pos++) = vs;
    gts_split_expand (vs, ps->s, ps->s->edge_class);

    return vs;
  }

  if (vs->v1) gts_object_destroy (vs->v1);
  if (vs->v2) gts_object_destroy (vs->v2);
  gts_object_destroy (GTS_OBJECT (vs));
  
  return NULL;
}

/**
 * gts_psurface_close:
 * @ps: a #GtsPSurface prealably created with gts_psurface_open().
 *
 * Closes a progressive surface.
 */
void gts_psurface_close (GtsPSurface * ps)
{
  g_return_if_fail (ps != NULL);
  g_return_if_fail (!GTS_PSURFACE_IS_CLOSED (ps));

  g_ptr_array_free (ps->vertices, TRUE);
  g_ptr_array_free (ps->faces, TRUE);
  ps->faces = ps->vertices = NULL;
  
  gts_surface_foreach_vertex (ps->s, 
			      (GtsFunc) gts_object_reset_reserved, NULL);
  if (ps->pos > 0)
    g_ptr_array_set_size (ps->split, ps->pos);
  if (ps->split->len > 1) {
    guint i, half = ps->split->len/2, n = ps->split->len - 1;
    
    for (i = 0; i < half; i++) {
      gpointer p1 = g_ptr_array_index (ps->split, i);
      gpointer p2 = g_ptr_array_index (ps->split, n - i);
      g_ptr_array_index (ps->split, n - i) = p1;
      g_ptr_array_index (ps->split, i) = p2;
    }
  }
  ps->pos = 0;
}
