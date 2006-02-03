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
#include "gts.h"

/* GtsGNode */

gboolean gts_allow_floating_gnodes = FALSE;

static void gnode_remove_container (GtsContainee * i, GtsContainer * c)
{
  (* GTS_CONTAINEE_CLASS (GTS_OBJECT_CLASS (gts_gnode_class ())->parent_class)->remove_container) (i, c);
  if (GTS_SLIST_CONTAINEE (i)->containers == NULL && 
      !gts_allow_floating_gnodes &&
      !GTS_OBJECT_DESTROYED(GTS_OBJECT (i)))
    gts_object_destroy (GTS_OBJECT (i));
}

static void gnode_class_init (GtsGNodeClass * klass)
{
  klass->weight = NULL;

  GTS_CONTAINEE_CLASS (klass)->remove_container = gnode_remove_container;
}

static void gnode_init (GtsGNode * n)
{
  n->level = 0;
}

/**
 * gts_gnode_class:
 * 
 * Returns: the #GtsGNodeClass.
 */
GtsGNodeClass * gts_gnode_class (void)
{
  static GtsGNodeClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo gnode_info = {
      "GtsGNode",
      sizeof (GtsGNode),
      sizeof (GtsGNodeClass),
      (GtsObjectClassInitFunc) gnode_class_init,
      (GtsObjectInitFunc) gnode_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = 
      gts_object_class_new (GTS_OBJECT_CLASS (gts_slist_container_class ()),
			    &gnode_info);
  }

  return klass;
}

/**
 * gts_gnode_new:
 * @klass: a #GtsGNodeClass.
 *
 * Returns: a new #GtsGNode.
 */
GtsGNode * gts_gnode_new (GtsGNodeClass * klass)
{
  GtsGNode * object;

  object = GTS_GNODE (gts_object_new (GTS_OBJECT_CLASS (klass)));

  return object;
}

/**
 * gts_gnode_foreach_neighbor:
 * @n: a #GtsGNode.
 * @g: a #GtsGraph or %NULL.
 * @func: a #GtsFunc.
 * @data: user data to be passed to @func.
 *
 * Calls @func for each neighbor #GtsGNode of @n (belonging to @g if
 * @g is not %NULL.  
 */
void gts_gnode_foreach_neighbor (GtsGNode * n, 
				 GtsGraph * g,
				 GtsFunc func,
				 gpointer data)
{
  GSList * i;

  g_return_if_fail (n != NULL);
  g_return_if_fail (func != NULL);

  i = GTS_SLIST_CONTAINER (n)->items;
  while (i) {
    GtsGNode * n1 = GTS_GNODE_NEIGHBOR (n, i->data);
    if (g == NULL || gts_containee_is_contained (GTS_CONTAINEE (n1),
						 GTS_CONTAINER (g)))
      (* func) (n1, data);
    i = i->next;
  }
}

/**
 * gts_gnode_foreach_edge:
 * @n: a #GtsGNode.
 * @g: a #GtsGraph or %NULL.
 * @func: a #GtsFunc.
 * @data: user data to be passed to @func.
 *
 * Calls @func for each #GtsGEdge connecting @n to another #GtsGNode
 * (belonging to @g if @g is not %NULL.  
 */
void gts_gnode_foreach_edge (GtsGNode * n, 
			     GtsGraph * g,
			     GtsFunc func,
			     gpointer data)
{
  GSList * i;

  g_return_if_fail (n != NULL);
  g_return_if_fail (func != NULL);

  i = GTS_SLIST_CONTAINER (n)->items;
  while (i) {
    GtsGNode * n1 = GTS_GNODE_NEIGHBOR (n, i->data);
    if (g == NULL || gts_containee_is_contained (GTS_CONTAINEE (n1),
						 GTS_CONTAINER (g)))
      (* func) (i->data, data);
    i = i->next;
  }
}

/**
 * gts_gnode_degree:
 * @n: a #GtsGNode.
 * @g: a #GtsGraph or %NULL.
 *
 * Returns: the number of neighbors of @n (belonging to @g if @g is not %NULL).
 */
guint gts_gnode_degree (GtsGNode * n,
			GtsGraph * g)
{
  GSList * i;
  guint nn = 0;

  g_return_val_if_fail (n != NULL, 0);

  i = GTS_SLIST_CONTAINER (n)->items;
  while (i) {
    GtsGNode * n1 = GTS_GNODE_NEIGHBOR (n, i->data);
    if (g == NULL || gts_containee_is_contained (GTS_CONTAINEE (n1),
						 GTS_CONTAINER (g)))
      nn++;
    i = i->next;
  }

  return nn;
}

/**
 * gts_gnode_move_cost:
 * @n: a #GtsGNode.
 * @src: a #GtsGraph containing @n.
 * @dst: another #GtsGraph.
 *
 * Returns: the cost (increase in the sum of the weights of the edges cut) of
 * moving @n from @src to @dst.
 */
gfloat gts_gnode_move_cost (GtsGNode * n,
			    GtsGraph * src,
			    GtsGraph * dst)
{
  GSList * i;
  gfloat cost = 0.;
  
  g_return_val_if_fail (n != NULL, G_MAXFLOAT);
  g_return_val_if_fail (src != NULL, G_MAXFLOAT);
  g_return_val_if_fail (dst != NULL, G_MAXFLOAT);
  g_return_val_if_fail (gts_containee_is_contained (GTS_CONTAINEE (n),
						    GTS_CONTAINER (src)),
			G_MAXFLOAT);

  i = GTS_SLIST_CONTAINER (n)->items;
  while (i) {
    GtsGEdge * ge = i->data;
    GtsGNode * neighbor = GTS_GNODE_NEIGHBOR (n, ge);

    if (gts_containee_is_contained (GTS_CONTAINEE (neighbor), 
				    GTS_CONTAINER (src)))
      cost += gts_gedge_weight (ge);
    else if (gts_containee_is_contained (GTS_CONTAINEE (neighbor), 
					 GTS_CONTAINER (dst)))
      cost -= gts_gedge_weight (ge);
    i = i->next;
  }
  
  return cost;
}

/**
 * gts_gnode_weight:
 * @n: a #GtsGNode.
 *
 * Returns: the weight of @n as defined by the weight() method of the
 * #GtsGNodeClass.  
 */
gfloat gts_gnode_weight (GtsGNode * n)
{
  g_return_val_if_fail (n != NULL, 0.);

  if (GTS_GNODE_CLASS (GTS_OBJECT (n)->klass)->weight)
    return (* GTS_GNODE_CLASS (GTS_OBJECT (n)->klass)->weight) (n);
  return 1.;
}

/* GtsNGNode */

static void ngnode_init (GtsNGNode * n)
{
  n->id = 0;
}

/**
 * gts_ngnode_class:
 *
 * Returns: the #GtsNGNodeClass.
 */
GtsNGNodeClass * gts_ngnode_class (void)
{
  static GtsNGNodeClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo ngnode_info = {
      "GtsNGNode",
      sizeof (GtsNGNode),
      sizeof (GtsNGNodeClass),
      (GtsObjectClassInitFunc) NULL,
      (GtsObjectInitFunc) ngnode_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_gnode_class ()),
				  &ngnode_info);
  }

  return klass;
}

/**
 * gts_ngnode_new:
 * @klass: a #GtsNGNodeClass.
 *
 * Returns: a new #GtsNGNode with identity @id.
 */
GtsNGNode * gts_ngnode_new (GtsNGNodeClass * klass,
			    guint id)
{
  GtsNGNode * n;

  n = GTS_NGNODE (gts_gnode_new (GTS_GNODE_CLASS (klass)));
  n->id = id;

  return n;
}

/* GtsWGNode */

static gfloat wgnode_weight (GtsGNode * n)
{
  return GTS_WGNODE (n)->weight;
}

static void wgnode_class_init (GtsWGNodeClass * klass)
{
  GTS_GNODE_CLASS (klass)->weight = wgnode_weight;
}

static void wgnode_init (GtsWGNode * n)
{
  n->weight = 1.;
}

/**
 * gts_wgnode_class:
 *
 * Returns: the #GtsWGNodeClass.
 */
GtsWGNodeClass * gts_wgnode_class (void)
{
  static GtsWGNodeClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo wgnode_info = {
      "GtsWGNode",
      sizeof (GtsWGNode),
      sizeof (GtsWGNodeClass),
      (GtsObjectClassInitFunc) wgnode_class_init,
      (GtsObjectInitFunc) wgnode_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_gnode_class ()),
				  &wgnode_info);
  }

  return klass;
}

/**
 * gts_wgnode_new:
 * @klass: a #GtsWGNodeClass.
 * @weight: the weight of the #GtsWGNode to create.
 *
 * Returns: a new #GtsWGNode of weight @weight.
 */
GtsWGNode * gts_wgnode_new (GtsWGNodeClass * klass,
			    gfloat weight)
{
  GtsWGNode * n;

  n = GTS_WGNODE (gts_gnode_new (GTS_GNODE_CLASS (klass)));
  n->weight = weight;

  return n;
}

/* GtsPNode */

static void pnode_write (GtsGNode * n, FILE * fp)
{
  if (GTS_IS_NVERTEX (GTS_PNODE (n)->data))
    fprintf (fp, "label=\"%p:%s\",", 
	     GTS_PNODE (n)->data,
	     GTS_NVERTEX (GTS_PNODE (n)->data)->name);
  else
    fprintf (fp, "label=\"%p\",", GTS_PNODE (n)->data);
}

static void pnode_class_init (GtsPNodeClass * klass)
{
  GTS_GNODE_CLASS (klass)->write = pnode_write;
}

static void pnode_init (GtsPNode * pn)
{
  pn->data = NULL;
}

/**
 * gts_pnode_class:
 *
 * Returns: the #GtsPNodeClass.
 */
GtsPNodeClass * gts_pnode_class (void)
{
  static GtsPNodeClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo pnode_info = {
      "GtsPNode",
      sizeof (GtsPNode),
      sizeof (GtsPNodeClass),
      (GtsObjectClassInitFunc) pnode_class_init,
      (GtsObjectInitFunc) pnode_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_gnode_class ()),
				  &pnode_info);
  }

  return klass;
}

/**
 * gts_pnode_new:
 * @klass: a #GtsPNodeClass.
 * @data: user data.
 *
 * Returns: a new #GtsPNode associated with @data.
 */
GtsPNode * gts_pnode_new (GtsPNodeClass * klass, gpointer data)
{
  GtsPNode * pn;

  pn = GTS_PNODE (gts_object_new (GTS_OBJECT_CLASS (klass)));
  pn->data = data;

  return pn;
}

/* GtsFNode */

static void fnode_write (GtsGNode * n, FILE * fp)
{
  fprintf (fp, "label=\"%p\",", GTS_FNODE (n)->f);
}

static void fnode_class_init (GtsGNodeClass * klass)
{
  klass->write = fnode_write;
}

static void fnode_init (GtsFNode * fn)
{
  fn->f = NULL;
}

/**
 * gts_fnode_class:
 *
 * Returns: the #GtsFNodeClass.
 */
GtsFNodeClass * gts_fnode_class (void)
{
  static GtsFNodeClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo fnode_info = {
      "GtsFNode",
      sizeof (GtsFNode),
      sizeof (GtsFNodeClass),
      (GtsObjectClassInitFunc) fnode_class_init,
      (GtsObjectInitFunc) fnode_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_gnode_class ()),
				  &fnode_info);
  }

  return klass;
}

/**
 * gts_fnode_new:
 * @klass: a #GtsFNodeClass.
 * @f: a #GtsFace.
 *
 * Returns: a new #GtsFNode associated with face @f.
 */
GtsFNode * gts_fnode_new (GtsFNodeClass * klass, GtsFace * f)
{
  GtsFNode * fn;

  g_return_val_if_fail (f != NULL, NULL);

  fn = GTS_FNODE (gts_object_new (GTS_OBJECT_CLASS (klass)));
  fn->f = f;

  return fn;
}

/* GtsGEdge */

static void gedge_destroy (GtsObject * object)
{
  GtsGEdge * ge = GTS_GEDGE (object);

  if (ge->n1)
    gts_container_remove (GTS_CONTAINER (ge->n1), GTS_CONTAINEE (ge));
  if (ge->n2)
    gts_container_remove (GTS_CONTAINER (ge->n2), GTS_CONTAINEE (ge));

  (* GTS_OBJECT_CLASS (gts_gedge_class ())->parent_class->destroy) (object);
}

static void gedge_remove_container (GtsContainee * i, GtsContainer * c)
{
  GtsGEdge * ge = GTS_GEDGE (i);
  GtsGNode * n1 = ge->n1;
  GtsGNode * n2 = ge->n2;

  ge->n1 = ge->n2 = NULL;
  if (n1 != NULL && n2 != NULL) {
    if (GTS_CONTAINER (n1) == c) {
      if (n2 && n2 != n1) gts_container_remove (GTS_CONTAINER (n2), i);
    }
    else if (GTS_CONTAINER (n2) == c) {
      if (n1 && n1 != n2) gts_container_remove (GTS_CONTAINER (n1), i);
    }
    else
      g_assert_not_reached ();
    (* GTS_OBJECT_CLASS (gts_gedge_class ())->parent_class->destroy)
      (GTS_OBJECT (i));
  }
}

static gboolean gedge_is_contained (GtsContainee * i, GtsContainer * c)
{
  GtsGEdge * ge = GTS_GEDGE (i);

  if (GTS_CONTAINER (ge->n1) == c || GTS_CONTAINER (ge->n2) == c)
    return TRUE;
  return FALSE;
}

static void gedge_class_init (GtsGEdgeClass * klass)
{
  klass->link = NULL;
  klass->weight = NULL;

  GTS_CONTAINEE_CLASS (klass)->remove_container = gedge_remove_container;
  GTS_CONTAINEE_CLASS (klass)->is_contained = gedge_is_contained;

  GTS_OBJECT_CLASS (klass)->destroy = gedge_destroy;
}

static void gedge_init (GtsGEdge * object)
{
  object->n1 = object->n2 = NULL;
}

/**
 * gts_gedge_class:
 *
 * Returns: the #GtsGEdgeClass.
 */
GtsGEdgeClass * gts_gedge_class (void)
{
  static GtsGEdgeClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo gedge_info = {
      "GtsGEdge",
      sizeof (GtsGEdge),
      sizeof (GtsGEdgeClass),
      (GtsObjectClassInitFunc) gedge_class_init,
      (GtsObjectInitFunc) gedge_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_containee_class ()),
				  &gedge_info);
  }

  return klass;
}

/**
 * gts_gedge_new:
 * @klass: a #GtsGEdgeClass.
 * @n1: a #GtsGNode.
 * @n2: another #GtsGNode.
 *
 * Returns: a new #GtsGEdge linking @n1 and @n2.
 */
GtsGEdge * gts_gedge_new (GtsGEdgeClass * klass, GtsGNode * n1, GtsGNode * n2)
{
  GtsGEdge * object;

  g_return_val_if_fail (n1 != NULL, NULL);
  g_return_val_if_fail (n2 != NULL, NULL);

  object = GTS_GEDGE (gts_object_new (GTS_OBJECT_CLASS (klass)));
  object->n1 = n1;
  gts_container_add (GTS_CONTAINER (n1), GTS_CONTAINEE (object));
  object->n2 = n2;
  if (n1 != n2)
    gts_container_add (GTS_CONTAINER (n2), GTS_CONTAINEE (object));

  if (klass->link)
    object = (* klass->link) (object, n1, n2);

  return object;
}

/**
 * gts_gedge_weight:
 * @e: a #GtsGEdge.
 *
 * Returns: the weight of edge @e as defined by the weight() method of
 * #GtsGEdgeClass.  
 */
gfloat gts_gedge_weight (GtsGEdge * e)
{
  g_return_val_if_fail (e != NULL, 0.);

  if (GTS_GEDGE_CLASS (GTS_OBJECT (e)->klass)->weight)
    return (* GTS_GEDGE_CLASS (GTS_OBJECT (e)->klass)->weight) (e);
  return 1.;
}

/* GtsPGEdge */

static void pgedge_write (GtsGEdge * ge, FILE * fp)
{
  if (GTS_IS_EDGE (GTS_PGEDGE (ge)->data)) {
    GtsEdge * e = GTS_PGEDGE (ge)->data;
    guint n = g_slist_length (e->triangles);

    fprintf (fp, "label=\"%p:%s:%d\",color=%s", e,
	     GTS_IS_NEDGE (e) ? GTS_NEDGE (e)->name : "",
	     n,
	     n == 0 ? "black" : 
             n == 1 ? "blue" :
	     n == 2 ? "green" :
	     n == 3 ? "violet" :
	     n == 4 ? "red" : 
	     "pink");
  }
  else
    fprintf (fp, "label=\"%p\",", GTS_PGEDGE (ge)->data);
}

static void pgedge_class_init (GtsPGEdgeClass * klass)
{
  GTS_GEDGE_CLASS (klass)->write = pgedge_write;
}

static void pgedge_init (GtsPGEdge * e)
{
  e->data = NULL;
}

/**
 * gts_pgedge_class:
 * 
 * Returns: the #GtsPGEdgeClass.
 */
GtsPGEdgeClass * gts_pgedge_class (void)
{
  static GtsPGEdgeClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo pgedge_info = {
      "GtsPGEdge",
      sizeof (GtsPGEdge),
      sizeof (GtsPGEdgeClass),
      (GtsObjectClassInitFunc) pgedge_class_init,
      (GtsObjectInitFunc) pgedge_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_gedge_class ()),
				  &pgedge_info);
  }

  return klass;
}

/**
 * gts_pgedge_new:
 * @klass: a #GtsPGEdgeClass.
 * @n1: a #GtsGNode.
 * @n2: another #GtsGNode.
 * @data: user data.
 *
 * Returns: a new #GtsPGEdge associated with @data linking @n1 and @n2.
 */ 
GtsPGEdge * gts_pgedge_new (GtsPGEdgeClass * klass,
			    GtsGNode * g1,
			    GtsGNode * g2,
			    gpointer data)
{
  GtsPGEdge * we;

  we = GTS_PGEDGE (gts_gedge_new (GTS_GEDGE_CLASS (klass), g1, g2));
  we->data = data;

  return we;
}

/* GtsWGEdge */

static gfloat wgedge_weight (GtsGEdge * e)
{
  return GTS_WGEDGE (e)->weight;
}

static void wgedge_class_init (GtsWGEdgeClass * klass)
{
  GTS_GEDGE_CLASS (klass)->weight = wgedge_weight;
}

static void wgedge_init (GtsWGEdge * e)
{
  e->weight = 1.;
}

/**
 * gts_wgedge_class:
 * 
 * Returns: the #GtsWGEdgeClass.
 */
GtsWGEdgeClass * gts_wgedge_class (void)
{
  static GtsWGEdgeClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo wgedge_info = {
      "GtsWGEdge",
      sizeof (GtsWGEdge),
      sizeof (GtsWGEdgeClass),
      (GtsObjectClassInitFunc) wgedge_class_init,
      (GtsObjectInitFunc) wgedge_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_gedge_class ()),
				  &wgedge_info);
  }

  return klass;
}

/**
 * gts_wgedge_new:
 * @klass: a #GtsWGEdgeClass.
 * @n1: a #GtsGNode.
 * @n2: another #GtsGNode.
 * @weight: the weight of the new edge.
 *
 * Returns: a new #GtsWGEdge of weight @weight linking @n1 and @n2.
 */ 
GtsWGEdge * gts_wgedge_new (GtsWGEdgeClass * klass,
			    GtsGNode * g1,
			    GtsGNode * g2,
			    gfloat weight)
{
  GtsWGEdge * we;

  we = GTS_WGEDGE (gts_gedge_new (GTS_GEDGE_CLASS (klass), g1, g2));
  we->weight = weight;

  return we;
}

/* GtsGraph */

static void graph_init (GtsGraph * g)
{
  g->graph_class = gts_graph_class ();
  g->node_class  = gts_gnode_class ();
  g->edge_class  = gts_gedge_class ();
}

static void graph_write (GtsObject * object, FILE * fp)
{
  GtsGraph * graph = GTS_GRAPH (object);

  fprintf (fp, " %s %s %s",
	   object->klass->info.name,
	   GTS_OBJECT_CLASS (graph->node_class)->info.name,
	   GTS_OBJECT_CLASS (graph->edge_class)->info.name);
}

static void graph_read (GtsObject ** object, GtsFile * f)
{
  GtsObjectClass * klass;

  if (f->type != GTS_STRING) {
    gts_file_error (f, "expecting a string (GtsGNodeClass)");
    return;
  }
  klass = gts_object_class_from_name (f->token->str);
  if (klass == NULL) {
    gts_file_error (f, "unknown class `%s'", f->token->str);
    return;
  }
  if (!gts_object_class_is_from_class (klass, gts_gnode_class ())) {
    gts_file_error (f, "class `%s' is not a GtsGNodeClass", f->token->str);
    return;
  }
  GTS_GRAPH (*object)->node_class = GTS_GNODE_CLASS (klass);
  gts_file_next_token (f);

  if (f->type != GTS_STRING) {
    gts_file_error (f, "expecting a string (GtsGEdgeClass)");
    return;
  }
  klass = gts_object_class_from_name (f->token->str);
  if (klass == NULL) {
    gts_file_error (f, "unknown class `%s'", f->token->str);
    return;
  }
  if (!gts_object_class_is_from_class (klass, gts_gedge_class ())) {
    gts_file_error (f, "class `%s' is not a GtsGEdgeClass", f->token->str);
    return;
  }
  GTS_GRAPH (*object)->edge_class = GTS_GEDGE_CLASS (klass);
  gts_file_next_token (f);
}

static void graph_class_init (GtsGraphClass * klass)
{
  klass->weight = NULL;

  GTS_OBJECT_CLASS (klass)->write = graph_write;
  GTS_OBJECT_CLASS (klass)->read = graph_read;
}

/**
 * gts_graph_class:
 *
 * Returns: the #GtsGraphClass.
 */
GtsGraphClass * gts_graph_class (void)
{
  static GtsGraphClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo graph_info = {
      "GtsGraph",
      sizeof (GtsGraph),
      sizeof (GtsGraphClass),
      (GtsObjectClassInitFunc) graph_class_init,
      (GtsObjectInitFunc) graph_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_hash_container_class ()),
				  &graph_info);
  }

  return klass;
}

/**
 * gts_graph_new:
 * @klass: a #GtsGraphClass.
 * @node_class: a #GtsGNodeClass.
 * @edge_class: a #GtsGEdgeClass.
 *
 * Returns: a new #GtsGraph using @node_class and @edge_class as node types.
 */
GtsGraph * gts_graph_new (GtsGraphClass * klass,
			  GtsGNodeClass * node_class,
			  GtsGEdgeClass * edge_class)
{
  GtsGraph * g;

  g_return_val_if_fail (klass != NULL, NULL);
  g_return_val_if_fail (node_class != NULL, NULL);
  g_return_val_if_fail (edge_class != NULL, NULL);

  g = GTS_GRAPH (gts_object_new (GTS_OBJECT_CLASS (klass)));
  g->node_class = node_class;
  g->edge_class = edge_class;

  return g;
}

static void compute_degree (GtsGNode * n, gpointer * data)
{
  GtsGraph * g = data[0];
  GtsRange * degree = data[1];

  gts_range_add_value (degree, gts_gnode_degree (n, g));
}

/**
 * gts_graph_print_stats:
 * @g: a #GtsGraph.
 * @fp: a file pointer.
 *
 * Writes to @fp a summary of the properties of @g.
 */
void gts_graph_print_stats (GtsGraph * g, FILE * fp)
{
  GtsRange degree;
  gpointer data[2];

  g_return_if_fail (g != NULL);
  g_return_if_fail (fp != NULL);

  fprintf (fp, "# nodes: %d weight: %g\n", 
	   gts_container_size (GTS_CONTAINER (g)),
	   gts_graph_weight (g));
  fprintf (fp, "#   degree: ");
  gts_range_init (&degree);
  data[0] = g;
  data[1] = &degree;
  gts_container_foreach (GTS_CONTAINER (g), (GtsFunc) compute_degree, data);
  gts_range_update (&degree);
  gts_range_print (&degree, fp);
  fprintf (fp, "\n");
  fprintf (fp, "#   edges cut: %d edges cut weight: %g\n", 
	   gts_graph_edges_cut (g),
	   gts_graph_edges_cut_weight (g));
}

struct _GtsGraphTraverse {
  GtsFifo * q;
  GtsGraph * g;
};

static void reset_level (GtsGNode * n)
{
  n->level = 0;
}

/**
 * gts_graph_traverse_new:
 * @g: a #GtsGraph.
 * @n: a #GtsGNode belonging to @g.
 * @type: the type of traversal.
 * @reinit: if %TRUE, the traversal is reinitialized.
 *
 * Returns: a new #GtsGraphTraverse initialized for the traversal of
 * @g of type @type, starting from @n.  
 */
GtsGraphTraverse * gts_graph_traverse_new (GtsGraph * g, 
					   GtsGNode * n,
					   GtsTraverseType type,
					   gboolean reinit)
{
  GtsGraphTraverse * t;

  g_return_val_if_fail (g != NULL, NULL);
  g_return_val_if_fail (n != NULL, NULL);
  g_return_val_if_fail (gts_containee_is_contained (GTS_CONTAINEE (n), 
						    GTS_CONTAINER (g)), 
			NULL);

  if (reinit)
    gts_container_foreach (GTS_CONTAINER (g), (GtsFunc) reset_level, NULL);

  t = g_malloc (sizeof (GtsGraphTraverse));
  t->q = gts_fifo_new ();
  t->g = g;
  n->level = 1;
  gts_fifo_push (t->q, n);

  return t;
}

static void push_neighbor (GtsGNode * n, gpointer * data)
{
  GtsFifo * q = data[0];
  GtsGNode * u = data[1];

  if (n->level == 0) {
    n->level = u->level + 1;
    gts_fifo_push (q, n);
  }
}

/**
 * gts_graph_traverse_next:
 * @t: a #GtsGraphTraverse.
 *
 * Returns: the next #GtsGNode of the traversal defined by @t or %NULL
 * if the traversal is complete.
 */
GtsGNode * gts_graph_traverse_next (GtsGraphTraverse * t) 
{ 
  GtsGNode * u;

  g_return_val_if_fail (t != NULL, NULL);

  u = gts_fifo_pop (t->q);
  if (u) {
    gpointer data[2];

    data[0] = t->q;
    data[1] = u;
    gts_gnode_foreach_neighbor (u, t->g, (GtsFunc) push_neighbor, data);
  }
  
  return u;
}

/**
 * gts_graph_traverse_what_next:
 * @t: a #GtsGraphTraverse.
 *
 * Returns: the next #GtsGNode of the traversal defined by @t or %NULL
 * if the traversal is complete but without advancing the traversal.
 */
GtsGNode * gts_graph_traverse_what_next (GtsGraphTraverse * t)
{
  g_return_val_if_fail (t != NULL, NULL);

  return gts_fifo_top (t->q);
}

/**
 * gts_graph_traverse_destroy:
 * @t: a #GtsGraphTraverse.
 *
 * Frees all the memory allocated for @t.
 */
void gts_graph_traverse_destroy (GtsGraphTraverse * t)
{
  g_return_if_fail (t != NULL);

  gts_fifo_destroy (t->q);
  g_free (t);
}

static void edge_foreach_node (GtsGNode * n, gpointer * info)
{
  GtsFunc func = (GtsFunc) info[0];
  gpointer data = info[1];
  GHashTable * hash = info[2];
  GSList * i = GTS_SLIST_CONTAINER (n)->items;

  while (i) {
    GtsGEdge * e = i->data;
    if (!g_hash_table_lookup (hash, e)) {
      (* func) (e, data);
      g_hash_table_insert (hash, e, e);
    }
    i = i->next;
  }  
}

/**
 * gts_graph_foreach_edge:
 * @g: a #GtsGraph.
 * @func: a #GtsFunc.
 * @data: user data to be passed to @func.
 *
 * Calls @func for each #GtsEdge of @g.
 */
void gts_graph_foreach_edge (GtsGraph * g, GtsFunc func, gpointer data)
{
  gpointer info[3];
  GHashTable * hash;

  g_return_if_fail (g != NULL);
  g_return_if_fail (func != NULL);

  info[0] = func;
  info[1] = data;
  info[2] = hash = g_hash_table_new (NULL, NULL);
  gts_container_foreach (GTS_CONTAINER (g), (GtsFunc) edge_foreach_node, info);
  g_hash_table_destroy (hash);
}

/**
 * gts_graph_weight:
 * @g: a #GtsGraph.
 *
 * Returns: the weight of graph @g as defined by the weight() method
 * of #GtsGraphClass. 
 */
gfloat gts_graph_weight (GtsGraph * g)
{
  g_return_val_if_fail (g != NULL, 0.);

  if (GTS_GRAPH_CLASS (GTS_OBJECT (g)->klass)->weight)
    return (* GTS_GRAPH_CLASS (GTS_OBJECT (g)->klass)->weight) (g);
  return (gfloat) gts_container_size (GTS_CONTAINER (g));
}

/**
 * gts_graph_distance_sum:
 * @g: a #GtsGraph.
 * @center: a #GtsGNode of @g.
 *
 * Returns: the sum of the distances between all the other #GtsGNode
 * of @g and @center.  
 */
guint gts_graph_distance_sum (GtsGraph * g, GtsGNode * center)
{
  GtsGraphTraverse * t;
  GtsGNode * n;
  guint sum = 0;

  g_return_val_if_fail (g != NULL, 0);
  g_return_val_if_fail (center != NULL, 0);

  t = gts_graph_traverse_new (g, center, GTS_BREADTH_FIRST, TRUE);
  while ((n = gts_graph_traverse_next (t)))
    sum += n->level - 1;
  gts_graph_traverse_destroy (t);

  return sum;
}

/**
 * gts_graph_farthest:
 * @g: a #GtsGraph.
 * @gnodes: a list of #GtsGNode belonging to @g.
 *
 * Returns: the #GtsGNode belonging to @g and farthest from all the nodes in
 * @gnodes (hmmm, definition of "farthest"?).
 */
GtsGNode * gts_graph_farthest (GtsGraph * g, GSList * gnodes)
{
  GtsGNode * farthest = NULL;
  GSList * i;
  gboolean reinit = TRUE, changed = TRUE;
  guint level = 1;

  g_return_val_if_fail (g != NULL, NULL);

  /* initialize traversals */
  i = gnodes;
  while (i) {
    GTS_OBJECT (i->data)->reserved = 
      gts_graph_traverse_new (g, i->data, GTS_BREADTH_FIRST, reinit);
    reinit = FALSE;
    i = i->next;
  }

  while (changed) {
    changed = FALSE;
    i = gnodes;
    while (i) {
      GtsGraphTraverse * t = GTS_OBJECT (i->data)->reserved;
      GtsGNode * n;
      while ((n = gts_graph_traverse_what_next (t)) && n->level == level) {
	changed = TRUE;
	farthest = n;
	gts_graph_traverse_next (t);
      }
      i = i->next;
    }
    level++;
  }

  /* destroy traversals */
  i = gnodes;
  while (i) {
    gts_graph_traverse_destroy (GTS_OBJECT (i->data)->reserved);
    GTS_OBJECT (i->data)->reserved = NULL;
    i = i->next;
  }
  return farthest;
}

static void neighbor_count (GtsGNode * n, gpointer * data)
{
  guint * cuts = data[0];
  GtsGraph * g = data[1];
  
  if (!gts_containee_is_contained (GTS_CONTAINEE (n), GTS_CONTAINER (g)))
    (*cuts)++;
}

static void count_edge_cuts (GtsGNode * n, gpointer * data)
{
  gts_gnode_foreach_neighbor (n, NULL, (GtsFunc) neighbor_count, data);
}

/**
 * gts_graph_edges_cut:
 * @g: a #GtsGraph.
 *
 * Returns: the number of edges of @g connecting nodes belonging to @g
 * to nodes not belonging to @g.  
 */
guint gts_graph_edges_cut (GtsGraph * g)
{
  guint cuts = 0;
  gpointer data[2];

  g_return_val_if_fail (g != NULL, 0);

  data[0] = &cuts;
  data[1] = g;
  gts_container_foreach (GTS_CONTAINER (g), (GtsFunc) count_edge_cuts, data);

  return cuts;
}

static void sum_edge_cuts_weight (GtsGNode * n, gpointer * data)
{
  gfloat * weight = data[0];
  GtsGraph * g = data[1];
  GSList * i = GTS_SLIST_CONTAINER (n)->items;

  while (i) {
    GtsGNode * n1 = GTS_GNODE_NEIGHBOR (n, i->data);
    if (!gts_containee_is_contained (GTS_CONTAINEE (n1), GTS_CONTAINER (g)))
      *weight += gts_gedge_weight (i->data);
    i = i->next;
  }
}

/**
 * gts_graph_edges_cut_weight:
 * @g: a #GtsGraph.
 *
 * Returns: the sum of the weights of the edges of @g connecting nodes
 * belonging to @g to nodes not belonging to @g.
 */
gfloat gts_graph_edges_cut_weight (GtsGraph * g)
{
  gfloat weight = 0.;
  gpointer data[2];

  g_return_val_if_fail (g != NULL, 0);

  data[0] = &weight;
  data[1] = g;
  gts_container_foreach (GTS_CONTAINER (g), (GtsFunc) sum_edge_cuts_weight, 
			 data);

  return weight;
}

/**
 * gts_graph_read_jostle:
 * @g: a #GtsGraph.
 * @fp: a #GtsFile.
 *
 * Adds to @g the nodes and edges defined in the file pointed to by
 * @fp. This file must use the Jostle "graph" ASCII format.  
 * The nodes created are of type #GtsNGNode and their identities are the
 * line number at which they appear in @fp.
 *
 * Returns: 0 if the lecture was successful, the line number at which
 * an error occured otherwise (in which case the @error field of @fp
 * is set).  
 */
guint gts_graph_read_jostle (GtsGraph * g, GtsFile * fp)
{
  guint nn, ne, n;
  GtsGNode ** nodes;

  g_return_val_if_fail (g != NULL, 1);
  g_return_val_if_fail (fp != NULL, 1);

  if (fp->type != GTS_INT) {
    gts_file_error (fp, "expecting an integer (number of nodes)");
    return fp->line;
  }
  nn = atoi (fp->token->str);
  gts_file_next_token (fp);

  if (fp->type != GTS_INT) {
    gts_file_error (fp, "expecting an integer (number of edges)");
    return fp->line;
  }
  ne = atoi (fp->token->str);

  gts_file_first_token_after (fp, '\n');
  nodes = g_malloc (sizeof (GtsGNode *)*(nn + 1));

  n = 0;
  while (n < nn && fp->type != GTS_ERROR) {
    GtsNGNode * node = gts_ngnode_new (gts_ngnode_class (), fp->line);
    
    nodes[n++] = GTS_GNODE (node);
    gts_container_add (GTS_CONTAINER (g), GTS_CONTAINEE (node));
    do {
      if (fp->type != GTS_INT)
	gts_file_error (fp, "expecting an integer (node index)");
      else {
	guint in = atoi (fp->token->str);
	
	if (in == 0 || in > nn)
	  gts_file_error (fp, "node index `%d' is out of range `[1,%d]'",
			  in, nn);
	else if (in == n)
	  gts_file_error (fp, "node index `%d' references itself", in);
	else if (in < n) {
	  gts_gedge_new (g->edge_class, GTS_GNODE (node), nodes[in - 1]);
	  ne--;
	  gts_file_next_token (fp);
	}
      }
    } while (fp->type != GTS_ERROR && fp->type != '\n');
  }
  g_free (nodes);

  if (fp->type != GTS_ERROR) {
    if (n != nn)
      gts_file_error (fp, "only `%d' nodes read out of `%d'",
		      n, nn);
    else if (ne > 0)
      gts_file_error (fp, "`%d' unallocated edges remaining",
		      ne);
  }

  if (fp->type == GTS_ERROR)
    return fp->line;
  return 0;
}

static void count_edges (GtsGEdge * e, guint * nedge)
{
  (*nedge)++;
}

static void write_node (GtsObject * node, gpointer * data)
{
  FILE * fp = data[0];
  guint * nnode = data[1];

  node->reserved = GUINT_TO_POINTER ((*nnode)++);
  if (node->klass->write)
    (* node->klass->write) (node, fp);
  fputc ('\n', fp);
}

static void write_edge (GtsGEdge * edge, FILE * fp)
{
  fprintf (fp, "%u %u", 
	   GPOINTER_TO_UINT (GTS_OBJECT (edge->n1)->reserved),
	   GPOINTER_TO_UINT (GTS_OBJECT (edge->n2)->reserved));
  if (GTS_OBJECT (edge)->klass->write)
    (* GTS_OBJECT (edge)->klass->write) (GTS_OBJECT (edge), fp);
  fputc ('\n', fp);
}

/**
 * gts_graph_write:
 * @g: a #GtsGraph.
 * @fp: a file pointer.
 *
 * Writes in the file @fp an ASCII representation of @g. The file
 * format is as follows. 
 *
 * All the lines beginning with #GTS_COMMENTS are ignored. The first line
 * contains two unsigned integers separated by spaces. The first
 * integer is the number of nodes, nn, the second is the number of
 * edges, ne.
 *
 * Follows nn lines containing node description.
 * Follows ne lines containing the two indices (starting
 * from one) of the nodes of each edge.
 *
 * The format described above is the least common denominator to all
 * GTS files.  Consistent with an object-oriented approach, the GTS
 * file format is extensible. Each of the lines of the file can be
 * extended with user-specific attributes accessible through the
 * read() and write() virtual methods of each of the objects written
 * (graph, nodes or edges). When read with different object classes,
 * these extra attributes are just ignored.  
 */
void gts_graph_write (GtsGraph * g, FILE * fp)
{
  guint nnode = 1, nedge = 0;
  gpointer data[2];

  g_return_if_fail (g != NULL);
  g_return_if_fail (fp != NULL);

  gts_graph_foreach_edge (g, (GtsFunc) count_edges, &nedge);
  fprintf (fp, "%u %u", gts_container_size (GTS_CONTAINER (g)), nedge);
  if (GTS_OBJECT (g)->klass->write)
    (* GTS_OBJECT (g)->klass->write) (GTS_OBJECT (g), fp);
  fputc ('\n', fp);
  data[0] = fp;
  data[1] = &nnode;
  gts_container_foreach (GTS_CONTAINER (g), (GtsFunc) write_node, data);
  gts_graph_foreach_edge (g, (GtsFunc) write_edge, fp);
  gts_container_foreach (GTS_CONTAINER (g), 
			 (GtsFunc) gts_object_reset_reserved, NULL);
}

/**
 * gts_graph_read:
 * @fp: a #GtsFile.
 *
 * Reads a graph from a file.
 *
 * Returns: the new #GtsGraph or %NULL if an error occured (in which
 * case the @error field of @fp is set).
 */
GtsGraph * gts_graph_read (GtsFile * fp)
{
  GtsGraph * g;
  GtsGNode ** nodes;
  guint nn, ne, n;

  g_return_val_if_fail (fp != NULL, NULL);

  if (fp->type != GTS_INT) {
    gts_file_error (fp, "expecting an integer (number of nodes)");
    return NULL;
  }
  nn = atoi (fp->token->str);
  gts_file_next_token (fp);

  if (fp->type != GTS_INT) {
    gts_file_error (fp, "expecting an integer (number of edges)");
    return NULL;
  }
  ne = atoi (fp->token->str);

  gts_file_next_token (fp);
  if (fp->type != '\n') {
    GtsObjectClass * klass;

    gts_graph_class ();
    gts_gnode_class ();
    gts_gedge_class ();

    if (fp->type != GTS_STRING) {
      gts_file_error (fp, "expecting a string (GtsGraphClass)");
      return NULL;
    }
    klass = gts_object_class_from_name (fp->token->str);
    if (klass == NULL) {
      gts_file_error (fp, "unknown class `%s'", fp->token->str);
      return NULL;
    }
    if (!gts_object_class_is_from_class (klass, gts_graph_class ())) {
      gts_file_error (fp, "class `%s' is not a GtsGraphClass", fp->token->str);
      return NULL;
    }
    g = GTS_GRAPH (gts_object_new (klass));
    g->graph_class = GTS_GRAPH_CLASS (klass);
    gts_file_next_token (fp);
    (* klass->read) ((GtsObject **) &g, fp);
    if (fp->type == GTS_ERROR) {
      gts_object_destroy (GTS_OBJECT (g));
      return NULL;
    }
  }
  else
    g = GTS_GRAPH (gts_object_new (GTS_OBJECT_CLASS (gts_graph_class ())));
  gts_file_first_token_after (fp, '\n');
  if (nn <= 0)
    return g;

  nodes = g_malloc ((nn + 1)*sizeof (GtsGNode *));

  n = 0;
  while (n < nn && fp->type != GTS_ERROR) {
    GtsObject * new_node = 
      gts_object_new (GTS_OBJECT_CLASS (g->node_class));

    gts_container_add (GTS_CONTAINER (g), GTS_CONTAINEE (new_node));
    if (GTS_OBJECT_CLASS (g->node_class)->read)
      (*GTS_OBJECT_CLASS (g->node_class)->read) (&new_node, fp);
    gts_file_first_token_after (fp, '\n');
    nodes[n++] = GTS_GNODE (new_node);
  }
  if (fp->type == GTS_ERROR)
    nn = n;

  n = 0;
  while (n < ne && fp->type != GTS_ERROR) {
    guint n1, n2;

    if (fp->type != GTS_INT)
      gts_file_error (fp, "expecting an integer (first node index)");
    else {
      n1 = atoi (fp->token->str);
      if (n1 == 0 || n1 > nn)
	gts_file_error (fp, "node index `%d' is out of range `[1,%d]'",
			n1, nn);
      else {
	gts_file_next_token (fp);
	if (fp->type != GTS_INT)
	  gts_file_error (fp, "expecting an integer (second node index)");
	else {
	  n2 = atoi (fp->token->str);
	  if (n2 == 0 || n2 > nn)
	    gts_file_error (fp, "node index `%d' is out of range `[1,%d]'",
			    n2, nn);
	  else {
	    GtsGEdge * new_edge =
	      gts_gedge_new (g->edge_class, nodes[n1 - 1], nodes [n2 - 1]);

	    gts_file_next_token (fp);
	    if (fp->type != '\n')
	      if (GTS_OBJECT_CLASS (g->edge_class)->read)
		(*GTS_OBJECT_CLASS (g->edge_class)->read)
		  ((GtsObject **) &new_edge, fp);
	    gts_file_first_token_after (fp, '\n');
	    n++;
	  }
	}
      }
    }
  }

  if (fp->type == GTS_ERROR) {
    gts_allow_floating_gnodes = TRUE;
    while (nn)
      gts_object_destroy (GTS_OBJECT (nodes[nn-- - 1]));
    gts_allow_floating_gnodes = FALSE;
  }
  g_free (nodes);

  if (fp->type == GTS_ERROR) {
    gts_object_destroy (GTS_OBJECT (g));
    return NULL;
  }
  return g;
}

static void write_dot_node (GtsGNode * node, gpointer * data)
{
  FILE * fp = data[0];
  guint * nnode = data[1];

  fprintf (fp, "  n%u", *nnode);
  if (GTS_GNODE_CLASS (GTS_OBJECT (node)->klass)->write) {
    fputs (" [", fp);
    (* GTS_GNODE_CLASS (GTS_OBJECT (node)->klass)->write) (node, fp);
    fputc (']', fp);
  }
  fputs (";\n", fp);
  GTS_OBJECT (node)->reserved = GUINT_TO_POINTER ((*nnode)++);  
}

static void write_dot_edge (GtsGEdge * edge, FILE * fp)
{
  fprintf (fp, "  n%u -> n%u", 
	   GPOINTER_TO_UINT (GTS_OBJECT (edge->n1)->reserved),
	   GPOINTER_TO_UINT (GTS_OBJECT (edge->n2)->reserved));
  if (GTS_GEDGE_CLASS (GTS_OBJECT (edge)->klass)->write) {
    fputs (" [", fp);
    (* GTS_GEDGE_CLASS (GTS_OBJECT (edge)->klass)->write) (edge, fp);
    fputc (']', fp);
  }
  fputs (";\n", fp);
}

/**
 * gts_graph_write_dot:
 * @g: a #GtsGraph.
 * @fp: a file pointer.
 *
 * Writes in the file @fp an ASCII representation of @g in the dot format of
 * AT&T Bell Labs.
 */
void gts_graph_write_dot (GtsGraph * g, FILE * fp)
{
  guint nnode = 1;
  gpointer data[2];

  g_return_if_fail (g != NULL);
  g_return_if_fail (fp != NULL);

  fprintf (fp, "digraph \"%p\" {\n", g);
  data[0] = fp;
  data[1] = &nnode;
  gts_container_foreach (GTS_CONTAINER (g), (GtsFunc) write_dot_node, data);
  gts_graph_foreach_edge (g, (GtsFunc) write_dot_edge, fp);
  fputs ("}\n", fp);

  gts_container_foreach (GTS_CONTAINER (g), 
			 (GtsFunc) gts_object_reset_reserved, NULL);
}

/* GtsWGraph */

static gfloat wgraph_weight (GtsGraph * g)
{
  return GTS_WGRAPH (g)->weight;
}

static void wgraph_add (GtsContainer * g, GtsContainee * n)
{
  GtsWGraph * wg = GTS_WGRAPH (g);
  gfloat w = gts_gnode_weight (GTS_GNODE (n));

  wg->weight += w;
  
  (* GTS_CONTAINER_CLASS (GTS_OBJECT_CLASS (gts_wgraph_class ())->parent_class)->add) (g, n);
}

static void wgraph_remove (GtsContainer * g, GtsContainee * n)
{
  GTS_WGRAPH (g)->weight -= gts_gnode_weight (GTS_GNODE (n));
  
  (* GTS_CONTAINER_CLASS (GTS_OBJECT_CLASS (gts_wgraph_class ())->parent_class)->remove) (g, n);
}

static void wgraph_class_init (GtsWGraphClass * klass)
{
  GTS_GRAPH_CLASS (klass)->weight = wgraph_weight;

  GTS_CONTAINER_CLASS (klass)->add = wgraph_add;
  GTS_CONTAINER_CLASS (klass)->remove = wgraph_remove;
}

static void wgraph_init (GtsWGraph * g)
{
  g->weight = 0.;
}

/**
 * gts_wgraph_class:
 *
 * Returns: the #GtsWGraphClass.
 */
GtsWGraphClass * gts_wgraph_class (void)
{
  static GtsWGraphClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo wgraph_info = {
      "GtsWGraph",
      sizeof (GtsWGraph),
      sizeof (GtsWGraphClass),
      (GtsObjectClassInitFunc) wgraph_class_init,
      (GtsObjectInitFunc) wgraph_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_graph_class ()),
				  &wgraph_info);
  }

  return klass;
}

static void weight_max (GtsGNode * n, gfloat * wmax)
{
  gfloat w = gts_gnode_weight (n);

  if (w > *wmax)
    *wmax = w;
}

/**
 * gts_wgraph_weight_max:
 * @wg: a #GtsWGraph.
 *
 * Returns: the maximum weight of any vertices belonging to @g.
 */
gfloat gts_wgraph_weight_max (GtsWGraph * wg)
{
  gfloat wmax = - G_MAXFLOAT;

  g_return_val_if_fail (wg != NULL, 0.);

  gts_container_foreach (GTS_CONTAINER (wg), (GtsFunc) weight_max, &wmax);

  return wmax;
}

/* Surface graph */

static void create_node (GtsFace * f, GtsGraph * graph)
{
  GtsFNode * fn = gts_fnode_new (gts_fnode_class (), f);

  gts_container_add (GTS_CONTAINER (graph), GTS_CONTAINEE (fn));
  GTS_OBJECT (f)->reserved = fn;
}

static void create_edge (GtsEdge * e, GtsSurface * s)
{
  GSList * i = e->triangles;
  
  while (i) {
    GtsFace * f = i->data;
    if (GTS_IS_FACE (f) && gts_face_has_parent_surface (f, s)) {
      GSList * j = i->next;
      while (j) {
	GtsFace * f1 = j->data;
	if (GTS_IS_FACE (f1) && gts_face_has_parent_surface (f1, s))
	  gts_pgedge_new (gts_pgedge_class (), 
			  GTS_OBJECT (f)->reserved,
			  GTS_OBJECT (f1)->reserved,
			  e);
	j = j->next;
      }
    }
    i = i->next;
  }
}

/**
 * gts_surface_graph_new:
 * @klass: a #GtsGraphClass.
 * @s: a #GtsSurface.
 *
 * Returns: a new #GtsGraph representing the connectivity of the faces
 * of @s. This graph uses #GtsFGNode as nodes which allows to store
 * the dependencies between nodes and faces of @s.  
 */
GtsGraph * gts_surface_graph_new (GtsGraphClass * klass,
				  GtsSurface * s)
{
  GtsGraph * graph;
  
  g_return_val_if_fail (klass != NULL, NULL);
  g_return_val_if_fail (s != NULL, NULL);

  graph = GTS_GRAPH (gts_object_new (GTS_OBJECT_CLASS (klass)));
  gts_surface_foreach_face (s, (GtsFunc) create_node, graph);
  gts_surface_foreach_edge (s, (GtsFunc) create_edge, s);
  gts_surface_foreach_face (s, (GtsFunc) gts_object_reset_reserved, NULL);

  return graph;
}

static void create_segment_edge (GtsSegment * s, GtsGraph * graph)
{
  GtsGNode * n1 = GTS_OBJECT (s->v1)->reserved, * n2;

  if (n1 == NULL) {
    n1 = GTS_GNODE (gts_pnode_new (gts_pnode_class (), s->v1));
    gts_container_add (GTS_CONTAINER (graph), GTS_CONTAINEE (n1));
    GTS_OBJECT (s->v1)->reserved = n1;
  }

  n2 = GTS_OBJECT (s->v2)->reserved;
  if (n2 == NULL) {
    n2 = GTS_GNODE (gts_pnode_new (gts_pnode_class (), s->v2));
    gts_container_add (GTS_CONTAINER (graph), GTS_CONTAINEE (n2));
    GTS_OBJECT (s->v2)->reserved = n2;
  }
  
  gts_pgedge_new (gts_pgedge_class (), n1, n2, s);
}

static void reset_reserved (GtsSegment * s)
{
  GTS_OBJECT (s->v1)->reserved = GTS_OBJECT (s->v2)->reserved = NULL;
}

/**
 * gts_segments_graph_new:
 * @klass: a #GtsGraphClass.
 * @segments: a list of #GtsSegment.
 *
 * Returns: a new #GtsGraph representing the connectivity of the segments
 * in @segments.
 */
GtsGraph * gts_segments_graph_new (GtsGraphClass * klass,
				   GSList * segments)
{
  GtsGraph * graph;
  
  g_return_val_if_fail (klass != NULL, NULL);

  graph = GTS_GRAPH (gts_object_new (GTS_OBJECT_CLASS (klass)));
  g_slist_foreach (segments, (GFunc) create_segment_edge, graph);
  g_slist_foreach (segments, (GFunc) reset_reserved, NULL);

  return graph;
}

static void add_to_surface (GtsGNode * n, GtsSurface * s)
{
  if (GTS_IS_FNODE (n))
    gts_surface_add_face (s, GTS_FNODE (n)->f);
}

/**
 * gts_surface_graph_surface:
 * @surface_graph: a #GtsGraph using #GtsFGNode as nodes.
 * @s: a #GtsSurface.
 *
 * Returns: a new #GtsSurface using the same classes as @s and
 * composed of the faces defined by @surface_graph.
 */
GtsSurface * gts_surface_graph_surface (GtsGraph * surface_graph,
					GtsSurface * s)
{
  GtsSurface * s1;

  g_return_val_if_fail (surface_graph != NULL, NULL);
  g_return_val_if_fail (s != NULL, NULL);
  
  s1 = gts_surface_new (GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass),
			s->face_class,
			s->edge_class,
			s->vertex_class);
  gts_container_foreach (GTS_CONTAINER (surface_graph), 
			 (GtsFunc) add_to_surface, s1);
  return s1;
}

