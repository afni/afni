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

/* GtsGNodeSplit */

static void gnode_split_destroy (GtsObject * object)
{
  GtsGNodeSplit * ns = GTS_GNODE_SPLIT (object);

  if (gts_container_size (GTS_CONTAINER (ns->n)) == 0) {
    g_assert (GTS_SLIST_CONTAINEE (ns->n)->containers == NULL);
    gts_object_destroy (GTS_OBJECT (ns->n));
  }
  else {
    GtsGNode * n1 = GTS_GNODE_SPLIT_N1 (ns);
    GtsGNode * n2 = GTS_GNODE_SPLIT_N2 (ns);

    g_warning ("Memory deallocation for GtsGNodeSplit not fully implemented yet: memory leak!");
  }

  (* GTS_OBJECT_CLASS (gts_gnode_split_class ())->parent_class->destroy) 
    (object);
}

static void gnode_split_class_init (GtsGNodeSplitClass * klass)
{
  GTS_OBJECT_CLASS (klass)->destroy = gnode_split_destroy;
}

static void gnode_split_init (GtsGNodeSplit * ns)
{
  ns->n = NULL;
  ns->n1 = ns->n2 = NULL;
}

/**
 * gts_gnode_split_class:
 *
 * Returns: the #GtsGNodeSplitClass.
 */
GtsGNodeSplitClass * gts_gnode_split_class (void)
{
  static GtsGNodeSplitClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo gnode_split_info = {
      "GtsGNodeSplit",
      sizeof (GtsGNodeSplit),
      sizeof (GtsGNodeSplitClass),
      (GtsObjectClassInitFunc) gnode_split_class_init,
      (GtsObjectInitFunc) gnode_split_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), &gnode_split_info);
  }

  return klass;
}

/**
 * gts_gnode_split_new:
 * @klass: a #GtsGNodeSplitClass.
 * @n: a #GtsGNode.
 * @n1: a #GtsGNodeSplit or #GtsGNode.
 * @n2: a #GtsGNodeSplit or #GtsGNode.
 *
 * Creates a new #GtsGNodeSplit which would collapse @n1 and @n2 into
 * @n. The collapse itself is not performed.
 *
 * Returns: the new #GtsGNodeSplit.
 */
GtsGNodeSplit * gts_gnode_split_new (GtsGNodeSplitClass * klass,
				     GtsGNode * n, 
				     GtsObject * n1,
				     GtsObject * n2)
{
  GtsGNodeSplit * ns;

  g_return_val_if_fail (klass != NULL, NULL);
  g_return_val_if_fail (n != NULL, NULL);
  g_return_val_if_fail (GTS_IS_GNODE_SPLIT (n1) || GTS_IS_GNODE (n1), NULL);
  g_return_val_if_fail (GTS_IS_GNODE_SPLIT (n2) || GTS_IS_GNODE (n2), NULL);

  ns = GTS_GNODE_SPLIT (gts_object_new (GTS_OBJECT_CLASS (klass)));
  ns->n = n;
  ns->n1 = n1;
  ns->n2 = n2;

  return ns;
}

static void connect_edge (GtsGEdge * e, gpointer * data)
{
  GtsGNode * n = data[0];
  GtsGNode * n1 = data[1];
  GtsGNode * n2 = data[2];

  if (GTS_OBJECT (e)->reserved || /* edge is disconnected */
      gts_gedge_connects (e, n1, n2))
    return;
  if (e->n1 == n1 || e->n1 == n2)
    e->n1 = n;
  else if (e->n2 == n1 || e->n2 == n2)
    e->n2 = n;
  else
    g_assert_not_reached ();
  gts_container_add (GTS_CONTAINER (n), GTS_CONTAINEE (e));
}

/**
 * gts_gnode_split_collapse:
 * @ns: a #GtsGNodeSplit.
 * @g: a #GtsGraph.
 * @klass: a #GtsWGEdgeClass.
 *
 * Collapses the node split @ns. Any new edge created during the
 * process will be of class @klass.  
 */
void gts_gnode_split_collapse (GtsGNodeSplit * ns,
			       GtsGraph * g,
			       GtsWGEdgeClass * klass)
{
  GtsGNode * n1, * n2;
  GSList * i;
  gpointer data[3];

  g_return_if_fail (ns != NULL);
  g_return_if_fail (g != NULL);
  g_return_if_fail (gts_container_size (GTS_CONTAINER (ns->n)) == 0);

  n1 = GTS_GNODE_SPLIT_N1 (ns);
  n2 = GTS_GNODE_SPLIT_N2 (ns);

  /* look for triangles */
  i = GTS_SLIST_CONTAINER (n1)->items;
  while (i) {
    GtsGEdge * e13 = i->data;
    GtsGNode * n3 = GTS_GNODE_NEIGHBOR (n1, e13);
    if (n3 != n2) {
      GSList * j = GTS_SLIST_CONTAINER (n3)->items;
      while (j) {
	GtsGEdge * e32 = j->data;
	GSList * next = j->next;
	GtsGNode * n4 = GTS_GNODE_NEIGHBOR (n3, e32);
	if (n4 == n2) { /* found triangle n1 (e13) n3 (e32) n2 */
	  gts_wgedge_new (klass, ns->n, n3,
			  gts_gedge_weight (e13) + gts_gedge_weight (e32));
	  GTS_OBJECT (e13)->reserved = n3;
	  GTS_OBJECT (e32)->reserved = n3;
	  GTS_SLIST_CONTAINER (n3)->items = 
	    g_slist_remove (GTS_SLIST_CONTAINER (n3)->items, e32);
	}
	j = next;
      }
      if (GTS_OBJECT (e13)->reserved == n3)
	GTS_SLIST_CONTAINER (n3)->items = 
	  g_slist_remove (GTS_SLIST_CONTAINER (n3)->items, e13);
    }
    i = i->next;
  }

  /* connect edges to new node */
  data[0] = ns->n;
  data[1] = n1;
  data[2] = n2;
  gts_container_foreach (GTS_CONTAINER (n1), (GtsFunc) connect_edge, data);
  gts_container_foreach (GTS_CONTAINER (n2), (GtsFunc) connect_edge, data);

  gts_allow_floating_gnodes = TRUE;
  gts_container_remove (GTS_CONTAINER (g), GTS_CONTAINEE (n1));
  gts_container_remove (GTS_CONTAINER (g), GTS_CONTAINEE (n2));
  gts_allow_floating_gnodes = FALSE;
  gts_container_add (GTS_CONTAINER (g), GTS_CONTAINEE (ns->n));
}

static void restore_edge (GtsGEdge * e, gpointer * data)
{
  GtsGNode * n = data[0];
  GtsGNode * n1 = data[1];
  GtsGNode * n2 = data[2];
  GtsGNode * n3 = GTS_OBJECT (e)->reserved;

  if (n3) { /* e is a disconnected edge */
    GTS_OBJECT (e)->reserved = NULL;
    gts_container_add (GTS_CONTAINER (n3), GTS_CONTAINEE (e));
    return;
  }

  if (gts_gedge_connects (e, n1, n2))
    return;

  if (e->n1 == n)
    e->n1 = n1;
  else if (e->n2 == n)
    e->n2 = n1;
  else
    g_assert_not_reached ();
  GTS_SLIST_CONTAINER (n)->items = 
    g_slist_remove (GTS_SLIST_CONTAINER (n)->items, e);
}

/**
 * gts_gnode_split_expand:
 * @ns: a #GtsGNodeSplit.
 * @g: a #GtsGraph.
 *
 * Expands the node split ns adding the new nodes to @g.
 */
void gts_gnode_split_expand (GtsGNodeSplit * ns,
			     GtsGraph * g)
{
  GtsGNode * n1, * n2;
  gpointer data[3];
  GSList * i;

  g_return_if_fail (ns != NULL);
  g_return_if_fail (g != NULL);
  g_return_if_fail (gts_containee_is_contained (GTS_CONTAINEE (ns->n), 
						GTS_CONTAINER (g)));

  n1 = GTS_GNODE_SPLIT_N1 (ns);
  n2 = GTS_GNODE_SPLIT_N2 (ns);

  data[0] = ns->n;
  data[1] = n1;
  data[2] = n2;
  gts_container_foreach (GTS_CONTAINER (n1), (GtsFunc) restore_edge, data);
  data[1] = n2;
  data[2] = n1;
  gts_container_foreach (GTS_CONTAINER (n2), (GtsFunc) restore_edge, data);

  i = GTS_SLIST_CONTAINER (ns->n)->items;
  while (i) {
    GSList * next = i->next;
    gts_container_remove (GTS_CONTAINER (ns->n), GTS_CONTAINEE (i->data));
    i = next;
  }
  g_assert (gts_container_size (GTS_CONTAINER (ns->n)) == 0);
  
  gts_allow_floating_gnodes = TRUE;
  gts_container_remove (GTS_CONTAINER (g), GTS_CONTAINEE (ns->n));
  gts_allow_floating_gnodes = FALSE;

  gts_container_add (GTS_CONTAINER (g), GTS_CONTAINEE (n1));
  gts_container_add (GTS_CONTAINER (g), GTS_CONTAINEE (n2));
}

/* GtsPGraph */

static void pgraph_destroy (GtsObject * object)
{
  GtsPGraph * pg = GTS_PGRAPH (object);
  guint i;

  for (i = 0; i < pg->split->len; i++)
    gts_object_destroy (GTS_OBJECT (g_ptr_array_index (pg->split, i)));
  g_ptr_array_free (pg->split, TRUE);
  g_array_free (pg->levels, TRUE);

  (* GTS_OBJECT_CLASS (gts_pgraph_class ())->parent_class->destroy) (object);
}

static void pgraph_class_init (GtsPGraphClass * klass)
{
  GTS_OBJECT_CLASS (klass)->destroy = pgraph_destroy;
}

static void pgraph_init (GtsPGraph * pg)
{
  pg->g = NULL;
  pg->split = g_ptr_array_new ();
  pg->levels = g_array_new (FALSE, FALSE, sizeof (guint));
  pg->level = 0;
  pg->split_class = gts_gnode_split_class ();
  pg->edge_class = gts_wgedge_class ();
  pg->pos = pg->min = 0;
}

/**
 * gts_pgraph_class:
 *
 * Returns: the #GtsPGraphClass.
 */
GtsPGraphClass * gts_pgraph_class (void)
{
  static GtsPGraphClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo pgraph_info = {
      "GtsPGraph",
      sizeof (GtsPGraph),
      sizeof (GtsPGraphClass),
      (GtsObjectClassInitFunc) pgraph_class_init,
      (GtsObjectInitFunc) pgraph_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), &pgraph_info);
  }

  return klass;
}

static void match_neighbor (GtsGNode * n, gpointer * data)
{
  if (!GTS_OBJECT (n)->reserved) {
    GtsGraph * g = data[0];
    GSList ** list = data[1];
    GSList * i = GTS_SLIST_CONTAINER (n)->items;
    gfloat wmax = - G_MAXFLOAT;
    GtsGEdge * emax = NULL;
    
    while (i) {
      GtsGNode * n1 = GTS_GNODE_NEIGHBOR (n, i->data);
      if (!GTS_OBJECT (n1)->reserved &&
	  gts_gedge_weight (i->data) > wmax &&
	  gts_containee_is_contained (GTS_CONTAINEE (n1), GTS_CONTAINER (g))) {
	emax = i->data;
	wmax = gts_gedge_weight (emax);
      }
      i = i->next;
    }
    if (emax) {
      GtsGNode * n1 = GTS_GNODE_NEIGHBOR (n, emax);

      GTS_OBJECT (n1)->reserved = n;
      GTS_OBJECT (n)->reserved = n1;
      *list = g_slist_prepend (*list, emax);
    }
  }
}

static GSList * maximal_matching (GtsGraph * g)
{
  GSList * list = NULL;
  gpointer data[2];

  data[0] = g;
  data[1] = &list;
  gts_container_foreach (GTS_CONTAINER (g), (GtsFunc) match_neighbor, data);
  gts_container_foreach (GTS_CONTAINER (g), 
			 (GtsFunc) gts_object_reset_reserved,
			 NULL);

  return list;
}

/**
 * gts_pgraph_new:
 * @klass: a #GtsPGraphClass.
 * @g: a #GtsGraph.
 * @split_class: a #GtsGNodeSplitClass.
 * @node_class: a #GtsWGNodeClass.
 * @edge_class: a #GtsWGEdgeClass.
 * @min: the minimum number of nodes.
 *
 * Creates a new multilevel approximation of graph @g. At each level a
 * maximal matching is created using the Heavy Edge Matching (HEM)
 * technique of Karypis and Kumar (1997). The newly created nodes are
 * of type @node_class and their weight is set to the sum of the
 * weights of their children. The newly created edges are of type
 * @edge_class and their weight is set to the sum of the weight of the
 * collapsed edges. The last level is reached when the maximal
 * matching obtained would lead to a graph with less than @min nodes.
 *
 * Returns: the new #GtsPGraph containing the multilevel
 * representation of @g.  
 */
GtsPGraph * gts_pgraph_new (GtsPGraphClass * klass,
			    GtsGraph * g,
			    GtsGNodeSplitClass * split_class,
			    GtsWGNodeClass * node_class,
			    GtsWGEdgeClass * edge_class,
			    guint min)
{
  GtsPGraph * pg;
  GSList * matching;

  g_return_val_if_fail (klass != NULL, NULL);
  g_return_val_if_fail (g != NULL, NULL);
  g_return_val_if_fail (split_class != NULL, NULL);
  g_return_val_if_fail (node_class != NULL, NULL);
  g_return_val_if_fail (edge_class != NULL, NULL);

  pg = GTS_PGRAPH (gts_object_new (GTS_OBJECT_CLASS (klass)));
  pg->g = g;
  pg->split_class = split_class;
  pg->edge_class = edge_class;

  while (gts_container_size (GTS_CONTAINER (g)) > min &&
	 (matching = maximal_matching (g))) {
    GSList * i = matching;
    guint size = gts_container_size (GTS_CONTAINER (g));

    g_array_append_val (pg->levels, size);

    while (i && gts_container_size (GTS_CONTAINER (g)) > min) {
      GtsGEdge * e = i->data;
      GtsGNode * n = GTS_GNODE (gts_wgnode_new (node_class,
						gts_gnode_weight (e->n1) +
						gts_gnode_weight (e->n2)));
      GtsGNodeSplit * ns = gts_gnode_split_new (split_class, n,
						GTS_OBJECT (e->n1),
						GTS_OBJECT (e->n2));
      gts_gnode_split_collapse (ns, g, edge_class);
      g_ptr_array_add (pg->split, ns);
      i = i->next;
    }
    g_slist_free (matching);
  }

  pg->pos = pg->split->len;
  pg->min = gts_container_size (GTS_CONTAINER (g));
  pg->level = pg->levels->len;
  
  return pg;
}

/**
 * gts_pgraph_add_node:
 * @pg: a #GtsPGraph.
 *
 * Adds one node to the multilevel graph @pg by expanding the next
 * available #GtsGNodeSplit.
 *
 * Returns: the expanded #GtsGNodeSplit or #NULL if all the
 * #GtsGNodeSplit have already been expanded.  
 */
GtsGNodeSplit * gts_pgraph_add_node (GtsPGraph * pg)
{ 
  GtsGNodeSplit * ns;

  g_return_val_if_fail (pg != NULL, NULL);

  if (pg->pos == 0)
    return NULL;

  ns = g_ptr_array_index (pg->split, --pg->pos);
  gts_gnode_split_expand (ns, pg->g);

  return ns;
}

/**
 * gts_pgraph_remove_node:
 * @pg: a #GtsPGraph.
 *
 * Removes one node from the multilevel graph @pg by collapsing the
 * first available #GtsGNodeSplit.
 *
 * Returns: the collapsed #GtsGNodeSplit or %NULL if all the
 * #GtsGNodeSplit have already been collapsed.  
 */
GtsGNodeSplit * gts_pgraph_remove_node (GtsPGraph * pg)
{
  GtsGNodeSplit * ns;

  g_return_val_if_fail (pg != NULL, NULL);

  if (pg->pos == pg->split->len)
    return NULL;

  ns = g_ptr_array_index (pg->split, pg->pos++);
  gts_gnode_split_collapse (ns, pg->g, pg->edge_class);

  return ns;
}

/**
 * gts_pgraph_max_node_number:
 * @pg: a #GtsPGraph.
 *
 * Returns: the maximum number of nodes of @pg i.e. the number of
 * nodes if all the #GtsGNodeSplit were expanded.  
 */
guint gts_pgraph_max_node_number (GtsPGraph * pg)
{
  g_return_val_if_fail (pg != NULL, 0);

  return pg->min + pg->split->len;
}

/**
 * gts_pgraph_min_node_number:
 * @pg: a #GtsPGraph.
 *
 * Returns: the minimum number of nodes of @pg i.e. the number of
 * nodes if all the #GtsGNodeSplit were collapsed.  
 */
guint gts_pgraph_min_node_number (GtsPGraph * pg)
{
  g_return_val_if_fail (pg != NULL, 0);

  return pg->min;
}

/**
 * gts_pgraph_set_node_number:
 * @pg: a #GtsPGraph.
 * @n: a number of nodes.
 *
 * Performs the required number of collapses or expansions to set the
 * number of nodes of @pg to @n.
 */
void gts_pgraph_set_node_number (GtsPGraph * pg, guint n)
{
  g_return_if_fail (pg != NULL);

  n = pg->min + pg->split->len - n;
  while (pg->pos > n && gts_pgraph_add_node (pg))
    ;
  while (pg->pos < n && gts_pgraph_remove_node (pg))
    ;
}

/**
 * gts_pgraph_get_node_number:
 * @pg: a #GtsPGraph.
 *
 * Returns: the current number of nodes of @pg.
 */
guint gts_pgraph_get_node_number (GtsPGraph * pg)
{
  g_return_val_if_fail (pg != NULL, 0);
  
  return pg->min + pg->split->len - pg->pos;
}

/**
 * gts_pgraph_down:
 * @pg: a #GtsPGraph.
 * @func: a #GtsFunc or %NULL.
 * @data: user data to pass to @func.
 *
 * Performs the required number of expansions to go from the current
 * level to the level immediately below.
 *
 * If @func is not %NULL, it is called after each #GtsGNodeSplit has
 * been expanded.  
 *
 * Returns: %FALSE if it is not possible to go down one level, %TRUE
 * otherwise.  
 */
gboolean gts_pgraph_down (GtsPGraph * pg,
			  GtsFunc func,
			  gpointer data)
{
  guint size;

  g_return_val_if_fail (pg != NULL, FALSE);

  if (pg->level == 0)
    return FALSE;

  size = g_array_index (pg->levels, guint, --(pg->level));
  while (gts_container_size (GTS_CONTAINER (pg->g)) < size) {
    GtsGNodeSplit * ns = gts_pgraph_add_node (pg);

    g_assert (ns);
    if (func)
      (* func) (ns, data);
  }
  return TRUE;
}

