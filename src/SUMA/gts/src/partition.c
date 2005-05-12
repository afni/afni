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

/* #define DEBUG */

/* Graph partition */

/**
 * gts_graph_partition_edges_cut:
 * @partition: a list of @GtsGraph representing a partition.
 *
 * Returns: the number of edges cut by the partition.
 */
guint gts_graph_partition_edges_cut (GSList * partition)
{
  guint cuts = 0;

  while (partition) {
    cuts += gts_graph_edges_cut (partition->data);
    partition = partition->next;
  }

  return cuts/2;
}

/**
 * gts_graph_partition_edges_cut_weight:
 * @partition: a list of @GtsGraph representing a partition.
 *
 * Returns: the total weight of the edges cut by the partition.
 */
gfloat gts_graph_partition_edges_cut_weight (GSList * partition)
{
  gfloat weight = 0.;

  while (partition) {
    weight += gts_graph_edges_cut_weight (partition->data);
    partition = partition->next;
  }

  return weight/2.;
}

/**
 * gts_graph_partition_print_stats:
 * @partition: a list of @GtsGraph representing a partition.
 * @fp: a file pointer.
 *
 * Writes to @fp a summary of the properties of @partition.
 */
void gts_graph_partition_print_stats (GSList * partition,
				      FILE * fp)
{
  GtsRange weight;
  GSList * i;

  g_return_if_fail (partition != NULL);
  g_return_if_fail (fp != NULL);

  gts_range_init (&weight);
  i = partition;
  while (i) {
    gts_range_add_value (&weight, gts_graph_weight (i->data));
    i = i->next;
  }
  gts_range_update (&weight);

  fprintf (fp, 
	   "# parts: %d\n"
	   "#   edge cuts: %5d edge cuts weight: %5g\n"
	   "#   weight: ",
	   g_slist_length (partition),
	   gts_graph_partition_edges_cut (partition),
	   gts_graph_partition_edges_cut_weight (partition));
  gts_range_print (&weight, fp);
  fputc ('\n', fp);
}

/**
 * gts_graph_partition_balance:
 * @partition: a list of @GtsGraph representing a partition.
 *
 * Returns: the difference between the maximum and the minimum weight
 * of the graphs in @partition.  
 */
gfloat gts_graph_partition_balance (GSList * partition)
{
  gfloat wmin = G_MAXFLOAT;
  gfloat wmax = - G_MAXFLOAT;

  g_return_val_if_fail (partition != NULL, 0.);

  while (partition) {
    gfloat weight = gts_graph_weight (partition->data);
    if (weight < wmin)
      wmin = weight;
    if (weight > wmax)
      wmax = weight;
    partition = partition->next;
  }
  return wmax - wmin;
}

/**
 * gts_graph_partition_clone:
 * @partition: a list of @GtsGraph representing a partition.
 *
 * Returns: a new partition clone of @partition (i.e. a list of new
 * graphs clones of the graphs in @partition).  
 */
GSList * gts_graph_partition_clone (GSList * partition)
{
  GSList * cparts = NULL;

  while (partition) {
    cparts = 
      g_slist_prepend (cparts, 
		       gts_object_clone (GTS_OBJECT (partition->data)));
    partition = partition->next;
  }
  return cparts;
}

/**
 * gts_graph_partition_destroy:
 * @partition: a list of @GtsGraph representing a partition.
 *
 * Destroys all the graphs in @partition and frees @partition.
 */
void gts_graph_partition_destroy (GSList * partition)
{
  GSList * i = partition;

  while (i) {
    gts_object_destroy (GTS_OBJECT (i->data));
    i = i->next;
  }
  g_slist_free (partition);
}

static void find_smallest_degree (GtsGNode * n, gpointer * data)
{
  GtsGNode ** nmin = data[0];
  GtsGraph * g = data[1];
  guint * min = data[2];
  guint degree = gts_gnode_degree (n, g);

  if (degree < *min) {
    *min = degree;
    *nmin = n;
  }
}

static gint graph_comp_weight (GtsGraph * g1, GtsGraph * g2)
{
  if (gts_graph_weight (g1) > gts_graph_weight (g2))
    return 1;
  return -1;
}

static void partition_update (GSList * list, GtsGraph * g)
{
  GSList * i;
  GtsGraph * g1;
  GtsHeap * size_heap;
  gboolean reinit = TRUE;

  /* initialize traversals */
  i = list;
  while (i) {
    GtsGNode * seed = GTS_OBJECT (i->data)->reserved;
    GTS_OBJECT (seed)->reserved = 
      gts_graph_traverse_new (g, seed, GTS_BREADTH_FIRST, reinit);
    reinit = FALSE;
    i = i->next;
  }
  
  size_heap = gts_heap_new ((GCompareFunc) graph_comp_weight);
  i = list;
  while (i) {
    gts_heap_insert (size_heap, i->data);
    i = i->next;
  }
  while ((g1 = gts_heap_remove_top (size_heap))) {
    GtsGraphTraverse * t = GTS_OBJECT (GTS_OBJECT (g1)->reserved)->reserved;
    GtsGNode * n = gts_graph_traverse_next (t);
    if (n) {
      gts_container_add (GTS_CONTAINER (g1), GTS_CONTAINEE (n));
      gts_heap_insert (size_heap, g1);
    }
  }
  gts_heap_destroy (size_heap);

  /* destroy traversals */
  i = list;
  while (i) {
    GtsGNode * seed = GTS_OBJECT (i->data)->reserved;
    gts_graph_traverse_destroy (GTS_OBJECT (seed)->reserved);
    GTS_OBJECT (seed)->reserved = NULL;
    i = i->next;
  }
}

static void better_seed (GtsGNode * n, gpointer * data)
{
  guint * sum = data[0];
  GtsGNode ** seed = data[1];
  GtsGraph * g = data[2];
  guint sum1 = gts_graph_distance_sum (g, n);
  
  if (sum1 < *sum) {
    *sum = sum1;
    *seed = n;
  }
}

static GtsGNode * graph_new_seed (GtsGraph * g, GtsGNode * seed)
{
  guint sum = gts_graph_distance_sum (g, seed);
  gpointer data[3];
  GtsGNode * new_seed = seed;

  data[0] = &sum;
  data[1] = &new_seed;
  data[2] = g;
  gts_gnode_foreach_neighbor (seed, g, (GtsFunc) better_seed, data);

  return new_seed;
}

/**
 * gts_graph_bubble_partition:
 * @g: a #GtsGraph.
 * @np: number of partitions.
 * @niter: the maximum number of iterations.
 * @step_info: a #GtsFunc or %NULL.
 * @data: user data to pass to @step_info.
 *
 * An implementation of the "bubble partitioning algorithm" of
 * Diekmann, Preis, Schlimbach and Walshaw (2000). The maximum number
 * of iteration on the positions of the graph growing seeds is
 * controlled by @niter.
 *
 * If not %NULL @step_info is called after each iteration on the seeds
 * positions passing the partition (a GSList) as argument.
 *
 * Returns: a list of @np new #GtsGraph representing the partition.  
 */
GSList * gts_graph_bubble_partition (GtsGraph * g, 
				     guint np, 
				     guint niter,
				     GtsFunc step_info,
				     gpointer data)
{
  GSList * list = NULL, * seeds = NULL;
  GtsGNode * seed = NULL;
  guint min = G_MAXINT/2 - 1;
  gpointer info[3];
  GtsGraph * g1;
  gboolean changed = TRUE;

  g_return_val_if_fail (g != NULL, NULL);
  g_return_val_if_fail (np > 0, NULL);

  info[0] = &seed;
  info[1] = g;
  info[2] = &min;
  gts_container_foreach (GTS_CONTAINER (g), 
			 (GtsFunc) find_smallest_degree,
			 info);
  if (seed == NULL)
    return NULL;

  g1 = GTS_GRAPH (gts_object_new (GTS_OBJECT (g)->klass));
  gts_container_add (GTS_CONTAINER (g1), GTS_CONTAINEE (seed));
  list = g_slist_prepend (list, g1);
  GTS_OBJECT (g1)->reserved = seed;
  seeds = g_slist_prepend (seeds, seed);

  while (--np && seed)
    if ((seed = gts_graph_farthest (g, seeds))) {
      g1 = GTS_GRAPH (gts_object_new (GTS_OBJECT (g)->klass));
      gts_container_add (GTS_CONTAINER (g1), GTS_CONTAINEE (seed));
      list = g_slist_prepend (list, g1);
      GTS_OBJECT (g1)->reserved = seed;
      seeds = g_slist_prepend (seeds, seed);
    }
  g_slist_free (seeds);
  
  partition_update (list, g);

  while (changed && niter--) {
    GSList * i;

    changed = FALSE;
    i = list;
    while (i) {
      GtsGraph * g1 = i->data;
      GtsGNode * seed = GTS_OBJECT (g1)->reserved;
      GtsGNode * new_seed = graph_new_seed (g1, seed);
      if (new_seed != seed) {
	changed = TRUE;
	GTS_OBJECT (g1)->reserved = new_seed;
      }
      i = i->next;
    }

    if (changed) {
      i = list;
      while (i) {
	GtsGraph * g1 = i->data;
	GtsGNode * seed = GTS_OBJECT (g1)->reserved;

	gts_object_destroy (GTS_OBJECT (g1));
	i->data = g1 = GTS_GRAPH (gts_object_new (GTS_OBJECT (g)->klass));
	gts_container_add (GTS_CONTAINER (g1), GTS_CONTAINEE (seed));
	GTS_OBJECT (g1)->reserved = seed;
	i = i->next;
      }
      partition_update (list, g);
      if (step_info)
	(* step_info) (list, data);
    }
  }
  g_slist_foreach (list, (GFunc) gts_object_reset_reserved, NULL);
  return list;
}

/* Graph bisection */

static gdouble node_cost (GtsGNode * n, gpointer * data)
{
  GtsGraph * g = data[0];
  GtsGraph * g1 = data[1];
  GSList * i = GTS_SLIST_CONTAINER (n)->items;
  gdouble cost = 0.;

  while (i) {
    GtsGEdge * e = i->data;
    GtsGNode * n1 = GTS_GNODE_NEIGHBOR (n, e);

    if (gts_containee_is_contained (GTS_CONTAINEE (n1), GTS_CONTAINER (g))) {
      if (gts_containee_is_contained (GTS_CONTAINEE (n1), GTS_CONTAINER (g1)))
	cost -= gts_gedge_weight (e);
      else 
	cost += gts_gedge_weight (e);
    }
    i = i->next;
  }

  return cost;
}

static void add_neighbor (GtsGNode * n, GtsEHeap * heap)
{
  if (GTS_OBJECT (n)->reserved == n)
    return;
  if (GTS_OBJECT (n)->reserved)
    gts_eheap_remove (heap, GTS_OBJECT (n)->reserved);
  GTS_OBJECT (n)->reserved = gts_eheap_insert (heap, n);
}

static void add_unused (GtsGNode * n, GtsGraph * g2)
{
  if (GTS_OBJECT (n)->reserved)
    GTS_OBJECT (n)->reserved = NULL;
  else
    gts_container_add (GTS_CONTAINER (g2), GTS_CONTAINEE (n));
}

static gdouble degree_cost (GtsGNode * n, GtsGraph * g)
{
  return gts_gnode_degree (n, g); 
}

static void add_seed (GtsGNode * n, GtsEHeap * heap)
{
  gts_eheap_insert (heap, n);
}

static void boundary_node1 (GtsGNode * n, GtsGraphBisection * bg)
{
  GSList * i = GTS_SLIST_CONTAINER (n)->items;

  while (i) {
    GtsGNode * n1 = GTS_GNODE_NEIGHBOR (n, i->data);
    if (gts_containee_is_contained (GTS_CONTAINEE (n1), 
				    GTS_CONTAINER (bg->g2))) {
      g_hash_table_insert (bg->bg1, n, n1);
      return;
    }
    i = i->next;
  }
}

static void boundary_node2 (GtsGNode * n, GtsGraphBisection * bg)
{
  GSList * i = GTS_SLIST_CONTAINER (n)->items;

  while (i) {
    GtsGNode * n1 = GTS_GNODE_NEIGHBOR (n, i->data);
    if (gts_containee_is_contained (GTS_CONTAINEE (n1), 
				    GTS_CONTAINER (bg->g1))) {
      g_hash_table_insert (bg->bg2, n, n1);
      return;
    }
    i = i->next;
  }
}

static void check_bg (GtsGNode * n, gpointer * data)
{
  GHashTable * bg = data[0];
  GtsGraph * g = data[1];
  gboolean * ok = data[2];
  guint * nb = data[3];
  guint nn = gts_gnode_degree (n, g);

  if (nn > 0)
    (*nb)++;
  if ((nn > 0 && !g_hash_table_lookup (bg, n)) ||
      (nn == 0 && g_hash_table_lookup (bg, n))) {
    g_warning ("nn: %d lookup: %p\n",
	       nn, g_hash_table_lookup (bg, n));
    *ok = FALSE;
  }
}

/**
 * gts_graph_bisection_check:
 * @bg: a #GtsGraphBisection.
 *
 * Checks that the boundary of @bg is correctly defined (used for
 * debugging purposes).
 *
 * Returns: %TRUE if @bg is ok, %FALSE otherwise.  
 */
gboolean gts_graph_bisection_check (GtsGraphBisection * bg)
{
  gboolean ok = TRUE;
  guint nb;
  gpointer data[4];

  g_return_val_if_fail (bg != NULL, FALSE);

  nb = 0;
  data[0] = bg->bg1;
  data[1] = bg->g2;
  data[2] = &ok;
  data[3] = &nb;
  gts_container_foreach (GTS_CONTAINER (bg->g1), (GtsFunc) check_bg, data);
  g_return_val_if_fail (g_hash_table_size (bg->bg1) == nb, FALSE);

  nb = 0;
  data[0] = bg->bg2;
  data[1] = bg->g1;
  gts_container_foreach (GTS_CONTAINER (bg->g2), (GtsFunc) check_bg, data);
  g_return_val_if_fail (g_hash_table_size (bg->bg2) == nb, FALSE);

  return ok;
}

/**
 * gts_graph_ggg_bisection:
 * @g: a #GtsGraph.
 * @ntry: the number of randomly selected initial seeds.
 *
 * An implementation of the "Greedy Graph Growing" algorithm of
 * Karypis and Kumar (1997).  
 *
 * @ntry randomly chosen seeds are used and the best partition is retained.
 *
 * Returns: a new #GtsGraphBisection of @g.
 */
GtsGraphBisection * gts_graph_ggg_bisection (GtsGraph * g, guint ntry)
{
  gfloat size, bestcost = G_MAXFLOAT, smin;
  GtsGraph * bestg1 = NULL, * bestg2 = NULL;
  gboolean balanced = FALSE;
  GtsEHeap * degree_heap;
  GtsGNode * seed;
  GtsGraphBisection * bg;

  g_return_val_if_fail (g != NULL, NULL);

  bg = g_malloc (sizeof (GtsGraphBisection));
  bg->g = g;

  size = gts_graph_weight (g)/2.;
  smin = 0.9*size;

  degree_heap = gts_eheap_new ((GtsKeyFunc) degree_cost, g);
  gts_eheap_freeze (degree_heap);
  gts_container_foreach (GTS_CONTAINER (g), (GtsFunc) add_seed, degree_heap);
  gts_eheap_thaw (degree_heap);

  while (ntry && ((seed = gts_eheap_remove_top (degree_heap, NULL)))) {
    GtsGraph * g1, * g2;
    GtsGNode * n;
    gdouble cost;
    gpointer data[2];
    GtsEHeap * heap;
  
    g1 = gts_graph_new (GTS_GRAPH_CLASS (GTS_OBJECT (g)->klass),
			g->node_class, g->edge_class);
    g2 = gts_graph_new (GTS_GRAPH_CLASS (GTS_OBJECT (g)->klass),
			g->node_class, g->edge_class);
    
    data[0] = g;
    data[1] = g1;
    heap = gts_eheap_new ((GtsKeyFunc) node_cost, data);

    gts_container_add (GTS_CONTAINER (g1), GTS_CONTAINEE (seed));
    GTS_OBJECT (seed)->reserved = seed;
    gts_gnode_foreach_neighbor (seed, g, (GtsFunc) add_neighbor, heap);

    while ((n = gts_eheap_remove_top (heap, &cost)))
      if (gts_graph_weight (g1) + gts_gnode_weight (n) <= size) {
	gts_container_add (GTS_CONTAINER (g1), GTS_CONTAINEE (n));
	GTS_OBJECT (n)->reserved = n;
	gts_gnode_foreach_neighbor (n, g, (GtsFunc) add_neighbor, heap);
      }
      else
	GTS_OBJECT (n)->reserved = NULL;
    gts_eheap_destroy (heap);
    
    gts_container_foreach (GTS_CONTAINER (g), (GtsFunc) add_unused, g2);

    cost = gts_graph_edges_cut_weight (g1);
    if (!bestg1 || 
	(!balanced && gts_graph_weight (g1) >= smin) ||
	(cost < bestcost && gts_graph_weight (g1) >= smin)) {
      if (bestg1)
	bestcost = cost;
      if (bestg1)
	gts_object_destroy (GTS_OBJECT (bestg1));
      if (bestg2)
	gts_object_destroy (GTS_OBJECT (bestg2));
      bestg1 = g1;
      bestg2 = g2;
      if (gts_graph_weight (g1) >= smin)
	balanced = TRUE;
    }
    else {
      gts_object_destroy (GTS_OBJECT (g1));
      gts_object_destroy (GTS_OBJECT (g2));
    }

    ntry--;
  }
  gts_eheap_destroy (degree_heap);

#ifdef DEBUG
  fprintf (stderr, "bestcost: %5g g1: %5g|%5d g2: %5g|%5d\n",
	   bestcost, 
	   gts_graph_weight (bestg1), 
	   gts_container_size (GTS_CONTAINER (bestg1)),
	   gts_graph_weight (bestg2), 
	   gts_container_size (GTS_CONTAINER (bestg2)));
#endif

  bg->g1 = bestg1;
  bg->g2 = bestg2;
  
  /* boundary nodes */
  bg->bg1 = g_hash_table_new (NULL, NULL);
  gts_container_foreach (GTS_CONTAINER (bg->g1), (GtsFunc) boundary_node1, bg);
  bg->bg2 = g_hash_table_new (NULL, NULL);
  gts_container_foreach (GTS_CONTAINER (bg->g2), (GtsFunc) boundary_node2, bg);

  return bg;
}

/**
 * gts_graph_bfgg_bisection:
 * @g: a #GtsGraph.
 * @ntry: the number of randomly selected initial seeds.
 *
 * An implementation of a "Breadth-First Graph Growing" algorithm.
 *
 * @ntry randomly chosen seeds are used and the best partition is retained.
 *
 * Returns: a new #GtsGraphBisection of @g.
 */
GtsGraphBisection * gts_graph_bfgg_bisection (GtsGraph * g, guint ntry)
{
  gfloat size, bestcost = G_MAXFLOAT, smin;
  GtsGraph * bestg1 = NULL, * bestg2 = NULL;
  GtsEHeap * degree_heap;
  GtsGNode * seed;
  GtsGraphBisection * bg;

  g_return_val_if_fail (g != NULL, NULL);

  bg = g_malloc (sizeof (GtsGraphBisection));
  bg->g = g;

  size = gts_graph_weight (g)/2.;
  smin = 0.9*size;

  degree_heap = gts_eheap_new ((GtsKeyFunc) degree_cost, g);
  gts_eheap_freeze (degree_heap);
  gts_container_foreach (GTS_CONTAINER (g), (GtsFunc) add_seed, degree_heap);
  gts_eheap_thaw (degree_heap);

  while (ntry && ((seed = gts_eheap_remove_top (degree_heap, NULL)))) {
    GtsGraph * g1, * g2;
    GtsGNode * n;
    gdouble cost;
    GtsGraphTraverse * t = gts_graph_traverse_new (g, seed, 
						   GTS_BREADTH_FIRST, TRUE);
    
    g1 = gts_graph_new (GTS_GRAPH_CLASS (GTS_OBJECT (g)->klass),
			g->node_class, g->edge_class);
    g2 = gts_graph_new (GTS_GRAPH_CLASS (GTS_OBJECT (g)->klass),
			g->node_class, g->edge_class);

    while ((n = gts_graph_traverse_next (t)))
      if (gts_graph_weight (g1) + gts_gnode_weight (n) <= size) {
	gts_container_add (GTS_CONTAINER (g1), GTS_CONTAINEE (n));
	GTS_OBJECT (n)->reserved = n;
      }
    gts_graph_traverse_destroy (t);
    
    gts_container_foreach (GTS_CONTAINER (g), (GtsFunc) add_unused, g2);

    cost = gts_graph_edges_cut_weight (g1);
    if (!bestg1 || (cost < bestcost && gts_graph_weight (g1) >= smin)) {
      if (bestg1)
	bestcost = cost;
      if (bestg1)
	gts_object_destroy (GTS_OBJECT (bestg1));
      if (bestg2)
	gts_object_destroy (GTS_OBJECT (bestg2));
      bestg1 = g1;
      bestg2 = g2;
    }
    else {
      gts_object_destroy (GTS_OBJECT (g1));
      gts_object_destroy (GTS_OBJECT (g2));
    }

    ntry--;
  }
  gts_eheap_destroy (degree_heap);

#ifdef DEBUG
  fprintf (stderr, "bestcost: %5g g1: %5g|%5d g2: %5g|%5d\n",
	   bestcost, 
	   gts_graph_weight (bestg1), 
	   gts_container_size (GTS_CONTAINER (bestg1)),
	   gts_graph_weight (bestg2), 
	   gts_container_size (GTS_CONTAINER (bestg2)));
#endif

  bg->g1 = bestg1;
  bg->g2 = bestg2;
  
  /* boundary nodes */
  bg->bg1 = g_hash_table_new (NULL, NULL);
  gts_container_foreach (GTS_CONTAINER (bg->g1), (GtsFunc) boundary_node1, bg);
  bg->bg2 = g_hash_table_new (NULL, NULL);
  gts_container_foreach (GTS_CONTAINER (bg->g2), (GtsFunc) boundary_node2, bg);

  return bg;
}

static gdouble node_move_cost1 (GtsGNode * n, GtsGraphBisection * bg)
{
  return gts_gnode_move_cost (n, bg->g1, bg->g2);
}

static gdouble node_move_cost2 (GtsGNode * n, GtsGraphBisection * bg)
{
  return gts_gnode_move_cost (n, bg->g2, bg->g1);
}

static void build_heap (GtsGNode * n, GtsEHeap * heap)
{
  GTS_OBJECT (n)->reserved = gts_eheap_insert (heap, n);
}

/**
 * gts_graph_bisection_kl_refine:
 * @bg: a #GtsGraphBisection.
 * @mmax: the maximum number of unsuccessful successive moves.
 *
 * An implementation of the simplified Kernighan-Lin algorithm for
 * graph bisection refinement as described in Karypis and Kumar
 * (1997).
 *
 * The algorithm stops if @mmax consecutive modes do not lead to a
 * decrease in the number of edges cut. This last @mmax moves are
 * undone.
 *
 * Returns: the decrease in the weight of the edges cut by the bisection.  
 */
gdouble gts_graph_bisection_kl_refine (GtsGraphBisection * bg,
				       guint mmax)
{
  GtsEHeap * h1, * h2;
  GtsGNode * n;
  guint nm = 0, i;
  GtsGNode ** moves;
  gdouble bestcost = 0., totalcost = 0., best_balance;

  g_return_val_if_fail (bg != NULL, 0.);
  g_return_val_if_fail (mmax > 0, 0.);

  h1 = gts_eheap_new ((GtsKeyFunc) node_move_cost1, bg);
  gts_eheap_freeze (h1);
  gts_container_foreach (GTS_CONTAINER (bg->g1), (GtsFunc) build_heap, h1);
  gts_eheap_thaw (h1);

  h2 = gts_eheap_new ((GtsKeyFunc) node_move_cost2, bg);
  gts_eheap_freeze (h2);
  gts_container_foreach (GTS_CONTAINER (bg->g2), (GtsFunc) build_heap, h2);
  gts_eheap_thaw (h2);

  moves = g_malloc (sizeof (GtsGNode *)*mmax);
  best_balance = fabs (gts_graph_weight (bg->g1) - gts_graph_weight (bg->g2));

  do {
    GtsGraph * g1, * g2;
    gdouble cost;

    if (gts_graph_weight (bg->g1) > gts_graph_weight (bg->g2)) {
      n = gts_eheap_remove_top (h1, &cost);
      g1 = bg->g1;
      g2 = bg->g2;
    }
    else {
      n = gts_eheap_remove_top (h2, &cost);
      g1 = bg->g2;
      g2 = bg->g1;
    }
    if (n) {
      GSList * i;

      GTS_OBJECT (n)->reserved = NULL;
      gts_container_add (GTS_CONTAINER (g2), GTS_CONTAINEE (n));
      gts_container_remove (GTS_CONTAINER (g1), GTS_CONTAINEE (n));

      totalcost += cost;
      if (totalcost < bestcost) {
	bestcost = totalcost;
	nm = 0;
      }
      else if (totalcost == bestcost) {
	gdouble balance = fabs (gts_graph_weight (g1) - gts_graph_weight (g2));

	if (balance < best_balance) {
	  best_balance = balance;
	  nm = 0;
	}
      }	       
      else
	moves[nm++] = n;

      i = GTS_SLIST_CONTAINER (n)->items;
      while (i) {
	GtsGNode * n1 = GTS_GNODE_NEIGHBOR (n, i->data);
	if (GTS_OBJECT (n1)->reserved && 
	    gts_containee_is_contained (GTS_CONTAINEE (n1), 
					GTS_CONTAINER (bg->g))) {
	  GtsEHeap * h = 
	    gts_containee_is_contained (GTS_CONTAINEE (n1), 
					GTS_CONTAINER (bg->g1)) ? h1 : h2;
	  gts_eheap_remove (h, GTS_OBJECT (n1)->reserved);
	  GTS_OBJECT (n1)->reserved = gts_eheap_insert (h, n1);
	}
	i = i->next;
      }
    }
  } while (n && nm < mmax);

  gts_eheap_foreach (h1, (GFunc) gts_object_reset_reserved, NULL);
  gts_eheap_foreach (h2, (GFunc) gts_object_reset_reserved, NULL);
  gts_eheap_destroy (h1);
  gts_eheap_destroy (h2);

  /* undo last nm moves */
  for (i = 0; i < nm; i++) {
    GtsGNode * n = moves[i];
    GtsGraph * g1 = 
      gts_containee_is_contained (GTS_CONTAINEE (n),
				  GTS_CONTAINER (bg->g1)) ? bg->g1 : bg->g2;
    GtsGraph * g2 = g1 == bg->g1 ? bg->g2 : bg->g1;
    
    gts_container_add (GTS_CONTAINER (g2), GTS_CONTAINEE (n));
    gts_container_remove (GTS_CONTAINER (g1), GTS_CONTAINEE (n));
  }
  g_free (moves);

  return bestcost;
}

static void build_bheap (GtsGNode * n, GtsGNode * n1, GtsEHeap * heap)
{
  GTS_OBJECT (n)->reserved = gts_eheap_insert (heap, n);
}

static void update_neighbors (GtsGNode * n, GtsGraphBisection * bg,
			      GtsEHeap * h1, GtsEHeap * h2)
{
  GSList * i;

  i = GTS_SLIST_CONTAINER (n)->items;
  while (i) {
    GtsGNode * n1 = GTS_GNODE_NEIGHBOR (n, i->data);
    if (gts_containee_is_contained (GTS_CONTAINEE (n1), 
				    GTS_CONTAINER (bg->g))) {
      GtsEHeap * h;
      GtsGraph * g1, * g2;
      GHashTable * bg1;

      if (gts_containee_is_contained (GTS_CONTAINEE (n1),
				      GTS_CONTAINER (bg->g1))) {
	h = h1;
	g1 = bg->g1;
	g2 = bg->g2;
	bg1 = bg->bg1;
      }
      else {
	h = h2;
	g1 = bg->g2;
	g2 = bg->g1;
	bg1 = bg->bg2;
      }
      g_hash_table_remove (bg1, n1);
      if (h && GTS_OBJECT (n1)->reserved && GTS_OBJECT (n1)->reserved != n1) {
	gts_eheap_remove (h, GTS_OBJECT (n1)->reserved);
	GTS_OBJECT (n1)->reserved = NULL;
      }
      if (gts_gnode_degree (n1, g2)) {
	g_hash_table_insert (bg1, n1, n1);
	if (h && GTS_OBJECT (n1)->reserved != n1)
	  GTS_OBJECT (n1)->reserved = gts_eheap_insert (h, n1);
      }
    }
    i = i->next;
  }  
}

/**
 * gts_graph_bisection_bkl_refine:
 * @bg: a #GtsGraphBisection.
 * @mmax: the maximum number of unsuccessful successive moves.
 * @imbalance: the maximum relative imbalance allowed between the
 * weights of both halves of the partition.
 *
 * An implementation of the simplified boundary Kernighan-Lin
 * algorithm for graph bisection refinement as described in Karypis
 * and Kumar (1997).
 *
 * The algorithm stops if @mmax consecutive modes do not lead to a
 * decrease in the number of edges cut. This last @mmax moves are
 * undone.
 *
 * Returns: the decrease in the weight of the edges cut by the bisection.  
 */
gdouble gts_graph_bisection_bkl_refine (GtsGraphBisection * bg,
					guint mmax,
					gfloat imbalance)
{
  GtsEHeap * h1, * h2;
  GtsGNode * n;
  guint nm = 0, i;
  GtsGNode ** moves;
  gdouble bestcost = 0., totalcost = 0., best_balance;
  gboolean balanced = FALSE;

  g_return_val_if_fail (bg != NULL, 0.);
  g_return_val_if_fail (mmax > 0, 0.);
  g_return_val_if_fail (imbalance >= 0. && imbalance <= 1., 0.);

  h1 = gts_eheap_new ((GtsKeyFunc) node_move_cost1, bg);
  gts_eheap_freeze (h1);
  g_hash_table_foreach (bg->bg1, (GHFunc) build_bheap, h1);
  gts_eheap_thaw (h1);

  h2 = gts_eheap_new ((GtsKeyFunc) node_move_cost2, bg);
  gts_eheap_freeze (h2);
  g_hash_table_foreach (bg->bg2, (GHFunc) build_bheap, h2);
  gts_eheap_thaw (h2);

  moves = g_malloc (sizeof (GtsGNode *)*mmax);
  imbalance *= gts_graph_weight (bg->g);
  best_balance = fabs (gts_graph_weight (bg->g1) - gts_graph_weight (bg->g2));
  if (best_balance <= imbalance)
    balanced = TRUE;

  do {
    GtsGraph * g1, * g2;
    GHashTable * bg1, * bg2;
    gdouble cost;

    if (gts_graph_weight (bg->g1) > gts_graph_weight (bg->g2)) {
      n = gts_eheap_remove_top (h1, &cost);
      g1 = bg->g1;
      g2 = bg->g2;
      bg1 = bg->bg1;
      bg2 = bg->bg2;
    }
    else {
      n = gts_eheap_remove_top (h2, &cost);
      g1 = bg->g2;
      g2 = bg->g1;
      bg1 = bg->bg2;
      bg2 = bg->bg1;
    }
    if (n) {
      gdouble balance;
	
      GTS_OBJECT (n)->reserved = n;
      gts_container_add (GTS_CONTAINER (g2), GTS_CONTAINEE (n));
      gts_container_remove (GTS_CONTAINER (g1), GTS_CONTAINEE (n));
      g_hash_table_remove (bg1, n);
      if (gts_gnode_degree (n, g1))
	g_hash_table_insert (bg2, n, n);

      update_neighbors (n, bg, h1, h2);

      totalcost += cost;
      balance = fabs (gts_graph_weight (g1) - gts_graph_weight (g2));
      
      if (!balanced && balance <= imbalance) {
	bestcost = totalcost;
	best_balance = balance;
	balanced = TRUE;
	nm = 0;
      }
      else if (totalcost < bestcost && 
	       (balance < best_balance || balance <= imbalance)) {
	bestcost = totalcost;
	best_balance = balance;
	nm = 0;
      }
      else if (totalcost == bestcost && balance < best_balance) {
	best_balance = balance;
	nm = 0;
      }
      else
	moves[nm++] = n;
    }
  } while (n && nm < mmax);

  gts_container_foreach (GTS_CONTAINER (bg->g), 
			 (GtsFunc) gts_object_reset_reserved, NULL);
  gts_eheap_destroy (h1);
  gts_eheap_destroy (h2);

  /* undo last nm moves */
  for (i = 0; i < nm; i++) {
    GtsGNode * n = moves[i];
    GtsGraph * g1, * g2;
    GHashTable * bg1, * bg2;

    if (gts_containee_is_contained (GTS_CONTAINEE (n),
				    GTS_CONTAINER (bg->g1))) {
      g1 = bg->g1;
      g2 = bg->g2;
      bg1 = bg->bg1;
      bg2 = bg->bg2;
    }
    else {
      g1 = bg->g2;
      g2 = bg->g1;
      bg1 = bg->bg2;
      bg2 = bg->bg1;
    }
    
    gts_container_add (GTS_CONTAINER (g2), GTS_CONTAINEE (n));
    gts_container_remove (GTS_CONTAINER (g1), GTS_CONTAINEE (n));
    g_hash_table_remove (bg1, n);
    if (gts_gnode_degree (n, g1))
      g_hash_table_insert (bg2, n, n);

    update_neighbors (n, bg, NULL, NULL);
  }
  g_free (moves);

  return bestcost;
}

/* Multilevel partitioning */

static void bisection_children (GtsGNodeSplit * ns, GtsGraphBisection * bg)
{
  GtsGraph * g, * g1;
  GHashTable * bbg;
  GtsGNode * n1 = GTS_GNODE_SPLIT_N1 (ns);
  GtsGNode * n2 = GTS_GNODE_SPLIT_N2 (ns);

  if (gts_containee_is_contained (GTS_CONTAINEE (ns->n),
				  GTS_CONTAINER (bg->g1))) {
    g = bg->g1;
    g1 = bg->g2;
    bbg = bg->bg1;
  }
  else {
    g = bg->g2;
    g1 = bg->g1;
    bbg = bg->bg2;
  }

  gts_allow_floating_gnodes = TRUE;
  gts_container_remove (GTS_CONTAINER (g), GTS_CONTAINEE (ns->n));
  gts_allow_floating_gnodes = FALSE;
  gts_container_add (GTS_CONTAINER (g), GTS_CONTAINEE (n1));
  gts_container_add (GTS_CONTAINER (g), GTS_CONTAINEE (n2));

  if (g_hash_table_lookup (bbg, ns->n)) {
    g_hash_table_remove (bbg, ns->n);
    if (gts_gnode_degree (n1, g1) > 0)
      g_hash_table_insert (bbg, n1, n1);
    if (gts_gnode_degree (n2, g1) > 0)
      g_hash_table_insert (bbg, n2, n2);
  }
}

/**
 * gts_graph_bisection_new:
 * @wg: a #GtsWGraph.
 * @ntry: the number of tries for the graph growing algorithm.
 * @mmax: the number of unsucessful moves for the refinement algorithm.
 * @nmin: the minimum number of nodes of the coarsest graph.
 * @imbalance: the maximum relative imbalance allowed between the
 * weights of both halves of the partition.
 *
 * An implementation of a multilevel bisection algorithm as presented
 * in Karypis and Kumar (1997). A multilevel hierarchy of graphs is
 * created using the #GtsPGraph object. The bisection of the coarsest
 * graph is created using the gts_graph_ggg_bisection() function. The
 * graph is then uncoarsened using gts_pgraph_down() and at each level
 * the bisection is refined using gts_graph_bisection_bkl_refine().
 *
 * Returns: a new #GtsGraphBisection of @wg.  
 */
GtsGraphBisection * gts_graph_bisection_new (GtsWGraph * wg,
					     guint ntry,
					     guint mmax,
					     guint nmin,
					     gfloat imbalance)
{
  GtsGraph * g;
  GtsPGraph * pg;
  GtsGraphBisection * bg;
  gdouble cost;

  g_return_val_if_fail (wg != NULL, NULL);

  g = GTS_GRAPH (wg);
  pg = gts_pgraph_new (gts_pgraph_class (), g, 
		       gts_gnode_split_class (),
		       gts_wgnode_class (),
		       gts_wgedge_class (),
		       nmin);

  bg = gts_graph_ggg_bisection (g, ntry);
#ifdef DEBUG
  fprintf (stderr, "before size: %5d weight: %5g cuts: %5d cweight: %5g\n",
	   gts_container_size (GTS_CONTAINER (bg->g1)),
	   gts_graph_weight (bg->g1),
	   gts_graph_edges_cut (bg->g1),
	   gts_graph_edges_cut_weight (bg->g1));
  g_assert (gts_graph_bisection_check (bg));
#endif
  while ((cost = gts_graph_bisection_bkl_refine (bg, mmax, imbalance))) {
#ifdef DEBUG
    fprintf (stderr, "  cost: %g\n", cost);
    g_assert (gts_graph_bisection_check (bg));
#endif
  }
#ifdef DEBUG
  fprintf (stderr, "after  size: %5d weight: %5g cuts: %5d cweight: %5g\n",
	   gts_container_size (GTS_CONTAINER (bg->g1)),
	   gts_graph_weight (bg->g1),
	   gts_graph_edges_cut (bg->g1),
	   gts_graph_edges_cut_weight (bg->g1));
#endif
  while (gts_pgraph_down (pg, (GtsFunc) bisection_children, bg)) {
#ifdef DEBUG
    fprintf (stderr, "before size: %5d weight: %5g cuts: %5d cweight: %5g\n",
	     gts_container_size (GTS_CONTAINER (bg->g1)),
	     gts_graph_weight (bg->g1),
	     gts_graph_edges_cut (bg->g1),
	     gts_graph_edges_cut_weight (bg->g1));	   
#endif
    while ((cost = gts_graph_bisection_bkl_refine (bg, mmax, imbalance))) {
#ifdef DEBUG
      fprintf (stderr, "  cost: %g\n", cost);
      g_assert (gts_graph_bisection_check (bg));
#endif
    }
#ifdef DEBUG
    fprintf (stderr, "after  size: %5d weight: %5g cuts: %5d cweight: %5g\n",
	     gts_container_size (GTS_CONTAINER (bg->g1)),
	     gts_graph_weight (bg->g1),
	     gts_graph_edges_cut (bg->g1),
	     gts_graph_edges_cut_weight (bg->g1));
#endif
  }
  gts_object_destroy (GTS_OBJECT (pg));

  return bg;
}

/**
 * gts_graph_bisection_destroy:
 * @bg: a #GtsGraphBisection.
 * @destroy_graphs: controls graph destruction.
 *
 * Frees all the memory allocated for @bg. If @destroy_graphs is %TRUE
 * the graphs created by @bg are destroyed.  
 */
void gts_graph_bisection_destroy (GtsGraphBisection * bg,
				  gboolean destroy_graphs)
{
  g_return_if_fail (bg != NULL);

  g_hash_table_destroy (bg->bg1);
  g_hash_table_destroy (bg->bg2);

  if (destroy_graphs) {
    gts_object_destroy (GTS_OBJECT (bg->g1));
    gts_object_destroy (GTS_OBJECT (bg->g2));
  }

  g_free (bg);
}

static void recursive_bisection (GtsWGraph * wg,
				 guint np,
				 guint ntry,
				 guint mmax,
				 guint nmin,
				 gfloat imbalance,
				 GSList ** list)
{
  if (np == 0)
    *list = g_slist_prepend (*list, wg);
  else {
    GtsGraphBisection * bg = 
      gts_graph_bisection_new (wg, ntry, mmax, nmin, imbalance);
    GtsGraph * g1 = bg->g1;
    GtsGraph * g2 = bg->g2;

    gts_object_destroy (GTS_OBJECT (wg));
    gts_graph_bisection_destroy (bg, FALSE);
    recursive_bisection (GTS_WGRAPH (g1), np - 1, ntry, mmax, nmin, imbalance,
			 list);
    recursive_bisection (GTS_WGRAPH (g2), np - 1, ntry, mmax, nmin, imbalance,
			 list);
  }
}

/**
 * gts_graph_recursive_bisection:
 * @wg: a #GtsWGraph.
 * @n: the number of bisection levels.
 * @ntry: the number of tries for the graph growing algorithm.
 * @mmax: the number of unsucessful moves for the refinement algorithm.
 * @nmin: the minimum number of nodes of the coarsest graph.
 * @imbalance: the maximum relative imbalance allowed between the
 * weights of both halves of the partition.
 *
 * Calls gts_graph_bisection_new() recursively in order to obtain a
 * 2^@n partition of @wg.
 *
 * Returns: a list of 2^@n new #GtsGraph representing the partition.
 */
GSList * gts_graph_recursive_bisection (GtsWGraph * wg,
					guint n,
					guint ntry,
					guint mmax,
					guint nmin,
					gfloat imbalance)
{
  GtsGraphBisection * bg;
  GtsGraph * g1, * g2;
  GSList * list = NULL;

  g_return_val_if_fail (wg != NULL, NULL);
  g_return_val_if_fail (n > 0, NULL);
  
  bg = gts_graph_bisection_new (wg, ntry, mmax, nmin, imbalance);
  g1 = bg->g1;
  g2 = bg->g2;
  gts_graph_bisection_destroy (bg, FALSE);
  recursive_bisection (GTS_WGRAPH (g1), n - 1, ntry, mmax, nmin, imbalance, 
		       &list);
  recursive_bisection (GTS_WGRAPH (g2), n - 1, ntry, mmax, nmin, imbalance,
		       &list);

  return list;
}
