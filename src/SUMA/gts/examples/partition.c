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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#endif /* HAVE_GETOPT_H */
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "gts.h"

static GtsColor default_face_color = { 1., 1., 1.};

static GtsColor random_color (void)
{
  GtsColor c;

  c.r = rand ()/(gfloat) RAND_MAX;  
  c.g = rand ()/(gfloat) RAND_MAX;  
  c.b = rand ()/(gfloat) RAND_MAX;  

  return c;
}

static GtsColor face_color (GtsObject * o)
{
  return default_face_color;
}

static gint compare_line (GtsNGNode * n1, GtsNGNode * n2)
{
  if (n1->id < n2->id)
    return -1;
  return 1;
}

static void create_heap (GtsGNode * n, GtsHeap * heap)
{
  gts_heap_insert (heap, n);
}

int main (int argc, char * argv[])
{
  GtsSurface * s;
  GtsGraph * g;
  GtsFile * fp;
  guint np;
  guint nmin = 100;
  guint mmax = 50;
  guint ntry = 10;
  GSList * partition;
  int c = 0;
  gboolean verbose = FALSE;
  gfloat imbalance = 0.1;

  /* parse options using getopt */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'},
      {"try", required_argument, NULL, 't'},
      {"mmax", required_argument, NULL, 'm'},
      {"nmin", required_argument, NULL, 'n'},
      {"imbalance", required_argument, NULL, 'i'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hvt:m:n:i:", 
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hvt:m:n:i:"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'i': /* imbalance */
      imbalance = atof (optarg);
      break;
    case 't': /* try */
      ntry = atoi (optarg);
      break;
    case 'm': /* mmax */
      mmax = atoi (optarg);
      break;
    case 'n': /* nmin */
      nmin = atoi (optarg);
      break;
    case 'v': /* verbose */
      verbose = TRUE;
      break;
    case 'h': /* help */
      fprintf (stderr,
	       "Usage: partition [OPTION] N < FILE\n"
	       "Partition the graph defined by FILE into 2^N parts using recursive multilevel\n"
	       "bisection.\n"
	       "FILE can be either a GTS file or a GRAPH file (Jostle graph format).\n"
	       "For GTS input the output is a OOGL (Geomview) description of the partitions.\n"
	       "For GRAPH input the output is a graph partition (.ptn format) as defined in\n"
	       "\"The Graph Partitioning Archive\" (http://www.gre.ac.uk/~c.walshaw/partition/).\n"
	       "\n"
	       "  -t N  --try=N       number of tries for graph growing (default 10)\n"
	       "  -m N  --mmax=N      number of unsuccessful moves for Kernighan-Lin (default 50)\n"
	       "  -n N  --nmin=N      minimum number of nodes on coarsest graph (default 100)\n"
               "  -i I  --imbalance=I relative imbalance (default is 0.1)\n"
	       "  -v    --verbose     print statistics about the graph and partition\n"
	       "  -h    --help        display this help and exit\n"
	       "\n"
	       "Reports bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `partition --help' for more information.\n");
      return 1; /* failure */
    }
  }

  if (optind >= argc) { /* missing N */  
    fprintf (stderr, 
	     "partition: missing N\n"
	     "Try `partition --help' for more information.\n");
    return 1; /* failure */
  }
  np = atoi (argv[optind]);

  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());

  fp = gts_file_new (stdin);
  if (gts_surface_read (s, fp)) {
    GtsFile * fp1;

    gts_object_destroy (GTS_OBJECT (s));
    s = NULL;
    rewind (stdin);

    fp1 = gts_file_new (stdin);
    g = gts_graph_new (GTS_GRAPH_CLASS (gts_wgraph_class ()),
		       gts_gnode_class (), gts_gedge_class ());
    if (gts_graph_read_jostle (g, fp1)) {
      fputs ("partition: file on standard input is neither a valid GTS file nor a valid GRAPH file\n",
	     stderr);
      fprintf (stderr, "GTS stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
      fprintf (stderr, "GRAPH stdin:%d:%d: %s\n", 
	       fp1->line, fp1->pos, fp1->error);
      return 1; /* failure */
    }
  }
  else {
    g = gts_surface_graph_new (GTS_GRAPH_CLASS (gts_wgraph_class ()), s);
    if (verbose)
      gts_surface_print_stats (s, stderr);
  }

  if (verbose)
    gts_graph_print_stats (g, stderr);
  partition = gts_graph_recursive_bisection (GTS_WGRAPH (g), 
					     np, ntry, mmax, nmin, imbalance);
  if (verbose)
    gts_graph_partition_print_stats (partition, stderr);

  if (s) {
    GSList * i;

    printf ("LIST {\n");
    GTS_OBJECT_CLASS (gts_face_class ())->color = face_color;
    i = partition;
    while (i) {
      GtsSurface * s1 = gts_surface_graph_surface (i->data, s);
      
      gts_surface_write_oogl_boundary (s1, stdout);
      default_face_color = random_color ();
      gts_surface_write_oogl (s1, stdout);
      gts_object_destroy (GTS_OBJECT (s1));
      
      i = i->next;
    }
    printf ("}\n");
  }
  else {
    GSList * i = partition;
    guint np = 0;
    GtsHeap * heap;
    GtsGNode * n;

    while (i) {
      GTS_OBJECT (i->data)->reserved = GUINT_TO_POINTER (np++);
      i = i->next;
    }

    heap = gts_heap_new ((GCompareFunc) compare_line);
    gts_container_foreach (GTS_CONTAINER (g), (GtsFunc) create_heap, heap);

    while ((n = gts_heap_remove_top (heap))) {
      GSList * j = partition;
      GtsGraph * own = NULL;
      while (j && !own) {
	if (gts_containee_is_contained (GTS_CONTAINEE (n), j->data))
	  own = j->data;
	j = j->next;
      }
      g_assert (own);
      printf ("%d\n", GPOINTER_TO_UINT (GTS_OBJECT (own)->reserved));
    }

    gts_heap_destroy (heap);
  }

  return 0;
}

