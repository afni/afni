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
#include "config.h"
#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#endif /* HAVE_GETOPT_H */
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "gts.h"

static void write_edge (GtsSegment * s, FILE * fp)
{
  fprintf (fp, "VECT 1 2 0 2 0 %g %g %g %g %g %g\n",
	   GTS_POINT (s->v1)->x,
	   GTS_POINT (s->v1)->y,
	   GTS_POINT (s->v1)->z,
	   GTS_POINT (s->v2)->x,
	   GTS_POINT (s->v2)->y,
	   GTS_POINT (s->v2)->z);
}

/* set - compute set operations between surfaces */
int main (int argc, char * argv[])
{
  GtsSurface * s1, * s2, * s3;
  GtsSurfaceInter * si;
  GNode * tree1, * tree2;
  FILE * fptr;
  GtsFile * fp;
  int c = 0;
  gboolean verbose = TRUE;
  gboolean inter = FALSE;
  gboolean check_self_intersection = FALSE;
  gchar * operation, * file1, * file2;
  gboolean closed = TRUE, is_open1, is_open2;

  /* parse options using getopt */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"inter", no_argument, NULL, 'i'},
      {"self", no_argument, NULL, 's'},
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hvis", 
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hvis"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 's': /* self */
      check_self_intersection = TRUE;
      break;
    case 'i': /* inter */
      inter = TRUE;
      break;
    case 'v': /* verbose */
      verbose = FALSE;
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: set [OPTION] OPERATION FILE1 FILE2\n"
             "Compute set operations between surfaces, where OPERATION is either.\n"
             "union, inter, diff.\n"
	     "\n"
             "  -i      --inter    output an OOGL (Geomview) representation of the curve\n"
             "                     intersection of the surfaces\n"
	     "  -s      --self     checks that the surfaces are not self-intersecting\n"
             "                     if one of them is, the set of self-intersecting faces\n"
	     "                     is written (as a GtsSurface) on standard output\n"
	     "  -v      --verbose  do not print statistics about the surface\n"
	     "  -h      --help     display this help and exit\n"
	     "\n"
	     "Reports bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `set --help' for more information.\n");
      return 1; /* failure */
    }
  }

  if (optind >= argc) { /* missing OPERATION */
    fprintf (stderr, 
	     "set: missing OPERATION\n"
	     "Try `set --help' for more information.\n");
    return 1; /* failure */
  }
  operation = argv[optind++];

  if (optind >= argc) { /* missing FILE1 */
    fprintf (stderr, 
	     "set: missing FILE1\n"
	     "Try `set --help' for more information.\n");
    return 1; /* failure */
  }
  file1 = argv[optind++];

  if (optind >= argc) { /* missing FILE2 */
    fprintf (stderr, 
	     "set: missing FILE2\n"
	     "Try `set --help' for more information.\n");
    return 1; /* failure */
  }
  file2 = argv[optind++];

  /* open first file */
  if ((fptr = fopen (file1, "rt")) == NULL) {
    fprintf (stderr, "set: can not open file `%s'\n", file1);
    return 1;
  }
  /* reads in first surface file */
  s1 = GTS_SURFACE (gts_object_new (GTS_OBJECT_CLASS (gts_surface_class ())));
  fp = gts_file_new (fptr);
  if (gts_surface_read (s1, fp)) {
    fprintf (stderr, "set: `%s' is not a valid GTS surface file\n", 
	     file1);
    fprintf (stderr, "%s:%d:%d: %s\n", file1, fp->line, fp->pos, fp->error);
    return 1;
  }
  gts_file_destroy (fp);
  fclose (fptr);

  /* open second file */
  if ((fptr = fopen (file2, "rt")) == NULL) {
    fprintf (stderr, "set: can not open file `%s'\n", file2);
    return 1;
  }
  /* reads in second surface file */
  s2 = GTS_SURFACE (gts_object_new (GTS_OBJECT_CLASS (gts_surface_class ())));
  fp = gts_file_new (fptr);
  if (gts_surface_read (s2, fp)) {
    fprintf (stderr, "set: `%s' is not a valid GTS surface file\n", 
	     file2);
    fprintf (stderr, "%s:%d:%d: %s\n", file2, fp->line, fp->pos, fp->error);
    return 1;
  }
  gts_file_destroy (fp);
  fclose (fptr);

  /* display summary information about both surfaces */
  if (verbose) {
    gts_surface_print_stats (s1, stderr);
    gts_surface_print_stats (s2, stderr);
  }

  /* check that the surfaces are orientable manifolds */
  if (!gts_surface_is_orientable (s1)) {
    fprintf (stderr, "set: surface `%s' is not an orientable manifold\n",
	     file1);
    return 1;
  }
  if (!gts_surface_is_orientable (s2)) {
    fprintf (stderr, "set: surface `%s' is not an orientable manifold\n",
	     file2);
    return 1;
  }

  /* check that the surfaces are not self-intersecting */
  if (check_self_intersection) {
    GtsSurface * self_intersects;

    self_intersects = gts_surface_is_self_intersecting (s1);
    if (self_intersects != NULL) {
      fprintf (stderr, "set: surface `%s' is self-intersecting\n", file1);
      if (verbose)
	gts_surface_print_stats (self_intersects, stderr);
      gts_surface_write (self_intersects, stdout);
      gts_object_destroy (GTS_OBJECT (self_intersects));
      return 1;
    }
    self_intersects = gts_surface_is_self_intersecting (s2);
    if (self_intersects != NULL) {
      fprintf (stderr, "set: surface `%s' is self-intersecting\n", file2);
      if (verbose)
	gts_surface_print_stats (self_intersects, stderr);
      gts_surface_write (self_intersects, stdout);
      gts_object_destroy (GTS_OBJECT (self_intersects));
      return 1;
    }
  }

  /* build bounding box tree for first surface */
  tree1 = gts_bb_tree_surface (s1);
  is_open1 = gts_surface_volume (s1) < 0. ? TRUE : FALSE;

  /* build bounding box tree for second surface */
  tree2 = gts_bb_tree_surface (s2);
  is_open2 = gts_surface_volume (s2) < 0. ? TRUE : FALSE;

  si = gts_surface_inter_new (gts_surface_inter_class (),
			      s1, s2, tree1, tree2, is_open1, is_open2);
  g_assert (gts_surface_inter_check (si, &closed));
  if (!closed) {
    fprintf (stderr,
	     "set: the intersection of `%s' and `%s' is not a closed curve\n",
	     file1, file2);
    return 1;
  }

  s3 = gts_surface_new (gts_surface_class (),
			gts_face_class (),
			gts_edge_class (),
			gts_vertex_class ());  
  if (!strcmp (operation, "union")) {
    gts_surface_inter_boolean (si, s3, GTS_1_OUT_2);
    gts_surface_inter_boolean (si, s3, GTS_2_OUT_1);
  }
  else if (!strcmp (operation, "inter")) {
    gts_surface_inter_boolean (si, s3, GTS_1_IN_2);
    gts_surface_inter_boolean (si, s3, GTS_2_IN_1);
  }
  else if (!strcmp (operation, "diff")) {
    gts_surface_inter_boolean (si, s3, GTS_1_OUT_2);
    gts_surface_inter_boolean (si, s3, GTS_2_IN_1);
    gts_surface_foreach_face (si->s2, (GtsFunc) gts_triangle_revert, NULL);
    gts_surface_foreach_face (s2, (GtsFunc) gts_triangle_revert, NULL);
  }
  else {
    fprintf (stderr, 
	     "set: operation `%s' unknown\n"
	     "Try `set --help' for more information.\n", 
	     operation);
    return 1;
  }
  
  /* check that the resulting surface is not self-intersecting */
  if (check_self_intersection) {
    GtsSurface * self_intersects;

    self_intersects = gts_surface_is_self_intersecting (s3);
    if (self_intersects != NULL) {
      fprintf (stderr, "set: the resulting surface is self-intersecting\n");
      if (verbose)
	gts_surface_print_stats (self_intersects, stderr);
      gts_surface_write (self_intersects, stdout);
      gts_object_destroy (GTS_OBJECT (self_intersects));
      return 1;
    }
  }
  /* display summary information about the resulting surface */
  if (verbose)
    gts_surface_print_stats (s3, stderr);
  /* write resulting surface to standard output */
  if (inter) {
    printf ("LIST {\n");
    g_slist_foreach (si->edges, (GFunc) write_edge, stdout);
    printf ("}\n");
  }
  else {
    GTS_POINT_CLASS (gts_vertex_class ())->binary = TRUE;
    gts_surface_write (s3, stdout);
  }

  /* destroy surfaces */
  gts_object_destroy (GTS_OBJECT (s1));
  gts_object_destroy (GTS_OBJECT (s2));
  gts_object_destroy (GTS_OBJECT (s3));
  gts_object_destroy (GTS_OBJECT (si));

  /* destroy bounding box trees (including bounding boxes) */
  gts_bb_tree_destroy (tree1, TRUE);
  gts_bb_tree_destroy (tree2, TRUE);  

  return 0;
}
