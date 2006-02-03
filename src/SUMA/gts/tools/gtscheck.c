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
#include "config.h"
#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#endif /* HAVE_GETOPT_H */
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "gts.h"

int main (int argc, char * argv[])
{
  GtsSurface * s, * self_intersects;
  gboolean verbose = FALSE;
  int c = 0;
  GtsFile * fp;

  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());

  /* parse options using getopt */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hv", 
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hv"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'v': /* verbose */
      verbose = TRUE;
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: gtscheck [OPTION] < FILE\n"
	     "Checks that a surface defines a closed, orientable\n"
	     "non self-intersecting manifold.\n"
	     "\n"
             "If the surface is self-intersecting, the set of intersected faces\n"
	     "is written on standard output as a GTS surface file.\n"
	     "\n"
	     "  -v      --verbose  print statistics about the surface\n"
	     "  -h      --help     display this help and exit\n"
	     "\n"
	     "The return status reflects the error encountered:\n"
	     "  0: none\n"
             "  1: the input file is not a valid GTS file\n"
             "  2: the surface is not an orientable manifold\n"
             "  3: the surface is an orientable manifold but is self-intersecting\n"
	     "\n"
	     "Reports bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `gtscheck --help' for more information.\n");
      return 1; /* failure */
    }
  }

  /* read surface in */
  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());
  fp = gts_file_new (stdin);
  if (gts_surface_read (s, fp)) {
    fputs ("gtscheck: file on standard input is not a valid GTS file\n", 
	   stderr);
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1; /* failure */
  }

  /* if verbose on print stats */
  if (verbose) 
    gts_surface_print_stats (s, stderr);
 
  if (!gts_surface_is_orientable (s)) {
    if (verbose)
      fprintf (stderr, "gtscheck: the surface on standard input is not an orientable manifold\n");
    return 2; /* failure */
  }

  self_intersects = gts_surface_is_self_intersecting (s);
  if (self_intersects != NULL) {
    if (verbose) {
      fprintf (stderr, "gtscheck: the surface on standard input is self-intersecting\n");
      gts_surface_print_stats (self_intersects, stderr);
    }
    gts_surface_write (self_intersects, stdout);
    return 3; /* failure */
  }

  return 0; /* success */
}
