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

/* volume - compute the volume of a given surface if it is a closed and
   orientable manifold */
int main (int argc, char * argv[])
{
  GtsSurface * s;
  gboolean verbose = FALSE;
  gboolean cm = FALSE;
  int c = 0;
  GtsFile * fp;

  /* parse options using getopt */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"cm", no_argument, NULL, 'c'},
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hvc", 
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hvc"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'c': /* cm */
      cm = TRUE;
      break;
    case 'v': /* verbose */
      verbose = TRUE;
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: volume [OPTION] < file.gts\n"
	     "Compute the volume of the domain bounded by the surface defined by file.srf.\n"
	     "Print the volume and exit successfully if the surface is a closed orientable\n"
	     "manifold. Exit unsuccessfully otherwise.\n"
	     "\n"
	     "  -v    --verbose  print statistics about the surface\n"
	     "  -c    --cm       output center of mass and volume\n"
	     "  -h    --help     display this help and exit\n"
	     "\n"
	     "Reports bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `volume --help' for more information.\n");
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
    fputs ("volume: the file on standard input is not a valid GTS file\n", 
	   stderr);
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1; /* failure */
  }

  /* if verbose on print stats */
  if (verbose)
    gts_surface_print_stats (s, stdout);

  /* test if surface is a closed and orientable manifold.
     we don't need to test if s is a manifold since both tests below
     implies that. */
  if (!gts_surface_is_closed (s) || !gts_surface_is_orientable (s))
    return 1; /* failure */
  
  /* print volume */
  printf ("%g\n", gts_surface_volume (s));

  if (cm) {
    GtsVector cm;

    /* print center of mass */
    gts_surface_center_of_mass (s, cm);
    printf ("(%g,%g,%g)\n", cm[0], cm[1], cm[2]);
  }

  return 0; /* success */
}
