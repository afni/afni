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

/* sphere - generate a triangulated unit sphere by recursive subdivision.
   First approximation is an isocahedron; each level of refinement
   increases the number of triangles by a factor of 4. 
  */
int main (int argc, char * argv[])
{
  GtsSurface * s;
  gboolean verbose = FALSE;
  guint level=4;
  int c = 0;

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
       "Usage: sphere [OPTION] [level]\n"
       "Generate a triangulated unit sphere by recursive subdivision.\n"
       "First approximation is an isocahedron; each level of refinement\n"
       "increases the number of triangles by a factor of 4.\n"
       "level must be a positive integer setting the recursion level\n"
       "(geodesation order), default is 4.\n"
       "\n"
       "Documentation: http://mathworld.wolfram.com/GeodesicDome.html\n"
       "\n"
       "  -v    --verbose  print statistics about the surface\n"
       "  -h    --help     display this help and exit\n"
       "\n"
       "Reports bugs to %s\n",
	       GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `sphere --help' for more information.\n");
      return 1; /* failure */
    }
  }
  
  /* read level */
  if (optind < argc)
    level = atoi(argv[optind]);

  /* generate triangulated sphere */	
  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());
  gts_surface_generate_sphere (s, level);

  /* if verbose on print stats */
  if (verbose)
    gts_surface_print_stats (s, stderr);

  /* write generating surface to standard output */
  gts_surface_write (s, stdout);
  
  return 0; /* success */
}
