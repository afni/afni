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
#include <string.h>
#include "config.h"
#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#endif /* HAVE_GETOPT_H */
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "gts.h"

static void write_face (GtsTriangle * t)
{
  GtsVertex * v1, * v2, * v3;
  GtsVector n;

  gts_triangle_vertices (t, &v1, &v2, &v3);
  gts_triangle_normal (t, &n[0], &n[1], &n[2]);
  gts_vector_normalize (n);
  printf ("facet normal %g %g %g\nouter loop\n", n[0], n[1], n[2]);
  printf ("vertex %g %g %g\n", 
	  GTS_POINT (v1)->x, GTS_POINT (v1)->y, GTS_POINT (v1)->z);
  printf ("vertex %g %g %g\n", 
	  GTS_POINT (v2)->x, GTS_POINT (v2)->y, GTS_POINT (v2)->z);
  printf ("vertex %g %g %g\n", 
	  GTS_POINT (v3)->x, GTS_POINT (v3)->y, GTS_POINT (v3)->z);
  puts ("endloop\nendfacet");
}

int main (int argc, char * argv[])
{
  int c = 0;
  gboolean verbose = FALSE;
  gboolean revert  = FALSE;  
  GtsSurface * s;
  GtsFile * fp;

  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"revert", no_argument, NULL, 'r'},
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'}
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hvr",
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hvr"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'r': /* revert */
      revert = TRUE;
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: gts2stl [OPTION]... < input.gts > output.stl\n"
	     "Convert a GTS file to STL format.\n"
	     "\n"
	     "  -r,     --revert       revert face normals\n"
	     "  -v,     --verbose      display surface statistics\n"
	     "  -h,     --help         display this help and exit\n"
	     "\n"
	     "Report bugs to %s\n",
	     GTS_MAINTAINER);
      return 0;
      break;
    case 'v':
      verbose = TRUE;
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `gts2stl --help' for more information.\n");
      return 1;
    }
  }

  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());
  fp = gts_file_new (stdin);
  if (gts_surface_read (s, fp)) {
    fputs ("gts2stl: file on standard input is not a valid GTS file\n", 
	   stderr);
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1; /* failure */
  }

  if (revert)
    gts_surface_foreach_face (s, (GtsFunc) gts_triangle_revert, NULL);
  if (verbose)
    gts_surface_print_stats (s, stderr);

  puts ("solid");
  gts_surface_foreach_face (s, (GtsFunc) write_face, NULL);
  puts ("endsolid");

  return 0;
}
