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

/* stripe - Turns the input surface into triangle strips and outputs a
   Geomview representation of the result. */
int main (int argc, char * argv[])
{
  GtsSurface * s;
  GSList * strips = NULL, * i;
  gboolean verbose = FALSE;
  int c = 0;
  GtsFile * fp;

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
             "Usage: stripe [OPTION] < FILE\n"
	     "Turns the input surface into triangle strips and outputs a\n"
	     "Geomview representation of the result.\n"
	     "\n"
	     "  -v      --verbose  print statistics about the surface and strips\n"
	     "  -h      --help     display this help and exit\n"
	     "\n"
	     "Report bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `stripe --help' for more information.\n");
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
    fputs ("stripe: file on standard input is not a valid GTS file\n", 
	   stderr);
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1; /* failure */
  }
  gts_file_destroy (fp);

  if (verbose)
    gts_surface_print_stats (s, stderr);
  
  strips = gts_surface_strip (s);

  /* if verbose on print stats */
  if (verbose) {
    GtsRange l;

    gts_range_init (&l);
    i = strips;
    while (i) {
      gts_range_add_value (&l, g_slist_length (i->data));
      i = i->next;
    }
    gts_range_update (&l);
    fprintf (stderr, "# Strips: %d\n#   length : ", l.n);
    gts_range_print (&l, stderr);
    fputc ('\n', stderr);
  }

  puts ("LIST {\n");
  i = strips;
  while (i) {
    GList * j = i->data;
    GtsTriangle * oldt = NULL;
    GtsColor c;

    c.r = rand ()/(gdouble) RAND_MAX;
    c.g = rand ()/(gdouble) RAND_MAX;
    c.b = rand ()/(gdouble) RAND_MAX;
    while (j) {
      GtsTriangle * t = j->data;
      GtsPoint
        * p1 = GTS_POINT (GTS_SEGMENT (t->e1)->v1),
        * p2 = GTS_POINT (GTS_SEGMENT (t->e1)->v2),
        * p3 = GTS_POINT (gts_triangle_vertex (t));

      printf ("OFF 3 1 3\n%g %g %g\n%g %g %g\n%g %g %g\n3 0 1 2 %g %g %g\n",
              p1->x, p1->y, p1->z,
              p2->x, p2->y, p2->z,
              p3->x, p3->y, p3->z,
	      c.r, c.g, c.b);
      if (oldt) {
        GtsSegment * cs = GTS_SEGMENT (gts_triangles_common_edge (t, oldt));
        GtsPoint
	  * op1 = GTS_POINT (GTS_SEGMENT (oldt->e1)->v1),
	  * op2 = GTS_POINT (GTS_SEGMENT (oldt->e1)->v2),
	  * op3 = GTS_POINT (gts_triangle_vertex (oldt));

        printf ("VECT 1 3 0 3 0 %g %g %g %g %g %g %g %g %g\n",
                (op1->x + op2->x + op3->x)/3.,
                (op1->y + op2->y + op3->y)/3.,
                (op1->z + op2->z + op3->z)/3.,
                (GTS_POINT (cs->v1)->x + GTS_POINT (cs->v2)->x)/2.,
                (GTS_POINT (cs->v1)->y + GTS_POINT (cs->v2)->y)/2.,
                (GTS_POINT (cs->v1)->z + GTS_POINT (cs->v2)->z)/2.,
                (p1->x + p2->x + p3->x)/3.,
                (p1->y + p2->y + p3->y)/3.,
                (p1->z + p2->z + p3->z)/3.);
      }
      oldt = t;
      j = j->next;
    }
    i = i->next;
  }
  puts ("}\n");

  return 0; /* success */
}
