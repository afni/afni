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
  GtsSurface * s;
  gboolean verbose = FALSE;
  int c = 0;

  /* parse options using getopt */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'},
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
             "Usage: merge [OPTION] file1.gts file2.gts ...\n"
	     "Merges files and outputs the resulting GTS surface.\n"
	     "\n"
	     "  -v        --verbose       print statistics about the surface\n"
	     "  -h        --help          display this help and exit\n"
	     "\n"
	     "Reports bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `merge --help' for more information.\n");
      return 1; /* failure */
    }
  }

  s = GTS_SURFACE (gts_object_new (GTS_OBJECT_CLASS (gts_surface_class ())));

  while (optind < argc) {
    FILE * f = fopen (argv[optind], "r");
    GtsFile * fp;
    GtsSurface * s1;

    if (f == NULL) {
      fprintf (stderr, "merge: can not open file `%s'\n", argv[optind]);
      return 1;
    }
    fp = gts_file_new (f);
    s1 = GTS_SURFACE (gts_object_new (GTS_OBJECT_CLASS (gts_surface_class ())));
    if (gts_surface_read (s1, fp)) {
      fprintf (stderr, "merge: `%s' is not a valid GTS surface file\n", 
	       argv[optind]);
      fprintf (stderr, "%s:%d:%d: %s\n", argv[optind], fp->line, fp->pos, fp->error);
      return 1;
    }
    gts_surface_merge (s, s1);
    gts_object_destroy (GTS_OBJECT (s1));
    gts_file_destroy (fp);
    fclose (f);
    optind++;
  }

  if (verbose)
    gts_surface_print_stats (s, stderr);

  gts_surface_write (s, stdout);

  return 0;
}
