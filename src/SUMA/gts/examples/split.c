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

/* split - splits a surface into connected and manifold components */
int main (int argc, char * argv[])
{
  GtsSurface * s = NULL;
  int c = 0;
  GtsFile * fp;
  gboolean verbose = FALSE;
  gchar fname[] = "component";
  guint ncomp = 0;
  GSList * i;

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
             "Usage: split [OPTION] [FNAME] < FILE\n"
	     "Splits the GTS surface into connected and manifold components.\n"
	     "FNAME is the base name of the components created (default is `component').\n"
	     "\n"
	     "  -v    --verbose  print statistics about the surface\n"
	     "  -h    --help     display this help and exit\n"
	     "\n"
	     "Reports bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `split --help' for more information.\n");
      return 1; /* failure */
    }
  }

  if (optind < argc)
    strcpy (fname, argv[optind]);

  /* read surface in */
  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());
  fp = gts_file_new (stdin);
  if (gts_surface_read (s, fp)) {
    fputs ("split: file on standard input is not a valid GTS file\n", 
	   stderr);
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1; /* failure */
  }

  /* if verbose on print stats */
  if (verbose)
    gts_surface_print_stats (s, stderr);

  i = gts_surface_split (s);
  while (i) {
    gchar name[80];
    FILE * fp;

    sprintf (name, "%s.%d.gts", fname, ncomp++);
    fp = fopen (name, "wt");
    if (fp == NULL) {
      fprintf (stderr, "split: cannot open file `%s'.\n", name);
      exit (1);
    }
    gts_surface_write (i->data, fp);
    fclose (fp);

    if (verbose) {
      fprintf (stderr, "# Component %d\n", ncomp - 1);
      gts_surface_print_stats (i->data, stderr);
    }

    i = i->next;
  }

  return 0; /* success */
}
