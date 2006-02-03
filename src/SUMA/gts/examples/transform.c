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

#ifndef PI
#define PI 3.14159265359
#endif

int main (int argc, char * argv[])
{
  GtsSurface * s;
  GtsFile * fp;
  GtsMatrix * m;
  int c = 0;
  gboolean verbose = FALSE;
  gboolean revert = FALSE;
  gboolean normalize = FALSE;

  m = gts_matrix_identity (NULL);

  /* parse options using getopt */
  while (c != EOF) {
#ifdef HAVE_GETOPT_LONG
    static struct option long_options[] = {
      {"rx", required_argument, NULL, 'r'},
      {"ry", required_argument, NULL, 'm'},
      {"rz", required_argument, NULL, 'n'},
      {"scale", required_argument, NULL, 's'},
      {"sx", required_argument, NULL, 'R'},
      {"sy", required_argument, NULL, 'M'},
      {"sz", required_argument, NULL, 'N'},
      {"tx", required_argument, NULL, 't'},
      {"ty", required_argument, NULL, 'u'},
      {"tz", required_argument, NULL, 'w'},
      {"revert", no_argument, NULL, 'i'},
      {"normalize", no_argument, NULL, 'o'},
      {"help", no_argument, NULL, 'h'},
      {"verbose", no_argument, NULL, 'v'},
    };
    int option_index = 0;
    switch ((c = getopt_long (argc, argv, "hvr:m:n:s:R:M:N:it:u:w:o",
			      long_options, &option_index))) {
#else /* not HAVE_GETOPT_LONG */
    switch ((c = getopt (argc, argv, "hvr:m:n:s:R:M:N:it:u:w:o"))) {
#endif /* not HAVE_GETOPT_LONG */
    case 'o': /* normalize */
      normalize = TRUE;
      break;
    case 'r': { /* rotate around x-axis */
      gdouble angle, cosa, sina;
      GtsMatrix * rot, * p;
      
      rot = gts_matrix_identity (NULL);
      angle = atof (optarg)*PI/180.;
      cosa = cos (angle);
      sina = sin (angle);
      rot[1][1] = cosa; rot[1][2] = -sina;
      rot[2][1] = sina; rot[2][2] = cosa;
      p = gts_matrix_product (m, rot);
      gts_matrix_destroy (rot);
      gts_matrix_destroy (m);
      m = p;
      break;
    }
    case 'm': { /* rotate around y-axis */
      gdouble angle, cosa, sina;
      GtsMatrix * rot, * p;
      
      rot = gts_matrix_identity (NULL);
      angle = atof (optarg)*PI/180.;
      cosa = cos (angle);
      sina = sin (angle);
      rot[0][0] = cosa; rot[0][2] = sina;
      rot[2][0] = -sina; rot[2][2] = cosa;
      p = gts_matrix_product (m, rot);
      gts_matrix_destroy (rot);
      gts_matrix_destroy (m);
      m = p;
      break;
    }
    case 'n': { /* rotate around z-axis */
      gdouble angle, cosa, sina;
      GtsMatrix * rot, * p;
      
      rot = gts_matrix_identity (NULL);
      angle = atof (optarg)*PI/180.;
      cosa = cos (angle);
      sina = sin (angle);
      rot[0][0] = cosa; rot[0][1] = -sina;
      rot[1][0] = sina; rot[1][1] = cosa;
      p = gts_matrix_product (m, rot);
      gts_matrix_destroy (rot);
      gts_matrix_destroy (m);
      m = p;
      break;
    }
    case 's': { /* scale */
      GtsMatrix * scale, * p;
      gdouble s = atof (optarg);

      scale = gts_matrix_identity (NULL);
      scale[0][0] = scale[1][1] = scale[2][2] = s;
      p = gts_matrix_product (m, scale);
      gts_matrix_destroy (scale);
      gts_matrix_destroy (m);
      m = p;
      break;
    }
    case 'R': { /* sx */
      GtsMatrix * scale, * p;
      gdouble s = atof (optarg);

      scale = gts_matrix_identity (NULL);
      scale[0][0] = s;
      p = gts_matrix_product (m, scale);
      gts_matrix_destroy (scale);
      gts_matrix_destroy (m);
      m = p;
      break;
    }
    case 'M': { /* sy */
      GtsMatrix * scale, * p;
      gdouble s = atof (optarg);

      scale = gts_matrix_identity (NULL);
      scale[1][1] = s;
      p = gts_matrix_product (m, scale);
      gts_matrix_destroy (scale);
      gts_matrix_destroy (m);
      m = p;
      break;
    }
    case 'N': { /* sz */
      GtsMatrix * scale, * p;
      gdouble s = atof (optarg);

      scale = gts_matrix_identity (NULL);
      scale[2][2] = s;
      p = gts_matrix_product (m, scale);
      gts_matrix_destroy (scale);
      gts_matrix_destroy (m);
      m = p;
      break;
    }
    case 't': /* tx */
      m[0][3] += atof (optarg);
      break;
    case 'u': /* ty */
      m[1][3] += atof (optarg);
      break;
    case 'w': /* tz */
      m[2][3] += atof (optarg);
      break;
    case 'i': /* revert */
      revert = TRUE;
      break;
    case 'v': /* verbose */
      verbose = TRUE;
      break;
    case 'h': /* help */
      fprintf (stderr,
             "Usage: transform [OPTION] < file.gts\n"
	     "Apply geometric transformations to the input.\n"
	     "\n"
	     "  -r ANGLE  --rx=ANGLE      rotate around x-axis (angle in degrees)\n"
	     "  -m ANGLE  --ry=ANGLE      rotate around y-axis\n"
	     "  -n ANGLE  --rz=ANGLE      rotate around z-axis\n"
	     "  -s FACTOR --scale=FACTOR  scale by FACTOR\n"
	     "  -R FACTOR --sx=FACTOR     scale x-axis by FACTOR\n"
	     "  -M FACTOR --sy=FACTOR     scale y-axis by FACTOR\n"
	     "  -N FACTOR --sz=FACTOR     scale z-axis by FACTOR\n"
             "  -t V      --tx=V          translate of V along x-axis\n"
             "  -u V      --ty=V          translate of V along y-axis\n"
             "  -w V      --tz=V          translate of V along z-axis\n"
	     "  -i        --revert        turn surface inside out\n"
             "  -o        --normalize     fit the resulting surface in a cube of\n"
	     "                            size 1 centered at the origin\n"
	     "  -v        --verbose       print statistics about the surface\n"
	     "  -h        --help          display this help and exit\n"
	     "\n"
	     "Reports bugs to %s\n",
	     GTS_MAINTAINER);
      return 0; /* success */
      break;
    case '?': /* wrong options */
      fprintf (stderr, "Try `transform --help' for more information.\n");
      return 1; /* failure */
    }
  }

  s = gts_surface_new (gts_surface_class (),
		       gts_face_class (),
		       gts_edge_class (),
		       gts_vertex_class ());
  fp = gts_file_new (stdin);
  if (gts_surface_read (s, fp)) {
    fputs ("transform: file on standard input is not a valid GTS file\n", 
	   stderr);
    fprintf (stderr, "stdin:%d:%d: %s\n", fp->line, fp->pos, fp->error);
    return 1; /* failure */
  }

  if (verbose)
    gts_surface_print_stats (s, stderr);

  if (revert)
    gts_surface_foreach_face (s, (GtsFunc) gts_triangle_revert, NULL);
  gts_surface_foreach_vertex (s, (GtsFunc) gts_point_transform, m);
  if (normalize) {
    GtsBBox * bb = gts_bbox_surface (gts_bbox_class (), s);
    gdouble scale = bb->x2 - bb->x1;
    GtsMatrix * sc;

    if (bb->y2 - bb->y1 > scale) scale = bb->y2 - bb->y1;
    if (bb->z2 - bb->z1 > scale) scale = bb->z2 - bb->z1;
    if (scale > 0.) scale = 1./scale;
    else scale = 1.;
    sc = gts_matrix_identity (NULL);
    sc[0][3] = - (bb->x1 + bb->x2)/2.;
    sc[1][3] = - (bb->y1 + bb->y2)/2.;
    sc[2][3] = - (bb->z1 + bb->z2)/2.;
    gts_surface_foreach_vertex (s, (GtsFunc) gts_point_transform, sc);
    sc[0][0] = sc[1][1] = sc[2][2] = scale;    
    sc[0][3] = sc[1][3] = sc[2][3] = 0.;
    gts_surface_foreach_vertex (s, (GtsFunc) gts_point_transform, sc);
    gts_matrix_destroy (sc);
  }
  gts_surface_write (s, stdout);

  return 0;
}
