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

/* This program creates a header file defining the various constants needed by
 * predicates.c. These constant are machine dependent.
 * adapted from predicates.c by Jonathan Richard Shewchuk
 */

#include <stdio.h>

/* FPU control. We MUST have only double precision (not extended precision) */
#include "rounding.h"

int main (int argc, char * argv[])
{
  double half = 0.5;
  double check = 1.0, lastcheck;
  int every_other = 1;
  /* epsilon = 2^(-p).  Used to estimate roundoff errors. */
  double epsilon = 1.0;   
  /* splitter = 2^ceiling(p / 2) + 1.  Used to split floats in half. */
  double splitter = 1.0;
  /* A set of coefficients used to calculate maximum roundoff errors. */
  double resulterrbound;
  double ccwerrboundA, ccwerrboundB, ccwerrboundC;
  double o3derrboundA, o3derrboundB, o3derrboundC;
  double iccerrboundA, iccerrboundB, iccerrboundC;
  double isperrboundA, isperrboundB, isperrboundC;

  FPU_ROUND_DOUBLE;

  epsilon = 1.0;
  splitter = 1.0;
  /* Repeatedly divide `epsilon' by two until it is too small to add to   */
  /* one without causing roundoff.  (Also check if the sum is equal to    */
  /* the previous sum, for machines that round up instead of using exact  */
  /* rounding.  Not that this library will work on such machines anyway). */
  do {
    lastcheck = check;
    epsilon *= half;
    if (every_other) {
      splitter *= 2.0;
    }
    every_other = !every_other;
    check = 1.0 + epsilon;
  } while ((check != 1.0) && (check != lastcheck));
  splitter += 1.0;
  /* Error bounds for orientation and incircle tests. */
  resulterrbound = (3.0 + 8.0 * epsilon) * epsilon;
  ccwerrboundA = (3.0 + 16.0 * epsilon) * epsilon;
  ccwerrboundB = (2.0 + 12.0 * epsilon) * epsilon;
  ccwerrboundC = (9.0 + 64.0 * epsilon) * epsilon * epsilon;
  o3derrboundA = (7.0 + 56.0 * epsilon) * epsilon;
  o3derrboundB = (3.0 + 28.0 * epsilon) * epsilon;
  o3derrboundC = (26.0 + 288.0 * epsilon) * epsilon * epsilon;
  iccerrboundA = (10.0 + 96.0 * epsilon) * epsilon;
  iccerrboundB = (4.0 + 48.0 * epsilon) * epsilon;
  iccerrboundC = (44.0 + 576.0 * epsilon) * epsilon * epsilon;
  isperrboundA = (16.0 + 224.0 * epsilon) * epsilon;
  isperrboundB = (5.0 + 72.0 * epsilon) * epsilon;
  isperrboundC = (71.0 + 1408.0 * epsilon) * epsilon * epsilon;

  puts ("/* This file was generated automatically by predicates_init\n"
	" *\n"
	" * This file is free software; you can redistribute it and/or\n"
	" * modify it under the terms of the GNU Library General Public\n"
	" * License as published by the Free Software Foundation; either\n"
	" * version 2 of the License, or (at your option) any later version.\n"
	" *\n"
	" * This file is distributed in the hope that it will be useful,\n"
	" * but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
	" * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
	" */\n");
  printf ("static double splitter = %f;\n", splitter);
  printf ("static double resulterrbound = %.16g;\n", resulterrbound);
  printf ("static double ccwerrboundA = %.16g;\n", ccwerrboundA);
  printf ("static double ccwerrboundB = %.16g;\n", ccwerrboundB);
  printf ("static double ccwerrboundC = %.16g;\n", ccwerrboundC);
  printf ("static double o3derrboundA = %.16g;\n", o3derrboundA);
  printf ("static double o3derrboundB = %.16g;\n", o3derrboundB);
  printf ("static double o3derrboundC = %.16g;\n", o3derrboundC);
  printf ("static double iccerrboundA = %.16g;\n", iccerrboundA);
  printf ("static double iccerrboundB = %.16g;\n", iccerrboundB);
  printf ("static double iccerrboundC = %.16g;\n", iccerrboundC);
  printf ("static double isperrboundA = %.16g;\n", isperrboundA);
  printf ("static double isperrboundB = %.16g;\n", isperrboundB);
  printf ("static double isperrboundC = %.16g;\n", isperrboundC);
  
  FPU_RESTORE;

  return 0;
}
