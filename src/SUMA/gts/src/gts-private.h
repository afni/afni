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

#ifndef __GTS_PRIVATE_H__
#define __GTS_PRIVATE_H__

#define USE_ROBUST_PREDICATES

/* Debugging flags */
  
/* #define DEBUG_FUNCTIONS */

#ifdef DEBUG_FUNCTIONS
/* #define DEBUG_LEAKS */
#define DEBUG_IDENTITY
guint id (gpointer p);
void id_insert (gpointer p);
void id_remove (gpointer p);
void gts_write_triangle (GtsTriangle * t, GtsPoint * o, FILE * fptr);
void gts_write_segment (GtsSegment * s, GtsPoint * o, FILE * fptr);
#endif /* DEBUG_FUNCTIONS */

#endif /* __GTS_PRIVATE_H__ */
