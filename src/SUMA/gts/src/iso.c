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

#include "gts.h"

typedef enum { LEFT = 0, RIGHT = 1 } Orientation;

typedef struct {
  GtsVertex * v;
  Orientation orientation;
} OrientedVertex;

struct _GtsIsoSlice {
  OrientedVertex *** vertices;
  guint nx, ny;
};

/* coordinates of the edges of the cube (see doc/isocube.fig) */
static guint c[12][4] = {
  {0, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 1}, {0, 0, 1, 0},
  {1, 0, 0, 0}, {1, 0, 0, 1}, {1, 1, 0, 1}, {1, 1, 0, 0},
  {2, 0, 0, 0}, {2, 1, 0, 0}, {2, 1, 1, 0}, {2, 0, 1, 0}};

/* first index is the edge number, second index is the edge orientation 
   (RIGHT or LEFT), third index are the edges which this edge may connect to
   in order */
static guint edge[12][2][3] = {
  {{9, 1, 8}, {4, 3, 7}},   /* 0 */
  {{6, 2, 5}, {8, 0, 9}},   /* 1 */
  {{10, 3, 11}, {5, 1, 6}}, /* 2 */
  {{7, 0, 4}, {11, 2, 10}}, /* 3 */
  {{3, 7, 0}, {8, 5, 11}},  /* 4 */
  {{11, 4, 8}, {1, 6, 2}},  /* 5 */
  {{2, 5, 1}, {9, 7, 10}},  /* 6 */
  {{10, 6, 9}, {0, 4, 3}},  /* 7 */
  {{5, 11, 4}, {0, 9, 1}},  /* 8 */
  {{1, 8, 0}, {7, 10, 6}},  /* 9 */
  {{6, 9, 7}, {3, 11, 2}},  /* 10 */
  {{2, 10, 3}, {4, 8, 5}}   /* 11 */
};

static void ** malloc2D (guint nx, guint ny, gulong size)
{
  void ** m = g_malloc (nx*sizeof (void *));
  guint i;

  for (i = 0; i < nx; i++)
    m[i] = g_malloc0 (ny*size);

  return m;
}

static void free2D (void ** m, guint nx)
{
  guint i;

  g_return_if_fail (m != NULL);

  for (i = 0; i < nx; i++)
    g_free (m[i]);
  g_free (m);
}

/**
 * gts_grid_plane_new:
 * @nx:
 * @ny:
 *
 * Returns:
 */
GtsGridPlane * gts_grid_plane_new (guint nx, guint ny)
{
  GtsGridPlane * g = g_malloc (sizeof (GtsGridPlane));

  g->p = (GtsPoint **) malloc2D (nx, ny, sizeof (GtsPoint));
  g->nx = nx;
  g->ny = ny;
  
  return g;
}

/**
 * gts_grid_plane_destroy:
 * @g:
 *
 */
void gts_grid_plane_destroy (GtsGridPlane * g)
{
  g_return_if_fail (g != NULL);

  free2D ((void **) g->p, g->nx);
  g_free (g);
}

/**
 * gts_iso_slice_new:
 * @nx: number of vertices in the x direction.
 * @ny: number of vertices in the y direction.
 *
 * Returns: a new #GtsIsoSlice.
 */
GtsIsoSlice * gts_iso_slice_new (guint nx, guint ny)
{
  GtsIsoSlice * slice;

  g_return_val_if_fail (nx > 1, NULL);
  g_return_val_if_fail (ny > 1, NULL);

  slice = g_malloc (sizeof (GtsIsoSlice));

  slice->vertices = g_malloc (3*sizeof (OrientedVertex **));
  slice->vertices[0] = 
    (OrientedVertex **) malloc2D (nx, ny, sizeof (OrientedVertex));
  slice->vertices[1] = 
    (OrientedVertex **) malloc2D (nx - 1, ny, sizeof (OrientedVertex));
  slice->vertices[2] = 
    (OrientedVertex **) malloc2D (nx, ny - 1, sizeof (OrientedVertex));
  slice->nx = nx;
  slice->ny = ny;

  return slice;
}

/**
 * gts_iso_slice_fill:
 * @slice: a #GtsIsoSlice.
 * @plane1: a #GtsGridPlane.
 * @plane2: another #GtsGridPlane.
 * @f1: values of the function corresponding to @plane1.
 * @f2: values of the function corresponding to @plane2.
 * @iso: isosurface value.
 * @klass: a #GtsVertexClass or one of its descendant to be used for the 
 * new vertices.
 *
 * Fill @slice with the coordinates of the vertices defined by 
 * f1 (x,y,z) = @iso and f2 (x, y, z) = @iso.
 */
void gts_iso_slice_fill (GtsIsoSlice * slice,
			 GtsGridPlane * plane1,
			 GtsGridPlane * plane2,
			 gdouble ** f1,
			 gdouble ** f2,
			 gdouble iso,
			 GtsVertexClass * klass)
{
  OrientedVertex *** vertices;
  GtsPoint ** p1, ** p2 = NULL;
  guint i, j, nx, ny;

  g_return_if_fail (slice != NULL);
  g_return_if_fail (plane1 != NULL);
  g_return_if_fail (f1 != NULL);
  g_return_if_fail (f2 == NULL || plane2 != NULL);

  p1 = plane1->p;
  if (plane2) 
    p2 = plane2->p;
  vertices = slice->vertices;
  nx = slice->nx;
  ny = slice->ny;

  if (f2)
    for (i = 0; i < nx; i++)
      for (j = 0; j < ny; j++) {
	gdouble v1 = f1[i][j] - iso;
	gdouble v2 = f2[i][j] - iso;
	if ((v1 >= 0. && v2 < 0.) || (v1 < 0. && v2 >= 0.)) {
	  gdouble c2 = v1/(v1 - v2), c1 = 1. - c2;
	  vertices[0][i][j].v = 
	    gts_vertex_new (klass,
			    c1*p1[i][j].x + c2*p2[i][j].x,
			    c1*p1[i][j].y + c2*p2[i][j].y,
			    c1*p1[i][j].z + c2*p2[i][j].z);
	  vertices[0][i][j].orientation = v2 >= 0. ? RIGHT : LEFT;
	}
	else
	  vertices[0][i][j].v = NULL;
      }
  for (i = 0; i < nx - 1; i++)
    for (j = 0; j < ny; j++) {
      gdouble v1 = f1[i][j] - iso;
      gdouble v2 = f1[i+1][j] - iso;
      if ((v1 >= 0. && v2 < 0.) || (v1 < 0. && v2 >= 0.)) {
	gdouble c2 = v1/(v1 - v2), c1 = 1. - c2;
	vertices[1][i][j].v = 
	  gts_vertex_new (klass,
			  c1*p1[i][j].x + c2*p1[i+1][j].x,
			  c1*p1[i][j].y + c2*p1[i+1][j].y,
			  c1*p1[i][j].z + c2*p1[i+1][j].z);
	vertices[1][i][j].orientation = v2 >= 0. ? RIGHT : LEFT;
      }
      else
	vertices[1][i][j].v = NULL;
    }
  for (i = 0; i < nx; i++)
    for (j = 0; j < ny - 1; j++) {
      gdouble v1 = f1[i][j] - iso;
      gdouble v2 = f1[i][j+1] - iso;
      if ((v1 >= 0. && v2 < 0.) || (v1 < 0. && v2 >= 0.)) {
	gdouble c2 = v1/(v1 - v2), c1 = 1. - c2;
	vertices[2][i][j].v = 
	  gts_vertex_new (klass,
			  c1*p1[i][j].x + c2*p1[i][j+1].x,
			  c1*p1[i][j].y + c2*p1[i][j+1].y,
			  c1*p1[i][j].z + c2*p1[i][j+1].z);
	vertices[2][i][j].orientation = v2 >= 0. ? RIGHT : LEFT;
      }
      else
	vertices[2][i][j].v = NULL;
    }
}
 
/**
 * gts_iso_slice_fill_cartesian:
 * @slice: a #GtsIsoSlice.
 * @g: a #GtsCartesianGrid.
 * @f1: values of the function for plane z = @g.z.
 * @f2: values of the function for plane z = @g.z + @g.dz.
 * @iso: isosurface value.
 * @klass: a #GtsVertexClass.
 *
 * Fill @slice with the coordinates of the vertices defined by 
 * f1 (x,y,z) = @iso and f2 (x, y, z) = @iso.
 */
void gts_iso_slice_fill_cartesian (GtsIsoSlice * slice,
				   GtsCartesianGrid g,
				   gdouble ** f1,
				   gdouble ** f2,
				   gdouble iso,
				   GtsVertexClass * klass)
{
  OrientedVertex *** vertices;
  guint i, j;
  gdouble x, y;

  g_return_if_fail (slice != NULL);
  g_return_if_fail (f1 != NULL);

  vertices = slice->vertices;

  if (f2)
    for (i = 0, x = g.x; i < g.nx; i++, x += g.dx)
      for (j = 0, y = g.y; j < g.ny; j++, y += g.dy) {
	gdouble v1 = f1[i][j] - iso;
	gdouble v2 = f2[i][j] - iso;
	if ((v1 >= 0. && v2 < 0.) || (v1 < 0. && v2 >= 0.)) {
	  vertices[0][i][j].v = 
	    gts_vertex_new (klass,
			    x, y, g.z + g.dz*v1/(v1 - v2));
	  vertices[0][i][j].orientation = v2 >= 0. ? RIGHT : LEFT;
	}
	else
	  vertices[0][i][j].v = NULL;
      }
  for (i = 0, x = g.x; i < g.nx - 1; i++, x += g.dx)
    for (j = 0, y = g.y; j < g.ny; j++, y += g.dy) {
      gdouble v1 = f1[i][j] - iso;
      gdouble v2 = f1[i+1][j] - iso;
      if ((v1 >= 0. && v2 < 0.) || (v1 < 0. && v2 >= 0.)) {
	vertices[1][i][j].v = 
	  gts_vertex_new (klass, x + g.dx*v1/(v1 - v2), y, g.z);
	vertices[1][i][j].orientation = v2 >= 0. ? RIGHT : LEFT;
      }
      else
	vertices[1][i][j].v = NULL;
    }
  for (i = 0, x = g.x; i < g.nx; i++, x += g.dx)
    for (j = 0, y = g.y; j < g.ny - 1; j++, y += g.dy) {
      gdouble v1 = f1[i][j] - iso;
      gdouble v2 = f1[i][j+1] - iso;
      if ((v1 >= 0. && v2 < 0.) || (v1 < 0. && v2 >= 0.)) {
	vertices[2][i][j].v = 
	  gts_vertex_new (klass, x, y + g.dy*v1/(v1 - v2), g.z);
	vertices[2][i][j].orientation = v2 >= 0. ? RIGHT : LEFT;
      }
      else
	vertices[2][i][j].v = NULL;
    }
}

/**
 * gts_iso_slice_destroy:
 * @slice: a #GtsIsoSlice.
 *
 * Free all memory allocated for @slice.
 */
void gts_iso_slice_destroy (GtsIsoSlice * slice)
{
  g_return_if_fail (slice != NULL);

  free2D ((void **) slice->vertices[0], slice->nx);
  free2D ((void **) slice->vertices[1], slice->nx - 1);
  free2D ((void **) slice->vertices[2], slice->nx);  
  g_free (slice->vertices);
  g_free (slice);
}

/**
 * gts_isosurface_slice:
 * @slice1: a #GtsIsoSlice.
 * @slice2: another #GtsIsoSlice.
 * @surface: a #GtsSurface.
 *
 * Given two successive slices @slice1 and @slice2 link their vertices with
 * segments and triangles which are added to @surface.
 */
void gts_isosurface_slice (GtsIsoSlice * slice1,
			   GtsIsoSlice * slice2,
			   GtsSurface * surface)
{
  guint j, k, l, nx, ny;
  OrientedVertex *** vertices[2];
  GtsVertex * va[12];

  g_return_if_fail (slice1 != NULL);
  g_return_if_fail (slice2 != NULL);
  g_return_if_fail (surface != NULL);
  g_return_if_fail (slice1->nx == slice2->nx && slice1->ny == slice2->ny);

  vertices[0] = slice1->vertices;
  vertices[1] = slice2->vertices;
  nx = slice1->nx;
  ny = slice1->ny;

  /* link vertices with segments and triangles */
  for (j = 0; j < nx - 1; j++)
    for (k = 0; k < ny - 1; k++) {
      gboolean cube_is_cut = FALSE;
      for (l = 0; l < 12; l++) {
	guint nv = 0, e = l;
	OrientedVertex ov = 
	  vertices[c[e][1]][c[e][0]][j + c[e][2]][k + c[e][3]];
	while (ov.v && !GTS_OBJECT (ov.v)->reserved) {
	  guint m = 0, * ne = edge[e][ov.orientation];
	  va[nv++] = ov.v;
	  GTS_OBJECT (ov.v)->reserved = surface;
	  ov.v = NULL;
	  while (m < 3 && !ov.v) {
	    e = ne[m++];
	    ov = vertices[c[e][1]][c[e][0]][j + c[e][2]][k + c[e][3]];
	  }
	}
	/* create edges and faces */
	if (nv > 2) {
	  GtsEdge * e1, * e2, * e3;
	  guint m;
	  if (!(e1 = GTS_EDGE (gts_vertices_are_connected (va[0], va[1]))))
	    e1 = gts_edge_new (surface->edge_class, va[0], va[1]);
	  for (m = 1; m < nv - 1; m++) {
	    if (!(e2 = GTS_EDGE (gts_vertices_are_connected (va[m], va[m+1]))))
	      e2 = gts_edge_new (surface->edge_class, va[m], va[m+1]);
	    if (!(e3 = GTS_EDGE (gts_vertices_are_connected (va[m+1], va[0]))))
	      e3 = gts_edge_new (surface->edge_class, va[m+1], va[0]);
	    gts_surface_add_face (surface, 
				  gts_face_new (surface->face_class,
						e1, e2, e3));
	    e1 = e3;
	  }
	}
	if (nv > 0)
	  cube_is_cut = TRUE;
      }
      if (cube_is_cut)
	for (l = 0; l < 12; l++) {
	  GtsVertex * v = 
	    vertices[c[l][1]][c[l][0]][j + c[l][2]][k + c[l][3]].v;
	  if (v)
	    GTS_OBJECT (v)->reserved = NULL;
	}
    }
}

#define SWAP(s1, s2, tmp) (tmp = s1, s1 = s2, s2 = tmp)

/**
 * gts_isosurface_cartesian:
 * @surface: a #GtsSurface.
 * @g: a #GtsCartesianGrid.
 * @f: a #GtsIsoCartesianFunc.
 * @data: user data to be passed to @f.
 * @iso: isosurface value.
 *
 * Adds to @surface new faces defining the isosurface f(x,y,z) = @iso. By
 * convention, the normals to the surface are pointing toward the positive
 * values of f(x,y,z) - @iso.
 *
 * The user function @f is called successively for each value of the z 
 * coordinate defined by @g. It must fill the corresponding (x,y) plane with
 * the values of the function for which the isosurface is to be computed.
 */
void gts_isosurface_cartesian (GtsSurface * surface,
			       GtsCartesianGrid g,
			       GtsIsoCartesianFunc f,
			       gpointer data,
			       gdouble iso)
{
  void * tmp;
  gdouble ** f1, ** f2;
  GtsIsoSlice * slice1, * slice2;
  guint i;

  g_return_if_fail (surface != NULL);
  g_return_if_fail (f != NULL);
  g_return_if_fail (g.nx > 1);
  g_return_if_fail (g.ny > 1);
  g_return_if_fail (g.nz > 1);

  slice1 = gts_iso_slice_new (g.nx, g.ny);
  slice2 = gts_iso_slice_new (g.nx, g.ny);
  f1 = (gdouble **) malloc2D (g.nx, g.ny, sizeof (gdouble));
  f2 = (gdouble **) malloc2D (g.nx, g.ny, sizeof (gdouble));

  (*f) (f1, g, 0, data);
  g.z += g.dz;
  (*f) (f2, g, 1, data);
  g.z -= g.dz;
  gts_iso_slice_fill_cartesian (slice1, g, f1, f2, iso, 
				surface->vertex_class);
  g.z += g.dz;
  for (i = 2; i < g.nz; i++) {
    g.z += g.dz;
    (*f) (f1, g, i, data);
    SWAP (f1, f2, tmp);
    g.z -= g.dz;
    gts_iso_slice_fill_cartesian (slice2, g, f1, f2, iso, 
				  surface->vertex_class);
    g.z += g.dz;
    gts_isosurface_slice (slice1, slice2, surface);
    SWAP (slice1, slice2, tmp);
  }
  gts_iso_slice_fill_cartesian (slice2, g, f2, NULL, iso,
				surface->vertex_class);
  gts_isosurface_slice (slice1, slice2, surface);

  gts_iso_slice_destroy (slice1);
  gts_iso_slice_destroy (slice2);
  free2D ((void **) f1, g.nx);
  free2D ((void **) f2, g.nx);
}
