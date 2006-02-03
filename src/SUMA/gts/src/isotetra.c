/* GTS-Library conform marching tetrahedra algorithm 
 * Copyright (C) 2002 Gert Wollny
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
#include <string.h>
#include <gts.h>
#ifdef NATIVE_WIN32
# include <memory.h>
# define M_SQRT2		1.41421356237309504880
#endif /* NATIVE_WIN32 */

typedef struct {
  gint nx, ny; 
  gdouble ** data; 
} slice_t;

typedef struct {
  gint x, y, z;
  gboolean mid;
  gdouble d; 
} tetra_vertex_t; 

/* this helper is a lookup table for vertices */
typedef struct {
  gint nx, ny; 
  GtsVertex ** vtop, ** vmid, **vbot;
} helper_t ;

typedef struct {
  GHashTable * vbot, * vtop;
} helper_bcl ;


static helper_t * init_helper (int nx, int ny) 
{
  gint nxy = 4*nx*ny; 
  helper_t *retval = g_malloc0 (sizeof (helper_t));

  retval->nx = nx; 
  retval->ny = ny; 
  retval->vtop = g_malloc0 (sizeof (GtsVertex *)*nxy);
  retval->vmid = g_malloc0 (sizeof (GtsVertex *)*nxy);
  retval->vbot = g_malloc0 (sizeof (GtsVertex *)*nxy);
  return retval;
}

static helper_bcl * init_helper_bcl (void)
{
  helper_bcl *retval = g_malloc0 (sizeof (helper_bcl));

  retval->vtop = g_hash_table_new (g_str_hash, g_str_equal);
  retval->vbot = g_hash_table_new (g_str_hash, g_str_equal);
  return retval;
}

static void free_helper (helper_t * h) 
{
  g_free (h->vtop);
  g_free (h->vmid);
  g_free (h->vbot);
  g_free (h);
}

static void free_helper_bcl (helper_bcl * h) 
{
  g_hash_table_destroy (h->vtop);
  g_hash_table_destroy (h->vbot);
  g_free (h);
}

/* move the vertices in the bottom slice to the top, and clear the
   other slices in the lookup tables */
static void helper_advance (helper_t * h) 
{
  GtsVertex ** help = h->vbot;
  h->vbot = h->vtop; 
  h->vtop = help;
  
  memset (h->vmid, 0, 4*sizeof(GtsVertex *) * h->nx * h->ny);
  memset (h->vbot, 0, 4*sizeof(GtsVertex *) * h->nx * h->ny);
}

static void helper_advance_bcl (helper_bcl * h) 
{
  GHashTable * help = g_hash_table_new (g_str_hash, g_str_equal);

  g_hash_table_destroy (h->vbot);
  h->vbot = h->vtop;
  h->vtop = help;
}

/* find the zero-crossing of line through v1 and v2 and return the
   corresponding GtsVertex */
static GtsVertex * get_vertex (gint mz, 
			       const tetra_vertex_t * v1, 
			       const tetra_vertex_t * v2, 
			       helper_t * help, 
			       GtsCartesianGrid * g,
			       GtsVertexClass * klass)
{
  GtsVertex ** vertex; 
  gint x, y, index, idx2, z; 
  gdouble dx, dy, dz, d; 

  g_assert (v1->d - v2->d != 0.);
  
  dx = dy = dz = 0.0;
  d = v1->d/(v1->d - v2->d);

  index = 0;
  
  if (v1->x != v2->x) {
    index |= 1;
    dx = d;
  }
  
  if (v1->y != v2->y) {
    index |= 2;
    dy = d;
  }
  
  if (v1->z != v2->z) {
    dz = d;
  }

  x = v1->x;
  if (v1->x > v2->x) {  x = v2->x; dx = 1.0 - dx; }
  
  y = v1->y;
  if (v1->y > v2->y) {  y = v2->y; dy = 1.0 - dy;}
  
  z = v1->z;
  if (v1->z > v2->z) {  z = v2->z; dz = 1.0 - dz;}
  
  idx2 = 4 * ( x + y * help->nx ) + index;
  
  if (v1->z == v2->z)
    vertex = (mz == z) ? &help->vtop[idx2] : &help->vbot[idx2];
  else
    vertex = &help->vmid[idx2];
  
  if (mz != z && dz != 0.0) {
    fprintf(stderr, "%f \n", dz);
  }
  
  /* if vertex is not yet created, do it now */
  if (!*vertex)
    *vertex = gts_vertex_new (klass,
			      g->dx * ( x + dx) + g->x, 
			      g->dy * ( y + dy) + g->y, 
			      g->dz * ( z + dz) + g->z);
  
  return *vertex;
}

static GtsVertex * get_vertex_bcl (gint mz, 
				   const tetra_vertex_t * v1, 
				   const tetra_vertex_t * v2, 
				   helper_bcl * help, 
				   GtsCartesianGrid * g,
				   GtsVertexClass * klass)
{
  GtsVertex * v;
  GHashTable * table;
  gchar * s1, * s2, * hash;
  gdouble x1, x2, y1, y2, z1, z2, d;

  g_assert (v1->d - v2->d != 0.);

  /* first find correct hash table */  
  if ((v1->z > mz) && (v2->z > mz))
    table = help->vtop;
  else
    table = help->vbot;

  d = v1->d / (v1->d - v2->d);

  /* sort vertices */
  s1 = g_strdup_printf ("%d %d %d %d", v1->x, v1->y, v1->z, v1->mid);
  s2 = g_strdup_printf ("%d %d %d %d", v2->x, v2->y, v2->z, v2->mid);

  hash = (d == 0.0) ? g_strdup (s1) :
    (d == 1.0) ? g_strdup (s2) :
    (strcmp (s1, s2) < 0) ? g_strjoin (" ", s1, s2, NULL) :
    g_strjoin (" ", s2, s1, NULL);

  /* return existing vertex or make a new one */
  v = g_hash_table_lookup (table, hash);
  if (!v){

    x1 = g->dx * (v1->x + (v1->mid / 2.0)) + g->x;
    x2 = g->dx * (v2->x + (v2->mid / 2.0)) + g->x;
    y1 = g->dy * (v1->y + (v1->mid / 2.0)) + g->y;
    y2 = g->dy * (v2->y + (v2->mid / 2.0)) + g->y;
    z1 = g->dz * (v1->z + (v1->mid / 2.0)) + g->z;
    z2 = g->dz * (v2->z + (v2->mid / 2.0)) + g->z;

    v = gts_vertex_new (klass, x1 * (1.0 - d) + d * x2,
			y1 * (1.0 - d) + d * y2,
			z1 * (1.0 - d) + d * z2);

    g_hash_table_insert (table, g_strdup(hash), v);
  }
  g_free (s1);
  g_free (s2);
  g_free (hash);

  return v;
}

/* create an edge connecting the zero crossings of lines through a
   pair of vertices, or return an existing one */
static GtsEdge * get_edge (GtsVertex * v1, GtsVertex * v2,
			   GtsEdgeClass * klass)
{
  GtsSegment *s;
  GtsEdge *edge; 
  
  g_assert (v1);
  g_assert (v2);
  
  s = gts_vertices_are_connected (v1, v2);
  
  if (GTS_IS_EDGE (s))
    edge = GTS_EDGE(s);
  else
    edge = gts_edge_new (klass, v1, v2);
  return edge; 
}

static void add_face (GtsSurface * surface, 
		      const tetra_vertex_t * a1, const tetra_vertex_t * a2, 
		      const tetra_vertex_t * b1, const tetra_vertex_t * b2, 
		      const tetra_vertex_t * c1, const tetra_vertex_t * c2, 
		      gint rev, helper_t * help, 
		      gint z, GtsCartesianGrid * g)
{
  GtsFace * t; 
  GtsEdge * e1, * e2, * e3; 	
  GtsVertex * v1 = get_vertex (z, a1, a2, help, g, surface->vertex_class);
  GtsVertex * v2 = get_vertex (z, b1, b2, help, g, surface->vertex_class);
  GtsVertex * v3 = get_vertex (z, c1, c2, help, g, surface->vertex_class);

  g_assert (v1 != v2);
  g_assert (v2 != v3);
  g_assert (v1 != v3);

  if (!rev) {
    e1 = get_edge (v1, v2, surface->edge_class);
    e2 = get_edge (v2, v3, surface->edge_class);
    e3 = get_edge (v1, v3, surface->edge_class);
  } else {
    e1 = get_edge (v1, v3, surface->edge_class);
    e2 = get_edge (v2, v3, surface->edge_class);
    e3 = get_edge (v1, v2, surface->edge_class);	
  }
  
  t = gts_face_new (surface->face_class, e1, e2, e3);	
  gts_surface_add_face (surface, t);
}

static void add_face_bcl (GtsSurface * surface, 
			  const tetra_vertex_t * a1, 
			  const tetra_vertex_t * a2, 
			  const tetra_vertex_t * b1, 
			  const tetra_vertex_t * b2, 
			  const tetra_vertex_t * c1, 
			  const tetra_vertex_t * c2, 
			  gint rev, helper_bcl * help, 
			  gint z, GtsCartesianGrid * g)
{
  GtsFace * t; 
  GtsEdge * e1, * e2, * e3; 	
  GtsVertex * v1 = get_vertex_bcl (z, a1, a2, help, g, surface->vertex_class);
  GtsVertex * v2 = get_vertex_bcl (z, b1, b2, help, g, surface->vertex_class);
  GtsVertex * v3 = get_vertex_bcl (z, c1, c2, help, g, surface->vertex_class);

  if (v1 == v2 || v2 == v3 || v1 == v3)
    return;

  if (!rev) {
    e1 = get_edge (v1, v2, surface->edge_class);
    e2 = get_edge (v2, v3, surface->edge_class);
    e3 = get_edge (v1, v3, surface->edge_class);
  } else {
    e1 = get_edge (v1, v3, surface->edge_class);
    e2 = get_edge (v2, v3, surface->edge_class);
    e3 = get_edge (v1, v2, surface->edge_class);	
  }

  t = gts_face_new (surface->face_class, e1, e2, e3);	
  gts_surface_add_face (surface, t);
}

/* create a new slice of site nx \times ny */
static slice_t * new_slice (gint nx, gint ny) 
{
  gint x; 
  slice_t * retval = g_malloc (sizeof (slice_t));

  retval->data = g_malloc (nx*sizeof(gdouble *));
  retval->nx = nx;
  retval->ny = ny;  
  for (x = 0; x < nx; x++) 
    retval->data[x] = g_malloc (ny*sizeof (gdouble));
  return retval; 
}

/* initialize a slice with inival */
static void slice_init (slice_t * slice, gdouble inival)
{
  gint x, y; 
  
  g_assert (slice);
	
  for (x = 0; x < slice->nx; x++) 
    for (y = 0; y < slice->ny; y++)
      slice->data[x][y] = inival; 
}

/* free the memory of a slice */
static void free_slice (slice_t * slice) 
{
  gint x; 
	
  g_return_if_fail (slice != NULL);
	
  for (x = 0; x < slice->nx; x++) 
    g_free (slice->data[x]);  
  g_free (slice->data);
  g_free (slice);
}

static void analyze_tetrahedra (const tetra_vertex_t * a, 
				const tetra_vertex_t * b, 
				const tetra_vertex_t * c, 
				const tetra_vertex_t * d, 
				gint parity, GtsSurface * surface, 
				helper_t * help, 
				gint z, GtsCartesianGrid * g)
{
  gint rev = parity; 
  gint code = 0; 
	
  if (a->d >= 0.) code |= 1; 
  if (b->d >= 0.) code |= 2; 
  if (c->d >= 0.) code |= 4; 
  if (d->d >= 0.) code |= 8;
		
  switch (code) {
  case 15:
  case 0: return; /* all inside or outside */		
  
  case 14:rev = !parity;
  case  1:add_face (surface, a, b, a, d, a, c, rev, help, z, g);
	  break;
  case 13:rev = ! parity;  
  case  2:add_face (surface, a, b, b, c, b, d, rev, help, z, g);
	  break;
  case 12:rev = !parity;	  
  case  3:add_face (surface, a, d, a, c, b, c, rev, help, z, g);
	  add_face (surface, a, d, b, c, b, d, rev, help, z, g);
	  break;
  case 11:rev = !parity;	  
  case  4:add_face (surface, a, c, c, d, b, c, rev, help, z, g);
	  break;
  case 10:rev = !parity; 	  
  case 5: add_face (surface, a, b, a, d, c, d, rev, help, z, g);
	  add_face (surface, a, b, c, d, b, c, rev, help, z, g);
	  break;	
  case  9:rev = !parity; 
  case  6:add_face (surface, a, b, a, c, c, d, rev, help, z, g);
	  add_face (surface, a, b, c, d, b, d, rev, help, z, g);
	  break;
  case  7:rev = !parity;
  case  8:add_face (surface, a, d, b, d, c, d, rev, help, z, g);
    break; 
  }
}

static void analyze_tetrahedra_bcl (const tetra_vertex_t * a, 
				    const tetra_vertex_t * b, 
				    const tetra_vertex_t * c, 
				    const tetra_vertex_t * d, 
				    GtsSurface * surface, 
				    helper_bcl * help, 
				    gint z, GtsCartesianGrid * g)
{
  gint rev = 0;
  gint code = 0; 
	
  if (a->d >= 0.) code |= 1; 
  if (b->d >= 0.) code |= 2; 
  if (c->d >= 0.) code |= 4; 
  if (d->d >= 0.) code |= 8;

  switch (code) {
  case 15:
  case 0: return; /* all inside or outside */		

  case 14:rev = !rev;
  case  1:add_face_bcl (surface, a, b, a, d, a, c, rev, help, z, g);
	  break;
  case 13:rev = !rev;  
  case  2:add_face_bcl (surface, a, b, b, c, b, d, rev, help, z, g);
	  break;
  case 12:rev = !rev;	  
  case  3:add_face_bcl (surface, a, d, a, c, b, c, rev, help, z, g);
	  add_face_bcl (surface, a, d, b, c, b, d, rev, help, z, g);
	  break;
  case 11:rev = !rev;	  
  case  4:add_face_bcl (surface, a, c, c, d, b, c, rev, help, z, g);
	  break;
  case 10:rev = !rev; 	  
  case 5: add_face_bcl (surface, a, b, a, d, c, d, rev, help, z, g);
	  add_face_bcl (surface, a, b, c, d, b, c, rev, help, z, g);
	  break;	
  case  9:rev = !rev; 
  case  6:add_face_bcl (surface, a, b, a, c, c, d, rev, help, z, g);
	  add_face_bcl (surface, a, b, c, d, b, d, rev, help, z, g);
	  break;
  case  7:rev = !rev;
  case  8:add_face_bcl (surface, a, d, b, d, c, d, rev, help, z, g);
    break;
  }
}

static void  iso_slice_evaluate (slice_t * s1, slice_t * s2, 
				 GtsCartesianGrid g, 
				 gint z, GtsSurface * surface, helper_t * help)
{
  gint x,y; 
  tetra_vertex_t v0, v1, v2, v3, v4, v5, v6, v7; 
  gdouble ** s1p = s1->data; 
  gdouble ** s2p = s2->data; 
	
  for (y = 0; y < g.ny-1; y++)
    for (x = 0; x < g.nx-1; x++) {
      gint parity = (((x ^ y) ^ z) & 1);
      
      v0.x = x  ; v0.y = y  ; v0.z = z  ; v0.mid = FALSE; v0.d = s1p[x  ][y  ];
      v1.x = x  ; v1.y = y+1; v1.z = z  ; v1.mid = FALSE; v1.d = s1p[x  ][y+1];
      v2.x = x+1; v2.y = y  ; v2.z = z  ; v2.mid = FALSE; v2.d = s1p[x+1][y  ];
      v3.x = x+1; v3.y = y+1; v3.z = z  ; v3.mid = FALSE; v3.d = s1p[x+1][y+1];
      v4.x = x  ; v4.y = y  ; v4.z = z+1; v4.mid = FALSE; v4.d = s2p[x  ][y  ];
      v5.x = x  ; v5.y = y+1; v5.z = z+1; v5.mid = FALSE; v5.d = s2p[x  ][y+1];
      v6.x = x+1; v6.y = y  ; v6.z = z+1; v6.mid = FALSE; v6.d = s2p[x+1][y  ];
      v7.x = x+1; v7.y = y+1; v7.z = z+1; v7.mid = FALSE; v7.d = s2p[x+1][y+1];
      
      if (parity == 0) {
	analyze_tetrahedra (&v0, &v1, &v2, &v4, parity, surface, help, z, &g);
	analyze_tetrahedra (&v7, &v1, &v4, &v2, parity, surface, help, z, &g);
	analyze_tetrahedra (&v1, &v7, &v3, &v2, parity, surface, help, z, &g);
	analyze_tetrahedra (&v1, &v7, &v4, &v5, parity, surface, help, z, &g);
	analyze_tetrahedra (&v2, &v6, &v4, &v7, parity, surface, help, z, &g);
      }else{
	analyze_tetrahedra (&v4, &v5, &v6, &v0, parity, surface, help, z, &g);
	analyze_tetrahedra (&v3, &v5, &v0, &v6, parity, surface, help, z, &g);
	analyze_tetrahedra (&v5, &v3, &v7, &v6, parity, surface, help, z, &g);
	analyze_tetrahedra (&v5, &v3, &v0, &v1, parity, surface, help, z, &g);
	analyze_tetrahedra (&v6, &v2, &v0, &v3, parity, surface, help, z, &g);
      }
    }
}

static void  iso_slice_evaluate_bcl (slice_t * s1, slice_t * s2, slice_t * s3,
				     GtsCartesianGrid g, 
				     gint z, GtsSurface * surface, 
				     helper_bcl * help)
{
  gint x,y; 
  tetra_vertex_t v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, w0; 
  gdouble ** s1p = s1->data;
  gdouble ** s2p = s2->data;
  gdouble ** s3p = s3->data;
	
  for (y = 0; y < g.ny-2; y++)
    for (x = 0; x < g.nx-2; x++) {
      v0.x = x  ; v0.y = y  ; v0.z = z  ; v0.mid = TRUE;
      v0.d = (s1p[x  ][y  ] + s2p[x  ][y  ] +
	      s1p[x  ][y+1] + s2p[x  ][y+1] +
	      s1p[x+1][y  ] + s2p[x+1][y  ] +
	      s1p[x+1][y+1] + s2p[x+1][y+1])/8.0;

      v1.x = x+1; v1.y = y  ; v1.z = z  ; v1.mid = TRUE;
      v1.d = (s1p[x+1][y  ] + s2p[x+1][y  ] +
	      s1p[x+1][y+1] + s2p[x+1][y+1] +
	      s1p[x+2][y  ] + s2p[x+2][y  ] +
	      s1p[x+2][y+1] + s2p[x+2][y+1])/8.0;

      v2.x = x  ; v2.y = y+1; v2.z = z  ; v2.mid = TRUE;
      v2.d = (s1p[x  ][y+1] + s2p[x  ][y+1] +
	      s1p[x  ][y+2] + s2p[x  ][y+2] +
	      s1p[x+1][y+1] + s2p[x+1][y+1] +
	      s1p[x+1][y+2] + s2p[x+1][y+2])/8.0;

      v3.x = x  ; v3.y = y  ; v3.z = z+1; v3.mid = TRUE;
      v3.d = (s2p[x  ][y  ] + s3p[x  ][y  ] +
	      s2p[x  ][y+1] + s3p[x  ][y+1] +
	      s2p[x+1][y  ] + s3p[x+1][y  ] +
	      s2p[x+1][y+1] + s3p[x+1][y+1])/8.0;

      v4.x = x+1; v4.y = y  ; v4.z = z  ; v4.mid = FALSE; v4.d = s1p[x+1][y  ];
      v5.x = x  ; v5.y = y+1; v5.z = z  ; v5.mid = FALSE; v5.d = s1p[x  ][y+1];
      v6.x = x+1; v6.y = y+1; v6.z = z  ; v6.mid = FALSE; v6.d = s1p[x+1][y+1];
      v7.x = x+1; v7.y = y  ; v7.z = z+1; v7.mid = FALSE; v7.d = s2p[x+1][y  ];
      v8.x = x  ; v8.y = y+1; v8.z = z+1; v8.mid = FALSE; v8.d = s2p[x  ][y+1];
      v9.x = x+1; v9.y = y+1; v9.z = z+1; v9.mid = FALSE; v9.d = s2p[x+1][y+1];
      w0.x = x  ; w0.y = y  ; w0.z = z+1; w0.mid = FALSE; w0.d = s2p[x  ][y  ];

      analyze_tetrahedra_bcl (&v0, &v9, &v6, &v1, surface, help, z, &g);
      analyze_tetrahedra_bcl (&v0, &v6, &v4, &v1, surface, help, z, &g);
      analyze_tetrahedra_bcl (&v0, &v4, &v7, &v1, surface, help, z, &g);
      analyze_tetrahedra_bcl (&v0, &v7, &v9, &v1, surface, help, z, &g);
      analyze_tetrahedra_bcl (&v0, &v5, &v6, &v2, surface, help, z, &g);
      analyze_tetrahedra_bcl (&v0, &v6, &v9, &v2, surface, help, z, &g);
      analyze_tetrahedra_bcl (&v0, &v9, &v8, &v2, surface, help, z, &g);
      analyze_tetrahedra_bcl (&v0, &v8, &v5, &v2, surface, help, z, &g);
      analyze_tetrahedra_bcl (&v0, &v8, &v9, &v3, surface, help, z, &g);
      analyze_tetrahedra_bcl (&v0, &v9, &v7, &v3, surface, help, z, &g);
      analyze_tetrahedra_bcl (&v0, &v7, &w0, &v3, surface, help, z, &g);
      analyze_tetrahedra_bcl (&v0, &w0, &v8, &v3, surface, help, z, &g);
    }
}

/*  copy src into dest by stripping off the iso value and leave out
    the boundary (which should be G_MINDOUBLE) */
static void copy_to_bounded (slice_t * dest, slice_t * src, 
			     gdouble iso, gdouble fill)
{
  gint x,y; 
  gdouble * src_ptr;
  gdouble * dest_ptr = dest->data[0];
  
  g_assert(dest->ny == src->ny + 2);
  g_assert(dest->nx == src->nx + 2);
	
  for (y = 0; y < dest->ny; ++y, ++dest_ptr)
    *dest_ptr = fill; 
	
  for (x = 1; x < src->nx - 1; ++x) {
    dest_ptr = dest->data[x];
    src_ptr = src->data[x-1];
    *dest_ptr++ = fill;
    for (y = 0; y < src->ny; ++y, ++dest_ptr, ++src_ptr)
      *dest_ptr  = *src_ptr - iso;
    *dest_ptr++ = fill; 
  }
  
  dest_ptr = dest->data[y];
  
  for (y = 0; y < dest->ny; ++y, ++dest_ptr)
    *dest_ptr = fill; 
}

static void iso_sub (slice_t * s, gdouble iso)
{
  gint x,y; 

  for (x = 0; x < s->nx; ++x) {
    gdouble *ptr = s->data[x];

    for (y = 0; y < s->ny; ++y, ++ptr)
      *ptr -= iso; 
  }
}


/**
 * gts_isosurface_tetra_bounded:
 * @surface: a #GtsSurface.
 * @g: a #GtsCartesianGrid.
 * @f: a #GtsIsoCartesianFunc.
 * @data: user data to be passed to @f.
 * @iso: isosurface value.
 *
 * Adds to @surface new faces defining the isosurface f(x,y,z) =
 * @iso. By convention, the normals to the surface are pointing toward
 * the positive values of f(x,y,z) - @iso. To ensure a closed object,
 * a boundary of G_MINDOUBLE is added around the domain
 *
 * The user function @f is called successively for each value of the z
 * coordinate defined by @g. It must fill the corresponding (x,y)
 * plane with the values of the function for which the isosurface is
 * to be computed.  
 */
void gts_isosurface_tetra_bounded (GtsSurface * surface,
				   GtsCartesianGrid g,
				   GtsIsoCartesianFunc f,
				   gpointer data,
				   gdouble iso)
{
  slice_t *slice1, *slice2, *transfer_slice; 
  GtsCartesianGrid g_intern = g; 
  helper_t *helper;
  gint z; 
	
  g_return_if_fail (surface != NULL);
  g_return_if_fail (f != NULL);
  g_return_if_fail (g.nx > 1);
  g_return_if_fail (g.ny > 1);
  g_return_if_fail (g.nz > 1);

  /* create the helper slices */
  slice1 = new_slice (g.nx + 2, g.ny + 2);
  slice2 = new_slice (g.nx + 2, g.ny + 2);
	
  /*  initialize the first slice as OUTSIDE */
  slice_init (slice1, -1.0);
	
  /* create a slice of the original image size */
  transfer_slice = new_slice (g.nx, g.ny);
	
  /* adapt the parameters to our enlarged image */
  g_intern.x -= g.dx;
  g_intern.y -= g.dy; 
  g_intern.z -= g.dz;
  g_intern.nx = g.nx + 2;
  g_intern.ny = g.ny + 2; 	
  g_intern.nz = g.nz;
	
  /* create the helper for vertex-lookup */
  helper = init_helper (g_intern.nx, g_intern.ny);
	
  /* go slicewise through the data */
  z = 0; 
  while (z < g.nz) {
    slice_t * hs; 
    
    /* request slice */
    f (transfer_slice->data, g, z, data);
    g.z += g.dz; 
    
    /* copy slice in enlarged image and mark the border as OUTSIDE */
    copy_to_bounded (slice2, transfer_slice, iso, -1.);
    
    /* triangulate */
    iso_slice_evaluate (slice1, slice2, g_intern, z, surface, helper);
    
    /* switch the input slices */
    hs = slice1; slice1 = slice2; slice2 = hs; 
    
    /* switch the vertex lookup tables */
    helper_advance(helper);
    ++z; 
  }
  
  /* initialize the last slice as OUTSIDE */
  slice_init (slice2, - 1.0);
		
  /* close the object */
  iso_slice_evaluate(slice1, slice2, g_intern, z, surface, helper);
  
  free_helper (helper);
  free_slice (slice1);
  free_slice (slice2);
  free_slice (transfer_slice);	
}

/**
 * gts_isosurface_tetra:
 * @surface: a #GtsSurface.
 * @g: a #GtsCartesianGrid.
 * @f: a #GtsIsoCartesianFunc.
 * @data: user data to be passed to @f.
 * @iso: isosurface value.
 *
 * Adds to @surface new faces defining the isosurface f(x,y,z) =
 * @iso. By convention, the normals to the surface are pointing toward
 * the positive values of f(x,y,z) - @iso.
 *
 * The user function @f is called successively for each value of the z
 * coordinate defined by @g. It must fill the corresponding (x,y)
 * plane with the values of the function for which the isosurface is
 * to be computed.  
 */
void gts_isosurface_tetra (GtsSurface * surface,
			   GtsCartesianGrid g,
			   GtsIsoCartesianFunc f,
			   gpointer data,
			   gdouble iso)
{
  slice_t *slice1, *slice2; 
  helper_t *helper;
  gint z; 
  GtsCartesianGrid g_internal;
  
  g_return_if_fail (surface != NULL);
  g_return_if_fail (f != NULL);
  g_return_if_fail (g.nx > 1);
  g_return_if_fail (g.ny > 1);
  g_return_if_fail (g.nz > 1);

  memcpy (&g_internal, &g, sizeof (GtsCartesianGrid));
	
  /* create the helper slices */
  slice1 = new_slice (g.nx, g.ny);
  slice2 = new_slice (g.nx, g.ny);
  
  /* create the helper for vertex-lookup */
  helper = init_helper (g.nx, g.ny);
	
  z = 0;
  f (slice1->data, g, z, data);
  iso_sub (slice1, iso); 
  
  z++; 
  g.z += g.dz;
  
  /* go slicewise through the data */
  while (z < g.nz) {
    slice_t * hs; 
    
    /* request slice */
    f (slice2->data, g, z, data);
    iso_sub (slice2, iso);
     
    g.z += g.dz;
    
    /* triangulate */
    iso_slice_evaluate (slice1, slice2, g_internal, z-1, surface, helper);
    
    /* switch the input slices */
    hs = slice1; slice1 = slice2; slice2 = hs; 
    
    /* switch the vertex lookup tables */
    helper_advance (helper);
    
    ++z; 
  }

  free_helper(helper);
  free_slice(slice1);
  free_slice(slice2);	
}

/**
 * gts_isosurface_tetra_bcl:
 * @surface: a #GtsSurface.
 * @g: a #GtsCartesianGrid.
 * @f: a #GtsIsoCartesianFunc.
 * @data: user data to be passed to @f.
 * @iso: isosurface value.
 *
 * Adds to @surface new faces defining the isosurface f(x,y,z) =
 * @iso. By convention, the normals to the surface are pointing toward
 * the positive values of f(x,y,z) - @iso.
 *
 * The user function @f is called successively for each value of the z
 * coordinate defined by @g. It must fill the corresponding (x,y)
 * plane with the values of the function for which the isosurface is
 * to be computed.  
 *
 * This version produces the dual "body-centered" faces relative to
 * the faces produced by gts_isosurface_tetra().
 */
void gts_isosurface_tetra_bcl (GtsSurface * surface,
			       GtsCartesianGrid g,
			       GtsIsoCartesianFunc f,
			       gpointer data,
			       gdouble iso)
{
  slice_t *slice1, *slice2, *slice3;
  helper_bcl *helper;
  gint z; 
  GtsCartesianGrid g_internal;
  
  g_return_if_fail (surface != NULL);
  g_return_if_fail (f != NULL);
  g_return_if_fail (g.nx > 1);
  g_return_if_fail (g.ny > 1);
  g_return_if_fail (g.nz > 1);

  memcpy (&g_internal, &g, sizeof (GtsCartesianGrid));
	
  /* create the helper slices */
  slice1 = new_slice (g.nx, g.ny);
  slice2 = new_slice (g.nx, g.ny);
  slice3 = new_slice (g.nx, g.ny);
  
  /* create the helper for vertex-lookup */
  helper = init_helper_bcl ();
	
  z = 0;
  f (slice1->data, g, z, data);
  iso_sub (slice1, iso); 

  z++; 
  g.z += g.dz;

  f (slice2->data, g, z, data);
  iso_sub (slice1, iso); 
  
  z++; 
  g.z += g.dz;
  
  /* go slicewise through the data */
  while (z < g.nz) {
    slice_t * hs; 
    
    /* request slice */
    f (slice3->data, g, z, data);
    iso_sub (slice3, iso);
     
    g.z += g.dz;
    
    /* triangulate */
    iso_slice_evaluate_bcl (slice1, slice2, slice3, g_internal, z-2, 
			    surface, helper);
    
    /* switch the input slices */
    hs = slice1; slice1 = slice2; slice2 = slice3; slice3 = hs;
    
    /* switch the vertex lookup tables */
    helper_advance_bcl (helper);
    
    ++z; 
  }

  free_helper_bcl(helper);
  free_slice(slice1);
  free_slice(slice2);	
  free_slice(slice3);	
}
