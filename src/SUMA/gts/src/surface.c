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
#include <string.h>
#include "gts.h"

#include "gts-private.h"

static void destroy_foreach_face (GtsFace * f, GtsSurface * s)
{
  f->surfaces = g_slist_remove (f->surfaces, s);
  if (!GTS_OBJECT_DESTROYED (f) &&
      !gts_allow_floating_faces && f->surfaces == NULL)
    gts_object_destroy (GTS_OBJECT (f));
}

static void surface_destroy (GtsObject * object)
{
  GtsSurface * surface = GTS_SURFACE (object);
  
  gts_surface_foreach_face (surface, (GtsFunc) destroy_foreach_face, surface);
#ifdef USE_SURFACE_BTREE
  g_tree_destroy (surface->faces);
#else /* not USE_SURFACE_BTREE */
  g_hash_table_destroy (surface->faces);
#endif /* not USE_SURFACE_BTREE */

  (* GTS_OBJECT_CLASS (gts_surface_class ())->parent_class->destroy) (object);
}

static void surface_write (GtsObject * object, FILE * fptr)
{
  fprintf (fptr, " %s %s %s %s", 
	   object->klass->info.name,
	   GTS_OBJECT_CLASS (GTS_SURFACE (object)->face_class)->info.name,
	   GTS_OBJECT_CLASS (GTS_SURFACE (object)->edge_class)->info.name,
	   GTS_POINT_CLASS (GTS_SURFACE (object)->vertex_class)->binary ?
	   "GtsVertexBinary" :
	   GTS_OBJECT_CLASS (GTS_SURFACE (object)->vertex_class)->info.name);
}

static void surface_class_init (GtsSurfaceClass * klass)
{
  GTS_OBJECT_CLASS (klass)->destroy = surface_destroy;
  GTS_OBJECT_CLASS (klass)->write = surface_write;
  klass->add_face = NULL;
  klass->remove_face = NULL;
}

#ifdef USE_SURFACE_BTREE
static gint compare_pointers (gconstpointer a, gconstpointer b)
{
  if (GPOINTER_TO_UINT (a) < GPOINTER_TO_UINT (b))
    return -1;
  if (GPOINTER_TO_UINT (a) > GPOINTER_TO_UINT (b))
    return 1;
  return 0;
}
#endif /* USE_SURFACE_BTREE */

static void surface_init (GtsSurface * surface)
{
#ifdef USE_SURFACE_BTREE
  surface->faces = g_tree_new (compare_pointers);
#else /* not USE_SURFACE_BTREE */
  surface->faces = g_hash_table_new (NULL, NULL);
#endif /* not USE_SURFACE_BTREE */
  surface->vertex_class = gts_vertex_class ();
  surface->edge_class = gts_edge_class ();
  surface->face_class = gts_face_class ();
  surface->keep_faces = FALSE;
}

/**
 * gts_surface_class:
 *
 * Returns: the #GtsSurfaceClass.
 */
GtsSurfaceClass * gts_surface_class (void)
{
  static GtsSurfaceClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo surface_info = {
      "GtsSurface",
      sizeof (GtsSurface),
      sizeof (GtsSurfaceClass),
      (GtsObjectClassInitFunc) surface_class_init,
      (GtsObjectInitFunc) surface_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (), &surface_info);
  }

  return klass;
}

/**
 * gts_surface_new:
 * @klass: a #GtsSurfaceClass.
 * @face_class: a #GtsFaceClass.
 * @edge_class: a #GtsEdgeClass.
 * @vertex_class: a #GtsVertexClass.
 *
 * Returns: a new empty #GtsSurface.
 */
GtsSurface * gts_surface_new (GtsSurfaceClass * klass,
			      GtsFaceClass * face_class,
			      GtsEdgeClass * edge_class,
			      GtsVertexClass * vertex_class)
{
  GtsSurface * s;

  s = GTS_SURFACE (gts_object_new (GTS_OBJECT_CLASS (klass)));
  s->vertex_class = vertex_class;
  s->edge_class = edge_class;
  s->face_class = face_class;

  return s;
}

/**
 * gts_surface_add_face:
 * @s: a #GtsSurface.
 * @f: a #GtsFace.
 *
 * Adds face @f to surface @s.
 */
void gts_surface_add_face (GtsSurface * s, GtsFace * f)
{
  g_return_if_fail (s != NULL);
  g_return_if_fail (f != NULL);

  g_assert (s->keep_faces == FALSE);

#ifdef USE_SURFACE_BTREE
  if (!g_tree_lookup (s->faces, f)) {
    f->surfaces = g_slist_prepend (f->surfaces, s);
    g_tree_insert (s->faces, f, f);
  }
#else /* not USE_SURFACE_BTREE */
  if (!g_hash_table_lookup (s->faces, f)) {
    f->surfaces = g_slist_prepend (f->surfaces, s);
    g_hash_table_insert (s->faces, f, f);
  }
#endif /* not USE_SURFACE_BTREE */

  if (GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass)->add_face)
    (* GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass)->add_face) (s, f);
}

/**
 * gts_surface_remove_face:
 * @s: a #GtsSurface.
 * @f: a #GtsFace.
 *
 * Removes face @f from surface @s.
 */
void gts_surface_remove_face (GtsSurface * s, 
			      GtsFace * f)
{
  g_return_if_fail (s != NULL);
  g_return_if_fail (f != NULL);

  g_assert (s->keep_faces == FALSE);

#ifdef USE_SURFACE_BTREE
  g_tree_remove (s->faces, f);
#else /* not USE_SURFACE_BTREE */
  g_hash_table_remove (s->faces, f);
#endif /* not USE_SURFACE_BTREE */

  f->surfaces = g_slist_remove (f->surfaces, s);

  if (GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass)->remove_face)
    (* GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass)->remove_face) (s, f);

  if (!GTS_OBJECT_DESTROYED (f) &&
      !gts_allow_floating_faces && 
      f->surfaces == NULL)
    gts_object_destroy (GTS_OBJECT (f));
}

/**
 * gts_surface_read:
 * @surface: a #GtsSurface.
 * @f: a #GtsFile.
 *
 * Add to @surface the data read from @f. The format of the file pointed to
 * by @f is as described in gts_surface_write().
 *
 * Returns: 0 if successful or the line number at which the parsing
 * stopped in case of error (in which case the @error field of @f is
 * set to a description of the error which occured).  
 */
/* Update split.c/surface_read() if modifying this function */
guint gts_surface_read (GtsSurface * surface, GtsFile * f)
{
  GtsVertex ** vertices;
  GtsEdge ** edges;
  guint n, nv, ne, nf;

  g_return_val_if_fail (surface != NULL, 1);
  g_return_val_if_fail (f != NULL, 1);

  if (f->type != GTS_INT) {
    gts_file_error (f, "expecting an integer (number of vertices)");
    return f->line;
  }
  nv = atoi (f->token->str);

  gts_file_next_token (f);
  if (f->type != GTS_INT) {
    gts_file_error (f, "expecting an integer (number of edges)");
    return f->line;
  }
  ne = atoi (f->token->str);

  gts_file_next_token (f);
  if (f->type != GTS_INT) {
    gts_file_error (f, "expecting an integer (number of faces)");
    return f->line;
  }
  nf = atoi (f->token->str);
  
  gts_file_next_token (f);
  if (f->type == GTS_STRING) {
    if (f->type != GTS_STRING) {
      gts_file_error (f, "expecting a string (GtsSurfaceClass)");
      return f->line;
    }
    gts_file_next_token (f);
    if (f->type != GTS_STRING) {
      gts_file_error (f, "expecting a string (GtsFaceClass)");
      return f->line;
    }
    gts_file_next_token (f);
    if (f->type != GTS_STRING) {
      gts_file_error (f, "expecting a string (GtsEdgeClass)");
      return f->line;
    }
    gts_file_next_token (f);
    if (f->type != GTS_STRING) {
      gts_file_error (f, "expecting a string (GtsVertexClass)");
      return f->line;
    }
    if (!strcmp (f->token->str, "GtsVertexBinary"))
      GTS_POINT_CLASS (surface->vertex_class)->binary = TRUE;
    else
      gts_file_first_token_after (f, '\n');
  }
  else
    gts_file_first_token_after (f, '\n');

  if (nf <= 0)
    return 0;

  /* allocate nv + 1 just in case nv == 0 */
  vertices = g_malloc ((nv + 1)*sizeof (GtsVertex *));
  edges = g_malloc ((ne + 1)*sizeof (GtsEdge *));
  
  n = 0;
  while (n < nv && f->type != GTS_ERROR) {
    GtsObject * new_vertex =
      gts_object_new (GTS_OBJECT_CLASS (surface->vertex_class));

    (* GTS_OBJECT_CLASS (surface->vertex_class)->read) (&new_vertex, f);
    if (f->type != GTS_ERROR) {
      if (!GTS_POINT_CLASS (surface->vertex_class)->binary)
	gts_file_first_token_after (f, '\n');
      vertices[n++] = GTS_VERTEX (new_vertex);
    }
    else
      gts_object_destroy (new_vertex);
  }
  if (f->type == GTS_ERROR)
    nv = n;
  if (GTS_POINT_CLASS (surface->vertex_class)->binary)
    gts_file_first_token_after (f, '\n');

  n = 0;
  while (n < ne && f->type != GTS_ERROR) {
    guint p1, p2;

    if (f->type != GTS_INT)
      gts_file_error (f, "expecting an integer (first vertex index)");
    else {
      p1 = atoi (f->token->str);
      if (p1 == 0 || p1 > nv)
	gts_file_error (f, "vertex index `%d' is out of range `[1,%d]'", 
			p1, nv);
      else {
	gts_file_next_token (f);
	if (f->type != GTS_INT)
	  gts_file_error (f, "expecting an integer (second vertex index)");
	else {
	  p2 = atoi (f->token->str);
	  if (p2 == 0 || p2 > nv)
	    gts_file_error (f, "vertex index `%d' is out of range `[1,%d]'", 
			    p2, nv);
	  else {
	    GtsEdge * new_edge =
	      gts_edge_new (surface->edge_class,
			    vertices[p1 - 1], vertices[p2 - 1]);

	    gts_file_next_token (f);
	    if (f->type != '\n')
	      if (GTS_OBJECT_CLASS (surface->edge_class)->read)
		(*GTS_OBJECT_CLASS (surface->edge_class)->read)
		  ((GtsObject **) &new_edge, f);
	    gts_file_first_token_after (f, '\n');
	    edges[n++] = new_edge;
	  }
	}
      }
    }
  }
  if (f->type == GTS_ERROR)
    ne = n;

  n = 0;
  while (n < nf && f->type != GTS_ERROR) {
    guint s1, s2, s3;

    if (f->type != GTS_INT)
      gts_file_error (f, "expecting an integer (first edge index)");
    else {
      s1 = atoi (f->token->str);
      if (s1 == 0 || s1 > ne)
	gts_file_error (f, "edge index `%d' is out of range `[1,%d]'", 
			s1, ne);
      else {
	gts_file_next_token (f);
	if (f->type != GTS_INT)
	  gts_file_error (f, "expecting an integer (second edge index)");
	else {
	  s2 = atoi (f->token->str);
	  if (s2 == 0 || s2 > ne)
	    gts_file_error (f, "edge index `%d' is out of range `[1,%d]'", 
			    s2, ne);
	  else {
	    gts_file_next_token (f);
	    if (f->type != GTS_INT)
	      gts_file_error (f, "expecting an integer (third edge index)");
	    else {
	      s3 = atoi (f->token->str);
	      if (s3 == 0 || s3 > ne)
		gts_file_error (f, "edge index `%d' is out of range `[1,%d]'", 
				s3, ne);
	      else {
		GtsFace * new_face = gts_face_new (surface->face_class,
						   edges[s1 - 1],
						   edges[s2 - 1],
						   edges[s3 - 1]);

		gts_file_next_token (f);
		if (f->type != '\n')
		  if (GTS_OBJECT_CLASS (surface->face_class)->read)
		    (*GTS_OBJECT_CLASS (surface->face_class)->read)
		      ((GtsObject **) &new_face, f);
		gts_file_first_token_after (f, '\n');
		gts_surface_add_face (surface, new_face);
		n++;
	      }
	    }
	  }
	}
      }
    }
  }

  if (f->type == GTS_ERROR) {
    gts_allow_floating_vertices = TRUE;
    while (nv)
      gts_object_destroy (GTS_OBJECT (vertices[nv-- - 1]));
    gts_allow_floating_vertices = FALSE;
  }

  g_free (vertices);
  g_free (edges);

  if (f->type == GTS_ERROR)
    return f->line;
  return 0;
}

static void sum_area (GtsFace * f, gdouble * area) {
  *area += gts_triangle_area (GTS_TRIANGLE (f));
}

/**
 * gts_surface_area:
 * @s: a #GtsSurface.
 *
 * Returns: the area of @s obtained as the sum of the signed areas of its
 * faces.
 */
gdouble gts_surface_area (GtsSurface * s)
{  
  gdouble area = 0.0;
  gts_surface_foreach_face (s, (GtsFunc)sum_area, &area);
  return area;
}

/**
 * gts_range_init:
 * @r: a #GtsRange.
 *
 * Initializes a #GtsRange.
 */
void gts_range_init (GtsRange * r)
{
  g_return_if_fail (r != NULL);

  r->max = - G_MAXDOUBLE;
  r->min = G_MAXDOUBLE;
  r->sum = r->sum2 = 0.0;
  r->n = 0;
}

/**
 * gts_range_reset:
 * @r: a #GtsRange.
 *
 * Sets all the fields of @r to 0.
 */
void gts_range_reset (GtsRange * r)
{
  g_return_if_fail (r != NULL);

  r->max = 0.0;
  r->min = 0.0;
  r->sum = r->sum2 = 0.0;
  r->n = 0;
}

/**
 * gts_range_add_value:
 * @r: a #GtsRange.
 * @val: a value to add to @r.
 *
 * Adds @val to @r.
 */
void gts_range_add_value (GtsRange * r, gdouble val)
{
  g_return_if_fail (r != NULL);

  if (val < r->min) r->min = val;
  if (val > r->max) r->max = val;
  r->sum += val;
  r->sum2 += val*val;
  r->n++;
}

/**
 * gts_range_update:
 * @r: a #GtsRange.
 * 
 * Updates the fields of @r.
 */
void gts_range_update (GtsRange * r)
{
  g_return_if_fail (r != NULL);

  if (r->n > 0) {
    if (r->sum2 - r->sum*r->sum/(gdouble) r->n >= 0.)
      r->stddev = sqrt ((r->sum2 - r->sum*r->sum/(gdouble) r->n)
			/(gdouble) r->n);
    else
      r->stddev = 0.;
    r->mean = r->sum/(gdouble) r->n;
  }
  else 
    r->min = r->max = r->mean = r->stddev = 0.;
}

/**
 * gts_range_print:
 * @r: a #GtsRange.
 * @fptr: a file pointer.
 * 
 * Writes a text representation of @r in @fptr.
 */
void gts_range_print (GtsRange * r, FILE * fptr)
{
  g_return_if_fail (r != NULL);
  g_return_if_fail (fptr != NULL);
  fprintf (fptr, "min: %g mean: %g | %g max: %g", 
	   r->min, r->mean, r->stddev, r->max);
}

static void stats_foreach_vertex (GtsVertex * v, GtsSurfaceStats * stats) 
{
  GSList * i = v->segments;
  guint nedges = 0;

  while (i) {
    if (GTS_IS_EDGE (i->data) && 
	gts_edge_has_parent_surface (i->data, stats->parent))
      nedges++;
    i = i->next;
  }
  gts_range_add_value (&stats->edges_per_vertex, nedges);
}

static void stats_foreach_edge (GtsEdge * e, GtsSurfaceStats * stats) 
{
  guint nt = gts_edge_face_number (e, stats->parent);

  if (gts_segment_is_duplicate (GTS_SEGMENT (e)))
    stats->n_duplicate_edges++;
  if (nt == 1)
    stats->n_boundary_edges++;
  else if (nt > 2)
    stats->n_non_manifold_edges++;
  gts_range_add_value (&stats->faces_per_edge, nt);
}

static void stats_foreach_face (GtsTriangle * t, GtsSurfaceStats * stats)
{
  GSList * i;
  gboolean incompatible = FALSE;

  i = t->e1->triangles;
  while (i && !incompatible) {
    if (i->data != t &&
	GTS_IS_FACE (i->data) &&
	gts_face_has_parent_surface (i->data, stats->parent) &&
	!gts_triangles_are_compatible (t, i->data, t->e1))
      incompatible = TRUE;
    i = i->next;
  }
  i = t->e2->triangles;
  while (i && !incompatible) {
    if (i->data != t &&
	GTS_IS_FACE (i->data) &&
	gts_face_has_parent_surface (i->data, stats->parent) &&
	!gts_triangles_are_compatible (t, i->data, t->e2))
      incompatible = TRUE;
    i = i->next;
  }
  i = t->e3->triangles;
  while (i && !incompatible) {
    if (i->data != t &&
	GTS_IS_FACE (i->data) &&
	gts_face_has_parent_surface (i->data, stats->parent) &&
	!gts_triangles_are_compatible (t, i->data, t->e3))
      incompatible = TRUE;
    i = i->next;
  }
  if (incompatible)
    stats->n_incompatible_faces++;
  if (gts_triangle_is_duplicate (t))
    stats->n_duplicate_faces++;
  stats->n_faces++;
}

/**
 * gts_surface_stats:
 * @s: a #GtsSurface.
 * @stats: a #GtsSurfaceStats.
 *
 * Fills @stats with the statistics relevant to surface @s.
 */
void gts_surface_stats (GtsSurface * s, GtsSurfaceStats * stats)
{
  g_return_if_fail (s != NULL);
  g_return_if_fail (stats != NULL);

  stats->parent = s;
  stats->n_faces = 0;
  stats->n_incompatible_faces = 0;
  stats->n_duplicate_faces = 0;
  stats->n_duplicate_edges = 0;
  stats->n_boundary_edges = 0;
  stats->n_non_manifold_edges = 0;
  gts_range_init (&stats->edges_per_vertex);
  gts_range_init (&stats->faces_per_edge);

  gts_surface_foreach_vertex (s, (GtsFunc) stats_foreach_vertex, stats);
  gts_surface_foreach_edge (s, (GtsFunc) stats_foreach_edge, stats);
  gts_surface_foreach_face (s, (GtsFunc) stats_foreach_face, stats);

  gts_range_update (&stats->edges_per_vertex);
  gts_range_update (&stats->faces_per_edge);
}

static void quality_foreach_edge (GtsSegment * s,
				  GtsSurfaceQualityStats * stats) 
{
  GSList * i = GTS_EDGE (s)->triangles;

  gts_range_add_value (&stats->edge_length, 
		   gts_point_distance (GTS_POINT (s->v1), 
				       GTS_POINT (s->v2)));
  while (i) {
    GSList * j = i->next;
    while (j) {
      gts_range_add_value (&stats->edge_angle,
			   fabs (gts_triangles_angle (i->data, j->data)));
      j = j->next;
    }
    i = i->next;
  }
}

static void quality_foreach_face (GtsTriangle * t,
				  GtsSurfaceQualityStats * stats) 
{
  gts_range_add_value (&stats->face_quality, gts_triangle_quality (t));
  gts_range_add_value (&stats->face_area, gts_triangle_area (t));
}

/**
 * gts_surface_quality_stats:
 * @s: a #GtsSurface.
 * @stats: a #GtsSurfaceQualityStats.
 *
 * Fills @stats with quality statistics relevant to surface @s.
 */
void gts_surface_quality_stats (GtsSurface * s, GtsSurfaceQualityStats * stats)
{
  g_return_if_fail (s != NULL);
  g_return_if_fail (stats != NULL);

  stats->parent = s;
  gts_range_init (&stats->face_quality);
  gts_range_init (&stats->face_area);
  gts_range_init (&stats->edge_length);
  gts_range_init (&stats->edge_angle);

  gts_surface_foreach_edge (s, (GtsFunc) quality_foreach_edge, stats);  
  gts_surface_foreach_face (s, (GtsFunc) quality_foreach_face, stats);

  gts_range_update (&stats->face_quality);
  gts_range_update (&stats->face_area);
  gts_range_update (&stats->edge_length);
  gts_range_update (&stats->edge_angle);
}

/**
 * gts_surface_print_stats:
 * @s: a #GtsSurface.
 * @fptr: a file pointer.
 *
 * Writes in the file pointed to by @fptr the statistics for surface @s.
 */
void gts_surface_print_stats (GtsSurface * s, FILE * fptr)
{
  GtsSurfaceStats stats;
  GtsSurfaceQualityStats qstats;

  g_return_if_fail (s != NULL);
  g_return_if_fail (fptr != NULL);

  gts_surface_stats (s, &stats);
  gts_surface_quality_stats (s, &qstats);

  fprintf (fptr, 
	   "# vertices: %u edges: %u faces: %u\n"
	   "# Connectivity statistics\n"
	   "#   incompatible faces: %u\n"
	   "#   duplicate faces: %u\n"
	   "#   boundary edges: %u\n"
	   "#   duplicate edges: %u\n"
	   "#   non-manifold edges: %u\n",
	   stats.edges_per_vertex.n, 
	   stats.faces_per_edge.n,
	   stats.n_faces,
	   stats.n_incompatible_faces,
	   stats.n_duplicate_faces,
	   stats.n_boundary_edges,
	   stats.n_duplicate_edges,
	   stats.n_non_manifold_edges);
  fputs ("#   edges per vertex: ", fptr); 
  gts_range_print (&stats.edges_per_vertex, fptr);
  fputs ("\n#   faces per edge: ", fptr);
  gts_range_print (&stats.faces_per_edge, fptr);
  fputs ("\n# Geometric statistics\n#   face quality: ", fptr);
  gts_range_print (&qstats.face_quality, fptr);
  fputs ("\n#   face area  : ", fptr);
  gts_range_print (&qstats.face_area, fptr);
  fputs ("\n#   edge length : ", fptr);
  gts_range_print (&qstats.edge_length, fptr);
  fputc ('\n', fptr);
}

static void write_vertex (GtsPoint * p, gpointer * data)
{
  (*GTS_OBJECT (p)->klass->write) (GTS_OBJECT (p), (FILE *) data[0]);
  if (!GTS_POINT_CLASS (GTS_OBJECT (p)->klass)->binary)
    fputc ('\n', (FILE *) data[0]);
  g_hash_table_insert (data[2], p, 
		       GUINT_TO_POINTER (++(*((guint *) data[1]))));
}

static void write_edge (GtsSegment * s, gpointer * data) 
{
  fprintf ((FILE *) data[0], "%u %u",
	   GPOINTER_TO_UINT (g_hash_table_lookup (data[2], s->v1)),
	   GPOINTER_TO_UINT (g_hash_table_lookup (data[2], s->v2)));
  if (GTS_OBJECT (s)->klass->write)
    (*GTS_OBJECT (s)->klass->write) (GTS_OBJECT (s), (FILE *) data[0]);
  fputc ('\n', (FILE *) data[0]);
  g_hash_table_insert (data[3], s, 
		       GUINT_TO_POINTER (++(*((guint *) data[1]))));
}

static void write_face (GtsTriangle * t, gpointer * data)
{
  fprintf (data[0], "%u %u %u",
	   GPOINTER_TO_UINT (g_hash_table_lookup (data[3], t->e1)),
	   GPOINTER_TO_UINT (g_hash_table_lookup (data[3], t->e2)),
	   GPOINTER_TO_UINT (g_hash_table_lookup (data[3], t->e3)));
  if (GTS_OBJECT (t)->klass->write)
    (*GTS_OBJECT (t)->klass->write) (GTS_OBJECT (t), data[0]);
  fputc ('\n', data[0]);
}

/**
 * gts_surface_write:
 * @s: a #GtsSurface.
 * @fptr: a file pointer.
 * 
 * Writes in the file @fptr an ASCII representation of @s. The file
 * format is as follows. 
 *
 * All the lines beginning with #GTS_COMMENTS are ignored. The first line
 * contains three unsigned integers separated by spaces. The first
 * integer is the number of vertices, nv, the second is the number of
 * edges, ne and the third is the number of faces, nf.
 *
 * Follows nv lines containing the x, y and z coordinates of the
 * vertices.  Follows ne lines containing the two indices (starting
 * from one) of the vertices of each edge. Follows nf lines containing
 * the three ordered indices (also starting from one) of the edges of
 * each face.  
 *
 * The format described above is the least common denominator to all
 * GTS files.  Consistent with an object-oriented approach, the GTS
 * file format is extensible. Each of the lines of the file can be
 * extended with user-specific attributes accessible through the
 * read() and write() virtual methods of each of the objects written
 * (surface, vertices, edges or faces). When read with different
 * object classes, these extra attributes are just ignored.  
 */
void gts_surface_write (GtsSurface * s, FILE * fptr)
{
  guint n;
  gpointer data[4];
  GHashTable * vindex, * eindex;
  GtsSurfaceStats stats;

  g_return_if_fail (s != NULL);
  g_return_if_fail (fptr != NULL);

  data[0] = fptr;
  data[1] = &n;
  data[2] = vindex = g_hash_table_new (NULL, NULL);
  data[3] = eindex = g_hash_table_new (NULL, NULL);

  gts_surface_stats (s, &stats);
  fprintf (fptr, "%u %u %u", 
	   stats.edges_per_vertex.n, 
	   stats.faces_per_edge.n, 
	   stats.n_faces);
  if (GTS_OBJECT (s)->klass->write)
    (*GTS_OBJECT (s)->klass->write) (GTS_OBJECT (s), fptr);
  fputc ('\n', fptr);
  n = 0;
  gts_surface_foreach_vertex (s, (GtsFunc) write_vertex, data);
  n = 0;
  if (GTS_POINT_CLASS (s->vertex_class)->binary)
    fputc ('\n', fptr);
  gts_surface_foreach_edge (s, (GtsFunc) write_edge, data);
  gts_surface_foreach_face (s, (GtsFunc) write_face, data);
  g_hash_table_destroy (vindex);
  g_hash_table_destroy (eindex);
}

static void write_vertex_oogl (GtsPoint * p, gpointer * data)
{
  FILE * fp = data[0];

  fprintf (fp, "%g %g %g", p->x, p->y, p->z);
  if (GTS_OBJECT (p)->klass->color) {
    GtsColor c = (* GTS_OBJECT (p)->klass->color) (GTS_OBJECT (p));
    fprintf (fp, " %g %g %g 1.0\n", c.r, c.g, c.b);
  }
  else
    fputc ('\n', fp);
  GTS_OBJECT (p)->reserved = GUINT_TO_POINTER ((*((guint *) data[1]))++);
}

static void write_face_oogl (GtsTriangle * t, FILE * fp)
{
  GtsVertex * v1, * v2, * v3;
  gts_triangle_vertices (t, &v1, &v2, &v3);
  fprintf (fp, "3 %u %u %u",
	   GPOINTER_TO_UINT (GTS_OBJECT (v1)->reserved),
	   GPOINTER_TO_UINT (GTS_OBJECT (v2)->reserved),
	   GPOINTER_TO_UINT (GTS_OBJECT (v3)->reserved));
  if (GTS_OBJECT (t)->klass->color) {
    GtsColor c = (* GTS_OBJECT (t)->klass->color) (GTS_OBJECT (t));
    fprintf (fp, " %g %g %g\n", c.r, c.g, c.b);
  }
  else
    fputc ('\n', fp);
}

/**
 * gts_surface_write_oogl:
 * @s: a #GtsSurface.
 * @fptr: a file pointer.
 * 
 * Writes in the file @fptr an OOGL (Geomview) representation of @s.
 */
void gts_surface_write_oogl (GtsSurface * s, FILE * fptr)
{
  guint n = 0;
  gpointer data[2];
  GtsSurfaceStats stats;

  g_return_if_fail (s != NULL);
  g_return_if_fail (fptr != NULL);

  data[0] = fptr;
  data[1] = &n;

  gts_surface_stats (s, &stats);
  if (GTS_OBJECT_CLASS (s->vertex_class)->color)
    fputs ("COFF ", fptr);
  else
    fputs ("OFF ", fptr);
  fprintf (fptr, "%u %u %u\n", 
	   stats.edges_per_vertex.n, 
	   stats.n_faces,
	   stats.faces_per_edge.n);
  gts_surface_foreach_vertex (s, (GtsFunc) write_vertex_oogl, data);
  gts_surface_foreach_face (s, (GtsFunc) write_face_oogl, fptr);
  gts_surface_foreach_vertex (s, (GtsFunc) gts_object_reset_reserved, NULL);
}

static void write_vertex_vtk (GtsPoint * p, gpointer * data)
{
  FILE * fp = data[0];

  fprintf (fp, "%g %g %g\n", p->x, p->y, p->z);
  GTS_OBJECT (p)->reserved = GUINT_TO_POINTER ((*((guint *) data[1]))++);
}

static void write_face_vtk (GtsTriangle * t, FILE * fp)
{
  GtsVertex * v1, * v2, * v3;
  gts_triangle_vertices (t, &v1, &v2, &v3);
  fprintf (fp, "3 %u %u %u\n",
	   GPOINTER_TO_UINT (GTS_OBJECT (v1)->reserved),
	   GPOINTER_TO_UINT (GTS_OBJECT (v2)->reserved),
	   GPOINTER_TO_UINT (GTS_OBJECT (v3)->reserved));
}

/**
 * gts_surface_write_vtk:
 * @s: a #GtsSurface.
 * @fptr: a file pointer.
 * 
 * Writes in the file @fptr a VTK representation of @s.
 */
void gts_surface_write_vtk (GtsSurface * s, FILE * fptr)
{
  guint n = 0;
  gpointer data[2];
  GtsSurfaceStats stats;

  g_return_if_fail (s != NULL);
  g_return_if_fail (fptr != NULL);

  data[0] = fptr;
  data[1] = &n;

  gts_surface_stats (s, &stats);
  fprintf (fptr,
	   "# vtk DataFile Version 2.0\n"
	   "Generated by GTS\n"
           "ASCII\n"
	   "DATASET POLYDATA\n"
	   "POINTS %u float\n",
	   stats.edges_per_vertex.n);
  gts_surface_foreach_vertex (s, (GtsFunc) write_vertex_vtk, data);
  fprintf (fptr,
	   "POLYGONS %u %u\n",
	   stats.n_faces, stats.n_faces*4);
  gts_surface_foreach_face (s, (GtsFunc) write_face_vtk, fptr);
  gts_surface_foreach_vertex (s, (GtsFunc) gts_object_reset_reserved, NULL);  
}

static void write_edge_oogl_boundary (GtsSegment * s, gpointer * data)
{
  if (!gts_edge_is_boundary (GTS_EDGE (s), data[1]))
    return;

  if (GTS_OBJECT (s)->klass->color) {
    GtsColor c = (* GTS_OBJECT (s)->klass->color) (GTS_OBJECT (s));
    fprintf (data[0], "VECT 1 2 1 2 1 %g %g %g %g %g %g %g %g %g 1.\n",
	     GTS_POINT (s->v1)->x, GTS_POINT (s->v1)->y, GTS_POINT (s->v1)->z,
	     GTS_POINT (s->v2)->x, GTS_POINT (s->v2)->y, GTS_POINT (s->v2)->z,
	     c.r, c.g, c.b);
  }
  else
    fprintf (data[0], "VECT 1 2 0 2 0 %g %g %g %g %g %g\n",
	     GTS_POINT (s->v1)->x, GTS_POINT (s->v1)->y, GTS_POINT (s->v1)->z,
	     GTS_POINT (s->v2)->x, GTS_POINT (s->v2)->y, GTS_POINT (s->v2)->z);
}

/**
 * gts_surface_write_oogl_boundary:
 * @s: a #GtsSurface.
 * @fptr: a file pointer.
 * 
 * Writes in the file @fptr an OOGL (Geomview) representation of the
 * boundary of @s.  
 */
void gts_surface_write_oogl_boundary (GtsSurface * s, FILE * fptr)
{
  gpointer data[2];

  g_return_if_fail (s != NULL);
  g_return_if_fail (fptr != NULL);

  data[0] = fptr;
  data[1] = s;
  fputs ("LIST {\n", fptr);
  gts_surface_foreach_edge (s, (GtsFunc) write_edge_oogl_boundary, data);
  fputs ("}\n", fptr);
}

#ifdef USE_SURFACE_BTREE
static gint vertex_foreach_face (GtsTriangle * t,
				 gpointer t_data,
				 gpointer * info)
#else /* not USE_SURFACE_BTREE */
static void vertex_foreach_face (GtsTriangle * t,
				 gpointer t_data,
				 gpointer * info)
#endif /* not USE_SURFACE_BTREE */
{
  GHashTable * hash = info[0];
  gpointer data = info[1];
  GtsFunc func = (GtsFunc) info[2];
  GtsSegment 
    * s1 = GTS_SEGMENT (t->e1);

  if (!g_hash_table_lookup (hash, s1->v1)) {
    (*func) (s1->v1, data);
    g_hash_table_insert (hash, s1->v1, GINT_TO_POINTER (-1));
  }
  if (!g_hash_table_lookup (hash, s1->v2)) {
    (*func) (s1->v2, data);
    g_hash_table_insert (hash, s1->v2, GINT_TO_POINTER (-1));
  }
  if (!g_hash_table_lookup (hash, gts_triangle_vertex (t))) {
    (*func) (gts_triangle_vertex (t), data);
    g_hash_table_insert (hash, gts_triangle_vertex (t), 
			 GINT_TO_POINTER (-1));
  }
#ifdef USE_SURFACE_BTREE
  return FALSE;
#endif /* USE_SURFACE_BTREE */
}

/**
 * gts_surface_foreach_vertex:
 * @s: a #GtsSurface.
 * @func: a #GtsFunc.
 * @data: user data to be passed to @func.
 *
 * Calls @func once for each vertex of @s.
 */
void gts_surface_foreach_vertex (GtsSurface * s, GtsFunc func, gpointer data)
{
  gpointer info[3];

  g_return_if_fail (s != NULL);
  g_return_if_fail (func != NULL);

  /* forbid removal of faces */
  s->keep_faces = TRUE;
  info[0] = g_hash_table_new (NULL, NULL);
  info[1] = data;
  info[2] = func;
#ifdef USE_SURFACE_BTREE
  g_tree_traverse (s->faces, (GTraverseFunc) vertex_foreach_face, G_IN_ORDER,
		   info);
#else /* not USE_SURFACE_BTREE */
  g_hash_table_foreach (s->faces, (GHFunc) vertex_foreach_face, info);
#endif /* not USE_SURFACE_BTREE */
  g_hash_table_destroy (info[0]);
  /* allow removal of faces */
  s->keep_faces = FALSE;
}

#ifdef USE_SURFACE_BTREE
static gint edge_foreach_face (GtsTriangle * t,
			       gpointer t_data, 
			       gpointer * info)
#else /* not USE_SURFACE_BTREE */
static void edge_foreach_face (GtsTriangle * t,
			       gpointer t_data, 
			       gpointer * info)
#endif /* not USE_SURFACE_BTREE */
{
  GHashTable * hash = info[0];
  gpointer data = info[1];
  GtsFunc func = (GtsFunc) info[2];

  if (!g_hash_table_lookup (hash, t->e1)) {
    (*func) (t->e1, data);
    g_hash_table_insert (hash, t->e1, GINT_TO_POINTER (-1));
  }
  if (!g_hash_table_lookup (hash, t->e2)) {
    (*func) (t->e2, data);
    g_hash_table_insert (hash, t->e2, GINT_TO_POINTER (-1));
  }
  if (!g_hash_table_lookup (hash, t->e3)) {
    (*func) (t->e3, data);
    g_hash_table_insert (hash, t->e3, GINT_TO_POINTER (-1));
  }
#ifdef USE_SURFACE_BTREE
  return FALSE;
#endif /* not USE_SURFACE_BTREE */
}

/**
 * gts_surface_foreach_edge:
 * @s: a #GtsSurface.
 * @func: a #GtsFunc.
 * @data: user data to be passed to @func.
 *
 * Calls @func once for each edge of @s.
 */
void gts_surface_foreach_edge (GtsSurface * s, GtsFunc func, gpointer data)
{
  gpointer info[3];

  g_return_if_fail (s != NULL);
  g_return_if_fail (func != NULL);
  
  /* forbid removal of faces */
  s->keep_faces = TRUE;
  info[0] = g_hash_table_new (NULL, NULL);
  info[1] = data;
  info[2] = func;
#ifdef USE_SURFACE_BTREE
  g_tree_traverse (s->faces, (GTraverseFunc) edge_foreach_face, G_IN_ORDER,
		   info);
#else /* not USE_SURFACE_BTREE */
  g_hash_table_foreach (s->faces, (GHFunc) edge_foreach_face, info);
#endif /* not USE_SURFACE_BTREE */
  g_hash_table_destroy (info[0]);
  /* allow removal of faces */
  s->keep_faces = FALSE;
}

#ifdef USE_SURFACE_BTREE
static gint foreach_face (GtsFace * f, 
			  gpointer t_data,
			  gpointer * info)
#else /* not USE_SURFACE_BTREE */
static void foreach_face (GtsFace * f, 
			  gpointer t_data,
			  gpointer * info)
#endif /* not USE_SURFACE_BTREE */
{
  (*((GtsFunc) info[0])) (f, info[1]);
#ifdef USE_SURFACE_BTREE
  return FALSE;
#endif /* USE_SURFACE_BTREE */
}

/**
 * gts_surface_foreach_face:
 * @s: a #GtsSurface.
 * @func: a #GtsFunc.
 * @data: user data to be passed to @func.
 *
 * Calls @func once for each face of @s.
 */
void gts_surface_foreach_face (GtsSurface * s,
			       GtsFunc func, 
			       gpointer data)
{
  gpointer info[2];

  g_return_if_fail (s != NULL);
  g_return_if_fail (func != NULL);

  /* forbid removal of faces */
  s->keep_faces = TRUE;
  info[0] = func;
  info[1] = data;
#ifdef USE_SURFACE_BTREE
  g_tree_traverse (s->faces, (GTraverseFunc) foreach_face, G_IN_ORDER,
		   info);
#else /* not USE_SURFACE_BTREE */
  g_hash_table_foreach (s->faces, (GHFunc) foreach_face, info);
#endif /* not USE_SURFACE_BTREE */
  /* allow removal of faces */
  s->keep_faces = FALSE;
}

#ifdef USE_SURFACE_BTREE
static gint foreach_face_remove (GtsFace * f,
				 gpointer t_data,
				 gpointer * info)
{
  if ((*((GtsFunc) info[0])) (f, info[1])) {
    GtsSurface * s = info[2];
    guint * n = info[3];

    f->surfaces = g_slist_remove (f->surfaces, s);
    if (!GTS_OBJECT_DESTROYED (f) &&
	!gts_allow_floating_faces && 
	f->surfaces == NULL)
      gts_object_destroy (GTS_OBJECT (f));
    
    if (GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass)->remove_face)
      (* GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass)->remove_face) (s, f);

    g_tree_remove (s->faces, f);
    (*n)++;
  }
  return FALSE;
}
#else /* not USE_SURFACE_BTREE */
static gboolean foreach_face_remove (GtsFace * f,
				     gpointer t_data,
				     gpointer * info)
{
  if ((*((GtsFunc) info[0])) (f, info[1])) {
    GtsSurface * s = info[2];

    f->surfaces = g_slist_remove (f->surfaces, s);
    if (!GTS_OBJECT_DESTROYED (f) &&
	!gts_allow_floating_faces && 
	f->surfaces == NULL)
      gts_object_destroy (GTS_OBJECT (f));
    
    if (GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass)->remove_face)
      (* GTS_SURFACE_CLASS (GTS_OBJECT (s)->klass)->remove_face) (s, f);

    return TRUE;
  }
  return FALSE;
}
#endif /* not USE_SURFACE_BTREE */

/**
 * gts_surface_foreach_face_remove:
 * @s: a #GtsSurface.
 * @func: a #GtsFunc.
 * @data: user data to be passed to @func.
 *
 * Calls @func once for each face of @s. If @func returns %TRUE the
 * corresponding face is removed from @s (and destroyed if it does not
 * belong to any other surface and #gts_allow_floating_faces is set to
 * %FALSE).
 *
 * Returns: the number of faces removed from @s.  
 */
guint gts_surface_foreach_face_remove (GtsSurface * s,
				       GtsFunc func, 
				       gpointer data)
{
  gpointer info[4];
  guint n = 0;

  g_return_val_if_fail (s != NULL, 0);
  g_return_val_if_fail (func != NULL, 0);

  /* forbid removal of faces */
  s->keep_faces = TRUE;
  info[0] = func;
  info[1] = data;
  info[2] = s;
#ifdef USE_SURFACE_BTREE
  info[3] = &n;
  g_tree_traverse (s->faces, (GTraverseFunc) foreach_face_remove, G_PRE_ORDER,
		   info);
#else /* not USE_SURFACE_BTREE */
  n = g_hash_table_foreach_remove (s->faces, 
				   (GHRFunc) foreach_face_remove, 
				   info);
#endif /* not USE_SURFACE_BTREE */
  /* allow removal of faces */
  s->keep_faces = FALSE;
  
  return n;
}

static void midvertex_insertion (GtsEdge * e,
				 GtsSurface * surface,
				 GtsEHeap * heap,
				 GtsRefineFunc refine_func,
				 gpointer refine_data,
				 GtsVertexClass * vertex_class,
				 GtsEdgeClass * edge_class)
{
  GtsVertex * midvertex;
  GtsEdge * e1, * e2;
  GSList * i;

  midvertex = (*refine_func) (e, vertex_class, refine_data);
  e1 = gts_edge_new (edge_class, GTS_SEGMENT (e)->v1, midvertex);
  gts_eheap_insert (heap, e1);
  e2 = gts_edge_new (edge_class, GTS_SEGMENT (e)->v2, midvertex);
  gts_eheap_insert (heap, e2);
  
  /* creates new faces and modifies old ones */
  i = e->triangles;
  while (i) {
    GtsTriangle * t = i->data;
    GtsVertex * v1, * v2, * v3;
    GtsEdge * te2, * te3, * ne, * tmp;

    gts_triangle_vertices_edges (t, e, &v1, &v2, &v3, &e, &te2, &te3);
    ne = gts_edge_new (edge_class, midvertex, v3);
    gts_eheap_insert (heap, ne);
    if (GTS_SEGMENT (e1)->v1 == v2) {
      tmp = e1; e1 = e2; e2 = tmp;
    }
    e1->triangles = g_slist_prepend (e1->triangles, t);
    ne->triangles = g_slist_prepend (ne->triangles, t);
    te2->triangles = g_slist_remove (te2->triangles, t);
    t->e1 = e1; t->e2 = ne; t->e3 = te3;
    gts_surface_add_face (surface, 
			  gts_face_new (surface->face_class, e2, te2, ne));
    i = i->next;
  }
  /* destroys edge */
  g_slist_free (e->triangles);
  e->triangles = NULL;
  gts_object_destroy (GTS_OBJECT (e));
}

static gdouble edge_length2_inverse (GtsSegment * s)
{
  return - gts_point_distance2 (GTS_POINT (s->v1), GTS_POINT (s->v2));
}

static void create_heap_refine (GtsEdge * e, GtsEHeap * heap)
{
  gts_eheap_insert (heap, e);
}

/**
 * gts_surface_refine:
 * @surface: a #GtsSurface.
 * @cost_func: a function returning the cost for a given edge.
 * @cost_data: user data to be passed to @cost_func.
 * @refine_func: a #GtsRefineFunc.
 * @refine_data: user data to be passed to @refine_func.
 * @stop_func: a #GtsStopFunc.
 * @stop_data: user data to be passed to @stop_func.
 *
 * Refine @surface using a midvertex insertion technique. All the
 * edges of @surface are ordered according to @cost_func. The edges
 * are then processed in order until @stop_func returns %TRUE. Each
 * edge is split in two and new edges and faces are created.
 *
 * If @cost_func is set to %NULL, the edges are sorted according 
 * to their length squared (the longest is on top).
 *
 * If @refine_func is set to %NULL gts_segment_midvertex() is used.
 * 
 */
void gts_surface_refine (GtsSurface * surface,
			 GtsKeyFunc cost_func,
			 gpointer cost_data,
			 GtsRefineFunc refine_func,
			 gpointer refine_data,
			 GtsStopFunc stop_func,
			 gpointer stop_data)
{
  GtsEHeap * heap;
  GtsEdge * e;
  gdouble top_cost;

  g_return_if_fail (surface != NULL);
  g_return_if_fail (stop_func != NULL);

  if (cost_func == NULL)
    cost_func = (GtsKeyFunc) edge_length2_inverse;
  if (refine_func == NULL)
    refine_func = (GtsRefineFunc) gts_segment_midvertex;

  heap = gts_eheap_new (cost_func, cost_data);
  gts_eheap_freeze (heap);
  gts_surface_foreach_edge (surface, (GtsFunc) create_heap_refine, heap);
  gts_eheap_thaw (heap);
  while ((e = gts_eheap_remove_top (heap, &top_cost)) &&
	 !(*stop_func) (top_cost,
			gts_eheap_size (heap) + 
			gts_edge_face_number (e, surface) + 2,
			stop_data))
    midvertex_insertion (e, surface, heap, refine_func, refine_data,
			 surface->vertex_class, surface->edge_class);
  gts_eheap_destroy (heap);
}

static GSList * edge_triangles (GtsEdge * e1, GtsEdge * e)
{
  GSList * i = e1->triangles;
  GSList * triangles = NULL;
  
  while (i) {
    GtsTriangle * t = i->data;
    if (t->e1 == e || t->e2 == e || t->e3 == e) {
      GtsEdge * e2;
      GSList * j;
      if (t->e1 == e) {
	if (t->e2 == e1)
	  e2 = t->e3;
	else
	  e2 = t->e2;
      }
      else if (t->e2 == e) {
	if (t->e3 == e1)
	  e2 = t->e1;
	else
	  e2 = t->e3;
      }
      else {
	if (t->e2 == e1)
	  e2 = t->e1;
	else
	  e2 = t->e2;
      }
      j = e2->triangles;
      while (j) {
	GtsTriangle * t = j->data;
	if (t->e1 != e && t->e2 != e && t->e3 != e)
	  triangles = g_slist_prepend (triangles, t);
	j = j->next;
      }
    }
    else
      triangles = g_slist_prepend (triangles, t);
    i = i->next;
  }
  return triangles;
}

static void replace_vertex (GSList * i, GtsVertex * v1, GtsVertex * v)
{
  while (i) {
    GtsSegment * s = i->data;
    if (s->v1 == v1)
      s->v1 = v;
    else
      s->v2 = v;
    i = i->next;
  }
}

/**
 * gts_edge_collapse_creates_fold:
 * @e: a #GtsEdge.
 * @v: a #GtsVertex.
 * @max:  the maximum value of the square of the cosine of the angle between
 * two triangles.
 *
 * Returns: %TRUE if collapsing edge @e to vertex @v would create
 * faces making an angle the cosine squared of which would be larger than max,
 * %FALSE otherwise.  
 */
gboolean gts_edge_collapse_creates_fold (GtsEdge * e, 
					 GtsVertex * v,
					 gdouble max)
{
  GtsVertex * v1, * v2;
  GtsSegment * s;
  GSList * i;
  gboolean folded = FALSE;

  g_return_val_if_fail (e != NULL, TRUE);
  g_return_val_if_fail (v != NULL, TRUE);

  s = GTS_SEGMENT (e);
  v1 = s->v1;
  v2 = s->v2;
  replace_vertex (v1->segments, v1, v);
  replace_vertex (v2->segments, v2, v);

  i = v1->segments;
  while (i && !folded) {
    GtsSegment * s = i->data;
    if (GTS_IS_EDGE (s)) {
      GtsEdge * e1 = GTS_EDGE (s);
      if (e1 != e) {
	GSList * triangles = edge_triangles (e1, e);
	folded = gts_triangles_are_folded (triangles, s->v1, s->v2, max);
	g_slist_free (triangles);
      }
    }
    i = i->next;
  }

  i = v2->segments;
  while (i && !folded) {
    GtsSegment * s = i->data;
    if (GTS_IS_EDGE (s)) {
      GtsEdge * e1 = GTS_EDGE (s);
      if (e1 != e) {
	GSList * triangles = edge_triangles (e1, e);
	folded = gts_triangles_are_folded (triangles, s->v1, s->v2, max);
	g_slist_free (triangles);
      }
    }
    i = i->next;
  }
#if 1
  if (!folded) {
    GSList * triangles = gts_vertex_triangles (v1, NULL);
    i = triangles = gts_vertex_triangles (v2, triangles);
    while (i && !folded) {
      GtsTriangle * t = i->data;
      if (t->e1 != e && t->e2 != e && t->e3 != e) {
	GtsEdge * e1 = gts_triangle_edge_opposite (t, v);
	g_assert (e1);
	folded = gts_triangles_are_folded (e1->triangles, 
					   GTS_SEGMENT (e1)->v1,
					   GTS_SEGMENT (e1)->v2,
					   max);
      }
      i = i->next;
    }
    g_slist_free (triangles);
  }
#endif
  replace_vertex (v1->segments, v, v1);
  replace_vertex (v2->segments, v, v2);
  return folded;
}

/**
 * gts_edge_collapse_is_valid:
 * @e: a #GtsEdge.
 *
 * An implementation of the topological constraints described in the 
 * "Mesh Optimization" article of Hoppe et al (1993).
 *
 * Returns: %TRUE if @e can be collapsed without violation of the topological
 * constraints, %FALSE otherwise.
 */
gboolean gts_edge_collapse_is_valid (GtsEdge * e)
{
  GSList * i;

  g_return_val_if_fail (e != NULL, FALSE);

  i = GTS_SEGMENT (e)->v1->segments;
  while (i) {
    GtsEdge * e1 = i->data;
    if (e1 != e && GTS_IS_EDGE (e1)) {
      GtsEdge * e2 = NULL;
      GSList * j = GTS_SEGMENT (e1)->v1 == GTS_SEGMENT (e)->v1 ? 
	GTS_SEGMENT (e1)->v2->segments : GTS_SEGMENT (e1)->v1->segments;
      while (j && !e2) {
	GtsEdge * e1 = j->data;
	if (GTS_IS_EDGE (e1) && 
	    (GTS_SEGMENT (e1)->v1 == GTS_SEGMENT (e)->v2 || 
	     GTS_SEGMENT (e1)->v2 == GTS_SEGMENT (e)->v2))
	  e2 = e1;
	j = j->next;
      }
      if (e2 && !gts_triangle_use_edges (e, e1, e2))
	return FALSE;
    }
    i = i->next;
  }

  if (gts_edge_is_boundary (e, NULL)) {
    GtsTriangle * t = e->triangles->data;
    if (gts_edge_is_boundary (t->e1, NULL) &&
	gts_edge_is_boundary (t->e2, NULL) &&
	gts_edge_is_boundary (t->e3, NULL))
      return FALSE;
  }
  else {
    if (gts_vertex_is_boundary (GTS_SEGMENT (e)->v1, NULL) &&
	gts_vertex_is_boundary (GTS_SEGMENT (e)->v2, NULL))
      return FALSE;    
    if (gts_edge_belongs_to_tetrahedron (e))
      return FALSE;
  }

  return TRUE;
}

#define HEAP_INSERT_EDGE(h, e) (GTS_OBJECT (e)->reserved = gts_eheap_insert (h, e))
#define HEAP_REMOVE_EDGE(h, e) (gts_eheap_remove (h, GTS_OBJECT (e)->reserved),\
                                GTS_OBJECT (e)->reserved = NULL)

static GtsVertex * edge_collapse (GtsEdge * e,
				  GtsEHeap * heap,
				  GtsCoarsenFunc coarsen_func,
				  gpointer coarsen_data,
				  GtsVertexClass * klass,
				  gdouble maxcosine2)
{
  GSList * i;
  GtsVertex  * v1 = GTS_SEGMENT (e)->v1, * v2 = GTS_SEGMENT (e)->v2, * mid;

  /* if the edge is degenerate (i.e. v1 == v2), destroy and return */
  if (v1 == v2) {
    gts_object_destroy (GTS_OBJECT (e));
    return NULL;
  }

  if (!gts_edge_collapse_is_valid (e)) {
    GTS_OBJECT (e)->reserved = 
      gts_eheap_insert_with_key (heap, e, G_MAXDOUBLE);
    return NULL;
  }

  mid = (*coarsen_func) (e, klass, coarsen_data);

  if (gts_edge_collapse_creates_fold (e, mid, maxcosine2)) {
    GTS_OBJECT (e)->reserved = 
      gts_eheap_insert_with_key (heap, e, G_MAXDOUBLE);
    gts_object_destroy (GTS_OBJECT (mid));
    return NULL;
  }

  gts_object_destroy (GTS_OBJECT (e));

  gts_vertex_replace (v1, mid);
  gts_object_destroy (GTS_OBJECT (v1));
  gts_vertex_replace (v2, mid);
  gts_object_destroy (GTS_OBJECT (v2));

  /* destroy duplicate edges */
  i = mid->segments;
  while (i) {
    GtsEdge * e1 = i->data;
    GtsEdge * duplicate;
    while ((duplicate = gts_edge_is_duplicate (e1))) {
      gts_edge_replace (duplicate, GTS_EDGE (e1));
      HEAP_REMOVE_EDGE (heap, duplicate);
      gts_object_destroy (GTS_OBJECT (duplicate));
    }
    i = i->next;
    if (!e1->triangles) {
      /* e1 is the result of the collapse of one edge of a pair of identical
	 faces (it should not happen unless duplicate triangles are present in
	 the initial surface) */
      g_warning ("file %s: line %d (%s): probably duplicate triangle.",
		 __FILE__, __LINE__, G_GNUC_PRETTY_FUNCTION);
      HEAP_REMOVE_EDGE (heap, e1);
      gts_object_destroy (GTS_OBJECT (e1));
      if (i == NULL) /* mid has been destroyed */
	mid = NULL;
    }
  }

  return mid;
}

static void update_closest_neighbors (GtsVertex * v, GtsEHeap * heap)
{
  GSList * i = v->segments;
  
  while (i) {
    GtsSegment * s = i->data;
    if (GTS_IS_EDGE (s)) {
      HEAP_REMOVE_EDGE (heap, GTS_EDGE (s));
      HEAP_INSERT_EDGE (heap, GTS_EDGE (s));
    }
    i = i->next;
  }
}

static void update_2nd_closest_neighbors (GtsVertex * v, GtsEHeap * heap)
{
  GSList * i = v->segments;
  GSList * list = NULL;
  
  while (i) {
    GtsSegment * s = i->data;
    if (GTS_IS_EDGE (s)) {
      GtsVertex * v1 = s->v1 == v ? s->v2 : s->v1;
      GSList * j = v1->segments;
      while (j) {
	GtsSegment * s1 = j->data;
	if (GTS_IS_EDGE (s1) && !g_slist_find (list, s1))
	  list = g_slist_prepend (list, s1);
	j = j->next;
      }
    }
    i = i->next;
  }

  i = list;
  while (i) {
    GtsEdge * e = i->data;
    HEAP_REMOVE_EDGE (heap, e);
    HEAP_INSERT_EDGE (heap, e);
    i = i->next;
  }

  g_slist_free (list);
}

static gdouble edge_length2 (GtsEdge * e)
{
  return gts_point_distance2 (GTS_POINT (GTS_SEGMENT (e)->v1), 
			      GTS_POINT (GTS_SEGMENT (e)->v2));
}

static void create_heap_coarsen (GtsEdge * e, GtsEHeap * heap)
{
  HEAP_INSERT_EDGE (heap, e);
}

/**
 * gts_surface_coarsen:
 * @surface: a #GtsSurface.
 * @cost_func: a function returning the cost for a given edge.
 * @cost_data: user data to be passed to @cost_func.
 * @coarsen_func: a #GtsCoarsenVertexFunc.
 * @coarsen_data: user data to be passed to @coarsen_func.
 * @stop_func: a #GtsStopFunc.
 * @stop_data: user data to be passed to @stop_func.
 * @minangle: minimum angle between two neighboring triangles.
 *
 * The edges of @surface are sorted according to @cost_func to 
 * create a priority heap (a #GtsEHeap). The edges are extracted in
 * turn from the top of the heap and collapsed (i.e. the vertices are
 * replaced by the vertex returned by the @coarsen_func function)
 * until the @stop_func functions returns %TRUE.
 *
 * If @cost_func is set to %NULL, the edges are sorted according 
 * to their length squared (the shortest is on top).
 *
 * If @coarsen_func is set to %NULL gts_segment_midvertex() is used.
 *
 * The minimum angle is used to avoid introducing faces which would be folded.
 */
void gts_surface_coarsen (GtsSurface * surface,
			  GtsKeyFunc cost_func,
			  gpointer cost_data,
			  GtsCoarsenFunc coarsen_func,
			  gpointer coarsen_data,
			  GtsStopFunc stop_func,
			  gpointer stop_data,
			  gdouble minangle)
{
  GtsEHeap * heap;
  GtsEdge * e;
  gdouble top_cost;
  gdouble maxcosine2;

  g_return_if_fail (surface != NULL);
  g_return_if_fail (stop_func != NULL);

  if (cost_func == NULL)
    cost_func = (GtsKeyFunc) edge_length2;
  if (coarsen_func == NULL)
    coarsen_func = (GtsCoarsenFunc) gts_segment_midvertex;

  heap = gts_eheap_new (cost_func, cost_data);
  maxcosine2 = cos (minangle); maxcosine2 *= maxcosine2;

  gts_eheap_freeze (heap);
  gts_surface_foreach_edge (surface, (GtsFunc) create_heap_coarsen, heap);
  gts_eheap_thaw (heap);
  /* we want to control edge destruction manually */
  gts_allow_floating_edges = TRUE;
  while ((e = gts_eheap_remove_top (heap, &top_cost)) &&
	 (top_cost < G_MAXDOUBLE) &&
	 !(*stop_func) (top_cost, gts_eheap_size (heap) - 
			gts_edge_face_number (e, surface), stop_data))
    {
      GtsVertex * v = edge_collapse (e, heap, coarsen_func, coarsen_data,
				     surface->vertex_class, maxcosine2);
      if (v != NULL)
	update_2nd_closest_neighbors (v, heap);
    }
  gts_allow_floating_edges = FALSE;

  /* set reserved field of remaining edges back to NULL */
  if (e) GTS_OBJECT (e)->reserved = NULL;
  gts_eheap_foreach (heap, (GFunc) gts_object_reset_reserved, NULL);

  gts_eheap_destroy (heap);
}

/**
 * gts_coarsen_stop_number:
 * @cost: the cost of the edge collapse considered.
 * @nedge: the current number of edges of the surface being simplified.
 * @min_number: a pointer to the minimum number of edges desired for the 
 * surface being simplified.
 *
 * This function is to be used as the @stop_func argument of 
 * gts_surface_coarsen() or gts_psurface_new().
 *
 * Returns: %TRUE if the edge collapse would create a surface with a smaller 
 * number of edges than given by @min_number, %FALSE otherwise.
 */
gboolean gts_coarsen_stop_number (gdouble cost, 
				  guint nedge, 
				  guint * min_number)
{
  g_return_val_if_fail (min_number != NULL, TRUE);

  if (nedge < *min_number)
    return TRUE;
  return FALSE;
}

/**
 * gts_coarsen_stop_cost:
 * @cost: the cost of the edge collapse considered.
 * @nedge: the current number of edges of the surface being simplified.
 * @max_cost: a pointer to the maximum cost allowed for an edge collapse.
 *
 * This function is to be used as the @stop_func argument of 
 * gts_surface_coarsen() or gts_psurface_new().
 *
 * Returns: %TRUE if the cost of the edge collapse considered is larger than
 * given by @max_cost, %FALSE otherwise.
 */
gboolean gts_coarsen_stop_cost (gdouble cost, 
				guint nedge, 
				gdouble * max_cost)
{
  g_return_val_if_fail (max_cost != NULL, TRUE);

  if (cost > *max_cost)
    return TRUE;
  return FALSE;
}

#define GTS_M_ICOSAHEDRON_X /* sqrt(sqrt(5)+1)/sqrt(2*sqrt(5)) */ \
  0.850650808352039932181540497063011072240401406
#define GTS_M_ICOSAHEDRON_Y /* sqrt(2)/sqrt(5+sqrt(5))         */ \
  0.525731112119133606025669084847876607285497935
#define GTS_M_ICOSAHEDRON_Z 0.0

static guint generate_icosahedron (GtsSurface * s)
{
  GtsVertex * v01 = gts_vertex_new (s->vertex_class,
      +GTS_M_ICOSAHEDRON_Z, +GTS_M_ICOSAHEDRON_X, -GTS_M_ICOSAHEDRON_Y);
  GtsVertex * v02 = gts_vertex_new (s->vertex_class,
      +GTS_M_ICOSAHEDRON_X, +GTS_M_ICOSAHEDRON_Y, +GTS_M_ICOSAHEDRON_Z);
  GtsVertex * v03 = gts_vertex_new (s->vertex_class,
      +GTS_M_ICOSAHEDRON_Y, +GTS_M_ICOSAHEDRON_Z, -GTS_M_ICOSAHEDRON_X);
  GtsVertex * v04 = gts_vertex_new (s->vertex_class,
      +GTS_M_ICOSAHEDRON_Y, +GTS_M_ICOSAHEDRON_Z, +GTS_M_ICOSAHEDRON_X);
  GtsVertex * v05 = gts_vertex_new (s->vertex_class,
      +GTS_M_ICOSAHEDRON_X, -GTS_M_ICOSAHEDRON_Y, +GTS_M_ICOSAHEDRON_Z);
  GtsVertex * v06 = gts_vertex_new (s->vertex_class,
      +GTS_M_ICOSAHEDRON_Z, +GTS_M_ICOSAHEDRON_X, +GTS_M_ICOSAHEDRON_Y);
  GtsVertex * v07 = gts_vertex_new (s->vertex_class,
      -GTS_M_ICOSAHEDRON_Y, +GTS_M_ICOSAHEDRON_Z, +GTS_M_ICOSAHEDRON_X);
  GtsVertex * v08 = gts_vertex_new (s->vertex_class,
      +GTS_M_ICOSAHEDRON_Z, -GTS_M_ICOSAHEDRON_X, -GTS_M_ICOSAHEDRON_Y);
  GtsVertex * v09 = gts_vertex_new (s->vertex_class,
      -GTS_M_ICOSAHEDRON_X, +GTS_M_ICOSAHEDRON_Y, +GTS_M_ICOSAHEDRON_Z);
  GtsVertex * v10 = gts_vertex_new (s->vertex_class,
      -GTS_M_ICOSAHEDRON_Y, +GTS_M_ICOSAHEDRON_Z, -GTS_M_ICOSAHEDRON_X);
  GtsVertex * v11 = gts_vertex_new (s->vertex_class,
      -GTS_M_ICOSAHEDRON_X, -GTS_M_ICOSAHEDRON_Y, +GTS_M_ICOSAHEDRON_Z);
  GtsVertex * v12 = gts_vertex_new (s->vertex_class,
      +GTS_M_ICOSAHEDRON_Z, -GTS_M_ICOSAHEDRON_X, +GTS_M_ICOSAHEDRON_Y);

  GtsEdge * e01 = gts_edge_new (s->edge_class, v01, v02);
  GtsEdge * e02 = gts_edge_new (s->edge_class, v03, v02);
  GtsEdge * e03 = gts_edge_new (s->edge_class, v01, v03);
  GtsEdge * e04 = gts_edge_new (s->edge_class, v04, v05);
  GtsEdge * e05 = gts_edge_new (s->edge_class, v02, v05);
  GtsEdge * e06 = gts_edge_new (s->edge_class, v04, v02);
  GtsEdge * e07 = gts_edge_new (s->edge_class, v06, v07);
  GtsEdge * e08 = gts_edge_new (s->edge_class, v04, v07);
  GtsEdge * e09 = gts_edge_new (s->edge_class, v06, v04);
  GtsEdge * e10 = gts_edge_new (s->edge_class, v08, v03);
  GtsEdge * e11 = gts_edge_new (s->edge_class, v03, v05);
  GtsEdge * e12 = gts_edge_new (s->edge_class, v08, v05);
  GtsEdge * e13 = gts_edge_new (s->edge_class, v06, v09);
  GtsEdge * e14 = gts_edge_new (s->edge_class, v07, v09);
  GtsEdge * e15 = gts_edge_new (s->edge_class, v08, v10);
  GtsEdge * e16 = gts_edge_new (s->edge_class, v03, v10);
  GtsEdge * e17 = gts_edge_new (s->edge_class, v06, v01);
  GtsEdge * e18 = gts_edge_new (s->edge_class, v01, v09);
  GtsEdge * e19 = gts_edge_new (s->edge_class, v08, v11);
  GtsEdge * e20 = gts_edge_new (s->edge_class, v10, v11);
  GtsEdge * e21 = gts_edge_new (s->edge_class, v06, v02);
  GtsEdge * e22 = gts_edge_new (s->edge_class, v12, v11);
  GtsEdge * e23 = gts_edge_new (s->edge_class, v12, v08);
  GtsEdge * e24 = gts_edge_new (s->edge_class, v12, v07);
  GtsEdge * e25 = gts_edge_new (s->edge_class, v07, v11);
  GtsEdge * e26 = gts_edge_new (s->edge_class, v12, v04);
  GtsEdge * e27 = gts_edge_new (s->edge_class, v09, v11);
  GtsEdge * e28 = gts_edge_new (s->edge_class, v10, v09);
  GtsEdge * e29 = gts_edge_new (s->edge_class, v12, v05);
  GtsEdge * e30 = gts_edge_new (s->edge_class, v01, v10);
  
  gts_surface_add_face (s, gts_face_new (s->face_class, e01, e02, e03));
  gts_surface_add_face (s, gts_face_new (s->face_class, e04, e05, e06));
  gts_surface_add_face (s, gts_face_new (s->face_class, e07, e08, e09));
  gts_surface_add_face (s, gts_face_new (s->face_class, e10, e11, e12));
  gts_surface_add_face (s, gts_face_new (s->face_class, e13, e14, e07));
  gts_surface_add_face (s, gts_face_new (s->face_class, e15, e16, e10));
  gts_surface_add_face (s, gts_face_new (s->face_class, e17, e18, e13));
  gts_surface_add_face (s, gts_face_new (s->face_class, e19, e20, e15));
  gts_surface_add_face (s, gts_face_new (s->face_class, e21, e01, e17));
  gts_surface_add_face (s, gts_face_new (s->face_class, e22, e19, e23));
  gts_surface_add_face (s, gts_face_new (s->face_class, e09, e06, e21));
  gts_surface_add_face (s, gts_face_new (s->face_class, e24, e25, e22));
  gts_surface_add_face (s, gts_face_new (s->face_class, e26, e08, e24));
  gts_surface_add_face (s, gts_face_new (s->face_class, e20, e27, e28));
  gts_surface_add_face (s, gts_face_new (s->face_class, e29, e04, e26));
  gts_surface_add_face (s, gts_face_new (s->face_class, e14, e27, e25));
  gts_surface_add_face (s, gts_face_new (s->face_class, e23, e12, e29));
  gts_surface_add_face (s, gts_face_new (s->face_class, e02, e05, e11));
  gts_surface_add_face (s, gts_face_new (s->face_class, e30, e28, e18));
  gts_surface_add_face (s, gts_face_new (s->face_class, e03, e16, e30));

  return 0;
}

static GtsVertex * unit_sphere_arc_midvertex (GtsSegment * s, 
					      GtsVertexClass * vertex_class)
{
  GtsPoint * p1, * p2;
  gdouble x, y, z, norm;

  p1 = GTS_POINT (s->v1); p2 = GTS_POINT (s->v2);

  x = 0.5*(p1->x + p2->x);
  y = 0.5*(p1->y + p2->y);
  z = 0.5*(p1->z + p2->z);

  norm = x*x + y*y + z*z;
  norm = sqrt (norm);

  x /= norm; y /= norm; z /= norm;

  return gts_vertex_new (vertex_class, x, y, z);
}

static void tessellate_face (GtsFace * f,
			     GtsSurface * s,
			     GtsRefineFunc refine_func,
			     gpointer refine_data,
			     GtsVertexClass * vertex_class,
			     GtsEdgeClass * edge_class)
{
  GtsTriangle * t;
  GtsEdge * e1, * e2, * e3;                          /* former edges     */
  GtsVertex * v1, * v2, * v3;                        /* initial vertices */
  GtsVertex * v4, * v5, * v6;                        /* new vertices     */ 
  GtsEdge * e56, * e64, * e45;                       /* new inside edges */
  GtsEdge * e24, * e34, * e35, * e15, * e16, * e26;  /* new border edges */
  GSList * dum;
  GtsEdge * edum;
  
  t = GTS_TRIANGLE (f);
  e1 = t->e1; e2 = t->e2; e3 = t->e3;

  if (GTS_SEGMENT (e1)->v2 == GTS_SEGMENT (e2)->v1) {
    v1 = GTS_SEGMENT (e2)->v2;
    v2 = GTS_SEGMENT (e1)->v1;
    v3 = GTS_SEGMENT (e1)->v2;
  }
  else if (GTS_SEGMENT (e1)->v2 == GTS_SEGMENT (e2)->v2) {
    v1 = GTS_SEGMENT (e2)->v1;
    v2 = GTS_SEGMENT (e1)->v1;
    v3 = GTS_SEGMENT (e1)->v2;
  }
  else if (GTS_SEGMENT (e1)->v1 == GTS_SEGMENT (e2)->v1) {
    v1 = GTS_SEGMENT (e2)->v2;
    v2 = GTS_SEGMENT (e1)->v2;
    v3 = GTS_SEGMENT (e1)->v1;
  }
  else if (GTS_SEGMENT (e1)->v1 == GTS_SEGMENT (e2)->v2) {
    v1 = GTS_SEGMENT (e2)->v1;
    v2 = GTS_SEGMENT (e1)->v2;
    v3 = GTS_SEGMENT (e1)->v1;
  }
  else {
    v1 = v2 = v3 = NULL;
    g_assert_not_reached ();
  }

  e1->triangles = g_slist_remove (e1->triangles, t);
  e2->triangles = g_slist_remove (e2->triangles, t);
  e3->triangles = g_slist_remove (e3->triangles, t);
  
  if (GTS_OBJECT (e1)->reserved) {
    dum = (GTS_OBJECT (e1)->reserved);
    e24 = dum->data;
    e34 = dum->next->data;
    v4 = GTS_SEGMENT (e24)->v2;
    if (GTS_SEGMENT (e24)->v1 == v3) {
      edum = e34; e34 = e24; e24 = edum;
    }
  }
  else {
    v4 = (*refine_func) (e1, vertex_class, refine_data);
    e24 = gts_edge_new (edge_class, v2, v4);
    e34 = gts_edge_new (edge_class, v3, v4);
    dum = g_slist_append (NULL, e24);
    dum = g_slist_append (dum,  e34);
    GTS_OBJECT (e1)->reserved = dum;
  }
  if (GTS_OBJECT (e2)->reserved) {
    dum = (GTS_OBJECT (e2)->reserved);
    e35 = dum->data;
    e15 = dum->next->data;
    v5 = GTS_SEGMENT (e35)->v2;
    if (GTS_SEGMENT (e35)->v1 == v1) {
      edum = e15; e15 = e35; e35 = edum;
    }
  }
  else {
    v5 = (*refine_func) (e2, vertex_class, refine_data);
    e35 = gts_edge_new (edge_class, v3, v5);
    e15 = gts_edge_new (edge_class, v1, v5);
    dum = g_slist_append (NULL, e35);
    dum = g_slist_append (dum,  e15);
    GTS_OBJECT (e2)->reserved = dum;
  }
  if (GTS_OBJECT (e3)->reserved) {
    dum = (GTS_OBJECT (e3)->reserved);
    e16 = dum->data;
    e26 = dum->next->data;
    v6 = GTS_SEGMENT (e16)->v2;
    if (GTS_SEGMENT (e16)->v1 == v2) {
      edum = e16; e16 = e26; e26 = edum;
    }
  }
  else {
    v6 = (*refine_func) (e3, vertex_class, refine_data);
    e16 = gts_edge_new (edge_class, v1, v6);
    e26 = gts_edge_new (edge_class, v2, v6);
    dum = g_slist_append (NULL, e16);
    dum = g_slist_append (dum,  e26);
    GTS_OBJECT (e3)->reserved = dum;
  }
  
  if (e1->triangles == NULL) {
    g_slist_free (GTS_OBJECT (e1)->reserved);
    GTS_OBJECT (e1)->reserved = NULL;
    gts_object_destroy (GTS_OBJECT (e1));
    e1 = NULL;
  }
  if (e2->triangles == NULL) {
    g_slist_free (GTS_OBJECT (e2)->reserved);
    GTS_OBJECT (e2)->reserved = NULL;
    gts_object_destroy (GTS_OBJECT (e2));
    e2 = NULL;
  }
  if (e3->triangles == NULL) {
    g_slist_free (GTS_OBJECT (e3)->reserved);
    GTS_OBJECT (e3)->reserved = NULL;
    gts_object_destroy (GTS_OBJECT (e3));
    e3 = NULL;
  }

  e56 = gts_edge_new (edge_class, v5, v6);
  e64 = gts_edge_new (edge_class, v6, v4);
  e45 = gts_edge_new (edge_class, v4, v5);
  t->e1 = e56; e56->triangles = g_slist_prepend (e56->triangles, t);
  t->e2 = e64; e64->triangles = g_slist_prepend (e64->triangles, t);
  t->e3 = e45; e45->triangles = g_slist_prepend (e45->triangles, t);
  
  gts_surface_add_face (s, gts_face_new (s->face_class, e16, e56, e15));
  gts_surface_add_face (s, gts_face_new (s->face_class, e26, e24, e64));
  gts_surface_add_face (s, gts_face_new (s->face_class, e45, e34, e35)); 
}

static void create_array_tessellate (GtsFace * f, GPtrArray * array)
{
  g_ptr_array_add (array, f);
}

/**
 * gts_surface_tessellate:
 * @s: a #GtsSurface.
 * @refine_func: a #GtsRefineFunc.
 * @refine_data: user data to be passed to @refine_func.
 *
 * Tessellate each triangle of @s with 4 triangles:   
 * the number of triangles is increased by a factor of 4.
 * http://mathworld.wolfram.com/GeodesicDome.html
 *
 * If @refine_func is set to %NULL a mid arc function is used: if
 * the surface is a polyhedron with the unit sphere as circum sphere,
 * then gts_surface_tessellate() corresponds to a geodesation step
 * (see gts_surface_generate_sphere()).
 * 
 */
void gts_surface_tessellate (GtsSurface * s,
			     GtsRefineFunc refine_func,
			     gpointer refine_data)
{
  GPtrArray * array;
  guint i;

  g_return_if_fail (s != NULL);
  
  if (refine_func == NULL) /* tessellate_surface == geodesate_surface */
    refine_func = (GtsRefineFunc) unit_sphere_arc_midvertex;

  array = g_ptr_array_new ();
  gts_surface_foreach_face (s, (GtsFunc) create_array_tessellate, array);
  for(i = 0; i < array->len; i++)
    tessellate_face (g_ptr_array_index (array, i),
		     s, refine_func, refine_data, 
		     s->vertex_class, s->edge_class);
  g_ptr_array_free (array, TRUE);
}

/**
 * gts_surface_generate_sphere:
 * @s: a #GtsSurface.
 * @geodesation_order: a #guint.
 *
 * Add a triangulated unit sphere generated by recursive subdivision to @s.
 * First approximation is an isocahedron; each level of refinement
 * (@geodesation_order) increases the number of triangles by a factor of 4.
 * http://mathworld.wolfram.com/GeodesicDome.html
 *
 * Returns: @s.
 */
GtsSurface * gts_surface_generate_sphere (GtsSurface * s, 
					  guint geodesation_order)
{
  guint cgo; 

  g_return_val_if_fail (s != NULL, NULL);
  g_return_val_if_fail (geodesation_order != 0, NULL);

  generate_icosahedron (s);

  for (cgo = 1; cgo < geodesation_order; cgo++)
    gts_surface_tessellate (s, NULL, NULL);
  
  return s;
}

static void foreach_vertex_copy (GtsPoint * p, GtsVertexClass * klass)
{
  GTS_OBJECT (p)->reserved = gts_vertex_new (klass, p->x, p->y, p->z);
}

static void foreach_edge_copy (GtsSegment * s, GtsEdgeClass * klass)
{
  GTS_OBJECT (s)->reserved = gts_edge_new (klass,
					   GTS_OBJECT (s->v1)->reserved, 
					   GTS_OBJECT (s->v2)->reserved);
}

static void foreach_face_copy (GtsTriangle * t,
			       GtsSurface * s)
{
  gts_surface_add_face (s, gts_face_new (s->face_class,
					 GTS_OBJECT (t->e1)->reserved,
					 GTS_OBJECT (t->e2)->reserved,
					 GTS_OBJECT (t->e3)->reserved));
}

/**
 * gts_surface_copy:
 * @s1: a #GtsSurface.
 * @s2: a #GtsSurface.
 *
 * Add a copy of all the faces, edges and vertices of @s2 to @s1.
 *
 * Returns: @s1.
 */
GtsSurface * gts_surface_copy (GtsSurface * s1, GtsSurface * s2)
{
  g_return_val_if_fail (s1 != NULL, NULL);
  g_return_val_if_fail (s2 != NULL, NULL);
  
  gts_surface_foreach_vertex (s2, (GtsFunc) foreach_vertex_copy, 
			      s1->vertex_class);
  gts_surface_foreach_edge (s2, (GtsFunc) foreach_edge_copy, s1->edge_class);
  gts_surface_foreach_face (s2, (GtsFunc) foreach_face_copy, s1);

  gts_surface_foreach_vertex (s2, (GtsFunc) gts_object_reset_reserved, NULL);
  gts_surface_foreach_edge (s2, (GtsFunc) gts_object_reset_reserved, NULL);
  
  return s1;
}

static void merge_foreach_face (GtsFace * f, 
				GtsSurface * s)
{
  gts_surface_add_face (s, f);
}

/**
 * gts_surface_merge:
 * @s: a #GtsSurface.
 * @with: another #GtsSurface.
 *
 * Adds all the faces of @with which do not already belong to @s
 * to @s.
 */
void gts_surface_merge (GtsSurface * s, GtsSurface * with)
{
  g_return_if_fail (s != NULL);
  g_return_if_fail (with != NULL);
  
  gts_surface_foreach_face (with, (GtsFunc) merge_foreach_face, s);
}

static void manifold_foreach_edge (GtsEdge * e, gpointer * data)
{
  gboolean * is_manifold = data[0];

  if (*is_manifold) {
    if (gts_edge_face_number (e, data[1]) > 2)
      *is_manifold = FALSE;
  }
}

/**
 * gts_surface_is_manifold:
 * @s: a #GtsSurface.
 *
 * Returns: %TRUE if the surface is a manifold, %FALSE otherwise.
 */
gboolean gts_surface_is_manifold (GtsSurface * s)
{
  gboolean is_manifold = TRUE;
  gpointer data[2];

  g_return_val_if_fail (s != NULL, FALSE);

  data[0] = &is_manifold;
  data[1] = s;
  gts_surface_foreach_edge (s, (GtsFunc) manifold_foreach_edge, data);
  return is_manifold;
}

static void closed_foreach_edge (GtsEdge * e, gpointer * data)
{
  gboolean * is_closed = data[0];

  if (*is_closed) {
    if (gts_edge_face_number (e, data[1]) != 2)
      *is_closed = FALSE;
  }
}

/**
 * gts_surface_is_closed:
 * @s: a #GtsSurface.
 *
 * Returns: %TRUE if @s is a closed surface, %FALSE otherwise. Note that a
 * closed surface is also a manifold.
 */
gboolean gts_surface_is_closed (GtsSurface * s)
{
  gboolean is_closed = TRUE;
  gpointer data[2];

  g_return_val_if_fail (s != NULL, FALSE);

  data[0] = &is_closed;
  data[1] = s;
  gts_surface_foreach_edge (s, (GtsFunc) closed_foreach_edge, data);
  return is_closed;
}

static void orientable_foreach_edge (GtsEdge * e, gpointer * data)
{
  gboolean * is_orientable = data[0];

  if (*is_orientable) {
    GtsSurface * surface = data[1];
    GtsFace * f1 = NULL, * f2 = NULL;
    GSList * i = e->triangles;
    while (i && *is_orientable) {
      GtsFace * f = i->data;
      if (GTS_IS_FACE (f) && gts_face_has_parent_surface (f, surface)) {
	if (!f1) f1 = f;
	else if (!f2) f2 = f;
	else *is_orientable = FALSE;
      }
      i = i->next;
    }
    if (f1 && f2 && !gts_triangles_are_compatible (GTS_TRIANGLE (f1), 
						   GTS_TRIANGLE (f2), e))
      *is_orientable = FALSE;
  }
}

/**
 * gts_surface_is_orientable:
 * @s: a #GtsSurface.
 *
 * Returns: %TRUE if all the faces of @s have compatible orientation
 * as checked by gts_faces_are_compatible(), %FALSE otherwise. Note that
 * an orientable surface is also a manifold.
 */
gboolean gts_surface_is_orientable (GtsSurface * s)
{
  gboolean is_orientable = TRUE;
  gpointer data[2];

  g_return_val_if_fail (s != NULL, FALSE);

  data[0] = &is_orientable;
  data[1] = s;
  gts_surface_foreach_edge (s, (GtsFunc) orientable_foreach_edge, data);
  return is_orientable;
}

static void volume_foreach_face (GtsTriangle * t,
				 gdouble * volume)
{
  GtsVertex * va, * vb, * vc;
  GtsPoint * pa, * pb, * pc;

  gts_triangle_vertices (t, &va, &vb, &vc);
  pa = GTS_POINT (va);
  pb = GTS_POINT (vb);
  pc = GTS_POINT (vc);
  
  *volume += (pa->x * (pb->y * pc->z - pb->z * pc->y) +
	      pb->x * (pc->y * pa->z - pc->z * pa->y) +
	      pc->x * (pa->y * pb->z - pa->z * pb->y));
}

/**
 * gts_surface_volume:
 * @s: a #GtsSurface.
 *
 * Returns: the signed volume of the domain bounded by the surface @s. It
 * makes sense only if @s is a closed and orientable manifold.
 */
gdouble gts_surface_volume (GtsSurface * s)
{
  gdouble volume = 0.0;

  g_return_val_if_fail (s != NULL, 0.0);

  gts_surface_foreach_face (s, (GtsFunc) volume_foreach_face, &volume);

  return volume/6.;
}

static void center_of_mass_foreach_face (GtsTriangle * t,
					 gpointer * data)
{
  GtsVertex * v1, * v2, * v3;
  GtsPoint * p1, * p2, * p3;
  gdouble x1, y1, z1, x2, y2, z2, nx, ny, nz;
  gdouble * volume = data[0];
  gdouble * cm = data[1];

  gts_triangle_vertices (t, &v1, &v2, &v3);
  p1 = GTS_POINT (v1);
  p2 = GTS_POINT (v2);
  p3 = GTS_POINT (v3);

  x1 = p2->x - p1->x;
  y1 = p2->y - p1->y;
  z1 = p2->z - p1->z;

  x2 = p3->x - p1->x;
  y2 = p3->y - p1->y;
  z2 = p3->z - p1->z;
  
  nx = y1*z2 - z1*y2;
  ny = z1*x2 - x1*z2;
  nz = x1*y2 - y1*x2;

  cm[0] += nx*(p1->x*p1->x + p2->x*p2->x + p3->x*p3->x + 
	       p1->x*p2->x + p1->x*p3->x + p2->x*p3->x);
  cm[1] += ny*(p1->y*p1->y + p2->y*p2->y + p3->y*p3->y + 
	       p1->y*p2->y + p1->y*p3->y + p2->y*p3->y);
  cm[2] += nz*(p1->z*p1->z + p2->z*p2->z + p3->z*p3->z + 
	       p1->z*p2->z + p1->z*p3->z + p2->z*p3->z);

  *volume += nx*(p1->x + p2->x + p3->x);
}


/**
 * gts_surface_center_of_mass:
 * @s: a #GtsSurface.
 * @cm: a #GtsVector.
 *
 * Fills @cm with the coordinates of the center of mass of @s.
 *
 * Returns: the signed volume of the domain bounded by the surface @s.
 */
gdouble gts_surface_center_of_mass (GtsSurface * s,
				    GtsVector cm)
{
  gdouble volume = 0.;
  gpointer data[2];

  g_return_val_if_fail (s != NULL, 0.0);

  data[0] = &volume;
  data[1] = &(cm[0]);
  cm[0] = cm[1] = cm[2] = 0.;
  gts_surface_foreach_face (s, (GtsFunc) center_of_mass_foreach_face, data);
  
  if (volume != 0.) {
    cm[0] /= 4.*volume;
    cm[1] /= 4.*volume;
    cm[2] /= 4.*volume;
  }

  return volume/6.;
}

static void center_of_area_foreach_face (GtsTriangle * t,
					 gpointer * data)
{
  GtsVertex * v1, * v2, * v3;
  GtsPoint * p1, * p2, * p3;
  gdouble a;
  gdouble * area = data[0];
  gdouble * cm = data[1];

  gts_triangle_vertices (t, &v1, &v2, &v3);
  p1 = GTS_POINT (v1);
  p2 = GTS_POINT (v2);
  p3 = GTS_POINT (v3);

  a = gts_triangle_area (t);
  cm[0] += a*(p1->x + p2->x + p3->x);
  cm[1] += a*(p1->y + p2->y + p3->y);
  cm[2] += a*(p1->z + p2->z + p3->z);
  *area += a;
}


/**
 * gts_surface_center_of_area:
 * @s: a #GtsSurface.
 * @cm: a #GtsVector.
 *
 * Fills @cm with the coordinates of the center of area of @s.
 *
 * Returns: the area of surface @s.
 */
gdouble gts_surface_center_of_area (GtsSurface * s,
				    GtsVector cm)
{
  gdouble area = 0.;
  gpointer data[2];

  g_return_val_if_fail (s != NULL, 0.0);

  data[0] = &area;
  data[1] = &(cm[0]);
  cm[0] = cm[1] = cm[2] = 0.;
  gts_surface_foreach_face (s, (GtsFunc) center_of_area_foreach_face, data);
  
  if (area != 0.) {
    cm[0] /= 3.*area;
    cm[1] /= 3.*area;
    cm[2] /= 3.*area;
  }

  return area;
}

static void number_foreach (gpointer data, guint * n)
{
  (*n)++;
}

/**
 * gts_surface_vertex_number:
 * @s: a #GtsSurface.
 *
 * Returns: the number of vertices of @s.
 */
guint gts_surface_vertex_number (GtsSurface * s)
{
  guint n = 0;

  g_return_val_if_fail (s != NULL, 0);

  gts_surface_foreach_vertex (s, (GtsFunc) number_foreach, &n);

  return n;
}

/**
 * gts_surface_edge_number:
 * @s: a #GtsSurface.
 *
 * Returns: the number of edges of @s.
 */
guint gts_surface_edge_number (GtsSurface * s)
{
  guint n = 0;

  g_return_val_if_fail (s != NULL, 0);

  gts_surface_foreach_edge (s, (GtsFunc) number_foreach, &n);

  return n;
}

/**
 * gts_surface_face_number:
 * @s: a #GtsSurface.
 *
 * Returns: the number of faces of @s
 */
guint gts_surface_face_number (GtsSurface * s)
{
  g_return_val_if_fail (s != NULL, 0);

#ifdef USE_SURFACE_BTREE
  return g_tree_nnodes (s->faces);
#else /* not USE_SURFACE_BTREE */
  return g_hash_table_size (s->faces);
#endif /* not USE_SURFACE_BTREE */
}

static void build_list_face (GtsTriangle * t, GSList ** list)
{
  *list = g_slist_prepend (*list, gts_bbox_triangle (gts_bbox_class (), t));
}

static void build_list_boundary (GtsEdge * e, GSList ** list)
{
  if (gts_edge_is_boundary (e, NULL))
    *list = g_slist_prepend (*list, gts_bbox_segment (gts_bbox_class (),
						      GTS_SEGMENT (e)));
}

/**
 * gts_surface_distance:
 * @s1: a #GtsSurface.
 * @s2: a #GtsSurface.
 * @delta: a spatial increment defined as the percentage of the diagonal
 * of the bounding box of @s2.
 * @face_range: a #GtsRange.
 * @boundary_range: a #GtsRange.
 *
 * Using the gts_bb_tree_surface_distance() and
 * gts_bb_tree_surface_boundary_distance() functions fills @face_range
 * and @boundary_range with the min, max and average Euclidean
 * (minimum) distances between the faces of @s1 and the faces of @s2
 * and between the boundary edges of @s1 and @s2.  
 */
void gts_surface_distance (GtsSurface * s1, GtsSurface * s2, gdouble delta,
			   GtsRange * face_range, GtsRange * boundary_range)
{
  GNode * face_tree, * boundary_tree;
  GSList * bboxes;

  g_return_if_fail (s1 != NULL);
  g_return_if_fail (s2 != NULL);
  g_return_if_fail (delta > 0. && delta < 1.);
  g_return_if_fail (face_range != NULL);
  g_return_if_fail (boundary_range != NULL);

  bboxes = NULL;
  gts_surface_foreach_face (s2, (GtsFunc) build_list_face, &bboxes);
  if (bboxes != NULL) {
    face_tree = gts_bb_tree_new (bboxes);
    g_slist_free (bboxes);
    
    gts_bb_tree_surface_distance (face_tree, s1, 
			       (GtsBBoxDistFunc) gts_point_triangle_distance,
				  delta, face_range);
    gts_bb_tree_destroy (face_tree, TRUE);
    
    bboxes = NULL;
    gts_surface_foreach_edge (s2, (GtsFunc) build_list_boundary, &bboxes);
    if (bboxes != NULL) {
      boundary_tree = gts_bb_tree_new (bboxes);
      g_slist_free (bboxes);

      gts_bb_tree_surface_boundary_distance (boundary_tree,
	       s1, 
	       (GtsBBoxDistFunc) gts_point_segment_distance,
	       delta, boundary_range);
      gts_bb_tree_destroy (boundary_tree, TRUE);
    }
    else
      gts_range_reset (boundary_range);
  }
  else {
    gts_range_reset (face_range);
    gts_range_reset (boundary_range);
  }
}

static void surface_boundary (GtsEdge * e, gpointer * data)
{
  GSList ** list = data[0];

  if (gts_edge_is_boundary (e, data[1]))
    *list = g_slist_prepend (*list, e);
}

/**
 * gts_surface_boundary:
 * @surface: a #GtsSurface.
 *
 * Returns: a list of #GtsEdge boundary of @surface.
 */
GSList * gts_surface_boundary (GtsSurface * surface)
{
  GSList * list = NULL;
  gpointer data[2];

  g_return_val_if_fail (surface != NULL, NULL);

  data[0] = &list;
  data[1] = surface;
  gts_surface_foreach_edge (surface, (GtsFunc) surface_boundary, data);
  
  return list;
}

struct _GtsSurfaceTraverse {
  GtsFifo * q;
  GtsSurface * s;
};

/**
 * gts_surface_traverse_new:
 * @s: a #GtsSurface.
 * @f: a #GtsFace belonging to @s.
 *
 * Returns: a new #GtsSurfaceTraverse, initialized to start traversing
 * from face @f of surface @s.  
 */
GtsSurfaceTraverse * gts_surface_traverse_new (GtsSurface * s,
					       GtsFace * f)
{
  GtsSurfaceTraverse * t;

  g_return_val_if_fail (s != NULL, NULL);
  g_return_val_if_fail (f != NULL, NULL);
  g_return_val_if_fail (gts_face_has_parent_surface (f, s), NULL);
  
  t = g_malloc (sizeof (GtsSurfaceTraverse));
  t->q = gts_fifo_new ();
  t->s = s;
  GTS_OBJECT (f)->reserved = GUINT_TO_POINTER (1);
  gts_fifo_push (t->q, f);
  return t;
}

static void push_neighbor (GtsFace * v, gpointer * data)
{
  if (!GTS_OBJECT (v)->reserved) {
    GTS_OBJECT (v)->reserved = 
      GUINT_TO_POINTER (GPOINTER_TO_UINT (GTS_OBJECT (data[1])->reserved) + 1);
    gts_fifo_push (data[0], v);
  }
}

/**
 * gts_surface_traverse_next:
 * @t: a #GtsSurfaceTraverse.
 * @level: a pointer to a guint or %NULL.
 *
 * Returns: the next face of the traversal in breadth-first order or
 * %NULL if no faces are left. If @level if not %NULL, it is filled
 * with the level of the returned face (0 for the initial face, 1 for
 * its neighbors and so on).  
 */
GtsFace * gts_surface_traverse_next (GtsSurfaceTraverse * t,
				     guint * level)
{
  GtsFace * u;

  g_return_val_if_fail (t != NULL, NULL);

  u = gts_fifo_pop (t->q);
  if (u) {
    gpointer data[2];

    if (level)
      *level = GPOINTER_TO_UINT (GTS_OBJECT (u)->reserved);
    data[0] = t->q;
    data[1] = u;
    gts_face_foreach_neighbor (u, t->s, (GtsFunc) push_neighbor, data);
  }
  return u;
}

/**
 * gts_surface_traverse_destroy:
 * @t: a #GtsSurfaceTraverse.
 *
 * Frees all the memory allocated for @t.
 */
void gts_surface_traverse_destroy (GtsSurfaceTraverse * t)
{
  g_return_if_fail (t != NULL);

  gts_surface_foreach_face (t->s, (GtsFunc) gts_object_reset_reserved, NULL);
  gts_fifo_destroy (t->q);
  g_free (t);
}

static void traverse_manifold (GtsTriangle * t, GtsSurface * s)
{
  if (g_slist_length (GTS_FACE (t)->surfaces) > 1)
    return;

  gts_surface_add_face (s, GTS_FACE (t));
  if (g_slist_length (t->e1->triangles) == 2) {
    if (t->e1->triangles->data != t)
      traverse_manifold (t->e1->triangles->data, s);
    else
      traverse_manifold (t->e1->triangles->next->data, s);
  }
  if (g_slist_length (t->e2->triangles) == 2) {
    if (t->e2->triangles->data != t)
      traverse_manifold (t->e2->triangles->data, s);
    else
      traverse_manifold (t->e2->triangles->next->data, s);
  }
  if (g_slist_length (t->e3->triangles) == 2) {
    if (t->e3->triangles->data != t)
      traverse_manifold (t->e3->triangles->data, s);
    else
      traverse_manifold (t->e3->triangles->next->data, s);
  }
}

static void non_manifold_edges (GtsEdge * e, gpointer * data)
{
  GtsSurface * s = data[0];
  GSList ** non_manifold = data[1];

  if (gts_edge_face_number (e, s) > 2) {
    GSList * i = e->triangles;

    while (i) {
      if (gts_face_has_parent_surface (i->data, s) &&
	  !g_slist_find (*non_manifold, i->data))
	*non_manifold = g_slist_prepend (*non_manifold, i->data);
      i = i->next;
    }
  }
}

static void traverse_boundary (GtsEdge * e, gpointer * data)
{
  GtsSurface * orig = data[0];
  GSList ** components = data[1];
  GtsFace * f = gts_edge_is_boundary (e, orig);

  if (f != NULL && g_slist_length (f->surfaces) == 1) {
    GtsSurface * s = 
      gts_surface_new (GTS_SURFACE_CLASS (GTS_OBJECT (orig)->klass),
		       orig->face_class,
		       orig->edge_class,
		       orig->vertex_class);
    GSList * non_manifold = NULL, * i;
    gpointer data[2];

    *components = g_slist_prepend (*components, s);
    data[0] = s;
    data[1] = &non_manifold;
    traverse_manifold (GTS_TRIANGLE (f), s);

    gts_surface_foreach_edge (s, (GtsFunc) non_manifold_edges, data);
    i = non_manifold;
    while (i) {
      gts_surface_remove_face (s, i->data);
      i = i->next;
    }
    g_slist_free (non_manifold);
  }
}

static void traverse_remaining (GtsFace * f, gpointer * data)
{
  GtsSurface * orig = data[0];
  GSList ** components = data[1];

  if (g_slist_length (f->surfaces) == 1) {
    GtsSurface * s = 
      gts_surface_new (GTS_SURFACE_CLASS (GTS_OBJECT (orig)->klass),
		       orig->face_class,
		       orig->edge_class,
		       orig->vertex_class);
    GSList * non_manifold = NULL, * i;
    gpointer data[2];

    *components = g_slist_prepend (*components, s);
    data[0] = s;
    data[1] = &non_manifold;
    traverse_manifold (GTS_TRIANGLE (f), s);

    gts_surface_foreach_edge (s, (GtsFunc) non_manifold_edges, data);
    i = non_manifold;
    while (i) {
      gts_surface_remove_face (s, i->data);
      i = i->next;
    }
    g_slist_free (non_manifold);
  }
}

/**
 * gts_surface_split:
 * @s: a #GtsSurface.
 *
 * Splits a surface into connected and manifold components.
 * 
 * Returns: a list of new #GtsSurface.
 */
GSList * gts_surface_split (GtsSurface * s)
{
  gpointer data[2];
  GSList * components = NULL;

  g_return_val_if_fail (s != NULL, NULL);

  data[0] = s;
  data[1] = &components;

  /* boundary components */
  gts_surface_foreach_edge (s, (GtsFunc) traverse_boundary, data);

  /* remaining components */
  gts_surface_foreach_face (s, (GtsFunc) traverse_remaining, data);

  return components;
}
