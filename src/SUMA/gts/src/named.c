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

#include <string.h>
#include "gts.h"

static void nvertex_read (GtsObject ** po, GtsFile * fp)
{
  if ((*po)->klass->parent_class->read)
    (* (*po)->klass->parent_class->read) (po, fp);

  if (fp->type != '\n' && fp->type != GTS_ERROR) {
    strncpy (GTS_NVERTEX (*po)->name, fp->token->str, GTS_NAME_LENGTH);
    gts_file_next_token (fp);
  }
}

static void nvertex_write (GtsObject * o, FILE * fptr)
{
  GtsNVertex * nv = GTS_NVERTEX (o);

  (* o->klass->parent_class->write) (o, fptr);
  if (nv->name[0] != '\0')
    fprintf (fptr, " %s", nv->name);
}

static void nvertex_class_init (GtsNVertexClass * klass)
{
  GTS_OBJECT_CLASS (klass)->read = nvertex_read;
  GTS_OBJECT_CLASS (klass)->write = nvertex_write;
}

static void nvertex_init (GtsNVertex * nvertex)
{
  nvertex->name[0] = '\0';
}

/**
 * gts_nvertex_class:
 *
 * Returns: the #GtsNVertexClass.
 */
GtsNVertexClass * gts_nvertex_class (void)
{
  static GtsNVertexClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo nvertex_info = {
      "GtsNVertex",
      sizeof (GtsNVertex),
      sizeof (GtsNVertexClass),
      (GtsObjectClassInitFunc) nvertex_class_init,
      (GtsObjectInitFunc) nvertex_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_vertex_class ()), 
				  &nvertex_info);
  }

  return klass;
}

static void nedge_read (GtsObject ** po, GtsFile * fp)
{
  if (fp->type != GTS_STRING) {
    gts_file_error (fp, "expecting a string (name)");
    return;
  }
  strncpy (GTS_NEDGE (*po)->name, fp->token->str, GTS_NAME_LENGTH);
  gts_file_next_token (fp);
}

static void nedge_write (GtsObject * o, FILE * fptr)
{
  GtsNEdge * ne = GTS_NEDGE (o);

  if (ne->name[0] != '\0')
    fprintf (fptr, " %s", ne->name);
}

static void nedge_class_init (GtsNEdgeClass * klass)
{
  GTS_OBJECT_CLASS (klass)->read = nedge_read;
  GTS_OBJECT_CLASS (klass)->write = nedge_write;
}

static void nedge_init (GtsNEdge * nedge)
{
  nedge->name[0] = '\0';
}

/**
 * gts_nedge_class:
 *
 * Returns: the #GtsNEdgeClass.
 */
GtsNEdgeClass * gts_nedge_class (void)
{
  static GtsNEdgeClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo nedge_info = {
      "GtsNEdge",
      sizeof (GtsNEdge),
      sizeof (GtsNEdgeClass),
      (GtsObjectClassInitFunc) nedge_class_init,
      (GtsObjectInitFunc) nedge_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_edge_class ()), 
				  &nedge_info);
  }

  return klass;
}

static void nface_read (GtsObject ** po, GtsFile * fp)
{
  if (fp->type != GTS_STRING) {
    gts_file_error (fp, "expecting a string (name)");
    return;
  }
  strncpy (GTS_NFACE (*po)->name, fp->token->str, GTS_NAME_LENGTH);
  gts_file_next_token (fp);
}

static void nface_write (GtsObject * o, FILE * fptr)
{
  GtsNFace * nf = GTS_NFACE (o);

  if (nf->name[0] != '\0')
    fprintf (fptr, " %s", GTS_NFACE (o)->name);
}

static void nface_class_init (GtsNFaceClass * klass)
{
  GTS_OBJECT_CLASS (klass)->read = nface_read;
  GTS_OBJECT_CLASS (klass)->write = nface_write;
}

static void nface_init (GtsNFace * nface)
{
  nface->name[0] = '\0';
}

/**
 * gts_nface_class:
 *
 * Returns: the #GtsNFaceClass.
 */
GtsNFaceClass * gts_nface_class (void)
{
  static GtsNFaceClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo nface_info = {
      "GtsNFace",
      sizeof (GtsNFace),
      sizeof (GtsNFaceClass),
      (GtsObjectClassInitFunc) nface_class_init,
      (GtsObjectInitFunc) nface_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_face_class ()), 
				  &nface_info);
  }

  return klass;
}
