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

/* GtsContainee */

static void containee_class_init (GtsContaineeClass * klass)
{
  klass->remove_container = NULL;
  klass->add_container = NULL;
  klass->foreach = NULL;
  klass->is_contained = NULL;
  klass->replace = NULL;
}

GtsContaineeClass * gts_containee_class (void)
{
  static GtsContaineeClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo containee_info = {
      "GtsContainee",
      sizeof (GtsContainee),
      sizeof (GtsContaineeClass),
      (GtsObjectClassInitFunc) containee_class_init,
      (GtsObjectInitFunc) NULL,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (gts_object_class (),
				  &containee_info);
  }

  return klass;
}

GtsContainee * gts_containee_new (GtsContaineeClass * klass)
{
  GtsContainee * object;

  object = GTS_CONTAINEE (gts_object_new (GTS_OBJECT_CLASS (klass)));

  return object;
}

gboolean gts_containee_is_contained (GtsContainee * item,
				     GtsContainer * c)
{
  g_return_val_if_fail (item != NULL, FALSE);
  g_return_val_if_fail (c != NULL, FALSE);

  if (GTS_CONTAINEE_CLASS (GTS_OBJECT (item)->klass)->is_contained)
    return
      (* GTS_CONTAINEE_CLASS (GTS_OBJECT (item)->klass)->is_contained) 
      (item, c);
  return FALSE;
}

void gts_containee_replace (GtsContainee * item,
			    GtsContainee * with)
{
  if (GTS_CONTAINEE_CLASS (GTS_OBJECT (item)->klass)->replace)
    (* GTS_CONTAINEE_CLASS (GTS_OBJECT (item)->klass)->replace) (item, with);
  if (GTS_CONTAINEE_CLASS (GTS_OBJECT (item)->klass)->foreach) {
    (* GTS_CONTAINEE_CLASS (GTS_OBJECT (item)->klass)->foreach) 
      (item, (GtsFunc) gts_container_add, with);
    (* GTS_CONTAINEE_CLASS (GTS_OBJECT (item)->klass)->foreach) 
      (item, (GtsFunc) gts_container_remove, item);
  }
}

/* GtsSListContainee */

static void slist_containee_destroy (GtsObject * object)
{
  GtsSListContainee * item = GTS_SLIST_CONTAINEE (object);
  GSList * i;

  i = item->containers;
  while (i) {
    GSList * next = i->next;

    gts_container_remove (i->data, GTS_CONTAINEE (item));
    i = next;
  }
  g_assert (item->containers == NULL);

  (* GTS_OBJECT_CLASS (gts_slist_containee_class ())->parent_class->destroy) 
    (object);
}

static void slist_containee_remove_container (GtsContainee * i, 
					      GtsContainer * c)
{
  GtsSListContainee * item = GTS_SLIST_CONTAINEE (i);
  item->containers = g_slist_remove (item->containers, c);
}

static void slist_containee_add_container (GtsContainee * i, 
					   GtsContainer * c)
{
  GtsSListContainee * item = GTS_SLIST_CONTAINEE (i);
  if (!g_slist_find (item->containers, c))
    item->containers = g_slist_prepend (item->containers, c);
}

static void slist_containee_foreach (GtsContainee * c,
				     GtsFunc func, 
				     gpointer data)
{
  GSList * i = GTS_SLIST_CONTAINEE (c)->containers;

  while (i) {
    GSList * next = i->next;
    
    (* func) (i->data, data);
    i = next;
  }
}

static gboolean slist_containee_is_contained (GtsContainee * i,
					      GtsContainer * c)
{
  return g_slist_find (GTS_SLIST_CONTAINEE (i)->containers, c) ? TRUE : FALSE;
}

static void slist_containee_class_init (GtsSListContaineeClass * klass)
{
  GTS_CONTAINEE_CLASS (klass)->remove_container = 
    slist_containee_remove_container;
  GTS_CONTAINEE_CLASS (klass)->add_container = 
    slist_containee_add_container;
  GTS_CONTAINEE_CLASS (klass)->foreach = 
    slist_containee_foreach;
  GTS_CONTAINEE_CLASS (klass)->is_contained = 
    slist_containee_is_contained;

  GTS_OBJECT_CLASS (klass)->destroy = slist_containee_destroy;
}

static void slist_containee_init (GtsSListContainee * object)
{
  object->containers = NULL;
}

GtsSListContaineeClass * gts_slist_containee_class (void)
{
  static GtsSListContaineeClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo slist_containee_info = {
      "GtsSListContainee",
      sizeof (GtsSListContainee),
      sizeof (GtsSListContaineeClass),
      (GtsObjectClassInitFunc) slist_containee_class_init,
      (GtsObjectInitFunc) slist_containee_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_containee_class ()),
				  &slist_containee_info);
  }

  return klass;
}

/* GtsContainer */

static void remove_container (GtsContainee * item, GtsContainer * c)
{
  if (GTS_CONTAINEE_CLASS (GTS_OBJECT (item)->klass)->remove_container)
    (* GTS_CONTAINEE_CLASS (GTS_OBJECT (item)->klass)->remove_container) 
      (item, c);
}

static void container_destroy (GtsObject * object)
{
  GtsContainer * c = GTS_CONTAINER (object);

  gts_container_foreach (c, (GtsFunc) remove_container, c);

  (* GTS_OBJECT_CLASS (gts_container_class ())->parent_class->destroy) 
    (object);
}

static void container_add (GtsContainer * c, GtsContainee * item)
{
  if (GTS_CONTAINEE_CLASS (GTS_OBJECT (item)->klass)->add_container)
    (* GTS_CONTAINEE_CLASS (GTS_OBJECT (item)->klass)->add_container)
      (item, c);
}

static void container_remove (GtsContainer * c, GtsContainee * item)
{
  if (GTS_CONTAINEE_CLASS (GTS_OBJECT (item)->klass)->remove_container)
    (* GTS_CONTAINEE_CLASS (GTS_OBJECT (item)->klass)->remove_container)
      (item, c);
}

static void container_clone_add (GtsContainee * item, GtsContainer * clone)
{
  gts_container_add (clone, item);
}

static void container_clone (GtsObject * clone, GtsObject * object)
{
  gts_object_init (clone, object->klass);
  gts_container_foreach (GTS_CONTAINER (object), 
			 (GtsFunc) container_clone_add, clone);
}

static void container_class_init (GtsContainerClass * klass)
{
  klass->add = container_add;
  klass->remove = container_remove;
  klass->foreach = NULL;
  klass->size = NULL;

  GTS_OBJECT_CLASS (klass)->destroy = container_destroy;
  GTS_OBJECT_CLASS (klass)->clone = container_clone;
}

GtsContainerClass * gts_container_class (void)
{
  static GtsContainerClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo container_info = {
      "GtsContainer",
      sizeof (GtsContainer),
      sizeof (GtsContainerClass),
      (GtsObjectClassInitFunc) container_class_init,
      (GtsObjectInitFunc) NULL,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = 
      gts_object_class_new (GTS_OBJECT_CLASS (gts_slist_containee_class ()), 
			    &container_info);
  }

  return klass;
}

GtsContainer * gts_container_new (GtsContainerClass * klass)
{
  GtsContainer * object;

  object = GTS_CONTAINER (gts_object_new (GTS_OBJECT_CLASS (klass)));

  return object;
}

void gts_container_add (GtsContainer * c,
			GtsContainee * item)
{
  g_return_if_fail (c != NULL);
  g_return_if_fail (item != NULL);

  g_assert (GTS_CONTAINER_CLASS (GTS_OBJECT (c)->klass)->add);
  (* GTS_CONTAINER_CLASS (GTS_OBJECT (c)->klass)->add) (c, item);
}

void gts_container_remove (GtsContainer * c,
			   GtsContainee * item)
{
  g_return_if_fail (c != NULL);
  g_return_if_fail (item != NULL);

  g_assert (GTS_CONTAINER_CLASS (GTS_OBJECT (c)->klass)->remove);
  (* GTS_CONTAINER_CLASS (GTS_OBJECT (c)->klass)->remove) (c, item);
}

void gts_container_foreach (GtsContainer * c,
			    GtsFunc func,
			    gpointer data)
{
  g_return_if_fail (c != NULL);
  g_return_if_fail (func != NULL);

  if (GTS_CONTAINER_CLASS (GTS_OBJECT (c)->klass)->foreach)
    (* GTS_CONTAINER_CLASS (GTS_OBJECT (c)->klass)->foreach) (c, func, data);
}

guint gts_container_size (GtsContainer * c)
{
  g_return_val_if_fail (c != NULL, 0);

  if (GTS_CONTAINER_CLASS (GTS_OBJECT (c)->klass)->size)
    return (* GTS_CONTAINER_CLASS (GTS_OBJECT (c)->klass)->size) (c);
  return 0;
}

/* GtsHashContainer */

static void hash_container_destroy (GtsObject * object)
{
  GHashTable * items = GTS_HASH_CONTAINER (object)->items;

  (* GTS_OBJECT_CLASS (gts_hash_container_class ())->parent_class->destroy) 
    (object);

  g_hash_table_destroy (items);
}

static void hash_container_add (GtsContainer * c, GtsContainee * item)
{
  g_return_if_fail (GTS_HASH_CONTAINER (c)->frozen == FALSE);

  g_hash_table_insert (GTS_HASH_CONTAINER (c)->items, item, NULL);

  (* GTS_CONTAINER_CLASS (GTS_OBJECT_CLASS (gts_hash_container_class ())->parent_class)->add) (c, item);
}

static void hash_container_remove (GtsContainer * c, GtsContainee * item)
{
  g_return_if_fail (GTS_HASH_CONTAINER (c)->frozen == FALSE);

  g_hash_table_remove (GTS_HASH_CONTAINER (c)->items, item);

  (* GTS_CONTAINER_CLASS (GTS_OBJECT_CLASS (gts_hash_container_class ())->parent_class)->remove) (c, item);
}

static void hash_foreach (GtsContainee * item, 
			  gpointer item_data, 
			  gpointer * info)
{
  (* ((GtsFunc) info[0])) (item, info[1]);
}

static void hash_container_foreach (GtsContainer * c, 
				    GtsFunc func, 
				    gpointer data)
{
  gpointer info[2];
  
  info[0] = func;
  info[1] = data;
  /* prevent removing or adding items */
  GTS_HASH_CONTAINER (c)->frozen = TRUE;
  g_hash_table_foreach (GTS_HASH_CONTAINER (c)->items, 
			(GHFunc) hash_foreach, info);
  GTS_HASH_CONTAINER (c)->frozen = FALSE;
}

static guint hash_container_size (GtsContainer * c)
{
  return g_hash_table_size (GTS_HASH_CONTAINER (c)->items);
}

static void hash_container_class_init (GtsHashContainerClass * klass)
{
  GTS_CONTAINER_CLASS (klass)->add = hash_container_add;
  GTS_CONTAINER_CLASS (klass)->remove = hash_container_remove;
  GTS_CONTAINER_CLASS (klass)->foreach = hash_container_foreach;
  GTS_CONTAINER_CLASS (klass)->size = hash_container_size;

  GTS_OBJECT_CLASS (klass)->destroy = hash_container_destroy;
}

static void hash_container_init (GtsHashContainer * object)
{
  object->items = g_hash_table_new (NULL, NULL);
  object->frozen = FALSE;
}

GtsHashContainerClass * gts_hash_container_class (void)
{
  static GtsHashContainerClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo hash_container_info = {
      "GtsHashContainer",
      sizeof (GtsHashContainer),
      sizeof (GtsHashContainerClass),
      (GtsObjectClassInitFunc) hash_container_class_init,
      (GtsObjectInitFunc) hash_container_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_container_class ()),
				  &hash_container_info);
  }

  return klass;
}

/* GtsSListContainer */

static void slist_container_destroy (GtsObject * object)
{
  GSList * items = GTS_SLIST_CONTAINER (object)->items;

  (* GTS_OBJECT_CLASS (gts_slist_container_class ())->parent_class->destroy) 
    (object);

  g_slist_free (items);
}

static void slist_container_add (GtsContainer * c, GtsContainee * item)
{
  g_return_if_fail (GTS_SLIST_CONTAINER (c)->frozen == FALSE);

  if (!g_slist_find (GTS_SLIST_CONTAINER (c)->items, item))
    GTS_SLIST_CONTAINER (c)->items = 
      g_slist_prepend (GTS_SLIST_CONTAINER (c)->items, item);

  (* GTS_CONTAINER_CLASS (GTS_OBJECT_CLASS (gts_slist_container_class ())->parent_class)->add) (c, item);
}

static void slist_container_remove (GtsContainer * c, GtsContainee * item)
{
  g_return_if_fail (GTS_SLIST_CONTAINER (c)->frozen == FALSE);

  GTS_SLIST_CONTAINER (c)->items = 
      g_slist_remove (GTS_SLIST_CONTAINER (c)->items, item);

  (* GTS_CONTAINER_CLASS (GTS_OBJECT_CLASS (gts_slist_container_class ())->parent_class)->remove) (c, item);
}

static void slist_container_foreach (GtsContainer * c, 
				     GtsFunc func, 
				     gpointer data)
{
  GSList * i;

  i = GTS_SLIST_CONTAINER (c)->items;
  while (i) {
    GSList * next = i->next;

    (* func) (i->data, data);
    i = next;
  }
}

static guint slist_container_size (GtsContainer * c)
{
  return g_slist_length (GTS_SLIST_CONTAINER (c)->items);
}

static void slist_container_class_init (GtsSListContainerClass * klass)
{
  GTS_CONTAINER_CLASS (klass)->add = slist_container_add;
  GTS_CONTAINER_CLASS (klass)->remove = slist_container_remove;
  GTS_CONTAINER_CLASS (klass)->foreach = slist_container_foreach;
  GTS_CONTAINER_CLASS (klass)->size = slist_container_size;

  GTS_OBJECT_CLASS (klass)->destroy = slist_container_destroy;
}

static void slist_container_init (GtsSListContainer * object)
{
  object->items = NULL;
  object->frozen = FALSE;
}

GtsSListContainerClass * gts_slist_container_class (void)
{
  static GtsSListContainerClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo slist_container_info = {
      "GtsSListContainer",
      sizeof (GtsSListContainer),
      sizeof (GtsSListContainerClass),
      (GtsObjectClassInitFunc) slist_container_class_init,
      (GtsObjectInitFunc) slist_container_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (GTS_OBJECT_CLASS (gts_container_class ()),
				  &slist_container_info);
  }

  return klass;
}
