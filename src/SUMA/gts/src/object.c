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
#include "gts-private.h"

static GHashTable * class_table = NULL;

static void gts_object_class_init (GtsObjectClass * klass,
				   GtsObjectClass * parent_class)
{
  if (parent_class) {
    gts_object_class_init (klass, parent_class->parent_class);
    if (parent_class->info.class_init_func)
      (*parent_class->info.class_init_func) (klass);
  }
}

/**
 * gts_object_class_new:
 * @parent_class: a #GtsObjectClass.
 * @info: a #GtsObjectClassInfo, description of the new class to create.
 *
 * Returns: a new #GtsObjectClass derived from @parent_class and described by
 * @info.
 */
gpointer gts_object_class_new (GtsObjectClass * parent_class,
			       GtsObjectClassInfo * info)
{
  GtsObjectClass * klass;

  g_return_val_if_fail (info != NULL, NULL);
  g_return_val_if_fail (parent_class == NULL ||
			info->object_size >= parent_class->info.object_size,
			NULL);
  g_return_val_if_fail (parent_class == NULL ||
			info->class_size >= parent_class->info.class_size,
			NULL);

  klass = g_malloc0 (info->class_size);
  klass->info = *info;
  klass->parent_class = parent_class;
  gts_object_class_init (klass, klass);

  if (!class_table)
    class_table = g_hash_table_new (g_str_hash, g_str_equal);
  g_hash_table_insert (class_table, klass->info.name, klass);

  return klass;
}

/**
 * gts_object_class_from_name:
 * @name: the name of a #GtsObjectClass.
 *
 * Returns: the #GtsObjectClass with name @name or %NULL if it hasn't been 
 * instantiated yet.
 */
GtsObjectClass * gts_object_class_from_name (gchar * name)
{
  g_return_val_if_fail (name != NULL, NULL);

  if (!class_table)
    return NULL;
  return g_hash_table_lookup (class_table, name);
}

static void object_destroy (GtsObject * object)
{
#ifdef DEBUG_IDENTITY
#ifdef DEBUG_LEAKS
  fprintf (stderr, "destroy %s %p->%d\n", 
	   object->klass->info.name,
	   object, 
	   id (object));
#endif
  id_remove (object);
#endif
  object->klass = NULL;
  g_free (object);
}

static void object_clone (GtsObject * clone, GtsObject * object)
{
  memcpy (clone, object, object->klass->info.object_size);
  clone->reserved = NULL;
}

static void object_class_init (GtsObjectClass * klass)
{
  klass->clone = object_clone;
  klass->destroy = object_destroy;
  klass->read = NULL;
  klass->write = NULL;
  klass->color = NULL;  
  klass->attributes = NULL;
}

static void object_init (GtsObject * object)
{
  object->reserved = NULL;
  object->flags = 0;
}

/**
 * gts_object_class:
 *
 * Returns: the #GtsObjectClass.
 */
GtsObjectClass * gts_object_class (void)
{
  static GtsObjectClass * klass = NULL;

  if (klass == NULL) {
    GtsObjectClassInfo object_info = {
      "GtsObject",
      sizeof (GtsObject),
      sizeof (GtsObjectClass),
      (GtsObjectClassInitFunc) object_class_init,
      (GtsObjectInitFunc) object_init,
      (GtsArgSetFunc) NULL,
      (GtsArgGetFunc) NULL
    };
    klass = gts_object_class_new (NULL, &object_info);
  }

  return klass;
}

#ifndef G_CAN_INLINE
/**
 * gts_object_is_from_class:
 * @object: a #GtsObject.
 * @klass: a #GtsObjectClass.
 *
 * Returns: @object if @object is of class @klass or of a class derived from
 * @klass, %NULL otherwise.
 */
gpointer gts_object_is_from_class (gpointer object,
				   gpointer klass)
{
  GtsObjectClass * c;

  g_return_val_if_fail (klass != NULL, NULL);

  if (object == NULL)
    return NULL;

  c = ((GtsObject *) object)->klass;

  g_return_val_if_fail (c != NULL, NULL);

  while (c) {
    if (c == klass)
      return object;
    c = c->parent_class;
  }

  return NULL;
}

/**
 * gts_object_class_is_from_class:
 * @klass: a #GtsObjectClass.
 * @from: a #GtsObjectClass.
 *
 * Returns: @klass if @klass is equal to @from or if @klass is derived
 * from @from, %NULL otherwise.
 */
gpointer gts_object_class_is_from_class (gpointer klass,
					 gpointer from)
{
  GtsObjectClass * c;

  g_return_val_if_fail (klass != NULL, NULL);
  g_return_val_if_fail (from != NULL, NULL);

  c = (GtsObjectClass *) klass;
  while (c) {
    if (c == from)
      return klass;
    c = c->parent_class;
  }

  return NULL;
}
#endif /* not G_CAN_INLINE */

/**
 * gts_object_check_cast:
 * @object: a #GtsObject.
 * @klass: a #GtsObjectClass.
 *
 * Returns: @object while emitting warnings if @object is not of class @klass.
 */
gpointer gts_object_check_cast (gpointer object, 
				gpointer klass)
{
  if (!object) {
    g_warning ("invalid cast from (NULL) pointer to `%s'",
	       GTS_OBJECT_CLASS (klass)->info.name);
    return object;
  }
  if (!((GtsObject *) object)->klass) {
    g_warning ("invalid unclassed pointer in cast to `%s'",
	       GTS_OBJECT_CLASS (klass)->info.name);
    return object;
  }
  if (!gts_object_is_from_class (object, klass)) {
    g_warning ("invalid cast from `%s' to `%s'",
	       ((GtsObject *) object)->klass->info.name,
	       GTS_OBJECT_CLASS (klass)->info.name);
    return object;
  }
  return object;
}

/**
 * gts_object_class_check_cast:
 * @klass: a #GtsObjectClass.
 * @from: a #GtsObjectClass.
 *
 * Returns: @klass while emitting warnings if @klass is not derived from
 * @from.
 */
gpointer gts_object_class_check_cast (gpointer klass, 
				      gpointer from)
{
  if (!klass) {
    g_warning ("invalid cast from (NULL) pointer to `%s'",
	       GTS_OBJECT_CLASS (from)->info.name);
    return klass;
  }
  if (!gts_object_class_is_from_class (klass, from)) {
    g_warning ("invalid cast from `%s' to `%s'",
	       GTS_OBJECT_CLASS (klass)->info.name,
	       GTS_OBJECT_CLASS (from)->info.name);
    return klass;
  }
  return klass;
}

/**
 * gts_object_init:
 * @object: a #GtsObject.
 * @klass: a #GtsObjectClass.
 *
 * Calls the init method of @klass with @object as argument. This is done 
 * recursively in the correct order (from the base class to the top). You
 * should rarely need this function as it is called automatically by the
 * constructor for each class.
 */
void gts_object_init (GtsObject * object, GtsObjectClass * klass)
{
  GtsObjectClass * parent_class;

  g_return_if_fail (object != NULL);
  g_return_if_fail (klass != NULL);

  parent_class = klass->parent_class;
  if (parent_class)
    gts_object_init (object, parent_class);
  if (klass->info.object_init_func)
    (*klass->info.object_init_func) (object);
}

/**
 * gts_object_new:
 * @klass: a #GtsObjectClass.
 *
 * Returns: a new initialized object of class @klass.
 */
GtsObject * gts_object_new (GtsObjectClass * klass)
{
  GtsObject * object;

  g_return_val_if_fail (klass != NULL, NULL);

  object = g_malloc0 (klass->info.object_size);
  object->klass = klass;
  gts_object_init (object, klass);

#ifdef DEBUG_IDENTITY
  id_insert (object);
#ifdef DEBUG_LEAKS
  fprintf (stderr, "new %s %p->%d\n", klass->info.name, 
	   object, 
	   id (object));
#endif
#endif

  return object;
}

/**
 * gts_object_clone:
 * @object: a #GtsObject.
 *
 * Calls the clone method of @object. The call to this function will fail
 * if no clone method exists for the given object.
 *
 * Returns: a new object clone of @object.
 */
GtsObject * gts_object_clone (GtsObject * object)
{
  GtsObject * clone;

  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->klass->clone, NULL);

  clone = g_malloc0 (object->klass->info.object_size);
  clone->klass = object->klass;
  object_init (clone);
  (* object->klass->clone) (clone, object);

#ifdef DEBUG_IDENTITY
  id_insert (clone);
#ifdef DEBUG_LEAKS
  fprintf (stderr, "clone %s %p->%d\n", clone->klass->info.name, 
	   clone, 
	   id (clone));
#endif
#endif

  return clone;
}

/**
 * gts_object_destroy:
 * @object: a #GtsObject.
 *
 * Calls the destroy method of @object, freeing all memory allocated for it.
 */
void gts_object_destroy (GtsObject * object)
{
  g_assert (object->klass->destroy);
  GTS_OBJECT_SET_FLAGS (object, GTS_DESTROYED);
  (* object->klass->destroy) (object);
}

/**
 * gts_object_reset_reserved:
 * @object: a #GtsObject.
 *
 * Reset the reserved field of @object.
 */
void gts_object_reset_reserved (GtsObject * object)
{
  g_return_if_fail (object != NULL);

  object->reserved = NULL;
}

/**
 * gts_object_attributes:
 * @object: a #GtsObject.
 * @from: a #GtsObject.
 *
 * Calls the attributes() method of @object using @from as source.
 */
void gts_object_attributes (GtsObject * object, GtsObject * from)
{
  g_return_if_fail (object != NULL);

  if (object->klass->attributes)
    (* object->klass->attributes) (object, from);
}

static void free_class (gchar * name, GtsObjectClass * klass)
{
  g_free (klass);
}

/**
 * gts_finalize:
 *
 * Free all the memory allocated by the object system of GTS. No other
 * GTS function can be called after this function has been called.
 */
void gts_finalize (void)
{
  if (class_table) {
    g_hash_table_foreach (class_table, (GHFunc) free_class, NULL);
    g_hash_table_destroy (class_table);
    class_table = NULL;
  }
}
