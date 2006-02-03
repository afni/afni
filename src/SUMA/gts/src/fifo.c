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

struct _GtsFifo {
  GList * head;
  GList * tail;
};

/**
 * gts_fifo_new:
 *
 * Returns: a new #GtsFifo.
 */
GtsFifo * gts_fifo_new ()
{
  GtsFifo * fifo = g_malloc (sizeof (GtsFifo));

  fifo->head = fifo->tail = NULL;
  return fifo;
}

/**
 * gts_fifo_write:
 * @fifo: a #GtsFifo.
 * @fp: a file pointer.
 *
 * Writes the content of @fifo in @fp.
 */
void gts_fifo_write (GtsFifo * fifo, FILE * fp)
{
  GList * i;

  g_return_if_fail (fifo != NULL);
  g_return_if_fail (fp != NULL);

  fprintf (fp, "[");
  i = fifo->head;
  while (i) {
    fprintf (fp, "%p ", i->data);
    i = i->next;
  }
  fprintf (fp, "]");
}

/**
 * gts_fifo_push:
 * @fifo: a #GtsFifo.
 * @data: data to add to @fifo.
 *
 * Push @data into @fifo.
 */
void gts_fifo_push (GtsFifo * fifo, gpointer data)
{
  g_return_if_fail (fifo != NULL);

  fifo->head = g_list_prepend (fifo->head, data);
  if (fifo->tail == NULL)
    fifo->tail = fifo->head;
}

/**
 * gts_fifo_pop:
 * @fifo: a #GtsFifo.
 *
 * Removes the first element from @fifo.
 *
 * Returns: the first element in @fifo or %NULL if @fifo is empty.
 */
gpointer gts_fifo_pop (GtsFifo * fifo)
{
  gpointer data;
  GList * tail;

  g_return_val_if_fail (fifo != NULL, NULL);

  if (fifo->tail == NULL)
    return NULL;
  tail = fifo->tail->prev;
  data = fifo->tail->data;
  fifo->head = g_list_remove_link (fifo->head, fifo->tail);
  g_list_free_1 (fifo->tail);
  fifo->tail = tail;
  return data;
}

/**
 * gts_fifo_top:
 * @fifo: a #GtsFifo.
 *
 * Returns: the first element in @fifo or %NULL if @fifo is empty.
 */
gpointer gts_fifo_top (GtsFifo * fifo)
{
  g_return_val_if_fail (fifo != NULL, NULL);

  if (fifo->tail == NULL)
    return NULL;
  return fifo->tail->data;
}

/**
 * gts_fifo_size:
 * @fifo: a #GtsFifo.
 *
 * Returns: the number of elements in @fifo.
 */
guint gts_fifo_size (GtsFifo * fifo)
{
  g_return_val_if_fail (fifo != NULL, 0);

  return g_list_length (fifo->head);
}

/**
 * gts_fifo_destroy:
 * @fifo: a #GtsFifo.
 *
 * Frees all the memory allocated for @fifo.
 */
void gts_fifo_destroy (GtsFifo * fifo)
{
  g_return_if_fail (fifo != NULL);
  g_list_free (fifo->head);
  g_free (fifo);
}

/**
 * gts_fifo_is_empty:
 * @fifo: a #GtsFifo.
 * 
 * Returns: %TRUE if @fifo is empty, %FALSE otherwise.
 */
gboolean gts_fifo_is_empty (GtsFifo * fifo)
{
  g_return_val_if_fail (fifo != NULL, TRUE);

  return (fifo->head == NULL);
}

/**
 * gts_fifo_foreach:
 * @fifo: a #GtsFifo.
 * @func: a #GtsFunc.
 * @data: user data to be passed to @func.
 *
 * Calls @func in order for each item in @fifo, passing @data.
 */
void gts_fifo_foreach (GtsFifo * fifo, GtsFunc func, gpointer data)
{
  GList * i;

  g_return_if_fail (fifo != NULL);
  g_return_if_fail (func != NULL);

  i = fifo->tail;
  while (i) {
    (* func) (i->data, data);
    i = i->prev;
  }
}

/**
 * gts_fifo_reverse:
 * @fifo: a #GtsFifo.
 *
 * Reverses the order of elements in @fifo.
 */
void gts_fifo_reverse (GtsFifo * fifo)
{
  g_return_if_fail (fifo != NULL);

  fifo->tail = fifo->head;
  fifo->head = g_list_reverse (fifo->head);
}
