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
#include <string.h>

#include "gts.h"
#include "gts-private.h"
#include "config.h"

const guint gts_major_version = GTS_MAJOR_VERSION;
const guint gts_minor_version = GTS_MINOR_VERSION;
const guint gts_micro_version = GTS_MICRO_VERSION;
const guint gts_interface_age = GTS_INTERFACE_AGE;
const guint gts_binary_age = GTS_BINARY_AGE;

static gboolean char_in_string (char c, const char * s)
{
  while (*s != '\0')
    if (*(s++) == c)
      return TRUE;
  return FALSE;
}

/**
 * gts_file_new:
 * @fp: a file pointer.
 *
 * Returns: a new #GtsFile.
 */
GtsFile * gts_file_new (FILE * fp)
{
  GtsFile * f;

  g_return_val_if_fail (fp != NULL, NULL);

  f = g_malloc (sizeof (GtsFile));
  f->fp = fp;
  f->curline = 1;
  f->curpos = 0;
  f->token = g_string_new ("");
  f->type = '\0';
  f->error = NULL;
  f->next_token = '\0';

  f->scope = f->scope_max = 0;
  f->delimiters = g_strdup (" \t");
  f->comments = g_strdup (GTS_COMMENTS);
  f->tokens = g_strdup ("\n{}()=");

  gts_file_next_token (f);

  return f;
}

/**
 * gts_file_destroy:
 * @f: a #GtsFile.
 *
 * Frees all the memory allocated for @f.
 */
void gts_file_destroy (GtsFile * f)
{
  g_return_if_fail (f != NULL);

  g_free (f->delimiters);
  g_free (f->comments);
  g_free (f->tokens);
  if (f->error)
    g_free (f->error);
  g_string_free (f->token, TRUE);
  g_free (f);
}

/**
 * gts_file_verror:
 * @f: a @GtsFile.
 * @format: the standard sprintf() format string.
 * @args: the list of parameters to insert into the format string.
 *
 * Sets the @error field of @f using g_strdup_vprintf().
 *
 * This function can be called only once and disables any other
 * operation on @f (gts_file_close() excepted).
 */
void gts_file_verror (GtsFile * f,
		      const gchar * format,
		      va_list args)
{
  g_return_if_fail (f != NULL);
  g_return_if_fail (format != NULL);

  g_assert (f->type != GTS_ERROR);
  f->error = g_strdup_vprintf (format, args);
  f->type = GTS_ERROR;
}

/**
 * gts_file_error:
 * @f: a @GtsFile.
 * @format: the standard sprintf() format string.
 * @...: the parameters to insert into the format string.
 *
 * Sets the @error field of @f using gts_file_verror().
 *
 * This function can be called only once and disables any other
 * operation on @f (gts_file_close() excepted).
 */
void gts_file_error (GtsFile * f,
		     const gchar * format,
		     ...)
{
  va_list args;

  g_return_if_fail (f != NULL);
  g_return_if_fail (format != NULL);

  va_start (args, format);  
  gts_file_verror (f, format, args);
  va_end (args);
}

/**
 * gts_file_getc :
 * @f: a #GtsFile.
 *
 * Returns: the next character in @f or EOF if the end of the file is
 * reached or if an error occured.
 */
gint gts_file_getc (GtsFile * f)
{
  gint c;

  g_return_val_if_fail (f != NULL, EOF);

  if (f->type == GTS_ERROR)
    return EOF;

  c = fgetc (f->fp); f->curpos++;
  switch (c) {
  case '\n': 
    f->curline++;
    f->curpos = 0; 
    break;
  case '{':
    f->scope++; 
    break;
  case '}':
    if (f->scope == 0) {
      f->line = f->curline;
      f->pos = f->curpos - 1;
      gts_file_error (f, "no matching opening brace");
      c = EOF;
    }
    else
      f->scope--;
  }
  return c;
}

/**
 * gts_file_read:
 * @f: a #GtsFile.
 * @ptr: a pointer.
 * @size: size of an element.
 * @nmemb: number of elements.
 *
 * Reads @nmemb elements of data, each @size bytes long, from @f,
 * storing them at the location given by @ptr.
 *
 * Returns: the number of elements read.
 */
guint gts_file_read (GtsFile * f, gpointer ptr, guint size, guint nmemb)
{
  guint i = 0;
  gchar * p = ptr;

  g_return_val_if_fail (f != NULL, 0);
  g_return_val_if_fail (ptr != NULL, 0);

  if (f->type == GTS_ERROR)
    return 0;

  while (i < nmemb) {
    guint j = 0;

    while (j < size) {
      gint c = fgetc (f->fp);

      if (c == EOF)
	return i;
      f->curpos++;
      if (c == '\n') {
	f->curline++;
	f->curpos = 0; 
      }
      *(p++) = c;
      j++;
    }
    i++;
  }

  return i;
}

/**
 * gts_file_getc_scope :
 * @f: a #GtsFile.
 *
 * Returns: the next character in @f in the scope defined by
 * @f->scope_max or EOF if the end of the file is reached or if an
 * error occured.
 */
gint gts_file_getc_scope (GtsFile * f)
{
  gint c;

  g_return_val_if_fail (f != NULL, EOF);

  if (f->type == GTS_ERROR)
    return EOF;
  
  if (f->scope <= f->scope_max)
    c = gts_file_getc (f);
  else {
    c = gts_file_getc (f);
    while (c != EOF && f->scope > f->scope_max)
      c = gts_file_getc (f);    
  }
  return c;
}

static void jump_to (GtsFile * f, gchar c)
{
  gint a;
  
  a = gts_file_getc_scope (f);
  while (a != EOF && a != c)
    a = gts_file_getc_scope (f);
}

/**
 * gts_file_next_token:
 * @f: a #GtsFile.
 *
 * Reads next token from @f and updates its @token and @delim fields.
 */
void gts_file_next_token (GtsFile * f)
{
  gint c;
  gboolean in_string = FALSE;

  g_return_if_fail (f != NULL);

  if (f->type == GTS_ERROR)
    return;
  f->token->str[0] = '\0';
  f->token->len = 0;
  if (f->next_token != '\0') {
    f->line = f->curline;
    f->pos = f->curpos - 1;
    g_string_append_c (f->token, f->next_token);
    f->type = f->next_token;
    f->next_token = '\0';
    return;
  }
  f->type = GTS_NONE;
  c = gts_file_getc_scope (f);
  while (c != EOF && (!in_string || !char_in_string (c, f->delimiters))) {
    if (char_in_string (c, f->comments))
      jump_to (f, '\n');
    else if (in_string) {
      if (char_in_string (c, f->tokens)) {
	f->next_token = c;
	break;
      }
      g_string_append_c (f->token, c);
    }
    else if (!char_in_string (c, f->delimiters)) {
      in_string = TRUE;
      f->line = f->curline;
      f->pos = f->curpos - 1;
      g_string_append_c (f->token, c);
      if (char_in_string (c, f->tokens)) {
	f->type = c;
	break;
      }
    }
    c = gts_file_getc_scope (f);
  }
  if (f->type == GTS_NONE && f->token->len > 0) {
    gchar * a;

    a = f->token->str;
    while (*a != '\0' && char_in_string (*a, "+-")) a++;
    if (*a == '\0') {
      f->type = GTS_STRING;
      return;
    }
    a = f->token->str;
    while (*a != '\0' && char_in_string (*a, "+-0123456789")) a++;
    if (*a == '\0') {
      f->type = GTS_INT;
      return;
    }
    a = f->token->str;
    while (*a != '\0' && char_in_string (*a, "+-eE.")) a++;
    if (*a == '\0') {
      f->type = GTS_STRING;
      return;
    }
    a = f->token->str;
    while (*a != '\0' && char_in_string (*a, "+-0123456789eE.")) a++;
    if (*a == '\0') {
      f->type = GTS_FLOAT;
      return;
    }
    a = f->token->str;
    if (!strncmp (a, "0x", 2) || 
	!strncmp (a, "-0x", 3) || 
	!strncmp (a, "+0x", 3)) {
      while (*a != '\0' && char_in_string (*a, "+-0123456789abcdefx")) a++;
      if (*a == '\0') {
	f->type = GTS_INT;
	return;
      }
      a = f->token->str;
      while (*a != '\0' && char_in_string (*a, "+-0123456789abcdefx.p")) a++;
      if (*a == '\0') {
	f->type = GTS_FLOAT;
	return;
      }
    }
    f->type = GTS_STRING;
  }
}

/**
 * gts_file_first_token_after:
 * @f: a #GtsFile.
 * @type: a #GtsTokenType.
 *
 * Finds and sets the first token of a type different from @type 
 * occuring after a token of type @type.
 */
void gts_file_first_token_after (GtsFile * f, GtsTokenType type)
{
  g_return_if_fail (f != NULL);

  while (f->type != GTS_ERROR && 
	 f->type != GTS_NONE &&
	 f->type != type)
    gts_file_next_token (f);
  while (f->type == type)
    gts_file_next_token (f);
}

/**
 * gts_file_assign_start:
 * @f: a #GtsFile.
 * @vars: a %GTS_NONE terminated array of #GtsFileVariable.
 *
 * Opens a block delimited by braces to read a list of optional
 * arguments specified by @vars.  
 *
 * If an error is encountered the @error field of @f is set.
 */
void gts_file_assign_start (GtsFile * f, GtsFileVariable * vars)
{
  GtsFileVariable * var;

  g_return_if_fail (f != NULL);
  g_return_if_fail (vars != NULL);

  var = vars;
  while (var->type != GTS_NONE)
    (var++)->set = FALSE;

  if (f->type != '{') {
    gts_file_error (f, "expecting an opening brace");
    return;
  }

  f->scope_max++;
  gts_file_next_token (f);
}

/**
 * gts_file_assign_next:
 * @f: a #GtsFile.
 * @vars: a %GTS_NONE terminated array of #GtsFileVariable.
 *
 * Assigns the next optional argument of @vars read from @f.
 *
 * Returns: the variable of @vars which has been assigned or %NULL if
 * no variable has been assigned (if an error has been encountered the
 * @error field of @f is set).  
 */
GtsFileVariable * gts_file_assign_next (GtsFile * f, GtsFileVariable * vars)
{
  GtsFileVariable * var;
  gboolean found = FALSE;

  g_return_val_if_fail (f != NULL, NULL);
  g_return_val_if_fail (vars != NULL, NULL);

  while (f->type == '\n')
    gts_file_next_token (f);
  if (f->type == '}') {
    f->scope_max--;
    gts_file_next_token (f);
    return NULL;
  }
  if (f->type == GTS_ERROR)
    return NULL;

  var = vars;
  while (f->type != GTS_ERROR && var->type != GTS_NONE && !found) {
    if (!strcmp (var->name, f->token->str)) {
      found = TRUE;
      if (var->unique && var->set)
	gts_file_error (f, "variable `%s' was already set at line %d:%d", 
			var->name, var->line, var->pos);
      else {
	var->line = f->line;
	var->pos = f->pos;
	gts_file_next_token (f);
	if (f->type != '=')
	  gts_file_error (f, "expecting `='");
	else {
	  var->set = TRUE;
	  switch (var->type) {
	  case GTS_FILE:
	    break;
	  case GTS_INT:
	    gts_file_next_token (f);
	    if (f->type != GTS_INT) {
	      gts_file_error (f, "expecting an integer");
	      var->set = FALSE;
	    }
	    else if (var->data)
	      *((gint *) var->data) = atoi (f->token->str); 
	    break;
	  case GTS_UINT:
	    gts_file_next_token (f);
	    if (f->type != GTS_INT) {
	      gts_file_error (f, "expecting an integer");
	      var->set = FALSE;
	    }
	    else if (var->data)
	      *((guint *) var->data) = atoi (f->token->str); 
	    break;
	  case GTS_FLOAT:
	    gts_file_next_token (f);
	    if (f->type != GTS_INT && f->type != GTS_FLOAT) {
	      gts_file_error (f, "expecting a number");
	      var->set = FALSE;
	    }
	    else if (var->data)
	      *((gfloat *) var->data) = atof (f->token->str); 
	    break;
	  case GTS_DOUBLE:
	    gts_file_next_token (f);
	    if (f->type != GTS_INT && f->type != GTS_FLOAT) {
	      gts_file_error (f, "expecting a number");
	      var->set = FALSE;
	    }
	    else if (var->data)
	      *((gdouble *) var->data) = atof (f->token->str); 
	    break;
	  case GTS_STRING:
	    gts_file_next_token (f);
	    if (f->type != GTS_INT && 
		f->type != GTS_FLOAT && 
		f->type != GTS_STRING) {
	      gts_file_error (f, "expecting a string");
	      var->set = FALSE;
	    }
	    else if (var->data)
	      *((gchar **) var->data) = g_strdup (f->token->str); 
	    break;
	  default:
	    g_assert_not_reached ();
	  }
	}
      }
    }
    else
      var++;
  }
  if (!found)
    gts_file_error (f, "unknown identifier `%s'", f->token->str);
  else if (f->type != GTS_ERROR) {
    g_assert (var->set);
    gts_file_next_token (f);
    return var;
  }
  return NULL;
}

/**
 * gts_file_assign_variables:
 * @f: a #GtsFile.
 * @vars: an array of #GtsFileVariable.
 *
 * Assigns all the variables belonging to @vars found in @f.
 *
 * If an error is encountered the @error field of @f is set.
 */
void gts_file_assign_variables (GtsFile * f, GtsFileVariable * vars)
{
  g_return_if_fail (f != NULL);
  g_return_if_fail (vars != NULL);

  gts_file_assign_start (f, vars);
  while (gts_file_assign_next (f, vars))
    ;
}

/**
 * gts_file_variable_error:
 * @f: a #GtsFile.
 * @vars: an array of #GtsFileVariable.
 * @name: the name of a variable in @vars.
 * @format: the standard sprintf() format string.
 * @...: the parameters to insert into the format string.
 *
 * Sets the @error field of @f using gts_file_verror().
 *
 * String @name must match one of the variable names in @vars.
 *
 * If variable @name has been assigned (using gts_file_assign_variables())
 * sets the @line and @pos fields of @f to the line and position where
 * it has been assigned.
 */
void gts_file_variable_error (GtsFile * f, 
			      GtsFileVariable * vars,
			      const gchar * name,
			      const gchar * format,
			      ...)
{
  va_list args;
  GtsFileVariable * var;

  g_return_if_fail (f != NULL);
  g_return_if_fail (vars != NULL);
  g_return_if_fail (name != NULL);
  g_return_if_fail (format != NULL);

  var = vars;
  while (var->type != GTS_NONE && strcmp (var->name, name))
    var++;

  g_return_if_fail (var->type != GTS_NONE); /* @name not found in @vars */

  if (var->set) {
    f->line = var->line;
    f->pos = var->pos;
  }

  va_start (args, format);  
  gts_file_verror (f, format, args);
  va_end (args);
}

#ifdef DEBUG_FUNCTIONS
static GHashTable * ids = NULL;
static guint next_id = 1;

guint id (gpointer p)
{
  g_return_val_if_fail (p != NULL, 0);
  g_return_val_if_fail (ids != NULL, 0);
  g_assert (g_hash_table_lookup (ids, p));
  return GPOINTER_TO_UINT (g_hash_table_lookup (ids, p));
}

void id_insert (gpointer p)
{
  g_return_if_fail (p != NULL);
  if (ids == NULL) ids = g_hash_table_new (NULL, NULL);
  g_assert (g_hash_table_lookup (ids, p) == NULL);
  g_hash_table_insert (ids, p, GUINT_TO_POINTER (next_id++));
}

void id_remove (gpointer p)
{
  g_assert (g_hash_table_lookup (ids, p));  
  g_hash_table_remove (ids, p);
}

void gts_write_triangle (GtsTriangle * t, 
			 GtsPoint * o,
			 FILE * fptr)
{
  gdouble xo = o ? o->x : 0.0;
  gdouble yo = o ? o->y : 0.0;
  gdouble zo = o ? o->z : 0.0;

  g_return_if_fail (t != NULL && fptr != NULL);

  fprintf (fptr, "(hdefine geometry \"t%d\" { =\n", id (t));
  fprintf (fptr, "OFF 3 1 0\n"
	   "%g %g %g\n%g %g %g\n%g %g %g\n3 0 1 2\n})\n"
	   "(geometry \"t%d\" { : \"t%d\"})\n"
	   "(normalization \"t%d\" none)\n",
	   GTS_POINT (GTS_SEGMENT (t->e1)->v1)->x - xo, 
	   GTS_POINT (GTS_SEGMENT (t->e1)->v1)->y - yo,
	   GTS_POINT (GTS_SEGMENT (t->e1)->v1)->z - zo,
	   GTS_POINT (GTS_SEGMENT (t->e1)->v2)->x - xo, 
	   GTS_POINT (GTS_SEGMENT (t->e1)->v2)->y - yo, 
	   GTS_POINT (GTS_SEGMENT (t->e1)->v2)->z - zo,
	   GTS_POINT (gts_triangle_vertex (t))->x - xo,
	   GTS_POINT (gts_triangle_vertex (t))->y - yo,
	   GTS_POINT (gts_triangle_vertex (t))->z - zo,
	   id (t), id (t), id (t));
}

void gts_write_segment (GtsSegment * s, 
			GtsPoint * o,
			FILE * fptr)
{
  gdouble xo = o ? o->x : 0.0;
  gdouble yo = o ? o->y : 0.0;
  gdouble zo = o ? o->z : 0.0;

  g_return_if_fail (s != NULL && fptr != NULL);

  fprintf (fptr, "(geometry \"s%d\" { =\n", id (s));
  fprintf (fptr, "VECT 1 2 0 2 0 %g %g %g %g %g %g })\n"
	   "(normalization \"s%d\" none)\n",
	   GTS_POINT (s->v1)->x - xo, 
	   GTS_POINT (s->v1)->y - yo, 
	   GTS_POINT (s->v1)->z - zo,
	   GTS_POINT (s->v2)->x - xo, 
	   GTS_POINT (s->v2)->y - yo, 
	   GTS_POINT (s->v2)->z - zo,
	   id (s));
}
#endif /* DEBUG_FUNCTIONS */
