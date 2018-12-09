/* Adapted from obj_to_ply.c by Greg Turk. See copyright notice below
   Most of the direct ply conversion is taken out. */

/*

Convert from Wavefront OBJ format to PLY format.

Greg Turk

-----------------------------------------------------------------------

Copyright (c) 1998 Georgia Institute of Technology.  All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation for any purpose is hereby granted without fee, provided
that the above copyright notice and this permission notice appear in
all copies of this software and that you do not sell the software.

THE SOFTWARE IS PROVIDED "AS IS" AND WITHOUT WARRANTY OF ANY KIND,
EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.


*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "ply.h"

/*** the PLY object ***/

static int nverts = 0;
static int max_verts = 0;
static float *verts=NULL;

static int nfaces = 0;
static int max_faces = 0;
static int *flist=NULL;
static int nelems = 2;

static int ncomments = 0;
static int max_comments = 0;
static char **comments = NULL;

static int texture_coords = 0;
static int has_normals = 0;
static int has_w = 0;

/* for file reading */
static char **words;
static int max_words = 0;
static int num_words = 0;
#define BIG_STRING 4096
static char str[BIG_STRING];
static char str_orig[BIG_STRING];
static int flip_vertex_order = 1;

/*
  Procedures.
*/
char *fetch_line ( FILE *fp );
int fetch_words ( void );
void get_indices ( char *word, int *vindex, int *tindex, int *nindex );
void make_comment ( char *comment );
void make_face ( char **words, int nwords );
void make_vertex ( float x, float y, float z, float w );
void read_obj ( FILE *fp );

/******************************************************************************/

char *fetch_line ( FILE *fp )

/******************************************************************************/
/*
  Purpose:

    FETCH_LINE gets a text line and see if it is a line of comments.

  Author:

    Greg Turk

  Entry:
    fp - file to read from

  Exit:
    returns a pointer to comments or NULL if not a comment line or -1 if EOF
*/
{
  char *comment_ptr;
  int i;
  int j;
  char *ptr;
  char *ptr2;
  char *result;
/*
  Read in a line.
*/
  result = fgets (str, BIG_STRING, fp);

/*
  Return NULL if we're at the end-of-file.
*/
  if (result == NULL)
  {
    return ((char *) -1);
  }
/*
  Convert line-feed and tabs into spaces.
  This guarentees that there will be a space before the
  null character at the end of the string.
*/
  str[BIG_STRING-2] = ' ';
  str[BIG_STRING-1] = '\0';

  for (ptr = str; *ptr != '\0'; ptr++)
  {
    if (*ptr == '\t')
    {
      *ptr = ' ';
    }
    else if (*ptr == '\n')
    {
      *ptr = ' ';
      break;
    }
  }
/*
  Copy the line.
*/
  for (ptr = str, ptr2 = str_orig; *ptr != '\0'; ptr++, ptr2++)
    *ptr2 = *ptr;
  *ptr2 = '\0';
/*
  Look to see if this is a comment line (first non-space is '#').
*/
  for (ptr = str; *ptr != '\0'; ptr++) {
    if (*ptr == '#') {
      ptr++;
      while (*ptr == ' ')
        ptr++;
      return (ptr);
    }
    else if (*ptr != ' ') {
      break;
    }
  }

  /* if we get here, we've got a non-comment line */

/*
  Strip off trailing comments.
*/
  while (*ptr != '\0')
  {
    if (*ptr == '#')
    {
      *ptr++ = ' ';
      *ptr = '\0';
      break;
    }
    ptr++;
  }

  return (NULL);
}
/******************************************************************************/

int fetch_words ( void )

/******************************************************************************/
/*
  Purpose:

    FETCH_WORDS breaks up the last read line into words.

  Author:

    Greg Turk

  Parameters:

    Output, int FETCH_WORDS, the number of words in the line.
*/
{
  char *ptr;
/*
  Allocate room for words if necessary.
*/
  if ( max_words == 0 )
  {
    max_words = 20;
    words = (char **) malloc (sizeof (char *) * max_words);
  }
/*
  Find the words in the line.
*/
  ptr = str;
  num_words = 0;

  while (*ptr != '\0')
  {
/*
  Jump over leading spaces.
*/
    while (*ptr == ' ')
      ptr++;

    /* break if we reach the end */
    if (*ptr == '\0')
      break;

    /* allocate more room for words if necessary */
    if (num_words >= max_words) {
      max_words += 10;
      words = (char **) realloc (words, sizeof (char *) * max_words);
    }

    /* save pointer to beginning of word */
    words[num_words++] = ptr;

    /* jump over non-spaces */
    while (*ptr != ' ')
      ptr++;

    /* place a null character here to mark the end of the word */
    *ptr++ = '\0';
  }
/*
  Return the number of words.
*/
  return (num_words);
}
/******************************************************************************/

void get_indices ( char *word, int *vindex, int *tindex, int *nindex )

/******************************************************************************/
/*
  Purpose:

    GET_INDICES breaks a word of slash-separated numbers into one or more numbers.

  Author:

    Greg Turk

  Entry:
    word - word to break up

  Exit:
    vindex - first number (vertex index)
    tindex - second number (texture index)
    nindex - third number (normal vector index)
*/
{
  char *np;
  char *null = " ";
  char *ptr;
  char *tp;
/*
  By default, the texture and normal pointers are set to the null string.
*/
  tp = null;
  np = null;
/*
  Replace slashes with null characters and cause tp and np to point
  to character immediately following the first or second slash
*/
  for (ptr = word; *ptr != '\0'; ptr++)
  {
    if (*ptr == '/')
    {
      if (tp == null)
      {
        tp = ptr + 1;
      }
      else
      {
        np = ptr + 1;
      }
      *ptr = '\0';
    }
  }

  *vindex = atoi ( word );
  *tindex = atoi ( tp );
  *nindex = atoi ( np );

  return;
}
/******************************************************************************/

void make_comment ( char *comment )

/******************************************************************************/
/*
  Purpose:

    MAKE_COMMENT saves a new comment.

  Author:

    Greg Turk

  Parameters:

    Input, char *COMMENT, a comment to tuck away.
*/
{
/*
  See if we need to allocate space for comments.
*/
  if ( max_comments == 0 )
  {
    max_comments = 10;
    comments = (char **) malloc (sizeof (char *) * max_comments);
  }

  if ( ncomments == max_comments)
  {
    max_comments += 10;
    comments = (char **) realloc (comments, sizeof (char *) * max_comments);
  }

  comments[ncomments] = strdup (comment);
  ncomments++;

  return;
}
/******************************************************************************/

void make_face ( char **words, int nwords )

/******************************************************************************/
/*
  Purpose:

    MAKE_FACE creates a new face.

  Modified:

    18 January 2011

  Author:

    Greg Turk

  Entry:

    Input, char **WORDS, a list of words describing the vertex.

    Input, int NWORDS, the number of words in the list.
*/
{
  int i;
  int ii;
  int *f;
  int nindex;
  int tindex;
  int vindex;
  static int warning = 0;
/*
  See if we need to allocate space for vertices.
*/
  if (nwords != 3) {
   fprintf(stderr,"Not ready to deal with non-triangular obj faces\n");
   exit(1);
  }

  if ( max_faces == 0 )
  {
    max_faces = 200;
    flist = ( int * ) malloc ( sizeof ( int ) * 3 * max_faces );
  }
  else if ( max_faces == nfaces )
  {
    max_faces = max_faces * 2;
    flist = ( int * ) realloc ( flist, sizeof ( int ) * 3 * max_faces );
  }

  f = flist+3*nfaces; ++nfaces;

  for ( i = 0; i < nwords; i++ )
  {
    fprintf(stderr,"Parsing '%s'\n", words[i]);
    get_indices (words[i], &vindex, &tindex, &nindex );
/*
  Maybe flip vertex order.
*/
    if ( flip_vertex_order )
    {
      ii = nwords - i - 1;
    }
    else
    {
      ii = i;
    }
/*
  Store the vertex index.
*/

/*
  Indices seem to start at 1, not zero?
*/
    if ( 0 < vindex )
    {
      f[ii] = vindex - 1;
    }
/*
  Indices are negative, so counting backwards?
*/
    else if (vindex < 0)
    {
      f[ii] = nverts + vindex;
    }
    else
    {
      fprintf (stderr, "Zero indices not allowed: (%d) '%s', '%s'\n",
                       vindex, str_orig, words[i]);
      exit (-1);
    }

    if ((tindex != 0 || nindex != 0) && warning == 0)
    {
      fprintf (stderr, "\n");
      fprintf (stderr, "Warning: textures and normals currently ignored.\n");
      fprintf (stderr, "\n");
      warning = 1;
    }

  }
  return;
}
/******************************************************************************/

void make_vertex ( float x, float y, float z, float w )

/******************************************************************************/
/*
  Purpose:

    MAKE_VERTEX creates a new vertex.

  Author:

    Greg Turk

  Entry:
    x,y,z,w - 3D positions, maybe with homogeneous component
*/
{
   float *v;
/*
  See if we need to allocate space for vertices.
*/
  if ( max_verts == 0 )
  {
    max_verts = 200;
    verts = ( float * ) malloc ( sizeof ( float ) * 3 * max_verts );
  }
  else if ( nverts == max_verts )
  {
    max_verts = max_verts * 2;
    verts = ( float * ) realloc ( verts, sizeof ( float ) * 3 * max_verts );
  }

  v = verts+3*nverts; ++nverts;
  v[0] = x;
  v[1] = y;
  v[2] = z;
  /* ignore w for now */

  return;
}
/******************************************************************************/

void read_obj ( FILE *fp )

/******************************************************************************/
/*
  Purpose:

    READ_OBJ reads in a Wavefront OBJ file.

  Author:

    Greg Turk
*/
{
  char *comment_ptr;
  char *first_word;
  int i;
  int j;
  int k;
  int nwords;
  float w;
  float x;
  float y;
  float z;
/*
  Read from standard input.
*/
  if (!fp) fp = stdin;

  while (1)
  {
    comment_ptr = fetch_line ( fp );
/*
  End of file?
*/
    if ( comment_ptr == ( char * ) -1 )
    {
      break;
    }
/*
  Did we actually get a comment?
*/
    if ( comment_ptr )
    {
      make_comment ( comment_ptr );
      continue;
    }
/*
  If we get here, the line was not a comment.
*/
    nwords = fetch_words ( );
/*
  Skip empty lines.
*/
    if ( nwords == 0 )
    {
      continue;
    }

    first_word = words[0];

    if (equal_strings (first_word, "v"))
    {
      if (nwords < 4)
      {
	    fprintf (stderr, "Too few coordinates: '%s'", str_orig);
	    exit (-1);
      }
      x = atof (words[1]);
      y = atof (words[2]);
      z = atof (words[3]);
      if (nwords == 5)
      {
        w = atof (words[3]);
	    has_w = 1;
      }
      else
      {
        w = 1.0;
      }
      make_vertex ( x, y, z, w );
    }
    else if (equal_strings (first_word, "vn"))
    {
    }
    else if (equal_strings (first_word, "vt"))
    {
    }
    else if (equal_strings (first_word, "f"))
    {
      make_face (&words[1], nwords-1);
    }
    else
    {
      fprintf (stderr, "Do not recognize: '%s'\n", str_orig);
    }
  }
  return;
}


