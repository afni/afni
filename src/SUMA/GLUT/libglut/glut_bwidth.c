
/* Copyright (c) Mark J. Kilgard, 1994. */

/* This program is freely distributable without licensing fees
   and is provided without guarantee or warrantee expressed or
   implied. This program is -not- in the public domain. */

#include <GL/glut.h>
#include "glutint.h"
#include "glutbitmap.h"

int
glutBitmapWidth(GLUTbitmapFont font, int c)
{
  BitmapFontPtr fontinfo = (BitmapFontPtr) font;
  BitmapCharPtr ch;

  if (c < fontinfo->first || c >= fontinfo->first + fontinfo->num_chars)
    return 0;
  ch = fontinfo->ch[c - fontinfo->first];
  if (ch)
    return ch->advance;
  else
    return 0;
}

int
glutBitmapLength(GLUTbitmapFont font, unsigned char *string)
{
  int c, length;
  BitmapFontPtr fontinfo = (BitmapFontPtr) font;
  BitmapCharPtr ch;

  length = 0;
  for (; *string != '\0'; string++) {
    c = *string;
    if (c < fontinfo->first || c >= fontinfo->first + fontinfo->num_chars) {
      ch = fontinfo->ch[c - fontinfo->first];
      if (ch)
	length += ch->advance;
    }
  }
  return length;
}

