
/* Copyright (c) Mark J. Kilgard, 1995. */

/* This program is freely distributable without licensing fees 
   and is provided without guarantee or warrantee expressed or 
   implied. This program is -not- in the public domain. */

#include <GL/glut.h>
#include "glutint.h"
#include "glutstroke.h"

int
glutStrokeWidth(GLUTstrokeFont font, int c)
{
  StrokeFontPtr fontinfo = (StrokeFontPtr) font;
  StrokeCharPtr ch;

  if (c < 0 || c >= fontinfo->num_chars)
    return 0;
  ch = &(fontinfo->ch[c]);
  if (ch)
    return ch->right;
  else
    return 0;
}

int
glutStrokeLength(GLUTstrokeFont font, unsigned char *string)
{
  int c, length;
  StrokeFontPtr fontinfo = (StrokeFontPtr) font;
  StrokeCharPtr ch;

  length = 0;
  for (; *string != '\0'; string++) {
    c = *string;
    if (c < 0 || c >= fontinfo->num_chars) {
      ch = &(fontinfo->ch[c]);
      if (ch)
	length += ch->right;
    }
  }
  return length;
}

