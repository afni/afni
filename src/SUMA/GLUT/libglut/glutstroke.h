#ifndef __glutstroke_h__
#define __glutstroke_h__

/* Copyright (c) Mark J. Kilgard, 1994. */

/* This program is freely distributable without licensing fees 
   and is provided without guarantee or warrantee expressed or 
   implied. This program is -not- in the public domain. */

typedef struct {
  float x;
  float y;
} CoordRec, *CoordPtr;

typedef struct {
  int num_coords;
  CoordPtr coord;
} StrokeRec, *StrokePtr;

typedef struct {
  int num_strokes;
  StrokePtr stroke;
  float center;
  float right;
} StrokeCharRec, *StrokeCharPtr;

typedef struct {
  char *name;
  int num_chars;
  StrokeCharPtr ch;
  float top;
  float bottom;
} StrokeFontRec, *StrokeFontPtr;

typedef void *GLUTstrokeFont;

#endif /* __glutstroke_h__ */
