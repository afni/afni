/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#ifndef _MCW_XIM_HEADER_
#define _MCW_XIM_HEADER_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/X.h>
#include <X11/Intrinsic.h>

#include "mrilib.h"
#include "display.h"

extern void MCW_kill_XImage( XImage * ) ;
extern XImage * mri_to_XImage( MCW_DC * , MRI_IMAGE * ) ;
extern XImage * resize_XImage( MCW_DC * , XImage * , int , int ) ;
extern MRI_IMAGE * XImage_to_mri( MCW_DC * , XImage * , int ) ;

extern XImage * pixar_to_XImage( MCW_DC * , int,int , Pixel * ) ;
extern XImage * rgb_to_XImage( MCW_DC * , MRI_IMAGE * ) ;

#define X2M_USE_CMAP  (1<<0)  /* masks for XImage_to_mri() 3rd arg */
#define X2M_FORCE_RGB (1<<1)

#endif /* _MCW_XIM_HEADER_ */
