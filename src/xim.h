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
extern MRI_IMAGE * XImage_to_mri( MCW_DC * , XImage * ) ;

#endif /* _MCW_XIM_HEADER_ */
