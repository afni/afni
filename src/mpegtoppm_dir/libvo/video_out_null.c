/*
 * video_out_null.c
 * Copyright (C) 2000-2002 Michel Lespinasse <walken@zoy.org>
 * Copyright (C) 1999-2000 Aaron Holtzman <aholtzma@ess.engr.uvic.ca>
 *
 * This file is part of mpeg2dec, a free MPEG-2 video stream decoder.
 * See http://libmpeg2.sourceforge.net/ for updates.
 *
 * mpeg2dec is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * mpeg2dec is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "config.h"

#include <stdlib.h>
#include <inttypes.h>

#include "video_out.h"
#include "convert.h"

static void null_draw_frame (vo_instance_t * instance,
			     uint8_t * const * buf, void * id)
{
}

static vo_instance_t * internal_open (int setup (vo_instance_t *, int, int,
						 vo_setup_result_t *))
{
    vo_instance_t * instance;

    instance = (vo_instance_t *) malloc (sizeof (vo_instance_t));
    if (instance == NULL)
	return NULL;

    instance->setup = setup;
    instance->setup_fbuf = NULL;
    instance->set_fbuf = NULL;
    instance->start_fbuf = NULL;
    instance->draw = null_draw_frame;
    instance->discard = NULL;
    instance->close = NULL;

    return instance;
}

static int null_setup (vo_instance_t * instance, int width, int height,
		       vo_setup_result_t * result)
{
    result->convert = NULL;
    return 0;
}

vo_instance_t * vo_null_open (void)
{
    return internal_open (null_setup);
}

static void nullslice_start (void * id, uint8_t * const * dest, int flags)
{
}

static void nullslice_copy (void * id, uint8_t * const * src,
			    unsigned int v_offset)
{
}

static void nullslice_convert (int width, int height, uint32_t accel,
			       void * arg, convert_init_t * result)
{
    result->id_size = 0;
    result->buf_size[0] = result->buf_size[1] = result->buf_size[2] = 0;
    result->start = nullslice_start;
    result->copy = nullslice_copy;
}

static int nullslice_setup (vo_instance_t * instance, int width, int height,
			    vo_setup_result_t * result)
{
    result->convert = nullslice_convert;
    return 0;
}

vo_instance_t * vo_nullslice_open (void)
{
    return internal_open (nullslice_setup);
}

static int nullrgb16_setup (vo_instance_t * instance, int width, int height,
			    vo_setup_result_t * result)
{
    result->convert = convert_rgb16;
    return 0;
}

static int nullrgb32_setup (vo_instance_t * instance, int width, int height,
			    vo_setup_result_t * result)
{
    result->convert = convert_rgb32;
    return 0;
}

vo_instance_t * vo_nullrgb16_open (void)
{
    return internal_open (nullrgb16_setup);
}

vo_instance_t * vo_nullrgb32_open (void)
{
    return internal_open (nullrgb32_setup);
}
