/*
 * yuv2rgb.c
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

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "mpeg2.h"
#include "convert.h"
#include "convert_internal.h"

static uint32_t matrix_coefficients = 6;

const int32_t Inverse_Table_6_9[8][4] = {
    {117504, 138453, 13954, 34903}, /* no sequence_display_extension */
    {117504, 138453, 13954, 34903}, /* ITU-R Rec. 709 (1990) */
    {104597, 132201, 25675, 53279}, /* unspecified */
    {104597, 132201, 25675, 53279}, /* reserved */
    {104448, 132798, 24759, 53109}, /* FCC */
    {104597, 132201, 25675, 53279}, /* ITU-R Rec. 624-4 System B, G */
    {104597, 132201, 25675, 53279}, /* SMPTE 170M */
    {117579, 136230, 16907, 35559}  /* SMPTE 240M (1987) */
};

typedef void yuv2rgb_c_internal (uint8_t *, uint8_t *, uint8_t *, uint8_t *,
				 void *, void *, int);

void * table_rV[256];
void * table_gU[256];
int table_gV[256];
void * table_bU[256];

#define RGB(type,i)						\
	U = pu[i];						\
	V = pv[i];						\
	r = (type *) table_rV[V];				\
	g = (type *) (((uint8_t *)table_gU[U]) + table_gV[V]);	\
	b = (type *) table_bU[U];

#define DST(py,dst,i)				\
	Y = py[2*i];				\
	dst[2*i] = r[Y] + g[Y] + b[Y];		\
	Y = py[2*i+1];				\
	dst[2*i+1] = r[Y] + g[Y] + b[Y];

#define DSTRGB(py,dst,i)						\
	Y = py[2*i];							\
	dst[6*i] = r[Y]; dst[6*i+1] = g[Y]; dst[6*i+2] = b[Y];		\
	Y = py[2*i+1];							\
	dst[6*i+3] = r[Y]; dst[6*i+4] = g[Y]; dst[6*i+5] = b[Y];

#define DSTBGR(py,dst,i)						\
	Y = py[2*i];							\
	dst[6*i] = b[Y]; dst[6*i+1] = g[Y]; dst[6*i+2] = r[Y];		\
	Y = py[2*i+1];							\
	dst[6*i+3] = b[Y]; dst[6*i+4] = g[Y]; dst[6*i+5] = r[Y];

static void yuv2rgb_c_32 (uint8_t * py_1, uint8_t * py_2,
			  uint8_t * pu, uint8_t * pv,
			  void * _dst_1, void * _dst_2, int width)
{
    int U, V, Y;
    uint32_t * r, * g, * b;
    uint32_t * dst_1, * dst_2;

    width >>= 3;
    dst_1 = (uint32_t *) _dst_1;
    dst_2 = (uint32_t *) _dst_2;

    do {
	RGB (uint32_t, 0);
	DST (py_1, dst_1, 0);
	DST (py_2, dst_2, 0);

	RGB (uint32_t, 1);
	DST (py_2, dst_2, 1);
	DST (py_1, dst_1, 1);

	RGB (uint32_t, 2);
	DST (py_1, dst_1, 2);
	DST (py_2, dst_2, 2);

	RGB (uint32_t, 3);
	DST (py_2, dst_2, 3);
	DST (py_1, dst_1, 3);

	pu += 4;
	pv += 4;
	py_1 += 8;
	py_2 += 8;
	dst_1 += 8;
	dst_2 += 8;
    } while (--width);
}

/* This is very near from the yuv2rgb_c_32 code */
static void yuv2rgb_c_24_rgb (uint8_t * py_1, uint8_t * py_2,
			      uint8_t * pu, uint8_t * pv,
			      void * _dst_1, void * _dst_2, int width)
{
    int U, V, Y;
    uint8_t * r, * g, * b;
    uint8_t * dst_1, * dst_2;

    width >>= 3;
    dst_1 = (uint8_t *) _dst_1;
    dst_2 = (uint8_t *) _dst_2;

    do {
	RGB (uint8_t, 0);
	DSTRGB (py_1, dst_1, 0);
	DSTRGB (py_2, dst_2, 0);

	RGB (uint8_t, 1);
	DSTRGB (py_2, dst_2, 1);
	DSTRGB (py_1, dst_1, 1);

	RGB (uint8_t, 2);
	DSTRGB (py_1, dst_1, 2);
	DSTRGB (py_2, dst_2, 2);

	RGB (uint8_t, 3);
	DSTRGB (py_2, dst_2, 3);
	DSTRGB (py_1, dst_1, 3);

	pu += 4;
	pv += 4;
	py_1 += 8;
	py_2 += 8;
	dst_1 += 24;
	dst_2 += 24;
    } while (--width);
}

/* only trivial mods from yuv2rgb_c_24_rgb */
static void yuv2rgb_c_24_bgr (uint8_t * py_1, uint8_t * py_2,
			      uint8_t * pu, uint8_t * pv,
			      void * _dst_1, void * _dst_2, int width)
{
    int U, V, Y;
    uint8_t * r, * g, * b;
    uint8_t * dst_1, * dst_2;

    width >>= 3;
    dst_1 = (uint8_t *) _dst_1;
    dst_2 = (uint8_t *) _dst_2;

    do {
	RGB (uint8_t, 0);
	DSTBGR (py_1, dst_1, 0);
	DSTBGR (py_2, dst_2, 0);

	RGB (uint8_t, 1);
	DSTBGR (py_2, dst_2, 1);
	DSTBGR (py_1, dst_1, 1);

	RGB (uint8_t, 2);
	DSTBGR (py_1, dst_1, 2);
	DSTBGR (py_2, dst_2, 2);

	RGB (uint8_t, 3);
	DSTBGR (py_2, dst_2, 3);
	DSTBGR (py_1, dst_1, 3);

	pu += 4;
	pv += 4;
	py_1 += 8;
	py_2 += 8;
	dst_1 += 24;
	dst_2 += 24;
    } while (--width);
}

/* This is exactly the same code as yuv2rgb_c_32 except for the types of */
/* r, g, b, dst_1, dst_2 */
static void yuv2rgb_c_16 (uint8_t * py_1, uint8_t * py_2,
			  uint8_t * pu, uint8_t * pv,
			  void * _dst_1, void * _dst_2, int width)
{
    int U, V, Y;
    uint16_t * r, * g, * b;
    uint16_t * dst_1, * dst_2;

    width >>= 3;
    dst_1 = (uint16_t *) _dst_1;
    dst_2 = (uint16_t *) _dst_2;

    do {
	RGB (uint16_t, 0);
	DST (py_1, dst_1, 0);
	DST (py_2, dst_2, 0);

	RGB (uint16_t, 1);
	DST (py_2, dst_2, 1);
	DST (py_1, dst_1, 1);

	RGB (uint16_t, 2);
	DST (py_1, dst_1, 2);
	DST (py_2, dst_2, 2);

	RGB (uint16_t, 3);
	DST (py_2, dst_2, 3);
	DST (py_1, dst_1, 3);

	pu += 4;
	pv += 4;
	py_1 += 8;
	py_2 += 8;
	dst_1 += 8;
	dst_2 += 8;
    } while (--width);
}

static int div_round (int dividend, int divisor)
{
    if (dividend > 0)
	return (dividend + (divisor>>1)) / divisor;
    else
	return -((-dividend + (divisor>>1)) / divisor);
}

static yuv2rgb_c_internal * yuv2rgb_c_init (int order, int bpp)
{
    int i;
    uint8_t table_Y[1024];
    uint32_t * table_32 = 0;
    uint16_t * table_16 = 0;
    uint8_t * table_8 = 0;
    int entry_size = 0;
    void * table_r = 0;
    void * table_g = 0;
    void * table_b = 0;
    yuv2rgb_c_internal * yuv2rgb;

    int crv = Inverse_Table_6_9[matrix_coefficients][0];
    int cbu = Inverse_Table_6_9[matrix_coefficients][1];
    int cgu = -Inverse_Table_6_9[matrix_coefficients][2];
    int cgv = -Inverse_Table_6_9[matrix_coefficients][3];

    for (i = 0; i < 1024; i++) {
	int j;

	j = (76309 * (i - 384 - 16) + 32768) >> 16;
	j = (j < 0) ? 0 : ((j > 255) ? 255 : j);
	table_Y[i] = j;
    }

    switch (bpp) {
    case 32:
	yuv2rgb = yuv2rgb_c_32;

	table_32 = (uint32_t *) malloc ((197 + 2*682 + 256 + 132) *
					sizeof (uint32_t));

	entry_size = sizeof (uint32_t);
	table_r = table_32 + 197;
	table_b = table_32 + 197 + 685;
	table_g = table_32 + 197 + 2*682;

	for (i = -197; i < 256+197; i++)
	    ((uint32_t *) table_r)[i] =
		table_Y[i+384] << ((order == CONVERT_RGB) ? 16 : 0);
	for (i = -132; i < 256+132; i++)
	    ((uint32_t *) table_g)[i] = table_Y[i+384] << 8;
	for (i = -232; i < 256+232; i++)
	    ((uint32_t *) table_b)[i] =
		table_Y[i+384] << ((order == CONVERT_RGB) ? 0 : 16);
	break;

    case 24:
	yuv2rgb = (order == CONVERT_RGB) ? yuv2rgb_c_24_rgb : yuv2rgb_c_24_bgr;

	table_8 = (uint8_t *) malloc ((256 + 2*232) * sizeof (uint8_t));

	entry_size = sizeof (uint8_t);
	table_r = table_g = table_b = table_8 + 232;

	for (i = -232; i < 256+232; i++)
	    ((uint8_t * )table_b)[i] = table_Y[i+384];
	break;

    case 15:
    case 16:
	yuv2rgb = yuv2rgb_c_16;

	table_16 = (uint16_t *) malloc ((197 + 2*682 + 256 + 132) *
					sizeof (uint16_t));

	entry_size = sizeof (uint16_t);
	table_r = table_16 + 197;
	table_b = table_16 + 197 + 685;
	table_g = table_16 + 197 + 2*682;

	for (i = -197; i < 256+197; i++) {
	    int j = table_Y[i+384] >> 3;

	    if (order == CONVERT_RGB)
		j <<= ((bpp==16) ? 11 : 10);

	    ((uint16_t *)table_r)[i] = j;
	}
	for (i = -132; i < 256+132; i++) {
	    int j = table_Y[i+384] >> ((bpp==16) ? 2 : 3);

	    ((uint16_t *)table_g)[i] = j << 5;
	}
	for (i = -232; i < 256+232; i++) {
	    int j = table_Y[i+384] >> 3;

	    if (order == CONVERT_RGB)
		j <<= ((bpp==16) ? 11 : 10);

	    ((uint16_t *)table_b)[i] = j;
	}
	break;

    default:
	fprintf (stderr, "%ibpp not supported by yuv2rgb\n", bpp);
	exit (1);
    }

    for (i = 0; i < 256; i++) {
	table_rV[i] = (((uint8_t *)table_r) +
		       entry_size * div_round (crv * (i-128), 76309));
	table_gU[i] = (((uint8_t *)table_g) +
		       entry_size * div_round (cgu * (i-128), 76309));
	table_gV[i] = entry_size * div_round (cgv * (i-128), 76309);
	table_bU[i] = (((uint8_t *)table_b) +
		       entry_size * div_round (cbu * (i-128), 76309));
    }

    return yuv2rgb;
}

static void convert_yuv2rgb_c (void * _id, uint8_t * const * src,
			       unsigned int v_offset)
{
    convert_rgb_t * id = (convert_rgb_t *) _id;
    uint8_t * dst;
    uint8_t * py;
    uint8_t * pu;
    uint8_t * pv;
    int loop;

    dst = id->rgb_ptr + id->rgb_stride * v_offset;
    py = src[0]; pu = src[1]; pv = src[2];

    loop = 8;
    do {
	id->yuv2rgb (py, py + (id->uv_stride << 1), pu, pv,
		     dst, dst + id->rgb_stride, id->width);
	py += id->uv_stride << 2;
	pu += id->uv_stride;
	pv += id->uv_stride;
	dst += 2 * id->rgb_stride;
    } while (--loop);
}

static void convert_start (void * _id, uint8_t * const * dest, int flags)
{
    convert_rgb_t * id = (convert_rgb_t *) _id;
    id->rgb_ptr = dest[0];
    switch (flags) {
    case CONVERT_BOTTOM_FIELD:
	id->rgb_ptr += id->rgb_stride_frame;
	/* break thru */
    case CONVERT_TOP_FIELD:
	id->uv_stride = id->uv_stride_frame << 1;
	id->rgb_stride = id->rgb_stride_frame << 1;
	break;
    default:
	id->uv_stride = id->uv_stride_frame;
	id->rgb_stride = id->rgb_stride_frame;
    }
}

static void convert_internal (int order, int bpp, int width, int height,
			      uint32_t accel, void * arg,
			      convert_init_t * result)
{
    convert_rgb_t * id = (convert_rgb_t *) result->id;

    if (!id) {
	result->id_size = sizeof (convert_rgb_t);
    } else {
	id->width = width;
	id->uv_stride_frame = width >> 1;
	id->rgb_stride_frame = ((bpp + 7) >> 3) * width;

	result->buf_size[0] = id->rgb_stride_frame * height;
	result->buf_size[1] = result->buf_size[2] = 0;
	result->start = convert_start;

	result->copy = NULL;
#ifdef ARCH_X86
	if ((result->copy == NULL) && (accel & MPEG2_ACCEL_X86_MMXEXT)) {
	    result->copy = yuv2rgb_init_mmxext (order, bpp);
	}
	if ((result->copy == NULL) && (accel & MPEG2_ACCEL_X86_MMX)) {
	    result->copy = yuv2rgb_init_mmx (order, bpp);
	}
#endif
#ifdef LIBVO_MLIB
	if ((result->copy == NULL) && (accel & MPEG2_ACCEL_MLIB)) {
	    result->copy = yuv2rgb_init_mlib (order, bpp);
	}
#endif
	if (result->copy == NULL) {
	    result->copy = convert_yuv2rgb_c;
	    id->yuv2rgb = yuv2rgb_c_init (order, bpp);
	}
    }
}

void convert_rgb32 (int width, int height, uint32_t accel, void * arg,
		    convert_init_t * result)
{
    convert_internal (CONVERT_RGB, 32, width, height, accel, arg, result);
}

void convert_rgb24 (int width, int height, uint32_t accel, void * arg,
		    convert_init_t * result)
{
    convert_internal (CONVERT_RGB, 24, width, height, accel, arg, result);
}

void convert_rgb16 (int width, int height, uint32_t accel, void * arg,
		    convert_init_t * result)
{
    convert_internal (CONVERT_RGB, 16, width, height, accel, arg, result);
}

void convert_rgb15 (int width, int height, uint32_t accel, void * arg,
		    convert_init_t * result)
{
    convert_internal (CONVERT_RGB, 15, width, height, accel, arg, result);
}

void convert_bgr32 (int width, int height, uint32_t accel, void * arg,
		    convert_init_t * result)
{
    convert_internal (CONVERT_BGR, 32, width, height, accel, arg, result);
}

void convert_bgr24 (int width, int height, uint32_t accel, void * arg,
		    convert_init_t * result)
{
    convert_internal (CONVERT_BGR, 24, width, height, accel, arg, result);
}

void convert_bgr16 (int width, int height, uint32_t accel, void * arg,
		    convert_init_t * result)
{
    convert_internal (CONVERT_BGR, 16, width, height, accel, arg, result);
}

void convert_bgr15 (int width, int height, uint32_t accel, void * arg,
		    convert_init_t * result)
{
    convert_internal (CONVERT_BGR, 15, width, height, accel, arg, result);
}

convert_t * convert_rgb (int order, int bpp)
{
    if (order == CONVERT_RGB || order == CONVERT_BGR)
	switch (bpp) {
	case 32: return (order == CONVERT_RGB) ? convert_rgb32 : convert_bgr32;
	case 24: return (order == CONVERT_RGB) ? convert_rgb24 : convert_bgr24;
	case 16: return (order == CONVERT_RGB) ? convert_rgb16 : convert_bgr16;
	case 15: return (order == CONVERT_RGB) ? convert_rgb15 : convert_bgr15;
	}
    return NULL;
}
