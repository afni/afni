/*
 * yuv2rgb_mmx.c
 * Copyright (C) 2000-2002 Silicon Integrated System Corp.
 * All Rights Reserved.
 *
 * Author: Olie Lho <ollie@sis.com.tw>
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

#ifdef ARCH_X86

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "convert.h"
#include "convert_internal.h"
#include "attributes.h"
#include "mmx.h"

#define CPU_MMXEXT 0
#define CPU_MMX 1

/* CPU_MMXEXT/CPU_MMX adaptation layer */

#define movntq(src,dest)	\
do {				\
    if (cpu == CPU_MMXEXT)	\
	movntq_r2m (src, dest);	\
    else			\
	movq_r2m (src, dest);	\
} while (0)

static inline void mmx_yuv2rgb (uint8_t * py, uint8_t * pu, uint8_t * pv)
{
    static mmx_t mmx_80w = {0x0080008000800080LL};
    static mmx_t mmx_U_green = {0xf37df37df37df37dLL};
    static mmx_t mmx_U_blue = {0x4093409340934093LL};
    static mmx_t mmx_V_red = {0x3312331233123312LL};
    static mmx_t mmx_V_green = {0xe5fce5fce5fce5fcLL};
    static mmx_t mmx_10w = {0x1010101010101010LL};
    static mmx_t mmx_00ffw = {0x00ff00ff00ff00ffLL};
    static mmx_t mmx_Y_coeff = {0x253f253f253f253fLL};

    movd_m2r (*pu, mm0);		/* mm0 = 00 00 00 00 u3 u2 u1 u0 */
    movd_m2r (*pv, mm1);		/* mm1 = 00 00 00 00 v3 v2 v1 v0 */
    movq_m2r (*py, mm6);		/* mm6 = Y7 Y6 Y5 Y4 Y3 Y2 Y1 Y0 */
    pxor_r2r (mm4, mm4);		/* mm4 = 0 */
    /* XXX might do cache preload for image here */

    /*
     * Do the multiply part of the conversion for even and odd pixels
     * register usage:
     * mm0 -> Cblue, mm1 -> Cred, mm2 -> Cgreen even pixels
     * mm3 -> Cblue, mm4 -> Cred, mm5 -> Cgreen odd  pixels
     * mm6 -> Y even, mm7 -> Y odd
     */

    punpcklbw_r2r (mm4, mm0);		/* mm0 = u3 u2 u1 u0 */
    punpcklbw_r2r (mm4, mm1);		/* mm1 = v3 v2 v1 v0 */
    psubsw_m2r (mmx_80w, mm0);		/* u -= 128 */
    psubsw_m2r (mmx_80w, mm1);		/* v -= 128 */
    psllw_i2r (3, mm0);			/* promote precision */
    psllw_i2r (3, mm1);			/* promote precision */
    movq_r2r (mm0, mm2);		/* mm2 = u3 u2 u1 u0 */
    movq_r2r (mm1, mm3);		/* mm3 = v3 v2 v1 v0 */
    pmulhw_m2r (mmx_U_green, mm2);	/* mm2 = u * u_green */
    pmulhw_m2r (mmx_V_green, mm3);	/* mm3 = v * v_green */
    pmulhw_m2r (mmx_U_blue, mm0);	/* mm0 = chroma_b */
    pmulhw_m2r (mmx_V_red, mm1);	/* mm1 = chroma_r */
    paddsw_r2r (mm3, mm2);		/* mm2 = chroma_g */

    psubusb_m2r (mmx_10w, mm6);		/* Y -= 16 */
    movq_r2r (mm6, mm7);		/* mm7 = Y7 Y6 Y5 Y4 Y3 Y2 Y1 Y0 */
    pand_m2r (mmx_00ffw, mm6);		/* mm6 =    Y6    Y4    Y2    Y0 */
    psrlw_i2r (8, mm7);			/* mm7 =    Y7    Y5    Y3    Y1 */
    psllw_i2r (3, mm6);			/* promote precision */
    psllw_i2r (3, mm7);			/* promote precision */
    pmulhw_m2r (mmx_Y_coeff, mm6);	/* mm6 = luma_rgb even */
    pmulhw_m2r (mmx_Y_coeff, mm7);	/* mm7 = luma_rgb odd */

    /*
     * Do the addition part of the conversion for even and odd pixels
     * register usage:
     * mm0 -> Cblue, mm1 -> Cred, mm2 -> Cgreen even pixels
     * mm3 -> Cblue, mm4 -> Cred, mm5 -> Cgreen odd  pixels
     * mm6 -> Y even, mm7 -> Y odd
     */

    movq_r2r (mm0, mm3);		/* mm3 = chroma_b */
    movq_r2r (mm1, mm4);		/* mm4 = chroma_r */
    movq_r2r (mm2, mm5);		/* mm5 = chroma_g */
    paddsw_r2r (mm6, mm0);		/* mm0 = B6 B4 B2 B0 */
    paddsw_r2r (mm7, mm3);		/* mm3 = B7 B5 B3 B1 */
    paddsw_r2r (mm6, mm1);		/* mm1 = R6 R4 R2 R0 */
    paddsw_r2r (mm7, mm4);		/* mm4 = R7 R5 R3 R1 */
    paddsw_r2r (mm6, mm2);		/* mm2 = G6 G4 G2 G0 */
    paddsw_r2r (mm7, mm5);		/* mm5 = G7 G5 G3 G1 */
    packuswb_r2r (mm0, mm0);		/* saturate to 0-255 */
    packuswb_r2r (mm1, mm1);		/* saturate to 0-255 */
    packuswb_r2r (mm2, mm2);		/* saturate to 0-255 */
    packuswb_r2r (mm3, mm3);		/* saturate to 0-255 */
    packuswb_r2r (mm4, mm4);		/* saturate to 0-255 */
    packuswb_r2r (mm5, mm5);		/* saturate to 0-255 */
    punpcklbw_r2r (mm3, mm0);		/* mm0 = B7 B6 B5 B4 B3 B2 B1 B0 */
    punpcklbw_r2r (mm4, mm1);		/* mm1 = R7 R6 R5 R4 R3 R2 R1 R0 */
    punpcklbw_r2r (mm5, mm2);		/* mm2 = G7 G6 G5 G4 G3 G2 G1 G0 */
}

static inline void mmx_unpack_16rgb (uint8_t * image, const int cpu)
{
    static mmx_t mmx_bluemask = {0xf8f8f8f8f8f8f8f8LL};
    static mmx_t mmx_greenmask = {0xfcfcfcfcfcfcfcfcLL};
    static mmx_t mmx_redmask = {0xf8f8f8f8f8f8f8f8LL};

    /*
     * convert RGB plane to RGB 16 bits
     * mm0 -> B, mm1 -> R, mm2 -> G
     * mm4 -> GB, mm5 -> AR pixel 4-7
     * mm6 -> GB, mm7 -> AR pixel 0-3
     */

    pand_m2r (mmx_bluemask, mm0);	/* mm0 = b7b6b5b4b3______ */
    pand_m2r (mmx_greenmask, mm2);	/* mm2 = g7g6g5g4g3g2____ */
    pand_m2r (mmx_redmask, mm1);	/* mm1 = r7r6r5r4r3______ */
    psrlq_i2r (3, mm0);			/* mm0 = ______b7b6b5b4b3 */
    pxor_r2r (mm4, mm4);		/* mm4 = 0 */
    movq_r2r (mm0, mm5);		/* mm5 = ______b7b6b5b4b3 */
    movq_r2r (mm2, mm7);		/* mm7 = g7g6g5g4g3g2____ */

    punpcklbw_r2r (mm4, mm2);
    punpcklbw_r2r (mm1, mm0);
    psllq_i2r (3, mm2);
    por_r2r (mm2, mm0);
    movntq (mm0, *image);

    punpckhbw_r2r (mm4, mm7);
    punpckhbw_r2r (mm1, mm5);
    psllq_i2r (3, mm7);
    por_r2r (mm7, mm5);
    movntq (mm5, *(image+8));
}

static inline void mmx_unpack_32rgb (uint8_t * image, const int cpu)
{
    /*
     * convert RGB plane to RGB packed format,
     * mm0 -> B, mm1 -> R, mm2 -> G, mm3 -> 0,
     * mm4 -> GB, mm5 -> AR pixel 4-7,
     * mm6 -> GB, mm7 -> AR pixel 0-3
     */

    pxor_r2r (mm3, mm3);
    movq_r2r (mm0, mm6);
    movq_r2r (mm1, mm7);
    movq_r2r (mm0, mm4);
    movq_r2r (mm1, mm5);
    punpcklbw_r2r (mm2, mm6);
    punpcklbw_r2r (mm3, mm7);
    punpcklwd_r2r (mm7, mm6);
    movntq (mm6, *image);
    movq_r2r (mm0, mm6);
    punpcklbw_r2r (mm2, mm6);
    punpckhwd_r2r (mm7, mm6);
    movntq (mm6, *(image+8));
    punpckhbw_r2r (mm2, mm4);
    punpckhbw_r2r (mm3, mm5);
    punpcklwd_r2r (mm5, mm4);
    movntq (mm4, *(image+16));
    movq_r2r (mm0, mm4);
    punpckhbw_r2r (mm2, mm4);
    punpckhwd_r2r (mm5, mm4);
    movntq (mm4, *(image+24));
}

static inline void yuv420_rgb16 (uint8_t * image,
				 uint8_t * py, uint8_t * pu, uint8_t * pv,
				 int width, int height,
				 int rgb_stride, int y_stride, int uv_stride,
				 const int cpu)
{
    int i;

    rgb_stride -= 2 * width;
    y_stride -= width;
    uv_stride -= width >> 1;
    width >>= 3;

    do {
	i = width;
	do {
	    mmx_yuv2rgb (py, pu, pv);
	    mmx_unpack_16rgb (image, cpu);
	    py += 8;
	    pu += 4;
	    pv += 4;
	    image += 16;
	} while (--i);

	py += y_stride;
	image += rgb_stride;
	if (height & 1) {
	    pu += uv_stride;
	    pv += uv_stride;
	} else {
	    pu -= 4 * width;
	    pv -= 4 * width;
	}
    } while (--height);
}

static inline void yuv420_argb32 (uint8_t * image, uint8_t * py,
				  uint8_t * pu, uint8_t * pv,
				  int width, int height,
				  int rgb_stride, int y_stride, int uv_stride,
				  const int cpu)
{
    int i;

    rgb_stride -= 4 * width;
    y_stride -= width;
    uv_stride -= width >> 1;
    width >>= 3;

    do {
	i = width;
	do {
	    mmx_yuv2rgb (py, pu, pv);
	    mmx_unpack_32rgb (image, cpu);
	    py += 8;
	    pu += 4;
	    pv += 4;
	    image += 32;
	} while (--i);

	py += y_stride;
	image += rgb_stride;
	if (height & 1) {
	    pu += uv_stride;
	    pv += uv_stride;
	} else {
	    pu -= 4 * width;
	    pv -= 4 * width;
	}
    } while (--height);
}

static void mmxext_rgb16 (void * _id, uint8_t * const * src,
			  unsigned int v_offset)
{
    convert_rgb_t * id = (convert_rgb_t *) _id;

    yuv420_rgb16 (id->rgb_ptr + id->rgb_stride * v_offset,
		  src[0], src[1], src[2], id->width, 16,
		  id->rgb_stride, id->uv_stride << 1, id->uv_stride,
		  CPU_MMXEXT);
}

static void mmxext_argb32 (void * _id, uint8_t * const * src,
			   unsigned int v_offset)
{
    convert_rgb_t * id = (convert_rgb_t *) _id;

    yuv420_argb32 (id->rgb_ptr + id->rgb_stride * v_offset,
		   src[0], src[1], src[2], id->width, 16,
		  id->rgb_stride, id->uv_stride << 1, id->uv_stride,
		  CPU_MMXEXT);
}

static void mmx_rgb16 (void * _id, uint8_t * const * src,
		       unsigned int v_offset)
{
    convert_rgb_t * id = (convert_rgb_t *) _id;

    yuv420_rgb16 (id->rgb_ptr + id->rgb_stride * v_offset,
		  src[0], src[1], src[2], id->width, 16,
		  id->rgb_stride, id->uv_stride << 1, id->uv_stride, CPU_MMX);
}

static void mmx_argb32 (void * _id, uint8_t * const * src,
			unsigned int v_offset)
{
    convert_rgb_t * id = (convert_rgb_t *) _id;

    yuv420_argb32 (id->rgb_ptr + id->rgb_stride * v_offset,
		   src[0], src[1], src[2], id->width, 16,
		  id->rgb_stride, id->uv_stride << 1, id->uv_stride, CPU_MMX);
}

yuv2rgb_copy * yuv2rgb_init_mmxext (int order, int bpp)
{
    if ((order == CONVERT_RGB) && (bpp == 16))
	return mmxext_rgb16;
    else if ((order == CONVERT_RGB) && (bpp == 32))
	return mmxext_argb32;
    return NULL;	/* Fallback to C */
}

yuv2rgb_copy * yuv2rgb_init_mmx (int order, int bpp)
{
    if ((order == CONVERT_RGB) && (bpp == 16))
	return mmx_rgb16;
    else if ((order == CONVERT_RGB) && (bpp == 32))
	return mmx_argb32;
    return NULL;	/* Fallback to C */
}
#endif
