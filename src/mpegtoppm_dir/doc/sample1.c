/*
 * sample1.c
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

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "mpeg2.h"

static void save_pgm (int width, int height, uint8_t * const * buf, int num)
{
    char filename[100];
    FILE * pgmfile;
    int i;

    sprintf (filename, "%d.pgm", num);
    pgmfile = fopen (filename, "wb");
    if (!pgmfile)
	return;
    fprintf (pgmfile, "P5\n%d %d\n255\n", width, height * 3 / 2);
    fwrite (buf[0], width, height, pgmfile);
    width >>= 1;
    height >>= 1;
    for (i = 0; i < height; i++) {
	fwrite (buf[1] + i * width, width, 1, pgmfile);
	fwrite (buf[2] + i * width, width, 1, pgmfile);
    }
    fclose (pgmfile);
}

static void sample1 (FILE * file)
{
#define BUFFER_SIZE 4096
    uint8_t buffer[BUFFER_SIZE];
    mpeg2dec_t * mpeg2dec;
    const mpeg2_info_t * info;
    int state;
    int size;
    int framenum = 0;

    mpeg2dec = mpeg2_init ();
    if (mpeg2dec == NULL)
	exit (1);
    info = mpeg2_info (mpeg2dec);

    size = BUFFER_SIZE;
    do {
	state = mpeg2_parse (mpeg2dec);
	switch (state) {
	case -1:
	    size = fread (buffer, 1, BUFFER_SIZE, file);
	    mpeg2_buffer (mpeg2dec, buffer, buffer + size);
	    break;
	case STATE_SLICE:
	case STATE_END:
	    if (info->display_fbuf)
		save_pgm (info->sequence->width, info->sequence->height,
			  info->display_fbuf->buf, framenum++);
	    break;
	}
    } while (size);

    mpeg2_close (mpeg2dec);
}

int main (int argc, char ** argv)
{
    FILE * file;

    if (argc > 1) {
	file = fopen (argv[1], "rb");
	if (!file) {
	    fprintf (stderr, "Could not open file %s\n", argv[1]);
	    exit (1);
	}
    } else
	file = stdin;

    sample1 (file);

    return 0;
}
