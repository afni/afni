/*****
* icons.h : HTML Predefined icon-like symbols
*			Based on W3C Working Draft WD-wwwicn-960729
*
* This file Version	$Revision$
*
* Creation date:		Fri Jun 26 21:48:39 CEST 1998
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				XmHTML Developers Account
*
* Copyright (C) 1994-1998 by Ripley Software Development
* All Rights Reserved
*
* This file is part of the XmHTML Widget Library
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Library General Public
* License as published by the Free Software Foundation; either
* version 2 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Library General Public License for more details.
*
* You should have received a copy of the GNU Library General Public
* License along with this library; if not, write to the Free
* Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*****/
/*****
* $Source$
*****/
/*****
* ChangeLog
* $Log$
* Revision 1.1  2011/06/30 16:08:57  rwcox
* Cadd
*
*
*****/

#ifndef _icons_h_
#define _icons_h_

/* for use with the standalone HTML parser */
#ifdef MINIPARSE
/*****
* Definition of a W3C icon entity.
*****/
typedef struct {
	char *escape;
	char *data;
	char *icon;
	int len;
}IconEntity;

#define W3CICON(NAME) #NAME

#define NUM_ESCAPE_ICONS	61

#else /* !MINIPARSE, XmHTML */

#define W3CICON(NAME) __builtin_w3c_icon_##NAME

/*****
* The builtin-icons.
* All color icons share the same colormap, which has at most 16 entries.
*****/
#include <pixmaps/archive.xpm>
#include <pixmaps/audio.xpm>
#include <pixmaps/binary.document.xpm>
#include <pixmaps/binhex.document.xpm>
#include <pixmaps/calculator.xpm>
#include <pixmaps/caution.xpm>
#include <pixmaps/cd_i.xpm>
#include <pixmaps/cd_rom.xpm>
#include <pixmaps/clock.xpm>
#include <pixmaps/compressed.document.xpm>
#include <pixmaps/disk.drive.xpm>
#include <pixmaps/diskette.xpm>
#include <pixmaps/display.xpm>
#include <pixmaps/document.xpm>
#include <pixmaps/fax.xpm>
#include <pixmaps/filing.cabinet.xpm>
#include <pixmaps/film.xpm>
#include <pixmaps/fixed.disk.xpm>
#include <pixmaps/folder.open.xpm>
#include <pixmaps/folder.xpm>
#include <pixmaps/form.xpm>
#include <pixmaps/ftp.xpm>
#include <pixmaps/glossary.xpm>
#include <pixmaps/gopher.xpm>
#include <pixmaps/home.xpm>
#include <pixmaps/html.xpm>
#include <pixmaps/image.xpm>
#include <pixmaps/index.xpm>
#include <pixmaps/keyboard.xpm>
#include <pixmaps/mail.in.xpm>
#include <pixmaps/mail.out.xpm>
#include <pixmaps/mail.xpm>
#include <pixmaps/map.xpm>
#include <pixmaps/mouse.xpm>
#include <pixmaps/network.xpm>
#include <pixmaps/new.xpm>
#include <pixmaps/next.xpm>
#include <pixmaps/notebook.xpm>
#include <pixmaps/parent.xpm>
#include <pixmaps/play.fast.forward.xpm>
#include <pixmaps/play.fast.reverse.xpm>
#include <pixmaps/play.pause.xpm>
#include <pixmaps/play.start.xpm>
#include <pixmaps/play.stop.xpm>
#include <pixmaps/previous.xpm>
#include <pixmaps/printer.xpm>
#include <pixmaps/sadsmiley.xpm>
#include <pixmaps/smiley.xpm>
#include <pixmaps/stop.xpm>
#include <pixmaps/summary.xpm>
#include <pixmaps/telephone.xpm>
#include <pixmaps/telnet.xpm>
#include <pixmaps/text.document.xpm>
#include <pixmaps/tn3270.xpm>
#include <pixmaps/toc.xpm>
#include <pixmaps/trash.xpm>
#include <pixmaps/unknown.document.xpm>
#include <pixmaps/uuencoded.document.xpm>
#include <pixmaps/work.xpm>
#include <pixmaps/www.xpm>

#endif /* !MINIPARSE */

/*****
* List of all proposed HTML icon-like symbols. This list has been
* alphabetically sorted to speed up the search process.
* This list contains 61 elements. The last element is the NULL element.
*****/

IconEntity _XmHTMLIconEntities[NUM_ESCAPE_ICONS] = {
	{"archive;",			W3CICON(archive),				NULL, 8 },
	{"audio;",				W3CICON(audio),					NULL, 6 },
	{"binary.document;",	W3CICON(binary_document),		NULL, 16},
	{"binhex.document;",	W3CICON(binhex_document),		NULL, 16},
	{"calculator;",			W3CICON(calculator),			NULL, 11},
	{"caution;",			W3CICON(caution),				NULL, 8 },
	{"cd.i;",				W3CICON(cd_i),					NULL, 5 },
	{"cd.rom;",				W3CICON(cd_rom),				NULL, 7 },
	{"clock;",				W3CICON(clock),					NULL, 6 },
	{"compressed.document;",W3CICON(compressed_document),	NULL, 20},
	{"disk.drive;",			W3CICON(disk_drive),			NULL, 11},
	{"diskette;",			W3CICON(diskette),				NULL, 9 },
	{"display;",			W3CICON(display),				NULL, 8 },
	{"document;",			W3CICON(document),				NULL, 9 },
	{"fax;",				W3CICON(fax),					NULL, 4 },
	{"filing.cabinet;",		W3CICON(filing_cabinet),		NULL, 15},
	{"film;",				W3CICON(film),					NULL, 5 },
	{"fixed.disk;",			W3CICON(fixed_disk),			NULL, 11},
	{"folder.open;",		W3CICON(folder_open),			NULL, 12},
	{"folder;",				W3CICON(folder),				NULL, 7 },
	{"form;",				W3CICON(form),					NULL, 5 },
	{"ftp;",				W3CICON(ftp),					NULL, 4 },
	{"glossary;",			W3CICON(glossary),				NULL, 9 },
	{"gopher;",				W3CICON(gopher),				NULL, 7 },
	{"home;",				W3CICON(home),					NULL, 5 },
	{"html;",				W3CICON(html),					NULL, 5 },
	{"image;",				W3CICON(image),					NULL, 6 },
	{"index;",				W3CICON(index),					NULL, 6 },
	{"keyboard;",			W3CICON(keyboard),				NULL, 9 },
	{"mail.in;",			W3CICON(mail_in),				NULL, 8 },
	{"mail.out;",			W3CICON(mail_out),				NULL, 9 },
	{"mail;",				W3CICON(mail),					NULL, 5 },
	{"map;",				W3CICON(map),					NULL, 4 },
	{"mouse;",				W3CICON(mouse),					NULL, 6 },
	{"network;",			W3CICON(network),				NULL, 8 },
	{"new;",				W3CICON(new),					NULL, 4 },
	{"next;",				W3CICON(next),					NULL, 5 },
	{"notebook;",			W3CICON(notebook),				NULL, 9 },
	{"parent;",				W3CICON(parent),				NULL, 7 },
	{"play.fast.forward;",	W3CICON(play_fast_forward),		NULL, 18},
	{"play.fast.reverse;",	W3CICON(play_fast_reverse),		NULL, 18},
	{"play.pause;",			W3CICON(play_pause),			NULL, 11},
	{"play.start;",			W3CICON(play_start),			NULL, 11},
	{"play.stop;",			W3CICON(play_stop),				NULL, 10},
	{"previous;",			W3CICON(previous),				NULL, 9 },
	{"printer;",			W3CICON(printer),				NULL, 8 },
	{"sadsmiley;",			W3CICON(sadsmiley),				NULL, 10},
	{"smiley;",				W3CICON(smiley),				NULL, 7 },
	{"stop;",				W3CICON(stop),					NULL, 5 },
	{"summary;",			W3CICON(summary),				NULL, 8 },
	{"telephone;",			W3CICON(telephone),				NULL, 10},
	{"telnet;",				W3CICON(telnet),				NULL, 7 },
	{"text.document;",		W3CICON(text_document),			NULL, 14},
	{"tn3270;",				W3CICON(tn3270),				NULL, 7 },
	{"toc;",				W3CICON(toc),					NULL, 4 },
	{"trash;",				W3CICON(trash),					NULL, 6 },
	{"unknown.document;",	W3CICON(unknown_document),		NULL, 17},
	{"uuencoded.document;",	W3CICON(uuencoded_document),	NULL, 19},
	{"work;",				W3CICON(work),					NULL, 5 },
	{"www;",				W3CICON(www),					NULL, 4 },
	{NULL,					NULL,							NULL, 0 }
};

/* Don't add anything after this endif! */
#endif /* _icons_h_ */

