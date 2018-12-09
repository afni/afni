/*****
* HTTPP.h : Private HTTP.c header file.
*
* This file Version	$Revision$
*
* Creation date:		Tue Oct 21 01:57:37 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				Richard Offer
*
* Copyright (C) 1994-1997 by Richard Offer <offer@sgi.com>
* All Rights Reserved
*
* This file is part of insert_program_name_here
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
* Revision 1.1  2011/06/30 16:09:09  rwcox
* Cadd
*
* Revision 1.1  1997/10/23 00:28:29  newt
* Initial Revision
*
*****/

#ifndef _HTTPP_h_
#define _HTTPP_h_

#include <http/HTTP.h>

#define HTTP_VERSION_09 900
#define HTTP_VERSION_10 1000
#define HTTP_VERSION_11 1100

#define DEFAULT_TIMEOUT		5	/* default connect() and select() timeout	*/
#define DEFAULT_RETRY		0	/* default retry if select() fails			*/

#define GET_METHOD			"GET "
#define POST_METHOD			"POST "
#define HEAD_METHOD			"HEAD "
#define META_METHOD			"META "
#define HTTPVERSIONHDR " HTTP/1.0\r\n"
#define NEWLINE "\r\n"
#define CONTENT_LEN "Content-Length: "
#define CONTENT_TYPE "Content-Type: text/plain"

/* makes the HTTP header for the cookieList, it is the string returned from
 * this function that should be sent across the wire
 * The user is responsible for freeing the space.
 */
char *makeCookie(HTTPCookieList *cookieList);

/* parse str, make a cookie and add it to the req->setCooke list, type is
 * SetCookie or SetCookie2, host is the host url (used as default for domain)
 */
void setCookie(HTTPCookieRequest *req, int type, char *str, char *host);

/*****
* This is the strlen of the Content-length of the form data.
* 10 => forms with up to 100000000 bytes of data.
*****/
#define MAX_FORM_LEN 10

/* read request size increment */
#define CHUNKSIZE 65536

/* HTTP server response definition */
typedef struct _HTTPResponse {
	unsigned char *data;			/* message						*/
	int http_version;				/* server version				*/
	HTTPRequestReturn status_code;	/* request succeeded?			*/
	HTTPNamedValues *headers;		/* array of returned headers	*/
	int num_headers;				/* no of headers				*/
} HTTPResponse;

/* Don't add anything after this endif! */
#endif /* _HTTPP_h_ */
