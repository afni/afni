#ifndef lint
static char rcsId[]="$Header$";
#endif
/*****
* HTTP.c : A first attempt at a simple HTTP library.
*
* This file Version	$Revision$
*
* Creation date:		Tue Oct 21 01:41:31 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				Richard Offer
*
* Copyright (C) 1994-1997 by Richard Offer <offer@sgi.com>
* All Rights Reserved
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
* Note from the Author:
*
*	A first attempt at a simple HTTP library, mainly used as a test harness for
*	the forms work in XmHTML, it does lots of bad things and isn't a complete
*	implementation. I didn't use the W3C libww 'cause its too big and doesn't
*	seem to work for POSTs --- rmo
*
*	The code is based on a quick read of the HTTP 1.0 rfc, with ideas for
*	implementation taken from the Chimera Browser.
*
*****/
/*****
* ChangeLog
* $Log$
* Revision 1.1  2011/06/30 16:10:37  rwcox
* Cadd
*
* Revision 1.1  1997/10/23 00:28:23  newt
* Initial Revision
*
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#include <unistd.h>
#include <ctype.h>
#include <errno.h>
#include <sys/time.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>		/* select() */
#endif
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#ifndef SO_RCVTIMEO
#include <setjmp.h>
#include <signal.h>
#endif

#include <http/HTTPP.h>

#ifdef DMALLOC
#include <dmalloc.h>
#endif /* DMALLOC */

#ifdef NEED_SOCKS
/* This is _very_ firewall specific, this works for me (after much trial and
 * error --- offer */
#include "socks.h"

#define connect Rconnect
#endif /* NEED_SOCKS */

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/
#ifdef DEBUG
int http_debug = 0;
#endif

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/

/* delete a no longer required response */
static void deleteResponse(HTTPResponse * res);

/* create a new response */
static HTTPResponse *newResponse(char *buf);

/*****
* hexify src and append to dest. Return value points to the next available
* position in dest.
*****/
static char *appendHex(char *dest, char *src);

/* convert all name-value pairs to a valid QUERY_STRING format */
static char *encodeFormData(HTTPNamedValues * formdata);

#ifndef SO_RCVTIMEO
static void connectTimeout(int signal);
#endif

/*** Private Variable Declarations ***/
#ifndef SO_RCVTIMEO
static jmp_buf http_setjmp_buffer;
#endif

#ifndef SO_RCVTIMEO
static void
connectTimeout(int signal)
{
	if(signal == SIGALRM)
		longjmp(http_setjmp_buffer, 1);
}
#endif

/* This is the main routine for sending a request and getting a response,
 * everything else in this file is waffle */
void
loadHTTPURL(void *unused, HTTPRequest * request, HTTPCookieRequest *cookieReq)
{
	struct hostent *server;
	struct sockaddr_in name;
	int sock;
	char *scheme = NULL, *username = NULL, *password = NULL;
	char *hostname = NULL, *filename = NULL;
	int port;
	char *buf = NULL;
	size_t offset = 0, bufsize = 0;
	HTTPResponse *res;
	ssize_t val;
	fd_set rfds;
	struct timeval tv;
	int retval, retry_count, nreads;
	char	*cookie = NULL ;

	/* see if we have an URI */
	if(request->url == NULL)
	{
		request->ret = HTTPBadURL;
		return;
	}

	/* verify request type */
	if(request->type != HTTPLoadToString &&
		request->type != HTTPLoadToFile)
	{
		request->ret = HTTPBadLoadType;
		return;
	}

	/* resolve the url */
	parseURL(request->url, PARSE_URL, &scheme, &username, &password,
		&hostname, &port, &filename);

re_issue_request:
	/* check protocol */
	if(scheme == NULL || strncasecmp(scheme, "http", 4))
	{
		/* free off the output from parseURL() */
		freeURL(PARSE_URL, scheme, username, password, hostname, port,
			filename);
		request->ret = HTTPBadProtocol;
		return;
	}

#ifdef DEBUG
	if(http_debug)
		fprintf(stderr, "Lookin up host %s...\n", hostname);
#endif

	/* see if we can resolve the host */
	if((server = gethostbyname(hostname)) == NULL)
	{
		freeURL(PARSE_URL, scheme, username, password, hostname, port,
			filename);
		request->ret = HTTPBadHost;
		return;
	}

	/* we've got the host, open a socket */
	if((sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
	{
		freeURL(PARSE_URL, scheme, username, password, hostname, port,
			filename);
		request->ret = HTTPNoSocket;
		return;
	}
#ifdef DEBUG
	if(http_debug)
		fprintf(stderr, "Found, connecting to %s (port %i)\n", hostname, port);
#endif

	name.sin_family = AF_INET;
	name.sin_port = htons(port);

#ifdef linux
	memcpy(&name.sin_addr, server->h_addr, server->h_length);
#else
	memcpy(&name.sin_addr.s_addr, server->h_addr, server->h_length);
#endif

	/*****
	* Wouldn't the world be easy if each system knew SO_RCVTIMEO.
	* But this is not the case on at least linux, so we use a brute force
	* approach: alarm.
	*
	* Just in case your system can enable timeouts on sockets, here's a piece
	* of code that should work.
	*****/
#ifdef SO_RCVTIMEO
	/* set socket timeout */
	tv.tv_sec = request->timeout;
	tv.tv_usec = 0;
	if(setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(struct timeval)))
	{
		freeURL(PARSE_URL, scheme, username, password, hostname, port,
			filename);
		request->ret = HTTPNoSocket;
		close(sock);
		return;
	}
#else
	if(setjmp(http_setjmp_buffer))
	{
		freeURL(PARSE_URL, scheme, username, password, hostname, port,
			filename);
#ifdef DEBUG
		if(http_debug)
			fprintf(stderr, "connect() timed out\n");
#endif
		request->ret = HTTPConnectTimeout;
		signal(SIGALRM, SIG_DFL);
		close(sock);
		return;
	}
	signal(SIGALRM, connectTimeout);
	alarm((long)request->timeout);
#endif

	if(connect(sock, (struct sockaddr*)&name, sizeof(name)) < 0)
	{
		freeURL(PARSE_URL, scheme, username, password, hostname, port,
			filename);

		if(errno == EWOULDBLOCK)
		{
#ifdef DEBUG
			if(http_debug)
				fprintf(stderr, "connect() timed out\n");
#endif
			request->ret = HTTPConnectTimeout;
		}
		else
			request->ret = HTTPNoConnection;
		close(sock);
		return;
	}
#ifndef SO_RCVTIMEO
	/* remove connection timeout */
	signal(SIGALRM, SIG_DFL);
	alarm(0L);
#endif

#ifdef DEBUG
	if(http_debug)
		fprintf(stderr, "sending request (%i)\n", request->method);
#endif

	if( cookieReq != NULL && cookieReq->cookieList != NULL )
		cookie = makeCookie(cookieReq->cookieList);
#ifdef DEBUG
	if( http_debug )
		if( cookie )
			fprintf(stderr,"The server wants a cookie '%s'\n",cookie);
#endif /* _DEBUG */

	switch(request->method)
	{
		case HTTPGET:
		{
			char *formStr = NULL, *reqStr = NULL;

			if(request->form_data)
			{
				formStr = encodeFormData((HTTPNamedValues*)request->form_data);
			}
			reqStr = (char*)malloc(strlen(GET_METHOD) + strlen(filename) +
						(formStr ? strlen(formStr) + 1 : 0) +
						strlen(HTTPVERSIONHDR) + strlen(USER_AGENT) +
						(cookie ? strlen(cookie) + 1 : 0) +
						strlen(NEWLINE) + 3);
			sprintf(reqStr,
					"%s%s%s%s%s%s%s%s",
					GET_METHOD,
					filename,
					(formStr ? "?" : ""),	/* cgi stuff requires a ? */
					(formStr ? formStr : ""),
					HTTPVERSIONHDR,
					USER_AGENT,
					( cookie ? cookie : "" ),
					NEWLINE);
			val = write(sock, reqStr, strlen(reqStr) + 1);
			free(reqStr);
			if(formStr)
				free(formStr);

		}
		break;

		case HTTPPOST:
		{
			char *formStr = NULL, *fullReqStr;

			char *reqStr = (char *) malloc(strlen(POST_METHOD) +
							strlen(filename) + strlen(HTTPVERSIONHDR) +
							(cookie ? strlen(cookie) + 1 : 0) +
							strlen(USER_AGENT) + strlen(NEWLINE) + 2);
			sprintf(reqStr, "%s%s%s%s%s",
					POST_METHOD,
					filename,
					HTTPVERSIONHDR,
					USER_AGENT,
					( cookie ? cookie : "" ) );

			if(request->form_data)
			{
				formStr = encodeFormData((HTTPNamedValues*)request->form_data);
			}
			fullReqStr = calloc(strlen(reqStr) + strlen(CONTENT_LEN) +
							strlen(CONTENT_TYPE) + MAX_FORM_LEN +
							(formStr ? strlen(formStr) : 0 )+ 10 /* safety */,
							sizeof(char));
			sprintf(fullReqStr,
					"%s%s%d%s%s%s%s%s%s",
					reqStr,
					CONTENT_LEN,
					(int)(formStr ? strlen(formStr) : 0),
					NEWLINE,
					CONTENT_TYPE,
					NEWLINE,
					NEWLINE,
					formStr,
					NEWLINE);
			val = write(sock, fullReqStr, strlen(fullReqStr) + 1);

			free(reqStr);
			if(formStr)
				free(formStr);
			free(fullReqStr);
		}
		break;

		case HTTPHEAD:
		/* not sure about cookies for HEAD, supported ? */
		{
			char *reqStr = NULL;

			reqStr = (char*)malloc(strlen(HEAD_METHOD) + strlen(filename) +
						strlen(HTTPVERSIONHDR) + strlen(USER_AGENT) +
						strlen(NEWLINE) + 3);
			sprintf(reqStr,
					"%s%s%s%s%s",
					HEAD_METHOD,
					filename,
					HTTPVERSIONHDR,
					USER_AGENT,
					NEWLINE);
			val = write(sock, reqStr, strlen(reqStr) + 1);
			free(reqStr);
		}
		break;

		default:
			/* free off the output from parseURL() */
			freeURL(PARSE_URL, scheme, username, password, hostname, port,
				filename);
			close(sock);
			request->ret = HTTPMethodUnsupported;
			return;
	}

	/* read output from remote HTTP server */
	offset = 0;
	val = 0;
	bufsize = CHUNKSIZE;
	buf = calloc(bufsize, sizeof(char));
	nreads = 0;
	retry_count = 0;

#ifdef DEBUG
	if(http_debug)
		fprintf(stderr, "awaiting input\n");
#endif

	/* watch socket to see when it has input */
	while(1)
	{
		/* no of bytes read from socket */
		val = 0;

		FD_ZERO(&rfds);
		FD_SET(sock, &rfds);

		/* wait up to the given no of seconds */
		tv.tv_sec = request->timeout;
		tv.tv_usec = 0;

		retval = select(sock+1, &rfds, NULL, NULL, &tv);

		if(retval)
		{
#ifdef DEBUG
			if(http_debug)
				fprintf(stderr, "reading socket.\n");
#endif
			val = read(sock, buf + offset, bufsize - offset);
			if(val <= 0)	/* error or end of input */
				break;

#ifdef DEBUG
			if(http_debug)
				fprintf(stderr, "read %i bytes, offset: %i)\n", val,
					offset);
#endif
			/* keep room for at least CHUNKSIZE bytes */
			if(bufsize - (offset + val) < CHUNKSIZE)
			{
				bufsize += CHUNKSIZE;
				buf = realloc(buf, bufsize);
			}
			offset += val;
			buf[offset] = '\0';		/* NULL terminate */
		}
		else
		{
#ifdef DEBUG
			if(http_debug)
				fprintf(stderr, "timed out after %i seconds.\n",
					request->timeout);
#endif
			/*
			* abort if we're timed out, have reached the maximum retry
			* times and no input was received.
			*/
			if(retry_count == request->retry && offset == 0)
			{
				close(sock);
				free(buf);
				request->ret = HTTPTimeout;
				return;
			}
			/* break out when we have an offset and this read timed out */
			else if(offset && val <= 0)
				break;
			else
			{
				/* read timed out before any input was received */
				retry_count++;
			}
#ifdef DEBUG
			if(http_debug)
				fprintf(stderr, "retrying for the %ith time.\n", retry_count);
#endif
		}
	}

	/* now parse the read message for headers */
	res = newResponse(buf);
	free(buf);

	/* set appropriate return code */
	if(val < 0)
		request->ret = HTTPPartialContent;
	else
		request->ret = res->status_code;

#if defined(PRINT_HDRS) && defined(DEBUG)
	{
		int i;
		for(i = 0; i < res->num_headers; i++)
		{
			printf("hdr %s = %s\n", res->headers[i].name,
				res->headers[i].value);
		}
	}
#endif

	/* valid return code? */
	if(request->ret > 199 && request->ret < 299)
	{
		/* get or post include data, which head does not contain */
		if(request->method != HTTPHEAD)
		{
			int i;

			/* parse the headers for any cookies */
			for (i = 0; i < res->num_headers; i++ )
			{
				if(!strcasecmp(res->headers[i].name, "Set-Cookie"))
					setCookie(cookieReq,SetCookie,res->headers[i].value,
						hostname);
				else if(!strcasecmp(res->headers[i].name, "Set-Cookie2"))
					setCookie(cookieReq, SetCookie2, res->headers[i].value,
						hostname);
			}

			/* store data in string (most likely this was a cgi request) */
			if(request->type == HTTPLoadToString)
			{
				size_t len = (res->data ? strlen((char *) res->data) : 0);
				for (i = 0; i < res->num_headers; i++)
				{
					if(!strcasecmp(res->headers[i].name, "Content-length"))
						len = atoi(res->headers[i].value);
				}
				request->out_data = calloc(len + 1, sizeof(char));
				memcpy((void *) request->out_data, res->data, len);
				request->out_data[len] = '\0';
				request->length = len;
			}
			else if(request->type == HTTPLoadToFile)
			{
				/* this was a request for a remote file. Save it */
				FILE *fp;

				if((fp = fopen((char *) request->in_data, "w")) == NULL)
				{
					request->ret = HTTPCannotCreateFile;
				}
				else
				{
					int i;
					size_t len = (res->data ? strlen((char *) res->data) : 0);
					size_t written;
					for (i = 0; i < res->num_headers; i++)
					{
						if(!strcasecmp(res->headers[i].name, "Content-length"))
							len = atoi(res->headers[i].value);
					}
					/* flush data */
					written = fwrite(res->data, sizeof(char), len, fp);
					fflush(fp);
					fclose(fp);
				}
			}
		}
		else
		{
			/* store data in string (most likely this was a cgi request) */
			if(request->type == HTTPLoadToString)
			{
				/*****
				* Transfer header array from the result structure to the
				* request.
				*****/
				request->headers = res->headers;
				request->num_headers = res->num_headers;
				res->headers = NULL;
				res->num_headers = 0;
			}
			else if(request->type == HTTPLoadToFile)
			{
				/* this was a request for a remote file. Save it */
				FILE *fp;

				if((fp = fopen((char *) request->in_data, "w")) == NULL)
				{
					request->ret = HTTPCannotCreateFile;
				}
				else
				{
					int i;
					for (i = 0; i < res->num_headers; i++)
					{
						fprintf(fp, "%s = %s\n", res->headers[i].name,
							res->headers[i].value);
					}
					/* flush data */
					fflush(fp);
					fclose(fp);
				}
			}
		}
	}
	else
	{
		/*****
		* if the URL has moved (_or_ the user left off a trailing '/' from a
		* directory request), then look in the Location: header for the
		* correct URL and re-issue the request	--- offer dec 97
		*****/
		if(request->ret == 301 || request->ret == 302 )
		{
			int i;
			for(i=0; i<  res->num_headers; i++)
			{
				if(!strcasecmp(res->headers[i].name, "location"))
				{
					freeURL(PARSE_URL, scheme, username, password, hostname,
						port, filename);

					parseURL(res->headers[i].value, PARSE_URL, &scheme,
						&username, &password, &hostname, &port, &filename);
					free(request->url);
					/*****
					* Update the URL that was requested to point to the
					* correct one
					*****/
					request->url = NewString(res->headers[i].value);
					goto re_issue_request;

				}
			}
		}
	}
	deleteResponse(res);

	/* free off the output from parseURL() */
	freeURL(PARSE_URL, scheme, username, password, hostname, port, filename);

	/* all done */
	close(sock);
}

void
deleteHTTPRequest(HTTPRequest * req)
{
	int i;

	if(req->in_data)
		free(req->in_data);

	if(req->form_data)
	{
		i = 0;
		while (req->form_data[i].name != NULL)
		{
			if(req->form_data[i].name)
				free(req->form_data[i].name);
			if(req->form_data[i].value)
				free(req->form_data[i].value);
			i++;
		}
		free(req->form_data);
	}

	for(i = 0; i < req->num_headers; i++)
	{
		if(req->headers[i].name)
			free(req->headers[i].name);

		if(req->headers[i].value)
			free(req->headers[i].value);
	}
	if(req->headers)
		free(req->headers);

	if(req->out_data)
		free(req->out_data);
	if(req->url)
		free(req->url);

	free(req);

}

HTTPRequest *
newHTTPRequest(void)
{
	HTTPRequest *new_r = (HTTPRequest *) calloc(1, sizeof(HTTPRequest));

	new_r->type = HTTPLoadToString;
	new_r->in_data = NULL;
	new_r->form_data = NULL;
	new_r->out_data = NULL;
	new_r->method = HTTPGET;
	new_r->url = NULL;
	new_r->ret = HTTPInvalid;
	new_r->timeout = DEFAULT_TIMEOUT;
	new_r->retry   = DEFAULT_RETRY;
	new_r->headers = NULL;
	new_r->num_headers = 0;

	return(new_r);
}

static void
deleteResponse(HTTPResponse * res)
{
	int i;

	if(res->data)
		free(res->data);

	for(i = 0; i < res->num_headers; i++)
	{
		if(res->headers[i].name)
			free(res->headers[i].name);

		if(res->headers[i].value)
			free(res->headers[i].value);
	}
	if(res->headers)
		free(res->headers);

	free(res);

}

/*****
* unescape HTTP escaped chars.
* Replacement is done inline.
*****/
void
HTTPUnescapeResponse(char *buf)
{
	register unsigned int x, y;
	register char digit;

	for(x = 0, y = 0; buf[y]; ++x, ++y)
	{
		if((buf[x] = buf[y]) == '%')
		{
			y++;
			digit = (buf[y] >= 'A' ? ((buf[y] & 0xdf)-'A')+10 : (buf[y]-'0'));
			y++;
			digit *= 16;
			digit += (buf[y] >= 'A' ? ((buf[y] & 0xdf)-'A')+10 : (buf[y]-'0'));
			buf[x] = digit;
		}
	}
	buf[x] = '\0';
}

static HTTPResponse *
newResponse(char *buf)
{

	HTTPResponse *res = (HTTPResponse *) calloc(1, sizeof(HTTPResponse));
	int ver, code;
	int i, start;
	int SOL;
	size_t len = 0;
	char *EOL;

	if(strncasecmp(buf, "HTTP", 4))
	{
		res->http_version = HTTP_VERSION_09;
		res->headers = NULL;
		res->num_headers = 0;
		res->status_code = HTTPInvalid;
		res->data = (unsigned char *) NewString(buf);

		return res;
	}
	sscanf(buf, "HTTP/1.%d %d", &ver, &code);

	EOL = strstr(buf, "\r\n");
	start = EOL - buf + 2;
#ifdef DEBUG
	if(http_debug)
		fprintf(stderr, "\nHTTP 1.%d return code = %d\n", ver, code);
#endif

	if(ver == 0)
		res->http_version = HTTP_VERSION_10;
	else
		res->http_version = HTTP_VERSION_11;

	res->status_code = (HTTPRequestReturn) code;

	for (i = start, SOL = start; i < strlen(buf); i++)
	{
		if(buf[i] == '\r' || buf[i] == '\n')
		{
			if(buf[i] == '\r' && buf[i + 1] && buf[i + 1] == '\n')
			{
				char *colon = strchr(&buf[SOL], ':');

				if(colon == NULL)
					break;

				if(res->headers == NULL)
					res->headers =
						(HTTPNamedValues *) malloc(sizeof(HTTPNamedValues));
				else
					res->headers = realloc((void *)res->headers,
						sizeof(HTTPNamedValues) * (res->num_headers + 1));

				res->headers[res->num_headers].name = NewNString(&buf[SOL],
						colon - &buf[SOL]);
				res->headers[res->num_headers].value = NewNString(colon + 2,
						&buf[i] - colon - 2);
				if(!strcasecmp(res->headers[res->num_headers].name,
					"Content-length"))
					len = atoi(res->headers[res->num_headers].value);

				res->num_headers++;

				if(buf[i + 2] && buf[i + 2] == '\r' &&
					buf[i + 3] && buf[i + 3] == '\n')
				{
					if(len == 0)
						len = strlen(&buf[i + 4]);

					res->data = calloc(len + 1, sizeof(char));
					memcpy((void *) res->data, &buf[i + 4], len);
					res->data[len] = '\0';

					goto finish;
				}
				i++;
			}
			SOL = i + 1;
		}
	}
  finish:

	return(res);
}

/*****
* Fast lookup table to determine which characters should be left alone and
* which should be encoded. Much faster than the Chimera implementation -- kdh
* const qualifier should put it in the text segment
*****/
static const unsigned char allow[97] =
{/* 0 1 2 3 4 5 6 7 8 9 A B C D E F */
	0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,1,	/* 2x   !"#$%&'()*+,-./  */
	1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,	/* 3x  0123456789:;<=>?  */
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,	/* 4x  @ABCDEFGHIJKLMNO  */
	1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,	/* 5X  PQRSTUVWXYZ[\]^_  */
	0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,	/* 6x  `abcdefghijklmno  */
	1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0 	/* 7X  pqrstuvwxyz{\}~  DEL */
};

static const char *hex = "0123456789ABCDEF";

/*****
* Name:			appendHex
* Return Type: 	char*
* Description: 	appends src to dest, translating certain chars to their
*				hexadecimal representation as we do;
* In:
*	dest:		destination buffer. This buffer must be large enough to contain
*				the expanded source text;
*	src:		text to be appended;
* Returns:
*	a ptr pointing to the next available position in dest.
* Note:
*	added 97/10/21 by kdh and based on HTEscape() from libwww
*****/
static char*
appendHex(char *dest, char *src)
{
	register char *ptr, *chPtr;

	for(ptr = dest, chPtr = src; *chPtr!= '\0'; chPtr++)
	{
		/* no negative values */
		int c = (int)((unsigned char)(*chPtr));
		if(*chPtr == ' ')	/* bloody exception */
			*ptr++ = '+';
		else if(c >= 32 && c <= 127 && allow[c-32])
			*ptr++ = *chPtr; /* acceptable char */
		else
		{
			*ptr++ = '%';	/* hex is following */
			*ptr++ = hex[c >> 4];
			*ptr++ = hex[c & 15];
		}
	}
	return(ptr);
}

/*****
* Name: 		encodeFormData
* Return Type: 	char*
* Description: 	creates a fully valid QUERY_STRING from the given name/value
*				pairs.
* In:
*	formdata:	array of name/value pairs from a form submit. Encoding
*				terminates when a NULL name has been detected.
* Returns:
*	an allocated and hex-encoded QUERY_STRING.
* Note:
*	- this function is based on the corresponding one from Chimera (rmo)
*	- 97/10/21, heavily modified by kdh
*****/
static char*
encodeFormData(HTTPNamedValues * formdata)
{
	char *data, *chPtr;
	int nvalues, i, len = 0;

	/*****
	* First count how many bytes we have to allocate. Each entry gets two
	* additional bytes: the equal sign and a spacer. Each entry is also
	* multiplied by three to allow full expansion.
	* Count no of entries as well.
	*****/
	for(i = 0; formdata[i].name != NULL; i++)
	{
		if(formdata[i].name)
		{
			len += strlen(formdata[i].name) * 3;
			if(formdata[i].value)
				len += strlen(formdata[i].value) * 3;
			len += 2;	/* equal sign and spacer */
		}
	}
	nvalues = i;
	/* allocate & reset query string */
	data = (char*)calloc(len + 1, sizeof(char));

	/*****
	* Now compose query string: append & convert to hex at the same time.
	* We can safely do this as we've already allocated room for hexadecimal
	* expansion of the *entire* query string.
	* Room for optimisation: appendHex could be done inline
	*****/
	chPtr = data;
	for(i = 0; i < nvalues; i++)
	{
		if(formdata[i].name)
		{
			chPtr = appendHex(chPtr, formdata[i].name);
			*chPtr++ = '=';
			if(formdata[i].value)
				chPtr = appendHex(chPtr, formdata[i].value);
			*chPtr++ = '&';	/* spacer */
		}
	}
	/* mask off last & */
	data[strlen(data)-1] = '\0';

#ifdef DEBUG
	if(http_debug)
	{
		fprintf(stderr, "encodeFormData, computed string length: %i, "
			"used: %i\n", len+1, strlen(data));
		fprintf(stderr, "return value: %s\n", data);
	}
#endif

	/*****
	* Could resize data to fit exactly the no of bytes used, but I wonder
	* if it's worth it as this data has a pretty short lifetime --- kdh
	*****/

	return(data);
}

/*
 * stolen from the chimera browser --- rmo.
 *
 */
#define isspace8(a) ((a) < 33 && (a) > 0)

void
parseURL(char *url, long parse, char **scheme, char **username,
	char **password, char **hostname, int *port, char **filename)
{
	char *start;
	char *colon, *slash, *fslash;
	char *at;					/* username/password @ */
	char *ucolon;				/* username colon */
	char *pcolon;				/* port number colon */
	struct _part {
		int start;
		int len;
	} sp, up, pwp, hp, pp, fp;

	sp.start = 0;
	sp.len = 0;
	up.start = 0;
	up.len = 0;
	pwp.start = 0;
	pwp.len = 0;
	hp.start = 0;
	hp.len = 0;
	pp.start = 0;
	pp.len = 0;
	fp.start = 0;
	fp.len = 0;

	if(url == NULL)
		return;

	/* skip leading white-space (if any) */
	for (start = url; isspace8(*start); start++);

	/* Look for indication of a scheme. */
	colon = strchr(start, ':');

	/*
	 * Search for characters that indicate the beginning of the
	 * path/params/query/fragment part.
	 */
	slash = strchr(start, '/');
	if(slash == NULL)
		slash = strchr(start, ';');
	if(slash == NULL)
		slash = strchr(start, '?');
	if(slash == NULL)
		slash = strchr(start, '#');

	/*
	 * Check to see if there is a scheme.  There is a scheme only if
	 * all other separators appear after the colon.
	 */
	if(colon != NULL && (slash == NULL || colon < slash))
	{
		sp.start = 0;
		sp.len = colon - start;
	}
	/*
	 * If there is a slash then sort out the hostname and filename.
	 * If there is no slash then there is no hostname but there is a
	 * filename.
	 */
	if(slash != NULL)
	{
		/* Check for leading //. If its there then there is a host string. */
		if((*(slash + 1) == '/') && ((colon == NULL && slash == start) ||
								(colon != NULL && slash == colon + 1)))
		{
			/* Check for filename at end of host string */
			slash += 2;
			if((fslash = strchr(slash, '/')) != NULL)
			{
				hp.start = slash - start;;
				hp.len = fslash - slash;
				fp.start = fslash - start;
				fp.len = strlen(fslash);
			}
			else
			{	/* there is no filename */
				hp.start = slash - start;
				hp.len = strlen(slash);
			}
		}
		else
		{
			/*
			 * the rest is a filename because there is no // or it appears
			 * after other characters
			 */
			if(colon != NULL && colon < slash)
			{
				fp.start = colon + 1 - start;
				fp.len = strlen(colon + 1);
			}
			else
			{
				fp.start = slash - start;
				fp.len = strlen(slash);
			}
		}
	}
	else
	{
		/* No slashes at all so the rest must be a filename */
		if(colon == NULL)
		{
			fp.start = 0;
			fp.len = strlen(start);
		}
		else
		{
			fp.start = colon - start + 1;
			fp.len = strlen(colon + 1);
		}
	}

	/*
	 * If there is a host string then divide it into
	 * username:password@hostname:port as needed.
	 */
	if(hp.len != 0)
	{
		/* Look for username:password. */
		if((at = strchr(&url[hp.start], '@')) != NULL)
		{

			up.start = hp.start;
			up.len = at - start - hp.start;

			hp.start = at + 1 - start;

			if((ucolon = strchr(&url[up.start], ':')) != NULL)
			{
				if(ucolon - start < hp.start)
				{
					pwp.start = ucolon + 1 - start;
					pwp.len = hp.start - pwp.start;
				}
			}
		}
		/* Grab the port. */
		if((pcolon = strchr(&url[hp.start], ':')) != NULL &&
			pcolon < ( &url[hp.start + hp.len]) )
		{
			pp.start = pcolon + 1 - start;
			pp.len = fp.start - pp.start;
			hp.len -= pp.len + 1;
		}
	}

	/* now have all the fragments, make them into strings */

	if(parse & PARSE_SCHEME)
	{
		if(sp.len > 0)
			*scheme = NewNString(&url[sp.start], sp.len);
		else
			*scheme = NULL;
	}
	if(parse & PARSE_USER)
	{
		if(up.len > 0)
			*username = NewNString(&url[up.start], up.len);
		else
			*username = NULL;
	}
	if(parse & PARSE_PASSWORD)
	{

		if(pwp.len > 0)
			*password = NewNString(&url[pwp.start], pwp.len);
		else
			*password = NULL;
	}
	if(parse & PARSE_HOSTNAME)
	{
		if(hp.len > 0)
			*hostname = NewNString(&url[hp.start], hp.len);
		else
			*hostname = NULL;
	}
	if(parse & PARSE_PORT)
	{
		if(pp.len > 0)
		{
			char *tmp = NewNString(&url[pp.start], pp.len);
			*port = atoi(tmp);
			free(tmp);
		}
		else
			*port = 80;
	}
	if(parse & PARSE_FILENAME)
	{
		if(fp.len > 0)
			*filename = NewString(&url[fp.start]);
		else
			*filename = NewString("/");
	}
	return;
}

/* this is brain dead, needs to be expanded to cover non http schemes -- rmo */

int
HTTPAbsoluteURL(char *url)
{
	if(strncasecmp(url, "http", 4))
		return(0);
	else
		return(1);
}

/* This is a very flakey routine and it needs a lot of work, it doesn't
   do compression of full paths, but it proved adequet for simple testing */

char *
HTTPFindAbsoluteURL(char *url, char *baseUrl)
{
	char new_url[1024];
	char *tmpP;

	char *u_scheme, *u_username, *u_password, *u_hostname, *u_filename;
	char *b_scheme, *b_username, *b_password, *b_hostname, *b_filename;
	int u_port, b_port;

	if(baseUrl == NULL || *baseUrl == '\0')
		return (NewString(url));

	parseURL(url, PARSE_URL, &u_scheme, &u_username, &u_password,
		&u_hostname, &u_port, &u_filename);

	parseURL(baseUrl, PARSE_URL, &b_scheme, &b_username, &b_password,
		&b_hostname, &b_port, &b_filename);

	if(u_scheme)
		sprintf(new_url, "%s://", u_scheme);
	else
		sprintf(new_url, "%s://", b_scheme);

	if(u_hostname)
		strcat(new_url, u_hostname);
	else if(b_hostname)
		strcat(new_url, b_hostname);
	else
		strcat(new_url, "localhost");

	if(u_filename && u_filename[0] == '/')
	{
		strcat(new_url, u_filename);
	}
	else if(u_filename && u_filename[0] == '~')
	{
		strcat(new_url, u_filename);
		strcat(new_url, "/");
	}
	else
	{
		if(b_filename == NULL || b_filename[0] != '/')
			printf("still to do\n");
		else
		{
			strcat(new_url, b_filename);
			tmpP = strrchr(new_url, '/');
			if(*tmpP++)
			{
				*tmpP = '\0';
				strcat(tmpP, u_filename);
			}
			else
				strcat(new_url, u_filename);
		}
	}
	freeURL(PARSE_URL, u_scheme, u_username, u_password, u_hostname,
		u_port, u_filename);

	freeURL(PARSE_URL, b_scheme, b_username, b_password, b_hostname,
		b_port, b_filename);

	return (NewString(new_url));
}

void
freeURL(long parse, char *scheme, char *username, char *password,
	char *hostname, int port, char *filename)
{

	if((parse & PARSE_SCHEME) && scheme)
		free(scheme);

	if((parse & PARSE_USER) && username)
		free(username);

	if((parse & PARSE_PASSWORD) && password)
		free(password);

	if((parse & PARSE_HOSTNAME) && hostname)
		free(hostname);

	if((parse & PARSE_FILENAME) && filename)
		free(filename);
}

void
HTTPError(char *msg, HTTPRequestReturn error)
{
	fprintf(stderr, "%s: %s.\n", msg, HTTPErrorString(error));
}

const char*
HTTPErrorString(HTTPRequestReturn error)
{
	switch(error)
	{
		/* 0 and up (client messages) */
		case HTTPInvalid:
			return("Invalid request (client failure)");
		case HTTPBadProtocol:
			return("Invalid protocol requested (client failure)");
		case HTTPBadHost:
			return("Invalid hostname (client failure)");
		case HTTPBadURL:
			return("Invalid URL (client failure)");
		case HTTPBadLoadType:
			return("Invalid load type (client failure)");
		case HTTPMethodUnsupported:
			return("Unsupported method (client failure)");
		case HTTPNoSocket:
			return("Could not open socket (client failure)");
		case HTTPNoConnection:
			return("Not connected (client failure)");
		case HTTPBadHttp10:
			return("Invalid HTTP/1.0 request (client failure)");
		case HTTPCannotCreateFile:
			return("Could not create file (client failure)");
		case HTTPConnectTimeout:
			return("Could not connect: timed out (client failure)");
		case HTTPTimeout:
			return("Connection timed out");

		/* 100 and up (informative messages) */
		case HTTPContinue:
			return("Continue");
		case HTTPSwitchProtocols:
			return("Bad protocol, switch required");

		/* 200 and up (request succeeded) */
		case HTTPSuccess:
			return("No error");
		case HTTPCreated:
			return("Document created");
		case HTTPAccepted:
			return("Request accepted");
		case HTTPNonAuthoritativeInfo:
			return("Non-authoritative information");
		case HTTPNoContent:
			return("Document is empty");
		case HTTPResetContent:
			return("Content has been reset");
		case HTTPPartialContent:
			return("Partial content");

		/* 300 and up (non-fatal errors, retry possible) */
		case HTTPMultipleChoices:
			return("Request not unique, multiple choices possible");
		case HTTPPermMoved:
			return("Document has been permanently removed");
		case HTTPTempMoved:
			return("Document has been temporarely moved");
		case HTTPSeeOther:
			return("Site has move");
		case HTTPNotModified:
			return("Document not modified since last access");
		case HTTPUseProxy:
			return("Document only accessible through proxy");

		/* 400 and up (fatal request errors) */
		case HTTPBadRequest:
			return("Invalid HTTP request");
		case HTTPUnauthorised:
			return("Client not authorized");
		case HTTPPaymentReq:
			return("Payment required");
		case HTTPForbidden:
			return("Access forbidden");
		case HTTPNotFound:
			return("Document not found");
		case HTTPMethodNotAllowed:
			return("Access method not allowed");
		case HTTPNotAcceptable:
			return("Unacceptable request");
		case HTTPProxyAuthReq:
			return("Proxy authorization required");
		case HTTPRequestTimeOut:
			return("Timed out");
		case HTTPConflict:
			return("Conflict of interest");
		case HTTPGone:
			return("Document has moved");
		case HTTPLengthReq:
			return("Invalid request length");
		case HTTPPreCondFailed:
			return("Condition failed");
		case HTTPReqEntityTooBig:
			return("Request entity too large");
		case HTTPURITooBig:
			return("URI specification too big");
		case HTTPUnsupportedMediaType:
			return("Unsupported media type");

		/* 500 and up (server errors) */
		case HTTPInternalServerError:
			return("Internal server error");
		case HTTPNotImplemented:
			return("Method not implemented");
		case HTTPBadGateway:
			return("Invalid gateway");
		case HTTPServiceUnavailable:
			return("Service unavailable");
		case HTTPGatewayTimeOut:
			return("Gateway timed out");
		case HTTPHTTPVersionNotSupported:
			return("Unsupported HTPP version");

		default:
			return("unknown error");
	}
}
