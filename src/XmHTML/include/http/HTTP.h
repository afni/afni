/*****
* HTTP.h : Public header file for HTTP.c, a simple HTTP/1.0 implementation.
*
* This file Version	$Revision$
*
* Creation date:		Tue Oct 21 01:56:19 GMT+0100 1997
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
* Revision 1.1  1997/10/23 00:28:26  newt
* Initial Revision
*
*****/ 

#ifndef _HTTP_h_
#define _HTTP_h_

#define HTTPVERSION	0
#define HTTPREVISION	1
#define HTTPUPDATE_LEVEL 1
#define HTTPVersion \
	(HTTPVERSION * 1000 + HTTPREVISION * 100 + HTTPUPDATE_LEVEL)

/* used by Imake to get Shared library version numbering */
#ifndef _LIBRARY

#define LIBHTTPVERSION_STRING \
	"Client/HTTP Version 0.1.1 (C)Richard Offer and Ripley Software Development"

/*****
* Feel free to re-define the following, this is passed to every web-server 
* during all requests. 
* If you do change this, be careful (bad editing may lead to failed requests) 
* the format is
* User-Agent: <space> (manditory)
* <client identifier,most significant parts first> / <version number>
* <space>
* <extra fields, for development software, a point of contact is reccommended
* \r\n terminator
* 
* wrapped in a #ifdef so an application can define it before #include'ing 
* this file
*****/
#ifndef USER_AGENT
#define USER_AGENT "User-Agent: XmHTML-HTTP/0.1.x ripley@xs4all.nl\r\n"
#endif /* USER_AGENT */	

typedef enum {
	HTTPLoadToFile,
	HTTPLoadToString
} HTTPLoadType;

typedef enum {
	HTTPGET,
	HTTPPOST,
	HTTPHEAD
} HTTPLoadMethod;

/* possible HTTP return values */
typedef enum {
	/* our own error values */
	HTTPInvalid					= 0,
	HTTPBadProtocol				= 1,
	HTTPBadHost					= 2,
	HTTPBadURL					= 3,
	HTTPBadLoadType				= 4,
	HTTPMethodUnsupported		= 5,
	HTTPNoSocket				= 6,
	HTTPNoConnection			= 7,
	HTTPBadHttp10				= 8,
	HTTPCannotCreateFile		= 9,
	HTTPConnectTimeout			= 10,
	HTTPTimeout					= 11,

	/* Now 'Real' HTTP return codes */
	HTTPContinue				= 100,
	HTTPSwitchProtocols			= 101,

	HTTPSuccess					= 200,
	HTTPCreated					= 201,
	HTTPAccepted				= 202,
	HTTPNonAuthoritativeInfo	= 203,
	HTTPNoContent				= 204,
	HTTPResetContent			= 205,
	HTTPPartialContent			= 206,

	HTTPMultipleChoices			= 300,
	HTTPPermMoved				= 301,
	HTTPTempMoved				= 302,
	HTTPSeeOther				= 303,
	HTTPNotModified				= 304,
	HTTPUseProxy				= 305,

	HTTPBadRequest				= 400,
	HTTPUnauthorised			= 401,
	HTTPPaymentReq				= 402,
	HTTPForbidden				= 403,
	HTTPNotFound				= 404,
	HTTPMethodNotAllowed		= 405,
	HTTPNotAcceptable			= 406,
	HTTPProxyAuthReq			= 407,
	HTTPRequestTimeOut			= 408,
	HTTPConflict				= 409,
	HTTPGone					= 410,
	HTTPLengthReq				= 411,
	HTTPPreCondFailed			= 412,
	HTTPReqEntityTooBig			= 413,
	HTTPURITooBig				= 414,
	HTTPUnsupportedMediaType	= 415,

	HTTPInternalServerError		= 500,
	HTTPNotImplemented			= 501,
	HTTPBadGateway				= 502,
	HTTPServiceUnavailable		= 503,
	HTTPGatewayTimeOut			= 504,
	HTTPHTTPVersionNotSupported	= 505

} HTTPRequestReturn;

/* flags for parseURL */
typedef enum {
	PARSE_SCHEME	= 1,
	PARSE_USER		= 2,
	PARSE_PASSWORD	= 4,
	PARSE_HOSTNAME	= 8,
	PARSE_PORT		= 16,
	PARSE_FILENAME	= 32
}HTTPParseURL;

#define PARSE_URL (PARSE_SCHEME|PARSE_HOSTNAME|PARSE_PORT|PARSE_FILENAME)

/*****
* Used internally for HTTP headers and externally for the form data 
* name/value pairs 
*****/
typedef struct _HTTPNamedValues {
	char *name;
	char *value;
} HTTPNamedValues;

/*****
* Definition of a requestor.
* *never* store static data in the ptr fields as deleteHTTPRequest explicitly
* frees any non-NULL fields.
*****/
typedef struct _HTTPRequest {
	HTTPLoadType type;			/* load to string or file			*/
	char *in_data;				/* filename for LoadToFile			*/
	HTTPNamedValues *form_data;	/* data for form processing			*/
	unsigned char *out_data;	/* response string					*/
	size_t length;				/* length of out_data, from
								 * Content-Length or strlen
								 */
	HTTPLoadMethod method;		/* get, post etc					*/
	char *url;					/* fully qualified location			*/

	int timeout;				/* select() timeout in seconds		*/
	int retry;					/* retry count, zero based			*/

	HTTPNamedValues *headers;	/* array of returned headers		*/
	int num_headers;			/* no of headers					*/

	HTTPRequestReturn ret;		/* Server return value				*/
}HTTPRequest;

/* type of cookies 
 *   Not a lot an application can do here, it all depends on what the 
 *   server sends.
 *   
 *   SetCookie implies the server is using Netscape style cookies.
 *   SetCookie means the server is using RFC2109 style cookies.
 */
enum { SetCookie, SetCookie2 }; 

/* type of cookie file 
 *   For safety I am only going to support the writting of CookieJar files
 *   however, I will allow an application to read cookie files generated by 
 *   netscape.
 *   
 *   CookieJar (a name I've just invented) files are much richer than Netscape 
 *   ones, they have to be they store the full SetCookie2 response. 
 */
enum { NetscapeCookieFile = 1, CookieJar = 2 } ;  

typedef struct _HTTPCookie {
	
	HTTPNamedValues	cookie;
	char	type;
	char	*comment;	/* this and the url are not preserved between sessions*/
	char	*commentURL;
	int		discard;
	char	*domain;
	int		exactHostMatch;
	int		secure;		/* not supported in HTTP.c */
	char	*path;
	int		expires;
	char	*port;
	int		version;

} HTTPCookie; 

typedef struct _HTTPCookieList {
	
	HTTPCookie	*cookie;
	struct _HTTPCookieList *next;

} HTTPCookieList;

typedef struct _HTTPCookieRequest {
	
	HTTPCookieList *cookieList;
	HTTPCookieList *setCookie;
	
	int	sendCookie;
	
} HTTPCookieRequest;


typedef struct _HTTPCookieCache {

	HTTPCookie	**cookies;
	int			ncookies;

	char		*filename;	
	char		fileType;

} HTTPCookieCache ;


/*****
* Create a new, empty, request instance. This is the only safe way to create
* a request. 
*****/
extern HTTPRequest *newHTTPRequest(void);

/* make a request for some URI */
extern void loadHTTPURL(void *unused, HTTPRequest * request, HTTPCookieRequest *cookieReq);

/*****
* Clear a no longer needed request instance. Deletes any field that have
* been allocated.
*****/
extern void deleteHTTPRequest(HTTPRequest * req);

/* split a full URI into separate fields */
extern void parseURL(char *url, long parseflag, char **scheme,
	char **username, char **password, char **hostname, int *port,
	char **filename);

/* free fields allocated by parseURL */
extern void freeURL(long parseflag, char *scheme, char *username,
	char *password, char *hostname, int port, char *filename);

/* Determine whether or not the given url should be retrieved via HTTP. */
extern int HTTPAbsoluteURL(char *url);

/*****
* Compose a full URI from a (possibly local) url and the base url for this
* document. Return value should be freed by caller unless the return value
* is stored in a HTTPRequest in which case it will be freed when
* deleteHTTPRequest is called.
*****/
extern char *HTTPFindAbsoluteURL(char *url, char *baseUrl);

/* display an error message for the given error code */
extern void HTTPError(char *msg, HTTPRequestReturn error);

extern const char *HTTPErrorString(HTTPRequestReturn error);

/* unescape a HTTP escaped string */
void HTTPUnescapeResponse(char *buf);

/* load the cookie file into memory, a cookieCache is self-contained so 
 * multiple files can be read within an app. 
 */ 
extern HTTPCookieCache * loadCookieFileToCache(char * filename, char fileType);

/* The procedure for using cookies
 * 
 *  1) load the cookie cache.
 *  2) for a given URL, call getCookieFromCache(), this returns a 
 *     CookieRequest, req->cookieList is a list of all the cookies that apply 
 *     to the the URL (it will be NULL if no cookies are wanted by the server).
 *  3) at this point the application can step in and inform the user that a 
 *     cookie is wanted by the server, if the user doesn't want to send the 
 *     cookie, it should set cookieRequest->sendCookie to false _before_ calling
 *     loadHTTPUrl() (this is better than simply passing NULL into 
 *     loadHTTPUrl() since it gives tha application chnace to recieve any new 
 *     cookies.
 *  4) on return from loadHTTPUrl() cookieRequest->setCookie will be non-null 
 *     if the server sent a save cookie response. The application can then 
 *     decide to save this cookie for the rest of the session by calling 
 *     addCookieListToCache (passing in cookieRequest->setCookie).
 *  5) for permanant saving of the cookies the application needs to call    
 *     writeCookieCache();
 */

/* 
 * Notes.
 * 
 * url must be fully qualified, CookieList is a linked-list of Cookies, all 
 * that apply to the url.
 * 
 * To save the cookies between sessions, use writeCookieCache(). This writes 
 * cache back out (including any new cookies set during the session). Note that
 * for safety, I'm not going to allow for the writing of Netscape cookie files 
 * (but using mergeCookieCache() we can read the existing Netscape cookie file
 * and merge it with a custom cookie file (makes it easier moving to a new 
 * browser from netscape.
 * ---if pushed, this may change later.
 */  

extern HTTPCookieRequest *getCookieFromCache(HTTPCookieCache *, char * url);

extern void addCookieListToCache(HTTPCookieCache *, HTTPCookieList *);

extern void writeCookieCache(HTTPCookieCache *cache);

/* take the cookies from cache c2 and add them into c1.
 *   The reason for this is so that you can open an empty CookieJar file _and_
 *   a Netscape cookie file (in that order), then take the cookies from the 
 *   Netscape file and save them into the CookieJar file (since I don't
 *   support the writing of Netscape cookie files
 *   
 *   Do not free c2 
 */ 
extern void mergeCookieCache(HTTPCookieCache *c1, HTTPCookieCache *c2);

void freeCookieCache(HTTPCookieCache * , int /* free cookies ? */);
void freeCookieRequest(HTTPCookieRequest *);

/* convenience macros */
#define NewNString(STR,len) \
	((STR) != NULL ? (strncpy(calloc(len+1,sizeof(char)), STR,(len))) : NULL)

#define NewString(STR) \
	((STR) != NULL ? (strcpy(malloc(strlen(STR)+1),STR)) : NULL)

#define	stringToBoolean(str) \
	((str) != NULL && *(str) ? ( *(str) == 'T' || *(str) == 't' || *(str) == 'y' ? 1: 0 ) : 0)

#endif /* _LIBRARY */

/* Don't add anything after this endif! */
#endif /* _HTTP_h_ */
