
#ifndef lint
static char rcsId[] = "$Header$";
#endif	/* lint */

/*
 * Cookie support for the simple HTTP library.
 * 
 * Copyright (c) 1997-1998 Richard Offer <offer@sgi.com>
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
 */

/* Author's Note
 *  Cookies are currently undergoing standardisation, the current accepted 
 *  practise is based on Netscapes implementation. The IETF has issued 
 *  RFC2109 in Feb97, its been ammended since then, and I'm working from 
 *  the 21Nov97 version. I expect it to change before its issued. 
 *  
 *  Pending the widespread acceptance of a standard, I've taken a few 
 *  short-cuts, namely:
 *  
 *  1) no support to restrict the ports of a cookie.
 *  2) interaction between Netscape and RFC2109 cookies isn't 100%
 *  
 *  Watch this space...
 */

/* ChangeLog 
 * 
$Log$
Revision 1.2  2012/03/01 17:56:31  ziad
Cput

Revision 1.1  2011/06/30 16:10:37  rwcox
Cadd
 
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h> /* for malloc */
#include <string.h> /* for strstr() */
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#include <time.h>	/* for mktime() */

#include <http/HTTPP.h>

#ifdef DMALLOC
#include <dmalloc.h>
#endif /* DMALLOC */

#ifndef True
#define True (1)
#endif /* True */

#ifndef False
#define False (0)
#endif /* False */

/* static function decls */
static HTTPCookie * newCookie(void);
static void freeCookie(HTTPCookie *);
static HTTPCookieList * newCookieList(void);
static HTTPCookieRequest * newCookieRequest(void);


static HTTPCookie *
 newCookie()
{
	HTTPCookie *new_c = (HTTPCookie *) calloc(1, sizeof(HTTPCookie));

	return (new_c);
}

void
freeCookie(HTTPCookie *cookie)
{
	if ( cookie ) {
		if ( cookie->domain)
			free(cookie->domain);
		
		if ( cookie->comment)
			free(cookie->comment);

		if ( cookie->commentURL)
			free(cookie->commentURL);
		
		if ( cookie->cookie.name)
			free(cookie->cookie.name);
		
		if ( cookie->cookie.value)
			free(cookie->cookie.value);
		
		if ( cookie->path)
			free(cookie->path);
		
		if ( cookie->port)
			free(cookie->port);
		
		free(cookie);
	}
	
	cookie = NULL;
}

/* This doesn't free the actual cookie, just the linked list */
static void
freeCookieList(HTTPCookieList *list)
{
	HTTPCookieList *tmp = list;
	HTTPCookieList *tmp2;

	if(!list)
		return;

	while ( tmp->next ) {
		tmp2 = tmp->next;
		free(tmp);
		tmp = tmp2;
	}
	if ( tmp )
		free(tmp);
}

void
freeCookieRequest(HTTPCookieRequest *req)
{
	
	if ( req->cookieList )
		freeCookieList(req->cookieList);

	if ( req->setCookie )
		freeCookieList(req->setCookie);
			
	free( req);
			
}

void
freeCookieCache(HTTPCookieCache *cache, int free_cookies)
{
	int i;

	if(cache == NULL)
		return;
	
	if ( free_cookies ) { 
		for (i=0; i< cache->ncookies ; i++ )
			freeCookie(cache->cookies[i]);
	}
	
	free( (void *) cache->cookies );
	free( (void *) cache->filename );
	
	
	free(cache);
}


static HTTPCookieRequest *
newCookieRequest()
{

	HTTPCookieRequest *new_c = (HTTPCookieRequest *) calloc(1, 
			sizeof(HTTPCookieRequest) );

	new_c->sendCookie = True;	
	
	return new_c;
}

static HTTPCookieList *
newCookieList()
{
	HTTPCookieList *new_c = (HTTPCookieList *) calloc(1, sizeof(HTTPCookieList));
	
	return (new_c);
}

static HTTPCookieCache *
newCookieCache(void)
{	
	HTTPCookieCache *new_cc = (HTTPCookieCache *) calloc(1, sizeof(HTTPCookieCache));

	new_cc->cookies = (HTTPCookie **) NULL;
	new_cc->ncookies = 0;

	new_cc->filename = NULL;
	new_cc->fileType = 0;

	return (new_cc);
}

int
sortCookies(const void *s1, const void *s2)
{
	HTTPCookie *c1 = *(HTTPCookie **) s1;
	HTTPCookie *c2 = *(HTTPCookie **) s2;
	int c = 0;
	
	if (c1->domain && c2->domain ) 
		c = strcasecmp(c1->domain, c2->domain);
	
	if ( c == 0 && c2->path && c1->path ) 
/* reverse sense of sort */		
			return strcmp(c2->path, c1->path); 

	return c;
}

HTTPCookieCache *
loadCookieFileToCache(char *filename, char fileType)
{
	HTTPCookieCache *cc = newCookieCache();
	HTTPCookie *cookie=NULL;
	FILE *fp;

	cc->filename = NewString(filename);
	cc->fileType = fileType;

	if ((fp = fopen(filename, "r")) != NULL) {

		char line[4097];		/* the maximum length of a cookie as defined by the spec. */
		char domain[128];
		char allInDomain[128];
		char path[128];
		char secure[128];
		int  expires;
		char name[128];
		char value[128];
		char comment[128];
		char commentURL[128];
		int	version;
		char port[128];
		int  allHostsInDomain, sec;

		while (fgets(line, 4096, fp) != NULL) {

/* very early versions of netscape (seen on 1.12) mark the first line of the
 * cookie file with MCOM-HTTP-Cookie-file-1
 */
			if (strlen(line) == 0 || 
					line[0] == '#' || 
					line[0] == '\n' ||
					!strcmp(line, "MCOM-HTTP-Cookie-file-1"))
				continue;

			switch (fileType) {
			case NetscapeCookieFile:

/* allow white space in name and value items */
				sscanf(line,
					   "%s\t%s\t%s\t%s\t%d\t%[ -~]\t%[ -~]",
					   domain,
					   allInDomain,
					   path,
					   secure,
					   &expires,
					   name,
					   value);

				cookie = newCookie();
				cookie->domain = NewString(domain);
				cookie->exactHostMatch = ! stringToBoolean(allInDomain);
				cookie->path = NewString(path);
				cookie->secure = stringToBoolean(secure);
				cookie->expires = expires;
				cookie->cookie.name = NewString(name);
				cookie->cookie.value = NewString(value);

				break;

			case CookieJar:
/* allow white space in name and value items */
				sscanf(line,
					   "%s\t%d\t%d\t%s\t%s\t%d%d\t%[ -~]\t%[ -~]\t%[ -~]\t%[ -~]",
					   domain,
					   &version,
					   &allHostsInDomain,
					   path,
					   port,
					   &sec,
					   &expires,
					   comment,
					   commentURL,
					   name,
					   value);

				cookie = newCookie();
				cookie->domain = NewString(domain);
				cookie->exactHostMatch = allHostsInDomain;
				cookie->path = NewString(path);
				cookie->port = NewString(port);
				cookie->secure = sec;
				cookie->expires = expires;
				cookie->comment = NewString(comment);
				cookie->commentURL = NewString(commentURL);
				cookie->cookie.name = NewString(name);
				cookie->cookie.value = NewString(value);

				break;

			}

			if (cc->ncookies == 0) {
				cc->cookies = (HTTPCookie **) calloc(1, sizeof(HTTPCookie *));
			}
			else {
				cc->cookies = (HTTPCookie **) realloc((void *) cc->cookies,
							  (cc->ncookies + 1) * sizeof(HTTPCookie *));
			}

			cc->cookies[cc->ncookies] = cookie;
			cc->ncookies++;

		}

		fclose(fp);
	}
	/* sort the cookies on domain and reversed path, makes it easier 
	 * generating the cookieList.
	 * 
	 * According to the spec (example 2), more specific paths come before
	 * the less specific one.
	 */
	
	qsort((void *) cc->cookies, cc->ncookies, sizeof(HTTPCookie *), sortCookies); 
	
	return cc;

}


HTTPCookieRequest *
getCookieFromCache(HTTPCookieCache * cache, char *url)
{
	HTTPCookieRequest *req = newCookieRequest();
	HTTPCookieList *cLP = req->cookieList;
	int i;
	char *hostname, *filename;
	char *domain;
	char tmpHost[128];

	parseURL(url, PARSE_HOSTNAME | PARSE_FILENAME,
			 NULL, NULL, NULL, &hostname, NULL, &filename);

	for (i = 0; i < cache->ncookies; i++) {

		memset((void *) tmpHost, 0, 128);
		
		strcat(tmpHost, hostname);
			
/* if the cookie has expired, ignore it (it wont get written out when we save 
 * the cache )
 */
		if (cache->cookies[i]->expires < time(NULL))
			continue;



		if ((domain = strstr(tmpHost, cache->cookies[i]->domain)) != NULL) {

			if (cache->cookies[i]->exactHostMatch) {

				if (!strcasecmp(cache->cookies[i]->domain, tmpHost) &&
				 !strncmp(filename, cache->cookies[i]->path, strlen(filename))) {

/* ToDo check port numbers */

					if (cLP == NULL) {
						req->cookieList = cLP = newCookieList();
					}
					else if (cLP->next == NULL) {
						cLP->next = newCookieList();
						cLP = cLP->next;
					}

					cLP->cookie = cache->cookies[i];

				}
			}
			else {

				domain[0] = '\0';

/* hostnames with embedded dots are not allowed and the domain name must have 
 * at least one dot 
 */
				if ( strchr(tmpHost, '.') ||
					strchr(cache->cookies[i]->domain, '.') == NULL )
					continue;

				if (cLP == NULL) {
					req->cookieList = cLP = newCookieList();
				}
				else if (cLP->next == NULL) {
					cLP->next = newCookieList();
					cLP = cLP->next;
				}

				cLP->cookie = cache->cookies[i];
			}
		}
		
	}

	freeURL(PARSE_HOSTNAME | PARSE_FILENAME,
			 NULL, NULL, NULL, hostname, 0, filename);

	return req;
}

char *
makeCookie(HTTPCookieList *cookieList )
{

	char cookie[4097]; /* max length of a cookie */

/* tell the server that we support RFC2109 style cookies (if the server 
 * does as well, it will reply with Set-Cookie2 
 * 
 * Note: assume that if one cookie is version 0 they all will be (for this URL).
 * This IS an ASSUMPTION (the spec says that it is allowable to send two 
 * cookies of different versions, but is this likely ?
 */	
	if ( cookieList->cookie->version == 0 )
		sprintf(cookie,"Cookie2: $VERSION=\"1\"\r\nCookie: $VERSION=\"%d\"; ",cookieList->cookie->version);
	else
		sprintf(cookie,"Cookie: $VERSION=\"%d\"; ",cookieList->cookie->version);

	while ( cookieList ) {
		strcat(cookie, cookieList->cookie->cookie.name);	
		strcat(cookie, "=");
		strcat(cookie, cookieList->cookie->cookie.value);	
		strcat(cookie, ";");
		
		if ( cookieList->cookie->type == SetCookie2 ) {
			if ( cookieList->cookie->path ) {
				strcat(cookie, "$Path");
				strcat(cookie, "=");
				strcat(cookie, cookieList->cookie->path);	
				strcat(cookie, ";");
			}
	
			if ( cookieList->cookie->domain ) {
				strcat(cookie, "$Domain");
				strcat(cookie, "=");
				strcat(cookie, cookieList->cookie->domain);	
				strcat(cookie, ";");
			}
/* ToDo: Port support */			
		}
		
		cookieList = cookieList->next;
	}
	
	strcat(cookie, "\r\n");

	return NewString(cookie);
}

void addCookieListToCache(HTTPCookieCache * cache, HTTPCookieList * cookieList)
{
	
	
	while ( cookieList ) {
		
		int	i;
		int install = 1;

/* we look to see if the cookie is already in the cache, if so we replace it 
 * _unless_ it has a newer VERSION 
 */		
		for ( i=0; i< cache->ncookies; i++ ) {
			
			if ( ! strcasecmp(cache->cookies[i]->domain, 
					cookieList->cookie->domain) )
			   if ( ! strcmp(cache->cookies[i]->path, 
					   cookieList->cookie->path) )
				   if ( ! strcmp(cache->cookies[i]->cookie.name, 
						   cookieList->cookie->cookie.name)  &&
						   cache->cookies[i]->version >= cookieList->cookie->version ) {
					   freeCookie(cache->cookies[i]);
					   cache->cookies[i] = cookieList->cookie;
					   install = 0;
				   }
					   

		}	
		
		if ( install ) {
			cache->cookies = (HTTPCookie **) realloc((void *) cache->cookies,
						  (cache->ncookies + 1) * sizeof(HTTPCookie *));

			cache->cookies[cache->ncookies] = cookieList->cookie;
			cache->ncookies++;
		}

		cookieList = cookieList->next;	
	}

	qsort((void *) cache->cookies, cache->ncookies, sizeof(HTTPCookie *), sortCookies); 
	
} 



void 
writeCookieCache(HTTPCookieCache * cache)
{
	
	if ( cache->fileType != NetscapeCookieFile ) {
		FILE	*fp;
		int		i;
		time_t t = time(NULL);
		
		if ( (fp=fopen(cache->filename, "w") ) != NULL ) {
			
			fprintf(fp,"# CookieJar-1\n");
			fprintf(fp,"# This file is autogenerated, don't edit it\n");
			fprintf(fp,"# format designed by Richard Offer <offer@sgi.com> for HTTP cookies that \n");
			fprintf(fp,"# comply with both Netscape format and the 21-Nov-97 draft of HTTP State \n");
			fprintf(fp,"# Management Mechanism (was RFC2109)\n\n");
#ifdef DEBUG			
			fprintf(fp,"# format:\n");
			fprintf(fp,"#   domain (String)\n");
			fprintf(fp,"#   <TAB>\n");
			fprintf(fp,"#   version (int) (0==SetCookie, 1==SetCookie2)\n");
			fprintf(fp,"#   <TAB>\n");
			fprintf(fp,"#   exactHostMatch (int) (0=all machines in domain can access cookie\n");
			fprintf(fp,"#   <TAB>\n");
			fprintf(fp,"#   path (String)\n");
			fprintf(fp,"#   <TAB>\n");
			fprintf(fp,"#   port (String) comma-separated list of ports\n");
			fprintf(fp,"#   <TAB>\n");
			fprintf(fp,"#   secure (int)\n");
			fprintf(fp,"#   <TAB>\n");
			fprintf(fp,"#   expires (int)\n");
			fprintf(fp,"#   <TAB>\n");
			fprintf(fp,"#   comment (String)\n");
			fprintf(fp,"#   <TAB>\n");
			fprintf(fp,"#   commentURL (String)\n");
			fprintf(fp,"#   <TAB>\n");
			fprintf(fp,"#   name (String)\n");
			fprintf(fp,"#   <TAB>\n");
			fprintf(fp,"#   value (String)\n\n");
			fprintf(fp,"# Netscape style cookies do not include port comment or commentURL\n");
#endif /* DEBUG */
			
			for (i=0; i< cache->ncookies; i++ ) {
				
				if ( ! cache->cookies[i]->discard && 
						cache->cookies[i]->expires > t ) {
					
					fprintf(fp,"%s\t",cache->cookies[i]->domain);
					fprintf(fp,"%d\t",cache->cookies[i]->version);
					fprintf(fp,"%d\t",cache->cookies[i]->exactHostMatch);
					fprintf(fp,"%s\t",cache->cookies[i]->path);
					fprintf(fp,"%s\t",cache->cookies[i]->port ? cache->cookies[i]->port : "" );
					fprintf(fp,"%d\t",cache->cookies[i]->secure);
					fprintf(fp,"%d\t",cache->cookies[i]->expires);
					fprintf(fp,"%s\t",cache->cookies[i]->comment ? cache->cookies[i]->comment : "" );
					fprintf(fp,"%s\t",cache->cookies[i]->commentURL ? cache->cookies[i]->commentURL : "" );
					fprintf(fp,"%s\t",cache->cookies[i]->cookie.name);
					fprintf(fp,"%s\n",cache->cookies[i]->cookie.value);
				}
				
			}

			fclose(fp);
		}	
	}
}


void 
setCookie(HTTPCookieRequest *req, int type, char *string, char *host)
{
	
	HTTPCookieList *cLP = req->setCookie;
	char *name, *value;
	char	*str=string;
	int		i=0;
	
	if (cLP == NULL) {
		cLP = newCookieList();
	}
	else if (cLP->next == NULL) {
		cLP->next = newCookieList();
		cLP = cLP->next;
	}

	cLP->cookie = newCookie();

	/* removes leading and trailing white space */
#define TRIM(str) \
	{ \
		int n; \
		while ( *str && *str == ' ' ) \
			(*str)++; \
			n=strlen(str)-1; \
		while ( n && str[n] == ' ' ) { \
			str[n] = '\0';\
			n--; \
		} \
	}

	while ( str[i] ) {
		name = &str[i];
		while ( str[i] && str[i] != '=' ) {
			i++;
		}
		if ( str[i] ) {
			str[i] = '\0';
			i++;
			value = &str[i];
			while ( str[i] && str[i] != ';' ) {
				i++;
			}
			if ( str[i] )
				str[i] = '\0';
			i++;

			TRIM(name);
			TRIM(value);
		
		
		if ( strlen(value)) {
			
			if ( type == SetCookie2 ) {
				if ( strcasecmp(name,"comment") ) 
					cLP->cookie->comment = NewString(value);
				else if ( strcasecmp(name,"commenturl") )
					cLP->cookie->commentURL = NewString(value);
				else if ( strcasecmp(name,"discard") )
					cLP->cookie->discard = True;
				else if ( strcasecmp(name,"domain") ) {
					/* if first char isn't a dot add one - spec says this */
					if ( value[0] != '.' ) { 
						char d[128];
						d[0] = '.';
						strcat(d,value); 
						cLP->cookie->domain = NewString(d);
					}
					else
						cLP->cookie->domain = NewString(value);
				}
				else if ( strcasecmp(name,"max-age") ) {
					int j = atoi(value);
					if ( j != 0 )
						cLP->cookie->expires = time(NULL) + j;
					else
					/* TODO dump cookie immediately */
						;	
				}
				else if ( strcasecmp(name,"path") ) 
					cLP->cookie->path = NewString(value);	
				else if ( strcasecmp(name,"port" ) ) {
					/* TODO */
					;
				}
				else if ( strcasecmp(name,"version") )
					cLP->cookie->version = atoi(value);
				
			}
			else {
				
	/* Netscape format cookie */			
				if ( strcasecmp(name,"domain") ) {
					/* if first char isn't a dot add one - spec says this */
					if ( value[0] != '.' ) { 
						char d[128];
						d[0] = '.';
						strcat(d,value); 
						cLP->cookie->domain = NewString(d);
					}
					else
						cLP->cookie->domain = NewString(value);
				}
				else if ( strcasecmp(name,"path") ) 
					cLP->cookie->path = NewString(value);	
			}
				
		}
		else {
/* a value that has no associated value */

			if ( type == SetCookie2 ) {
				
				if ( strcasecmp(name,"discard" ) )
					cLP->cookie->discard = True;
				else if ( strcasecmp(name, "secure") )
					cLP->cookie->secure = True;
			}			
			else {
				
				if ( strcasecmp(name, "expires" ) ) {
/* typical of netscape they have a non-standard and facist-to-parse date 
 * format */
					char day[16],month[3];
					int date,mon=0,yr,hr,min,sec;
					struct tm timeStruct;
					time_t t;
					
					sscanf(name, "expires %s, %d-%s-%d %d:%d%d",
							day,
							&date, month, &yr,
							&hr, &min, &sec);
					
					if ( month[0] == 'J' ) {
						if ( month[1] == 'a' )
							mon = 0;
						else
							if ( month[2] == 'n' )
								mon = 5;
							else
								mon = 6;
					}
					else if ( month[0] == 'F' )
						mon = 1;
					else if ( month[0] == 'M' ) {
						if ( month[2] == 'r' )
							mon = 2;
						else
							mon = 4;
					}
					else if ( month[0] == 'A' )  {
						if ( month[1] == 'p' )
							mon = 3;
						else
							mon= 7;
					}
					else if ( month[0] == 'S' )
						mon = 8;
					else if ( month[0] == 'O' )
						mon = 9;
					else if ( month[0] == 'N' )
						mon = 10;
					else if ( month[0] == 'D' )
						mon = 11;
					
					timeStruct.tm_sec = sec;
					timeStruct.tm_min = min; 
					timeStruct.tm_hour = hr;
					timeStruct.tm_mday = date;
					timeStruct.tm_mon = mon;
					timeStruct.tm_isdst = -1;	/* the don't know flag */
					
					t = mktime( & timeStruct );
					
					if ( t != -1 )
						cLP->cookie->expires = t ;
				}

				if ( strcasecmp(name, "secure") )
					cLP->cookie->secure = True;
				
			}
		}
		
		}
	}
	
	if ( cLP->cookie->domain == NULL ) {
		cLP->cookie->domain = NewString(host);
		
	}
	
	if ( cLP->cookie->path == NULL ) {
		cLP->cookie->path = NewString("/");	
	}
}

void
mergeCookieCache(HTTPCookieCache *c1, HTTPCookieCache *c2)
{

	if ( c1 && c2 ) {
		c1->cookies = (HTTPCookie **) realloc( (void *) c1->cookies,
				(c1->ncookies + c2->ncookies) * sizeof(HTTPCookie) );
	
		memcpy( (void *) & (c1->cookies[c1->ncookies]),
				(void *) c2->cookies,
				c2->ncookies * sizeof(HTTPCookie *) );
		
		c1->ncookies += c2->ncookies; 	
	}

}	
	
