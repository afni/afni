/**************************************************************************
  mcw_glob.c -- slightly adapted from glob.c in tcsh-6.05
                (made to compile without support files besides mcw_glob.h)
             -- added routines MCW_*_expand at end
***************************************************************************/

/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Guido van Rossum.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * Glob: the interface is a superset of the one defined in POSIX 1003.2,
 * draft 9.
 *
 * The [!...] convention to negate a range is supported (SysV, Posix, ksh).
 *
 * Optional extra services, controlled by flags not defined by POSIX:
 *
 * GLOB_QUOTE:
 *	Escaping convention: \ inhibits any special meaning the following
 *	character might have (except \ at end of string is retained).
 * GLOB_MAGCHAR:
 *	Set in gl_flags if pattern contained a globbing character.
 * GLOB_ALTNOT:
 *	Use ^ instead of ! for "not".
 * gl_matchc:
 *	Number of matches in the current invocation of glob.
 */


/** the following were in "sh.h",
    but I put them here to get rid of the need for that file -- RWCox **/

#define xfree     free
#define xmalloc   malloc
#define xrealloc  realloc

#ifdef SPARKY
#undef _POSIX_SOURCE
#endif

#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>
#include <ctype.h>
typedef void * ptr_t;

/** don't use sh.h any more **/

#if 0
#  define Char __Char
#  include "sh.h"
#  undef Char
#  undef QUOTE
#  undef TILDE
#  undef META
#  undef CHAR
#  undef ismeta
#  undef Strchr
#endif

#include "l_mcw_glob.h"

/* added for direction control on sorting                14 Feb 2005 [rickr] */
static int g_sort_dir = 1 ;       /* 1 = small to large, -1 = large to small */

#ifndef S_ISDIR
#define S_ISDIR(a)	(((a) & S_IFMT) == S_IFDIR)
#endif

#if !defined(S_ISLNK) && defined(S_IFLNK)
#define S_ISLNK(a)	(((a) & S_IFMT) == S_IFLNK)
#endif

#if !defined(S_ISLNK) && !defined(lstat)
#define lstat stat
#endif

typedef unsigned short Char;

#undef  __P
#define __P(a) a

static	int	 glob1 		__P((Char *, glob_t *, int));
static	int	 glob2		__P((Char *, Char *, Char *, glob_t *, int));
static	int	 glob3		__P((Char *, Char *, Char *, Char *,
				     glob_t *, int));
static	int	 globextend	__P((Char *, glob_t *));
static	int	 match		__P((Char *, Char *, Char *, int));
#ifndef __clipper__
static	int	 compare	__P((const ptr_t, const ptr_t));
#endif
static 	DIR	*Opendir	__P((Char *));
#ifdef S_IFLNK
static	int	 Lstat		__P((Char *, struct stat *));
#endif
static 	Char 	*Strchr		__P((Char *, int));
#ifdef DEBUG
static	void	 qprintf	__P((Char *));
#endif

#define	DOLLAR		'$'
#define	DOT		'.'
#define	EOS		'\0'
#define	LBRACKET	'['
#define	NOT		'!'
#define ALTNOT		'^'
#define	QUESTION	'?'
#define	QUOTE		'\\'
#define	RANGE		'-'
#define	RBRACKET	']'
#define	SEP		'/'
#define	STAR		'*'
#define	TILDE		'~'
#define	UNDERSCORE	'_'

#define	M_META		0x8000
#define M_PROTECT	0x4000
#define	M_MASK		0xffff
#define	M_ASCII		0x00ff

#define	CHAR(c)		((c)&M_ASCII)
#define	META(c)		((c)|M_META)
#define	M_ALL		META('*')
#define	M_END		META(']')
#define	M_NOT		META('!')
#define	M_ALTNOT	META('^')
#define	M_ONE		META('?')
#define	M_RNG		META('-')
#define	M_SET		META('[')
#define	ismeta(c)	(((c)&M_META) != 0)

#if defined(SOLARIS_DIRENT_ZERO) && !defined(SOLARIS_DIRENT_PATCH)
#  define SOLARIS_DIRENT_PATCH
#endif

#ifdef SOLARIS_DIRENT_PATCH
struct  dirent {
     ino_t            d_ino;
     off_t            d_off;
     unsigned short        d_reclen;
     char             d_name[1];
};
#endif

/*
 * Need to dodge two kernel bugs:
 * opendir("") != opendir(".")
 * NAMEI_BUG: on plain files trailing slashes are ignored in some kernels.
 *            POSIX specifies that they should be ignored in directories.
 */

static DIR *
Opendir(Char *str)
{
    char    buf[MAXPATHLEN];
    register char *dc = buf;

    if (!*str)
	return (opendir("."));
    while ((*dc++ = *str++) != '\0')
	continue;
    return (opendir(buf));
}

#ifdef S_IFLNK
static int
Lstat(Char *fn, struct stat *sb)
{
    char    buf[MAXPATHLEN];
    register char *dc = buf;

    while ((*dc++ = *fn++) != '\0')
	continue;
# ifdef NAMEI_BUG
    {
	int     st;

	st = lstat(buf, sb);
	if (*buf)
	    dc--;
	return (*--dc == '/' && !S_ISDIR(sb->st_mode) ? -1 : st);
    }
# else
    return (lstat(buf, sb));
# endif	/* NAMEI_BUG */
}
#else
#define Lstat Stat
#endif /* S_IFLNK */

static int
Stat(Char *fn, struct stat *sb)
{
    char    buf[MAXPATHLEN];
    register char *dc = buf;

    while ((*dc++ = *fn++) != '\0')
	continue;
#ifdef NAMEI_BUG
    {
	int     st;

	st = stat(buf, sb);
	if (*buf)
	    dc--;
	return (*--dc == '/' && !S_ISDIR(sb->st_mode) ? -1 : st);
    }
#else
    return (stat(buf, sb));
#endif /* NAMEI_BUG */
}

static Char *
Strchr(Char *str, int ch)
{
    do
	if (*str == ch)
	    return (str);
    while (*str++);
    return (NULL);
}

#ifdef DEBUG
static void
qprintf(Char *s)
{
    Char *p;

    for (p = s; *p; p++)
	printf("%c", *p & 0xff);
    printf("\n");
    for (p = s; *p; p++)
	printf("%c", *p & M_PROTECT ? '"' : ' ');
    printf("\n");
    for (p = s; *p; p++)
	printf("%c", *p & M_META ? '_' : ' ');
    printf("\n");
}
#endif /* DEBUG */

static int
compare(const ptr_t p, const ptr_t q)
{
#if defined(NLS) && !defined(NOSTRCOLL)

#if 0
    errno = 0;  /* strcoll sets errno, another brain-damage */
#endif

    return (g_sort_dir * strcoll(*(char **) p, *(char **) q));
#else
    return (g_sort_dir * strcmp(*(char **) p, *(char **) q));
#endif /* NLS && !NOSTRCOLL */
}

/*
 * The main glob() routine: compiles the pattern (optionally processing
 * quotes), calls glob1() to do the real pattern matching, and finally
 * sorts the list (unless unsorted operation is requested).  Returns 0
 * if things went well, nonzero if errors occurred.  It is not an error
 * to find no matches.
 */
int
glob(const char *pattern, int flags, int(*errfunc)(char *,int), glob_t *pglob)
{
    int     err, oldpathc;
    Char *bufnext, *bufend, *compilebuf, m_not;
    const unsigned char *compilepat, *patnext;
    int     c, nnot;
    Char patbuf[MAXPATHLEN + 1], *qpatnext;
    int     no_match;

    patnext = (unsigned char *) pattern;
    if (!(flags & GLOB_APPEND)) {
	pglob->gl_pathc = 0;
	pglob->gl_pathv = NULL;
	if (!(flags & GLOB_DOOFFS))
	    pglob->gl_offs = 0;
    }
    pglob->gl_flags = flags & ~GLOB_MAGCHAR;
    pglob->gl_errfunc = errfunc;
    oldpathc = pglob->gl_pathc;
    pglob->gl_matchc = 0;

    if (pglob->gl_flags & GLOB_ALTNOT) {
	nnot = ALTNOT;
	m_not = M_ALTNOT;
    }
    else {
	nnot = NOT;
	m_not = M_NOT;
    }

    bufnext = patbuf;
    bufend = bufnext + MAXPATHLEN;
    compilebuf = bufnext;
    compilepat = patnext;

    no_match = *patnext == nnot;
    if (no_match)
	patnext++;

    if (flags & GLOB_QUOTE) {
	/* Protect the quoted characters */
	while (bufnext < bufend && (c = *patnext++) != EOS)
	    if (c == QUOTE) {
		if ((c = *patnext++) == EOS) {
		    c = QUOTE;
		    --patnext;
		}
		*bufnext++ = (Char) (c | M_PROTECT);
	    }
	    else
		*bufnext++ = (Char) c;
    }
    else
	while (bufnext < bufend && (c = *patnext++) != EOS)
	    *bufnext++ = (Char) c;
    *bufnext = EOS;

    bufnext = patbuf;
    qpatnext = patbuf;
    /* we don't need to check for buffer overflow any more */
    while ((c = *qpatnext++) != EOS) {
	switch (c) {
	case LBRACKET:
	    c = *qpatnext;
	    if (c == nnot)
		++qpatnext;
	    if (*qpatnext == EOS ||
		Strchr(qpatnext + 1, RBRACKET) == NULL) {
		*bufnext++ = LBRACKET;
		if (c == nnot)
		    --qpatnext;
		break;
	    }
	    pglob->gl_flags |= GLOB_MAGCHAR;
	    *bufnext++ = M_SET;
	    if (c == nnot)
		*bufnext++ = m_not;
	    c = *qpatnext++;
	    do {
		*bufnext++ = CHAR(c);
		if (*qpatnext == RANGE &&
		    (c = qpatnext[1]) != RBRACKET) {
		    *bufnext++ = M_RNG;
		    *bufnext++ = CHAR(c);
		    qpatnext += 2;
		}
	    } while ((c = *qpatnext++) != RBRACKET);
	    *bufnext++ = M_END;
	    break;
	case QUESTION:
	    pglob->gl_flags |= GLOB_MAGCHAR;
	    *bufnext++ = M_ONE;
	    break;
	case STAR:
	    pglob->gl_flags |= GLOB_MAGCHAR;
	    /* collapse adjacent stars to one, to avoid
	     * exponential behavior
	     */
	    if (bufnext == patbuf || bufnext[-1] != M_ALL)
		*bufnext++ = M_ALL;
	    break;
	default:
	    *bufnext++ = CHAR(c);
	    break;
	}
    }
    *bufnext = EOS;
#ifdef DEBUG
    qprintf(patbuf);
#endif

    if ((err = glob1(patbuf, pglob, no_match)) != 0)
	return (err);

    /*
     * If there was no match we are going to append the pattern
     * if GLOB_NOCHECK was specified or if GLOB_NOMAGIC was specified
     * and the pattern did not contain any magic characters
     * GLOB_NOMAGIC is there just for compatibility with csh.
     */
    if (pglob->gl_pathc == oldpathc &&
	((flags & GLOB_NOCHECK) ||
	 ((flags & GLOB_NOMAGIC) && !(pglob->gl_flags & GLOB_MAGCHAR)))) {
	if (!(flags & GLOB_QUOTE)) {
	    Char *dp = compilebuf;
	    const unsigned char *sp = compilepat;

	    while ((*dp++ = *sp++) != '\0')
		continue;
	}
	else {
	    /*
	     * copy pattern, interpreting quotes; this is slightly different
	     * than the interpretation of quotes above -- which should prevail?
	     */
	    while (*compilepat != EOS) {
		if (*compilepat == QUOTE) {
		    if (*++compilepat == EOS)
			--compilepat;
		}
		*compilebuf++ = (unsigned char) *compilepat++;
	    }
	    *compilebuf = EOS;
	}
	return (globextend(patbuf, pglob));
    }
    else if (!(flags & GLOB_NOSORT))
	qsort((char *) (pglob->gl_pathv + pglob->gl_offs + oldpathc),
	      pglob->gl_pathc - oldpathc, sizeof(char *),
	      (int (*) __P((const void *, const void *))) compare);
    return (0);
}

static int
glob1(Char *pattern, glob_t *pglob, int no_match)
{
    Char pathbuf[MAXPATHLEN + 1];

    /*
     * a null pathname is invalid -- POSIX 1003.1 sect. 2.4.
     */
    if (*pattern == EOS)
	return (0);
    return (glob2(pathbuf, pathbuf, pattern, pglob, no_match));
}

/*
 * functions glob2 and glob3 are mutually recursive; there is one level
 * of recursion for each segment in the pattern that contains one or
 * more meta characters.
 */
static int
glob2( Char *pathbuf,Char *pathend, Char *pattern, glob_t *pglob, int no_match)
{
    struct stat sbuf;
    int anymeta;
    Char *p, *q;

    /*
     * loop over pattern segments until end of pattern or until segment with
     * meta character found.
     */
    anymeta = 0;
    for (;;) {
	if (*pattern == EOS) {	/* end of pattern? */
	    *pathend = EOS;

	    if (Lstat(pathbuf, &sbuf))
		return (0);

	    if (((pglob->gl_flags & GLOB_MARK) &&
		 pathend[-1] != SEP) &&
		(S_ISDIR(sbuf.st_mode)
#ifdef S_IFLNK
		 || (S_ISLNK(sbuf.st_mode) &&
		     (Stat(pathbuf, &sbuf) == 0) &&
		     S_ISDIR(sbuf.st_mode))
#endif
		 )) {
		*pathend++ = SEP;
		*pathend = EOS;
	    }
	    ++pglob->gl_matchc;
	    return (globextend(pathbuf, pglob));
	}

	/* find end of next segment, copy tentatively to pathend */
	q = pathend;
	p = pattern;
	while (*p != EOS && *p != SEP) {
	    if (ismeta(*p))
		anymeta = 1;
	    *q++ = *p++;
	}

	if (!anymeta) {		/* no expansion, do next segment */
	    pathend = q;
	    pattern = p;
	    while (*pattern == SEP)
		*pathend++ = *pattern++;
	}
	else			/* need expansion, recurse */
	    return (glob3(pathbuf, pathend, pattern, p, pglob, no_match));
    }
    /* NOTREACHED */
}


static int
glob3(Char *pathbuf, Char *pathend, Char *pattern, Char *restpattern, glob_t *pglob, int no_match)
{
#if 0
    extern int errno;
#endif
    DIR    *dirp;
    struct dirent *dp;
    int     err;
    Char m_not = (pglob->gl_flags & GLOB_ALTNOT) ? M_ALTNOT : M_NOT;
    char cpathbuf[MAXPATHLEN], *ptr;
#ifdef SOLARIS_DIRENT_PATCH
    /* declaration of vars used in the solaris-patch */
    char dname[255];
    int ii;
#endif

    *pathend = EOS;

#if 0
    errno = 0;
#endif

    if (!(dirp = Opendir(pathbuf))) {
	/* todo: don't call for ENOENT or ENOTDIR? */
	for (ptr = cpathbuf; (*ptr++ = (char) *pathbuf++) != EOS;)
	    continue;
#if 0
	if ((pglob->gl_errfunc && (*pglob->gl_errfunc) (cpathbuf, errno)) ||
	    (pglob->gl_flags & GLOB_ERR))
#else
	if ( (pglob->gl_flags & GLOB_ERR))
#endif
	    return (GLOB_ABEND);
	else
	    return (0);
    }

    err = 0;

    /* search directory for matching names */
    while ((dp = readdir(dirp)) != NULL) {
	register unsigned char *sc;
	register Char *dc;

#ifdef SOLARIS_DIRENT_PATCH
	/**********
	begin patch
	**********/

#ifndef SOLARIS_DIRENT_ZERO
	for (ii = -2 ; dp->d_name[ii] != '\0' ; ++ii) {
	  dname[ii+2] = dp->d_name[ii];
	}
        dname[ii+2] = '\0';
#else
        strcpy(dname, dp->d_name); /* John Koger, March 1999 */
#endif
	/**********
	end patch
	now use dname for dp->d_name
	**********/

	/* initial DOT must be matched literally */
	if (dname[0] == DOT && *pattern != DOT)
	    continue;
	for (sc = (unsigned char *) dname, dc = pathend;
#else
	if (dp->d_name[0] == DOT && *pattern != DOT)
	    continue;
	for (sc = (unsigned char *) dp->d_name, dc = pathend;
#endif
	     (*dc++ = *sc++) != '\0';)
	    continue;
	if (match(pathend, pattern, restpattern, (int) m_not) == no_match) {
	    *pathend = EOS;
	    continue;
	}
	err = glob2(pathbuf, --dc, restpattern, pglob, no_match);
	if (err)
	    break;
    }
    /* todo: check error from readdir? */
    (void) closedir(dirp);
    return (err);
}


/*
 * Extend the gl_pathv member of a glob_t structure to accommodate a new item,
 * add the new item, and update gl_pathc.
 *
 * This assumes the BSD realloc, which only copies the block when its size
 * crosses a power-of-two boundary; for v7 realloc, this would cause quadratic
 * behavior.
 *
 * Return 0 if new item added, error code if memory couldn't be allocated.
 *
 * Invariant of the glob_t structure:
 *	Either gl_pathc is zero and gl_pathv is NULL; or gl_pathc > 0 and
 *	 gl_pathv points to (gl_offs + gl_pathc + 1) items.
 */
static int
globextend(Char *path, glob_t *pglob)
{
    register char **pathv;
    register int i;
    unsigned int newsize;
    char   *copy;
    Char *p;

    newsize = sizeof(*pathv) * (2 + pglob->gl_pathc + pglob->gl_offs);
    pathv = (char **) (pglob->gl_pathv ?
		       xrealloc((ptr_t) pglob->gl_pathv, (size_t) newsize) :
		       xmalloc((size_t) newsize));
    if (pathv == NULL)
	return (GLOB_NOSPACE);

    if (pglob->gl_pathv == NULL && pglob->gl_offs > 0) {
	/* first time around -- clear initial gl_offs items */
	pathv += pglob->gl_offs;
	for (i = pglob->gl_offs; --i >= 0;)
	    *--pathv = NULL;
    }
    pglob->gl_pathv = pathv;

    for (p = path; *p++;)
	continue;
    if ((copy = (char *) xmalloc((size_t) (p - path))) != NULL) {
	register char *dc = copy;
	register Char *sc = path;

	while ((*dc++ = *sc++) != '\0')
	    continue;
	pathv[pglob->gl_offs + pglob->gl_pathc++] = copy;
    }
    pathv[pglob->gl_offs + pglob->gl_pathc] = NULL;
    return ((copy == NULL) ? GLOB_NOSPACE : 0);
}


/*
 * pattern matching function for filenames.  Each occurrence of the *
 * pattern causes a recursion level.
 */
static  int
match(Char *name, Char *pat, Char *patend, int m_not)
{
    int ok, negate_range;
    Char c, k;

    while (pat < patend) {
	c = *pat++;
	switch (c & M_MASK) {
	case M_ALL:
	    if (pat == patend)
		return (1);
	    do
		if (match(name, pat, patend, m_not))
		    return (1);
	    while (*name++ != EOS);
	    return (0);
	case M_ONE:
	    if (*name++ == EOS)
		return (0);
	    break;
	case M_SET:
	    ok = 0;
	    if ((k = *name++) == EOS)
		return (0);
	    if ((negate_range = ((*pat & M_MASK) == m_not)) != 0)
		++pat;
	    while (((c = *pat++) & M_MASK) != M_END) {
		if ((*pat & M_MASK) == M_RNG) {
		    if (c <= k && k <= pat[1])
			ok = 1;
		    pat += 2;
		}
		else if (c == k)
		    ok = 1;
	    }
	    if (ok == negate_range)
		return (0);
	    break;
	default:
	    k = *name++;
	    if (k != c)
		return (0);
	    break;
	}
    }
    return (*name == EOS);
}

/* free allocated data belonging to a glob_t structure */
void
globfree(glob_t *pglob)
{
    register int i;
    register char **pp;

    if (pglob->gl_pathv != NULL) {
	pp = pglob->gl_pathv + pglob->gl_offs;
	for (i = pglob->gl_pathc; i--; ++pp)
	    if (*pp)
		xfree((ptr_t) *pp), *pp = NULL;
	xfree((ptr_t) pglob->gl_pathv), pglob->gl_pathv = NULL;
    }
}

/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*! set the direction of the sort (either small to large, or the reverse)    */
int rglob_set_sort_dir( int dir )                     /* 14 Feb 2005 [rickr] */
{
   if ( dir == 1 )       g_sort_dir =  1;
   else if ( dir == -1 ) g_sort_dir = -1;
   else                  return 1;          /* else, ignore and signal error */
                                                                                
   return 0;
}


static int warn = 0 ;
void MCW_warn_expand( int www ){ warn = www; return; }  /* 13 Jul 2001 */

/*------------------------------------------------------------------------*/
/*! Routines that allows filename wildcarding to be handled inside
    to3d.  The advantage: limitations of shell command line lengths.
     - 29 July 1996:  Incorporated "glob" functions from tcsh-6.05, rather
                       than rely on system supplying a library.
     - 30 July 1996:  Extended routine to allow for 3D: type prefixes.
     - 10 Feb  2000:  and for 3A: prefixes.
--------------------------------------------------------------------------*/

void MCW_file_expand( int nin , char ** fin , int * nout , char *** fout )
{
   glob_t gl ;
   int    ii , gnum, gold , ilen ;
   char ** gout ;
   char *  fn ;
   char prefix[4] , fpre[128] , fname[256] ;
   int  b1,b2,b3,b4,b5 , ib,ig , lpre=0 ;

   if( nin <= 0 ){ *nout = 0 ; return ; }

   gnum = 0 ;
   gout = NULL ;

   for( ii=0 ; ii < nin ; ii++ ){
      fn = fin[ii] ;

      ig = 0 ;

      /** look for 3D: prefix **/

      if( strlen(fn) > 9 && fn[0] == '3' && fn[1] == 'D' ){
         ib = 0 ;
         prefix[ib++] = '3' ;
         prefix[ib++] = 'D' ;
         if( fn[2] == ':' ){ prefix[ib++] = '\0' ; }
         else              { prefix[ib++] = fn[2] ; prefix[ib++] = '\0' ; }

         ig = sscanf( fn+ib , "%d:%d:%d:%d:%d:%s" ,     /* must scan all */
                      &b1,&b2,&b3,&b4,&b5 , fname ) ;   /* six items OK  */

         /** if have all 6 3D: items, then make a 3D: prefix for output **/

         if( ig == 6 ){
            sprintf(fpre , "%s:%d:%d:%d:%d:%d:" , prefix,b1,b2,b3,b4,b5) ;
            lpre = strlen(fpre) ;
         } else {
            ig = 0 ;
         }
      }

      if( strlen(fn) > 9 && fn[0] == '3' && fn[1] == 'A' && fn[3] == ':' ){
         ib = 0 ;
         prefix[ib++] = '3' ;
         prefix[ib++] = 'A' ;
         prefix[ib++] = fn[2] ;
         prefix[ib++] = '\0' ;

         ig = sscanf( fn+ib , "%d:%d:%d:%s" ,  /* must scan all */
                      &b1,&b2,&b3, fname ) ;   /* four items OK */

         /** if have all 4 3A: items, then make a 3A: prefix for output **/

         if( ig == 4 ){
            sprintf(fpre , "%s:%d:%d:%d:" , prefix,b1,b2,b3) ;
            lpre = strlen(fpre) ;
         } else {
            ig = 0 ;
         }
      }

      if( ig > 0 ) (void) glob( fname , 0 , NULL ,  &gl ) ;  /* 3D: was OK */
      else         (void) glob( fn    , 0 , NULL ,  &gl ) ;  /*     not OK */

      /** put each matched string into the output array **/

      if( gl.gl_pathc > 0 ){

         /** make space for output now **/
         gold  = gnum ;
         gnum += gl.gl_pathc ;
         if( gout == NULL ) gout = (char **) malloc (      sizeof(char *)*gnum);
         else               gout = (char **) realloc(gout, sizeof(char *)*gnum);

         for( ib=0 ; ib < gl.gl_pathc ; ib++ ){
            ilen = strlen( gl.gl_pathv[ib] ) + 1 ;  /* length of this name */
            if( ig > 0 ) ilen += lpre ;             /* plus 3D: prefix?    */

            gout[ib+gold] = (char *) malloc( sizeof(char) * ilen ) ; /* output! */

            if( ig > 0 ){
               strcpy( gout[ib+gold] , fpre ) ;             /* 3D: prefix */
               strcat( gout[ib+gold] , gl.gl_pathv[ib] ) ;  /* then name  */
            }
            else {
               strcpy( gout[ib+gold] , gl.gl_pathv[ib] ) ;  /* just name */
            }
         }

      } else if( ig == 6 && strcmp(fname,"ALLZERO") == 0 ){ /* 06 Mar 2001 */

         gold = gnum ; gnum++ ;
         if( gout == NULL ) gout = (char **) malloc (      sizeof(char *)*gnum);
         else               gout = (char **) realloc(gout, sizeof(char *)*gnum);

         ilen = lpre + strlen(fname) + 1 ;
         gout[gold] = (char *) malloc( sizeof(char) * ilen ) ; /* output! */
         strcpy( gout[gold] , fpre ) ;
         strcat( gout[gold] , fname ) ;

      } else {  /* 30 Apr 2001 */

         if( warn )  /* 13 Jul 2001 - print only if told to do so */
           fprintf(stderr,"** Can't find file %s\n", (ig>0) ? fname : fn ) ;
      }

      globfree( &gl ) ;
   }

   *nout = gnum ; *fout = gout ; return ;
}

/*-----------------------------------------------------------------------*/
/*! Simpler interface to MCW_file_expand().
      - fnam = string of form "*.zork fred*.* ?a?b"; e.g., 1 or more
               wildcards
      - nout = pointer to output count
      - fout = pointer to output list of strings.

    Sample usage:
      int nfile ; char **flist ;
      MCW_wildcards( "*.jpg *.JPG" , &nfile , &flist ) ;
       ... do something with flist[0]..flist[nfile-1] if nfile > 0 ...
      MCW_free_wildcards( nfile , flist ) ;
-------------------------------------------------------------------------*/

void MCW_wildcards( char *fnam , int *nout , char ***fout )  /* 01 Dec 2003 */
{
   char **fin=NULL, *fcop ;
   int ii , nin , lf , ls ;

   if( fnam == NULL || *fnam == '\0' ){ *nout = 0 ; return ; }
   fcop = strdup(fnam) ; lf = strlen(fcop) ;
   ls = 1 ;
   for( nin=ii=0 ; ii < lf ; ii++ ){
     if( isspace(fcop[ii]) ){   /* This is a blank, so next */
       ls = 1 ;                 /*  non-blank is a new word. */
       fcop[ii] = '\0' ;        /* Set this char to NUL.      */

     } else {                   /* Not a blank. */

       if( ls ){                /* If last was a blank, is new name. */
         fin = (char **) realloc( fin , sizeof(char *)*(nin+1) ) ;
         fin[nin++] = fcop+ii ;
       }
       ls = 0 ;
     }
   }

   if( nin == 0 ){ *nout = 0 ; free(fcop) ; return ; }

   MCW_file_expand( nin , fin , nout , fout ) ;
   free(fin) ; free(fcop) ; return ;
}

/*-----------------------------------------------------------------------*/

void MCW_free_expand( int gnum , char ** gout )
{
   int ii ;

   if( gout == NULL ) return ;

   for( ii=0 ; ii < gnum ; ii++ ) free( gout[ii] ) ;
   free( gout ) ;
   return ;
}
