/**********************************************************************
  Program to read GE RT-EPI image files and divine their ordering
  in time and space.  To compile on Linux or other sane systems:

    cc -o Ifile -O2 Ifile.c -lm

  If on a Solaris machine, try

    cc -o Ifile -O2 Ifile.c -lm -DSOLARIS_DIRENT_ZERO
***********************************************************************/

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <math.h>

/*------- prototypes [stuff included here from mrilib] -------*/

time_t THD_file_mtime( char * pathname ) ;
int THD_is_file( char * pathname ) ;
int THD_is_symlink( char * pathname ) ;
long THD_filesize( char * pathname ) ;
int THD_is_directory( char * pathname ) ;
int THD_is_executable( char * pathname ) ;

void MCW_file_expand( int nin , char ** fin , int * nout , char *** fout ) ;
void MCW_free_expand( int gnum , char ** gout ) ;
void MCW_warn_expand( int www ) ;
void NIH_glob( char *fn , int *nout , char ***fout ) ;
void NIH_glob_free( int gnum , char **gout ) ;

/*------- typedefs -------*/

typedef struct {                    /* stuff extracted from GE I.* image */
  int good  ;                       /* is this a good image? */
  int nx,ny ;                       /* image matrix */
  float dx,dy,dz , zoff , tr,te ;   /* various dimensions */
  char orients[8] ;                 /* orientation string */
} ge_header_info ;

void ge_header( char *pathname , ge_header_info *hi ) ;

/***************************************************************************/

int main( int argc , char *argv[] )
{
   int num_I , ii,jj , ngood , nrun ;
   char **nam_I  ;
   char **gnam_I ;
   int   *time_I , lmax=0 , ll , thresh , ibot,itop ;
   float *zoff_I , tr , zth1,zth2 , zd ;
   ge_header_info geh ;
   char fmt[128] ;

   /*-- get the list of files */

   NIH_glob( "*/I.*" , &num_I , &nam_I ) ;

   fprintf(stderr,"++ found %d '*/I.*' files\n",num_I) ;

   if( num_I <= 0 ) exit(1) ;

   /*-- get time and z-offset of each good image --*/

   time_I = (int *)   calloc( sizeof(int)    , num_I ) ;
   zoff_I = (float *) calloc( sizeof(float)  , num_I ) ;
   gnam_I = (char **) calloc( sizeof(char *) , num_I ) ;
   ngood  = 0 ;

   fprintf(stderr,"++ Scanning GE headers") ;

   for( ii=0 ; ii < num_I ; ii++ ){
      ge_header( nam_I[ii] , &geh ) ;    /* read GE header */

      if( ii%1000==999 ) fprintf(stderr,".") ;

      if( geh.good ){                    /* is good image file */
         zoff_I[ngood] = geh.zoff ;
         time_I[ngood] = (int) THD_file_mtime( nam_I[ii] ) ;
         gnam_I[ngood] = strdup( nam_I[ii] ) ;
         ngood++ ;

         ll = strlen(nam_I[ii]) ; if( ll > lmax ) lmax = ll ;

         tr += geh.tr ;
      }
   }

   fprintf(stderr,"\n++ %d files are good images\n",ngood) ;

   NIH_glob_free( num_I , nam_I ) ;  /* don't need nam_I any more */

   if( ngood < 3 ) exit(1) ;

   /*-- convert to time difference from previous image --*/

   for( ii=ngood-1 ; ii > 0 ; ii-- )
      time_I[ii] -= time_I[ii-1] ;
   time_I[0] = 0 ;

   /*-- set threshold for time, to find distinct imaging runs --*/

   tr    /= ngood ;                 /* average TR reported */
   thresh = (int)( 3.0*tr+10.5 ) ;  /* threshold (may need some work) */

   fprintf(stderr,"++ File time threshold = %d s\n",thresh) ;

   /*-- find time steps longer than thresh:
        these are starts of new imaging runs --*/

   nrun = 0 ;
   ibot = 0 ;
   while( ibot < ngood ){  /* scan forward from ibot */

      /* scan itop until end, or until time step is too big */

      for( itop=ibot+1; itop<ngood && time_I[itop]<thresh; itop++ ) ; /* nada */

      /* this run is from ibot to itop-1 */

      if( ibot == itop-1 ){                    /* skip single files */

         printf("skip:   %s\n",gnam_I[ibot]) ;

      } else {                                 /* more than 1 file */

         nrun++ ;

         printf("run %02d:  %s .. %s\n",nrun,gnam_I[ibot],gnam_I[itop-1]) ;

         /* check for skipped slices [assume 1st 2 slices are OK] */

         /* [[ this algorithm could use some thought!  ]] */
         /* [[ maybe compute median delta-zoff?        ]] */
         /* [[ what about 1 or 2 slice imaging runs?   ]] */
         /* [[ what about slice missing at top or bot? ]] */

         zth1 = fabs( zoff_I[ibot+1] - zoff_I[ibot] ) * 1.01 + 0.01 ;
         zth2 = 3.01*zth1 ;

         for( jj=ibot ; jj < itop-1 ; jj++ ){
            zd = fabs( zoff_I[jj+1] - zoff_I[jj] ) ;
            if( zd > zth1 && zd < zth2 )
               printf("  image %s seems out of place\n",gnam_I[jj+1]) ;
         }
      }

      ibot = itop ;  /* start scan here */
   }

#if 0
   /* old code to print out stuff to check if program was working */
   sprintf(fmt,"%%-%d.%ds: dt=%%d  zoff=%%g\n",lmax,lmax) ;
   for( ii=0 ; ii < ngood ; ii++ )
      printf(fmt,gnam_I[ii],time_I[ii],zoff_I[ii]) ;
#endif

   exit(0) ;
}

/***************************************************************************/
/*** from thd_filestuff.c ***/

time_t THD_file_mtime( char * pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL ) return 0 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return 0 ;
   return buf.st_mtime ;
}

/*-------------------------------------------*/

int THD_is_file( char * pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL ) return 0 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return 0 ;
   ii = (buf.st_mode & S_IFREG) != 0 ; return ii ;
}

/*-------------------------------------------*/

int THD_is_symlink( char * pathname )
{
   char buf[32] ; int ii ;

   ii = readlink( pathname , buf , 32 ) ;
   return (ii > 0) ;
}

/*-------------------------------------------*/

long THD_filesize( char * pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL ) return -1 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return -1 ;
   return buf.st_size ;
}

/*-------------------------------------------*/

int THD_is_directory( char * pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL ) return 0 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return 0 ;
   ii = (buf.st_mode & S_IFDIR) != 0 ; return ii ;
}

/*-------------------------------------------*/

int THD_is_executable( char * pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL ) return 0 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return 0 ;
   ii = (buf.st_mode & S_IXOTH) != 0 ; return ii ;
}

/***************************************************************************/
/*** from mcw_glob.[ch] ***/
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
 *
 *	@(#)glob.h	5.6 (Berkeley) 4/3/91
 */

typedef struct {
	int gl_pathc;		/* count of total paths so far */
	int gl_matchc;		/* count of paths matching pattern */
	int gl_offs;		/* reserved at beginning of gl_pathv */
	int gl_flags;		/* copy of flags parameter to glob() */
	int (*gl_errfunc)();	/* copy of errfunc parameter to glob() */
	char **gl_pathv;	/* list of paths matching pattern */
} glob_t;

#define	GLOB_APPEND	0x001	/* append to output from previous call */
#define	GLOB_DOOFFS	0x002	/* use gl_offs */
#define	GLOB_ERR	0x004	/* return on error */
#define	GLOB_MAGCHAR	0x008	/* pattern had globbing characters */
#define	GLOB_MARK	0x010	/* append / to matching directories */
#define	GLOB_NOCHECK	0x020	/* return pattern itself if nothing matches */
#define	GLOB_NOSORT	0x040	/* don't sort */
#define	GLOB_QUOTE	0x080	/* quote special chars with \ */
#define GLOB_NOMAGIC	0x100	/* like GLOB_NOCHECK but only if the pattern
				 * did not have any magic characters */
#define	GLOB_ALTNOT	0x200	/* use alternate glob character [^ not !] */

#define	GLOB_NOSPACE	(-1)	/* malloc call failed */
#define	GLOB_ABEND	(-2)	/* unignored error */

int glob (const char *, int, int (*)(char *, int), glob_t *);
void globfree (glob_t *);

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

#undef  __P        /* in case already defined elsewhere (by gcc, say)*/
#define __P(a) a

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
Opendir(str)
    register Char *str;
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
Lstat(fn, sb)
    register Char *fn;
    struct stat *sb;
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
Stat(fn, sb)
    register Char *fn;
    struct stat *sb;
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
Strchr(str, ch)
    Char *str;
    int ch;
{
    do
	if (*str == ch)
	    return (str);
    while (*str++);
    return (NULL);
}

#ifdef DEBUG
static void
qprintf(s)
Char *s;
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
compare(p, q)
    const ptr_t  p, q;
{
#if defined(NLS) && !defined(NOSTRCOLL)
    errno = 0;  /* strcoll sets errno, another brain-damage */

    return (strcoll(*(char **) p, *(char **) q));
#else
    return (strcmp(*(char **) p, *(char **) q));
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
glob(pattern, flags, errfunc, pglob)
    const char *pattern;
    int     flags;
    int     (*errfunc) __P((char *, int));
    glob_t *pglob;
{
    int     err, oldpathc;
    Char *bufnext, *bufend, *compilebuf, m_not;
    const unsigned char *compilepat, *patnext;
    int     c, not;
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
	not = ALTNOT;
	m_not = M_ALTNOT;
    }
    else {
	not = NOT;
	m_not = M_NOT;
    }

    bufnext = patbuf;
    bufend = bufnext + MAXPATHLEN;
    compilebuf = bufnext;
    compilepat = patnext;

    no_match = *patnext == not;
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
	    if (c == not)
		++qpatnext;
	    if (*qpatnext == EOS ||
		Strchr(qpatnext + 1, RBRACKET) == NULL) {
		*bufnext++ = LBRACKET;
		if (c == not)
		    --qpatnext;
		break;
	    }
	    pglob->gl_flags |= GLOB_MAGCHAR;
	    *bufnext++ = M_SET;
	    if (c == not)
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
glob1(pattern, pglob, no_match)
    Char *pattern;
    glob_t *pglob;
    int     no_match;
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
glob2(pathbuf, pathend, pattern, pglob, no_match)
    Char *pathbuf, *pathend, *pattern;
    glob_t *pglob;
    int     no_match;
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
glob3(pathbuf, pathend, pattern, restpattern, pglob, no_match)
    Char *pathbuf, *pathend, *pattern, *restpattern;
    glob_t *pglob;
    int     no_match;
{
    extern int errno;
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
    errno = 0;

    if (!(dirp = Opendir(pathbuf))) {
	/* todo: don't call for ENOENT or ENOTDIR? */
	for (ptr = cpathbuf; (*ptr++ = (char) *pathbuf++) != EOS;)
	    continue;
	if ((pglob->gl_errfunc && (*pglob->gl_errfunc) (cpathbuf, errno)) ||
	    (pglob->gl_flags & GLOB_ERR))
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
 * Extend the gl_pathv member of a glob_t structure to accomodate a new item,
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
globextend(path, pglob)
    Char *path;
    glob_t *pglob;
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
match(name, pat, patend, m_not)
    register Char *name, *pat, *patend;
    int m_not;
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
globfree(pglob)
    glob_t *pglob;
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

static int warn = 0 ;
void MCW_warn_expand( int www ){ warn = www; return; }

/*------------------------------------------------------------------------
   Routines that allows filename wildcarding to be handled inside
   to3d.  The advantage: limitations of shell command line lengths.
   29 July 1996:  Incorporated "glob" functions from tcsh-6.05, rather
                    than rely on system supplying a library.
   30 July 1996:  Extended routine to allow for 3D: type prefixes.
   10 Feb  2000:  and for 3A: prefixes.
--------------------------------------------------------------------------*/

void NIH_glob( char *fn , int *nout , char ***fout )
{
   MCW_file_expand( 1 , &fn , nout , fout ) ;
}

void NIH_glob_free( int gnum , char **gout )
{
   MCW_free_expand( gnum , gout ) ;
}

/*------------------------------------------------------------------------*/

void MCW_file_expand( int nin , char ** fin , int * nout , char *** fout )
{
   glob_t gl ;
   int    ii , gnum, gold , ilen ;
   char ** gout ;
   char *  fn ;
   char prefix[4] , fpre[128] , fname[256] ;
   int  b1,b2,b3,b4,b5 , ib,ig , lpre ;

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

void MCW_free_expand( int gnum , char ** gout )
{
   int ii ;

   if( gout == NULL ) return ;

   for( ii=0 ; ii < gnum ; ii++ ) free( gout[ii] ) ;
   free( gout ) ;
   return ;
}

/***************************************************************************/
/*** adapted from ge_header.c ***/

/*---------------------------------------------------------------*/

static void swap_4(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;

   b0 = *pntr; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   *pntr = b3; *(pntr+1) = b2; *(pntr+2) = b1; *(pntr+3) = b0;
}

/*---------------------------------------------------------------*/

static void swap_8(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;
   unsigned char b4, b5, b6, b7;

   b0 = *pntr    ; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   b4 = *(pntr+4); b5 = *(pntr+5); b6 = *(pntr+6); b7 = *(pntr+7);

   *pntr     = b7; *(pntr+1) = b6; *(pntr+2) = b5; *(pntr+3) = b4;
   *(pntr+4) = b3; *(pntr+5) = b2; *(pntr+6) = b1; *(pntr+7) = b0;
}

/*---------------------------------------------------------------*/

static void swap_2(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1;

   b0 = *pntr; b1 = *(pntr+1);
   *pntr = b1; *(pntr+1) = b0;
}

/******************************************************************/
/*** Return info from a GEMS IMGF file into user-supplied struct **/

void ge_header( char *pathname , ge_header_info *hi )
{
   FILE *imfile ;
   int  length , skip , swap=0 , gg ;
   char orients[8] , str[8] ;
   int nx , ny , bpp , cflag , hdroff , stamp=0 , iarg=1 ;

   if( hi == NULL ) return ;            /* bad */
   hi->good = 0 ;                       /* not good yet */
   if( pathname    == NULL ||
       pathname[0] == '\0'   ) return ; /* bad */

   length = THD_filesize( pathname ) ;
   if( length < 1024 ) return ;         /* bad */

   imfile = fopen( pathname , "r" ) ;
   if( imfile == NULL ) return ;        /* bad */

   strcpy(str,"JUNK") ;     /* initialize string */
   fread(str,1,4,imfile) ;  /* check for "IMGF" at start of file */

   if( str[0]!='I' || str[1]!='M' || str[2]!='G' || str[3]!='F' ){ /* bad */
      fclose(imfile) ; return ;
   }

   /*-- read next 5 ints (after the "IMGF" string) --*/

   fread( &skip , 4,1, imfile ) ; /* offset into file of image data */
   fread( &nx   , 4,1, imfile ) ; /* x-size */
   fread( &ny   , 4,1, imfile ) ; /* y-size */
   fread( &bpp  , 4,1, imfile ) ; /* bits per pixel (should be 16) */
   fread( &cflag, 4,1, imfile ) ; /* compression flag (1=uncompressed)*/

   /*-- check if nx is funny --*/

   if( nx < 0 || nx > 8192 ){      /* have to byte swap these 5 ints */
     swap = 1 ;                    /* flag to swap data, too */
     swap_4(&skip); swap_4(&nx); swap_4(&ny); swap_4(&bpp); swap_4(&cflag);
   } else {
     swap = 0 ;  /* data is ordered for this CPU */
   }
   if( nx < 0 || nx > 8192 || ny < 0 || ny > 8192 ){  /* bad */
      fclose(imfile) ; return ;
   }

   hi->nx = nx ;
   hi->ny = ny ;

   if( skip+2*nx*ny >  length ||               /* file is too short */
       skip         <= 0      ||               /* bizarre  */
       cflag        != 1      ||               /* data is compressed */
       bpp          != 16        ) return ;    /* data is not shorts */

   /*-- try to read image header data as well --*/

   fseek( imfile , 148L , SEEK_SET ) ; /* magic GEMS offset */
   fread( &hdroff , 4,1 , imfile ) ;   /* location of image header */
   if( swap ) swap_4(&hdroff) ;

   if( hdroff > 0 && hdroff+256 < length ){   /* can read from image header */
       float dx,dy,dz, xyz[9], zz, tr ; int itr, ii,jj,kk ;

       /*-- get voxel grid sizes --*/

       fseek( imfile , hdroff+26 , SEEK_SET ) ;    /* dz */
       fread( &dz , 4,1 , imfile ) ;

       fseek( imfile , hdroff+50 , SEEK_SET ) ;    /* dx and dy */
       fread( &dx , 4,1 , imfile ) ;
       fread( &dy , 4,1 , imfile ) ;

       if( swap ){ swap_4(&dx); swap_4(&dy); swap_4(&dz); }

       hi->dx = dx ; hi->dy = dy ; hi->dz = dz ;

       /* grid orientation: from 3 sets of LPI corner coordinates: */
       /*   xyz[0..2] = top left hand corner of image     (TLHC)   */
       /*   xyz[3..5] = top right hand corner of image    (TRHC)   */
       /*   xyz[6..8] = bottom right hand corner of image (BRHC)   */
       /* GEMS coordinate orientation here is LPI                  */

       fseek( imfile , hdroff+154 , SEEK_SET ) ;  /* another magic number */
       fread( xyz , 4,9 , imfile ) ;
       if( swap ){
          swap_4(xyz+0); swap_4(xyz+1); swap_4(xyz+2);
          swap_4(xyz+3); swap_4(xyz+4); swap_4(xyz+5);
          swap_4(xyz+6); swap_4(xyz+7); swap_4(xyz+8);
       }

       /* x-axis orientation */
       /* ii determines which spatial direction is x-axis  */
       /* and is the direction that has the biggest change */
       /* between the TLHC and TRHC                        */

       dx = fabs(xyz[3]-xyz[0]) ; ii = 1 ;
       dy = fabs(xyz[4]-xyz[1]) ; if( dy > dx ){ ii=2; dx=dy; }
       dz = fabs(xyz[5]-xyz[2]) ; if( dz > dx ){ ii=3;        }
       dx = xyz[ii+2]-xyz[ii-1] ; if( dx < 0. ){ ii = -ii;    }
       switch( ii ){
        case  1: orients[0]= 'L'; orients[1]= 'R'; break; /* Left      to Right     */
        case -1: orients[0]= 'R'; orients[1]= 'L'; break; /* Right     to Left      */
        case  2: orients[0]= 'P'; orients[1]= 'A'; break; /* Posterior to Anterior  */
        case -2: orients[0]= 'A'; orients[1]= 'P'; break; /* Anterior  to Posterior */
        case  3: orients[0]= 'I'; orients[1]= 'S'; break; /* Inferior  to Superior  */
        case -3: orients[0]= 'S'; orients[1]= 'I'; break; /* Superior  to Inferior  */
        default: orients[0]='\0'; orients[1]='\0'; break; /* should never happen    */
       }

       /* y-axis orientation */
       /* jj determines which spatial direction is y-axis  */
       /* and is the direction that has the biggest change */
       /* between the BRHC and TRHC                        */

       dx = fabs(xyz[6]-xyz[3]) ; jj = 1 ;
       dy = fabs(xyz[7]-xyz[4]) ; if( dy > dx ){ jj=2; dx=dy; }
       dz = fabs(xyz[8]-xyz[5]) ; if( dz > dx ){ jj=3;        }
       dx = xyz[jj+5]-xyz[jj+2] ; if( dx < 0. ){ jj = -jj;    }
       switch( jj ){
         case  1: orients[2] = 'L'; orients[3] = 'R'; break;
         case -1: orients[2] = 'R'; orients[3] = 'L'; break;
         case  2: orients[2] = 'P'; orients[3] = 'A'; break;
         case -2: orients[2] = 'A'; orients[3] = 'P'; break;
         case  3: orients[2] = 'I'; orients[3] = 'S'; break;
         case -3: orients[2] = 'S'; orients[3] = 'I'; break;
         default: orients[2] ='\0'; orients[3] ='\0'; break;
       }

       orients[4] = '\0' ;   /* terminate orientation string */

       kk = 6 - abs(ii)-abs(jj) ;   /* which spatial direction is z-axis   */
                                    /* where 1=LR, 2=PA, 3=IS               */
                                    /* (can't tell orientation from 1 slice) */

       zz = xyz[kk-1] ;             /* z-coordinate of this slice */

       hi->zoff = zz ;
       strcpy(hi->orients,orients) ;

       /*-- get TR in seconds --*/

       fseek( imfile , hdroff+194 , SEEK_SET ) ;
       fread( &itr , 4,1 , imfile ) ; /* note itr is an int */
       if( swap ) swap_4(&itr) ;
       hi->tr = 1.0e-6 * itr ;        /* itr is in microsec */

       /*-- get TE in milliseconds --*/

       fseek( imfile , hdroff+202 , SEEK_SET ) ;
       fread( &itr , 4,1 , imfile ) ; /* itr is an int, in microsec */
       if( swap ) swap_4(&itr) ;
       hi->te = 1.0e-6 * itr ;

       hi->good = 1 ;                 /* this is a good file */

   } /* end of actually reading image header */

   fclose(imfile) ; return ;
}
