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

#ifndef _MCW_GLOB_H_
#define	_MCW_GLOB_H_

/** prototypes for routines added by RWCox **/

#ifdef  __cplusplus
extern "C" {
#endif

extern void MCW_file_expand( int nin , char ** fin , int * nout , char *** fout ) ;
extern void MCW_free_expand( int gnum , char ** gout ) ;
extern void MCW_warn_expand( int www ) ;                             /* 13 Jul 2001 */
extern void MCW_set_glob_whine( int www ) ;                          /* 07 Feb 2012 [rickr] */
extern void MCW_wildcards( char *fnam , int *nout , char ***fout ) ; /* 01 Dec 2003 */
#define MCW_free_wildcards(a,b) MCW_free_expand(a,b)                 /* 02 Dec 2003 */

typedef struct {
	int gl_pathc;		/* count of total paths so far */
	int gl_matchc;		/* count of paths matching pattern */
	int gl_offs;		/* reserved at beginning of gl_pathv */
	int gl_flags;		/* copy of flags parameter to glob() */
	int (*gl_errfunc)(char *, int);	/* copy of errfunc parameter to glob() */
	char **gl_pathv;	/* list of paths matching pattern */
} afni_glob_t;

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

int afni_glob (const char *, int, int (*)(char *, int), afni_glob_t *);
void afni_globfree (afni_glob_t *);
int rglob_set_sort_dir(int dir);        /*  4 Jan 2010 [rickr] */

#ifdef  __cplusplus
}
#endif

#include <string.h>

#include "mcw_malloc.h"

#endif /* !_MCW_GLOB_H_ */
