/*===========================================================================*
 * general.h								     *
 *									     *
 *	very general stuff						     *
 *									     *
 *===========================================================================*/

/*
 * Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/*  
 *  $Header$
 *  $Log$
 *  Revision 1.5  2004/04/02 15:12:41  rwcox
 *  Cput
 *
 *  Revision 1.4  2004/02/05 21:32:23  rwcox
 *  Cput
 *
 *  Revision 1.3  2003/12/23 13:50:08  rwcox
 *  Cput
 *
 *  Revision 1.2  2003/12/03 14:46:15  rwcox
 *  Cput
 *
 *  Revision 1.1  2001/12/17 18:25:44  rwcox
 *  Cadd
 *
 *  Revision 1.7  1995/08/04 23:34:13  smoot
 *  jpeg5 changed the silly HAVE_BOOLEAN define....
 *
 *  Revision 1.6  1995/01/19 23:54:49  eyhung
 *  Changed copyrights
 *
 * Revision 1.5  1994/11/12  02:12:48  keving
 * nothing
 *
 * Revision 1.4  1993/07/22  22:24:23  keving
 * nothing
 *
 * Revision 1.3  1993/07/09  00:17:23  keving
 * nothing
 *
 * Revision 1.2  1993/06/03  21:08:53  keving
 * nothing
 *
 * Revision 1.1  1993/02/22  22:39:19  keving
 * nothing
 *
 */


#ifndef GENERAL_INCLUDED
#define GENERAL_INCLUDED


/* prototypes for library procedures
 *
 * if your /usr/include headers do not have these, then pass -DMISSING_PROTOS
 * to your compiler
 *
 */ 
#ifdef MISSING_PROTOS
int fprintf();
int fwrite();
int fread();
int fflush();
int fclose();

int sscanf();
int bzero();
int bcopy();
int system();
int time();
int perror();

int socket();
int bind();
int listen();
int accept();
int connect();
int close();
int read();
int write();

int pclose();

#endif


/*===========*
 * CONSTANTS *
 *===========*/

#ifndef NULL
#define NULL 0
#endif

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define SPACE ' '
#define TAB '\t'
#define SEMICOLON ';'
#define NULL_CHAR '\0'
#define NEWLINE '\n'


/*==================*
 * TYPE DEFINITIONS *
 *==================*/

typedef int boolean;
/* this is for JPEG stuff */
#define BOOLEAN_DEFINED
#define HAVE_BOOLEAN

typedef unsigned char uint8;
typedef char int8;
typedef unsigned short uint16;
typedef short int16;

    /* LONG_32 should only be defined iff
     *	    1) long's are 32 bits and
     *	    2) int's are not
     */
#ifdef LONG_32		
typedef unsigned long uint32;
typedef long int32;
#else
typedef unsigned int uint32;
typedef int int32;
#endif


/*========*
 * MACROS *
 *========*/

#undef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#undef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#undef abs
#define abs(a) ((a) >= 0 ? (a) : -(a))


#endif
