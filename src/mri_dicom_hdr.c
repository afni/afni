/*****************************************************************************/
/*****************************************************************************/
/*** Function to read DICOM images into memory.
     Adapted from dcm_dump_file.c (et cetera) from the RSNA.
     July 2002 -- RW Cox -- NIMH/NIH/PHS/HHS/USA                           ***/
/*****************************************************************************/
/*****************************************************************************/

/*
          Copyright (C) 1993, 1994, RSNA and Washington University

          The software and supporting documentation for the Radiological
          Society of North America (RSNA) 1993, 1994 Digital Imaging and
          Communications in Medicine (DICOM) Demonstration were developed
          at the
                  Electronic Radiology Laboratory
                  Mallinckrodt Institute of Radiology
                  Washington University School of Medicine
                  510 S. Kingshighway Blvd.
                  St. Louis, MO 63110
          as part of the 1993, 1994 DICOM Central Test Node project for, and
          under contract with, the Radiological Society of North America.

          THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND NEITHER RSNA NOR
          WASHINGTON UNIVERSITY MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
          PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
          USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
          SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
          THE SOFTWARE IS WITH THE USER.

          Copyright of the software and supporting documentation is
          jointly owned by RSNA and Washington University, and free access
          is hereby granted as a license to use this software, copy this
          software and prepare derivative works based upon this software.
          However, any distribution of this software source code or
          supporting documentation or derivative works (source code and
          supporting documentation) must include the three paragraphs of
          the copyright notice.
*/
/* Copyright marker.  Copyright will be inserted above.  Do not remove */
/*
**				DICOM 93
**		     Electronic Radiology Laboratory
**		   Mallinckrodt Institute of Radiology
**		Washington University School of Medicine
**
** Author, Date:	Stephen M. Moore, 9-May-93
** Intent:		This program uses the DICOM OBJECTS package to open
**			DICOM files and dump their contents to stdout.  Each
**			argument to the program is expected to be the name
**			of a file containing a DICOM stream.
**   Usage:
**			dcm_dump_file [-b] [-g] [-v] [-z] file [file ...]
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>

#include "mri_dicom_hdr.h"

/* pass private(!) Siemens slice times back, if found 
 * nused : number of slice times read                    8 Apr 2011 [rickr] */
typedef struct {
   int     nalloc;              /* number of times allocated for  */
   int     nused;               /* actual length of 'times' array */
   float * times;               /* list of slice times            */
} siemens_slice_times_t;

siemens_slice_times_t g_siemens_slice_times = { 0, 0, NULL };
static int g_MDH_verb = 1;             /* verbose level */

int mri_sst_get_verb(void) { return g_MDH_verb; }
int mri_sst_set_verb(int verb) { g_MDH_verb = verb; return g_MDH_verb; }

/* interface to return slice timing info (calling function should copy data)
 *                                                       12 Apr 2011 [rickr] */
int mri_siemens_slice_times(int * nalloc, int * nused, float ** times)
{
   if( ! nalloc || ! nused || ! times ) return 1;
   *nalloc = g_siemens_slice_times.nalloc;
   *nused  = g_siemens_slice_times.nused;
   *times  = g_siemens_slice_times.times;
   return 0;
}

/* Dimon needs to compile without libmri     18 May 2006 */
/* (this allows removal of rickr/l_mri_dicom_hdr.c)      */
#ifndef FOR_DIMON

#include "mcw_malloc.h"
#include "Amalloc.h"
#include "debugtrace.h"    /* 10 Sep 2002 */

#else

#include "Amalloc.h"
#include "dbtrace.h"

#endif

/* cast int to pointer and vice-versa without warning messages */

#ifndef SOLARIS_OLD
#include <stdint.h>
#endif
#undef  ITOP
#define ITOP(qw) ((void *)(intptr_t)(qw))
#undef  PTOI
#define PTOI(qw) ((int)(intptr_t)(qw))

/****************************************************************/
/***** Function and variables to set byte order of this CPU *****/
/****************************************************************/
#define BYTEORDER_SAME        1
#define BYTEORDER_REVERSE     2
#define NATIVE_ORDER          BYTEORDER_SAME

static int LITTLE_ORDER ;
static int BIG_ORDER ;
static int LITTLE_ENDIAN_ARCHITECTURE = -1 ;

static void RWC_set_endianosity(void)
{
   union { unsigned char bb[2] ;
           short         ss    ; } fred ;

   if( LITTLE_ENDIAN_ARCHITECTURE < 0 ){
     fred.bb[0] = 1 ; fred.bb[1] = 0 ;

     LITTLE_ENDIAN_ARCHITECTURE = (fred.ss == 1) ;

     if( LITTLE_ENDIAN_ARCHITECTURE ){
       LITTLE_ORDER = BYTEORDER_SAME ;
       BIG_ORDER    = BYTEORDER_REVERSE ;
     } else {
       BIG_ORDER    = BYTEORDER_SAME ;
       LITTLE_ORDER = BYTEORDER_REVERSE ;
     }
   }
}

/******************************************************************/
/*** decide in DCM_OpenFile whether we have a 128 byte preamble ***/
int g_readpreamble = FALSE;                /* 18 May 2006 [rickr] */

/****************************************************************/
/***** Function and variables to replace printf() ***************/

static int just_do_printf = 0 ;  /* 02 May 2008 */
void mri_dicom_header_use_printf( int i ){ just_do_printf = i; }

/* in order to run diffs, be able to skip sizes and offsets */
static int show_size_n_offset = 1 ;  /* 17 Oct 2012 [rickr] */
void mri_dicom_header_show_size_offset( int i ){ show_size_n_offset = i; }

static char *pbuf = NULL ;  /* output string buffer */
static int  npbuf = 0 ;     /* number of bytes allocated in pbuf */
static int  lpbuf = 0 ;     /* number of bytes used in pbuf */

#define NPBUF 2048

static void RWC_clear_pbuf(void)
{
   if( pbuf != NULL ){ free(pbuf); pbuf = NULL; npbuf = 0; lpbuf = 0; }
}

int RWC_printf( char *fmt , ... )
{
   static char *sbuf = NULL ;
   int nsbuf , nn ;
   va_list vararg_ptr ;

ENTRY("RWC_printf") ;

   va_start( vararg_ptr , fmt ) ;

   if( sbuf == NULL ) sbuf = AFMALL( char, NPBUF) ;  /* 1st time in */

   sbuf[0] = '\0' ;
   nn = vsprintf( sbuf , fmt , vararg_ptr ) ;
   va_end( vararg_ptr ) ;
   nsbuf = strlen(sbuf) ;
   if( nsbuf == 0 ) RETURN(0);

   if( just_do_printf ){ fputs(sbuf,stdout); RETURN(nn); }

   if( npbuf == 0 ){
STATUS("initial allocation of pbuf") ;
     pbuf = AFMALL(char,NPBUF); npbuf = NPBUF; pbuf[0] = '\0'; lpbuf = 0;
   }

#if 0
   lpbuf = strlen(pbuf) ;
#endif
   if( lpbuf+nsbuf+8 > npbuf ){
     npbuf += NPBUF + nsbuf + npbuf/16 ; pbuf = AFREALL( pbuf, char, npbuf) ;
if(PRINT_TRACING){
  char str[256];
  sprintf(str,"realloc pbuf: lpbuf=%d nsbuf=%d npbuf=%d",lpbuf,nsbuf,npbuf);
  STATUS(str);
}
   }
#if 0
   strcat(pbuf,sbuf) ;
#else
   strcpy(pbuf+lpbuf,sbuf) ; lpbuf += nsbuf ;
#endif
   RETURN(nn);
}

/****************************************************************/

static off_t        pxl_off = 0 ;  /* store pixel array offset */
static unsigned int pxl_len = 0 ;  /* and length in file */

void mri_dicom_pxlarr( off_t *poff , unsigned int *plen )
{
   *poff = pxl_off ; *plen = pxl_len ;
}

/****************************************************************/

static int rwc_opt = 0 ;

#define RWC_NONAME_MASK  1
#define RWC_NOHEX_MASK   2

void mri_dicom_noname( int ii )
{
   if( ii )
     rwc_opt |= RWC_NONAME_MASK ;
   else if( rwc_opt & RWC_NONAME_MASK )
     rwc_opt ^= RWC_NONAME_MASK ;
}

void mri_dicom_nohex( int ii )
{
   if( ii )
     rwc_opt |= RWC_NOHEX_MASK ;
   else if( rwc_opt & RWC_NOHEX_MASK )
     rwc_opt ^= RWC_NOHEX_MASK ;
}

/****************************************************************/

static int rwc_vm=0 ;                     /* 28 Oct 2002 */

void mri_dicom_setvm( int vv )
{
  rwc_vm = vv ;
}

/****************************************************************/

static int rwc_err=3 ;                     /* 28 Oct 2002 */

void mri_dicom_seterr( int vv )
{
  rwc_err = vv ;   /* 07 May 2003: an error will subtract 1 from rwc_err */
}

/****************************************************************/

static int rwc_fd ;  /* 10 Sep 2002 */

char * mri_dicom_header( char *fname )
{
    DCM_OBJECT * object;
    CONDITION cond;
    CTNBOOLEAN verbose = FALSE ,
               exitFlag = FALSE,
               formatFlag = FALSE;
    unsigned long
        options = DCM_ORDERLITTLEENDIAN;
    long vmLimit = rwc_vm ;             /* 28 Oct 2002 */
    LST_HEAD* fileNames = 0;
    UTL_FILEITEM* p = NULL;

    char *ppp=NULL ;

ENTRY("mri_dicom_header") ;

    if( fname == NULL ) RETURN(NULL) ;

    RWC_set_endianosity() ;

    { char *eee = getenv("AFNI_TRACE") ;
      if( eee!=NULL && (*eee=='y' || *eee=='Y') ) verbose = TRUE ;
    }

    DCM_Debug(verbose);

    RWC_clear_pbuf() ; pxl_len = 0 ; pxl_off = 0 ;

STATUS(fname) ;
    rwc_fd = -1 ;
    cond = DCM_OpenFile(fname, options, &object);
    if (cond != DCM_NORMAL && ((options & DCM_PART10FILE) == 0)) {
STATUS("DCM_OpenFile open failed; try again as Part 10") ;
      (void) DCM_CloseObject(&object);
      (void) COND_PopCondition(TRUE);
      if( rwc_fd >= 0 ){ close(rwc_fd); rwc_fd = -1; }
      cond = DCM_OpenFile(fname, options | DCM_PART10FILE, &object);
    }
    if (cond == DCM_NORMAL) {
STATUS("DCM_OpenFile is good") ;
       RWC_printf("DICOM File: %s\n", fname);
       if (formatFlag){
STATUS("call DCM_FormatElements") ;
         cond = DCM_FormatElements(&object, vmLimit, "");
       } else {
STATUS("call DCM_DumpElements") ;
         cond = DCM_DumpElements(&object, vmLimit);
       }
    } else {
STATUS("DCM_OpenFile failed") ;
    }
STATUS("closing") ;
    (void) DCM_CloseObject(&object);
    (void) COND_PopCondition(TRUE);

    if( pbuf != NULL ){
      ppp = strdup(pbuf) ; RWC_clear_pbuf() ;  /* copy results for output */
    }

    if( rwc_fd >= 0 ){ close(rwc_fd); rwc_fd = -1; }

    RETURN(ppp);  /* output */
}

/*
          Copyright (C) 1993, 1994, RSNA and Washington University

          The software and supporting documentation for the Radiological
          Society of North America (RSNA) 1993, 1994 Digital Imaging and
          Communications in Medicine (DICOM) Demonstration were developed
          at the
                  Electronic Radiology Laboratory
                  Mallinckrodt Institute of Radiology
                  Washington University School of Medicine
                  510 S. Kingshighway Blvd.
                  St. Louis, MO 63110
          as part of the 1993, 1994 DICOM Central Test Node project for, and
          under contract with, the Radiological Society of North America.

          THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND NEITHER RSNA NOR
          WASHINGTON UNIVERSITY MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
          PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
          USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
          SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
          THE SOFTWARE IS WITH THE USER.

          Copyright of the software and supporting documentation is
          jointly owned by RSNA and Washington University, and free access
          is hereby granted as a license to use this software, copy this
          software and prepare derivative works based upon this software.
          However, any distribution of this software source code or
          supporting documentation or derivative works (source code and
          supporting documentation) must include the three paragraphs of
          the copyright notice.
*/
/* Copyright marker.  Copyright will be inserted above.  Do not remove */
/*
** @$=@$=@$=
*/
/*
**				DICOM 93
**		     Electronic Radiology Laboratory
**		   Mallinckrodt Institute of Radiology
**		Washington University School of Medicine
**
** Module Name(s):	COND_PushCondition
**			COND_ExtractConditions
**			COND_TopCondition
**			COND_PopCondition
**			COND_DumpConditions
**			COND_CopyText
**			COND_WriteConditions
** Author, Date:	Stephen M. Moore, 15-Apr-93
** Intent:		This file contains functions implementing a simple
**			error facility.  It was first written by Stephen Moore
**			(smm@wuerl.wustl.edu) to support PACS development at
**			the Mallinckrodt Institute of Radiology.  The function
**			names have been modified to have a slightly more
**			generic name, but the basic model and concepts are
**			the same.
**
**			The condition package maintains a stack of
**			<condition, message> pairs that callers can push or
**			pop.  When a routine returns an abnormal value, it
**			should push a condition onto the stack so that the
**			caller can examine the value at a later time.  Nested
**			routines may push a number of conditions onto the
**			stack providing more detailed information about why
**			a routine did not return normally.
**
**			The stack is maintained as a simple stack array.  If
**			it overflows, we dump the stack to stdout and reset it.
**
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/


/*
**
**  INCLUDE FILES
**
*/


typedef struct {
    CONDITION statusCode;
    char statusText[256];
}   EDB;

#define MAXEDB  100

static int stackPtr = -1;
static EDB EDBStack[MAXEDB];
static void (*ErrorCallback) () = NULL;
static void dumpstack(FILE * fp);


/*
**++
**  FUNCTIONAL DESCRIPTION:
**
**      COND_PushCondition
**	This routine is used to log a condition on the stack.  The user
**	passes an error code (currently uninterpreted), a format string
**	and optional format arguments.  We use the vsprintf routine to
**	interpret the user's format string and arguments, and we place the
**	error condition and resultant string on the stack.
**
**  FORMAL PARAMETERS:
**
**      code:
**          The condition code of the error/warning.
**
**      controlString:
**          format string for vsprintf statement
**
**      [varargs]:
**          variable arguments to be used with controlString
**
**  RETURN VALUE:
**
**      code (as passed in by the user)
**
**  SIDE EFFECTS:
**
**      Places a new entry on the stack.  If the stack
**	fills up, drop the last condition.
**	Calls a user-established callback just before return.
**
*/
CONDITION
COND_PushCondition(CONDITION cond, char *controlString,...)
{
    va_list
	args;
    char
        buffer[1024];

/*lint -e40 -e50 */
    va_start(args, controlString);
    if (controlString == NULL)
	controlString = "NULL Control string passedto PushCondition";
    (void) vsprintf(buffer, controlString, args);
    va_end(args);
/*lint +e40 +e50 */

    stackPtr++;
    EDBStack[stackPtr].statusCode = cond;
    buffer[256] = '\0';

    (void) strcpy(EDBStack[stackPtr].statusText, buffer);
    if (ErrorCallback != NULL)
#if 0
	ErrorCallback(EDBStack[stackPtr].statusCode,
		      EDBStack[stackPtr].statusText);
#else
        AFNI_CALL_VOID_2ARG( ErrorCallback ,
                             CONDITION,EDBStack[stackPtr].statusCode ,
                             char *   ,EDBStack[stackPtr].statusText  ) ;
#endif

    if (stackPtr >= MAXEDB - 2) {
	dumpstack(stderr);
	fprintf(stderr, "CONDITION Stack overflow\n");
	stackPtr = 0;
    }

    return cond;

}


/*
**++
**  FUNCTIONAL DESCRIPTION:
**
**  COND_ExtractConditions
**	This routine walks through the stack and passes the condition
**	codes and text back to the user.  The caller supplies a
**	callback routine.  We start at the top of the stack and
**	call the user's callback for each message on the stack.  The
**	user can terminate the process at any time by returning
**	a zero from his callback.
**
**  FORMAL PARAMETERS:
**
**      callback:
**          User routine to call for each message on the stack.
**
**  RETURN VALUE:
**
**      1
**
**  SIDE EFFECTS:
**
**
**  DESIGN:
**
**      None
**--
*/

CONDITION
COND_ExtractConditions(CTNBOOLEAN(*callback) ())
{
    int
        index,
        returnflag;

    for (index = stackPtr, returnflag = 1; index >= 0 && returnflag != 0;
	 index--) {
#if 0
	returnflag = callback(EDBStack[index].statusCode,
			      EDBStack[index].statusText);
#else
        AFNI_CALL_VALU_2ARG( callback , int,returnflag            ,
                             CONDITION,EDBStack[index].statusCode ,
                             char *   ,EDBStack[index].statusText  ) ;
#endif
    }

    return COND_NORMAL;
}

/*
**++
**  FUNCTIONAL DESCRIPTION:
**
**      COND_TopCondition
**	This routine is used to look at the top condition message on
**	the stack.  The user passes pointers to areas to place
**	the error message.  The function also returns the code
**	for the top error message.  If the stack is empty, the
**	success code (0) is returned.
**
**  FORMAL PARAMETERS:
**
**      code:
**          Pointer to the user's area to hold the error code
**
**      text
**          Pointer to the user's area to hold the error text
**
**      maxlength
**          Maximum buffer length in the user's text area
**
**  RETURN VALUE:
**
**      top error code on the stack
**
**  SIDE EFFECTS:
**
**
*/

CONDITION
COND_TopCondition(CONDITION * code, char *text, unsigned long maxlength)
{
    CONDITION rtnValue;

    if (stackPtr >= 0) {
	*code = EDBStack[stackPtr].statusCode;
	(void) strncpy(text, EDBStack[stackPtr].statusText, maxlength - 1);
	text[maxlength - 1] = '\0';
	rtnValue = EDBStack[stackPtr].statusCode;
    } else {
	*code = COND_NORMAL;
	*text = '\0';
	rtnValue = COND_NORMAL;
    }

    return rtnValue;
}

/*
**++
**  FUNCTIONAL DESCRIPTION:
**
**      COND_PopCondition
**	This routine pops one or all conditions off the stack.
**	The user passes a flag which indicates the operation.
**	After the clear, the current top error code is returned.
**	If the stack is empty at this point, the success code (0)
**	is returned.
**
**  FORMAL PARAMETERS:
**
**      clearstack:
**          Flag which indicates if the entire stack is to be cleared.
**		0	    Just pop the top error
**		non zero    Clear the entire stack
**
**  RETURN VALUE:
**
**      The new top error code.  0 if the stack is empty
**
**  SIDE EFFECTS:
**
**
*/

CONDITION
COND_PopCondition(CTNBOOLEAN clearstack)
{
    CONDITION
	value;

    if (stackPtr >= 0)
	value = EDBStack[stackPtr].statusCode;
    else
	value = COND_NORMAL;

    if (clearstack) {
	stackPtr = -1;
    } else if (stackPtr <= 0) {
	stackPtr = -1;
    } else {
	stackPtr--;
    }

    return value;
}

/*
**++
**  FUNCTIONAL DESCRIPTION:
**
**      COND_EstablishCallback
**	Establishes a callback routine to be called whenever a
**	new condition is placed on the stack.  There is no stack
**	mechanism for these callbacks, so each new callback routine
**	completely supersedes the previous one.
**
**  FORMAL PARAMETERS:
**
**      callback:
**          The new callback routine.  If NULL, this will
**	    disable callbacks.
**
**  RETURN VALUE:
**
**      0
**
**  SIDE EFFECTS:
**
**
*/

CONDITION
COND_EstablishCallback(void (*callback) ())
{

    ErrorCallback = callback;

    return COND_NORMAL;
}


/* function name
**
** Purpose:
**	Describe the purpose of the function
**
** Parameter Dictionary:
**	Define the parameters to the function
**
** Return Values:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

void
COND_DumpConditions(void)
{

    dumpstack(stderr);
    stackPtr = -1;
}

static void
dumpstack(FILE * lfp)
{
    int
        index;

    for (index = 0; index <= stackPtr; index++)
	fprintf(lfp, "%8x %s\n", (unsigned int)EDBStack[index].statusCode,
		EDBStack[index].statusText);
}

/*
**++
**  FUNCTIONAL DESCRIPTION:
**
**      COND_CopyText
**	This function copies as much text as possible from the
**	condition stack and places it in the caller's buffer.
**
**  FORMAL PARAMETERS:
**
**      txt
**          Pointer to the user's area to hold the error text
**
**      length
**          Maximum buffer length in the user's text area
**
**  RETURN VALUE:
**
**      none
**
**  SIDE EFFECTS:
**
*/

void
COND_CopyText(char *txt, size_t length)
{
    size_t i;
    int j;

    txt[0] = '\0';

    j = stackPtr;
    while (length > 2 && j >= 0) {
	i = strlen(EDBStack[j].statusText);
	if (i > length)
	    i = length - 2;
	strncpy(txt, EDBStack[j].statusText, i);
	txt[i++] = '\n';
	txt[i] = '\0';
	length -= i;
	txt += i;
	j--;
    }
}

/* COND_WriteConditions
**
** Purpose:
**	Write the condition stack to a file, ie stdout or stderr.
**
** Parameter Dictionary:
**	File * lfp, the file to which the stack is written.
**
** Return Values:
**
** Algorithm:
**	A reiteration of the COND_DumpConditions except this takes an argument.
*/

void
COND_WriteConditions(FILE * lfp)
{
    dumpstack(lfp);
    stackPtr = -1;
}
/*
          Copyright (C) 1995 - 1996, RSNA and Washington University

          The software and supporting documentation for the Radiological
          Society of North America (RSNA) 1993 - 1996 Digital Imaging and
          Communications in Medicine (DICOM) Demonstration were developed
          at the
                  Electronic Radiology Laboratory
                  Mallinckrodt Institute of Radiology
                  Washington University School of Medicine
                  510 S. Kingshighway Blvd.
                  St. Louis, MO 63110
          as part of the 1993 - 1996 DICOM Central Test Node project for, and
          under contract with, the Radiological Society of North America.

          THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND NEITHER RSNA NOR
          WASHINGTON UNIVERSITY MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
          PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
          USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
          SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
          THE SOFTWARE IS WITH THE USER.

          Copyright of the software and supporting documentation is
          jointly owned by RSNA and Washington University, and free access
          is hereby granted as a license to use this software, copy this
          software and prepare derivative works based upon this software.
          However, any distribution of this software source code or
          supporting documentation or derivative works (source code and
          supporting documentation) must include the three paragraphs of
          the copyright notice.
*/
/*
+-+-+-+-+-+-+-+-+-
*/
/*
**		     Electronic Radiology Laboratory
**		   Mallinckrodt Institute of Radiology
**		Washington University School of Medicine
**
** Module Name(s):
** Author, Date:	Steve Moore, 30-Jun-96
** Intent:		Provide common abstractions needed for operations
**			in a multi-threaded environment.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

/*
          Copyright (C) 1993, 1994, RSNA and Washington University

          The software and supporting documentation for the Radiological
          Society of North America (RSNA) 1993, 1994 Digital Imaging and
          Communications in Medicine (DICOM) Demonstration were developed
          at the
                  Electronic Radiology Laboratory
                  Mallinckrodt Institute of Radiology
                  Washington University School of Medicine
                  510 S. Kingshighway Blvd.
                  St. Louis, MO 63110
          as part of the 1993, 1994 DICOM Central Test Node project for, and
          under contract with, the Radiological Society of North America.

          THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND NEITHER RSNA NOR
          WASHINGTON UNIVERSITY MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
          PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
          USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
          SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
          THE SOFTWARE IS WITH THE USER.

          Copyright of the software and supporting documentation is
          jointly owned by RSNA and Washington University, and free access
          is hereby granted as a license to use this software, copy this
          software and prepare derivative works based upon this software.
          However, any distribution of this software source code or
          supporting documentation or derivative works (source code and
          supporting documentation) must include the three paragraphs of
          the copyright notice.
*/
/* Copyright marker.  Copyright will be inserted above.  Do not remove */

/*
**				DICOM 93
**		     Electronic Radiology Laboratory
**		   Mallinckrodt Institute of Radiology
**		Washington University School of Medicine
**
** Module Name(s):	DCM_OpenFile
**			DCM_CreateObject
**			DCM_CloseObject
**			DCM_AddElement
**			DCM_AddSequenceElement
**			DCM_RemoveElement
**			DCM_GetElementValue
**			DCM_GetElementSize
**			DCM_ScanParseObject
**			DCM_ImportStream
**			DCM_ExportStream
**			DCM_GetObjectSize
**			DCM_DumpElements
**			DCM_Debug
**			DCM_WriteFile
**			DCM_ModifyElements
**			DCM_ParseObject
**			DCM_RemoveGroup
**			DCM_GetSequenceList
**			DCM_ComputeExportSize
**	private functions
**			newElementItem
**			findCreateGroup
**			insertNewElement
**			updateObjectType
**			updateSpecialElements
**			exportFixedFields
**			exportData
**			fileSize
**			swapInPlace
**			checkObject
**			writeFile
**			countBytes
**			exportStream
**			verifyFormat
**			readFile
** Author, Date:	Stephen M. Moore, 26-Apr-93
** Intent:
**	This file contains the routines which implement a facility for
**	manipulating DICOM V3 information objects.  These routines parse
**	and construct NEMA objects (or streams).  Users are able to
**	read/write/examine individual attributes.  The package uses those
**	individual elements to construct an internal memory representation of
**	an object.  This representation is a linked list of the individual
**	attributes.  The user of the package can add attributes to an object,
**	remove an element from an object, query the object about an attribute,
**	and convert the object to and from its "stream" representation.
**	In addition, the package can parse a file which contains a stream
**	and create its internal object.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/


static CTNBOOLEAN debug = FALSE;/* Flag for debugging messages to stdout */

/* Prototypes for internal functions
*/
static CONDITION
newElementItem(DCM_ELEMENT * src, CTNBOOLEAN allocateData,
	       PRV_ELEMENT_ITEM ** dst);
static CONDITION
findCreateGroup(PRIVATE_OBJECT ** object, unsigned short group,
		PRV_GROUP_ITEM ** groupPtr);
static CONDITION
insertNewElement(PRIVATE_OBJECT ** object,
		 DCM_ELEMENT * element);
static CONDITION
updateObjectType(PRIVATE_OBJECT ** object,
		 DCM_ELEMENT * element);
static CONDITION
updateSpecialElements(PRIVATE_OBJECT ** object,
		      PRV_ELEMENT_ITEM * item);
static void
exportFixedFields(DCM_ELEMENT * element,
		  unsigned char *b, U32 length, int byteOrder,
		  CTNBOOLEAN explicitVR, U32 * rtnLength);
static CONDITION
exportData(PRIVATE_OBJECT ** object, PRV_ELEMENT_ITEM * item,
	   unsigned char *src,
	   unsigned char *dst, U32 length, int byteOrder,
	   U32 * rtnLength);
#ifdef MACOS
static long fileSize(int fd);
#else
static int fileSize(int fd);
#endif
static void swapInPlace(PRIVATE_OBJECT ** object, DCM_ELEMENT * e);
static CONDITION checkObject(PRIVATE_OBJECT ** object, char *caller);
static CONDITION
    writeFile(void *buffer, U32 length, int flag, void /* int */ *fd);
static CONDITION
countBytes(void *buffer, U32 length, int flag,
	   void /* unsigned long */ *sizePtr);
static CONDITION
exportStream(DCM_OBJECT ** callerObject, unsigned long opt,
	     void *buffer, U32 bufferlength, CONDITION(*callback) (),
	     void *ctx, int sequenceLevel);

static CONDITION
    verifyFormat(PRV_ELEMENT_ITEM * item);
static CONDITION
readFile(char *name, unsigned char *callerBuf, int fd, long size,
	 off_t fileOffset, int recursionLevel,
	 unsigned long opt, DCM_OBJECT ** callerObject,
	 U32 * scannedLength, CTNBOOLEAN * remainOpenFlag,
	 void *ctx,
	 CONDITION(*rd) (void *ctx, void *buf, int toRead, int *bytesRead),
	 CONDITION(*sk) (void *ctx, int offset, int flag));
static CONDITION
readFile1(const char *name, unsigned char *callerBuf, int fd, U32 size,
	  off_t * fileOffset, int recursionLevel,
	  unsigned long opt, PRIVATE_OBJECT ** parentObject,
	  DCM_OBJECT ** callerObject,
	  U32 * scannedLength, CTNBOOLEAN * remainOpenFlag,
	  void *ctx,
	  CONDITION(*rd) (void *ctx, void *buf, int toRead, int *bytesRead),
	  CONDITION(*sk) (void *ctx, int offset, int flag));

static PRV_ELEMENT_ITEM *locateElement(PRIVATE_OBJECT ** obj, DCM_TAG tag);
static void computeVM(PRIVATE_OBJECT ** object, DCM_ELEMENT * element);
static void ctxSensitiveLookup(PRIVATE_OBJECT ** object, DCM_ELEMENT * element);
static CONDITION
copyData(PRIVATE_OBJECT ** object, PRV_ELEMENT_ITEM * item,
	 DCM_ELEMENT * to, U32 * rtnLength);
static CONDITION
readLengthToEnd(int fd, const char *fileName,
		unsigned long opt, U32 * lengthToEnd);

static void swapATGroupElement(DCM_ELEMENT * e);

static void
dumpBinaryData(void *d, DCM_VALUEREPRESENTATION vr,
	       long vm, long vmLimit);
static void
compareGroup(PRV_GROUP_ITEM * g1, PRV_GROUP_ITEM * g2,
	     void (*callback) (const DCM_ELEMENT * e1,
			       const DCM_ELEMENT * e2,
			       void *ctx),
	     void *ctx);
static void remapFileName(const char *name, char *mapName);


/* DCM_OpenFile
**
** Purpose:
**  This function opens a file that conforms to the DICOM V3 standard.
**  The file is parsed and an internal representation of the data is created.
**  A handle is returned to the caller to allow access to the data.
**
** Parameter Dictionary:
**   name	ASCIZ string giving path name to file to be opened.
**   opt	BITMASK giving options used when opening file and
**		interpreting data.  Current options have to do with the
**		byte order of the data in the file.  Legal masks for
**		this field are:
**			DCM_ORDERNATIVE
**			DCM_ORDERLITTLEENDIAN
**			DCM_ORDERBIGENDIAN
**   object	Pointer to handle to return to caller  giving caller
**		future access to the data in the object.
**
** Return Values:
**
**	DCM_ELEMENTCREATEFAILED
**	DCM_ELEMENTLENGTHERROR
**	DCM_ELEMENTOUTOFORDER
**	DCM_FILEACCESSERROR
**	DCM_FILEOPENFAILED
**	DCM_ILLEGALOPTION
**	DCM_ILLEGALSTREAMLENGTH
**	DCM_LISTFAILURE
**	DCM_NORMAL
**	DCM_OBJECTCREATEFAILED
**	DCM_UNEVENELEMENTLENGTH
**
** Algorithm:
**	Determine file byte order (per caller's options)
**	Create new ACR object
**	Open file read only.
**	Determine size of file
**	While you have not reached end of file
**	    Read next (group, element, length) triple
**	    Lookup data element in dictionary
**	    Read data value according to byte order and (group,element)
**	    Check to see that data element is in numerical order
**	    Add data element to linked list
**	End while
*/

CONDITION
DCM_OpenFile(const char *name, unsigned long opt, DCM_OBJECT ** callerObject)
{
    CONDITION cond;
    int fd;
    off_t fileOffset = 0;
    U32 lengthToEnd;
    U32 size;
    CTNBOOLEAN
	remainFileOpen = FALSE;	/* Leave file open after parse ? */

ENTRY("DCM_OpenFile") ;

    if ((opt & (DCM_ORDERMASK | DCM_FILEFORMATMASK)) == 0)
	RETURN(COND_PushCondition(DCM_ILLEGALOPTION,
			       DCM_Message(DCM_ILLEGALOPTION), "Byte order",
				  "DCM_OpenFile"));

#ifdef _MSC_VER
    fd = open(name, O_RDONLY | O_BINARY);
#else
    rwc_fd = fd = open(name, O_RDONLY);
#endif
    if ((fd < 0) && ((opt & DCM_FILENAMEMASK) == DCM_TRYFILENAMECHANGE)) {
	char mapName[1024];
	remapFileName(name, mapName);
#ifdef _MSC_VER
	fd = open(mapName, O_RDONLY | O_BINARY);
#else
	fd = open(mapName, O_RDONLY);
	if (fd < 0) {
	    strcat(mapName, ".");
	    fd = open(mapName, O_RDONLY);
	}
#endif
    }
    if (fd < 0) {
        char msg[1024] ;
        sprintf(msg,"DCM_OpenFile open(%s) fails",name) ;
        perror(msg) ;
	RETURN(COND_PushCondition(DCM_FILEOPENFAILED,
				  DCM_Message(DCM_FILEOPENFAILED), name,
				  "DCM_OpenFile"));
    }
    size = fileSize(fd);
    if (size <= 0)
	RETURN(DCM_FILEACCESSERROR);

    /* check whether this file has a preamble   18 May 2006 */
    {
        char lbuf[132];             /* 128 preamble + 4 byte "DICM" label */
        if (read(fd, lbuf, 132) != 132) {
            (void) close(fd); rwc_fd = -1;
            RETURN(COND_PushCondition(DCM_FILEOPENFAILED,
                     DCM_Message(DCM_FILEOPENFAILED), name, "DCM_OpenFile"));
        }
        g_readpreamble = (strncmp(lbuf+128, "DICM", 4) == 0);
        (void) lseek(fd, 0, SEEK_SET); /* either way, rewind */
    }

    if ((opt & DCM_LENGTHTOENDMASK) == DCM_USELENGTHTOEND) {
	cond = readLengthToEnd(fd, name,
			       opt & (~DCM_LENGTHTOENDMASK), &lengthToEnd);
	if (cond != DCM_NORMAL) {
	    (void) close(fd); rwc_fd = -1 ;
	    RETURN(COND_PushCondition(DCM_FILEOPENFAILED,
		     DCM_Message(DCM_FILEOPENFAILED), name, "DCM_OpenFile"));
	}
	size = lengthToEnd;
	fileOffset = 24;
	(void) lseek(fd, 24, SEEK_SET);
    }
#ifdef OLDSMM
    cond = readFile(name, NULL, fd, size, 0, 0, opt, callerObject, NULL,
		    &remainFileOpen, NULL, NULL, NULL);
#endif
    cond = readFile1(name, NULL, fd, size, &fileOffset, 0, opt, NULL,
		     callerObject, NULL, &remainFileOpen, NULL, NULL, NULL);
    if ((cond != DCM_NORMAL) || !remainFileOpen){
	(void) close(fd); rwc_fd = -1 ;
    }
    if (cond != DCM_NORMAL) {
	if (debug)
	    DCM_DumpElements(callerObject, 1);
	RETURN(COND_PushCondition(DCM_FILEOPENFAILED,
		     DCM_Message(DCM_FILEOPENFAILED), name, "DCM_OpenFile"));
    } else
	RETURN(DCM_NORMAL);
}

CONDITION
DCM_ReadStream(DCM_OBJECT ** callerObject, unsigned long opt, long size,
	       void *ctx,
	  CONDITION(*rd) (void *ctx, void *buf, int toRead, int *bytesRead),
	       CONDITION(*sk) (void *ctx, int offset, int flag))
{
    CONDITION cond;
    int fd = -1;
    CTNBOOLEAN
	remainFileOpen = FALSE;	/* Leave file open after parse ? */
    off_t fileOffset = 0;

    if ((opt & (DCM_ORDERMASK | DCM_FILEFORMATMASK)) == 0)
	return COND_PushCondition(DCM_ILLEGALOPTION,
			       DCM_Message(DCM_ILLEGALOPTION), "Byte order",
				  "DCM_ReadStream");

    cond = readFile1("", NULL, fd, size, &fileOffset, 0, opt, NULL,
		     callerObject, NULL, &remainFileOpen, ctx, rd, sk);
    if (cond != DCM_NORMAL)
	return COND_PushCondition(DCM_READSTREAMFAILED,
		       DCM_Message(DCM_READSTREAMFAILED), "DCM_ReadStream");
    else
	return DCM_NORMAL;
}

/* DCM_CreateObject
**
** Purpose:
**	This function creates a new object and initializes some
**	of the fields in the object
**
** Parameter Dictionary:
**	object		Pointer to caller's handle for object to be created.
**	opt		Flag with options used when creating object.
**			The only option that we use now is DCM_NOGROUPLENGTH.
**
** Return Values:
**	DCM_NORMAL
**	DCM_OBJECTCREATEFAILED
**	DCM_LISTFAILURE
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

CONDITION
DCM_CreateObject(DCM_OBJECT ** object, unsigned long opt)
{
    PRIVATE_OBJECT
	* obj;

    if (object == NULL) {
	(void) COND_PushCondition(DCM_NULLADDRESS,
			  DCM_Message(DCM_NULLADDRESS), "DCM_CreateObject");
	return COND_PushCondition(DCM_OBJECTCREATEFAILED,
		   DCM_Message(DCM_OBJECTCREATEFAILED), "DCM_CreateObject");
    }
    obj = (PRIVATE_OBJECT *) CTN_MALLOC(sizeof(PRIVATE_OBJECT));
    if (obj == NULL) {
	(void) COND_PushCondition(DCM_MALLOCFAILURE,
		     DCM_Message(DCM_MALLOCFAILURE), sizeof(PRIVATE_OBJECT),
				  "DCM_CreateObject");
	*object = NULL;
	return COND_PushCondition(DCM_OBJECTCREATEFAILED,
		   DCM_Message(DCM_OBJECTCREATEFAILED), "DCM_CreateObject");
    }
    (void) memset(obj, 0, sizeof(PRIVATE_OBJECT));
    (void) strcpy(obj->keyType, KEY_DCM_OBJECT);


    obj->objectType = DCM_OBJECTUNKNOWN;
    obj->accessMethod = DCM_MEMORY_ACCESS;
    obj->deleteFlag = FALSE;
    if ((opt & DCM_GROUPLENGTHMASK) == DCM_NOGROUPLENGTH)
	obj->groupLengthFlag = FALSE;
    else
	obj->groupLengthFlag = TRUE;
    obj->objectSize = 0;
    obj->offset = 0;
    obj->pixelSize = 0;
    obj->pixelOffset = 0;
    obj->pixelBitsAllocated = 0;
    obj->pixelRepresentation = 0xffff;
    obj->groupCtx = NULL;
    obj->elementCtx = NULL;
    obj->fd = -1;
    obj->fileName[0] = '\0';
    obj->preambleFlag = FALSE;
    obj->preamble[0] = '\0';
    obj->dataOptions = 0;
    obj->metaHeaderLength = 0xffffffff;
    obj->longVRAttributes = 0;
    obj->waveformDataVR[0] = '\0';

    obj->groupList = LST_Create();
    if (obj->groupList == NULL) {
	CTN_FREE(obj);
	*object = NULL;
	return COND_PushCondition(DCM_LISTFAILURE,
				  DCM_Message(DCM_LISTFAILURE),
				  "DCM_CreateObject");
    }
    *object = (DCM_OBJECT *) obj;
    return DCM_NORMAL;
}


/* DCM_CloseObject
**
** Purpose:
**	Close an information object by freeing memory allocated to it and
**	destroying caller's reference to it.
**
** Parameter Dictionary:
**	callerObject	Address of caller's pointer to a DCM_OBJECT.
**
** Return Values:
**
**	DCM_FILEDELETEFAILED
**	DCM_ILLEGALOBJECT
**	DCM_LISTFAILURE
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Algorithm:
*/

CONDITION
DCM_CloseObject(DCM_OBJECT ** callerObject)
{
    CONDITION
	cond;
    PRV_GROUP_ITEM
	* group;
    PRV_ELEMENT_ITEM
	* element;
    PRIVATE_OBJECT
	** object;
    DCM_SEQUENCE_ITEM
	* sequenceItem;
    DCM_FRAGMENT_ITEM* fragmentItem;

    if (debug)
	fprintf(stderr, "Starting DCM_CloseObject\n");

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_CloseObject");
    if (cond != DCM_NORMAL)
	return cond;

    if ((*object)->fd > 0)
	(void) close((*object)->fd);

    if (debug)
	fprintf(stderr, "DCM_CloseObject: Legal object and file closed\n");

    while ((group = (void *)LST_Pop(&(*object)->groupList)) != NULL) {
	if (debug)
	    fprintf(stderr, "DCM_CloseObject: group %04x\n", group->group);

	while ((element = (void *)LST_Pop(&group->elementList)) != NULL) {
	    if (debug)
		fprintf(stderr, "DCM_CloseObject: Element %08x\n",
			element->element.tag);
	    if (element->element.representation == DCM_SQ) {
		if (debug)
		    fprintf(stderr, "Sequence List Address: %p\n",
			    element->element.d.sq);
		if (element->element.d.sq != NULL) {
		    while ((sequenceItem = (void *)LST_Pop(&element->element.d.sq)) != NULL) {
			(void) DCM_CloseObject(&sequenceItem->object);
			CTN_FREE(sequenceItem);
		    }
		    (void) LST_Destroy(&element->element.d.sq);
		}
	    } else if (element->fragmentFlag) {
		if (debug)
		    fprintf(stderr, "Fragment List Address: %p\n",
			    element->element.d.fragments);
		if (element->element.d.fragments != NULL) {
		    while ((fragmentItem = (void *)LST_Pop(&element->element.d.fragments)) != NULL) {
			CTN_FREE(fragmentItem);
		    }
		    (void) LST_Destroy(&element->element.d.fragments);
		}
	    }
	    if (debug)
		fprintf(stderr, "DCM_CloseObject: free %8p\n", element);

	    CTN_FREE(element);
	}
	cond = LST_Destroy(&group->elementList);
	if (cond != LST_NORMAL)
	    return COND_PushCondition(DCM_LISTFAILURE,
			   DCM_Message(DCM_LISTFAILURE), "DCM_CloseObject");
	CTN_FREE(group);
    }
    cond = LST_Destroy(&(*object)->groupList);
    if (cond != LST_NORMAL)
	return COND_PushCondition(DCM_LISTFAILURE,
			   DCM_Message(DCM_LISTFAILURE), "DCM_CloseObject");

    cond = DCM_NORMAL;
    if ((*object)->deleteFlag) {
	if (unlink((*object)->fileName) != 0) {
/****    (void) COND_PushCondition(DCM_FILEDELETEFAILED, strerror(errno));****/
	    cond = COND_PushCondition(DCM_FILEDELETEFAILED,
				      DCM_Message(DCM_FILEDELETEFAILED), (*object)->fileName, strerror(errno),
				      "DCM_CloseObject");

	}
    }
    CTN_FREE(*object);
    *object = NULL;
    return cond;
}

/* DCM_AddElement
**
** Purpose:
**	Add an element to an existing DCM object
**
** Parameter Dictionary:
**	object		Pointer to caller's existing DCM object.
**	element		Pointer to DCM element to be added to object
**
** Return Values:
**
**	DCM_ILLEGALOBJECT
**	DCM_ILLEGALREPRESENTATION
**	DCM_INSERTFAILED
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Algorithm:
**	Check caller's object to make certain it is a legal object
**	Check element to see that caller is not trying to add
**	    group length or length to end data elements.
**	Lookup element in the dictionary
**	If element is not in the dictionary, use caller's definition
**	If element is in the dictionary, make certain caller used
**	    proper definitions or left things undefined.
**	Call findCreateGroup to
**	    - create new group if this group does not exist
**	    - create length to end element if necessary
**	    - update object size for this object
**	    - set CURRENT pointer in linked list to head of this group
**	Call insertNewElement to
**	    - create a copy of the caller's element
**	    - insert copy into linked list
**	Call updateObjectType to
**	    - update this object as type COMMAND, DATASET, MESSAGE
*/

CONDITION
DCM_AddElement(DCM_OBJECT ** callerObject, DCM_ELEMENT * element)
{
    CONDITION
	cond;
    DCM_ELEMENT
	localElement;
    PRIVATE_OBJECT
	** object;
    PRV_GROUP_ITEM
	* groupItem;

    object = (PRIVATE_OBJECT **) callerObject;

    cond = checkObject(object, "DCM_AddElement");
    if (cond != DCM_NORMAL)
	return cond;

    if ((DCM_TAG_ELEMENT(element->tag) == 0x0000))
	return COND_PushCondition(DCM_ILLEGALADD,
		   DCM_Message(DCM_ILLEGALADD), DCM_TAG_GROUP(element->tag),
			   DCM_TAG_ELEMENT(element->tag), "DCM_AddElement");


    localElement = *element;

    cond = DCM_LookupElement(&localElement);
    if (cond != DCM_NORMAL) {
	(void) COND_PopCondition(0);
	localElement = *element;
    } else {
	if (localElement.representation == DCM_OT ||
	    localElement.representation == DCM_CTX)
	    localElement.representation = element->representation;
	if (element->representation != DCM_UN &&
	    element->representation != localElement.representation) {
	    return COND_PushCondition(DCM_ILLEGALREPRESENTATION,
				      DCM_Message(DCM_ILLEGALREPRESENTATION),
				      DCM_TAG_GROUP(element->tag),
				      DCM_TAG_ELEMENT(element->tag),
				      "DCM_AddElement");
	}
    }

    cond = findCreateGroup(object, DCM_TAG_GROUP(localElement.tag), &groupItem);
    if (cond != DCM_NORMAL)
	return COND_PushCondition(DCM_INSERTFAILED,
		 DCM_Message(DCM_INSERTFAILED), DCM_TAG_GROUP(element->tag),
				  DCM_TAG_ELEMENT(element->tag),
				  "DCM_AddElement");

    cond = insertNewElement(object, &localElement);
    if (cond != DCM_NORMAL)
	return COND_PushCondition(DCM_INSERTFAILED,
		 DCM_Message(DCM_INSERTFAILED), DCM_TAG_GROUP(element->tag),
				  DCM_TAG_ELEMENT(element->tag),
				  "DCM_AddElement");

    cond = updateObjectType(object, &localElement);
    if (cond != DCM_NORMAL)
	return COND_PushCondition(DCM_INSERTFAILED,
		 DCM_Message(DCM_INSERTFAILED), DCM_TAG_GROUP(element->tag),
				  DCM_TAG_ELEMENT(element->tag),
				  "DCM_AddElement");

    return DCM_NORMAL;
}

/* DCM_AddSequenceElement
**
** Purpose:
**	Add a sequence element to an existing DCM object.  This
**	function takes ownership of the caller's sequence list
**	when it adds the element to the object.  The caller's
**	copy of the sequence list is removed.
**
** Parameter Dictionary:
**	object		Pointer to caller's existing DCM object.
**	element		Pointer to DCM element to be added to object
**
** Return Values:
**
**	DCM_ILLEGALOBJECT
**	DCM_ILLEGALREPRESENTATION
**	DCM_INSERTFAILED
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Algorithm:
*/

CONDITION
DCM_AddSequenceElement(DCM_OBJECT ** callerObject, DCM_ELEMENT * element)
{
    CONDITION cond;
    DCM_ELEMENT localElement;
    PRIVATE_OBJECT **object;
    PRV_GROUP_ITEM *groupItem;

    object = (PRIVATE_OBJECT **) callerObject;

    cond = checkObject(object, "DCM_AddSequenceElement");
    if (cond != DCM_NORMAL)
	return cond;

    if ((DCM_TAG_ELEMENT(element->tag) == 0x0000))
	return COND_PushCondition(DCM_ILLEGALADD,
		   DCM_Message(DCM_ILLEGALADD), DCM_TAG_GROUP(element->tag),
			   DCM_TAG_ELEMENT(element->tag), "DCM_AddElement");


    localElement = *element;

    cond = DCM_LookupElement(&localElement);
    if (cond != DCM_NORMAL) {
	(void) COND_PopCondition(0);
	localElement = *element;
    } else {
	localElement.representation = element->representation;
    }
    if (localElement.representation != DCM_SQ) {
	return COND_PushCondition(DCM_NOTASEQUENCE,
				  DCM_Message(DCM_NOTASEQUENCE),
				  DCM_TAG_GROUP(localElement.tag),
				  DCM_TAG_ELEMENT(localElement.tag),
				  "DCM_AddSequenceElement");
    }
    cond = findCreateGroup(object, DCM_TAG_GROUP(localElement.tag), &groupItem);
    if (cond != DCM_NORMAL)
	return COND_PushCondition(DCM_INSERTFAILED,
		 DCM_Message(DCM_INSERTFAILED), DCM_TAG_GROUP(element->tag),
				  DCM_TAG_ELEMENT(element->tag),
				  "DCM_AddSequenceElement");

    cond = insertNewElement(object, &localElement);
    if (cond != DCM_NORMAL)
	return COND_PushCondition(DCM_INSERTFAILED,
		 DCM_Message(DCM_INSERTFAILED), DCM_TAG_GROUP(element->tag),
				  DCM_TAG_ELEMENT(element->tag),
				  "DCM_AddElement");

    cond = updateObjectType(object, &localElement);
    if (cond != DCM_NORMAL)
	return COND_PushCondition(DCM_INSERTFAILED,
		 DCM_Message(DCM_INSERTFAILED), DCM_TAG_GROUP(element->tag),
				  DCM_TAG_ELEMENT(element->tag),
				  "DCM_AddSequenceElement");

    /*
     * We have taken ownership of the sequence list, so zero out caller's
     * copy
     */
    element->d.sq = NULL;

    return DCM_NORMAL;
}

/* DCM_RemoveElement
**
** Purpose:
**	This function removes a single element from an information object.
**
** Parameter Dictionary:
**	callerObject		Handle to the object
**	tag			The tag of the element to be removed
**
** Return Values:
**
**	DCM_ELEMENTNOTFOUND
**	DCM_ILLEGALOBJECT
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

CONDITION
DCM_RemoveElement(DCM_OBJECT ** callerObject, DCM_TAG tag)
{
    PRIVATE_OBJECT
	** object;
    PRV_GROUP_ITEM
	* groupItem;
    PRV_ELEMENT_ITEM
	* elementItem,
	*groupLengthItem;
    CONDITION
	cond;
    CTNBOOLEAN
	flag;
    unsigned short
        group,
        element;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_RemoveElement");
    if (cond != DCM_NORMAL)
	return cond;

    group = DCM_TAG_GROUP(tag);
    element = DCM_TAG_ELEMENT(tag);

    groupItem = (void *)LST_Head(&((*object)->groupList));
    if (groupItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
			   DCM_Message(DCM_ELEMENTNOTFOUND), group, element,
				  "DCM_RemoveElement");

    (void) LST_Position(&((*object)->groupList), (void *)groupItem);

    flag = FALSE;
    while ((groupItem != NULL) && (flag == FALSE)) {
	if (groupItem->group == group)
	    flag = TRUE;
	else
	    groupItem = (void *)LST_Next(&(*object)->groupList);
    }
    if (flag == FALSE)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
			   DCM_Message(DCM_ELEMENTNOTFOUND), group, element,
				  "DCM_RemoveElement");

    elementItem = (void *)LST_Head(&groupItem->elementList);
    if (elementItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
			   DCM_Message(DCM_ELEMENTNOTFOUND), group, element,
				  "DCM_RemoveElement");

    (void) LST_Position(&groupItem->elementList, (void *)elementItem);

    groupLengthItem = elementItem;
    if (DCM_TAG_ELEMENT(groupLengthItem->element.tag) != 0x0000)
	groupLengthItem = NULL;


    flag = FALSE;
    while ((elementItem != NULL) && (flag == FALSE)) {
	if (DCM_TAG_ELEMENT(elementItem->element.tag) == element)
	    flag = TRUE;
	else
	    elementItem = (void *)LST_Next(&groupItem->elementList);
    }

    if (flag == FALSE)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
			   DCM_Message(DCM_ELEMENTNOTFOUND), group, element,
				  "DCM_RemoveElement");

    if (groupItem->baseLength != DCM_UNSPECIFIEDLENGTH) {
	groupItem->baseLength -= elementItem->paddedDataLength + 2 + 2 + 4;
	if (groupLengthItem != NULL) {
	    *groupLengthItem->element.d.ul = groupItem->baseLength;
	}
    }
    if ((*object)->objectSize != DCM_UNSPECIFIEDLENGTH)
	(*object)->objectSize -= elementItem->paddedDataLength + 2 + 2 + 4;
    if (elementItem->element.representation == DCM_OW ||
	elementItem->element.representation == DCM_OB ||
	elementItem->element.representation == DCM_SQ) {
	groupItem->longVRAttributes--;
	(*object)->longVRAttributes--;
    }
    (void) LST_Remove(&(groupItem->elementList), LST_K_AFTER);
    CTN_FREE(elementItem);
    return DCM_NORMAL;
}

/* DCM_GetElementValue
**
** Purpose:
**	This function retrieves the data from one data element and
**	returns it in a buffer allocated by the caller.  In the event
**	the data is larger than the caller's buffer, multiple calls
**	are used to retrieve the data.
**
** Parameter Dictionary:
**	object		Pointer to user's object containing desired element
**	element		DCM_ELEMENT structure containing (group,element)
**			specification of desired data element
**	rtnLength	Pointer to caller variable to hold length of
**			data returned by this call.
**	ctx		Pointer to context variable used for multiple
**			calls to this function.  Caller should set the
**			pointer to NULL before the first call and not
**			touch the pointer again.
**
** Return Values:
**
**	DCM_CANNOTGETSEQUENCEVALUE
**	DCM_ELEMENTNOTFOUND
**	DCM_GETINCOMPLETE
**	DCM_ILLEGALCONTEXT
**	DCM_ILLEGALOBJECT
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Algorithm:
**	Check caller's object to make certain it is a legal DCM object.
**	Find head of the object linked list.
**	Search linked list sequentially until object is found or end
**	of list reached.
**	If end of list
**		return DCM_ELEMENTNOTFOUND
**	If CTX pointer containts NULL
**	    Begin copy from beginning of element
**	else
**	    Begin copy from address in CTX pointer
**	Copy data from element data area to user buffer
**	If copy is incomplete (remaining data longer than caller's buffer)
**	    Update CTX pointer to point to next uncopied part of data
**	    Return DCM_GETINCOMPLETE
**	else
**	    Update CTX pointer to point past data area.
**	    Return DCM_NORMAL
*/

CONDITION
DCM_GetElementValue(DCM_OBJECT ** callerObject, DCM_ELEMENT * element,
		    U32 * rtnLength, void **ctx)
{
    PRIVATE_OBJECT
	** object;
    PRV_GROUP_ITEM
	* groupItem;
    PRV_ELEMENT_ITEM
	* elementItem;
    int
        nBytes;
    CONDITION
	cond;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_GetElementValue");
    if (cond != DCM_NORMAL)
	return cond;

    groupItem = (void *)LST_Head(&(*object)->groupList);
    if (groupItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
	      DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(element->tag),
				  DCM_TAG_ELEMENT(element->tag),
				  "DCM_GetElementValue");

    (void) LST_Position(&(*object)->groupList, (void *)groupItem);
    while (groupItem != NULL) {
	if (groupItem->group == DCM_TAG_GROUP(element->tag))
	    break;

	groupItem = (void *)LST_Next(&(*object)->groupList);
    }
    if (groupItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
	      DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(element->tag),
				  DCM_TAG_ELEMENT(element->tag),
				  "DCM_GetElementValue");

    elementItem = (void *)LST_Head(&groupItem->elementList);
    if (elementItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
	      DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(element->tag),
				  DCM_TAG_GROUP(element->tag),
				  "DCM_GetElementValue");

    (void) LST_Position(&groupItem->elementList, (void *)elementItem);
    while (elementItem != NULL) {
	if (elementItem->element.tag == element->tag) {
	    unsigned char *p;
	    U32 l;

	    if (element->representation == DCM_SQ)
		return COND_PushCondition(DCM_CANNOTGETSEQUENCEVALUE,
				    DCM_Message(DCM_CANNOTGETSEQUENCEVALUE),
				       element->tag, "DCM_GetElementValue");

	    p = *ctx;
	    if ((U32)PTOI(p) > elementItem->element.length)
		return COND_PushCondition(DCM_ILLEGALCONTEXT,
					  DCM_Message(DCM_ILLEGALCONTEXT),
					  "DCM_GetElementValue");

	    l = MIN(element->length, (elementItem->element.length - (U32)PTOI(p)));

	    *rtnLength = l;
	    {
		if (elementItem->element.d.ot == NULL) {
		    if ((*object)->fd != -1) {
			(void) lseek((*object)->fd,
			     elementItem->dataOffset + (off_t) p, SEEK_SET);
			nBytes = read((*object)->fd, element->d.ot, (int) l);
		    } else {
			(*object)->sk((*object)->userCtx,
				      (long) (elementItem->dataOffset + (off_t) p), SEEK_SET);
			cond = (*object)->rd((*object)->userCtx, element->d.ot, l,
					     &nBytes);
		    }
		    if ((unsigned) nBytes != l) {
			return COND_PushCondition(DCM_FILEACCESSERROR,
					   DCM_Message(DCM_FILEACCESSERROR),
						  (*object)->fileName,
						  "DCM_GetElementValue");
		    }
		    if( LITTLE_ENDIAN_ARCHITECTURE ){
		      if (elementItem->element.representation == DCM_AT) {
			  DCM_ELEMENT e;
			  e = elementItem->element;
			  e.length = l;
			  e.d.ot = element->d.ot;
			  swapATGroupElement(&e);
		      }
                    }
		    if (elementItem->byteOrder == BYTEORDER_REVERSE) {
			DCM_ELEMENT e;
			e = elementItem->element;
			e.length = l;
			e.d.ot = element->d.ot;
			swapInPlace(object, &e);
		    }
		} else {
		    unsigned char *q;
		    q = (unsigned char *) elementItem->element.d.ot + (U32)PTOI(p);
		    (void) memcpy(element->d.ot, q, l);
		    if (elementItem->byteOrder == BYTEORDER_REVERSE) {
			DCM_ELEMENT e;
			e = elementItem->element;
			e.length = l;
			e.d.ot = element->d.ot;
			swapInPlace(object, &e);
		    }
		}
		p += l;
		*ctx = (void *) p;
		if ((unsigned)PTOI(p) == elementItem->element.length)
		    return DCM_NORMAL;
		else
		    return DCM_GETINCOMPLETE;
	    }

	}
	elementItem = (void *)LST_Next(&groupItem->elementList);
    }
    return COND_PushCondition(DCM_ELEMENTNOTFOUND,
	      DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(element->tag),
			      DCM_TAG_ELEMENT(element->tag),
			      "DCM_GetElementValue");
}

char*
DCM_GetString(DCM_OBJECT** callerObject, DCM_TAG tag)
{
  DCM_ELEMENT e;
  CONDITION cond;
  char* s;
  char tmp[64] = "";
  char b[64] = "";

  e.tag = tag;
  cond = DCM_GetElement(callerObject, tag, &e);
  if (cond != DCM_NORMAL) {
    COND_PopCondition(TRUE);
    return 0;
  }

  if (DCM_IsString(e.representation)) {
    s = AFMALL( char, e.length + 1);
    e.d.string = s;
    cond = DCM_ParseObject(callerObject, &e, 1, 0, 0, 0);
    if (cond != DCM_NORMAL) {
      free(s);
      s = 0;
    }
    return s;
  }

  if (e.representation == DCM_SQ) {
    return 0;
  }

  if (e.length > sizeof(b))
    return 0;

  e.d.ot = b;
  cond = DCM_ParseObject(callerObject, &e, 1, 0, 0, 0);
  if (cond != DCM_NORMAL) {
    COND_PopCondition(TRUE);
    return 0;
  }

  switch (e.representation) {
    case DCM_AT:
    case DCM_FD:
    case DCM_FL:
      strcpy(tmp, "<Unimplemented>");
      break;
    case DCM_SL:
      sprintf(tmp, "%d", *e.d.sl);
      break;
    case DCM_SQ:
      strcpy(tmp, "<Unimplemented>");
      break;
    case DCM_SS:
      sprintf(tmp, "%d", *e.d.ss);
      break;
    case DCM_UL:
      sprintf(tmp, "%d", *e.d.ul);
      break;
    case DCM_UN:
      strcpy(tmp, "<Unimplemented>");
      break;
    case DCM_US:
      sprintf(tmp, "%d", *e.d.us);
      break;
    /*case DCM_UNKNOWN:*/
    case DCM_RET:
    case DCM_CTX:
    case DCM_OB:
    case DCM_OW:
    case DCM_DLM:
    default:
      strcpy(tmp, "<Unimplemented>");
      break;
  }

  s = (char*) malloc(strlen(tmp) + 1);
  strcpy(s, tmp);

  return s;
}



CONDITION
DCM_GetElementValueOffset(DCM_OBJECT ** callerObject, DCM_ELEMENT * element,
			  unsigned long offset)
{
    PRIVATE_OBJECT **object;
    PRV_ELEMENT_ITEM *elementItem;
    int nBytes;
    CONDITION cond;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_GetElementValue");
    if (cond != DCM_NORMAL)
	return cond;

    elementItem = locateElement(object, element->tag);
    if (elementItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
	      DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(element->tag),
				  DCM_TAG_ELEMENT(element->tag),
				  "DCM_GetElementValueOffset");


    {
	unsigned char *p;
	U32 l;

	if (element->representation == DCM_SQ)
	    return COND_PushCondition(DCM_CANNOTGETSEQUENCEVALUE,
				    DCM_Message(DCM_CANNOTGETSEQUENCEVALUE),
				 element->tag, "DCM_GetElementValueOffset");

	p = (unsigned char *) offset;;
	if ((U32)PTOI(p) > elementItem->element.length)
	    return COND_PushCondition(DCM_BADOFFSET,
				      DCM_Message(DCM_BADOFFSET),
				      (int) offset,
				      (int) elementItem->element.length,
				      "DCM_GetElementValueLength");

	l = element->length;
	if (l + offset > elementItem->element.length) {
	    return COND_PushCondition(DCM_BADLENGTH,
				      DCM_Message(DCM_BADLENGTH),
				      (int) offset, (int) l,
				      (int) elementItem->element.length,
				      "DCM_GetElementValueLength");
	} {
	    if (elementItem->element.d.ot == NULL) {
		if ((*object)->fd != -1) {
		    (void) lseek((*object)->fd,
			     elementItem->dataOffset + (off_t) p, SEEK_SET);
		    nBytes = read((*object)->fd, element->d.ot, (int) l);
		} else {
		    (*object)->sk((*object)->userCtx,
		    (long) (elementItem->dataOffset + (off_t) p), SEEK_SET);
		    cond = (*object)->rd((*object)->userCtx, element->d.ot, l,
					 &nBytes);
		}
		if ((unsigned) nBytes != l) {
		    return COND_PushCondition(DCM_FILEACCESSERROR,
					   DCM_Message(DCM_FILEACCESSERROR),
					      (*object)->fileName,
					      "DCM_GetElementValueValue");
		}
                if( LITTLE_ENDIAN_ARCHITECTURE ){
		  if (elementItem->element.representation == DCM_AT) {
		      DCM_ELEMENT e;
		      e = elementItem->element;
		      e.length = l;
		      e.d.ot = element->d.ot;
		      swapATGroupElement(&e);
		  }
                }
		if (elementItem->byteOrder == BYTEORDER_REVERSE) {
		    DCM_ELEMENT e;
		    e = elementItem->element;
		    e.length = l;
		    e.d.ot = element->d.ot;
		    swapInPlace(object, &e);
		}
	    } else {
		unsigned char *q;
		q = (unsigned char *) elementItem->element.d.ot + (U32)PTOI(p);
		(void) memcpy(element->d.ot, q, l);
		if (elementItem->byteOrder == BYTEORDER_REVERSE) {
		    DCM_ELEMENT e;
		    e = elementItem->element;
		    e.length = l;
		    e.d.ot = element->d.ot;
		    swapInPlace(object, &e);
		}
	    }
	    return DCM_NORMAL;
	}

    }
}



/* DCM_GetElementSize
**
** Purpose:
**	Return the size of one data element in an ACR object.
**
** Parameter Dictionary:
**	object		Pointer to caller's ACR object
**	element		Pointer to ACR element that defines data element
**			of interest by specifying (group,element) pair
**	rtnLength	Pointer to caller variable to hold returned
**			length of data element
**
** Return Values:
**
**	DCM_NORMAL
**	DCM_NULLOBJECT
**	DCM_ILLEGALOBJECT
**	DCM_ELEMENTNOTFOUND
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

CONDITION
DCM_GetElementSize(DCM_OBJECT ** callerObject, DCM_TAG tag,
		   U32 * rtnLength)
{
    PRIVATE_OBJECT
	** object;
    PRV_GROUP_ITEM
	* groupItem;
    PRV_ELEMENT_ITEM
	* elementItem;
    CONDITION
	cond;
    CTNBOOLEAN
	flag;
    unsigned short
        group,
        element;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_GetElementSize");
    if (cond != DCM_NORMAL)
	return cond;

    group = DCM_TAG_GROUP(tag);
    element = DCM_TAG_ELEMENT(tag);

    groupItem = (void *)LST_Head(&((*object)->groupList));
    if (groupItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
			   DCM_Message(DCM_ELEMENTNOTFOUND), group, element,
				  "DCM_GetElementSize");

    (void) LST_Position(&((*object)->groupList), (void *)groupItem);

    flag = FALSE;
    while ((groupItem != NULL) && (flag == FALSE)) {
	if (groupItem->group == group)
	    flag = TRUE;
	else
	    groupItem = (void *)LST_Next(&(*object)->groupList);
    }
    if (flag == FALSE)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
			   DCM_Message(DCM_ELEMENTNOTFOUND), group, element,
				  "DCM_GetElementSize");

    elementItem = (void *)LST_Head(&groupItem->elementList);
    if (elementItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
			   DCM_Message(DCM_ELEMENTNOTFOUND), group, element,
				  "DCM_GetElementSize");

    (void) LST_Position(&groupItem->elementList, (void *)elementItem);

    flag = FALSE;
    while ((elementItem != NULL) && (flag == FALSE)) {
	if (elementItem->element.tag == tag)
	    flag = TRUE;
	else
	    elementItem = (void *)LST_Next(&groupItem->elementList);
    }

    if (flag == FALSE)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
			   DCM_Message(DCM_ELEMENTNOTFOUND), group, element,
				  "DCM_GetElementSize");


    *rtnLength = elementItem->element.length;
    return DCM_NORMAL;
}


/* DCM_ScanParseObject
**
** Purpose:
**	DCM_ScanParseObject is used to allow a caller to examine every
**	element in a DICOM object and to parse the elements in the object.
**	The caller passes a list of elements to be parsed from the object.
**	This function examines each element in the object in order
**	(ascending group/element).  If the element in the object is found
**	in the caller's parse list, the element is parsed (and a value
**	placed in storage allocated by the caller).  If the element is
**	not found in the caller's list, a callback function is invoked
**	to notify the caller of the element.  When the callback function
**	is invoked, the arguments are:
**		DCM_ELEMENT *e	Pointer to the individual element
**		void *ctx	Caller's context information
**
**	This function is very useful for determining exactly which
**	elements are present in an object without having to ask for
**	each one individually.
**
** Parameter Dictionary:
**	callerObject	Pointer to caller's DICOM object
**	buf		Unused
**	bufferSizd	Unused
**	vector		A vector of elements which are to be parsed.  An entry
**			in the vector gives the tag and describes where the
**			parsed data is to be stored.
**	vectorLength	Number of entries in the vector.
**	callback	Caller function invoked for an element that is in
**			the object but is not found in caller's list.
**	ctx		Context information that is passed to callback function.
**
** Return Values:
**
**	DCM_NORMAL
**	DCM_NULLOBJECT
**	DCM_ILLEGALOBJECT
**
** Algorithm:
*/

CONDITION
DCM_ScanParseObject(DCM_OBJECT ** callerObject, void *buf, size_t bufferSize,
		    DCM_FLAGGED_ELEMENT * elementVector, int vectorLength,
		    CONDITION(*callback) (const DCM_ELEMENT* e, void* ctx),
		    void *ctx)
{
    PRIVATE_OBJECT
	** object;
    PRV_GROUP_ITEM
	* groupItem;
    PRV_ELEMENT_ITEM
	* elementItem;
    CONDITION
	cond;
    CTNBOOLEAN
	done = FALSE;
    DCM_ELEMENT
	e;
    int
        i;
    CTNBOOLEAN
	found;
    U32
	l=0;
    char
       *p;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_ScanParseObject");
    if (cond != DCM_NORMAL)
	return cond;

    groupItem = (void *)LST_Head(&((*object)->groupList));
    (void) LST_Position(&((*object)->groupList), (void *)groupItem);
    while (groupItem != NULL && !done) {
	elementItem = (void *)LST_Head(&groupItem->elementList);
	(void) LST_Position(&groupItem->elementList, (void *)elementItem);
	while (elementItem != NULL && !done) {
	    for (found = FALSE, i = 0; !found && i < vectorLength; i++) {
		if (elementItem->element.tag == elementVector[i].e.tag) {
		    found = TRUE;
		    (void)copyData(object,elementItem,&elementVector[i].e, &l);
		    *elementVector[i].flagAddress |= elementVector[i].flag;

		    if (DCM_IsString(elementVector[i].e.representation)) {
			elementVector[i].e.d.string[l] = '\0';
			p = elementVector[i].e.d.string + l - 1;
			while (p >= elementVector[i].e.d.string && (*p == ' '))
			    *p-- = '\0';
		    }
		}
	    }
	    if (!found) {
		e = elementItem->element;
		cond = callback(&e, ctx);
		if (cond != DCM_NORMAL)
		    done = TRUE;
	    }
	    elementItem = (void *)LST_Next(&groupItem->elementList);
	}
	groupItem = (void *)LST_Next(&((*object)->groupList));
    }
    return DCM_NORMAL;
}

/* DCM_ImportStream
**
** Purpose:
**	Import data from memory in DCM stream format and create
**	an internal memory representation of the object.
**
** Parameter Dictionary:
**	buf		Pointer to caller's buffer containing ACR NEMA data
**	length		Length of input data in bytes
**	opt		Bitmask giving options for interpreting data.
**			Legal values specify the order of the bytes in the data
**				ACR_ORDERNATIVE
**				ACR_ORDERLITTLEENDIAN
**				ACR_ORDERBIGENDIAN
**	object		Pointer to object created and returned by this function
**
** Return Values:
**
**	DCM_ELEMENTCREATEFAILED
**	DCM_ELEMENTLENGTHERROR
**	DCM_ELEMENTOUTOFORDER
**	DCM_FILEACCESSERROR
**	DCM_ILLEGALOPTION
**	DCM_ILLEGALSTREAMLENGTH
**	DCM_LISTFAILURE
**	DCM_NORMAL
**	DCM_OBJECTCREATEFAILED
**	DCM_UNEVENELEMENTLENGTH
**
** Algorithm:
**	call private import stream function which handles recursion
**
*/

CONDITION
DCM_ImportStream(unsigned char *buf, unsigned long length,
		 unsigned long opt, DCM_OBJECT ** callerObject)
{
#ifdef DEBUG
    if (debug)
	(void) fprintf(stderr, "DCM_ImportStream, %ld bytes\n", length);
#endif

    if ((opt & DCM_ORDERMASK) == 0)
	return COND_PushCondition(DCM_ILLEGALOPTION,
			       DCM_Message(DCM_ILLEGALOPTION), "Byte order",
				  "DCM_ImportStream");

    return readFile("", buf, -1, length, 0, 0, opt, callerObject, NULL, NULL,
		    NULL, NULL, NULL);
}

/* DCM_GetObjectSize
**
** Purpose:
**	Return the size of a DICOM object when it is represented in
**	stream format.
**
** Parameter Dictionary:
**	object		Pointer to caller's DICOM object
**	returnlength	Pointer to unsigned long variable to hold length of
**			object
**
** Return Values:
**
**	DCM_ILLEGALOBJECT
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

CONDITION
DCM_GetObjectSize(DCM_OBJECT ** callerObject, unsigned long *returnlength)
{
    PRIVATE_OBJECT
	** object;
    CONDITION
	cond;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_GetObjectSize");
    if (cond != DCM_NORMAL)
	return cond;

    *returnlength = (*object)->objectSize;
    return DCM_NORMAL;
}

/* DCM_DumpElements
**
** Purpose:
**	Dump a short description of each data element in an object to
**	stdout (for use as a debugging tool).
**
** Parameter Dictionary:
**	object		Pointer to caller's handle for DCM object to be dumped
**	vm		Limit on the value multiplicity for printing
**			binary data.  Print no more than vm values.
**
** Return Values:
**
**	DCM_ILLEGALOBJECT
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Algorithm:
**	Check caller's handle to make certain it is a legal DCM object
**	Print object type (COMMAND, DATASET, MESSAGE) and size in bytes
**	For each GROUP in the object linked list
**	    For each ELEMENT ITEM in the group linked list
**		print group, element, size, description
**		print some or all of data based on data element representation
**			(ASCII number, ASCII text, binary)
**	    End for
**	End for
*/

/* rcr - maybe do something better with this */
#include "siemens_dicom_csa.c"

static void dumpOB(unsigned char* c, long vm);

CONDITION
DCM_DumpElements(DCM_OBJECT ** callerObject, long vm)
{
    PRV_GROUP_ITEM
	* groupItem;
    PRV_ELEMENT_ITEM
	* elementItem;
    PRIVATE_OBJECT
	** object;
    CONDITION
	cond;
    DCM_SEQUENCE_ITEM
	* sq;
    char
        scratch[128];
    int
        stringLength;

ENTRY("DCM_DumpElements") ;

    object = (PRIVATE_OBJECT **) callerObject;

STATUS("calling checkObject") ;
    cond = checkObject(object, "DCM_DumpElements");
    if (cond != DCM_NORMAL){
STATUS("abnormal condition") ;
	RETURN(cond);
   }

    switch ((*object)->objectType) {
    case DCM_OBJECTUNKNOWN:
STATUS("objectType=UNKNOWN") ;
	RWC_printf("Object type: UNKNOWN\n");
	break;
    case DCM_OBJECTCOMMAND:
STATUS("objectType=COMMAND") ;
	RWC_printf("Object type: COMMAND\n");
	break;
    case DCM_OBJECTIMAGE:
STATUS("objectType=IMAGE") ;
	RWC_printf("Object type: IMAGE\n");
	break;
    case DCM_OBJECTELEMENTLIST:
STATUS("objectType=ELEMENTLIST") ;
	RWC_printf("Object type: ELEMENT LIST\n");
	break;
    default:
STATUS("objectType=Unknown") ;
	RWC_printf("Object type: Unknown (error)\n");
	break;
    }
    RWC_printf("Object size: %ld\n", (*object)->objectSize);

    groupItem = (void *)LST_Head(&(*object)->groupList);
    if (groupItem != NULL)
	(void) LST_Position(&(*object)->groupList, (void *)groupItem);

STATUS("looping over groupItem") ;
    while (groupItem != NULL) {
#ifdef MACOS
	RWC_printf("Group: %04x, Length: %8ld\n", groupItem->group,
	       groupItem->baseLength);
#else
	RWC_printf("Group: %04x, Length: %8d\n", groupItem->group,
	       groupItem->baseLength);
#endif
	elementItem = (void *)LST_Head(&groupItem->elementList);
	if (elementItem != NULL)
	    (void) LST_Position(&groupItem->elementList, (void *)elementItem);
	while (elementItem != NULL) {
#ifdef MACOS
	    (void) RWC_printf("%04x %04x ",
			  DCM_TAG_GROUP(elementItem->element.tag),
			  DCM_TAG_ELEMENT(elementItem->element.tag) );
            if (show_size_n_offset)
               (void) RWC_printf("%8ld [%-8lu] ",
			  elementItem->element.length ,
                          (unsigned long) elementItem->element.data_offset );
#else
	    (void) RWC_printf("%04x %04x ",
			  DCM_TAG_GROUP(elementItem->element.tag),
			  DCM_TAG_ELEMENT(elementItem->element.tag) );
            if (show_size_n_offset)
               (void) RWC_printf("%8d [%-8lu] ",
			  elementItem->element.length ,
                          (unsigned long) elementItem->element.data_offset );
#endif

            if( (rwc_opt & RWC_NONAME_MASK) == 0 )
	      (void) RWC_printf("//%31s//", elementItem->element.description);
            else
	      (void) RWC_printf("//") ;

	    if (elementItem->element.d.ot == NULL)
		(void) RWC_printf("Data on disk\n");
	    else {
		switch (elementItem->element.representation) {
		case DCM_AE:
		case DCM_AS:
		case DCM_CS:
		case DCM_DA:
		case DCM_DT:
		    stringLength = MIN(sizeof(scratch) - 1, elementItem->element.length);
		    strncpy(scratch, elementItem->element.d.string, stringLength);
		    scratch[stringLength] = '\0';
		    (void) RWC_printf("%s\n", scratch);
		    break;
		case DCM_DD:
		case DCM_FD:
		case DCM_FL:
		    (void) RWC_printf("Unimplemented\n");
		    break;
		case DCM_DS:
		case DCM_IS:
		case DCM_LO:
		case DCM_LT:
		case DCM_PN:
		case DCM_SH:
		case DCM_UT:
		    stringLength = MIN(sizeof(scratch) - 1, elementItem->element.length);
		    strncpy(scratch, elementItem->element.d.string, stringLength);
		    scratch[stringLength] = '\0';
		    (void) RWC_printf("%s\n", scratch);
		    break;
		case DCM_SL:
#ifdef MACOS
		    (void) RWC_printf("%8lx %ld\n", *elementItem->element.d.sl,
				  *elementItem->element.d.sl);
#else
                    if( (rwc_opt & RWC_NOHEX_MASK) == 0 )
		      (void) RWC_printf("%8x %d\n", *elementItem->element.d.sl,
				        *elementItem->element.d.sl);
                    else
		      (void) RWC_printf(" %d\n", *elementItem->element.d.sl ) ;

		    if (vm > 1)
			dumpBinaryData(elementItem->element.d.ot,
				       elementItem->element.representation,
			     elementItem->element.length / sizeof(U32), vm);
#endif
		    break;
		case DCM_SS:
                    if( (rwc_opt & RWC_NOHEX_MASK) == 0 )
		      (void) RWC_printf("%4x %d\n", *elementItem->element.d.ss,
				    *elementItem->element.d.ss);
                    else
		      (void) RWC_printf(" %d\n", *elementItem->element.d.ss ) ;

		    if (vm > 1)
			dumpBinaryData(elementItem->element.d.ot,
				       elementItem->element.representation,
			   elementItem->element.length / sizeof(short), vm);
		    break;
		case DCM_SQ:
		    (void) RWC_printf("SEQUENCE\n");
		    sq = (void *)LST_Head(&elementItem->element.d.sq);
		    if (sq != NULL)
			(void) LST_Position(&elementItem->element.d.sq, (void *)sq);
		    RWC_printf("DCM Dump SEQUENCE{\n");
		    while (sq != NULL) {
			(void) DCM_DumpElements(&sq->object, vm);
			sq = (void *)LST_Next(&elementItem->element.d.sq);
		    }
		    RWC_printf("DCM Dump SEQUENCE Complete}\n");
		    break;
		case DCM_ST:
		    stringLength = MIN(sizeof(scratch) - 1, elementItem->element.length);
		    strncpy(scratch, elementItem->element.d.string, stringLength);
		    scratch[stringLength] = '\0';
		    (void) RWC_printf("%s\n", scratch);
		    break;
		case DCM_TM:
		case DCM_UI:
		    stringLength = MIN(sizeof(scratch) - 1, elementItem->element.length);
		    strncpy(scratch, elementItem->element.d.string, stringLength);
		    scratch[stringLength] = '\0';
		    (void) RWC_printf("%s\n", scratch);
		    break;
		case DCM_AT:
		case DCM_UL:
#ifdef MACOS
		    (void) RWC_printf("%8lx %ld\n", *elementItem->element.d.ul,
				  *elementItem->element.d.ul);
#else
                    if( (rwc_opt & RWC_NOHEX_MASK) == 0 )
		      (void) RWC_printf("%8x %d\n", *elementItem->element.d.ul,
				    *elementItem->element.d.ul);
                    else
		      (void) RWC_printf(" %d\n", *elementItem->element.d.ul ) ;

		    if (vm > 1)
			dumpBinaryData(elementItem->element.d.ot,
				       elementItem->element.representation,
			     elementItem->element.length / sizeof(U32), vm);
#endif
		    break;
		case DCM_US:{
                    int nel = elementItem->element.length / sizeof(unsigned short) , rr ;
                    for( rr=0 ; rr < nel ; rr++ ){
                     if( (rwc_opt & RWC_NOHEX_MASK) == 0 )
		       (void) RWC_printf("%4x %d", elementItem->element.d.us[rr],
		          		           elementItem->element.d.us[rr]);
                     else
		       (void) RWC_printf(" %d", elementItem->element.d.us[rr] ) ;
                    }
                    RWC_printf("\n") ;

		    if (vm > 1)
			dumpBinaryData(elementItem->element.d.ot,
				       elementItem->element.representation,
				       elementItem->element.length / sizeof(unsigned short), vm);
                    }
		    break;
		case DCM_OB:
		case DCM_UN:
		    dumpBinaryData(elementItem->element.d.ot,
				       elementItem->element.representation,
			       elementItem->element.length , MAX(rwc_vm,8));

                    /* moved everything to new siemens_dicom_csa.c
                     *                          7 May 2011 [rickr] */
                    check_for_mosaic_slice_times(elementItem);

		    break;

		case DCM_OT:
		case DCM_OW:
		/*case DCM_UNKNOWN:*/
		case DCM_RET:
		    (void) RWC_printf("Unimplemented\n");
		    break;
		default:
		    (void) RWC_printf("Some unimplemented logic if here\n");
		    break;
		}
	    }
	    elementItem = (void *)LST_Next(&groupItem->elementList);
	}
	groupItem = (void *)LST_Next(&(*object)->groupList);
    }

    RWC_printf("DCM Dump Elements Complete\n");
    RETURN(DCM_NORMAL);
}

CONDITION
DCM_FormatElements(DCM_OBJECT ** callerObject, long vm, const char* prefix)
{
    PRV_GROUP_ITEM
	* groupItem;
    PRV_ELEMENT_ITEM
	* elementItem;
    PRIVATE_OBJECT
	** object;
    CONDITION
	cond;
    DCM_SEQUENCE_ITEM
	* sq;
    char
        scratch[128];
    int
        stringLength;
    char localPrefix[128];

    object = (PRIVATE_OBJECT **) callerObject;

    cond = checkObject(object, "DCM_DumpElements");
    if (cond != DCM_NORMAL)
	return cond;

    RWC_printf("\n%sDCM Dump Elements\n", prefix);
    switch ((*object)->objectType) {
    case DCM_OBJECTUNKNOWN:
	RWC_printf("%sObject type: UNKNOWN\n", prefix);
	break;
    case DCM_OBJECTCOMMAND:
	RWC_printf("%sObject type: COMMAND\n", prefix);
	break;
    case DCM_OBJECTIMAGE:
	RWC_printf("%sObject type: IMAGE\n", prefix);
	break;
    case DCM_OBJECTELEMENTLIST:
	RWC_printf("%sObject type: ELEMENT LIST\n", prefix);
	break;
    default:
	RWC_printf("%sObject type: Unknown (error)\n", prefix);
	break;
    }
    RWC_printf("%sObject size: %ld\n", prefix, (*object)->objectSize);

    groupItem = (void *)LST_Head(&(*object)->groupList);
    if (groupItem != NULL)
	(void) LST_Position(&(*object)->groupList, (void *)groupItem);

    while (groupItem != NULL) {
	RWC_printf("%sGroup: %04x, Length: %8d\n", prefix, groupItem->group,
	       groupItem->baseLength);
	elementItem = (void *)LST_Head(&groupItem->elementList);
	if (elementItem != NULL)
	    (void) LST_Position(&groupItem->elementList, (void *)elementItem);
	while (elementItem != NULL) {
	    (void) RWC_printf("%s%04x %04x %8d ",
			  prefix,
			  DCM_TAG_GROUP(elementItem->element.tag),
			  DCM_TAG_ELEMENT(elementItem->element.tag),
			  elementItem->element.length);
	    (void) RWC_printf("//%31s//", elementItem->element.description);
	    if (elementItem->element.d.ot == NULL)
		(void) RWC_printf("Data on disk\n");
	    else {
		switch (elementItem->element.representation) {
		case DCM_AE:
		case DCM_AS:
		case DCM_CS:
		case DCM_DA:
		case DCM_DT:
		    stringLength = MIN(sizeof(scratch) - 1, elementItem->element.length);
		    strncpy(scratch, elementItem->element.d.string, stringLength);
		    scratch[stringLength] = '\0';
		    (void) RWC_printf("%s\n", scratch);
		    break;
		case DCM_DD:
		case DCM_FD:
		case DCM_FL:
		    (void) RWC_printf("Unimplemented\n");
		    break;
		case DCM_DS:
		case DCM_IS:
		case DCM_LO:
		case DCM_LT:
		case DCM_PN:
		case DCM_SH:
		case DCM_UT:
		    stringLength = MIN(sizeof(scratch) - 1, elementItem->element.length);
		    strncpy(scratch, elementItem->element.d.string, stringLength);
		    scratch[stringLength] = '\0';
		    (void) RWC_printf("%s\n", scratch);
		    break;
		case DCM_SL:
#ifdef MACOS
		    (void) RWC_printf("%8lx %ld\n", *elementItem->element.d.sl,
				  *elementItem->element.d.sl);
#else
		    (void) RWC_printf("%8x %d\n", *elementItem->element.d.sl,
				  *elementItem->element.d.sl);
		    if (vm > 1)
			dumpBinaryData(elementItem->element.d.ot,
				       elementItem->element.representation,
			     elementItem->element.length / sizeof(U32), vm);
#endif
		    break;
		case DCM_SS:
		    (void) RWC_printf("%4x %d\n", *elementItem->element.d.ss,
				  *elementItem->element.d.ss);
		    if (vm > 1)
			dumpBinaryData(elementItem->element.d.ot,
				       elementItem->element.representation,
			   elementItem->element.length / sizeof(short), vm);
		    break;
		case DCM_SQ:
		    (void) RWC_printf("SEQUENCE\n");
		    sq = (void *)LST_Head(&elementItem->element.d.sq);
		    if (sq != NULL)
			(void) LST_Position(&elementItem->element.d.sq, (void *)sq);
		    RWC_printf("%sDCM Dump SEQUENCE{\n", prefix);
		    strcpy(localPrefix, prefix);
		    strcat(localPrefix, " ");
		    while (sq != NULL) {
			(void) DCM_FormatElements(&sq->object, vm, localPrefix);
			sq = (void *)LST_Next(&elementItem->element.d.sq);
		    }
		    RWC_printf("%sDCM Dump SEQUENCE Complete}\n", prefix);
		    break;
		case DCM_ST:
		    stringLength = MIN(sizeof(scratch) - 1, elementItem->element.length);
		    strncpy(scratch, elementItem->element.d.string, stringLength);
		    scratch[stringLength] = '\0';
		    (void) RWC_printf("%s\n", scratch);
		    break;
		case DCM_TM:
		case DCM_UI:
		    stringLength = MIN(sizeof(scratch) - 1, elementItem->element.length);
		    strncpy(scratch, elementItem->element.d.string, stringLength);
		    scratch[stringLength] = '\0';
		    (void) RWC_printf("%s\n", scratch);
		    break;
		case DCM_AT:
		case DCM_UL:
		    (void) RWC_printf("%8x %d\n", *elementItem->element.d.ul,
				  *elementItem->element.d.ul);
		    if (vm > 1)
			dumpBinaryData(elementItem->element.d.ot,
				       elementItem->element.representation,
			     elementItem->element.length / sizeof(U32), vm);
		    break;
		case DCM_US:
		    (void) RWC_printf("%4x %d\n", *elementItem->element.d.us,
				  *elementItem->element.d.us);
		    if (vm > 1)
			dumpBinaryData(elementItem->element.d.ot,
				       elementItem->element.representation,
				       elementItem->element.length / sizeof(unsigned short), vm);
		    break;
		case DCM_OT:
		case DCM_OW:
		case DCM_OB:
		/*case DCM_UNKNOWN:*/
		case DCM_RET:
		    (void) RWC_printf("Unimplemented\n");
		    break;
		default:
		    (void) RWC_printf("Some unimplemented logic if here\n");
		    break;
		}
	    }
	    elementItem = (void *)LST_Next(&groupItem->elementList);
	}
	groupItem = (void *)LST_Next(&(*object)->groupList);
    }

    RWC_printf("%sDCM Dump Elements Complete\n\n", prefix);
    return DCM_NORMAL;
}

/* DCM_Debug
**
** Purpose:
**	To enable the debugging facility
**
** Parameter Dictionary:
**	flag	CTNBOOLEAN variable TRUE if caller wants to turn on debugging
**		info; FALSE otherwise
**
** Return Values:
**	None
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

void
DCM_Debug(CTNBOOLEAN flag)
{
    debug = flag;
}

/* DCM_WriteFile
**
** Purpose:
**	Export an object from the internal representation and
**	write the stream representation to a file.
**
** Parameter Dictionary:
**	object		DCM_OBJECT which is to be written to the file
**	opt		Bitmask giving options for exporting data.  Legal
**			options give the byte order of exported data:
**				DCM_ORDERNATIVE
**				DCM_ORDERLITTLEENDIAN
**				DCM_ORDERBIGENDIAN
**	file		ASCIIZ name of the file to be created.
**
** Return Values:
**
**	DCM_FILEACCESSERROR
**	DCM_FILECREATEFAILED
**	DCM_ILLEGALOBJECT
**	DCM_LISTFAILURE
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

CONDITION
DCM_ExportStream(DCM_OBJECT ** callerObject, unsigned long opt,
		 void *buffer, unsigned long bufferlength,
		 DCM_EXPORT_STREAM_CALLBACK* callback,
		 void *ctx) ;

CONDITION
DCM_WriteFile(DCM_OBJECT ** callerObject, unsigned long opt, const char *file)
{
    PRIVATE_OBJECT
	** object;
    int
        fd;
    unsigned char
        buf[2048];
    CONDITION
	cond;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_WriteFile");
    if (cond != DCM_NORMAL)
	return cond;
#ifdef MACOS
    fd = open(file, O_WRONLY | O_CREAT | O_TRUNC);
#elif _MSC_VER
    fd = _open(file, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY,
	       _S_IREAD | _S_IWRITE);
#else
    fd = open(file, O_WRONLY | O_CREAT | O_TRUNC, 0666);
#endif
    if (fd < 0) {
	return COND_PushCondition(DCM_FILECREATEFAILED,
		   DCM_Message(DCM_FILECREATEFAILED), file, strerror(errno),
				  "DCM_WriteFile");
    }
    cond = DCM_ExportStream(callerObject, opt, buf,
			    (unsigned long) sizeof(buf), writeFile, &fd);
    if (cond != DCM_NORMAL)
	return cond;

    (void) close(fd);
    return DCM_NORMAL;
}

/* DCM_ModifyElements
**
** Purpose:
**
** Parameter Dictionary:
**	callerObject		Handle to user's DICOM object to be modified
**	vector			Mandatory elements that need to be stored
**				in the object
**	count			Number of such mandatory elements
**	flaggedVector		Optional elements
**	flaggedCount		Number of such optional elements
**	updateCount		Total number of elements updated (returned to
**				caller)
**
** Return Values:
**
**	DCM_ILLEGALOBJECT
**	DCM_ILLEGALREPRESENTATION
**	DCM_INSERTFAILED
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Algorithm:
**	Check caller's object to make certain it is a legal DCM object.
**	Find head of the object linked list.
**	Search linked list sequentially until object is found or end
**	of list reached.
**	If end of list
**		return DCM_ELEMENTNOTFOUND
**	If CTX pointer containts NULL
**	    Begin copy from beginning of element
**	else
**	    Begin copy from address in CTX pointer
**	Copy data from element data area to user buffer
**	If copy is incomplete (remaining data longer than caller's buffer)
**	    Update CTX pointer to point to next uncopied part of data
**	    Return DCM_GETINCOMPLETE
**	else
**	    Update CTX pointer to point past data area.
**	    Return DCM_NORMAL
*/

CONDITION
DCM_ModifyElements(DCM_OBJECT ** callerObject, DCM_ELEMENT * vector, int count,
		   DCM_FLAGGED_ELEMENT * flaggedVector, int flaggedCount,
		   int *updateCount)
{
    PRIVATE_OBJECT
	** object;
    CONDITION
	cond;
    DCM_ELEMENT
	e;
    int
        c = 0;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_ModifyElement");
    if (cond != DCM_NORMAL)
	return cond;

    while (count-- > 0) {
	cond = DCM_RemoveElement(callerObject, vector->tag);
	if (cond != DCM_NORMAL)
	    (void) COND_PopCondition(FALSE);

	e = *vector;
	if (DCM_IsString(e.representation))
	    e.length = strlen(e.d.string);

	cond = DCM_AddElement(callerObject, &e);
	if (cond != DCM_NORMAL)
	    return cond;

	c++;
	vector++;
    }

    while (flaggedCount-- > 0) {
	if ((*(flaggedVector->flagAddress) & flaggedVector->flag) != 0) {
	    cond = DCM_RemoveElement(callerObject, flaggedVector->e.tag);
	    if (cond != DCM_NORMAL)
		(void) COND_PopCondition(FALSE);

	    e = flaggedVector->e;
	    if (DCM_IsString(e.representation))
		e.length = strlen(e.d.string);
	    cond = DCM_AddElement(callerObject, &e);
	    if (cond != DCM_NORMAL)
		return cond;
	    c++;
	}
	flaggedVector++;
    }

    if (updateCount != NULL)
	*updateCount = c;
    return DCM_NORMAL;
}


/* DCM_ParseObject
**
** Purpose:
**	Parse the object and store the mandatory and optional elements in
**	different vectors.
**
** Parameter Dictionary:
**      callerObject            Handle to user's DICOM object to be modified
**      vector                  Mandatory elements that need to be stored
**                              in the object
**      count                   Number of such mandatory elements
**      flaggedVector           Optional elements
**      flaggedCount            Number of such optional elements
**      parseCount              Total number of elements parsed (returned to
**                              caller)
**
** Return Values:
**
**	DCM_CANNOTGETSEQUENCEVALUE
**	DCM_ELEMENTNOTFOUND
**	DCM_GETINCOMPLETE
**	DCM_ILLEGALCONTEXT
**	DCM_ILLEGALOBJECT
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/
CONDITION
DCM_ParseObject(DCM_OBJECT ** callerObject, DCM_ELEMENT * vector,
	      int count, DCM_FLAGGED_ELEMENT * flaggedVector, int flagCount,
		int *parseCount)
{
    PRIVATE_OBJECT
	** object;
    CONDITION
	cond;
    void
       *ctx;
    U32
	l;
    int
        c = 0;
    char
       *p;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_ParseObject");
    if (cond != DCM_NORMAL)
	return cond;

    while (count-- > 0) {
	ctx = NULL;
	cond = DCM_GetElementValue(callerObject, vector, &l, &ctx);
	if (cond != DCM_NORMAL)
	    return cond;
	if (DCM_IsString(vector->representation)) {
	    vector->d.string[l] = '\0';
	    p = vector->d.string + l - 1;
	    while (p >= vector->d.string && (*p == ' '))
		*p-- = '\0';
	}
	c++;
	vector++;
    }

    while (flagCount-- > 0) {
	ctx = NULL;
	cond = DCM_GetElementValue(callerObject, &flaggedVector->e, &l, &ctx);
	if (cond != DCM_NORMAL) {
	    (void) COND_PopCondition(FALSE);
	} else {
	    c++;
	    if (DCM_IsString(flaggedVector->e.representation)) {
		flaggedVector->e.d.string[l] = '\0';
		p = flaggedVector->e.d.string + l - 1;
		while (p >= flaggedVector->e.d.string && (*p == ' '))
		    *p-- = '\0';
	    }
	    *(flaggedVector->flagAddress) |= flaggedVector->flag;
	}
	flaggedVector++;
    }

    if (parseCount != NULL)
	*parseCount = c;
    return DCM_NORMAL;
}


/* DCM_RemoveGroup
**
** Purpose:
**	Remove an element with the given group number from the object
**
** Parameter Dictionary:
**	callerObject		Handle to caller's object
**	group			Group number of the element to be removed.
**
** Return Values:
**
**	DCM_GROUPNOTFOUND
**	DCM_ILLEGALOBJECT
**	DCM_LISTFAILURE
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

CONDITION
DCM_RemoveGroup(DCM_OBJECT ** callerObject, unsigned short group)
{
    PRIVATE_OBJECT
	** object;
    CONDITION
	cond;
    PRV_GROUP_ITEM
	* groupItem;
    PRV_ELEMENT_ITEM
	* elementItem;
    CTNBOOLEAN
	found = FALSE;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_RemoveGroup");
    if (cond != DCM_NORMAL)
	return cond;

    groupItem = (void *)LST_Head(&(*object)->groupList);
    if (groupItem == NULL)
	return COND_PushCondition(DCM_GROUPNOTFOUND,
	    DCM_Message(DCM_GROUPNOTFOUND), (int) group, "DCM_RemoveGroup");

    (void) LST_Position(&(*object)->groupList, (void *)groupItem);

    while (!found && (groupItem != NULL)) {
	if (groupItem->group == group)
	    found = TRUE;
	else
	    groupItem = (void *)LST_Next(&(*object)->groupList);
    }
    if (groupItem == NULL)
	return COND_PushCondition(DCM_GROUPNOTFOUND,
	    DCM_Message(DCM_GROUPNOTFOUND), (int) group, "DCM_RemoveGroup");


    while ((elementItem = (void *)LST_Pop(&groupItem->elementList)) != NULL)
	CTN_FREE(elementItem);

    groupItem = (void *)LST_Remove(&(*object)->groupList, LST_K_AFTER);
    cond = LST_Destroy(&groupItem->elementList);
    if (cond != LST_NORMAL)
	return COND_PushCondition(DCM_LISTFAILURE,
			   DCM_Message(DCM_LISTFAILURE), "DCM_RemoveGroup");
    CTN_FREE(groupItem);
    return DCM_NORMAL;
}

/* DCM_GetSequenceList
**
** Purpose:
**	Obtain the sequence list from the DICOM object corresponding to the
**	tag value.
**
** Parameter Dictionary:
**	object		Handle to the DICOM object
**	tag		Tag number of the sequence list element to be obtained
**			from the DICOM object
**	list		Holds the sequence list. Returned to the caller.
**
** Return Values:
**
**	DCM_ELEMENTNOTFOUND
**	DCM_ILLEGALOBJECT
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

CONDITION
DCM_GetSequenceList(DCM_OBJECT ** object, DCM_TAG tag, LST_HEAD ** list)
{
    PRIVATE_OBJECT
	** obj;
    CONDITION
	cond;
    PRV_GROUP_ITEM
	* groupItem;
    PRV_ELEMENT_ITEM
	* elementItem;
    CTNBOOLEAN
	found = FALSE;

    obj = (PRIVATE_OBJECT **) object;
    cond = checkObject(obj, "DCM_GetSequenceList");
    if (cond != DCM_NORMAL)
	return cond;

    groupItem = (void *)LST_Head(&(*obj)->groupList);
    if (groupItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
		       DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(tag),
				  DCM_TAG_ELEMENT(tag),
				  "DCM_GetSequenceList");

    (void) LST_Position(&(*obj)->groupList, (void *)groupItem);
    while (groupItem != NULL) {
	if (groupItem->group == DCM_TAG_GROUP(tag))
	    break;

	groupItem = (void *)LST_Next(&(*obj)->groupList);
    }
    if (groupItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
		       DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(tag),
				  DCM_TAG_ELEMENT(tag),
				  "DCM_GetSequenceList");

    elementItem = (void *)LST_Head(&groupItem->elementList);
    if (elementItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
		       DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(tag),
				  DCM_TAG_GROUP(tag),
				  "DCM_GetSequenceTag");

    (void) LST_Position(&groupItem->elementList, (void *)elementItem);
    while (!found && (elementItem != NULL)) {
	if (elementItem->element.tag == tag) {
	    *list = elementItem->element.d.sq;
	    found = TRUE;
	}
	elementItem = (void *)LST_Next(&groupItem->elementList);
    }
    if (found)
	return DCM_NORMAL;
    else
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
		       DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(tag),
				  DCM_TAG_ELEMENT(tag),
				  "DCM_GetSequenceList");
}

CONDITION
DCM_GetSequenceElement(DCM_OBJECT ** object, DCM_TAG top, DCM_ELEMENT * e)
{
    PRIVATE_OBJECT **obj;
    CONDITION cond;
    PRV_GROUP_ITEM *groupItem;
    PRV_ELEMENT_ITEM *elementItem;
    DCM_SEQUENCE_ITEM *seqItem;

    CTNBOOLEAN found = FALSE;

    obj = (PRIVATE_OBJECT **) object;
    cond = checkObject(obj, "DCM_GetSequenceElement");
    if (cond != DCM_NORMAL)
	return cond;

    elementItem = locateElement(obj, top);
    if (elementItem == NULL) {
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
				  DCM_Message(DCM_ELEMENTNOTFOUND),
				  DCM_TAG_GROUP(top),
				  DCM_TAG_ELEMENT(top),
				  "DCM_GetElementSequence");
    }
    if (elementItem->element.representation != DCM_SQ) {
	return COND_PushCondition(DCM_UNEXPECTEDREPRESENTATION,
				  DCM_Message(DCM_UNEXPECTEDREPRESENTATION),
				  "DCM_GetSequenceElement", "sequence");
    }
    seqItem = (void *)LST_Head(&elementItem->element.d.sq);
    cond = DCM_ParseObject(&seqItem->object, e, 1, NULL, 0, NULL);
    return cond;

#if 0
    return DCM_NORMAL;
#endif
}

/* DCM_GetElementValueList
**
** Purpose:
**
** Parameter Dictionary:
**	Define the parameters to the function
**
** Return Values:
**
**	DCM_ELEMENTNOTFOUND
**	DCM_ILLEGALOBJECT
**	DCM_LISTFAILURE
**	DCM_MALLOCFAILURE
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/
CONDITION
DCM_GetElementValueList(DCM_OBJECT ** object, DCM_TAG tag,
		  size_t structureSize, long stringOffset, LST_HEAD ** list)
{
    PRIVATE_OBJECT
	** obj;
    CONDITION
	cond;
    PRV_GROUP_ITEM
	* groupItem;
    PRV_ELEMENT_ITEM
	* elementItem;
    CTNBOOLEAN
	found = FALSE;
    char
       *src,
       *dst,
       *p;
    U32
	l;

    obj = (PRIVATE_OBJECT **) object;
    cond = checkObject(obj, "DCM_GetSequenceList");
    if (cond != DCM_NORMAL)
	return cond;

    groupItem = (void *)LST_Head(&(*obj)->groupList);
    if (groupItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
		       DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(tag),
				  DCM_TAG_ELEMENT(tag),
				  "DCM_GetSequenceList");

    (void) LST_Position(&(*obj)->groupList, (void *)groupItem);
    while (groupItem != NULL) {
	if (groupItem->group == DCM_TAG_GROUP(tag))
	    break;

	groupItem = (void *)LST_Next(&(*obj)->groupList);
    }
    if (groupItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
		       DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(tag),
				  DCM_TAG_ELEMENT(tag),
				  "DCM_GetSequenceList");

    elementItem = (void *)LST_Head(&groupItem->elementList);
    if (elementItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
		       DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(tag),
				  DCM_TAG_GROUP(tag),
				  "DCM_GetSequenceTag");

    (void) LST_Position(&groupItem->elementList, (void *)elementItem);
    while (!found && (elementItem != NULL)) {
	if (elementItem->element.tag == tag) {
	    found = TRUE;
	} else
	    elementItem = (void *)LST_Next(&groupItem->elementList);
    }
    if (!found)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
		       DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(tag),
				  DCM_TAG_ELEMENT(tag),
				  "DCM_GetElementValueList");

    if (!DCM_IsString(elementItem->element.representation)) {
	return COND_PushCondition(DCM_UNEXPECTEDREPRESENTATION,
	DCM_Message(DCM_UNEXPECTEDREPRESENTATION), "DCM_GetElementValueList",
				  "string");
    }
    src = elementItem->element.d.string;
    l = elementItem->element.length;
    while (l > 0) {
	while (l > 1 && (*src == ' ' || *src == DCM_DELIMITOR)) {
	    l--;
	    src++;
	}
	if ((l == 1) && (*src == ' ' || *src == DCM_DELIMITOR))
	    l--;

	if (l != 0) {
	    p = CTN_MALLOC(structureSize);
	    if (p == NULL)
		return COND_PushCondition(DCM_MALLOCFAILURE,
			      DCM_Message(DCM_MALLOCFAILURE), structureSize,
					  "DCM_GetElementValueList");
	    dst = p + stringOffset;
	    while ((l > 1) && (*src != DCM_DELIMITOR)) {
		*dst++ = *src++;
		l--;
	    }
	    if ((l == 1) && (*src != ' ')) {
		*dst++ = *src++;
		l--;
	    }
	    *dst = '\0';;
	    cond = LST_Enqueue(list, (void *)p);
	    if (cond != LST_NORMAL)
		return COND_PushCondition(DCM_LISTFAILURE,
		   DCM_Message(DCM_LISTFAILURE), "DCM_GetElementValueList");
	}
    }
    return DCM_NORMAL;
}

/* DCM_AddElementList
**
** Purpose:
**	Add an element list to the DICOM object
**
** Parameter Dictionary:
**	callerObject		Handle to object to which the element is to be
**				added
**	element			The element in which the string obtained from
**				the list is to be stored. Finally the element
**				is added to the DICOM object.
**	list			List of structures , each containing a string
**				starting at some offset specified by the
**				parameter "offset"
**	offset			Offset in each individual structure (see
**				explanation for parameter list)
**
** Return Values:
**
**	DCM_ILLEGALOBJECT
**	DCM_ILLEGALREPRESENTATION
**	DCM_INSERTFAILED
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/
CONDITION
DCM_AddElementList(DCM_OBJECT ** callerObject, DCM_ELEMENT * element,
		   LST_HEAD * list, long offset)
{
    DCM_ELEMENT
	e;			/* Local copy of caller's element */
    CONDITION
	cond;
    char
       *s;

    e = *element;
    cond = DCM_ListToString(list, offset, &s);
    if (cond != DCM_NORMAL)
	return cond;

    e.d.string = s;
    e.length = strlen(s);
    cond = DCM_AddElement(callerObject, &e);
    CTN_FREE(s);
    return cond;
}

/* DCM_GetElement
**
** Purpose:
**	Get the element with the specified tag number from the given DICOM
**	object
**
** Parameter Dictionary:
**	callerObject		Handle to the DICOM object
**	tag			Tag number of the element to be obtained
**				from the object
**	element			The element to be returned
**
** Return Values:
**
**	DCM_ELEMENTNOTFOUND
**	DCM_ILLEGALOBJECT
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/
CONDITION
DCM_GetElement(DCM_OBJECT ** callerObject, DCM_TAG tag, DCM_ELEMENT * element)
{
    PRIVATE_OBJECT
	** obj;
    CONDITION
	cond;
    PRV_ELEMENT_ITEM
	* elementItem;

    obj = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(obj, "DCM_GetElementVM");
    if (cond != DCM_NORMAL)
	return cond;

    elementItem = locateElement(obj, tag);
    if (elementItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
		       DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(tag),
				  DCM_TAG_ELEMENT(tag),
				  "DCM_GetElementVM");
    *element = elementItem->element;
    element->d.ot = NULL;
    return DCM_NORMAL;
}

CONDITION
DCM_ComputeExportLength(DCM_OBJECT ** callerObject, unsigned long opt,
			unsigned long *length)
{
    PRIVATE_OBJECT
	** object;
    unsigned char
        buf[2048];
    CONDITION
	cond;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_ComputeExportSize");
    if (cond != DCM_NORMAL)
	return cond;

    *length = 0;
    cond = DCM_ExportStream(callerObject, opt, buf,
			    (unsigned long) sizeof(buf), countBytes, length);
    if (cond != DCM_NORMAL)
	return cond;

    return DCM_NORMAL;
}

CONDITION
DCM_CompareAttributes(DCM_OBJECT ** o1, DCM_OBJECT ** o2,
		      void (*callback) (const DCM_ELEMENT * e1,
					const DCM_ELEMENT * e2,
					void *ctx),
		      void *ctx)
{
    PRIVATE_OBJECT **object1,
      **object2;
    PRV_GROUP_ITEM *groupItem1,
       *groupItem2;
    CONDITION cond;

    object1 = (PRIVATE_OBJECT **) o1;
    cond = checkObject(object1, "DCM_CompareAttributes");
    if (cond != DCM_NORMAL)
	return cond;

    object2 = (PRIVATE_OBJECT **) o2;
    cond = checkObject(object1, "DCM_CompareAttributes");
    if (cond != DCM_NORMAL)
	return cond;

    groupItem1 = (void *)LST_Head(&(*object1)->groupList);
    if (groupItem1 != NULL)
	(void) LST_Position(&(*object1)->groupList, (void *)groupItem1);

    groupItem2 = (void *)LST_Head(&(*object2)->groupList);
    if (groupItem2 != NULL)
	(void) LST_Position(&(*object2)->groupList, (void *)groupItem2);


    while (groupItem1 != NULL) {
	if (groupItem2 == NULL) {
	    compareGroup(groupItem1, NULL, callback, ctx);
	    groupItem1 = (void *)LST_Next(&(*object1)->groupList);
	} else if (groupItem1->group == groupItem2->group) {
	    compareGroup(groupItem1, groupItem2, callback, ctx);
	    groupItem1 = (void *)LST_Next(&(*object1)->groupList);
	    groupItem2 = (void *)LST_Next(&(*object2)->groupList);
	} else if (groupItem1->group > groupItem2->group) {
	    compareGroup(NULL, groupItem2, callback, ctx);
	    groupItem2 = (void *)LST_Next(&(*object2)->groupList);
	} else {
	    compareGroup(groupItem1, NULL, callback, ctx);
	    groupItem1 = (void *)LST_Next(&(*object1)->groupList);
	}
    }

    while (groupItem2 != NULL) {
	compareGroup(NULL, groupItem2, callback, ctx);
	groupItem2 = (void *)LST_Next(&(*object2)->groupList);
    }
    return DCM_NORMAL;
}

CTNBOOLEAN
DCM_GroupPresent(DCM_OBJECT ** o1, U16 group)
{
    PRIVATE_OBJECT **object;
    PRV_GROUP_ITEM * item;
    CONDITION cond;
    CTNBOOLEAN tooFar = FALSE;

    object = (PRIVATE_OBJECT **) o1;
    cond = checkObject(object, "DCM_CompareAttributes");
    if (cond != DCM_NORMAL)
	return FALSE;


    item = (void *)LST_Head(&(*object)->groupList);
    if (item != NULL)
	(void) LST_Position(&(*object)->groupList, (void *)item);

    while (item != NULL && !tooFar) {
	if (item->group == group) {
	    return TRUE;
	} else if (item->group > group) {
	    tooFar = TRUE;
	} else {
	    item = (void *)LST_Next(&(*object)->groupList);
	}
    }
    return FALSE;
}

/*     ------------------------------------------------------------
**  Private functions below here
*/

/* newElementItem
**
** Purpose:
**	Create a new element item suitable for placing in the linked
**	list representation of an ACR object.  Copy data from an
**	existing element, but skip the actual data field.
**	Describe the purpose of the function
**
** Parameter Dictionary:
**	src	Pointer to source element that is to be copied
**	dst	Pointer to pointer to destination element which is allocated
**		by this routine and filled in appropriately.
**
** Return Values:
**	DCM_NORMAL
**	DCM_ELEMENTCREATEFAILED
**
** Algorithm:
**	Allocate new element item of size:
**		Size PRV_ELEMENT_ITEM + length of data value
**	Copy data from caller's DCM_ELEMENT into newly created
**		PRV_ELEMENT_ITEM.
**	Point data value of newly created PRV_ELEMENT_ITEM to part of the
**	allocated space (just past the end of the PRV_ELEMENT_ITEM).
*/
static CONDITION
newElementItem(DCM_ELEMENT * src, CTNBOOLEAN allocateData,
	       PRV_ELEMENT_ITEM ** dst)
{
    U32
    l;

    if (allocateData && (src->representation != DCM_SQ)) {
	l = src->length;
	if (l & 1)
	    l++;
    } else
	l = 0;

    if (debug)
	fprintf(stderr, "newElementItem: CTN_MALLOC %8d %8d ", l,
		(int)(sizeof(PRV_ELEMENT_ITEM) + l));

    *dst = (PRV_ELEMENT_ITEM *) CTN_MALLOC(sizeof(PRV_ELEMENT_ITEM) + l);
    if (debug)
	fprintf(stderr, "%8p\n", *dst);

    if (*dst == NULL) {
	return COND_PushCondition(DCM_ELEMENTCREATEFAILED,
		     DCM_Message(DCM_ELEMENTCREATEFAILED), "newElementItem",
				  DCM_TAG_GROUP(src->tag),
				  DCM_TAG_ELEMENT(src->tag),
				  l);
    }
    memset(*dst, 0, sizeof(PRV_ELEMENT_ITEM));
    (*dst)->element = *src;
    (*dst)->byteOrder = NATIVE_ORDER;
    (*dst)->allocatedDataLength = (size_t) l;
    (*dst)->originalDataLength = src->length;
    (*dst)->paddedDataLength = src->length;
    if (allocateData)
	(*dst)->element.d.ot = ((char *) (*dst)) + sizeof(PRV_ELEMENT_ITEM);
    else
	(*dst)->element.d.ot = NULL;

    (*dst)->fragmentFlag = 0;
    return DCM_NORMAL;
}

/* findCreateGroup
**
** Purpose:
**	Find the group in the DCM object corresponding to the group
**	passed by the caller.  If the group does not yet exist, create
**	a new group.  Set the CURRENT pointer in the linked list
**	to point at that group.
**
** Parameter Dictionary:
**	object		Pointer to caller's DCM object
**	group		Group number to locate/create
**	groupPtr	Mechanism for returning pointer to located group
**
** Return Values:
**
**	DCM_ELEMENTCREATEFAILED
**	DCM_LISTFAILURE
**	DCM_NORMAL
**
** Algorithm:
**	Set ITEM to head of linked list of ACR object
**	Set CURRENT item in linked list to ITEM
**	Search sequentially through linked list until:
**	    - Reach exisiting group that matches caller's group
**	    - Reach a group with larger group number than caller's group
**	    - Reach end of linked list
**	Each time you move to a new item, update CURRENT to point to that item
**	If reached existing group
**	    return
**	If reached a group with larger group number than caller's group,
**	    Insert new group with Group Length Element (0000) just before
**	    the group with the larger group number.
**	    Set CURRENT pointer in linked list to point at new group
**	    If group is COMMAND or IDENTIFYING,
**		Insert Length to End Element
**	    Return
**	If reached end of the linked list
**	    Append new group with Group Length Element (0000) to the end
**	    of the linked list.
**	    Set CURRENT pointer in linked list to point at new group
**	    If group is COMMAND or IDENTIFYING,
**		Insert Length to End Element
**	    Return
**
*/

static CONDITION
findCreateGroup(PRIVATE_OBJECT ** object, unsigned short group,
		PRV_GROUP_ITEM ** groupItem)
{
    PRV_GROUP_ITEM
    * item;
    CONDITION
	cond;
    CTNBOOLEAN
	tooFar = FALSE;

    item = (void *)LST_Head(&(*object)->groupList);
    if (item != NULL)
	(void) LST_Position(&(*object)->groupList, (void *)item);

    while (item != NULL && !tooFar) {
	if (item->group == group) {
	    *groupItem = item;
	    return DCM_NORMAL;
	} else if (item->group > group) {
	    tooFar = TRUE;
	} else {
	    item = (void *)LST_Next(&(*object)->groupList);
	}
    }

    {
	U32 l;
	PRV_GROUP_ITEM *newGroupItem;
	DCM_ELEMENT groupLength = {0, DCM_UL, "", 1, sizeof(l), NULL};
	PRV_ELEMENT_ITEM *groupLengthItem;

	newGroupItem = CTN_MALLOC(sizeof(*newGroupItem));
	if (newGroupItem == NULL)
	    return COND_PushCondition(DCM_ELEMENTCREATEFAILED,
				      DCM_Message(DCM_ELEMENTCREATEFAILED),
				      "findCreateGroup",
				      group, 0xffff, sizeof(*newGroupItem));


	*groupItem = newGroupItem;
	newGroupItem->group = group;
	newGroupItem->baseLength = 0;
	newGroupItem->longVRAttributes = 0;
	newGroupItem->elementList = LST_Create();
	if (newGroupItem->elementList == NULL)
	    return COND_PushCondition(DCM_LISTFAILURE,
				      DCM_Message(DCM_LISTFAILURE),
				      "findCreateGroup");

	if (tooFar)
	    cond = LST_Insert(&(*object)->groupList, (void *)newGroupItem, LST_K_BEFORE);
	else
	    cond = LST_Enqueue(&(*object)->groupList, (void *)newGroupItem);
	if (cond != LST_NORMAL)
	    return COND_PushCondition(DCM_LISTFAILURE,
				      DCM_Message(DCM_LISTFAILURE),
				      "findCreateGroup");
	(void) LST_Position(&(*object)->groupList, (void *)newGroupItem);
	if (cond != LST_NORMAL)
	    return COND_PushCondition(DCM_LISTFAILURE,
				      DCM_Message(DCM_LISTFAILURE),
				      "findCreateGroup");

	groupLength.d.ul = &l;
	l = 0;
	if ((*object)->groupLengthFlag) {
	    groupLength.tag = DCM_MAKETAG(group, 0);
	    cond = newElementItem(&groupLength, TRUE, &groupLengthItem);
	    (void) memcpy(groupLengthItem->element.d.ot, &l, sizeof(l));

	    if (LST_Insert(&newGroupItem->elementList, (void *)groupLengthItem, LST_K_AFTER) !=
		LST_NORMAL)
		return COND_PushCondition(DCM_LISTFAILURE,
					  DCM_Message(DCM_LISTFAILURE),
					  "findCreateGroup");

	    (*object)->objectSize += 8 + groupLengthItem->element.length;
	}
    }
    return DCM_NORMAL;
}

/* insertNewElement
**
** Purpose:
**	Create a new DCM_ELEMENT item using a copy of the caller's
**	DCM_ELEMENT and insert it into the ACR object's linked list.
**
** Parameter Dictionary:
**	object		Pointer to caller's ACR_OBJECT
**	element		Pointer to caller's ACR_ELEMENT to be copied
**			and inserted into linked list.
**
** Return Values:
**
**	DCM_ELEMENTCREATEFAILED
**	DCM_LISTFAILURE
**	DCM_NORMAL
**	DCM_UNEVENELEMENTLENGTH
**	DCM_BADELEMENTINGROUP
**
** Algorithm:
**	Call newElementItem to create a copy of the DCM_ELEMENT
**	Copy caller's data into data area allocated by newElementItem
**	Increment object's OBJECTSIZE field by size of new element
**	Use CURRENT pointer in DCM object linked list to get pointer
**	to the group where we insert this element
**	Update Group Length by adding size of new element
**	Search sequentially through linked list until we reach:
**	    - End of linked list
**	    - A different group
**	    - An element in the same group with a larger element number
**	If reached end of linked list
**	    Append new ACR_ELEMENTITEM to end of linked list
**	If reached a different group
**	    Insert new ACR_ELEMENTITEM just before new group
**	If reached an element in the same group with a larger element number
**	    Insert new ACR_ELEMENTITEM just before the "larger" element
*/
static CONDITION
insertNewElement(PRIVATE_OBJECT ** object, DCM_ELEMENT * element)
{
    PRV_ELEMENT_ITEM
    * nextItem,
    *newItem;
    PRV_GROUP_ITEM
	* groupItem;
    CONDITION
	cond;
    char
       *p;

    cond = newElementItem(element, TRUE, &newItem);
    if (cond != DCM_NORMAL) {
	return cond;
    }
    newItem->byteOrder = DCM_ORDERNATIVE;
    if ((newItem->element.length & 1) != 0) {
	if (newItem->element.representation == DCM_AE) {
	    p = newItem->element.d.string;	/* repair, check for 16 */
	    p[newItem->element.length] = ' ';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_AS) {
	    p = newItem->element.d.string;
	    p[newItem->element.length] = ' ';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_CS) {
	    p = newItem->element.d.string;
	    p[newItem->element.length] = ' ';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_DA) {
	    p = newItem->element.d.string;
	    p[newItem->element.length] = ' ';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_DS) {
	    p = newItem->element.d.string;
	    p[newItem->element.length] = ' ';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_IS) {
	    p = newItem->element.d.string;
	    p[newItem->element.length] = ' ';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_LT) {
	    p = newItem->element.d.string;
	    p[newItem->element.length] = ' ';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_LO) {
	    p = newItem->element.d.string;
	    p[newItem->element.length] = ' ';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_PN) {
	    p = newItem->element.d.string;
	    p[newItem->element.length] = ' ';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_SH) {
	    p = newItem->element.d.string;
	    p[newItem->element.length] = ' ';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_ST) {
	    p = newItem->element.d.string;
	    p[newItem->element.length] = ' ';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_TM) {
	    p = newItem->element.d.string;
	    p[newItem->element.length] = ' ';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_UI) {
	    p = newItem->element.d.string;
	    p[newItem->element.length] = '\0';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_UT) {
	    p = newItem->element.d.string;
	    p[newItem->element.length] = ' ';
	    newItem->paddedDataLength = element->length + 1;
	    (void) memcpy(p, element->d.string, element->length);
	} else if (newItem->element.representation == DCM_SQ) {
/*	    newItem->element.length = 0xffffffff; */
	    newItem->element.d.sq = element->d.sq;
	} else {
	    CTN_FREE(newItem);
	    return COND_PushCondition(DCM_UNEVENELEMENTLENGTH,
				      DCM_Message(DCM_UNEVENELEMENTLENGTH),
				      DCM_TAG_GROUP(element->tag),
			     DCM_TAG_ELEMENT(element->tag), element->length,
				      "insertNewElement");
	}
    } else if (newItem->element.representation != DCM_SQ) {
	(void) memcpy(newItem->element.d.ot, element->d.ot, element->length);
    } else {
/*	newItem->element.length = 0xffffffff; */
	newItem->element.d.sq = element->d.sq;
    }
    if ((*object)->objectSize != DCM_UNSPECIFIEDLENGTH)
	(*object)->objectSize += 8 + newItem->paddedDataLength;

/* repair */
    cond = updateSpecialElements(object, newItem);
    if (cond != DCM_NORMAL)
	return cond;

    groupItem = (void *)LST_Current(&(*object)->groupList);
    if (groupItem == NULL)
	return COND_PushCondition(DCM_LISTFAILURE,
			  DCM_Message(DCM_LISTFAILURE), "insertNewElement");

    if (groupItem->baseLength != DCM_UNSPECIFIEDLENGTH)
	groupItem->baseLength += 2 + 2 + 4 + newItem->paddedDataLength;

    if (newItem->element.representation == DCM_OW ||
	newItem->element.representation == DCM_OB ||
	newItem->element.representation == DCM_SQ) {
	groupItem->longVRAttributes++;
	(*object)->longVRAttributes++;
    }
    if ((nextItem = (void *)LST_Head(&groupItem->elementList)) == NULL) {
	cond = LST_Enqueue(&groupItem->elementList, (void *)newItem);
	if (cond != LST_NORMAL)
	    return COND_PushCondition(DCM_LISTFAILURE,
				      DCM_Message(DCM_LISTFAILURE),
				      "insertNewElement");
	else
	    return DCM_NORMAL;
    }
    (void) LST_Position(&groupItem->elementList, (void *)nextItem);
    if (DCM_TAG_ELEMENT(nextItem->element.tag) == 0x0000)
	(void) memcpy(nextItem->element.d.ot, &groupItem->baseLength,
		      sizeof(groupItem->baseLength));

/*  Now, search through the linked list for a place to insert/append
**  this new item.
*/

    while (nextItem != NULL) {
	if (DCM_TAG_GROUP(element->tag) !=
	    DCM_TAG_GROUP(nextItem->element.tag)) {
	    return COND_PushCondition(DCM_BADELEMENTINGROUP,
				      DCM_Message(DCM_BADELEMENTINGROUP),
				      DCM_TAG_GROUP(nextItem->element.tag),
				      DCM_TAG_ELEMENT(nextItem->element.tag),
				      groupItem->group, "insertNewElement");
	} else if (DCM_TAG_ELEMENT(element->tag) <
		   DCM_TAG_ELEMENT(nextItem->element.tag)) {
	    cond = LST_Insert(&groupItem->elementList, (void *)newItem, LST_K_BEFORE);
	    if (cond != LST_NORMAL)
		return COND_PushCondition(DCM_LISTFAILURE,
					  DCM_Message(DCM_LISTFAILURE),
					  "insertNewElement");
	    else
		return DCM_NORMAL;
	}
	nextItem = (void *)LST_Next(&groupItem->elementList);
    }

/*  If we fall out of the loop, we must have reached the end of
**  the group.  Add the element to the end of the list of elements
**  in this group.
*/

    cond = LST_Enqueue(&groupItem->elementList, (void *)newItem);
    if (cond != LST_NORMAL)
	return COND_PushCondition(DCM_LISTFAILURE,
				  DCM_Message(DCM_LISTFAILURE),
				  "insertNewElement");
    else
	return DCM_NORMAL;
}

static CONDITION
insertThisElementItem(PRIVATE_OBJECT ** object, PRV_ELEMENT_ITEM* newItem)
{
  PRV_ELEMENT_ITEM * nextItem;
  PRV_GROUP_ITEM * groupItem = 0;
  CONDITION cond;

/* repair */
  cond = updateSpecialElements(object, newItem);
  if (cond != DCM_NORMAL)
    return cond;

  cond = findCreateGroup(object, DCM_TAG_GROUP(newItem->element.tag),
	&groupItem);

  if (groupItem == NULL)
    return COND_PushCondition(DCM_LISTFAILURE,
			  DCM_Message(DCM_LISTFAILURE), "insertThisElementItem");

  if (groupItem->baseLength != DCM_UNSPECIFIEDLENGTH)
    groupItem->baseLength += 2 + 2 + 4 + newItem->paddedDataLength;

  if (newItem->element.representation == DCM_OW ||
	newItem->element.representation == DCM_OB ||
	newItem->element.representation == DCM_SQ) {
	groupItem->longVRAttributes++;
	(*object)->longVRAttributes++;
  }

  if ((nextItem = (void *)LST_Head(&groupItem->elementList)) == NULL) {
    cond = LST_Enqueue(&groupItem->elementList, (void *)newItem);
    if (cond != LST_NORMAL)
      return COND_PushCondition(DCM_LISTFAILURE,
				      DCM_Message(DCM_LISTFAILURE),
				      "insertThisElementItem");
    else
      return DCM_NORMAL;
  }

  (void) LST_Position(&groupItem->elementList, (void *)nextItem);
  if (DCM_TAG_ELEMENT(nextItem->element.tag) == 0x0000)
    (void) memcpy(nextItem->element.d.ot, &groupItem->baseLength,
		      sizeof(groupItem->baseLength));

/*  Now, search through the linked list for a place to insert/append
**  this new item.
*/

  while (nextItem != NULL) {
    if (DCM_TAG_GROUP(newItem->element.tag) !=
	    DCM_TAG_GROUP(nextItem->element.tag)) {
      return COND_PushCondition(DCM_BADELEMENTINGROUP,
				      DCM_Message(DCM_BADELEMENTINGROUP),
				      DCM_TAG_GROUP(nextItem->element.tag),
				      DCM_TAG_ELEMENT(nextItem->element.tag),
				      groupItem->group, "insertThisElementItem");
    } else if (DCM_TAG_ELEMENT(newItem->element.tag) <
		   DCM_TAG_ELEMENT(nextItem->element.tag)) {
      cond = LST_Insert(&groupItem->elementList, (void *)newItem, LST_K_BEFORE);
      if (cond != LST_NORMAL)
	return COND_PushCondition(DCM_LISTFAILURE,
					  DCM_Message(DCM_LISTFAILURE),
					  "insertThisElementItem");
      else
	return DCM_NORMAL;
      }
      nextItem = (void *)LST_Next(&groupItem->elementList);
  }

/*  If we fall out of the loop, we must have reached the end of
**  the group.  Add the element to the end of the list of elements
**  in this group.
*/

  cond = LST_Enqueue(&groupItem->elementList, (void *)newItem);
  if (cond != LST_NORMAL)
    return COND_PushCondition(DCM_LISTFAILURE,
				  DCM_Message(DCM_LISTFAILURE),
				  "insertThisElementItem");
  else
    return DCM_NORMAL;
}

/* updateObjectType
**
** Purpose:
**	Possibly modify the objectType field of an DCM object to identify
**	the object as COMMAND, DATASET or MESSAGE.
**
** Parameter Dictionary:
**	object		Pointer to caller's PRIVATE object to be updated
**	element		Pointer to DCM_ELEMENT which will be added to
**			the object and possibly cause a change in the
**			type of the object.
**
** Return Values:
**	DCM_NORMAL
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

static CONDITION
updateObjectType(PRIVATE_OBJECT ** object, DCM_ELEMENT * element)
{
    switch ((*object)->objectType) {
	case DCM_OBJECTUNKNOWN:
	if (DCM_TAG_GROUP(element->tag) == DCM_GROUPCOMMAND)
	    (*object)->objectType = DCM_OBJECTCOMMAND;
	else
	    (*object)->objectType = DCM_OBJECTELEMENTLIST;
	break;
    case DCM_OBJECTCOMMAND:
	if (DCM_TAG_GROUP(element->tag) != DCM_GROUPCOMMAND)
	    (*object)->objectType = DCM_OBJECTELEMENTLIST;
	break;
    case DCM_OBJECTELEMENTLIST:
    case DCM_OBJECTIMAGE:
	break;
    default:
	break;
    }
    return DCM_NORMAL;
}

/* updateSpecialElements
**
** Purpose:
**	Update special elements in a DICOM object when a new data element
**	is added to the object.  These special fields are used by other
**	parts of the package which have to refer to those fields and wish
**	to do so without searching through the entire list.  This could
**	get messy and is a candidate for redesign.
**
** Parameter Dictionary:
**	object		Pointer to caller's PRIVATE DICOM object
**	element		Pointer to DCM_ELEMENT that is being added to
**			the DICOM object
**
** Return Values:
**	DCM_NORMAL
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/
static CONDITION
updateSpecialElements(PRIVATE_OBJECT ** object,
		      PRV_ELEMENT_ITEM * item)
{
    int idx;

    switch (item->element.tag) {
    case DCM_IMGBITSALLOCATED:
	(*object)->pixelBitsAllocated = *item->element.d.us;
	break;
    case DCM_IMGPIXELREPRESENTATION:
	(*object)->pixelRepresentation = *item->element.d.us;
	break;
    case DCM_METAGROUPLENGTH:
	(*object)->metaHeaderLength = *item->element.d.ul;
	break;
    case DCM_METATRANSFERSYNTAX:
	if (strcmp(item->element.d.string, DICOM_TRANSFERLITTLEENDIAN) == 0) {
	    (*object)->dataOptions = DCM_ORDERLITTLEENDIAN;
	} else if (strcmp(item->element.d.string, DICOM_TRANSFERLITTLEENDIANEXPLICIT) == 0) {
	    (*object)->dataOptions = DCM_EXPLICITLITTLEENDIAN;
	} else if (strcmp(item->element.d.string, DICOM_TRANSFERBIGENDIANEXPLICIT) == 0) {
	    (*object)->dataOptions = DCM_EXPLICITBIGENDIAN;
	} else {	/* Must be an encapsulated transfer syntax */
	    (*object)->dataOptions = DCM_EXPLICITLITTLEENDIAN;
	}
	break;
    case DCM_MAKETAG(0x003a, 0x0103):
	strncpy((*object)->waveformDataVR, item->element.d.string,
		item->element.length);
	(*object)->waveformDataVR[item->element.length] = '\0';
	idx = item->element.length - 1;
	while (idx >= 0 && (*object)->waveformDataVR[idx] == ' ') {
	    (*object)->waveformDataVR[idx] = '\0';
	    idx--;
	}
	break;
    default:
	break;
    }
    return DCM_NORMAL;
}

typedef struct {
    DCM_VALUEREPRESENTATION representation;
    char code[3];
}   VRMAP;

static VRMAP vrMap[] = {
    {DCM_AE, "AE"},
    {DCM_AS, "AS"},
    {DCM_AT, "AT"},
    {DCM_CS, "CS"},
    {DCM_DA, "DA"},
    {DCM_DD, "DD"},
    {DCM_DS, "DS"},
    {DCM_FD, "FD"},
    {DCM_FL, "FL"},
    {DCM_IS, "IS"},
    {DCM_LO, "LO"},
    {DCM_LT, "LT"},
    {DCM_OT, "OT"},
    {DCM_SH, "SH"},
    {DCM_SL, "SL"},
    {DCM_SQ, "SQ"},
    {DCM_SS, "SS"},
    {DCM_ST, "ST"},
    {DCM_TM, "TM"},
    {DCM_UI, "UI"},
    {DCM_UL, "UL"},
    {DCM_UN, "UN"},
    {DCM_US, "US"},
    {DCM_UT, "UT"},
    /*{DCM_UNKNOWN, "UK"},*/
    {DCM_RET, "RT"},
    {DCM_CTX, "  "},
    {DCM_PN, "PN"},
    {DCM_OB, "OB"},
    {DCM_OW, "OW"},
    {DCM_DT, "DT"},
    {DCM_DLM, ""}
};

static VRMAP *
lookupVRCode(const char *code)
{
    int i;

    for (i = 0; i < (int) DIM_OF(vrMap); i++) {
	if (strcmp(code, vrMap[i].code) == 0)
	    return &vrMap[i];
    }

    return NULL;
}

static void
mapVRtoASCII(DCM_VALUEREPRESENTATION vr, char *s)
{
    int i;

    for (i = 0; i < (int) DIM_OF(vrMap); i++) {
	if (vr == vrMap[i].representation) {
	    strcpy(s, vrMap[i].code);
	    return;
	}
    }

    strcpy(s, "");
    return;
}

static void
exportVRLength(DCM_ELEMENT * e, unsigned char *b, int byteOrder,
	       U32 * rtnLength)
{
    int i;
    char *c = "xx";
    unsigned char *p;
    U16 shortLength;
    DCM_VALUEREPRESENTATION vr;

    vr = e->representation;
    if (e->tag == DCM_MAKETAG(0x003a, 0x1000))
	vr = DCM_OB;

    for (i = 0; i < DIM_OF(vrMap); i++) {
	if (vr == vrMap[i].representation) {
	    c = vrMap[i].code;
	    break;
	}
    }

    *b++ = *c++;
    *b++ = *c++;
    *rtnLength += 2;

    if (vr == DCM_OB || vr == DCM_OW || vr == DCM_SQ || vr == DCM_UN) {
	*b++ = 0x00;
	*b++ = 0x00;
	if (byteOrder == BYTEORDER_SAME) {
	    p = (unsigned char *) &e->length;
	    *b++ = *p++;
	    *b++ = *p++;
	    *b++ = *p++;
	    *b++ = *p++;
	} else {
	    p = (unsigned char *) &e->length;
	    *b++ = p[3];
	    *b++ = p[2];
	    *b++ = p[1];
	    *b++ = p[0];
	}
	*rtnLength += 6;
    } else {
	shortLength = (U16) e->length;
	if (byteOrder == BYTEORDER_SAME) {
	    p = (unsigned char *) &shortLength;
	    *b++ = *p++;
	    *b++ = *p++;
	} else {
	    p = (unsigned char *) &shortLength;
	    *b++ = p[1];
	    *b++ = p[0];
	}
	*rtnLength += 2;
    }
}

static CONDITION
exportPreamble(PRIVATE_OBJECT ** obj, unsigned char *dst,
	       U32 bufferLength, U32 * rtnLength)
{
    *rtnLength = 0;
    if (bufferLength < (DCM_PREAMBLELENGTH + 4))
	return COND_PushCondition(DCM_EXPORTBUFFERTOOSMALL,
		  DCM_Message(DCM_EXPORTBUFFERTOOSMALL), (int) bufferLength,
				  "exportPreamble");

    (void) memcpy(dst, (*obj)->preamble, DCM_PREAMBLELENGTH);
    dst += DCM_PREAMBLELENGTH;
    (void) memcpy(dst, "DICM", 4);
    *rtnLength += DCM_PREAMBLELENGTH + 4;

    return DCM_NORMAL;
}

/* exportFixedFields
**
** Purpose:
**	This function exports the fixed length fields of an DCM_ELEMENT
**	to the caller's buffer if there is sufficient space in the
**	caller's buffer.
**
** Parameter Dictionary:
**	element		Pointer to the actual data element to be exported
**	b		Pointer to the caller's buffer to hold exported data
**	length		Length of the remaining space in the caller's
**			buffer
**	byteOrder	flag giving the order of the bytes as they are
**			exported.  Should be one of:
**				BYTEORDER_SAME
**				BYTEORDER_REVERSE
**	rtnLength	Pointer to caller variable to hold the length
**			of the data exported.  The length of the data
**			exported will be 0 if the caller's buffer is
**			too small to hold the fixed length fields.
**
** Return Values:
**	None
**
** Algorithm:
**	If caller buffer is too small to hold all fixed length fields
**	    Place 0 in caller's rtnLength variable
**	    return
**	Else
**	    If byteOrder is the same
**		Copy fixed length fields in same byte order
**	    Else
**		Copy fixed length fields in reverse byte order
**	    Set caller's rtnLength variable to 8 (short, short, long)
*/

static void
exportFixedFields(DCM_ELEMENT * e,
		  unsigned char *b, U32 length, int byteOrder,
		  CTNBOOLEAN explicitVR, U32 * rtnLength)
{
    unsigned char
       *p;
    unsigned short
        group,
        element;
    U32
	minimumLength;

    group = DCM_TAG_GROUP(e->tag);
    element = DCM_TAG_ELEMENT(e->tag);
    if (e->representation == DCM_DLM)
	explicitVR = FALSE;

    minimumLength = sizeof(group) + sizeof(element) + sizeof(e->length);
    if (explicitVR)
	minimumLength += 4;

    *rtnLength = 0;
    if (length >= minimumLength) {
	if (byteOrder == BYTEORDER_SAME) {
	    p = (unsigned char *) &group;
	    *b++ = *p++;
	    *b++ = *p++;
	    p = (unsigned char *) &element;
	    *b++ = *p++;
	    *b++ = *p++;
	    *rtnLength += 4;
	    if (explicitVR) {
		exportVRLength(e, b, byteOrder, rtnLength);
	    } else {
		p = (unsigned char *) &e->length;
		*b++ = *p++;
		*b++ = *p++;
		*b++ = *p++;
		*b++ = *p++;
		*rtnLength += 4;
	    }
	} else {
	    p = (unsigned char *) &group;
	    *b++ = p[1];
	    *b++ = p[0];
	    p = (unsigned char *) &element;
	    *b++ = p[1];
	    *b++ = p[0];
	    *rtnLength += 4;
	    if (explicitVR) {
		exportVRLength(e, b, byteOrder, rtnLength);
	    } else {
		p = (unsigned char *) &e->length;
		*b++ = p[3];
		*b++ = p[2];
		*b++ = p[1];
		*b++ = p[0];
		*rtnLength += 4;
	    }
	}
    }
}

/* exportData
**
** Purpose:
**	Export the data part of a DCM_ELEMENT.  This function exports
**	all or part of the data portion of an DCM_ELEMENT in the byte order
**	requested by the caller.  The caller specifies the byte order
**	in a formal argument.  The function uses context information to
**	know where to start the export in one data element.  The function
**	does not update the context information but does return the
**	number of bytes exported.
**
** Parameter Dictionary:
**	object		Pointer to the caller's ACR object which is
**			being exported.
**	element		Pointer to the ACR_ELEMENT that is being exported
**	b		Pointer to the caller's buffer to hold the
**			exported data.
**	length		Length of the caller's buffer to hold the data.
**	byteOrder	Flag giving the order of the bytes in the exported
**			stream.  Flag should be one of:
**			    BYTEORDER_SAME
**			    BYTEORDER_REVERSE
**	rtnLength	Pointer to caller variable to hold number of bytes
**			that are actually exported.
**
** Return Values:
**
**	DCM_FILEACCESSERROR
**	DCM_NORMAL
**
** Algorithm
**
**	Set caller's rtnLength variable to 0
**	Export data based on representation of data element
**	CASE 16 bit binary:
**	    While (length >= 2)
**		If (byte order is same OR element is 8 bit pixel data)
**		    Copy 2 bytes to output area
**		    Increment input/output pointers by 2
**		Else
**		    Copy and swap 2 bytes to output area
**		    Increment input/output pointers by 2
**		Endif
**		Decrement length by 2
**		Increment caller's rtnLength by 2
**	    End while
**
**	CASE 32 bit binary:
**	    While (length >= 4)
**		If (byte order is same)
**		    Copy 4 bytes to output area
**		    Increment input/output pointers by 4
**		Else
**		    Copy and swap 4 bytes to output area
**		    Increment input/output pointers by 4
**		Endif
**		Decrement length by 4
**		Increment caller's rtnLength by 4
**
**	CASE ascii text, ascii numeric, or unknown:
**	    Use memcpy to copy as of the remaining data as will fit
**		in the caller's buffer.
**	    Set caller's rtnLength to the amount of data copied.
**
*/
union {
    unsigned short sh[2];
    unsigned char ch[4];
}   groupElement;

static CONDITION
exportData(PRIVATE_OBJECT ** object, PRV_ELEMENT_ITEM * item,
	   unsigned char *src,
	   unsigned char *b, U32 length, int byteOrder,
	   U32 * rtnLength)
{
/* repair OT for pixel data*/
    unsigned char
       *p;
    DCM_TAG
	* tag;
    DCM_ELEMENT
	* element;
    int nBytes;
    CONDITION cond;

    element = &item->element;

    *rtnLength = 0;
    if (element->d.ot == NULL) {
	if ((*object)->fd != -1) {
	    (void) lseek((*object)->fd, item->currentOffset, SEEK_SET);
	    nBytes = read((*object)->fd, b, (int) length);
	} else {
	    (*object)->sk((*object)->userCtx, item->currentOffset, SEEK_SET);
	    cond = (*object)->rd((*object)->userCtx, b, (long) length, &nBytes);
	}
	if ((U32) nBytes != length) {
	    char b[512];
	    sprintf(b, "byte count: %d %d, errno: %d", nBytes, length, errno);
	    (void) COND_PushCondition(DCM_GENERALWARNING,
			  DCM_Message(DCM_GENERALWARNING), "exportData", b);
	    return COND_PushCondition(DCM_FILEACCESSERROR,
		      DCM_Message(DCM_FILEACCESSERROR), (*object)->fileName,
				      "exportData");
	}
        if( LITTLE_ENDIAN_ARCHITECTURE ){
	  if (item->element.representation == DCM_AT) {
	      DCM_ELEMENT e;
	      e = *element;
	      e.length = length;
	      e.d.ot = b;
	      swapATGroupElement(&e);
	  }
        }
	if (byteOrder != item->byteOrder) {
	    DCM_ELEMENT e;
	    e = *element;
	    e.length = length;
	    e.d.ot = b;
	    swapInPlace(object, &e);
	}
	*rtnLength = (U32) nBytes;
	item->currentOffset += nBytes;
    } else {
	p = src;
	switch (element->representation) {
	case DCM_AE:
	case DCM_AS:
	case DCM_CS:
	case DCM_DA:
	case DCM_DT:
	case DCM_DD:
	case DCM_DS:
	case DCM_FD:
	case DCM_IS:
	case DCM_LO:
	case DCM_LT:
	case DCM_OB:
	case DCM_OT:
	case DCM_PN:
	case DCM_SH:
	case DCM_SQ:
	case DCM_ST:
	case DCM_TM:
	case DCM_UI:
	case DCM_UT:
	    (void) memcpy(b, p, length);
	    *rtnLength = length;
	    break;
	case DCM_AT:
	    tag = (DCM_TAG *) p;
	    while (length >= 4) {
		groupElement.sh[0] = DCM_TAG_GROUP(*tag);
		groupElement.sh[1] = DCM_TAG_ELEMENT(*tag);
		if (byteOrder == BYTEORDER_SAME) {
		    *b++ = groupElement.ch[0];	/* Copy the group */
		    *b++ = groupElement.ch[1];
		    *b++ = groupElement.ch[2];	/* Now, the element */
		    *b++ = groupElement.ch[3];
		} else {
		    *b++ = groupElement.ch[1];	/* Copy the group */
		    *b++ = groupElement.ch[0];
		    *b++ = groupElement.ch[3];	/* Now, the element */
		    *b++ = groupElement.ch[2];
		}
		tag++;
		length -= 4;
		*rtnLength += 4;
	    }
	    break;
	case DCM_SL:
	case DCM_UL:
	case DCM_FL:
	    while (length >= 4) {
		if (byteOrder == BYTEORDER_SAME) {
		    *b++ = *p++;
		    *b++ = *p++;
		    *b++ = *p++;
		    *b++ = *p++;
		} else {
		    *b++ = p[3];
		    *b++ = p[2];
		    *b++ = p[1];
		    *b++ = p[0];
		    p += 4;
		}
		length -= 4;
		*rtnLength += 4;
	    }
	    break;
	case DCM_SS:
	case DCM_US:
	case DCM_OW:
	    /*
	     * Temorary hack by Nilesh to support memory mapping for testing
	     * purposes.
	     */
	    length &= ~1;
	    *rtnLength += length;
	    if (element->tag == DCM_PXLPIXELDATA) {
		if (byteOrder == item->byteOrder)
		    (void) memcpy(b, p, length);
		else
#ifdef SOLARIS
		    swab((char *) p, (char *) b, length);
#elif defined AIXV3
		swab((short *) p, (short *) b, length);
#elif defined MACOS
		/* Not Yet Defined */
#else
		    swab(p, b, length);
#endif
	    } else {
		if (byteOrder == BYTEORDER_SAME)
		    (void) memcpy(b, p, length);
		else
#ifdef SOLARIS
		    swab((char *) p, (char *) b, length);
#elif defined AIXV3
		swab((short *) p, (short *) b, length);
#elif defined MACOS
		/* Not Yet Defined */
#else
		    swab(p, b, length);
#endif
	    }
	    break;
	/*case DCM_UNKNOWN:*/
	case DCM_UN:
	default:
#if 0
	    fprintf(stderr, "Should not get to default in exportData: %08x\n",
		    element->tag);
#endif
	    (void) memcpy(b, p, length);
	    *rtnLength = length;
	    break;
	}
    }
    return DCM_NORMAL;
}

static CONDITION
exportEncapsulatedPixels(PRIVATE_OBJECT ** object, PRV_ELEMENT_ITEM * item,
	unsigned char* buffer, U32 bufferlength, DCM_EXPORT_STREAM_CALLBACK* callback,
	void* ctx)
{
  DCM_ELEMENT * element;
  int nBytes;
  CONDITION cond;
  U32 toExport;
  int length;
  DCM_FRAGMENT_ITEM* fragmentItem = 0;
  DCM_ELEMENT e;
  U32 rtnLength = 0;

  element = &item->element;
  if (element->d.ot == NULL) {
    if ((*object)->fd != -1) {
      /* Seek to the beginning of the data. Have to back up 12 bytes to
      ** get the pixel tag, VR, etc
      */
      (void) lseek((*object)->fd, item->dataOffset-12, SEEK_SET);
    } else {
      (*object)->sk((*object)->userCtx, item->dataOffset-12, SEEK_SET);
    }

    toExport = item->originalDataLength + 12;
    while(toExport > 0) {
      length = (toExport < bufferlength) ? toExport : bufferlength;

      if ((*object)->fd != -1) {
	nBytes = read((*object)->fd, buffer, length);
      } else {
	cond = (*object)->rd((*object)->userCtx, buffer, (long) length, &nBytes);
      }
      if ((U32) nBytes != length) {
	char b[512];
	sprintf(b, "byte count: %d %d, errno: %d", nBytes, length, errno);
	(void) COND_PushCondition(DCM_GENERALWARNING,
			  DCM_Message(DCM_GENERALWARNING), "exportEncapsualtedPixels", b);
	return COND_PushCondition(DCM_FILEACCESSERROR,
		      DCM_Message(DCM_FILEACCESSERROR), (*object)->fileName,
				      "exportEncapsualtedPixels");
      }
      cond = callback(buffer, length, 0, ctx);
      if (cond != DCM_NORMAL) {
	return COND_PushCondition(DCM_CALLBACKABORTED,
	      DCM_Message(DCM_CALLBACKABORTED), "exportStream");
      }
      toExport -= length;
    }
  } else {
    if (item->fragmentFlag != 1) {
      return COND_PushCondition(DCM_NOFRAGMENTSINOBJECT,
	"DCM Exporting pixels but did not find expected fragments in object");
    }
    e.tag = DCM_PXLPIXELDATA;
    e.d.ot = 0;
    e.representation = DCM_OB;
    e.length = 0xffffffff;
    exportFixedFields(&e, buffer, bufferlength,
			LITTLE_ORDER /*byteOrder*/,
			1 /* explicitV*/,
			&rtnLength);
    toExport = rtnLength;
    e.tag = 0xfffee000;
    e.length = 0;
    e.representation = DCM_DLM;
    e.d.ot = 0;
    exportFixedFields(&e, buffer+toExport, bufferlength,
			LITTLE_ORDER /*byteOrder*/,
			1 /* explicitV*/,
			&rtnLength);
    toExport += rtnLength;

    cond = callback(buffer, toExport, 0, ctx);
    if (cond != DCM_NORMAL) {
      return COND_PushCondition(DCM_CALLBACKABORTED,
	      DCM_Message(DCM_CALLBACKABORTED), "exportEncapsulatedPixels");
    }

    fragmentItem = (DCM_FRAGMENT_ITEM*)LST_Head(&item->element.d.fragments);
    (void)LST_Position(&item->element.d.fragments, (void *)fragmentItem);
    while (fragmentItem != NULL) {
      RWC_printf("Fragment size: %6d\n", fragmentItem->length);
      e.tag = 0xfffee000;
      e.length = fragmentItem->length;
      e.representation = DCM_DLM;
      exportFixedFields(&e, buffer, bufferlength,
			LITTLE_ORDER /*byteOrder*/,
			1 /* explicitV*/,
			&rtnLength);
      cond = callback(buffer, rtnLength, 0, ctx);
      if (cond != DCM_NORMAL) {
	return COND_PushCondition(DCM_CALLBACKABORTED,
	      DCM_Message(DCM_CALLBACKABORTED), "exportEncapsulatedPixels");
      }
      cond = callback(fragmentItem->fragment, fragmentItem->length, 0, ctx);
      if (cond != DCM_NORMAL) {
	return COND_PushCondition(DCM_CALLBACKABORTED,
	      DCM_Message(DCM_CALLBACKABORTED), "exportEncapsulatedPixels");
      }

      fragmentItem = (void *)LST_Next(&item->element.d.fragments);
    }
    e.tag = 0xfffee0dd;
    e.length = 0;
    e.representation = DCM_DLM;
    e.d.ot = 0;
    exportFixedFields(&e, buffer, bufferlength,
			LITTLE_ORDER /*byteOrder*/,
			1 /* explicitV*/,
			&rtnLength);
    cond = callback(buffer, rtnLength, 0, ctx);
    if (cond != DCM_NORMAL) {
      return COND_PushCondition(DCM_CALLBACKABORTED,
	      DCM_Message(DCM_CALLBACKABORTED), "exportEncapsulatedPixels");
    }
  }
  return DCM_NORMAL;
}

static CONDITION
exportPixels(PRIVATE_OBJECT ** object, PRV_ELEMENT_ITEM * item,
	int encapsulatedPixels,
	unsigned char* buffer, U32 bufferlength, DCM_EXPORT_STREAM_CALLBACK* callback,
	void* ctx,
	int byteOrder, int explicitVR)
{
  DCM_ELEMENT * element;
  int nBytes;
  CONDITION cond;
  U32 toExport;
  U32 bytesExported = 0;
  U32 exportLength = 0;
  int length;
  U32 rtnLength;
  U32 remainingData;
  unsigned char* dst;
  unsigned char* src;
  int c;

  if (encapsulatedPixels) {
    return exportEncapsulatedPixels(object, item, buffer,
	bufferlength, callback, ctx);
  }

  element = &item->element;
  rtnLength = 0;
  dst = buffer;
  c = bufferlength;
  exportFixedFields(element, dst, bufferlength, byteOrder,
				  explicitVR, &rtnLength);
  dst += rtnLength;
  c -= rtnLength;
  bytesExported = rtnLength;

  remainingData = element->length;
  src = element->d.ot;
  item->currentOffset = item->dataOffset;

  while (remainingData > 0) {
    if (debug) {
      fprintf(stderr, "Export: (%08x) %d\n", element->tag, element->length);
    }

    if (element->d.ot != NULL) {
      remainingData = element->length -
			    (src - ((unsigned char *) element->d.ot));
    } else {
      remainingData = element->length -
			    (item->currentOffset - item->dataOffset);
    }

    exportLength = (remainingData < c) ? remainingData : c;
    cond = exportData(object, item, src, dst,
		      exportLength, byteOrder, &rtnLength);
    if (cond != DCM_NORMAL)
      return cond;

    src += rtnLength;
    dst += rtnLength;
    bytesExported += rtnLength;
    c -= rtnLength;

    if (c <= 20) {
      cond = callback(buffer, bytesExported, 0, ctx);
      if (cond != DCM_NORMAL) {
	return COND_PushCondition(DCM_CALLBACKABORTED,
	      DCM_Message(DCM_CALLBACKABORTED), "exportPixels");
      }
      bytesExported = 0;
      c = bufferlength;
      dst = (unsigned char *) buffer;
    }
  }
  if (bytesExported > 0) {
    cond = callback(buffer, bytesExported, 0, ctx);
    if (cond != DCM_NORMAL) {
      return COND_PushCondition(DCM_CALLBACKABORTED,
	      DCM_Message(DCM_CALLBACKABORTED), "exportPixels");
    }
  }

  return DCM_NORMAL;

#if 0
  if (element->d.ot == NULL) {
    if ((*object)->fd != -1) {
      /* Seek to the beginning of the data. Have to back up 12 bytes to
      ** get the pixel tag, VR, etc
      */
      (void) lseek((*object)->fd, item->dataOffset-12, SEEK_SET);
    } else {
      (*object)->sk((*object)->userCtx, item->dataOffset-12, SEEK_SET);
    }

    toExport = item->originalDataLength + 12;
    while(toExport > 0) {
      length = (toExport < bufferlength) ? toExport : bufferlength;

      if ((*object)->fd != -1) {
	nBytes = read((*object)->fd, buffer, length);
      } else {
	cond = (*object)->rd((*object)->userCtx, buffer, (long) length, &nBytes);
      }
      if ((U32) nBytes != length) {
	char b[512];
	sprintf(b, "byte count: %d %d, errno: %d", nBytes, length, errno);
	(void) COND_PushCondition(DCM_GENERALWARNING,
			  DCM_Message(DCM_GENERALWARNING), "exportPixels", b);
	return COND_PushCondition(DCM_FILEACCESSERROR,
		      DCM_Message(DCM_FILEACCESSERROR), (*object)->fileName,
				      "exportPixels");
      }
      cond = callback(buffer, length, 0, ctx);
      if (cond != DCM_NORMAL) {
	return COND_PushCondition(DCM_CALLBACKABORTED,
	      DCM_Message(DCM_CALLBACKABORTED), "exportStream");
      }
      toExport -= length;
    }
  } else {
  }
  return DCM_NORMAL;
#endif

}

/* fileSize
**
** Purpose:
**	Determine the file size of a file on an open descriptor.
**
** Parameter Dictionary:
**	fd	File descriptor for an open file.
**
** Return Values:
**	the size of the open file in bytes (nonnegative)
**	a negative status value returned by fstat
**
** Algorithm:
**	call unix fstat system call to get file size.
**	if successful call
**	    return file size
**	else
**	    return status returned by fstat call (-1)
*/
#ifdef MACOS
static long
#else
static int
#endif
fileSize(int fd)
{
    int
        status;
    struct stat
        im_stat;

    status = fstat(fd, &im_stat);
    if (status < 0) {
	return status;
    } else
	return im_stat.st_size;
}

/* swapInPlace
**
** Purpose:
**	Swap data in place for byte order adjustment.  Bytes are swapped
**	for data with representations of DCM_US and DCM_UL (binary values).
**	Special care is taken with pixel data which may be 8 bits.
**
** Parameter Dictionary:
**	object		Pointer to caller's DCM object containing the
**			element with the data to be swapped
**	element		Pointer to DCM_ELEMENT that contains the data to be
**			swapped.
**
** Return Values:
**	None
**
** Algorithm:
**	If (element->representation is 16 bit binary)
**	    If (element is pixel data and pixel data is not 16 bits)
**		return
**	    Swap in place short integers for this element.
**	Else if (element->representation is 32 bit binary)
**	    Swap in place long integers for this element
*/

static void
swapInPlace(PRIVATE_OBJECT ** object, DCM_ELEMENT * e)
{
    U32
    length;
    unsigned char
        tmp,
       *p1;

    length = e->length;
    p1 = e->d.ot;
    if (e->representation == DCM_US || e->representation == DCM_SS ||
	e->representation == DCM_OW || e->representation == DCM_AT) {
	if (e->tag == DCM_PXLPIXELDATA &&
	    (*object)->pixelBitsAllocated != 16)
	    return;

	while (length > 0) {
	    tmp = p1[0];
	    p1[0] = p1[1];
	    p1[1] = tmp;
	    p1 += 2;
	    length -= 2;
	}
    } else if (e->representation == DCM_UL || e->representation == DCM_SL) {
	while (length > 0) {
	    tmp = p1[0];
	    p1[0] = p1[3];
	    p1[3] = tmp;
	    tmp = p1[1];
	    p1[1] = p1[2];
	    p1[2] = tmp;
	    length -= 4;
	    p1 += 4;
	}
    }
}


/* checkObject
**
** Purpose:
**	Examine a PRIVATE OBJECT to see if it looks like is has the proper
**	fields defined.  This function is used to try to make certain that
**	users call the DCM routines with the proper objects.  If the object
**	is legal, the function returns DCM_NORMAL.  If the object is not
**	legal, the function will return an error.
**
** Parameter Dictionary:
**	object		PRIVATE_OBJECT to be examined by this function
**	caller		Name of the function (ASCIZ) that called this
**			function.  In case of failure, this becomes part of
**			the error message that is pushed on the stack.
**
** Return Values:
**	DCM_NORMAL
**	DCM_NULLOBJECT
**	DCM_ILLEGALOBJECT
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

static CONDITION
checkObject(PRIVATE_OBJECT ** object, char *caller)
{
    if (object == NULL)
	return COND_PushCondition(DCM_NULLOBJECT, DCM_Message(DCM_NULLOBJECT),
				  caller);
    if (*object == NULL)
	return COND_PushCondition(DCM_NULLOBJECT, DCM_Message(DCM_NULLOBJECT),
				  caller);

    if (strcmp((*object)->keyType, KEY_DCM_OBJECT) != 0)
	return COND_PushCondition(DCM_ILLEGALOBJECT,
				  DCM_Message(DCM_ILLEGALOBJECT), caller);
    return DCM_NORMAL;
}


/* writeFile
**
** Purpose:
**	Write the data in the buffer into the file specified by the file
**	descriptor
**
** Parameter Dictionary:
**	buffer		Buffer holding the information to be written
**	length		Length of the buffer
**	flag		Unused
**	fd		File descriptor
**
** Return Values:
**
**	DCM_FILEIOERROR
**	DCM_NORMAL
**
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

static CONDITION
writeFile(void *buffer, U32 length, int flag,
	  void /* int */ *fdPtr)
{
    int
        bytesWritten;
    int *fd;

    fd = (int *) fdPtr;

    bytesWritten = write(*fd, buffer, (int) length);
    if (bytesWritten != (int) length)
	return COND_PushCondition(DCM_FILEIOERROR,
			  DCM_Message(DCM_FILEIOERROR), "", strerror(errno),
				  "writeFile");
    else
	return DCM_NORMAL;
}

static CONDITION
countBytes(void *buffer, U32 length, int flag,
	   void /* unsigned long */ *sizePtr)
{
    unsigned long *size;

    size = (unsigned long *) sizePtr;

    *size += length;

    return DCM_NORMAL;
}

static CONDITION
setFileOptions(DCM_OBJECT ** obj, unsigned long *opt)
{
    CONDITION cond;
    char xferSyntax[DICOM_UI_LENGTH + 1];
    DCM_ELEMENT e = {DCM_METATRANSFERSYNTAX, DCM_UI, "", 1, sizeof(xferSyntax),
    NULL};

    e.d.string = xferSyntax;
    cond = DCM_ParseObject(obj, &e, 1, NULL, 0, NULL);
    if (cond != DCM_NORMAL)
	return cond;

    *opt = 0;
    if (strcmp(xferSyntax, DICOM_TRANSFERLITTLEENDIAN) == 0) {
	*opt = DCM_ORDERLITTLEENDIAN;
    } else if (strcmp(xferSyntax, DICOM_TRANSFERLITTLEENDIANEXPLICIT) == 0) {
	*opt = DCM_EXPLICITLITTLEENDIAN;
    } else if (strcmp(xferSyntax, DICOM_TRANSFERBIGENDIANEXPLICIT) == 0) {
	*opt = DCM_EXPLICITBIGENDIAN;
    } else {	/* Must be an encapsulated xfer syntax */
	*opt = DCM_ENCAPSULATEDPIXELS;
    }

    return DCM_NORMAL;
}

static CONDITION
extractFileOptions(unsigned long opt, CTNBOOLEAN * part10File,
		   CTNBOOLEAN * explicitVR, int *byteOrder,
		   CTNBOOLEAN* encapsulatedPixels)
{
    *part10File = *explicitVR = FALSE;

    if ((opt & DCM_FILEFORMATMASK) == DCM_PART10FILE) {
	*part10File = TRUE;
	opt &= ~DCM_ORDERMASK;
	opt |= DCM_EXPLICITLITTLEENDIAN;
    }
    if ((opt & DCM_ORDERMASK) == 0)
	return COND_PushCondition(DCM_ILLEGALOPTION,
			       DCM_Message(DCM_ILLEGALOPTION), "Byte order",
				  "extractFileOptions");

    switch (opt & DCM_ORDERMASK) {
    case DCM_ORDERNATIVE:
	*byteOrder = NATIVE_ORDER;
	*encapsulatedPixels = FALSE;
	break;
    case DCM_ORDERLITTLEENDIAN:
	*byteOrder = LITTLE_ORDER;
	*encapsulatedPixels = FALSE;
	break;
    case DCM_EXPLICITLITTLEENDIAN:
	*byteOrder = LITTLE_ORDER;
	*explicitVR = TRUE;
	*encapsulatedPixels = FALSE;
	break;
    case DCM_ORDERBIGENDIAN:
	*byteOrder = BIG_ORDER;
	*encapsulatedPixels = FALSE;
	break;
    case DCM_EXPLICITBIGENDIAN:
	*byteOrder = BIG_ORDER;
	*explicitVR = TRUE;
	*encapsulatedPixels = FALSE;
	break;
    case DCM_ENCAPSULATEDPIXELS:
	*byteOrder = LITTLE_ORDER;
	*explicitVR = TRUE;
	*encapsulatedPixels = TRUE;
	break;
    default:
	*byteOrder = LITTLE_ORDER;
	*encapsulatedPixels = FALSE;
	break;
    }

    return DCM_NORMAL;
}

static U32
computeGroupLength(PRV_GROUP_ITEM * groupItem,
		   CTNBOOLEAN explicitVR)
{
    return (explicitVR) ?
    groupItem->baseLength + 4 * groupItem->longVRAttributes :
    groupItem->baseLength;

}

/* exportStream
**
** Purpose:
**      Export a DICOM object into the stream format suitable
**      for network transmission or disk storage.
**
** Parameter Dictionary:
**	callerObject		Handle to caller's DICOM object
**	opt			Bit mask giving options for exporting data
**	buffer			Pointer to caller's buffer to hold next chunk
**				of DCM stream data
**	bufferlength		Length of caller's buffer to hold stream data
**	callback		Callback routine to be called.
**	ctx			Pointer to context variable we maintain to keep
**				track of our location in export process.
**	sequenceLevel		Current level in the sequence hierarchy
**
** Return Values:
**
**	DCM_FILEACCESSERROR
**	DCM_ILLEGALOBJECT
**	DCM_LISTFAILURE
**	DCM_NORMAL
**	DCM_NULLOBJECT
**	DCM_CALLBACKABORTED
**
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

static CONDITION
exportStream(DCM_OBJECT ** callerObject, unsigned long opt,
	     void *buffer, U32 bufferlength, DCM_EXPORT_STREAM_CALLBACK* callback,
	     void *ctx, int sequenceLevel)
{
    PRIVATE_OBJECT
    ** object;
    PRV_GROUP_ITEM
	* groupItem;
    PRV_ELEMENT_ITEM
	* elementItem;
    DCM_ELEMENT
	element;
    int
        byteOrder;
    int
        lastFlag = 0;
    unsigned char
       *src,
       *dst;
    U32
	c,
	bytesExported = 0,
	rtnLength,
	remainingData,
	exportLength;
    CONDITION
	cond;
    DCM_SEQUENCE_ITEM
	* sequenceItem;
    DCM_ELEMENT
	itemMarker = {
	DCM_DLMITEM, DCM_DLM, "", 1, DCM_UNSPECIFIEDLENGTH, NULL
    },
	itemDelimiter = {
	DCM_DLMITEMDELIMITATIONITEM, DCM_DLM, "", 1, 0, NULL
    },
	sequenceDelimiter = {
	DCM_DLMSEQUENCEDELIMITATIONITEM, DCM_DLM, "", 1, 0, NULL
    };
    CTNBOOLEAN
	unspecifiedSQLength = FALSE,
	explicitVR = FALSE,
	part10File = FALSE,
	encapsulatedPixels = FALSE;
    unsigned long fileOptions = 0;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "exportStream");
    if (cond != DCM_NORMAL)
	return cond;

    if ((opt & DCM_FILEFORMATMASK) == DCM_PART10FILE) {
	part10File = TRUE;
	opt &= ~DCM_ORDERMASK;
	opt |= DCM_EXPLICITLITTLEENDIAN;
	cond = setFileOptions(callerObject, &fileOptions);
	if (cond != DCM_NORMAL)
	    return cond;
    }
    if ((opt & DCM_ORDERMASK) == 0)
	return COND_PushCondition(DCM_ILLEGALOPTION,
			       DCM_Message(DCM_ILLEGALOPTION), "Byte order",
				  "exportStream");

    switch (opt & DCM_ORDERMASK) {
    case DCM_ORDERNATIVE:
	byteOrder = NATIVE_ORDER;
	break;
    case DCM_ORDERLITTLEENDIAN:
	byteOrder = LITTLE_ORDER;
	break;
    case DCM_EXPLICITLITTLEENDIAN:
	byteOrder = LITTLE_ORDER;
	explicitVR = TRUE;
	break;
    case DCM_ORDERBIGENDIAN:
	byteOrder = BIG_ORDER;
	break;
    case DCM_EXPLICITBIGENDIAN:
	byteOrder = BIG_ORDER;
	explicitVR = TRUE;
	break;
    case DCM_ENCAPSULATEDPIXELS:
	byteOrder = LITTLE_ORDER;
	explicitVR = TRUE;
	encapsulatedPixels = TRUE;
	break;
    default:
	byteOrder = LITTLE_ORDER;
	break;
    }

/*  We are making this step mandatory for now (smm)*/

    opt &= ~DCM_SEQUENCELENGTHMASK;
    opt |= DCM_UNSPECIFIEDLENGTHFLAG;

/*  End of something that is out of place */

    if ((opt & DCM_SEQUENCELENGTHMASK) == DCM_UNSPECIFIEDLENGTHFLAG)
	unspecifiedSQLength = TRUE;

    dst = (unsigned char *) buffer;
    c = bufferlength;

    if (part10File) {
	cond = exportPreamble(object, dst, c, &rtnLength);
	if (cond != DCM_NORMAL)
	    return cond;

	dst += rtnLength;
	c -= rtnLength;
	bytesExported += rtnLength;
    }
    if (sequenceLevel != 0) {
	if (!unspecifiedSQLength)
	    itemMarker.length = (*object)->objectSize;
	exportFixedFields(&itemMarker, dst, bufferlength, byteOrder,
			  explicitVR, &rtnLength);
	dst += rtnLength;
	c -= rtnLength;
	bytesExported += rtnLength;
    }
    groupItem = (void *)LST_Head(&(*object)->groupList);

/*  Take this code out to allow empty groups. */
#if 0
    if (groupItem == NULL)
	return COND_PushCondition(DCM_LISTFAILURE,
			      DCM_Message(DCM_LISTFAILURE), "exportStream");
#endif
    if (groupItem != NULL)
	(void) LST_Position(&(*object)->groupList, (void *)groupItem);

    while (groupItem != NULL) {
	if (part10File && groupItem->group != DCM_GROUPFILEMETA) {
	    if (opt != fileOptions) {
		opt = fileOptions;
		cond = extractFileOptions(opt, &part10File,
					  &explicitVR, &byteOrder,
					  &encapsulatedPixels);
		if (cond != DCM_NORMAL)
		    return cond;
	    }
	}
	elementItem = (void *)LST_Head(&groupItem->elementList);
	if (elementItem != NULL)
	    (void) LST_Position(&groupItem->elementList, (void *)elementItem);
	if (DCM_TAG_ELEMENT(elementItem->element.tag) == 0x0000) {
	    U32 l;
	    l = computeGroupLength(groupItem, explicitVR);
	    *elementItem->element.d.ul = l;

/* We have some problems computing group length for groups with sequences.
** For now, just remove this attribute, except for group 0000 and 0002.
*/
	    if (groupItem->group != 0x0000 && groupItem->group != 0x0002)
		elementItem = (void *)LST_Next(&groupItem->elementList);
	}
	while (elementItem != NULL) {
	    if (c <= 20) {
		cond = callback(buffer, bytesExported, 0, ctx);
		if (cond != DCM_NORMAL)
		    return COND_PushCondition(DCM_CALLBACKABORTED,
			  DCM_Message(DCM_CALLBACKABORTED), "exportStream");

		bytesExported = 0;
		c = bufferlength;
		dst = (unsigned char *) buffer;
	    }
	    element = elementItem->element;

	    if (element.tag == DCM_PXLPIXELDATA) {
		/* Lots of special rules for pixel data. Handle separately */
		/* First, dump the current buffer */
		cond = callback(buffer, bytesExported, 0, ctx);
		if (cond != DCM_NORMAL)
		    return COND_PushCondition(DCM_CALLBACKABORTED,
			  DCM_Message(DCM_CALLBACKABORTED), "exportStream");

		cond = exportPixels(object, elementItem, encapsulatedPixels,
			buffer, bufferlength, callback, ctx, byteOrder, explicitVR);
		if (cond != DCM_NORMAL)
		    return cond;

		bytesExported = 0;
		c = bufferlength;
		dst = (unsigned char *) buffer;
		rtnLength = 0;
	    } else if (element.representation == DCM_SQ) {
		if (unspecifiedSQLength)
		    element.length = DCM_UNSPECIFIEDLENGTH;

		exportFixedFields(&element, dst, bufferlength, byteOrder,
				  explicitVR, &rtnLength);
	    } else {
		element.length = elementItem->paddedDataLength;
		exportFixedFields(&element, dst, bufferlength, byteOrder,
				  explicitVR, &rtnLength);
	    }
	    dst += rtnLength;
	    c -= rtnLength;
	    bytesExported += rtnLength;

	    remainingData = element.length;
	    src = element.d.ot;
	    elementItem->currentOffset = elementItem->dataOffset;

	    if (element.tag == DCM_PXLPIXELDATA) {
		/* Then, we did that above */
		;
	    } else if (element.representation == DCM_SQ) {

		cond = callback(buffer, bytesExported, 0, ctx);
		if (cond != DCM_NORMAL)
		    return COND_PushCondition(DCM_CALLBACKABORTED,
			  DCM_Message(DCM_CALLBACKABORTED), "exportStream");

		bytesExported = 0;
		c = bufferlength;
		dst = (unsigned char *) buffer;

		if (element.d.sq != NULL) {
		    sequenceItem = (void *)LST_Head(&element.d.sq);
		    if (sequenceItem != NULL)
			(void) LST_Position(&element.d.sq, (void *)sequenceItem);
		    while (sequenceItem != NULL) {
			cond = exportStream(&sequenceItem->object, opt,
					buffer, bufferlength, callback, ctx,
					    sequenceLevel + 1);
			if (cond != DCM_NORMAL)
			    return cond;
			sequenceItem = (void *)LST_Next(&element.d.sq);
		    }
		}
		if (element.length == DCM_UNSPECIFIEDLENGTH) {
		    sequenceDelimiter.length = 0;
		    exportFixedFields(&sequenceDelimiter, dst, bufferlength,
				      byteOrder, explicitVR, &rtnLength);
		    dst += rtnLength;
		    c -= rtnLength;
		    bytesExported += rtnLength;
		}
	    } else {
		while (remainingData > 0) {
		    if (debug)
			fprintf(stderr, "Export: (%08x) %d\n",
				element.tag, element.length);
		    if (element.d.ot != NULL)
			remainingData = element.length -
			    (src - ((unsigned char *) element.d.ot));
		    else
			remainingData = element.length -
			    (elementItem->currentOffset - elementItem->dataOffset);

		    exportLength = (remainingData < c) ? remainingData : c;
		    cond = exportData(object, elementItem, src, dst,
				      exportLength, byteOrder, &rtnLength);
		    if (cond != DCM_NORMAL)
			return cond;

		    src += rtnLength;
		    dst += rtnLength;
		    bytesExported += rtnLength;
		    c -= rtnLength;

		    if (c <= 20) {
			cond = callback(buffer, bytesExported, 0, ctx);
			if (cond != DCM_NORMAL)
			    return COND_PushCondition(DCM_CALLBACKABORTED,
						      DCM_Message(DCM_CALLBACKABORTED), "exportStream");

			bytesExported = 0;
			c = bufferlength;
			dst = (unsigned char *) buffer;
		    }
		}
	    }
	    elementItem = (void *)LST_Next(&groupItem->elementList);
	}
	groupItem = (void *)LST_Next(&(*object)->groupList);
    }
    if ((sequenceLevel != 0) && unspecifiedSQLength) {
	if (c <= 20) {
	    cond = callback(buffer, bytesExported, 0, ctx);
	    if (cond != DCM_NORMAL)
		return COND_PushCondition(DCM_CALLBACKABORTED,
			  DCM_Message(DCM_CALLBACKABORTED), "exportStream");

	    bytesExported = 0;
	    c = bufferlength;
	    dst = (unsigned char *) buffer;
	}
	exportFixedFields(&itemDelimiter, dst, bufferlength, byteOrder,
			  explicitVR, &rtnLength);
	dst += rtnLength;
	c -= rtnLength;
	bytesExported += rtnLength;
    }
    lastFlag = (sequenceLevel == 0) ? 1 : 0;
    cond = callback(buffer, bytesExported, lastFlag, ctx);
    if (cond != DCM_NORMAL)
	return COND_PushCondition(DCM_CALLBACKABORTED,
			  DCM_Message(DCM_CALLBACKABORTED), "exportStream");

    return DCM_NORMAL;
}

/* verifyFormat
**
** Purpose:
**  This routine verifies the format of the data value of attributes according to
**  the DICOM v3 standard.
**
** Parameter Dictionary:
**   element    Pointer to the DCM_ELEMENT containing the element to be examined.
**
** Return Values:
**	DCM_NORMAL
**
** Algorithm:
**	switch(representation) {
**	case DCM_DA:
**	    Retain all characters that are digits, '-', or '\'
**	    If the resulting string is of odd length
**		Pad the string with ' '
**	    break;
**	case DCM_TM:
**	    Retain all characters that are digits, '.', '-', or '\'
**	    If the resulting string is of odd length
**		Pad the string with ' '
**	    break;
**	case DCM_CS, DCM_AS, DCM_DS, DCM_IS, DCM_LO, DCM_SH, DCM_UT:
**	    Delete all the leading and trailing spaces.
**	    If the resulting string is of odd length
**		Pad the string with ' '
**	    break;
**	case DCM_LT, DCM_ST, DCM_PN:
**	    Delete all the trailing spaces.
**	    If the resulting string is of odd length
**		Pad the string with ' '
**	    break;
**	}
*/

static CONDITION
verifyFormat(PRV_ELEMENT_ITEM * item)
{
    int
        i,
        l;
    char
       *src,
       *dst,
       *p;
    DCM_ELEMENT
	* element;
    CTNBOOLEAN
	stopFlag = FALSE;

    element = &item->element;
    if (element->length > 0) {
	switch (element->representation) {
	case DCM_DA:
	    src = dst = element->d.string;
	    l = (int) element->length;
	    for (i = 0; i < l; i++) {
		if (isdigit(*src) || (*src == '-') || (*src == '\\')) {
		    *dst++ = *src++;
		} else {
		    src++;
		    element->length--;
		}
	    }
	    item->paddedDataLength = element->length;
	    if (element->length & 1) {
		*dst = ' ';
		item->paddedDataLength++;
	    }
	    break;
	case DCM_TM:
	    l = (int) element->length;
	    src = dst = element->d.string;
	    for (i = 0; i < l; i++) {
		if (isdigit(*src) || (*src == '.') || (*src == '-') || (*src == '\\')) {
		    *dst++ = *src++;
		} else {
		    src++;
		    element->length--;
		}
	    }
	    item->paddedDataLength = element->length;
	    if (element->length & 1) {
		*dst = ' ';
		item->paddedDataLength++;
	    }
	    break;
	    /*
	     * Both the leading and trailing spaces are non-significant.
	     */
	case DCM_CS:
	case DCM_AS:
	case DCM_DS:
	case DCM_IS:
	case DCM_LO:
	case DCM_SH:
	case DCM_UT:
	    l = (int) element->length;
	    src = dst = element->d.string;
	    for (i = 0; i < l; i++) {
		if ((*src == ' ') && !stopFlag) {
		    src++;
		    element->length--;
		} else {
		    stopFlag = TRUE;
		    *dst++ = *src++;
		}
	    }
	    /*
	     * Right now, dst points to the char follows the last char in the
	     * string.
	     */
	    stopFlag = FALSE;
	    l = (int) element->length;
	    p = dst - 1;
	    for (i = l; (i > 0) && !stopFlag; i--) {
		if ((*p == ' ') && !stopFlag) {
		    p--;
		    dst--;
		    element->length--;
		} else
		    stopFlag = TRUE;
	    }
	    item->paddedDataLength = element->length;
	    if (element->length & 1) {
		*dst = ' ';
		item->paddedDataLength++;
	    }
	    break;
	    /*
	     * The trailing spaces are non-significant.
	     */
	case DCM_LT:
	case DCM_ST:
	    l = (int) element->length;
	    src = element->d.string + l - 1;
	    for (i = l; (i > 0) && !stopFlag; i--) {
		if ((*src == ' ') && !stopFlag) {
		    src--;
		    element->length--;
		} else
		    stopFlag = TRUE;
	    }
	    item->paddedDataLength = element->length;
	    if (element->length & 1) {
		*++src = ' ';
		item->paddedDataLength++;
	    }
	    break;
	case DCM_PN:
	    /*
	     * Strip off the trailing spaces.
	     */
	    l = (int) element->length;
	    src = element->d.string + l - 1;
	    for (i = l; (i > 0) && !stopFlag; i--) {
		if ((*src == ' ') && !stopFlag) {
		    src--;
		    element->length--;
		} else
		    stopFlag = TRUE;
	    }
	    /*
	     * Convert the name to the standard V3 format.
	     */
	    src = dst = element->d.string;
	    l = element->length;
	    for (i = 0; i < l;) {
		if ((src[i] == ',') || (src[i] == '^')) {
		    *dst++ = '^';
		    i++;
		    while ((i < l) && (src[i] == ' ')) {
			element->length--;
			i++;
		    }
		} else {
		    *dst++ = src[i++];
		}
	    }

	    item->paddedDataLength = element->length;
	    if (element->length & 1) {
		*dst = ' ';
		item->paddedDataLength++;
	    }
	    break;
	case DCM_UI:
	    if (element->d.string[element->length - 1] == '\0')
		element->length--;
	    if (element->d.string[element->length - 1] == ' ') {
		element->d.string[element->length - 1] = '\0';
		element->length--;
	    }
	    break;
	default:
	    break;
	}
    }
    return DCM_NORMAL;
}

/* readFile
**
** Purpose:
**	Read DICOM object from a file
**
** Parameter Dictionary:
**	name			Name of the file
**	callerBuf		Buffer from which to read the object
**	fd			File descriptor
**	size			Size of the file
**	fileOffset		Offset in the file from which point read starts
**	recursionLevel		Level of recursion
**	opt			Indicates in what byte order to read
**	callerObject		The object into which the contents are stored
**	scannedLength		Length of data scanned
**	remainOpenFlag		Indicates whether the file remains open
**
** Return Values:
**
**	DCM_ELEMENTCREATEFAILED
**	DCM_ELEMENTLENGTHERROR
**	DCM_ELEMENTOUTOFORDER
**	DCM_FILEACCESSERROR
**	DCM_ILLEGALSTREAMLENGTH
**	DCM_LISTFAILURE
**	DCM_NORMAL
**	DCM_OBJECTCREATEFAILED
**	DCM_UNEVENELEMENTLENGTH
**
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/
static CONDITION
readFile(char *name, unsigned char *callerBuf, int fd, long size,
	 off_t fileOffset, int recursionLevel,
	 unsigned long opt, DCM_OBJECT ** callerObject,
	 U32 * scannedLength, CTNBOOLEAN * remainOpenFlag,
	 void *ctx,
	 CONDITION(*rd) (void *ctx, void *buf, int toRead, int *bytesRead),
	 CONDITION(*sk) (void *ctx, int offset, int flag))
{
    CONDITION
    cond;
    int
        byteOrder;
    long
        lastGroup = -1,
        lastElement = -1;
    U32
	sequenceLength,
	localLength;
    PRIVATE_OBJECT
	** object;
    PRV_GROUP_ITEM
	* groupItem = NULL;
    unsigned short
        group,
        element,
        tagGroup,
        tagElement;
    DCM_ELEMENT
	e,
	tagE;
    CTNBOOLEAN
	pixelFlag,
	convertFlag = FALSE,
	done = FALSE,
	knownLength = TRUE,
	sequenceDone = FALSE,
	createGroupFlag,
	explicitVR = FALSE;
    unsigned char
        buf[8],
       *ptr;
    int
        nBytes;
    PRV_ELEMENT_ITEM
	* elementItem = NULL;
    DCM_OBJECT
	* sequenceObject;
    DCM_SEQUENCE_ITEM
	* sequenceItem;
    CTNBOOLEAN
	fileFlag = TRUE;

    if (callerBuf != NULL) {
	ptr = callerBuf;
	fileFlag = FALSE;
    } else
	ptr = buf;

    switch (opt & DCM_ORDERMASK) {
    case DCM_ORDERNATIVE:
	byteOrder = NATIVE_ORDER;
	break;
    case DCM_ORDERLITTLEENDIAN:
	byteOrder = LITTLE_ORDER;
	break;
    case DCM_EXPLICITLITTLEENDIAN:
	byteOrder = LITTLE_ORDER;
	explicitVR = TRUE;
	break;
    case DCM_ORDERBIGENDIAN:
	byteOrder = BIG_ORDER;
	break;
    case DCM_EXPLICITBIGENDIAN:
	byteOrder = BIG_ORDER;
	explicitVR = TRUE;
	break;
    default:
	byteOrder = NATIVE_ORDER;
	break;
    }
    if ((opt & DCM_CONVERTMASK) == DCM_FORMATCONVERSION)
	convertFlag = TRUE;

    if (scannedLength != NULL)
	*scannedLength = 0;

    cond = DCM_CreateObject(callerObject, opt);
    if (cond != DCM_NORMAL)
	return cond;

    object = (PRIVATE_OBJECT **) callerObject;
    if (fileFlag)
	strcpy((*object)->fileName, name);

    (*object)->fd = -1;
    (*object)->rd = rd;
    (*object)->sk = sk;
    (*object)->userCtx = ctx;
    if (size == (long) DCM_UNSPECIFIEDLENGTH)
	knownLength = FALSE;

    if ((fileFlag) && ((opt & DCM_DELETEMASK) == DCM_DELETEONCLOSE) && (recursionLevel == 0))
	(*object)->deleteFlag = TRUE;

    if (knownLength && (size == 0))
	done = TRUE;

    while (!done) {

	if ((size < 8) && knownLength) {
	    if (debug)
		(void) DCM_DumpElements(callerObject, 0);
	    (void) DCM_CloseObject(callerObject);
	    return COND_PushCondition(DCM_ILLEGALSTREAMLENGTH,
				 DCM_Message(DCM_ILLEGALSTREAMLENGTH), size,
				      "readFile");
	}
	if (fileFlag) {
	    if (fd != -1) {
		nBytes = read(fd, buf, 4);
	    } else {
		cond = (*object)->rd((*object)->userCtx, buf, 4, &nBytes);
	    }

	    if (nBytes != 4)
		return COND_PushCondition(DCM_FILEACCESSERROR,
				     DCM_Message(DCM_FILEACCESSERROR), name,
					  "readFile");
	    ptr = buf;
	}
	if (knownLength)
	    size -= 4;
	fileOffset += (off_t) 4;
	if (scannedLength != NULL)
	    (*scannedLength) += 4;
	(*object)->objectSize += 4;

	if (byteOrder == BYTEORDER_SAME) {
	    GET_SHORT_SAME_ORDER(ptr, group);
	    GET_SHORT_SAME_ORDER(ptr + 2, element);
	    e.tag = DCM_MAKETAG(group, element);
	} else {
	    GET_SHORT_REVERSE_ORDER(ptr, group);
	    GET_SHORT_REVERSE_ORDER(ptr + 2, element);
	    e.tag = DCM_MAKETAG(group, element);
	}
	ptr += 4;

	if (explicitVR) {
	    if (fileFlag) {
		if (fd != -1) {
		    nBytes = read(fd, buf, 4);
		} else {
		    cond = (*object)->rd((*object)->userCtx, buf, 4, &nBytes);
		}

		if (nBytes != 4)
		    return COND_PushCondition(DCM_FILEACCESSERROR,
				     DCM_Message(DCM_FILEACCESSERROR), name,
					      "readFile");
		ptr = buf;
	    }
	    if (knownLength)
		size -= 4;
	    fileOffset += (off_t) 4;
	    if (scannedLength != NULL)
		(*scannedLength) += 4;
	    (*object)->objectSize += 4;
	    if ((strncmp((char *) ptr, "OB", 2) == 0) ||
		(strncmp((char *) ptr, "OW", 2) == 0) ||
		(strncmp((char *) ptr, "SQ", 2) == 0)) {
	    } else {
	    }
	} else {

	    if (fileFlag) {
		if (fd != -1) {
		    nBytes = read(fd, buf, 4);
		} else {
		    cond = (*object)->rd((*object)->userCtx, buf, 4, &nBytes);
		}

		if (nBytes != 4)
		    return COND_PushCondition(DCM_FILEACCESSERROR,
				     DCM_Message(DCM_FILEACCESSERROR), name,
					      "readFile");
		ptr = buf;
	    }
	    if (knownLength)
		size -= 4;
	    fileOffset += (off_t) 4;
	    if (scannedLength != NULL)
		(*scannedLength) += 4;
	    (*object)->objectSize += 4;


	    if (byteOrder == BYTEORDER_SAME) {
		GET_LONG_SAME_ORDER(ptr, e.length);
	    } else {
		GET_LONG_REVERSE_ORDER(ptr, e.length);
	    }
	    ptr += 4;
	}

	if (((e.length & 1) != 0) && (e.length != DCM_UNSPECIFIEDLENGTH)) {
	    if (debug)
		(void) DCM_DumpElements(callerObject, 0);
	    (void) DCM_CloseObject(callerObject);
	    return COND_PushCondition(DCM_UNEVENELEMENTLENGTH,
				      DCM_Message(DCM_UNEVENELEMENTLENGTH),
				      group, element, e.length,
				      "readFile");
	}
	if ((e.length != (U32) DCM_UNSPECIFIEDLENGTH) && (e.length > (U32) size)) {
	    if (debug)
		(void) DCM_DumpElements(callerObject, 0);
	    (void) DCM_CloseObject(callerObject);
	    return COND_PushCondition(DCM_ELEMENTLENGTHERROR,
				      DCM_Message(DCM_ELEMENTLENGTHERROR),
				group, element, e.length, size, "readFile");
	}
	if ((e.tag == DCM_DLMITEMDELIMITATIONITEM) ||
	    (e.tag == DCM_DLMSEQUENCEDELIMITATIONITEM)) {
	    return DCM_NORMAL;
	}
	cond = DCM_LookupElement(&e);
	if (cond != DCM_NORMAL)
	    (void) COND_PopCondition(0);
	if (e.representation == DCM_CTX)
	    ctxSensitiveLookup(object, &e);

	if (e.representation == DCM_SQ) {
	    cond = newElementItem(&e, FALSE, &elementItem);
	    if (cond != DCM_NORMAL)
		return cond;
	    elementItem->element.d.sq = LST_Create();
	    if (elementItem->element.d.sq == NULL)
		return COND_PushCondition(DCM_LISTFAILURE,
				  DCM_Message(DCM_LISTFAILURE), "readFile");

	    localLength = elementItem->element.length;
	    sequenceDone = (localLength == 0);

	    while (!sequenceDone) {
		if (debug)
		    fprintf(stderr, "Sequence Length: %d %x\n", localLength,
			    localLength);
		if (fileFlag) {
		    if (fd != -1) {
			nBytes = read(fd, buf, 8);
		    } else {
			cond = (*object)->rd((*object)->userCtx, buf, 8, &nBytes);
		    }
		    if (nBytes != 8)
			return COND_PushCondition(DCM_FILEACCESSERROR,
				     DCM_Message(DCM_FILEACCESSERROR), name,
						  "readFile");
		    ptr = buf;
		}
		if (size != (long) DCM_UNSPECIFIEDLENGTH)
		    size -= 8;
		fileOffset += (off_t) 8;
		if (scannedLength != NULL)
		    (*scannedLength) += 8;
		(*object)->objectSize += 8;
		if (localLength != DCM_UNSPECIFIEDLENGTH)
		    localLength -= 8;

		if (byteOrder == BYTEORDER_SAME) {
		    GET_SHORT_SAME_ORDER(ptr, tagGroup);
		    GET_SHORT_SAME_ORDER(ptr + 2, tagElement);
		    tagE.tag = DCM_MAKETAG(tagGroup, tagElement);
		    GET_LONG_SAME_ORDER(ptr + 4, tagE.length);
		} else {
		    GET_SHORT_REVERSE_ORDER(ptr, tagGroup);
		    GET_SHORT_REVERSE_ORDER(ptr + 2, tagElement);
		    tagE.tag = DCM_MAKETAG(tagGroup, tagElement);
		    GET_LONG_REVERSE_ORDER(ptr + 4, tagE.length);
		}
		ptr += 8;
		if (debug)
		    fprintf(stderr, "Sequence item: %4x %4x %d (%x)\n",
			    tagGroup, tagElement, tagE.length, tagE.length);
		if (tagE.tag == DCM_DLMITEM) {
/*		    if (size != DCM_UNSPECIFIEDLENGTH)
			size -= 8;
*/
/*		    fileOffset += 8;
*/
		    cond = readFile(name,
				    (fileFlag) ? NULL : ptr,
				    fd, tagE.length,
				    fileOffset, recursionLevel + 1, opt,
				    &sequenceObject, &sequenceLength,
				    remainOpenFlag, ctx, rd, sk);
		    if (cond == DCM_NORMAL) {
			sequenceItem = CTN_MALLOC(sizeof(*sequenceItem));
			if (sequenceItem == NULL)
			    return COND_PushCondition(DCM_MALLOCFAILURE,
					     DCM_Message(DCM_MALLOCFAILURE),
					 sizeof(*sequenceItem), "readFile");

			sequenceItem->object = sequenceObject;
			cond = LST_Enqueue(&elementItem->element.d.sq,
					   (void *)sequenceItem);
			if (cond != LST_NORMAL)
			    return COND_PushCondition(DCM_LISTFAILURE,
				  DCM_Message(DCM_LISTFAILURE), "readFile");
			if (size != (long) DCM_UNSPECIFIEDLENGTH)
			    size -= sequenceLength;
			fileOffset += (off_t) sequenceLength;
			if (scannedLength != NULL)
			    *scannedLength += sequenceLength;
			(*object)->objectSize += sequenceLength;
			if (localLength != DCM_UNSPECIFIEDLENGTH)
			    localLength -= sequenceLength;
			ptr += sequenceLength;
		    } else
			return cond;
		} else {
		    sequenceDone = TRUE;
		}
		if (localLength == 0)
		    sequenceDone = TRUE;
	    }
	} else {
	    pixelFlag = (e.tag == DCM_PXLPIXELDATA);
	    cond = newElementItem(&e, (pixelFlag == FALSE), &elementItem);
	    if (cond != DCM_NORMAL) {
		(void) DCM_CloseObject(callerObject);
		return cond;
	    }
	    if (pixelFlag) {
		if (fileFlag)
		    *remainOpenFlag = TRUE;
		elementItem->byteOrder = byteOrder;
		elementItem->dataOffset = fileOffset;
		elementItem->currentOffset = 0;
		if (fileFlag)
		    elementItem->element.d.ot = NULL;
		else
		    elementItem->element.d.ot = (void *) ptr;
		if ((*object)->pixelBitsAllocated == 8)
		    elementItem->element.representation = DCM_OB;
		else
		    elementItem->element.representation = DCM_OW;
		if (fileFlag) {
		    if (fd != -1) {
			(void) lseek(fd, (off_t) elementItem->element.length, SEEK_CUR);
		    } else {
			(*object)->sk((*object)->userCtx,
				      elementItem->element.length, SEEK_CUR);
		    }
		    (*object)->fd = fd;
		}
	    } else {
		if (fileFlag) {
		    if (fd != -1) {
			nBytes = read(fd, elementItem->element.d.ot,
				      (int) elementItem->element.length);
		    } else {
			cond = (*object)->rd((*object)->userCtx,
					     elementItem->element.d.ot,
			       (long) elementItem->element.length, &nBytes);
		    }
		    if (nBytes != (int) elementItem->element.length) {
			(void) DCM_CloseObject(callerObject);
			return COND_PushCondition(DCM_FILEACCESSERROR,
			DCM_Message(DCM_FILEACCESSERROR), name, "readFile");
		    }
		} else {
		    (void) memcpy(elementItem->element.d.ot, ptr,
				  elementItem->element.length);
		    ptr += elementItem->originalDataLength;
		}

                if( LITTLE_ENDIAN_ARCHITECTURE ){
		  if (elementItem->element.representation == DCM_AT)
		    swapATGroupElement(&elementItem->element);
                }
		if (byteOrder != BYTEORDER_SAME)
		    swapInPlace(object, &elementItem->element);
		if (convertFlag) {
		    cond = verifyFormat(elementItem);
		    if (cond != DCM_NORMAL)
			return cond;
		}
	    }
	    if (size != (long) DCM_UNSPECIFIEDLENGTH)
		size -= elementItem->originalDataLength;
	    fileOffset += (off_t) elementItem->originalDataLength;
	    if (scannedLength != NULL)
		(*scannedLength) += elementItem->originalDataLength;

	    elementItem->paddedDataLength = elementItem->element.length;
	    if (elementItem->paddedDataLength & 1)
		elementItem->paddedDataLength += 1;
	    (*object)->objectSize += elementItem->paddedDataLength;
	}

	computeVM(object, &elementItem->element);

	if ((long) DCM_TAG_GROUP(e.tag) == lastGroup) {
	    if ((long) DCM_TAG_ELEMENT(e.tag) <= lastElement)
		return COND_PushCondition(DCM_ELEMENTOUTOFORDER,
					  DCM_Message(DCM_ELEMENTOUTOFORDER),
					  group, element, "readFile");
	} else if ((long) DCM_TAG_GROUP(e.tag) > lastGroup) {
	} else {
	    return COND_PushCondition(DCM_ELEMENTOUTOFORDER,
			 DCM_Message(DCM_ELEMENTOUTOFORDER), group, element,
				      "readFile");
	}
	lastGroup = (long) group;
	lastElement = (long) element;

	if (groupItem == NULL)
	    createGroupFlag = TRUE;
	else if (groupItem->group != group)
	    createGroupFlag = TRUE;
	else
	    createGroupFlag = FALSE;

	if (createGroupFlag == TRUE) {
	    groupItem = CTN_MALLOC(sizeof(*groupItem));
	    if (groupItem == NULL) {
		(void) DCM_CloseObject(callerObject);
		return COND_PushCondition(DCM_ELEMENTCREATEFAILED,
				       DCM_Message(DCM_ELEMENTCREATEFAILED),
					  "readFile",
					  group, 0xffff, sizeof(*groupItem));
	    }
	    groupItem->group = group;
	    groupItem->baseLength = 0;
	    groupItem->longVRAttributes = 0;
	    groupItem->elementList = LST_Create();
	    if (groupItem->elementList == NULL) {
		(void) DCM_CloseObject(callerObject);
		return COND_PushCondition(DCM_LISTFAILURE,
					  DCM_Message(DCM_LISTFAILURE),
					  "readFile");
	    }
	    if (LST_Enqueue(&(*object)->groupList, (void *)groupItem) != LST_NORMAL) {
		(void) DCM_CloseObject(callerObject);
		return COND_PushCondition(DCM_LISTFAILURE,
					  DCM_Message(DCM_LISTFAILURE),
					  "readFile");
	    }
	}
	if (element != 0x0000)
	    groupItem->baseLength += 8 + elementItem->paddedDataLength;
	if ((element == 0x0000) && ((*object)->groupLengthFlag == FALSE)) {
	    CTN_FREE(elementItem);
	} else {
	    cond = LST_Enqueue(&groupItem->elementList, (void *)elementItem);
	    if (cond != LST_NORMAL) {
		(void) DCM_CloseObject(callerObject);
		return COND_PushCondition(DCM_LISTFAILURE,
					  DCM_Message(DCM_LISTFAILURE),
					  "readFile");
	    }
	    cond = updateObjectType(object, &elementItem->element);	/* repair */

	    cond = updateSpecialElements(object, elementItem);	/* repair */
	}

	if (size == 0)
	    done = TRUE;

#ifdef DEBUG
	if (debug) {
/*lint -e644 */
	    (void) fprintf(stderr, "Address: %px Group %2x, element %2x, length %ld ",
			   elementItem,
			   DCM_TAG_GROUP(elementItem->element.tag),
			   DCM_TAG_ELEMENT(elementItem->element.tag),
			   elementItem->element.length);
/*lint +e644 */
	    (void) fprintf(stderr, "Object size: %d\n", (*object)->objectSize);
	}
#endif
    }

    groupItem = (void *)LST_Head(&(*object)->groupList);
    if (groupItem != NULL) {
	(void) LST_Position(&(*object)->groupList, (void *)groupItem);
	while (groupItem != NULL) {
	    elementItem = (void *)LST_Head(&groupItem->elementList);
	    if (elementItem != NULL) {
		if (DCM_TAG_ELEMENT(elementItem->element.tag) == 0x0000) {
		    *elementItem->element.d.ul = groupItem->baseLength;
		}
	    }
	    groupItem = (void *)LST_Next(&(*object)->groupList);
	}
    }
    return DCM_NORMAL;
}

static CONDITION
readPreamble(const char *name, unsigned char **ptr, int fd, U32 * size,
	     off_t * fileOffset, CTNBOOLEAN knownLength,
	     PRIVATE_OBJECT ** object, U32 * scannedLength)
{
    int nBytes,
        tmp;
    CONDITION cond;
    char label[4];

    if (*size == 0)
	return DCM_STREAMCOMPLETE;

    if ((*size < DCM_PREAMBLELENGTH + 4) && knownLength) {
	if (debug)
	    (void) DCM_DumpElements((DCM_OBJECT **) object, 0);
	(void) DCM_CloseObject((DCM_OBJECT **) object);
	return COND_PushCondition(DCM_ILLEGALSTREAMLENGTH,
				DCM_Message(DCM_ILLEGALSTREAMLENGTH), *size,
				  "readPreamble");
    }
    if (*ptr == NULL) {
	if (fd != -1) {
	    nBytes = read(fd, (*object)->preamble, DCM_PREAMBLELENGTH);
	    nBytes += read(fd, label, sizeof(label));
	} else {
	    cond = (*object)->rd((*object)->userCtx, (*object)->preamble,
				 DCM_PREAMBLELENGTH, &nBytes);
	    cond = (*object)->rd((*object)->userCtx, label,
				 sizeof(label), &tmp);
	    nBytes += tmp;
	}

	if (nBytes != DCM_PREAMBLELENGTH + sizeof(label))
	    return COND_PushCondition(DCM_FILEACCESSERROR,
				      DCM_Message(DCM_FILEACCESSERROR), name,
				      "readPreamble");
    } else {
	(void) memcpy((*object)->preamble, *ptr, DCM_PREAMBLELENGTH);
	(void) memcpy(label, (*ptr) + DCM_PREAMBLELENGTH, sizeof(label));
    }

    if (knownLength)
	*size -= DCM_PREAMBLELENGTH + sizeof(label);
    *fileOffset += (off_t) DCM_PREAMBLELENGTH + sizeof(label);
    if (*ptr != NULL)
	*ptr += DCM_PREAMBLELENGTH + sizeof(label);
    (*object)->objectSize += DCM_PREAMBLELENGTH + sizeof(label);

    if (strncmp(label, "DICM", 4) != 0)
	return 0;

    (*object)->preambleFlag = TRUE;
    return DCM_NORMAL;
}


static CONDITION
readGroupElement(const char *name, unsigned char **ptr, int fd, U32 * size,
		 off_t * fileOffset, CTNBOOLEAN knownLength, int byteOrder,
		 CTNBOOLEAN explicitVR, CTNBOOLEAN acceptVRMismatch,
		 PRIVATE_OBJECT ** object, U32 * scannedLength,
		 DCM_ELEMENT * e)
{
    unsigned char *localPtr;
    unsigned char buf[4];
    int nBytes;
    CONDITION cond;
    unsigned short group,
        element;

    if (*size == 0)
	return DCM_STREAMCOMPLETE;

    if ((*size < 4) && knownLength) {
	if (debug)
	    (void) DCM_DumpElements((DCM_OBJECT **) object, 0);
	(void) DCM_CloseObject((DCM_OBJECT **) object);
	return COND_PushCondition(DCM_ILLEGALSTREAMLENGTH,
				DCM_Message(DCM_ILLEGALSTREAMLENGTH), *size,
				  "readFile");
    }
    if (*ptr == NULL) {
	if (fd != -1) {
	    nBytes = read(fd, buf, 4);
	} else {
	    cond = (*object)->rd((*object)->userCtx, buf, 4, &nBytes);
	}

	if (nBytes != 4)
	    return COND_PushCondition(DCM_FILEACCESSERROR,
				      DCM_Message(DCM_FILEACCESSERROR), name,
				      "readGroupElement");
	localPtr = buf;
    } else {
	localPtr = *ptr;
    }

    if (knownLength)
	*size -= 4;
    *fileOffset += (off_t) 4;
    if (scannedLength != NULL)
	(*scannedLength) += 4;
    (*object)->objectSize += 4;

    if (byteOrder == BYTEORDER_SAME) {
	GET_SHORT_SAME_ORDER(localPtr, group);
	GET_SHORT_SAME_ORDER(localPtr + 2, element);
	e->tag = DCM_MAKETAG(group, element);
    } else {
	GET_SHORT_REVERSE_ORDER(localPtr, group);
	GET_SHORT_REVERSE_ORDER(localPtr + 2, element);
	e->tag = DCM_MAKETAG(group, element);
    }
    if (*ptr != NULL)
	*ptr += 4;

    if (debug)
	fprintf(stderr, "%04x %04x ", group, element);

    cond = DCM_LookupElement(e);
    if (cond != DCM_NORMAL)
	(void) COND_PopCondition(0);
    if (e->representation == DCM_CTX)
	ctxSensitiveLookup(object, e);

    return DCM_NORMAL;
}

static CONDITION
readVRLength(const char *name, unsigned char **ptr, int fd, U32 * size,
	     off_t * fileOffset,
	     CTNBOOLEAN knownLength, int byteOrder, CTNBOOLEAN explicitVR,
	     CTNBOOLEAN acceptVRMismatch,
	     PRIVATE_OBJECT ** object, U32 * scannedLength, DCM_ELEMENT * e)
{
    unsigned char *localPtr;
    unsigned char buf[4];
    char vrCode[3];
    VRMAP *vrPtr;
    int nBytes;
    CONDITION cond;
    CTNBOOLEAN calculatedLength = FALSE;

ENTRY("readVRLength") ;

    if (*size == 0)
	RETURN( DCM_STREAMCOMPLETE );

    if ((*size < 4) && knownLength) {
	if (debug)
	    (void) DCM_DumpElements((DCM_OBJECT **) object, 0);
	(void) DCM_CloseObject((DCM_OBJECT **) object);
	RETURN( COND_PushCondition(DCM_ILLEGALSTREAMLENGTH,
				DCM_Message(DCM_ILLEGALSTREAMLENGTH), *size,
				  "readVRLength") );
    }
    if (*ptr == NULL) {
	if (fd != -1) {
	    nBytes = read(fd, buf, 4);
	} else {
	    cond = (*object)->rd((*object)->userCtx, buf, 4, &nBytes);
	}

	if (nBytes != 4)
	    RETURN( COND_PushCondition(DCM_FILEACCESSERROR,
				      DCM_Message(DCM_FILEACCESSERROR), name,
				      "readVRLength") ) ;
	localPtr = buf;
    } else
	localPtr = *ptr;

    if (knownLength)
	*size -= 4;
    *fileOffset += (off_t) 4;
    if (scannedLength != NULL)
	(*scannedLength) += 4;
    (*object)->objectSize += 4;

    e->length = 0;
    if (e->representation == DCM_DLM) {
	explicitVR = FALSE;	/* Special rule for delimitors */
    }

    /* if there is no or no valid vrCode, skip explicitVR */
    if (explicitVR) {
      if ( !buf[0] ) {           /* 22 Mar 2011 [rickr] */
        explicitVR = FALSE;
        fprintf(stderr,
                "** DICOM WARNING, missing VR code in element (%04x,%04x)\n", 
                DCM_TAG_GROUP(e->tag), DCM_TAG_ELEMENT(e->tag) );
      }

      /* do not fail on invalid vfCode, but try to ignore */
      vrCode[0] = buf[0];
      vrCode[1] = buf[1];
      vrCode[2] = '\0';
      vrPtr = lookupVRCode(vrCode);
      if (vrPtr == NULL){
        explicitVR = FALSE;      /* 20 Jan 2012 [rickr] */
        if( rwc_err ){
          fprintf(stderr,
          "** DICOM ERROR: unknown VR code 0x%02x%02x in element (%04x,%04x)\n",
          buf[0], buf[1], DCM_TAG_GROUP(e->tag), DCM_TAG_ELEMENT(e->tag) ) ;
          rwc_err-- ;
        }
      }
    }

    if (explicitVR) {
	vrCode[0] = buf[0];
	vrCode[1] = buf[1];
	vrCode[2] = '\0';
	vrPtr = lookupVRCode(vrCode);
	if (vrPtr == NULL){
            if( rwc_err ){
             fprintf(stderr,"** DICOM ERROR: unknown VR code %s in element (%04x,%04x)\n",  /* RWC */
                     vrCode,DCM_TAG_GROUP(e->tag), DCM_TAG_ELEMENT(e->tag) ) ; rwc_err-- ;
            }
	    RETURN( COND_PushCondition(DCM_UNRECOGNIZEDVRCODE,
				DCM_Message(DCM_UNRECOGNIZEDVRCODE), vrCode,
				      "readVRLength") );
        }

	if (vrPtr->representation != e->representation) {
	    if (vrPtr->representation == DCM_OB) {
		/* This is probably from the waveform supplement where they */
		/* transmit as OB and expect us to pull it out later */
		/* We will just keep our VR which was based on context in */
		/* the object */
		e->representation = vrPtr->representation;
	    } else if (e->representation == DCM_UN ||
		       e->representation == DCM_CTX ||
		       e->representation == DCM_RET ||
		       vrPtr->representation == DCM_OW ||
		       acceptVRMismatch) {	/* Believe input */
		e->representation = vrPtr->representation;
	    } else {
#if 0
		if (e->tag != DCM_PXLPIXELDATA){
                    STATUS("VR mismatch") ;
		    RETURN( COND_PushCondition(DCM_VRMISMATCH,
			       DCM_Message(DCM_VRMISMATCH), vrCode, e->tag));
                }
#else
               if( rwc_err ){
                fprintf(stderr,"++ DICOM WARNING: VR mismatch in element (%04x,%04x)\n",  /* RWC */
                        DCM_TAG_GROUP(e->tag), DCM_TAG_ELEMENT(e->tag) ) ; rwc_err-- ;
               }
               e->representation = vrPtr->representation;
#endif
	    }
	}
	if (vrPtr->representation != DCM_OW &&
	    vrPtr->representation != DCM_OB &&
	    vrPtr->representation != DCM_UN &&
	    vrPtr->representation != DCM_UT &&
	    vrPtr->representation != DCM_SQ) {
	    unsigned short shortLength;
	    if (byteOrder == BYTEORDER_SAME) {
		GET_SHORT_SAME_ORDER(localPtr + 2, shortLength);
	    } else {
		GET_SHORT_REVERSE_ORDER(localPtr + 2, shortLength);
	    }
	    e->length = shortLength;
	    if (*ptr != NULL)
		*ptr += 4;
	    calculatedLength = TRUE;
	} else {
	    if ((*size < 4) && knownLength) {
		if (debug)
		    (void) DCM_DumpElements((DCM_OBJECT **) object, 0);
		(void) DCM_CloseObject((DCM_OBJECT **) object);
		RETURN( COND_PushCondition(DCM_ILLEGALSTREAMLENGTH,
				DCM_Message(DCM_ILLEGALSTREAMLENGTH), *size,
					  "readVRLength"));
	    }
	    if (*ptr == NULL) {
		if (fd != -1) {
		    nBytes = read(fd, buf, 4);
		} else {
		    cond = (*object)->rd((*object)->userCtx, buf, 4, &nBytes);
		}

		if (nBytes != 4)
		    RETURN( COND_PushCondition(DCM_FILEACCESSERROR,
				     DCM_Message(DCM_FILEACCESSERROR), name,
					      "readVRLength"));
		localPtr = buf;
	    } else
		localPtr = *ptr;

	    if (knownLength)
		*size -= 4;
	    *fileOffset += (off_t) 4;
	    if (scannedLength != NULL)
		(*scannedLength) += 4;
	    (*object)->objectSize += 4;
	}
    }
    if (!calculatedLength) {
	if (byteOrder == BYTEORDER_SAME) {
	    GET_LONG_SAME_ORDER(localPtr, e->length);
	} else {
	    GET_LONG_REVERSE_ORDER(localPtr, e->length);
	}
	if (*ptr != NULL)
	    *ptr += 4;
    }
    if (debug) {
	char localVR[10];
	mapVRtoASCII(e->representation, localVR);
	fprintf(stderr, "%2s %6d %06x %s\n", localVR, e->length,
                (unsigned int)*fileOffset, e->description);
    }
    if (((e->length & 1) != 0) && (e->length != DCM_UNSPECIFIEDLENGTH)) {
	if (debug)
	    (void) DCM_DumpElements((DCM_OBJECT **) object, 0);
	(void) DCM_CloseObject((DCM_OBJECT **) object);
        if( rwc_err ){

#if 0 /* do not report this (the NIMH GE scanners are producing such files)
         17 May 2006 [rickr] */

         fprintf(stderr,"** DICOM ERROR: illegal odd length=%d in element (%04x,%04x)\n",  /* RWC */
                 e->length,DCM_TAG_GROUP(e->tag), DCM_TAG_ELEMENT(e->tag) ) ; rwc_err-- ;
#endif
        }
	RETURN( COND_PushCondition(DCM_UNEVENELEMENTLENGTH,
				  DCM_Message(DCM_UNEVENELEMENTLENGTH),
			     DCM_TAG_GROUP(e->tag), DCM_TAG_ELEMENT(e->tag),
				  e->length, "readFile"));
    }
    if ((e->length != (U32) DCM_UNSPECIFIEDLENGTH) && (e->length > (U32) (*size))) {
	if (debug)
	    (void) DCM_DumpElements((DCM_OBJECT **) object, 0);
	(void) DCM_CloseObject((DCM_OBJECT **) object);
        if( rwc_err ){
         fprintf(stderr,"** DICOM ERROR: oversize length=%d in element (%04x,%04x)\n",  /* RWC */
                 e->length,DCM_TAG_GROUP(e->tag), DCM_TAG_ELEMENT(e->tag) ) ; rwc_err-- ;
        }
	RETURN( COND_PushCondition(DCM_ELEMENTLENGTHERROR,
				  DCM_Message(DCM_ELEMENTLENGTHERROR),
			     DCM_TAG_GROUP(e->tag), DCM_TAG_ELEMENT(e->tag),
				  e->length, *size, "readFile"));
    }
    RETURN( DCM_NORMAL);
}

static CONDITION
readSequence(const char *name, unsigned char **ptr, int fd, U32 * size,
	     off_t * fileOffset, int recursionLevel, unsigned long opt,
	  int byteOrder, CTNBOOLEAN explicitVR, CTNBOOLEAN acceptVRMismatch,
	     CTNBOOLEAN fileFlag, CTNBOOLEAN * remainOpenFlag,
	     CTNBOOLEAN convertFlag, PRIVATE_OBJECT ** object,
	     U32 * scannedLength, DCM_ELEMENT * e,
	     PRV_ELEMENT_ITEM ** elementItem)
{
    CTNBOOLEAN knownLength = TRUE;
    CONDITION cond;
    U32 sequenceLength;

    U32 localLength;
    CTNBOOLEAN sequenceDone;
    DCM_ELEMENT tagE;
    DCM_OBJECT
	* sequenceObject;
    DCM_SEQUENCE_ITEM *sequenceItem;
    CONDITION flag;
    unsigned char *localPtr;
    off_t itemTagOffset;

    if (*size == (long) DCM_UNSPECIFIEDLENGTH)
	knownLength = FALSE;

    cond = newElementItem(e, FALSE, elementItem);
    if (cond != DCM_NORMAL)
	return cond;
    (*elementItem)->element.d.sq = LST_Create();
    if ((*elementItem)->element.d.sq == NULL)
	return COND_PushCondition(DCM_LISTFAILURE,
			      DCM_Message(DCM_LISTFAILURE), "readSequence");

    localLength = (*elementItem)->element.length;
    sequenceDone = (localLength == 0);

    while (!sequenceDone) {
	if (debug)
	    fprintf(stderr, "Sequence Length: %d %x\n", localLength,
		    localLength);

	sequenceLength = 0;
	itemTagOffset = *fileOffset;

	flag = readGroupElement(name, ptr, fd, &localLength, fileOffset, knownLength,
				byteOrder, explicitVR, acceptVRMismatch, object, &sequenceLength, &tagE);
	if (flag == DCM_STREAMCOMPLETE)
	    break;
	else if (flag != DCM_NORMAL)
	    return flag;

	flag = readVRLength(name, ptr, fd, &localLength, fileOffset, knownLength,
			    byteOrder, explicitVR, acceptVRMismatch, object,
			    &sequenceLength, &tagE);
	if (flag != DCM_NORMAL)
	    return flag;

	if (*size != (long) DCM_UNSPECIFIEDLENGTH)
	    *size -= sequenceLength;
	if (scannedLength != NULL)
	    *scannedLength += sequenceLength;

	sequenceLength = 0;


	if (debug)
	    fprintf(stderr, "Sequence item: %4x %4x %d (%x)\n",
		    DCM_TAG_GROUP(tagE.tag),
		    DCM_TAG_ELEMENT(tagE.tag), tagE.length, tagE.length);
	if (tagE.tag == DCM_DLMITEM) {
	    localPtr = *ptr;
	    cond = readFile1(name,
			     localPtr,
			     fd, tagE.length,
			     fileOffset, recursionLevel + 1, opt,
			     object, &sequenceObject, &sequenceLength,
			  remainOpenFlag, (*object)->userCtx, (*object)->rd,
			     (*object)->sk);
	    *ptr = localPtr;
	    if (cond == DCM_NORMAL) {
		sequenceItem = CTN_MALLOC(sizeof(*sequenceItem));
		if (sequenceItem == NULL)
		    return COND_PushCondition(DCM_MALLOCFAILURE,
					      DCM_Message(DCM_MALLOCFAILURE),
					 sizeof(*sequenceItem), "readFile");

		((PRIVATE_OBJECT *) sequenceObject)->offset = itemTagOffset;
		sequenceItem->object = sequenceObject;
		cond = LST_Enqueue(&(*elementItem)->element.d.sq,
				   (void *)sequenceItem);
		if (cond != LST_NORMAL)
		    return COND_PushCondition(DCM_LISTFAILURE,
				  DCM_Message(DCM_LISTFAILURE), "readFile");
		if (*size != (long) DCM_UNSPECIFIEDLENGTH)
		    *size -= sequenceLength;
		if (scannedLength != NULL)
		    *scannedLength += sequenceLength;
		(*object)->objectSize += sequenceLength;
		if (localLength != DCM_UNSPECIFIEDLENGTH)
		    localLength -= sequenceLength;
	    } else
		return cond;
	} else {
	    sequenceDone = TRUE;
	}
	if (localLength == 0)
	    sequenceDone = TRUE;
    }
    return DCM_NORMAL;
}

static CONDITION
scanCompressedPixels(char *name, unsigned char **ptr, int fd, U32 * size,
		  off_t * fileOffset, int recursionLevel, unsigned long opt,
		     int byteOrder, CTNBOOLEAN explicitVR,
		     CTNBOOLEAN acceptVRMismatch,
		     CTNBOOLEAN fileFlag, CTNBOOLEAN * remainOpenFlag,
		     CTNBOOLEAN convertFlag, PRIVATE_OBJECT ** object,
		     U32 * scannedLength, DCM_ELEMENT * e,
		     PRV_ELEMENT_ITEM ** elementItem)
{
    CTNBOOLEAN knownLength = TRUE;
    U32 sequenceLength;
    U32 scannedBytes = 0;

    U32 localLength;
    CTNBOOLEAN sequenceDone;
    DCM_ELEMENT tagE;
    CONDITION flag;
    unsigned char *localPtr;

    if (*size == (long) DCM_UNSPECIFIEDLENGTH)
	knownLength = FALSE;

    localLength = (*elementItem)->element.length;
    sequenceDone = (localLength == 0);

    while (!sequenceDone) {
	sequenceLength = 0;
	flag = readGroupElement(name, ptr, fd, &localLength, fileOffset,
			     FALSE, byteOrder, explicitVR, acceptVRMismatch,
				object, &sequenceLength, &tagE);
	if (flag == DCM_STREAMCOMPLETE)
	    break;
	else if (flag != DCM_NORMAL)
	    return flag;

	flag = readVRLength(name, ptr, fd, &localLength, fileOffset, knownLength,
			    byteOrder, explicitVR, acceptVRMismatch, object,
			    &sequenceLength, &tagE);
	if (flag != DCM_NORMAL)
	    return flag;

	if (*size != (long) DCM_UNSPECIFIEDLENGTH)
	    *size -= sequenceLength;
	if (scannedLength != NULL)
	    *scannedLength += sequenceLength;
	scannedBytes += sequenceLength;

	if (debug)
	    fprintf(stderr, "Sequence item: %4x %4x %d (%x)\n",
		    DCM_TAG_GROUP(tagE.tag),
		    DCM_TAG_ELEMENT(tagE.tag), tagE.length, tagE.length);
	if (tagE.tag == DCM_DLMITEM) {
	    localPtr = *ptr;
	    if (tagE.length != 0) {
		lseek(fd, tagE.length, SEEK_CUR);
		*fileOffset += tagE.length;
		if (*size != (long) DCM_UNSPECIFIEDLENGTH)
		    *size -= tagE.length;
		if (scannedLength != NULL)
		    *scannedLength += tagE.length;
	    }
	} else {
	    sequenceDone = TRUE;
	}
	if (localLength == 0)
	    sequenceDone = TRUE;

	if (debug)
	    fprintf(stderr, "Scanned Bytes: %d\n", scannedBytes);
    }
    if ((scannedBytes & 1) != 0) {
	lseek(fd, 1, SEEK_CUR);
	*fileOffset += 1;
	if (*size != (long) DCM_UNSPECIFIEDLENGTH)
	    *size -= 1;
    }
    return DCM_NORMAL;
}

static CONDITION
readData(const char *name, unsigned char **ptr, int fd, U32 * size,
	 off_t * fileOffset,
	 CTNBOOLEAN knownLength, int byteOrder, CTNBOOLEAN explicitVR,
	 CTNBOOLEAN acceptVRMismatch,
	 CTNBOOLEAN fileFlag, CTNBOOLEAN * remainOpenFlag,
	 CTNBOOLEAN convertFlag, PRIVATE_OBJECT ** object,
	 U32 * scannedLength, DCM_ELEMENT * e,
	 PRV_ELEMENT_ITEM ** elementItem)
{
    CTNBOOLEAN pixelFlag;
    CONDITION cond;
    int nBytes;

    pixelFlag = (e->tag == DCM_PXLPIXELDATA);
    cond = newElementItem(e, (pixelFlag == FALSE), elementItem);
    if (cond != DCM_NORMAL) {
	(void) DCM_CloseObject((DCM_OBJECT **) object);
	return cond;
    }
    (*elementItem)->element.data_offset = 0 ;      /* RWCox */
    if (pixelFlag) {
	if (fileFlag)
	    *remainOpenFlag = TRUE;
	(*elementItem)->byteOrder = byteOrder;
	(*elementItem)->dataOffset = *fileOffset;
	(*elementItem)->currentOffset = 0;
	(*elementItem)->element.d.ot = NULL;
	if ((*object)->pixelBitsAllocated == 8)
	    (*elementItem)->element.representation = DCM_OB;
	else
	    (*elementItem)->element.representation = DCM_OW;
	if (fileFlag) {
	    if (fd != -1) {
		if ((*elementItem)->element.length != DCM_UNSPECIFIEDLENGTH){

                    pxl_off = lseek( fd , 0 , SEEK_CUR ) ;
                    pxl_len = (unsigned int)((*elementItem)->element.length) ;

                    (*elementItem)->element.data_offset = pxl_off ;   /* RWCox */

		    (void) lseek(fd,
				 (off_t) (*elementItem)->element.length,
				 SEEK_CUR);
		} else {
		    U32 l1 = 0;
		    U32 s1;
		    off_t f1 = 0;

		    s1 = *size;
		    scanCompressedPixels("", ptr, fd,
					 &s1,	/* size */
					 &f1,	/* fileOffset */
					 0, 0,
					 byteOrder, explicitVR,
					 acceptVRMismatch,
					 fileFlag, remainOpenFlag,
					 convertFlag, object,
					 &l1,	/* scannedLength */
					 e, elementItem);
		    (*elementItem)->originalDataLength = l1;
		    (*elementItem)->paddedDataLength = l1;
		}
	    } else {
		(*object)->sk((*object)->userCtx,
			      (*elementItem)->element.length, SEEK_CUR);
	    }
	    (*object)->fd = fd;
	}
    } else {
	if (fileFlag) {
	    if (fd != -1) {
                (*elementItem)->element.data_offset = lseek(fd,0,SEEK_CUR);  /* RWCox */
		nBytes = read(fd, (*elementItem)->element.d.ot,
			      (int) (*elementItem)->element.length);
	    } else {
		cond = (*object)->rd((*object)->userCtx,
				     (*elementItem)->element.d.ot,
			    (long) (*elementItem)->element.length, &nBytes);
	    }
	    if (nBytes != (int) (*elementItem)->element.length) {
		(void) DCM_CloseObject((DCM_OBJECT **) object);
		return COND_PushCondition(DCM_FILEACCESSERROR,
			DCM_Message(DCM_FILEACCESSERROR), name, "readFile");
	    }
	} else {
	    (void) memcpy((*elementItem)->element.d.ot, ptr,
			  (*elementItem)->element.length);
	    ptr += (*elementItem)->originalDataLength;
	}
        if( LITTLE_ENDIAN_ARCHITECTURE ){
	  if ((*elementItem)->element.representation == DCM_AT)
	    swapATGroupElement(&(*elementItem)->element);
        }
	if (byteOrder != BYTEORDER_SAME)
	    swapInPlace(object, &(*elementItem)->element);
	if (convertFlag) {
	    cond = verifyFormat(*elementItem);
	    if (cond != DCM_NORMAL)
		return cond;
	}
    }
    if (*size != (long) DCM_UNSPECIFIEDLENGTH)
	*size -= (*elementItem)->originalDataLength;
    *fileOffset += (off_t) (*elementItem)->originalDataLength;
    if (scannedLength != NULL)
	(*scannedLength) += (*elementItem)->originalDataLength;

    if ((*elementItem)->element.length != DCM_UNSPECIFIEDLENGTH) {
      (*elementItem)->paddedDataLength = (*elementItem)->element.length;
    }
    if (((*elementItem)->paddedDataLength != DCM_UNSPECIFIEDLENGTH) &&
	((*elementItem)->paddedDataLength & 1) )
	(*elementItem)->paddedDataLength += 1;
    (*object)->objectSize += (*elementItem)->paddedDataLength;

    return DCM_NORMAL;

}

static CONDITION
checkAttributeOrder(DCM_ELEMENT * e, long *lastGroup, long *lastElement,
		    CTNBOOLEAN allowRepeatElements)
{
    unsigned short group;
    unsigned short element;

    group = DCM_TAG_GROUP(e->tag);
    element = DCM_TAG_ELEMENT(e->tag);

    if ((long) group == *lastGroup) {
	if (((long) element == *lastElement) && allowRepeatElements) {
	  return DCM_REPEATEDELEMENT;
	}
	if ((long) element <= *lastElement)
	    return COND_PushCondition(DCM_ELEMENTOUTOFORDER,
				      DCM_Message(DCM_ELEMENTOUTOFORDER),
				      group, element, "checkAttributeOrder");
    } else if ((long) group > *lastGroup) {
    } else {
	return COND_PushCondition(DCM_ELEMENTOUTOFORDER,
				  DCM_Message(DCM_ELEMENTOUTOFORDER),
				  group, element, "checkAttributeOrder");
    }
    *lastGroup = (long) group;
    *lastElement = (long) element;

    return DCM_NORMAL;
}

static CONDITION
handleGroupItem(PRIVATE_OBJECT ** obj, PRV_GROUP_ITEM ** groupItem,
		unsigned short group)
{
    CTNBOOLEAN createGroupFlag = TRUE;

    if (*groupItem == NULL)
	createGroupFlag = TRUE;
    else if ((*groupItem)->group != group)
	createGroupFlag = TRUE;
    else
	createGroupFlag = FALSE;

    if (createGroupFlag == TRUE) {
	*groupItem = CTN_MALLOC(sizeof(**groupItem));
	if (*groupItem == NULL) {
	    (void) DCM_CloseObject((DCM_OBJECT **) obj);
	    return COND_PushCondition(DCM_ELEMENTCREATEFAILED,
				      DCM_Message(DCM_ELEMENTCREATEFAILED),
				      "handleGroupItem",
				      group, 0xffff, sizeof(**groupItem));
	}
	(*groupItem)->group = group;
	(*groupItem)->baseLength = 0;
	(*groupItem)->longVRAttributes = 0;
	(*groupItem)->elementList = LST_Create();
	if ((*groupItem)->elementList == NULL) {
	    (void) DCM_CloseObject((DCM_OBJECT **) obj);
	    return COND_PushCondition(DCM_LISTFAILURE,
				      DCM_Message(DCM_LISTFAILURE),
				      "handleGroupItem");
	}
	if (LST_Enqueue(&(*obj)->groupList, (void *)*groupItem) != LST_NORMAL) {
	    (void) DCM_CloseObject((DCM_OBJECT **) obj);
	    return COND_PushCondition(DCM_LISTFAILURE,
				      DCM_Message(DCM_LISTFAILURE),
				      "handleGroupItem");
	}
    }
    return DCM_NORMAL;
}

static CONDITION
readFile1(const char *name, unsigned char *callerBuf, int fd, U32 size,
	  off_t * fileOffset, int recursionLevel,
	  unsigned long opt, PRIVATE_OBJECT ** parentObject,
	  DCM_OBJECT ** callerObject,
	  U32 * scannedLength, CTNBOOLEAN * remainOpenFlag,
	  void *ctx,
	  CONDITION(*rd) (void *ctx, void *buf, int toRead, int *bytesRead),
	  CONDITION(*sk) (void *ctx, int offset, int flag))
{
    /* avoid 'uninitialized elements'   28 Sep 2018 [rickr] */
    CONDITION
    cond = DCM_NORMAL;
    int
        byteOrder = NATIVE_ORDER;
    long
        lastGroup = -1,
        lastElement = -1;
    U32
	sequenceLength = 0,
	scannedSequenceLength = 0;
    PRIVATE_OBJECT
	** object=NULL;
    PRV_GROUP_ITEM
	* groupItem = NULL;
    DCM_ELEMENT
	e = {};
    CTNBOOLEAN
	convertFlag = FALSE,
	done = FALSE,
	knownLength = TRUE,
	explicitVR = FALSE,
	acceptVRMismatch = FALSE,
	part10Flag = FALSE;
    unsigned char
       *ptr = NULL;
    PRV_ELEMENT_ITEM
	* elementItem = NULL;
    CTNBOOLEAN
	fileFlag = TRUE;
    CONDITION flag = TRUE;
    CTNBOOLEAN allowRepeatElements = FALSE;

ENTRY("readFile1") ;

    ptr = callerBuf;
    if (ptr != NULL)
	fileFlag = FALSE;

    if ((opt & DCM_FILEFORMATMASK) == DCM_PART10FILE) {
	part10Flag = TRUE;
	opt &= ~DCM_ORDERMASK;
	opt &= ~DCM_FILEFORMATMASK;
	opt |= DCM_EXPLICITLITTLEENDIAN;
    }
    if ((opt & DCM_SPECIALFORMATMASK) == DCM_EFILM) {
	part10Flag = TRUE;
	opt &= ~DCM_ORDERMASK;
	opt &= ~DCM_FILEFORMATMASK;
	opt |= DCM_ORDERLITTLEENDIAN;
    }
    if ((opt & DCM_REPEATELEMENTSMASK) == DCM_ALLOWREPEATELEMENTS) {
      allowRepeatElements = TRUE;
    }

    switch (opt & DCM_ORDERMASK) {
    case DCM_ORDERNATIVE:
	byteOrder = NATIVE_ORDER;
	break;
    case DCM_ORDERLITTLEENDIAN:
	byteOrder = LITTLE_ORDER;
	break;
    case DCM_EXPLICITLITTLEENDIAN:
	byteOrder = LITTLE_ORDER;
	explicitVR = TRUE;
	break;
    case DCM_ORDERBIGENDIAN:
	byteOrder = BIG_ORDER;
	break;
    case DCM_EXPLICITBIGENDIAN:
	byteOrder = BIG_ORDER;
	explicitVR = TRUE;
	break;
    default:
	byteOrder = NATIVE_ORDER;
	break;
    }
    if ((opt & DCM_CONVERTMASK) == DCM_FORMATCONVERSION)
	convertFlag = TRUE;
    if ((opt & DCM_VRMASK) == DCM_ACCEPTVRMISMATCH)
	acceptVRMismatch = TRUE;

    if (scannedLength != NULL)
	*scannedLength = 0;

    cond = DCM_CreateObject(callerObject, opt);
    if (cond != DCM_NORMAL)
	RETURN( cond ) ;

    object = (PRIVATE_OBJECT **) callerObject;
    if (fileFlag)
	strcpy((*object)->fileName, name);

    (*object)->fd = -1;
    (*object)->rd = rd;
    (*object)->sk = sk;
    (*object)->userCtx = ctx;
    (*object)->dataOptions = 0;
    if (size == (long) DCM_UNSPECIFIEDLENGTH)
	knownLength = FALSE;

    if ((fileFlag) && ((opt & DCM_DELETEMASK) == DCM_DELETEONCLOSE) && (recursionLevel == 0))
	(*object)->deleteFlag = TRUE;

    if (parentObject != NULL)
	(*object)->pixelRepresentation = (*parentObject)->pixelRepresentation;

    if (recursionLevel == 0 && (part10Flag || g_readpreamble)) {
	flag = readPreamble(name, &ptr, fd, &size, fileOffset, knownLength,
			    object, scannedLength);
	if (flag != DCM_NORMAL)
	    { STATUS("readPreamble fails"); goto abort; }
    }
    while (!done) {
	flag = readGroupElement(name, &ptr, fd, &size, fileOffset, knownLength,
			    byteOrder, explicitVR, acceptVRMismatch, object,
				scannedLength, &e);
	if (flag == DCM_STREAMCOMPLETE)
	    break;
	else if (flag != DCM_NORMAL)
	    { STATUS("readGroupElement fails"); goto abort; }
#if 0
	if (e.tag == DCM_MAKETAG(0x003a, 0x1000)) {
	    fprintf(stderr, "Found waveform\n");
	}
#endif
	flag = readVRLength(name, &ptr, fd, &size, fileOffset, knownLength,
			    byteOrder, explicitVR, acceptVRMismatch, object,
			    scannedLength, &e);
	if (flag != DCM_NORMAL)
	    { STATUS("readVRLength fails"); goto abort; }

	if ((e.representation == DCM_UN) &&
	    (e.length == DCM_UNSPECIFIEDLENGTH)) {
	    e.representation = DCM_SQ;
	}
#ifndef SMM
	if ((e.tag == DCM_DLMITEMDELIMITATIONITEM) ||
	    (e.tag == DCM_DLMSEQUENCEDELIMITATIONITEM)) {
	    RETURN( DCM_NORMAL) ;
	}
#else
	if (e.tag == DCM_DLMITEMDELIMITATIONITEM) {
	    (*object)->objectSize -= 8;
	    RETURN( DCM_NORMAL );
	}
	if (e.tag == DCM_DLMSEQUENCEDELIMITATIONITEM)
	    RETURN( DCM_NORMAL );
#endif

	if (e.representation == DCM_SQ) {
	    sequenceLength = e.length;
	    scannedSequenceLength = 0;
	    flag = readSequence(name, &ptr, fd, &sequenceLength,
				fileOffset, recursionLevel, opt,
				byteOrder, explicitVR, acceptVRMismatch,
				fileFlag, remainOpenFlag,
				convertFlag, object, &scannedSequenceLength,
				&e, &elementItem);
	    if (flag != DCM_NORMAL)
		{ STATUS("readSequence fails"); goto abort; }
	    if (size != (long) DCM_UNSPECIFIEDLENGTH)
		size -= scannedSequenceLength;
	    if (scannedLength != NULL)
		*scannedLength += scannedSequenceLength;

	} else {

	    flag = readData(name, &ptr, fd, &size, fileOffset, knownLength,
			  byteOrder, explicitVR, acceptVRMismatch, fileFlag,
			    remainOpenFlag, convertFlag,
			    object, scannedLength, &e, &elementItem);
	    if (flag != DCM_NORMAL)
		{ STATUS("readData fails"); goto abort; }
	}
	computeVM(object, &elementItem->element);

	cond = checkAttributeOrder(&e, &lastGroup, &lastElement, allowRepeatElements);
	if (cond != DCM_NORMAL) {
	    if (cond == DCM_REPEATEDELEMENT) {
		CTN_FREE(elementItem);
		continue;
	    } else {
                CTN_FREE(elementItem);   /* 22 June 2005 [rickr] */
		RETURN( cond ) ;
	    }
	}

	cond = handleGroupItem(object, &groupItem, DCM_TAG_GROUP(e.tag));
	if (cond != DCM_NORMAL)
	     /* goto abort; ASG */ RETURN( cond );

	if (DCM_TAG_ELEMENT(e.tag) != 0x0000) {
	    groupItem->baseLength += 8 + elementItem->paddedDataLength;
	    if (elementItem->element.representation == DCM_OB ||
		elementItem->element.representation == DCM_OW ||
		elementItem->element.representation == DCM_SQ) {
		groupItem->longVRAttributes++;
		(*object)->longVRAttributes++;
	    }
	}
	if ((DCM_TAG_ELEMENT(e.tag) == 0x0000) && ((*object)->groupLengthFlag == FALSE)) {
	    CTN_FREE(elementItem);
	} else {
	    cond = LST_Enqueue(&groupItem->elementList, (void *)elementItem);
	    if (cond != LST_NORMAL) {
		(void) DCM_CloseObject(callerObject);
		RETURN( COND_PushCondition(DCM_LISTFAILURE,
					  DCM_Message(DCM_LISTFAILURE),
					  "readFile") );
	    }
	    cond = updateObjectType(object, &elementItem->element);	/* repair */

	    cond = updateSpecialElements(object, elementItem);	/* repair */
	}

	if (size == 0)
	    done = TRUE;

	if (part10Flag) {
	    if ((*object)->objectSize == (DCM_PREAMBLELENGTH + 4 + 12 + (*object)->metaHeaderLength)) {
		opt &= ~DCM_ORDERMASK;
		opt |= (*object)->dataOptions & DCM_ORDERMASK;
		explicitVR = FALSE;
		switch (opt & DCM_ORDERMASK) {
		case DCM_ORDERNATIVE:
		    byteOrder = NATIVE_ORDER;
		    break;
		case DCM_ORDERLITTLEENDIAN:
		    byteOrder = LITTLE_ORDER;
		    break;
		case DCM_EXPLICITLITTLEENDIAN:
		    byteOrder = LITTLE_ORDER;
		    explicitVR = TRUE;
		    break;
		case DCM_ORDERBIGENDIAN:
		    byteOrder = BIG_ORDER;
		    break;
		case DCM_EXPLICITBIGENDIAN:
		    byteOrder = BIG_ORDER;
		    explicitVR = TRUE;
		    break;
		default:
		    byteOrder = LITTLE_ORDER;
		    explicitVR = TRUE;
		    break;
		}
	    }
	}
    }

#ifdef SMM
#endif

    groupItem = (void *)LST_Head(&(*object)->groupList);
    if (groupItem != NULL) {
	(void) LST_Position(&(*object)->groupList, (void *)groupItem);
	while (groupItem != NULL) {
	    elementItem = (void *)LST_Head(&groupItem->elementList);
	    if (elementItem != NULL) {
		if (DCM_TAG_ELEMENT(elementItem->element.tag) == 0x0000) {
		    *elementItem->element.d.ul = groupItem->baseLength;
		}
	    }
	    groupItem = (void *)LST_Next(&(*object)->groupList);
	}
    }
    RETURN( DCM_NORMAL );

abort:
    RETURN (flag);
}

/* locateElement
**
** Purpose:
**	Locate the DICOM element with the specified tag number in the given
**	DICOM object
**
** Parameter Dictionary:
**	obj		Handle to the DICOM object to be searched
**	tag		Tag number of the element to be searched
**
** Return Values:
**	Pointer to the element if found else NULL
**
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/
static PRV_ELEMENT_ITEM *
locateElement(PRIVATE_OBJECT ** obj, DCM_TAG tag)
{
    PRV_GROUP_ITEM
    * groupItem;
    PRV_ELEMENT_ITEM
	* elementItem;
    CTNBOOLEAN
	found = FALSE;

    groupItem = (void *)LST_Head(&(*obj)->groupList);
    if (groupItem == NULL)
	return NULL;

    (void) LST_Position(&(*obj)->groupList, (void *)groupItem);
    while (groupItem != NULL) {
	if (groupItem->group == DCM_TAG_GROUP(tag))
	    break;

	groupItem = (void *)LST_Next(&(*obj)->groupList);
    }
    if (groupItem == NULL)
	return NULL;

    elementItem = (void *)LST_Head(&groupItem->elementList);
    if (elementItem == NULL)
	return NULL;

    (void) LST_Position(&groupItem->elementList, (void *)elementItem);
    while (!found && (elementItem != NULL)) {
	if (elementItem->element.tag == tag) {
	    found = TRUE;
	} else
	    elementItem = (void *)LST_Next(&groupItem->elementList);
    }
    if (found)
	return elementItem;
    else
	return NULL;
}

/* computeVM
**
** Purpose:
**	Compute the multiplicity of the specified element in the DICOM
**	object
**
** Parameter Dictionary:
**	object		Handle to the DICOM object
**	element		Element whose value multiplicity is to be found out.
**
** Return Values:
**	None
**
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/
static void
computeVM(PRIVATE_OBJECT ** object, DCM_ELEMENT * element)
{
    char
       *c;
    int
        i;

    switch (element->representation) {
    case DCM_AE:		/* Application Entity */
    case DCM_AS:		/* Age string */
    case DCM_CS:		/* Control string */
    case DCM_DA:		/* Date */
    case DCM_DS:		/* Decimal string */
    case DCM_DT:		/* Date/Time */
    case DCM_IS:		/* Integer string */
    case DCM_LO:		/* Long string */
    case DCM_PN:		/* Person Name */
    case DCM_SH:		/* Short string */
    case DCM_TM:		/* Time */
    case DCM_UI:		/* Unique identifier (UID) */
    case DCM_UT:		/* Unlimited text */
	element->multiplicity = 1;
	c = element->d.string;
	for (i = 0; i < (int) element->length; i++)
	    if (*c++ == '\\')
		element->multiplicity++;
	break;

    case DCM_FD:		/* Floating double */
	element->multiplicity = element->length / 8;
	break;
    case DCM_AT:		/* Attribute tag */
    case DCM_FL:		/* Float */
    case DCM_SL:		/* Signed long */
    case DCM_UL:		/* Unsigned long */
	element->multiplicity = element->length / 4;
	break;
    case DCM_SS:		/* Signed short */
    case DCM_US:		/* Unsigned short */
	element->multiplicity = element->length / 2;
	break;
    case DCM_LT:		/* Long text */
    case DCM_OT:		/* Other binary value */
    case DCM_SQ:		/* Sequence of items */
    case DCM_ST:		/* Short text */
    /*case DCM_UNKNOWN:*/
    case DCM_UN:
    case DCM_RET:
    case DCM_CTX:
    case DCM_DD:		/* Data set */
    default:
	element->multiplicity = 1;
	break;
    }
}
/* ctxSensitiveLookup
**
** Purpose:
**	Lookup representation of elements that are context sensitive
**
** Parameter Dictionary:
**	object		Handle to the DICOM object containing this element.
**	element		Element who representation is to be determined.
**
** Return Values:
**	None
**
** Notes:
**
*/
static void
ctxSensitiveLookup(PRIVATE_OBJECT ** object, DCM_ELEMENT * element)
{
    switch (element->tag) {
	case DCM_IMGSMALLESTIMAGEPIXELVALUE:
	case DCM_IMGLARGESTIMAGEPIXELVALUE:
	case DCM_IMGSMALLESTPIXELVALUESERIES:
	case DCM_IMGLARGESTPIXELVALUESERIES:
	case DCM_IMGSMALLESTIMAGEPIXELVALUEPLANE:
	case DCM_IMGLARGESTIMAGEPIXELVALUEPLANE:
	case DCM_IMGLUTDESCRIPTOR:
	case DCM_IMGLUTDATA:
	case DCM_IMGLOOKUPDATARED:
	case DCM_IMGLOOKUPDATAGREEN:
	case DCM_IMGLOOKUPDATABLUE:
	if ((*object)->pixelRepresentation == 0x0000)
	    element->representation = DCM_US;
	else if ((*object)->pixelRepresentation == 0x0001)
	    element->representation = DCM_SS;
	else
	    element->representation = DCM_US;
	break;
    case DCM_MAKETAG(0x003a, 0x1000):
	if (strcmp((*object)->waveformDataVR, "SS") == 0)
	    element->representation = DCM_SS;
	break;

    default:
	break;
    }
}

static CONDITION
copyData(PRIVATE_OBJECT ** object, PRV_ELEMENT_ITEM * from,
	 DCM_ELEMENT * to, U32 * rtnLength)
{
    unsigned char *p = NULL;
    U32 l;
    int nBytes;
    CONDITION cond;

    if (from->element.representation == DCM_SQ)
	return COND_PushCondition(DCM_CANNOTGETSEQUENCEVALUE,
				  DCM_Message(DCM_CANNOTGETSEQUENCEVALUE),
			      from->element.tag, "copyData (DCM internal)");

    l = MIN(from->element.length, to->length);
    if (rtnLength != NULL)
	*rtnLength = l;

    if (from->element.d.ot == NULL) {
	if ((*object)->fd != -1) {
	    (void) lseek((*object)->fd,
			 from->dataOffset + (off_t) p, SEEK_SET);
	    nBytes = read((*object)->fd, to->d.ot, (int) l);
	} else {
	    (*object)->sk((*object)->userCtx,
			  (long) (from->dataOffset + (off_t) p), SEEK_SET);
	    cond = (*object)->rd((*object)->userCtx, to->d.ot, (long) l, &nBytes);
	}
	if (nBytes != (int) l) {
	    return COND_PushCondition(DCM_FILEACCESSERROR,
				      DCM_Message(DCM_FILEACCESSERROR),
				      (*object)->fileName,
				      "copyData (DCM internal)");
	}
        if( LITTLE_ENDIAN_ARCHITECTURE ){
	  if (from->element.representation == DCM_AT) {
	    DCM_ELEMENT e;
	    e = from->element;
	    e.length = l;
	    e.d.ot = to->d.ot;
	    swapATGroupElement(&e);
	  }
        }
	if (from->byteOrder == BYTEORDER_REVERSE) {
	    DCM_ELEMENT e;
	    e = from->element;
	    e.length = l;
	    e.d.ot = to->d.ot;
	    swapInPlace(object, &e);
	}
    } else {
	unsigned char *q;
	q = (unsigned char *) from->element.d.ot + (U32)PTOI(p);
	(void) memcpy(to->d.ot, q, l);
    }
    p += l;
    if ((unsigned)PTOI(p) == from->element.length)
	return DCM_NORMAL;
    else
	return DCM_GETINCOMPLETE;
}

static CONDITION
readLengthToEnd(int fd, const char *fileName,
		unsigned long opt, U32 * lengthToEnd)
{
    unsigned char buf[24];
    DCM_OBJECT *obj;
    CONDITION cond;
    DCM_ELEMENT e = {DCM_MAKETAG(0x0008, 0x0001), DCM_UL, "", 1, 4, NULL};
    void *ctx = NULL;
    U32 rtnLength = 0;

    if (read(fd, buf, 24) != 24)
	return COND_PushCondition(DCM_FILEACCESSERROR,
				  DCM_Message(DCM_FILEACCESSERROR), fileName,
				  "(DCM)readLengthToEnd");

    cond = DCM_ImportStream(buf, 24, opt, &obj);
    if (cond != DCM_NORMAL)
	return cond;

    e.d.ul = lengthToEnd;
    cond = DCM_GetElementValue(&obj, &e, &rtnLength, &ctx);

    (void) DCM_CloseObject(&obj);

    return cond;
}

static void
swapATGroupElement(DCM_ELEMENT * e)
{
    U32
    length;
    unsigned short
        tmp,
       *us;

    length = e->length;
    us = e->d.us;
    while (length >= 4) {
	tmp = us[0];
	us[0] = us[1];
	us[1] = tmp;
	us += 2;
	length -= 4;
    }
}

static void
dumpSS(short *ss, long vm)
{
    long index = 0;
    RWC_printf("decimal SS:") ;
    while (index < vm) {
	RWC_printf("%7d ", *(ss++));
	if ((++index) % 8 == 0)
	    RWC_printf("\n");
    }
    RWC_printf("\n");
}

static void
dumpSL(S32 * sl, long vm)
{
    long index = 0;
    RWC_printf("decimal SL:") ;
    while (index < vm) {
	RWC_printf("%7d ", *(sl++));
	if ((++index) % 8 == 0)
	    RWC_printf("\n");
    }
    RWC_printf("\n");
}

static void
dumpUS(unsigned short *us, long vm)
{
    long index = 0;
    RWC_printf("decimal US:") ;
    while (index < vm) {
	RWC_printf("%7d ", *(us++));
	if ((++index) % 8 == 0)
	    RWC_printf("\n");
    }
    RWC_printf("\n");
}
static void
dumpUL(U32 * ul, long vm)
{
    long index = 0;
    RWC_printf("decimal UL:") ;
    while (index < vm) {
	RWC_printf("%7d ", *(ul++));
	if ((++index) % 8 == 0)
	    RWC_printf("\n");
    }
    RWC_printf("\n");
}

void swap_4bytes( size_t n , void *ar )
{
   register size_t ii ;
   unsigned char * cp0 = (unsigned char *)ar, * cp1, * cp2 ;
   register unsigned char tval ;

   for( ii=0 ; ii < n ; ii++ ){
       cp1 = cp0; cp2 = cp0+3;
       tval = *cp1;  *cp1 = *cp2;  *cp2 = tval;
       cp1++;  cp2--;
       tval = *cp1;  *cp1 = *cp2;  *cp2 = tval;
       cp0 += 4;
   }
   return ;
}

static void
dumpOB(unsigned char* c, long vm)
{
  long index = 0;
  RWC_printf("hex OB: (len %d)", vm) ;

  while (index < vm) {
    RWC_printf("%02x ", *(c++));
    if ((++index) % 8 == 0)
      RWC_printf("\n");
  }
  if( index%8 != 0 ) RWC_printf("\n");
}

static void
dumpBinaryData(void *d, DCM_VALUEREPRESENTATION vr, long vm,
	       long vmLimit)
{
    vm = (vm < vmLimit) ? vm : vmLimit;

    if (vm <= 1)
	return;

    switch (vr) {
    case DCM_SL:
	dumpSL((S32 *) d, vm);
	break;
    case DCM_UL:
	dumpUL((U32 *) d, vm);
	break;
    case DCM_SS:
	dumpSS((short *) d, vm);
	break;
    case DCM_US:
	dumpUS((unsigned short *) d, vm);
	break;
    case DCM_OB:
    case DCM_UN:
	dumpOB((unsigned char*) d, vm);
	break;
    default:
	break;
    }
}

static void
compareGroup(PRV_GROUP_ITEM * g1, PRV_GROUP_ITEM * g2,
	     void (*callback) (const DCM_ELEMENT * e1,
			       const DCM_ELEMENT * e2,
			       void *ctx),
	     void *ctx)
{
    PRV_ELEMENT_ITEM *e1 = NULL,
       *e2 = NULL;

    if (g1 != NULL) {
	e1 = (void *)LST_Head(&g1->elementList);
	if (e1 != NULL)
	    LST_Position(&g1->elementList, (void *)e1);
    }
    if (g2 != NULL) {
	e2 = (void *)LST_Head(&g2->elementList);
	if (e2 != NULL)
	    LST_Position(&g2->elementList, (void *)e2);
    }
    while (e1 != NULL) {
	if (e2 == NULL) {
	    callback(&e1->element, NULL, ctx);
	    e1 = (void *)LST_Next(&g1->elementList);
	} else if (e1->element.tag == e2->element.tag) {
	    callback(&e1->element, &e2->element, ctx);
	    e1 = (void *)LST_Next(&g1->elementList);
	    e2 = (void *)LST_Next(&g2->elementList);
	} else if (e1->element.tag < e2->element.tag) {
	    callback(&e1->element, NULL, ctx);
	    e1 = (void *)LST_Next(&g1->elementList);
	} else {
	    callback(NULL, &e2->element, ctx);
	    e2 = (void *)LST_Next(&g2->elementList);
	}
    }

    while (e2 != NULL) {
	callback(NULL, &e2->element, ctx);
	e2 = (void *)LST_Next(&g2->elementList);
    }
}

static void
remapFileName(const char *name, char *mapName)
{
    char c;

    while ((c = *name++) != '\0') {
	if (c == '\\')
	    *mapName++ = '/';
	else if (isupper(c))
	    *mapName++ = tolower(c);
	else
	    *mapName++ = c;
    }
    *mapName = '\0';
}

static void
copySequence(PRIVATE_OBJECT ** dstObj, DCM_ELEMENT * e)
{
    LST_HEAD *lst;
    DCM_SEQUENCE_ITEM *sqItem=NULL;
    DCM_ELEMENT newElement;

    lst = LST_Create();
    if (e->d.sq != NULL) {
	sqItem = (void *)LST_Head(&e->d.sq);
	(void) LST_Position(&e->d.sq, (void *)sqItem);
    }
    while (sqItem != NULL) {
	DCM_OBJECT *copy;
	DCM_SEQUENCE_ITEM *copyItem;

	DCM_CopyObject(&sqItem->object, &copy);
	copyItem = AFMALL( DCM_SEQUENCE_ITEM, sizeof(*copyItem));
	copyItem->object = copy;
	(void) LST_Enqueue(&lst, (void *)copyItem);

	sqItem = (void *)LST_Next(&e->d.sq);
    }

    memset(&newElement, 0, sizeof(newElement));
    newElement.tag = e->tag;
    newElement.representation = e->representation;
    newElement.d.sq = lst;
    DCM_AddSequenceElement((DCM_OBJECT **) dstObj, &newElement);
}

/* Restart public functions */

CONDITION
DCM_GetCompressedValue(DCM_OBJECT ** callerObject, DCM_TAG tag, void *buf,
		       size_t bufSize, DCM_GET_COMPRESSED_CALLBACK* callback,
		       void *ctx)
{
    PRIVATE_OBJECT
	** object;
    PRV_ELEMENT_ITEM
	* elementItem;
    S32 nBytes;
    S32 toRead;
    CONDITION cond;
    int doneFlag = 0;
    size_t elementLength;
    unsigned char *ptr;
    U32 size = 0;
    off_t fileOffset = 0;
    unsigned long opt=0 ;
    int byteOrder;
    int explicitVR;
    CTNBOOLEAN acceptVRMismatch = FALSE;
    DCM_ELEMENT e;
    U32 sequenceLength = 0;
    CONDITION flag;
    int index = 0;
    CTNBOOLEAN firstBuffer = TRUE;
    U32 *offsetBuffer = NULL;
    U32 offsetBufferCount = 0;
    U32 streamOffset = 0;
    int startOfFragment = 1;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_GetCompressedValue");
    if (cond != DCM_NORMAL)
	return cond;

    elementItem = locateElement(object, tag);

    if (elementItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
		       DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(tag),
				  DCM_TAG_ELEMENT(tag),
				  "DCM_GetEncodedValue");

    elementLength = elementItem->originalDataLength;
    ptr = NULL;			/* Means reading from a file */
    size = DCM_UNSPECIFIEDLENGTH;
    fileOffset = elementItem->dataOffset;

    opt |= (*object)->dataOptions & DCM_ORDERMASK;
    explicitVR = FALSE;
    switch (opt & DCM_ORDERMASK) {
    case DCM_ORDERNATIVE:
	byteOrder = NATIVE_ORDER;
	break;
    case DCM_ORDERLITTLEENDIAN:
	byteOrder = LITTLE_ORDER;
	break;
    case DCM_EXPLICITLITTLEENDIAN:
	byteOrder = LITTLE_ORDER;
	explicitVR = TRUE;
	break;
    case DCM_ORDERBIGENDIAN:
	byteOrder = BIG_ORDER;
	break;
    case DCM_EXPLICITBIGENDIAN:
	byteOrder = BIG_ORDER;
	explicitVR = TRUE;
	break;
    default:
	byteOrder = LITTLE_ORDER;
	explicitVR = TRUE;
	break;
    }
    if ((opt & DCM_VRMASK) == DCM_ACCEPTVRMISMATCH)
	acceptVRMismatch = TRUE;

    (void) lseek((*object)->fd, elementItem->dataOffset, SEEK_SET);
    while (elementLength != 0) {
	sequenceLength = 0;
	memset(&e, 0, sizeof(e));
	flag = readGroupElement("", &ptr, (*object)->fd, &size, &fileOffset,
			     FALSE, byteOrder, explicitVR, acceptVRMismatch,
				object, &sequenceLength, &e);
	if (flag == DCM_STREAMCOMPLETE)
	    break;
	else if (flag != DCM_NORMAL)
	    return flag;

	flag = readVRLength("", &ptr, (*object)->fd, &size, &fileOffset,
			    FALSE,	/* Known length */
			    byteOrder, explicitVR, acceptVRMismatch, object,
			    &sequenceLength, &e);
	if (flag != DCM_NORMAL)
	    return flag;

	elementLength -= sequenceLength + e.length;

	if (firstBuffer) {
	    firstBuffer = FALSE;
	    if (e.length != 0) {
		offsetBuffer = CTN_MALLOC(e.length);
		offsetBufferCount = e.length / sizeof(U32);
		if (offsetBuffer == NULL)
		    exit(1);	/* repair */
		nBytes = read((*object)->fd, offsetBuffer, e.length);
		if (nBytes != e.length) {
		    exit(1);	/* repair */
		}
		if (byteOrder == BYTEORDER_REVERSE) {
		    DCM_ELEMENT offsetBufferElement;
		    memset(&offsetBufferElement, 0, sizeof(DCM_ELEMENT));
		    offsetBufferElement.length = e.length;
		    offsetBufferElement.d.ul = offsetBuffer;
		    offsetBufferElement.representation = DCM_UL;
		    swapInPlace(object, &offsetBufferElement);
		}
		callback(offsetBuffer, e.length, index, 1, 0, 1, ctx);
		streamOffset = 0;
	    } else {
		streamOffset = 0xffffffff;
	    }
	} else {
	    U32 l = e.length;
	    int j;
	    int lastIndex;

	    lastIndex = index;
	    for (j = 0; j < offsetBufferCount; j++) {
		if (streamOffset == offsetBuffer[j])
		    index = j + 1;
	    }
	    startOfFragment = 1;
	    while (l != 0) {
		toRead = MIN(bufSize, l);
		nBytes = read((*object)->fd, buf, toRead);
		if (nBytes != toRead) {
		    exit(1);	/* repair */
		}
		callback(buf, toRead, index,
			 (index != lastIndex) ? 1 : 0,
			 0, startOfFragment, ctx);
		l -= toRead;
		lastIndex = index;	/* Guarantee first flag is off */
		startOfFragment = 0;
	    }
	    streamOffset += sequenceLength + e.length;
	}
	fileOffset += e.length;
	index++;
    }
    callback(buf, 0, index, 0, 1, 1, ctx);
    return DCM_NORMAL;
}

CONDITION
DCM_PrintSequenceList(DCM_OBJECT ** object, DCM_TAG tag)
{
    PRIVATE_OBJECT **obj,
       *sqObject;
    CONDITION cond;
    PRV_ELEMENT_ITEM *elementItem;
    LST_HEAD *lst;
    DCM_SEQUENCE_ITEM *sqItem;

    obj = (PRIVATE_OBJECT **) object;
    cond = checkObject(obj, "DCM_PrintSequenceList");
    if (cond != DCM_NORMAL)
	return cond;

    elementItem = locateElement(obj, tag);

    if (elementItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
		       DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(tag),
				  DCM_TAG_ELEMENT(tag),
				  "DCM_PrintSequenceList");

    lst = elementItem->element.d.sq;
    sqItem = (void *)LST_Head(&lst);
    (void) LST_Position(&lst, (void *)sqItem);
    while (sqItem != NULL) {
	sqObject = (PRIVATE_OBJECT *) sqItem->object;
	RWC_printf("size: %6d offset: %6d, pixel offset: %6d\n",
	       sqObject->objectSize,
	       sqObject->offset,
	       sqObject->pixelOffset);
	sqItem = (void *)LST_Next(&lst);
    }
   return cond ;
}

CONDITION
DCM_GetSequenceByOffset(DCM_OBJECT ** object, DCM_TAG tag, unsigned long offset,
			DCM_OBJECT ** rtnObject)
{
    PRIVATE_OBJECT **obj,
       *sqObject;
    CONDITION cond;
    PRV_ELEMENT_ITEM *elementItem;
    LST_HEAD *lst;
    DCM_SEQUENCE_ITEM *sqItem;

    obj = (PRIVATE_OBJECT **) object;
    cond = checkObject(obj, "DCM_PrintSequenceList");
    if (cond != DCM_NORMAL)
	return cond;

    elementItem = locateElement(obj, tag);

    if (elementItem == NULL)
	return COND_PushCondition(DCM_ELEMENTNOTFOUND,
		       DCM_Message(DCM_ELEMENTNOTFOUND), DCM_TAG_GROUP(tag),
				  DCM_TAG_ELEMENT(tag),
				  "DCM_PrintSequenceList");

    lst = elementItem->element.d.sq;
    sqItem = (void *)LST_Head(&lst);
    (void) LST_Position(&lst, (void *)sqItem);
    while (sqItem != NULL) {
	sqObject = (PRIVATE_OBJECT *) sqItem->object;
	if (sqObject->offset == offset) {
	    *rtnObject = sqItem->object;
	    return DCM_NORMAL;
	}
	sqItem = (void *)LST_Next(&lst);
    }
    return 0;
}

CONDITION
DCM_CopyObject(DCM_OBJECT ** src, DCM_OBJECT ** dst)
{
    PRIVATE_OBJECT **srcObj;
    PRIVATE_OBJECT *dstObj;
    PRV_GROUP_ITEM *groupItem;
    PRV_ELEMENT_ITEM *elementItem;

    if (src == NULL) {
	(void) COND_PushCondition(DCM_NULLADDRESS,
			    DCM_Message(DCM_NULLADDRESS), "DCM_CopyObject");
	return COND_PushCondition(DCM_OBJECTCREATEFAILED,
		     DCM_Message(DCM_OBJECTCREATEFAILED), "DCM_CopyObject");
    }
    dstObj = (PRIVATE_OBJECT *) CTN_MALLOC(sizeof(PRIVATE_OBJECT));
    if (dstObj == NULL) {
	(void) COND_PushCondition(DCM_MALLOCFAILURE,
		     DCM_Message(DCM_MALLOCFAILURE), sizeof(PRIVATE_OBJECT),
				  "DCM_CopyObject");
	*dst = NULL;
	return COND_PushCondition(DCM_OBJECTCREATEFAILED,
		     DCM_Message(DCM_OBJECTCREATEFAILED), "DCM_CopyObject");
    }
    (void) memset(dstObj, 0, sizeof(PRIVATE_OBJECT));
    (void) strcpy(dstObj->keyType, KEY_DCM_OBJECT);

    dstObj->objectType = DCM_OBJECTUNKNOWN;
    dstObj->accessMethod = DCM_MEMORY_ACCESS;
    dstObj->deleteFlag = FALSE;
    dstObj->groupLengthFlag = FALSE;
    dstObj->objectSize = 0;
    dstObj->offset = 0;
    dstObj->pixelSize = 0;
    dstObj->pixelOffset = 0;
    dstObj->pixelBitsAllocated = 0;
    dstObj->pixelRepresentation = 0xffff;
    dstObj->groupCtx = NULL;
    dstObj->elementCtx = NULL;
    dstObj->fd = -1;
    dstObj->fileName[0] = '\0';
    dstObj->preambleFlag = FALSE;
    dstObj->preamble[0] = '\0';
    dstObj->dataOptions = 0;
    dstObj->metaHeaderLength = 0xffffffff;
    dstObj->longVRAttributes = 0;
    dstObj->waveformDataVR[0] = '\0';

    dstObj->groupList = LST_Create();
    if (dstObj->groupList == NULL) {
	CTN_FREE(dstObj);
	*dst = NULL;
	return COND_PushCondition(DCM_LISTFAILURE,
				  DCM_Message(DCM_LISTFAILURE),
				  "DCM_CreateObject");
    }
    srcObj = (PRIVATE_OBJECT **) src;

    groupItem = (void *)LST_Head(&(*srcObj)->groupList);
    if (groupItem != NULL)
	(void) LST_Position(&(*srcObj)->groupList, (void *)groupItem);

    while (groupItem != NULL) {
	elementItem = (void *)LST_Head(&groupItem->elementList);
	if (elementItem != NULL)
	    (void) LST_Position(&groupItem->elementList, (void *)elementItem);
	while (elementItem != NULL) {
	    if (elementItem->element.representation == DCM_SQ) {
		copySequence(&dstObj, &elementItem->element);
	    } else {
		DCM_AddElement((DCM_OBJECT **) & dstObj, &elementItem->element);
	    }
	    elementItem = (void *)LST_Next(&groupItem->elementList);
	}
	groupItem = (void *)LST_Next(&(*srcObj)->groupList);
    }

    *dst = (DCM_OBJECT *) dstObj;
    return DCM_NORMAL;
}

CONDITION
DCM_MergeObject(DCM_OBJECT ** src, DCM_OBJECT ** dst)
{
    PRIVATE_OBJECT **srcObj;
    PRIVATE_OBJECT *dstObj;
    PRV_GROUP_ITEM *groupItem;
    PRV_ELEMENT_ITEM *elementItem;

    if (src == NULL) {
	(void) COND_PushCondition(DCM_NULLADDRESS,
			    DCM_Message(DCM_NULLADDRESS), "DCM_MergeObject");
	return COND_PushCondition(DCM_OBJECTCREATEFAILED,
		     DCM_Message(DCM_OBJECTCREATEFAILED), "DCM_MergeObject");
    }
    dstObj = *((PRIVATE_OBJECT **)dst);
    if (dstObj == NULL) {
	(void) COND_PushCondition(DCM_MALLOCFAILURE,
		     DCM_Message(DCM_MALLOCFAILURE), sizeof(PRIVATE_OBJECT),
				  "DCM_MergeObject");
	*dst = NULL;
	return COND_PushCondition(DCM_OBJECTCREATEFAILED,
		     DCM_Message(DCM_OBJECTCREATEFAILED), "DCM_MergeObject");
    }
    srcObj = (PRIVATE_OBJECT **) src;

    groupItem = (void *)LST_Head(&(*srcObj)->groupList);
    if (groupItem != NULL)
	(void) LST_Position(&(*srcObj)->groupList, (void *)groupItem);

    while (groupItem != NULL) {
	elementItem = (void *)LST_Head(&groupItem->elementList);
	if (elementItem != NULL)
	    (void) LST_Position(&groupItem->elementList, (void *)elementItem);
	while (elementItem != NULL) {
	    if (elementItem->element.representation == DCM_SQ) {
		copySequence(&dstObj, &elementItem->element);
	    } else {
		DCM_AddElement((DCM_OBJECT **) & dstObj, &elementItem->element);
	    }
	    elementItem = (void *)LST_Next(&groupItem->elementList);
	}
	groupItem = (void *)LST_Next(&(*srcObj)->groupList);
    }

    /**dst = (DCM_OBJECT *) dstObj;*/
    return DCM_NORMAL;
}


CONDITION
DCM_GetFirstElement(DCM_OBJECT ** callerObject, DCM_ELEMENT** e)
{
  PRIVATE_OBJECT** object;
  PRV_GROUP_ITEM* groupItem;
  PRV_ELEMENT_ITEM* elementItem;
  CONDITION cond;

  object = (PRIVATE_OBJECT **) callerObject;
  cond = checkObject(object, "DCM_GetFirstElement");
  if (cond != DCM_NORMAL)
    return cond;

  groupItem = (void *)LST_Head(&(*object)->groupList);

  if (groupItem == NULL) {
    *e = 0;
    return DCM_EMPTYOBJECT;
  }
  (void) LST_Position(&(*object)->groupList, (void *)groupItem);
  (*object)->groupCtx = groupItem;

  elementItem = (void *)LST_Head(&groupItem->elementList);
  (*object)->elementCtx = elementItem;
  if (elementItem == NULL) {
    return DCM_GetNextElement(callerObject, e);
  }

  *e = &elementItem->element;
  return DCM_NORMAL;
}

CONDITION
DCM_GetNextElement(DCM_OBJECT ** callerObject, DCM_ELEMENT** e)
{
  PRIVATE_OBJECT** object;
  PRV_GROUP_ITEM* groupItem;
  PRV_ELEMENT_ITEM* elementItem;
  CONDITION cond;

  object = (PRIVATE_OBJECT **) callerObject;
  cond = checkObject(object, "DCM_GetNextElement");
  if (cond != DCM_NORMAL)
    return cond;

  groupItem = (*object)->groupCtx;
  elementItem = (*object)->elementCtx;

  if (elementItem != 0) {
    (void)LST_Position(&groupItem->elementList, (void *)elementItem);
    elementItem = (PRV_ELEMENT_ITEM*)LST_Next(&groupItem->elementList);
  }

  if (elementItem == 0) {
    (void)LST_Position(&(*object)->groupList, (void *)groupItem);
    groupItem = (PRV_GROUP_ITEM*)LST_Next(&(*object)->groupList);
    if (groupItem != 0) {
      elementItem = (PRV_ELEMENT_ITEM*)LST_Head(&groupItem->elementList);
    }
  }

  if (groupItem == 0) {
    *e = 0;
    return DCM_GETNEXTELEMENTCOMPLETE;
  }

  (*object)->groupCtx = groupItem;
  (*object)->elementCtx = elementItem;

  if (elementItem == 0)
    return DCM_GetNextElement(callerObject, e);

  *e = &elementItem->element;
  return DCM_NORMAL;
}

CONDITION
DCM_AddFragment(DCM_OBJECT** callerObject, void* fragment, U32 fragmentLength)
{
  PRIVATE_OBJECT** object;
  PRV_ELEMENT_ITEM* elementItem;
  PRV_ELEMENT_ITEM* newItem;
  CONDITION cond;
  PRV_GROUP_ITEM *groupItem = 0;
  DCM_FRAGMENT_ITEM* fragmentItem;
  U32 mallocLength;

  if ((fragmentLength & 1) != 0) {
    return COND_PushCondition(DCM_UNEVENFRAGMENTLENGTH,
	 DCM_Message(DCM_UNEVENFRAGMENTLENGTH), fragmentLength, "DCM_AddFragment");
  }

  object = (PRIVATE_OBJECT **) callerObject;
  cond = checkObject(object, "DCM_AddFragment");
  if (cond != DCM_NORMAL)
    return cond;

  cond = findCreateGroup(object, 0x7fe0, &groupItem);
  if (cond != DCM_NORMAL)
    return COND_PushCondition(DCM_INSERTFAILED,
	 DCM_Message(DCM_INSERTFAILED), 0x7fe0, 0x0010, "DCM_AddFragment");

  elementItem = locateElement(object, 0x7fe00010);
  if (elementItem == NULL) {
    DCM_ELEMENT e;
    memset(&e, 0, sizeof(e));
    e.tag = DCM_PXLPIXELDATA;
    e.representation = DCM_OB;
    e.multiplicity = 1;
    e.length = 0;
    e.d.fragments = 0;
    cond = newElementItem(&e, FALSE, &newItem);
    if (cond != DCM_NORMAL)
      return cond;
    newItem->element.d.fragments = LST_Create();
    if (newItem->element.d.fragments == NULL) {
      return COND_PushCondition(DCM_LISTFAILURE,
		DCM_Message(DCM_LISTFAILURE), "DCM_AddFragment");
    }
    cond = insertThisElementItem(object, newItem);
    if (cond != DCM_NORMAL)
      return cond;
  }

  elementItem = locateElement(object, 0x7fe00010);

  mallocLength = sizeof(DCM_FRAGMENT_ITEM) + fragmentLength;
  fragmentItem = CTN_MALLOC(mallocLength);
  if (fragmentItem == NULL) {
    return COND_PushCondition(DCM_MALLOCFAILURE,
				DCM_Message(DCM_MALLOCFAILURE), mallocLength,
				"DCM_AddFragment");
  }

  fragmentItem->fragment = ((char*)fragmentItem)+ sizeof(DCM_FRAGMENT_ITEM);
  fragmentItem->length = fragmentLength;
  memcpy(fragmentItem->fragment, fragment, fragmentLength);
  elementItem->fragmentFlag = 1;
  LST_Enqueue(&elementItem->element.d.fragments, (void *)fragmentItem);

  return DCM_NORMAL;
}
/*
          Copyright (C) 1993, 1994, RSNA and Washington University

          The software and supporting documentation for the Radiological
          Society of North America (RSNA) 1993, 1994 Digital Imaging and
          Communications in Medicine (DICOM) Demonstration were developed
          at the
                  Electronic Radiology Laboratory
                  Mallinckrodt Institute of Radiology
                  Washington University School of Medicine
                  510 S. Kingshighway Blvd.
                  St. Louis, MO 63110
          as part of the 1993, 1994 DICOM Central Test Node project for, and
          under contract with, the Radiological Society of North America.

          THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND NEITHER RSNA NOR
          WASHINGTON UNIVERSITY MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
          PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
          USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
          SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
          THE SOFTWARE IS WITH THE USER.

          Copyright of the software and supporting documentation is
          jointly owned by RSNA and Washington University, and free access
          is hereby granted as a license to use this software, copy this
          software and prepare derivative works based upon this software.
          However, any distribution of this software source code or
          supporting documentation or derivative works (source code and
          supporting documentation) must include the three paragraphs of
          the copyright notice.
*/
/* Copyright marker.  Copyright will be inserted above.  Do not remove */

/*
**				DICOM 93
**		     Electronic Radiology Laboratory
**		   Mallinckrodt Institute of Radiology
**		Washington University School of Medicine
**
** Module Name(s):	DCM_Message
** Author, Date:	Stephen M. Moore, 27-Apr-93
** Intent:		Define the ASCIZ messages that go with each DCM
**			error number and provide a function for looking up
**			the error message.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

typedef struct vector {
    CONDITION cond;
    char *message;
}   VECTOR;

static VECTOR messageVector[] = {
    {DCM_NORMAL, "Normal return from DCM routine"},
    {DCM_FILEOPENFAILED, "DCM failed to open file: %s in %s"},
    {DCM_FILEACCESSERROR, "DCM failed to access file: %s in %s"},
    {DCM_OBJECTCREATEFAILED, "DCM failed to create object in %s"},
    {DCM_NULLOBJECT, "NULL object passed to routine %s"},
    {DCM_ILLEGALOBJECT, "Illegal object passed to routine %s"},
    {DCM_ELEMENTNOTFOUND, "Requested element (%x %x) not found in %s"},
    {DCM_ILLEGALSTREAMLENGTH,
    "DCM Illegal Stream Length (%ld) (Not enough data to define a full element) in %s"},
    {DCM_ELEMENTCREATEFAILED, "DCM failed to create element in %s (%04x %04x %d)"},
    {DCM_UNRECOGNIZEDGROUP, "DCM unrecognized group: %04x in %s"},
    {DCM_UNRECOGNIZEDELEMENT, "DCM unrecognized element: (%04x %04x) in %s"},
    {DCM_ELEMENTOUTOFORDER, "DCM group/element out of order (%04x %04x) in %s"},
    {DCM_LISTFAILURE, "DCM routine failed on list operation in %s"},
    {DCM_ILLEGALOPTION, "DCM illegal stream option: %s"},
    {DCM_ILLEGALADD, "DCM attempt to add illegal element: %x %x in %s"},
    {DCM_GETINCOMPLETE, "DCM Get Element incomplete in %s"},
    {DCM_ILLEGALCONTEXT, "DCM Illegal context value in %s"},
    {DCM_ILLEGALREPRESENTATION,
    "DCM Caller specified illegal representation for element (%04x %04x) in %s"},
    {DCM_UNEVENELEMENTLENGTH,
    "DCM attempt to add data element (%x %x) with uneven length (%ld)  in %s"},
    {DCM_ELEMENTLENGTHERROR,
    "DCM Data Element (%04x %04x) longer (%ld) than remaining length (%ld) of \
data in stream or file in %s"},
    {DCM_GROUPNOTFOUND, "Requested group (%x) not found in %s"},
    {DCM_FILECREATEFAILED, "DCM failed to create file %s (%s)"},
    {DCM_FILEIOERROR, "DCM io error on file %s (%s)"},
    {DCM_INSERTFAILED,
    "DCM failed to insert new element (%04x %04x) in %s"},
    {DCM_CANNOTGETSEQUENCEVALUE,
    "DCM Cannot retrieve value of element with SQ representation (%08x) in (%s)"},
    {DCM_FILEDELETEFAILED, "DCM Failed to delete file %s in %s"},
    {DCM_MALLOCFAILURE, "DCM Failed to malloc %ld bytes in %s"},
    {DCM_NULLADDRESS, "DCM NULL address passed to routine %s"},
    {DCM_UNEXPECTEDREPRESENTATION,
    "DCM Routine %s expected %s representation for element %04x %04x"},
    {DCM_BADELEMENTINGROUP,
    "DCM Bad element (%04x %04x) found in group %04x in %s"},
    {DCM_CALLBACKABORTED, "DCM Callback aborted by user in %s"},
    {DCM_READSTREAMFAILED, "DCM Failed to read stream in %s"},
    {DCM_UNRECOGNIZEDVRCODE, "DCM Unrecognized VR code (%s) in %s"},
    {DCM_VRMISMATCH, "DCM Incorrect VR (%s) for attribute with tag %08x"},
    {DCM_EXPORTBUFFERTOOSMALL,
    "DCM Caller's export buffer length (%d) is too short in %s"},
    {DCM_BADOFFSET,
    "DCM Offset value (%d) larger than attribute length (%d) in %s"},
    {DCM_BADLENGTH,
    "DCM Combination of offset, length (%d %d) is longer than element length (%d) in %s"},
    {DCM_NOTASEQUENCE,
    "DCM Attempt to perform sequence operation on element (%04x %04x) not a sequence in %s"},
    {DCM_GENERALWARNING, "DCM General warning in %s: %s"},
    {DCM_UNEVENFRAGMENTLENGTH,
    "DCM attempt to add fragment with uneven length (%ld) in %s"},
    {0, NULL}

};


/* DCM_Message
**
** Purpose:
**	Find the ASCIZ message that goes with an DCM error number and
**	return a pointer to static memory containing that error message.
**
** Parameter Dictionary:
**	condition	The error condition for which the message is to be
**			returned
**
** Return Values:
**	The error message if a valid error condition was reported else NULL.
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

char *
DCM_Message(CONDITION condition)
{
    int
        index;

    for (index = 0; messageVector[index].message != NULL; index++)
	if (condition == messageVector[index].cond)
	    return messageVector[index].message;

    return NULL;
}

void DCM_DumpVector()
{
    int index;

    for (index = 0; index < (int) DIM_OF(messageVector); index++) {
	if (messageVector[index].message != NULL)
	    RWC_printf("%8x %8d %s\n", messageVector[index].cond,
		   messageVector[index].cond,
		   messageVector[index].message);
    }
    return ;
}

/*
          Copyright (C) 1993, 1994, RSNA and Washington University

          The software and supporting documentation for the Radiological
          Society of North America (RSNA) 1993, 1994 Digital Imaging and
          Communications in Medicine (DICOM) Demonstration were developed
          at the
                  Electronic Radiology Laboratory
                  Mallinckrodt Institute of Radiology
                  Washington University School of Medicine
                  510 S. Kingshighway Blvd.
                  St. Louis, MO 63110
          as part of the 1993, 1994 DICOM Central Test Node project for, and
          under contract with, the Radiological Society of North America.

          THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND NEITHER RSNA NOR
          WASHINGTON UNIVERSITY MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
          PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
          USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
          SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
          THE SOFTWARE IS WITH THE USER.

          Copyright of the software and supporting documentation is
          jointly owned by RSNA and Washington University, and free access
          is hereby granted as a license to use this software, copy this
          software and prepare derivative works based upon this software.
          However, any distribution of this software source code or
          supporting documentation or derivative works (source code and
          supporting documentation) must include the three paragraphs of
          the copyright notice.
*/
/* Copyright marker.  Copyright will be inserted above.  Do not remove */

/*
**				DICOM 93
**		     Electronic Radiology Laboratory
**		   Mallinckrodt Institute of Radiology
**		Washington University School of Medicine
**
** Module Name(s):	DCM_LookupElement(DCM_ELEMENT *element)
** Author, Date:	Stephen M. Moore, 30-Apr-93
** Intent:		This module contains the routine and data which
**			define the DICOM data dictionary.  A number of
**			static objects are maintained which define how
**			elements in the DICOM V3.0 standard are to be
**			interpreted.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

/*  The DCM dictionary consists of a list of lists.  Each group (COMMAND,
**  IMAGE, ...) is defined in a list separately.  The object DCMDICT
**  below is used to define the entry for a single data element in a
**  known group.  We define the fields:
**	element
**	representation
**	english Description
**  The outer layer DCM dictionary consists of a list of groups.  The
**  group entries define the "group" number and give a pointer to the
**  DCMDICT list for that group.  The intent is to search the outer layer
**  dictionary to find the proper group, then search the particular group
**  list to find the proper element.
*/
typedef struct {
    DCM_TAG tag;
    DCM_VALUEREPRESENTATION representation;
    char englishDescription[48];
}   DCMDICT;

typedef struct {
    unsigned short group;
    unsigned long entries;
    DCMDICT *dict;
}   GROUPPTR;


/*  Define the entries for the COMMAND group
*/
static DCMDICT CMD_dictionary[] = {
    {DCM_CMDGROUPLENGTH, DCM_UL, "CMD Group Length"},
    {DCM_CMDAFFECTEDCLASSUID, DCM_UI, "CMD Affected SOP Class UID"},
    {DCM_CMDREQUESTEDCLASSUID, DCM_UI, "CMD Requested SOP Class UID"},
    {DCM_CMDCOMMANDFIELD, DCM_US, "CMD Command Field"},
    {DCM_CMDMSGID, DCM_US, "CMD Message ID"},
    {DCM_CMDMSGIDRESPOND, DCM_US, "CMD Message ID Responded to"},
    {DCM_CMDMOVEDESTINATION, DCM_AE, "CMD Move Destination"},
    {DCM_CMDPRIORITY, DCM_US, "CMD Priority"},
    {DCM_CMDDATASETTYPE, DCM_US, "CMD Data Set Type"},
    {DCM_CMDSTATUS, DCM_US, "CMD Status"},
    {DCM_CMDOFFENDINGELEMENT, DCM_AT, "CMD Offending Element"},
    {DCM_CMDERRORCOMMENT, DCM_LO, "CMD Error Comment"},
    {DCM_CMDERRORID, DCM_US, "CMD Error ID"},
    {DCM_CMDREQUESTEDINSTANCEUID, DCM_UI, "CMD SOP Requested Instance UID"},
    {DCM_CMDAFFECTEDINSTANCEUID, DCM_UI, "CMD SOP Affected Instance UID"},
    {DCM_CMDEVENTTYPEID, DCM_US, "CMD Event Type ID"},
    {DCM_CMDACTIONTYPEID, DCM_US, "CMD Action Type ID"},
    {DCM_CMDREMAININGSUBOPERATIONS, DCM_US, "CMD Remaining Suboperations"},
    {DCM_CMDCOMPLETEDSUBOPERATIONS, DCM_US, "CMD Completed Suboperations"},
    {DCM_CMDFAILEDSUBOPERATIONS, DCM_US, "CMD Failed Suboperations"},
    {DCM_CMDWARNINGSUBOPERATIONS, DCM_US, "CMD Warning Suboperations"},
    {DCM_CMDMOVEAETITLE, DCM_AE, "CMD AE Title"},
    {DCM_CMDMOVEMESSAGEID, DCM_US, "CMD Message ID"},
    {DCM_CMDATTRIBUTEIDLIST, DCM_AT, "CMD Attribute Identifier List"},

};

/* Define the entries for the file Meta Header group
*/

static DCMDICT META_dictionary[] = {
    {DCM_METAGROUPLENGTH, DCM_UL, "META Group Length"},
    {DCM_METAINFORMATIONVERSION, DCM_OB, "META File Meta Information Version"},
    {DCM_METAMEDIASTORAGESOPCLASS, DCM_UI, "META Media Stored SOP Class UID"},
    {DCM_METAMEDIASTORAGESOPINSTANCE, DCM_UI, "META Media Stored SOP Instance UID"},
    {DCM_METATRANSFERSYNTAX, DCM_UI, "META Transfer Syntax UID"},
    {DCM_METAIMPLEMENTATIONCLASS, DCM_UI, "META Implementation Class UID"},
    {DCM_METAIMPLEMENTATIONVERSION, DCM_SH, "META Implementation Version Name"},
    {DCM_METASOURCEAETITLE, DCM_AE, "META Source Application Entity Title"},
    {DCM_METAPRIVATEINFORMATIONCREATOR, DCM_UI, "META Private Information Creator"},
    {DCM_METAPRIVATEINFORMATION, DCM_OB, "META Private Information"}
};

/* Define the elements in the Basic Directory Information Group, 0x0004 */

static DCMDICT BASICDIR_dictionary[] = {
    {DCM_DIRFILESETID, DCM_CS, "DIR File-set ID"},
    {DCM_DIRFILESETDESCRFILEID, DCM_CS, "DIR File-set descriptor ID"},
    {DCM_DIRSPECIFICCHARACTER, DCM_CS, "DIR Specific character set"},
    {DCM_DIRFIRSTOFFSET, DCM_UL, "DIR Offset of the first dir of root dir entity"},
    {DCM_DIRLASTOFFSET, DCM_UL, "DIR Offset of the last dir of root dir entity"},
    {DCM_DIRFILESETCONSISTENCY, DCM_US, "DIR File-set consistency flag"},
    {DCM_DIRRECORDSEQUENCE, DCM_SQ, "DIR Directory record sequence"},
    {DCM_DIRNEXTRECORDOFFSET, DCM_UL, "DIR Offset of next directory record"},
    {DCM_DIRRECORDINUSE, DCM_US, "DIR Record in use flag"},
    {DCM_DIRLOWERLEVELOFFSET, DCM_UL, "DIR Offset of referenced lower-level dir entity"},
    {DCM_DIRRECORDTYPE, DCM_CS, "DIR Directory Record Type"},
    {DCM_DIRPRIVATERECORDUID, DCM_UI, "DIR Private Record UID"},
    {DCM_DIRREFERENCEDFILEID, DCM_CS, "DIR Referenced File ID"},
    {DCM_DIRMRDRRECORDOFFSET, DCM_UL, "DIR Directory Record Offset"},
    {DCM_DIRREFSOPCLASSUID, DCM_UI, "DIR Referenced SOP Class UID in File"},
    {DCM_DIRREFSOPINSTANCEUID, DCM_UI, "DIR Referenced SOP Instance UID in File"},
    {DCM_DIRREFTRANSFERSYNTAXUID, DCM_UI, "DIR Referenced Transfer Syntax in File"},
    {DCM_DIRNUMREFERENCES, DCM_UL, "DIR Number of References"}
};

/* Define the entries for the IDENTIFYING group
*/
static DCMDICT ID_dictionary[] = {
    {DCM_IDGROUPLENGTH, DCM_UL, "ID Group Length"},
/*    {DCM_IDLENGTHTOEND, DCM_RET, "ID Length to End (RET)"}, */
    {DCM_IDLENGTHTOEND, DCM_UL, "ID Length to End (RET)"},
    {DCM_IDSPECIFICCHARACTER, DCM_CS, "ID Specific Character Set"},
    {DCM_IDIMAGETYPE, DCM_CS, "ID Image Type"},
    {DCM_IDRECOGNITIONCODE, DCM_RET, "ID Recognition Code (RET)"},
    {DCM_IDINSTANCECREATEDATE, DCM_DA, "ID Instance Creation Date"},
    {DCM_IDINSTANCECREATETIME, DCM_TM, "ID Instance Creation Time"},
    {DCM_IDINSTANCECREATORUID, DCM_UI, "ID Instance Creator UID"},
    {DCM_IDSOPCLASSUID, DCM_UI, "ID SOP Class UID"},
    {DCM_IDSOPINSTANCEUID, DCM_UI, "ID SOP Instance UID"},
    {DCM_IDSTUDYDATE, DCM_DA, "ID Study Date"},
    {DCM_IDSERIESDATE, DCM_DA, "ID Series Date"},
    {DCM_IDACQUISITIONDATE, DCM_DA, "ID Acquisition Date"},
    {DCM_IDIMAGEDATE, DCM_DA, "ID Image Date"},
    {DCM_IDOVERLAYDATE, DCM_DA, "ID Overlay Date"},
    {DCM_IDCURVEDATE, DCM_DA, "ID Curve Date"},
    {DCM_IDSTUDYTIME, DCM_TM, "ID Study Time"},
    {DCM_IDSERIESTIME, DCM_TM, "ID Series Time"},
    {DCM_IDACQUISITIONTIME, DCM_TM, "ID Acquisition Time"},
    {DCM_IDIMAGETIME, DCM_TM, "ID Image Time"},
    {DCM_IDOVERLAYTIME, DCM_TM, "ID Overlay Time"},
    {DCM_IDCURVETIME, DCM_TM, "ID Curve Time"},
    {DCM_IDDATASETTYPE, DCM_RET, "ID Data Set Type (RET)"},
    {DCM_IDDATASETSUBTYPE, DCM_RET, "ID Data Set Subtype (RET)"},
    {DCM_IDNMSERIESTYPE, DCM_CS, "ID Nuc Med Series Type (RET)"},
    {DCM_IDACCESSIONNUMBER, DCM_SH, "ID Accession Number"},
    {DCM_IDQUERYLEVEL, DCM_CS, "ID Query Level"},
    {DCM_IDRETRIEVEAETITLE, DCM_AE, "ID Retrieve AE Title"},
    {DCM_IDINSTANCEAVAILABILITY, DCM_CS, "ID Instance Availability"},
    {DCM_IDFAILEDINSTANCEUIDLIST, DCM_UI, "ID Failed SOP Instances"},
    {DCM_IDMODALITY, DCM_CS, "ID Modality"},
    {DCM_IDMODALITIESINSTUDY, DCM_CS, "ID Modalities in Study"},
    {DCM_IDMODALITYSUBTYPE, DCM_SQ, "ID Modality Subtype"},	/* Sup 30 0.6 */
    {DCM_IDPRESENTATIONINTENTTYPE, DCM_CS, "ID Presentation Intent Type"},
    {DCM_IDCONVERSIONTYPE, DCM_CS, "ID Conversion Type"},
    {DCM_IDMANUFACTURER, DCM_LO, "ID Manufacturer"},
    {DCM_IDINSTITUTIONNAME, DCM_LO, "ID Institution Name"},
    {DCM_IDINSTITUTIONADDR, DCM_ST, "ID Institution Address"},
    {DCM_IDINSTITUTECODESEQUENCE, DCM_SQ, "ID Institution Code Sequence"},
    {DCM_IDREFERRINGPHYSICIAN, DCM_PN, "ID Referring Physician's Name"},
    {DCM_IDREFERRINGPHYSADDR, DCM_ST, "ID Referring Physician's Address"},
    {DCM_IDREFERRINGPHYSPHONE, DCM_SH, "ID Referring Physician's Telephone"},
    {DCM_IDCODEVALUE, DCM_SH, "ID Code Value"},
    {DCM_IDCODINGSCHEMEDESIGNATOR, DCM_SH, "ID Coding Scheme Designator"},
    {DCM_IDCODINGSCHEMEVERSION, DCM_SH, "ID Coding Scheme Version"},
    /* Sup 15, Version 1.2_interim_971226 */
    {DCM_IDCODEMEANING, DCM_LO, "ID Code Meaning"},
    {DCM_IDMAPPINGRESOURCE, DCM_CS, "ID Mapping Resource"},	/* Sup 15, 1.1a */
    {DCM_IDCONTEXTGROUPVERSION, DCM_DT, "ID Context Group Version"},
    /* Sup 15, Version 1.1a */
    {DCM_IDCODESETEXTENSIONFLAG, DCM_CS, "ID Code Set Extension Flag"},
    /* 0x010B: Sup 15, Version 1.a */
    {DCM_IDPRIVATECODINGSCHEMECREATORUID, DCM_UI, "ID Private Coding Scheme Creator UID"},
    /* 0x010C: Sup 15, Version 1.1 */
    {DCM_IDCODESETEXTENSIONCREATORUID, DCM_UI, "ID Coding Scheme Creator UID"},
    /* 0x010D: Sup 15, Version 1.1 */
    {DCM_IDMAPPINGRESOURCESEQ, DCM_SQ, "ID Mapping Resource Sequence"},
    /* Sup 15, Version 1.1 */
    {DCM_IDCONTEXTIDENTIFIER, DCM_CS, "ID Context Identifier"},	/* Sup 15 */
    {DCM_IDNETWORKID, DCM_LO, "ID Network ID (RET)"},
    {DCM_IDSTATIONNAME, DCM_SH, "ID Station Name"},
    {DCM_IDSTUDYDESCRIPTION, DCM_LO, "ID Study Description"},
    {DCM_IDPROCEDURECODESEQUENCE, DCM_SQ, "ID Procedure Code Sequence"},
    {DCM_IDSERIESDESCR, DCM_LO, "ID Series Description"},
    {DCM_IDINSTITUTIONALDEPT, DCM_LO, "ID Institutional Department Name"},
    {DCM_IDPHYSICIANOFRECORD, DCM_PN, "ID Physician of Record"},
    {DCM_IDPERFORMINGPHYSICIAN, DCM_PN, "ID Performing Physician's Name"},
    {DCM_IDPHYSREADINGSTUDY, DCM_PN, "ID Name of Physician(s) Reading Study"},
    {DCM_IDOPERATORNAME, DCM_PN, "ID Operator's Name"},
    {DCM_IDADMITTINGDIAGDESCR, DCM_LO, "ID Admitting Diagnoses Description"},
    {DCM_IDADMITDIAGCODESEQUENCE, DCM_SQ, "ID Admitting Diagnosis Code Sequence"},
    {DCM_IDMANUFACTURERMODEL, DCM_LO, "ID Manufacturer Model Name"},
    {DCM_IDREFERENCEDRESULTSSEQ, DCM_SQ, "ID Referenced Results Sequence"},
    {DCM_IDREFERENCEDSTUDYSEQ, DCM_SQ, "ID Referenced Study Sequence"},
    {DCM_IDREFERENCEDSTUDYCOMPONENTSEQ, DCM_SQ, "ID Referenced Study Component Sequence"},
    {DCM_IDREFERENCEDSERIESSEQ, DCM_SQ, "ID Referenced Series Sequence"},
    {DCM_IDREFERENCEDPATIENTSEQ, DCM_SQ, "ID Referenced Patient Sequence"},
    {DCM_IDREFERENCEDVISITSEQ, DCM_SQ, "ID Referenced Visit Sequence"},
    {DCM_IDREFERENCEDOVERLAYSEQ, DCM_SQ, "ID Referenced Overlay Sequence"},
    {DCM_IDREFERENCEDIMAGESEQ, DCM_SQ, "ID Referenced Image Sequence"},
    {DCM_IDREFERENCEDCURVESEQ, DCM_SQ, "ID Referenced Curve Sequence"},
    {DCM_IDREFERENCEDPREVIOUSWAVEFORM, DCM_SQ, "ID Referenced Previous Waveform"},	/* Sup 30 0.6 */
    {DCM_IDREFERENCEDSIMULTANEOUSWAVEFORMS, DCM_SQ, "ID Referenced Simultaneous Waveforms"},	/* Sup 30 0.6 */
    {DCM_IDREFERENCEDSUBSEQUENTWAVEFORM, DCM_SQ, "ID Referenced Subsequent Waveform"},	/* Sup 30 0.6 */
    {DCM_IDREFERENCEDSOPCLASSUID, DCM_UI, "ID Referenced SOP Class UID"},
    {DCM_IDREFERENCEDSOPINSTUID, DCM_UI, "ID Referenced SOP Instance UID"},
    {DCM_IDREFERENCEDFRAMENUMBER, DCM_IS, "ID Referenced Frame Number"},
    {DCM_IDTRANSACTIONUID, DCM_UI, "ID Transaction UID"},
    {DCM_IDFAILUREREASON, DCM_US, "ID Failure Reason"},
    {DCM_IDFAILEDSOPSEQUENCE, DCM_SQ, "ID Failed SOP Sequence"},
    {DCM_IDREFERENCEDSOPSEQUENCE, DCM_SQ, "ID Referenced SOP Sequence"},
    {DCM_IDLOSSYIMAGECOMPRESSION, DCM_CS, "ID Lossy Image Compression (RET)"},
    {DCM_IDDERIVATIONDESCR, DCM_ST, "ID Derivation Description"},
    {DCM_IDSOURCEIMAGESEQ, DCM_SQ, "ID Source Image Sequence"},
    {DCM_IDSTAGENAME, DCM_SH, "ID Stage Name"},
    {DCM_IDSTAGENUMBER, DCM_IS, "ID Stage Number"},
    {DCM_IDNUMBEROFSTAGES, DCM_IS, "ID Number of Stages"},
    {DCM_IDVIEWNUMBER, DCM_IS, "ID View Number"},
    {DCM_IDNUMBEROFEVENTTIMERS, DCM_IS, "ID Number of Event Timers"},
    {DCM_IDNUMBERVIEWSINSTAGE, DCM_IS, "ID Number of Views in Stage"},
    {DCM_IDEVENTELAPSEDTIME, DCM_DS, "ID Event Elapsed Time(s)"},
    {DCM_IDEVENTTIMERNAME, DCM_LO, "ID Event Event Timer Name(s)"},
    {DCM_IDSTARTTRIM, DCM_IS, "ID Start Trim"},
    {DCM_IDSTOPTRIM, DCM_IS, "ID Stop Trim"},
    {DCM_IDDISPLAYFRAMERATE, DCM_IS, "ID Recommended Display Frame Rate"},
    {DCM_IDTRANSDUCERPOSITION, DCM_CS, "ID Transducer Position (RET)"},
    {DCM_IDTRANSDUCERORIENTATION, DCM_CS, "ID Transducer Orientation (RET)"},
    {DCM_IDANATOMICSTRUCTURE, DCM_CS, "ID Anatomic Structure (RET)"},
    {DCM_IDANATOMICREGIONSEQUENCE, DCM_SQ, "ID Anatomic Region of Interest Sequence"},
    {DCM_IDANATOMICREGIONMODIFIERSEQ, DCM_SQ,
    "ID Anatomic Region Modifier Sequence"},
    {DCM_IDPRIMARYANATOMICSTRUCTURESEQ, DCM_SQ,
    "ID Primary Anatomic Structure Sequence"},
    {DCM_IDPRIMARYANATOMICSTRUCTUREMODIFIERSEQ, DCM_SQ,
    "ID Primary Anatomic Structure Modifier Sequence"},
    {DCM_IDTRANSDUCERPOSITIONSEQ, DCM_SQ, "ID Transducer Position Sequence"},
    {DCM_IDTRANSDUCERPOSITIONMODIFIERSEQ, DCM_SQ, "ID Transducer Position Modifer Sequence"},
    {DCM_IDTRANSDUCERORIENTATIONSEQ, DCM_SQ, "ID Transducer Orientation Sequence"},
    {DCM_IDTRANSDUCERORIENTATIONMODIFIERSEQ, DCM_SQ, "ID Transducer Orientation Modifer Sequence"},
    {DCM_IDCOMMENTS, DCM_RET, "ID Comments (RET)"}
};

/* Define the entries for the PATIENT INFORMATION group
*/
static DCMDICT PAT_dictionary[] = {
    {DCM_PATGROUPLENGTH, DCM_UL, "PAT Group Length"},
    {DCM_PATNAME, DCM_PN, "PAT Patient Name"},
    {DCM_PATID, DCM_LO, "PAT Patient ID"},
    {DCM_ISSUERPATIENTID, DCM_LO, "PAT Issuer of Patient ID"},
    {DCM_PATBIRTHDATE, DCM_DA, "PAT Patient Birthdate"},
    {DCM_PATBIRTHTIME, DCM_TM, "PAT Patient Birth Time"},
    {DCM_PATSEX, DCM_CS, "PAT Patient Sex"},
    {DCM_PATINSURANCEPLANCODESEQ, DCM_SQ, "PAT Patient's Insurance Plan Code Sequence"},
    {DCM_PATOTHERIDS, DCM_LO, "PAT Other Patient IDs"},
    {DCM_PATOTHERNAMES, DCM_PN, "PAT Other Patient Names"},
    {DCM_PATBIRTHNAME, DCM_PN, "PAT Patient's Birth Name "},
    {DCM_PATAGE, DCM_AS, "PAT Patient Age"},
    {DCM_PATSIZE, DCM_DS, "PAT Patient Size"},
    {DCM_PATWEIGHT, DCM_DS, "PAT Patient Weight"},
    {DCM_PATADDRESS, DCM_LO, "PAT Patient Address"},
    {DCM_PATINSURANCEPLANID, DCM_RET, "PAT Insurance Plan Identifier"},
    {DCM_PATMOTHERBIRTHNAME, DCM_PN, "PAT Patient's Mother's Birth Name"},
    {DCM_PATMILITARYRANK, DCM_LO, "PAT Military Rank"},
    {DCM_PATBRANCHOFSERVICE, DCM_LO, "PAT Branch of Service"},
    {DCM_PATMEDICALRECORDLOCATOR, DCM_LO, "PAT Medical Record Locator"},
    {DCM_PATMEDICALALERTS, DCM_LO, "PAT Medical Alerts"},
    {DCM_PATCONTRASTALLERGIES, DCM_LO, "PAT Contrast Allergies"},
    {DCM_COUNTRYOFRESIDENCE, DCM_LO, "PAT Country of Residence"},
    {DCM_REGIONOFRESIDENCE, DCM_LO, "PAT Region of Residence"},
    {DCM_PATTELEPHONENUMBER, DCM_SH, "PAT Patient's Telephone Numbers"},
    {DCM_PATETHNICGROUP, DCM_SH, "PAT Ethnic Group"},
    {DCM_PATOCCUPATION, DCM_SH, "PAT Occupation"},
    {DCM_PATSMOKINGSTATUS, DCM_CS, "PAT Smoking Status"},
    {DCM_PATADDITIONALPATHISTORY, DCM_LT, "PAT Additional Patient History"},
    {DCM_PATPREGNANCYSTATUS, DCM_US, "PAT Pregnancy Status"},
    {DCM_PATLASTMENSTRUALDATE, DCM_DA, "PAT Last Menstrual Date"},
    {DCM_PATRELIGIOUSPREFERENCE, DCM_LO, "PAT Religious Preference"},
    {DCM_PATCOMMENTS, DCM_LT, "PAT Comments"}
};

/* Define the entries for the ACQUISITION group, 0018
*/

static DCMDICT ACQ_dictionary[] = {
    {DCM_ACQGROUPLENGTH, DCM_UL, "ACQ Group Length"},
    {DCM_ACQCONTRASTBOLUSAGENT, DCM_LO, "ACQ Contrast/Bolus Agent"},
    {DCM_ACQCONTRASTBOLUSAGENTSEQ, DCM_SQ, "ACQ Contrast/Bolus Agent Sequence"},
    {DCM_ACQCONTRASTBOLUSADMINROUTESEQ, DCM_SQ, "ACQ Contrast/Bolus Administration Route Seq"},
    {DCM_ACQBODYPARTEXAMINED, DCM_CS, "ACQ Body Part Examined"},
    {DCM_ACQSCANNINGSEQUENCE, DCM_CS, "ACQ Scanning Sequence"},
    {DCM_ACQSEQUENCEVARIANT, DCM_CS, "ACQ Sequence Variant"},
    {DCM_ACQSCANOPTIONS, DCM_CS, "ACQ Scan Options"},
    {DCM_ACQMRACQUISITIONTYPE, DCM_CS, "ACQ MR Acquisition Type "},
    {DCM_ACQSEQUENCENAME, DCM_SH, "ACQ Sequence Name"},
    {DCM_ACQANGIOFLAG, DCM_CS, "ACQ Angio Flag"},
    {DCM_ACQINTERVENTIONDRUGINFOSEQ, DCM_SQ, "ACQ Intervention Drug Information Sequence"},
    {DCM_ACQINTERVENTIONDRUGSTOPTIME, DCM_TM, "ACQ Intervention Drug Stop Time"},
    {DCM_ACQINTERVENTIONDRUGDOSE, DCM_DS, "ACQ Intervention Drug Dose"},
    {DCM_ACQINTERVENTIONDRUGCODESEQ, DCM_SQ, "ACQ Intervention Drug Code Sequence"},
    {DCM_ACQADDITIONALDRUGSEQ, DCM_SQ, "ACQ Additional Drug Sequence"},
    {DCM_ACQRADIONUCLIDE, DCM_LO, "ACQ Radionuclide (RET)"},
    {DCM_ACQRADIOPHARMACEUTICAL, DCM_LO, "ACQ Radiopharmaceutical"},
    {DCM_ACQENERGYWCENTERLINE, DCM_DS, "ACQ Energy Window Centerline (RET)"},
    {DCM_ACQENERGYWTOTALWIDTH, DCM_DS, "ACQ Energy Window Total Width (RET)"},
    {DCM_ACQINTERVENTIONDRUGNAME, DCM_LO, "ACQ Intervention Drug Name"},
    {DCM_ACQINTERVENTIONDRUGSTART, DCM_TM, "ACQ Intervention Drug Start Time"},
    {DCM_ACQINTERVENTIONALTHERAPYSEQ, DCM_SQ, "ACQ Interventional Therapy Sequence"},
    {DCM_ACQTHERAPYTYPE, DCM_CS, "ACQ Therapy type"},
    {DCM_ACQINTERVENTIONALSTATUS, DCM_CS, "ACQ Interventional status"},
    {DCM_ACQTHERAPYDESCRIPTION, DCM_CS, "ACQ Therapy descriptionm"},
    {DCM_ACQCINERATE, DCM_IS, "ACQ Cine Rate"},
    {DCM_ACQSLICETHICKNESS, DCM_DS, "ACQ Slice Thickness"},
    {DCM_ACQKVP, DCM_DS, "ACQ KVP"},
    {DCM_ACQCOUNTSACCUMULATED, DCM_IS, "ACQ Counts Accumulated"},
    {DCM_ACQTERMINATIONCONDITION, DCM_CS, "ACQ Acquisition Termination Condition"},
    {DCM_ACQEFFECTIVESERIESDURATION, DCM_DS, "ACQ Effective Series Duration"},
    {DCM_ACQSTARTCONDITION, DCM_CS, "ACQ Start Condition"},
    {DCM_ACQSTARTCONDITIONDATA, DCM_IS, "ACQ Start Condition Data"},
    {DCM_ACQTERMINATIONCONDITIONDATA, DCM_IS, "ACQ Termination Condition Data"},
    {DCM_ACQREPETITIONTIME, DCM_DS, "ACQ Repetition Time"},
    {DCM_ACQECHOTIME, DCM_DS, "ACQ Echo Time"},
    {DCM_ACQINVERSIONTIME, DCM_DS, "ACQ Inversion Time"},
    {DCM_ACQNUMBEROFAVERAGES, DCM_DS, "ACQ Number of Averages"},
    {DCM_ACQIMAGINGFREQUENCY, DCM_DS, "ACQ Imaging Frequency"},
    {DCM_ACQIMAGEDNUCLEUS, DCM_SH, "ACQ Imaged Nucleus"},
    {DCM_ACQECHONUMBER, DCM_IS, "ACQ Echo Number"},
    {DCM_ACQMAGNETICFIELDSTRENGTH, DCM_DS, "ACQ Magnetic Field Strength"},
    {DCM_ACQSLICESPACING, DCM_DS, "ACQ Spacing Between Slices"},
    {DCM_ACQPHASEENCODINGSTEPS, DCM_IS, "ACQ Number of Phase Encoding Steps"},
    {DCM_ACQDATACOLLECTIONDIAMETER, DCM_DS, "ACQ Data Collection Diameter"},
    {DCM_ACQECHOTRAINLENGTH, DCM_IS, "ACQ Echo Train Length"},
    {DCM_ACQPERCENTSAMPLING, DCM_DS, "ACQ Percent Sampling"},
    {DCM_ACQPERCENTPHASEFIELDVIEW, DCM_DS, "ACQ Percent Phase Field of View"},
    {DCM_ACQPIXELBANDWIDTH, DCM_DS, "ACQ Pixel Bandwidth"},
    {DCM_ACQDEVICESERIALNUM, DCM_LO, "ACQ Device Serial Number"},
    {DCM_ACQPLATEID, DCM_LO, "ACQ Plate ID"},
    {DCM_ACQSECONDARYCAPTUREDEVID, DCM_LO, "ACQ Secondary Capture Device ID"},
    {DCM_ACQDATESECONDARYCAPTURE, DCM_DA, "ACQ Date of Secondary Capture"},
    {DCM_ACQTIMESECONDARYCAPTURE, DCM_TM, "ACQ Time of Secondary Capture"},
    {DCM_ACQSECONDARYCAPTMANUFACTURER, DCM_LO,
    "ACQ Secondary Capture Device Manufacturer"},
    {DCM_ACQSECONDARYCAPTMODEL, DCM_LO, "ACQ Secondary Capture Device Model Name"},
    {DCM_ACQSECONDARYCAPTSOFTWAREVERSION, DCM_LO,
    "ACQ Secondary Capture Device Software Version"},
    {DCM_ACQSOFTWAREVERSION, DCM_LO, "ACQ Software Version"},
    {DCM_ACQVIDEOIMAGEFORMATACQ, DCM_SH, "ACQ Video Image Format Acquired"},
    {DCM_ACQDIGITALIMAGEFORMATACQ, DCM_LO, "ACQ Digital Image Format Acquired"},
    {DCM_ACQPROTOCOLNAME, DCM_LO, "ACQ Protocol Name"},
    {DCM_ACQCONTRASTBOLUSROUTE, DCM_LO, "ACQ Contrast/Bolus Route"},
    {DCM_ACQCONTRASTBOLUSVOL, DCM_DS, "ACQ Contrast/Bolus Volume"},
    {DCM_ACQCONTRASTBOLUSSTARTTIME, DCM_TM, "ACQ Contrast/Bolus Start Time"},
    {DCM_ACQCONTRASTBOLUSSTOPTIME, DCM_TM, "ACQ Contrast/Bolus Stop Time"},
    {DCM_ACQCONTRASTBOLUSTOTALDOSE, DCM_DS, "ACQ Contrast/Bolus Total Dose"},
    {DCM_ACQSYRINGECOUNTS, DCM_IS, "ACQ Syringe Counts"},
    {DCM_ACQCONTRASTFLOWRATE, DCM_DS, "ACQ Contrast Flow Rate (ml/sec)"},
    {DCM_ACQCONTRASTFLOWDURATION, DCM_DS, "ACQ Contrast Flow Duration (sec)"},
    {DCM_ACQCONTRASTBOLUSINGREDIENT, DCM_CS, "ACQ Contrast Bolus Ingredient"},
    {DCM_ACQCONTRASTBOLUSINGREDIENTCONCENTRATION, DCM_DS, "ACQ Contrast Bolus Ingredient Concentration"},
    {DCM_ACQSPATIALRESOLUTION, DCM_DS, "ACQ Spatial Resolution"},
    {DCM_ACQTRIGGERTIME, DCM_DS, "ACQ Trigger Time"},
    {DCM_ACQTRIGGERSRCTYPE, DCM_LO, "ACQ Trigger Source or Type"},
    {DCM_ACQNOMINALINTERVAL, DCM_IS, "ACQ Nominal Interval"},
    {DCM_ACQFRAMETIME, DCM_DS, "ACQ Frame Time"},
    {DCM_ACQFRAMINGTYPE, DCM_LO, "ACQ Framing Type"},
    {DCM_ACQFRAMETIMEVECTOR, DCM_DS, "ACQ Frame Time Vector"},
    {DCM_ACQFRAMEDELAY, DCM_DS, "ACQ Frame Delay"},
    {DCM_ACQIMAGETRIGGERDELAY, DCM_DS, "ACQ Image Trigger Delay"},	/* Sup 30 0.6 */
    {DCM_ACQGROUPTIMEOFFSET, DCM_DS, "ACQ Group Time Offset"},	/* Sup 30 0.6 */
    {DCM_ACQTRIGGERTIMEOFFSET, DCM_DS, "ACQ Trigger Time Offset"},	/* Sup 30 0.6 */
    {DCM_ACQSYNCTRIGGER, DCM_CS, "ACQ Synchronization Trigger"},	/* Sup 30 0.6 */
    {DCM_ACQSYNCFRAMEOFREFERENCE, DCM_UI, "ACQ Synchronization Frame of Reference"},	/* Sup 30 0.6 */
    {DCM_ACQTRIGGERSAMPLEPOSITION, DCM_UL, "ACQ Trigger Sample Position"},	/* Sup 30 0.6 */
    {DCM_ACQRADIOPHARMROUTE, DCM_LO, "ACQ Radiopharmaceutical Route"},
    {DCM_ACQRADIOPHARMVOLUME, DCM_DS, "ACQ Radiopharmaceutical Volume"},
    {DCM_ACQRADIOPHARMSTARTTIME, DCM_TM, "ACQ Radiopharmaceutical Start Time"},
    {DCM_ACQRADIOPHARMSTOPTIME, DCM_TM, "ACQ Radiopharmaceutical Stop Time"},
    {DCM_ACQRADIONUCLIDETOTALDOSE, DCM_DS, "ACQ Radionuclide Total Dose"},
    {DCM_ACQRADIONUCLIDEHALFLIFE, DCM_DS, "ACQ Radionuclide Half Life"},
    {DCM_ACQRADIONUCLIDEPOSITRONFRACTION, DCM_DS, "ACQ Radionuclide Positron Fraction"},
    {DCM_ACQRADIOPHARMACEUTICALSPECIFICACTIVITY, DCM_DS,
    "ACQ Radiopharmaceutical Specific Activity"},
    {DCM_ACQBEATREJECTIONFLAG, DCM_CS, "ACQ Beat Rejection Flag"},
    {DCM_ACQLOWRRVALUE, DCM_IS, "ACQ Low R-R Value"},
    {DCM_ACQHIGHRRVALUE, DCM_IS, "ACQ High R-R Value"},
    {DCM_ACQINTERVALSACQUIRED, DCM_IS, "ACQ Intervals Acquired"},
    {DCM_ACQINTERVALSREJECTED, DCM_IS, "ACQ Intervals Rejected"},
    {DCM_ACQPVCREJECTION, DCM_LO, "ACQ PVC Rejection"},
    {DCM_ACQSKIPBEATS, DCM_IS, "ACQ Skip Beats"},
    {DCM_ACQHEARTRATE, DCM_IS, "ACQ Heart Rate"},
    {DCM_ACQCARDIACNUMBEROFIMAGES, DCM_IS, "ACQ Cardiac Number of Images"},
    {DCM_ACQTRIGGERWINDOW, DCM_IS, "ACQ Trigger Window"},
    {DCM_ACQRECONSTRUCTIONDIAMETER, DCM_DS, "ACQ Reconstruction Diameter"},
    {DCM_ACQDISTANCESRCTODETECTOR, DCM_DS, "ACQ Distance Source-Detector"},
    {DCM_ACQDISTANCESRCTOPATIENT, DCM_DS, "ACQ Distance Source-Patient"},
    {DCM_ACQESTIMATEDRADIOGRAPHICMAGFACTOR, DCM_DS, "ACQ Estimated Radiographic Mag Factor"},
    {DCM_ACQGANTRYTILT, DCM_DS, "ACQ Gantry/Detector Tilt"},
    {DCM_ACQGANTRYSLEW, DCM_DS, "ACQ Gantry/Detector Slew"},
    {DCM_ACQTABLEHEIGHT, DCM_DS, "ACQ Table Height"},
    {DCM_ACQTABLETRAVERSE, DCM_DS, "ACQ Table Traverse"},
    {DCM_ACQTABLEMOTION, DCM_CS, "ACQ Table Motion (STATIC, DYNAMIC)"},
    {DCM_ACQTABLEVERTICALINCREMENT, DCM_DS, "ACQ Table Vertical Increment (mm)"},
    {DCM_ACQTABLELATERALINCREMENT, DCM_DS, "ACQ Table Lateral Increment (mm)"},
    {DCM_ACQTABLELONGITUDINALINCREMENT, DCM_DS, "ACQ Table Longitudinal Increment (mm)"},
    {DCM_ACQTABLEANGLE, DCM_DS, "ACQ Table Angle (relative to horizontal: deg)"},
    {DCM_ACQROTATIONDIRECTION, DCM_CS, "ACQ Rotation Direction"},
    {DCM_ACQANGULARPOSITION, DCM_DS, "ACQ Angular Position"},
    {DCM_ACQRADIALPOSITION, DCM_DS, "ACQ Radial Position"},
    {DCM_ACQSCANARC, DCM_DS, "ACQ Scan Arc"},
    {DCM_ACQANGULARSTEP, DCM_DS, "ACQ Angular Step"},
    {DCM_ACQCENTERROTATIONOFFSET, DCM_DS, "ACQ Center of Rotation Offset"},
    {DCM_ACQROTATIONOFFSET, DCM_DS, "ACQ Rotation Offset (RET)"},
    {DCM_ACQFIELDOFVIEWSHAPE, DCM_CS, "ACQ Field of View Shape"},
    {DCM_ACQFIELDOFVIEWDIMENSION, DCM_IS, "ACQ Field of View Dimension(s)"},
    {DCM_ACQEXPOSURETIME, DCM_IS, "ACQ Exposure Time"},
    {DCM_ACQXRAYTUBECURRENT, DCM_IS, "ACQ X-ray Tube Current"},
    {DCM_ACQEXPOSURE, DCM_IS, "ACQ Exposure"},
    {DCM_ACQAVERAGEPULSEWIDTH, DCM_DS, "ACQ Average width of X-Ray pulse (ms)"},
    {DCM_ACQRADIATIONSETTING, DCM_CS, "ACQ General level of X-Ray dose exposure"},
    {DCM_ACQRADIATIONMODE, DCM_CS, "ACQ X-Ray radiation mode (CONTINUOUS, PULSED)"},
    {DCM_ACQIMAGEAREADOSEPRODUCT, DCM_DS, "ACQ X-Ray dose to which patient was exposed"},
    {DCM_ACQFILTERTYPE, DCM_SH, "ACQ Filter Type, extremity"},
    {DCM_ACQTYPEOFFILTERS, DCM_LO, "ACQ Type of filter(s) inserted into X-Ray beam"},
    {DCM_ACQINTENSIFIERSIZE, DCM_DS, "ACQ Intensifier Size (mm)"},
    {DCM_ACQIMAGERPIXELSPACING, DCM_DS, "ACQ Image Pixel Spacing"},
    {DCM_ACQGRID, DCM_CS, "ACQ Grid (IN, NONE)"},
    {DCM_ACQGENERATORPOWER, DCM_IS, "ACQ Generator Power"},
    {DCM_ACQCOLLIMATORGRIDNAME, DCM_SH, "ACQ Collimator/Grid Name"},
    {DCM_ACQCOLLIMATORTYPE, DCM_CS, "ACQ Collimator Type"},
    {DCM_ACQFOCALDISTANCE, DCM_IS, "ACQ Focal Distance"},
    {DCM_ACQXFOCUSCENTER, DCM_DS, "ACQ X Focus Center"},
    {DCM_ACQYFOCUSCENTER, DCM_DS, "ACQ Y Focus Center"},
    {DCM_ACQFOCALSPOT, DCM_DS, "ACQ Focal Spot"},
    {DCM_ACQDATELASTCALIBRATION, DCM_DA, "ACQ Date of Last Calibration"},
    {DCM_ACQTIMELASTCALIBRATION, DCM_TM, "ACQ Time of Last Calibration"},
    {DCM_ACQCONVOLUTIONKERNEL, DCM_SH, "ACQ Convolution Kernel"},
    {DCM_ACQUPPERLOWERPIXELVALUES, DCM_RET, "ACQ Upper/Lower Pixel Values (RET)"},
    {DCM_ACQACTUALFRAMEDURATION, DCM_IS, "ACQ Actual Frame Duration"},
    {DCM_ACQCOUNTRATE, DCM_IS, "ACQ Count Rate"},
    {DCM_ACQPREFPLAYBACKSEQUENCING, DCM_US, "ACQ Preferred Playback Sequencing"},
    {DCM_ACQRECEIVINGCOIL, DCM_SH, "ACQ Receiving Coil"},
    {DCM_ACQTRANSMITTINGCOIL, DCM_SH, "ACQ Transmitting Coil"},
    {DCM_ACQPLATETYPE, DCM_SH, "ACQ Plate Type"},
    {DCM_ACQPHOSPHORTYPE, DCM_LO, "ACQ Phosphor Type"},
#if STANDARD_VERSION < VERSION_APR1995
    {DCM_ACQSCANVELOCITY, DCM_IS, "ACQ Scan Velocity"},
#else
    {DCM_ACQSCANVELOCITY, DCM_DS, "ACQ Scan Velocity"},
#endif
    {DCM_ACQWHOLEBODYTECHNIQUE, DCM_CS, "ACQ Whole Body Technique"},
    {DCM_ACQSCANLENGTH, DCM_IS, "ACQ Scan Length"},
    {DCM_ACQACQUISITIONMATRIX, DCM_US, "ACQ Acquisition Matrix"},
    {DCM_ACQPHASEENCODINGDIRECTION, DCM_CS, "ACQ Phase Encoding Direction"},
    {DCM_ACQFLIPANGLE, DCM_DS, "ACQ Flip Angle"},
    {DCM_ACQVARIABLEFLIPANGLE, DCM_CS, "ACQ Variable Flip Angle"},
    {DCM_ACQSAR, DCM_DS, "ACQ SAR"},
    {DCM_ACQDBDT, DCM_DS, "ACQ DB/DT"},
    {DCM_ACQDEVICEPROCESSINGDESCR, DCM_LO, "ACQ Acquisition Device Processing Description"},
    {DCM_ACQDEVICEPROCESSINGCODE, DCM_LO, "ACQ Acquisition Device Processing Code"},
    {DCM_ACQCASSETTEORIENTATION, DCM_CS, "ACQ Cassette Orientation"},
    {DCM_ACQCASSETTESIZE, DCM_CS, "ACQ Cassette Size"},
    {DCM_ACQEXPOSURESONPLATE, DCM_US, "ACQ Exposures on Plate"},
    {DCM_ACQRELATIVEXRAYEXPOSURE, DCM_IS, "ACQ Relative X-ray Exposure"},
    {DCM_ACQCOLUMNANGULATION, DCM_CS, "ACQ Column Angulation"},
    {DCM_ACQTOMOLAYERHEIGHT, DCM_DS, "ACQ Tomo Layer Height (mm)"},
    {DCM_ACQTOMOANGLE, DCM_DS, "ACQ Tomo Angle"},
    {DCM_ACQTOMOTIME, DCM_DS, "ACQ Tomo Time"},
    {0x00181490, DCM_CS, "ACQ Tomo Type"},			/* 2002.04.26 */
    {0x00181491, DCM_CS, "ACQ Tomo Class"},			/* 2002.04.26 */
    {0x00181495, DCM_IS, "ACQ Number of Tomosynthesis Source Images"}, /* 2002.04.26 */
    {DCM_ACQPOSITIONERMOTION, DCM_CS, "ACQ Positioner Motion"},
    {0x00181508, DCM_CS, "ACQ Positioner Type"},		/* 2002.04.26 */
    {DCM_ACQPOSITIONERPRIMARYANGLE, DCM_DS, "ACQ Positioner Primary Angle"},
    {DCM_ACQPOSITIONERSECONDARYANGLE, DCM_DS, "ACQ Positioner Secondary Angle"},
    {DCM_ACQPOSITIONERPRIMARYANGLEINCR, DCM_DS, "ACQ Positioner Primary Angle Increment"},
    {DCM_ACQPOSITIONERSECONDARYANGLEINCR, DCM_DS, "ACQ Positioner Secondary Angle Increment"},
    {DCM_ACQDETECTORPRIMARYANGLE, DCM_DS, "ACQ Detector Primary Angle"},
    {DCM_ACQDETECTORSECONDARYANGLE, DCM_DS, "ACQ Detector Secondary Angle"},
    {DCM_ACQSHUTTERSHAPE, DCM_CS, "ACQ Shutter Shape"},
    {DCM_ACQSHUTTERLEFTVERTICALEDGE, DCM_IS, "ACQ Shutter Left Vertical Edge"},
    {DCM_ACQSHUTTERRIGHTVERTICALEDGE, DCM_IS, "ACQ Shutter Right Vertical Edge"},
    {DCM_ACQSHUTTERUPPERHORIZONTALEDGE, DCM_IS, "ACQ Shutter Upper Horizontal Edge"},
    {DCM_ACQSHUTTERLOWERHORIZONTALEDGE, DCM_IS, "ACQ Shutter Lower Horizontal Edge"},
    {DCM_ACQCENTEROFCIRCULARSHUTTER, DCM_IS, "ACQ Center of Circular Shutter"},
    {DCM_ACQRADIUSOFCIRCULARSHUTTER, DCM_IS, "ACQ Radius of Circular Shutter"},
    {DCM_ACQVERTICESOFPOLYGONALSHUTTER, DCM_IS, "ACQ Vertices of the Polygonal Shutter"},
    {DCM_ACQCOLLIMATORSHAPE, DCM_CS, "ACQ Collimator Shape"},
    {DCM_ACQCOLLIMATORLEFTVERTICALEDGE, DCM_IS, "ACQ Collimator Left Vertical Edge"},
    {DCM_ACQCOLLIMATORRIGHTVERTICALEDGE, DCM_IS, "ACQ Collimator Right Vertical Edge"},
    {DCM_ACQCOLLIMATORUPPERHORIZONTALEDGE, DCM_IS, "ACQ Collimator Upper Horizontal Edge"},
    {DCM_ACQCOLLIMATORLOWERHORIZONTALEDGE, DCM_IS, "ACQ Collimator Lower Horizontal Edge"},
    {DCM_ACQCENTEROFCIRCULARCOLLIMATOR, DCM_IS, "ACQ Center of Circular Collimator"},
    {DCM_ACQRADIUSOFCIRCULARCOLLIMATOR, DCM_IS, "ACQ Radius of Circular Collimator"},
    {DCM_ACQVERTICESOFPOLYGONALCOLLIMATOR, DCM_IS, "ACQ Vertices of the Polygonal Collimator"},
    {DCM_ACQACQUISITIONTIMESYNCHRONIZED, DCM_CS,
    "ACQ Acquisition Time Synchronized"},	/* Sup 30 0.7 */
    {DCM_ACQTIMESOURCE, DCM_SH, "ACQ Time Source"},	/* Sup 30 0.7 */
    {DCM_ACQTIMEDISTRIBUTIONPROTOCOL, DCM_CS,
    "ACQ Time Distribution Protocol"},	/* Sup 30 0.7 */
    {DCM_ACQCOMMENTS, DCM_RET, "ACQ Comments"},
    {DCM_ACQOUTPUTPOWER, DCM_SH, "ACQ Output Power"},
    {DCM_ACQTRANSDUCERDATA, DCM_LO, "ACQ Transducer Data"},
    {DCM_ACQFOCUSDEPTH, DCM_DS, "ACQ Focus Depth"},
#if STANDARD_VERSION < VERSION_APR1995
    {DCM_ACQPREPROCESSINGFUNCTION, DCM_LO, "ACQ Preprocessing Function"},
#else
    {DCM_ACQPROCESSINGFUNCTION, DCM_LO, "ACQ Processing Function"},
#endif
    {DCM_ACQPOSTPROCESSINGFUNCTION, DCM_LO, "ACQ Postprocessing Function"},
    {DCM_ACQMECHANICALINDEX, DCM_DS, "ACQ Mechanical Index"},
    {DCM_ACQTHERMALINDEX, DCM_DS, "ACQ Thermal Index"},
    {DCM_ACQCRANIALTHERMALINDEX, DCM_DS, "ACQ Cranial Thermal Index"},
    {DCM_ACQSOFTTISSUETHERMALINDEX, DCM_DS, "ACQ Soft Tissue Thermal Index"},
    {DCM_ACQSOFTTISSUEFOCUSTHERMALINDEX, DCM_DS,
    "ACQ Soft Tissue-focus Thermal Index"},
    {DCM_ACQSOFTTISSUESURFACETHERMALINDEX, DCM_CS,
    "ACQ Soft Tissue-surface Thermal Index"},
    {DCM_ACQDEPTHOFSCANFIELD, DCM_IS, "ACQ Depth of Scan Field"},
    {DCM_ACQPATIENTPOSITION, DCM_CS, "ACQ Patient Position"},
    {DCM_ACQVIEWPOSITION, DCM_CS, "ACQ View Position"},
    {DCM_ACQIMAGETRANSFORMATIONMATRIX, DCM_DS,
    "ACQ Image Transformation Matrix"},
    {DCM_ACQIMAGETRANSLATIONVECTOR, DCM_DS,
    "ACQ Image Translation Vector"},
    {DCM_ACQSENSITIVITY, DCM_DS, "ACQ Sensitivity"},
    {DCM_ACQUSREGIONSEQUENCE, DCM_SQ, "ACQ Ultrasound Region Sequence"},
    {DCM_ACQREGIONSPATIALFORMAT, DCM_US, "ACQ Region Spatial Format"},
    {DCM_ACQREGIONDATATYPE, DCM_US, "ACQ Region Data Type"},
    {DCM_ACQREGIONFLAGS, DCM_UL, "ACQ Region Flags"},
    {DCM_ACQREGIONLOCATIONMINX0, DCM_UL, "ACQ Region Location Min X(0)"},
    {DCM_ACQREGIONLOCATIONMINY0, DCM_UL, "ACQ Region Location Min Y(0)"},
    {DCM_ACQREGIONLOCATIONMAXX1, DCM_UL, "ACQ Region Location Max X(1)"},
    {DCM_ACQREGIONLOCATIONMAXY1, DCM_UL, "ACQ Region Location Max Y(1)"},
    {DCM_ACQREFERENCEPIXELX, DCM_SL, "ACQ Reference Pixel X"},
    {DCM_ACQREFERENCEPIXELY, DCM_SL, "ACQ Reference Pixel Y"},
    {DCM_ACQPHYSICALUNITSXDIRECTION, DCM_US, "ACQ Physical Units X Direction"},
    {DCM_ACQPHYSICALUNITSYDIRECTION, DCM_US, "ACQ Physical Units Y Direction"},
    {DCM_ACQREFPIXELPHYSICALVALUEX, DCM_FD, "ACQ Reference Pixel Physical Value X"},
    {DCM_ACQREFPIXELPHYSICALVALUEY, DCM_FD, "ACQ Reference Pixel Physical Value Y"},
    {DCM_ACQPHYSICALDELTAX, DCM_FD, "ACQ Physical Delta X"},
    {DCM_ACQPHYSICALDELTAY, DCM_FD, "ACQ Physical Delta Y"},
    {DCM_ACQTRANSDUCERFREQUENCY, DCM_UL, "ACQ Transducer Frequency"},
    {DCM_ACQTRANSDUCERTYPE, DCM_CS, "ACQ Transducer Type"},
    {DCM_ACQPULSEREPETITIONFREQ, DCM_UL, "ACQ Pulse Repetition Frequency"},
    {DCM_ACQDOPPLERCORRECTIONANGLE, DCM_FD, "ACQ Doppler Correction Angle"},
    {DCM_ACQSTERRINGANGLE, DCM_FD, "ACQ Sterring Angle"},
    {DCM_ACQDOPPLERSAMPLEVOLXPOS, DCM_UL, "ACQ Doppler Sample Volume X Position"},
    {DCM_ACQDOPPLERSAMPLEVOLYPOS, DCM_UL, "ACQ Doppler Sample Volume Y Position"},
    {DCM_ACQTMLINEPOSITIONX0, DCM_UL, "ACQ TM-Line Position X(0)"},
    {DCM_ACQTMLINEPOSITIONY0, DCM_UL, "ACQ TM-Line Position Y(0)"},
    {DCM_ACQTMLINEPOSITIONX1, DCM_UL, "ACQ TM-Line Position X(1)"},
    {DCM_ACQTMLINEPOSITIONY1, DCM_UL, "ACQ TM-Line Position Y(1)"},
    {DCM_ACQPIXELCOMPORGANIZATION, DCM_US, "ACQ Pixel Component Organization"},
    {DCM_ACQPIXELCOMPMASK, DCM_UL, "ACQ Pixel Component Mask"},
    {DCM_ACQPIXELCOMPRANGESTART, DCM_UL, "ACQ Pixel Component Range Start"},
    {DCM_ACQPIXELCOMPRANGESTOP, DCM_UL, "ACQ Pixel Component Range Stop"},
    {DCM_ACQPIXELCOMPPHYSUNITS, DCM_US, "ACQ Pixel Component Physical Units"},
    {DCM_ACQPIXELCOMPDATATYPE, DCM_US, "ACQ Pixel Component Data Type"},
    {DCM_ACQNUMBERTABLEBREAKPOINTS, DCM_UL, "ACQ Number of Table Break Points"},
    {DCM_ACQTABLEXBREAKPOINTS, DCM_UL, "ACQ Table of X Break Points"},
    {DCM_ACQTABLEYBREAKPOINTS, DCM_FD, "ACQ Table of Y Break Points"},
    {DCM_ACQNUMBEROFTABLEENTRIES, DCM_UL, "ACQ Number of Table Entries"},
    {DCM_ACQTABLEOFPIXELVALUES, DCM_UL, "ACQ Table of Pixel Values"},
    {DCM_ACQTABLEOFPARAMETERVALUES, DCM_FL, "ACQ Table of Parameter Values"},

    {0x00187000, DCM_CS, "ACQ Detector Conditions Nominal Flag"}, /* 2002.04.26 */
    {0x00187001, DCM_DS, "ACQ Detector Temperature"},		/* 2002.04.26 */
    {0x00187004, DCM_CS, "ACQ Detector Type"},			/* 2002.04.26 */
    {0x00187005, DCM_CS, "ACQ Detector Configuration"},		/* 2002.04.26 */
    {0x00187006, DCM_LT, "ACQ Detector Description"},		/* 2002.04.26 */
    {0x00187008, DCM_LT, "ACQ Detector Mode"},			/* 2002.04.26 */
    {0x0018700A, DCM_SH, "ACQ Detector ID"},			/* 2002.04.26 */

    {0x00187028, DCM_DS, "ACQ Detector Active Origin"}		/* 2002.04.26 */
};

/* Define the entries for the RELATIONSHIP group (0020)
*/
static DCMDICT REL_dictionary[] = {
    {DCM_RELGROUPLENGTH, DCM_UL, "REL Group Length"},
    {DCM_RELSTUDYINSTANCEUID, DCM_UI, "REL Study Instance UID"},
    {DCM_RELSERIESINSTANCEUID, DCM_UI, "REL Series Instance UID"},
    {DCM_RELSTUDYID, DCM_SH, "REL Study ID"},
    {DCM_RELSERIESNUMBER, DCM_IS, "REL Series Number"},
    {DCM_RELACQUISITIONNUMBER, DCM_IS, "REL Acquisition Number"},
    {DCM_RELIMAGENUMBER, DCM_IS, "REL Instance Number"},

    {DCM_RELISOTOPENUMBER, DCM_IS, "REL Isotope Number (RET)"},
    {DCM_RELPHASENUMBER, DCM_IS, "REL Phase Number (RET)"},
    {DCM_RELINTERVALNUMBER, DCM_IS, "REL Interval Number (RET)"},
    {DCM_RELTIMESLOTNUMBER, DCM_IS, "REL Time Slot Number (RET)"},
    {DCM_RELANGLENUMBER, DCM_IS, "REL Angle Number (RET)"},

    {DCM_RELPATIENTORIENTATION, DCM_CS, "REL Patient Orientation"},
    {DCM_RELOVERLAYNUMBER, DCM_IS, "REL Overlay Number"},
    {DCM_RELCURVENUMBER, DCM_IS, "REL Curve Number"},
    {DCM_RELLOOKUPTABLENUMBER, DCM_IS, "REL Looup Table Number"},
    {DCM_RELIMAGEPOSITION, DCM_RET, "REL Image Position (RET)"},
    {DCM_RELIMAGEPOSITIONPATIENT, DCM_DS, "REL Image Position Patient"},
    {DCM_RELIMAGEORIENTATION, DCM_RET, "REL Image Orientation"},
    {DCM_RELIMAGEORIENTATIONPATIENT, DCM_DS, "REL Image Orientation (Patient)"},
    {DCM_RELLOCATION, DCM_RET, "REL Location (RET)"},
    {DCM_RELFRAMEOFREFERENCEUID, DCM_UI, "REL Frame of Reference UID"},
    {DCM_RELLATERALITY, DCM_CS, "REL Laterality"},
    { DCM_MAKETAG(0x0020, 0x0062), DCM_CS, "REL Image Laterality"},
    {DCM_RELIMAGEGEOMETRYTYPE, DCM_RET, "REL Image Geometry Type (RET)"},
    {DCM_RELMASKINGIMAGE, DCM_RET, "REL Masking Image (RET)"},
    {DCM_RELTEMPORALPOSITIONID, DCM_IS, "REL Temporal Position Identifier"},
    {DCM_RELNUMBERTEMPORALPOSITIONS, DCM_IS, "REL Number of Temporal Positions"},
    {DCM_RELTEMPORALRESOLUTION, DCM_DS, "REL Temporal Resolution"},
    {DCM_RELSERIESINSTUDY, DCM_IS, "REL Series in Study"},
    {DCM_RELACQUISITIONSINSERIES, DCM_RET, "REL Acquisitions in Series"},
    {DCM_RELIMAGESINACQUISITION, DCM_IS, "REL Images in Acquisition"},
    {DCM_RELACQUISITIONSINSTUDY, DCM_IS, "REL Acquisitions in Study"},
    {DCM_RELREFERENCE, DCM_RET, "REL Reference (RET)"},
    {DCM_RELPOSITIONREFINDICATOR, DCM_LO, "REL Position Reference Indicator"},
    {DCM_RELSLICELOCATION, DCM_DS, "REL Slice Location"},
    {DCM_RELOTHERSTUDYNUMBERS, DCM_IS, "REL Other Study Numbers"},
    {DCM_RELNUMBERPATRELATEDSTUDIES, DCM_IS,
    "REL Number of Patient Related Studies"},
    {DCM_RELNUMBERPATRELATEDSERIES, DCM_IS, "REL Number of Patient Related Series"},
    {DCM_RELNUMBERPATRELATEDIMAGES, DCM_IS, "REL Number of Patient Related Instances"},
    {DCM_RELNUMBERSTUDYRELATEDSERIES, DCM_IS, "REL Number of Study Related Series"},
    {DCM_RELNUMBERSTUDYRELATEDIMAGES, DCM_IS, "REL Number of Study Related Instances"},
    {DCM_RELNUMBERSERIESRELATEDINST, DCM_IS, "REL Number of Series Related Instances"},
    {DCM_RELSOURCEIMAGEID, DCM_RET, "REL Source Image IDs (RET)"},
    {DCM_RELMODIFYINGDEVICEID, DCM_RET, "REL Modifying Device ID (RET)"},
    {DCM_RELMODIFIEDIMAGEID, DCM_RET, "REL Modified Image ID (RET)"},
    {DCM_RELMODIFIEDIMAGEDATE, DCM_RET, "REL Modified Image Date (RET)"},
    {DCM_RELMODIFYINGDEVICEMFR, DCM_RET, "REL Modifying Device Mfr (RET)"},
    {DCM_RELMODIFIEDIMAGETIME, DCM_RET, "REL Modified Image Time"},
    {DCM_RELMODIFIEDIMAGEDESCRIPTION, DCM_RET,
    "REL Modified Image Description (RET)"},
    {DCM_RELIMAGECOMMENTS, DCM_LT, "REL Image Comments"},
    {DCM_RELORIGINALIMAGEID, DCM_RET, "REL Original Image ID (RET)"},
    {DCM_RELORIGINALIMAGEIDNOMENCLATURE, DCM_RET,
    "REL Orig Image ID Nomenclature (RET)"}
};

/* Define the entries for the IMAGE group (0028)
*/
static DCMDICT IMG_dictionary[] = {
    {DCM_IMGGROUPLENGTH, DCM_UL, "IMG Group Length"},
    {DCM_IMGSAMPLESPERPIXEL, DCM_US, "IMG Samples Per Pixel"},
    {DCM_IMGPHOTOMETRICINTERP, DCM_CS, "IMG Photometric Interpretation"},
    {DCM_IMGIMAGEDIMENSIONS, DCM_RET, "IMG Image Dimensions (RET)"},
    {DCM_IMGPLANARCONFIGURATION, DCM_US, "IMG Planar Configuration"},
    {DCM_IMGNUMBEROFFRAMES, DCM_IS, "IMG Number of Frames"},
    {DCM_IMGFRAMEINCREMENTPOINTER, DCM_AT, "IMG Frame Increment Pointer"},
    {DCM_IMGROWS, DCM_US, "IMG Rows"},
    {DCM_IMGCOLUMNS, DCM_US, "IMG Columns"},
    {DCM_IMGPLANES, DCM_US, "IMG Planes"},
    {DCM_IMGUSOUNDCOLORDATAPRESENT, DCM_US, "IMG Ultrasound Color Data Present"},
    {DCM_IMGPIXELSPACING, DCM_DS, "IMG Pixel Spacing"},
    {DCM_IMGZOOMFACTOR, DCM_DS, "IMG Zoom Factor"},
    {DCM_IMGZOOMCENTER, DCM_DS, "IMG Zoom Center"},
    {DCM_IMGPIXELASPECTRATIO, DCM_IS, "IMG Pixel Aspect Ratio"},
    {DCM_IMGIMAGEFORMAT, DCM_RET, "IMG Image Format (RET)"},
    {DCM_IMGMANIPULATEDIMAGE, DCM_RET, "IMG Manipulated Image (RET)"},
    {DCM_IMGCORRECTEDIMAGE, DCM_CS, "IMG Corrected Image"},
    {DCM_IMGCOMPRESSIONCODE, DCM_RET, "IMG Compression Code"},
    {DCM_IMGBITSALLOCATED, DCM_US, "IMG Bits Allocated"},
    {DCM_IMGBITSSTORED, DCM_US, "IMG Bits Stored"},
    {DCM_IMGHIGHBIT, DCM_US, "IMG High Bit"},
    {DCM_IMGPIXELREPRESENTATION, DCM_US, "IMG Pixel Representation"},
    {DCM_IMGSMALLESTPIXELVALUE, DCM_RET, "IMG Smallest Pixel Value (RET)"},
    {DCM_IMGLARGESTPIXELVALUE, DCM_RET, "IMG Largest Pixel Vaue (RET)"},
    {DCM_IMGSMALLESTIMAGEPIXELVALUE, DCM_CTX, "IMG Smallest Image Pixel Value"},
    {DCM_IMGLARGESTIMAGEPIXELVALUE, DCM_CTX, "IMG Largest Image Pixel Value"},
    {DCM_IMGSMALLESTPIXELVALUESERIES, DCM_CTX, "IMG Smallest Pixel Value in Series"},
    {DCM_IMGLARGESTPIXELVALUESERIES, DCM_CTX, "IMG Largest Pixel Value in Series"},
    {DCM_IMGSMALLESTIMAGEPIXELVALUEPLANE, DCM_CTX, "IMG Smallest Pixel Value in Plane"},
    {DCM_IMGLARGESTIMAGEPIXELVALUEPLANE, DCM_CTX, "IMG Largest Pixel Value in Plane"},
    {DCM_IMGPIXELPADDINGVALUE, DCM_CTX, "IMG Pixel Padding Value"},
    {DCM_IMGWAVEFORMPADDINGVALUE, DCM_CTX, "IMG Waveform Padding Value"},	/* Sup 30 0.6 */
    {DCM_IMGIMAGELOCATION, DCM_RET, "IMG Image Location"},
    {DCM_MAKETAG(0x0028, 0x0300), DCM_CS, "IMG Quality Control Image"},
    {DCM_MAKETAG(0x0028, 0x0301), DCM_CS, "IMG Burned In Annotation"},
    {DCM_IMGPIXELINTENSITYRELATIONSHIP, DCM_CS, "IMG Pixel Intensity Relationship"},
    {DCM_MAKETAG(0x0028, 0x1041), DCM_SS, "IMG Pixel Intensity Relationship Sign"},
    {DCM_IMGWINDOWCENTER, DCM_DS, "IMG Window Center"},
    {DCM_IMGWINDOWWIDTH, DCM_DS, "IMG Window Width"},
    {DCM_IMGRESCALEINTERCEPT, DCM_DS, "IMG Rescale Intercept"},
    {DCM_IMGRESCALESLOPE, DCM_DS, "IMG Rescale Slope"},
    {DCM_IMGRESCALETYPE, DCM_LO, "IMG Rescale Type"},
    {DCM_IMGWINDOWCWEXPLANATION, DCM_LO, "IMG Window Center & Width Explanation"},
    {DCM_IMGGRAYSCALE, DCM_RET, "IMG Gray Scale (RET)"},
    {DCM_IMGRECOMMENDEDVIEWINGMODE, DCM_CS, "IMG Recommended Viewing Mode"},
    {DCM_IMGLUTDESCRIPTGRAY, DCM_RET, "IMG Lookup Table Desc-Gray (RET)"},
    {DCM_IMGLUTDESCRIPTRED, DCM_US, "IMG Lookup Table Desc-Red"},
    {DCM_IMGLUTDESCRIPTGREEN, DCM_US, "IMG Lookup Table Desc-Green"},
    {DCM_IMGLUTDESCRIPTBLUE, DCM_US, "IMG Lookup Table Desc-Blue"},
    {DCM_IMGPALETTECOLORLUTUID, DCM_UI, "IMG Palette Color Lookup Table UID"},
    {DCM_IMGLOOKUPDATAGRAY, DCM_RET, "IMG Lookup Data-Gray"},

#if 0
    /* As originally defined in 1993 */
    {DCM_IMGLOOKUPDATARED, DCM_US, "IMG Lookup Data-Red"},
    {DCM_IMGLOOKUPDATAGREEN, DCM_US, "IMG Lookup Data-Green"},
    {DCM_IMGLOOKUPDATABLUE, DCM_US, "IMG Lookup Data-Blue"},
#endif

    {DCM_IMGLOOKUPDATARED, DCM_CTX, "IMG Lookup Data-Red"},
    {DCM_IMGLOOKUPDATAGREEN, DCM_CTX, "IMG Lookup Data-Green"},
    {DCM_IMGLOOKUPDATABLUE, DCM_CTX, "IMG Lookup Data-Blue"},

    {DCM_IMGSEGMENTEDREDLUTDATA, DCM_OW, "IMG Segmented Red Palette Color LUT Data"},
    {DCM_IMGSEGMENTEDGREENLUTDATA, DCM_OW, "IMG Segmented Green Palette Color LUT Data"},
    {DCM_IMGSEGMENTEDBLUELUTDATA, DCM_OW, "IMG Segmented Blue Palette Color LUT Data"},

    {DCM_IMGLOSSYIMAGECOMPRESSION, DCM_CS, "IMG Lossy Image Compression"},
    {DCM_IMGMODALITYLUTSEQUENCE, DCM_SQ, "IMG Modality LUT Sequence"},
    {DCM_IMGLUTDESCRIPTOR, DCM_CTX, "IMG LUT Descriptor"},
    {DCM_IMGLUTEXPLANATION, DCM_LO, "IMG LUT Explanation"},
    {DCM_IMGMODALITYLUTTYPE, DCM_LO, "IMG Modality LUT Type"},
    {DCM_IMGLUTDATA, DCM_CTX, "IMG LUT Data"},
    {DCM_IMGVOILUTSEQUENCE, DCM_SQ, "IMG VOI LUT Sequence"},
    {DCM_IMGCOMMENTS, DCM_RET, "IMG Comments (RET)"},
    {DCM_IMGBIPLANEACQSEQUENCE, DCM_SQ, "IMG Bi-Plane Acquisition Sequence"},
    {DCM_IMGREPRESENTATIVEFRAMENUMBER, DCM_US, "IMG Representative Frame Number"},
    {DCM_IMGFRAMENUMBERSOFINTEREST, DCM_US, "IMG Frame Numbers of Interest"},
    {DCM_IMGFRAMEOFINTERESTDESCRIPTION, DCM_LO, "IMG Frame of Interest Description"},
    {DCM_IMGMASKPOINTER, DCM_US, "IMG Mask Pointer(s)"},
    {DCM_IMGRWAVEPOINTER, DCM_US, "IMG R Wave Pointer"},
    {DCM_IMGMASKSUBTRACTIONSEQ, DCM_SQ, "IMG Mask Subtraction Sequence"},
    {DCM_IMGMASKOPERATION, DCM_CS, "IMG Mask Operation"},
    {DCM_IMGAPPLICABLEFRAMERANGE, DCM_US, "IMG Applicable Frame Range"},
    {DCM_IMGMASKFRAMENUMBERS, DCM_US, "IMG Mask Frame Numbers"},
    {DCM_IMGCONTRASTFRAMEAVERAGING, DCM_US, "IMG Contrast Frame Averaging"},
    {DCM_IMGMASKSUBPIXELSHIFT, DCM_FL, "IMG Mask Sub-pixel shift"},
    {DCM_IMGTIDOFFSET, DCM_SS, "IMG TID Offset"},
    {DCM_MASKOPERATIONEXPLANATION, DCM_ST, "IMG Mask Operation Explanation"}
};

/* Define the entries for the STUDY group (0032)
*/
static DCMDICT SDY_dictionary[] = {
    {DCM_SDYGROUPLENGTH, DCM_UL, "SDY Study Group length"},
    {DCM_SDYSTATUSID, DCM_CS, "SDY Study Status ID"},
    {DCM_SDYPRIORITYID, DCM_CS, "SDY Study Priority ID"},
    {DCM_SDYIDISSUER, DCM_LO, "SDY Study ID Issuer"},
    {DCM_SDYVERIFIEDDATE, DCM_DA, "SDY Study Verified Date"},
    {DCM_SDYVERIFIEDTIME, DCM_TM, "SDY Study Verified Time"},
    {DCM_SDYREADDATE, DCM_DA, "SDY Study Read Date"},
    {DCM_SDYREADTIME, DCM_TM, "SDY Study Read Time"},
    {DCM_SDYSCHEDULEDSTARTDATE, DCM_DA, "SDY Scheduled Study Start Date"},
    {DCM_SDYSCHEDULEDSTARTTIME, DCM_TM, "SDY Scheduled Study Start Time"},
    {DCM_SDYSCHEDULEDSTOPDATE, DCM_DA, "SDY Scheduled Study Stop Date"},
    {DCM_SDYSCHEDULEDSTOPTIME, DCM_TM, "SDY Scheduled Study Stop Time"},
    {DCM_SDYSCHEDULEDLOCATION, DCM_LO, "SDY Scheduled Study Location"},
    {DCM_SDYSCHEDULEDLOCATIONAETITLE, DCM_AE,
    "SDY Scheduled Study Location AE Title(s)"},
    {DCM_SDYREASON, DCM_LO, "SDY Study Reason"},
    {DCM_SDYREQUESTINGPHYSICIAN, DCM_PN, "SDY Requesting Physician "},
    {DCM_SDYREQUESTINGSERVICE, DCM_LO, "SDY Requesting Service"},
    {DCM_SDYARRIVALDATE, DCM_DA, "SDY Study Arrival Date"},
    {DCM_SDYARRIVALTIME, DCM_TM, "SDY Study Arrival Time"},
    {DCM_SDYCOMPLETIONDATE, DCM_DA, "SDY Study Completion Date"},
    {DCM_SDYCOMPLETIONTIME, DCM_TM, "SDY Study Completion Time"},
    {DCM_SDYSTUDYCOMPONENTSTATUSID, DCM_CS, "SDY Study Component Status ID"},
    {DCM_SDYREQUESTEDPRODESCRIPTION, DCM_LO, "SDY Requested Procedure Description"},
    {DCM_SDYREQUESTEDPROCODESEQ, DCM_SQ, "SDY Requested Procedure Code Seq"},
    {DCM_SDYREQUESTEDCONTRASTAGENT, DCM_LO, "SDY Requested Contrast Agent"},
    {DCM_SDYCOMMENTS, DCM_LT, "SDY Comments"}
};

/* Define the entries for the VISIT group, 0038
*/
static DCMDICT VIS_dictionary[] = {
    {DCM_VISGROUPLENGTH, DCM_UL, "VIS Group Length"},
    {DCM_VISREFERENCEDPATALIASSEQ, DCM_SQ, "VIS Referenced Patient Alias Sequence"},
    {DCM_VISSTATUSID, DCM_CS, "VIS Visit Status ID"},
    {DCM_VISADMISSIONID, DCM_LO, "VIS Admission ID"},
    {DCM_VISISSUEROFADMISSIONID, DCM_LO, "VIS Issuer of Admission ID"},
    {DCM_VISROUTEOFADMISSION, DCM_LO, "VIS Route of Admission"},
    {DCM_VISSCHEDULEDADMISSIONDATE, DCM_DA, "VIS Scheduled Admission Date"},
    {DCM_VISSCHEDULEDADMISSIONTIME, DCM_TM, "VIS Scheduled Admission Time"},
    {DCM_VISSCHEDULEDDISCHARGEDATE, DCM_DA, "VIS Scheduled Discharge Date"},
    {DCM_VISSCHEDULEDDISCHARGETIME, DCM_TM, "VIS Scheduled Discharge Time"},
    {DCM_VISSCHEDULEDPATINSTRESIDENCE, DCM_LO, "VIS Scheduled Patient Institution Residence"},
    {DCM_VISADMITTINGDATE, DCM_DA, "VIS Admitting Date"},
    {DCM_VISADMITTINGTIME, DCM_TM, "VIS Admitting Time"},
    {DCM_VISDISCHARGEDATE, DCM_DA, "VIS Discharge Date"},
    {DCM_VISDISCHARGETIME, DCM_TM, "VIS Discharge Time"},
    {DCM_VISDISCHARGEDIAGDESCRIPTION, DCM_LO, "VIS Discharge Diagnosis Description"},
    {DCM_VISDISCHARGEDIAGNOSISCODESEQ, DCM_SQ, "VIS Discharge Diagnosis Code Sequence"},
    {DCM_VISSPECIALNEEDS, DCM_LO, "VIS Special Needs"},
    {DCM_VISCURRENTPATIENTLOCATION, DCM_LO, "VIS Current Patient Location"},
    {DCM_VISPATIENTSINSTRESIDENCE, DCM_LO, "VIS Patient's Institution Residence"},
    {DCM_VISPATIENTSTATE, DCM_LO, "VIS Patient State"},
    {DCM_VISCOMMENTS, DCM_LT, "VIS Comments"}
};

/* Define the entries for the Waveform group, 003a
*/
static DCMDICT WAV_dictionary[] = {
    {DCM_MAKETAG(0x003a, 0x0000), DCM_UL, "WAV Group Length"},
    {DCM_MAKETAG(0x003a, 0x0002), DCM_SQ, "WAV Waveform Sequence"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0005), DCM_US, "WAV Number of Channels"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0010), DCM_UL, "WAV Number of Samples"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x001a), DCM_DS, "WAV Sampling Frequency"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0020), DCM_SH, "WAV Group Label"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0103), DCM_CS, "WAV Data Value Representation"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0200), DCM_SQ, "WAV Channel Definition"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0202), DCM_IS, "WAV Channel Number"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0203), DCM_SH, "WAV Channel Label"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0205), DCM_CS, "WAV Channel Status"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0208), DCM_SQ, "WAV Waveform Source"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0209), DCM_SQ, "WAV Waveform Source Modifiers"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x020a), DCM_SQ, "WAV Differential Waveform Source"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x020b), DCM_SQ, "WAV Differential Waveform Source Modifiers"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0210), DCM_DS, "WAV Channel Sensitivity"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0211), DCM_SQ, "WAV Channel Sensitivity Units"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0212), DCM_DS, "WAV Channel Sensitivity Correction Factor"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0213), DCM_DS, "WAV Channel Baseline"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0214), DCM_DS, "WAV Channel Time Skew"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0215), DCM_DS, "WAV Channel Sample Skew"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0218), DCM_DS, "WAV Channel Offset"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x021a), DCM_US, "WAV Bits Per Sample"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0216), DCM_CTX, "WAV Channel Minimum Value"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0217), DCM_CTX, "WAV Channel Maximum Value"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0220), DCM_DS, "WAV Filter Low Frequency"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0221), DCM_DS, "WAV Filter High Frequency"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0222), DCM_DS, "WAV Notch Filter Frequency"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x0223), DCM_DS, "WAV Notch Filter Bandwidth"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x003a, 0x1000), DCM_CTX, "WAV Waveform Data"}	/* Sup 30 0.6 */
};

/* Define the entries for the Procedure Step group, 0040
*/

static DCMDICT PRC_dictionary[] = {
    {DCM_PRCGROUPLENGTH, DCM_UL, "PRC Group Length"},
    {DCM_PRCSCHEDULEDSTATIONAETITLE, DCM_AE, "PRC Scheduled Station AE Title"},
    {DCM_PRCSCHEDULEDPROCSTEPSTARTDATE, DCM_DA, "PRC Scheduled Procedure Step Start Date"},
    {DCM_PRCSCHEDULEDPROCSTEPSTARTTIME, DCM_TM, "PRC Scheduled Procedure Step Start Time"},
    {DCM_PRCSCHEDULEDPROCSTEPENDDATE, DCM_DA, "PRC Scheduled Procedure Step End Date"},
    {DCM_PRCSCHEDULEDPROCSTEPENDTIME, DCM_TM, "PRC Scheduled Procedure Step End Time"},
    {DCM_PRCSCHEDULEDPERFORMINGPHYSNAME, DCM_PN, "PRC Scheduled Performing Physician's Name"},
    {DCM_PRCSCHEDULEDPROCSTEPDESCRIPTION, DCM_LO, "PRC Scheduled Step Description"},
    {DCM_PRCSCHEDULEDACTIONITEMCODESEQ, DCM_SQ, "PRC Scheduled Action Item Code Sequence"},
    {DCM_PRCSCHEDULEDPROCSTEPID, DCM_SH, "PRC Scheduled Procedure Step ID"},
    {DCM_PRCSCHEDULEDSTATIONNAME, DCM_SH, "PRC Scheduled Station Name"},
    {DCM_PRCSCHEDULEDPROCSTEPLOCATION, DCM_SH, "PRC Scheduled Procedure Step Location"},
    {DCM_PRCPREMEDICATION, DCM_LO, "PRC Pre-Medication"},
    {DCM_PRCSTATUS, DCM_CS, "PRC SPStep Status"},
    {DCM_PRCREFSTANDALONESOPSEQ, DCM_SQ, "PRC Ref Standalone SOP Inst Seq"},
    {DCM_PRCPERFORMEDSTATIONAET, DCM_AE, "PRC Performed Station AE Title"},
    {DCM_PRCPERFORMEDSTATIONNAME, DCM_SH, "PRC Performed Station Name"},
    {DCM_PRCPERFORMEDLOCATION, DCM_SH, "PRC Performed Location"},
    {DCM_PRCPPSSTARTDATE, DCM_DA, "PRC PPS Start Date"},
    {DCM_PRCPPSSTARTTIME, DCM_TM, "PRC PPS Start Time"},
    {DCM_PRCPPSENDDATE, DCM_DA, "PRC PPS End Date"},
    {DCM_PRCPPSENDTIME, DCM_TM, "PRC PPS End Time"},
    {DCM_PRCPPSSTATUS, DCM_CS, "PRC PPS Status"},
#if 0
    {DCM_PRCPPSID, DCM_CS, "PRC PPS ID"},
#else
    {DCM_PRCPPSID, DCM_SH, "PRC PPS ID"},    /* RWC correction */
#endif
    {DCM_PRCPPSDESCRIPTION, DCM_LO, "PRC PPS Description"},
    {DCM_PRCPPTYPEDESCRIPTION, DCM_LO, "PRC Perf Procedure Type Description"},
    {DCM_PRCPERFORMEDAISEQUENCE, DCM_SQ, "PRC Perf AI Sequence"},
    {DCM_PRCSCHEDSTEPATTRSEQ, DCM_SQ, "PRC Scheduled Step Attr Seq"},
    {DCM_PRCREQUESTATTRIBUTESSEQ, DCM_SQ, "PRC Request Attributes Seq"},
    {DCM_PRCCOMMENTSPPS, DCM_ST, "PRC Comments on PPS"},
    {DCM_PRCQUANTITYSEQ, DCM_SQ, "PRC Quantity Sequence"},
    {DCM_PRCQUANTITY, DCM_DS, "PRC Quantity"},
    {DCM_PRCMEASURINGUNITSSEQ, DCM_SQ, "PRC Measuring Units Sequence"},
    {DCM_PRCBILLINGITEMSEQ, DCM_SQ, "PRC Billing Item Seq"},
    {DCM_PRCTOTALTIMEFLUOROSCOPY, DCM_US, "PRC Total Time Fluoroscopy"},
    {DCM_PRCTOTALNUMBEREXPOSURES, DCM_US, "PRC Total Number Exposures"},
    {DCM_PRCENTRANCEDOSE, DCM_US, "PRC Entrance Dose"},
    {DCM_PRCEXPOSEDAREA, DCM_US, "PRC Exposed Area"},
    {DCM_PRCDISTANCESOURCEENTRANCE, DCM_DS, "PRC Distance Source to Entrance"},
    {DCM_PRCCOMMENTSRADIATIONDOSE, DCM_ST, "PRC Comments on Radiation Dose"},

    {0x00400312, DCM_DS, "PRC X-Ray Output"},		/* 2002.04.26 */
    {0x00400314, DCM_DS, "PRC Half Value Layer"},	/* 2002.04.26 */
    {0x00400316, DCM_DS, "PRC Organ Dose"},		/* 2002.04.26 */
    {0x00400318, DCM_CS, "PRC Organ Exposed"},		/* 2002.04.26 */

    {DCM_PRCBILLINGPROCEDURESTEPSEQ, DCM_SQ, "PRC Billing Proc Step Seq"},
    {DCM_PRCFILMCONSUMPTIONSEQ, DCM_SQ, "PRC Film Consumption Seq"},
    {DCM_PRCBILLINGSUPPLIESDEVICESEQ, DCM_SQ, "PRC Billing Supplies/Devices Seq"},
    {DCM_PRCREFERENCEDPPS, DCM_SQ, "PRC Ref Procedure Step Seq"},
    {DCM_PRCPERFORMEDSERIESSEQ, DCM_SQ, "PRC Performed Series Seq"},
    {DCM_PRCSCHEDULEDPROCSTEPSEQ, DCM_SQ, "PRC Scheduled Procedure Step Sequence"},
    {DCM_PRCCOMMENTSONSCHEDULEDPROCSTEP, DCM_LT, "PRC Comments on the Scheduled Procedure Step"},
    {DCM_MAKETAG(0x0040, 0x050a), DCM_LO, "PRC Specimen Accession Number"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x0550), DCM_SQ, "PRC Specimen Sequence"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x0551), DCM_LO, "PRC Specimen Identifier"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x0552), DCM_SQ, "PRC Specimen Description Sequence"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x0553), DCM_ST, "PRC Specimen Description"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x0555), DCM_SQ, "PRC Acquisition Context Sequence"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x0556), DCM_ST, "PRC Acquisition Context Description"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x059a), DCM_SQ, "PRC Specimen Type Code Sequence"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x06fa), DCM_LO, "PRC Slide Identifier"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x071a), DCM_SQ, "PRC Image Center Point Coordinates Sequence"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x072a), DCM_DS, "PRC X offset in Slide Coordinate System"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x073a), DCM_DS, "PRC Y offset in Slide Coordinate System"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x074a), DCM_DS, "PRC Z offset in Slide Coordinate System"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x08d8), DCM_SQ, "PRC Pixel Spacing Sequence"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x08da), DCM_SQ, "PRC Coordinate System Axis Code Sequence"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x08ea), DCM_SQ, "PRC Measurement Units Code Sequence"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0x09f8), DCM_SQ, "PRC Vital Stain Code Sequence"},	/* Sup 15 */
    {DCM_PRCREQUESTEDPROCEDUREID, DCM_SH, "PRC Requested Procedure ID"},
    {DCM_PRCREASONFORREQUESTEDPROC, DCM_LO, "PRC Reason for the Requested Procedure"},
    {DCM_PRCREQUESTEDPROCPRIORITY, DCM_SH, "PRC Patient Transport Arrangements"},
    {DCM_PRCPATIENTTRANSPORTARRANGEMENTS, DCM_LO, "PRC Patient Transport Arrangements"},
    {DCM_PRCREQUESTEDPROCLOCATION, DCM_LO, "PRC Requested Procedure Location"},
    {DCM_PRCPLACERORDERNUMBERPROC, DCM_SH, "PRC Placer Order Number / Procedure"},

    {DCM_PRCFILLERORDERNUMBERPROC, DCM_SH, "PRC Filler Order Number / Procedure"},
    {DCM_PRCCONFIDENTIALITYCODE, DCM_LO, "PRC Confidentiality Code"},
    {DCM_PRCREPORTINGPRIORITY, DCM_SH, "PRC  Reporting Priority"},
    {DCM_PRCNAMESINTENDEDRECIPIENTSRESULTS, DCM_PN, "PRC Names of Intended Recipients of Results"},
    {DCM_PRCREQUESTEDPROCCOMMENTS, DCM_LT, "PRC Requested Procedure Comments"},
    {DCM_PRCREASONFORIMAGINGSERVICEREQ, DCM_LO, "PRC Reason for the Imaging Service Request"},
    {DCM_PRCISSUEDATEIMAGINGSERVICEREQ, DCM_DA, "PRC Issue Date of Imaging Service Request"},
    {DCM_PRCISSUETIMEIMAGINGSERVICEREQ, DCM_TM, "PRC Issue Time of Imaging Service Request"},
    {DCM_PRCPLACERORDERNUMBERIMAGINGSRVREQ, DCM_SH, "PRC Placer Order Number/Imaging Service Request"},
    {DCM_PRCFILLERORDERNUMBERIMAGINGSRVREQ, DCM_SH, "PRC Filler Order Number/Imaging Service Request"},
    {DCM_PRCORDERENTEREDBY, DCM_PN, "PRC Order Entered By"},
    {DCM_PRCORDERENTERERSLOCATION, DCM_SH, "PRC Order Enterer's Location"},
    {DCM_PRCORDERCALLBACKPHONENUMBER, DCM_SH, "PRC Order Callback Phone Number"},
    {DCM_MAKETAG(0x0040, 0x2016), DCM_LO, "PRC Placer Order Number/ISR"},
    {DCM_MAKETAG(0x0040, 0x2017), DCM_LO, "PRC Filler Order Number/ISR"},

    {DCM_PRCIMAGINGSERVICEREQCOMMENTS, DCM_LT, "PRC Imaging Service Request Comments"},
    {DCM_PRCCONFIDIENTIALITYCONSTRAINTPATIENTDATADES, DCM_LO, "PRC Confidientiality Constraint Patient Data..."},

    {DCM_PRCGPSPSSTATUS, DCM_CS, "PRC General Purpose Sched Procedure Step Status"},
    {DCM_PRCGPPPSSTATUS, DCM_CS, "PRC Gen. Purpose Perf Procedure Step Status"},
    {DCM_PRCGPSPSPRIORITY, DCM_CS, "PRC Gen. Purpose Sched Procedure Step Priority"},
    {DCM_PRCSCHEDULEDPROCAPPCODESEQ, DCM_SQ, "PRC Scheduled Proccessing Application Code Seq"},
    {DCM_PRCGPSPSSTARTDATETIME, DCM_DT, "PRC Sched Procedure Step Start Date and Time"},
    {DCM_PRCGPSPSMULTIPLECOPIESFLAG, DCM_CS, "PRC Multiple Copies Flag"},
    {DCM_PRCPERFORMEDPROCAPPCODESEQ, DCM_SQ, "PRC Performed Proccessing Applications Code Seq"},
    {DCM_PRCHUMANPERFORMERCODESEQ, DCM_SQ, "PRC Human Performer Code Sequence"},
    {DCM_PRCGPSPSEXPECTEDCOMPLETEDATETIME, DCM_DT, "PRC Expected Completion Date and Time"},
    {DCM_PRCRESULTINGGPPERFPROCSTEPSEQ, DCM_SQ, "PRC Resulting Gen Purpose Perf Proc Steps Seq"},
    {DCM_PRCREFERENCEDGPSCHEDPROCSTEPSEQ, DCM_SQ, "PRC Referenced Gen Purp Sched Proc Steps Seq"},
    {DCM_PRCSCHEDWORKITEMCODESEQ, DCM_SQ, "PRC Scheduled Workitem Code Sequence"},
    {DCM_PRCPERFORMEDWORKITEMCODESEQ, DCM_SQ, "PRC Performed Workitem Code Sequence"},
    {DCM_PRCINPUTAVAILFLAG, DCM_CS, "PRC Input Availability Flag"},
    {DCM_PRCINPUTINFOSEQ, DCM_SQ, "PRC Input Information Sequence"},
    {DCM_PRCRELEVANTINFOSEQ, DCM_SQ, "PRC Relevant Information Sequence"},
    {DCM_PRCREFERENCEDGPSPSTRANSACTIONUID, DCM_UI, "PRC Referenced Gen Purp SPS Transaction UID"},
    {DCM_PRCSCHEDSTATIONNAMECODESEQ, DCM_SQ, "PRC Scheduled Station Name Code Sequence"},
    {DCM_PRCSCHEDSTATIONCLASSCODESEQ, DCM_SQ, "PRC Scheduled Station Class Code Sequence"},
    {DCM_PRCSCHEDSTATIONLOCCODESEQ, DCM_SQ, "PRC Sched Station Geographic Location Code Seq"},
    {DCM_PRCPERFORMEDSTATIONNAMECODESEQ, DCM_SQ, "PRC Performed Station Name Code Seq"},
    {DCM_PRCPERFORMEDSTATIONCLASSCODESEQ, DCM_SQ, "PRC Performed Station Class Code Sequence"},
    {DCM_PRCPERFORMEDSTATIONLOCCODESEQ, DCM_SQ, "PRC Perf Station Geographic Location Code Seq"},
    {DCM_PRCREQSUBSWORKITEMCODESEQ, DCM_SQ, "PRC Requested Subsequent Workitem Code Sequence"},
    {DCM_PRCNONDICOMOUTPUTCODESEQ, DCM_SQ, "PRC Non-DICOM Output Code Sequence"},
    {DCM_PRCOUTPUTINFOSEQ, DCM_SQ, "PRC Output Information Sequence"},
    {DCM_PRCSCHEDHUMANPERFORMERSSEQ, DCM_SQ, "PRC Scheduled Human Performers Sequence"},
    {DCM_PRCHUMANPERFORMERSORG, DCM_LO, "PRC Human Performer's Organization"},
    {DCM_PRCHUMANPERFORMERSNAME, DCM_PN, "PRC Human Performer's Name"},


    {DCM_MAKETAG(0x0040, 0xa010), DCM_CS, "PRC Relationship Type"}, /* Sup 23*/
    {DCM_MAKETAG(0x0040, 0xa027), DCM_LO, "PRC Verifying Organization"}, /* Sup 23*/
    {DCM_MAKETAG(0x0040, 0xa030), DCM_DT, "PRC Verification DateTime"}, /* Sup 23*/
    {DCM_MAKETAG(0x0040, 0xa032), DCM_DT, "PRC Observation DateTime"}, /* Sup 23*/
    {DCM_MAKETAG(0x0040, 0xa040), DCM_CS, "PRC Value Type"}, /* Sup 23*/

    {DCM_MAKETAG(0x0040, 0xa043), DCM_SQ, "PRC Concept-name Code Sequence"},	/* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xa050), DCM_CS, "PRC Continuity of Content"}, /* Sup 23*/
    {DCM_MAKETAG(0x0040, 0xa073), DCM_SQ, "PRC Verifying Observer Sequence"}, /* Sup 23*/
    {DCM_MAKETAG(0x0040, 0xa075), DCM_PN, "PRC Verifying Observer Name"}, /* Sup 23*/
    {DCM_MAKETAG(0x0040, 0xa088), DCM_SQ, "PRC Verifying Observer Identification Code Seq"}, /* Sup 23*/
    {DCM_MAKETAG(0x0040, 0xa0a0), DCM_CS, "PRC Referenced Type of Data"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x0040, 0xa0b0), DCM_US, "PRC Referenced Waveform Channels"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x0040, 0xa120), DCM_DT, "PRC Date Time"}, /* Sup 23*/
    {DCM_MAKETAG(0x0040, 0xa121), DCM_DA, "PRC Date"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0xa122), DCM_TM, "PRC Time"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0xa123), DCM_PN, "PRC Person Name"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0xa124), DCM_UI, "PRC UID"}, /* Sup 23*/
    {DCM_MAKETAG(0x0040, 0xa130), DCM_CS, "PRC Temporal Range Type"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x0040, 0xa132), DCM_UL, "PRC Referenced Sample Offsets"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x0040, 0xa138), DCM_DS, "PRC Referenced Time Offsets"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x0040, 0xa13a), DCM_DT, "PRC Referenced Datetime"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x0040, 0xa160), DCM_UT, "PRC Text Value"},	/* */
    {DCM_MAKETAG(0x0040, 0xa168), DCM_SQ, "PRC Concept Code Sequence"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0xa16a), DCM_ST, "PRC Bibliographics Citation"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0xa180), DCM_US, "PRC Annotation Group Number"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x0040, 0xa195), DCM_SQ, "PRC Concept-name Code Sequence Modifier"},	/* Sup 15 */

    {DCM_MAKETAG(0x0040, 0xa300), DCM_SQ, "PRC Measured Value Sequence"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xa30a), DCM_DS, "PRC Numeric Value"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0xa353), DCM_ST, "PRC Address"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0xa354), DCM_LO, "PRC Telephone Number"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0xa360), DCM_SQ, "PRC Predecessor Documents Sequence"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xa370), DCM_SQ, "PRC Referenced Request Sequence"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xa372), DCM_SQ, "PRC Performed Procedure Code Sequence"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xa375), DCM_SQ, "PRC Current Reqeusted Procedure Evidence Seq"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xa385), DCM_SQ, "PRC Pertinent Other Evidence Sequence"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xa491), DCM_CS, "PRC Completion Flag"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xa492), DCM_LO, "PRC Completion Flag Description"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xa493), DCM_CS, "PRC Verification Flag"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xa504), DCM_SQ, "PRC Content Template Sequence"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xa525), DCM_SQ, "PRC Identical Documents Sequence"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xa730), DCM_SQ, "PRC Content Sequence"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xa992), DCM_ST, "PRC Uniform Resource Locator"},	/* Sup 15 */
    {DCM_MAKETAG(0x0040, 0xb020), DCM_SQ, "PRC Annotation Sequence"},	/* Sup 30 0.6 */
    {DCM_MAKETAG(0x0040, 0xadb00), DCM_CS, "PRC Template Identifier"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xadb06), DCM_DT, "PRC Template Version"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xadb07), DCM_DT, "PRC Template Local Version"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xadb0b), DCM_CS, "PRC Template Extension Flag"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xadb0c), DCM_UI, "PRC Template Extension Organization UID"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xadb0d), DCM_UI, "PRC Template Extension Creator UID"}, /* Sup 23 */
    {DCM_MAKETAG(0x0040, 0xadb73), DCM_UL, "PRC Referenced Content Item Identifier"} /* Sup 23 */
};

/* Define the entries for the DEVICE group, 0050
*/
static DCMDICT DEV_dictionary[] = {
    {DCM_DEVCALIBRATIONOBJECT, DCM_CS, "DEV Calibration Object"},
    {DCM_DEVDEVICESEQUENCE, DCM_SQ, "DEV Device Sequence"},
    {DCM_DEVDEVICELENGTH, DCM_DS, "DEV Device Length"},
    {DCM_DEVDEVICEDIAMETER, DCM_DS, "DEV Device Diameter"},
    {DCM_DEVDEVICEDIAMETERUNITS, DCM_CS, "DEV Device Diameter Units"},
    {DCM_DEVDEVICEVOLUME, DCM_DS, "DEV Device Volume"},
    {DCM_DEVINTERMARKERDISTANCE, DCM_DS, "DEV Inter-Marker Distance"},
    {DCM_DEVDEVICEDESCRIPTION, DCM_LO, "DEV Device Description"},
};

/* Define the entries for the RESULTS group, 4008
*/
static DCMDICT RES_dictionary[] = {
    {DCM_RESGROUPLENGTH, DCM_UL, "RES Group Length"},
    {DCM_RESID, DCM_SH, "RES Results ID"},
    {DCM_RESIDISSUER, DCM_LO, "RES Results ID Issuer"},
    {DCM_RESREFERENCEDINTERPSEQ, DCM_SQ, "RES Referenced Interpretation Sequence"},
    {DCM_RESINTERPRECORDEDDATE, DCM_DA, "RES Interpretation Recorded Date"},
    {DCM_RESINTERPRECORDEDTIME, DCM_TM, "RES Interpretation Recorded Time"},
    {DCM_RESINTERPRECORDER, DCM_PN, "RES Interpretation Recorder"},
    {DCM_RESREFERENCETORECORDEDSOUND, DCM_LO, "RES Reference to Recorded Sound"},
    {DCM_RESINTERPTRANSCRIPTIONDATE, DCM_DA, "RES Interpretation Transcription Date"},
    {DCM_RESINTERPTRANSCRIPTIONTIME, DCM_TM, "RES Interpretation Transcription Time"},
    {DCM_RESINTERPTRANSCRIBER, DCM_PN, "RES Interpretation Transcriber"},
    {DCM_RESINTERPTEXT, DCM_ST, "RES Interpretation Text"},
    {DCM_RESINTERPAUTHOR, DCM_PN, "RES Interpretation Author"},
    {DCM_RESINTERPAPPROVERSEQUENCE, DCM_SQ, "RES Interpretation Approver Sequence"},
    {DCM_RESINTERPAPPROVALDATE, DCM_DA, "RES Interpretation Approval Date"},
    {DCM_RESINTERPAPPROVALTIME, DCM_TM, "RES Interpretation Approval Time"},
    {DCM_RESPHYSICIANAPPROVINGINTERP, DCM_PN, "RES Physician Approving Interpretation"},
    {DCM_RESDIAGNOSIS, DCM_LT, "RES Diagnosis"},
    {DCM_RESDIAGNOSISCODESEQ, DCM_SQ, "RES Diagnosis Code Sequence"},
    {DCM_RESDISTRIBUTIIONLISTSEQUENCE, DCM_SQ, "RES Results Distribution List Sequence"},
    {DCM_RESDISTRIBUTIONNAME, DCM_PN, "RES Distribution Name"},
    {DCM_RESDISTRIBUTIONADDRESS, DCM_LO, "RES Distribution Address"},
    {DCM_RESINTERPID, DCM_SH, "RES Interpretation ID"},
    {DCM_RESINTERPIDISSUER, DCM_LO, "RES Interpretation ID Issuer"},
    {DCM_RESINTERPTYPEID, DCM_CS, "RES Interpretation Type ID"},
    {DCM_RESINTERPSTATUSID, DCM_CS, "RES Interpretation Status ID"},
    {DCM_RESIMPRESSIONS, DCM_ST, "RES Impressions"},
    {DCM_RESCOMMENTS, DCM_ST, "RES Comments"}
};

/* Define entries for the CURVE group */
static DCMDICT CRV_dictionary[] = {
    {DCM_CURVEGROUPLENGTH, DCM_UL, "CRV Group Length"},
    {DCM_CURVEDIMENSIONS, DCM_US, "CRV Curve Dimensions"},
    {DCM_CURVENUMBEROFPOINTS, DCM_US, "CRV Number of points"},
    {DCM_CURVETYPEOFDATA, DCM_CS, "CRV Type of Data"},
    {DCM_CURVEDESCRIPTION, DCM_LO, "CRV Curve Description"},
    {DCM_CURVEAXISUNITS, DCM_SH, "CRV Axis Units"},
    {DCM_CURVEAXISLABELS, DCM_SH, "CRV Axis Labels"},
    {DCM_CURVEDATAVALUEREPRESENTATION, DCM_US, "CRV Data Value Representation"},
    {DCM_CURVEMINCOORDINATEVALUE, DCM_US, "CRV Minimum Coordinate Value"},
    {DCM_CURVEMAXCOORDINATEVALUE, DCM_US, "CRV Maximum Coordinate Value"},
    {DCM_CURVERANGE, DCM_SH, "CRV Curve Range"},
    {DCM_CURVEDATADESCRIPTOR, DCM_US, "CRV Data Descriptor"},
    {DCM_CURVECOORDINATESTARTVALUE, DCM_US, "CRV Coordinate Start Value"},
    {DCM_CURVECOORDINATESTEPVALUE, DCM_US, "CRV Coordinate Step Value"},
    {DCM_CURVEAUDIOTYPE, DCM_US, "CRV Audio Type"},
    {DCM_CURVEAUDIOSAMPLEFORMAT, DCM_US, "CRV Audio Sample Format"},
    {DCM_CURVENUMBEROFCHANNELS, DCM_US, "CRV Number of Channels"},
    {DCM_CURVENUMBEROFSAMPLES, DCM_UL, "CRV Number of Samples"},
    {DCM_CURVESAMPLERATE, DCM_UL, "CRV Sample Rate"},
    {DCM_CURVETOTALTIME, DCM_UL, "CRV Total Time"},
    {DCM_CURVEAUDIOSAMPLEDATA, DCM_OW, "CRV Audio Sample Data"},
    {DCM_CURVEAUDIOCOMMENTS, DCM_LT, "CRV Audio Comments"},
    {DCM_CURVELABEL, DCM_LO, "CRV Curve Label"},
    {DCM_CURVEREFOVERLAYSEQUENCE, DCM_SQ, "CRV Referenced Overlay Sequence"},
    {DCM_CURVEREFOVERLAYGROUP, DCM_US, "CRV Referenced Overlay Group"},
    {DCM_CURVEDATA, DCM_OW, "CRV Curve Data"}
};

/* Define the entries for the NMI (nuclear medicine image) group, 0054 */
static DCMDICT NMI_dictionary[] = {
    {DCM_NMIGROUPLENGTH, DCM_UL, "NMI Group Length"},
    {DCM_NMIENERGYWINDOWVECTOR, DCM_US, "NMI Energy Window Vector"},
    {DCM_NMINUMBEROFENERGYWINDOWS, DCM_US, "NMI Number of Energy Windows"},
    {DCM_NMIENERGYWINDOWINFOSEQ, DCM_SQ, "NMI Energy Window Information Sequence"},
    {DCM_NMIENERGYWINDOWRANGESEQ, DCM_SQ, "NMI Energy Window Range Sequence"},
    {DCM_NMIENERGYWINDOWLOWERLIMIT, DCM_DS, "NMI Energy Window Lower Limit"},
    {DCM_NMIENERGYWINDOWUPPERLIMIT, DCM_DS, "NMI Energy Window Upper Limit"},
    {DCM_NMIRADIOPHARMINFOSEQ, DCM_SQ, "NMI Radiopharmaceutical Information Sequence"},
    {DCM_NMIRESIDUALSYRINGECOUNTS, DCM_IS, "NMI Residual Syringe Counts"},
    {DCM_NMIENERGYWINDOWNAME, DCM_SH, "NMI Energy Window Name"},
    {DCM_NMIDETECTORVECTOR, DCM_US, "NMI Detector Vector"},
    {DCM_NMINUMBEROFDETECTORS, DCM_US, "NMI Number of Detectors"},
    {DCM_NMIDETECTORINFOSEQUENCE, DCM_SQ, "NMI Detector Information Sequence"},
    {DCM_NMIPHASEVECTOR, DCM_US, "NMI Phase Vector"},
    {DCM_NMINUMBEROFPHASES, DCM_US, "NMI Number of Phases"},
    {DCM_NMIPHASEINFOSEQUENCE, DCM_SQ, "NMI Phase Information Sequence"},
    {DCM_NMINUMBEROFFRAMESINPHASE, DCM_US, "NMI Number of Frames in Phase"},
    {DCM_NMIPHASEDELAY, DCM_IS, "NMI Phase Delay"},
    {DCM_NMIPAUSEBETWEENFRAMES, DCM_IS, "NMI Pause between Frames"},
    {DCM_NMIROTATIONVECTOR, DCM_US, "NMI Rotation Vector"},
    {DCM_NMINUMBEROFROTATIONS, DCM_US, "NMI Number of rotations"},
    {DCM_NMIROTATIONINFOSEQUENCE, DCM_SQ, "NMI Rotation Information Sequence"},
    {DCM_NMINUMBEROFFRAMESINROTATION, DCM_US, "NMI Number of frames in rotation"},
    {DCM_NMIRRINTERVALVECTOR, DCM_US, "NMI R-R Interval Vector"},
    {DCM_NMINUMBEROFRRINTERVALS, DCM_US, "NMI Number of R-R Intervals"},
    {DCM_NMIGATEDINFOSEQUENCE, DCM_SQ, "NMI Gated Information Sequence"},
    {DCM_NMIDATAINFORMATIONSEQUENCE, DCM_SQ, "NMI Data Information Sequence"},
    {DCM_NMITIMESLOTVECTOR, DCM_US, "NMI Time Slot Vector"},
    {DCM_NMINUMBEROFTIMESLOTS, DCM_US, "NMI Number of Time Slots"},
    {DCM_NMITIMESLOTINFOSEQUENCE, DCM_SQ, "NMI Time Slot Information Sequence"},
    {DCM_NMITIMESLOTTIME, DCM_DS, "NMI Time Slot Time"},
    {DCM_NMISLICEVECTOR, DCM_US, "NMI Slice Vector"},
    {DCM_NMINUMBEROFSLICES, DCM_US, "NMI Number of Slices"},
    {DCM_NMIANGULARVIEWVECTOR, DCM_US, "NMI Angular View Vector"},
    {DCM_NMITIMESLICEVECTOR, DCM_US, "NMI Time Slice Vector"},
    {DCM_NMINUMBEROFTIMESLICES, DCM_US, "NMI Number of Time Slices"},
    {DCM_NMISTARTANGLE, DCM_DS, "NMI Start Angle"},
    {DCM_NMITYPEOFDETECTORMOTION, DCM_CS, "NMI Type of Detector Motion"},
    {DCM_NMITRIGGERVECTOR, DCM_IS, "NMI Trigger Vector"},
    {DCM_NMINUMBEROFTRIGGERSINPHASE, DCM_US, "NMI Number of Triggers in Phase"},
    {DCM_NMIVIEWCODESEQUENCE, DCM_SQ, "NMI View Code Sequence"},
    {DCM_NMIVIEWANGULATIONMODIFIERCODESEQ, DCM_SQ, "NMI View Angulation Modifer Code Sequence"},
    {DCM_NMIRADIONUCLIDECODESEQUENCE, DCM_SQ, "NMI Radionuclide Code Sequence"},
    {DCM_NMIRADIOPHARMROUTECODESEQUENCE, DCM_SQ, "NMI Radiopharmaceutical Route Code Sequence"},
    {DCM_NMIRADIOPHARMCODESEQUENCE, DCM_SQ, "NMI Radiopahrmaceutical Code Sequence"},
    {DCM_NMICALIBRATIONDATASEQUENCE, DCM_SQ, "NMI Calibration Data Sequence"},
    {DCM_NMIENERGYWINDOWNUMBER, DCM_US, "NMI Energy Window Number"},
    {DCM_NMIIMAGEID, DCM_SH, "NMI Image ID"},
    {DCM_NMIPATIENTORIENTATIONCODESEQ, DCM_SQ, "NMI Patient Orientation Code Sequence"},
    {DCM_NMIPATIENTORIENTATIONMODIFIERCODESEQ, DCM_SQ, "NMI Patient Orientation Modifier Code Sequence"},
    {DCM_NMIPATIENTGANTRYRELATIONSHIPCODESEQ, DCM_SQ, "NMI Patient Gantry Relationship Code Sequence"},
    {DCM_NMISERIESTYPE, DCM_CS, "NMI Series Type"},
    {DCM_NMIUNITS, DCM_CS, "NMI Units"},
    {DCM_NMICOUNTSSOURCE, DCM_CS, "NMI Counts Source"},	/* 1002 */
    {DCM_NMIREPROJECTIONMETHOD, DCM_CS, "NMI Reprojection Method"},	/* 1004 */
    {DCM_NMIRANDOMSCORRECTIONMETHOD, DCM_CS,
    "NMI Randoms Correction Method"},	/* 1100 */
    {DCM_NMIATTENUATIONCORRECTIONMETHOD, DCM_LO,
    "NMI Attenuation Correction Method"},	/* 1101 */
    {DCM_NMIDECAYCORRECTION, DCM_CS, "NMI Decay Correction"},	/* 1102 */
    {DCM_NMIRECONSTRUCTIONMETHOD, DCM_LO, "NMI Reconstruction Method"},	/* 1103 */
    {DCM_NMIDETECTORLINESRESPONSEUSED, DCM_LO,
    "NMI Detector Lines of Response Used"},	/* 1104 */
    {DCM_NMISCATTERCORRECTIONMETHOD, DCM_LO, "NMI Scatter Correction Method"},	/* 1105 */
    {DCM_NMIAXIALACCEPTANCE, DCM_DS, "NMI Axial Acceptance"},	/* 1200 */
    {DCM_NMIAXIALMASH, DCM_IS, "NMI Axial Mash"},	/* 1201 */
    {DCM_NMITRANSVERSEMASH, DCM_IS, "NMI Transverse Mash"},	/* 1202 */
    {DCM_NMIDETECTORELEMENTSIZE, DCM_DS, "NMI Detector Element Size"},	/* 1203 */
    {DCM_NMICOINCIDENCEWINDOWWIDTH, DCM_DS, "NMI Coincidence Window Width"},	/* 1210 */
    {DCM_NMISECONDARYCOUNTSTYPE, DCM_CS, "NMI Secondary Counts Type"},	/* 1220 */
    {DCM_NMIFRAMEREFERENCETIME, DCM_DS, "NMI Frame Reference Time"},	/* 1300 */
    {DCM_NMIPRIMARYCOUNTSACCUMULATED, DCM_IS,
    "NMI Primary (Prompts) Counts Accumulated"},	/* 1310 */
    {DCM_NMISECONDARYCOUNTSACCUMULATED, DCM_IS,
    "NMI Secondary Counts Accumulated"},	/* 1311 */
    {DCM_NMISLICESENSITIVITYFACTOR, DCM_DS, "NMI Slice Sensitivity Factor"},	/* 1320 */
    {DCM_NMIDECAYFACTOR, DCM_DS, "NMI Decay Factor"},	/* 1321 */
    {DCM_NMIDOSECALIBRATIONFACTOR, DCM_DS, "NMI Dose Calibration Factor"},	/* 1322 */
    {DCM_NMISCATTERFRACTIONFACTOR, DCM_DS, "NMI Scatter Fraction Factor"},	/* 1323 */
    {DCM_NMIDEADTIMEFACTOR, DCM_DS, "NMI Dead Time Factor"},	/* 1324 */
    {DCM_NMIIMAGEINDEX, DCM_US, "NMI Image Index"},	/* 1330 */
    {DCM_NMICOUNTSINCLUDED, DCM_CS, "NMI Counts Included"},	/* 1400 */
    {DCM_NMIDEADTIMECORRECTIONFLAG, DCM_CS,
    "NMI Dead Time Correction Flag"},	/* 1401 */
};

/* Define the entries for the Graphics group, 0070 */
static DCMDICT GRP_dictionary[] = {
    {DCM_MAKETAG(0x0070, 0x0000), DCM_UL, "GRP Group Length"},
    {DCM_MAKETAG(0x0070, 0x0022), DCM_FL, "GRP Graphic Data"}, /* Sup 33 */
    {DCM_MAKETAG(0x0070, 0x0023), DCM_CS, "GRP Graphic Type"}, /* Sup 33 */
    {DCM_MAKETAG(0x0070, 0x0024), DCM_CS, "GRP Graphic Filled"}, /* Sup 33 */
    {DCM_MAKETAG(0x0070, 0x0041), DCM_CS, "GRP Image Horizontal Flip"}, /* Sup 33*/
    {DCM_MAKETAG(0x0070, 0x0042), DCM_US, "GRP Image Rotation"}, /* Sup 33 */
    {DCM_MAKETAG(0x0070, 0x0052), DCM_SL, "GRP Displayed Area Top LH Corner"},
    {DCM_MAKETAG(0x0070, 0x0053), DCM_SL, "GRP Displayed Area Bottom RH Corner"},
    {DCM_MAKETAG(0x0070, 0x005a), DCM_SQ, "GRP Display Area Selection Seq"},
    {DCM_MAKETAG(0x0070, 0x0060), DCM_SQ, "GRP Graphic Layer Sequence"},
    {DCM_MAKETAG(0x0070, 0x0062), DCM_IS, "GRP Graphic Layer Order"},
    {DCM_MAKETAG(0x0070, 0x0066), DCM_US, "GRP Graphic Layer Rec Disp GS Val"},
    {DCM_MAKETAG(0x0070, 0x0067), DCM_US, "GRP Graphic Layer Rec Disp RGB Val"},
    {DCM_MAKETAG(0x0070, 0x0068), DCM_LO, "GRP Graphic Layer Description"},

    {DCM_MAKETAG(0x0070, 0x0080), DCM_CS, "GRP Presentation Label"}, /* Sup 33 */
    {DCM_MAKETAG(0x0070, 0x0081), DCM_LO, "GRP Presentation Description"}, /* Sup 33 */
    {DCM_MAKETAG(0x0070, 0x0082), DCM_DA, "GRP Presentation Creation Date"}, /* Sup 33 */
    {DCM_MAKETAG(0x0070, 0x0083), DCM_TM, "GRP Presentation Creation Time"}, /* Sup 33 */
    {DCM_MAKETAG(0x0070, 0x0084), DCM_PN, "GRP Presentation Creators Name"}, /* Sup 33 */
    {DCM_MAKETAG(0x0070, 0x0100), DCM_CS, "GRP Presentation Size Mode"}, /* Sup 33 */
    {DCM_MAKETAG(0x0070, 0x0101), DCM_DS, "GRP Presentation Pixel Spacing"}, /* Sup 33 */
    {DCM_MAKETAG(0x0070, 0x0102), DCM_IS, "GRP Presentation Pixel Aspect Ratio"}, /* Sup 33 */
    {DCM_MAKETAG(0x0070, 0x0103), DCM_FL, "GRP Presentation Pixel Magnification Ratio"}, /* Sup 33 */
};

/* Define the entries for the OLY (Overlay) group */
static DCMDICT OLY_dictionary[] = {
    {DCM_OLYGROUPLENGTH, DCM_UL, "OLY Group Length"},
    {DCM_OLYROWS, DCM_US, "OLY Rows"},
    {DCM_OLYCOLUMNS, DCM_US, "OLY Columns"},
    {DCM_OLYPLANES, DCM_US, "OLY Planes"},
    {DCM_OLYNUMBEROFFRAMESINOVERLAY, DCM_IS, "OLY Number of frames in Overlay"},
    {DCM_OLYOVERLAYDESCRIPTION, DCM_LO, "OLY Overlay Description"},
    {DCM_OLYTYPE, DCM_CS, "OLY Type"},
    {DCM_OLYSUBTYPE, DCM_LO, "OLY Subtype"},
    {DCM_OLYORIGIN, DCM_SS, "OLY Origin"},
    {DCM_OLYIMAGEFRAMEORIGIN, DCM_US, "OLY Image Frame Origin"},
    {DCM_OLYOVERLAYPLANEORIGIN, DCM_US, "OLY Overlay Plane Origin"},
    {DCM_OLYCOMPRESSIONCODE, DCM_LO, "OLY Compression Code (RET)"},
    {DCM_OLYBITSALLOCATED, DCM_US, "OLY Overlay Bits Allocated"},
    {DCM_OLYBITPOSITION, DCM_US, "OLY Overlay Bit Position"},
    {DCM_OLYOVERLAYFORMAT, DCM_LO, "OLY Overlay Format (RET)"},
    {DCM_OLYOVERLAYLOCATION, DCM_US, "OLY Overlay Location (RET)"},
    {DCM_OLYDESCRIPTORGRAY, DCM_US, "OLY Overlay Descriptor - Gray"},
    {DCM_OLYDESCRIPTORRED, DCM_US, "OLY Overlay Descriptor - Red"},
    {DCM_OLYDESCRIPTORGREEN, DCM_US, "OLY Overlay Descriptor - Green"},
    {DCM_OLYDESCRIPTORBLUE, DCM_US, "OLY Overlay Descriptor - Blue"},
    {DCM_OLYGRAY, DCM_US, "OLY Overlays - Gray"},
    {DCM_OLYRED, DCM_US, "OLY Overlays - Red"},
    {DCM_OLYGREEN, DCM_US, "OLY Overlays - Green"},
    {DCM_OLYBLUE, DCM_US, "OLY Overlays - Blue"},
    {DCM_OLYROIAREA, DCM_IS, "OLY ROI Area"},
    {DCM_OLYROIMEAN, DCM_DS, "OLY ROI Mean"},
    {DCM_OLYROISTANDARDDEVIATION, DCM_DS, "OLY ROI Standard Deviation"},
    {DCM_OLYOVERLAYLABEL, DCM_LO, "OLY Overlay Label"},
    {DCM_OLYDATA, DCM_OW, "OLY Data"},
    {DCM_OLYCOMMENTS, DCM_LO, "OLY Comments (RET)"}
};

/* Define the entries for the PIXEL group (7FE0)
*/
static DCMDICT PXL_dictionary[] = {
    {DCM_PXLGROUPLENGTH, DCM_UL, "PXL Group Length"},
    {DCM_PXLPIXELDATA, DCM_OT, "PXL Pixel Data"}
};

/* Define the elements for the MEDIA group (0088) */
static DCMDICT MED_dictionary[] = {
    {DCM_MEDIAGROUPLENGTH, DCM_UL, "MED Media Group Length "},
    {DCM_MEDIASTORAGEFILESETID, DCM_SH, "MED Storage Media File-set ID"},
    {DCM_MEDIASTORAGEFILESETUID, DCM_UI, "MED Storage Media File-setUID"},
    {DCM_MEDIAICONIMAGE, DCM_SQ, "MED Icon Image Sequence"},
    {DCM_MEDIATOPICTITLE, DCM_LO, "MED Topic Title"},
    {DCM_MEDIATOPICSUBJECT, DCM_ST, "MED Topic Subject"},
    {DCM_MEDIATOPICAUTHOR, DCM_LO, "MED Topic Author"},
    {DCM_MEDIATOPICKEYWORD, DCM_LO, "MED Topic Keywords"}
};

/* Define the entries in the BASICFILMSESSION group (2000)
*/
static DCMDICT BFS_dictionary[] = {
    {DCM_BFSGROUPLENGTH, DCM_UL, "BFS Group Length"},
    {DCM_BFSCOPIES, DCM_IS, "BFS Number of copies printed for each film"},
    {DCM_BFSPRINTPRIORITY, DCM_CS, "BFS Specifies priority of print job"},
    {DCM_BFSMEDIUMTYPE, DCM_CS, "BFS Medium on which page will be printed"},
    {DCM_BFSFILMDESTINATION, DCM_CS, "BFS Film destination"},
    {DCM_BFSFILMSESSIONLABEL, DCM_LO, "BFS Human readable label to identify film"},
    {DCM_BFSMEMORYALLOCATION, DCM_IS, "BFS Amount of mem allocated for film session"},
    {DCM_BFSREFERENCEDFILMBOXSEQ, DCM_SQ, "BFS seq of UIDs of diff FILMBOX instances"}
};

/* Define the entries in the BASICFILMBOX group (2010)
*/
static DCMDICT BFB_dictionary[] = {
    {DCM_BFBGROUPLENGTH, DCM_UL, "BFB Group Length"},
    {DCM_BFBIMAGEDISPLAYFORMAT, DCM_ST, "BFB Type of image display format"},
    {DCM_BFBANNOTATIONDISPLAYFORMAT, DCM_CS, "BFB Id of annotation display format"},
    {DCM_BFBFILMORIENTATION, DCM_CS, "BFB Film orientation"},
    {DCM_BFBFILMSIZEID, DCM_CS, "BFB Film size identification"},
    {DCM_BFBMAGNIFICATIONTYPE, DCM_CS, "BFB Interpol. type by which printer mag image"},
    {DCM_BFBSMOOTHINGTYPE, DCM_CS, "BFB Specifies type of interpolation function"},
    {DCM_BFBBORDERDENSITY, DCM_CS, "BFB density of film areas around/between images"},
    {DCM_BFBEMPTYIMAGEDENSITY, DCM_CS, "BFB density of image box area having no image"},
    {DCM_BFBMINDENSITY, DCM_US, "BFB Minimum density of images on the film"},
    {DCM_BFBMAXDENSITY, DCM_US, "BFB Maximum density of images on the film"},
    {DCM_BFBTRIM, DCM_CS, "BFB specifies whether to trim or not"},
    {DCM_BFBCONFIGURATIONINFO, DCM_ST, "BFB ID of configuration table"},
    {DCM_BFBREFBASICFILMSESSIONSEQ, DCM_SQ, "BFB seq. of film session instance"},
    {DCM_BFBREFBASICIMAGEBOXSEQ, DCM_SQ, "BFB seq. of basic image box SOP instance"},
    {DCM_BFBREFBASICANNOTBOXSEQ, DCM_SQ, "BFB seq. of basic annotation box SOP instance"},
};

/* Defines the entries in the BASICIMAGEBOX (2020)
*/
static DCMDICT BIB_dictionary[] = {
    {DCM_BIBGROUPLENGTH, DCM_UL, "BIB Group Length"},
    {DCM_BIBIMAGEPOSITION, DCM_US, "BIB Specifies position of the image in the film"},
    {DCM_BIBPOLARITY, DCM_CS, "BIB Specifies image polarity"},
    {DCM_BIBREQUESTEDIMAGESIZE, DCM_DS, "BIB Requested image size"},
    {DCM_BIBPREFORMATGREYSCALEIMAGESEQ, DCM_SQ, "BIB Preformatted Greyscale image"},
    {DCM_BIBPREFORMATCOLORIMAGESEQ, DCM_SQ, "BIB Preformatted Color image"},
    {DCM_BIBREFIMAGEOVERLAYBOXSEQ, DCM_SQ, "BIB Referenced Image Overlay Box seq"},
    {DCM_BIBREFVOILUTSEQ, DCM_SQ, "BIB Referenced VOI LUT seq."}
};

/* Defines the entries in the BASICANNOTATIONBOX group (2030)
*/
static DCMDICT BAB_dictionary[] = {
    {DCM_BABGROUPLENGTH, DCM_UL, "BAB Group Length"},
    {DCM_BABANNOTATIONPOSITION, DCM_US, "BAB posn of the annot. box in parent film box"},
    {DCM_BABTEXTSTRING, DCM_LO, "BAB text string"}
};

/* Defines entries for BASICIMAGEOVERLAYBOX group (2040)
*/
static DCMDICT IOB_dictionary[] = {
    {DCM_IOBGROUPLENGTH, DCM_UL, "IOB Group Length"},
    {DCM_IOBREFOVERLAYPLANESEQ, DCM_SQ, "IOB Ref Overlay Plane Sequence"},
    {DCM_IOBREFOVERLAYPLANEGROUPS, DCM_US, "IOB Ref Overlay Plane Groups"},
    {DCM_IOBOVERLAYMAGNIFICATIONTYPE, DCM_CS, "IOB Overlay Magnification Type"},
    {DCM_IOBOVERLAYSMOOTHINGTYPE, DCM_CS, "IOB Overlay Smoothing Type"},
    {DCM_IOBOVERLAYFOREGROUNDDENSITY, DCM_CS, "IOB Overlay Foreground Density"},
    {DCM_IOBOVERLAYMODE, DCM_CS, "IOB Overlay Mode"},
    {DCM_IOBTHRESHOLDDENSITY, DCM_CS, "IOB Threshold Density"},
    {DCM_IOBREFIMAGEBOXSEQUENCE, DCM_SQ, "IOB Ref Image Box Sequence (RET)"}
};

/* Defines entries for Presentation LUT Group (2050)
*/
static DCMDICT PLUT_dictionary[] = {
    {DCM_MAKETAG(0x2050, 0x0000), DCM_UL, "PLUT Group Length"},
    {DCM_MAKETAG(0x2050, 0x0010), DCM_SQ, "PLUT Presentation LUT Sequence"},
    {DCM_MAKETAG(0x2050, 0x0020), DCM_CS, "PLUT Presentation LUT Shape"},
    {DCM_MAKETAG(0x2050, 0x0500), DCM_SQ, "PLUT Referenced Presentation LUT Sequence"}
};

/* Defines the entries in the PRINTJOB group (2100)
*/
static DCMDICT PJ_dictionary[] = {
    {DCM_PJGROUPLENGTH, DCM_UL, "PJ Group Length"},
    {DCM_PJEXECUTIONSTATUS, DCM_CS, "PJ execution status of print job"},
    {DCM_PJEXECUTIONSTATUSINFO, DCM_CS, "PJ additional information"},
    {DCM_PJCREATIONDATE, DCM_DA, "PJ date of print job creation"},
    {DCM_PJCREATIONTIME, DCM_TM, "PJ time of print job creation"},
    {DCM_PJORIGINATOR, DCM_AE, "PJ Appln entity title that issued the print opn"},
    {DCM_PJREFPRINTJOBSEQ, DCM_SQ, "PJ Referenced print job seq."}
};

/* Defines the entries in the PRINTER group (2110)
*/
static DCMDICT PRN_dictionary[] = {
    {DCM_PRINTERGROUPLENGTH, DCM_UL, "PRINTER Group Length"},
    {DCM_PRINTERSTATUS, DCM_CS, "PRINTER printer device status"},
    {DCM_PRINTERSTATUSINFO, DCM_CS, "PRINTER additional information"},
    {DCM_PRINTERNAME, DCM_LO, "PRINTER printer name"},
    {DCM_PRINTERQUEUEID, DCM_SH, "Printer Queue ID"}
};

/* Define the entries in the 0x3002 group, used for RT planning
*/
static DCMDICT G3002_dictionary[] = {
    {DCM_MAKETAG(0x3002, 0x0000), DCM_UL, "RT Group Length"},
    {DCM_MAKETAG(0x3002, 0x0002), DCM_SH, "RT Image Label"},
    {DCM_MAKETAG(0x3002, 0x0003), DCM_LO, "RT Image Name"},
    {DCM_MAKETAG(0x3002, 0x0004), DCM_ST, "RT Image Description"},
    {DCM_MAKETAG(0x3002, 0x000a), DCM_CS, "RT Reported Values Origin"},
    {DCM_MAKETAG(0x3002, 0x000c), DCM_CS, "RT Image Plane"},
    {DCM_MAKETAG(0x3002, 0x000e), DCM_DS, "RT X-Ray Image Receptor Angle"},
    {DCM_MAKETAG(0x3002, 0x0010), DCM_DS, "RT Image Orientation"},
    {DCM_MAKETAG(0x3002, 0x0011), DCM_DS, "RT Image Plane Pixel Spacing"},
    {DCM_MAKETAG(0x3002, 0x0012), DCM_DS, "RT Image Position"},
    {DCM_MAKETAG(0x3002, 0x0020), DCM_SH, "RT Radiation Machine Name"},
    {DCM_MAKETAG(0x3002, 0x0022), DCM_DS, "RT Radiation Machine SAD"},
    {DCM_MAKETAG(0x3002, 0x0024), DCM_DS, "RT Radiation Machine SSD"},
    {DCM_MAKETAG(0x3002, 0x0026), DCM_DS, "RT Image SID"},
    {DCM_MAKETAG(0x3002, 0x0028), DCM_DS, "RT Source to Reference Object Distance"},
    {DCM_MAKETAG(0x3002, 0x0029), DCM_IS, "RT Fraction Number"},
    {DCM_MAKETAG(0x3002, 0x0030), DCM_SQ, "RT Exposure Sequence"},
    {DCM_MAKETAG(0x3002, 0x0032), DCM_DS, "RT Meterset Exposure"}
};

/* Define the entries in the 0x3004 group, Dose Volume Histogram (DVH),
** used in RT planning.
*/
static DCMDICT DVH_dictionary[] = {
    {DCM_MAKETAG(0x3004, 0x0000), DCM_UL, "DVH Group Length"},
    {DCM_MAKETAG(0x3004, 0x0001), DCM_CS, "DVH Type"},
    {DCM_MAKETAG(0x3004, 0x0002), DCM_CS, "DVH Dose Units"},
    {DCM_MAKETAG(0x3004, 0x0004), DCM_CS, "DVH Dose Type"},
    {DCM_MAKETAG(0x3004, 0x0006), DCM_LO, "DVH Dose Comment"},
    {DCM_MAKETAG(0x3004, 0x0008), DCM_DS, "DVH Normalization Point"},
    {DCM_MAKETAG(0x3004, 0x000a), DCM_CS, "DVH Dose Summation Type"},
    {DCM_MAKETAG(0x3004, 0x000c), DCM_DS, "DVH Grid Frame Offset Vector"},
    {DCM_MAKETAG(0x3004, 0x000e), DCM_DS, "DVH Dose Grid Scaling"},
    {DCM_MAKETAG(0x3004, 0x0010), DCM_SQ, "DVH RT Dose ROI Sequence"},
    {DCM_MAKETAG(0x3004, 0x0012), DCM_DS, "DVH Dose Value"},
    {DCM_MAKETAG(0x3004, 0x0040), DCM_DS, "DVH Normalization Point"},
    {DCM_MAKETAG(0x3004, 0x0042), DCM_DS, "DVH Normalization Dose Value"},
    {DCM_MAKETAG(0x3004, 0x0050), DCM_SQ, "DVH Sequence"},
    {DCM_MAKETAG(0x3004, 0x0052), DCM_DS, "DVH Dose Scaling"},
    {DCM_MAKETAG(0x3004, 0x0054), DCM_CS, "DVH Volume Units"},
    {DCM_MAKETAG(0x3004, 0x0056), DCM_IS, "DVH Number of Bins"},
    {DCM_MAKETAG(0x3004, 0x0058), DCM_DS, "DVH Data"},
    {DCM_MAKETAG(0x3004, 0x0060), DCM_SQ, "DVH Referenced ROI Sequence"},
    {DCM_MAKETAG(0x3004, 0x0062), DCM_CS, "DVH ROI Contribution Type"},
    {DCM_MAKETAG(0x3004, 0x0070), DCM_DS, "DVH Minimum Dose"},
    {DCM_MAKETAG(0x3004, 0x0072), DCM_DS, "DVH Maximum Dose"},
    {DCM_MAKETAG(0x3004, 0x0074), DCM_DS, "DVH Mean Dose"}
};

/* Define the entries in the 0x3006 group, Structure Set,
** used in RT planning.
*/
static DCMDICT SSET_dictionary[] = {
    {DCM_MAKETAG(0x3006, 0x0000), DCM_UL, "SSET Group Length"},
    {DCM_MAKETAG(0x3006, 0x0002), DCM_SH, "SSET Structure Set Label"},
    {DCM_MAKETAG(0x3006, 0x0004), DCM_LO, "SSET Structure Set Name"},
    {DCM_MAKETAG(0x3006, 0x0006), DCM_ST, "SSET Structure Set Description"},
    {DCM_MAKETAG(0x3006, 0x0008), DCM_DA, "SSET Structure Set Date"},
    {DCM_MAKETAG(0x3006, 0x0009), DCM_TM, "SSET Structure Set Time"},
    {DCM_MAKETAG(0x3006, 0x0010), DCM_SQ, "SSET Referenced Frame of Reference Sequence"},
    {DCM_MAKETAG(0x3006, 0x0012), DCM_SQ, "SSET RT Referenced Study Sequence"},
    {DCM_MAKETAG(0x3006, 0x0014), DCM_SQ, "SSET RT Referenced Series Sequence"},
    {DCM_MAKETAG(0x3006, 0x0016), DCM_SQ, "SSET Contour Image Sequence"},
    {DCM_MAKETAG(0x3006, 0x0020), DCM_SQ, "SSET Structure Set ROI Sequence"},
    {DCM_MAKETAG(0x3006, 0x0022), DCM_IS, "SSET ROI Number"},
    {DCM_MAKETAG(0x3006, 0x0024), DCM_UI, "SSET Referenced Frame of Reference UID"},
    {DCM_MAKETAG(0x3006, 0x0026), DCM_LO, "SSET ROI Name"},
    {DCM_MAKETAG(0x3006, 0x0028), DCM_ST, "SSET ROI Description"},
    {DCM_MAKETAG(0x3006, 0x002a), DCM_IS, "SSET ROI Display Color"},
    {DCM_MAKETAG(0x3006, 0x002c), DCM_DS, "SSET ROI Volume"},
    {DCM_MAKETAG(0x3006, 0x0030), DCM_SQ, "SSET RT Related ROI Sequence"},
    {DCM_MAKETAG(0x3006, 0x0033), DCM_CS, "SSET RT ROI Relationship"},
    {DCM_MAKETAG(0x3006, 0x0036), DCM_CS, "SSET ROI Generation Algorithm"},
    {DCM_MAKETAG(0x3006, 0x0038), DCM_LO, "SSET ROI Generation Description"},
    {DCM_MAKETAG(0x3006, 0x0039), DCM_SQ, "SSET ROI Contour Sequence"},
    {DCM_MAKETAG(0x3006, 0x0040), DCM_SQ, "SSET Contour Sequence"},
    {DCM_MAKETAG(0x3006, 0x0042), DCM_CS, "SSET Contour Geometric Type"},
    {DCM_MAKETAG(0x3006, 0x0044), DCM_DS, "SSET Contour Slab Thickness"},
    {DCM_MAKETAG(0x3006, 0x0045), DCM_DS, "SSET Contour Offset Vector"},
    {DCM_MAKETAG(0x3006, 0x0046), DCM_IS, "SSET Number of Contour Points"},
    {DCM_MAKETAG(0x3006, 0x0050), DCM_DS, "SSET Contour Data"},
    {DCM_MAKETAG(0x3006, 0x0080), DCM_SQ, "SSET RT ROI Observations Sequence"},
    {DCM_MAKETAG(0x3006, 0x0082), DCM_IS, "SSET Observation Number"},
    {DCM_MAKETAG(0x3006, 0x0084), DCM_IS, "SSET Referenced ROI Number"},
    {DCM_MAKETAG(0x3006, 0x0085), DCM_SH, "SSET ROI Observation Label"},
    {DCM_MAKETAG(0x3006, 0x0086), DCM_SQ, "SSET RT ROI Identification Code Sequence"},
    {DCM_MAKETAG(0x3006, 0x0088), DCM_ST, "SSET ROI Observation Description"},
    {DCM_MAKETAG(0x3006, 0x00a0), DCM_SQ, "SSET Relation RT ROI Observations Sequence"},
    {DCM_MAKETAG(0x3006, 0x00a4), DCM_CS, "SSET RT ROI Interpreted Type"},
    {DCM_MAKETAG(0x3006, 0x00a6), DCM_PN, "SSET ROI Interpreter"},
    {DCM_MAKETAG(0x3006, 0x00b0), DCM_SQ, "SSET ROI Physical Properties Sequence"},
    {DCM_MAKETAG(0x3006, 0x00b2), DCM_CS, "SSET ROI Physical Property"},
    {DCM_MAKETAG(0x3006, 0x00b4), DCM_DS, "SSET ROI Physical Property Value"},
    {DCM_MAKETAG(0x3006, 0x00c0), DCM_SQ, "SSET Frame of Referenced Relationship Sequence"},
    {DCM_MAKETAG(0x3006, 0x00c2), DCM_UI, "SSET Related Frame of Reference UID"},
    {DCM_MAKETAG(0x3006, 0x00c4), DCM_CS, "SSET Frame of Reference Transformation Type"},
    {DCM_MAKETAG(0x3006, 0x00c6), DCM_DS, "SSET Frame of Reference Transformation Matrix"},
    {DCM_MAKETAG(0x3006, 0x00c8), DCM_LO, "SSET Frame of Reference Transformation Comment"}
};

/* Define the entries in the 0x300A group, used in RT planning.
*/
static DCMDICT G300A_dictionary[] = {
    {DCM_MAKETAG(0x300a, 0x0000), DCM_UL, "     Group Length"},
    {DCM_MAKETAG(0x300a, 0x0002), DCM_SH, "     RT Plan Label"},
    {DCM_MAKETAG(0x300a, 0x0003), DCM_LO, "     RT Plan Name"},
    {DCM_MAKETAG(0x300a, 0x0004), DCM_ST, "     RT Plan Description"},
    {DCM_MAKETAG(0x300a, 0x0006), DCM_DA, "     RT Plan Date"},
    {DCM_MAKETAG(0x300a, 0x0007), DCM_TM, "     RT Plan Time"},
    {DCM_MAKETAG(0x300a, 0x0009), DCM_LO, "     RT Treatment Protocols"},
    {DCM_MAKETAG(0x300a, 0x000a), DCM_CS, "     Treatment Intent"},
    {DCM_MAKETAG(0x300a, 0x000b), DCM_LO, "     Treatment Sites"},
    {DCM_MAKETAG(0x300a, 0x000c), DCM_CS, "     RT Plan Geometry"},
    {DCM_MAKETAG(0x300a, 0x000e), DCM_ST, "     Prescription Description"},
    {DCM_MAKETAG(0x300a, 0x0010), DCM_SQ, "     Dose Reference Sequence"},
    {DCM_MAKETAG(0x300a, 0x0012), DCM_IS, "     Dose Reference Number"},
    {DCM_MAKETAG(0x300a, 0x0014), DCM_CS, "     Dose Reference Structure Type"},
    {DCM_MAKETAG(0x300a, 0x0016), DCM_LO, "     Dose Reference Description"},
    {DCM_MAKETAG(0x300a, 0x0018), DCM_DS, "     Dose Reference Point Coordinates"},
    {DCM_MAKETAG(0x300a, 0x001a), DCM_DS, "     Nominal Prior Dose"},
    {DCM_MAKETAG(0x300a, 0x0020), DCM_CS, "     Dose Reference Type"},
    {DCM_MAKETAG(0x300a, 0x0021), DCM_DS, "     Constraint Weight"},
    {DCM_MAKETAG(0x300a, 0x0022), DCM_DS, "     Delivery Warning Dose"},
    {DCM_MAKETAG(0x300a, 0x0023), DCM_DS, "     Delivery Maximum Dose"},
    {DCM_MAKETAG(0x300a, 0x0025), DCM_DS, "     Target Minimum Dose"},
    {DCM_MAKETAG(0x300a, 0x0026), DCM_DS, "     Target Prescription Dose"},
    {DCM_MAKETAG(0x300a, 0x0027), DCM_DS, "     Target Maximum Dose"},
    {DCM_MAKETAG(0x300a, 0x0028), DCM_DS, "     Target Underdose Volume Fraction"},
    {DCM_MAKETAG(0x300a, 0x002a), DCM_DS, "     Organ at Risk Full-volume Dose"},
    {DCM_MAKETAG(0x300a, 0x002b), DCM_DS, "     Organ at Risk Limit Dose"},
    {DCM_MAKETAG(0x300a, 0x002c), DCM_DS, "     Organ at Risk Maximum Dose"},
    {DCM_MAKETAG(0x300a, 0x002d), DCM_DS, "     Organ at Risk Overdose Volume Fraction"},
    {DCM_MAKETAG(0x300a, 0x0040), DCM_SQ, "     Tolerance Table Sequence"},
    {DCM_MAKETAG(0x300a, 0x0042), DCM_IS, "     Tolerance Table Number"},
    {DCM_MAKETAG(0x300a, 0x0043), DCM_SH, "     Tolerance Table Label"},
    {DCM_MAKETAG(0x300a, 0x0044), DCM_DS, "     Gantry Angle Tolerance"},
    {DCM_MAKETAG(0x300a, 0x0046), DCM_DS, "     Beam Limiting Device Angle Tolerance"},
    {DCM_MAKETAG(0x300a, 0x0048), DCM_SQ, "     Beam Limiting Device Tolerance Sequence"},
    {DCM_MAKETAG(0x300a, 0x004a), DCM_DS, "     Beam Limiting Device Position Tolerance"},
    {DCM_MAKETAG(0x300a, 0x004c), DCM_DS, "     Patient Support Angle Tolerance"},
    {DCM_MAKETAG(0x300a, 0x004e), DCM_DS, "     Table Top Eccentric Angle Tolerance"},
    {DCM_MAKETAG(0x300a, 0x0051), DCM_DS, "     Table Top Vertical Position Tolerance"},
    {DCM_MAKETAG(0x300a, 0x0052), DCM_DS, "     Table Top Longitudinal Position Tolerance"},
    {DCM_MAKETAG(0x300a, 0x0053), DCM_DS, "     Table Top Lateral Position Tolerance"},
    {DCM_MAKETAG(0x300a, 0x0055), DCM_CS, "     RT Plan Relationship"},
    {DCM_MAKETAG(0x300a, 0x0070), DCM_SQ, "     Fraction Group Sequence"},
    {DCM_MAKETAG(0x300a, 0x0071), DCM_IS, "     Fraction Group Number"},
    {DCM_MAKETAG(0x300a, 0x0078), DCM_IS, "     Number of Fractions Planned"},
    {DCM_MAKETAG(0x300a, 0x0079), DCM_IS, "     Number of Fractions Per Day"},
    {DCM_MAKETAG(0x300a, 0x007a), DCM_IS, "     Repeat Fraction Cycle Length"},
    {DCM_MAKETAG(0x300a, 0x007b), DCM_LT, "     Fraction Pattern"},
    {DCM_MAKETAG(0x300a, 0x0080), DCM_IS, "     Number of Beams"},
    {DCM_MAKETAG(0x300a, 0x0082), DCM_DS, "     Beam Dose Specification Point"},
    {DCM_MAKETAG(0x300a, 0x0084), DCM_DS, "     Beam Dose"},
    {DCM_MAKETAG(0x300a, 0x0086), DCM_DS, "     Beam Meterset"},
    {DCM_MAKETAG(0x300a, 0x00a0), DCM_IS, "     Number of Brachy Application Setups"},
    {DCM_MAKETAG(0x300a, 0x00a2), DCM_DS, "     Brachy App Setup Dose Specification Point"},
    {DCM_MAKETAG(0x300a, 0x00a4), DCM_DS, "     Brachy Application Setup Dose"},
    {DCM_MAKETAG(0x300a, 0x00b0), DCM_SQ, "     Beam Sequence"},
    {DCM_MAKETAG(0x300a, 0x00b2), DCM_SH, "     Treatment Machine Name"},
    {DCM_MAKETAG(0x300a, 0x00b3), DCM_CS, "     Primary Dosimeter Unit"},
    {DCM_MAKETAG(0x300a, 0x00b4), DCM_DS, "     Source-Axis Distance"},
    {DCM_MAKETAG(0x300a, 0x00b6), DCM_SQ, "     Beam Limiting Device Sequence"},
    {DCM_MAKETAG(0x300a, 0x00b8), DCM_CS, "     RT Beam Limiting Device Type"},
    {DCM_MAKETAG(0x300a, 0x00ba), DCM_DS, "     Source to Beam Limiting Device Distance"},
    {DCM_MAKETAG(0x300a, 0x00bc), DCM_IS, "     Number of Leaf/Jaw Pairs"},
    {DCM_MAKETAG(0x300a, 0x00be), DCM_DS, "     Leaf Position Boundaries"},
    {DCM_MAKETAG(0x300a, 0x00c0), DCM_IS, "     Beam Number"},
    {DCM_MAKETAG(0x300a, 0x00c2), DCM_LO, "     Beam Name"},
    {DCM_MAKETAG(0x300a, 0x00c3), DCM_ST, "     Beam Description"},
    {DCM_MAKETAG(0x300a, 0x00c4), DCM_CS, "     Beam Type"},
    {DCM_MAKETAG(0x300a, 0x00c6), DCM_CS, "     Radiation Type"},
    {DCM_MAKETAG(0x300a, 0x00c8), DCM_IS, "     Reference Image Number"},
    {DCM_MAKETAG(0x300a, 0x00ca), DCM_SQ, "     Planned Verification Image Sequence"},
    {DCM_MAKETAG(0x300a, 0x00cc), DCM_LO, "     Imaging Device-Specific Acq Parameters"},
    {DCM_MAKETAG(0x300a, 0x00ce), DCM_CS, "     Treatment Delivery Type"},
    {DCM_MAKETAG(0x300a, 0x00d0), DCM_IS, "     Number of Wedges"},
    {DCM_MAKETAG(0x300a, 0x00d1), DCM_SQ, "     Wedge Sequence"},
    {DCM_MAKETAG(0x300a, 0x00d2), DCM_IS, "     Wedge Number"},
    {DCM_MAKETAG(0x300a, 0x00d3), DCM_CS, "     Wedge Type"},
    {DCM_MAKETAG(0x300a, 0x00d4), DCM_SH, "     Wedge ID"},
    {DCM_MAKETAG(0x300a, 0x00d5), DCM_IS, "     Wedge Angle"},
    {DCM_MAKETAG(0x300a, 0x00d6), DCM_DS, "     Wedge Factor"},
    {DCM_MAKETAG(0x300a, 0x00d8), DCM_DS, "     Wedge Orientation"},
    {DCM_MAKETAG(0x300a, 0x00da), DCM_DS, "     Source to Wedge Tray Distance"},
    {DCM_MAKETAG(0x300a, 0x00e0), DCM_IS, "     Number of Compensators"},
    {DCM_MAKETAG(0x300a, 0x00e1), DCM_SH, "     Material ID"},
    {DCM_MAKETAG(0x300a, 0x00e2), DCM_DS, "     Total Compensator Tray Factor"},
    {DCM_MAKETAG(0x300a, 0x00e3), DCM_SQ, "     Compensator Sequence"},
    {DCM_MAKETAG(0x300a, 0x00e4), DCM_IS, "     Compensator Number"},
    {DCM_MAKETAG(0x300a, 0x00e5), DCM_SH, "     Compensator ID"},
    {DCM_MAKETAG(0x300a, 0x00e6), DCM_DS, "     Source to Compensator Tray Distance"},
    {DCM_MAKETAG(0x300a, 0x00e7), DCM_IS, "     Compensator Rows"},
    {DCM_MAKETAG(0x300a, 0x00e8), DCM_IS, "     Compensator Columns"},
    {DCM_MAKETAG(0x300a, 0x00e9), DCM_DS, "     Compensator Pixel Spacing"},
    {DCM_MAKETAG(0x300a, 0x00ea), DCM_DS, "     Compensator Position"},
    {DCM_MAKETAG(0x300a, 0x00eb), DCM_DS, "     Compensator Transmission Data"},
    {DCM_MAKETAG(0x300a, 0x00ec), DCM_DS, "     Compensator Thickness Data"},
    {DCM_MAKETAG(0x300a, 0x00ed), DCM_IS, "     Number of Boli"},
    {DCM_MAKETAG(0x300a, 0x00f0), DCM_IS, "     Number of Blocks"},
    {DCM_MAKETAG(0x300a, 0x00f2), DCM_DS, "     Total Block Tray Factor"},
    {DCM_MAKETAG(0x300a, 0x00f4), DCM_SQ, "     Block Sequence"},
    {DCM_MAKETAG(0x300a, 0x00f5), DCM_SH, "     Block Tray ID"},
    {DCM_MAKETAG(0x300a, 0x00f6), DCM_DS, "     Source to Block Tray Distance"},
    {DCM_MAKETAG(0x300a, 0x00f8), DCM_CS, "     Block Type"},
    {DCM_MAKETAG(0x300a, 0x00fa), DCM_CS, "     Block Divergence"},
    {DCM_MAKETAG(0x300a, 0x00fc), DCM_IS, "     Block Number"},
    {DCM_MAKETAG(0x300a, 0x00fe), DCM_LO, "     Block Name"},
    {DCM_MAKETAG(0x300a, 0x0100), DCM_DS, "     Block Thickness"},
    {DCM_MAKETAG(0x300a, 0x0102), DCM_DS, "     Block Transmission"},
    {DCM_MAKETAG(0x300a, 0x0104), DCM_IS, "     Block Number of Points"},
    {DCM_MAKETAG(0x300a, 0x0106), DCM_DS, "     Block Data"},
    {DCM_MAKETAG(0x300a, 0x0107), DCM_SQ, "     Applicator Sequence"},
    {DCM_MAKETAG(0x300a, 0x0108), DCM_SH, "     Applicator ID"},
    {DCM_MAKETAG(0x300a, 0x0109), DCM_CS, "     Applicator Type"},
    {DCM_MAKETAG(0x300a, 0x010a), DCM_LO, "     Applicator Description"},
    {DCM_MAKETAG(0x300a, 0x010c), DCM_DS, "     Cumulative Dose Reference COefficient"},
    {DCM_MAKETAG(0x300a, 0x010e), DCM_DS, "     Final Cumulative Meterset Weight"},
    {DCM_MAKETAG(0x300a, 0x0110), DCM_IS, "     Number of Control Points"},
    {DCM_MAKETAG(0x300a, 0x0111), DCM_SQ, "     Control Point Sequence"},
    {DCM_MAKETAG(0x300a, 0x0112), DCM_IS, "     Control Point Index"},
    {DCM_MAKETAG(0x300a, 0x0114), DCM_DS, "     Nominal Beam Energy"},
    {DCM_MAKETAG(0x300a, 0x0115), DCM_DS, "     Dose Rate Set"},
    {DCM_MAKETAG(0x300a, 0x0116), DCM_SQ, "     Wedge Position Sequence"},
    {DCM_MAKETAG(0x300a, 0x0118), DCM_CS, "     Wedge Position"},
    {DCM_MAKETAG(0x300a, 0x011a), DCM_SQ, "     Beam Limiting Device Position Sequence"},
    {DCM_MAKETAG(0x300a, 0x011c), DCM_DS, "     Leaf/Jaw Positions"},
    {DCM_MAKETAG(0x300a, 0x011e), DCM_DS, "     Gantry Angle"},
    {DCM_MAKETAG(0x300a, 0x011f), DCM_CS, "     Gantry Rotation Direction"},
    {DCM_MAKETAG(0x300a, 0x0120), DCM_DS, "     Beam Limiting Device Angle"},
    {DCM_MAKETAG(0x300a, 0x0121), DCM_CS, "     Beam Limiting Device Rotation Direction"},
    {DCM_MAKETAG(0x300a, 0x0122), DCM_DS, "     Patient Support Angle"},
    {DCM_MAKETAG(0x300a, 0x0123), DCM_CS, "     Patient Support Rotation Direction"},
    {DCM_MAKETAG(0x300a, 0x0124), DCM_DS, "     Table Top Eccentric Axis Distance"},
    {DCM_MAKETAG(0x300a, 0x0125), DCM_DS, "     Table Top Eccentric Angle"},
    {DCM_MAKETAG(0x300a, 0x0126), DCM_CS, "     Table Top Eccentric Rotation Direction"},
    {DCM_MAKETAG(0x300a, 0x0128), DCM_DS, "     Table Top Vertical Position"},
    {DCM_MAKETAG(0x300a, 0x0129), DCM_DS, "     Table Top Longitudinal Position"},
    {DCM_MAKETAG(0x300a, 0x012a), DCM_DS, "     Table Top Lateral Position"},
    {DCM_MAKETAG(0x300a, 0x012c), DCM_DS, "     Isocenter Position"},
    {DCM_MAKETAG(0x300a, 0x012e), DCM_DS, "     Surface Entry Point"},
    {DCM_MAKETAG(0x300a, 0x0130), DCM_DS, "     Source to Surface Distance"},
    {DCM_MAKETAG(0x300a, 0x0134), DCM_DS, "     Cumulative Meterset Weight"},
    {DCM_MAKETAG(0x300a, 0x0180), DCM_SQ, "     Patient Setup Sequence"},
    {DCM_MAKETAG(0x300a, 0x0182), DCM_IS, "     Patient Setup Number"},
    {DCM_MAKETAG(0x300a, 0x0184), DCM_LO, "     Patient Additional Position"},
    {DCM_MAKETAG(0x300a, 0x0190), DCM_SQ, "     Fixation Device Sequence"},
    {DCM_MAKETAG(0x300a, 0x0192), DCM_CS, "     Fixation Device Type"},
    {DCM_MAKETAG(0x300a, 0x0194), DCM_SH, "     Fixation Device Label"},
    {DCM_MAKETAG(0x300a, 0x0196), DCM_ST, "     Fixation Device Description"},
    {DCM_MAKETAG(0x300a, 0x0198), DCM_SH, "     Fixation Device Position"},
    {DCM_MAKETAG(0x300a, 0x01a0), DCM_SQ, "     Shielding Device Sequence"},
    {DCM_MAKETAG(0x300a, 0x01a2), DCM_CS, "     Shielding Device Type"},
    {DCM_MAKETAG(0x300a, 0x01a4), DCM_SH, "     Shielding Device Label"},
    {DCM_MAKETAG(0x300a, 0x01a6), DCM_ST, "     Shielding Device Description"},
    {DCM_MAKETAG(0x300a, 0x01a8), DCM_SH, "     Shielding Device Position"},
    {DCM_MAKETAG(0x300a, 0x01b0), DCM_CS, "     Setup Technique"},
    {DCM_MAKETAG(0x300a, 0x01b2), DCM_ST, "     Setup Technique Description"},
    {DCM_MAKETAG(0x300a, 0x01b4), DCM_SQ, "     Setup Device Sequence"},
    {DCM_MAKETAG(0x300a, 0x01b6), DCM_CS, "     Setup Device Type"},
    {DCM_MAKETAG(0x300a, 0x01b8), DCM_SH, "     Setup Device Label"},
    {DCM_MAKETAG(0x300a, 0x01ba), DCM_ST, "     Setup Device Description"},
    {DCM_MAKETAG(0x300a, 0x01bc), DCM_DS, "     Setup Device Parameter"},
    {DCM_MAKETAG(0x300a, 0x01d0), DCM_ST, "     Setup Reference Description"},
    {DCM_MAKETAG(0x300a, 0x01d2), DCM_DS, "     Table Top Vertical Setup Displacement"},
    {DCM_MAKETAG(0x300a, 0x01d4), DCM_DS, "     Table Top Longitudinal Setup Displacement"},
    {DCM_MAKETAG(0x300a, 0x01d6), DCM_DS, "     Table Top Lateral Setup Displacement"},
    {DCM_MAKETAG(0x300a, 0x0200), DCM_CS, "     Brachy Treatment Technique"},
    {DCM_MAKETAG(0x300a, 0x0202), DCM_CS, "     Brachy Treatment Type"},
    {DCM_MAKETAG(0x300a, 0x0206), DCM_SQ, "     Treatment Machine Sequence"},
    {DCM_MAKETAG(0x300a, 0x0210), DCM_SQ, "     Source Sequence"},
    {DCM_MAKETAG(0x300a, 0x0212), DCM_IS, "     Source Number"},
    {DCM_MAKETAG(0x300a, 0x0214), DCM_CS, "     Source Type"},
    {DCM_MAKETAG(0x300a, 0x0216), DCM_LO, "     Source Manufacturer"},
    {DCM_MAKETAG(0x300a, 0x0218), DCM_DS, "     Active Source Diameter"},
    {DCM_MAKETAG(0x300a, 0x021a), DCM_DS, "     Active Source Length"},
    {DCM_MAKETAG(0x300a, 0x0222), DCM_DS, "     Source Encapsulation Nominal Thickness"},
    {DCM_MAKETAG(0x300a, 0x0224), DCM_DS, "     Source Encapsulation Nominal Transmission"},
    {DCM_MAKETAG(0x300a, 0x0226), DCM_LO, "     Source Isotope Name"},
    {DCM_MAKETAG(0x300a, 0x0228), DCM_DS, "     Source Isotope Half Life"},
    {DCM_MAKETAG(0x300a, 0x022a), DCM_DS, "     Reference Air Kerma Rate"},
    {DCM_MAKETAG(0x300a, 0x022c), DCM_DA, "     Air Kerma Rate Reference Date"},
    {DCM_MAKETAG(0x300a, 0x022e), DCM_TM, "     Air Kerma Rate Reference Time"},
    {DCM_MAKETAG(0x300a, 0x0230), DCM_SQ, "     Application Setup Sequence"},
    {DCM_MAKETAG(0x300a, 0x0232), DCM_CS, "     Application Setup Type"},
    {DCM_MAKETAG(0x300a, 0x0234), DCM_IS, "     Application Setup Number"},
    {DCM_MAKETAG(0x300a, 0x0236), DCM_LO, "     Application Setup Name"},
    {DCM_MAKETAG(0x300a, 0x0238), DCM_LO, "     Application Setup Manufacturer"},
    {DCM_MAKETAG(0x300a, 0x0240), DCM_IS, "     Template Number"},
    {DCM_MAKETAG(0x300a, 0x0242), DCM_SH, "     Template Type"},
    {DCM_MAKETAG(0x300a, 0x0244), DCM_LO, "     Template Name"},
    {DCM_MAKETAG(0x300a, 0x0250), DCM_DS, "     Total Reference Air Kerma"},
    {DCM_MAKETAG(0x300a, 0x0260), DCM_SQ, "     Brachy Acessory Device Sequence"},
    {DCM_MAKETAG(0x300a, 0x0262), DCM_IS, "     Brachy Accessory Device Number"},
    {DCM_MAKETAG(0x300a, 0x0263), DCM_SH, "     Brachy Accessory Device ID"},
    {DCM_MAKETAG(0x300a, 0x0264), DCM_CS, "     Brachy Accessory Device Type"},
    {DCM_MAKETAG(0x300a, 0x0266), DCM_LO, "     Brachy Accessory Device Name"},
    {DCM_MAKETAG(0x300a, 0x026a), DCM_DS, "     Brachy Accessory Device Nominal Thickness"},
    {DCM_MAKETAG(0x300a, 0x026c), DCM_DS, "     Brachy Acc'ry Device Nominal Transmission"},
    {DCM_MAKETAG(0x300a, 0x0280), DCM_SQ, "     Channel Sequence"},
    {DCM_MAKETAG(0x300a, 0x0282), DCM_IS, "     Channel Number"},
    {DCM_MAKETAG(0x300a, 0x0284), DCM_DS, "     Channel Length"},
    {DCM_MAKETAG(0x300a, 0x0286), DCM_DS, "     Channel Total Time"},
    {DCM_MAKETAG(0x300a, 0x0288), DCM_CS, "     Source Movement Type"},
    {DCM_MAKETAG(0x300a, 0x028a), DCM_IS, "     Number of Pulses"},
    {DCM_MAKETAG(0x300a, 0x028c), DCM_DS, "     Pulse Repetition Interval"},
    {DCM_MAKETAG(0x300a, 0x0290), DCM_IS, "     Source Applicator Number"},
    {DCM_MAKETAG(0x300a, 0x0291), DCM_SH, "     Source Applicator ID"},
    {DCM_MAKETAG(0x300a, 0x0292), DCM_CS, "     Source Applicator Type"},
    {DCM_MAKETAG(0x300a, 0x0294), DCM_LO, "     Source Applicator Name"},
    {DCM_MAKETAG(0x300a, 0x0296), DCM_DS, "     Source Applicator Length"},
    {DCM_MAKETAG(0x300a, 0x0298), DCM_LO, "     Source Applicator Manufacturer"},
    {DCM_MAKETAG(0x300a, 0x029c), DCM_DS, "     Source Applicator Wall Nominal Thickness"},
    {DCM_MAKETAG(0x300a, 0x029e), DCM_DS, "     Src Applicator Wall Nominal Transmission"},
    {DCM_MAKETAG(0x300a, 0x02a0), DCM_DS, "     Source Applicator Step Size"},
    {DCM_MAKETAG(0x300a, 0x02a2), DCM_IS, "     Transfer Tube Number"},
    {DCM_MAKETAG(0x300a, 0x02a4), DCM_DS, "     Transfer Tube Length"},
    {DCM_MAKETAG(0x300a, 0x02b0), DCM_SQ, "     Channel Shield Sequence"},
    {DCM_MAKETAG(0x300a, 0x02b2), DCM_IS, "     Channel Shield Number"},
    {DCM_MAKETAG(0x300a, 0x02b3), DCM_SH, "     Channel Shield ID"},
    {DCM_MAKETAG(0x300a, 0x02b4), DCM_LO, "     Channel Shield Name"},
    {DCM_MAKETAG(0x300a, 0x02b8), DCM_DS, "     Channel Shield Nominal Thickness"},
    {DCM_MAKETAG(0x300a, 0x02ba), DCM_DS, "     Channel Shield Nominal Transmission"},
    {DCM_MAKETAG(0x300a, 0x02c8), DCM_DS, "     Final Cumulative Time Weight"},
    {DCM_MAKETAG(0x300a, 0x02d0), DCM_SQ, "     Brachy Control Point Sequence"},
    {DCM_MAKETAG(0x300a, 0x02d2), DCM_DS, "   Control Point Relative Position"},
    {DCM_MAKETAG(0x300a, 0x02d4), DCM_DS, "     Control Point 3D Position"},
    {DCM_MAKETAG(0x300a, 0x02d6), DCM_DS, "     Cumulative Time Weight"}
};

/* Define the entries in the 0x300C group, used in RT planning.
*/
static DCMDICT G300C_dictionary[] = {
    {DCM_MAKETAG(0x300c, 0x0000), DCM_UL, "     Group Length"},
    {DCM_MAKETAG(0x300c, 0x0002), DCM_SQ, "     Referenced RT Plan Sequence"},
    {DCM_MAKETAG(0x300c, 0x0004), DCM_SQ, "     Referenced Beam Sequence"},
    {DCM_MAKETAG(0x300c, 0x0006), DCM_IS, "     Referenced Beam Number"},
    {DCM_MAKETAG(0x300c, 0x0007), DCM_IS, "     Referenced Reference Image Number"},
    {DCM_MAKETAG(0x300c, 0x0008), DCM_DS, "     Start Cumulative Meterset Weight"},
    {DCM_MAKETAG(0x300c, 0x0009), DCM_DS, "     End Cumulative Meterset Weight"},
    {DCM_MAKETAG(0x300c, 0x000a), DCM_SQ, "     Referenced Brachy Application Setup Seq"},
    {DCM_MAKETAG(0x300c, 0x000c), DCM_IS, "     Referenced Brachy Application Setup Number"},
    {DCM_MAKETAG(0x300c, 0x000e), DCM_IS, "     Referenced Source Number"},
    {DCM_MAKETAG(0x300c, 0x0020), DCM_SQ, "     Referenced Fraction Group Sequence"},
    {DCM_MAKETAG(0x300c, 0x0022), DCM_IS, "     Referenced Fraction Group Number"},
    {DCM_MAKETAG(0x300c, 0x0040), DCM_SQ, "     Referenced Verification Image Sequence"},
    {DCM_MAKETAG(0x300c, 0x0042), DCM_SQ, "     Referenced Reference Image Sequence"},
    {DCM_MAKETAG(0x300c, 0x0050), DCM_SQ, "     Referenced Dose Reference Sequence"},
    {DCM_MAKETAG(0x300c, 0x0051), DCM_IS, "     Referenced Dose Reference Numer"},
    {DCM_MAKETAG(0x300c, 0x0055), DCM_SQ, "     Brachy Referenced Dose Reference Sequence"},
    {DCM_MAKETAG(0x300c, 0x0060), DCM_SQ, "     Referenced Structure Set Sequence"},
    {DCM_MAKETAG(0x300c, 0x006a), DCM_IS, "     Referenced Patient Setup Number"},
    {DCM_MAKETAG(0x300c, 0x0080), DCM_SQ, "     Referenced Dose Sequence"},
    {DCM_MAKETAG(0x300c, 0x00a0), DCM_IS, "     Referenced Tolerance Table Number"},
    {DCM_MAKETAG(0x300c, 0x00b0), DCM_SQ, "     Referenced Bolus Sequence"},
    {DCM_MAKETAG(0x300c, 0x00c0), DCM_IS, "     Referenced Wedge Number"},
    {DCM_MAKETAG(0x300c, 0x00d0), DCM_IS, "     Referenced Compensator Number"},
    {DCM_MAKETAG(0x300c, 0x00e0), DCM_IS, "     Referenced Block Number"},
    {DCM_MAKETAG(0x300c, 0x00f0), DCM_IS, "     Referenced Control Point Index"}
};


/* Define the entries in the 0x300E group, used in RT planning.
*/
static DCMDICT G300E_dictionary[] = {
    {DCM_MAKETAG(0x300e, 0x0000), DCM_UL, "     Group Length"},
    {DCM_MAKETAG(0x300e, 0x0002), DCM_CS, "     Approval Status"},
    {DCM_MAKETAG(0x300e, 0x0004), DCM_DA, "     Review Date"},
    {DCM_MAKETAG(0x300e, 0x0005), DCM_TM, "     Review Time"},
    {DCM_MAKETAG(0x300e, 0x0008), DCM_PN, "     Reviewer Name"}
};

/* Defines the entries in the Text group (4000)
*/
#if 0
static DCMDICT TXT_dictionary[] = {
};
#endif

/* Define the entries in the PAD group, 0xfffc
*/

static DCMDICT PAD_dictionary[] = {
    {DCM_PADITEM, DCM_OB, "Pad item"}
};

/* Define the entries in the DELIMITER group, 0xfffe
*/

static DCMDICT DLM_dictionary[] = {
    {DCM_DLMITEM, DCM_DLM, "DELIMITER Item"},
    {DCM_DLMITEMDELIMITATIONITEM, DCM_DLM, "DELIMITER Item Delimitation Item"},
    {DCM_DLMSEQUENCEDELIMITATIONITEM, DCM_DLM, "DELIMITER Sequence Delimitation Item"}
};

/* Define the outer layer dictionary which contains group numbers and
** pointers to each of the individual group lists.
*/

static GROUPPTR group_dictionary[] = {
    {DCM_GROUPCOMMAND, sizeof(CMD_dictionary) / sizeof(DCMDICT), CMD_dictionary},
    {DCM_GROUPFILEMETA, sizeof(META_dictionary) / sizeof(DCMDICT), META_dictionary},
    {DCM_GROUPBASICDIRINFO, sizeof(BASICDIR_dictionary) / sizeof(DCMDICT), BASICDIR_dictionary},
    {DCM_GROUPIDENTIFYING,
    sizeof(ID_dictionary) / sizeof(DCMDICT), ID_dictionary},
    {DCM_GROUPPATIENTINFO,
    sizeof(PAT_dictionary) / sizeof(DCMDICT), PAT_dictionary},
    {DCM_GROUPACQUISITION,
    sizeof(ACQ_dictionary) / sizeof(DCMDICT), ACQ_dictionary},
    {DCM_GROUPRELATIONSHIP,
    sizeof(REL_dictionary) / sizeof(DCMDICT), REL_dictionary},
    {DCM_GROUPIMAGE,
    sizeof(IMG_dictionary) / sizeof(DCMDICT), IMG_dictionary},
    {DCM_GROUPSTUDY,
    sizeof(SDY_dictionary) / sizeof(DCMDICT), SDY_dictionary},
    {DCM_GROUPVISIT,
    sizeof(VIS_dictionary) / sizeof(DCMDICT), VIS_dictionary},
    {DCM_GROUPWAVEFORM,
    sizeof(WAV_dictionary) / sizeof(DCMDICT), WAV_dictionary},
    {DCM_GRPPROCEDURE,
    sizeof(PRC_dictionary) / sizeof(DCMDICT), PRC_dictionary},
    {DCM_GROUPDEVICE,
    sizeof(DEV_dictionary) / sizeof(DCMDICT), DEV_dictionary},
    {DCM_GROUPNMIMAGE,
    sizeof(NMI_dictionary) / sizeof(DCMDICT), NMI_dictionary},
    {DCM_GROUPGRAPHICS,
    sizeof(GRP_dictionary) / sizeof(DCMDICT), GRP_dictionary},
    {DCM_GROUPMEDIA,
    sizeof(MED_dictionary) / sizeof(DCMDICT), MED_dictionary},
    {DCM_GROUPBASICFILMSESSION,
    sizeof(BFS_dictionary) / sizeof(DCMDICT), BFS_dictionary},
    {DCM_GROUPBASICFILMBOX,
    sizeof(BFB_dictionary) / sizeof(DCMDICT), BFB_dictionary},
    {DCM_GROUPBASICIMAGEBOX,
    sizeof(BIB_dictionary) / sizeof(DCMDICT), BIB_dictionary},
    {DCM_GROUPBASICANNOTATIONBOX,
    sizeof(BAB_dictionary) / sizeof(DCMDICT), BAB_dictionary},

    {DCM_GROUPBASICIMAGEOVERLAYBOX,
    sizeof(IOB_dictionary) / sizeof(DCMDICT), IOB_dictionary},

    {0x2050,
    sizeof(PLUT_dictionary) / sizeof(DCMDICT), PLUT_dictionary},

    {DCM_GROUPPRINTJOB,
    sizeof(PJ_dictionary) / sizeof(DCMDICT), PJ_dictionary},

    {DCM_GROUPPRINTER,
    sizeof(PRN_dictionary) / sizeof(DCMDICT), PRN_dictionary},
    {0x3002,
    sizeof(G3002_dictionary) / sizeof(DCMDICT), G3002_dictionary},
    {0x3004,
    sizeof(DVH_dictionary) / sizeof(DCMDICT), DVH_dictionary},
    {0x3006,
    sizeof(SSET_dictionary) / sizeof(DCMDICT), SSET_dictionary},
    {0x300a,
    sizeof(G300A_dictionary) / sizeof(DCMDICT), G300A_dictionary},
    {0x300c,
    sizeof(G300C_dictionary) / sizeof(DCMDICT), G300C_dictionary},
    {0x300e,
    sizeof(G300E_dictionary) / sizeof(DCMDICT), G300E_dictionary},

/*  Add this entry in when we define retired attributes
**  in text group.
*/
#if 0
    {DCM_GROUPTEXT,
    sizeof(TXT_dictionary) / sizeof(DCMDICT), TXT_dictionary},
#endif
    {DCM_GROUPRESULTS,
    sizeof(RES_dictionary) / sizeof(DCMDICT), RES_dictionary},
    {DCM_GROUPCURVE,
    sizeof(CRV_dictionary) / sizeof(DCMDICT), CRV_dictionary},
    {DCM_GROUPOVERLAY,
    sizeof(OLY_dictionary) / sizeof(DCMDICT), OLY_dictionary},
    {DCM_GROUPPIXEL,
    sizeof(PXL_dictionary) / sizeof(DCMDICT), PXL_dictionary},
    {DCM_GROUPPAD,
    sizeof(PAD_dictionary) / sizeof(DCMDICT), PAD_dictionary},
    {DCM_GROUPDELIMITER,
    sizeof(DLM_dictionary) / sizeof(DCMDICT), DLM_dictionary}
};


/* DCM_LookupElement
**
** Purpose:
**	Lookup an element in the DICOM dictionary and return information
**	about the element, including representation, type and english
**	description.
**
** Parameter Dictionary:
**	element		Pointer to an DCM element (group, element) to
**			be found in the dictionary.
**
** Return Values:
**	DCM_NORMAL
**	DCM_UNRECOGNIZEDGROUP
**	DCM_UNRECOGNIZEDELEMENT
**
** Algorithm:
**	Set representation, type, englishDescription fields of caller's
**	element to NULL values
**	Search group_dictionary to find caller's group.
**	If group not found,
**	    return DCM_UNRECOGNIZEDGROUP
**	Search particular group list to find caller's element.
**	If element not found,
**	    return DCM_UNRECOGNIZEDELEMENT
**	Else
**	    Copy representation, type, englishDescription from dictionary
**	    to caller's element
**	    return DCM_NORMAL
**	EndIf
*/

CONDITION
DCM_LookupElement(DCM_ELEMENT * element)
{
    int
        found;
    unsigned long
        index,
        entries;
    GROUPPTR
	* p;
    DCMDICT
	* dictionaryPtr;

    element->representation = DCM_UN;
    (void) strcpy(element->description, "");

    for (index = 0, p = NULL;
	 index < sizeof(group_dictionary) / sizeof(group_dictionary[0]) && p == NULL;
	 index++)
	if (DCM_TAG_GROUP(element->tag) == group_dictionary[index].group)
	    p = &group_dictionary[index];

    if (p == NULL) {
	if (DCM_TAG_ELEMENT(element->tag) == 0x0000) {
	    element->representation = DCM_UL;
	    (void) strcpy(element->description, "Unknown group length");
	    return DCM_NORMAL;
	}
	return COND_PushCondition(DCM_UNRECOGNIZEDGROUP,
				  DCM_Message(DCM_UNRECOGNIZEDGROUP),
				  DCM_TAG_GROUP(element->tag),
				  "DCM_LookupElement");
    }
    entries = p->entries;
    dictionaryPtr = p->dict;

    for (found = 0; !found && entries > 0; entries--)
	if (element->tag == dictionaryPtr->tag)
	    found++;
	else
	    dictionaryPtr++;

    if (!found)
	return COND_PushCondition(DCM_UNRECOGNIZEDELEMENT,
				  DCM_Message(DCM_UNRECOGNIZEDELEMENT),
				  DCM_TAG_GROUP(element->tag),
				  DCM_TAG_ELEMENT(element->tag),
				  "DCM_LookupElement");


    element->representation = dictionaryPtr->representation;
    (void) strcpy(element->description, dictionaryPtr->englishDescription);
    return DCM_NORMAL;
}

typedef struct {
    unsigned short group;
    char *description;
}   GROUP_DESCRIPTION;

static GROUP_DESCRIPTION groupTable[] = {
    {0x0000, "Command"},
    {0x0002, "File Meta"},
    {0x0004, "Basic Directory Information"},
    {0x0008, "Identifying"},
    {0x0010, "Patient Information"},
    {0x0018, "Acquisition"},
    {0x0020, "Relationship"},
    {0x0028, "Image"},
    {0x0032, "Study"},
    {0x0038, "Visit"},
    {0x003a, "Waveform"},
    {0x0040, "Procedure Step"},
    {0x0050, "Device"},
    {0x0054, "NM Image"},
    {0x0070, "Graphics"},
    {0x0088, "Media"},
    {0x2000, "Basic Film Session"},
    {0x2010, "Basic Film Box"},
    {0x2020, "Basic Image Box"},
    {0x2030, "Basic Annotation Box"},
    {0x2040, "Basic Image Overlay Box"},
    {0x2050, "Presentation LUT"},
    {0x2100, "Print Job"},
    {0x2110, "Printer"},
    {0x3002, "RT"},
    {0x3004, "Dose Volume Histogram"},
    {0x3006, "Structure Set"},
    {0x300a, "300a"},
    {0x300c, "300c"},
    {0x300e, "300e"},
#if 0
    {0x4000, "Text"},
#endif
    {0x4008, "Results"},
    {0x5000, "Curve"},
    {0x6000, "Overlay"},
    {0x7fe0, "Pixel"}
};


/* DCM_GroupDictionary
**
** Purpose:
**	DCM_GroupDictionary is used to lookup descriptions of groups in
**	the internal DCM group dictionary.  Caller specifies one group
**	with a group number or all groups by passing 0xffff.  For each
**	group that matches (the one group or wildcard), this function
**	invokes the caller's callback function.
**	When the callback function is invoked, the arguments are the
**	group number, an ASCII description of the group and user context
**	information that was passed by the caller originally.
**
** Parameter Dictionary:
**	group		The number of the group to be found in the dictionary.
**	ctx		User context information to be passed to callback
**			function.
**	callback	The user's callback function, invoked once for each
**			group that is found during the dictionary lookup.
**
** Return Values:
**	DCM_NORMAL
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

CONDITION
DCM_GroupDictionary(unsigned short group, void *ctx,
	  void (*callback) (unsigned short g, char *description, void *ctx))
{
    int i;

    for (i = 0; i < (int) DIM_OF(groupTable); i++) {
	if ((group == 0xffff) || (group == groupTable[i].group)) {
	    callback(groupTable[i].group, groupTable[i].description, ctx);
	}
    }
    return DCM_NORMAL;
}

/* DCM_ElementDictionary
**
** Purpose:
**	DCM_ElementDictionary is used to lookup descriptions of elements in
**	the internal DCM element dictionary.  The caller can specify one
**	element to be found or a number of elements as follows:
**		(Group,  Element)	Description
**		GGGG,    EEEE		Lookup one particular element (GGGGEEEE)
**		GGGG,    0xffff		Lookup all elements in group GGGG
**		0xffff,  EEEE		Lookup all elements in all groups with
**					element number EEEE
**		0xffff,  0xffff		Lookup all elements in all groups
**	For each element that matches (the one element or wildcard), this
**	function invokes the caller's callback function.
**	When the callback function is invoked, the arguments are the
**	element tag, an ASCII description of the element, the element value
**	representation and user context information that was passed by
**	the caller originally.
**
** Parameter Dictionary:
**	tag		The tag of the element to be found in the dictionary.
**	ctx		User context information to be passed to callback
**			function.
**	callback	The user's callback function, invoked once for each
**			element that is found during the dictionary lookup.
**
** Return Values:
**	DCM_NORMAL
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

CONDITION
DCM_ElementDictionary(DCM_TAG tag, void *ctx,
  void (*callback) (DCM_TAG t, char *description, DCM_VALUEREPRESENTATION r,
		    void *ctx))
{
    int i;
    unsigned long j;
    GROUPPTR *p;
    DCMDICT *dictionaryPtr;

    for (i = 0; i < (int) DIM_OF(group_dictionary); i++) {
	if ((DCM_TAG_GROUP(tag) == group_dictionary[i].group) ||
	    (DCM_TAG_GROUP(tag) == 0xffff)) {
	    p = &group_dictionary[i];
	    dictionaryPtr = p->dict;
	    for (j = 0; j < p->entries; j++, dictionaryPtr++) {
		if ((DCM_TAG_ELEMENT(tag) == 0xffff) ||
		    (DCM_TAG_ELEMENT(tag) == DCM_TAG_ELEMENT(dictionaryPtr->tag))) {
		    callback(dictionaryPtr->tag,
			     dictionaryPtr->englishDescription,
			     dictionaryPtr->representation,
			     ctx);
		}
	    }
	}
    }
    return DCM_NORMAL;
}
/*
          Copyright (C) 1993, 1994, RSNA and Washington University

          The software and supporting documentation for the Radiological
          Society of North America (RSNA) 1993, 1994 Digital Imaging and
          Communications in Medicine (DICOM) Demonstration were developed
          at the
                  Electronic Radiology Laboratory
                  Mallinckrodt Institute of Radiology
                  Washington University School of Medicine
                  510 S. Kingshighway Blvd.
                  St. Louis, MO 63110
          as part of the 1993, 1994 DICOM Central Test Node project for, and
          under contract with, the Radiological Society of North America.

          THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND NEITHER RSNA NOR
          WASHINGTON UNIVERSITY MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
          PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
          USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
          SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
          THE SOFTWARE IS WITH THE USER.

          Copyright of the software and supporting documentation is
          jointly owned by RSNA and Washington University, and free access
          is hereby granted as a license to use this software, copy this
          software and prepare derivative works based upon this software.
          However, any distribution of this software source code or
          supporting documentation or derivative works (source code and
          supporting documentation) must include the three paragraphs of
          the copyright notice.
*/
/* Copyright marker.  Copyright will be inserted above.  Do not remove */

/*
**				DICOM 93
**		     Electronic Radiology Laboratory
**		   Mallinckrodt Institute of Radiology
**		Washington University School of Medicine
**
** Module Name(s):
**			DCM_ListToString
**			DCM_IsString
** Author, Date:	Stephen M. Moore, 13-Jun-93
** Intent:		This file contains more DCM routines which are used
**			as support for the DCM facility and for applications.
**			These routines help parse strings and other data
**			values that are encoded in DICOM objects.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

/* DCM_ListToString
**
** Purpose:
**	Convert the list of strings into a single string separated by '\'
**
** Parameter Dictionary:
**	list		Handle to the list of strings
**	offset		The actual string starts at "offset" offset in
**			each individual structure chained in the list
**	string		The single large string returned to the caller
**
** Return Values:
**	DCM_NORMAL
**	DCM_LISTFAILURE
**	DCM_MALLOCFAILURE
**
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/
typedef struct {
    void *reserved[2];
    char *s;
}   GENERIC;

CONDITION
DCM_ListToString(LST_HEAD * list, long offset, char **string)
{
    GENERIC
	* g;
    char
       *c,
       *p;
    long
        length;

    *string = NULL;
    if (list == NULL)
	return DCM_NORMAL;

    g = (void *)LST_Head(&list);
    if (g == NULL)
	return DCM_NORMAL;

    (void) LST_Position(&list, (void *)g);

    length = 0;
    while (g != NULL) {
	c = ((char *) g) + offset;
	length += strlen(c) + 1;
	g = (void *)LST_Next(&list);
    }

    p = CTN_MALLOC(length);
    if (p == NULL)
	return COND_PushCondition(DCM_MALLOCFAILURE,
		DCM_Message(DCM_MALLOCFAILURE), length, "DCM_ListToString");

    *string = p;
    g = (void *)LST_Head(&list);
    if (g == NULL)
	return COND_PushCondition(DCM_LISTFAILURE, DCM_Message(DCM_LISTFAILURE),
				  "DCM_ListToString");
    (void) LST_Position(&list, (void *)g);

    length = 0;
    while (g != NULL) {
	c = ((char *) g) + offset;
	length = strlen(c);
	(void) memcpy(p, c, length);
	p += length;
	*p++ = '\\';
	g = (void *)LST_Next(&list);
    }
    *--p = '\0';
    return DCM_NORMAL;
}


/* DCM_IsString
**
** Purpose:
**	Verify if the DICOM value representation is that of a string
**
** Parameter Dictionary:
**	representation		One of the many DICOM value representations
**
** Return Values:
**	TRUE
**	FALSE
**
** Notes:
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

CTNBOOLEAN
DCM_IsString(DCM_VALUEREPRESENTATION representation)
{
    CTNBOOLEAN
	flag = FALSE;

    switch (representation) {
    default: break ;
    case DCM_AE:		/* Application Entity */
    case DCM_AS:		/* Age string */
	flag = TRUE;
	break;
    case DCM_AT:		/* Attribute tag */
	break;
    case DCM_CS:		/* Control string */
    case DCM_DA:		/* Date */
	flag = TRUE;
	break;
    case DCM_DD:		/* Data set */
	break;
    case DCM_DS:		/* Decimal string */
    case DCM_DT:		/* Old date/time */
	flag = TRUE;
	break;
    case DCM_FD:		/* Floating double */
    case DCM_FL:		/* Float */
	break;
    case DCM_IS:		/* Integer string */
    case DCM_LO:		/* Long string */
    case DCM_LT:		/* Long text */
	flag = TRUE;
	break;
    case DCM_OB:		/* Other binary value (byte) */
    case DCM_OT:		/* Other binary value */
    case DCM_OW:		/* Other binary value (word) */
	break;
    case DCM_SH:		/* Short string */
	flag = TRUE;
	break;
    case DCM_SL:		/* Signed long */
    case DCM_SQ:		/* Sequence of items */
    case DCM_SS:		/* Signed short */
	break;
    case DCM_ST:		/* Short text */
    case DCM_TM:		/* Time */
	flag = TRUE;
	break;
    case DCM_UL:		/* Unsigned long */
    case DCM_US:		/* Unsigned short */
    /*case DCM_UNKNOWN:*/	/* Unknown/unspecified */
    case DCM_RET:		/* Retired */
    case DCM_CTX:		/* Context sensitive */
	break;
    case DCM_PN:		/* Person Name */
    case DCM_UI:		/* Unique identifier (UID) */
    case DCM_UT:		/* Unlimited Text */
	flag = TRUE;
	break;
    };
    return flag;
}
/*
          Copyright (C) 1993, RSNA and Washington University

          The software and supporting documentation for the Radiological
          Society of North America (RSNA) 1993 Digital Imaging and
          Communications in Medicine (DICOM) Demonstration were developed
          at the
                  Electronic Radiology Laboratory
                  Mallinckrodt Institute of Radiology
                  Washington University School of Medicine
                  510 S. Kingshighway Blvd.
                  St. Louis, MO 63110
          as part of the 1993 DICOM Central Test Node project for, and
          under contract with, the Radiological Society of North America.

          THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND NEITHER RSNA NOR
          WASHINGTON UNIVERSITY MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
          PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
          USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
          SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
          THE SOFTWARE IS WITH THE USER.

          Copyright of the software and supporting documentation is
          jointly owned by RSNA and Washington University, and free access
          is hereby granted as a license to use this software, copy this
          software and prepare derivative works based upon this software.
          However, any distribution of this software source code or
          supporting documentation or derivative works (source code and
          supporting documentation) must include the three paragraphs of
          the copyright notice.
*/
/*
** @$=@$=@$=
*/
/*
**				DICOM 93
**		     Electronic Radiology Laboratory
**		   Mallinckrodt Institute of Radiology
**		Washington University School of Medicine
**
** Module Name(s):
** Author, Date:	Thomas R. Leith, 15-Apr-93
** Intent:		This package implements atomic functions on
**			linked lists.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

#define CURRENT  (*list)->current
#define OLD_NEXT (*list)->current->next
#define OLD_PREV (*list)->current->previous



LST_HEAD *
LST_Create(void)
/*
**  This module creates a new list head and returns your handle to it.
**
*/
{
    LST_HEAD
    * ptr;

    ptr = CTN_MALLOC(sizeof(LST_HEAD));
    if (ptr == NULL)
	return NULL;

    ptr->head = NULL;
    ptr->tail = NULL;
    ptr->current = NULL;
    ptr->count = 0;
    return ptr;
}



CONDITION
LST_Destroy(LST_HEAD ** list)
/*
 *  This routine will destroy a list.  The list must be empty.
 *  The list handle is set to NULL as a side-effect.
 *
 */
{

    if ((*list)->count != 0)
	return LST_LISTNOTEMPTY;

    CTN_FREE(*list);
    *list = NULL;
    return LST_NORMAL;
}



CONDITION
LST_Enqueue(LST_HEAD ** list, LST_NODE * node)
/*
 *  Adds a new node to the tail of the list and returns
 *  status.
 *
 */
{
    node->next = NULL;		/* no next node              */
    node->previous = (*list)->tail;	/* previous is old tail      */
    if ((*list)->head == NULL)	/* if list was empty...      */
	(*list)->head = node;	/* it has a head now!        */
    else
	(*list)->tail->next = node;	/* old tail now has a next   */

    (*list)->tail = node;	/* list now has a new tail    */
    (*list)->count++;		/* bump the counter           */
    return LST_NORMAL;
}

CONDITION
LST_Push(LST_HEAD ** list, LST_NODE * node)
/*
 *  Adds a new node to the head of the list and returns
 *  status.
 *
 */

{
    node->next = (*list)->head;	/* set the forward link      */
    node->previous = NULL;	/* set rearward link         */
    if ((*list)->tail == NULL)	/* if the list was empty     */
	(*list)->tail = node;	/* set the tail pointer      */
    else			/* otherwise,                */
	(*list)->head->previous = node;	/* old head now has a previous                  */

    (*list)->head = node;	/* set new first node        */
    (*list)->count++;		/* bump the counter          */
    return LST_NORMAL;

}

LST_NODE *
LST_Dequeue(LST_HEAD ** list)
/*
 *  Removes a node from the head of the list and returns
 *  a pointer to it.
 *
 */
{
    LST_NODE
    * ptr;

    if ((*list)->head == NULL) {/* list is empty             */
	(*list)->count = 0;
	return NULL;
    }
    ptr = (*list)->head;	/* save the head             */
    (*list)->head = ptr->next;	/* set new head of list      */
    if ((*list)->head == NULL)	/* if the list is now empty  */
	(*list)->tail = NULL;	/* there is no tail anymore  */
    else
	(*list)->head->previous = NULL;	/* new head has no previous  */
    ptr->next = NULL;		/* hide data from user       */
    (*list)->count--;		/* list has one fewer node   */
    /* now                       */
    return ptr;
}



LST_NODE *
LST_Pop(LST_HEAD ** list)
/*
 *  Removes a node from the head of the list and returns
 *  a pointer to it.
 *
 */
{
    LST_NODE
    * ptr;

    if ((*list)->head == NULL) {/* list is empty             */
	(*list)->count = 0;
	return NULL;
    }
    ptr = (*list)->head;	/* save the head             */
    (*list)->head = ptr->next;	/* set new head of list      */
    if ((*list)->head == NULL)	/* if the list is now empty  */
	(*list)->tail = NULL;	/* there is no tail anymore  */
    else
	(*list)->head->previous = NULL;	/* new head has no previous  */
    ptr->next = NULL;		/* hide data from user       */
    (*list)->count--;		/* list has one fewer node   */
    /* now                       */
    return ptr;
}



unsigned long
LST_Count(LST_HEAD ** list)
/*
 *  Returns the number of nodes in the list.
 *
 */
{
    return (*list)->count;
}



LST_NODE *
LST_Head(LST_HEAD ** list)
/*
 *  Returns a pointer to the node at the head of the list.
 *  It does NOT remove the node from the list.
 *
 */
{
    return (*list)->head;
}


LST_NODE *
LST_Current(LST_HEAD ** list)
/*
 *  Returns a pointer to the current node.
 *  It does NOT remove the node from the list.
 *
 */
{
    return (*list)->current;
}



LST_NODE *
LST_Tail(LST_HEAD ** list)
/*
 *  Returns a pointer to the node at the tail of the list.
 *  It does NOT remove the node from the list.
 *
 */
{
    return (*list)->tail;
}


CONDITION
LST_Insert(LST_HEAD ** list, LST_NODE * node, LST_END where)
/*
**  Inserts a new node in the list.  User selects whether to insert closer
**  the HEAD end, or the TAIL end.  If the list is empty, the distinction is
**  moot.  In any case, CURRENT is set to the newly-inserted node.  In the
**  case of an error, the list is unchanged.
**/

{
    if ((where != LST_K_BEFORE) && (where != LST_K_AFTER))
	goto badend;

    if ((*list)->head == NULL) {/* if the list was empty     */
	(*list)->tail = node;	/* set the tail pointer      */
	(*list)->head = node;	/* set the head pointer      */
	(*list)->count = 0;	/* will get bumped later...  */
	(node)->next = NULL;	/* there is no next          */
	(node)->previous = NULL;/* and no previous           */

    } else if (CURRENT == NULL)	/* is he mixing semantics?	 */
	goto nocurrent;

    else if ((CURRENT == (*list)->head) &&	/* if at the head           */
	     (where == LST_K_BEFORE)) {	/* and inserting BEFORE   */
	node->next = CURRENT;	/* splice new node in       */
	CURRENT->previous = node;	/* before the current     */
	node->previous = NULL;	/* new one has no previous  */
	(*list)->head = node;	/* new one is first now     */

    } else if ((CURRENT == (*list)->tail) &&	/* if at the tail           */
	       (where == LST_K_AFTER)) {	/* and inserting AFTER    */
	node->next = NULL;	/* new node has no next     */
	node->previous = (*list)->tail;	/* previous is old tail     */
	CURRENT->next = node;	/* splice new node in       */
	(*list)->tail = node;	/* new node is now the tail */

    } else if (where == LST_K_AFTER) {	/* not a special case       */
	OLD_NEXT->previous = node;	/* we preceed a node        */
	node->next = OLD_NEXT;	/* the old next follows us  */
	node->previous = CURRENT;	/* the current preceeds us  */
	CURRENT->next = node;	/* we follow current        */

    } else {			/* not a special case       */
	OLD_PREV->next = node;	/* we follow the previous   */
	node->previous = OLD_PREV;	/* of current            */
	node->next = CURRENT;	/* current follows us and   */
	CURRENT->previous = node;	/* we preceed current     */
    };

    (*list)->count++;		/* bump the counter          */
    (*list)->current = node;	/* and set current        */
    return LST_NORMAL;

badend:
    return LST_BADEND;

nocurrent:
    return LST_NOCURRENT;
}



LST_NODE *
LST_Remove(LST_HEAD ** list, LST_END dir)
/*
**  Removes the current node from the list and returns a pointer to it.
**  How CURRENT gets set depends on which way the DIR argument points.  If
**  DIR is LST_K_BEFORE, CURRENT will move towards the tail-end of the
**  list.  If DIR is LST_K_AFTER, CURRENT will move towards the head-end of
**  the list.  If there is no node in the direction of DIR, CURRENT becomes
**  undefined.
**
**/
{
    LST_NODE
    * ptr;

    if ((dir != LST_K_BEFORE) && (dir != LST_K_AFTER))
	goto baddir;
    if (CURRENT == NULL)
	goto nocurrent;
    if ((*list)->head == NULL)
	goto listempty;

    ptr = CURRENT;		/* save node                 */

    if (CURRENT == (*list)->head) {	/* removing the head         */
	(*list)->head = OLD_NEXT;	/* set new head of list      */
	if ((*list)->head == NULL)	/* if the list is now empty  */
	    (*list)->tail = NULL;	/* no tail anymore either    */
	else
	    (*list)->head->previous = NULL;	/* new head has no previous  */
	if (dir == LST_K_BEFORE)/* there is nothing before   */
	    (*list)->current = NULL;	/* the head of the list      */
	else			/* otherwise, remain         */
	    (*list)->current = (*list)->head;	/* at the head...         */

    } else if (CURRENT == (*list)->tail) {	/* removing the tail         */
	(*list)->tail = OLD_PREV;	/* set new tail of list      */
	(*list)->tail->next = NULL;	/* new tail has no next      */
	if (dir == LST_K_AFTER)	/* there is nothing after    */
	    (*list)->current = NULL;	/* the tail of a list        */
	else			/* otherwise, remain         */
	    (*list)->current = (*list)->tail;	/* at the tail...            */

    } else {			/* not a special case        */
	OLD_PREV->next = CURRENT->next;	/* set forward pointer       */
	OLD_NEXT->previous = CURRENT->previous;	/* set backward pointer      */
	if (dir == LST_K_BEFORE)/* depending on direction,   */
	    (*list)->current = CURRENT->previous;	/* set current             */
	else			/* in the                    */
	    (*list)->current = CURRENT->next;	/* list head                 */
    }

    (*list)->count--;		/* one fewer nodes now       */
    ptr->previous = NULL;	/* hide data from user       */
    ptr->next = NULL;		/* hide data from user       */
    return ptr;

baddir:
    return NULL;

nocurrent:
    return NULL;

listempty:
    (*list)->count = 0;
    (*list)->current = NULL;
    (*list)->head = (*list)->tail = NULL;
    return NULL;
}



LST_NODE *
LST_Next(LST_HEAD ** list)
/*
 *  Returns a pointer to the next node in the list and
 *  makes it current.
 *
 */
{
    if ((*list)->head == NULL) {/* list is empty            */
	(*list)->count = 0;
	return NULL;
    }
    if (CURRENT == NULL) {	/* there is no CURRENT      */
	return NULL;
    }
    CURRENT = CURRENT->next;	/* Set current to next and return it */
    return CURRENT;
}



LST_NODE *
LST_Previous(LST_HEAD ** list)
/*
 *  Returns a pointer to the previous node in the list and
 *  makes it current.
 *
 */
{
    if ((*list)->head == NULL) {/* list is empty     */
	(*list)->count = 0;
	return NULL;
    }
    if (CURRENT == NULL) {	/* there is no CURRENT       */
	return NULL;
    }
    if (CURRENT->previous == NULL) {	/* no PREVIOUS               */
	return NULL;
    }
    CURRENT = CURRENT->previous;/* found it                  */
    return CURRENT;
}



LST_NODE *
LST_Position(LST_HEAD ** list, LST_NODE * node)
/*
 *  Make a node current and return the argument.
 *
 *
 *  Notes:  node = lst_position(list, lst_head(list));
 *          makes the node at the head of the list current
 *          and returns a pointer to it.
 *
 *      The routine tries to verify that "node" is in the list
 *      by doing a few consistency checks.  It assumes that if
 *      any of three "known" pointers are what they should be
 *      that all is well.  Its not damnfoolproof, but...
 */
{
    if ((*list)->head == NULL) {/* list is empty     */
	return NULL;
    }
    if (node == NULL)
	return NULL;
    if (((node->previous == NULL) && ((*list)->head == node)) ||
	((node->next == NULL) && ((*list)->tail == node)) ||
	(node->previous->next == node)) {	/* its probably OK       */

	CURRENT = node;
	return CURRENT;
    };

    return NULL;
}

/*
 *  Sort a list in order according to a comparison algorithm provided
 *  by the caller.
 *
 */
CONDITION
LST_Sort(LST_HEAD ** list, size_t nodeSize, int (*compare) ())
{
    LST_NODE
	* n1,
	*n2;
    LST_HEAD
	temp,
	*head;
    CTNBOOLEAN
	inserted;
    int ccc=0 ;

    if ((*list)->head == NULL) {/* list is empty     */
	return LST_NORMAL;
    }
    head = &temp;
    head->head = NULL;
    head->tail = NULL;
    head->current = NULL;
    head->count = 0;

    while ((n1 = LST_Dequeue(list)) != NULL) {
	n2 = LST_Head(&head);
	if (n2 != NULL)
	    (void) LST_Position(&head, n2);
	inserted = FALSE;
	while (n2 != NULL && !inserted) {
#if 0
	    if (compare(n1, n2) < 0) {
#else
            AFNI_CALL_VALU_2ARG(compare,int,ccc,LST_NODE *,n1,LST_NODE *,n2) ;
            if( ccc < 0 ){
#endif
		(void) LST_Insert(&head, n1, LST_K_BEFORE);
		inserted = TRUE;
	    } else
		n2 = LST_Next(&head);
	}
	if (n2 == NULL)
	    (void) LST_Enqueue(&head, n1);
    }
    **list = *head;
    return LST_NORMAL;
}

/*
 *  Return the item at position index.  Can be NULL if list is
 *  empty or we go off the end of the list.
 *
 */
LST_NODE *
LST_Index(LST_HEAD ** l, int index)
{
    LST_NODE
    * n;

    n = LST_Head(l);
    if (n == NULL)
	return NULL;

    index--;
    LST_Position(l, n);
    while (index-- > 0 && n != NULL)
	n = LST_Next(l);

    return n;
}
/*
          Copyright (C) 1993, 1994, RSNA and Washington University

          The software and supporting documentation for the Radiological
          Society of North America (RSNA) 1993, 1994 Digital Imaging and
          Communications in Medicine (DICOM) Demonstration were developed
          at the
                  Electronic Radiology Laboratory
                  Mallinckrodt Institute of Radiology
                  Washington University School of Medicine
                  510 S. Kingshighway Blvd.
                  St. Louis, MO 63110
          as part of the 1993, 1994 DICOM Central Test Node project for, and
          under contract with, the Radiological Society of North America.

          THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND NEITHER RSNA NOR
          WASHINGTON UNIVERSITY MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
          PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
          USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
          SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
          THE SOFTWARE IS WITH THE USER.

          Copyright of the software and supporting documentation is
          jointly owned by RSNA and Washington University, and free access
          is hereby granted as a license to use this software, copy this
          software and prepare derivative works based upon this software.
          However, any distribution of this software source code or
          supporting documentation or derivative works (source code and
          supporting documentation) must include the three paragraphs of
          the copyright notice.
*/
/* Copyright marker.  Copyright will be inserted above.  Do not remove */

/*
**		     Electronic Radiology Laboratory
**		   Mallinckrodt Institute of Radiology
**		Washington University School of Medicine
**
** Module Name(s):	UTL_RegexMatch, UTL_ConvertRegex,
**			UTL_ConvertDatetoLong, UTL_ConvertLongtoDate,
**			UTL_ConvertTimetoFloat, UTL_ConvertFloattoTime,
**			UTL_SqueezeBlanks, UTL_DateMatch, UTL_TimeMatch
**			UTL_GetDicomDate, UTL_GetDicomTime
**
** Author, Date:	David E. Beecher, March 1994
** Intent:		Miscellaneous functions that may be useful in
**			a number of different areas.
**
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

#if 0
/* UTL_RegexMatch
**
** Purpose:
**	Perform a DICOM regular expression match with the specified string, stm.
**
** Parameter Dictionary:
**	char *regex:
**		The DICOM regular expression to try and match.
**	char *stm:
**		The input string to match.
**
** Return Values:
**	UTL_MATCH:	The input string matched the regular expression.
**	UTL_NOMATCH:	The input string did not match the regular expression.
**
** Algorithm:
**	A simple function to perform a DICOM regular expression match with the
**	specified  string, stm.  The sematics of the DICOM patterns must be altered
**	slightly to work correctly with regex under unix...more information may
**	be found below.
**
*/

#ifdef DARWIN
#define USEREGCOMP
#endif

#ifdef USEREGCOMP
#include <regex.h>
#endif

CONDITION
UTL_RegexMatch(char *regex, char *stm)
{
#ifdef USEREGCOMP

    int
        ret,
        regexReturn;
    char
       *new_rstring;
    regex_t preg;
    char errorBuff[256];
    regmatch_t pmatch;

    new_rstring = UTL_ConvertRegex(regex);

    regexReturn = regcomp(&preg, new_rstring, 0);
    if (regexReturn != 0) {
	regerror(regexReturn, &preg, errorBuff, sizeof(errorBuff));
	fprintf(stderr, "%d\n", regexReturn);
	fprintf(stderr, "%s\n", errorBuff);

	free(new_rstring);
	return (UTL_NOMATCH);
    } else {
	ret = regexec(&preg, stm, 1, &pmatch, 0);

	switch (ret) {
	case 0:
	    free(new_rstring);
	    return (UTL_MATCH);
	    break;
	default:
	    free(new_rstring);
	    return (UTL_NOMATCH);
	    break;
	}
    }
#else
    int
        ret;
    char
       *new_rstring;

    new_rstring = UTL_ConvertRegex(regex);
    if (re_comp(new_rstring) != (char *) 0) {
	free(new_rstring);
	return (UTL_NOMATCH);
    } else {
	ret = re_exec(stm);
	switch (ret) {
	case 0:
	case -1:
	    free(new_rstring);
	    return (UTL_NOMATCH);
	    break;
	case 1:
	    free(new_rstring);
	    return (UTL_MATCH);
	    break;
	}
    }
#endif
}

/* UTL_ConvertRegex
**
** Purpose:
**	This function converts a DICOM "regular expression" to the proper
**	regex semantics under unix.
**
** Parameter Dictionary:
**	char *regex:
**		The DICOM regular expression to convert.
**
** Return Values:
**	char *:	The converted regular expression which expresses DICOM pattern
**		matching in regex semantics.
**
** Notes:
**	This routine needs to return a string of unknown length.  Since we
**	don't want to burden the caller with having to remember to free the
**	string after it has been used, we simply reuse the same piece of storage
**	and realloc it when necessary to increase the size.
**
** Algorithm:
**	Simple function to convert a DICOM "regular expression" to the proper
**	regex semantics under unix.  DICOM has only 2 meta characters, "*" for 0
**	or more occurrences, and "?" for a single character.  The "*" must be
**	converted to ".*" for regex while the "?" must be converted to ".".
**	Other special characters to regex like "[", "]", and "." must also
**	be escaped with the "\".  The DICOM escape character is assumed to be "\".
*/
char *
UTL_ConvertRegex(char *regex)
{

    char
       *new_regex = (char *) NULL;
    int
        malloced_size = 0;
    int
        i,
        j,
        escape_on;

    if (new_regex == (char *) NULL) {
	malloced_size = REGEX_SIZE;
	if ((new_regex = (char *) malloc(malloced_size)) == (char *) NULL) {
	    return ((char *) NULL);
	}
    }
    i = j = 0;
    escape_on = OFF;
    new_regex[j++] = '^';
    while (regex[i] != '\000') {
	switch (regex[i]) {
	case '*':		/* Transform the "*" to ".*" or "\*" if
				 * escaped */
	    switch (escape_on) {
	    case OFF:
		new_regex[j++] = '.';
		break;
	    case ON:
		new_regex[j++] = '\\';
		escape_on = OFF;
		break;
	    }
	    new_regex[j++] = '*';
	    i++;
	    break;
	case '?':		/* Transform the "?" to "." or "?" if escaped */
	    switch (escape_on) {
	    case OFF:
		new_regex[j++] = '.';
		break;
	    case ON:
		new_regex[j++] = '?';
		escape_on = OFF;
		break;
	    }
	    i++;
	    break;
	case '\\':		/* Note that we have seen the escape
				 * character */
	    switch (escape_on) {
	    case OFF:
		escape_on = ON;
		break;
	    case ON:
		escape_on = OFF;
		new_regex[j++] = '\\';
		new_regex[j++] = '\\';
		break;
	    }
	    i++;
	    break;
	case '.':
	case '[':		/* These are special to regex and need to be
				 * escaped */
	case ']':
	    new_regex[j++] = '\\';
	    new_regex[j++] = regex[i++];
	    escape_on = OFF;
	    break;
	default:		/* Leave the "\" in at this juncture */
	    switch (escape_on) {
	    case ON:
		new_regex[j++] = '\\';
		escape_on = OFF;
		break;
	    case OFF:
		break;
	    }
	    new_regex[j++] = regex[i++];
	    break;
	}
	if (j >= (malloced_size - 2)) {
	    malloced_size += REGEX_SIZE;
	    if ((new_regex = (char *) realloc(new_regex, malloced_size)) ==
		(char *) NULL) {
		return ((char *) NULL);
	    }
	}
    }
    new_regex[j++] = '$';
    new_regex[j] = '\000';
    return (new_regex);
}
#endif
/* UTL_ConvertDatetoLong
**	Convert a Dicom date to a long for comparision ease.
*/
long
UTL_ConvertDatetoLong(const char *date)
{

    char
        year[5],
        month[3],
        day[3];

    strncpy(year, date, 4);
    year[4] = '\000';
    strncpy(month, date + 4, 2);
    month[2] = '\000';
    strncpy(day, date + 6, 2);
    day[2] = '\000';

    return ((atol(year) * 10000) + (atol(month) * 100) + atol(day));
}

/* UTL_ConvertLongtoDate
**	Convert a long to a Dicom date.
*/
void
UTL_ConvertLongtoDate(long ld, char *date)
{

    int
        year,
        month,
        day;

    year = ld / 10000;
    ld -= (year * 10000);
    month = ld / 100;
    ld -= (month * 100);
    day = ld;

    sprintf(date, "%04d%02d%02d", year, month, day);

    return;
}

/* UTL_ConvertTimetoFloat
**	Convert a Dicom time to a floating point number for comparision ease.
*/
double
UTL_ConvertTimetoFloat(const char *time)
{

    size_t
    i;
    char
        hour[3],
        minute[3],
        second[3],
        fracsec[7];
    const char *p;
    double
        divisor,
        hh,
        mm,
        ss,
        fs;

    hh = mm = ss = fs = 0.0;
    hour[0] = minute[0] = second[0] = fracsec[0] = '\000';

    p = time;
    /*
     * Just a brute force way to tear down a Dicom time...not very pretty,
     * but it works... We are not guaranteed to have every field present as
     * we are in the date...
     */
    hour[0] = *p++;
    hour[1] = *p++;
    hour[2] = '\000';
    if (isdigit(*p)) {
	minute[0] = *p++;
	minute[1] = *p++;
	minute[2] = '\000';
	if (isdigit(*p)) {
	    second[0] = *p++;
	    second[1] = *p++;
	    second[2] = '\000';
	    if (*p == '.') {
		p++;
		fracsec[0] = *p++;
		if ((*p != '\000') && (isdigit(*p))) {
		    fracsec[1] = *p++;
		    if ((*p != '\000') && (isdigit(*p))) {
			fracsec[2] = *p++;
			if ((*p != '\000') && (isdigit(*p))) {
			    fracsec[3] = *p++;
			    if ((*p != '\000') && (isdigit(*p))) {
				fracsec[4] = *p++;
				if ((*p != '\000') && (isdigit(*p))) {
				    fracsec[5] = *p++;
				    fracsec[6] = '\000';
				} else
				    fracsec[5] = '\000';
			    } else
				fracsec[4] = '\000';
			} else
			    fracsec[3] = '\000';
		    } else
			fracsec[2] = '\000';
		} else
		    fracsec[1] = '\000';
	    }
	}
    }
    hh = atof(hour);
    mm = atof(minute);
    ss = atof(second);
    divisor = 1;
    for (i = 0; i < strlen(fracsec); i++)
	divisor *= 10;
    fs = atof(fracsec) / divisor;

    return ((hh * 3600.0) + (mm * 60.0) + ss + fs);
}

/* UTL_ConvertFloattoTime
**	Convert a floating point number to a Dicom time.
*/
void
UTL_ConvertFloattoTime(double dt, char *time)
{
    int
        hour,
        minute,
        second,
        fracsec;

    hour = (int) (dt / 3600.0);
    dt -= (hour * 3600);

    minute = (int) (dt / 60.);
    dt -= (minute * 60);

    second = (int) dt;
    dt -= second;

    fracsec = (int) ((dt * 1000000) + 0.5);

    sprintf(time, "%02d%02d%02d.%06d", hour, minute, second, fracsec);

    return;
}


/* UTL_SqueezeBlanks
**
*/
void
UTL_SqueezeBlanks(char *s)
{

    char
       *t1,
       *t2;

    t1 = t2 = s;
    while (*t2 != '\000') {
	if (*t2 != ' ') {
	    *t1 = *t2;
	    t1++;
	}
	t2++;
    }
    *t1 = '\000';

    return;
}
/* UTL_DateMatch
**	Match a date range as specified in the Dicom standard
*/
CONDITION
UTL_DateMatch(char *datestring, char *stm)
{

    int
        match;
    char
       *ndate;
    long
        start_date,
        end_date,
        date_in_question;

    if ((ndate = (char *) malloc(strlen(datestring) + 1)) == (char *) NULL)
	return (UTL_NOMATCH);

    strcpy(ndate, datestring);
    UTL_SqueezeBlanks(ndate);
    UTL_SqueezeBlanks(stm);

    match = 0;
    if (strchr(ndate, (int) '-') == (char *) NULL) {
	if (strcmp(ndate, stm) == 0)
	    match = 1;
    } else {
	date_in_question = UTL_ConvertDatetoLong(stm);
	if (ndate[0] == '-') {
	    end_date = UTL_ConvertDatetoLong(ndate + 1);
	    if (date_in_question <= end_date)
		match = 1;
	} else if (ndate[strlen(ndate) - 1] == '-') {
	    start_date = UTL_ConvertDatetoLong(ndate);
	    if (date_in_question >= start_date)
		match = 1;
	} else {
	    start_date = UTL_ConvertDatetoLong(ndate);
	    end_date = UTL_ConvertDatetoLong(strchr(ndate, (int) '-') + 1);
	    if ((date_in_question >= start_date) &&
		(date_in_question <= end_date))
		match = 1;
	}
    }
    free(ndate);
    if (match)
	return (UTL_MATCH);
    else
	return (UTL_NOMATCH);
}
/* UTL_TimeMatch
**	Match a time range as specified in the Dicom standard
*/
CONDITION
UTL_TimeMatch(char *timestring, char *stm)
{

    int
        match;
    char
       *ntime;
    double
        start_time,
        end_time,
        time_in_question;

    if ((ntime = (char *) malloc(strlen(timestring) + 2)) == (char *) NULL)
	return (UTL_NOMATCH);

    strcpy(ntime, timestring);
    UTL_SqueezeBlanks(ntime);
    UTL_SqueezeBlanks(stm);

    match = 0;
    if (strchr(ntime, (int) '-') == (char *) NULL) {
	if (strcmp(ntime, stm) == 0)
	    match = 1;
    } else {
	time_in_question = UTL_ConvertTimetoFloat(stm);
	if (ntime[0] == '-') {
	    end_time = UTL_ConvertTimetoFloat(ntime + 1);
	    if (time_in_question <= end_time)
		match = 1;
	} else if (ntime[strlen(ntime) - 1] == '-') {
	    start_time = UTL_ConvertTimetoFloat(ntime);
	    if (time_in_question >= start_time)
		match = 1;
	} else {
	    start_time = UTL_ConvertTimetoFloat(ntime);
	    end_time = UTL_ConvertTimetoFloat(strchr(ntime, (int) '-') + 1);
	    if ((time_in_question >= start_time) &&
		(time_in_question <= end_time))
		match = 1;
	}
    }
    free(ntime);
    if (match)
	return (UTL_MATCH);
    else
	return (UTL_NOMATCH);
}
/*
** UTL_GetDicomDate
**	Get the current date and store as a Dicom date.
*/
void
UTL_GetDicomDate(char *datestr)
{

    struct tm
       *tf;
    time_t
	loctime;

    loctime = time((time_t *) NULL);
    tf = localtime(&loctime);

    sprintf(datestr, "%04d%02d%02d", (tf->tm_year) + 1900, (tf->tm_mon) + 1, tf->tm_mday);
    return;

}
/*
** UTL_GetDicomTime
**	Get the current time and store as a Dicom time.
*/
void
UTL_GetDicomTime(char *timestr)
{

    struct tm
       *tf;
    time_t
	loctime;

    loctime = time((time_t *) NULL);
    tf = localtime(&loctime);

    sprintf(timestr, "%02d%02d%02d.%06d", (tf->tm_hour), (tf->tm_min), (tf->tm_sec), 0);
    return;
}

#ifdef _MSC_VER
typedef struct {
    char key[10];
    struct _timeb t;
}   UTL_TIMESTRUCTURE;
#else
typedef struct {
    char key[10];
    struct timeval t;
}   UTL_TIMESTRUCTURE;
#endif

void *
UTL_GetTimeStamp()
{
    UTL_TIMESTRUCTURE *t;

    t = AFMALL( UTL_TIMESTRUCTURE, sizeof(*t));
    if (t == NULL)
	return NULL;

    strcpy(t->key, "UTL STAMP");

    gettimeofday(&t->t, NULL);

    return t;
}

double
UTL_DeltaTime(void *timeStamp)
{
    struct timeval timeNow;
    UTL_TIMESTRUCTURE *t;
    double delta = 0.;

    gettimeofday(&timeNow, NULL);

    t = (UTL_TIMESTRUCTURE *) timeStamp;
    if (t == NULL)
	return -1.0;

    if (strcmp(t->key, "UTL STAMP") != 0)
	return -1.0;

    delta = timeNow.tv_sec - t->t.tv_sec;
    delta += (timeNow.tv_usec - t->t.tv_usec) / 1000000.;

    return delta;
}

void
UTL_ReleaseTimeStamp(void *timeStamp)
{
    UTL_TIMESTRUCTURE *t;

    t = (UTL_TIMESTRUCTURE *) timeStamp;
    if (t == NULL)
	return;

    if (strcmp(t->key, "UTL STAMP") != 0)
	return;

    free(timeStamp);
}

CONDITION
UTL_VerifyCreatePath(const char *path)
{
    int i;
#ifdef _MSC_VER
    struct _stat buf;
#else
    struct stat buf;
#endif
    char
       *p,
        temp[1024];
    int flag = 0;
    static int statCount = 0;

#ifdef _MSC_VER
    statCount++;
    i = _stat(path, &buf);
#else
    i = stat(path, &buf);
#endif


    if (i == 0) {
#ifdef _MSC_VER
	flag = ((buf.st_mode & _S_IFDIR) != 0);
#else
	flag = (S_ISDIR(buf.st_mode));
#endif
	if (flag)
	    return UTL_NORMAL;
	else
	    return UTL_PATHNOTDIR;
    }
    p = temp;

    while (*path != '\0') {
	*p++ = *path++;
	while (*path != '/' && *path != '\\' && *path != '\0') {
#ifdef _MSC_VER
	    if (*path == ':') {
		*p++ = *path++;
		if (*path == '\0')	/* We should not get C:\0, but test
					 * it */
		    break;
	    }
#endif
	    *p++ = *path++;
	}

	*p = '\0';
#ifdef _MSC_VER
	statCount++;
	i = _stat(temp, &buf);
#else
	i = stat(temp, &buf);
#endif

	if (i == 0) {
#ifdef _MSC_VER
	    flag = ((buf.st_mode & _S_IFDIR) != 0);
#else
	    flag = (S_ISDIR(buf.st_mode));
#endif
	    if (!flag)
		return UTL_PATHNOTDIR;
	} else {
#ifdef _MSC_VER
	    int e1;
	    e1 = errno;
	    memset(&buf, 0, sizeof(buf));
	    /*fprintf(stderr, "Stat Count = %d\n", statCount);*/
	    statCount++;
	    i = _stat(temp, &buf);
	    e1 = errno;
	    i = _mkdir(temp);
#else
	    i = mkdir(temp, 0777);
#endif
	    if (i != 0) {
		int e1;
		e1 = errno;
		fprintf(stderr, "Stat Count = %d\n", statCount);
		perror(temp);
		return UTL_FILECREATEFAILED;
	    }
	}
    }
    return UTL_NORMAL;
}

CTNBOOLEAN UTL_IsDirectory(const char* path)
{
    int i;
#ifdef _MSC_VER
    struct _stat buf;
#else
    struct stat buf;
#endif

    int flag = 0;

#ifdef _MSC_VER
    i = _stat(path, &buf);
#else
    i = stat(path, &buf);
#endif


    if (i == 0) {
#ifdef _MSC_VER
	flag = ((buf.st_mode & _S_IFDIR) != 0);
#else
	flag = (S_ISDIR(buf.st_mode));
#endif
	if (flag)
	    return TRUE;
    }
    return FALSE;
}


#if 0
CONDITION UTL_ScanDirectory(const char* path,
			    LST_HEAD** lst)
{
  UTL_FILEITEM* item = 0;

#ifdef _WIN32
  long hFile = 0;
  struct _finddata_t fileInfo;
  char directoryText[1024];
  *lst = LST_Create();
  strcpy(directoryText, path);
  strcat(directoryText, "/*");
  if( (hFile = _findfirst(directoryText, &fileInfo)) == -1L)
    return 0;

  item = malloc(sizeof(*item));
  strcpy(item->path, fileInfo.name);
  LST_Enqueue(lst, item);

  while(_findnext(hFile, &fileInfo) == 0) {
    item = malloc(sizeof(*item));
    strcpy(item->path, fileInfo.name);
    LST_Enqueue(lst, item);
  }
  _findclose(hFile);

#else
  DIR* dirp;
  struct dirent* dp;

  *lst = LST_Create();
  dirp = opendir(path);
  if (dirp == 0)
    return 0;

  while ((dp = readdir(dirp)) != NULL) {
    item = malloc(sizeof(*item));
    strcpy(item->path, dp->d_name);
    LST_Enqueue(lst, (void *)item);
  }
  closedir(dirp);
#endif

  return UTL_NORMAL;
}
#endif

static char* UTL_configFile = 0;
static LST_HEAD* UTL_configList = 0;
typedef struct {
  void* reserved[2];
  char *pName;
  char *pValue;
} CONFIG_ITEM;

CONDITION UTL_ReadConfigFile( )
{
  FILE* f;
  char buf[1024];

  if (UTL_configList != 0)
    return UTL_NORMAL;

  UTL_configList = LST_Create();
  if (UTL_configList == NULL)
    return 0;

  if (UTL_configFile == 0)
    return UTL_NORMAL;

  if (UTL_configFile[0] == '\0')
    return UTL_NORMAL;

  f = fopen(UTL_configFile, "r");
  if (f == NULL)
    return 0;

  while (fgets(buf, sizeof(buf), f) != NULL) {
    char* token1;
    char* token2;
    CONFIG_ITEM* item;

    if (buf[0] == '#') continue;
    if (buf[0] == '\n') continue;
    token1 = strtok(buf, " \t\n");
    token2 = strtok(0, " \t\n");
    if (token2 == NULL) continue;

    item = (CONFIG_ITEM*)malloc(sizeof(*item) + strlen(token1) +
				strlen(token2) + 2);
    item->pName = ((char*)item) + sizeof(*item);
    strcpy(item->pName, token1);
    item->pValue = item->pName + strlen(token1) + 1;
    strcpy(item->pValue, token2);

    LST_Enqueue(&UTL_configList, (void *)item);
  }

  fclose(f);

  return UTL_NORMAL;
}

CONDITION UTL_SetConfigFile(const char* configFile)
{
  if (UTL_configFile != 0) {
    CTN_FREE(UTL_configFile);
  }

  if (configFile == 0 || configFile[0] == '\0') {
    char* p = getenv("CTN_TARGET");
    if (p == NULL) {
      return UTL_NO_CTN_TARGET;
    }
    UTL_configFile = (char*) malloc(strlen(p) + strlen("/runtime/ctn_cfg.txt") + 1);
    strcpy(UTL_configFile, p);
    strcat(UTL_configFile, "/runtime/ctn_cfg.txt");
  } else {
    UTL_configFile = (char*) malloc(strlen(configFile)+1);
    strcpy(UTL_configFile, configFile);
  }

  return UTL_NORMAL;
}

CONDITION UTL_TestConfigFile(const char* configFile)
{
  return UTL_NORMAL;
}
char* UTL_GetConfigParameter(const char* paramName)
{
  CONDITION cond;
  char nameCopy[256];
  CONFIG_ITEM* item;
  int idx;

  cond = UTL_ReadConfigFile( );
  if (cond != UTL_NORMAL)
    return NULL;

  item = (void *)LST_Head(&UTL_configList);
  if (item == NULL)
    return NULL;

  (void) LST_Position(&UTL_configList, (void *)item);
  while(item != NULL) {
    if (strcmp(item->pName, paramName) == 0)
      return item->pValue;

    item = (void *)LST_Next(&UTL_configList);
  }

  strcpy(nameCopy, paramName);
  idx = strlen(nameCopy) - 1;
  while (idx > 0) {
    if (nameCopy[idx] == '/') {
      nameCopy[idx] = '\0';
      idx = -1;
      break;
    } else {
      idx--;
    }
  }

  if (idx < 0) {
    return UTL_GetConfigParameter(nameCopy);
  } else {
    return NULL;
  }
}

char**
UTL_ExpandToPointerArray(const char* inputText,
			 const char* delimiters,
			 int* numberOfEntries)
{
  int idx;
  int memorySize = 0;
  int arrayIndex = 0;
  char** array;
  char* outputPtr;
  char* token;

  *numberOfEntries = 1;
  for (idx = 0; inputText[idx] != '\0'; idx++) {
    int j;
    for (j = 0; delimiters[j] != '\0'; j++) {
      if (inputText[idx] == delimiters[j]) {
	(*numberOfEntries)++;
	break;
      }
    }
  }

  memorySize = (sizeof(char*)) * (*numberOfEntries);
  memorySize += strlen(inputText) + 1;

  array = (char**)CTN_MALLOC(memorySize);
  outputPtr = ((char*) array) + ((sizeof(char*)) * (*numberOfEntries));
  strcpy(outputPtr, inputText);

  token = strtok(outputPtr, delimiters);
  while(token != NULL) {
    array[arrayIndex++] = token;
    token = strtok(NULL, delimiters);
  }

  return array;
}

CTNBOOLEAN UTL_IsFile(const char* path)
{
  int i;
  CTNBOOLEAN rtnValue = FALSE;

#ifdef _WIN32
  struct _stat buf;

  i = _stat(path, &buf);
  if (i == 0) {
    rtnValue = ((buf.st_mode & _S_IFREG) != 0);
  }
#else
  struct stat buf;
  i = stat(path, &buf);
  if (i == 0) {
    rtnValue = (S_ISREG(buf.st_mode));
  }
#endif

  return rtnValue;
}

CONDITION UTL_DeleteFile(const char* path)
{
  int i = 0;

  i = unlink(path);

  if (i == 0)
    return UTL_NORMAL;

  return COND_PushCondition(UTL_DELETEFILEFAILED, "");
}


CONDITION
UTL_FileSize(const char* path, U32* size)
{
  int status;
  struct stat im_stat;

  status = stat(path, &im_stat);
  if (status < 0) {
    *size = 0;
    return 0;
  } else {
    *size = im_stat.st_size;
    return UTL_NORMAL;
  }
}


/* DCM_ExportStream
**
** Purpose:
**	Export a DICOM object into the stream format suitable
**	for network transmission or disk storage.
**
** Parameter Dictionary:
**	object		Pointer to caller's DICOM object
**	opt		Bitmask giving options for exporting data.  Legal
**			options give the byte order of exported data:
**				DCM_ORDERNATIVE
**				DCM_ORDERLITTLEENDIAN
**				DCM_ORDERBIGENDIAN
**	buffer		Pointer to caller's buffer to hold next slug
**			of DCM stream data.
**	bufferlength	Length of caller's buffer to hold stream data.
**	returnlength	Pointer to caller's variable into which we write
**			the amount of data exported.
**	ctx		Pointer to context variable we maintain to keep
**			track of our location in export process.
**
** Return Values:
**
**	DCM_FILEACCESSERROR
**	DCM_ILLEGALOBJECT
**	DCM_LISTFAILURE
**	DCM_NORMAL
**	DCM_NULLOBJECT
**
** Algorithm:
**	Description of the algorithm (optional) and any other notes.
*/

CONDITION
DCM_ExportStream(DCM_OBJECT ** callerObject, unsigned long opt,
		 void *buffer, unsigned long bufferlength,
		 DCM_EXPORT_STREAM_CALLBACK* callback,
		 void *ctx)
{


    PRIVATE_OBJECT
	** object;
    CONDITION
	cond;

    object = (PRIVATE_OBJECT **) callerObject;
    cond = checkObject(object, "DCM_ExportStream");
    if (cond != DCM_NORMAL)
	return cond;

    return exportStream(callerObject, opt, buffer, bufferlength, callback,
			ctx, 0);
}
