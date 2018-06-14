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
** Module Name(s):
** Author, Date:	Stephen M. Moore, 15-Apr-93
** Intent:		This header defines public typedefs for the DICOM
**			software produced at the Mallinckrodt Institute of
**			Radiology.  It also defines unique identifiers
**			for standard classes and objects defined by the
**			standard.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

#ifndef DICOM_IS_IN
#define DICOM_IS_IN 1

#ifdef _MSC_VER
#include "dicom_platform.h"
#endif

#ifdef  __cplusplus
extern "C" {
#endif

#define SHORTSIZE 16
#define INTSIZE   32
#define LONGSIZE  64

#ifndef _SITE_MACROS
    typedef unsigned long CONDITION;
    typedef unsigned short U_SHORT;	/* normal unsigned short */
    typedef unsigned long U_LONG;	/* normal unsigned long */
    typedef unsigned long MASK_32;	/* For bit masks	 */
    typedef unsigned long CTNBOOLEAN;	/* for boolean ops	 */

#if !defined(SHORTSIZE) || SHORTSIZE != 16
/* The writers of this code assume that shorts are 16 bits long.
** If that is not the case, this system will not operate properly.
** This code will trip the compiler.  This code also tripes the
** compiler if you have not defined the macro SHORTSIZE.  You
** also want to define INTSIZE and LONGSIZE.
*/

    short c;
    char c;			/* See note above */
#endif

    typedef unsigned short U16;	/* unsigned, 16 bit */
    typedef short S16;		/* signed, 16 bit */

#if LONGSIZE == 64 && INTSIZE == 32	/* Such as an Alpha */
    typedef unsigned int U32;
    typedef int S32;

#elif LONGSIZE == 32		/* Most 32 bit workstations */
    typedef unsigned long U32;
    typedef long S32;

#else				/* Something we do not support */

/* The writers of this code assume that we can find a 32 bit integer
** defined for this system as an int or a long.  If that assumption
** is not true, this code will not operate properly.
** This code will trip the compiler.
*/

    short c;
    char c;			/* See note above */

#endif

#endif

#define	FORM_COND(facility, severity, value) \
	(CONDITION)((((unsigned long)value)<<16) | \
	(((unsigned long)facility) << 4) | ((unsigned long)severity))

#define	SEV_SUCC	1
#define SEV_INFORM	3
#define	SEV_WARN	5
#define	SEV_ERROR	2
#define	SEV_FATAL	4

#define CTN_SUCCESS(A)	(((A)&0xf) == SEV_SUCC)
#define CTN_INFORM(A)	(((A)&0xf) == SEV_INFORM)
#define CTN_WARNING(A)	(((A)&0xf) == SEV_WARN)
#define CTN_ERROR(A)	(((A)&0xf) == SEV_ERROR)
#define CTN_FATAL(A)	(((A)&0xf) == SEV_FATAL)

#if 0
/* We turn these on to force compiler errors to find dependencies
** on these older macros.  These are retired as of 2.8.6.
*/
#define SUCCESS(A)	(zzzz)
#define INFORM(A)	(zzzz)
#define WARNING(A)	(zzzz)
#define ERROR(A)	(zzzz)
#define FATAL(A)	(zzzz)
#endif

#define	FACILITY(A)	((unsigned long)(A)>>4) & 0xfff

#ifndef _FACILITY_CODES
#define	FAC_DUL		1
#define	FAC_IDBMB	2	/* IDB Multibyte */
#define	FAC_IDX		3
#define	FAC_LST		4
#define	FAC_DIAG	5
#define	FAC_COND	6
#define	FAC_GQ		7
#define	FAC_SRV		8
#define	FAC_DCM		9
#define	FAC_MSG		10
#define FAC_HUNK	11
#define FAC_DB		12
#define FAC_CFG		13
#define FAC_IAP		14
#define FAC_HIS		15
#define FAC_HAP		16
#define	FAC_IE		17
#define	FAC_UID		18
#define	FAC_SQ		19
#define FAC_ICON	20
#define FAC_PRN		21
#define	FAC_TBL		22	/* Table functions (relational database) */
#define	FAC_DMAN	23	/* DICOM Management of application
				 * connections */
#define	FAC_UTL		24	/* Utility functions */
#define	FAC_IDB		25	/* Image database */
#define	FAC_MUT		26	/* Motif utilities */
#define	FAC_IMAN	27	/* Image management */
#define	FAC_ICPY	30	/* Image copy (structures for queueing) */
#define	FAC_FIS		31	/* Fake information system */
#define FAC_SNP		32	/* TCP/IP snoop facility */
#define FAC_LUT		34	/* LUT facility */
#define FAC_IODV	35	/* IOD Verification */
#define FAC_THR		36	/* CTN Threading routines */
#define	FAC_DDR		37	/* DICOM Directory Services */
#define	FAC_ATH		38	/* Application thread usage */
#define	FAC_IRS		39	/* Image recycle system */
#define	FAC_TBLMB	40	/* Table functions (relational database) */
#define	FAC_CHR		41	/* Character set encoding utilities */

#define	FAC_MAXIMUM	50	/* Maximum number of facilities.  This can increase */

#define FAC_APP 0x0fff		/* for stand-alone programs	 */
#endif

#ifndef TRUE
#define TRUE	1
#define	FALSE	0
#endif


#ifndef MAX
#define MAX(x, y) (((x) < (y)) ? (y) : (x))
#endif
#ifndef MIN
#define MIN(x, y) (((x) < (y)) ? (x) : (y))
#endif
#define IS_EVEN(i) (~(i) & 0x01)
#define DIM_OF(a) (sizeof(a) / sizeof(a[0]))
#define IN_RANGE(n, lo, hi) ((lo) <= n && (n) <= (hi))
#define STRUCT_OFFSET(s, f)  (off_t)(((s *)(0))->f)

#ifdef NO_STRERROR
    static char *
        strerror(int e) {
	static char string[256];

	    sprintf(string, "Error number: %d", e);
	    return string;
    }
#endif

#define	DICOM_AS_LENGTH	4
#define	DICOM_CS_LENGTH	16
#define	DICOM_DS_LENGTH	16
#define	DICOM_IS_LENGTH	12
#define	DICOM_PN_LENGTH	64
#define	DICOM_DA_LENGTH	8
#define	DICOM_LO_LENGTH	64
#define	DICOM_TM_LENGTH	16
#define	DICOM_UI_LENGTH	64
#define	DICOM_SH_LENGTH	16
#define	DICOM_AE_LENGTH	16
#define	DICOM_ST_LENGTH	1024
#define	DICOM_LT_LENGTH	10240
#define DICOM_DT_LENGTH 26

#define VERSION_JUN1993	199306
#define VERSION_JUL1993	199307
#define VERSION_AUG1993	199308
#define VERSION_SEP1993	199309
#define VERSION_OCT1993	199310
#define VERSION_NOV1993	199311
#define VERSION_DEC1993	199312
#define VERSION_JAN1994	199401
#define VERSION_FEB1994	199402
#define VERSION_MAR1994	199403
#define VERSION_APR1994	199404
#define VERSION_MAY1994	199405
#define VERSION_JUN1994	199406
#define VERSION_JUL1994	199407
#define VERSION_AUG1994	199408
#define VERSION_SEP1994	199409
#define VERSION_OCT1994	199410
#define VERSION_NOV1994	199411
#define VERSION_DEC1994	199412
#define VERSION_JAN1995	199501
#define VERSION_FEB1995	199502
#define VERSION_MAR1995	199503
#define VERSION_APR1995	199504
#define VERSION_MAY1995	199505
#define VERSION_JUN1995	199506

#ifndef STANDARD_VERSION
#define STANDARD_VERSION VERSION_JUN1995
#endif

#define CTN_MALLOC(a) malloc((a))
#define CTN_FREE(a)   free((a))

#ifdef  __cplusplus
}
#endif

#endif
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
** Module Name(s):
** Author, Date:	Stephen M. Moore, 14-Apr-1993
** Intent:		This module defines function prototypes for the
**			CONDITION facility which is used to record status
**			and error messages on a stack.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

#ifndef COND_IS_IN
#define COND_IS_IN 1

#include <stdio.h>

#ifdef  __cplusplus
extern "C" {
#endif

CONDITION COND_PushCondition(CONDITION cond, char *controlString,...);
CONDITION
COND_ExtractConditions(CTNBOOLEAN(*callback) ());
CONDITION
COND_TopCondition(CONDITION * condition, char *text,
		  unsigned long maxlength);
CONDITION COND_PopCondition(CTNBOOLEAN clearstack);
CONDITION COND_EstablishCallback(void (*callback) ());
void COND_DumpConditions(void);
void COND_CopyText(char *txt, size_t length);
void COND_WriteConditions(FILE * lfp);

/*  Now define the fixed values for conditions returned by this
**  package.  Note that FAC_COND is used to generate these
**  conditions.  This should be defined in some global include
**  file so that we can keep all of the facilities straight.
*/

#define	COND_NORMAL	/* Successful return */ \
	FORM_COND(FAC_COND, SEV_SUCC, 1)


#ifdef  __cplusplus
}
#endif

#endif
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
** Author, Date:	Thomas R. Leith, xx-May-92
** Intent:		This module defines private structures
**			used by the LST facility.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

#ifdef  __cplusplus
extern "C" {
#endif

#define LST_KEYS	1	/* Private defs override public ones */

typedef struct lst_node {
    struct lst_node *next;	/* next node in list	 */
    struct lst_node *previous;	/* previous node	 */
    void *data;			/* node data		 */
}   LST_NODE, *LST_NODEPTR;

typedef struct lst_head {
    LST_NODE *head;		/* points at first node */
    LST_NODE *tail;		/* points at last node  */
    LST_NODE *current;		/* "    "  " node 	 */
    unsigned long count;	/* # of nodes in list   */
}   LST_HEAD, *LST_HEADPTR;

#ifdef  __cplusplus
}
#endif
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
** Author, Date:	Stephen M. Moore, 14-Apr-1993
** Intent:		This module defines several constants and function
**			prototypes for the LST facility which is used to
**			manipulate objects in linked lists.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

#ifndef LST_IS_IN
#define LST_IS_IN

#ifdef  __cplusplus
extern "C" {
#endif

#define LST_K_BEFORE		0x00000000
#define LST_K_AFTER		0xFFFFFFFF

#ifndef LST_KEYS
typedef void LST_HEAD;
typedef void LST_NODE;
#endif

typedef unsigned long LST_END;

LST_HEAD *LST_Create(void);
CONDITION LST_Destroy(LST_HEAD ** list);
CONDITION LST_Enqueue(LST_HEAD ** list, LST_NODE * node);
CONDITION LST_Push(LST_HEAD ** list, LST_NODE * node);
LST_NODE *LST_Dequeue(LST_HEAD ** list);
LST_NODE *LST_Pop(LST_HEAD ** list);
unsigned long LST_Count(LST_HEAD ** list);
LST_NODE *LST_Head(LST_HEAD ** list);
LST_NODE *LST_Current(LST_HEAD ** list);
LST_NODE *LST_Tail(LST_HEAD ** list);
CONDITION LST_Insert(LST_HEAD ** list, LST_NODE * node, LST_END where);
LST_NODE *LST_Remove(LST_HEAD ** list, LST_END dir);
LST_NODE *LST_Next(LST_HEAD ** list);
LST_NODE *LST_Previous(LST_HEAD ** list);
LST_NODE *LST_Position(LST_HEAD ** list, LST_NODE * node);
CONDITION LST_Sort(LST_HEAD ** list, size_t nodeSize, int (*compare) ());
LST_NODE *LST_Index(LST_HEAD ** list, int index);
char *LST_Message(CONDITION cond);

#define LST_Top(x) LST_Head((x))
#define LST_Front(x) LST_Head((x))

#define LST_NORMAL		/* Normal return from LST package */ \
	FORM_COND(FAC_LST, SEV_SUCC, 1)
#define LST_LISTNOTEMPTY	/* Attempt to destroy list with elements */ \
	FORM_COND(FAC_LST, SEV_ERROR, 3)
#define LST_BADEND		/* */ \
	FORM_COND(FAC_LST, SEV_ERROR, 5)
#define LST_NOCURRENT	/* */ \
	FORM_COND(FAC_LST, SEV_ERROR, 7)

#ifdef  __cplusplus
}
#endif

#endif
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
** Author, Date:	Stephen M. Moore, 22-Apr-93
** Intent:		This file contains definitions and function prototypes
**			for the OBJECT facility which allows the user to
**			manipulate DICOM Objects.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

#ifndef DCM_OBJECTS_IS_IN
#define DCM_OBJECTS_IS_IN 1

#ifdef  __cplusplus
extern "C" {
#endif


typedef void *DCM_OBJECT;

typedef enum {
    DCM_AE,			/* Application Entity */
    DCM_AS,			/* Age string */
    DCM_AT,			/* Attribute tag */
    DCM_CS,			/* Control string */
    DCM_DA,			/* Date */
    DCM_DD,			/* Data set */
    DCM_DS,			/* Decimal string */
    DCM_FD,			/* Floating double */
    DCM_FL,			/* Float */
    DCM_IS,			/* Integer string */
    DCM_LO,			/* Long string */
    DCM_LT,			/* Long text */
    DCM_OT,			/* Other binary value */
    DCM_SH,			/* Short string */
    DCM_SL,			/* Signed long */
    DCM_SQ,			/* Sequence of items */
    DCM_SS,			/* Signed short */
    DCM_ST,			/* Short text */
    DCM_TM,			/* Time */
    DCM_UI,			/* Unique identifier (UID) */
    DCM_UL,			/* Unsigned long */
    DCM_UN,			/* Unknown (DICOM Unknown) */
    DCM_US,			/* Unsigned short */
    /*DCM_UNKNOWN,*/		/* Unknown/unspecified (non-standard) */
    DCM_RET,			/* Retired */
    DCM_CTX,			/* Context sensitive (non-standard) */
    DCM_PN,			/* Person Name */
    DCM_OB,			/* Other, byte */
    DCM_OW,			/* Other, word */
    DCM_DT,			/* Date/Time */
    DCM_DLM,			/* Delimiter (non-standard) */
    DCM_UT			/* Unlimited text */
}   DCM_VALUEREPRESENTATION;

typedef enum {
    DCM_OBJECTUNKNOWN,
    DCM_OBJECTCOMMAND,
    DCM_OBJECTIMAGE,
    DCM_OBJECTELEMENTLIST
}   DCM_OBJECTTYPE;

#if LONGSIZE == 64
typedef unsigned int DCM_TAG;
#define DCM_MAKETAG(g, e) (((((unsigned int)(g)) << 16) & 0xffff0000) \
| ((unsigned int)(e) & 0xffff))
#else
typedef unsigned long DCM_TAG;
#define DCM_MAKETAG(g, e) (((((unsigned long)(g)) << 16) & 0xffff0000) \
| ((unsigned long)(e) & 0xffff))

#endif

#define	DCM_TAG_GROUP(t) (unsigned short) ((t) >> 16)
#define	DCM_TAG_ELEMENT(t) (unsigned short) ((t) & 0xffff)


typedef struct {
    DCM_TAG tag;
    DCM_VALUEREPRESENTATION representation;
    char description[48];
    unsigned long multiplicity;
    U32 length;
    union {
	char *string;
	char **stringArray;
	short *ss;
	S32 *sl;
	unsigned short *us;
	U32 *ul;
	void *ot;
	unsigned short *ow;
	unsigned char *ob;
	LST_HEAD *sq;
	DCM_TAG *at;
	LST_HEAD* fragments;
    }   d;
    off_t data_offset ;    /* RWCox: offset into file, if > 0 */
}   DCM_ELEMENT;

typedef struct {
    void *reserved[2];
    DCM_ELEMENT e;
}   DCM_ELEMENT_NODE;
typedef struct {
    void *reserved[2];
    DCM_TAG tag;
}   DCM_TAG_NODE;

typedef struct {
    DCM_ELEMENT e;
    long flag;
    long *flagAddress;
}   DCM_FLAGGED_ELEMENT;

typedef struct {
    void *reserved[2];
    DCM_OBJECT *object;
}   DCM_SEQUENCE_ITEM;

typedef struct {
  void* reserved[2];
  U32 length;
  unsigned char* fragment;
} DCM_FRAGMENT_ITEM;


#define DCM_PREAMBLELENGTH 128

#define	DCM_FILEMETA_IMPLEMENTATIONVERSIONNAME	(1 << 0)
#define	DCM_FILEMETA_SOURCEAPPLICATIONENTITYTITLE (1 << 1)
#define	DCM_FILEMETA_PRIVATEINFORMATIONCREATORUID (1 << 2)
#define	DCM_FILEMETA_PRIVATEINFORMATION			(1 << 3)

typedef struct {
    long flag;
    unsigned char preamble[DCM_PREAMBLELENGTH];
    unsigned char fileMetaInformationVersion[2];
    char mediaStorageSOPClassUID[DICOM_UI_LENGTH + 1];
    char mediaStorageSOPInstanceUID[DICOM_UI_LENGTH + 1];
    char transferSyntaxUID[DICOM_UI_LENGTH + 1];
    char implementationClassUID[DICOM_UI_LENGTH + 1];
    char implementationVersionName[DICOM_SH_LENGTH + 1];
    char sourceApplicationEntityTitle[DICOM_AE_LENGTH + 1];
    char privateInformationCreatorUID[DICOM_UI_LENGTH + 1];
    unsigned char *privateInformation;
    unsigned long privateInformationLength;
}   DCM_FILE_META;


#define	DCM_ORDERMASK		0x7f
#define	DCM_ORDERNATIVE		0x01
#define	DCM_ORDERLITTLEENDIAN	0x02
#define	DCM_ORDERBIGENDIAN	0x03
#define	DCM_EXPLICITLITTLEENDIAN	0x04
#define	DCM_EXPLICITBIGENDIAN		0x05
#define	DCM_ENCAPSULATEDPIXELS	0x06

#define	DCM_FILEFORMATMASK	0x80
#define	DCM_PART10FILE		0x80

#define	DCM_CONVERTMASK		0x100
#define	DCM_FORMATCONVERSION	0x100

#define	DCM_DELETEMASK		0x200
#define	DCM_DELETEONCLOSE	0x200

#define	DCM_GROUPLENGTHMASK	0x400
#define	DCM_NOGROUPLENGTH	0x400

#define	DCM_SEQUENCELENGTHMASK	0x800
#define	DCM_UNSPECIFIEDLENGTHFLAG 0x800

#define	DCM_LENGTHTOENDMASK	0x1000
#define	DCM_USELENGTHTOEND	0x1000

#define DCM_REPEATELEMENTSMASK	0x2000
#define	DCM_ALLOWREPEATELEMENTS	0x2000

/* These bits allow us to modify the file name during the open
** to map from upper to lower case and \ to / to allow compatibility
** between Unix filenames and Win filenames.
*/
#define	DCM_FILENAMEMASK	0x2000
#define	DCM_TRYFILENAMECHANGE	0x2000

/* These bits allow us to accept objects with explicit VR with improper
** VRs (according to our dictionary).  Examples are people who send
** a VR of US when we expect SS.
*/
#define	DCM_VRMASK		0x4000
#define DCM_ACCEPTVRMISMATCH	0x4000

#define	DCM_SPECIALFORMATMASK	0x8000
#define	DCM_EFILM		0x8000

/* Define prototypes for functions provided by this facility.
*/
CONDITION DCM_OpenFile(const char *name, unsigned long opt, DCM_OBJECT ** object);
CONDITION DCM_CreateObject(DCM_OBJECT ** obj, unsigned long opt);
CONDITION DCM_CopyObject(DCM_OBJECT ** src, DCM_OBJECT** dst);
CONDITION DCM_MergeObject(DCM_OBJECT ** src, DCM_OBJECT** dst);
CONDITION DCM_AddElement(DCM_OBJECT ** obj, DCM_ELEMENT * ele);
CONDITION DCM_AddSequenceElement(DCM_OBJECT ** obj, DCM_ELEMENT * ele);
CONDITION DCM_RemoveElement(DCM_OBJECT ** obj, DCM_TAG tag);
CONDITION DCM_RemoveGroup(DCM_OBJECT ** callerObject, unsigned short group);
CONDITION
DCM_ImportStream(unsigned char *buf, unsigned long length,
		 unsigned long opt, DCM_OBJECT ** rntObj);
CONDITION
DCM_ReadStream(DCM_OBJECT ** obj, unsigned long opt, long size, void *ctx,
	  CONDITION(*rd) (void *ctx, void *buf, int toRead, int *bytesRead),
	       CONDITION(*sk) (void *ctx, int offset, int flag));
CONDITION DCM_CloseObject(DCM_OBJECT ** obj);

typedef
CONDITION(DCM_EXPORT_STREAM_CALLBACK) (void *buf, U32 bytesExported, int lastFlag, void *ctx);

CONDITION
DCM_ExportStream(DCM_OBJECT ** obj, unsigned long opt, void *buf,
		 unsigned long maxlen, DCM_EXPORT_STREAM_CALLBACK* callback,
		 void *ctx);
CONDITION
DCM_GetElementValue(DCM_OBJECT ** obj, DCM_ELEMENT * element,
		    U32 * rtnLength, void **ctx);
char*
DCM_GetString(DCM_OBJECT** obj, DCM_TAG tag);

CONDITION
DCM_GetElement(DCM_OBJECT ** obj, DCM_TAG tag,
	       DCM_ELEMENT * attribute);
CONDITION
DCM_GetElementSize(DCM_OBJECT ** obj, DCM_TAG tag,
		   U32 * retlen);
CONDITION
DCM_GetElementValueOffset(DCM_OBJECT **obj, DCM_ELEMENT *element,
		unsigned long offset);
typedef
CONDITION(DCM_GET_COMPRESSED_CALLBACK) (void *buf, U32 bytesExported,
	int index, int startFlag, int lastFlag, int startOfFragment, void *ctx);

CONDITION
DCM_GetCompressedValue(DCM_OBJECT ** obj, DCM_TAG tag, void *buf,
	size_t bufSize, DCM_GET_COMPRESSED_CALLBACK *callback, void *ctx);

CONDITION DCM_GetObjectSize(DCM_OBJECT ** obj, unsigned long *retlen);
CONDITION DCM_DumpElements(DCM_OBJECT ** obj, long vm);
CONDITION DCM_FormatElements(DCM_OBJECT ** obj, long vm, const char* prefix);
CONDITION DCM_LookupElement(DCM_ELEMENT * attribute);
CONDITION
DCM_GroupDictionary(unsigned short group, void *ctx,
	 void (*callback) (unsigned short g, char *description, void *ctx));
CONDITION
DCM_ElementDictionary(DCM_TAG tag, void *ctx,
		      void (*callback) (DCM_TAG t, char *description, DCM_VALUEREPRESENTATION r, void *ctx));
char *DCM_Message(CONDITION cond);
void DCM_Debug(CTNBOOLEAN flag);
CONDITION DCM_WriteFile(DCM_OBJECT ** obj, unsigned long opt, const char *file);
CONDITION 
DCM_ComputeExportLength(DCM_OBJECT ** obj, unsigned long opt,
			unsigned long *length);
CONDITION
DCM_ModifyElements(DCM_OBJECT ** obj, DCM_ELEMENT * element,
	     int count, DCM_FLAGGED_ELEMENT * flaggedElement, int flagCount,
		   int *updateCount);
CONDITION DCM_ListToString(LST_HEAD * list, long offset, char **string);
CONDITION
DCM_ParseObject(DCM_OBJECT ** obj, DCM_ELEMENT * list, int count,
      DCM_FLAGGED_ELEMENT * flaggedElement, int flagCount, int *parseCount);
CONDITION
DCM_ScanParseObject(DCM_OBJECT ** object, void *buf, size_t bufferSize,
    DCM_FLAGGED_ELEMENT * elementVector, int vectorLength,
    CONDITION(*callback) (const DCM_ELEMENT* e, void* ctx),
		    void *ctx);
CTNBOOLEAN DCM_IsString(DCM_VALUEREPRESENTATION representation);
CONDITION
DCM_GetSequenceList(DCM_OBJECT ** object, DCM_TAG tag, LST_HEAD ** list);
CONDITION
DCM_GetSequenceElement(DCM_OBJECT** obj, DCM_TAG top, DCM_ELEMENT* e);
CONDITION
DCM_GetSequenceByOffset(DCM_OBJECT ** object, DCM_TAG tag, unsigned long offset,
			DCM_OBJECT ** rtnObject);
CONDITION
DCM_GetElementValueList(DCM_OBJECT ** object, DCM_TAG tag,
		 size_t structureSize, long stringOffset, LST_HEAD ** list);
CONDITION
DCM_AddElementList(DCM_OBJECT ** callerObject, DCM_ELEMENT * element,
		   LST_HEAD * list, long offset);
CONDITION
DCM_GetFileMeta(DCM_OBJECT ** callerObject, DCM_FILE_META ** fileMeta);
CONDITION
DCM_SetFileMeta(DCM_OBJECT ** callerObject, DCM_FILE_META * fileMeta);
CONDITION
DCM_FreeFileMeta(DCM_FILE_META ** fileMeta);
CONDITION
DCM_DefaultFileMeta(DCM_OBJECT ** object, DCM_FILE_META ** fileMeta);
CONDITION
DCM_CompareAttributes(DCM_OBJECT **o1, DCM_OBJECT **o2,
		      void (*callback) (const DCM_ELEMENT *e1,
					const DCM_ELEMENT *e2,
					void *ctx),
			void *ctx);

CTNBOOLEAN
DCM_GroupPresent(DCM_OBJECT** callerObject, U16 group);

CONDITION
DCM_GetFirstElement(DCM_OBJECT** callerObject, DCM_ELEMENT** element);
CONDITION
DCM_GetNextElement(DCM_OBJECT** callerObject, DCM_ELEMENT** element);

CONDITION
DCM_AddFragment(DCM_OBJECT** callerObject, void* fragment, U32 fragmentLength);

#define DCM_NORMAL		/* Normal return from DCM package */ \
	FORM_COND(FAC_DCM, SEV_SUCC, 1)
#define DCM_FILEOPENFAILED	/* Failed to open file requested by caller */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 2)
#define DCM_FILEACCESSERROR	/* Error accessing open file */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 3)
#define DCM_OBJECTCREATEFAILED	/* Failed to create a new DCM object */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 4)
#define DCM_NULLOBJECT	/* Null object passed to DCM routine */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 5)
#define DCM_ILLEGALOBJECT	/* Illegal object passed to DCM routine */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 6)
#define DCM_ELEMENTNOTFOUND	/* Requested element not found in object */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 7)
#define DCM_ILLEGALSTREAMLENGTH	/* Illegal length for stream (too short) */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 8)
#define DCM_ELEMENTCREATEFAILED	/* Failed to create a new DCM element */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 9)
#define DCM_UNRECOGNIZEDGROUP	/* Unrecognized group */ \
	FORM_COND(FAC_DCM, SEV_WARN, 10)
#define DCM_UNRECOGNIZEDELEMENT	/* Unrecognized element */ \
	FORM_COND(FAC_DCM, SEV_WARN, 11)
#define DCM_ELEMENTOUTOFORDER	/* Element out of order in DCM stream */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 12)
#define DCM_LISTFAILURE		/* Failure by a list routine */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 13)
#define	DCM_ILLEGALOPTION	/* Illegal option for processing stream */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 14)
#define	DCM_ILLEGALADD		/* Illegal Add of an element */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 19)
#define	DCM_GETINCOMPLETE	/* Incomplete data get operation */ \
	FORM_COND(FAC_DCM, SEV_WARN, 20)
#define	DCM_ILLEGALCONTEXT	/* Illegal context value */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 21)
#define	DCM_ILLEGALREPRESENTATION	/* Illegal rep given by caller */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 22)
#define	DCM_UNEVENELEMENTLENGTH	/* Uneven element data length */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 23)
#define	DCM_ELEMENTLENGTHERROR	/* Element length > remaining bytes in data */\
	FORM_COND(FAC_DCM, SEV_ERROR, 25)
#define	DCM_GROUPNOTFOUND	/* Did not find requested group */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 27)
#define	DCM_FILECREATEFAILED	/* Failed to create a file */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 28)
#define	DCM_FILEIOERROR		/* File I/O error */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 29)
#define	DCM_INSERTFAILED	/* Failed to insert a new element */ \
	FORM_COND(FAC_DCM, SEV_ERROR, 30)
#define	DCM_CANNOTGETSEQUENCEVALUE	FORM_COND(FAC_DCM, SEV_ERROR, 31)
#define	DCM_FILEDELETEFAILED		FORM_COND(FAC_DCM, SEV_ERROR, 32)
#define	DCM_MALLOCFAILURE		FORM_COND(FAC_DCM, SEV_ERROR, 33)
#define	DCM_NULLADDRESS			FORM_COND(FAC_DCM, SEV_ERROR, 34)
#define	DCM_UNEXPECTEDREPRESENTATION	FORM_COND(FAC_DCM, SEV_ERROR, 35)
#define	DCM_BADELEMENTINGROUP		FORM_COND(FAC_DCM, SEV_ERROR, 36)
#define	DCM_CALLBACKABORTED		FORM_COND(FAC_DCM, SEV_WARN, 37)
#define	DCM_READSTREAMFAILED		FORM_COND(FAC_DCM, SEV_ERROR, 38)
#define	DCM_STREAMCOMPLETE		FORM_COND(FAC_DCM, SEV_SUCC, 39)
#define	DCM_UNRECOGNIZEDVRCODE		FORM_COND(FAC_DCM, SEV_ERROR, 40)
#define	DCM_VRMISMATCH			FORM_COND(FAC_DCM, SEV_ERROR, 41)
#define DCM_EXPORTBUFFERTOOSMALL	FORM_COND(FAC_DCM, SEV_ERROR, 42)
#define	DCM_BADOFFSET			FORM_COND(FAC_DCM, SEV_ERROR, 43)
#define	DCM_BADLENGTH			FORM_COND(FAC_DCM, SEV_ERROR, 44)
#define	DCM_NOTASEQUENCE		FORM_COND(FAC_DCM, SEV_ERROR, 45)
#define	DCM_GENERALWARNING		FORM_COND(FAC_DCM, SEV_WARN, 46)
#define DCM_EMPTYOBJECT			FORM_COND(FAC_DCM, SEV_WARN, 47)
#define DCM_GETNEXTELEMENTCOMPLETE	FORM_COND(FAC_DCM, SEV_SUCC, 48)
#define DCM_REPEATEDELEMENT		FORM_COND(FAC_DCM, SEV_WARN, 49)
#define	DCM_NOFRAGMENTSINOBJECT		FORM_COND(FAC_DCM, SEV_ERROR, 50)
#define	DCM_UNEVENFRAGMENTLENGTH	FORM_COND(FAC_DCM, SEV_ERROR, 51)


/*  Define all of the known groups and elements in those groups.  This
**  will allow coders to use symbolic references to these rather than
**  remember the values.
*/

/*  Define the known groups
*/
#define	DCM_GROUPCOMMAND		0x0000
#define	DCM_GROUPFILEMETA		0x0002
#define	DCM_GROUPBASICDIRINFO		0x0004	/* Part 10, Media */
#define	DCM_GROUPIDENTIFYING		0x0008
#define	DCM_GROUPPATIENTINFO		0x0010
#define	DCM_GROUPACQUISITION		0x0018
#define	DCM_GROUPRELATIONSHIP		0x0020
#define	DCM_GROUPIMAGE			0x0028
#define	DCM_GROUPSTUDY			0x0032
#define	DCM_GROUPVISIT			0x0038
#define	DCM_GROUPWAVEFORM		0x003a
#define	DCM_GRPPROCEDURE		0x0040
#define	DCM_GROUPDEVICE			0x0050
#define DCM_GROUPNMIMAGE		0x0054
#define	DCM_GROUPGRAPHICS		0x0070
#define DCM_GROUPMEDIA			0x0088
#define DCM_GROUPBASICFILMSESSION	0x2000
#define DCM_GROUPBASICFILMBOX		0x2010
#define DCM_GROUPBASICIMAGEBOX		0x2020
#define DCM_GROUPBASICANNOTATIONBOX	0x2030
#define DCM_GROUPBASICIMAGEOVERLAYBOX	0x2040
#define DCM_GROUPPRINTJOB		0x2100
#define DCM_GROUPPRINTER		0x2110
#define	DCM_GROUPTEXT			0x4000
#define	DCM_GROUPRESULTS		0x4008
#define DCM_GROUPCURVE			0x5000
#define	DCM_GROUPOVERLAY		0x6000
#define	DCM_GROUPPIXEL			0x7fe0
#define	DCM_GROUPPAD			0xfffc
#define	DCM_GROUPDELIMITER		0xfffe


#define	DCM_CMDGROUPLENGTH		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0000)
#define	DCM_CMDAFFECTEDCLASSUID		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0002)
#define	DCM_CMDREQUESTEDCLASSUID	DCM_MAKETAG(DCM_GROUPCOMMAND,0x0003)
#define	DCM_CMDCOMMANDFIELD		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0100)
#define	DCM_CMDMSGID			DCM_MAKETAG(DCM_GROUPCOMMAND,0x0110)
#define	DCM_CMDMSGIDRESPOND		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0120)
#define	DCM_CMDMOVEDESTINATION		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0600)
#define	DCM_CMDPRIORITY			DCM_MAKETAG(DCM_GROUPCOMMAND,0x0700)
#define	DCM_CMDDATASETTYPE		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0800)
#define	DCM_CMDSTATUS			DCM_MAKETAG(DCM_GROUPCOMMAND,0x0900)
#define DCM_CMDOFFENDINGELEMENT		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0901)
#define DCM_CMDERRORCOMMENT		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0902)
#define DCM_CMDERRORID			DCM_MAKETAG(DCM_GROUPCOMMAND,0x0903)
#define	DCM_CMDAFFECTEDINSTANCEUID	DCM_MAKETAG(DCM_GROUPCOMMAND,0x1000)
#define	DCM_CMDREQUESTEDINSTANCEUID	DCM_MAKETAG(DCM_GROUPCOMMAND,0x1001)
#define	DCM_CMDEVENTTYPEID		DCM_MAKETAG(DCM_GROUPCOMMAND,0x1002)
#define	DCM_CMDATTRIBUTEIDLIST		DCM_MAKETAG(DCM_GROUPCOMMAND,0x1005)
#define	DCM_CMDACTIONTYPEID		DCM_MAKETAG(DCM_GROUPCOMMAND,0x1008)
#define	DCM_CMDREMAININGSUBOPERATIONS	DCM_MAKETAG(DCM_GROUPCOMMAND,0x1020)
#define	DCM_CMDCOMPLETEDSUBOPERATIONS	DCM_MAKETAG(DCM_GROUPCOMMAND,0x1021)
#define	DCM_CMDFAILEDSUBOPERATIONS	DCM_MAKETAG(DCM_GROUPCOMMAND,0x1022)
#define	DCM_CMDWARNINGSUBOPERATIONS	DCM_MAKETAG(DCM_GROUPCOMMAND,0x1023)
#define	DCM_CMDMOVEAETITLE		DCM_MAKETAG(DCM_GROUPCOMMAND,0x1030)
#define	DCM_CMDMOVEMESSAGEID		DCM_MAKETAG(DCM_GROUPCOMMAND,0x1031)

/*  Define the legal values for the Command Field in the COMMAND group
*/

#define	DCM_STORE_REQUEST		0x0001
#define	DCM_STORE_RESPONSE		0x8001
#define	DCM_GET_REQUEST			0x0010
#define	DCM_GET_RESPONSE		0x8010
#define	DCM_FIND_REQUEST		0x0020
#define	DCM_FIND_RESPONSE		0x8020
#define	DCM_MOVE_REQUEST		0x0021
#define	DCM_MOVE_RESPONSE		0x8021
#define	DCM_ECHO_REQUEST		0x0030
#define	DCM_ECHO_RESPONSE		0x8030
#define	DCM_CANCEL_REQUEST		0x0fff
#define	DCM_CANCEL_RESPONSE		0x8fff
#define	DCM_DIALOG_REQUEST		0x0080
#define	DCM_DIALOG_RESPONSE		0x8080
#define	DCM_N_EVENT_REPORT_REQUEST	0x0100
#define	DCM_N_EVENT_REPORT_RESPONSE	0x8100
#define	DCM_N_GET_REQUEST		0x0110
#define	DCM_N_GET_RESPONSE		0x8110
#define	DCM_N_SET_REQUEST		0x0120
#define	DCM_N_SET_RESPONSE		0x8120
#define	DCM_N_ACTION_REQUEST		0x0130
#define	DCM_N_ACTION_RESPONSE		0x8130
#define	DCM_N_CREATE_REQUEST		0x0140
#define	DCM_N_CREATE_RESPONSE		0x8140
#define	DCM_N_DELETE_REQUEST		0x0150
#define	DCM_N_DELETE_RESPONSE		0x8150

/*  Define the elements in the COMMAND group
*/
#define	DCM_CMDLENGTHTOEND		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0001)
#define	DCM_CMDRECOGNITIONCODE		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0010)
#define	DCM_CMDINITIATOR		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0200)
#define	DCM_CMDRECEIVER			DCM_MAKETAG(DCM_GROUPCOMMAND,0x0300)
#define	DCM_CMDFINDLOCATION		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0400)
#define	DCM_CMDNUMBERMATCHES		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0850)
#define	DCM_CMDRESPSEQNUMBER		DCM_MAKETAG(DCM_GROUPCOMMAND,0x0860)
#define	DCM_CMDDIALOGRECEIVOR		DCM_MAKETAG(DCM_GROUPCOMMAND,0x4000)
#define	DCM_CMDTERMINALTYPE		DCM_MAKETAG(DCM_GROUPCOMMAND,0x4010)
#define	DCM_CMDMSGSETID			DCM_MAKETAG(DCM_GROUPCOMMAND,0x5010)
#define	DCM_CMDENDMSGD			DCM_MAKETAG(DCM_GROUPCOMMAND,0x5020)
#define	DCM_CMDDISPLAYFORMAT		DCM_MAKETAG(DCM_GROUPCOMMAND,0x5110)
#define	DCM_CMDPAGEPOSITIONID		DCM_MAKETAG(DCM_GROUPCOMMAND,0x5120)
#define	DCM_CMDTEXTFORMATID		DCM_MAKETAG(DCM_GROUPCOMMAND,0x5130)
#define	DCM_CMDNORREV			DCM_MAKETAG(DCM_GROUPCOMMAND,0x5140)
#define	DCM_CMDADDGRAYSCALE		DCM_MAKETAG(DCM_GROUPCOMMAND,0x5150)
#define	DCM_CMDBORDERS			DCM_MAKETAG(DCM_GROUPCOMMAND,0x5160)
#define	DCM_CMDCOPIES			DCM_MAKETAG(DCM_GROUPCOMMAND,0x5170)
#define	DCM_CMDMAGNIFICATIONTYPE 	DCM_MAKETAG(DCM_GROUPCOMMAND,0x5180)
#define	DCM_CMDERASE			DCM_MAKETAG(DCM_GROUPCOMMAND,0x5190)
#define	DCM_CMDPRINT			DCM_MAKETAG(DCM_GROUPCOMMAND,0x51a0)
#define	DCM_CMDOVERLAYS			DCM_MAKETAG(DCM_GROUPCOMMAND,0x51b0)

/* Define the elements in the File Meta Header Group (0x0002) */

#define	DCM_METAGROUPLENGTH		DCM_MAKETAG(DCM_GROUPFILEMETA,0x0000)
#define	DCM_METAINFORMATIONVERSION	DCM_MAKETAG(DCM_GROUPFILEMETA,0x0001)
#define	DCM_METAMEDIASTORAGESOPCLASS	DCM_MAKETAG(DCM_GROUPFILEMETA,0x0002)
#define	DCM_METAMEDIASTORAGESOPINSTANCE	DCM_MAKETAG(DCM_GROUPFILEMETA,0x0003)
#define	DCM_METATRANSFERSYNTAX		DCM_MAKETAG(DCM_GROUPFILEMETA,0x0010)
#define	DCM_METAIMPLEMENTATIONCLASS	DCM_MAKETAG(DCM_GROUPFILEMETA,0x0012)
#define	DCM_METAIMPLEMENTATIONVERSION	DCM_MAKETAG(DCM_GROUPFILEMETA,0x0013)
#define	DCM_METASOURCEAETITLE		DCM_MAKETAG(DCM_GROUPFILEMETA,0x0016)
#define	DCM_METAPRIVATEINFORMATIONCREATOR DCM_MAKETAG(DCM_GROUPFILEMETA,0x0100)
#define	DCM_METAPRIVATEINFORMATION	DCM_MAKETAG(DCM_GROUPFILEMETA,0x0102)

/* Define the elements in the Basic Directory information group, (0x0004) */

#define	DCM_DIRFILESETID          DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1130)
#define	DCM_DIRFILESETDESCRFILEID DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1141)
#define	DCM_DIRSPECIFICCHARACTER  DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1142)
#define	DCM_DIRFIRSTOFFSET        DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1200)
#define	DCM_DIRLASTOFFSET         DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1202)
#define	DCM_DIRFILESETCONSISTENCY DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1212)
#define	DCM_DIRRECORDSEQUENCE     DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1220)
#define	DCM_DIRNEXTRECORDOFFSET   DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1400)
#define	DCM_DIRRECORDINUSE        DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1410)
#define	DCM_DIRLOWERLEVELOFFSET   DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1420)
#define	DCM_DIRRECORDTYPE         DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1430)
#define	DCM_DIRPRIVATERECORDUID   DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1432)
#define	DCM_DIRREFERENCEDFILEID   DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1500)
#define	DCM_DIRMRDRRECORDOFFSET   DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1504)
#define	DCM_DIRREFSOPCLASSUID     DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1510)
#define	DCM_DIRREFSOPINSTANCEUID  DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1511)
#define	DCM_DIRREFTRANSFERSYNTAXUID DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1512)
#define	DCM_DIRNUMREFERENCES      DCM_MAKETAG(DCM_GROUPBASICDIRINFO, 0x1600)

/* Define the elements in the IDENTIFYING group, 0008
*/
#define	DCM_IDGROUPLENGTH		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0000)
#define	DCM_IDLENGTHTOEND		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0001)
#define	DCM_IDSPECIFICCHARACTER		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0005)
#define	DCM_IDIMAGETYPE			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0008)
#define	DCM_IDRECOGNITIONCODE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0010)
#define	DCM_IDINSTANCECREATEDATE 	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0012)
#define	DCM_IDINSTANCECREATETIME 	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0013)
#define	DCM_IDINSTANCECREATORUID 	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0014)
#define	DCM_IDSOPCLASSUID		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0016)
#define	DCM_IDSOPINSTANCEUID		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0018)
#define	DCM_IDSTUDYDATE			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0020)
#define	DCM_IDSERIESDATE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0021)
#define	DCM_IDACQUISITIONDATE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0022)
#define	DCM_IDIMAGEDATE			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0023)
#define	DCM_IDOVERLAYDATE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0024)
#define	DCM_IDCURVEDATE			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0025)
#define	DCM_IDSTUDYTIME			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0030)
#define	DCM_IDSERIESTIME		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0031)
#define	DCM_IDACQUISITIONTIME		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0032)
#define	DCM_IDIMAGETIME			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0033)
#define	DCM_IDOVERLAYTIME		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0034)
#define	DCM_IDCURVETIME			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0035)
/* Retired 0040, 0041, 0042 */
#define	DCM_IDDATASETTYPE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0040)
#define	DCM_IDDATASETSUBTYPE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0041)
#define	DCM_IDNMSERIESTYPE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0042)

#define	DCM_IDACCESSIONNUMBER		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0050)
#define	DCM_IDQUERYLEVEL		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0052)
#define	DCM_IDRETRIEVEAETITLE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0054)
#define	DCM_IDINSTANCEAVAILABILITY	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0056)
#define	DCM_IDFAILEDINSTANCEUIDLIST	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0058)
#define	DCM_IDMODALITY			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0060)
#define	DCM_IDMODALITIESINSTUDY		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0061)
#define	DCM_IDMODALITYSUBTYPE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0062)
#define	DCM_IDCONVERSIONTYPE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0064)
#define	DCM_IDPRESENTATIONINTENTTYPE	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0068)
#define	DCM_IDMANUFACTURER		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0070)
#define	DCM_IDINSTITUTIONNAME		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0080)
#define	DCM_IDINSTITUTIONADDR		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0081)
#define DCM_IDINSTITUTECODESEQUENCE	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0082)
#define	DCM_IDREFERRINGPHYSICIAN	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0090)
#define	DCM_IDREFERRINGPHYSADDR		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0092)
#define	DCM_IDREFERRINGPHYSPHONE	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0094)
#define DCM_IDCODEVALUE			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0100)
#define DCM_IDCODINGSCHEMEDESIGNATOR	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0102)
#define	DCM_IDCODINGSCHEMEVERSION	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0103)
#define DCM_IDCODEMEANING		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0104)
#define	DCM_IDMAPPINGRESOURCE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0105)
#define	DCM_IDCONTEXTGROUPVERSION	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x0106)
#define	DCM_IDCODESETEXTENSIONFLAG	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x010B)
#define	DCM_IDPRIVATECODINGSCHEMECREATORUID DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x010C)
#define	DCM_IDCODESETEXTENSIONCREATORUID DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x010D)
#define	DCM_IDMAPPINGRESOURCESEQ	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x010E)
#define	DCM_IDCONTEXTIDENTIFIER		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x010F)
#define	DCM_IDNETWORKID			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1000)
#define	DCM_IDSTATIONNAME		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1010)
#define DCM_IDSTUDYDESCRIPTION		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1030)
#define	DCM_IDPROCEDURECODESEQUENCE	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1032)
#define	DCM_IDSERIESDESCR		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x103e)
#define	DCM_IDINSTITUTIONALDEPT		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1040)
#define	DCM_IDPHYSICIANOFRECORD		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1048)
#define	DCM_IDPERFORMINGPHYSICIAN 	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1050)
#define	DCM_IDPHYSREADINGSTUDY		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1060)
#define	DCM_IDOPERATORNAME		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1070)
#define	DCM_IDADMITTINGDIAGDESCR	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1080)
#define	DCM_IDADMITDIAGCODESEQUENCE	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1084)
#define	DCM_IDMANUFACTURERMODEL		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1090)
#define	DCM_IDREFERENCEDRESULTSSEQ	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1100)
#define	DCM_IDREFERENCEDSTUDYSEQ	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1110)
#define DCM_IDREFERENCEDSTUDYCOMPONENTSEQ DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1111)
#define DCM_IDREFERENCEDSERIESSEQ	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1115)
#define	DCM_IDREFERENCEDPATIENTSEQ	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1120)
#define	DCM_IDREFERENCEDVISITSEQ	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1125)
#define	DCM_IDREFERENCEDOVERLAYSEQ	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1130)
#define	DCM_IDREFERENCEDIMAGESEQ	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1140)
#define DCM_IDREFERENCEDCURVESEQ	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1145)
#define	DCM_IDREFERENCEDPREVIOUSWAVEFORM DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1148)
#define	DCM_IDREFERENCEDSIMULTANEOUSWAVEFORMS DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x114A)
#define	DCM_IDREFERENCEDSUBSEQUENTWAVEFORM DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x114C)
#define	DCM_IDREFERENCEDSOPCLASSUID	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1150)
#define	DCM_IDREFERENCEDSOPINSTUID	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1155)
#define	DCM_IDREFERENCEDFRAMENUMBER	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1160)
#define	DCM_IDTRANSACTIONUID		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1195)
#define	DCM_IDFAILUREREASON		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1197)
#define	DCM_IDFAILEDSOPSEQUENCE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1198)
#define	DCM_IDREFERENCEDSOPSEQUENCE	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x1199)
#define	DCM_IDLOSSYIMAGECOMPRESSION	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2110)
#define	DCM_IDDERIVATIONDESCR		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2111)
#define	DCM_IDSOURCEIMAGESEQ		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2112)
#define	DCM_IDSTAGENAME			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2120)
#define	DCM_IDSTAGENUMBER		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2122)
#define	DCM_IDNUMBEROFSTAGES		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2124)
#define	DCM_IDVIEWNUMBER		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2128)
#define DCM_IDNUMBEROFEVENTTIMERS	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2129)
#define	DCM_IDNUMBERVIEWSINSTAGE	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x212a)
#define	DCM_IDEVENTELAPSEDTIME		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2130)
#define	DCM_IDEVENTTIMERNAME		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2132)
#define	DCM_IDSTARTTRIM			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2142)
#define	DCM_IDSTOPTRIM			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2143)
#define	DCM_IDDISPLAYFRAMERATE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2144)
#define DCM_IDTRANSDUCERPOSITION	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2200)
#define DCM_IDTRANSDUCERORIENTATION	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2204)
#define DCM_IDANATOMICSTRUCTURE		DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2208)
#define DCM_IDANATOMICREGIONSEQUENCE	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2218)
#define DCM_IDANATOMICREGIONMODIFIERSEQ	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2220)
#define DCM_IDPRIMARYANATOMICSTRUCTURESEQ DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2228)
#define DCM_IDPRIMARYANATOMICSTRUCTUREMODIFIERSEQ DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2230)
#define	DCM_IDTRANSDUCERPOSITIONSEQ	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2240)
#define	DCM_IDTRANSDUCERPOSITIONMODIFIERSEQ DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2242)
#define	DCM_IDTRANSDUCERORIENTATIONSEQ	DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2244)
#define	DCM_IDTRANSDUCERORIENTATIONMODIFIERSEQ DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x2246)
#define	DCM_IDCOMMENTS			DCM_MAKETAG(DCM_GROUPIDENTIFYING,0x4000)

/*  Define the elements in the PATIENT INFORMATION group (0x0010)
*/
#define	DCM_PATGROUPLENGTH		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x0000)
#define	DCM_PATNAME			DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x0010)
#define	DCM_PATID			DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x0020)
#define	DCM_ISSUERPATIENTID		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x0021)
#define	DCM_PATBIRTHDATE		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x0030)
#define DCM_PATBIRTHTIME		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x0032)
#define	DCM_PATSEX			DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x0040)
#define DCM_PATINSURANCEPLANCODESEQ	DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x0050)
#define	DCM_PATOTHERIDS			DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x1000)
#define	DCM_PATOTHERNAMES		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x1001)
#define	DCM_PATBIRTHNAME		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x1005)
#define	DCM_PATAGE			DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x1010)
#define	DCM_PATSIZE			DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x1020)
#define	DCM_PATWEIGHT			DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x1030)
#define	DCM_PATADDRESS			DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x1040)
#define	DCM_PATINSURANCEPLANID		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x1050)
#define	DCM_PATMOTHERBIRTHNAME		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x1060)
#define	DCM_PATMILITARYRANK		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x1080)
#define DCM_PATBRANCHOFSERVICE		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x1081)
#define	DCM_PATMEDICALRECORDLOCATOR	DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x1090)
#define DCM_PATMEDICALALERTS		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x2000)
#define	DCM_PATCONTRASTALLERGIES	DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x2110)
#define	DCM_COUNTRYOFRESIDENCE		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x2150)
#define	DCM_REGIONOFRESIDENCE		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x2152)
#define	DCM_PATTELEPHONENUMBER		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x2154)
#define	DCM_PATETHNICGROUP		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x2160)
#define	DCM_PATOCCUPATION		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x2180)
#define	DCM_PATSMOKINGSTATUS		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x21a0)
#define	DCM_PATADDITIONALPATHISTORY	DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x21b0)
#define	DCM_PATPREGNANCYSTATUS		DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x21c0)
#define	DCM_PATLASTMENSTRUALDATE	DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x21d0)
#define	DCM_PATRELIGIOUSPREFERENCE	DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x21f0)
#define	DCM_PATCOMMENTS			DCM_MAKETAG(DCM_GROUPPATIENTINFO,0x4000)

/*  Define the elements in the ACQUISITION INFORMATION group (0018)
*/

#define	DCM_ACQGROUPLENGTH		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0000)
#define	DCM_ACQCONTRASTBOLUSAGENT	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0010)
#define	DCM_ACQCONTRASTBOLUSAGENTSEQ	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0012)
#define	DCM_ACQCONTRASTBOLUSADMINROUTESEQ DCM_MAKETAG(DCM_GROUPACQUISITION,0x0014)
#define	DCM_ACQBODYPARTEXAMINED		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0015)
#define	DCM_ACQSCANNINGSEQUENCE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0020)
#define	DCM_ACQSEQUENCEVARIANT		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0021)
#define	DCM_ACQSCANOPTIONS		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0022)
#define	DCM_ACQMRACQUISITIONTYPE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0023)
#define	DCM_ACQSEQUENCENAME		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0024)
#define	DCM_ACQANGIOFLAG		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0025)
#define	DCM_ACQINTERVENTIONDRUGINFOSEQ	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0026)
#define	DCM_ACQINTERVENTIONDRUGSTOPTIME	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0027)
#define	DCM_ACQINTERVENTIONDRUGDOSE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0028)
#define	DCM_ACQINTERVENTIONDRUGCODESEQ	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0029)
#define	DCM_ACQADDITIONALDRUGSEQ	DCM_MAKETAG(DCM_GROUPACQUISITION,0x002a)
#define	DCM_ACQRADIONUCLIDE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0030)
#define	DCM_ACQRADIOPHARMACEUTICAL	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0031)
#define	DCM_ACQENERGYWCENTERLINE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0032)
#define	DCM_ACQENERGYWTOTALWIDTH	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0033)
#define	DCM_ACQINTERVENTIONDRUGNAME	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0034)
#define	DCM_ACQINTERVENTIONDRUGSTART	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0035)
#define	DCM_ACQINTERVENTIONALTHERAPYSEQ	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0036)
#define	DCM_ACQTHERAPYTYPE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0037)
#define	DCM_ACQINTERVENTIONALSTATUS	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0038)
#define	DCM_ACQTHERAPYDESCRIPTION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0039)
#define	DCM_ACQCINERATE			DCM_MAKETAG(DCM_GROUPACQUISITION,0x0040)
#define	DCM_ACQSLICETHICKNESS		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0050)
#define	DCM_ACQKVP			DCM_MAKETAG(DCM_GROUPACQUISITION,0x0060)
#define	DCM_ACQCOUNTSACCUMULATED	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0070)
#define	DCM_ACQTERMINATIONCONDITION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0071)
#define	DCM_ACQEFFECTIVESERIESDURATION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0072)
#define	DCM_ACQSTARTCONDITION		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0073)
#define DCM_ACQSTARTCONDITIONDATA	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0074)
#define	DCM_ACQTERMINATIONCONDITIONDATA DCM_MAKETAG(DCM_GROUPACQUISITION,0x0075)
#define	DCM_ACQREPETITIONTIME		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0080)
#define	DCM_ACQECHOTIME			DCM_MAKETAG(DCM_GROUPACQUISITION,0x0081)
#define	DCM_ACQINVERSIONTIME		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0082)
#define	DCM_ACQNUMBEROFAVERAGES		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0083)
#define	DCM_ACQIMAGINGFREQUENCY		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0084)
#define	DCM_ACQIMAGEDNUCLEUS		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0085)
#define	DCM_ACQECHONUMBER		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0086)
#define	DCM_ACQMAGNETICFIELDSTRENGTH	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0087)
#define	DCM_ACQSLICESPACING		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0088)
#define	DCM_ACQPHASEENCODINGSTEPS	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0089)
#define	DCM_ACQDATACOLLECTIONDIAMETER	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0090)
#define	DCM_ACQECHOTRAINLENGTH		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0091)
#define DCM_ACQPERCENTSAMPLING		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0093)
#define DCM_ACQPERCENTPHASEFIELDVIEW	DCM_MAKETAG(DCM_GROUPACQUISITION,0x0094)
#define DCM_ACQPIXELBANDWIDTH		DCM_MAKETAG(DCM_GROUPACQUISITION,0x0095)
#define	DCM_ACQDEVICESERIALNUM		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1000)
#define	DCM_ACQPLATEID			DCM_MAKETAG(DCM_GROUPACQUISITION,0x1004)
#define	DCM_ACQSECONDARYCAPTUREDEVID	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1010)
#define	DCM_ACQDATESECONDARYCAPTURE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1012)
#define	DCM_ACQTIMESECONDARYCAPTURE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1014)
#define	DCM_ACQSECONDARYCAPTMANUFACTURER DCM_MAKETAG(DCM_GROUPACQUISITION,0x1016)
#define	DCM_ACQSECONDARYCAPTMODEL	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1018)
#define	DCM_ACQSECONDARYCAPTSOFTWAREVERSION DCM_MAKETAG(DCM_GROUPACQUISITION,0x1019)
#define	DCM_ACQSOFTWAREVERSION		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1020)
#define	DCM_ACQVIDEOIMAGEFORMATACQ	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1022)
#define	DCM_ACQDIGITALIMAGEFORMATACQ	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1023)
#define	DCM_ACQPROTOCOLNAME		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1030)
#define	DCM_ACQCONTRASTBOLUSROUTE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1040)
#define	DCM_ACQCONTRASTBOLUSVOL		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1041)
#define	DCM_ACQCONTRASTBOLUSSTARTTIME	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1042)
#define	DCM_ACQCONTRASTBOLUSSTOPTIME	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1043)
#define	DCM_ACQCONTRASTBOLUSTOTALDOSE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1044)
#define	DCM_ACQSYRINGECOUNTS		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1045)
#define	DCM_ACQCONTRASTFLOWRATE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1046)
#define	DCM_ACQCONTRASTFLOWDURATION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1047)
#define	DCM_ACQCONTRASTBOLUSINGREDIENT	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1048)
#define	DCM_ACQCONTRASTBOLUSINGREDIENTCONCENTRATION DCM_MAKETAG(DCM_GROUPACQUISITION,0x1049)
#define	DCM_ACQSPATIALRESOLUTION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1050)
#define	DCM_ACQTRIGGERTIME		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1060)
#define	DCM_ACQTRIGGERSRCTYPE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1061)
#define	DCM_ACQNOMINALINTERVAL		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1062)
#define	DCM_ACQFRAMETIME		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1063)
#define	DCM_ACQFRAMINGTYPE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1064)
#define DCM_ACQFRAMETIMEVECTOR		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1065)
#define DCM_ACQFRAMEDELAY		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1066)
#define	DCM_ACQIMAGETRIGGERDELAY	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1067)
#define	DCM_ACQGROUPTIMEOFFSET		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1068)
#define	DCM_ACQTRIGGERTIMEOFFSET	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1069)
#define	DCM_ACQSYNCTRIGGER		DCM_MAKETAG(DCM_GROUPACQUISITION,0x106A)
#define	DCM_ACQSYNCFRAMEOFREFERENCE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x106B)
#define	DCM_ACQTRIGGERSAMPLEPOSITION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x106E)
#define	DCM_ACQRADIOPHARMROUTE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1070)
#define	DCM_ACQRADIOPHARMVOLUME		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1071)
#define	DCM_ACQRADIOPHARMSTARTTIME	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1072)
#define	DCM_ACQRADIOPHARMSTOPTIME 	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1073)
#define	DCM_ACQRADIONUCLIDETOTALDOSE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1074)
#define	DCM_ACQRADIONUCLIDEHALFLIFE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1075)
#define	DCM_ACQRADIONUCLIDEPOSITRONFRACTION DCM_MAKETAG(DCM_GROUPACQUISITION,0x1076)
#define	DCM_ACQRADIOPHARMACEUTICALSPECIFICACTIVITY DCM_MAKETAG(DCM_GROUPACQUISITION,0x1077)
#define	DCM_ACQBEATREJECTIONFLAG	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1080)
#define	DCM_ACQLOWRRVALUE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1081)
#define	DCM_ACQHIGHRRVALUE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1082)
#define	DCM_ACQINTERVALSACQUIRED	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1083)
#define	DCM_ACQINTERVALSREJECTED	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1084)
#define	DCM_ACQPVCREJECTION		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1085)
#define	DCM_ACQSKIPBEATS		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1086)
#define	DCM_ACQHEARTRATE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1088)
#define DCM_ACQCARDIACNUMBEROFIMAGES	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1090)
#define	DCM_ACQTRIGGERWINDOW		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1094)
#define	DCM_ACQRECONSTRUCTIONDIAMETER	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1100)
#define	DCM_ACQDISTANCESRCTODETECTOR	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1110)
#define	DCM_ACQDISTANCESRCTOPATIENT	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1111)
#define	DCM_ACQESTIMATEDRADIOGRAPHICMAGFACTOR	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1114)
#define	DCM_ACQGANTRYTILT		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1120)
#define	DCM_ACQGANTRYSLEW		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1121)
#define	DCM_ACQTABLEHEIGHT		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1130)
#define	DCM_ACQTABLETRAVERSE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1131)
#define	DCM_ACQTABLEMOTION		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1134)
#define	DCM_ACQTABLEVERTICALINCREMENT	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1135)
#define	DCM_ACQTABLELATERALINCREMENT	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1136)
#define	DCM_ACQTABLELONGITUDINALINCREMENT	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1137)
#define	DCM_ACQTABLEANGLE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1138)
#define	DCM_ACQROTATIONDIRECTION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1140)
#define	DCM_ACQANGULARPOSITION		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1141)
#define	DCM_ACQRADIALPOSITION		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1142)
#define	DCM_ACQSCANARC			DCM_MAKETAG(DCM_GROUPACQUISITION,0x1143)
#define DCM_ACQANGULARSTEP		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1144)
#define	DCM_ACQCENTERROTATIONOFFSET	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1145)
#define	DCM_ACQROTATIONOFFSET		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1146)
#define	DCM_ACQFIELDOFVIEWSHAPE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1147)
#define	DCM_ACQFIELDOFVIEWDIMENSION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1149)
#define	DCM_ACQEXPOSURETIME		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1150)
#define	DCM_ACQXRAYTUBECURRENT		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1151)
#define	DCM_ACQEXPOSURE			DCM_MAKETAG(DCM_GROUPACQUISITION,0x1152)
#define	DCM_ACQAVERAGEPULSEWIDTH	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1154)
#define	DCM_ACQRADIATIONSETTING		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1155)
#define	DCM_ACQRADIATIONMODE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x115a)
#define	DCM_ACQIMAGEAREADOSEPRODUCT	DCM_MAKETAG(DCM_GROUPACQUISITION,0x115e)
#define	DCM_ACQFILTERTYPE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1160)
#define	DCM_ACQTYPEOFFILTERS		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1161)
#define	DCM_ACQINTENSIFIERSIZE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1162)
#define	DCM_ACQIMAGERPIXELSPACING	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1164)
#define	DCM_ACQGRID			DCM_MAKETAG(DCM_GROUPACQUISITION,0x1166)
#define	DCM_ACQGENERATORPOWER		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1170)
#define	DCM_ACQCOLLIMATORGRIDNAME	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1180)
#define	DCM_ACQCOLLIMATORTYPE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1181)
#define	DCM_ACQFOCALDISTANCE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1182)
#define	DCM_ACQXFOCUSCENTER		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1183)
#define	DCM_ACQYFOCUSCENTER		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1184)
#define	DCM_ACQFOCALSPOT		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1190)
#define	DCM_ACQDATELASTCALIBRATION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1200)
#define	DCM_ACQTIMELASTCALIBRATION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1201)
#define	DCM_ACQCONVOLUTIONKERNEL	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1210)
#define	DCM_ACQUPPERLOWERPIXELVALUES	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1240)
#define	DCM_ACQACTUALFRAMEDURATION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1242)
#define	DCM_ACQCOUNTRATE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1243)
#define	DCM_ACQPREFPLAYBACKSEQUENCING	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1244)
#define	DCM_ACQRECEIVINGCOIL		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1250)
#define	DCM_ACQTRANSMITTINGCOIL		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1251)
#define	DCM_ACQPLATETYPE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1260)
#define	DCM_ACQPHOSPHORTYPE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1261)
#define	DCM_ACQSCANVELOCITY		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1300)
#define	DCM_ACQWHOLEBODYTECHNIQUE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1301)
#define	DCM_ACQSCANLENGTH		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1302)
#define	DCM_ACQACQUISITIONMATRIX	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1310)
#define	DCM_ACQPHASEENCODINGDIRECTION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1312)
#define	DCM_ACQFLIPANGLE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1314)
#define DCM_ACQVARIABLEFLIPANGLE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1315)
#define	DCM_ACQSAR			DCM_MAKETAG(DCM_GROUPACQUISITION,0x1316)
#define	DCM_ACQDBDT			DCM_MAKETAG(DCM_GROUPACQUISITION,0x1318)
#define	DCM_ACQDEVICEPROCESSINGDESCR	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1400)
#define	DCM_ACQDEVICEPROCESSINGCODE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1401)
#define	DCM_ACQCASSETTEORIENTATION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1402)
#define	DCM_ACQCASSETTESIZE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1403)
#define	DCM_ACQEXPOSURESONPLATE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1404)
#define	DCM_ACQRELATIVEXRAYEXPOSURE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1405)
#define	DCM_ACQCOLUMNANGULATION		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1450)
#define	DCM_ACQTOMOLAYERHEIGHT		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1460)
#define	DCM_ACQTOMOANGLE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1470)
#define	DCM_ACQTOMOTIME			DCM_MAKETAG(DCM_GROUPACQUISITION,0x1480)
#define	DCM_ACQPOSITIONERMOTION		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1500)
#define	DCM_ACQPOSITIONERPRIMARYANGLE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1510)
#define	DCM_ACQPOSITIONERSECONDARYANGLE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1511)
#define	DCM_ACQPOSITIONERPRIMARYANGLEINCR DCM_MAKETAG(DCM_GROUPACQUISITION,0x1520)
#define	DCM_ACQPOSITIONERSECONDARYANGLEINCR DCM_MAKETAG(DCM_GROUPACQUISITION,0x1521)
#define	DCM_ACQDETECTORPRIMARYANGLE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1530)
#define	DCM_ACQDETECTORSECONDARYANGLE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1531)
#define	DCM_ACQSHUTTERSHAPE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1600)
#define	DCM_ACQSHUTTERLEFTVERTICALEDGE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1602)
#define	DCM_ACQSHUTTERRIGHTVERTICALEDGE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1604)
#define	DCM_ACQSHUTTERUPPERHORIZONTALEDGE DCM_MAKETAG(DCM_GROUPACQUISITION,0x1606)
#define	DCM_ACQSHUTTERLOWERHORIZONTALEDGE DCM_MAKETAG(DCM_GROUPACQUISITION,0x1608)
#define	DCM_ACQCENTEROFCIRCULARSHUTTER	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1610)
#define	DCM_ACQRADIUSOFCIRCULARSHUTTER	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1612)
#define	DCM_ACQVERTICESOFPOLYGONALSHUTTER DCM_MAKETAG(DCM_GROUPACQUISITION,0x1620)
#define	DCM_ACQCOLLIMATORSHAPE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1700)
#define	DCM_ACQCOLLIMATORLEFTVERTICALEDGE DCM_MAKETAG(DCM_GROUPACQUISITION,0x1702)
#define	DCM_ACQCOLLIMATORRIGHTVERTICALEDGE DCM_MAKETAG(DCM_GROUPACQUISITION,0x1704)
#define	DCM_ACQCOLLIMATORUPPERHORIZONTALEDGE DCM_MAKETAG(DCM_GROUPACQUISITION,0x1706)
#define	DCM_ACQCOLLIMATORLOWERHORIZONTALEDGE DCM_MAKETAG(DCM_GROUPACQUISITION,0x1708)
#define	DCM_ACQCENTEROFCIRCULARCOLLIMATOR DCM_MAKETAG(DCM_GROUPACQUISITION,0x1710)
#define	DCM_ACQRADIUSOFCIRCULARCOLLIMATOR DCM_MAKETAG(DCM_GROUPACQUISITION,0x1712)
#define	DCM_ACQVERTICESOFPOLYGONALCOLLIMATOR DCM_MAKETAG(DCM_GROUPACQUISITION,0x1720)
#define	DCM_ACQACQUISITIONTIMESYNCHRONIZED DCM_MAKETAG(DCM_GROUPACQUISITION,0x1800)
#define	DCM_ACQTIMESOURCE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x1801)
#define	DCM_ACQTIMEDISTRIBUTIONPROTOCOL	DCM_MAKETAG(DCM_GROUPACQUISITION,0x1802)
#define	DCM_ACQCOMMENTS			DCM_MAKETAG(DCM_GROUPACQUISITION,0x4000)
#define	DCM_ACQOUTPUTPOWER		DCM_MAKETAG(DCM_GROUPACQUISITION,0x5000)
#define	DCM_ACQTRANSDUCERDATA		DCM_MAKETAG(DCM_GROUPACQUISITION,0x5010)
#define	DCM_ACQFOCUSDEPTH		DCM_MAKETAG(DCM_GROUPACQUISITION,0x5012)
#define	DCM_ACQPROCESSINGFUNCTION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x5020)
#define	DCM_ACQPOSTPROCESSINGFUNCTION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x5021)
#define	DCM_ACQMECHANICALINDEX		DCM_MAKETAG(DCM_GROUPACQUISITION,0x5022)
#define	DCM_ACQTHERMALINDEX		DCM_MAKETAG(DCM_GROUPACQUISITION,0x5024)
#define DCM_ACQCRANIALTHERMALINDEX	DCM_MAKETAG(DCM_GROUPACQUISITION,0x5026)
#define DCM_ACQSOFTTISSUETHERMALINDEX	DCM_MAKETAG(DCM_GROUPACQUISITION,0x5027)
#define DCM_ACQSOFTTISSUEFOCUSTHERMALINDEX DCM_MAKETAG(DCM_GROUPACQUISITION,0x5028)
#define DCM_ACQSOFTTISSUESURFACETHERMALINDEX DCM_MAKETAG(DCM_GROUPACQUISITION,0x5029)
#define	DCM_ACQDEPTHOFSCANFIELD		DCM_MAKETAG(DCM_GROUPACQUISITION,0x5050)
#define	DCM_ACQPATIENTPOSITION		DCM_MAKETAG(DCM_GROUPACQUISITION,0x5100)
#define	DCM_ACQVIEWPOSITION		DCM_MAKETAG(DCM_GROUPACQUISITION,0x5101)
#define DCM_ACQIMAGETRANSFORMATIONMATRIX DCM_MAKETAG(DCM_GROUPACQUISITION,0x5210)
#define DCM_ACQIMAGETRANSLATIONVECTOR	DCM_MAKETAG(DCM_GROUPACQUISITION,0x5212)
#define	DCM_ACQSENSITIVITY		DCM_MAKETAG(DCM_GROUPACQUISITION,0x6000)
#define	DCM_ACQUSREGIONSEQUENCE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x6011)
#define	DCM_ACQREGIONSPATIALFORMAT	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6012)
#define	DCM_ACQREGIONDATATYPE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x6014)
#define	DCM_ACQREGIONFLAGS		DCM_MAKETAG(DCM_GROUPACQUISITION,0x6016)
#define	DCM_ACQREGIONLOCATIONMINX0	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6018)
#define	DCM_ACQREGIONLOCATIONMINY0	DCM_MAKETAG(DCM_GROUPACQUISITION,0x601a)
#define	DCM_ACQREGIONLOCATIONMAXX1	DCM_MAKETAG(DCM_GROUPACQUISITION,0x601c)
#define	DCM_ACQREGIONLOCATIONMAXY1	DCM_MAKETAG(DCM_GROUPACQUISITION,0x601e)
#define	DCM_ACQREFERENCEPIXELX		DCM_MAKETAG(DCM_GROUPACQUISITION,0x6020)
#define	DCM_ACQREFERENCEPIXELY		DCM_MAKETAG(DCM_GROUPACQUISITION,0x6022)
#define	DCM_ACQPHYSICALUNITSXDIRECTION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6024)
#define	DCM_ACQPHYSICALUNITSYDIRECTION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6026)
#define	DCM_ACQREFPIXELPHYSICALVALUEX	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6028)
#define	DCM_ACQREFPIXELPHYSICALVALUEY	DCM_MAKETAG(DCM_GROUPACQUISITION,0x602a)
#define	DCM_ACQPHYSICALDELTAX		DCM_MAKETAG(DCM_GROUPACQUISITION,0x602c)
#define	DCM_ACQPHYSICALDELTAY		DCM_MAKETAG(DCM_GROUPACQUISITION,0x602e)
#define	DCM_ACQTRANSDUCERFREQUENCY	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6030)
#define DCM_ACQTRANSDUCERTYPE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x6031)
#define	DCM_ACQPULSEREPETITIONFREQ	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6032)
#define	DCM_ACQDOPPLERCORRECTIONANGLE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6034)
#define	DCM_ACQSTERRINGANGLE		DCM_MAKETAG(DCM_GROUPACQUISITION,0x6036)
#define	DCM_ACQDOPPLERSAMPLEVOLXPOS	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6038)
#define	DCM_ACQDOPPLERSAMPLEVOLYPOS	DCM_MAKETAG(DCM_GROUPACQUISITION,0x603a)
#define	DCM_ACQTMLINEPOSITIONX0		DCM_MAKETAG(DCM_GROUPACQUISITION,0x603c)
#define	DCM_ACQTMLINEPOSITIONY0		DCM_MAKETAG(DCM_GROUPACQUISITION,0x603e)
#define	DCM_ACQTMLINEPOSITIONX1		DCM_MAKETAG(DCM_GROUPACQUISITION,0x6040)
#define	DCM_ACQTMLINEPOSITIONY1		DCM_MAKETAG(DCM_GROUPACQUISITION,0x6042)
#define	DCM_ACQPIXELCOMPORGANIZATION	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6044)
#define	DCM_ACQPIXELCOMPMASK		DCM_MAKETAG(DCM_GROUPACQUISITION,0x6046)
#define	DCM_ACQPIXELCOMPRANGESTART	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6048)
#define	DCM_ACQPIXELCOMPRANGESTOP	DCM_MAKETAG(DCM_GROUPACQUISITION,0x604a)
#define	DCM_ACQPIXELCOMPPHYSUNITS	DCM_MAKETAG(DCM_GROUPACQUISITION,0x604c)
#define	DCM_ACQPIXELCOMPDATATYPE	DCM_MAKETAG(DCM_GROUPACQUISITION,0x604e)
#define	DCM_ACQNUMBERTABLEBREAKPOINTS	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6050)
#define	DCM_ACQTABLEXBREAKPOINTS	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6052)
#define	DCM_ACQTABLEYBREAKPOINTS	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6054)
#define	DCM_ACQNUMBEROFTABLEENTRIES	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6056)
#define	DCM_ACQTABLEOFPIXELVALUES	DCM_MAKETAG(DCM_GROUPACQUISITION,0x6058)
#define	DCM_ACQTABLEOFPARAMETERVALUES	DCM_MAKETAG(DCM_GROUPACQUISITION,0x605a)


/*  Define the elements for the RELATIONSHIP group (0x0020)
*/

#define	DCM_RELGROUPLENGTH		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0000)
#define	DCM_RELSTUDYINSTANCEUID		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x000d)
#define	DCM_RELSERIESINSTANCEUID	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x000e)
#define	DCM_RELSTUDYID			DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0010)
#define	DCM_RELSERIESNUMBER		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0011)
#define	DCM_RELACQUISITIONNUMBER	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0012)
#define	DCM_RELIMAGENUMBER		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0013)

/* The following attributes are retired (0014 - 0018) */
#define	DCM_RELISOTOPENUMBER		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0014)
#define	DCM_RELPHASENUMBER		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0015)
#define	DCM_RELINTERVALNUMBER		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0016)
#define	DCM_RELTIMESLOTNUMBER		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0017)
#define	DCM_RELANGLENUMBER		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0018)

#define	DCM_RELPATIENTORIENTATION	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0020)
#define	DCM_RELOVERLAYNUMBER		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0022)
#define	DCM_RELCURVENUMBER		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0024)
#define DCM_RELLOOKUPTABLENUMBER	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0026)
/* Retired 0030 */
#define	DCM_RELIMAGEPOSITION		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0030)
#define	DCM_RELIMAGEPOSITIONPATIENT	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0032)
/* Retired 0035 */
#define	DCM_RELIMAGEORIENTATION		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0035)
#define	DCM_RELIMAGEORIENTATIONPATIENT	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0037)
/* Retired 0050 */
#define	DCM_RELLOCATION			DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0050)
#define	DCM_RELFRAMEOFREFERENCEUID	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0052)
#define	DCM_RELLATERALITY		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0060)

/* Retired 0070, 0080 */
#define	DCM_RELIMAGEGEOMETRYTYPE	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0070)
#define	DCM_RELMASKINGIMAGE		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0080)

#define	DCM_RELTEMPORALPOSITIONID	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0100)
#define	DCM_RELNUMBERTEMPORALPOSITIONS	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0105)
#define	DCM_RELTEMPORALRESOLUTION	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x0110)
#define	DCM_RELSERIESINSTUDY		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1000)
#define	DCM_RELACQUISITIONSINSERIES	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1001)
#define	DCM_RELIMAGESINACQUISITION	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1002)
#define	DCM_RELACQUISITIONSINSTUDY	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1004)

/* Retired, 1020 */
#define	DCM_RELREFERENCE		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1020)
#define	DCM_RELPOSITIONREFINDICATOR	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1040)
#define	DCM_RELSLICELOCATION		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1041)
#define	DCM_RELOTHERSTUDYNUMBERS	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1070)
#define	DCM_RELNUMBERPATRELATEDSTUDIES	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1200)
#define	DCM_RELNUMBERPATRELATEDSERIES	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1202)
#define	DCM_RELNUMBERPATRELATEDIMAGES	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1204)
#define	DCM_RELNUMBERSTUDYRELATEDSERIES	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1206)
#define	DCM_RELNUMBERSTUDYRELATEDIMAGES	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1208)
#define	DCM_RELNUMBERSERIESRELATEDINST	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x1209)

#define	DCM_RELSOURCEIMAGEID		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x3100)
#define	DCM_RELMODIFYINGDEVICEID	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x3401)
#define	DCM_RELMODIFIEDIMAGEID		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x3402)
#define	DCM_RELMODIFIEDIMAGEDATE	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x3403)
#define	DCM_RELMODIFYINGDEVICEMFR	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x3404)
#define	DCM_RELMODIFIEDIMAGETIME	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x3405)
#define	DCM_RELMODIFIEDIMAGEDESCRIPTION	DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x3406)
#define	DCM_RELIMAGECOMMENTS		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x4000)
#define	DCM_RELORIGINALIMAGEID		DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x5000)
#define	DCM_RELORIGINALIMAGEIDNOMENCLATURE DCM_MAKETAG(DCM_GROUPRELATIONSHIP,0x5002)

/*  Define the elements for the IMAGE PRESENTATION group (0028)
*/
#define	DCM_IMGGROUPLENGTH		DCM_MAKETAG(DCM_GROUPIMAGE,0x0000)
#define	DCM_IMGSAMPLESPERPIXEL		DCM_MAKETAG(DCM_GROUPIMAGE,0x0002)
#define	DCM_IMGPHOTOMETRICINTERP	DCM_MAKETAG(DCM_GROUPIMAGE,0x0004)
#define	DCM_IMGIMAGEDIMENSIONS		DCM_MAKETAG(DCM_GROUPIMAGE,0x0005)
#define	DCM_IMGPLANARCONFIGURATION	DCM_MAKETAG(DCM_GROUPIMAGE,0x0006)
#define	DCM_IMGNUMBEROFFRAMES		DCM_MAKETAG(DCM_GROUPIMAGE,0x0008)
#define	DCM_IMGFRAMEINCREMENTPOINTER	DCM_MAKETAG(DCM_GROUPIMAGE,0x0009)
#define	DCM_IMGROWS			DCM_MAKETAG(DCM_GROUPIMAGE,0x0010)
#define	DCM_IMGCOLUMNS			DCM_MAKETAG(DCM_GROUPIMAGE,0x0011)
#define	DCM_IMGPLANES			DCM_MAKETAG(DCM_GROUPIMAGE,0x0012)
#define	DCM_IMGUSOUNDCOLORDATAPRESENT	DCM_MAKETAG(DCM_GROUPIMAGE,0x0014)
#define	DCM_IMGPIXELSPACING		DCM_MAKETAG(DCM_GROUPIMAGE,0x0030)
#define	DCM_IMGZOOMFACTOR		DCM_MAKETAG(DCM_GROUPIMAGE,0x0031)
#define	DCM_IMGZOOMCENTER		DCM_MAKETAG(DCM_GROUPIMAGE,0x0032)
#define	DCM_IMGPIXELASPECTRATIO		DCM_MAKETAG(DCM_GROUPIMAGE,0x0034)

/* Retired 0040, 0050 */
#define	DCM_IMGIMAGEFORMAT		DCM_MAKETAG(DCM_GROUPIMAGE,0x0040)
#define	DCM_IMGMANIPULATEDIMAGE		DCM_MAKETAG(DCM_GROUPIMAGE,0x0050)
#define	DCM_IMGCORRECTEDIMAGE		DCM_MAKETAG(DCM_GROUPIMAGE,0x0051)

/* Retired 0060 */
#define DCM_IMGCOMPRESSIONCODE		DCM_MAKETAG(DCM_GROUPIMAGE,0x0060)
#define	DCM_IMGBITSALLOCATED		DCM_MAKETAG(DCM_GROUPIMAGE,0x0100)
#define	DCM_IMGBITSSTORED		DCM_MAKETAG(DCM_GROUPIMAGE,0x0101)
#define	DCM_IMGHIGHBIT			DCM_MAKETAG(DCM_GROUPIMAGE,0x0102)
#define	DCM_IMGPIXELREPRESENTATION 	DCM_MAKETAG(DCM_GROUPIMAGE,0x0103)
#define	DCM_IMGSMALLESTPIXELVALUE 	DCM_MAKETAG(DCM_GROUPIMAGE,0x0104)
#define	DCM_IMGLARGESTPIXELVALUE  	DCM_MAKETAG(DCM_GROUPIMAGE,0x0105)
#define	DCM_IMGSMALLESTIMAGEPIXELVALUE	DCM_MAKETAG(DCM_GROUPIMAGE,0x0106)
#define	DCM_IMGLARGESTIMAGEPIXELVALUE	DCM_MAKETAG(DCM_GROUPIMAGE,0x0107)
#define	DCM_IMGSMALLESTPIXELVALUESERIES	DCM_MAKETAG(DCM_GROUPIMAGE,0x0108)
#define	DCM_IMGLARGESTPIXELVALUESERIES	DCM_MAKETAG(DCM_GROUPIMAGE,0x0109)
#define	DCM_IMGSMALLESTIMAGEPIXELVALUEPLANE DCM_MAKETAG(DCM_GROUPIMAGE, 0x0110)
#define	DCM_IMGLARGESTIMAGEPIXELVALUEPLANE DCM_MAKETAG(DCM_GROUPIMAGE, 0x0111)
#define	DCM_IMGPIXELPADDINGVALUE	DCM_MAKETAG(DCM_GROUPIMAGE,0x0120)
#define	DCM_IMGWAVEFORMPADDINGVALUE	DCM_MAKETAG(DCM_GROUPIMAGE,0x0122)

/* Retired 0200 */
#define DCM_IMGIMAGELOCATION		DCM_MAKETAG(DCM_GROUPIMAGE,0x0200)
#define	DCM_IMGPIXELINTENSITYRELATIONSHIP DCM_MAKETAG(DCM_GROUPIMAGE,0x1040)
#define	DCM_IMGWINDOWCENTER		DCM_MAKETAG(DCM_GROUPIMAGE,0x1050)
#define	DCM_IMGWINDOWWIDTH		DCM_MAKETAG(DCM_GROUPIMAGE,0x1051)
#define	DCM_IMGRESCALEINTERCEPT		DCM_MAKETAG(DCM_GROUPIMAGE,0x1052)
#define	DCM_IMGRESCALESLOPE		DCM_MAKETAG(DCM_GROUPIMAGE,0x1053)
#define DCM_IMGRESCALETYPE		DCM_MAKETAG(DCM_GROUPIMAGE,0x1054)
#define	DCM_IMGWINDOWCWEXPLANATION	DCM_MAKETAG(DCM_GROUPIMAGE,0x1055)

/* Retired 1080 */
#define	DCM_IMGGRAYSCALE		DCM_MAKETAG(DCM_GROUPIMAGE,0x1080)
#define	DCM_IMGRECOMMENDEDVIEWINGMODE	DCM_MAKETAG(DCM_GROUPIMAGE,0x1090)

/* Retired 1100 */
#define	DCM_IMGLUTDESCRIPTGRAY		DCM_MAKETAG(DCM_GROUPIMAGE,0x1100)
#define	DCM_IMGLUTDESCRIPTRED		DCM_MAKETAG(DCM_GROUPIMAGE,0x1101)
#define	DCM_IMGLUTDESCRIPTGREEN		DCM_MAKETAG(DCM_GROUPIMAGE,0x1102)
#define	DCM_IMGLUTDESCRIPTBLUE		DCM_MAKETAG(DCM_GROUPIMAGE,0x1103)
#define	DCM_IMGPALETTECOLORLUTUID	DCM_MAKETAG(DCM_GROUPIMAGE,0x1199)

/* Retired, 1200 */
#define	DCM_IMGLOOKUPDATAGRAY		DCM_MAKETAG(DCM_GROUPIMAGE,0x1200)
#define	DCM_IMGLOOKUPDATARED		DCM_MAKETAG(DCM_GROUPIMAGE,0x1201)
#define	DCM_IMGLOOKUPDATAGREEN		DCM_MAKETAG(DCM_GROUPIMAGE,0x1202)
#define	DCM_IMGLOOKUPDATABLUE		DCM_MAKETAG(DCM_GROUPIMAGE,0x1203)
#define	DCM_IMGSEGMENTEDREDLUTDATA	DCM_MAKETAG(DCM_GROUPIMAGE,0x1221)
#define	DCM_IMGSEGMENTEDGREENLUTDATA	DCM_MAKETAG(DCM_GROUPIMAGE,0x1222)
#define	DCM_IMGSEGMENTEDBLUELUTDATA	DCM_MAKETAG(DCM_GROUPIMAGE,0x1223)
#define DCM_IMGLOSSYIMAGECOMPRESSION	DCM_MAKETAG(DCM_GROUPIMAGE,0x2110)
#define DCM_IMGMODALITYLUTSEQUENCE	DCM_MAKETAG(DCM_GROUPIMAGE,0x3000)
#define DCM_IMGLUTDESCRIPTOR		DCM_MAKETAG(DCM_GROUPIMAGE,0x3002)
#define DCM_IMGLUTEXPLANATION		DCM_MAKETAG(DCM_GROUPIMAGE,0x3003)
#define DCM_IMGMODALITYLUTTYPE		DCM_MAKETAG(DCM_GROUPIMAGE,0x3004)
#define DCM_IMGLUTDATA			DCM_MAKETAG(DCM_GROUPIMAGE,0x3006)
#define DCM_IMGVOILUTSEQUENCE		DCM_MAKETAG(DCM_GROUPIMAGE,0x3010)

/* Retired, 4000 */
#define	DCM_IMGCOMMENTS			DCM_MAKETAG(DCM_GROUPIMAGE,0x4000)
#define	DCM_IMGBIPLANEACQSEQUENCE	DCM_MAKETAG(DCM_GROUPIMAGE,0x5000)
#define	DCM_IMGREPRESENTATIVEFRAMENUMBER DCM_MAKETAG(DCM_GROUPIMAGE,0x6010)
#define	DCM_IMGFRAMENUMBERSOFINTEREST	DCM_MAKETAG(DCM_GROUPIMAGE,0x6020)
#define	DCM_IMGFRAMEOFINTERESTDESCRIPTION DCM_MAKETAG(DCM_GROUPIMAGE,0x6022)
#define	DCM_IMGMASKPOINTER		DCM_MAKETAG(DCM_GROUPIMAGE,0x6030)
#define	DCM_IMGRWAVEPOINTER		DCM_MAKETAG(DCM_GROUPIMAGE,0x6040)
#define	DCM_IMGMASKSUBTRACTIONSEQ	DCM_MAKETAG(DCM_GROUPIMAGE,0x6100)
#define	DCM_IMGMASKOPERATION		DCM_MAKETAG(DCM_GROUPIMAGE,0x6101)
#define	DCM_IMGAPPLICABLEFRAMERANGE	DCM_MAKETAG(DCM_GROUPIMAGE,0x6102)
#define	DCM_IMGMASKFRAMENUMBERS		DCM_MAKETAG(DCM_GROUPIMAGE,0x6110)
#define	DCM_IMGCONTRASTFRAMEAVERAGING	DCM_MAKETAG(DCM_GROUPIMAGE,0x6112)
#define	DCM_IMGMASKSUBPIXELSHIFT	DCM_MAKETAG(DCM_GROUPIMAGE,0x6114)
#define	DCM_IMGTIDOFFSET		DCM_MAKETAG(DCM_GROUPIMAGE,0x6120)
#define	DCM_MASKOPERATIONEXPLANATION	DCM_MAKETAG(DCM_GROUPIMAGE,0x6190)


/*  Define the elements for the STUDY group (0x0032)
*/

#define DCM_SDYGROUPLENGTH		DCM_MAKETAG(DCM_GROUPSTUDY,0x0000)
#define	DCM_SDYSTATUSID			DCM_MAKETAG(DCM_GROUPSTUDY,0x000A)
#define	DCM_SDYPRIORITYID		DCM_MAKETAG(DCM_GROUPSTUDY,0x000C)
#define DCM_SDYIDISSUER			DCM_MAKETAG(DCM_GROUPSTUDY,0x0012)
#define DCM_SDYVERIFIEDDATE		DCM_MAKETAG(DCM_GROUPSTUDY,0x0032)
#define DCM_SDYVERIFIEDTIME		DCM_MAKETAG(DCM_GROUPSTUDY,0x0033)
#define DCM_SDYREADDATE			DCM_MAKETAG(DCM_GROUPSTUDY,0x0034)
#define DCM_SDYREADTIME			DCM_MAKETAG(DCM_GROUPSTUDY,0x0035)
#define DCM_SDYSCHEDULEDSTARTDATE	DCM_MAKETAG(DCM_GROUPSTUDY,0x1000)
#define DCM_SDYSCHEDULEDSTARTTIME	DCM_MAKETAG(DCM_GROUPSTUDY,0x1001)
#define DCM_SDYSCHEDULEDSTOPDATE	DCM_MAKETAG(DCM_GROUPSTUDY,0x1010)
#define DCM_SDYSCHEDULEDSTOPTIME	DCM_MAKETAG(DCM_GROUPSTUDY,0x1011)
#define DCM_SDYSCHEDULEDLOCATION	DCM_MAKETAG(DCM_GROUPSTUDY,0x1020)
#define DCM_SDYSCHEDULEDLOCATIONAETITLE	DCM_MAKETAG(DCM_GROUPSTUDY,0x1021)
#define DCM_SDYREASON			DCM_MAKETAG(DCM_GROUPSTUDY,0x1030)
#define DCM_SDYREQUESTINGPHYSICIAN	DCM_MAKETAG(DCM_GROUPSTUDY,0x1032)
#define DCM_SDYREQUESTINGSERVICE	DCM_MAKETAG(DCM_GROUPSTUDY,0x1033)
#define DCM_SDYARRIVALDATE		DCM_MAKETAG(DCM_GROUPSTUDY,0x1040)
#define DCM_SDYARRIVALTIME		DCM_MAKETAG(DCM_GROUPSTUDY,0x1041)
#define DCM_SDYCOMPLETIONDATE		DCM_MAKETAG(DCM_GROUPSTUDY,0x1050)
#define DCM_SDYCOMPLETIONTIME		DCM_MAKETAG(DCM_GROUPSTUDY,0x1051)
#define DCM_SDYSTUDYCOMPONENTSTATUSID	DCM_MAKETAG(DCM_GROUPSTUDY,0x1055)
#define DCM_SDYREQUESTEDPRODESCRIPTION	DCM_MAKETAG(DCM_GROUPSTUDY,0x1060)
#define DCM_SDYREQUESTEDPROCODESEQ	DCM_MAKETAG(DCM_GROUPSTUDY,0x1064)
#define DCM_SDYREQUESTEDCONTRASTAGENT	DCM_MAKETAG(DCM_GROUPSTUDY,0x1070)
#define DCM_SDYCOMMENTS			DCM_MAKETAG(DCM_GROUPSTUDY,0x4000)

/* Define the elements for the VISIT group (0x0038)
*/

#define DCM_VISGROUPLENGTH 		DCM_MAKETAG(DCM_GROUPVISIT,0x0000)
#define DCM_VISREFERENCEDPATALIASSEQ	DCM_MAKETAG(DCM_GROUPVISIT,0x0004)
#define DCM_VISSTATUSID 		DCM_MAKETAG(DCM_GROUPVISIT,0x0008)
#define DCM_VISADMISSIONID 		DCM_MAKETAG(DCM_GROUPVISIT,0x0010)
#define DCM_VISISSUEROFADMISSIONID 	DCM_MAKETAG(DCM_GROUPVISIT,0x0011)
#define DCM_VISROUTEOFADMISSION 	DCM_MAKETAG(DCM_GROUPVISIT,0x0016)
#define DCM_VISSCHEDULEDADMISSIONDATE 	DCM_MAKETAG(DCM_GROUPVISIT,0x001a)
#define DCM_VISSCHEDULEDADMISSIONTIME 	DCM_MAKETAG(DCM_GROUPVISIT,0x001b)
#define DCM_VISSCHEDULEDDISCHARGEDATE 	DCM_MAKETAG(DCM_GROUPVISIT,0x001c)
#define DCM_VISSCHEDULEDDISCHARGETIME 	DCM_MAKETAG(DCM_GROUPVISIT,0x001d)
#define DCM_VISSCHEDULEDPATINSTRESIDENCE DCM_MAKETAG(DCM_GROUPVISIT,0x001e)
#define DCM_VISADMITTINGDATE 		DCM_MAKETAG(DCM_GROUPVISIT,0x0020)
#define DCM_VISADMITTINGTIME 		DCM_MAKETAG(DCM_GROUPVISIT,0x0021)
#define DCM_VISDISCHARGEDATE 		DCM_MAKETAG(DCM_GROUPVISIT,0x0030)
#define DCM_VISDISCHARGETIME 		DCM_MAKETAG(DCM_GROUPVISIT,0x0032)
#define DCM_VISDISCHARGEDIAGDESCRIPTION DCM_MAKETAG(DCM_GROUPVISIT,0x0040)
#define DCM_VISDISCHARGEDIAGNOSISCODESEQ	DCM_MAKETAG(DCM_GROUPVISIT,0x0044)
#define DCM_VISSPECIALNEEDS 		DCM_MAKETAG(DCM_GROUPVISIT,0x0050)
#define DCM_VISCURRENTPATIENTLOCATION 	DCM_MAKETAG(DCM_GROUPVISIT,0x0300)
#define DCM_VISPATIENTSINSTRESIDENCE 	DCM_MAKETAG(DCM_GROUPVISIT,0x0400)
#define DCM_VISPATIENTSTATE 		DCM_MAKETAG(DCM_GROUPVISIT,0x0500)
#define DCM_VISCOMMENTS			DCM_MAKETAG(DCM_GROUPVISIT,0x4000)

/* Define elements for the Procedure Step group (0040)
*/

#define	DCM_PRCGROUPLENGTH		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0000)
#define	DCM_PRCSCHEDULEDSTATIONAETITLE	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0001)
#define	DCM_PRCSCHEDULEDPROCSTEPSTARTDATE 	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0002)
#define	DCM_PRCSCHEDULEDPROCSTEPSTARTTIME 	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0003)
#define	DCM_PRCSCHEDULEDPROCSTEPENDDATE		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0004)
#define	DCM_PRCSCHEDULEDPROCSTEPENDTIME		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0005)
#define	DCM_PRCSCHEDULEDPERFORMINGPHYSNAME 	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0006)
#define	DCM_PRCSCHEDULEDPROCSTEPDESCRIPTION 	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0007)
#define	DCM_PRCSCHEDULEDACTIONITEMCODESEQ	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0008)
#define	DCM_PRCSCHEDULEDPROCSTEPID	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0009)
#define	DCM_PRCSCHEDULEDSTATIONNAME	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0010)
#define	DCM_PRCSCHEDULEDPROCSTEPLOCATION	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0011)
#define	DCM_PRCPREMEDICATION		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0012)
#define DCM_PRCSTATUS			DCM_MAKETAG(DCM_GRPPROCEDURE,0x0020)
#define	DCM_PRCSCHEDULEDPROCSTEPSEQ	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0100)
#define DCM_PRCREFSTANDALONESOPSEQ      DCM_MAKETAG(DCM_GRPPROCEDURE,0x0220)
#define DCM_PRCPERFORMEDSTATIONAET      DCM_MAKETAG(DCM_GRPPROCEDURE,0x0241)
#define DCM_PRCPERFORMEDSTATIONNAME     DCM_MAKETAG(DCM_GRPPROCEDURE,0x0242)
#define DCM_PRCPERFORMEDLOCATION 	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0243)
#define DCM_PRCPPSSTARTDATE		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0244)
#define DCM_PRCPPSSTARTTIME		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0245)
#define DCM_PRCPPSENDDATE		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0250)
#define DCM_PRCPPSENDTIME		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0251)
#define DCM_PRCPPSSTATUS		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0252)
#define DCM_PRCPPSID			DCM_MAKETAG(DCM_GRPPROCEDURE,0x0253)
#define DCM_PRCPPSDESCRIPTION		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0254)
#define DCM_PRCPPTYPEDESCRIPTION	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0255)
#define DCM_PRCPERFORMEDAISEQUENCE	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0260)
#define DCM_PRCSCHEDSTEPATTRSEQ		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0270)
#define DCM_PRCREQUESTATTRIBUTESSEQ	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0275)
#define DCM_PRCCOMMENTSPPS		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0280)
#define DCM_PRCQUANTITYSEQ		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0293)
#define DCM_PRCQUANTITY			DCM_MAKETAG(DCM_GRPPROCEDURE,0x0294)
#define DCM_PRCMEASURINGUNITSSEQ	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0295)
#define DCM_PRCBILLINGITEMSEQ		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0296)
#define DCM_PRCTOTALTIMEFLUOROSCOPY	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0300)
#define DCM_PRCTOTALNUMBEREXPOSURES	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0301)
#define DCM_PRCENTRANCEDOSE		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0302)
#define DCM_PRCEXPOSEDAREA		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0303)
#define DCM_PRCDISTANCESOURCEENTRANCE	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0306)
#define DCM_PRCCOMMENTSRADIATIONDOSE	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0310)
#define DCM_PRCBILLINGPROCEDURESTEPSEQ	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0320)
#define DCM_PRCFILMCONSUMPTIONSEQ	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0321)
#define DCM_PRCBILLINGSUPPLIESDEVICESEQ DCM_MAKETAG(DCM_GRPPROCEDURE,0x0324)
#define DCM_PRCREFERENCEDPPS		DCM_MAKETAG(DCM_GRPPROCEDURE,0x0330)
#define DCM_PRCPERFORMEDSERIESSEQ	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0340)
#define	DCM_PRCCOMMENTSONSCHEDULEDPROCSTEP	DCM_MAKETAG(DCM_GRPPROCEDURE,0x0400)
#define	DCM_PRCREQUESTEDPROCEDUREID	DCM_MAKETAG(DCM_GRPPROCEDURE,0x1001)
#define	DCM_PRCREASONFORREQUESTEDPROC	DCM_MAKETAG(DCM_GRPPROCEDURE,0x1002)
#define	DCM_PRCREQUESTEDPROCPRIORITY	DCM_MAKETAG(DCM_GRPPROCEDURE,0x1003)
#define	DCM_PRCPATIENTTRANSPORTARRANGEMENTS	DCM_MAKETAG(DCM_GRPPROCEDURE,0x1004)
#define	DCM_PRCREQUESTEDPROCLOCATION	DCM_MAKETAG(DCM_GRPPROCEDURE,0x1005)
#define	DCM_PRCPLACERORDERNUMBERPROC	DCM_MAKETAG(DCM_GRPPROCEDURE,0x1006)
#define	DCM_PRCFILLERORDERNUMBERPROC	DCM_MAKETAG(DCM_GRPPROCEDURE,0x1007)
#define	DCM_PRCCONFIDENTIALITYCODE	DCM_MAKETAG(DCM_GRPPROCEDURE,0x1008)
#define	DCM_PRCREPORTINGPRIORITY	DCM_MAKETAG(DCM_GRPPROCEDURE,0x1009)
#define	DCM_PRCNAMESINTENDEDRECIPIENTSRESULTS	DCM_MAKETAG(DCM_GRPPROCEDURE,0x1010)
#define	DCM_PRCREQUESTEDPROCCOMMENTS	DCM_MAKETAG(DCM_GRPPROCEDURE,0x1400)
#define	DCM_PRCREASONFORIMAGINGSERVICEREQ	DCM_MAKETAG(DCM_GRPPROCEDURE,0x2001)
#define	DCM_PRCISSUEDATEIMAGINGSERVICEREQ	DCM_MAKETAG(DCM_GRPPROCEDURE,0x2004)
#define	DCM_PRCISSUETIMEIMAGINGSERVICEREQ	DCM_MAKETAG(DCM_GRPPROCEDURE,0x2005)
#define	DCM_PRCPLACERORDERNUMBERIMAGINGSRVREQ	DCM_MAKETAG(DCM_GRPPROCEDURE,0x2006)
#define	DCM_PRCFILLERORDERNUMBERIMAGINGSRVREQ	DCM_MAKETAG(DCM_GRPPROCEDURE,0x2007)
#define	DCM_PRCORDERENTEREDBY		DCM_MAKETAG(DCM_GRPPROCEDURE,0x2008)
#define	DCM_PRCORDERENTERERSLOCATION	DCM_MAKETAG(DCM_GRPPROCEDURE,0x2009)
#define	DCM_PRCORDERCALLBACKPHONENUMBER	DCM_MAKETAG(DCM_GRPPROCEDURE,0x2010)
#define	DCM_PRCIMAGINGSERVICEREQCOMMENTS	DCM_MAKETAG(DCM_GRPPROCEDURE,0x2400)
#define	DCM_PRCCONFIDIENTIALITYCONSTRAINTPATIENTDATADES DCM_MAKETAG(DCM_GRPPROCEDURE,0x3001)

/* Define elements for the Procedure Step group (0040), General Purpose
 * Scheduled/Performed Procedure Step Info
*/

#define DCM_PRCGPSPSSTATUS			DCM_MAKETAG(0x0040, 0x4001)
#define DCM_PRCGPPPSSTATUS			DCM_MAKETAG(0x0040, 0x4002)
#define DCM_PRCGPSPSPRIORITY			DCM_MAKETAG(0x0040, 0x4003)
#define DCM_PRCSCHEDULEDPROCAPPCODESEQ		DCM_MAKETAG(0x0040, 0x4004)
#define DCM_PRCGPSPSSTARTDATETIME		DCM_MAKETAG(0x0040, 0x4005)
#define DCM_PRCGPSPSMULTIPLECOPIESFLAG		DCM_MAKETAG(0x0040, 0x4006)
#define DCM_PRCPERFORMEDPROCAPPCODESEQ		DCM_MAKETAG(0x0040, 0x4007)
#define DCM_PRCHUMANPERFORMERCODESEQ		DCM_MAKETAG(0x0040, 0x4009)
#define DCM_PRCGPSPSEXPECTEDCOMPLETEDATETIME	DCM_MAKETAG(0x0040, 0x4011)
#define DCM_PRCRESULTINGGPPERFPROCSTEPSEQ	DCM_MAKETAG(0x0040, 0x4015)
#define DCM_PRCREFERENCEDGPSCHEDPROCSTEPSEQ	DCM_MAKETAG(0x0040, 0x4016)
#define DCM_PRCSCHEDWORKITEMCODESEQ		DCM_MAKETAG(0x0040, 0x4018)
#define DCM_PRCPERFORMEDWORKITEMCODESEQ		DCM_MAKETAG(0x0040, 0x4019)
#define DCM_PRCINPUTAVAILFLAG			DCM_MAKETAG(0x0040, 0x4020)
#define DCM_PRCINPUTINFOSEQ			DCM_MAKETAG(0x0040, 0x4021)
#define DCM_PRCRELEVANTINFOSEQ			DCM_MAKETAG(0x0040, 0x4022)
#define DCM_PRCREFERENCEDGPSPSTRANSACTIONUID	DCM_MAKETAG(0x0040, 0x4023)
#define DCM_PRCSCHEDSTATIONNAMECODESEQ		DCM_MAKETAG(0x0040, 0x4025)
#define DCM_PRCSCHEDSTATIONCLASSCODESEQ		DCM_MAKETAG(0x0040, 0x4026)
#define DCM_PRCSCHEDSTATIONLOCCODESEQ		DCM_MAKETAG(0x0040, 0x4027)
#define DCM_PRCPERFORMEDSTATIONNAMECODESEQ	DCM_MAKETAG(0x0040, 0x4028)
#define DCM_PRCPERFORMEDSTATIONCLASSCODESEQ	DCM_MAKETAG(0x0040, 0x4029)
#define DCM_PRCPERFORMEDSTATIONLOCCODESEQ	DCM_MAKETAG(0x0040, 0x4030)
#define DCM_PRCREQSUBSWORKITEMCODESEQ		DCM_MAKETAG(0x0040, 0x4031)
#define DCM_PRCNONDICOMOUTPUTCODESEQ		DCM_MAKETAG(0x0040, 0x4032)
#define DCM_PRCOUTPUTINFOSEQ			DCM_MAKETAG(0x0040, 0x4033)
#define DCM_PRCSCHEDHUMANPERFORMERSSEQ		DCM_MAKETAG(0x0040, 0x4034)
#define DCM_PRCACTUALHUMANPERFORMERSSEQ		DCM_MAKETAG(0x0040, 0x4035)
#define DCM_PRCHUMANPERFORMERSORG		DCM_MAKETAG(0x0040, 0x4036)
#define DCM_PRCHUMANPERFORMERSNAME		DCM_MAKETAG(0x0040, 0x4037)

#define DCM_PRCRELATIONSHIPTYPE		DCM_MAKETAG(0x0040, 0xa010)
#define DCM_PRCVERIFYINGORGANIZATION	DCM_MAKETAG(0x0040, 0xa027)
#define DCM_PRCVERIFICATIONDATETIME	DCM_MAKETAG(0x0040, 0xa030)
#define DCM_PRCOBSERVATIONDATETIME	DCM_MAKETAG(0x0040, 0xa032)
#define DCM_PRCVALUETYPE		DCM_MAKETAG(0x0040, 0xa040)
#define DCM_PRCCONCEPTNAMECODESEQ	DCM_MAKETAG(0x0040, 0xa043)
#define DCM_PRCCONTINUITYOFCONTENT	DCM_MAKETAG(0x0040, 0xa050)
#define DCM_PRCVERIFYINGOBSERVERSEQ	DCM_MAKETAG(0x0040, 0xa073)
#define DCM_PRCVERIFYINGOBSERVERNAME	DCM_MAKETAG(0x0040, 0xa075)
#define DCM_PRCVERIFYINGOBSERVERIDCODESEQ	DCM_MAKETAG(0x0040, 0xa088) 
#define DCM_PRCDATETIME			DCM_MAKETAG(0x0040, 0xa120)
#define DCM_PRCUID			DCM_MAKETAG(0x0040, 0xa124)
#define DCM_PRCTEXTVALUE		DCM_MAKETAG(0x0040, 0xa160)
#define DCM_PRCMEASUREDVALUESEQ		DCM_MAKETAG(0x0040, 0xa300)
#define DCM_PRCPREDECESSORDOCUMENTSSEQ	DCM_MAKETAG(0x0040, 0xa360)
#define DCM_PRCREFERENCEDDOCUMENTSSEQ	DCM_MAKETAG(0x0040, 0xa370)
#define DCM_PRCPERFORMEDPROCEDURECODESEQ	DCM_MAKETAG(0x0040, 0xa372) 
#define DCM_PRCCURRENTREQPROCEVIDENCESEQ	DCM_MAKETAG(0x0040, 0xa375)
#define DCM_PRCPERTINENTOTHEREVIDENCESEQ	DCM_MAKETAG(0x0040, 0xa385)
#define DCM_PRCCOMPLETIONFLAG		DCM_MAKETAG(0x0040, 0xa491)
#define DCM_PRCCOMPLETIONFLAGDESCR	DCM_MAKETAG(0x0040, 0xa492)
#define DCM_PRCVERIFICATIONFLAG		DCM_MAKETAG(0x0040, 0xa493)
#define DCM_PRCCONTENTTEMPLATESEQ	DCM_MAKETAG(0x0040, 0xa504)
#define DCM_PRCIDENTIFICALDOCUMENTSSEQ	DCM_MAKETAG(0x0040, 0xa525)
#define DCM_PRCCONTENTSEQ		DCM_MAKETAG(0x0040, 0xa730)
#define DCM_PRCTEMPLATEIDENTIFIER	DCM_MAKETAG(0x0040, 0xdb00)
#define DCM_PRCTEMPLATEVERSION		DCM_MAKETAG(0x0040, 0xdb06)
#define DCM_PRCTEMPLATELOCALVERSION	DCM_MAKETAG(0x0040, 0xdb07)
#define DCM_PRCTEMPLATEEXTENSIONFLAG	DCM_MAKETAG(0x0040, 0xdb0b)
#define DCM_PRCTEMPLATEEXTENSIONORGUID	DCM_MAKETAG(0x0040, 0xdb0c)
#define DCM_PRCTEMPLATEEXTENSIONCREATORUID	DCM_MAKETAG(0x0040, 0xdb0d) 
#define DCM_PRCREFERENCEDCONTENTITEMID	DCM_MAKETAG(0x0040, 0xdb73)

/* Define the elements for the DEVICE group (0x0050)
*/

#define DCM_DEVGROUPLENGTH 		DCM_MAKETAG(DCM_GROUPDEVICE,0x0000)
#define DCM_DEVCALIBRATIONOBJECT	DCM_MAKETAG(DCM_GROUPDEVICE,0x0004)
#define	DCM_DEVDEVICESEQUENCE		DCM_MAKETAG(DCM_GROUPDEVICE,0x0010)
#define	DCM_DEVDEVICELENGTH		DCM_MAKETAG(DCM_GROUPDEVICE,0x0014)
#define	DCM_DEVDEVICEDIAMETER		DCM_MAKETAG(DCM_GROUPDEVICE,0x0016)
#define	DCM_DEVDEVICEDIAMETERUNITS	DCM_MAKETAG(DCM_GROUPDEVICE,0x0017)
#define	DCM_DEVDEVICEVOLUME		DCM_MAKETAG(DCM_GROUPDEVICE,0x0018)
#define	DCM_DEVINTERMARKERDISTANCE	DCM_MAKETAG(DCM_GROUPDEVICE,0x0019)
#define	DCM_DEVDEVICEDESCRIPTION	DCM_MAKETAG(DCM_GROUPDEVICE,0x0020)

/* define the elements of the RESULTS (0x4008) group
*/

#define DCM_RESGROUPLENGTH		DCM_MAKETAG(DCM_GROUPRESULTS,0x0000)
#define DCM_RESID			DCM_MAKETAG(DCM_GROUPRESULTS,0x0040)
#define DCM_RESIDISSUER			DCM_MAKETAG(DCM_GROUPRESULTS,0x0042)
#define DCM_RESREFERENCEDINTERPSEQ	DCM_MAKETAG(DCM_GROUPRESULTS,0x0050)
#define DCM_RESINTERPRECORDEDDATE	DCM_MAKETAG(DCM_GROUPRESULTS,0x0100)
#define DCM_RESINTERPRECORDEDTIME	DCM_MAKETAG(DCM_GROUPRESULTS,0x0101)
#define DCM_RESINTERPRECORDER		DCM_MAKETAG(DCM_GROUPRESULTS,0x0102)
#define DCM_RESREFERENCETORECORDEDSOUND	DCM_MAKETAG(DCM_GROUPRESULTS,0x0103)
#define DCM_RESINTERPTRANSCRIPTIONDATE	DCM_MAKETAG(DCM_GROUPRESULTS,0x0108)
#define DCM_RESINTERPTRANSCRIPTIONTIME	DCM_MAKETAG(DCM_GROUPRESULTS,0x0109)
#define DCM_RESINTERPTRANSCRIBER	DCM_MAKETAG(DCM_GROUPRESULTS,0x010a)
#define DCM_RESINTERPTEXT		DCM_MAKETAG(DCM_GROUPRESULTS,0x010b)
#define DCM_RESINTERPAUTHOR		DCM_MAKETAG(DCM_GROUPRESULTS,0x010c)
#define DCM_RESINTERPAPPROVERSEQUENCE	DCM_MAKETAG(DCM_GROUPRESULTS,0x0111)
#define DCM_RESINTERPAPPROVALDATE	DCM_MAKETAG(DCM_GROUPRESULTS,0x0112)
#define DCM_RESINTERPAPPROVALTIME	DCM_MAKETAG(DCM_GROUPRESULTS,0x0113)
#define DCM_RESPHYSICIANAPPROVINGINTERP	DCM_MAKETAG(DCM_GROUPRESULTS,0x0114)
#define DCM_RESDIAGNOSIS		DCM_MAKETAG(DCM_GROUPRESULTS,0x0115)
#define DCM_RESDIAGNOSISCODESEQ		DCM_MAKETAG(DCM_GROUPRESULTS,0x0117)
#define DCM_RESDISTRIBUTIIONLISTSEQUENCE DCM_MAKETAG(DCM_GROUPRESULTS,0x0118)
#define DCM_RESDISTRIBUTIONNAME		DCM_MAKETAG(DCM_GROUPRESULTS,0x0119)
#define DCM_RESDISTRIBUTIONADDRESS	DCM_MAKETAG(DCM_GROUPRESULTS,0x011a)
#define DCM_RESINTERPID			DCM_MAKETAG(DCM_GROUPRESULTS,0x0200)
#define DCM_RESINTERPIDISSUER		DCM_MAKETAG(DCM_GROUPRESULTS,0x0202)
#define DCM_RESINTERPTYPEID		DCM_MAKETAG(DCM_GROUPRESULTS,0x0210)
#define DCM_RESINTERPSTATUSID		DCM_MAKETAG(DCM_GROUPRESULTS,0x0212)
#define DCM_RESIMPRESSIONS		DCM_MAKETAG(DCM_GROUPRESULTS,0x0300)
#define DCM_RESCOMMENTS			DCM_MAKETAG(DCM_GROUPRESULTS,0x4000)

/* Define the elements for the  Curve group (50xx) */
#define DCM_CURVEGROUPLENGTH		DCM_MAKETAG(DCM_GROUPCURVE, 0x0000)
#define DCM_CURVEDIMENSIONS		DCM_MAKETAG(DCM_GROUPCURVE, 0x0005)
#define DCM_CURVENUMBEROFPOINTS		DCM_MAKETAG(DCM_GROUPCURVE, 0x0010)
#define DCM_CURVETYPEOFDATA		DCM_MAKETAG(DCM_GROUPCURVE, 0x0020)
#define DCM_CURVEDESCRIPTION		DCM_MAKETAG(DCM_GROUPCURVE, 0x0022)
#define DCM_CURVEAXISUNITS		DCM_MAKETAG(DCM_GROUPCURVE, 0x0030)
#define DCM_CURVEAXISLABELS		DCM_MAKETAG(DCM_GROUPCURVE, 0x0040)
#define DCM_CURVEDATAVALUEREPRESENTATION   DCM_MAKETAG(DCM_GROUPCURVE, 0x0103)
#define DCM_CURVEMINCOORDINATEVALUE	DCM_MAKETAG(DCM_GROUPCURVE, 0x0104)
#define DCM_CURVEMAXCOORDINATEVALUE	DCM_MAKETAG(DCM_GROUPCURVE, 0x0105)
#define DCM_CURVERANGE			DCM_MAKETAG(DCM_GROUPCURVE, 0x0106)
#define DCM_CURVEDATADESCRIPTOR		DCM_MAKETAG(DCM_GROUPCURVE, 0x0110)
#define DCM_CURVECOORDINATESTARTVALUE	DCM_MAKETAG(DCM_GROUPCURVE, 0x0112)
#define DCM_CURVECOORDINATESTEPVALUE	DCM_MAKETAG(DCM_GROUPCURVE, 0x0114)
#define DCM_CURVEAUDIOTYPE		DCM_MAKETAG(DCM_GROUPCURVE, 0x2000)
#define DCM_CURVEAUDIOSAMPLEFORMAT	DCM_MAKETAG(DCM_GROUPCURVE, 0x2002)
#define DCM_CURVENUMBEROFCHANNELS	DCM_MAKETAG(DCM_GROUPCURVE, 0x2004)
#define DCM_CURVENUMBEROFSAMPLES	DCM_MAKETAG(DCM_GROUPCURVE, 0x2006)
#define DCM_CURVESAMPLERATE		DCM_MAKETAG(DCM_GROUPCURVE, 0x2008)
#define DCM_CURVETOTALTIME		DCM_MAKETAG(DCM_GROUPCURVE, 0x200A)
#define DCM_CURVEAUDIOSAMPLEDATA	DCM_MAKETAG(DCM_GROUPCURVE, 0x200C)
#define DCM_CURVEAUDIOCOMMENTS		DCM_MAKETAG(DCM_GROUPCURVE, 0x200E)
#define DCM_CURVELABEL			DCM_MAKETAG(DCM_GROUPCURVE, 0x2500)
#define DCM_CURVEREFOVERLAYSEQUENCE	DCM_MAKETAG(DCM_GROUPCURVE, 0x2600)
#define DCM_CURVEREFOVERLAYGROUP	DCM_MAKETAG(DCM_GROUPCURVE, 0x2610)
#define DCM_CURVEDATA			DCM_MAKETAG(DCM_GROUPCURVE, 0x3000)

/*  Define the elements for the NM Image group.  (0054) */
#define DCM_NMIGROUPLENGTH		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0000)
#define DCM_NMIENERGYWINDOWVECTOR	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0010)
#define DCM_NMINUMBEROFENERGYWINDOWS	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0011)
#define DCM_NMIENERGYWINDOWINFOSEQ	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0012)
#define DCM_NMIENERGYWINDOWRANGESEQ	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0013)
#define DCM_NMIENERGYWINDOWLOWERLIMIT	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0014)
#define DCM_NMIENERGYWINDOWUPPERLIMIT	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0015)
#define	DCM_NMIRADIOPHARMINFOSEQ	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0016)
#define DCM_NMIRESIDUALSYRINGECOUNTS	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0017)
#define	DCM_NMIENERGYWINDOWNAME		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0018)
#define DCM_NMIDETECTORVECTOR		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0020)
#define DCM_NMINUMBEROFDETECTORS	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0021)
#define DCM_NMIDETECTORINFOSEQUENCE	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0022)
#define DCM_NMIPHASEVECTOR		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0030)
#define DCM_NMINUMBEROFPHASES		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0031)
#define DCM_NMIPHASEINFOSEQUENCE	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0032)
#define DCM_NMINUMBEROFFRAMESINPHASE	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0033)
#define DCM_NMIPHASEDELAY		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0036)
#define DCM_NMIPAUSEBETWEENFRAMES	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0038)
#define DCM_NMIROTATIONVECTOR		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0050)
#define DCM_NMINUMBEROFROTATIONS	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0051)
#define DCM_NMIROTATIONINFOSEQUENCE	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0052)
#define DCM_NMINUMBEROFFRAMESINROTATION	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0053)
#define DCM_NMIRRINTERVALVECTOR		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0060)
#define DCM_NMINUMBEROFRRINTERVALS	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0061)
#define DCM_NMIGATEDINFOSEQUENCE	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0062)
#define	DCM_NMIDATAINFORMATIONSEQUENCE	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0063)
#define DCM_NMITIMESLOTVECTOR		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0070)
#define DCM_NMINUMBEROFTIMESLOTS	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0071)
#define DCM_NMITIMESLOTINFOSEQUENCE	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0072)
#define DCM_NMITIMESLOTTIME		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0073)
#define DCM_NMISLICEVECTOR		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0080)
#define DCM_NMINUMBEROFSLICES		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0081)
#define DCM_NMIANGULARVIEWVECTOR	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0090)
#define DCM_NMITIMESLICEVECTOR		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0100)
#define	DCM_NMINUMBEROFTIMESLICES	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0101)
#define DCM_NMISTARTANGLE		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0200)
#define DCM_NMITYPEOFDETECTORMOTION	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0202)
#define DCM_NMITRIGGERVECTOR		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0210)
#define DCM_NMINUMBEROFTRIGGERSINPHASE	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0211)
#define	DCM_NMIVIEWCODESEQUENCE		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0220)
#define	DCM_NMIVIEWANGULATIONMODIFIERCODESEQ DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0222)
#define DCM_NMIRADIONUCLIDECODESEQUENCE	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0300)
#define DCM_NMIRADIOPHARMROUTECODESEQUENCE DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0302)
#define DCM_NMIRADIOPHARMCODESEQUENCE	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0304)
#define DCM_NMICALIBRATIONDATASEQUENCE	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0306)
#define	DCM_NMIENERGYWINDOWNUMBER	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0308)
#define DCM_NMIIMAGEID 			DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0400)
#define	DCM_NMIPATIENTORIENTATIONCODESEQ	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0410)
#define	DCM_NMIPATIENTORIENTATIONMODIFIERCODESEQ DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0412)
#define	DCM_NMIPATIENTGANTRYRELATIONSHIPCODESEQ DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x0414)
#define	DCM_NMISERIESTYPE		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1000)
#define	DCM_NMIUNITS			DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1001)
#define	DCM_NMICOUNTSSOURCE		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1002)
#define	DCM_NMIREPROJECTIONMETHOD	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1004)
#define	DCM_NMIRANDOMSCORRECTIONMETHOD	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1100)
#define	DCM_NMIATTENUATIONCORRECTIONMETHOD DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1101)
#define	DCM_NMIDECAYCORRECTION		DCM_MAKETAG(DCM_GROUPNMIMAGE,0x1102)
#define	DCM_NMIRECONSTRUCTIONMETHOD	DCM_MAKETAG(DCM_GROUPNMIMAGE,0x1103)
#define	DCM_NMIDETECTORLINESRESPONSEUSED DCM_MAKETAG(DCM_GROUPNMIMAGE,0x1104)
#define	DCM_NMISCATTERCORRECTIONMETHOD	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1105)
#define	DCM_NMIAXIALACCEPTANCE		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1200)
#define	DCM_NMIAXIALMASH		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1201)
#define	DCM_NMITRANSVERSEMASH		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1202)
#define	DCM_NMIDETECTORELEMENTSIZE	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1203)
#define	DCM_NMICOINCIDENCEWINDOWWIDTH	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1210)
#define	DCM_NMISECONDARYCOUNTSTYPE	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1220)
#define	DCM_NMIFRAMEREFERENCETIME	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1300)
#define	DCM_NMIPRIMARYCOUNTSACCUMULATED	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1310)
#define	DCM_NMISECONDARYCOUNTSACCUMULATED DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1311)
#define	DCM_NMISLICESENSITIVITYFACTOR	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1320)
#define	DCM_NMIDECAYFACTOR		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1321)
#define	DCM_NMIDOSECALIBRATIONFACTOR	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1322)
#define	DCM_NMISCATTERFRACTIONFACTOR	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1323)
#define	DCM_NMIDEADTIMEFACTOR		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1324)
#define	DCM_NMIIMAGEINDEX		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1330)
#define	DCM_NMICOUNTSINCLUDED		DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1400)
#define	DCM_NMIDEADTIMECORRECTIONFLAG	DCM_MAKETAG(DCM_GROUPNMIMAGE, 0x1401)

/*  Define the elements for the OVERLAY group. (60xx) */
#define DCM_OLYGROUPLENGTH		DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0000)
#define DCM_OLYROWS			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0010)
#define DCM_OLYCOLUMNS			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0011)
#define	DCM_OLYPLANES			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0012)
#define DCM_OLYNUMBEROFFRAMESINOVERLAY	DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0015)
#define DCM_OLYOVERLAYDESCRIPTION	DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0022)
#define DCM_OLYTYPE			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0040)
#define	DCM_OLYSUBTYPE			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0045)
#define DCM_OLYORIGIN			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0050)
#define	DCM_OLYIMAGEFRAMEORIGIN		DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0051)
#define	DCM_OLYOVERLAYPLANEORIGIN	DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0052)

/* Retired, 0060 */
#define	DCM_OLYCOMPRESSIONCODE		DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0060)
#define DCM_OLYBITSALLOCATED		DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0100)
#define DCM_OLYBITPOSITION		DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0102)

/* Retired, 00110, 0200) */
#define	DCM_OLYOVERLAYFORMAT		DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0110)
#define	DCM_OLYOVERLAYLOCATION		DCM_MAKETAG(DCM_GROUPOVERLAY, 0x0200)
#define DCM_OLYDESCRIPTORGRAY		DCM_MAKETAG(DCM_GROUPOVERLAY, 0x1100)
#define DCM_OLYDESCRIPTORRED		DCM_MAKETAG(DCM_GROUPOVERLAY, 0x1101)
#define DCM_OLYDESCRIPTORGREEN		DCM_MAKETAG(DCM_GROUPOVERLAY, 0x1102)
#define DCM_OLYDESCRIPTORBLUE		DCM_MAKETAG(DCM_GROUPOVERLAY, 0x1103)
#define DCM_OLYGRAY			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x1200)
#define DCM_OLYRED			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x1201)
#define DCM_OLYGREEN			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x1202)
#define DCM_OLYBLUE			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x1203)
#define DCM_OLYROIAREA			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x1301)
#define DCM_OLYROIMEAN			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x1302)
#define DCM_OLYROISTANDARDDEVIATION	DCM_MAKETAG(DCM_GROUPOVERLAY, 0x1303)
#define DCM_OLYOVERLAYLABEL		DCM_MAKETAG(DCM_GROUPOVERLAY, 0x1500)
#define DCM_OLYDATA			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x3000)

/* Retired, 4000 */
#define	DCM_OLYCOMMENTS			DCM_MAKETAG(DCM_GROUPOVERLAY, 0x4000)

/*  Define the elements for the PIXEL group (7FE0)
*/
#define	DCM_PXLGROUPLENGTH		DCM_MAKETAG(DCM_GROUPPIXEL,0x0000)
#define	DCM_PXLPIXELDATA		DCM_MAKETAG(DCM_GROUPPIXEL,0x0010)

/* Define the elements for the MEDIA group, 0x0088  */
#define	DCM_MEDIAGROUPLENGTH		DCM_MAKETAG(DCM_GROUPMEDIA,0x0000)
#define	DCM_MEDIASTORAGEFILESETID	DCM_MAKETAG(DCM_GROUPMEDIA,0x0130)
#define	DCM_MEDIASTORAGEFILESETUID	DCM_MAKETAG(DCM_GROUPMEDIA,0x0140)
#define	DCM_MEDIAICONIMAGE		DCM_MAKETAG(DCM_GROUPMEDIA,0x0200)
#define	DCM_MEDIATOPICTITLE		DCM_MAKETAG(DCM_GROUPMEDIA,0x0904)
#define	DCM_MEDIATOPICSUBJECT		DCM_MAKETAG(DCM_GROUPMEDIA,0x0906)
#define	DCM_MEDIATOPICAUTHOR		DCM_MAKETAG(DCM_GROUPMEDIA,0x0910)
#define	DCM_MEDIATOPICKEYWORD		DCM_MAKETAG(DCM_GROUPMEDIA,0x0912)

/* Define the elements for the BASICFILMSESSION group (2000)
*/
#define DCM_BFSGROUPLENGTH		DCM_MAKETAG(DCM_GROUPBASICFILMSESSION,0x000)
#define DCM_BFSCOPIES			DCM_MAKETAG(DCM_GROUPBASICFILMSESSION,0x0010)
#define DCM_BFSPRINTPRIORITY		DCM_MAKETAG(DCM_GROUPBASICFILMSESSION,0x0020)
#define DCM_BFSMEDIUMTYPE		DCM_MAKETAG(DCM_GROUPBASICFILMSESSION,0x0030)
#define DCM_BFSFILMDESTINATION		DCM_MAKETAG(DCM_GROUPBASICFILMSESSION,0x0040)
#define DCM_BFSFILMSESSIONLABEL		DCM_MAKETAG(DCM_GROUPBASICFILMSESSION,0x0050)
#define DCM_BFSMEMORYALLOCATION		DCM_MAKETAG(DCM_GROUPBASICFILMSESSION,0x0060)
#define DCM_BFSREFERENCEDFILMBOXSEQ	DCM_MAKETAG(DCM_GROUPBASICFILMSESSION,0x0500)

/* Define the elements for the BASICFILMBOX group (2010)
*/
#define DCM_BFBGROUPLENGTH		DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0000)
#define DCM_BFBIMAGEDISPLAYFORMAT	DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0010)
#define DCM_BFBANNOTATIONDISPLAYFORMAT	DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0030)
#define DCM_BFBFILMORIENTATION		DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0040)
#define DCM_BFBFILMSIZEID		DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0050)
#define DCM_BFBMAGNIFICATIONTYPE	DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0060)
#define DCM_BFBSMOOTHINGTYPE		DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0080)
#define DCM_BFBBORDERDENSITY		DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0100)
#define DCM_BFBEMPTYIMAGEDENSITY	DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0110)
#define DCM_BFBMINDENSITY		DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0120)
#define DCM_BFBMAXDENSITY		DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0130)
#define DCM_BFBTRIM			DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0140)
#define DCM_BFBCONFIGURATIONINFO	DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0150)
#define DCM_BFBREFBASICFILMSESSIONSEQ	DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0500)
#define DCM_BFBREFBASICIMAGEBOXSEQ	DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0510)
#define DCM_BFBREFBASICANNOTBOXSEQ	DCM_MAKETAG(DCM_GROUPBASICFILMBOX,0x0520)

/* Define the elements of the BASICIMAGEBOX group (2020)
*/
#define DCM_BIBGROUPLENGTH		DCM_MAKETAG(DCM_GROUPBASICIMAGEBOX,0x0000)
#define DCM_BIBIMAGEPOSITION		DCM_MAKETAG(DCM_GROUPBASICIMAGEBOX,0x0010)
#define DCM_BIBPOLARITY			DCM_MAKETAG(DCM_GROUPBASICIMAGEBOX,0x0020)
#define DCM_BIBREQUESTEDIMAGESIZE	DCM_MAKETAG(DCM_GROUPBASICIMAGEBOX,0x0030)
#define DCM_BIBPREFORMATGREYSCALEIMAGESEQ	DCM_MAKETAG(DCM_GROUPBASICIMAGEBOX,0x0110)
#define DCM_BIBPREFORMATCOLORIMAGESEQ	DCM_MAKETAG(DCM_GROUPBASICIMAGEBOX,0x0111)
#define DCM_BIBREFIMAGEOVERLAYBOXSEQ	DCM_MAKETAG(DCM_GROUPBASICIMAGEBOX,0x0130)
#define DCM_BIBREFVOILUTSEQ		DCM_MAKETAG(DCM_GROUPBASICIMAGEBOX,0x0140)

/* Define the elements of the BASICANNOTATIONBOX group (2030)
*/
#define DCM_BABGROUPLENGTH		DCM_MAKETAG(DCM_GROUPBASICANNOTATIONBOX,0x0000)
#define DCM_BABANNOTATIONPOSITION	DCM_MAKETAG(DCM_GROUPBASICANNOTATIONBOX,0x0010)
#define DCM_BABTEXTSTRING		DCM_MAKETAG(DCM_GROUPBASICANNOTATIONBOX,0x0020)

/* Defines the elements of the BASICIMAGEOVERLAYBOX group (2040)
*/
#define DCM_IOBGROUPLENGTH		DCM_MAKETAG(DCM_GROUPBASICIMAGEOVERLAYBOX,0x0000)
#define DCM_IOBREFOVERLAYPLANESEQ	DCM_MAKETAG(DCM_GROUPBASICIMAGEOVERLAYBOX,0x0010)
#define DCM_IOBREFOVERLAYPLANEGROUPS	DCM_MAKETAG(DCM_GROUPBASICIMAGEOVERLAYBOX,0x0011)
#define DCM_IOBOVERLAYMAGNIFICATIONTYPE	DCM_MAKETAG(DCM_GROUPBASICIMAGEOVERLAYBOX,0x0060)
#define DCM_IOBOVERLAYSMOOTHINGTYPE	DCM_MAKETAG(DCM_GROUPBASICIMAGEOVERLAYBOX,0x0070)
#define DCM_IOBOVERLAYFOREGROUNDDENSITY	DCM_MAKETAG(DCM_GROUPBASICIMAGEOVERLAYBOX,0x0080)
#define DCM_IOBOVERLAYMODE		DCM_MAKETAG(DCM_GROUPBASICIMAGEOVERLAYBOX,0x0090)
#define DCM_IOBTHRESHOLDDENSITY		DCM_MAKETAG(DCM_GROUPBASICIMAGEOVERLAYBOX,0x0100)
#define DCM_IOBREFIMAGEBOXSEQUENCE	DCM_MAKETAG(DCM_GROUPBASICIMAGEOVERLAYBOX,0x0500)

/* Define the elements of the PRINTJOB group (2100)
*/
#define DCM_PJGROUPLENGTH		DCM_MAKETAG(DCM_GROUPPRINTJOB,0x0000)
#define DCM_PJEXECUTIONSTATUS		DCM_MAKETAG(DCM_GROUPPRINTJOB,0x0020)
#define DCM_PJEXECUTIONSTATUSINFO	DCM_MAKETAG(DCM_GROUPPRINTJOB,0x0030)
#define DCM_PJCREATIONDATE		DCM_MAKETAG(DCM_GROUPPRINTJOB,0x0040)
#define DCM_PJCREATIONTIME		DCM_MAKETAG(DCM_GROUPPRINTJOB,0x0050)
#define DCM_PJORIGINATOR		DCM_MAKETAG(DCM_GROUPPRINTJOB,0x0070)
#define DCM_PJREFPRINTJOBSEQ		DCM_MAKETAG(DCM_GROUPPRINTJOB,0x0500)

/* Define the elements of the PRINTER group, 0x2110
*/
#define DCM_PRINTERGROUPLENGTH		DCM_MAKETAG(DCM_GROUPPRINTER,0x0000)
#define DCM_PRINTERSTATUS		DCM_MAKETAG(DCM_GROUPPRINTER,0x0010)
#define DCM_PRINTERSTATUSINFO		DCM_MAKETAG(DCM_GROUPPRINTER,0x0020)
#define DCM_PRINTERNAME			DCM_MAKETAG(DCM_GROUPPRINTER,0x0030)
#define	DCM_PRINTERQUEUEID		DCM_MAKETAG(DCM_GROUPPRINTER,0x0099)

/*  Define any semantics associated with pixel representation
*/
#define	DCM_PIXELUNSIGNED	0
#define	DCM_PIXELTWOSCOMPLEMENT	1

/* Define the attributes in the Padding group, 0xfffc
*/

#define	DCM_PADITEM			DCM_MAKETAG(DCM_GROUPPAD, 0xfffc)

/* Define the attributes in the Delimiter group
*/

#define	DCM_DLMITEM			DCM_MAKETAG(DCM_GROUPDELIMITER, 0xe000)
#define	DCM_DLMITEMDELIMITATIONITEM	DCM_MAKETAG(DCM_GROUPDELIMITER, 0xe00d)
#define	DCM_DLMSEQUENCEDELIMITATIONITEM	DCM_MAKETAG(DCM_GROUPDELIMITER, 0xe0dd)

/* Define the values for the Data Set Type in the Command Field
*/

#define	DCM_CMDDATAIMAGE	0x0000
#define	DCM_CMDDATAGRAPHICS	0x0002
#define	DCM_CMDDATATEXT		0x0003
#define	DCM_CMDDATAOTHER	0x0100
#define	DCM_CMDDATANULL		0x0101
#define	DCM_CMDDATAIDENTIFIER	0x0102
#define	DCM_CMDDATAPRIVIMAGE	0x8000
#define	DCM_CMDDATAPRIVGRAPHICS	0x8002
#define	DCM_CMDDATAPRIVTEXT	0x8003

/* Define status values */

#define	DCM_STATUS_SUCCESS	0x0000
#define	DCM_STATUS_PENDING	0xff00
#define	DCM_STATUS_REFUSED	0x01
#define	DCM_STATUS_FAILED	0x02

/* Define priority values in messages */

#define DCM_PRIORITYLOW		0x2
#define	DCM_PRIORITYMEDIUM	0x0
#define	DCM_PRIORITYHIGH	0x1

/* Define legal values to be transmitted as Query Level */

#define	DCM_QUERYLEVELPATIENT	"PATIENT"
#define	DCM_QUERYLEVELSTUDY	"STUDY"
#define	DCM_QUERYLEVELSERIES	"SERIES"
#define	DCM_QUERYLEVELIMAGE	"IMAGE"

/* Define legal values for the Photometric Interpretation */

#define DCM_IMGPHOTOINTERPMONOCHROME1	"MONOCHROME1"
#define DCM_IMGPHOTOINTERPMONOCHROME2	"MONOCHROME2"
#define DCM_IMGPHOTOINTERPPALETTECOLOR	"PALETTE COLOR"
#define DCM_IMGPHOTOINTERPRGB		"RGB"
#define DCM_IMGPHOTOINTERPHSV		"HSV"
#define DCM_IMGPHOTOINTERPRGBA		"RGBA"
#define DCM_IMGPHOTOINTERPCMYK		"CMYK"

#define	DCM_UNSPECIFIEDLENGTH	0xffffffff
#define	DCM_DELIMITOR	'\\'

#ifdef  __cplusplus
}
#endif

#endif
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
** Module Name(s):
** Author, Date:	Stephen M. Moore, 15-Apr-93
** Intent:		This header defines public typedefs for the DICOM
**			software produced at the Mallinckrodt Institute of
**			Radiology.  It also defines unique identifiers
**			for standard classes and objects defined by the
**			standard.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

#ifndef CTNOS_IS_IN
#define CTNOS_IS_IN 1

#ifdef  __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
/** #include <stdarg.h> **/
#include <errno.h>
#include <signal.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#include <winsock.h>
#include <process.h>
#include <sys/timeb.h>
#include <direct.h>

typedef SOCKET CTN_SOCKET;
#define CTN_BAD_SOCKET INVALID_SOCKET

#else
#include <unistd.h>
#include <sys/file.h>
#include <sys/socket.h>
/*#include <sys/param.h>*/
#include <sys/time.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/param.h>
#include <sys/utsname.h>
#include <dirent.h>

typedef int CTN_SOCKET;
#define CTN_BAD_SOCKET -1
#endif

#ifdef SOLARIS
#include <fcntl.h>
#endif

#ifdef USEREGCOMP
#include <regex.h>
#endif


#ifdef  __cplusplus
}
#endif

#endif
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
** Author, Date:	Stephen M. Moore, 26-Apr-93
** Intent:
**	This file defines private structures for the DICOM information
**	object package.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

#ifdef  __cplusplus
extern "C" {
#endif

typedef struct {
    void *reserved[2];
    unsigned short group;
/*    unsigned long groupLength; */
    unsigned long baseLength;
    int longVRAttributes;
    LST_HEAD *elementList;
}   PRV_GROUP_ITEM;

typedef struct {
    void *reserved[2];
    DCM_ELEMENT element;
    int byteOrder;
    off_t dataOffset;
    off_t currentOffset;
    size_t allocatedDataLength;
    size_t originalDataLength;
    size_t paddedDataLength;
    int fragmentFlag;
}   PRV_ELEMENT_ITEM;

#define	DCM_OBJUNDEFINED 0x01
#define	DCM_OBJCOMMAND	0x02
#define	DCM_OBJDATASET	0x03

typedef struct {
    void *reserved[2];
    char keyType[32];
    DCM_OBJECTTYPE objectType;
    int accessMethod;
    CTNBOOLEAN deleteFlag;
    CTNBOOLEAN groupLengthFlag;
    unsigned long objectSize;
    unsigned long offset;
    unsigned long pixelSize;
    unsigned long pixelOffset;
    unsigned short pixelBitsAllocated;
    unsigned short pixelRepresentation;
    PRV_GROUP_ITEM *groupCtx;
    PRV_ELEMENT_ITEM *elementCtx;
    int fd;
    char fileName[1024];
    void *userCtx;
        CONDITION(*rd) (void *ctx, void *buf, int toRead, int *bytesRead);
        CONDITION(*sk) (void *ctx, int offset, int flag);
    LST_HEAD *groupList;
    CTNBOOLEAN preambleFlag;
    unsigned char preamble[DCM_PREAMBLELENGTH];
    unsigned long dataOptions;
    unsigned long metaHeaderLength;
    int longVRAttributes;
    char waveformDataVR[DICOM_CS_LENGTH+1];
}   PRIVATE_OBJECT;

#define KEY_DCM_OBJECT	"KEY ACR NEMA V3 OBJECT"

#define	DCM_FILE_ACCESS		1
#define	DCM_MEMORY_ACCESS	2

typedef union {
    unsigned short s;
    unsigned char u[2];
}   SHORT_WORD;

typedef union {
/* Type unsigned long is 64-bits on 64-bit Solaris, but such checks have
   been made for defining U32, so use it.            10 Nov 2005 [rickr] */
#if 0
#ifdef __alpha
    unsigned int l;
#else
    unsigned long l;
    U32 l;
#endif
#endif

    U32 l;
    unsigned char u[4];
}   LONG_WORD;

#define GET_SHORT_SAME_ORDER(A,B) {		\
	SHORT_WORD sss;				\
	sss.u[0] = (A)[0];			\
	sss.u[1] = (A)[1];			\
	(B) = sss.s;				\
}

#define GET_SHORT_REVERSE_ORDER(A,B) {		\
	SHORT_WORD sss;				\
	sss.u[0] = (A)[1];			\
	sss.u[1] = (A)[0];			\
	(B) = sss.s;				\
}

#define GET_LONG_SAME_ORDER(A,B) {		\
	LONG_WORD lll;				\
	lll.u[0] = (A)[0];			\
	lll.u[1] = (A)[1];			\
	lll.u[2] = (A)[2];			\
	lll.u[3] = (A)[3];			\
	(B) = lll.l;				\
}

#define GET_LONG_REVERSE_ORDER(A,B) {		\
	LONG_WORD lll;				\
	lll.u[0] = (A)[3];			\
	lll.u[1] = (A)[2];			\
	lll.u[2] = (A)[1];			\
	lll.u[3] = (A)[0];			\
	(B) = lll.l;				\
}

#ifdef  __cplusplus
}
#endif
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
** Author, Date:	Stephen M. Moore, 2-Jun-93
** Intent:		This include file defines constants for all of
**			the standard UIDs defined in the DICOM standard.
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/

#ifndef DICOM_UIDS_IS_IN
#define DICOM_UIDS_IS_IN 1

#ifdef  __cplusplus
extern "C" {
#endif

#define	MIR_IMPLEMENTATIONCLASSUID		"1.2.840.113654.2.3.1995.3.0.4"
#define	MIR_IMPLEMENTATIONVERSIONNAME		"MIRCTN17MAY2002"
#define	MIR_SOPCLASSKILLSERVER			"1.2.840.113654.2.30.1"
#define	MIR_SOPCLASSRESETSERVER			"1.2.840.113654.2.30.2"

#define DICOM_SOPCLASSVERIFICATION		"1.2.840.10008.1.1"

#define DICOM_TRANSFERLITTLEENDIAN		"1.2.840.10008.1.2"
#define DICOM_TRANSFERLITTLEENDIANEXPLICIT	"1.2.840.10008.1.2.1"
#define DICOM_TRANSFERBIGENDIANEXPLICIT		"1.2.840.10008.1.2.2"
#define DICOM_TRANSFERJPEGBASELINEPROCESS1	"1.2.840.10008.1.2.4.50"
#define DICOM_TRANSFERJPEGEXTENDEDPROC2AND4	"1.2.840.10008.1.2.4.51"
#define DICOM_TRANSFERJPEGEXTENDEDPROC3AND5	"1.2.840.10008.1.2.4.52"
#define DICOM_TRANSFERJPEGSPECTRALPROC6AND8	"1.2.840.10008.1.2.4.53"
#define DICOM_TRANSFERJPEGSPECTRALPROC7AND9	"1.2.840.10008.1.2.4.54"
#define DICOM_TRANSFERJPEGFULLPROGRESSPROC10AND12 "1.2.840.10008.1.2.4.55"
#define DICOM_TRANSFERJPEGFULLPROGRESSPROC11AND13 "1.2.840.10008.1.2.4.56"
#define DICOM_TRANSFERJPEGLOSSLESSPROC14	"1.2.840.10008.1.2.4.57"
#define DICOM_TRANSFERJPEGLOSSLESSPROC15	"1.2.840.10008.1.2.4.58"
#define DICOM_TRANSFERJPEGEXTENDEDPROC16AND18	"1.2.840.10008.1.2.4.59"
#define DICOM_TRANSFERJPEGEXTENDEDPROC17AND19	"1.2.840.10008.1.2.4.60"
#define DICOM_TRANSFERJPEGSPECTRALPROC20AND22	"1.2.840.10008.1.2.4.61"
#define DICOM_TRANSFERJPEGSPECTRALPROC21AND23	"1.2.840.10008.1.2.4.62"
#define DICOM_TRANSFERJPEGFULLPROGRESSPROC24AND26 "1.2.840.10008.1.2.4.63"
#define DICOM_TRANSFERJPEGFULLPROGRESSPROC25AND27 "1.2.840.10008.1.2.4.64"
#define DICOM_TRANSFERJPEGLOSSLESSPROC28	"1.2.840.10008.1.2.4.65"
#define DICOM_TRANSFERJPEGLOSSLESSPROC29	"1.2.840.10008.1.2.4.66"
#define DICOM_TRANSFERJPEGLOSSLESSPROCFIRSTORDERREDICT "1.2.840.10008.1.2.4.70"


/* Define the UIDS for the service classes defined by the DICOM standard
*/
#define DICOM_SOPCLASSBASICSTUDYCONTENTNOTIFICATION "1.2.840.10008.1.9"
#define	DICOM_SOPCLASSSTORAGECOMMITMENTPUSHMODEL "1.2.840.10008.1.20.1"
#define	DICOM_WELLKNOWNSTORAGECOMMITMENTPUSHMODEL "1.2.840.10008.1.20.1.1"
#define	DICOM_SOPCLASSSTORAGECOMMITMENTPULLMODEL "1.2.840.10008.1.20.2"
#define	DICOM_WELLKNOWNSTORAGECOMMITMENTPULLMODEL "1.2.840.10008.1.20.2.1"

#define DICOM_STDAPPLICATIONCONTEXT		"1.2.840.10008.3.1.1.1"

#define	DICOM_SOPCLASSDETACHEDPATIENTMGMT	"1.2.840.10008.3.1.2.1.1"
#define DICOM_SOPCLASSDETACHEDPATIENTMGMTMETA	"1.2.840.10008.3.1.2.1.4"
#define DICOM_SOPCLASSDETACHEDVISITMGMT		"1.2.840.10008.3.1.2.2.1"
#define DICOM_SOPCLASSDETACHEDSTUDYMGMT		"1.2.840.10008.3.1.2.3.1"
#define DICOM_SOPCLASSSTUDYCOMPONENTMGMT	"1.2.840.10008.3.1.2.3.2"
#define DICOM_SOPCLASSMPPS			"1.2.840.10008.3.1.2.3.3"
#define DICOM_SOPCLASSDETACHEDRESULTSMGMT	"1.2.840.10008.3.1.2.5.1"
#define DICOM_SOPCLASSDETACHEDRESULTSMGMTMETA	"1.2.840.10008.3.1.2.5.4"
#define DICOM_SOPCLASSDETACHEDSTUDYMGMTMETA	"1.2.840.10008.3.1.2.5.5"
#define DICOM_SOPCLASSDETACHEDINTERPRETMGMT	"1.2.840.10008.3.1.2.6.1"

#define DICOM_SOPCLASSBASICFILMSESSION		"1.2.840.10008.5.1.1.1"
#define	DICOM_SOPCLASSBASICFILMBOX		"1.2.840.10008.5.1.1.2"
#define	DICOM_SOPCLASSBASICGREYSCALEIMAGEBOX	"1.2.840.10008.5.1.1.4"
#define	DICOM_SOPCLASSBASICCOLORIMAGEBOX	"1.2.840.10008.5.1.1.4.1"
#define	DICOM_SOPCLASSREFERENCEDIMAGEBOX	"1.2.840.10008.5.1.1.4.2"
#define	DICOM_SOPCLASSGREYSCALEPRINTMGMTMETA	"1.2.840.10008.5.1.1.9"
#define	DICOM_SOPCLASSREFGREYSCALEPRINTMGMTMETA	"1.2.840.10008.5.1.1.9.1"
#define	DICOM_SOPCLASSPRINTJOB			"1.2.840.10008.5.1.1.14"
#define	DICOM_SOPCLASSBASICANNOTATIONBOX	"1.2.840.10008.5.1.1.15"
#define	DICOM_SOPCLASSPRINTER			"1.2.840.10008.5.1.1.16"
#define	DICOM_SOPPRINTERINSTANCE		"1.2.840.10008.5.1.1.17"
#define	DICOM_SOPCLASSCOLORPRINTMGMTMETA	"1.2.840.10008.5.1.1.18"
#define	DICOM_SOPCLASSREFCOLORPRINTMGMTMETA	"1.2.840.10008.5.1.1.18.1"
#define DICOM_SOPCLASSVOILUT			"1.2.840.10008.5.1.1.22"
#define DICOM_SOPCLASSIMAGEOVERLAYBOX		"1.2.840.10008.5.1.1.24"

#define	DICOM_SOPCLASSSTOREDPRINT		"1.2.840.10008.5.1.1.27"
#define	DICOM_SOPCLASSHARDCOPYGRAYSCALEIMAGE	"1.2.840.10008.5.1.1.29"
#define	DICOM_SOPCLASSHARDCOPYCOLORIMAGE	"1.2.840.10008.5.1.1.30"

#define	DICOM_SOPCLASSCOMPUTEDRADIOGRAPHY	"1.2.840.10008.5.1.4.1.1.1"
#define DICOM_SOPCLASSDIGXRAYPRESENTATION       "1.2.840.10008.5.1.4.1.1.1.1"
#define DICOM_SOPCLASSDIGXRAYPROCESSING         "1.2.840.10008.5.1.4.1.1.1.1.1"
#define DICOM_SOPCLASSMAMMOXRPRESENTATION       "1.2.840.10008.5.1.4.1.1.1.2"
#define DICOM_SOPCLASSMAMMOXRPROCESSING         "1.2.840.10008.5.1.4.1.1.1.2.1"
#define DICOM_SOPCLASSINTRAORALPRESENTATION     "1.2.840.10008.5.1.4.1.1.1.3"
#define DICOM_SOPCLASSINTRAORALPROCESSING       "1.2.840.10008.5.1.4.1.1.1.3.1"
#define	DICOM_SOPCLASSCT			"1.2.840.10008.5.1.4.1.1.2"
#define	DICOM_SOPCLASSUSMULTIFRAMEIMAGE1993	"1.2.840.10008.5.1.4.1.1.3"
#define	DICOM_SOPCLASSUSMULTIFRAMEIMAGE		"1.2.840.10008.5.1.4.1.1.3.1"
#define	DICOM_SOPCLASSMR			"1.2.840.10008.5.1.4.1.1.4"
#define	DICOM_SOPCLASSNM1993			"1.2.840.10008.5.1.4.1.1.5"
#define	DICOM_SOPCLASSUS1993			"1.2.840.10008.5.1.4.1.1.6"
#define	DICOM_SOPCLASSUS			"1.2.840.10008.5.1.4.1.1.6.1"
#define	DICOM_SOPCLASSSECONDARYCAPTURE		"1.2.840.10008.5.1.4.1.1.7"
#define DICOM_SOPCLASSSTANDALONEOVERLAY		"1.2.840.10008.5.1.4.1.1.8"
#define DICOM_SOPCLASSSTANDALONECURVE		"1.2.840.10008.5.1.4.1.1.9"
#define	DICOM_SOPCLASSWAVEFORMSTORAGE		"1.2.840.10008.5.1.4.1.1.9.1"
#define	DICOM_SOPCLASSECGWAVEFORMSTORAGE	"1.2.840.10008.5.1.4.1.1.9.1.1"
#define DICOM_SOPCLASSSTANDALONEMODALITYLUT	"1.2.840.10008.5.1.4.1.1.10"
#define DICOM_SOPCLASSSTANDALONEVOILUT		"1.2.840.10008.5.1.4.1.1.11"
#define DICOM_SOPCLASSGREYSCALEPS		"1.2.840.10008.5.1.4.1.1.11.1"
#define	DICOM_SOPCLASSXRAYANGIO			"1.2.840.10008.5.1.4.1.1.12.1"
#define	DICOM_SOPCLASSXRAYFLUORO		"1.2.840.10008.5.1.4.1.1.12.2"
#define DICOM_SOPCLASSXRAYANGIOBIPLANE_RET      "1.2.840.10008.5.1.4.1.1.12.3"
#define	DICOM_SOPCLASSNM			"1.2.840.10008.5.1.4.1.1.20"
#define DICOM_SOPCLASSVLENDOSCOPIC              "1.2.840.10008.5.1.4.1.1.77.1.1"
#define DICOM_SOPCLASSVLMICROSCOPIC             "1.2.840.10008.5.1.4.1.1.77.1.2"
#define DICOM_SOPCLASSVLSLIDEMICROSCOPIC        "1.2.840.10008.5.1.4.1.1.77.1.3"
#define DICOM_SOPCLASSVLPHOTOGRAPHIC            "1.2.840.10008.5.1.4.1.1.77.1.4"
#define	DICOM_SOPCLASSBASICTEXTSR		"1.2.840.10008.5.1.4.1.1.88.11"
#define	DICOM_SOPCLASSENHANCEDSR		"1.2.840.10008.5.1.4.1.1.88.22"
#define	DICOM_SOPCLASSCOMPREHENSIVESR		"1.2.840.10008.5.1.4.1.1.88.33"
#define	DICOM_SOPCLASSKEYOBJECTNOTE		"1.2.840.10008.5.1.4.1.1.88.59"
#define	DICOM_SOPCLASSPET			"1.2.840.10008.5.1.4.1.1.128"
#define	DICOM_SOPCLASSSTANDALONEPETCURVE	"1.2.840.10008.5.1.4.1.1.129"
#define DICOM_SOPRTIMAGESTORAGE			"1.2.840.10008.5.1.4.1.1.481.1"
#define DICOM_SOPRTDOSESTORAGE			"1.2.840.10008.5.1.4.1.1.481.2"
#define DICOM_SOPRTSTRUCTURESETSTORAGE		"1.2.840.10008.5.1.4.1.1.481.3"
#define DICOM_SOPRTBREAMS                       "1.2.840.10008.5.1.4.1.1.481.4"
#define DICOM_SOPRTPLANSTORAGE			"1.2.840.10008.5.1.4.1.1.481.5"
#define DICOM_SOPRTBRACHYTREATMENT              "1.2.840.10008.5.1.4.1.1.481.6"
#define DICOM_SOPRTTREATMENTSUMMARY             "1.2.840.10008.5.1.4.1.1.481.7"

#define	DICOM_SOPPATIENTQUERY_FIND		"1.2.840.10008.5.1.4.1.2.1.1"
#define	DICOM_SOPPATIENTQUERY_MOVE		"1.2.840.10008.5.1.4.1.2.1.2"
#define	DICOM_SOPPATIENTQUERY_GET		"1.2.840.10008.5.1.4.1.2.1.3"

#define	DICOM_SOPSTUDYQUERY_FIND		"1.2.840.10008.5.1.4.1.2.2.1"
#define	DICOM_SOPSTUDYQUERY_MOVE		"1.2.840.10008.5.1.4.1.2.2.2"
#define	DICOM_SOPSTUDYQUERY_GET			"1.2.840.10008.5.1.4.1.2.2.3"

#define	DICOM_SOPPATIENTSTUDYQUERY_FIND		"1.2.840.10008.5.1.4.1.2.3.1"
#define	DICOM_SOPPATIENTSTUDYQUERY_MOVE		"1.2.840.10008.5.1.4.1.2.3.2"
#define	DICOM_SOPPATIENTSTUDYQUERY_GET		"1.2.840.10008.5.1.4.1.2.3.3"

#define	DICOM_SOPMODALITYWORKLIST_FIND		"1.2.840.10008.5.1.4.31"

#define	DICOM_SOPGPWORKLIST_FIND		"1.2.840.10008.5.1.4.32.1"

typedef enum {
    UID_PATIENT = 2,
    UID_VISIT,
    UID_STUDY,
    UID_SERIES,
    UID_IMAGE,
    UID_RESULTS,
    UID_INTERPRETATION,
    UID_PRINTER,
    UID_DEVICE,
    UID_STUDYCOMPONENT,
    UID_STORAGECOMMITTRANSACTION
}   UID_TYPE;

typedef struct {
    unsigned long patient;
    unsigned long visit;
    unsigned long study;
    unsigned long series;
    unsigned long image;
    unsigned long results;
    unsigned long interpretation;
    unsigned long printer;
    unsigned long deviceType;
    unsigned long serialNumber;
    unsigned long studyComponent;
    unsigned long storageCommitTransaction;
    char root[DICOM_UI_LENGTH + 1];
}   UID_BLOCK;

typedef enum {
    UID_CLASS_K_APPLICATIONCONTEXT,
    UID_CLASS_K_IMPLEMENTATION,
    UID_CLASS_K_SOPCLASS,
    UID_CLASS_K_METASOPCLASS,
    UID_CLASS_K_TRANSFERSYNTAX,
    UID_CLASS_K_WELLKNOWNUID
}   UID_CLASS;

typedef struct {
    UID_CLASS UIDclass;
    char UID[DICOM_UI_LENGTH + 1];
    char description[64];
    char originator[32];
}   UID_DESCRIPTION;

CONDITION
UID_NewUID(UID_TYPE type, char *uid);
CONDITION
UID_NewNumber(UID_TYPE type, unsigned long *value);
CONDITION
UID_Lookup(char *UID, UID_DESCRIPTION * description);
void
UID_ScanDictionary(void (*callback)(const UID_DESCRIPTION *d1, void *ctx1),
	void *ctx);
char *UID_Message(CONDITION cond);
char* UID_Translate(const char* value);
int UID_IsStorageClass(const char* sopClassUID);

#define	UID_NORMAL		FORM_COND(FAC_UID, SEV_SUCC, 1)
#define	UID_NOUIDFILENAME	FORM_COND(FAC_UID, SEV_ERROR, 2)
#define	UID_GENERATEFAILED	FORM_COND(FAC_UID, SEV_ERROR, 3)
#define	UID_FILEOPENFAILURE	FORM_COND(FAC_UID, SEV_ERROR, 5)
#define	UID_FILECREATEFAILURE	FORM_COND(FAC_UID, SEV_ERROR, 6)
#define	UID_ILLEGALROOT		FORM_COND(FAC_UID, SEV_ERROR, 7)
#define	UID_ILLEGALNUMERIC	FORM_COND(FAC_UID, SEV_ERROR, 8)
#define	UID_NODEVICETYPE	FORM_COND(FAC_UID, SEV_ERROR, 9)
#define	UID_NOROOT		FORM_COND(FAC_UID, SEV_ERROR, 10)
#define UID_UIDNOTFOUND		FORM_COND(FAC_UID, SEV_ERROR, 11)

#ifdef  __cplusplus
}
#endif

#endif
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
** Module Name(s):	utility.h
** Author, Date:	David E. Beecher, March 1994
** Intent:		Define typedefs and function prototypes for
**			Utility (UTL) facility (for functions which may
**			generally useful in a number of areas).
** Last Update:		$Author$, $Date$
** Source File:		$RCSfile$
** Revision:		$Revision$
** Status:		$State$
*/


#ifndef _UTL_IS_IN
#define _UTL_IS_IN 1

#ifdef  __cplusplus
extern "C" {
#endif

#define OFF		0
#define ON		1
#define REGEX_SIZE	128

#if 0
#if !defined(LINUX) && !defined(IRIX) && !defined(DARWIN) && !defined(SGI)
char *re_comp(char *);
int re_exec(char *);
#endif
char *UTL_ConvertRegex(char *regex);
CONDITION UTL_RegexMatch(char *regex, char *stm);
#endif

typedef struct {
  void* reserved[2];
  char path[1024];
} UTL_FILEITEM;


long UTL_ConvertDatetoLong(const char *date);
double UTL_ConvertTimetoFloat(const char *time);

void UTL_ConvertLongtoDate(long ld, char *date);
void UTL_ConvertFloattoTime(double dt, char *time);
void UTL_SqueezeBlanks(char *s);

void UTL_GetDicomDate(char *date);
void UTL_GetDicomTime(char *time);

CONDITION UTL_DateMatch(char *datestring, char *stm);
CONDITION UTL_TimeMatch(char *timestring, char *stm);

void* UTL_GetTimeStamp();
double UTL_DeltaTime(void* timeStamp);
void UTL_ReleaseTimeStamp(void* timeStamp);

CONDITION UTL_VerifyCreatePath(const char* path);
CTNBOOLEAN UTL_IsDirectory(const char* path);
CTNBOOLEAN UTL_IsFile(const char* path);
CONDITION UTL_DeleteFile(const char* path);
CONDITION UTL_ScanDirectory(const char* path, LST_HEAD** lst);

CONDITION UTL_SetConfigFile(const char* configFile);
CONDITION UTL_TestConfigFile(const char* configFile);
char* UTL_GetConfigParameter(const char* paramName);
char**
UTL_ExpandToPointerArray(const char* inputText,
			 const char* delimiters,
			 int* numberOfEntries);
CONDITION UTL_FileSize(const char* path, U32* size);

#define	UTL_NORMAL		FORM_COND(FAC_UTL, SEV_SUCC, 1)
#define	UTL_UNIMPLEMENTED	FORM_COND(FAC_UTL, SEV_ERROR, 2)
#define UTL_MATCH		FORM_COND(FAC_UTL, SEV_SUCC, 3)
#define UTL_NOMATCH		FORM_COND(FAC_UTL, SEV_SUCC, 4)
#define UTL_PATHNOTDIR		FORM_COND(FAC_UTL, SEV_ERROR, 5)
#define UTL_FILECREATEFAILED	FORM_COND(FAC_UTL, SEV_ERROR, 6)
#define UTL_NO_CTN_TARGET	FORM_COND(FAC_UTL, SEV_ERROR, 7)
#define UTL_DELETEFILEFAILED	FORM_COND(FAC_UTL, SEV_ERROR, 8)
#ifdef  __cplusplus
}
#endif

#endif
