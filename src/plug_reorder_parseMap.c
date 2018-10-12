/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef __PLUGIN_REORDER_PARSEMAP_C__
#define __PLUGIN_REORDER_PARSEMAP_C__

#ifndef MAIN_PLUGIN_REORDER
#define MAIN_PLUGIN_REORDER_PARSEMAP /* compile parseMap for command line testing */
#define DEBUG_PLUGIN_REORDER_PARSEMAP
#endif

/*
    Function:   REORDER_parseMap

    Author: 	Jay Brian Kummer/Medical College Of WI/Neitz & DeYoe Labs

    Date:   	April 21, 1997

    Purpose:

		AFNI 'reorder' plugin routine that parses the epoch map file
		for a requested shuffling of a 3D+Time dataSet. This function
		will return an array of indices representing the new (reorderd)
		position of time-course values. The caller will then apply this
		order to all voxels in the dataSet.

		This version of the parseMap function, 'parseMap', 'collates'
		duplicated classes in the map file in the order that they appear
		(e.g., given a sequence of classes [ D C A B A ], the reorderd
		order returned will be indices for [ A1 A2 B C D ]).
		
		The "epoch map" is a series of entries (given in a text file)
		which indicates the classification and, implicitly, the target
		order of epochs of a time-course. These maps are companions to
		dataSets arising from specific sequences of stimulus presentation
		and, therefore, should have a one-to-one correspondence to those
		dataSets (i.e., they have the same time length).

		The expected format of the epoch map file is as follows:

			# Comment lines begin with a '#' and persist to the end of the line.
			[ <EpochClass><PointNumberInClass> | - ]
			...one entry for each TR in the stimulus presentation...

		Each entry is either an 'EpochClass' or a '-'. The latter excludes the
		point from the resulting reordering. 'EpochClass' is a single letter,
		[a-zA-Z], which classifies the current epoch; the contatenated number
		is an increasing value from 1 to the epoch length and is used mainly
		to delimit contiguous instances of the same class. 

		For example:

			# This map is a companion to a dataSet acquired during visual
			# presentation of sequence of different size rings:
			#
			#   RingSize |__A__
			#            |    |          __D__
			#            |    |__B__    |
			#            |         |    |
			#            |         |__C__
			#            |_____________________
			#            1                   20 (TR)
			#
			# 5 scans per presentation, total of 20 scans, 4 epochs.
			#
			# User expects response amplitudes to be proportional
			# to ring sizes; this will be much easier to analyze if
			# epochs are ordered corresponding to an increasing
			# trend in ring size, so reorder the epochs:
			C1
			C2
			C3
			C4
			C5
			B1
			B2
			B3
			B4
			B5
			D1
			D2
			D3
			D4
			D5
			A1
			A2
			A3
			A4
			A5
			# Resulting order:
			#   RingSize |               __A__
			#            |          __D__|
			#            |     __B__|
			#            |    |     
			#            |__C_|    
			#            |_____________________
			#            1                   20 (TR)
			#

		The TR number of each entry is, implicitly, the line number of the
		entry; therefore, the number of lines in the map file (except for
		comment lines) should equal the number of TRs in the companion
		dataSet.

    Usage:

		char *myFile = "QQ_epoch.map";	/  epoch map for a given experiment  /
		int length = npts;				/  number of time-course points in  /
										/  the dataSet  /
		ClassInfo *classKRH;				/  Place to store the sequence of classes /
		int classCount;					/  Number of classes in 'class' array /
		if(NULL == REORDER_parseMap(myFile, &length, &classKRH, &classCount)) {
			printf("!! error !!\n");
			FreeWorkspace();
			return NULL;
			}

    Input Parameters:

		myFile, char *, pointer to a NULL-terminated string, filename for an
			epoch map
		length, int *, pointer to a value storing the number of time-course points
			in the dataSet
    Output Parameters:

		int *, pointer to an array of length 'length' containing indices (on
			the interval [0, (length-1)] for the new ordering of time-course
			data points. NULL is returned on any error.
		By parameter:
		class, ClassInfo **, address of a pointer to an array of structures
			that will contain class and class length info.
		classCount, integer, address of an integer to receive the length of 'class'

    Side Effects:

		The contents of 'length' will hold the length of the 3D+time dataSet
		to be processed by this plugin; on return, this will contain the length of
		the array of indices returned by this function, which in turn will also be
		the time-length of the reorderd 3D+time dataset. The return length can only
		be altered (decreased) by excluding TR points from the reorderd dataSet (by
		specifying '-' in the epoch map file). 'class' will be allocated to the
		length 'classCount' if parsing is successful.

	Pseudo Code:

		Check input parameters.
		Count the number of entries in the file.
		...Pseudo code boring...losing consciousness...

	Code: */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#ifndef MAIN_PLUGIN_REORDER
typedef struct {
	char classKRH;
	int length;
	} ClassInfo;
#endif

int *REORDER_parseMap(char *mapFile
					, int *length
					, ClassInfo **classKRH
					, int *classCount)
{
FILE *inf = NULL;
char c; int icc ;
char *sptr;
char *classList=NULL;
char cBuf[256] = {0};
int i;
int j;
int k=0;
int line;
int excluded;
int rawLength;
int *index = NULL;
int *classNum=NULL;
int classStart;
char currentClass;
int currentClassPos;

/* Check input parameters */
if(NULL == mapFile) {
	printf("!! [AFNI/reorder] NULL file name !!\n");
	*length = *classCount = 0;
	return NULL;
	}

if(0 == mapFile[0]) {
	printf("!! [AFNI/reorder] Empty file name !!\n");
	*length = *classCount = 0;
	return NULL;
	}

if(NULL == (inf = fopen(mapFile, "r"))) {
	printf("!! [AFNI/reorder] Trouble opening '%s' for reading !!\n", mapFile);
	*length = *classCount = 0;
	return NULL;
	}

/* Count the number of noncomment entries in the file */
for(i = 0, line = 1, excluded = 0;; line++) {
	if(i > *length) {
		#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
		printf("[parseMap] Entry count exceeds expected\n");
		#endif
		printf("!! [AFNI/reorder] Number of entries in 'mapFile' exceeds expected of %d !!\n"
			, *length);
		*length = *classCount = 0;
		return NULL;
		}

	/* Test for EOF */
    c = icc = fgetc(inf);
	if(EOF == icc || feof(inf)) {
		#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
		printf("[parseMap] EOF detected: ");
		#endif
		if(i != *length) {
			#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
			printf("Abnormal\n");
			#endif
			printf("!! [AFNI/reorder] Unexpected EOF at line %d !!\n", line);
			*length = *classCount = 0;
			return NULL;
			}
		else {
			#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
			printf("Normal\n");
			#endif
			break;
			}
		}
	ungetc(c, inf);

	#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
	printf("[parseMap] Processing line %d...\n", line);
	#endif

	/* Get the next line */
	fgets(cBuf, sizeof(cBuf), inf);
	
	/* Delete newline character */
	sptr = strchr(cBuf, '\n');
	if(sptr) {
		*sptr = 0;
		}
  
	/* Eat leading whitespace */
	sptr = cBuf;
	while(isspace(*sptr)) ++sptr;

	/* Skip comments and empty lines */
	if('#' == *sptr || 0 == *sptr) {
		#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
		printf("[parseMap] Skipping comment or empty line.\n");
		#endif
		continue;
		}

	if('-' == *sptr) { /* excluded value */
		#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
		printf("[parseMap] Excluded point detected.\n");
		#endif
		++i;
		++excluded;
		continue;
		}

	/* Check for valid class name */
	if(!isalpha(*sptr)) {
		printf("!! [AFNI/reorder] Bad map entry: '%s' at line %d !!\n"
			, sptr, line);
		*length = *classCount = 0;
		return NULL;
		}

	if(!isdigit(*(sptr+1))) {
		printf("!! [AFNI/reorder] Illformed entry '%s' at line %d !!\n"
			, sptr, line);
		*length = *classCount = 0;
		return NULL;
		}

	++i;
	}

/* Rewind the input file for reuse */
rewind(inf);

/* Allocate workspace */
index = (int *)calloc(sizeof(int), i-excluded);
if(NULL == index) {
	printf("!! [AFNI/reorder] Allocation error(1) !!\n");
	*length = *classCount = 0;
	return NULL;
	}

classList = (char *)calloc(sizeof(char), i);
if(NULL == classList) {
	printf("!! [AFNI/reorder] Allocation error(2) !!\n");
	free(index);
	*length = *classCount = 0;
	return NULL;
	}

classNum = (int *)calloc(sizeof(int), i);
if(NULL == classNum) {
	printf("!! [AFNI/reorder] Allocation error(3) !!\n");
	free(index);
	free(classList);
	*length = *classCount = 0;
	return NULL;
	}

/* Arrays to be reallocated (initialize) */
(*classKRH) = (ClassInfo *)calloc(sizeof(ClassInfo), 1);
if(NULL == (*classKRH)) {
	printf("!! [AFNI/reorder] Allocation error(4) !!\n");
	free(index);
	free(classList);
	free(classNum);
	*length = *classCount = 0;
	return NULL;
	}

/* Set return length */
rawLength = i;
*length = i - excluded;
#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
printf("[parseMap] Return length of array is %d elements...\n", *length);
#endif

/* Collect mapping information from file */
currentClass = 0;
*classCount = 0;
for(i = 0, line = 1; i < rawLength; line++) {
	/* Get the next line */
	fgets(cBuf, sizeof(cBuf), inf);

	/* Delete newline character */
	sptr = strchr(cBuf, '\n');
	if(sptr) {
		*sptr = 0;
		}
  
	/* Eat leading whitespace */
	sptr = cBuf;
	while(isspace(*sptr)) ++sptr;

	/* Skip comments and empty lines */
	if('#' == *sptr || 0 == *sptr) {
		continue;
		}

	if('-' == *sptr) { /* excluded value */
		classList[i] = classNum[i] = 0;
		i++;
		continue;
		}

	#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
	printf("[parseMap] Processing line %d [%s]...\n", line, sptr);
	#endif

	classList[i] = cBuf[0];
	classNum[i] = atoi(&cBuf[1]);

	/* Count classes and make sure they are numbered properly */
	if(0 == currentClass && 0 == *classCount) { /* first class */
		currentClass = cBuf[0];
		#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
		printf("[parseMap] First class: %c\n", currentClass);
		#endif
		k = classNum[i];
		if(k != 1) {
			printf("!! [AFNI/reorder] Invalid class numbering at line %d [Should start at 1] {1} !!\n"
				, line); 
			free(classList);
			free(classNum);
			free(index);
			free((*classKRH));
			(*classKRH) = NULL;
			*length = *classCount = 0;
			return NULL;
			}

		(*classCount)++;

		/* reallocate space */
		(*classKRH) = (ClassInfo *)realloc((void *)(*classKRH), (*classCount)*sizeof(ClassInfo));
		if(NULL == (*classKRH)) {
			printf("!! [AFNI/reorder] Allocation error(4) !!\n");
			free(index);
			free(classList);
			free(classNum);
			*length = *classCount = 0;
			return NULL;
			}
		(*classKRH)[(*classCount)-1].classKRH = currentClass;
		(*classKRH)[(*classCount)-1].length = 1;
		}
	else if(currentClass != cBuf[0]) { /* new class */
		currentClass = cBuf[0];
		#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
		printf("[parseMap] Next class: %c\n", currentClass);
		#endif
		k = classNum[i];
		if(k != 1) {
			printf("!! [AFNI/reorder] Invalid class numbering at line %d [Should start at 1] {2} !!\n"
				, line); 
			free(classList);
			free(classNum);
			free(index);
			free((*classKRH));
			(*classKRH) = NULL;
			*length = *classCount = 0;
			return NULL;
			}

		(*classCount)++;

		/* reallocate space */
		(*classKRH) = (ClassInfo *)realloc((void *)(*classKRH), (*classCount)*sizeof(ClassInfo));
		if(NULL == (*classKRH)) {
			printf("!! [AFNI/reorder] Allocation error(4) !!\n");
			free(index);
			free(classList);
			free(classNum);
			*length = *classCount = 0;
			return NULL;
			}
		(*classKRH)[(*classCount)-1].classKRH = currentClass;
		(*classKRH)[(*classCount)-1].length = 1;
		}
	else {
		#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
		printf("[parseMap] Next entry, checking class numbering...\n");
		#endif
		++k;
		if(1 == classNum[i]) { /* contiguous instance of current class */
			#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
			printf("[parseMap] Contiguous class: %c\n", currentClass);
			#endif
	
			k = 1;
			(*classCount)++;
	
			/* reallocate space */
			(*classKRH) = (ClassInfo *)realloc((void *)(*classKRH), (*classCount)*sizeof(ClassInfo));
			if(NULL == (*classKRH)) {
				printf("!! [AFNI/reorder] Allocation error(4) !!\n");
				free(index);
				free(classList);
				free(classNum);
				*length = *classCount = 0;
				return NULL;
				}
			(*classKRH)[(*classCount)-1].classKRH = currentClass;
			(*classKRH)[(*classCount)-1].length = 0;
			}
		else if(classNum[i] != k) {
			printf("!! [AFNI/reorder] Invalid class numbering at line %d {3} !!\n"
				, line); 
			free(index);
			free(classList);
			free(classNum);
			free((*classKRH));
			(*classKRH) = NULL;
			*length = *classCount = 0;
			return NULL;
			}
		++(*classKRH)[(*classCount)-1].length;
		}
	++i;
	}
fclose(inf);

#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
printf("\n[parseMap] Epoch map has %d distinct classes.\n\n", *classCount);
printf("[parseMap] Parsed the following:\n");
for(i = 0; i < rawLength; i++) {
	if(classList[i]) {
		printf("  [%d] Class %c %d\n", i, classList[i], classNum[i]);
		}
	else {
		printf("  [%d] Excluded.\n", i);
		}
	}
printf("\n[parseMap] Meta-sequence of classes is:\n");
for(i = 0; i < *classCount; i++) {
	printf("  [%d] Class %c [Width %d TRs]\n", i, (*classKRH)[i].classKRH, (*classKRH)[i].length);
	}
#endif

/* Build array of indices for reordering the time-course */
/* Find the first occurrence of the lowest value class [currentClass].
   Store the index positions of that class sequence in the current
   positions available in the 'index' array.
*/

#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
printf("\n[parseMap] Building mapping array...\n");
#endif
for(i = 0, currentClass = 0; i < *length; /* quit when all indices are remapped */) {
	/* Get next class to remap */
	if(!currentClass) {
		#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
		printf("\n  Determining next class to map...\n");
		#endif
		j = 0;
		while(j < rawLength) { /* look for next non-null class */
			if(classList[j]) {
				break;
				}
			++j;
			}
		if(j == rawLength) {
			#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
			printf("  << No more classes to map >>\n");
			#endif
			break; /* done if there is none */
			}

		currentClass = classList[j];	/* current epoch class to remap */
		classStart = classNum[j];	/* starting number of current class */
		currentClassPos = j;		/* index of current class */

		#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
		printf("  Next valid class is '%c', checking to see if there is a better one...\n"
			, currentClass);
		#endif

		/* See if there's a better one */
		for(j = 1; j < rawLength; j++) {
			if(classList[j]) {
				if(classList[j] < currentClass) {
					#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
					printf("    Class '%c' should be done before '%c'...\n"
						, classList[j], currentClass);
					#endif
					currentClass = classList[j];		/* new epoch class to remap */
					classStart = classNum[j];	/* starting number of new class */
					currentClassPos = j;		/* index of new class */
					}
				}
			}

		#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
		printf("  Remapping class '%c' [starting index is %d]...\n"
			, currentClass, currentClassPos);
		#endif
		}

	/* currenClassPos is the index of the start position for the current
	   class to be remapped; remap until class changes or class number
	   returns to 1 */
	/* for(k = currentClassPos; i < rawLength, k < rawLength; ) { */
	/* for(k = currentClassPos; i < rawLength && k < rawLength; ) { */
	   for(k = currentClassPos; k < rawLength; ) {
		if(classList[k]) {
			#ifdef DEBUG_PLUGIN_REORDER_PARSEMAP
			printf("    Index[%d] <-- %d [Old index]\n", i, k);
			#endif
			index[i] = k; /* store the position of the TR value from the old time-course */ 
			classList[k] = 0; /* mark as 'done' */
			++i;
			++k;
			}
		else { /* stop when a 'exclusion' point is found */
			break;
			}

		if(classList[k] != currentClass || 1 == classNum[k]) {
			break;
			}
		}
	currentClass = 0;
	}

/* Free workspace */
free(classNum);

return(index);
}

#ifdef MAIN_PLUGIN_REORDER_PARSEMAP
main(int argc, char *argv[])
{
int *index = NULL;
ClassInfo *classKRH = NULL;
int classCount = 0;
int length;
int i;

if(3 != argc) {
	printf("usage: parseMap <MapFile> <TargetCount>\n");
	exit(1);
	}

length = atoi(argv[2]);
if(length < 1) {
	printf("!! [Main] Invalid target count: %d !!\n", length);
	exit(1);
	}

if(NULL == (index = REORDER_parseMap(argv[1], &length, &classKRH, &classCount))) {
	printf("!! [Main] Trouble parsing epoch map file !!\n");
	exit(1);
	}

printf("\n[Main] Indices for epoch remapping:\n");
for(i = 0; i < length; i++) {
	printf("  %d\n", index[i]);
	}

printf("\n[Main] Meta-sequence of classes is:\n");
for(i = 0; i < classCount; i++) {
	printf("  [%d] Class %c [Width %d TRs]\n", i, classKRH[i].classKRH, classKRH[i].length);
	}

free(index);
free(classKRH);
}
#endif

#endif

