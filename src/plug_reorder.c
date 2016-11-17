/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef __MAIN_PLUGIN_REORDER_C__
#define __MAIN_PLUGIN_REORDER_C__

/***************************************************
*  File:   plug_reorder.c
*
*  Author: Jay Brian Kummer, Medical College of WI
*          [kummer@its.mcw.edu]
*
*  Date:   May 8, 1997
*
*  Purpose:
*
*    Plugin to reorder the epochs (i.e., stimulus
*    presentation regions) in a 3D+Time data set.
****************************************************/

/***********************************************************************/
/* C O N D I T I O N A L   C O M P I L A T I O N   D I R E C T I V E S */
/***********************************************************************/

#define MAIN_PLUGIN_REORDER
/* Uncomment to make this plugin verbose:
#define DEBUG_PLUGIN_REORDER
*/

/*********************************/
/* D A T A   S T R U C T U R E S */
/*********************************/

typedef struct { /* returned from 'plug_reorder_parseMap' */
	char classKRH;  /* the class (i.e., [a-zA-Z]) of a time-course segment */
	int length;  /* the length of the class in time points */
	} ClassInfo;

static char helpstring[] = /* Help string for plugin interface */
		"Purpose: Reorders voxel time-courses within a 3D+time BRIK.\n"
		"Input	: Name of the 3D+time dataset to reorder.\n"
		"\n"
		"Output	: Prefix of filename for the new dataset.\n"
		"\n"
		"Map file : Text file with instructions a reordering the input dataset.\n"
		"	Sample map file:\n"
		"		# Experiment: Bogus\n"
		"		# Date: April 1, 1997\n"
		"		-\n"
		"		-\n"
		"		D1\n"
		"		D2\n"
		"		D3\n"
		"		B1\n"
		"		B2\n"
		"		B3\n"
		"		B4\n"
		"		-\n"
		"		a1\n"
		"		a2\n"
		"		a3\n"
		"		A1\n"
		"		A2\n"
		"		A3\n"
		"		A1\n"
		"		A2\n"
		"		A3\n"
		"		-\n"
		"\n"
		"	# : Comment line (ignored)\n"
		"	- : Point to be omitted from the output time-series. In the sample map,\n"
		"		time points 1, 2, 10, and 20 will be omitted.\n"
		"	Each non-comment line in a map file corresponds to a point in the time-series of\n"
		"	an input 3D+time dataset. Codes for each time point define the plug-in action for\n"
		"	those points. For example, the code in the fifth non-comment line of the sample\n"
		"	above, 'D3', means that the fifth point in the time-series is the third point in\n"
		"	the presentation of a stimulus named 'D'. \n"
		"		Stimulus codes are single alphabetic characters and are case-sensitive, with\n"
		"	uppercase preceding lowercase in the reordering (i.e., A-Z then a-z). Stimulus\n"
		"	regions are numbered starting from 1 (i.e., [ X0 X1 X2 ] and [ X1 X3 X5 ] will\n"
		"	cause the plug-in to abort), and, as shown in the sample with the code 'A', this\n"
		"	delimits duplicate instances of the same stimulus code. For the sample map, images\n"
		"	would be reordered so that the output time-series is:\n"
		"					[ A1 A2 A3 A1 A2 A3 B1 B2 B3 B4 C1 C2 C3 a1 a2 a3 ]\n"
		"\n"
		"Duplicates : If there are duplicates of a stimulus presentation within a map (e.g.,\n"
		"[ A1 A2 A3 ] in the sample), they can be:\n"
		"	1) Collated: Duplicate instances of a stimulus will be placed in the order in\n"
		"		which they occur in the map. In the case of the sample, this results in the\n"
		"		ordering [ A1 A2 A3 A1 A2 A3 ... ].\n"
		"	2) Averaged: Duplicate instances of a stimulus will be averaged. In the sample,\n"
		"		this would result in the ordering [ A1' A2' A3' ... ], where Ai' is the mean\n"
		"		of the i-th point of all of the stimulus A's present in the map file.\n"
		"	**Duplicate stimulus regions must have the same length! For example,\n"
		"	[ D1 D2 X1 D1 D2 D3 ] will cause the plug-in to abort.\n"
		;

static char *dupaction_strings[] = /* strings for duplicate action in plugin interface */ 
		{ "Collate", "Average" };

#define NUM_DUPACTION_STRINGS (sizeof(dupaction_strings)/sizeof(char *))

/***************************/
/* H E A D E R   F I L E S */
/***************************/

#include "afni.h"

#include <unistd.h>
#include "plug_reorder_parseMap.c"

/***********************************************************/
/* I N T E R N A L   F U N C T I O N   P R O T O T Y P E S */
/***********************************************************/

char *REORDER_main(PLUGIN_interface *);/* the entry point */

/*************************************/
/* S U P P O R T   F U N C T I O N S */
/*************************************/

int compare_class(const void *i, const void *j)
/* qsort compare routine for
   sorting 'ClassInfo' array
*/
{
ClassInfo *iTmp = (ClassInfo *)i;
ClassInfo *jTmp = (ClassInfo *)j;

if(iTmp->classKRH == jTmp->classKRH) {
	return(0);
	}
else if(iTmp->classKRH < jTmp->classKRH) {
	return(-1);
	}
else {
	return(1);
	}
}

/***********************************************************************
*  Set up the interface to the user:
*  1) Create a new interface using "PLUTO_new_interface";
*  
*  2) For each line of inputs, create the line with "PLUTO_add_option"
*  	(this line of inputs can be optional or mandatory);
*  
*  3) For each item on the line, create the item with
*  	"PLUTO_add_dataset" for a dataset chooser,
*  	"PLUTO_add_string"for a string chooser,
*  	"PLUTO_add_number"for a number chooser.
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface *PLUGIN_init(int ncall)
{
PLUGIN_interface *plint;  /* will be the output of this routine */

if(ncall >= 1) { /* only one interface */
	return NULL;
	}

   CHECK_IF_ALLOWED("REORDER","Reorder") ;  /* 30 Sep 2016 */

/*---------------- set titles and call point ----------------*/

plint = PLUTO_new_interface("Reorder",
							"Reorders voxel time-courses within a 3D+Time BRIK",
							helpstring,
							PLUGIN_CALL_VIA_MENU,
							REORDER_main);

   PLUTO_set_sequence( plint , "z:Kummer" ) ;

/*--------- 1st line: Input dataset ---------*/

PLUTO_add_option(plint,
				"Input",/* label at left of input line */
				"Input",/* tag to return to plugin */
				 TRUE		/* is this mandatory? */
				 );

PLUTO_add_dataset(plint,
				  "---->>",          /* label next to button	*/
				  ANAT_ALL_MASK,     /* take any anat datasets */
				  0,                 /* don't allow fim funcs*/
				  DIMEN_4D_MASK |    /* need 3D+time datasets*/
				  BRICK_ALLTYPE_MASK /* handle any type		*/
				  );

/*---------- 2nd line: Output dataset prefix ----------*/

PLUTO_add_option(plint,
				 "Output",  /* label at left of input line */
				 "Output",  /* tag to return to plugin */
				 TRUE       /* is this mandatory? */
				 );

PLUTO_add_string(plint,
				 "Prefix",  /* label next to textfield */
				 0,NULL,    /* no fixed strings to choose among */
				 19         /* 19 spaces for typing in value */
				 );

/*---------- 3rd line: Map file name ----------*/

PLUTO_add_option(plint,
				 "Map file",  /* label at left of input line */
				 "MapFile",   /* tag to return to plugin */
				 TRUE         /* is this mandatory? */
				 );

PLUTO_add_string(plint,
				 "FileName",  /* label next to textfield */
				 0,NULL,      /* no fixed strings to choose among */
				 19           /* 19 spaces for typing in value */
				 );

PLUTO_add_string(plint,
				 "Duplicates",          /* label next to textfield */
				 NUM_DUPACTION_STRINGS, /* number of strings to choose among */
				 dupaction_strings,     /* fixed strings to choose among */
				 0                      /* index of default (collate) */
				 );

/*--------- done with interface setup ---------*/

return plint;
}

/*************************************/
/* M A C R O   D E F I N I T I O N S */
/*************************************/

/* Free vectors  */
#undef  FREEUP
#define FREEUP(thing) if((thing) != NULL) {free((thing)); (thing) = NULL;}

#define FREE_WORKSPACE do{ FREEUP(bPtr); FREEUP(sPtr); FREEUP(fPtr); \
						   FREEUP(map); FREEUP(classKRH); } while(0)

/*****************************************************************
*  Main routine for this plugin (will be called from AFNI).
*  If the return string is not NULL, some error transpired, and
*  AFNI will popup the return string in a message box.
******************************************************************/

char *REORDER_main(PLUGIN_interface *plint)
{
MCW_idcode *idc;            /* input dataset idcode */
THD_3dim_dataset *old_dset; /* input dataset  */
THD_3dim_dataset *new_dset; /* output dataset */

ClassInfo *classKRH;           /* array of ClassInfo structures, returned by reference from 'parseMap' */

char *new_prefix;           /* pointer to the output dataset prefix */
char *mapFile;              /* pointer to the name of map file to open */
char *dupstr;               /* pointer to duplicate action label string */
char currentClass;          /* character sentinel */

register int ii;            /* register counter variables */
register int jj;
register int kk;
register int mm;

int perc;                   /* percentage value for progress meter */
int ninp;                   /* number of time points */
int nvox;                   /* number of voxels */
int old_datum;              /* data type of input dataset */
int error;                  /* general status variable */
int startIndex;             /* index sentinel */
int currentLength;          /* length of the current epoch */
int newLength;              /* length of the time-course after averaging duplicates */
int instances;              /* number of instances of the current epoch class */
int timePoint;              /* current time point being processed */
int *map;                   /* array of indices for remapping time points, returned from 'parseMap' */
int mapLength;              /* length of 'map' array, returned by reference from 'parseMap' */
int dupaction;              /* 0: collate duplicate epochs, !0: average duplicate epochs */
int classCount;             /* length of 'ClassInfo' array, returned by reference from 'parseMap' */

double sum;                 /* sum variable for averaging */
double divisor;             /* divisor for calculating the mean, this will equal 'instances' */

byte  **bPtr = NULL;        /* for byte data, will point to the 3D+time data to be reordered */
short **sPtr = NULL;        /* for short data, will point to the 3D+time data to be reordered */
float **fPtr = NULL;        /* for float data, will point to the 3D+time data to be reordered */

byte  **bData = NULL;       /* for byte data, will point to the reordered 3D+time data */
short **sData = NULL;       /* for short data, will point to the reordered 3D+time data */
float **fData = NULL;       /* for float data, will point to the reordered 3D+time data */

Bool dupsToAverage = False; /* flag to indicate whether there are segments to average */

/**********************************************************************/
/****** Check inputs from AFNI to see if they are reasonable-ish ******/

/*--------- go to first input line ---------*/

PLUTO_next_option(plint);

idc	= PLUTO_get_idcode(plint);    /* get dataset item */

old_dset = PLUTO_find_dset(idc);  /* get Ptr to dataset */
if(NULL == old_dset) {
	return "*************************\n"
			"Cannot find Input Dataset\n"
			"*************************";
	}

/*--------- go to second input line ---------*/

PLUTO_next_option(plint);

new_prefix = PLUTO_get_string(plint);   /* get string item (the output prefix) */
if(! PLUTO_prefix_ok(new_prefix)) {     /* check if it is OK */
	return "************************\n"
			"Output Prefix is illegal\n"
			"************************";
	}

/*--------- go to next input lines ---------*/

PLUTO_next_option(plint); /* skip to next line */

mapFile = PLUTO_get_string(plint);                  /* get the map filename */
if(NULL == mapFile || access(mapFile, R_OK) < 0) {  /* check if it is OK and readable */
	return "****************************\n"
			"Epoch map file is unreadable\n"
			"****************************";
	}

dupstr = PLUTO_get_string(plint);         /* get the string item */
dupaction = PLUTO_string_index(dupstr,    /* find it in the list it came from */
							   NUM_DUPACTION_STRINGS,
							   dupaction_strings);

/********************************************************/
/*********** At this point, the inputs are OK ***********/

PLUTO_popup_meter(plint);  /* popup a progress meter */

/*--------- set up pointers to each sub-brick in the input dataset ---------*/

#ifdef DEBUG_PLUGIN_REORDER
printf("[Reorder] Loading dataset into memory...\n");
#endif
DSET_load(old_dset);      /* must be in memory before we get pointers to it */

nvox = /* number of voxels per sub-brick */
	old_dset->daxes->nxx * old_dset->daxes->nyy * old_dset->daxes->nzz;

#ifdef DEBUG_PLUGIN_REORDER
printf("[Reorder] %d voxels per subBRIK [%dx%dx%d].\n"
	, nvox, old_dset->daxes->nxx, old_dset->daxes->nyy, old_dset->daxes->nzz);
#endif

ninp = DSET_NUM_TIMES(old_dset); /* get number of time points in old dataset */
#ifdef DEBUG_PLUGIN_REORDER
printf("[Reorder] %d time points [subBRIK].\n", ninp);
#endif

/********************************************************/
/*************** Parse the epoch map file ***************/

mapLength = ninp; /* initialize with the expected length for mapFile entries */
if(NULL == (map = REORDER_parseMap(mapFile
								, &mapLength
								, &classKRH
								, &classCount))) {
	return "*******************************\n"
	 	   "Critical error parsing map file\n"
		   "*******************************";
	}

#ifdef DEBUG_PLUGIN_REORDER0
printf("\n[Reorder] Indices for epoch remapping:\n");
for(ii = 0; ii < mapLength; ii++) {
	printf("%d\n", map[ii]);
	}

printf("\n[Reorder] Meta-sequence of epoch classes is:\n");
for(ii = 0; ii < classCount; ii++) {
	printf("[%d] Class %c [Width %d TRs]\n", ii, classKRH[ii].classKRH, classKRH[ii].length);
	}
#endif

/* Sort 'class' to reflect the final order
   to aid in any epoch averaging required */
qsort((void *)classKRH, classCount, sizeof(ClassInfo), compare_class);

#ifdef DEBUG_PLUGIN_REORDER
printf("\n[Reorder] Sorted meta-sequence of epoch classes is:\n");
for(ii = 0; ii < classCount; ii++) {
	printf("[%d] Class %c [Width %d TRs]\n", ii, classKRH[ii].classKRH, classKRH[ii].length);
	}
#endif

old_datum /* get old dataset datum type */
	= DSET_BRICK_TYPE(old_dset, 0);

switch(old_datum) { /* pointer type depends on input datum type */

	default:
		return "*******************************\n"
				"Illegal datum in Input Dataset\n"
				"******************************";

	/**create array of pointers into old dataset sub-bricks **/
	/**Note that we skip the first 'ignore' sub-bricks here **/

	/*---------- input is bytes -----------*/
	/* voxel #i at time #k is bPtr[k][i]   */
	/* for i = 0..nvox-1 and k = 0..ninp-1.*/

	case MRI_byte:
		/* Workspace for 3D+time data */
		bPtr = (byte **)calloc(sizeof(byte *), ninp);
		if(NULL == bPtr) {
			return "************************\n"
				   "!! Allocation Failure !!\n"
				   "************************";
			}

		for(kk = 0; kk < ninp; kk++) {
			bPtr[kk] = (byte *)DSET_ARRAY(old_dset,kk);
			}

		/* Array of pointers for reordering subBRIKS */
		bData = (byte **)calloc(sizeof(byte *), mapLength);
		if(NULL == bData) {
			FREEUP(bPtr);
			return "************************\n"
				   "!! Allocation Failure !!\n"
				   "************************";
			}
		/* Allocate memory for subBRIKS */
		for(kk = 0; kk < mapLength; kk++) {
			bData[kk] = (byte *)calloc(sizeof(byte), nvox);
			if(NULL == bData[kk]) {
				FREEUP(bPtr);
				FREEUP(bData);
				return "************************\n"
					   "!! Allocation Failure !!\n"
					   "************************";
				}
			}
		break;

	/*---------- input is shorts ----------*/
	/* voxel #i at time #k is sPtr[k][i]   */
	/* for i = 0..nvox-1 and k = 0..ninp-1.*/

	case MRI_short:
		/* Workspace for 3D+time data */
		sPtr = (short **)calloc(sizeof(short *), ninp);
		if(NULL == sPtr) {
			return "************************\n"
				   "!! Allocation Failure !!\n"
				   "************************";
			}

		for(kk = 0; kk < ninp; kk++) {
			sPtr[kk] = (short *)DSET_ARRAY(old_dset,kk);
			}

		/* Array of pointers for reordering subBRIKS */
		sData = (short **)calloc(sizeof(short *), mapLength);
		if(NULL == sData) {
			FREEUP(sPtr);
			return "************************\n"
				   "!! Allocation Failure !!\n"
				   "************************";
			}
		/* Allocate memory for subBRIKS */
		for(kk = 0; kk < mapLength; kk++) {
			sData[kk] = (short *)calloc(sizeof(short), nvox);
			if(NULL == sData[kk]) {
				FREEUP(sPtr);
				FREEUP(sData);
				return "************************\n"
					   "!! Allocation Failure !!\n"
					   "************************";
				}
			}
		break;

	/*---------- input is floats ----------*/
	/* voxel #i at time #k is fPtr[k][i]   */
	/* for i = 0..nvox-1 and k = 0..ninp-1.*/

	case MRI_float:
		/* Workspace for 3D+time data */
		fPtr = (float **)calloc(sizeof(float *), ninp);
		if(NULL == fPtr) {
			return "************************\n"
				   "!! Allocation Failure !!\n"
				   "************************";
			}

		for(kk = 0; kk < ninp; kk++) {
			fPtr[kk] = (float *)DSET_ARRAY(old_dset,kk);
			}

		/* Array of pointers for reordering subBRIKS */
		fData = (float **)calloc(sizeof(float *), mapLength);
		if(NULL == fData) {
			FREEUP(fPtr);
			return "************************\n"
				   "!! Allocation Failure !!\n"
				   "************************";
			}
		/* Allocate memory for subBRIKS */
		for(kk = 0; kk < mapLength; kk++) {
			fData[kk] = (float *)calloc(sizeof(float), nvox);
			if(NULL == fData[kk]) {
				FREEUP(fPtr);
				FREEUP(fData);
				return "************************\n"
					   "!! Allocation Failure !!\n"
					   "************************";
				}
			}
		break;

	} /* end of switch on input type */

/*********************** Make a new dataset ***********************/

new_dset /* start with copy of old one */
	= EDIT_empty_copy(old_dset);

      { char * his = PLUTO_commandstring(plint) ;
        tross_Copy_History(old_dset,new_dset) ;
        tross_Append_History(new_dset,his) ; free(his);
      }

/*-- edit some of its internal parameters --*/

ii = EDIT_dset_items(
		new_dset,
			ADN_prefix     , new_prefix,           /* filename prefix */
			ADN_malloc_type, DATABLOCK_MEM_MALLOC, /* store in memory */
			ADN_nvals      , mapLength,            /* # time points */
			ADN_ntt        , mapLength,            /* # time points */
		ADN_none);
if(0 != ii) {
	THD_delete_3dim_dataset(new_dset, False);
	switch(old_datum) {
		case MRI_byte:
			FREEUP(bPtr);
			FREEUP(bData);
			break;
		case MRI_short:
			FREEUP(sPtr);
			FREEUP(sData);
			break;
		case MRI_float:
			FREEUP(fPtr);
			FREEUP(fData);
			break;
		}
	FREE_WORKSPACE;
	return "***********************************\n"
			"Error while creating output dataset\n"
			"***********************************";
	}

/*****************************************************/
/****** Setup has ended. Now do some real work. ******/

switch(old_datum) {

	/*****************************************/
	/* voxel #i at time #k is array[kk][ii]  */
	/* for ii = 0..nvox-1 and kk = 0..ninp-1.*/

	case MRI_byte:
		#ifdef DEBUG_PLUGIN_REORDER
		printf("\n[Reorder] Collating/byte data...\n");
		#endif
		
		/* Reorder the time-course of each voxel */
		for(kk = 0; kk < mapLength; kk++) {
			if(NULL == memcpy((void *)bData[kk]
							, (void *)bPtr[map[kk]]
							, sizeof(byte)*nvox)) {
				THD_delete_3dim_dataset(new_dset, False);
				FREEUP(bPtr);
				FREEUP(bData);
				FREE_WORKSPACE;
				return "***********************************\n"
						"!! Error performing memory copy !!\n"
						"**********************************";
				}
			perc = (100 * kk) / mapLength; /* display percentage done */
			PLUTO_set_meter(plint, perc); /* on the progress meter */
			}
		
		PLUTO_set_meter(plint, 100);      /* set progress meter to 100% */
		
		if(dupaction) { /* new_dset may change in length again */
			/* Verify that averaging is possible:
			     All duplicates must have the same length.
			*/
			error = 0; /* start with no error */
			for(ii = 0; ii < classCount; ii++) {
				for(kk = 1; kk < (classCount-1); kk++) {
					if(classKRH[ii].classKRH == classKRH[kk].classKRH) {
						if(classKRH[ii].length != classKRH[kk].length) {
							error = 1; /* lengths differ! */
							break;
							}
						dupsToAverage = True; /* there is at least one duplicate to average */
						}
					}
				if(error) {
					PLUTO_popup_message(plint
							,"*********************************************************\n" 
							 "* Duplicate stimulus regions in map file must have the  *\n"
							 "* same length for averaging. Returning collated regions.*\n"
							 "*********************************************************");
					break;
					}
				}
		
			if(!error) { /* average the crap */
				#ifdef DEBUG_PLUGIN_REORDER
				printf("[Reorder] All duplicate epochs have the same length...\n");
				#endif
			
				if(dupsToAverage) {
					#ifdef DEBUG_PLUGIN_REORDER
					printf("[Reorder] Averaging duplicate epochs...\n");
					printf("[Reorder] Initial time-length is %d...\n", mapLength);
					#endif
			
					newLength = mapLength; /* length may change after averaging, track new length */
		
					/* Perform averaging */
					/* For each class, do: */
					for(ii = 0, startIndex = 0, timePoint = 0; ii < classCount;) {
						#ifdef DEBUG_PLUGIN_REORDER
						printf("[Reorder] Processing instances of class %c...\n"
							, classKRH[ii].classKRH);
						#endif
			
						/* Get the number of duplicates of the current class */
						for(jj = ii + 1, instances = 1; jj < classCount; jj++, instances++) {
							if(classKRH[ii].classKRH != classKRH[jj].classKRH) {
								break;
								}
							}
			
						#ifdef DEBUG_PLUGIN_REORDER
						printf("	%d instances of class %c...\n", instances, classKRH[ii].classKRH);
						#endif
			
						if(instances > 1) {
							#ifdef DEBUG_PLUGIN_REORDER
							printf("	Averaging %d contiguous epochs...\n", instances);
							#endif
		
							divisor = (double)instances;
		
							/* For each time point in the duplicated classes: */
							for(jj = startIndex; jj < (classKRH[ii].length+startIndex); jj++, timePoint++) {
								/* For each voxel: */
								for(kk = 0; kk < nvox; kk++) {
									/* Average the duplicate time-points for the current voxel */
									for(mm = 0, sum = 0.0; mm < instances; mm++) {
										sum = sum + (double)bData[jj+(mm*classKRH[ii].length)][kk];
										}
									bData[timePoint][kk] = (byte)((sum / divisor) + 0.5);
									}
								}
		
							/* Update final length of time-course */
							newLength = newLength - (classKRH[ii].length * (instances - 1));
		
							/* Jump over the indices of all instances of the current class */
							startIndex = startIndex + (instances *classKRH[ii].length);
		
							/* Go to the next class to process */
							ii += instances;
							}
						else { /* just copy the data points */
							#ifdef DEBUG_PLUGIN_REORDER
							printf("	Copying single epoch...\n");
							#endif
		
							for(jj = startIndex; jj < (classKRH[ii].length+startIndex); jj++, timePoint++) {
								for(kk = 0; kk < nvox; kk++) {
									bData[timePoint][kk] = bData[jj][kk];
									}
								}
		
							/* Jump to the index of the next class in the time-course */
							startIndex = startIndex + classKRH[ii].length;
		
							/* eat up another class */
							++ii;
							}
		
						/* Update progress meter with the percentage done */
						perc = (100 * timePoint) / mapLength;
						PLUTO_set_meter(plint, perc);
						}
			
					/* Set progress meter to 100% */
					PLUTO_set_meter(plint, 100);
			
					#ifdef DEBUG_PLUGIN_REORDER
					printf("[Reorder] Final time-length is %d...\n", newLength);
					#endif
					ii = EDIT_dset_items(
								new_dset,
									ADN_nvals, newLength,  /* # time points */
									ADN_ntt,   newLength,  /* # time points */
								ADN_none);
					if(0 != ii) {
						THD_delete_3dim_dataset(new_dset, False);
						FREEUP(bPtr);
						FREEUP(bData);
						FREE_WORKSPACE;
						return "******************************************\n"
								"!! Error while creating output dataset !!\n"
								"*****************************************";
						}
			
					/* Free unused subBRIKS */
					if(newLength < mapLength) {
						#ifdef DEBUG_PLUGIN_REORDER
						printf("[Reorder] Freeing %d unused subBRIKs...\n", mapLength-newLength);
						#endif
						for(ii = newLength; ii < mapLength; ii++) {
							FREEUP(bData[ii]);
							}
						bData = (byte **)realloc((void *)bData, sizeof(byte *)*newLength);
						if(NULL == bData) {
							THD_delete_3dim_dataset(new_dset, False);
							FREEUP(bPtr);
							FREE_WORKSPACE;
							return "************************\n"
								   "!! Reallocation error !!\n"
								   "************************";
							}
						}
					mapLength = newLength;
					}
				}
			}
		
		/* Write results into new dataset */
		#ifdef DEBUG_PLUGIN_REORDER
		printf("[Reorder] Writing %d subBRIKS into new data set...\n", mapLength);
		#endif
		for(kk = 0; kk < mapLength; kk++) {
			EDIT_substitute_brick(new_dset, kk, MRI_byte, bData[kk]);
			}
		break;

	case MRI_short:
		#ifdef DEBUG_PLUGIN_REORDER
		printf("\n[Reorder] Collating/short data...\n");
		#endif
		
		/* Reorder the time-course of each voxel */
		for(kk = 0; kk < mapLength; kk++) {
			if(NULL == memcpy((void *)sData[kk]
							, (void *)sPtr[map[kk]]
							, sizeof(short)*nvox)) {
				THD_delete_3dim_dataset(new_dset, False);
				FREEUP(sPtr);
				FREEUP(sData);
				FREE_WORKSPACE;
				return "***********************************\n"
						"!! Error performing memory copy !!\n"
						"**********************************";
				}
			perc = (100 * kk) / mapLength; /* display percentage done */
			PLUTO_set_meter(plint, perc); /* on the progress meter */
			}
		
		PLUTO_set_meter(plint, 100);      /* set progress meter to 100% */
		
		if(dupaction) { /* new_dset may change in length again */
			/* Verify that averaging is possible:
			     All duplicates must have the same length.
			*/
			error = 0; /* start with no error */
			for(ii = 0; ii < classCount; ii++) {
				for(kk = 1; kk < (classCount-1); kk++) {
					if(classKRH[ii].classKRH == classKRH[kk].classKRH) {
						if(classKRH[ii].length != classKRH[kk].length) {
							error = 1; /* lengths differ! */
							break;
							}
						dupsToAverage = True; /* there is at least one duplicate to average */
						}
					}
				if(error) {
					PLUTO_popup_message(plint
							,"*********************************************************\n" 
							 "* Duplicate stimulus regions in map file must have the  *\n"
							 "* same length for averaging. Returning collated regions.*\n"
							 "*********************************************************");
					break;
					}
				}
		
			if(!error) { /* average the crap */
				#ifdef DEBUG_PLUGIN_REORDER
				printf("[Reorder] All duplicate epochs have the same length...\n");
				#endif
			
				if(dupsToAverage) {
					#ifdef DEBUG_PLUGIN_REORDER
					printf("[Reorder] Averaging duplicate epochs...\n");
					printf("[Reorder] Initial time-length is %d...\n", mapLength);
					#endif
			
					newLength = mapLength; /* length may change after averaging, track new length */
		
					/* Perform averaging */
					/* For each class, do: */
					for(ii = 0, startIndex = 0, timePoint = 0; ii < classCount;) {
						#ifdef DEBUG_PLUGIN_REORDER
						printf("[Reorder] Processing instances of class %c...\n"
							, classKRH[ii].classKRH);
						#endif
			
						/* Get the number of duplicates of the current class */
						for(jj = ii + 1, instances = 1; jj < classCount; jj++, instances++) {
							if(classKRH[ii].classKRH != classKRH[jj].classKRH) {
								break;
								}
							}
			
						#ifdef DEBUG_PLUGIN_REORDER
						printf("	%d instances of class %c...\n", instances, classKRH[ii].classKRH);
						#endif
			
						if(instances > 1) {
							#ifdef DEBUG_PLUGIN_REORDER
							printf("	Averaging %d contiguous epochs...\n", instances);
							#endif
		
							divisor = (double)instances;
		
							/* For each time point in the duplicated classes: */
							for(jj = startIndex; jj < (classKRH[ii].length+startIndex); jj++, timePoint++) {
								/* For each voxel: */
								for(kk = 0; kk < nvox; kk++) {
									/* Average the duplicate time-points for the current voxel */
									for(mm = 0, sum = 0.0; mm < instances; mm++) {
										sum = sum + (double)sData[jj+(mm*classKRH[ii].length)][kk];
										}
									sData[timePoint][kk] = (short)((sum / divisor) + 0.5);
									}
								}
		
							/* Update final length of time-course */
							newLength = newLength - (classKRH[ii].length * (instances - 1));
		
							/* Jump over the indices of all instances of the current class */
							startIndex = startIndex + (instances *classKRH[ii].length);
		
							/* Go to the next class to process */
							ii += instances;
							}
						else { /* just copy the data points */
							#ifdef DEBUG_PLUGIN_REORDER
							printf("	Copying single epoch...\n");
							#endif
		
							for(jj = startIndex; jj < (classKRH[ii].length+startIndex); jj++, timePoint++) {
								for(kk = 0; kk < nvox; kk++) {
									sData[timePoint][kk] = sData[jj][kk];
									}
								}
		
							/* Jump to the index of the next class in the time-course */
							startIndex = startIndex + classKRH[ii].length;
		
							/* eat up another class */
							++ii;
							}
		
						/* Update progress meter with the percentage done */
						perc = (100 * timePoint) / mapLength;
						PLUTO_set_meter(plint, perc);
						}
			
					/* Set progress meter to 100% */
					PLUTO_set_meter(plint, 100);
			
					#ifdef DEBUG_PLUGIN_REORDER
					printf("[Reorder] Final time-length is %d...\n", newLength);
					#endif
					ii = EDIT_dset_items(
								new_dset,
									ADN_nvals, newLength,  /* # time points */
									ADN_ntt,   newLength,  /* # time points */
								ADN_none);
					if(0 != ii) {
						THD_delete_3dim_dataset(new_dset, False);
						FREEUP(sPtr);
						FREEUP(sData);
						FREE_WORKSPACE;
						return "******************************************\n"
								"!! Error while creating output dataset !!\n"
								"*****************************************";
						}
			
					/* Free unused subBRIKS */
					if(newLength < mapLength) {
						#ifdef DEBUG_PLUGIN_REORDER
						printf("[Reorder] Freeing %d unused subBRIKs...\n", mapLength-newLength);
						#endif
						for(ii = newLength; ii < mapLength; ii++) {
							FREEUP(sData[ii]);
							}
						sData = (short **)realloc((void *)sData, sizeof(short *)*newLength);
						if(NULL == sData) {
							THD_delete_3dim_dataset(new_dset, False);
							FREEUP(sPtr);
							FREE_WORKSPACE;
							return "*************************\n"
								   "!! Reallocation error !!\n"
								   "************************";
							}
						}
					mapLength = newLength;
					}
				}
			}
		
		/* Write results into new dataset */
		#ifdef DEBUG_PLUGIN_REORDER
		printf("[Reorder] Writing %d subBRIKS into new data set...\n", mapLength);
		#endif
		for(kk = 0; kk < mapLength; kk++) {
			EDIT_substitute_brick(new_dset, kk, MRI_short, sData[kk]);
			}
		break;

	case MRI_float:
		#ifdef DEBUG_PLUGIN_REORDER
		printf("\n[Reorder] Collating/float data...\n");
		#endif
		
		/* Reorder the time-course of each voxel */
		for(kk = 0; kk < mapLength; kk++) {
			if(NULL == memcpy((void *)fData[kk]
							, (void *)fPtr[map[kk]]
							, sizeof(float)*nvox)) {
				THD_delete_3dim_dataset(new_dset, False);
				FREEUP(fPtr);
				FREEUP(fData);
				FREE_WORKSPACE;
				return "***********************************\n"
						"!! Error performing memory copy !!\n"
						"**********************************";
				}
			perc = (100 * kk) / mapLength; /* display percentage done */
			PLUTO_set_meter(plint, perc); /* on the progress meter */
			}
		
		PLUTO_set_meter(plint, 100);      /* set progress meter to 100% */
		
		if(dupaction) { /* new_dset may change in length again */
			/* Verify that averaging is possible:
			     All duplicates must have the same length.
			*/
			error = 0; /* start with no error */
			for(ii = 0; ii < classCount; ii++) {
				for(kk = 1; kk < (classCount-1); kk++) {
					if(classKRH[ii].classKRH == classKRH[kk].classKRH) {
						if(classKRH[ii].length != classKRH[kk].length) {
							error = 1; /* lengths differ! */
							break;
							}
						dupsToAverage = True; /* there is at least one duplicate to average */
						}
					}
				if(error) {
					PLUTO_popup_message(plint
							,"*********************************************************\n" 
							 "* Duplicate stimulus regions in map file must have the  *\n"
							 "* same length for averaging. Returning collated regions.*\n"
							 "*********************************************************");
					break;
					}
				}
		
			if(!error) { /* average the crap */
				#ifdef DEBUG_PLUGIN_REORDER
				printf("[Reorder] All duplicate epochs have the same length...\n");
				#endif
			
				if(dupsToAverage) {
					#ifdef DEBUG_PLUGIN_REORDER
					printf("[Reorder] Averaging duplicate epochs...\n");
					printf("[Reorder] Initial time-length is %d...\n", mapLength);
					#endif
			
					newLength = mapLength; /* length may change after averaging, track new length */
		
					/* Perform averaging */
					/* For each class, do: */
					for(ii = 0, startIndex = 0, timePoint = 0; ii < classCount;) {
						#ifdef DEBUG_PLUGIN_REORDER
						printf("[Reorder] Processing instances of class %c...\n"
							, classKRH[ii].classKRH);
						#endif
			
						/* Get the number of duplicates of the current class */
						for(jj = ii + 1, instances = 1; jj < classCount; jj++, instances++) {
							if(classKRH[ii].classKRH != classKRH[jj].classKRH) {
								break;
								}
							}
			
						#ifdef DEBUG_PLUGIN_REORDER
						printf("	%d instances of class %c...\n", instances, classKRH[ii].classKRH);
						#endif
			
						if(instances > 1) {
							#ifdef DEBUG_PLUGIN_REORDER
							printf("	Averaging %d contiguous epochs...\n", instances);
							#endif
		
							divisor = (double)instances;
		
							/* For each time point in the duplicated classes: */
							for(jj = startIndex; jj < (classKRH[ii].length+startIndex); jj++, timePoint++) {
								/* For each voxel: */
								for(kk = 0; kk < nvox; kk++) {
									/* Average the duplicate time-points for the current voxel */
									for(mm = 0, sum = 0.0; mm < instances; mm++) {
										sum = sum + (double)fData[jj+(mm*classKRH[ii].length)][kk];
										}
									fData[timePoint][kk] = (float)((sum / divisor) + 0.5);
									}
								}
		
							/* Update final length of time-course */
							newLength = newLength - (classKRH[ii].length * (instances - 1));
		
							/* Jump over the indices of all instances of the current class */
							startIndex = startIndex + (instances *classKRH[ii].length);
		
							/* Go to the next class to process */
							ii += instances;
							}
						else { /* just copy the data points */
							#ifdef DEBUG_PLUGIN_REORDER
							printf("	Copying single epoch...\n");
							#endif
		
							for(jj = startIndex; jj < (classKRH[ii].length+startIndex); jj++, timePoint++) {
								for(kk = 0; kk < nvox; kk++) {
									fData[timePoint][kk] = fData[jj][kk];
									}
								}
		
							/* Jump to the index of the next class in the time-course */
							startIndex = startIndex + classKRH[ii].length;
		
							/* eat up another class */
							++ii;
							}
		
						/* Update progress meter with the percentage done */
						perc = (100 * timePoint) / mapLength;
						PLUTO_set_meter(plint, perc);
						}
			
					/* Set progress meter to 100% */
					PLUTO_set_meter(plint, 100);
			
					#ifdef DEBUG_PLUGIN_REORDER
					printf("[Reorder] Final time-length is %d...\n", newLength);
					#endif
					ii = EDIT_dset_items(
								new_dset,
									ADN_nvals, newLength,  /* # time points */
									ADN_ntt,   newLength,  /* # time points */
								ADN_none);
					if(0 != ii) {
						THD_delete_3dim_dataset(new_dset, False);
						FREEUP(fPtr);
						FREEUP(fData);
						FREE_WORKSPACE;
						return "******************************************\n"
								"!! Error while creating output dataset !!\n"
								"*****************************************";
						}
			
					/* Free unused subBRIKS */
					if(newLength < mapLength) {
						#ifdef DEBUG_PLUGIN_REORDER
						printf("[Reorder] Freeing %d unused subBRIKs...\n", mapLength-newLength);
						#endif
						for(ii = newLength; ii < mapLength; ii++) {
							FREEUP(fData[ii]);
							}
						fData = (float **)realloc((void *)fData, sizeof(float *)*newLength);
						if(NULL == fData) {
							THD_delete_3dim_dataset(new_dset, False);
							FREEUP(fPtr);
							FREE_WORKSPACE;
							return "*************************\n"
								   "!! Reallocation error !!\n"
								   "************************";
							}
						}
					mapLength = newLength;
					}
				}
			}
		
		/* Write results into new dataset */
		#ifdef DEBUG_PLUGIN_REORDER
		printf("[Reorder] Writing %d subBRIKS into new data set...\n", mapLength);
		#endif
		for(kk = 0; kk < mapLength; kk++) {
			EDIT_substitute_brick(new_dset, kk, MRI_float, fData[kk]);
			}
		break;

	} /* end of switch over input type */

DSET_unload(old_dset);

/*************** Cleanup and go home *****************/

PLUTO_add_dset(plint, new_dset, DSET_ACTION_MAKE_CURRENT);

#ifdef DEBUG_PLUGIN_REORDER
printf("[Reorder] Cleaning up workspace...\n");
#endif
FREE_WORKSPACE;

return NULL;    /* NULL string returned means all was OK */
}

#endif

