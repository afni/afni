/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This is the header file for Intracranial.c.

  File:    Intracranial.h
  Author:  B. Douglas Ward
  Date:    04 June 1999
*/

/*---------------------------------------------------------------------------*/
/*
  Global variables and constants.
*/


static THD_3dim_dataset * anat;       /* input anatomical dataset  */

static float min_val_float = 25.0;    /* minimum voxel intensity limit */
static int   min_val_int   = 25;
static float max_val_float = 150.0;   /* maximum voxel intensity limit */
static int   max_val_int   = 150;

static float min_conn_float = 4.0;    /* minimum voxel connectivity to enter */
static int   min_conn_int   = 4;
static float max_conn_float = 2.0;    /* maximum voxel connectivity to leave */
static int   max_conn_int   = 2;

#define MAX_STRING_LENGTH 80


/*---------------------------------------------------------------------------*/

/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
if((ptr)==NULL) \
( SI_error ("Cannot allocate memory") )
     
/*---------------------------------------------------------------------------*/
/*
  Forward declarations
*/

void check_one_output_file 
(
  char * filename                   /* name of output file */
);
     
/*---------------------------------------------------------------------------*/




