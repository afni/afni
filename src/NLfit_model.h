/*
  This file contains header information for NLfit_model.c.
  This file was adapted from afni_plugin.h.

  File:     NLfit_model.h
  Author:   B. Douglas Ward
  Date:     6 June 1997

*/


/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This software is Copyright 1997 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application. The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
*/

/*---------------------------------------------------------------------------*/

#ifndef _NLFIT_MODEL_HEADER_
#define _NLFIT_MODEL_HEADER_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "mrilib.h"

struct NLFIT_MODEL_array ; /* incomplete definition */

/*******************************************************************
   Define macros and typedefs for opening, closing, and finding
   symbols from dynamic libraries.  This is not done the same
   way on all Unixoid systems, unfortunately (that is to say,
   HP-UX is different).
*******************************************************************/

typedef void (*vfp)();       /* pointer to generic function */

#ifndef VOID_FUNC
#define VOID_FUNC
typedef void void_func() ;
#endif

#ifndef _AFNI_PLUGIN_HEADER_

typedef int int_func() ;     /* generic function returning integer */
typedef void * vptr_func() ; /* generic function returning void *  */
typedef char * cptr_func() ; /* generic function returning char *  */

/***************** The dlfcn.h and dl library ****************/

#ifdef DYNAMIC_LOADING_VIA_DL
#  include <dlfcn.h>
   typedef void * DYNAMIC_handle ;

#  define ISVALID_DYNAMIC_handle(handle) ((handle) != (DYNAMIC_handle) 0)

#  define DYNAMIC_OPEN(libname,handle) \
      (handle) = dlopen( (libname) , RTLD_LAZY )

#  define DYNAMIC_CLOSE(handle) \
      (void) dlclose( (handle) )

#  define DYNAMIC_SYMBOL(handle,symbol,address) \
      (address) = dlsym( (handle) , (symbol) )

#  define DYNAMIC_suffix ".so"
#endif

/****************** The dl.h and dld library ******************/

#ifdef DYNAMIC_LOADING_VIA_SHL
#  include <dl.h>
   typedef shl_t DYNAMIC_handle ;

#  define ISVALID_DYNAMIC_handle(handle) ((handle) != (DYNAMIC_handle) 0)

#  define DYNAMIC_OPEN(libname,handle) \
      (handle) = shl_load( (libname) , BIND_DEFERRED , 0L )

#  define DYNAMIC_CLOSE(handle) \
      (void) shl_unload( (handle) )

#  define DYNAMIC_SYMBOL(handle,symbol,address)      \
      do{ (address) = NULL ;                         \
          (void) shl_findsym( &(handle) , (symbol) , \
                              TYPE_UNDEFINED , &(address) ) ; } while(0)

#  define DYNAMIC_suffix ".sl"
#endif

#ifndef DYNAMIC_suffix
#  error "Plugins not properly set up -- see machdep.h"
#endif

#endif

/*****************************************************************
   Data to define the interface between a MODEL and NLFIT
******************************************************************/

/*----- dimensions of various arrays -----*/
#define MAX_NAME_LENGTH  80          /* for model and file names */
#define MAX_PARAMETERS   10          /* maximum number of model parameters */
#define MAX_MODELS       50          /* maximum number of models */

/*----- model type codes -----*/
#define MODEL_NOISE_TYPE   0
#define MODEL_SIGNAL_TYPE  1 


/** macro to copy string into MODEL label array,
    filling with blanks or truncating length, as needed **/
#define MODEL_LABEL_strcpy(mlab,str)                                \
   do{ int ll=strlen((str)) , ii ;                                   \
       if( ll >= MAX_NAME_LENGTH ) ll = MAX_NAME_LENGTH - 1 ;    \
       for( ii=0 ; ii < ll ; ii++ ) (mlab)[ii] = (str)[ii] ;         \
       for( ; ii < MAX_NAME_LENGTH - 1 ; ii++ ) (mlab)[ii] = ' ' ; \
       mlab[MAX_NAME_LENGTH - 1] = '\0' ; } while(0)


typedef struct {
  char label[MAX_NAME_LENGTH];          /* name of the model */
  int  model_type;                      /* noise or signal model? */
  int  params;                          /* number of parameters */
  char plabel[MAX_PARAMETERS][MAX_NAME_LENGTH];   /* parameter labels */
  float min_constr[MAX_PARAMETERS];   /* minimum parameter constraints */
  float max_constr[MAX_PARAMETERS];   /* maximum parameter constraints */
  void_func * call_func ;             /* function which implements the model */
} MODEL_interface ;


/**************************************************************************/
/***** Define data structures to hold control information for MODELs *****/

#define NLFIT_MODEL_TYPE        1066
#define ISVALID_NLFIT_MODEL(pl) ((pl)!=NULL && (pl)->type==NLFIT_MODEL_TYPE)

#define MAX_MODEL_NAME 128

/*** one MODEL ***/

typedef struct {
   int type ;        /* identifier */

   char              libname[MAX_MODEL_NAME] ;
   DYNAMIC_handle    libhandle ;
   vptr_func       * libinit_func ;
   MODEL_interface * interface ;
} NLFIT_MODEL ;

/*** dynamic array of many MODELs ***/

typedef struct NLFIT_MODEL_array {
   int num , nall ;
   NLFIT_MODEL ** modar ;
} NLFIT_MODEL_array ;

/*** macros to create, add to, destroy, and free an array of MODELs ***/

#define INC_MODEL_ARRAY 8

/** "name" is a variable of type (NLFIT_MODEL_array *) **/

#define INIT_MODEL_ARRAY(name)                                                \
   do{ int iq ;                                      		              \
     (name)       = (NLFIT_MODEL_array *) malloc(sizeof(NLFIT_MODEL_array));  \
     (name)->num  = 0 ;                                                       \
     (name)->nall = INC_MODEL_ARRAY ;                                         \
     (name)->modar= (NLFIT_MODEL **)malloc(sizeof(NLFIT_MODEL*)*(name)->nall);\
     for (iq=(name)->num; iq < (name)->nall; iq++ ) (name)->modar[iq] = NULL; \
     } while(0)

/** "model" is a variable of type (NLFIT_MODEL *) **/

#define ADDTO_MODEL_ARRAY(name,model)                                         \
   do{ int nn , iq ;                                                          \
       if( (name)->num == (name)->nall ){                                     \
          nn = (name)->nall = 1.1*(name)->nall + INC_MODEL_ARRAY ;            \
          (name)->modar = realloc( (name)->modar,sizeof(NLFIT_MODEL *)*nn );  \
          for( iq=(name)->num ; iq < (name)->nall ; iq++ )                    \
	    (name)->modar[iq] = NULL ;}                                       \
       nn = (name)->num ; ((name)->num)++ ;                                   \
       (name)->modar[nn] = (model) ;                                          \
     } while(0)

/** this frees all the memory associated with this array **/

#define DESTROY_MODEL_ARRAY(name)                                      \
   do{ int nn ;                                                         \
       if( (name) != NULL ){                                            \
          for( nn=0 ; nn < (name)->num ; nn++ )                         \
             if( (name)->modar[nn] != NULL ) free( (name)->modar[nn] ) ;  \
          free( (name)->modar ) ; free((name)) ; (name) = NULL ;         \
       } } while(0)

/** this just frees the control data associated
    with this array -- the actual MODELs are not freed. **/

#define FREE_MODEL_ARRAY(name)                                         \
   do{ int nn ;                                                         \
       if( (name) != NULL ){                                            \
          free( (name)->modar ) ; free((name)) ; (name) = NULL ;         \
       } } while(0)

/*---------------------------------------------------------------------------*/

/*----- Other prototypes -----*/

extern NLFIT_MODEL_array * NLFIT_get_all_MODELs (char * dname);
extern NLFIT_MODEL *       NLFIT_read_MODEL (char * fname);
extern NLFIT_MODEL_array * NLFIT_get_many_MODELs (void);

/*---------------------------------------------------------------------------*/
/*----- Array of pointers to functions that are needed by the model_conv*.c  */
/*----- routines.  This array is just to force loading of these functions    */
/*----- from libraries. ----- RW Cox (21 July 1998) -------------------------*/

#ifndef RWC_FORCED_LOADS
#define RWC_FORCED_LOADS

static vptr_func * NL_forced_loads[] = {
   (vptr_func *) mri_read_ascii ,
   (vptr_func *) mri_to_float ,
   (vptr_func *) mri_transpose ,
   (vptr_func *) mri_free ,
NULL } ;

vptr_func * RWC_forced_loads(int n){  /* this function is to try to ensure   */
  return NL_forced_loads[n] ;         /* that the array isn't optimized away */
}

#endif


/*---------------------------------------------------------------------------*/

#endif /* _NLFIT_MODEL_HEADER_ */


