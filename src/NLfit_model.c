/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
  This file contains routines to open and initialize models. The interface 
  with the signal and noise models is accomplished using dynamic libraries.
  This file was adapted from afni_plugin.c.
  
  File:     NLfit_model.c
  Author:   B. Douglas Ward
  Date:     23 June 1997

*/

/*---------------------------------------------------------------------------*/

#include "NLfit_model.h"
#include "debugtrace.h"

/*----- Compile this only if plugins are properly enabled in machdep.h -----*/
#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

#define EMPTY_STRING "\0"
#define NL_DEBUG 0


/*---------------------------------------------------------------------------*/
/*
   Routine to read in all MODELs found in a given directory
*/


NLFIT_MODEL_array * NLFIT_get_all_MODELs( char * dname )
{
   THD_string_array * flist , * rlist ;
   int ir , ii ;
   char * fname , * suff ;
   NLFIT_MODEL_array * outar ;
   NLFIT_MODEL       * model ;

ENTRY("NLFIT_get_all_MODELs") ;

   /*----- sanity check and initialize -----*/

   if( dname == NULL || strlen(dname) == 0 )  RETURN (NULL) ;
   if( ! THD_is_directory(dname) )            RETURN (NULL) ;

   INIT_MODEL_ARRAY( outar ) ;

   if (NL_DEBUG)
     { 
       char str[256] ; 
       sprintf (str,"scanning directory %s \n",dname) ; 
       printf (str) ; 
     }

   /*----- find all filenames -----*/

   flist = THD_get_all_filenames( dname ) ;
   if( flist == NULL || flist->num <= 0 ){
      DESTROY_SARR(flist) ;
      DESTROY_MODEL_ARRAY(outar) ;
      RETURN (NULL) ;
   }

   rlist = THD_extract_regular_files( flist ) ;
   DESTROY_SARR(flist) ;
   if( rlist == NULL || rlist->num <= 0 ){
      DESTROY_SARR(rlist) ;
      DESTROY_MODEL_ARRAY(outar) ;
      RETURN (NULL) ;
   }

  if (NL_DEBUG)
    { 
      char str[256] ; 
      sprintf(str,"%d files to scan \n",rlist->num) ; 
      printf (str) ; 
    }


   /*----- scan thru and find all filenames ending in DYNAMIC_suffix -----*/

   for( ir=0 ; ir < rlist->num ; ir++ ){
      fname = rlist->ar[ir] ; if( fname == NULL ) continue ;
      if (strstr(fname, "model_") == NULL)  continue;

      suff = strstr(fname,DYNAMIC_suffix) ;
      if( suff != NULL  &&  strlen(suff) == strlen(DYNAMIC_suffix)){
         model  = NLFIT_read_MODEL( fname ) ;
         if( model != NULL ) ADDTO_MODEL_ARRAY( outar , model ) ;
      }
   }

  if (NL_DEBUG)
    { 
      char str[256] ;
      sprintf (str,"directory %s has %d MODELs \n",dname,outar->num) ; 
      printf (str) ; 
    }


   DESTROY_SARR(rlist) ;
   if( outar->num == 0 ) DESTROY_MODEL_ARRAY(outar) ;
   RETURN (outar) ;
}


/*---------------------------------------------------------------------------*/
/*
   Routine to open and initialize a single MODEL
*/

NLFIT_MODEL * NLFIT_read_MODEL( char * fname )
{
   NLFIT_MODEL * model ;
   static int firsterr=1 ;

ENTRY("NLFIT_read_MODEL") ;

   /*----- sanity checks -----*/

   if( fname == NULL || strlen(fname) == 0 )  RETURN (NULL) ;
   if( ! THD_is_file(fname) )                 RETURN (NULL) ;

   /*----- make space for new MODEL -----*/

   model = (NLFIT_MODEL *) XtMalloc( sizeof(NLFIT_MODEL) ) ;
   model->type = NLFIT_MODEL_TYPE ;

   /*----- copy name into model structure -----*/

   MCW_strncpy( model->libname , fname , MAX_MODEL_NAME ) ;

   /*----- open the library (we hope) -----*/

   DYNAMIC_OPEN( fname , model->libhandle ) ;
   if( ! ISVALID_DYNAMIC_handle( model->libhandle ) ){
      char *er ;
      if( firsterr ){ fprintf(stderr,"\n"); firsterr=0; }
      fprintf (stderr,"failed to open library %s ",fname); 
      er = (char *)DYNAMIC_ERROR_STRING ;
      if( er != NULL ) fprintf(stderr," -- %s\n",er) ;
      else             fprintf(stderr,"\n") ;
      myXtFree(model) ;
      RETURN (NULL) ;
   }

   if (NL_DEBUG)
     { 
       char str[256] ;
       sprintf (str,"opened library %s with handle %p \n" , 
	       fname,model->libhandle ) ;
       printf (str) ; 
     }


   /*----- find the required symbols -----*/
   /*..... 13 Sep 2001: add _ for Mac OS X [RWCox] .....*/
   /*..... 30 Oct 2003: remove it for OS X 10.3    .....*/

#ifndef NEED_UNDERSCORE
   DYNAMIC_SYMBOL(model->libhandle, "initialize_model" , 
		  model->libinit_func );
#else
   DYNAMIC_SYMBOL(model->libhandle, "_initialize_model" , 
		  model->libinit_func );
#endif

   /*----- if symbols not found, complain and kill this MODEL -----*/

   if( model->libinit_func == (vptr_func *) NULL ){
      char *er = (char *)DYNAMIC_ERROR_STRING ;
      if( firsterr ){ fprintf(stderr,"\n"); firsterr=0; }
      fprintf(stderr,"model %s lacks initialize_model() function\n",fname) ;
      if( er != NULL ) fprintf(stderr," -- %s\n",er) ;
      DYNAMIC_CLOSE( model->libhandle ) ;
      myXtFree(model) ;
      RETURN (NULL) ;
   }

   /*----- create interface(s) by calling initialization function -----*/

   model->interface = (MODEL_interface *) model->libinit_func() ;
   if( model->interface == NULL ) 
     {
       DYNAMIC_CLOSE( model->libhandle ) ;
       myXtFree(model) ;
       RETURN (NULL) ;
     }

   if (NL_DEBUG)
     { 
       char str[256] ;
       sprintf (str,"Interface created for %s model\n",
		model->interface->label) ; 
       printf (str) ; 
     }

   /*----- done -----*/

   RETURN (model) ;
}


/*---------------------------------------------------------------------------*/
/*
   Routine to read in all MODELs in the desired list of directories
*/

NLFIT_MODEL_array * NLFIT_get_many_MODELs(void)
{
   char * epath , * elocal , * eee ;
   char ename[THD_MAX_NAME] , efake[]="/usr/local/lib/afni:./" ;
   NLFIT_MODEL_array * outar , * tmpar ;
   int epos , ll , ii , id ;
   THD_string_array *qlist ;  /* 02 Feb 2002 */

ENTRY("NLFIT_get_many_MODELs") ;

   /*----- sanity checks -----*/

   epath = my_getenv("AFNI_MODELPATH") ;     /* get the path list to read from */

   if( epath == NULL )
      epath = my_getenv("AFNI_PLUGINPATH") ; /* try another name? */

   if( epath == NULL )
      epath = my_getenv("PATH") ;             /* try another name? */

   if( epath == NULL ) epath = efake ;     /* put in a fake path instead? */

   /*----- copy path list into local memory -----*/

   ll = strlen(epath) ;
   elocal = (char *) XtMalloc( sizeof(char) * (ll+2) ) ;

   /*----- put a blank at the end -----*/

   strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;

   /*----- replace colons with blanks -----*/

   for( ii=0 ; ii < ll ; ii++ )
      if( elocal[ii] == ':' ) elocal[ii] = ' ' ;

   if (NL_DEBUG)
     { 
       printf ("paths to be searched for MODELs follow:") ;
       printf("%s\n",elocal) ; 
       fflush(stdout) ; 
     }


   INIT_SARR(qlist) ; /* 02 Feb 2002: list of searched directories */

   /*----- extract blank delimited strings;
           use as directory names to get libraries -----*/

   INIT_MODEL_ARRAY( outar ) ;
   epos = 0 ;

   do{
      ii = sscanf( elocal+epos , "%s%n" , ename , &id ) ; /* next substring */
      if( ii < 1 || id < 1 ) break ;                     /* none --> end of work */
      epos += id ;                               /* char after last scanned */

      if( !THD_is_directory(ename) ) continue ;  /* 21 May 2002 - rcr */

      /* 02 Feb 2002: check if ename has already been checked */

      for( ii=0 ; ii < qlist->num ; ii++ )
         if( THD_equiv_files(qlist->ar[ii],ename) ) break ;
      if( ii < qlist->num ) continue ;
      ADDTO_SARR(qlist,ename) ;

      ii = strlen(ename) ;                           /* make sure name has */
      if( ename[ii-1] != '/' ){                     /* a trailing '/' on it */
	ename[ii]  = '/' ; ename[ii+1] = '\0' ; 
      }

      tmpar = NLFIT_get_all_MODELs( ename ) ;        /* read this directory */
      if( tmpar != NULL ){
         for( ii=0 ; ii < tmpar->num ; ii++ )     /* move results to output */
            ADDTO_MODEL_ARRAY( outar , tmpar->modar[ii] ) ;

         FREE_MODEL_ARRAY(tmpar) ;                      /* toss temp array */
      }
   } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */

   myXtFree(elocal) ;

   if (NL_DEBUG)
     { 
       char str[256] ; 
       sprintf (str,"found %d MODELs \n",outar->num) ; 
       printf (str) ; 
     }

   if( outar->num == 0 ) DESTROY_MODEL_ARRAY(outar) ;

   DESTROY_SARR(qlist) ; /* 02 Feb 2002 */
   RETURN (outar) ;
}


