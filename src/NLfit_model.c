/*
  This file contains routines to open and initialize models. The interface 
  with the signal and noise models is accomplished using dynamic libraries.
  This file was adapted from afni_plugin.c.
  
  File:     NLfit_model.c
  Author:   B. Douglas Ward
  Date:     23 June 1997

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

#include "NLfit_model.h"

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

   /*----- sanity check and initialize -----*/

   if( dname == NULL || strlen(dname) == 0 )  return (NULL) ;
   if( ! THD_is_directory(dname) )            return (NULL) ;

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
      return (NULL) ;
   }

   rlist = THD_extract_regular_files( flist ) ;
   DESTROY_SARR(flist) ;
   if( rlist == NULL || rlist->num <= 0 ){
      DESTROY_SARR(rlist) ;
      DESTROY_MODEL_ARRAY(outar) ;
      return (NULL) ;
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
      if (strstr(fname, "model") == NULL)  continue;

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
   return (outar) ;
}


/*---------------------------------------------------------------------------*/
/*
   Routine to open and initialize a single MODEL
*/

NLFIT_MODEL * NLFIT_read_MODEL( char * fname )
{
   NLFIT_MODEL * model ;

   /*----- sanity checks -----*/

   if( fname == NULL || strlen(fname) == 0 )  return (NULL) ;
   if( ! THD_is_file(fname) )                 return (NULL) ;

   /*----- make space for new MODEL -----*/

   model = (NLFIT_MODEL *) XtMalloc( sizeof(NLFIT_MODEL) ) ;
   model->type = NLFIT_MODEL_TYPE ;

   /*----- copy name into model structure -----*/

   MCW_strncpy( model->libname , fname , MAX_MODEL_NAME ) ;

   /*----- open the library (we hope) -----*/

   DYNAMIC_OPEN( fname , model->libhandle ) ;
   if( ! ISVALID_DYNAMIC_handle( model->libhandle ) ){

     if (NL_DEBUG)
       { 
	 char str[256]; 
	 sprintf (str,"failed to open library %s \n",fname); 
	 printf (str); 
       }

      myXtFree(model) ;
      return (NULL) ;
   }

   if (NL_DEBUG)
     { 
       char str[256] ;
       sprintf (str,"opened library %s with handle %p \n" , 
	       fname,model->libhandle ) ;
       printf (str) ; 
     }


   /*----- find the required symbols -----*/

   DYNAMIC_SYMBOL(model->libhandle, "initialize_model" , 
		  model->libinit_func );

   /*----- if symbols not found, complain and kill this MODEL -----*/

   if( model->libinit_func == (vptr_func *) NULL ){

     if (NL_DEBUG)
       { 
	 char str[256] ;
	 sprintf (str,"library %s lacks required global symbol \n",fname) ; 
	 printf (str) ; 
       }

      DYNAMIC_CLOSE( model->libhandle ) ;
      myXtFree(model) ;
      return (NULL) ;
   }

   /*----- create interface(s) by calling initialization function -----*/

   model->interface = (MODEL_interface *) model->libinit_func() ;
   if( model->interface == NULL ) 
     {
       DYNAMIC_CLOSE( model->libhandle ) ;
       myXtFree(model) ;
       return (NULL) ;
     }

   if (NL_DEBUG)
     { 
       char str[256] ;
       sprintf (str,"Interface created for %s model\n",
		model->interface->label) ; 
       printf (str) ; 
     }

   /*----- done -----*/

   return (model) ;
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

   /*----- sanity checks -----*/

   epath = getenv("AFNI_MODELPATH") ;     /* get the path list to read from */

   if( epath == NULL )
      epath = getenv("AFNI_PLUGINPATH") ; /* try another name? */

   if( epath == NULL )
      epath = getenv("PATH") ;             /* try another name? */

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


   /*----- extract blank delimited strings;
           use as directory names to get libraries -----*/

   INIT_MODEL_ARRAY( outar ) ;
   epos = 0 ;

   do{
      ii = sscanf( elocal+epos , "%s%n" , ename , &id ) ; /* next substring */
      if( ii < 1 ) break ;                          /* none --> end of work */

      /** check if ename occurs earlier in elocal **/

      eee = strstr( elocal , ename ) ;
      if( eee != NULL && (eee-elocal) < epos ){ epos += id ; continue ; }

      epos += id ;                               /* char after last scanned */

      ii = strlen(ename) ;                            /* make sure name has */
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
   return (outar) ;
}


