#include "mrilib.h"
#include "thd.h"

static int               einit = 0 ;
static THD_string_array *elist = NULL ;

THD_string_array *get_elist(void) { 
   if( !einit ){ 
      einit = 1 ; 
      elist = THD_getpathprogs(NULL, 1) ; 
   }
   return(elist); 
}

/*----------------------------------------------------------------------------*/
/*! Find an executable in the PATH by its name, if it exists.
    If not, NULL is returned.  If it exists, a pointer to static storage
    is returned (i.e., don't free() this pointer!).
------------------------------------------------------------------------------*/

char * THD_find_executable( char *ename )
{
   char *etr , *str ;
   int ii ;

ENTRY("THD_find_executable") ;

   if( !einit ){ einit = 1 ; elist = THD_getpathprogs(NULL, 1) ; }
   if( elist == NULL ) RETURN(NULL) ;

   etr = THD_trailname( ename , 0 ) ;

   for( ii=0 ; ii < elist->num ; ii++ ){
      str = THD_trailname( elist->ar[ii] , 0 ) ;
      if( strcmp(str,etr) == 0 ) RETURN(elist->ar[ii]) ;
   }

   RETURN(NULL) ;
}

/*----------------------------------------------------------------------------*/
/*! Find afni's bin directory if it exists.
    If not, NULL is returned.  If it exists, a pointer to the path is returned.
    Do free it with free()
------------------------------------------------------------------------------*/
char * THD_abindir (byte withslash) 
{
   char *afr = NULL, *af=NULL;
   int  nn = 0, N_afni=strlen("afni");
   THD_string_array *elist=NULL;
   
   if (!(elist = get_elist()) ||
       !(af = THD_find_executable("afni"))) {
      ERROR_message("Could not find afni, we're doomed daddy!");
      RETURN(NULL);
   }
   
   /* remove afni from the end to get the path */
   nn = strlen(af);
   if (strcmp(af+nn-N_afni,"afni")) {
      ERROR_message("This should not be (%s)!", af+nn-N_afni);
      RETURN(NULL);
   }
   
   afr = strdup(af);
   afr[strlen(af)-N_afni]='\0'; 
   
   /* remove slash */
   while ( (nn=strlen(afr)-1) && afr[nn] == '/') 
      afr[nn] = '\0';

   if (withslash) {
      nn=strlen(afr);
      afr[nn] = '/'; afr[nn+1]='\0';
   }
   return(afr);
}

/*----------------------------------------------------------------------------*/
/*! Find a regular file in the PATH by its name, if it exists.
    Does not include directories.
    If not, NULL is returned.
    If it exists, a pointer to malloc-ed storage is returned
    (e.g., free it when you are done).
    
    thispath is a user supplied ':' delimited path string of the form
      somewhere/here:/over/there . If null then path is taken from 
      the env PATH
------------------------------------------------------------------------------*/

char * THD_find_regular_file( char *ename, char *thispath )
{
   char *fullname , *str ;
   int id , ii ;
   char *epath;
ENTRY("THD_find_regular_file") ;
   
   if (!thispath) epath = my_getenv( "PATH" ) ;
   else epath = thispath;
   
   if( epath != NULL ){
      int epos =0 , ll = strlen(epath) ;
      char *elocal ;
      char dirname[THD_MAX_NAME] ;

      /* copy path list into local memory */

      elocal = (char *) malloc( sizeof(char) * (ll+2) ) ;
      strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;
      fullname = (char *) malloc( sizeof(char) * THD_MAX_NAME);

      /* replace colons with blanks */
      for( ii=0 ; ii < ll ; ii++ )
         if( elocal[ii] == ':' ) elocal[ii] = ' ' ;

      /* extract blank delimited strings,
         use as directory names to get timeseries files */

      do{
         ii = sscanf( elocal+epos , "%s%n" , dirname , &id ) ;
         if( ii < 1 ) break ;  /* no read ==> end of work */
         epos += id ;          /* epos = char after last one scanned */

         ii = strlen(dirname) ;                      /* make sure name has */
         if( dirname[ii-1] != '/' ){                 /* a trailing '/' on it */
            dirname[ii]  = '/' ; dirname[ii+1] = '\0' ;
         }
         if( !THD_is_directory(dirname) ) continue ;    /* 25 Feb 2002 */

         sprintf(fullname, "%s%s",dirname,ename);
         if( THD_is_file(fullname) ) {
            /* found the file in the current directory */
            free(elocal) ;
            RETURN(fullname);
         }

      } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */

      free(elocal) ; free(fullname);
   }

   RETURN(NULL) ;
}

/*===========================================================================*/
/*! Return a list of all executable files in the PATH and the dlist. */

THD_string_array * THD_getpathprogs( THD_string_array *dlist, char exec_flag )
{
   int id , ii , ndir ;
   char *epath , *eee ;
   THD_string_array *elist , *tlist , *qlist ;

ENTRY("THD_getpathprogs") ;

   /*----- sanity check and initialize -----*/

   epath = my_getenv( "PATH" ) ;
   ndir  = (dlist != NULL) ? dlist->num : 0 ;

   if( ndir == 0 && epath == NULL ) RETURN(NULL) ;

   INIT_SARR(elist) ;
   INIT_SARR(qlist) ;  /* 04 Feb 2002: list of searched directories */

   /*----- for each input directory, find all files / executable files -----*/

   for( id=0 ; id < ndir ; id++ ){

      tlist = THD_get_all_files( dlist->ar[id], exec_flag ) ;
      if( tlist == NULL ) continue ;

      for( ii=0 ; ii < tlist->num ; ii++ )  /* copy names to output array */
         ADDTO_SARR( elist , tlist->ar[ii] ) ;

      ADDTO_SARR(qlist,dlist->ar[id]) ;     /* 04 Feb 2002 */

      DESTROY_SARR(tlist) ;
   }

   /*----- also do directories in environment path, if any -----*/

   if( epath != NULL ){
      int epos =0 , ll = strlen(epath) ;
      char *elocal ;
      char ename[THD_MAX_NAME] ;

      /* copy path list into local memory */

      elocal = (char *) malloc( sizeof(char) * (ll+2) ) ;
      strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;

      /* replace colons with blanks */

      for( ii=0 ; ii < ll ; ii++ )
         if( elocal[ii] == ':' ) elocal[ii] = ' ' ;

      /* extract blank delimited strings,
         use as directory names to get timeseries files */

      do{
         ii = sscanf( elocal+epos , "%s%n" , ename , &id ) ;
         if( ii < 1 ) break ;  /* no read ==> end of work */
         epos += id ;          /* epos = char after last one scanned */

         ii = strlen(ename) ;                         /* make sure name has */
         if( ename[ii-1] != '/' ){                    /* a trailing '/' on it */
            ename[ii]  = '/' ; ename[ii+1] = '\0' ;
         }
         if( !THD_is_directory(ename) ) continue ;    /* 25 Feb 2002 */

         /* 04 Feb 2002: check if we already searched this directory */

         for( ii=0 ; ii < qlist->num ; ii++ )
            if( THD_equiv_files(qlist->ar[ii],ename) ) break ;
         if( ii < qlist->num ) continue ;  /* skip this directory */
         ADDTO_SARR(qlist,ename) ;

         /* read this directory */
         tlist = THD_get_all_files( ename, exec_flag ) ;
         if( tlist != NULL ){
            for( ii=0 ; ii < tlist->num ; ii++ )    /* move names to output */
               ADDTO_SARR( elist , tlist->ar[ii] ) ;
            DESTROY_SARR(tlist) ;
         }

      } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */

      free(elocal) ;
   }

   if( SARR_NUM(elist) == 0 ) DESTROY_SARR(elist) ;

   DESTROY_SARR(qlist) ;  /* 04 Feb 2002 */
   RETURN(elist) ;
}

/*--------------------------------------------------*/
/*! Read all regular files or executable filenames from a directory. */

THD_string_array * THD_get_all_files( char *dname, char exec_flag )
{
   int ir , ll , ii ;
   char *fname , *tname ;
   float *far ;
   THD_string_array *outar, *alist, *rlist ;

ENTRY("THD_get_all_files") ;

   /*----- sanity check and initialize -----*/

   if( dname == NULL || strlen(dname) == 0 ) RETURN(NULL) ;

   /*----- find all regular files -----*/

if(PRINT_TRACING){
char str[256];sprintf(str,"call THD_get_all_filenames(%s)",dname); STATUS(str);
}
   alist = THD_get_all_filenames( dname ) ;

   if( alist == NULL ) RETURN(NULL) ;
STATUS("call THD_extract_regular_files") ;
   rlist = THD_extract_regular_files( alist ) ;
   DESTROY_SARR( alist ) ;
   if( rlist == NULL ) RETURN(NULL) ;

   /* return regular list if not looking for executables */
   if(!exec_flag) RETURN(rlist);

   INIT_SARR( outar ) ;

   /* 04 Feb 2002: don't include .so libraries, etc. */

   for( ir=0 ; ir < rlist->num ; ir++ ){
      fname = rlist->ar[ir] ;
      if( THD_is_executable(fname) &&
          !strstr(fname,".so")     &&
          !strstr(fname,".la")       ) {
          ADDTO_SARR(outar,fname) ;
      }
   }

   DESTROY_SARR(rlist) ;

   if( SARR_NUM(outar) == 0 ) DESTROY_SARR(outar) ;

   RETURN( outar );
}

/*! Get all executables in directory where afni resides */
THD_string_array * THD_get_all_afni_executables(void )
{
   THD_string_array *outar=NULL, *elist=NULL;
   char *af=NULL, *etr=NULL;
   int N_af, iaf=0, ii=0, smode, *isrt=NULL;
   char scomm[256]={""};
   
   ENTRY("THD_get_all_afni_executables");
   
   if (!(elist = get_elist()) ||
       !(af = THD_abindir(1)) ) {
      ERROR_message("Could not find afni, we're doomed daddy!");
      RETURN(outar);
   }
   
   N_af = strlen(af);
   
   /* Now get all executables under af */
   INIT_SARR( outar );
   for (ii=0, iaf=0; ii<elist->num ; ii++ ){
      smode = storage_mode_from_filename(elist->ar[ii]);
      etr = THD_trailname( elist->ar[ii] , 0 ) ; 
      if (
          !THD_is_directory(elist->ar[ii]) &&
          !strncmp(af, elist->ar[ii], N_af) &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".jpg") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".a") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".c") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".h") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".f") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".xbm") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".tex") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".lib") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".so") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".la") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".txt") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".R") &&
          !(STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".py") &&
             /* add a couple of python types to skip  20 Jul 2012 [rickr] */
             !strncmp(etr,"lib_",4) && !strncmp(etr,"gui_",4) &&
             !strncmp(etr,"ui_",4) ) &&
          (smode <= STORAGE_UNDEFINED || smode >= LAST_STORAGE_MODE)  &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".sumarc") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".afnirc")&&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], "lib.py") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".pyc") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".Xdefaults") &&
          /* skip README files, processed as scripts     20 Jul 2012 [rickr]
             of note: README.atlas_building starts with the line:
                      README.atlas_building
             such process recursion is not good for the system... */
          strncmp(etr, "README.", 7)
              )  {
         ADDTO_SARR( outar , elist->ar[ii] ) ; ++iaf;
         /* fprintf(stderr," %d- %s\n", iaf, etr); */
      } else {
         /* fprintf(stderr," skip %s (%s) %d--%d--%d isd %d\n", 
               elist->ar[ii], af, STORAGE_UNDEFINED, smode, LAST_STORAGE_MODE,
               THD_is_directory(elist->ar[ii])); */
      }
   } 
   
   qsort(outar->ar, outar->num, sizeof(char*), 
      (int(*) (const void *, const void *))compare_string);
   
   if( SARR_NUM(outar) == 0 ) DESTROY_SARR(outar) ;
   
   if (af) free(af); af = NULL;
   
   RETURN( outar );
}

/*! Get all readme files in directory where afni resides */
THD_string_array * THD_get_all_afni_readmes(void )
{
   THD_string_array *outar=NULL, *elist=NULL;
   char *af=NULL, *etr=NULL, *key="README.";
   int N_af, N_afni=strlen("afni"), iaf=0, ii=0, *isrt=NULL, N_key=0;
   char scomm[256]={""};
   
   ENTRY("THD_get_all_afni_readmes");
   
   if (!(elist = get_elist()) ||
       !(af = THD_abindir(1))) {
      ERROR_message("Could not find afni, we're doomed daddy!");
      RETURN(outar);
   }
   
   /* remove afni from the end to get the path */
   N_af = strlen(af);
   
   elist = THD_get_all_files(af,'\0');
   
   /* Now get all readmes under af */
   N_key = strlen(key);
   INIT_SARR( outar );
   for (ii=0, iaf=0; ii<elist->num ; ii++ ){
      etr = THD_trailname( elist->ar[ii] , 0 ) ; 
      if (!THD_is_directory(elist->ar[ii]) &&
          !strncmp(af, elist->ar[ii], N_af)  &&
          !strncmp(key, etr, N_key)
              )  {
         ADDTO_SARR( outar , elist->ar[ii] ) ; ++iaf;
         /* fprintf(stderr," %d- %s (%s)\n", iaf, elist->ar[ii], etr); */ 
      } else {
         /* fprintf(stderr," skip %s (%s)\n", elist->ar[ii], af); */ 
      }
   } 
   
   qsort(outar->ar, outar->num, sizeof(char*), 
      (int(*) (const void *, const void *))compare_string);
   
   if( SARR_NUM(outar) == 0 ) DESTROY_SARR(outar) ;
   if (af) free(af); af = NULL;
   RETURN( outar );
}

/*! get all 3D datasets in directory where afni resides */
THD_string_array * THD_get_all_afni_dsets(void )
{
   THD_string_array *outar=NULL, *elist=NULL;
   char *af=NULL, *etr=NULL;
   int N_af, N_afni=strlen("afni"), iaf=0, ii=0, smode, *isrt=NULL;
   char scomm[256]={""};
   
   ENTRY("THD_get_all_afni_dsets");
   
   if (!(elist = get_elist()) ||
       !(af = THD_abindir(1))) {
      ERROR_message("Could not find afni, we're doomed daddy!");
      RETURN(outar);
   }
   
   N_af = strlen(af);

   elist = THD_get_all_files(af,'\0');
   
   /* Now get all dsets under af */
   INIT_SARR( outar );
   for (ii=0, iaf=0; ii<elist->num ; ii++ ){
      smode = storage_mode_from_filename(elist->ar[ii]);
      etr = THD_trailname( elist->ar[ii] , 0 ) ; 
      if (
          !THD_is_directory(elist->ar[ii]) &&
          !strncmp(af, elist->ar[ii], N_af) &&
          (smode > STORAGE_UNDEFINED && smode <= LAST_STORAGE_MODE) &&
          (smode != STORAGE_BY_BRICK ||  /* don't want the .BRICK, just .HEAD */
               STRING_HAS_SUFFIX(elist->ar[ii], ".HEAD")) &&
          (smode != STORAGE_BY_NIFTI ||        /* don't want the .img */
               !STRING_HAS_SUFFIX(elist->ar[ii], ".img")) &&
          strcmp(etr,"AFNI_atlas_spaces.niml")
              )  {
         ADDTO_SARR( outar , elist->ar[ii] ) ; ++iaf;
         /*fprintf(stderr," %d- %s smode %d[%d]%d\n", iaf, etr,
                        STORAGE_UNDEFINED, smode, LAST_STORAGE_MODE); */
      } else {
         /*fprintf(stderr," skip %s (%s) smode %d[%d]%d\n", 
            elist->ar[ii], af, STORAGE_UNDEFINED, smode, LAST_STORAGE_MODE); */ 
      }
   } 
   
   qsort(outar->ar, outar->num, sizeof(char*), 
      (int(*) (const void *, const void *))compare_string);
   
   if( SARR_NUM(outar) == 0 ) DESTROY_SARR(outar) ;
   if (af) free(af); af = NULL;
   RETURN( outar );
}

int list_afni_files(int type, int withpath, int withnum)
{
   int nprogs=0, ii=0;
   char *etr=NULL, s[12];
   THD_string_array *progs=NULL;
   
   switch (type) {
      case 0:
         if (!(progs = THD_get_all_afni_executables())) {
            ERROR_message(
               "Cannot get list of programs from your afni bin directory %s", 
               THD_abindir(1));
            RETURN(0);
         }
         break;
      case 1:
         if (!(progs = THD_get_all_afni_readmes())) {
            ERROR_message(
               "Cannot get list of readmes from your afni bin directory %s", 
               THD_abindir(1));
            RETURN(0);
         }
         break;
      case 2:
         if (!(progs = THD_get_all_afni_dsets())) {
            ERROR_message(
               "Cannot get list of dsets from your afni bin directory %s", 
               THD_abindir(1));
            RETURN(0);
         }
         break;
      default:
         ERROR_message("Whatchyoutalkinboutwillis?");
         RETURN(0);
         break;
   }
   
   for (ii=0; ii<progs->num ; ii++ ){
      if (withpath) etr = progs->ar[ii];
      else etr = THD_trailname( progs->ar[ii] , 0 ) ;
      if (withnum) {
         sprintf(s,"%d", ii);
         fprintf(stdout,"  %3s.   %s\n", s, etr);
      } else {
         fprintf(stdout,"%s\n", etr);
      }
   }
   nprogs = progs->num;
   
   DESTROY_SARR(progs);
   
   return(nprogs);
}

int list_afni_programs(int withpath, int withnum) {
   return(list_afni_files(0, withpath, withnum));
}

int list_afni_readmes(int withpath, int withnum) {
   return(list_afni_files(1, withpath, withnum));
}

int list_afni_dsets(int withpath, int withnum) {
   return(list_afni_files(2, withpath, withnum));
}

