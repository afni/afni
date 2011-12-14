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
/*! Find a regular file in the PATH by its name, if it exists.
    Does not include directories.
    If not, NULL is returned.
    If it exists, a pointer to malloc-ed storage is returned
    (e.g., free it when you are done).
------------------------------------------------------------------------------*/

char * THD_find_regular_file( char *ename )
{
   char *fullname , *str ;
   int id , ii ;
   char *epath;
ENTRY("THD_find_regular_file") ;
   epath = my_getenv( "PATH" ) ;
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

         ii = strlen(dirname) ;                         /* make sure name has */
         if( dirname[ii-1] != '/' ){                    /* a trailing '/' on it */
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
   int N_af, N_afni=strlen("afni"), iaf=0, ii=0, smode, *isrt=NULL;
   char scomm[256]={""};
   
   ENTRY("THD_get_all_afni_executables");
   
   if (!(elist = get_elist()) ||
       !(af = THD_find_executable("afni"))) {
      ERROR_message("Could not find afni, we're doomed daddy!");
      RETURN(outar);
   }
   
   /* remove afni from the end to get the path */
   N_af = strlen(af);
   if (strcmp(af+N_af-N_afni,"afni")) {
      ERROR_message("This should not be (%s)!", af+N_af-N_afni);
      RETURN(outar);
   }
   af[strlen(af)-N_afni]='\0'; N_af = strlen(af);
   if (af[N_af-1] != '/') {
      af[N_af] = '/'; af[N_af+1] = '\0';
      ++N_af;
   }
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
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".xbm") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".tex") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".lib") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".so") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".la") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".txt") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".R") &&
    !(STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".py") && !strncmp(etr,"lib_",4)) &&
          (smode <= STORAGE_UNDEFINED || smode >= LAST_STORAGE_MODE)  &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".sumarc") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".afnirc")
              )  {
         ADDTO_SARR( outar , elist->ar[ii] ) ; ++iaf;
         /* fprintf(stderr," %d- %s\n", iaf, etr); */
      } else {
         /* fprintf(stderr," skip %s (%s)\n", elist->ar[ii], af); */
      }
   } 
   
   qsort(outar->ar, outar->num, sizeof(char*), 
      (int(*) (const void *, const void *))compare_string);
   
   if( SARR_NUM(outar) == 0 ) DESTROY_SARR(outar) ;

   RETURN( outar );
}

int list_afni_programs(int withnum)
{
   int nprogs=0, ii=0;
   char *etr=NULL, s[12];
   THD_string_array *progs=NULL;
   
   if (!(progs = THD_get_all_afni_executables())) {
      ERROR_message("Cannot get list of programs");
      RETURN(0);
   }

   for (ii=0; ii<progs->num ; ii++ ){
      etr = THD_trailname( progs->ar[ii] , 0 ) ;
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
