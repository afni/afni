#include "mrilib.h"
#include "thd.h"
#include "suma_suma.h"

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

/* 
   Find a file somewhere afniish
   Do not free returned pointer
   Empty string means nothing was found
   if altpath is not NULL, and nimlname does not
   have an absolute path, altpath is considere before diving into the
   default locations
*/
char *find_afni_file(char * nimlname, int niname, char *altpath)
{
   static char filestr[5][1024];
   static int icall = -1;
   static char *envlist[]={"AFNI_PLUGINPATH",
                           "AFNI_PLUGIN_PATH", 
                           "AFNI_TTAPATH", 
                           "AFNI_TTATLAS_DATASET", NULL };
   char namebuf[1024];
   char *fstr, *epath, *abpath=NULL;
   int kk = 0;
   
   ENTRY("find_afni_file");
   
   ++icall; if (icall > 4) icall = 0;
   filestr[icall][0]='\0';
   namebuf[0] = '\0';
   
   if(wami_verb() > 1) 
      INFO_message("trying to open %s \n",nimlname);   
   snprintf(namebuf, 1000*sizeof(char),
             "%s", nimlname);  
   if (THD_is_file(namebuf)) goto GOTIT;
   
   if(wami_verb() > 1) 
      INFO_message("%s not found, trying different paths, if no path is set.\n"
                     ,nimlname);   
   
   if (nimlname[0] == '/') { /* not found and have abs path, get out */
      RETURN(filestr[icall]);
   }
   
   if (altpath) {
      fstr = THD_find_regular_file(nimlname, altpath);
      snprintf(namebuf, 1000*sizeof(char), "%s", fstr);
      if (THD_is_file(namebuf)) goto GOTIT;
   }
   
   /* okay that didn't work, try the AFNI plugin directory */
   kk = 0;
   while (envlist[kk]) {
      namebuf[0]='\0';
                          epath = getenv(envlist[kk]) ;
      if( epath == NULL ) epath = getenv(envlist[kk]) ;
      if( epath != NULL ) {
         if(wami_verb() > 1) 
            INFO_message("trying to open %s in %s directory %s\n",
                 nimlname, envlist[kk], epath);   
         fstr = THD_find_regular_file(nimlname, epath);
         if(fstr) {
            if(wami_verb() > 1)
               INFO_message("found %s in %s", nimlname, fstr);
            snprintf(namebuf, 1000*sizeof(char), "%s", fstr);
            if (THD_is_file(namebuf)) goto GOTIT;
            if(wami_verb() > 1) 
               INFO_message("failed to open %s as %s\n",
                            nimlname, namebuf);  
         }
      }
      ++kk;
   }

   /* Look in AFNI data directory */
   namebuf[0]='\0';
   epath = THD_datadir(1);
   if( epath[0] == '\0' ) RETURN(filestr[icall]) ;  /* should not happen */
   if(wami_verb() > 1) 
      INFO_message("trying to open %s in path as regular file\n  %s\n",
                     nimlname, epath);   

   fstr = THD_find_regular_file(nimlname, epath);
   if(fstr) {
      if(wami_verb() > 1)
         INFO_message("found %s in %s", nimlname, fstr);
      snprintf(namebuf, 1000*sizeof(char), "%s", fstr);
      if (THD_is_file(namebuf)) goto GOTIT;
      if(wami_verb() > 1) 
         INFO_message("failed to open %s as %s\n",
                      nimlname, namebuf);  
   }
   
   /* still can't find it. Maybe it's in the afni path */ 
   namebuf[0]='\0';
   abpath = THD_abindir(1);
   if( abpath == NULL ) RETURN(filestr[icall]) ;  /* bad-who has no afni?*/
   if(wami_verb() > 1) 
      INFO_message("trying to open %s in path as regular file\n  %s\n",
                     nimlname, abpath);   

   fstr = THD_find_regular_file(nimlname, abpath);
   if(fstr) {
      if(wami_verb() > 1)
         INFO_message("found %s in %s", nimlname, fstr);
      snprintf(namebuf, 1000*sizeof(char), "%s", fstr);
      if (THD_is_file(namebuf)) goto GOTIT;
      if(wami_verb() > 1) 
         INFO_message("failed to open %s as %s\n",
                      nimlname, namebuf);  
   }
   
   if (abpath) free(abpath);
   RETURN(filestr[icall]);
   
   GOTIT:
   if (niname) {
      snprintf(filestr[icall], 1000*sizeof(char),
               "file:%s", namebuf);
   } else {
      snprintf(filestr[icall], 1000*sizeof(char),
               "%s", namebuf);
   }

   if (abpath) free(abpath);
   RETURN(filestr[icall]);
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
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".o") &&
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

/* Include file that has C struct containing programs and all their options */
#include "prog_opts.c"

char *form_C_progopt_string(char *prog, char **ws, int N_ws) 
{
   char *sout=NULL, sbuf[128];
   int maxch=0, i, jj, N_opts=0;
   NI_str_array *nisa=NULL;
   
   if (!prog || !ws) {
      return(NULL);
   }
   
   maxch = 256;
   for (i=0; i<N_ws; ++i) {
      if (ws[i]) {
         maxch+=strlen(ws[i])+10;
         if (strlen(ws[i]) > 127) {
            WARNING_message("Truncating atrocious option %s\n", ws[i]);
            ws[127] = '\0';
         }
      }
   }
   if (!(sout = (char *)calloc((maxch+1), sizeof(char)))) {
      ERROR_message("Failed to allocate for %d chars!", maxch+1);
      return(NULL);
   }
   sout[0]='\0';
   strncat(sout,"{ \"", maxch-1);
   strncat(sout,prog, maxch-1);
   strncat(sout,"\", \"", maxch-1);
   
   N_opts = 0;
   for (i=0; i<N_ws; ++i) {
      if (ws[i] && (nisa = NI_strict_decode_string_list(ws[i] ,"/"))) {
         for (jj=0; jj<nisa->num; ++jj) {
            if (ws[i][0]=='-' && nisa->str[jj][0] != '-') {
               snprintf(sbuf,127,"-%s; ", nisa->str[jj]);
            } else { 
               snprintf(sbuf,127,"%s; ", nisa->str[jj]);
            }
            ++N_opts;
            strncat(sout,sbuf, maxch-1);
            NI_free(nisa->str[jj]);
         }
         if (nisa->str) NI_free(nisa->str); 
         NI_free(nisa); nisa=NULL;
      }
   }
   sprintf(sbuf,"\", %d", N_opts); strncat(sout,sbuf, maxch-1);
   
   
   strncat(sout,"}", maxch-1);
   if (strlen(sout)>=maxch) {
      ERROR_message("Truncated complete string possible");
      free(sout); sout=NULL;
      return(sout);
   }

   return(sout);
}

/*
   Generate C array that lists all afni programs and their options
   
   There is a most unholy relationship between this function and 
   the include line: #include "prog_opts.c"
   
   This function is for internal machinations having to do with
   automatic generation of help web pages. It is not for mass
   consumption.
*/
int progopt_C_array(FILE *fout, int verb) 
{
   char **ws=NULL, *sout=NULL;
   float *ws_score=NULL;
   int N_ws=0, ii = 0, jj = 0;
   THD_string_array *progs=NULL;
   
   ENTRY("progopt_C_array");
   
   if (!fout) fout = stdout;
   
   
   if (!(progs = THD_get_all_afni_executables()) || progs->num < 1) {
      ERROR_message("Could not get list of executables");
      RETURN(1);
   }

   fprintf(fout, 
      "#ifndef PROG_OPTS_INCLUDED\n"
      "#define PROG_OPTS_INCLUDED\n"
      "\n"
      "/* \n"
      "   ***********           DO NOT EDIT!                ***********\n"
      "   *************** File created automatically ******************\n"
      "\n"
      "   This file is created by function progopt_C_array(), probably \n"
      "   via program apsearch with:\n"
      "        apsearch -C_prog_opt_array > prog_opts.c\n\n"
      "\n"
      "If you want to freshen the one in the distribution, rerun the \n"
      "same command. You'll need to also touch thd_getpathprogs.c\n"
      "before rebuilding libmri.a, etc.\n"
      "*/\n\n"
      "typedef struct {\n"
      "  char *program;\n"
      "  char *options;\n"
      "  int N_options;\n"
      "} PROG_OPTS;\n\n"
      "#if 0\n"
      "static PROG_OPTS poptslist[] = {\n"
      "   {NULL, NULL, 0}\n"
      "}\n"
      "#else\n"
      "static PROG_OPTS poptslist[] = {\n");
      
   for (ii=0; ii<progs->num; ++ii) {
      if (verb) fprintf(stderr,"Prog %d/%d: %s ", ii+1, progs->num,
                     THD_trailname(progs->ar[ii],0) );
      if ((ws = approx_str_sort_all_popts(progs->ar[ii], &N_ws,  
                   1, &ws_score,
                   NULL, NULL, 1, 0, '\\'))) {
         if (verb) fprintf(stderr,"%d opts\t ", N_ws);
         if ((sout = form_C_progopt_string(
                        THD_trailname(progs->ar[ii], 0), ws, N_ws))){
            fprintf(fout, "%s,\n", sout);
            free(sout); sout = NULL;
         }
         for (jj=0; jj<N_ws; ++jj) if (ws[jj]) free(ws[jj]);
         free(ws); ws = NULL;
         if (ws_score) free(ws_score); ws_score=NULL;
      }
   }
   fprintf(fout, "   {  NULL, NULL, 0  }\n};\n\n"
                 "#endif\n\n\n"
                 "#endif /* For #ifdef PROG_OPTS_INCLUDED */\n");
   
   DESTROY_SARR(progs) ;
   
   RETURN(0);
}

char *phelp(char *prog, TFORM targ, int verb) 
{
   char cmd[512], uid[64], tout[128];
   char *help=NULL;
   char *hopt;
   
   ENTRY("phelp");
   
   if (!prog ) RETURN(help);
   
   switch(targ){
      case NO_FORMAT:
         hopt = "-h_raw";
         break;
      case SPX:
         hopt = "-h_spx";
         break;
      case TXT:
         hopt = "-help";
         break;
      default:
         ERROR_message("I hate myself for failing you with %d", targ);
         RETURN(help);
   }
   
   UNIQ_idcode_fill(uid);
   sprintf(tout,"/tmp/%s.%s.txt", APSEARCH_TMP_PREF, uid); 
   snprintf(cmd,500*sizeof(char),"\\echo '' 2>&1 | %s %s > %s 2>&1 ",
            prog, hopt, tout);
   if (system(cmd)) {
      if (0) {/* many programs finish help and set status afterwards. Naughty. */
         ERROR_message("Failed to get help for %s\nCommand: %s\n", prog, cmd);
         return 0;
      }
   }
   
   if (!(help = AFNI_suck_file(tout))) {
      if (verb) ERROR_message("File %s could not be read\n", tout);
      RETURN(help);
   }
                                 
   snprintf(cmd,500*sizeof(char),"\\rm -f %s", tout);
   system(cmd);
   
   RETURN(help);
}

/* Check the static list in prog_opts.c to find whether 
or not an option exists for a particular program.
Return  1: If program is found and the option exists
        0: If program found and option does not exist
       -1: If program was not in the list
       -2: If the caller needs brains.
*/
int check_for_opt_in_prog_opts(char *prog, char *opt)
{
   PROG_OPTS PO;
   int i=0;
   char sbuf[64]={""}, *found;
   
   if (!prog || !opt) return(-2);
   PO = poptslist[i++];
   while (PO.program) {
      if (!strcmp(THD_trailname(prog, 0),PO.program)) {
         snprintf(sbuf, 64, "%s;", opt);
         /* fprintf(stderr,"%s, %s-->%s, %s\n", 
            prog, sbuf, PO.program, PO.options); */
         if ((found=strstr(PO.options,sbuf))) {
            return(1);
         } else {
            return(0);
         }
      }
      PO = poptslist[i++];
   }
   /* program not found */
   return(-1);
}

/* 
   Return 1 if program uprog has option option opt
          0 otherwsise
   
   The function first checks if the program has an
   entry in array poptslist from file prog_opts.c included above.
   
   If an entry is found, the decision is based on whether or not
   opt is listed for that program. Otherwise, if no entry is found,
   the function resorts to running the program with option opt
   followed by value oval (if not NULL). If the program returns a status 
   of 1, OR creates no output in response to the option then the 
   option is considered non-existent. 
   
   Obviously, this is not a general purpose option checker.
   It was written for the purpose of checking whether or not 
   a program supports the newfangled -h_raw, etc. options.
   
   If uprog is "ALL", the function check all existing programs
   for opt and returns the total number of programs that seem
   to support it.
*/ 
int program_supports(char *uprog, char *opt, char *oval, int verb) 
{
   char cmd[512], uid[64], tout[128], *prog=NULL;
   int sup=0, ii=0, quick=0;
   THD_string_array *progs=NULL;
   
   ENTRY("program_supports");
   
   if (!uprog || !opt) RETURN(sup);
   
   if (!strcmp(uprog,"ALL")) {
      if (!(progs = THD_get_all_afni_executables()) || progs->num < 1) {
         ERROR_message("Could not get list of executables");
         RETURN(sup);
      }
      prog = progs->ar[ii++];
   } else {
      prog = uprog;
   }
   
   if (!oval) oval = "";
   sup = 0;
   do {
      switch (quick = check_for_opt_in_prog_opts(prog, opt)) {
         case 1:
            sup += 1;
            if (verb) {
               fprintf(stderr,"%s -- OK for %s %s (quick)\n", 
                                 prog, opt, oval);
            }
            break;
         case 0:
            sup += 0;
            if (verb) {
               fprintf(stderr,"%s -- No support for %s %s (quick)\n", 
                                 prog, opt, oval);
            }
            break;
         case -1:
            UNIQ_idcode_fill(uid);
            sprintf(tout,"/tmp/%s.%s.ps.txt", APSEARCH_TMP_PREF, uid); 
            snprintf(cmd,500*sizeof(char),"\\echo '' 2>&1 | %s %s %s > %s 2>&1 ",
                     prog, opt, oval, tout);
            if (system(cmd) || !THD_filesize(tout)) {
               sup += 0;
               if (verb) {
                  fprintf(stderr,"%s -- No support for %s %s\n", 
                                 prog, opt, oval);
               }
            } else {
               sup += 1;
               if (verb) {
                  fprintf(stderr,"%s -- OK for %s %s\n", prog, opt, oval);
               }
            }
            snprintf(cmd,500*sizeof(char),"\\rm -f %s", tout);
            system(cmd);
            break;
          case -2:
            ERROR_message("Nonesense here?");
            break;
      }
   
      if (progs && ii < progs->num) {
         prog = progs->ar[ii++];   
      }else prog = NULL;
   } while (prog);
   
   if (progs) {
      DESTROY_SARR(progs) ;
   }
   
   RETURN(sup);
}

char *find_popt(char *sh, char *opt, int *nb)
{
   char *loc=NULL, *other=NULL;
   
   ENTRY("find_popt");
   
   if (!sh || !opt) {
      ERROR_message("NULL option or null string");
      RETURN(loc);
   } 
   
   loc = line_begins_with(sh, opt, nb);
   
   if (loc) { /* Check that we do not have more than one */
      if ((other = line_begins_with(loc+*nb+1, opt, NULL))) { 
         WARNING_message("More than one match for opt %s.\n"
                         "Returning first hit\n", opt);
      }
   }
   
   RETURN(loc);
}

char *form_complete_command_string(char *prog, char **ws, int N_ws, int shtp) {
   char *sout=NULL, sbuf[128];
   int maxch=0, i, jj;
   NI_str_array *nisa=NULL;
   
   if (!prog || !ws || shtp < 0) {
      return(NULL);
   }
   
   maxch = 256;
   for (i=0; i<N_ws; ++i) {
      if (ws[i]) {
         maxch+=strlen(ws[i])+10;
         if (strlen(ws[i]) > 127) {
            WARNING_message("Truncating atrocious option %s\n", ws[i]);
            ws[127] = '\0';
         }
      }
   }
   if (!(sout = (char *)calloc((maxch+1), sizeof(char)))) {
      ERROR_message("Failed to allocate for %d chars!", maxch+1);
      return(NULL);
   }
   sout[0]='\0';
   switch (shtp) {
      default:
      case 0: /* csh/tcsh */
         strncat(sout,"set ARGS=(",maxch-1);
         break;
      case 1: /* bash */
         strncat(sout,"ARGS=(",maxch-1);
         break;
   }
   
   for (i=0; i<N_ws; ++i) {
      if (ws[i] && (nisa = NI_strict_decode_string_list(ws[i] ,"/"))) {
         for (jj=0; jj<nisa->num; ++jj) {
            if (ws[i][0]=='-' && nisa->str[jj][0] != '-') {
               snprintf(sbuf,127,"'-%s' ", nisa->str[jj]);
            } else { 
               snprintf(sbuf,127,"'%s' ", nisa->str[jj]);
            }
            strncat(sout,sbuf, maxch-1);
            NI_free(nisa->str[jj]);
         }
         if (nisa->str) NI_free(nisa->str); 
         NI_free(nisa); nisa=NULL;
      }
   }
   
   switch (shtp) {
      default:
      case 0: /* csh/tcsh */
         snprintf(sbuf,127,") ; "
               "complete %s \"C/-/($ARGS)/\" \"p/*/f:/\" ; ##%s##\n",prog, prog);
         break;
      case 1: /* bash */
         snprintf(sbuf,127,") ; "
               "complete -W \"${ARGS[*]}\" -o bashdefault -o default %s ; "
               "##%s##\n",prog, prog);
         break;
   }
   if (strlen(sbuf) >= 127) {
      ERROR_message("Too short a buffer for complete command %s\n");
      free(sout); sout=NULL;
      return(sout);
   }
   strncat(sout,sbuf, maxch-1);
   if (strlen(sout)>=maxch) {
      ERROR_message("Truncated complete string possible");
      free(sout); sout=NULL;
      return(sout);
   }

   return(sout);
}

int prog_complete_command (char *prog, char *ofileu, int shtp) {
   char **ws=NULL, *sout=NULL, *ofile=NULL;
   float *ws_score=NULL;
   int N_ws=0, ishtp=0, shtpmax = 0, i;
   FILE *fout=NULL;
   
   if (!prog || !(ws = approx_str_sort_all_popts(prog, &N_ws,  
                   1, &ws_score,
                   NULL, NULL, 1, 0, '\\'))) {
      return 0;
   }

   if (shtp < 0) { shtp=0; shtpmax = 2;}
   else { shtpmax = shtp+1; }
   
   for (ishtp=shtp; ishtp<shtpmax; ++ishtp) {
      if (ofileu) {
          if (shtpmax != shtp+1) { /* autoname */
            switch (ishtp) {
               default:
               case 0:
                  ofile = strdup(ofileu);
                  break;
               case 1:
                  ofile = (char*)calloc((strlen(ofileu)+20), sizeof(char));
                  strcat(ofile, ofileu);
                  strcat(ofile, ".bash");
                  break;
            }
          } else {
            ofile = strdup(ofileu);
          }
            
          if (!(fout = fopen(ofile,"w"))) {
            ERROR_message("Failed to open %s for writing\n", ofile);
            return(0);
          }

      } else {
         fout = stdout;
      }

      if ((sout = form_complete_command_string(prog, ws, N_ws, ishtp))){
         fprintf(fout, "%s", sout);
         free(sout); sout = NULL;
      }
      if (ofileu) fclose(fout); fout=NULL;
      if (ofile) free(ofile); ofile=NULL;
   }
   
   for (i=0; i<N_ws; ++i) if (ws[i]) free(ws[i]);
   free(ws); ws = NULL;
   if (ws_score) free(ws_score); ws_score=NULL;
   return 0;
}


