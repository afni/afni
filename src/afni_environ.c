/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

extern int SUMA_IcoNums(int depth, byte bin, char what);

/*------------------------------------------------------------------------
   Read an entire file into a character string.  When you are
   done with the returned string, free() it.  If the string pointer
   is returned as NULL, something bad happened, and you are doomed.
--------------------------------------------------------------------------*/

static int nsuck = 0 ;
int AFNI_suck_file_len(void){ return nsuck; }  /* 27 Feb 2009 */

char * AFNI_suck_file( char *fname )
{
   int len , fd , ii ;
   char *buf ;

ENTRY("AFNI_suck_file") ;
   nsuck = 0 ;
   if( fname == NULL || fname[0] == '\0' ) RETURN(NULL);

   len = THD_filesize( fname ) ;
   if( len <= 0 ) RETURN(NULL) ;

   fd = open( fname , O_RDONLY ) ;
   if( fd < 0 ) RETURN(NULL) ;

   buf = (char *) malloc( sizeof(char) * (len+4) ) ;
   ii  = read( fd , buf , len ) ;
   close( fd ) ;
   if( ii <= 0 ){ free(buf); RETURN(NULL); }

   buf[len] = '\0' ;  /* 27 July 1998: 'len' used to be 'ii+1', which is bad */
   nsuck = len ; RETURN(buf) ;
}

/*-----------------------------------------------------------------------
   Read environment section only from an AFNI setup file.
   [See also afni_setup.c]
-------------------------------------------------------------------------*/

#define ISTARRED(s) ( (s)[0]=='*' && (s)[1]=='*' && (s)[2]=='*' )

#define EOLSKIP                                                          \
  do{ for( ; fptr[0] != '\n' && fptr[0] != '\0' ; fptr++ ) ; /* nada */  \
      if( fptr[0] == '\0' ) goto Done ;                                  \
      fptr++ ; } while(0)

#define GETSSS                                                            \
  do{ int nu=0,qq;                                                        \
      if( fptr-fbuf >= nbuf || fptr[0] == '\0' ) goto Done ;              \
      str[0]='\0'; qq=sscanf(fptr,"%127s%n",str,&nu); nused+=nu;fptr+=nu; \
      if( str[0]=='\0' || qq==0 || nu==0 ) goto Done ;                    \
    } while(0)

#define GETSTR                                                            \
  do{ GETSSS ;                                                            \
      while(str[0]=='!' || (str[0]=='/' && str[1]=='/') ||                \
            (str[0]=='#' && str[1]=='\0') ){EOLSKIP; GETSSS;}             \
    } while(0)

#define GETEQN                                      \
  do{ GETSTR ; if(ISTARRED(str)) goto SkipSection ; \
      strcpy(left,str) ;                            \
      GETSTR ; if(ISTARRED(str)) goto SkipSection ; \
      strcpy(middle,str) ;                          \
      GETSTR ; if(ISTARRED(str)) goto SkipSection ; \
      strcpy(right,str) ; } while(0)

#undef  NSBUF
#define NSBUF 256

static int afni_env_done = 0 ;

/*---------------------------------------------------------------------------*/

void AFNI_mark_environ_done(void)   { afni_env_done = 1 ; return ; }
void AFNI_mark_environ_undone(void) { afni_env_done = 0 ; return ; }
int  AFNI_check_environ_done(void)  { return afni_env_done ; }

/*---------------------------------------------------------------------------*/

static int bloced=0 ;

char * my_getenv( char *ename )
{
   if( !bloced && !afni_env_done ){
     char *sysenv = getenv("AFNI_SYSTEM_AFNIRC") ;       /* 16 Apr 2000 */
     if( sysenv != NULL ) AFNI_process_environ(sysenv) ; /* 16 Apr 2000 */
     AFNI_process_environ(NULL) ;
   }
   return getenv( ename ) ;
}

/*---------------------------------------------------------------------------*/

int AFNI_process_environ( char *fname )
{
   int   nbuf , nused , ii ;
   char *fbuf , *fptr ;
   char  str[NSBUF] , left[NSBUF] , middle[NSBUF] ,
         right[NSBUF], fname_str[NSBUF] = {"not_set"};
   int nenv=0 , senv=0 ; static int first=1 ;  /* 13 Mar 2008 */

ENTRY("AFNI_process_environ") ;
   bloced = 1 ;

   if( fname != NULL ){
     strcpy(str,fname) ;
   } else {
     char *home ;
     if( afni_env_done ) RETURN(nenv) ;
     home = getenv("HOME") ;
     if( home != NULL ){ strcpy(str,home) ; strcat(str,"/.afnirc") ; }
     else              { strcpy(str,".afnirc") ; }
     if( !THD_is_file(str) ){                      /* 19 Sep 2007 */
       if( home != NULL ){ strcpy(str,home) ; strcat(str,"/AFNI.afnirc") ; }
       else              { strcpy(str,"AFNI.afnirc") ; }
     }
     afni_env_done = 1 ;
   }
   strcpy(fname_str,str) ; /* ZSS: Nov. 25 08 */

   fbuf = AFNI_suck_file( str ) ;
   if( fbuf == NULL ){ bloced=0; if(fname==NULL)afni_env_done=0; RETURN(nenv); }
   nbuf = strlen(fbuf) ;
   if( nbuf == 0    ){ bloced=0; if(fname==NULL)afni_env_done=0; RETURN(nenv); }

   fptr = fbuf ; nused = 0 ;

   /** scan for section strings, which start with "***" **/

   str[0] = '\0' ;  /* initialize string */

   while( nused < nbuf ){

      /**----------------------------------------**/
      /**-- skip ahead to next section keyword --**/

      SkipSection: while( ! ISTARRED(str) ){ GETSTR; }

      /*- 04 Jun 1999 -*/

      if( strcmp(str,"***END") == 0 ) break ;  /* exit main loop */

      if( strcmp(str,"***ENVIRONMENT") != 0 ){ GETSTR ; goto SkipSection ; }

      /**---------------------------------------**/
      /**-- ENVIRONMENT section [04 Jun 1999] --**/

      if( strcmp(str,"***ENVIRONMENT") == 0 ){ /* loop: find environment eqns */
         char *enveqn , *eee; int nl , nr , allow_reset ;
         senv = 1 ;

         eee = getenv("AFNI_ENVIRON_RESET") ; allow_reset = YESSISH(eee) ;

         while(1){                        /* loop, looking for 'name = value' */
            GETEQN ;

            if( !THD_filename_pure(left) ) continue ;

            nl = strlen(left) ; nr = strlen(right) ;
            enveqn = (char *) malloc(nl+nr+4) ;
            strcpy(enveqn,left) ; strcat(enveqn,"=") ; strcat(enveqn,right) ;
            if( !(eee = getenv(left)) || allow_reset ){  /* ZSS Nov 25 2008 */
               putenv(enveqn) ;
            } else if( !AFNI_noenv("AFNI_ENVIRON_WARNINGS") &&
                        strcmp(right, eee)){
               INFO_message(  "Environment variable %s already set to '%s'. "
                              "Value of '%s' from %s is ignored. \n"
                         "To kill such warnings Set AFNI_ENVIRON_WARNINGS to NO",
                              left, eee, right, fname_str);
            }
            nenv++ ;
         }

         continue ;  /* to end of outer while */
      } /* end of ENVIRONMENT */

   }  /* end of while loop */

  Done:
   if( fname == NULL && first ){
     if( senv == 0 )
       WARNING_message("didn't find '***ENVIRONMENT' line in ~/.afnirc") ;
     else if( nenv == 0 )
       WARNING_message("didn't find any environment equations in ~/.afnirc") ;
   }

   first = 0 ; free(fbuf) ; bloced = 0 ; RETURN(nenv) ;
}

/*-----------------------------------------------------------------*/

int AFNI_yesenv( char *ename )     /* 21 Jun 2000 */
{
   char *ept ;
   if( ename == NULL ) return 0 ;
   ept = my_getenv(ename) ;
   return YESSISH(ept) ;
}

/*------------------------------------------------------------------*/

int AFNI_noenv( char *ename )     /* 21 Jun 2000 */
{
   char *ept ;
   if( ename == NULL ) return 0 ;
   ept = my_getenv(ename) ;
   return NOISH(ept) ;
}

/*------------------------------------------------------------------*/

double AFNI_numenv( char *ename )  /* 23 Aug 2003 */
{
   char *ept,*ccc ; double val ;
   if( ename == NULL ) return 0.0 ;
   ept = my_getenv(ename) ;
   if( ept   == NULL ) return 0.0 ;
   val = strtod(ept,&ccc) ;
        if( *ccc == 'k' || *ccc == 'K' ) val *= 1024.0 ;
   else if( *ccc == 'm' || *ccc == 'M' ) val *= 1024.0*1024.0 ;
   else if( *ccc == 'g' || *ccc == 'G' ) val *= 1024.0*1024.0*1024.0 ;
   return val ;
}

/*------------------------------------------------------------------*/

double AFNI_numenv_def( char *ename, double dd ) /* 18 Sep 2007 */
{
   char *ept,*ccc ; double val=dd ;
   if( ename == NULL ) return val ;
   ept = my_getenv(ename) ;
   if( ept   == NULL ) return val ;
   val = strtod(ept,&ccc) ; if( ccc == ept ) val = dd ;
   return val ;
}

/*------------------------------------------------------------------*/
/*! Input is "name value".  Return is 0 if OK, -1 if not OK. */

int AFNI_setenv( char *cmd )
{
   char nam[256]="\0" , val[1024]="\0" , eqn[1280] , *eee ;

   if( cmd == NULL || strlen(cmd) < 3 ) return(-1) ;

   sscanf( cmd , "%255s %1023s" , nam , val ) ;
   if( (nam[0] == '\0' || val[0] == '\0') && strchr(cmd,'=') != NULL ){
     char *ccc = strdup(cmd) ;
     eee = strchr(ccc,'=') ; *eee = ' ' ;
     sscanf( ccc , "%255s %1023s" , nam , val ) ;
     free((void *)ccc) ;
   }
   if( nam[0] == '\0' || val[0] == '\0' ) return(-1) ;

   sprintf(eqn,"%s=%s",nam,val) ;
   eee = strdup(eqn) ; putenv(eee) ;  /* note that eee is never free()-ed */

#ifdef USE_TRACING  /* else DBG_trace is #defined to 0   2 Jan 2008 [rickr] */
   if( strcmp(nam,"AFNI_DEBUG") == 0 ){  /* 29 Dec 2008 */
     switch( val[0] ){
       default:  DBG_trace = 0 ; break ;
       case 'y': DBG_trace = 1 ; break ;
       case 'Y': DBG_trace = 2 ; break ;
     }
   }
#endif

   return(0) ;
}

/*-------------------------------------------------------------------------*/
char *get_gopt_help() {
   static char GOPT_HELP[] = {
"   -overwrite: Overwrite existing output dataset.\n"
"               Equivalent to setting env. AFNI_DECONFLICT=OVERWRITE\n"
"   -ok_1D_text: Zero out uncommented text in 1D file.\n"
"                Equivalent to setting env. AFNI_1D_ZERO_TEXT=YES\n"
"   -Dname=val: Set environment variable 'name' to value 'val'\n"
"             For example: -DAFNI_1D_ZERO_TEXT=YES\n"
"   -Vname=: Print value of environment variable 'name' to stdout and quit.\n"
"            This is more reliable that the shell's env query because it would\n"
"            include envs set in .afnirc files and .sumarc files for SUMA\n"
"            programs.\n"
"             For example: -VAFNI_1D_ZERO_TEXT=\n"
"   -skip_afnirc: Do not read the afni resource (like ~/.afnirc) file.\n"
"   -pad_to_node NODE: Output a full dset from node 0 to MAX_NODE-1\n"
"                   ** Instead of directly setting NODE to an integer you \n"
"                      can set NODE to something like:\n"
"                   ld120 (or rd17) which sets NODE to be the maximum \n"
"                      node index on an Icosahedron with -ld 120. See \n"
"                      CreateIcosahedron for details.\n"
"                   d:DSET.niml.dset which sets NODE to the maximum node found\n"
"                      in dataset DSET.niml.dset.\n" 
"                   ** This option is for surface-based datasets only.\n"
"                      Some programs may not heed it, so check the output if\n"
"                      you are not sure.\n"
"   -pif SOMETHING: Does absolutely nothing but provide for a convenient\n"
"                   way to tag a process and find it in the output of ps -a\n"
"   -echo_edu: Echos the entire command line to stdout (without -echo_edu)\n"
"              for edification purposes\n"
"\n" 
/* Do not add options for NIML ports here, get them with get_np_help() instead */
   };
   return(GOPT_HELP);
}

char *get_help_help() {
   static char HELP_HELP[] = {
"-h: Mini help, at time, same as -help in many cases.\n"
"-help: The entire help output\n"
"-HELP: Extreme help, same as -help in majority of cases.\n"
"-h_view: Open help in text editor. AFNI will try to find a GUI editor\n"
"-hview : on your machine. You can control which it should use by\n"
"         setting environment variable AFNI_GUI_EDITOR.\n"
"-h_web: Open help in web browser. AFNI will try to find a browser.\n"
"-hweb : on your machine. You can control which it should use by\n"
"        setting environment variable AFNI_GUI_EDITOR. \n"
"-h_find WORD: Look for lines in this programs's -help output that match\n"
"              (approximately) WORD.\n"
"-h_raw: Help string unedited\n"\
"-h_spx: Help string in sphinx loveliness, but do not try to autoformat\n"\
"-h_aspx: Help string in sphinx with autoformatting of options, etc.\n"\
"-all_opts: Try to identify all options for the program from the\n"
"           output of its -help option. Some options might be missed\n"
"           and others misidentified. Use this output for hints only.\n"
   };
   return(HELP_HELP);
}

int MRILIB_DomainMaxNodeIndex = -1;

int AFNI_prefilter_args( int *argc , char **argv )
{
   int narg=*argc , ii,jj , nused , ttt ;
   char *used , *eee ;

   if( narg <= 1 || argv == NULL ) return(0) ;

   used = (char *)calloc((size_t)narg,sizeof(char)) ;

   eee = getenv("AFNI_TRACE") ; ttt = YESSISH(eee) ;
   if( ttt )
     fprintf(  stderr,
               "++ AFNI_prefilter_args() processing argv[1..%d]\n",narg-1) ;

   /*--- scan thru argv[];
         see if any should be processed now and marked as 'used up' ---*/

   for( ii=1 ; ii < narg ; ii++ ){

     /*** empty argument (should never happen in Unix) ***/

     if( argv[ii] == NULL ){
       if( ttt ) fprintf(stderr,"++ argv[%d] is NULL\n",ii) ;
       used[ii] = 1 ; continue ;
     }

     /*** -Dname=val to set environment variable ***/

     if( strncmp(argv[ii],"-D",2) == 0 && strchr(argv[ii],'=') != NULL ){
       if( ttt ) fprintf(stderr,"++ argv[%d] does setenv %s\n",ii,argv[ii]) ;
       (void)AFNI_setenv(argv[ii]+2) ; used[ii] = 1 ; continue ;
     }

     /*** -Vname to get environment variable ***/

     if( strncmp(argv[ii],"-V",2) == 0 && strchr(argv[ii],'=') != NULL ){
       if( ttt ) fprintf(stderr,"++ argv[%d] does getenv %s\n",ii,argv[ii]) ;
       fprintf(stdout,"%s\n",
               (eee = my_getenv(argv[ii]+2)) ? eee:"") ; 
        used[ii] = 1 ; exit(0) ;
     }

     /*** -overwrite to set AFNI_DECONFLICT ***/

     if( strcmp(argv[ii],"-overwrite") == 0 ){
       if( ttt ) fprintf(stderr,"++ argv[%d] is -overwrite\n",ii) ;
       AFNI_setenv("AFNI_DECONFLICT=OVERWRITE") ; 
       THD_set_quiet_overwrite(1); /* no need to kvetch */
       used[ii] = 1 ; continue ;
     }

     /*** echo command ***/
     
     if( strcmp(argv[ii],"-echo_edu") == 0 ){
       if( ttt ) fprintf(stderr,"++ argv[%d] is -echo_edu\n",ii) ;
       {
         int jjj=0; 
         fprintf(stdout,"\n+++ Command Echo:\n   "); 
         for (jjj=0; jjj<narg; ++jjj)  { 
            if (jjj != ii) {   
               fprintf(stdout,"%s ", argv[jjj]);  
            }     
         }
         fprintf(stdout,"\n\n");
         used[ii] = 1 ; continue ;
       }
     }
     
     if( strcmp(argv[ii],"-all_opts") == 0 ){
       if( ttt ) fprintf(stderr,"++ argv[%d] is -all_opts\n",ii) ;
       print_prog_options(argv[0]); used[ii] = 1 ; 
       exit(0); 
         /* better exit, otherwise output get burried by program's own -help */ 
     }

     if( strcmp(argv[ii],"-h_find") == 0 ){
       if( ttt ) fprintf(stderr,"++ argv[%d] is -h_find\n",ii) ;
       if (ii+1 >= narg) {
         fprintf(stderr,"** -h_find needs a string.\n");
         exit(1);
       }
       used[ii] = 1 ; ii++;
       suggest_best_prog_option(argv[0], argv[ii]);
       used[ii] = 1 ; 
       exit(0); 
         /* better exit, otherwise output get burried by program's own -help */ 
     }
     
     if( strcmp(argv[ii],"-h_aspx") == 0 && ii == 1){
       char *s=NULL;
       if( ttt ) fprintf(stderr,"++ argv[%d] is -h_apsx\n",ii) ;
       if (!(s = sphinxize_prog_help(argv[0], 0))) {
         fprintf(stderr,"** Failed to get auto-sphinxized string\n");
         exit(1);
       }
       fprintf(stdout,"%s", s); free(s);
       used[ii] = 1 ; 
       exit(0); 
     }
     
     
     if( strcmp(argv[ii],"-h_view") == 0 || strcmp(argv[ii],"-hview") == 0 ){
       if( ttt ) fprintf(stderr,"++ argv[%d] is -h_view or -hview \n",ii) ;
       view_prog_help(argv[0]);
       used[ii] = 1 ; 
       exit(0); 
         /* better exit, otherwise output get burried by program's own -help */ 
     }
     
     if( strcmp(argv[ii],"-h_web") == 0  || strcmp(argv[ii],"-hweb") == 0 ){
       if( ttt ) fprintf(stderr,"++ argv[%d] is -h_web or -hweb\n",ii) ;
       web_prog_help(argv[0],0);
       used[ii] = 1 ; 
       exit(0); 
         /* better exit, otherwise output get burried by program's own -help */ 
     }
     
     /*** -ok_1D_text to set AFNI_1D_ZERO_TEXT ZSS Dec 09 ***/

     if( strcmp(argv[ii],"-ok_1D_text") == 0 ){
       if( ttt ) fprintf(stderr,"++ argv[%d] is -ok_1D_text\n",ii) ;
       AFNI_setenv("AFNI_1D_ZERO_TEXT=YES") ; used[ii] = 1 ; continue ;
     }

     /*** -skip_afnirc to avoid .afnirc file ***/

     if( strcmp(argv[ii],"-skip_afnirc") == 0 ){
       if( ttt ) fprintf(stderr,"++ argv[%d] is -skip_afnirc\n",ii) ;
       AFNI_mark_environ_done() ; used[ii] = 1 ; continue ;
     }

     /*** -pad_to_node to force sparse data to a particular size ***/

     if( strcmp(argv[ii],"-pad_to_node") == 0 ){
       if( ttt ) fprintf(stderr,"++ argv[%d] is -pad_to_node\n",ii) ;
       if (ii+1 >= narg) {
         fprintf(stderr,"** -pad_to_node needs a positive integer,\n"
                        "   or standard mesh description such as ld120\n");
         exit(1);
       }
       used[ii] = 1 ; ii++;
       
       if (!strncasecmp(argv[ii],"ld",2)) {
         if (strlen(argv[ii]) < 3) {
            fprintf(stderr,"** need a number right after ld (like ld120)\n");
            exit(1);
         }
         MRILIB_DomainMaxNodeIndex = SUMA_IcoNums(atoi(argv[ii]+2), 0, 'n')-1;
         if( ttt ) fprintf(stderr, "ld pad_to_node %d\n", 
                                    MRILIB_DomainMaxNodeIndex);
       } else if (!strncasecmp(argv[ii],"rd",2)) {
         if (strlen(argv[ii]) < 3) {
            fprintf(stderr,"** need a number right after rd (like rd6)\n");
            exit(1);
         }
         MRILIB_DomainMaxNodeIndex = SUMA_IcoNums(atoi(argv[ii]+2), 1, 'n')-1;
         if( ttt ) fprintf(stderr, "rd pad_to_node %d\n", 
                                    MRILIB_DomainMaxNodeIndex);
       } else if (!strncasecmp(argv[ii],"d:",2)) {
         THD_3dim_dataset *dset=NULL;
         if (strlen(argv[ii]) < 3) {
            fprintf(stderr,
               "** need a dataset right after d: (like d:hello.niml.dset)\n");
            exit(1);
         }
         dset = THD_open_dataset(argv[ii]+2);
         if (dset) { 
            DSET_MAX_NODE(dset, MRILIB_DomainMaxNodeIndex); 
            DSET_delete(dset); dset = NULL;
            if( ttt ) fprintf(stderr, "d: pad_to_node %d\n", 
                                    MRILIB_DomainMaxNodeIndex);         
         } else {
            fprintf(stderr,"** Could not load dset %s to determine padding\n",
                           argv[ii]+2);
         } 
       } else {
         MRILIB_DomainMaxNodeIndex = atoi(argv[ii]);
         if( ttt ) fprintf(stderr, "pad_to_node %d\n", 
                                    MRILIB_DomainMaxNodeIndex);     
       }
       if (MRILIB_DomainMaxNodeIndex < 0) {
         fprintf(stderr,"** parameter for -pad_to_node (%d) is negative!\n",
                        MRILIB_DomainMaxNodeIndex);
         exit(1);
       }else if (MRILIB_DomainMaxNodeIndex > 500000) {
         fprintf(stderr,
                  "** parameter for -pad_to_node (%d) is suspiciously large.\n"
                  "   I hope you know what you're doing.\n",
                  MRILIB_DomainMaxNodeIndex );
       }
       used[ii] = 1;
       continue ;
     }
     
     if( strcmp(argv[ii],"-np") == 0 ||
         strcmp(argv[ii],"-npq") == 0 ){   /* ZSS, June 2011 */
       if( ttt ) fprintf(stderr,"++ argv[%d] is -np\n",ii) ;
       if (ii+1 >= narg) {
         fprintf(stderr,
               "** -np needs an integer NP such that 1024 <= NP <= 65500\n");
         exit(1);
       }
       used[ii] = 1 ; ii++;
       if (set_user_np(atoi(argv[ii]))<1) {
         if (strcmp(argv[ii-1],"-npq")) { /* not quiet mode? */
            fprintf(stderr,
               "** -np is not an integer such that 1024 <= NP <= 65500\n"
               "   -np was ignored\n");
         }
       } else {
         if (strcmp(argv[ii-1],"-npq")) 
            fprintf(stderr,"++ -np set to %d\n", get_user_np());
       }
       used[ii] = 1;
       continue ;
     }
     
     if( strcmp(argv[ii],"-npb") == 0 ){   /* ZSS, June 2011 */
       if( ttt ) fprintf(stderr,"++ argv[%d] is -npb\n",ii) ;
       if (ii+1 >= narg) {
         fprintf(stderr,
               "** -npb needs an integer NPB such that 0 <= NPB <= %d\n", 
               get_max_port_bloc());
         exit(1);
       }
       used[ii] = 1 ; ii++;
       if (set_user_np_bloc(atoi(argv[ii]))<1) {
            fprintf(stderr,
               "** -npb is not an integer such that 0 <= NPB <= %d\n"
               "   -npb was ignored\n", get_max_port_bloc());
       } 
       used[ii] = 1;
       continue ;
     }

     if( strcmp(argv[ii],"-pif") == 0 ){   /* ZSS, June 2011 */
       if( ttt ) fprintf(stderr,"++ argv[%d] is -pif\n",ii) ;
       if (ii+1 >= narg) {
         fprintf(stderr,
               "** -pif needs a string value\n");
         exit(1);
       }
       used[ii] = 1 ; ii++;
       set_user_pif(argv[ii]);
       used[ii] = 1;
       continue ;
     }

      /* -max_port_bloc number and quit */
      if( strncmp(argv[ii],"-max_port_bloc", 8) == 0) {
         int pp = 0;
         pp = get_max_port_bloc(); 
         if (strcmp(argv[ii-1], "-max_port_bloc_quiet")) {
            fprintf(stdout, "Maximum port bloc number: %d\n", 
                                pp); 
         } else {
            fprintf(stdout, "%d\n", pp); 
         }
         exit(0);
      }
      
      /* -num_assigned port number and quit */
      if( strncmp(argv[ii],"-num_assigned_ports", 8) == 0) {
         int pp = 0;
         pp = get_max_port_bloc(); 
         if (strcmp(argv[ii-1], "-num_assigned_ports_quiet")) {
            fprintf(stdout, "Number of assigned ports: %d\n", 
                                pp); 
         } else {
            fprintf(stdout, "%d\n", pp); 
         }
         exit(0);
      }
      

     /*** if get to here, argv[ii] is nothing special ***/

   } /* end of loop over argv[] */

   /*--- compress out used up argv[] entries ---*/

   for( nused=0,ii=narg-1 ; ii >= 1 ; ii-- ){
     if( !used[ii] ) continue ;
     for( jj=ii+1 ; jj < narg ; jj++ ) argv[jj-1] = argv[jj] ;
     argv[narg-1] = NULL ; narg-- ; nused++ ;
   }

   if( ttt && nused > 0 )
     fprintf(stderr,"++ 'used up' %d argv[] entries, leaving %d\n",nused,narg) ;

   free((void *)used) ; *argc = narg ; return(nused);
}


/*-------------------------------------------------------------------------*/
/* These functions moved here: 05 Feb 2008. */

int THD_deathcon(void)  /* 06 Jun 2007 */
{
   char *ppp = my_getenv("AFNI_DECONFLICT") ;
   if( ppp != NULL && *ppp == 'N' ) return 1 ;
   return 0 ;
}

/*-------------------------------------------------------------------------*/

static int force_ok_overwrite = 0 ;
void THD_force_ok_overwrite( int ii ){ force_ok_overwrite = ii; }
int THD_get_ok_overwrite (void) { return(force_ok_overwrite); }

int THD_ok_overwrite(void)  /* Jan 2008 */
{
   char *ppp=my_getenv("AFNI_DECONFLICT");
   if( force_ok_overwrite ) return 1 ;
   if (ppp && strcmp(ppp,"OVERWRITE")==0) return 1;
   return 0;
}

/* accessor functions for setting and getting globalrange variable */
static int image_globalrange = -1;

/* force globalrange to a specific value - reset with -1 */
void THD_set_image_globalrange(int ii)
{
   image_globalrange = ii;
}

/* get current value (maybe default value) for image globalrange */
int THD_get_image_globalrange()
{
   char *temp_envstr = NULL;

   if(image_globalrange<0) {
      if(AFNI_yesenv("AFNI_IMAGE_GLOBALRANGE")) image_globalrange = 1;
      else {
         temp_envstr = my_getenv("AFNI_IMAGE_GLOBALRANGE");
         if(temp_envstr){
            if((strcasecmp(temp_envstr,"VOLUME")==0) ||
               (strcasecmp(temp_envstr,"SUBBRICK")==0))
               image_globalrange = 1;   /* also same as "YES" */
            else if ((strcasecmp(temp_envstr,"DSET")==0) ||
                      (strcasecmp(temp_envstr,"DATASET")==0))
               image_globalrange = 2;   /* apply display LUT based on whole dataset */
         }
         if(image_globalrange < 0)
            image_globalrange = 0;   /* default to slice-based scaling */
      }
   }

   return(image_globalrange);
}

/* set short string to put in image viewer */
char *THD_get_image_globalrange_str()
{
   int ig;

   ig = THD_get_image_globalrange();

   switch (ig) {
      default:
      case 0 : return("Slice");
      case 1 : return("Vol");
      case 2 : return("Dset");
   }
}

/* cycle to next range setting */
void THD_cycle_image_globalrange()
{
   int ig;
   
   ig = THD_get_image_globalrange();
   ig++;
   if(ig>2) ig = 0;
   THD_set_image_globalrange(ig);
}

