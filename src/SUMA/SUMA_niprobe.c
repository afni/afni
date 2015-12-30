#include "niml/niml.h"
#include "afni.h"
#include "suma_suma.h"

/* can't modify that poor niccc anymore */

/*--- Debug printout of a NIML element. ---*/

void NIML_to_terminal( void *nini, int mode, int do_stderr)
{
   NI_stream ns_err ;
   if (do_stderr) ns_err = NI_stream_open( "stderr:" , "w" ) ;
   else ns_err = NI_stream_open( "stdout:" , "w" ) ;
   if( ns_err != NULL ){
     NI_write_element( ns_err , nini , mode ) ;
     NI_stream_close( ns_err ) ;
   }
}

void usage_niprobe(int detail) {
      fprintf(stderr,
"\n"
"   Usage: niprobe [-dup] [-nodata] streamspec\n"
"\n"
"A program based on niccc which could bear no more modifications\n"
"This program is also for conducting certain NIML tests and checking\n"
"the content of niml files and streams.\b"
"\n"
"Examples:\n"
"    niprobe -find_nel_named histogram -f h.mean.20_mm-G-SK04.niml.hist \\\n"
"               | 1dplot -sepscl -stdin \n"
"    niprobe -find_nel_named histogram -f h.mean.20_mm-G-SK04.niml.hist \\\n"
"               | niprobe -attribute window 'stdin:' \n"
"    niprobe -find_nel_named AFNI_labeltable -f lh.OccROIs.niml.dset\n"
"\n"
"Mandatory arguments:\n"
"   streamspec: A string defining a NIML stream.\n"
"\n"
"Options:\n"
"   -dup: Duplicate the element before showing it.\n"
"         This is to test NI_duplicate function.\n"
"   -nodata: Show header parts only in output\n"
"   -attribute ATTR: Dump the value of attribute ATTR\n"
"   -match MATCH: If MATCH is exact, then attribute name\n"
"                 is matched exactly. If MATCH is partial,\n"
"                 then a match of all the characters in ATTR\n"
"                 is enough. For example, an ATTR of COEF would\n" 
"                 match any of COEF COEF.1 COEF.2, etc.\n"
"            Default is -match exact\n"
"   -f: streamspec is a filename (last option on the command line)\n"
"   -s: streamspec is an element string like: \n"
"            '<T font=9 coords=\"2.3 23 2\"/>'\n"
"            (last option on the command line)\n"
"   -stdout: write elements to stdout (default), instead of stderr\n"
"   -stderr: write elements to stderr, instead of stdout\n"
"   -#: put the # at the beginning of lines with no data (default)\n"
"   -No#: Do not put the # at the beginning of lines with no data \n"
"   -quiet: quiet stderr messages, and don't echo attribute\n"
"           name with -attribute option\n"
"   -find_nel_with_attr ATTR ATTRVAL: Only output elements \n"
"               that have an attribute ATTR of value ATTRVAL.\n"
"               a status of 1 is returned if no match is found.\n"
"   -find_nel_named NAME: Only print element named NAME\n"
"   -skip_nel_with_attr ATTR ATTRVAL: Do not output elements \n"
"               that have an attribute ATTR of value ATTRVAL.\n"
"   -mw MAX_WAIT: Don't wait on a stream for more than MAX_WAIT\n"
"                 before you receive an element. Default is 100 msec.\n"
"                 Set MAX_WAIT to -1 to wait forever and a day\n"
"   niprobe returns a status of 0 if it the stream opened.\n"
"         and there were no interruptions.\n"
"\n");
   return;
}

/*--- Open a NIML stream, read elements from it, print them ---*/

int main( int argc , char *argv[] )
{
   static char FuncName[]={"niprobe"};
   NI_stream ns ;
   void *nini = NULL, *vel=NULL;
   NI_element *nel=NULL;
   char *strm=NULL, *attr=NULL;
   int nn, mode = NI_TEXT_MODE, shhh=0, exact=1;
   FILE *outf = stdout;
   char *aa=NULL, *select_attr=NULL, *select_attr_val=NULL, *select_elmt=NULL;
   int dodup = 0, nodata=0, dostderr=0, isfile=0;
   int stat = 0, excl = 0, pound = 1, maxwait = 100, waittime = 0;
   
   SUMA_mainENTRY;
   
   nn = 1;
   while (nn < argc && argv[nn][0] == '-') {
      if (!strcmp(argv[nn], "-help") || !strcmp(argv[nn], "-h")) {
         usage_niprobe(strlen(argv[nn]) > 3 ? 2:1);
         exit(0);
      }
      if (!strcmp(argv[nn],"-dup")) {
         dodup = 1; ++nn; continue;
            }
      if (!strcmp(argv[nn],"-nodata")) {
         nodata = 0; ++nn; continue;
      }
      if (!strcmp(argv[nn],"-stdout")) {
         dostderr = 0; ++nn; continue;
      }
      if (!strcmp(argv[nn],"-stderr")) {
         dostderr = 1; ++nn; continue;
      }
      if (!strcmp(argv[nn],"-No#")) {
         pound = 0; 
         ++nn; continue;
      }
      if (!strcmp(argv[nn],"-#")) {
         pound = 1; 
         ++nn; continue;
      }
      if (!strcmp(argv[nn],"-quiet")) {
         shhh=1; 
         ++nn; continue;
      }
      if (!strcmp(argv[nn],"-f")) {
         isfile=1; 
         ++nn; continue;
      }
      if (!strcmp(argv[nn],"-s")) {
         isfile=2; 
         ++nn; continue;
      }
      if (!strcmp(argv[nn],"-attribute")) {
         ++nn;
         if (nn >= argc) {
            fprintf(stderr,"Need attribute after -attribute\n");
            exit(1);
         }  
         attr=argv[nn]; 
         ++nn; continue;
      }
      
      if (!strcmp(argv[nn],"-mw")) {
         ++nn;
         if (nn >= argc) {
            fprintf(stderr,"Need time in msec after -mw\n");
            exit(1);
         }  
         maxwait = (int)strtol(argv[nn], NULL, 10);
         ++nn; continue;
      }
      
      if (!strcmp(argv[nn],"-nel_from_string")) {
         ++nn;
         if (nn >= argc) {
            fprintf(stderr,"Need string after -nel_from_string\n");
            exit(1);
         }  
         nini = (NI_element *)NI_read_element_fromstring(argv[nn]);
         
         ++nn; continue;
      }
      
      if (!strcmp(argv[nn],"-skip_nel_with_attr") ||
          !strcmp(argv[nn],"-find_nel_with_attr")) {
         if (strstr(argv[nn],"-find")) {
            excl=0;
         } else {
            excl=1;
         }
         ++nn;
         if (nn+1 >= argc) {
            fprintf(stderr,
               "Need attribute and value after -skip_nel_with_attr\n");
            exit(1);
         }  
         select_attr=argv[nn++];
         select_attr_val = argv[nn]; 
         ++nn; continue;
      }
      if (!strcmp(argv[nn],"-find_nel_named")) {
         ++nn;
         if (nn >= argc) {
            fprintf(stderr,
               "Need name after -find_nel_named\n");
            exit(1);
         }  
         select_elmt=argv[nn];
         ++nn; continue;
      }
      
      if (!strcmp(argv[nn],"-match")) {
         ++nn;
         if (nn >= argc) {
            fprintf(stderr,"Need parameter after -match\n");
            exit(1);
         }  
         if (!strcmp(argv[nn],"exact")) exact = 1;
         else if (!strcmp(argv[nn],"partial")) exact = 0;
         else {
            fprintf(stderr,"%s is not a valid value for -match. \n"
                           "Use either exact or partial\n", argv[nn]);
            exit(1);
         } 
         ++nn; continue;
      }
      ERROR_message("Bad option %s. See niprobe -help for details.\n", 
               argv[nn]);
      suggest_best_prog_option(argv[0], argv[nn]);
      exit(1);
   }
   if( argc < 2 ){
      ERROR_exit("Too few options");
   }
   
   if (pound) mode = mode|NI_HEADERSHARP_FLAG;
   
   if (nodata) mode = mode&NI_HEADERONLY_FLAG;
   
   if (nn >= argc) {
      fprintf(stderr,"Usage: niprobe [-dup] streamspec\n");exit(1);
   }
   
   strm = (char *) realloc(strm, (strlen(argv[nn])+32)*sizeof(char));
   if (isfile == 2) {
      sprintf(strm,"str:");
      ns = NI_stream_open( strm , "r" ) ;
      NI_stream_setbuf( ns , argv[nn] ) ;
   } else {
      if (isfile == 1) {
         sprintf(strm,"file:%s",argv[nn]);
      } else {
         strcpy(strm, argv[nn]);
      }
      ns = NI_stream_open( strm, "r" ) ;
      if( ns == NULL ){
         fprintf(stderr,"*** niprobe: NI_stream_open fails for %s\n", strm) ; 
         if (THD_is_file(strm)) {
            fprintf(stderr,
               "  It looks like %s is a file.\n"
               "  Make sure you use option -f before it\n",
               strm) ;       
         }  
         exit(1) ;
      }
   }
   /*** NI_stream_setbufsize( ns , 6666 ) ; ***/
   while(1){
     nn = NI_stream_goodcheck( ns , 1 ) ;
     if( nn < 0 ){
       if (strncmp(strm,"file:",5) && strncmp(strm,"str:",4)) {
         fprintf(stderr,"\n*** niprobe: Stream %s fails\n", strm); exit(1);
       } else {
         exit(stat);
       }
     }
     if( nn == 0 ){ NI_sleep(5);  continue; } /* waiting for Godot*/
     
     if (dostderr) outf = stderr;
     
     if ( (nn = NI_stream_readcheck( ns , 1 ) ) > 0) { /* check for data */
       if (!excl) {
         stat = 1; /* using -find, exit with status if not found */
       } else {
         stat = 0; /* you got something, exit well */
       }
       while( (nini = NI_read_element( ns , 2 )) ) {
         waittime = 0;
         if (select_elmt) {
            void *thisit = NULL;
            if ((thisit = SUMA_FindNgrNamedAny(nini, select_elmt))) {
               NIML_to_terminal( thisit, mode, dostderr ) ;
            }
            goto NEXT;
         }
         if (select_attr) {
            if ((aa = NI_get_attribute(nini, select_attr))) {
               if (!strcmp(aa,select_attr_val)) { /* match */
                  if (excl) goto NEXT;
                  else stat = 0; /* found something */
               } else if (!excl) goto NEXT; /* no match */
            }
         }
         if (attr) {
            if (exact) {
               aa = NI_get_attribute(nini, attr);
               if (aa) {
                  if (shhh) fprintf(outf,"%s\n", aa); 
                  else fprintf(outf,"%s: %s\n",attr, aa); 
                  exit(0);
               } else {
                  if (!shhh) fprintf(stderr,"%s: Not found.\n", attr);
                  exit(1);
               }
            } else {
               int tt=NI_element_type(nini) ;
               int nn, nfound=0;
               if( tt == NI_ELEMENT_TYPE ){
                  nel=(NI_element *)nini;
                  for( nn=0 ; nn < nel->attr_num ; nn++ ) {
                     if( strncmp(nel->attr_lhs[nn],attr, 
                                 strlen(attr)) == 0 ) {
                        if (shhh) fprintf(outf,"%s\n", nel->attr_rhs[nn]); 
                        else fprintf(outf,"%s: %s\n",
                                    nel->attr_lhs[nn], nel->attr_rhs[nn]); 
                        ++nfound;
                     }
                  }
                  if (nfound) exit(0);
                  else {
                     if (!shhh) fprintf(stderr,"%s: Not found.\n", attr);
                     exit(1);
                  }
               } else {
                  if (strncmp(strm,"file:",5)) {
                     fprintf(stderr,"\n*** niprobe: not ready for non elements\n");
                     exit(1);
                   } else {
                     exit(1);
                   }   
               }
            }
         }
         if (dodup) {
            if (!shhh) fprintf(stderr,
                           "*** niprobe: duplication returned element:\n");
            vel = NI_duplicate(nini, 1);
            NI_free_element( nini ) ; nini=NULL;
            NIML_to_terminal( vel, mode, dostderr ) ;
         } else {
           if (!shhh) fprintf(stderr,"*** niprobe: reading returned element:\n") ;
            NIML_to_terminal( nini, mode, dostderr ) ;
         }
         NEXT:
         if (nini) NI_free_element( nini ) ; nini=NULL;
         if (vel) NI_free_element( vel ); vel = NULL;
       }
       NI_sleep(5); waittime += 5; 
       if (maxwait > 0 && waittime > maxwait) {
          exit(stat);
       }
     }
   } /* while stream is good */
}
