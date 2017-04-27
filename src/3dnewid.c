/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

void Syntax(void)
{
   printf(
    "Assigns a new ID code to a dataset; this is useful when making\n"
    "a copy of a dataset, so that the internal ID codes remain unique.\n\n"
    "Usage: 3dnewid dataset [dataset ...]\n"
    " or\n"
    "       3dnewid -fun [n]\n"
    "       to see what n randomly generated ID codes look like.\n"
    "       (If the integer n is not present, 1 ID code is printed.)\n"
    " or\n"
    "       3dnewid -fun11\n"
    "       to get an 11 character ID code (for use in scripting).\n"
    " or\n"
    "       3dnewid -hash STR\n"
    "       to get a unique hashcode of STR\n"
    "       (Unlike the other ways of using 3dnewid, if STR is the)\n"
    "       (same in 2 different runs, the output will be the same.)\n"
    "       (The -hash algorithm begins at step 2 in the list below.)\n"
    " or\n"
    "       3dnewid -MD5 STR\n"
    "       to get the MD5 hash of STR, should be same as -hash output \n"
    "       without the prefix and without the + and / char substitutions.\n"
    "\n"
    "How ID codes are created (here and in other AFNI programs):\n"
    "----------------------------------------------------------\n"
    "The AFNI ID code generator attempts to create a globally unique\n"
    "string identifier, using the following steps.\n"
    "1) A long string is created from the system identifier\n"
    "   information ('uname -a'), the current epoch time in seconds\n"
    "   and microseconds, the process ID, and the number of times\n"
    "   the current process has called the ID code function.\n"
    "2) This string is then hashed into a 128 bit code using the\n"
    "   MD5 algorithm. (cf. file thd_md5.c)\n"
    "3) This bit code is then converted to a 22 character string\n"
    "   using Base64 encoding, replacing '/' with '-' and '+' with '_'.\n"
    "   With these changes, the ID code can be used as a Unix filename\n"
    "   or an XML name string. (cf. file thd_base64.c)\n"
    "4) A 4 character prefix is attached at the beginning to produce\n"
    "   the final ID code.  If you set the environment variable\n"
    "   IDCODE_PREFIX to something, then its first 3 characters and an\n"
    "   underscore will be used for the prefix of the new ID code,\n"
    "   provided that the first character is alphabetic and the other\n"
    "   2 alphanumeric; otherwise, the default prefix 'NIH_' will be\n"
    "   used.\n"
    "The source code is function UNIQ_idcode() in file niml_uuid.c\n"
   ) ;
   PRINT_COMPILE_DATE ; exit(0) ;
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset ;
   int iarg ;
   MCW_idcode idc ;
   char str[256] ;

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax() ;

   iarg = 1 ;

   mainENTRY("3dnewid main"); machdep();

   if( strcmp(argv[1],"-fun") == 0 ){         /* 22 May 2000: for fun */
      int nid=0 , ii ;
      char *eee = getenv("UUID") ;
      if( argc > 2 ) nid = strtol(argv[2],NULL,10) ;
      if( nid <= 0 ) nid = 1 ;
      if( eee == NULL ){
        for( ii=0 ; ii < nid ; ii++ ){
          idc = MCW_new_idcode() ; printf("%s\n",idc.str) ;
        }
      } else {                                /* 20 Aug 2002: test of niml.c */
        for( ii=0 ; ii < nid ; ii++ ){
          eee = UUID_idcode(); printf("%s\n",eee); free(eee);
        }
      }
      exit(0) ;
   } else if ( strcmp(argv[1],"-fun11") == 0 ){ /* 10 Feb 2016 */
     int nid=0 , ii ; char *eee ;
     if( argc > 2 ) nid = strtol(argv[2],NULL,10) ;
     if( nid <= 0 ) nid = 1 ;
     for( ii=0 ; ii < nid ; ii++ ){
       eee = UNIQ_idcode_11(); printf("%s\n",eee); free(eee);
     }
     exit(0) ;
   } else if ( strcmp(argv[1],"-hash") == 0 ){ /* Oct. 2011:for repeatable fun */
      if( argc != 3 ) {
         ERROR_message("You need a string following -hash");
         exit(1);
      }
      printf("%s\n", UNIQ_hashcode(argv[2]));
      exit(0) ;
   }  else if ( strcmp(argv[1],"-MD5") == 0 ){ /* Feb. 2015:for debugging
                                                -MD5 jj.k40.gii
                                                   is giving the same string as
                                                -MD5 jj.k60.gii */
      char *eee = NULL;

      if( argc != 3 ) {
         ERROR_message("You need a string following -MD5");
         exit(1);
      }
      MD5_set_xor_use(1) ;
      eee = MD5_B64_string(argv[2]) ;
      printf("%s [with XOR of input]\n", eee);  free(eee);
      MD5_set_xor_use(0) ;
      eee = MD5_B64_string(argv[2]) ;
      printf("%s [without XOR of input]\n", eee);  free(eee);
      exit(0) ;
   }

   /*-- OK, not for fun --*/

   AFNI_logger("3dnewid",argc,argv) ;

   for( ; iarg < argc ; iarg++ ){
      dset = THD_open_one_dataset( argv[iarg] ) ;
      if( dset == NULL ){
         fprintf(stderr,"** Skipping dataset %s\n",argv[iarg]) ;
         continue ;
      }
      dset->idcode = MCW_new_idcode() ;
      sprintf(str,"3dnewid %s\n",argv[iarg]) ;
      tross_Append_History( dset , str) ;
      putenv("AFNI_DECONFLICT=OVERWRITE") ;
      THD_write_3dim_dataset( NULL , NULL , dset , False ) ;
      THD_delete_3dim_dataset( dset , False ) ;
   }
   exit(0) ;
}
