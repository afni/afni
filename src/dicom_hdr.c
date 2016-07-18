#include "mrilib.h"

int main(int argc, char **argv)
{
   char *ppp=NULL , *sin ;
   int ii, iarg=1 , do_sin=0 , do_printf=0 , do_mul=0 , do_length=1 ;
   int do_stimes=0, do_stimes_verb=0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: dicom_hdr [options] fname [...]\n"
            "Prints information from the DICOM file 'fname' to stdout.\n"
            "Multiple files can be given on the command line.\n"
            "\n"
            "OPTIONS:\n"
            " -hex     = Include hexadecimal printout for integer values.\n"
            " -noname  = Don't include element names in the printout.\n"
            " -sexinfo = Dump Siemens EXtra INFO text (0029 1020), if present\n"
            "             (can be VERY lengthy).\n"
            " -mulfram = Dump multi-frame information, if present\n"
            "             (1 line per frame, plus an XML-style header/footer)\n"
            "             [-mulfram also implies -noname]\n"
            " -v n     = Dump n words of binary data also.\n"
#if 0
            " -printf  = Use 'printf' directly, instead of an intermediate string.\n"
#endif
            "\n"
            " -no_length        = Skip lengths and offsets (helps diffs).\n"
            " -slice_times      = Show slice times from Siemens mosaic images.\n"
            " -slice_times_verb = Same, but be more verbose about it.\n"
            "\n"
            "Based on program dcm_dump_file from the RSNA, developed at\n"
            "the Mallinckrodt Institute of Radiology.  See the source\n"
            "code file mri_dicom_hdr.c for their Copyright and license.\n"
            "\n"
            "SOME SAMPLE OUTPUT LINES:\n"
            "\n"
            "0028 0010      2 [1234   ] //              IMG Rows// 512\n"
            "0028 0011      2 [1244   ] //           IMG Columns// 512\n"
            "0028 0030     18 [1254   ] //     IMG Pixel Spacing//0.488281\\0.488281\n"
            "0028 0100      2 [1280   ] //    IMG Bits Allocated// 16\n"
            "0028 0101      2 [1290   ] //       IMG Bits Stored// 12\n"
            "0028 0102      2 [1300   ] //          IMG High Bit// 11\n"
            "\n"
            "* The first 2 numbers on each line are the DICOM group and element tags,\n"
            "   in hexadecimal.\n"
            "* The next number is the number of data bytes, in decimal.\n"
            "* The next number [in brackets] is the offset in the file of the data,\n"
            "   in decimal.  This is where the data bytes start, and does not include\n"
            "   the tag, Value Representation, etc.\n"
            "* If -noname is NOT given, then the string in the '// ... //' region is\n"
            "   the standard DICOM dictionary name for this data element.  If this string\n"
            "   is blank, then this element isn't in the dictionary (e.g., is a private\n"
            "   tag, or an addition to DICOM that the program doesn't know about, etc.).\n"
            "* The value after the last '//' is the value of the data in the element.\n"
            "* In the example above, we have a 512x512 image with 0.488281 mm pixels,\n"
            "   with 12 bits (stored in 16 bits) per pixel.\n"
            "* For vastly more detail on DICOM standard, you can start with the\n"
            "   documents at ftp://afni.nimh.nih.gov/dicom/ (1000+ pages of PDF)!\n"
            "* Also see program dicom_hinfo -- which will print out just a few user-chosen\n"
            "   values for each input file.  It can be used in a script to sort through\n"
            "   a lot of files at once.\n"
           );
     exit(0);
   }

   mainENTRY("dicom_hdr main") ; machdep() ;

   mri_dicom_nohex( 1 ) ;

   while( argv[iarg] && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-sexinfo") == 0 ){  /* 23 Dec 2002 */
       do_sin++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-printf") == 0 ){  /* 02 May 2008 */
       do_printf++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-no_length") == 0 ){  /* 17 Oct 2012 [rickr] */
       do_length = 0 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-mulfram",4) == 0 ){  /* 05 May 2008 */
       mri_dicom_noname(1) ; do_mul++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-hex") == 0 ){
       mri_dicom_nohex(0) ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-noname") == 0 ){
       mri_dicom_noname(1) ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-slice_times") == 0 ){  /* 14 Apr 2010 [rickr] */
       do_stimes++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-slice_times_verb") == 0 ){ /* 15 Apr 2010 */
       do_stimes++ ;    /* redundant, but for complete output */
       do_stimes_verb++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-v") == 0 ){
       int vv = strtol( argv[++iarg] , NULL , 10 ) ;
       if( vv > 0 ) mri_dicom_setvm( vv ) ;
       else         fprintf(stderr,"*** Illegal value after -v!\n") ;
       iarg++ ; continue ;
     }

     fprintf(stderr,"*** Unknown option: %s\n",argv[iarg]) ; iarg++ ;
   }

   if( iarg == argc ) { fprintf(stderr,"** no input files?\n"); return 0; }

   mri_dicom_header_use_printf(do_printf) ;  /* 02 May 2008 */
   mri_dicom_header_show_size_offset(do_length) ; /* 17 Oct 2012 [rickr] */
   if( do_stimes_verb ) mri_sst_set_verb(1+do_stimes_verb); /* 02 May 2011 */

   for( ii=iarg ; ii < argc ; ii++ ){
     if( ii > iarg )
       printf("---------------------------------------------------------------\n");

     mri_dicom_seterr(-1) ;  /* make sure all errors are printed - 07 May 2003 */
STATUS("calling funct mri_dicom_header()") ;
     if( ppp != NULL ) free(ppp) ;
     ppp = mri_dicom_header( argv[ii] ) ;

     /* show slice times (in lieu of header)    14 Apr 2011 [rickr] */
     if( do_stimes ) { get_and_display_siemens_times(); continue; }

     if( !do_printf && ppp != NULL ){
       off_t poff ; unsigned int plen ;
       printf("%s",ppp) ;
       mri_dicom_pxlarr( &poff , &plen ) ;
       if( plen > 0 )
         printf("Pixel array offset = %u (bytes)\n"
                "Pixel array length = %u (bytes)\n" ,
                (unsigned int)poff , plen ) ;
       if( do_sin ){
         (void) mri_imcount_dicom( argv[ii] ) ;  /* only to get the sexinfo */
         sin = mri_dicom_sexinfo() ;
         if( sin ){
           printf("................... Siemens Extra Info [0029 1020] ...................\n"
                  "%s\n" , sin ) ;
         } else {
           printf("........... Siemens Extra Info [0029 1020] = NOT PRESENT .............\n");
         }
       }
       if( do_mul ){  /* 05 May 2008 */
         MultiFrame_info *mfi = AFD_scanfor_MultiFrame(ppp) ;
         if( mfi != NULL ){
           int nz = mfi->nframe , jj ;
           printf("........... DICOM MultiFrame Information          ...........\n");
           printf("........... time_index stack_index xpos ypos zpos ...........\n");
           printf("<DICOM_MultiFrame nframe='%d'>\n",nz) ;
           for( jj=0 ; jj < nz ; jj++ ){
             printf(" %4d %4d" , mfi->time_index[jj] , mfi->stack_index[jj] ) ;
             if( mfi->xpos != NULL )
               printf("  %.3f %.3f %.3f",mfi->xpos[jj],mfi->ypos[jj],mfi->zpos[jj]);
             printf("\n") ;
           }
           printf("</DICOM_MultiFrame>\n") ;
           KILL_MultiFrame(mfi) ;
         } else {
           printf("........... DICOM MultiFrame Information = ABSENT ...........\n");
         }
       }
     } else if( !do_printf ) {
       printf("***\n*** ERROR: can't open %s as a DICOM file!\n***\n",argv[ii]) ;
     }
   }
   exit(0) ;
}
