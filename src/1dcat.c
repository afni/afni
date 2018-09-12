/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

#define MAXIM 1024
int main( int argc , char * argv[] )
{
   int nim , ii , jj , kk , nx, narg, oform ;
   MRI_IMAGE **inim ;  int num_inim=0 ;
   NI_element **inel ; int num_inel=0 ;
   int first_nx=0 ;
   float *far;
   char *formatstr=NULL, *sel=NULL, *fname=NULL;
   int nonconst=0 , ncol,ncold , cc , nonfixed=0 , stack=0;
   intvec *ncv=NULL ;
   char *hline=NULL ;

   mainENTRY("1dcat:main");

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
"Usage: 1dcat [options] a.1D b.1D ...\n"
"  where each file a.1D, b.1D, etc. is a 1D file.\n"
"  In the simplest form, a 1D file is an ASCII file of numbers\n"
"  arranged in rows and columns.\n"
"\n"
"1dcat takes as input one or more 1D files, and writes out a 1D file\n"
"containing the side-by-side concatenation of all or a subset of the\n"
"columns from the input files.\n"
"\n"
"* Output goes to stdout (the screen); redirect (e.g., '>') to save elsewhere.\n"
"* All files MUST have the same number of rows!\n"
"* Any header lines (i.e., lines that start with '#') will be lost.\n"
"* For generic 1D file usage help and information, see '1dplot -help'\n"
"\n"
"OPTIONS:\n"
"--------\n"
"  -nonconst = Columns that are identically constant should be omitted\n"
"              from the output.\n"
"\n"
"  -nonfixed = Keep only columns that are marked as 'free' in the \n"
"              3dAllineate header from '-1Dparam_save'.\n"
"              If there is no such header, all columns are kept.\n"
"\n"
"  -form FORM = Format of the numbers to be output.\n"
"               You can also substitute -form FORM with shortcuts such \n"
"               as -i, -f, or -c.\n"
"               For help on -form's usage, and its shortcut versions\n"
"               see ccalc's help for the option of the same name. \n"
"\n"
"  -stack = Stack the columns of the resultant matrix in the output.\n"
"\n"
"  -sel SEL = Apply the same column/row selection string to all filenames\n"
"             on the command line.\n"
"             For example:\n"
"               1dcat -sel '[0,2]' f1.1D f2.1D\n"
"             is the same as: 1dcat f1.1D'[1,2]' f2.1D'[1,2]'\n"
"             The advantage of the option is that it allows wildcard use\n"
"             in file specification so that you can run something like:\n"
"               1dcat -sel '[0,2]' f?.1D\n"
"\n"
"EXAMPLE:\n"
"--------\n"
"  Input file 1:\n   1\n   2\n   3\n   4\n"
"  Input file 2:\n   5\n   6\n   7\n   8\n"
"\n"
"  1dcat data1.1D data2.1D > catout.1D\n"
"  Output file: \n   1 5\n   2 6\n   3 7\n   4 8\n"
"\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   machdep() ;

   /* do we have any options? */
   oform = CCALC_NOT_SET;
   sel = NULL;
   stack = 0;
   narg = 1;
   while (narg < argc && argv[narg][0] == '-') {

      if( strncmp(argv[narg],"-nonconst",7) == 0 ){  /* 04 Dec 2010 */
        nonconst++ ; narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-nonfixed",7) == 0 ){  /* 06 Dec 2010 */
        nonfixed++ ; narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-stack",6) == 0 ){  /* 05 Sep 2013 */
        stack = 1 ; narg++ ; continue ;
      }

      if (strcmp(argv[narg],"-form") == 0) {
         ++narg;
         if (narg >= argc) ERROR_exit("need argument after -form") ;
         if (strcmp(argv[narg],"double") == 0 ) oform = CCALC_DOUBLE;
         else if (strcmp(argv[narg],"nice") == 0 ) oform = CCALC_NICE;
         else if (strcmp(argv[narg],"int") == 0 ) oform = CCALC_INT;
         else if (strcmp(argv[narg],"rint") == 0 ) oform = CCALC_INT;
         else if (strcmp(argv[narg],"fint") == 0 ) oform = CCALC_FINT;
         else if (strcmp(argv[narg],"cint") == 0 ) oform = CCALC_CINT;
         else if (strlen(argv[narg])<=256) {
            oform = CCALC_CUSTOM;
            formatstr = argv[narg];
         }
         else {
            ERROR_exit("Format type '%s' not supported :(",argv[narg]) ;
         }
         ++narg;
      } else if (strcmp(argv[narg],"-sel") == 0) {
         ++narg;
         if (narg >= argc)  {
	         ERROR_exit("need argument after -sel ");
         }
         sel = argv[narg]; ++narg;
      } else if (strncmp(argv[narg],"-d",2) == 0) {
         oform = CCALC_DOUBLE; ++narg;
      } else if (strncmp(argv[narg],"-n",2) == 0) {
         oform = CCALC_NICE; ++narg;
      } else if (strncmp(argv[narg],"-i",2) == 0) {
         oform = CCALC_INT; ++narg;
      } else if (strncmp(argv[narg],"-r",2) == 0) {
         oform = CCALC_INT; ++narg;
      } else if (strncmp(argv[narg],"-f",2) == 0) {
         oform = CCALC_FINT; ++narg;
      } else if (strncmp(argv[narg],"-c",2) == 0) {
         oform = CCALC_CINT; ++narg;
      } else { /* break if option is not recognized */
         ++narg;
         break;
      }
   }

   /* read input files */

   nim  = argc-narg ;
   inim = (MRI_IMAGE **)  malloc( sizeof(MRI_IMAGE *)  * nim ) ;
   inel = (NI_element **) malloc( sizeof(NI_element *) * nim ) ;
   ncol = 0 ;
   if( nonconst || nonfixed ) MAKE_intvec(ncv,1) ;
   for( jj=0 ; jj < nim ; jj++ ){

#if 0                                   /** for testing only **/
      if( AFNI_yesenv("ragged") ){
        MRI_IMAGE *qim ;
        qim      = mri_read_ascii_ragged( argv[jj+narg] , 3.e+33 ) ;
        fprintf(stderr,"qim: nx=%d ny=%d\n",qim->nx,qim->ny) ;
        inim[jj] = mri_transpose(qim) ; mri_free(qim) ;
      } else
#endif

      if (sel) {
         fname = (char *)
                  calloc((strlen(argv[jj+narg])+strlen(sel)+1), sizeof(char));
         strcat(fname, argv[jj+narg]); strcat(fname, sel);
      } else {
         fname = argv[jj+narg];
      }

      ncold = ncol ;

      if( STRING_HAS_SUFFIX(fname,".tsv") ){
        inel[jj] = THD_read_tsv(fname) ; inim[jj] = NULL ;
        if( inel[jj] == NULL )
          ERROR_exit("Can't read input file '%s'",fname) ;
        if( jj == 0 ) first_nx = inel[jj]->vec_len ;
        else if( inel[jj]->vec_len != first_nx )
          ERROR_exit("Input file %s doesn't match first file %s in length!",
                     fname,argv[narg]) ;
        ncol += inel[jj]->vec_num ; num_inel++ ;

#if 0
fprintf(stderr,"%s : ",fname) ;
for( kk=0; kk < inel[jj]->vec_num ; kk++ )
  fprintf(stderr," %d",inel[jj]->vec_typ[kk]) ;
fprintf(stderr,"\n") ;
#endif

      } else {
        inim[jj] = mri_read_1D( fname ) ; inel[jj] = NULL ;
        if( inim[jj] == NULL )
          ERROR_exit("Can't read input file '%s'",fname) ;
        if( jj == 0 ) first_nx = inim[jj]->nx ;
        else if( inim[jj]->nx != first_nx )
          ERROR_exit("Input file %s doesn't match first file %s in length!",
                     fname,argv[narg]) ;
        ncol += inim[jj]->ny ; num_inim++ ;
      }

      if( ncv != NULL && inim[jj] != NULL ){  /* check for constant columns [04 Dec 2010] */
        RESIZE_intvec(ncv,ncol) ;
        for( kk=0 ; kk < inim[jj]->ny ; kk++ ) ncv->ar[ncold+kk] = 1 ;
        far = MRI_FLOAT_PTR(inim[jj]) ; nx = inim[jj]->nx ;
        if( nonconst ){
          for( kk=0 ; kk < inim[jj]->ny ; kk++ ){ /* loop over columns */
            for( ii=1 ; ii < nx ; ii++ ){         /* loop down column */
              if( far[ii+kk*nx] != far[kk*nx] ) break ;
            }
            if( ii == nx ) ncv->ar[ncold+kk] = 0 ; /* constant */
          }
        }
        if( nonfixed ){
          char *hl = mri_read_1D_headerlines( fname ) ;
          if( hl != NULL && *hl == '#' ){
            char *spt = strchr(hl,'\n') ;
            if( spt != NULL ) spt = strchr(spt,'#') ; /* start of line 2 */
            if( spt != NULL ){
              NI_str_array *sar = NI_decode_string_list( spt+1 , "~" ) ;
              if( sar != NULL && sar->num >= inim[jj]->ny ){
                for( kk=0 ; kk < inim[jj]->ny ; kk++ ){
                  spt = strchr(sar->str[kk],'$') ;
                  if( spt != NULL && spt[1] == '\0' ) ncv->ar[ncold+kk] = 0 ;
                  else {
                    if( hline == NULL ) hline = strdup("#") ;
                    hline = THD_zzprintf( hline , " %s" , sar->str[kk] ) ;
                  }
                }
              }
              NI_delete_str_array(sar) ;
            }
          }
        }
      } /* end of computing ncv = array marking non-constant vectors */
      if (sel) {
         free(fname); fname = NULL;
      }
   } /* end of input loop */

   if( stack && num_inel > 0 )
     ERROR_exit("-stack and .tsv inputs are not compatible at this time :(") ;

   /* now do the output */

   if( hline != NULL ) printf("%s\n",hline) ;

   nx = first_nx ;

   if (stack) {
      if (oform == CCALC_NOT_SET) {
         for( cc=jj=0 ; jj < nim ; jj++ ){
            far = MRI_FLOAT_PTR(inim[jj]) ;
            for( kk=0 ; kk < inim[jj]->ny ; kk++,cc++ ){
               for( ii=0 ; ii < nx ; ii++ ){
                  if( ncv == NULL || ncv->ar[cc] )
                    printf(" %g\n", far[ii+kk*nx] ) ;
               }
            }
         }
      } else {
         for( cc=jj=0 ; jj < nim ; jj++ ){
            far = MRI_FLOAT_PTR(inim[jj]) ;
            for( kk=0 ; kk < inim[jj]->ny ; kk++,cc++ ){
               for( ii=0 ; ii < nx ; ii++ ){
                  if( ncv == NULL || ncv->ar[cc] )
                    printf(" %s\n",
                        format_value_4print(far[ii+kk*nx], oform, formatstr ));
               }
            }
         }
      }
   } else {  /* not stacked */
      if (oform == CCALC_NOT_SET) {
         for( ii=0 ; ii < nx ; ii++ ){
            for( cc=jj=0 ; jj < nim ; jj++ ){
               if( inim[jj] != NULL ){
                 far = MRI_FLOAT_PTR(inim[jj]) ;
                 for( kk=0 ; kk < inim[jj]->ny ; kk++,cc++ ){
                    if( ncv == NULL || ncv->ar[cc] )
                      printf(" %g", far[ii+kk*nx] ) ;
                   /* printf(" %+.2f", far[ii+kk*nx] ) ;*/
                 }
               } else {  /* write inel instead */
                 for( kk=0 ; kk < inel[jj]->vec_num ; kk++,cc++ ){
                   if( inel[jj]->vec_typ[kk] == NI_FLOAT ){
                     far = (float *)inel[jj]->vec[kk] ;
                     printf(" %g",far[ii]) ;
                   } else if( inel[jj]->vec_typ[kk] == NI_STRING ){
                     char **cpt = (char **)inel[jj]->vec[kk] ;
                     printf(" %s" , (cpt[ii] != NULL) ? cpt[ii] : "(null)" ) ;
                   }
                 }
               }
            } 
            printf("\n") ;
         }
      } else {
         for( ii=0 ; ii < nx ; ii++ ){
            for( cc=jj=0 ; jj < nim ; jj++ ){
               if( inim[jj] != NULL ){
                 far = MRI_FLOAT_PTR(inim[jj]) ;
                 for( kk=0 ; kk < inim[jj]->ny ; kk++,cc++ ){
                    if( ncv == NULL || ncv->ar[cc] )
                      printf(" %s",
                          format_value_4print(far[ii+kk*nx], oform, formatstr ));
                 }
               } else { /* write inel instead */
                 for( kk=0 ; ii < inel[jj]->vec_num ; kk++,cc++ ){
                   if( inel[jj]->vec_typ[jj] == NI_FLOAT ){
                     far = (float *)inel[jj]->vec[kk] ;
                      printf(" %s",
                          format_value_4print(far[ii], oform, formatstr ));
                   } else if( inel[jj]->vec_typ[jj] == NI_STRING ){
                     char **cpt = (char **)inel[jj]->vec[kk] ;
                     printf(" %s" , (cpt[ii] != NULL) ? cpt[ii] : "(null)" ) ;
                   }
                 }
               }
            }
            printf("\n") ;
         }
      }
   }
   exit(0) ;
}
