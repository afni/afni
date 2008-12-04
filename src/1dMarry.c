#include "mrilib.h"

#define NFMAX 26

int main( int argc , char *argv[] )
{
   int iarg=1 ;
   int do_divorce=0 ;
   char *sep = "*," ; int nsep=2 ;
   char *filex[NFMAX] ; int nfilex , vv ;
   float filler=3.e+33 ;

   /*------------------------------------------------------------------------*/

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 1dMarry [options] file1 file2 ...\n"
      "\n"
      "  Joins together 2 (or more) ragged-right .1D files, for use with\n"
      "    3dDeconvolve -stim_times_AM2.\n"
      " **_OR_**\n"
      "  Breaks up 1 married file into 2 (or more) single-valued files.\n"
      "\n"
      "OPTIONS:\n"
      "=======\n"
      " -sep abc  == Use the first character (e.g., 'a') as the separator\n"
      "              between values 1 and 2, the second character (e.g., 'b')\n"
      "              as the separator between values 2 and 3, etc.\n"
      "            * These characters CANNOT be a blank, a tab, a digit,\n"
      "              or a non-printable control character!\n"
      "            * Default separator string is '*,' which will result\n"
      "              in output similar to '3*4,5,6'\n"
      "\n"
      " -divorce  == Instead of marrying the files, assume that file1\n"
      "              is already a married file: split time*value*value... tuples\n"
      "              into separate files, and name them in the pattern\n"
      "              'file2_A.1D' 'file2_B.1D' et cetera.\n"
      "\n"
      "If not divorcing, the 'married' file is written to stdout, and\n"
      "probably should be captured using a redirection such as '>'.\n"
      "\n"
      "NOTES:\n"
      "=====\n"
      "* You cannot use column [...] or row {...} selectors on\n"
      "    ragged-right .1D files, so don't even think about trying!\n"
      "* The maximum number of values that can be married is %d.\n"
      "    (No polygamy or polyandry jokes here, please.)\n"
      "* For debugging purposes, with '-divorce', if 'file2' is '-',\n"
      "    then all the divorcees are written directly to stdout.\n"
      "\n"
      "-- RWCox -- written hastily in March 2007 -- hope I don't repent\n"
      "         -- modified to deal with multiple marriages -- December 2008\n"

      , NFMAX
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*------------------------------------------------------------------------*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-divorce") == 0 ){
       do_divorce = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-sep") == 0 ){
       int jj ;
       iarg++ ;
       if( iarg >= argc )
         ERROR_exit("1dMarry: need argument after '-sep'") ;
       sep = argv[iarg] ; nsep = strlen(sep) ;
       if( nsep < 1 ){
         WARNING_message("Replacing empty '-sep' option with default") ;
         sep = "*," ; nsep = 2 ;
       }
       for( jj=0 ; jj < nsep ; jj++ ){
         if( isspace(sep[jj]) || iscntrl(sep[jj]) || isdigit(sep[jj]) )
           ERROR_exit("1dMarry: Illegal character after '-sep'") ;
       }
       iarg++ ; continue ;
     }

     ERROR_exit("1dMarry: Unknown option '%s'",argv[iarg]) ;
   }

   /*------------------------------------------------------------------------*/

   /** copy filenames to something more convenient **/

   for( nfilex=0 ; iarg < argc && nfilex < NFMAX ; iarg++,nfilex++ )
     filex[nfilex] = argv[iarg] ;

   /** check for various stoopidities **/

   if( nfilex == 0 )
     ERROR_exit("1dMarry: need filenames at end of command line!") ;

   if( !do_divorce && nfilex == NFMAX && iarg < argc )
     WARNING_message("Maximum number of marriages (%d) exceeded!",NFMAX) ;
   else if( do_divorce && nfilex > 2 )
     WARNING_message("Divorcing ==> filenames after #2 are ignored!") ;

   if( !THD_is_file(filex[0]) )
     ERROR_exit("1dMarry: file '%s' does not exist!",filex[0]) ;

   if( !do_divorce ){  /* check all input files to see if */
     int nbad ;        /* they are willing to be married */
     if( nfilex < 2 )
       ERROR_exit("Need at least 2 input filenames to perform the marriage!") ;
     for( nbad=0,vv=1 ; vv < nfilex ; vv++ ){
       if( !THD_is_file(filex[vv]) ){
         ERROR_message("1Dmarry: file '%s' does not exist!",filex[vv]) ;
         nbad++ ;
       }
     }
     if( nbad > 0 )
       ERROR_exit("The desired nuptials cannot take place!") ;

   } else if( nfilex == 1 ){  /* must create name of divorce file */
                              /* since user didn't give us one   */
     filex[1] = filex[0] ;
     INFO_message(
       "No filename given to get divorce results ==> using '%s'",filex[1]) ;
   }

   /******----------- divorce first, it's easier -----------******/

   if( do_divorce ){

     MRI_IMAGE *vim ; MRI_IMARR *vimar ;
     MRI_IMAGE *aim ; float *aar ; FILE *afp ; char *aname ;
     int ii,jj , nx,ny , itop , needmult , vdim ;

     vim = mri_read_ascii_ragged_fvect( filex[0] , filler , 0 ) ;
     if( vim == NULL )
       ERROR_exit("1dMarry: can't read file '%s' for divorce",filex[0]) ;
     vimar = mri_fvect_to_imarr(vim) ; mri_free(vim) ;
     if( vimar == NULL )
       ERROR_exit("1dMarry: can't process file '%s' for divorce!?!",filex[0]);

     vdim = IMARR_COUNT(vimar) ;
     if( vdim < 2 )
       ERROR_exit("1dMarry: file '%s' doesn't have 2+ values to divorce!",filex[0]) ;
     else if( vdim > NFMAX )
       ERROR_exit("1dMarry: file '%s' has more than %s values to divorce!",
                  filex[0],NFMAX) ;
     else
       INFO_message("1dMarry: divorcing %d values from file '%s'",vdim,filex[0]) ;

     aim = IMARR_SUBIM(vimar,0) ; nx = aim->nx ; ny = aim->ny ;
     aname = malloc(strlen(filex[1])+20) ;

     for( vv=0 ; vv < vdim ; vv++ ){            /* loop over images */
       aim = IMARR_SUBIM(vimar,vv) ; aar = MRI_FLOAT_PTR(aim) ;
       if( strcmp(filex[1],"-") == 0 ){
         afp = stdout ; strcpy(aname,"stdout") ;
       } else {
         sprintf(aname,"%s_%c.1D",filex[1],'A'+vv) ; afp = fopen(aname,"w") ;
       }
       if( afp == NULL )
         ERROR_exit("1dMarry: can't open file '%s' for output!",aname) ;

       needmult = (nx > 1) ;
       for( jj=0 ; jj < ny ; jj++ ){            /* loop over rows */
         for( itop=nx-1 ; itop >= 0 ; itop-- )  /* find last good value */
           if( aar[itop+jj*nx] < filler ) break ;
         if( itop < 0 ){
           fprintf(afp," *\n") ;  /* indicates no data on this line */
         } else {
           needmult = needmult && (itop == 0) ;
           for( ii=0 ; ii <= itop ; ii++ ){
             if( aar[ii+jj*nx] < filler ) fprintf(afp," %g",aar[ii+jj*nx]) ;
             else                         fprintf(afp," *") ;
           }
         }
         if( itop == 0 && ny > 1 && needmult ){
           fprintf(afp," *") ; needmult = 0 ;
         }
         fprintf(afp,"\n") ;
       }
       if( afp != stdout ) fclose(afp) ; else fflush(stdout) ;
       INFO_message("1dMarry: wrote out file '%s'",aname) ;
     }
     DESTROY_IMARR(vimar) ;
   }

   /******---------------------- Wedding Bells! ----------------------******/

   else {

     MRI_IMAGE *aim ; MRI_IMARR *vimar ;
     float vval ;
     int ii,jj , nx=0,ny=0 , itop , needmult , ngood,vdim=nfilex ;
     FILE *fp=stdout ;

     INIT_IMARR(vimar) ;
     for( vv=0 ; vv < nfilex ; vv++ ){
       aim = mri_read_ascii_ragged( filex[vv] , filler ) ;
       if( aim == NULL )
         ERROR_exit("1dMarry: can't read file '%s'",filex[vv]) ;
       if( vv == 0 ){ nx = aim->nx ; ny = aim->ny ; }
       else if( aim->ny != ny )
         ERROR_exit("files '%s' and '%s' don't have same number of rows!",
                    filex[0] , filex[vv] ) ;
       else if( aim->nx != nx )
         ERROR_exit("files '%s' and '%s' don't have same length of rows!",
                    filex[0] , filex[vv] ) ;
       ADDTO_IMARR(vimar,aim) ;
     }

     needmult = (nx > 1) ;

#define VAL(v,i,j) (MRI_FLOAT_PTR(IMARR_SUBIM(vimar,(v))))[(i)+(j)*nx]
#define SEP(i) sep[ ((i)<nsep) ? (i) : nsep-1 ]

     for( jj=0 ; jj < ny ; jj++ ){              /* loop over rows */

       for( itop=nx-1 ; itop >= 0 ; itop-- ){   /* find last good value */
         for( ngood=vv=0 ; vv < vdim ; vv++ )
           if( VAL(vv,itop,jj) < filler ) ngood++ ;
         if( ngood == vdim ) break ;  /* at least one good value found at itop */
       }
       if( itop < 0 ){
         fprintf(fp," *\n") ;     /* signal that no values are in this row */
       } else {
         needmult = needmult && (itop == 0) ;
         for( ii=0 ; ii <= itop ; ii++ ){
           fprintf(fp," ") ;
           for( ngood=vv=0 ; vv < vdim ; vv++ )
             if( VAL(vv,itop,jj) < filler ) ngood++ ;
           if( ngood == vdim ){
             for( vv=0 ; vv < vdim ; vv++ ){
               vval = VAL(vv,ii,jj) ;
               if( vval < filler ) fprintf(fp,"%g",vval) ;
               else                fprintf(fp,"*") ;
               if( vv < vdim-1 )   fprintf(fp,"%c",SEP(vv)) ;
             }
           } else {
             fprintf(fp," *") ;
           }
         }
         if( itop == 0 && ny > 1 && needmult ){ fprintf(fp," *"); needmult = 0; }
         fprintf(fp,"\n") ; fflush(fp) ;
       }
     }
     if( fp != stdout ) fclose(fp) ;
     DESTROY_IMARR(vimar) ;
   }

   exit(0) ;
}
