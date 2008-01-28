#include "mrilib.h"

int main( int argc , char *argv[] )
{
   int iarg=1 ;
   int do_divorce=0 ;
   char sep='*' ;
   char *file1=NULL , *file2=NULL ;
   float filler=3.e+33 ;

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 1dMarry [options] file1 file2\n"
      "\n"
      "  Joins together 2 ragged-right .1D files, for use in with\n"
      "  3dDeconvolve -stim_times_AM2.\n"
      "*OR*\n"
      "  Breaks up 1 married file into 2 single-valued files.\n"
      "\n"
      "Options:\n"
      "=======\n"
      " -sep c    == Use the character 'c' as the separator for the values.\n"
      "              [Default separator is '*']\n"
      " -divorce  == Instead of marrying file1 and file2, assume that file1\n"
      "              is already a married file; split it time*value pairs\n"
      "              into two files, and name them 'file2_A.1D' 'file2_B.1D'.\n"
      "\n"
      "If not divorcing, the 'married' file is written to stdout.\n"
      "\n"
      "-- RWCox -- written hastily in March 2007 -- hope I don't repent\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-divorce") == 0 ){
       do_divorce = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-sep") == 0 ){
       iarg++ ;
       if( iarg >= argc )
         ERROR_exit("1dMarry: need argument after '-sep'") ;
       sep = argv[iarg][0] ;
       if( sep == '\0' || isspace(sep) || iscntrl(sep) )
         ERROR_exit("1dMarry: Illegal character after '-sep'") ;
       iarg++ ; continue ;
     }

     ERROR_exit("1dMarry: Illegal option '%s'",argv[iarg]) ;
   }

   if( iarg >= argc )
     ERROR_exit("1dMarry: need filenames at end of command line!") ;

   file1 = argv[iarg++] ;
   if( !THD_is_file(file1) )
     ERROR_exit("1dMarry: file '%s' does not exist!",file1) ;
   if( iarg >= argc ){
     if( !do_divorce )
       ERROR_exit("1dMarry: need second filename at end of command line!") ;
     file2 = file1 ;
   } else {
     file2 = argv[iarg++] ;
     if( !do_divorce && !THD_is_file(file2) )
       ERROR_exit("1dMarry: file '%s' does not exist!",file2) ;
   }

   /******-- divorce first, it's easier --******/

   if( do_divorce ){
     MRI_IMAGE *cim , *aim , *bim ;
     MRI_IMARR *abimar ;
     float *aar , *bar ;
     int ii,jj , nx,ny , itop , needmult ;
     char *aname , *bname ;
     FILE *afp   , *bfp   ;

     cim = mri_read_ascii_ragged_complex( file1 , filler ) ;
     if( cim == NULL )
       ERROR_exit("1dMarry: can't read file '%s'",file1) ;
     abimar = mri_complex_to_pair( cim ) ;
     aim = IMARR_SUBIM(abimar,0) ; aar = MRI_FLOAT_PTR(aim) ;
     bim = IMARR_SUBIM(abimar,1) ; bar = MRI_FLOAT_PTR(bim) ;
     nx = aim->nx ; ny = aim->ny ;
     aname = malloc(strlen(file2)+20) ; sprintf(aname,"%s_A.1D",file2) ;
     bname = malloc(strlen(file2)+20) ; sprintf(bname,"%s_B.1D",file2) ;
     afp = fopen(aname,"w") ; bfp = fopen(bname,"w") ;

     needmult = (nx > 1) ;
     for( jj=0 ; jj < ny ; jj++ ){
       for( itop=nx-1 ; itop >= 0 ; itop-- ){
         if( aar[itop+jj*nx] < filler &&
             bar[itop+jj*nx] < filler   ) break ;
       }
       if( itop < 0 ){
         fprintf(afp," *\n") ; fprintf(bfp," *\n") ;
       } else {
         needmult = needmult && (itop == 0) ;
         for( ii=0 ; ii <= itop ; ii++ ){
           if( aar[ii+jj*nx] < filler && bar[ii+jj*nx] < filler ){
             fprintf(afp," %g",aar[ii+jj*nx]) ;
             fprintf(bfp," %g",bar[ii+jj*nx]) ;
           } else {
             fprintf(afp," *") ; fprintf(bfp," *") ;
           }
         }
         if( itop == 0 && ny > 1 && needmult ){
           fprintf(afp," *") ; fprintf(bfp," *") ; needmult = 0 ;
         }
         fprintf(afp,"\n") ; fprintf(bfp,"\n") ;
       }
     }
     fclose(afp) ; fclose(bfp) ;
     INFO_message("Wrote out files '%s' and '%s'",aname,bname) ;
   }

   /******-- Wedding Bells! --******/

   else {
     MRI_IMAGE *aim , *bim ;
     float *aar , *bar ;
     int ii,jj , nx,ny , itop , needmult ;
     FILE *fp=stdout ;

     aim = mri_read_ascii_ragged( file1 , filler ) ;
     if( aim == NULL )
       ERROR_exit("1dMarry: can't read file '%s'",file1) ;
     bim = mri_read_ascii_ragged( file2 , filler ) ;
     if( bim == NULL )
       ERROR_exit("1dMarry: can't read file '%s'",file2) ;

     nx = aim->nx ; ny = aim->ny ;
     if( bim->ny != ny )
       ERROR_exit("1dMarry: files '%s' and '%s' don't have same number of rows",
                 file1 , file2 ) ;
     if( bim->nx != nx )
       ERROR_exit("1dMarry: files '%s' and '%s' don't have same length of rows",
                  file1 , file2 ) ;
     aar = MRI_FLOAT_PTR(aim) ; bar = MRI_FLOAT_PTR(bim) ;
     needmult = (nx > 1) ;
     for( jj=0 ; jj < ny ; jj++ ){
       for( itop=nx-1 ; itop >= 0 ; itop-- ){
         if( aar[itop+jj*nx] < filler &&
             bar[itop+jj*nx] < filler   ) break ;
       }
       if( itop < 0 ){
         fprintf(fp," *\n") ;
       } else {
         needmult = needmult && (itop == 0) ;
         for( ii=0 ; ii <= itop ; ii++ ){
           if( aar[ii+jj*nx] < filler && bar[ii+jj*nx] < filler )
             fprintf(fp," %g%c%g",aar[ii+jj*nx],sep,bar[ii+jj*nx]) ;
           else
             fprintf(fp," *") ;
         }
         if( itop == 0 && ny > 1 && needmult ){ fprintf(fp," *"); needmult = 0; }
         fprintf(fp,"\n") ;
       }
     }
     if( fp != stdout ) fclose(fp); else fflush(stdout);
   }

   exit(0) ;
}
