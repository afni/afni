/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>

#include "machdep.h"

/*** the command that will output the info from ParaVision ***/

#ifdef HP
#  define PIPE_COMMAND "remsh 3T60 -l scan ./bin/wrap"
#else
#  define PIPE_COMMAND "rsh 3T60 -l scan ./bin/wrap"
#endif

/*** miscellaneous constants ***/

#define INFO_SIZE     (16*1024)       /* bytes */
#define SHORT_DELAY      1            /* msec */
#define LONG_DELAY      10

#define DEBUG

/*** global variables and prototypes ***/

int dummy = 0 ;

int RT_get_3T_info(void) ;
int RT_3T_to_AFNI( int , char * , char * ) ;

/*******************************************************************************
  Gets the information about the current imaging sequence from the MCW
  Bruker 3T/60 scanner, formats it for AFNI, and sends it to AFNI.
  Usage:
   3T_toafni [-dummy]
  The output is written to stdout (AFNI will pipe it into itself).  If the
  -dummy option is used, then the data from the scanner will be read from
  stdin instead of from the scanner console computer.  Note that this program
  exits with _exit(), since it is likely to be forked from AFNI.
********************************************************************************/

int main( int argc , char * argv[] )
{
   int iarg = 1 ;

   if( argc > 1 && strncmp(argv[1],"-help",4) == 0 ){
      printf("Usage: 3T_toafni [-dummy]\n"
             "  Gets information about current imaging sequence from the MCW\n"
             "  Bruker 3T/60 scanner, formats it for AFNI, and sends it to AFNI.\n"
             "  The output is written to stdout ((AFNI will pipe it into itself).\n"
             "  If the -dummy option is used, then the data from the scanner will\n"
             "  be read from stdin instead of from the scanner console computer.\n"
            ) ;
      _exit(0) ;
   }

   /** loop over command line arguments **/

   while( iarg < argc ){

      /** -dummy **/

      if( strncmp(argv[iarg],"-dummy",4) == 0 ){
         dummy = 1 ;
         iarg++ ; continue ;
      }

      /** unknown argument **/

      fprintf(stderr,"3T_toafni: unknown argument '%s'\n",argv[iarg]) ;
      _exit(1) ;
   }

   RT_get_3T_info() ; _exit(0) ;
}

/*******************************************************************************/

int RT_get_3T_info(void)
{
   FILE * fp ;
   char * buf  = (char *) malloc( sizeof(char) * INFO_SIZE ) ; int nbuf  = 0 ;
   char * info = (char *) malloc( sizeof(char) * INFO_SIZE ) ; int ninfo = 0 ;
   int jj ;

   /** send message to 3T to get ParaVision control information **/

   if( !dummy ){
      fp = popen( PIPE_COMMAND , "r" ) ;  /* open pipe to 3T */
      if( fp == NULL ){
         fprintf(stderr,"3T_toafni: can't open pipe to 3T60!\n") ; _exit(1) ;
      }
   } else {
      fp = stdin ;  /* dummy input from stdin */
   }

   /** read control information from pipe until all used up **/

   while( fgets(buf+nbuf,INFO_SIZE-nbuf,fp) != NULL ){
      nbuf = strlen(buf) ;
   }
   if( !dummy ) pclose(fp) ;  /* close pipe, if it was opened */

   /** convert ParaVision control data into AFNI control data **/

   RT_3T_to_AFNI( nbuf,buf , info ) ;

   /** send data to the parent **/

   ninfo = strlen(info) ;
   fwrite( info , 1 , ninfo , stdout ) ; fflush(stdout) ;
   free(buf) ; free(info) ; return 0 ;
}

/*******************************************************************************/

char * RT_find_value( char * name , char * buf )
{
   char * qq ;

   if( name == NULL || buf == NULL ) return NULL ;

   qq = strstr( buf , name ) ;
   if( qq == NULL ) return NULL ;
   qq = strstr( qq , "=") ;
   if( qq == NULL ) return NULL ;
   return (qq+2) ;
}

#define INC_QAR 10

int RT_number_array( char * name , char * buf , float ** far )
{
   char * qq , * eptr ;
   int iar , nqar ;
   float * qar ;
   float val ;

   qq = RT_find_value( name , buf ) ;
   if( qq == NULL ){ *far = NULL ;  return 0 ; }

   iar  = 0 ;
   nqar = INC_QAR ;
   qar  = (float *) malloc( sizeof(float) * INC_QAR ) ;

   if( *qq == '{' ) qq++ ;  /* skip leading array brace */

   /* loop until end of line, end of string, or conversion fails */

   do{
      val = strtod( qq , &eptr ) ;
      if( val == 0.0 && eptr == qq ) break ;  /* stop when no conversion */

      if( iar == nqar ){
         nqar += INC_QAR ;
         qar   = (float *) realloc( qar , sizeof(float) * nqar ) ;
      }
      qar[iar++] = val ; qq = eptr ; if( *qq == ',' ) qq++ ;
   } while( *qq != '\0' && *qq != '\n' ) ;

   if( iar == 0 ){ free(qar) ; qar = NULL ; }

   *far = qar ; return iar ;
}

/*******************************************************************************/

/** get AFNI orientation codes from matrix elements **/

#define XYZ_TO_ANAT(x,y,z)                                \
    ((x) > 0.999) ? "L-R" : ((x) <-0.999) ? "R-L"         \
  : ((y) > 0.999) ? "P-A" : ((y) <-0.999) ? "A-P"         \
  : ((z) > 0.999) ? "I-S" : ((z) <-0.999) ? "S-I" : "GEN"

/** get an integer **/

#define GET_INT(iname,oname)                                      \
 do{ vstart = RT_find_value( iname , buf3T ) ;                    \
     if( vstart != NULL ){                                        \
        ival = strtol( vstart , NULL , 10 ) ;                     \
        sprintf(buf,"%s %d\n",oname,ival) ; strcat(info,buf) ;    \
     } else { fprintf(stderr,"3T_toafni: %s not found\n",iname) ; } } while(0)

/** get a float **/

#define GET_FLOAT(iname,oname)                                    \
 do{ vstart = RT_find_value( iname , buf3T ) ;                    \
     if( vstart != NULL ){                                        \
        val = strtod( vstart , NULL ) ;                           \
        sprintf(buf,"%s %g\n",oname,val) ; strcat(info,buf) ;     \
     } else { fprintf(stderr,"3T_toafni: %s not found\n",iname) ; } } while(0)

/*******************************************************************************/

int RT_3T_to_AFNI( int nbuf , char * buf3T , char * info )
{
   char * vstart ;
   float val ;
   int  ival , jj , nfar ;
   char buf[128] ;
   float * far ;
   char * fes=NULL , * pes=NULL , * ses=NULL ;

#ifdef DEBUG
fprintf(stderr,"3T buffer follows:\n%s\n",buf3T) ;
#endif

   info[0] = '\0' ;  /* initialize output info to be empty */

   /* read scalar values and format them for AFNI */

   GET_INT( "NSLICES" , "ZNUM"   ) ;
   GET_INT( "NR"      , "NUMVOL" ) ;

#if 0
   GET_FLOAT( "ACQ_slice_thick" , "ZDELTA" ) ;
#else
   nfar = RT_number_array( "ACQ_slice_sepn" , buf3T , &far ) ;
   if( nfar > 0 ){
      float zdel = fabs(far[0]) ;
      if( zdel > 0 ){
         sprintf(buf,"ZDELTA %g\n",zdel) ; strcat(info,buf) ;
      } else {
         fprintf(stderr,"3T_toafni: ACQ_slice_sepn not positive\n") ;
         GET_FLOAT( "ACQ_slice_thick" , "ZDELTA" ) ;
      }
   } else {
      fprintf(stderr,"3T_toafni: ACQ_slice_sepn not found\n") ;
      GET_FLOAT( "ACQ_slice_thick" , "ZDELTA" ) ;
   }
   if( far != NULL ) free(far) ;
#endif

   /* read slice orientation matrix and compute AFNI orientation codes */

   nfar = RT_number_array( "ACQ_grad_matrix" , buf3T , &far ) ;
   if( nfar >= 9 ){
      float fex=far[0],fey=far[1],fez=far[2] ,  /* frequency encode */
            pex=far[3],pey=far[4],pez=far[5] ,  /* phase encode */
            sex=far[6],sey=far[7],sez=far[8]  ; /* slice encode */

#if 1
      fes = XYZ_TO_ANAT(-fex,-fey,-fez) ; /* mirror imaging */
      pes = XYZ_TO_ANAT(-pex,-pey,-pez) ; /* mirror imaging */
#else
      fes = XYZ_TO_ANAT( fex, fey, fez) ; /* no mirror imaging */
      pes = XYZ_TO_ANAT( pex, pey, pez) ; /* no mirror imaging */
#endif

      ses = XYZ_TO_ANAT( sex, sey, sez) ;
      sprintf(buf,"XYZAXES %s %s %s\n",fes,pes,ses) ; strcat(info,buf) ;
   } else if( nfar == 0 ){
      fprintf(stderr,"3T_toafni: ACQ_grad_matrix not found\n") ;
   } else {
      fprintf(stderr,"3T_toafni: ACQ_grad_matrix not good\n") ;
   }
   if( far != NULL ) free(far) ;

   /* read FOV */

   nfar = RT_number_array( "ACQ_fov" , buf3T , &far ) ;
   if( nfar >= 1 ){
      float xxfov , yyfov , zzfov ;

      xxfov = 10.0 * far[0] ;
      yyfov = (nfar >= 2) ? 10.0 * far[1] : 0.0 ;
      zzfov = (nfar >= 3) ? 10.0 * far[2] : 0.0 ;

      sprintf(buf,"XYFOV %g %g %g\n",xxfov,yyfov,zzfov) ; strcat(info,buf) ;
   } else {
      fprintf(stderr,"3T_toafni: ACQ_fov not found\n") ;
   }
   if( far != NULL ) free(far) ;

   /* read slice offsets */

   nfar = RT_number_array( "ACQ_slice_offset" , buf3T , &far ) ;
   if( nfar >= 1 ){
      float zoff = far[0] ;

      if( ses == NULL ){
         sprintf(buf,"ZFIRST %g\n",zoff) ; strcat(info,buf) ;
      } else {
         sprintf(buf,"ZFIRST %g%c\n" , zoff , ses[2] ) ; strcat(info,buf) ;
      }
   } else {
      fprintf(stderr,"3T_toafni: ACQ_slice_offset not found\n") ;
   }
   if( far != NULL ) free(far) ;

   /* read slice order */

   nfar = RT_number_array( "ACQ_obj_order" , buf3T , &far ) ;
   if( nfar >= 2 ){
      int b0=far[0] , b1=far[1] ;

      if( b1-b0 == 1 ) sprintf(buf,"ZORDER seq\n") ;
      else             sprintf(buf,"ZORDER alt\n") ;

      strcat(info,buf) ;
   } else if( nfar == 1 ){
      strcat(info,"ZORDER alt\n") ;
   } else {
      fprintf(stderr,"3T_toafni: ACQ_obj_order not found\n") ;
   }
   if( far != NULL ) free(far) ;

   return 0 ;
}
