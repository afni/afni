/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

void Syntax(void)
{
   int ii ;

   printf("Usage 1: cdf [-v] -t2p statname t params\n"
          "Usage 2: cdf [-v] -p2t statname p params\n"
          "Usage 3: cdf [-v] -t2z statname t params\n"
          "\n"
          "This program does various conversions using the cumulative distribution\n"
          "function (cdf) of certain canonical probability functions.  The optional\n"
          "'-v' indicates to be verbose -- this is for debugging purposes, mostly.\n"
          "\n"
          "Usage 1: Converts a statistic 't' to a tail probability.\n"
          "Usage 2: Converts a tail probability 'p' to a statistic.\n"
          "Usage 3: Converts a statistic 't' to a N(0,1) value (or z-score)\n"
          "         that has the same tail probability.\n"
          "\n"
          "The parameter 'statname' refers to the type of distribution to be used.\n"
          "The numbers in the params list are the auxiliary parameters for the\n"
          "particular distribution.  The following table shows the available\n"
          "distribution functions and their parameters:\n\n"
   ) ;

   printf("   statname  Description  PARAMETERS\n"
          "   --------  -----------  ----------------------------------------\n" ) ;
   for( ii=FIRST_FUNC_TYPE ; ii <= LAST_FUNC_TYPE ; ii++ ){
      if( FUNC_IS_STAT(ii) )
         printf("       %4s  %-11.11s  %s\n",
                FUNC_prefixstr[ii] , FUNC_typestr[ii]+6 , FUNC_label_stat_aux[ii] ) ;
   }

   printf("\n") ; exit(0) ;
}

static char * Usage_str[3] = { "-t2p = statistic-to-probability" ,
                               "-p2t = probability-to-statistic" ,
                               "-t2z = statistic-to-N(0,1) [z-score]" } ;


int main( int argc , char * argv[] )
{
   int usage = -1 , statcode = -1 , ii,fc,iarg , npar , verb=0 ;
   float stat , prob , val ;
   float par[MAX_STAT_AUX] ;
   char * cpt ;

   if( argc < 4 || strcmp(argv[1],"-help") == 0 ) Syntax() ;

   iarg = 1 ;

   if( strcmp(argv[iarg],"-v") == 0 ){ verb = 1 ; iarg++ ; }

        if( strcmp(argv[iarg],"-t2p")==0 || strcmp(argv[iarg],"t2p")==0 ) usage = 1 ;
   else if( strcmp(argv[iarg],"-p2t")==0 || strcmp(argv[iarg],"p2t")==0 ) usage = 2 ;
   else if( strcmp(argv[iarg],"-t2z")==0 || strcmp(argv[iarg],"t2z")==0 ) usage = 3 ;

   if( usage < 0 ){
      fprintf(stderr,"Don't recognize usage code: %s\n",argv[iarg]) ; exit(1) ;
   }
   if( verb ) printf("*** usage=%d: %s\n",usage,Usage_str[usage-1]) ;

   iarg++ ;

   fc = (argv[iarg][0] == '-') ? 1 : 0 ;
   for( ii=FIRST_FUNC_TYPE ; ii <= LAST_FUNC_TYPE ; ii++ ){
      if( ! FUNC_IS_STAT(ii) ) continue ;
      if( strcmp( &(argv[iarg][fc]) , FUNC_prefixstr[ii] ) == 0 ){
         statcode = ii ; break ;
      }
   }

   if( statcode < 0 ){
      fprintf(stderr,"Don't recognize statname: %s\n",argv[iarg]) ; exit(1) ;
   }
   if( verb ) printf("*** statcode=%d  type=%s\n",statcode,FUNC_typestr[ii]+6) ;
   iarg++ ;

   stat = strtod( argv[iarg] , &cpt ) ;
   if( usage == 2 ) prob = stat ;

   if( verb ) printf("*** value=%g\n",stat) ;

   if( *cpt != '\0' ){
      fprintf(stderr,"Illegal numeric parameter: %s\n",argv[iarg]) ; exit(1) ;
   }

   iarg++ ; ii = 0 ;
   while( iarg < argc && ii < MAX_STAT_AUX ){
      val = strtod( argv[iarg] , &cpt ) ;
      if( *cpt != '\0' ){
         fprintf(stderr,"Illegal numeric parameter: %s\n",argv[iarg]) ; exit(1) ;
      }
      par[ii++] = val ; iarg++ ;
   }
   npar = ii ;

   if( verb ){
      printf("*** npar=%d  parameters=",npar) ;
      for( ii=0 ; ii < npar ; ii++ ) printf("%g ",par[ii]) ;
      printf("\n") ;
   }

   if( npar < FUNC_need_stat_aux[statcode] ){
      fprintf(stderr,"Need %d parameters, but you only gave %d\n",
              FUNC_need_stat_aux[statcode] , npar ) ;
      exit(1) ;
   }

   switch( usage ){

      case 1:  prob = THD_stat_to_pval( stat , statcode , par ) ;
               printf("p = %g\n",prob) ;
      break ;

      case 2:  stat = THD_pval_to_stat( prob , statcode , par ) ;
               printf("t = %g\n",stat) ;
      break ;

      case 3:  val = THD_stat_to_zscore( stat , statcode , par ) ;
               printf("z = %g\n",val) ;
      break ;
   }

   exit(0) ;
}
