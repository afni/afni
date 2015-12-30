#include "niml/niml.h"

/*************************************************************************/
/*** Compile with
     gcc -O2 -o nimltest nimltest.c niml.c -I.                         ***/
/*************************************************************************/

int main( int argc , char *argv[] )
{
   NI_stream ns , nsout , nsf=NULL ;
   int nn , tt , nopt=1 , bmode , tflag=0 ;
   void *nini ;
   char *ccc ;

   if( argc < 2 ){
      printf("Usage: nimltest [-bB fname] [-w[r]] [-#] streamspec\n");exit(0);
   }

#if 0
   for( nn=0 ; nn < 5 ; nn++ ){
     ccc = UNIQ_idcode() ;
     fprintf(stderr,"%d: %s\n",nn,ccc) ; free(ccc) ;
   }
   ccc = UNIQ_hashcode("Elvis") ;
   fprintf(stderr,"   %s\n",ccc) ; free(ccc) ;
#endif

   /* writing to a stream? */

   if( strncmp(argv[1],"-w",2) == 0 ){
      char lbuf[1024] , *bbb ;
      int raw=(argv[1][2]=='r') ;
      if( argc < 3 ) exit(1) ;
      nopt = 2 ;
      ns = NI_stream_open( argv[nopt] , "w" ) ;
      if( ns == NULL ){
         fprintf(stderr,"NI_stream_open fails\n") ; exit(1) ;
      }
      while(1){
        nn = NI_stream_writecheck( ns , 400 ) ;
        if( nn == 1 ){ fprintf(stderr,"\n") ; break ; }
        if( nn <  0 ){ fprintf(stderr,"BAD writecheck\n"); exit(1) ; }
        fprintf(stderr,".") ;
      }
      if( !raw ){
        while(1){
          fprintf(stderr,"READY> ") ;
          bbb = fgets( lbuf , 1024 , stdin ) ; if( bbb == NULL ) exit(0) ;
          nn = NI_stream_write( ns , lbuf , strlen(lbuf) ) ;
          if( nn < 0 ){
             fprintf(stderr,"NI_stream_write fails\n"); exit(1);
          }
        }
      } else {
        int nbyt ;
        while(1){
          nbyt = fread( lbuf , 1,1024 , stdin ) ;
          if( nbyt <= 0 ){ NI_sleep(10) ; NI_stream_close(ns) ; exit(0) ; }
          nn = NI_stream_write( ns , lbuf , nbyt ) ;
          if( nn < 0 ){
             fprintf(stderr,"NI_stream_write fails\n"); exit(1);
          }
        }
      }
   }

   if( strcmp(argv[1],"-b") == 0 || strcmp(argv[1],"-B") == 0 ){
      char fname[256] ;
      nopt = 3 ;
      if( argc < 4 ){ fprintf(stderr,"Too few args\n"); exit(1); }

      sprintf(fname,"file:%s",argv[2]) ;
      nsf = NI_stream_open( fname, "w" ) ;
      if( nsf == NULL ) fprintf(stderr,"Can't open %s\n",fname) ;

      bmode = (strcmp(argv[1],"-b") == 0) ? NI_BINARY_MODE
                                          : NI_BASE64_MODE ;
   }

   if( strcmp(argv[nopt],"-#") == 0 ){
     tflag = NI_HEADERSHARP_FLAG ; nopt++ ;
   }

   /* reading! */

   NI_add_trusted_host(NULL) ;

   ns = NI_stream_open( argv[nopt] , "r" ) ;
   if( ns == NULL ){
      fprintf(stderr,"NI_stream_open fails\n") ; exit(1) ;
   }
   while(1){
      nn = NI_stream_goodcheck( ns , 400 ) ;
      if( nn == 1 ){ fprintf(stderr,"\n") ; break ; }
      if( nn <  0 ){ fprintf(stderr,"BAD goodcheck\n"); exit(1) ; }
      fprintf(stderr,".") ;
   }

GetElement:
   nini = NI_read_element( ns , -1 ) ;  /* wait forever */
   if( nini == NULL ){
      if( NI_stream_goodcheck(ns,0) < 0 ){
         fprintf(stderr,"NI_read_element fails\n") ; exit(1) ;
      }
      NI_sleep(999) ; goto GetElement ;
   }

   tt = NI_element_type( nini ) ;

   if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *) nini ;
      fprintf(stderr,"Data element:\n"
                     "  name       = %s\n"
                     "  vec_num    = %d\n"
                     "  vec_len    = %d\n"
                     "  vec_filled = %d\n"
                     "  vec_rank   = %d\n"
                     "  attr_num   = %d\n" ,
          nel->name,nel->vec_num,nel->vec_len,nel->vec_filled,
          nel->vec_rank,nel->attr_num );
       for( nn=0 ; nn < nel->attr_num ; nn++ )
          fprintf(stderr,"  %2d: lhs=%s  rhs=%s\n",
                  nn , nel->attr_lhs[nn] , nel->attr_rhs[nn] ) ;

#if 0
       for( nn=0 ; nn < nel->vec_rank ; nn++ ){
          fprintf(stderr,"  axis[%d]: len=%d delta=%f origin=%f unit=%s label=%s\n",
                  nn , nel->vec_axis_len[nn] ,
                  (nel->vec_axis_delta)  ? nel->vec_axis_delta[nn]  : -666.0 ,
                  (nel->vec_axis_origin) ? nel->vec_axis_origin[nn] : -666.0 ,
                  (nel->vec_axis_unit)   ? nel->vec_axis_unit[nn]   : "NULL" ,
                  (nel->vec_axis_label)  ? nel->vec_axis_label[nn]  : "NULL"  ) ;
       }
#endif
   } else if( tt == NI_GROUP_TYPE ){
      NI_group *ngr = (NI_group *) nini ;
      fprintf(stderr,"Group element:\n"
                     "  part_num = %d\n"
                     "  attr_num = %d\n" ,
              ngr->part_num , ngr->attr_num ) ;
       for( nn=0 ; nn < ngr->attr_num ; nn++ )
          fprintf(stderr,"  %2d: lhs=%s  rhs=%s\n",
                  nn , ngr->attr_lhs[nn] , ngr->attr_rhs[nn] ) ;

   } else if( tt == NI_PROCINS_TYPE ){
      NI_procins *npi = (NI_procins *)nini ;
      fprintf(stderr,"Processing instruction:\n"
                     "  target = %s\n" , npi->name ) ;
      for( nn=0 ; nn < npi->attr_num ; nn++ )
         fprintf(stderr,"  %2d: lhs=%s  rhs=%s\n",
                 nn , npi->attr_lhs[nn] , npi->attr_rhs[nn] ) ;
   }

#if 1
   nsout = NI_stream_open( "str:" , "w" ) ;
   if( nsout == NULL ){
      fprintf(stderr,"NI_stream_open fails for output\n"); exit(1);
   }

   nn = NI_write_element( nsout , nini , NI_TEXT_MODE | tflag ) ;

   fprintf(stderr,"\n------ NI_write_element = %d ------\n%s\n==========================\n" ,
           nn, NI_stream_getbuf(nsout) ) ;
   NI_stream_close(nsout) ;
#endif

   if( nsf != NULL ){
      nn = NI_write_element( nsf , nini , bmode ) ;
      fprintf(stderr,"NI_write_element to file = %d\n",nn) ;
   }

   goto GetElement ;
}
