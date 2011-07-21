#include "mrilib.h"

#undef  NLL
#define NLL 32766  /* lbuf length below -- should be enuf */

#undef  LVEC
#define LVEC 666

/*** Modified 20-21 Jul 2011 to all column selectors []
     for the table.  Column #0 must be the first one!
     Row selectors {} don't really make sense, given that
     the actual rows used are supposed to be taken using
     column #0 as a key, and unselected rows are to be ignored. ***/

/*------------------------------------------------------------------------*/
/* Simple table: first column = labels, the rest are numbers. */

NI_element * THD_simple_table_read( char *fname )
{
   NI_str_array *sar ;
   char *dname , *cpt , *dpt , *qpt , lbuf[NLL] ;
   int ii,jj , ibot , nlab , row=0 , *ivlist=NULL , niv=0 ;
   NI_element *nel ;
   FILE *fts ;
   float val ;

ENTRY("THD_simple_table_read") ;

   /*--  check for bad-ositiness --*/

   if( fname == NULL || *fname == '\0' )                 RETURN(NULL) ;
   ii = strlen(fname) ;
   if( (ii <= 2 && fname[0] == '-')                  ||
       (ii <= 6 && strncmp(fname,"stdin"   ,5) == 0) ||
       (ii <= 9 && strncmp(fname,"/dev/fd0",8) == 0)   ) RETURN(NULL) ;
   if( strncmp(fname,"1D:",3) == 0 )                     RETURN(NULL) ;

   /* copy filename to local variable for editing purposes */

   dname = strdup(fname) ; if( dname[ii-1] == '\'' ) dname[ii-1] = '\0' ;

   /*-- split filename and subvector list --*/

   cpt = strstr(fname,"[") ;  /* column list */
   dpt = strstr(fname,"{") ;  /* we don't use this row list */

   if( cpt == fname || dpt == fname ){  /* can't be at start of filename! */
     free(dname) ; RETURN(NULL) ;
   } else {                             /* got a subvector list */
     if( cpt != NULL ){ ii = cpt-fname; dname[ii] = '\0'; }
     if( dpt != NULL ){ ii = dpt-fname; dname[ii] = '\0'; }
   }

   /* open input file */

   fts = fopen(dname,"r") ; if( fts == NULL ){ free(dname); RETURN(NULL); }

   /* read lines until we get a useful line */

   while(1){
     lbuf[0] = '\0' ;
     dpt = fgets( lbuf , NLL , fts ) ;                 /* read a line of data */
     if( dpt == NULL ){ fclose(fts); free(dname); RETURN(NULL); }    /* error */
     ii = strlen(lbuf) ; if( ii == 0 ) continue ;         /* nada => loopback */
     if( ii == 1 && isspace(lbuf[0]) ) continue ; /* 1 blank only => loopback */
     ibot = (lbuf[0] == '#') ? 1 : 0 ;                   /* start of scanning */
     for( ii=ibot ; isspace(lbuf[ii]) ; ii++ ) continue ;      /* skip blanks */
     if( lbuf[ii] == '\0' ) continue ;              /* all blanks => loopback */
     ibot = ii ; break ;                             /* can process this line */
   }

   /* break line into sub-strings */

   sar = NI_decode_string_list( lbuf+ibot , ";" ) ;
   if( sar == NULL ){ fclose(fts); free(dname); RETURN(NULL); }    /* nuthin? */

   nlab = sar->num ;
   if( nlab <= 1 ){
     if( nlab == 1 )                      /* need to have at least 2 columns! */
       ERROR_message("Short table line (missing label or data?) -- %s",dname) ;
     else
       ERROR_message("No valid table line found in file %s",dname) ;
     fclose(fts) ; NI_delete_str_array(sar) ; free(dname) ; RETURN(NULL) ;
   }

   /* 20 Jul 2011 -- get column list, if present */

   if( cpt != NULL ){
     ivlist = MCW_get_intlist( nlab , cpt ) ;
     if( ivlist != NULL ){
       if( ivlist[0] <= 1 || ivlist[1] != 0 ){  /* must have col #0 first */
         free(ivlist) ; ivlist=NULL ;
         WARNING_message("Ignoring selector '%s' for table file '%s'",cpt,dname) ;
       } else {
         niv = ivlist[0] ;  /* number of columns */
       }
     }
   }

   /* setup output data structure */

   nel = NI_new_data_element( "AFNI_table" , LVEC ) ;

   NI_add_column( nel , NI_STRING , NULL ) ;
   if( niv <= 1 )
     for( ii=1 ; ii < nlab ; ii++ ) NI_add_column( nel , NI_FLOAT , NULL ) ;
   else
     for( jj=2 ; jj <= niv ; jj++ ) NI_add_column( nel , NI_FLOAT , NULL ) ;

   strcpy( lbuf , sar->str[0] ) ;
   if( niv <= 1 ){
     for( ii=1 ; ii < nlab ; ii++ ){
       strcat( lbuf , ";" ) ; strcat( lbuf , sar->str[ii] ) ;
     }
   } else {
     for( jj=2 ; jj <= niv ; jj++ ){
       ii = ivlist[jj] ;
       strcat( lbuf , ";" ) ; strcat( lbuf , sar->str[ii] ) ;
     }
   }
   NI_set_attribute( nel , "Labels" , lbuf ) ;
   NI_delete_str_array(sar) ;

   /* now read lines and process them */

   while(1){

     /* scan ahead for next good input line */

     while(1){
       lbuf[0] = '\0' ; ibot = -1 ;
       dpt = fgets( lbuf , NLL , fts ) ;               /* read a line of data */
       if( dpt == NULL ) break ;                                     /* error */
       ii = strlen(lbuf) ; if( ii <= 2*nlab ) continue ;  /* nada => loopback */
       if( lbuf[0] == '#' ) continue ;                 /* comment => loopback */
       ibot = (lbuf[0] == '#') ? 1 : 0 ;                 /* start of scanning */
       for( ii=ibot ; isspace(lbuf[ii]) ; ii++ ) continue ;    /* skip blanks */
       if( lbuf[ii] == '\0' ) continue ;            /* all blanks => loopback */
       ibot = ii ; break ;                           /* can process this line */
     } /* loop to get next line */

     if( ibot < 0 ) break ;           /* end of input ==> done with this file */

     sar = NI_decode_string_list( lbuf+ibot , ";" ) ;
     if( sar == NULL ) continue ;                      /* nuthin ==> loopback */

     if( row >= nel->vec_len )
       NI_alter_veclen( nel , nel->vec_len + LVEC ) ; /* need more data space */

     /* put values from this line into this row of the table */

     NI_insert_string( nel , row , 0 , sar->str[0] ) ;           /* column #0 */
     if( niv <= 1 ){                                        /* do all columns */
       for( ii=1 ; ii < nlab ; ii++ ){
         if( ii < sar->num ){
           val = (float)strtod( sar->str[ii] , &qpt ) ;
           if( *qpt != '\0' )
             WARNING_message("value '%s' in table file '%s' row #%d col #%d is non-float",
                              sar->str[ii] , dname , row+1 , ii ) ;
         } else {
           val = 0.0f ;
           WARNING_message("table file '%s' row #%d col #%d : no value present :-(",
                           dname , row+1 , ii ) ;
         }
         NI_insert_value( nel , row , ii , &val ) ;
       }
     } else {                                            /* just some columns */
       for( jj=2 ; jj <= niv ; jj++ ){
         ii = ivlist[jj] ;
         if( ii < sar->num ){
           val = (float)strtod( sar->str[ii] , &qpt ) ;
           if( *qpt != '\0' )
             WARNING_message("value '%s' in table file '%s' row #%d col #%d is non-float",
                              sar->str[ii] , dname , row+1 , ii ) ;
         } else {
           val = 0.0f ;
           WARNING_message("table file '%s' row #%d col #%d : no value present :-(",
                           dname , row+1 , ii ) ;
         }
         NI_insert_value( nel , row , jj-1 , &val ) ;
       }
     }

     row++ ;  /* have one more row! */
     NI_delete_str_array(sar) ;

   } /* loop to get next row */

   /* cleanup and exit */

   fclose(fts) ;
   if( row == 0 ){ NI_free_element(nel); free(dname); RETURN(NULL); }
   if( ivlist != NULL ) free(ivlist) ;

   NI_alter_veclen(nel,row) ;

   /* check for duplicate first column labels */

   for( ii=0 ; ii < nel->vec_len ; ii++ ){
     cpt = ((char **)(nel->vec[0]))[ii] ;
     for( jj=ii+1 ; jj < nel->vec_len ; jj++ ){
       dpt = ((char **)(nel->vec[0]))[jj] ;
       if( strcmp(cpt,dpt) == 0 )
         WARNING_message("Table file '%s': rows %d & %d have same label %s",
                         dname , ii+1,jj+1,cpt) ;
     }
   }

   if( AFNI_yesenv("AFNI_DEBUG_TABLE") )
     NI_write_element_tofile( "stdout:" , nel , NI_TEXT_MODE ) ;

   free(dname) ; RETURN(nel) ;
}

/*------------------------------------------------------------------------*/
/* This table has column #0 as strings (labels), and remaining
   columns are numbers or strings.  Each column is 'pure' in type.
*//*----------------------------------------------------------------------*/

NI_element * THD_mixed_table_read( char *fname )
{
   NI_str_array *sar ;
   char *dname , *cpt , *dpt , *qpt , lbuf[NLL] ;
   int ii,jj , ibot , nlab , row=0 , *ivlist=NULL , niv=0 ;
   NI_element *nel ;
   FILE *fts ;
   float val ;

ENTRY("THD_mixed_table_read") ;

   /*--  check for bad-ositiness --*/

   if( fname == NULL || *fname == '\0' )                 RETURN(NULL) ;
   ii = strlen(fname) ;
   if( (ii <= 2 && fname[0] == '-')                  ||
       (ii <= 6 && strncmp(fname,"stdin"   ,5) == 0) ||
       (ii <= 9 && strncmp(fname,"/dev/fd0",8) == 0)   ) RETURN(NULL) ;
   if( strncmp(fname,"1D:",3) == 0 )                     RETURN(NULL) ;

   /* copy filename to local variable for editing purposes */

   dname = strdup(fname) ; if( dname[ii-1] == '\'' ) dname[ii-1] = '\0' ;

   /*-- split filename and subvector list --*/

   cpt = strstr(fname,"[") ;  /* column list */
   dpt = strstr(fname,"{") ;  /* we don't use this row list */

   if( cpt == fname || dpt == fname ){  /* can't be at start of filename! */
     free(dname) ; RETURN(NULL) ;
   } else {                             /* got a subvector list */
     if( cpt != NULL ){ ii = cpt-fname; dname[ii] = '\0'; }
     if( dpt != NULL ){ ii = dpt-fname; dname[ii] = '\0'; }
   }

   /* open input file */

   fts = fopen(dname,"r") ; if( fts == NULL ){ free(dname); RETURN(NULL); }

   /* read lines until we get a useful line */

   while(1){
     lbuf[0] = '\0' ;
     dpt = fgets( lbuf , NLL , fts ) ;                 /* read a line of data */
     if( dpt == NULL ){ fclose(fts); free(dname); RETURN(NULL); }    /* error */
     ii = strlen(lbuf) ; if( ii == 0 ) continue ;         /* nada => loopback */
     if( ii == 1 && isspace(lbuf[0]) ) continue ; /* 1 blank only => loopback */
     ibot = (lbuf[0] == '#') ? 1 : 0 ;                   /* start of scanning */
     for( ii=ibot ; isspace(lbuf[ii]) ; ii++ ) continue ;      /* skip blanks */
     if( lbuf[ii] == '\0' ) continue ;              /* all blanks => loopback */
     ibot = ii ; break ;                             /* can process this line */
   }

   /* break line into sub-strings */

   sar = NI_decode_string_list( lbuf+ibot , ";" ) ;
   if( sar == NULL ){ fclose(fts); free(dname); RETURN(NULL); }    /* nuthin? */

   nlab = sar->num ;         /* number of labels = number of separate strings */
   if( nlab <= 1 ){
     if( nlab == 1 )                      /* need to have at least 2 columns! */
       ERROR_message("short table line (missing label or data?) -- %s",dname) ;
     else
       ERROR_message("no valid table line found in file %s",dname) ;
     fclose(fts) ; NI_delete_str_array(sar) ; free(dname) ; RETURN(NULL) ;
   }

   /* 21 Jul 2011 -- get column list, if present */

   if( cpt != NULL ){
     ivlist = MCW_get_intlist( nlab , cpt ) ;
     if( ivlist != NULL ){
       if( ivlist[0] <= 1 || ivlist[1] != 0 ){
         free(ivlist) ; ivlist=NULL ;
         WARNING_message("Ignoring selector '%s' for table file '%s'",cpt,dname) ;
       } else {
         niv = ivlist[0] ;
       }
     }
   }

   /* setup output data structure */

   nel = NI_new_data_element( "AFNI_table" , LVEC ) ;

   NI_add_column( nel , NI_STRING , NULL ) ;

   strcpy( lbuf , sar->str[0] ) ;
   if( niv <= 1 ){
     for( ii=1 ; ii < nlab ; ii++ ){
       strcat( lbuf , ";" ) ; strcat( lbuf , sar->str[ii] ) ;
     }
   } else {
     for( jj=2 ; jj <= niv ; jj++ ){
       ii = ivlist[jj] ;
       strcat( lbuf , ";" ) ; strcat( lbuf , sar->str[ii] ) ;
     }
   }
   NI_set_attribute( nel , "Labels" , lbuf ) ;
   NI_delete_str_array(sar) ;

   /* now read following lines and process them */

   while(1){

     /* scan ahead for next good input line */

     while(1){
       lbuf[0] = '\0' ; ibot = -1 ;
       dpt = fgets( lbuf , NLL , fts ) ;               /* read a line of data */
       if( dpt == NULL ) break ;                                     /* error */
       ii = strlen(lbuf) ; if( ii <= 2*nlab ) continue ;  /* nada => loopback */
       if( lbuf[0] == '#' ) continue ;                 /* comment => loopback */
       ibot = (lbuf[0] == '#') ? 1 : 0 ;                 /* start of scanning */
       for( ii=ibot ; isspace(lbuf[ii]) ; ii++ ) continue ;    /* skip blanks */
       if( lbuf[ii] == '\0' ) continue ;            /* all blanks => loopback */
       ibot = ii ; break ;                           /* can process this line */
     } /* loop to get next line */

     if( ibot < 0 ) break ;           /* end of input ==> done with this file */

     sar = NI_decode_string_list( lbuf+ibot , ";" ) ;
     if( sar == NULL ) continue ;                      /* nuthin ==> loopback */

     if( row == 0 ){   /* first row ==> figure out format */
       if( sar->num < nlab ){
         ERROR_message("First row of values in '%s' is too short!",dname) ;
         NI_delete_str_array(sar) ; NI_free_element(nel) ; fclose(fts) ;
         free(dname) ; RETURN(NULL) ;
       }
       if( niv <= 1 ){
         for( ii=1 ; ii < nlab ; ii++ ){
           val = (float)strtod( sar->str[ii] , &qpt ) ;
           if( *qpt == '\0' ) NI_add_column( nel , NI_FLOAT  , NULL ) ;
           else               NI_add_column( nel , NI_STRING , NULL ) ;
         }
       } else {
         for( jj=2 ; jj <= niv ; jj++ ){
           ii = ivlist[jj] ;
           val = (float)strtod( sar->str[ii] , &qpt ) ;
           if( *qpt == '\0' ) NI_add_column( nel , NI_FLOAT  , NULL ) ;
           else               NI_add_column( nel , NI_STRING , NULL ) ;
         }
       }
     }

     if( row >= nel->vec_len )
       NI_alter_veclen( nel , nel->vec_len + LVEC ) ; /* need more data space */

     /* put values from this line into this row of the table */

     NI_insert_string( nel , row , 0 , sar->str[0] ) ;
     if( niv <= 1 ){
       for( ii=1 ; ii < nlab ; ii++ ){
         if( nel->vec_typ[ii] == NI_FLOAT ){
           if( ii < sar->num ){
             val = (float)strtod( sar->str[ii] , &qpt ) ;
             if( *qpt != '\0' )
               WARNING_message("value '%s' in table file '%s' row #%d col #%d is non-float",
                                sar->str[ii] , dname , row+1 , ii ) ;
           } else{
             val = 0.0f ;
             WARNING_message("table file '%s' row #%d col #%d : no value present :-(",
                             dname , row+1 , ii ) ;
           }
           NI_insert_value( nel , row , ii , &val ) ;
         } else {
           if( ii < sar->num ) dpt = sar->str[ii] ;
           else                dpt = "N/A" ;
           NI_insert_string( nel , row , ii , dpt ) ;
         }
       }
     } else {
       for( jj=2 ; jj <= niv ; jj++ ){
         ii = ivlist[jj] ;
         if( nel->vec_typ[ii] == NI_FLOAT ){
           if( ii < sar->num ){
             val = (float)strtod( sar->str[ii] , &qpt ) ;
             if( *qpt != '\0' )
               WARNING_message("value '%s' in table file '%s' row #%d col #%d is non-float",
                                sar->str[ii] , dname , row+1 , ii ) ;
           } else{
             val = 0.0f ;
             WARNING_message("table file '%s' row #%d col #%d : no value present :-(",
                             dname , row+1 , ii ) ;
           }
           NI_insert_value( nel , row , jj-1 , &val ) ;
         } else {
           if( ii < sar->num ) dpt = sar->str[ii] ;
           else                dpt = "N/A" ;
           NI_insert_string( nel , row , jj-1 , dpt ) ;
         }
       }
     }

     row++ ;  /* have one more row! */
     NI_delete_str_array(sar) ;

   } /* loop to get next row */

   /* cleanup and exit */

   fclose(fts) ;
   if( ivlist != NULL ) free(ivlist) ;
   if( row == 0 ){ NI_free_element(nel); free(dname); RETURN(NULL); }

   NI_alter_veclen(nel,row) ;

   /* check for duplicate first column labels */

   for( ii=0 ; ii < nel->vec_len ; ii++ ){
     cpt = ((char **)(nel->vec[0]))[ii] ;
     for( jj=ii+1 ; jj < nel->vec_len ; jj++ ){
       dpt = ((char **)(nel->vec[0]))[jj] ;
       if( strcmp(cpt,dpt) == 0 )
         WARNING_message("Table file '%s': rows %d & %d have same label %s",
                         dname,ii+1,jj+1,cpt) ;
     }
   }

   if( AFNI_yesenv("AFNI_DEBUG_TABLE") )
     NI_write_element_tofile( "stdout:" , nel , NI_TEXT_MODE ) ;

   free(dname) ; RETURN(nel) ;
}
