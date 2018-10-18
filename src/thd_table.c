#include "mrilib.h"

#undef  NLL
#define NLL 32766  /* lbuf length below -- should be enuf */

#undef  LVEC
#define LVEC 666

#undef  ERREX
#define ERREX(str) do{ ERROR_message(str); RETURN(NULL); } while(0)

#undef  TRBUF
#define TRBUF(bb) \
  do{ int lb=strlen(bb) ; if( bb[lb-1] == '\n' ) bb[lb-1] = '\0' ; } while(0)

/*** Modified 20-21 Jul 2011 to allow column selectors []
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
   int verb = AFNI_yesenv("AFNI_DEBUG_TABLE") ;

ENTRY("THD_simple_table_read") ;

   /*--  check for bad-ositiness --*/

   if( fname == NULL || *fname == '\0' )                 ERREX("Table: bad filename") ;
   ii = strlen(fname) ;
   if( (ii <= 2 && fname[0] == '-')                  ||
       (ii <= 6 && strncmp(fname,"stdin"   ,5) == 0) ||
       (ii <= 9 && strncmp(fname,"/dev/fd0",8) == 0)   ) ERREX("Table: stdin not allowed") ;
   if( strncmp(fname,"1D:",3) == 0 )                     ERREX("Table: 1D: not allowed") ;

   /* copy filename to local variable for editing purposes */

   dname = strdup(fname) ; if( dname[ii-1] == '\'' ) dname[ii-1] = '\0' ;

   /*-- split filename and subvector list --*/

   cpt = strstr(fname,"[") ;  /* column list */
   dpt = strstr(fname,"{") ;  /* we don't use this row list */

   if( cpt == fname || dpt == fname ){  /* can't be at start of filename! */
     free(dname) ; ERREX("Table: '[ or {' selectors at start of filename") ;
   } else {                             /* got a subvector list */
     if( cpt != NULL ){ ii = cpt-fname; dname[ii] = '\0'; }
     if( dpt != NULL ){ ii = dpt-fname; dname[ii] = '\0'; }
   }

   /* open input file */

   if( verb ) INFO_message("Simple Table: processing file %s",dname) ;

   fts = fopen(dname,"r") ; if( fts == NULL ){ free(dname); ERREX("Table: can't open file"); }

   /* read lines until we get a useful line */

   while(1){
     lbuf[0] = '\0' ;
     dpt = afni_fgets( lbuf , NLL , fts ) ;            /* read a line of data */
     if( dpt == NULL ){ fclose(fts); free(dname); ERREX("Table: can't read first line"); }
     ii = strlen(lbuf) ; if( ii == 0 ) continue ;         /* nada => loopback */
     if( ii == 1 && isspace(lbuf[0]) ) continue ; /* 1 blank only => loopback */
     ibot = (lbuf[0] == '#') ? 1 : 0 ;                   /* start of scanning */
     for( ii=ibot ; isspace(lbuf[ii]) ; ii++ ) continue ;      /* skip blanks */
     if( lbuf[ii] == '\0' ) continue ;              /* all blanks => loopback */
     ibot = ii ; break ;                             /* can process this line */
   }

   TRBUF(lbuf) ;
   if( verb ) ININFO_message("  first line = '%s'",lbuf+ibot) ;

   /* break line into sub-strings */

   sar = NI_decode_string_list( lbuf+ibot , ";" ) ;
   if( sar == NULL ){ fclose(fts); free(dname); ERREX("Table: can't decode first line"); }    /* nuthin? */

   nlab = sar->num ;
   if( verb ) ININFO_message("  found %d labels in first line",nlab) ;
   if( nlab <= 1 ){
     if( nlab == 1 )                      /* need to have at least 2 columns! */
       ERROR_message("Short table line (missing label or data?) -- %s",dname) ;
     else
       ERROR_message("No valid table line found in file %s",dname) ;
     fclose(fts) ; NI_delete_str_array(sar) ; free(dname) ; RETURN(NULL) ;
   }

   /* 20 Jul 2011 -- get column list, if present */

   if( cpt != NULL ){
     if( verb ) ININFO_message("  processing column selector '%s'",cpt) ;
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
       dpt = afni_fgets( lbuf , NLL , fts ) ;          /* read a line of data */
       if( dpt == NULL ) break ;                                     /* error */
       ii = strlen(lbuf) ; if( ii <= 2*nlab ) continue ;  /* nada => loopback */
       if( lbuf[0] == '#' ) continue ;                 /* comment => loopback */
       ibot = (lbuf[0] == '#') ? 1 : 0 ;                 /* start of scanning */
       for( ii=ibot ; isspace(lbuf[ii]) ; ii++ ) continue ;    /* skip blanks */
       if( lbuf[ii] == '\0' ) continue ;            /* all blanks => loopback */
       ibot = ii ; break ;                           /* can process this line */
     } /* loop to get next line */

     if( ibot < 0 ) break ;           /* end of input ==> done with this file */

     TRBUF(lbuf) ;
     if( verb ) ININFO_message("  processing row #%d = '%s'",row,lbuf+ibot) ;

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
   if( row == 0 ){ NI_free_element(nel); free(dname); ERREX("Table: no data lines found"); }
   if( ivlist != NULL ) free(ivlist) ;

   NI_alter_veclen(nel,row) ;

   /* check for duplicate first column labels */

   if( verb ) ININFO_message("checking for duplicate labels") ;

   for( ii=0 ; ii < nel->vec_len ; ii++ ){
     cpt = ((char **)(nel->vec[0]))[ii] ;
     for( jj=ii+1 ; jj < nel->vec_len ; jj++ ){
       dpt = ((char **)(nel->vec[0]))[jj] ;
       if( strcmp(cpt,dpt) == 0 )
         WARNING_message("Table file '%s': rows %d & %d have same label %s",
                         dname , ii+1,jj+1,cpt) ;
     }
   }

   if( verb ){
     ININFO_message("Table element follows::") ;
     NI_write_element_tofile( "stdout:" , nel , NI_TEXT_MODE ) ;
   }

   free(dname) ; RETURN(nel) ;
}

/*------------------------------------------------------------------------*/

void string_ectomy( char *src , char *bad )  /* 20 Jun 2012 */
{
   int nsrc , nbad , is , io , ib ;
   char *out , ccc ;

   if( src == NULL || bad == NULL || *src == '\0' || *bad == '\0' ) return ;

   nsrc = strlen(src) ; out = calloc(sizeof(char),(nsrc+1)) ;
   nbad = strlen(bad) ;

   for( io=is=0 ; is < nsrc ; is++ ){
     ccc = src[is] ;
     for( ib=0 ; ib < nbad && bad[ib] != ccc ; ib++ ) ; /*nada*/
     if( ib == nbad ) out[io++] = ccc ;
   }

   if( io < nsrc ){
     ININFO_message("Table reading: replaced string %s with %s",src,out) ;
     strcpy(src,out) ;
   }

   free(out) ; return ;
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
   int verb = AFNI_yesenv("AFNI_DEBUG_TABLE") ;

ENTRY("THD_mixed_table_read") ;

   /*--  check for bad-ositiness --*/

   if( fname == NULL || *fname == '\0' )                 ERREX("Table: bad filename") ;
   ii = strlen(fname) ;
   if( (ii <= 2 && fname[0] == '-')                  ||
       (ii <= 6 && strncmp(fname,"stdin"   ,5) == 0) ||
       (ii <= 9 && strncmp(fname,"/dev/fd0",8) == 0)   ) ERREX("Table: stdin not allowed") ;
   if( strncmp(fname,"1D:",3) == 0 )                     ERREX("Table: 1D: not allowed") ;

   /* copy filename to local variable for editing purposes */

   dname = strdup(fname) ; if( dname[ii-1] == '\'' ) dname[ii-1] = '\0' ;

   /*-- split filename and subvector list --*/

   cpt = strstr(fname,"[") ;  /* column list */
   dpt = strstr(fname,"{") ;  /* we don't use this row list */

   if( cpt == fname || dpt == fname ){  /* can't be at start of filename! */
     free(dname) ; ERREX("Table: '[{' selector at start of filename") ;
   } else {                             /* got a subvector list */
     if( cpt != NULL ){ ii = cpt-fname; dname[ii] = '\0'; }
     if( dpt != NULL ){ ii = dpt-fname; dname[ii] = '\0'; }
   }

   /* open input file */

   if( verb ) INFO_message("Mixed Table: processing file %s",dname) ;

   fts = fopen(dname,"r") ; if( fts == NULL ){ free(dname); ERREX("Table: can't open file"); }

   /* read lines until we get a useful line */

   while(1){
     lbuf[0] = '\0' ;
     dpt = afni_fgets( lbuf , NLL , fts ) ;            /* read a line of data */
     if( dpt == NULL ){ fclose(fts); free(dname); ERREX("Table: Can't read first line"); }
     ii = strlen(lbuf) ; if( ii == 0 ) continue ;         /* nada => loopback */
     if( ii == 1 && isspace(lbuf[0]) ) continue ; /* 1 blank only => loopback */
     ibot = (lbuf[0] == '#') ? 1 : 0 ;                   /* start of scanning */
     for( ii=ibot ; isspace(lbuf[ii]) ; ii++ ) continue ;      /* skip blanks */
     if( lbuf[ii] == '\0' ) continue ;              /* all blanks => loopback */
     ibot = ii ; break ;                             /* can process this line */
   }

   TRBUF(lbuf) ;
   if( verb ) ININFO_message("  first line = '%s'",lbuf+ibot) ;

   /* break line into sub-strings */

   sar = NI_decode_string_list( lbuf+ibot , ";" ) ;
   if( sar == NULL ){ fclose(fts); free(dname); ERREX("Table: Can't decode first line"); }    /* nuthin? */

   nlab = sar->num ;         /* number of labels = number of separate strings */
   if( verb ) ININFO_message("  found %d labels in first line",nlab) ;
   if( nlab <= 1 ){
     if( nlab == 1 )                      /* need to have at least 2 columns! */
       ERROR_message("short table line (missing label or data?) -- %s",dname) ;
     else
       ERROR_message("no valid table line found in file %s",dname) ;
     fclose(fts) ; NI_delete_str_array(sar) ; free(dname) ; RETURN(NULL) ;
   }

   /* 21 Jul 2011 -- get column list, if present */

   if( cpt != NULL ){
     if( verb ) ININFO_message("  processing column selector '%s'",cpt) ;
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
       dpt = afni_fgets( lbuf , NLL , fts ) ;          /* read a line of data */
       if( dpt == NULL ) break ;                                     /* error */
       ii = strlen(lbuf) ; if( ii <= 2*nlab ) continue ;  /* nada => loopback */
       if( lbuf[0] == '#' ) continue ;                 /* comment => loopback */
       ibot = (lbuf[0] == '#') ? 1 : 0 ;                 /* start of scanning */
       for( ii=ibot ; isspace(lbuf[ii]) ; ii++ ) continue ;    /* skip blanks */
       if( lbuf[ii] == '\0' ) continue ;            /* all blanks => loopback */
       ibot = ii ; break ;                           /* can process this line */
     } /* loop to get next line */

     if( ibot < 0 ) break ;           /* end of input ==> done with this file */

     TRBUF(lbuf) ;
     if( verb ) ININFO_message("  processing row #%d = '%s'",row,lbuf+ibot) ;

     sar = NI_decode_string_list( lbuf+ibot , ";" ) ;
     if( sar == NULL ) continue ;                      /* nuthin ==> loopback */

     if( row == 0 ){   /* first row ==> figure out format */
       if( sar->num < nlab ){
         ERROR_message("First row of values in '%s' is too short!",dname) ;
         NI_delete_str_array(sar) ; NI_free_element(nel) ; fclose(fts) ;
         free(dname) ; RETURN(NULL) ;
       }
       if( niv <= 1 ){
         if( verb ) ININFO_message("  deciding format of %d columns",nlab-1) ;
         for( ii=1 ; ii < nlab ; ii++ ){
           val = (float)strtod( sar->str[ii] , &qpt ) ;
           if( *qpt == '\0' ){
             NI_add_column( nel , NI_FLOAT  , NULL ) ;
             if( verb ) ININFO_message("  -- col#%d is numeric",ii) ;
           } else {
             NI_add_column( nel , NI_STRING , NULL ) ;
             if( verb ) ININFO_message("  -- col#%d is string",ii) ;
           }
         }
       } else {
         if( verb ) ININFO_message("  deciding format of %d chosen columns",niv-1) ;
         for( jj=2 ; jj <= niv ; jj++ ){
           ii = ivlist[jj] ;
           val = (float)strtod( sar->str[ii] , &qpt ) ;
           if( *qpt == '\0' ){
             NI_add_column( nel , NI_FLOAT  , NULL ) ;
             if( verb ) ININFO_message("  -- col#%d is numeric",ii) ;
           } else {
             NI_add_column( nel , NI_STRING , NULL ) ;
             if( verb ) ININFO_message("  -- col#%d is string",ii) ;
           }
         }
       }
     } /* end of decoding format */

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
           string_ectomy( dpt , "\"'" ) ;
           NI_insert_string( nel , row , ii , dpt ) ;
         }
       }
     } else {
       for( jj=2 ; jj <= niv ; jj++ ){
         ii = ivlist[jj] ;
         /* vec_typ[ii] here would likely mean that K final columns would be
            lost in the case of K text columns (after label)
            --> issue noted by Phoebe from Harvard       26 Jul 2012 [rickr] */
         if( nel->vec_typ[jj-1] == NI_FLOAT ){
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
           string_ectomy( dpt , "\"'" ) ;
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
   if( row == 0 ){ NI_free_element(nel); free(dname); ERREX("Table: no data in file"); }

   NI_alter_veclen(nel,row) ;

   /* check for duplicate first column labels */

   if( verb ) ININFO_message("checking for duplicate labels") ;

   for( ii=0 ; ii < nel->vec_len ; ii++ ){
     cpt = ((char **)(nel->vec[0]))[ii] ;
     for( jj=ii+1 ; jj < nel->vec_len ; jj++ ){
       dpt = ((char **)(nel->vec[0]))[jj] ;
       if( strcmp(cpt,dpt) == 0 )
         WARNING_message("Table file '%s': rows %d & %d have same label %s",
                         dname,ii+1,jj+1,cpt) ;
     }
   }

   if( verb ){
     ININFO_message("Table element follows::") ;
     NI_write_element_tofile( "stdout:" , nel , NI_TEXT_MODE ) ;
   }

   free(dname) ; RETURN(nel) ;
}

/*------------------------------------------------------------------------*/
/* This table has only strings, and no header labels.
   Bit flags:
     1 = read as tsv (tab separated values)
     2 = skip column selectors
*//*----------------------------------------------------------------------*/

NI_element * THD_string_table_read( char *fname , int flags )
{
   NI_str_array *sar ;
   char *dname , *cpt , *dpt , *qpt , lbuf[NLL] ;
   int ii,jj , ibot , ncol=0 , row=0 , *ivlist=NULL , niv=0 ;
   NI_element *nel ;
   FILE *fts ;
   float val ;
   int verb = AFNI_yesenv("AFNI_DEBUG_TABLE") ;
   int do_tsv   = (flags & 1) != 0 ;
   int skip_sel = (flags & 2) != 0 ;

ENTRY("THD_string_table_read") ;

   /*--  check for bad-ositiness --*/

   if( fname == NULL || *fname == '\0' )                 ERREX("Table: bad filename") ;
   ii = strlen(fname) ;
   if( (ii <= 2 && fname[0] == '-')                  ||
       (ii <= 6 && strncmp(fname,"stdin"   ,5) == 0) ||
       (ii <= 9 && strncmp(fname,"/dev/fd0",8) == 0)   ) ERREX("Table: stdin not allowed") ;
   if( strncmp(fname,"1D:",3) == 0 )                     ERREX("Table: 1D: not allowed") ;

   /* copy filename to local variable for editing purposes */

   dname = strdup(fname) ; if( dname[ii-1] == '\'' ) dname[ii-1] = '\0' ;

   /*-- split filename and subvector list --*/

   cpt = strstr(fname,"[") ;  /* column list */
   dpt = strstr(fname,"{") ;  /* we don't use this row list */

   if( cpt == fname || dpt == fname ){  /* can't be at start of filename! */
     free(dname) ; ERREX("Table: '[{' selector at start of filename") ;
   } else {                             /* got a subvector list */
     if( cpt != NULL ){ ii = cpt-fname; dname[ii] = '\0'; }
     if( dpt != NULL ){ ii = dpt-fname; dname[ii] = '\0'; }
   }

   /* open input file */

   if( verb ) INFO_message("String Table: processing file %s",dname) ;

   fts = fopen(dname,"r") ; if( fts == NULL ){ free(dname); ERREX("Table: can't open file"); }

   /* 21 Jul 2011 -- get column list, if present */

   if( cpt != NULL && !skip_sel ){
     if( verb ) ININFO_message("  processing column selector '%s'",cpt) ;
     ivlist = MCW_get_intlist( 6666 , cpt ) ;
     if( ivlist != NULL ){
       if( ivlist[0] < 1 ){
         free(ivlist) ; ivlist=NULL ;
         WARNING_message("Ignoring selector '%s' for table file '%s'",cpt,dname) ;
       } else {
         niv = ivlist[0] ;
       }
     }
   }

   /* setup output data structure */

   nel = NI_new_data_element( "AFNI_table" , LVEC ) ;

   /* now read lines and process them */

   while(1){

     /* scan ahead for next good input line */

     while(1){
       lbuf[0] = '\0' ; ibot = -1 ;
       dpt = afni_fgets( lbuf , NLL , fts ) ;          /* read a line of data */
       if( dpt == NULL ) break ;                                     /* error */
       ii = strlen(lbuf) ; if( ii == 0 ) continue ;       /* nada => loopback */
       if( lbuf[0] == '#' ) continue ;                 /* comment => loopback */
       ibot = 0 ;                                        /* start of scanning */
       for( ii=ibot ; isspace(lbuf[ii]) ; ii++ ) continue ;    /* skip blanks */
       if( lbuf[ii] == '\0' ) continue ;            /* all blanks => loopback */
       ibot = ii ; break ;                           /* can process this line */
     } /* loop to get next line */

     if( ibot < 0 ) break ;           /* end of input ==> done with this file */

     TRBUF(lbuf) ;
     if( verb ) ININFO_message("  processing row #%d = '%s'",row,lbuf+ibot) ;

     if( do_tsv )
       sar = NI_strict_decode_string_list( lbuf+ibot, "\t" ) ; /* 08 Feb 2018 */
     else
       sar = NI_decode_string_list( lbuf+ibot , ";" ) ;
     if( sar == NULL ) continue ;                      /* nuthin ==> loopback */

     if( row == 0 ){   /* first row ==> format element */
       ncol = (niv <= 0) ? sar->num : niv ;
       for( ii=1 ; ii <= ncol ; ii++ )
         NI_add_column( nel , NI_STRING , NULL ) ;
     }

     if( row >= nel->vec_len )
       NI_alter_veclen( nel , nel->vec_len + LVEC ) ; /* need more data space */

     /* put values from this line into this row of the table */

     if( niv <= 0 ){
       for( ii=0 ; ii < ncol ; ii++ ){
         if( ii < sar->num ) dpt = sar->str[ii] ;
         else                dpt = "N/A" ;
         string_ectomy( dpt , "\"'" ) ;
         NI_insert_string( nel , row , ii , dpt ) ;
       }
     } else {
       for( jj=1 ; jj <= niv ; jj++ ){
         ii = ivlist[jj] ;
         if( ii < sar->num ) dpt = sar->str[ii] ;
         else                dpt = "N/A" ;
         string_ectomy( dpt , "\"'" ) ;
         NI_insert_string( nel , row , jj-1 , dpt ) ;
       }
     }

     row++ ;  /* have one more row! */
     NI_delete_str_array(sar) ;

   } /* loop to get next row */

   /* cleanup and exit */

   fclose(fts) ;
   if( ivlist != NULL ) free(ivlist) ;
   if( row == 0 ){ NI_free_element(nel); free(dname); ERREX("Table: no data in file"); }

   NI_alter_veclen(nel,row) ;

   if( verb ){
     ININFO_message("Table element follows::") ;
     NI_write_element_tofile( "stdout:" , nel , NI_TEXT_MODE ) ;
   }

   free(dname) ; RETURN(nel) ;
}

/*----------------------------------------------------------------------------*/
/* Read a tab-separated file, and convert to a NI_element with
   vector labels (vec_lab) and with numeric columns where possible.
*//*--------------------------------------------------------------------------*/

NI_element * THD_read_tsv( char *fname )
{
   NI_element *tnel , *fnel=NULL ;
   int ii,jj , vnum,vlen , nbad ;
   char **vec_lab , **cpt , *dpt ;

ENTRY("THD_read_tsv") ;

   /* try to read as a table of strings */

   tnel = THD_string_table_read( fname , 3 ) ;
   if( tnel == NULL )                           RETURN(NULL) ;

   vnum = tnel->vec_num ;     /* number of columns */
   vlen = tnel->vec_len - 1 ; /* first row is labels */
   if( vnum < 1 && vlen < 2 )                   RETURN(NULL) ;

   /* extract labels from first string of each column vector */

/* INFO_message("TSV data from %s - %d cols %d rows",fname,vnum,vlen) ; */
   vec_lab = NI_malloc( char* , sizeof(char *)*vnum ) ;
   for( ii=0 ; ii < vnum ; ii++ ){
     cpt = (char **)tnel->vec[ii] ;
     vec_lab[ii] = NI_strdup( cpt[0] ) ;
/* ININFO_message(" vec_lab[%d] = %s",ii,vec_lab[ii]) ; */
   }

   fnel = NI_new_data_element( "tsv" , vlen ) ;

#undef  FBAD
#define FBAD(sss)                       \
 ( strcasecmp ((sss),"N/A"  ) == 0 ||   \
   strncasecmp((sss),"NAN",3) == 0 ||   \
   strncasecmp((sss),"INF",3) == 0   )

/* ININFO_message("---columns---")  ; */
   for( ii=0 ; ii < vnum ; ii++ ){
     cpt = (char **)tnel->vec[ii] ;
     jj  = NI_count_numbers( vlen , cpt+1 ) ;
     if( jj == vlen ){  /* pure numbers */
       float *far = (float *)malloc(sizeof(float)*vlen) ;
       for( jj=0 ; jj < vlen ; jj++ ){
         far[jj] = (float)strtod(cpt[jj+1],NULL) ;
       }
       nbad = thd_floatscan( vlen , far ) ;

       /* repair bad things [17 Oct 2018] */
       { int ngood=0 ; float sgood=0.0f ;
         for( jj=0 ; jj < vlen ; jj++ ){
           if( !FBAD(cpt[jj+1]) ){ ngood++ ; sgood += far[jj]; }
         }
         if( ngood < vlen ){
           if( ngood > 0 ) sgood /= ngood ;
           for( jj=0 ; jj < vlen ; jj++ ){
             if( FBAD(cpt[jj+1]) ) far[jj] = sgood ;
           }
         }
       }

       NI_add_column( fnel , NI_FLOAT , far ) ;
       NI_set_column_label( fnel , ii , vec_lab[ii] ) ;
       free(far) ;
/* ININFO_message(" column %s: floats with %d errors",vec_lab[ii],nbad) ; */
     } else {           /* strings */
       NI_add_column( fnel , NI_STRING , cpt+1 ) ;
       NI_set_column_label( fnel , ii , vec_lab[ii] ) ;
/* ININFO_message(" column %s: strings",vec_lab[ii]) ; */
     }
   }

   NI_free_element(tnel) ; /* old stuff gets thrown out */

   /* see if we have to deal with column selectors */

   dpt = strstr(fname,"[") ;
   if( dpt != NULL ){
     int *ivlist ;
     ivlist = MCW_get_labels_intlist( fnel->vec_lab , vnum , dpt ) ;
     if( ivlist != NULL && ivlist[0] > 0 ){ /* extract subset of columns */
       NI_element *qnel = NI_extract_columns( fnel , ivlist[0] , ivlist+1 ) ;
       if( qnel != NULL ){ NI_free_element(fnel) ; fnel = qnel ; }
     }
   }

/*   NI_write_element_tofile( "stderr:" , fnel , NI_TEXT_MODE ) ; */

#if 0 /* debug */
INFO_message("=========== tsv dump ==========") ;
   THD_write_tsv( "stdout:" , fnel ) ;
INFO_message("===============================") ;
#endif

   RETURN(fnel) ;
}

/*----------------------------------------------------------------------------*/

void THD_write_tsv( char *fname , NI_element *nel )
{
   char ssep[2] ; int notfirst,ii,kk ;
   FILE *fp ;

ENTRY("THD_write_tsv") ;

   if( fname == NULL || NI_element_type(nel) != NI_ELEMENT_TYPE ) EXRETURN ;
   if( nel->vec_lab == NULL || nel->vec_num == 0 )                EXRETURN ;

   fp = fopen_maybe(fname) ; if( fp == NULL ) EXRETURN ;

#undef  SET_SEPCHAR
#define SET_SEPCHAR  do{ ssep[0] = ( (notfirst++) ? '\t' : '\0' ) ; } while(0)
   ssep[1] = '\0' ;

   /* header row */

   for( notfirst=kk=0 ; kk < nel->vec_num ; kk++ ){
     SET_SEPCHAR ;
     fprintf(fp,"%s%s",ssep,nel->vec_lab[kk]) ;
   }
   fprintf(fp,"\n") ;

   /* data rows */

   for( ii=0 ; ii < nel->vec_len ; ii++ ){
     for( notfirst=kk=0 ; kk < nel->vec_num ; kk++ ){
       SET_SEPCHAR ;
       if( nel->vec_typ[kk] == NI_FLOAT ){
         float *far = (float *)nel->vec[kk] ;
         fprintf(fp,"%s%g",ssep,far[ii]) ;
       } else if( nel->vec_typ[kk] == NI_STRING ){
         char **cpt = (char **)nel->vec[kk] ;
         fprintf(fp,"%s%s" , ssep , (cpt[ii] != NULL) ? cpt[ii] : "(null)" ) ;
       }
     }
     fprintf(fp,"\n") ;
   }

   fclose_maybe(fp) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void THD_set_tsv_column_labels( NI_element *fnel , char **clab )
{
   int jj ;

ENTRY("THD_set_tsv_column_labels") ;

   if( NI_element_type(fnel) != NI_ELEMENT_TYPE ||
       fnel->vec_num         == 0               ||
       clab                  == NULL              ) EXRETURN ;

   for( jj=0 ; jj < fnel->vec_num ; jj++ )
      NI_set_column_label( fnel , jj , clab[jj] ) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* convert 1D or 2D MRI_IMAGE to a TSV NI_element */

NI_element * THD_mri_to_tsv_element( MRI_IMAGE *imin , char **clab )
{
   MRI_IMAGE *qim ;
   int nx,ny , jj ;
   float *far ;
   NI_element *fnel ;
   char qlab[32] ;

ENTRY("THD_mri_to_tsv_element") ;

   if( imin == NULL || imin->nz > 1 ) RETURN(NULL) ;

   nx = imin->nx ; ny = imin->ny ;
   if( nx < 1 || ny < 1 )             RETURN(NULL) ;

   if( imin->kind != MRI_float ) qim = mri_to_float(imin) ;
   else                          qim = imin ;

   far = MRI_FLOAT_PTR(qim) ;

   fnel = NI_new_data_element( "tsv" , nx ) ;

   for( jj=0 ; jj < ny ; jj++ ){
     NI_add_column( fnel , NI_FLOAT , far+jj*nx ) ;
     if( clab != NULL ){
       NI_set_column_label( fnel , jj , clab[jj] ) ;
     } else {
       sprintf(qlab,"Col#%d",jj) ;
       NI_set_column_label( fnel , jj , qlab ) ;
     }
   }

   if( qim != imin ) mri_free(qim) ;

   RETURN(fnel) ;
}

/*----------------------------------------------------------------------------*/
/* convert NIML data element to an MRI_IMAGE;
   only numeric columns are included in the output;
   if none are found, NULL is returned.
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * THD_niml_to_mri( NI_element *nel )
{
   int ncol , *icol , ii,jj , nx ;
   MRI_IMAGE *outim=NULL ;
   float *outar , *far ;
   char *comlab=NULL ;

ENTRY("THD_niml_to_mri") ;

   if( NI_element_type(nel) != NI_ELEMENT_TYPE ) RETURN(NULL) ;
   if( nel->vec_num < 1 || nel->vec_len < 1 )    RETURN(NULL) ;

   /* find numeric columns */

   icol = (int *)malloc(sizeof(int)*nel->vec_num) ;
   for( ncol=jj=0 ; jj < nel->vec_num ; jj++ ){
     if( NI_IS_NUMERIC_TYPE(nel->vec_typ[jj]) ) icol[ncol++] = jj ;
   }
   if( ncol == 0 ){ free(icol) ; RETURN(NULL) ; } /* nothing */

   nx    = nel->vec_len ;
   outim = mri_new( nx , ncol , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;
   for( jj=0 ; jj < ncol ; jj++ ){
     far = outar + jj*nx ;
     for( ii=0 ; ii < nx ; ii++ )
       far[ii] = NI_extract_float_value(nel,ii,icol[jj]) ;
     /* extract label */
     if( nel->vec_lab != NULL ){
       char *ccc = nel->vec_lab[icol[jj]] ;
       if( ccc == NULL || *ccc == '\0' ) ccc = "Fred" ;
       if( comlab == NULL ){
         comlab = (char *)calloc(16,sizeof(char)) ;
         strcpy(comlab,"LABELS:\t") ;
       }
       comlab = (char *)realloc( comlab , sizeof(char)*(strlen(comlab)+strlen(ccc)+4) ) ;
       if( jj > 0 ) strcat(comlab,"\t") ;
       strcat(comlab,ccc) ;
     }
   }

   free(icol) ;
   if( comlab != NULL ) outim->comments = comlab ;
   RETURN(outim) ;
}
