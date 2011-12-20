#include "mrilib.h"

#define VERSION_STRING "<center><small>galler v0.1 - RWCox</small></center>\n"

typedef struct { char name[64] ; int *vpt , retab,bot,top ; } cvar ;

static int thumbsize = 120 ;
static int imagesize = 900 ;
static int rowmax    = 7 ;

static cvar clist[] = {
 { "thumbsize" , &thumbsize , 1 , 50,500 } ,
 { "imagesize" , &imagesize , 0 , 50,2000} ,
 { "rowmax"    , &rowmax    , 1 ,  2,10  } ,
 { "\0"        , NULL       , 0 ,  0,0   }
} ;

static int tabled = 0 ;
static int tr     = 0 ;
static int tc     = 0 ;

static float gam  = 1.0f ;

static char imagename[1024] ;
static char thumbname[1024] ;

static int  nx_im , ny_im , nx_th , ny_th ;

static void GAL_imageize( char *iname, char *prefix , int lab ) ;

#define CLOSEROW                        \
 do{ if( tabled && tr ){                 \
       fprintf(ofp,"</tr>\n\n"); tr=tc=0; \
     }} while(0)

#define OPENTABLE                                   \
 do{ fprintf(ofp,"\n<center>\n"                      \
       "<table cellspacing='2' cellpadding='2'>\n") ; \
     tabled = 1 ; tr = tc = 0 ;                        \
 } while(0)

#define CLOSETABLE                                           \
 do{ if(tabled){ CLOSEROW ;                                   \
       fprintf(ofp,"</table>\n</center>\n<hr>\n"); tabled = 0; \
     }} while(0)

#define HEADERIZE                                                    \
 do{ if( !head_done ){                                               \
       int hh ;                                                      \
       fprintf(ofp, "<html>\n") ;                                    \
       if( num_tst > 0 ){                                            \
         fprintf(ofp,"<head><title>") ;                              \
         for( hh=0; hh < num_tst; hh++ ) fprintf(ofp," %s",tst[hh]); \
         fprintf(ofp,"</title></head>\n") ;                          \
       }                                                             \
       fprintf(ofp, "<body") ;                                       \
       for( hh=0 ; hh < num_bst ; hh++ ) fprintf(ofp," %s",bst[hh]); \
       fprintf(ofp," >\n") ;                                         \
       head_done = 1 ;                                               \
     }} while(0)

/*-----------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   NI_str_array *sar=NULL ;
   FILE *ifp=stdin , *ofp=stdout ;
   int iarg=1 , ii , nim=0 ;
   char linbuf[65536] , *cpt , *prefix , *inam,*tnam ;
   int head_done=0 , tdperc ;
   int num_bst=0 ; char *bst[666] ;
   int num_tst=0 ; char *tst[666] ;

   /* - - - - - - - - - - */

   if( argc < 3 || strcasecmp(argv[1],"-help") == 0 ){
     printf("Usage: galler infile outdirectory\n") ;
     printf(
       "Creates an HTML thumbnail image gallery in 'outdirectory',\n"
       "using the specification in 'infile', which consists of\n"
       "a series of 1 line commands and file specifications.\n"
     ) ;
     exit(0) ;
   }

   /* - - - - - - - - - - */

   while( iarg < argc && argv[iarg][0] == '-' && argv[iarg][1] != '\0' ){
     ERROR_exit("This program has no options") ;
   }

   /* - - - - - - - - - - */

   if( strcmp(argv[iarg],"-") != 0 ){
     ifp = fopen( argv[iarg] , "r" ) ;
     if( ifp == NULL ) ERROR_exit("Can't open input file '%s'",argv[iarg]);
     INFO_message("Opened input file '%s'",argv[iarg]) ;
   }
   iarg++ ;

   /* - - - - - - - - - - */

   prefix = argv[iarg] ;
   if( THD_is_ondisk(prefix) ){
     if( !THD_is_directory(prefix) )
       ERROR_exit("Prefix '%s' already exists, but not a directory!",prefix);
   } else {
     ii = THD_mkdir(prefix) ;
     if( ii == 0 ) ERROR_exit("Can't create directory '%s'",prefix);
     INFO_message("Created directory '%s'",prefix) ;
   }

   /* - - - - - - - - - - */

   sprintf(imagename,"%-1.999s/index.html",prefix) ;
   ofp = fopen( imagename , "w" ) ;
   if( ofp == NULL ) ERROR_exit("Can't open file '%s' for output",imagename);

   /* - - - - - - - - - - */

#if 0
   fprintf(ofp,"<html>\n"
               "<body bgcolor='#ffeebb'>\n"
          ) ;
   head_done = 1 ;
#endif

   /* - - - - - - - - - - */

 GetLine:
   cpt = afni_fgets( linbuf , 65536 , ifp ) ;
   if( cpt == NULL ) goto CleanUp ;
 GotLine:
   if( sar != NULL ) NI_delete_str_array(sar) ;
   sar = NI_decode_string_list( linbuf , "~" ) ;
   if( sar == NULL || sar->num < 1 ){ CLOSEROW ; goto GetLine ; }
   if( sar->str[0][0] == '#' ){
     char *dpt ; int ndpt ;
     CLOSEROW ;
     if( sar->str[0][1] == '\0' ){
       if( sar->num < 2 ) goto GetLine ;
       cpt = sar->str[1] ;
       dpt = (sar->num > 2) ? sar->str[2] : NULL ;
       ndpt = 2 ;
     } else {
       cpt = sar->str[0]+1 ;
       dpt = (sar->num > 1) ? sar->str[1] : NULL ;
       ndpt = 1 ;
     }
     if( dpt != NULL ){
       int val=(int)strtod(dpt,NULL) , didit=0 ;
       for( ii=0 ; clist[ii].vpt != NULL ; ii++ ){
         if( strcasecmp(cpt,clist[ii].name) == 0 ){
                if( val < clist[ii].bot ) val = clist[ii].bot ;
           else if( val > clist[ii].top ) val = clist[ii].top ;
           *(clist[ii].vpt) = val ;
           if( clist[ii].retab ) CLOSETABLE ;
           INFO_message("Set variable %s = %d",clist[ii].name,val) ;
           didit = 1 ; break ;
         }
       }
       if( didit ) goto GetLine ;
     }
     if( strncasecmp(cpt,"cop",3)==0 || strncasecmp(cpt,"lit",3)==0 ){
       HEADERIZE ; CLOSETABLE ;
       while(1){
         cpt = afni_fgets( linbuf , 65536 , ifp ) ;
         if( cpt == NULL ) goto CleanUp ;
         for( ; isspace(*cpt) ; cpt++ ) ; /* nada */
         if( *cpt == '#' ) goto GotLine ;
         fprintf(ofp,"%s",cpt) ;
       }
     } else if( strcasecmp(cpt,"hr")==0 ){
       HEADERIZE ;
       if( tabled ) CLOSETABLE ;
       else         fprintf(ofp,"<hr>\n") ;
     } else if( strncasecmp(cpt,"newtab",6)==0 ){
       HEADERIZE ;
       CLOSETABLE ;
     } else if( strncasecmp(cpt,"body",4)==0 && dpt != NULL ){
       for( ii=ndpt ; ii < sar->num ; ii++ )
         bst[num_bst++] = strdup(sar->str[ii]) ;
     } else if( strncasecmp(cpt,"titl",4)==0 && dpt != NULL ){
       for( ii=ndpt ; ii < sar->num ; ii++ )
         tst[num_tst++] = strdup(sar->str[ii]) ;
     } else if( strncasecmp(cpt,"gam",3)==0 && dpt != NULL ){
       float qam=(float)strtod(dpt,NULL) ;
       if( qam > 0.0f ){ gam = qam; INFO_message("Set gamma = %f",gam); }
     }
     goto GetLine ;
   }

   HEADERIZE ;
   if( !tabled ) OPENTABLE ;

   if( !tr ){
     fprintf(ofp,"<tr>\n") ; tr = 1 ;
   }

   INFO_message("Processing image '%s'",sar->str[0]) ;
   GAL_imageize( sar->str[0] , prefix , ++nim ) ;
   if( imagename[0] == '\0' || thumbname[0] == '\0' ){
     WARNING_message("Can't process image '%s'",sar->str[0]) ;
     --nim ; goto GetLine ;
   }
   inam = THD_trailname(imagename,0) ;
   tnam = THD_trailname(thumbname,0) ;
   tdperc = (int)(100.0/rowmax) ;
   fprintf(ofp,
           "<td align='center' width='%d%%'> "
           "<a href='%s'><img src='%s' width='%d' height='%d' /></a>" ,
           tdperc , inam , tnam , nx_th , ny_th ) ;
   if( sar->num > 1 ){
    fprintf(ofp,"<br><small>\n") ;
    for( ii=1 ; ii < sar->num ; ii++ ) fprintf(ofp," %s",sar->str[ii]) ;
    fprintf(ofp,"</small>\n") ;
   }
   fprintf(ofp,"</td>\n") ;
   if( ++tc >= rowmax ) CLOSEROW ;
   goto GetLine ;

   /* - - - - - - - - - - */

 CleanUp:
   CLOSETABLE ;
   if( head_done )
     fprintf(ofp,
              VERSION_STRING
              "</body>\n"
              "</html>\n") ;
   else
     WARNING_message("No Web page output?!") ;

   INFO_message("Program done") ; exit(0) ;
}

/*-----------------------------------------------------------------------*/

static void GAL_imageize( char *iname, char *prefix , int lab )
{
   MRI_IMAGE *inim , *qim , *bim ;
   int nx,ny , nxnew , nynew ;

   imagename[0] = thumbname[0] = '\0' ;

   if( iname  == NULL || *iname  == '\0' ) return ;
   if( prefix == NULL || *prefix == '\0' ) prefix = "." ;

   inim = mri_read( iname ) ; if( inim == NULL ) return ;
   if( inim->kind != MRI_rgb ){
     qim = mri_to_rgb(inim) ; mri_free(inim) ; inim = qim ;
   }
   if( gam != 1.0f ){
     ININFO_message(" -Processing gamma") ;
     mri_gamma_rgb_inplace( gam , inim ) ;
   }

   nx = inim->nx ; ny = inim->ny ;

   sprintf(imagename,"%-1.999s/Im%04d.jpg",prefix,lab) ;
   if( nx <= imagesize && ny <= imagesize ){
     mri_write_jpg( imagename , inim ) ;
     nx_im = inim->nx ; ny_im = inim->ny ;
   } else {
     float fx , fy ;
     fx = imagesize / (float)nx ; fy = imagesize / (float)ny ;
     fx = MIN(fx,fy) ; nxnew = (int)(nx*fx); nynew = (int)(ny*fx) ;
     if( fx < 0.95f ){
       float sigma = 0.3456789f/fx ;
       ININFO_message(" -Antialias filter sigma=%.2f",sigma) ;
       bim = mri_rgb_blur2D( sigma , inim ) ;
     }
     else bim = inim ;
     ININFO_message(" -Resizing to %d x %d",nxnew,nynew) ;
     qim = mri_resize( bim , nxnew , nynew ) ;
     nx_im = qim->nx ; ny_im = qim->ny ;
     mri_write_jpg( imagename , qim ) ;
     mri_free(qim) ; if( bim != inim ) mri_free(bim) ;
   }

   sprintf(thumbname,"%-1.999s/Th%04d.jpg",prefix,lab) ;
   if( nx <= thumbsize && ny <= thumbsize ){
     mri_write_jpg( thumbname , inim ) ;
     nx_th = inim->nx ; ny_th = inim->ny ;
   } else {
     float fx , fy ;
     fx = thumbsize / (float)nx ; fy = thumbsize / (float)ny ;
     fx = MIN(fx,fy) ; nxnew = (int)(nx*fx); nynew = (int)(ny*fx) ;
     ININFO_message(" - Making thumbnail %d x %d",nxnew,nynew) ;
     qim = mri_resize( inim , nxnew , nynew ) ;
     nx_th = qim->nx ; ny_th = qim->ny ;
     mri_write_jpg( thumbname , qim ) ; mri_free(qim) ;
   }

   mri_free(inim) ; return ;
}
