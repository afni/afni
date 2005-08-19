/*** Whereami.c modified 1/11/05 -- main function by Mike Angstadt of U Chicago ***/

#define MAIN

#include "mrilib.h"
#include "afni.h"
#include <stdio.h>
#include <stdlib.h>

static int           have_dseTT = -1   ;
static THD_3dim_dataset * dseTT = NULL ;
static THD_3dim_dataset * dseTT_big = NULL ; /* 01 Aug 2001 */

#define MAX_FIND 9                    /* max number to find within WAMIRAD  */
#define WAMIRAD  7.5                  /* search radius: must not exceed 9.5 */
static MCW_cluster * wamiclust=NULL ;

/*-----------------------------------------------------------------------*/

THD_3dim_dataset * TT_retrieve_atlas(void)
{
   if( have_dseTT < 0 ) TT_load_atlas() ;
   return dseTT ;                         /* might be NULL */
}

/*-----------------------------------------------------------------------*/

THD_3dim_dataset * TT_retrieve_atlas_big(void) /* 01 Aug 2001 */
{
   if( dseTT_big != NULL ) return dseTT_big ;
   if( have_dseTT < 0    ) TT_load_atlas() ;
   if( dseTT == NULL     ) return NULL ;
   dseTT_big = THD_zeropad( dseTT , 10,0,0,0,0,0 , "TTatlas_big" , 0 ) ;
   DSET_unload( dseTT ) ; /* probably won't need again */
   return dseTT_big ;
}

/*-----------------------------------------------------------------------*/

THD_3dim_dataset * TT_retrieve_atlas_either(void) /* 22 Aug 2001 */
{
   if( dseTT_big != NULL ) return dseTT_big ;
   if( dseTT     != NULL ) return dseTT     ;
   if( have_dseTT < 0    ) TT_load_atlas()  ;
   return dseTT ;
}

/*-----------------------------------------------------------------------*/

int TT_load_atlas(void)
{
   char *epath, *elocal, ename[THD_MAX_NAME], dname[THD_MAX_NAME], *eee ;
   int epos , ll , ii , id ;

ENTRY("TT_load_atlas") ;

   if( have_dseTT >= 0 ) RETURN(have_dseTT) ;  /* for later calls */

   have_dseTT = 0 ;  /* don't have it yet */

   /*----- 20 Aug 2001: see if user specified alternate database -----*/

   epath = my_getenv("AFNI_TTATLAS_DATASET") ;
   if( epath != NULL ){
      dseTT = THD_open_one_dataset( epath ) ;  /* try to open it */
      if( dseTT != NULL ){                     /* got it!!! */
         have_dseTT = 1; RETURN(1);
      }
   }

   /*----- get path to search -----*/

                       epath = getenv("AFNI_PLUGINPATH") ;
   if( epath == NULL ) epath = getenv("AFNI_PLUGIN_PATH") ;
   if( epath == NULL ) epath = getenv("PATH") ;
   if( epath == NULL ) RETURN(0) ;

   /*----- copy path list into local memory -----*/

   ll = strlen(epath) ;
   elocal = AFMALL(char, sizeof(char) * (ll+2) ) ;

   /*----- put a blank at the end -----*/

   strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;

   /*----- replace colons with blanks -----*/

   for( ii=0 ; ii < ll ; ii++ )
      if( elocal[ii] == ':' ) elocal[ii] = ' ' ;

   /*----- extract blank delimited strings;
           use as directory names to look for atlas -----*/

   epos = 0 ;

   do{
      ii = sscanf( elocal+epos , "%s%n" , ename , &id ); /* next substring */
      if( ii < 1 ) break ;                               /* none -> done   */

      epos += id ;                                 /* char after last scanned */

      ii = strlen(ename) ;                         /* make sure name has   */
      if( ename[ii-1] != '/' ){                    /* a trailing '/' on it */
          ename[ii]  = '/' ; ename[ii+1] = '\0' ;
      }
      strcpy(dname,ename) ;
      strcat(dname,"TTatlas+tlrc") ;               /* add dataset name */

      dseTT = THD_open_one_dataset( dname ) ;      /* try to open it */

      if( dseTT != NULL ){                         /* got it!!! */
         free(elocal); have_dseTT = 1; RETURN(1);
      }

      strcpy(dname,ename) ; strcat(dname,"TTatlas.nii.gz") ;
      dseTT = THD_open_one_dataset( dname ) ;
      if( dseTT != NULL ){ free(elocal); have_dseTT = 1; RETURN(1); }

   } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */

   free(elocal) ; RETURN(0) ; /* got here -> didn't find it */
}

/*----------------------------------------------------------------------
  Allows the program to purge the memory used by the TT atlas dataset
------------------------------------------------------------------------*/

void TT_purge_atlas(void)
{
  PURGE_DSET(dseTT) ; return ;
}

void TT_purge_atlas_big(void)
{
   if( dseTT_big != NULL ){ DSET_delete(dseTT_big) ; dseTT_big = NULL ; }
   return ;
}

/*----------------------------------------------------------------------
   Return a multi-line string of TT atlas labels near the given point
   (xx,yy,zz are in Dicom order coordinates).
   If NULL is returned, an error happened.  If no labels are near the
   given point, then a single line saying "you're lost" is returned.
   The string returned is malloc()-ed and should be free()-ed later.
   The string will end with a newline '\n' character.
------------------------------------------------------------------------*/

char * TT_whereami( float xx , float yy , float zz )
{
   int ii,kk , ix,jy,kz , nx,ny,nz,nxy , aa,bb,cc , ff,b2f,b4f,rff ;
   THD_ivec3 ijk ;
   byte *b2 , *b4 ;
   THD_string_array *sar ;
   char *b2lab , *b4lab ;
   char lbuf[256] , *rbuf ;
   int nfind, b2_find[MAX_FIND], b4_find[MAX_FIND], rr_find[MAX_FIND] ;

   THD_3dim_dataset * dset ; /* 01 Aug 2001 */

ENTRY("TT_whereami") ;

   /*-- setup stuff: load atlas dataset, prepare search mask --*/

   if( dseTT == NULL ){
      ii = TT_load_atlas() ; if( ii == 0 ) RETURN(NULL) ;
   }

   /* 01 Aug 2001: maybe use big dataset (so don't need both in memory) */

   dset = (dseTT_big != NULL) ? dseTT_big : dseTT ;

#if 0
if( dset == dseTT_big ) fprintf(stderr,"TT_whereami using dseTT_big\n") ;
else                    fprintf(stderr,"TT_whereami using dseTT\n") ;
#endif

   DSET_load(dset) ;
   b2 = DSET_BRICK_ARRAY(dset,0) ; if( b2 == NULL ) RETURN(NULL) ;
   b4 = DSET_BRICK_ARRAY(dset,1) ; if( b4 == NULL ) RETURN(NULL) ;

   if( wamiclust == NULL ){
      wamiclust = MCW_build_mask( 1.0,1.0,1.0 , WAMIRAD ) ;
      if( wamiclust == NULL ) RETURN(NULL) ;  /* should not happen! */

      for( ii=0 ; ii < wamiclust->num_pt ; ii++ )       /* load radius */
         wamiclust->mag[ii] = (int)rint(sqrt((double)(
                                         wamiclust->i[ii]*wamiclust->i[ii]
                                        +wamiclust->j[ii]*wamiclust->j[ii]
                                        +wamiclust->k[ii]*wamiclust->k[ii]))) ;

      MCW_sort_cluster( wamiclust ) ;  /* sort by radius */
   }

   /*-- find locations near the given one that are in the Atlas --*/

   ijk = THD_3dmm_to_3dind( dset , TEMP_FVEC3(xx,yy,zz) ) ;  /* get indexes */
   UNLOAD_IVEC3(ijk,ix,jy,kz) ;                               /* from coords */

   nx = DSET_NX(dset) ;               /* size of TT atlas dataset axes */
   ny = DSET_NY(dset) ;
   nz = DSET_NZ(dset) ; nxy = nx*ny ;

   nfind = 0 ;

   /*-- check the exact input location --*/

   kk = ix + jy*nx + kz*nxy ;        /* index into brick arrays */
   if( b2[kk] != 0 || b4[kk] != 0 ){
      b2_find[0] = b2[kk] ;
      b4_find[0] = b4[kk] ;
      rr_find[0] = 0      ; nfind++ ;
   }

   /*-- check locations near it --*/

   for( ii=0 ; ii < wamiclust->num_pt ; ii++ ){

      /* compute index of nearby location, skipping if outside atlas */

      aa = ix + wamiclust->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
      bb = jy + wamiclust->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
      cc = kz + wamiclust->k[ii] ; if( cc < 0 || cc >= nz ) continue ;

      kk  = aa + bb*nx + cc*nxy ;   /* index into bricks */
      b2f = b2[kk] ; b4f = b4[kk] ; /* TT structures markers there */

      if( b2f == 0 && b4f == 0 )                            continue ;

      for( ff=0 ; ff < nfind ; ff++ ){       /* cast out         */
         if( b2f == b2_find[ff] ) b2f = 0 ;  /* duplicate labels */
         if( b4f == b4_find[ff] ) b4f = 0 ;  /* we already found */
      }
      if( b2f == 0 && b4f == 0 )                            continue ;

      b2_find[nfind] = b2f ;  /* save what we found */
      b4_find[nfind] = b4f ;
      rr_find[nfind] = (int) wamiclust->mag[ii] ;
      nfind++ ;

      if( nfind == MAX_FIND ) break ;  /* don't find TOO much */
   }

   /*-- assemble output string(s) --*/

#define WAMI_HEAD "+++++++ nearby Talairach Daemon structures +++++++\n"
#define WAMI_TAIL "\n******** Please use results with caution! ********"  \
                  "\n******** Brain anatomy is quite variable! ********"  \
                  "\n******** The database may contain errors! ********"

   if( nfind == 0 ){
      char xlab[24], ylab[24] , zlab[24] ;
      THD_fvec3 tv , mv ;
      float mx,my,mz ;
      char mxlab[24], mylab[24] , mzlab[24] ;

      sprintf(xlab,"%4.0f mm [%c]",-xx,(xx<0.0)?'R':'L') ;
      sprintf(ylab,"%4.0f mm [%c]",-yy,(yy<0.0)?'A':'P') ;
      sprintf(zlab,"%4.0f mm [%c]", zz,(zz<0.0)?'I':'S') ;

      LOAD_FVEC3(tv,xx,yy,zz);
      mv = THD_tta_to_mni(tv); UNLOAD_FVEC3(mv,mx,my,mz);
      sprintf(mxlab,"%4.0f mm [%c]",mx,(mx>=0.0)?'R':'L') ;
      sprintf(mylab,"%4.0f mm [%c]",my,(my>=0.0)?'A':'P') ;
      sprintf(mzlab,"%4.0f mm [%c]",mz,(mz< 0.0)?'I':'S') ;

      rbuf = AFMALL(char, 500) ;
      sprintf(rbuf,"%s\n"
                   "Focus point=%s,%s,%s {T-T Atlas}\n"
                   "           =%s,%s,%s {MNI Brain}\n"
                   "\n"
                   "***** Not near any region stored in database *****\n" ,
              WAMI_HEAD , xlab,ylab,zlab , mxlab,mylab,mzlab ) ;
      RETURN(rbuf) ;
   }

   /*-- bubble-sort what we found, by radius --*/

   if( nfind > 1 ){  /* don't have to sort only 1 result */
     int swap, tmp ;
     do{
        swap=0 ;
        for( ii=1 ; ii < nfind ; ii++ ){
           if( rr_find[ii-1] > rr_find[ii] ){
             tmp = rr_find[ii-1]; rr_find[ii-1] = rr_find[ii]; rr_find[ii] = tmp;
             tmp = b2_find[ii-1]; b2_find[ii-1] = b2_find[ii]; b2_find[ii] = tmp;
             tmp = b4_find[ii-1]; b4_find[ii-1] = b4_find[ii]; b4_find[ii] = tmp;
             swap++ ;
           }
        }
     } while(swap) ;
   }

   /*-- find anatomical label for each found marker, make result string --*/

   INIT_SARR(sar) ; ADDTO_SARR(sar,WAMI_HEAD) ;

   /* 04 Apr 2002: print coordinates (LPI) as well (the HH-PB addition) */

   { char lbuf[128], xlab[24], ylab[24] , zlab[24] ;
     sprintf(xlab,"%4.0f mm [%c]",-xx,(xx<0.0)?'R':'L') ;
     sprintf(ylab,"%4.0f mm [%c]",-yy,(yy<0.0)?'A':'P') ;
     sprintf(zlab,"%4.0f mm [%c]", zz,(zz<0.0)?'I':'S') ;
     sprintf(lbuf,"Focus point=%s,%s,%s {T-T Atlas}",xlab,ylab,zlab) ;
     ADDTO_SARR(sar,lbuf) ;
   }

   /* 29 Apr 2002: print MNI coords as well */

   { THD_fvec3 tv , mv ;
     float mx,my,mz ;
     char mxlab[24], mylab[24] , mzlab[24] , lbuf[128] ;
     LOAD_FVEC3(tv,xx,yy,zz);
     mv = THD_tta_to_mni(tv); UNLOAD_FVEC3(mv,mx,my,mz);
     sprintf(mxlab,"%4.0f mm [%c]",mx,(mx>=0.0)?'R':'L') ;
     sprintf(mylab,"%4.0f mm [%c]",my,(my>=0.0)?'A':'P') ;
     sprintf(mzlab,"%4.0f mm [%c]",mz,(mz< 0.0)?'I':'S') ;
     sprintf(lbuf,"Focus point=%s,%s,%s {MNI Brain}\n",mxlab,mylab,mzlab) ;
     ADDTO_SARR(sar,lbuf) ;
   }

   rff = -1 ;  /* rff = radius of last found label */

   for( ff=0 ; ff < nfind ; ff++ ){
      b2f = b2_find[ff] ; b4f = b4_find[ff] ; b2lab = NULL ; b4lab = NULL ;

      if( b2f != 0 ){                               /* find label     */
         for( ii=0 ; ii < TTO_COUNT ; ii++ )        /* in AFNI's list */
            if( b2f == TTO_list[ii].tdval ) break ;
         if( ii < TTO_COUNT )                       /* always true? */
            b2lab = TTO_list[ii].name ;

         if( b2lab != NULL && xx < 0 && strstr(b2lab,"Left") != NULL ) /* maybe is Right */
            b2lab = TTO_list[ii+1].name ;
      }

      if( b4f != 0 ){
         for( ii=0 ; ii < TTO_COUNT ; ii++ )
            if( b4f == TTO_list[ii].tdval ) break ;
         if( ii < TTO_COUNT )
            b4lab = TTO_list[ii].name ;
         if( b4lab != NULL && xx < 0 && strstr(b4lab,"Left") != NULL )
            b4lab = TTO_list[ii+1].name ;
      }

      if( b2lab == NULL && b4lab == NULL ) continue ;  /* no labels? */

      /* make output label into lbuf */

      lbuf[0] = '\0' ;
      if( b2lab != NULL ){
         if( rr_find[ff] != rff ){
            if( rr_find[ff] > 0 )
              sprintf( lbuf , "Within %d mm: %s" , rr_find[ff] , b2lab ) ;
            else
              sprintf( lbuf , "Focus point: %s" , b2lab ) ;
         } else {
            sprintf( lbuf , "             %s" , b2lab ) ;
         }

         for( kk=strlen(lbuf)-1 ; kk > 0 && lbuf[kk] == '.' ; kk-- )
            lbuf[kk] = '\0' ;                  /* trim trailing .'s */
      }

      if( b4lab != NULL ){
         kk = strlen(lbuf) ;
         if( kk > 0 ){
            sprintf( lbuf+kk , " -AND- %s" , b4lab ) ;
         } else if( rr_find[ff] != rff ){
            if( rr_find[ff] > 0 )
              sprintf( lbuf , "Within %d mm: %s" , rr_find[ff] , b4lab ) ;
            else
              sprintf( lbuf , "Focus point: %s" , b4lab ) ;
         } else {
            sprintf( lbuf , "             %s" , b4lab ) ;
         }

         for( kk=strlen(lbuf)-1 ; kk > 0 && lbuf[kk] == '.' ; kk-- )
            lbuf[kk] = '\0' ;
      }

      ADDTO_SARR(sar,lbuf) ;  /* make a list of labels */

      rff = rr_find[ff] ;  /* save for next time around */
   }

   /*- if didn't make any label, must produce something -*/

   if( sar->num == 1 ){    /* shouldn't ever happen */
      sprintf(lbuf,"Found %d marked but unlabeled regions???\n",nfind) ;
      ADDTO_SARR(sar,lbuf) ;
   } else {
      ADDTO_SARR(sar,WAMI_TAIL) ;  /* cautionary tail */
   }

   /*- convert list of labels into one big multi-line string -*/

   for( nfind=ii=0 ; ii < sar->num ; ii++ ) nfind += strlen(sar->ar[ii]) ;
   rbuf = AFMALL(char, nfind + 2*sar->num + 32 ) ; rbuf[0] = '\0' ;
   for( ii=0 ; ii < sar->num ; ii++ ){
      strcat(rbuf,sar->ar[ii]) ; strcat(rbuf,"\n") ;
   }

   DESTROY_SARR(sar) ; RETURN(rbuf) ;
}


/**************************************************************************
  Main function added by Mike Angstadt on 1/12/05
  usage: whereami x y z [output format]
  where x,y,z are float coordinates in tlrc space
  and output format is a 0 or 1
   0 (default) just outputs the string as it would appear from the interactive
   AFNI Where am I? command
   1 outputs the string as a tab-delimited list of the form:
   Focus point: Some area <tab> Within 6 mm: Some area <tab> etc
***************************************************************************/

#define zischar(ch) ( ( ((ch) >= 'A' && (ch) <= 'Z' ) || ((ch) >= 'a' && (ch) <= 'z' ) ) ? 1 : 0 )
int main(int argc, char **argv)
{
  float x, y, z;
  char *string, *fstring;
  int output = 0;
  int first = 1, num = 0;
  int a;
  int iarg, dicom = 1, i, nakedarg, arglen;

   dicom = 1;
   output = 0;
   iarg = 1 ; nakedarg = 0;
   while( iarg < argc ){
      arglen = strlen(argv[iarg]);
      if(argv[iarg][0] == '-' && arglen > 1 && zischar(argv[iarg][1])) {
         for (i=1;i<arglen; ++i) argv[iarg][i] = tolower(argv[iarg][i]);
         if (strcmp(argv[iarg],"-spm") == 0 || strcmp(argv[iarg],"-lpi") == 0) dicom = 0;
         else if (strcmp(argv[iarg],"-dicom") == 0 || strcmp(argv[iarg],"-rai") == 0) dicom = 1;
         else {
            fprintf(stderr,"** bad option %s\n", argv[iarg]);
            return 1;
         }
      } else {
         /* xyz format */
         if (nakedarg == 0) x = atof(argv[iarg]);
         else if (nakedarg == 1) y = atof(argv[iarg]);
         else if (nakedarg == 2) z = atof(argv[iarg]);
         else if (nakedarg == 3) output = atoi(argv[iarg]);
         ++nakedarg;
      }
      ++iarg;
   }
  
  
  if (nakedarg < 3) {
      printf("Usage: whereami [-lpi/-spm] x y z [output format]\n");
      printf("x y z coordinates are assumed to be in RAI or DICOM format\n");
      printf("unless you use -lpi or -spm to indicate otherwise.\n");
      printf("Output format: 0 - standard AFNI 'Where am I?' format [default]\n");
      printf("               1 - Blank separated list\n");
      return 1;
  }
  
  if (!dicom) {
   /* go from lpi to rai */
   x = -x;
   y = -y; 
  }

  string = TT_whereami(x,y,z);
  if (string == NULL ) {                              /* 30 Apr 2005 [rickr] */
    fprintf(stderr,"** whereami lookup failure: is TTatlas+tlrc/TTatlas.nii.gz available?\n");
    fprintf(stderr,"   (the TTatlas+tlrc or TTatlas.nii.gz dataset must be in your PATH)\n");
    return 1;
  }

  if (output == 1) {
    fstring = malloc(sizeof(string));
    strncpy(fstring, "Focus point", 11);
    num = 11;
    for(a = 0; string[a] != '\0'; a++) {
      /* remove header info up to Focus point:
          remove newlines as you go; once you hit a *, stop */
      if ((string[a] != ':') && (first == 1)) {
        continue;
      }
      first = 0;
      if ((string[a] == ' ') && (string[a-1] == ' ')) {
        continue;
      }
      if ((string[a] == '*')) {
        fstring[num] = '\0';
        printf("%s\n", fstring);
        break;
      }
      if ((string[a] != '\n')) {
        if (string[a] == 'W') {
          fstring[num++] = '\t';
        }
        fstring[num++] = string[a];
      } else {
        fstring[num++] = ' ';
      }
    }
      free(fstring);
  } else {
    printf("%s\n", string);
  }

return 0;
}
