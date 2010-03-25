#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>

#define LINE_GAP 9

static int  line_num = 0 ;

static int   nwsub = 0 ;
static char **wsub = NULL ;
static int  *lwsub = NULL ;
static int  *pwsub = NULL ;

static int   nwign = 0 ;
static char **wign = NULL ;
static int  *lwign = NULL ;

#define WEB "/pub/dist/doc/program_help/"

static void qsort_intchar( int n , int *a , char **ia ) ; /* at end of file */

/*-------------------------------------------------------------------------*/

#define HTTP_check(str) ( strncmp((str),"http://",7) == 0  &&  \
                          !isspace((str)[7])               &&  \
                          !iscntrl((str)[7])               &&  \
                          (str)[7] != '\0'                 &&  \
                          (str)[7] != '*'                  &&  \
                          (str)[7] != '.'                     )

/*-------------------------------------------------------------------------*/

int WSUB_check( char *str )
{
   int qq ;
   for( qq=0 ; qq < nwsub ; qq++ ){
     if( strncmp(str,wsub[qq],lwsub[qq]) == 0 ) return qq ;
   }
   return -1 ;
}

int WIGN_check( char *str )
{
   int qq ;
   for( qq=0 ; qq < nwign ; qq++ ){
     if( strncmp(str,wign[qq],lwign[qq]) == 0 ) return qq ;
   }
   return -1 ;
}

/*-------------------------------------------------------------------------*/
#if 0
int need_expansion( char *buf )
{
   char *hpt ; int jj ;

   if( buf == NULL || buf[0] == '\0' ) return 0 ;

   hpt = strstr(buf,"http://") ;
   if( hpt != NULL      &&
       !isspace(hpt[7]) && !iscntrl(hpt[7]) &&
       hpt[7] != '\0'   && hpt[7] != '*'    && hpt[7] != '.' ) return 1 ;

   for( jj=0 ; jj < nwsub ; jj++ ){
     hpt = strstr(buf,wsub[jj]) ; if( hpt != NULL ) return 1 ;
   }

   return 0 ;
}
#endif

/*-------------------------------------------------------------------------*/

#define CHK4(abcd)                                                    \
  ( tolower(buf[hend-4])==abcd[0] && tolower(buf[hend-3])==abcd[1] && \
    tolower(buf[hend-2])==abcd[2] && tolower(buf[hend-1])==abcd[3]   )

void complex_echo_line( char *buf )
{
   int hend , ii , jj , is_img ;
   char *hpt , cc ;

   /* scan thru buf, printing stuff */

   while( *buf != '\0' ){

     /* direct link to a web page? */

     if( HTTP_check(buf) ){

       /* scan forward to get to end of 'http://something' string at hend */

       for( hend=7 ; buf[hend] != '\0' && !isspace(buf[hend]) ; hend++ ) ; /*nada*/

       /* is this a link to an image? */

       is_img = ( CHK4(".jpg") || CHK4(".png") || CHK4(".gif") ) ;

       if( is_img ){             /* if so, insert an image callout here */
         printf("<center>\n") ;
         printf("<img src='") ;
         for( ii=0 ; ii < hend ; ii++ ) printf("%c",buf[ii]) ;
         printf("' /><br />\n") ;
       }

       /* insert hyperlink here*/

       printf("<a href='") ;
       for( ii=0 ; ii < hend ; ii++ ) printf("%c",buf[ii]) ;
       printf("'>") ;
       for( ii=0 ; ii < hend ; ii++ ) printf("%c",buf[ii]) ;
       printf("</a>") ;

       if( is_img ) printf("</center>\n") ;

       buf += hend ; continue ;
     }

     /* a name to output directly with no editing? */

     jj = WIGN_check(buf) ;
     if( jj >= 0 ){
       printf("%s",wign[jj]) ;
       buf += lwign[jj] ; continue ;
     }

     /* a name to substitute with a link to an AFNI help web page? */

     jj = WSUB_check(buf) ;

     if( jj >= 0 ){

        /* expand name to be a hyperlink to that web page? */

        if( line_num > pwsub[jj]+LINE_GAP ){
          printf("<a href='%s%s.html'>%s</a>",WEB,wsub[jj],wsub[jj]) ;
          pwsub[jj] = line_num ;
        } else {
          printf("%s",wsub[jj]) ;
        }

        buf += lwsub[jj] ; continue ;

     }

     /* a single normal character? */

     cc = buf[0] ;
          if( isspace(cc) ) printf("&nbsp;") ;   /* put in HTML escapes */
     else if( cc == '&'   ) printf("&amp;")  ;   /* for special cases */
     else if( cc == '<'   ) printf("&lt;")   ;
     else if( cc == '>'   ) printf("&gt;")   ;
     else                   printf("%c",cc)  ;   /* perfectly normal character */

     buf++ ; continue ;  /* just advanced one character here */
   }

   /* explicit end of line callout */

   printf("<br />\n") ; return ;
}

/*-------------------------------------------------------------------------*/

void echo_line( char *buf )
{
   int kend = strlen(buf) ;

   if( kend == 0 ) return ;
   if( kend == 1 ){ printf("<br />\n") ; return ; }
   if( buf[kend-1] == '\n' ) buf[kend-1] = '\0' ;

   complex_echo_line(buf) ;
}

/*-------------------------------------------------------------------------*/

int THD_filesize( char *pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL || *pathname == '\0' ) return 0 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return 0 ;
   return (int)buf.st_size ;
}

/*-------------------------------------------------------------------------*/

char * AFNI_suck_file( char *fname )
{
   int len , fd , ii ;
   char *buf ;

   if( fname == NULL || fname[0] == '\0' )        return(NULL) ;
   len = THD_filesize( fname ) ;   if( len <= 0 ) return(NULL) ;
   fd = open( fname , O_RDONLY ) ; if( fd < 0   ) return(NULL) ;

   buf = (char *) malloc( sizeof(char) * (len+4) ) ;
   ii  = read( fd , buf , len ) ;
   close( fd ) ;
   if( ii <= 0 ){ free(buf); return(NULL); }

   buf[len] = '\0' ; return(buf) ;
}

/*-------------------------------------------------------------------------*/
/* default list of program name for which to make links */

static char *wlist_default =
   "afni afni_proc.py align_epi_anat.py "
   "3dABoverlap 3dAFNIto3D 3dAFNItoANALYZE 3dAFNItoMINC 3dAFNItoNIFTI 3dAFNItoNIML "
   "3dAFNItoRaw 3dANALYZEtoAFNI 3dANOVA 3dANOVA2 3dANOVA3 3dAcost 3dAllineate "
   "3dAnatNudge 3dAnhist 3dAttribute 3dAutoTcorrelate 3dAutobox 3dAutomask 3dBRAIN_VOYAGERtoAFNI "
   "3dBandpass 3dBlurInMask 3dBlurToFWHM 3dBrickStat 3dCM 3dCRUISEtoAFNI 3dClipLevel "
   "3dConvolve 3dDFT 3dDTeig 3dDWItoDT 3dDeconvolve 3dDeconvolve_f 3dDespike "
   "3dDetrend 3dEmpty 3dEntropy 3dErrtsCormat 3dExtrema 3dFDR 3dFFT "
   "3dFWHM 3dFWHMx 3dFourier 3dFriedman 3dGetrow 3dGroupInCorr "
   "3dIntracranial 3dInvFMRI 3dKruskalWallis 3dLRflip "
   "3dLocalBistat 3dLocalCormat 3dLocalPV 3dLocalSVD 3dLocalstat 3dMEMA "
   "3dMINCtoAFNI 3dMannWhitney 3dMax 3dMean 3dMedianFilter 3dNLfim 3dNotes "
   "3dOverlap 3dPAR2AFNI.pl 3dPeriodogram 3dREMLfit 3dROIstats 3dRank 3dRegAna "
   "3dRowFillin 3dSatCheck 3dSetupGroupInCorr 3dSkullStrip 3dSpatNorm 3dStatClust 3dSurf2Vol "
   "3dSurfMask 3dSynthesize 3dTSgen 3dTagalign 3dTcat 3dTcorrMap 3dTcorrelate 3dTfitter "
   "3dThreetoRGB 3dToutcount 3dTqual 3dTshift 3dTsmooth 3dTsort "
   "3dTstat 3dTwotoComplex 3dUndump 3dUniformize 3dUpsample 3dVol2Surf "
   "3dWarp 3dWarpDrive 3dWavelets 3dWilcoxon 3dWinsor 3dZcat "
   "3dZcutup 3dZeropad 3dZregrid 3danisosmooth 3daxialize 3dbuc2fim "
   "3dbucket 3dcalc 3dclust 3dcopy 3ddelay "
   "3ddot 3ddup 3dedge3 3dfim 3dfim+ 3dfractionize "
   "3dhistog 3dinfo 3dmaskSVD 3dmaskave 3dmaskdump 3dmatcalc "
   "3dmatmult 3dmaxima 3dmerge 3dnewid 3dnoise 3dnvals "
   "3dpc 3dproject 3drefit 3drename 3dresample 3dretroicor "
   "3drotate 3dsvm 3dttest 3dvolreg "
   "1dAstrip 1dBandpass 1dFlagMotion 1dGentimes 1dMarry "
   "1dRansplit 1dSEM 1dTsort 1dUpsample 1d_tool.py "
   "1dcat 1ddot 1deval 1dfft 1dgenARMA11 1dgrayplot "
   "1dmatcalc 1dnorm 1dplot 1dsum 1dsvd 1dtranspose "
   "README.environment "
;

#define DALL 32

void setup_wsub( int nskip , char **skip )
{
   char *wlist , *cc,*dd ;
   int nall , len , ii ;

   nwign   = 4 + nskip ;
    wign   = (char **)malloc(sizeof(char *)*nwign) ;
   lwign   = (int *)  malloc(sizeof(int)   *nwign) ;
   wign[0] = ".afni.vctime" ;
   wign[1] = ".afni.log" ;
   wign[2] = ".afnirc" ;
   wign[3] = "AFNI" ;
   for( ii=0 ; ii < nskip ; ii++ ) wign[ii+4] = skip[ii] ;

   for( ii=0 ; ii < nwign ; ii++ ) lwign[ii] = -strlen(wign[ii]) ;
   qsort_intchar( nwign , lwign , wign ) ;
   for( ii=0 ; ii < nwign ; ii++ ) lwign[ii] = -lwign[ii] ;

   wlist = AFNI_suck_file("../dist_help.list") ;
   if( wlist == NULL ) wlist = wlist_default ;

   nall  = DALL ;
    wsub = (char **)malloc(sizeof(char *)*nall) ;
   lwsub = (int *)  malloc(sizeof(int)   *nall) ;
   nwsub = 0 ;

 RESTART:
   cc = wlist ;
   while(1){

     /* skip whitespace */

     for( ; *cc != '\0' && isspace(*cc) ; cc++ ) ; /*nada*/
     if( *cc == '\0' ) break ; /* done */

     /* skip to end of non-whitespace string */

     for( dd=cc ; *dd != '\0' && !isspace(*dd) ; dd++ ) ; /*nada*/

     len = dd - cc ;        /* length of string we found */

     if( nwsub == nall ){   /* need new space in wsub? */
       nall += DALL ;
        wsub = (char **)realloc( wsub,sizeof(char *)*nall) ;
       lwsub = (int *)  realloc(lwsub,sizeof(int)   *nall) ;
     }

     /* store string we found and its length */

     lwsub[nwsub] = len ;
      wsub[nwsub] = (char *)malloc(sizeof(char)*(len+1)) ;
     memcpy( wsub[nwsub] , cc , len ) ; wsub[nwsub][len] = '\0' ;
     nwsub++ ; cc = dd ;
   }

   if( nwsub == 0 && wlist != wlist_default ){  /* nothing found? */
     free(wlist) ;
     wlist = wlist_default ;
     goto RESTART ;                             /* try again! */
   }

   if( nwsub > 1 ){  /* sort so length is decreasing */
     for( ii=0 ; ii < nwsub ; ii++ ) lwsub[ii] = -lwsub[ii] ;
     qsort_intchar( nwsub , lwsub , wsub ) ;
     for( ii=0 ; ii < nwsub ; ii++ ) lwsub[ii] = -lwsub[ii] ;
   }
   pwsub = (int *)malloc(sizeof(int)*nwsub) ;
   for( ii=0 ; ii < nwsub ; ii++ ) pwsub[ii] = -666 ;

   if( wlist != wlist_default ) free(wlist) ;

   return ;
}

/*-------------------------------------------------------------------------*/

#define LBUF 4096

int main( int argc , char *argv[] )
{
   char buf[LBUF] , *cpt ;

   if( argc > 1 && strcmp(argv[1],"-help") == 0 ){
     printf("Reads from stdin; lines with http://... hyperlinks\n"
            "get converted into HTML <a href=...> hyperlinks.\n"
            "Purpose: formatting AFNI '-help' files for the Web.\n") ;
     exit(0) ;
   }

   setup_wsub( argc-1 , argv+1 ) ;

   printf("<tt>\n") ;

   do{
     cpt = fgets( buf , LBUF , stdin ) ;
     if( cpt == NULL || buf[0] == '\0' ) break ;
     echo_line(buf) ; buf[0] = '\0' ; line_num++ ;
   } while(1) ;

   printf("</tt>\n") ; exit(0) ;
}

/********************************************************************************/
/* insertion_sort : sort an array of int + char*                              */

static void isort_intchar( int n , int *ar , char **iar )
{
   register int  j , p ;  /* array indices */
   register int temp ;    /* a[j] holding place */
   register char *itemp ;
   register int  *a = ar ;
   register char **ia = iar ;

   if( n < 2 ) return ;

   for( j=1 ; j < n ; j++ ){

     if( a[j] < a[j-1] ){   /* out of order */
       p    = j ;
       temp = a[j] ; itemp = ia[j] ;

       do{
           a[p] =  a[p-1] ; /* at this point, a[p-1] > temp, so move it up */
          ia[p] = ia[p-1] ;
          p-- ;
        } while( p > 0 && temp < a[p-1] ) ;

        a[p] = temp ;       /* finally, put temp in its place */
       ia[p] = itemp ;
     }
   }
}

/********************************************************************************/
/* qsrec : recursive part of quicksort (stack implementation)                   */

#define QS_STACK  1024  /* stack size */
#define QS_SWAPF(x,y) ( temp=(x),(x)=(y),(y)= temp)
#define QS_SWAPI(i,j) (itemp=(i),(i)=(j),(j)=itemp)
#define QS_SWAPV(i,j) (vtemp=(i),(i)=(j),(j)=vtemp)

static void qsrec_intchar( int n , int *ar , char **iar , int cutoff )
{
   register int i , j ;         /* scanning indices */
   register int temp , pivot ;  /* holding places */
   register char *ipivot ;
   register int *a = ar ;
   register char **ia = iar ;
   int itemp ;
   char *vtemp ;

   int left , right , mst , stack[QS_STACK] , nnew ;

   /* return if too short (insertion sort will clean up) */

   if( cutoff < 3 ) cutoff = 3 ;
   if( n < cutoff ) return ;

   /* initialize stack to start with whole array */

   stack[0] = 0   ;
   stack[1] = n-1 ;
   mst      = 2   ;

   /* loop while the stack is nonempty */

   while( mst > 0 ){
      right = stack[--mst] ;  /* work on subarray from left -> right */
      left  = stack[--mst] ;

      i = ( left + right ) / 2 ;           /* middle of subarray */

      /* sort the left, middle, and right a[]'s */

      if( a[left] > a[i]     ){ QS_SWAPF(a[left] ,a[i]    ); QS_SWAPV(ia[left] ,ia[i]    ); }
      if( a[left] > a[right] ){ QS_SWAPF(a[left] ,a[right]); QS_SWAPV(ia[left] ,ia[right]); }
      if( a[i] > a[right]    ){ QS_SWAPF(a[right],a[i]    ); QS_SWAPV(ia[right],ia[i]    ); }

      pivot  = a[i] ;                      /* a[i] is the median-of-3 pivot! */
      a[i]   = a[right] ;
      ipivot = ia[i] ;
      ia[i]  = ia[right] ;

      i = left ;                           /* initialize scanning */
      j = right ;

      /*----- partition:  move elements bigger than pivot up and elements
                          smaller than pivot down, scanning in from ends -----*/

      do{
        for( ; a[++i] < pivot ; ) ;  /* scan i up,   until a[i] >= pivot */
        for( ; a[--j] > pivot ; ) ;  /* scan j down, until a[j] <= pivot */

        if( j <= i ) break ;         /* if j meets i, quit */

        QS_SWAPF( a[i] , a[j] ) ; QS_SWAPV( ia[i] , ia[j] ) ;
      } while( 1 ) ;

      /*----- at this point, the array is partitioned -----*/

      a[right]  = a[i] ;           /*restore the pivot*/
      a[i]      = pivot ;
      ia[right] = ia[i] ;
      ia[i]     = ipivot ;

      /*----- push subarrays [left..i-1] and [i+1..right] onto stack, if big -----*/

      nnew = 0 ;
      if( (i-left)  > cutoff ){ stack[mst++] = left ; stack[mst++] = i-1   ; nnew++ ; }
      if( (right-i) > cutoff ){ stack[mst++] = i+1  ; stack[mst++] = right ; nnew++ ; }

      /* if just added two subarrays to stack, make sure shorter one comes first */

      if( nnew == 2 && stack[mst-3] - stack[mst-4] > stack[mst-1] - stack[mst-2] ){
         QS_SWAPI( stack[mst-4] , stack[mst-2] ) ;
         QS_SWAPI( stack[mst-3] , stack[mst-1] ) ;
      }

   }  /* end of while stack is non-empty */

}

/********************************************************************************/
/* quick_sort :  sort an array partially recursively, and partially insertion   */

#ifndef QS_CUTOFF
#define QS_CUTOFF 10
#endif

static void qsort_intchar( int n , int *a , char **ia )
{
   qsrec_intchar( n , a , ia , QS_CUTOFF ) ;
   isort_intchar( n , a , ia ) ;
   return ;
}
