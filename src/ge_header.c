#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/*****    Compilation:  cc -o ge_header ge_header.c -lm     ******/

/*---------------------------------------------------------------*/

static void swap_4(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;

   b0 = *pntr; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   *pntr = b3; *(pntr+1) = b2; *(pntr+2) = b1; *(pntr+3) = b0;
}

/*---------------------------------------------------------------*/

static void swap_8(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;
   unsigned char b4, b5, b6, b7;

   b0 = *pntr    ; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   b4 = *(pntr+4); b5 = *(pntr+5); b6 = *(pntr+6); b7 = *(pntr+7);

   *pntr     = b7; *(pntr+1) = b6; *(pntr+2) = b5; *(pntr+3) = b4;
   *(pntr+4) = b3; *(pntr+5) = b2; *(pntr+6) = b1; *(pntr+7) = b0;
}

/*---------------------------------------------------------------*/

static void swap_2(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1;

   b0 = *pntr; b1 = *(pntr+1);
   *pntr = b1; *(pntr+1) = b0;
}

/******************************************************************/

int main( int argc , char *argv[] )
{
   FILE *imfile ;
   int  length , skip , swap , gg ;
   char orients[8] , str[8] ;
   int nx , ny , bpp , cflag , hdroff ;

   /*------ help? ------*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: ge_header file ...\n"
            "Prints out information from the GE image header of each file.\n"
           ) ;
     exit(0) ;
   }

   /*----- loop over input files -----*/

   for( gg=1 ; gg < argc ; gg++ ){

     imfile = fopen( argv[gg] , "r" ) ;
     if( imfile == NULL ){
        printf("-----------------------------------\n"
               "Can't open file %s\n" ,
               argv[gg] ) ;
        continue ;  /* skip to next file */
     }

     fseek( imfile , 0L , SEEK_END ) ;  /* get the length of the file */
     length = ftell( imfile ) ;         /* (the AJ way) */

     if( length < 1024 ){
        printf("-----------------------------------\n"
               "File %s is too short to be an image\n" ,
               argv[gg] ) ;
        fclose(imfile) ; continue ;
     }

     /*--- 03 Dec 2001: check for GEMS format file "IMGF"   ---*/
     /*[[[ Information herein from Medical Image Format FAQ ]]]*/

     strcpy(str,"JUNK") ;     /* initialize string */
     rewind(imfile) ;
     fread(str,1,4,imfile) ;  /* check for "IMGF" at start of file */

     if( str[0]=='I' && str[1]=='M' && str[2]=='G' && str[3]=='F' ){ /* good */

       /*-- read next 5 ints (after the "IMGF" string) --*/

       fread( &skip , 4,1, imfile ) ; /* offset into file of image data */
       fread( &nx   , 4,1, imfile ) ; /* x-size */
       fread( &ny   , 4,1, imfile ) ; /* y-size */
       fread( &bpp  , 4,1, imfile ) ; /* bits per pixel (should be 16) */
       fread( &cflag, 4,1, imfile ) ; /* compression flag (1=uncompressed)*/

       /*-- check if nx is funny --*/

       if( nx < 0 || nx > 8192 ){      /* have to byte swap these 5 ints */
         swap = 1 ;                    /* flag to swap data, too */
         swap_4(&skip); swap_4(&nx); swap_4(&ny); swap_4(&bpp); swap_4(&cflag);
         if( nx < 0 || nx > 8192 || ny < 0 || ny > 8192 ){
            printf("-----------------------------------\n"
                   "File %s: illegal nx hand/or ny in header\n" ,
                  argv[gg] ) ;
            fclose(imfile) ; continue ;
         }
       } else {
         swap = 0 ;  /* data is ordered for this CPU */
       }

       printf("-----------------------------------\n"
              "File %s\n"
              " nx=%d  ny=%d  skip=%d  swap=%s  compressed=%s  bits-per-pixel=%d\n",
              argv[gg] , nx , ny , skip ,
              (swap)     ? "YES" : "NO"  ,
              (cflag==1) ? "NO"  : "YES" ,
              bpp
             ) ;

       /*-- try to read image header data as well --*/

       length = fseek( imfile , 148L , SEEK_SET ) ; /* magic GEMS offset */
       if( length == 0 ){                   /* seek was good */
         fread( &hdroff , 4,1 , imfile ) ;  /* location of image header */
         if( swap ) swap_4(&hdroff) ;
         if( hdroff > 0 ){                  /* read from image header */
           float dx,dy,dz , xyz[9] , zz , tr ; int itr , ii,jj,kk ;

           /*-- get voxel grid sizes --*/

           fseek( imfile , hdroff+26 , SEEK_SET ) ;    /* dz */
           fread( &dz , 4,1 , imfile ) ;

           fseek( imfile , hdroff+50 , SEEK_SET ) ;    /* dx and dy */
           fread( &dx , 4,1 , imfile ) ;
           fread( &dy , 4,1 , imfile ) ;

           if( swap ){ swap_4(&dx); swap_4(&dy); swap_4(&dz); }

           printf(" dx=%g dy=%g dz=%g" , dx,dy,dz ) ;

           /* grid orientation: from 3 sets of LPI corner coordinates: */
           /*   xyz[0..2] = top left hand corner of image     (TLHC)   */
           /*   xyz[3..5] = top right hand corner of image    (TRHC)   */
           /*   xyz[6..8] = bottom right hand corner of image (BRHC)   */
           /* GEMS coordinate orientation here is LPI                  */

           fseek( imfile , hdroff+154 , SEEK_SET ) ;  /* another magic number */
           fread( xyz , 4,9 , imfile ) ;
           if( swap ){
              swap_4(xyz+0); swap_4(xyz+1); swap_4(xyz+2);
              swap_4(xyz+3); swap_4(xyz+4); swap_4(xyz+5);
              swap_4(xyz+6); swap_4(xyz+7); swap_4(xyz+8);
           }

           /* x-axis orientation */
           /* ii determines which spatial direction is x-axis  */
           /* and is the direction that has the biggest change */
           /* between the TLHC and TRHC                        */

           dx = fabs(xyz[3]-xyz[0]) ; ii = 1 ;
           dy = fabs(xyz[4]-xyz[1]) ; if( dy > dx ){ ii=2; dx=dy; }
           dz = fabs(xyz[5]-xyz[2]) ; if( dz > dx ){ ii=3;        }
           dx = xyz[ii+2]-xyz[ii-1] ; if( dx < 0. ){ ii = -ii;    }
           switch( ii ){
            case  1: orients[0]= 'L'; orients[1]= 'R'; break; /* Left      to Right     */
            case -1: orients[0]= 'R'; orients[1]= 'L'; break; /* Right     to Left      */
            case  2: orients[0]= 'P'; orients[1]= 'A'; break; /* Posterior to Anterior  */
            case -2: orients[0]= 'A'; orients[1]= 'P'; break; /* Anterior  to Posterior */
            case  3: orients[0]= 'I'; orients[1]= 'S'; break; /* Inferior  to Superior  */
            case -3: orients[0]= 'S'; orients[1]= 'I'; break; /* Superior  to Inferior  */
            default: orients[0]='\0'; orients[1]='\0'; break; /* should never happen    */
           }

           /* y-axis orientation */
           /* jj determines which spatial direction is y-axis  */
           /* and is the direction that has the biggest change */
           /* between the BRHC and TRHC                        */

           dx = fabs(xyz[6]-xyz[3]) ; jj = 1 ;
           dy = fabs(xyz[7]-xyz[4]) ; if( dy > dx ){ jj=2; dx=dy; }
           dz = fabs(xyz[8]-xyz[5]) ; if( dz > dx ){ jj=3;        }
           dx = xyz[jj+5]-xyz[jj+2] ; if( dx < 0. ){ jj = -jj;    }
           switch( jj ){
             case  1: orients[2] = 'L'; orients[3] = 'R'; break;
             case -1: orients[2] = 'R'; orients[3] = 'L'; break;
             case  2: orients[2] = 'P'; orients[3] = 'A'; break;
             case -2: orients[2] = 'A'; orients[3] = 'P'; break;
             case  3: orients[2] = 'I'; orients[3] = 'S'; break;
             case -3: orients[2] = 'S'; orients[3] = 'I'; break;
             default: orients[2] ='\0'; orients[3] ='\0'; break;
           }

           orients[4] = '\0' ;   /* terminate orientation string */

           kk = 6 - abs(ii)-abs(jj) ;   /* which spatial direction is z-axis   */
                                        /* where 1=LR, 2=PA, 3=IS               */
                                        /* (can't tell orientation from 1 slice) */

           zz = xyz[kk-1] ;             /* z-coordinate of this slice */

           printf(" slice offset=%g orient=%s", zz,orients ) ;

           /*-- get TR in seconds --*/

           fseek( imfile , hdroff+194 , SEEK_SET ) ;
           fread( &itr , 4,1 , imfile ) ; /* note itr is an int */
           if( swap ) swap_4(&itr) ;
           tr = 1.0e-6 * itr ;            /* itr is in microsec */
           printf(" TR=%g(s)",tr) ;

           /*-- get TE in milliseconds --*/

           fseek( imfile , hdroff+202 , SEEK_SET ) ;
           fread( &itr , 4,1 , imfile ) ; /* itr is an int, in microsec */
           if( swap ) swap_4(&itr) ;
           printf(" TE=%g(ms)",1.0e-3*itr) ;

           /*-- end of image header printouts --*/

           printf("\n") ;

         } /* end of actually reading image header */

       } /* end of trying to read image header */

       /*****************************************************************
         To actually read image data, do something like this:
           int npix=nx*ny , nb=bpp/8 ;
           data = malloc(nb*npix) ;
           fseek( imfile , skip , SEEK_SET ) ;
           fread( data , 1 , nb*npix , imfile ) ;
           if( swap ){ switch( nb ){
                         case 2: swap_twobytes ( npix , data ); break;
                         case 4: swap_fourbytes( npix , data ); break;
                     } }
         The latter 2 swap functions are in mri_swapbytes.c
       ******************************************************************/

     }  else {  /*-------------- not a GEMS file --------------*/

       printf("-----------------------------------\n"
              "File %s is not a GE image file [doesn't start with IMGF]\n" ,
              argv[gg] ) ;
     }

     fclose(imfile) ;  /* done with this file */

   } /* end of loop over input files */

   exit(0) ;
}
