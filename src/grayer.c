#include <mrilib.h>

#define TOP ((byte)255)

void main( int argc , char * argv[] )
{
   MRI_IMAGE * rim, * gim, * bim ;
   byte      * rby, * gby, * bby ;
   byte  bmax ;
   int   nx,ny,npix , ii , whiten ;
   float fac ;
   char name[256] ;

   if( argc < 4 || strncmp(argv[1],"-help",4) == 0 ){
      printf("Usage: grayer factor root_in root_out [-whiten]\n"
             "       [files are root.red root.grn root.blu]\n" ) ;
      exit(0) ;
   }

   fac = strtod( argv[1] , NULL ) ;
   if( fac < 0.0 || fac > 1.0 ){ printf("Illegal factor\n") ; exit(-1) ; }

   sprintf(name,"%s.red",argv[2]) ; rim = mri_read(name) ;
   if( rim == NULL || rim->kind != MRI_byte ){printf("bad red\n"); exit(-1) ;}

   sprintf(name,"%s.grn",argv[2]) ; gim = mri_read(name) ;
   if( gim == NULL || gim->kind != MRI_byte ){printf("bad grn\n"); exit(-1) ;}

   sprintf(name,"%s.blu",argv[2]) ; bim = mri_read(name) ;
   if( bim == NULL || bim->kind != MRI_byte ){printf("bad blu\n"); exit(-1) ;}

   nx = rim->nx ; ny = rim->ny ; npix = nx * ny ;
   if( bim->nx != nx || bim->ny != ny || gim->nx != nx || gim->ny != ny ){
      printf("input images not conformant!\n") ;
      exit(-1) ;
   }

   rby = mri_data_pointer(rim) ;
   gby = mri_data_pointer(gim) ;
   bby = mri_data_pointer(bim) ;

   whiten = (argc > 4) && (strncmp(argv[4],"-whiten",3)==0) ;

   for( ii=0 ; ii < npix ; ii++ ){
      if( rby[ii] == gby[ii] && gby[ii] == bby[ii] ){
         if( rby[ii] != TOP ){
            rby[ii] *= fac ; gby[ii] *= fac ; bby[ii] *= fac ;
         }
      } else if( whiten ){
         bmax = rby[ii] ; if( gby[ii] > bmax ) bmax = gby[ii] ;
                          if( bby[ii] > bmax ) bmax = bby[ii] ;
         rby[ii] = gby[ii] = bby[ii] = bmax ;
      }
   }

   sprintf(name,"%s.red",argv[3]) ; mri_write(name,rim) ;

   sprintf(name,"%s.grn",argv[3]) ; mri_write(name,gim) ;

   sprintf(name,"%s.blu",argv[3]) ; mri_write(name,bim) ;
}
