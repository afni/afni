#include "mrilib.h"

#define FatalError(str) \
   ( fprintf(stderr,"\nEPsim error: %s\n",(str)) , exit(1) )

static int use_shm    = 1 ;
static int delay      = 100 ;
static int use_child  = 0 ;
static int use_3T     = 0 ;
char *     fname_3T   = NULL ;
static int ntimes     = 1 ;
static char host[128] = "localhost" ;
static char buf[4096] ;
static int  nbytes , jj , first=1 ;

#define CONTROL_PORT 7954
#define TCP_PORT     7955
#define SHM_NAME     "shm:epsim:1M"

static IOCHAN * ioc = NULL ;

#define SHORT_DELAY      1            /* msec */
#define LONG_DELAY      10

/*---------------------------------------------------------------------------*/

void Syntax(void)
{
   printf(
    "Program:   epsim \n\n"
    "Purpose:   Extract 2D data files from 3D dataset & send to AFNI\n"
    "Usage:     epsim [-v] [-shm | -tcp] [-delay mmm] [-host cpu] [-child]\n"
    "                 -input fname \n"
    "Options: 	\n"
    "-v                 Print out verbose information.\n"
#if 0
    "-nsize             Adjust size of 2d data file to be NxN, by padding \n"
    "                     with zeros, where N is a power of 2. \n"
#endif
    "-shm               Use shared memory to communicate with AFNI. \n"
    "-tcp               Use TCP/IP to communicate with AFNI. \n"
    "-delay mmm         Delay mmm milliseconds between slices. \n"
    "-host cpu          Access AFNI on host named 'cpu'. \n"
    "-child             Tell AFNI to read header info from a child. \n"
    "-3T fname          Tell AFNI to get data from the 3T_toafni program\n"
    "                     with the 3T control data stored in file 'fname'.\n"
    "-input fname       Read from dataset in file 'fname'. \n"
    "-times n           Send the dataset 'n' times.\n"
   ) ;
   exit(0) ;
}

/*---------------------------------------------------------------------------*/

void F3D_initialize_user_data ( int Argc, char * Argv[],
   Boolean * verbose, Boolean * nsize,
   int * zfirst, int * zlast, int * tfirst, int * tlast,
   char * input_filename, char * prefix_filename )
   
{
   const int BIGNUMBER = 10000;
   int nopt;
   float ftemp;

   if (Argc < 2)  Syntax();

   /* --- set default values --- */
   *verbose = FALSE;
   *nsize = FALSE;
   *zfirst = 1;
   *zlast = BIGNUMBER;
   *tfirst = 1;
   *tlast = BIGNUMBER;
   strcpy(input_filename, "");
   strcpy(prefix_filename, "");

   
   /* --- scan options --- */
   nopt = 1 ; 
   while ( nopt < Argc && Argv[nopt][0] == '-' )
   {
     
       /* --- help option --- */
      if ( strncmp(Argv[nopt],"-help",4) == 0 )  Syntax() ;

      /* --- verbose option --- */
      if ( strncmp(Argv[nopt],"-v",2) == 0 )  
      {  
         *verbose = TRUE;
         nopt++ ;
         continue;
      }
#if 0
      /* --- nsize option --- */
      if ( strncmp(Argv[nopt],"-nsize",4) == 0 ) 
      {
         *nsize = TRUE;
         nopt++ ;
         continue;
      }
#endif

      /*-- -times n --*/
      if( strncmp(Argv[nopt],"-times",4) == 0 ){
         if( ++nopt >= Argc ) FatalError("-times needs an argument") ;
         ftemp = strtod( Argv[nopt] , NULL ) ;
         if( ftemp >= 1.0 ) ntimes = (int) ftemp ;
         nopt++ ; continue ;   
      }

#if 0
      /* --- zfirst option --- */
      if ( strncmp(Argv[nopt],"-zfirst",4) == 0 )
      {
         if( ++nopt >= Argc ) FatalError("-zfirst needs an argument") ;
         ftemp = strtod( Argv[nopt] , NULL ) ;
         *zfirst = (int) ftemp ;
         nopt++ ;
         continue ;   
      }

      /* --- zlast option --- */
      if ( strncmp(Argv[nopt],"-zlast",4) == 0 )
      {
         if( ++nopt >= Argc ) FatalError("-zlast needs an argument") ;
         ftemp = strtod( Argv[nopt] , NULL ) ;
         *zlast = (int) ftemp ;
         nopt++ ;
         continue ;
      }

      /* --- tfirst option --- */ 
      if ( strncmp(Argv[nopt],"-tfirst",4) == 0 )
      {
         if( ++nopt >= Argc ) FatalError("-tfirst needs an argument") ;
         ftemp = strtod( Argv[nopt] , NULL ) ;
         *tfirst = (int) ftemp ;
         nopt++ ;
         continue ;
      }
       
      /* --- tlast option --- */
      if ( strncmp(Argv[nopt],"-tlast",4) == 0 )
      {
         if( ++nopt >= Argc ) FatalError("-tlast needs an argument") ;
         ftemp = strtod( Argv[nopt] , NULL ) ;
         *tlast = (int) ftemp ;
         nopt++ ;
         continue ;
      }
#endif

      if( strncmp(Argv[nopt],"-shm",4) == 0 ){
         use_shm = 1 ;
         nopt++ ; continue ;
      }

      if( strncmp(Argv[nopt],"-tcp",4) == 0 ){
         use_shm = 0 ;
         nopt++ ; continue ;
      }

      if( strncmp(Argv[nopt],"-child",4) == 0 ){
         use_child = 1 ;
         nopt++ ; continue ;
      }

      if( strncmp(Argv[nopt],"-3T",4) == 0 ){
         if( ++nopt >= Argc ) FatalError("-3T needs a filename") ;
         fname_3T = Argv[nopt] ;
         use_3T = 1 ;
         nopt++ ; continue ;
      }

      if( strncmp(Argv[nopt],"-delay",4) == 0 ){
         if ( ++nopt >= Argc ) FatalError("-delay needs a number") ;
         ftemp = strtod( Argv[nopt] , NULL ) ;
         if( ftemp >= 0.0 ) delay = (int) ftemp ;
         nopt++ ; continue ;
      }

      if( strncmp(Argv[nopt],"-host",4) == 0 ){
         if ( ++nopt >= Argc ) FatalError("-host needs a cpu name") ;
         strcpy( host , Argv[nopt] ) ;
         nopt++ ; continue ;
      }
    
      /* --- input file name --- */
      if ( strncmp(Argv[nopt],"-input",4) == 0 )
      {
         if ( ++nopt >= Argc ) FatalError("-input needs a name") ;
         strcpy ( input_filename , Argv[nopt] ) ;
         nopt++ ; continue ;
      }

#if 0      
      /* --- prefix name --- */
      if ( strncmp(Argv[nopt],"-prefix",4) == 0 )
      {
         if ( ++nopt >= Argc ) FatalError("-prefix needs a name") ;
         strcpy ( prefix_filename , Argv[nopt] ) ;
         nopt++ ; continue ;
      }
#endif

      /* --- exception --- */
      fprintf(stderr,"Don't understand argument %s\n",Argv[nopt]) ;
      FatalError ("Illegal input");
      
   }  /* nopt */
   
   /* --- check for valid inputs --- */
#if 0
   if (*zfirst > *zlast)   
      FatalError ("Cannot have zfirst > zlast");
   if (*tfirst > *tlast)   
      FatalError ("Cannot have tfirst > tlast");
   if (!strcmp(prefix_filename,"")) 
      FatalError ("Must specify prefix file name.");
#endif
   if (!strcmp(input_filename,"")) 
      FatalError ("Must specify input file name. ");

   if( strcmp(host,"localhost") != 0 && use_shm ){
      printf("EPsim: must use TCP/IP for host %s\n",host) ;
      use_shm = 0 ;
   }

   if( use_child && use_3T )
      FatalError("Can't use -child and -3T together!") ;

   return;
}

/*---------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   /* --- variable declarations --- */
   THD_3dim_dataset * dset ;
   THD_diskptr * dskptr;
   int nx, ny, nz, nv, itim;
   Boolean verbose, nsize;
   int ok;
   MRI_IMAGE * im, * im2d, * tim2d;
   MRI_TYPE kind;
   int ibr, iz, count, izz,izsub ;
   int zfirst, zlast, tfirst, tlast;
   char input_filename[THD_MAX_NAME],
        prefix_filename[THD_MAX_NAME], 
        output_filename[THD_MAX_NAME],
        str[THD_MAX_NAME];

   /* --- get user command line inputs --- */
   F3D_initialize_user_data (argc, argv, 
      &verbose, &nsize,
      &zfirst, &zlast, &tfirst, &tlast,
      input_filename, prefix_filename );
      
   /* --- open 3d data set --- */
   dset = THD_open_one_dataset( input_filename ) ;
   if( dset == NULL )  FatalError ("Unable to open input file") ;
   if ( verbose )  printf("EPsim: 3d Dataset File = %s\n" , input_filename ) ; 
      
   /* --- load data block --- */
   ok = THD_load_datablock( dset->dblk );
   if ( !ok )  FatalError ("Unable to load data block") ;

   /* --- get data dimensions --- */
   dskptr = dset->dblk->diskptr;
   nx = dskptr->dimsizes[0];
   ny = dskptr->dimsizes[1];
   nz = dskptr->dimsizes[2];
   nv = dskptr->nvals;
   if ( verbose )  
      printf ("EPsim: nx=%d  ny=%d  nz=%d  nv=%d\n",   nx, ny, nz, nv);

   /* --- check for valid user inputs --- */
   if (zfirst < 1) zfirst = 1;
   if (zlast > nz) zlast = nz;
   if (tfirst < 1) tfirst = 1;
   if (tlast > nv) tlast = nv;
   if (zfirst > nz)  FatalError ("No data selected -- zfirst too large.");
   if (zlast < 1)    FatalError ("No data selected -- zlast too small.");
   if (tfirst > nv)  FatalError ("No data selected -- tfirst too large.");
   if (tlast < 1)    FatalError ("No data selected -- tlast too small.");    

   /* --- get data type --- */
   kind = IMAGE_IN_IMARR ( dset->dblk->brick, 0 ) -> kind;
   if ( verbose )  printf ("EPsim: datum = %s \n", MRI_TYPE_name[kind]);

   /* --- create 2d data pointer --- */
   im2d = mri_new_vol_empty ( nx, ny, 1, kind );

   /*** open channel to AFNI ***/

   sprintf( buf , "tcp:%s:%d" , host , CONTROL_PORT ) ;
   ioc = iochan_init( buf , "create" ) ;
   if( ioc == NULL ) FatalError("Cannot open control channel to AFNI") ;

   if( verbose ) printf("EPsim: waiting for AFNI") ; fflush(stdout) ;

   while(1){
      iz = iochan_goodcheck( ioc , 1000 ) ;
      if( iz < 0 ) FatalError("control channel failed") ;
      if( iz > 0 ) break ;
      if( verbose ){ printf(".") ; fflush(stdout) ; }
   }
   if( verbose ){ printf("!\n") ; fflush(stdout) ; }

   if( use_shm ) strcpy( buf , SHM_NAME ) ;
   else          sprintf(buf , "tcp:%s:%d" , host , TCP_PORT ) ;

   if( use_child ){
      jj = strlen(buf) ;
      sprintf(buf+jj,"\ncat epsim.out") ;
   } else if( use_3T ){
      jj = strlen(buf) ;
      sprintf(buf+jj,"\n3T_toafni -dummy < %s" , fname_3T ) ;
   }

   if( verbose ) printf("sending control data: %s\n",buf) ;

   jj = iochan_sendall( ioc , buf , strlen(buf)+1 ) ;
   if( jj < 0 ) FatalError("send control data failed") ;
   iochan_sleep(LONG_DELAY) ;                      /* wait a bit */
   while( ! iochan_clearcheck(ioc,LONG_DELAY) )    /* loop until cleared */
      iochan_sleep(LONG_DELAY) ;

   if( verbose ) printf("EPsim: closing control channel\n") ;
   IOCHAN_CLOSE(ioc) ;

   /*** now open data channel ***/

   ioc = iochan_init( buf , "create" ) ;
   if( ioc == NULL ) FatalError("Cannot open data channel to AFNI") ;

   if( verbose ) printf("EPsim: waiting for AFNI") ; fflush(stdout) ;

   while(1){
      iz = iochan_goodcheck( ioc , 1000 ) ;
      if( iz < 0 ) FatalError("data channel failed") ;
      if( iz > 0 ) break ;
      if( verbose ){ printf(".") ; fflush(stdout) ; }
   }
   if( verbose ){ printf("!\n") ; fflush(stdout) ; }

   if( use_child ){
      FILE * fp = fopen( "epsim.out" , "w" ) ;
      if( fp == NULL ){fprintf(stderr,"Can't open epsim.out!\n");IOCHAN_CLOSE(ioc);exit(1);}
      fprintf( fp ,  "ZNUM %d\n"
                     "ZDELTA %g\n"
                     "XYFOV %g %g\n"
                     "ZFIRST %g%c\n"
                     "ZORDER seq\n"
                     "XYZAXES %s %s %s\n"
                     "ACQUISITION_TYPE %s\n"
                   ,
                     nz ,
                     fabs(dset->daxes->zzdel) ,
                     fabs(dset->daxes->xxdel)*nx , fabs(dset->daxes->yydel)*ny ,
                     fabs(dset->daxes->zzorg) , ORIENT_first[dset->daxes->zzorient] ,
                     ORIENT_shortstr[dset->daxes->xxorient] ,
                       ORIENT_shortstr[dset->daxes->yyorient] ,
                       ORIENT_shortstr[dset->daxes->zzorient] ,
                     ( ((zlast-zfirst)>0) ? "2D+zt" : "2D+z" )
                   ) ;
      fclose(fp) ;
      if( verbose ) printf("EPsim: wrote epsim.out file\n") ;

      sprintf( buf , "XYMATRIX %d %d\n"
                     "DATUM %s\n"
                   ,
                     nx , ny ,
                     MRI_TYPE_name[kind]
                   ) ;
   } else if( use_3T ){
      sprintf( buf , "XYMATRIX %d %d\n"
                     "DATUM %s\n"
                   ,
                     nx , ny ,
                     MRI_TYPE_name[kind]
                   ) ;
   } else {
      sprintf( buf , "ZNUM %d\n"
                     "ZDELTA %g\n"
                     "XYFOV %g %g\n"
                     "ZFIRST %g%c\n"
                     "ZORDER seq\n"
                     "XYZAXES %s %s %s\n"
                     "ACQUISITION_TYPE %s\n"
                     "XYMATRIX %d %d\n"
                     "DATUM %s\n"
                   ,
                     nz ,
                     fabs(dset->daxes->zzdel) ,
                     fabs(dset->daxes->xxdel)*nx , fabs(dset->daxes->yydel)*ny ,
                     fabs(dset->daxes->zzorg) , ORIENT_first[dset->daxes->zzorient] ,
                     ORIENT_shortstr[dset->daxes->xxorient] ,
                       ORIENT_shortstr[dset->daxes->yyorient] ,
                       ORIENT_shortstr[dset->daxes->zzorient] ,
                     ( ((zlast-zfirst)>0) ? "2D+zt" : "2D+z" ) ,
                     nx , ny ,
                     MRI_TYPE_name[kind]
                   ) ;
   }

   nbytes = im2d->nvox * im2d->pixel_size ;

   for( itim=0 ; itim < ntimes ; itim++ ){
   count = 0;

   if( use_3T ) izsub = (nz%2 == 0) ? (nz-1) : (nz) ;

   for ( ibr = tfirst-1 ; ibr < tlast ; ibr++ )
   {
      for ( iz = zfirst-1 ; iz < zlast ; iz++ )
      {
         /* --- set 2d data pointer into 3d data set --- */
         im = IMAGE_IN_IMARR ( dset->dblk->brick, ibr ); 

         if( use_3T ){
           izz = 2*iz ; if( izz >= nz ) izz -= izsub ;  /* alt ordering */
         } else {
           izz = iz ;                                   /* seq ordering */
         }

         switch ( kind )
         {
            case MRI_byte :
               im2d->im.byte_data = im->im.byte_data + izz * nx * ny ;
            break;
            case MRI_short :
               im2d->im.short_data = im->im.short_data + izz * nx * ny ;
            break;
            case MRI_int :
               im2d->im.int_data = im->im.int_data + izz * nx * ny ;
            break;
            case MRI_float :
               im2d->im.float_data = im->im.float_data + izz * nx * ny ;
            break;
            case MRI_double :
               im2d->im.double_data = im->im.double_data + izz * nx * ny ;
            break;
            case MRI_complex :
               im2d->im.complex_data = im->im.complex_data + izz * nx * ny ;
            break;
            case MRI_rgb :
               im2d->im.rgb_data = im->im.rgb_data + izz * nx * ny ;
            break;
            default :
               FatalError ("Illegal data type encountered.");
         } 

#if 0
         /* --- create 2d data file name --- */
         strcpy ( output_filename, prefix_filename );
         if ( nv > 1 )  
            sprintf ( str, "%02d.%04d", izz+1, ibr+1 );
         else
            if ( nz > 999 )
               sprintf ( str, ".%04d", izz+1 );
            else
               sprintf ( str, ".%03d", izz+1 );
         strcat ( output_filename, str );
#endif

         if( first ){
            if( verbose )
               printf("EPsim: sending this data as header info in image channel:\n%s\n",buf) ;
            jj = iochan_sendall( ioc , buf , strlen(buf)+1 ) ;
            if( jj < 0 ) FatalError("send header info failed") ;
            first = 0 ;
         }
 
         if ( verbose )
            printf ( "EPsim: sending 2D image izz=%d ibr=%d\n", izz,ibr ); 

         jj = iochan_sendall( ioc , mri_data_pointer(im2d) , nbytes ) ;
         if( jj < 0 ) FatalError("send image failed") ;
         iochan_sleep( delay ) ;

#if 0
         if ( !nsize )
            ok = mri_write ( output_filename, im2d );
         else
         {
            tim2d = mri_nsize (im2d);
            ok = mri_write ( output_filename, tim2d);
            mri_free (tim2d);
         }
#endif
         
         count ++ ;

      }  /* --- iz --- */
   }  /* --- ibr --- */ 
   sleep(20) ;
   } /* -- itim --*/

   if ( verbose )  printf ("Sent %d 2D images. \n", count);

   if( verbose ){ printf("Waiting for AFNI") ; fflush(stdout) ; }
   while(1){
      jj = iochan_clearcheck(ioc,1000) ;
      if( jj ) break ;
      if( verbose ){ printf(".") ; fflush(stdout) ; }
   }
   if( verbose ) printf("!\n") ;
   iochan_sleep(100) ; IOCHAN_CLOSE(ioc) ;

   exit(0) ;
}
