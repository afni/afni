
/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  This software is Copyright 1996 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application.  The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/

#include "mrilib.h"

#define FatalError(str) \
   ( fprintf(stderr,"\nError: %s\n\r try 'from3d -help'\n",(str)) , exit(1) )

/*---------------------------------------------------------------------------*/

void Syntax(void)
{
   printf(
    "Copyright  1996 Medical College of Wisconsin\n\n"
    "Program:   from3d \n\n"
    "Purpose:   Extract 2d data files from 3d data set. \n"
    "Usage:     from3d [-v] [-nsize]  \n"
    "              [-zfirst num] [-zlast num] [-tfirst num] [-tlast num] \n"
    "              -input fname  -prefix rname \n\n"
    "Options: 	\n"
    "-v                 Print out verbose information.\n"
    "-nsize             Adjust size of 2d data file to be NxN, by padding \n"
    "                     with zeros, where N is a power of 2. \n"
    "-zfirst num        Set 'num' = number of first z slice to be extracted. \n"
    "                     (default = 1) \n"
    "-zlast num         Set 'num' = number of last z slice to be extracted. \n"
    "                     (default = largest) \n"
    "-tfirst num        Set 'num' = number of first time slice to be extracted. \n"
    "                     (default = 1) \n"
    "-tlast num         Set 'num' = number of last time slice to be extracted. \n"
    "                     (default = largest) \n"
    "-input fname       Read 3d data set from file 'fname'. \n"
    "                     'fname' may include a sub-brick selector list.\n"
    "-prefix rname      Write 2d data sets using prefix 'rname'. \n"
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
         
      /* --- nsize option --- */
      if ( strncmp(Argv[nopt],"-nsize",4) == 0 ) 
      {
         *nsize = TRUE;
         nopt++ ;
         continue;
      }
      
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
    
      /* --- input file name --- */
      if ( strncmp(Argv[nopt],"-input",4) == 0 )
      {
         if ( ++nopt >= Argc ) FatalError("-input needs a name") ;
         strcpy ( input_filename , Argv[nopt] ) ;
         nopt++ ; continue ;
      }
      
      /* --- prefix name --- */
      if ( strncmp(Argv[nopt],"-prefix",4) == 0 )
      {
         if ( ++nopt >= Argc ) FatalError("-prefix needs a name") ;
         strcpy ( prefix_filename , Argv[nopt] ) ;
         nopt++ ; continue ;
      }

      /* --- exception --- */
      FatalError ("Illegal input");
      
   }  /* nopt */
   
   /* --- check for valid inputs --- */
   if (*zfirst > *zlast)   
      FatalError ("Cannot have zfirst > zlast");
   if (*tfirst > *tlast)   
      FatalError ("Cannot have tfirst > tlast");
   if (!strcmp(input_filename,"")) 
      FatalError ("Must specify input file name. ");
   if (!strcmp(prefix_filename,"")) 
      FatalError ("Must specify prefix file name.");
   
   return;
}

/*---------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   /* --- variable declarations --- */
   THD_3dim_dataset * dset ;
   THD_diskptr * dskptr;
   int nx, ny, nz, nv;
   Boolean verbose, nsize;
   int ok;
   MRI_IMAGE * im, * im2d, * tim2d;
   MRI_TYPE kind;
   int ibr, iz, count;
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
   dset = THD_open_dataset( input_filename ) ;
   if( dset == NULL )  FatalError ("Unable to open input file") ;
   if ( verbose )  printf("\n" "3d Dataset File:    %s\n" , input_filename ) ; 
      
   /* --- load data block --- */
   ok = THD_load_datablock( dset->dblk , NULL);
   if ( !ok )  FatalError ("Unable to load data block") ;

   /* --- get data dimensions --- */
   dskptr = dset->dblk->diskptr;
   nx = dskptr->dimsizes[0];
   ny = dskptr->dimsizes[1];
   nz = dskptr->dimsizes[2];
   nv = dskptr->nvals;
   if ( verbose )  
      printf ("nx = %d  ny = %d  nz = %d  nv = %d  \n",   nx, ny, nz, nv);

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
   if ( verbose )  printf ("Data type: %s \n", MRI_TYPE_name[kind]);

   /* --- create 2d data pointer --- */
   im2d = mri_new_vol_empty ( nx, ny, 1, kind );

   count = 0;
   for ( ibr = tfirst-1 ; ibr < tlast ; ibr++ )
   {
      for ( iz = zfirst-1 ; iz < zlast ; iz++ )
      {
         /* --- set 2d data pointer into 3d data set --- */
         im = IMAGE_IN_IMARR ( dset->dblk->brick, ibr ); 
         switch ( kind )
         {
            case MRI_byte :
               im2d->im.byte_data = im->im.byte_data + iz * nx * ny ;
            break;
            case MRI_short :
               im2d->im.short_data = im->im.short_data + iz * nx * ny ;
            break;
            case MRI_int :
               im2d->im.int_data = im->im.int_data + iz * nx * ny ;
            break;
            case MRI_float :
               im2d->im.float_data = im->im.float_data + iz * nx * ny ;
            break;
            case MRI_double :
               im2d->im.double_data = im->im.double_data + iz * nx * ny ;
            break;
            case MRI_complex :
               im2d->im.complex_data = im->im.complex_data + iz * nx * ny ;
            break;
            case MRI_rgb :
               im2d->im.rgb_data = im->im.rgb_data + iz * nx * ny ;
            break;
            default :
               FatalError ("Illegal data type encountered.");
         } 

         /* --- create 2d data file name --- */
         strcpy ( output_filename, prefix_filename );
         if ( nv > 1 )  
            sprintf ( str, "%02d.%04d", iz+1, ibr+1 );
         else
            if ( nz > 999 )
               sprintf ( str, ".%04d", iz+1 );
            else
               sprintf ( str, ".%03d", iz+1 );
         strcat ( output_filename, str );
 
         /* --- write 2d data file --- */
         if ( verbose )
            printf ( "Writing 2d data file: %s \n", output_filename ); 
            
         if ( !nsize )
            ok = mri_write ( output_filename, im2d );
         else
         {
            tim2d = mri_nsize (im2d);
            ok = mri_write ( output_filename, tim2d);
            mri_free (tim2d);
         }
         
         count += ok ;

      }  /* --- iz --- */
   }  /* --- ibr --- */ 

   if ( verbose )  printf ("Created %d  2d data files. \n", count);
   
   /* --- clean up --- */
   free ( im2d );
   THD_delete_3dim_dataset( dset , False ) ;
   
   exit(0) ;
}
