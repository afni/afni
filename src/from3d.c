/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This program extracts 2D image files from an AFNI 3D dataset.

  File:    from3d.c
  Author:  B. Douglas Ward
  Date:    30 August 1996

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

*/
/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "from3d"                        /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_INITIAL "30 August 1996"  /* date of initial program release */
#define PROGRAM_LATEST "15 August 2001"     /* date of last program revision */

/*---------------------------------------------------------------------------*/

#include "mrilib.h"

#define FatalError(str) \
   ( fprintf(stderr,"\nError: %s\n\r try 'from3d -help'\n",(str)) , exit(1) )

/*---------------------------------------------------------------------------*/

void Syntax(void)
{
   printf(
    "Usage:   from3d [options] -input fname -prefix rname\n"
    "Purpose: Extract 2D image files from a 3D AFNI dataset.\n"
    "Options:\n"
    "-v             Print out verbose information during the run.\n"
    "-nsize         Adjust size of 2D data file to be NxN, by padding\n"
    "                 with zeros, where N is a power of 2.\n"
    "-raw           Write images in 'raw' format (just the data bytes)\n"
    "                 N.B.: there will be no header information saying\n"
    "                       what the image dimensions are - you'll have\n"
    "                       to get that information from the x and y\n"
    "                       axis information output by 3dinfo.\n"
    "-float         Write images as floats, no matter what they are in\n"
    "                 the dataset itself.\n"
    "-zfirst num    Set 'num' = number of first z slice to be extracted.\n"
    "                 (default = 1)\n"
    "-zlast num     Set 'num' = number of last z slice to be extracted.\n"
    "                 (default = largest)\n"
    "-tfirst num    Set 'num' = number of first time slice to be extracted.\n"
    "                 (default = 1)\n"
    "-tlast num     Set 'num' = number of last time slice to be extracted.\n"
    "                 (default = largest)\n"
    "-input fname   Read 3D dataset from file 'fname'.\n"
    "                 'fname' may include a sub-brick selector list.\n"
    "-prefix rname  Write 2D images using prefix 'rname'.\n"
    "\n"
    "               (-input and -prefix are non-optional options: they)\n"
    "               (must be present or the program will not execute. )\n"
    "\n"
    "N.B.: * Image data is extracted directly from the dataset bricks.\n"
    "         If a brick has a floating point scaling factor, it will NOT\n"
    "         be applied.\n"
    "      * Images are extracted parallel to the xy-plane of the dataset\n"
    "         orientation (which can be determined by program 3dinfo).\n"
    "         This is the order in which the images were input to the\n"
    "         dataset originally, via to3d.\n"
    "      * If either of these conditions is unacceptable, you can also\n"
    "         try to use the Save:bkg function from an AFNI image window.\n"
   ) ;
   exit(0) ;
}

/*---------------------------------------------------------------------------*/

void F3D_initialize_user_data ( int Argc, char * Argv[],
   Boolean * verbose, Boolean * nsize, Boolean * raw, Boolean * do_floats ,
   int * zfirst, int * zlast, int * tfirst, int * tlast,
   char * input_filename, char * prefix_filename )
{
   const int BIGNUMBER = 100000;
   int nopt;
   float ftemp;

   /*----- Does user request help menu? -----*/
   if (Argc < 2 || strncmp(Argv[1],"-help",4) == 0) Syntax();

   /*----- Add to program log -----*/
   AFNI_logger (PROGRAM_NAME,Argc,Argv); 

   /* --- set default values --- */
   *verbose = FALSE;
   *nsize = FALSE;
   *raw   = FALSE;      /* 05 Jan 2000 */
   *do_floats = FALSE ; /* 05 Jan 2000 */
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

      /* --- raw option [05 Jan 2000]--- */
      if ( strncmp(Argv[nopt],"-raw",4) == 0 )
      {
         *raw = TRUE;
         nopt++ ;
         continue;
      }

      /* --- float option [05 Jan 2000]--- */
      if ( strncmp(Argv[nopt],"-float",4) == 0 )
      {
         *do_floats = TRUE;
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
   Boolean verbose, nsize, raw, do_floats;
   int ok;
   MRI_IMAGE * im, * im2d, * tim2d , * fim2d ;
   MRI_TYPE kind;
   int ibr, iz, count;
   int zfirst, zlast, tfirst, tlast;
   char input_filename[THD_MAX_NAME],
        prefix_filename[THD_MAX_NAME],
        output_filename[THD_MAX_NAME],
        str[THD_MAX_NAME];

  
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");

   /* --- get user command line inputs --- */
   F3D_initialize_user_data (argc, argv,
      &verbose, &nsize,&raw,&do_floats,
      &zfirst, &zlast, &tfirst, &tlast,
      input_filename, prefix_filename );

   /* --- open 3D dataset --- */
   dset = THD_open_dataset( input_filename ) ;
   if( dset == NULL )  FatalError ("Unable to open input file") ;
   if ( verbose )  printf("\n" "3D Dataset File:    %s\n" , input_filename ) ;

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
      printf ("nx = %d  ny = %d  nz = %d  nv = %d\n",   nx, ny, nz, nv);

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
   if ( verbose ){
      printf ("Input data type : %s\n", MRI_TYPE_name[kind]);
      if( do_floats && kind != MRI_float )
         printf ("Output data type: float\n") ;
   }

   /* --- create 2D data pointer --- */
   im2d = mri_new_vol_empty ( nx, ny, 1, kind );

   count = 0;
   for ( ibr = tfirst-1 ; ibr < tlast ; ibr++ )
   {
      for ( iz = zfirst-1 ; iz < zlast ; iz++ )
      {
         /* --- set 2D data pointer into 3D dataset --- */
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

         /* --- create 2D data file name --- */
         strcpy ( output_filename, prefix_filename );
         if ( nv > 1 )
            sprintf ( str, "%02d.%04d", iz+1, ibr+1 );
         else
            if ( nz > 999 )
               sprintf ( str, ".%04d", iz+1 );
            else
               sprintf ( str, ".%03d", iz+1 );
         strcat ( output_filename, str );

         /* --- write 2D data file --- */
         if ( verbose )
            printf ( "Writing%s2D image: %s\n",
                      (raw) ? " raw " : " " , output_filename );

         fim2d = (nsize) ? mri_nsize(im2d) : im2d ;

         if( do_floats ){
            tim2d = mri_to_float(fim2d) ;
            if( fim2d != im2d ) mri_free(fim2d) ;
            fim2d = tim2d ;
         }

         ok = (raw)? mri_write_raw( output_filename, fim2d )
                   : mri_write    ( output_filename, fim2d ) ;

         if( fim2d != im2d ) mri_free(fim2d) ;

         count += ok ;

      }  /* --- iz --- */
   }  /* --- ibr --- */

   if ( verbose )  printf ("Created %d 2D image files.\n", count);

   /* --- clean up --- */
   free ( im2d );
   THD_delete_3dim_dataset( dset , False ) ;

   exit(0) ;
}
