/*************************************************************************
 * iopnm.c - homemade I/O procedures for PBM/PGM/PPM raw images
 *
 * $Id$
 *
 * Copyright©INRIA 1999
 *
 * AUTHOR:
 * Gregoire Malandain (greg@sophia.inria.fr)
 * 
 * CREATION DATE: 
 * July, 6 1999
 *
 * ADDITIONS, CHANGES
 *
 * * Jul 15 1999 (Gregoire Malandain)
 *   add P6 (color) images. They are considered as 3D images with z=3.
 *
 */

#include <iopnm.h>

static int _VERBOSE_ = 1;

typedef enum {
  Pfive=5,
  Psix=6
} typeP;


static int _WriteGreyImagesAsColorOnes_ = 0;
static int _MaxGreyValueIs255_ = 0;







static int _convertVectBufferTo3DBuffer( unsigned char *buf,
					 int dimx, int dimy )
{
  char *proc="_convertVectBufferTo3DBuffer";
  unsigned char *tmp = (unsigned char *)NULL;
  int x, y, s=dimx*dimy;

  if ( s <= 0 ) return( 0 );
  tmp = (unsigned char *)malloc( s * 3 * sizeof(unsigned char) );
  if ( tmp == (unsigned char *)NULL ) {
    if ( _VERBOSE_ ) 
      fprintf( stderr, "%s: error in allocating auxiliary buffer\n", proc );
    return( 0 );
  }

  (void)memcpy( (void*)tmp, (void*)buf, s * 3 );
  
  for ( y = 0; y < dimy; y++ )
  for ( x = 0; x < dimx; x++ ) {
    buf[       y*dimx + x ] = tmp [ 3*(y*dimx + x) ];
    buf[ s +   y*dimx + x ] = tmp [ 3*(y*dimx + x) + 1];
    buf[ 2*s + y*dimx + x ] = tmp [ 3*(y*dimx + x) + 2];
  }
  
  free( tmp );
  return( 1 );

}
					 



static int _convert3DBufferToVectBuffer( unsigned char *buf,
					 int dimx, int dimy )
{
  char *proc="_convert3DBufferToVectBuffer";
  unsigned char *tmp = (unsigned char *)NULL;
  int x, y, s=dimx*dimy;

  if ( s <= 0 ) return( 0 );
  tmp = (unsigned char *)malloc( s * 3 * sizeof(unsigned char) );
  if ( tmp == (unsigned char *)NULL ) {
    if ( _VERBOSE_ ) 
      fprintf( stderr, "%s: error in allocating auxiliary buffer\n", proc );
    return( 0 );
  }

  (void)memcpy( (void*)tmp, (void*)buf, s * 3 );
  
  for ( y = 0; y < dimy; y++ )
  for ( x = 0; x < dimx; x++ ) {
    buf[ 3*(y*dimx + x)    ] = tmp [       y*dimx + x ];
    buf[ 3*(y*dimx + x) + 1] = tmp [ s +   y*dimx + x ];
    buf[ 3*(y*dimx + x) + 2] = tmp [ 2*s + y*dimx + x ];
  }
  
  free( tmp );
  return( 1 );

}
					 


static int _convertGreyBufferToVectBuffer( unsigned char *theBuf,
					   unsigned char *resBuf,
					   int dimx, int dimy )
{
  int x, y, s=dimx*dimy;

  if ( s <= 0 ) return( 0 );
  
  for ( y = 0; y < dimy; y++ )
  for ( x = 0; x < dimx; x++ ) {
    resBuf[ 3*(y*dimx + x)    ] = theBuf [ y*dimx + x ];
    resBuf[ 3*(y*dimx + x) + 1] = theBuf [ y*dimx + x ];
    resBuf[ 3*(y*dimx + x) + 2] = theBuf [ y*dimx + x ];
  }
  
  return( 1 );

}
					 



















void *_readPnmImage( char *name, 
		   int *dimx, int *dimy, int *dimz )
{
  char *proc="_readPnmImage";
  char string[256];
  int x=0, y=0, z=1, max=0;
  void *buf = (void*)NULL;
  FILE *f, *fopen();
  int xIsRead = 0;
  int yIsRead = 0;
  int maxIsRead = 0;
  typeP P;
  int sizeOfBuffer;

  *dimx = *dimy = *dimz = 0;

  f = fopen( name, "r" );
  if ( f == (FILE*)NULL ) {
    if ( _VERBOSE_ ) 
      fprintf( stderr, "%s: error in opening %s\n", proc, name );
    return( (void*)NULL );
  }


  if ( fscanf( f, "%s\n", string ) != 1 ) {
    if ( _VERBOSE_ ) 
      fprintf( stderr, "%s: error in reading magic string in %s\n", proc, name );
    fclose( f );
    return( (void*)NULL );
  }

  if ( strncmp( string, "P5", 2 ) == 0 ) {
    P = Pfive;
  }
  else if ( strncmp( string, "P6", 2 ) == 0 ) {
    P = Psix;
  } else {
    if ( _VERBOSE_ ) 
      fprintf( stderr, "%s: the magic string (%s) is not recognised in %s\n", proc, string, name );
    fclose( f );
    return( (void*)NULL );
  }




  do {
    if ( fscanf( f, "%s", string ) != 1 ) {
      fprintf( stderr, "%s: error in reading header in %s\n", proc, name );
      fclose( f );
      return( (void*)NULL );
    }

    /* fprintf( stderr, "read='%s'\n",string);  */
    
    /* is it a comment?
     */
    if ( string[0] == '#' ) {
      do {
	if ( fscanf( f, "%1c", string ) != 1 ) {
	  fprintf( stderr, "%s: error in reading header in %s\n", proc, name );
	  fclose( f );
	  return( (void*)NULL );
	}
      } while ( string[0] != '\n' );
    } else {

      if ( xIsRead == 0 ) {
	if ( sscanf( string, "%d", &x ) != 1 ) {
	  fprintf( stderr, "%s: error in reading X dimension in %s\n", proc, name );
	  fclose( f );
	  return( (void*)NULL );
	}
	xIsRead = 1;
      } else if ( yIsRead == 0 ) {
	if ( sscanf( string, "%d", &y ) != 1 ) {
	  fprintf( stderr, "%s: error in reading Y dimension in %s\n", proc, name );
	  fclose( f );
	  return( (void*)NULL );
	}
	yIsRead = 1;
      } else if ( maxIsRead == 0 ) {
	if ( sscanf( string, "%d", &max ) != 1 ) {
	  fprintf( stderr, "%s: error in reading maximum value in %s\n", proc, name );
	  fclose( f );
	  return( (void*)NULL );
	}
	maxIsRead = 1;
	/* there is still one carriage return to read
	 */
	if ( fscanf( f, "%1c", string ) != 1 ) {
	  fprintf( stderr, "%s: error in reading header in %s\n", proc, name );
	  fclose( f );
	  return( (void*)NULL );
	}
      }

    }
    /* fprintf( stderr, "(x,y,m)=(%d,%d,%d)\n",x,y,max); */

  } while (  maxIsRead == 0 );
  

  /* here it is assumed that we read unsigned char
   */
  sizeOfBuffer = x * y;
  switch( P ) {
  case Psix :
    z = 3;
    sizeOfBuffer *= 3;
    break;
  case Pfive :
    z = 1;
    break;
  default :
    break;
  }

  buf = (void*)malloc( sizeOfBuffer * sizeof( unsigned char ) );
  if ( buf == (void*)NULL ) {
    if ( _VERBOSE_ ) 
      fprintf( stderr, "%s: error in allocating data buffer for %s\n", proc, name );
    fclose( f );
    return( (void*)NULL );
  }
  
  if ( fread( buf, sizeof( unsigned char ), sizeOfBuffer, f ) != sizeOfBuffer ) {
    if ( _VERBOSE_ ) 
      fprintf( stderr, "%s: error in reading data in %s\n", proc, name );
    fclose( f );
    free( buf );
    return( (void*)NULL );
  }
  fclose( f );
    
  switch( P ) {
  case Psix :
    if ( _convertVectBufferTo3DBuffer( buf, x, y ) != 1 ) {
      if ( _VERBOSE_ ) 
	fprintf( stderr, "%s: can not convert data in %s\n", proc, name );
      free( buf );
      return( (void*)NULL );
    }
    break;
  default :
    break;
  }

  *dimx = x;
  *dimy = y;
  *dimz = z;
  return( buf );
}


















void _writePnmImage( char *name, 
		     int x, int y, int z, void *buf )
{
  char *proc="_writePnmImage";
  unsigned char *theBuf = (unsigned char *)buf;
  unsigned char *bufToWrite = (unsigned char *)buf;
  int max, i;
  FILE *f, *fopen();
  int sizeOfBuffer = x * y * z;
  int localz = z;

  if ( buf == (void*)NULL || sizeOfBuffer <= 0 ) {
    if ( _VERBOSE_ ) 
      fprintf( stderr, "%s: null buffer or bad dimensions \n", proc );
    return;
  }

  switch ( z ) {
  case 1 :
    if ( _WriteGreyImagesAsColorOnes_ != 0 ) {
      bufToWrite = (unsigned char *)NULL;
      bufToWrite = (unsigned char *)malloc(  x * y * 3 * sizeof(unsigned char) );
      if ( bufToWrite == (unsigned char *)NULL ) {
	if ( _VERBOSE_ ) 
	  fprintf( stderr, "%s: error in allocating auxiliary buffer\n", proc );
	return;
      }
      _convertGreyBufferToVectBuffer( buf, bufToWrite, x, y );
      localz = 3;
    }
    break;
  case 3 :
    if ( _convert3DBufferToVectBuffer( buf, x, y ) != 1 ) {
      if ( _VERBOSE_ ) 
	fprintf( stderr, "%s: can not convert data\n", proc );
      return;
    }
    break;
  default :
    if ( _VERBOSE_ ) 
      fprintf( stderr, "%s: can not deal with such z dimension (%d)\n", proc, z );
    return;
  }
    
  
  if ( _MaxGreyValueIs255_ == 0 ) {
    max = theBuf[0];
    for ( i=1; i<sizeOfBuffer; i++ )
      if ( max < theBuf[i] ) max = theBuf[i];
  } else {
    max = 255;
  }

  f = fopen( name, "w" );
  if ( f == (FILE*)NULL ) {
    if ( _VERBOSE_ ) 
      fprintf( stderr, "%s: error in opening %s\n", proc, name );
    if ( _WriteGreyImagesAsColorOnes_ != 0 ) free( bufToWrite );
    return;
  }

  switch ( localz ) {
  case 1 :
    if ( fprintf( f, "P5\n" ) <= 0 ) {
      if ( _VERBOSE_ ) 
	fprintf( stderr, "%s: error in writing magic string in %s\n", proc, name );
      fclose( f );
      if ( _WriteGreyImagesAsColorOnes_ != 0 ) free( bufToWrite );
      return;
    }
    break;
  case 3 :
    if ( fprintf( f, "P6\n" ) <= 0 ) {
      if ( _VERBOSE_ ) 
	fprintf( stderr, "%s: error in writing magic string in %s\n", proc, name );
      fclose( f );
      if ( _WriteGreyImagesAsColorOnes_ != 0 ) free( bufToWrite );
      return;
    }
  }

  if ( fprintf( f, "%d %d\n%d\n", x, y, max ) <= 0 ) {
    if ( _VERBOSE_ ) 
      fprintf( stderr, "%s: error in writing header in %s\n", proc, name );
    fclose( f );
    if ( _WriteGreyImagesAsColorOnes_ != 0 ) free( bufToWrite );
    return;
  }
  
  sizeOfBuffer = x * y * localz;
  if ( fwrite( bufToWrite, sizeof( unsigned char ), sizeOfBuffer, f ) != sizeOfBuffer ) {
    if ( _VERBOSE_ ) 
      fprintf( stderr, "%s: error in writing data in %s\n", proc, name );
    fclose( f );
    if ( _WriteGreyImagesAsColorOnes_ != 0 ) free( bufToWrite );
    return;
  }

  if ( _WriteGreyImagesAsColorOnes_ != 0 ) free( bufToWrite );
}






void IoPnm_verbose ( )
{
  _VERBOSE_ = 1;
}

void IoPnm_noverbose ( )
{
  _VERBOSE_ = 0;
}




void IoPnm_WriteGreyAsColor() 
{
  _WriteGreyImagesAsColorOnes_ = 1;
}

void IoPnm_DontWriteGreyAsColor() 
{
  _WriteGreyImagesAsColorOnes_ = 0;
}




void IoPnm_SetMaxGreyValueTo255()
{
  _MaxGreyValueIs255_ = 1;
}

void IoPnm_DontSetMaxGreyValueTo255()
{
  _MaxGreyValueIs255_ = 0;
}

