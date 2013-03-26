/*************************************************************************
 * convert.c - conversion between types
 *
 * $Id$
 *
 * LICENSE:
 * GPL v3.0 (see gpl-3.0.txt for details)
 *
 * AUTHOR:
 * Gregoire Malandain (gregoire.malandain@inria.fr)
 * 
 * CREATION DATE: 
 * June, 9 1998
 *
 * ADDITIONS, CHANGES
 *
 * - Tue Feb 22 11:25:39 MET 2000 (G. Malandain)
 *   add in ConvertBuffer():
 *       USHORT to UCHAR
 *       USHORT to SHORT
 *
 */

#include <convert.h>

void ConvertBuffer( void *bufferIn,
		    bufferType typeIn,
		    void *bufferOut,
		    bufferType typeOut,
		    int bufferLength )
{
  char *proc = "ConvertBuffer";
  register int i, min, max;
  register u8 *u8buf;
  register s8 *s8buf;
  register u16 *u16buf;
  register s16 *s16buf;
  register u32 *u32buf;
  register s32 *s32buf;
  register r32 *r32buf;
  register r64 *r64buf;

  if ( (typeOut == typeIn) && (bufferOut == bufferIn) )
    return;

  if ( bufferLength <= 0 ) {
    fprintf( stderr, " Fatal error in %s: buffer length is negative or zero.\n", 
	     proc );
    return;
  }
  if ( (bufferIn == (void*)NULL) || (bufferOut == (void*)NULL) ) {
    fprintf( stderr, " Fatal error in %s: NULL buffer(s).\n", 
	     proc );
    return;
  }
  
  switch ( typeOut ) {
  case SCHAR :
    s8buf = (s8*)bufferOut;
    min = -128; max = 127;
    switch( typeIn ) {
    case SCHAR :
      if ( bufferOut == bufferIn ) return;
      (void)memcpy( bufferOut, bufferIn, bufferLength * sizeof(s8) );
      break;
    case FLOAT :
      r32buf = (r32*)bufferIn;
      for (i=bufferLength; i>0; i--, s8buf++, r32buf++ ) {
	if ( *r32buf < min ) *s8buf = min;
	else if ( *r32buf < 0.0 ) *s8buf = (int)(*r32buf - 0.5);
	else if ( *r32buf < max ) *s8buf = (int)(*r32buf + 0.5);
	else *s8buf = max;
      }
      break;
    case DOUBLE :
      r64buf = (r64*)bufferIn;
      for (i=bufferLength; i>0; i--, s8buf++, r64buf++ ) {
	if ( *r64buf < min ) *s8buf = min;
	else if ( *r64buf < 0.0 ) *s8buf = (int)(*r64buf - 0.5);
	else if ( *r64buf < max ) *s8buf = (int)(*r64buf + 0.5);
	else *s8buf = max;
      }
      break;
    default :
      fprintf( stderr, " Error in %s: such conversion not yet implemented.\n", 
	       proc );
      return;
    }
    break; /* end case typeOut = SCHAR */




    
  case UCHAR :
    u8buf = (u8*)bufferOut;
    min = 0; max = 255;
    switch( typeIn ) {
    case UCHAR :
      if ( bufferOut == bufferIn ) return;
      (void)memcpy( bufferOut, bufferIn, bufferLength * sizeof(u8) );
      break;
    case USHORT :
      u16buf = (u16*)bufferIn;
      for (i=bufferLength; i>0; i--, u8buf++, u16buf++ ) {
	if ( *u16buf < max ) *u8buf = (u8)*u16buf;
	else *u8buf = max;
      }
      break;
    case FLOAT :
      r32buf = (r32*)bufferIn;
      for (i=bufferLength; i>0; i--, u8buf++, r32buf++ ) {
	if ( *r32buf < min ) *u8buf = min;
	else if ( *r32buf < max ) *u8buf = (int)(*r32buf + 0.5);
	else *u8buf = max;
      }
      break;
    case DOUBLE :
      r64buf = (r64*)bufferIn;
      for (i=bufferLength; i>0; i--, u8buf++, r64buf++ ) {
	if ( *r64buf < min ) *u8buf = min;
	else if ( *r64buf < max ) *u8buf = (int)(*r64buf + 0.5);
	else *u8buf = max;
      }
      break;
    default :
      fprintf( stderr, " Error in %s: such conversion not yet implemented.\n", 
	       proc );
      return;
    }
    break; /* end case typeOut = UCHAR */





    
  case SSHORT :
    s16buf = (s16*)bufferOut;
    min = -32768; max = 32767;
    switch( typeIn ) {
    case SSHORT :
      if ( bufferOut == bufferIn ) return;
      (void)memcpy( bufferOut, bufferIn, bufferLength * sizeof(s16) );
      break;
    case USHORT :
      u16buf = (u16*)bufferIn;
      for (i=bufferLength; i>0; i--, s16buf++, u16buf++ ) {
	if ( *u16buf < max ) *s16buf = (s16)*u16buf;
	else *s16buf = max;
      }
      break;
    case FLOAT :
      r32buf = (r32*)bufferIn;
      for (i=bufferLength; i>0; i--, s16buf++, r32buf++ ) {
	if ( *r32buf < min ) *s16buf = min;
	else if ( *r32buf < 0.0 ) *s16buf = (int)(*r32buf - 0.5);
	else if ( *r32buf < max ) *s16buf = (int)(*r32buf + 0.5);
	else *s16buf = max;
      }
      break;
    case DOUBLE :
      r64buf = (r64*)bufferIn;
      for (i=bufferLength; i>0; i--, s16buf++, r64buf++ ) {
	if ( *r64buf < min ) *s16buf = min;
	else if ( *r64buf < 0.0 ) *s16buf = (int)(*r64buf - 0.5);
	else if ( *r64buf < max ) *s16buf = (int)(*r64buf + 0.5);
	else *s16buf = max;
      }
      break;
    default :
      fprintf( stderr, " Error in %s: such conversion not yet implemented.\n",
	       proc );
      return;
    }
    break; /* end case typeOut = SSHORT */




    
  case USHORT :
    u16buf = (u16*)bufferOut;
    min = 0; max = 65535;
    switch( typeIn ) {
    case UCHAR :
      u8buf = (u8*)bufferIn;
      for (i=bufferLength; i>0; i--, u8buf++, u16buf++ ) {
	*u16buf = (u16)*u8buf;
      }
      break;
    case USHORT :
      if ( bufferOut == bufferIn ) return;
      (void)memcpy( bufferOut, bufferIn, bufferLength * sizeof(u16) );
      break;
    case FLOAT :
      r32buf = (r32*)bufferIn;
      for (i=bufferLength; i>0; i--, u16buf++, r32buf++ ) {
	if ( *r32buf < min ) *u16buf = min;
	else if ( *r32buf < 0.0 ) *u16buf = (int)(*r32buf - 0.5);
	else if ( *r32buf < max ) *u16buf = (int)(*r32buf + 0.5);
	else *u16buf = max;
      }
      break;
    case DOUBLE :
      r64buf = (r64*)bufferIn;
      for (i=bufferLength; i>0; i--, u16buf++, r64buf++ ) {
	if ( *r64buf < min ) *u16buf = min;
	else if ( *r64buf < 0.0 ) *u16buf = (int)(*r64buf - 0.5);
	else if ( *r64buf < max ) *u16buf = (int)(*r64buf + 0.5);
	else *u16buf = max;
      }
      break;
    default :
      fprintf( stderr, " Error in %s: such conversion not yet implemented.\n",
	       proc );
      return;
    }
    break; /* end case typeOut = USHORT */





    
  case INT :
    s32buf = (s32*)bufferOut;
    switch( typeIn ) {
    case INT :
      if ( bufferOut == bufferIn ) return;
      (void)memcpy( bufferOut, bufferIn, bufferLength * sizeof(s32) );
      break;
    case FLOAT :
      r32buf = (r32*)bufferIn;
      for (i=bufferLength; i>0; i--, s32buf++, r32buf++ ) {
	*s32buf = (int)(*r32buf);
      }
      break;
    case DOUBLE :
      r64buf = (r64*)bufferIn;
      for (i=bufferLength; i>0; i--, s32buf++, r64buf++ ) {
	*s32buf = (int)(*r64buf);
      }
      break;
    default :
      fprintf( stderr, " Error in %s: such conversion not yet implemented.\n",
	       proc );
      return;
    }
    break; /* end case typeOut = INT */





    
  case UINT :
    u32buf = (u32*)bufferOut;
    switch( typeIn ) {
    case UINT :
      if ( bufferOut == bufferIn ) return;
      (void)memcpy( bufferOut, bufferIn, bufferLength * sizeof(u32) );
      break;
    case INT :
      s32buf = (s32*)bufferIn;
      for (i=bufferLength; i>0; i--, u32buf++, s32buf++ ) {
	if ( *s32buf <= 0 ) *u32buf = (int)0;
	else *u32buf = *s32buf;
      }
      break;
    case FLOAT :
      r32buf = (r32*)bufferIn;
      for (i=bufferLength; i>0; i--, u32buf++, r32buf++ ) {
	if ( *r32buf <= 0.0 ) *u32buf = (int)0;
	else *u32buf = (int)(*r32buf + 0.5);
      }
      break;
    case DOUBLE :
      r64buf = (r64*)bufferIn;
      for (i=bufferLength; i>0; i--, u32buf++, r64buf++ ) {
	if ( *r64buf <= 0.0 ) *u32buf = (int)0;
	else *u32buf = (int)(*r64buf + 0.5);
      }
      break;
    default :
      fprintf( stderr, " Error in %s: such conversion not yet implemented.\n",
	       proc );
      return;
    }
    break; /* end case typeOut = INT */





    
  case FLOAT :
    r32buf = (r32*)bufferOut;
    switch( typeIn ) {
    case UCHAR :
      u8buf = (u8*)bufferIn;
      for (i=bufferLength; i>0; i--, r32buf++, u8buf++ ) {
	*r32buf = (float)(*u8buf);
      }
      break;
    case SCHAR :
      s8buf = (s8*)bufferIn;
      for (i=bufferLength; i>0; i--, r32buf++, s8buf++ ) {
	*r32buf = (float)(*s8buf);
      }
      break;
    case USHORT :
      u16buf = (u16*)bufferIn;
      for (i=bufferLength; i>0; i--, r32buf++, u16buf++ ) {
	*r32buf = (float)(*u16buf);
      }
      break;
    case SSHORT :
      s16buf = (s16*)bufferIn;
      for (i=bufferLength; i>0; i--, r32buf++, s16buf++ ) {
	*r32buf = (float)(*s16buf);
      }
      break;
    case INT :
      s32buf = (s32*)bufferIn;
      for (i=bufferLength; i>0; i--, r32buf++, s32buf++ ) {
	*r32buf = (float)(*s32buf);
      }
      break;
    case FLOAT :
      if ( bufferOut == bufferIn ) return;
      (void)memcpy( bufferOut, bufferIn, bufferLength * sizeof(r32) );
      break;
    case DOUBLE :
      r64buf = (r64*)bufferIn;
      for (i=bufferLength; i>0; i--, r32buf++, r64buf++ ) {
	*r32buf = (float)(*r64buf);
      }
      break;
    default :
      fprintf( stderr, " Error in %s: such conversion not yet implemented.\n",
	       proc );
      return;
    }
    break; /* end case typeOut = FLOAT */




    
  case DOUBLE :
    r64buf = (r64*)bufferOut;
    switch( typeIn ) {
    case UCHAR :
      u8buf = (u8*)bufferIn;
      for (i=bufferLength; i>0; i--, r64buf++, u8buf++ ) {
	*r64buf = (double)(*u8buf);
      }
      break;
    case SCHAR :
      s8buf = (s8*)bufferIn;
      for (i=bufferLength; i>0; i--, r64buf++, s8buf++ ) {
	*r64buf = (double)(*s8buf);
      }
      break;
    case USHORT :
      u16buf = (u16*)bufferIn;
      for (i=bufferLength; i>0; i--, r64buf++, u16buf++ ) {
	*r64buf = (double)(*u16buf);
      }
      break;
    case SSHORT :
      s16buf = (s16*)bufferIn;
      for (i=bufferLength; i>0; i--, r64buf++, s16buf++ ) {
	*r64buf = (double)(*s16buf);
      }
      break;
    case INT :
      s32buf = (s32*)bufferIn;
      for (i=bufferLength; i>0; i--, r64buf++, s32buf++ ) {
	*r64buf = (double)(*s32buf);
      }
      break;
    case FLOAT :
      r32buf = (r32*)bufferIn;
      for (i=bufferLength; i>0; i--, r32buf++, r64buf++ ) {
	*r64buf = (double)(*r32buf);
      }
      break;
    case DOUBLE :
      if ( bufferOut == bufferIn ) return;
      (void)memcpy( bufferOut, bufferIn, bufferLength * sizeof(r64) );
      break;
    default :
      fprintf( stderr, " Error in %s: such conversion not yet implemented.\n", 
	       proc );
      return;
    }
    break; /* end case typeOut = DOUBLE */



   
  default :
    fprintf( stderr, " Error in %s: such output type not yet handled.\n",
	     proc );
    return;
  }
}





void Convert_r32_to_s8( r32 *theBuf,
			s8 *resBuf,
			int size )
{
  register int i;
  register r32* tb = theBuf;
  register s8* rb = resBuf;
  
  for ( i=0; i<size; i++, tb++, rb++ ) {
    if ( *tb < -128.0 ) {
      *rb = -128;
    } else if ( *tb < 0.0 ) {
      *rb = (int)(*tb - 0.5);
    } else if ( *tb < 127.0 ) {
      *rb = (int)(*tb + 0.5);
    } else {
      *rb = 127;
    }
  }
}





void Convert_r32_to_u8( r32 *theBuf,
			u8 *resBuf,
			int size )
{
  register int i;
  register r32* tb = theBuf;
  register u8* rb = resBuf;
  
  for ( i=0; i<size; i++, tb++, rb++ ) {
    if ( *tb < 0.0 ) {
      *rb = 0;
    } else if ( *tb < 255.0 ) {
      *rb = (int)(*tb + 0.5);
    } else {
      *rb = 255;
    }
  }
}





void Convert_r32_to_s16( r32 *theBuf,
			 s16 *resBuf,
			 int size )
{
  register int i;
  register r32* tb = theBuf;
  register s16* rb = resBuf;
  
  for ( i=0; i<size; i++, tb++, rb++ ) {
    if ( *tb < -32768.0 ) {
      *rb = -32768;
    } else if ( *tb < 0.0 ) {
      *rb = (int)(*tb - 0.5);
    } else if ( *tb < 32767.0 ) {
      *rb = (int)(*tb + 0.5);
    } else {
      *rb = 32767;
    }
  }
}





void Convert_r32_to_u16( r32 *theBuf,
			 u16 *resBuf,
			 int size )
{
  register int i;
  register r32* tb = theBuf;
  register u16* rb = resBuf;
  
  for ( i=0; i<size; i++, tb++, rb++ ) {
    if ( *tb < 0.0 ) {
      *rb = 0;
    } else if ( *tb < 65535.0 ) {
      *rb = (int)(*tb + 0.5);
    } else {
      *rb = 65535;
    }
  }
}


      
