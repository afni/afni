#include "znzlib.h"

/*
znzlib.c  (zipped or non-zipped library)

*****            This code is released to the public domain.            *****

*****  Author: Mark Jenkinson, FMRIB Centre, University of Oxford       *****
*****  Date:   September 2004                                           *****

*****  Neither the FMRIB Centre, the University of Oxford, nor any of   *****
*****  its employees imply any warranty of usefulness of this software  *****
*****  for any purpose, and do not assume any liability for damages,    *****
*****  incidental or otherwise, caused by any use of this document.     *****

*/

/*

This library provides an interface to both compressed (gzip/zlib) and
uncompressed (normal) file IO.  The functions are written to have the
same interface as the standard file IO functions.  

To use this library instead of normal file IO, the following changes
are required:
 - replace all instances of FILE* with znzFile
 - change the name of all function calls, replacing the initial character
   f with the znz  (e.g. fseek becomes znzseek)
   one exception is rewind() -> znzrewind()
 - add a third parameter to all calls to znzopen (previously fopen)
   that specifies whether to use compression (1) or not (0)
 - use znz_isnull rather than any (pointer == NULL) comparisons in the code
   for znzfile types (normally done after a return from znzopen)
 
NB: seeks for writable files with compression are quite restricted

*/


int znz_isnull(znzFile file)
{
  if (file==NULL)  return 1;
  return ( (file->nzfptr == NULL) 
#ifdef HAVE_ZLIB
            && (file->zfptr == NULL) 
#endif
  );
}


void znz_setnull(znzFile file)
{
  if (file!=NULL) {
    file->nzfptr = NULL; 
#ifdef HAVE_ZLIB
    file->zfptr = NULL; 
#endif
  }
}


/* Note extra argument (use_compression) where 
   use_compression==0 is no compression
   use_compression!=0 uses zlib (gzip) compression
*/

znzFile znzopen(const char *path, const char *mode, int use_compression)
{
  znzFile file;
  file = (znzFile) calloc(1,sizeof(struct znzptr));
#ifdef HAVE_ZLIB
  if (use_compression) {
    file->withz = 1;
    file->zfptr = gzopen(path,mode);
    file->nzfptr = NULL;
  } else {
#endif
    file->withz = 0;
    file->nzfptr = fopen(path,mode);
#ifdef HAVE_ZLIB
    file->zfptr = NULL;
  };
#endif
  return file;
}


znzFile znzdopen(int fd, const char *mode, int use_compression)
{
  znzFile file;
  file = (znzFile) calloc(1,sizeof(struct znzptr));
#ifdef HAVE_ZLIB
  if (use_compression) {
    file->withz = 1;
    file->zfptr = gzdopen(fd,mode);
    file->nzfptr = NULL;
  } else {
#endif
    file->withz = 0;
#ifdef HAVE_FDOPEN
    file->nzfptr = fdopen(fd,mode);
#endif
#ifdef HAVE_ZLIB
    file->zfptr = NULL;
  };
#endif
  return file;
}


int znzclose(znzFile file)
{
  int retval;
  if (file==NULL) return 0;
#ifdef HAVE_ZLIB
  if (file->zfptr!=NULL) { 
    retval = gzclose(file->zfptr);
    file->zfptr = NULL;
    return retval;
  }
#endif
  retval = fclose(file->nzfptr);
  file->nzfptr = NULL;
  return retval;
}

size_t znzread(void* buf, size_t size, size_t nmemb, znzFile file)
{
  if (file==NULL) { return 0; }
#ifdef HAVE_ZLIB
  if (file->zfptr!=NULL) 
    return (size_t) (gzread(file->zfptr,buf,((int) size)*((int) nmemb)) / size);
#endif
  return fread(buf,size,nmemb,file->nzfptr);
}

size_t znzwrite(void* buf, size_t size, size_t nmemb, znzFile file)
{
  if (file==NULL) { return 0; }
#ifdef HAVE_ZLIB
  if (file->zfptr!=NULL) 
    return (size_t) ( gzwrite(file->zfptr,buf,size*nmemb) / size );
#endif
  return fwrite(buf,size,nmemb,file->nzfptr);
}

long znzseek(znzFile file, long offset, int whence)
{
  if (file==NULL) { return 0; }
#ifdef HAVE_ZLIB
  if (file->zfptr!=NULL) return (long) gzseek(file->zfptr,offset,whence);
#endif
  return fseek(file->nzfptr,offset,whence);
}

int znzrewind(znzFile stream)
{
  if (stream==NULL) { return 0; }
#ifdef HAVE_ZLIB
  if (stream->zfptr!=NULL) return gzrewind(stream->zfptr);
#endif
  rewind(stream->nzfptr);
  return 0;
}

long znztell(znzFile file)
{
  if (file==NULL) { return 0; }
#ifdef HAVE_ZLIB
  if (file->zfptr!=NULL) return (long) gztell(file->zfptr);
#endif
  return ftell(file->nzfptr);
}

int znzputs(char* str, znzFile file)
{
  if (file==NULL) { return 0; }
#ifdef HAVE_ZLIB
  if (file->zfptr!=NULL) return gzputs(file->zfptr,str);
#endif
  return fputs(str,file->nzfptr);
}


char * znzgets(char* str, int size, znzFile file)
{
  if (file==NULL) { return NULL; }
#ifdef HAVE_ZLIB
  if (file->zfptr!=NULL) return gzgets(file->zfptr,str,size);
#endif
  return fgets(str,size,file->nzfptr);
}


int znzflush(znzFile file)
{
  if (file==NULL) { return 0; }
#ifdef HAVE_ZLIB
  if (file->zfptr!=NULL) return gzflush(file->zfptr,Z_SYNC_FLUSH);
#endif
  return fflush(file->nzfptr);
}


int znzeof(znzFile file)
{
  if (file==NULL) { return 0; }
#ifdef HAVE_ZLIB
  if (file->zfptr!=NULL) return gzeof(file->zfptr);
#endif
  return feof(file->nzfptr);
}


int znzputc(int c, znzFile file)
{
  if (file==NULL) { return 0; }
#ifdef HAVE_ZLIB
  if (file->zfptr!=NULL) return gzputc(file->zfptr,c);
#endif
  return fputc(c,file->nzfptr);
}


int znzgetc(znzFile file)
{
  if (file==NULL) { return 0; }
#ifdef HAVE_ZLIB
  if (file->zfptr!=NULL) return gzgetc(file->zfptr);
#endif
  return fgetc(file->nzfptr);
}


int znzprintf(znzFile stream, const char *format, ...)
{
  int retval=0;
  char *tmpstr;
  va_list va;
  if (stream==NULL) { return 0; }
  va_start(va, format);
#ifdef HAVE_ZLIB
  if (stream->zfptr!=NULL) {
    tmpstr = (char *)calloc(1,strlen(format) + 1000000);  /* overkill I hope */
    vsprintf(tmpstr,format,va);
    retval=gzprintf(stream->zfptr,"%s",tmpstr);
    free(tmpstr);
  } else 
#endif
  {
   retval=vfprintf(stream->nzfptr,format,va);
  }
  va_end(va);
  return retval;
}

