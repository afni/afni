#include "znzlib.h"

int main(int argc, char *argv[])
{
  int b;
  char *c;
  znzFile fptr;

  if (argc<=1) {
    fprintf(stderr,"Usage: testprog [filename]\n");
    return 1;
  }
  
  b = 2;
  c = (char *)calloc(1,10);
  strcpy(c,"Three");
  fptr = znzopen(argv[1],"wb",1);
  znzprintf(fptr,"Message 1\n");
  znzprintf(fptr,"Message %d ",b);
  znzprintf(fptr,"Message %d %s\n",b,c);
  znzclose(fptr);

  return 0;
}
