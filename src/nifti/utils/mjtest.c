#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "znzlib.h"

void print_mat(float stdmat[4][4])
{
    int i, j;
    for (i=0; i<4; i++) {
	for (j=0; j<4; j++) {
	    fprintf(stderr,"%f  ",stdmat[i][j]);
	}
	fprintf(stderr,"\n");
    }
    stdmat[0][0]=1;
}


int main(int argc, char *argv[]) 
{
  int b, withgz=0;
  znzFile fp;
  char msg[500];

/*  
  int i,j;
  float stdmat[4][4];
  for (i=0; i<4; i++) {
      for (j=0; j<4; j++) {
	  stdmat[i][j]=0;
      }
      stdmat[i][i]=2*i - 3;
  }

  print_mat(stdmat);
  print_mat(stdmat);

  return 0;
*/

  if (argc<2) {
    printf("Usage: %s <filename> [gz]\n",argv[0]);
    return 1;
  }


  if (argc>=3) {
    if (strcmp("gz",argv[2])==0) {
      withgz=1;
    }  
  }

  printf("withgz = %d\n",withgz);

  /*
  fp = znzopen(argv[1],"rb",withgz);
  if (znz_isnull(fp)) {
    printf("Error in opening file!\n");
    return 2;
  }
  b=znzread(msg,1,20,fp);

  msg[20]=0;
  znzclose(fp);
  */

  /* strcpy(msg,"you know"); */
  msg[0]='y';
  msg[1]='o';
  msg[2]='u';
  msg[3]='\0';
  b=47;

  fp = znzopen(argv[1],"wb",withgz);
  printf("And (%s) everywhere that Mary went\nThe (%d) lamb was sure to go\n",msg,b);
  fprintf(stdout,"And (%s) everywhere that Mary went\nThe (%d) lamb was sure to go\n",msg,b);
  znzprintf(fp,"And (%s) everywhere that Mary went\nThe (%d) lamb was sure to go\n",msg,b);

  znzclose(fp);
  return 0;
}
