//------------------------------------------------
// MarchingCubes
//------------------------------------------------
//
// MarchingCubes Command Line interface
// Version 0.2 - 12/08/2002
//
// Thomas Lewiner thomas.lewiner@polytechnique.org
// Math Dept, PUC-Rio
//
// Translated to C by Ziad S. Saad November 30/04
//________________________________________________


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "MarchingCubes.h"

static char Obj_Types[][10] = { {"Cushin"}, {"Sphere"}, {"Plane"}, {"Cassini"}, {"Blooby"}, {"Chair"}, {"Cyclide"}, {"2 Torus"}, {"mc case"}, {"Drip"} };
void mc_help(void)
{
   int i;
   printf(  "Creates an isosurface for a predetermined set of objects\n"
            "or a 1D volume of choice. Isosurface is the envelope of\n"
            "volumetric region = 0.\n"
            "If region is spherical of radius 5, isosurface would be\n"
            " x^2+y^2+z^2-r^2 = 0\n"
            "Usage 1: mc <obj_type>\n"
            "           obj_type is a number between 0 and 9\n");
   for (i=0; i<10;++i) {
   printf(  "           %d: %s\n", i, Obj_Types[i]);   
   }
   printf(  "Usage 2: mc <Vol.1D> <Res> \n"
            "           Vol.1D is the (Res x Res x Res) volume in 1D format.\n"
            "           You can't use a binary file where you have 0 and 1,\n"
            "           like in a segmented volume, you need to create a gradient\n"
            "           over zero. For example if you have a mask with 0 outside \n"
            "           the brain and 1 inside, turn all 0s into -1.\n"
            "           Say Anat is a 128x128x128 volume for example:\n"
            "           3dAutomask -prefix Mask Anat+orig.            \n"
            "           3dcalc -datum float -a Mask+orig. -expr '( -1*(1-bool(a))+bool(a) )' -prefix GradMask\n"
            "           3dmaskdump GradMask+orig. > GradMask.1D\n"
            "           mc GradMask.1D 128\n"
            "           quickspec -tn 1D testNodes.1D testFaces.1D\n"
            "           suma -spec quic.spec\n"
            "\n" );
            
}
/* usage:
   mc 
*/
//_____________________________________________________________________________
// main function
int main (int argc, char **argv)
//-----------------------------------------------------------------------------
{
  MCB *mcp ;
  int obj_type;
  int Res = 256;
  char *fname = NULL;
  
  if (argc == 1) {
   /* help */
   mc_help();
   exit(1);
  }else if (argc == 2) {
   if (!strcmp(argv[1], "-h") || !strcmp(argv[1], "-help")) {
      mc_help(); exit(0);
   }
   Res = 60 ;
   obj_type = atoi(argv[1]);
   if (obj_type < 0 || obj_type > 9) {
      fprintf(stderr,"Bad object type (value between 0 and 9).\n");
      exit(1);
   }
  }else if (argc == 3){
   if (argc != 3){
      fprintf(stderr,"Usage: mc 1Dname nvoxel\n\n1D file is the output of 3dmaskdump volume is assumed cubic [nvoxel * nvoxel *nvoxel]for now...\n");
      exit(1);
   }
   fname = argv[1];
   Res = atoi(argv[2]) ;
  }else {
   fprintf(stderr,"Bad usage.\n");
   exit(1);
  }
  
  mcp = MarchingCubes(-1, -1, -1);
  set_resolution( mcp, Res, Res, Res ) ;

  init_all(mcp) ;
  if (argc == 2) {
   printf("Creating %s...\n", Obj_Types[obj_type]);
   compute_data( *mcp , obj_type) ;
  }else if (argc == 3) {
   z_compute_data( *mcp , fname) ;
  } 
  run(mcp) ;
  clean_temps(mcp) ;

  #if 0
  writePLY(mcp, "test.ply", false) ;
  #endif
  write1Dmcb(mcp);
  clean_all(mcp) ;
   free(mcp);
  return 0 ;
}


