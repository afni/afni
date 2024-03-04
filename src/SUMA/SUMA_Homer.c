#include "SUMA_suma.h"

#include "SUMA_Homer.h"


#if 1

void usage_SUMA_Homer()
{
   printf ("\nUsage:  SUMA_Homer\n");
   exit (1);
}


int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_Homer"};
   float *NodeList = NULL;
   int N_Node = 0, N_FaceSet = 0,
      N_parts = 0, ipart=0;
   int *FaceSetList = NULL;
   char sbuf[100], fName[100];
   SUMA_SURF_NORM SN;
   SUMA_OVERLAYS *NewColPlane=NULL;
   SUMA_SurfaceObject **SOv=NULL;
   FILE *SpecOut = NULL;
   SUMA_Boolean LocalHead = NOPE;

	SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;



   N_parts = 19;
   SOv = (SUMA_SurfaceObject **)
      SUMA_malloc(N_parts * sizeof(SUMA_SurfaceObject *));

   if( ! THD_is_directory("Springfield") ){
      if( mkdir( "Springfield" , THD_MKDIR_MODE ) != 0 ){
         SUMA_S_Err("Failed to create Springfield");
         exit(1);
      }
   }

   SpecOut = fopen("HJS.spec", "w");
   if (!SpecOut) {
      fprintf(SUMA_STDERR,"Error %s: Failed in opening spec file.\n", FuncName);
		exit(1);
   }

   fprintf (SpecOut,"\tGroup = HJS\n");
   fprintf (SpecOut,"\tStateDef = Duffed\n");

   for (ipart = 0; ipart < N_parts; ++ipart) {
      SOv[ipart] = SUMA_HJS_Surface(ipart);
      if (!SOv[ipart]) {
         SUMA_S_Err("Failed to get part");
         exit(1);
      }
      sprintf (fName, "Springfield/HomerJaySimpson_%d", ipart);
      SOv[ipart]->Group = SUMA_copy_string("HJS");
      SOv[ipart]->State = SUMA_copy_string("Duffed");
      SOv[ipart]->Name = SUMA_StripPath(fName);
      SOv[ipart]->FileType = SUMA_PLY;
      SOv[ipart]->FileFormat = SUMA_FF_NOT_SPECIFIED;
      SOv[ipart]->idcode_str = UNIQ_hashcode(fName);
      SOv[ipart]->SUMA_VolPar_Aligned = NOPE;
      SOv[ipart]->VolPar = NULL;
      SOv[ipart]->NodeDim = 3;
      SOv[ipart]->FaceSetDim = 3;

      SUMA_MIN_MAX_SUM_VECMAT_COL (
         SOv[ipart]->NodeList, SOv[ipart]->N_Node, SOv[ipart]->NodeDim,
         SOv[ipart]->MinDims, SOv[ipart]->MaxDims, SOv[ipart]->Center);

      SOv[ipart]->Center[0] /= SOv[ipart]->N_Node;
      SOv[ipart]->Center[1] /= SOv[ipart]->N_Node;
      SOv[ipart]->Center[2] /= SOv[ipart]->N_Node;

      SUMA_MIN_VEC (SOv[ipart]->MinDims, 3, SOv[ipart]->aMinDims );
      SUMA_MAX_VEC (SOv[ipart]->MaxDims, 3, SOv[ipart]->aMaxDims);

      /* Calculate SurfaceNormals */
      SN = SUMA_SurfNorm(SOv[ipart]->NodeList,  SOv[ipart]->N_Node,
                  SOv[ipart]->FaceSetList, SOv[ipart]->N_FaceSet );
      SOv[ipart]->NodeNormList = SN.NodeNormList;
      SOv[ipart]->FaceNormList = SN.FaceNormList;



      /* Write out the surfaces in PLY format and create a dummy spec file */
      if (!SUMA_Save_Surface_Object (  fName, SOv[ipart],
                                       SUMA_PLY, SUMA_FF_NOT_SPECIFIED, NULL)) {
         fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n",
                              FuncName);
         exit (1);
      }

      fprintf (SpecOut,"NewSurface\n");
      fprintf (SpecOut, "\tSurfaceFormat = ASCII\n");
      fprintf (SpecOut, "\tSurfaceType = Ply\n");
      fprintf (SpecOut, "\tSurfaceName = %s\n", fName);
      fprintf (SpecOut, "\tMappingRef = SAME\n");
      fprintf (SpecOut, "\tSurfaceState = %s\n", SOv[ipart]->State);
      fprintf (SpecOut, "\tEmbedDimension = 3\n\n");


   }
   if (SpecOut) fclose (SpecOut);
   SUMA_RETURN(0);
}
#endif
