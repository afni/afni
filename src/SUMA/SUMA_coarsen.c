/***************************************************************************
                          coarsen2.c  -  description
                             -------------------
    begin                : Fri Jun 18 2004
    copyright            : (C) 2004 by jakub
    email                : jakub@hurin
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "SUMA_suma.h"
#include "SUMA_GeomComp.h"
#include "SUMA_gts.h"

#define SURFPATCH_MAX_SURF 1  /*!< Maximum number of input surfaces */

/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_SUMA_getPatch ()
{
   static char FuncName[]={"usage_SUMA_getPatch"};
   char * s = NULL;
   s = SUMA_help_basics();
   printf ( "\nUsage:\n"
            "  SurfMesh -spec SPECFILE -surf SURFNAME \n"
            "\n"
            "  Mandatory parameters:\n"
            "     -spec SpecFile: Spec file containing input surfaces.\n"
            "     -surf SURFNAME: Name of input surface \n"
            "     -edges fraction: surface will be simplified to number of\n"
            "              edges times this fraction. Default is .5\n"
            "              refines surface if edges > 1\n"
            "  outputs files testNodes.1D and testFaces.1D\n"
            "\n"
            "%s"
            "\n",s); SUMA_free(s); s = NULL;
   s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
   printf(" blame Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
   printf(" this program uses the GTS library gts.sf.net\n"
          " for fun read \"Fast and memory efficient polygonal simplification\" (1998) \n"
          " and \"Evaluation of memoryless simplification\" (1999) by Lindstrom and Turk.\n");
   exit (0);
}

typedef struct {
   SUMA_SO_File_Type iType;
   char *out_prefix;
   char *sv_name;
   char *surf_names[SURFPATCH_MAX_SURF];
   int N_surf;
   char *spec_file;
   char *in_name;
   int minhits;
   int thislabel;
   int labelcol;
   int nodecol;
   float edges;
} SUMA_KUBATEST_OPTIONS;

/*!
   \brief parse the arguments for SurfSmooth program

   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_GETPATCH_OPTIONS *) options structure.
               To free it, use
               SUMA_free(Opt->out_prefix);
               SUMA_free(Opt);
*/
SUMA_KUBATEST_OPTIONS *SUMA_GetPatch_ParseInput (char *argv[], int argc)
{
   static char FuncName[]={"SUMA_GetPatch_ParseInput"};
   SUMA_KUBATEST_OPTIONS *Opt=NULL;
   int kar, i, ind;
   char *outprefix;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   Opt = (SUMA_KUBATEST_OPTIONS *)SUMA_malloc(sizeof(SUMA_KUBATEST_OPTIONS));

   kar = 1;
   Opt->iType = SUMA_FT_NOT_SPECIFIED;
   Opt->out_prefix = NULL;
   Opt->sv_name = NULL;
   Opt->spec_file = NULL;
   Opt->in_name = NULL;
   Opt->minhits = 2;
   Opt->labelcol = -1;
   Opt->nodecol = -1;
   Opt->thislabel = -1;
   Opt->N_surf = -1;
   for (i=0; i<SURFPATCH_MAX_SURF; ++i)
      Opt->surf_names[i] = NULL;
   Opt->edges = .5;
   brk = NOPE;

   while (kar < argc)
   { /* loop accross command ine options */
      /*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0)
      {
         usage_SUMA_getPatch();
         exit (0);
      }

      SUMA_SKIP_COMMON_OPTIONS(brk, kar);

      if (!brk && (strcmp(argv[kar], "-spec") == 0))
      {
         kar ++;
         if (kar >= argc)
         {
            fprintf (SUMA_STDERR, "need argument after -spec \n");
            exit (1);
         }
         Opt->spec_file = argv[kar];
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-surf") == 0))
      {
         if (kar + 1>= argc)
         {
            fprintf (SUMA_STDERR, "need argument after -surf SURF_NAME \n");
            exit (1);
         }
         /*ind = argv[kar][6] - 'A';
         if (ind < 0 || ind >= SURFPATCH_MAX_SURF)
         {
            fprintf (SUMA_STDERR, "you can use only one surface.\n");
            exit (1);
         }*/
         kar ++;
         Opt->surf_names[0] = argv[kar];
         Opt->N_surf = 1;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-edges") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -edges \n");
            exit (1);
         }
         Opt->edges = atof(argv[++kar]);
         /*if (Opt->edges > 1.0)
         {
            fprintf (SUMA_STDERR,"Error %s:\nfraction shouldn't be greater than 1, duh.\n", FuncName);
            exit (1);
         }*/

         brk = YUP;
      }


      if (!brk)
      {
         fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
         exit (1);
      } else
      {
         brk = NOPE;
         kar ++;
      }

   }

   /* sanity checks */
   if (Opt->N_surf != 1)
   {
      SUMA_SL_Err("No surface specified.");
      exit(1);
   }
   if (Opt->spec_file == NULL) 
   {
      SUMA_SL_Err("No spec file specified.");
      exit(1);
   }
   SUMA_RETURN (Opt);

}

double distance(SUMA_SurfaceObject *SO, int n, int m)
{
   int i;
   double result = 0;
   for (i = 0; i<3; i++)
      result += pow(SO->NodeList[n*3+i] - SO->NodeList[m*3+i], 2);
   return sqrt(result);
}


int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"iotest"};
   SUMA_KUBATEST_OPTIONS *Opt;
   int SO_read = -1;
   int i = 0;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_SurfSpecFile Spec;
   void *SO_name = NULL;
   SUMA_Boolean LocalHead = NOPE;
   FILE* out = NULL;

   SUMA_mainENTRY;

   SUMA_STANDALONE_INIT;


   /* Allocate space for DO structure */
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);

   if (argc < 4)
   {
      usage_SUMA_getPatch();
      exit (1);
   }

   Opt = SUMA_GetPatch_ParseInput (argv, argc);

   /* read all surfaces */
   if (!SUMA_Read_SpecFile (Opt->spec_file, &Spec)) {
      fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
      exit(1);
   }
   SO_read = SUMA_spec_select_surfs(&Spec, Opt->surf_names, SURFPATCH_MAX_SURF, 0);
   if ( SO_read != Opt->N_surf )
   {
      if (SO_read >=0 )
      fprintf(SUMA_STDERR,"Error %s:\nFound %d surfaces, expected %d.\n", FuncName,  SO_read, Opt->N_surf);
   exit(1);
   }
   /* now read into SUMAg_DOv */
   if (!SUMA_LoadSpec_eng(&Spec, SUMAg_DOv, &SUMAg_N_DOv, Opt->sv_name, 0, SUMAg_CF->DsetList) ) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec_eng\n", FuncName);
      exit(1);
   }

      /* now identify surface needed */
      SO = SUMA_find_named_SOp_inDOv(Opt->surf_names[0], SUMAg_DOv, SUMAg_N_DOv);
      if (!SO)
      {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface %s\n"
                              "in spec file. Use full name.\n",
                              FuncName, Opt->surf_names[0]);
         exit(1);
      }
        //SUMA_SurfaceMetrics(SO, "Convexity, EdgeList, MemberFace", NULL);


      //try to simplify mesh
      GtsSurface* s  = SumaToGts(SO);
      //FILE *f = NULL;
      //f = fopen("gts_test.gts", "w");
      if (Opt->edges < 1)
         coarsen(s, SO->EL->N_Distinct_Edges * Opt->edges);
      else
         refine(s, SO->EL->N_Distinct_Edges * Opt->edges);
      // gts_surface_print_stats(s, f);

      //gts_surface_write(s, f);
      // fclose(f);

      SUMA_SurfaceObject* S2 = GtsToSuma(s);
      
      out = fopen("testNodes.1D", "w");      
      for (i=0; i < S2->N_Node*3; i+=3)
         fprintf(out, "%f\t%f\t%f\n",
         S2->NodeList[i],
         S2->NodeList[i+1],
         S2->NodeList[i+2]);
      fclose(out);
      out = fopen("testFaces.1D", "w");
      for (i=0; i < S2->N_FaceSet*3; i+=3)
         fprintf(out, "%i\t%i\t%i\n",
         S2->FaceSetList[i],
         S2->FaceSetList[i+1],
         S2->FaceSetList[i+2]);
      fclose(out);
      SUMA_free (S2);
      //SUMA_Free_Surface_Object (S2);
      gts_object_destroy((GtsObject*)s);

   SUMA_LH("clean up");
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt) SUMA_free(Opt);
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }

   if (!SUMA_Free_CommonFields(SUMAg_CF)) {SUMA_SL_Err("SUMAg_CF Cleanup Failed!");}

   SUMA_RETURN(0);
}
