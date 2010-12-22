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

#include "SUMA_suma.h"
#include "SUMA_GeomComp.h"
#include "SUMA_gts.h"

#define SURFPATCH_MAX_SURF 1  /*!< Maximum number of input surfaces */

void usage_SUMA_coarsen (SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"usage_SUMA_coarsen"};
   char * s = NULL, *sio=NULL;
   s = SUMA_help_basics();
   sio = SUMA_help_IO_Args (ps);
   printf ( "\nUsage:\n"
            "  SurfMesh <-i_TYPE SURFACE> <-o_TYPE OUTPUT> <-edges FRAC> \n"
            "           [-sv SURF_VOL]\n"
            " \n"
            "  Example:\n"
            "  SurfMesh -i_ply surf1.ply -o_ply surf1_half -edges 0.5\n"
            "\n"
            "  Mandatory parameters:\n"
            "     -i_TYPE SURFACE: Input surface. See below for details. \n"
            "              You can also use the -t* method or\n"
            "              the -spec SPECFILE -surf SURFACE method.\n"
            "     -o_TYPE OUTPUT: Output surface, see below.\n"  
            "     -edges FRAC: surface will be simplified to number of\n"
            "              edges times FRAC (fraction). Default is .5\n"
            "              refines surface if edges > 1\n"
            "\n"
            "%s"
            "\n"
            "%s"
            "\n",sio,s); SUMA_free(sio); sio = NULL;SUMA_free(s); s = NULL;
   s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
   printf(  " Originally written by Jakub Otwinowski.\n"
            " Now maintained by Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
   printf(  " This program uses the GTS library gts.sf.net\n"
            " for fun read \"Fast and memory efficient polygonal simplification\" (1998) \n"
            " and \"Evaluation of memoryless simplification\" (1999) by Lindstrom and Turk.\n");
   exit (0);
}

/*!
   \brief parse the arguments for SurfSmooth program

   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_coarsen_OPTIONS *) options structure.
               To free it, use
               SUMA_free(Opt->out_prefix);
               SUMA_free(Opt);
*/
SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_coarsen_ParseInput (char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_coarsen_ParseInput"};
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar, i, ind;
   char *outprefix;
   SUMA_Boolean brk = NOPE;
   void *SO_name=NULL;
   SUMA_Boolean exists = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();

   kar = 1;
   Opt->out_prefix = NULL;
   Opt->v0 = .5;
   brk = NOPE;

   while (kar < argc)
   { /* loop accross command ine options */
      /*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0)
      {
         usage_SUMA_coarsen(ps);
         exit (0);
      }

      SUMA_SKIP_COMMON_OPTIONS(brk, kar);

      
      if (!brk && (strcmp(argv[kar], "-edges") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -edges \n");
            exit (1);
         }
         Opt->v0 = atof(argv[++kar]);

         brk = YUP;
      }


      if (!brk && !ps->arg_checked[kar])
      {
         fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
         exit (1);
      } else
      {
         brk = NOPE;
         kar ++;
      }

   }

   /* check for only one surface at input */
   if (ps->s_N_surfnames + ps->i_N_surfnames + ps->t_N_surfnames != 1) {
      SUMA_S_Err("Multiple surface specifications used. Only one surface allowed.");
      exit(1);
   }

   /* write out the surface */
   if (ps->o_N_surfnames) {
      Opt->out_prefix = SUMA_copy_string(ps->o_surfnames[0]);
      Opt->SurfFileType = ps->o_FT[0];
      Opt->SurfFileFormat = ps->o_FF[0];
   } else {
      Opt->out_prefix = SUMA_copy_string("SurfMesh_out");
      Opt->SurfFileType = SUMA_PLY;
      Opt->SurfFileFormat = SUMA_ASCII;
   }
   SO_name = SUMA_Prefix2SurfaceName(Opt->out_prefix, NULL, NULL, Opt->SurfFileType, &exists);
   if (exists) {
      fprintf(SUMA_STDERR,"Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n", FuncName, Opt->out_prefix);
      exit(1);
   }
   /* free SO_name for now */
   if (SO_name) SUMA_free(SO_name); SO_name = NULL;

   
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
   static char FuncName[]={"SurfMesh"};
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int SO_read = -1;
   int i = 0;
   SUMA_SurfaceObject *SO = NULL, *S2 = NULL;
   void *SO_name = NULL;
   SUMA_Boolean exists = NOPE;
   FILE* out = NULL;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int N_Spec=0;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;



   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-s;-sv;-o;");
   
   /* Allocate space for DO structure */
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);

   if (argc < 4)
   {
      usage_SUMA_coarsen(ps);
      exit (1);
   }

   Opt = SUMA_coarsen_ParseInput (argv, argc, ps);

   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec != 1) {
      SUMA_S_Err("Multiple spec at input.");
      exit(1);
   }

   SO = SUMA_Load_Spec_Surf(Spec, 0, ps->sv[0], 0);
   if (!SO) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
      
   }   
   
   S2 = SUMA_Mesh_Resample (SO, Opt->v0);
      
   SO_name = SUMA_Prefix2SurfaceName(Opt->out_prefix, NULL, NULL, Opt->SurfFileType, &exists);
   if (exists) {
      fprintf(SUMA_STDERR,"Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n", FuncName, Opt->out_prefix);
      exit(1);
   }
   
   /* write the surfaces to disk */
   fprintf (SUMA_STDERR,"%s: Writing surface  ...\n", FuncName);
   if (!SUMA_Save_Surface_Object (SO_name, S2, Opt->SurfFileType, Opt->SurfFileFormat, NULL)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
      exit (1);
   }
      

   SUMA_LH("clean up");
   
   if (N_Spec) {
      int k=0; 
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) { SUMA_S_Err("Failed to free spec fields"); } 
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }
   if (SO_name) SUMA_free(SO_name); SO_name = NULL;
   if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
   if (S2) SUMA_Free_Surface_Object(S2); S2 = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Spec) SUMA_free(Spec); Spec = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   
   if (!SUMA_Free_CommonFields(SUMAg_CF)) {SUMA_SL_Err("SUMAg_CF Cleanup Failed!");}

   SUMA_RETURN(0);
}
