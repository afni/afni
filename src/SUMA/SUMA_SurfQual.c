#include "SUMA_suma.h"

#define SURFQUAL_MAX_SURF 10  /*!< Maximum number of input surfaces */

void usage_SUMA_SurfQual (SUMA_GENERIC_ARGV_PARSE *ps, int detail)
   {
      static char FuncName[]={"usage_SUMA_SurfQual"};
      char * s = NULL, *sio=NULL;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( 
"\nUsage: A program to check the quality of surfaces.\n"
"  SurfQual <-spec SpecFile> <-surf_A insurf> <-surf_B insurf> ...\n"
"             <-sphere> [-self_intersect] [-prefix OUTPREF]  \n"
"\n%s", detail ? "":"use -h or -help for more help detail.\n");
   if (detail) {
      printf ( 
"  Mandatory parameters:\n"
"     -spec SpecFile: Spec file containing input surfaces.\n"
"     -surf_X: Name of input surface X where X is a character\n"
"              from A to Z. If surfaces are specified using two\n"
"              files, use the name of the node coordinate file.\n"
"  Mesh winding consistency and 2-manifold checks are performed\n"
"  on all surfaces.\n"
"  Optional parameters:\n"
"     -summary: Provide summary of results to stdout\n"
"     -self_intersect: Check if surface is self intersecting.\n"
"                      This option is rather slow, so be patient.\n"
"                      In the presence of intersections, the output file\n"
"                      OUTPREF_IntersNodes.1D.dset will contain the indices\n"
"                      of nodes forming segments that intersect the surface.\n"
"  Most other checks are specific to spherical surfaces (see option below).\n"
"     -sphere: Indicates that surfaces read are spherical.\n"
"              With this option you get the following output.\n"
"              - Absolute deviation between the distance (d) of each\n"
"                node from the surface's center and the estimated\n"
"                radius(r). The distances, abs (d - r), are \n"
"                and written to the file OUTPREF_Dist.1D.dset .\n"
"                The first column represents node index and the \n"
"                second is the absolute distance. A colorized \n"
"                version of the distances is written to the file \n"
"                OUTPREF_Dist.1D.col (node index followed \n"
"                by r g b values). A list of the 10 largest absolute\n"
"                distances is also output to the screen.\n"
"              - Also computed is the cosine of the angle between \n"
"                the normal at a node and the direction vector formed\n"
"                formed by the center and that node. Since both vectors\n"
"                are normalized, the cosine of the angle is the dot product.\n"
"                On a sphere, the abs(dot product) should be 1 or pretty \n"
"                close. Nodes where abs(dot product) < 0.9 are flagged as\n"
"                bad and written out to the file OUTPREF_BadNodes.1D.dset .\n"
"                The file OUTPREF_dotprod.1D.dset contains the dot product \n"
"                values for all the nodes. The files with colorized results\n"
"                are OUTPREF_BadNodes.1D.col and OUTPREF_dotprod.1D.col .\n"
"                A list of the bad nodes is also output to the screen for\n"
"                convenience. You can use the 'j' option in SUMA to have\n"
"                the cross-hair go to a particular node. Use 'Alt+l' to\n"
"                have the surface rotate and place the cross-hair at the\n"
"                center of your screen.\n"
"              NOTE: For detecting topological problems with spherical\n"
"                surfaces, I find the dot product method to work best.\n"              
"  Optional parameters:\n"
"     -prefix OUTPREF: Prefix of output files. If more than one surface\n"
"                      are entered, then the prefix will have _X added\n"
"                      to it, where X is a character from A to Z.\n"
"                      THIS PROGRAM WILL OVERWRITE EXISTING FILES.\n"
"                      Default prefix is the surface's label.\n"
"\n"
"  Comments:\n"
"     - The colorized (.col) files can be loaded into SUMA (with the 'c' \n"
"     option. By focusing on the bright spots, you can find trouble spots\n"
"     which would otherwise be very difficult to locate.\n"
"     - You should also pay attention to the messages output when the \n"
"     surfaces are being loaded, particularly to edges (segments that \n"
"     join 2 nodes) are shared by more than 2 triangles. For a proper\n"
"     closed surface, every segment should be shared by 2 triangles. \n"
"     For cut surfaces, segments belonging to 1 triangle only form\n"
"     the edge of that surface.\n"
"     - There are no utilities within SUMA to correct these defects.\n"
"     It is best to fix these problems with the surface creation\n"
"     software you are using.\n"
"     - Some warnings may be redundant. That should not hurt you.\n"
"%s"
"\n"
"%s"
"\n", (detail > 1) ? sio : "Use -help for I/O and miscellaneous options." , 
      (detail > 1) ? s : "");
   }
    if (sio) SUMA_free(sio); s = NULL;        
    if (s) SUMA_free(s); s = NULL;        
    if (detail) {
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
    }
    return;
}

typedef struct {
   char *out_prefix;
   char *surftype;
   int self_intersect;
   int DoSum;
} SUMA_SURFQUAL_OPTIONS;

/*!
   \brief parse the arguments for SurfQual program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_SURFQUAL_OPTIONS *) options structure.
               To free it, use 
               SUMA_free(Opt->out_prefix); 
               SUMA_free(Opt);
*/
SUMA_SURFQUAL_OPTIONS *SUMA_SurfQual_ParseInput (char *argv[], int argc, 
                                                 SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SurfQual_ParseInput"}; 
   SUMA_SURFQUAL_OPTIONS *Opt=NULL;
   int kar, i, ind;
   char *outprefix;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_SURFQUAL_OPTIONS *)SUMA_malloc(sizeof(SUMA_SURFQUAL_OPTIONS));

   kar = 1;
   Opt->out_prefix = NULL;
   Opt->surftype = NULL;
   Opt->self_intersect = 0;
   Opt->DoSum = 0;
   
   brk = NOPE;
   
   while (kar < argc) { /* loop accross command ine options */
      /*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
          usage_SUMA_SurfQual(ps, strlen(argv[kar]) > 3 ? 2:1);
          exit (0);
      }
      
      /* skip the options parsed in SUMA_ParseInput_basics_s */
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-sphere") == 0)) {
         if (Opt->surftype) {
            SUMA_S_Err( "Surface type already specified.\n"
                        "Only one type allowed.");
            exit(1);
         }
         Opt->surftype = argv[kar];
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-self_intersect") == 0)) {
         Opt->self_intersect = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-summary") == 0)) {
         Opt->DoSum = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -prefix \n");
            exit (1);
         }
         Opt->out_prefix = SUMA_copy_string(argv[kar]);
         brk = YUP;
      }
            
      if (!brk && !ps->arg_checked[kar]) {
         fprintf (SUMA_STDERR,
                  "Error %s:\nOption %s not understood. Try -help for usage\n", 
                  FuncName, argv[kar]);
         suggest_best_prog_option(argv[0], argv[kar]);
         exit (1);
      } else { 
         brk = NOPE;
         kar ++;
      }
      
   }
   
#if 0
   if (Spec->N_Surfs < 1) {
      SUMA_SL_Err("No surface specified.");
      exit(1);
   }
#endif      
   SUMA_RETURN (Opt);
     
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfQual"};
   char *OutName = NULL, ext[5], *prefix = NULL, *shist=NULL;
   SUMA_SURFQUAL_OPTIONS *Opt; 
   int SO_read = -1;
   int i, cnt, trouble, consistent = -1, eu = -1, nsi = -1, N_Spec=0;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_SurfSpecFile *Spec=NULL; 
   void *SO_name = NULL;
   SUMA_Boolean DoConv = NOPE, DoSphQ = NOPE, DoSelfInt = NOPE;   
   int N_bad_nodes, N_bad_facesets;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-s;-sv;");
      
   /* Allocate space for DO structure */
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   
   Opt = SUMA_SurfQual_ParseInput (argv, argc, ps);
   if (argc < 2)
    {
       SUMA_S_Err("Too few options");
       usage_SUMA_SurfQual(ps, 0);
       exit (1);
    }

   /* read all surfaces */
   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
      exit(1);
   }

   if (N_Spec > 1 ) {
      SUMA_S_Err( "Mike, you cannot mix -spec with -i or -t options "
                  "for specifying surfaces.");
      exit(1);
   }
   
   if (Spec->N_Surfs < 1) {
      SUMA_S_Err("No surfaces");
      exit(1);
   }
     
   if (Opt->self_intersect) DoSelfInt = YUP;

   DoConv = NOPE;
   DoSphQ = NOPE;   
   if (Opt->surftype) {
      if (!strcmp(Opt->surftype, "-sphere")) { 
         DoSphQ = YUP;
      }else {
         /* Don't complain anymore, maybe winding checking is all users need */
      }
   }
   
   for (i=0; i < Spec->N_Surfs; ++i) {/* loop to read in surfaces */
      /* now identify surface needed */
      if (!(SO = SUMA_Load_Spec_Surf_with_Metrics(Spec, i, ps->sv[0], 0))) {
         SUMA_S_Err("Failed to load surface .\n");
         exit(1);
      }
      fprintf(SUMA_STDERR,"\nReport for Surface %s\n", SO->Label);     
      /* do the quality thing based on the Opt->surftype */
      if (!Opt->out_prefix) prefix = SUMA_copy_string(SO->Label);
      else prefix = SUMA_copy_string (Opt->out_prefix);
      
      /* check the winding */
      if (!SUMA_MakeConsistent (SO->FaceSetList, SO->N_FaceSet, 
                                SO->EL, 0, &trouble)) {
         SUMA_S_Warn(
            "Failed to make sure surface's mesh is consistently wound.\n"
            "You should fix the mesh.\n");
         consistent = 0;
      } 
      
      {
         int iii=0, isbad=0, ht0;
         int *badedge=(int*)SUMA_calloc(SO->N_Node, sizeof(int));
         /* check on troubled edges */
         while (iii < SO->EL->N_EL) {
            ht0 = SO->EL->ELps[iii][1];
            /* make sure edge is not part of three triangles, if it is,skip it*/
            if (SO->EL->ELps[iii][2] > 2) {
               ++iii;
               fprintf( SUMA_STDERR, 
                        "%s: Bad edge (#%d: %d--%d), \n"
                        " part of more than 2 triangles, skip it\n", 
                        FuncName, i, SO->EL->EL[iii][0], SO->EL->EL[iii][1]); 
               ++badedge[SO->EL->EL[iii][0]];
               ++badedge[SO->EL->EL[iii][1]];
               isbad = 1;
               continue;
            }
            ++iii;
         }
         if (isbad) {
            if (Spec->N_Surfs > 1) {
            sprintf(ext,"_%c", 65+i);
            OutName = SUMA_append_replace_string (
                           prefix, 
                           "_BadEdgeNodes.1D.dset", 
                           ext, 0);
            } else { 
               OutName = SUMA_append_string (prefix, "_BadEdgeNodes.1D.dset");
            }
            SUMA_WRITE_ARRAY_1D(badedge,SO->N_Node,1,OutName);
            if (OutName) SUMA_free(OutName); OutName = NULL;
         }
         SUMA_free(badedge); badedge = NULL;
      } 
      
      if (DoConv) {
         float *Cx = NULL;
         if (Spec->N_Surfs > 1) {
            sprintf(ext,"_%c", 65+i);
            OutName = SUMA_append_replace_string 
                           (prefix, "_Conv_detail.1D.dset", ext, 0);
         } else { 
            OutName = SUMA_append_string (prefix, "_Conv_detail.1D.dset");
         }
         Cx = SUMA_Convexity_Engine ( SO->NodeList, SO->N_Node, 
                                      SO->NodeNormList, SO->FN, OutName, NULL);
         if (Cx) SUMA_free(Cx); Cx = NULL;
         if (OutName) SUMA_free(OutName); OutName = NULL;
      } 
      
      if (DoSphQ) {
         if (Spec->N_Surfs > 1) {
            sprintf(ext,"_%c", 65+i);
            OutName = SUMA_append_string (prefix, ext);
         } else { 
            OutName = SUMA_copy_string (prefix);
         }
         shist = SUMA_HistString (NULL, argc, argv, NULL);
         SUMA_SphereQuality (SO, OutName, shist, &N_bad_nodes, &N_bad_facesets);   
         if (shist) SUMA_free(shist); shist = NULL;
         if (OutName) SUMA_free(OutName); OutName = NULL;
      }
      
      if (trouble) { /* put winding problem here to make it visible */
         fprintf (SUMA_STDERR,"\n");
         SUMA_S_Warn(
            "Mesh is not consistent, use ConvertSurface's -make_consistent \n"
            "option to fix the problem before proceeding further.\n"
            "Other results reported by this and other programs\n"
            "may be incorrect if mesh is not consistently wound.\n" ); 
         consistent = 0;
      } else {
         consistent = 1;
         fprintf (SUMA_STDERR,"\n");
         fprintf (SUMA_STDERR,"Surface is consistently wound\n");
      }
      { 
         SUMA_EULER_SO(SO, eu);
         fprintf (SUMA_STDERR,"\n");
         fprintf(SUMA_STDERR,"Surface Euler Characteristic is: %d\n", eu);
      }
      if ((SO->EL->min_N_Hosts == 1 || SO->EL->max_N_Hosts == 1)) {
            fprintf (SUMA_STDERR,"\n");
            fprintf(SUMA_STDERR,
                    "Warning %s:\n"
                    " Min/Max number of edge hosting triangles: [%d/%d] \n", 
                    FuncName, SO->EL->min_N_Hosts, SO->EL->max_N_Hosts);
            fprintf( SUMA_STDERR,
                     " You have edges that form a border in the surface.\n");
      }
      if (SO->EL->min_N_Hosts == 2 && SO->EL->max_N_Hosts == 2) {
         fprintf (SUMA_STDERR,"\n");
         fprintf(SUMA_STDERR,"Surface is closed and is a 2-manifold.");
      }
      if (SO->EL->min_N_Hosts > 2 || SO->EL->max_N_Hosts > 2) {
         fprintf (SUMA_STDERR,"\n");
         fprintf( SUMA_STDERR, 
                  "Warning %s:\n"
                  "Min/Max number of edge hosting triangles: [%d/%d] \n", 
                  FuncName, SO->EL->min_N_Hosts, SO->EL->max_N_Hosts);
         fprintf(SUMA_STDERR, 
            "Warning %s:\n"
            " You have edges that belong to more than two triangles.\n"
            " Bad for analysis assuming surface is a 2-manifold.\n", 
            FuncName);
         if (1) {
            int iii=0;
            fprintf( SUMA_STDERR, 
                     " These edges are formed by the following nodes:\n");
            for (iii = 0; iii < SO->EL->N_EL; ++iii) { 
               if (SO->EL->ELps[iii][2] > 2) 
                  fprintf (SUMA_STDERR,
                           " %d: Edge [%d %d] shared by %d triangles.\n", 
                           iii+1, SO->EL->EL[iii][0], SO->EL->EL[iii][1] , 
                           SO->EL->ELps[iii][2] );
            }
         }
      }

      if (DoSelfInt) {
         int iii;
         FILE *fout=NULL;
         byte *report = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte));
         if (!report) {
            SUMA_SL_Crit("Failed to allocate for report");
            report = NULL;
         }  
         fprintf( SUMA_STDERR, "\n\nChecking for intersections...:\n");
         nsi = SUMA_isSelfIntersect(SO, 500, report);
         if (nsi) {
            fprintf( SUMA_STDERR, 
                     " Surface is self intersecting.\n"
                     "%d segments were found to intersect the surface.\n", nsi);
            if (nsi >= 500) {
               fprintf( SUMA_STDERR, 
                        " It is possible that you have additional segments"
                        " intersecting the surface.\n");
            }
            if (report) {
               if (Spec->N_Surfs > 1) {
                  sprintf(ext,"_%c", 65+i);
                  OutName = SUMA_append_replace_string ( prefix, 
                                                         "_IntersNodes.1D.dset", 
                                                         ext, 0);
               } else { 
                  OutName = SUMA_append_string (prefix, "_IntersNodes.1D.dset");
               }
               fout = fopen(OutName, "w");
               if (fout) {
                  fprintf(fout,  
                     "#List of nodes that are part of segments which intersect "
                     "the surface\n"
                     "#%s\n"
                     "#A total of %d segments (search limit is 500) were found to "
                     "intersect the surface.\n"
                     "#Col.1 : Node index\n"
                     "#Col.2 : Dummy flag, always 1\n", 
                           SUMA_CHECK_NULL_STR(SO->Label), nsi );
                  for (iii=0; iii<SO->N_Node; ++iii) 
                     if (report[iii]) fprintf(fout, "%d\t1\n", iii);
                  fclose(fout); fout = NULL;
               } else {
                  SUMA_SL_Err("Failed to open file for output.");
               }
               if (OutName) SUMA_free(OutName);
            }         
         }else {
            fprintf(SUMA_STDERR, " Surface is not self intersecting.\n");
         }   
         if (report) SUMA_free(report); report = NULL;
      }

      fprintf (SUMA_STDERR,"\n");

      if (Opt->DoSum) {   /* do not change syntax, scripts depend on this */
                  fprintf(stdout,"Summary for %s:\n", SO->Label);
                  fprintf(stdout,"Euler_Charac. %d\n", eu);
                  fprintf(stdout,"Consistent_Winding %d\n", consistent);
         if (DoSphQ) {
                  fprintf(stdout,"Folding_Triangles %d\n", N_bad_facesets);
                  fprintf(stdout,"Sketchy_nodes %d\n", N_bad_nodes);
                     }
         if (DoSelfInt) 
                  fprintf(stdout,"Self_Intersections %d\n", nsi);
                  fprintf(stdout,"\n");
      }
      
   }
   
  
   
   SUMA_LH("clean up");
   if (!SUMA_FreeSpecFields(Spec)) { SUMA_S_Err("Failed to free spec fields"); }
   SUMA_free(Spec); Spec = NULL;
   if (prefix) SUMA_free(prefix); prefix = NULL;
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt) SUMA_free(Opt);   
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   SUMA_RETURN(0);
} 
