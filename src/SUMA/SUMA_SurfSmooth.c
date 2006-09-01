/*USE This sample to start writing standalone programs.
Change SurfSmooth to the program name of your choosing.
*/
#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */


void usage_SUMA_SurfSmooth ()
   {
      static char FuncName[]={"usage_SUMA_SurfSmooth"};
      char * s = NULL, *st = NULL;
      s = SUMA_help_basics();
      st = SUMA_help_talk();
      printf ("\nUsage:  SurfSmooth <-spec SpecFile> <-surf_A insurf> <-met method> \n"
              "\n"
              "   Some methods require additional options detailed below.\n"
              "   I recommend using the -talk_suma option to watch the \n"
              "   progression of the smoothing in real-time in suma.\n"
              "\n"
              "   Method specific options:\n"
              "      HEAT: <-input inData.1D> <-fwhm F> <-Niter N>  \n"
              "            This method is used to filter data\n"
              "            on the surface. It is faster and more stable than\n"
              "            the older LB_FEM below.\n"
              "      LB_FEM: <-input inData.1D> <-fwhm f>\n"
              "              This method is used to filter data\n"
              "              on the surface.\n"
              "      LM: [-kpb k] [-lm l m] [-surf_out surfname] [-iw weights]\n"
              "          This method is used to filter the surface's\n"
              "          geometry (node coordinates).\n"
              "      NN_geom: smooth by averaging coordinates of \n"
              "               nearest neighbors.\n"
              "               This method causes shrinkage of surface\n"
              "               and is meant for test purposes only.\n"
              "\n"
              "   Common options:\n"
              "      [-Niter N] [-output out.1D] [-h/-help] [-dbg_n node]\n"
              "      [-add_index] [-ni_text|-ni_binary] [-talk_suma]\n\n"
              "\n"
              "   Detailed usage:\n"
              "      -spec SpecFile: Name of specfile containing surface of interest.\n"
              "                      If the surface does not have a spec file, use the \n"
              "                      program quickspec to create one.\n"
              "      -surf_A insurf: Name of surface of interest. \n"
              "                      NOTE: i_TYPE inSurf option is now obsolete.\n"
              "      -met method: name of smoothing method to use. Choose from:\n"
              "                 HEAT: The newer method by Chung et al. [Ref. 3&4 below]\n"
              "                         This method is used for filtering \n"
              "                         data on the surface and not for smoothing \n"
              "                         the surface's geometry per se. \n" 
              "                 LB_FEM: (Olde, better use HEAT)\n"
              "                         The method by Chung et al. 03.\n"
              "                         This method is used for filtering \n"
              "                         data on the surface and not for smoothing the\n"
              "                         surface's geometry per se. See References below.\n"
              "                 LM: The smoothing method proposed by G. Taubin 2000\n"
              "                     This method is used for smoothing\n"
              "                     a surface's geometry. See References below.\n"
              "                 NN_geom: A simple nearest neighbor coordinate smoothing.\n"
              "                          This interpolation method causes surface shrinkage\n"
              "                          that might need to be corrected with the -match_*\n"
              "                          options below. \n" 
              "\n"
              "   Options for LB_FEM:\n"
              "   It is now recommended that you use the newer method HEAT (see below).\n"
              "      -input inData: file containing data (in 1D or niml format)\n"
              "                        Each column in inData is processed separately.\n"
              "                        The number of rows must equal the number of\n"
              "                        nodes in the surface. You can select certain\n"
              "                        columns using the [] notation adopted by AFNI's\n"
              "                        programs.\n"
              "                  Note: The program will infer the format of the input\n"
              "                        file from the extension of inData. \n" 
              "      -fwhm f: Full Width at Half Maximum in surface coordinate units (usuallly mm)\n"
              "               of an equivalent Gaussian filter had the surface been flat.\n"
              "               With curved surfaces, the equation used to estimate FWHM is \n"
              "               an approximation. \n"
              "               Blurring on the surface depends on the geodesic instead \n"
              "               of the Euclidean disntaces. See Ref #1 for more details \n"
              "               on this parameter.\n"
              "\n"
              "   Options for HEAT:\n"
              "      -input inData : file containing data (in 1D or NIML format)\n"
              "                        Each column in inData is processed separately.\n"
              "                        The number of rows must equal the number of\n"
              "                        nodes in the surface. You can select certain\n"
              "                        columns using the [] notation adopted by AFNI's\n"
              "                  Note: The program will infer the format of the input\n"
              "                        file from the extension of inData. \n" 
              "                        programs.\n"
              "      Two and only two of the following three parameters:\n"
              "                     (See Refs #3&4 for more details)\n"
              "      -fwhm F: Effective Full Width at Half Maximum in surface coordinate units \n"
              "               (usuallly mm) of an equivalent Gaussian filter had the surface been \n"
              "               flat. With curved surfaces, the equation used to estimate FWHM is \n"
              "               an approximation. For Gaussian filters, FWHM, SIGMA (STD-DEV) and RMS\n"
              "               FWHM = 2.354820 * SIGMA = 1.359556 * RMS\n"
              "               Blurring on the surface depends on the geodesic instead \n"
              "               of the Euclidean disntaces. \n"
              "      -Niter N: Number of iterations, must be multiple of 2.\n"
              "                For this method, N > 200 is a good start. \n"
              "      -sigma  S: Bandwidth of smoothing kernel (for a single iteration).\n"
              "                 S should be small (< 1) and is related to the previous two\n"
              "                 parameters by: F = sqrt(N) * S * 2.355\n"
              "\n"
              "\n"
              "   Options for LM:\n"
              "      -kpb k: Band pass frequency (default is 0.1).\n"
              "              values should be in the range 0 < k < 10\n"
              "              -lm and -kpb options are mutually exclusive.\n"
              "      -lm l m: Lambda and Mu parameters. Sample values are:\n"
              "               0.6307 and -.6732\n"
              "      NOTE: -lm and -kpb options are mutually exclusive.\n"
              "      -surf_out surfname: Writes the surface with smoothed coordinates\n"
              "                          to disk. For SureFit and 1D formats, only the\n"
              "                          coord file is written out.\n"
              "      NOTE: -surf_out and -output are mutually exclusive.\n" 
              "      -iw wgt: Set interpolation weights to wgt. You can choose from:\n"
              "               Equal   : Equal weighting, fastest (default), \n"
              "                         tends to make edges equal.\n"
              "               Fujiwara: Weighting based on inverse edge length.\n"
              "                         Would be a better preserver of geometry when\n"
              "                         mesh has irregular edge lengths.\n"
              "               Desbrun : Weighting based on edge angles (slooow).\n"
              "                         Removes tangential displacement during smoothing.\n"
              "                         Might not be too useful for brain surfaces.\n"
              "\n"
              "   Options for NN_geom:\n"
              "      -match_size r: Adjust node coordinates of smoothed surface to \n"
              "                   approximates the original's size.\n"
              "                   Node i on the filtered surface is repositioned such \n"
              "                   that |c i| = 1/N sum(|cr j|) where\n"
              "                   c and cr are the centers of the smoothed and original\n"
              "                   surfaces, respectively.\n"
              "                   N is the number of nodes that are within r [surface \n"
              "                   coordinate units] along the surface (geodesic) from node i.\n"
              "                   j is one of the nodes neighboring i.\n"
              "      -match_vol tol: Adjust node coordinates of smoothed surface to \n"
              "                   approximates the original's volume.\n"
              "                   Nodes on the filtered surface are repositioned such\n"
              "                   that the volume of the filtered surface equals, \n"
              "                   within tolerance tol, that of the original surface. \n"
              "                   See option -vol in SurfaceMetrics for information about\n"
              "                   and calculation of the volume of a closed surface.\n"
              "      -match_area tol: Adjust node coordinates of smoothed surface to \n"
              "                   approximates the original's surface.\n"
              "                   Nodes on the filtered surface are repositioned such\n"
              "                   that the surface of the filtered surface equals, \n"
              "                   within tolerance tol, that of the original surface. \n"
              "      -match_sphere rad: Project nodes of smoothed surface to a sphere\n"
              "                   of radius rad. Projection is carried out along the \n"
              "                   direction formed by the surface's center and the node.\n"
              "      -surf_out surfname: Writes the surface with smoothed coordinates\n"
              "                          to disk. For SureFit and 1D formats, only the\n"
              "                          coord file is written out.\n"
              "\n"
              "   Common options:\n"
              "      -Niter N: Number of smoothing iterations (default is 100)\n"
              "                For practical reasons, this number must be a multiple of 2\n"
              "          NOTE: For LB_FEM method, the number of iterations controls the\n"
              "                iteration steps (dt in Ref #1).\n"
              "                dt = fwhm*fwhm / (16*Niter*log(2));\n"
              "                dt must satisfy conditions that depend on the internodal\n"
              "                distance and the spatial derivatives of the signals being \n"
              "                filtered on the surface.\n"
              "                As a rule of thumb, if increasing Niter does not alter\n"
              "                the results then your choice is fine (smoothing has converged).\n"
              "                For an example of the artifact caused by small Niter see:\n"
              "          http://afni.nimh.nih.gov/sscc/staff/ziad/SUMA/SuSmArt/DSart.html\n"
              "                To avoid this problem altogether, it is better that you use \n"
              "                the newer method HEAT which does not suffer from this problem.\n"
              "      -output OUT: Name of output file. \n"
              "                   The default is inData_sm with LB_FEM and HEAT method\n"
              "                   and NodeList_sm with LM method.\n" 
              "             NOTE: For data smoothing methods like HEAT, If a format extension,\n"
              "                   such as .1D.dset or .niml.dset is present in OUT, \n"
              "                   then the output will be written in that format. \n"
              "                   Otherwise, the format is the same as the input's\n"
              "      -add_index : Output the node index in the first column.\n"
              "                   This is not done by default.\n"
              "      -dbg_n node : output debug information for node 'node'.\n"
              "      -n_mask filter_mask: Apply filtering to nodes listed in\n"
              "                          filter_mask only. Nodes not in the filter_mask will\n"
              "                          not see their value change, but they will still \n"
              "                          contribute to the values of nodes in the filtermask.\n"
              "                          At the moment, it is only implemented for methods\n"
              "                          NN_geom, LM, LB_FEM and HEAT (and maybe other ones)\n"
              "      -b_mask filter_binary_mask: Similar to -n_mask, except that filter_binary_mask\n"
              "                          contains 1 for nodes to filter and 0 for nodes to be ignored.\n"
              "                          The number of rows in filter_binary_mask must be equal to the\n"
              "                          number of nodes forming the surface.\n"
              "\n"
              "%s"
              "\n"
              "%s"
              "\n"
              "   Sample commands lines for using SurfSmooth:\n"
              "         The surface used in this example had no spec file, so \n"
              "         a quick.spec was created using:\n"
              "         quickspec -tn 1D NodeList.1D FaceSetList.1D \n"
              "\n"
              "   Sample commands lines for data smoothing:\n"
              "      SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met HEAT   \\\n"
              "                  -input in.1D -Niter 200 -fwhm 8 -add_index         \\\n"
              "                  -output in_smh8.1D.dset \n"
              "      Or using the older (less recommended method):\n"
              "      SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met LB_FEM   \\\n"
              "                  -input in.1D -Niter 100 -fwhm 8 -add_index         \\\n"
              "                  -output in_sm8.1D.dset \n"
              "         This command filters (on the surface) the data in in.1D\n"
              "         and puts the output in in_sm8.1D.dset with the first column \n"
              "         containing the node index and the second containing the \n"
              "         filtered version of in.1D.\n"
              "         \n"
              "\n"
              "         You can colorize the input and output data using ScaleToMap:\n"
              "         ScaleToMap  -input in.1D 0 1 -cmap BGYR19       \\\n"
              "                     -clp MIN MAX > in.1D.col            \\\n"
              "         ScaleToMap  -input in_sm8.1D 0 1 -cmap BGYR19   \\\n"
              "                     -clp MIN MAX > in_sm8.1D.col        \\\n"
              "\n"
              "         For help on using ScaleToMap see ScaleToMap -help\n"
              "         Note that the MIN MAX represent the minimum and maximum\n"
              "         values in in.1D. You should keep them constant in both \n"
              "         commands in order to be able to compare the resultant colorfiles.\n"
              "         You can import the .col files with the 'c' command in SUMA.\n"
              "\n"
              "         You can send the data to SUMA with each iteration.\n"
              "         To do so, start SUMA with these options:\n"
              "         suma -spec quick.spec -niml &\n"
              "         and add these options to SurfSmooth's command line above:\n"
              "         -talk_suma -refresh_rate 5\n" 
              "\n"
              "   Sample commands lines for surface smoothing:\n"
              "      SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met LM    \\\n"
              "                  -output NodeList_sm100.1D -Niter 100 -kpb 0.1   \n"
              "         This command smoothes the surface's geometry. The smoothed\n"
              "         node coordinates are written out to NodeList_sm100.1D. \n"
              "\n"
              "   Sample command for considerable surface smoothing and inflation\n"
              "   back to original volume:\n"
              "       SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met NN_geom \\\n"
              "                   -output NodeList_inflated_mvol.1D -Niter 1500 \\\n"
              "                   -match_vol 0.01\n" 
              "   Sample command for considerable surface smoothing and inflation\n"
              "   back to original area:\n"
              "       SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met NN_geom \\\n"
              "                   -output NodeList_inflated_marea.1D -Niter 1500 \\\n"
              "                   -match_area 0.01\n" 
              "\n"
              "   References: \n"
              "      (1) M.K. Chung et al.   Deformation-based surface morphometry\n"
              "                              applied to gray matter deformation. \n"
              "                              Neuroimage 18 (2003) 198-213\n"
              "          M.K. Chung   Statistical morphometry in computational\n"
              "                       neuroanatomy. Ph.D. thesis, McGill Univ.,\n"
              "                       Montreal, Canada\n"
              "      (2) G. Taubin.       Mesh Signal Processing. \n"
              "                           Eurographics 2000.\n"
              "      (3) M.K. Chung et al.  Cortical thickness analysis in autism \n"
              "                             via heat kernel smoothing. NeuroImage, \n"
              "                             submitted.(2005) \n"
              "             http://www.stat.wisc.edu/~mchung/papers/ni_heatkernel.pdf\n"
              "      (4) M.K. Chung,  Heat kernel smoothing and its application to \n"
              "                       cortical manifolds. Technical Report 1090. \n"
              "                       Department of Statististics, U.W.Madison\n"
              "             http://www.stat.wisc.edu/~mchung/papers/heatkernel_tech.pdf"
              "\n"
              "   See Also:   \n"
              "       ScaleToMap to colorize the output, however it is better\n"
              "       to load surface datasets directly into SUMA and colorize\n"
              "       them interactively."
              "\n"
              "\n", st, s); SUMA_free(s); s = NULL; SUMA_free(st); st = NULL;
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
       exit (0);
   }


#define SURFSMOOTH_MAX_SURF 1  /*!< Maximum number of input surfaces */

typedef enum { SUMA_NO_METH, SUMA_LB_FEM_1D, SUMA_LB_FEM, SUMA_LM, SUMA_BRUTE_FORCE, SUMA_NN_GEOM, SUMA_HEAT_05_1D, SUMA_HEAT_05} SUMA_SMOOTHING_METHODS;

typedef struct {
   float OffsetLim;
   float lim;
   float fwhm;
   float kpb;
   float l;
   float m;
   int ShowNode;
   int Method;
   int dbg;
   int N_iter;
   int AddIndex;
   int insurf_method; /* method used to specify input surfaces. 
                        0 then none input 
                        1 the old way
                        2 the new (-spec way) */
   SUMA_SO_File_Type iType;
   char *vp_name;
   char *sv_name;
   char *if_name;
   char *if_name2;
   char *in_name;
   char *out_name;   /* this one's dynamically allocated so you'll have to free it yourself */
   char *ShowOffset_DBG; /* meant to be a file where one outputs some debugging info. Not being used ...*/
   char *surf_out;
   char *surf_names[SURFSMOOTH_MAX_SURF];
   char *spec_file;
   int MatchMethod;
   byte *nmask;
   char *nmaskname;
   char *bmaskname;
   float sigma;
   SUMA_DSET_FORMAT oform;
} SUMA_SURFSMOOTH_OPTIONS;

/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_SURFSMOOTH_OPTIONS *) options structure.
               To free it, use 
               SUMA_free(Opt->out_name); 
               SUMA_free(Opt);
*/
SUMA_SURFSMOOTH_OPTIONS *SUMA_SurfSmooth_ParseInput (char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SurfSmooth_ParseInput"}; 
   SUMA_SURFSMOOTH_OPTIONS *Opt=NULL;
   int kar, i, ind;
   char *outname;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_SURFSMOOTH_OPTIONS *)SUMA_malloc(sizeof(SUMA_SURFSMOOTH_OPTIONS));
   
   kar = 1;
   Opt->OffsetLim = 10.0;
   Opt->MatchMethod = 0;
   Opt->lim = 1000000.0;
   Opt->fwhm = -1;
   Opt->ShowNode = -1;
   Opt->Method = SUMA_NO_METH;
   Opt->dbg = 0;
   Opt->if_name = NULL;
   Opt->if_name2 = NULL;
   Opt->in_name = NULL;
   Opt->out_name = NULL;
   Opt->vp_name = NULL; 
   Opt->sv_name = NULL;
   Opt->surf_out = NULL;
   Opt->ShowOffset_DBG = NULL;
   Opt->iType = SUMA_FT_NOT_SPECIFIED;
   Opt->N_iter = 100;
   Opt->kpb = -1.0;
   Opt->l = -1.0;
   Opt->m = -1.0;
   Opt->AddIndex = 0;
   Opt->insurf_method = 0;
   Opt->nmask = NULL;
   Opt->nmaskname = NULL;
   Opt->bmaskname = NULL;
   Opt->spec_file = NULL;
   Opt->sigma = -1.0;
   Opt->oform = SUMA_NO_DSET_FORMAT;
   SUMA_Set_Taubin_Weights(SUMA_EQUAL);
   for (i=0; i<SURFSMOOTH_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
   outname = NULL;
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_SurfSmooth();
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);

      #if 0 /* now  in the SUMA_GENERIC_ARGV_PARSE struct */
      if (!brk && strcmp(argv[kar], "-ni_text") == 0)
		{
         SUMA_GEOMCOMP_NI_MODE = NI_TEXT_MODE;
         brk = YUP;
      }
      
      if (!brk && strcmp(argv[kar], "-ni_binary") == 0)
		{
         SUMA_GEOMCOMP_NI_MODE = NI_BINARY_MODE;
         brk = YUP;
      }
      
		if (!brk && strcmp(argv[kar], "-sh") == 0)
		{
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -sh \n");
				exit (1);
			}
			if (strcmp(argv[kar],"localhost") != 0) {
            Opt->suma_host_name = SUMA_copy_string(argv[kar]);
         }else {
           fprintf (SUMA_STDERR, "localhost is the default for -sh\nNo need to specify it.\n");
         }

			brk = YUP;
		}	
      if (!brk && strcmp(argv[kar], "-refresh_rate") == 0)
		{
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -refresh_rate \n");
				exit (1);
			}
			Opt->rps = atof(argv[kar]);
         if (Opt->rps <= 0) {
            fprintf (SUMA_STDERR, "Bad value (%f) for refresh_rate\n", Opt->rps);
				exit (1);
         }

			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-talk_suma") == 0)) {
			Opt->talk_suma = 1; 
			brk = YUP;
		}
      
      #endif
      if (!brk && strcmp(argv[kar], "-dist") == 0)
		{
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -dist \n");
				exit (1);
			}
			Opt->OffsetLim = atof(argv[kar]);
         if (Opt->OffsetLim <= 0) {
            fprintf (SUMA_STDERR, "Bad value (%f) for refresh_rate\n", Opt->OffsetLim);
				exit (1);
         }

			brk = YUP;
		}
     
      if (!brk && (strcmp(argv[kar], "-dbg_n") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 argument after -dbg_n \n");
				exit (1);
			}
			Opt->ShowNode = atoi(argv[kar]); 
         SUMA_Set_SurfSmooth_NodeDebug(Opt->ShowNode);
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-n_mask") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 argument after -n_mask \n");
				exit (1);
			}
			Opt->nmaskname = argv[kar]; 
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-b_mask") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 argument after -b_mask \n");
				exit (1);
			}
			Opt->bmaskname = argv[kar]; 
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-Niter") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 arguments after -Niter \n");
				exit (1);
			}
			Opt->N_iter = atoi(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-kpb") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 arguments after -kpb \n");
				exit (1);
			}
         if (Opt->l != -1.0  || Opt->m != -1.0) {
            fprintf (SUMA_STDERR, "options -lm and -kpb are mutually exclusive\n");
				exit (1);
         }
			Opt->kpb = atof(argv[kar]); 
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-surf_out") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need 1 arguments after -surf_out\n");
				exit (1);
			}
         if (outname) {
            fprintf (SUMA_STDERR, "-output and -surf_out are mutually exclusive.\n");
            exit(1);
         }
			Opt->surf_out = argv[kar]; 
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-lm") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 arguments after -lm \n");
				exit (1);
			}
         if (Opt->kpb != -1.0) {
            fprintf (SUMA_STDERR, "options -lm and -kpb are mutually exclusive\n");
				exit (1);
         }
			Opt->l = atof(argv[kar]); kar ++;
         Opt->m = atof(argv[kar]);  
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-iw") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -iw \n");
				exit (1);
			}
         SUMA_TO_LOWER(argv[kar]);
         if (strcmp(argv[kar], "equal") == 0 ) {
            SUMA_Set_Taubin_Weights(SUMA_EQUAL);
         }else if (strcmp(argv[kar],"fujiwara")==0) {
            SUMA_Set_Taubin_Weights(SUMA_FUJIWARA); 
         }else if (strcmp(argv[kar],"desbrun")==0) {
            SUMA_Set_Taubin_Weights(SUMA_DESBRUN);
         } else {
            fprintf (SUMA_STDERR, "Weights option %s not understood.\n", argv[kar]);
            exit (1);
         } 
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -input \n");
				exit (1);
			}
			Opt->in_name = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-output") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -output\n");
				exit (1);
			}
			if (Opt->surf_out) {
            fprintf (SUMA_STDERR, "options -surf_out and -output are mutually exclusive\n");
				exit (1);
         }
         outname = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-add_index") == 0)) {
			Opt->AddIndex = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_fs") == 0)) {
         SUMA_SL_Err("Option -i_fs is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -i_fs \n");
				exit (1);
			}
			Opt->if_name = argv[kar];
         Opt->iType = SUMA_FREE_SURFER;
         if (!Opt->insurf_method) Opt->insurf_method = 1;
         else {
            fprintf (SUMA_STDERR, "already specified input surface.\n");
            exit(1);
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_sf") == 0)) {
         SUMA_SL_Err("Option -i_sf is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 argument after -i_sf\n");
				exit (1);
			}
			Opt->if_name = argv[kar]; kar ++;
         Opt->if_name2 = argv[kar];
         Opt->iType = SUMA_SUREFIT;
         if (!Opt->insurf_method) Opt->insurf_method = 1;
         else {
            fprintf (SUMA_STDERR, "already specified input surface.\n");
            exit(1);
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_vec") == 0)) {
         SUMA_SL_Err("Option -i_vec is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 argument after -i_vec\n");
				exit (1);
			}
			Opt->if_name = argv[kar]; kar ++;
         Opt->if_name2 = argv[kar];
         Opt->iType = SUMA_VEC;
         if (!Opt->insurf_method) Opt->insurf_method = 1;
         else {
            fprintf (SUMA_STDERR, "already specified input surface.\n");
            exit(1);
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_dx") == 0)) {
         SUMA_SL_Err("Option -i_ply is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -i_dx\n ");
				exit (1);
			}
			Opt->if_name = argv[kar];
         Opt->iType = SUMA_OPENDX_MESH;
         if (!Opt->insurf_method) Opt->insurf_method = 1;
         else {
            fprintf (SUMA_STDERR, "already specified input surface.\n");
            exit(1);
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-i_ply") == 0)) {
         SUMA_SL_Err("Option -i_ply is obsolete.\nUse -spec and -surf_A instead.\n");
         exit(1);
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -i_ply\n ");
				exit (1);
			}
			Opt->if_name = argv[kar];
         Opt->iType = SUMA_PLY;
         if (!Opt->insurf_method) Opt->insurf_method = 1;
         else {
            fprintf (SUMA_STDERR, "already specified input surface.\n");
            exit(1);
         }
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-spec") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -spec \n");
				exit (1);
			}
			Opt->spec_file = argv[kar];
         if (!Opt->insurf_method) Opt->insurf_method = 2;
         else {
            fprintf (SUMA_STDERR, "already specified spec file.\n");
            exit(1);
         }
			brk = YUP;
		}
      
      if (!brk && (strncmp(argv[kar], "-surf_", 6) == 0)) {
			if (kar + 1>= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -surf_X SURF_NAME \n");
				exit (1);
			}
			ind = argv[kar][6] - 'A';
         if (ind < 0 || ind >= SURFSMOOTH_MAX_SURF) {
            fprintf (SUMA_STDERR, "-surf_X SURF_NAME option is out of range,\n"
                                  "   only surf_A allowed.\n");
				exit (1);
         }
         kar ++;
         Opt->surf_names[ind] = argv[kar];
			if (Opt->insurf_method && Opt->insurf_method != 2) {
            fprintf (SUMA_STDERR, "-surf_X SURF_NAME option must be used with -spec option.\n");
            exit(1);
         }
         brk = YUP;
		}
      
      
      if (!brk && (strcmp(argv[kar], "-lim") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -lim \n");
				exit (1);
			}
			Opt->lim = atof(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-match_size") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -match_size \n");
				exit (1);
			}
			Opt->lim = atof(argv[kar]);
         Opt->MatchMethod = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-match_vol") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -match_vol \n");
				exit (1);
			}
			Opt->lim = atof(argv[kar]);
         Opt->MatchMethod = 2;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-match_area") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -match_area \n");
				exit (1);
			}
			Opt->lim = atof(argv[kar]);
         Opt->MatchMethod = 3;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-match_sphere") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -match_sphere \n");
				exit (1);
			}
			Opt->lim = atof(argv[kar]);
         Opt->MatchMethod = 4;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-fwhm") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -fwhm \n");
				exit (1);
			}
			Opt->fwhm = atof(argv[kar]);
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-sigma") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -sigma \n");
				exit (1);
			}
			Opt->sigma = atof(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-met") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -met \n");
				exit (1);
			}
			if (strcmp(argv[kar], "LB_FEM_1D") == 0)  Opt->Method = SUMA_LB_FEM_1D;
         else if (strcmp(argv[kar], "LB_FEM") == 0)  Opt->Method = SUMA_LB_FEM;
         else if (strcmp(argv[kar], "LM") == 0)  Opt->Method = SUMA_LM;
         else if (strcmp(argv[kar], "BF") == 0)  Opt->Method = SUMA_BRUTE_FORCE;
         else if (strcmp(argv[kar], "NN_geom") == 0)  Opt->Method = SUMA_NN_GEOM;
         else if (strcmp(argv[kar], "HEAT_1D") == 0)  Opt->Method = SUMA_HEAT_05_1D;
         else if (strcmp(argv[kar], "HEAT") == 0)  Opt->Method = SUMA_HEAT_05;
         else {
            fprintf (SUMA_STDERR, "Method %s not supported.\n", argv[kar]);
				exit (1);
         }
			brk = YUP;
		}
      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }

   /* check on options for HEAT budiness first */
   if (Opt->Method == SUMA_HEAT_05_1D || Opt->Method == SUMA_HEAT_05) {
      float sequiv;
      if (  (Opt->N_iter < 0 && Opt->fwhm < 0 && Opt->sigma < 0) || 
            (Opt->N_iter > 0 && Opt->fwhm > 0 && Opt->sigma > 0) || 
            (Opt->N_iter < 0 && Opt->fwhm < 0) ||
            (Opt->N_iter < 0 && Opt->sigma < 0)||
            (Opt->fwhm < 0 && Opt->sigma < 0) ) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Need to specify two and only two out of the three options:\n"
                              "-N_iter, -fwhm, -sigma\n", FuncName);
         exit (1);
      }
      if (Opt->N_iter < 0) {
         sequiv  = Opt->fwhm * 0.42466090;
         Opt->N_iter = SUMA_POW2(sequiv/Opt->sigma);
         if (Opt->N_iter % 2) ++Opt->N_iter;
         if (Opt->N_iter < 100) {
            fprintf (SUMA_STDERR,"Warning: N_iter = %d\n"
                                 "Perhaps too small for comfort.\n"
                                 "Consider reducing sigma.\n", Opt->N_iter);
         }
      }
      if (Opt->fwhm < 0) {
         sequiv = sqrt(Opt->N_iter)*Opt->sigma;
         Opt->fwhm = sequiv / 0.42466090;
      }
      if (Opt->sigma < 0) {
         sequiv = Opt->fwhm * 0.42466090;
         Opt->sigma = sequiv / sqrt(Opt->N_iter);
      }
      fprintf (SUMA_STDERR,"Effective FWHM = %f, kernel bandwidth = %f, N_iter = %d\n", Opt->fwhm, Opt->sigma, Opt->N_iter);
   }
   
   if (Opt->N_iter == -1) { /* default */ Opt->N_iter = 100; }
   if (Opt->N_iter < 1) {
      fprintf (SUMA_STDERR,"Error %s:\nWith -Niter N option, N must be > 1\n", FuncName);
      exit (1);
   }
   
   if ( (Opt->N_iter % 2) &&
        (Opt->Method == SUMA_LB_FEM_1D || Opt->Method == SUMA_LB_FEM || 
        Opt->Method == SUMA_HEAT_05_1D || Opt->Method == SUMA_HEAT_05 ||
        Opt->Method == SUMA_LM) ) {
      fprintf (SUMA_STDERR, "Number of iterations must be a multiple of 2.\n%d is not a multiple of 2.\n", Opt->N_iter);
      exit(1);
   }
   
   if (Opt->ShowNode < 0 && Opt->ShowOffset_DBG) {
      fprintf (SUMA_STDERR,"Error %s:\nBad debug node index (%d) in option -dbg_n\n", FuncName, Opt->ShowNode);
      exit (1);
   }
   
   if (Opt->insurf_method == 1) {
      SUMA_SL_Err("Obsolete method for surface specification.\nShould not have gotten here.");
      exit(1);
   }
      
   /* can't test for file existence here because of square brackets */
   if (0 && Opt->in_name && !SUMA_filexists(Opt->in_name)) {
      fprintf (SUMA_STDERR,"Error %s:\n%s not found.\n", FuncName, Opt->if_name);
      exit(1);
   }
   
   if (Opt->Method == SUMA_NO_METH) {
      fprintf (SUMA_STDERR,"Error %s:\nNo method was specified.\n", FuncName);
      exit(1);  
   }
   
   if (ps->cs->talk_suma && Opt->insurf_method != 2) {
      fprintf (SUMA_STDERR,   "must specify surface using -spec option\n"
                              "if you whish to talk to suma.\n");
      exit(1); 
   }
   
   
   if (Opt->insurf_method == 2) {
      if (!Opt->surf_names[0] || !Opt->spec_file) {
         fprintf (SUMA_STDERR,   "failed to specify either -spec or -surf_X options.\n");
         exit(1);  
      }
   }
    
   if (outname) {
      if (SUMA_filexists(outname)) {
         fprintf (SUMA_STDERR,"Error %s:\noutput file %s exists.\n", FuncName, outname);
         exit(1);
      }
      Opt->out_name = SUMA_copy_string(outname);
      Opt->oform = SUMA_GuessFormatFromExtension(Opt->out_name);
   } else {
      switch (Opt->Method) {
         case SUMA_LB_FEM_1D:
            /* form autoname  */
            Opt->out_name = SUMA_Extension(Opt->in_name, ".1D", YUP); /*remove .1D */
            Opt->out_name = SUMA_append_replace_string(Opt->out_name,"_sm", "", 1); /* add _sm to prefix */
            Opt->out_name = SUMA_append_replace_string(Opt->out_name,".1D", "", 1); /* add .1D */
            break;
         case SUMA_LB_FEM:
            /* form autoname  */
            Opt->oform = SUMA_GuessFormatFromExtension(Opt->in_name);
            Opt->out_name = SUMA_RemoveDsetExtension_s(Opt->in_name, Opt->oform);
            Opt->out_name = SUMA_append_replace_string(Opt->out_name, "_sm", "", 1); /* add _sm to prefix */
            Opt->out_name = SUMA_append_replace_string(Opt->out_name, (char*)SUMA_ExtensionOfDsetFormat (Opt->oform), "", 1); /* add extension */
            break;
         case SUMA_LM:
            /* form autoname  */
            Opt->out_name = SUMA_copy_string("NodeList_sm.1D");
            break;
         case SUMA_BRUTE_FORCE:
            /* form autoname  */
            Opt->out_name = SUMA_copy_string("NodeList_Offsetsm.1D");
            break;
         case SUMA_NN_GEOM:
            /* form autoname  */
            Opt->out_name = SUMA_copy_string("NodeList_NNsm.1D");
            break;
         case SUMA_HEAT_05_1D:
            /* form autoname  */
            Opt->out_name = SUMA_Extension(Opt->in_name, ".1D", YUP); /*remove .1D */
            Opt->out_name = SUMA_append_replace_string(Opt->out_name,"_smh", "", 1); /* add _smh to prefix */
            Opt->out_name = SUMA_append_replace_string(Opt->out_name,".1D", "", 1); /* add .1D */
            break;
         case SUMA_HEAT_05:
            /* form autoname  */
            Opt->oform = SUMA_GuessFormatFromExtension(Opt->in_name);
            Opt->out_name = SUMA_RemoveDsetExtension_s(Opt->in_name, Opt->oform);
            Opt->out_name = SUMA_append_replace_string(Opt->out_name,"_sm", "", 1); /* add _sm to prefix */
            Opt->out_name = SUMA_append_replace_string(Opt->out_name, (char*)SUMA_ExtensionOfDsetFormat (Opt->oform), "", 1); /* add extension */
            break;
         default:
            fprintf (SUMA_STDERR,"Error %s:\nNot ready for this option here.\n", FuncName);
            exit(1);
            break;
      }
      if (SUMA_filexists(Opt->out_name)) {
         fprintf (SUMA_STDERR,"Error %s:\noutput file %s exists.\n", FuncName, Opt->out_name);
         exit(1);
      }
   }

   /* method specific checks */
   switch (Opt->Method) {
      case SUMA_LB_FEM_1D:
      case SUMA_LB_FEM:
         if (!Opt->in_name) {
            fprintf (SUMA_STDERR,"Error %s:\ninput data not specified.\n", FuncName);
            exit(1);
         }
         if (Opt->fwhm ==  -1.0) {
            fprintf (SUMA_STDERR,"Error %s:\n-fwhm option must be used with -met LB_FEM.\n", FuncName); 
            exit(1);
         }else if (Opt->fwhm <= 0.0) {
            fprintf (SUMA_STDERR,"Error %s:\nFWHM must be > 0\n", FuncName);
            exit(1);
         }
         if (Opt->kpb >= 0) {
            fprintf (SUMA_STDERR,"Error %s:\n-kpb option is not valid with -met LB_FEM.\n", FuncName); 
            exit(1);
         }         
         
         break;
      case SUMA_HEAT_05:
      case SUMA_HEAT_05_1D:
         if (!Opt->in_name) {
            fprintf (SUMA_STDERR,"Error %s:\ninput data not specified.\n", FuncName);
            exit(1);
         }
         if (Opt->fwhm ==  -1.0) {
            fprintf (SUMA_STDERR,"Error %s:\n-fwhm option must be used with -met HEAT.\n", FuncName); 
            exit(1);
         }else if (Opt->fwhm <= 0.0) {
            fprintf (SUMA_STDERR,"Error %s:\nFWHM must be > 0\n", FuncName);
            exit(1);
         }
         if (Opt->kpb >= 0) {
            fprintf (SUMA_STDERR,"Error %s:\n-kpb option is not valid with -met HEAT.\n", FuncName); 
            exit(1);
         }         
         
         break;
      case SUMA_BRUTE_FORCE:
         
         break;
      case SUMA_LM:
         
         if ( (Opt->l != -1.0 || Opt->m != -1.0) && Opt->kpb != -1.0) {
            fprintf (SUMA_STDERR,"Error %s:\nYou cannot mix options -kpb and -lm \n", FuncName);
            exit(1);
         }
         if (Opt->kpb != -1.0 && (Opt->kpb < 0.000001 || Opt->kpb > 10)) {
            fprintf (SUMA_STDERR,"Error %s:\nWith -kpb k option, you should satisfy 0 < k < 10\n", FuncName);
            exit(1);
         }
         if (Opt->l == -1.0 && Opt->m == -1.0 && Opt->kpb == -1.0) {
            Opt->kpb = 0.1;
         }

         if (Opt->l == -1.0 || Opt->m == -1.0) { /* convert kpb into l and m */
            if (!SUMA_Taubin_Smooth_Coef (Opt->kpb, &(Opt->l), &(Opt->m))) {
               SUMA_SL_Err("Failed to find smoothing coefficients");
               exit(1);            
            }
         } 

         if (Opt->in_name) {
            fprintf (SUMA_STDERR,"Error %s:\nOption -input not valid with -met LM.\n", FuncName);
            exit(1);
         }
         
         if (Opt->fwhm !=  -1.0) {
            fprintf (SUMA_STDERR,"Error %s:\nOption -fwhm not valid with -met LM.\n", FuncName);
            exit(1);
         }
         
         break;
      case SUMA_NN_GEOM:
         if (Opt->in_name) {
            fprintf (SUMA_STDERR,"Error %s:\nOption -input not valid with -met NN_geom.\n", FuncName);
            exit(1);
         }
         
         if (0 && Opt->lim > 1000) {
            fprintf (SUMA_STDERR,"Error %s:\n-lim option not specified.\n", FuncName);
            exit(1);
         }
         
         if (Opt->fwhm !=  -1.0) {
            fprintf (SUMA_STDERR,"Error %s:\nOption -fwhm not valid with -met NN_geom.\n", FuncName);
            exit(1);
         }
         break;
      
         
      default:
         fprintf (SUMA_STDERR,"Error %s:\nNot ready for this option here.\n", FuncName);
         exit(1);
         break;
   }
   
   if (Opt->bmaskname && Opt->nmaskname) {
      fprintf (SUMA_STDERR,"Error %s:\n-n_mask and -b_mask options are mutually exclusive.\n", FuncName);
      exit(1);
   }
   SUMA_RETURN (Opt);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfSmooth"}; 
	int kar, icol, nvec, ncol=0, i, ii;
   float *data_old = NULL, *far = NULL;
   float **DistFirstNeighb;
   void *SO_name = NULL;
   SUMA_SurfaceObject *SO = NULL, *SOnew = NULL;
   MRI_IMAGE *im = NULL;
   SUMA_SFname *SF_name = NULL;
   struct  timeval start_time, start_time_all;
   float etime_GetOffset, etime_GetOffset_all;   
   SUMA_GET_OFFSET_STRUCT *OffS = NULL;
   SUMA_SURFSMOOTH_OPTIONS *Opt;  
   FILE *fileout=NULL; 
   float **wgt=NULL, *dsmooth=NULL;
   SUMA_INDEXING_ORDER d_order=SUMA_NO_ORDER;
   SUMA_COMM_STRUCT *cs = NULL;
	SUMA_SurfSpecFile Spec; 
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_DSET *dset = NULL;
   int iform;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;
   
   
	/* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-i;-sv;-talk;");
   
   if (argc < 6)
       {
          usage_SUMA_SurfSmooth();
          exit (1);
       }
   
   Opt = SUMA_SurfSmooth_ParseInput (argv, argc, ps);
   cs = ps->cs;
   if (!cs) exit(1);
   
   /* now for the real work */
   if (Opt->insurf_method == 1) { /* method 1 */
      SUMA_SL_Err("Input in this method is no longer supported.\n");
      exit(1);
   } else { /* method 2 */
      int SO_read = -1;
      if (!SUMA_AllocSpecFields(&Spec)) {
         SUMA_S_Err("Failed to allocate");
         exit(1);
      }
      if (!SUMA_Read_SpecFile (Opt->spec_file, &Spec)) {
			fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
			exit(1);
		}
      SO_read = SUMA_spec_select_surfs(&Spec, Opt->surf_names, SURFSMOOTH_MAX_SURF, 0);
      if ( SO_read != 1 )
      {
	      fprintf(SUMA_STDERR,"Error %s: Found %d surfaces, expected only 1.\n", FuncName,  SO_read);
         exit(1);
      }
      /* now read into SUMAg_DOv */
      if (!SUMA_LoadSpec_eng(&Spec, SUMAg_DOv, &SUMAg_N_DOv, Opt->sv_name, 0, SUMAg_CF->DsetList) ) {
	      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec_eng\n", FuncName);
         exit(1);
      }
      /* now identify surface needed */
      SO = SUMA_find_named_SOp_inDOv(Opt->surf_names[0], SUMAg_DOv, SUMAg_N_DOv);
   }
   
   if (!SO) {
      fprintf (SUMA_STDERR,"Error %s: Failed to read input surface.\n", FuncName);
      exit (1);
   }

   /* setup the mask, if needed */
   if (Opt->nmaskname) {
      int kk;
      float *far=NULL;
      MRI_IMAGE * im=NULL;
      
      
      im = mri_read_1D (Opt->nmaskname);
      if (!im) {
         SUMA_S_Err("Failed to read mask file");  
         exit(1);
      }
      far = MRI_FLOAT_PTR(im);
   
      if (!im->nx) {
         SUMA_S_Err("Empty file");  
         exit(1);
      }
      if (im->ny != 1 ) {
         SUMA_S_Err("nmask file must have\n"
                     " 1 column.");
         fprintf(SUMA_STDERR,"Have %d columns!\n", im->ny);
         exit(1);
      }
      Opt->nmask = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte));
      if (!Opt->nmask) {
         SUMA_S_Crit("Failed to allocate"); exit(1);
      }
      for (kk=0; kk<im->nx; ++kk) {
         if (far[kk] < 0 || far[kk] >= SO->N_Node) {
            SUMA_S_Err( "Bad indices in mask file.\n"
                        "Values either < 0 or >= number\n"
                        "of nodes in surface.");
            exit(1);
         }
         Opt->nmask[(int)far[kk]] = 1;   
      }
      mri_free(im); im = NULL;
   }
   if (Opt->bmaskname) {
      int kk;
      float *far=NULL;
      MRI_IMAGE * im=NULL;
      
      
      im = mri_read_1D (Opt->bmaskname);
      if (!im) {
         SUMA_S_Err("Failed to read mask file");  
         exit(1);
      }
      far = MRI_FLOAT_PTR(im);
   
      if (!im->nx) {
         SUMA_S_Err("Empty file");  
         exit(1);
      }
      if (im->nx != SO->N_Node) {
         SUMA_S_Err( "Number of rows in mask file is not \n"
                     "equal to number of nodes in surface.\n");  
         exit(1);
      }
      if (im->ny != 1 ) {
         SUMA_S_Err("nmask file must have\n"
                     " 1 column.");
         fprintf(SUMA_STDERR,"Have %d columns!\n", im->ny);
         exit(1);
      }
      Opt->nmask = (byte *)SUMA_calloc(SO->N_Node, sizeof(byte));
      if (!Opt->nmask) {
         SUMA_S_Crit("Failed to allocate"); exit(1);
      }
      for (kk=0; kk<im->nx; ++kk) {
         if ((int)far[kk]) {
            Opt->nmask[kk] = 1; 
            /* fprintf (SUMA_STDERR,"%d   ", kk);  */
         }
      }
      mri_free(im); im = NULL;
   }
   
   if (Opt->ShowNode >= 0 && Opt->ShowNode >= SO->N_Node) {
      fprintf (SUMA_STDERR,"Error %s: Requesting debugging info for a node index (%d) \n"
                           "that does not exist in a surface of %d nodes.\nRemember, indexing starts at 0.\n", 
                           FuncName, Opt->ShowNode, SO->N_Node);
      exit (1);
   }
   
      
   /* form EL and FN */
   if (!SO->EL || !SO->FN) {
      /* normally you'd call SUMA_SurfaceMetrics_eng (SO, "EdgeList", NULL, 0) 
      but that should be done in SUMA_SurfaceMetrics_eng. */
      SUMA_SLP_Err("Unexpexted NULL SO->EL or SO->FN");
      exit(1); 
   }

   
   /* see if SUMA talk is turned on */
   if (ps->cs->talk_suma) {
      cs->istream = SUMA_GEOMCOMP_LINE;
      
      if (  Opt->Method == SUMA_LB_FEM_1D || Opt->Method == SUMA_LB_FEM || 
            Opt->Method == SUMA_HEAT_05_1D || Opt->Method == SUMA_HEAT_05 ) { 
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            cs->Send = NOPE;
            ps->cs->talk_suma = NOPE;
         }
      }else if (Opt->Method == SUMA_LM) { 
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            cs->Send = NOPE;
            ps->cs->talk_suma = NOPE;
         }
      }else if (Opt->Method == SUMA_NN_GEOM) { 
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            cs->Send = NOPE;
            ps->cs->talk_suma = NOPE;
         }
      }else if (Opt->Method == SUMA_BRUTE_FORCE) { 
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            cs->Send = NOPE;
            ps->cs->talk_suma = NOPE;
         }
      }else {
         SUMA_SL_Err("Can't talk to suma with the chosen method.\n");
         ps->cs->talk_suma = NOPE;
      }
   }
  
   ncol = 3; /* default for geometry smoothing. That is changed below for data smoothing. */
   switch (Opt->Method) {
      case SUMA_HEAT_05_1D:/* Operates on 1D files, OBSOLETE but still accessible with -met HEAT_1D */
         /* Moo Chung's method for interpolation weights */
         {
            /* now load the input data */
            im = mri_read_1D (Opt->in_name);

            if (!im) {
               SUMA_SL_Err("Failed to read 1D file");
               exit(1);
            }

            far = MRI_FLOAT_PTR(im);
            nvec = im->nx;
            ncol = im->ny;
            d_order = SUMA_COLUMN_MAJOR;
            
            if (!nvec) {
               SUMA_SL_Err("Empty file");
               exit(1);
            }
            if (nvec != SO->N_Node) {
               fprintf(SUMA_STDERR, "Error %s:\n"
                                    "Expecting 1D file to have %d rows\n"
                                    "                    found %d rows instead.\n",
                                     FuncName, SO->N_Node, nvec);
               exit(1); 
            }
            if (LocalHead) SUMA_etime(&start_time,0);
            wgt = SUMA_Chung_Smooth_Weights_05(SO, Opt->sigma);
            if (!wgt) {
               SUMA_SL_Err("Failed to compute weights.\n");
               exit(1);
            }
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, "%s: weight computation took %f seconds for %d nodes.\n"
                                 "Projected time per 100000 nodes is: %f minutes\n", 
                                       FuncName, etime_GetOffset, SO->N_Node, 
                                       etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            
            dsmooth = SUMA_Chung_Smooth_05 (SO, wgt, Opt->N_iter, Opt->fwhm, far, ncol, SUMA_COLUMN_MAJOR, NULL, cs, Opt->nmask);
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, "%s: Total processing took %f seconds for %d nodes.\n"
                                 "Projected time per 100000 nodes is: %f minutes\n", 
                                       FuncName, etime_GetOffset, SO->N_Node, 
                                       etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            
            if (wgt) SUMA_free2D ((char **)wgt, SO->N_Node); wgt = NULL;
         }
         break; 
         
      case SUMA_LB_FEM_1D: /* Operates on 1D files, OBSOLETE but still accessible with -met LB_FEM_1D */
         /* Moo Chung's method for interpolation weights */
         {
            /* now load the input data */
            im = mri_read_1D (Opt->in_name);

            if (!im) {
               SUMA_SL_Err("Failed to read 1D file");
               exit(1);
            }

            far = MRI_FLOAT_PTR(im);
            nvec = im->nx;
            ncol = im->ny;
            d_order = SUMA_COLUMN_MAJOR;
            
            if (!nvec) {
               SUMA_SL_Err("Empty file");
               exit(1);
            }
            if (nvec != SO->N_Node) {
               fprintf(SUMA_STDERR, "Error %s:\n"
                                    "Expecting 1D file to have %d rows\n"
                                    "                    found %d rows instead.\n",
                                     FuncName, SO->N_Node, nvec);
               exit(1); 
            }
            if (LocalHead) SUMA_etime(&start_time,0);
            wgt = SUMA_Chung_Smooth_Weights(SO);
            if (!wgt) {
               SUMA_SL_Err("Failed to compute weights.\n");
               exit(1);
            }
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, "%s: weight computation took %f seconds for %d nodes.\n"
                                 "Projected time per 100000 nodes is: %f minutes\n", 
                                       FuncName, etime_GetOffset, SO->N_Node, 
                                       etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            
            
            dsmooth = SUMA_Chung_Smooth (SO, wgt, Opt->N_iter, Opt->fwhm, far, ncol, SUMA_COLUMN_MAJOR, NULL, cs, Opt->nmask);
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, "%s: Total processing took %f seconds for %d nodes.\n"
                                 "Projected time per 100000 nodes is: %f minutes\n", 
                                       FuncName, etime_GetOffset, SO->N_Node, 
                                       etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            
            #if 0
            /* writing is now done below ... */
            fileout = fopen(Opt->out_name, "w");
            if (Opt->AddIndex) SUMA_disp_vecmat (dsmooth, SO->N_Node, ncol, 1, d_order, fileout, YUP);
            else SUMA_disp_vecmat (dsmooth, SO->N_Node, ncol, 1, d_order, fileout, NOPE);
            fclose(fileout); fileout = NULL;

            if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;
            #endif
            if (wgt) SUMA_free2D ((char **)wgt, SO->N_Node); wgt = NULL;
         }
         break;
      
         case SUMA_LB_FEM: 
         /* Moo Chung's method for interpolation weights with dsets */
         {
            /* now load the input data */
            iform = SUMA_GuessFormatFromExtension(Opt->in_name);
            if (!(dset = SUMA_LoadDset_s (Opt->in_name, &iform, 0))) {
               SUMA_S_Err("Failed to read dset");
               exit(1);
            }  

            if (Opt->oform == SUMA_NO_DSET_FORMAT) Opt->oform = iform;
            if (!SDSET_VECLEN(dset) || !SDSET_VECNUM(dset)) {
               SUMA_SL_Err("Empty file");
               exit(1);
            }
            if (SDSET_VECLEN(dset) != SO->N_Node) {
               if (LocalHead) fprintf(SUMA_STDERR, "Warning %s:\n"
                                    "Expecting 1D file to have %d rows\n"
                                    "                    found %d rows instead.\n"
                                    "Function should deal with this properly but check results\n",
                                     FuncName, SO->N_Node, SDSET_VECLEN(dset));
            }
            if (Opt->AddIndex || Opt->oform == SUMA_NIML) {
               if (!SUMA_AddNodeIndexColumn(dset, SO->N_Node)) {
                  SUMA_S_Err("Failed to add a node index column");
                  exit(1);
               }
            } 
            if (LocalHead) SUMA_etime(&start_time,0);
            wgt = SUMA_Chung_Smooth_Weights(SO);
            if (!wgt) {
               SUMA_SL_Err("Failed to compute weights.\n");
               exit(1);
            }
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, "%s: weight computation took %f seconds for %d nodes.\n"
                                 "Projected time per 100000 nodes is: %f minutes\n", 
                                       FuncName, etime_GetOffset, SO->N_Node, 
                                       etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            
            if (!SUMA_Chung_Smooth_dset ( SO, wgt, 
                                          Opt->N_iter, Opt->fwhm, 
                                          dset, cs, Opt->nmask)) {
               SUMA_S_Err("Failed in  SUMA_Chung_Smooth_dset");
               exit(1);                            
            }
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, "%s: Total processing took %f seconds for %d nodes.\n"
                                 "Projected time per 100000 nodes is: %f minutes\n", 
                                       FuncName, etime_GetOffset, SO->N_Node, 
                                       etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            
            if (wgt) SUMA_free2D ((char **)wgt, SO->N_Node); wgt = NULL;
         }
         break;
         
      case SUMA_HEAT_05:
         /* Moo Chung's method for interpolation weights */
         {
            /* now load the input data */
            iform = SUMA_GuessFormatFromExtension(Opt->in_name);
            if (!(dset = SUMA_LoadDset_s (Opt->in_name, &iform, 0))) {
               SUMA_S_Err("Failed to read dset");
               exit(1);
            }
            if (LocalHead) {
               SUMA_LHv("Input %s:\n", Opt->in_name);
               SUMA_ShowDset(dset, 0, NULL);
            }  
            if (Opt->oform == SUMA_NO_DSET_FORMAT) Opt->oform = iform;

            if (!SDSET_VECLEN(dset) || !SDSET_VECNUM(dset)) {
               SUMA_SL_Err("Empty file");
               exit(1);
            }
            if (SDSET_VECLEN(dset) != SO->N_Node) {
               if (LocalHead) fprintf(SUMA_STDERR, "Warning %s:\n"
                                    "Expecting 1D file to have %d rows\n"
                                    "                    found %d rows instead.\n"
                                    "Function should deal with this properly but check results\n",
                                     FuncName, SO->N_Node, SDSET_VECLEN(dset));
            }
            
            if (Opt->AddIndex || Opt->oform == SUMA_NIML) {
               if (!SUMA_AddNodeIndexColumn(dset, SO->N_Node)) {
                  SUMA_S_Err("Failed to add a node index column");
                  exit(1);
               }
            }
             
            if (LocalHead) SUMA_etime(&start_time,0);
            wgt = SUMA_Chung_Smooth_Weights_05(SO, Opt->sigma);
            if (!wgt) {
               SUMA_SL_Err("Failed to compute weights.\n");
               exit(1);
            }
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, "%s: weight computation took %f seconds for %d nodes.\n"
                                 "Projected time per 100000 nodes is: %f minutes\n", 
                                       FuncName, etime_GetOffset, SO->N_Node, 
                                       etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            
            if (!SUMA_Chung_Smooth_05_dset ( SO, wgt, 
                                          Opt->N_iter, Opt->fwhm, 
                                          dset, cs, Opt->nmask)) {
               SUMA_S_Err("Failed in  SUMA_Chung_Smooth_05_dset");
               exit(1);                            
            }
            
            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, "%s: Total processing took %f seconds for %d nodes.\n"
                                 "Projected time per 100000 nodes is: %f minutes\n", 
                                       FuncName, etime_GetOffset, SO->N_Node, 
                                       etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            
            if (wgt) SUMA_free2D ((char **)wgt, SO->N_Node); wgt = NULL;
         }
         break; 
      
      case SUMA_NN_GEOM:
         /* brute forcem nearset neighbor interpolation */
         {
            if (LocalHead) SUMA_etime(&start_time,0);
            
            d_order =  SUMA_ROW_MAJOR; 
            
            dsmooth = SUMA_NN_GeomSmooth( SO, Opt->N_iter, SO->NodeList,
                                          3, d_order, NULL, cs, Opt->nmask);
            if (0 && LocalHead) {
               SUMA_LH("See dsmooth.1D");
               SUMA_disp_vecmat (dsmooth, SO->N_Node, 3, 1,  d_order, NULL, YUP);
            }
            if (!dsmooth) {
               SUMA_SL_Err("Failed in SUMA_NN_Geom_Smooth");
               exit(1);
            }
            
            /* writing of results is done below */
         }
         break;  
      case SUMA_BRUTE_FORCE:
         /* a method that will likely be dropped NOT FINISHED  */
         {
            if (LocalHead) SUMA_etime(&start_time,0);
            
            d_order =  SUMA_ROW_MAJOR; 
            
            SUMA_etime(&start_time_all,0);
            dsmooth = SUMA_Offset_GeomSmooth( SO, Opt->N_iter, Opt->OffsetLim, SO->NodeList,
                                              3, d_order, NULL, cs);
            if (0 && LocalHead) {
               SUMA_LH("See dsmooth.1D");
               SUMA_disp_vecmat (dsmooth, SO->N_Node, 3, 1,  d_order, NULL, YUP);
            }
            if (!dsmooth) {
               SUMA_SL_Err("Failed in SUMA_Offset_Geom_Smooth");
               exit(1);
            }
            
            /* writing of results is done below */
            
            etime_GetOffset_all = SUMA_etime(&start_time_all,1);
            fprintf(SUMA_STDERR, "%s: Done.\nSearch to %f mm took %f minutes for %d nodes.\n" , 
                                 FuncName, Opt->lim, etime_GetOffset_all / 60.0 , SO->N_Node);

         }
         break;
      case SUMA_LM:
         /* Taubin's */
         {
            
            if (LocalHead) SUMA_etime(&start_time,0);
            wgt = NULL;
            
            d_order =  SUMA_ROW_MAJOR; 
            if (SUMA_Get_Taubin_Weights() == SUMA_FUJIWARA) {
               SUMA_SL_Note("Fujiwara!!!");
               wgt = SUMA_Taubin_Fujiwara_Smooth_Weights(SO, NULL, NULL);
               if (!wgt) {
                  SUMA_SL_Err("Failed to compute weights.\n");
                  exit(1);
               }
            } else if (SUMA_Get_Taubin_Weights() == SUMA_DESBRUN) {
               wgt = SUMA_Taubin_Desbrun_Smooth_Weights(SO, NULL, NULL);
               if (!wgt) {
                  SUMA_SL_Err("Failed to compute weights.\n");
                  exit(1);
               }
            } else if (SUMA_Get_Taubin_Weights() != SUMA_EQUAL) {
               /* fprintf(stderr,"%d, %d\n", SUMA_Get_Taubin_Weights() , SUMA_EQUAL); */
               SUMA_SL_Err("Weights improperly initialized!");
               exit(1);
            }
             
            dsmooth = SUMA_Taubin_Smooth (SO, wgt, 
                      Opt->l, Opt->m, SO->NodeList, 
                      Opt->N_iter, 3, d_order,
                      NULL, cs, Opt->nmask); 

            if (LocalHead) {
               etime_GetOffset = SUMA_etime(&start_time,1);
               fprintf(SUMA_STDERR, "%s: Total processing took %f seconds for %d nodes.\n"
                                    "Projected time per 100000 nodes is: %f minutes\n", 
                                       FuncName, etime_GetOffset, SO->N_Node, 
                                       etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
            }
            
            if (wgt) SUMA_free2D ((char **)wgt, SO->N_Node); wgt = NULL;
            /* writing of results is done below */
         }
         break;
      default:
         SUMA_SL_Err("Bad method, should not be here.");
         exit(1);
         break;
   }
   
   
   if (Opt->Method == SUMA_BRUTE_FORCE || Opt->Method == SUMA_NN_GEOM)
   {
      if (Opt->MatchMethod) {   
         SUMA_LH("Fixing shrinkage...");
         /* Create a surface that is a descendant of SO, use the new coordinates */
         SOnew = SUMA_CreateChildSO( SO,
                                     dsmooth, SO->N_Node,
                                     NULL, -1,
                                     0); /* SOnew->NodeList is now == dsmooth */

         /* fix the shrinking ..*/

         switch (Opt->MatchMethod) {
            case 1:
               if (!SUMA_EquateSurfaceSize(SOnew, SO, Opt->lim, cs)) {
                  SUMA_SL_Warn("Failed to fix surface size.\nTrying to finish ...");
               }

               /* send the unshrunk bunk */
               if (cs->Send) {
                  SUMA_LH("Sending last fix to SUMA ...");
                  if (!SUMA_SendToSuma (SO, cs, (void *)SOnew->NodeList, SUMA_NODE_XYZ, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }

               /* to make matters parallel with the other methods, keep dsmooth and free SOnew */
               dsmooth = SOnew->NodeList; /* CHECK IF THAT's the case here... coordinates have a new pointer after Equating surface size */
               SOnew->NodeList = NULL; /* new coordinates will stay alive in dsmooth */
               SUMA_Free_Surface_Object(SOnew); SOnew=NULL;

               break;
            case 2:
               if (!SUMA_EquateSurfaceVolumes(SOnew, SO, Opt->lim, cs)) {
                  SUMA_SL_Warn("Failed to fix surface size.\nTrying to finish ...");
               }

               /* send the unshrunk bunk */
               if (cs->Send) {
                  SUMA_LH("Sending last fix to SUMA ...");
                  if (!SUMA_SendToSuma (SO, cs, (void *)SOnew->NodeList, SUMA_NODE_XYZ, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }
               /* to make matters parallel with the other methods, keep dsmooth and free SOnew */
               dsmooth = SOnew->NodeList; /* coordinates have a new pointer after Equating surface volumes */
               SOnew->NodeList = NULL; /* new coordinates will stay alive in dsmooth */
               SUMA_Free_Surface_Object(SOnew); SOnew=NULL;
               break;
            case 3:
               if (!SUMA_EquateSurfaceAreas(SOnew, SO, Opt->lim, cs)) {
                  SUMA_SL_Warn("Failed to fix surface size.\nTrying to finish ...");
               }

               /* send the unshrunk bunk */
               if (cs->Send) {
                  SUMA_LH("Sending last fix to SUMA ...");
                  if (!SUMA_SendToSuma (SO, cs, (void *)SOnew->NodeList, SUMA_NODE_XYZ, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }
               /* to make matters parallel with the other methods, keep dsmooth and free SOnew */
               dsmooth = SOnew->NodeList; /* coordinates have a new pointer after Equating surface volumes */
               SOnew->NodeList = NULL; /* new coordinates will stay alive in dsmooth */
               SUMA_Free_Surface_Object(SOnew); SOnew=NULL;
               break;
            case 4:
               if (!SUMA_ProjectSurfaceToSphere(SOnew, SO, Opt->lim, cs)) {
                  SUMA_SL_Warn("Failed to fix surface size.\nTrying to finish ...");
               }

               /* send the unshrunk bunk */
               if (cs->Send) {
                  SUMA_LH("Sending last fix to SUMA ...");
                  if (!SUMA_SendToSuma (SO, cs, (void *)SOnew->NodeList, SUMA_NODE_XYZ, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }

               /* to make matters parallel with the other methods, keep dsmooth and free SOnew */
               dsmooth = SOnew->NodeList; /* CHECK IF THAT's the case here... coordinates have a new pointer after Equating surface size */
               SOnew->NodeList = NULL; /* new coordinates will stay alive in dsmooth */
               SUMA_Free_Surface_Object(SOnew); SOnew=NULL;
               break;
            case 0:
               break;
            default:
               SUMA_SL_Err("Huh ?");
               break;   
         }
      }
      if (LocalHead) {
         etime_GetOffset = SUMA_etime(&start_time,1);
         fprintf(SUMA_STDERR, "%s: Total processing took %f seconds for %d nodes.\n"
                              "Projected time per 100000 nodes is: %f minutes\n", 
                                 FuncName, etime_GetOffset, SO->N_Node, 
                                 etime_GetOffset * 100000 / 60.0 / (SO->N_Node));
      }

   }
   /* write out the filtered geometry. Should not be executed for data smoothing */
   if (Opt->surf_out) {
      if (!dsmooth) {
         SUMA_SL_Err("NULL dsmooth for geometry smoothing. Either failed to smooth or logical error.");
         exit(1);
      }
       
      SUMA_free(SO->NodeList); SO->NodeList = dsmooth; dsmooth = NULL; /* replace NodeList */
      switch (SO->FileType) {
         case SUMA_SUREFIT:
            SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
            sprintf(SF_name->name_coord,"%s", Opt->surf_out);
            SF_name->name_topo[0] = '\0'; 
            SO_name = (void *)SF_name;
            if (!SUMA_Save_Surface_Object (SO_name, SO, SUMA_SUREFIT, SUMA_ASCII, NULL)) {
               fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
               exit (1);
            }
            break;
         case SUMA_VEC:
            SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
            sprintf(SF_name->name_coord,"%s", Opt->surf_out);
            SF_name->name_topo[0] = '\0';
            SO_name = (void *)SF_name;
            if (!SUMA_Save_Surface_Object (SO_name, SO, SUMA_VEC, SUMA_ASCII, NULL)) {
               fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
               exit (1);
            }
            break;
         case SUMA_FREE_SURFER:
            SO_name = (void *)Opt->surf_out; 
            if (!SUMA_Save_Surface_Object (SO_name, SO, SUMA_FREE_SURFER, SUMA_ASCII, NULL)) {
               fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
               exit (1);
            }
            break;
         case SUMA_FREE_SURFER_PATCH:
            fprintf (SUMA_STDERR,"Error %s: No support for writing Free Surfer patches.\n", FuncName);
            exit (1);  
            break;
         case SUMA_PLY:
            SO_name = (void *)Opt->surf_out; 
            if (!SUMA_Save_Surface_Object (SO_name, SO, SUMA_PLY, SUMA_FF_NOT_SPECIFIED, NULL)) {
               fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
               exit (1);
            }
            break;
         case SUMA_OPENDX_MESH:
            SO_name = (void *)Opt->surf_out; 
            if (!SUMA_Save_Surface_Object (SO_name, SO, SUMA_OPENDX_MESH, SUMA_ASCII, NULL)) {
               fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
               exit (1);
            }
            break;  
         default:
            fprintf (SUMA_STDERR,"Error %s: Bad format.\n", FuncName);
            exit(1);
      }
   } else {
      if (Opt->Method != SUMA_LB_FEM && Opt->Method != SUMA_HEAT_05 ) {
         if (!dsmooth) {
            SUMA_SL_Err("NULL dsmooth for data smoothing. Either failed to smooth or logical error.");
            exit(1);
         }      
         fileout = fopen(Opt->out_name, "w");
         if (Opt->AddIndex) SUMA_disp_vecmat (dsmooth, SO->N_Node, ncol, 1, d_order, fileout, YUP);
         else SUMA_disp_vecmat (dsmooth, SO->N_Node, ncol, 1, d_order, fileout, NOPE);
         fclose(fileout); fileout = NULL;
      } else if (Opt->Method == SUMA_LB_FEM || Opt->Method == SUMA_HEAT_05) {
         SUMA_NEWDSET_ID_LABEL_HIST(dset, Opt->out_name) ;
         SUMA_WriteDset_s(Opt->out_name, dset, Opt->oform, 0, 0);
         SUMA_FreeDset(dset); dset = NULL;
      } else {
         SUMA_S_Err("Fix me");
      }
   }



   /* you don't want to exit rapidly because the SUMA might not be done processing the last elements*/
   if (cs->Send && !cs->GoneBad) {
      /* cleanup and close connections */
      if (Opt->Method == SUMA_LB_FEM_1D) {
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NODE_RGBAb, 2)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
         }
      }else if (Opt->Method == SUMA_LM) {
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NODE_XYZ, 2)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
         }
      }else if (Opt->Method == SUMA_NN_GEOM) {
         if (!SUMA_SendToSuma (SO, cs, NULL, SUMA_NODE_XYZ, 2)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
         }
      }
   }   
      
   
   SUMA_LH("clean up");
   if (SO->NodeList != dsmooth) {
      SUMA_LH("Freeing dsmooth...:");
      if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;
      SUMA_LH("Done:");
   }
   if (!SUMA_FreeSpecFields(&Spec)) {
      SUMA_S_Err("Failed to free spec fields");
   }
   mri_free(im); im = NULL;   /* done with that baby */
   if (cs) cs = NULL; /* ps->cs if freed below */
   if (SF_name) SUMA_free(SF_name);
   if (Opt->insurf_method == 1) { if (SO) SUMA_Free_Surface_Object(SO); }
   if (data_old) SUMA_free(data_old);  
   if (Opt->out_name) SUMA_free(Opt->out_name); Opt->out_name = NULL;
   if (Opt->nmask) SUMA_free(Opt->nmask); Opt->nmask = NULL;
   if (Opt) SUMA_free(Opt);
 	if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }

   exit(0);
}

